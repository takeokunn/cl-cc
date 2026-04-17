(in-package :cl-cc/emit)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Regalloc — Linear Scan Allocation, Spill Code Insertion, Public API
;;;
;;; Contains: %interval-next-use-after, %preferred-register-for-interval,
;;; linear-scan-allocate, vm-spill-store/vm-spill-load instruction structs,
;;; %regalloc-map-tree, %regalloc-rewrite-inst, %regalloc-scratch-candidates,
;;; insert-spill-code, allocate-registers (public API), regalloc-lookup.
;;;
;;; Data structures (live-interval, regalloc-result), def/use analysis, and
;;; liveness computation are in regalloc.lisp (loads before this file).
;;;
;;; Load order: after regalloc.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Linear Scan Allocation

(defun %interval-next-use-after (interval position)
  "Return the next use position of INTERVAL after POSITION, or NIL."
  (find-if (lambda (use-pos) (> use-pos position))
           (interval-use-positions interval)))

(defun %preferred-register-for-interval (interval cc free-regs)
  "Return a preferred free physical register for INTERVAL, or NIL."
  (or (let ((preferred (and (interval-return-value-p interval)
                            (if (interval-fp-p interval)
                                (cc-fp-return-register cc)
                                (cc-return-register cc)))))
        (and preferred (member preferred free-regs) preferred))
      (when (and (interval-crosses-call-p interval)
                 (not (interval-fp-p interval)))
        (find-if (lambda (reg) (member reg free-regs :test #'eq))
                 (cc-callee-saved cc)))
      (let* ((param-index (interval-parameter-index interval))
             (arg-regs (if (interval-fp-p interval)
                           (cc-fp-arg-registers cc)
                           (cc-arg-registers cc)))
             (preferred (and (integerp param-index)
                             (< param-index (length arg-regs))
                             (nth param-index arg-regs))))
        (and preferred (member preferred free-regs) preferred))))

(defun linear-scan-allocate (intervals cc)
  "Perform linear scan register allocation.
   INTERVALS: sorted list of live-interval objects.
   CC: calling-convention object.
   Returns (values assignment-ht spill-ht spill-count)."
  (let ((assignment (make-hash-table :test 'eq))
        (spill-map (make-hash-table :test 'eq))
        (spill-count 0)
        (free-regs (copy-list (cc-gpr-pool cc)))
        (free-fp-regs (remove-duplicates
                       (append (copy-list (cc-fp-arg-registers cc))
                               (list (cc-fp-return-register cc)))
                       :test #'eq))
        (active nil)
        (interval-map (make-hash-table :test 'eq)))
    (labels ((interval-pool (interval)
               (if (interval-fp-p interval) free-fp-regs free-regs))
             (set-interval-pool (interval new-pool)
               (if (interval-fp-p interval)
                   (setf free-fp-regs new-pool)
                   (setf free-regs new-pool)))
             (expire-old (interval)
               (setf active
                     (remove-if (lambda (a)
                                  (when (< (interval-end a) (interval-start interval))
                                    (set-interval-pool a (cons (interval-phys-reg a) (interval-pool a)))
                                    t))
                                active)))
             (spill-current (interval)
               (incf spill-count)
               (setf (interval-spill-slot interval) spill-count)
               (setf (gethash (interval-vreg interval) spill-map) spill-count))
             (best-spill-candidate (interval)
               (let ((same-class (remove-if-not (lambda (cand)
                                                  (eq (interval-fp-p cand) (interval-fp-p interval)))
                                                active)))
                 (reduce (lambda (best candidate)
                           (let ((best-next (%interval-next-use-after best (interval-start interval)))
                                 (cand-next (%interval-next-use-after candidate (interval-start interval))))
                             (cond ((null best) candidate)
                                   ((null cand-next) candidate)
                                   ((null best-next) best)
                                   ((> cand-next best-next) candidate)
                                   (t best))))
                         same-class
                         :initial-value interval))))
      (dolist (int intervals)
        (setf (gethash (interval-vreg int) interval-map) int))
      (setf free-regs (remove (cc-scratch-register cc) free-regs))
      (dolist (interval intervals)
        (expire-old interval)
        (let ((coalesced nil))
          (let* ((src-vreg (interval-coalesce-with interval))
                 (src-int (and src-vreg (gethash src-vreg interval-map))))
            (when (and src-int
                       (eq (interval-fp-p src-int) (interval-fp-p interval))
                       (interval-phys-reg src-int)
                       (<= (interval-end src-int) (interval-start interval)))
              (let ((phys (interval-phys-reg src-int)))
                (setf active (remove src-int active :test #'eq))
                (setf (interval-phys-reg interval) phys)
                (setf (gethash (interval-vreg interval) assignment) phys)
                (setf active (merge 'list (list interval) active #'< :key #'interval-end))
                (setf coalesced t))))
          (unless coalesced
            (let ((pool (interval-pool interval)))
              (if pool
                  (let* ((preferred (%preferred-register-for-interval interval cc pool))
                         (phys (if preferred preferred (car pool)))
                         (new-pool (remove phys pool :count 1 :test #'eq)))
                    (set-interval-pool interval new-pool)
                    (setf (interval-phys-reg interval) phys)
                    (setf (gethash (interval-vreg interval) assignment) phys)
                    (setf active (merge 'list (list interval) active #'< :key #'interval-end)))
                  (let ((spill-candidate (best-spill-candidate interval)))
                    (if (eq spill-candidate interval)
                        (spill-current interval)
                        (let ((freed-reg (interval-phys-reg spill-candidate)))
                          (spill-current spill-candidate)
                          (remhash (interval-vreg spill-candidate) assignment)
                          (setf (interval-phys-reg spill-candidate) nil)
                          (setf active (remove spill-candidate active))
                          (setf (interval-phys-reg interval) freed-reg)
                          (setf (gethash (interval-vreg interval) assignment) freed-reg)
                          (setf active (merge 'list (list interval) active #'< :key #'interval-end))))))))))
      (values assignment spill-map spill-count))))

;;; Spill Code Insertion

(define-vm-instruction vm-spill-store (vm-instruction)
  "Store register to spill slot [RBP - slot*8]."
  (src-reg nil :reader vm-spill-src)
  (slot nil :reader vm-spill-slot))

(define-vm-instruction vm-spill-load (vm-instruction)
  "Load spill slot [RBP - slot*8] into register."
  (dst-reg nil :reader vm-spill-dst)
  (slot nil :reader vm-spill-slot))

(defun %regalloc-map-tree (fn tree)
  (if (consp tree)
      (cons (%regalloc-map-tree fn (car tree))
            (%regalloc-map-tree fn (cdr tree)))
      (funcall fn tree)))

(defun %regalloc-rewrite-inst (inst reg-map)
  "Return INST with register keywords substituted per REG-MAP."
  (flet ((sub (x)
           (if (and (keywordp x) (gethash x reg-map))
               (gethash x reg-map)
               x)))
    (handler-case
        (sexp->instruction (%regalloc-map-tree #'sub (instruction->sexp inst)))
      (error () inst))))

(defun %regalloc-scratch-candidates (cc used-phys)
  (remove-if (lambda (reg) (or (null reg) (member reg used-phys :test #'eq)))
             (remove-duplicates
              (append (list (cc-scratch-register cc)
                            (cc-return-register cc))
                      (cc-caller-saved cc)
                      (cc-gpr-pool cc))
              :test #'eq)))

(defun insert-spill-code (instructions assignment spill-map cc &optional remat-map)
  "Insert spill loads/stores around instructions that use spilled registers.
   Returns a new instruction list with spill code inserted."
  (let ((result nil))
    (dolist (inst instructions)
      (let* ((used-phys (remove nil
                                (mapcar (lambda (vreg)
                                          (and vreg
                                               (not (gethash vreg spill-map))
                                               (gethash vreg assignment)))
                                        (append (instruction-uses inst)
                                                (instruction-defs inst)))))
             (available (%regalloc-scratch-candidates cc used-phys))
             (reg-map (make-hash-table :test #'eq)))
        (labels ((alloc-scratch ()
                   (or (pop available)
                       (error "insert-spill-code: no scratch register available for ~S" inst)))
                 (ensure-scratch (vreg)
                   (or (gethash vreg reg-map)
                       (setf (gethash vreg reg-map) (alloc-scratch)))))
          ;; Load spilled uses before the instruction.
          (dolist (vreg (instruction-uses inst))
            (when (and vreg (gethash vreg spill-map))
              (let ((scratch (ensure-scratch vreg)))
                (let ((remat (and remat-map (gethash vreg remat-map))))
                  (push (if remat
                            (make-vm-const :dst scratch :value remat)
                            (make-vm-spill-load :dst-reg scratch
                                                :slot (gethash vreg spill-map)))
                        result)))))
          ;; Rewrite the instruction itself to use per-vreg scratch registers.
          (let ((rewritten (if (zerop (hash-table-count reg-map))
                               inst
                               (%regalloc-rewrite-inst inst reg-map))))
            ;; Ensure spilled defs get distinct scratch registers when needed.
            (dolist (vreg (instruction-defs inst))
              (when (and vreg (gethash vreg spill-map))
                (ensure-scratch vreg)))
            (setf rewritten (if (zerop (hash-table-count reg-map))
                                inst
                                (%regalloc-rewrite-inst inst reg-map)))
            (push rewritten result))
          ;; Store spilled defs after the instruction.
          (dolist (vreg (instruction-defs inst))
            (when (and vreg (gethash vreg spill-map))
              (push (make-vm-spill-store :src-reg (gethash vreg reg-map)
                                         :slot (gethash vreg spill-map))
                    result))))))
    (nreverse result)))

;;; Public API

(defun allocate-registers (instructions cc &optional float-vregs)
  "Run register allocation on VM instruction list.
   CC is a calling-convention object.
   Returns a regalloc-result."
  (let ((intervals (compute-live-intervals instructions float-vregs)))
    (multiple-value-bind (assignment spill-map spill-count)
        (linear-scan-allocate intervals cc)
      (let* ((remat-map (let ((ht (make-hash-table :test #'eq)))
                          (dolist (interval intervals ht)
                            (when (interval-remat-const interval)
                              (setf (gethash (interval-vreg interval) ht)
                                    (interval-remat-const interval))))))
             (final-instructions
              (if (> spill-count 0)
                  (insert-spill-code instructions assignment spill-map cc remat-map)
                  instructions)))
        (make-regalloc-result
                        :assignment assignment
                       :spill-map spill-map
                       :spill-count spill-count
                       :instructions final-instructions)))))

(defun regalloc-lookup (result vreg)
  "Look up physical register for VREG in allocation result.
   Returns the physical register keyword or NIL if spilled."
  (gethash vreg (regalloc-assignment result)))
