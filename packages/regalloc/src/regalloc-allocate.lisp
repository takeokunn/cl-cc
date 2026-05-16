(in-package :cl-cc/regalloc)
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

;;; Linear Scan Allocation State

(defstruct (lsa-state (:conc-name lsa-))
  "Mutable state shared by the linear-scan allocation helpers."
  (assignment   (make-hash-table :test 'eq) :type hash-table)
  (spill-map    (make-hash-table :test 'eq) :type hash-table)
  (spill-count  0                           :type integer)
  (free-regs    nil                         :type list)
  (free-fp-regs nil                         :type list)
  (active       nil                         :type list)
  (interval-map (make-hash-table :test 'eq) :type hash-table))

(defvar *current-allocation-policy* nil
  "Hint-derived allocation policy plist bound during allocation.
Recognized keys:
  :prefer-callee-saved-p
  :prefer-caller-saved-p")

(defun %lsa-interval-pool (state interval)
  "Return the free-register pool for INTERVAL's register class."
  (if (interval-fp-p interval) (lsa-free-fp-regs state) (lsa-free-regs state)))

(defun %lsa-set-interval-pool (state interval new-pool)
  "Replace the free-register pool for INTERVAL's register class."
  (if (interval-fp-p interval)
      (setf (lsa-free-fp-regs state) new-pool)
      (setf (lsa-free-regs state) new-pool)))

(defun %lsa-expire-old (state interval)
  "Return expired intervals to their register pools; remove from active list."
  (setf (lsa-active state)
        (remove-if (lambda (a)
                     (when (< (interval-end a) (interval-start interval))
                       (%lsa-set-interval-pool state a
                                               (cons (interval-phys-reg a)
                                                     (%lsa-interval-pool state a)))
                       t))
                   (lsa-active state))))

(defun %lsa-spill-current (state interval)
  "Assign the next spill slot to INTERVAL and record it in spill-map."
  (incf (lsa-spill-count state))
  (setf (interval-spill-slot interval) (lsa-spill-count state))
  (setf (gethash (interval-vreg interval) (lsa-spill-map state)) (lsa-spill-count state)))

(defun %lsa-best-spill-candidate (state interval)
  "Return the active interval (or INTERVAL itself) with the farthest next use."
  (let ((same-class (remove-if-not (lambda (cand)
                                     (eq (interval-fp-p cand) (interval-fp-p interval)))
                                   (lsa-active state))))
    (reduce (lambda (best candidate)
              (let ((best-next (%interval-next-use-after best (interval-start interval)))
                    (cand-next (%interval-next-use-after candidate (interval-start interval))))
                (cond ((null best) candidate)
                      ((null cand-next) candidate)
                      ((null best-next) best)
                      ((> cand-next best-next) candidate)
                      (t best))))
            same-class
            :initial-value interval)))

;;; Linear Scan Allocation — named helpers

(defun %lsa-assign (state interval phys)
  "Assign PHYS to INTERVAL and insert it into the active set (sorted by end)."
  (setf (interval-phys-reg interval) phys
        (gethash (interval-vreg interval) (lsa-assignment state)) phys
        (lsa-active state) (merge 'list (list interval) (lsa-active state) #'< :key #'interval-end)))

(defun %lsa-try-coalesce (state interval)
  "Attempt register coalescing for INTERVAL.  Returns T on success."
  (let* ((src-vreg (interval-coalesce-with interval))
         (src-int (and src-vreg (gethash src-vreg (lsa-interval-map state)))))
    (when (and src-int
               (eq (interval-fp-p src-int) (interval-fp-p interval))
               (interval-phys-reg src-int)
               (<= (interval-end src-int) (interval-start interval)))
      (let ((phys (interval-phys-reg src-int)))
        (setf (lsa-active state) (remove src-int (lsa-active state) :test #'eq))
        (%lsa-assign state interval phys)
        t))))

(defun %lsa-allocate-from-pool (state interval cc pool)
  "Pick a physical register from POOL (preferred first) and assign it."
  (let* ((preferred (%preferred-register-for-interval interval cc pool))
         (phys (or preferred (car pool))))
    (%lsa-set-interval-pool state interval (remove phys pool :count 1 :test #'eq))
    (%lsa-assign state interval phys)))

(defun %lsa-evict-and-assign (state interval)
  "Spill the worst active interval and reassign its register to INTERVAL."
  (let ((spill-candidate (%lsa-best-spill-candidate state interval)))
    (if (eq spill-candidate interval)
        (%lsa-spill-current state interval)
        (let ((freed-reg (interval-phys-reg spill-candidate)))
          (%lsa-spill-current state spill-candidate)
          (remhash (interval-vreg spill-candidate) (lsa-assignment state))
          (setf (interval-phys-reg spill-candidate) nil
                (lsa-active state) (remove spill-candidate (lsa-active state)))
          (%lsa-assign state interval freed-reg)))))

(defun %interval-next-use-after (interval position)
  "Return the next use position of INTERVAL after POSITION, or NIL."
  (find-if (lambda (use-pos) (> use-pos position))
           (interval-use-positions interval)))

(defun %return-value-preferred-reg (interval cc free-regs)
  "Strategy: prefer the ABI return register for return-value intervals."
  (let ((preferred (and (interval-return-value-p interval)
                        (if (interval-fp-p interval)
                            (target-fp-ret-reg cc)
                            (target-ret-reg cc)))))
    (and preferred (member preferred free-regs) preferred)))

(defun %call-crossing-preferred-reg (interval cc free-regs)
  "Strategy: prefer a callee-saved register for intervals crossing a call."
  (when (and (interval-crosses-call-p interval) (not (interval-fp-p interval)))
    (find-if (lambda (reg) (member reg free-regs :test #'eq))
             (target-callee-saved cc))))

(defun %param-preferred-reg (interval cc free-regs)
  "Strategy: prefer the ABI argument register matching the parameter position."
  (let* ((param-index (interval-parameter-index interval))
         (arg-regs (if (interval-fp-p interval) (target-fp-arg-regs cc) (target-arg-regs cc)))
         (preferred (and (integerp param-index)
                          (< param-index (length arg-regs))
                          (nth param-index arg-regs))))
    (and preferred (member preferred free-regs) preferred)))

(defun %hint-policy-preferred-reg (interval cc free-regs)
  "Strategy: when policy prefers caller-saved regs, bias allocation accordingly.
Safety guard: do not force caller-saved for call-crossing intervals."
  (when (and (getf *current-allocation-policy* :prefer-caller-saved-p)
             (not (interval-crosses-call-p interval))
             (not (interval-fp-p interval)))
    (find-if (lambda (reg) (member reg free-regs :test #'eq))
             (target-caller-saved cc))))

(defparameter *preferred-register-strategies*
  (list #'%hint-policy-preferred-reg
        #'%return-value-preferred-reg
        #'%call-crossing-preferred-reg
        #'%param-preferred-reg)
  "Ordered list of preferred-register strategies tried left-to-right; first truthy result wins.")

(defun %derive-single-function-policy (instructions)
  "Best-effort default policy derivation for single-function instruction streams."
  (let* ((bodies (regalloc-collect-linear-functions instructions))
         (labels nil))
    (maphash (lambda (label body)
               (declare (ignore body))
               (push label labels))
             bodies)
    (when (= (length labels) 1)
      (let* ((label (first labels))
             (hints (regalloc-compute-interprocedural-hints instructions)))
        (regalloc-build-allocation-policy-from-hints hints label)))))

(defun %preferred-register-for-interval (interval cc free-regs)
  "Return a preferred free physical register for INTERVAL, or NIL."
  (some (lambda (strategy) (funcall strategy interval cc free-regs))
        *preferred-register-strategies*))

(defun linear-scan-allocate (intervals cc)
  "Perform linear scan register allocation.
   INTERVALS: sorted list of live-interval objects.
   CC: target-desc object.
   Returns (values assignment-ht spill-ht spill-count)."
  (let ((state (make-lsa-state
                :free-regs (remove (first (target-scratch-regs cc)) (copy-list (target-allocatable-regs cc)))
                :free-fp-regs (remove-duplicates
                               (append (copy-list (target-fp-arg-regs cc))
                                       (list (target-fp-ret-reg cc)))
                               :test #'eq))))
    (dolist (int intervals)
      (setf (gethash (interval-vreg int) (lsa-interval-map state)) int))
    (dolist (interval intervals)
      (%lsa-expire-old state interval)
      (unless (%lsa-try-coalesce state interval)
        (let ((pool (%lsa-interval-pool state interval)))
          (if pool
              (%lsa-allocate-from-pool state interval cc pool)
              (%lsa-evict-and-assign state interval)))))
    (values (lsa-assignment state) (lsa-spill-map state) (lsa-spill-count state))))

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
  (handler-case
      (sexp->instruction
       (%regalloc-map-tree (lambda (x)
                             (if (and (keywordp x) (gethash x reg-map))
                                 (gethash x reg-map)
                                 x))
                            (instruction->sexp inst)))
    (error () inst)))

(defun %regalloc-reserved-scratch-regs (inst cc)
  "Return scratch registers reserved internally by INST on CC, if any."
  (when (and (eq (target-name cc) :x86-64)
             (or (typep inst 'vm-integer-mul-high-u)
                 (typep inst 'vm-integer-mul-high-s)
                 (typep inst 'vm-truncate)
                 (typep inst 'vm-rem)
                 (typep inst 'vm-div)
                 (typep inst 'vm-mod)))
    (list (first (target-scratch-regs cc)))))

(defun %regalloc-scratch-candidates (cc used-phys &optional inst)
  (let ((reserved (remove nil (%regalloc-reserved-scratch-regs inst cc))))
    (remove-if (lambda (reg)
                 (or (null reg)
                     (member reg used-phys :test #'eq)
                     (member reg reserved :test #'eq)))
              (remove-duplicates
               (append (list (first (target-scratch-regs cc))
                             (target-ret-reg cc))
                       (target-caller-saved cc)
                       (target-allocatable-regs cc))
               :test #'eq))))

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
             (available (%regalloc-scratch-candidates cc used-phys inst))
             (reg-map (make-hash-table :test #'eq)))
        (flet ((alloc-scratch ()
                 (or (pop available)
                     (error "insert-spill-code: no scratch register available for ~S" inst))))
          (flet ((ensure-scratch (vreg)
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
          ;; Ensure spilled defs have scratch registers (completes reg-map before rewrite).
          (dolist (vreg (instruction-defs inst))
            (when (and vreg (gethash vreg spill-map))
              (ensure-scratch vreg)))
          ;; Rewrite instruction once with complete reg-map, then push.
          (push (if (zerop (hash-table-count reg-map))
                    inst
                    (%regalloc-rewrite-inst inst reg-map))
                result)
          ;; Store spilled defs after the instruction.
          (dolist (vreg (instruction-defs inst))
            (when (and vreg (gethash vreg spill-map))
              (push (make-vm-spill-store :src-reg (gethash vreg reg-map)
                                         :slot (gethash vreg spill-map))
                    result)))))))
    (nreverse result)))

;;; Public API

(defun allocate-registers (instructions cc &optional float-vregs allocation-policy)
  "Run register allocation on VM instruction list.
   CC is a target-desc object.
   ALLOCATION-POLICY is an optional plist (e.g. from
   REGALLOC-BUILD-ALLOCATION-POLICY-FROM-HINTS) used to bias preferred registers.
   Returns a regalloc-result."
  (let* ((intervals (compute-live-intervals instructions float-vregs))
         (effective-policy (or allocation-policy
                               (%derive-single-function-policy instructions)))
         (*current-allocation-policy* effective-policy))
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
