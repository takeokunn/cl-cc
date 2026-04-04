;;;; src/backend/regalloc.lisp - Linear Scan Register Allocator
;;;
;;; Maps unlimited virtual registers (VM :R0, :R1, ...) to physical
;;; machine registers. Uses linear scan with liveness analysis.
;;; Spills to stack when registers are exhausted.

(in-package :cl-cc)

;;; Data Structures

(defstruct (live-interval (:conc-name interval-))
  "Live interval for register allocation."
  (vreg nil)
  (start nil)
  (end nil)
  (use-positions nil)
  (parameter-index nil)
  (coalesce-with nil)
  (crosses-call-p nil)
  (fp-p nil)
  (remat-const nil)
  (return-value-p nil)
  (phys-reg nil)
  (spill-slot nil))

(defstruct (regalloc-result (:conc-name regalloc-))
  (assignment nil)
  (spill-map nil)
  (spill-count 0 :type fixnum)
  (instructions nil :type list))

;;; Instruction Def/Use Analysis

(defgeneric instruction-defs (inst)
  (:documentation "Returns list of virtual registers defined (written) by INST."))

(defgeneric instruction-uses (inst)
  (:documentation "Returns list of virtual registers used (read) by INST."))

(defmethod instruction-defs ((inst vm-instruction)) nil)
(defmethod instruction-uses ((inst vm-instruction)) nil)

;; dst-only: vm-const, vm-func-ref, vm-get-global
(defmethod instruction-defs ((inst vm-const)) (list (vm-dst inst)))
(defmethod instruction-defs ((inst vm-func-ref)) (list (vm-dst inst)))
(defmethod instruction-defs ((inst vm-get-global)) (list (vm-dst inst)))

;; src-only: vm-set-global
(defmethod instruction-uses ((inst vm-set-global)) (list (vm-src inst)))

;; dst+src: vm-move and unary type predicates
(defmethod instruction-defs ((inst vm-move)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-move)) (list (vm-src inst)))

;; vm-binop: dst = lhs op rhs (covers add, sub, mul, and all comparison ops)
(defmethod instruction-defs ((inst vm-binop)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-binop)) (list (vm-lhs inst) (vm-rhs inst)))

;; vm-select: dst = cond ? then : else
(defmethod instruction-defs ((inst vm-select)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-select))
  (list (vm-select-cond-reg inst)
        (vm-select-then-reg inst)
        (vm-select-else-reg inst)))

;; Jump-zero: uses reg
(defmethod instruction-uses ((inst vm-jump-zero)) (list (vm-reg inst)))

;; Print: uses reg
(defmethod instruction-uses ((inst vm-print)) (list (vm-reg inst)))

;; Halt: uses reg
(defmethod instruction-uses ((inst vm-halt)) (list (vm-reg inst)))

;; Return: uses reg
(defmethod instruction-uses ((inst vm-ret)) (list (vm-reg inst)))

;; Closure: defs dst, uses captured register values
(defmethod instruction-defs ((inst vm-closure)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-closure))
  (mapcar #'cdr (vm-captured-vars inst)))

;; Make-closure: defs dst, uses env-regs
(defmethod instruction-defs ((inst vm-make-closure)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-make-closure))
  (copy-list (vm-env-regs inst)))

;; Closure-ref-idx: defs dst, uses closure reg
(defmethod instruction-defs ((inst vm-closure-ref-idx)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-closure-ref-idx))
  (list (vm-closure-reg inst)))

;; Call: defs dst, uses func + args
(defmethod instruction-defs ((inst vm-call)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-call))
  (cons (vm-func-reg inst) (copy-list (vm-args inst))))

;; Tail-call: does not define a live destination because control never returns.
(defmethod instruction-defs ((inst vm-tail-call)) nil)
(defmethod instruction-uses ((inst vm-tail-call))
  (cons (vm-func-reg inst) (copy-list (vm-args inst))))

;;; Primitive instructions (vm-primitives.lisp)

;; Comparison ops inherit from vm-instruction, have dst/lhs/rhs pattern
;; vm-eq, vm-lt, vm-gt, vm-le, vm-ge, vm-num-eq, vm-div, vm-mod, vm-and, vm-or
(macrolet ((def-binop-like (class)
             `(progn
                (defmethod instruction-defs ((inst ,class)) (list (vm-dst inst)))
                (defmethod instruction-uses ((inst ,class)) (list (vm-lhs inst) (vm-rhs inst))))))
  (def-binop-like vm-eq)
  (def-binop-like vm-lt)
  (def-binop-like vm-gt)
  (def-binop-like vm-le)
  (def-binop-like vm-ge)
  (def-binop-like vm-num-eq)
  (def-binop-like vm-div)
  (def-binop-like vm-cl-div)
  (def-binop-like vm-mod)
  (def-binop-like vm-and)
  (def-binop-like vm-or)
  ;; Bitwise logical ops (inherit vm-instruction directly, not vm-binop)
  (def-binop-like vm-logand)
  (def-binop-like vm-logior)
  (def-binop-like vm-logxor)
  (def-binop-like vm-logeqv)
  (def-binop-like vm-logtest)
  (def-binop-like vm-logbitp)
  ;; Min/max (inherit vm-instruction directly)
  (def-binop-like vm-min)
  (def-binop-like vm-max)
  ;; Arithmetic shift (inherit vm-instruction directly)
  (def-binop-like vm-ash)
  (def-binop-like vm-rotate)
  ;; Integer division ops (inherit vm-instruction directly, not vm-binop)
  (def-binop-like vm-truncate)
  (def-binop-like vm-rem)
  ;; FR-640: nreconc
  (def-binop-like vm-nreconc)
  )

;; Unary ops: dst+src pattern
(macrolet ((def-unary-like (class)
             `(progn
                (defmethod instruction-defs ((inst ,class)) (list (vm-dst inst)))
                (defmethod instruction-uses ((inst ,class)) (list (vm-src inst))))))
  (def-unary-like vm-cons-p)
  (def-unary-like vm-null-p)
  (def-unary-like vm-symbol-p)
  (def-unary-like vm-number-p)
  (def-unary-like vm-integer-p)
  (def-unary-like vm-function-p)
  (def-unary-like vm-neg)
  (def-unary-like vm-abs)
  (def-unary-like vm-inc)
  (def-unary-like vm-dec)
  (def-unary-like vm-not)
  ;; Bitwise complement (vm-instruction directly, not vm-binop)
  (def-unary-like vm-lognot)
  (def-unary-like vm-bswap)
  ;; FR-648: simple-vector-p predicate
  (def-unary-like vm-simple-vector-p)
  ;; FR-563: sixth–tenth list accessors
  (def-unary-like vm-sixth)
  (def-unary-like vm-seventh)
  (def-unary-like vm-eighth)
  (def-unary-like vm-ninth)
  (def-unary-like vm-tenth)
  ;; FR-596: nbutlast
  (def-unary-like vm-nbutlast)
  ;; FR-597: function combinators
  (def-unary-like vm-identity)
  (def-unary-like vm-constantly)
  (def-unary-like vm-complement)
  ;; FR-631: macro expansion
  (def-unary-like vm-macroexpand-1-inst)
  (def-unary-like vm-macroexpand-inst)
  ;; FR-498: hash code
  (def-unary-like vm-sxhash)
  ;; FR-677: CLOS introspection
  (def-unary-like vm-class-name-fn)
  (def-unary-like vm-class-of-fn))

;;; List instructions (vm-list.lisp)

;; vm-cons: defs dst, uses car-reg + cdr-reg
(defmethod instruction-defs ((inst vm-cons)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-cons))
  (list (vm-car-reg inst) (vm-cdr-reg inst)))

;; vm-car, vm-cdr: defs dst, uses cons-reg
(defmethod instruction-defs ((inst vm-car)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-car)) (list (vm-cons-reg inst)))
(defmethod instruction-defs ((inst vm-cdr)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-cdr)) (list (vm-cons-reg inst)))

;; vm-rplaca, vm-rplacd: uses cons-reg + val-reg
(defmethod instruction-uses ((inst vm-rplaca))
  (list (vm-cons-reg inst) (vm-val-reg inst)))
(defmethod instruction-uses ((inst vm-rplacd))
  (list (vm-cons-reg inst) (vm-val-reg inst)))

;; vm-push: defs dst, uses src + list-reg
(defmethod instruction-defs ((inst vm-push)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-push))
  (list (vm-src inst) (vm-cons-reg inst)))

;; vm-pop: defs dst, uses cons-reg
(defmethod instruction-defs ((inst vm-pop)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-pop)) (list (vm-cons-reg inst)))

;;; CLOS instructions (vm.lisp)

(defmethod instruction-defs ((inst vm-class-def)) (list (vm-dst inst)))
(defmethod instruction-defs ((inst vm-make-obj)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-make-obj))
  (cons (vm-class-reg inst)
        (mapcar #'cdr (vm-initarg-regs inst))))
(defmethod instruction-defs ((inst vm-slot-read)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-slot-read)) (list (vm-obj-reg inst)))
(defmethod instruction-uses ((inst vm-slot-write))
  (list (vm-obj-reg inst) (vm-value-reg inst)))
(defmethod instruction-uses ((inst vm-register-method))
  (list (vm-gf-reg inst) (vm-method-reg inst)))
(defmethod instruction-defs ((inst vm-generic-call)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-generic-call))
  (cons (vm-gf-reg inst) (copy-list (vm-args inst))))

;; vm-values: defs dst, uses all src-regs
(defmethod instruction-defs ((inst vm-values)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-values))
  (copy-list (vm-src-regs inst)))

;; vm-mv-bind: defs all dst-regs
(defmethod instruction-defs ((inst vm-mv-bind))
  (copy-list (vm-dst-regs inst)))

;; vm-apply: defs dst, uses func + args
(defmethod instruction-defs ((inst vm-apply)) (list (vm-dst inst)))
(defmethod instruction-uses ((inst vm-apply))
  (cons (vm-func-reg inst) (copy-list (vm-args inst))))

;; Handler-case instructions
(defmethod instruction-defs ((inst vm-establish-handler))
  (list (vm-handler-result-reg inst)))
(defmethod instruction-uses ((inst vm-signal-error))
  (list (vm-error-reg inst)))
;; Catch/throw instructions
(defmethod instruction-defs ((inst vm-establish-catch))
  (list (vm-catch-result-reg inst)))
(defmethod instruction-uses ((inst vm-establish-catch))
  (list (vm-catch-tag-reg inst)))
(defmethod instruction-uses ((inst vm-throw))
  (list (vm-throw-tag-reg inst) (vm-throw-value-reg inst)))

;;; Liveness Analysis

(defun build-label-map (instructions)
  "Build a hash table mapping label names to instruction indices."
  (let ((map (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
          do (setf (gethash (vm-name inst) map) i))
    map))

(defun compute-live-intervals (instructions &optional float-vregs)
  "Compute live intervals for all virtual registers.
   Returns a list of live-interval objects sorted by start point."
  (let ((intervals (make-hash-table :test #'eq))
        (label-map (build-label-map instructions))
        (inst-vec (coerce instructions 'vector))
        (parameter-counter 0))
    (declare (ignore inst-vec))
    ;; Forward pass to collect initial def/use info
    (loop for inst in instructions
          for i from 0
          do (dolist (vreg (instruction-defs inst))
               (when vreg
                  (unless (gethash vreg intervals)
                    (setf (gethash vreg intervals)
                           (make-live-interval
                            :vreg vreg :start i :end i
                            :fp-p (and float-vregs (gethash vreg float-vregs))
                            :coalesce-with (when (typep inst 'vm-move)
                                             (vm-src inst)))))))
              (dolist (vreg (instruction-uses inst))
                (when vreg
                  (let ((interval (gethash vreg intervals)))
                    (if interval
                        (progn
                          (setf (interval-end interval) (max (interval-end interval) i))
                          (push i (interval-use-positions interval))
                          (when (typep inst 'vm-ret)
                            (setf (interval-return-value-p interval) t)))
                        (setf (gethash vreg intervals)
                              (make-live-interval
                                             :vreg vreg :start 0 :end i
                                             :use-positions (list i)
                                             :fp-p (and float-vregs (gethash vreg float-vregs))
                                             :parameter-index (prog1 parameter-counter
                                                                (incf parameter-counter))
                                             :return-value-p (typep inst 'vm-ret))))))))
    ;; Extend intervals across jumps (conservative)
    (loop for inst in instructions
          for i from 0
          when (and (typep inst 'vm-jump)
                    (gethash (vm-label-name inst) label-map))
           do (let ((target (gethash (vm-label-name inst) label-map)))
                (maphash (lambda (vreg interval)
                           (declare (ignore vreg))
                           (when (and (<= (interval-start interval) i)
                                      (>= (interval-end interval) target))
                             (setf (interval-start interval)
                                   (min (interval-start interval) target))
                             (setf (interval-end interval)
                                   (max (interval-end interval) i))))
                         intervals))
           when (and (typep inst 'vm-jump-zero)
                     (gethash (vm-label-name inst) label-map))
           do (let ((target (gethash (vm-label-name inst) label-map)))
                (maphash (lambda (vreg interval)
                           (declare (ignore vreg))
                           (when (and (<= (interval-start interval) i)
                                      (>= (interval-end interval) target))
                             (setf (interval-start interval)
                                   (min (interval-start interval) target))
                             (setf (interval-end interval)
                                   (max (interval-end interval) i))))
                         intervals)))
    ;; Mark intervals that span over a call site.
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-call vm-tail-call))
          do (maphash (lambda (vreg interval)
                        (declare (ignore vreg))
                        (when (and (< (interval-start interval) i)
                                   (> (interval-end interval) i))
                          (setf (interval-crosses-call-p interval) t)))
                      intervals))

    ;; Sort by start point
    (let ((result nil))
      (maphash (lambda (vreg interval)
                  (declare (ignore vreg))
                  (setf (interval-use-positions interval)
                        (sort (copy-list (interval-use-positions interval)) #'<))
                  (push interval result))
                intervals)
      (sort result #'< :key #'interval-start))))

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
