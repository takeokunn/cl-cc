;;;; src/backend/regalloc.lisp - Linear Scan Register Allocator
;;;
;;; Maps unlimited virtual registers (VM :R0, :R1, ...) to physical
;;; machine registers. Uses linear scan with liveness analysis.
;;; Spills to stack when registers are exhausted.

(in-package :cl-cc)

;;; ----------------------------------------------------------------------------
;;; Data Structures
;;; ----------------------------------------------------------------------------

(defclass live-interval ()
  ((vreg :initarg :vreg :reader interval-vreg)
   (start :initarg :start :accessor interval-start)
   (end :initarg :end :accessor interval-end)
   (phys-reg :initform nil :accessor interval-phys-reg)
   (spill-slot :initform nil :accessor interval-spill-slot)))

(defclass regalloc-result ()
  ((assignment :initarg :assignment :reader regalloc-assignment
               :documentation "Hash table: vreg -> physical register keyword")
   (spill-map :initarg :spill-map :reader regalloc-spill-map
              :documentation "Hash table: vreg -> spill slot offset (integer)")
   (spill-count :initarg :spill-count :reader regalloc-spill-count)
   (instructions :initarg :instructions :reader regalloc-instructions)))

;;; ----------------------------------------------------------------------------
;;; Instruction Def/Use Analysis
;;; ----------------------------------------------------------------------------

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

;; Env-ref: defs dst
(defmethod instruction-defs ((inst vm-env-ref)) (list (vm-dst inst)))

;;; --- Primitive instructions (vm-primitives.lisp) ---

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
  (def-binop-like vm-mod)
  (def-binop-like vm-and)
  (def-binop-like vm-or))

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
  (def-unary-like vm-not))

;;; --- List instructions (vm-list.lisp) ---

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

;;; --- CLOS instructions (vm.lisp) ---

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

;;; ----------------------------------------------------------------------------
;;; Liveness Analysis
;;; ----------------------------------------------------------------------------

(defun build-label-map (instructions)
  "Build a hash table mapping label names to instruction indices."
  (let ((map (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
          do (setf (gethash (vm-name inst) map) i))
    map))

(defun compute-live-intervals (instructions)
  "Compute live intervals for all virtual registers.
   Returns a list of live-interval objects sorted by start point."
  (let ((intervals (make-hash-table :test #'eq))
        (label-map (build-label-map instructions))
        (inst-vec (coerce instructions 'vector))
        (n (length instructions)))
    (declare (ignore inst-vec))
    ;; Forward pass to collect initial def/use info
    (loop for inst in instructions
          for i from 0
          do (dolist (vreg (instruction-defs inst))
               (when vreg
                 (unless (gethash vreg intervals)
                   (setf (gethash vreg intervals)
                         (make-instance 'live-interval
                                        :vreg vreg :start i :end i)))))
             (dolist (vreg (instruction-uses inst))
               (when vreg
                 (let ((interval (gethash vreg intervals)))
                   (if interval
                       (setf (interval-end interval) (max (interval-end interval) i))
                       (setf (gethash vreg intervals)
                             (make-instance 'live-interval
                                            :vreg vreg :start 0 :end i)))))))
    ;; Extend intervals across jumps (conservative)
    (loop for inst in instructions
          for i from 0
          when (and (typep inst 'vm-jump)
                    (gethash (vm-label-name inst) label-map))
          do (let ((target (gethash (vm-label-name inst) label-map)))
               (when (< target i)
                 ;; Backward jump — extend all live intervals that overlap
                 (maphash (lambda (vreg interval)
                            (declare (ignore vreg))
                            (when (and (<= (interval-start interval) i)
                                       (>= (interval-end interval) target))
                              (setf (interval-start interval)
                                    (min (interval-start interval) target))
                              (setf (interval-end interval)
                                    (max (interval-end interval) i))))
                          intervals)))
          when (and (typep inst 'vm-jump-zero)
                    (gethash (vm-label-name inst) label-map))
          do (let ((target (gethash (vm-label-name inst) label-map)))
               (when (< target i)
                 (maphash (lambda (vreg interval)
                            (declare (ignore vreg))
                            (when (and (<= (interval-start interval) i)
                                       (>= (interval-end interval) target))
                              (setf (interval-start interval)
                                    (min (interval-start interval) target))
                              (setf (interval-end interval)
                                    (max (interval-end interval) i))))
                          intervals))))
    ;; Extend intervals across call sites for caller-saved awareness
    ;; (handled at allocation time instead)

    ;; Sort by start point
    (let ((result nil))
      (maphash (lambda (vreg interval)
                 (declare (ignore vreg))
                 (push interval result))
               intervals)
      (sort result #'< :key #'interval-start))))

;;; ----------------------------------------------------------------------------
;;; Linear Scan Allocation
;;; ----------------------------------------------------------------------------

(defun linear-scan-allocate (intervals cc)
  "Perform linear scan register allocation.
   INTERVALS: sorted list of live-interval objects.
   CC: calling-convention object.
   Returns (values assignment-ht spill-ht spill-count)."
  (let ((assignment (make-hash-table :test #'eq))
        (spill-map (make-hash-table :test #'eq))
        (spill-count 0)
        (free-regs (copy-list (cc-gpr-pool cc)))
        (active nil))
    ;; Remove scratch register from free pool
    (setf free-regs (remove (cc-scratch-register cc) free-regs))
    (dolist (interval intervals)
      ;; Expire old intervals
      (setf active
            (remove-if (lambda (a)
                         (when (< (interval-end a) (interval-start interval))
                           (push (interval-phys-reg a) free-regs)
                           t))
                       active))
      (if free-regs
          ;; Allocate a physical register
          (let ((phys (pop free-regs)))
            (setf (interval-phys-reg interval) phys)
            (setf (gethash (interval-vreg interval) assignment) phys)
            (setf active (merge 'list (list interval) active #'< :key #'interval-end)))
          ;; Spill: pick the interval with the longest remaining range
          (let ((spill-candidate (if (and active
                                          (> (interval-end (car (last active)))
                                             (interval-end interval)))
                                     (car (last active))
                                     interval)))
            (if (eq spill-candidate interval)
                ;; Spill the current interval
                (progn
                  (incf spill-count)
                  (setf (interval-spill-slot interval) spill-count)
                  (setf (gethash (interval-vreg interval) spill-map) spill-count))
                ;; Spill the existing interval and give its register to current
                (let ((freed-reg (interval-phys-reg spill-candidate)))
                  (incf spill-count)
                  (setf (interval-spill-slot spill-candidate) spill-count)
                  (setf (gethash (interval-vreg spill-candidate) spill-map) spill-count)
                  (remhash (interval-vreg spill-candidate) assignment)
                  (setf (interval-phys-reg spill-candidate) nil)
                  (setf active (remove spill-candidate active))
                  (setf (interval-phys-reg interval) freed-reg)
                  (setf (gethash (interval-vreg interval) assignment) freed-reg)
                  (setf active (merge 'list (list interval) active
                                      #'< :key #'interval-end)))))))
    (values assignment spill-map spill-count)))

;;; ----------------------------------------------------------------------------
;;; Spill Code Insertion
;;; ----------------------------------------------------------------------------

(defclass vm-spill-store (vm-instruction)
  ((src-reg :initarg :src-reg :reader vm-spill-src)
   (slot :initarg :slot :reader vm-spill-slot))
  (:documentation "Store register to spill slot [RBP - slot*8]."))

(defclass vm-spill-load (vm-instruction)
  ((dst-reg :initarg :dst-reg :reader vm-spill-dst)
   (slot :initarg :slot :reader vm-spill-slot))
  (:documentation "Load spill slot [RBP - slot*8] into register."))

(defun insert-spill-code (instructions assignment spill-map cc)
  "Insert spill loads/stores around instructions that use spilled registers.
   Returns a new instruction list with spill code inserted."
  (let ((scratch (cc-scratch-register cc))
        (result nil))
    (dolist (inst instructions)
      ;; Load spilled uses before the instruction
      (dolist (vreg (instruction-uses inst))
        (when (and vreg (gethash vreg spill-map))
          (push (make-instance 'vm-spill-load
                               :dst-reg scratch
                               :slot (gethash vreg spill-map))
                result)
          ;; Temporarily map this vreg to scratch for the instruction
          (setf (gethash vreg assignment) scratch)))
      ;; The instruction itself
      (push inst result)
      ;; Store spilled defs after the instruction
      (dolist (vreg (instruction-defs inst))
        (when (and vreg (gethash vreg spill-map))
          ;; Map def to scratch, then store
          (setf (gethash vreg assignment) scratch)
          (push (make-instance 'vm-spill-store
                               :src-reg scratch
                               :slot (gethash vreg spill-map))
                result))))
    (nreverse result)))

;;; ----------------------------------------------------------------------------
;;; Public API
;;; ----------------------------------------------------------------------------

(defun allocate-registers (instructions cc)
  "Run register allocation on VM instruction list.
   CC is a calling-convention object.
   Returns a regalloc-result."
  (let ((intervals (compute-live-intervals instructions)))
    (multiple-value-bind (assignment spill-map spill-count)
        (linear-scan-allocate intervals cc)
      (let ((final-instructions
              (if (> spill-count 0)
                  (insert-spill-code instructions assignment spill-map cc)
                  instructions)))
        (make-instance 'regalloc-result
                       :assignment assignment
                       :spill-map spill-map
                       :spill-count spill-count
                       :instructions final-instructions)))))

(defun regalloc-lookup (result vreg)
  "Look up physical register for VREG in allocation result.
   Returns the physical register keyword or NIL if spilled."
  (gethash vreg (regalloc-assignment result)))
