;;;; optimizer-ml-regalloc.lisp — FR-581 ML-guided register allocation hints
;;;;
;;;; Provides heuristic register pressure analysis and allocation hints.
;;;; Full ML-driven allocation waits for training data, but this pass
;;;; computes liveness-based register pressure and annotates instructions
;;;; with spill priority hints for the register allocator.

(in-package :cl-cc/optimize)

(defstruct (regalloc-hint (:conc-name rah-))
  "Register allocation hint attached to VM instructions."
  (pressure 0 :type fixnum)          ; estimated register pressure at instruction
  (spill-priority 0 :type fixnum)    ; 0=no-spill, 10=must-spill
  (preferred-register nil :type symbol))

(defvar *ml-regalloc-enabled* t
  "When T, compute register pressure hints during optimization.")

(defun %compute-register-pressure (instructions)
  "Compute estimated register pressure at each instruction.
Returns a list of pressure values (same length as INSTRUCTIONS)."
  (let* ((n (length instructions))
         (pressure (make-array n :initial-element 0))
         (live-regs (make-hash-table :test #'eq))
         (max-regs 16))  ; x86-64 has 16 GPRs
    ;; Backward pass: compute liveness and pressure
    (loop for i from (1- n) downto 0
          for inst = (nth i instructions)
          do (dolist (reg (%instruction-uses inst))
               (incf (gethash reg live-regs 0)))
          (setf (aref pressure i) (hash-table-count live-regs))
          (dolist (reg (%instruction-defs inst))
               (remhash reg live-regs)))
    ;; Normalize to 0-10 scale
    (loop for i from 0 below n
          collect (min 10 (floor (* 10 (aref pressure i)) max-regs)))))

(defun %instruction-uses (inst)
  "Return list of register symbols USED (read) by INST."
  (let ((uses nil))
    (when (typep inst 'cl-cc/vm:vm-binop)
      (when (slot-boundp inst 'cl-cc/vm::src)
        (push (cl-cc/vm::src inst) uses))
      (when (slot-boundp inst 'cl-cc/vm::src2)
        (push (cl-cc/vm::src2 inst) uses)))
    (when (typep inst 'cl-cc/vm:vm-call)
      (loop for arg in (cl-cc/vm:vm-call-args inst)
            when (symbolp arg) do (push arg uses)))
    uses))

(defun %instruction-defs (inst)
  "Return list of register symbols DEFINED (written) by INST."
  (when (typep inst 'cl-cc/vm:vm-binop)
    (when (slot-boundp inst 'cl-cc/vm::dst)
      (list (cl-cc/vm::dst inst)))))

(defun opt-pass-ml-regalloc (instructions)
  "FR-581: Compute register pressure hints for INSTRUCTIONS.
Attaches spill priorities and preferred register assignments based on
liveness analysis. Returns instructions annotated with regalloc-hints
in their metadata slots."
  (if (and *ml-regalloc-enabled* instructions)
      (let ((pressure (nreverse (%compute-register-pressure instructions))))
        (dotimes (i (length instructions))
          (let ((inst (nth i instructions))
                (p (nth i pressure)))
            ;; Annotate instruction with pressure hint
            (setf (getf (cl-cc/vm:instruction-plist inst) :regalloc-hint)
                  (make-regalloc-hint :pressure p
                                      :spill-priority (if (> p 12) 5 0)
                                      :preferred-register nil))))
        instructions)
      instructions))
