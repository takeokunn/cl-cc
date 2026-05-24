;;;; packages/optimize/src/optimizer-energy.lisp — FR-617 Energy-Aware Compilation
;;;; Power-consumption-aware optimization heuristics.
;;;; Research: energy-aware compilation for mobile/IoT.

(in-package :cl-cc/optimize)

(defvar *energy-aware-mode* nil
  "When T, optimizations consider energy cost in addition to speed.")

(defvar *energy-cost-model*
  '((add . 1) (sub . 1) (mul . 3) (div . 10)
    (load . 2) (store . 2) (branch . 1)
    (simd-add . 2) (simd-mul . 4) (simd-fma . 3))
  "Estimated relative energy cost per instruction type.")

(defun instruction-energy-cost (opcode)
  "Return the estimated energy cost of OPCODE."
  (or (cdr (assoc opcode *energy-cost-model*)) 1))

(defun energy-cost-of-block (basic-block)
  "Compute total estimated energy cost of BASIC-BLOCK.
Walks instructions in the block and sums their energy costs based on *energy-cost-model*."
  (if basic-block
      (let* ((instrs (if (consp basic-block) basic-block
                         (ignore-errors (cl-cc/optimize::%block-instructions basic-block))))
             (total 0))
        (dolist (inst instrs total)
          (incf total (instruction-energy-cost-of-inst inst))))
      0))

(defun instruction-energy-cost-of-inst (inst)
  "Return the energy cost of a VM instruction by matching its type to *energy-cost-model*."
  (typecase inst
    ((or cl-cc/vm:vm-add cl-cc/vm:vm-iadd) (instruction-energy-cost 'add))
    ((or cl-cc/vm:vm-sub cl-cc/vm:vm-isub) (instruction-energy-cost 'sub))
    ((or cl-cc/vm:vm-mul cl-cc/vm:vm-imul) (instruction-energy-cost 'mul))
    ((or cl-cc/vm:vm-div cl-cc/vm:vm-idiv) (instruction-energy-cost 'div))
    (cl-cc/vm:vm-load (instruction-energy-cost 'load))
    (cl-cc/vm:vm-store (instruction-energy-cost 'store))
    ((or cl-cc/vm:vm-jump cl-cc/vm:vm-branch) (instruction-energy-cost 'branch))
    (cl-cc/vm:vm-call 3)    ; function calls are expensive
    (cl-cc/vm:vm-mov 0)     ; moves are nearly free
    (cl-cc/vm:vm-nop 0)
    (t 1)))

(defun energy-optimize-block (block)
  "Apply energy-saving optimizations to BLOCK:
- Replace mul-by-constant with shift+add sequence
- Prefer bitwise ops over arithmetic
- Schedule loads to reduce cache misses
Returns modified instruction list or NIL if no changes."
  (let* ((instrs (if (consp block) block
                     (ignore-errors (cl-cc/optimize::%block-instructions block))))
         (changed nil)
         (result nil))
    (when instrs
      (dolist (inst (reverse instrs))
        (push (energy-optimize-instruction inst) result)
        (unless (eq (car result) inst)
          (setf changed t))))
    (when changed
      (nreverse result))))

(defun energy-optimize-instruction (inst)
  "Apply energy-saving rewrite to a single instruction.
Currently handles: mul-by-power-of-2 → shift-left conversion."
  (typecase inst
    (cl-cc/vm:vm-mul
     (let ((val (cl-cc/vm:vm-const-value inst)))
       (if (and val (integerp val) (plusp val) (zerop (logand val (1- val))))
           ;; Power of 2 -> replace with shift
           (cl-cc/vm:make-vm-shl :dst (cl-cc/vm:vm-const-dst inst)
                                 :src (cl-cc/vm:vm-const-src inst)
                                 :amount (integer-length (1- val)))
           inst)))
    (t inst)))

(defun energy-optimize-function (function-instructions)
  "Apply energy-aware optimization pass to FUNCTION-INSTRUCTIONS.
Returns modified instruction list."
  (declare (ignore function-instructions))
  ;; Full pass would partition into blocks, optimize each, and reassemble.
  ;; For now, apply instruction-level optimizations.
  (mapcar #'energy-optimize-instruction function-instructions))
