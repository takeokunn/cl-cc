;;;; packages/backend/emit/src/aarch64-program.lisp - AArch64 Program-Level Emission
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains: *a64-emitter-table* dispatch table, emit-a64-instruction,
;;; a64-used-callee-saved-pairs, emit-a64-prologue, emit-a64-epilogue,
;;; emit-a64-program (two-pass label resolution), compile-to-aarch64-bytes.
;;;
;;; Register constants, instruction encoders (encode-add/sub/mul, etc.),
;;; per-instruction emitters (emit-a64-vm-*), label-offset computation, and
;;; spill helpers are in aarch64-codegen.lisp (loads before this file).
;;;
;;; Load order: after aarch64-codegen.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/emit)

;;; Main program emitter

(defparameter *a64-emitter-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash 'vm-const ht) #'emit-a64-vm-const)
    (setf (gethash 'vm-move ht) #'emit-a64-vm-move)
    (setf (gethash 'vm-add ht) #'emit-a64-vm-add)
    (setf (gethash 'vm-integer-add ht) #'emit-a64-vm-add)
    (setf (gethash 'vm-sub ht) #'emit-a64-vm-sub)
    (setf (gethash 'vm-integer-sub ht) #'emit-a64-vm-sub)
    (setf (gethash 'vm-mul ht) #'emit-a64-vm-mul)
    (setf (gethash 'vm-integer-mul ht) #'emit-a64-vm-mul)
    (setf (gethash 'vm-min ht) #'emit-a64-vm-min)
    (setf (gethash 'vm-max ht) #'emit-a64-vm-max)
    (setf (gethash 'vm-select ht) #'emit-a64-vm-select)
    (setf (gethash 'vm-bswap ht) #'emit-a64-vm-bswap)
    (setf (gethash 'vm-rotate ht) #'emit-a64-vm-rotate)
    (setf (gethash 'vm-halt ht) #'emit-a64-vm-halt)
    (setf (gethash 'vm-ret ht) #'emit-a64-vm-ret)
    (setf (gethash 'vm-spill-store ht) #'emit-a64-vm-spill-store)
    (setf (gethash 'vm-spill-load ht) #'emit-a64-vm-spill-load)
    ht)
  "Maps VM instruction type symbols to AArch64 emitter functions (inst stream).")

(defun emit-a64-instruction (inst stream current-pos label-offsets)
  "Emit AArch64 machine code for one VM instruction."
  (let ((tp (type-of inst)))
    (cond
      ((or (eq tp 'vm-label) (eq tp 'vm-print)) nil)
      ((eq tp 'vm-jump)
       (emit-a64-vm-jump inst stream current-pos label-offsets))
      ((eq tp 'vm-jump-zero)
       (emit-a64-vm-jump-zero inst stream current-pos label-offsets))
      ((eq tp 'vm-call)
       (emit-a64-instr (encode-blr (a64-reg (vm-func-reg inst))) stream))
      ((eq tp 'vm-tail-call)
       (emit-a64-instr (encode-br (a64-reg (vm-func-reg inst))) stream))
      (t (let ((emitter (gethash tp *a64-emitter-table*)))
           (if emitter
               (funcall emitter inst stream)
               (error "Unsupported AArch64 instruction: ~A" tp)))))))

;;; AArch64 callee-saved registers to save/restore (X19-X28, X29, X30)
;;; X29 = frame pointer (FP), X30 = link register (LR)
;;; Saved as pairs: (X29,X30), (X19,X20), (X21,X22), (X23,X24), (X25,X26), (X27,X28)
;;; Total prologue size: 6 STP x 4 bytes = 24 bytes

(defun a64-used-callee-saved-pairs (ra &key frame-pointer-p)
  "Return callee-saved register pairs actually used by RA.
   When FRAME-POINTER-P is true, include the FP/LR pair first.
   Each pair is emitted as one STP/LDP."
  (let ((phys-regs (loop for phys being the hash-values of (regalloc-assignment ra)
                         collect phys)))
    (append (when frame-pointer-p '((29 30)))
            (remove-if-not (lambda (pair)
                             (or (member (first pair) phys-regs :test #'eq)
                                 (member (second pair) phys-regs :test #'eq)))
                           '((19 20) (21 22) (23 24) (25 26) (27 28))))))

(defun emit-a64-prologue (stream save-pairs)
  "Emit AArch64 function prologue: save FP/LR and the callee-saved pairs in SAVE-PAIRS."
  ;; Shadow call stack: STR LR, [X18], #8
  (emit-a64-instr (encode-str-post +a64-lr+ +a64-scs+ 8) stream)
  (dolist (pair save-pairs)
    (destructuring-bind (rn rm) pair
      ;; STP Xn, Xm, [SP, #-16]!
      (emit-a64-instr (encode-stp-pre rn rm +a64-sp+ -2) stream))))

(defun emit-a64-epilogue (stream save-pairs)
  "Emit AArch64 function epilogue: restore callee-saved pairs and return."
  (dolist (pair (reverse save-pairs))
    (destructuring-bind (rn rm) pair
      (emit-a64-instr (encode-ldp-post rn rm +a64-sp+ 2) stream)))
  ;; Shadow call stack verification:
  ;;   LDR X17, [X18, #-8]!
  ;;   CMP X17, X30
  ;;   B.EQ +2 instructions
  ;;   BRK #0
  (emit-a64-instr (encode-ldr-pre +a64-scs-tmp+ +a64-scs+ -8) stream)
  (emit-a64-instr (encode-cmp +a64-scs-tmp+ +a64-lr+) stream)
  (emit-a64-instr (encode-b-cond 2 0) stream)
  (emit-a64-instr (encode-brk 0) stream)
  ;; RET
  (emit-a64-instr +a64-ret+ stream))

(defun emit-a64-program (program stream)
  "Emit AArch64 machine code for the entire VM program.
   Uses two-pass approach for label resolution."
  (let* ((instructions (vm-program-instructions program))
         (cfg (cfg-build instructions))
          (leaf-p (vm-program-leaf-p program))
          (frame-pointer-p (or (not leaf-p)
                               (plusp (regalloc-spill-count *current-a64-regalloc*))))
          (save-pairs (a64-used-callee-saved-pairs *current-a64-regalloc*
                                                   :frame-pointer-p frame-pointer-p))
          ;; Shadow call stack prologue is 1 instruction; each STP/LDP pair is 1 instruction.
          (prologue-size (* 4 (1+ (length save-pairs))))
          (ordered-instructions (if (cfg-entry cfg)
                                    (progn
                                      (cfg-compute-dominators cfg)
                                      (cfg-compute-loop-depths cfg)
                                      (cfg-flatten-hot-cold cfg))
                                    instructions))
          (label-offsets (build-a64-label-offsets ordered-instructions prologue-size)))
    ;; Prologue
    (emit-a64-prologue stream save-pairs)
    ;; Second pass: emit instructions
    (let ((pos prologue-size))
      (dolist (inst ordered-instructions)
        (emit-a64-instruction inst stream pos label-offsets)
        (incf pos (a64-instruction-size inst))))
    ;; Epilogue
    (emit-a64-epilogue stream save-pairs)))

;;; Public API

(defun compile-to-aarch64-bytes (program)
  "Compile VM program to AArch64 machine code bytes.
   Returns: (simple-array (unsigned-byte 8) (*))"
  (let* ((instructions (vm-program-instructions program))
          (ra (allocate-registers instructions *aarch64-calling-convention*))
          (allocated-program (make-vm-program
                              :instructions (regalloc-instructions ra)
                              :result-register (vm-program-result-register program)
                              :leaf-p (vm-program-leaf-p program))))
    (let ((*current-a64-regalloc* ra))
      (with-output-to-vector (stream)
        (emit-a64-program allocated-program stream)))))
