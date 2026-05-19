;;;; packages/emit/src/aarch64-program.lisp - AArch64 Program-Level Emission
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

(in-package :cl-cc/codegen)

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
    (setf (gethash 'vm-truncate ht) #'emit-a64-vm-truncate)
    (setf (gethash 'vm-float-add ht) #'emit-a64-vm-float-add)
    (setf (gethash 'vm-float-sub ht) #'emit-a64-vm-float-sub)
    (setf (gethash 'vm-float-mul ht) #'emit-a64-vm-float-mul)
    (setf (gethash 'vm-float-div ht) #'emit-a64-vm-float-div)
    (setf (gethash 'vm-fma ht) #'emit-a64-vm-fma)
    (setf (gethash 'vm-integer-mul-high-u ht) #'emit-a64-vm-integer-mul-high-u)
    (setf (gethash 'vm-integer-mul-high-s ht) #'emit-a64-vm-integer-mul-high-s)
    ;; Checked arithmetic (FR-303 overflow detection)
    (setf (gethash 'vm-add-checked ht) #'emit-a64-vm-add-checked)
    (setf (gethash 'vm-sub-checked ht) #'emit-a64-vm-sub-checked)
    (setf (gethash 'vm-mul-checked ht) #'emit-a64-vm-mul-checked)
    (setf (gethash 'vm-sqrt ht) #'emit-a64-vm-sqrt)
    (setf (gethash 'vm-sin-inst ht) #'emit-a64-vm-sin)
    (setf (gethash 'vm-cos-inst ht) #'emit-a64-vm-cos)
    (setf (gethash 'vm-exp-inst ht) #'emit-a64-vm-exp)
    (setf (gethash 'vm-log-inst ht) #'emit-a64-vm-log)
    (setf (gethash 'vm-tan-inst ht) #'emit-a64-vm-tan)
    (setf (gethash 'vm-asin-inst ht) #'emit-a64-vm-asin)
    (setf (gethash 'vm-acos-inst ht) #'emit-a64-vm-acos)
    (setf (gethash 'vm-atan-inst ht) #'emit-a64-vm-atan)
    (setf (gethash 'vm-min ht) #'emit-a64-vm-min)
    (setf (gethash 'vm-max ht) #'emit-a64-vm-max)
    (setf (gethash 'vm-select ht) #'emit-a64-vm-select)
    (setf (gethash 'vm-bswap ht) #'emit-a64-vm-bswap)
    (setf (gethash 'vm-rotate ht) #'emit-a64-vm-rotate)
    (setf (gethash 'vm-halt ht) #'emit-a64-vm-halt)
    (setf (gethash 'vm-ret ht) #'emit-a64-vm-ret)
    (setf (gethash 'vm-spill-store ht) #'emit-a64-vm-spill-store)
    (setf (gethash 'vm-spill-load ht) #'emit-a64-vm-spill-load)
    (setf (gethash 'vm-prefetch ht) #'emit-a64-vm-prefetch)
    ht)
  "Maps VM instruction type symbols to AArch64 emitter functions (inst stream).")

(defun emit-a64-instruction (inst stream current-pos label-offsets)
  "Emit AArch64 machine code for one VM instruction."
  (let ((tp (type-of inst)))
    (cond
      ((or (eq tp 'vm-label)
           (eq tp 'vm-print)
           (eq tp 'vm-closure)
           (eq tp 'vm-register-function)
           (eq tp 'vm-set-global))
       nil)
      ((eq tp 'vm-const)
       (emit-a64-vm-const-at inst stream current-pos))
      ((eq tp 'vm-jump)
       (emit-a64-vm-jump inst stream current-pos label-offsets))
      ((eq tp 'vm-jump-zero)
       (emit-a64-vm-jump-zero inst stream current-pos label-offsets))
      ((eq tp 'vm-call)
       (emit-a64-instr (encode-blr (a64-reg (vm-func-reg inst))) stream))
      ((eq tp 'vm-tail-call)
       (emit-a64-instr (encode-br (a64-reg (vm-func-reg inst))) stream))
      ((member tp '(vm-sin-inst vm-cos-inst vm-exp-inst vm-log-inst
                    vm-tan-inst vm-asin-inst vm-acos-inst vm-atan-inst)
               :test #'eq)
       (funcall (gethash tp *a64-emitter-table*) inst stream current-pos))
      (t (let ((emitter (gethash tp *a64-emitter-table*)))
            (if emitter
                (funcall emitter inst stream)
                 (error "Unsupported AArch64 instruction: ~A" tp)))))))

(defun a64-libm-function-for-instruction (inst)
  "Return the libm symbol name used by INST, or NIL."
  (case (type-of inst)
    (vm-sin-inst "sin")
    (vm-cos-inst "cos")
    (vm-exp-inst "exp")
    (vm-log-inst "log")
    (vm-tan-inst "tan")
    (vm-asin-inst "asin")
    (vm-acos-inst "acos")
    (vm-atan-inst "atan")
    (otherwise nil)))

(defun a64-collect-literal-pool (instructions)
  "Collect all large AArch64 constants and absolute addresses in INSTRUCTIONS."
  (let ((pool (make-a64-literal-pool)))
    (dolist (inst instructions pool)
      (cond
        ((typep inst 'vm-const)
         (let ((value (logand (vm-value inst) #xFFFFFFFFFFFFFFFF)))
           (when (a64-literal-pool-value-p value)
             (a64-pool-add pool value))))
        ((a64-libm-function-for-instruction inst)
         (a64-pool-add pool (a64-libm-address (a64-libm-function-for-instruction inst))))))))

(defun a64-program-instruction-bytes (instructions)
  "Return the byte size of encoded INSTRUCTIONS."
  (loop for inst in instructions sum (a64-instruction-size inst)))

(defun a64-island-size-at (pool pos)
  "Return byte size of a branch-around constant island at POS."
  (if (a64-literal-pool-entries pool)
      (let* ((data-pos (a64-pool-align-offset (+ pos 4)))
             (pad (- data-pos (+ pos 4))))
        (+ 4 pad (* 8 (length (a64-literal-pool-entries pool)))))
      0))

(defun a64-compute-labels-and-islands (instructions prologue-size pool)
  "Compute label offsets and constant-island insertion points.

For large functions, islands are inserted immediately before basic-block labels
once the distance from the previous island exceeds the FR-268 threshold.  Each
island duplicates the function literal pool and is skipped by a short B."
  (let ((offsets (make-hash-table :test #'equal))
        (island-sites nil)
        (island-bases nil)
        (pos prologue-size)
        (last-island-pos prologue-size))
    (dolist (inst instructions)
      (when (and (a64-literal-pool-entries pool)
                 (typep inst 'vm-label)
                 (> (- pos last-island-pos) +a64-literal-island-threshold+))
        (let ((base (a64-pool-align-offset (+ pos 4))))
          (push pos island-sites)
          (push base island-bases)
          (incf pos (a64-island-size-at pool pos))
          (setf last-island-pos pos)))
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) offsets) pos))
      (incf pos (a64-instruction-size inst)))
    (values offsets (nreverse island-sites) (nreverse island-bases) pos)))

(defun a64-constant-island-plan (instructions prologue-size island-bases body-end)
  "Return conservative constant-island metadata for AArch64 literal pools.

ADRP addresses the literal page directly, so the LDR unsigned immediate is the
literal's page offset rather than the distance from the load.  The plan still
tracks the 32 KiB island threshold so future block-local island insertion can
split this end-of-function pool without changing collection semantics."
  (declare (ignore instructions))
  (let ((function-bytes body-end))
    (list :large-function-p (> (- function-bytes prologue-size) +a64-literal-island-threshold+)
          :island-count (length island-bases)
          :threshold +a64-literal-island-threshold+
          :function-bytes function-bytes)))

(defun aarch64-cfi-plan (&key has-indirect-calls-p)
  "Return AArch64 CFI entry planning metadata.

Result plist keys:
- :enabled-p whether CFI entry marker is enabled
- :entry-opcode marker opcode keyword (`:bti-c` or `:none`)"
  (let* ((plan (opt-build-cfi-plan
                :target :aarch64
                :has-indirect-calls-p has-indirect-calls-p))
         (entry-opcode (opt-cfi-entry-opcode plan)))
    (list :enabled-p (eq entry-opcode :bti-c)
          :entry-opcode entry-opcode)))

(defun emit-aarch64-cfi-entry (stream cfi-plan)
  "Emit AArch64 CFI entry marker (BTI C) when enabled in CFI-PLAN."
  (when (eq (getf cfi-plan :entry-opcode) :bti-c)
    ;; BTI C = 0xD503245F
    (emit-a64-instr #xD503245F stream)))

;;; AArch64 callee-saved registers to save/restore (X19-X28, X29, X30)
;;; X29 = frame pointer (FP), X30 = link register (LR)
;;; Saved as pairs: (X29,X30), (X19,X20), (X21,X22), (X23,X24), (X25,X26), (X27,X28)
;;; Total prologue size: 6 STP x 4 bytes = 24 bytes

(defun a64-used-callee-saved-pairs (ra &key frame-pointer-p)
  "Return callee-saved register pairs actually used by RA.
   When FRAME-POINTER-P is true, include the FP/LR pair first.
   Each pair is emitted as one STP/LDP."
  (let ((phys-regs (loop for phys being the hash-values of (regalloc-assignment ra)
                          for entry = (assoc phys *aarch64-reg-number*)
                          when entry
                            collect (cdr entry))))
    (append (when (or frame-pointer-p (member +a64-fp+ phys-regs)) '((29 30)))
            (remove-if-not (lambda (pair)
                              (or (member (first pair) phys-regs)
                                  (member (second pair) phys-regs)))
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

(defun a64-stack-frame-size (save-pairs spill-count)
  "Return conservative stack-frame bytes represented by pair saves plus spill slots."
  (+ (* 16 (length save-pairs))
     (* 8 spill-count)))

(defun emit-a64-stack-probes (stream probe-count)
  "Emit one non-mutating page touch per PROBE-COUNT below SP."
  (loop for page from 1 to probe-count
        do (progn
             (emit-a64-instr (encode-sub-imm +a64-stack-probe-scratch+ +a64-sp+ page 1) stream)
             (emit-a64-instr (encode-ldur +a64-zr+ +a64-stack-probe-scratch+ 0) stream))))

(defun emit-a64-stack-allocate (stream spill-frame-size)
  "Reserve SP-relative spill space. Supports immediate adjustments up to 4095 bytes."
  (unless (<= spill-frame-size 4095)
    (error "AArch64 spill frame ~D exceeds ADD/SUB immediate range for minimal FPE support"
           spill-frame-size))
  (emit-a64-instr (encode-sub-imm +a64-sp+ +a64-sp+ spill-frame-size 0) stream))

(defun emit-a64-stack-deallocate (stream spill-frame-size)
  "Release SP-relative spill space reserved by EMIT-A64-STACK-ALLOCATE."
  (unless (<= spill-frame-size 4095)
    (error "AArch64 spill frame ~D exceeds ADD/SUB immediate range for minimal FPE support"
           spill-frame-size))
  (emit-a64-instr (encode-add-imm +a64-sp+ +a64-sp+ spill-frame-size 0) stream))

(defun emit-a64-program (program stream)
  "Emit AArch64 machine code for the entire VM program.
   Uses two-pass approach for label resolution."
  (let* ((instructions (vm-program-instructions program))
         (calling-convention (vm-program-calling-convention-object program))
          (has-indirect-calls-p
            (some (lambda (inst)
                    (typep inst '(or vm-call vm-tail-call vm-generic-call)))
                  instructions))
          (cfi-plan (aarch64-cfi-plan :has-indirect-calls-p has-indirect-calls-p))
          (cfi-entry-size (if (eq (getf cfi-plan :entry-opcode) :bti-c) 4 0))
           (leaf-p (vm-program-leaf-p program))
          (spill-count (regalloc-spill-count *current-a64-regalloc*))
          (frame-pointer-p (and (not *a64-omit-frame-pointer*)
                                (not (calling-convention-omit-frame-pointer-p calling-convention))
                                 (or (not leaf-p)
                                    (plusp spill-count))))
          (spill-frame-size (if (and (not frame-pointer-p)
                                     (plusp spill-count))
                                (* 8 spill-count)
                                0))
          (*current-a64-spill-base-reg* (if frame-pointer-p +a64-fp+ +a64-sp+))
          (*current-a64-spill-offset-bias* (if frame-pointer-p 0 spill-frame-size))
           (save-pairs (a64-used-callee-saved-pairs *current-a64-regalloc*
                                                     :frame-pointer-p frame-pointer-p))
           (probe-count (stack-probe-count
                         (a64-stack-frame-size save-pairs
                                               spill-count)))
           ;; Shadow call stack prologue is 1 instruction; each STP/LDP pair is 1 instruction.
           ;; Each stack probe is SUB-immediate + STUR, two 4-byte instructions.
           (spill-frame-adjust-size (if (plusp spill-frame-size) 4 0))
            (prologue-size (+ cfi-entry-size
                              (* 8 probe-count)
                              (* 4 (1+ (length save-pairs)))
                              spill-frame-adjust-size))
          (cfg (cfg-build instructions))
           (ordered-instructions (if (cfg-entry cfg)
                                     (progn
                                       (cfg-compute-dominators cfg)
                                       (cfg-compute-loop-depths cfg)
                                       (cfg-flatten-hot-cold cfg))
                                     instructions))
           (literal-pool (a64-collect-literal-pool ordered-instructions)))
      (multiple-value-bind (label-offsets island-sites island-bases body-end)
          (a64-compute-labels-and-islands ordered-instructions prologue-size literal-pool)
        (let* ((island-plan (a64-constant-island-plan ordered-instructions prologue-size
                                                      island-bases body-end))
            (stack-deallocate-size (if (plusp spill-frame-size) 4 0))
            (epilogue-size (+ (* 4 (length save-pairs)) 20))
               (pool-input-offset (+ body-end
                                  stack-deallocate-size epilogue-size)))
      (declare (ignore island-plan))
          (a64-finalize-literal-pool-islands literal-pool pool-input-offset island-bases)
      ;; CFI entry marker (FR-315): must be at function entry.
      (let ((*current-a64-literal-pool* literal-pool))
        (emit-aarch64-cfi-entry stream cfi-plan)

        ;; Stack probing: touch each guard page before the large frame is used.
        (emit-a64-stack-probes stream probe-count)
        ;; Prologue
        (emit-a64-prologue stream save-pairs)
        (when (plusp spill-frame-size)
          (emit-a64-stack-allocate stream spill-frame-size))
        ;; Second pass: emit instructions
            (let ((pos prologue-size)
                  (remaining-islands island-sites))
          (dolist (inst ordered-instructions)
                (when (and remaining-islands (= pos (first remaining-islands)))
                  (incf pos (a64-emit-constant-island literal-pool stream pos))
                  (pop remaining-islands))
            (emit-a64-instruction inst stream pos label-offsets)
            (incf pos (a64-instruction-size inst))))
        (when (plusp spill-frame-size)
          (emit-a64-stack-deallocate stream spill-frame-size))
        ;; Epilogue
        (emit-a64-epilogue stream save-pairs)
        (loop repeat (/ (- (a64-literal-pool-base-offset literal-pool)
                           pool-input-offset)
                        4)
              do (emit-a64-instr 0 stream))
            (a64-pool-emit literal-pool))))))

;;; Public API

(defun compile-to-aarch64-bytes (program &key retpoline stack-protector shadow-stack
                                         asan msan tsan ubsan hwasan)
  "Compile VM program to AArch64 machine code bytes.
   Returns: (simple-array (unsigned-byte 8) (*))"
  (declare (ignore retpoline stack-protector shadow-stack asan msan tsan ubsan hwasan))
  (let ((*current-calling-convention* (vm-program-calling-convention-object program)))
  (let* ((instructions (schedule-pre-ra (vm-program-instructions program)))
            (ra (allocate-registers instructions (a64-codegen-target)))
          (allocated-program (make-vm-program
                              :instructions (regalloc-instructions ra)
                              :result-register (vm-program-result-register program)
                              :leaf-p (vm-program-leaf-p program)
                              :calling-convention (vm-program-calling-convention program)
                              :function-conventions (vm-program-function-conventions program))))
    (let ((*current-a64-regalloc* ra))
      (aarch64-peephole-optimize
       (with-output-to-vector (stream)
         (emit-a64-program allocated-program stream)))))))
