;;;; packages/emit/src/x86-64-codegen-dispatch.lisp — Per-instruction emitters and dispatch table
;;;
;;; Contains the individual VM-instruction emitters that bridge from the VM
;;; instruction layer to the raw byte emitters in x86-64-emit-ops.lisp:
;;;   emit-vm-halt-inst, emit-vm-call-like-inst, emit-vm-tail-call-inst,
;;;   emit-vm-jump-inst, emit-vm-jump-zero-inst, emit-vm-ret-inst,
;;;   x86-64-used-callee-saved-regs, emit-vm-spill-store-inst, emit-vm-spill-load-inst
;;; and the alist/*x86-64-emitter-table* dispatch hash table
;;; and emit-vm-instruction-with-labels.
;;;
;;; Depends on x86-64-codegen.lisp (size table, build-label-offsets).
;;; Load order: after x86-64-codegen.lisp, before emit-vm-program users.

(in-package :cl-cc/codegen)

(defun emit-x86-64-cfi-indirect-target-guard (func-reg stream)
  "Emit conservative x86-64 CFI guard for an indirect branch target in FUNC-REG.

Checks:
1) target != NULL
2) first 4 bytes at target match ENDBR64 signature

When FUNC-REG is RAX, use RCX as scratch to avoid clobbering the branch target."
  ;; null target check
  (emit-test-rr64 func-reg func-reg stream)
  ;; JNE +2 => non-zero target skips UD2
  (emit-byte #x0F stream)
  (emit-byte #x85 stream)
  (emit-dword 2 stream)
  (emit-byte #x0F stream)
  (emit-byte #x0B stream)
  ;; ENDBR64 signature check
  (let ((scratch (if (= func-reg +rax+) +rcx+ +rax+)))
    (emit-mov-rm64 scratch func-reg 0 stream)
    (emit-cmp-ri32 scratch #xFA1E0FF3 stream)
    ;; JE +2 => valid entry marker skips trap
    (emit-byte #x0F stream)
    (emit-byte #x84 stream)
    (emit-dword 2 stream)
    (emit-byte #x0F stream)
    (emit-byte #x0B stream)))

;;; VM Instruction Emitters (with label support)

(defun x86-64-bce-eliminable-inst-p (inst)
  "Return T when BCE metadata proves INST's array bounds check redundant."
  (let ((metadata (opt-bounds-check-eliminable-metadata inst)))
    (or (getf metadata :bce-eliminable)
        (getf metadata :bounds-check-eliminable))))

(defun emit-vm-array-bounds-check-if-needed (inst stream &optional reg-name-fn)
  "Emit an array bounds check for INST unless BCE proved it redundant.

  Returns true when a check was emitted and NIL when BCE metadata allowed the
  caller to use the unchecked access path.  Textual x86-64 lowering consumes this
  hook directly; encoded native lowering must use the same predicate before
  emitting any compare/jump guard sequence."
  (flet ((reg-name (reg)
           (if reg-name-fn
               (funcall reg-name-fn reg)
               (string-downcase (symbol-name reg)))))
    (if (x86-64-bce-eliminable-inst-p inst)
        nil
        (progn
          (when *x86-64-spectre-mitigations-enabled*
            ;; LFENCE serializes before the bounds-dependent load/store path.
            (format stream "  lfence~%"))
          (format stream "  cmp ~A, [~A + 0]~%"
                  (reg-name (vm-index-reg inst))
                  (reg-name (vm-array-reg inst)))
          (format stream "  jae clcc_array_bounds_trap~%")
          t))))

(defun emit-vm-halt-inst (inst stream)
  "Emit code for VM HALT instruction.
   Moves the result register to RAX for the return value."
  (if (x86-64-float-vreg-p (vm-reg inst))
      (let ((result-reg (vm-reg-to-xmm (vm-reg inst))))
        (unless (= result-reg +xmm0+)
          (emit-movsd-xx +xmm0+ result-reg stream)))
      (let ((result-reg (vm-reg-to-x86 (vm-reg inst))))
        (unless (= result-reg +rax+)
          (emit-mov-rr64 +rax+ result-reg stream)))))

(defun emit-vm-call-like-inst (inst stream)
  "Emit code for VM CALL / VM TAIL-CALL instructions.

   The function designator is already in a register. We perform an indirect
   CALL through that register and then copy the return value from RAX into the
   destination register."
  (let ((func (vm-reg-to-x86 (vm-func-reg inst))))
    ;; Conservative CFI guard: null indirect target traps immediately.
    (when *x86-64-cfi-enabled*
      (emit-x86-64-cfi-indirect-target-guard func stream))
    (if *x86-64-use-retpoline*
        ;; Retpoline call-safe lowering:
        ;;   call .+0              ; get RIP
        ;;   pop  r11
        ;;   add  r11, 5           ; callee return target = after-call
        ;;   push r11
        ;;   call setup
        ;; after-call:
        ;;   mov  dst, rax
        ;;   jmp  end              ; skip thunk region on normal return
        ;; capture:
        ;;   pause; lfence; jmp capture
        ;; setup:
        ;;   mov [rsp], func
        ;;   ret
        ;; end:
        (progn
          ;; call .+0
          (emit-byte #xE8 stream)
          (emit-dword 0 stream)
          ;; pop r11
          (emit-pop-r64 +r11+ stream)
          ;; add r11, 16 (from call .+0 return address to after-call)
          (emit-add-ri32 +r11+ 16 stream)
          ;; push r11 (callee return target)
          (emit-push-r64 +r11+ stream)
          ;; call setup (setup starts 18 bytes after post-call PC)
          (emit-byte #xE8 stream)
          (emit-dword 18 stream)
          ;; after-call
          (if (x86-64-float-vreg-p (vm-dst inst))
              (emit-movsd-xx (vm-reg-to-xmm (vm-dst inst)) +xmm0+ stream)
              (emit-mov-rr64 (vm-reg-to-x86 (vm-dst inst)) +rax+ stream))
          ;; jump over thunk region to end
          (emit-jmp-rel32 15 stream)
          ;; thunk capture loop
          (emit-byte #xF3 stream) (emit-byte #x90 stream) ; PAUSE
          (emit-byte #x0F stream) (emit-byte #xAE stream) (emit-byte #xE8 stream) ; LFENCE
          (emit-jmp-rel32 -10 stream)
          ;; setup: write target and return
          (emit-mov-mr64 +rsp+ 0 func stream)
          (emit-ret stream))
        (emit-call-r64 func stream))
    (unless *x86-64-use-retpoline*
      (if (x86-64-float-vreg-p (vm-dst inst))
          (emit-movsd-xx (vm-reg-to-xmm (vm-dst inst)) +xmm0+ stream)
          (emit-mov-rr64 (vm-reg-to-x86 (vm-dst inst)) +rax+ stream)))))

(defun emit-vm-tail-call-inst (inst stream)
  "Emit code for VM TAIL-CALL instruction.

   Tail calls transfer control directly to the callee so the current stack
   frame is not extended. The callee returns to the original caller, so no
   destination move is emitted here."
  (let ((dst (vm-dst inst)))
    (declare (ignore dst))
    (let ((func (vm-reg-to-x86 (vm-func-reg inst))))
      ;; Conservative CFI guard: null indirect target traps immediately.
      (when *x86-64-cfi-enabled*
        (emit-x86-64-cfi-indirect-target-guard func stream))
      (if *x86-64-use-retpoline*
          ;; Retpoline tail-call lowering:
          ;;   call setup
          ;; capture:
          ;;   pause; lfence; jmp capture
          ;; setup:
          ;;   mov [rsp], func
          ;;   ret
          (progn
            (emit-byte #xE8 stream)
            (emit-dword 10 stream)
            (emit-byte #xF3 stream) (emit-byte #x90 stream) ; PAUSE
            (emit-byte #x0F stream) (emit-byte #xAE stream) (emit-byte #xE8 stream) ; LFENCE
            (emit-jmp-rel32 -10 stream)
            (emit-mov-mr64 +rsp+ 0 func stream)
          (emit-ret stream))
          (emit-jmp-r64 func stream)))))

(defun emit-vm-shadow-stack-control-inst (inst stream)
  "Emit CET shadow-stack transition sequences for non-local control ops.

These instructions participate in exception/restart control flow and require
dedicated native lowering to preserve shadow-stack semantics. Current lowering
materializes explicit CET SS instructions under the shadow-stack gate:

- Shadow-stack enabled  => emit CET SS save/restore/adjust instruction slots.
- Shadow-stack disabled => 2-byte NOP placeholder (reserved integration slot)."
  (if *x86-64-shadow-stack-enabled*
      (cond
        ;; handler/restart push-like sites => SAVEPREVSSP (4B) + NOP2 (2B) = 6B
        ((or (typep inst 'cl-cc/vm::vm-push-handler)
             (typep inst 'cl-cc/vm::vm-bind-restart)
             (typep inst 'cl-cc/vm::vm-establish-handler)
             (typep inst 'cl-cc/vm::vm-establish-catch))
         ;; F3 0F 01 EA = SAVEPREVSSP
         (emit-byte #xF3 stream)
         (emit-byte #x0F stream)
         (emit-byte #x01 stream)
         (emit-byte #xEA stream)
         (emit-byte #x66 stream)
         (emit-byte #x90 stream))
        ;; pop/invoke restart sites => RSTORSSP [RAX] (4B) + NOP2 (2B) = 6B
        ((or (typep inst 'cl-cc/vm::vm-pop-handler)
             (typep inst 'cl-cc/vm::vm-invoke-restart)
             (typep inst 'cl-cc/vm::vm-remove-handler))
         ;; F3 0F 01 /5, ModRM 00 101 000 = 0x28 => RSTORSSP [RAX]
         (emit-byte #xF3 stream)
         (emit-byte #x0F stream)
         (emit-byte #x01 stream)
         (emit-byte #x28 stream)
         (emit-byte #x66 stream)
         (emit-byte #x90 stream))
        ;; condition/error paths => INCSSPQ RAX (5B) + NOP1 (1B) = 6B
        (t
         ;; F3 48 0F AE /5 with ModRM 11 101 000 = 0xE8 => INCSSPQ RAX
         (emit-byte #xF3 stream)
         (emit-byte #x48 stream)
         (emit-byte #x0F stream)
         (emit-byte #xAE stream)
         (emit-byte #xE8 stream)
         (emit-byte #x90 stream)))
      (progn
        ;; Keep fixed 2-byte footprint for label-size accounting.
        (emit-byte #x66 stream)
        (emit-byte #x90 stream))))

(defun fits-in-rel8-p (offset)
  "Return T when OFFSET is encodable as a signed 8-bit branch displacement."
  (typep offset '(signed-byte 8)))

(defun emit-branch-short (kind offset stream)
  "Emit a two-byte x86-64 short branch KIND with signed rel8 OFFSET."
  (unless (fits-in-rel8-p offset)
    (error "Short branch displacement out of range: ~D" offset))
  (emit-byte (ecase kind
               (:jmp #xEB)
               (:je #x74)
               (:jne #x75)
               (:jl #x7C)
               (:jg #x7F)
               (:jle #x7E)
               (:jge #x7D))
             stream)
  (emit-byte (logand offset #xFF) stream))

(defun emit-branch-relaxed (kind offset inst stream)
  "Emit branch KIND for INST using its relaxed :SHORT/:NEAR encoding choice."
  (ecase (x86-64-branch-encoding inst)
    (:short (emit-branch-short kind offset stream))
    (:near
     (ecase kind
       (:jmp (emit-jmp-rel32 offset stream))
       (:je (emit-je-rel32 offset stream))))))

(defun emit-vm-jump-inst (inst stream current-pos label-offsets)
  "Emit code for VM JUMP instruction (unconditional jump)."
  (let* ((target-label (vm-label-name inst))
          (target-pos (gethash target-label label-offsets))
          (size (instruction-size inst))
          (offset (- target-pos (+ current-pos size))))
    (emit-branch-relaxed :jmp offset inst stream)))

(defun emit-vm-jump-zero-inst (inst stream current-pos label-offsets)
  "Emit code for VM JUMP-ZERO instruction (jump if register is zero).
   TEST reg, reg + optional static branch hint prefix + JE rel32"
  (let* ((reg (vm-reg-to-x86 (vm-reg inst)))
          (target-label (vm-label-name inst))
          (target-pos (gethash target-label label-offsets))
           (size (instruction-size inst))
           (offset (- target-pos (+ current-pos size))))
     ;; TEST reg, reg (sets ZF if reg is 0)
     (emit-test-rr64 reg reg stream)
     (emit-x86-64-unlikely-branch-prefix inst stream)
      ;; JE (jump if zero flag set), relaxed to rel8 when possible.
      (emit-branch-relaxed :je offset inst stream)))

(defun emit-vm-ret-inst (inst stream)
  "Emit code for VM RET instruction."
  (declare (ignore inst))
  (emit-ret stream))

(defun x86-64-used-callee-saved-regs (ra cc)
  "Return the callee-saved physical registers used by RA, in ABI order.
    When FPE exposes RBP as a general register, used RBP is preserved here like
    any other callee-saved register."
  (let ((callee-saved (target-callee-saved cc))
        (used nil))
    (loop for phys being the hash-values of (regalloc-assignment ra)
          when (member phys callee-saved)
          do (pushnew phys used :test #'eq))
    (loop for reg in callee-saved
          when (member reg used :test #'eq)
          collect (let ((entry (assoc reg *phys-reg-to-x86-code*)))
                    (or (cdr entry)
                        (error "Unknown x86-64 callee-saved register: ~A" reg))))))

(defun emit-vm-spill-store-inst (inst stream)
  "Emit code for VM SPILL-STORE instruction using the active spill base register."
  (let* ((src-reg (vm-spill-src inst))
         (offset (x86-64-spill-slot-offset (vm-spill-slot inst)))
         (gpr-entry (assoc src-reg *phys-reg-to-x86-code*))
         (fp-entry (assoc src-reg *phys-fp-reg-to-x86-code*)))
    (cond
      (gpr-entry
       (emit-mov-mr64 *current-spill-base-reg* offset (cdr gpr-entry) stream))
      (fp-entry
       (emit-movsd-mx *current-spill-base-reg* offset (cdr fp-entry) stream))
      (t
       (error "Unknown physical register for spill store: ~A" src-reg)))))

(defun emit-vm-spill-load-inst (inst stream)
  "Emit code for VM SPILL-LOAD instruction using the active spill base register."
  (let* ((dst-reg (vm-spill-dst inst))
         (offset (x86-64-spill-slot-offset (vm-spill-slot inst)))
         (gpr-entry (assoc dst-reg *phys-reg-to-x86-code*))
         (fp-entry (assoc dst-reg *phys-fp-reg-to-x86-code*)))
    (cond
      (gpr-entry
       (emit-mov-rm64 (cdr gpr-entry) *current-spill-base-reg* offset stream))
      (fp-entry
       (emit-movsd-xm (cdr fp-entry) *current-spill-base-reg* offset stream))
      (t
       (error "Unknown physical register for spill load: ~A" dst-reg)))))

(defun emit-x86-64-lea-address-inst (inst stream)
  "Emit internal LEA instruction produced by x86-64 peephole lowering."
  (emit-lea (x86-64-lea-address-dst inst)
            (x86-64-lea-address-base inst)
            (x86-64-lea-address-index inst)
            (x86-64-lea-address-scale inst)
             (x86-64-lea-address-displacement inst)
             stream))

(defparameter *x86-64-emitter-entries*
  '(;; Core instructions
    (vm-const        . emit-vm-const)
    (x86-64-lea-address . emit-x86-64-lea-address-inst)
    (x86-64-bextr-field . emit-x86-64-bextr-field-inst)
    (vm-move         . emit-vm-move)
    (vm-add          . emit-vm-add)
    ;; FR-171: vm-integer-add uses LEA optimization (active)
    (vm-integer-add  . emit-vm-integer-add)
    (vm-float-add    . emit-vm-float-add)
    (vm-sub          . emit-vm-sub)
    (vm-integer-sub  . emit-vm-sub)
    (vm-float-sub    . emit-vm-float-sub)
    (vm-mul          . emit-vm-mul)
    (vm-integer-mul  . emit-vm-mul)
    (vm-integer-mul-high-u . emit-vm-integer-mul-high-u)
    (vm-integer-mul-high-s . emit-vm-integer-mul-high-s)
    ;; Checked arithmetic (FR-303 overflow detection)
    (vm-add-checked  . emit-vm-add-checked)
    (vm-sub-checked  . emit-vm-sub-checked)
    (vm-mul-checked  . emit-vm-mul-checked)
    (vm-float-mul    . emit-vm-float-mul)
    (vm-fma          . emit-vm-fma)
    (vm-float-div    . emit-vm-float-div)
    (vm-sqrt         . emit-vm-sqrt)
    (vm-sin-inst     . emit-vm-sin)
    (vm-cos-inst     . emit-vm-cos)
    (vm-exp-inst     . emit-vm-exp)
    (vm-log-inst     . emit-vm-log)
    (vm-tan-inst     . emit-vm-tan)
    (vm-asin-inst    . emit-vm-asin)
    (vm-acos-inst    . emit-vm-acos)
    (vm-atan-inst    . emit-vm-atan)
    ;; FR-298: vm-print with real I/O (indirect call through R11)
    (vm-print        . emit-vm-print)
    (vm-halt         . emit-vm-halt-inst)
     (vm-call         . emit-vm-call-like-inst)
     (vm-tail-call    . emit-vm-tail-call-inst)
    (vm-ret          . emit-vm-ret-inst)
    (vm-spill-store  . emit-vm-spill-store-inst)
    (vm-spill-load   . emit-vm-spill-load-inst)
    ;; Array access: scaled indexed memory operands [base + index*scale + disp]
    (vm-aref         . emit-vm-aref)
    (vm-aset         . emit-vm-aset)
    (vm-prefetch     . emit-vm-prefetch)
    (vm-simd-vector-op . emit-vm-simd-vector-op)
    ;; Comparison
    (vm-lt           . emit-vm-lt)
    (vm-gt           . emit-vm-gt)
    (vm-le           . emit-vm-le)
    (vm-ge           . emit-vm-ge)
    (vm-num-eq       . emit-vm-num-eq)
    (vm-eq           . emit-vm-eq)
    ;; Unary arithmetic/logical
    (vm-neg          . emit-vm-neg)
     (vm-not          . emit-vm-not)
     (vm-lognot       . emit-vm-lognot)
     (vm-logcount     . emit-vm-logcount)
     (vm-integer-length . emit-vm-integer-length)
     (vm-bswap        . emit-vm-bswap)
    (vm-inc          . emit-vm-inc)
    (vm-dec          . emit-vm-dec)
    (vm-abs          . emit-vm-abs)
    ;; Min/max/ash
    (vm-min          . emit-vm-min)
    (vm-max          . emit-vm-max)
    (vm-select       . emit-vm-select)
    (vm-ash          . emit-vm-ash)
    (vm-rotate       . emit-vm-rotate)
    ;; Integer division (IDIV-based)
    (vm-truncate     . emit-vm-truncate)
    (vm-rem          . emit-vm-rem)
    (vm-div          . emit-vm-div)
    (vm-mod          . emit-vm-mod)
    ;; Boolean logical
    (vm-and          . emit-vm-and)
    (vm-or           . emit-vm-or)
    ;; Binary logical
    (vm-logand       . emit-vm-logand)
    (vm-logior       . emit-vm-logior)
    (vm-logxor       . emit-vm-logxor)
    (vm-logeqv       . emit-vm-logeqv)
    (vm-logtest      . emit-vm-logtest)
    (vm-logbitp      . emit-vm-logbitp)
    ;; Type predicates
    (vm-null-p       . emit-vm-null-p)
    (vm-number-p     . emit-vm-true-pred)
    (vm-integer-p    . emit-vm-true-pred)
    (vm-cons-p       . emit-vm-false-pred)
    (vm-symbol-p     . emit-vm-false-pred)
    (vm-function-p   . emit-vm-false-pred)
    ;; Non-local control flow (FR-318 integration safety path)
    (cl-cc/vm::vm-push-handler   . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-pop-handler    . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-bind-restart   . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-invoke-restart . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-signal         . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-error-instruction . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-cerror         . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-warn           . emit-vm-shadow-stack-control-inst)
    ;; VM non-local control ops from vm-run.lisp
    (cl-cc/vm::vm-establish-handler . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-remove-handler    . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-sync-handler-regs . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-signal-error      . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-establish-catch   . emit-vm-shadow-stack-control-inst)
    (cl-cc/vm::vm-throw             . emit-vm-shadow-stack-control-inst)
    ;; FR-073: Multiple values via registers (active)
    (vm-values-regs   . emit-vm-values-regs)
    (vm-mv-bind-regs  . emit-vm-mv-bind-regs))
  "Alist mapping VM instruction type symbols to emitter function names.")

(defparameter *x86-64-emitter-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (entry *x86-64-emitter-entries* ht)
      (setf (gethash (car entry) ht) (symbol-function (cdr entry)))))
  "Maps VM instruction type symbols to emitter functions (inst stream).")

(defun emit-vm-instruction-with-labels (inst stream current-pos label-offsets)
  "Emit machine code for a VM instruction, with label/jump support."
  (let ((tp (type-of inst)))
    (cond
      ;; No-op instructions
      ((eq tp 'vm-label) nil)
      ;; Jump instructions need extra args (current-pos, label-offsets)
      ((eq tp 'vm-jump)
       (emit-vm-jump-inst inst stream current-pos label-offsets))
      ((eq tp 'vm-jump-zero)
       (emit-vm-jump-zero-inst inst stream current-pos label-offsets))
      ;; Table-driven dispatch for all (inst stream) emitters
      (t (let ((emitter (gethash tp *x86-64-emitter-table*)))
           (if emitter
               (funcall emitter inst stream)
               (error "Unsupported x86-64 instruction: ~A" tp)))))))
