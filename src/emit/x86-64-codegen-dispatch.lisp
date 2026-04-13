;;;; src/emit/x86-64-codegen-dispatch.lisp — Per-instruction emitters and dispatch table
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

(in-package :cl-cc)

;;; VM Instruction Emitters (with label support)

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
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (func (vm-reg-to-x86 (vm-func-reg inst))))
    (emit-call-r64 func stream)
    (emit-mov-rr64 dst +rax+ stream)))

(defun emit-vm-tail-call-inst (inst stream)
  "Emit code for VM TAIL-CALL instruction.

   Tail calls transfer control directly to the callee so the current stack
   frame is not extended. The callee returns to the original caller, so no
   destination move is emitted here."
  (let ((dst (vm-dst inst)))
    (declare (ignore dst))
    (emit-jmp-r64 (vm-reg-to-x86 (vm-func-reg inst)) stream)))

(defun emit-vm-jump-inst (inst stream current-pos label-offsets)
  "Emit code for VM JUMP instruction (unconditional jump)."
  (let* ((target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         ;; JMP rel32 is 5 bytes, offset is relative to end of instruction
         (offset (- target-pos (+ current-pos 5))))
    (emit-jmp-rel32 offset stream)))

(defun emit-vm-jump-zero-inst (inst stream current-pos label-offsets)
  "Emit code for VM JUMP-ZERO instruction (jump if register is zero).
   TEST reg, reg + JE rel32"
  (let* ((reg (vm-reg-to-x86 (vm-reg inst)))
         (target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         ;; TEST is 3 bytes, JE rel32 is 6 bytes, total 9 bytes
         (offset (- target-pos (+ current-pos 9))))
    ;; TEST reg, reg (sets ZF if reg is 0)
    (emit-test-rr64 reg reg stream)
    ;; JE rel32 (jump if zero flag set)
    (emit-je-rel32 offset stream)))

(defun emit-vm-ret-inst (inst stream)
  "Emit code for VM RET instruction."
  (declare (ignore inst))
  (emit-ret stream))

(defun x86-64-used-callee-saved-regs (ra cc)
  "Return the callee-saved physical registers used by RA, in ABI order.
   RBP is always preserved separately as the frame pointer."
  (let ((callee-saved (cc-callee-saved cc))
        (used nil))
    (loop for phys being the hash-values of (regalloc-assignment ra)
          when (member phys callee-saved)
          do (pushnew phys used :test #'eq))
    (loop for reg in callee-saved
          when (member reg used :test #'eq)
          collect reg)))

(defun emit-vm-spill-store-inst (inst stream)
  "Emit code for VM SPILL-STORE instruction using the active spill base register."
  (let* ((src-reg (vm-spill-src inst))
         (src-code (let ((entry (assoc src-reg *phys-reg-to-x86-code*)))
                     (if entry (cdr entry)
                          (error "Unknown physical register for spill store: ~A" src-reg))))
          (offset (- (* (vm-spill-slot inst) 8))))
    (emit-mov-mr64 *current-spill-base-reg* offset src-code stream)))

(defun emit-vm-spill-load-inst (inst stream)
  "Emit code for VM SPILL-LOAD instruction using the active spill base register."
  (let* ((dst-reg (vm-spill-dst inst))
         (dst-code (let ((entry (assoc dst-reg *phys-reg-to-x86-code*)))
                     (if entry (cdr entry)
                          (error "Unknown physical register for spill load: ~A" dst-reg))))
          (offset (- (* (vm-spill-slot inst) 8))))
    (emit-mov-rm64 dst-code *current-spill-base-reg* offset stream)))

(defparameter *x86-64-emitter-entries*
  '(;; Core instructions
    (vm-const        . emit-vm-const)
    (vm-move         . emit-vm-move)
    (vm-add          . emit-vm-add)
    (vm-integer-add  . emit-vm-add)
    (vm-float-add    . emit-vm-float-add)
    (vm-sub          . emit-vm-sub)
    (vm-integer-sub  . emit-vm-sub)
    (vm-float-sub    . emit-vm-float-sub)
    (vm-mul          . emit-vm-mul)
    (vm-integer-mul  . emit-vm-mul)
    (vm-float-mul    . emit-vm-float-mul)
    (vm-float-div    . emit-vm-float-div)
    (vm-halt         . emit-vm-halt-inst)
     (vm-call         . emit-vm-call-like-inst)
     (vm-tail-call    . emit-vm-tail-call-inst)
    (vm-ret          . emit-vm-ret-inst)
    (vm-spill-store  . emit-vm-spill-store-inst)
    (vm-spill-load   . emit-vm-spill-load-inst)
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
    (vm-function-p   . emit-vm-false-pred))
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
      ((or (eq tp 'vm-label) (eq tp 'vm-print)) nil)
      ;; Jump instructions need extra args (current-pos, label-offsets)
      ((eq tp 'vm-jump)
       (emit-vm-jump-inst inst stream current-pos label-offsets))
      ((eq tp 'vm-jump-zero)
       (emit-vm-jump-zero-inst inst stream current-pos label-offsets))
      ;; Table-driven dispatch for all (inst stream) emitters
      (t (let ((emitter (gethash tp *x86-64-emitter-table*)))
           (if emitter
               (funcall emitter inst stream)
               (warn "Skipping unsupported VM instruction: ~A" tp)))))))
