(in-package :cl-cc/emit)

(defstruct (ebpf-program-plan (:constructor make-ebpf-program-plan))
  program-name
  hook-point
  instructions
  uses-helpers-p
  verifier-safe-p)

(defconstant +ebpf-op-exit+ #x95)
(defconstant +ebpf-op-mov64-imm+ #xB7)
(defconstant +ebpf-op-add64-imm+ #x07)

(defun %u8 (n)
  (logand n #xff))

(defun %u16 (n)
  (logand n #xffff))

(defun %u32 (n)
  (logand n #xffffffff))

(defun %ebpf-emit-insn (opcode dst src off imm)
  "Emit a single 64-bit eBPF instruction encoding as 8 bytes."
  (let* ((off16 (%u16 off))
         (imm32 (%u32 imm)))
    (vector (%u8 opcode)
            (%u8 (logior (ash (%u8 src) 4) (%u8 dst)))
            (%u8 off16)
            (%u8 (ash off16 -8))
            (%u8 imm32)
            (%u8 (ash imm32 -8))
            (%u8 (ash imm32 -16))
            (%u8 (ash imm32 -24)))))

(defun %ebpf-lower-inst (inst)
  "Lower INST to a single encoded eBPF instruction.
Supported forms:
  (:exit)
  (:mov64-imm dst imm)
  (:add64-imm dst imm)"
  (unless (consp inst)
    (error "Invalid eBPF instruction: ~S" inst))
  (case (car inst)
    (:exit
     (%ebpf-emit-insn +ebpf-op-exit+ 0 0 0 0))
    (:mov64-imm
     (destructuring-bind (_ dst imm) inst
       (declare (ignore _))
       (%ebpf-emit-insn +ebpf-op-mov64-imm+ dst 0 0 imm)))
    (:add64-imm
     (destructuring-bind (_ dst imm) inst
       (declare (ignore _))
       (%ebpf-emit-insn +ebpf-op-add64-imm+ dst 0 0 imm)))
    (otherwise
     (error "Unsupported eBPF instruction form: ~S" inst))))

(defun %ebpf-instruction-heap-op-p (insn)
  "Conservative guard: reject obvious heap operations for eBPF lowering."
  (and (consp insn)
       (member (car insn) '(alloc malloc free make-array make-instance)
               :test #'eq)))

(defun plan-ebpf-program (program-name instructions &key (hook-point :xdp)
                                                   (uses-helpers-p t))
  "Build an eBPF plan while enforcing basic verifier-oriented constraints (FR-440)."
  (when (find-if #'%ebpf-instruction-heap-op-p instructions)
    (error "eBPF lowering rejects heap operations in ~S" program-name))
  (make-ebpf-program-plan :program-name program-name
                          :hook-point hook-point
                          :instructions instructions
                          :uses-helpers-p uses-helpers-p
                          :verifier-safe-p t))

(defun emit-ebpf-bytecode (instructions)
  "Encode INSTRUCTIONS to eBPF bytecode vector."
  (let ((chunks (mapcar #'%ebpf-lower-inst instructions))
        (out (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))
    (dolist (chunk chunks)
      (loop for b across chunk
            do (vector-push-extend b out)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun compile-ebpf-program (program-name instructions &key (hook-point :xdp)
                                                      (uses-helpers-p t))
  "Plan + encode an eBPF program into bytecode.
Returns two values: PLAN and BYTECODE." 
  (let ((plan (plan-ebpf-program program-name instructions
                                 :hook-point hook-point
                                 :uses-helpers-p uses-helpers-p)))
    (values plan (emit-ebpf-bytecode (ebpf-program-plan-instructions plan)))))
