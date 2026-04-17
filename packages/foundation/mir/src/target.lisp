;;;; packages/foundation/mir/src/target.lisp — Unified Target Descriptor
;;;
;;; Replaces the ad-hoc calling-convention.lisp + scattered target state.
;;; One TARGET-DESC instance fully describes a compilation target:
;;; register file, calling convention, ABI constraints, and feature flags.
;;;
;;; Design: adding a new target requires only:
;;;   1. A TARGET-DESC instance  (this file or a new target file)
;;;   2. isel-rules/<target>.lisp  (instruction selection rules)
;;;   3. encode/<target>.lisp      (machine code encoder)
;;;
;;; Current predefined targets:
;;;   *x86-64-target*   — System V AMD64 ABI
;;;   *aarch64-target*  — AAPCS64
;;;   *riscv64-target*  — RV64GC + RISC-V ELF psABI
;;;   *wasm32-target*   — WebAssembly Component Model + WASI 0.2

(in-package :cl-cc/mir)

;;;; ────────────────────────────────────────────────────────────────────────
;;;; Target Descriptor Struct
;;;; ────────────────────────────────────────────────────────────────────────

(defstruct (target-desc (:conc-name target-))
  "Complete ABI + ISA description for one compilation target.

   LEGAL-OPS maps generic MIR op keywords to:
     T          — op is natively legal (isel handles it)
     NIL        — op is illegal (isel must signal an error)
     FUNCTION   — legalization function: (op dst srcs) → list of replacement insts

   FEATURES is a list of capability keywords read by isel rules and encoders:
     :has-native-tail-call     — return_call / TCO instruction exists
     :has-fused-cmp-branch     — compare + branch can be fused
     :has-mul-imm              — multiply-immediate is a single instruction
     :structured-control-flow  — target requires Stackifier (Wasm)
     :gc-proposal              — Wasm GC reference types
     :sysv-abi                 — System V calling convention
     :aapcs64                  — ARM AAPCS64 calling convention
     :riscv-elf-psabi          — RISC-V ELF psABI calling convention
     :wasi-0.2                 — WASI Preview 2 system interface"
  (name            nil  :type keyword)  ; :x86-64 :aarch64 :riscv64 :wasm32
  (word-size       8    :type fixnum)   ; bytes per pointer/word (8=64-bit, 4=32-bit)
  (endianness      :little :type keyword) ; :little or :big
  (gpr-count       0    :type fixnum)   ; number of general-purpose registers
  (gpr-names       #()  :type vector)   ; vector of register name keywords (index = phys reg #)
  (arg-regs        nil  :type list)     ; argument-passing registers in calling-convention order
  (ret-reg         nil)                 ; primary return-value register
  (callee-saved    nil  :type list)     ; callee-saved registers (must be preserved by callees)
  (scratch-regs    nil  :type list)     ; reserved / non-allocatable (SP, link reg, etc.)
  (stack-alignment 16   :type fixnum)   ; required stack alignment in bytes; 0 = N/A
  (legal-ops                            ; generic-op → t / nil / legalization-fn
   (make-hash-table)
   :type hash-table)
  (features        nil  :type list))    ; capability keyword flags

;;;; ────────────────────────────────────────────────────────────────────────
;;;; x86-64 — System V AMD64 ABI
;;;; ────────────────────────────────────────────────────────────────────────
;;;
;;; References:
;;;   System V Application Binary Interface AMD64 Architecture Processor Supplement
;;;   Intel SDM Vol. 1–4
;;;
;;; Register layout (16 GPRs):
;;;   rax rcx rdx rbx rsp rbp rsi rdi r8–r15
;;;
;;; Calling convention:
;;;   Args:       rdi rsi rdx rcx r8 r9  (integer/pointer; first 6 in regs)
;;;   Return:     rax
;;;   Caller-saved: rax rcx rdx rsi rdi r8 r9 r10 r11
;;;   Callee-saved: rbx rbp r12 r13 r14 r15
;;;   Scratch (non-allocatable): rsp r11

(defvar *x86-64-target*
  (make-target-desc
    :name         :x86-64
    :word-size    8
    :endianness   :little
    :gpr-count    16
    :gpr-names    #(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi
                    :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
    :arg-regs     '(:rdi :rsi :rdx :rcx :r8 :r9)
    :ret-reg      :rax
    :callee-saved '(:rbx :rbp :r12 :r13 :r14 :r15)
    :scratch-regs '(:rsp :r11)           ; rsp = stack pointer, r11 = scratch for thunks
    :stack-alignment 16
    :features     '(:has-fused-cmp-branch :has-mul-imm :sysv-abi)))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; AArch64 — AAPCS64
;;;; ────────────────────────────────────────────────────────────────────────
;;;
;;; References:
;;;   Procedure Call Standard for the Arm 64-bit Architecture (AAPCS64)
;;;   Arm Architecture Reference Manual (DDI 0487)
;;;
;;; Register layout (31 named GPRs + xzr/sp):
;;;   x0–x30 (general); x29 = frame pointer, x30 = link register
;;;
;;; Calling convention:
;;;   Args:       x0–x7  (integer/pointer; first 8 in regs)
;;;   Return:     x0
;;;   Caller-saved: x0–x17
;;;   Callee-saved: x19–x28, x29 (FP)
;;;   Scratch: x16 x17 (inter-procedure scratch), x18 (platform), x30 (LR)

(defvar *aarch64-target*
  (make-target-desc
    :name         :aarch64
    :word-size    8
    :endianness   :little
    :gpr-count    31
    :gpr-names    #(:x0  :x1  :x2  :x3  :x4  :x5  :x6  :x7
                    :x8  :x9  :x10 :x11 :x12 :x13 :x14 :x15
                    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
                    :x24 :x25 :x26 :x27 :x28 :x29 :x30)
    :arg-regs     '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7)
    :ret-reg      :x0
    :callee-saved '(:x19 :x20 :x21 :x22 :x23 :x24 :x25 :x26 :x27 :x28 :x29)
    :scratch-regs '(:x16 :x17 :x18 :x30) ; x30 = link reg (saved separately)
    :stack-alignment 16
    :features     '(:has-native-tail-call :has-fused-cmp-branch :aapcs64)))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; RISC-V 64 — RV64GC + RISC-V ELF psABI
;;;; ────────────────────────────────────────────────────────────────────────
;;;
;;; References:
;;;   The RISC-V Instruction Set Manual, Volumes I and II
;;;   RISC-V ELF psABI Specification
;;;
;;; Register layout (32 GPRs, ABI names):
;;;   x0=zero  x1=ra   x2=sp   x3=gp   x4=tp
;;;   x5=t0    x6=t1   x7=t2   x8=s0/fp x9=s1
;;;   x10=a0   x11=a1  ...     x17=a7
;;;   x18=s2   ...     x27=s11
;;;   x28=t3   x29=t4  x30=t5  x31=t6
;;;
;;; Calling convention:
;;;   Args:       a0–a7  (x10–x17)
;;;   Return:     a0 (x10)
;;;   Caller-saved: ra t0–t6 a0–a7 (temporaries + args)
;;;   Callee-saved: s0–s11 (x8–x9, x18–x27)
;;;   Reserved: zero gp tp sp ra  (non-allocatable)
;;;
;;; RISC-V is the "abstraction canary": its completely different encoding
;;; validates that the target-desc abstraction works across ISA families.

(defvar *riscv64-target*
  (make-target-desc
    :name         :riscv64
    :word-size    8
    :endianness   :little
    :gpr-count    32
    :gpr-names    #(:zero :ra  :sp  :gp  :tp  :t0 :t1 :t2
                    :s0   :s1  :a0  :a1  :a2  :a3 :a4 :a5
                    :a6   :a7  :s2  :s3  :s4  :s5 :s6 :s7
                    :s8   :s9  :s10 :s11 :t3  :t4 :t5 :t6)
    :arg-regs     '(:a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7)
    :ret-reg      :a0
    :callee-saved '(:s0 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9 :s10 :s11)
    :scratch-regs '(:zero :ra :sp :gp :tp) ; architectural reservations
    :stack-alignment 16
    :features     '(:has-native-tail-call :riscv-elf-psabi)))

;;;; ────────────────────────────────────────────────────────────────────────
;;;; WebAssembly 32 — Component Model + WASI 0.2
;;;; ────────────────────────────────────────────────────────────────────────
;;;
;;; References:
;;;   WebAssembly Core Specification 2.0
;;;   WebAssembly Component Model
;;;   WASI Preview 2 (wasi.dev)
;;;   WebAssembly GC Proposal
;;;
;;; Wasm is a stack machine, not a register machine.  GPR-COUNT = 0 because
;;; Wasm has no named architectural registers; locals/operands live on the
;;; implicit operand stack or in typed locals.
;;;
;;; Key differences from native targets:
;;;   - Control flow must be STRUCTURED (Stackifier converts CFG → Wasm blocks/loops)
;;;   - Tail calls via `return_call` / `return_call_indirect` instructions
;;;   - GC reference types from the GC proposal replace raw pointer tagging
;;;   - WASI 0.2 replaces POSIX-style syscalls with Component Model interface

(defvar *wasm32-target*
  (make-target-desc
    :name         :wasm32
    :word-size    4                      ; wasm32 uses 32-bit linear memory addresses
    :endianness   :little
    :gpr-count    0                      ; stack machine — no architectural registers
    :gpr-names    #()
    :arg-regs     nil                    ; args pass via Wasm function signature (typed stack)
    :ret-reg      nil                    ; return via Wasm function signature
    :callee-saved nil                    ; N/A: Wasm locals are implicitly saved by the runtime
    :scratch-regs nil
    :stack-alignment 0                   ; N/A: runtime manages the operand stack
    :features     '(:has-native-tail-call     ; return_call instruction (GC proposal)
                    :structured-control-flow  ; must convert CFG via Stackifier
                    :gc-proposal              ; typed reference types
                    :wasi-0.2)))             ; WASI Preview 2 system interface

;;;; ────────────────────────────────────────────────────────────────────────
;;;; Target Registry
;;;; ────────────────────────────────────────────────────────────────────────

(defvar *target-registry*
  (make-hash-table)
  "Maps target name keywords (:x86-64 etc.) to TARGET-DESC instances.")

(defun register-target (desc)
  "Register DESCRIPTOR under its :NAME keyword in *TARGET-REGISTRY*.
   Overwrites any previous registration with the same name."
  (setf (gethash (target-name desc) *target-registry*) desc)
  desc)

(defun find-target (name)
  "Look up target NAME keyword in *TARGET-REGISTRY*. Returns NIL if not found."
  (gethash name *target-registry*))

;; Register all predefined targets at load time
(register-target *x86-64-target*)
(register-target *aarch64-target*)
(register-target *riscv64-target*)
(register-target *wasm32-target*)

;;;; ────────────────────────────────────────────────────────────────────────
;;;; Target Utility Functions
;;;; ────────────────────────────────────────────────────────────────────────

(defun target-64-bit-p (target)
  "True if TARGET uses 64-bit words."
  (= (target-word-size target) 8))

(defun target-has-feature-p (target feature)
  "True if TARGET has FEATURE in its features list."
  (not (null (member feature (target-features target)))))

(defun target-allocatable-regs (target)
  "Return all registers eligible for allocation: GPRs minus scratch-regs.
   Result is a list of register keywords."
  (let ((scratch (target-scratch-regs target)))
    (remove-if (lambda (r) (member r scratch))
               (coerce (target-gpr-names target) 'list))))

(defun target-caller-saved (target)
  "Return caller-saved registers: allocatable registers minus callee-saved.
   These are the registers a caller cannot assume are preserved across a call."
  (let ((callee (target-callee-saved target)))
    (remove-if (lambda (r) (member r callee))
               (target-allocatable-regs target))))

(defun target-reg-index (target reg)
  "Return the physical index (0-based) of REG in TARGET's GPR file.
   Returns NIL if REG is not in the GPR file."
  (position reg (target-gpr-names target)))

(defun target-op-legal-p (target op)
  "True if generic op keyword OP is natively legal on TARGET.
   An op with no entry in LEGAL-OPS is assumed legal (permissive default)."
  (let ((entry (gethash op (target-legal-ops target) t)))
    (not (null entry))))

(defun target-op-expand (target op dst srcs)
  "If OP requires legalization on TARGET, call the registered expansion function.
   Returns a list of replacement mir-insts, or NIL if no expansion is needed."
  (let ((entry (gethash op (target-legal-ops target))))
    (when (functionp entry)
      (funcall entry op dst srcs))))
