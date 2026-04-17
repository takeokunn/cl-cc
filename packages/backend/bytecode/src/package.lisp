;;;; packages/backend/bytecode/src/package.lisp - CL-CC Bytecode ISA v2 Package Definition
;;;;
;;;; 32-bit instruction encoding for the CL-CC bytecode VM.
;;;; Instruction formats:
;;;;   [opcode:8][dst:8][src1:8][src2:8]   ; 3-operand
;;;;   [opcode:8][dst:8][imm16:16]          ; immediate load
;;;;   [opcode:8][offset:24]                ; branch (signed)
;;;;   [opcode:8][dst:8][src:8][pad:8]      ; 2-operand

(defpackage :cl-cc/bytecode
  (:use :cl)
  (:export
   ;; Bytecode word type
   #:bytecode-word

   ;; -----------------------------------------------------------
   ;; Opcode Constants
   ;; -----------------------------------------------------------

   ;; Misc
   #:+op-nop+

   ;; Load / Move
   #:+op-load-const+
   #:+op-move+
   #:+op-load-nil+
   #:+op-load-true+
   #:+op-load-fixnum+

   ;; Arithmetic
   #:+op-add+
   #:+op-sub+
   #:+op-mul+
   #:+op-div+
   #:+op-mod+
   #:+op-neg+
   #:+op-inc+
   #:+op-dec+

   ;; Comparison
   #:+op-eq+
   #:+op-eql+
   #:+op-equal+
   #:+op-num-lt+
   #:+op-num-gt+
   #:+op-num-le+
   #:+op-num-ge+
   #:+op-num-eq+

   ;; Control Flow
   #:+op-jump+
   #:+op-jump-if-nil+
   #:+op-jump-if-true+
   #:+op-call+
   #:+op-tail-call+
   #:+op-return+
   #:+op-return-nil+

   ;; Closure & Upvalue
   #:+op-make-closure+
   #:+op-get-upvalue+
   #:+op-set-upvalue+
   #:+op-close-upvalue+

   ;; Object Access (with inline cache)
   #:+op-get-slot+
   #:+op-set-slot+
   #:+op-get-global+
   #:+op-set-global+
   #:+op-make-instance+

   ;; Collections
   #:+op-cons+
   #:+op-car+
   #:+op-cdr+
   #:+op-make-vector+
   #:+op-vector-ref+
   #:+op-vector-set+
   #:+op-make-hash+
   #:+op-hash-ref+
   #:+op-hash-set+

   ;; Type Check (fast path)
   #:+op-type-check+
   #:+op-fixnump+
   #:+op-consp+
   #:+op-symbolp+
   #:+op-functionp+
   #:+op-stringp+

   ;; Multiple Values
   #:+op-values+
   #:+op-recv-values+

   ;; Exception Handling
   #:+op-push-handler+
   #:+op-pop-handler+
   #:+op-signal+
   #:+op-push-unwind+
   #:+op-pop-unwind+

   ;; Wide prefix
   #:+op-wide+

   ;; -----------------------------------------------------------
   ;; Encoding Functions
   ;; -----------------------------------------------------------
   #:encode-3op
   #:encode-2op
   #:encode-imm
   #:encode-branch
   #:encode-nop

   ;; Specific instruction encoders
   #:encode-load-const
   #:encode-move
   #:encode-load-nil
   #:encode-load-true
   #:encode-load-fixnum
   #:encode-add
   #:encode-sub
   #:encode-mul
   #:encode-div
   #:encode-mod
   #:encode-neg
   #:encode-inc
   #:encode-dec
   #:encode-eq
   #:encode-eql
   #:encode-equal
   #:encode-num-lt
   #:encode-num-gt
   #:encode-num-le
   #:encode-num-ge
   #:encode-num-eq
   #:encode-jump
   #:encode-jump-if-nil
   #:encode-jump-if-true
   #:encode-call
   #:encode-tail-call
   #:encode-return
   #:encode-return-nil
   #:encode-make-closure
   #:encode-get-upvalue
   #:encode-set-upvalue
   #:encode-close-upvalue
   #:encode-get-slot
   #:encode-set-slot
   #:encode-get-global
   #:encode-set-global
   #:encode-make-instance
   #:encode-cons
   #:encode-car
   #:encode-cdr
   #:encode-make-vector
   #:encode-vector-ref
   #:encode-vector-set
   #:encode-make-hash
   #:encode-hash-ref
   #:encode-hash-set
   #:encode-type-check
   #:encode-fixnump
   #:encode-consp
   #:encode-symbolp
   #:encode-functionp
   #:encode-stringp
   #:encode-values
   #:encode-recv-values
   #:encode-push-handler
   #:encode-pop-handler
   #:encode-signal
   #:encode-push-unwind
   #:encode-pop-unwind

   ;; -----------------------------------------------------------
   ;; Bytecode Chunk
   ;; -----------------------------------------------------------
   #:bytecode-chunk
   #:make-bytecode-chunk
   #:bytecode-chunk-code
   #:bytecode-chunk-constants

   ;; -----------------------------------------------------------
   ;; Assembler / Builder
   ;; -----------------------------------------------------------
   #:bytecode-builder
   #:make-bytecode-builder
   #:emit
   #:emit-constant
   #:build-bytecode

   ;; -----------------------------------------------------------
   ;; Decoding / Disassembly
   ;; -----------------------------------------------------------
   #:decode-opcode
   #:decode-dst
   #:decode-src1
   #:decode-src2
   #:decode-imm16
   #:decode-offset24
   #:disassemble-instruction
   #:disassemble-chunk
   #:*opcode-names*
   #:instruction-format))
