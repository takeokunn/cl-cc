;;;; exports-opcodes.lisp — VM opcode constants, VM2 state, dispatch tables
;;;;
;;;; This file exports opcode-related symbols for the :cl-cc/vm package.
;;;; Organized into sections:
;;;;   1. Opcode name symbols (interned in :cl-cc/vm)
;;;;   2. Opcode dispatch/encoder/name tables
;;;;   3. +OP2-*+ opcode constants
;;;;   4. VM2 state and execution
;;;;   5. Register count constant

(in-package :cl-cc/vm)

(export
 '(;; ─── Opcode Name Symbols ───────────────────────────────────────────────────
   const
   load-const
   load-nil
   load-true
   load-fixnum
   const-halt2
   move
   neg
   inc
   dec
   fixnump
   make-vector
   vector-ref
   vector-set
   make-hash
   hash-ref
   hash-set
   get-global
   set-global
   add2
   sub2
   mul2
   div
   add-imm2
   sub-imm2
   mul-imm2
   num-eq2
   num-lt2
   num-gt2
   num-le2
   num-ge2
   num-eq-imm2
   num-lt-imm2
   num-gt-imm2
   num-le-imm2
   num-ge-imm2
   jump-if-nil
   jump-if-true
   recv-values
   return-nil
   halt2

   ;; ─── Opcode Tables ─────────────────────────────────────────────────────────
   *opcode-dispatch-table*
   *opcode-encoder-table*
   *opcode-name-table*

   ;; ─── +OP2-*+ Opcode Constants ──────────────────────────────────────────────
   +op2-const+
   +op2-nop+
   +op2-load-const+
   +op2-load-nil+
   +op2-load-true+
   +op2-load-fixnum+
   +op2-const-halt2+
   +op2-move+
   +op2-neg+
   +op2-inc+
   +op2-dec+
   +op2-fixnump+
   +op2-consp+
   +op2-symbolp+
   +op2-functionp+
   +op2-stringp+
   +op2-cons+
   +op2-car+
   +op2-cdr+
   +op2-make-vector+
   +op2-vector-ref+
   +op2-vector-set+
   +op2-make-hash+
   +op2-hash-ref+
   +op2-hash-set+
   +op2-get-global+
   +op2-set-global+
   +op2-make-instance+
   +op2-eq+
   +op2-eql+
   +op2-equal+
   +op2-add2+
   +op2-sub2+
   +op2-mul2+
   +op2-div+
   +op2-mod+
   +op2-add-imm2+
   +op2-sub-imm2+
   +op2-mul-imm2+
   +op2-num-eq2+
   +op2-num-lt2+
   +op2-num-gt2+
   +op2-num-le2+
   +op2-num-ge2+
   +op2-num-eq-imm2+
   +op2-num-lt-imm2+
   +op2-num-gt-imm2+
   +op2-num-le-imm2+
   +op2-num-ge-imm2+
   +op2-jump+
   +op2-jump-if-nil+
   +op2-jump-if-true+
   +op2-values+
   +op2-recv-values+
   +op2-return+
   +op2-return-nil+
   +op2-halt2+

   ;; ─── Register Count ────────────────────────────────────────────────────────
   +vm-register-count+

   ;; ─── Internal flat-vector VM2 helpers remain intentionally unexported ────
   ))
