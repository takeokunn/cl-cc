;;;; packages/target/src/package.lisp — feature package for cl-cc/target
;;;;
;;;; Phase 6 of package-by-feature migration. Houses target-desc and
;;;; predefined target descriptors (x86-64, aarch64, riscv64, wasm32).
;;;; Pure :cl leaf — no other cl-cc dependencies.

(defpackage :cl-cc/target
  (:use :cl)
  (:export
   ;; target.lisp — struct + accessors
   #:target-desc #:make-target-desc #:target-desc-p
   #:target-name #:target-word-size #:target-endianness
   #:target-gpr-count #:target-gpr-names
   #:target-arg-regs #:target-ret-reg
   #:target-fp-arg-regs #:target-fp-ret-reg
   #:target-callee-saved #:target-scratch-regs
   #:target-stack-alignment #:target-legal-ops #:target-features
   ;; target.lisp — predefined descriptors
   #:*x86-64-target* #:*aarch64-target* #:*riscv64-target* #:*wasm32-target*
   #:*target-registry* #:register-target #:find-target
   ;; target.lisp — utility functions
   #:target-64-bit-p #:target-has-feature-p
   #:target-allocatable-regs #:target-caller-saved
   #:target-reg-index #:target-op-legal-p #:target-op-expand))
