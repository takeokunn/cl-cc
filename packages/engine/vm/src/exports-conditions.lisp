;;;; exports-conditions.lisp — Condition types, handlers, transcendentals, misc
;;;;
;;;; This file exports condition-related and miscellaneous symbols for :cl-cc/vm.
;;;; Organized into sections:
;;;;   1. Condition system operations
;;;;   2. Condition types and slots
;;;;   3. Restart operations
;;;;   4. Transcendental / numeric constructors
;;;;   5. Bitwise constructors
;;;;   6. Additional constructors and types

(in-package :cl-cc/vm)

(export
 '(;; ─── Condition System Operations ───────────────────────────────────────────
   vm-signal-condition
   vm-find-handler
   vm-find-restart
   vm-add-restart
   vm-get-restarts
   vm-get-handler-stack
   vm-push-handler-to-stack
   vm-pop-handler-from-stack
   vm-clear-condition-context
   *active-restarts*

   ;; ─── Condition Types ───────────────────────────────────────────────────────
   vm-type-error
   vm-unbound-variable
   vm-undefined-function
   vm-division-by-zero
   vm-warning
   vm-error

   ;; ─── Condition Slots ───────────────────────────────────────────────────────
   vm-dividend
   vm-restart-name
   vm-handler-type
   vm-handler-entry-saved-regs

   ;; ─── Transcendental Constructors ───────────────────────────────────────────
   make-vm-sin-inst
   make-vm-cos-inst
   make-vm-tan-inst
   make-vm-acos-inst
   make-vm-asin-inst
   make-vm-atan-inst
   make-vm-atan2-inst
   make-vm-sinh-inst
   make-vm-cosh-inst
   make-vm-tanh-inst
   make-vm-exp-inst
   make-vm-log-inst
   make-vm-sqrt
   make-vm-expt
   make-vm-float-inst
   make-vm-decode-float
   make-vm-integer-decode-float
   make-vm-scale-float

   ;; ─── Numeric Constructors ──────────────────────────────────────────────────
   make-vm-round-inst

   ;; ─── Bitwise Constructors ──────────────────────────────────────────────────
   make-vm-ash
   make-vm-bswap
   make-vm-rotate
   make-vm-logand
   make-vm-logior
   make-vm-logxor
   make-vm-logeqv
   make-vm-lognot
   make-vm-logtest
   make-vm-logbitp
   make-vm-logcount
   make-vm-integer-length

   ;; ─── Additional Constructors and Types ─────────────────────────────────────
   make-vm-boundp
   make-vm-fboundp
   make-vm-char-equal
   make-vm-string
   make-vm-type-error
   make-vm-unbound-variable
   make-vm-undefined-function
   make-vm-division-by-zero))
