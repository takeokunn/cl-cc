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

#-cl-cc-self-hosting
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
    *debugger-hook*
    *break-on-signals*
    invoke-debugger
    break

    ;; ─── Condition Types ───────────────────────────────────────────────────────
    vm-condition
    vm-serious-condition
    vm-simple-condition
    vm-type-error
    vm-simple-error
    vm-simple-warning
    vm-unbound-variable
    vm-undefined-function
    vm-arithmetic-error
    vm-division-by-zero
    vm-floating-point-overflow
    vm-floating-point-underflow
    vm-cell-error
    vm-unbound-slot
    vm-control-error
    vm-program-error
    vm-stream-error
    vm-end-of-file
    vm-reader-error
    vm-package-error
    vm-storage-condition
    vm-style-warning
    vm-warning
    vm-error
    condition
    serious-condition
    error
    simple-error
    type-error
    arithmetic-error
    division-by-zero
    floating-point-overflow
    floating-point-underflow
    cell-error
    unbound-variable
    undefined-function
    unbound-slot
    control-error
    program-error
    stream-error
    end-of-file
    reader-error
    package-error
    warning
    simple-warning
    style-warning
    storage-condition

    ;; ─── Condition Slots ───────────────────────────────────────────────────────
    vm-condition-state
    vm-condition-error-code
    vm-condition-fix-it
    vm-dividend
    simple-condition-format-control
    simple-condition-format-arguments
    type-error-datum
    type-error-expected-type
    arithmetic-error-operation
    arithmetic-error-operands
    cell-error-name
    unbound-slot-instance
    stream-error-stream
    package-error-package

    ;; ─── Restart Operations ────────────────────────────────────────────────────
    abort
    continue
    muffle-warning
    use-value
    store-value
    retry
    invoke-restart
    find-restart
    restart-name
    compute-restarts
    restart-case
    restart-bind
    with-simple-restart
    vm-abort-restart
    vm-muffle-warning-restart
    vm-use-value-restart
    vm-store-value-restart
    vm-retry-restart
    vm-compute-restarts
    vm-invoke-restart
    vm-find-standard-restart
    vm-invoke-debugger
    vm-break
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
   make-vm-asinh-inst
   make-vm-acosh-inst
   make-vm-atanh-inst
   make-vm-exp-inst
   make-vm-log-inst
    make-vm-sqrt
    make-vm-expt
    make-vm-float-inst
    make-vm-float-precision
    make-vm-float-radix
    make-vm-float-sign
    make-vm-float-digits
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
    make-vm-simple-error
    make-vm-simple-warning
    make-vm-unbound-variable
   make-vm-undefined-function
   make-vm-division-by-zero))
