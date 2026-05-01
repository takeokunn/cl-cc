(in-package :cl-cc/vm)

;;; VM — Transcendental and Exponential Functions (FR-304)
;;;
;;; Contains: expt, sqrt, exp, log, sin, cos, tan, asin, acos, atan, atan2,
;;;           sinh, cosh, tanh.
;;;
;;; Load order: after vm-bitwise.lisp.
;;; define-vm-unary-instruction / define-vm-binary-instruction defined in vm.lisp.

;;; ─── Exponential / logarithm ────────────────────────────────────────────────

(define-vm-binary-instruction vm-expt     :expt "Exponentiation: (expt base power).")
(define-vm-unary-instruction  vm-sqrt     :sqrt "Square root.")
(define-vm-unary-instruction  vm-exp-inst :exp  "e raised to the power x.")
(define-vm-unary-instruction  vm-log-inst :log  "Natural logarithm.")

(define-simple-instruction vm-expt     :binary expt)
(define-simple-instruction vm-sqrt     :unary  sqrt)
(define-simple-instruction vm-exp-inst :unary  exp)
(define-simple-instruction vm-log-inst :unary  log)

;;; ─── Trigonometric ──────────────────────────────────────────────────────────

(define-vm-unary-instruction  vm-sin-inst  :sin  "Sine (radians).")
(define-vm-unary-instruction  vm-cos-inst  :cos  "Cosine (radians).")
(define-vm-unary-instruction  vm-tan-inst  :tan  "Tangent (radians).")
(define-vm-unary-instruction  vm-asin-inst :asin "Arc sine.")
(define-vm-unary-instruction  vm-acos-inst :acos "Arc cosine.")
(define-vm-unary-instruction  vm-atan-inst :atan "Arc tangent (1-arg: atan y).")
(define-vm-binary-instruction vm-atan2-inst :atan2 "Arc tangent (2-arg: atan y x).")

(define-simple-instruction vm-sin-inst   :unary  sin)
(define-simple-instruction vm-cos-inst   :unary  cos)
(define-simple-instruction vm-tan-inst   :unary  tan)
(define-simple-instruction vm-asin-inst  :unary  asin)
(define-simple-instruction vm-acos-inst  :unary  acos)
(define-simple-instruction vm-atan-inst  :unary  atan)
(define-simple-instruction vm-atan2-inst :binary atan)

;;; ─── Hyperbolic ─────────────────────────────────────────────────────────────

(define-vm-unary-instruction vm-sinh-inst :sinh "Hyperbolic sine.")
(define-vm-unary-instruction vm-cosh-inst :cosh "Hyperbolic cosine.")
(define-vm-unary-instruction vm-tanh-inst :tanh "Hyperbolic tangent.")

(define-simple-instruction vm-sinh-inst :unary sinh)
(define-simple-instruction vm-cosh-inst :unary cosh)
(define-simple-instruction vm-tanh-inst :unary tanh)

;;; ─── Inverse hyperbolic ─────────────────────────────────────────────────────

(define-vm-unary-instruction vm-asinh-inst :asinh "Inverse hyperbolic sine.")
(define-vm-unary-instruction vm-acosh-inst :acosh "Inverse hyperbolic cosine.")
(define-vm-unary-instruction vm-atanh-inst :atanh "Inverse hyperbolic tangent.")

(define-simple-instruction vm-asinh-inst :unary asinh)
(define-simple-instruction vm-acosh-inst :unary acosh)
(define-simple-instruction vm-atanh-inst :unary atanh)
