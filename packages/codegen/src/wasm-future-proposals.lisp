;;;; packages/codegen/src/wasm-future-proposals.lisp - Planned Wasm proposal WAT helpers
;;;
;;; WAT emitters for Wasm proposals not yet connected to any emit-instruction method.
;;; These are stubs documenting planned integration points for future phases.
;;;
;;; Proposals covered:
;;;   FR-233: Non-trapping float-to-int (saturating conversions)
;;;   FR-234: Sign-extension operators
;;;   FR-238: i64 wide/128-bit arithmetic
;;;   FR-246: Flexible vectors
;;;   FR-248: Half-precision float (f16)
;;;   FR-251: Stringref
;;;   FR-272: Algebraic effects (perform)
;;;   FR-284: array.init_data / array.init_elem / array.fill sub-word
;;;   FR-290: func.bind partial application
;;;   FR-301: cont.throw
;;;   FR-205: Continuations (cont.new, cont.bind, resume, suspend)
;;;   FR-323: MVP Bit Operations (clz/ctz/popcnt)
;;;   FR-327: Sub-word atomics

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Shared WAT emitter macros (also used by wasm-trampoline.lisp)
;;; ─────────────────────────────────────────────────────────────────────────────

;;; (defined in wasm-trampoline.lisp: define-wasm-unary-wat, define-wasm-binary-wat)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-233: Non-trapping float-to-int — saturating conversions
;;; ─────────────────────────────────────────────────────────────────────────────

(define-wasm-unary-wat wasm-trunc-sat-f64-i64-wat "i64.trunc_sat_f64_s" f64-wat "Return WAT for non-trapping f64->i64 conversion (saturating).")
(define-wasm-unary-wat wasm-trunc-sat-f64-i32-wat "i32.trunc_sat_f64_s" f64-wat "Return WAT for non-trapping f64->i32 conversion (saturating).")
(define-wasm-unary-wat wasm-trunc-sat-f32-i64-wat "i64.trunc_sat_f32_s" f32-wat "Return WAT for non-trapping f32->i64 conversion (saturating).")
(define-wasm-unary-wat wasm-trunc-sat-f32-i32-wat "i32.trunc_sat_f32_s" f32-wat "Return WAT for non-trapping f32->i32 conversion (saturating).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-234: Sign-extension — 1-instruction replacements for shift pairs
;;; ─────────────────────────────────────────────────────────────────────────────

(define-wasm-unary-wat wasm-sign-extend-32-8-wat  "i32.extend8_s"  i32-wat "Return WAT for i32.extend8_s.")
(define-wasm-unary-wat wasm-sign-extend-32-16-wat "i32.extend16_s" i32-wat "Return WAT for i32.extend16_s.")
(define-wasm-unary-wat wasm-sign-extend-64-32-wat "i64.extend32_s" i64-wat "Return WAT for i64.extend32_s.")
(define-wasm-unary-wat wasm-sign-extend-64-8-wat  "i64.extend8_s"  i64-wat "Return WAT for i64.extend8_s.")
(define-wasm-unary-wat wasm-sign-extend-64-16-wat "i64.extend16_s" i64-wat "Return WAT for i64.extend16_s.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-323: MVP Bit Operations — clz/ctz/popcnt for integer-length/logcount
;;; ─────────────────────────────────────────────────────────────────────────────

(define-wasm-unary-wat wasm-i64-clz-wat    "i64.clz"    i64-wat "Return WAT for i64.clz — count leading zeros.")
(define-wasm-unary-wat wasm-i64-ctz-wat    "i64.ctz"    i64-wat "Return WAT for i64.ctz — count trailing zeros.")
(define-wasm-unary-wat wasm-i64-popcnt-wat "i64.popcnt" i64-wat "Return WAT for i64.popcnt — population count.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-238: i64 wide/128-bit arithmetic emitters
;;; ─────────────────────────────────────────────────────────────────────────────

(define-wasm-binary-wat wasm-i64-mul-wide-s-wat "i64.mul_wide_s" lhs-wat rhs-wat "FR-238: Return WAT for signed i64.mul_wide_s.")
(define-wasm-binary-wat wasm-i64-mul-wide-u-wat "i64.mul_wide_u" lhs-wat rhs-wat "FR-238: Return WAT for unsigned i64.mul_wide_u.")

(defun wasm-i64-add128-wat (lo-lhs-wat hi-lhs-wat lo-rhs-wat hi-rhs-wat)
  "FR-238: Return WAT for i64.add128."
  (format nil "(i64.add128 ~A ~A ~A ~A)" lo-lhs-wat hi-lhs-wat lo-rhs-wat hi-rhs-wat))

(defun wasm-i64-sub128-wat (lo-lhs-wat hi-lhs-wat lo-rhs-wat hi-rhs-wat)
  "FR-238: Return WAT for i64.sub128."
  (format nil "(i64.sub128 ~A ~A ~A ~A)" lo-lhs-wat hi-lhs-wat lo-rhs-wat hi-rhs-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-246: Flexible vectors
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-flexible-vector-op-wat (op lhs-wat rhs-wat &key (width :v128x2))
  "FR-246: Return WAT for flexible-vector OP at WIDTH (:V128X2 or :V512)."
  (let ((prefix (ecase width
                  (:v128x2 "v128x2")
                  (:v512 "v512"))))
    (format nil "(~A.~A ~A ~A)" prefix op lhs-wat rhs-wat)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-248: Half-precision float (f16)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-f16-binop-wat (op lhs-wat rhs-wat)
  "FR-248: Return WAT for an f16 binary operation OP."
  (format nil "(f16.~A ~A ~A)" op lhs-wat rhs-wat))

(defun wasm-f16-load-wat (addr-wat &key (align 2) (offset 0))
  "FR-248: Return WAT for f16.load."
  (format nil "(f16.load align=~D offset=~D ~A)" align offset addr-wat))

(defun wasm-f16-store-wat (addr-wat value-wat &key (align 2) (offset 0))
  "FR-248: Return WAT for f16.store."
  (format nil "(f16.store align=~D offset=~D ~A ~A)" align offset addr-wat value-wat))

(define-wasm-unary-wat wasm-f16-convert-f32-wat "f16.convert_f32" f32-wat "FR-248: Return WAT for f16.convert_f32.")
(define-wasm-unary-wat wasm-f32-convert-f16-wat "f32.convert_f16" f16-wat "FR-248: Return WAT for f32.convert_f16.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-251: Stringref
;;; ─────────────────────────────────────────────────────────────────────────────

(define-wasm-unary-wat wasm-stringref-length-wat "string.length" string-wat "FR-251: Return WAT for native stringref length.")
(define-wasm-binary-wat wasm-stringref-get-codeunit-wat "string.get_codeunit" string-wat index-wat "FR-251: Return WAT for native stringref code-unit access.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-290: func.bind partial application
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-func-bind-wat (type-name func-wat &rest bound-args)
  "FR-290: Return WAT for func.bind partial application."
  (format nil "(func.bind (type ~A) ~A~{ ~A~})" type-name func-wat bound-args))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-205: Continuations (cont.new, cont.bind, resume, suspend)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-cont-new-wat (type-name func-wat)
  "FR-205: Return WAT for cont.new."
  (format nil "(cont.new (type ~A) ~A)" type-name func-wat))

(defun wasm-cont-bind-wat (type-name cont-wat &rest bound-args)
  "FR-205: Return WAT for cont.bind."
  (format nil "(cont.bind (type ~A) ~A~{ ~A~})" type-name cont-wat bound-args))

(defun wasm-suspend-wat (tag-name &rest args)
  "FR-205: Return WAT for suspend."
  (format nil "(suspend ~A~{ ~A~})" tag-name args))

(defun wasm-resume-wat (cont-wat &rest args)
  "FR-205: Return WAT for resume."
  (format nil "(resume ~A~{ ~A~})" cont-wat args))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-272: Algebraic effects
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-effect-perform-wat (effect-name &rest args)
  "FR-272: Return WAT for an algebraic effect perform placeholder."
  (format nil "(perform ~A~{ ~A~})" effect-name args))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-301: cont.throw
;;; ─────────────────────────────────────────────────────────────────────────────

(define-wasm-binary-wat wasm-cont-throw-wat "cont.throw" cont-wat exnref-wat "FR-301: Return WAT for cont.throw.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-327: Sub-word atomic WAT helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-i32-atomic-rmw8-cmpxchg-u-wat (addr-wat expected-wat replacement-wat &key (align 1) (offset 0))
  "FR-327: Return WAT for i32.atomic.rmw8.cmpxchg_u."
  (format nil "(i32.atomic.rmw8.cmpxchg_u align=~D offset=~D ~A ~A ~A)"
          align offset addr-wat expected-wat replacement-wat))

(defun wasm-i32-atomic-rmw16-cmpxchg-u-wat (addr-wat expected-wat replacement-wat &key (align 2) (offset 0))
  "FR-327: Return WAT for i32.atomic.rmw16.cmpxchg_u."
  (format nil "(i32.atomic.rmw16.cmpxchg_u align=~D offset=~D ~A ~A ~A)"
          align offset addr-wat expected-wat replacement-wat))

(defun wasm-i32-atomic-rmw8-op-u-wat (op addr-wat value-wat &key (align 1) (offset 0))
  "FR-327: Return WAT for an i32.atomic.rmw8.*_u operation."
  (format nil "(i32.atomic.rmw8.~A_u align=~D offset=~D ~A ~A)"
          op align offset addr-wat value-wat))

(defun wasm-i32-atomic-rmw16-op-u-wat (op addr-wat value-wat &key (align 2) (offset 0))
  "FR-327: Return WAT for an i32.atomic.rmw16.*_u operation."
  (format nil "(i32.atomic.rmw16.~A_u align=~D offset=~D ~A ~A)"
          op align offset addr-wat value-wat))
