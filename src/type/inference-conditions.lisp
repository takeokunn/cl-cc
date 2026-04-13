(in-package :cl-cc/type)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Type Inference — Condition Classes and Exports
;;;
;;; Extracted from inference-forms.lisp.
;;; Depends on inference-forms.lisp (type-to-string).
;;; Load order: immediately after inference-forms.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Condition Classes

(define-condition type-inference-error (error)
  ((message :initarg :message :reader type-inference-error-message))
  (:report (lambda (condition stream)
             (format stream "Type inference error: ~A"
                     (type-inference-error-message condition)))))

(define-condition typed-hole-error (type-inference-error)
  ())

(define-condition unbound-variable-error (type-inference-error)
  ((name :initarg :name :initform nil :reader unbound-variable-error-name))
  (:report (lambda (condition stream)
             (format stream "Unbound variable: ~A"
                     (unbound-variable-error-name condition)))))

(define-condition type-mismatch-error (type-inference-error)
  ((expected :initarg :expected :initform nil :reader type-mismatch-error-expected)
   (actual :initarg :actual :initform nil :reader type-mismatch-error-actual))
  (:report (lambda (condition stream)
             (format stream "Type mismatch: expected ~A, got ~A"
                     (type-to-string (type-mismatch-error-expected condition))
                     (type-to-string (type-mismatch-error-actual condition))))))

;;; Exports

(export '(infer
          infer-binop
          infer-if
          infer-let
          infer-lambda
          infer-call
          infer-progn
          infer-args
          infer-with-env
          annotate-type
          syntactic-value-p
          *type-predicate-table*
          register-type-predicate

          type-inference-error
          type-inference-error-message
          unbound-variable-error
          unbound-variable-error-name
          type-mismatch-error
          type-mismatch-error-expected
          type-mismatch-error-actual

          ;; Type class registries (Phase 4) — defined in typeclass.lisp, re-exported here
          *typeclass-registry*
          register-typeclass
          lookup-typeclass
          *typeclass-instance-registry*
          register-typeclass-instance
          lookup-typeclass-instance
          has-typeclass-instance-p
          check-typeclass-constraint
          dict-env-extend
          dict-env-lookup
          check-qualified-constraints

          ;; Phase 6 rank-N: check mode required for forall
          ))
