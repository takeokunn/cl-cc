(in-package :cl-cc/type)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Type Inference — Condition Classes
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

(defun unbound-variable-name (condition)
  "Compatibility reader for UNBOUND-VARIABLE-ERROR variable names."
  (unbound-variable-error-name condition))

(define-condition type-mismatch-error (type-inference-error)
  ((expected :initarg :expected :initform nil :reader type-mismatch-error-expected)
   (actual :initarg :actual :initform nil :reader type-mismatch-error-actual))
  (:report (lambda (condition stream)
             (format stream "Type mismatch: expected ~A, got ~A"
                     (type-to-string (type-mismatch-error-expected condition))
                     (type-to-string (type-mismatch-error-actual condition))))))
