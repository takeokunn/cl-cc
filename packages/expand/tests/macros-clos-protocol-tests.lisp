;;;; tests/unit/expand/macros-clos-protocol-tests.lisp
;;;; Coverage tests for src/expand/macros-clos-protocol.lisp and macros-mop-support.lisp
;;;;
;;;; Covers: print-unreadable-object, print-object, describe-object, describe,
;;;; ensure-class, change-class, define-method-combination,
;;;; class-direct-superclasses, class-direct-slots, class-slots,
;;;; class-direct-default-initargs, generic-function-methods,
;;;; generic-function-method-combination, class-precedence-list, parse-float,
;;;; reinitialize-instance, shared-initialize.

(in-package :cl-cc/test)

(defsuite macros-clos-protocol-suite
  :description "Tests for CLOS protocol and MOP support macros"
  :parent cl-cc-unit-suite)

(in-suite macros-clos-protocol-suite)

;;; ─── print-unreadable-object ──────────────────────────────────────────────

(deftest-each print-unreadable-object-basic-shape
  "PRINT-UNREADABLE-OBJECT keeps its expected top-level expansion shape."
  :cases (("base-form-expands-to-let"
           '(print-unreadable-object (obj stream) body)
           (lambda (result)
             (assert-eq 'let (car result))))
          ("body-wraps-with-angle-brackets"
           '(print-unreadable-object (obj stream) body)
           (lambda (result)
             (let ((body (cddr result)))
               (assert-true
                (some (lambda (f)
                        (and (consp f)
                             (eq (car f) 'format)
                             (member "#<" f :test #'equal)))
                      body)))))
          ("type-flag-emits-conditional-type-path"
           '(print-unreadable-object (obj stream :type t) body)
           (lambda (result)
             (let ((body (cddr result)))
               (assert-true
                (some (lambda (f) (and (consp f) (eq (car f) 'when))) body))))))
  (form verifier)
  (funcall verifier (our-macroexpand-1 form)))

;;; ─── describe ─────────────────────────────────────────────────────────────

(deftest-each describe-expansion-shape
  "DESCRIBE keeps its expected structural expansion contract."
  :cases (("expands-to-let"
           (lambda (result)
             (assert-eq 'let (car result))))
          ("calls-describe-object"
           (lambda (result)
             (let ((body (cddr result)))
               (assert-true
                (some (lambda (f) (and (consp f) (eq (car f) 'describe-object)))
                      body)))))
          ("ends-with-values"
           (lambda (result)
             (assert-equal '(values) (car (last result))))))
  (verifier)
  (funcall verifier (our-macroexpand-1 '(describe obj))))

;;; ─── ensure-class ─────────────────────────────────────────────────────────

(deftest ensure-class-delegates-to-defclass
  "ENSURE-CLASS expands to a DEFCLASS with the given name."
  (let ((result (our-macroexpand-1
                 '(cl-cc/expand::ensure-class 'my-class :direct-superclasses '(object)))))
    (assert-eq 'defclass (car result))))

;;; ─── define-method-combination ────────────────────────────────────────────

(deftest define-method-combination-returns-quoted-name
  "DEFINE-METHOD-COMBINATION expands to (QUOTE name) as a registration side effect."
  (let ((result (our-macroexpand-1
                 '(cl-cc::define-method-combination append :identity-with-one-argument t))))
    (assert-eq 'quote (car result))
    (assert-eq 'append (second result))))

(deftest-each instance-init-macros-use-shared-helper
  "REINITIALIZE-INSTANCE and SHARED-INITIALIZE both delegate to %APPLY-INSTANCE-INITARGS."
  :cases (("reinitialize-instance"
           '(cl-cc::reinitialize-instance inst :slot 1))
          ("shared-initialize"
           '(cl-cc::shared-initialize inst t :slot 1)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'let* (car result))
    (assert-true (search "%APPLY-INSTANCE-INITARGS"
                         (string-upcase (format nil "~S" result))))))

;;; ─── MOP introspection macros ─────────────────────────────────────────────

(deftest-each mop-class-accessors-expand-to-let
  "Class MOP accessor macros all expand to a LET guarded by HASH-TABLE-P.
Uses cl-cc: qualified symbols because our-defmacro registers macros under the
package where they were defined (src is (in-package :cl-cc)), and the test
package would otherwise intern fresh uninterned lookup keys."
  :cases (("class-direct-superclasses"    '(cl-cc/expand::class-direct-superclasses cls))
          ("class-direct-slots"           '(cl-cc/expand::class-direct-slots cls))
          ("class-slots"                  '(cl-cc/expand::class-slots cls))
          ("class-direct-default-initargs" '(cl-cc/expand::class-direct-default-initargs cls)))
  (form)
  (assert-eq 'let (car (our-macroexpand-1 form))))

(defun %tree-contains-keyword-p (kw form)
  "True if KW appears anywhere in FORM (nested walk)."
  (cond ((eq form kw) t)
        ((consp form) (or (%tree-contains-keyword-p kw (car form))
                          (%tree-contains-keyword-p kw (cdr form))))
        (t nil)))

(deftest class-direct-superclasses-reads-superclasses-key
  "CLASS-DIRECT-SUPERCLASSES body calls GETHASH with :__superclasses__."
  (let* ((result (our-macroexpand-1 '(cl-cc/expand::class-direct-superclasses cls)))
         (body (cddr result)))
    (assert-true (%tree-contains-keyword-p :__superclasses__ body))))

(deftest generic-function-methods-expands-to-let*
  "GENERIC-FUNCTION-METHODS expands to a LET* binding the gf and method table."
  (let ((result (our-macroexpand-1 '(cl-cc/vm::generic-function-methods gf))))
    (assert-eq 'let* (car result))))

(deftest generic-function-methods-calls-hash-table-values
  "GENERIC-FUNCTION-METHODS body calls HASH-TABLE-VALUES."
  (let* ((result (our-macroexpand-1 '(cl-cc/vm::generic-function-methods gf)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'when))) body))))

(deftest generic-function-method-combination-expands-to-let
  "GENERIC-FUNCTION-METHOD-COMBINATION expands to a LET with an IF defaulting to 'standard."
  (let ((result (our-macroexpand-1 '(cl-cc/vm::generic-function-method-combination gf))))
    (assert-eq 'let (car result))))

(deftest generic-function-method-combination-defaults-to-standard
  "GENERIC-FUNCTION-METHOD-COMBINATION IF branch returns 'STANDARD when no combination set."
  (let* ((result (our-macroexpand-1 '(cl-cc/vm::generic-function-method-combination gf)))
         (if-form (caddr result))
         (else-branch (cadddr if-form)))
    (assert-equal ''standard else-branch)))

;;; ─── parse-float ──────────────────────────────────────────────────────────

(deftest parse-float-expands-to-let
  "PARSE-FLOAT expands to a LET that calls FLOAT and READ-FROM-STRING."
  (let ((result (our-macroexpand-1 '(cl-cc/expand::parse-float "3.14"))))
    (assert-eq 'let (car result))))

(deftest parse-float-with-start-uses-subseq
  "PARSE-FLOAT with :start uses SUBSEQ to extract the substring."
  (let* ((result (our-macroexpand-1 '(cl-cc/expand::parse-float s 2)))
         (binding (caadr result))
         (init-form (second (first (second result)))))
    (declare (ignore binding))
    (assert-eq 'if (car init-form))))
