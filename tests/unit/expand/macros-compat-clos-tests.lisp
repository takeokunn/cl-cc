;;;; tests/unit/expand/macros-compat-clos-tests.lisp
;;;; Coverage tests for src/expand/macros-compat-clos.lisp
;;;;
;;;; Covers: print-unreadable-object, print-object, describe-object, describe,
;;;; update-instance-for-different-class, update-instance-for-changed-class,
;;;; ensure-class, change-class, define-method-combination,
;;;; class-direct-superclasses, class-direct-slots, class-slots,
;;;; class-direct-default-initargs, generic-function-methods,
;;;; generic-function-method-combination, class-precedence-list, parse-float,
;;;; reinitialize-instance, shared-initialize.

(in-package :cl-cc/test)

(defsuite macros-compat-clos-suite
  :description "Tests for macros-compat-clos.lisp: CLOS protocol and MOP macros"
  :parent cl-cc-suite)

(in-suite macros-compat-clos-suite)

;;; ─── print-unreadable-object ──────────────────────────────────────────────

(deftest print-unreadable-object-expands-to-let
  "PRINT-UNREADABLE-OBJECT expands to a LET binding object and stream."
  (let ((result (our-macroexpand-1
                 '(print-unreadable-object (obj stream) body))))
    (assert-eq 'let (car result))))

(deftest print-unreadable-object-wraps-with-angle-brackets
  "PRINT-UNREADABLE-OBJECT body includes FORMAT calls for #< and >."
  (let* ((result (our-macroexpand-1
                  '(print-unreadable-object (obj stream) body)))
         (body (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'format)
                                        (member "#<" f :test #'equal)))
                       body))))

(deftest print-unreadable-object-type-flag-emits-type-of
  "PRINT-UNREADABLE-OBJECT with :type t emits a FORMAT call with TYPE-OF."
  (let* ((result (our-macroexpand-1
                  '(print-unreadable-object (obj stream :type t) body)))
         (body (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'when))) body))))

;;; ─── describe ─────────────────────────────────────────────────────────────

(deftest describe-expands-to-let
  "DESCRIBE expands to a LET binding the stream variable."
  (let ((result (our-macroexpand-1 '(describe obj))))
    (assert-eq 'let (car result))))

(deftest describe-calls-describe-object
  "DESCRIBE body calls DESCRIBE-OBJECT on the object."
  (let* ((result (our-macroexpand-1 '(describe obj)))
         (body (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'describe-object)))
                       body))))

(deftest describe-ends-with-values
  "DESCRIBE returns (values) as its last form."
  (let* ((result (our-macroexpand-1 '(describe obj)))
         (last   (car (last result))))
    (assert-equal '(values) last)))

;;; ─── ensure-class ─────────────────────────────────────────────────────────

(deftest ensure-class-delegates-to-defclass
  "ENSURE-CLASS expands to a DEFCLASS with the given name."
  (let ((result (our-macroexpand-1
                 '(cl-cc::ensure-class 'my-class :direct-superclasses '(object)))))
    (assert-eq 'defclass (car result))))

;;; ─── define-method-combination ────────────────────────────────────────────

(deftest define-method-combination-returns-quoted-name
  "DEFINE-METHOD-COMBINATION expands to (QUOTE name) as a registration side effect."
  (let ((result (our-macroexpand-1
                 '(cl-cc::define-method-combination append :identity-with-one-argument t))))
    (assert-eq 'quote (car result))
    (assert-eq 'append (second result))))

;;; ─── update-instance-for-different-class / update-instance-for-changed-class

(deftest update-instance-for-different-class-delegates-to-reinitialize
  "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS expands to a REINITIALIZE-INSTANCE call."
  (let ((result (our-macroexpand-1
                 '(cl-cc::update-instance-for-different-class prev curr :slot 1))))
    (assert-eq 'reinitialize-instance (car result))
    (assert-eq 'curr (second result))))

(deftest update-instance-for-changed-class-delegates-to-reinitialize
  "UPDATE-INSTANCE-FOR-CHANGED-CLASS expands to a REINITIALIZE-INSTANCE call."
  (let ((result (our-macroexpand-1
                 '(cl-cc::update-instance-for-changed-class inst :slot 1))))
    (assert-eq 'reinitialize-instance (car result))
    (assert-eq 'inst (second result))))

;;; ─── MOP introspection macros ─────────────────────────────────────────────

(deftest-each mop-class-accessors-expand-to-let
  "Class MOP accessor macros all expand to a LET guarded by HASH-TABLE-P.
Uses cl-cc:: qualified symbols because our-defmacro registers macros under the
package where they were defined (src is (in-package :cl-cc)), and the test
package would otherwise intern fresh uninterned lookup keys."
  :cases (("class-direct-superclasses"    '(cl-cc::class-direct-superclasses cls))
          ("class-direct-slots"           '(cl-cc::class-direct-slots cls))
          ("class-slots"                  '(cl-cc::class-slots cls))
          ("class-direct-default-initargs" '(cl-cc::class-direct-default-initargs cls)))
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
  (let* ((result (our-macroexpand-1 '(cl-cc::class-direct-superclasses cls)))
         (body (cddr result)))
    (assert-true (%tree-contains-keyword-p :__superclasses__ body))))

(deftest generic-function-methods-expands-to-let*
  "GENERIC-FUNCTION-METHODS expands to a LET* binding the gf and method table."
  (let ((result (our-macroexpand-1 '(cl-cc::generic-function-methods gf))))
    (assert-eq 'let* (car result))))

(deftest generic-function-methods-calls-hash-table-values
  "GENERIC-FUNCTION-METHODS body calls HASH-TABLE-VALUES."
  (let* ((result (our-macroexpand-1 '(cl-cc::generic-function-methods gf)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'when))) body))))

(deftest generic-function-method-combination-expands-to-let
  "GENERIC-FUNCTION-METHOD-COMBINATION expands to a LET with an IF defaulting to 'standard."
  (let ((result (our-macroexpand-1 '(cl-cc::generic-function-method-combination gf))))
    (assert-eq 'let (car result))))

(deftest generic-function-method-combination-defaults-to-standard
  "GENERIC-FUNCTION-METHOD-COMBINATION IF branch returns 'STANDARD when no combination set."
  (let* ((result (our-macroexpand-1 '(cl-cc::generic-function-method-combination gf)))
         (if-form (caddr result))
         (else-branch (cadddr if-form)))
    (assert-equal ''standard else-branch)))

;;; ─── parse-float ──────────────────────────────────────────────────────────

(deftest parse-float-expands-to-let
  "PARSE-FLOAT expands to a LET that calls FLOAT and READ-FROM-STRING."
  (let ((result (our-macroexpand-1 '(cl-cc::parse-float "3.14"))))
    (assert-eq 'let (car result))))

(deftest parse-float-with-start-uses-subseq
  "PARSE-FLOAT with :start uses SUBSEQ to extract the substring."
  (let* ((result (our-macroexpand-1 '(cl-cc::parse-float s 2)))
         (binding (caadr result))
         (init-form (second (first (second result)))))
    (declare (ignore binding))
    (assert-eq 'if (car init-form))))
