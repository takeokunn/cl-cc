;;;; tests/unit/expand/macro-definition-tests.lisp — Macro definition tests

(in-package :cl-cc/test)

(defsuite macro-definition-suite
  :description "Macro definition expansion tests"
  :parent cl-cc-suite)


(in-suite macro-definition-suite)
(deftest-each defun-macro-structure
  "DEFUN expands to (setf (fdefinition ...) (lambda ...)) regardless of docstring"
  :cases (("basic"          '(defun foo (x y) body1 body2))
          ("with-docstring" '(defun foo (x y) "Docstring" body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'setf)
    (assert-equal (cadr result) '(fdefinition 'foo))
    (assert-eq (caaddr result) 'lambda)))

(deftest define-compiler-macro-returns-name
  "DEFINE-COMPILER-MACRO returns the macro name (no compile-time expansion)."
  (let ((result (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))))
    (assert-equal '(quote foo) result)))

(deftest define-compiler-macro-expands-call
  "DEFINE-COMPILER-MACRO registers a compiler macro used by compiler-macroexpand-all."
  (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))
  (let ((result (cl-cc::compiler-macroexpand-all '(foo 2))))
    (assert-equal result 3)))
