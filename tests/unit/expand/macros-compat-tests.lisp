;;;; tests/unit/expand/macros-compat-tests.lisp
;;;; Coverage tests for src/expand/macros-compat.lisp

(in-package :cl-cc/test)

(defsuite macros-compat-suite
  :description "Tests for macros-compat.lisp"
  :parent cl-cc-suite)

(in-suite macros-compat-suite)

(deftest in-package-expansion
  "IN-PACKAGE expands to a progn that installs the package."
  (let ((result (our-macroexpand-1 '(in-package :cl-cc))))
    (assert-eq 'progn (car result))
    (assert-eq 'setq (car (second result)))
    (assert-equal '(quote :cl-cc) (third result))))

(deftest declare-declaim-expand-to-nil
  "DECLARE and DECLAIM are compatibility stubs that expand to NIL."
  (assert-equal nil (our-macroexpand-1 '(declare (special x))))
  (assert-equal nil (our-macroexpand-1 '(declaim (special x)))))

(deftest locally-preserves-declarations
  "LOCALLY keeps declarations in a LET wrapper."
  (let ((result (our-macroexpand-1 '(locally (declare (special x)) x))))
    (assert-eq 'let (car result))
    (assert-eq 'declare (car (caddr result)))))

(deftest progv-expands-with-unwind-protect
  "PROGV binds dynamically through LET* and UNWIND-PROTECT."
  (let ((result (our-macroexpand-1 '(progv syms vals (foo)))))
    (assert-eq 'let* (car result))
    (assert-eq 'unwind-protect (car (caddr result)))))
