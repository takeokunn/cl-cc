;;;; tests/unit/expand/macros-compat-tests.lisp
;;;; Coverage tests for src/expand/macros-compat.lisp

(in-package :cl-cc/test)

(defsuite macros-compat-suite
  :description "Tests for macros-compat.lisp"
  :parent cl-cc-unit-suite)

(in-suite macros-compat-suite)

(deftest in-package-expansion
  "IN-PACKAGE expands to a progn that installs the package."
  (let ((result (our-macroexpand-1 '(in-package :cl-cc))))
    (assert-eq 'progn (car result))
    (assert-eq 'setq (car (second result)))
    (assert-equal '(quote :cl-cc) (third result))))

(deftest-each declare-declaim-expand-to-nil
  "DECLARE and DECLAIM are compatibility stubs that expand to NIL."
  :cases (("declare" '(declare (special x)))
          ("declaim" '(declaim (special x))))
  (form)
  (assert-equal nil (our-macroexpand-1 form)))

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

(deftest defpackage-creates-package
  "(defpackage :foo ...) expands to progn with find-package/make-package."
  (let* ((result (our-macroexpand-1 '(defpackage :foo (:use :cl))))
         (result-str (format nil "~S" result)))
    (assert-eq (car result) 'progn)
    (assert-true (search "FIND-PACKAGE" result-str))
    (assert-true (search "MAKE-PACKAGE" result-str))))

(deftest defpackage-local-nicknames-expand-and-apply
  "(defpackage ... (:local-nicknames ...)) expands to host local nickname registration."
  (let* ((form '(defpackage :fr275-pkg
                  (:use :cl)
                  (:local-nicknames (:a :cl-user))))
         (result (our-macroexpand-1 form))
         (result-str (format nil "~S" result))
         (pkg-name (eval result))
         (pkg (find-package pkg-name)))
    (assert-true (search "ADD-PACKAGE-LOCAL-NICKNAME" result-str))
    (assert-true (equal (package-name pkg) "FR275-PKG"))
    (assert-true (assoc "A" (sb-ext:package-local-nicknames pkg) :test #'string=))))
