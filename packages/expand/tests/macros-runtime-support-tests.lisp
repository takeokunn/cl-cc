;;;; tests/unit/expand/macros-runtime-support-tests.lisp
;;;; Coverage tests for src/expand/macros-runtime-support.lisp and macros-package-system.lisp

(in-package :cl-cc/test)

(defsuite macros-runtime-support-suite
  :description "Tests for runtime support and package-system macros"
  :parent cl-cc-unit-suite)

(in-suite macros-runtime-support-suite)

(deftest in-package-expansion
  "IN-PACKAGE expands to a progn that installs the package."
  (let ((result (our-macroexpand-1 '(in-package :cl-cc))))
    (assert-eq 'progn (car result))
    (assert-eq 'setq (car (second result)))
    (assert-equal '(quote :cl-cc) (third result))))

(deftest-each declare-declaim-expand-to-nil
  "DECLARE and DECLAIM expand to NIL in the current macro runtime."
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
  "(defpackage :foo ...) expands to progn with runtime-backed package helpers."
  (let* ((result (our-macroexpand-1 '(defpackage :foo (:use :cl) (:export foo))))
         (result-str (format nil "~S" result)))
    (assert-eq (car result) 'progn)
    (assert-true (search "RT-FIND-PACKAGE" result-str))
    (assert-true (search "RT-MAKE-PACKAGE" result-str))
    (assert-true (search "RT-EXPORT" result-str))
    (assert-false (search "ADD-PACKAGE-LOCAL-NICKNAME" result-str))))

(deftest package-iteration-expands-to-runtime-package-lookup
  "Package iteration macros resolve explicit package designators through RT-FIND-PACKAGE."
  (let* ((result (our-macroexpand-1 '(do-symbols (s :cl-user) s)))
         (result-str (format nil "~S" result)))
    (assert-true (search "RT-FIND-PACKAGE" result-str))
    (assert-false (search "(FIND-PACKAGE :CL-USER)" result-str))))

(deftest foreign-funcall-expands-to-vm-bridge
  "FOREIGN-FUNCALL expands directly to the VM bridge implementation."
  (let ((result (our-macroexpand-1 '(foreign-funcall "strlen" :string "abcd" :int))))
    (assert-string= "%FOREIGN-FUNCALL" (symbol-name (car result)))
    (assert-string= "CL-CC/VM" (package-name (symbol-package (car result))))))
