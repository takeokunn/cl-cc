;;;; tests/unit/expand/macros-stdlib-list-set-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)

(defsuite macros-stdlib-list-set-suite
  :description "Tests for macros-stdlib.lisp: list/set/sequence basics"
  :parent cl-cc-suite)

(in-suite macros-stdlib-list-set-suite)

(deftest concatenate-list-expands-to-append
  "CONCATENATE 'list expands to (append ...sequences)."
  (assert-equal (our-macroexpand-1 '(concatenate 'list '(1 2) '(3 4)))
                '(append '(1 2) '(3 4))))

(deftest concatenate-vector-expands-to-coerce-to-vector
  "CONCATENATE 'vector expands to (coerce-to-vector (append ...))."
  (let ((result (our-macroexpand-1 '(concatenate 'vector '(1) '(2)))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-VECTOR")
    (assert-eq (caadr result) 'append)))

(deftest concatenate-simple-vector-expands-to-coerce-to-vector
  "CONCATENATE 'simple-vector also uses coerce-to-vector path."
  (let ((result (our-macroexpand-1 '(concatenate 'simple-vector '(1) '(2)))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-VECTOR")))

(deftest concatenate-runtime-string
  "CONCATENATE runtime: string concatenation."
  (assert-equal (run-string "(concatenate 'string \"hello\" \" \" \"world\")") "hello world")
  (assert-equal (run-string "(concatenate 'string)") "")
  (assert-equal (run-string "(concatenate 'string \"only\")") "only"))

(deftest concatenate-runtime-list
  "CONCATENATE runtime: list concatenation."
  (assert-equal (run-string "(concatenate 'list '(1 2) '(3 4))") '(1 2 3 4))
  (assert-= (run-string "(length (concatenate 'list '(1 2) '(3 4)))") 4)
  (assert-= (run-string "(length (concatenate 'list '(1)))") 1))

(deftest-each notany-notevery-runtime
  "notany/notevery runtime behaviour mirrors (not (some/every ...))."
  :cases (("notany-all-fail"    "(notany #'evenp '(1 3 5))"   t)
          ("notany-one-passes"  "(notany #'evenp '(1 2 3))"   nil)
          ("notevery-all-pass"  "(notevery #'oddp '(1 3 5))"  nil)
          ("notevery-one-fails" "(notevery #'oddp '(1 2 3))"  t))
  (code expected)
  (if expected
      (assert-true (run-string code))
      (assert-false (run-string code))))

(deftest nreconc-expands-to-nconc-nreverse
  "NRECONC expands to (nconc (nreverse list) tail)."
  (let ((result (our-macroexpand-1 '(nreconc lst tail))))
    (assert-eq (car result) 'nconc)
    (assert-eq (caadr result) 'nreverse)
    (assert-eq (caddr result) 'tail)))

(deftest nreconc-runtime
  "NRECONC prepends reversed list onto tail."
  (assert-equal (run-string "(nreconc (list 3 2 1) '(4 5))") '(1 2 3 4 5))
  (assert-equal (run-string "(nreconc '() '(1 2))") '(1 2)))
