;;;; tests/unit/expand/macros-stdlib-list-set-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)

(defsuite macros-stdlib-list-set-suite
  :description "Tests for macros-stdlib.lisp: list/set/sequence basics"
  :parent cl-cc-integration-suite)

(in-suite macros-stdlib-list-set-suite)

(deftest concatenate-list-expands-to-append
  "CONCATENATE 'list expands to (append ...sequences)."
  (assert-equal (our-macroexpand-1 '(concatenate 'list '(1 2) '(3 4)))
                '(append '(1 2) '(3 4))))

(deftest-each concatenate-vector-types-expand-to-coerce-to-vector
  "CONCATENATE 'vector and 'simple-vector both use the coerce-to-vector path."
  :cases (("vector"        '(concatenate 'vector '(1) '(2)))
          ("simple-vector" '(concatenate 'simple-vector '(1) '(2))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-equal (symbol-name (car result)) "COERCE-TO-VECTOR")
    (assert-eq (caadr result) 'append)))

(deftest-each concatenate-runtime-string
  "CONCATENATE runtime: string concatenation."
  :cases (("two-strings" "(concatenate 'string \"hello\" \" \" \"world\")" "hello world")
          ("empty"       "(concatenate 'string)"                            "")
          ("single"      "(concatenate 'string \"only\")"                  "only"))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest-each concatenate-runtime-list
  "CONCATENATE runtime: list concatenation and length checks."
  :cases (("two-lists"   "(concatenate 'list '(1 2) '(3 4))"          '(1 2 3 4))
          ("four-length" "(length (concatenate 'list '(1 2) '(3 4)))"  4)
          ("one-length"  "(length (concatenate 'list '(1)))"           1))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest-each notany-notevery-runtime
  "notany/notevery runtime behaviour mirrors (not (some/every ...))."
  :cases (("notany-all-fail"    "(notany #'evenp '(1 3 5))"   t)
          ("notany-one-passes"  "(notany #'evenp '(1 2 3))"   nil)
          ("notevery-all-pass"  "(notevery #'oddp '(1 3 5))"  nil)
          ("notevery-one-fails" "(notevery #'oddp '(1 2 3))"  t))
  (code expected)
  (assert-equal expected (not (null (run-string code)))))

(deftest nreconc-expands-to-nconc-nreverse
  "NRECONC expands to (nconc (nreverse list) tail)."
  (let ((result (our-macroexpand-1 '(nreconc lst tail))))
    (assert-eq (car result) 'nconc)
    (assert-eq (caadr result) 'nreverse)
    (assert-eq (caddr result) 'tail)))

(deftest-each nreconc-runtime
  "NRECONC prepends reversed list onto tail."
  :cases (("basic"      "(nreconc (list 3 2 1) '(4 5))" '(1 2 3 4 5))
          ("empty-left" "(nreconc '() '(1 2))"          '(1 2)))
  (form expected)
  (assert-equal expected (run-string form)))
