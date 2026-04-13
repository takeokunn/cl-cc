;;;; tests/unit/compile/stdlib-source-tests.lisp — Standard Library Source tests
(in-package :cl-cc/test)
(in-suite cl-cc-suite)

(defun %count-substring (needle haystack)
  (let ((count 0)
        (start 0)
        (step (length needle)))
    (loop for pos = (search needle haystack :start2 start)
          while pos do
            (incf count)
            (setf start (+ pos step)))
    count))

(deftest stdlib-source-string-present
  "The extracted standard library source string exists."
  (assert-true (stringp cl-cc::*standard-library-source*))
  (assert-true (> (length cl-cc::*standard-library-source*) 0)))

(deftest-each stdlib-source-contains-key-defuns
  "The standard library source keeps representative top-level definitions."
  :cases (("mapcar"               "(defun mapcar")
          ("reduce"               "(defun reduce")
          ("class-precedence-list" "(defun class-precedence-list")
          ("set-fdefinition"      "(defun set-fdefinition"))
  (needle)
  (assert-true (search needle cl-cc::*standard-library-source*)))

(deftest stdlib-source-has-many-defun-forms
  "The source string still contains the bulk of the stdlib as defun forms."
  (assert-true (> (%count-substring "(defun " cl-cc::*standard-library-source*) 20)))
