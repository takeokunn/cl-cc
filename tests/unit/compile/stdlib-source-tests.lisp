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

(deftest stdlib-source-contains-key-defuns
  "The standard library source keeps representative top-level definitions."
  (let ((source cl-cc::*standard-library-source*))
    (assert-true (search "(defun mapcar" source))
    (assert-true (search "(defun reduce" source))
    (assert-true (search "(defun class-precedence-list" source))
    (assert-true (search "(defun set-fdefinition" source))))

(deftest stdlib-source-has-many-defun-forms
  "The source string still contains the bulk of the stdlib as defun forms."
  (assert-true (> (%count-substring "(defun " cl-cc::*standard-library-source*) 20)))
