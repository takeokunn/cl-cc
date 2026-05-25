;;; runtime-stdlib-3-expander-tests.lisp — FR-935 proclamation verification

(in-package :cl-cc/test)

(defsuite runtime-stdlib-3-expander-suite
  :description "Runtime-stdlib-3 expander FR verification tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-3-expander-suite)

(defun %runtime-stdlib-3-proclamation (kind name)
  (let ((bucket (gethash kind cl-cc/expand:*global-proclamations*)))
    (and bucket (gethash name bucket))))

(deftest runtime-stdlib-3-declaim-records-type-ftype-special
  "FR-935: DECLAIM records TYPE, FTYPE, and SPECIAL proclamations."
  (let ((cl-cc/expand:*global-proclamations* (make-hash-table :test #'eq)))
    (assert-null (cl-cc/expand:our-macroexpand-1
                  '(declaim (type fixnum *counter*)
                            (ftype (function (integer) integer) inc-counter)
                            (special *counter*))))
    (assert-equal 'fixnum (%runtime-stdlib-3-proclamation 'type '*counter*))
    (assert-equal '(function (integer) integer)
                  (%runtime-stdlib-3-proclamation 'ftype 'inc-counter))
    (assert-true (%runtime-stdlib-3-proclamation 'special '*counter*))))
