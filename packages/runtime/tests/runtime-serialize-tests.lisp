;;;; packages/runtime/tests/runtime-serialize-tests.lisp — FR-683 tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defclass serialize-test-node ()
  ((name :initarg :name :accessor serialize-test-node-name)
   (next :initarg :next :accessor serialize-test-node-next)))

(deftest runtime-serialize-basic-types-roundtrip
  "serialize/deserialize roundtrips primitive and collection values."
  (let* ((table (make-hash-table :test #'equal))
         (value (list 42 3.5 "x" :kw #(1 2))))
    (setf (gethash "value" table) value)
    (let ((copy (cl-cc/runtime:deserialize (cl-cc/runtime:serialize table))))
      (assert-equal value (gethash "value" copy)))))

(deftest runtime-serialize-circular-cons-roundtrip
  "serialize/deserialize preserves circular references."
  (let ((cell (cons :head nil)))
    (setf (cdr cell) cell)
    (let ((copy (cl-cc/runtime:deserialize (cl-cc/runtime:serialize cell))))
      (assert-eq copy (cdr copy))
      (assert-eq :head (car copy)))))

(deftest runtime-serialize-clos-instance-roundtrip
  "serialize/deserialize preserves standard CLOS instance slots."
  (let* ((node (make-instance 'serialize-test-node :name "root"))
         (copy nil))
    (setf (serialize-test-node-next node) node)
    (setf copy (cl-cc/runtime:deserialize (cl-cc/runtime:serialize node)))
    (assert-equal "root" (serialize-test-node-name copy))
    (assert-eq copy (serialize-test-node-next copy))))
