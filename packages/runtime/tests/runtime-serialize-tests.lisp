;;;; packages/runtime/tests/runtime-serialize-tests.lisp — FR-683 tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defclass serialize-test-node ()
  ((name :initarg :name :accessor serialize-test-node-name)
   (next :initarg :next :accessor serialize-test-node-next)))

(defun assert-runtime-serialization-roundtrip (value &key (test #'equalp))
  (let ((copy (cl-cc/runtime:deserialize (cl-cc/runtime:serialize value))))
    (assert-true (funcall test value copy))
    copy))

(deftest runtime-serialize-basic-types-roundtrip
  "serialize/deserialize roundtrips primitive and collection values."
  (let* ((table (make-hash-table :test #'equal))
         (value (list 42 3.5 "x" :kw #(1 2))))
    (setf (gethash "value" table) value)
    (let ((copy (assert-runtime-serialization-roundtrip table)))
      (assert-true (equalp value (gethash "value" copy))))))

(deftest runtime-serialize-circular-cons-roundtrip
  "serialize/deserialize preserves circular references."
  (let ((cell (cons :head nil)))
    (setf (cdr cell) cell)
    (let ((copy (cl-cc/runtime:deserialize (cl-cc/runtime:serialize cell))))
      (assert-eq copy (cdr copy))
      (assert-eq :head (car copy)))))

(deftest runtime-serialize-clos-instance-roundtrip
  "serialize/deserialize preserves standard CLOS instance slots."
  (let ((node (make-instance 'serialize-test-node :name "root")))
    (setf (serialize-test-node-next node) node)
    (assert-runtime-serialization-roundtrip
     node
     :test (lambda (expected copy)
             (and (not (eq expected copy))
                  (typep copy 'serialize-test-node)
                  (equal "root" (serialize-test-node-name copy))
                  (eq copy (serialize-test-node-next copy)))))))
