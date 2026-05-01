;;;; tests/unit/vm/vm-dispatch-tests.lisp — VM dispatch helper tests

(in-package :cl-cc/test)

(defsuite vm-dispatch-suite
  :description "Unit tests for vm-dispatch helpers"
  :parent cl-cc-unit-suite)

(in-suite vm-dispatch-suite)

(deftest-each vm-classify-arg-primitive
  "Each primitive type is classified by its CL typecase clause."
  :cases (("integer" 42      'integer)
          ("string"  "hello" 'string)
          ("symbol"  'foo    'symbol))
  (value expected-class)
  (assert-eq expected-class (cl-cc/vm::vm-classify-arg value nil)))

(deftest vm-classify-arg-hash-table-no-class
  "A plain hash table with no :__class__ key is classified as T (catch-all)."
  (let ((ht (make-hash-table :test #'eq)))
    (assert-eq t (cl-cc/vm::vm-classify-arg ht nil))))

(deftest vm-classify-arg-hash-table-with-class
  "A hash table representing a CLOS instance returns the class name."
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__name__ class-ht) 'my-class)
    (setf (gethash :__class__ obj-ht) class-ht)
    (assert-eq 'my-class (cl-cc/vm::vm-classify-arg obj-ht nil))))

(deftest-each vm-generic-function-p
  "vm-generic-function-p recognises generic functions and rejects non-gf values."
  :cases (("plain-hash-table"
           (make-hash-table :test #'eq)
           (lambda (value)
             (assert-false (cl-cc/vm::vm-generic-function-p value))))
          ("integer"
           99
           (lambda (value)
             (assert-false (cl-cc/vm::vm-generic-function-p value))))
          ("hash-with-methods"
           (let ((ht (make-hash-table :test #'eq)))
             (setf (gethash :__methods__ ht)
                   (make-hash-table :test #'equal))
             ht)
           (lambda (value)
             (assert-true (cl-cc/vm::vm-generic-function-p value)))))
  (value verify)
  (funcall verify value))
