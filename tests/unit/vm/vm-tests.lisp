;;;; tests/unit/vm/vm-tests.lisp — VM core helper tests

(in-package :cl-cc/test)

(defsuite vm-suite
  :description "VM core helper tests"
  :parent cl-cc-suite)

(in-suite vm-suite)

(deftest vm-state-initial-globals
  "vm-state seeds representative standard globals at initialization time."
  (let ((state (make-instance 'cl-cc::vm-state)))
    (assert-true (hash-table-p (cl-cc::vm-global-vars state)))
    (multiple-value-bind (features found-p)
        (gethash 'cl-cc::*features* (cl-cc::vm-global-vars state))
      (assert-true found-p)
      (assert-equal '(:common-lisp :cl-cc) features))
    (multiple-value-bind (package found-p)
        (gethash 'cl-cc::*package* (cl-cc::vm-global-vars state))
      (assert-true found-p)
      (assert-eq (find-package :cl-user) package))
    (multiple-value-bind (docs found-p)
        (gethash 'cl-cc::*documentation-table* (cl-cc::vm-global-vars state))
      (assert-true found-p)
      (assert-null docs))))

(deftest vm-heap-address-normalization
  "vm-heap-address normalizes integers, wrapped addresses, and nil."
  (let ((wrapped (cl-cc::make-vm-heap-address :value 7)))
    (assert-= 9 (cl-cc::vm-heap-address 9))
    (assert-= 7 (cl-cc::vm-heap-address wrapped))
    (assert-null (cl-cc::vm-heap-address nil))))

(deftest vm-build-list-copies-values
  "vm-build-list returns a native list copy of the provided rest values."
  (let* ((values (list 1 2 3))
         (result (cl-cc::vm-build-list nil values)))
    (assert-equal values result)
    (assert-true (not (eq values result)))))

(deftest vm-host-bridge-registration
  "vm-register-host-bridge marks a symbol as host-callable in the bridge table."
  (let ((sym (gensym "VM-BRIDGE-")))
    (unwind-protect
        (progn
          (assert-null (gethash sym cl-cc::*vm-host-bridge-functions*))
          (cl-cc::vm-register-host-bridge sym)
          (assert-true (gethash sym cl-cc::*vm-host-bridge-functions*)))
      (remhash sym cl-cc::*vm-host-bridge-functions*))))

(deftest vm-generic-function-p
  "vm-generic-function-p recognizes dispatch tables tagged with :__methods__."
  (let ((table (make-hash-table :test #'eq)))
    (assert-null (cl-cc::vm-generic-function-p table))
    (setf (gethash :__methods__ table) '(:dispatch))
    (assert-true (cl-cc::vm-generic-function-p table))))
