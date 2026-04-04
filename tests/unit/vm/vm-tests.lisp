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

(deftest vm-build-list-stack-allocates-values
  "vm-build-list reuses the provided rest tail when stack allocation is enabled."
  (let* ((values (list 1 2 3))
          (result (cl-cc::vm-build-list nil values :stack-allocate-p t)))
    (assert-equal values result)
    (assert-true (eq values result))))

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

;;; ─── vm runtime objects and heap helpers ────────────────────────────────────

(deftest vm-closure-object
  "vm-closure-object: entry-label/params/captured-values stored; typep passes."
  ;; basic slots
  (let ((c (make-instance 'cl-cc::vm-closure-object
                           :entry-label "my-fn"
                           :params (list :r1 :r2)
                           :captured-values #())))
    (assert-string= "my-fn" (cl-cc::vm-closure-entry-label c))
    (assert-equal (list :r1 :r2) (cl-cc::vm-closure-params c))
    (assert-true (vectorp (cl-cc::vm-closure-captured-values c)))
    (assert-= 0 (length (cl-cc::vm-closure-captured-values c)))
    (assert-true (typep c 'cl-cc::vm-closure-object)))
  ;; captured variables
  (let ((c (make-instance 'cl-cc::vm-closure-object
                           :entry-label "adder"
                           :params (list :r1)
                           :captured-values (vector (cons :r0 10)))))
    (assert-true (vectorp (cl-cc::vm-closure-captured-values c)))
    (assert-= 1 (length (cl-cc::vm-closure-captured-values c)))
    (assert-equal (cons :r0 10) (aref (cl-cc::vm-closure-captured-values c) 0))))

(deftest vm-cons-cell
  "vm-cons-cell: stores car/cdr, car is setf-able, subtype of vm-heap-object."
  (let ((cell (make-instance 'cl-cc::vm-cons-cell :car 1 :cdr 2)))
    (assert-= 1 (cl-cc::vm-cons-cell-car cell))
    (assert-= 2 (cl-cc::vm-cons-cell-cdr cell))
    (setf (cl-cc::vm-cons-cell-car cell) 99)
    (assert-= 99 (cl-cc::vm-cons-cell-car cell))
    (assert-true (typep cell 'cl-cc::vm-heap-object))))

(deftest vm-heap-address-struct
  "make-vm-heap-address creates a struct with correct value; predicate recognizes it."
  (let ((ha (cl-cc::make-vm-heap-address :value 42)))
    (assert-= 42 (cl-cc::vm-heap-address-value ha))
    (assert-true (cl-cc::vm-heap-address-p ha))))

(deftest rt-plist-put
  "rt-plist-put: add new key, replace existing, preserve others, non-destructive."
  (assert-equal 42 (getf (cl-cc::rt-plist-put nil :foo 42) :foo))
  (let ((result (cl-cc::rt-plist-put '(:foo 1 :bar 2) :foo 99)))
    (assert-equal 99 (getf result :foo))
    (assert-equal 2  (getf result :bar)))
  (let ((result (cl-cc::rt-plist-put '(:a 1 :b 2 :c 3) :b 20)))
    (assert-equal 1  (getf result :a))
    (assert-equal 20 (getf result :b))
    (assert-equal 3  (getf result :c)))
  (let ((orig '(:x 10)))
    (cl-cc::rt-plist-put orig :x 99)
    (assert-equal 10 (getf orig :x))))

(deftest vm-heap-alloc-operations
  "vm-heap-alloc returns a positive integer; get roundtrips; set overwrites; addresses are unique."
  (let ((s (make-instance 'cl-cc::vm-state)))
    (let ((addr (cl-cc::vm-heap-alloc s :some-object)))
      (assert-true (integerp addr))
      (assert-true (> addr 0))))
  (let ((s (make-instance 'cl-cc::vm-state)))
    (let ((addr (cl-cc::vm-heap-alloc s "original")))
      (cl-cc::vm-heap-set s addr "replaced")
      (assert-string= "replaced" (cl-cc::vm-heap-get s addr))))
  (let ((s (make-instance 'cl-cc::vm-state)))
    (let ((a1 (cl-cc::vm-heap-alloc s 1))
          (a2 (cl-cc::vm-heap-alloc s 2))
          (a3 (cl-cc::vm-heap-alloc s 3)))
      (assert-true (/= a1 a2))
      (assert-true (/= a2 a3))
      (assert-true (/= a1 a3)))))

(deftest-each vm-heap-alloc-roundtrip
  "vm-heap-alloc followed by vm-heap-get retrieves the original object for any value type."
  :cases (("string" "test-payload")
          ("list"   '(a b c)))
  (value)
  (let* ((s    (make-instance 'cl-cc::vm-state))
         (addr (cl-cc::vm-heap-alloc s value)))
    (assert-equal value (cl-cc::vm-heap-get s addr))))
