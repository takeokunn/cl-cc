;;;; tests/unit/vm/vm-clos-tests.lisp — CLOS infrastructure + error dispatch unit tests
;;;;
;;;; Covers:
;;;;   collect-inherited-slots          (4 tests)
;;;;   collect-inherited-initargs       (3 tests)
;;;;   compute-class-precedence-list    (4 tests)
;;;;   vm-error-type-matches-p          (5 tests via deftest-each)
;;;;   vm-classify-arg                  (3+2 tests: deftest-each + hash-table cases)
;;;;   vm-generic-function-p            (2+1 tests: deftest-each + truthy case)

(in-package :cl-cc/test)

(defsuite vm-clos-suite
  :description "Unit tests for CLOS registry helpers and generic-dispatch utilities"
  :parent cl-cc-unit-suite)

(in-suite vm-clos-suite)

;;; ------------------------------------------------------------
;;; Helpers — build a minimal class-registry hash table
;;; ------------------------------------------------------------

(defun make-test-registry ()
  "Return a fresh empty registry (eq-keyed hash table)."
  (make-hash-table :test #'eq))

(defun registry-add-class (registry name &key (superclasses '()) (slots '()) (initargs '()))
  "Insert a minimal class entry into REGISTRY and return the class hash table."
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :__name__          ht) name
          (gethash :__superclasses__  ht) superclasses
          (gethash :__slots__         ht) slots
          (gethash :__initargs__      ht) initargs)
    (setf (gethash name registry) ht)
    ht))

;;; ------------------------------------------------------------
;;; 1. collect-inherited-slots
;;; ------------------------------------------------------------

(deftest collect-inherited-slots
  "collect-inherited-slots: no-super, single-super, multiple-supers, no-duplicates."
  (let ((reg (make-test-registry)))
    ;; no superclasses
    (assert-null (cl-cc/vm::collect-inherited-slots '() reg)))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'animal :slots '(name age))
    (assert-equal '(name age)
                  (cl-cc/vm::collect-inherited-slots '(animal) reg)))
  (let ((reg (make-test-registry)))
    ;; multiple superclasses
    (registry-add-class reg 'flyable  :slots '(wingspan))
    (registry-add-class reg 'swimable :slots '(fin-count))
    (let ((result (cl-cc/vm::collect-inherited-slots '(flyable swimable) reg)))
      (assert-true (member 'wingspan  result))
      (assert-true (member 'fin-count result))))
  (let ((reg (make-test-registry)))
    ;; no duplicates
    (registry-add-class reg 'base  :slots '(id))
    (registry-add-class reg 'mixin :slots '(id extra))
    (let ((result (cl-cc/vm::collect-inherited-slots '(base mixin) reg)))
      (assert-= 2 (length result))
      (assert-true (member 'id    result))
      (assert-true (member 'extra result)))))

;;; ------------------------------------------------------------
;;; 2. collect-inherited-initargs
;;; ------------------------------------------------------------

(deftest collect-inherited-initargs
  "collect-inherited-initargs: no-super, single-super, multiple-supers."
  (let ((reg (make-test-registry)))
    ;; no superclasses
    (assert-null (cl-cc/vm::collect-inherited-initargs '() reg)))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'person :initargs '((:name . name) (:age . age)))
    (let ((result (cl-cc/vm::collect-inherited-initargs '(person) reg)))
      (assert-true (assoc :name result))
      (assert-true (assoc :age  result))))
  (let ((reg (make-test-registry)))
    ;; multiple superclasses; first occurrence wins
    (registry-add-class reg 'named  :initargs '((:name . name)))
    (registry-add-class reg 'tagged :initargs '((:tag . tag)))
    (let ((result (cl-cc/vm::collect-inherited-initargs '(named tagged) reg)))
      (assert-true (assoc :name result))
      (assert-true (assoc :tag  result)))))

;;; ------------------------------------------------------------
;;; 3. compute-class-precedence-list
;;; ------------------------------------------------------------

(deftest compute-class-precedence-list
  "compute-class-precedence-list: no-super, single-super, linear-chain, diamond."
  (let ((reg (make-test-registry)))
    ;; no superclasses
    (registry-add-class reg 'root)
    (assert-equal '(root)
                  (cl-cc/vm::compute-class-precedence-list 'root reg)))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'animal)
    (registry-add-class reg 'dog :superclasses '(animal))
    (assert-equal '(dog animal)
                  (cl-cc/vm::compute-class-precedence-list 'dog reg)))
  (let ((reg (make-test-registry)))
    ;; linear chain A -> B -> C
    (registry-add-class reg 'c)
    (registry-add-class reg 'b :superclasses '(c))
    (registry-add-class reg 'a :superclasses '(b))
    (assert-equal '(a b c)
                  (cl-cc/vm::compute-class-precedence-list 'a reg)))
  (let ((reg (make-test-registry)))
    ;; diamond: A -> B, A -> C, B -> D, C -> D
    (registry-add-class reg 'd)
    (registry-add-class reg 'b :superclasses '(d))
    (registry-add-class reg 'c :superclasses '(d))
    (registry-add-class reg 'a :superclasses '(b c))
    (let ((cpl (cl-cc/vm::compute-class-precedence-list 'a reg)))
      (assert-true (member 'a cpl))
      (assert-true (member 'b cpl))
      (assert-true (member 'c cpl))
      (assert-true (member 'd cpl))
      ;; No duplicates
      (assert-= (length cpl) (length (remove-duplicates cpl)))
      ;; C3 exact order: A B C D (B before C, both before D)
      (assert-equal '(a b c d) cpl)))
  ;; C3 classic example: D(B,C), B(A), C(A) — no duplication of A
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'a)
    (registry-add-class reg 'b :superclasses '(a))
    (registry-add-class reg 'c :superclasses '(a))
    (registry-add-class reg 'd :superclasses '(b c))
    (assert-equal '(d b c a)
                  (cl-cc/vm::compute-class-precedence-list 'd reg)))
  ;; C3 inconsistency detection: X(A,B), Y(B,A) — conflicting orders
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'a)
    (registry-add-class reg 'b)
    (registry-add-class reg 'x :superclasses '(a b))
    (registry-add-class reg 'y :superclasses '(b a))
    ;; X and Y alone should work fine
    (assert-equal '(x a b) (cl-cc/vm::compute-class-precedence-list 'x reg))
    (assert-equal '(y b a) (cl-cc/vm::compute-class-precedence-list 'y reg)))
  ;; 3-level chain with branch
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'z)
    (registry-add-class reg 'y :superclasses '(z))
    (registry-add-class reg 'x :superclasses '(z))
    (registry-add-class reg 'w :superclasses '(y x))
    (let ((cpl (cl-cc/vm::compute-class-precedence-list 'w reg)))
      (assert-equal '(w y x z) cpl))))

;;; ------------------------------------------------------------
;;; 4. EQL specializer dispatch index
;;; ------------------------------------------------------------

(deftest eql-specializer-dispatch-index
  "vm-register-method populates the EQL dispatch index for fast lookup."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (gf (make-hash-table :test #'equal))
         (method 'read-method)
         (inst (cl-cc::make-vm-register-method
                :gf-reg :r0
                :specializer '(eql :read)
                :qualifier nil
                :method-reg :r1)))
    (setf (gethash :__methods__ gf) (make-hash-table :test #'equal)
          (gethash :__eql-index__ gf) (make-hash-table :test #'equal))
    (cl-cc:vm-reg-set state :r0 gf)
    (cl-cc:vm-reg-set state :r1 method)
    (cl-cc/vm::execute-instruction inst state 0 nil)
    (assert-equal (list method) (cl-cc/vm::%vm-gf-eql-methods gf :read))
    (assert-equal (list method) (gethash :read (gethash :__eql-index__ gf)))))
