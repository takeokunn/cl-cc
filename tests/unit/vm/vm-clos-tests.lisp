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
  :parent cl-cc-suite)

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
    (assert-null (cl-cc::collect-inherited-slots '() reg)))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'animal :slots '(name age))
    (assert-equal '(name age)
                  (cl-cc::collect-inherited-slots '(animal) reg)))
  (let ((reg (make-test-registry)))
    ;; multiple superclasses
    (registry-add-class reg 'flyable  :slots '(wingspan))
    (registry-add-class reg 'swimable :slots '(fin-count))
    (let ((result (cl-cc::collect-inherited-slots '(flyable swimable) reg)))
      (assert-true (member 'wingspan  result))
      (assert-true (member 'fin-count result))))
  (let ((reg (make-test-registry)))
    ;; no duplicates
    (registry-add-class reg 'base  :slots '(id))
    (registry-add-class reg 'mixin :slots '(id extra))
    (let ((result (cl-cc::collect-inherited-slots '(base mixin) reg)))
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
    (assert-null (cl-cc::collect-inherited-initargs '() reg)))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'person :initargs '((:name . name) (:age . age)))
    (let ((result (cl-cc::collect-inherited-initargs '(person) reg)))
      (assert-true (assoc :name result))
      (assert-true (assoc :age  result))))
  (let ((reg (make-test-registry)))
    ;; multiple superclasses; first occurrence wins
    (registry-add-class reg 'named  :initargs '((:name . name)))
    (registry-add-class reg 'tagged :initargs '((:tag . tag)))
    (let ((result (cl-cc::collect-inherited-initargs '(named tagged) reg)))
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
                  (cl-cc::compute-class-precedence-list 'root reg)))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'animal)
    (registry-add-class reg 'dog :superclasses '(animal))
    (assert-equal '(dog animal)
                  (cl-cc::compute-class-precedence-list 'dog reg)))
  (let ((reg (make-test-registry)))
    ;; linear chain A -> B -> C
    (registry-add-class reg 'c)
    (registry-add-class reg 'b :superclasses '(c))
    (registry-add-class reg 'a :superclasses '(b))
    (assert-equal '(a b c)
                  (cl-cc::compute-class-precedence-list 'a reg)))
  (let ((reg (make-test-registry)))
    ;; diamond: A -> B, A -> C, B -> D, C -> D
    (registry-add-class reg 'd)
    (registry-add-class reg 'b :superclasses '(d))
    (registry-add-class reg 'c :superclasses '(d))
    (registry-add-class reg 'a :superclasses '(b c))
    (let ((cpl (cl-cc::compute-class-precedence-list 'a reg)))
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
                  (cl-cc::compute-class-precedence-list 'd reg)))
  ;; C3 inconsistency detection: X(A,B), Y(B,A) — conflicting orders
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'a)
    (registry-add-class reg 'b)
    (registry-add-class reg 'x :superclasses '(a b))
    (registry-add-class reg 'y :superclasses '(b a))
    ;; X and Y alone should work fine
    (assert-equal '(x a b) (cl-cc::compute-class-precedence-list 'x reg))
    (assert-equal '(y b a) (cl-cc::compute-class-precedence-list 'y reg)))
  ;; 3-level chain with branch
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'z)
    (registry-add-class reg 'y :superclasses '(z))
    (registry-add-class reg 'x :superclasses '(z))
    (registry-add-class reg 'w :superclasses '(y x))
    (let ((cpl (cl-cc::compute-class-precedence-list 'w reg)))
      (assert-equal '(w y x z) cpl))))

;;; ------------------------------------------------------------
;;; 4. vm-error-type-matches-p
;;; ------------------------------------------------------------

(deftest-each vm-error-type-matches
  "vm-error-type-matches-p dispatch table"
  :cases
  (("string-matches-error"
    "boom"  'error   t)
   ("string-matches-condition"
    "boom"  'condition t)
   ("string-matches-t"
    "boom"  't       t)
   ("string-no-match-specific-subtype"
    "boom"  'type-error nil)
   ("condition-object-matches-t"
    (make-condition 'simple-error :format-control "x") 't t))
  (error-val handler-type expected-result)
  (let ((actual (cl-cc::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (assert-true  actual)
        (assert-false actual))))

;;; ------------------------------------------------------------
;;; 5. vm-classify-arg
;;; ------------------------------------------------------------

;;; vm-classify-arg takes (arg state); state is only used for heap lookups on
;;; hash-table arguments, so nil is safe for primitive values.
(deftest-each vm-classify-arg-primitive
  "Each primitive type is classified by its CL typecase clause."
  :cases (("integer" 42      'integer)
          ("string"  "hello" 'string)
          ("symbol"  'foo    'symbol))
  (value expected-class)
  (assert-eq expected-class (cl-cc::vm-classify-arg value nil)))

(deftest vm-classify-arg-hash-table-no-class
  "A plain hash table with no :__class__ key is classified as T (catch-all)."
  (let ((ht (make-hash-table :test #'eq)))
    (assert-eq t (cl-cc::vm-classify-arg ht nil))))

(deftest vm-classify-arg-hash-table-with-class
  "A hash table representing a CLOS instance returns the class name."
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__name__ class-ht) 'my-class)
    (setf (gethash :__class__ obj-ht) class-ht)
    (assert-eq 'my-class (cl-cc::vm-classify-arg obj-ht nil))))

;;; ------------------------------------------------------------
;;; 6. vm-generic-function-p
;;; ------------------------------------------------------------

(deftest-each vm-generic-function-p
  "vm-generic-function-p recognises generic functions and rejects non-gf values."
  :cases (("plain-hash-table"   (make-hash-table :test #'eq)                          nil)
          ("integer"            99                                                     nil)
          ("hash-with-methods"  (let ((ht (make-hash-table :test #'eq)))
                                  (setf (gethash :__methods__ ht)
                                        (make-hash-table :test #'equal))
                                  ht)                                                  t))
  (value expected)
  (if expected
      (assert-true  (cl-cc::vm-generic-function-p value))
      (assert-false (cl-cc::vm-generic-function-p value))))
