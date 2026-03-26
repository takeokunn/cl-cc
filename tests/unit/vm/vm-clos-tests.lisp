;;;; tests/unit/vm/vm-clos-tests.lisp — CLOS infrastructure + error dispatch unit tests
;;;;
;;;; Covers:
;;;;   collect-inherited-slots          (4 tests)
;;;;   collect-inherited-initargs       (3 tests)
;;;;   compute-class-precedence-list    (4 tests)
;;;;   vm-error-type-matches-p          (5 tests via deftest-each)
;;;;   vm-classify-arg                  (4 tests)
;;;;   vm-generic-function-p            (3 tests)

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

(deftest collect-inherited-slots-no-superclasses
  "No superclasses yields an empty slot list."
  (let ((reg (make-test-registry)))
    (assert-null (cl-cc::collect-inherited-slots '() reg))))

(deftest collect-inherited-slots-single-super
  "Slots from a single superclass are returned."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'animal :slots '(name age))
    (assert-equal '(name age)
                  (cl-cc::collect-inherited-slots '(animal) reg))))

(deftest collect-inherited-slots-multiple-supers
  "Slots from multiple superclasses are unioned in depth-first, left-to-right order."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'flyable  :slots '(wingspan))
    (registry-add-class reg 'swimable :slots '(fin-count))
    (let ((result (cl-cc::collect-inherited-slots '(flyable swimable) reg)))
      (assert-true (member 'wingspan  result))
      (assert-true (member 'fin-count result)))))

(deftest collect-inherited-slots-no-duplicates
  "A slot appearing in two superclasses is included only once."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'base  :slots '(id))
    (registry-add-class reg 'mixin :slots '(id extra))
    (let ((result (cl-cc::collect-inherited-slots '(base mixin) reg)))
      (assert-= 2 (length result))
      (assert-true (member 'id    result))
      (assert-true (member 'extra result)))))

;;; ------------------------------------------------------------
;;; 2. collect-inherited-initargs
;;; ------------------------------------------------------------

(deftest collect-inherited-initargs-no-superclasses
  "No superclasses yields an empty initarg list."
  (let ((reg (make-test-registry)))
    (assert-null (cl-cc::collect-inherited-initargs '() reg))))

(deftest collect-inherited-initargs-single-super
  "Initargs from a single superclass are returned."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'person :initargs '((:name . name) (:age . age)))
    (let ((result (cl-cc::collect-inherited-initargs '(person) reg)))
      (assert-true (assoc :name result))
      (assert-true (assoc :age  result)))))

(deftest collect-inherited-initargs-multiple-supers
  "Initargs from multiple superclasses are unioned; first occurrence wins."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'named  :initargs '((:name . name)))
    (registry-add-class reg 'tagged :initargs '((:tag . tag)))
    (let ((result (cl-cc::collect-inherited-initargs '(named tagged) reg)))
      (assert-true (assoc :name result))
      (assert-true (assoc :tag  result)))))

;;; ------------------------------------------------------------
;;; 3. compute-class-precedence-list
;;; ------------------------------------------------------------

(deftest compute-cpl-no-super
  "A class with no superclasses has a CPL containing only itself."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'root)
    (assert-equal '(root)
                  (cl-cc::compute-class-precedence-list 'root reg))))

(deftest compute-cpl-single-super
  "A class with one superclass yields [class super] in CPL."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'animal)
    (registry-add-class reg 'dog :superclasses '(animal))
    (assert-equal '(dog animal)
                  (cl-cc::compute-class-precedence-list 'dog reg))))

(deftest compute-cpl-linear-chain
  "A linear chain A -> B -> C yields CPL [A B C]."
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'c)
    (registry-add-class reg 'b :superclasses '(c))
    (registry-add-class reg 'a :superclasses '(b))
    (assert-equal '(a b c)
                  (cl-cc::compute-class-precedence-list 'a reg))))

(deftest compute-cpl-diamond-no-crash
  "Diamond inheritance (A -> B, A -> C, B -> D, C -> D) does not crash and includes all classes."
  (let ((reg (make-test-registry)))
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
      (assert-= (length cpl) (length (remove-duplicates cpl))))))

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

(deftest vm-classify-arg-integer
  "An integer argument is classified as 'integer."
  ;; vm-classify-arg takes (arg state); state is only used for heap lookups on
  ;; hash-table arguments, so nil is safe for primitive values.
  (assert-eq 'integer (cl-cc::vm-classify-arg 42 nil)))

(deftest vm-classify-arg-string
  "A string argument is classified as 'string."
  (assert-eq 'string (cl-cc::vm-classify-arg "hello" nil)))

(deftest vm-classify-arg-symbol
  "A symbol argument is classified as 'symbol."
  (assert-eq 'symbol (cl-cc::vm-classify-arg 'foo nil)))

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

(deftest vm-generic-function-p-plain-hash-table
  "A plain hash table without :__methods__ is not a generic function."
  (let ((ht (make-hash-table :test #'eq)))
    (assert-false (cl-cc::vm-generic-function-p ht))))

(deftest vm-generic-function-p-with-methods-key
  "A hash table that has :__methods__ set to a truthy value is a generic function."
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :__methods__ ht) (make-hash-table :test #'equal))
    (assert-true (cl-cc::vm-generic-function-p ht))))

(deftest vm-generic-function-p-non-hash-table
  "A non-hash-table value (integer) is not a generic function."
  (assert-false (cl-cc::vm-generic-function-p 99)))
