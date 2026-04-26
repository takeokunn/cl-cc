;;;; tests/unit/runtime/runtime-clos-tests.lisp
;;;;
;;;; Tests for packages/backend/runtime/src/runtime-clos.lisp:
;;;; rt-defclass, rt-register-method, rt-call-generic, *rt-primitive-type-classifiers*,
;;;; %rt-classify-arg, %rt-eql-specializer-p, %rt-extract-eql-specializer-keys,
;;;; rt-slot-value/set/boundp/makunbound/exists-p, rt-class-name, rt-class-of.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-class 'rt-clos-test-fixture nil)
    (defclass rt-clos-test-fixture () ((x :initarg :x)))))

;;; ─── Class Descriptors ──────────────────────────────────────────────────────

(deftest rt-defclass-registers-descriptor
  "rt-defclass registers and returns a descriptor hash table with CPL metadata."
  (let ((cl-cc/runtime:*rt-class-registry* (make-hash-table :test #'eq)))
    (let ((base  (cl-cc/runtime:rt-defclass 'rt-base '() '(x)))
          (child (cl-cc/runtime:rt-defclass 'rt-child '(rt-base) '(y))))
      (assert-true (hash-table-p base))
      (assert-true (hash-table-p child))
      (assert-eq child (cl-cc/runtime:rt-find-class 'rt-child))
      (assert-eq 'rt-child (gethash :__name__ child))
      (assert-true (member 'rt-base (gethash :__cpl__ child))))))

(deftest rt-register-method-and-call-generic
  "rt-register-method stores a method; rt-call-generic dispatches it."
  (let ((cl-cc/runtime:*rt-class-registry* (make-hash-table :test #'eq)))
    (let* ((klass (cl-cc/runtime:rt-defclass 'rt-node '() '(value)))
           (obj   (make-hash-table :test #'eq))
           (gf    (make-hash-table :test #'equal)))
      (setf (gethash :__class__ obj) klass
            (gethash :__name__  gf)  'rt-describe)
      (cl-cc/runtime:rt-register-method gf 'rt-node
        (lambda (instance)
          (gethash :__name__ (gethash :__class__ instance))))
      (assert-eq 'rt-node (cl-cc/runtime:rt-call-generic gf obj)))))

;;; ─── EQL Specializers ───────────────────────────────────────────────────────

(deftest-each rt-eql-specializer-p-cases
  "%rt-eql-specializer-p recognizes (eql ...) lists only."
  :cases (("eql-form" '(eql 42)  t)
          ("non-eql"  '(foo 42)  nil)
          ("not-cons" 'symbol    nil)
          ("bare-eql" 'eql       nil))
  (form expected)
  (assert-equal expected (cl-cc/runtime::%rt-eql-specializer-p form)))

(deftest-each rt-extract-eql-specializer-keys-cases
  "%rt-extract-eql-specializer-keys extracts value from eql specializer forms."
  :cases (("direct"  '(eql 42)    '(42))
          ("wrapped" '((eql 42))  '(42))
          ("non-eql" '(integer)   nil)
          ("symbol"  'integer     nil))
  (spec expected)
  (assert-equal expected (cl-cc/runtime::%rt-extract-eql-specializer-keys spec)))

(deftest rt-call-generic-eql-dispatch
  "rt-call-generic dispatches on eql specializers before class-based lookup."
  (let ((cl-cc/runtime:*rt-class-registry* (make-hash-table :test #'eq)))
    (let ((gf (make-hash-table :test #'equal)))
      (setf (gethash :__name__ gf) 'rt-eql-test-gf)
      (cl-cc/runtime:rt-register-method gf '(eql 99) (lambda (x) (* x 2)))
      (assert-= 198 (cl-cc/runtime:rt-call-generic gf 99)))))

;;; ─── Argument Classification ────────────────────────────────────────────────

(deftest-each rt-classify-arg-primitive-types
  "*rt-primitive-type-classifiers* drives %rt-classify-arg for CL primitive values."
  :cases (("integer" 42        'integer)
          ("string"  "hello"   'string)
          ("symbol"  'foo      'symbol)
          ("unknown" 3.14      t)
          ("vector"  #(1 2 3)  t))
  (arg expected)
  (assert-equal expected (cl-cc/runtime::%rt-classify-arg arg)))

(deftest rt-classify-arg-clos-descriptor
  "%rt-classify-arg extracts :__name__ from a descriptor hash table."
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj      (make-hash-table :test #'eq)))
    (setf (gethash :__name__  class-ht) 'my-class
          (gethash :__class__ obj)       class-ht)
    (assert-eq 'my-class (cl-cc/runtime::%rt-classify-arg obj))))

(deftest rt-classify-arg-plain-hash-table-returns-t
  "%rt-classify-arg returns T for a hash table without :__class__."
  (let ((ht (make-hash-table :test #'eq)))
    (assert-eq t (cl-cc/runtime::%rt-classify-arg ht))))

(deftest rt-primitive-type-classifiers-table-entries
  "*rt-primitive-type-classifiers* contains exactly integer/string/symbol."
  (let ((table cl-cc/runtime::*rt-primitive-type-classifiers*))
    (assert-true (assoc 'integer table))
    (assert-true (assoc 'string  table))
    (assert-true (assoc 'symbol  table))
    (assert-= 3 (length table))))

;;; ─── Slot Access ────────────────────────────────────────────────────────────

(deftest rt-slot-ops
  "rt-slot-value/set/boundp/makunbound/exists-p on standard CLOS instances."
  (let ((obj (make-instance 'rt-clos-test-fixture :x 42)))
    (assert-= 42 (cl-cc/runtime:rt-slot-value obj 'x))
    (cl-cc/runtime:rt-slot-set obj 'x 99)
    (assert-= 99 (cl-cc/runtime:rt-slot-value obj 'x))
    (assert-= 1 (cl-cc/runtime:rt-slot-boundp   obj 'x))
    (assert-= 1 (cl-cc/runtime:rt-slot-exists-p obj 'x))
    (assert-= 0 (cl-cc/runtime:rt-slot-exists-p obj 'nonexistent))
    (cl-cc/runtime:rt-slot-makunbound obj 'x)
    (assert-= 0 (cl-cc/runtime:rt-slot-boundp obj 'x))))

(deftest rt-class-name-and-of
  "rt-class-name/rt-class-of work on hash-table descriptors and real CLOS objects."
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :__name__ ht) 'my-class)
    (assert-eq 'my-class (cl-cc/runtime:rt-class-name ht)))
  (let ((obj (make-instance 'rt-clos-test-fixture :x 1)))
    (assert-eq 'rt-clos-test-fixture
               (class-name (cl-cc/runtime:rt-class-of obj)))))

;;; ─── %rt-cpl-walk (extracted helper) ────────────────────────────────────────

(deftest rt-cpl-walk-stops-at-already-seen
  "%rt-cpl-walk: passing NAME already in SEEN returns SEEN unchanged."
  (let ((cl-cc/runtime:*rt-class-registry* (make-hash-table :test #'eq)))
    (assert-equal '(foo) (cl-cc/runtime::%rt-cpl-walk 'foo '(foo)))))

(deftest rt-cpl-walk-single-class-no-supers
  "%rt-cpl-walk on a class with no superclasses returns (name)."
  (let ((cl-cc/runtime:*rt-class-registry* (make-hash-table :test #'eq)))
    (cl-cc/runtime:rt-defclass 'cpl-base '() '())
    (assert-equal '(cpl-base) (cl-cc/runtime::%rt-cpl-walk 'cpl-base '()))))

(deftest rt-cpl-walk-inherits-parent
  "%rt-cpl-walk on child→parent returns (child parent) in order."
  (let ((cl-cc/runtime:*rt-class-registry* (make-hash-table :test #'eq)))
    (cl-cc/runtime:rt-defclass 'cpl-parent '() '())
    (cl-cc/runtime:rt-defclass 'cpl-child '(cpl-parent) '())
    (let ((result (cl-cc/runtime::%rt-cpl-walk 'cpl-child '())))
      (assert-true (member 'cpl-child result :test #'eq))
      (assert-true (member 'cpl-parent result :test #'eq))
      (assert-true (< (position 'cpl-child result)
                      (position 'cpl-parent result))))))
