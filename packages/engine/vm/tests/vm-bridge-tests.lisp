;;;; tests/unit/vm/vm-bridge-tests.lisp
;;;; Unit tests for src/vm/vm-bridge.lisp — host bridge + CLOS slot metadata.
;;;;
;;;; Covers: hash-table-values, vm-register-host-bridge,
;;;;   slot-definition-name, slot-definition-initform,
;;;;   slot-definition-initargs, slot-definition-allocation,
;;;;   %class-slot-initargs-for-slot, %class-slot-metadata,
;;;;   %class-slot-definitions, rt-plist-put,
;;;;   generic-function-methods, generic-function-method-combination.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── hash-table-values ───────────────────────────────────────────────────

(deftest hash-table-values-cases
  "hash-table-values: nil for empty; singleton for one entry; membership for multiple."
  (assert-null (cl-cc/vm::hash-table-values (make-hash-table :test #'eq)))
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :k ht) 42)
    (assert-equal '(42) (cl-cc/vm::hash-table-values ht)))
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    (let ((vals (cl-cc/vm::hash-table-values ht)))
      (assert-= 3 (length vals))
      (assert-true (member 1 vals))
      (assert-true (member 2 vals))
      (assert-true (member 3 vals)))))

;;; ─── vm-register-host-bridge ─────────────────────────────────────────────

(deftest vm-register-host-bridge-behavior
  "vm-register-host-bridge registers a symbol; registering twice is idempotent."
  (let ((sym (gensym "BRIDGE-TEST-")))
    (cl-cc/vm::vm-register-host-bridge sym)
    (assert-true (gethash sym cl-cc/vm::*vm-host-bridge-functions*)))
  (let ((sym (gensym "BRIDGE-IDEM-")))
    (cl-cc/vm::vm-register-host-bridge sym)
    (cl-cc/vm::vm-register-host-bridge sym)
    (assert-true (gethash sym cl-cc/vm::*vm-host-bridge-functions*))))

;;; ─── slot-definition-name ────────────────────────────────────────────────

(deftest slot-definition-name-cases
  "slot-definition-name returns name from symbol/ht-descriptor, or nil for other types."
  (assert-eq 'my-slot (cl-cc/vm::slot-definition-name 'my-slot))
  (let ((slot (make-hash-table :test #'eq)))
    (setf (gethash :name slot) 'count)
    (assert-eq 'count (cl-cc/vm::slot-definition-name slot)))
  (assert-null (cl-cc/vm::slot-definition-name 42)))

;;; ─── slot-definition-initform ────────────────────────────────────────────

(deftest-each slot-definition-initform
  "slot-definition-initform extracts :initform from a hash-table descriptor, or nil."
  :cases (("stored-value"
           (let ((s (make-hash-table :test #'eq)))
             (setf (gethash :initform s) 0) s)
           (lambda (slot)
             (assert-= 0 (cl-cc/vm::slot-definition-initform slot))))
          ("absent"
           (make-hash-table :test #'eq)
           (lambda (slot)
             (assert-null (cl-cc/vm::slot-definition-initform slot))))
          ("symbol-slot"
           'x
           (lambda (slot)
             (assert-null (cl-cc/vm::slot-definition-initform slot)))))
  (slot verify)
  (funcall verify slot))

;;; ─── slot-definition-initargs ────────────────────────────────────────────

(deftest-each slot-definition-initargs
  "slot-definition-initargs extracts :initargs list from a hash-table descriptor, or nil."
  :cases (("stored-list"
           (let ((s (make-hash-table :test #'eq)))
             (setf (gethash :initargs s) '(:count)) s)
           (lambda (slot)
             (assert-equal '(:count) (cl-cc/vm::slot-definition-initargs slot))))
          ("symbol-slot"
           'x
           (lambda (slot)
             (assert-null (cl-cc/vm::slot-definition-initargs slot)))))
  (slot verify)
  (funcall verify slot))

;;; ─── slot-definition-allocation ──────────────────────────────────────────

(deftest-each slot-definition-allocation
  "slot-definition-allocation returns :allocation value or :instance as default."
  :cases (("instance-default"  :instance  (make-hash-table :test #'eq))
          ("class-when-set"    :class     (let ((s (make-hash-table :test #'eq)))
                                            (setf (gethash :allocation s) :class) s))
          ("symbol-slot"       :instance  'x))
  (expected slot)
  (assert-eq expected (cl-cc/vm::slot-definition-allocation slot)))

;;; ─── %class-slot-initargs-for-slot ──────────────────────────────────────

(deftest class-slot-initargs-for-slot-cases
  "%class-slot-initargs-for-slot: finds initarg; nil when absent; nil for non-ht class."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__ class)
          '((:count . count) (:value . value)))
    (assert-equal '(:count) (cl-cc/vm::%class-slot-initargs-for-slot class 'count)))
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__ class) '((:x . x)))
    (assert-null (cl-cc/vm::%class-slot-initargs-for-slot class 'y)))
  (assert-null (cl-cc/vm::%class-slot-initargs-for-slot 'not-a-class 'slot)))

;;; ─── %class-slot-metadata ────────────────────────────────────────────────

(deftest class-slot-metadata-cases
  "%class-slot-metadata: returns hash-table descriptor; marks class-allocated slots."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__  class) '((:n . n)))
    (setf (gethash :__initforms__ class) '((n . 0)))
    (setf (gethash :__class-slots__ class) nil)
    (let ((slot (cl-cc/vm::%class-slot-metadata class 'n)))
      (assert-true (hash-table-p slot))
      (assert-eq 'n   (gethash :name slot))
      (assert-=  0    (gethash :initform slot))
      (assert-equal '(:n) (gethash :initargs slot))
      (assert-eq :instance (gethash :allocation slot))))
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__   class) nil)
    (setf (gethash :__initforms__  class) nil)
    (setf (gethash :__class-slots__ class) '(shared))
    (let ((slot (cl-cc/vm::%class-slot-metadata class 'shared)))
      (assert-eq :class (gethash :allocation slot)))))

;;; ─── %class-slot-definitions ─────────────────────────────────────────────

(deftest class-slot-definitions-returns-list-of-descriptors
  "%class-slot-definitions returns one descriptor per slot in :__slots__."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__slots__       class) '(a b))
    (setf (gethash :__initargs__    class) nil)
    (setf (gethash :__initforms__   class) nil)
    (setf (gethash :__class-slots__ class) nil)
    (let ((defs (cl-cc/vm::%class-slot-definitions class)))
      (assert-= 2 (length defs))
      (assert-true (every #'hash-table-p defs)))))

(deftest-each class-slot-definitions-nil-cases
  "%class-slot-definitions returns nil for a non-hash-table class or when :__slots__ is absent."
  :cases (("non-ht"   'symbol-class)
          ("no-slots" (make-hash-table :test #'eq)))
  (class)
  (assert-null (cl-cc/vm::%class-slot-definitions class)))

;;; ─── rt-plist-put ────────────────────────────────────────────────────────

(deftest rt-plist-put-cases
  "rt-plist-put: inserts into empty; updates existing; preserves others; non-destructive."
  (assert-equal '(:color red) (cl-cc/bootstrap::rt-plist-put nil :color 'red))
  (let* ((plist  '(:a 1 :b 2))
         (result (cl-cc/bootstrap::rt-plist-put plist :a 99)))
    (assert-= 99 (getf result :a))
    (assert-= 2  (getf result :b)))
  (let* ((plist  '(:x 10 :y 20 :z 30))
         (result (cl-cc/bootstrap::rt-plist-put plist :y 99)))
    (assert-= 10 (getf result :x))
    (assert-= 99 (getf result :y))
    (assert-= 30 (getf result :z)))
  (let* ((plist '(:a 1))
         (result (cl-cc/bootstrap::rt-plist-put plist :a 2)))
    (declare (ignore result))
    (assert-= 1 (getf plist :a))))

;;; ─── generic-function-methods ────────────────────────────────────────────

(deftest generic-function-methods-cases
  "generic-function-methods: nil when :__methods__ absent; returns all methods otherwise."
  (assert-null (cl-cc/vm::generic-function-methods (make-hash-table :test #'eq)))
  (let ((gf (make-hash-table :test #'eq))
        (methods-ht (make-hash-table :test #'equal)))
    (setf (gethash '(integer) methods-ht) 'method-a)
    (setf (gethash '(string)  methods-ht) 'method-b)
    (setf (gethash :__methods__ gf) methods-ht)
    (let ((result (cl-cc/vm::generic-function-methods gf)))
      (assert-= 2 (length result))
      (assert-true (member 'method-a result))
      (assert-true (member 'method-b result)))))

;;; ─── generic-function-method-combination ─────────────────────────────────

(deftest generic-function-method-combination-behavior
  "generic-function-method-combination defaults to STANDARD; returns stored value when set."
  (let ((gf (make-hash-table :test #'eq)))
    (assert-eq 'standard (cl-cc/vm::generic-function-method-combination gf)))
  (let ((gf (make-hash-table :test #'eq)))
    (setf (gethash :__method-combination__ gf) '+)
    (assert-eq '+ (cl-cc/vm::generic-function-method-combination gf))))
