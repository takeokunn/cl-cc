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
(in-suite cl-cc-suite)

;;; ─── hash-table-values ───────────────────────────────────────────────────

(deftest hash-table-values-empty-returns-nil
  "hash-table-values returns nil for an empty hash table."
  (assert-null (cl-cc::hash-table-values (make-hash-table :test #'eq))))

(deftest hash-table-values-single-entry
  "hash-table-values returns a singleton list for a single-entry table."
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :k ht) 42)
    (assert-equal '(42) (cl-cc::hash-table-values ht))))

(deftest hash-table-values-multiple-entries
  "hash-table-values returns all values (order may vary; verify membership)."
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    (let ((vals (cl-cc::hash-table-values ht)))
      (assert-= 3 (length vals))
      (assert-true (member 1 vals))
      (assert-true (member 2 vals))
      (assert-true (member 3 vals)))))

;;; ─── vm-register-host-bridge ─────────────────────────────────────────────

(deftest vm-register-host-bridge-adds-to-table
  "vm-register-host-bridge registers a symbol in *vm-host-bridge-functions*."
  (let ((sym (gensym "BRIDGE-TEST-")))
    (cl-cc::vm-register-host-bridge sym)
    (assert-true (gethash sym cl-cc::*vm-host-bridge-functions*))))

(deftest vm-register-host-bridge-idempotent
  "Registering the same symbol twice does not cause an error."
  (let ((sym (gensym "BRIDGE-IDEM-")))
    (cl-cc::vm-register-host-bridge sym)
    (cl-cc::vm-register-host-bridge sym)
    (assert-true (gethash sym cl-cc::*vm-host-bridge-functions*))))

;;; ─── slot-definition-name ────────────────────────────────────────────────

(deftest slot-definition-name-symbol-returns-self
  "slot-definition-name returns the symbol when the slot is represented as a symbol."
  (assert-eq 'my-slot (cl-cc::slot-definition-name 'my-slot)))

(deftest slot-definition-name-hash-table-returns-name
  "slot-definition-name reads :name from a hash-table slot descriptor."
  (let ((slot (make-hash-table :test #'eq)))
    (setf (gethash :name slot) 'count)
    (assert-eq 'count (cl-cc::slot-definition-name slot))))

(deftest slot-definition-name-nil-for-other
  "slot-definition-name returns nil for non-symbol, non-hash-table values."
  (assert-null (cl-cc::slot-definition-name 42)))

;;; ─── slot-definition-initform ────────────────────────────────────────────

(deftest-each slot-definition-initform
  "slot-definition-initform extracts :initform from a hash-table descriptor, or nil."
  :cases (("stored-value"   0     (let ((s (make-hash-table :test #'eq)))
                                    (setf (gethash :initform s) 0) s))
          ("absent"         nil   (make-hash-table :test #'eq))
          ("symbol-slot"    nil   'x))
  (expected slot)
  (if expected
      (assert-= expected (cl-cc::slot-definition-initform slot))
      (assert-null (cl-cc::slot-definition-initform slot))))

;;; ─── slot-definition-initargs ────────────────────────────────────────────

(deftest-each slot-definition-initargs
  "slot-definition-initargs extracts :initargs list from a hash-table descriptor, or nil."
  :cases (("stored-list"   '(:count)  (let ((s (make-hash-table :test #'eq)))
                                        (setf (gethash :initargs s) '(:count)) s))
          ("symbol-slot"   nil        'x))
  (expected slot)
  (if expected
      (assert-equal expected (cl-cc::slot-definition-initargs slot))
      (assert-null (cl-cc::slot-definition-initargs slot))))

;;; ─── slot-definition-allocation ──────────────────────────────────────────

(deftest-each slot-definition-allocation
  "slot-definition-allocation returns :allocation value or :instance as default."
  :cases (("instance-default"  :instance  (make-hash-table :test #'eq))
          ("class-when-set"    :class     (let ((s (make-hash-table :test #'eq)))
                                            (setf (gethash :allocation s) :class) s))
          ("symbol-slot"       :instance  'x))
  (expected slot)
  (assert-eq expected (cl-cc::slot-definition-allocation slot)))

;;; ─── %class-slot-initargs-for-slot ──────────────────────────────────────

(deftest class-slot-initargs-for-slot-finds-matching-initarg
  "%class-slot-initargs-for-slot returns initargs that initialise the named slot."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__ class)
          '((:count . count) (:value . value)))
    (assert-equal '(:count) (cl-cc::%class-slot-initargs-for-slot class 'count))))

(deftest class-slot-initargs-for-slot-nil-when-absent
  "%class-slot-initargs-for-slot returns nil when no initarg maps to the slot."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__ class) '((:x . x)))
    (assert-null (cl-cc::%class-slot-initargs-for-slot class 'y))))

(deftest class-slot-initargs-for-slot-nil-for-non-ht-class
  "%class-slot-initargs-for-slot returns nil for a non-hash-table class."
  (assert-null (cl-cc::%class-slot-initargs-for-slot 'not-a-class 'slot)))

;;; ─── %class-slot-metadata ────────────────────────────────────────────────

(deftest class-slot-metadata-returns-hash-table
  "%class-slot-metadata returns a hash table descriptor for the named slot."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__  class) '((:n . n)))
    (setf (gethash :__initforms__ class) '((n . 0)))
    (setf (gethash :__class-slots__ class) nil)
    (let ((slot (cl-cc::%class-slot-metadata class 'n)))
      (assert-true (hash-table-p slot))
      (assert-eq 'n   (gethash :name slot))
      (assert-=  0    (gethash :initform slot))
      (assert-equal '(:n) (gethash :initargs slot))
      (assert-eq :instance (gethash :allocation slot)))))

(deftest class-slot-metadata-class-allocation-for-class-slot
  "%class-slot-metadata marks slots listed in :__class-slots__ as :class allocation."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__   class) nil)
    (setf (gethash :__initforms__  class) nil)
    (setf (gethash :__class-slots__ class) '(shared))
    (let ((slot (cl-cc::%class-slot-metadata class 'shared)))
      (assert-eq :class (gethash :allocation slot)))))

;;; ─── %class-slot-definitions ─────────────────────────────────────────────

(deftest class-slot-definitions-returns-list-of-descriptors
  "%class-slot-definitions returns one descriptor per slot in :__slots__."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__slots__       class) '(a b))
    (setf (gethash :__initargs__    class) nil)
    (setf (gethash :__initforms__   class) nil)
    (setf (gethash :__class-slots__ class) nil)
    (let ((defs (cl-cc::%class-slot-definitions class)))
      (assert-= 2 (length defs))
      (assert-true (every #'hash-table-p defs)))))

(deftest class-slot-definitions-nil-for-non-ht-class
  "%class-slot-definitions returns nil for non-hash-table class."
  (assert-null (cl-cc::%class-slot-definitions 'symbol-class)))

(deftest class-slot-definitions-nil-when-no-slots-key
  "%class-slot-definitions returns nil when :__slots__ is absent."
  (let ((class (make-hash-table :test #'eq)))
    (assert-null (cl-cc::%class-slot-definitions class))))

;;; ─── rt-plist-put ────────────────────────────────────────────────────────

(deftest rt-plist-put-adds-new-key-to-empty-plist
  "rt-plist-put inserts a key-value pair into an empty plist."
  (let ((result (cl-cc::rt-plist-put nil :color 'red)))
    (assert-equal '(:color red) result)))

(deftest rt-plist-put-updates-existing-key
  "rt-plist-put replaces the value for an existing key."
  (let* ((plist  '(:a 1 :b 2))
         (result (cl-cc::rt-plist-put plist :a 99)))
    (assert-= 99 (getf result :a))
    (assert-= 2  (getf result :b))))

(deftest rt-plist-put-preserves-other-keys
  "rt-plist-put does not remove unrelated keys."
  (let* ((plist  '(:x 10 :y 20 :z 30))
         (result (cl-cc::rt-plist-put plist :y 99)))
    (assert-= 10 (getf result :x))
    (assert-= 99 (getf result :y))
    (assert-= 30 (getf result :z))))

(deftest rt-plist-put-non-destructive
  "rt-plist-put does not modify the original plist."
  (let* ((plist '(:a 1))
         (result (cl-cc::rt-plist-put plist :a 2)))
    (declare (ignore result))
    (assert-= 1 (getf plist :a))))

;;; ─── generic-function-methods ────────────────────────────────────────────

(deftest generic-function-methods-nil-for-no-methods-key
  "generic-function-methods returns nil when :__methods__ is absent."
  (let ((gf (make-hash-table :test #'eq)))
    (assert-null (cl-cc::generic-function-methods gf))))

(deftest generic-function-methods-returns-value-list
  "generic-function-methods returns all registered methods as a list."
  (let ((gf (make-hash-table :test #'eq))
        (methods-ht (make-hash-table :test #'equal)))
    (setf (gethash '(integer) methods-ht) 'method-a)
    (setf (gethash '(string)  methods-ht) 'method-b)
    (setf (gethash :__methods__ gf) methods-ht)
    (let ((result (cl-cc::generic-function-methods gf)))
      (assert-= 2 (length result))
      (assert-true (member 'method-a result))
      (assert-true (member 'method-b result)))))

;;; ─── generic-function-method-combination ─────────────────────────────────

(deftest generic-function-method-combination-defaults-to-standard
  "generic-function-method-combination returns STANDARD when not explicitly set."
  (let ((gf (make-hash-table :test #'eq)))
    (assert-eq 'standard (cl-cc::generic-function-method-combination gf))))

(deftest generic-function-method-combination-returns-stored-value
  "generic-function-method-combination returns the stored combination name."
  (let ((gf (make-hash-table :test #'eq)))
    (setf (gethash :__method-combination__ gf) '+)
    (assert-eq '+ (cl-cc::generic-function-method-combination gf))))
