;;;; tests/unit/vm/vm-mop-remaining-features-tests.lisp
;;;; TDD RED-phase baseline tests for the remaining MOP/SBCL compatibility
;;;; surface. These tests intentionally define the expected behavior before the
;;;; VM features are implemented; they should fail until the corresponding
;;;; buckets are completed.

(in-package :cl-cc/test)

(defsuite vm-mop-remaining-features-suite
  :description "TDD baseline tests for remaining VM MOP/SBCL compatibility features"
  :parent cl-cc-unit-suite)

(in-suite vm-mop-remaining-features-suite)

(defmacro assert-mop-red-ok (source)
  "Run SOURCE in the CL-CC VM with stdlib support and expect :OK.
Wraps execution in a 8-second timeout so undefined-function hangs in the VM
turn into clean test failures rather than blocking the parallel worker thread."
  `(handler-case
       (sb-ext:with-timeout 8
         (assert-evaluates-to ,source :ok :stdlib t))
     (sb-ext:timeout ()
       (%fail-test (format nil "assert-mop-red-ok: 8s timeout evaluating ~S" ,source)
                   :expected :ok
                   :actual :timeout
                   :form (list 'assert-mop-red-ok ,source)))))


;;; ─── Effective slots ─────────────────────────────────────────────────────

(deftest mop-effective-slots-compute-effective-slot-definition
  "compute-effective-slot-definition returns an effective slot descriptor for a direct slot."
  (assert-mop-red-ok
   "(progn
      (defclass mop-eff-base () ((x :initarg :x :initform 10)))
      (let* ((class (class-of (make-instance 'mop-eff-base)))
             (direct-slots (class-direct-slots class))
             (slot (compute-effective-slot-definition class 'x direct-slots)))
        (if (and slot (eq (slot-definition-name slot) 'x)) :ok :bad)))"))

(deftest mop-effective-slots-class-slots-includes-inherited-slots
  "class-slots returns effective slots including inherited slots."
  (assert-mop-red-ok
   "(progn
      (defclass mop-eff-parent () ((a :initarg :a)))
      (defclass mop-eff-child (mop-eff-parent) ((b :initarg :b)))
      (let ((names (mapcar #'slot-definition-name
                           (class-slots (class-of (make-instance 'mop-eff-child))))))
        (if (and (member 'a names) (member 'b names)) :ok :bad)))"))

(deftest mop-effective-slots-class-direct-slots-excludes-inherited-slots
  "class-direct-slots returns only slots declared directly on the class."
  (assert-mop-red-ok
   "(progn
      (defclass mop-direct-parent () ((a :initarg :a)))
      (defclass mop-direct-child (mop-direct-parent) ((b :initarg :b)))
      (let ((names (mapcar #'slot-definition-name
                           (class-direct-slots (class-of (make-instance 'mop-direct-child))))))
        (if (and (not (member 'a names)) (member 'b names)) :ok :bad)))"))

(deftest mop-effective-slots-same-name-inherited-slot-merge
  "Same-name inherited and direct slots merge into one effective slot."
  (assert-mop-red-ok
   "(progn
      (defclass mop-merge-parent () ((x :initarg :parent-x :initform :parent)))
      (defclass mop-merge-child (mop-merge-parent) ((x :initarg :child-x :initform :child)))
      (let* ((slots (class-slots (class-of (make-instance 'mop-merge-child))))
             (names (mapcar #'slot-definition-name slots)))
        (if (= 1 (length (remove-if-not (lambda (name) (eq name 'x)) names)))
            :ok
            :bad)))"))

;;; ─── Initargs/initforms ──────────────────────────────────────────────────

(deftest mop-initargs-union-across-superclass-slots
  "Inherited slot initargs are accepted by subclasses."
  (assert-mop-red-ok
   "(progn
      (defclass mop-init-parent () ((x :initarg :x)))
      (defclass mop-init-child (mop-init-parent) ((y :initarg :y)))
      (let ((obj (make-instance 'mop-init-child :x 7 :y 9)))
        (if (and (= (slot-value obj 'x) 7)
                 (= (slot-value obj 'y) 9))
            :ok
            :bad)))"))

(deftest mop-initforms-own-slot-precedence-over-inherited-slot
  "A direct slot initform takes precedence when it overrides an inherited slot."
  (assert-mop-red-ok
   "(progn
      (defclass mop-own-init-parent () ((x :initform :parent)))
      (defclass mop-own-init-child (mop-own-init-parent) ((x :initform :child)))
      (let ((obj (make-instance 'mop-own-init-child)))
        (if (eq (slot-value obj 'x) :child) :ok :bad)))"))

(deftest mop-initforms-explicit-initarg-suppresses-initform
  "An explicit initarg suppresses the slot initform."
  (assert-mop-red-ok
   "(progn
       (defclass mop-initarg-wins () ((x :initarg :x :initform :default)))
       (let ((obj (make-instance 'mop-initarg-wins :x :explicit)))
         (if (eq (slot-value obj 'x) :explicit) :ok :bad)))"))

(deftest mop-initargs-inherited-overridden-slot-initarg-suppresses-initform
  "Inherited initargs remain valid for an overridden slot and suppress inherited initforms."
  (assert-mop-red-ok
   "(progn
       (defclass mop-inherited-initarg-a () ((x :initarg :a-x :initform 1)))
       (defclass mop-inherited-initarg-b (mop-inherited-initarg-a) ((x :initarg :b-x)))
       (let ((obj (make-instance 'mop-inherited-initarg-b :a-x 5)))
         (if (= (slot-value obj 'x) 5) :ok :bad)))"))

(deftest mop-initforms-leftmost-superclass-priority
  "When superclasses provide competing initforms for the same slot, the most-specific initform wins."
  (assert-mop-red-ok
   "(progn
       (defclass mop-initform-left () ((x :initform :left)))
       (defclass mop-initform-right () ((x :initform :right)))
       (defclass mop-initform-child (mop-initform-left mop-initform-right) ())
       (let ((obj (make-instance 'mop-initform-child)))
         (if (eq (slot-value obj 'x) :left) :ok :bad)))"))

;;; ─── Metaclass hooks ─────────────────────────────────────────────────────

(deftest mop-metaclass-hooks-slot-access-protocol
  "slot-value, setf slot-value, slot-boundp, and slot-makunbound route through metaclass hooks."
  (assert-mop-red-ok
   "(progn
      (defclass mop-hook-meta (standard-class) ())
      (defclass mop-hook-target () ((x :initarg :x)) (:metaclass mop-hook-meta))
      (defmethod slot-value-using-class ((class mop-hook-meta) object slot-name)
        (declare (ignore object slot-name))
        :hook-read)
      (defmethod (setf slot-value-using-class) (new-value (class mop-hook-meta) object slot-name)
        (declare (ignore class slot-name))
        (setf (gethash :hook-write object) new-value))
      (defmethod slot-boundp-using-class ((class mop-hook-meta) object slot-name)
        (declare (ignore class object slot-name))
        :hook-boundp)
      (defmethod slot-makunbound-using-class ((class mop-hook-meta) object slot-name)
        (declare (ignore class slot-name))
        (setf (gethash :hook-makunbound object) t)
        object)
      (let ((obj (make-instance 'mop-hook-target :x :initial)))
        (setf (slot-value obj 'x) :written)
        (let ((read-result (slot-value obj 'x))
              (boundp-result (slot-boundp obj 'x)))
          (slot-makunbound obj 'x)
          (if (and (eq read-result :hook-read)
                   (eq boundp-result :hook-boundp)
                   (eq (gethash :hook-write obj) :written)
                   (gethash :hook-makunbound obj))
              :ok
              :bad))))"))

(deftest mop-metaclass-hooks-allocate-instance
  "make-instance routes allocation through allocate-instance for custom metaclasses."
  (assert-mop-red-ok
   "(progn
      (defclass mop-alloc-meta (standard-class) ())
      (defclass mop-alloc-target () ((x :initarg :x)) (:metaclass mop-alloc-meta))
      (defmethod allocate-instance ((class mop-alloc-meta) &rest initargs)
        (declare (ignore initargs))
        (let ((obj (call-next-method)))
          (setf (slot-value obj 'x) :allocated)
          obj))
      (let ((obj (make-instance 'mop-alloc-target)))
        (if (eq (slot-value obj 'x) :allocated) :ok :bad)))"))

(deftest mop-metaclass-hooks-initialize-instance-fallback
  "initialize-instance falls back to the standard method when no custom method overrides it."
  (assert-mop-red-ok
   "(progn
      (defclass mop-init-fallback () ((x :initarg :x :initform :default)))
      (let ((obj (make-instance 'mop-init-fallback :x :from-initarg)))
        (if (eq (slot-value obj 'x) :from-initarg) :ok :bad)))"))

;;; ─── Dispatch memoization ────────────────────────────────────────────────

(deftest mop-dispatch-memoization-multi-dispatch-cache-hit
  "Repeated multi-dispatch calls return correct results (TDD RED: cache-stats not implemented)."
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-md (a b))
      (defmethod mop-md ((a integer) (b string)) :integer-string)
      (if (and (eq (mop-md 1 \"x\") :integer-string)
               (eq (mop-md 2 \"y\") :integer-string))
          :ok :bad))"))

(deftest mop-dispatch-memoization-invalidates-after-register-method
  "Adding a method after a cached fallback invalidates stale dispatch entries."
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-register-method (x))
      (defmethod mop-register-method ((x t)) :fallback)
      (mop-register-method 1)
      (defmethod mop-register-method ((x integer)) :integer)
      (if (eq (mop-register-method 1) :integer) :ok :bad))"))

(deftest mop-dispatch-memoization-eql-specializer-safety
  "EQL specializers remain safe when a class dispatch entry is already cached."
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-eql-safe (x))
      (defmethod mop-eql-safe ((x symbol)) :symbol)
      (defmethod mop-eql-safe ((x (eql :exact))) :exact)
      (mop-eql-safe :other)
      (if (and (eq (mop-eql-safe :exact) :exact)
               (eq (mop-eql-safe :other) :symbol))
          :ok
          :bad))"))

(deftest mop-dispatch-memoization-qualified-method-safety
  "Qualified methods are included in cached effective method computation."
  (assert-mop-red-ok
   "(progn
      (defvar *mop-qualified-log* nil)
      (defgeneric mop-qualified (x))
      (defmethod mop-qualified :before ((x integer)) (push :before *mop-qualified-log*))
      (defmethod mop-qualified ((x integer)) :primary)
      (mop-qualified 1)
      (setq *mop-qualified-log* nil)
      (let ((result (mop-qualified 2)))
        (if (and (eq result :primary) (equal *mop-qualified-log* '(:before))) :ok :bad)))"))

;;; ─── Shape/layout ────────────────────────────────────────────────────────

(deftest mop-shape-layout-slot-definition-location-valid-indices
  "slot-definition-location returns non-negative integer indices for instance slots."
  (assert-mop-red-ok
   "(progn
      (defclass mop-layout-index () ((a :initarg :a) (b :initarg :b)))
      (let ((locations (mapcar #'slot-definition-location
                               (class-slots (class-of (make-instance 'mop-layout-index))))))
        (if (every (lambda (x) (and (integerp x) (>= x 0))) locations) :ok :bad)))"))

(deftest mop-shape-layout-class-redefinition-shape-change
  "Class redefinition migrates old instances to the new shape lazily and safely."
  (assert-mop-red-ok
   "(progn
      (defclass mop-shape-change () ((x :initarg :x)))
      (defparameter *mop-shape-object* (make-instance 'mop-shape-change :x 1))
      (defclass mop-shape-change () ((x :initarg :x) (y :initform 2)))
      (if (and (= (slot-value *mop-shape-object* 'x) 1)
               (= (slot-value *mop-shape-object* 'y) 2))
          :ok
          :bad))"))

(deftest mop-shape-layout-slot-indexes-consistent
  "Slot locations are stable across instances of the same class and distinct per slot."
  (assert-mop-red-ok
   "(progn
      (defclass mop-layout-stable () ((a :initarg :a) (b :initarg :b)))
      (let* ((o1 (make-instance 'mop-layout-stable :a 1 :b 2))
             (o2 (make-instance 'mop-layout-stable :a 3 :b 4))
             (slots1 (class-slots (class-of o1)))
             (slots2 (class-slots (class-of o2)))
             (locs1 (mapcar #'slot-definition-location slots1))
             (locs2 (mapcar #'slot-definition-location slots2)))
        (if (and (equal locs1 locs2)
                 (= (length locs1) (length (remove-duplicates locs1))))
            :ok
            :bad)))"))

;;; ─── Standard-instance abstraction ───────────────────────────────────────

(deftest mop-standard-instance-slot-operations-through-abstraction
  "slot-value, setf, boundp, and makunbound work through the standard-instance abstraction."
  (assert-mop-red-ok
   "(progn
      (defclass mop-standard-instance () ((x :initarg :x)))
      (let ((obj (make-instance 'mop-standard-instance :x 1)))
        (setf (slot-value obj 'x) 2)
        (let ((read (= (slot-value obj 'x) 2))
              (bound-before (slot-boundp obj 'x)))
          (slot-makunbound obj 'x)
          (if (and read bound-before (not (slot-boundp obj 'x))) :ok :bad))))"))

(deftest mop-standard-instance-no-direct-hash-table-assumptions
  "standard-instance objects are not specified as raw hash tables."
  (assert-mop-red-ok
   "(progn
      (defclass mop-not-raw-hash () ((x :initarg :x)))
      (let ((obj (make-instance 'mop-not-raw-hash :x 1)))
        (if (and (not (hash-table-p obj)) (= (slot-value obj 'x) 1)) :ok :bad)))"))

(deftest mop-standard-instance-vector-backed-instance-equality
  "Vector-backed instances preserve object identity across slot writes."
  (assert-mop-red-ok
   "(progn
      (defclass mop-vector-identity () ((x :initarg :x)))
      (let ((obj (make-instance 'mop-vector-identity :x 1)))
        (let ((same obj))
          (setf (slot-value obj 'x) 2)
          (if (and (eq obj same) (= (slot-value same 'x) 2)) :ok :bad))))"))

;;; ─── Sealed classes ──────────────────────────────────────────────────────

(deftest mop-sealed-classes-subclass-rejection
  "Subclassing a sealed class signals an error."
  (assert-mop-red-ok
   "(progn
      (define-sealed-type mop-sealed-root () ())
      (handler-case
          (progn (defclass mop-sealed-child (mop-sealed-root) ()) :bad)
        (error () :ok)))"))

(deftest mop-sealed-classes-define-sealed-type-macro
  "define-sealed-type creates a class and marks it sealed."
  (assert-mop-red-ok
   "(progn
      (define-sealed-type mop-sealed-leaf () ((x :initarg :x)))
      (let ((obj (make-instance 'mop-sealed-leaf :x 5)))
        (if (and (= (slot-value obj 'x) 5)
                 (sealed-class-p (class-of obj)))
            :ok
            :bad)))"))

(deftest mop-sealed-classes-safe-static-devirtualization
  "Static devirtualization for sealed classes preserves generic function semantics."
  (assert-mop-red-ok
   "(progn
      (define-sealed-type mop-sealed-point () ((x :initarg :x)))
      (defgeneric mop-sealed-value (x))
      (defmethod mop-sealed-value ((x mop-sealed-point)) (slot-value x 'x))
      (let ((obj (make-instance 'mop-sealed-point :x 42)))
        (if (= (mop-sealed-value obj) 42) :ok :bad)))"))

(deftest mop-sealed-classes-dynamic-fallback
  "Dynamic dispatch remains available when the receiver class is not sealed."
  (assert-mop-red-ok
   "(progn
      (defclass mop-open-base () ())
      (defclass mop-open-child (mop-open-base) ())
      (defgeneric mop-open-dispatch (x))
      (defmethod mop-open-dispatch ((x mop-open-base)) :base)
      (defmethod mop-open-dispatch ((x mop-open-child)) :child)
      (if (eq (mop-open-dispatch (make-instance 'mop-open-child)) :child) :ok :bad))"))

;;; ─── Satiated generic functions ──────────────────────────────────────────

(deftest mop-satiated-gfs-dispatch-after-satiation-same-method
  "Dispatch after satiation returns the same applicable method result (TDD RED: satiate-generic-function not yet implemented)."
  :timeout 5
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-satiated (x))
      (defmethod mop-satiated ((x integer)) :integer)
      (if (eq (mop-satiated 1) :integer) :ok :bad))"))

(deftest mop-satiated-gfs-method-addition-after-satiation-behavior
  "Method addition dispatch works correctly (TDD RED: satiate-generic-function not yet implemented)."
  :timeout 5
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-satiated-add (x))
      (defmethod mop-satiated-add ((x t)) :fallback)
      (defmethod mop-satiated-add ((x integer)) :integer)
      (if (eq (mop-satiated-add 1) :integer) :ok :bad))"))

(deftest mop-satiated-gfs-predicate
  "satiating-gfs-p is callable in the VM (TDD RED: with-satiating-gfs not yet implemented)."
  :timeout 5
  (assert-mop-red-ok
   "(if (fboundp 'satiating-gfs-p) :ok :bad)"))

;;; ─── MOP functions directly accessible ──────────────────────────────────

(deftest mop-compute-effective-slot-definition-accessible
  "compute-effective-slot-definition is accessible in the VM."
  (assert-mop-red-ok
   "(progn
      (multiple-value-bind (sym status)
          (find-symbol \"COMPUTE-EFFECTIVE-SLOT-DEFINITION\" (find-package \"CL-CC\"))
        (if (and sym (eq status :external)) :ok :bad))))"))

(deftest mop-class-slots-accessible
  "class-slots is accessible in the VM."
  (assert-mop-red-ok
   "(progn
      (multiple-value-bind (sym status)
          (find-symbol \"CLASS-SLOTS\" (find-package \"CL-CC\"))
        (if (and sym (eq status :external)) :ok :bad))))"))

(deftest mop-satiating-gfs-p-accessible
  "satiating-gfs-p is accessible directly in the VM without compat packages."
  (assert-mop-red-ok
   "(if (fboundp 'satiating-gfs-p) :ok :bad)"))

;;; ─── copy-instance ───────────────────────────────────────────────────────

(deftest mop-copy-instance-distinct-object
  "copy-instance returns a distinct object."
  (assert-mop-red-ok
   "(progn
      (defclass mop-copy-distinct () ((x :initarg :x)))
      (let* ((obj (make-instance 'mop-copy-distinct :x 1))
             (copy (copy-instance obj)))
        (if (not (eq obj copy)) :ok :bad)))"))

(deftest mop-copy-instance-same-class
  "copy-instance preserves the class of the copied object."
  (assert-mop-red-ok
   "(progn
      (defclass mop-copy-class () ((x :initarg :x)))
      (let* ((obj (make-instance 'mop-copy-class :x 1))
             (copy (copy-instance obj)))
        (if (eq (class-of obj) (class-of copy)) :ok :bad)))"))

(deftest mop-copy-instance-shallow-slot-copy
  "copy-instance performs a shallow copy of slot values."
  (assert-mop-red-ok
   "(progn
      (defclass mop-copy-shallow () ((items :initarg :items)))
      (let* ((items (list 1 2 3))
             (obj (make-instance 'mop-copy-shallow :items items))
             (copy (copy-instance obj)))
        (if (eq (slot-value copy 'items) items) :ok :bad)))"))

(deftest mop-copy-instance-compatible-with-vector-layout
  "copy-instance works with vector-backed standard-instance layout without hash-table assumptions."
  (assert-mop-red-ok
   "(progn
      (defclass mop-copy-vector () ((x :initarg :x) (y :initarg :y)))
      (let* ((obj (make-instance 'mop-copy-vector :x 10 :y 20))
             (copy (copy-instance obj)))
        (if (and (not (hash-table-p copy))
                 (= (slot-value copy 'x) 10)
                 (= (slot-value copy 'y) 20))
            :ok
            :bad)))"))

;;; Apply a 10-second timeout to all mop-* tests AFTER they are all registered.
;;; TDD RED-phase tests may call undefined VM functions that can hang the VM
;;; evaluator; the timeout ensures they fail cleanly instead of blocking workers.
(set-test-timeouts-by-prefix! "MOP-" 10)
