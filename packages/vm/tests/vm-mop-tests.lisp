(in-package :cl-cc/test)

(defsuite vm-mop-suite
  :description "Runtime-stdlib-2 VM MOP scaffold tests"
  :parent cl-cc-unit-suite)

(in-suite vm-mop-suite)

(deftest vm-mop-loads-with-vm
  "The VM MOP scaffold test suite loads with :cl-cc-vm."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-vm nil)))

(defun make-mop-test-class (name &key superclasses direct-slots slots initargs initforms
                                   slot-types class-slots cpl direct-subclasses)
  "Build a VM class descriptor for MOP tests."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__name__ class) name
          (gethash :__superclasses__ class) superclasses
          (gethash :__direct-slots__ class) direct-slots
          (gethash :__slots__ class) (or slots direct-slots)
          (gethash :__initargs__ class) initargs
          (gethash :__initforms__ class) initforms
          (gethash :__slot-types__ class) slot-types
          (gethash :__class-slots__ class) class-slots
          (gethash :__slot-locations__ class)
          (loop for slot in (or slots direct-slots)
                for index from 0
                collect (cons slot index))
          (gethash :__cpl__ class) (or cpl (list name t))
          (gethash :__direct-subclasses__ class) direct-subclasses)
    class))

(defun make-mop-test-method (specializers &key qualifiers function)
  "Build a VM method descriptor for MOP tests."
  (let ((method (make-hash-table :test #'eq)))
    (setf (gethash :specializers method) specializers
          (gethash :specializer method)
          (if (= (length specializers) 1) (first specializers) specializers)
          (gethash :qualifiers method) qualifiers
          (gethash :function method) (or function #'identity))
    method))

(deftest fr-930-class-slot-and-class-introspection
  "FR-930: class/slot MOP introspection returns descriptor metadata."
  :timeout 10
  (let* ((parent (make-mop-test-class 'mop-parent :direct-slots '(a) :slots '(a)
                                      :direct-subclasses '(mop-child)))
         (child (make-mop-test-class 'mop-child
                                     :superclasses '(mop-parent)
                                     :direct-slots '(b)
                                     :slots '(a b)
                                     :initargs '((:a . a) (:b . b))
                                     :initforms '((a . 1) (b . 2))
                                     :slot-types '((a . integer) (b . string))
                                     :class-slots '(b)
                                     :cpl '(mop-child mop-parent t))))
    (declare (ignore parent))
    (let ((slots (cl-cc/vm::class-slots child)))
      (assert-equal '(a b) (mapcar #'cl-cc/vm::slot-definition-name slots))
      (assert-eq 'integer (cl-cc/vm::slot-definition-type (first slots)))
      (assert-= 1 (cl-cc/vm::slot-definition-initform (first slots)))
      (assert-eq :class (cl-cc/vm::slot-definition-allocation (second slots)))
      (assert-equal '(mop-parent) (cl-cc/vm::class-direct-superclasses child))
      (assert-equal '(mop-child) (cl-cc/vm::class-direct-subclasses parent))
      (assert-equal '(mop-child mop-parent t) (cl-cc/vm::class-precedence-list child)))))

(deftest fr-930-generic-function-method-introspection
  "FR-930: generic-function and method introspection includes qualified methods."
  :timeout 10
  (let* ((gf (cl-cc/vm::ensure-generic-function 'mop-gf :lambda-list '(x)))
         (primary (make-mop-test-method '(integer)))
         (before (make-mop-test-method '(integer) :qualifiers '(:before))))
    (cl-cc/vm::add-method gf primary)
    (cl-cc/vm::add-method gf before)
    (assert-true (member primary (cl-cc/vm::generic-function-methods gf)))
    (assert-true (member before (cl-cc/vm::generic-function-methods gf)))
    (assert-equal '(integer) (cl-cc/vm::method-specializers primary))
    (assert-equal '(:before) (cl-cc/vm::method-qualifiers before))
    (assert-eq 'standard (cl-cc/vm::method-combination-type
                          (cl-cc/vm::generic-function-method-combination gf)))))

(deftest fr-930-satiating-gfs-p-introspection
  "FR-930: satiating-gfs-p reports generic-function dispatch saturation state."
  :timeout 10
  (let ((gf (cl-cc/vm::ensure-generic-function 'mop-satiated :lambda-list '(x))))
    (assert-false (cl-cc/vm::satiating-gfs-p))
    (assert-false (cl-cc/vm::satiating-gfs-p gf))
    (setf (gethash :__satiated__ gf) t)
    (assert-true (cl-cc/vm::satiating-gfs-p gf))
    (setf (gethash :__satiated__ gf) nil)
    (assert-false (cl-cc/vm::satiating-gfs-p gf))))

(deftest fr-930-compute-applicable-methods
  "FR-930: compute-applicable-methods selects applicable descriptors."
  :timeout 10
  (let* ((gf (cl-cc/vm::ensure-generic-function 'mop-applicable :lambda-list '(x)))
         (fallback (make-mop-test-method '(t)))
         (integer-method (make-mop-test-method '(integer)))
         (string-method (make-mop-test-method '(string))))
    (cl-cc/vm::add-method gf fallback)
    (cl-cc/vm::add-method gf integer-method)
    (cl-cc/vm::add-method gf string-method)
    (let ((methods (cl-cc/vm::compute-applicable-methods gf (list 42))))
      (assert-true (member integer-method methods))
      (assert-true (member fallback methods))
      (assert-false (member string-method methods)))))

(deftest fr-931-compute-applicable-methods-using-classes
  "FR-931: class-only method selection returns methods and definitivep."
  :timeout 10
  (let* ((gf (cl-cc/vm::ensure-generic-function 'mop-camuc :lambda-list '(x)))
         (fallback (make-mop-test-method '(t)))
         (integer-method (make-mop-test-method '(integer))))
    (cl-cc/vm::add-method gf fallback)
    (cl-cc/vm::add-method gf integer-method)
    (multiple-value-bind (methods definitivep)
        (cl-cc/vm::compute-applicable-methods-using-classes gf '(integer))
      (assert-true definitivep)
      (assert-true (member integer-method methods))
      (assert-true (member fallback methods)))))

(deftest fr-931-find-add-remove-method
  "FR-931: find-method, add-method, and remove-method support dynamic method changes."
  :timeout 10
  (let* ((gf (cl-cc/vm::ensure-generic-function 'mop-dynamic :lambda-list '(x)))
         (method (make-mop-test-method '(integer))))
    (cl-cc/vm::add-method gf method)
    (assert-eq method (cl-cc/vm::find-method gf nil '(integer) nil))
    (cl-cc/vm::remove-method gf method)
    (assert-null (cl-cc/vm::find-method gf nil '(integer) nil))))

(deftest fr-931-slot-value-using-class-hooks
  "FR-931: slot-value-using-class operations use class slot metadata."
  :timeout 10
  (let* ((class (make-mop-test-class 'mop-slots :direct-slots '(x) :slots '(x)))
         (object (make-array 2)))
    (setf (aref object 0) class)
    (setf (cl-cc/vm::slot-value-using-class class object 'x) 7)
    (assert-= 7 (cl-cc/vm::slot-value-using-class class object 'x))
    (assert-true (cl-cc/vm::slot-bound-using-class-p class object 'x))
    (cl-cc/vm::slot-makunbound-using-class class object 'x)
    (assert-false (cl-cc/vm::slot-bound-using-class-p class object 'x))))
