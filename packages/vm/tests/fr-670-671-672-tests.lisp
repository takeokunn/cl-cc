(in-package :cl-cc/test)

(defsuite fr-670-671-672-suite
  :description "FR-670 flat closures, FR-671 slot vectors, FR-672 vtable dispatch"
  :parent cl-cc-unit-suite)

(in-suite fr-670-671-672-suite)

(deftest fr-670-flat-closure-uses-fixed-vector-access
  "Flat closures store captures in a dense shared vector with O(1) indexed access."
  (let* ((env (cl-cc/vm::vm-make-flat-environment #(:x :y) #(10 20)))
         (closure (cl-cc/vm::vm-make-flat-closure "L" '(:arg) #(:x :y) #(0 0)
                                                  :environment env)))
    (assert-true (typep closure 'cl-cc/vm::vm-flat-closure))
    (assert-equal 10 (cl-cc/vm::vm-flat-closure-ref closure 0))
    (assert-equal 20 (cl-cc/vm::vm-flat-closure-ref closure 1))
    (setf (cl-cc/vm::vm-flat-closure-ref closure 1) 99)
    (assert-equal 99 (aref (cl-cc/vm::vm-flat-environment-values env) 1))))

(deftest fr-670-lambda-liftable-small-non-recursive-closures
  "Small non-recursive non-escaping lambdas are eligible for free-vars→params lifting."
  (assert-true (cl-cc/vm::vm-lambda-liftable-p '(:a :b) :recursive-p nil :escaping-p nil))
  (assert-false (cl-cc/vm::vm-lambda-liftable-p '(:a :b) :recursive-p t :escaping-p nil))
  (assert-equal '(:a :b :x)
                (cl-cc/vm::vm-lambda-lift-params '(:x) '(:a :b))))

(deftest fr-671-slot-vector-layout-and-direct-ref
  "Class slot layouts provide fixed offsets, and slot values are direct vector refs."
  (let* ((class (make-hash-table :test #'eq))
         (object (make-array 3 :initial-element cl-cc/vm::*unbound-slot-marker*)))
    (setf (gethash :__name__ class) 'point
          (gethash :__slots__ class) '(x y)
          (aref object 0) class)
    (cl-cc/vm::vm-install-slot-vector-layout class '(x y))
    (assert-equal 0 (cl-cc/vm::vm-slot-offset class 'x))
    (assert-equal 1 (cl-cc/vm::vm-slot-offset class 'y))
    (setf (cl-cc/vm::vm-slot-ref object 0) 7)
    (setf (cl-cc/vm::vm-slot-ref object 1) 9)
    (assert-equal 7 (cl-cc/vm::vm-slot-ref object 0))
    (assert-equal 9 (cl-cc/vm::vm-slot-ref object 1))))

(deftest fr-671-change-class-remaps-shared-slots
  "CHANGE-CLASS slow path remaps same-named slots into the new fixed layout."
  (let* ((old-class (make-hash-table :test #'eq))
         (new-class (make-hash-table :test #'eq))
         (object (make-array 3 :initial-element cl-cc/vm::*unbound-slot-marker*)))
    (setf (gethash :__slots__ old-class) '(x y)
          (gethash :__slots__ new-class) '(y z x)
          (aref object 0) old-class)
    (cl-cc/vm::vm-install-slot-vector-layout old-class '(x y))
    (cl-cc/vm::vm-install-slot-vector-layout new-class '(y z x))
    (setf (cl-cc/vm::vm-slot-ref object 0) :x-value
          (cl-cc/vm::vm-slot-ref object 1) :y-value)
    (let ((remapped (cl-cc/vm::vm-change-class-remap-slot-vector object new-class)))
      (assert-equal :y-value (cl-cc/vm::vm-slot-ref remapped 0))
      (assert-equal :x-value (cl-cc/vm::vm-slot-ref remapped 2)))))

(deftest fr-672-vtable-single-dispatch-rebuilds-lazily
  "Single-dispatch primary methods are stored in per-class vtable slots."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (class (make-hash-table :test #'eq))
         (gf (make-hash-table :test #'equal))
         (methods (make-hash-table :test #'equal))
         (method-a (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label "A" :params nil))
         (method-b (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label "B" :params nil)))
    (setf (gethash :__name__ class) 'thing
          (gethash :__cpl__ class) '(thing t)
          (gethash :__methods__ gf) methods
          (gethash 'thing methods) method-a)
    (setf (gethash 'thing (cl-cc/vm::vm-class-registry state)) class)
    (cl-cc/vm::vm-mark-vtables-dirty gf)
    (assert-eq method-a (cl-cc/vm::vm-vtable-method class gf state))
    (setf (gethash 'thing methods) method-b)
    (cl-cc/vm::vm-mark-vtables-dirty gf)
    (assert-eq method-b (cl-cc/vm::vm-vtable-method class gf state))))

(deftest fr-672-two-dimensional-dispatch-table
  "Two-argument generic functions can precompute a 2D dispatch table."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (gf (make-hash-table :test #'equal))
         (methods (make-hash-table :test #'equal))
         (method :method-ab))
    (setf (gethash :__methods__ gf) methods
          (gethash '(a b) methods) method)
    (let ((table (cl-cc/vm::vm-build-2d-dispatch-table gf state '(a) '(b))))
      (assert-equal method (aref table 0 0)))))
