;;;; tests/unit/vm/vm-clos-execute-tests.lisp
;;;; Coverage for src/vm/vm-clos-execute.lisp:
;;;;   execute-instruction for vm-class-def, vm-make-obj,
;;;;   vm-slot-read, vm-slot-write, vm-slot-boundp,
;;;;   vm-slot-makunbound, vm-slot-exists-p.

(in-package :cl-cc/test)
(defsuite vm-clos-execute-suite
  :description "execute-instruction tests for CLOS VM instructions"
  :parent cl-cc-unit-suite)
(in-suite vm-clos-execute-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-clos-vm ()
  "Fresh vm-state for CLOS instruction tests."
  (make-instance 'cl-cc/vm::vm-io-state))
(defun class-def-inst (dst name &key (supers '()) (slots '()) (initargs '()) (class-slots '()))
  "Build a vm-class-def instruction for simple class registration."
  (cl-cc:make-vm-class-def
   :dst dst
   :class-name name
   :superclasses supers
   :slot-names slots
   :slot-initargs initargs
   :slot-initform-regs nil
   :default-initarg-regs nil
   :class-slots class-slots))

(defun exec-class-def (s dst name &key (supers '()) (slots '()) (initargs '()) (class-slots '()))
  "Execute a vm-class-def and return the new PC."
  (values (cl-cc/vm::execute-instruction
           (class-def-inst dst name :supers supers :slots slots :initargs initargs :class-slots class-slots)
           s 0 nil)))
(defun make-cdef-for-test (name &key (supers '()) (default-initarg-regs nil) (class-slots nil))
  "Build a vm-class-def with empty slot/initarg lists and explicit defaults for testing."
  (cl-cc:make-vm-class-def
   :dst :R0 :class-name name :superclasses supers
   :slot-names '() :slot-initargs '() :slot-initform-regs nil
   :default-initarg-regs default-initarg-regs :class-slots class-slots))

(defun test-instance-class (instance)
  "Return INSTANCE's class descriptor for either supported instance layout."
  (cond
    ((hash-table-p instance) (gethash :__class__ instance))
    ((and (vectorp instance) (> (length instance) 0)) (aref instance 0))))

(defun test-slot-index (class slot-name)
  "Return SLOT-NAME's vector index in CLASS."
  (let ((location (cdr (assoc slot-name (gethash :__slot-locations__ class) :test #'eq))))
    (and location (1+ location))))

(defun test-instance-slot-value (instance slot-name)
  "Return SLOT-NAME from INSTANCE using the active raw layout."
  (let ((class (test-instance-class instance)))
    (if (hash-table-p instance)
        (gethash slot-name instance)
        (aref instance (test-slot-index class slot-name)))))

(defun test-instance-slot-bound-p (instance slot-name)
  "Return whether SLOT-NAME is bound in INSTANCE using VM raw slot logic."
  (multiple-value-bind (class class-slots) (cl-cc/vm::%vm-class-slots-of instance)
    (cl-cc/vm::%vm-raw-slot-boundp class class-slots instance slot-name)))

(defun test-instance-makunbound (instance slot-name)
  "Make SLOT-NAME unbound in INSTANCE using VM raw slot logic."
  (multiple-value-bind (class class-slots) (cl-cc/vm::%vm-class-slots-of instance)
    (cl-cc/vm::%vm-raw-slot-makunbound class class-slots instance slot-name)))

;;; ─── vm-class-def / vm-make-obj ──────────────────────────────────────────

(deftest vm-class-def-registers-in-class-registry
  "vm-class-def adds the class name to the vm-class-registry."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'my-class :slots '(x y))
    (assert-true (gethash 'my-class (cl-cc/vm::vm-class-registry s)))))

(deftest vm-class-def-stores-name-in-ht
  "vm-class-def stores :__name__ in the class hash-table written to DST."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'my-class :slots '(a b))
    (let ((class-ht (cl-cc:vm-reg-get s :R0)))
      (assert-true (hash-table-p class-ht))
      (assert-eq 'my-class (gethash :__name__ class-ht)))))

(deftest vm-class-def-stores-slots-in-ht
  "vm-class-def stores declared slot names under :__slots__ in the class HT."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'pt :slots '(x y))
    (let* ((class-ht (cl-cc:vm-reg-get s :R0))
           (slots (gethash :__slots__ class-ht)))
      (assert-true (member 'x slots))
      (assert-true (member 'y slots)))))

(deftest vm-class-def-advances-pc
  "vm-class-def returns PC+1."
  (let ((s (make-clos-vm)))
    (let ((new-pc (first (multiple-value-list
                          (cl-cc/vm::execute-instruction
                           (class-def-inst :R0 'foo :slots '())
                           s 5 nil)))))
      (assert-= 6 new-pc))))

(deftest vm-class-def-computes-cpl
  "vm-class-def computes a CPL containing the class itself and all superclasses."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'base)
    (exec-class-def s :R1 'child :supers '(base))
    (let* ((child-ht (cl-cc:vm-reg-get s :R1))
           (cpl (gethash :__cpl__ child-ht)))
      (assert-true (member 'child cpl))
      (assert-true (member 'base  cpl)))))

(deftest vm-make-obj-stores-class-ref
  "vm-make-obj creates an instance pointing to the class HT."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'animal :slots '(name))
    (let ((class-ht (cl-cc:vm-reg-get s :R0)))
      (cl-cc/vm::execute-instruction
       (cl-cc:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
       s 0 nil)
      (let ((obj-ht (cl-cc:vm-reg-get s :R1)))
        (assert-true (vectorp obj-ht))
        (assert-eq class-ht (test-instance-class obj-ht))))))

(deftest vm-make-obj-initializes-slots-to-nil
  "vm-make-obj creates entries for all declared slots, initialized to NIL."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'pt :slots '(x y))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
     s 0 nil)
    (let ((obj-ht (cl-cc:vm-reg-get s :R1)))
      (assert-null (test-instance-slot-value obj-ht 'x))
      (assert-null (test-instance-slot-value obj-ht 'y)))))

;;; ─── vm-slot-read / vm-slot-write / vm-slot-boundp / vm-slot-makunbound ─

(defun make-test-instance (s class-reg obj-reg class-name slots)
  "Register CLASS-NAME with SLOTS and create an instance, storing in OBJ-REG."
  (exec-class-def s class-reg class-name :slots slots)
  (cl-cc/vm::execute-instruction
   (cl-cc:make-vm-make-obj :dst obj-reg :class-reg class-reg :initarg-regs nil)
   s 0 nil))

(deftest vm-slot-write-read-roundtrip
  "vm-slot-write stores a value; vm-slot-read retrieves it."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'box '(width))
    (cl-cc:vm-reg-set s :R2 42)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-write :obj-reg :R1 :slot-name 'width :value-reg :R2)
     s 0 nil)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-read :dst :R3 :obj-reg :R1 :slot-name 'width)
     s 0 nil)
    (assert-= 42 (cl-cc:vm-reg-get s :R3))))

(deftest vm-slot-read-signals-error-when-unbound
  "vm-slot-read signals an error when the slot key has been removed from the instance HT."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'thing '(val))
    (test-instance-makunbound (cl-cc:vm-reg-get s :R1) 'val)
    (assert-signals error
      (cl-cc/vm::execute-instruction
       (cl-cc:make-vm-slot-read :dst :R2 :obj-reg :R1 :slot-name 'val)
       s 0 nil))))

(deftest vm-slot-write-advances-pc
  "vm-slot-write returns PC+1."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'c '(v))
    (cl-cc:vm-reg-set s :R2 0)
    (let ((new-pc (first (multiple-value-list
                          (cl-cc/vm::execute-instruction
                           (cl-cc:make-vm-slot-write :obj-reg :R1 :slot-name 'v :value-reg :R2)
                           s 7 nil)))))
      (assert-= 8 new-pc))))

(deftest-each vm-slot-boundp-bound-and-unbound
  "vm-slot-boundp stores T when slot is present, NIL when the slot key has been removed."
  :cases (("bound"   nil)
          ("unbound" t))
  (remove-p)
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'car '(model))
    (when remove-p (test-instance-makunbound (cl-cc:vm-reg-get s :R1) 'model))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'model)
     s 0 nil)
    (if remove-p
        (assert-false (cl-cc:vm-reg-get s :R2))
        (assert-true  (cl-cc:vm-reg-get s :R2)))))

(deftest vm-slot-boundp-uses-class-slot-storage
  "vm-slot-boundp checks the class HT for class-allocated slots."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'shared-box :slots '(shared) :class-slots '(shared))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
     s 0 nil)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'shared)
     s 0 nil)
    (assert-true (cl-cc:vm-reg-get s :R2))
    (remhash 'shared (cl-cc:vm-reg-get s :R0))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'shared)
     s 0 nil)
    (assert-false (cl-cc:vm-reg-get s :R2))))

(deftest vm-slot-boundp-migrates-obsolete-instance
  "vm-slot-boundp triggers lazy migration before checking redefined-class slots."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'redef :slots '(x))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
     s 0 nil)
    (exec-class-def s :R3 'redef :slots '(y))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'y)
     s 0 nil)
    (assert-true (cl-cc:vm-reg-get s :R2))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'x)
     s 0 nil)
    (assert-false (cl-cc:vm-reg-get s :R2))))

(deftest vm-slot-makunbound-removes-key-and-stores-obj
  "vm-slot-makunbound removes the slot key from the instance HT and writes the object to DST."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'node '(data))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-makunbound :dst :R2 :obj-reg :R1 :slot-name-sym 'data)
     s 0 nil)
    (assert-false (test-instance-slot-bound-p (cl-cc:vm-reg-get s :R1) 'data))
    (assert-eq (cl-cc:vm-reg-get s :R1)
               (cl-cc:vm-reg-get s :R2))))

(deftest vm-slot-makunbound-removes-class-slot-storage
  "vm-slot-makunbound removes class-allocated slots from the class HT."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'shared-node :slots '(shared) :class-slots '(shared))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
     s 0 nil)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-makunbound :dst :R2 :obj-reg :R1 :slot-name-sym 'shared)
     s 0 nil)
    (assert-false (nth-value 1 (gethash 'shared (cl-cc:vm-reg-get s :R0))))
    (assert-false (test-instance-slot-bound-p (cl-cc:vm-reg-get s :R1) 'shared))
    (assert-eq (cl-cc:vm-reg-get s :R1)
               (cl-cc:vm-reg-get s :R2))))

(deftest vm-slot-makunbound-migrates-obsolete-instance
  "vm-slot-makunbound triggers lazy migration before unbinding a redefined-class slot."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'makun-redef :slots '(x))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
     s 0 nil)
    (exec-class-def s :R3 'makun-redef :slots '(y))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-makunbound :dst :R2 :obj-reg :R1 :slot-name-sym 'y)
     s 0 nil)
    (assert-eq (cl-cc:vm-reg-get s :R3)
               (test-instance-class (cl-cc:vm-reg-get s :R1)))
    (assert-false (test-instance-slot-bound-p (cl-cc:vm-reg-get s :R1) 'y))))

;;; ─── vm-class-def helper functions ──────────────────────────────────────

(defun %make-reg-with-base (slots &key (initargs nil))
  "Build a class registry HT containing a single 'base class with given SLOTS."
  (let ((reg (make-hash-table :test #'eq))
        (base-ht (make-hash-table :test #'eq)))
    (setf (gethash :__slots__ base-ht) slots
          (gethash :__superclasses__ base-ht) nil)
    (when initargs (setf (gethash :__initargs__ base-ht) initargs))
    (setf (gethash 'base reg) base-ht)
    reg))

(deftest vm-cdef-collect-slots-merges-inherited
  "%vm-cdef-collect-slots merges super slots with own slots, deduplicating."
  (let* ((reg (%make-reg-with-base '(a b)))
         (slots (cl-cc/vm::%vm-cdef-collect-slots '(base) '(b c) reg)))
    (assert-true (member 'a slots))
    (assert-true (member 'b slots))
    (assert-true (member 'c slots))
    (assert-= 1 (count 'b slots))))

(deftest vm-cdef-collect-initargs-merges-and-deduplicates
  "%vm-cdef-collect-initargs merges super initargs with own, deduplicating by key."
  (let* ((reg (%make-reg-with-base '(x) :initargs '((:x-arg . x))))
         (iargs (cl-cc/vm::%vm-cdef-collect-initargs '(base) '((:x-arg . x) (:y-arg . y)) reg)))
    (assert-true (assoc :x-arg iargs))
    (assert-true (assoc :y-arg iargs))
    (assert-= 1 (count :x-arg iargs :key #'car))))

(deftest vm-cdef-collect-default-initargs-from-regs
  "%vm-cdef-collect-default-initargs resolves register values from the vm-state."
  (let* ((s    (make-clos-vm))
         (inst (make-cdef-for-test 'foo :default-initarg-regs '((:size . :R1))))
         (reg  (cl-cc/vm::vm-class-registry s)))
    (cl-cc:vm-reg-set s :R1 42)
    (let ((result (cl-cc/vm::%vm-cdef-collect-default-initargs inst '() reg s)))
      (assert-= 1 (length result))
      (assert-= 42 (cdr (assoc :size result))))))

(deftest vm-cdef-collect-default-initargs-overrides-super
  "%vm-cdef-collect-default-initargs: own reg value overrides super default; non-overridden super default is preserved."
  (let* ((s       (make-clos-vm))
         (base-ht (make-hash-table :test #'eq))
         (reg     (cl-cc/vm::vm-class-registry s))
         (inst    (make-cdef-for-test 'child :supers '(base)
                                      :default-initarg-regs '((:width . :R2)))))
    (setf (gethash :__default-initargs__ base-ht) '((:width . 10) (:height . 20))
          (gethash 'base reg) base-ht)
    (cl-cc:vm-reg-set s :R2 99)
    (let ((result (cl-cc/vm::%vm-cdef-collect-default-initargs inst '(base) reg s)))
      (assert-= 99 (cdr (assoc :width result)))
      (assert-= 1 (count :width result :key #'car))
      (assert-true (assoc :height result)))))

(deftest vm-cdef-collect-class-slots-merges-inherited
  "%vm-cdef-collect-class-slots merges super class-slots with own."
  (let* ((s       (make-clos-vm))
         (base-ht (make-hash-table :test #'eq))
         (reg     (cl-cc/vm::vm-class-registry s))
         (inst    (make-cdef-for-test 'child :supers '(base) :class-slots '(count))))
    (setf (gethash :__class-slots__ base-ht) '(shared)
          (gethash 'base reg) base-ht)
    (let ((result (cl-cc/vm::%vm-cdef-collect-class-slots inst '(base) reg)))
      (assert-true (member 'count result))
      (assert-true (member 'shared result)))))

(deftest vm-cdef-collect-class-slots-no-supers
  "%vm-cdef-collect-class-slots with no superclasses returns just own class-slots."
  (let* ((reg  (make-hash-table :test #'eq))
         (inst (make-cdef-for-test 'leaf :class-slots '(x y))))
    (assert-equal '(x y) (cl-cc/vm::%vm-cdef-collect-class-slots inst '() reg))))

(deftest vm-cdef-init-class-slots-initializes-fresh-slots
  "%vm-cdef-init-class-slots writes alist values into a fresh class HT."
  (let ((class-ht (make-hash-table :test #'eq)))
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(x y) '((x . 1) (y . 2)))
    (assert-= 1 (gethash 'x class-ht))
    (assert-= 2 (gethash 'y class-ht))))

(deftest vm-cdef-init-class-slots-does-not-overwrite-existing
  "%vm-cdef-init-class-slots leaves pre-existing slot values untouched."
  (let ((class-ht (make-hash-table :test #'eq)))
    (setf (gethash 'x class-ht) 99)
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(x) '((x . 1)))
    (assert-= 99 (gethash 'x class-ht))))

(deftest vm-cdef-init-class-slots-nil-for-missing-alist
  "%vm-cdef-init-class-slots leaves slot as NIL when no alist entry exists."
  (let ((class-ht (make-hash-table :test #'eq)))
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(z) '())
    (assert-null (gethash 'z class-ht))))

(deftest vm-obj-class-ht-returns-class-entry
  "%vm-obj-class-ht extracts the :__class__ value from an instance HT."
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__class__ obj-ht) class-ht)
    (assert-eq class-ht (cl-cc/vm::%vm-obj-class-ht obj-ht))))

(deftest-each vm-obj-class-ht-nil-for-non-instance
  "%vm-obj-class-ht returns NIL for non-hash-table inputs and bare hash-tables without :__class__."
  :cases (("integer"  42)
          ("bare-ht"  (make-hash-table :test #'eq)))
  (obj)
  (assert-null (cl-cc/vm::%vm-obj-class-ht obj)))

(deftest vm-class-slots-of-returns-class-and-slots
  "%vm-class-slots-of returns the class HT and its :__class-slots__ list."
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__class-slots__ class-ht) '(shared-slot)
          (gethash :__class__ obj-ht) class-ht)
    (multiple-value-bind (c-ht c-slots)
        (cl-cc/vm::%vm-class-slots-of obj-ht)
      (assert-eq class-ht c-ht)
      (assert-equal '(shared-slot) c-slots))))

(deftest vm-apply-initarg-instance-slot
  "%vm-apply-initarg stores the value in the instance HT for a non-class-slot."
  (let* ((obj-ht      (make-hash-table :test #'eq))
         (class-ht    (make-hash-table :test #'eq))
         (initarg-map '((:width . width))))
    (cl-cc/vm::%vm-apply-initarg :width 100 initarg-map '() class-ht obj-ht)
    (assert-= 100 (gethash 'width obj-ht))))

(deftest vm-apply-initarg-class-slot
  "%vm-apply-initarg stores the value in the class HT for a class-slot, not the instance HT."
  (let* ((obj-ht      (make-hash-table :test #'eq))
         (class-ht    (make-hash-table :test #'eq))
         (initarg-map '((:count . count))))
    (cl-cc/vm::%vm-apply-initarg :count 5 initarg-map '(count) class-ht obj-ht)
    (assert-= 5 (gethash 'count class-ht))
    (assert-null (gethash 'count obj-ht))))

;;; ─── copy-instance layout coverage ───────────────────────────────────────

(deftest vm-copy-instance-vector-backed-standard-instance
  "copy-instance creates a distinct vector-backed object with the same class and shallow-copied slots."
  (assert-evaluates-to
   "(progn
      (defclass vm-copy-vector () ((items :initarg :items) (flag :initarg :flag)))
      (let* ((items (list 1 2 3))
             (obj (make-instance 'vm-copy-vector :items items :flag :original))
             (copy (copy-instance obj)))
        (setf (slot-value copy 'flag) :changed)
        (if (and (vectorp obj)
                 (vectorp copy)
                 (not (eq obj copy))
                 (eq (class-of obj) (class-of copy))
                 (eq (slot-value copy 'items) items)
                 (eq (slot-value obj 'flag) :original)
                 (eq (slot-value copy 'flag) :changed))
            :ok
            :bad)))"
   :ok
   :stdlib t))

(deftest vm-copy-instance-vector-backed-preserves-unbound-slot
  "copy-instance preserves the vector unbound-slot marker for unbound slots."
  (assert-evaluates-to
   "(progn
      (defclass vm-copy-vector-unbound () ((x :initarg :x) (y :initarg :y)))
      (let* ((obj (make-instance 'vm-copy-vector-unbound :x 10 :y 20)))
        (slot-makunbound obj 'y)
        (let ((copy (copy-instance obj)))
          (if (and (slot-boundp copy 'x)
                   (= (slot-value copy 'x) 10)
                   (not (slot-boundp copy 'y)))
              :ok
              :bad))))"
   :ok
   :stdlib t))

(deftest vm-copy-instance-hash-table-backed-standard-instance
  "copy-instance creates a distinct hash-table-backed object with the same class and shallow-copied slots."
  (assert-evaluates-to
   "(progn
      (defclass vm-copy-hash-meta (standard-class) ())
      (defclass vm-copy-hash ()
        ((items :initarg :items) (flag :initarg :flag))
        (:metaclass vm-copy-hash-meta))
      (let* ((items (list :a :b))
             (obj (make-instance 'vm-copy-hash :items items :flag :original))
             (copy (copy-instance obj)))
        (setf (slot-value copy 'flag) :changed)
        (if (and (hash-table-p obj)
                 (hash-table-p copy)
                 (not (eq obj copy))
                 (eq (gethash :__class__ obj) (gethash :__class__ copy))
                 (eq (slot-value copy 'items) items)
                 (eq (slot-value obj 'flag) :original)
                 (eq (slot-value copy 'flag) :changed))
            :ok
            :bad)))"
   :ok
   :stdlib t))

(deftest vm-copy-instance-hash-table-backed-preserves-unbound-slot
  "copy-instance preserves absent hash-table slot bindings for unbound slots."
  (assert-evaluates-to
   "(progn
      (defclass vm-copy-hash-unbound-meta (standard-class) ())
      (defclass vm-copy-hash-unbound ()
        ((x :initarg :x) (y :initarg :y))
        (:metaclass vm-copy-hash-unbound-meta))
      (let ((obj (make-instance 'vm-copy-hash-unbound :x 10 :y 20)))
        (slot-makunbound obj 'y)
        (let ((copy (copy-instance obj)))
          (if (and (slot-boundp copy 'x)
                   (= (slot-value copy 'x) 10)
                   (not (slot-boundp copy 'y)))
              :ok
              :bad))))"
   :ok
   :stdlib t))

;;; ─── vm-slot-exists-p / vm-class-name-fn / vm-class-of-fn / vm-find-class

(deftest-each vm-slot-exists-p-declared-and-undeclared
  "vm-slot-exists-p stores T for declared slots and NIL for undeclared ones."
  :cases (("declared"   'width  t)
          ("undeclared" 'color  nil))
  (slot-sym expected-p)
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'rect '(width height))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-slot-exists-p :dst :R2 :obj-reg :R1 :slot-name-sym slot-sym)
     s 0 nil)
    (if expected-p
        (assert-true  (cl-cc:vm-reg-get s :R2))
        (assert-false (cl-cc:vm-reg-get s :R2)))))

(deftest vm-class-name-fn-stores-name
  "vm-class-name-fn writes the class name symbol into DST."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'my-class)
    (cl-cc:vm-reg-set s :R1 (cl-cc:vm-reg-get s :R0))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-class-name-fn :dst :R2 :src :R1)
     s 0 nil)
    (assert-eq 'my-class (cl-cc:vm-reg-get s :R2))))

(deftest vm-class-name-fn-advances-pc
  "vm-class-name-fn returns PC+1."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'pc-test)
    (cl-cc:vm-reg-set s :R1 (cl-cc:vm-reg-get s :R0))
    (let ((new-pc (first (multiple-value-list
                          (cl-cc/vm::execute-instruction
                           (cl-cc:make-vm-class-name-fn :dst :R2 :src :R1)
                           s 3 nil)))))
      (assert-= 4 new-pc))))

(deftest vm-class-of-fn-returns-class-ht
  "vm-class-of-fn writes the :__class__ HT of the instance into DST."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'dog '(name breed))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-class-of-fn :dst :R2 :src :R1)
     s 0 nil)
    (assert-eq (cl-cc:vm-reg-get s :R0)
               (cl-cc:vm-reg-get s :R2))))

(deftest-each vm-find-class-registered-and-unknown
  "vm-find-class writes the class HT when registered and NIL when unknown."
  :cases (("registered" 'cat   t)
          ("unknown"    'completely-unknown-class-xyz nil))
  (class-sym register-p)
  (let ((s (make-clos-vm)))
    (when register-p (exec-class-def s :R0 class-sym))
    (cl-cc:vm-reg-set s :R1 class-sym)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-find-class :dst :R2 :src :R1)
     s 0 nil)
    (let ((found (cl-cc:vm-reg-get s :R2)))
      (if register-p
           (progn
             (assert-true (hash-table-p found))
             (assert-eq class-sym (gethash :__name__ found)))
           (assert-null found)))))

;;; ─── vm-generic-call inline cache ─────────────────────────────────────────

(deftest vm-generic-call-caches-multi-dispatch-tuple-key
  "A 2-argument GF caches by class tuple, hits on the second call, and misses after generation invalidation."
  (let* ((s (make-clos-vm))
         (labels (make-hash-table :test #'eql))
         (gf-ht (make-hash-table :test #'equal))
         (methods-ht (make-hash-table :test #'equal))
         (method-fn (make-instance 'cl-cc/vm::vm-closure-object
                                   :entry-label 'multi-method
                                   :params '(:X :Y)))
         (method-desc (make-hash-table :test #'eq))
         (inst (cl-cc:make-vm-generic-call :dst :OUT :gf-reg :GF :args '(:A :B))))
    (exec-class-def s :C0 'left)
    (exec-class-def s :C1 'right)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :A :class-reg :C0 :initarg-regs nil)
     s 0 labels)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :B :class-reg :C1 :initarg-regs nil)
     s 0 labels)
    (cl-cc/vm::vm-label-table-store labels 'multi-method 77)
    (setf (gethash :function method-desc) method-fn
          (gethash :qualifiers method-desc) nil
          (gethash :specializer method-desc) '(left right)
          (gethash :gf method-desc) gf-ht
          (gethash '(left right) methods-ht) method-desc
          (gethash :__methods__ gf-ht) methods-ht
          (gethash :__name__ gf-ht) 'multi-ic-gf
          (gethash '__ic-gen__ gf-ht) 0)
    (cl-cc:vm-reg-set s :GF gf-ht)

    ;; First call resolves through full multi-dispatch and writes a tuple-key cache.
    (assert-= 77 (first (multiple-value-list
                         (cl-cc/vm::execute-instruction inst s 10 labels))))
    (assert-equal '(left right) (first (cl-cc/vm::vm-ic-cache inst)))

    ;; Remove the method table entry; the second call can only succeed via IC hit.
    (remhash '(left right) methods-ht)
    (assert-= 77 (first (multiple-value-list
                         (cl-cc/vm::execute-instruction inst s 20 labels))))

    ;; Generation changes invalidate the cached tuple entry.
    (incf (gethash '__ic-gen__ gf-ht))
    (assert-signals error
      (cl-cc/vm::execute-instruction inst s 30 labels))))
