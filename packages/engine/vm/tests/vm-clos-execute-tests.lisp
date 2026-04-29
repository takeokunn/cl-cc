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
(defun class-def-inst (dst name &key (supers '()) (slots '()) (initargs '()))
  "Build a vm-class-def instruction for simple class registration."
  (cl-cc::make-vm-class-def
   :dst dst
   :class-name name
   :superclasses supers
   :slot-names slots
   :slot-initargs initargs
   :slot-initform-regs nil
   :default-initarg-regs nil
   :class-slots nil))

(defun exec-class-def (s dst name &key (supers '()) (slots '()) (initargs '()))
  "Execute a vm-class-def and return the new PC."
  (values (cl-cc/vm::execute-instruction
           (class-def-inst dst name :supers supers :slots slots :initargs initargs)
           s 0 nil)))
(defun make-cdef-for-test (name &key (supers '()) (default-initarg-regs nil) (class-slots nil))
  "Build a vm-class-def with empty slot/initarg lists and explicit defaults for testing."
  (cl-cc::make-vm-class-def
   :dst :R0 :class-name name :superclasses supers
   :slot-names '() :slot-initargs '() :slot-initform-regs nil
   :default-initarg-regs default-initarg-regs :class-slots class-slots))

;;; ─── vm-class-def / vm-make-obj ──────────────────────────────────────────

(deftest vm-clos-exec-class-def-make-obj-cases
  "vm-class-def: registers class; HT stores :__name__/:__slots__; advances PC; computes CPL. vm-make-obj: creates instance HT with :__class__; initializes slots to nil."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'my-class :slots '(x y))
    (assert-true (gethash 'my-class (cl-cc/vm::vm-class-registry s))))
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'my-class :slots '(a b))
    (let ((class-ht (cl-cc:vm-reg-get s :R0)))
      (assert-true (hash-table-p class-ht))
      (assert-eq 'my-class (gethash :__name__ class-ht))))
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'pt :slots '(x y))
    (let* ((class-ht (cl-cc:vm-reg-get s :R0))
           (slots (gethash :__slots__ class-ht)))
      (assert-true (member 'x slots))
      (assert-true (member 'y slots))))
  (let ((s (make-clos-vm)))
    (let ((new-pc (first (multiple-value-list
                          (cl-cc/vm::execute-instruction
                           (class-def-inst :R0 'foo :slots '())
                           s 5 nil)))))
      (assert-= 6 new-pc)))
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'base)
    (exec-class-def s :R1 'child :supers '(base))
    (let* ((child-ht (cl-cc:vm-reg-get s :R1))
           (cpl (gethash :__cpl__ child-ht)))
      (assert-true (member 'child cpl))
      (assert-true (member 'base  cpl))))
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'animal :slots '(name))
    (let ((class-ht (cl-cc:vm-reg-get s :R0)))
      (cl-cc/vm::execute-instruction
       (cl-cc::make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
       s 0 nil)
      (let ((obj-ht (cl-cc:vm-reg-get s :R1)))
        (assert-true (hash-table-p obj-ht))
        (assert-eq class-ht (gethash :__class__ obj-ht)))))
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'pt :slots '(x y))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-make-obj :dst :R1 :class-reg :R0 :initarg-regs nil)
     s 0 nil)
    (let ((obj-ht (cl-cc:vm-reg-get s :R1)))
      (assert-null (gethash 'x obj-ht))
      (assert-null (gethash 'y obj-ht)))))

;;; ─── vm-slot-read / vm-slot-write / vm-slot-boundp / vm-slot-makunbound ─

(defun make-test-instance (s class-reg obj-reg class-name slots)
  "Register CLASS-NAME with SLOTS and create an instance, storing in OBJ-REG."
  (exec-class-def s class-reg class-name :slots slots)
  (cl-cc/vm::execute-instruction
   (cl-cc::make-vm-make-obj :dst obj-reg :class-reg class-reg :initarg-regs nil)
   s 0 nil))

(deftest vm-clos-exec-slot-ops-cases
  "vm-slot-write/read (store, retrieve, error-when-unbound, PC+1); vm-slot-boundp (T/nil); vm-slot-makunbound (removes key, stores obj in DST)."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'box '(width))
    (cl-cc:vm-reg-set s :R2 42)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-write :obj-reg :R1 :slot-name 'width :value-reg :R2)
     s 0 nil)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-read :dst :R3 :obj-reg :R1 :slot-name 'width)
     s 0 nil)
    (assert-= 42 (cl-cc:vm-reg-get s :R3)))
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'thing '(val))
    (let ((obj-ht (cl-cc:vm-reg-get s :R1)))
      (remhash 'val obj-ht))
    (assert-signals error
      (cl-cc/vm::execute-instruction
       (cl-cc::make-vm-slot-read :dst :R2 :obj-reg :R1 :slot-name 'val)
       s 0 nil)))
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'c '(v))
    (cl-cc:vm-reg-set s :R2 0)
    (let ((new-pc (first (multiple-value-list
                          (cl-cc/vm::execute-instruction
                           (cl-cc::make-vm-slot-write :obj-reg :R1 :slot-name 'v :value-reg :R2)
                           s 7 nil)))))
      (assert-= 8 new-pc)))
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'car '(model))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'model)
     s 0 nil)
    (assert-true (cl-cc:vm-reg-get s :R2)))
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'car '(model))
    (remhash 'model (cl-cc:vm-reg-get s :R1))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-boundp :dst :R2 :obj-reg :R1 :slot-name-sym 'model)
     s 0 nil)
    (assert-false (cl-cc:vm-reg-get s :R2)))
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'node '(data))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-makunbound :dst :R2 :obj-reg :R1 :slot-name-sym 'data)
     s 0 nil)
    (let ((obj-ht (cl-cc:vm-reg-get s :R1)))
      (assert-false (nth-value 1 (gethash 'data obj-ht))))
    (assert-eq (cl-cc:vm-reg-get s :R1)
               (cl-cc:vm-reg-get s :R2))))

;;; ─── vm-class-def helper functions ──────────────────────────────────────

(deftest vm-cdef-collect-helpers-cases
  "Coverage for %vm-cdef-collect-slots/initargs/default-initargs/class-slots, %vm-cdef-init-class-slots,
   %vm-obj-class-ht, %vm-class-slots-of, and %vm-apply-initarg helper functions."
  (let* ((reg (make-hash-table :test #'eq))
         (base-ht (make-hash-table :test #'eq)))
    (setf (gethash :__slots__ base-ht) '(a b))
    (setf (gethash :__superclasses__ base-ht) nil)
    (setf (gethash 'base reg) base-ht)
    (let ((slots (cl-cc/vm::%vm-cdef-collect-slots '(base) '(b c) reg)))
      (assert-true (member 'a slots))
      (assert-true (member 'b slots))
      (assert-true (member 'c slots))
      (assert-= 1 (count 'b slots))))
  (let* ((reg (make-hash-table :test #'eq))
         (base-ht (make-hash-table :test #'eq)))
    (setf (gethash :__slots__ base-ht) '(x))
    (setf (gethash :__superclasses__ base-ht) nil)
    (setf (gethash :__initargs__ base-ht) '((:x-arg . x)))
    (setf (gethash 'base reg) base-ht)
    (let ((iargs (cl-cc/vm::%vm-cdef-collect-initargs '(base) '((:x-arg . x) (:y-arg . y)) reg)))
      (assert-true (assoc :x-arg iargs))
      (assert-true (assoc :y-arg iargs))
      (assert-= 1 (count :x-arg iargs :key #'car))))
  (let* ((s    (make-clos-vm))
         (inst (make-cdef-for-test 'foo :default-initarg-regs '((:size . :R1))))
         (reg  (cl-cc/vm::vm-class-registry s)))
    (cl-cc:vm-reg-set s :R1 42)
    (let ((result (cl-cc/vm::%vm-cdef-collect-default-initargs inst '() reg s)))
      (assert-= 1 (length result))
      (assert-= 42 (cdr (assoc :size result)))))
  (let* ((s        (make-clos-vm))
         (base-ht  (make-hash-table :test #'eq))
         (reg      (cl-cc/vm::vm-class-registry s))
         (inst     (make-cdef-for-test 'child :supers '(base)
                                       :default-initarg-regs '((:width . :R2)))))
    (setf (gethash :__default-initargs__ base-ht) '((:width . 10) (:height . 20)))
    (setf (gethash 'base reg) base-ht)
    (cl-cc:vm-reg-set s :R2 99)
    (let ((result (cl-cc/vm::%vm-cdef-collect-default-initargs inst '(base) reg s)))
      (assert-= 99 (cdr (assoc :width result)))
      (assert-= 1 (count :width result :key #'car))
      (assert-true (assoc :height result))))
  (let* ((s       (make-clos-vm))
         (base-ht (make-hash-table :test #'eq))
         (reg     (cl-cc/vm::vm-class-registry s))
         (inst    (make-cdef-for-test 'child :supers '(base) :class-slots '(count))))
    (setf (gethash :__class-slots__ base-ht) '(shared))
    (setf (gethash 'base reg) base-ht)
    (let ((result (cl-cc/vm::%vm-cdef-collect-class-slots inst '(base) reg)))
      (assert-true (member 'count result))
      (assert-true (member 'shared result))))
  (let* ((reg  (make-hash-table :test #'eq))
         (inst (make-cdef-for-test 'leaf :class-slots '(x y))))
    (assert-equal '(x y) (cl-cc/vm::%vm-cdef-collect-class-slots inst '() reg)))
  (let ((class-ht (make-hash-table :test #'eq)))
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(x y) '((x . 1) (y . 2)))
    (assert-= 1 (gethash 'x class-ht))
    (assert-= 2 (gethash 'y class-ht)))
  (let ((class-ht (make-hash-table :test #'eq)))
    (setf (gethash 'x class-ht) 99)
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(x) '((x . 1)))
    (assert-= 99 (gethash 'x class-ht)))
  (let ((class-ht (make-hash-table :test #'eq)))
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(z) '())
    (assert-null (gethash 'z class-ht)))
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__class__ obj-ht) class-ht)
    (assert-eq class-ht (cl-cc/vm::%vm-obj-class-ht obj-ht)))
  (assert-null (cl-cc/vm::%vm-obj-class-ht 42))
  (let ((bare-ht (make-hash-table :test #'eq)))
    (assert-null (cl-cc/vm::%vm-obj-class-ht bare-ht)))
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__class-slots__ class-ht) '(shared-slot))
    (setf (gethash :__class__ obj-ht) class-ht)
    (multiple-value-bind (c-ht c-slots)
        (cl-cc/vm::%vm-class-slots-of obj-ht)
      (assert-eq class-ht c-ht)
      (assert-equal '(shared-slot) c-slots)))
  (let* ((obj-ht      (make-hash-table :test #'eq))
         (initarg-map '((:width . width)))
         (class-ht    (make-hash-table :test #'eq)))
    (cl-cc/vm::%vm-apply-initarg :width 100 initarg-map '() class-ht obj-ht)
    (assert-= 100 (gethash 'width obj-ht)))
  (let* ((obj-ht      (make-hash-table :test #'eq))
         (class-ht    (make-hash-table :test #'eq))
         (initarg-map '((:count . count))))
    (cl-cc/vm::%vm-apply-initarg :count 5 initarg-map '(count) class-ht obj-ht)
    (assert-= 5 (gethash 'count class-ht))
    (assert-null (gethash 'count obj-ht))))

;;; ─── vm-slot-exists-p / vm-class-name-fn / vm-class-of-fn / vm-find-class

(deftest vm-clos-exec-class-inspect-fn-cases
  "vm-slot-exists-p (T/nil); vm-class-name-fn (name in DST, PC+1); vm-class-of-fn (:__class__ HT); vm-find-class (found/nil)."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'rect '(width height))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-exists-p :dst :R2 :obj-reg :R1 :slot-name-sym 'width)
     s 0 nil)
    (assert-true (cl-cc:vm-reg-get s :R2)))
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'rect '(width height))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-exists-p :dst :R2 :obj-reg :R1 :slot-name-sym 'color)
     s 0 nil)
    (assert-false (cl-cc:vm-reg-get s :R2)))
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'my-class)
    (let ((class-ht (cl-cc:vm-reg-get s :R0)))
      (cl-cc:vm-reg-set s :R1 class-ht)
      (cl-cc/vm::execute-instruction
       (cl-cc::make-vm-class-name-fn :dst :R2 :src :R1)
       s 0 nil)
      (assert-eq 'my-class (cl-cc:vm-reg-get s :R2))))
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'pc-test)
    (cl-cc:vm-reg-set s :R1 (cl-cc:vm-reg-get s :R0))
    (let ((new-pc (first (multiple-value-list
                          (cl-cc/vm::execute-instruction
                           (cl-cc::make-vm-class-name-fn :dst :R2 :src :R1)
                           s 3 nil)))))
      (assert-= 4 new-pc)))
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'dog '(name breed))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-class-of-fn :dst :R2 :src :R1)
     s 0 nil)
    (let ((class-ht (cl-cc:vm-reg-get s :R0))
          (result   (cl-cc:vm-reg-get s :R2)))
      (assert-eq class-ht result)))
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'cat)
    (cl-cc:vm-reg-set s :R1 'cat)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-find-class :dst :R2 :src :R1)
     s 0 nil)
    (let ((found (cl-cc:vm-reg-get s :R2)))
      (assert-true (hash-table-p found))
      (assert-eq 'cat (gethash :__name__ found))))
  (let ((s (make-clos-vm)))
    (cl-cc:vm-reg-set s :R0 'completely-unknown-class-xyz)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-find-class :dst :R1 :src :R0)
     s 0 nil)
    (assert-null (cl-cc:vm-reg-get s :R1))))
