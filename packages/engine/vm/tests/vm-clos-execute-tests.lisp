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

;;; ─── vm-class-def ────────────────────────────────────────────────────────

(deftest vm-clos-exec-class-def-cases
  "vm-class-def: registers class; stores HT in dst with :__name__/:__slots__; advances PC; computes CPL."
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
      (assert-true (member 'base  cpl)))))

;;; ─── vm-make-obj ─────────────────────────────────────────────────────────

(deftest vm-clos-exec-make-obj-cases
  "vm-make-obj: creates instance HT with :__class__ pointer; initializes slots to nil."
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

;;; ─── vm-slot-read / vm-slot-write ────────────────────────────────────────

(defun make-test-instance (s class-reg obj-reg class-name slots)
  "Register CLASS-NAME with SLOTS and create an instance, storing in OBJ-REG."
  (exec-class-def s class-reg class-name :slots slots)
  (cl-cc/vm::execute-instruction
   (cl-cc::make-vm-make-obj :dst obj-reg :class-reg class-reg :initarg-regs nil)
   s 0 nil))

(deftest vm-clos-exec-slot-write-then-read
  "vm-slot-write stores a value; vm-slot-read retrieves it."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'box '(width))
    ;; Write 42 into slot 'width
    (cl-cc:vm-reg-set s :R2 42)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-write :obj-reg :R1 :slot-name 'width :value-reg :R2)
     s 0 nil)
    ;; Read it back
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-read :dst :R3 :obj-reg :R1 :slot-name 'width)
     s 0 nil)
    (assert-= 42 (cl-cc:vm-reg-get s :R3))))

(deftest vm-clos-exec-slot-read-unbound-signals-error
  "vm-slot-read signals an error when the slot is unbound (removed by makunbound)."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'thing '(val))
    ;; Remove the slot to simulate unbound
    (let ((obj-ht (cl-cc:vm-reg-get s :R1)))
      (remhash 'val obj-ht))
    (assert-signals error
      (cl-cc/vm::execute-instruction
       (cl-cc::make-vm-slot-read :dst :R2 :obj-reg :R1 :slot-name 'val)
       s 0 nil))))

(deftest vm-clos-exec-slot-write-advances-pc
  "vm-slot-write advances PC by 1."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'c '(v))
    (cl-cc:vm-reg-set s :R2 0)
    (let ((new-pc (first (multiple-value-list
                          (cl-cc/vm::execute-instruction
                           (cl-cc::make-vm-slot-write :obj-reg :R1 :slot-name 'v :value-reg :R2)
                           s 7 nil)))))
      (assert-= 8 new-pc))))

;;; ─── vm-slot-boundp ──────────────────────────────────────────────────────

(deftest vm-clos-exec-slot-boundp-cases
  "vm-slot-boundp: T when slot key exists; nil after remhash (makunbound simulation)."
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
    (assert-false (cl-cc:vm-reg-get s :R2))))

;;; ─── vm-slot-makunbound ──────────────────────────────────────────────────

(deftest vm-clos-exec-slot-makunbound-cases
  "vm-slot-makunbound: removes key from object HT; stores object itself in DST."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'node '(data))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-slot-makunbound :dst :R2 :obj-reg :R1 :slot-name-sym 'data)
     s 0 nil)
    (let ((obj-ht (cl-cc:vm-reg-get s :R1)))
      (assert-false (nth-value 1 (gethash 'data obj-ht))))
    (assert-eq (cl-cc:vm-reg-get s :R1)
               (cl-cc:vm-reg-get s :R2))))

;;; ─── %vm-cdef-collect-slots / %vm-cdef-collect-initargs ─────────────────

(deftest vm-cdef-collect-slots-merges-inherited-first
  "%vm-cdef-collect-slots puts inherited slots before own slots, deduplicating."
  (let* ((reg (make-hash-table :test #'eq))
         (base-ht (make-hash-table :test #'eq)))
    (setf (gethash :__slots__ base-ht) '(a b))
    (setf (gethash :__superclasses__ base-ht) nil)
    (setf (gethash 'base reg) base-ht)
    (let ((slots (cl-cc/vm::%vm-cdef-collect-slots '(base) '(b c) reg)))
      (assert-true (member 'a slots))
      (assert-true (member 'b slots))
      (assert-true (member 'c slots))
      (assert-= 1 (count 'b slots)))))

(deftest vm-cdef-collect-initargs-puts-inherited-first
  "%vm-cdef-collect-initargs merges inherited entries before own, deduplicating by key."
  (let* ((reg (make-hash-table :test #'eq))
         (base-ht (make-hash-table :test #'eq)))
    (setf (gethash :__slots__ base-ht) '(x))
    (setf (gethash :__superclasses__ base-ht) nil)
    (setf (gethash :__initargs__ base-ht) '((:x-arg . x)))
    (setf (gethash 'base reg) base-ht)
    (let ((iargs (cl-cc/vm::%vm-cdef-collect-initargs '(base) '((:x-arg . x) (:y-arg . y)) reg)))
      (assert-true (assoc :x-arg iargs))
      (assert-true (assoc :y-arg iargs))
      (assert-= 1 (count :x-arg iargs :key #'car)))))

;;; ─── %vm-cdef-collect-default-initargs ──────────────────────────────────

(deftest vm-cdef-collect-default-initargs-own-values
  "%vm-cdef-collect-default-initargs collects own default-initarg values from registers."
  (let* ((s    (make-clos-vm))
         (inst (cl-cc::make-vm-class-def
                :dst :R0 :class-name 'foo :superclasses '()
                :slot-names '() :slot-initargs '() :slot-initform-regs nil
                :default-initarg-regs '((:size . :R1)) :class-slots nil))
         (reg  (cl-cc/vm::vm-class-registry s)))
    (cl-cc:vm-reg-set s :R1 42)
    (let ((result (cl-cc/vm::%vm-cdef-collect-default-initargs inst '() reg s)))
      (assert-= 1 (length result))
      (assert-= 42 (cdr (assoc :size result))))))

(deftest vm-cdef-collect-default-initargs-own-overrides-inherited
  "%vm-cdef-collect-default-initargs: own key takes precedence over inherited key."
  (let* ((s        (make-clos-vm))
         (base-ht  (make-hash-table :test #'eq))
         (reg      (cl-cc/vm::vm-class-registry s))
         (inst     (cl-cc::make-vm-class-def
                    :dst :R0 :class-name 'child :superclasses '(base)
                    :slot-names '() :slot-initargs '() :slot-initform-regs nil
                    :default-initarg-regs '((:width . :R2)) :class-slots nil)))
    (setf (gethash :__default-initargs__ base-ht) '((:width . 10) (:height . 20)))
    (setf (gethash 'base reg) base-ht)
    (cl-cc:vm-reg-set s :R2 99)
    (let ((result (cl-cc/vm::%vm-cdef-collect-default-initargs inst '(base) reg s)))
      (assert-= 99 (cdr (assoc :width result)))
      (assert-= 1 (count :width result :key #'car))
      (assert-true (assoc :height result)))))

;;; ─── %vm-cdef-collect-class-slots ────────────────────────────────────────

(deftest vm-cdef-collect-class-slots-merges-with-inherited
  "%vm-cdef-collect-class-slots unions own class slots with inherited class slots."
  (let* ((s       (make-clos-vm))
         (base-ht (make-hash-table :test #'eq))
         (reg     (cl-cc/vm::vm-class-registry s))
         (inst    (cl-cc::make-vm-class-def
                   :dst :R0 :class-name 'child :superclasses '(base)
                   :slot-names '() :slot-initargs '() :slot-initform-regs nil
                   :default-initarg-regs nil :class-slots '(count))))
    (setf (gethash :__class-slots__ base-ht) '(shared))
    (setf (gethash 'base reg) base-ht)
    (let ((result (cl-cc/vm::%vm-cdef-collect-class-slots inst '(base) reg)))
      (assert-true (member 'count result))
      (assert-true (member 'shared result)))))

(deftest vm-cdef-collect-class-slots-no-inheritance
  "%vm-cdef-collect-class-slots returns own slots when supers have no class-slots."
  (let* ((reg  (make-hash-table :test #'eq))
         (inst (cl-cc::make-vm-class-def
                :dst :R0 :class-name 'leaf :superclasses '()
                :slot-names '() :slot-initargs '() :slot-initform-regs nil
                :default-initarg-regs nil :class-slots '(x y))))
    (assert-equal '(x y) (cl-cc/vm::%vm-cdef-collect-class-slots inst '() reg))))

;;; ─── %vm-cdef-init-class-slots ───────────────────────────────────────────

(deftest vm-cdef-init-class-slots-sets-from-initforms
  "%vm-cdef-init-class-slots initializes class-ht entries from initform-values."
  (let ((class-ht (make-hash-table :test #'eq)))
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(x y) '((x . 1) (y . 2)))
    (assert-= 1 (gethash 'x class-ht))
    (assert-= 2 (gethash 'y class-ht))))

(deftest vm-cdef-init-class-slots-skips-existing
  "%vm-cdef-init-class-slots does not overwrite an already-initialized slot."
  (let ((class-ht (make-hash-table :test #'eq)))
    (setf (gethash 'x class-ht) 99)
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(x) '((x . 1)))
    (assert-= 99 (gethash 'x class-ht))))

(deftest vm-cdef-init-class-slots-nil-when-no-initform
  "%vm-cdef-init-class-slots stores nil when a slot has no initform entry."
  (let ((class-ht (make-hash-table :test #'eq)))
    (cl-cc/vm::%vm-cdef-init-class-slots class-ht '(z) '())
    (assert-null (gethash 'z class-ht))))

;;; ─── %vm-obj-class-ht / %vm-class-slots-of ──────────────────────────────

(deftest vm-obj-class-ht-returns-class-ht
  "%vm-obj-class-ht returns the :__class__ entry from an instance hash-table."
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__class__ obj-ht) class-ht)
    (assert-eq class-ht (cl-cc/vm::%vm-obj-class-ht obj-ht))))

(deftest vm-obj-class-ht-returns-nil-for-non-instance
  "%vm-obj-class-ht returns nil for non-hash-table or instance without :__class__."
  (assert-null (cl-cc/vm::%vm-obj-class-ht 42))
  (let ((bare-ht (make-hash-table :test #'eq)))
    (assert-null (cl-cc/vm::%vm-obj-class-ht bare-ht))))

(deftest vm-class-slots-of-returns-class-slots
  "%vm-class-slots-of extracts class-allocated slot list from the class HT."
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__class-slots__ class-ht) '(shared-slot))
    (setf (gethash :__class__ obj-ht) class-ht)
    (multiple-value-bind (c-ht c-slots)
        (cl-cc/vm::%vm-class-slots-of obj-ht)
      (assert-eq class-ht c-ht)
      (assert-equal '(shared-slot) c-slots))))

;;; ─── %vm-apply-initarg ───────────────────────────────────────────────────

(deftest vm-apply-initarg-writes-instance-slot
  "%vm-apply-initarg routes a normal initarg to the instance hash-table."
  (let* ((obj-ht      (make-hash-table :test #'eq))
         (initarg-map '((:width . width)))
         (class-ht    (make-hash-table :test #'eq)))
    (cl-cc/vm::%vm-apply-initarg :width 100 initarg-map '() class-ht obj-ht)
    (assert-= 100 (gethash 'width obj-ht))))

(deftest vm-apply-initarg-routes-class-slot
  "%vm-apply-initarg routes a class-allocated slot to the class hash-table."
  (let* ((obj-ht      (make-hash-table :test #'eq))
         (class-ht    (make-hash-table :test #'eq))
         (initarg-map '((:count . count))))
    (cl-cc/vm::%vm-apply-initarg :count 5 initarg-map '(count) class-ht obj-ht)
    (assert-= 5 (gethash 'count class-ht))
    (assert-null (gethash 'count obj-ht))))

;;; ─── vm-slot-exists-p ────────────────────────────────────────────────────

(deftest vm-clos-exec-slot-exists-p-cases
  "vm-slot-exists-p: T for defined class slot; nil for unknown slot."
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
    (assert-false (cl-cc:vm-reg-get s :R2))))

;;; ─── vm-class-name-fn ────────────────────────────────────────────────────

(deftest vm-clos-exec-class-name-fn-returns-class-name
  "vm-class-name-fn stores the :__name__ of a class HT into DST."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'my-class)
    (let ((class-ht (cl-cc:vm-reg-get s :R0)))
      (cl-cc:vm-reg-set s :R1 class-ht)
      (cl-cc/vm::execute-instruction
       (cl-cc::make-vm-class-name-fn :dst :R2 :src :R1)
       s 0 nil)
      (assert-eq 'my-class (cl-cc:vm-reg-get s :R2)))))

(deftest vm-clos-exec-class-name-fn-advances-pc
  "vm-class-name-fn advances PC by 1."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'pc-test)
    (cl-cc:vm-reg-set s :R1 (cl-cc:vm-reg-get s :R0))
    (let ((new-pc (first (multiple-value-list
                          (cl-cc/vm::execute-instruction
                           (cl-cc::make-vm-class-name-fn :dst :R2 :src :R1)
                           s 3 nil)))))
      (assert-= 4 new-pc))))

;;; ─── vm-class-of-fn ──────────────────────────────────────────────────────

(deftest vm-clos-exec-class-of-fn-returns-class-ht
  "vm-class-of-fn returns the :__class__ hash-table from an instance."
  (let ((s (make-clos-vm)))
    (make-test-instance s :R0 :R1 'dog '(name breed))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-class-of-fn :dst :R2 :src :R1)
     s 0 nil)
    (let ((class-ht (cl-cc:vm-reg-get s :R0))
          (result   (cl-cc:vm-reg-get s :R2)))
      (assert-eq class-ht result))))

;;; ─── vm-find-class ───────────────────────────────────────────────────────

(deftest vm-clos-exec-find-class-finds-registered-class
  "vm-find-class retrieves a class registered via vm-class-def."
  (let ((s (make-clos-vm)))
    (exec-class-def s :R0 'cat)
    (cl-cc:vm-reg-set s :R1 'cat)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-find-class :dst :R2 :src :R1)
     s 0 nil)
    (let ((found (cl-cc:vm-reg-get s :R2)))
      (assert-true (hash-table-p found))
      (assert-eq 'cat (gethash :__name__ found)))))

(deftest vm-clos-exec-find-class-returns-nil-for-unknown
  "vm-find-class returns nil for a class name that was never registered."
  (let ((s (make-clos-vm)))
    (cl-cc:vm-reg-set s :R0 'completely-unknown-class-xyz)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-find-class :dst :R1 :src :R0)
     s 0 nil)
    (assert-null (cl-cc:vm-reg-get s :R1))))
