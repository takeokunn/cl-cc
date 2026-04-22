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
