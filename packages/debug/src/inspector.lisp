(in-package :cl-cc/debug)

(defparameter *inspected-objects* (make-array 0 :adjustable t :fill-pointer 0)
  "History of objects passed to INSPECT.  The returned inspector id is an index into this vector.")

(defun %record-inspected-object (object)
  "Append OBJECT to *INSPECTED-OBJECTS* and return its history id."
  (vector-push-extend object *inspected-objects*))

(defun %safe-princ-to-string (object)
  "Return a bounded, non-readably-required summary for OBJECT."
  (with-output-to-string (stream)
    (let ((*print-length* 8)
          (*print-level* 4)
          (*print-circle* t)
          (*print-readably* nil))
      (prin1 object stream))))

(defun %mop-symbol-function (name)
  "Return an SB-MOP/CLOSER-MOP function named NAME when available."
  (dolist (package-name '("SB-MOP" "CLOSER-MOP"))
    (let* ((package (find-package package-name))
           (symbol (and package (find-symbol name package))))
      (when (and symbol (fboundp symbol))
        (return (symbol-function symbol))))))

(defun %clos-slot-names (object)
  "Best-effort list of CLOS slot names for OBJECT."
  (let ((class-slots (%mop-symbol-function "CLASS-SLOTS"))
        (slot-definition-name (%mop-symbol-function "SLOT-DEFINITION-NAME")))
    (when (and class-slots slot-definition-name)
      (ignore-errors
        (mapcar slot-definition-name (funcall class-slots (class-of object)))))))

(defun %inspect-clos-instance (object)
  "Return inspector parts for a CLOS standard object."
  (loop for slot-name in (%clos-slot-names object)
        collect (list :slot slot-name
                      :boundp (slot-boundp object slot-name)
                      :value (and (slot-boundp object slot-name)
                                  (slot-value object slot-name)))))

(defun %inspect-cons (object)
  "Return inspector parts for a cons cell."
  (list (list :slot 'car :value (car object))
        (list :slot 'cdr :value (cdr object))))

(defun %inspect-hash-table (object)
  "Return inspector parts for hash table keys and values."
  (let ((parts nil))
    (maphash (lambda (key value)
               (push (list :key key :value value) parts))
             object)
    (nreverse parts)))

(defun %inspect-vm-closure (object)
  "Return captured environment details for a CL-CC VM closure object."
  (let ((regs (vm-closure-captured-regs object))
        (vals (vm-closure-captured-vals object)))
    (loop for index below (min (length regs) (length vals))
          collect (list :capture (aref regs index)
                        :value (aref vals index)))))

(defun %inspect-host-function (object)
  "Return best-effort host closure metadata.  Portable CL cannot expose lexical cells."
  (multiple-value-bind (lambda-expression closure-p name)
      (function-lambda-expression object)
    (list (list :name name)
          (list :closure-p closure-p)
          (list :lambda-expression lambda-expression))))

(defun %object-inspector-parts (object)
  "Dispatch OBJECT to the supported FR-688 inspector views."
  (cond
    ((typep object 'vm-closure-object) (%inspect-vm-closure object))
    ((consp object) (%inspect-cons object))
    ((hash-table-p object) (%inspect-hash-table object))
    ((functionp object) (%inspect-host-function object))
    ((typep object 'standard-object) (%inspect-clos-instance object))
    (t nil)))

(defun inspect (object)
  "Inspect OBJECT and return a structured plist.

The result contains :ID, :OBJECT, :TYPE, :SUMMARY, and :PARTS.  Supported
structured parts include CLOS instance slots, cons CAR/CDR, host function closure
metadata, CL-CC VM closure captures, and hash-table key/value entries."
  (let ((id (%record-inspected-object object)))
    (list :id id
          :object object
          :type (type-of object)
          :summary (%safe-princ-to-string object)
          :parts (%object-inspector-parts object))))

;;; ─── FR-314: VM Watchpoints ──────────────────────────────────────────────

(defparameter *vm-watchpoints* (make-hash-table :test #'eql)
  "Watchpoint table.  Maps register designators to T (watched).
When a watched register is written, VM-WATCHPOINT-CONDITION is signaled
from the VM interpreter register-write path.")

(define-condition vm-watchpoint-condition (condition)
  ((reg       :initarg :reg       :reader vm-watchpoint-reg)
   (old-value :initarg :old-value :reader vm-watchpoint-old-value)
   (new-value :initarg :new-value :reader vm-watchpoint-new-value)
   (pc        :initarg :pc        :initform nil :reader vm-watchpoint-pc))
  (:report (lambda (c s)
             (format s "Watchpoint: register ~S changed from ~S to ~S"
                     (vm-watchpoint-reg c)
                     (vm-watchpoint-old-value c)
                     (vm-watchpoint-new-value c)))))

(defun add-vm-watchpoint (register)
  "Add a watchpoint on REGISTER.  Every subsequent write to REGISTER through
VM-REG-SET will signal VM-WATCHPOINT-CONDITION before the write completes."
  (setf (gethash register *vm-watchpoints*) t)
  register)

(defun remove-vm-watchpoint (register)
  "Remove an existing watchpoint on REGISTER."
  (remhash register *vm-watchpoints*)
  register)

(defun clear-vm-watchpoints ()
  "Remove all VM watchpoints."
  (clrhash *vm-watchpoints*))

(defmethod vm-reg-set :around ((state vm-state) reg value)
  "FR-314: Watchpoint hook — signal VM-WATCHPOINT-CONDITION on watched registers.

Provides a CONTINUE restart so that handlers can inspect the write and
let execution proceed."
  (let ((old-value (gethash reg (vm-state-registers state))))
    (if (gethash reg *vm-watchpoints*)
        (let ((result (call-next-method)))
          (restart-case
              (signal 'vm-watchpoint-condition
                      :reg reg
                      :old-value old-value
                      :new-value value)
            (continue ()
              :report "Continue execution after watchpoint"
              nil))
          result)
        (call-next-method))))
