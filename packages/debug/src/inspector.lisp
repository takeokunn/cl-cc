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
