;;; runtime-stdlib-3-expander.lisp — FR-935 proclamation recording

(in-package :cl-cc/expand)

(defun %global-proclamation-bucket (kind)
  "Return KIND's proclamation table, creating it on first use."
  (or (gethash kind *global-proclamations*)
      (setf (gethash kind *global-proclamations*) (make-hash-table :test #'eq))))

(defun %record-global-proclamation (kind name value)
  "Record proclamation KIND for NAME with VALUE in *GLOBAL-PROCLAMATIONS*."
  (when (symbolp name)
    (setf (gethash name (%global-proclamation-bucket kind)) value)))

(defun %record-declaim-type-clause (clause)
  "Record (TYPE type-spec var*) proclamations."
  (when (and (consp clause) (eq (car clause) 'type) (cddr clause))
    (let ((type-spec (second clause)))
      (dolist (name (cddr clause))
        (%record-global-proclamation 'type name type-spec)))))

(defun %record-declaim-ftype-clause (clause)
  "Record (FTYPE function-type name*) proclamations."
  (when (and (consp clause) (eq (car clause) 'ftype) (cddr clause))
    (let ((type-spec (second clause)))
      (dolist (name (cddr clause))
        (%record-global-proclamation 'ftype name type-spec)))))

(defun %record-declaim-special-clause (clause)
  "Record (SPECIAL var*) proclamations."
  (when (and (consp clause) (eq (car clause) 'special))
    (dolist (name (cdr clause))
      (%record-global-proclamation 'special name t))))

(defun %record-runtime-stdlib-3-declaim-clause (clause)
  "Record every DECLAIM clause whose side effects are tracked by the expander."
  (%record-declaim-inline-clause clause)
  (%record-declaim-optimize-clause clause)
  (%record-declaim-type-clause clause)
  (%record-declaim-ftype-clause clause)
  (%record-declaim-special-clause clause))

(register-macro 'declaim
  (lambda (form env)
    (declare (ignore env))
    (dolist (clause (cdr form))
      (%record-runtime-stdlib-3-declaim-clause clause))
    nil))

(export '(*global-proclamations*))
