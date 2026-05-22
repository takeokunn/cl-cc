;;;; packages/vm/src/vm-tier.lisp — tiering, value profiling, TLS dispatch

(in-package :cl-cc/vm)

(defvar *vm-state* nil
  "Thread-local dynamically bound VM state used by lock-free dispatch helpers.")

(defvar *vm-tier-call-counters* (make-hash-table :test #'equal)
  "Function/key -> call count table for tiered compilation policy.")

(defvar *vm-value-profile* (make-hash-table :test #'equal)
  "Site -> observed value/type frequency table for value profiling.")

(defun vm-tier-note-call (key &optional (delta 1))
  "Increment KEY's tiering call counter and return the new count."
  (incf (gethash key *vm-tier-call-counters* 0) delta))

(defun vm-tier-hot-p (key &optional (threshold *tier-upgrade-threshold*))
  "Return true when KEY's call counter reached THRESHOLD."
  (>= (gethash key *vm-tier-call-counters* 0) threshold))

(defun vm-tier-reset-counters ()
  "Clear tiering counters."
  (clrhash *vm-tier-call-counters*)
  t)

(defun vm-record-value-profile (site value &key (classifier #'type-of))
  "Record VALUE at profiling SITE and return the updated class count."
  (let* ((class (funcall classifier value))
         (bucket (or (gethash site *vm-value-profile*)
                     (setf (gethash site *vm-value-profile*)
                           (make-hash-table :test #'equal)))))
    (incf (gethash class bucket 0))))

(defun vm-value-profile-snapshot ()
  "Return a serializable snapshot of value profiling counters."
  (let (rows)
    (maphash (lambda (site bucket)
               (let (entries)
                 (maphash (lambda (class count) (push (cons class count) entries)) bucket)
                 (push (cons site (nreverse entries)) rows)))
             *vm-value-profile*)
    (nreverse rows)))

(defmacro with-vm-state-dispatch ((state) &body body)
  "Bind STATE as TLS-like *VM-STATE* for lock-free VM dispatch paths."
  `(let ((*vm-state* ,state))
     ,@body))

(defun vm-current-state ()
  "Return the currently bound VM state, or NIL."
  *vm-state*)

(defun vm-dispatch-lock-free (thunk &optional (state *vm-state*))
  "Run THUNK with STATE dynamically visible and without global dispatch locks."
  (with-vm-state-dispatch (state)
    (funcall thunk)))
