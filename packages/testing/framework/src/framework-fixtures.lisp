(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Shared state-copy helpers
;;; ------------------------------------------------------------

(defun %copy-hash-table-shallow (table)
  "Return a shallow copy of TABLE preserving its test function."
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :size (hash-table-count table))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) table)
    copy))

(defun %copy-macro-environment ()
  "Return a fresh macro-env instance populated from the current global macro table."
  (let* ((copy (make-instance 'cl-cc/expand::macro-env))
         (src  (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*))
         (dst  (cl-cc/expand::macro-env-table copy)))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    copy))

(defun %snapshot-hash-table (table &key (copy-value #'identity))
  "Return a fresh snapshot of TABLE, applying COPY-VALUE to every value."
  (let ((snapshot (make-hash-table :test (hash-table-test table)
                                   :size (hash-table-count table))))
    (maphash (lambda (k v)
               (setf (gethash k snapshot) (funcall copy-value v)))
             table)
    snapshot))

(defun %restore-hash-table (target snapshot &key (copy-value #'identity))
  "Replace TARGET contents with SNAPSHOT, applying COPY-VALUE during restore."
  (clrhash target)
  (maphash (lambda (k v)
             (setf (gethash k target) (funcall copy-value v)))
           snapshot)
  target)

;;; ------------------------------------------------------------
;;; Test fixtures
;;; ------------------------------------------------------------

(defmacro with-reset-repl-state (&body body)
  "Run BODY with a clean REPL state, and always restore the REPL to empty."
  `(unwind-protect
       (progn
         (reset-repl-state)
         ,@body)
     (reset-repl-state)))

(defun %snapshot-prolog-db (&key (copy-value #'identity))
  "Snapshot the Prolog rule database using COPY-VALUE for each rule bucket."
  (%snapshot-hash-table cl-cc/prolog::*prolog-rules* :copy-value copy-value))

(defun %restore-prolog-db (snapshot &key (copy-value #'identity))
  "Restore the Prolog rule database from SNAPSHOT using COPY-VALUE per bucket."
  (%restore-hash-table cl-cc/prolog::*prolog-rules* snapshot :copy-value copy-value))

(defmacro with-fresh-prolog (&body body)
  "Run BODY with an empty Prolog rule database and restore the prior state."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (%snapshot-prolog-db)))
       (cl-cc:clear-prolog-database)
       (unwind-protect
           (progn ,@body)
         (cl-cc:clear-prolog-database)
         (%restore-prolog-db ,saved)))))

(defun make-test-vm ()
  "Create a fresh VM state for instruction-level testing."
  (make-instance 'cl-cc:vm-state))

(defmacro with-test-vm ((state &rest bindings) &body body)
  "Create a fresh VM STATE, preload register BINDINGS, then run BODY."
  `(let ((,state (make-test-vm)))
     ,@(mapcar (lambda (binding)
                 (destructuring-bind (register value) binding
                   `(cl-cc:vm-reg-set ,state ,register ,value)))
               bindings)
     ,@body))

(defun vm-exec (inst state &optional (pc 0) (labels (make-hash-table :test #'equal)))
  "Execute one VM instruction and return the next program counter."
  (cl-cc:execute-instruction inst state pc labels))

(defun exec1 (inst state &optional (pc 0))
  "Execute one VM instruction with a fresh label table."
  (vm-exec inst state pc (make-hash-table)))

;;; ------------------------------------------------------------
;;; Invariants
;;; ------------------------------------------------------------

(defmacro definvariant (name &rest args)
  "Register an invariant checked after every test.
   Syntax: (definvariant name [:after-each t] body...)"
  (let ((body args))
    (when (and body (eq (first body) :after-each))
      (setf body (cddr body)))
    `(progn
       (push (cons ',name (lambda () ,@body)) *invariant-registry*)
       ',name)))

(defun %run-invariants ()
  "Run all registered invariants, signaling test-failure with invariant name on violation."
  (dolist (inv *invariant-registry*)
    (handler-case
        (funcall (cdr inv))
      (test-failure (c)
        (error 'test-failure
               :message (format nil "Invariant ~S violated: ~A"
                                (car inv) (test-failure-message c)))))))
