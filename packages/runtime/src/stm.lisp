(in-package :cl-cc/runtime)

;;;; Software Transactional Memory (FR-740)
;;;;
;;;; This is an optimistic STM for the runtime package.  Transactions keep a
;;;; read log of TVar versions and a write log of deferred updates.  Commit
;;;; validates the read set while holding write locks in deterministic order;
;;;; if any observed version changed, the transaction is retried automatically.

(defstruct (rt-tvar (:constructor %make-rt-tvar))
  (value nil)
  (version 0 :type integer)
  (id 0 :type integer)
  (lock (rt-make-mutex))
  (waiters (rt-make-condition-variable)))

(defstruct rt-stm-transaction
  (read-log nil)
  (write-log nil)
  (retry-tvars nil)
  (attempt 0 :type integer))

(define-condition rt-stm-conflict (error)
  ((tvar :initarg :tvar :reader rt-stm-conflict-tvar))
  (:report (lambda (c s)
             (format s "STM conflict~@[ on TVar ~A~]" (rt-stm-conflict-tvar c)))))

(define-condition rt-stm-retry (condition) ())

(defvar *rt-stm-current-transaction* nil)
(defvar *rt-stm-read-log* nil)
(defvar *rt-stm-write-log* nil)
(defvar *rt-stm-clock* 0)
(defvar *rt-stm-tvar-counter* 0)
(defparameter *rt-stm-max-backoff* 0.01)

(defun rt-make-tvar (&optional value)
  "Create a transactional variable initialized to VALUE."
  (%make-rt-tvar :value value
                 :version (incf *rt-stm-clock*)
                 :id (incf *rt-stm-tvar-counter*)))

(defun %rt-stm-read-log ()
  (if *rt-stm-current-transaction*
      (rt-stm-transaction-read-log *rt-stm-current-transaction*)
      *rt-stm-read-log*))

(defun (setf %rt-stm-read-log) (value)
  (if *rt-stm-current-transaction*
      (setf (rt-stm-transaction-read-log *rt-stm-current-transaction*) value)
      (setf *rt-stm-read-log* value)))

(defun %rt-stm-write-log ()
  (if *rt-stm-current-transaction*
      (rt-stm-transaction-write-log *rt-stm-current-transaction*)
      *rt-stm-write-log*))

(defun (setf %rt-stm-write-log) (value)
  (if *rt-stm-current-transaction*
      (setf (rt-stm-transaction-write-log *rt-stm-current-transaction*) value)
      (setf *rt-stm-write-log* value)))

(defun %rt-stm-record-retry-tvar (tv)
  (when *rt-stm-current-transaction*
    (pushnew tv (rt-stm-transaction-retry-tvars *rt-stm-current-transaction*) :test #'eq)))

(defun rt-read-tvar (tv)
  "Read TV from the current transaction, recording its version for validation."
  (check-type tv rt-tvar)
  (let ((pending (assoc tv (%rt-stm-write-log) :test #'eq)))
    (if pending
        (cdr pending)
        (let ((value nil) (version nil))
          (rt-with-mutex ((rt-tvar-lock tv))
            (setf value (rt-tvar-value tv)
                  version (rt-tvar-version tv)))
          (pushnew (cons tv version) (%rt-stm-read-log) :key #'car :test #'eq)
          (%rt-stm-record-retry-tvar tv)
          value))))

(defun rt-write-tvar (tv value)
  "Stage VALUE as TV's new value in the current transaction."
  (check-type tv rt-tvar)
  (let ((cell (assoc tv (%rt-stm-write-log) :test #'eq)))
    (if cell
        (setf (cdr cell) value)
        (push (cons tv value) (%rt-stm-write-log))))
  value)

(defun %rt-stm-conflict-p ()
  (some (lambda (entry)
          (let ((tv (car entry))
                (version (cdr entry)))
            (rt-with-mutex ((rt-tvar-lock tv))
              (when (/= version (rt-tvar-version tv)) tv))))
        (%rt-stm-read-log)))

(defun %rt-stm-write-tvars ()
  (sort (copy-list (mapcar #'car (%rt-stm-write-log))) #'< :key #'rt-tvar-id))

(defun %rt-stm-validate-locked ()
  (dolist (entry (%rt-stm-read-log) t)
    (let ((tv (car entry))
          (version (cdr entry)))
      (unless (= version (rt-tvar-version tv))
        (error 'rt-stm-conflict :tvar tv)))))

(defun %rt-stm-commit ()
  "Validate and publish the current transaction's writes."
  (let ((write-tvars (%rt-stm-write-tvars)))
    (labels ((commit-with-locks (remaining)
               (if (endp remaining)
                   (progn
                     (%rt-stm-validate-locked)
                     (dolist (entry (%rt-stm-write-log))
                       (let ((tv (car entry))
                             (value (cdr entry)))
                         (setf (rt-tvar-value tv) value
                               (rt-tvar-version tv) (incf *rt-stm-clock*))
                         (rt-condition-notify-all (rt-tvar-waiters tv))))
                     t)
                   (let ((tv (car remaining)))
                     (rt-with-mutex ((rt-tvar-lock tv))
                       (commit-with-locks (cdr remaining)))))))
      (commit-with-locks write-tvars))))

(defun %rt-stm-wait-for-retry (transaction attempt)
  (declare (ignore transaction))
  ;; Portable fallback: short exponential backoff.  TVars still broadcast on
  ;; commit, so host-thread integrations can later replace this with condition
  ;; waits without changing the public API.
  (sleep (min *rt-stm-max-backoff* (/ (expt 2 (min attempt 8)) 1000000.0))))

(defmacro rt-atomically (&body body)
  "Run BODY as an atomic STM transaction with automatic retry on conflicts."
  (let ((tx (gensym "TX"))
        (attempt (gensym "ATTEMPT"))
        (result (gensym "RESULT")))
    `(loop for ,attempt from 0
           do (let* ((,tx (make-rt-stm-transaction :attempt ,attempt))
                     (*rt-stm-current-transaction* ,tx)
                     (*rt-stm-read-log* nil)
                     (*rt-stm-write-log* nil))
                (handler-case
                    (let ((,result (progn ,@body)))
                      (%rt-stm-commit)
                      (return ,result))
                  (rt-stm-conflict ()
                    (%rt-stm-wait-for-retry ,tx ,attempt))
                  (rt-stm-retry ()
                    (%rt-stm-wait-for-retry ,tx ,attempt)))))))

(defmacro atomic (&body body)
  "Alias for RT-ATOMICALLY."
  `(rt-atomically ,@body))

(defun rt-retry ()
  "Abort the current transaction and retry it later."
  (signal 'rt-stm-retry))

(defun rt-tvar-value-unsafe (tv)
  "Read TV's raw value outside STM. Intended for diagnostics/tests only."
  (check-type tv rt-tvar)
  (rt-with-mutex ((rt-tvar-lock tv))
    (rt-tvar-value tv)))

(defun rt-tvar-version-unsafe (tv)
  "Read TV's raw version outside STM. Intended for diagnostics/tests only."
  (check-type tv rt-tvar)
  (rt-with-mutex ((rt-tvar-lock tv))
    (rt-tvar-version tv)))

(defun opt-pass-stm (form)
  "Runtime registration hook for STM-aware compiler pipelines.
Currently preserves FORM; compiler passes can recognize RT-ATOMICALLY/ATOMIC."
  form)
