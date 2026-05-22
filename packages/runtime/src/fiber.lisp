(in-package :cl-cc/runtime)
(defstruct rt-fiber
  (id 0) (thunk nil) (status :ready) (result nil) (error nil)
  (locals (make-hash-table :test #'eq)) (yielded-value nil)
  (continuation nil)
  (stack nil)
  (future nil)
  (scheduler nil))
(defvar *rt-fiber-counter* 0)
(defvar *rt-current-fiber* nil)
(defvar *rt-fiber-ready* nil)
(defun rt-make-fiber (thunk &key future scheduler)
  "Create a lightweight cooperative fiber.
STACK is represented as an explicit continuation stack; no OS thread stack is
required, which keeps fibers portable and suitable for M:N scheduling."
  (make-rt-fiber :id (incf *rt-fiber-counter*)
                 :thunk thunk
                 :future future
                 :scheduler scheduler))
(defun rt-fiber-schedule (fiber)
  (setf (rt-fiber-status fiber) :ready)
  (push fiber *rt-fiber-ready*)
  fiber)
(defun rt-fiber-spawn (thunk &key future scheduler)
  (rt-fiber-schedule (rt-make-fiber thunk :future future :scheduler scheduler)))

(defun rt-fiber-set-continuation (fiber continuation)
  "Install CONTINUATION as FIBER's next resumable step."
  (check-type fiber rt-fiber)
  (check-type continuation function)
  (setf (rt-fiber-continuation fiber) continuation
        (rt-fiber-status fiber) :suspended)
  fiber)

(defun rt-fiber-push-frame (fiber frame)
  (push frame (rt-fiber-stack fiber))
  frame)

(defun rt-fiber-pop-frame (fiber)
  (pop (rt-fiber-stack fiber)))
(defun rt-fiber-resume (f)
  (when (member (rt-fiber-status f) '(:done :failed :cancelled))
    (return-from rt-fiber-resume (rt-fiber-result f)))
  (setf (rt-fiber-status f) :running)
  (handler-case
      (let ((*rt-current-fiber* f))
        (let ((step (or (rt-fiber-continuation f) (rt-fiber-thunk f))))
          (setf (rt-fiber-continuation f) nil)
          (setf (rt-fiber-result f) (funcall step))
          (unless (member (rt-fiber-status f) '(:ready :suspended :blocked))
            (setf (rt-fiber-status f) :done))
          (when (and (eq (rt-fiber-status f) :done) (rt-fiber-future f))
            (rt-future-resolve (rt-fiber-future f) (rt-fiber-result f)))
          (rt-fiber-result f)))
    (error (c)
      (setf (rt-fiber-error f) c
             (rt-fiber-status f) :failed)
      (when (rt-fiber-future f)
        (rt-future-resolve (rt-fiber-future f) c))
      nil)))
(defun rt-fiber-done-p (f) (eq (rt-fiber-status f) :done))
(defun rt-fiber-status-failed-p (f) (eq (rt-fiber-status f) :failed))
(defun rt-fiber-yield (&optional value)
  (when *rt-current-fiber*
    (setf (rt-fiber-yielded-value *rt-current-fiber*) value
          (rt-fiber-status *rt-current-fiber*) :ready)
    (rt-fiber-schedule *rt-current-fiber*))
  value)
(defun rt-fiber-block (continuation &optional value)
  "Suspend the current fiber and resume it later with CONTINUATION."
  (unless *rt-current-fiber* (return-from rt-fiber-block value))
  (rt-fiber-set-continuation *rt-current-fiber* continuation)
  (setf (rt-fiber-yielded-value *rt-current-fiber*) value)
  (rt-fiber-schedule *rt-current-fiber*)
  value)

(defun rt-fiber-await (future)
  "Await FUTURE from a fiber-friendly path."
  (if (rt-future-done-p future)
      (rt-future-await future)
      (progn
        (rt-fiber-yield future)
        (rt-await future))))
(defun rt-fiber-local (key &optional default)
  (if *rt-current-fiber*
      (gethash key (rt-fiber-locals *rt-current-fiber*) default)
      default))
(defun (setf rt-fiber-local) (value key &optional default)
  (declare (ignore default))
  (unless *rt-current-fiber* (error "no current fiber"))
  (setf (gethash key (rt-fiber-locals *rt-current-fiber*)) value))
(defun rt-run-fibers (&key once)
  (loop for f = (pop *rt-fiber-ready*)
        while f
        do (rt-fiber-resume f)
        when once return f))

(defun rt-fiber-async (thunk)
  "Spawn THUNK as a fiber and return a future for its result."
  (let ((future (rt-make-future)))
    (rt-fiber-spawn thunk :future future)
    future))

(defun rt-green-thread-spawn (thunk &key (scheduler *rt-work-stealing-scheduler*))
  "Spawn THUNK on the work-stealing scheduler when available, else global scheduler."
  (cond
    (scheduler (rt-work-stealing-submit scheduler thunk))
    (*rt-global-scheduler* (rt-spawn thunk))
    (t (rt-fiber-spawn thunk))))

(defun rt-run-green-threads (&key once)
  "Run available green-thread work across the M:N scheduler and fiber queue."
  (cond
    (*rt-work-stealing-scheduler* (rt-work-stealing-run *rt-work-stealing-scheduler* :once once))
    (*rt-global-scheduler* (rt-scheduler-run :once once))
    (t (rt-run-fibers :once once))))
