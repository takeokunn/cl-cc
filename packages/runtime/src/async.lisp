(in-package :cl-cc/runtime)

;;;; Async/Await runtime surface (FR-741)
;;;;
;;;; The compiler can lower ASYNC forms to the CPS helpers below.  At runtime
;;;; async tasks are represented as futures and scheduled through the green
;;;; scheduler, with a synchronous fallback when no scheduler is initialized.

(defvar *rt-async-event-loop* nil)

(defun %rt-async-ensure-scheduler ()
  (unless *rt-global-scheduler*
    (rt-scheduler-init))
  *rt-global-scheduler*)

(defun %rt-resolve-future-from-thunk (future thunk)
  (handler-case
      (multiple-value-call #'rt-future-resolve future (funcall thunk))
    (error (c)
      ;; Preserve the existing future representation by resolving to the
      ;; condition as a value.  Existing callers already treat futures as value
      ;; containers, and this avoids introducing a breaking error slot.
      (rt-future-resolve future c))))

(defun rt-async-submit (thunk &key scheduler)
  "Schedule THUNK as an async task and return a future for its values."
  (check-type thunk function)
  (let ((future (rt-make-future))
        (sched (or scheduler (%rt-async-ensure-scheduler))))
    (let ((*rt-global-scheduler* sched))
      (rt-spawn (lambda () (%rt-resolve-future-from-thunk future thunk))))
    future))

(defmacro rt-async (&body body)
  "Evaluate BODY asynchronously and return a future."
  `(rt-async-submit (lambda () ,@body)))

(defun rt-await* (future &key timeout)
  "Await FUTURE, yielding the current green thread/fiber while possible."
  (loop until (rt-future-done-p future)
        do (progn
             (when (and (boundp '*rt-current-fiber*)
                        (symbol-value '*rt-current-fiber*)
                        (fboundp 'rt-fiber-yield))
               (rt-fiber-yield))
             (when *rt-current-green-thread* (rt-yield))
             (when *rt-global-scheduler* (rt-scheduler-run :once t))
             (sleep 0.0005)))
  (rt-future-await future :timeout timeout))

(defmacro rt-await (future-form &key timeout)
  `(rt-await* ,future-form :timeout ,timeout))

(defmacro rt-async-lambda (lambda-list &body body)
  "Return a function that starts BODY asynchronously when called."
  `(lambda ,lambda-list
     (rt-async ,@body)))

(defmacro rt-async-defun (name lambda-list &body body)
  "Define NAME as an async function returning a future."
  `(defun ,name ,lambda-list
     (rt-async ,@body)))

(defun %rt-await-form-p (form)
  (and (consp form) (eq (car form) 'rt-await)))

(defun rt-async-cps-transform (form continuation)
  "Transform a small async expression FORM into continuation-passing style.
This helper is intentionally conservative: it handles atoms, PROGN, LET, IF,
and RT-AWAIT forms, and leaves other function calls in direct style after their
arguments have been transformed by the compiler front-end."
  (cond
    ((%rt-await-form-p form)
     `(rt-future-then ,(second form) ,continuation))
    ((atom form)
     `(funcall ,continuation ,form))
    ((eq (car form) 'progn)
     (let ((forms (cdr form)))
       (if (endp forms)
           `(funcall ,continuation nil)
           (labels ((chain (remaining)
                      (if (endp (cdr remaining))
                          (rt-async-cps-transform (car remaining) continuation)
                          (rt-async-cps-transform
                           (car remaining)
                           `(lambda (&rest ignored)
                              (declare (ignore ignored))
                              ,(chain (cdr remaining)))))))
             (chain forms)))))
    ((eq (car form) 'if)
     (destructuring-bind (test then &optional else) (cdr form)
       `(if ,test
            ,(rt-async-cps-transform then continuation)
            ,(rt-async-cps-transform else continuation))))
    ((eq (car form) 'let)
     (destructuring-bind (bindings &body body) (cdr form)
       `(let ,bindings
          ,(rt-async-cps-transform `(progn ,@body) continuation))))
    (t
     `(funcall ,continuation ,form))))

(defmacro rt-async-cps (&body body)
  "Build a future by executing BODY through the runtime CPS transformer."
  (let ((future (gensym "FUTURE")))
    `(let ((,future (rt-make-future)))
       ,(rt-async-cps-transform
         `(progn ,@body)
         `(lambda (&rest values)
            (apply #'rt-future-resolve ,future values)))
       ,future)))

(defun rt-async-channel (&key (capacity 0))
  "Create a channel intended for async task communication."
  (rt-make-channel :capacity capacity))

(defun rt-async-send (channel value)
  "Asynchronously send VALUE to CHANNEL, returning a future."
  (rt-async (rt-channel-send channel value)))

(defun rt-async-recv (channel)
  "Asynchronously receive from CHANNEL, returning a future."
  (rt-async (rt-channel-recv channel)))
