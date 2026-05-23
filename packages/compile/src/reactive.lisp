;;;; packages/compile/src/reactive.lisp — FR-601 Reactive Streams / FRP
;;;; Observable pipelines with backpressure.
;;;; RxJava / Kotlin Flow / Reactor / Akka Streams equivalent.

(in-package :cl-cc/compile)

;;; ──── Observable ────
(defstruct (observable (:conc-name obs-))
  "A reactive stream source: emits values to subscribers."
  (subscribe-fn nil :type function))

(defstruct (subscription (:conc-name sub-))
  "A handle to an active subscription (for cancellation)."
  (observable nil :type observable)
  (on-next nil :type function)
  (on-error nil :type function)
  (on-complete nil :type function)
  (active-p t)
  (cancelled-p nil))

;;; ──── Creation operators ────
(defun observable-from-list (items)
  "Create an observable that emits ITEMS one by one."
  (make-observable
   :subscribe-fn
   (lambda (on-next on-error on-complete)
     (declare (ignore on-error))
     (dolist (item items)
       (funcall on-next item))
     (when on-complete
       (funcall on-complete)))))

(defun observable-from-function (fn)
  "Create an observable that calls FN repeatedly until it returns :complete."
  (make-observable
   :subscribe-fn
   (lambda (on-next on-error on-complete)
     (loop
       (handler-case
           (let ((value (funcall fn)))
             (if (eq value :complete)
                 (return (when on-complete (funcall on-complete)))
                 (funcall on-next value)))
         (error (e)
           (when on-error (funcall on-error e))
           (return)))))))

;;; ──── Transformation operators ────
(defun observable-map (source fn)
  "Transform each value emitted by SOURCE using FN."
  (make-observable
   :subscribe-fn
   (lambda (on-next on-error on-complete)
     (funcall (obs-subscribe-fn source)
              (lambda (v) (funcall on-next (funcall fn v)))
              on-error
              on-complete))))

(defun observable-filter (source pred)
  "Only emit values from SOURCE that satisfy PRED."
  (make-observable
   :subscribe-fn
   (lambda (on-next on-error on-complete)
     (funcall (obs-subscribe-fn source)
              (lambda (v) (when (funcall pred v) (funcall on-next v)))
              on-error
              on-complete))))

(defun observable-reduce (source init fn)
  "Accumulate values from SOURCE using FN, emitting the final result."
  (let ((acc init))
    (make-observable
     :subscribe-fn
     (lambda (on-next on-error on-complete)
       (funcall (obs-subscribe-fn source)
                (lambda (v) (setf acc (funcall fn acc v)))
                on-error
                (lambda ()
                  (funcall on-next acc)
                  (when on-complete (funcall on-complete))))))))

;;; ──── Backpressure operators ────
(defun observable-throttle (source n)
  "Drop values so at most N are emitted per second."
  (declare (ignore source n))
  ;; Simplified: throttle using sleep
  (make-observable
   :subscribe-fn
   (lambda (on-next on-error on-complete)
     (let ((last-time 0))
       (funcall (obs-subscribe-fn source)
                (lambda (v)
                  (let ((now (get-internal-real-time)))
                    (when (> (- now last-time) (/ internal-time-units-per-second n))
                      (setf last-time now)
                      (funcall on-next v))))
                on-error
                on-complete)))))

(defun observable-buffer (source n)
  "Buffer N values from SOURCE, emitting them as a list."
  (let ((buffer nil))
    (make-observable
     :subscribe-fn
     (lambda (on-next on-error on-complete)
       (funcall (obs-subscribe-fn source)
                (lambda (v)
                  (push v buffer)
                  (when (>= (length buffer) n)
                    (funcall on-next (nreverse buffer))
                    (setf buffer nil)))
                on-error
                (lambda ()
                  (when buffer
                    (funcall on-next (nreverse buffer)))
                  (when on-complete (funcall on-complete))))))))

;;; ──── Error handling ────
(defun observable-catch-error (source handler)
  "Handle errors from SOURCE using HANDLER."
  (make-observable
   :subscribe-fn
   (lambda (on-next on-error on-complete)
     (funcall (obs-subscribe-fn source)
              on-next
              (lambda (e)
                (handler-case
                    (funcall handler e on-next)
                  (error (e2)
                    (when on-error (funcall on-error e2)))))
              on-complete))))

;;; ──── Subscription ────
(defun subscribe (observable &key on-next on-error on-complete)
  "Subscribe to OBSERVABLE, returning a subscription handle."
  (let ((sub (make-subscription
              :observable observable
              :on-next on-next
              :on-error on-error
              :on-complete on-complete)))
    (when (obs-subscribe-fn observable)
      (funcall (obs-subscribe-fn observable)
               (or on-next (constantly nil))
               (or on-error #'print)
               (or on-complete (constantly nil))))
    sub))

(defun unsubscribe (sub)
  "Cancel subscription SUB."
  (setf (sub-active-p sub) nil
        (sub-cancelled-p sub) t))

;;; ──── Hot vs Cold Observables ────
(defun cold-observable-p (obs)
  "Return T if OBS is cold (generates values per-subscriber)."
  (functionp (obs-subscribe-fn obs)))

(defun hot-observable-p (obs)
  "Return T if OBS is hot (broadcasts to all subscribers)."
  (not (cold-observable-p obs)))

;;; ──── Pipeline DSL ────
(defmacro observable-pipeline (expr &rest operators)
  "Build an observable pipeline from EXPR with chained OPERATORS.
Usage: (observable-pipeline (from-list '(1 2 3))
          (map #'1+)
          (filter #'evenp)
          (reduce #'+ 0))"
  (let ((source expr))
    (dolist (op operators source)
      (setf source
            (ecase (car op)
              (map `(observable-map ,source ,(second op)))
              (filter `(observable-filter ,source ,(second op)))
              (reduce `(observable-reduce ,source ,(second op) ,(third op)))
              (throttle `(observable-throttle ,source ,(second op)))
              (buffer `(observable-buffer ,source ,(second op)))
              (catch-error `(observable-catch-error ,source ,(second op))))))))
