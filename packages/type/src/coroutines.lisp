;;;; coroutines.lisp — FR-2205 coroutine and generator types

(in-package :cl-cc/type)

(defstruct (typed-generator (:constructor %make-typed-generator))
  "A generator yielding YIELD-TYPE values and finally returning RETURN-TYPE."
  yield-type
  return-type
  values
  final-value)

(defstruct (typed-coroutine (:constructor %make-typed-coroutine))
  "A bidirectional coroutine with typed send/receive/return channels."
  send-type
  receive-type
  return-type
  handler
  (done-p nil :type boolean))

(defun make-generator (yield-type values &key (return-type type-null) final-value)
  "Construct a typed finite generator from VALUES."
  (dolist (value values)
    (unless (%typed-channel-value-matches-p value yield-type)
      (error "Generator value ~S does not match yield type ~S" value yield-type)))
  (%make-typed-generator :yield-type yield-type
                         :return-type return-type
                         :values (copy-list values)
                         :final-value final-value))

(defun generator-next (generator)
  "Return (values yielded done-p) for GENERATOR."
  (unless (typed-generator-p generator)
    (error "Expected typed generator, got ~S" generator))
  (if (typed-generator-values generator)
      (let ((next (first (typed-generator-values generator))))
        (setf (typed-generator-values generator) (rest (typed-generator-values generator)))
        (values next nil))
      (values (typed-generator-final-value generator) t)))

(defun make-coroutine (send-type receive-type return-type handler)
  "Construct a bidirectional typed coroutine."
  (%make-typed-coroutine :send-type send-type
                         :receive-type receive-type
                         :return-type return-type
                         :handler handler))

(defun coroutine-resume (coroutine value)
  "Resume COROUTINE with VALUE after send-type validation."
  (unless (typed-coroutine-p coroutine)
    (error "Expected typed coroutine, got ~S" coroutine))
  (when (typed-coroutine-done-p coroutine)
    (error "Coroutine already completed"))
  (unless (%typed-channel-value-matches-p value (typed-coroutine-send-type coroutine))
    (error "Coroutine send value ~S does not match ~S"
           value (typed-coroutine-send-type coroutine)))
  (multiple-value-bind (received done-p)
      (funcall (typed-coroutine-handler coroutine) value)
    (setf (typed-coroutine-done-p coroutine) (not (null done-p)))
    (unless (or done-p (%typed-channel-value-matches-p received (typed-coroutine-receive-type coroutine)))
      (error "Coroutine yielded ~S which does not match receive type ~S"
             received (typed-coroutine-receive-type coroutine)))
    (values received done-p)))

(defun make-generator-type (yield-type return-type)
  "Construct the FR-2205 static generator type."
  (make-type-advanced :feature-id "FR-2205"
                      :name 'generator
                      :args (list yield-type return-type)))

(defun make-coroutine-type (send-type receive-type return-type)
  "Construct the FR-2205 static coroutine type."
  (make-type-advanced :feature-id "FR-2205"
                      :name 'coroutine
                      :args (list send-type receive-type return-type)))
