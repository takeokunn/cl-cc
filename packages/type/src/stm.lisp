;;;; stm.lisp — FR-2204 STM types

(in-package :cl-cc/type)

(defvar *stm-transaction-active* nil
  "Dynamically true while atomically executes an STM action.")

(defstruct (tvar (:constructor %make-tvar))
  "A transactional variable with an explicit value type."
  type
  value)

(defstruct (stm-action (:constructor %make-stm-action))
  "A pure transactional action producing RESULT-TYPE."
  result-type
  thunk
  (effects nil :type list))

(defun make-tvar (type initial-value)
  "Construct a TVar after checking INITIAL-VALUE against TYPE."
  (unless (%typed-channel-value-matches-p initial-value type)
    (error "Initial value ~S does not match TVar type ~S" initial-value type))
  (%make-tvar :type type :value initial-value))

(defun stm-return (value &optional type)
  "Lift VALUE into the STM monad."
  (%make-stm-action :result-type (or type type-any)
                    :thunk (lambda () value)))

(defun stm-read (tvar)
  "Return an STM action that reads TVAR."
  (unless (tvar-p tvar)
    (error "Expected TVar, got ~S" tvar))
  (%make-stm-action :result-type (tvar-type tvar)
                    :thunk (lambda () (tvar-value tvar))))

(defun stm-write (tvar value)
  "Return an STM action that writes VALUE to TVAR."
  (unless (tvar-p tvar)
    (error "Expected TVar, got ~S" tvar))
  (unless (%typed-channel-value-matches-p value (tvar-type tvar))
    (error "Value ~S does not match TVar type ~S" value (tvar-type tvar)))
  (%make-stm-action :result-type type-unit
                    :thunk (lambda () (setf (tvar-value tvar) value) value)))

(defun stm-bind (action function)
  "Sequence ACTION and feed its value to FUNCTION, preserving STM purity." 
  (unless (stm-action-p action)
    (error "Expected STM action, got ~S" action))
  (%make-stm-action :result-type type-any
                    :effects (stm-action-effects action)
                    :thunk (lambda ()
                             (let ((next (funcall function (funcall (stm-action-thunk action)))))
                               (if (stm-action-p next)
                                   (funcall (stm-action-thunk next))
                                   next)))))

(defun atomically (action)
  "Execute a pure STM ACTION and return its value. IO effects are rejected."
  (unless (stm-action-p action)
    (error "atomically expects an STM action, got ~S" action))
  (when (member :io (stm-action-effects action) :test #'eq)
    (error "IO effects are forbidden inside STM"))
  (let ((*stm-transaction-active* t))
    (funcall (stm-action-thunk action))))

(defun make-stm-type (result-type)
  "Construct the FR-2204 static STM monad type."
  (make-type-advanced :feature-id "FR-2204"
                      :name 'stm
                      :args (list result-type)
                      :properties '((:effects . transaction-only))))
