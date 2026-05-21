;;;; Algebraic effect handler runtime (FR-411)

(in-package :cl-cc/runtime)

(defstruct rt-effect
  "A performed effect with a keyword NAME and list PAYLOAD."
  (name nil :type keyword)
  (payload nil :type list))

(defstruct rt-handler-state
  "Dynamic handler frame. HANDLERS is an alist of effect-name to function."
  (handlers nil))

(define-condition rt-effect-condition (error)
  ((effect :initarg :effect :reader rt-effect-condition-effect))
  (:report (lambda (condition stream)
              (format stream "Unhandled runtime effect: ~S"
                      (rt-effect-condition-effect condition)))))

(defvar *rt-handler-stack* nil
  "Dynamically scoped stack of RT-HANDLER-STATE frames.")

(defvar *rt-current-effect* nil
  "Effect currently being dispatched.")

(defun rt-current-handler ()
  "Return the innermost handler for *RT-CURRENT-EFFECT*, if any."
  (when *rt-current-effect*
    (let ((name (rt-effect-name *rt-current-effect*)))
      (dolist (state *rt-handler-stack*)
        (let ((handler (or (cdr (assoc name (rt-handler-state-handlers state)))
                           (cdr (assoc t (rt-handler-state-handlers state))))))
          (when handler
            (return handler)))))))

(defun rt-resume (value)
  "Resume the suspended RT-PERFORM call with VALUE."
  (invoke-restart 'rt-resume value))

(defun rt-perform (effect-name &rest args)
  "Perform EFFECT-NAME with ARGS and return the handler's resumed value."
  (check-type effect-name keyword)
  (let ((effect (make-rt-effect :name effect-name :payload args)))
    (restart-case
        (let* ((*rt-current-effect* effect)
               (handler (rt-current-handler)))
          (if handler
               (funcall handler effect)
               (error 'rt-effect-condition :effect effect)))
      (rt-resume (value)
        :report "Resume the performed runtime effect with a value."
        value))))

(defun rt-handle (handler-fn computation-fn)
  "Run COMPUTATION-FN with HANDLER-FN installed as a catch-all effect handler."
  (let ((*rt-handler-stack*
          (cons (make-rt-handler-state :handlers (list (cons t handler-fn)))
                *rt-handler-stack*)))
    (funcall computation-fn)))

(defmacro rt-with-handler ((name handler-fn) &body body)
  "Install HANDLER-FN for effect NAME while evaluating BODY."
  `(let ((*rt-handler-stack*
           (cons (make-rt-handler-state
                  :handlers (list (cons ,name ,handler-fn)))
                 *rt-handler-stack*)))
     ,@body))

(defun rt-effect-state (operation &optional value)
  "Perform the state effect. OPERATION is typically :GET or :PUT."
  (ecase operation
    (:get (rt-perform :state :get))
    (:put (rt-perform :state :put value))))

(defun rt-effect-error (condition)
  "Perform an error effect carrying CONDITION."
  (rt-perform :error condition))

(defun rt-effect-read (&optional prompt)
  "Perform an input effect with PROMPT."
  (rt-perform :read prompt))

(defun rt-effect-write (value)
  "Perform an output effect carrying VALUE."
  (rt-perform :write value))
