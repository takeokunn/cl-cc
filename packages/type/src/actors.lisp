;;;; actors.lisp — FR-2203 typed actors

(in-package :cl-cc/type)

(defstruct (typed-actor-ref (:constructor %make-typed-actor-ref))
  "A typed actor reference that accepts only MESSAGE-TYPE-compatible messages."
  message-type
  handler
  (state :running)
  (remote-p nil :type boolean)
  (mailbox nil :type list))

(defun make-actor-ref (message-type &key handler remote-p)
  "Construct a local or remote typed actor reference."
  (%make-typed-actor-ref :message-type message-type
                         :handler handler
                         :remote-p (not (null remote-p))))

(defun actor-ref-type (message-type)
  "Construct the FR-2203 static actor-ref type for MESSAGE-TYPE."
  (make-type-advanced :feature-id "FR-2203"
                      :name 'actor-ref
                      :args (list message-type)
                      :properties '((:delivery . typed))))

(defun %message-pattern-matches-p (pattern message)
  (cond
    ((and (consp pattern) (keywordp (first pattern)))
     (and (consp message)
          (eq (first pattern) (first message))
          (= (length pattern) (length message))
          (loop for p in (rest pattern)
                for m in (rest message)
                always (%typed-channel-value-matches-p m p))))
    ((keywordp pattern)
     (or (eq pattern message)
         (and (consp message) (eq pattern (first message)))))
    ((type-advanced-p pattern)
     (eq (type-advanced-name pattern) 'actor-ref))
    ((type-union-p pattern)
     (some (lambda (member) (%message-pattern-matches-p member message))
           (type-union-types pattern)))
    ((listp pattern)
     (some (lambda (member) (%message-pattern-matches-p member message)) pattern))
    (t
     (%typed-channel-value-matches-p message pattern))))

(defun actor-message-accepted-p (actor message)
  "Return T when ACTOR accepts MESSAGE."
  (and (typed-actor-ref-p actor)
       (eq (typed-actor-ref-state actor) :running)
       (%message-pattern-matches-p (typed-actor-ref-message-type actor) message)))

(defun actor-send (actor message)
  "Send MESSAGE to ACTOR after typed protocol validation."
  (unless (typed-actor-ref-p actor)
    (error "Expected typed actor ref, got ~S" actor))
  (unless (actor-message-accepted-p actor message)
    (error "Actor message ~S does not match accepted type ~S"
           message (typed-actor-ref-message-type actor)))
  (push message (typed-actor-ref-mailbox actor))
  (when (typed-actor-ref-handler actor)
    (funcall (typed-actor-ref-handler actor) message))
  t)

(defun actor-stop (actor)
  "Move ACTOR to the terminated typestate."
  (setf (typed-actor-ref-state actor) :terminated)
  actor)
