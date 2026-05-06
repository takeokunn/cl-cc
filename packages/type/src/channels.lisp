;;;; channels.lisp — FR-2202 typed channels

(in-package :cl-cc/type)

(defstruct (typed-channel (:constructor %make-typed-channel))
  "Concrete channel carrying values of PAYLOAD-TYPE with optional CAPACITY."
  payload-type
  capacity
  (queue nil :type list)
  (closed-p nil :type boolean))

(defstruct send-channel channel)
(defstruct recv-channel channel)

(defun %runtime-type-designator (type)
  (cond
    ((type-primitive-p type) (type-primitive-name type))
    ((type-advanced-p type) (type-advanced-name type))
    ((type-constructor-p type) (type-constructor-name type))
    ((symbolp type) type)
    (t t)))

(defun %typed-channel-value-matches-p (value type)
  (let ((designator (%runtime-type-designator type)))
    (or (eq designator t)
        (eq designator 'any)
        (ignore-errors (typep value designator)))))

(defun make-typed-channel (payload-type &key capacity)
  "Return (values SEND-END RECV-END) for PAYLOAD-TYPE and optional CAPACITY."
  (when (and capacity (not (and (integerp capacity) (not (minusp capacity)))))
    (error "Channel capacity must be a non-negative integer, got ~S" capacity))
  (let ((channel (%make-typed-channel :payload-type payload-type :capacity capacity)))
    (values (make-send-channel :channel channel)
            (make-recv-channel :channel channel))))

(defun make-buffered-channel (payload-type capacity)
  "Return typed send/receive endpoints for a capacity-indexed buffered channel."
  (make-typed-channel payload-type :capacity capacity))

(defun channel-payload-type (endpoint)
  "Return ENDPOINT's payload type."
  (typed-channel-payload-type
   (etypecase endpoint
     (send-channel (send-channel-channel endpoint))
     (recv-channel (recv-channel-channel endpoint))
     (typed-channel endpoint))))

(defun channel-send (sender value)
  "Send VALUE through SENDER after payload type and capacity checks."
  (unless (send-channel-p sender)
    (error "Expected send-channel endpoint, got ~S" sender))
  (let ((channel (send-channel-channel sender)))
    (when (typed-channel-closed-p channel)
      (error "Cannot send on closed channel"))
    (unless (%typed-channel-value-matches-p value (typed-channel-payload-type channel))
      (error "Value ~S does not match channel payload type ~S"
             value (typed-channel-payload-type channel)))
    (when (and (typed-channel-capacity channel)
               (>= (length (typed-channel-queue channel)) (typed-channel-capacity channel)))
      (error "Buffered channel is full"))
    (setf (typed-channel-queue channel)
          (append (typed-channel-queue channel) (list value)))
    t))

(defun channel-recv (receiver)
  "Receive the next value from RECEIVER, or NIL when empty."
  (unless (recv-channel-p receiver)
    (error "Expected recv-channel endpoint, got ~S" receiver))
  (let* ((channel (recv-channel-channel receiver))
         (queue (typed-channel-queue channel)))
    (when queue
      (prog1 (first queue)
        (setf (typed-channel-queue channel) (rest queue))))))

(defun close-typed-channel (endpoint)
  "Close the underlying typed channel for either endpoint."
  (let ((channel (etypecase endpoint
                   (send-channel (send-channel-channel endpoint))
                   (recv-channel (recv-channel-channel endpoint))
                   (typed-channel endpoint))))
    (setf (typed-channel-closed-p channel) t)
    channel))

(defun make-channel-type (payload-type &key direction capacity)
  "Construct the FR-2202 static channel type for PAYLOAD-TYPE."
  (make-type-advanced :feature-id "FR-2202"
                      :name (ecase (or direction :bidirectional)
                              (:send 'send-channel)
                              (:recv 'recv-channel)
                              (:bidirectional 'channel))
                      :args (list payload-type)
                      :properties (append (list (cons :direction (or direction :bidirectional)))
                                          (when capacity (list (cons :capacity capacity))))))
