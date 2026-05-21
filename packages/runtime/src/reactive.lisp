;;;; Reactive Streams / Backpressure (FR-410)
(in-package :cl-cc/runtime)

(defstruct rt-subscription
  "Reactive Streams subscription.

REQUEST-FN is called with a positive integer demand count. CANCEL-FN is called
when the downstream no longer wants items."
  (request-fn (lambda (n) (declare (ignore n)) nil) :type function)
  (cancel-fn (lambda () nil) :type function))

(defstruct rt-subscriber
  "Reactive Streams subscriber callback set.

ON-SUBSCRIBE receives an rt-subscription. ON-NEXT receives one item. ON-ERROR
receives a condition or error object. ON-COMPLETE is called exactly once after
normal completion."
  (on-subscribe (lambda (subscription) (declare (ignore subscription)) nil) :type function)
  (on-next (lambda (item) (declare (ignore item)) nil) :type function)
  (on-error (lambda (error) (declare (ignore error)) nil) :type function)
  (on-complete (lambda () nil) :type function))

(defstruct rt-list-publisher
  "Cold publisher that emits ITEMS in order as demand is requested."
  (items nil :type list))

(defstruct rt-map-publisher
  "Publisher that transforms each upstream item with FN."
  publisher
  (fn #'identity :type function))

(defstruct rt-filter-publisher
  "Publisher that emits only upstream items satisfying PRED."
  publisher
  (pred (lambda (item) (declare (ignore item)) t) :type function))

(defstruct rt-merge-publisher
  "Publisher that merges items from multiple PUBLISHERS under downstream demand."
  (publishers nil :type list))

(defstruct rt-zip-publisher
  "Publisher that pairs items from A and B and combines them with FN."
  a
  b
  (fn #'list :type function))

(defgeneric rt-subscribe (publisher subscriber)
  (:documentation "Subscribe SUBSCRIBER to PUBLISHER and call rt-on-subscribe."))

(defgeneric rt-on-subscribe (subscriber subscription)
  (:documentation "Notify SUBSCRIBER that SUBSCRIPTION is ready for requests."))

(defgeneric rt-on-next (subscriber item)
  (:documentation "Deliver ITEM to SUBSCRIBER if it has not terminated."))

(defgeneric rt-on-error (subscriber error)
  (:documentation "Deliver ERROR to SUBSCRIBER and terminate the stream."))

(defgeneric rt-on-complete (subscriber)
  (:documentation "Notify SUBSCRIBER that the stream completed normally."))

(defmethod rt-on-subscribe ((subscriber rt-subscriber) subscription)
  "Invoke SUBSCRIBER's on-subscribe callback with SUBSCRIPTION."
  (funcall (rt-subscriber-on-subscribe subscriber) subscription))

(defmethod rt-on-next ((subscriber rt-subscriber) item)
  "Invoke SUBSCRIBER's on-next callback with ITEM."
  (funcall (rt-subscriber-on-next subscriber) item))

(defmethod rt-on-error ((subscriber rt-subscriber) error)
  "Invoke SUBSCRIBER's on-error callback with ERROR."
  (funcall (rt-subscriber-on-error subscriber) error))

(defmethod rt-on-complete ((subscriber rt-subscriber))
  "Invoke SUBSCRIBER's on-complete callback."
  (funcall (rt-subscriber-on-complete subscriber)))

(defun rt-request (subscription n)
  "Request N more items from SUBSCRIPTION's upstream publisher.

N must be a positive integer. Invalid demand signals an error through the
subscription request path."
  (funcall (rt-subscription-request-fn subscription) n))

(defun rt-cancel (subscription)
  "Cancel SUBSCRIPTION and stop future item delivery."
  (funcall (rt-subscription-cancel-fn subscription)))

(defun rt-publisher-from-list (items)
  "Create a cold publisher that emits ITEMS in list order with backpressure."
  (make-rt-list-publisher :items (copy-list items)))

(defun rt-publisher-map (publisher fn)
  "Create a publisher that applies FN to every item from PUBLISHER."
  (make-rt-map-publisher :publisher publisher :fn fn))

(defun rt-publisher-filter (publisher pred)
  "Create a publisher that emits only items from PUBLISHER satisfying PRED."
  (make-rt-filter-publisher :publisher publisher :pred pred))

(defun rt-publisher-merge (&rest publishers)
  "Create a publisher that merges all PUBLISHERS into a single stream."
  (make-rt-merge-publisher :publishers publishers))

(defun rt-publisher-zip (a b &key (fn #'list))
  "Create a publisher that zips items from publishers A and B using FN.

FN is called with one item from A and one item from B for every downstream
request. Completion occurs when either upstream completes and no complete pair
can be formed."
  (make-rt-zip-publisher :a a :b b :fn fn))

(defun rt-make-subscriber (&key on-subscribe on-next on-error on-complete)
  "Create a simple callback-based subscriber.

ON-SUBSCRIBE, ON-NEXT, ON-ERROR, and ON-COMPLETE default to no-op callbacks."
  (make-rt-subscriber
   :on-subscribe (or on-subscribe (lambda (subscription) (declare (ignore subscription)) nil))
   :on-next (or on-next (lambda (item) (declare (ignore item)) nil))
   :on-error (or on-error (lambda (error) (declare (ignore error)) nil))
   :on-complete (or on-complete (lambda () nil))))

(defun %rt-positive-demand-p (n)
  "Return true when N is valid positive Reactive Streams demand."
  (and (integerp n) (> n 0)))

(defun %rt-signal-error-once (subscriber done-p error)
  "Signal ERROR to SUBSCRIBER once and mark DONE-P."
  (unless (symbol-value done-p)
    (setf (symbol-value done-p) t)
    (rt-on-error subscriber error)))

(defun %rt-demand-error (n)
  "Create a condition describing invalid demand N."
  (make-condition 'simple-error
                  :format-control "Reactive Streams request must be positive: ~s"
                  :format-arguments (list n)))

(defmethod rt-subscribe ((publisher rt-list-publisher) subscriber)
  "Subscribe SUBSCRIBER to list PUBLISHER with pull-based delivery."
  (let ((remaining (copy-list (rt-list-publisher-items publisher)))
        (done nil)
        (cancelled nil))
    (labels ((complete-if-empty ()
               (when (and (not done) (not cancelled) (null remaining))
                 (setf done t)
                 (rt-on-complete subscriber)))
             (request (n)
               (cond
                 ((or done cancelled) nil)
                 ((not (%rt-positive-demand-p n))
                  (setf cancelled t done t)
                  (rt-on-error subscriber (%rt-demand-error n)))
                 (t
                  (loop while (and (> n 0) remaining (not done) (not cancelled))
                        for item = (pop remaining)
                        do (handler-case
                               (progn
                                 (rt-on-next subscriber item)
                                 (decf n))
                             (error (condition)
                               (setf cancelled t done t)
                               (rt-on-error subscriber condition))))
                  (complete-if-empty))))
             (cancel ()
               (setf cancelled t)
               t))
      (rt-on-subscribe subscriber
                       (make-rt-subscription :request-fn #'request :cancel-fn #'cancel)))))

(defmethod rt-subscribe ((publisher rt-map-publisher) subscriber)
  "Subscribe SUBSCRIBER to map PUBLISHER."
  (let ((upstream nil)
        (done nil))
    (labels ((cancel-upstream ()
               (when upstream (rt-cancel upstream)))
             (fail (error)
               (unless done
                 (setf done t)
                 (cancel-upstream)
                 (rt-on-error subscriber error))))
      (rt-subscribe
       (rt-map-publisher-publisher publisher)
       (rt-make-subscriber
        :on-subscribe (lambda (subscription)
                        (setf upstream subscription)
                        (rt-on-subscribe
                         subscriber
                         (make-rt-subscription
                          :request-fn (lambda (n)
                                        (if (%rt-positive-demand-p n)
                                            (rt-request subscription n)
                                            (fail (%rt-demand-error n))))
                          :cancel-fn (lambda ()
                                       (setf done t)
                                       (rt-cancel subscription)))))
        :on-next (lambda (item)
                   (unless done
                     (handler-case
                         (rt-on-next subscriber (funcall (rt-map-publisher-fn publisher) item))
                       (error (condition) (fail condition)))))
        :on-error (lambda (error) (fail error))
        :on-complete (lambda ()
                       (unless done
                         (setf done t)
                         (rt-on-complete subscriber))))))))

(defmethod rt-subscribe ((publisher rt-filter-publisher) subscriber)
  "Subscribe SUBSCRIBER to filter PUBLISHER."
  (let ((upstream nil)
        (done nil)
        (downstream-demand 0))
    (labels ((cancel-upstream ()
               (when upstream (rt-cancel upstream)))
             (fail (error)
               (unless done
                 (setf done t)
                 (cancel-upstream)
                 (rt-on-error subscriber error)))
             (request-upstream (n)
               (when (and upstream (not done))
                 (rt-request upstream n))))
      (rt-subscribe
       (rt-filter-publisher-publisher publisher)
       (rt-make-subscriber
        :on-subscribe (lambda (subscription)
                        (setf upstream subscription)
                        (rt-on-subscribe
                         subscriber
                         (make-rt-subscription
                          :request-fn (lambda (n)
                                        (cond
                                          ((not (%rt-positive-demand-p n))
                                           (fail (%rt-demand-error n)))
                                          ((not done)
                                           (incf downstream-demand n)
                                           (request-upstream n))))
                          :cancel-fn (lambda ()
                                       (setf done t)
                                       (rt-cancel subscription)))))
        :on-next (lambda (item)
                   (unless done
                     (handler-case
                         (if (funcall (rt-filter-publisher-pred publisher) item)
                             (when (> downstream-demand 0)
                               (decf downstream-demand)
                               (rt-on-next subscriber item))
                             (when (> downstream-demand 0)
                               (request-upstream 1)))
                       (error (condition) (fail condition)))))
        :on-error (lambda (error) (fail error))
        :on-complete (lambda ()
                       (unless done
                         (setf done t)
                         (rt-on-complete subscriber))))))))

(defmethod rt-subscribe ((publisher rt-merge-publisher) subscriber)
  "Subscribe SUBSCRIBER to merge PUBLISHER."
  (let* ((publishers (rt-merge-publisher-publishers publisher))
         (count (length publishers))
         (subscriptions nil)
         (completed 0)
         (next-index 0)
         (done nil))
    (labels ((cancel-all ()
               (dolist (subscription subscriptions) (rt-cancel subscription)))
             (fail (error)
               (unless done
                 (setf done t)
                 (cancel-all)
                 (rt-on-error subscriber error)))
             (request-one (subscription)
               (when (and subscription (not done))
                 (rt-request subscription 1)))
             (request-many (n)
               (cond
                 ((not (%rt-positive-demand-p n))
                  (fail (%rt-demand-error n)))
                 ((and subscriptions (not done))
                  (loop repeat n
                        for subscription = (nth (mod next-index count) subscriptions)
                        do (incf next-index)
                           (request-one subscription))))))
      (if (null publishers)
          (rt-on-subscribe subscriber
                           (make-rt-subscription
                            :request-fn (lambda (n)
                                          (declare (ignore n))
                                          (unless done
                                            (setf done t)
                                            (rt-on-complete subscriber)))
                            :cancel-fn (lambda () (setf done t) t)))
          (progn
            (dolist (source publishers)
              (rt-subscribe
               source
               (rt-make-subscriber
                :on-subscribe (lambda (subscription)
                                (setf subscriptions (append subscriptions (list subscription))))
                :on-next (lambda (item)
                           (unless done
                             (handler-case
                                 (rt-on-next subscriber item)
                               (error (condition) (fail condition)))))
                :on-error (lambda (error) (fail error))
                :on-complete (lambda ()
                               (unless done
                                 (incf completed)
                                 (when (= completed count)
                                   (setf done t)
                                   (rt-on-complete subscriber)))))))
            (rt-on-subscribe subscriber
                             (make-rt-subscription
                              :request-fn #'request-many
                              :cancel-fn (lambda ()
                                           (setf done t)
                                           (cancel-all)))))))))

(defmethod rt-subscribe ((publisher rt-zip-publisher) subscriber)
  "Subscribe SUBSCRIBER to zip PUBLISHER."
  (let ((sub-a nil)
        (sub-b nil)
        (queue-a nil)
        (queue-b nil)
        (completed-a nil)
        (completed-b nil)
        (demand 0)
        (done nil))
    (labels ((cancel-all ()
               (when sub-a (rt-cancel sub-a))
               (when sub-b (rt-cancel sub-b)))
             (fail (error)
               (unless done
                 (setf done t)
                 (cancel-all)
                 (rt-on-error subscriber error)))
             (maybe-complete ()
               (when (and (not done)
                          (or (and completed-a (null queue-a))
                              (and completed-b (null queue-b))))
                 (setf done t)
                 (cancel-all)
                 (rt-on-complete subscriber)))
             (emit ()
               (loop while (and (not done) (> demand 0) queue-a queue-b)
                     for a = (pop queue-a)
                     for b = (pop queue-b)
                     do (decf demand)
                        (handler-case
                            (rt-on-next subscriber (funcall (rt-zip-publisher-fn publisher) a b))
                          (error (condition) (fail condition))))
               (maybe-complete))
             (request-pair (n)
               (cond
                 ((not (%rt-positive-demand-p n))
                  (fail (%rt-demand-error n)))
                 ((not done)
                  (incf demand n)
                  (when sub-a (rt-request sub-a n))
                  (when sub-b (rt-request sub-b n))
                  (emit)))))
      (rt-subscribe
       (rt-zip-publisher-a publisher)
       (rt-make-subscriber
        :on-subscribe (lambda (subscription) (setf sub-a subscription))
        :on-next (lambda (item)
                   (unless done
                     (setf queue-a (append queue-a (list item)))
                     (emit)))
        :on-error (lambda (error) (fail error))
        :on-complete (lambda ()
                       (setf completed-a t)
                       (maybe-complete))))
      (rt-subscribe
       (rt-zip-publisher-b publisher)
       (rt-make-subscriber
        :on-subscribe (lambda (subscription) (setf sub-b subscription))
        :on-next (lambda (item)
                   (unless done
                     (setf queue-b (append queue-b (list item)))
                     (emit)))
        :on-error (lambda (error) (fail error))
        :on-complete (lambda ()
                       (setf completed-b t)
                       (maybe-complete))))
      (rt-on-subscribe subscriber
                       (make-rt-subscription
                        :request-fn #'request-pair
                        :cancel-fn (lambda ()
                                     (setf done t)
                                     (cancel-all)))))))

(defun rt-subscriber-collect (subscriber)
  "Collect SUBSCRIBER items into a list and return a future.

SUBSCRIBER is updated in place so its callbacks still run, while every item is
accumulated. The returned future resolves to the collected list on completion.
On error, the subscription is cancelled and the future resolves to the error
object after forwarding on-error."
  (let ((future (rt-make-future))
        (items nil)
        (subscription nil)
        (done nil)
        (old-on-subscribe (rt-subscriber-on-subscribe subscriber))
        (old-on-next (rt-subscriber-on-next subscriber))
        (old-on-error (rt-subscriber-on-error subscriber))
        (old-on-complete (rt-subscriber-on-complete subscriber)))
    (setf (rt-subscriber-on-subscribe subscriber)
          (lambda (sub)
            (setf subscription sub)
            (funcall old-on-subscribe sub))
          (rt-subscriber-on-next subscriber)
          (lambda (item)
            (unless done
              (setf items (append items (list item)))
              (funcall old-on-next item)))
          (rt-subscriber-on-error subscriber)
          (lambda (error)
            (unless done
              (setf done t)
              (when subscription (rt-cancel subscription))
              (funcall old-on-error error)
              (rt-future-resolve future error)))
          (rt-subscriber-on-complete subscriber)
          (lambda ()
            (unless done
              (setf done t)
              (funcall old-on-complete)
              (rt-future-resolve future items))))
    future))
