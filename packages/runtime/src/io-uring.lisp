(in-package :cl-cc/runtime)

(defstruct rt-iouring
  (entries 64 :type integer)
  (submission-queue nil)
  (completion-queue nil)
  (closed-p nil))

(defvar *rt-iouring-instance* nil)

(defun rt-iouring-available-p ()
  #+linux t
  #-linux nil)

(defun rt-iouring-init (&key (entries 64))
  (setf *rt-iouring-instance*
        (make-rt-iouring :entries entries :submission-queue nil :completion-queue nil)))

(defun %rt-iouring-submit (op fd buf off len cb)
  (unless *rt-iouring-instance* (rt-iouring-init))
  (let ((request (list :op op :fd fd :buffer buf :offset off :length len :callback cb)))
    (push request (rt-iouring-submission-queue *rt-iouring-instance*))
    request))

(defun rt-iouring-read (fd buf off len cb)
  (%rt-iouring-submit :read fd buf off len cb)
  len)

(defun rt-iouring-write (fd buf off len cb)
  (%rt-iouring-submit :write fd buf off len cb)
  len)

(defun rt-iouring-poll (&key (min-completions 0) timeout)
  "Poll the portable io_uring simulation for completed requests.

This is a timeout-aware portable queue simulation, not a Linux-native
io_uring_enter implementation.  TIMEOUT is expressed in seconds.  When
provided, polling stops once the computed internal-time deadline is exceeded;
a TIMEOUT of 0 checks immediately available simulated completions and returns
without blocking."
  (unless *rt-iouring-instance* (return-from rt-iouring-poll 0))
  (let ((completed 0)
        (deadline (and timeout
                       (+ (get-internal-real-time)
                          (* timeout internal-time-units-per-second)))))
    (labels ((need-more-p ()
               (or (zerop min-completions) (< completed min-completions)))
             (deadline-exceeded-p ()
               (and deadline (>= (get-internal-real-time) deadline)))
             (complete-one ()
               (let* ((req (pop (rt-iouring-submission-queue *rt-iouring-instance*)))
                      (len (getf (cdr req) :length))
                      (cb (getf (cdr req) :callback)))
                 (push (list :request req :result len)
                       (rt-iouring-completion-queue *rt-iouring-instance*))
                 (incf completed)
                 (when cb (funcall cb len)))))
      (loop while (need-more-p)
            do (cond
                 ((rt-iouring-submission-queue *rt-iouring-instance*)
                  (complete-one))
                 ((or (null timeout) (deadline-exceeded-p))
                  (return completed))
                 (t
                  (sleep 0.001))))
      completed)))

(defun rt-iouring-close ()
  (when *rt-iouring-instance*
    (setf (rt-iouring-closed-p *rt-iouring-instance*) t
          *rt-iouring-instance* nil))
  t)
