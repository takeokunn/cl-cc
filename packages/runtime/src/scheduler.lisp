;;;;; Green Threads Scheduler (FR-257, FR-258)
(in-package :cl-cc/runtime)
(defstruct (rt-green-thread (:constructor %make-rt-green-thread))
  (id 0) (thunk nil) (status :ready) (result nil) (error nil)
  (wake-time nil) (cancelled-p nil) (priority :normal) (context nil))
(defstruct rt-scheduler
  (ready nil) (sleeping nil) (counter 0) (mutex (rt-make-mutex))
  (priority-queues (let ((h (make-hash-table)))
                     (setf (gethash :high h) nil
                           (gethash :normal h) nil
                           (gethash :low h) nil)
                     h))
  (workers nil) (shutdown-p nil))
(defvar *rt-global-scheduler* nil)
(defvar *rt-current-green-thread* nil)
(defun rt-make-scheduler () (make-rt-scheduler))
(defun rt-scheduler-init () (setf *rt-global-scheduler* (rt-make-scheduler)) t)
(defun %rt-scheduler-enqueue (s th)
  (let ((priority (rt-green-thread-priority th)))
    (push th (gethash priority (rt-scheduler-priority-queues s)))
    (push th (rt-scheduler-ready s)))
  th)

(defun %rt-scheduler-dequeue (s)
  (or (loop for p in '(:high :normal :low)
            for q = (gethash p (rt-scheduler-priority-queues s))
            when q
              do (let ((th (pop (gethash p (rt-scheduler-priority-queues s)))))
                   (setf (rt-scheduler-ready s) (remove th (rt-scheduler-ready s) :count 1))
                   (return th)))
      (pop (rt-scheduler-ready s))))

(defun rt-spawn (thunk &key (priority :normal))
  (let* ((s *rt-global-scheduler*)
         (ctx (when (and (boundp '*rt-current-context*) *rt-current-context*
                         (fboundp 'rt-context-copy))
                (funcall (symbol-function 'rt-context-copy) *rt-current-context*)))
         (th (%make-rt-green-thread
               :id (incf (rt-scheduler-counter s))
               :thunk thunk
               :priority priority
               :context ctx)))
    (%rt-scheduler-enqueue s th)
    (let ((group-symbol (find-symbol "*RT-CURRENT-TASK-GROUP*" :cl-cc/runtime)))
      (when (and group-symbol (boundp group-symbol) (symbol-value group-symbol)
                 (fboundp 'rt-task-group-add))
        (funcall (symbol-function 'rt-task-group-add) (symbol-value group-symbol) th)))
    th))
(defun rt-spawn-high (thunk) (rt-spawn thunk :priority :high))
(defun rt-spawn-low (thunk) (rt-spawn thunk :priority :low))
(defun rt-yield ()
  (when *rt-current-green-thread*
    (%rt-scheduler-enqueue *rt-global-scheduler* *rt-current-green-thread*)))
(defun rt-current-thread-id ()
  (when *rt-current-green-thread*
    (rt-green-thread-id *rt-current-green-thread*)))
(defun rt-sleep-task (seconds)
  (when *rt-current-green-thread*
    (setf (rt-green-thread-status *rt-current-green-thread*) :sleeping)
    (setf (rt-green-thread-wake-time *rt-current-green-thread*)
          (+ (get-internal-real-time)
             (floor (* seconds internal-time-units-per-second))))
    (push *rt-current-green-thread* (rt-scheduler-sleeping *rt-global-scheduler*))))
(defun %rt-scheduler-wake-sleepers (s)
  (let ((now (get-internal-real-time)))
    (setf (rt-scheduler-sleeping s)
          (remove-if
           (lambda (th)
             (when (and (rt-green-thread-wake-time th)
                        (<= (rt-green-thread-wake-time th) now))
               (setf (rt-green-thread-wake-time th) nil)
               (setf (rt-green-thread-status th) :ready)
               (%rt-scheduler-enqueue s th)
               t))
           (rt-scheduler-sleeping s)))))

(defun %rt-run-green-thread (th)
  (let ((*rt-current-green-thread* th)
        (*rt-current-context* (or (rt-green-thread-context th)
                                 (and (boundp '*rt-current-context*) *rt-current-context*))))
    (setf (rt-green-thread-status th) :running)
    (handler-case
        (if (rt-green-thread-cancelled-p th)
            (setf (rt-green-thread-status th) :cancelled)
            (progn
              (setf (rt-green-thread-result th)
                    (funcall (rt-green-thread-thunk th)))
              (setf (rt-green-thread-status th) :done)))
      (error (c)
        (setf (rt-green-thread-status th) :failed)
        (setf (rt-green-thread-error th) c)))))

(defun rt-scheduler-run (&key once)
  (let ((s *rt-global-scheduler*))
    (loop
      (%rt-scheduler-wake-sleepers s)
      (let ((th (%rt-scheduler-dequeue s)))
        (when th
          (%rt-run-green-thread th))
        (when once (return th))
        (unless (or th (rt-scheduler-ready s) (rt-scheduler-sleeping s))
          (return nil))))))

(defun rt-scheduler-steal (victim)
  "Steal the lowest-priority available task from VICTIM scheduler."
  (rt-with-mutex ((rt-scheduler-mutex victim))
    (loop for p in '(:low :normal :high)
          for q = (gethash p (rt-scheduler-priority-queues victim))
          when q do (return (pop (gethash p (rt-scheduler-priority-queues victim)))))))

(defstruct rt-thread-pool
  (scheduler (rt-make-scheduler))
  (size 0)
  (threads nil)
  (shutdown-p nil))

(defun rt-make-thread-pool (&key (size 1))
  (make-rt-thread-pool :size size))

(defun rt-thread-pool-submit (pool thunk &key (priority :normal))
  (let* ((s (rt-thread-pool-scheduler pool))
         (th (%make-rt-green-thread :id (incf (rt-scheduler-counter s))
                                    :thunk thunk :priority priority)))
    (rt-with-mutex ((rt-scheduler-mutex s))
      (%rt-scheduler-enqueue s th))
    th))

(defun rt-thread-pool-run (pool)
  (let ((*rt-global-scheduler* (rt-thread-pool-scheduler pool)))
    (rt-scheduler-run)))

(defun rt-thread-pool-start (pool)
  #+sbcl
  (dotimes (i (rt-thread-pool-size pool) pool)
    (push (sb-thread:make-thread
           (lambda ()
             (loop until (rt-thread-pool-shutdown-p pool)
                   do (rt-thread-pool-run pool)
                      (sleep 0.001)))
           :name (format nil "rt-pool-~D" i))
          (rt-thread-pool-threads pool)))
  #-sbcl pool)

(defun rt-thread-pool-shutdown (pool)
  (setf (rt-thread-pool-shutdown-p pool) t)
  pool)
