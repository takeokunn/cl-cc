;;;;; Green Threads Scheduler (FR-257, FR-258)
(in-package :cl-cc/runtime)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bt)
    (defpackage :bt
      (:use :cl)
      (:export #:make-thread #:join-thread #:thread-name #:current-thread
               #:thread-alive-p #:all-threads #:interrupt-thread
               #:destroy-thread #:thread-yield))))

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

(defstruct rt-work-deque
  "Per-worker deque used by the work-stealing scheduler.
Owner pushes/pops at the front; thieves steal from the back.  The mutex keeps
the implementation portable while preserving the CAS-style try/fail API shape."
  (items nil)
  (mutex (rt-make-mutex))
  (owner nil))

(defstruct rt-worker
  (id 0 :type integer)
  (scheduler nil)
  (deque (make-rt-work-deque))
  (thread nil)
  (running-p nil)
  (tasks-executed 0 :type integer)
  (steals 0 :type integer))

(defstruct rt-work-stealing-scheduler
  (workers nil)
  (next-worker 0 :type integer)
  (counter 0 :type integer)
  (mutex (rt-make-mutex))
  (shutdown-p nil)
  (min-workers 1 :type integer)
  (max-workers 1 :type integer))

(defvar *rt-work-stealing-scheduler* nil)
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

(defun rt-work-deque-push-front (deque task)
  (rt-with-mutex ((rt-work-deque-mutex deque))
    (push task (rt-work-deque-items deque))
    task))

(defun rt-work-deque-pop-front (deque)
  (rt-with-mutex ((rt-work-deque-mutex deque))
    (pop (rt-work-deque-items deque))))

(defun rt-work-deque-steal-back (deque)
  "Attempt to steal one task from DEQUE. Returns NIL when empty or contended."
  (rt-with-try-mutex ((rt-work-deque-mutex deque))
    (let ((items (rt-work-deque-items deque)))
      (when items
        (if (endp (cdr items))
            (pop (rt-work-deque-items deque))
            (let* ((prev (last items 2))
                   (task (second prev)))
              (setf (cdr prev) nil)
              task))))))

(defun rt-work-deque-count (deque)
  (rt-with-mutex ((rt-work-deque-mutex deque))
    (length (rt-work-deque-items deque))))

(defun %rt-make-worker (scheduler id)
  (let* ((deque (make-rt-work-deque))
         (worker (make-rt-worker :id id :scheduler scheduler :deque deque)))
    (setf (rt-work-deque-owner deque) worker)
    worker))

(defun rt-make-work-stealing-scheduler (&key workers (min-workers 1) max-workers)
  "Create a work-stealing scheduler with per-worker deques."
  (let* ((detected (or workers 1))
         (max-count (max 1 (or max-workers detected)))
         (min-count (max 1 (min min-workers max-count)))
         (scheduler (make-rt-work-stealing-scheduler
                     :min-workers min-count
                     :max-workers max-count)))
    (dotimes (i (or workers min-count))
      (push (%rt-make-worker scheduler i)
            (rt-work-stealing-scheduler-workers scheduler)))
    (setf (rt-work-stealing-scheduler-workers scheduler)
          (nreverse (rt-work-stealing-scheduler-workers scheduler)))
    scheduler))

(defun rt-work-stealing-init (&key workers min-workers max-workers)
  (setf *rt-work-stealing-scheduler*
        (rt-make-work-stealing-scheduler :workers workers
                                         :min-workers (or min-workers 1)
                                         :max-workers max-workers)))

(defun %rt-ws-workers (scheduler)
  (or (rt-work-stealing-scheduler-workers scheduler)
      (setf (rt-work-stealing-scheduler-workers scheduler)
            (list (%rt-make-worker scheduler 0)))))

(defun %rt-ws-choose-worker (scheduler)
  (let* ((workers (%rt-ws-workers scheduler))
         (count (length workers))
         (index (mod (rt-work-stealing-scheduler-next-worker scheduler) count)))
    (incf (rt-work-stealing-scheduler-next-worker scheduler))
    (nth index workers)))

(defun rt-work-stealing-submit (scheduler thunk &key (priority :normal))
  "Submit THUNK to SCHEDULER. Returns an RT-GREEN-THREAD task object."
  (check-type thunk function)
  (let* ((worker (%rt-ws-choose-worker scheduler))
         (task (%make-rt-green-thread
                :id (incf (rt-work-stealing-scheduler-counter scheduler))
                :thunk thunk
                :priority priority)))
    (rt-work-deque-push-front (rt-worker-deque worker) task)
    task))

(defun %rt-worker-steal (worker)
  (let* ((scheduler (rt-worker-scheduler worker))
         (victims (remove worker (%rt-ws-workers scheduler) :test #'eq)))
    (loop for victim in victims
          for task = (rt-work-deque-steal-back (rt-worker-deque victim))
          when task
            do (incf (rt-worker-steals worker))
               (return task))))

(defun rt-worker-run-once (worker)
  "Run one task from WORKER's own deque, or steal from another worker."
  (let ((task (or (rt-work-deque-pop-front (rt-worker-deque worker))
                  (%rt-worker-steal worker))))
    (when task
      (%rt-run-green-thread task)
      (incf (rt-worker-tasks-executed worker)))
    task))

(defun rt-work-stealing-run (scheduler &key once)
  "Drain SCHEDULER cooperatively on the current OS thread."
  (loop
    with ran = nil
    do (setf ran nil)
       (dolist (worker (%rt-ws-workers scheduler))
         (when (rt-worker-run-once worker)
           (setf ran t)
           (when once (return-from rt-work-stealing-run t))))
       (unless ran (return nil))))

(defun rt-work-stealing-maybe-grow (scheduler)
  "Grow the worker set when all deques contain backlog and capacity remains."
  (rt-with-mutex ((rt-work-stealing-scheduler-mutex scheduler))
    (let ((workers (%rt-ws-workers scheduler)))
      (when (and (< (length workers) (rt-work-stealing-scheduler-max-workers scheduler))
                 (every (lambda (w) (> (rt-work-deque-count (rt-worker-deque w)) 0)) workers))
        (let ((worker (%rt-make-worker scheduler (length workers))))
          (setf (rt-work-stealing-scheduler-workers scheduler)
                (append workers (list worker)))
          worker)))))

(defun rt-scheduler-steal (victim)
  "Steal the lowest-priority available task from VICTIM scheduler."
  (etypecase victim
    (rt-scheduler
     (rt-with-mutex ((rt-scheduler-mutex victim))
       (loop for p in '(:low :normal :high)
             for q = (gethash p (rt-scheduler-priority-queues victim))
             when q do (return (pop (gethash p (rt-scheduler-priority-queues victim)))))))
    (rt-worker
     (rt-work-deque-steal-back (rt-worker-deque victim)))
    (rt-work-stealing-scheduler
     (loop for worker in (%rt-ws-workers victim)
           for task = (rt-work-deque-steal-back (rt-worker-deque worker))
           when task return task))))

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
  (dotimes (i (rt-thread-pool-size pool) pool)
    (push (sb-thread:make-thread
           (lambda ()
             (loop until (rt-thread-pool-shutdown-p pool)
                   do (rt-thread-pool-run pool)
                      (sleep 0.001)))
           :name (format nil "rt-pool-~D" i))
          (rt-thread-pool-threads pool))))

(defun rt-thread-pool-shutdown (pool)
  (setf (rt-thread-pool-shutdown-p pool) t)
  pool)

;;; Native OS threads (FR-1097)

(defstruct rt-native-thread
  "Runtime wrapper around an implementation-native OS thread."
  (host-thread nil)
  (name nil :type (or null string))
  (return-value nil)
  (error nil)
  (state :new)
  (gc-state nil))

(defvar *rt-native-thread-registry* (make-hash-table :test #'eq))

(defun %rt-register-native-thread (wrapper host-thread)
  (setf (rt-native-thread-host-thread wrapper) host-thread
        (gethash host-thread *rt-native-thread-registry*) wrapper)
  wrapper)

(defun %rt-current-native-wrapper ()
  (or (gethash sb-thread:*current-thread* *rt-native-thread-registry*)
      (let* ((name (ignore-errors (sb-thread:thread-name sb-thread:*current-thread*)))
             (wrapper (make-rt-native-thread :host-thread sb-thread:*current-thread*
                                             :name name
                                             :state :running)))
        (%rt-register-native-thread wrapper sb-thread:*current-thread*)
        wrapper)))

(defun rt-make-thread (function &key name)
  "Create an actual native OS thread and return an RT-NATIVE-THREAD wrapper."
  (check-type function function)
  (let ((wrapper (make-rt-native-thread :name name :state :starting)))
    (flet ((run-thread ()
             (let ((*rt-current-green-thread* nil))
               (%rt-register-native-thread wrapper sb-thread:*current-thread*)
               (setf (rt-native-thread-state wrapper) :running
                     (rt-native-thread-gc-state wrapper) :active)
               (rt-gc-register-thread :id sb-thread:*current-thread*
                                      :state (list :id sb-thread:*current-thread*
                                                   :name (or name "rt-native-thread")
                                                   :native-thread wrapper))
               (unwind-protect
                    (handler-case
                        (setf (rt-native-thread-return-value wrapper) (funcall function)
                              (rt-native-thread-state wrapper) :finished)
                      (error (c)
                        (setf (rt-native-thread-error wrapper) c
                              (rt-native-thread-state wrapper) :failed)
                        (error c)))
                 (rt-gc-unregister-thread sb-thread:*current-thread*)))))
      (%rt-register-native-thread
       wrapper
       (sb-thread:make-thread #'run-thread :name (or name "rt-native-thread"))))
    wrapper))

(defun rt-thread-join (thread &optional timeout)
  "Join THREAD and return its function return value."
  (check-type thread rt-native-thread)
  (if timeout
      (sb-thread:join-thread (rt-native-thread-host-thread thread) :timeout timeout :default nil)
      (sb-thread:join-thread (rt-native-thread-host-thread thread)))
  (when (rt-native-thread-error thread)
    (error (rt-native-thread-error thread)))
  (rt-native-thread-return-value thread))

(defun rt-thread-name (thread)
  (etypecase thread
    (rt-native-thread (or (rt-native-thread-name thread)
                          (ignore-errors (sb-thread:thread-name (rt-native-thread-host-thread thread)))))
    (sb-thread:thread (sb-thread:thread-name thread))))

(defun rt-current-thread ()
  (%rt-current-native-wrapper))

(defun rt-thread-alive-p (thread)
  (check-type thread rt-native-thread)
  (sb-thread:thread-alive-p (rt-native-thread-host-thread thread)))

(defun rt-all-threads ()
  (mapcar (lambda (host-thread)
            (or (gethash host-thread *rt-native-thread-registry*)
                (%rt-register-native-thread
                 (make-rt-native-thread :host-thread host-thread
                                        :name (ignore-errors (sb-thread:thread-name host-thread))
                                        :state (if (sb-thread:thread-alive-p host-thread) :running :finished))
                 host-thread)))
          (sb-thread:list-all-threads)))

(defun rt-interrupt-thread (thread function)
  "Interrupt THREAD and run FUNCTION in that thread when supported."
  (check-type thread rt-native-thread)
  (check-type function function)
  (sb-thread:interrupt-thread (rt-native-thread-host-thread thread) function))

(defun rt-destroy-thread (thread)
  "Forcefully terminate THREAD. Discouraged; prefer cooperative cancellation."
  (check-type thread rt-native-thread)
  (setf (rt-native-thread-state thread) :destroyed)
  (sb-thread:terminate-thread (rt-native-thread-host-thread thread)))

(defun rt-thread-yield ()
  "Yield the current native thread's CPU time slice."
  (sb-thread:thread-yield)
  t)

(eval-when (:load-toplevel :execute)
  (setf (fdefinition (intern "MAKE-THREAD" :bt)) #'rt-make-thread
        (fdefinition (intern "JOIN-THREAD" :bt)) #'rt-thread-join
        (fdefinition (intern "THREAD-NAME" :bt)) #'rt-thread-name
        (fdefinition (intern "CURRENT-THREAD" :bt)) #'rt-current-thread
        (fdefinition (intern "THREAD-ALIVE-P" :bt)) #'rt-thread-alive-p
        (fdefinition (intern "ALL-THREADS" :bt)) #'rt-all-threads
        (fdefinition (intern "INTERRUPT-THREAD" :bt)) #'rt-interrupt-thread
        (fdefinition (intern "DESTROY-THREAD" :bt)) #'rt-destroy-thread
        (fdefinition (intern "THREAD-YIELD" :bt)) #'rt-thread-yield))
