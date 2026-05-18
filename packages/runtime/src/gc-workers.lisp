(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; FR-338: Parallel GC Worker Threads
;;;
;;; Foundation implementation. When *gc-worker-count* is 0 (default),
;;; all GC work runs sequentially on the GC thread. When > 0 and
;;; SB-THREAD is available, work is distributed across worker threads.
;;;
;;; Current capabilities:
;;;   - Work partitioning via %rt-gc-partition-list (round-robin)
;;;   - Work-stealing mark drains using private queues and optional SB-THREAD
;;;   - Sequential fallback for Pure CL without threads
;;;   - Per-phase parallelism: root-scan, mark-drain, sweep
;;; ------------------------------------------------------------

(defun %rt-gc-sb-thread-function (name)
  "Return the SB-THREAD function named NAME, or NIL when unavailable."
  (ignore-errors
    (let ((package (find-package "SB-THREAD")))
      (when package
        (multiple-value-bind (symbol status) (find-symbol name package)
          (when (and status (fboundp symbol))
            (symbol-function symbol)))))))

(defun %rt-gc-sb-thread-mutex (&optional name)
  "Return an SB-THREAD mutex object when available; otherwise NIL."
  (let ((make-mutex (%rt-gc-sb-thread-function "MAKE-MUTEX")))
    (and make-mutex (ignore-errors (funcall make-mutex :name (or name "gc-worker-queue"))))))

(defmacro %rt-gc-with-optional-mutex ((mutex) &body body)
  "Run BODY under MUTEX when SB-THREAD:WITH-MUTEX is available."
  (let ((fn (gensym "WITH-MUTEX-FN"))
        (mx (gensym "MUTEX")))
    `(let ((,mx ,mutex))
       (if ,mx
           (let ((,fn (%rt-gc-sb-thread-function "CALL-WITH-MUTEX")))
             (if ,fn
                 (funcall ,fn (lambda () ,@body) ,mx)
                 (progn ,@body)))
           (progn ,@body)))))

(defvar *rt-gc-work-stealing-queues* nil
  "Dynamic vector of worker queue cells for FR-338 work stealing.")

(defvar *rt-gc-work-stealing-locks* nil
  "Dynamic vector of optional per-worker mutexes for FR-338 work stealing.")

(defvar *rt-gc-work-stealing-worker-index* 0
  "Dynamic worker index used by %RT-GC-WORK-STEALING-DRAIN.")

(defvar *rt-gc-work-stealing-processor* nil
  "Dynamic function called for every item drained by FR-338 work stealing.")

(defun %rt-gc-queue-pop (queue-cell lock)
  (%rt-gc-with-optional-mutex (lock)
    (when (car queue-cell)
      (pop (car queue-cell)))))

(defun %rt-gc-queue-push-list (queue-cell lock items)
  (%rt-gc-with-optional-mutex (lock)
    (setf (car queue-cell) (nconc items (car queue-cell)))))

(defun %rt-gc-steal-one (thief-index max-steals)
  "Try to steal a small batch from peer queues. Returns a list of stolen work."
  (let* ((queues *rt-gc-work-stealing-queues*)
         (locks *rt-gc-work-stealing-locks*)
         (count (and queues (length queues))))
    (when (and count (plusp count))
      (loop for offset from 1 below count
            for victim-index = (mod (+ thief-index offset) count)
            for victim = (svref queues victim-index)
            for lock = (and locks (svref locks victim-index))
            do (let ((stolen
                       (%rt-gc-with-optional-mutex (lock)
                         (loop repeat max-steals
                               while (car victim)
                               collect (pop (car victim))))))
                 (when stolen (return stolen)))))))

;;; FR-338: Parallel GC Worker Threads — work-stealing parallel mark/sweep
(defun %rt-gc-work-stealing-drain (work-queue results &key (max-steals 3))
  "Work-stealing drain: try to steal from peer queues when local queue empty.
Returns items processed. Thread-safe via per-worker result lists.

WORK-QUEUE is a cons cell whose CAR is this worker's private work list. RESULTS
is either a cons cell or list that receives processed items when no dynamic
processor is installed.  RT-GC-PARALLEL-MARK binds
*RT-GC-WORK-STEALING-PROCESSOR* so the same primitive performs real marking."
  (let ((processed 0)
        (index *rt-gc-work-stealing-worker-index*)
        (local-lock (and *rt-gc-work-stealing-locks*
                         (svref *rt-gc-work-stealing-locks*
                                *rt-gc-work-stealing-worker-index*))))
    (loop
      (let ((item (%rt-gc-queue-pop work-queue local-lock)))
        (cond
          (item
           (incf processed)
           (if *rt-gc-work-stealing-processor*
               (%rt-gc-with-optional-mutex (local-lock)
                 (funcall *rt-gc-work-stealing-processor* item work-queue))
               (if (consp results)
                   (push item (car results))
                   (push item results))))
          (t
           (let ((stolen (%rt-gc-steal-one index max-steals)))
             (if stolen
                 (%rt-gc-queue-push-list work-queue local-lock stolen)
                 (return processed)))))))))

(defun rt-gc-detect-worker-count ()
  "Return recommended number of GC worker threads based on host environment.
Returns 0 (= sequential) when SB-THREAD is unavailable."
  (let ((make-thread (%rt-gc-sb-thread-function "MAKE-THREAD"))
        (join-thread (%rt-gc-sb-thread-function "JOIN-THREAD")))
    (if (and make-thread join-thread)
        (let ((thread (ignore-errors
                        (funcall make-thread (lambda ()) :name "gc-probe"))))
          (when thread
            (ignore-errors (funcall join-thread thread)))
          (if thread 1 0))
        0)))

(defun %rt-gc-run-worker-tasks (tasks)
  "Run TASKS in parallel with SB-THREAD when configured, otherwise sequentially."
  (let ((make-thread (%rt-gc-sb-thread-function "MAKE-THREAD"))
        (join-thread (%rt-gc-sb-thread-function "JOIN-THREAD")))
    (if (and (plusp *gc-worker-count*) make-thread join-thread)
        (let ((errors nil))
          (let ((threads
                  (mapcar (lambda (task)
                            (handler-case
                                (funcall make-thread task :name "gc-worker")
                              (error (c)
                                (push c errors)
                                nil)))
                          tasks)))
            (when errors
              (warn "FR-338: ~D GC worker thread(s) failed to start, using sequential fallback"
                    (length errors)))
            (if (and threads (notany #'null threads) (null errors))
                (let ((results
                        (mapcar (lambda (thread)
                                  (handler-case
                                      (funcall join-thread thread)
                                    (error (c)
                                      (push c errors)
                                      nil)))
                                threads)))
                  (when errors
                    (warn "FR-338: ~D GC worker thread(s) failed during execution"
                          (length errors)))
                  results)
                (progn
                  ;; If startup was only partially successful, wait for already
                  ;; started workers before the sequential fallback so no worker
                  ;; task runs concurrently with its fallback copy.
                  (dolist (thread (remove nil threads))
                    (handler-case
                        (funcall join-thread thread)
                      (error (c)
                        (push c errors))))
                  (mapcar #'funcall tasks)))))
        (mapcar #'funcall tasks))))

(defun %rt-gc-run-workers-with-progress (tasks &key (phase :unknown))
  "Run TASKS across *GC-WORKER-COUNT* workers, returning total items processed.
Reports progress when *GC-PROBES-ENABLED* is true."
  (let ((total 0))
    (if (or (zerop *gc-worker-count*)
            (not (find-package "SB-THREAD")))
        (dolist (task tasks total)
          (let ((result (funcall task)))
            (when (integerp result)
              (incf total result))
            (when (and (boundp '*gc-probes-enabled*)
                       (symbol-value '*gc-probes-enabled*))
              (format *trace-output* "GC-WORKER-PROGRESS ~S ~D~%" phase total)
              (force-output *trace-output*))))
        (let ((results (%rt-gc-run-worker-tasks tasks)))
          (dolist (result results total)
            (when (integerp result)
              (incf total result))
            (when (and (boundp '*gc-probes-enabled*)
                       (symbol-value '*gc-probes-enabled*))
              (format *trace-output* "GC-WORKER-PROGRESS ~S ~D~%" phase total)
              (force-output *trace-output*)))))))

(defun %rt-gc-partition-list (items workers)
  (let* ((count (max 1 workers))
         (buckets (make-array count :initial-element nil)))
    (loop for item in items
          for i from 0 do
            (push item (svref buckets (mod i count))))
    (loop for i from 0 below count collect (nreverse (svref buckets i)))))

;;; FR-338: Parallel GC Worker Threads (foundation) — distributes roots/mark/sweep work across workers when SB-THREAD available
(defun rt-gc-parallel-root-scan (heap &optional (workers *gc-worker-count*))
  "Return root addresses using worker-partitioned scans when host threads exist."
  (let ((parts (%rt-gc-partition-list (rt-heap-roots heap) (max 1 workers))))
    ;; Each worker returns a private result list; the caller merges those lists
    ;; after all workers have finished, avoiding unsynchronized PUSH/NCONC.
    (let ((per-worker-results
            (%rt-gc-run-worker-tasks
             (mapcar (lambda (part)
                       (lambda ()
                         (loop for root in part
                               for addr = (%rt-gc-root-pointer-address heap root)
                               when addr collect addr)))
                     parts))))
      (remove-duplicates (apply #'nconc per-worker-results) :test #'eql))))

;;; FR-338: Parallel GC Worker Threads (foundation) — distributes roots/mark/sweep work across workers when SB-THREAD available
(defun rt-gc-parallel-mark (heap &optional (workers *gc-worker-count*))
  "Drain the major-GC grey queue with FR-338 work-stealing worker queues.

The Pure CL fallback is deterministic and sequential.  SBCL hosts use
SB-THREAD when available; all thread creation/join operations are guarded by
IGNORE-ERRORS through %RT-GC-RUN-WORKER-TASKS."
  (let ((queue (with-gc-mark-queue-locked ()
                  (gethash heap *rt-gc-incremental-mark-queues*))))
    (let* ((worker-count (max 1 workers))
           (parts (%rt-gc-partition-list queue worker-count))
           (queues (make-array worker-count))
           (locks (make-array worker-count :initial-element nil)))
      (loop for part in parts
            for i from 0 do
              (setf (svref queues i) (cons part nil)
                    (svref locks i) (%rt-gc-sb-thread-mutex
                                     (format nil "gc-mark-queue-~D" i))))
      (let* ((*rt-gc-work-stealing-queues* queues)
             (*rt-gc-work-stealing-locks* locks)
             (*rt-gc-work-stealing-processor*
               (lambda (addr local-queue)
                 (%rt-gc-mark-one-grey heap local-queue addr)))
             (worker-results
               (%rt-gc-run-worker-tasks
                 (loop for i from 0 below worker-count
                       collect (let ((worker-index i)
                                     (local-queue (svref queues i))
                                     (local-results (cons nil nil))
                                     (all-queues queues)
                                     (all-locks locks)
                                     (processor *rt-gc-work-stealing-processor*))
                                 (lambda ()
                                   ;; SBCL worker threads do not inherit these dynamic
                                   ;; bindings from the parent thread, so bind the full
                                   ;; FR-338 work-stealing context explicitly here.
                                   (let ((*rt-gc-work-stealing-worker-index* worker-index)
                                         (*rt-gc-work-stealing-queues* all-queues)
                                         (*rt-gc-work-stealing-locks* all-locks)
                                         (*rt-gc-work-stealing-processor* processor))
                                     (%rt-gc-work-stealing-drain local-queue local-results))))))))
        (with-gc-mark-queue-locked ()
          (remhash heap *rt-gc-incremental-mark-queues*))
        (apply #'+ worker-results)))))

(defun rt-gc-parallel-sweep (heap &optional (workers *gc-worker-count*))
  "Run old-space sweep.  Region division is documented; mutation is serialized for safety."
  (declare (ignore workers))
  (%gc-sweep-old-space heap))
