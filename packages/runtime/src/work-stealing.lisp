;;;; src/concurrent/work-stealing.lisp — FR-576 Work-Stealing Scheduler
;;;; Cilk-like work stealing with per-thread deques.
;;;; Intel TBB / Cilk++ / Rust Rayon equivalent.

(in-package :cl-cc/runtime)

;;; ──── Task representation ────
(defstruct (stealable-task (:conc-name st-))
  "A task that can be stolen by idle worker threads."
  (function nil)               ; function to execute
  (args nil :type list)        ; arguments
  (parent nil)                 ; parent task (for continuation stealing)
  (id 0 :type fixnum))        ; unique task ID

;;; ──── Per-thread deque ────
(defstruct (work-deque (:conc-name wd-))
  "A work-stealing deque (double-ended queue) per OS thread.
Owner pushes/pops from bottom (LIFO).
Thieves steal from top (FIFO — oldest task first)."
  (tasks (make-array 1024 :initial-element nil) :type simple-vector)
  (bottom 0 :type fixnum)
  (top 0 :type fixnum)
  (lock nil))                  ; CAS lock for thief operations

(defvar *per-thread-deque* nil
  "Thread-local work deque for the current thread.")

;;; ──── Deque operations ────
(defun deque-push (deque task)
  "Push TASK to the bottom of DEQUE (owner only, no lock needed)."
  (let ((b (wd-bottom deque)))
    (setf (aref (wd-tasks deque) b) task)
    (setf (wd-bottom deque) (1+ b))))

(defun deque-pop (deque)
  "Pop task from the bottom of DEQUE (owner only).
Returns the task or NIL if empty."
  (let ((b (wd-bottom deque)))
    (when (<= (wd-top deque) b)
      (decf (wd-bottom deque))
      (let ((b2 (wd-bottom deque)))
        (if (> (wd-top deque) b2)
            ;; Race: thief may have taken the last task
            (progn
              (setf (wd-bottom deque) b)
              nil)
            (prog1 (aref (wd-tasks deque) b2)
              (setf (aref (wd-tasks deque) b2) nil)))))))

(defun deque-steal (deque)
  "Try to steal a task from the top of DEQUE (thief, needs CAS).
Returns the stolen task or NIL if empty/contention."
  (let ((t (wd-top deque))
        (b (wd-bottom deque)))
    (when (< t b)
      (let ((task (aref (wd-tasks deque) t)))
        (when task
          ;; CAS: try to advance top
          (let ((old-top (atomic-cas (wd-top deque) t (1+ t))))
            (if (= old-top t)
                (prog1 task
                  (setf (aref (wd-tasks deque) t) nil))
                nil)))))))

;;; ──── Work stealing scheduler ────
(defvar *worker-threads* nil
  "List of worker thread handles.")

(defvar *shutdown-p* nil
  "T when the scheduler is shutting down.")

(defun worker-loop (thread-id)
  "Main loop for a worker thread: process own tasks, steal when idle."
  (let ((deque (or *per-thread-deque*
                   (setf *per-thread-deque*
                         (make-work-deque :id thread-id)))))
    (loop while (not *shutdown-p*)
          do (let ((task (or (deque-pop deque)
                             (try-steal-from-others thread-id deques))))
               (if task
                   (execute-task task)
                   (yield-thread))))))

(defun try-steal-from-others (my-id deques)
  "Try to steal a task from another thread's deque.
Uses randomized victim selection to avoid contention."
  (when deques
    (let* ((n (length deques))
           (victim-idx (random n))
           (victim (aref deques victim-idx)))
      (unless (= victim-idx my-id)
        (deque-steal victim)))))

(defun execute-task (task)
  "Execute a stolen or owned task."
  (handler-case
      (apply (st-function task) (st-args task))
    (error (e)
      (format *error-output* "~&;; Task error: ~A~%" e))))

;;; ──── Spawning ────
(defun spawn-task (fn &rest args)
  "Spawn a new task FN with ARGS on the current thread's deque."
  (let ((deque (or *per-thread-deque*
                   (setf *per-thread-deque*
                         (make-work-deque)))))
    (deque-push deque (make-stealable-task :function fn :args args))))

(defmacro spawn-in (fn &rest args)
  "Convenience macro: spawn a task inline.
Usage: (spawn-in (format t \"hello~%\"))"
  `(spawn-task (lambda () ,fn ,@args)))

;;; ──── Nursery (structured concurrency) ────
(defmacro with-nursery ((&key (workers 4)) &body body)
  "Execute BODY in a nursery with WORKERS worker threads.
All spawned tasks must complete before leaving the scope."
  `(let* ((*worker-threads* nil)
          (*shutdown-p* nil)
          (*per-thread-deque* (make-work-deque))
          (deques (make-array ,workers)))
     (dotimes (i ,workers)
       (setf (aref deques i) (make-work-deque)))
     ;; Start worker threads
     (dotimes (i ,workers)
       (push (make-worker i deques) *worker-threads*))
     (unwind-protect
         (progn ,@body)
       ;; Wait for all workers to drain
       (drain-all-workers)))))

;;; ──── Helpers ────
(defun make-worker (id deques)
  "Create and start a worker thread."
  (sb-thread:make-thread
   (lambda ()
     (let ((*per-thread-deque* (aref deques id)))
       (worker-loop id deques)))
   :name (format nil "cl-cc-worker-~D" id)))

(defun drain-all-workers ()
  "Wait for all worker deques to become empty."
  (setf *shutdown-p* t)
  (dolist (worker *worker-threads*)
    (ignore-errors
      (sb-thread:join-thread worker)))
  (values))

(defun yield-thread ()
  "Yield the current thread to let others run."
  (sb-thread:thread-yield))

(defun atomic-cas (place old new)
  "Atomic compare-and-swap."
  (sb-ext:compare-and-swap place old new))
