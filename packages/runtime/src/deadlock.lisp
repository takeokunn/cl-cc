(in-package :cl-cc/runtime)

;;;; Wait-for graph deadlock detection.
;;;;
;;;; The detector is disabled by default.  When *RT-DL-ENABLED* is true,
;;;; synchronization primitives can call the instrumentation hooks below to
;;;; record which thread owns each lock and which lock each thread is waiting
;;;; to acquire.  RT-DEADLOCK-DETECT builds a wait-for graph on demand and
;;;; searches it for cycles; lock acquisition itself never runs detection.

(defvar *rt-dl-enabled* nil)
(defvar *rt-dl-thread-locks* (make-hash-table :test #'eq))
(defvar *rt-dl-thread-waits* (make-hash-table :test #'eq))

(defun %rt-dl-current-thread ()
  sb-thread:*current-thread*)

(defun %rt-dl-hash-keys (table)
  (let ((keys nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key keys))
             table)
    keys))

(defun %rt-dl-add-held-lock (thread lock)
  (let ((locks (gethash thread *rt-dl-thread-locks*)))
    (unless (member lock locks :test #'eq)
      (setf (gethash thread *rt-dl-thread-locks*) (cons lock locks)))))

(defun %rt-dl-remove-held-lock (thread lock)
  (let ((locks (remove lock (gethash thread *rt-dl-thread-locks*) :test #'eq)))
    (if locks
        (setf (gethash thread *rt-dl-thread-locks*) locks)
        (remhash thread *rt-dl-thread-locks*))))

(defun %rt-dl-lock-owner (lock)
  (let ((owner nil))
    (maphash (lambda (thread locks)
               (when (member lock locks :test #'eq)
                 (setf owner thread)))
             *rt-dl-thread-locks*)
    owner))

(defun %rt-dl-add-edge (graph from to lock)
  (when (and from to (not (eq from to)))
    (let ((edges (gethash from graph)))
      (unless (find to edges :key #'car :test #'eq)
        (setf (gethash from graph) (cons (cons to lock) edges))))))

(defun %rt-dl-build-wait-for-graph ()
  (let ((graph (make-hash-table :test #'eq)))
    (maphash (lambda (waiting-thread lock)
               (let ((owning-thread (%rt-dl-lock-owner lock)))
                 ;; Edge direction follows the project requirement:
                 ;; owning thread -> waiting thread.
                 (%rt-dl-add-edge graph owning-thread waiting-thread lock)))
             *rt-dl-thread-waits*)
    graph))

(defun %rt-dl-cycle-pairs (path repeated-thread)
  (let ((cycle nil))
    (loop for pairs on path
          for pair = (car pairs)
          do (push pair cycle)
          until (eq (car pair) repeated-thread))
    (nreverse cycle)))

(defun %rt-dl-dfs-cycle (thread graph visited active path)
  (setf (gethash thread visited) t
        (gethash thread active) t)
  (let ((cycle nil))
    (dolist (edge (gethash thread graph) cycle)
      (let ((next-thread (car edge))
            (lock (cdr edge)))
        (cond
          ((gethash next-thread active)
           (setf cycle (%rt-dl-cycle-pairs (cons (cons thread lock) path)
                                           next-thread))
           (return cycle))
          ((not (gethash next-thread visited))
           (setf cycle (%rt-dl-dfs-cycle next-thread graph visited active
                                         (cons (cons thread lock) path)))
           (when cycle (return cycle))))))
    (remhash thread active)
    cycle))

(defun rt-deadlock-before-lock (lock &optional (thread (%rt-dl-current-thread)))
  "Record that THREAD is about to wait for LOCK when deadlock tracking is enabled."
  (when *rt-dl-enabled*
    (setf (gethash thread *rt-dl-thread-waits*) lock))
  t)

(defun rt-deadlock-after-lock (lock &optional (thread (%rt-dl-current-thread)) (acquired-p t))
  "Record THREAD's successful acquisition of LOCK and clear its wait state."
  (when *rt-dl-enabled*
    (remhash thread *rt-dl-thread-waits*)
    (when acquired-p
      (%rt-dl-add-held-lock thread lock)))
  t)

(defun rt-deadlock-after-unlock (lock &optional (thread (%rt-dl-current-thread)))
  "Record THREAD's release of LOCK when deadlock tracking is enabled."
  (when *rt-dl-enabled*
    (%rt-dl-remove-held-lock thread lock))
  t)

(defun rt-deadlock-detect (&optional starting)
  "Return NIL if no wait-for graph cycle exists, or thread-lock pairs for one cycle.

Each returned pair is (THREAD . LOCK), meaning THREAD owns LOCK and the next
thread in the returned cycle is waiting for that lock.  STARTING, when supplied,
limits the initial DFS root to that thread."
  (when *rt-dl-enabled*
    (let ((graph (%rt-dl-build-wait-for-graph))
          (visited (make-hash-table :test #'eq))
          (active (make-hash-table :test #'eq)))
      (if starting
          (%rt-dl-dfs-cycle starting graph visited active nil)
          (let ((cycle nil))
            (dolist (thread (%rt-dl-hash-keys graph) cycle)
              (unless (gethash thread visited)
                (setf cycle (%rt-dl-dfs-cycle thread graph visited active nil))
                (when cycle (return cycle)))))))))

(defun rt-deadlock-init ()
  "Reset deadlock tracking state and leave detection disabled by default."
  (setf *rt-dl-enabled* nil)
  (clrhash *rt-dl-thread-locks*)
  (clrhash *rt-dl-thread-waits*)
  t)
