;;;; tests/framework-parallel.lisp — CL-CC Test Framework (Parallel Worker Pool)
;;;; Persistent worker threads with watchdog-based timeout enforcement.

(in-package :cl-cc/test)

;; Defined later in framework-runner.lisp. Declare it here too so SBCL treats
;; the worker-local binding as the intended dynamic special binding during
;; compile-file, even though this file loads first.
(defvar *test-runner-mode*)

(defparameter *heartbeat-interval-seconds*
  (or (let ((raw (uiop:getenv "CLCC_HEARTBEAT_SECONDS")))
        (and raw
             (ignore-errors
               (let ((parsed (parse-integer raw)))
                 (and (plusp parsed) parsed)))))
      30)
  "Seconds between heartbeat emits. CLCC_HEARTBEAT_SECONDS env override; 0 disables.")

(defparameter *kill-grace-seconds* 5
  "Seconds to wait after firing a per-test timeout interrupt before escalating
to sb-ext:exit. Workers that don't respond to interrupt-thread within this
grace window are considered hung and the parent process is killed.")

(defparameter *watchdog-exit-fn*
  (lambda (&rest args &key code &allow-other-keys)
    (declare (ignore args))
    (sb-ext:exit :code code :abort t))
  "Indirection for the hard-kill action so tests can rebind to a recorder.
Default uses sb-ext:exit with :abort t for IMMEDIATE process termination
without unwinding stuck worker threads. The :abort flag is critical: a
plain (sb-ext:exit :code N) does an orderly shutdown that joins all threads,
which would block forever on the very threads the watchdog is trying to
kill. The lambda accepts &allow-other-keys for forward-compatibility with
future watchdog signal arguments. Tests rebind to a lambda that records
the code without killing.")

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

(defun %run-tests-parallel (tests workers &optional prior-timings)
  "Run tests in parallel using WORKERS persistent worker threads.
A single watchdog thread monitors elapsed time per worker and fires a
sb-ext:timeout interrupt when a test exceeds its timeout (O(workers) threads).

Correctness invariants:
 - Epoch counter: each worker-slot registration captures worker-epochs[i] at
   that moment. The interrupt lambda checks the current epoch before signalling,
   making stale interrupts (fired after the test already completed) self-defusing.
 - Interrupt outside lock: timed-out workers are collected inside watchdog-lock,
   then interrupted OUTSIDE it so a slow interrupt delivery cannot block all
   worker threads from registering their next test.
 - sb-sys:without-interrupts around cleanup: prevents the watchdog interrupt from
   landing inside the unwind-protect cleanup path where a second non-local exit
   would leak watchdog-lock."
  (let* ((tests (if prior-timings
                    (%sort-parallel-slow-first tests prior-timings)
                    tests))
         (n (length tests))
         (results (make-array n :initial-element nil))
         (results-lock (sb-thread:make-mutex :name "results-lock"))
         (work-queue (coerce tests 'vector))
         (queue-index 0)
         (queue-lock (sb-thread:make-mutex :name "queue-lock"))
         (prolog-snapshot cl-cc/prolog:*prolog-rules*)
         ;; watchdog-slots[i] = (thread epoch start-iticks timeout-secs) or nil.
         (watchdog-slots (make-array workers :initial-element nil))
         ;; worker-epochs[i] is incremented in the unwind-protect cleanup when a
         ;; test ends. The interrupt lambda captures the epoch at registration and
         ;; checks it before signalling, defusing stale interrupts.
         (worker-epochs (make-array workers :initial-element 0))
         (current-tests (make-array workers :initial-element nil))
         (watchdog-lock (sb-thread:make-mutex :name "watchdog-lock"))
         (watchdog-stop nil)
         (threads '()))
    (labels ((watchdog ()
               (let ((escalations '()))
                 (loop
                   ;; Read stop flag under lock for cross-thread visibility.
                   (when (sb-thread:with-mutex (watchdog-lock) watchdog-stop) (return))
                   (sleep 0.5)
                   (let ((now (get-internal-real-time))
                         (to-interrupt '()))
                     ;; Collect timed-out workers inside the lock, interrupt outside.
                     (sb-thread:with-mutex (watchdog-lock)
                       (dotimes (i workers)
                         (let ((slot (aref watchdog-slots i)))
                           (when slot
                             (let* ((thread      (first slot))
                                    (epoch       (second slot))
                                    (start-time  (third slot))
                                    (timeout-sec (fourth slot))
                                    (elapsed (/ (- now start-time)
                                                (float internal-time-units-per-second))))
                               (when (> elapsed timeout-sec)
                                 (setf (aref watchdog-slots i) nil)
                                 (push (list thread epoch i) to-interrupt)))))))
                     ;; Fire interrupts outside the lock to avoid blocking worker
                     ;; threads that need watchdog-lock to register their next test.
                     (dolist (item to-interrupt)
                       (let ((thread     (first item))
                             (epoch      (second item))
                             (worker-idx (third item)))
                         (ignore-errors
                           (sb-thread:interrupt-thread thread
                             (lambda ()
                               ;; Only signal if the worker is still in the same test.
                               ;; worker-epochs[i] is incremented during cleanup before
                               ;; the slot is cleared, so any mismatch means the test
                               ;; already finished and this interrupt is stale.
                               (when (= (aref worker-epochs worker-idx) epoch)
                                 (error 'sb-ext:timeout)))))
                         ;; Record escalation deadline: if worker doesn't acknowledge
                         ;; the interrupt within *kill-grace-seconds*, the watchdog
                         ;; escalates by calling *watchdog-exit-fn* with code 124.
                         (push (list worker-idx epoch
                                     (+ now (* *kill-grace-seconds*
                                               internal-time-units-per-second)))
                               escalations))))
                   ;; Check escalations: any entry whose worker hasn't advanced its
                   ;; epoch and whose deadline has passed triggers a hard exit.
                   (let ((now (get-internal-real-time)))
                     (dolist (esc escalations)
                       (let ((worker-idx (first esc))
                             (epoch (second esc))
                             (deadline (third esc)))
                         (when (and (>= now deadline)
                                    (= (aref worker-epochs worker-idx) epoch))
                           (format *error-output*
                                   "# FATAL: worker ~A hung past timeout+~As; exiting 124~%"
                                   worker-idx *kill-grace-seconds*)
                           (finish-output *error-output*)
                           (funcall *watchdog-exit-fn* :code 124))))
                     ;; Prune resolved (epoch advanced) or already-actioned escalations.
                     (setf escalations
                           (remove-if (lambda (esc)
                                        (or (>= now (third esc))
                                            (/= (aref worker-epochs (first esc))
                                                (second esc))))
                                      escalations))))))
             (heartbeat ()
               (let ((start (get-internal-real-time))
                     (interval *heartbeat-interval-seconds*))
                 (when (and interval (plusp interval))
                   (loop
                     (when (sb-thread:with-mutex (watchdog-lock) watchdog-stop) (return))
                     (sleep interval)
                     (when (sb-thread:with-mutex (watchdog-lock) watchdog-stop) (return))
                     (let* ((elapsed-secs (round (/ (- (get-internal-real-time) start)
                                                    (float internal-time-units-per-second))))
                            (done (sb-thread:with-mutex (results-lock)
                                    (count-if-not #'null results)))
                            ;; Snapshot the slot pointers under the lock; iterate
                            ;; them outside so the heartbeat does not hold
                            ;; watchdog-lock while consing the inflight list.
                            ;; Plists in current-tests are immutable once written,
                            ;; so reading them after release is safe.
                            (snapshot
                              (sb-thread:with-mutex (watchdog-lock)
                                (copy-seq current-tests)))
                            (inflight-names
                              (loop for slot across snapshot
                                    when slot
                                      collect (getf slot :name))))
                       (format *error-output*
                               "# heartbeat: t=~As done=~A/~A workers=~A inflight=~A~%"
                               elapsed-secs done n workers inflight-names)
                       (finish-output *error-output*))))))
              (make-worker (worker-idx)
                ;; Each worker captures its own index and runs tests directly
                ;; (no per-test sub-thread). SBCL worker threads do not inherit
                ;; parent dynamic bindings, so *test-runner-mode* and
                ;; *prolog-rules* are explicitly rebound here.
                (lambda ()
                  (let ((*test-runner-mode* :parallel)
                        (cl-cc/prolog:*prolog-rules*
                          (%copy-prolog-rules prolog-snapshot)))
                    (loop
                      (let ((task nil) (idx nil))
                       (sb-thread:with-mutex (queue-lock)
                         (when (< queue-index n)
                           (setf idx queue-index
                                 task (aref work-queue queue-index))
                           (incf queue-index)))
                       (unless task (return))
                       (let* ((test-plist task)
                              (number     (getf test-plist :number))
                              (timeout    (%effective-test-timeout test-plist))
                              (start-time (get-internal-real-time))
                              (result
                                (progn
                                  (when timeout
                                    ;; Capture epoch BEFORE registering in slot.
                                    ;; Epoch is incremented at cleanup time so any
                                    ;; interrupt fired after the test ends sees a
                                    ;; stale epoch and becomes a no-op.
                                    (let ((epoch (aref worker-epochs worker-idx)))
                                      (sb-thread:with-mutex (watchdog-lock)
                                        (setf (aref watchdog-slots worker-idx)
                                              (list sb-thread:*current-thread*
                                                    epoch
                                                    start-time
                                                    timeout)))))
                                  ;; Track in-flight test for heartbeat regardless
                                  ;; of whether the test has a timeout.
                                  (sb-thread:with-mutex (watchdog-lock)
                                    (setf (aref current-tests worker-idx) test-plist))
                                   (unwind-protect
                                        ;; Parallel-safe tests have no :depends-on,
                                        ;; so results-so-far (nil) is never consulted.
                                        (handler-case
                                            (%run-single-test test-plist number nil)
                                          (sb-ext:timeout ()
                                            (list :name        (getf test-plist :name)
                                                  :status      :fail
                                                  :detail      (format nil "  ---~%  message: \"timeout after ~A seconds (watchdog)\"~%  ..."
                                                                       timeout)
                                                  :number      number
                                                  :suite       (getf test-plist :suite)
                                                  :source-file (getf test-plist :source-file)
                                                  :duration-ns (%compute-duration-ns
                                                                start-time
                                                                (get-internal-real-time))))
                                          (condition (e)
                                            ;; Keep worker threads alive when tests
                                            ;; intentionally override uiop:quit to
                                            ;; signal custom conditions (e.g. FAKE-QUIT).
                                            ;; Model as a normal test failure instead of
                                            ;; aborting the whole parallel suite.
                                            (list :name        (getf test-plist :name)
                                                  :status      :fail
                                                  :detail      (format nil "  ---~%  message: ~S~%  ..." e)
                                                  :number      number
                                                  :suite       (getf test-plist :suite)
                                                  :source-file (getf test-plist :source-file)
                                                  :duration-ns (%compute-duration-ns
                                                                start-time
                                                                (get-internal-real-time)))))
                                     ;; Increment epoch first (invalidates the captured
                                     ;; epoch in any pending interrupt lambda), then
                                    ;; clear the slot. sb-sys:without-interrupts
                                    ;; prevents a watchdog interrupt from landing
                                    ;; inside this cleanup path.
                                    (sb-sys:without-interrupts
                                      (when timeout
                                        (incf (aref worker-epochs worker-idx))
                                        (sb-thread:with-mutex (watchdog-lock)
                                          (setf (aref watchdog-slots worker-idx) nil)))
                                      (sb-thread:with-mutex (watchdog-lock)
                                        (setf (aref current-tests worker-idx) nil)))))))
                         ;; Tag each parallel result with its batch index for TSV output.
                         (setf result (append result (list :batch-id idx)))
                         (sb-thread:with-mutex (results-lock)
                           (setf (aref results idx) result))
                         (%tap-print-result result))))))))
      (let ((watchdog-thread
               (sb-thread:make-thread #'watchdog :name "test-watchdog"))
            (heartbeat-thread
               (sb-thread:make-thread #'heartbeat :name "test-heartbeat")))
        (dotimes (i workers)
          (push (sb-thread:make-thread (make-worker i)
                                       :name (format nil "test-worker-~A" i))
                threads))
        (dolist (th (nreverse threads))
          (sb-thread:join-thread th))
        ;; Signal watchdog and heartbeat to stop under lock for cross-thread visibility.
        (sb-thread:with-mutex (watchdog-lock)
          (setf watchdog-stop t))
        (sb-thread:join-thread watchdog-thread)
        (sb-thread:join-thread heartbeat-thread)))
    (coerce results 'list)))
