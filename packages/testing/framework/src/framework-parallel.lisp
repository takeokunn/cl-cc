;;;; tests/framework-parallel.lisp — CL-CC Test Framework (Parallel Worker Pool)
;;;; Persistent worker threads with watchdog-based timeout enforcement.

(in-package :cl-cc/test)

;; Defined later in framework-runner.lisp. Declare it here too so SBCL treats
;; the worker-local binding as the intended dynamic special binding during
;; compile-file, even though this file loads first.
(defvar *test-runner-mode*)

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

(defun %run-tests-parallel (tests workers &optional prior-timings)
  "Run tests in parallel using WORKERS persistent worker threads.
A single watchdog thread monitors elapsed time per worker and fires a
sb-ext:timeout interrupt when a test exceeds its timeout — eliminating
the per-test sub-thread, mutex, and waitqueue that were previously created
for every individual test (was O(n-tests) thread objects; now O(workers)).

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
         (watchdog-lock (sb-thread:make-mutex :name "watchdog-lock"))
         (watchdog-stop nil)
         (threads '()))
    (labels ((watchdog ()
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
                               (error 'sb-ext:timeout))))))))))
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
                                  (unwind-protect
                                       ;; Parallel-safe tests have no :depends-on,
                                       ;; so results-so-far (nil) is never consulted.
                                       (handler-case
                                           (%run-single-test test-plist number nil)
                                         (sb-ext:timeout ()
                                           (list :name   (getf test-plist :name)
                                                 :status :fail
                                                 :detail (format nil "  ---~%  message: \"timeout after ~A seconds (watchdog)\"~%  ..."
                                                                 timeout)
                                                 :number number
                                                 :suite  (getf test-plist :suite)
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
                                          (setf (aref watchdog-slots worker-idx) nil))))))))
                         ;; Tag each parallel result with its batch index for TSV output.
                         (setf result (append result (list :batch-id idx)))
                         (sb-thread:with-mutex (results-lock)
                           (setf (aref results idx) result))
                         (%tap-print-result result))))))))
      (let ((watchdog-thread
               (sb-thread:make-thread #'watchdog :name "test-watchdog")))
        (dotimes (i workers)
          (push (sb-thread:make-thread (make-worker i)
                                       :name (format nil "test-worker-~A" i))
                threads))
        (dolist (th (nreverse threads))
          (sb-thread:join-thread th))
        ;; Signal watchdog to stop under lock for cross-thread visibility.
        (sb-thread:with-mutex (watchdog-lock)
          (setf watchdog-stop t))
        (sb-thread:join-thread watchdog-thread)))
    (coerce results 'list)))
