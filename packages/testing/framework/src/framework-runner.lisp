(in-package :cl-cc/test)

(defun %compute-duration-ns (start-time end-time)
  "Convert internal-time-units difference to integer nanoseconds.
LC_ALL-independent integer arithmetic, never scientific notation."
  (floor (* (- end-time start-time) 1000000000)
         internal-time-units-per-second))

(defun %run-single-test (test-plist number results-so-far)
  "Run one test and return a result plist.
The plist includes :duration-ns — integer nanoseconds measured via
get-internal-real-time. Duration is recorded regardless of terminal status
(pass/fail/skip/pending/timeout) via unwind-protect, and attached to the
result just before we return."
  (let* ((name     (getf test-plist :name))
         (fn       (getf test-plist :fn))
         (timeout  (getf test-plist :timeout))
         (suite    (getf test-plist :suite))
         (start-time (get-internal-real-time))
         (duration-ns 0)
         (result nil))
    ;; Optional hang-diagnosis tracing (opt-in via CLCC_TEST_TRACE=1)
    (when (uiop:getenv "CLCC_TEST_TRACE")
      (format *error-output* "# [trace] running ~A~%" name)
      (force-output *error-output*))
    (unwind-protect
         (setf result
               (block %run-body
                 ;; Check dependency
                 (unless (%check-dependency test-plist results-so-far)
                   (return-from %run-body
                     (list :name name :status :skip :detail "dependency failed"
                           :number number :suite suite)))
                 ;; Run before-each fixtures
                 (multiple-value-bind (before-fns after-fns)
                     (%get-suite-fixtures suite)
                   (handler-case
                       (progn
                         (let ((cl-cc/expand::*macro-environment* (%copy-macro-environment))
                               (cl-cc/expand::*symbol-macro-table*
                                 (%copy-hash-table-shallow cl-cc/expand::*symbol-macro-table*))
                               (cl-cc/expand::*compiler-macro-table*
                                 (%copy-hash-table-shallow cl-cc/expand::*compiler-macro-table*))
                               (cl-cc/expand::*macroexpand-step-cache*
                                 (make-hash-table :test #'eq :weakness :key))
                               (cl-cc/expand::*macroexpand-all-cache*
                                 (make-hash-table :test #'eq :weakness :key)))
                           (dolist (bf before-fns) (funcall bf))
                           (handler-case
                               (handler-bind
                                   ((skip-condition
                                      (lambda (c)
                                        (return-from %run-body
                                          (list :name name :status :skip
                                                :detail (skip-reason c) :number number
                                                :suite suite))))
                                    (pending-condition
                                      (lambda (c)
                                        (return-from %run-body
                                          (list :name name :status :pending
                                                :detail (pending-reason c) :number number
                                                :suite suite)))))
                                 ;; NOTE: sb-ext:with-timeout relies on SIGALRM delivery
                                 ;; which is unreliable inside sb-thread worker threads —
                                 ;; parallel runs used to hang intermittently because the
                                 ;; alarm never fired. Sequential runs still use it below;
                                 ;; parallel runs enforce timeouts via join-thread :timeout
                                 ;; in %run-tests-parallel, so this branch is the
                                 ;; sequential-only fallback and is always safe.
                                 (if (or (eq timeout :none)
                                         (eq *test-runner-mode* :parallel))
                                     (funcall fn)
                                     (sb-ext:with-timeout (or timeout (%default-test-timeout))
                                       (funcall fn)))
                                 ;; Run after-each fixtures
                                 (dolist (af after-fns) (funcall af))
                                 ;; Run invariants
                                 (%run-invariants)
                                 (list :name name :status :pass :detail nil
                                       :number number :suite suite))
                             (test-failure (c)
                               (dolist (af after-fns) (ignore-errors (funcall af)))
                               (list :name name :status :fail
                                     :detail (test-failure-message c) :number number
                                     :suite suite))
                             (sb-ext:timeout ()
                               (dolist (af after-fns) (ignore-errors (funcall af)))
                               (list :name name :status :fail
                                     :detail (format nil "  ---~%  message: \"timeout after ~A seconds\"~%  ..."
                                                     (if (eq timeout :none) "disabled" (or timeout (%default-test-timeout))))
                                     :number number :suite suite))
                             (error (e)
                               (dolist (af after-fns) (ignore-errors (funcall af)))
                               (list :name name :status :fail
                                     :detail (format nil "  ---~%  message: ~S~%  ..."
                                                     (princ-to-string e))
                                     :number number :suite suite)))))
                     (error (e)
                       ;; fixture setup error
                       (list :name name :status :fail
                             :detail (format nil "  ---~%  message: \"fixture error: ~A\"~%  ..."
                                             (princ-to-string e))
                             :number number :suite suite))))))
      ;; Always record wall-clock duration, even on non-local exit.
      (setf duration-ns
            (%compute-duration-ns start-time (get-internal-real-time))))
    ;; Attach duration to whatever plist was produced. If result is NIL
    ;; (e.g. runtime aborted mid-body without establishing RESULT), fall back
    ;; to a minimal errored plist.
    (if result
        (append result (list :duration-ns duration-ns))
        (list :name name :status :fail
              :detail (format nil "  ---~%  message: \"aborted before producing result\"~%  ...")
              :number number :suite suite
              :duration-ns duration-ns))))

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

(defvar *test-runner-mode* :sequential
  "Dynamic indicator of the current runner mode (:sequential or :parallel).
%run-single-test consults this to decide whether sb-ext:with-timeout is safe
to use (sequential) or whether the parallel runner will enforce the timeout
via sb-thread:join-thread :timeout instead.")

(defun %effective-test-timeout (test-plist)
  (let ((tm (getf test-plist :timeout)))
    (cond
      ((eq tm :none) nil)        ; no timeout
      ((and (integerp tm) (plusp tm)) tm)
      (t (%default-test-timeout)))))

(defun %copy-prolog-rules (src)
  "Return a shallow copy of the Prolog fact DB SRC (hash-table).
Per-thread rebinding of `cl-cc/prolog:*prolog-rules*` needs a fresh
table per worker so concurrent add-rule / clear-prolog-database calls
do not race through the global binding."
  (let ((dst (make-hash-table :test 'eq)))
    (when src
      (maphash (lambda (k v) (setf (gethash k dst) v)) src))
    dst))

(defun %load-prior-timings (&optional (path (%timings-output-path)))
  "Return a hash-table mapping test-name-string to duration-ns from a prior TSV run.
Returns nil when the file is absent. On parse errors logs to *error-output* and
returns nil. Keeps the MAX duration per name so :repeat runs stay stable."
  (when (probe-file path)
    (handler-case
        (let ((ht (make-hash-table :test #'equal :size 8192)))
          (with-open-file (s path :direction :input)
            (loop for line = (read-line s nil nil)
                  while line
                  unless (zerop (length line))
                    do (let ((fields '())
                             (start 0))
                         (loop for pos = (position #\Tab line :start start)
                               do (push (subseq line start (or pos (length line))) fields)
                               while pos
                               do (setf start (1+ pos)))
                         (setf fields (nreverse fields))
                         (when (>= (length fields) 3)
                           (let ((name-str (second fields))
                                 (dur (ignore-errors
                                        (parse-integer (third fields)))))
                             (when (and (plusp (length name-str)) dur (plusp dur))
                               ;; Keep max duration per name: stable under :repeat N
                               ;; (which writes N entries per test name to the TSV).
                               (let ((existing (gethash name-str ht 0)))
                                 (when (> dur existing)
                                   (setf (gethash name-str ht) dur)))))))))
          ht)
      (error (e)
        (format *error-output* "# WARN: could not load prior timings from ~A: ~A~%"
                path e)
        nil))))

(defun %sort-parallel-slow-first (tests prior-timings)
  "Return TESTS stable-sorted by descending prior duration.
Tests absent from PRIOR-TIMINGS get 0 (sorted last — treated as fast or new)."
  (stable-sort (copy-list tests) #'>
               :key (lambda (test)
                      (let ((name (getf test :name)))
                        (or (and name (gethash (symbol-name name) prior-timings))
                            0)))))

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

(defun %run-tests-sequential (tests &optional results-so-far)
  "Run tests sequentially and return result plists.
RESULTS-SO-FAR is consulted for dependency checks but is not included in the
returned list."
  (let ((results '()))
    (dolist (test tests)
      (let* ((number (getf test :number))
             (result (%run-single-test test number
                                       (append results-so-far (reverse results)))))
        (push result results)
        (%tap-print-result result)))
    (nreverse results)))

(defun %run-tests-mixed (tests workers &optional prior-timings)
  "Run TESTS with a mixed strategy: parallel-safe tests use the worker pool,
while tests from explicitly serial suites and tests with dependencies run
sequentially in dependency-safe input order."
  (let ((results '())
        (tests (%order-tests-for-dependencies tests))
        (parallel-batch '()))
    (flet ((flush-parallel-batch ()
             (when parallel-batch
               (setf results
                     (append results
                             (%run-tests-parallel (nreverse parallel-batch)
                                                  workers prior-timings)))
               (setf parallel-batch '()))))
      (dolist (test tests)
        (if (%test-parallel-safe-p test)
            (push test parallel-batch)
            (progn
              (flush-parallel-batch)
              (setf results
                    (append results
                            (%run-tests-sequential (list test) results))))))
      (flush-parallel-batch)
      results)))

(defun %detect-cpu-count ()
  "Detect the host CPU count.
Honours CL_CC_TEST_WORKERS (override), then shells out to sysctl (macOS)
/ nproc (Linux). Falls back to 4 when everything else fails so the test
runner still makes forward progress on exotic hosts."
  (or (let ((env (uiop:getenv "CL_CC_TEST_WORKERS")))
        (and env (ignore-errors
                   (let ((n (parse-integer env :junk-allowed t)))
                     (and n (plusp n) n)))))
      (ignore-errors
        (let* ((out (with-output-to-string (s)
                      (uiop:run-program '("sysctl" "-n" "hw.ncpu")
                                        :output s :ignore-error-status t)))
               (n (parse-integer out :junk-allowed t)))
          (and n (plusp n) n)))
      (ignore-errors
        (let* ((out (with-output-to-string (s)
                      (uiop:run-program '("nproc")
                                        :output s :ignore-error-status t)))
               (n (parse-integer out :junk-allowed t)))
          (and n (plusp n) n)))
      4))

(defun %effective-worker-count (ordered-tests parallel workers)
  "Return the effective worker count for ORDERED-TESTS.
Serial runs, serial-only batches, and single-worker requests all collapse to 1.
When WORKERS is NIL, the host CPU count is auto-detected (overridable via
CL_CC_TEST_WORKERS)."
  (let ((w (or workers (%detect-cpu-count))))
    (if (and parallel
             (> w 1)
             (some #'%test-parallel-safe-p ordered-tests))
        w
        1)))

;;; ------------------------------------------------------------
;;; run-suite
;;; ------------------------------------------------------------

(defun run-suite (suite-name &key
                               (parallel t)
                               (random t)
                               (seed nil)
                               (workers nil)
                               (repeat 1)
                               (update-snapshots nil)
                               (tags nil)
                               (exclude-tags nil)
                               (exclude-suites nil)
                               (coverage nil)
                               (warm-stdlib t))
  "Run all tests in suite-name (and children).
   Returns t if all tests passed, nil otherwise."
  ;; Coverage forces sequential
  (when coverage
    (setf parallel nil)
    (format t "# Coverage mode: parallel disabled~%"))

  ;; Seed random state
  (let* ((actual-seed (or seed (random most-positive-fixnum)))
         (*random-state* (sb-ext:seed-random-state actual-seed)))

    ;; Collect tests
    (let* ((tests-plists (%collect-all-suite-tests suite-name tags
                                                    exclude-tags exclude-suites))
           (n (length tests-plists)))
      (when (or exclude-tags exclude-suites)
        (format t "# Excluding tags: ~S~%# Excluding suites: ~S~%"
                exclude-tags exclude-suites))

      ;; Assign numbers in definition order
      (let ((numbered (loop for p in tests-plists
                            for i from 1
                            collect (append p (list :number i)))))

        ;; Shuffle if random
        (let ((test-vec (coerce numbered 'vector)))
          (when random
            (%fisher-yates-shuffle test-vec))
          (let* ((ordered-tests (%order-tests-for-dependencies
                                 (coerce test-vec 'list)))
                 (effective-workers
                   (%effective-worker-count ordered-tests parallel workers)))

            ;; Print TAP header
            (let ((nworkers effective-workers))
              (format t "# Seed: ~A~%" actual-seed)
              (format t "# Workers: ~A~%" nworkers)
              (format t "TAP version 13~%")
              (format t "1..~A~%" (* n repeat))
              (force-output)

               ;; Optionally warm the stdlib AST cache before any tests run.
               ;; This is valuable for broad/heavy runs, but far too expensive
               ;; for the default fast-path targets.
               (when warm-stdlib
                 (ignore-errors (cl-cc::warm-stdlib-cache)))

              ;; Run (with optional repeat for flaky detection)
              (let ((all-run-results '())
                    (prior-timings (%load-prior-timings)))
                (dotimes (r repeat)
                  (when (> repeat 1)
                    (format t "# Run ~A/~A~%" (1+ r) repeat))
                  (let ((run-results
                          (if (and parallel (> nworkers 1))
                              (%run-tests-mixed ordered-tests nworkers prior-timings)
                              (%run-tests-sequential ordered-tests))))
                    (push run-results all-run-results)))

                ;; Flaky detection
                (when (> repeat 1)
                  (%detect-flaky (reverse all-run-results) repeat))

                ;; Reproducibility hint
                (format t "# To reproduce this run: (run-suite '~A :seed ~A)~%"
                        suite-name actual-seed)

                ;; Compute pass/fail
                (let* ((flat-results (apply #'append (reverse all-run-results)))
                       (any-fail (some (lambda (r) (eq (getf r :status) :fail))
                                       flat-results)))

                  ;; Snapshot update mode
                  (when update-snapshots
                    (format t "# Snapshot update mode enabled~%"))

                  ;; Coverage report placeholder
                  (when coverage
                    (format t "# Coverage report enabled~%")
                    (%print-coverage-report flat-results))

                  ;; Print result summary
                  (let* ((pass-count (count :pass flat-results :key (lambda (r) (getf r :status))))
                         (fail-count (count :fail flat-results :key (lambda (r) (getf r :status))))
                         (skip-count (count :skip flat-results :key (lambda (r) (getf r :status))))
                         (total (length flat-results))
                         (bar "#  ---------------------------------------------------"))
                    (format t "#~%")
                    (format t "~A~%" bar)
                    (format t "#  Test Results~%")
                    (format t "~A~%" bar)
                    (format t "#    PASS  ~4D~%" pass-count)
                    (format t "#    FAIL  ~4D~%" fail-count)
                    (when (> skip-count 0)
                      (format t "#    SKIP  ~4D~%" skip-count))
                    (format t "#   -------~%")
                    (format t "#   TOTAL  ~4D~%" total)
                    (format t "~A~%" bar)
                    (when any-fail
                      (format t "#~%")
                      (format t "#  Failed tests:~%")
                      (dolist (r (sort (remove-if-not (lambda (r) (eq (getf r :status) :fail))
                                                      flat-results)
                                       #'< :key (lambda (r) (getf r :number))))
                        (format t "#    [~4D] ~A~%" (getf r :number) (getf r :name)))
                      (format t "~A~%" bar))
                    (format t "#~%"))

                  ;; Emit per-test timings TSV and the top-20 slowest digest.
                  ;; Any failure is logged to *error-output* and swallowed so a
                  ;; filesystem hiccup never flips a passing run into a failure,
                  ;; but we no longer lose the error silently.
                  (handler-case
                      (%write-timings-tsv flat-results (%timings-output-path))
                    (error (e)
                      (format *error-output* "# WARN: test-timings emission failed: ~A~%" e)))
                  (handler-case
                      (%emit-slowest-summary flat-results 20)
                    (error (e)
                      (format *error-output* "# WARN: slowest-summary emission failed: ~A~%" e)))

                  (if any-fail
                      (uiop:quit 1)
                      (uiop:quit 0)))))))))))

(defun run-tests (&key
                    (tags nil)
                    (exclude-tags nil)
                    (exclude-suites nil)
                    (parallel t)
                    (random nil))
  "Run the canonical CL-CC test plan.

This single entry point executes unit, integration, property-based, and e2e suites.
Use the filtering keywords for focused debugging from the REPL, but the public
automation workflow is always `nix run .#test`."
  (run-suite 'cl-cc-suite
             :parallel parallel
             :random random
             :warm-stdlib t
             :tags tags
             :exclude-tags exclude-tags
             :exclude-suites exclude-suites))

(defun %resolve-suite (package-name symbol-name)
  (let* ((pkg (find-package package-name))
         (sym (and pkg (find-symbol symbol-name pkg))))
    (unless sym
      (error "Suite ~A::~A not found (package not loaded?)"
             package-name symbol-name))
    sym))
