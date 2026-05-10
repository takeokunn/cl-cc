;;;; framework-parallel-runner.lisp — Parallel worker pool, CPU detection, run-suite, run-tests
(in-package :cl-cc/test)

(defparameter *cpu-detect-command-timeout-seconds* 2
  "Timeout in seconds for external CPU-count detection commands.")

(defparameter *suite-killer-exit-fn*
  (lambda (&rest args &key code &allow-other-keys)
    (declare (ignore args))
    (sb-ext:exit :code code :abort t))
  "Indirection for the suite-deadline-killer exit action so tests can rebind to a recorder.
Default uses sb-ext:exit :code code :abort t.  The :abort t flag calls _exit(2) directly,
bypassing the SBCL thread join that plain (sb-ext:exit) would perform; without :abort t a
GC-safepoint-blocked main thread cannot be terminated by the killer.
&allow-other-keys lets callers pass forward-compatible keyword arguments without error (the
same reason *watchdog-exit-fn* in framework-parallel.lisp uses it).
Mirrors *watchdog-exit-fn* in framework-parallel.lisp.")

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

(defun %copy-prolog-rules (src)
  "Return a shallow copy of the Prolog fact DB SRC (hash-table).
Per-thread rebinding of `cl-cc/prolog:*prolog-rules*` needs a fresh
table per worker so concurrent add-rule / clear-prolog-database calls
do not race through the global binding."
  (let ((dst (make-hash-table :test 'eq)))
    (when src
      (maphash (lambda (k v) (setf (gethash k dst) v)) src))
    dst))

(defun %sort-parallel-slow-first (tests prior-timings)
  "Return TESTS stable-sorted by descending prior duration.
Tests absent from PRIOR-TIMINGS get 0 (sorted last — treated as fast or new)."
  (stable-sort (copy-list tests) #'>
               :key (lambda (test)
                      (let ((name (getf test :name)))
                        (or (and name (gethash (symbol-name name) prior-timings))
                            0)))))

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
              (let ((*test-runner-mode* :parallel))
                (setf results
                      (append results
                              (%run-tests-sequential (list test) results)))))))
      (flush-parallel-batch)
      results)))

(defun %default-run-command-output (cmd)
  "Run CMD and return its captured output as a string, bounded by
*cpu-detect-command-timeout-seconds* to prevent hung subprocesses."
  (with-output-to-string (stream)
    (sb-ext:with-timeout *cpu-detect-command-timeout-seconds*
      (uiop:run-program cmd :output stream :ignore-error-status t))))

(defparameter *run-command-output-fn* #'%default-run-command-output
  "Function of one argument CMD used by `%run-command-output'.")

(defun %run-command-output (cmd)
  "Run CMD through `*run-command-output-fn*' and return its captured output."
  (funcall *run-command-output-fn* cmd))

(defun %parse-command-cpu-count (cmd)
  "Run CMD and parse its output as a positive integer, or NIL."
  (ignore-errors
    (let* ((output (sb-ext:with-timeout *cpu-detect-command-timeout-seconds*
                     (%run-command-output cmd)))
           (n (parse-integer output :junk-allowed t)))
      (and n (plusp n) n))))

;;; CPU detection sources tried left-to-right; first positive integer wins.
(defparameter *cpu-count-sources*
  (list
   ;; 1. Environment override (e.g. CI matrix, Nix sandbox)
   (lambda ()
     (let ((env (uiop:getenv "CL_CC_TEST_WORKERS")))
       (and env (ignore-errors
                  (let ((n (parse-integer env :junk-allowed t)))
                    (and n (plusp n) n))))))
   ;; 2. macOS sysctl
   (lambda () (%parse-command-cpu-count '("sysctl" "-n" "hw.ncpu")))
   ;; 3. Linux nproc
   (lambda () (%parse-command-cpu-count '("nproc"))))
  "Ordered list of CPU-count detection thunks; first truthy result wins.")

(defun %detect-cpu-count ()
  "Detect host CPU count using *cpu-count-sources*. Falls back to 4."
  (or (some #'funcall *cpu-count-sources*) 4))

(defun %default-suite-timeout ()
  "Return the default whole-suite timeout in seconds, or NIL when disabled.
Uses CLCC_SUITE_TIMEOUT when it is a positive integer; otherwise falls back
to 600 seconds. Sized to accommodate a cold-cache full recompile (which
ASDF triggers at ~3-5 minutes when output translations bypass the Nix store
FASLs); warm-cache runs complete in well under a minute."
  (let ((raw (uiop:getenv "CLCC_SUITE_TIMEOUT")))
    (or (and raw
             (ignore-errors
               (let ((parsed (parse-integer raw)))
                 (and (plusp parsed) parsed))))
        600)))

(defun %suite-timeout-result (suite-name timeout quit-p)
  "Handle a whole-suite timeout consistently for CLI and programmatic callers."
  (format *error-output*
          "# ERROR: suite ~A timed out after ~A seconds~%"
          suite-name timeout)
  (if quit-p
      (uiop:quit 124)
      t))

(defun %number-tests (plists)
  "Annotate each test plist in PLISTS with a :number index (1-based)."
  (loop for p in plists for i from 1
        collect (append p (list :number i))))

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
                               (warm-stdlib t)
                               (quit-p t))
  "Run all tests in suite-name (and children).
When QUIT-P is true, exits via uiop:quit; otherwise returns whether any test failed."
  (when coverage
    (unless *coverage-reload-in-progress*
      (let ((*coverage-reload-in-progress* t))
        (enable-coverage)
        ;; Coverage app is responsible for loading the desired source systems
        ;; after instrumentation is enabled. Avoid a second force-reload here,
        ;; which otherwise recompiles overlapping test systems in-place and can
        ;; perturb global registries during the coverage phases.
        nil))
    (setf parallel nil)
    (format t "# Coverage mode: parallel disabled~%"))
  (when (or exclude-tags exclude-suites)
    (format t "# Excluding tags: ~S~%# Excluding suites: ~S~%" exclude-tags exclude-suites))
  (when update-snapshots
    (format t "# Snapshot update mode enabled~%"))

  (let* ((suite-timeout (%default-suite-timeout))
         (suite-deadline (and suite-timeout
                              (+ (get-internal-real-time)
                                 (round (* suite-timeout
                                           internal-time-units-per-second)))))
         (killer-live    t)
         (killer-lock    (sb-thread:make-mutex :name "suite-killer-lock"))
         ;; Semaphore used to cancel the deadline-killer thread early (when
         ;; tests finish normally).  We signal it in the unwind-protect cleanup;
         ;; the killer polls try-semaphore so it exits on the next 0.1s tick.
         ;; Note: sb-thread:make-thread in SBCL 2.6.1 accepts only &key name
         ;; arguments — :daemon is NOT a valid keyword and causes a hang on
         ;; macOS 26 ARM64.  The semaphore-based cancellation removes the need
         ;; for a daemon/ephemeral thread entirely.
         (killer-sem     (when suite-deadline
                           (sb-thread:make-semaphore :name "suite-killer-sem" :count 0))))
    ;; Deadline killer: fires (sb-ext:exit :abort t) from a side thread so that
    ;; a GC-safepoint-blocked main thread cannot outlive the suite deadline.
    ;; sb-ext:with-timeout alone is unreliable when the main thread is stuck in GC.
    ;; Poll via sleep (GC-safe safepoint) + try-semaphore (non-blocking) instead of
    ;; wait-on-semaphore :timeout, which uses __ulock_wait2 (NOT a GC safepoint on
    ;; macOS 26 ARM64).  Without this, GC blocks waiting for the killer thread to
    ;; reach a safepoint, while the main thread is already suspended by GC — deadlock.
    (when suite-deadline
      (let ((err *error-output*)
            (remaining (max 1.0 (/ (- suite-deadline (get-internal-real-time))
                                   (float internal-time-units-per-second)))))
        (sb-thread:make-thread
          (lambda ()
            ;; Poll every 0.1s: sleep marks a GC safepoint before blocking.
            ;; try-semaphore is a non-blocking probe — returns t if signalled
            ;; (suite finished normally, do nothing), nil otherwise.
            (loop with check-interval = 0.1d0
                  for elapsed = 0.0d0 then (+ elapsed check-interval)
                  while (< elapsed remaining)
                  do (sleep check-interval)
                     (when (sb-thread:try-semaphore killer-sem)
                       (return))
                  finally
                  (when (sb-thread:with-mutex (killer-lock) killer-live)
                    (format err "# FATAL: suite deadline killer triggered; aborting~%")
                    (finish-output err)
                    (funcall *suite-killer-exit-fn* :code 124))))
          :name "suite-deadline-killer")))
    ;; Do NOT use sb-ext:with-timeout here — on macOS ARM64 SIGALRM is
    ;; delivered to an arbitrary thread and will kill the watchdog thread
    ;; rather than the main thread, disabling per-test timeout enforcement.
    ;; The semaphore-based killer above handles the suite deadline reliably.
    (unwind-protect
         (let ((*parallel-suite-deadline* suite-deadline))
           (let* ((actual-seed     (or seed (random most-positive-fixnum)))
                  (*random-state*  (sb-ext:seed-random-state actual-seed))
                  (tests-plists    (%collect-all-suite-tests suite-name tags exclude-tags exclude-suites))
                  (n               (length tests-plists))
                  (test-vec        (coerce (%number-tests tests-plists) 'vector)))
             (when random (%fisher-yates-shuffle test-vec))
             (let* ((ordered-tests     (%order-tests-for-dependencies (coerce test-vec 'list)))
                    (effective-workers (%effective-worker-count ordered-tests parallel workers)))
               (%print-tap-header n repeat actual-seed effective-workers)
               ;; Second warm call: should be a no-op cache hit if apps.nix pre-warmed.
               ;; No inner sb-ext:with-timeout — see note above about SIGALRM delivery.
               (when warm-stdlib
                 (ignore-errors (cl-cc:warm-stdlib-cache)))
               (when coverage (format t "# Coverage report enabled~%"))
               (let* ((prior-timings   (%load-prior-timings))
                      (all-run-results
                        (loop for r from 1 to repeat
                              do (when (> repeat 1) (format t "# Run ~A/~A~%" r repeat))
                              collect (if parallel
                                          (%run-tests-mixed ordered-tests (max 1 effective-workers) prior-timings)
                                          (%run-tests-sequential ordered-tests)))))
                 (when (> repeat 1) (%detect-flaky (reverse all-run-results) repeat))
                 (format t "# To reproduce this run: (run-suite '~A :seed ~A)~%" suite-name actual-seed)
                 (let* ((flat-results (apply #'append (reverse all-run-results)))
                        (any-fail     (%print-result-summary flat-results)))
                   (when coverage (%print-coverage-report flat-results))
                   (%emit-postrun-artifacts flat-results)
                   (if quit-p
                       (uiop:quit (if any-fail 1 0))
                       any-fail)))))
      ;; Cleanup: cancel the deadline-killer thread before calling uiop:quit,
      ;; so it exits promptly rather than blocking on the remaining sleep time.
      (sb-thread:with-mutex (killer-lock) (setf killer-live nil))
      (when killer-sem (sb-thread:signal-semaphore killer-sem))))))

(defun run-tests (&key
                      (tags nil)
                      (exclude-tags nil)
                      (exclude-suites nil)
                      (parallel t)
                      (random nil))
  "Run the canonical fast CL-CC test plan.
Integration and end-to-end tests are run explicitly by suite taxonomy, not by
naming anything slow or auto-loading an auxiliary system."
  (run-suite 'cl-cc-suite
             :parallel parallel
             :random random
             :warm-stdlib t
             :tags tags
             :exclude-tags exclude-tags
             :exclude-suites (remove-duplicates
                              (append exclude-suites
                                      '(cl-cc-integration-suite cl-cc-e2e-suite))
                              :test #'eq)))

(defun %resolve-suite (package-name symbol-name)
  (let* ((pkg (find-package package-name))
         (sym (and pkg (find-symbol symbol-name pkg))))
    (unless sym
      (error "Suite ~A::~A not found (package not loaded?)"
             package-name symbol-name))
    sym))
