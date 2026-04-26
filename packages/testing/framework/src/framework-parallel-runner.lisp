;;;; framework-parallel-runner.lisp — Parallel worker pool, CPU detection, run-suite, run-tests
(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

(defun %effective-test-timeout (test-plist)
  (let ((tm (getf test-plist :timeout)))
    (cond
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
              (setf results
                    (append results
                            (%run-tests-sequential (list test) results))))))
      (flush-parallel-batch)
      results)))

(defun %parse-command-cpu-count (cmd)
  "Run CMD via uiop:run-program and parse the output as a positive integer, or NIL."
  (ignore-errors
    (let ((n (sb-ext:with-timeout *cpu-detect-command-timeout-seconds*
               (parse-integer (with-output-to-string (s)
                                (uiop:run-program cmd :output s :ignore-error-status t))
                              :junk-allowed t))))
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
Uses CLCC_SUITE_TIMEOUT when it is a positive integer; otherwise falls back to
1800 seconds so the canonical test command cannot hang indefinitely."
  (let ((raw (uiop:getenv "CLCC_SUITE_TIMEOUT")))
    (or (and raw
             (ignore-errors
               (let ((parsed (parse-integer raw)))
                 (and (plusp parsed) parsed))))
        1800)))

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
        (ignore-errors
          (when (find-package :cl-cc/prolog)
            (cl-cc/prolog:clear-prolog-database)))
        ;; Force a fresh local-system reload after instrumentation is enabled so
        ;; sb-cover observes compiled definitions instead of reusing stale fasls.
        (asdf:load-system :cl-cc/test :force t)))
    (setf parallel nil)
    (format t "# Coverage mode: parallel disabled~%"))
  (when (or exclude-tags exclude-suites)
    (format t "# Excluding tags: ~S~%# Excluding suites: ~S~%" exclude-tags exclude-suites))
  (when update-snapshots
    (format t "# Snapshot update mode enabled~%"))

  (let ((suite-timeout (%default-suite-timeout)))
    (handler-case
        (sb-ext:with-timeout suite-timeout
          (let* ((actual-seed     (or seed (random most-positive-fixnum)))
                 (*random-state*  (sb-ext:seed-random-state actual-seed))
                 (tests-plists    (%collect-all-suite-tests suite-name tags exclude-tags exclude-suites))
                 (n               (length tests-plists))
                 (test-vec        (coerce (%number-tests tests-plists) 'vector)))
            (when random (%fisher-yates-shuffle test-vec))
            (let* ((ordered-tests     (%order-tests-for-dependencies (coerce test-vec 'list)))
                   (effective-workers (%effective-worker-count ordered-tests parallel workers)))
              (%print-tap-header n repeat actual-seed effective-workers)
              (when warm-stdlib (ignore-errors (cl-cc::warm-stdlib-cache)))
              (when coverage (format t "# Coverage report enabled~%"))
              (let* ((prior-timings   (%load-prior-timings))
                     (all-run-results
                       (loop for r from 1 to repeat
                             do (when (> repeat 1) (format t "# Run ~A/~A~%" r repeat))
                             collect (if (and parallel (> effective-workers 1))
                                         (%run-tests-mixed ordered-tests effective-workers prior-timings)
                                         (%run-tests-sequential ordered-tests)))))
                (when (> repeat 1) (%detect-flaky (reverse all-run-results) repeat))
                (format t "# To reproduce this run: (run-suite '~A :seed ~A)~%" suite-name actual-seed)
                (let* ((flat-results (apply #'append (reverse all-run-results)))
                       (any-fail     (%print-result-summary flat-results)))
                  (when coverage (%print-coverage-report flat-results))
                  (%emit-postrun-artifacts flat-results)
                  (if quit-p
                      (uiop:quit (if any-fail 1 0))
                      any-fail)))))))
      (sb-ext:timeout ()
        (%suite-timeout-result suite-name suite-timeout quit-p)))))

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
  (let ((effective-exclude-suites (adjoin 'selfhost-slow-suite exclude-suites)))
    (run-suite 'cl-cc-suite
               :parallel parallel
               :random random
               :warm-stdlib t
               :tags tags
               :exclude-tags exclude-tags
               :exclude-suites effective-exclude-suites)))

(defun %resolve-suite (package-name symbol-name)
  (let* ((pkg (find-package package-name))
         (sym (and pkg (find-symbol symbol-name pkg))))
    (unless sym
      (error "Suite ~A::~A not found (package not loaded?)"
             package-name symbol-name))
    sym))
(defparameter *cpu-detect-command-timeout-seconds* 2
  "Timeout in seconds for external CPU-count detection commands.")
