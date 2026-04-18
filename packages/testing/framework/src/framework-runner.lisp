(in-package :cl-cc/test)

;;; ── Result constructors ──────────────────────────────────────────────────
;;; All test outcomes share the same plist shape. Named constructors eliminate
;;; repeated inline `(list :name ... :status ... :detail ... :number ... :suite ...)`.

(defun %make-test-result (name number suite status detail)
  (list :name name :status status :detail detail :number number :suite suite))

(defun %format-timeout-detail (timeout)
  (format nil "  ---~%  message: \"timeout after ~A seconds\"~%  ..."
          (if (eq timeout :none) "disabled" (or timeout (%default-test-timeout)))))

(defun %fail-result (name number suite detail)
  (%make-test-result name number suite :fail detail))

(defun %run-after-fns-safely (after-fns)
  (dolist (af after-fns) (ignore-errors (funcall af))))

;;; ── Macro-environment isolation ──────────────────────────────────────────
;;; Each test runs in a fresh copy of the macro/expander tables so that
;;; defmacro side-effects inside one test do not bleed into the next.

(defmacro %with-isolated-macro-environment (&body body)
  `(let ((cl-cc/expand::*macro-environment*      (%copy-macro-environment))
         (cl-cc/expand::*symbol-macro-table*
           (%copy-hash-table-shallow cl-cc/expand::*symbol-macro-table*))
         (cl-cc/expand::*compiler-macro-table*
           (%copy-hash-table-shallow cl-cc/expand::*compiler-macro-table*))
         (cl-cc/expand::*macroexpand-step-cache*  (make-hash-table :test #'eq :weakness :key))
         (cl-cc/expand::*macroexpand-all-cache*   (make-hash-table :test #'eq :weakness :key)))
     ,@body))

(defvar *test-runner-mode* :sequential
  "Dynamic indicator of the current runner mode (:sequential or :parallel).
%run-single-test consults this to decide whether sb-ext:with-timeout is safe
to use (sequential) or whether the parallel runner will enforce the timeout
via sb-thread:join-thread :timeout instead.")

;;; ── Test body execution ───────────────────────────────────────────────────
;;; Runs FN with skip/pending/timeout/error condition handling.
;;; NOTE: sb-ext:with-timeout relies on SIGALRM which is unreliable inside
;;; sb-thread worker threads — parallel runs enforce timeouts via watchdog;
;;; this branch is the sequential-only fallback and is always safe.

(defun %run-test-body (fn name number suite timeout after-fns)
  (block %run-body
    (handler-case
        (handler-bind
            ((skip-condition
               (lambda (c)
                 (return-from %run-body
                   (%make-test-result name number suite :skip (skip-reason c)))))
             (pending-condition
               (lambda (c)
                 (return-from %run-body
                   (%make-test-result name number suite :pending (pending-reason c))))))
          (if (or (eq timeout :none) (eq *test-runner-mode* :parallel))
              (funcall fn)
              (sb-ext:with-timeout (or timeout (%default-test-timeout))
                (funcall fn)))
          (dolist (af after-fns) (funcall af))
          (%run-invariants)
          (%make-test-result name number suite :pass nil))
      (test-failure (c)
        (%run-after-fns-safely after-fns)
        (%fail-result name number suite (test-failure-message c)))
      (sb-ext:timeout ()
        (%run-after-fns-safely after-fns)
        (%fail-result name number suite (%format-timeout-detail timeout)))
      (error (e)
        (%run-after-fns-safely after-fns)
        (%fail-result name number suite
                      (format nil "  ---~%  message: ~S~%  ..." (princ-to-string e)))))))

(defun %compute-duration-ns (start-time end-time)
  "Convert internal-time-units difference to integer nanoseconds.
LC_ALL-independent integer arithmetic, never scientific notation."
  (floor (* (- end-time start-time) 1000000000)
         internal-time-units-per-second))

(defun %run-single-test (test-plist number results-so-far)
  "Run one test and return a result plist with :duration-ns attached."
  (let* ((name       (getf test-plist :name))
         (fn         (getf test-plist :fn))
         (timeout    (getf test-plist :timeout))
         (suite      (getf test-plist :suite))
         (start-time (get-internal-real-time))
         (duration-ns 0)
         (result nil))
    (when (uiop:getenv "CLCC_TEST_TRACE")
      (format *error-output* "# [trace] running ~A~%" name)
      (force-output *error-output*))
    (unwind-protect
         (setf result
               (block %run-body
                 (unless (%check-dependency test-plist results-so-far)
                   (return-from %run-body
                     (%make-test-result name number suite :skip "dependency failed")))
                 (multiple-value-bind (before-fns after-fns)
                     (%get-suite-fixtures suite)
                   (handler-case
                       (%with-isolated-macro-environment
                         (dolist (bf before-fns) (funcall bf))
                         (%run-test-body fn name number suite timeout after-fns))
                     (error (e)
                       (%fail-result name number suite
                                     (format nil "  ---~%  message: \"fixture error: ~A\"~%  ..."
                                             (princ-to-string e))))))))
      (setf duration-ns (%compute-duration-ns start-time (get-internal-real-time))))
    (if result
        (append result (list :duration-ns duration-ns))
        (append (%fail-result name number suite
                              "  ---~%  message: \"aborted before producing result\"~%  ...")
                (list :duration-ns duration-ns)))))

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

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
    (let ((n (parse-integer (with-output-to-string (s)
                              (uiop:run-program cmd :output s :ignore-error-status t))
                            :junk-allowed t)))
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
  "Run all tests in suite-name (and children). Exits via uiop:quit."
  (when coverage
    (setf parallel nil)
    (format t "# Coverage mode: parallel disabled~%"))
  (when (or exclude-tags exclude-suites)
    (format t "# Excluding tags: ~S~%# Excluding suites: ~S~%" exclude-tags exclude-suites))
  (when update-snapshots
    (format t "# Snapshot update mode enabled~%"))

  (flet ((%number-tests (plists)
           (loop for p in plists for i from 1
                 collect (append p (list :number i)))))
    (let* ((actual-seed    (or seed (random most-positive-fixnum)))
           (*random-state* (sb-ext:seed-random-state actual-seed))
           (tests-plists   (%collect-all-suite-tests suite-name tags exclude-tags exclude-suites))
           (n              (length tests-plists))
           (test-vec       (coerce (%number-tests tests-plists) 'vector)))
    (when random (%fisher-yates-shuffle test-vec))
    (let* ((ordered-tests    (%order-tests-for-dependencies (coerce test-vec 'list)))
           (effective-workers (%effective-worker-count ordered-tests parallel workers)))
      (%print-tap-header n repeat actual-seed effective-workers)
      (when warm-stdlib (ignore-errors (cl-cc::warm-stdlib-cache)))
      (when coverage (format t "# Coverage report enabled~%"))

      (let* ((prior-timings  (%load-prior-timings))
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
          (uiop:quit (if any-fail 1 0))))))))

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
