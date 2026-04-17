(in-package :cl-cc/test)

(defun %run-single-test (test-plist number results-so-far)
  "Run one test and return a result plist."
  (let* ((name     (getf test-plist :name))
         (fn       (getf test-plist :fn))
         (timeout  (getf test-plist :timeout))
         (suite    (getf test-plist :suite)))
    ;; Optional hang-diagnosis tracing (opt-in via CLCC_TEST_TRACE=1)
    (when (uiop:getenv "CLCC_TEST_TRACE")
      (format *error-output* "# [trace] running ~A~%" name)
      (force-output *error-output*))
    ;; Check dependency
    (unless (%check-dependency test-plist results-so-far)
      (return-from %run-single-test
        (list :name name :status :skip :detail "dependency failed" :number number)))
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
                           (return-from %run-single-test
                             (list :name name :status :skip
                                   :detail (skip-reason c) :number number))))
                       (pending-condition
                         (lambda (c)
                           (return-from %run-single-test
                             (list :name name :status :pending
                                   :detail (pending-reason c) :number number)))))
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
                    (list :name name :status :pass :detail nil :number number))
                (test-failure (c)
                  (dolist (af after-fns) (ignore-errors (funcall af)))
                  (list :name name :status :fail
                        :detail (test-failure-message c) :number number))
                (sb-ext:timeout ()
                  (dolist (af after-fns) (ignore-errors (funcall af)))
                  (list :name name :status :fail
                        :detail (format nil "  ---~%  message: \"timeout after ~A seconds\"~%  ..."
                                        (if (eq timeout :none) "disabled" (or timeout (%default-test-timeout))))
                        :number number))
                (error (e)
                  (dolist (af after-fns) (ignore-errors (funcall af)))
                  (list :name name :status :fail
                        :detail (format nil "  ---~%  message: ~S~%  ..."
                                        (princ-to-string e))
                        :number number)))))
        (error (e)
          ;; fixture setup error
          (list :name name :status :fail
                :detail (format nil "  ---~%  message: \"fixture error: ~A\"~%  ..."
                                (princ-to-string e))
                :number number))))))

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

(defun %run-tests-parallel (tests workers)
  "Run tests in parallel using sb-thread worker threads.
Each task is executed in its own short-lived sub-thread, and the worker
waits for it with sb-thread:join-thread :timeout. If the sub-thread does
not finish in time it is terminated and the test is recorded as a
timeout fail, guaranteeing the runner can never wedge on a hanging test."
  (let* ((*test-runner-mode* :parallel)
         (n (length tests))
         (results (make-array n :initial-element nil))
         (results-lock (sb-thread:make-mutex :name "results-lock"))
         (work-queue (coerce tests 'vector))
         (queue-index 0)
         (queue-lock (sb-thread:make-mutex :name "queue-lock"))
         (threads '()))
    (labels ((run-with-timeout (test-plist number snapshot)
               (let* ((timeout (%effective-test-timeout test-plist))
                      (result-cell (list nil))
                      (done-lock (sb-thread:make-mutex :name "done"))
                      (done-cv (sb-thread:make-waitqueue :name "done-cv"))
                      (done nil)
                      (runner-thread
                        (sb-thread:make-thread
                         (lambda ()
                           ;; CRITICAL: SBCL sub-threads do NOT inherit the
                           ;; parent's dynamic bindings, so we must rebind
                           ;; *test-runner-mode* here explicitly. Without this,
                           ;; %run-single-test would see the global default
                           ;; (:sequential) and re-enter sb-ext:with-timeout,
                           ;; defeating the parallel timeout enforcement.
                           (let ((*test-runner-mode* :parallel))
                             (let ((r (%run-single-test test-plist number snapshot)))
                               (sb-thread:with-mutex (done-lock)
                                 (setf (car result-cell) r
                                       done t)
                                 (sb-thread:condition-notify done-cv)))))
                         :name (format nil "test-case-~A" number))))
                 (sb-thread:with-mutex (done-lock)
                   (if timeout
                       (loop until done
                             do (unless (sb-thread:condition-wait
                                         done-cv done-lock :timeout timeout)
                                  (return)))
                       (loop until done
                             do (sb-thread:condition-wait done-cv done-lock))))
                 (cond
                   (done
                    (ignore-errors (sb-thread:join-thread runner-thread))
                    (car result-cell))
                   (t
                    (ignore-errors
                     (sb-thread:terminate-thread runner-thread))
                    (ignore-errors (sb-thread:join-thread runner-thread))
                    (list :name (getf test-plist :name)
                          :status :fail
                          :detail (format nil "  ---~%  message: \"timeout after ~A seconds (parallel runner killed thread)\"~%  ..."
                                          timeout)
                          :number number)))))
             (worker ()
               (loop
                 (let ((task nil)
                       (idx nil))
                   (sb-thread:with-mutex (queue-lock)
                     (when (< queue-index n)
                       (setf idx queue-index
                             task (aref work-queue queue-index))
                       (incf queue-index)))
                   (unless task (return))
                   (let* ((test-plist task)
                          (number (getf test-plist :number))
                          (results-snapshot
                            (sb-thread:with-mutex (results-lock)
                              (remove nil (coerce results 'list))))
                          (result (run-with-timeout test-plist number
                                                    results-snapshot)))
                     (sb-thread:with-mutex (results-lock)
                       (setf (aref results idx) result))
                     (%tap-print-result result))))))
      (dotimes (i workers)
        (push (sb-thread:make-thread #'worker :name (format nil "test-worker-~A" i))
              threads))
      (dolist (th threads)
        (sb-thread:join-thread th))
      (coerce results 'list))))

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

(defun %run-tests-mixed (tests workers)
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
                                                  workers)))
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

(defun %effective-worker-count (ordered-tests parallel workers)
  "Return the effective worker count for ORDERED-TESTS.
Serial runs, serial-only batches, and single-worker requests all collapse to 1."
  (if (and parallel
           (> (or workers 4) 1)
           (some #'%test-parallel-safe-p ordered-tests))
      (or workers 4)
      1))

;;; ------------------------------------------------------------
;;; Flaky Detection
;;; ------------------------------------------------------------

(defun %detect-flaky (all-run-results repeat)
  "Given a list of repeat result-lists, find tests with inconsistent status."
  (let ((by-name (make-hash-table)))
    (dolist (run-results all-run-results)
      (dolist (r run-results)
        (let ((name (getf r :name))
              (status (getf r :status)))
          (push status (gethash name by-name)))))
    (let ((flaky '()))
      (maphash (lambda (name statuses)
                 (let ((pass-count (count :pass statuses))
                       (total (length statuses)))
                   (when (and (< pass-count total) (> pass-count 0))
                     (push (list name pass-count total) flaky))))
               by-name)
      (when flaky
        (format t "# Flaky tests detected (inconsistent across ~A runs):~%" repeat)
        (dolist (f flaky)
          (format t "#   ~A: passed ~A/~A runs~%"
                  (first f) (second f) (third f)))))))

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
              (let ((all-run-results '()))
                (dotimes (r repeat)
                  (when (> repeat 1)
                    (format t "# Run ~A/~A~%" (1+ r) repeat))
                  (let ((run-results
                          (if (and parallel (> nworkers 1))
                              (%run-tests-mixed ordered-tests nworkers)
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

;;; ------------------------------------------------------------
;;; Suite Definitions (bottom of file — other files use in-suite)
;;; ------------------------------------------------------------

(defsuite cl-cc-suite :description "CL-CC test root suite")

(defsuite cl-cc-unit-suite
  :description "Unit tests"
  :parent cl-cc-suite)

;; Integration tests that exercise the full compile pipeline via run-string.
;; Each case invokes parse → expand → cps → optimize → codegen → execute.
;; Heavy tests rely on *stdlib-expanded-cache* being pre-warmed by run-suite
;; before the parallel workers start.
(defsuite cl-cc-integration-suite
  :description "Full-pipeline integration tests"
  :parent cl-cc-suite)

(defsuite cl-cc-integration-serial-suite
  :description "Sequential-only integration tests"
  :parent cl-cc-integration-suite
  :parallel nil)

(defsuite cl-cc-e2e-suite
  :description "End-to-end tests"
  :parent cl-cc-suite)

(defsuite cl-cc-serial-suite
  :description "Sequential-only unit tests"
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite cl-cc-suite)
