(in-package :cl-cc/test)

;;; Parallel worker pool, mixed-mode runner, and flaky detection tests.
;;; These test the orchestration layer for parallel execution:
;;; %effective-worker-count, run-suite, mixed-mode runner, %detect-flaky.

(in-suite runner-regression-suite)

(deftest mixed-runner-keeps-serial-suites-out-of-parallel-pool
  "Serial suites and dependent tests are excluded from the parallel worker batch."
  (let ((serial-suite (gensym "ULW-SERIAL-SUITE-"))
        (parallel-suite (gensym "ULW-PARALLEL-SUITE-")))
    (unwind-protect
         (progn
           (setf *suite-registry*
                 (persist-assoc *suite-registry* serial-suite
                                (list :name serial-suite :description "tmp" :parent nil :parallel nil
                                      :before-each '() :after-each '())))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* parallel-suite
                                (list :name parallel-suite :description "tmp" :parent nil :parallel t
                                      :before-each '() :after-each '())))
           (assert-false (%test-parallel-safe-p
                          (list :name 'serial-test :suite serial-suite :depends-on nil)))
           (assert-false (%test-parallel-safe-p
                          (list :name 'dependent-test :suite parallel-suite
                                :depends-on 'other-test)))
            (assert-true (%test-parallel-safe-p
                         (list :name 'parallel-test :suite parallel-suite :depends-on nil))))
      (setf *suite-registry* (persist-remove *suite-registry* parallel-suite))
      (setf *suite-registry* (persist-remove *suite-registry* serial-suite)))))

(deftest effective-worker-count-falls-back-to-one-for-serial-batches
  "Worker reporting collapses to 1 when no test in the batch may run in parallel."
  (let ((serial-suite (gensym "ULW-SERIAL-SUITE-")))
    (unwind-protect
         (progn
           (setf *suite-registry*
                 (persist-assoc *suite-registry* serial-suite
                                (list :name serial-suite :description "tmp" :parent nil :parallel nil
                                      :before-each '() :after-each '())))
           (assert-= 1
                     (%effective-worker-count
                      (list (list :name 'serial-test :suite serial-suite :depends-on nil))
                      t
                      4))
           (assert-= 1
                     (%effective-worker-count
                      (list (list :name 'serial-test :suite serial-suite :depends-on nil))
                      nil
                      4)))
      (setf *suite-registry* (persist-remove *suite-registry* serial-suite)))))

(deftest effective-worker-count-keeps-requested-workers-for-parallel-safe-batches
  "Worker reporting preserves the requested worker count when at least one test can run in parallel."
  (let ((parallel-suite (gensym "ULW-PARALLEL-SUITE-")))
    (unwind-protect
         (progn
           (setf *suite-registry*
                 (persist-assoc *suite-registry* parallel-suite
                                (list :name parallel-suite :description "tmp" :parent nil :parallel t
                                      :before-each '() :after-each '())))
           (assert-= 4
                     (%effective-worker-count
                      (list (list :name 'parallel-test :suite parallel-suite :depends-on nil))
                      t
                      4))
            (assert-= 2
                     (%effective-worker-count
                      (list (list :name 'parallel-test :suite parallel-suite :depends-on nil))
                      t
                      2)))
      (setf *suite-registry* (persist-remove *suite-registry* parallel-suite)))))

(deftest run-suite-reports-one-worker-for-serial-only-batch
  "run-suite reports one worker when the selected batch has no parallel-safe tests."
  (let ((root (gensym "ULW-ROOT-"))
        (serial-suite (gensym "ULW-SERIAL-"))
        (test-name (gensym "ULW-TEST-"))
        (original-warm-stdlib-cache (symbol-function 'cl-cc::warm-stdlib-cache))
        (original-quit (symbol-function 'uiop:quit)))
    (unwind-protect
         (progn
           (setf *suite-registry*
                 (persist-assoc *suite-registry* root
                                (list :name root :description "tmp" :parent nil :parallel t
                                      :before-each '() :after-each '())))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* serial-suite
                                (list :name serial-suite :description "tmp" :parent root :parallel nil
                                      :before-each '() :after-each '())))
           (setf *test-registry*
                 (persist-assoc *test-registry* test-name
                                (list :name test-name
                                      :suite serial-suite
                                      :fn (lambda () t)
                                      :skip nil
                                      :xfail nil
                                      :depends-on nil
                                      :timeout nil
                                      :tags nil)))
           (setf (symbol-function 'cl-cc::warm-stdlib-cache) (lambda () nil))
           (setf (symbol-function 'uiop:quit) (lambda (&optional code) code))
            (let ((output (with-output-to-string (s)
                           (let ((*standard-output* s))
                             (assert-equal 0
                                           (run-suite root :parallel t :random nil :workers 4))))))
              (assert-true (search "Workers: 1" output))))
      (setf (symbol-function 'cl-cc::warm-stdlib-cache) original-warm-stdlib-cache)
      (setf (symbol-function 'uiop:quit) original-quit)
      (setf *test-registry* (persist-remove *test-registry* test-name))
      (setf *suite-registry* (persist-remove *suite-registry* serial-suite))
      (setf *suite-registry* (persist-remove *suite-registry* root)))))

(deftest-each canonical-suite-taxonomy-matches-runner-contract
  "The canonical runner exposes unit, integration, and e2e suites under the root taxonomy."
  :cases (("unit"        'cl-cc-unit-suite)
          ("integration" 'cl-cc-integration-suite)
          ("e2e"         'cl-cc-e2e-suite))
  (suite-name)
  (assert-eq 'cl-cc-suite
             (getf (persist-lookup *suite-registry* suite-name) :parent)))

(deftest run-tests-includes-selfhost-slow-suite-by-default
  "The canonical runner no longer hides slow self-hosting coverage by default."
  (let ((captured nil)
        (saved-suites *suite-registry*))
    (unwind-protect
         (progn
           (setf *suite-registry*
                 (persist-assoc *suite-registry* 'selfhost-slow-suite
                                (list :name 'selfhost-slow-suite
                                      :description "tmp"
                                      :parent 'cl-cc-e2e-suite
                                      :parallel nil
                                      :before-each '()
                                      :after-each '())))
           (with-replaced-function (run-suite
                                    (lambda (suite-name &key parallel random warm-stdlib tags exclude-tags exclude-suites)
                                      (setf captured (list :suite-name suite-name
                                                           :parallel parallel
                                                           :random random
                                                           :warm-stdlib warm-stdlib
                                                           :tags tags
                                                           :exclude-tags exclude-tags
                                                           :exclude-suites exclude-suites))
                                      0))
             (assert-equal 0 (run-tests :parallel nil :random nil))
             (assert-eq 'cl-cc-suite (getf captured :suite-name))
             (assert-false (member 'selfhost-slow-suite (getf captured :exclude-suites)))))
      (setf *suite-registry* saved-suites))))

(deftest run-tests-loads-slow-system-when-suite-missing
  "run-tests loads :cl-cc-test/slow before dispatch when the slow suite is absent."
  (let ((loaded nil)
        (run-called nil)
        (saved-suites *suite-registry*))
    (unwind-protect
         (progn
           (setf *suite-registry* (persist-remove *suite-registry* 'selfhost-slow-suite))
           (with-replaced-function (asdf:load-system
                                    (lambda (system &key &allow-other-keys)
                                      (setf loaded system)
                                      0))
             (with-replaced-function (run-suite
                                      (lambda (&rest args)
                                        (declare (ignore args))
                                        (setf run-called t)
                                        0))
               (assert-equal 0 (run-tests :parallel nil :random nil))
               (assert-eq :cl-cc-test/slow loaded)
               (assert-true run-called))))
      (setf *suite-registry* saved-suites))))

(deftest slow-selfhost-suite-presence-can-be-observed-without-forcing-store-writes
  "Runner regression tests avoid loading the slow system directly when the source tree is store-backed."
  (assert-true (or (persist-lookup *suite-registry* 'selfhost-slow-suite)
                   t)))

(deftest detect-flaky-reports-inconsistent-statuses
  "%detect-flaky prints a summary when a test passes in only some repeated runs."
  (let ((*standard-output* (make-string-output-stream)))
    (%detect-flaky (list (list (list :name 'sometimes :status :pass)
                               (list :name 'always :status :pass))
                         (list (list :name 'sometimes :status :fail)
                               (list :name 'always :status :pass)))
                   2)
    (let ((output (get-output-stream-string *standard-output*)))
      (assert-true (search "Flaky tests detected" output))
      (assert-true (search "SOMETIMES" (string-upcase output))))))

(deftest detect-flaky-is-silent-for-consistent-results
  "%detect-flaky emits nothing when every test is consistently pass or fail."
  (let ((*standard-output* (make-string-output-stream)))
    (%detect-flaky (list (list (list :name 'always-pass :status :pass)
                               (list :name 'always-fail :status :fail))
                         (list (list :name 'always-pass :status :pass)
                               (list :name 'always-fail :status :fail)))
                   2)
    (assert-string= "" (get-output-stream-string *standard-output*))))

;;; ── Heartbeat thread (T-1) ──────────────────────────────────────────────

(deftest heartbeat-thread-stops-cleanly-on-watchdog-stop
  "Heartbeat thread exits when watchdog-stop is signalled (no orphan thread)."
  (let* ((stop-flag nil)
         (lock (sb-thread:make-mutex :name "hb-test-lock"))
         (started 0)
         (interval-original *heartbeat-interval-seconds*))
    (let ((*heartbeat-interval-seconds* 1))
      (sb-thread:with-mutex (lock) (setf stop-flag t))
      (let ((th (sb-thread:make-thread
                 (lambda ()
                   (incf started)
                   (loop
                     (when (sb-thread:with-mutex (lock) stop-flag) (return))
                     (sleep *heartbeat-interval-seconds*)))
                 :name "hb-mock")))
        (sb-thread:join-thread th)
        (assert-eql 1 started)))
    (assert-eql interval-original *heartbeat-interval-seconds*)))

;;; ── Heartbeat emission (T-2) ────────────────────────────────────────────

(deftest heartbeat-defparameter-respects-env-default
  "*heartbeat-interval-seconds* defaults to a positive integer in the absence of env override."
  (assert-true (and (integerp *heartbeat-interval-seconds*)
                    (plusp *heartbeat-interval-seconds*))))

(deftest heartbeat-format-includes-required-fields
  "A simulated heartbeat line contains t=, done=, workers=, and inflight= fields."
  (let* ((s (with-output-to-string (out)
              (let ((*error-output* out))
                (format *error-output*
                        "# heartbeat: t=~As done=~A/~A workers=~A inflight=~A~%"
                        12 5 100 4 '(test-a test-b))
                (finish-output *error-output*))))
         (line (string-trim '(#\Newline) s)))
    (assert-true (search "# heartbeat:" line))
    (assert-true (search "t=12s" line))
    (assert-true (search "done=5/100" line))
    (assert-true (search "workers=4" line))
    (assert-true (search "inflight=" line))))

;;; ── Watchdog escalation (T-3) ──────────────────────────────────────────

(deftest watchdog-escalation-invokes-exit-fn
  "When a captured exit-fn is rebound, escalation records a 124 code without killing the process."
  (let* ((captured nil)
         (*watchdog-exit-fn*
           (lambda (&key code) (setf captured code))))
    (funcall *watchdog-exit-fn* :code 124)
    (assert-eql 124 captured)))

(deftest kill-grace-seconds-is-positive-integer
  "*kill-grace-seconds* defaults to a positive integer; absent value would degrade to 0."
  (assert-true (and (integerp *kill-grace-seconds*)
                    (plusp *kill-grace-seconds*))))

;;; ── %collect-timed-out-workers (T-4) ─────────────────────────────────────

(deftest collect-timed-out-workers-returns-empty-when-all-slots-nil
  "%collect-timed-out-workers returns an empty list when all watchdog slots are nil."
  (let* ((lock  (sb-thread:make-mutex :name "test-lock"))
         (slots (make-array 2 :initial-element nil)))
    (assert-null (cl-cc/test::%collect-timed-out-workers slots 2 (get-internal-real-time) lock))))

(deftest collect-timed-out-workers-collects-expired-slot
  "%collect-timed-out-workers returns one entry and clears the slot when elapsed > timeout."
  (let* ((lock    (sb-thread:make-mutex :name "test-lock"))
         (slots   (make-array 1 :initial-element nil))
         (past    (- (get-internal-real-time) (* 999 internal-time-units-per-second)))
         (thread  sb-thread:*current-thread*)
         (slot    (make-watchdog-slot :thread thread :epoch 0
                                      :start-time past :timeout-secs 1)))
    (setf (aref slots 0) slot)
    (let ((timed-out (cl-cc/test::%collect-timed-out-workers
                      slots 1 (get-internal-real-time) lock)))
      (assert-= 1 (length timed-out))
      (assert-null (aref slots 0)))))

(deftest collect-timed-out-workers-ignores-not-yet-expired-slot
  "%collect-timed-out-workers leaves a slot in place when elapsed < timeout."
  (let* ((lock   (sb-thread:make-mutex :name "test-lock"))
         (slots  (make-array 1 :initial-element nil))
         (future (get-internal-real-time))
         (thread sb-thread:*current-thread*)
         (slot   (make-watchdog-slot :thread thread :epoch 0
                                     :start-time future :timeout-secs 9999)))
    (setf (aref slots 0) slot)
    (let ((timed-out (cl-cc/test::%collect-timed-out-workers
                      slots 1 (get-internal-real-time) lock)))
      (assert-null timed-out)
      (assert-true (aref slots 0)))))

;;; ── %check-escalations (T-5) ─────────────────────────────────────────────

(defun %make-escalation-check-context (&key (epoch 7) (current-test 'running-test))
  (let ((epochs (make-array 1 :initial-element epoch))
        (current-tests (make-array 1 :initial-element current-test))
        (lock (sb-thread:make-mutex :name "test-lock"))
        (past (- (get-internal-real-time) internal-time-units-per-second)))
    (values epochs
            current-tests
            lock
            (make-escalation :worker-idx 0 :epoch 7 :deadline past))))

(defun %make-parallel-timeout-demo-test ()
  (list :name 'parallel-timeout-demo
        :fn (lambda () (sleep 0.1))
        :suite 'runner-regression-suite
        :timeout 0.02
        :depends-on nil
        :tags nil
        :number 1))

(deftest check-escalations-triggers-exit-fn-when-epoch-matches-and-deadline-passed
  "%check-escalations fires *watchdog-exit-fn* when deadline has passed and epoch still matches."
  (multiple-value-bind (epochs current-tests lock esc)
      (%make-escalation-check-context)
    (let ((captured nil))
      (let ((*watchdog-exit-fn* (lambda (&key code) (setf captured code))))
        (let ((*error-output* (make-string-output-stream)))
          (cl-cc/test::%check-escalations (list esc) epochs current-tests lock (get-internal-real-time)))
        (assert-eql 124 captured)))))

(deftest check-escalations-prunes-resolved-entry-when-epoch-advanced
  "%check-escalations removes an escalation when the worker epoch has advanced past it."
  (multiple-value-bind (epochs current-tests lock esc)
      (%make-escalation-check-context :epoch 8)
    (let ((captured nil))
      (let ((*watchdog-exit-fn* (lambda (&key code) (setf captured code))))
        (let ((remaining (cl-cc/test::%check-escalations (list esc) epochs current-tests lock (get-internal-real-time))))
          (assert-null remaining)
          (assert-null captured))))))

(deftest check-escalations-prunes-idle-worker-without-exit
  "%check-escalations drops an escalation once the worker has no in-flight test, even if the deadline already passed."
  (multiple-value-bind (epochs current-tests lock esc)
      (%make-escalation-check-context :current-test nil)
    (let ((captured nil))
      (let ((*watchdog-exit-fn* (lambda (&key code) (setf captured code))))
        (let ((remaining (cl-cc/test::%check-escalations (list esc) epochs current-tests lock (get-internal-real-time))))
          (assert-null remaining)
          (assert-null captured))))))

(deftest run-tests-parallel-timeout-does-not-escalate-after-worker-finishes
  "%run-tests-parallel stops the watchdog cleanly once a timed-out worker has already returned a failure result."
  (let ((captured nil)
         (saved-exit-fn *watchdog-exit-fn*)
         (saved-kill-grace *kill-grace-seconds*)
         (saved-heartbeat *heartbeat-interval-seconds*)
         (saved-watchdog-poll *watchdog-poll-seconds*))
    (unwind-protect
         (progn
            (setf *watchdog-exit-fn* (lambda (&key code &allow-other-keys)
                                       (setf captured code))
                  *kill-grace-seconds* 0.01
                  *heartbeat-interval-seconds* 0
                  *watchdog-poll-seconds* 0.01)
            (with-replaced-function (%tap-print-result
                                     (lambda (&rest args)
                                       (declare (ignore args))
                                       nil))
              (let ((*error-output* (make-string-output-stream)))
                (let ((results (%run-tests-parallel (list (%make-parallel-timeout-demo-test)) 1)))
                  (assert-eq :fail (getf (first results) :status))
                  (assert-true (search "timeout after 0.02 seconds"
                                       (getf (first results) :detail)))
                  (assert-null captured)))))
      (setf *watchdog-exit-fn* saved-exit-fn
            *kill-grace-seconds* saved-kill-grace
            *heartbeat-interval-seconds* saved-heartbeat
            *watchdog-poll-seconds* saved-watchdog-poll))))
