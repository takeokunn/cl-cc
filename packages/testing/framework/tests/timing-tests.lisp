;;;; Unit tests for the per-test timing harness (FR-001/002/003).
;;;;
;;;; These exercise the private surface of the runner directly:
;;;;   - %run-single-test populates :duration-ns for every terminal status
;;;;   - %tap-print-result emits `duration_ms:` in its YAML diagnostic
;;;;   - %write-timings-tsv writes the frozen 5-column schema
;;;;   - %timings-output-path honors the CLCC_TIMINGS_FILE env override
;;;;
;;;; Durations are never asserted for specific values — wall-clock timing is
;;;; inherently non-deterministic. We assert integrality and non-negativity.

(in-package :cl-cc/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(in-suite cl-cc-unit-suite)

(defun %timing-test-plist (name fn)
  "Build a minimal test-plist consumable by %run-single-test."
  (list :name name
        :fn fn
        :suite 'cl-cc-unit-suite
        :timeout nil
        :depends-on nil
        :tags nil))

(deftest-each timing-result-carries-non-negative-duration
  "Every terminal status (pass/fail/skip) carries a non-negative integer :duration-ns."
  :cases (("pass" :pass (lambda () t))
          ("fail" :fail (lambda () (error 'test-failure :message "boom")))
          ("skip" :skip (lambda () (error 'skip-condition :reason "nope"))))
  (expected-status thunk)
  (let* ((plist  (%timing-test-plist 'timing-duration-case thunk))
         (result (%run-single-test plist 1 '()))
         (ns     (getf result :duration-ns)))
    (assert-eq expected-status (getf result :status))
    (assert-true (integerp ns))
    (assert-true (>= ns 0))))

(deftest timing-tap-diagnostic-contains-duration-ms
  "The TAP diagnostic block for a passing test contains `duration_ms:`."
  (let* ((plist (%timing-test-plist 'timing-tap-pass (lambda () t)))
         (result (%run-single-test plist 42 '()))
         (output (with-output-to-string (*standard-output*)
                   (%tap-print-result result))))
    (assert-true (search "duration_ms:" output))
    ;; %tap-print-result emits literal lowercase "ok" followed by the symbol
    ;; name which prints upcased under the default readtable — don't upcase
    ;; the whole output or "ok" itself becomes "OK" and the search fails.
    (assert-true (search "ok 42 - TIMING-TAP-PASS" output))))

(deftest timing-tap-diagnostic-fail-preserves-existing-yaml
  "Failure results still carry their failure YAML and gain duration_ms."
  (let* ((plist (%timing-test-plist
                 'timing-tap-fail
                 (lambda () (error 'test-failure
                                   :message (format nil "  ---~%  message: \"x\"~%  ...")))))
         (result (%run-single-test plist 7 '()))
         (output (with-output-to-string (*standard-output*)
                   (%tap-print-result result))))
    (assert-true (search "not ok 7" output))
    (assert-true (search "message:" output))
    (assert-true (search "duration_ms:" output))))

(deftest timing-tsv-row-has-five-tab-separated-columns
  "%write-timings-tsv produces rows with exactly 5 tab-separated columns in
the frozen order suite\\ttest-name\\tduration-ns\\tstatus\\tbatch-id."
  (let* ((results (list (list :name 'some-test
                              :suite 'cl-cc-unit-suite
                              :status :pass
                              :duration-ns 123456
                              :batch-id 3)
                        (list :name 'other-test
                              :suite 'cl-cc-unit-suite
                              :status :fail
                              :duration-ns 987
                              :batch-id nil)))
         (tmp (format nil "/tmp/cl-cc-timings-test-~A.tsv" (get-universal-time))))
    (unwind-protect
         (progn
           (%write-timings-tsv results tmp)
           (with-open-file (in tmp :direction :input)
             (let ((first-line (read-line in))
                   (second-line (read-line in)))
               ;; Split on tab — the separator is #\Tab (character 9).
               (let ((cols1 (uiop:split-string first-line :separator (list #\Tab)))
                     (cols2 (uiop:split-string second-line :separator (list #\Tab))))
                 (assert-= 5 (length cols1))
                 (assert-= 5 (length cols2))
                 (assert-string= "CL-CC-UNIT-SUITE" (first cols1))
                 (assert-string= "SOME-TEST" (second cols1))
                 (assert-string= "123456" (third cols1))
                 (assert-string= "passed" (fourth cols1))
                 (assert-string= "3" (fifth cols1))
                 ;; Sequential row: batch-id absent → "-"
                 (assert-string= "failed" (fourth cols2))
                 (assert-string= "-" (fifth cols2))))))
      (ignore-errors (delete-file tmp)))))

(deftest timing-output-path-respects-env-override
  "CLCC_TIMINGS_FILE overrides the default ./test-timings.tsv path."
  ;; %timings-output-path rejects absolute paths that don't canonicalize under
  ;; cwd (security guard against arbitrary filesystem writes). Use a cwd-relative
  ;; path so the override is accepted verbatim. The "for-test" suffix avoids
  ;; collision with real test-suite artifacts.
  (let ((override "./cl-cc-timings-override-for-test.tsv"))
    (sb-posix:setenv "CLCC_TIMINGS_FILE" override 1)
    (unwind-protect
         (let ((result (handler-bind ((warning #'muffle-warning))
                         (%timings-output-path))))
           (assert-string= override result))
      (sb-posix:unsetenv "CLCC_TIMINGS_FILE"))))

(deftest timing-output-path-defaults-when-env-unset
  "Without the env var %timings-output-path falls back to the repo-relative default."
  (let ((saved (uiop:getenv "CLCC_TIMINGS_FILE")))
    (sb-posix:unsetenv "CLCC_TIMINGS_FILE")
    (unwind-protect
         (assert-string= "./test-timings.tsv" (%timings-output-path))
      (if saved
          (sb-posix:setenv "CLCC_TIMINGS_FILE" saved 1)
          (sb-posix:unsetenv "CLCC_TIMINGS_FILE")))))

(deftest timing-output-path-rejects-unsafe-absolute-path
  "CLCC_TIMINGS_FILE pointing at /etc/passwd is rejected in favor of default."
  (let ((saved (uiop:getenv "CLCC_TIMINGS_FILE")))
    (sb-posix:setenv "CLCC_TIMINGS_FILE" "/etc/passwd" 1)
    (unwind-protect
         ;; Suppress the warning from %timings-output-path — the assertion is
         ;; that it falls back to the default, not the warning channel.
         (let ((result (handler-bind ((warning #'muffle-warning))
                         (%timings-output-path))))
           (assert-string= "./test-timings.tsv" result))
      (if saved
          (sb-posix:setenv "CLCC_TIMINGS_FILE" saved 1)
          (sb-posix:unsetenv "CLCC_TIMINGS_FILE")))))

(deftest timing-pending-result-carries-duration
  "A test that signals pending-condition still reports :duration-ns."
  (let* ((plist (%timing-test-plist
                 'timing-pending-case
                 (lambda () (error 'pending-condition :reason "soon"))))
         (result (%run-single-test plist 1 '()))
         (ns (getf result :duration-ns)))
    (assert-eq :pending (getf result :status))
    (assert-true (integerp ns))
    (assert-true (>= ns 0))))

(deftest timing-tsv-sanitizes-tab-in-suite-or-name
  "Tab/Newline chars in suite or test name are replaced so each row keeps 5 columns."
  (let* ((bad-name (intern (concatenate 'string "BAD" (string #\Tab) "NAME")
                           :cl-cc/test))
         (bad-suite (intern (concatenate 'string "BAD" (string #\Newline) "SUITE")
                            :cl-cc/test))
         (results (list (list :name bad-name
                              :suite bad-suite
                              :status :pass
                              :duration-ns 1
                              :batch-id nil)))
         (tmp (format nil "/tmp/cl-cc-timings-sanitize-~A.tsv"
                      (get-universal-time))))
    (unwind-protect
         (progn
           (%write-timings-tsv results tmp)
           (with-open-file (in tmp :direction :input)
             (let* ((line (read-line in))
                    (cols (uiop:split-string line :separator (list #\Tab))))
               (assert-= 5 (length cols))
               ;; Sanitizer replaces the tab with a space — exactly one column each.
               (assert-true (search "BAD NAME" (second cols)))
               (assert-true (search "BAD SUITE" (first cols))))))
      (ignore-errors (delete-file tmp)))))

(deftest-each timing-status-keyword-mapping
  "%status-keyword-to-string maps every framework-emitted status to its frozen TSV token."
  :cases (("pass"      "passed"    :pass)
          ("fail"      "failed"    :fail)
          ("skip"      "skipped"   :skip)
          ("pending"   "pending"   :pending)
          ("errored"   "errored"   :errored)
          ("timed-out" "timed-out" :timed-out))
  (expected kw)
  (assert-string= expected (%status-keyword-to-string kw)))

(deftest timing-load-prior-timings-keeps-maximum-duration-per-test
  "%load-prior-timings keeps the max duration when the TSV contains repeated test names."
  (let ((tmp (format nil "/tmp/cl-cc-prior-timings-~A.tsv" (get-universal-time))))
    (unwind-protect
         (progn
           (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format out "SUITE~CFOO~C10~Cpassed~C-~%" #\Tab #\Tab #\Tab #\Tab)
             (format out "SUITE~CFOO~C25~Cpassed~C-~%" #\Tab #\Tab #\Tab #\Tab)
             (format out "SUITE~CBAR~C7~Cfailed~C-~%" #\Tab #\Tab #\Tab #\Tab))
           (let ((timings (%load-prior-timings tmp)))
             (assert-= 25 (gethash "FOO" timings))
             (assert-= 7 (gethash "BAR" timings))))
      (ignore-errors (delete-file tmp)))))

(deftest timing-print-result-summary-reports-failures-and-skips
  "%print-result-summary prints aggregated counts and the failed test list."
  (let* ((results (list (list :name 'alpha :status :pass :number 1)
                        (list :name 'beta :status :skip :number 2)
                        (list :name 'gamma :status :fail :number 3)))
         (output (with-output-to-string (*standard-output*)
                   (assert-true (%print-result-summary results)))))
    (assert-true (search "PASS" output))
    (assert-true (search "FAIL" output))
    (assert-true (search "SKIP" output))
    (assert-true (search "[   3] GAMMA" output))))
