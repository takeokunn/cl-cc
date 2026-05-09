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

(deftest timing-tap-diagnostic-compact-pass-emits-nothing
  "In compact mode (default), passing tests emit no output."
  (let ((*verbose-tap-mode* :compact))
    (let* ((plist  (%timing-test-plist 'timing-tap-pass (lambda () t)))
           (result (%run-single-test plist 42 '()))
           (output (with-output-to-string (*standard-output*)
                     (%tap-print-result result))))
      (assert-true (zerop (length output))))))

(deftest timing-tap-diagnostic-verbose-pass-contains-duration-ms
  "In verbose TAP mode, a passing test emits duration_ms: in its YAML diagnostic."
  (let ((*verbose-tap-mode* :verbose))
    (let* ((plist  (%timing-test-plist 'timing-tap-pass (lambda () t)))
           (result (%run-single-test plist 42 '()))
           (output (with-output-to-string (*standard-output*)
                     (%tap-print-result result))))
      (assert-true (search "duration_ms:" output))
      (assert-true (search "ok 42 - TIMING-TAP-PASS" output)))))

(deftest timing-tap-diagnostic-fail-preserves-existing-yaml
  "Failure results still carry their failure YAML and gain duration_ms."
  (let ((*verbose-tap-mode* :compact))
    (let* ((plist (%timing-test-plist
                   'timing-tap-fail
                   (lambda () (error 'test-failure
                                     :message (format nil "  ---~%  message: \"x\"~%  ...")))))
           (result (%run-single-test plist 7 '()))
           (output (with-output-to-string (*standard-output*)
                     (%tap-print-result result))))
      (assert-true (search "not ok - " output))
      (assert-true (search "message:" output))
      (assert-true (search "duration_ms:" output)))))

;;; ── %tap-verbose-first-line (T-V) ──────────────────────────────────────────

(deftest-each tap-verbose-first-line-all-statuses
  "%tap-verbose-first-line builds the correct TAP first-line for each status."
  :cases (("pass"    :pass    "ok"     nil   "ok 1 - MY-TEST")
          ("fail"    :fail    "not ok" nil   "not ok 2 - MY-TEST")
          ("skip"    :skip    "ok"     "why" "ok 3 - MY-TEST # SKIP why")
          ("pending" :pending "not ok" "tbd" "not ok 4 - MY-TEST # TODO tbd"))
  (status _prefix detail expected)
  (declare (ignore _prefix))
  (let ((num (ecase status (:pass 1) (:fail 2) (:skip 3) (:pending 4))))
    (assert-string= expected
                    (%tap-verbose-first-line num 'my-test status detail))))

(deftest tap-verbose-first-line-skip-without-detail-falls-back-to-empty
  "%tap-verbose-first-line with NIL detail for :skip uses empty string."
  (assert-string= "ok 5 - TEST # SKIP "
                  (%tap-verbose-first-line 5 'test :skip nil)))

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

(deftest timing-set-test-timeouts-by-prefix-updates-only-matching-tests
  "set-test-timeouts-by-prefix! only rewrites matching registered tests."
  (let ((saved *test-registry*))
    (unwind-protect
         (progn
           (setf *test-registry* (persist-empty))
           (setf *test-registry*
                 (persist-assoc *test-registry* 'foo-match
                                (list :name 'foo-match :suite 'cl-cc-unit-suite :timeout nil)))
           (setf *test-registry*
                 (persist-assoc *test-registry* 'bar-miss
                                (list :name 'bar-miss :suite 'cl-cc-unit-suite :timeout nil)))
           (set-test-timeouts-by-prefix! "FOO-" 17)
           (assert-= 17 (getf (persist-lookup *test-registry* 'foo-match) :timeout))
           (assert-null (getf (persist-lookup *test-registry* 'bar-miss) :timeout)))
      (setf *test-registry* saved))))

(deftest timing-bulk-timeout-helpers-preserve-explicit-timeouts-by-default
  "Bulk timeout helpers do not overwrite explicit per-test timeouts unless asked."
  (let ((saved-tests *test-registry*)
        (saved-suites *suite-registry*)
        (suite 'timing-preserve-suite))
    (unwind-protect
         (progn
           (setf *suite-registry* (persist-empty))
           (setf *test-registry* (persist-empty))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* suite
                                (list :name suite :description "preserve" :parent nil :parallel t :before-each '() :after-each '())))
           (setf *test-registry*
                 (persist-assoc *test-registry* 'foo-keep
                                (list :name 'foo-keep :suite suite :timeout 9)))
           (setf *test-registry*
                 (persist-assoc *test-registry* 'foo-fill
                                (list :name 'foo-fill :suite suite :timeout nil)))
           (set-test-timeouts-by-prefix! "FOO-" 17)
           (set-suite-test-timeout! suite 23 :recursive t)
           (assert-= 9 (getf (persist-lookup *test-registry* 'foo-keep) :timeout))
           (assert-= 17 (getf (persist-lookup *test-registry* 'foo-fill) :timeout)))
      (setf *test-registry* saved-tests
            *suite-registry* saved-suites))))

(deftest timing-set-suite-test-timeout-can-update-descendants
  "set-suite-test-timeout! optionally updates descendant suite tests too."
  (let ((saved-tests *test-registry*)
        (saved-suites *suite-registry*)
        (parent 'timing-timeout-parent)
        (child 'timing-timeout-child))
    (unwind-protect
         (progn
           (setf *suite-registry* (persist-empty))
           (setf *test-registry* (persist-empty))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* parent
                                (list :name parent :description "parent" :parent nil :parallel t :before-each '() :after-each '())))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* child
                                (list :name child :description "child" :parent parent :parallel t :before-each '() :after-each '())))
           (setf *test-registry*
                 (persist-assoc *test-registry* 'parent-test
                                (list :name 'parent-test :suite parent :timeout nil)))
           (setf *test-registry*
                 (persist-assoc *test-registry* 'child-test
                                (list :name 'child-test :suite child :timeout nil)))
           (set-suite-test-timeout! parent 23 :recursive t)
           (assert-= 23 (getf (persist-lookup *test-registry* 'parent-test) :timeout))
           (assert-= 23 (getf (persist-lookup *test-registry* 'child-test) :timeout)))
      (setf *test-registry* saved-tests
            *suite-registry* saved-suites))))

(deftest timing-print-result-summary-reports-failures-and-skips
  "%print-result-summary prints aggregated counts and the failed test list."
  (let* ((*verbose-tap-mode* :compact)
         (results (list (list :name 'alpha :status :pass :number 1)
                        (list :name 'beta :status :skip :number 2)
                        (list :name 'gamma :status :fail :number 3)))
         (output (with-output-to-string (*standard-output*)
                    (assert-true (%print-result-summary results)))))
    (assert-true (search "1 passed" output))
    (assert-true (search "1 failed" output))
    (assert-true (search "1 skipped" output))))

;;; ── %detail-ends-with-yaml-terminator-p (T-Y) ──────────────────────────────

(deftest-each detail-ends-with-yaml-terminator-p-cases
  "%detail-ends-with-yaml-terminator-p detects trailing '  ...' correctly."
  :cases (("ends-with-terminator"    "  ---\n  message: x\n  ..."  t)
          ("no-terminator"           "  ---\n  message: x"          nil)
          ("too-short"               "..."                           nil)
          ("nil-input"               nil                             nil)
          ("exact-terminator-only"   "  ..."                        t))
  (detail expected)
  (assert-eq (and expected t)
             (%detail-ends-with-yaml-terminator-p detail)))

;;; ── %duration-ms-from-result (T-D) ─────────────────────────────────────────

(deftest-each duration-ms-from-result-cases
  "%duration-ms-from-result converts :duration-ns to ms or returns 0.0d0 for missing/invalid values."
  :cases (("positive-ns"   1000000   1.0d0)
          ("zero-ns"       0         0.0d0)
          ("nil-ns"        nil       0.0d0))
  (ns expected)
  (let ((result (list :duration-ns ns)))
    (assert-equal expected (%duration-ms-from-result result))))

;;; ── %source-file-display (T-S) ──────────────────────────────────────────────

(deftest source-file-display-nil-returns-nil
  "%source-file-display returns NIL for a NIL path."
  (assert-null (%source-file-display nil)))

(deftest source-file-display-relative-path-stays-relative
  "%source-file-display strips the cwd prefix when the path starts with cwd."
  (let* ((cwd     (namestring (uiop:getcwd)))
         (abs-path (concatenate 'string cwd "tests/foo.lisp")))
    (assert-string= "tests/foo.lisp" (%source-file-display abs-path))))

(deftest source-file-display-unrelated-path-returned-unchanged
  "%source-file-display returns the original namestring for paths outside cwd."
  (let ((path "/tmp/unrelated.lisp"))
    (assert-string= path (%source-file-display path))))
