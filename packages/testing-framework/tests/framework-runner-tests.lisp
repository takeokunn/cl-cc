(in-package :cl-cc/test)

;;; These test the orchestration layer (%run-single-test, %run-tests-sequential,
;;; %effective-worker-count, run-suite, %suite-parallel-p, %test-parallel-safe-p).

(defsuite runner-regression-suite
  :description "Serial regression tests for test runner orchestration"
  :parallel nil
  :parent cl-cc-suite)

(in-suite runner-regression-suite)

(deftest suite-parallel-policy-inherits-from-parent
  "Child suites inherit serial execution policy from ancestors."
  (let ((parent (gensym "ULW-PARENT-"))
        (child (gensym "ULW-CHILD-")))
    (unwind-protect
         (progn
           (setf *suite-registry*
                 (persist-assoc *suite-registry* parent
                                (list :name parent :description "tmp" :parent nil :parallel nil
                                      :before-each '() :after-each '())))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* child
                                (list :name child :description "tmp" :parent parent :parallel t
                                      :before-each '() :after-each '())))
           (assert-false (%suite-parallel-p child))
           (assert-false (%test-parallel-safe-p (list :suite child :depends-on nil))))
      (setf *suite-registry* (persist-remove *suite-registry* child))
      (setf *suite-registry* (persist-remove *suite-registry* parent)))))

(deftest suite-parent-cycle-does-not-hang-discovery-helpers
  "Suite parent cycles terminate in collection, fixture lookup, and parallel checks."
  (let* ((*suite-registry* (persist-empty))
         (*test-registry* (persist-empty))
         (suite-a (gensym "ULW-CYCLE-A-"))
         (suite-b (gensym "ULW-CYCLE-B-"))
         (before-a (lambda () :before-a))
         (before-b (lambda () :before-b))
         (after-a (lambda () :after-a))
         (after-b (lambda () :after-b)))
    (setf *suite-registry*
          (persist-assoc *suite-registry* suite-a
                         (list :name suite-a :description "tmp" :parent suite-b :parallel t
                               :before-each (list before-a) :after-each (list after-a))))
    (setf *suite-registry*
          (persist-assoc *suite-registry* suite-b
                         (list :name suite-b :description "tmp" :parent suite-a :parallel t
                               :before-each (list before-b) :after-each (list after-b))))
    (setf *test-registry*
          (persist-assoc *test-registry* 'ulw-cycle-test-a
                         (list :name 'ulw-cycle-test-a :suite suite-a :tags nil)))
    (setf *test-registry*
          (persist-assoc *test-registry* 'ulw-cycle-test-b
                         (list :name 'ulw-cycle-test-b :suite suite-b :tags nil)))
    (assert-= 2 (length (%collect-all-suite-tests suite-a nil)))
    (multiple-value-bind (before-chain after-chain) (%get-suite-fixtures suite-a)
      (assert-= 2 (length before-chain))
      (assert-= 2 (length after-chain))
      (assert-true (member before-a before-chain :test #'eq))
      (assert-true (member before-b before-chain :test #'eq))
      (assert-true (member after-a after-chain :test #'eq))
      (assert-true (member after-b after-chain :test #'eq)))
    (assert-false (%suite-parallel-p suite-a))
    (assert-false (%test-parallel-safe-p (list :suite suite-a :depends-on nil)))))

(deftest dependency-ordering-moves-dependent-after-prerequisite
  "%order-tests-for-dependencies places a dependent test after its prerequisite."
  (let* ((dependency (list :name 'ulw-dependency :depends-on nil))
         (dependent  (list :name 'ulw-dependent  :depends-on 'ulw-dependency))
         (ordered    (%order-tests-for-dependencies (list dependent dependency))))
    (assert-equal '(ulw-dependency ulw-dependent)
                  (mapcar (lambda (test) (getf test :name)) ordered))))

(deftest dependency-ordering-cycle-break-preserves-prefix
  "%order-tests-for-dependencies falls back to preserving non-dependent tests first."
  (let* ((ordered (%order-tests-for-dependencies
                   (list (list :name 'ulw-a :depends-on 'ulw-b)
                         (list :name 'ulw-x :depends-on nil)
                         (list :name 'ulw-b :depends-on 'ulw-a)))))
    (assert-equal '(ulw-x ulw-a ulw-b)
                  (mapcar (lambda (test) (getf test :name)) ordered))))

(deftest run-single-test-skips-when-dependency-failed
  "A test with a failed dependency is reported as skipped without executing its body."
  (let* ((called nil)
         (test-plist (list :name 'needs-dep
                           :fn (lambda () (setf called t))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on 'upstream
                           :tags nil))
         (result (%run-single-test test-plist 1 (list (list :name 'upstream :status :fail)))))
    (assert-false called)
    (assert-eq :skip (getf result :status))))

(deftest run-single-test-pending-condition-returns-pending-with-reason
  "%run-single-test: a pending-condition signals :pending status with the reason in :detail."
  (let* ((test-plist (list :name 'pending-demo
                           :fn (lambda () (error 'pending-condition :reason "later"))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on nil
                           :tags nil))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :pending (getf result :status))
    (assert-true (search "later" (getf result :detail))))

(deftest effective-test-timeout-bogus-normalizes-to-default
  "%effective-test-timeout returns the default when given a non-integer keyword."
  (assert-= (%default-test-timeout) (%effective-test-timeout (list :timeout :bogus))))

(deftest effective-test-timeout-preserves-valid-integer
  "%effective-test-timeout returns the given positive integer unchanged."
  (assert-equal 7 (%effective-test-timeout (list :timeout 7))))

(deftest effective-test-timeout-preserves-valid-real
  "%effective-test-timeout returns the given positive real unchanged."
  (assert-equal 0.25 (%effective-test-timeout (list :timeout 0.25))))

(deftest effective-test-timeout-nil-normalizes-to-default
  "%effective-test-timeout returns the default when given nil."
  (assert-= (%default-test-timeout) (%effective-test-timeout (list :timeout nil))))

(deftest default-test-timeout-honors-env-var-and-falls-back-to-10
  "%default-test-timeout: unset→10; set to 14→14; set to bogus→10."
  (let ((old (uiop:getenv "CLCC_TEST_TIMEOUT")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "CLCC_TEST_TIMEOUT")
           (assert-= 10 (%default-test-timeout))
           (sb-posix:setenv "CLCC_TEST_TIMEOUT" "14" 1)
           (assert-= 14 (%default-test-timeout))
           (sb-posix:setenv "CLCC_TEST_TIMEOUT" "bogus" 1)
           (assert-= 10 (%default-test-timeout)))
      (if old
          (sb-posix:setenv "CLCC_TEST_TIMEOUT" old 1)
          (sb-posix:unsetenv "CLCC_TEST_TIMEOUT")))))

(deftest default-suite-timeout-honors-env-var-and-falls-back-to-600
  "%default-suite-timeout: unset→600; set to 90→90; set to bogus→600."
  (let ((old (uiop:getenv "CLCC_SUITE_TIMEOUT")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "CLCC_SUITE_TIMEOUT")
           (assert-= 600 (%default-suite-timeout))
           (sb-posix:setenv "CLCC_SUITE_TIMEOUT" "90" 1)
           (assert-= 90 (%default-suite-timeout))
           (sb-posix:setenv "CLCC_SUITE_TIMEOUT" "bogus" 1)
           (assert-= 600 (%default-suite-timeout)))
      (if old
          (sb-posix:setenv "CLCC_SUITE_TIMEOUT" old 1)
          (sb-posix:unsetenv "CLCC_SUITE_TIMEOUT")))))

(deftest suite-timeout-result-returns-true-when-quit-p-nil
  "%suite-timeout-result returns t and emits an error log when quit-p is nil."
  (let* ((err-out (make-string-output-stream))
         (*error-output* err-out)
         (result (%suite-timeout-result 'demo-suite 42 nil)))
    (assert-true result)
    (let ((log (get-output-stream-string err-out)))
      (assert-true (search "DEMO-SUITE" (string-upcase log)))
      (assert-true (search "42" log)))))

(deftest suite-timeout-result-calls-quit-124-when-quit-p-true
  "%suite-timeout-result calls (uiop:quit 124) when quit-p is true."
  (let ((captured nil)
        (err-out (make-string-output-stream)))
    (with-replaced-function (uiop:quit (lambda (&optional code) (setf captured code)))
      (let ((*error-output* err-out))
        (%suite-timeout-result 'other-suite 10 t)))
    (assert-eql 124 captured)
    (assert-true (search "OTHER-SUITE" (string-upcase (get-output-stream-string err-out))))))

  (let* ((test-plist (list :name 'skip-demo
                           :fn (lambda () (error 'skip-condition :reason "not today"))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on nil
                           :tags nil))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :skip (getf result :status))
    (assert-true (search "not today" (getf result :detail)))))

(deftest run-single-test-reports-fixture-setup-errors
  "A before-each fixture error is surfaced as a fixture error result."
  (let ((fixture-suite (gensym "ULW-FIXTURE-SUITE-")))
    (unwind-protect
         (progn
           (setf *suite-registry*
                 (persist-assoc *suite-registry* fixture-suite
                                (list :name fixture-suite
                                      :description "tmp"
                                      :parent nil
                                      :parallel t
                                      :before-each (list (lambda () (error "fixture boom")))
                                      :after-each '())))
           (let* ((test-plist (list :name 'fixture-demo
                                    :fn (lambda () t)
                                    :suite fixture-suite
                                    :timeout nil
                                    :depends-on nil
                                    :tags nil))
                  (result (%run-single-test test-plist 1 '())))
             (assert-eq :fail (getf result :status))
             (assert-true (search "fixture error" (getf result :detail)))
             (assert-true (search "fixture boom" (getf result :detail)))))
      (setf *suite-registry* (persist-remove *suite-registry* fixture-suite)))))

(deftest run-single-test-times-out-sequentially
  "Sequential runner reports timeout failures when a test exceeds its budget."
  ;; Bind *test-runner-mode* to :sequential so sb-ext:with-timeout is applied.
  ;; The global mode is :parallel (set in apps.nix to suppress SIGALRM delivery
  ;; issues), but this test specifically exercises the sequential timeout path.
  (let* ((*test-runner-mode* :sequential)
         (test-plist (list :name 'slow-demo
                           :fn (lambda () (sleep 2))
                           :suite 'cl-cc-unit-suite
                           :timeout 1
                           :depends-on nil
                           :tags nil))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :fail (getf result :status))
    (assert-true (search "timeout after 1 seconds" (getf result :detail)))))

(deftest run-tests-sequential-returns-ordered-results
  "%run-tests-sequential preserves input order and records pass statuses."
  (let ((results (%run-tests-sequential
                  (list (list :name 'first
                              :fn (lambda () t)
                              :suite 'cl-cc-unit-suite
                              :timeout nil
                              :depends-on nil
                              :tags nil
                              :number 1)
                        (list :name 'second
                              :fn (lambda () t)
                              :suite 'cl-cc-unit-suite
                              :timeout nil
                              :depends-on nil
                              :tags nil
                              :number 2)))))
    (assert-equal '(first second)
                  (mapcar (lambda (result) (getf result :name)) results))
    (assert-true (every (lambda (result) (eq :pass (getf result :status))) results))))

(deftest duplicate-test-names-overwrite-registry-entry
  "deftest overwrites an existing test registry entry when the same name is reloaded."
  (let ((*test-registry* (persist-empty))
        (*current-suite* 'cl-cc-unit-suite)
        (test-name (gensym "DUPLICATE-TEST-NAME-")))
    (eval `(deftest ,test-name t))
    (eval `(deftest ,test-name nil))
    (let ((entry (persist-lookup *test-registry* test-name)))
      (assert-true entry)
      (assert-eq test-name (getf entry :name)))))

(deftest duplicate-suite-names-overwrite-registry-entry
  "defsuite overwrites an existing suite registry entry when the same name is reloaded."
  (let ((*suite-registry* (persist-empty))
        (suite-name (gensym "DUPLICATE-SUITE-NAME-")))
    (eval `(defsuite ,suite-name :description "first"))
    (eval `(defsuite ,suite-name :description "second"))
    (let ((entry (persist-lookup *suite-registry* suite-name)))
      (assert-true entry)
      (assert-string= "second" (getf entry :description)))))

(deftest resolve-suite-returns-symbol-and-signals-for-missing-suite
  "%resolve-suite returns existing suites and errors on missing ones."
  (assert-eq 'cl-cc-unit-suite (%resolve-suite :cl-cc/test "CL-CC-UNIT-SUITE"))
  (handler-case
      (progn
        (%resolve-suite :cl-cc/test "MISSING-SUITE")
        (assert-false t))
    (error (e)
      (assert-true (search "Suite CL-CC/TEST::MISSING-SUITE not found"
                           (princ-to-string e))))))

;;; ── New-helper coverage (added after %run-single-test decomposition) ─────

(deftest make-test-result-pass-shape
  "%make-test-result: :pass result has all expected fields and nil detail."
  (let ((r (%make-test-result 'my-test 3 'my-suite :pass nil)))
    (assert-eq 'my-test  (getf r :name))
    (assert-eq :pass     (getf r :status))
    (assert-eq 'my-suite (getf r :suite))
    (assert-= 3          (getf r :number))
    (assert-null         (getf r :detail))))

(deftest make-test-result-fail-carries-detail
  "%make-test-result: :fail result carries the detail string."
  (let ((r (%make-test-result 'my-test 1 'my-suite :fail "oops")))
    (assert-eq :fail      (getf r :status))
    (assert-string= "oops" (getf r :detail))))

(deftest-each format-timeout-detail-rendering
  "%format-timeout-detail renders the configured timeout or the default timeout."
  :cases (("defaulted" nil nil)
          ("numeric"   5   "5 seconds"))
  (timeout expected-substr)
  (assert-true (search (or expected-substr
                           (format nil "~A seconds" (%default-test-timeout)))
                       (%format-timeout-detail timeout))))

(deftest-each count-results-by-status-counts-correctly
  "%count-results-by-status filters by exact status keyword."
  :cases (("pass" :pass 2)
          ("fail" :fail 1)
          ("skip" :skip 0))
  (status expected)
  (let ((results (list (list :status :pass) (list :status :fail) (list :status :pass))))
    (assert-= expected (%count-results-by-status results status))))

(deftest-each path-is-cwd-descendant-cases
  "%path-is-cwd-descendant-p correctly identifies descendant paths."
  :cases (("same"      "/home/user/"      "/home/user/"
           (lambda (path-ns cwd-ns)
             (assert-true (%path-is-cwd-descendant-p path-ns cwd-ns))))
          ("child"     "/home/user/foo/"  "/home/user/"
           (lambda (path-ns cwd-ns)
             (assert-true (%path-is-cwd-descendant-p path-ns cwd-ns))))
          ("sibling"   "/home/other/"     "/home/user/"
           (lambda (path-ns cwd-ns)
             (assert-false (%path-is-cwd-descendant-p path-ns cwd-ns))))
          ("nil-path"  nil                  "/home/user/"
           (lambda (path-ns cwd-ns)
             (assert-false (%path-is-cwd-descendant-p path-ns cwd-ns))))
          ("nil-cwd"   "/home/user/"      nil
           (lambda (path-ns cwd-ns)
             (assert-false (%path-is-cwd-descendant-p path-ns cwd-ns)))))
  (path-ns cwd-ns verify)
  (funcall verify path-ns cwd-ns))

(deftest cpu-count-detect-returns-positive-integer
  "%detect-cpu-count always returns a positive integer."
  (let ((*cpu-count-sources* (list (lambda () 8))))
    (let ((n (%detect-cpu-count)))
    (assert-true (integerp n))
      (assert-true (plusp n)))))

(deftest cpu-count-env-source-returns-integer-or-nil
  "The first *cpu-count-sources* thunk returns a positive integer or nil."
  (let* ((env-source (first *cpu-count-sources*))
         (result (funcall env-source)))
    (assert-true (or (null result) (and (integerp result) (plusp result))))))

(deftest number-tests-annotates-with-index
  "%number-tests adds a :number key (1-based) to each plist in the list."
  (let* ((plists (list '(:name a) '(:name b) '(:name c)))
         (result (%number-tests plists)))
    (assert-= 3 (length result))
    (assert-= 1 (getf (first  result) :number))
    (assert-= 2 (getf (second result) :number))
    (assert-= 3 (getf (third  result) :number))))

(deftest number-tests-empty-returns-nil
  "%number-tests on an empty list returns nil."
  (assert-null (%number-tests nil)))

(deftest cpu-count-command-failure-yields-nil
  "%parse-command-cpu-count returns NIL when the command runner fails."
  (let ((*run-command-output-fn* (lambda (&rest args)
                                   (declare (ignore args))
                                   (error "cpu-count failed"))))
    (assert-null (%parse-command-cpu-count '("fake-cpu-count")))))
