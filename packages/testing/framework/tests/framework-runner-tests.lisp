(in-package :cl-cc/test)

;;; Runner regression tests — extracted from framework-advanced.lisp.
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

(deftest dependency-ordering-cases
  "Dependency ordering: reorders dependent behind prerequisite; fallback preserves confirmed prefix."
  (let* ((dependency (list :name 'ulw-dependency :depends-on nil))
         (dependent (list :name 'ulw-dependent :depends-on 'ulw-dependency))
         (ordered (%order-tests-for-dependencies (list dependent dependency))))
    (assert-equal '(ulw-dependency ulw-dependent)
                  (mapcar (lambda (test) (getf test :name)) ordered)))
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

(deftest run-single-test-skip-pending-cases
  "%run-single-test: pending condition→:pending with reason; skip condition→:skip with reason."
  (let* ((test-plist (list :name 'pending-demo
                           :fn (lambda () (error 'pending-condition :reason "later"))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on nil
                           :tags nil))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :pending (getf result :status))
    (assert-true (search "later" (getf result :detail))))

(deftest effective-test-timeout-cases
  "%effective-test-timeout normalizes nil, invalid values, and positive integers."
(assert-= (%default-test-timeout) (%effective-test-timeout (list :timeout :bogus)))
  (assert-equal 7 (%effective-test-timeout (list :timeout 7)))
  (assert-equal (%default-test-timeout)
                (%effective-test-timeout (list :timeout nil))))

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
  (let* ((test-plist (list :name 'slow-demo
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

(deftest make-test-result-plist-shape-cases
  "%make-test-result: :pass has nil detail and all fields; :fail carries detail string."
  (let ((r (%make-test-result 'my-test 3 'my-suite :pass nil)))
    (assert-eq 'my-test  (getf r :name))
    (assert-eq :pass     (getf r :status))
    (assert-eq 'my-suite (getf r :suite))
    (assert-= 3          (getf r :number))
    (assert-null         (getf r :detail)))
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

(deftest count-results-by-status-counts-correctly
  "%count-results-by-status filters by exact status keyword."
  (let ((results (list (list :status :pass) (list :status :fail) (list :status :pass))))
    (assert-= 2 (%count-results-by-status results :pass))
    (assert-= 1 (%count-results-by-status results :fail))
    (assert-= 0 (%count-results-by-status results :skip))))

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

(deftest cpu-count-cases
  "%detect-cpu-count returns positive integer; env source returns positive integer or nil."
  (let ((n (%detect-cpu-count)))
    (assert-true (integerp n))
    (assert-true (plusp n)))
  (let ((env-source (first *cpu-count-sources*)))
    (uiop:with-temporary-file (:pathname tmp :keep nil)
      (declare (ignore tmp)))
    (let ((result (funcall env-source)))
      (assert-true (or (null result) (and (integerp result) (plusp result)))))))

(deftest cpu-count-command-timeout-yields-nil
  "%parse-command-cpu-count returns NIL when the command runner times out."
  (with-replaced-function (uiop:run-program
                           (lambda (&rest args)
                             (declare (ignore args))
                             (error 'sb-ext:timeout)))
    (assert-null (%parse-command-cpu-count '("fake-cpu-count")))))
