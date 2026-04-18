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
             (assert-true (search "# Workers: 1" output))))
      (setf (symbol-function 'cl-cc::warm-stdlib-cache) original-warm-stdlib-cache)
      (setf (symbol-function 'uiop:quit) original-quit)
      (setf *test-registry* (persist-remove *test-registry* test-name))
      (setf *suite-registry* (persist-remove *suite-registry* serial-suite))
      (setf *suite-registry* (persist-remove *suite-registry* root)))))

(deftest canonical-suite-taxonomy-matches-runner-contract
  "The canonical runner exposes unit, integration, and e2e suites under the root taxonomy."
  (assert-eq 'cl-cc-suite
             (getf (persist-lookup *suite-registry* 'cl-cc-unit-suite) :parent))
  (assert-eq 'cl-cc-suite
             (getf (persist-lookup *suite-registry* 'cl-cc-integration-suite) :parent))
  (assert-eq 'cl-cc-suite
             (getf (persist-lookup *suite-registry* 'cl-cc-e2e-suite) :parent)))

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
