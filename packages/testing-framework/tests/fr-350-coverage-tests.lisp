(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defun %fr-350-temp-dir (name)
  (uiop:merge-pathnames*
   (format nil "cl-cc-fr-350-~(~A~)-~D-~D/" name (get-universal-time) (random 1000000))
   (uiop:temporary-directory)))

(defun %fr-350-read-file (path)
  (with-open-file (stream path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun %fr-350-clean-dir (dir)
  (when (and dir (probe-file dir))
    (uiop:delete-directory-tree dir :validate t)))

(defparameter *fr-350-sample-entries*
  (list (list :source-file "packages/testing-framework/tests/sample.lisp"
              :lines (list (cons 10 1) (cons 11 0))
              :branches (list (list 10 0 0 1)
                              (list :line 10 :block 0 :branch 1 :hits 0))))
  "Stable coverage report fixture used by FR-350 tests.")

(deftest fr-350-enable-disable-coverage-is-idempotent
  "FR-350: coverage enable/disable API calls are idempotent and observable."
  :tags '(:fr-350)
  (disable-coverage)
  (assert-false (coverage-enabled-p))
  (enable-coverage)
  (enable-coverage)
  (assert-true (coverage-enabled-p))
  (disable-coverage)
  (disable-coverage)
  (assert-false (coverage-enabled-p)))

(deftest fr-350-cli-coverage-flag-is-accepted
  "FR-350: CLI argument parsing accepts the --coverage compile flag."
  :tags '(:fr-350)
  (let ((parsed (cl-cc/cli:parse-args '("compile" "file.lisp" "--coverage" "true"))))
    (assert-true (cl-cc/cli:flag parsed "--coverage"))))

(deftest fr-351-cli-mcdc-flag-is-accepted
  "FR-351: CLI argument parsing accepts --coverage=mcdc."
  :tags '(:fr-351)
  (let ((parsed (cl-cc/cli:parse-args '("compile" "file.lisp" "--coverage=mcdc"))))
    (assert-string= "mcdc" (cl-cc/cli:flag parsed "--coverage"))))

(deftest fr-351-cli-bare-coverage-flag-is-still-accepted
  "FR-351: bare --coverage remains a boolean coverage request."
  :tags '(:fr-351)
  (let ((parsed (cl-cc/cli:parse-args '("compile" "file.lisp" "--coverage"))))
    (assert-true (cl-cc/cli:flag parsed "--coverage"))))

(deftest fr-351-collects-and-or-mcdc-decisions
  "FR-351: MC/DC collection records independent effects for AND/OR conditions."
  :tags '(:fr-351)
  (let* ((coverage (cl-cc/compile:collect-mcdc-coverage '((and a b c) (or a b c))))
         (decisions (getf coverage :decisions)))
    (assert-eq :mcdc (getf coverage :mode))
    (assert-= 2 (length decisions))
    (dolist (decision decisions)
      (assert-= 3 (length (getf decision :conditions)))
      (dolist (pair (getf decision :pairs))
        (assert-true (getf pair :independent-effect))))))

(deftest fr-351-pipeline-accepts-mcdc-coverage
  "FR-351: pipeline compilation accepts :coverage :mcdc for MC/DC."
  :tags '(:fr-351)
  (let ((result (cl-cc:compile-string "(and t t nil)" :target :vm :coverage :mcdc)))
    (assert-true (typep result 'cl-cc:compilation-result))
    (assert-eq :mcdc (getf (cl-cc/compile:compilation-result-coverage result) :mode))))

(deftest fr-350-pipeline-accepts-coverage-keyword
  "FR-350: pipeline compilation accepts the :coverage keyword propagated from CLI flags."
  :tags '(:fr-350)
  (let ((result (cl-cc:compile-string "(+ 1 2)" :target :vm :coverage t)))
    (assert-true (typep result 'cl-cc:compilation-result))))

(deftest fr-350-report-directory-and-empty-html-detection
  "FR-350: dependency-free HTML report generation creates the expected index and detects empty reports."
  :tags '(:fr-350)
  (let ((dir (%fr-350-temp-dir 'empty-html)))
    (unwind-protect
         (progn
           (generate-coverage-report :directory dir :entries nil)
           (assert-true (coverage-report-exists-p dir))
           (assert-true (coverage-report-empty-p dir))
           (assert-true (search "No code coverage data found"
                                (%fr-350-read-file (coverage-report-index-path dir)))))
      (%fr-350-clean-dir dir))))

(deftest fr-350-non-empty-html-report-is-not-empty
  "FR-350: HTML report generation records line/branch rows for non-empty coverage entries."
  :tags '(:fr-350)
  (let ((dir (%fr-350-temp-dir 'non-empty-html)))
    (unwind-protect
         (progn
           (generate-coverage-report :directory dir :entries *fr-350-sample-entries*)
           (assert-true (coverage-report-exists-p dir))
           (assert-false (coverage-report-empty-p dir))
           (let ((html (%fr-350-read-file (coverage-report-index-path dir))))
             (assert-true (search "packages/testing-framework/tests/sample.lisp" html))
             (assert-true (search "<table>" html))))
      (%fr-350-clean-dir dir))))

(deftest fr-350-lcov-output-shape-includes-line-and-branch-records
  "FR-350: LCOV writer emits stable tracefile records for line and branch coverage."
  :tags '(:fr-350)
  (let* ((dir (%fr-350-temp-dir 'lcov))
         (lcov (merge-pathnames #P"coverage.lcov" dir)))
    (unwind-protect
         (progn
           (write-lcov-report *fr-350-sample-entries* lcov :test-name "fr-350")
           (let ((text (%fr-350-read-file lcov)))
             (assert-true (search "TN:fr-350" text))
             (assert-true (search "SF:packages/testing-framework/tests/sample.lisp" text))
             (assert-true (search "DA:10,1" text))
             (assert-true (search "DA:11,0" text))
             (assert-true (search "LF:2" text))
             (assert-true (search "LH:1" text))
             (assert-true (search "BRDA:10,0,0,1" text))
             (assert-true (search "BRDA:10,0,1,0" text))
             (assert-true (search "BRF:2" text))
             (assert-true (search "BRH:1" text))
             (assert-true (search "end_of_record" text))))
      (%fr-350-clean-dir dir))))

(deftest fr-351-mcdc-report-is-written-next-to-lcov
  "FR-351: coverage report generation writes MC/DC artifacts alongside LCOV."
  :tags '(:fr-351)
  (let* ((dir (%fr-350-temp-dir 'mcdc-report))
         (lcov (merge-pathnames #P"coverage.lcov" dir))
         (mcdc (merge-pathnames #P"coverage-mcdc.txt" dir))
         (coverage (cl-cc/compile:collect-mcdc-coverage '((and a b c) (or a b c)))))
    (unwind-protect
         (progn
           (generate-coverage-report :directory dir
                                     :entries *fr-350-sample-entries*
                                     :lcov-path lcov
                                     :mcdc-coverage coverage)
           (assert-true (probe-file lcov))
           (assert-true (probe-file mcdc))
           (let ((text (%fr-350-read-file mcdc)))
             (assert-true (search "CL-CC MC/DC Coverage Report" text))
             (assert-true (search "Decision:" text))
             (assert-true (search "Condition 0" text))))
      (%fr-350-clean-dir dir))))

(deftest fr-350-with-coverage-cleans-up-and-writes-reports
  "FR-350: with-coverage enables instrumentation for the body, writes reports, and disables on exit."
  :tags '(:fr-350)
  (let* ((dir (%fr-350-temp-dir 'with-coverage))
         (lcov (merge-pathnames #P"coverage.lcov" dir)))
    (unwind-protect
         (progn
           (disable-coverage)
           (assert-equal :body-result
                         (with-coverage (:report-directory dir
                                         :lcov-path lcov
                                         :entries *fr-350-sample-entries*)
                           (assert-true (coverage-enabled-p))
                           :body-result))
           (assert-false (coverage-enabled-p))
           (assert-true (coverage-report-exists-p dir))
           (assert-true (probe-file lcov)))
      (disable-coverage)
      (%fr-350-clean-dir dir))))

(deftest fr-350-with-coverage-cleans-up-after-error
  "FR-350: with-coverage disables instrumentation even when BODY signals."
  :tags '(:fr-350)
  (disable-coverage)
  (handler-case
      (with-coverage ()
        (assert-true (coverage-enabled-p))
        (error "synthetic FR-350 failure"))
    (error () nil))
  (assert-false (coverage-enabled-p)))

(deftest fr-350-run-suite-coverage-mode-is-sequential-and-reports
  "FR-350: run-suite :coverage t disables parallel execution and invokes coverage reporting without changing quit behavior."
  :tags '(:fr-350)
  (let ((root (gensym "FR-350-ROOT-"))
        (test-name (gensym "FR-350-TEST-"))
        (report-called nil)
        (saved-enable (symbol-function 'enable-coverage))
        (saved-disable (symbol-function 'disable-coverage))
        (saved-report (symbol-function '%print-coverage-report))
        (saved-warm (symbol-function 'cl-cc::warm-stdlib-cache)))
    (unwind-protect
         (progn
           (setf *suite-registry*
                 (persist-assoc *suite-registry* root
                                (list :name root :description "tmp" :parent nil :parallel t
                                      :before-each '() :after-each '())))
           (setf *test-registry*
                 (persist-assoc *test-registry* test-name
                                (list :name test-name
                                      :suite root
                                      :fn (lambda () t)
                                      :skip nil
                                      :xfail nil
                                      :depends-on nil
                                      :timeout nil
                                      :tags '(:fr-350-private))))
           (setf (symbol-function 'enable-coverage) (lambda () (setf *coverage-enabled* t)))
           (setf (symbol-function 'disable-coverage) (lambda () (setf *coverage-enabled* nil)))
           (setf (symbol-function '%print-coverage-report)
                 (lambda (results)
                   (setf report-called (and (= 1 (length results))
                                            (eq :pass (getf (first results) :status))))
                   (format t "# mocked coverage report~%")))
           (setf (symbol-function 'cl-cc::warm-stdlib-cache) (lambda () nil))
           (let ((output (with-output-to-string (stream)
                           (let ((*standard-output* stream))
                             (assert-false
                              (run-suite root
                                         :coverage t
                                         :parallel t
                                         :random nil
                                         :workers 8
                                         :warm-stdlib nil
                                         :quit-p nil))))))
             (assert-true report-called)
             (assert-false (coverage-enabled-p))
             (assert-true (search "Coverage mode: parallel disabled" output))
             (assert-true (search "Workers: 1" output))
             (assert-true (search "Coverage report enabled" output))
             (assert-true (search "mocked coverage report" output))))
      (setf (symbol-function 'enable-coverage) saved-enable)
      (setf (symbol-function 'disable-coverage) saved-disable)
      (setf (symbol-function '%print-coverage-report) saved-report)
      (setf (symbol-function 'cl-cc::warm-stdlib-cache) saved-warm)
      (setf *test-registry* (persist-remove *test-registry* test-name))
      (setf *suite-registry* (persist-remove *suite-registry* root))
      (disable-coverage))))
