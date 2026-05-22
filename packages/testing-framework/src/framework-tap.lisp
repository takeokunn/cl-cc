;;;; tests/framework-tap.lisp — CL-CC Test Framework (TAP output + coverage helpers)
;;;; Coverage instrumentation (sb-cover), TAP header/summary, timings artifact emission.

;;; Load sb-cover contrib before the reader encounters sb-cover: symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (ignore-errors (require :sb-cover)))

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Notes on shared state (defined in framework.lisp)
;;; ------------------------------------------------------------

;;; *tap-mutex* is defined in framework.lisp

;;; ------------------------------------------------------------
;;; Coverage Helpers (FR-035) — sb-cover integration
;;; ------------------------------------------------------------

(defvar *coverage-enabled* nil
  "True while CL-CC test coverage instrumentation is enabled.")

(defvar *coverage-report-directory* nil
  "Optional override for the default coverage report directory.")

(defun enable-coverage ()
  "Enable host coverage instrumentation for subsequently compiled code.
The operation is idempotent and uses SBCL's sb-cover as the low-level engine."
  (proclaim '(optimize (sb-cover:store-coverage-data 3)))
  (setf *coverage-enabled* t))

(defun disable-coverage ()
  "Disable host coverage instrumentation.  The operation is idempotent."
  (proclaim '(optimize (sb-cover:store-coverage-data 0)))
  (setf *coverage-enabled* nil))

(defun coverage-enabled-p ()
  "Return true when CL-CC coverage mode is currently enabled."
  *coverage-enabled*)

(defun %coverage-report-directory ()
  "Return a writable directory for the sb-cover HTML report."
  (or *coverage-report-directory*
      (uiop:merge-pathnames* #P"cl-cc-coverage/" (uiop:temporary-directory))))

(defun coverage-report-index-path (&optional (directory (%coverage-report-directory)))
  "Return the expected sb-cover HTML index path for DIRECTORY."
  (merge-pathnames #P"cover-index.html" directory))

(defun coverage-report-exists-p (&optional (directory (%coverage-report-directory)))
  "Return true when DIRECTORY contains an sb-cover HTML index."
  (not (null (probe-file (coverage-report-index-path directory)))))

(defun %coverage-report-empty-p (directory)
  "Return T when sb-cover produced an empty HTML index for DIRECTORY."
  (let ((index (coverage-report-index-path directory)))
    (and (probe-file index)
         (with-open-file (stream index :direction :input)
           (let ((contents (make-string (file-length stream))))
             (read-sequence contents stream)
             (not (null (search "No code coverage data found" contents))))))))

(defun coverage-report-empty-p (&optional (directory (%coverage-report-directory)))
  "Return true when DIRECTORY contains an empty/no-data coverage report."
  (%coverage-report-empty-p directory))

(defun %write-simple-coverage-html-index (directory entries)
  "Write a minimal dependency-free HTML coverage index for ENTRIES.
This helper is intentionally small and stable for tests and non-sb-cover callers;
the normal runner still uses sb-cover:report for instrumented code."
  (ensure-directories-exist directory)
  (let ((index (coverage-report-index-path directory)))
    (with-open-file (stream index
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "<!doctype html><html><head><title>CL-CC Coverage</title></head><body>~%")
      (format stream "<h1>CL-CC Coverage</h1>~%")
      (if entries
          (progn
            (format stream "<table><thead><tr><th>Source</th><th>Lines</th><th>Branches</th></tr></thead><tbody>~%")
            (dolist (entry entries)
              (format stream "<tr><td>~A</td><td>~D</td><td>~D</td></tr>~%"
                      (getf entry :source-file)
                      (length (getf entry :lines))
                      (length (getf entry :branches))))
            (format stream "</tbody></table>~%"))
          (format stream "<p>No code coverage data found</p>~%"))
      (format stream "</body></html>~%"))
    index))

(defun %branch-hit-count (branch)
  (cond
    ((consp branch)
     (or (getf branch :hits)
         (fourth branch)
         0))
    (t 0)))

(defun %write-lcov-line (stream line)
  (destructuring-bind (line-number . hits) line
    (format stream "DA:~D,~D~%" line-number (or hits 0))))

(defun %write-lcov-branch (stream branch)
  (if (and (consp branch) (keywordp (first branch)))
      (format stream "BRDA:~D,~D,~D,~A~%"
              (getf branch :line 0)
              (getf branch :block 0)
              (getf branch :branch 0)
              (or (getf branch :hits) "-"))
      (destructuring-bind (line block branch-id &optional hits) branch
        (format stream "BRDA:~D,~D,~D,~A~%" line block branch-id (or hits "-")))))

(defun write-lcov-report (entries output-path &key (test-name "cl-cc"))
  "Write ENTRIES to OUTPUT-PATH using LCOV tracefile syntax.
Each entry is a plist with :SOURCE-FILE, optional :LINES as (LINE . HITS)
pairs, and optional :BRANCHES as (LINE BLOCK BRANCH HITS) lists or plists."
  (ensure-directories-exist output-path)
  (with-open-file (stream output-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (entry entries)
      (let* ((lines (copy-list (getf entry :lines)))
             (branches (copy-list (getf entry :branches)))
             (line-count (length lines))
             (line-hit-count (count-if (lambda (line) (plusp (or (cdr line) 0))) lines))
             (branch-count (length branches))
             (branch-hit-count (count-if (lambda (branch) (plusp (%branch-hit-count branch))) branches)))
        (format stream "TN:~A~%" test-name)
        (format stream "SF:~A~%" (getf entry :source-file))
        (dolist (line lines) (%write-lcov-line stream line))
        (format stream "LF:~D~%LH:~D~%" line-count line-hit-count)
        (dolist (branch branches) (%write-lcov-branch stream branch))
        (format stream "BRF:~D~%BRH:~D~%end_of_record~%" branch-count branch-hit-count))))
  output-path)

(defun %mcdc-report-path-from-lcov (lcov-path)
  "Return the default MC/DC report path next to LCOV-PATH."
  (merge-pathnames #P"coverage-mcdc.txt"
                   (make-pathname :defaults (pathname lcov-path)
                                  :name nil :type nil)))

(defun write-mcdc-report (coverage output-path)
  "Write human-readable MC/DC COVERAGE metadata to OUTPUT-PATH."
  (ensure-directories-exist output-path)
  (with-open-file (stream output-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "CL-CC MC/DC Coverage Report~%")
    (format stream "Mode: ~A~%~%" (getf coverage :mode))
    (dolist (decision (getf coverage :decisions))
      (format stream "Decision: ~S~%" (getf decision :form))
      (format stream "Operator: ~A~%" (getf decision :operator))
      (format stream "Path: ~S~%" (getf decision :path))
      (format stream "Conditions: ~S~%" (getf decision :conditions))
      (dolist (pair (getf decision :pairs))
        (format stream "  Condition ~D (~S): ~:[NO~;YES~]~%"
                (getf pair :condition-index)
                (getf pair :condition)
                (getf pair :independent-effect))
        (format stream "    false row ~S => ~S~%"
                (getf pair :false-row)
                (getf pair :false-outcome))
        (format stream "    true  row ~S => ~S~%"
                (getf pair :true-row)
                (getf pair :true-outcome)))
      (format stream "~%")))
  output-path)

(defun generate-coverage-report (&key
                                    (directory (%coverage-report-directory))
                                    entries
                                    lcov-path
                                    mcdc-coverage
                                    mcdc-path
                                    (html t))
  "Generate stable CL-CC coverage artifacts from ENTRIES.
When HTML is true, writes a small cover-index.html.  When LCOV-PATH is supplied,
writes an LCOV tracefile.  Returns DIRECTORY."
  (when html
    (%write-simple-coverage-html-index directory entries))
  (when lcov-path
    (write-lcov-report entries lcov-path))
  (when mcdc-coverage
    (write-mcdc-report mcdc-coverage
                       (or mcdc-path
                           (and lcov-path (%mcdc-report-path-from-lcov lcov-path))
                           (merge-pathnames #P"coverage-mcdc.txt" directory))))
  directory)

(defmacro with-coverage (options &body body)
  "Run BODY with coverage enabled and always disable it afterward.
OPTIONS is a plist accepting :REPORT-DIRECTORY, :LCOV-PATH and :ENTRIES for
stable CL-CC report artifact generation around the covered body."
  (let ((report-directory (getf options :report-directory))
        (lcov-path (getf options :lcov-path))
        (entries (getf options :entries))
        (mcdc-coverage (getf options :mcdc-coverage))
        (mcdc-path (getf options :mcdc-path)))
    `(unwind-protect
          (progn
            (enable-coverage)
            (multiple-value-prog1
                (progn ,@body)
              (when (or ,report-directory ,lcov-path ,entries)
                 (generate-coverage-report :directory (or ,report-directory (%coverage-report-directory))
                                           :entries ,entries
                                           :lcov-path ,lcov-path
                                           :mcdc-coverage ,mcdc-coverage
                                           :mcdc-path ,mcdc-path))))
       (disable-coverage))))

(defun %print-coverage-report (covered-modules)
  "Print sb-cover coverage report for covered modules."
  (declare (ignore covered-modules))
  (let ((dir (%coverage-report-directory)))
    (ensure-directories-exist dir)
    (sb-cover:report dir)
    (when (%coverage-report-empty-p dir)
      (error "Coverage report at ~A is empty; sb-cover instrumentation did not capture any executable code."
             (namestring (merge-pathnames #P"cover-index.html" dir))))
    (format t "# Coverage report written to ~A (cl-cc-coverage)~%"
            (namestring dir))))

(defun %status-keyword-to-string (status)
  "Map an internal result status keyword to its frozen TSV token."
  (case status
    (:pass "passed")
    (:fail "failed")
    (:skip "skipped")
    (:pending "pending")
    (:errored "errored")
    (:timed-out "timed-out")
    (t (string-downcase (symbol-name status)))))

(defun %path-contains-parent-segment-p (raw)
  "Return T if RAW contains a `..` path segment delimited by / or \\."
  (let ((len (length raw)))
    (loop for i from 0 below (- len 1)
          thereis
          (and (char= (char raw i) #\.)
               (char= (char raw (1+ i)) #\.)
               (or (zerop i)
                   (let ((prev (char raw (1- i))))
                     (or (char= prev #\/) (char= prev #\\))))
               (or (= (+ i 2) len)
                   (let ((next (char raw (+ i 2))))
                     (or (char= next #\/) (char= next #\\))))))))

(defun %path-is-cwd-descendant-p (path-ns cwd-ns)
  "Return T when PATH-NS is a path string that starts with CWD-NS."
  (and path-ns cwd-ns
       (>= (length path-ns) (length cwd-ns))
       (string= path-ns cwd-ns :end1 (length cwd-ns))))

(defun %timings-output-path ()
  "Resolve the TSV output path honoring CLCC_TIMINGS_FILE override."
  (let ((raw     (uiop:getenv "CLCC_TIMINGS_FILE"))
        (default "./test-timings.tsv"))
    (when (or (null raw) (zerop (length raw)))
      (return-from %timings-output-path default))
    (when (%path-contains-parent-segment-p raw)
      (warn "CLCC_TIMINGS_FILE rejected: contains `..` segment — falling back to default")
      (return-from %timings-output-path default))
    (unless (uiop:absolute-pathname-p raw)
      (return-from %timings-output-path raw))
    (let* ((cwd-ns   (ignore-errors (namestring (truename (uiop:getcwd)))))
           (canon    (ignore-errors (uiop:truenamize raw)))
           (canon-ns (and canon (namestring canon))))
      (cond
        ((null canon-ns)
         (let* ((parent    (make-pathname :defaults (pathname raw) :name nil :type nil))
                (parent-ns (ignore-errors (namestring (uiop:truenamize parent)))))
           (if (%path-is-cwd-descendant-p parent-ns cwd-ns)
               raw
               (progn
                 (warn "CLCC_TIMINGS_FILE rejected: not a descendant of cwd — falling back to default")
                 default))))
        ((%path-is-cwd-descendant-p canon-ns cwd-ns) raw)
        (t
         (warn "CLCC_TIMINGS_FILE rejected: not a descendant of cwd — falling back to default")
         default)))))

(defun %tsv-sanitize (s)
  "Replace TSV-breaking characters (Tab/Newline/Return) with a single space."
  (if s
      (substitute-if #\Space (lambda (c) (find c '(#\Tab #\Newline #\Return))) s)
      ""))

(defun %write-timings-tsv (results path)
  "Write RESULTS (a flat list of test result plists) to PATH as TSV."
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (r results)
      (let* ((suite      (getf r :suite))
             (name       (getf r :name))
             (duration   (or (getf r :duration-ns) 0))
             (status     (getf r :status))
             (batch-id   (getf r :batch-id))
             (suite-str  (%tsv-sanitize (if suite (symbol-name suite) "")))
             (name-str   (%tsv-sanitize (if name (symbol-name name) "")))
             (status-str (%status-keyword-to-string status))
             (batch-str  (if batch-id (format nil "~D" batch-id) "-")))
        (format stream "~A~C~A~C~D~C~A~C~A~%"
                suite-str #\Tab
                name-str #\Tab
                duration #\Tab
                status-str #\Tab
                batch-str)))))

(defun %emit-slowest-summary (results &optional (n 20))
  "Print the top N slowest tests by :duration-ns (descending)."
  (let* ((timed (remove-if-not (lambda (r) (getf r :duration-ns)) results))
         (sorted (sort (copy-list timed) #'>
                       :key (lambda (r) (getf r :duration-ns))))
         (top (subseq sorted 0 (min n (length sorted)))))
    (format t "# Top ~A slowest tests:~%" n)
    (dolist (r top)
      (let* ((name (getf r :name))
             (suite (getf r :suite))
             (ns (getf r :duration-ns))
             (ms (/ ns 1000000.0d0)))
        (format t "#   ~A (~,3Fms) ~A~%"
                name ms (or suite "-"))))
    (force-output)))

(defun %load-prior-timings (&optional (path (%timings-output-path)))
  "Return a hash-table mapping test-name-string to duration-ns from a prior TSV run."
  (when (probe-file path)
    (handler-case
        (let ((ht (make-hash-table :test #'equal :size 8192)))
          (with-open-file (s path :direction :input)
            (loop for line = (read-line s nil nil)
                  while line
                  unless (zerop (length line))
                    do (let ((fields '())
                             (start 0))
                         (loop for pos = (position #\Tab line :start start)
                               do (push (subseq line start (or pos (length line))) fields)
                               while pos
                               do (setf start (1+ pos)))
                         (setf fields (nreverse fields))
                         (when (>= (length fields) 3)
                           (let ((name-str (second fields))
                                 (dur (ignore-errors (parse-integer (third fields)))))
                             (when (and (plusp (length name-str)) dur (plusp dur))
                               (let ((existing (gethash name-str ht 0)))
                                 (when (> dur existing)
                                   (setf (gethash name-str ht) dur)))))))))
          ht)
      (error (e)
        (format *error-output* "# WARN: could not load prior timings from ~A: ~A~%" path e)
        nil))))

(defun %print-tap-header (n repeat actual-seed nworkers)
  "Emit TAP header. Compact mode suppresses TAP version and plan lines."
  (if (%verbose-tap-p)
      (progn
        (format t "# Seed: ~A~%" actual-seed)
        (format t "# Workers: ~A~%" nworkers)
        (format t "TAP version 13~%")
        (format t "1..~A~%" (* n repeat)))
      (format t "# Seed: ~A  Workers: ~A~%" actual-seed nworkers))
  (force-output))

(defun %count-results-by-status (flat-results status)
  "Count result plists in FLAT-RESULTS whose :status equals STATUS."
  (count status flat-results :key (lambda (r) (getf r :status))))

(defun %print-result-summary (flat-results)
  "Print result summary. Compact mode: one line. Verbose: full TAP table.
Returns true when any test failed."
  (let* ((pass-count (%count-results-by-status flat-results :pass))
         (fail-count (%count-results-by-status flat-results :fail))
         (skip-count (%count-results-by-status flat-results :skip))
         (total      (length flat-results))
         (any-fail   (> fail-count 0)))
    (if (%verbose-tap-p)
        (let ((bar "#  ---------------------------------------------------"))
          (format t "#~%~A~%#  Test Results~%~A~%" bar bar)
          (format t "#    PASS  ~4D~%" pass-count)
          (format t "#    FAIL  ~4D~%" fail-count)
          (when (> skip-count 0)
            (format t "#    SKIP  ~4D~%" skip-count))
          (format t "#   -------~%#   TOTAL  ~4D~%~A~%" total bar)
          (when any-fail
            (format t "#~%#  Failed tests:~%")
            (dolist (r (sort (remove-if-not (lambda (r) (eq (getf r :status) :fail)) flat-results)
                             #'< :key (lambda (r) (getf r :number))))
              (format t "#    [~4D] ~A~%" (getf r :number) (getf r :name)))
            (format t "~A~%" bar))
          (format t "#~%"))
        (if (> skip-count 0)
            (format t "# ~A passed  ~A failed  ~A skipped~%" pass-count fail-count skip-count)
            (format t "# ~A passed  ~A failed~%" pass-count fail-count)))
    any-fail))

(defun %emit-postrun-artifacts (flat-results)
  "Write timings TSV and slowest summary; log but do not propagate errors."
  (handler-case (%write-timings-tsv flat-results (%timings-output-path))
    (error (e) (format *error-output* "# WARN: test-timings emission failed: ~A~%" e)))
  (handler-case (%emit-slowest-summary flat-results 20)
    (error (e) (format *error-output* "# WARN: slowest-summary emission failed: ~A~%" e))))
