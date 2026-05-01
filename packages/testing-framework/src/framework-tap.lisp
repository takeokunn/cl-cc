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

(defun enable-coverage ()
  "Instrument code for coverage tracking."
  (proclaim '(optimize (sb-cover:store-coverage-data 3))))

(defun disable-coverage ()
  "Disable coverage instrumentation."
  (proclaim '(optimize (sb-cover:store-coverage-data 0))))

(defun %coverage-report-directory ()
  "Return a writable directory for the sb-cover HTML report."
  (uiop:merge-pathnames* #P"cl-cc-coverage/" (uiop:temporary-directory)))

(defun %coverage-report-empty-p (directory)
  "Return T when sb-cover produced an empty HTML index for DIRECTORY."
  (let ((index (merge-pathnames #P"cover-index.html" directory)))
    (and (probe-file index)
         (with-open-file (stream index :direction :input)
           (let ((contents (make-string (file-length stream))))
             (read-sequence contents stream)
             (not (null (search "No code coverage data found" contents))))))))

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
