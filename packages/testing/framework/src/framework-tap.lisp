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
;;; %timings-output-path is defined in framework.lisp
;;; %write-timings-tsv is defined in framework.lisp
;;; %emit-slowest-summary is defined in framework.lisp

;;; ------------------------------------------------------------
;;; Coverage Helpers (FR-035) — sb-cover integration
;;; ------------------------------------------------------------

(defun enable-coverage ()
  "Instrument code for coverage tracking."
  #+sbcl
  (proclaim '(optimize (sb-cover:store-coverage-data 3)))
  #-sbcl
  nil)

(defun disable-coverage ()
  "Disable coverage instrumentation."
  #+sbcl
  (proclaim '(optimize (sb-cover:store-coverage-data 0)))
  #-sbcl
  nil)

(defun %coverage-report-directory ()
  "Return a writable directory for the sb-cover HTML report.
Uses uiop:temporary-directory so sandboxed test builds (e.g. Nix)
don't try to write to a read-only `/tmp`."
  (uiop:ensure-directory-pathname
   (merge-pathnames "cl-cc-coverage/" (uiop:temporary-directory))))

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
  #+sbcl
  (let ((dir (%coverage-report-directory)))
    (ensure-directories-exist dir)
    (sb-cover:report dir)
    (when (%coverage-report-empty-p dir)
      (error "Coverage report at ~A is empty; sb-cover instrumentation did not capture any executable code."
             (namestring (merge-pathnames #P"cover-index.html" dir))))
    (format t "# Coverage report written to ~A (cl-cc-coverage)~%"
            (namestring dir)))
  #-sbcl
  (format t "# Coverage: only available on SBCL~%"))

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
  "Emit TAP header metadata for a run-suite invocation."
  (format t "# Seed: ~A~%" actual-seed)
  (format t "# Workers: ~A~%" nworkers)
  (format t "TAP version 13~%")
  (format t "1..~A~%" (* n repeat))
  (force-output))

(defun %count-results-by-status (flat-results status)
  "Count result plists in FLAT-RESULTS whose :status equals STATUS."
  (count status flat-results :key (lambda (r) (getf r :status))))

(defun %print-result-summary (flat-results)
  "Print a TAP-style result summary and return true when any test failed."
  (let* ((pass-count (%count-results-by-status flat-results :pass))
         (fail-count (%count-results-by-status flat-results :fail))
         (skip-count (%count-results-by-status flat-results :skip))
         (total (length flat-results))
         (bar "#  ---------------------------------------------------")
         (any-fail (> fail-count 0)))
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
    (format t "#~%")
    any-fail))

(defun %emit-postrun-artifacts (flat-results)
  "Write timings TSV and slowest summary; log but do not propagate errors."
  (handler-case (%write-timings-tsv flat-results (%timings-output-path))
    (error (e) (format *error-output* "# WARN: test-timings emission failed: ~A~%" e)))
  (handler-case (%emit-slowest-summary flat-results 20)
    (error (e) (format *error-output* "# WARN: slowest-summary emission failed: ~A~%" e))))
