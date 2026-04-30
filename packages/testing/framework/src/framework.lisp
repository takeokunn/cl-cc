;;;; tests/framework.lisp — CL-CC Custom Test Framework (Core TAP + canonical suites)
;;;; Zero external dependencies. Replaces FiveAM.
;;;;
;;;; Load order note:
;;;;   - framework-conditions-state.lisp owns conditions + dynamic state
;;;;   - framework-definitions.lisp owns deftest/defsuite/fixtures
;;;;   - this file keeps TAP output and canonical suite declarations

(in-package :cl-cc/test)

;;; ─────────────────────────────────────────────────────────────────────────
;;; TAP Output
;;; ─────────────────────────────────────────────────────────────────────────

(defun %tap-print (line)
  "Print a TAP line with mutex protection."
  (sb-thread:with-mutex (*tap-mutex*)
    (format t "~A~%" line)
    (force-output)))

(defun %verbose-tap-p ()
  "Return T when CLCC_VERBOSE_TAP is set to a non-empty value."
  (let ((v (uiop:getenv "CLCC_VERBOSE_TAP")))
    (and v (not (zerop (length v))))))

(defun %source-file-display (path)
  "Return PATH as a relative string when under cwd, else its namestring."
  (when (null path) (return-from %source-file-display nil))
  (let* ((ns  (if (pathnamep path) (namestring path) (princ-to-string path)))
         (cwd (ignore-errors (namestring (uiop:getcwd)))))
    (if (and cwd (>= (length ns) (length cwd))
             (string= ns cwd :end1 (length cwd)))
        (subseq ns (length cwd))
        ns)))

(defun %detail-inject-context (detail suite source-file duration-ms)
  "Build compact failure block: inject suite/file after YAML '---', then inject duration."
  (let ((base (if detail
                  (%detail-inject-duration detail duration-ms)
                  (%format-minimal-yaml-duration duration-ms))))
    (if (and (>= (length base) 6)
             (string= base "  ---" :end1 5)
             (char= (char base 5) #\Newline))
        (with-output-to-string (s)
          (format s "  ---~%")
          (when suite
            (format s "  suite:       ~A~%" suite))
          (let ((file-str (and source-file (%source-file-display source-file))))
            (when file-str
              (format s "  file:        ~A~%" file-str)))
          (write-string (subseq base 6) s))
        base)))

(defun %duration-ms-from-result (result)
  "Derive duration_ms (double-float) from :duration-ns. Returns 0.0d0 when
 the timing harness hasn't populated the field (for synthetic result records).
 Integer division is deliberate — we want deterministic output regardless of LC_ALL."
  (let ((ns (getf result :duration-ns)))
    (if (and (integerp ns) (>= ns 0))
        (/ ns 1000000.0d0)
        0.0d0)))

(defun %detail-ends-with-yaml-terminator-p (detail)
  "Return T when the :detail string already ends with a YAML block
terminator line (\"  ...\"). Short-circuits on strings shorter than 5 chars
so subseq/string= never see negative bounds."
  (and (stringp detail)
       (let ((len (length detail)))
         (and (>= len 5)
              (string= detail "  ..." :start1 (- len 5) :end1 len)))))

(defun %detail-inject-duration (detail duration-ms)
  "Inject duration_ms into an existing YAML diagnostic block."
  (cond
    ((%detail-ends-with-yaml-terminator-p detail)
     (let ((prefix (subseq detail 0 (- (length detail) 5))))
       (format nil "~A  duration_ms: ~,3F~%  ..." prefix duration-ms)))
    (t
     (format nil "~A~%  ---~%  duration_ms: ~,3F~%  ..." detail duration-ms))))

(defun %format-minimal-yaml-duration (duration-ms)
  "Return a minimal TAP v13 YAML block carrying only duration_ms."
  (format nil "  ---~%  duration_ms: ~,3F~%  ..." duration-ms))

;;; TAP status table: status → (ok-prefix keyword-tag)
;;; keyword-tag is nil for :pass/:fail, "SKIP"/"TODO" for annotated statuses.
(defparameter *tap-verbose-status-table*
  '((:pass    "ok"      nil)
    (:fail    "not ok"  nil)
    (:skip    "ok"      "SKIP")
    (:pending "not ok"  "TODO"))
  "Maps test status to TAP ok/not-ok prefix and optional # DIRECTIVE keyword.")

(defun %tap-verbose-first-line (number name status detail)
  "Build the TAP first-line string for verbose mode."
  (let* ((row     (assoc status *tap-verbose-status-table*))
         (prefix  (second row))
         (keyword (third row)))
    (if keyword
        (format nil "~A ~A - ~A # ~A ~A" prefix number name keyword (or detail ""))
        (format nil "~A ~A - ~A" prefix number name))))

(defun %tap-print-result (result)
  "Print a single test result. Compact mode (default) emits only failures.
CLCC_VERBOSE_TAP=1 restores full TAP v13 output."
  (let* ((number      (getf result :number))
         (name        (getf result :name))
         (status      (getf result :status))
         (detail      (getf result :detail))
         (suite       (getf result :suite))
         (source-file (getf result :source-file))
         (duration-ms (%duration-ms-from-result result))
         (verbose-p   (%verbose-tap-p)))
    (sb-thread:with-mutex (*tap-mutex*)
      (if verbose-p
          (let ((first-line (%tap-verbose-first-line number name status detail))
                (yaml-line  (if (eq status :fail)
                                (if detail
                                    (%detail-inject-duration detail duration-ms)
                                    (%format-minimal-yaml-duration duration-ms))
                                (%format-minimal-yaml-duration duration-ms))))
            (format t "~A~%" first-line)
            (format t "~A~%" yaml-line))
          (when (eq status :fail)
            (format t "not ok - ~A~%" name)
            (format t "~A~%~%"
                    (%detail-inject-context detail suite source-file duration-ms))))
      (force-output))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Canonical Suite Definitions
;;; ─────────────────────────────────────────────────────────────────────────

(defsuite cl-cc-suite :description "CL-CC test root suite")

(defsuite cl-cc-unit-suite
  :description "Unit tests"
  :parent cl-cc-suite)

(defsuite cl-cc-integration-suite
  :description "Full-pipeline integration tests"
  :parent cl-cc-suite)

(defsuite cl-cc-integration-serial-suite
  :description "Sequential-only integration tests"
  :parent cl-cc-integration-suite
  :parallel nil)

(defsuite cl-cc-e2e-suite
  :description "End-to-end tests"
  :parent cl-cc-suite)

(defsuite cl-cc-serial-suite
  :description "Sequential-only unit tests"
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite cl-cc-suite)

;;; Test selection, fixture discovery, advanced reporting, and execution helpers
;;; are defined in framework-discovery.lisp, framework-advanced.lisp,
;;; framework-meta.lisp, and framework-runner.lisp.
