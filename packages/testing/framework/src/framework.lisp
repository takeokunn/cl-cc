;;;; tests/framework.lisp — CL-CC Custom Test Framework (Core TAP + canonical suites)
;;;; Zero external dependencies. Replaces FiveAM.
;;;;
;;;; Load order note:
;;;;   - framework-conditions-state.lisp owns conditions + dynamic state
;;;;   - framework-definitions.lisp owns deftest/defsuite/fixtures
;;;;   - this file keeps TAP output and canonical suite declarations

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; TAP Output
;;; ------------------------------------------------------------

(defun %tap-print (line)
  "Print a TAP line with mutex protection."
  (sb-thread:with-mutex (*tap-mutex*)
    (format t "~A~%" line)
    (force-output)))

(defun %duration-ms-from-result (result)
  "Derive duration_ms (double-float) from :duration-ns. Returns 0.0d0 when
the timing harness hasn't populated the field (e.g. legacy synthetic
results). Integer division is deliberate — we want deterministic output
regardless of LC_ALL."
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

(defun %tap-print-result (result)
  "Print a single TAP test result line plus a YAML diagnostic block."
  (let* ((number (getf result :number))
         (name   (getf result :name))
         (status (getf result :status))
         (detail (getf result :detail))
         (duration-ms (%duration-ms-from-result result)))
    (sb-thread:with-mutex (*tap-mutex*)
      (ecase status
        (:pass
         (format t "ok ~A - ~A~%" number name)
         (format t "~A~%" (%format-minimal-yaml-duration duration-ms)))
        (:fail
         (format t "not ok ~A - ~A~%" number name)
         (if detail
             (format t "~A~%" (%detail-inject-duration detail duration-ms))
             (format t "~A~%" (%format-minimal-yaml-duration duration-ms))))
        (:skip
         (format t "ok ~A - ~A # SKIP ~A~%" number name (or detail ""))
         (format t "~A~%" (%format-minimal-yaml-duration duration-ms)))
        (:pending
         (format t "not ok ~A - ~A # TODO ~A~%" number name (or detail ""))
         (format t "~A~%" (%format-minimal-yaml-duration duration-ms))))
       (force-output))))

;;; ------------------------------------------------------------
;;; Canonical Suite Definitions
;;; ------------------------------------------------------------

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
