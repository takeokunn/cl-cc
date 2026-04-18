;;;; tests/framework.lisp — CL-CC Custom Test Framework (Core)
;;;; Zero external dependencies. Replaces FiveAM.
;;;; TAP version 13 output, parallel execution, fixtures, skip/pending.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Conditions
;;; ------------------------------------------------------------

(define-condition test-failure (error)
  ((message :initarg :message :reader test-failure-message))
  (:report (lambda (c s)
             (format s "Test failure: ~A" (test-failure-message c)))))

(define-condition skip-condition (condition)
  ((reason :initarg :reason :reader skip-reason))
  (:report (lambda (c s)
             (format s "SKIP: ~A" (skip-reason c)))))

(define-condition pending-condition (condition)
  ((reason :initarg :reason :reader pending-reason))
  (:report (lambda (c s)
             (format s "TODO: ~A" (pending-reason c)))))

;;; ------------------------------------------------------------
;;; Internal State
;;; ------------------------------------------------------------

(defvar *suite-registry* (persist-empty)
  "symbol -> plist (:name :description :parent :before-each :after-each).
   Immutable persistent-map; mutated only by `setf' rebinding the symbol.")

(defvar *test-registry* (persist-empty)
  "symbol -> plist (:name :fn :suite :timeout :depends-on :tags :docstring).
   Immutable persistent-map; mutated only by `setf' rebinding the symbol.")

(defvar *current-suite* nil
  "Currently active suite symbol.")

(defvar *invariant-registry* '()
  "List of invariant functions called after every test.")

(defvar *tap-mutex* (sb-thread:make-mutex :name "tap-output")
  "Mutex for thread-safe TAP output.")

(defvar *snapshot-dir* "tests/snapshots/"
  "Directory for snapshot files.")

(defvar *metamorphic-relations* '()
  "List of metamorphic relation plists.")

;;; ------------------------------------------------------------
;;; Suite Definition
;;; ------------------------------------------------------------

(defmacro defsuite (name &key description parent (parallel t))
  "Define a test suite. Stores metadata in *suite-registry*.
Use :parallel NIL for suites that must always run sequentially."
  `(progn
     (setf *suite-registry*
           (persist-assoc *suite-registry* ',name
                          (list :name ',name
                                :description ,description
                                :parent ',parent
                                :parallel ,parallel
                                :before-each '()
                                :after-each '())))
     ',name))

(defmacro in-suite (name)
  "Set the current active suite."
  `(setf *current-suite* ',name))

;;; ------------------------------------------------------------
;;; Test Definition
;;; ------------------------------------------------------------

(defun %parse-deftest-body (forms)
  "Parse body forms, extracting optional docstring and keyword args.
Returns (values docstring timeout depends-on tags body-forms)."
  (let ((docstring nil)
        (timeout nil)
        (depends-on nil)
        (tags nil)
        (rest forms))
    (when (and rest (stringp (car rest)))
      (setf docstring (car rest)
            rest (cdr rest)))
    (loop while (and rest (keywordp (car rest)))
          do (let ((key (pop rest))
                   (val (pop rest)))
               (case key
                 (:timeout   (setf timeout val))
                 (:depends-on (setf depends-on val))
                 (:tags      (setf tags val)))))
    (values docstring timeout depends-on tags rest)))

(defmacro deftest (name &body body)
  "Define a test. Syntax:
     (deftest name
       \"optional docstring\"
       :timeout 5
       :timeout :none
       :depends-on other-test
       :tags '(:tag1)
       body-form...)"
  (multiple-value-bind (docstring timeout depends-on tags body-forms)
      (%parse-deftest-body body)
    `(progn
       (setf *test-registry*
             (persist-assoc *test-registry* ',name
                            (list :name ',name
                                  :fn (lambda () ,@body-forms)
                                  :suite *current-suite*
                                  :timeout ,timeout
                                  :depends-on ',depends-on
                                  :tags ,tags
                                  :docstring ,docstring)))
       ',name)))

;;; ------------------------------------------------------------
;;; Fixtures
;;; ------------------------------------------------------------

(defmacro defbefore (when-spec suite-spec &body body)
  "Register a before-each fixture for the given suite.
   (defbefore :each (suite-name) body...)"
  (declare (ignore when-spec))
  (let ((suite-name (if (listp suite-spec) (car suite-spec) suite-spec)))
    `(let ((entry (persist-lookup *suite-registry* ',suite-name)))
       (when entry
         (let ((new-entry (copy-list entry)))
           (setf (getf new-entry :before-each)
                 (append (getf new-entry :before-each)
                         (list (lambda () ,@body))))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* ',suite-name new-entry)))))))

(defmacro defafter (when-spec suite-spec &body body)
  "Register an after-each fixture for the given suite.
   (defafter :each (suite-name) body...)"
  (declare (ignore when-spec))
  (let ((suite-name (if (listp suite-spec) (car suite-spec) suite-spec)))
    `(let ((entry (persist-lookup *suite-registry* ',suite-name)))
       (when entry
         (let ((new-entry (copy-list entry)))
           (setf (getf new-entry :after-each)
                 (append (getf new-entry :after-each)
                         (list (lambda () ,@body))))
           (setf *suite-registry*
                 (persist-assoc *suite-registry* ',suite-name new-entry)))))))

;;; ------------------------------------------------------------
;;; Skip / Pending
;;; ------------------------------------------------------------

(defun skip (reason)
  "Signal a skip condition with the given reason."
  (signal 'skip-condition :reason reason))

(defun pending (reason)
  "Signal a pending condition with the given reason."
  (signal 'pending-condition :reason reason))


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
  "Given an existing YAML diagnostic string (\"  ---\\n  message: …\\n  ...\")
that was produced by the runner for :fail results, append a
`duration_ms:` key right before the terminating `  ...` sentinel. Returns
the new string. If the format is unexpected, fall back to appending a
freestanding block — this keeps the output TAP v13 valid either way."
  (cond
    ((%detail-ends-with-yaml-terminator-p detail)
     ;; Replace the final "  ..." with "  duration_ms: X\n  ...".
     (let ((prefix (subseq detail 0 (- (length detail) 5))))
       (format nil "~A  duration_ms: ~,3F~%  ..." prefix duration-ms)))
    (t
     ;; No recognizable YAML terminator — wrap existing detail in one.
     (format nil "~A~%  ---~%  duration_ms: ~,3F~%  ..." detail duration-ms))))

(defun %format-minimal-yaml-duration (duration-ms)
  "Return a minimal TAP v13 YAML block carrying only duration_ms."
  (format nil "  ---~%  duration_ms: ~,3F~%  ..." duration-ms))

(defun %tap-print-result (result)
  "Print a single TAP test result line plus a YAML diagnostic block.
Every status now emits a YAML block containing at least `duration_ms:` so
downstream tools can ingest per-test wall-clock timing. Failure diagnostics
are preserved — we splice duration_ms into the existing YAML rather than
replacing it."
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

;; Integration tests that exercise the full compile pipeline via run-string.
;; Each case invokes parse → expand → cps → optimize → codegen → execute.
;; Heavy tests rely on *stdlib-expanded-cache* being pre-warmed by run-suite
;; before the parallel workers start.
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
