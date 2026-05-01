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

(defvar *coverage-reload-in-progress* nil
  "Internal guard used by `run-suite` to force exactly one sb-cover reload.
When coverage mode is enabled, the test framework recompiles the local ASDF
systems with coverage instrumentation before executing tests. This guard
prevents recursive reload loops while that happens.")
