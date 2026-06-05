;;;; packages/vm/tests/crash-report-tests.lisp — Crash Report Diagnostic Tests
;;;;
;;;; Tests for the build-crash-report, write-crash-report, format-timestamp,
;;;; and save-crash-dump diagnostic pipeline functions.

(in-package :cl-cc/test)

(defsuite crash-report-suite
  :description "Crash report diagnostic pipeline unit tests"
  :parent cl-cc-unit-suite)

(in-suite crash-report-suite)

(deftest crash-report-build-has-required-struct-fields
  "build-crash-report on a simple error returns a crash-report struct with
   condition-type, condition-message, and backtrace populated."
  (handler-case (error "test crash for cl-cc")
    (error (c)
      (let ((report (cl-cc/vm::build-crash-report c)))
        (assert-true (cl-cc/vm::crash-report-p report))
        (assert-true (cl-cc/vm::cr-condition-type report))
        (assert-true (stringp (cl-cc/vm::cr-condition-message report)))
        (assert-true (search "test crash for cl-cc" (cl-cc/vm::cr-condition-message report)))
        (assert-true (listp (cl-cc/vm::cr-backtrace report)))))))

(deftest crash-report-write-emits-condition-message
  "write-crash-report writes human-readable text containing the error message."
  (handler-case (error "unique-crash-token-42")
    (error (c)
      (let* ((report (cl-cc/vm::build-crash-report c))
             (out    (make-string-output-stream)))
        (cl-cc/vm::write-crash-report report out)
        (let ((text (get-output-stream-string out)))
          (assert-true (search "unique-crash-token-42" text)))))))

(deftest crash-report-format-timestamp-returns-string
  "format-timestamp on a known universal time returns a non-empty string."
  (let ((ts (cl-cc/vm::format-timestamp (encode-universal-time 0 0 12 1 1 2000))))
    (assert-true (stringp ts))
    (assert-true (> (length ts) 0))))

(deftest crash-report-timestamp-is-set-on-build
  "build-crash-report sets the crash-report timestamp to a positive integer."
  (handler-case (error "ts-test")
    (error (c)
      (let ((report (cl-cc/vm::build-crash-report c)))
        (assert-true (integerp (cl-cc/vm::cr-timestamp report)))
        (assert-true (> (cl-cc/vm::cr-timestamp report) 0))))))
