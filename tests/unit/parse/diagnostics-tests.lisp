;;;; tests/unit/parse/diagnostics-tests.lisp — Diagnostic System Tests
;;;;
;;;; Tests for src/parse/diagnostics.lisp:
;;;; byte-offset-to-line-col, source-line-at, format-diagnostic,
;;;; format-diagnostic-list, make-parse-error, make-parse-warning,
;;;; parse-failure condition.

(in-package :cl-cc/test)

(defsuite diagnostics-suite :description "Parser diagnostic system tests"
  :parent cl-cc-suite)


(in-suite diagnostics-suite)
;;; ─── byte-offset-to-line-col ──────────────────────────────────────────────────

(deftest-each diag-byte-offset-to-line-col-cases
  "byte-offset-to-line-col returns correct (line col) for each case."
  ((src offset expected-line expected-col)
   ("hello"                      0   1 1)
   ("hello world"                5   1 6)
   ((format nil "abc~%def")      4   2 1)
   ((format nil "a~%b~%cde")     4   3 1)
   ("hi"                       100   1 3)
   (""                            0   1 1))
  (multiple-value-bind (line col)
      (cl-cc::byte-offset-to-line-col src offset)
    (assert-equal expected-line line)
    (assert-equal expected-col col)))

;;; ─── source-line-at ───────────────────────────────────────────────────────────

(deftest-each diag-source-line-at-cases
  "source-line-at returns correct line for each offset."
  ((src offset expected)
   ((format nil "hello~%world") 0 "hello")
   ((format nil "hello~%world") 6 "world")
   ("foobar"                    3 "foobar"))
  (assert-equal expected (cl-cc::source-line-at src offset)))

;;; ─── diagnostic struct ────────────────────────────────────────────────────────

(deftest diag-struct-creation
  "diagnostic struct constructors produce correct fields."
  (let ((d (cl-cc::make-diagnostic)))
    (assert-eq :error (cl-cc::diagnostic-severity d))
    (assert-equal "" (cl-cc::diagnostic-message d))
    (assert-null (cl-cc::diagnostic-hints d))
    (assert-null (cl-cc::diagnostic-notes d)))
  (let ((d (cl-cc::make-parse-error "bad syntax" '(5 . 10))))
    (assert-eq :error (cl-cc::diagnostic-severity d))
    (assert-equal "bad syntax" (cl-cc::diagnostic-message d))
    (assert-equal '(5 . 10) (cl-cc::diagnostic-span d)))
  (let ((d (cl-cc::make-parse-warning "deprecated" '(0 . 3))))
    (assert-eq :warning (cl-cc::diagnostic-severity d))
    (assert-equal "deprecated" (cl-cc::diagnostic-message d))))

(deftest diag-error-with-hints-and-notes
  "make-parse-error supports :hints and :notes."
  (let ((d (cl-cc::make-parse-error "err" '(0 . 1)
             :hints '(("try this" . (0 . 1)))
             :notes '("see docs"))))
    (assert-equal 1 (length (cl-cc::diagnostic-hints d)))
    (assert-equal 1 (length (cl-cc::diagnostic-notes d)))))

;;; ─── format-diagnostic ────────────────────────────────────────────────────────

(deftest diag-format-diagnostic-content
  "format-diagnostic output includes severity, message, location, source line, hints, and notes."
  (let* ((d (cl-cc::make-parse-error "unexpected )" '(5 . 6)))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "hello)" out))))
    (assert-true (search "error" s)))
  (let* ((d (cl-cc::make-parse-error "bad token" '(0 . 3)))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "foo" out))))
    (assert-true (search "bad token" s)))
  (let* ((d (cl-cc::make-parse-error "err" '(0 . 1) :source-file "test.lisp"))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "x" out))))
    (assert-true (search "test.lisp" s))
    (assert-true (search "1:1" s)))
  (let* ((src "(defun bad)")
         (d (cl-cc::make-parse-error "err" '(7 . 10)))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d src out))))
    (assert-true (search "(defun bad)" s)))
  (let* ((d (cl-cc::make-parse-error "err" '(0 . 1)
             :hints '(("try this" . (0 . 1)))))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "x" out))))
    (assert-true (search "hint: try this" s)))
  (let* ((d (cl-cc::make-parse-error "err" '(0 . 1)
             :notes '("see documentation")))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "x" out))))
    (assert-true (search "note: see documentation" s))))

;;; ─── format-diagnostic-list ───────────────────────────────────────────────────

(deftest diag-format-diagnostic-list
  "format-diagnostic-list reports correct error/warning counts."
  (let* ((d1 (cl-cc::make-parse-error "err1" '(0 . 1)))
         (d2 (cl-cc::make-parse-warning "warn1" '(2 . 3)))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic-list (list d1 d2) "abcdef" out))))
    (assert-true (search "1 error" s))
    (assert-true (search "1 warning" s)))
  (let ((s (with-output-to-string (out)
             (cl-cc::format-diagnostic-list nil "x" out))))
    (assert-true (search "0 errors" s))
    (assert-true (search "0 warnings" s))))

;;; ─── parse-failure condition ──────────────────────────────────────────────────

(deftest diag-parse-failure-behavior
  "parse-failure is signalable, carries diagnostics, and reports error count."
  (let* ((d (cl-cc::make-parse-error "oops" '(0 . 1)))
         (caught nil))
    (handler-case
      (error 'cl-cc::parse-failure :diagnostics (list d))
      (cl-cc::parse-failure (c)
        (setf caught c)))
    (assert-true caught)
    (assert-equal 1 (length (cl-cc::parse-failure-diagnostics caught))))
  (let* ((d1 (cl-cc::make-parse-error "e1" '(0 . 1)))
         (d2 (cl-cc::make-parse-error "e2" '(2 . 3)))
         (msg (handler-case
                (error 'cl-cc::parse-failure :diagnostics (list d1 d2))
                (cl-cc::parse-failure (c) (format nil "~A" c)))))
    (assert-true (search "2 error" msg))))
