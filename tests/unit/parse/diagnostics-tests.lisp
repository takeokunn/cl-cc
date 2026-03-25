;;;; tests/unit/parse/diagnostics-tests.lisp — Diagnostic System Tests
;;;;
;;;; Tests for src/parse/diagnostics.lisp:
;;;; byte-offset-to-line-col, source-line-at, format-diagnostic,
;;;; format-diagnostic-list, make-parse-error, make-parse-warning,
;;;; parse-failure condition.

(in-package :cl-cc/test)

(defsuite diagnostics-suite :description "Parser diagnostic system tests")

;;; ─── byte-offset-to-line-col ──────────────────────────────────────────────────

(deftest diag-line-col-start-of-source
  "Byte offset 0 returns line 1, column 1."
  (multiple-value-bind (line col)
      (cl-cc::byte-offset-to-line-col "hello" 0)
    (assert-equal 1 line)
    (assert-equal 1 col)))

(deftest diag-line-col-middle-of-line
  "Byte offset in middle of first line returns correct column."
  (multiple-value-bind (line col)
      (cl-cc::byte-offset-to-line-col "hello world" 5)
    (assert-equal 1 line)
    (assert-equal 6 col)))

(deftest diag-line-col-second-line
  "Byte offset after newline returns line 2."
  (let ((src (format nil "abc~%def")))
    (multiple-value-bind (line col)
        (cl-cc::byte-offset-to-line-col src 4)
      (assert-equal 2 line)
      (assert-equal 1 col))))

(deftest diag-line-col-third-line
  "Byte offset on third line returns line 3."
  (let ((src (format nil "a~%b~%cde")))
    ;; "a\nb\ncde" -> offset 4 is 'c' on line 3
    (multiple-value-bind (line col)
        (cl-cc::byte-offset-to-line-col src 4)
      (assert-equal 3 line)
      (assert-equal 1 col))))

(deftest diag-line-col-past-end
  "Byte offset beyond source length is clamped."
  (multiple-value-bind (line col)
      (cl-cc::byte-offset-to-line-col "hi" 100)
    (assert-equal 1 line)
    (assert-equal 3 col)))

(deftest diag-line-col-empty-source
  "Byte offset 0 on empty source returns 1,1."
  (multiple-value-bind (line col)
      (cl-cc::byte-offset-to-line-col "" 0)
    (assert-equal 1 line)
    (assert-equal 1 col)))

;;; ─── source-line-at ───────────────────────────────────────────────────────────

(deftest diag-source-line-first-line
  "source-line-at returns the first line for offset 0."
  (let ((src (format nil "hello~%world")))
    (assert-equal "hello" (cl-cc::source-line-at src 0))))

(deftest diag-source-line-second-line
  "source-line-at returns the second line for offset past newline."
  (let ((src (format nil "hello~%world")))
    (assert-equal "world" (cl-cc::source-line-at src 6))))

(deftest diag-source-line-single-line
  "source-line-at on single-line source returns the whole line."
  (assert-equal "foobar" (cl-cc::source-line-at "foobar" 3)))

;;; ─── diagnostic struct ────────────────────────────────────────────────────────

(deftest diag-struct-defaults
  "diagnostic struct has correct defaults."
  (let ((d (cl-cc::make-diagnostic)))
    (assert-eq :error (cl-cc::diagnostic-severity d))
    (assert-equal "" (cl-cc::diagnostic-message d))
    (assert-null (cl-cc::diagnostic-hints d))
    (assert-null (cl-cc::diagnostic-notes d))))

(deftest diag-make-parse-error
  "make-parse-error creates an :error diagnostic."
  (let ((d (cl-cc::make-parse-error "bad syntax" '(5 . 10))))
    (assert-eq :error (cl-cc::diagnostic-severity d))
    (assert-equal "bad syntax" (cl-cc::diagnostic-message d))
    (assert-equal '(5 . 10) (cl-cc::diagnostic-span d))))

(deftest diag-make-parse-warning
  "make-parse-warning creates a :warning diagnostic."
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

(deftest diag-format-contains-severity
  "format-diagnostic output includes the severity."
  (let* ((d (cl-cc::make-parse-error "unexpected )" '(5 . 6)))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "hello)" out))))
    (assert-true (search "error" s))))

(deftest diag-format-contains-message
  "format-diagnostic output includes the message text."
  (let* ((d (cl-cc::make-parse-error "bad token" '(0 . 3)))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "foo" out))))
    (assert-true (search "bad token" s))))

(deftest diag-format-contains-location
  "format-diagnostic output includes file:line:col."
  (let* ((d (cl-cc::make-parse-error "err" '(0 . 1) :source-file "test.lisp"))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "x" out))))
    (assert-true (search "test.lisp" s))
    (assert-true (search "1:1" s))))

(deftest diag-format-contains-source-line
  "format-diagnostic output includes the source line."
  (let* ((src "(defun bad)")
         (d (cl-cc::make-parse-error "err" '(7 . 10)))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d src out))))
    (assert-true (search "(defun bad)" s))))

(deftest diag-format-contains-hints
  "format-diagnostic output includes hints."
  (let* ((d (cl-cc::make-parse-error "err" '(0 . 1)
             :hints '(("try this" . (0 . 1)))))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "x" out))))
    (assert-true (search "hint: try this" s))))

(deftest diag-format-contains-notes
  "format-diagnostic output includes notes."
  (let* ((d (cl-cc::make-parse-error "err" '(0 . 1)
             :notes '("see documentation")))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic d "x" out))))
    (assert-true (search "note: see documentation" s))))

;;; ─── format-diagnostic-list ───────────────────────────────────────────────────

(deftest diag-format-list-summary
  "format-diagnostic-list includes error/warning count summary."
  (let* ((d1 (cl-cc::make-parse-error "err1" '(0 . 1)))
         (d2 (cl-cc::make-parse-warning "warn1" '(2 . 3)))
         (s (with-output-to-string (out)
              (cl-cc::format-diagnostic-list (list d1 d2) "abcdef" out))))
    (assert-true (search "1 error" s))
    (assert-true (search "1 warning" s))))

(deftest diag-format-list-empty
  "format-diagnostic-list with no diagnostics shows 0 errors, 0 warnings."
  (let ((s (with-output-to-string (out)
             (cl-cc::format-diagnostic-list nil "x" out))))
    (assert-true (search "0 errors" s))
    (assert-true (search "0 warnings" s))))

;;; ─── parse-failure condition ──────────────────────────────────────────────────

(deftest diag-parse-failure-condition
  "parse-failure is signalable and carries diagnostics."
  (let* ((d (cl-cc::make-parse-error "oops" '(0 . 1)))
         (caught nil))
    (handler-case
      (error 'cl-cc::parse-failure :diagnostics (list d))
      (cl-cc::parse-failure (c)
        (setf caught c)))
    (assert-true caught)
    (assert-equal 1 (length (cl-cc::parse-failure-diagnostics caught)))))

(deftest diag-parse-failure-report
  "parse-failure report string includes error count."
  (let* ((d1 (cl-cc::make-parse-error "e1" '(0 . 1)))
         (d2 (cl-cc::make-parse-error "e2" '(2 . 3)))
         (msg (handler-case
                (error 'cl-cc::parse-failure :diagnostics (list d1 d2))
                (cl-cc::parse-failure (c) (format nil "~A" c)))))
    (assert-true (search "2 error" msg))))
