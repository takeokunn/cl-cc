;;;; tests/unit/parse/diagnostics-tests.lisp — Diagnostic System Tests
;;;;
;;;; Tests for src/parse/diagnostics.lisp:
;;;; byte-offset-to-line-col, source-line-at, format-diagnostic,
;;;; format-diagnostic-list, make-parse-error, make-parse-warning,
;;;; parse-failure condition.

(in-package :cl-cc/test)

(defsuite diagnostics-suite :description "Parser diagnostic system tests"
  :parent cl-cc-unit-suite)


(in-suite diagnostics-suite)
;;; ─── byte-offset-to-line-col ──────────────────────────────────────────────────

(deftest-each diag-byte-offset-to-line-col-cases
  "byte-offset-to-line-col returns correct (line col) for each case."
  :cases (("simple" "hello"                      0   1 1)
          ("with-space" "hello world"                5   1 6)
          ("with-format-1" (format nil "abc~%def")      4   2 1)
          ("with-format-2" (format nil "a~%b~%cde")     4   3 1)
          ("late" "hi"                       100   1 3)
          ("empty" ""                            0   1 1))
  (src offset expected-line expected-col)
  (multiple-value-bind (line col)
       (cl-cc/parse:byte-offset-to-line-col src offset)
     (assert-equal expected-line line)
     (assert-equal expected-col col)))

;;; ─── source-line-at ───────────────────────────────────────────────────────────

(deftest-each diag-source-line-at-cases
  "source-line-at returns correct line for each offset."
  :cases (("offset-zero" (format nil "hello~%world") 0 "hello")
          ("offset-middle" (format nil "hello~%world") 6 "world")
          ("no-format" "foobar"                    3 "foobar"))
  (src offset expected)
  (assert-equal expected (cl-cc/parse:source-line-at src offset)))

;;; ─── diagnostic struct ────────────────────────────────────────────────────────

(deftest diag-struct-creation
  "diagnostic struct constructors produce correct fields."
  (let ((d (cl-cc/parse:make-diagnostic)))
    (assert-eq :error (cl-cc/parse:diagnostic-severity d))
    (assert-equal "" (cl-cc/parse:diagnostic-message d))
    (assert-null (cl-cc/parse::diagnostic-hints d))
    (assert-null (cl-cc/parse::diagnostic-notes d)))
  (let ((d (cl-cc/parse:make-parse-error "bad syntax" '(5 . 10))))
    (assert-eq :error (cl-cc/parse:diagnostic-severity d))
    (assert-equal "bad syntax" (cl-cc/parse:diagnostic-message d))
    (assert-equal '(5 . 10) (cl-cc/parse:diagnostic-span d)))
  (let ((d (cl-cc/parse:make-parse-warning "deprecated" '(0 . 3))))
    (assert-eq :warning (cl-cc/parse:diagnostic-severity d))
    (assert-equal "deprecated" (cl-cc/parse:diagnostic-message d))))

(deftest diag-error-with-hints-and-notes
  "make-parse-error supports :hints and :notes."
  (let ((d (cl-cc/parse:make-parse-error "err" '(0 . 1)
             :hints '(("try this" . (0 . 1)))
             :notes '("see docs"))))
    (assert-equal 1 (length (cl-cc/parse::diagnostic-hints d)))
    (assert-equal 1 (length (cl-cc/parse::diagnostic-notes d)))))

;;; ─── format-diagnostic ────────────────────────────────────────────────────────

(deftest diag-format-diagnostic-content
  "format-diagnostic output includes severity, message, location, source line, hints, and notes."
  (let* ((d (cl-cc/parse:make-parse-error "unexpected )" '(5 . 6)))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "hello)" out))))
    (assert-true (search "error" s)))
  (let* ((d (cl-cc/parse:make-parse-error "bad token" '(0 . 3)))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "foo" out))))
    (assert-true (search "bad token" s)))
  (let* ((d (cl-cc/parse:make-parse-error "err" '(0 . 1) :source-file "test.lisp"))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "x" out))))
    (assert-true (search "test.lisp" s))
    (assert-true (search "1:1" s)))
  (let* ((src "(defun bad)")
         (d (cl-cc/parse:make-parse-error "err" '(7 . 10)))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d src out))))
    (assert-true (search "(defun bad)" s)))
  (let* ((d (cl-cc/parse:make-parse-error "err" '(0 . 1)
             :hints '(("try this" . (0 . 1)))))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "x" out))))
    (assert-true (search "hint: try this" s)))
  (let* ((d (cl-cc/parse:make-parse-error "err" '(0 . 1)
             :notes '("see documentation")))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "x" out))))
    (assert-true (search "note: see documentation" s))))

;;; ─── format-diagnostic-list ───────────────────────────────────────────────────

(deftest diag-format-diagnostic-list
  "format-diagnostic-list reports correct error/warning counts."
  (let* ((d1 (cl-cc/parse:make-parse-error "err1" '(0 . 1)))
         (d2 (cl-cc/parse:make-parse-warning "warn1" '(2 . 3)))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic-list (list d1 d2) "abcdef" out))))
    (assert-true (search "1 error" s))
    (assert-true (search "1 warning" s)))
  (let ((s (with-output-to-string (out)
             (cl-cc/parse:format-diagnostic-list nil "x" out))))
    (assert-true (search "0 errors" s))
    (assert-true (search "0 warnings" s))))

;;; ─── parse-failure condition ──────────────────────────────────────────────────

(deftest diag-parse-failure-behavior
  "parse-failure is signalable, carries diagnostics, and reports error count."
  (let* ((d (cl-cc/parse:make-parse-error "oops" '(0 . 1)))
         (caught nil))
    (handler-case
      (error 'cl-cc/parse:parse-failure :diagnostics (list d))
      (cl-cc/parse:parse-failure (c)
        (setf caught c)))
    (assert-true caught)
    (assert-equal 1 (length (cl-cc/parse::parse-failure-diagnostics caught))))
  (let* ((d1 (cl-cc/parse:make-parse-error "e1" '(0 . 1)))
         (d2 (cl-cc/parse:make-parse-error "e2" '(2 . 3)))
         (msg (handler-case
                (error 'cl-cc/parse:parse-failure :diagnostics (list d1 d2))
                (cl-cc/parse:parse-failure (c) (format nil "~A" c)))))
    (assert-true (search "2 error" msg))))
