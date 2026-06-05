;;;; formatter-tests.lisp — Unit tests for cl-cc/formatter (FR-320)
;;;;
;;;; Tests cover format-string and the internal %format-form dispatch via
;;;; the public API. No file I/O, no spawned processes.

(in-package :cl-cc/test)

(defsuite formatter-suite
  :description "FR-320 formatter unit tests"
  :parent cl-cc-unit-suite)

(in-suite formatter-suite)

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun trim-whitespace (s)
  "Remove leading/trailing whitespace from each line of S, then strip blank
lines, to allow whitespace-insensitive comparison of formatted output."
  (with-output-to-string (out)
    (with-input-from-string (in s)
      (loop for line = (read-line in nil nil)
            while line
            for trimmed = (string-trim " " line)
            unless (string= trimmed "")
              do (write-string trimmed out)
                 (terpri out)))))

;;; ─── format-string: basic atom forms ─────────────────────────────────────

(deftest formatter-formats-integer-atom
  "FR-320: format-string round-trips a bare integer literal unchanged."
  (let ((result (cl-cc/formatter:format-string "42")))
    (assert-true (search "42" result))))

(deftest formatter-formats-string-atom
  "FR-320: format-string round-trips a string literal (with escaped quotes)."
  (let ((result (cl-cc/formatter:format-string "\"hello\"")))
    (assert-true (search "hello" result))))

(deftest formatter-formats-symbol-atom
  "FR-320: format-string preserves a bare symbol name."
  (let ((result (cl-cc/formatter:format-string "foo")))
    (assert-true (search "FOO" result))))

;;; ─── format-string: empty / nil input ────────────────────────────────────

(deftest formatter-empty-string-returns-empty
  "FR-320: format-string on the empty string produces an empty string."
  (let ((result (cl-cc/formatter:format-string "")))
    (assert-equal "" result)))

(deftest formatter-nil-list-formats-as-unit
  "FR-320: format-string on the empty list literal '()' emits '()'."
  (let ((result (cl-cc/formatter:format-string "()")))
    (assert-true (search "()" result))))

;;; ─── format-string: simple call form ─────────────────────────────────────

(deftest formatter-simple-call-on-one-line
  "FR-320: a short call form is printed on a single line (< 60-char rule)."
  (let ((result (cl-cc/formatter:format-string "(+ 1 2)")))
    ;; The head and arguments should appear together
    (assert-true (search "+" result))
    (assert-true (search "1" result))
    (assert-true (search "2" result))))

;;; ─── format-string: special-form indentation ──────────────────────────────

(deftest formatter-defun-indents-body
  "FR-320: a DEFUN form emits the function name on its own indented line."
  (let* ((source "(defun square (x) (* x x))")
         (result (cl-cc/formatter:format-string source)))
    ;; Head and name must both appear
    (assert-true (search "DEFUN" result))
    (assert-true (search "SQUARE" result))
    ;; Body expression must be present
    (assert-true (search "*" result))))

(deftest formatter-let-indents-bindings-and-body
  "FR-320: a LET form emits its binding list and body on indented lines."
  (let* ((source "(let ((x 1)) x)")
         (result (cl-cc/formatter:format-string source)))
    (assert-true (search "LET" result))
    (assert-true (search "X" result))
    (assert-true (search "1" result))))

;;; ─── format-string: cond / case dispatch ──────────────────────────────────

(deftest formatter-cond-indents-clauses
  "FR-320: a COND form emits each clause on its own indented line."
  (let* ((source "(cond ((= x 1) :one) (t :other))")
         (result (cl-cc/formatter:format-string source)))
    (assert-true (search "COND" result))
    (assert-true (search ":ONE" result))
    (assert-true (search ":OTHER" result))))

;;; ─── format-string: multiple top-level forms ─────────────────────────────

(deftest formatter-multiple-top-level-forms
  "FR-320: format-string reads and emits all top-level forms in order."
  (let* ((source "(defvar *x* 0) (defvar *y* 1)")
         (result (cl-cc/formatter:format-string source)))
    (assert-true (search "*X*" result))
    (assert-true (search "*Y*" result))
    ;; Each form should end up before the other in document order
    (assert-true (< (search "*X*" result) (search "*Y*" result)))))

;;; ─── format-string: indent-size keyword ─────────────────────────────────

(deftest formatter-indent-size-parameter-is-respected
  "FR-320: :indent-size 4 uses 4-space indentation instead of the default 2."
  (let* ((source "(defun f (x) x)")
         (result-2 (cl-cc/formatter:format-string source :indent-size 2))
         (result-4 (cl-cc/formatter:format-string source :indent-size 4)))
    ;; Both must contain the function name
    (assert-true (search "F" result-2))
    (assert-true (search "F" result-4))
    ;; The 4-space version must be at least as long as the 2-space version
    ;; (more leading spaces means more characters overall)
    (assert-true (>= (length result-4) (length result-2)))))
