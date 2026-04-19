;;;; packages/backend/runtime/tests/runtime-io-tests.lisp — Runtime I/O Unit Tests
;;;;
;;;; Tests for packages/backend/runtime/src/runtime-io.lisp:
;;;; define-rt-stream-op macro, format/read-line/write-line/peek-char,
;;;; string-output-stream helpers, stream predicates, and pathname utilities.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── rt-format ──────────────────────────────────────────────────────────────

(deftest rt-format-cases
  "rt-format: nil stream returns string; explicit stream writes to it."
  (assert-equal "42" (cl-cc/runtime:rt-format nil "~A" 42))
  (let ((s (make-string-output-stream)))
    (cl-cc/runtime:rt-format s "~A" "hello")
    (assert-equal "hello" (get-output-stream-string s))))

;;; ─── String output stream helpers ──────────────────────────────────────────

(deftest rt-string-output-stream-cases
  "rt-make-string-output-stream: creates stream; get returns empty; get-after-write returns content; alias works."
  (let ((s (cl-cc/runtime:rt-make-string-output-stream)))
    (assert-true (streamp s))
    (assert-equal "" (cl-cc/runtime:rt-get-output-stream-string s)))
  (let ((s (cl-cc/runtime:rt-make-string-output-stream)))
    (write-string "hello world" s)
    (assert-equal "hello world" (cl-cc/runtime:rt-get-output-stream-string s)))
  (let ((s (make-string-output-stream)))
    (write-string "test" s)
    (assert-equal "test" (cl-cc/runtime:rt-get-string-from-stream s))))

;;; ─── rt-read-line ───────────────────────────────────────────────────────────

(deftest rt-read-line-from-string-stream
  "rt-read-line reads a line from a string input stream."
  (let ((s (make-string-input-stream "hello")))
    (assert-equal "hello" (cl-cc/runtime:rt-read-line s))))

;;; ─── rt-write-line ──────────────────────────────────────────────────────────

(deftest rt-write-line-to-string
  "rt-write-line writes string followed by newline to stream."
  (let ((s (make-string-output-stream)))
    (cl-cc/runtime:rt-write-line "test" s)
    (assert-equal (format nil "test~%") (get-output-stream-string s))))

;;; ─── rt-peek-char ───────────────────────────────────────────────────────────

(deftest rt-peek-char-does-not-advance
  "rt-peek-char returns next character without consuming it; peeking twice gives same char."
  (let ((s (make-string-input-stream "abc")))
    (let ((first-peek  (cl-cc/runtime:rt-peek-char s))
          (second-peek (cl-cc/runtime:rt-peek-char s)))
      (assert-equal #\a first-peek)
      (assert-equal #\a second-peek))))

;;; ─── Stream predicates ──────────────────────────────────────────────────────

(deftest rt-stream-predicate-cases
  "rt-stream-element-type → character; rt-interactive-stream-p → 0 for string stream."
  (assert-equal 'character (cl-cc/runtime:rt-stream-element-type *standard-input*))
  (let ((s (make-string-input-stream "x")))
    (assert-= 0 (cl-cc/runtime:rt-interactive-stream-p s))))

;;; ─── Pathname utilities ─────────────────────────────────────────────────────

(deftest rt-pathname-cases
  "rt-make-pathname: creates pathname; namestring contains name; components :name/:type/:unknown; merge-pathnames."
  (assert-true (pathnamep (cl-cc/runtime:rt-make-pathname :name "foo" :type "lisp")))
  (let ((ns (cl-cc/runtime:rt-namestring
             (cl-cc/runtime:rt-make-pathname :name "foo" :type "lisp"))))
    (assert-true (search "foo" ns)))
  (let ((p (cl-cc/runtime:rt-make-pathname :name "bar" :type "txt")))
    (assert-equal "bar" (cl-cc/runtime:rt-pathname-component p :name))
    (assert-equal "txt" (cl-cc/runtime:rt-pathname-component p :type))
    (assert-false (cl-cc/runtime:rt-pathname-component p :unknown-key)))
  (assert-true (pathnamep (cl-cc/runtime:rt-merge-pathnames "foo.lisp" "/tmp/"))))
