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

(deftest rt-stream-op-optional-stream-cases
  "Optional-stream wrappers honor explicit streams for both output and input operations."
  (let ((s (make-string-output-stream)))
    (cl-cc/runtime:rt-write-char #\A s)
    (cl-cc/runtime:rt-write-string "BC" s)
    (cl-cc/runtime:rt-write-line "D" s)
    (assert-equal (format nil "ABCD~%") (get-output-stream-string s)))
  (let ((s (make-string-input-stream "xyz")))
    (assert-equal #\x (cl-cc/runtime:rt-read-char s))
    (assert-equal #\y (cl-cc/runtime:rt-peek-char s))
    (assert-equal #\y (cl-cc/runtime:rt-read-char s))))

;;; ─── rt-read-line ───────────────────────────────────────────────────────────

(deftest rt-read-line-from-string-stream
  "rt-read-line reads a line from a string input stream."
  (let ((s (make-string-input-stream "hello")))
    (assert-equal "hello" (cl-cc/runtime:rt-read-line s))))

(deftest rt-read-byte-write-byte-cases
  "Binary byte helpers round-trip bytes through a temporary file stream."
  (let* ((path (merge-pathnames "cl-cc-runtime-io-bytes.bin" (uiop:temporary-directory)))
         (out (open path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (cl-cc/runtime:rt-write-byte 65 out)
           (cl-cc/runtime:rt-write-byte 66 out)
           (close out)
           (let ((in (open path :direction :input :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (progn
                    (assert-equal 65 (cl-cc/runtime:rt-read-byte in))
                    (assert-equal 66 (cl-cc/runtime:rt-read-byte in)))
               (close in))))
      (when (probe-file path)
        (delete-file path)))))

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

(deftest rt-make-string-stream-cases
  "rt-make-string-stream supports both input and output directions."
  (let ((in (cl-cc/runtime:rt-make-string-stream "abc" :direction :input)))
    (assert-equal #\a (read-char in)))
  (let ((out (cl-cc/runtime:rt-make-string-stream "ignored" :direction :output)))
    (write-string "ok" out)
    (assert-equal "ok" (cl-cc/runtime:rt-get-string-from-stream out))))

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

(deftest rt-stream-constructor-cases
  "Broadcast, two-way, echo, and concatenated stream helpers construct CL streams."
  (let ((out1 (make-string-output-stream))
        (out2 (make-string-output-stream)))
    (assert-true (streamp (cl-cc/runtime:rt-make-broadcast-stream out1 out2))))
  (let ((in (make-string-input-stream "x"))
        (out (make-string-output-stream)))
    (assert-true (streamp (cl-cc/runtime:rt-make-two-way-stream in out)))
    (assert-true (streamp (cl-cc/runtime:rt-make-echo-stream in out))))
  (let ((a (make-string-input-stream "a"))
        (b (make-string-input-stream "b")))
    (assert-true (streamp (cl-cc/runtime:rt-make-concatenated-stream a b)))))
