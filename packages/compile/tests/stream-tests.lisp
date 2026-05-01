;;;; tests/integration/stream-tests.lisp — Stream I/O Integration Tests
;;;; Tests for Phase 2 (Streams) of self-hosting: stream predicates,
;;;; string streams, binary I/O, write-line, with-open-file, etc.

(in-package :cl-cc/test)

(defsuite stream-suite
  :description "Stream I/O integration tests"
  :parent cl-cc-integration-serial-suite
  :parallel nil)

(in-suite stream-suite)

;;; ─── Stream Predicates ──────────────────────────────────────────────────

(deftest-each stream-predicates
  "Stream type predicates return correct values."
  :cases (("streamp-on-string-output"
           "(let ((s (make-string-output-stream))) (streamp s))" t)
          ("streamp-on-number"
           "(streamp 42)" nil)
          ("streamp-on-string"
           "(streamp \"hello\")" nil)
          ("input-stream-p-on-input"
           "(let ((s (make-string-input-stream \"hello\"))) (input-stream-p s))" t)
          ("input-stream-p-on-output"
           "(let ((s (make-string-output-stream))) (input-stream-p s))" nil)
          ("output-stream-p-on-output"
           "(let ((s (make-string-output-stream))) (output-stream-p s))" t)
          ("output-stream-p-on-input"
           "(let ((s (make-string-input-stream \"hi\"))) (output-stream-p s))" nil)
          ("open-stream-p-open"
           "(let ((s (make-string-output-stream))) (open-stream-p s))" t))
  (expr expected)
  (assert-run= expected expr))

;;; ─── String Streams (with-output-to-string / with-input-from-string) ────

(deftest-each string-stream-ops
  "String stream macros work correctly."
  :cases (("with-output-to-string-basic"
           "(with-output-to-string (s) (write-string \"hello\" s))" "hello")
          ("with-output-to-string-multiple"
           "(with-output-to-string (s) (write-string \"a\" s) (write-string \"b\" s))" "ab")
          ("with-output-to-string-initial-string"
           "(with-output-to-string (s \"prefix:\") (write-string \"body\" s))" "prefix:body")
          ("with-input-from-string-read-char"
           "(with-input-from-string (s \"abc\") (read-char s))" #\a)
          ("with-input-from-string-read-line"
           "(with-input-from-string (s \"hello\") (read-line s))" "hello")
          ("make-string-output-stream-get"
           "(let ((s (make-string-output-stream)))
               (write-string \"test\" s)
               (get-output-stream-string s))" "test")
          ("make-string-input-stream-read-char"
           "(let ((s (make-string-input-stream \"xyz\"))) (read-char s))" #\x))
  (expr expected)
  (with-reset-repl-state
    (assert-equal expected (run-string-repl expr))))

;;; ─── write-line ─────────────────────────────────────────────────────────

(deftest-each write-line-ops
  "write-line writes string followed by newline."
  :cases (("write-line-to-string-stream"
           "(with-output-to-string (s) (write-line \"hello\" s))"
           (format nil "hello~%"))
          ("write-line-returns-string"
           "(let ((s (make-string-output-stream)))
              (write-line \"test\" s))" "test"))
  (expr expected)
  (assert-run-string= expected expr))

;;; ─── read-char / read-line with optional stream ─────────────────────────

(deftest-each read-char-optional-stream
  "read-char works with explicit stream argument."
  :cases (("read-char-from-string-stream"
           "(let ((s (make-string-input-stream \"hello\"))) (read-char s))" #\h)
          ("read-char-sequence"
           "(let ((s (make-string-input-stream \"ab\"))) (read-char s))" #\a))
  (expr expected)
  (with-reset-repl-state
    (assert-equal expected (run-string-repl expr))))

(deftest-each read-line-optional-stream
  "read-line works with explicit stream argument."
  :cases (("read-line-from-string-stream"
           "(let ((s (make-string-input-stream \"hello world\")))
              (read-line s))" "hello world"))
  (expr expected)
  (assert-run-string= expected expr))

;;; ─── write-char / write-string to stream ────────────────────────────────

(deftest-each write-char-to-stream
  "write-char writes to explicit stream."
  :cases (("write-char-to-string-stream"
           "(with-output-to-string (s) (write-char #\\A s))" "A")
          ("write-char-sequence"
           "(with-output-to-string (s)
              (write-char #\\H s) (write-char #\\i s))" "Hi"))
  (expr expected)
  (assert-run-string= expected expr))

(deftest-each write-string-to-stream
  "write-string writes to explicit stream."
  :cases (("write-string-to-output-stream"
           "(with-output-to-string (s) (write-string \"hello\" s))" "hello")
          ("write-string-returns-string"
           "(let ((s (make-string-output-stream)))
              (write-string \"test\" s))" "test"))
  (expr expected)
  (assert-run-string= expected expr))

;;; ─── Stream element type ────────────────────────────────────────────────

(deftest stream-element-type-basic
  "stream-element-type returns a type for string streams."
  (let ((result (run-string
                 "(let ((s (make-string-output-stream)))
                    (stream-element-type s))")))
    (assert-true (not (null result)))))

;;; ─── force-output / finish-output ───────────────────────────────────────

(deftest-each stream-output-control
  "force-output and finish-output flush without error; content is preserved."
  :cases (("force-output"  "force-output")
          ("finish-output" "finish-output"))
  (flush-fn)
  (with-reset-repl-state
    (let ((result (ignore-errors
                    (run-string-repl
                     (format nil "(let ((s (make-string-output-stream)))
                        (write-string \"test\" s)
                        (~A s)
                        (get-output-stream-string s))" flush-fn)))))
      (assert-true (stringp result))
      (assert-equal "test" result))))

;;; ─── with-open-file (file I/O) ──────────────────────────────────────────

(deftest with-open-file-write
  "with-open-file can write file contents."
  (let* ((tmpfile (format nil "/tmp/cl-cc-stream-test-~A.txt" (get-universal-time)))
         (write-expr (format nil
                             "(progn
                                (with-open-file (s ~S :direction :output)
                                  (write-string \"hello world\" s))
                                t)"
                             tmpfile))
         (write-result (ignore-errors (run-string write-expr)))
         (host-result (ignore-errors
                        (with-open-file (in tmpfile :direction :input)
                          (read-line in nil nil)))))
    (ignore-errors (delete-file tmpfile))
    (assert-true write-result)
    (assert-true (and (stringp host-result) (string= host-result "hello world")))))

(deftest with-open-file-read
  "with-open-file can read file contents."
  (let* ((tmpfile (format nil "/tmp/cl-cc-stream-read-test-~A.txt" (get-universal-time)))
         (read-expr (format nil
                            "(with-open-file (s ~S :direction :input)
                               (read-line s))"
                            tmpfile))
         (result nil))
    (unwind-protect
         (progn
           (with-open-file (out tmpfile :direction :output :if-exists :supersede)
             (write-string "hello world" out))
           (setf result (ignore-errors (run-string read-expr)))
           (assert-true (and (stringp result) (string= result "hello world"))))
      (ignore-errors (delete-file tmpfile)))))

;;; ─── Standard stream variables accessible ───────────────────────────────

(deftest-each standard-stream-vars
  "Standard stream variables are accessible in compiled code."
  :cases (("standard-output-bound"
           "(streamp *standard-output*)" t)
          ("standard-input-bound"
           "(streamp *standard-input*)" t)
          ("error-output-bound"
           "(streamp *error-output*)" t))
  (expr expected)
  (assert-run= expected expr))

;;; ─── Peek-char ──────────────────────────────────────────────────────────

(deftest peek-char-basic
  "peek-char returns next char without consuming it."
  (assert-run= #\a
    "(let ((s (make-string-input-stream \"abc\")))
       (let ((peeked (peek-char nil s)))
         (let ((actual (read-char s)))
           (if (eql peeked actual) peeked nil))))"))

;;; ─── with-open-stream ──────────────────────────────────────────────────

(deftest with-open-stream-basic
  "with-open-stream binds variable and closes on exit."
  (assert-run-string= "hello"
    "(with-open-stream (s (make-string-input-stream \"hello\"))
       (read-line s))"))

;;; ─── Load ───────────────────────────────────────────────────────────────

(deftest load-file-basic
  "load reads and executes a Lisp source file."
  (let* ((tmpfile (format nil "/tmp/cl-cc-load-test-~A.lisp" (get-universal-time))))
    (unwind-protect
         (progn
           ;; Write the source file from host CL
           (with-open-file (s tmpfile :direction :output :if-exists :supersede)
             (write-string "(defun load-test-fn-42 (x) (* x 3))" s))
           ;; Use REPL pipeline: load defines the function, then call it
            (with-reset-repl-state
              (run-string-repl (format nil "(load ~S)" tmpfile))
              (let ((result (run-string-repl "(load-test-fn-42 14)")))
                (assert-true (eql result 42)))))
       (ignore-errors (delete-file tmpfile))
       (reset-repl-state))))
