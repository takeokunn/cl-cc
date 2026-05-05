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

(deftest-each compound-stream-constructors
  "Compound stream constructors compile and preserve their underlying stream behavior."
  :cases (("make-synonym-stream"
           "(let ((*terminal-io* (make-string-output-stream)))
               (write-char #\\A (make-synonym-stream '*terminal-io*))
               (get-output-stream-string *terminal-io*))"
           "A")
          ("make-broadcast-stream"
           "(let ((a (make-string-output-stream))
                  (b (make-string-output-stream)))
              (let ((s (make-broadcast-stream a b)))
                (write-string \"hi\" s)
                (list (get-output-stream-string a)
                      (get-output-stream-string b))))"
           '("hi" "hi"))
          ("make-two-way-stream"
           "(let* ((in (make-string-input-stream \"abc\"))
                   (out (make-string-output-stream))
                   (s (make-two-way-stream in out)))
              (write-char #\\Z s)
              (list (read-char s) (get-output-stream-string out)))"
           '(#\a "Z"))
          ("make-echo-stream"
           "(let* ((in (make-string-input-stream \"Q\"))
                   (out (make-string-output-stream))
                   (s (make-echo-stream in out)))
              (list (read-char s) (get-output-stream-string out)))"
           '(#\Q "Q"))
          ("make-concatenated-stream"
           "(let ((s (make-concatenated-stream
                      (make-string-input-stream \"ab\")
                      (make-string-input-stream \"cd\"))))
              (list (read-char s) (read-char s) (read-char s) (read-char s)))"
           '(#\a #\b #\c #\d)))
  (expr expected)
  (with-reset-repl-state
    (assert-equal expected (run-string-repl expr))))

(deftest-each compound-stream-accessors
  "Compound stream accessor bridges expose the original component streams."
  :cases (("broadcast-stream-streams"
           "(let ((a (make-string-output-stream))
                  (b (make-string-output-stream)))
              (let ((s (make-broadcast-stream a b)))
                (list (eq a (first (broadcast-stream-streams s)))
                      (eq b (second (broadcast-stream-streams s))))))"
           '(t t))
          ("two-way-stream-accessors"
           "(let ((in (make-string-input-stream \"a\"))
                  (out (make-string-output-stream)))
              (let ((s (make-two-way-stream in out)))
                (list (eq in (two-way-stream-input-stream s))
                      (eq out (two-way-stream-output-stream s)))))"
           '(t t))
          ("echo-stream-accessors"
           "(let ((in (make-string-input-stream \"a\"))
                  (out (make-string-output-stream)))
              (let ((s (make-echo-stream in out)))
                (list (eq in (echo-stream-input-stream s))
                      (eq out (echo-stream-output-stream s)))))"
           '(t t))
          ("concatenated-stream-streams"
           "(let ((a (make-string-input-stream \"ab\"))
                  (b (make-string-input-stream \"cd\")))
              (let ((s (make-concatenated-stream a b)))
                (list (eq a (first (concatenated-stream-streams s)))
                      (eq b (second (concatenated-stream-streams s))))))"
           '(t t)))
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

(deftest-each query-io-prompt-bridges
  "y-or-n-p and yes-or-no-p read from *query-io* without blocking tests."
  :cases (("y-or-n-p-accepts-y"
           "(let* ((in (make-string-input-stream (format nil \"Y~%\")))
                   (out (make-string-output-stream))
                   (*query-io* (make-two-way-stream in out)))
              (list (y-or-n-p \"Proceed? \") (get-output-stream-string out)))"
           '(t "Proceed? "))
          ("y-or-n-p-accepts-yes"
           "(let* ((in (make-string-input-stream (format nil \"YES~%\")))
                   (out (make-string-output-stream))
                   (*query-io* (make-two-way-stream in out)))
              (list (y-or-n-p \"Proceed? \") (get-output-stream-string out)))"
           '(t "Proceed? "))
          ("yes-or-no-p-rejects-short-answer"
           "(let* ((in (make-string-input-stream (format nil \"y~%no~%\")))
                   (out (make-string-output-stream))
                   (*query-io* (make-two-way-stream in out)))
              (list (yes-or-no-p \"Continue? \")
                    (> (length (get-output-stream-string out)) 0)))"
           '(nil t)))
  (expr expected)
  (with-reset-repl-state
    (assert-equal expected (run-string-repl expr))))

(deftest-each file-string-length-uses-utf-8-byte-count
  "file-string-length uses UTF-8 byte counts for characters and strings on streams."
  :cases (("character-on-stream"
           "(let ((s (make-string-output-stream)))
              (file-string-length s (code-char 233)))"
           2)
          ("string-on-stream"
           "(let ((s (make-string-output-stream)))
              (file-string-length s \"hé\"))"
           3))
  (expr expected)
  (assert-run= expected expr))

(deftest file-string-length-supports-vm-file-handles
  "file-string-length also works when the stream designator is a VM file handle integer."
  (let ((tmpfile (format nil "/tmp/cl-cc-file-string-length-~A.txt" (get-universal-time))))
    (unwind-protect
         (assert-=
          3
          (run-string
           (format nil "(let ((h (open ~S :direction :output :if-exists :supersede :if-does-not-exist :create)))
  (prog1 (file-string-length h \"hé\")
    (close h)))"
                   tmpfile)))
      (ignore-errors (delete-file tmpfile)))))

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
