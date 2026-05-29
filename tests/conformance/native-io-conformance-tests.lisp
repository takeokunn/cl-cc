;;;; tests/conformance/native-io-conformance-tests.lisp
;;;; ANSI CL Native I/O / Stream / Pathname Conformance Tests
;;;; (expected-fail for known gaps)
;;;;
;;;; Tests I/O operations that work in the VM interpreter (via host SBCL)
;;;; but are NOT available in native x86-64 binaries. Per ANSI CL, these
;;;; must work regardless of compilation mode.

(in-package :cl-cc/test)

(defsuite ansi-conformance-native-io-suite
  :description "ANSI CL Native I/O / Stream / Pathname Conformance Tests"
  :parent cl-cc-conformance-suite
  :parallel nil)

(in-suite ansi-conformance-native-io-suite)

;;; ──────────────────────────────────────────────────────────────────────
;;; Helper
;;; ──────────────────────────────────────────────────────────────────────

(defun io-run (form-string)
  "Run FORM-STRING through cl-cc pipeline and return result."
  (cl-cc:run-string form-string))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: String Streams
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-string-output-stream
  "make-string-output-stream and get-output-stream-string should work."
  :timeout 30
  :tags '(:io :string-stream :native :e2e)
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (write-string \"hello\" s)
                    (get-output-stream-string s))")))
    (assert-equal "hello" result)))

(deftest io-with-output-to-string
  "with-output-to-string should capture output."
  :timeout 30
  :tags '(:io :with-output-to-string :native :e2e)
  (let ((result (io-run
                 "(with-output-to-string (s)
                    (princ 42 s)
                    (write-string \"abc\" s))")))
    (assert-equal "42abc" result)))

(deftest io-with-input-from-string
  "with-input-from-string should provide string as input."
  :timeout 30
  :tags '(:io :with-input-from-string :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (read-char s))")))
    (assert-eql #\h result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Character I/O
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-read-char-write-char
  "read-char and write-char should work with string streams."
  :timeout 30
  :tags '(:io :read-char :write-char :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-char #\\X out)
                    (write-char #\\Y out)
                    (get-output-stream-string out))")))
    (assert-equal "XY" result)))

(deftest io-peek-char
  "peek-char should look ahead without consuming."
  :timeout 30
  :tags '(:io :peek-char :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"ABC\")
                    (list (peek-char nil s) (read-char s) (read-char s)))")))
    (assert-equal '(#\A #\A #\B) result)))

(deftest io-unread-char
  "unread-char should push character back."
  :timeout 30
  :tags '(:io :unread-char :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (let ((c (read-char s)))
                      (unread-char c s)
                      (list (read-char s) (read-char s))))")))
    (assert-equal '(#\h #\e) result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Line I/O
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-read-line
  "read-line should read until newline."
  :timeout 30
  :tags '(:io :read-line :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s (format nil \"hello~%world\"))
                    (list (read-line s) (read-line s)))")))
    (assert-equal '("hello" "world") result)))

(deftest io-write-line
  "write-line should append newline."
  :timeout 30
  :tags '(:io :write-line :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-line \"hello\" out)
                    (get-output-stream-string out))")))
    (assert-equal (format nil "hello~%") result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Print Functions
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-print-prin1-princ
  "print/princ/prin1 should work via native code."
  :timeout 30
  :tags '(:io :print :princ :prin1 :native :e2e)
  (let ((result (io-run
                 "(with-output-to-string (s)
                    (prin1 \"hello\" s)
                    (princ #\\space s)
                    (princ 42 s))")))
    (assert-equal "\"hello\" 42" result)))

(deftest io-write-to-string
  "write-to-string should return string representation."
  :timeout 30
  :tags '(:io :write-to-string :native :e2e)
  (let ((result (io-run "(write-to-string 42 :base 16)")))
    (assert-equal "2A" result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Stream Predicates
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-stream-predicates
  "streamp/input-stream-p/output-stream-p should work."
  :timeout 30
  :tags '(:io :streamp :stream-predicates :native :e2e)
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (list (streamp s)
                          (output-stream-p s)
                          (input-stream-p s)
                          (open-stream-p s)))")))
    (assert-equal '(t t nil t) result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: File I/O
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-open-close
  "open and close should work with file streams."
  :timeout 30
  :tags '(:io :open :close :file :native :e2e)
  (let ((result (io-run
                 "(let ((s (open \"/tmp/cl-cc-conformance-test.txt\"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)))
                    (write-line \"test\" s)
                    (close s)
                    :ok)")))
    (assert-equal :ok result)))

(deftest io-with-open-file
  "with-open-file should handle file open/close automatically."
  :timeout 30
  :tags '(:io :with-open-file :native :e2e)
  (let ((result (io-run
                 "(with-open-file (s \"/tmp/cl-cc-conformance-test2.txt\"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                    (write-line \"hello\" s))
                  (with-open-file (s \"/tmp/cl-cc-conformance-test2.txt\"
                                     :direction :input)
                    (read-line s))")))
    (assert-equal "hello" result)))

(deftest io-file-position-length
  "file-position and file-length should work."
  :timeout 30
  :tags '(:io :file-position :file-length :native :e2e)
  (let ((result (io-run
                 "(with-open-file (s \"/tmp/cl-cc-conformance-test3.txt\"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                    (write-string \"abcdef\" s)
                    (file-position s 3)
                    (write-string \"XYZ\" s)
                    (file-length s))")))
    (assert-= 6 result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Stream Control
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-force-finish-output
  "force-output and finish-output should not error."
  :timeout 30
  :tags '(:io :force-output :finish-output :native :e2e)
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (write-char #\\X s)
                    (finish-output s)
                    (force-output s)
                    :ok)")))
    (assert-equal :ok result)))

(deftest io-listen
  "listen should detect available input."
  :timeout 30
  :tags '(:io :listen :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (list (listen s) (read-char s) (listen s)))")))
    (assert-equal '(t #\h t) result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Pathname Operations
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-make-pathname
  "make-pathname should construct pathnames."
  :timeout 30
  :tags '(:io :make-pathname :pathname :native :e2e)
  (let ((result (io-run
                 "(let ((p (make-pathname :name \"test\" :type \"txt\")))
                    (list (pathnamep p) (pathname-name p) (pathname-type p)))")))
    (assert-equal '(t "test" "txt") result)))

(deftest io-namestring
  "namestring should convert pathname to string."
  :timeout 30
  :tags '(:io :namestring :pathname :native :e2e)
  (let ((result (io-run
                 "(let ((p (make-pathname :name \"test\" :type \"lisp\")))
                    (namestring p))")))
    (assert-true (search "test.lisp" result :test #'char-equal))))

(deftest io-merge-pathnames
  "merge-pathnames should fill in defaults."
  :timeout 30
  :tags '(:io :merge-pathnames :pathname :native :e2e)
  (let ((result (io-run
                 "(let* ((d (make-pathname :name \"base\" :type \"lisp\"))
                         (m (merge-pathnames (make-pathname :type \"txt\") d)))
                    (pathname-type m))")))
    (assert-equal "txt" result)))

(deftest io-probe-file
  "probe-file should detect file existence."
  :timeout 30
  :tags '(:io :probe-file :pathname :native :e2e)
  (let ((result (io-run
                 "(progn
                    (with-open-file (s \"/tmp/cl-cc-probe-test.txt\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"data\" s))
                    (if (probe-file \"/tmp/cl-cc-probe-test.txt\")
                        :exists
                        :not-found))")))
    (assert-equal :exists result)))

(deftest io-delete-file
  "delete-file should remove a file."
  :timeout 30
  :tags '(:io :delete-file :pathname :native :e2e)
  (let ((result (io-run
                 "(progn
                    (with-open-file (s \"/tmp/cl-cc-delete-test.txt\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"x\" s))
                    (delete-file \"/tmp/cl-cc-delete-test.txt\")
                    (if (probe-file \"/tmp/cl-cc-delete-test.txt\")
                        :still-there
                        :deleted))")))
    (assert-equal :deleted result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: File System Operations
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-directory-listing
  "directory should list files matching pattern."
  :timeout 30
  :tags '(:io :directory :pathname :native :e2e)
  (let ((result (io-run
                 "(progn
                    (with-open-file (s \"/tmp/cl-cc-dir-test-a.txt\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"a\" s))
                    (length (directory \"/tmp/cl-cc-dir-test-*.txt\")))")))
    (assert-true (>= result 1))))

(deftest io-ensure-directories-exist
  "ensure-directories-exist should create directories."
  :timeout 30
  :tags '(:io :ensure-directories-exist :pathname :native :e2e)
  (let ((result (io-run
                 "(let ((dir \"/tmp/cl-cc-ensure-dir-test/\"))
                    (ensure-directories-exist dir)
                    (probe-file dir))")))
    (assert-true result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: LOAD (loading files at runtime)
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-load-file
  "load should evaluate forms from a file."
  :timeout 30
  :tags '(:io :load :native :e2e)
  ;; Write a file, then load it
  (let ((result (io-run
                 "(progn
                    (with-open-file (s \"/tmp/cl-cc-load-test.lisp\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"(defparameter *load-test-var* 42)\" s))
                    (load \"/tmp/cl-cc-load-test.lisp\")
                    *load-test-var*)")))
    (assert-= 42 result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Compound Streams
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-broadcast-stream
  "make-broadcast-stream should create broadcast stream."
  :timeout 30
  :tags '(:io :broadcast-stream :native :e2e)
  (let ((result (io-run
                 "(let* ((a (make-string-output-stream))
                         (b (make-string-output-stream))
                         (bc (make-broadcast-stream a b)))
                    (write-string \"hello\" bc)
                    (list (get-output-stream-string a)
                          (get-output-stream-string b)))")))
    (assert-equal '("hello" "hello") result)))

(deftest io-concatenated-stream
  "make-concatenated-stream should concatenate input streams."
  :timeout 30
  :tags '(:io :concatenated-stream :native :e2e)
  (let ((result (io-run
                 "(let* ((a (make-string-input-stream \"ABC\"))
                         (b (make-string-input-stream \"DEF\"))
                         (cc (make-concatenated-stream a b)))
                    (list (read-char cc) (read-char cc) (read-char cc)
                          (read-char cc) (read-char cc) (read-char cc)))")))
    (assert-equal '(#\A #\B #\C #\D #\E #\F) result)))

(deftest io-echo-stream
  "make-echo-stream should echo input to output."
  :timeout 30
  :tags '(:io :echo-stream :native :e2e)
  (let ((result (io-run
                 "(let* ((in (make-string-input-stream \"hello\"))
                         (out (make-string-output-stream))
                         (ec (make-echo-stream in out)))
                    (list (read-char ec) (read-char ec)
                          (get-output-stream-string out)))")))
    (assert-equal '(#\h #\e "hh") result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Sequence I/O
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-read-sequence
  "read-sequence should fill a sequence with stream input."
  :timeout 30
  :tags '(:io :read-sequence :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"ABCDEF\")
                    (let ((v (make-string 3)))
                      (read-sequence v s)
                      v))")))
    (assert-equal "ABC" result)))

(deftest io-write-sequence
  "write-sequence should write sequence elements to stream."
  :timeout 30
  :tags '(:io :write-sequence :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-sequence \"hello\" out)
                    (get-output-stream-string out))")))
    (assert-equal "hello" result)))
