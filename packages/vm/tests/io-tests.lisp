;;;; tests/unit/vm/io-tests.lisp — VM I/O Operations Unit Tests
;;;;
;;;; Tests for I/O instructions: make-string-output-stream,
;;;; get-output-stream-string, stream-write-string, read-from-string,
;;;; stream predicates, file-handle helpers, and read/write line behavior.

(in-package :cl-cc/test)

(defsuite io-suite
  :description "VM I/O operations unit tests"
  :parent cl-cc-unit-suite)

(in-suite io-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun io-vm (&optional (out (make-string-output-stream)))
  "Create a vm-io-state with a string output stream for capture."
  (cl-cc/vm::make-vm-state :output-stream out))

(defun io-vm-full (&optional (out (make-string-output-stream)))
  "Create a vm-io-state (with file handle management) for IO tests."
  (cl-cc/vm::make-vm-state :output-stream out))

(defun io-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm::execute-instruction inst state 0 (make-hash-table :test #'equal)))

;;; ─── make-string-output-stream / get-output-stream-string ─────────────────

(deftest io-string-output-stream-roundtrip
  "Create string output stream, write to it, get result."
  (let ((s (io-vm)))
    ;; Create string output stream
    (io-exec (cl-cc:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (streamp stream))
      ;; Write string to it
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (io-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      ;; Get accumulated string
      (io-exec (cl-cc:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (assert-equal "hello" (cl-cc/vm::vm-reg-get s :R3)))))

(deftest io-string-output-stream-multiple-writes
  "Multiple writes accumulate in string output stream."
  (let ((s (io-vm)))
    (io-exec (cl-cc:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (io-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (cl-cc/vm::vm-reg-set s :R2 " world")
      (io-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (io-exec (cl-cc:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (assert-equal "hello world" (cl-cc/vm::vm-reg-get s :R3)))))

;;; ─── read-from-string ─────────────────────────────────────────────────────

(defun %io-read-str (src)
  "Execute vm-read-from-string on SRC in a fresh vm-state and return the result."
  (let ((vm (io-vm)))
    (cl-cc/vm::vm-reg-set vm :R1 src)
    (io-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) vm)
    (cl-cc/vm::vm-reg-get vm :R0)))

(deftest io-read-from-string-integer
  "vm-read-from-string reads integer 42 from \"42\"."
  (assert-equal 42 (%io-read-str "42")))

(deftest io-read-from-string-symbol
  "vm-read-from-string reads the symbol HELLO from \"hello\"."
  (assert-equal "HELLO" (symbol-name (%io-read-str "hello"))))

(deftest io-read-from-string-list
  "vm-read-from-string reads a 3-element list from \"(1 2 3)\"."
  (let ((result (%io-read-str "(1 2 3)")))
    (assert-true (listp result))
    (assert-equal 3 (length result))))

(deftest io-read-from-string-empty
  "vm-read-from-string returns nil for an empty string."
  (assert-null (%io-read-str "")))

;;; ─── vm-allocate-file-handle ────────────────────────────────────────────────

(deftest io-allocate-handle-sequence
  "Handles start at 2 (0=stdin, 1=stdout reserved) and increment by 1."
  (let ((s (io-vm-full)))
    (let ((h1 (cl-cc/vm::vm-allocate-file-handle s))
          (h2 (cl-cc/vm::vm-allocate-file-handle s))
          (h3 (cl-cc/vm::vm-allocate-file-handle s)))
      (assert-equal 2 h1)
      (assert-equal 3 h2)
      (assert-equal 4 h3))))

;;; ─── vm-get-stream ──────────────────────────────────────────────────────────

(deftest-each io-get-stream-std-handles
  "vm-get-stream resolves stdin/stdout handles to the corresponding vm streams."
  :cases (("stdin"  #'cl-cc/vm::vm-standard-input  cl-cc/vm::+stdin-handle+)
          ("stdout" #'cl-cc/vm::vm-standard-output cl-cc/vm::+stdout-handle+))
  (accessor handle)
  (let ((s (io-vm-full)))
    (assert-equal (funcall accessor s) (cl-cc/vm::vm-get-stream s handle))))

(deftest io-get-stream-cl-stream-passthrough
  "vm-get-stream passes through CL stream objects directly."
  (let ((s (io-vm-full))
        (stream (make-string-output-stream)))
    (assert-equal stream (cl-cc/vm::vm-get-stream s stream))))

(deftest-each io-get-stream-handle-map-lookup-cases
  "vm-get-stream resolves handles from open-files and string-streams maps."
  :cases (("open-files"    (lambda (s h stream) (setf (gethash h (cl-cc/vm::vm-open-files s)) stream)))
          ("string-streams" (lambda (s h stream) (setf (gethash h (cl-cc/vm::vm-string-streams s)) stream))))
  (register-fn)
  (let* ((s      (io-vm-full))
         (stream (make-string-output-stream))
         (handle (cl-cc/vm::vm-allocate-file-handle s)))
    (funcall register-fn s handle stream)
    (assert-equal stream (cl-cc/vm::vm-get-stream s handle))))

(deftest io-get-stream-invalid-handle-error
  "vm-get-stream errors on unregistered handle."
  (let ((s (io-vm-full)))
    (assert-signals error (cl-cc/vm::vm-get-stream s 999))))

;;; ─── vm-stream-open-p ──────────────────────────────────────────────────────

(deftest-each io-stream-open-p-standard-handles
  "vm-stream-open-p returns truthy for both standard stdin and stdout handles."
  :cases (("stdin"  cl-cc/vm::+stdin-handle+)
          ("stdout" cl-cc/vm::+stdout-handle+))
  (handle)
  (let ((s (io-vm-full)))
    (assert-true (cl-cc/vm::vm-stream-open-p s handle))))

(deftest io-stream-open-p-unknown-handle-returns-nil
  "vm-stream-open-p returns NIL for a handle that was never opened."
  (let ((s (io-vm-full)))
    (assert-equal nil (cl-cc/vm::vm-stream-open-p s 999))))

(deftest io-stream-open-p-direct-cl-stream-returns-truthy
  "vm-stream-open-p returns a truthy value when passed a direct CL stream object."
  (let ((s (io-vm-full)))
    (assert-true (cl-cc/vm::vm-stream-open-p s (make-string-output-stream)))))

;;; ─── stream predicate instructions ─────────────────────────────────────────

(deftest-each io-streamp
  "vm-streamp returns t for CL streams, nil for non-streams."
  :cases (("cl-stream" (make-string-output-stream) t)
          ("non-stream" 42                         nil))
  (value expected)
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (io-exec (cl-cc:make-vm-streamp :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

(deftest io-streamp-handle-resolved
  "vm-streamp resolves integer handle to stream via vm-io-state."
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 cl-cc/vm::+stdin-handle+)
    (io-exec (cl-cc:make-vm-streamp :dst :R0 :src :R1) s)
    (assert-equal t (cl-cc/vm::vm-reg-get s :R0))))

(deftest-each io-input-stream-p
  "vm-input-stream-p returns t for input streams, nil for output streams."
  :cases (("input-stream"  (make-string-input-stream "hello") t)
          ("output-stream" (make-string-output-stream)        nil))
  (stream-val expected)
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc:make-vm-input-stream-p :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

(deftest-each io-output-stream-p
  "vm-output-stream-p returns t for output streams, nil for input streams."
  :cases (("output-stream" (make-string-output-stream)   t)
          ("input-stream"  (make-string-input-stream "x") nil))
  (stream-val expected)
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc:make-vm-output-stream-p :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

(deftest io-open-stream-p-true
  "vm-open-stream-p returns t for open stream."
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 (make-string-output-stream))
    (io-exec (cl-cc:make-vm-open-stream-p :dst :R0 :src :R1) s)
    (assert-equal t (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── handle-based string streams (vm-make-string-stream) ───────────────────

(deftest io-make-string-stream-output
  "vm-make-string-stream creates an output string stream with handle."
  (let ((s (io-vm-full)))
    (io-exec (cl-cc:make-vm-make-string-stream :dst :R0 :direction :output) s)
    (let ((handle (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (integerp handle))
      (assert-true (>= handle 2)))))

(deftest io-make-string-stream-input
  "vm-make-string-stream creates an input string stream with initial string."
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (io-exec (cl-cc:make-vm-make-string-stream :dst :R0 :direction :input
                                                 :initial-string :R1) s)
    (let ((handle (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (integerp handle))
      ;; Read a char from it to verify content
      (cl-cc/vm::vm-reg-set s :R2 handle)
      (io-exec (cl-cc:make-vm-read-char :dst :R3 :handle :R2) s)
      (assert-equal #\h (cl-cc/vm::vm-reg-get s :R3)))))

(deftest io-make-string-stream-get-string
  "vm-get-string-from-stream extracts accumulated string from handle."
  (let ((s (io-vm-full)))
    ;; Create output string stream (handle-based)
    (io-exec (cl-cc:make-vm-make-string-stream :dst :R0 :direction :output) s)
    (let ((handle (cl-cc/vm::vm-reg-get s :R0)))
      ;; Write to it via handle
      (cl-cc/vm::vm-reg-set s :R1 handle)
      (cl-cc/vm::vm-reg-set s :R2 "test output")
      (io-exec (cl-cc:make-vm-write-string :handle :R1 :str :R2) s)
      ;; Get string from handle
      (io-exec (cl-cc:make-vm-get-string-from-stream :dst :R3 :handle :R1) s)
      (assert-equal "test output" (cl-cc/vm::vm-reg-get s :R3)))))

;;; ─── eof-p ──────────────────────────────────────────────────────────────────

(deftest-each io-eof-p
  "vm-eof-p returns 1 for :eof, 0 for non-eof values."
  :cases (("eof"     :eof 1)
          ("non-eof" #\a  0))
  (value expected)
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (io-exec (cl-cc:make-vm-eof-p :dst :R0 :value :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── read-char / read-line via handle ──────────────────────────────────────

(deftest io-read-line-from-string-stream
  "vm-read-line reads a line from handle-based input string stream."
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 "first line")
    (io-exec (cl-cc:make-vm-make-string-stream :dst :R0 :direction :input
                                                 :initial-string :R1) s)
    (let ((handle (cl-cc/vm::vm-reg-get s :R0)))
      (cl-cc/vm::vm-reg-set s :R2 handle)
      (io-exec (cl-cc:make-vm-read-line :dst :R3 :handle :R2) s)
      (assert-equal "first line" (cl-cc/vm::vm-reg-get s :R3)))))

;;; ─── %resolve-integer-stream-handle (extracted helper) ──────────────────

(deftest resolve-integer-stream-handle-stdin
  "%resolve-integer-stream-handle returns standard-input for stdin handle."
  (let ((s   (io-vm-full))
        (h   cl-cc/vm::+stdin-handle+))
    (assert-eq (cl-cc/vm::vm-standard-input s)
               (cl-cc/vm::%resolve-integer-stream-handle h s))))

(deftest resolve-integer-stream-handle-stdout
  "%resolve-integer-stream-handle returns standard-output for stdout handle."
  (let ((s (io-vm-full))
        (h cl-cc/vm::+stdout-handle+))
    (assert-eq (cl-cc/vm::vm-standard-output s)
               (cl-cc/vm::%resolve-integer-stream-handle h s))))

(deftest resolve-integer-stream-handle-unknown-returns-nil
  "%resolve-integer-stream-handle returns nil for an unregistered handle."
  (let ((s (io-vm-full)))
    (assert-null (cl-cc/vm::%resolve-integer-stream-handle 9999 s))))

;;; ─── %copy-ht-into ────────────────────────────────────────────────────────

(deftest copy-ht-into-copies-all-entries
  "%copy-ht-into copies all entries from src into dst, clearing dst first."
  (let ((src (make-hash-table :test #'eq))
        (dst (make-hash-table :test #'eq)))
    (setf (gethash :a src) 1
          (gethash :b src) 2
          (gethash :old dst) 99)
    (cl-cc/vm::%copy-ht-into src dst)
    (assert-= 1  (gethash :a dst))
    (assert-= 2  (gethash :b dst))
    (assert-null (gethash :old dst))
    (assert-= 2  (hash-table-count dst))))

(deftest copy-ht-into-empty-src-clears-dst
  "%copy-ht-into with empty src results in empty dst."
  (let ((src (make-hash-table :test #'eq))
        (dst (make-hash-table :test #'eq)))
    (setf (gethash :x dst) 42)
    (cl-cc/vm::%copy-ht-into src dst)
    (assert-= 0 (hash-table-count dst))))

;;; ─── clone-vm-state ───────────────────────────────────────────────────────

(deftest clone-vm-state-copies-global-vars
  "clone-vm-state copies global-vars from source to clone."
  (let ((source (cl-cc/vm::make-vm-state)))
    (setf (gethash "x" (cl-cc/vm::vm-global-vars source)) 42)
    (let ((clone (cl-cc/vm::clone-vm-state source)))
      (assert-= 42 (gethash "x" (cl-cc/vm::vm-global-vars clone)))
      (assert-false (eq (cl-cc/vm::vm-global-vars source)
                        (cl-cc/vm::vm-global-vars clone))))))
