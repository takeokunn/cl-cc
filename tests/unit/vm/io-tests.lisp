;;;; tests/unit/vm/io-tests.lisp — VM I/O Operations Unit Tests
;;;;
;;;; Tests for I/O instructions: write-to-string, format, princ, prin1,
;;;; make-string-output-stream, get-output-stream-string, stream-write-string,
;;;; read-from-string, stream predicates, and print output capture.

(in-package :cl-cc/test)

(defsuite io-suite :description "VM I/O operations unit tests")

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun io-vm (&optional (out (make-string-output-stream)))
  "Create a vm-state with a string output stream for capture."
  (make-instance 'cl-cc::vm-state :output-stream out))

(defun io-vm-full (&optional (out (make-string-output-stream)))
  "Create a vm-io-state (with file handle management) for IO tests."
  (make-instance 'cl-cc::vm-io-state :output-stream out))

(defun io-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc::execute-instruction inst state 0 (make-hash-table :test #'equal)))

(defun io-capture (state)
  "Get captured output from the vm-state's output stream."
  (get-output-stream-string (cl-cc::vm-output-stream state)))

;;; ─── write-to-string ──────────────────────────────────────────────────────

(deftest io-write-to-string-number
  "vm-write-to-string converts number to string."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 42)
    (io-exec (cl-cc::make-vm-write-to-string-inst :dst :R0 :src :R1) s)
    (assert-equal "42" (cl-cc::vm-reg-get s :R0))))

(deftest io-write-to-string-symbol
  "vm-write-to-string converts symbol to string (may include package)."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 :test)
    (io-exec (cl-cc::make-vm-write-to-string-inst :dst :R0 :src :R1) s)
    (assert-equal ":TEST" (cl-cc::vm-reg-get s :R0))))

(deftest io-write-to-string-string
  "vm-write-to-string wraps string in quotes."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "hi")
    (io-exec (cl-cc::make-vm-write-to-string-inst :dst :R0 :src :R1) s)
    (assert-equal "\"hi\"" (cl-cc::vm-reg-get s :R0))))

;;; ─── format ───────────────────────────────────────────────────────────────

(deftest io-format-simple
  "vm-format-inst formats string with no args."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "hello world")
    (io-exec (cl-cc::make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (assert-equal "hello world" (cl-cc::vm-reg-get s :R0))))

(deftest io-format-with-args
  "vm-format-inst formats string with arguments."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "~A is ~A")
    (cl-cc::vm-reg-set s :R2 "answer")
    (cl-cc::vm-reg-set s :R3 42)
    (io-exec (cl-cc::make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2 :R3)) s)
    (assert-equal "answer is 42" (cl-cc::vm-reg-get s :R0))))

(deftest io-format-directive-d
  "vm-format-inst handles ~D directive."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "count: ~D")
    (cl-cc::vm-reg-set s :R2 99)
    (io-exec (cl-cc::make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "count: 99" (cl-cc::vm-reg-get s :R0))))

;;; ─── princ / prin1 / print ───────────────────────────────────────────────

(deftest io-princ-number
  "vm-princ prints number without escaping."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 42)
    (io-exec (cl-cc::make-vm-princ :src :R1) s)
    (assert-equal "42" (io-capture s))))

(deftest io-princ-string
  "vm-princ prints string without quotes."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (io-exec (cl-cc::make-vm-princ :src :R1) s)
    (assert-equal "hello" (io-capture s))))

(deftest io-prin1-string
  "vm-prin1 prints string with quotes."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (io-exec (cl-cc::make-vm-prin1 :src :R1) s)
    (assert-equal "\"hello\"" (io-capture s))))

(deftest io-terpri
  "vm-terpri-inst outputs a newline."
  (let ((s (io-vm)))
    (io-exec (cl-cc::make-vm-terpri-inst) s)
    (let ((out (io-capture s)))
      (assert-equal 1 (length out))
      (assert-equal #\Newline (char out 0)))))

;;; ─── make-string-output-stream / get-output-stream-string ─────────────────

(deftest io-string-output-stream-roundtrip
  "Create string output stream, write to it, get result."
  (let ((s (io-vm)))
    ;; Create string output stream
    (io-exec (cl-cc::make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc::vm-reg-get s :R0)))
      (assert-true (streamp stream))
      ;; Write string to it
      (cl-cc::vm-reg-set s :R1 stream)
      (cl-cc::vm-reg-set s :R2 "hello")
      (io-exec (cl-cc::make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      ;; Get accumulated string
      (io-exec (cl-cc::make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (assert-equal "hello" (cl-cc::vm-reg-get s :R3)))))

(deftest io-string-output-stream-multiple-writes
  "Multiple writes accumulate in string output stream."
  (let ((s (io-vm)))
    (io-exec (cl-cc::make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc::vm-reg-get s :R0)))
      (cl-cc::vm-reg-set s :R1 stream)
      (cl-cc::vm-reg-set s :R2 "hello")
      (io-exec (cl-cc::make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (cl-cc::vm-reg-set s :R2 " world")
      (io-exec (cl-cc::make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (io-exec (cl-cc::make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (assert-equal "hello world" (cl-cc::vm-reg-get s :R3)))))

;;; ─── read-from-string ─────────────────────────────────────────────────────

(deftest io-read-from-string-number
  "vm-read-from-string reads a number."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "42")
    (io-exec (cl-cc::make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal 42 (cl-cc::vm-reg-get s :R0))))

(deftest io-read-from-string-symbol
  "vm-read-from-string reads a symbol."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (io-exec (cl-cc::make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal "HELLO" (symbol-name (cl-cc::vm-reg-get s :R0)))))

(deftest io-read-from-string-list
  "vm-read-from-string reads a list."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "(1 2 3)")
    (io-exec (cl-cc::make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (let ((result (cl-cc::vm-reg-get s :R0)))
      (assert-true (listp result))
      (assert-equal 3 (length result)))))

(deftest io-read-from-string-empty
  "vm-read-from-string returns nil for empty string."
  (let ((s (io-vm)))
    (cl-cc::vm-reg-set s :R1 "")
    (io-exec (cl-cc::make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal nil (cl-cc::vm-reg-get s :R0))))

;;; ─── vm-allocate-file-handle ────────────────────────────────────────────────

(deftest io-allocate-handle-starts-at-2
  "First allocated handle is 2 (0=stdin, 1=stdout reserved)."
  (let ((s (io-vm-full)))
    (assert-equal 2 (cl-cc::vm-allocate-file-handle s))))

(deftest io-allocate-handle-increments
  "Successive allocations return incrementing handles."
  (let ((s (io-vm-full)))
    (let ((h1 (cl-cc::vm-allocate-file-handle s))
          (h2 (cl-cc::vm-allocate-file-handle s))
          (h3 (cl-cc::vm-allocate-file-handle s)))
      (assert-equal 2 h1)
      (assert-equal 3 h2)
      (assert-equal 4 h3))))

;;; ─── vm-get-stream ──────────────────────────────────────────────────────────

(deftest io-get-stream-stdin
  "vm-get-stream resolves stdin handle to standard-input."
  (let ((s (io-vm-full)))
    (assert-equal (cl-cc::vm-standard-input s)
                  (cl-cc::vm-get-stream s cl-cc::+stdin-handle+))))

(deftest io-get-stream-stdout
  "vm-get-stream resolves stdout handle to standard-output."
  (let ((s (io-vm-full)))
    (assert-equal (cl-cc::vm-standard-output s)
                  (cl-cc::vm-get-stream s cl-cc::+stdout-handle+))))

(deftest io-get-stream-cl-stream-passthrough
  "vm-get-stream passes through CL stream objects directly."
  (let ((s (io-vm-full))
        (stream (make-string-output-stream)))
    (assert-equal stream (cl-cc::vm-get-stream s stream))))

(deftest io-get-stream-open-files-lookup
  "vm-get-stream resolves registered file handles."
  (let* ((s (io-vm-full))
         (stream (make-string-output-stream))
         (handle (cl-cc::vm-allocate-file-handle s)))
    (setf (gethash handle (cl-cc::vm-open-files s)) stream)
    (assert-equal stream (cl-cc::vm-get-stream s handle))))

(deftest io-get-stream-string-streams-lookup
  "vm-get-stream resolves registered string stream handles."
  (let* ((s (io-vm-full))
         (stream (make-string-output-stream))
         (handle (cl-cc::vm-allocate-file-handle s)))
    (setf (gethash handle (cl-cc::vm-string-streams s)) stream)
    (assert-equal stream (cl-cc::vm-get-stream s handle))))

(deftest io-get-stream-invalid-handle-error
  "vm-get-stream errors on unregistered handle."
  (let ((s (io-vm-full)))
    (assert-signals error (cl-cc::vm-get-stream s 999))))

;;; ─── vm-stream-open-p ──────────────────────────────────────────────────────

(deftest io-stream-open-p-stdin
  "vm-stream-open-p returns truthy for stdin handle."
  (let ((s (io-vm-full)))
    (assert-true (cl-cc::vm-stream-open-p s cl-cc::+stdin-handle+))))

(deftest io-stream-open-p-stdout
  "vm-stream-open-p returns truthy for stdout handle."
  (let ((s (io-vm-full)))
    (assert-true (cl-cc::vm-stream-open-p s cl-cc::+stdout-handle+))))

(deftest io-stream-open-p-unknown
  "vm-stream-open-p returns nil for unknown handle."
  (let ((s (io-vm-full)))
    (assert-equal nil (cl-cc::vm-stream-open-p s 999))))

(deftest io-stream-open-p-cl-stream
  "vm-stream-open-p returns truthy for direct CL stream."
  (let ((s (io-vm-full)))
    (assert-true (cl-cc::vm-stream-open-p s (make-string-output-stream)))))

;;; ─── stream predicate instructions ─────────────────────────────────────────

(deftest io-streamp-cl-stream
  "vm-streamp returns t for a CL stream object."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 (make-string-output-stream))
    (io-exec (cl-cc::make-vm-streamp :dst :R0 :src :R1) s)
    (assert-equal t (cl-cc::vm-reg-get s :R0))))

(deftest io-streamp-non-stream
  "vm-streamp returns nil for non-stream."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 42)
    (io-exec (cl-cc::make-vm-streamp :dst :R0 :src :R1) s)
    (assert-equal nil (cl-cc::vm-reg-get s :R0))))

(deftest io-streamp-handle-resolved
  "vm-streamp resolves integer handle to stream via vm-io-state."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 cl-cc::+stdin-handle+)
    (io-exec (cl-cc::make-vm-streamp :dst :R0 :src :R1) s)
    (assert-equal t (cl-cc::vm-reg-get s :R0))))

(deftest io-input-stream-p-true
  "vm-input-stream-p returns t for input stream."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 (make-string-input-stream "hello"))
    (io-exec (cl-cc::make-vm-input-stream-p :dst :R0 :src :R1) s)
    (assert-equal t (cl-cc::vm-reg-get s :R0))))

(deftest io-input-stream-p-false
  "vm-input-stream-p returns nil for output stream."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 (make-string-output-stream))
    (io-exec (cl-cc::make-vm-input-stream-p :dst :R0 :src :R1) s)
    (assert-equal nil (cl-cc::vm-reg-get s :R0))))

(deftest io-output-stream-p-true
  "vm-output-stream-p returns t for output stream."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 (make-string-output-stream))
    (io-exec (cl-cc::make-vm-output-stream-p :dst :R0 :src :R1) s)
    (assert-equal t (cl-cc::vm-reg-get s :R0))))

(deftest io-output-stream-p-false
  "vm-output-stream-p returns nil for input stream."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 (make-string-input-stream "x"))
    (io-exec (cl-cc::make-vm-output-stream-p :dst :R0 :src :R1) s)
    (assert-equal nil (cl-cc::vm-reg-get s :R0))))

(deftest io-open-stream-p-true
  "vm-open-stream-p returns t for open stream."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 (make-string-output-stream))
    (io-exec (cl-cc::make-vm-open-stream-p :dst :R0 :src :R1) s)
    (assert-equal t (cl-cc::vm-reg-get s :R0))))

;;; ─── handle-based string streams (vm-make-string-stream) ───────────────────

(deftest io-make-string-stream-output
  "vm-make-string-stream creates an output string stream with handle."
  (let ((s (io-vm-full)))
    (io-exec (cl-cc::make-vm-make-string-stream :dst :R0 :direction :output) s)
    (let ((handle (cl-cc::vm-reg-get s :R0)))
      (assert-true (integerp handle))
      (assert-true (>= handle 2)))))

(deftest io-make-string-stream-input
  "vm-make-string-stream creates an input string stream with initial string."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 "hello")
    (io-exec (cl-cc::make-vm-make-string-stream :dst :R0 :direction :input
                                                 :initial-string :R1) s)
    (let ((handle (cl-cc::vm-reg-get s :R0)))
      (assert-true (integerp handle))
      ;; Read a char from it to verify content
      (cl-cc::vm-reg-set s :R2 handle)
      (io-exec (cl-cc::make-vm-read-char :dst :R3 :handle :R2) s)
      (assert-equal #\h (cl-cc::vm-reg-get s :R3)))))

(deftest io-make-string-stream-get-string
  "vm-get-string-from-stream extracts accumulated string from handle."
  (let ((s (io-vm-full)))
    ;; Create output string stream (handle-based)
    (io-exec (cl-cc::make-vm-make-string-stream :dst :R0 :direction :output) s)
    (let ((handle (cl-cc::vm-reg-get s :R0)))
      ;; Write to it via handle
      (cl-cc::vm-reg-set s :R1 handle)
      (cl-cc::vm-reg-set s :R2 "test output")
      (io-exec (cl-cc::make-vm-write-string :handle :R1 :str :R2) s)
      ;; Get string from handle
      (io-exec (cl-cc::make-vm-get-string-from-stream :dst :R3 :handle :R1) s)
      (assert-equal "test output" (cl-cc::vm-reg-get s :R3)))))

;;; ─── eof-p ──────────────────────────────────────────────────────────────────

(deftest io-eof-p-true
  "vm-eof-p returns 1 for :eof value."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 :eof)
    (io-exec (cl-cc::make-vm-eof-p :dst :R0 :value :R1) s)
    (assert-equal 1 (cl-cc::vm-reg-get s :R0))))

(deftest io-eof-p-false
  "vm-eof-p returns 0 for non-eof value."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 #\a)
    (io-exec (cl-cc::make-vm-eof-p :dst :R0 :value :R1) s)
    (assert-equal 0 (cl-cc::vm-reg-get s :R0))))

;;; ─── read-char / read-line via handle ──────────────────────────────────────

(deftest io-read-line-from-string-stream
  "vm-read-line reads a line from handle-based input string stream."
  (let ((s (io-vm-full)))
    (cl-cc::vm-reg-set s :R1 "first line")
    (io-exec (cl-cc::make-vm-make-string-stream :dst :R0 :direction :input
                                                 :initial-string :R1) s)
    (let ((handle (cl-cc::vm-reg-get s :R0)))
      (cl-cc::vm-reg-set s :R2 handle)
      (io-exec (cl-cc::make-vm-read-line :dst :R3 :handle :R2) s)
      (assert-equal "first line" (cl-cc::vm-reg-get s :R3)))))
