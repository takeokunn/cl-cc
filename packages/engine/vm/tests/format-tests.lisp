;;;; tests/unit/vm/format-tests.lisp — VM formatted output and reader tests

(in-package :cl-cc/test)

(defsuite format-suite
  :description "VM formatted output and reader operation tests"
  :parent cl-cc-unit-suite)

(in-suite format-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun fmt-vm (&optional (out (make-string-output-stream)))
  "Create a vm-state with a string output stream for capture."
  (make-instance 'cl-cc/vm::vm-state :output-stream out))

(defun fmt-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm::execute-instruction inst state 0 (make-hash-table :test #'equal)))

(defun fmt-capture (state)
  "Get captured output from the vm-state's output stream."
  (get-output-stream-string (cl-cc/vm::vm-output-stream state)))

;;; ─── write-to-string / princ-to-string ────────────────────────────────────

(deftest-each fmt-write-to-string
  "vm-write-to-string converts values to their printed representation."
  :cases (("number" 42   "42")
          ("symbol" :test ":TEST")
          ("string" "hi" "\"hi\""))
  (value expected)
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc::make-vm-write-to-string-inst :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

(deftest-each fmt-princ-to-string
  "vm-princ-to-string returns unescaped printed representations."
  :cases (("number" 42 "42")
          ("string" "hi" "hi"))
  (value expected)
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc::make-vm-princ-to-string-inst :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── princ / prin1 / print / terpri / fresh-line ─────────────────────────

(deftest-each fmt-princ-values
  "vm-princ prints values without escaping."
  :cases (("number" 42 "42")
          ("string" "hello" "hello"))
  (value expected)
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc::make-vm-princ :src :R1) s)
    (assert-equal expected (fmt-capture s))))

(deftest fmt-prin1-print-cases
  "vm-prin1 prints strings with quotes; vm-print emits object to output stream."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (fmt-exec (cl-cc::make-vm-prin1 :src :R1) s)
    (assert-equal "\"hello\"" (fmt-capture s)))
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 42)
    (fmt-exec (cl-cc::make-vm-print-inst :src :R1) s)
    (assert-true (search "42" (fmt-capture s)))))

(deftest fmt-newline-cases
  "vm-terpri outputs newline; vm-fresh-line outputs newline after prior output."
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc::make-vm-terpri-inst) s)
    (assert-equal (string #\Newline) (fmt-capture s)))
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "x")
    (fmt-exec (cl-cc::make-vm-princ :src :R1) s)
    (fmt-exec (cl-cc::make-vm-fresh-line-inst) s)
    (assert-equal (concatenate 'string "x" (string #\Newline))
                  (fmt-capture s))))

;;; ─── format ───────────────────────────────────────────────────────────────

(deftest fmt-format-cases
  "vm-format-inst: no args; ~A args; ~D directive."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello world")
    (fmt-exec (cl-cc::make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (assert-equal "hello world" (cl-cc/vm::vm-reg-get s :R0)))
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~A is ~A")
    (cl-cc/vm::vm-reg-set s :R2 "answer")
    (cl-cc/vm::vm-reg-set s :R3 42)
    (fmt-exec (cl-cc::make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2 :R3)) s)
    (assert-equal "answer is 42" (cl-cc/vm::vm-reg-get s :R0)))
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "count: ~D")
    (cl-cc/vm::vm-reg-set s :R2 99)
    (fmt-exec (cl-cc::make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "count: 99" (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── String Output Stream ────────────────────────────────────────────────

(deftest fmt-string-output-stream-cases
  "String output stream: roundtrip single write; multiple writes accumulate."
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc::make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (streamp stream))
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (fmt-exec (cl-cc::make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (fmt-exec (cl-cc::make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (assert-equal "hello" (cl-cc/vm::vm-reg-get s :R3))))
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc::make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (fmt-exec (cl-cc::make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (cl-cc/vm::vm-reg-set s :R2 " world")
      (fmt-exec (cl-cc::make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (fmt-exec (cl-cc::make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (assert-equal "hello world" (cl-cc/vm::vm-reg-get s :R3)))))

;;; ─── Reader Instructions (use cl-cc's own lexer/parser) ──────────────────

(deftest fmt-read-from-string-cases
  "vm-read-from-string: number with values-list; symbol uppercased; list; empty string → nil."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "42")
    (fmt-exec (cl-cc::make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal 42 (cl-cc/vm::vm-reg-get s :R0))
    (assert-equal '(42 2) (cl-cc/vm::vm-values-list s)))
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (fmt-exec (cl-cc::make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal "HELLO" (symbol-name (cl-cc/vm::vm-reg-get s :R0))))
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "(1 2 3)")
    (fmt-exec (cl-cc::make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (listp result))
      (assert-equal 3 (length result))))
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "")
    (fmt-exec (cl-cc::make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal nil (cl-cc/vm::vm-reg-get s :R0))
    (assert-equal '(nil 0) (cl-cc/vm::vm-values-list s))))

(deftest fmt-read-sexp-from-stream
  "vm-read-sexp reads the first form from a CL input stream."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 (make-string-input-stream "(1 2 3)"))
    (fmt-exec (cl-cc::make-vm-read-sexp-inst :dst :R0 :src :R1) s)
    (assert-equal '(1 2 3) (cl-cc/vm::vm-reg-get s :R0))))
