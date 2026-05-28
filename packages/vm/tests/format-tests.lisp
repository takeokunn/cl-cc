;;;; tests/unit/vm/format-tests.lisp — VM formatted output and reader tests

(in-package :cl-cc/test)

(defsuite format-suite
  :description "VM formatted output and reader operation tests"
  :parent cl-cc-unit-suite)

(in-suite format-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun fmt-vm (&optional (out (make-string-output-stream)))
  "Create a vm-io-state with a string output stream for capture."
  (make-instance 'cl-cc/vm::vm-io-state :output-stream out))

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
    (fmt-exec (cl-cc:make-vm-write-to-string-inst :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

(deftest-each fmt-princ-to-string
  "vm-princ-to-string returns unescaped printed representations."
  :cases (("number" 42 "42")
          ("string" "hi" "hi"))
  (value expected)
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-princ-to-string-inst :dst :R0 :src :R1) s)
    (assert-equal expected (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── princ / prin1 / print / terpri / fresh-line ─────────────────────────

(deftest-each fmt-princ-values
  "vm-princ prints values without escaping."
  :cases (("number" 42 "42")
          ("string" "hello" "hello"))
  (value expected)
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-princ :src :R1) s)
    (assert-equal expected (fmt-capture s))))

(deftest fmt-prin1-prints-strings-with-quotes
  "vm-prin1 prints string values with surrounding double-quote characters."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (fmt-exec (cl-cc:make-vm-prin1 :src :R1) s)
    (assert-equal "\"hello\"" (fmt-capture s))))

(deftest fmt-print-inst-emits-object-representation
  "vm-print-inst emits the printed representation of an object to the output stream."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 42)
    (fmt-exec (cl-cc:make-vm-print-inst :src :R1) s)
    (assert-true (search "42" (fmt-capture s)))))

(deftest fmt-terpri-outputs-newline
  "vm-terpri emits a newline character to the output stream."
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc:make-vm-terpri-inst) s)
    (assert-equal (string #\Newline) (fmt-capture s))))

(deftest fmt-fresh-line-outputs-newline-after-prior-output
  "vm-fresh-line emits a newline character after existing output on the stream."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "x")
    (fmt-exec (cl-cc:make-vm-princ :src :R1) s)
    (fmt-exec (cl-cc:make-vm-fresh-line-inst) s)
    (assert-equal (concatenate 'string "x" (string #\Newline))
                  (fmt-capture s))))

;;; ─── format ───────────────────────────────────────────────────────────────

(deftest fmt-format-no-args-passes-string-through
  "vm-format-inst with no arg-regs stores the format string unchanged."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello world")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (assert-equal "hello world" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-format-tilde-a-interpolates-args
  "vm-format-inst with ~A directives interpolates the argument registers."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~A is ~A")
    (cl-cc/vm::vm-reg-set s :R2 "answer")
    (cl-cc/vm::vm-reg-set s :R3 42)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2 :R3)) s)
    (assert-equal "answer is 42" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-format-tilde-d-formats-integer
  "vm-format-inst with ~D directive formats an integer argument."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "count: ~D")
    (cl-cc/vm::vm-reg-set s :R2 99)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "count: 99" (cl-cc/vm::vm-reg-get s :R0))))

;;; ─── Wave 2: ~I and ~_ directive tests ─────────────────────────────────────

(deftest fmt-directive-tilde-i
  "~3I indents 3 spaces (new Wave 2 directive)."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "x~3Iy")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (assert-equal "x   y" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-underscore
  "~_ emits newline (new Wave 2 directive)."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 (format nil "a~_b"))
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (search "a" result))
      (assert-true (search "b" result)))))

;;; ─── format directive coverage tests ───────────────────────────────────────

(deftest fmt-directive-tilde-percent
  "~% emits newline."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "a~%b")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (search "a" result))
      (assert-true (search "b" result))
      (assert-true (> (length result) 3)))))

(deftest fmt-directive-tilde-ampersand
  "~& emits fresh-line."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "a~&b")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (assert-equal (format nil "a~&b") (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-tilde
  "~~ emits literal tilde."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~~hello~~")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (assert-equal "~hello~" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-b-o-x
  "~B ~O ~X format in binary/octal/hex."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~B ~O ~X")
    (cl-cc/vm::vm-reg-set s :R2 42)
    (cl-cc/vm::vm-reg-set s :R3 42)
    (cl-cc/vm::vm-reg-set s :R4 42)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2 :R3 :R4)) s)
    (assert-equal "101010 52 2A" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-f
  "~F formats floating-point."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~F")
    (cl-cc/vm::vm-reg-set s :R2 3.14d0)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (search "3.14" result)))))

(deftest fmt-directive-tilde-c
  "~C formats character."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~C")
    (cl-cc/vm::vm-reg-set s :R2 #\A)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "A" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-s
  "~S formats with escaping."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~S")
    (cl-cc/vm::vm-reg-set s :R2 "hello")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "\"hello\"" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-t
  "~T tabulates to column."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "a~4Tb")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (assert-equal "a   b" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-asterisk
  "~* jumps arguments."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~*~D")
    (cl-cc/vm::vm-reg-set s :R2 1)
    (cl-cc/vm::vm-reg-set s :R3 2)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2 :R3)) s)
    (assert-equal "2" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-question
  "~? does recursive formatting."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~?")
    (cl-cc/vm::vm-reg-set s :R2 "~A")
    (cl-cc/vm::vm-reg-set s :R3 "hello")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2 :R3)) s)
    (assert-equal "hello" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-bracket
  "~[ does conditional formatting."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~[zero~;one~;two~]")
    (cl-cc/vm::vm-reg-set s :R2 1)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "one" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-brace
  "~{ iterates over list."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~{~A~}")
    (cl-cc/vm::vm-reg-set s :R2 '(#\a #\b #\c))
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "abc" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-caret
  "~^ escapes when no more args."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~{~A~^, ~}")
    (cl-cc/vm::vm-reg-set s :R2 '(#\a #\b #\c))
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "a, b, c" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-at-d
  "~@D always prints sign."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~@D")
    (cl-cc/vm::vm-reg-set s :R2 42)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "+42" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-colon-d
  "~:D prints with commas."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~:D")
    (cl-cc/vm::vm-reg-set s :R2 1000)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (assert-equal "1,000" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-i
  "~I indents (new Wave 2 directive)."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "x~4Iy")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    ;; ~4I indents 4 spaces from current position
    (assert-equal "x    y" (cl-cc/vm::vm-reg-get s :R0))))

(deftest fmt-directive-tilde-underscore
  "~_ emits newline (new Wave 2 directive)."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "a~_b")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (search "a" result))
      (assert-true (search "b" result))
      (assert-true (> (length result) 3)))))

;;; ─── String Output Stream ────────────────────────────────────────────────

(deftest fmt-string-output-stream-single-write-roundtrips
  "A single write to a string output stream is recovered intact by get-output-stream-string."
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (streamp stream))
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (fmt-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (fmt-exec (cl-cc:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (assert-equal "hello" (cl-cc/vm::vm-reg-get s :R3)))))

(deftest fmt-string-output-stream-multiple-writes-accumulate
  "Multiple writes to a string output stream concatenate in the buffer."
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (fmt-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (cl-cc/vm::vm-reg-set s :R2 " world")
      (fmt-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (fmt-exec (cl-cc:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (assert-equal "hello world" (cl-cc/vm::vm-reg-get s :R3)))))

;;; ─── Reader Instructions (use cl-cc's own lexer/parser) ──────────────────

(deftest fmt-read-from-string-number-sets-values-list
  "vm-read-from-string parses a number and stores the position in the values list."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "42")
    (fmt-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal 42 (cl-cc/vm::vm-reg-get s :R0))
    (assert-equal '(42 2) (cl-cc/vm::vm-values-list s))))

(deftest fmt-read-from-string-symbol-is-uppercased
  "vm-read-from-string interns the symbol name in uppercase."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (fmt-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal "HELLO" (symbol-name (cl-cc/vm::vm-reg-get s :R0)))))

(deftest fmt-read-from-string-list-produces-list-value
  "vm-read-from-string parses a parenthesized list into a proper list."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "(1 2 3)")
    (fmt-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (assert-true (listp result))
      (assert-equal 3 (length result)))))

(deftest fmt-read-from-string-empty-string-yields-nil
  "vm-read-from-string on an empty string stores nil with position 0 in the values list."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "")
    (fmt-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (assert-equal nil (cl-cc/vm::vm-reg-get s :R0))
    (assert-equal '(nil 0) (cl-cc/vm::vm-values-list s))))

(deftest fmt-read-sexp-from-stream
  "vm-read-sexp reads the first form from a CL input stream."
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 (make-string-input-stream "(1 2 3)"))
    (fmt-exec (cl-cc:make-vm-read-sexp-inst :dst :R0 :src :R1) s)
    (assert-equal '(1 2 3) (cl-cc/vm::vm-reg-get s :R0))))
