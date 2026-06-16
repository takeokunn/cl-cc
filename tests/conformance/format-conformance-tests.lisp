;;;; tests/conformance/format-conformance-tests.lisp
;;;; ANSI CL FORMAT Directive Conformance Tests
;;;;
;;;; Tests FORMAT directives that should work per ANSI CL. These run as
;;;; regular conformance tests; native binary parity is tracked separately.

(in-package :cl-cc/test)

(defsuite ansi-conformance-format-suite
  :description "ANSI CL FORMAT Directive Conformance Tests"
  :parent cl-cc-conformance-suite
  :parallel nil)

(in-suite ansi-conformance-format-suite)

;;; ──────────────────────────────────────────────────────────────────────
;;; Helper
;;; ──────────────────────────────────────────────────────────────────────

(defun fmt-run (control-string &rest args)
  "Run FORMAT with CONTROL-STRING and ARGS via cl-cc pipeline. Returns output string."
  (let ((out (make-string-output-stream))
        (forms (mapcar (lambda (arg)
                         (if (or (consp arg) (and arg (not (atom arg))))
                             `(quote ,arg)
                             arg))
                       args)))
    (let ((*standard-output* out))
      (cl-cc:run-string (format nil "(format t ~S~{ ~S~})" control-string forms)))
    (get-output-stream-string out)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Basic Output Directives
;;; ──────────────────────────────────────────────────────────────────────

(deftest format-tilde-a-self-host
  "~A should print values aesthetically without host CL fallback."
  :timeout 30
  :tags '(:format :tilde-a :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "hello 42"
                  (fmt-run "~A ~A" "hello" 42))))

(deftest format-tilde-s-self-host
  "~S should print values with escaping without host CL fallback."
  :timeout 30
  :tags '(:format :tilde-s :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "\"hello\" 42"
                  (fmt-run "~S ~S" "hello" 42))))

(deftest format-tilde-percent-self-host
  "~% should emit newline without host CL."
  :timeout 30
  :tags '(:format :tilde-percent :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal (format nil "a~%b")
                  (fmt-run "a~%b"))))

(deftest format-tilde-ampersand-self-host
  "~& should emit fresh-line without host CL."
  :timeout 30
  :tags '(:format :tilde-ampersand :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~& at start should not emit extra newline
    (assert-equal "" (fmt-run "~&"))))

(deftest format-tilde-tilde-self-host
  "~~ should emit literal tilde without host CL."
  :timeout 30
  :tags '(:format :tilde-tilde :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "~" (fmt-run "~~"))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Numeric Directives
;;; ──────────────────────────────────────────────────────────────────────

(deftest format-tilde-d-self-host
  "~D should print decimal integers without host CL."
  :timeout 30
  :tags '(:format :tilde-d :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "42" (fmt-run "~D" 42))
    (assert-equal "-1" (fmt-run "~D" -1))))

(deftest format-tilde-b-self-host
  "~B should print binary integers without host CL."
  :timeout 30
  :tags '(:format :tilde-b :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "101010" (fmt-run "~B" 42))))

(deftest format-tilde-o-self-host
  "~O should print octal integers without host CL."
  :timeout 30
  :tags '(:format :tilde-o :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "52" (fmt-run "~O" 42))))

(deftest format-tilde-x-self-host
  "~X should print hexadecimal integers without host CL."
  :timeout 30
  :tags '(:format :tilde-x :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "2A" (fmt-run "~X" 42))))

(deftest format-tilde-r-self-host
  "~R should print Roman/English numerals without host CL."
  :timeout 30
  :tags '(:format :tilde-r :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~R with no arg prints cardinal English
    (assert-equal "forty-two" (fmt-run "~R" 42))))

(deftest format-tilde-f-self-host
  "~F should print floating-point numbers without host CL."
  :timeout 30
  :tags '(:format :tilde-f :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "3.14" (fmt-run "~F" 3.14))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Control Flow Directives
;;; ──────────────────────────────────────────────────────────────────────

(deftest format-tilde-asterisk-self-host
  "~* should jump arguments without host CL."
  :timeout 30
  :tags '(:format :tilde-asterisk :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~* skips one arg, ~:* backs up
    (assert-equal "2" (fmt-run "~*~D" 1 2))))

(deftest format-tilde-question-self-host
  "~? should do recursive formatting without host CL."
  :timeout 30
  :tags '(:format :tilde-question :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "hello" (fmt-run "~?" "~A" "hello"))))

(deftest format-tilde-bracket-self-host
  "~[ should do conditional formatting without host CL."
  :timeout 30
  :tags '(:format :tilde-bracket :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "zero" (fmt-run "~[zero~;one~;two~]" 0))
    (assert-equal "one" (fmt-run "~[zero~;one~;two~]" 1))))

(deftest format-tilde-brace-self-host
  "~{ should iterate over list without host CL."
  :timeout 30
  :tags '(:format :tilde-brace :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "abc" (fmt-run "~{~A~}" '("a" "b" "c")))))

(deftest format-tilde-caret-self-host
  "~^ should escape on missing args without host CL."
  :timeout 30
  :tags '(:format :tilde-caret :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~^ inside ~{ suppresses the trailing separator on the LAST element only:
    ;; ~{~A~^, ~} is the canonical comma-join, so this is "a, b, c" (not "ab" —
    ;; the previous expectation encoded a bug where plain ~^ always terminated,
    ;; dropping every separator and the final element).
    (assert-equal "a, b, c"
                  (fmt-run "~{~A~^, ~}" '("a" "b" "c")))))

;;; ──────────────────────────────────────────────────────────────────────
;;; format nil (return as string)
;;; ──────────────────────────────────────────────────────────────────────

(deftest format-nil-self-host
  "(format nil ...) should return formatted string without host CL."
  :timeout 30
  :tags '(:format :nil-destination :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t)
        (result (cl-cc:run-string "(format nil \"hello ~A\" \"world\")")))
    (assert-equal "hello world" result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Format Edge Cases
;;; ──────────────────────────────────────────────────────────────────────

(deftest format-tilde-t-self-host
  "~T should tabulate without host CL."
  :timeout 30
  :tags '(:format :tilde-t :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~4T should tab to column 4
    (assert-equal "    x" (fmt-run "~4Tx"))))

(deftest format-tab-after-rendered-directives-self-host
  "~T should use the current output column after other native directives."
  :timeout 30
  :tags '(:format :tilde-t :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "12  !" (fmt-run "~D~4T!" 12))
    (assert-equal "A   !" (fmt-run "~C~4T!" #\A))
    (assert-equal "1.5   !" (fmt-run "~F~6T!" 1.5))))

(deftest format-tilde-p-self-host
  "~P should pluralize without host CL."
  :timeout 30
  :tags '(:format :tilde-p :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "1 dog" (fmt-run "~D dog~:P" 1))
    (assert-equal "2 dogs" (fmt-run "~D dog~:P" 2))))

(deftest format-tilde-c-self-host
  "~C should format characters without host CL."
  :timeout 30
  :tags '(:format :tilde-c :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "A" (fmt-run "~C" #\A))))

(deftest format-case-conversion-self-host
  "~(...~) should apply ANSI case conversion without host CL fallback."
  :timeout 30
  :tags '(:format :case-conversion :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "hello world" (fmt-run "~(~A~)" "HELLO WORLD"))
    (assert-equal "Hello World" (fmt-run "~:(~A~)" "hello world"))
    (assert-equal "Hello world" (fmt-run "~@(~A~)" "hello world"))
    (assert-equal "HELLO WORLD" (fmt-run "~:@(~A~)" "hello world"))))

(deftest format-at-modifier-self-host
  "~@D should always print sign."
  :timeout 30
  :tags '(:format :at-modifier :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "+42" (fmt-run "~@D" 42))
    (assert-equal "-1" (fmt-run "~@D" -1))))

(deftest format-colon-modifier-self-host
  "~:D should print with commas."
  :timeout 30
  :tags '(:format :colon-modifier :self-host)
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (assert-equal "1,000" (fmt-run "~:D" 1000))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Native Binary FORMAT
;;; ──────────────────────────────────────────────────────────────────────

(deftest format-native-binary-e2e
  "FORMAT should work in native-compiled code (not just VM interpreter)."
  :timeout 60
  :tags '(:format :native :e2e)
  ;; Compile a simple program that uses FORMAT and run it
  (let ((result (cl-cc:run-string
                 "(let ((out (make-string-output-stream)))
                    (format out \"Hello ~A!\" \"World\")
                    (get-output-stream-string out))")))
    (assert-equal "Hello World!" result)))
