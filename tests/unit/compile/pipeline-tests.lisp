;;;; tests/unit/compile/pipeline-tests.lisp — Pipeline API Tests
;;;
;;; Tests for compile-expression, compile-string, run-string,
;;; %prescan-in-package, parse-source-for-language, get-stdlib-forms,
;;; run-string-repl, our-eval, and reset-repl-state.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── compile-expression ─────────────────────────────────────────────────

(deftest pipeline-compile-expression-returns-result
  "compile-expression returns a compilation-result struct."
  (let ((result (compile-expression '(+ 1 2))))
    (assert-true (typep result 'cl-cc::compilation-result))))

(deftest pipeline-compile-expression-has-program
  "compilation-result contains a vm-program."
  (let* ((result (compile-expression '(+ 1 2)))
         (prog (compilation-result-program result)))
    (assert-true (typep prog 'cl-cc::vm-program))))

(deftest pipeline-compile-expression-has-instructions
  "The vm-program has a non-empty instruction list."
  (let* ((result (compile-expression '(+ 1 2)))
         (instrs (vm-program-instructions (compilation-result-program result))))
    (assert-true (> (length instrs) 0))))

(deftest pipeline-compile-expression-has-assembly
  "compilation-result includes assembly output."
  (let* ((result (compile-expression '(+ 1 2)))
         (asm (compilation-result-assembly result)))
    (assert-true (stringp asm))))

(deftest pipeline-compile-expression-literal
  "Compiling a literal produces instructions ending with halt."
  (let* ((result (compile-expression 42))
         (instrs (vm-program-instructions (compilation-result-program result))))
    (assert-true (typep (car (last instrs)) 'cl-cc::vm-halt))))

(deftest pipeline-compile-expression-cps
  "compilation-result includes CPS transformation when possible."
  (let* ((result (compile-expression '(+ 1 2)))
         (cps (compilation-result-cps result)))
    (assert-true (or (null cps) (consp cps)))))

;;; ─── compile-string ─────────────────────────────────────────────────────

(deftest pipeline-compile-string-basic
  "compile-string compiles a string expression."
  (let ((result (compile-string "(+ 1 2)")))
    (assert-true (typep result 'cl-cc::compilation-result))))

(deftest pipeline-compile-string-multiple-forms
  "compile-string handles multiple forms."
  (let ((result (compile-string "(defun f (x) x) (f 42)")))
    (assert-true (typep result 'cl-cc::compilation-result))))

;;; ─── run-string ─────────────────────────────────────────────────────────

(deftest pipeline-run-string-arithmetic
  "run-string evaluates arithmetic expressions."
  (assert-= 3 (run-string "(+ 1 2)")))

(deftest pipeline-run-string-literal
  "run-string returns literal values."
  (assert-= 42 (run-string "42")))

(deftest pipeline-run-string-nested
  "run-string handles nested expressions."
  (assert-= 12 (run-string "(+ (* 2 3) (- 7 1))")))

(deftest pipeline-run-string-let
  "run-string handles let forms."
  (assert-= 5 (run-string "(let ((x 2) (y 3)) (+ x y))")))

(deftest pipeline-run-string-if-true
  "run-string handles if with true condition."
  (assert-= 1 (run-string "(if t 1 2)")))

(deftest pipeline-run-string-if-false
  "run-string handles if with false condition."
  (assert-= 2 (run-string "(if nil 1 2)")))

(deftest pipeline-run-string-lambda
  "run-string handles lambda calls."
  (assert-= 9 (run-string "((lambda (x) (* x x)) 3)")))

;;; ─── %prescan-in-package ────────────────────────────────────────────────

(deftest pipeline-prescan-keyword-package
  "%prescan-in-package extracts keyword package name."
  (let ((result (cl-cc::%prescan-in-package "(in-package :cl-cc)")))
    (assert-true (stringp result))
    (assert-string= "CL-CC" (string-upcase result))))

(deftest pipeline-prescan-string-package
  "%prescan-in-package extracts string package name."
  (assert-equal "CL-CC"
    (cl-cc::%prescan-in-package "(in-package \"CL-CC\")")))

(deftest pipeline-prescan-no-package
  "%prescan-in-package returns nil when no in-package."
  (assert-null (cl-cc::%prescan-in-package "(defun f (x) x)")))

(deftest pipeline-prescan-with-preceding-code
  "%prescan-in-package finds in-package after other forms."
  (let ((result (cl-cc::%prescan-in-package
                  (format nil ";;; header comment~%(in-package :cl-cc)"))))
    (assert-true (stringp result))))

;;; ─── parse-source-for-language ──────────────────────────────────────────

(deftest pipeline-parse-lisp-language
  "parse-source-for-language returns forms for :lisp."
  (let ((forms (cl-cc::parse-source-for-language "(+ 1 2)" :lisp)))
    (assert-true (consp forms))
    (assert-equal '(+ 1 2) (first forms))))

(deftest pipeline-parse-lisp-multiple
  "parse-source-for-language returns multiple forms."
  (let ((forms (cl-cc::parse-source-for-language "(+ 1 2) (* 3 4)" :lisp)))
    (assert-= 2 (length forms))))

(deftest pipeline-parse-unknown-language-signals
  "parse-source-for-language signals error for unknown language."
  (assert-signals error
    (cl-cc::parse-source-for-language "(+ 1 2)" :unknown)))

;;; ─── get-stdlib-forms ───────────────────────────────────────────────────

(deftest pipeline-stdlib-forms-nonempty
  "get-stdlib-forms returns a non-empty list of forms."
  (let ((forms (cl-cc::get-stdlib-forms)))
    (assert-true (> (length forms) 10))))

(deftest pipeline-stdlib-has-mapcar
  "Standard library includes mapcar definition."
  (let ((forms (cl-cc::get-stdlib-forms)))
    (assert-true (cl:some (lambda (f)
                            (and (consp f) (eq (car f) 'defun)
                                 (eq (cadr f) 'mapcar)))
                          forms))))

(deftest pipeline-stdlib-has-reduce
  "Standard library includes reduce definition."
  (let ((forms (cl-cc::get-stdlib-forms)))
    (assert-true (cl:some (lambda (f)
                            (and (consp f) (eq (car f) 'defun)
                                 (eq (cadr f) 'reduce)))
                          forms))))

;;; ─── our-eval ───────────────────────────────────────────────────────────

(deftest pipeline-our-eval-arithmetic
  "our-eval evaluates arithmetic."
  (assert-= 6 (cl-cc::our-eval '(* 2 3))))

(deftest pipeline-our-eval-quote
  "our-eval handles quoted data."
  (assert-equal '(a b c) (cl-cc::our-eval '(quote (a b c)))))

(deftest pipeline-our-eval-if
  "our-eval handles conditionals."
  (assert-= 10 (cl-cc::our-eval '(if t 10 20))))

;;; ─── run-string-repl (persistent state) ─────────────────────────────────

(deftest pipeline-repl-simple-eval
  "run-string-repl evaluates a simple expression."
  (reset-repl-state)
  (let ((result (run-string-repl "42")))
    (assert-= 42 result)
    (reset-repl-state)))

(deftest pipeline-repl-defun-and-call
  "run-string-repl persists defun across calls."
  (reset-repl-state)
  (run-string-repl "(defun repl-test-double (x) (* x 2))")
  (let ((result (run-string-repl "(repl-test-double 21)")))
    (assert-= 42 result)
    (reset-repl-state)))

(deftest pipeline-repl-defvar-persists
  "run-string-repl persists defvar across calls."
  (reset-repl-state)
  (run-string-repl "(defvar *repl-test-val* 99)")
  (let ((result (run-string-repl "*repl-test-val*")))
    (assert-= 99 result)
    (reset-repl-state)))

;;; ─── reset-repl-state ──────────────────────────────────────────────────

(deftest pipeline-reset-clears-state
  "reset-repl-state clears all persistent REPL variables."
  (reset-repl-state)
  (run-string-repl "42")
  (assert-true (not (null cl-cc::*repl-vm-state*)))
  (reset-repl-state)
  (assert-null cl-cc::*repl-vm-state*)
  (assert-null cl-cc::*repl-pool-instructions*)
  (assert-null cl-cc::*repl-pool-labels*))

;;; ─── compile-string-with-stdlib ─────────────────────────────────────────

(deftest pipeline-compile-with-stdlib
  "compile-string-with-stdlib includes stdlib definitions."
  (let ((result (cl-cc::compile-string-with-stdlib "(+ 1 2)" :target :vm)))
    (assert-true (typep result 'cl-cc::compilation-result))))

;;; ─── run-string with :stdlib ────────────────────────────────────────────

(deftest pipeline-run-string-stdlib-mapcar
  "run-string with :stdlib enables mapcar."
  (let ((result (run-string "(mapcar (lambda (x) (+ x 1)) '(1 2 3))" :stdlib t)))
    (assert-equal '(2 3 4) result)))

(deftest pipeline-run-string-stdlib-reduce
  "run-string with :stdlib enables reduce."
  (let ((result (run-string "(reduce (lambda (a b) (+ a b)) '(1 2 3 4) 0 t)" :stdlib t)))
    (assert-= 10 result)))
