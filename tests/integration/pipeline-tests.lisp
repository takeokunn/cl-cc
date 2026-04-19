;;;; tests/integration/pipeline-tests.lisp — Pipeline API Tests
;;;
;;; Tests for compile-expression, compile-string, run-string,
;;; %prescan-in-package, parse-source-for-language, get-stdlib-forms,
;;; run-string-repl, our-eval, and reset-repl-state.

(in-package :cl-cc/test)
(in-suite cl-cc-integration-serial-suite)

;;; ─── compile-expression ─────────────────────────────────────────────────

(deftest pipeline-compile-expression-binop-structure
  "compile-expression returns a well-formed compilation-result for a binop expression."
  (let* ((result (compile-expression '(+ 1 2)))
         (prog   (compilation-result-program result))
         (instrs (vm-program-instructions prog))
         (asm    (compilation-result-assembly result))
         (cps    (compilation-result-cps result)))
    (assert-true (typep result 'cl-cc/compile::compilation-result))
    (assert-true (typep prog 'cl-cc/vm::vm-program))
    (assert-true (> (length instrs) 0))
    (assert-true (stringp asm))
    (assert-true (consp cps))))

(deftest pipeline-compile-expression-constant-halts
  "compile-expression for a constant ends in vm-halt with a consp CPS form."
  (let* ((result (compile-expression 42))
          (instrs (vm-program-instructions (compilation-result-program result)))
          (cps (compilation-result-cps result)))
     (assert-true (consp cps))
     (assert-true (typep (car (last instrs)) 'cl-cc/vm::vm-halt))))

(deftest pipeline-compile-expression-program-uses-optimized-stream
  "compile-expression stores the optimized instruction stream in the VM program while preserving the raw stream separately."
  (let* ((result (compile-expression '(+ 1 2)))
         (program-instrs (vm-program-instructions (compilation-result-program result)))
         (raw-instrs (cl-cc:compilation-result-vm-instructions result))
         (optimized-instrs (cl-cc:compilation-result-optimized-instructions result)))
    (assert-true (equal program-instrs optimized-instrs))
    (assert-true (> (length raw-instrs) 0))))

(deftest-each pipeline-compile-toplevel-forms-defvar-type-env
  "compile-toplevel-forms infers fixnum type for defvar regardless of type-check flag."
  :cases (("with-type-check"
           '((defvar *typed-top-level* 42))
           '*typed-top-level*
           t)
          ("without-type-check"
           '((defvar *typed-top-level-no-check* 42) *typed-top-level-no-check*)
           '*typed-top-level-no-check*
           nil))
  (forms lookup-sym type-check)
  (let ((result (cl-cc/compile::compile-toplevel-forms forms :type-check type-check)))
    (assert-true (typep (cl-cc/compile::compilation-result-type-env result)
                        'cl-cc/type:type-env))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup lookup-sym
                                     (cl-cc/compile::compilation-result-type-env result))
      (assert-true found-p)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                          (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-compile-toplevel-forms-captures-cps
  "compile-toplevel-forms stores a CPS form for top-level input."
  (let ((result (cl-cc/compile::compile-toplevel-forms '((+ 1 2) (- 4 1)))))
    (assert-true (consp (cl-cc/compile::compilation-result-cps result)))))

(deftest pipeline-compile-toplevel-forms-program-uses-optimized-stream
  "compile-toplevel-forms stores optimized instructions in the program while keeping raw instructions in the result payload."
  (let* ((result (cl-cc/compile::compile-toplevel-forms '((+ 1 2) (- 4 1))))
         (program-instrs (vm-program-instructions (cl-cc/compile::compilation-result-program result)))
         (raw-instrs (cl-cc/compile::compilation-result-vm-instructions result))
         (optimized-instrs (cl-cc/compile::compilation-result-optimized-instructions result)))
    (assert-true (equal program-instrs optimized-instrs))
    (assert-true (> (length raw-instrs) 0))))

(deftest-each pipeline-compile-toplevel-forms-defun-type-env
  "compile-toplevel-forms infers function type for defun regardless of type-check flag."
  :cases (("with-type-check"
           '((defun typed-id (x) x) (typed-id 42))
           'typed-id
           t)
          ("without-type-check"
           '((defun typed-id-no-check (x) x) (typed-id-no-check 42))
           'typed-id-no-check
           nil))
  (forms lookup-sym type-check)
  (let ((result (cl-cc/compile::compile-toplevel-forms forms :type-check type-check)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup lookup-sym
                                     (cl-cc/compile::compilation-result-type-env result))
      (assert-true found-p)
      (assert-true (cl-cc/type:type-function-p
                    (cl-cc/type::type-scheme-type scheme))))))

(deftest-each pipeline-the-type-assertions
  "run-string evaluates (the ...) assertions: accepts matching values, signals on mismatch."
  :cases (("fixnum"
           "(the fixnum 42)"              42  "(the fixnum \"oops\")")
          ("refinement"
           "(the (refine fixnum plusp) 42)" 42 "(the (refine fixnum plusp) -1)"))
  (ok-form expected err-form)
  (assert-= expected (run-string ok-form))
  (assert-signals error (run-string err-form)))

(deftest-each pipeline-typed-fixnum-instruction-types
  "Typed fixnum operations compile to the specialized VM instruction types."
  :cases (("add" "(defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y))" 'cl-cc/vm::vm-add)
          ("lt"  "(defun typed-lt  ((x fixnum) (y fixnum)) fixnum (< x y))"  'cl-cc/vm::vm-lt))
  (code expected-type)
  (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (assert-true (some (lambda (i) (typep i expected-type)) instrs))))

;;; ─── compile-string ─────────────────────────────────────────────────────

(deftest-each pipeline-compile-string-returns-result
  "compile-string returns a compilation-result for simple and multi-form inputs."
  :cases (("single-form"   "(+ 1 2)")
          ("multiple-forms" "(defun f (x) x) (f 42)"))
  (expr)
  (assert-true (typep (compile-string expr) 'cl-cc/compile::compilation-result)))

(deftest pipeline-compile-string-custom-pass-pipeline
  "compile-string forwards a string pass pipeline to optimizer core."
  (let* ((baseline (compile-string "(+ 1 2)" :target :vm))
         (result (compile-string "(+ 1 2)" :target :vm :pass-pipeline "fold,dce")))
    (assert-true (typep result 'cl-cc/compile::compilation-result))
    (assert-true
     (<= (length (cl-cc:compilation-result-optimized-instructions result))
         (length (cl-cc:compilation-result-optimized-instructions baseline))))))

;;; ─── run-string ─────────────────────────────────────────────────────────

(deftest-each pipeline-run-string-forms
  "run-string evaluates various expression forms."
  :cases ((arithmetic  3  "(+ 1 2)")
          (literal    42  "42")
          (nested     12  "(+ (* 2 3) (- 7 1))")
          (let-form    5  "(let ((x 2) (y 3)) (+ x y))")
          (if-true     1  "(if t 1 2)")
          (if-false    2  "(if nil 1 2)")
          (lambda      9  "((lambda (x) (* x x)) 3)"))
  (expected expr)
  (assert-= expected (run-string expr)))

(deftest-each pipeline-run-string-output-options
  "run-string forwards timing/stats/trace-json output without altering the evaluation result."
  :cases (("timings"
           (list :print-pass-timings t :timing-stream)
           (lambda (text) (assert-true (search "OPT-PASS-FOLD" (string-upcase text)))))
          ("stats"
           (list :print-pass-stats t :stats-stream)
           (lambda (text)
             (let ((u (string-upcase text)))
               (assert-true (search "OPT-PASS-FOLD" u))
               (assert-true (search "BEFORE=" u)))))
          ("trace-json"
           (list :trace-json-stream)
           (lambda (text)
             (assert-true (search "\"traceEvents\"" text))
             (assert-true (search "OPT-PASS-FOLD" text)))))
  (kwarg-prefix verify)
  (let* ((stream (make-string-output-stream))
         (kwargs (append kwarg-prefix (list stream))))
    (assert-= 3 (apply #'run-string "(+ 1 2)" :pass-pipeline "fold" kwargs))
    (funcall verify (get-output-stream-string stream))))

;;; ─── %prescan-in-package ────────────────────────────────────────────────

(deftest-each pipeline-prescan-in-package-behavior
  "%prescan-in-package extracts the package name from in-package forms; nil for others."
  :cases (("keyword-form"
           "(in-package :cl-cc)"
           (lambda (result)
             (assert-string= "CL-CC" (string-upcase result))))
          ("string-form"
           "(in-package \"CL-CC\")"
           (lambda (result)
             (assert-string= "CL-CC" (string-upcase result))))
          ("non-package"
           "(defun f (x) x)"
           (lambda (result)
             (assert-null result)))
          ("with-comment"
           (format nil ";;; header comment~%(in-package :cl-cc)")
           (lambda (result)
             (assert-true (stringp result)))))
  (source verify)
  (funcall verify (cl-cc::%prescan-in-package source)))

;;; ─── parse-source-for-language ──────────────────────────────────────────

(deftest-each pipeline-parse-source-for-language
  "parse-source-for-language: :lisp parses one/multiple forms; unknown signals error."
  :cases (("single-form"
           "(+ 1 2)" :lisp 1
           (lambda (forms)
             (assert-equal '(+ 1 2) (first forms))))
          ("multiple-forms"
           "(+ 1 2) (* 3 4)" :lisp 2
           (lambda (_forms)
             (declare (ignore _forms)))))
  (source lang expected-count verify)
  (let ((forms (cl-cc::parse-source-for-language source lang)))
    (assert-= expected-count (length forms))
    (funcall verify forms)))

(deftest pipeline-parse-unknown-language-signals
  "parse-source-for-language signals error for unknown language."
  (assert-signals error
    (cl-cc::parse-source-for-language "(+ 1 2)" :unknown)))

;;; ─── get-stdlib-forms ───────────────────────────────────────────────────

(deftest pipeline-stdlib-forms-content
  "get-stdlib-forms returns a non-empty list containing key definitions."
  (let ((forms (cl-cc::get-stdlib-forms)))
    (assert-true (> (length forms) 10))
    (assert-true (cl:some (lambda (f)
                            (and (consp f) (eq (car f) 'defun)
                                 (eq (cadr f) 'mapcar)))
                          forms))
    (assert-true (cl:some (lambda (f)
                            (and (consp f) (eq (car f) 'defun)
                                 (eq (cadr f) 'reduce)))
                          forms))))

(deftest pipeline-stdlib-forms-return-fresh-tree
  "get-stdlib-forms returns a fresh nested tree on each call.

Mutating a nested cons in one caller must not affect later callers, otherwise
parallel stdlib-heavy compilation can leak state across workers."
  (let* ((forms-a (cl-cc::get-stdlib-forms))
         (forms-b (cl-cc::get-stdlib-forms))
         (defun-a (cl:find-if (lambda (f) (and (consp f) (eq (car f) 'defun))) forms-a))
         (defun-b (cl:find-if (lambda (f) (and (consp f) (eq (car f) 'defun))) forms-b)))
    (assert-true (consp defun-a))
    (assert-true (consp defun-b))
    (assert-false (eq defun-a defun-b))
    (let ((original-name (second defun-b)))
      (setf (second defun-a) 'mutated-stdlib-name)
      (assert-eq original-name (second defun-b)))))

;;; ─── our-eval ───────────────────────────────────────────────────────────

(deftest-each pipeline-our-eval-forms
  "our-eval evaluates arithmetic, quoted data, and conditionals."
  :cases ((arithmetic  6          '(* 2 3))
          (quote-data  '(a b c)   '(quote (a b c)))
          (if-form     10         '(if t 10 20)))
  (expected expr)
  (assert-equal expected (cl-cc::our-eval expr)))

(deftest pipeline-our-eval-uses-cps-path-for-simple-expression
  "our-eval evaluates simple expressions via the CPS host path without calling compile-expression." 
  (let ((orig (symbol-function 'cl-cc::compile-expression)))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-cc::compile-expression)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (error "compile-expression should not run for CPS-safe our-eval")))
           (assert-= 3 (cl-cc::our-eval '(+ 1 2))))
      (setf (symbol-function 'cl-cc::compile-expression) orig))))

(deftest pipeline-our-eval-falls-back-to-vm-for-definitions
  "our-eval still uses the compile→VM path for top-level definition forms." 
  (let ((orig (symbol-function 'cl-cc::compile-expression))
        (called nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-cc::compile-expression)
                 (lambda (&rest args)
                   (setf called t)
                   (apply orig args)))
           (assert-eq '*pipeline-cps-fallback*
                      (cl-cc::our-eval '(defvar *pipeline-cps-fallback* 7)))
           (assert-true called))
      (ignore-errors (makunbound '*pipeline-cps-fallback*))
      (setf (symbol-function 'cl-cc::compile-expression) orig))))

;;; run-string-repl, reset-repl-state, stdlib tests moved to pipeline-repl-tests.lisp.
