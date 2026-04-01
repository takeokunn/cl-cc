;;;; tests/unit/compile/pipeline-tests.lisp — Pipeline API Tests
;;;
;;; Tests for compile-expression, compile-string, run-string,
;;; %prescan-in-package, parse-source-for-language, get-stdlib-forms,
;;; run-string-repl, our-eval, and reset-repl-state.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── compile-expression ─────────────────────────────────────────────────

(deftest pipeline-compile-expression-properties
  "compile-expression returns a well-formed compilation-result."
  (let* ((result (compile-expression '(+ 1 2)))
         (prog   (compilation-result-program result))
         (instrs (vm-program-instructions prog))
         (asm    (compilation-result-assembly result))
         (cps    (compilation-result-cps result)))
    (assert-true (typep result 'cl-cc::compilation-result))
    (assert-true (typep prog 'cl-cc::vm-program))
    (assert-true (> (length instrs) 0))
    (assert-true (stringp asm))
    (assert-true (consp cps)))
  (let* ((result (compile-expression 42))
         (instrs (vm-program-instructions (compilation-result-program result)))
         (cps (compilation-result-cps result)))
    (assert-true (consp cps))
    (assert-true (typep (car (last instrs)) 'cl-cc::vm-halt))))

(deftest pipeline-compile-toplevel-forms-captures-type-env
  "compile-toplevel-forms retains the inferred top-level type environment."
  (let ((result (cl-cc::compile-toplevel-forms '((defvar *typed-top-level* 42))
                                                :type-check t)))
    (assert-true (typep (cl-cc::compilation-result-type-env result)
                        'cl-cc/type:type-env))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup '*typed-top-level*
                                     (cl-cc::compilation-result-type-env result))
      (assert-true found-p)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                          (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-compile-toplevel-forms-captures-cps
  "compile-toplevel-forms stores a CPS form for top-level input."
  (let ((result (cl-cc::compile-toplevel-forms '((+ 1 2) (- 4 1)))))
    (assert-true (consp (cl-cc::compilation-result-cps result)))))

(deftest pipeline-compile-toplevel-forms-captures-defun-type-env
  "compile-toplevel-forms records inferred defun types for later forms."
  (let ((result (cl-cc::compile-toplevel-forms
                 '((defun typed-id (x) x)
                   (typed-id 42))
                 :type-check t)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup 'typed-id
                                     (cl-cc::compilation-result-type-env result))
      (assert-true found-p)
      (assert-true (cl-cc/type:type-function-p
                    (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-compile-toplevel-forms-records-defun-type-without-type-check
  "compile-toplevel-forms still records inferred defun types when type-check is off."
  (let ((result (cl-cc::compile-toplevel-forms
                 '((defun typed-id-no-check (x) x)
                   (typed-id-no-check 42)))))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup 'typed-id-no-check
                                     (cl-cc::compilation-result-type-env result))
      (assert-true found-p)
      (assert-true (cl-cc/type:type-function-p
                    (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-compile-toplevel-forms-records-defvar-type-without-type-check
  "compile-toplevel-forms still records inferred defvar types when type-check is off."
  (let ((result (cl-cc::compile-toplevel-forms
                 '((defvar *typed-top-level-no-check* 42)
                   *typed-top-level-no-check*)))))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup '*typed-top-level-no-check*
                                     (cl-cc::compilation-result-type-env result))
      (assert-true found-p)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                          (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-the-runtime-assertion
  "run-string executes (the ...) assertions and signals on mismatch."
  (assert-= 42 (run-string "(the fixnum 42)"))
  (assert-signals error
    (run-string "(the fixnum \"oops\")")))

(deftest pipeline-the-refinement-runtime-assertion
  "run-string accepts refinement assertions and rejects predicate failures."
  (assert-= 42 (run-string "(the (refine fixnum plusp) 42)"))
  (assert-signals error
    (run-string "(the (refine fixnum plusp) -1)")))

(deftest pipeline-typed-fixnum-defun-folds-checks
  "Typed fixnum parameters no longer emit vm-typep checks in the function body."
  (let* ((result (compile-string
                  "(defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y))"))
         (prog (compilation-result-program result))
         (instrs (vm-program-instructions prog)))
    (assert-true (notany (lambda (i) (typep i 'cl-cc::vm-typep)) instrs))))

(deftest pipeline-typed-fixnum-compare-fast-path
  "Typed fixnum comparisons emit the specialized compare VM instruction without vm-typep."
  (let* ((ctx (make-instance 'cl-cc::compiler-context))
         (env0 (cl-cc/type:type-env-empty))
         (env1 (cl-cc/type:type-env-extend 'x
                                           (cl-cc/type:type-to-scheme cl-cc/type:type-int)
                                           env0))
         (env2 (cl-cc/type:type-env-extend 'y
                                           (cl-cc/type:type-to-scheme cl-cc/type:type-int)
                                           env1))
         (ast (make-ast-binop :op '<
                              :lhs (make-ast-var :name 'x)
                              :rhs (make-ast-var :name 'y))))
    (setf (cl-cc::ctx-type-env ctx) env2)
    (compile-ast ast ctx)
    (let ((instrs (nreverse (copy-list (cl-cc::ctx-instructions ctx)))))
      (assert-true (notany (lambda (i) (typep i 'cl-cc::vm-typep)) instrs))
      (assert-true (some (lambda (i) (typep i 'cl-cc::vm-lt)) instrs)))))

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

(deftest-each pipeline-run-string-forms
  "run-string evaluates various expression forms."
  :cases ((arithmetic  3  "(+ 1 2)")
          (literal    42  "42")
          (nested     12  "(+ (* 2 3) (- 7 1))")
          (let-form    5  "(let ((x 2) (y 3)) (+ x y))")
          (if-true     1  "(if t 1 2)")
          (if-false    2  "(if nil 1 2)")
          (lambda      9  "((lambda (x) (* x x)) 3)"))
  (assert-= expected (run-string expr)))

;;; ─── %prescan-in-package ────────────────────────────────────────────────

(deftest pipeline-prescan-in-package-behavior
  "%prescan-in-package extracts package names in all supported forms."
  (let ((kw-result (cl-cc::%prescan-in-package "(in-package :cl-cc)")))
    (assert-true (stringp kw-result))
    (assert-string= "CL-CC" (string-upcase kw-result)))
  (assert-equal "CL-CC"
    (cl-cc::%prescan-in-package "(in-package \"CL-CC\")"))
  (assert-null (cl-cc::%prescan-in-package "(defun f (x) x)"))
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

;;; ─── our-eval ───────────────────────────────────────────────────────────

(deftest-each pipeline-our-eval-forms
  "our-eval evaluates arithmetic, quoted data, and conditionals."
  :cases ((arithmetic  6          '(* 2 3))
          (quote-data  '(a b c)   '(quote (a b c)))
          (if-form     10         '(if t 10 20)))
  (assert-equal expected (cl-cc::our-eval expr)))

;;; ─── run-string-repl (persistent state) ─────────────────────────────────

(deftest pipeline-repl-simple-eval
  "run-string-repl evaluates a simple expression."
  (with-reset-repl-state
    (let ((result (run-string-repl "42")))
      (assert-= 42 result))))

(deftest pipeline-repl-defun-and-call
  "run-string-repl persists defun across calls."
  (with-reset-repl-state
    (run-string-repl "(defun repl-test-double (x) (* x 2))")
    (let ((result (run-string-repl "(repl-test-double 21)")))
      (assert-= 42 result))))

(deftest pipeline-repl-defvar-persists
  "run-string-repl persists defvar across calls."
  (with-reset-repl-state
    (run-string-repl "(defvar *repl-test-val* 99)")
    (let ((result (run-string-repl "*repl-test-val*")))
      (assert-= 99 result))))

;;; ─── reset-repl-state ──────────────────────────────────────────────────

(deftest pipeline-reset-clears-state
  "reset-repl-state clears all persistent REPL variables."
  (with-reset-repl-state
    (run-string-repl "42")
    (assert-true (not (null cl-cc::*repl-vm-state*)))
    (reset-repl-state)
    (assert-null cl-cc::*repl-vm-state*)
    (assert-null cl-cc::*repl-pool-instructions*)
    (assert-null cl-cc::*repl-pool-labels*)))

;;; ─── compile-string-with-stdlib ─────────────────────────────────────────

(deftest pipeline-compile-with-stdlib
  "compile-string-with-stdlib includes stdlib definitions."
  (let ((result (cl-cc::compile-string-with-stdlib "(+ 1 2)" :target :vm)))
    (assert-true (typep result 'cl-cc::compilation-result))))

;;; ─── run-string with :stdlib ────────────────────────────────────────────

(deftest-each pipeline-run-string-stdlib-forms
  "run-string with :stdlib enables stdlib functions."
  :cases ((mapcar-inc  '(2 3 4)
           "(mapcar (lambda (x) (+ x 1)) '(1 2 3))")
          (reduce-sum  10
           "(reduce (lambda (a b) (+ a b)) '(1 2 3 4) 0 t)"))
  (assert-equal expected (run-string expr :stdlib t)))
