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

(deftest pipeline-compile-toplevel-forms-captures-type-env
  "compile-toplevel-forms retains the inferred top-level type environment."
  (let ((result (cl-cc/compile::compile-toplevel-forms '((defvar *typed-top-level* 42))
                                                :type-check t)))
    (assert-true (typep (cl-cc/compile::compilation-result-type-env result)
                        'cl-cc/type:type-env))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup '*typed-top-level*
                                     (cl-cc/compile::compilation-result-type-env result))
      (assert-true found-p)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                          (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-compile-toplevel-forms-captures-cps
  "compile-toplevel-forms stores a CPS form for top-level input."
  (let ((result (cl-cc/compile::compile-toplevel-forms '((+ 1 2) (- 4 1)))))
    (assert-true (consp (cl-cc/compile::compilation-result-cps result)))))


(deftest pipeline-compile-toplevel-forms-captures-defun-type-env
  "compile-toplevel-forms records inferred defun types for later forms."
  (let ((result (cl-cc/compile::compile-toplevel-forms
                 '((defun typed-id (x) x)
                   (typed-id 42))
                 :type-check t)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup 'typed-id
                                     (cl-cc/compile::compilation-result-type-env result))
      (assert-true found-p)
      (assert-true (cl-cc/type:type-function-p
                    (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-compile-toplevel-forms-records-defun-type-without-type-check
  "compile-toplevel-forms still records inferred defun types when type-check is off."
  (let ((result (cl-cc/compile::compile-toplevel-forms
                 '((defun typed-id-no-check (x) x)
                   (typed-id-no-check 42)))))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup 'typed-id-no-check
                                     (cl-cc/compile::compilation-result-type-env result))
      (assert-true found-p)
      (assert-true (cl-cc/type:type-function-p
                    (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-compile-toplevel-forms-records-defvar-type-without-type-check
  "compile-toplevel-forms still records inferred defvar types when type-check is off."
  (let ((result (cl-cc/compile::compile-toplevel-forms
                  '((defvar *typed-top-level-no-check* 42)
                    *typed-top-level-no-check*))))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup '*typed-top-level-no-check*
                                     (cl-cc/compile::compilation-result-type-env result))
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
  "Typed fixnum functions compile and keep type assertions out of the hot path when possible."
  (let* ((result (compile-string
                  "(defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y))"))
         (prog (compilation-result-program result))
         (instrs (vm-program-instructions prog)))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-add)) instrs))
    (assert-true (< 0 (length instrs)))))

(deftest pipeline-typed-fixnum-compare-fast-path
  "Typed fixnum comparisons compile to the specialized compare VM instruction."
  (let* ((result (compile-string
                  "(defun typed-lt ((x fixnum) (y fixnum)) fixnum (< x y))"
                  :target :vm))
         (instrs (vm-program-instructions (compilation-result-program result))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-lt)) instrs))))

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

(deftest pipeline-run-string-pass-timings
  "run-string forwards timing output options while preserving evaluation result."
  (let ((stream (make-string-output-stream)))
    (assert-= 3 (run-string "(+ 1 2)" :pass-pipeline "fold" :print-pass-timings t :timing-stream stream))
    (assert-true (search "OPT-PASS-FOLD" (string-upcase (get-output-stream-string stream))))))

(deftest pipeline-run-string-pass-stats
  "run-string forwards pass stats output options while preserving evaluation result."
  (let ((stream (make-string-output-stream)))
    (assert-= 3 (run-string "(+ 1 2)" :pass-pipeline "fold" :print-pass-stats t :stats-stream stream))
    (let ((text (string-upcase (get-output-stream-string stream))))
      (assert-true (search "OPT-PASS-FOLD" text))
      (assert-true (search "BEFORE=" text)))))

(deftest pipeline-run-string-trace-json
  "run-string forwards trace-json output while preserving evaluation result."
  (let ((stream (make-string-output-stream)))
    (assert-= 3 (run-string "(+ 1 2)" :pass-pipeline "fold" :trace-json-stream stream))
    (let ((text (get-output-stream-string stream)))
      (assert-true (search "\"traceEvents\"" text))
      (assert-true (search "OPT-PASS-FOLD" text)))))

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

(deftest pipeline-run-form-repl-registers-top-level-defmacro-on-host
  "run-form-repl handles top-level defmacro by registering a host expander immediately." 
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-DEFMACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(defmacro pipeline-repl-temp-defmacro (&body body) `(progn ,@body))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (assert-eq macro-name (cl-cc::run-form-repl form))
           (let ((expander (gethash macro-name table)))
              (assert-true expander)
              (assert-equal '(progn (print 1))
                           (funcall expander '(pipeline-repl-temp-defmacro (print 1)) nil))))
      (remhash macro-name table))))

(deftest pipeline-run-form-repl-registers-destructuring-defmacro-on-host
  "run-form-repl supports top-level defmacro lambda lists with nested destructuring." 
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-DESTRUCTURING-DEFMACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(defmacro pipeline-repl-temp-destructuring-defmacro (name (parent) &body body) `(list ',name ',parent ',body))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (assert-eq macro-name (cl-cc::run-form-repl form))
           (let ((expander (gethash macro-name table)))
             (assert-true expander)
             (assert-equal '(cons 'foo
                                  (cons 'bar
                                        (cons '(baz quux) nil)))
                           (funcall expander '(pipeline-repl-temp-destructuring-defmacro foo (bar) baz quux) nil))))
      (remhash macro-name table))))

(deftest pipeline-run-form-repl-normalizes-register-macro-lambda-body
  "run-form-repl normalizes top-level register-macro lambda bodies before storing the host expander." 
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-REGISTER-MACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(register-macro 'pipeline-repl-temp-register-macro (lambda (form env) (declare (ignore env)) (let ((x (second form))) `(progn ,x))))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (assert-eq macro-name (cl-cc::run-form-repl form))
           (let ((expander (gethash macro-name table)))
             (assert-true expander)
              (assert-equal '(progn 42)
                            (funcall expander '(pipeline-repl-temp-register-macro 42) nil))))
      (remhash macro-name table))))

(deftest pipeline-run-string-uses-cps-fast-path-for-safe-single-form
  "run-string uses the CPS fast path for a safe single-form Lisp expression." 
  (let ((hook-called nil)
        (hook-source nil)
        (hook-form nil)
        (hook-value nil))
    (let ((cl-cc::*run-string-cps-fast-path-hook*
            (lambda (source form value)
              (setf hook-called t
                    hook-source source
                    hook-form form
                    hook-value value))))
      (assert-eql 3 (cl-cc:run-string "(+ 1 2)"))
      (assert-true hook-called)
      (assert-equal "(+ 1 2)" hook-source)
      (assert-equal '(+ 1 2) hook-form)
      (assert-eql 3 hook-value))))

(deftest pipeline-run-string-skips-cps-fast-path-for-definition-forms
  "run-string does not use the CPS fast path for definition-like top-level forms." 
  (let ((hook-called nil))
    (let ((cl-cc::*run-string-cps-fast-path-hook*
            (lambda (&rest args)
              (declare (ignore args))
              (setf hook-called t))))
      (assert-true (cl-cc:run-string "(defun pipeline-cps-fast-path-def () 42)"))
      (assert-false hook-called))))

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
    (assert-true (typep result 'cl-cc/compile::compilation-result))))

;;; ─── run-string with :stdlib ────────────────────────────────────────────

(deftest-each pipeline-run-string-stdlib-forms
  "run-string with :stdlib enables stdlib functions."
  :cases ((mapcar-inc  '(2 3 4)
           "(mapcar (lambda (x) (+ x 1)) '(1 2 3))")
          (reduce-sum  10
           "(reduce (lambda (a b) (+ a b)) '(1 2 3 4) 0 t)"))
  (expected expr)
  (assert-equal expected (run-string expr :stdlib t)))

;;; ─── %whitespace-symbol-p ───────────────────────────────────────────────

(deftest-each pipeline-whitespace-symbol-p
  "%whitespace-symbol-p identifies symbols whose name is all whitespace."
  :cases (("plain-symbol"      nil 'hello)
          ("nil"               nil nil)
          ("keyword"           nil :foo)
          ("number"            nil 42)
          ("empty-string"      nil ""))
  (expected form)
  (assert-equal expected (cl-cc::%whitespace-symbol-p form)))

(deftest pipeline-whitespace-symbol-p-space-sym
  "%whitespace-symbol-p returns T for a symbol named with only spaces."
  ;; Intern a whitespace-only symbol to test the predicate directly.
  (let ((ws-sym (intern " " (find-package :cl-cc))))
    (assert-true (cl-cc::%whitespace-symbol-p ws-sym))))

;;; ─── %ensure-repl-state ─────────────────────────────────────────────────

(deftest pipeline-ensure-repl-state-initializes
  "%ensure-repl-state lazily initializes all REPL state variables."
  (with-reset-repl-state
    ;; All state vars should be nil after reset
    (assert-null cl-cc::*repl-vm-state*)
    (assert-null cl-cc::*repl-pool-instructions*)
    ;; Trigger lazy init
    (cl-cc::%ensure-repl-state)
    ;; All should now be initialized
    (assert-true (not (null cl-cc::*repl-vm-state*)))
    (assert-true (not (null cl-cc::*repl-pool-instructions*)))
    (assert-true (not (null cl-cc::*repl-pool-labels*)))
    (assert-true (not (null cl-cc::*repl-global-vars-persistent*)))))

(deftest pipeline-ensure-repl-state-idempotent
  "%ensure-repl-state is idempotent — second call does not reset existing state."
  (with-reset-repl-state
    (cl-cc::%ensure-repl-state)
    (let ((first-state cl-cc::*repl-vm-state*))
      (cl-cc::%ensure-repl-state)
      (assert-eq first-state cl-cc::*repl-vm-state*))))
