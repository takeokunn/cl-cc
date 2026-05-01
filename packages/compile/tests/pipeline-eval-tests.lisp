(in-package :cl-cc/test)
(in-suite cl-cc-integration-serial-suite)

;;; ─── typed assertions / typed instructions ───────────────────────────────

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

;;; ─── compile-string / run-string ─────────────────────────────────────────

(deftest-each pipeline-compile-string-returns-result
  "compile-string returns a compilation-result for simple and multi-form inputs."
  :cases (("single-form"   "(+ 1 2)")
          ("multiple-forms" "(defun f (x) x)"))
  (expr)
  (assert-true (typep (compile-string expr :target :vm) 'cl-cc/compile::compilation-result)))

(deftest pipeline-compile-string-custom-pass-pipeline
  "compile-string forwards a string pass pipeline to optimizer core."
  (let* ((baseline (compile-string "(+ 1 2)" :target :vm))
         (result (compile-string "(+ 1 2)" :target :vm :pass-pipeline "fold,dce")))
    (assert-true (typep result 'cl-cc/compile::compilation-result))
    (assert-true (listp (cl-cc:compilation-result-optimized-instructions baseline)))
    (assert-true (listp (cl-cc:compilation-result-optimized-instructions result)))
    (assert-true (> (length (cl-cc:compilation-result-vm-instructions result)) 0))))

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

(deftest-each pipeline-run-string-list-regressions
  "run-string executes list-building and local-recursion forms correctly on the VM path."
  :cases (("list"          '(1 2 3) "(list 1 2 3)")
          ("cons"          '(1 2 3) "(cons 1 (list 2 3))")
          ("dolist-sum"    6         "(let ((acc 0)) (dolist (x (list 1 2 3) acc) (setq acc (+ acc x))))")
          ("labels-simple" 3         "(labels ((f (x) (if (= x 0) 0 (+ 1 (f (- x 1)))))) (f 3))"))
  (expected expr)
  (assert-true (equal expected (run-string expr))))

(deftest-each pipeline-run-string-stdlib-regressions
  "run-string with stdlib handles the recovered HOF and set/list paths."
  :cases (("mapcar"        '(2 4 6)   "(mapcar (lambda (x) (+ x x)) (list 1 2 3))")
          ("find-if"       4           "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
          ("count-if"      2           "(count-if (lambda (x) (> x 2)) (list 1 2 3 4))")
          ("reduce"        10          "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))")
          ("set-difference" '(1 3 5)   "(set-difference (list 1 2 3 4 5) (list 2 4))")
          ("position-miss" nil         "(position 9 (list 1 2 3))"))
  (expected expr)
  (assert-true (equal expected (run-string expr :stdlib t))))

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

;;; ─── prescan / parse / stdlib / our-eval ─────────────────────────────────

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
  "get-stdlib-forms returns a fresh nested tree on each call."
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

(deftest-each pipeline-our-eval-forms
  "our-eval evaluates arithmetic, quoted data, and conditionals."
  :cases ((arithmetic  6          '(* 2 3))
          (quote-data  '(a b c)   '(quote (a b c)))
          (if-form     10         '(if t 10 20)))
  (expected expr)
  (assert-equal expected (cl-cc::our-eval expr)))

(deftest pipeline-our-eval-uses-vm-compile-path-for-simple-expression
  "our-eval evaluates simple expressions via the VM compile path."
  (let ((orig (symbol-function 'cl-cc::compile-expression)))
    (unwind-protect
         (let ((called nil))
           (setf (symbol-function 'cl-cc::compile-expression)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (setf called t)
                (make-compilation-result
                     :program (make-vm-program :instructions (list (make-vm-const :dst :r0 :value 3)
                                                                    (make-vm-halt :reg :r0)))
                     :assembly ""
                     :cps '(identity 3)
                     :vm-instructions nil
                    :optimized-instructions nil)))
           (assert-= 3 (cl-cc::our-eval '(+ 1 2)))
           (assert-true called))
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
            (assert-eq 7
                       (cl-cc::our-eval '(defvar *pipeline-cps-fallback* 7)))
            (assert-true called))
      (ignore-errors (makunbound '*pipeline-cps-fallback*))
      (setf (symbol-function 'cl-cc::compile-expression) orig))))
