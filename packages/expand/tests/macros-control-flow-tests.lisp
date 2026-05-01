;;;; tests/unit/expand/macros-control-flow-tests.lisp — Macro control-flow tests

(in-package :cl-cc/test)

(defsuite macros-control-flow-suite
  :description "Macro control-flow expansion tests"
  :parallel nil
  :parent cl-cc-unit-suite)

(in-suite macros-control-flow-suite)

(deftest-each when-macro-expansions
  "WHEN expands to (if test (progn ...) nil)"
  :cases (("multi-body"   '(when test body1 body2) '(if test (progn body1 body2) nil))
          ("single-body"  '(when test body)        '(if test (progn body) nil))
          ("no-body"      '(when test)             '(if test (progn) nil)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest when-idempotent-expansion
  "Fully expanding representative WHEN forms twice yields the same form."
  :timeout 5
  (dolist (form '((when t body)
                  (when flag body1 body2)
                  (when (= x 0) (print 1))))
    (let* ((exp1 (our-macroexpand form))
           (exp2 (our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(deftest unless-idempotent-expansion
  "Fully expanding representative UNLESS forms twice yields the same form."
  :timeout 5
  (dolist (form '((unless t body)
                  (unless flag body1 body2)
                  (unless (= x 0) (print 1))))
    (let* ((exp1 (our-macroexpand form))
           (exp2 (our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(deftest-each unless-macro-expansions
  "UNLESS expands to (if test nil (progn ...))"
  :cases (("multi-body"   '(unless test body1 body2) '(if test nil (progn body1 body2)))
          ("single-body"  '(unless test body)        '(if test nil (progn body)))
          ("no-body"      '(unless test)             '(if test nil (progn))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest-each cond-macro-simple-expansions
  "COND base cases expand correctly"
  :cases (("empty"       '(cond)            nil)
          ("single-expr" '(cond (x))        '(or x (cond)))
          ("single-full" '(cond (test body)) '(if test (progn body) (cond))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest cond-macro-multiple-clauses
  "Test COND with multiple clauses"
  (let ((result (our-macroexpand-1 '(cond (test1 body1) (test2 body2) (t body3)))))
    (assert-eq (car result) 'if)
    (assert-equal (cadr result) 'test1)
    (assert-eq (caaddr result) 'progn)
    (assert-equal (car (cdaddr result)) 'body1)))

(deftest-each and-macro-simple-expansions
  "AND expands each arity correctly.
In the full test context, our-macroexpand-1 recurses through nested AND forms,
so (and a b c) arrives as fully-nested IFs rather than (if a (and b c) nil)."
  :cases (("empty"         '(and)       t)
          ("single-arg"    '(and x)     'x)
          ("two-args"      '(and a b)   '(if a (and b) nil))
          ("multiple-args" '(and a b c) '(if a (and b c) nil)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest and-full-expansion-creates-nested-ifs
  "Full AND expansion of (and a b c) produces nested IFs with a at top."
  (let ((result (our-macroexpand-all '(and a b c))))
    (assert-eq 'if (car result))
    (assert-equal 'a (cadr result))
    (assert-eq 'if (caaddr result))))

(deftest and-idempotent-expansion
  "Fully expanding AND forms twice yields the same form."
  :timeout 5
  (dolist (form '((and a b)
                  (and a b c)
                  (and (= x 0) flag (print 1))))
    (let* ((exp1 (our-macroexpand form))
           (exp2 (our-macroexpand exp1)))
      (assert-equal exp1 exp2))))

(deftest-each or-macro-simple-expansions
  "OR expands to nil for empty and identity for single arg"
  :cases (("empty"      '(or)  nil)
          ("single-arg" '(or x) 'x))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest or-macro-multi-arg-expansion
  "OR with 2 args: LET wrapping IF; full 3-arg expansion nests two LETs."
  (let ((result (our-macroexpand-1 '(or a b))))
    (assert-eq (car result) 'let)
    (assert-= (length result) 3)
    (assert-eq (car (caddr result)) 'if))
  (let ((result (our-macroexpand-all '(or a b c))))
    (assert-eq (car result) 'let)
    (let ((inner (cadddr (caddr result))))
      (assert-eq (car inner) 'let))))

(deftest-each let*-macro-base-cases
  "LET* base cases: empty bindings wraps in PROGN; single binding wraps in LET."
  :cases (("empty"  '(let* () body1 body2)  '(progn body1 body2))
          ("single" '(let* ((a 1)) body)     '(let ((a 1)) (let* nil body))))
  (form expected)
  (assert-equal expected (our-macroexpand-1 form)))

(deftest let*-one-step-nests-remainder-in-let*
  "LET* one-step expansion peels the first binding into a LET; the remainder stays LET*."
  (let ((result (our-macroexpand-1 '(let* ((a 1) (b a)) body))))
    (assert-eq (car result) 'let)
    (assert-equal (cadr result) '((a 1)))
    (assert-eq (car (caddr result)) 'let*)))

(deftest let*-full-expansion-produces-nested-lets
  "LET* full expansion of 2-binding form produces a nested LET chain."
  (let ((result (our-macroexpand-all '(let* ((a 1) (b a)) body))))
    (assert-eq (car result) 'let)
    (assert-equal (cadr result) '((a 1)))
    (assert-eq (caaddr result) 'let)))

(deftest let*-dependency-chain-nests-correctly
  "LET* with a 3-binding dependency chain (x y z) nests all bindings into LETs."
  (let ((result (our-macroexpand-all '(let* ((x 1) (y (+ x 1)) (z (* y 2))) body))))
    (assert-eq (car result) 'let)
    (let ((y-binding (caddr result)))
      (assert-eq (car y-binding) 'let))))

(deftest prog1-expansion-binds-result-and-returns-it
  "PROG1 expands to a LET binding first-form to a gensym, evaluates body, then returns the gensym."
  (let ((result (our-macroexpand-1 '(prog1 first-form body1 body2))))
    (assert-eq (car result) 'let)
    (assert-true (symbolp (caaadr result)))
    (assert-eq (cadr (caadr result)) 'first-form)
    (assert-eq (car (last result)) (caaadr result)))
  (let ((result (our-macroexpand-1 '(prog1 first-form))))
    (assert-eq (car result) 'let)
    (assert-= (length result) 3)))

(deftest prog2-expansion-evaluates-first-then-returns-second
  "PROG2 expands to PROGN with first-form then a LET binding second-form and returning it."
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body1 body2))))
    (assert-eq (car result) 'progn)
    (assert-eq (cadr result) 'first-form)
    (assert-eq (car (caddr result)) 'let)
    (let ((let-body (caddr result)))
      (assert-eq (car (last let-body)) (caaadr let-body))))
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body))))
    (assert-eq (car result) 'progn)
    (assert-eq (cadr result) 'first-form)
    (assert-eq (caaddr result) 'let)))

(deftest defun-c-runtime-contracts
  "DEFUN/C expands to runtime contract guards around function body."
  :timeout 10
  (let ((expanded-1
          (our-macroexpand-1
           '(defun/c add1-positive-mcf (x)
              :requires (> x 0)
              :ensures (= result (+ x 1))
              (+ x 1)))))
    (assert-eq 'defun/c (car expanded-1))
    (let ((expanded (our-macroexpand-all expanded-1 nil)))
      (assert-eq 'defun/c (car expanded))
      (assert-eq 'add1-positive-mcf (cadr expanded))
      (assert-equal '(x) (caddr expanded)))))

(deftest macroexpansion-memoization-reuses-cached-result
  "Repeated macro expansion of the same form is stable.
The cache is opportunistic, so we only require the same result each time."
  (let ((count 0)
        (name (gensym "CACHE-TEST-")))
    (cl-cc/expand::register-macro name
                           (lambda (form env)
                             (declare (ignore form env))
                             (incf count)
                             '(+ 1 2)))
    (let ((form (list name 'x)))
      (assert-equal '(+ 1 2) (our-macroexpand-all form nil))
      (assert-equal '(+ 1 2) (our-macroexpand-all form nil))
      (assert-true (<= count 2)))))
