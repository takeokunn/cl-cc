;;;; tests/unit/expand/macros-control-flow-tests.lisp — Macro control-flow tests

(in-package :cl-cc/test)

(defsuite macros-control-flow-suite
  :description "Macro control-flow expansion tests"
  :parent cl-cc-suite)

(in-suite macros-control-flow-suite)

(deftest-each when-macro-expansions
  "WHEN expands to (if test (progn ...) nil)"
  :cases (("multi-body"   '(when test body1 body2) '(if test (progn body1 body2) nil))
          ("single-body"  '(when test body)        '(if test (progn body) nil))
          ("no-body"      '(when test)             '(if test (progn) nil)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

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

(deftest and-macro-full-expansion
  "Test full expansion of AND creates nested IFs"
  (let ((result (our-macroexpand-all '(and a b c))))
    (assert-eq (car result) 'if)
    (assert-equal (cadr result) 'a)
    (assert-eq (caaddr result) 'if)))

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

(deftest let*-macro-multiple-bindings
  "Test LET* with multiple bindings (creates nested LETs).
One-step expansion of (let* ((a 1) (b a)) body) yields
(let ((a 1)) (let* ((b a)) body)) — the inner LET* stays unexpanded."
  (let ((result (our-macroexpand-1 '(let* ((a 1) (b a)) body))))
    (assert-eq (car result) 'let)
    (assert-equal (cadr result) '((a 1)))
    (assert-eq (car (caddr result)) 'let*)))

(deftest let*-macro-full-expansion
  "Test full expansion of LET* creates nested LETs"
  (let ((result (our-macroexpand-all '(let* ((a 1) (b a)) body))))
    (assert-eq (car result) 'let)
    (assert-equal (cadr result) '((a 1)))
    (assert-eq (caaddr result) 'let)))

(deftest let*-macro-dependency-chain
  "Test LET* where later bindings depend on earlier ones"
  (let ((result (our-macroexpand-all '(let* ((x 1) (y (+ x 1)) (z (* y 2))) body))))
    (assert-eq (car result) 'let)
    (let ((y-binding (caddr result)))
      (assert-eq (car y-binding) 'let))))

(deftest prog1-macro-expansion
  "PROG1 binds the first-form result via a gensym and returns it after executing body."
  (let ((result (our-macroexpand-1 '(prog1 first-form body1 body2))))
    (assert-eq (car result) 'let)
    (assert-true (symbolp (caaadr result)))
    (assert-eq (cadr (caadr result)) 'first-form)
    (assert-eq (car (last result)) (caaadr result)))
  (let ((result (our-macroexpand-1 '(prog1 first-form))))
    (assert-eq (car result) 'let)
    (assert-= (length result) 3)))

(deftest prog2-macro-expansion
  "PROG2 wraps in progn: evaluates first-form, then returns second-form after body."
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

(deftest macroexpansion-memoization-reuses-cached-result
  "Repeated macro expansion of the same form should hit the memoization cache."
  (let ((count 0)
        (name (gensym "CACHE-TEST-")))
    (cl-cc::register-macro name
                           (lambda (form env)
                             (declare (ignore form env))
                             (incf count)
                             '(+ 1 2)))
    (let ((form (list name 'x)))
      (assert-equal '(+ 1 2) (our-macroexpand-all form nil))
      (assert-equal '(+ 1 2) (our-macroexpand-all form nil))
      (assert-= count 1))))

(deftest-each dolist-expansion-is-block
  "DOLIST always expands to a (block ...) containing tagbody, regardless of arity."
  :cases (("basic"          '(dolist (item list) body))
          ("with-result"    '(dolist (item list result) body))
          ("multi-body"     '(dolist (item list) body1 body2 body3)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest-each dotimes-expansion-is-block
  "DOTIMES always expands to a (block ...) containing tagbody, regardless of arity."
  :cases (("basic"        '(dotimes (i 10) body))
          ("with-result"  '(dotimes (i 10 'done) body))
          ("zero-count"   '(dotimes (i 0) body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest-each do-expansion-is-block
  "DO always expands to a (block ...) containing let+tagbody, in all forms."
  :cases (("basic"        '(do ((i 0 (1+ i))) ((>= i 10) result) body))
          ("multi-vars"   '(do ((i 0 (1+ i)) (j 10 (1- j))) ((= i j) i) body))
          ("no-step"      '(do ((x init)) (test result) body))
          ("multi-body"   '(do ((i 0)) (test) body1 body2 body3)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest-each do*-expansion-uses-let*
  "DO* uses LET* (sequential binding) and expands to (block ...) with tagbody."
  :cases (("basic"       '(do* ((i 0 (1+ i)) (j i (1+ j))) ((>= i 10) j) body))
          ("dep-binding" '(do* ((x 1) (y (+ x 1))) (t y) body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "let*" (string-downcase (format nil "~S" result))))))

(deftest-each case-expansion-is-let
  "CASE expands to (let ...) with eql dispatch; body forms wrapped in progn."
  :cases (("basic"         '(case key (a body-a) (b body-b)))
          ("list-of-keys"  '(case key ((a b c) body-abc) (d body-d)))
          ("multi-body"    '(case key (a body1 body2 body3))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)))

(deftest-each case-default-clause
  "CASE with otherwise/t clauses both produce a let including the default body"
  :cases (("otherwise" '(case key (a body-a) (otherwise default-body)))
          ("t-clause"  '(case key (a body-a) (t default-body))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)
    (assert-true (search "default-body" (string-downcase (format nil "~S" result))))))

(deftest case-expands-sparse-integer-keys-into-binary-search
  "CASE with sparse integer keys lowers to a binary-search-style decision tree."
  (let ((result (our-macroexpand-1
                 '(case key
                    (1 body-1)
                    (8 body-8)
                    (16 body-16)
                    (32 body-32)
                    (otherwise default-body)))))
    (let ((printed (string-downcase (format nil "~S" result))))
      (assert-true (search "integerp" printed))
      (assert-true (search "<" printed))
      (assert-true (search "default-body" printed)))))

(deftest case-expands-dense-integer-keys-into-table-dispatch
  "CASE with dense integer keys lowers to a table dispatch."
  (let ((result (our-macroexpand-1
                 '(case key
                    (1 body-1)
                    (2 body-2)
                    (3 body-3)
                    (4 body-4)
                    (otherwise default-body)))))
    (let ((printed (string-downcase (format nil "~S" result))))
      (assert-true (search "svref" printed))
      (assert-true (search "vector" printed))
      (assert-true (search "default-body" printed)))))

(deftest-each typecase-expansion-is-let
  "TYPECASE expands to (let ...) with typep dispatch; body forms wrapped in progn."
  :cases (("basic"      '(typecase val (string body-string) (integer body-int)))
          ("multi-body" '(typecase val (string body1 body2))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)))

(deftest-each typecase-default-clause
  "TYPECASE with otherwise/t clauses both produce a let including the default body"
  :cases (("otherwise" '(typecase val (string body-string) (otherwise default-body)))
          ("t-clause"  '(typecase val (string body-string) (t default-body))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)
    (assert-true (search "default-body" (string-downcase (format nil "~S" result))))))

(deftest typecase-prunes-subsumed-later-clause
  "TYPECASE drops later clauses already covered by an earlier supertype."
  (let* ((result (our-macroexpand-1 '(typecase v
                                      (number body-number)
                                      (fixnum body-fixnum)
                                      (otherwise default-body))))
         (printed (string-downcase (format nil "~S" result))))
    (assert-true (search "body-number" printed))
    (assert-false (search "body-fixnum" printed))
    (assert-true (search "default-body" printed))))
