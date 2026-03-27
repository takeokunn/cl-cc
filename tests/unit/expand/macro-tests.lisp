;;;; tests/macro-tests.lisp - Comprehensive macro expansion tests for CL-CC
;;;
;;;; This file tests all built-in macros in the CL-CC macro system:
;;;; including: when, unless, cond, and, or, let*, defun, prog1, prog2,
;;;; setf, psetq, multiple-value-bind, multiple-value-setq
;;;;
(in-package :cl-cc/test)

(defsuite macro-suite
  :description "Test suite for macro expansion"
  :parent cl-cc-suite)

;;; Conditional Macro Tests

(deftest-each when-macro-expansions
  "WHEN expands to (if test (progn ...) nil)"
  :cases (("multi-body"   '(when test body1 body2) '(if test (progn body1 body2) nil))
          ("single-body"  '(when test body)        '(if test (progn body) nil))
          ("no-body"      '(when test)              '(if test (progn) nil)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest-each unless-macro-expansions
  "UNLESS expands to (if test nil (progn ...))"
  :cases (("multi-body"   '(unless test body1 body2) '(if test nil (progn body1 body2)))
          ("single-body"  '(unless test body)        '(if test nil (progn body)))
          ("no-body"      '(unless test)              '(if test nil (progn))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest-each cond-macro-simple-expansions
  "COND base cases expand correctly"
  :cases (("empty"       '(cond)           nil)
          ("single-expr" '(cond (x))       '(or x (cond)))
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

;;; Boolean Macro Tests

(deftest-each and-macro-simple-expansions
  "AND expands each arity correctly"
  :cases (("empty"         '(and)       t)
          ("single-arg"    '(and x)     'x)
          ("two-args"      '(and a b)   '(if a (and b) nil))
          ("multiple-args" '(and a b c) '(if a (and b c) nil)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest and-macro-full-expansion
  "Test full expansion of AND creates nested IFs"
  (let ((result (our-macroexpand-all '(and a b c))))
    ;; Full expansion: (if a (if b c nil) nil)
    (assert-eq (car result) 'if)
    (assert-equal (cadr result) 'a)
    ;; Then-branch should be another IF
    (assert-eq (caaddr result) 'if)))

(deftest-each or-macro-simple-expansions
  "OR expands to nil for empty and identity for single arg"
  :cases (("empty"      '(or)  nil)
          ("single-arg" '(or x) 'x))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest or-macro-multi-arg-expansion
  "OR with 2 args: LET wrapping IF; full 3-arg expansion nests two LETs."
  ;; two args: (let ((tmp a)) (if tmp tmp b))
  (let ((result (our-macroexpand-1 '(or a b))))
    (assert-eq (car result) 'let)
    (assert-= (length result) 3)
    (assert-eq (car (caddr result)) 'if))
  ;; three args full: else-branch of if is another let
  (let ((result (our-macroexpand-all '(or a b c))))
    (assert-eq (car result) 'let)
    (let ((inner (cadddr (caddr result))))
      (assert-eq (car inner) 'let))))

;;; Sequential Binding Tests

(deftest let*-macro-base-cases
  "LET* base cases: empty bindings wraps in PROGN; single binding wraps in LET."
  (assert-equal (our-macroexpand-1 '(let* () body1 body2))
                '(progn body1 body2))
  (assert-equal (our-macroexpand-1 '(let* ((a 1)) body))
                '(let ((a 1)) (let* nil body))))

(deftest let*-macro-multiple-bindings
  "Test LET* with multiple bindings (creates nested LETs)"
  (let ((result (our-macroexpand-1 '(let* ((a 1) (b a)) body))))
    (assert-eq (car result) 'let)
    (assert-equal (cadr result) '((a 1)))
    ;; Inner should be another LET*
    (assert-eq (car (caddr result)) 'let*)))

(deftest let*-macro-full-expansion
  "Test full expansion of LET* creates nested LETs"
  (let ((result (our-macroexpand-all '(let* ((a 1) (b a)) body))))
    ;; Should be (let ((a 1)) (let ((b a)) (progn body)))
    (assert-eq (car result) 'let)
    (assert-equal (cadr result) '((a 1)))
    ;; Inner body should be another let (from let* expansion)
    (assert-eq (caaddr result) 'let)))

(deftest let*-macro-dependency-chain
  "Test LET* where later bindings depend on earlier ones"
  (let ((result (our-macroexpand-all '(let* ((x 1) (y (+ x 1)) (z (* y 2))) body))))
    ;; Should create nested: (let ((x 1)) (let ((y ...)) (let ((z ...)) (progn body))))
    (assert-eq (car result) 'let)
    ;; y should reference x, z should reference y
    (let ((y-binding (caddr result)))
      (assert-eq (car y-binding) 'let))))

;;; Definition Macro Tests

(deftest-each defun-macro-structure
  "DEFUN expands to (setf (fdefinition ...) (lambda ...)) regardless of docstring"
  :cases (("basic"          '(defun foo (x y) body1 body2))
          ("with-docstring" '(defun foo (x y) "Docstring" body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'setf)
    (assert-equal (cadr result) '(fdefinition 'foo))
    (assert-eq (caaddr result) 'lambda)))

;;; Sequencing Macro Tests

(deftest prog1-macro-expansion
  "PROG1 binds the first-form result via a gensym and returns it after executing body."
  (let ((result (our-macroexpand-1 '(prog1 first-form body1 body2))))
    (assert-eq (car result) 'let)
    (assert-true (symbolp (caaadr result)))
    (assert-eq (cadr (caadr result)) 'first-form)
    (assert-eq (car (last result)) (caaadr result)))
  ;; With no body: (let ((#:R first-form)) #:R) — exactly 3 elements
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
  ;; With single body form, structure is the same
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body))))
    (assert-eq (car result) 'progn)
    (assert-eq (cadr result) 'first-form)
    (assert-eq (caaddr result) 'let)))

;;; Assignment Macro Tests

(deftest setf-macro-simple-symbol
  "Test SETF with simple symbol"
  (assert-equal (our-macroexpand-1 '(setf x 10))
                '(setq x 10)))

(deftest-each setf-macro-unknown-place-errors
  "SETF signals an error for place forms with unknown accessor names."
  :cases (("car-place"  '(setf (car x)  10))
          ("cons-place" '(setf (cons x) 10)))
  (form)
  (assert-signals error (our-macroexpand-1 form)))

(deftest psetq-macro-behavior
  "PSETQ: empty expands to nil; 1-pair and N-pair forms produce outer LET with bindings, body starting with SETQ."
  (assert-equal (our-macroexpand-1 '(psetq))
                nil)
  (let ((result (our-macroexpand-1 '(psetq a 1))))
    (assert-eq 'let (car result))
    (assert-true (consp (cadr result)))
    (assert-eq 'setq (car (caddr result))))
  (let ((result (our-macroexpand-1 '(psetq a 1 b 2 c 3))))
    (assert-eq 'let (car result))
    (assert-true (consp (cadr result)))))

(deftest psetq-macro-full-expansion
   "Test full expansion of PSETQ"
   (let ((result (our-macroexpand '(psetq a 1 b 2))))
     ;; Expansion: (let ((#:A 1) (#:B 2)) (setq a #:A) (setq b #:B) nil)
     (assert-eq (car result) 'let)
     (assert-true (consp (cadr result)))
     ;; Body forms are SETQ (not wrapped in PROGN)
     (assert-eq (car (caddr result)) 'setq)))

;;; Multiple Value Macro Tests

(deftest multiple-value-bind-macro-expansion
  "MULTIPLE-VALUE-BIND: expands to multiple-value-call+lambda for all input shapes."
  ;; Multi-var with body
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b c) (values 1 2 3) body1 body2))))
    (assert-eq 'multiple-value-call (car result))
    (assert-eq 'lambda (caadr result))
    (assert-equal '(a b c) (cadr (cadr result)))
    (assert-eq 'values (caaddr result)))
  ;; Single var
  (let ((result (our-macroexpand-1 '(multiple-value-bind (x) (foo) body))))
    (assert-eq 'multiple-value-call (car result))
    (assert-eq 'lambda (caadr result))
    (assert-equal '(x) (cadr (cadr result))))
  ;; No body: lambda body is empty
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b) (values 1 2)))))
    (assert-eq 'multiple-value-call (car result))
    (assert-null (cddr (cadr result)))))

(deftest multiple-value-setq-macro-expansion
  "MULTIPLE-VALUE-SETQ: outer LET binding from multiple-value-list, for all input shapes."
  ;; Multi-var
  (let* ((result   (our-macroexpand-1 '(multiple-value-setq (a b c) (values 1 2 3))))
         (bindings (cadr result)))
    (assert-eq 'let (car result))
    (assert-true (consp bindings))
    (assert-eq 'multiple-value-list (caadar bindings)))
  ;; Extra body is ignored: (let (...) setq setq setq car) — 5 elements
  (let ((result (our-macroexpand-1 '(multiple-value-setq (a b) (values 1 2) extra-body))))
    (assert-eq 'let (car result))
    (assert-= 5 (length result)))
  ;; Single var
  (let* ((result   (our-macroexpand-1 '(multiple-value-setq (x) (foo))))
         (bindings (cadr result)))
    (assert-eq 'let (car result))
    (assert-eq 'multiple-value-list (caadar bindings))))

;;; Nested Macro Tests

(deftest nested-when-in-let*
  "Test WHEN nested inside LET*"
  (let ((result (our-macroexpand-all '(let* ((x (when test body))) x))))
    ;; Full expansion: (let ((x (if test (progn body) nil))) (progn x))
    (assert-eq (car result) 'let)
    ;; Binding value should be IF (from when expansion)
    (let ((binding-value (cadr (car (cadr result)))))
      (assert-eq (car binding-value) 'if))))

(deftest nested-cond-in-and
  "Test COND nested inside AND"
  (let ((result (our-macroexpand-all '(and test1 (cond ((test2 result)))))))
    ;; (cond ((test2 result))) = single-expr clause (no body) → (or (test2 result) (cond))
    ;; Full expansion: (if test1 (let ((#:OR (test2 result))) (if #:OR #:OR nil)) nil)
    (assert-eq (car result) 'if)
    ;; Then-branch is a LET form (from or expansion inside cond)
    (let ((and-part (caddr result)))
      (assert-eq (car and-part) 'let))))

(deftest nested-prog1-in-prog2
  "Test PROG1 nested inside PROG2"
  (let ((result (our-macroexpand-all '(prog2 (prog1 a b) c body))))
    ;; Full expansion: (progn (let ((#:R a)) b #:R) (let ((#:R2 c)) body #:R2))
    (assert-eq (car result) 'progn)
    ;; The prog1 first-form should be expanded to a LET
    (let ((first-form (cadr result)))
      (assert-eq (car first-form) 'let))))

(deftest nested-multiple-value-bind
  "Test nested MULTIPLE-VALUE-BIND"
  (let ((result (our-macroexpand-all '(multiple-value-bind (a b) (multiple-value-bind (x y) (values 1 2 3 4) (list x y)) (list a b)))))
    ;; Full expansion: both MVB forms become multiple-value-call
    (assert-eq (car result) 'multiple-value-call)
    ;; Inner form (3rd element) should also be expanded to multiple-value-call
    (let ((inner-form (caddr result)))
      (assert-eq (car inner-form) 'multiple-value-call))))

;;; Error Case Tests
;;; All macros must signal error on malformed input — expressed as one table.

(deftest-each error-macro-malformed-args
  "Macros signal errors on malformed arguments (bad clauses, variable lists, binding lists)."
  :cases (("cond-non-list-clause"   '(cond x))
          ("cond-empty-clause"      '(cond ()))
          ("psetq-non-pair"         '(psetq x))
          ("psetq-odd-args"         '(psetq a 1 b))
          ("mvbind-non-list-vars"   '(multiple-value-bind a b (values 1 2) body))
          ("mvbind-empty-vars"      '(multiple-value-bind () (values 1 2) body))
          ("mvsetq-non-list-vars"   '(multiple-value-setq a b (values 1 2)))
          ("mvsetq-empty-vars"      '(multiple-value-setq () (values 1 2)))
          ("let*-non-list-binding"  '(let* (x 1) body))
          ("let*-invalid-binding"   '(let* ((a)) body)))
  (form)
  (assert-signals error (our-macroexpand-1 form)))

;;; Integration Tests

(deftest-each integration-when-unless-evaluation
  "Integration: WHEN/UNLESS expand to the expected IF form."
  :cases (("when"   '(when   t   1 2 3) '(if t   (progn 1 2 3) nil))
          ("unless" '(unless nil 1 2 3) '(if nil nil (progn 1 2 3))))
  (form expected)
  (assert-equal (our-macroexpand form) expected))

(deftest integration-cond-evaluation
  "Integration test: COND produces correct nested IF structure"
  (let ((expanded (our-macroexpand-all '(cond ((= x 1) 'one)
                                              ((= x 2) 'two)
                                              (t 'other)))))
    ;; Full expansion: (if (= x 1) (progn 'one) (if (= x 2) (progn 'two) 'other))
    (assert-eq (car expanded) 'if)
    ;; Else-branch (4th element) should be another IF
    (assert-eq (car (cadddr expanded)) 'if)))

;;; MULTIPLE-VALUE-LIST Macro Tests (Wave 2)

(deftest multiple-value-list-expansion
  "MULTIPLE-VALUE-LIST: outer LET, body is multiple-value-call; fully expands away."
  ;; Any form: outer LET + multiple-value-call in body
  (let ((result (our-macroexpand-1 '(multiple-value-list (values 1 2 3)))))
    (assert-eq 'let (car result))
    (assert-eq 'multiple-value-call (car (caddr result))))
  (let ((result (our-macroexpand-1 '(multiple-value-list (foo)))))
    (assert-eq 'let (car result))
    (assert-eq 'multiple-value-call (caaddr result)))
  ;; Full expansion removes the macro entirely
  (let ((result (our-macroexpand '(multiple-value-list (values 1 2 3)))))
    (assert-false (search "multiple-value-list" (string-downcase (format nil "~S" result))))))

;;; Control Flow Macro Tests - dolist, dotimes, do, do*, case, typecase, loop

;;; DOLIST Tests

(deftest-each dolist-expansion-is-block
  "DOLIST always expands to a (block ...) containing tagbody, regardless of arity."
  :cases (("basic"          '(dolist (item list) body))
          ("with-result"    '(dolist (item list result) body))
          ("multi-body"     '(dolist (item list) body1 body2 body3)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

;;; DOTIMES Tests

(deftest-each dotimes-expansion-is-block
  "DOTIMES always expands to a (block ...) containing tagbody, regardless of arity."
  :cases (("basic"        '(dotimes (i 10) body))
          ("with-result"  '(dotimes (i 10 'done) body))
          ("zero-count"   '(dotimes (i 0) body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

;;; DO Tests

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

;;; DO* Tests

(deftest-each do*-expansion-uses-let*
  "DO* uses LET* (sequential binding) and expands to (block ...) with tagbody."
  :cases (("basic"       '(do* ((i 0 (1+ i)) (j i (1+ j))) ((>= i 10) j) body))
          ("dep-binding" '(do* ((x 1) (y (+ x 1))) (t y) body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "let*" (string-downcase (format nil "~S" result))))))

;;; CASE Tests

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


;;; TYPECASE Tests

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


;;; LOOP Tests (Simplified)

(deftest-each loop-forms-expand-to-block
  "LOOP iteration forms expand to (block ...) containing tagbody"
  :cases (("for-from-to"  '(loop for i from 1 to 10 do (print i)))
          ("for-in-list"  '(loop for item in list do (print item)))
          ("for-on-list"  '(loop for tail on list do (print tail))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'block (car result))
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest-each loop-accumulation-is-block
  "LOOP with accumulation keywords (collect, sum, count) expands to (block ...)."
  :cases (("collect" '(loop for i from 1 to 5 collect (* i 2)))
          ("sum"     '(loop for i from 1 to 10 sum i))
          ("count"   '(loop for i from 1 to 10 count (evenp i))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)))

(deftest-each loop-conditional-forms
  "LOOP with while/until/below all expand to block+tagbody"
  :cases (("while"    '(loop for i from 1 to 10 while (< i 5) do (print i)))
          ("until"    '(loop for i from 1 to 10 until (>= i 5) do (print i)))
          ("below"    '(loop for i from 0 below 5 do (print i))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'block (car result))
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))


;;; Integration Tests for Control Flow Macros

(deftest-each integration-loop-macros-expand-to-block
  "dolist/dotimes/do all expand to a block at the top level"
  :cases (("dolist"  '(dolist (x '(1 2 3)) (print x)))
          ("dotimes" '(dotimes (i 3) (print i)))
          ("do"      '(do ((i 0 (1+ i))) ((= i 5)) (print i))))
  (form)
  (let ((expanded (our-macroexpand form)))
    (assert-eq 'block (car expanded))))

(deftest integration-case-evaluation
  "Integration test: CASE produces correct structure"
  (let ((expanded (our-macroexpand '(case x (1 'one) (2 'two) (t 'other)))))
    (assert-eq (car expanded) 'let)
    ;; Should have nested if/eql checks
    (assert-true (search "if" (string-downcase (format nil "~S" expanded))))))

(deftest integration-typecase-evaluation
  "Integration test: TYPECASE produces correct structure"
  (let ((expanded (our-macroexpand '(typecase x (string "string") (integer "int") (t "other")))))
    (assert-eq (car expanded) 'let)
    (assert-true (search "typep" (string-downcase (format nil "~S" expanded))))))

;;; ─── %expand-quasiquote ──────────────────────────────────────────────────

(deftest-each expand-quasiquote-wraps-in-quote
  "%expand-quasiquote wraps self-evaluating atoms and symbols in (quote ...)."
  :cases (("atom"   'foo 'foo)
          ("number" 42   42))
  (input expected-val)
  (assert-equal (list 'quote expected-val) (cl-cc::%expand-quasiquote input)))

(deftest expand-quasiquote-unquote-extracts
  "Top-level unquote returns its argument directly."
  (assert-equal 'x (cl-cc::%expand-quasiquote '(cl-cc::unquote x))))

(deftest expand-quasiquote-list-wraps-in-list
  "Plain list elements are wrapped in (list ...) and appended."
  (let ((result (cl-cc::%expand-quasiquote '(a b))))
    ;; Result should be (append (list ...) (list ...))
    (assert-eq 'append (car result))))

(deftest expand-quasiquote-unquote-in-list
  "Unquote inside list is spliced as a (list val) part."
  ;; Use explicit quote to avoid CL's own unquote processing.
  (let* ((result (cl-cc::%expand-quasiquote '(a (cl-cc::unquote x))))
         (str (format nil "~S" result)))
    (assert-true (search "X" str))))

;;; ─── generate-lambda-bindings ────────────────────────────────────────────

(deftest generate-lambda-bindings-shapes
  "generate-lambda-bindings: empty→nil; required pair (a b)→4 bindings; &rest args→has 'args entry."
  ;; empty lambda list → no bindings
  (assert-equal nil (cl-cc::generate-lambda-bindings '() 'form))
  ;; each required param produces 2 bindings (temp + param): 2 params = 4
  (assert-= 4 (length (cl-cc::generate-lambda-bindings '(a b) 'form)))
  ;; &rest arg appears in the binding alist
  (assert-true (assoc 'args (cl-cc::generate-lambda-bindings '(&rest args) 'form))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; parse-lambda-list
;;; ─────────────────────────────────────────────────────────────────────────

(deftest parse-lambda-list-empty
  "parse-lambda-list of () returns a lambda-list-info with all nil fields."
  (let ((info (cl-cc::parse-lambda-list '())))
    (assert-equal nil (cl-cc::lambda-list-info-required info))
    (assert-equal nil (cl-cc::lambda-list-info-optional info))
    (assert-equal nil (cl-cc::lambda-list-info-rest info))))

(deftest parse-lambda-list-required-only
  "parse-lambda-list (a b c) fills the :required slot."
  (let ((info (cl-cc::parse-lambda-list '(a b c))))
    (assert-equal '(a b c) (cl-cc::lambda-list-info-required info))
    (assert-equal nil (cl-cc::lambda-list-info-rest info))))

(deftest parse-lambda-list-optional-symbol
  "parse-lambda-list &optional bare symbol becomes (name nil nil)."
  (let ((info (cl-cc::parse-lambda-list '(x &optional y))))
    (assert-equal '(x) (cl-cc::lambda-list-info-required info))
    (let ((opt (first (cl-cc::lambda-list-info-optional info))))
      (assert-eq 'y (first opt))
      (assert-eq nil (second opt)))))

(deftest parse-lambda-list-optional-with-default
  "parse-lambda-list &optional (y 42) captures default."
  (let ((info (cl-cc::parse-lambda-list '(x &optional (y 42)))))
    (let ((opt (first (cl-cc::lambda-list-info-optional info))))
      (assert-eq 'y (first opt))
      (assert-equal 42 (second opt)))))

(deftest-each parse-lambda-list-rest-and-body
  "&rest fills the :rest slot; &body fills the :body slot (and leaves :rest nil)."
  :cases (("rest" '(x &rest r)      #'cl-cc::lambda-list-info-rest  'r    :skip)
          ("body" '(form &body body) #'cl-cc::lambda-list-info-body  'body  nil))
  (ll get-slot expected-sym expected-rest)
  (let ((info (cl-cc::parse-lambda-list ll)))
    (assert-eq expected-sym (funcall get-slot info))
    (unless (eq expected-rest :skip)
      (assert-eq expected-rest (cl-cc::lambda-list-info-rest info)))))

(deftest parse-lambda-list-key
  "parse-lambda-list &key k generates (((:k k) nil nil)) in key-params."
  (let ((info (cl-cc::parse-lambda-list '(x &key k))))
    (let ((kp (first (cl-cc::lambda-list-info-key-params info))))
      ;; name-spec = (:K K)
      (assert-eq :k (first (first kp)))
      (assert-eq 'k (second (first kp))))))

(deftest parse-lambda-list-allow-other-keys
  "parse-lambda-list &allow-other-keys sets the flag."
  (let ((info (cl-cc::parse-lambda-list '(&key x &allow-other-keys))))
    (assert-true (cl-cc::lambda-list-info-allow-other-keys info))))

(deftest parse-lambda-list-aux
  "parse-lambda-list &aux (x 0) records aux binding."
  (let ((info (cl-cc::parse-lambda-list '(a &aux (x 0)))))
    (let ((aux (first (cl-cc::lambda-list-info-aux info))))
      (assert-eq 'x (first aux))
      (assert-equal 0 (second aux)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; destructure-lambda-list
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each destructure-lambda-list
  "destructure-lambda-list produces bindings for all named params."
  :cases (("required"  '(x y)           '(x y))
          ("rest"      '(x &rest r)      '(x r))
          ("optional"  '(x &optional y)  '(x y)))
  (lambda-list expected-vars)
  (let ((bindings (cl-cc::destructure-lambda-list lambda-list 'form)))
    (dolist (var expected-vars)
      (assert-true (assoc var bindings)))))
