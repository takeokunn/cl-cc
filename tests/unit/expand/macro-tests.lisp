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

(deftest or-macro-two-args
  "Test OR with two arguments"
  (let ((result (our-macroexpand-1 '(or a b))))
    (assert-eq (car result) 'let)
    ;; Creates a temporary variable
    (assert-= (length result) 3)
    ;; Body should be IF expression
    (assert-eq (car (caddr result)) 'if)))

(deftest or-macro-full-expansion
  "Test full expansion of OR creates nested LETs"
  (let ((result (our-macroexpand-all '(or a b c))))
    ;; Full expansion: (let ((#:OR1 a)) (if #:OR1 #:OR1 (let ((#:OR2 b)) (if #:OR2 #:OR2 c))))
    (assert-eq (car result) 'let)
    ;; The else-branch of the if should be another let (from (or b c) expansion)
    (let ((inner (cadddr (caddr result))))
      (assert-eq (car inner) 'let))))

;;; Sequential Binding Tests

(deftest let*-macro-empty-bindings
  "Test LET* with no bindings (just wraps in PROGN)"
  (assert-equal (our-macroexpand-1 '(let* () body1 body2))
                '(progn body1 body2)))

(deftest let*-macro-single-binding
  "Test LET* with single binding"
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
  "Test PROG1 macro expansion"
  (let ((result (our-macroexpand-1 '(prog1 first-form body1 body2))))
    ;; Expansion: (let ((#:RESULT first-form)) body1 body2 #:RESULT)
    (assert-eq (car result) 'let)
    ;; Binding var should be a gensym symbol: (caaadr result) = car of binding pair
    (assert-true (symbolp (caaadr result)))
    ;; Binding value should be first-form: (cadr of binding pair)
    (assert-eq (cadr (caadr result)) 'first-form)
    ;; Last element should be the gensym
    (assert-eq (car (last result)) (caaadr result))))

(deftest prog1-macro-single-body
  "Test PROG1 with single body form"
  (let ((result (our-macroexpand-1 '(prog1 first-form))))
    ;; Expansion: (let ((#:RESULT first-form)) #:RESULT) — 3 elements
    (assert-eq (car result) 'let)
    ;; Should still work even without body
    (assert-= (length result) 3)))

(deftest prog2-macro-expansion
  "Test PROG2 macro expansion"
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body1 body2))))
    ;; Expansion: (progn first-form (let ((#:R second-form)) body1 body2 #:R))
    (assert-eq (car result) 'progn)
    ;; First element of body should be first-form
    (assert-eq (cadr result) 'first-form)
    ;; Second element should be a LET form (not the symbol 'let)
    (assert-eq (car (caddr result)) 'let)
    ;; Should return the gensym at end of let body
    (let ((let-body (caddr result)))
      (assert-eq (car (last let-body)) (caaadr let-body)))))

(deftest prog2-macro-single-body
  "Test PROG2 with single body form"
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body))))
    (assert-eq (car result) 'progn)
    (assert-eq (cadr result) 'first-form)
    (assert-eq (caaddr result) 'let)))

;;; Assignment Macro Tests

(deftest setf-macro-simple-symbol
  "Test SETF with simple symbol"
  (assert-equal (our-macroexpand-1 '(setf x 10))
                '(setq x 10)))

(deftest setf-macro-complex-place
  "Test SETF with complex place (should error)"
  (assert-signals error (our-macroexpand-1 '(setf (car x) 10))))

(deftest setf-macro-cons-place
  "Test SETF with cons place (should error)"
  (assert-signals error (our-macroexpand-1 '(setf (cons x) 10))))

(deftest psetq-macro-empty
  "Test PSETQ with no pairs"
  (assert-equal (our-macroexpand-1 '(psetq))
                nil))

(deftest psetq-macro-single-pair
   "Test PSETQ with single pair"
   (let ((result (our-macroexpand-1 '(psetq a 1))))
     (assert-eq (car result) 'let)
     ;; Should create bindings
     (let ((bindings (cadr result)))
       (assert-true (consp bindings))
       ;; Body should be setq forms
       (assert-eq (car (caddr result)) 'setq))))

(deftest psetq-macro-multiple-pairs
  "Test PSETQ with multiple pairs"
  (let ((result (our-macroexpand-1 '(psetq a 1 b 2 c 3))))
    (assert-eq (car result) 'let)
    ;; Should evaluate all values before assignment
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
  "Test MULTIPLE-VALUE-BIND macro expansion"
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b c) (values 1 2 3) body1 body2))))
    ;; Expansion: (multiple-value-call (lambda (a b c) body1 body2) (values 1 2 3))
    (assert-eq (car result) 'multiple-value-call)
    ;; Second element is a lambda FORM (not the symbol 'lambda)
    (assert-eq (caadr result) 'lambda)
    ;; Lambda should have the variables
    (assert-equal (cadr (cadr result)) '(a b c))
    ;; Third element is the values form (not the symbol 'values)
    (assert-eq (caaddr result) 'values)))

(deftest multiple-value-bind-macro-single-var
  "Test MULTIPLE-VALUE-BIND with single variable"
  (let ((result (our-macroexpand-1 '(multiple-value-bind (x) (foo) body))))
    ;; Expansion: (multiple-value-call (lambda (x) body) (foo))
    (assert-eq (car result) 'multiple-value-call)
    ;; Second element is the lambda FORM
    (assert-eq (caadr result) 'lambda)
    (assert-equal (cadr (cadr result)) '(x))))

(deftest multiple-value-bind-macro-no-body
  "Test MULTIPLE-VALUE-BIND with no body"
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b) (values 1 2)))))
    (assert-eq (car result) 'multiple-value-call)
    ;; Lambda should have empty body
    (assert-null (cddr (cadr result)))))

(deftest multiple-value-setq-macro-expansion
  "Test MULTIPLE-VALUE-SETQ macro expansion"
  (let ((result (our-macroexpand-1 '(multiple-value-setq (a b c) (values 1 2 3)))))
    ;; Expansion: (let ((#:MVS (multiple-value-list ...))) (setq a ...) ...)
    (assert-eq (car result) 'let)
    ;; bindings = ((#:MVS (multiple-value-list ...)))
    (let ((bindings (cadr result)))
      (assert-true (consp bindings))
      ;; The binding value's car is multiple-value-list: (caadar bindings)
      (assert-eq (caadar bindings) 'multiple-value-list))))

(deftest multiple-value-setq-macro-with-body
  "Test MULTIPLE-VALUE-SETQ with body (body should be ignored)"
  ;; The macro ignores extra-body (only takes vars and form)
  (let ((result (our-macroexpand-1 '(multiple-value-setq (a b) (values 1 2) extra-body))))
    ;; Expansion: (let ((#:MVS ...)) (setq a ...) (setq b ...) (car #:MVS)) — 5 elements
    (assert-eq (car result) 'let)
    (assert-= (length result) 5)))

(deftest multiple-value-setq-macro-single-var
  "Test MULTIPLE-VALUE-SETQ with single variable"
  (let ((result (our-macroexpand-1 '(multiple-value-setq (x) (foo)))))
    (assert-eq (car result) 'let)
    (let ((bindings (cadr result)))
      ;; The binding value's car is multiple-value-list: (caadar bindings)
      (assert-eq (caadar bindings) 'multiple-value-list))))

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

(deftest error-cond-non-list-clause
  "Test COND error with non-list clause"
  (assert-signals error (our-macroexpand-1 '(cond x))))

(deftest error-cond-empty-clause
  "Test COND error with empty clause list"
  (assert-signals error (our-macroexpand-1 '(cond ()))))

(deftest-each error-psetq-malformed
  "PSETQ signals an error on malformed argument lists"
  :cases (("non-pair"  '(psetq x))
          ("odd-args"  '(psetq a 1 b)))
  (form)
  (assert-signals error (our-macroexpand-1 form)))

(deftest-each error-multiple-value-bind-malformed
  "MULTIPLE-VALUE-BIND signals an error on malformed variable lists"
  :cases (("non-list-vars"  '(multiple-value-bind a b (values 1 2) body))
          ("empty-vars"     '(multiple-value-bind () (values 1 2) body)))
  (form)
  (assert-signals error (our-macroexpand-1 form)))

(deftest-each error-multiple-value-setq-malformed
  "MULTIPLE-VALUE-SETQ signals an error on malformed variable lists"
  :cases (("non-list-vars"  '(multiple-value-setq a b (values 1 2)))
          ("empty-vars"     '(multiple-value-setq () (values 1 2))))
  (form)
  (assert-signals error (our-macroexpand-1 form)))

(deftest-each error-let*-malformed
  "LET* signals an error on malformed binding lists"
  :cases (("non-list-binding"    '(let* (x 1) body))
          ("invalid-binding"     '(let* ((a)) body)))
  (form)
  (assert-signals error (our-macroexpand-1 form)))

;;; Integration Tests

(deftest integration-when-evaluation
  "Integration test: WHEN macro produces correct runtime behavior"
  (let ((expanded (our-macroexpand '(when t 1 2 3))))
    ;; When test is true, should return last value
    (assert-equal expanded '(if t (progn 1 2 3) nil))))

(deftest integration-unless-evaluation
  "Integration test: UNLESS macro produces correct runtime behavior"
  (let ((expanded (our-macroexpand '(unless nil 1 2 3))))
    ;; When test is nil, should execute body
    (assert-equal expanded '(if nil nil (progn 1 2 3)))))

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

(deftest multiple-value-list-basic
  "Test MULTIPLE-VALUE-LIST basic expansion"
  (let ((result (our-macroexpand-1 '(multiple-value-list (values 1 2 3)))))
    (assert-eq (car result) 'let)
    ;; Body should contain multiple-value-call
    (let ((body (caddr result)))
      (assert-eq (car body) 'multiple-value-call))))

(deftest multiple-value-list-with-form
  "Test MULTIPLE-VALUE-LIST with arbitrary form"
  (let ((result (our-macroexpand-1 '(multiple-value-list (floor 10 3)))))
    (assert-eq (car result) 'let)
    ;; Should collect values from form
    (assert-true (search "multiple-value-call" (string-downcase (format nil "~S" result))))))

(deftest multiple-value-list-full-expansion
  "Test full expansion of MULTIPLE-VALUE-LIST"
  (let ((result (our-macroexpand '(multiple-value-list (values 1 2 3)))))
    ;; Should be fully expanded
    (assert-false (search "multiple-value-list" (string-downcase (format nil "~S" result))))))

(deftest multiple-value-list-collects-values
  "Integration test: MULTIPLE-VALUE-LIST collects values into list"
  ;; Expansion: (let ((#:ACC nil)) (multiple-value-call (lambda ...)) #:FORM)
  (let ((result (our-macroexpand-1 '(multiple-value-list (foo)))))
    (assert-eq (car result) 'let)
    ;; First body form (caddr) is the multiple-value-call
    (assert-eq (caaddr result) 'multiple-value-call)))

;;; Control Flow Macro Tests - dolist, dotimes, do, do*, case, typecase, loop

;;; DOLIST Tests

(deftest dolist-basic-expansion
  "Test DOLIST basic macro expansion"
  (let ((result (our-macroexpand-1 '(dolist (item list) body))))
    (assert-eq (car result) 'block)
    ;; Should contain let, tagbody for iteration
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest dolist-with-result-form
  "Test DOLIST with result form"
  (let ((result (our-macroexpand-1 '(dolist (item list result) body))))
    (assert-eq (car result) 'block)
    ;; Result form should be present at the end
    (assert-true (search "result" (string-downcase (format nil "~S" result))))))

(deftest dolist-with-multiple-body-forms
  "Test DOLIST with multiple body forms"
  (let ((result (our-macroexpand-1 '(dolist (item list) body1 body2 body3))))
    (assert-eq (car result) 'block)
    ;; All body forms should be present
    (assert-true (search "body1" (string-downcase (format nil "~S" result))))
    (assert-true (search "body2" (string-downcase (format nil "~S" result))))
    (assert-true (search "body3" (string-downcase (format nil "~S" result))))))

;;; DOTIMES Tests

(deftest dotimes-basic-expansion
  "Test DOTIMES basic macro expansion"
  (let ((result (our-macroexpand-1 '(dotimes (i 10) body))))
    (assert-eq (car result) 'block)
    ;; Should contain let with counter, tagbody for iteration
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest dotimes-with-result-form
  "Test DOTIMES with result form"
  (let ((result (our-macroexpand-1 '(dotimes (i 10 'done) body))))
    (assert-eq (car result) 'block)
    ;; Result form should be present
    (assert-true (search "done" (string-downcase (format nil "~S" result))))))

(deftest dotimes-with-zero-count
  "Test DOTIMES with zero count (should not execute body)"
  (let ((result (our-macroexpand-1 '(dotimes (i 0) body))))
    (assert-eq (car result) 'block)
    ;; Should still have proper structure
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

;;; DO Tests

(deftest do-basic-expansion
  "Test DO basic macro expansion"
  (let ((result (our-macroexpand-1 '(do ((i 0 (1+ i))) ((>= i 10) result) body))))
    (assert-eq (car result) 'block)
    ;; Should contain let for bindings, tagbody for iteration
    (assert-true (search "let" (string-downcase (format nil "~S" result))))
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest do-with-multiple-vars
  "Test DO with multiple variables"
  (let ((result (our-macroexpand-1 '(do ((i 0 (1+ i)) (j 10 (1- j))) ((= i j) i) body))))
    (assert-eq (car result) 'block)
    ;; Both variables should be present
    (assert-true (search "i" (string-downcase (format nil "~S" result))))
    (assert-true (search "j" (string-downcase (format nil "~S" result))))))

(deftest do-with-no-step
  "Test DO with no step form (variable keeps its value)"
  (let ((result (our-macroexpand-1 '(do ((x init)) (test result) body))))
    (assert-eq (car result) 'block)
    (assert-true (search "init" (string-downcase (format nil "~S" result))))))

(deftest do-with-multiple-body-forms
  "Test DO with multiple body forms"
  (let ((result (our-macroexpand-1 '(do ((i 0)) (test) body1 body2 body3))))
    (assert-eq (car result) 'block)
    (assert-true (search "body1" (string-downcase (format nil "~S" result))))))

;;; DO* Tests

(deftest do*-basic-expansion
  "Test DO* basic macro expansion (sequential binding)"
  (let ((result (our-macroexpand-1 '(do* ((i 0 (1+ i)) (j i (1+ j))) ((>= i 10) j) body))))
    (assert-eq (car result) 'block)
    ;; Should use let* for sequential binding
    (assert-true (search "let*" (string-downcase (format nil "~S" result))))))

(deftest do*-sequential-dependency
  "Test DO* with sequential variable dependency"
  (let ((result (our-macroexpand-1 '(do* ((x 1) (y (+ x 1))) (t y) body))))
    (assert-eq (car result) 'block)
    ;; let* should be present for sequential binding
    (assert-true (search "let*" (string-downcase (format nil "~S" result))))))

;;; CASE Tests

(deftest case-basic-expansion
  "Test CASE basic macro expansion"
  (let ((result (our-macroexpand-1 '(case key (a body-a) (b body-b)))))
    ;; Should expand to let with if/eql chain
    (assert-eq (car result) 'let)
    (assert-true (search "eql" (string-downcase (format nil "~S" result))))))

(deftest-each case-default-clause
  "CASE with otherwise/t clauses both produce a let including the default body"
  :cases (("otherwise" '(case key (a body-a) (otherwise default-body)))
          ("t-clause"  '(case key (a body-a) (t default-body))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)
    (assert-true (search "default-body" (string-downcase (format nil "~S" result))))))

(deftest case-with-list-of-keys
  "Test CASE with list of keys"
  (let ((result (our-macroexpand-1 '(case key ((a b c) body-abc) (d body-d)))))
    (assert-eq (car result) 'let)
    ;; Should check multiple keys with or
    (assert-true (search "or" (string-downcase (format nil "~S" result))))))

(deftest case-with-multiple-body-forms
  "Test CASE with multiple body forms in clause"
  (let ((result (our-macroexpand-1 '(case key (a body1 body2 body3)))))
    (assert-eq (car result) 'let)
    ;; Body forms should be wrapped in progn
    (assert-true (search "progn" (string-downcase (format nil "~S" result))))))

;;; TYPECASE Tests

(deftest typecase-basic-expansion
  "Test TYPECASE basic macro expansion"
  (let ((result (our-macroexpand-1 '(typecase val (string body-string) (integer body-int)))))
    (assert-eq (car result) 'let)
    ;; Should use typep for type checking
    (assert-true (search "typep" (string-downcase (format nil "~S" result))))))

(deftest-each typecase-default-clause
  "TYPECASE with otherwise/t clauses both produce a let including the default body"
  :cases (("otherwise" '(typecase val (string body-string) (otherwise default-body)))
          ("t-clause"  '(typecase val (string body-string) (t default-body))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)
    (assert-true (search "default-body" (string-downcase (format nil "~S" result))))))

(deftest typecase-with-multiple-body-forms
  "Test TYPECASE with multiple body forms"
  (let ((result (our-macroexpand-1 '(typecase val (string body1 body2)))))
    (assert-eq (car result) 'let)
    (assert-true (search "progn" (string-downcase (format nil "~S" result))))))

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

(deftest loop-with-collect
  "Test LOOP with collect accumulation"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 5 collect (* i 2)))))
    (assert-eq (car result) 'block)
    ;; Should contain cons for collection
    (assert-true (search "cons" (string-downcase (format nil "~S" result))))))

(deftest loop-with-sum
  "Test LOOP with sum accumulation"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 10 sum i))))
    (assert-eq (car result) 'block)
    ;; Should contain + for summation
    (assert-true (search "+" (string-downcase (format nil "~S" result))))))

(deftest-each loop-conditional-forms
  "LOOP with while/until/below all expand to block+tagbody"
  :cases (("while"    '(loop for i from 1 to 10 while (< i 5) do (print i)))
          ("until"    '(loop for i from 1 to 10 until (>= i 5) do (print i)))
          ("below"    '(loop for i from 0 below 5 do (print i))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'block (car result))
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest loop-with-count
  "Test LOOP with count accumulation"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 10 count (evenp i)))))
    (assert-eq (car result) 'block)
    ;; Should contain counting logic
    (assert-true (search "+" (string-downcase (format nil "~S" result))))))

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
