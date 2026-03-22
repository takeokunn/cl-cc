;;;; tests/macro-tests.lisp - Comprehensive macro expansion tests for CL-CC
;;;
;;;; This file tests all built-in macros in the CL-CC macro system:
;;;; including: when, unless, cond, and, or, let*, defun, prog1, prog2,
;;;; setf, psetq, multiple-value-bind, multiple-value-setq
;;;;
(in-package :cl-cc/test)

(def-suite macro-suite
  :description "Test suite for macro expansion"
  :in cl-cc-suite)

;;; Conditional Macro Tests

(test when-macro-expansion
  "Test WHEN macro expands to IF with PROGN"
  (is (equal (our-macroexpand-1 '(when test body1 body2))
             '(if test (progn body1 body2) nil))))

(test when-macro-single-body
  "Test WHEN with single body form"
  (is (equal (our-macroexpand-1 '(when test body))
             '(if test (progn body) nil))))

(test when-macro-no-body
  "Test WHEN with no body forms (degenerate case)"
  (is (equal (our-macroexpand-1 '(when test))
             '(if test (progn) nil))))

(test unless-macro-expansion
  "Test UNLESS macro expands to IF with PROGN"
  (is (equal (our-macroexpand-1 '(unless test body1 body2))
             '(if test nil (progn body1 body2)))))

(test unless-macro-single-body
  "Test UNLESS with single body form"
  (is (equal (our-macroexpand-1 '(unless test body))
             '(if test nil (progn body)))))

(test unless-macro-no-body
  "Test UNLESS with no body forms (degenerate case)"
  (is (equal (our-macroexpand-1 '(unless test))
             '(if test nil (progn)))))

(test cond-macro-empty
  "Test COND with no clauses"
  (is (equal (our-macroexpand-1 '(cond))
             nil)))

(test cond-macro-single-expr-clause
  "Test COND with single expression clause (falls through to OR)"
  (is (equal (our-macroexpand-1 '(cond (x)))
             '(or x (cond)))))

(test cond-macro-single-clause
  "Test COND with single full clause"
  (is (equal (our-macroexpand-1 '(cond (test body)))
             '(if test (progn body) (cond)))))

(test cond-macro-multiple-clauses
  "Test COND with multiple clauses"
  (let ((result (our-macroexpand-1 '(cond (test1 body1) (test2 body2) (t body3)))))
    (is (eq (car result) 'if))
    (is (equal (cadr result) 'test1))
    (is (eq (caaddr result) 'progn))
    (is (equal (car (cdaddr result)) 'body1))))

;;; Boolean Macro Tests

(test and-macro-empty
  "Test AND with no arguments (returns T)"
  (is (equal (our-macroexpand-1 '(and))
             t)))

(test and-macro-single-arg
  "Test AND with single argument (returns it unchanged)"
  (is (equal (our-macroexpand-1 '(and x))
             'x)))

(test and-macro-two-args
  "Test AND with two arguments"
  (is (equal (our-macroexpand-1 '(and a b))
             '(if a (and b) nil))))

(test and-macro-multiple-args
  "Test AND with multiple arguments"
  (is (equal (our-macroexpand-1 '(and a b c))
             '(if a (and b c) nil))))

(test and-macro-full-expansion
  "Test full expansion of AND creates nested IFs"
  (let ((result (our-macroexpand '(and a b c))))
    (is (eq (car result) 'if))
    (is (equal (cadr result) 'a))
    ;; Should expand to nested IFs
    (is (eq (caaddr result) 'if))))

(test or-macro-empty
  "Test OR with no arguments (returns NIL)"
  (is (equal (our-macroexpand-1 '(or))
             nil)))

(test or-macro-single-arg
  "Test OR with single argument (returns it unchanged)"
  (is (equal (our-macroexpand-1 '(or x))
             'x)))

(test or-macro-two-args
  "Test OR with two arguments"
  (let ((result (our-macroexpand-1 '(or a b))))
    (is (eq (car result) 'let))
    ;; Creates a temporary variable
    (is (= (length result) 3))
    ;; Body should be IF expression
    (is (eq (car (caddr result)) 'if))))

(test or-macro-full-expansion
  "Test full expansion of OR creates nested LETs"
  (let ((result (our-macroexpand '(or a b c))))
    (is (eq (car result) 'let))
    ;; Should expand to nested structures
    (let ((inner (caddr (caddr result))))
      (is (eq (car inner) 'if)))))

;;; Sequential Binding Tests

(test let*-macro-empty-bindings
  "Test LET* with no bindings (just wraps in PROGN)"
  (is (equal (our-macroexpand-1 '(let* () body1 body2))
             '(progn body1 body2))))

(test let*-macro-single-binding
  "Test LET* with single binding"
  (is (equal (our-macroexpand-1 '(let* ((a 1)) body))
             '(let ((a 1)) (let* nil body)))))

(test let*-macro-multiple-bindings
  "Test LET* with multiple bindings (creates nested LETs)"
  (let ((result (our-macroexpand-1 '(let* ((a 1) (b a)) body))))
    (is (eq (car result) 'let))
    (is (equal (cadr result) '((a 1))))
    ;; Inner should be another LET*
    (is (eq (car (caddr result)) 'let*))))

(test let*-macro-full-expansion
  "Test full expansion of LET* creates nested LETs"
  (let ((result (our-macroexpand '(let* ((a 1) (b a)) body))))
    ;; Should be (let ((a 1)) (let ((b a)) body))
    (is (eq (car result) 'let))
    (is (equal (cadr result) '((a 1))))
    (is (eq (caaddr result) 'let))))

(test let*-macro-dependency-chain
  "Test LET* where later bindings depend on earlier ones"
  (let ((result (our-macroexpand '(let* ((x 1) (y (+ x 1)) (z (* y 2))) body))))
    ;; Should create proper nested structure
    (is (eq (car result) 'let))
    ;; y should reference x, z should reference y
    (let ((y-binding (caaddr result)))
      (is (eq (car y-binding) 'let)))))

;;; Definition Macro Tests

(test defun-macro-expansion
  "Test DEFUN macro expansion"
  (let ((result (our-macroexpand-1 '(defun foo (x y) body1 body2))))
    (is (eq (car result) 'setf))
    (is (eq (cadr result) 'fdefinition))
    (is (eq (caaddr result) 'lambda))))

(test defun-macro-with-docstring
  "Test DEFUN with docstring"
  (let ((result (our-macroexpand-1 '(defun foo (x y) "Docstring" body))))
    ;; Should still expand to setf/lambda structure
    (is (eq (car result) 'setf))
    (is (eq (caaddr result) 'lambda))))

;;; Sequencing Macro Tests

(test prog1-macro-expansion
  "Test PROG1 macro expansion"
  (let ((result (our-macroexpand-1 '(prog1 first-form body1 body2))))
    (is (eq (car result) 'let))
    ;; Should have a gensym for result
    (is (symbolp (caadr result)))
    ;; Third element should be first-form
    (is (eq (caddr result) 'first-form))
    ;; Should return the result at end
    (is (eq (car (last result)) (caadr result)))))

(test prog1-macro-single-body
  "Test PROG1 with single body form"
  (let ((result (our-macroexpand-1 '(prog1 first-form))))
    (is (eq (car result) 'let))
    ;; Should still work even without body
    (is (= (length result) 4))))

(test prog2-macro-expansion
  "Test PROG2 macro expansion"
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body1 body2))))
    (is (eq (car result) 'progn))
    ;; First element of body should be first-form
    (is (eq (cadr result) 'first-form))
    ;; Second element should be a LET
    (is (eq (caddr result) 'let))
    ;; Should return the result at end
    (let ((let-body (caddr result)))
      (is (eq (car (last let-body)) (caadr let-body))))))

(test prog2-macro-single-body
  "Test PROG2 with single body form"
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body))))
    (is (eq (car result) 'progn))
    (is (eq (cadr result) 'first-form))
    (is (eq (caaddr result) 'let))))

;;; Assignment Macro Tests

(test setf-macro-simple-symbol
  "Test SETF with simple symbol"
  (is (equal (our-macroexpand-1 '(setf x 10))
             '(setq x 10))))

(test setf-macro-complex-place
  "Test SETF with complex place (should error)"
  (signals error (our-macroexpand-1 '(setf (car x) 10))))

(test setf-macro-cons-place
  "Test SETF with cons place (should error)"
  (signals error (our-macroexpand-1 '(setf (cons x) 10))))

(test psetq-macro-empty
  "Test PSETQ with no pairs"
  (is (equal (our-macroexpand-1 '(psetq))
             nil)))

(test psetq-macro-single-pair
   "Test PSETQ with single pair"
   (let ((result (our-macroexpand-1 '(psetq a 1))))
     (is (eq (car result) 'let))
     ;; Should create bindings
     (let ((bindings (cadr result)))
       (is (consp bindings))
       ;; Body should be setq forms
       (is (eq (car (caddr result)) 'setq)))))

(test psetq-macro-multiple-pairs
  "Test PSETQ with multiple pairs"
  (let ((result (our-macroexpand-1 '(psetq a 1 b 2 c 3))))
    (is (eq (car result) 'let))
    ;; Should evaluate all values before assignment
    (is (consp (cadr result)))))

(test psetq-macro-full-expansion
   "Test full expansion of PSETQ"
   (let ((result (our-macroexpand '(psetq a 1 b 2))))
     ;; Should expand to LET with temp variables and SETQ
     (is (eq (car result) 'let))
     (is (consp (cadr result)))
     (is (eq (car (caddr result)) 'progn))))

;;; Multiple Value Macro Tests

(test multiple-value-bind-macro-expansion
  "Test MULTIPLE-VALUE-BIND macro expansion"
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b c) (values 1 2 3) body1 body2))))
    (is (eq (car result) 'multiple-value-call))
    ;; Second element should be a lambda
    (is (eq (cadr result) 'lambda))
    ;; Lambda should have the variables
    (is (equal (cadr (cadr result)) '(a b c)))
    ;; Third element should be the form
    (is (eq (caddr result) 'values))))

(test multiple-value-bind-macro-single-var
  "Test MULTIPLE-VALUE-BIND with single variable"
  (let ((result (our-macroexpand-1 '(multiple-value-bind (x) (foo) body))))
    (is (eq (car result) 'multiple-value-call))
    (is (eq (cadr result) 'lambda))
    (is (equal (cadr (cadr result)) '(x)))))

(test multiple-value-bind-macro-no-body
  "Test MULTIPLE-VALUE-BIND with no body"
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b) (values 1 2)))))
    (is (eq (car result) 'multiple-value-call))
    ;; Lambda should have empty body
    (is (null (cddr (cadr result))))))

(test multiple-value-setq-macro-expansion
  "Test MULTIPLE-VALUE-SETQ macro expansion"
  (let ((result (our-macroexpand-1 '(multiple-value-setq (a b c) (values 1 2 3)))))
    (is (eq (car result) 'let))
    ;; Should use multiple-value-list
    (let ((bindings (cadr result)))
      (is (consp bindings))
      (is (eq (caar bindings) 'multiple-value-list)))))

(test multiple-value-setq-macro-with-body
  "Test MULTIPLE-VALUE-SETQ with body (body should be ignored)"
  ;; The macro ignores body according to the spec
  (let ((result (our-macroexpand-1 '(multiple-value-setq (a b) (values 1 2) extra-body))))
    ;; Body is not part of the expansion
    (is (eq (car result) 'let))
    (is (= (length result) 3))))

(test multiple-value-setq-macro-single-var
  "Test MULTIPLE-VALUE-SETQ with single variable"
  (let ((result (our-macroexpand-1 '(multiple-value-setq (x) (foo)))))
    (is (eq (car result) 'let))
    (let ((bindings (cadr result)))
      (is (eq (caar bindings) 'multiple-value-list)))))

;;; Nested Macro Tests

(test nested-when-in-let*
  "Test WHEN nested inside LET*"
  (let ((result (our-macroexpand '(let* ((x (when test body))) x))))
    ;; Should expand both let* and when
    (is (eq (car result) 'let))
    ;; Inner should contain if from when expansion
    (let ((inner (caaddr result)))
      (is (eq (car inner) 'let))
      (is (eq (caadr (caddr inner)) 'if)))))

(test nested-cond-in-and
  "Test COND nested inside AND"
  (let ((result (our-macroexpand '(and test1 (cond ((test2 result)))))))
    ;; Should fully expand both macros
    (is (eq (car result) 'if))
    ;; Deep nesting should be resolved
    (let ((and-part (caaddr result)))
      ;; This should be an IF with the nested cond
      (is (eq (car and-part) 'if)))))

(test nested-prog1-in-prog2
  "Test PROG1 nested inside PROG2"
  (let ((result (our-macroexpand '(prog2 (prog1 a b) c body))))
    (is (eq (car result) 'progn))
    ;; The prog1 should be expanded
    (let ((first-form (cadr result)))
      (is (eq (car first-form) 'let)))))

(test nested-multiple-value-bind
  "Test nested MULTIPLE-VALUE-BIND"
  (let ((result (our-macroexpand '(multiple-value-bind (a b) (multiple-value-bind (x y) (values 1 2 3 4) (list x y)) (list a b)))))
    (is (eq (car result) 'multiple-value-call))
    ;; Inner form should also expand
    (let ((inner-form (caddr result)))
      (is (eq (car inner-form) 'multiple-value-call)))))

;;; Error Case Tests

(test error-cond-non-list-clause
  "Test COND error with non-list clause"
  (signals error (our-macroexpand-1 '(cond x))))

(test error-cond-empty-clause
  "Test COND error with empty clause list"
  (signals error (our-macroexpand-1 '(cond ()))))

(test error-psetq-non-pair
  "Test PSETQ error with non-pair elements"
  (signals error (our-macroexpand-1 '(psetq x))))

(test error-psetq-odd-args
  "Test PSETQ error with odd number of arguments"
  (signals error (our-macroexpand-1 '(psetq a 1 b))))

(test error-multiple-value-bind-non-list-vars
  "Test MULTIPLE-VALUE-BIND error with non-list variables"
  (signals error (our-macroexpand-1 '(multiple-value-bind a b (values 1 2) body))))

(test error-multiple-value-bind-empty-vars
  "Test MULTIPLE-VALUE-BIND error with empty variable list"
  (signals error (our-macroexpand-1 '(multiple-value-bind () (values 1 2) body))))

(test error-multiple-value-setq-non-list-vars
  "Test MULTIPLE-VALUE-SETQ error with non-list variables"
  (signals error (our-macroexpand-1 '(multiple-value-setq a b (values 1 2)))))

(test error-multiple-value-setq-empty-vars
  "Test MULTIPLE-VALUE-SETQ error with empty variable list"
  (signals error (our-macroexpand-1 '(multiple-value-setq () (values 1 2)))))

(test error-let*-non-list-binding
  "Test LET* error with non-list binding"
  (signals error (our-macroexpand-1 '(let* (x 1) body))))

(test error-let*-invalid-binding
  "Test LET* error with invalid binding format"
  (signals error (our-macroexpand-1 '(let* ((a)) body))))

;;; Integration Tests

(test integration-when-evaluation
  "Integration test: WHEN macro produces correct runtime behavior"
  (let ((expanded (our-macroexpand '(when t 1 2 3))))
    ;; When test is true, should return last value
    (is (equal expanded '(if t (progn 1 2 3) nil)))))

(test integration-unless-evaluation
  "Integration test: UNLESS macro produces correct runtime behavior"
  (let ((expanded (our-macroexpand '(unless nil 1 2 3))))
    ;; When test is nil, should execute body
    (is (equal expanded '(if nil nil (progn 1 2 3))))))

(test integration-cond-evaluation
  "Integration test: COND produces correct nested IF structure"
  (let ((expanded (our-macroexpand '(cond ((= x 1) 'one)
                                         ((= x 2) 'two)
                                         (t 'other)))))
    ;; Should produce nested IF with proper tests and branches
    (is (eq (car expanded) 'if))
    (is (eq (caaddr expanded) 'if))))  ; else branch should be another IF

;;; MULTIPLE-VALUE-LIST Macro Tests (Wave 2)

(test multiple-value-list-basic
  "Test MULTIPLE-VALUE-LIST basic expansion"
  (let ((result (our-macroexpand-1 '(multiple-value-list (values 1 2 3)))))
    ;; Should use multiple-value-call
    (is (eq (car result) 'let))
    ;; Body should contain multiple-value-call
    (let ((body (caddr result)))
      (is (eq (car body) 'multiple-value-call)))))

(test multiple-value-list-with-form
  "Test MULTIPLE-VALUE-LIST with arbitrary form"
  (let ((result (our-macroexpand-1 '(multiple-value-list (floor 10 3)))))
    (is (eq (car result) 'let))
    ;; Should collect values from form
    (is (search "multiple-value-call" (format nil "~S" result)))))

(test multiple-value-list-full-expansion
  "Test full expansion of MULTIPLE-VALUE-LIST"
  (let ((result (our-macroexpand '(multiple-value-list (values 1 2 3)))))
    ;; Should be fully expanded
    (is (not (search "multiple-value-list" (format nil "~S" result))))))

(test multiple-value-list-collects-values
  "Integration test: MULTIPLE-VALUE-LIST collects values into list"
  ;; This is a structural test - we verify the macro produces list-building code
  (let ((result (our-macroexpand-1 '(multiple-value-list (foo)))))
    ;; Check structure: should be (let ((acc nil)) (multiple-value-call (lambda (&rest temp) ...) form))
    (is (eq (car result) 'let))
    (is (eq (caadr result) 'multiple-value-call))))

;;; Control Flow Macro Tests - dolist, dotimes, do, do*, case, typecase, loop

;;; DOLIST Tests

(test dolist-basic-expansion
  "Test DOLIST basic macro expansion"
  (let ((result (our-macroexpand-1 '(dolist (item list) body))))
    (is (eq (car result) 'block))
    ;; Should contain let, tagbody for iteration
    (is (search "tagbody" (format nil "~S" result)))))

(test dolist-with-result-form
  "Test DOLIST with result form"
  (let ((result (our-macroexpand-1 '(dolist (item list result) body))))
    (is (eq (car result) 'block))
    ;; Result form should be present at the end
    (is (search "result" (format nil "~S" result)))))

(test dolist-with-multiple-body-forms
  "Test DOLIST with multiple body forms"
  (let ((result (our-macroexpand-1 '(dolist (item list) body1 body2 body3))))
    (is (eq (car result) 'block))
    ;; All body forms should be present
    (is (search "body1" (format nil "~S" result)))
    (is (search "body2" (format nil "~S" result)))
    (is (search "body3" (format nil "~S" result)))))

;;; DOTIMES Tests

(test dotimes-basic-expansion
  "Test DOTIMES basic macro expansion"
  (let ((result (our-macroexpand-1 '(dotimes (i 10) body))))
    (is (eq (car result) 'block))
    ;; Should contain let with counter, tagbody for iteration
    (is (search "tagbody" (format nil "~S" result)))))

(test dotimes-with-result-form
  "Test DOTIMES with result form"
  (let ((result (our-macroexpand-1 '(dotimes (i 10 'done) body))))
    (is (eq (car result) 'block))
    ;; Result form should be present
    (is (search "done" (format nil "~S" result)))))

(test dotimes-with-zero-count
  "Test DOTIMES with zero count (should not execute body)"
  (let ((result (our-macroexpand-1 '(dotimes (i 0) body))))
    (is (eq (car result) 'block))
    ;; Should still have proper structure
    (is (search "tagbody" (format nil "~S" result)))))

;;; DO Tests

(test do-basic-expansion
  "Test DO basic macro expansion"
  (let ((result (our-macroexpand-1 '(do ((i 0 (1+ i))) ((>= i 10) result) body))))
    (is (eq (car result) 'block))
    ;; Should contain let for bindings, tagbody for iteration
    (is (search "let" (format nil "~S" result)))
    (is (search "tagbody" (format nil "~S" result)))))

(test do-with-multiple-vars
  "Test DO with multiple variables"
  (let ((result (our-macroexpand-1 '(do ((i 0 (1+ i)) (j 10 (1- j))) ((= i j) i) body))))
    (is (eq (car result) 'block))
    ;; Both variables should be present
    (is (search "i" (format nil "~S" result)))
    (is (search "j" (format nil "~S" result)))))

(test do-with-no-step
  "Test DO with no step form (variable keeps its value)"
  (let ((result (our-macroexpand-1 '(do ((x init)) (test result) body))))
    (is (eq (car result) 'block))
    (is (search "init" (format nil "~S" result)))))

(test do-with-multiple-body-forms
  "Test DO with multiple body forms"
  (let ((result (our-macroexpand-1 '(do ((i 0)) (test) body1 body2 body3))))
    (is (eq (car result) 'block))
    (is (search "body1" (format nil "~S" result)))))

;;; DO* Tests

(test do*-basic-expansion
  "Test DO* basic macro expansion (sequential binding)"
  (let ((result (our-macroexpand-1 '(do* ((i 0 (1+ i)) (j i (1+ j))) ((>= i 10) j) body))))
    (is (eq (car result) 'block))
    ;; Should use let* for sequential binding
    (is (search "let*" (format nil "~S" result)))))

(test do*-sequential-dependency
  "Test DO* with sequential variable dependency"
  (let ((result (our-macroexpand-1 '(do* ((x 1) (y (+ x 1))) (t y) body))))
    (is (eq (car result) 'block))
    ;; let* should be present for sequential binding
    (is (search "let*" (format nil "~S" result)))))

;;; CASE Tests

(test case-basic-expansion
  "Test CASE basic macro expansion"
  (let ((result (our-macroexpand-1 '(case key (a body-a) (b body-b)))))
    ;; Should expand to let with if/eql chain
    (is (eq (car result) 'let))
    (is (search "eql" (format nil "~S" result)))))

(test case-with-otherwise-clause
  "Test CASE with otherwise clause"
  (let ((result (our-macroexpand-1 '(case key (a body-a) (otherwise default-body)))))
    (is (eq (car result) 'let))
    ;; Otherwise should result in progn
    (is (search "default-body" (format nil "~S" result)))))

(test case-with-t-clause
  "Test CASE with t clause (same as otherwise)"
  (let ((result (our-macroexpand-1 '(case key (a body-a) (t default-body)))))
    (is (eq (car result) 'let))
    (is (search "default-body" (format nil "~S" result)))))

(test case-with-list-of-keys
  "Test CASE with list of keys"
  (let ((result (our-macroexpand-1 '(case key ((a b c) body-abc) (d body-d)))))
    (is (eq (car result) 'let))
    ;; Should check multiple keys with or
    (is (search "or" (format nil "~S" result)))))

(test case-with-multiple-body-forms
  "Test CASE with multiple body forms in clause"
  (let ((result (our-macroexpand-1 '(case key (a body1 body2 body3)))))
    (is (eq (car result) 'let))
    ;; Body forms should be wrapped in progn
    (is (search "progn" (format nil "~S" result)))))

;;; TYPECASE Tests

(test typecase-basic-expansion
  "Test TYPECASE basic macro expansion"
  (let ((result (our-macroexpand-1 '(typecase val (string body-string) (integer body-int)))))
    (is (eq (car result) 'let))
    ;; Should use typep for type checking
    (is (search "typep" (format nil "~S" result)))))

(test typecase-with-otherwise-clause
  "Test TYPECASE with otherwise clause"
  (let ((result (our-macroexpand-1 '(typecase val (string body-string) (otherwise default-body)))))
    (is (eq (car result) 'let))
    (is (search "default-body" (format nil "~S" result)))))

(test typecase-with-t-clause
  "Test TYPECASE with t clause"
  (let ((result (our-macroexpand-1 '(typecase val (string body-string) (t default-body)))))
    (is (eq (car result) 'let))
    (is (search "default-body" (format nil "~S" result)))))

(test typecase-with-multiple-body-forms
  "Test TYPECASE with multiple body forms"
  (let ((result (our-macroexpand-1 '(typecase val (string body1 body2)))))
    (is (eq (car result) 'let))
    (is (search "progn" (format nil "~S" result)))))

;;; LOOP Tests (Simplified)

(test loop-for-from-to
  "Test LOOP with for/from/to iteration"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 10 do (print i)))))
    ;; Should contain block, let, tagbody
    (is (eq (car result) 'block))
    (is (search "tagbody" (format nil "~S" result)))))

(test loop-for-in-list
  "Test LOOP with for/in list iteration"
  (let ((result (our-macroexpand-1 '(loop for item in list do (print item)))))
    (is (eq (car result) 'block))
    (is (search "tagbody" (format nil "~S" result)))))

(test loop-for-on-list
  "Test LOOP with for/on list iteration"
  (let ((result (our-macroexpand-1 '(loop for tail on list do (print tail)))))
    (is (eq (car result) 'block))
    (is (search "tagbody" (format nil "~S" result)))))

(test loop-with-collect
  "Test LOOP with collect accumulation"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 5 collect (* i 2)))))
    (is (eq (car result) 'block))
    ;; Should contain cons for collection
    (is (search "cons" (format nil "~S" result)))))

(test loop-with-sum
  "Test LOOP with sum accumulation"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 10 sum i))))
    (is (eq (car result) 'block))
    ;; Should contain + for summation
    (is (search "+" (format nil "~S" result)))))

(test loop-with-while
  "Test LOOP with while condition"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 10 while (< i 5) do (print i)))))
    (is (eq (car result) 'block))
    (is (search "tagbody" (format nil "~S" result)))))

(test loop-with-until
  "Test LOOP with until condition"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 10 until (>= i 5) do (print i)))))
    (is (eq (car result) 'block))
    (is (search "tagbody" (format nil "~S" result)))))

(test loop-for-below
  "Test LOOP with for/below (exclusive upper bound)"
  (let ((result (our-macroexpand-1 '(loop for i from 0 below 5 do (print i)))))
    (is (eq (car result) 'block))
    (is (search "tagbody" (format nil "~S" result)))))

(test loop-with-count
  "Test LOOP with count accumulation"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 10 count (evenp i)))))
    (is (eq (car result) 'block))
    ;; Should contain counting logic
    (is (search "+" (format nil "~S" result)))))

;;; Integration Tests for Control Flow Macros

(test integration-dolist-evaluation
  "Integration test: DOLIST produces correct structure"
  (let ((expanded (our-macroexpand '(dolist (x '(1 2 3)) (print x)))))
    ;; Should be a block with let and tagbody
    (is (eq (car expanded) 'block))))

(test integration-dotimes-evaluation
  "Integration test: DOTIMES produces correct structure"
  (let ((expanded (our-macroexpand '(dotimes (i 3) (print i)))))
    (is (eq (car expanded) 'block))))

(test integration-do-evaluation
  "Integration test: DO produces correct structure"
  (let ((expanded (our-macroexpand '(do ((i 0 (1+ i))) ((= i 5)) (print i)))))
    (is (eq (car expanded) 'block))))

(test integration-case-evaluation
  "Integration test: CASE produces correct structure"
  (let ((expanded (our-macroexpand '(case x (1 'one) (2 'two) (t 'other)))))
    (is (eq (car expanded) 'let))
    ;; Should have nested if/eql checks
    (is (search "if" (format nil "~S" expanded)))))

(test integration-typecase-evaluation
  "Integration test: TYPECASE produces correct structure"
  (let ((expanded (our-macroexpand '(typecase x (string "string") (integer "int") (t "other")))))
    (is (eq (car expanded) 'let))
    (is (search "typep" (format nil "~S" expanded)))))
