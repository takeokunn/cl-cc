;;;; compiler-tests-call-forms.lisp — Function call, lambda list, variadic, and multiple-values compiler tests
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; Function Call Tests

(deftest-each compile-function-call-structure
  "Lambda, flet, labels, and higher-order forms all compile to a valid vm-program."
  :cases (("lambda-simple"       "((lambda (x) x) 5)")
          ("lambda-multi-arg"    "((lambda (a b c) (+ a (+ b c))) 1 2 3)")
          ("lambda-nested"       "((lambda (x) (+ x 1)) ((lambda (y) (* y 2)) 3))")
          ("lambda-return-fn"    "((lambda (n) (lambda (x) (+ x n))) 5)")
          ("flet-basic"          "(flet ((double (x) (* x 2))) (double 21))")
          ("flet-multi"          "(flet ((add1 (x) (+ x 1)) (add2 (x) (+ x 2))) (add2 (add1 10)))")
          ("labels-recursive"    "(labels ((count (n) (if (= n 0) 0 (+ 1 (count (- n 1)))))) (count 5))")
          ("labels-mutual"       "(labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (even? 10))")
          ("labels-with-let"     "(let ((x 10)) (labels ((rec (n) (if (= n 0) x (+ 1 (rec (- n 1)))))) (rec 3)))"))
  (form)
  (let* ((result (compile-string form :target :vm))
         (program (compilation-result-program result)))
    (assert-false (null program))
    (assert-type vm-program program)))

;;; Multiple Top-Level Forms and Values Tests

(deftest-each compile-multiple-forms-and-values
  "Multiple top-level forms, values, and multiple-value-bind all return expected numeric results."
  :cases (("forms-simple"    6  "(defun foo (x) (+ x 1)) (foo 5)")
          ("forms-progn"     3  "1 2 3")
          ("forms-chain"     12 "(defun add1 (x) (+ x 1)) (defun add2 (x) (add1 (add1 x))) (add2 10)")
          ("forms-let-call"  15 "(defun triple (x) (* x 3)) (let ((y 5)) (triple y))")
          ("forms-recursion"  6 "(defun sum-to (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))) (sum-to 3)")
          ("values-single"   42 "(values 42)")
          ("values-primary"   1 "(values 1 2 3)")
          ("mvb-basic"        3 "(multiple-value-bind (a b) (values 1 2) (+ a b))")
          ("apply-user"       6 "(defun my-add (a b c) (+ a (+ b c))) (apply my-add (quote (1 2 3)))")
          ("mvb-single"      42 "(multiple-value-bind (x) (values 42) x)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-values-nil
  "values and multiple-value-bind return nil in edge cases."
  :cases (("empty"        "(values)")
          ("mvb-missing"  "(multiple-value-bind (a b c) (values 1 2) c)"))
  (form)
  (assert-true (null (run-string form))))


;;; Extended List and Macro Tests

(deftest-each compile-keywordp-and-list-macros
  "keywordp, push/pop/incf/decf/nth/nthcdr/nreverse return the expected numeric values."
  :cases (("keyword"     1  "(keywordp :foo)")
          ("non-keyword" 0  "(keywordp 'foo)")
          ("push-front"  3  "(let ((lst nil)) (push 1 lst) (push 2 lst) (push 3 lst) (car lst))")
          ("pop"         1  "(let ((lst (list 1 2 3))) (pop lst))")
          ("incf"        5  "(let ((x 3)) (incf x 2) x)")
          ("decf"        1  "(let ((x 3)) (decf x 2) x)")
          ("nth"         30 "(nth 2 (list 10 20 30 40))")
          ("nthcdr"      30 "(car (nthcdr 2 (list 10 20 30 40)))")
          ("nreverse"    3  "(car (nreverse (list 1 2 3)))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-member-builtin
  "member finds element in list"
  (assert-true (not (null (run-string "(member 3 (list 1 2 3 4))")))))

(deftest-each compile-numeric-predicates-and-case
  "Numeric predicates return CL booleans and case returns the selected value."
  :cases (("zerop-0"    t  "(zerop 0)")
          ("zerop-5"    nil  "(zerop 5)")
          ("plusp-5"    t  "(plusp 5)")
          ("minusp-neg" t  "(minusp (- 0 3))")
          ("evenp-true"  t "(evenp 4)")
          ("evenp-false" nil "(evenp 3)")
          ("oddp-true"   t "(oddp 3)")
          ("case-match"     2  "(case 'b (a 1) (b 2) (c 3))")
          ("case-otherwise" 99 "(case 'z (a 1) (otherwise 99))"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest compile-keyword-self-eval
  "keywords evaluate to themselves; typecase dispatches on type."
  (assert-eq :test (run-string ":test"))
  (assert-run= 1 "(typecase 42 (integer 1) (string 2) (otherwise 3))"))

;;; Extended Lambda List Tests (&rest, &optional, &key)

;; &rest tests
(deftest-each compile-rest-params
  "&rest collects remaining arguments into a list."
  :cases (("basic"     '(1 2 3) "(defun my-list (&rest args) args) (my-list 1 2 3)")
          ("required"  '(1 2 3) "(defun fr (a &rest r) (cons a r)) (fr 1 2 3)")
          ("single"    '(42)    "(defun my-list1 (&rest args) args) (my-list1 42)"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest-each compile-rest-extras
  "&rest edge cases: empty produces nil; car and recursive length work on rest lists."
  :cases (("empty-nil"   nil "(defun my-list0 (&rest args) args) (my-list0)")
          ("car-rest"    10  "(defun first-rest (&rest args) (car args)) (first-rest 10 20 30)")
          ("length-rest"  3  "(defun my-len (lst) (if (null lst) 0 (+ 1 (my-len (cdr lst))))) (defun count-args (&rest args) (my-len args)) (count-args 1 2 3)"))
  (expected form)
  (assert-equal expected (run-string form)))

;; &optional tests
(deftest-each compile-optional-params
  "&optional parameters use provided values, defaults, or nil."
  :cases (("provided"          15 "(defun opt-add (a &optional b) (if b (+ a b) a)) (opt-add 10 5)")
          ("missing"           10 "(defun opt-add2 (a &optional b) (if b (+ a b) a)) (opt-add2 10)")
          ("default"           10 "(defun opt-def (a &optional (b 0)) (+ a b)) (opt-def 10)")
          ("default-overridden" 15 "(defun opt-def2 (a &optional (b 0)) (+ a b)) (opt-def2 10 5)")
          ("multiple"           6 "(defun opt-multi (a &optional (b 0) (c 0)) (+ (+ a b) c)) (opt-multi 1 2 3)")
          ("partial"            3 "(defun opt-part (a &optional (b 0) (c 0)) (+ (+ a b) c)) (opt-part 1 2)"))
  (expected form)
  (assert-= expected (run-string form)))

;; &key tests
(deftest-each compile-key-params
  "&key parameters support defaults, reordering, and combination with required args."
  :cases (("basic"         7  "(defun key-add (&key x y) (+ x y)) (key-add :x 3 :y 4)")
          ("default"      10  "(defun key-def (&key (x 0) (y 0)) (+ x y)) (key-def :x 10)")
          ("reorder"       7  "(defun key-ord (&key x y) (+ x y)) (key-ord :y 4 :x 3)")
          ("four-keys"   '(1 2 3 4) "(defun key-many (&key a b c d) (list a b c d)) (key-many :d 4 :b 2 :a 1 :c 3)")
          ("four-defaults" '(1 20 3 4) "(defun key-many-def (&key (a 1) (b 2) (c 3) (d 4)) (list a b c d)) (key-many-def :b 20 :d 4)")
          ("with-required" 30 "(defun rk (a &key (b 0)) (+ a b)) (rk 10 :b 20)"))
  (expected form)
  (assert-equal expected (run-string form)))

;; lambda with extended params
(deftest-each compile-lambda-params
  "lambda forms support &rest and &optional parameters."
  :cases (("rest"              '(10 20 30) "(funcall (lambda (&rest args) args) 10 20 30)")
          ("optional-default"  5           "(funcall (lambda (a &optional (b 0)) (+ a b)) 5)")
          ("optional-provided" 15          "(funcall (lambda (a &optional (b 0)) (+ a b)) 5 10)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;; combined features
(deftest-each compile-rest-combined
  "&rest combined with &optional and closure capture."
  :cases (("with-optional" '(3 4 5)     " (defun opt-rest (a &optional (b 0) &rest r) r) (opt-rest 1 2 3 4 5)")
          ("closure"       '(10 20 30)  " (defun make-lister () (lambda (&rest args) args)) (funcall (make-lister) 10 20 30)"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; Variadic Arithmetic and List Tests

(deftest-each compile-variadic-arith
  "Variadic +, *, - and car of list produce correct numeric results."
  :cases (("plus-3"    6  "(+ 1 2 3)")
          ("plus-5"   15  "(+ 1 2 3 4 5)")
          ("plus-1"   10  "(+ 10)")
          ("plus-0"    0  "(+)")
          ("times-3"  24  "(* 2 3 4)")
          ("times-0"   1  "(*)")
          ("minus-3"   5  "(- 10 3 2)")
          ("list-car"  1  "(car (list 1 2 3))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-list-construction
  "list builds proper lists."
  :cases (("basic" '(1 2 3) "(list 1 2 3)")
          ("single" '(42)   "(list 42)"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; Stdlib / I/O / array/sort/coerce tests moved to compiler-tests-stdlib.lisp.
