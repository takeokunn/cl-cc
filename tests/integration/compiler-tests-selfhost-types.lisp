;;;; compiler-tests-selfhost-types.lisp — Parametric types, defparameter, equal, numeric, warn, format tests
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; Parametric Types (type-constructor)

(deftest-each parametric-type-parse-single-arg
  "parse-type-specifier produces a type-constructor with one arg of the expected type."
  :cases (("list-fixnum"   '(list fixnum)   'list   cl-cc/type:type-int)
          ("option-string" '(Option string)  'Option cl-cc/type:type-string))
  (spec expected-name expected-arg-type)
  (let ((ty (cl-cc/type:parse-type-specifier spec)))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq expected-name (cl-cc/type:type-constructor-name ty))
    (assert-= 1 (length (cl-cc/type:type-constructor-args ty)))
    (assert-true (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                          expected-arg-type))))

(deftest parametric-type-parse-pair
  "Parsing (Pair fixnum string) yields a type-constructor with 2 args"
  (let ((ty (cl-cc/type:parse-type-specifier '(Pair fixnum string))))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq 'Pair (cl-cc/type:type-constructor-name ty))
    (assert-true (= 2 (length (cl-cc/type:type-constructor-args ty))))
    (assert-true (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-int))
    (assert-true (cl-cc/type:type-equal-p (second (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-string))))

(deftest parametric-type-unify-same
  "Unifying (List fixnum) with (List fixnum) succeeds with no new bindings"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(list fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (assert-true ok)
      ;; No bindings needed — subst may be empty struct or nil
      (assert-true (or (null subst)
                       (zerop (hash-table-count
                                (cl-cc/type:substitution-bindings subst))))))))

(deftest parametric-type-unify-with-var
  "Unifying (List ?a) with (List fixnum) binds ?a to fixnum"
  (let* ((tv (cl-cc/type:make-type-variable 'a))
         (t1 (cl-cc/type:make-type-constructor 'list (list tv)))
         (t2 (cl-cc/type:parse-type-specifier '(list fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (assert-true ok)
      (assert-false (null subst))
      (let ((resolved (cl-cc/type:type-substitute tv subst)))
        (assert-true (cl-cc/type:type-equal-p resolved cl-cc/type:type-int))))))

(deftest parametric-type-unify-different-constructors
  "Unifying (List fixnum) with (Option fixnum) fails"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(Option fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (declare (ignore subst))
      (assert-false ok))))

(deftest-each parametric-type-utilities
  "Type-constructor utility operations: unparse, to-string, equal-p, free-vars."
  :cases (("unparse-roundtrip"
           (lambda ()
             (let* ((ty   (cl-cc/type:parse-type-specifier '(Pair fixnum string)))
                    (spec (cl-cc/type:unparse-type ty)))
               (assert-equal 'Pair (first spec))
               (assert-=     3     (length spec)))))
          ("to-string"
           (lambda ()
             (let ((ty (cl-cc/type:parse-type-specifier '(list fixnum))))
               (assert-string= "(LIST FIXNUM)" (cl-cc/type:type-to-string ty)))))
          ("equal-p"
           (lambda ()
             (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
                   (t2 (cl-cc/type:parse-type-specifier '(list fixnum)))
                   (t3 (cl-cc/type:parse-type-specifier '(list string))))
               (assert-true  (cl-cc/type:type-equal-p t1 t2))
               (assert-false (cl-cc/type:type-equal-p t1 t3)))))
          ("free-vars"
           (lambda ()
             (let* ((tv (cl-cc/type:make-type-variable 'x))
                    (ty (cl-cc/type:make-type-constructor 'list (list tv))))
               (assert-= 1 (length (cl-cc/type:type-free-vars ty)))))))
  (check)
  (funcall check))

(deftest parametric-type-nested
  "Nested parametric types: (List (Option fixnum))"
  (let ((ty (cl-cc/type:parse-type-specifier '(list (Option fixnum)))))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq 'list (cl-cc/type:type-constructor-name ty))
    (let ((inner (first (cl-cc/type:type-constructor-args ty))))
      (assert-type cl-cc/type:type-constructor inner)
      (assert-eq 'Option (cl-cc/type:type-constructor-name inner)))))

(deftest parametric-type-in-typed-defun
  "Typed defun with parametric return type compiles"
  (let ((result (run-string "(progn
    (deftype int-list (list fixnum))
    (defun make-nums () (list 1 2 3))
    (length (make-nums)))")))
    (assert-= 3 result)))

;;; Defparameter Tests

(deftest-each defparameter-persistence
  "defparameter defines and persists dynamic variables across various usage patterns."
  :cases (("basic"         42 "(progn (defparameter *val* 42) *val*)")
          ("with-function" 10 "(progn (defparameter *base* 10) (defun get-base () *base*) (get-base))")
          ("setq-mutation"  5 "(progn (defparameter *x* 0) (setq *x* 5) *x*)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; String= and Equal Tests

(deftest-each compile-equal-truthy
  "equal and string= return truthy for matching values."
  :cases (("string=-match"  "(string= \"hello\" \"hello\")")
          ("equal-numbers"  "(equal 42 42)")
          ("equal-strings"  "(equal \"abc\" \"abc\")")
          ("equal-lists"    "(equal '(1 2 3) (list 1 2 3))"))
  (form)
  (assert-true (run-string form)))

(deftest-each compile-equal-false
  "equal and string= return NIL (falsy) for non-matching values."
  :cases (("string=-diff" "(string= \"hello\" \"world\")")
          ("equal-diff"   "(equal 1 2)"))
  (form)
  (assert-true (null (run-string form))))

;;; Numeric Builtins Tests (max, min, mod, zerop, plusp, minusp)

(deftest-each numeric-arithmetic
  "max/min/mod/abs return the expected numeric value."
  :cases (("max"       5 "(max 3 5)")
          ("min"       3 "(min 3 5)")
          ("mod-basic" 1 "(mod 7 3)")
          ("mod-even"  0 "(mod 6 3)")
          ("abs"       5 "(abs -5)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each numeric-predicates-truthy
  "Numeric predicates return truthy for matching values."
  :cases (("zerop-zero"  "(zerop 0)")
          ("plusp-pos"   "(plusp 5)")
          ("minusp-neg"  "(minusp -3)")
          ("evenp-even"  "(evenp 4)")
          ("oddp-odd"    "(oddp 3)"))
  (form)
  (assert-true (run-string form)))

(deftest-each numeric-predicates-false
  "Numeric predicates return NIL for non-matching values."
  :cases (("zerop-nonzero" "(zerop 5)")
          ("plusp-neg"     "(plusp -3)")
          ("minusp-pos"    "(minusp 5)"))
  (form)
  (assert-false (run-string form)))

;;; Warn Compilation Tests

(deftest compile-warn
  "warn compiles without crashing and execution continues past it."
  (assert-true (null (run-string "(warn \"test warning\")")))
  (assert-= 42 (run-string "(progn (warn \"warning\") 42)")))

;;; Format Compilation Tests

(deftest-each compile-format-nil
  "format nil with various directives returns the correctly formatted string."
  :cases (("simple" "hello"       "(format nil \"~A\" \"hello\")")
          ("number" "42"          "(format nil \"~D\" 42)")
          ("concat" "hello world" "(format nil \"~A ~A\" \"hello\" \"world\")"))
  (expected form)
  (assert-string= expected (run-string form)))
