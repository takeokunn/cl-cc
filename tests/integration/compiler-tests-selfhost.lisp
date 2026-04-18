(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(deftest self-host-clos-full-pipeline
  "Full self-hosting: CLOS AST -> register compiler -> hash-table VM with let/if"
  (assert-= 100 (run-string *self-host-clos-full-pipeline-program* :stdlib t)))

;;; Generic Function as First-Class Value Tests

(deftest-each generic-function-numeric
  "Generic functions work with funcall/apply/let and return correct numeric values."
  :cases (("funcall" 11 "(progn (defgeneric my-fn (x)) (defmethod my-fn ((x t)) (+ x 1)) (funcall #'my-fn 10))")
          ("apply"   42 "(progn (defgeneric add1 (x)) (defmethod add1 ((x t)) (+ x 1)) (apply #'add1 (list 41)))")
          ("in-let"   5 "(progn (defgeneric double (x)) (defmethod double ((x t)) (* x 2)) (let ((f #'double)) (funcall f 2) (+ (funcall f 2) 1)))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest funcall-generic-function-clos
  "funcall with #'generic-function should dispatch on CLOS class"
  (assert-string= "dog-speak" (run-string "(progn (defclass animal () ()) (defclass dog (animal) ()) (defgeneric speak (x)) (defmethod speak ((x dog)) \"dog-speak\") (defmethod speak ((x t)) \"default\") (funcall #'speak (make-instance 'dog)))")))

(deftest mapcar-generic-function
  "mapcar with #'generic-function via stdlib"
  (assert-equal '(2 3 4) (run-string "(progn (defgeneric inc (x)) (defmethod inc ((x t)) (+ x 1)) (mapcar #'inc (list 1 2 3)))" :stdlib t)))

(deftest mapcar-generic-function-reader
  "mapcar with #'reader-method on CLOS instances"
  (assert-true (string= "(a b c)"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(progn (defclass item () ((name :initarg :name :reader item-name))) (let ((items (list (make-instance 'item :name 'a) (make-instance 'item :name 'b) (make-instance 'item :name 'c)))) (mapcar #'item-name items)))" :stdlib t)))))))

(deftest self-host-mapcar-inst-sexp
  "Self-hosting pattern: mapcar #'generic-function over instruction list"
  (assert-true (string= "((:const r0 42) (:const r1 7) (:add r2 r0 r1))"
    (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
      (string-downcase (format nil "~S"
                               (run-string *self-host-mapcar-inst-sexp-program*
                                           :stdlib t)))))))

;;; Run Tests Function

;;; Global Variable (defvar) Persistence Tests

(deftest-each defvar-persistence
  "defvar combined with setq and functions persists state correctly."
  :cases (("counter"  3
           "(progn (defvar *counter* 0) (defun inc-counter () (setq *counter* (+ *counter* 1)) *counter*) (inc-counter) (inc-counter) (inc-counter))")
          ("sequence" '(0 1 2)
           "(progn (defvar *n* 0) (defun next-n () (let ((val *n*)) (setq *n* (+ *n* 1)) val)) (list (next-n) (next-n) (next-n)))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest defvar-label-generation
  "defvar counter for label generation pattern"
  (assert-equal '("L_0" "L_1" "L_2") (run-string "(progn (defvar *lbl* 0) (defun make-label (prefix) (let ((n *lbl*)) (setq *lbl* (+ n 1)) (concatenate 'string prefix \"_\" (write-to-string n)))) (list (make-label \"L\") (make-label \"L\") (make-label \"L\")))" :stdlib t)))

;;; Defmacro in progn Tests

(deftest-each defmacro-in-progn
  "defmacro works within progn, making the macro available for subsequent forms."
  :cases (("simple"     10 "(progn (defmacro my-dbl (x) (list '+ x x)) (my-dbl 5))")
          ("rest"       42 "(progn (defmacro my-when (test &rest body) (list 'if test (cons 'progn body) nil)) (my-when (= 1 1) 42))")
          ("used-twice" 12 "(progn (defmacro my-add1 (x) (list '+ x 1)) (+ (my-add1 5) (my-add1 5)))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Self-Hosting Compiler Pattern Tests

(deftest self-host-compiler-context-full
  "Self-hosting: full compiler context with make-register, make-label, emit"
  (let ((result (run-string *self-host-compiler-context-program* :stdlib t)))
    (assert-true (equal '((:CONST :R0 42) (:CONST :R1 7) (:ADD :R2 :R0 :R1)) result))))

(deftest self-host-ast-compile-dispatch
  "Self-hosting: CLOS compile-ast dispatch compiles (+ (* 3 4) 5)"
  (let ((result (run-string *self-host-ast-compile-dispatch-program* :stdlib t)))
    (assert-equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1) (:CONST :R3 5) (:ADD :R4 :R2 :R3)) result)))

(deftest self-host-macro-expander
  "Self-hosting: macro expansion system with hash table registry"
  (let ((result (run-string *self-host-simple-macro-expander-program* :stdlib t)))
    (assert-true (string= "(if x (progn (+ 1 2)) nil)"
                         (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" result)))))))

;;; Multiple-Value-List Tests

(deftest-each multiple-value-list
  "multiple-value-list captures the full values list from various forms."
  :cases (("floor"  '(3 2)   "(multiple-value-list (floor 17 5))")
          ("values" '(1 2 3) "(multiple-value-list (values 1 2 3))")
          ("single" '(42)    "(multiple-value-list (values 42))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Apply with Spread Arguments Tests

(deftest-each apply-spread-args-numeric
  "apply with spread arguments computes the correct numeric result."
  :cases (("plus-spread"  10 "(apply #'+ 1 2 (list 3 4))")
          ("minus-list"    5 "(apply #'- (list 10 3 2))")
          ("multiply-list" 24 "(apply #'* (list 2 3 4))")
          ("plus-five"    15 "(apply #'+ (list 1 2 3 4 5))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest apply-spread-args-append
  "apply #'append with list of lists"
  (assert-true (equal '(1 2 3 4) (run-string "(apply #'append (list (list 1 2) (list 3 4)))"))))


;;; Typed Defun/Lambda Tests

(deftest-each typed-defun-runtime
  "Typed defun compiles and executes to the correct result."
  :cases (("basic-add"      7             "(progn (defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y)) (typed-add 3 4))")
          ("no-return-type" 12            "(progn (defun typed-mul ((x fixnum) (y fixnum)) (* x y)) (typed-mul 3 4))")
          ("mixed-params"   7             "(progn (defun typed-mixed ((x fixnum) y) (+ x y)) (typed-mixed 3 4))")
          ("string-return"  "Hello World" "(progn (defun typed-greet ((name string)) string (concatenate 'string \"Hello \" name)) (typed-greet \"World\"))"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest-each typed-lambda
  "Typed lambda with parameter type annotations compiles correctly."
  :cases (("with-return" 30 "(funcall (lambda ((x fixnum) (y fixnum)) fixnum (+ x y)) 10 20)")
          ("no-return"    6 "(funcall (lambda ((a fixnum) (b fixnum)) (* a b)) 2 3)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest typed-defun-type-registry
  "typed defun registers function type for type checking"
  (let ((old-count (hash-table-count cl-cc/compile::*function-type-registry*)))
    (run-string "(defun typed-reg-test ((x fixnum)) fixnum x)")
    (assert-true (> (hash-table-count cl-cc/compile::*function-type-registry*) old-count))))

(deftest typed-multi-form-top-level
  "Type checking applies to multi-form top-level compilation as well."
  (multiple-value-bind (result type)
      (run-string-typed "(defvar *typed-top-level* 1)
                         42")
    (assert-= 42 result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "FIXNUM" (symbol-name (cl-cc/type:type-primitive-name type)))))

;;; CLOS Type Inference Tests

(deftest clos-type-inference-make-instance
  "make-instance infers the class name as the result type."
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
        (make-instance 'point :x 1 :y 2))")
    (declare (ignore result))
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "POINT" (symbol-name (cl-cc/type:type-primitive-name type)))))

(deftest clos-type-inference-slot-fixnum
  "slot-value infers fixnum type from defclass slot :type fixnum."
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
        (let ((p (make-instance 'point :x 10 :y 20)))
          (slot-value p 'x)))")
    (assert-= 10 result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "FIXNUM" (symbol-name (cl-cc/type:type-primitive-name type)))))

(deftest clos-type-inference-slot-string
  "slot-value infers string type from defclass slot :type string."
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass person () ((name :initarg :name :type string)))
        (let ((p (make-instance 'person :name \"Alice\")))
          (slot-value p 'name)))")
    (assert-string= "Alice" result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "STRING" (symbol-name (cl-cc/type:type-primitive-name type)))))

;;; Type Alias (deftype) Tests

(deftest-each deftype-numeric
  "deftype aliases work in typed defun and defclass slot :type."
  :cases (("basic"   42 "(progn (deftype my-int fixnum) (defun typed-id ((x my-int)) my-int x) (typed-id 42))")
          ("in-slot" 10 "(progn (deftype coordinate fixnum) (defclass point2 () ((x :initarg :x :type coordinate))) (let ((p (make-instance 'point2 :x 10))) (slot-value p 'x)))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest deftype-union
  "deftype with union type expands in type registry"
  (let ((old-count (hash-table-count cl-cc/type:*type-alias-registry*)))
    (run-string "(deftype int-or-str (or fixnum string))")
    (assert-true (> (hash-table-count cl-cc/type:*type-alias-registry*) old-count))))

;;; Type Narrowing Tests

(deftest type-narrowing-numberp
  "numberp narrows x to fixnum in the then-branch."
  (multiple-value-bind (result type)
      (run-string-typed "(let ((x 42)) (if (numberp x) (+ x 1) 0))")
    (assert-= 43 result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "FIXNUM" (symbol-name (cl-cc/type:type-primitive-name type)))))

(deftest type-narrowing-stringp
  "stringp narrows x to string in the then-branch."
  (multiple-value-bind (result type)
      (run-string-typed "(let ((x \"hello\")) (if (stringp x) x \"default\"))")
    (assert-string= "hello" result)
    (assert-type cl-cc/type:type-primitive type)))

;;; Higher-Order Function Macro Expansions (Self-Hosting)

(deftest-each hof-list-result
  "Higher-order functions that return lists or boolean-like values."
  :cases (("mapcar-basic"       '(2 4 6)       "(mapcar (lambda (x) (* x 2)) (list 1 2 3))")
          ("mapcar-empty"       nil            "(mapcar (lambda (x) x) nil)")
          ("mapc-original"      '(1 2 3)       "(mapc (lambda (x) (+ x 1)) (list 1 2 3))")
          ("mapcan-flatten"     '(1 1 2 2 3 3) "(mapcan (lambda (x) (list x x)) (list 1 2 3))")
          ("every-true"         t              "(every (lambda (x) (> x 0)) (list 1 2 3))")
          ("every-false"        nil            "(every (lambda (x) (> x 2)) (list 1 2 3))")
          ("every-empty"        t              "(every (lambda (x) x) nil)")
          ("some-not-found"     nil            "(some (lambda (x) (if (> x 10) x nil)) (list 1 2 3))")
          ("find-not-found"     nil            "(find 99 (list 1 2 3))")
          ("position-not-found" nil            "(position 99 (list 1 2 3))")
          ("remove-if"          '(1 3 5)       "(remove-if (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5))")
          ("remove-if-not"      '(2 4)         "(remove-if-not (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5))")
          ("remove-basic"       '(1 3 5)       "(remove 2 (list 1 2 3 2 5))")
          ("remove-duplicates"  '(1 2 3)       "(remove-duplicates (list 1 2 3 2 1))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest-each hof-numeric-result
  "Higher-order functions that return numeric values."
  :cases (("some-found"   3 "(some (lambda (x) (if (> x 2) x nil)) (list 1 2 3 4))")
          ("find-basic"   3 "(find 3 (list 1 2 3 4 5))")
          ("find-if"      4 "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
          ("position"     2 "(position 3 (list 1 2 3 4 5))")
          ("count"        3 "(count 2 (list 1 2 2 3 2))")
          ("count-if"     2 "(count-if (lambda (x) (> x 3)) (list 1 2 3 4 5))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Parametric Types (type-constructor)

(deftest parametric-type-parse-list
  "Parsing (list fixnum) yields a type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(list fixnum))))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq 'list (cl-cc/type:type-constructor-name ty))
    (assert-true (= 1 (length (cl-cc/type:type-constructor-args ty))))
    (assert-true (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-int))))

(deftest parametric-type-parse-option
  "Parsing (Option string) yields a type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(Option string))))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq 'Option (cl-cc/type:type-constructor-name ty))
    (assert-true (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-string))))

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

(deftest parametric-type-unparse
  "Unparsing a type-constructor roundtrips correctly"
  (let* ((ty (cl-cc/type:parse-type-specifier '(Pair fixnum string)))
         (spec (cl-cc/type:unparse-type ty)))
    (assert-equal 'Pair (first spec))
    (assert-= 3 (length spec))))

(deftest parametric-type-to-string
  "type-to-string works for type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(list fixnum))))
    (assert-string= "(LIST FIXNUM)" (cl-cc/type:type-to-string ty))))

(deftest parametric-type-equal-p
  "type-equal-p works for type-constructors"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t3 (cl-cc/type:parse-type-specifier '(list string))))
    (assert-true (cl-cc/type:type-equal-p t1 t2))
    (assert-false (cl-cc/type:type-equal-p t1 t3))))

(deftest parametric-type-free-vars
  "Free vars are extracted from type-constructor args"
  (let* ((tv (cl-cc/type:make-type-variable 'x))
         (ty (cl-cc/type:make-type-constructor 'list (list tv))))
    (assert-true (= 1 (length (cl-cc/type:type-free-vars ty))))))

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
