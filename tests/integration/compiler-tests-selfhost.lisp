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
  :timeout 180
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
  :timeout 180
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
  "Multi-form top-level programs still evaluate correctly under the typed entrypoint."
  (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (result type)
        (run-string-typed "(defvar *typed-top-level* 1)
                           42")
      (declare (ignore type))
      (assert-= 42 result))))

;;; CLOS Type Inference Tests

(deftest-each clos-type-inference-slot-types
  "run-string-typed infers the correct type-primitive name for make-instance and slot-value."
  :cases (("make-instance"
           "(progn (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
              (make-instance 'point :x 1 :y 2))"
           (lambda (r) (declare (ignore r)))
           "POINT")
          ("slot-fixnum"
           "(progn (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
              (let ((p (make-instance 'point :x 10 :y 20))) (slot-value p 'x)))"
           (lambda (r) (assert-= 10 r))
           "FIXNUM")
          ("slot-string"
           "(progn (defclass person () ((name :initarg :name :type string)))
              (let ((p (make-instance 'person :name \"Alice\"))) (slot-value p 'name)))"
           (lambda (r) (assert-string= "Alice" r))
           "STRING"))
  (form check-result expected-type-name)
  (multiple-value-bind (result type) (run-string-typed form)
    (funcall check-result result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= expected-type-name (symbol-name (cl-cc/type:type-primitive-name type)))))

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

(deftest-each type-narrowing-predicate
  "Predicate-based narrowing produces the expected type-primitive in the then-branch."
  :cases (("numberp-to-fixnum"
           "(let ((x 42)) (if (numberp x) (+ x 1) 0))"
           (lambda (r) (assert-= 43 r))
           "FIXNUM")
          ("stringp-to-string"
           "(let ((x \"hello\")) (if (stringp x) x \"default\"))"
           (lambda (r) (assert-string= "hello" r))
           nil))
  (form check-result expected-type-name)
  (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (result type) (run-string-typed form)
      (funcall check-result result)
      (assert-type cl-cc/type:type-primitive type)
      (when expected-type-name
        (assert-string= expected-type-name (symbol-name (cl-cc/type:type-primitive-name type)))))))

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

;;; Parametric types, defparameter, equal, numeric, warn, format tests → compiler-tests-selfhost-types.lisp.
