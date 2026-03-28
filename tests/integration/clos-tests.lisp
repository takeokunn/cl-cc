;;;; tests/clos-tests.lisp - CLOS Compilation Tests
;;;
;;; Comprehensive tests for CLOS support: defclass, defgeneric, defmethod,
;;; make-instance, slot-value, reader/writer/accessor methods, and
;;; generic function dispatch.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; AST Parsing Tests

(deftest clos-parse-clos-forms
  "AST parsing for defclass, defclass-with-superclass, defgeneric, defmethod, make-instance, and slot-value forms."
  (let ((ast (lower-sexp-to-ast '(defclass point ()
                                    ((x :initarg :x :reader point-x)
                                     (y :initarg :y :reader point-y))))))
    (assert-type ast-defclass ast)
    (assert-eq 'point (ast-defclass-name ast))
    (assert-null (ast-defclass-superclasses ast))
    (assert-= 2 (length (ast-defclass-slots ast)))
    (let ((x-slot (first (ast-defclass-slots ast))))
      (assert-eq 'x (ast-slot-name x-slot))
      (assert-eq :x (ast-slot-initarg x-slot))
      (assert-eq 'point-x (ast-slot-reader x-slot))))
  (let ((ast (lower-sexp-to-ast '(defclass colored-point (point)
                                    ((color :initarg :color))))))
    (assert-type ast-defclass ast)
    (assert-eq 'colored-point (ast-defclass-name ast))
    (assert-equal '(point) (ast-defclass-superclasses ast)))
  (let ((ast (lower-sexp-to-ast '(defgeneric area (shape)))))
    (assert-type ast-defgeneric ast)
    (assert-eq 'area (ast-defgeneric-name ast))
    (assert-equal '(shape) (ast-defgeneric-params ast)))
  (let ((ast (lower-sexp-to-ast '(defmethod area ((s circle))
                                   (* 3 (slot-value s 'radius))))))
    (assert-type ast-defmethod ast)
    (assert-eq 'area (ast-defmethod-name ast))
    (assert-equal '(s) (ast-defmethod-params ast))
    (assert-= 1 (length (ast-defmethod-body ast)))
    ;; Check specializer
    (let ((specs (ast-defmethod-specializers ast)))
      (assert-= 1 (length specs))
      (assert-equal '(s . circle) (first specs))))
  (let ((ast (lower-sexp-to-ast '(make-instance 'point :x 10 :y 20))))
    (assert-type ast-make-instance ast)
    (assert-type ast-quote (ast-make-instance-class ast))
    (assert-= 2 (length (ast-make-instance-initargs ast)))
    (assert-eq :x (car (first (ast-make-instance-initargs ast))))
    (assert-eq :y (car (second (ast-make-instance-initargs ast)))))
  (let ((ast (lower-sexp-to-ast '(slot-value obj 'x))))
    (assert-type ast-slot-value ast)
    (assert-eq 'x (ast-slot-value-slot ast))
    (assert-type ast-var (ast-slot-value-object ast))))

;;; defgeneric options tests

(deftest clos-defgeneric-with-documentation
  "defgeneric with :documentation option is accepted and ignored."
  (let ((ast (lower-sexp-to-ast '(defgeneric area (shape)
                                   (:documentation "Compute area")))))
    (assert-type ast-defgeneric ast)
    (assert-eq 'area (ast-defgeneric-name ast))
    (assert-equal '(shape) (ast-defgeneric-params ast))))

(deftest clos-defgeneric-inline-method
  "defgeneric with (:method ...) expands to progn of defgeneric + defmethod."
  (let ((ast (lower-sexp-to-ast '(defgeneric area (shape)
                                   (:method ((s circle))
                                     (* 3 (slot-value s 'radius)))))))
    ;; Should be a progn wrapping defgeneric + defmethod
    (assert-type ast-progn ast)
    (let ((forms (ast-progn-forms ast)))
      (assert-= 2 (length forms))
      (assert-type ast-defgeneric (first forms))
      (assert-type ast-defmethod (second forms))
      (assert-eq 'area (ast-defmethod-name (second forms))))))

(deftest clos-defgeneric-multiple-inline-methods
  "defgeneric with multiple (:method ...) forms."
  (let ((ast (lower-sexp-to-ast '(defgeneric describe-it (x)
                                   (:documentation "Describe an object")
                                   (:method ((x integer)) (format nil "int:~A" x))
                                   (:method ((x string)) (format nil "str:~A" x))))))
    (assert-type ast-progn ast)
    (let ((forms (ast-progn-forms ast)))
      (assert-= 3 (length forms))
      (assert-type ast-defgeneric (first forms))
      (assert-type ast-defmethod (second forms))
      (assert-type ast-defmethod (third forms)))))

(deftest clos-defgeneric-options-ignored
  "defgeneric accepts :argument-precedence-order and :generic-function-class without error."
  (let ((ast (lower-sexp-to-ast '(defgeneric combine (a b)
                                   (:argument-precedence-order b a)
                                   (:generic-function-class standard-generic-function)))))
    (assert-type ast-defgeneric ast)
    (assert-eq 'combine (ast-defgeneric-name ast))))

(deftest clos-defgeneric-inline-method-compile
  "defgeneric with inline method compiles and runs correctly."
  (let ((result (run-string "
    (defgeneric greet (who)
      (:method ((who string))
        (concatenate 'string \"Hello \" who)))
    (greet \"World\")")))
    (assert-equal "Hello World" result)))

;;; AST Roundtrip Tests

(deftest clos-roundtrip-forms
  "CLOS AST-to-sexp roundtrip for defclass, defgeneric, and slot-value forms."
  (let* ((sexp '(defclass point nil
                  ((x :initarg :x :reader point-x)
                   (y :initarg :y :reader point-y))))
         (ast (lower-sexp-to-ast sexp))
         (result (ast-to-sexp ast)))
    (assert-eq 'defclass (first result))
    (assert-eq 'point (second result))
    (assert-null (third result))
    (assert-= 2 (length (fourth result))))
  (let* ((sexp '(defgeneric compute (obj)))
         (ast (lower-sexp-to-ast sexp))
         (result (ast-to-sexp ast)))
    (assert-equal '(defgeneric compute (obj)) result))
  (let* ((ast (lower-sexp-to-ast '(slot-value obj 'x)))
         (result (ast-to-sexp ast)))
    (assert-eq 'slot-value (first result))
    ;; The slot name is quoted in roundtrip: (slot-value obj 'x)
    (assert-equal '(quote x) (third result))))

;;; Compilation and Execution Tests

(deftest clos-compile-slot-access
  "Slot access (first, second, arithmetic) compiles and evaluates correctly."
  (assert-= 10 (run-string
             "(defclass point ()
                ((x :initarg :x)
                 (y :initarg :y)))
              (let ((p (make-instance 'point :x 10 :y 20)))
                (slot-value p 'x))"))
  (assert-= 20 (run-string
             "(defclass point ()
                ((x :initarg :x)
                 (y :initarg :y)))
              (let ((p (make-instance 'point :x 10 :y 20)))
                (slot-value p 'y))"))
  (assert-= 8 (run-string
            "(defclass rect ()
               ((w :initarg :w)
                (h :initarg :h)))
             (let ((r (make-instance 'rect :w 5 :h 3)))
               (+ (slot-value r 'w) (slot-value r 'h)))")))

(deftest-each clos-compile-reader-methods
  "Reader accessor methods work on first and second class slots."
  :cases (("first-field"  3 "(defclass vec ()
               ((dx :initarg :dx :reader vec-dx)
                (dy :initarg :dy :reader vec-dy)))
             (let ((v (make-instance 'vec :dx 3 :dy 4)))
               (vec-dx v))")
          ("second-field" 4 "(defclass vec ()
               ((dx :initarg :dx :reader vec-dx)
                (dy :initarg :dy :reader vec-dy)))
             (let ((v (make-instance 'vec :dx 3 :dy 4)))
               (vec-dy v))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest clos-compile-generic-methods
  "Generic function dispatch, slot-accessing methods, and formula methods compile correctly."
  (assert-= 42 (run-string
             "(defclass animal ()
                ((name :initarg :name)))
              (defgeneric speak (a))
              (defmethod speak ((a animal))
                42)
              (let ((a (make-instance 'animal :name 'dog)))
                (speak a))"))
  (assert-= 20 (run-string
             "(defclass pair ()
                ((a :initarg :a)
                 (b :initarg :b)))
              (defgeneric pair-sum (p))
              (defmethod pair-sum ((p pair))
                (+ (slot-value p 'a) (slot-value p 'b)))
              (let ((p (make-instance 'pair :a 7 :b 13)))
                (pair-sum p))"))
  (assert-= 15 (run-string
             "(defclass rect ()
                ((w :initarg :w)
                 (h :initarg :h)))
              (defgeneric area (shape))
              (defmethod area ((r rect))
                (* (slot-value r 'w) (slot-value r 'h)))
              (let ((r (make-instance 'rect :w 3 :h 5)))
                (area r))")))

(deftest clos-compile-instance-variations
  "Multiple instances, uninitialized slots, conditional slot access, and three-slot classes compile correctly."
  (assert-= 30 (run-string
             "(defclass counter ()
                ((val :initarg :val)))
              (let ((c1 (make-instance 'counter :val 10))
                    (c2 (make-instance 'counter :val 20)))
                (+ (slot-value c1 'val) (slot-value c2 'val)))"))
  (assert-null (run-string
            "(defclass box ()
               ((content :initarg :content)))
             (let ((b (make-instance 'box)))
               (slot-value b 'content))"))
  (assert-= 1 (run-string
            "(defclass flag ()
               ((active :initarg :active)))
             (let ((f (make-instance 'flag :active 1)))
               (if (slot-value f 'active) 1 0))"))
  (assert-= 60 (run-string
             "(defclass color ()
                ((r :initarg :r)
                 (g :initarg :g)
                 (b :initarg :b)))
              (let ((c (make-instance 'color :r 10 :g 20 :b 30)))
                (+ (slot-value c 'r)
                   (+ (slot-value c 'g)
                      (slot-value c 'b))))")))

;;; Slot Specification Parsing Tests

(deftest clos-parse-slot-specs
  "Slot specification parsing: bare symbol, full spec with all options, and sexp conversion."
  (let ((slot (parse-slot-spec 'x)))
    (assert-type ast-slot-def slot)
    (assert-eq 'x (ast-slot-name slot))
    (assert-null (ast-slot-initarg slot))
    (assert-null (ast-slot-reader slot)))
  (let ((slot (parse-slot-spec '(x :initarg :x :reader get-x :writer set-x :accessor x-accessor))))
    (assert-eq 'x (ast-slot-name slot))
    (assert-eq :x (ast-slot-initarg slot))
    (assert-eq 'get-x (ast-slot-reader slot))
    (assert-eq 'set-x (ast-slot-writer slot))
    (assert-eq 'x-accessor (ast-slot-accessor slot)))
  (let* ((slot (parse-slot-spec '(x :initarg :x :reader get-x)))
         (sexp (slot-def-to-sexp slot)))
    (assert-eq 'x (first sexp))
    (assert-true (member :initarg sexp))
    (assert-true (member :reader sexp))))

;;; CLOS Inheritance Tests

(deftest clos-inherit-slot-from-superclass
  "Subclass inherits slots from its superclass."
  (assert-= 10 (run-string
             "(defclass base ()
                ((x :initarg :x)))
              (defclass child (base)
                ((y :initarg :y)))
              (let ((c (make-instance 'child :x 10 :y 20)))
                (slot-value c 'x))")))

(deftest clos-inherit-own-slot-accessible
  "Subclass's own slots are accessible alongside inherited ones."
  (assert-= 20 (run-string
             "(defclass base ()
                ((x :initarg :x)))
              (defclass child (base)
                ((y :initarg :y)))
              (let ((c (make-instance 'child :x 10 :y 20)))
                (slot-value c 'y))")))

(deftest clos-inherit-slot-arithmetic
  "Arithmetic on inherited + own slots."
  (assert-= 30 (run-string
             "(defclass base ()
                ((x :initarg :x)))
              (defclass child (base)
                ((y :initarg :y)))
              (let ((c (make-instance 'child :x 10 :y 20)))
                (+ (slot-value c 'x) (slot-value c 'y)))")))

(deftest clos-inherit-method-from-superclass
  "Method defined on superclass is callable on subclass instance."
  (assert-= 4 (run-string
            "(defclass animal ()
               ((legs :initarg :legs)))
             (defgeneric leg-count (a))
             (defmethod leg-count ((a animal))
               (slot-value a 'legs))
             (defclass dog (animal)
               ((breed :initarg :breed)))
             (let ((d (make-instance 'dog :legs 4 :breed 'lab)))
               (leg-count d))")))

(deftest clos-inherit-method-override
  "Subclass method overrides superclass method."
  (assert-= 99 (run-string
             "(defclass shape ()
                ((n :initarg :n)))
              (defgeneric info (s))
              (defmethod info ((s shape))
                (slot-value s 'n))
              (defclass circle (shape)
                ((r :initarg :r)))
              (defmethod info ((s circle))
                99)
              (let ((c (make-instance 'circle :n 1 :r 5)))
                (info c))")))

(deftest clos-inherit-superclass-method-still-works
  "Superclass instances still use their own method after subclass override."
  (assert-= 7 (run-string
            "(defclass shape ()
               ((n :initarg :n)))
             (defgeneric info (s))
             (defmethod info ((s shape))
               (slot-value s 'n))
             (defclass circle (shape)
               ((r :initarg :r)))
             (defmethod info ((s circle))
               99)
             (let ((s (make-instance 'shape :n 7)))
               (info s))")))

(deftest clos-inherit-two-levels
  "Three-level inheritance chain: grandchild inherits grandparent slots."
  (assert-= 100 (run-string
              "(defclass a ()
                 ((x :initarg :x)))
               (defclass b (a)
                 ((y :initarg :y)))
               (defclass c (b)
                 ((z :initarg :z)))
               (let ((obj (make-instance 'c :x 100 :y 200 :z 300)))
                 (slot-value obj 'x))")))

(deftest clos-inherit-method-two-levels
  "Method defined on grandparent dispatches on grandchild instance."
  (assert-= 600 (run-string
              "(defclass a ()
                 ((x :initarg :x)))
               (defgeneric get-x (obj))
               (defmethod get-x ((obj a))
                 (slot-value obj 'x))
               (defclass b (a)
                 ((y :initarg :y)))
               (defclass c (b)
                 ((z :initarg :z)))
               (let ((obj (make-instance 'c :x 600 :y 0 :z 0)))
                 (get-x obj))")))

(deftest clos-inherit-initargs-from-super
  "Subclass inherits initarg mappings from superclass."
  (assert-= 5 (run-string
            "(defclass base ()
               ((val :initarg :val)))
             (defclass ext (base)
               ((extra :initarg :extra)))
             (let ((e (make-instance 'ext :val 5 :extra 10)))
               (slot-value e 'val))")))

;;; Setf Slot-Value Tests

(deftest clos-setf-slot-value-basic
  "Basic setf slot-value sets the slot."
  (assert-= 99 (run-string
             "(defclass box ()
                ((content :initarg :content)))
              (let ((b (make-instance 'box :content 0)))
                (setf (slot-value b 'content) 99)
                (slot-value b 'content))")))

(deftest clos-setf-slot-value-returns-new-value
  "setf slot-value returns the new value."
  (assert-= 42 (run-string
             "(defclass box ()
                ((content :initarg :content)))
              (let ((b (make-instance 'box :content 0)))
                (setf (slot-value b 'content) 42))")))

(deftest clos-setf-slot-value-multiple-mutations
  "Multiple setf slot-value mutations."
  (assert-= 30 (run-string
             "(defclass counter ()
                ((val :initarg :val)))
              (let ((c (make-instance 'counter :val 10)))
                (setf (slot-value c 'val) 20)
                (setf (slot-value c 'val) 30)
                (slot-value c 'val))")))

(deftest clos-setf-slot-value-computed
  "setf slot-value with computed value."
  (assert-= 15 (run-string
             "(defclass pair ()
                ((a :initarg :a)
                 (b :initarg :b)))
              (let ((p (make-instance 'pair :a 5 :b 10)))
                (setf (slot-value p 'a)
                      (+ (slot-value p 'a) (slot-value p 'b)))
                (slot-value p 'a))")))

(deftest clos-setf-slot-value-roundtrip
  "setf slot-value AST roundtrip."
  (let* ((sexp '(setf (slot-value obj (quote x)) 42))
         (ast (lower-sexp-to-ast sexp))
         (result (ast-to-sexp ast)))
    (assert-eq 'setf (first result))
    (assert-eq 'slot-value (first (second result)))
    (assert-equal '(quote x) (third (second result)))))

;;; ── :default-initargs ──────────────────────────────────────────────────────

(deftest clos-parse-default-initargs
  "defclass with :default-initargs parses into AST."
  (let ((ast (lower-sexp-to-ast '(defclass point ()
                                    ((x :initarg :x :initform 0))
                                    (:default-initargs :x 42)))))
    (assert-type ast-defclass ast)
    (let ((di (cl-cc::ast-defclass-default-initargs ast)))
      (assert-= 1 (length di))
      (assert-eq :x (car (first di))))))

(deftest clos-default-initargs-runtime
  ":default-initargs provides default values for initargs at make-instance time."
  ;; Default applies when no explicit initarg provided
  (assert-= 42 (run-string "(progn (defclass da-point () ((x :initarg :x :initform 0)) (:default-initargs :x 42)) (slot-value (make-instance 'da-point) 'x))"))
  ;; Explicit initarg overrides default
  (assert-= 99 (run-string "(progn (defclass da-point2 () ((x :initarg :x :initform 0)) (:default-initargs :x 42)) (slot-value (make-instance 'da-point2 :x 99) 'x))")))

;;; ── :allocation :class Tests ────────────────────────────────────────────────

(deftest clos-allocation-class-shared
  ":allocation :class slots are shared across all instances."
  (assert-= 42 (run-string
             "(defclass counter ()
                ((count :initarg :count :allocation :class)))
              (let ((c1 (make-instance 'counter :count 42))
                    (c2 (make-instance 'counter)))
                (slot-value c2 'count))"))
  ;; Writing to class slot via one instance affects all
  (assert-= 99 (run-string
             "(defclass shared-box ()
                ((val :initarg :val :allocation :class)))
              (let ((a (make-instance 'shared-box :val 10))
                    (b (make-instance 'shared-box)))
                (setf (slot-value a 'val) 99)
                (slot-value b 'val))")))

(deftest clos-allocation-class-with-instance
  "Mixed :allocation :class and :instance slots."
  (assert-= 100 (run-string
              "(defclass mixed ()
                 ((shared :initarg :shared :allocation :class)
                  (own    :initarg :own)))
               (let ((a (make-instance 'mixed :shared 100 :own 1))
                     (b (make-instance 'mixed :own 2)))
                 (slot-value b 'shared))"))
  ;; Instance slots are independent
  (assert-= 1 (run-string
            "(defclass mixed2 ()
               ((shared :initarg :shared :allocation :class)
                (own    :initarg :own)))
             (let ((a (make-instance 'mixed2 :shared 100 :own 1))
                   (b (make-instance 'mixed2 :own 2)))
               (slot-value a 'own))")))

;;; ── EQL Specializer Tests ──────────────────────────────────────────────────

(deftest clos-eql-specializer-basic
  "EQL specializer dispatches on specific values."
  (assert-= 1 (run-string
            "(defgeneric coin-value (c))
             (defmethod coin-value ((c (eql :penny))) 1)
             (defmethod coin-value ((c (eql :nickel))) 5)
             (coin-value :penny)"))
  (assert-= 5 (run-string
            "(defgeneric coin-val2 (c))
             (defmethod coin-val2 ((c (eql :penny))) 1)
             (defmethod coin-val2 ((c (eql :nickel))) 5)
             (coin-val2 :nickel)")))

(deftest clos-eql-specializer-with-fallback
  "EQL specializer with class-based fallback."
  (assert-= 42 (run-string
             "(defgeneric describe-it (x))
              (defmethod describe-it ((x (eql 42))) 42)
              (defmethod describe-it ((x integer)) 0)
              (describe-it 42)"))
  (assert-= 0 (run-string
            "(defgeneric describe-it2 (x))
             (defmethod describe-it2 ((x (eql 42))) 42)
             (defmethod describe-it2 ((x integer)) 0)
             (describe-it2 99)")))

(deftest clos-eql-specializer-symbol
  "EQL specializer on symbols."
  (assert-= 100 (run-string
              "(defgeneric sym-val (s))
               (defmethod sym-val ((s (eql 'foo))) 100)
               (defmethod sym-val ((s symbol)) 0)
               (sym-val 'foo)"))
  (assert-= 0 (run-string
            "(defgeneric sym-val2 (s))
             (defmethod sym-val2 ((s (eql 'foo))) 100)
             (defmethod sym-val2 ((s symbol)) 0)
             (sym-val2 'bar)")))

;;; ── Method Qualifier Tests (:before/:after/:around) ──────────────────────

(deftest clos-defmethod-before-qualifier
  "defmethod :before runs before the primary method."
  ;; Use defvar for shared state between methods
  (assert-equal "before:primary"
    (run-string
     "(defvar *bq-log* \"\")
      (defgeneric greet-q (x))
      (defmethod greet-q ((x integer))
        (setf *bq-log* (concatenate 'string *bq-log* \"primary\"))
        *bq-log*)
      (defmethod greet-q :before ((x integer))
        (setf *bq-log* (concatenate 'string *bq-log* \"before:\")))
      (greet-q 1)")))

(deftest clos-defmethod-after-qualifier
  "defmethod :after runs after the primary method; primary value is returned."
  (assert-= 42
    (run-string
     "(defgeneric aft-test (x))
      (defmethod aft-test ((x integer))
        42)
      (defmethod aft-test :after ((x integer))
        99)
      (aft-test 1)")))

(deftest clos-defmethod-before-and-after
  "defmethod :before and :after both execute in correct order."
  (assert-equal "B:P:A"
    (run-string
     "(defvar *ba-log* \"\")
      (defgeneric ba-test (x))
      (defmethod ba-test ((x integer))
        (setf *ba-log* (concatenate 'string *ba-log* \"P\"))
        *ba-log*)
      (defmethod ba-test :before ((x integer))
        (setf *ba-log* (concatenate 'string *ba-log* \"B:\")))
      (defmethod ba-test :after ((x integer))
        (setf *ba-log* (concatenate 'string *ba-log* \":A\")))
      (ba-test 1)
      *ba-log*")))

(deftest clos-defmethod-qualifier-parse
  "defmethod with :before qualifier parses correctly."
  (let ((ast (lower-sexp-to-ast '(defmethod foo :before ((x integer)) (print x)))))
    (assert-type ast-defmethod ast)
    (assert-eq 'foo (ast-defmethod-name ast))
    (assert-eq :before (cl-cc::ast-defmethod-qualifier ast))))

(deftest clos-defmethod-around-qualifier
  "defmethod :around wraps the primary method; around's return is the final result."
  (assert-equal "WRAPPED:42"
    (run-string
     "(defgeneric around-test (x))
      (defmethod around-test ((x integer))
        42)
      (defmethod around-test :around ((x integer))
        (let ((result (call-next-method)))
          (concatenate 'string \"WRAPPED:\" (write-to-string result))))
      (around-test 1)")))

(deftest clos-defmethod-around-without-cnm
  "defmethod :around without call-next-method returns around's value."
  (assert-equal "AROUND-ONLY"
    (run-string
     "(defgeneric around-only-test (x))
      (defmethod around-only-test ((x integer))
        \"PRIMARY\")
      (defmethod around-only-test :around ((x integer))
        \"AROUND-ONLY\")
      (around-only-test 1)")))

(deftest clos-defmethod-around-with-before-after
  "defmethod :around + :before + :after: around wraps everything."
  (assert-equal "B:P:A"
    (run-string
     "(defvar *aba-log* \"\")
      (defgeneric aba-test (x))
      (defmethod aba-test ((x integer))
        (setf *aba-log* (concatenate 'string *aba-log* \"P\"))
        *aba-log*)
      (defmethod aba-test :before ((x integer))
        (setf *aba-log* (concatenate 'string *aba-log* \"B:\")))
      (defmethod aba-test :after ((x integer))
        (setf *aba-log* (concatenate 'string *aba-log* \":A\")))
      (defmethod aba-test :around ((x integer))
        (call-next-method)
        *aba-log*)
      (aba-test 1)")))

(deftest clos-defmethod-around-qualifier-parse
  "defmethod with :around qualifier parses correctly."
  (let ((ast (lower-sexp-to-ast '(defmethod foo :around ((x integer)) (print x)))))
    (assert-type ast-defmethod ast)
    (assert-eq :around (cl-cc::ast-defmethod-qualifier ast))))
