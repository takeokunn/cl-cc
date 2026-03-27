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
