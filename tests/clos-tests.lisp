;;;; tests/clos-tests.lisp - CLOS Compilation Tests
;;;
;;; Comprehensive tests for CLOS support: defclass, defgeneric, defmethod,
;;; make-instance, slot-value, reader/writer/accessor methods, and
;;; generic function dispatch.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ----------------------------------------------------------------------------
;;; AST Parsing Tests
;;; ----------------------------------------------------------------------------

(test clos-parse-defclass
  "Test parsing a defclass form into AST."
  (let ((ast (lower-sexp-to-ast '(defclass point ()
                                    ((x :initarg :x :reader point-x)
                                     (y :initarg :y :reader point-y))))))
    (is (typep ast 'ast-defclass))
    (is (eq 'point (ast-defclass-name ast)))
    (is (null (ast-defclass-superclasses ast)))
    (is (= 2 (length (ast-defclass-slots ast))))
    (let ((x-slot (first (ast-defclass-slots ast))))
      (is (eq 'x (ast-slot-name x-slot)))
      (is (eq :x (ast-slot-initarg x-slot)))
      (is (eq 'point-x (ast-slot-reader x-slot))))))

(test clos-parse-defclass-with-superclass
  "Test parsing defclass with superclasses."
  (let ((ast (lower-sexp-to-ast '(defclass colored-point (point)
                                    ((color :initarg :color))))))
    (is (typep ast 'ast-defclass))
    (is (eq 'colored-point (ast-defclass-name ast)))
    (is (equal '(point) (ast-defclass-superclasses ast)))))

(test clos-parse-defgeneric
  "Test parsing a defgeneric form."
  (let ((ast (lower-sexp-to-ast '(defgeneric area (shape)))))
    (is (typep ast 'ast-defgeneric))
    (is (eq 'area (ast-defgeneric-name ast)))
    (is (equal '(shape) (ast-defgeneric-params ast)))))

(test clos-parse-defmethod
  "Test parsing a defmethod form."
  (let ((ast (lower-sexp-to-ast '(defmethod area ((s circle))
                                   (* 3 (slot-value s 'radius))))))
    (is (typep ast 'ast-defmethod))
    (is (eq 'area (ast-defmethod-name ast)))
    (is (equal '(s) (ast-defmethod-params ast)))
    (is (= 1 (length (ast-defmethod-body ast))))
    ;; Check specializer
    (let ((specs (ast-defmethod-specializers ast)))
      (is (= 1 (length specs)))
      (is (equal '(s . circle) (first specs))))))

(test clos-parse-make-instance
  "Test parsing a make-instance form."
  (let ((ast (lower-sexp-to-ast '(make-instance 'point :x 10 :y 20))))
    (is (typep ast 'ast-make-instance))
    (is (typep (ast-make-instance-class ast) 'ast-quote))
    (is (= 2 (length (ast-make-instance-initargs ast))))
    (is (eq :x (car (first (ast-make-instance-initargs ast)))))
    (is (eq :y (car (second (ast-make-instance-initargs ast)))))))

(test clos-parse-slot-value
  "Test parsing a slot-value form."
  (let ((ast (lower-sexp-to-ast '(slot-value obj 'x))))
    (is (typep ast 'ast-slot-value))
    (is (eq 'x (ast-slot-value-slot ast)))
    (is (typep (ast-slot-value-object ast) 'ast-var))))

;;; ----------------------------------------------------------------------------
;;; AST Roundtrip Tests
;;; ----------------------------------------------------------------------------

(test clos-defclass-roundtrip
  "Test defclass AST to sexp roundtrip."
  (let* ((sexp '(defclass point nil
                  ((x :initarg :x :reader point-x)
                   (y :initarg :y :reader point-y))))
         (ast (lower-sexp-to-ast sexp))
         (result (ast-to-sexp ast)))
    (is (eq 'defclass (first result)))
    (is (eq 'point (second result)))
    (is (null (third result)))
    (is (= 2 (length (fourth result))))))

(test clos-defgeneric-roundtrip
  "Test defgeneric AST to sexp roundtrip."
  (let* ((sexp '(defgeneric compute (obj)))
         (ast (lower-sexp-to-ast sexp))
         (result (ast-to-sexp ast)))
    (is (equal '(defgeneric compute (obj)) result))))

(test clos-slot-value-roundtrip
  "Test slot-value AST to sexp roundtrip."
  (let* ((ast (lower-sexp-to-ast '(slot-value obj 'x)))
         (result (ast-to-sexp ast)))
    (is (eq 'slot-value (first result)))
    ;; The slot name is quoted in roundtrip: (slot-value obj 'x)
    (is (equal '(quote x) (third result)))))

;;; ----------------------------------------------------------------------------
;;; Compilation and Execution Tests
;;; ----------------------------------------------------------------------------

(test clos-compile-defclass-slot-value
  "Test compiling defclass + make-instance + slot-value."
  (is (= 10 (run-string
             "(defclass point ()
                ((x :initarg :x)
                 (y :initarg :y)))
              (let ((p (make-instance 'point :x 10 :y 20)))
                (slot-value p 'x))"))))

(test clos-compile-slot-value-second-slot
  "Test accessing the second slot."
  (is (= 20 (run-string
             "(defclass point ()
                ((x :initarg :x)
                 (y :initarg :y)))
              (let ((p (make-instance 'point :x 10 :y 20)))
                (slot-value p 'y))"))))

(test clos-compile-multiple-slots-arithmetic
  "Test arithmetic on multiple slot values."
  (is (= 8 (run-string
            "(defclass rect ()
               ((w :initarg :w)
                (h :initarg :h)))
             (let ((r (make-instance 'rect :w 5 :h 3)))
               (+ (slot-value r 'w) (slot-value r 'h)))"))))

(test clos-compile-reader-accessor
  "Test reader accessor method compilation."
  (is (= 3 (run-string
            "(defclass vec ()
               ((dx :initarg :dx :reader vec-dx)
                (dy :initarg :dy :reader vec-dy)))
             (let ((v (make-instance 'vec :dx 3 :dy 4)))
               (vec-dx v))"))))

(test clos-compile-reader-second-field
  "Test reader accessor on the second field."
  (is (= 4 (run-string
            "(defclass vec ()
               ((dx :initarg :dx :reader vec-dx)
                (dy :initarg :dy :reader vec-dy)))
             (let ((v (make-instance 'vec :dx 3 :dy 4)))
               (vec-dy v))"))))

(test clos-compile-defgeneric-defmethod
  "Test basic generic function dispatch."
  (is (= 42 (run-string
             "(defclass animal ()
                ((name :initarg :name)))
              (defgeneric speak (a))
              (defmethod speak ((a animal))
                42)
              (let ((a (make-instance 'animal :name 'dog)))
                (speak a))"))))

(test clos-compile-method-with-slot-access
  "Test method that accesses slots of its argument."
  (is (= 20 (run-string
             "(defclass pair ()
                ((a :initarg :a)
                 (b :initarg :b)))
              (defgeneric pair-sum (p))
              (defmethod pair-sum ((p pair))
                (+ (slot-value p 'a) (slot-value p 'b)))
              (let ((p (make-instance 'pair :a 7 :b 13)))
                (pair-sum p))"))))

(test clos-compile-method-with-arithmetic
  "Test method that computes a formula on slot values."
  (is (= 15 (run-string
             "(defclass rect ()
                ((w :initarg :w)
                 (h :initarg :h)))
              (defgeneric area (shape))
              (defmethod area ((r rect))
                (* (slot-value r 'w) (slot-value r 'h)))
              (let ((r (make-instance 'rect :w 3 :h 5)))
                (area r))"))))

(test clos-compile-multiple-instances
  "Test creating multiple instances of the same class."
  (is (= 30 (run-string
             "(defclass counter ()
                ((val :initarg :val)))
              (let ((c1 (make-instance 'counter :val 10))
                    (c2 (make-instance 'counter :val 20)))
                (+ (slot-value c1 'val) (slot-value c2 'val)))"))))

(test clos-compile-slot-default-nil
  "Test that uninitialized slots default to nil."
  (is (null (run-string
            "(defclass box ()
               ((content :initarg :content)))
             (let ((b (make-instance 'box)))
               (slot-value b 'content))"))))

(test clos-compile-nested-slot-access
  "Test conditional based on slot value."
  (is (= 1 (run-string
            "(defclass flag ()
               ((active :initarg :active)))
             (let ((f (make-instance 'flag :active 1)))
               (if (slot-value f 'active) 1 0))"))))

(test clos-compile-defclass-three-slots
  "Test class with three slots."
  (is (= 60 (run-string
             "(defclass color ()
                ((r :initarg :r)
                 (g :initarg :g)
                 (b :initarg :b)))
              (let ((c (make-instance 'color :r 10 :g 20 :b 30)))
                (+ (slot-value c 'r)
                   (+ (slot-value c 'g)
                      (slot-value c 'b))))"))))

;;; ----------------------------------------------------------------------------
;;; Slot Specification Parsing Tests
;;; ----------------------------------------------------------------------------

(test clos-parse-slot-spec-symbol
  "Test parsing a bare symbol slot spec."
  (let ((slot (parse-slot-spec 'x)))
    (is (typep slot 'ast-slot-def))
    (is (eq 'x (ast-slot-name slot)))
    (is (null (ast-slot-initarg slot)))
    (is (null (ast-slot-reader slot)))))

(test clos-parse-slot-spec-full
  "Test parsing a full slot spec with all options."
  (let ((slot (parse-slot-spec '(x :initarg :x :reader get-x :writer set-x :accessor x-accessor))))
    (is (eq 'x (ast-slot-name slot)))
    (is (eq :x (ast-slot-initarg slot)))
    (is (eq 'get-x (ast-slot-reader slot)))
    (is (eq 'set-x (ast-slot-writer slot)))
    (is (eq 'x-accessor (ast-slot-accessor slot)))))

(test clos-slot-def-to-sexp
  "Test slot-def to sexp conversion."
  (let* ((slot (parse-slot-spec '(x :initarg :x :reader get-x)))
         (sexp (slot-def-to-sexp slot)))
    (is (eq 'x (first sexp)))
    (is (member :initarg sexp))
    (is (member :reader sexp))))

;;; ----------------------------------------------------------------------------
;;; CLOS Inheritance Tests
;;; ----------------------------------------------------------------------------

(test clos-inherit-slot-from-superclass
  "Subclass inherits slots from its superclass."
  (is (= 10 (run-string
             "(defclass base ()
                ((x :initarg :x)))
              (defclass child (base)
                ((y :initarg :y)))
              (let ((c (make-instance 'child :x 10 :y 20)))
                (slot-value c 'x))"))))

(test clos-inherit-own-slot-accessible
  "Subclass's own slots are accessible alongside inherited ones."
  (is (= 20 (run-string
             "(defclass base ()
                ((x :initarg :x)))
              (defclass child (base)
                ((y :initarg :y)))
              (let ((c (make-instance 'child :x 10 :y 20)))
                (slot-value c 'y))"))))

(test clos-inherit-slot-arithmetic
  "Arithmetic on inherited + own slots."
  (is (= 30 (run-string
             "(defclass base ()
                ((x :initarg :x)))
              (defclass child (base)
                ((y :initarg :y)))
              (let ((c (make-instance 'child :x 10 :y 20)))
                (+ (slot-value c 'x) (slot-value c 'y)))"))))

(test clos-inherit-method-from-superclass
  "Method defined on superclass is callable on subclass instance."
  (is (= 4 (run-string
            "(defclass animal ()
               ((legs :initarg :legs)))
             (defgeneric leg-count (a))
             (defmethod leg-count ((a animal))
               (slot-value a 'legs))
             (defclass dog (animal)
               ((breed :initarg :breed)))
             (let ((d (make-instance 'dog :legs 4 :breed 'lab)))
               (leg-count d))"))))

(test clos-inherit-method-override
  "Subclass method overrides superclass method."
  (is (= 99 (run-string
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
                (info c))"))))

(test clos-inherit-superclass-method-still-works
  "Superclass instances still use their own method after subclass override."
  (is (= 7 (run-string
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
               (info s))"))))

(test clos-inherit-two-levels
  "Three-level inheritance chain: grandchild inherits grandparent slots."
  (is (= 100 (run-string
              "(defclass a ()
                 ((x :initarg :x)))
               (defclass b (a)
                 ((y :initarg :y)))
               (defclass c (b)
                 ((z :initarg :z)))
               (let ((obj (make-instance 'c :x 100 :y 200 :z 300)))
                 (slot-value obj 'x))"))))

(test clos-inherit-method-two-levels
  "Method defined on grandparent dispatches on grandchild instance."
  (is (= 600 (run-string
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
                 (get-x obj))"))))

(test clos-inherit-initargs-from-super
  "Subclass inherits initarg mappings from superclass."
  (is (= 5 (run-string
            "(defclass base ()
               ((val :initarg :val)))
             (defclass ext (base)
               ((extra :initarg :extra)))
             (let ((e (make-instance 'ext :val 5 :extra 10)))
               (slot-value e 'val))"))))

;;; ----------------------------------------------------------------------------
;;; Setf Slot-Value Tests
;;; ----------------------------------------------------------------------------

(test clos-setf-slot-value-basic
  "Basic setf slot-value sets the slot."
  (is (= 99 (run-string
             "(defclass box ()
                ((content :initarg :content)))
              (let ((b (make-instance 'box :content 0)))
                (setf (slot-value b 'content) 99)
                (slot-value b 'content))"))))

(test clos-setf-slot-value-returns-new-value
  "setf slot-value returns the new value."
  (is (= 42 (run-string
             "(defclass box ()
                ((content :initarg :content)))
              (let ((b (make-instance 'box :content 0)))
                (setf (slot-value b 'content) 42))"))))

(test clos-setf-slot-value-multiple-mutations
  "Multiple setf slot-value mutations."
  (is (= 30 (run-string
             "(defclass counter ()
                ((val :initarg :val)))
              (let ((c (make-instance 'counter :val 10)))
                (setf (slot-value c 'val) 20)
                (setf (slot-value c 'val) 30)
                (slot-value c 'val))"))))

(test clos-setf-slot-value-computed
  "setf slot-value with computed value."
  (is (= 15 (run-string
             "(defclass pair ()
                ((a :initarg :a)
                 (b :initarg :b)))
              (let ((p (make-instance 'pair :a 5 :b 10)))
                (setf (slot-value p 'a)
                      (+ (slot-value p 'a) (slot-value p 'b)))
                (slot-value p 'a))"))))

(test clos-setf-slot-value-roundtrip
  "setf slot-value AST roundtrip."
  (let* ((sexp '(setf (slot-value obj (quote x)) 42))
         (ast (lower-sexp-to-ast sexp))
         (result (ast-to-sexp ast)))
    (is (eq 'setf (first result)))
    (is (eq 'slot-value (first (second result))))
    (is (equal '(quote x) (third (second result))))))
