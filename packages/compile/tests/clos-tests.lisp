;;;; tests/clos-tests.lisp - CLOS Compilation Tests
;;;
;;; Comprehensive tests for CLOS support: defclass, defgeneric, defmethod,
;;; make-instance, slot-value, reader/writer/accessor methods, and
;;; generic function dispatch.

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; AST Parsing Tests

(deftest clos-parse-defclass
  "lower-sexp-to-ast parses defclass: name, nil superclasses, and slot metadata."
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
      (assert-eq 'point-x (ast-slot-reader x-slot)))))

(deftest clos-parse-defclass-with-superclass
  "lower-sexp-to-ast parses defclass: superclass list is preserved."
  (let ((ast (lower-sexp-to-ast '(defclass colored-point (point)
                                    ((color :initarg :color))))))
    (assert-type ast-defclass ast)
    (assert-eq 'colored-point (ast-defclass-name ast))
    (assert-equal '(point) (ast-defclass-superclasses ast))))

(deftest clos-parse-defgeneric
  "lower-sexp-to-ast parses defgeneric: name and parameter list."
  (let ((ast (lower-sexp-to-ast '(defgeneric area (shape)))))
    (assert-type ast-defgeneric ast)
    (assert-eq 'area (ast-defgeneric-name ast))
    (assert-equal '(shape) (ast-defgeneric-params ast))))

(deftest clos-parse-defmethod
  "lower-sexp-to-ast parses defmethod: name, params, body, and specializers."
  (let ((ast (lower-sexp-to-ast '(defmethod area ((s circle))
                                   (* 3 (slot-value s 'radius))))))
    (assert-type ast-defmethod ast)
    (assert-eq 'area (ast-defmethod-name ast))
    (assert-equal '(s) (ast-defmethod-params ast))
    (assert-= 1 (length (ast-defmethod-body ast)))
    (let ((specs (ast-defmethod-specializers ast)))
      (assert-= 1 (length specs))
      (assert-equal '(s . circle) (first specs)))))

(deftest clos-parse-make-instance
  "lower-sexp-to-ast parses make-instance: class and initarg pairs."
  (let ((ast (lower-sexp-to-ast '(make-instance 'point :x 10 :y 20))))
    (assert-type ast-make-instance ast)
    (assert-type ast-quote (ast-make-instance-class ast))
    (assert-= 2 (length (ast-make-instance-initargs ast)))
    (assert-eq :x (car (first (ast-make-instance-initargs ast))))
    (assert-eq :y (car (second (ast-make-instance-initargs ast))))))

(deftest clos-parse-slot-value
  "lower-sexp-to-ast parses slot-value: object and slot name."
  (let ((ast (lower-sexp-to-ast '(slot-value obj 'x))))
    (assert-type ast-slot-value ast)
    (assert-eq 'x (ast-slot-value-slot ast))
    (assert-type ast-var (ast-slot-value-object ast))))

;;; defgeneric options tests

(deftest-each clos-defgeneric-with-options
  "defgeneric with :documentation, :argument-precedence-order, and :generic-function-class all parse to ast-defgeneric."
  :cases (("documentation"    'area    '(shape)
           '(defgeneric area (shape) (:documentation "Compute area")))
          ("precedence+class" 'combine nil
           '(defgeneric combine (a b)
              (:argument-precedence-order b a)
              (:generic-function-class standard-generic-function))))
  (expected-name expected-params form)
  (let ((ast (lower-sexp-to-ast form)))
    (assert-type ast-defgeneric ast)
    (assert-eq expected-name (ast-defgeneric-name ast))
    (when expected-params
      (assert-equal expected-params (ast-defgeneric-params ast)))))

(deftest-each clos-defgeneric-inline-method-parse
  "defgeneric with (:method ...) expands to ast-progn of defgeneric + defmethod(s)."
  :cases (("single-method"
           '(defgeneric area (shape)
              (:method ((s circle)) (* 3 (slot-value s 'radius))))
           2
           'area)
          ("multiple-methods"
           '(defgeneric describe-it (x)
              (:documentation "Describe an object")
              (:method ((x integer)) (format nil "int:~A" x))
              (:method ((x string)) (format nil "str:~A" x)))
           3
           'describe-it))
  (form expected-form-count expected-method-name)
  (let ((ast (lower-sexp-to-ast form)))
    (assert-type ast-progn ast)
    (let ((forms (ast-progn-forms ast)))
      (assert-= expected-form-count (length forms))
      (assert-type ast-defgeneric (first forms))
      (dolist (f (cdr forms))
        (assert-type ast-defmethod f))
      (assert-eq expected-method-name (ast-defmethod-name (second forms))))))

(deftest clos-defgeneric-inline-method-compile
  "defgeneric with inline method compiles and runs correctly."
  (let ((result (run-string "
    (defgeneric greet (who)
      (:method ((who string))
        (concatenate 'string \"Hello \" who)))
    (greet \"World\")")))
    (assert-equal "Hello World" result)))

;;; AST Roundtrip Tests

(deftest-each clos-ast-roundtrip-forms
  "ast-to-sexp/lower-sexp-to-ast roundtrip preserves structure for core CLOS forms."
  :cases (("defclass"
           '(defclass point nil
              ((x :initarg :x :reader point-x)
               (y :initarg :y :reader point-y)))
           (lambda (result)
             (assert-eq   'defclass (first result))
             (assert-eq   'point    (second result))
             (assert-null           (third result))
             (assert-=    2         (length (fourth result)))))
          ("defgeneric"
           '(defgeneric compute (obj))
           (lambda (result)
             (assert-equal '(defgeneric compute (obj)) result)))
          ("slot-value"
           '(slot-value obj 'x)
           (lambda (result)
             (assert-eq    'slot-value (first result))
             (assert-equal '(quote x)  (third result))))
          ("setf-slot-value"
           '(setf (slot-value obj (quote x)) 42)
           (lambda (result)
             (assert-eq   'setf       (first result))
             (assert-eq   'slot-value (first (second result)))
             (assert-equal '(quote x)  (third (second result))))))
  (form check)
  (funcall check (ast-to-sexp (lower-sexp-to-ast form))))

;;; Compilation and Execution Tests
