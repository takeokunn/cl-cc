;;;; tests/unit/type/parser-tests.lisp — Type Parser Tests (primitive/compound/structural)
;;;;
;;;; Tests for src/type/parser.lisp:
;;;; parse-type-specifier, parse-primitive-type, parse-compound-type,
;;;; typed AST nodes, looks-like-type-specifier-p.
;;;; Arrow/quantifier/modal tests → parser-arrow-quantifier-tests.lisp.

(in-package :cl-cc/test)

(defsuite parser-suite :description "Type annotation parser tests"
  :parent cl-cc-unit-suite)


(in-suite parser-suite)
;;; ─── parse-type-specifier: atoms ─────────────────────────────────────────

(deftest-each parse-hole-type-specifiers
  "? and _ both parse to a type-error node (gradual hole)."
  :cases (("question-mark" '?)
          ("underscore"    '_))
  (spec)
  (assert-true (cl-cc/type:type-error-p (cl-cc/type:parse-type-specifier spec))))

(deftest parse-option-type
  "(option T) parses to (or null T)."
  (let ((ty (cl-cc/type:parse-type-specifier '(option string))))
    (assert-true (type-union-p ty))
    (assert-equal 2 (length (type-union-types ty)))
    (assert-true (some (lambda (x) (type-equal-p x type-null)) (type-union-types ty)))
    (assert-true (some (lambda (x) (type-equal-p x type-string)) (type-union-types ty)))))

(deftest looks-like-type-specifier-option
  "looks-like-type-specifier-p recognizes option sugar forms."
  (assert-true (cl-cc/type:looks-like-type-specifier-p '(option fixnum))))

(deftest-each parse-primitive-symbols
  "Primitive type symbols (and nil) parse to their expected type nodes."
  :cases (("nil"       nil        type-null)
          ("fixnum"    'fixnum    type-int)
          ("integer"   'integer   type-int)
          ("string"    'string    type-string)
          ("boolean"   'boolean   type-bool)
          ("bool"      'bool      type-bool)
          ("symbol"    'symbol    type-symbol)
          ("character" 'character type-char)
          ("char"      'char      type-char)
          ("t"         't         type-any)
          ("top"       'top       type-any)
          ("cons"      'cons      type-cons))
  (sym expected)
  (assert-true (type-equal-p expected (cl-cc/type:parse-type-specifier sym))))

(deftest parse-unknown-symbol
  "Unknown symbol becomes type-primitive with that name."
  (let ((ty (cl-cc/type:parse-type-specifier 'my-custom-type)))
    (assert-true (type-primitive-p ty))
    (assert-eq 'my-custom-type (type-primitive-name ty))))

;;; ─── parse-type-specifier: union / intersection ──────────────────────────

(deftest-each parse-set-type-ops
  "(or ...) and (and ...) produce union/intersection nodes; empty args signal an error."
  :cases (("or-two"                 '(or fixnum string)   #'type-union-p        #'type-union-types        nil)
          ("and-compatible"         '(and fixnum integer) #'type-intersection-p #'type-intersection-types nil)
          ("or-empty-error"         '(or)                 nil                   nil                       t)
          ("and-empty-error"        '(and)                nil                   nil                       t)
          ("and-uninhabited-error"  '(and fixnum string)  nil                   nil                       t))
  (form pred accessor error-p)
  (if error-p
      (assert-signals cl-cc/type:type-parse-error
        (cl-cc/type:parse-type-specifier form))
      (let ((ty (cl-cc/type:parse-type-specifier form)))
        (assert-true (funcall pred ty))
        (assert-equal 2 (length (funcall accessor ty))))))

;;; ─── parse-type-specifier: function / values ──────────────────────────────

(deftest-each parse-function-type-cases
  "(-> PARAMS... RET) produces arrow with correct param count and return type."
  :cases (("one-param"    '(-> fixnum string)          1 type-string)
          ("multi-param"  '(-> fixnum string boolean)  2 type-bool))
  (form expected-nparams expected-ret)
  (let ((ty (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-arrow-p ty))
    (assert-equal expected-nparams (length (type-arrow-params ty)))
    (assert-true (type-equal-p expected-ret (type-arrow-return ty)))))

(deftest-each parse-type-specifier-wrong-arity-errors
  "arrow and list forms with wrong arity signal type-parse-error."
  :cases (("arrow-no-param-list" '(-> fixnum))
          ("list-two-args"          '(list fixnum string)))
  (form)
  (assert-signals cl-cc/type:type-parse-error
    (cl-cc/type:parse-type-specifier form)))

(deftest parse-product-type-forms
  "(values T1 T2) produces a 2-element type-product."
  (let ((ty (cl-cc/type:parse-type-specifier '(values fixnum string))))
    (assert-true (type-product-p ty))
    (assert-equal 2 (length (type-product-elems ty)))))

;;; ─── parse-type-specifier: list / vector / array ─────────────────────────

(deftest-each parse-collection-type-apps
  "(list/vector/array/simple-vector/simple-array T) produce type-app nodes."
  :cases (("list"          '(list fixnum)          'list   type-int)
          ("vector"        '(vector fixnum)         'vector type-int)
          ("simple-vector" '(simple-vector fixnum)  'vector type-int)
          ("array"         '(array string)          'array  type-string)
          ("simple-array"  '(simple-array string)   'array  type-string))
  (form expected-fun-name expected-arg)
  (let ((ty (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-app-p ty))
    (assert-eq expected-fun-name (type-primitive-name (type-app-fun ty)))
    (assert-true (type-equal-p expected-arg (type-app-arg ty)))))

(deftest-each parse-cl-numeric-range-type-specifiers
  "CL numeric range type specifiers treat bounds as values, not nested type specs."
  :cases (("unsigned-byte" '(unsigned-byte 64))
          ("signed-byte"   '(signed-byte 32))
          ("integer-range" '(integer 0 *))
          ("mod"           '(mod 256)))
  (form)
  (assert-true (type-equal-p type-int (cl-cc/type:parse-type-specifier form))))

(deftest-each parse-collection-type-apps-with-dimensions
  "Vector/array type specifiers accept CL's optional size/dimension operand."
  :cases (("vector-size"  '(vector (unsigned-byte 8) 4)        'vector)
          ("array-dims"   '(array (unsigned-byte 8) (*))       'array)
          ("simple-array" '(simple-array (unsigned-byte 8) (*)) 'array))
  (form expected-fun-name)
  (let ((ty (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-app-p ty))
    (assert-eq expected-fun-name (type-primitive-name (type-app-fun ty)))
    (assert-true (type-equal-p type-int (type-app-arg ty)))))

(deftest parse-ansi-function-type-specifier
  "ANSI CL (function (A B ...) R) parses to the internal arrow type."
  (let ((ty (cl-cc/type:parse-type-specifier '(function (fixnum string) boolean))))
    (assert-true (type-arrow-p ty))
    (assert-equal 2 (length (type-arrow-params ty)))
    (assert-true (type-equal-p type-bool (type-arrow-return ty)))))

(deftest parse-compound-type-app-table-covers-five-aliases
  "*parse-compound-type-app-table* has exactly 5 entries: list + 2 vector forms + 2 array forms."
  (let ((table cl-cc/type::*parse-compound-type-app-table*))
    (assert-= 5 (length table))
    (assert-true (assoc 'list          table))
    (assert-true (assoc 'vector        table))
    (assert-true (assoc 'simple-vector table))
    (assert-true (assoc 'array         table))
    (assert-true (assoc 'simple-array  table))))

(deftest parse-compound-multi-arg-table-covers-or-and
  "*parse-compound-multi-arg-table* drives (or) and (and) dispatch."
  (let ((table cl-cc/type::*parse-compound-multi-arg-table*))
    (assert-= 2 (length table))
    (assert-true (assoc 'or  table))
    (assert-true (assoc 'and table))))

(deftest-each parser-graded-arrow-syntax
  "Graded arrow syntax (->1, ->0) parses to type-arrow with the correct multiplicity."
  :cases (("linear-1" '(->1 fixnum boolean) :one)
          ("erased-0" '(->0 fixnum boolean) :zero))
  (form expected-mult)
  (let ((result (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-arrow-p result))
    (assert-eq expected-mult (type-arrow-mult result))))

(deftest parser-forall-body-keyword
  "(forall a T) parses to type-forall with :body set."
  (let* ((result (cl-cc/type:parse-type-specifier '(forall a (-> a a)))))
    (assert-true (type-forall-p result))
    (assert-eq 'a (type-var-name (type-forall-var result)))
    (assert-true (type-arrow-p (type-forall-body result)))))

(deftest parser-bounded-forall-variable
  "(forall (a extends number supertype-of fixnum) T) records both bounded-polymorphism edges."
  (let* ((result (cl-cc/type:parse-type-specifier
                  '(forall (a extends number supertype-of fixnum) a)))
         (var (type-forall-var result)))
    (assert-true (type-forall-p result))
    (assert-eq 'a (type-var-name var))
    (assert-true (type-equal-p (make-type-primitive :name 'number)
                               (cl-cc/type:type-var-upper-bound var)))
    (assert-true (type-equal-p type-int (cl-cc/type:type-var-lower-bound var)))))

(deftest-each parser-quantified-types
  "exists and mu binders parse to their respective node types with correct var-name and body kind."
  :cases (("exists" '(exists a (values string a)) #'type-exists-p #'type-exists-var #'type-exists-body #'type-product-p)
          ("mu"     '(mu a (or null (values int a))) #'type-mu-p #'type-mu-var #'type-mu-body #'type-union-p))
  (form pred-p get-var get-body body-pred-p)
  (let ((result (cl-cc/type:parse-type-specifier form)))
    (assert-true (funcall pred-p result))
    (assert-eq 'a (type-var-name (funcall get-var result)))
    (assert-true (funcall body-pred-p (funcall get-body result)))))

(deftest-each parser-record
  "Record type syntax parses to type-record with the correct field count and row-var."
  :cases (("closed" '(record (name string) (age fixnum)) 2 nil)
          ("open"   `(record (name string) ,(intern "|" :cl-cc/type) rho) 1 t))
  (form n-fields open-p)
  (let ((result (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-record-p result))
    (assert-= n-fields (length (type-record-fields result)))
    (if open-p
        (assert-true  (type-record-row-var result))
        (assert-null  (type-record-row-var result)))))

(deftest parser-variant-syntax
  "(Variant (L T) ...) parses to a closed variant type."
  (let ((result (cl-cc/type:parse-type-specifier '(variant (some fixnum) (none null)))))
    (assert-true (type-variant-p result))
    (assert-= 2 (length (type-variant-cases result)))
    (assert-null (type-variant-row-var result))))

(deftest-each parser-linear-modal-syntax
  "(!1 T), (!ω T), and (!0 T) each parse to a graded modal type with the correct grade."
  :cases (("linear-1" '(!1 fixnum)   :one)
          ("omega"    '(!ω string)   :omega)
          ("erased-0" '(!0 boolean)  :zero))
  (form expected-grade)
  (let ((result (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-linear-p result))
    (assert-eq expected-grade (type-linear-grade result))))

(deftest-each parser-refinement-syntax
  "(refine T pred) parses to a refinement type; pred may be a lambda or a symbol."
  :cases (("lambda-pred" '(refine fixnum (lambda (x) (> x 0))) t)
          ("symbol-pred" '(refine fixnum positive-p)           'positive-p))
  (form expected-pred)
  (let ((result (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-refinement-p result))
    (assert-true (type-equal-p type-int (type-refinement-base result)))
    (if (eq expected-pred t)
        (assert-true (type-refinement-predicate result))
        (assert-eq expected-pred (cl-cc/type:type-refinement-predicate result)))))
