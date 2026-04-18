;;;; tests/unit/type/parser-tests.lisp — Type Parser Tests
;;;;
;;;; Tests for src/type/parser.lisp:
;;;; parse-type-specifier, parse-primitive-type, parse-compound-type,
;;;; arrow/row/constraint parsing, lambda-list parsing, return-type extraction,
;;;; typed AST nodes, looks-like-type-specifier-p.

(in-package :cl-cc/test)

(defsuite parser-suite :description "Type annotation parser tests"
  :parent cl-cc-unit-suite)


(in-suite parser-suite)
;;; ─── parse-type-specifier: atoms ─────────────────────────────────────────

(deftest parse-nil-to-null
  "nil parses to type-null."
  (assert-true (type-equal-p type-null (cl-cc/type::parse-type-specifier nil))))

(deftest-each parse-hole-type-specifiers
  "? and _ both parse to a type-error node (gradual hole)."
  :cases (("question-mark" '?)
          ("underscore"    '_))
  (spec)
  (assert-true (cl-cc/type::type-error-p (cl-cc/type::parse-type-specifier spec))))

(deftest parse-option-type
  "(option T) parses to (or null T)."
  (let ((ty (cl-cc/type::parse-type-specifier '(option string))))
    (assert-true (type-union-p ty))
    (assert-equal 2 (length (type-union-types ty)))
    (assert-true (some (lambda (x) (type-equal-p x type-null)) (type-union-types ty)))
    (assert-true (some (lambda (x) (type-equal-p x type-string)) (type-union-types ty)))))

(deftest looks-like-type-specifier-option
  "looks-like-type-specifier-p recognizes option sugar forms."
  (assert-true (cl-cc/type::looks-like-type-specifier-p '(option fixnum))))

(deftest-each parse-primitive-symbols
  "Primitive type symbols parse to their expected type nodes."
  :cases (("fixnum"    'fixnum    type-int)
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
  (assert-true (type-equal-p expected (cl-cc/type::parse-type-specifier sym))))

(deftest parse-unknown-symbol
  "Unknown symbol becomes type-primitive with that name."
  (let ((ty (cl-cc/type::parse-type-specifier 'my-custom-type)))
    (assert-true (type-primitive-p ty))
    (assert-eq 'my-custom-type (type-primitive-name ty))))

;;; ─── parse-type-specifier: union / intersection ──────────────────────────

(deftest-each parse-set-type-ops
  "(or ...) and (and ...) produce union/intersection nodes; empty args signal an error."
  :cases (("or-two"          '(or fixnum string)  #'type-union-p        #'type-union-types        nil)
          ("and-two"         '(and fixnum string) #'type-intersection-p #'type-intersection-types nil)
          ("or-empty-error"  '(or)                nil                   nil                       t)
          ("and-empty-error" '(and)               nil                   nil                       t))
  (form pred accessor error-p)
  (if error-p
      (assert-signals cl-cc/type::type-parse-error
        (cl-cc/type::parse-type-specifier form))
      (let ((ty (cl-cc/type::parse-type-specifier form)))
        (assert-true (funcall pred ty))
        (assert-equal 2 (length (funcall accessor ty))))))

;;; ─── parse-type-specifier: function / values / cons ──────────────────────

(deftest-each parse-function-type-cases
  "(function (PARAMS...) RET) produces arrow with correct param count and return type."
  :cases (("one-param"    '(function (fixnum) string)          1 type-string)
          ("multi-param"  '(function (fixnum string) boolean)  2 type-bool))
  (form expected-nparams expected-ret)
  (let ((ty (cl-cc/type::parse-type-specifier form)))
    (assert-true (type-arrow-p ty))
    (assert-equal expected-nparams (length (type-arrow-params ty)))
    (assert-true (type-equal-p expected-ret (type-arrow-return ty)))))

(deftest-each parse-type-specifier-wrong-arity-errors
  "function, cons, and list forms with wrong arity signal type-parse-error."
  :cases (("function-no-param-list" '(function fixnum))
          ("cons-one-arg"           '(cons fixnum))
          ("list-two-args"          '(list fixnum string)))
  (form)
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier form)))

(deftest-each parse-product-type-forms
  "(values T1 T2) and (cons T1 T2) both produce 2-element type-product."
  :cases (("values" '(values fixnum string))
          ("cons"   '(cons fixnum string)))
  (form)
  (let ((ty (cl-cc/type::parse-type-specifier form)))
    (assert-true (type-product-p ty))
    (assert-equal 2 (length (type-product-elems ty)))))

;;; ─── parse-type-specifier: list / vector / array ─────────────────────────

(deftest-each parse-collection-type-apps
  "(list T), (vector T), and (array T) all produce type-app nodes."
  :cases (("list"   '(list fixnum)   type-int  t)
          ("vector" '(vector fixnum) type-int  nil)
          ("array"  '(array string)  type-string nil))
  (form expected-arg check-arg-p)
  (let ((ty (cl-cc/type::parse-type-specifier form)))
    (assert-true (type-app-p ty))
    (when check-arg-p
      (assert-true (type-equal-p expected-arg (type-app-arg ty))))))

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

(deftest parser-refinement-syntax
  "(Refine T pred) parses to a refinement type."
  (let ((result (cl-cc/type:parse-type-specifier
                 '(refine fixnum (lambda (x) (> x 0))))))
    (assert-true (type-refinement-p result))
    (assert-true (type-equal-p type-int (type-refinement-base result)))
    (assert-true (type-refinement-predicate result))))

;;; ─── Arrow types: ->, ->1, ->0 ──────────────────────────────────────────

(deftest-each parse-arrow-basic-cases
  "-> arrow: pure 1-param; multi 2-param with boolean return."
  :cases (("pure"        `(,(intern "->" :cl-cc/type) fixnum string)          1 t)
          ("multi-param" `(,(intern "->" :cl-cc/type) fixnum string boolean)  2 nil))
  (form expected-param-count check-effects)
  (let ((ty (cl-cc/type::parse-type-specifier form)))
    (assert-true (type-arrow-p ty))
    (assert-equal expected-param-count (length (type-arrow-params ty)))
    (when check-effects
      (assert-true (type-effect-row-p (type-arrow-effects ty))))))

(deftest-each parse-arrow-multiplicity
  "->1 and ->0 produce arrow types with :one and :zero multiplicity respectively."
  :cases (("linear" (intern "->1" :cl-cc/type) :one)
          ("erased" (intern "->0" :cl-cc/type) :zero))
  (arrow-sym expected-mult)
  (let ((ty (cl-cc/type::parse-type-specifier `(,arrow-sym fixnum string))))
    (assert-true (type-arrow-p ty))
    (assert-eq expected-mult (cl-cc/type::type-arrow-mult ty))))

(deftest-each parse-type-specifier-malformed-errors
  "Malformed arrow/refinement/field specifiers signal type-parse-error."
  :cases (("arrow-too-few"       `(,(intern "->" :cl-cc/type) fixnum))
          ("refinement-no-pred"  '(refine fixnum))
          ("record-field-no-type" '(record (x))))
  (form)
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier form)))

(deftest parse-arrow-with-bang-effects
  "(-> fixnum string ! IO) has IO effect."
  (let ((ty (cl-cc/type::parse-type-specifier
             `(,(intern "->" :cl-cc/type) fixnum string ,(intern "!" :cl-cc/type) io))))
    (assert-true (type-arrow-p ty))
    (let ((eff (type-arrow-effects ty)))
      (assert-true (type-effect-row-p eff))
      (assert-true (> (length (type-effect-row-effects eff)) 0)))))

;;; ─── Quantifiers: forall, exists, mu ─────────────────────────────────────

(deftest-each parse-quantifier-binding-types
  "forall/exists/mu produce their respective type nodes with a type-var bound variable."
  :cases (("forall" '(forall a fixnum) #'type-forall-p #'type-forall-var
                    #'cl-cc/type::type-forall-body)
          ("exists" '(exists a fixnum) #'type-exists-p #'cl-cc/type::type-exists-var nil)
          ("mu"     '(mu a fixnum)     #'type-mu-p     #'cl-cc/type::type-mu-var     nil))
  (form pred var-fn body-fn)
  (let ((ty (cl-cc/type::parse-type-specifier form)))
    (assert-true (funcall pred ty))
    (assert-true (type-var-p (funcall var-fn ty)))
    (when body-fn
      (assert-true (type-equal-p type-int (funcall body-fn ty))))))

(deftest-each parse-quantifier-arity-errors
  "forall, exists, and mu with only one argument each signal type-parse-error."
  :cases (("forall" '(forall a))
          ("exists" '(exists a))
          ("mu"     '(mu a)))
  (form)
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier form)))

(deftest parse-type-lambda
  "(type-lambda a fixnum) produces type-lambda."
  (let ((ty (cl-cc/type::parse-type-specifier '(type-lambda a fixnum))))
    (assert-true (type-lambda-p ty))
    (assert-true (type-var-p (type-lambda-var ty)))
    (assert-true (type-equal-p type-int (type-lambda-body ty)))))

;;; ─── Qualified types: => ─────────────────────────────────────────────────

(deftest parse-qualified-type
  "(=> (Num fixnum) string) produces type-qualified."
  (let ((ty (cl-cc/type::parse-type-specifier
             `(,(intern "=>" :cl-cc/type) (num fixnum) string))))
    (assert-true (type-qualified-p ty))
    (assert-equal 1 (length (type-qualified-constraints ty)))
    (assert-true (type-equal-p type-string (cl-cc/type::type-qualified-body ty)))))

(deftest parse-qualified-error-no-body
  "(=>) signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier `(,(intern "=>" :cl-cc/type)))))

;;; ─── Refinement types ───────────────────────────────────────────────────

(deftest parse-refinement
  "(Refine fixnum positive-p) produces type-refinement."
  (let ((ty (cl-cc/type::parse-type-specifier '(refine fixnum positive-p))))
    (assert-true (cl-cc/type::type-refinement-p ty))
    (assert-true (type-equal-p type-int (cl-cc/type::type-refinement-base ty)))
    (assert-eq 'positive-p (cl-cc/type::type-refinement-predicate ty))))

;;; ─── Graded modal types ─────────────────────────────────────────────────

(deftest-each parse-graded-modal-types
  "!1, !0, and !W produce linear types with :one, :zero, :omega grades."
  :cases (("one"   (intern "!1" :cl-cc/type) :one)
          ("zero"  (intern "!0" :cl-cc/type) :zero)
          ("omega" (intern "!W" :cl-cc/type) :omega))
  (bang-sym expected-grade)
  (let ((ty (cl-cc/type::parse-type-specifier `(,bang-sym fixnum))))
    (assert-true (type-linear-p ty))
    (assert-eq expected-grade (cl-cc/type::type-linear-grade ty))))

(deftest parse-graded-bang-explicit
  "(! 1 fixnum) produces linear type with grade :one."
  (let ((ty (cl-cc/type::parse-type-specifier `(,(intern "!" :cl-cc/type) 1 fixnum))))
    (assert-true (type-linear-p ty))
    (assert-eq :one (cl-cc/type::type-linear-grade ty))))

