;;;; tests/unit/type/parser-arrow-quantifier-tests.lisp — Arrow & Quantifier Parser Tests
;;;;
;;;; Tests for src/type/parser.lisp: arrow multiplicity (->1/->0), bang effects,
;;;; forall/exists/mu quantifiers, type-lambda, qualified types (=>), and graded modal (!1/!0/!W).
;;;; Suite: parser-suite (defined in parser-tests.lisp).

(in-package :cl-cc/test)

(in-suite parser-suite)

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

(deftest-each parse-qualified-type-cases
  "(=>) with body produces type-qualified; without body signals type-parse-error."
  :cases (("valid"    `(,(intern "=>" :cl-cc/type) (num fixnum) string) nil)
          ("no-body"  `(,(intern "=>" :cl-cc/type))                     t))
  (form error-p)
  (if error-p
      (assert-signals cl-cc/type::type-parse-error
        (cl-cc/type::parse-type-specifier form))
      (let ((ty (cl-cc/type::parse-type-specifier form)))
        (assert-true (type-qualified-p ty))
        (assert-equal 1 (length (type-qualified-constraints ty)))
        (assert-true (type-equal-p type-string (cl-cc/type::type-qualified-body ty))))))

;;; ─── Graded modal types ─────────────────────────────────────────────────

(deftest-each parse-graded-modal-types
  "!1, !0, !W shorthand and explicit (! N T) all produce linear types with correct grades."
  :cases (("one"      `(,(intern "!1" :cl-cc/type) fixnum)    :one)
          ("zero"     `(,(intern "!0" :cl-cc/type) fixnum)    :zero)
          ("omega"    `(,(intern "!W" :cl-cc/type) fixnum)    :omega)
          ("explicit" `(,(intern "!"  :cl-cc/type) 1 fixnum)  :one))
  (form expected-grade)
  (let ((ty (cl-cc/type::parse-type-specifier form)))
    (assert-true (type-linear-p ty))
    (assert-eq expected-grade (cl-cc/type::type-linear-grade ty))))
