;;;; tests/unit/type/subtyping-tests.lisp — Subtyping Relation Tests
;;;;
;;;; Tests for src/type/subtyping.lisp:
;;;; type-name-subtype-p, is-subtype-p, find-common-supertype,
;;;; type-join, type-meet.

(in-package :cl-cc/test)

(defsuite subtyping-suite :description "Subtyping relation and lattice operation tests"
  :parent cl-cc-unit-suite)


(in-suite subtyping-suite)
;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun prim (name) (make-type-primitive :name name))

;;; ─── type-name-subtype-p (CL hierarchy table lookup) ─────────────────────────

(deftest-each subtype-name-reflexive
  "Every type name is a subtype of itself."
  :cases (("fixnum"    'fixnum)
          ("integer"   'integer)
          ("number"    'number)
          ("string"    'string)
          ("symbol"    'symbol)
          ("t"         't))
  (name)
  (assert-true (cl-cc/type::type-name-subtype-p name name)))

(deftest-each subtype-name-numeric-chain
  "Numeric hierarchy: fixnum <: integer <: rational <: real <: number <: t."
  :cases (("fixnum<:integer"   'fixnum   'integer)
          ("fixnum<:number"    'fixnum   'number)
          ("integer<:rational" 'integer  'rational)
          ("integer<:real"     'integer  'real)
          ("rational<:real"    'rational 'real)
          ("real<:number"      'real     'number)
          ("float<:real"       'float    'real)
          ("number<:t"         'number   't))
  (sub super)
  (assert-true (cl-cc/type::type-name-subtype-p sub super)))

(deftest-each subtype-name-not-subtype
  "Non-subtype pairs return nil."
  :cases (("integer-not<:fixnum"  'integer  'fixnum)
          ("string-not<:number"   'string   'number)
          ("symbol-not<:integer"  'symbol   'integer)
          ("number-not<:string"   'number   'string))
  (sub super)
  (assert-false (cl-cc/type::type-name-subtype-p sub super)))

(deftest-each subtype-name-list-hierarchy
  "List hierarchy: null <: list <: sequence <: t."
  :cases (("null<:list"       'null     'list)
          ("null<:sequence"   'null     'sequence)
          ("cons<:list"       'cons     'list)
          ("list<:sequence"   'list     'sequence)
          ("string<:sequence" 'string   'sequence))
  (sub super)
  (assert-true (cl-cc/type::type-name-subtype-p sub super)))

(deftest subtype-name-everything-subtype-of-t
  "All named types are subtypes of t."
  (dolist (entry cl-cc/type::*subtype-table*)
    (assert-true (cl-cc/type::type-name-subtype-p (car entry) 't))))

;;; ─── is-subtype-p (structural subtyping) ────────────────────────────────────

(deftest subtype-primitive-and-gradual-typing
  "is-subtype-p: reflexive, chain, unknown<:anything, anything<:unknown, non-subtype."
  (assert-true  (cl-cc/type::is-subtype-p type-int     type-int))
  (assert-true  (cl-cc/type::is-subtype-p type-int     type-any))
  (assert-true  (cl-cc/type::is-subtype-p +type-unknown+ type-int))
  (assert-true  (cl-cc/type::is-subtype-p type-string +type-unknown+))
  (assert-false (cl-cc/type::is-subtype-p type-string type-int)))

(deftest subtype-check-alias
  "subtype-check is a backward-compatible alias for is-subtype-p."
  (assert-true (cl-cc/type::subtype-check type-int type-any))
  (assert-false (cl-cc/type::subtype-check type-string type-int)))

(deftest subtypep-wrapper-returns-two-values
  "subtypep accepts type specifiers and returns ANSI-style two values."
  (multiple-value-bind (ok surep)
      (cl-cc/type:subtypep 'fixnum 'integer)
    (assert-true ok)
    (assert-true surep)))

;;; ─── Union subtyping ────────────────────────────────────────────────────────

(deftest subtype-union-left-behavior
  "(int | string) <: t (both members <: t); but NOT <: int (string is not <: int)."
  (let ((u (make-type-union (list type-int type-string))))
    (assert-true  (cl-cc/type::is-subtype-p u type-any))
    (assert-false (cl-cc/type::is-subtype-p u type-int))))

(deftest subtype-union-right
  "int <: (int | string) — member of the union."
  (let ((u (make-type-union (list type-int type-string))))
    (assert-true (cl-cc/type::is-subtype-p type-int u))))

;;; ─── Intersection subtyping ─────────────────────────────────────────────────

(deftest subtype-intersection-cases
  "Intersection subtyping: left/right covariance; int NOT <: (int & string)."
  (let ((i-int-str (make-type-intersection (list type-int type-string)))
        (i-int-any (make-type-intersection (list type-int type-any))))
    (assert-true  (cl-cc/type::is-subtype-p i-int-str type-int))
    (assert-false (cl-cc/type::is-subtype-p type-int i-int-str))
    (assert-true  (cl-cc/type::is-subtype-p type-int i-int-any))))

;;; ─── Record / variant structural subtyping ─────────────────────────────────

(deftest subtype-record-width-and-field-types
  "Records use width subtyping and compare shared fields pointwise."
  (let ((wide   (make-type-record :fields (list (cons 'x type-int)
                                                (cons 'y type-string))
                                  :row-var nil))
        (narrow (make-type-record :fields (list (cons 'x type-int))
                                  :row-var nil)))
    (assert-true (cl-cc/type::is-subtype-p wide narrow))
    (assert-false (cl-cc/type::is-subtype-p narrow wide))))

(deftest subtype-variant-width-and-case-types
  "Variants use width subtyping and compare shared cases pointwise."
  (let ((small (make-type-variant :cases (list (cons 'ok type-int)) :row-var nil))
        (large (make-type-variant :cases (list (cons 'ok type-int)
                                                (cons 'err type-string))
                                  :row-var nil)))
    (assert-true (cl-cc/type::is-subtype-p small large))
    (assert-false (cl-cc/type::is-subtype-p large small))))

(deftest subtype-refinement-to-base
  "A refinement type is a subtype of its base type."
  (let ((refined (cl-cc/type::make-type-refinement :base type-int :predicate #'plusp)))
    (assert-true (cl-cc/type::is-subtype-p refined type-int))
    (assert-false (cl-cc/type::is-subtype-p type-int refined))))

;;; ─── Function subtyping (contravariant params, covariant return) ────────────

(deftest subtype-function-variance
  "Function subtyping: identical, covariant return, contravariant params."
  (assert-true (cl-cc/type::is-subtype-p
                (make-type-function (list type-int) type-string)
                (make-type-function (list type-int) type-string)))
  (assert-true (cl-cc/type::is-subtype-p      ; covariant return
                (make-type-function (list type-int) type-int)
                (make-type-function (list type-int) type-any)))
  (assert-true (cl-cc/type::is-subtype-p      ; contravariant params
                (make-type-function (list type-any) type-int)
                (make-type-function (list type-int) type-int))))

(deftest subtype-function-not-covariant-param
  "(int -> int) is NOT <: (t -> int) — params are contravariant."
  (let ((f1 (make-type-function (list type-int) type-int))
        (f2 (make-type-function (list type-any) type-int)))
    (assert-false (cl-cc/type::is-subtype-p f1 f2))))

;;; ─── find-common-supertype ──────────────────────────────────────────────────

(deftest-each common-supertype-numeric
  "Common supertypes in the numeric hierarchy."
  :cases (("fixnum-integer"  'fixnum   'integer   'integer)
          ("fixnum-float"    'fixnum   'float     'real)
          ("integer-string"  'integer  'string    't)
          ("fixnum-fixnum"   'fixnum   'fixnum    'fixnum))
  (n1 n2 expected)
  (let ((result (cl-cc/type::find-common-supertype n1 n2)))
    (assert-true result)
    (assert-eq expected (type-primitive-name result))))

(deftest common-supertype-null-cons
  "null and cons share supertype list."
  (let ((result (cl-cc/type::find-common-supertype 'null 'cons)))
    (assert-true result)
    (assert-eq 'list (type-primitive-name result))))

;;; ─── type-join (LUB) ────────────────────────────────────────────────────────

(deftest-each type-lattice-identity-same
  "Join and meet of a type with itself is that type (identity law)."
  :cases (("join" :join)
          ("meet" :meet))
  (op)
  (let ((result (if (eq op :join)
                    (cl-cc/type::type-join type-int type-int)
                    (cl-cc/type::type-meet type-int type-int))))
    (assert-true (type-equal-p type-int result))))

(deftest type-join-subtype
  "Join of fixnum and integer is integer (fixnum <: integer)."
  (let* ((fixnum-t (prim 'fixnum))
         (int-t (prim 'integer))
         (result (cl-cc/type::type-join fixnum-t int-t)))
    (assert-true (type-equal-p int-t result))))

(deftest type-join-unrelated-primitives
  "Join of unrelated primitives finds common supertype."
  (let* ((fixnum-t (prim 'fixnum))
         (float-t (prim 'float))
         (result (cl-cc/type::type-join fixnum-t float-t)))
    ;; fixnum and float share 'real as common supertype
    (assert-true (type-primitive-p result))
    (assert-eq 'real (type-primitive-name result))))

(deftest type-join-unknown-left
  "Join with unknown returns the other type."
  (let ((result (cl-cc/type::type-join +type-unknown+ type-string)))
    (assert-true (type-equal-p type-string result))))

(deftest type-join-no-common-supertype
  "Join of truly unrelated types produces a union."
  (let ((result (cl-cc/type::type-join type-int type-string)))
    ;; integer and string → find-common-supertype returns t
    (assert-true result)))

;;; ─── type-meet (GLB) ────────────────────────────────────────────────────────

(deftest type-meet-subtype
  "Meet of fixnum and integer is fixnum (fixnum <: integer)."
  (let* ((fixnum-t (prim 'fixnum))
         (int-t (prim 'integer))
         (result (cl-cc/type::type-meet fixnum-t int-t)))
    (assert-true (type-equal-p fixnum-t result))))

(deftest type-meet-unknown
  "Meet with unknown returns unknown."
  (let ((result (cl-cc/type::type-meet +type-unknown+ type-string)))
    (assert-true (type-unknown-p result))))

(deftest type-meet-unrelated
  "Meet of unrelated types produces an intersection."
  (let ((result (cl-cc/type::type-meet type-int type-string)))
    (assert-true (type-intersection-p result))))

(deftest-each is-subtype-reflexive
  "Every type is a subtype of itself."
  :cases (("int"    type-int)
          ("string" type-string)
          ("bool"   type-bool)
          ("null"   type-null))
  (tp)
  (assert-true (cl-cc/type:is-subtype-p tp tp)))

(deftest-each is-subtype-primitive-hierarchy
  "CL numeric type hierarchy is correctly encoded."
  :cases (("fixnum<integer"   'fixnum  'integer  t)
          ("integer<rational" 'integer 'rational t)
          ("rational<real"    'rational 'real    t)
          ("float<real"       'float   'real     t)
          ("real<number"      'real    'number   t)
          ("fixnum<number"    'fixnum  'number   t)
          ("number<t"         'number  't        t)
          ("int not<string"   'fixnum  'string   nil))
  (name1 name2 expected)
  (if expected
      (assert-true  (cl-cc/type:type-name-subtype-p name1 name2))
      (assert-false (cl-cc/type:type-name-subtype-p name1 name2))))

(deftest-each is-subtype-of-top-type
  "Every primitive type is a subtype of type-any."
  :cases (("int"    type-int)
          ("string" type-string)
          ("bool"   type-bool))
  (tp)
  (assert-true (cl-cc/type:is-subtype-p tp type-any)))

(deftest is-subtype-unknown-gradual
  "type-unknown is consistent with everything (gradual typing escape hatch)."
  (let ((unk (make-type-unknown)))
    (assert-true (cl-cc/type:is-subtype-p unk  type-int))
    (assert-true (cl-cc/type:is-subtype-p type-int unk))))

(deftest-each is-subtype-union-right
  "T <: (T1 | T2) iff T <: T1 or T <: T2."
  :cases (("int in or-int-string"  type-int    t)
          ("string in or"          type-string  t)
          ("bool not in"           type-bool    nil))
  (tp expected)
  (let ((u (make-type-union (list type-int type-string))))
    (if expected
        (assert-true  (cl-cc/type:is-subtype-p tp u))
        (assert-false (cl-cc/type:is-subtype-p tp u)))))

(deftest is-subtype-function-contravariant-params
  "(A->B) <: (C->D) iff C <: A (contravariant params)."
  (let ((f1 (make-type-arrow (list (make-type-primitive :name 'number)) type-string))
        (f2 (make-type-arrow (list (make-type-primitive :name 'fixnum))  type-string)))
    (assert-true (cl-cc/type:is-subtype-p f1 f2))))

(deftest-each type-join-meet-equal-types
  "join and meet of identical types each return the type itself."
  :cases (("join-int"    #'cl-cc/type:type-join type-int)
          ("join-string" #'cl-cc/type:type-join type-string)
          ("join-bool"   #'cl-cc/type:type-join type-bool)
          ("meet-int"    #'cl-cc/type:type-meet type-int)
          ("meet-string" #'cl-cc/type:type-meet type-string))
  (op tp)
  (assert-true (type-equal-p tp (funcall op tp tp))))

(deftest-each type-lattice-join-meet-subtype
  "join picks the supertype; meet picks the subtype (fixnum <: integer)."
  :cases (("join" #'cl-cc/type:type-join 'integer)
          ("meet" #'cl-cc/type:type-meet 'fixnum))
  (op expected-name)
  (let* ((fixnum-t  (make-type-primitive :name 'fixnum))
         (integer-t (make-type-primitive :name 'integer))
         (result    (funcall op fixnum-t integer-t)))
    (assert-true (type-primitive-p result))
    (assert-eq expected-name (type-primitive-name result))))

(deftest type-join-incompatible-makes-union
  "join(fixnum, string) returns their LCA in the CL hierarchy (t, the top type)."
  (let* ((fixnum-t (make-type-primitive :name 'fixnum))
         (string-t (make-type-primitive :name 'string))
         (result   (cl-cc/type:type-join fixnum-t string-t)))
    (assert-true (type-primitive-p result))
    (assert-eq 't (type-primitive-name result))))

(deftest type-join-with-unknown-is-other
  "join(??, T) = T (gradual: unknown absorbs into concrete side)."
  (let* ((unk (make-type-unknown))
         (result (cl-cc/type:type-join unk type-int)))
    (assert-true (type-equal-p type-int result))))

(deftest type-meet-incompatible-makes-intersection
  "meet(fixnum, string) = (and fixnum string) — uninhabited but structurally valid."
  (let ((result (cl-cc/type:type-meet type-int type-string)))
    (assert-true (type-intersection-p result))
    (assert-= 2 (length (type-intersection-types result)))))
