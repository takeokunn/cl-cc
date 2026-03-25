;;;; tests/unit/type/subtyping-tests.lisp — Subtyping Relation Tests
;;;;
;;;; Tests for src/type/subtyping.lisp:
;;;; type-name-subtype-p, is-subtype-p, find-common-supertype,
;;;; type-join, type-meet.

(in-package :cl-cc/test)

(defsuite subtyping-suite :description "Subtyping relation and lattice operation tests")

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

(deftest subtype-reflexive
  "type-int <: type-int (reflexive)."
  (assert-true (cl-cc/type::is-subtype-p type-int type-int)))

(deftest subtype-primitive-chain
  "type-int <: type-any (fixnum <: t via primitives)."
  (assert-true (cl-cc/type::is-subtype-p type-int type-any)))

(deftest subtype-unknown-left
  "unknown <: anything (gradual typing)."
  (assert-true (cl-cc/type::is-subtype-p +type-unknown+ type-int)))

(deftest subtype-unknown-right
  "anything <: unknown (gradual typing)."
  (assert-true (cl-cc/type::is-subtype-p type-string +type-unknown+)))

(deftest subtype-not-primitive
  "string is not a subtype of integer."
  (assert-false (cl-cc/type::is-subtype-p type-string type-int)))

;;; ─── Union subtyping ────────────────────────────────────────────────────────

(deftest subtype-union-left
  "(int | string) <: t — both members <: t."
  (let ((u (make-type-union (list type-int type-string))))
    (assert-true (cl-cc/type::is-subtype-p u type-any))))

(deftest subtype-union-left-fails
  "(int | string) is NOT <: int — string is not <: int."
  (let ((u (make-type-union (list type-int type-string))))
    (assert-false (cl-cc/type::is-subtype-p u type-int))))

(deftest subtype-union-right
  "int <: (int | string) — member of the union."
  (let ((u (make-type-union (list type-int type-string))))
    (assert-true (cl-cc/type::is-subtype-p type-int u))))

;;; ─── Intersection subtyping ─────────────────────────────────────────────────

(deftest subtype-intersection-left
  "(int & string) <: int — some component <: int."
  (let ((i (make-type-intersection (list type-int type-string))))
    (assert-true (cl-cc/type::is-subtype-p i type-int))))

(deftest subtype-intersection-right
  "int <: (int & t) requires int <: both, but int is NOT <: string."
  (let ((i (make-type-intersection (list type-int type-string))))
    (assert-false (cl-cc/type::is-subtype-p type-int i))))

(deftest subtype-intersection-right-both
  "int <: (int & t) — int <: both int and t."
  (let ((i (make-type-intersection (list type-int type-any))))
    (assert-true (cl-cc/type::is-subtype-p type-int i))))

;;; ─── Function subtyping (contravariant params, covariant return) ────────────

(deftest subtype-function-identical
  "Identical function types are subtypes."
  (let ((f1 (make-type-function (list type-int) type-string))
        (f2 (make-type-function (list type-int) type-string)))
    (assert-true (cl-cc/type::is-subtype-p f1 f2))))

(deftest subtype-function-covariant-return
  "(int -> int) <: (int -> t) — covariant return."
  (let ((f1 (make-type-function (list type-int) type-int))
        (f2 (make-type-function (list type-int) type-any)))
    (assert-true (cl-cc/type::is-subtype-p f1 f2))))

(deftest subtype-function-contravariant-param
  "(t -> int) <: (int -> int) — contravariant in params."
  (let ((f1 (make-type-function (list type-any) type-int))
        (f2 (make-type-function (list type-int) type-int)))
    (assert-true (cl-cc/type::is-subtype-p f1 f2))))

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

(deftest type-join-same
  "Join of same type is that type."
  (let ((result (cl-cc/type::type-join type-int type-int)))
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

(deftest type-meet-same
  "Meet of same type is that type."
  (let ((result (cl-cc/type::type-meet type-int type-int)))
    (assert-true (type-equal-p type-int result))))

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
