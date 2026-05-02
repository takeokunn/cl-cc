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
  (assert-true (cl-cc/type:type-name-subtype-p name name)))

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
  (assert-true (cl-cc/type:type-name-subtype-p sub super)))

(deftest-each subtype-name-not-subtype
  "Non-subtype pairs return nil."
  :cases (("integer-not<:fixnum"  'integer  'fixnum)
          ("string-not<:number"   'string   'number)
          ("symbol-not<:integer"  'symbol   'integer)
          ("number-not<:string"   'number   'string))
  (sub super)
  (assert-false (cl-cc/type:type-name-subtype-p sub super)))

(deftest-each subtype-name-list-hierarchy
  "List hierarchy: null <: list <: sequence <: t."
  :cases (("null<:list"       'null     'list)
          ("null<:sequence"   'null     'sequence)
          ("cons<:list"       'cons     'list)
          ("list<:sequence"   'list     'sequence)
          ("string<:sequence" 'string   'sequence))
  (sub super)
  (assert-true (cl-cc/type:type-name-subtype-p sub super)))

(deftest subtype-name-everything-subtype-of-t
  "All named types are subtypes of t."
  (dolist (entry cl-cc/type:*subtype-table*)
    (assert-true (cl-cc/type:type-name-subtype-p (car entry) 't))))

;;; ─── is-subtype-p (structural subtyping) ────────────────────────────────────

(deftest subtype-primitive-and-gradual-typing
  "is-subtype-p: reflexive, chain, unknown<:anything, anything<:unknown, non-subtype."
  (assert-true  (cl-cc/type:is-subtype-p type-int     type-int))
  (assert-true  (cl-cc/type:is-subtype-p type-int     type-any))
  (assert-true  (cl-cc/type:is-subtype-p cl-cc/type:+type-unknown+ type-int))
  (assert-true  (cl-cc/type:is-subtype-p type-string cl-cc/type:+type-unknown+))
  (assert-false (cl-cc/type:is-subtype-p type-string type-int)))

(deftest is-subtype-p-basic-wrapper
  "is-subtype-p remains the direct structural subtyping entrypoint."
  (assert-true (cl-cc/type:is-subtype-p type-int type-any))
  (assert-false (cl-cc/type:is-subtype-p type-string type-int)))

(deftest subtypep-wrapper-returns-two-values
  "subtypep accepts type specifiers and returns ANSI-style two values."
  (multiple-value-bind (ok surep)
      (cl-cc/type:subtypep 'fixnum 'integer)
    (assert-true ok)
    (assert-true surep)))

;;; ─── Union subtyping ────────────────────────────────────────────────────────

(deftest subtype-union-is-subtype-of-any
  "(int|string) is a subtype of type-any."
  (let ((u (make-type-union (list type-int type-string))))
    (assert-true (cl-cc/type:is-subtype-p u type-any))))

(deftest subtype-union-not-subtype-of-member
  "(int|string) is NOT a subtype of int (a strict subset)."
  (let ((u (make-type-union (list type-int type-string))))
    (assert-false (cl-cc/type:is-subtype-p u type-int))))

(deftest subtype-member-is-subtype-of-union
  "int is a subtype of (int|string)."
  (let ((u (make-type-union (list type-int type-string))))
    (assert-true (cl-cc/type:is-subtype-p type-int u))))

;;; ─── Intersection subtyping ─────────────────────────────────────────────────

(deftest subtype-intersection-is-subtype-of-left-member
  "(int&string) is a subtype of int (projection to left component)."
  (let ((i-int-str (make-type-intersection (list type-int type-string))))
    (assert-true (cl-cc/type:is-subtype-p i-int-str type-int))))

(deftest subtype-type-not-subtype-of-intersection
  "int is NOT a subtype of (int&string); it doesn't satisfy both simultaneously."
  (let ((i-int-str (make-type-intersection (list type-int type-string))))
    (assert-false (cl-cc/type:is-subtype-p type-int i-int-str))))

(deftest subtype-type-is-subtype-of-intersection-with-any
  "int IS a subtype of (int&t); any type satisfies the 'any' component."
  (let ((i-int-any (make-type-intersection (list type-int type-any))))
    (assert-true (cl-cc/type:is-subtype-p type-int i-int-any))))

;;; ─── Record / variant structural subtyping ─────────────────────────────────

(deftest subtype-record-width-and-field-types
  "Records use width subtyping and compare shared fields pointwise."
  (let ((wide   (make-type-record :fields (list (cons 'x type-int)
                                                (cons 'y type-string))
                                  :row-var nil))
        (narrow (make-type-record :fields (list (cons 'x type-int))
                                  :row-var nil)))
    (assert-true (cl-cc/type:is-subtype-p wide narrow))
    (assert-false (cl-cc/type:is-subtype-p narrow wide))))

(deftest subtype-variant-width-and-case-types
  "Variants use width subtyping and compare shared cases pointwise."
  (let ((small (make-type-variant :cases (list (cons 'ok type-int)) :row-var nil))
        (large (make-type-variant :cases (list (cons 'ok type-int)
                                                (cons 'err type-string))
                                  :row-var nil)))
    (assert-true (cl-cc/type:is-subtype-p small large))
    (assert-false (cl-cc/type:is-subtype-p large small))))

(deftest subtype-refinement-to-base
  "A refinement type is a subtype of its base type."
  (let ((refined (cl-cc/type:make-type-refinement :base type-int :predicate #'plusp)))
    (assert-true (cl-cc/type:is-subtype-p refined type-int))
    (assert-false (cl-cc/type:is-subtype-p type-int refined))))

;;; ─── Function subtyping (contravariant params, covariant return) ────────────

(deftest subtype-function-variance
  "Function subtyping: identical, covariant return, contravariant params; params not covariant."
  (assert-true (cl-cc/type:is-subtype-p
                (make-type-arrow (list type-int) type-string)
                (make-type-arrow (list type-int) type-string)))
  (assert-true (cl-cc/type:is-subtype-p      ; covariant return
                (make-type-arrow (list type-int) type-int)
                (make-type-arrow (list type-int) type-any)))
  (assert-true (cl-cc/type:is-subtype-p      ; contravariant params
                (make-type-arrow (list type-any) type-int)
                (make-type-arrow (list type-int) type-int)))
  ;; params are NOT covariant: (int -> int) is NOT <: (t -> int)
  (assert-false (cl-cc/type:is-subtype-p
                 (make-type-arrow (list type-int) type-int)
                 (make-type-arrow (list type-any) type-int))))

;;; ─── find-common-supertype ──────────────────────────────────────────────────

(deftest-each common-supertype-numeric
  "Common supertypes in the numeric and list hierarchies."
  :cases (("fixnum-integer"  'fixnum   'integer   'integer)
          ("fixnum-float"    'fixnum   'float     'real)
          ("integer-string"  'integer  'string    't)
          ("fixnum-fixnum"   'fixnum   'fixnum    'fixnum)
          ("null-cons"       'null     'cons      'list))
  (n1 n2 expected)
  (let ((result (cl-cc/type:find-common-supertype n1 n2)))
    (assert-true result)
    (assert-eq expected (type-primitive-name result))))

;;; ─── type-join (LUB) ────────────────────────────────────────────────────────

(deftest-each type-lattice-identity-same
  "Join and meet of a type with itself is that type (identity law)."
  :cases (("join" :join)
          ("meet" :meet))
  (op)
  (let ((result (if (eq op :join)
                    (cl-cc/type:type-join type-int type-int)
                    (cl-cc/type:type-meet type-int type-int))))
    (assert-true (type-equal-p type-int result))))

(deftest type-join-subtype-yields-larger
  "type-join: join of a subtype and its supertype returns the supertype."
  (let ((fixnum-t (prim 'fixnum))
        (int-t    (prim 'integer)))
    (assert-true (type-equal-p int-t (cl-cc/type:type-join fixnum-t int-t)))))

(deftest type-join-unrelated-yields-common-supertype
  "type-join: join of fixnum and float yields real (common supertype in numeric hierarchy)."
  (let ((fixnum-t (prim 'fixnum))
        (float-t  (prim 'float)))
    (let ((result (cl-cc/type:type-join fixnum-t float-t)))
      (assert-true (type-primitive-p result))
      (assert-eq 'real (type-primitive-name result)))))

(deftest type-join-unknown-yields-other-type
  "type-join: join of +type-unknown+ and string returns string."
  (assert-true (type-equal-p type-string (cl-cc/type:type-join cl-cc/type:+type-unknown+ type-string))))

(deftest type-join-int-and-string-returns-truthy
  "type-join: join of int and string returns a non-nil result."
  (assert-true (cl-cc/type:type-join type-int type-string)))

;;; ─── type-meet (GLB) ────────────────────────────────────────────────────────

(deftest type-meet-subtype-yields-smaller
  "type-meet: meet of fixnum and integer returns fixnum (the more specific type)."
  (let ((fixnum-t (prim 'fixnum))
        (int-t    (prim 'integer)))
    (assert-true (type-equal-p fixnum-t (cl-cc/type:type-meet fixnum-t int-t)))))

(deftest type-meet-unknown-yields-unknown
  "type-meet: meet of +type-unknown+ and string returns +type-unknown+."
  (assert-true (cl-cc/type:type-unknown-p (cl-cc/type:type-meet cl-cc/type:+type-unknown+ type-string))))

(deftest type-meet-unrelated-yields-intersection
  "type-meet: meet of unrelated types int and string returns a type-intersection."
  (assert-true (type-intersection-p (cl-cc/type:type-meet type-int type-string))))
