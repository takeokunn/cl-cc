;;;; tests/unit/type/subtyping-extended-tests.lisp — Extended Subtyping and Lattice Tests
;;;;
;;;; Additional coverage for is-subtype-p, type-join, and type-meet beyond the
;;;; base cases in subtyping-tests.lisp. Depends on subtyping-tests.lisp being
;;;; loaded first (via ASDF :serial t) for the prim helper and suite definition.

(in-package :cl-cc/test)

(in-suite subtyping-suite)

;;; ─── is-subtype-p — extended reflexivity and hierarchy ───────────────────────

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
  (let ((unk cl-cc/type::+type-unknown+))
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

;;; ─── type-join / type-meet — extended lattice coverage ──────────────────────

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
  (let* ((unk cl-cc/type::+type-unknown+)
         (result (cl-cc/type:type-join unk type-int)))
    (assert-true (type-equal-p type-int result))))

(deftest type-meet-incompatible-makes-intersection
  "meet(fixnum, string) = (and fixnum string) — uninhabited but structurally valid."
  (let ((result (cl-cc/type:type-meet type-int type-string)))
    (assert-true (type-intersection-p result))
    (assert-= 2 (length (type-intersection-types result)))))
