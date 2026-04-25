;;;; subtyping.lisp - Subtyping Relation for HM Type System
;;;;
;;;; Implements is-subtype-p, type-join, type-meet based on the CL type hierarchy.
;;;;
;;;; CL type hierarchy (simplified):
;;;;   fixnum <: integer <: rational <: real <: number <: t
;;;;   float <: real <: number <: t
;;;;   character <: t
;;;;   boolean <: symbol <: t
;;;;   null <: symbol <: t,  null <: list <: sequence <: t
;;;;   cons <: list <: sequence <: t
;;;;   string <: sequence <: t
;;;;   vector <: sequence <: t
;;;;
;;;; Function subtyping is contravariant in params, covariant in return:
;;;;   (A -> B) <: (C -> D)  iff  C <: A  and  B <: D

(in-package :cl-cc/type)

;;; CL Type Hierarchy Table

(defvar *subtype-table*
  '((fixnum       . (integer rational real number t))
    (integer      . (rational real number t))
    (rational     . (real number t))
    (float        . (real number t))
    (real         . (number t))
    (number       . (t))
    (character    . (t))
    (boolean      . (symbol t))
    (null         . (symbol list sequence t))
    (cons         . (list sequence t))
    (list         . (sequence t))
    (string       . (sequence t))
    (simple-string . (string sequence t))
    (vector       . (sequence t))
    (simple-vector . (vector sequence t))
    (array        . (t))
    (sequence     . (t))
    (symbol       . (t))
    (function     . (t))
    (hash-table   . (t))
    (package      . (t))
    (stream       . (t)))
  "Maps a type name to its transitive CL supertypes in specificity order.")

(defun type-name-subtype-p (name1 name2)
  "Check if NAME1 is a subtype of NAME2 in the CL type hierarchy."
  (or (eq name1 name2)
      (eq name2 't)
      (let ((supertypes (cdr (assoc name1 *subtype-table*))))
        (and supertypes
             (not (null (member name2 supertypes :test #'eq)))))))

;;; NOTE: effect-row-subset-p is defined in effect.lisp which loads after
;;;       this file.  Do not define a duplicate here.

;;; ── Structural subtype helpers ────────────────────────────────────────────

(defun %subtype-row-p (pairs1 pairs2 query-fn)
  "True iff every pair in PAIRS2 has a matching key in PAIRS1 with pointwise
subtyping.  QUERY-FN is called as (QUERY-FN val-from-1 val-from-2).
Used for both record (width: t2 fields ⊆ t1) and arrow subtype checks."
  (every (lambda (p2)
           (let ((p1 (assoc (car p2) pairs1 :test #'eq)))
             (and p1 (funcall query-fn (cdr p1) (cdr p2)))))
         pairs2))

(defun %subtype-arrow-p (t1 t2)
  "True iff arrow T1 is a subtype of arrow T2.
Function types are contravariant in parameters and covariant in return type:
  (A -> B) <: (C -> D) iff C <: A and B <: D.
Effects and multiplicities are preserved conservatively for now — we only accept
the subtype relation when they are structurally equal or both absent." 
  (let ((effects1 (type-arrow-effects t1))
        (effects2 (type-arrow-effects t2)))
    (and (= (length (type-arrow-params t1)) (length (type-arrow-params t2)))
         (every #'is-subtype-p
                (type-arrow-params t2)
                (type-arrow-params t1))
         (is-subtype-p (type-arrow-return t1) (type-arrow-return t2))
         (or (and (null effects1) (null effects2))
             (and effects1 effects2 (type-equal-p effects1 effects2)))
         (eql (or (type-arrow-mult t1) :omega)
              (or (type-arrow-mult t2) :omega)))))

;;; Main Subtyping Predicate

(defun %is-subtype-p-by-t2 (t1 t2)
  "Structural subtype rules keyed on T2's type.
Called as the fallback when no T1-driven rule matched."
  (typecase t2
    (type-union       (some  (lambda (m) (is-subtype-p t1 m)) (type-union-types t2)))
    (type-intersection (every (lambda (c) (is-subtype-p t1 c)) (type-intersection-types t2)))
    (t nil)))

(defun is-subtype-p (t1 t2)
  "Check if T1 is a subtype of T2 (written T1 <: T2).

   Key rules:
   - Reflexivity: T <: T
   - Top: T <: t for any T
   - Primitives: follows CL type hierarchy via *subtype-table*
   - Union (left): T1|T2 <: T3 iff T1 <: T3 and T2 <: T3
   - Union (right): T <: T1|T2 iff T <: some member
   - Intersection (left): T1&T2 <: T3 iff some component <: T3
   - Intersection (right): T <: T1&T2 iff T <: all components
   - Arrow: contravariant params, covariant return
   - Constructor: same name + covariant args (simplified)"
  (or (type-equal-p t1 t2)
      (and (typep t2 'type-primitive) (eq (type-primitive-name t2) 't))
      ;; T1-driven rules take priority; each terminal arm falls through to T2 rules.
      ;; T1 union/intersection/refinement recurse — their recursion handles T2 structure.
      (typecase t1
        (type-union
         (every (lambda (m) (is-subtype-p m t2)) (type-union-types t1)))
        (type-intersection
         (some  (lambda (c) (is-subtype-p c t2)) (type-intersection-types t1)))
        (type-refinement
         (is-subtype-p (type-refinement-base t1) t2))
        (type-primitive
         (or (and (typep t2 'type-primitive)
                  (type-name-subtype-p (type-primitive-name t1) (type-primitive-name t2)))
             (%is-subtype-p-by-t2 t1 t2)))
        (type-record
         (or (and (typep t2 'type-record)
                  (%subtype-row-p (type-record-fields t1) (type-record-fields t2) #'is-subtype-p))
             (%is-subtype-p-by-t2 t1 t2)))
        (type-variant
         (or (and (typep t2 'type-variant)
                  (%subtype-row-p (type-variant-cases t2) (type-variant-cases t1) #'is-subtype-p))
             (%is-subtype-p-by-t2 t1 t2)))
        (type-constructor
         (or (and (typep t2 'type-constructor)
                  (eq (type-constructor-name t1) (type-constructor-name t2))
                  (= (length (type-constructor-args t1)) (length (type-constructor-args t2)))
                  (every #'is-subtype-p (type-constructor-args t1) (type-constructor-args t2)))
             (%is-subtype-p-by-t2 t1 t2)))
        (type-arrow
         (or (and (type-arrow-p t2) (%subtype-arrow-p t1 t2))
             (%is-subtype-p-by-t2 t1 t2)))
        (type-effect-row
         (or (and (typep t2 'type-effect-row) (effect-row-subset-p t1 t2))
             (%is-subtype-p-by-t2 t1 t2)))
        (t (%is-subtype-p-by-t2 t1 t2)))))

(defun subtypep (type1 type2)
  "Subtype predicate for cl-cc/type.

Returns (values subtype-p sure-p). TYPE1 and TYPE2 may be type nodes or
type specifiers."
  (let ((t1 (if (typep type1 'type-node) type1 (parse-type-specifier type1)))
        (t2 (if (typep type2 'type-node) type2 (parse-type-specifier type2))))
    (values (is-subtype-p t1 t2) t)))

;;; Type Lattice Operations

(defun find-common-supertype (name1 name2)
  "Find the most specific common supertype of NAME1 and NAME2.
   Returns a type-primitive node or nil if no match found."
  (let ((supers1 (cons name1 (cdr (assoc name1 *subtype-table*))))
        (supers2 (cons name2 (cdr (assoc name2 *subtype-table*)))))
    (dolist (s supers1 nil)
      (when (member s supers2 :test #'eq)
        (return (make-type-primitive :name s))))))

(defun type-join (t1 t2)
  "Compute the join (least upper bound / LUB) of T1 and T2.

   Returns the most specific type that is a supertype of both.
   Used for merging branch types in if/cond expressions.

   Examples:
     (type-join fixnum string)   => (or fixnum string)
     (type-join fixnum integer)  => integer
     (type-join fixnum fixnum)   => fixnum"
  (cond
    ((type-equal-p t1 t2) t1)
    ((is-subtype-p t1 t2) t2)
    ((is-subtype-p t2 t1) t1)
    (t
     (typecase t1
       (type-primitive
        (typecase t2
          (type-primitive
           (or (find-common-supertype (type-primitive-name t1)
                                      (type-primitive-name t2))
               type-any))
          (t (make-type-union (list t1 t2)))))
       (type-refinement (type-join (type-refinement-base t1) t2))
       (t
        (typecase t2
          (type-refinement (type-join t1 (type-refinement-base t2)))
          (t (make-type-union (list t1 t2)))))))))

(defun type-meet (t1 t2)
  "Compute the meet (greatest lower bound / GLB) of T1 and T2.

   Returns the most specific type that is a subtype of both.
   Used for narrowing types in type guards.

   Examples:
     (type-meet integer number)  => integer
     (type-meet fixnum string)   => (and fixnum string)  ; uninhabited"
  (cond
    ((type-equal-p t1 t2) t1)
    ((is-subtype-p t1 t2) t1)
    ((is-subtype-p t2 t1) t2)
    (t
     (typecase t1
       (type-refinement (type-meet (type-refinement-base t1) t2))
       (t
        (typecase t2
          (type-refinement (type-meet t1 (type-refinement-base t2)))
          (t (make-type-intersection (list t1 t2)))))))))

(defun %normalize-type-specifier (typespec)
  (typecase typespec
    (type-node typespec)
    (symbol (parse-type-specifier typespec))
    (t type-any)))

(defun upgraded-array-element-type (typespec &optional environment)
  "Return the upgraded array element type for TYPESPEC.
This repository treats arrays as untyped at the core type layer, so the
upgraded element type is the top type."
  (declare (ignore environment))
  (let ((ty   (%normalize-type-specifier typespec))
        (t-bit  (parse-type-specifier 'bit))
        (t-char (parse-type-specifier 'character)))
    (cond
      ((or (type-equal-p ty t-bit)  (is-subtype-p ty t-bit))  t-bit)
      ((or (type-equal-p ty t-char) (is-subtype-p ty t-char)) t-char)
      (t (parse-type-specifier 't)))))

(defun upgraded-complex-part-type (typespec &optional environment)
  "Return the upgraded complex part type for TYPESPEC.
Complex numbers are represented with real parts in the core type layer."
  (declare (ignore environment))
  (declare (ignore typespec))
  (parse-type-specifier 'real))
