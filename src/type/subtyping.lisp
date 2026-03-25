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

;;; Main Subtyping Predicate

(defun is-subtype-p (t1 t2)
  "Check if T1 is a subtype of T2 (written T1 <: T2).

   Key rules:
   - Reflexivity: T <: T
   - Gradual: ? <: T and T <: ? (gradual typing escape hatch)
   - Top: T <: t for any T
   - Primitives: follows CL type hierarchy via *subtype-table*
   - Union (left): T1|T2 <: T3 iff T1 <: T3 and T2 <: T3
   - Union (right): T <: T1|T2 iff T <: T1 or T <: T2
   - Intersection (left): T1&T2 <: T3 iff T1 <: T3 or T2 <: T3
   - Intersection (right): T <: T1&T2 iff T <: T1 and T <: T2
   - Function (contravariant params, covariant return):
       (A->B) <: (C->D) iff C <: A and B <: D
   - Constructor: same name + covariant args (simplified)"
  (cond
    ;; Reflexivity (handles identical objects and equal types)
    ((type-equal-p t1 t2) t)

    ;; Gradual typing: unknown is consistent with everything
    ((or (typep t1 'type-unknown) (typep t2 'type-unknown)) t)

    ;; Top type: everything is a subtype of t
    ((and (typep t2 'type-primitive)
          (eq (type-primitive-name t2) 't))
     t)

    ;; Both primitives: use CL hierarchy
    ((and (typep t1 'type-primitive) (typep t2 'type-primitive))
     (type-name-subtype-p (type-primitive-name t1)
                          (type-primitive-name t2)))

    ;; Union left: T1|T2 <: T3 iff every member <: T3
    ((typep t1 'type-union)
     (every (lambda (m) (is-subtype-p m t2))
            (type-union-types t1)))

    ;; Union right: T <: T1|T2 iff T <: some member
    ((typep t2 'type-union)
     (some (lambda (m) (is-subtype-p t1 m))
           (type-union-types t2)))

    ;; Intersection left: T1&T2 <: T3 iff some component <: T3
    ((typep t1 'type-intersection)
     (some (lambda (c) (is-subtype-p c t2))
           (type-intersection-types t1)))

    ;; Intersection right: T <: T1&T2 iff T <: all components
    ((typep t2 'type-intersection)
     (every (lambda (c) (is-subtype-p t1 c))
            (type-intersection-types t2)))

    ;; Function types: contravariant params, covariant return
    ((and (typep t1 'type-function) (typep t2 'type-function))
     (let ((params1 (type-function-params t1))
           (params2 (type-function-params t2)))
       (and (= (length params1) (length params2))
            (every #'is-subtype-p params2 params1)   ; contravariant
            (is-subtype-p (type-function-return t1)  ; covariant
                          (type-function-return t2)))))

    ;; Parametric types: same constructor, covariant args (simplified, not invariant)
    ((and (typep t1 'type-constructor) (typep t2 'type-constructor))
     (and (eq (type-constructor-name t1) (type-constructor-name t2))
          (= (length (type-constructor-args t1))
             (length (type-constructor-args t2)))
          (every #'is-subtype-p
                 (type-constructor-args t1)
                 (type-constructor-args t2))))

    ;; Effect rows: row1 <: row2 iff all effects in row1 are in row2
    ;; (an open row is a supertype of any row it can extend)
    ((and (typep t1 'type-effect-row) (typep t2 'type-effect-row))
     (effect-row-subset-p t1 t2))

    ;; Effectful functions: subtype with covariant effects
    ;; A -[e1]-> B <: A -[e2]-> B iff e1 <: e2 (covariant in effects)
    ((and (typep t1 'type-effectful-function)
          (typep t2 'type-effectful-function))
     (let ((params1 (type-function-params t1))
           (params2 (type-function-params t2)))
       (and (= (length params1) (length params2))
            (every #'is-subtype-p params2 params1)   ; contravariant in params
            (is-subtype-p (type-function-return t1)  ; covariant return
                          (type-function-return t2))
            (is-subtype-p (type-effectful-function-effects t1)  ; covariant effects
                          (type-effectful-function-effects t2)))))

    ;; Type variables or mismatched kinds: not a subtype
    (t nil)))

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
    ((typep t1 'type-unknown) t2)
    ((typep t2 'type-unknown) t1)
    ((is-subtype-p t1 t2) t2)
    ((is-subtype-p t2 t1) t1)
    ((and (typep t1 'type-primitive) (typep t2 'type-primitive))
     (or (find-common-supertype (type-primitive-name t1)
                                (type-primitive-name t2))
         type-any))
    ;; Function join: meet params (contra), join return
    ((and (typep t1 'type-function)
          (typep t2 'type-function)
          (= (length (type-function-params t1))
             (length (type-function-params t2))))
     (make-type-function
      (mapcar #'type-meet (type-function-params t1) (type-function-params t2))
      (type-join (type-function-return t1) (type-function-return t2))))
    ;; Default: explicit union
    (t (make-type-union (list t1 t2)))))

(defun type-meet (t1 t2)
  "Compute the meet (greatest lower bound / GLB) of T1 and T2.

   Returns the most specific type that is a subtype of both.
   Used for narrowing types in type guards.

   Examples:
     (type-meet integer number)  => integer
     (type-meet fixnum string)   => (and fixnum string)  ; uninhabited"
  (cond
    ((type-equal-p t1 t2) t1)
    ((typep t1 'type-unknown) t1)
    ((typep t2 'type-unknown) t2)
    ((is-subtype-p t1 t2) t1)
    ((is-subtype-p t2 t1) t2)
    ;; Function meet: join params (contra), meet return
    ((and (typep t1 'type-function)
          (typep t2 'type-function)
          (= (length (type-function-params t1))
             (length (type-function-params t2))))
     (make-type-function
      (mapcar #'type-join (type-function-params t1) (type-function-params t2))
      (type-meet (type-function-return t1) (type-function-return t2))))
    ;; Default: explicit intersection
    (t (make-type-intersection (list t1 t2)))))
