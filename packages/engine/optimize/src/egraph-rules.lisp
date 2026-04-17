(in-package :cl-cc/optimize)

;;; ─── E-Graph Rewrite Rules ───────────────────────────────────────────────
;;;
;;; Defines the rewrite rule macro `defrule` and all built-in rules.
;;; Rules are compiled to:
;;;   1. A Prolog fact (`def-fact`) for pattern-matching via the Prolog engine
;;;   2. A plist entry (:lhs :rhs :when :name) in *egraph-rules*
;;;
;;; The use of the existing Prolog unification engine for pattern matching
;;; is the unique contribution of this compiler: no other compiler uses a
;;; logic programming engine as its rewrite rule evaluator.
;;;
;;; Rules subsume ALL current optimizer passes:
;;;   opt-pass-fold            → fold-* rules (constant folding)
;;;   opt-simplify-binop       → algebraic identity rules
;;;   opt-pass-strength-reduce → mul-pow2, div-pow2 rules
;;;   opt-pass-copy-prop       → FREE via e-class merge
;;;   opt-pass-cse             → FREE via hash-consing memo table

;;; ─── Rule Registry ───────────────────────────────────────────────────────

(defparameter *egraph-rules* nil
  "All registered e-graph rewrite rules.
   Each entry is a plist: (:name name :lhs pattern :rhs template :when guard).")

(defun egraph-rule-register (name lhs rhs when-fn)
  "Register a rewrite rule with NAME, LHS pattern, RHS template, WHEN-FN guard.
   Replaces any existing rule with the same NAME."
  (setq *egraph-rules*
        (cons (list :name name :lhs lhs :rhs rhs :when when-fn)
              (remove-if (lambda (r) (eq (getf r :name) name)) *egraph-rules*))))

;;; ─── defrule Macro ───────────────────────────────────────────────────────
;;;
;;; Syntax:
;;;   (defrule name pattern replacement &key when)
;;;
;;; Where:
;;;   pattern     — s-expression with ?var pattern variables
;;;   replacement — s-expression template with ?var references
;;;   :when       — optional guard: (lambda (bindings eg) ...)
;;;
;;; Examples:
;;;   (defrule fold-add (add (const ?a) (const ?b)) (const (cl:+ ?a ?b))
;;;            :when (lambda (b _) (numberp (egraph-binding-value b '?a))))

(defmacro defrule (name pattern replacement &key when)
  "Define an e-graph rewrite rule and register it in *egraph-rules*.
   Also emits a Prolog fact for documentation/alternative-backend use."
  (let ((rule-sym (intern (format nil "ERULE-~A" name))))
    `(progn
       ;; Register in the CL rule table
       (egraph-rule-register
        ',name
        ',pattern
        ',replacement
        ,(if when
             `(lambda (bindings eg) (declare (ignorable bindings eg)) ,when)
             nil))
       ;; Register as a Prolog fact (for documentation and Prolog-backend use)
       (def-fact (egraph-rule ,rule-sym ',pattern ',replacement))
       ',name)))

;;; ─── Binding Helpers ─────────────────────────────────────────────────────

(defun egraph-binding (bindings var)
  "Look up the e-class ID bound to pattern variable VAR in BINDINGS."
  (cdr (assoc var bindings)))

(defun egraph-binding-const (eg bindings var)
  "Return the constant value of the e-class bound to VAR, or NIL if not constant."
  (let ((cid (egraph-binding bindings var)))
    (when cid
      (let ((cls (gethash (egraph-find eg cid) (eg-classes eg))))
        (when cls (ec-data cls))))))

;;; ─── Constant Folding Rules ───────────────────────────────────────────────
;;;
;;; These replace opt-fold-binop-value and the unary fold table.

(defun %egraph-store-const (eg result)
  "Allocate a new const e-class, store RESULT into it, and return T."
  (let* ((cid (egraph-add eg 'const))
         (cls (gethash (egraph-find eg cid) (eg-classes eg))))
    (when cls (setf (ec-data cls) result))
    t))

(defun %egraph-fold-binop (eg bindings cl-fn)
  "Fold a binary const rule: extract ?a and ?b, apply CL-FN, store result."
  (let ((a (egraph-binding-const eg bindings '?a))
        (b (egraph-binding-const eg bindings '?b)))
    (when (and (numberp a) (numberp b))
      (%egraph-store-const eg (funcall cl-fn a b)))))

(defun %egraph-fold-cmp (eg bindings cl-pred)
  "Fold a comparison const rule: apply CL-PRED, store 0 or 1."
  (let ((a (egraph-binding-const eg bindings '?a))
        (b (egraph-binding-const eg bindings '?b)))
    (when (and (numberp a) (numberp b))
      (%egraph-store-const eg (if (funcall cl-pred a b) 1 0)))))

(defrule fold-add (add (const ?a) (const ?b)) (const) :when (%egraph-fold-binop eg bindings #'+))
(defrule fold-sub (sub (const ?a) (const ?b)) (const) :when (%egraph-fold-binop eg bindings #'-))
(defrule fold-mul (mul (const ?a) (const ?b)) (const) :when (%egraph-fold-binop eg bindings #'*))

(defrule fold-neg
  (neg (const ?a))
  (const)
  :when (let ((a (egraph-binding-const eg bindings '?a)))
          (when (numberp a) (%egraph-store-const eg (- a)))))

(defrule fold-not
  (not (const ?a))
  (const)
  :when (let ((a (egraph-binding-const eg bindings '?a)))
          (%egraph-store-const eg (if (or (null a) (eql a 0)) t nil))))

(defrule fold-lt (lt (const ?a) (const ?b)) (const) :when (%egraph-fold-cmp eg bindings #'<))
(defrule fold-gt (gt (const ?a) (const ?b)) (const) :when (%egraph-fold-cmp eg bindings #'>))
(defrule fold-le (le (const ?a) (const ?b)) (const) :when (%egraph-fold-cmp eg bindings #'<=))
(defrule fold-ge (ge (const ?a) (const ?b)) (const) :when (%egraph-fold-cmp eg bindings #'>=))

;;; ─── Algebraic Identity Rules ─────────────────────────────────────────────
;;;
;;; These replace opt-simplify-binop (30+ manual branches).

(defrule add-zero-r   (add ?x (const 0))  ?x)
(defrule add-zero-l   (add (const 0) ?x)  ?x)
(defrule sub-zero     (sub ?x (const 0))  ?x)
(defrule mul-one-r    (mul ?x (const 1))  ?x)
(defrule mul-one-l    (mul (const 1) ?x)  ?x)
(defrule mul-zero-r   (mul ?x (const 0))  (const 0))
(defrule mul-zero-l   (mul (const 0) ?x)  (const 0))
(defrule div-one      (div ?x (const 1))  ?x)

;;; Self-reference identities
(defrule sub-self    (sub ?x ?x)          (const 0))
(defrule eq-self     (num-eq ?x ?x)       (const 1))
(defrule lt-self     (lt ?x ?x)           (const 0))
(defrule gt-self     (gt ?x ?x)           (const 0))
(defrule le-self     (le ?x ?x)           (const 1))
(defrule ge-self     (ge ?x ?x)           (const 1))

;;; Negation
(defrule mul-neg1-r  (mul ?x (const -1))  (neg ?x))
(defrule mul-neg1-l  (mul (const -1) ?x)  (neg ?x))
(defrule double-neg  (neg (neg ?x))        ?x)
(defrule not-not     (not (not ?x))        ?x)
(defrule not-lt      (not (lt ?x ?y))      (ge ?x ?y))
(defrule not-gt      (not (gt ?x ?y))      (le ?x ?y))
(defrule not-le      (not (le ?x ?y))      (gt ?x ?y))
(defrule not-ge      (not (ge ?x ?y))      (lt ?x ?y))
(defrule add-neg     (add ?x (neg ?y))     (sub ?x ?y))
(defrule sub-neg     (sub ?x (neg ?y))     (add ?x ?y))

;;; Bitwise identities
(defrule logand-zero  (logand ?x (const 0))   (const 0))
(defrule logand-zero-l (logand (const 0) ?x)  (const 0))
(defrule logand-neg1  (logand ?x (const -1))  ?x)
(defrule logand-neg1-l (logand (const -1) ?x) ?x)
(defrule logand-self  (logand ?x ?x)          ?x)
(defrule logior-zero  (logior ?x (const 0))   ?x)
(defrule logior-zero-l (logior (const 0) ?x)  ?x)
(defrule logior-self  (logior ?x ?x)          ?x)
(defrule logxor-zero  (logxor ?x (const 0))   ?x)
(defrule logxor-zero-l (logxor (const 0) ?x)  ?x)
(defrule logxor-self  (logxor ?x ?x)          (const 0))
(defrule ash-zero     (ash ?x (const 0))      ?x)
(defrule ash-zero-base (ash (const 0) ?x)     (const 0))

;;; Min/max identities
(defrule max-self     (max ?x ?x)             ?x)
(defrule min-self     (min ?x ?x)             ?x)
(defrule max-min-absorb (max ?x (min ?x ?y))  ?x)
(defrule min-max-absorb (min ?x (max ?x ?y))  ?x)

;;; ─── Strength Reduction Rules ─────────────────────────────────────────────
;;;
;;; These replace opt-pass-strength-reduce.

(defrule mul-pow2
  (mul ?x (const ?n))
  (ash ?x (const))
  :when (let ((n (egraph-binding-const eg bindings '?n)))
          (and (integerp n) (>= n 2) (zerop (logand n (1- n)))
               ;; Build the shift-amount constant e-class
               (let* ((k  (1- (integer-length n)))
                      (cid (egraph-add eg 'const)))
                 (let ((cls (gethash (egraph-find eg cid) (eg-classes eg))))
                   (when cls (setf (ec-data cls) k)))
                 t))))

(defrule mul-pow2-l
  (mul (const ?n) ?x)
  (ash ?x (const))
  :when (let ((n (egraph-binding-const eg bindings '?n)))
          (and (integerp n) (>= n 2) (zerop (logand n (1- n)))
               (let* ((k  (1- (integer-length n)))
                      (cid (egraph-add eg 'const)))
                 (let ((cls (gethash (egraph-find eg cid) (eg-classes eg))))
                   (when cls (setf (ec-data cls) k)))
                 t))))

(defrule div-pow2
  (div ?x (const ?n))
  (ash ?x (const))
  :when (let ((n (egraph-binding-const eg bindings '?n)))
          (and (integerp n) (>= n 2) (zerop (logand n (1- n)))
               (let* ((k   (- (1- (integer-length n))))
                      (cid (egraph-add eg 'const)))
                 (let ((cls (gethash (egraph-find eg cid) (eg-classes eg))))
                   (when cls (setf (ec-data cls) k)))
                 t))))

;;; ─── Type Predicate Folding ───────────────────────────────────────────────

(defrule null-p-const
  (null-p (const ?v))
  (const)
  :when (let ((v (egraph-binding-const eg bindings '?v)))
          (let ((cid (egraph-add eg 'const)))
            (let ((cls (gethash (egraph-find eg cid) (eg-classes eg))))
              (when cls (setf (ec-data cls) (if (null v) 1 0))))
            t)))

(defrule cons-p-const
  (cons-p (const ?v))
  (const)
  :when (let ((v (egraph-binding-const eg bindings '?v)))
          (let ((cid (egraph-add eg 'const)))
            (let ((cls (gethash (egraph-find eg cid) (eg-classes eg))))
              (when cls (setf (ec-data cls) (if (consp v) 1 0))))
            t)))

(defrule number-p-const
  (number-p (const ?v))
  (const)
  :when (let ((v (egraph-binding-const eg bindings '?v)))
          (let ((cid (egraph-add eg 'const)))
            (let ((cls (gethash (egraph-find eg cid) (eg-classes eg))))
              (when cls (setf (ec-data cls) (if (numberp v) 1 0))))
            t)))

(defrule integer-p-const
  (integer-p (const ?v))
  (const)
  :when (let ((v (egraph-binding-const eg bindings '?v)))
          (let ((cid (egraph-add eg 'const)))
            (let ((cls (gethash (egraph-find eg cid) (eg-classes eg))))
              (when cls (setf (ec-data cls) (if (integerp v) 1 0))))
            t)))

;;; Advanced rules (mul-neg-neg, neg-sub), egraph-builtin-rules,
;;; and optimize-with-egraph are in egraph-rules-advanced.lisp (loads next).
