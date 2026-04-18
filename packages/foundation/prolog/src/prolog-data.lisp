(in-package :cl-cc/prolog)

;;; Prolog data tables

(defparameter *builtin-predicate-specs*
  '((! prolog-cut-handler)
    (and prolog-and-handler)
    (or prolog-or-handler)
    (= prolog-unify-handler)
    (/= prolog-not-unify-handler)
    (:when prolog-when-handler)
    (when prolog-when-handler))
  "Data table of built-in Prolog predicates and their CPS handlers.")

(defparameter *prolog-declarative-rule-specs*
  '((((member ?x (cons ?x ?rest))))
    (((member ?x (cons ?y ?rest)))
     ((member ?x ?rest)))
    (((append nil ?l ?l)))
    (((append (cons ?x ?l1) ?l2 (cons ?x ?l3)))
     ((append ?l1 ?l2 ?l3)))
    (((reverse nil nil)))
    (((reverse (cons ?x ?xs) ?result))
     ((reverse ?xs ?rev-xs)
      (append ?rev-xs (cons ?x nil) ?result)))
    (((length nil 0)))
    (((length (cons ?x ?rest) (+ 1 ?n)))
     ((length ?rest ?n)))
    (((type-of (const ?val) ?env (integer-type)))
     ((when (integerp ?val))))
    (((type-of (var ?name) ?env ?type))
     ((env-lookup ?env ?name ?type)))
    (((type-of (if ?cond ?then ?else) ?env ?type))
     ((type-of ?cond ?env (boolean-type))
      (type-of ?then ?env ?type)
      (type-of ?else ?env ?type)))
    (((env-lookup (cons (cons ?name ?type) ?rest) ?name ?type)))
    (((env-lookup (cons ?binding ?rest) ?name ?type))
     ((env-lookup ?rest ?name ?type))))
  "Declarative Prolog rules encoded as data.")

(defun %make-symbol-dispatch-table (specs)
  "Build an EQ hash table from SPECS of the shape (symbol handler-symbol).

This keeps the dispatch description data-only while the runtime table remains a
separate concern. Handler symbols are resolved via SYMBOL-FUNCTION at table
construction time."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (spec specs table)
      (destructuring-bind (name handler) spec
        (setf (gethash name table)
              (symbol-function handler))))))

(defparameter *peephole-rules*
  '(;; (:const :R1 42)(:move :R2 :R1) → (:const :R2 42)
    ;; Fires when copy-prop is blocked by a label reset but DCE kept the const alive.
    ((:const ?src ?val) (:move ?dst ?src) ((:const ?dst ?val)))

    ;; (:jump "L0")(:label "L0") → (:label "L0")
    ;; Eliminates a jump to the immediately following label (dead branch after threading).
    ((:jump ?lbl) (:label ?lbl) ((:label ?lbl)))

    ;; (:const ?r ?v1)(:const ?r ?v2) → (:const ?r ?v2)
    ;; Second const-load to the same register makes the first dead.
    ;; Safe in a 2-window because no instruction can read ?r between adjacent instructions.
    ((:const ?r ?_v1) (:const ?r ?v2) ((:const ?r ?v2)))

    ;; (:move ?mid ?src)(:move ?dst ?mid) → (:move ?mid ?src)(:move ?dst ?src)
    ;; Copy-propagation through a move chain: ?mid still gets ?src (in case it
    ;; is read elsewhere), but ?dst now reads directly from ?src, enabling DCE
    ;; to later eliminate ?mid if it has no remaining readers.
    ((:move ?mid ?src) (:move ?dst ?mid) ((:move ?mid ?src) (:move ?dst ?src)))

    ;; Arithmetic and comparison identities that simplify the current
    ;; instruction while preserving the following instruction unchanged.
    ((:add ?dst ?src 0)   ?next ((:move ?dst ?src) ?next))
    ((:add ?dst 0 ?src)   ?next ((:move ?dst ?src) ?next))
    ((:sub ?dst ?src 0)   ?next ((:move ?dst ?src) ?next))
    ((:sub ?dst 0 ?src)   ?next ((:neg ?dst ?src) ?next))
    ((:sub ?dst ?src ?src) ?next ((:const ?dst 0) ?next))
    ((:mul ?dst ?src 1)   ?next ((:move ?dst ?src) ?next))
    ((:mul ?dst 1 ?src)   ?next ((:move ?dst ?src) ?next))
    ((:mul ?dst ?src 0)   ?next ((:const ?dst 0) ?next))
    ((:mul ?dst 0 ?src)   ?next ((:const ?dst 0) ?next))
    ((:div ?dst ?src 1)   ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst ?src -1) ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst -1 ?src) ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst ?src 0) ?next ((:const ?dst 0) ?next))
    ((:logior ?dst ?src 0) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst 0 ?src) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst ?src -1) ?next ((:const ?dst -1) ?next))
    ((:logxor ?dst ?src 0) ?next ((:move ?dst ?src) ?next))
    ((:eq ?dst ?src ?src)   ?next ((:const ?dst 1) ?next))
    ((:gt ?dst ?src ?src)   ?next ((:const ?dst 0) ?next))
    ((:le ?dst ?src ?src)   ?next ((:const ?dst 1) ?next))
    ((:logand ?dst ?src ?src) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst ?src ?src) ?next ((:move ?dst ?src) ?next))
    ((:logxor ?dst ?src ?src) ?next ((:const ?dst 0) ?next))
    ((:num-eq ?dst ?src ?src) ?next ((:const ?dst 1) ?next))
    ((:lt ?dst ?src ?src)   ?next ((:const ?dst 0) ?next))
    ((:ge ?dst ?src ?src)   ?next ((:const ?dst 1) ?next))

    ;; Negated comparisons can be collapsed into the opposite comparison.
    ((:lt ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:ge ?dst ?lhs ?rhs)))
    ((:gt ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:le ?dst ?lhs ?rhs)))
    ((:le ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:gt ?dst ?lhs ?rhs)))
    ((:ge ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:lt ?dst ?lhs ?rhs)))

    ;; Unconditional transfers make the immediately-following instruction dead.
    ((:jump ?lbl1) (:jump ?lbl2) ((:jump ?lbl1)))
    ((:jump ?lbl) (:ret ?reg) ((:jump ?lbl)))
    ((:jump ?lbl) (:halt ?reg) ((:jump ?lbl)))
    ((:ret ?reg) (:jump ?lbl) ((:ret ?reg)))
    ((:halt ?reg) (:jump ?lbl) ((:halt ?reg)))
    ((:ret ?reg1) (:ret ?reg2) ((:ret ?reg1)))
    ((:halt ?reg1) (:halt ?reg2) ((:halt ?reg1)))
    ((:ret ?reg1) (:halt ?reg2) ((:ret ?reg1)))
    ((:halt ?reg1) (:ret ?reg2) ((:halt ?reg1)))))

(defparameter *enable-prolog-peephole* t)

(defparameter *prolog-integer-binop-type-operators*
  '(+ - * / mod)
  "Arithmetic operators whose binop forms always infer INTEGER-TYPE in Prolog rules.")

(defparameter *prolog-comparison-type-operators*
  '(< > <= >= = /=)
  "Comparison operators whose cmp forms always infer BOOLEAN-TYPE in Prolog rules.")

;;; Prolog database state and clause-definition surface

(defvar *prolog-rules* (make-hash-table :test 'eq)
  "Hash table mapping predicate symbols to lists of rules.")

(defun clear-prolog-database ()
  "Clear all rules from the Prolog database."
  (clrhash *prolog-rules*))

(defun add-rule (predicate rule)
  "Add RULE to the database under PREDICATE."
  (setf (gethash predicate *prolog-rules*)
        (cons rule (gethash predicate *prolog-rules*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %def-prolog-clause (head &key body)
    `(add-rule ',(car head)
               (make-prolog-rule :head ',head
                                 ,@(when body `(:body ',body)))))

  (defmacro define-prolog-integer-binop-type-rules ()
    "Emit the repetitive integer binop type rules from
*PROLOG-INTEGER-BINOP-TYPE-OPERATORS*.

This keeps the rule set data-driven and avoids repeating nearly identical
DEF-RULE forms for each arithmetic operator."
    `(progn
       ,@(mapcar (lambda (op)
                   `(def-rule (type-of (binop ,op ?a ?b) ?env (integer-type))
                      (type-of ?a ?env (integer-type))
                      (type-of ?b ?env (integer-type))))
                 *prolog-integer-binop-type-operators*)))

  (defmacro define-prolog-comparison-type-rule ()
    "Emit the comparison type rule using the data table from prolog-data.lisp."
    `(def-rule (type-of (cmp ?op ?a ?b) ?env (boolean-type))
       (type-of ?a ?env (integer-type))
       (type-of ?b ?env (integer-type))
       (:when (cl:member ?op ',*prolog-comparison-type-operators*))))

  (defmacro define-prolog-declarative-rules ()
    "Emit `def-rule` forms from *PROLOG-DECLARATIVE-RULE-SPECS*."
    `(progn
       ,@(mapcar (lambda (spec)
                   (destructuring-bind (head &optional body) spec
                     `(def-rule ,@head ,@(or body '()))))
                 *prolog-declarative-rule-specs*)))

  (defmacro def-fact (head)
    "Define a Prolog fact. Usage: (def-fact (parent tom mary))"
    `(%def-prolog-clause ,head))

  (defmacro def-rule (head &body body)
    "Define a Prolog rule. Usage: (def-rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))"
    `(%def-prolog-clause ,head :body ,body)))
