(in-package :cl-cc)

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
