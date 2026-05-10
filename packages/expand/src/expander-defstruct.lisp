(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — DEFSTRUCT Dispatcher
;;;
;;; Data/model helpers live in expander-helpers.lisp. Emission helpers are split
;;; across expander-defstruct-{copy,boa,typed,clos}.lisp so this file only
;;; registers expansion-time metadata and chooses the representation strategy.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Main defstruct expander ──────────────────────────────────────────────

(defun expand-defstruct (form)
  "Expand (defstruct name-or-options slot...) to (progn defclass constructor predicate).

Supported options:
  (:conc-name prefix)   — accessor prefix; defaults to NAME-
  (:constructor name lambda-list?) — constructor name and optional BOA list
  (:include parent)     — inherit parent slots
  (:type list/vector)   — use list/vector representation instead of CLOS"
  (let* ((model       (%defstruct-build-model form))
         (name        (getf model :name))
         (all-slots   (getf model :all-slots))
         (struct-type (getf model :struct-type)))
    (setf (gethash name *defstruct-slot-registry*) all-slots)
    (setf (gethash name *defstruct-type-registry*) struct-type)
    (if struct-type
        (%defstruct-typed-expansion model)
        (%defstruct-clos-expansion model))))
