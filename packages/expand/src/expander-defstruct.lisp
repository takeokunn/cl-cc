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
  (let ((model (%defstruct-build-model form))
        (tail nil)
        (key nil)
        (value nil)
        (name nil)
        (all-slots nil)
        (struct-type nil))
    (setq tail model)
    (tagbody
     scan
       (if (null tail) (go done))
       (setq key (car tail))
       (setq value (cadr tail))
       (if (eq key :name) (setq name value))
       (if (eq key :all-slots) (setq all-slots value))
       (if (eq key :struct-type) (setq struct-type value))
       (setq tail (cddr tail))
       (go scan)
     done)
    ;; Register slot info for :include inheritance.
    (setf (gethash name *defstruct-slot-registry*) all-slots)
    (setf (gethash name *defstruct-type-registry*) struct-type)
    ;; Dispatch: :type list/vector → non-CLOS expansion; otherwise → CLOS expansion.
    (if struct-type
        (%defstruct-typed-expansion model)
        (%defstruct-clos-expansion model))))
