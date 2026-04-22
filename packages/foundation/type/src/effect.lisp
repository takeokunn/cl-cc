;;;; effect.lisp - Row-Based Algebraic Effect System
;;;;
;;;; Koka-style algebraic effects with handlers.
;;;; Effect rows are sets of effect labels; handlers eliminate effects.
;;;;
;;;; Public API:
;;;;   *effect-registry*         — effect-name -> effect-def
;;;;   register-effect           — register an effect definition
;;;;   lookup-effect             — look up by name
;;;;   *effect-signature-table*  — op-name -> effect-name
;;;;   register-effect-signature — register which effect owns an op
;;;;   lookup-effect-signature   — returns the effect name for an op
;;;;   effect-row-union          — merge two type-effect-row nodes
;;;;   effect-row-subset-p       — row1 ≤ row2 (all effects in row1 appear in row2)
;;;;   infer-effects             — conservative effect inference for AST nodes
;;;;   infer-with-effects        — infer type + effect row
;;;;   check-body-effects        — check body effects ≤ expected

(in-package :cl-cc/type)

;;; ─── Effect definition ────────────────────────────────────────────────────

(defstruct effect-def
  "An algebraic effect definition.
NAME:        the effect name symbol (e.g., 'STATE, 'IO).
TYPE-PARAMS: list of type-var — the effect's type parameters.
OPERATIONS:  alist of (op-name . type-arrow) — the effect's operations."
  (name        nil :type symbol)
  (type-params nil :type list)
  (operations  nil :type list))

;;; ─── Effect registry ──────────────────────────────────────────────────────

(defvar *effect-registry*
  (make-hash-table :test #'eq)
  "Maps effect-name symbol -> effect-def.")

(defun register-effect (name effect-def)
  "Register EFFECT-DEF under NAME in *effect-registry*."
  (setf (gethash name *effect-registry*) effect-def)
  name)

(defun lookup-effect (name)
  "Look up effect definition by NAME. Returns effect-def or nil."
  (gethash name *effect-registry*))

;;; ─── Effect signature table ───────────────────────────────────────────────
;;; NOTE: *effect-signature-table*, register-effect-signature, and
;;;       lookup-effect-signature are defined in inference.lisp which loads
;;;       later.  Do NOT define them here to avoid API mismatch warnings.

;;; ─── Effect row operations ────────────────────────────────────────────────

;;; Helper: extract effect name from canonical type-effect-op nodes.
(defun %effect-node-name (e)
  "Return the effect name symbol from canonical effect node E."
  (if (type-effect-op-p e)
      (type-effect-op-name e)
      (error "Cannot get effect name from ~S" e)))

(defun effect-row-union (row1 row2)
  "Merge ROW1 and ROW2 into a new type-effect-row.
Deduplicates by effect name. The result's row-var is ROW2's row-var
(or ROW1's if ROW2 has none), preserving openness."
  (let* ((effs1  (type-effect-row-effects row1))
         (effs2  (type-effect-row-effects row2))
         (names1 (mapcar #'%effect-node-name effs1))
         ;; Keep all of row1, plus any row2 effects not already in row1
         (new-effs (append effs1
                           (remove-if (lambda (e)
                                        (member (%effect-node-name e) names1))
                                      effs2)))
         ;; Prefer row2's row-var if present, else row1's
         (rv (or (type-effect-row-row-var row2)
                 (type-effect-row-row-var row1))))
    (make-type-effect-row :effects new-effs :row-var rv)))

(defun effect-row-subset-p (row1 row2)
  "True iff every effect in ROW1 appears in ROW2 (by name).
Open row2 (with a row-var) is treated as a superset of everything."
  (when (type-effect-row-row-var row2)
    (return-from effect-row-subset-p t))
  (let ((names2 (mapcar #'%effect-node-name (type-effect-row-effects row2))))
    (every (lambda (e)
             (member (%effect-node-name e) names2))
           (type-effect-row-effects row1))))

;;; NOTE: infer-effects, infer-with-effects, check-body-effects are defined
;;;       in inference.lisp which loads later with full AST-walker implementations.
