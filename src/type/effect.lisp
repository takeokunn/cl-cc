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

(defvar *effect-signature-table*
  (make-hash-table :test #'eq)
  "Maps op-name symbol -> effect-name symbol.")

(defun register-effect-signature (op-name effect-name)
  "Register that OP-NAME belongs to effect EFFECT-NAME."
  (setf (gethash op-name *effect-signature-table*) effect-name)
  op-name)

(defun lookup-effect-signature (op-name)
  "Return the effect name that owns OP-NAME, or nil if unknown."
  (gethash op-name *effect-signature-table*))

;;; ─── Effect row operations ────────────────────────────────────────────────

;;; Helper: extract effect name from either type-effect-op (new) or type-effect
;;; (old backward-compat struct defined in typeclass.lisp which loads later).
;;; We defer to type-effect-name when it becomes available (typeclass.lisp
;;; defines a polymorphic type-effect-name).  At load time of this file,
;;; only type-effect-op-p is available, so we default to that path.
(defun %effect-node-name (e)
  "Return the effect name symbol from E, handling both type-effect-op and
old-style backward-compat effect objects.  After typeclass.lisp loads,
type-effect-name handles the dispatch; we call it if bound."
  (cond
    ((type-effect-op-p e) (type-effect-op-name e))
    ;; After typeclass.lisp loads, type-effect-name handles old-style nodes.
    ((fboundp 'type-effect-name) (funcall 'type-effect-name e))
    ;; Last resort: this shouldn't happen in a correctly ordered ASDF build.
    (t (error "Cannot get effect name from ~S" e))))

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

;;; ─── Effect inference ─────────────────────────────────────────────────────

(defun infer-effects (ast env)
  "Conservative effect inference for AST node AST in type environment ENV.
Returns a type-effect-row.

For ast-call nodes whose function name appears in *effect-signature-table*,
we return a singleton effect row for that effect.  All other nodes are
treated as pure for now — a full effect solver elaborates these later."
  (declare (ignore env))
  (let ((node-type (type-of ast)))
    (cond
      ;; For call nodes, check if the function is an effect operation.
      ;; We handle both the struct name used in the project and a plain list form.
      ((eq node-type 'cons)
       ;; Plain s-expression: (fn-name arg ...) — check fn-name
       (let ((fn (car ast)))
         (when (symbolp fn)
           (let ((eff-name (lookup-effect-signature fn)))
             (when eff-name
               (return-from infer-effects
                 (make-type-effect-row
                  :effects (list (make-type-effect-op :name eff-name))
                  :row-var nil))))))
       +pure-effect-row+)
      (t
       ;; For all other AST types, conservatively return pure.
       ;; Structural effect inference is performed by the constraint solver.
       +pure-effect-row+))))

(defun infer-with-effects (ast env)
  "Infer a type and effect row for AST in ENV.
Returns (values type effect-row).
The type is inferred via a fresh type variable (placeholder); a full
inference pass will unify it.  This function exists for the interface —
callers that need full inference should use the constraint solver."
  (declare (ignore ast env))
  (values (fresh-type-var "t") +pure-effect-row+))

(defun check-body-effects (body-forms expected-effects env)
  "Check that every form in BODY-FORMS produces effects ≤ EXPECTED-EFFECTS.
Signals type-mismatch-error if any body form's inferred effects
are not a subset of EXPECTED-EFFECTS."
  (dolist (form body-forms)
    (let ((actual (infer-effects form env)))
      (unless (effect-row-subset-p actual expected-effects)
        (error 'type-mismatch-error
               :message
               (format nil "Body has unexpected effects not in expected effect row"))))))
