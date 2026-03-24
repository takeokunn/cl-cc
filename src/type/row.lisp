;;;; row.lisp - Row Polymorphism Operations
;;;;
;;;; Functional operations on type-record, type-variant, and type-effect-row.
;;;; All operations return fresh structs — they do not mutate their inputs.
;;;;
;;;; Public API:
;;;;   row-extend          — add label:type to a record
;;;;   row-restrict        — remove label from a record
;;;;   row-select          — get type for a label; nil if absent
;;;;   row-labels          — list all labels in a record or variant
;;;;   row-closed-p        — true iff row-var is nil
;;;;   row-open-p          — true iff row-var is non-nil
;;;;   effect-row-extend   — add an effect-op to an effect row
;;;;   effect-row-restrict — remove an effect by name from an effect row
;;;;   effect-row-member-p — test if an effect name is in an effect row

(in-package :cl-cc/type)

;;; ─── Record row operations ────────────────────────────────────────────────

(defun row-extend (label type row)
  "Return a new type-record with LABEL mapped to TYPE prepended to ROW's fields.
If LABEL already exists the new binding shadows the old one (functional override)."
  (make-type-record
   :fields  (acons label type (type-record-fields row))
   :row-var (type-record-row-var row)))

(defun row-restrict (label row)
  "Return a new type-record with LABEL removed from ROW's fields.
All occurrences of LABEL are removed (idempotent)."
  (make-type-record
   :fields  (remove-if (lambda (entry) (eq (car entry) label))
                       (type-record-fields row))
   :row-var (type-record-row-var row)))

(defun row-select (label row)
  "Return the type bound to LABEL in ROW, or nil if absent."
  (cond
    ((type-record-p row)
     (let ((entry (assoc label (type-record-fields row))))
       (when entry (cdr entry))))
    ((type-variant-p row)
     (let ((entry (assoc label (type-variant-cases row))))
       (when entry (cdr entry))))
    (t nil)))

(defun row-labels (row)
  "Return a list of all labels present in ROW.
Works for both type-record (uses fields) and type-variant (uses cases)."
  (cond
    ((type-record-p row)
     (mapcar #'car (type-record-fields row)))
    ((type-variant-p row)
     (mapcar #'car (type-variant-cases row)))
    (t nil)))

(defun row-closed-p (row)
  "True iff ROW is a closed row (row-var is nil).
Works for type-record, type-variant, and type-effect-row."
  (cond
    ((type-record-p row)      (null (type-record-row-var row)))
    ((type-variant-p row)     (null (type-variant-row-var row)))
    ((type-effect-row-p row)  (null (type-effect-row-row-var row)))
    (t t)))

(defun row-open-p (row)
  "True iff ROW is an open row (row-var is non-nil)."
  (not (row-closed-p row)))

;;; ─── Effect row operations ────────────────────────────────────────────────

(defun effect-row-extend (effect-op row)
  "Return a new type-effect-row with EFFECT-OP prepended.
EFFECT-OP must be a type-effect-op.
Does not deduplicate — callers that need deduplication should use
effect-row-union from effect.lisp."
  (make-type-effect-row
   :effects (cons effect-op (type-effect-row-effects row))
   :row-var (type-effect-row-row-var row)))

(defun effect-row-restrict (name row)
  "Return a new type-effect-row with all effects named NAME removed."
  (make-type-effect-row
   :effects (remove-if (lambda (e) (eq (type-effect-op-name e) name))
                       (type-effect-row-effects row))
   :row-var (type-effect-row-row-var row)))

(defun effect-row-member-p (name row)
  "True iff an effect named NAME appears in ROW's effects list."
  (and (type-effect-row-p row)
       (some (lambda (e) (eq (type-effect-op-name e) name))
             (type-effect-row-effects row))))
