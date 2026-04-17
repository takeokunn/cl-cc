(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; LOOP — Data Layer
;;;
;;; This file is the "Prolog database" for the LOOP macro: all grammar tables,
;;; keyword sets, and emitter dispatch tables are declared here as *vars*.
;;; No logic lives here — only facts.  Every other LOOP file reads this data.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; ── Keyword sets ─────────────────────────────────────────────────────────

;; Boundary keywords: mark the end of multi-form DO/INITIALLY/FINALLY bodies.
(defvar *loop-boundary-keywords*
  '("FOR" "COLLECT" "COLLECTING" "SUM" "SUMMING" "COUNT" "COUNTING"
    "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING"
    "WHILE" "UNTIL" "WHEN" "IF" "UNLESS" "DO" "INITIALLY" "FINALLY" "END"
    "REPEAT" "WITH" "APPEND" "APPENDING" "NCONC" "NCONCING"
    "BEING" "USING" "=" "ALWAYS" "NEVER" "THEREIS" "BY" "INTO"
    "NAMED"))

;; Condition types that return T when the loop body never executes (vacuous truth).
;; ALWAYS and NEVER hold trivially when there is no element to violate them.
(defvar *loop-vacuous-truth-conditions* '(:always :never))

;;; ── Accumulation grammar ─────────────────────────────────────────────────

;; Maps each accumulation keyword string to its canonical type keyword.
;; Both the canonical name and the -ING synonym map to the same type.
;; Rule derivation: one entry here → one emitter + one clause handler.
(defvar *loop-accum-keyword-table*
  '(("COLLECT"   . :collect)  ("COLLECTING"  . :collect)
    ("SUM"        . :sum)      ("SUMMING"     . :sum)
    ("COUNT"      . :count)    ("COUNTING"    . :count)
    ("MAXIMIZE"   . :maximize) ("MAXIMIZING"  . :maximize)
    ("MINIMIZE"   . :minimize) ("MINIMIZING"  . :minimize)
    ("APPEND"     . :append)   ("APPENDING"   . :append)
    ("NCONC"      . :nconc)    ("NCONCING"    . :nconc)))

;;; ── BEING THE sub-keyword grammar ────────────────────────────────────────

;; Maps HASH-KEYS/HASH-KEY/HASH-VALUES/HASH-VALUE strings to iteration types.
(defvar *loop-hash-iter-keywords*
  '(("HASH-KEYS"   . :hash-keys) ("HASH-KEY"    . :hash-keys)
    ("HASH-VALUES" . :hash-values) ("HASH-VALUE" . :hash-values)))

;; Maps USING sub-keywords to their semantic roles.
(defvar *loop-using-keywords*
  '(("HASH-VALUE" . :hash-value)
    ("HASH-KEY"   . :hash-key)))

;;; ── Emitter dispatch tables ───────────────────────────────────────────────
;;;
;;; These hash tables are populated by the define-loop-*-emitter macros in
;;; loop-emitters.lisp.  They are declared here so the parser layer can
;;; reference them without depending on the emitter layer at read time.

(defvar *loop-iter-emitters* (make-hash-table :test 'eq)
  "Dispatch: iteration-type → (lambda (var iter)) → (values binds ends pre steps).")

(defvar *loop-acc-emitters* (make-hash-table :test 'eq)
  "Dispatch: accumulation-type → (lambda (acc-var form binds result into)) → (values body binds result).")

(defvar *loop-condition-emitters* (make-hash-table :test 'eq)
  "Dispatch: condition-type → (lambda (form end-tag)) → tagbody form.")

) ; end eval-when (data layer)
