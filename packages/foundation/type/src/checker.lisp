;;;; checker.lisp - Bidirectional Type Checker
;;;;
;;;; Wraps inference.lisp's check/synthesize with explicit bidirectional
;;;; semantics.  All public names are exported from the cl-cc/type package.
;;;;
;;;; Public API:
;;;;   synthesize (ast env)                 — synthesis mode (bottom-up)
;;;;   check      (ast expected-type env)   — checking mode  (top-down)
;;;;   check-body (forms expected-type env) — check last form in sequence
;;;;   check-skolem-escape (skolem subst)   — verify no skolem escape
;;;;   skolem-appears-in-type-p (skolem ty) — helper predicate
;;;;
;;;; Canonical rigid-variable API:
;;;;   type-rigid-p            — predicate for rigid variables
;;;;   type-rigid-id           — accessor
;;;;   type-rigid-name         — accessor
;;;;   type-rigid-equal-p      — equality predicate
;;;;   fresh-rigid-var         — constructor
;;;;
;;;; Note: the actual implementations of synthesize, check, check-body,
;;;; check-skolem-escape, and skolem-appears-in-type-p live in inference.lisp
;;;; to avoid circular dependencies.  This file documents the module boundary
;;;; and re-exports / re-documents the interface.
;;;;
;;;; Legacy skolem names are thin wrappers over the canonical rigid-variable
;;;; representation and should not be used by new code.

(in-package :cl-cc/type)

;;; All the substantive implementations are in inference.lisp.
;;; This file exists as the "checker" module entry-point; it verifies
;;; at compile-time that the expected symbols are defined and provides
;;; any checker-specific helpers not appropriate for inference.lisp.

;;; ─── check-skolem-escape (documented re-export) ───────────────────────────
;;;
;;; Defined in inference.lisp:
;;;   (defun check-skolem-escape (skolem subst) ...)
;;;
;;; Signals type-inference-error if SKOLEM appears free in SUBST's range.

;;; ─── skolem-appears-in-type-p (documented re-export) ─────────────────────
;;;
;;; Defined in inference.lisp:
;;;   (defun skolem-appears-in-type-p (skolem ty) ...)

;;; ─── rigid variable summary ───────────────────────────────────────────────
;;;
;;; Canonical representation lives in types-core.lisp:
;;;
;;;   (defstruct type-rigid ...)         — rigid variable with :id, :name
;;;   (defun fresh-rigid-var ...)        — constructor (generates unique ID)
;;;   (defun type-rigid-equal-p ...)     — equality by ID

;;; ─── check-body-effects thin wrapper ─────────────────────────────────────
;;;
;;; Defined in inference.lisp; re-documented here for discoverability.
;;;
;;; (defun check-body-effects (body-forms expected-effects env) ...)
;;; Checks that the union of effects in BODY-FORMS ⊆ EXPECTED-EFFECTS.

;;; ─── Sanity check: ensure critical functions are defined ──────────────────
;;;
;;; Evaluated at load time to catch missing definitions early.

(eval-when (:load-toplevel :execute)
  (assert (fboundp 'synthesize)
          nil
          "checker.lisp: synthesize must be defined in inference.lisp")
  (assert (fboundp 'check)
          nil
          "checker.lisp: check must be defined in inference.lisp")
  (assert (fboundp 'check-body)
          nil
          "checker.lisp: check-body must be defined in inference.lisp")
  (assert (fboundp 'check-skolem-escape)
          nil
          "checker.lisp: check-skolem-escape must be defined in inference.lisp")
  (assert (fboundp 'skolem-appears-in-type-p)
          nil
          "checker.lisp: skolem-appears-in-type-p must be defined in inference.lisp")
  (assert (fboundp 'fresh-rigid-var)
          nil
          "checker.lisp: fresh-rigid-var must be defined in types-core.lisp"))

(defun checker-interface-ready-p ()
  "Return T when the bidirectional checker boundary is fully wired.
This gives the checker entry module an explicit, testable contract instead of
relying only on load-time assertions."
  (every #'fboundp
         '(synthesize
           check
           check-body
           check-skolem-escape
           skolem-appears-in-type-p
           fresh-rigid-var)))
