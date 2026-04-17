;;;; packages/type/type/src/exhaustiveness.lisp — FR-1903: Pattern Exhaustiveness / Coverage Checking
;;;;
;;;; Checks whether typecase/case/our-typecase arms are exhaustive and warns
;;;; about unreachable (subsumed) patterns.
;;;;
;;;; API:
;;;;   check-typecase-exhaustiveness (arm-types)
;;;;     → (values exhaustive-p unreachable-indices warnings)
;;;;
;;;;   typecase-arm-subsumed-p (type-spec covered-before)
;;;;     → T if type-spec is subsumed by any type already in covered-before
;;;;
;;;; Maranget (2007). "Warnings for Pattern Matching" — the canonical reference
;;;; for efficient exhaustiveness/usefulness checking.

(in-package :cl-cc/type)

;;; ─── Subsumption predicate ────────────────────────────────────────────────

(defun typecase-arm-subsumed-p (arm-type already-covered)
  "Return T if ARM-TYPE is completely covered by the types in ALREADY-COVERED.
   ARM-TYPE and each element of ALREADY-COVERED are type name symbols (or T).
   Uses the nominal subtype hierarchy from subtyping.lisp."
  (some (lambda (covered)
          (or (eq covered t)           ; catch-all covers everything
              (eq covered arm-type)    ; exact duplicate
              ;; arm-type <: covered (covered subsumes arm-type)
              (and (symbolp arm-type)
                   (symbolp covered)
                   (type-name-subtype-p arm-type covered))))
        already-covered))

;;; ─── Main exhaustiveness checker ─────────────────────────────────────────

(defun check-typecase-exhaustiveness (arm-types)
  "Check typecase coverage given ARM-TYPES — a list of type name symbols in order.
   The symbol T represents the catch-all (otherwise) arm.

   Returns:
     exhaustive-p        — T if the match is exhaustive (has T or covers all)
     unreachable-indices — 0-based indices of arms subsumed by earlier arms
     warnings            — list of human-readable warning strings"
  (let ((exhaustive-p nil)
        (unreachable-indices nil)
        (warnings nil)
        (covered nil))
    (loop for arm in arm-types
          for idx from 0
          do (cond
               ;; T / otherwise catch-all
               ((or (eq arm t) (eq arm 'otherwise))
                (when (typecase-arm-subsumed-p arm covered)
                  (push idx unreachable-indices)
                  (push (format nil "arm ~D (t/otherwise): unreachable — all cases already handled" idx)
                        warnings))
                (setf exhaustive-p t)
                (push t covered))
               ;; Normal type specifier
               (t
                (if (typecase-arm-subsumed-p arm covered)
                    (progn
                      (push idx unreachable-indices)
                      (push (format nil "arm ~D (~A): unreachable — subsumed by earlier arm" idx arm)
                            warnings))
                    (push arm covered)))))
    (values exhaustive-p
            (nreverse unreachable-indices)
            (nreverse warnings))))

;;; ─── etypecase completeness check ────────────────────────────────────────

(defun check-etypecase-completeness (arm-types)
  "Like check-typecase-exhaustiveness but also warns when there is no T arm
   (etypecase semantics: signals an error at runtime if no arm matches).
   Returns (values exhaustive-p unreachable-indices warnings)."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (check-typecase-exhaustiveness arm-types)
    (unless exhaustive-p
      (push "etypecase: no otherwise/t arm — will signal an error if no arm matches at runtime"
            warnings))
    (values exhaustive-p unreachable (nreverse warnings))))

;;; ─── Arm usefulness check ─────────────────────────────────────────────────

(defun useful-typecase-arms (arm-types)
  "Return the subset of ARM-TYPES that are useful (not subsumed by earlier arms).
   Arms are useful if they handle at least one case not already covered."
  (let ((covered nil)
        (useful nil))
    (dolist (arm arm-types)
      (unless (typecase-arm-subsumed-p arm covered)
        (push arm useful)
        (push arm covered)))
    (nreverse useful)))

;;; ─── Exports ──────────────────────────────────────────────────────────────

(export '(check-typecase-exhaustiveness
          check-etypecase-completeness
          useful-typecase-arms
          typecase-arm-subsumed-p))
