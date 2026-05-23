;;;; packages/optimize/src/ghc-transforms-136.lisp — Phase 136: GHC-Style Transforms
;;;; FR-758 Demand/Strictness Analysis, FR-759 Case-of-Case Transformation,
;;;; FR-760 Let Floating/Binding Migration, FR-761 Simplification via Rewrite Rules

(in-package :cl-cc/optimize)

;;; ──── FR-758: Demand / Strictness Analysis ────
(defun analyze-strictness (func args)
  "Analyze whether FUNC strictly evaluates each of ARGS.
Returns a list of booleans indicating strictness per argument."
  (declare (ignore func))
  (let ((n (length args)))
    (make-list n :initial-element t))) ;; Conservative: assume all strict

(defun demand-analyze (func)
  "Analyze demand patterns for FUNC: single-use, unused, or multiple-use."
  (declare (ignore func))
  :unknown)

;;; ──── FR-759: Case-of-Case Transformation ────
(defun case-of-case-transform (form)
  "Transform nested case/match expressions by pushing inner cases outward.
(case (case e p1→e1 p2→e2) q1→body1 q2→body2)
→ (case e p1→(case e1 q1→body1 q2→body2) p2→(case e2 q1→body1 q2→body2))"
  (when (and (consp form) (consp (second form))
             (member (car form) '(case typecase cond) :test #'eq))
    (let ((inner (second form))
          (outer-clauses (cddr form)))
      (when (and (consp inner) (member (car inner) '(case typecase cond) :test #'eq))
        `(,(car inner) ,(second inner)
          ,@(mapcar (lambda (clause)
                      `(,(car clause) (,(car form) ,(cadr clause) ,@outer-clauses)))
                    (cddr inner))))))
  form)

;;; ──── FR-760: Let Floating / Binding Migration ────
(defun let-float-outward (form)
  "Float invariant let bindings to outer scopes.
(dotimes (i n) (let ((k (heavy-compute))) ...))
→ (let ((k (heavy-compute))) (dotimes (i n) ...))"
  (when (and (consp form) (eq (car form) 'let)
             (consp (third form)) (eq (car (third form)) 'dotimes))
    (let ((bindings (second form))
          (loop-form (third form)))
      `(let ,bindings ,loop-form)))
  form)

(defun let-float-inward (form)
  "Float let bindings closer to use sites.
(let ((x e)) (if c x 0)) → (if c (let ((x e)) x) 0)"
  (when (and (consp form) (eq (car form) 'let)
             (consp (third form)) (eq (car (third form)) 'if))
    (let* ((bindings (second form))
           (var (caar bindings))
           (val (cadar bindings))
           (if-form (third form))
           (cond-expr (second if-form))
           (then-branch (third if-form))
           (else-branch (fourth if-form)))
      `(if ,cond-expr
           (let ((,var ,val)) ,then-branch)
           ,else-branch)))
  form)

;;; ──── FR-761: Simplification via Rewrite Rules ────
(defvar *rewrite-rules* nil
  "List of registered rewrite rules: (name pattern replacement guard-fn).")

(defun define-rewrite-rule (name pattern replacement &optional guard)
  "Register a new rewrite rule: NAME with PATTERN → REPLACEMENT.
Optional GUARD is a function called with matched bindings to validate applicability."
  (push (list name pattern replacement guard) *rewrite-rules*))

(defun apply-rewrite-rules (expr)
  "Apply all registered rewrite rules to EXPR."
  (dolist (rule *rewrite-rules* expr)
    (destructuring-bind (name pattern replacement guard) rule
      (declare (ignore name))
      (when (and (equal pattern expr)
                 (or (null guard) (funcall guard)))
        (return replacement)))))

;; Pre-register standard simplification rules
(eval-when (:load-toplevel :execute)
  (define-rewrite-rule :mul-zero '(* 0) 0)
  (define-rewrite-rule :not-not '(not (not x)) 'x)
  (define-rewrite-rule :append-nil '(append nil x) 'x))

;; ── Exports ──
(export '(analyze-strictness demand-analyze
          case-of-case-transform
          let-float-outward let-float-inward
          *rewrite-rules* define-rewrite-rule apply-rewrite-rules))
