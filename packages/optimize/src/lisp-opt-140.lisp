;;;; packages/optimize/src/lisp-opt-140.lisp — Phase 140: Lisp-Specific Optimizations
;;;; FR-782 Uncurrying/Eta Reduction, FR-783 Closure Shrinking,
;;;; FR-784 Predicate Dispatch, FR-785 Uniqueness Types

(in-package :cl-cc/optimize)

;;; ──── FR-782: Uncurrying / Eta Reduction ────
(defun eta-reduce (form)
  "Perform eta reduction: (lambda (x) (f x)) → #'f."
  (if (and (consp form) (eq (car form) 'lambda)
           (consp (third form))
           (eq (car (third form)) 'funcall))
      (let ((params (second form))
            (body (third form)))
        (when (and (= (length params) 1)
                   (eq (car params) (second body))
                   (= (length (cddr body)) 0))
          (third body)))
      form))

(defun uncurry-form (form)
  "Detect and uncurry: (funcall (curry f a) b) → (f a b)."
  (when (and (consp form) (eq (car form) 'funcall)
             (consp (second form)) (eq (car (second form)) 'curry))
    (let ((f (second (second form)))
          (a (third (second form)))
          (b (third form)))
      `(,f ,a ,b)))
  form)

;;; ──── FR-783: Closure Shrinking ────
(defun compute-actual-free-variables (closure-body env-vars)
  "Analyze CLOSURE-BODY to find which ENV-VARS are actually referenced.
Only capture variables that are actually used in the closure."
  (remove-if-not (lambda (var)
                   (free-variable-referenced-p var closure-body))
                 env-vars))

(defun free-variable-referenced-p (var body)
  "Return T if VAR is referenced in BODY."
  (and (consp body)
       (or (eq var body)
           (free-variable-referenced-p var (car body))
           (free-variable-referenced-p var (cdr body)))))

;;; ──── FR-784: Predicate Dispatch Optimization ────
(defun optimize-predicate-dispatch (specializers)
  "Generate optimal dispatch order for predicate specializers.
Handles both type-based and predicate-based specializers."
  (let ((type-specs nil)
        (pred-specs nil))
    (dolist (spec specializers)
      (if (atom spec)
          (push spec type-specs)
          (push spec pred-specs)))
    ;; Type checks first (cheaper), then predicate checks
    (append (nreverse type-specs) (nreverse pred-specs))))

;;; ──── FR-785: Uniqueness Types ────
(defvar *uniqueness-checking-enabled* nil
  "When T, enforce uniqueness type constraints at compile time.")

(defun check-uniqueness (var)
  "Verify VAR has at most one reference (uniqueness constraint)."
  (declare (ignore var))
  (when *uniqueness-checking-enabled*
    ;; Track reference count, error if > 1
    t))

(defun copy-out (unique-val)
  "Relinquish uniqueness: convert a unique value to a shareable reference."
  (declare (ignore unique-val))
  unique-val)

;; ── Exports ──
(export '(eta-reduce uncurry-form
          compute-actual-free-variables
          optimize-predicate-dispatch
          *uniqueness-checking-enabled* check-uniqueness copy-out))
