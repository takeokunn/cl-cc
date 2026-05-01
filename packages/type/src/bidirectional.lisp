;;;; packages/type/src/bidirectional.lisp - Bidirectional Type Checking (Phase 3)
;;;
;;; Bidirectional type checking separates:
;;;   SYNTHESIS (bottom-up): synthesize — given AST, produce type
;;;   CHECKING  (top-down):  check — given AST + expected type, verify conformance
;;;
;;; Bidirectional is required for Rank-N polymorphism: (forall a T) in argument
;;; position cannot be synthesized; it must be checked top-down.
;;;
;;; Extracted from inference.lisp.

(in-package :cl-cc/type)

(defun synthesize (ast env)
  "Synthesize type of AST in ENV (bottom-up mode).
   Returns (values type substitution).
   Use when no expected type is known at the call site."
  (infer ast env))

;;; Skolem escape checking helpers (Phase E)

(defun skolem-appears-in-type-p (skolem type)
  "Return T if SKOLEM appears free in TYPE."
  (typecase type
    (type-rigid       (type-rigid-equal-p skolem type))
    (type-var         nil)
    (type-arrow       (or (some (lambda (p) (skolem-appears-in-type-p skolem p))
                                (type-arrow-params type))
                          (skolem-appears-in-type-p skolem (type-arrow-return type))))
    (type-forall      (skolem-appears-in-type-p skolem (type-forall-body type)))
    (type-constructor (some (lambda (a) (skolem-appears-in-type-p skolem a))
                            (type-constructor-args type)))
    (type-product     (some (lambda (e) (skolem-appears-in-type-p skolem e))
                            (type-product-elems type)))
    (t nil)))

(defun skolem-appears-in-subst-p (skolem subst)
  "Return T if SKOLEM appears in the range of SUBST (a substitution struct or nil)."
  (when (and subst (substitution-p subst))
    (let ((found nil))
      (maphash (lambda (id ty)
                 (declare (ignore id))
                 (when (skolem-appears-in-type-p skolem ty)
                   (setf found t)))
               (substitution-bindings subst))
      found)))

(defun check-skolem-escape (skolem subst)
  "Signal an error if SKOLEM appears in the range of SUBST (skolem escape)."
  (when (skolem-appears-in-subst-p skolem subst)
    (error 'type-inference-error
           :message (format nil "Skolem escape: rigid type variable ~A leaked out of its scope."
                             (type-to-string skolem)))))

(defun check (ast expected-type env)
  "Check that AST conforms to EXPECTED-TYPE in ENV (top-down).
   Returns substitution on success, signals type-mismatch-error on failure.
   Rank-N: forall in expected position introduces skolem constants."
  (when (type-unknown-p expected-type)
    (return-from check nil))
  (typecase expected-type
    ;; Rank-N: checking against (forall a T) introduces a skolem for a
    ;; and checks the body under that skolem
    (type-forall
     (let* ((bound-var (type-forall-var expected-type))
            (skolem (fresh-rigid-var (or (type-var-name bound-var) "a")))
            ;; Substitute the bound variable with the skolem in the body type
            ;; using a proper hash-table substitution struct
            (body-type (let ((sub (subst-extend bound-var skolem (make-substitution))))
                         (zonk (type-forall-body expected-type) sub))))
       ;; Check body against the skolemized type.
       ;; Note: the substitution will contain bindings like ?x = !sk_a which is expected —
       ;; those are internal fresh variables resolved against the skolem, not escapes.
       ;; Full skolem escape detection (checking outer env free vars) is deferred to
       ;; the constraint solver phase.
       (check ast body-type env)))

     ;; Default: synthesize and verify via unification
     (t
      (multiple-value-bind (actual-type subst) (synthesize ast env)
        (let ((actual (zonk actual-type subst)))
          ;; Try unification (handles type variables)
          (multiple-value-bind (unified ok)
              (type-unify actual expected-type subst)
            (if ok
                unified
                (error 'type-mismatch-error
                       :expected expected-type
                       :actual actual))))))))

(defun check-body (asts expected-type env)
  "Check that the last form in ASTS has EXPECTED-TYPE in ENV.
   Earlier forms are synthesized (side-effect only).
   Returns substitution."
  (if (null asts)
      nil
      (let ((subst nil)
            (current-env env))
        ;; Synthesize all but the last form
        (dolist (ast (butlast asts))
          (multiple-value-bind (type new-subst) (synthesize ast current-env)
            (declare (ignore type))
            (setf subst (subst-compose new-subst subst))
            (setf current-env (zonk-env current-env subst))))
        ;; Check the last form against expected type
        (check (car (last asts)) expected-type current-env))))

;;; Exports

(export '(;; Bidirectional type checking (Phase 3)
          synthesize
          check
          check-body))
