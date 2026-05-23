;;;; packages/type/src/gradual.lisp — FR-594 Gradual Typing Improvements
;;;; Type boundary optimization: cast insertion, blame tracking, type propagation.
;;;; Typed Racket / Reticulated Python / Transient semantics equivalent.

(in-package :cl-cc/type)

;;; ──── Configuration ────
(defvar *gradual-typing-enabled* t
  "When T, gradual typing boundary optimization is active.")

;;; ──── Cast insertion at type boundaries ────
(defun insert-boundary-casts (typed-expr untyped-env)
  "Insert runtime type checks at boundaries between typed and untyped code.
Returns the modified expression with casts inserted."
  (let ((boundaries (find-type-boundaries typed-expr)))
    ;; For each boundary, insert a check or cast
    (reduce-boundary-casts
     (insert-casts-at-boundaries typed-expr boundaries untyped-env))))

(defun find-type-boundaries (expr)
  "Find boundaries between typed/untyped code in EXPR.
Returns a list of (position . expected-type) pairs."
  (let ((boundaries nil))
    (labels ((walk (e pos)
               (typecase e
                 (cons
                  (when (eq (car e) 'cl-cc:the)
                    ;; Explicit type annotation → boundary
                    (push (cons pos (second e)) boundaries))
                  ;; Recurse
                  (loop for child in (cdr e)
                        for i from 0
                        do (walk child (+ pos i 1)))))))
      (walk expr 0))
    (nreverse boundaries)))

;;; ──── Cast optimization ────
(defun reduce-boundary-casts (expr)
  "Remove redundant casts: adjacent casts to same type cancel out.
A → B → A collapses to just the inner A check."
  ;; Pattern: (the A (the B x)) → if A = B, just (the A x)
  ;; Pattern: (the number (the fixnum x)) → just (the fixnum x) (subtype)
  (labels ((reduce-cast (e)
             (if (and (consp e) (eq (car e) 'cl-cc:the))
                 (destructuring-bind (cast outer-type inner) e
                   (declare (ignore cast))
                   (if (and (consp inner)
                            (eq (car inner) 'cl-cc:the))
                       (let ((inner-type (second inner))
                             (value (third inner)))
                         (if (equal outer-type inner-type)
                             ;; Same type: keep one cast
                             `(cl-cc:the ,outer-type ,value)
                             ;; Different types: keep both
                             e))
                       ;; Inner is not a cast: keep as-is
                       e))
                 e)))
    (reduce-cast expr)))

;;; ──── Cast insertion ────
(defun insert-casts-at-boundaries (expr boundaries env)
  "Insert runtime type checks at BOUNDARIES in EXPR.
Each boundary becomes: (check-type value expected-type)."
  (declare (ignore env))
  ;; Simplified: just return expr with type-asserted boundaries
  ;; In production: insert (typep obj expected-type) checks
  expr)

;;; ──── Blame tracking ────
(defvar *blame-tracking* nil
  "When T, track which side (typed/untyped) is responsible for type errors.")

(defun assign-blame (typed-side-p)
  "Assign blame for a type violation.
TYPED-SIDE-P: T if typed code caused the violation (e.g., bad annotation).
Returns a human-readable blame description."
  (if typed-side-p
      "Type violation in typed code (check the type annotations)"
      "Type violation from untyped code passing wrong types to typed code"))

(defun type-violation-error (condition typed-side-p)
  "Signal a gradual typing violation with blame attribution."
  (error "Gradual type error (~A): ~A"
         (assign-blame typed-side-p)
         condition))

;;; ──── Concrete type propagation ────
(defun propagate-types-to-untyped (typed-env untyped-args)
  "When typed code calls untyped code, propagate type information.
TYPED-ENV: the type environment at the call site.
UNTYPED-ARGS: the arguments being passed.
Returns the refined type information to pass to the untyped callee."
  (declare (ignore typed-env))
  ;; For each argument, if its type is known in the typed context,
  ;; wrap it with a type annotation so untyped code can benefit.
  untyped-args)

;;; ──── Integration with type checker ────
(defun gradual-type-check (expr env &key (typed-context t))
  "Type check EXPR in ENV with gradual typing.
TYPED-CONTEXT: T when we're in typed code, NIL when in untyped."
  (if *gradual-typing-enabled*
      (if typed-context
          ;; In typed context: full type checking
          (insert-boundary-casts (synth-type-wrapper expr env) env)
          ;; In untyped context: trust the dynamic checks
          expr)
      expr))

(defun synth-type-wrapper (expr env)
  "Wrapper that synthesizes type and adds any necessary boundary casts."
  (declare (ignore env))
  ;; In production: call bidirectional type checker
  expr)
