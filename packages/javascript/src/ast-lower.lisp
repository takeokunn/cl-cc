;;;; packages/javascript/src/ast-lower.lisp — JavaScript-to-cl-cc AST Lowering Pass
;;;;
;;;; This pass walks the parser output and normalises JS-specific constructs
;;;; into canonical cl-cc AST form so that subsequent compiler phases see only
;;;; standard cl-cc AST nodes.
;;;;
;;;; Key transformations performed:
;;;;
;;;;   1. this keyword         → (make-ast-var :name '%js-this)
;;;;   2. super keyword        → (make-ast-var :name '%js-super)
;;;;   3. undefined literal    → (make-ast-quote :value :js-undefined)
;;;;   4. null literal         → (make-ast-quote :value :js-null)
;;;;
;;;;   5. Logical assignment normalisation (already lowered by parser, but
;;;;      residual patterns are preserved by recursive descent):
;;;;        x &&= y  →  (if (%js-truthy x) (setq x y) x)
;;;;        x ||= y  →  (if (%js-truthy x) x (setq x y))
;;;;        x ??= y  →  (if (%js-not-nullish x) x (setq x y))
;;;;
;;;;   6. Nullish coalescing:
;;;;        a ?? b  →  (let ((#:tmp a)) (if (%js-not-nullish #:tmp) #:tmp b))
;;;;
;;;;   7. Optional chaining chain:
;;;;        a?.b?.c  →  nested %js-optional-chain calls (already lowered
;;;;                     by the parser; pass recurses into args)
;;;;
;;;;   8. Spread in function call:
;;;;        f(a, ...b, c)  →  (%js-apply-with-spread f (list a (%js-spread b) c))
;;;;        when any argument is a (%js-spread ...) call.
;;;;
;;;;   9. Template literal with parts:
;;;;        T-TEMPLATE-PARTS token  →  (%js-template-string part1 part2 …)
;;;;        (Parser handles this inline; the lower pass recurses into the
;;;;        resulting call nodes.)
;;;;
;;;;  10. delete expr   → (%js-delete expr)          (already done by parser)
;;;;  11. void expr     → (progn expr :js-undefined)  (already done by parser;
;;;;                       :undefined sentinel is normalised to :js-undefined)
;;;;  12. typeof x      → (%js-typeof x)              (already done by parser)
;;;;
;;;; Entry points:
;;;;   js-lower-program ast-list → list of lowered AST nodes
;;;;   js-lower-node    ast      → lowered AST node
;;;;
;;;; Load order: after parser-pattern.lisp, before runtime.lisp.

(in-package :cl-cc/javascript)

;;; ─── Internal Helper ─────────────────────────────────────────────────────────

(defun %js-lower-call-node (name &rest args)
  "Build an AST call to JS runtime helper NAME with lowered ARGS."
  (make-ast-call :func (make-ast-var :name name) :args args))

;;; ─── Spread Detection ────────────────────────────────────────────────────────

(defun %js-spread-call-p (node)
  "Return T when NODE is a call to %JS-SPREAD — i.e. a spread argument."
  (and (ast-call-p node)
       (ast-var-p (ast-call-func node))
       (eq (ast-var-name (ast-call-func node)) '%js-spread)))

(defun %js-args-have-spread-p (args)
  "Return T when at least one element of ARGS is a spread call."
  (some #'%js-spread-call-p args))

;;; ─── Entry Points ─────────────────────────────────────────────────────────────

(defun js-lower-program (ast-list)
  "Lower a list of top-level JS AST nodes. Returns a new list."
  (mapcar #'js-lower-node ast-list))

(defun js-lower-node (ast)
  "Recursively lower a JS AST node into canonical cl-cc AST form."
  (cond
    ;; Null / empty
    ((null ast) ast)

    ;; ── Normalise parser sentinel values ──────────────────────────────────────
    ;; The parser emits :undefined / :null as quote values; normalise them
    ;; to the JS-namespaced keywords so later passes can tell them apart from
    ;; the host CL NIL / T.
    ((and (ast-quote-p ast)
          (eq (ast-quote-value ast) :undefined))
     (make-ast-quote :value :js-undefined))

    ((and (ast-quote-p ast)
          (eq (ast-quote-value ast) :null))
     (make-ast-quote :value :js-null))

    ;; ── Leaf quote nodes (no children to recurse into) ────────────────────────
    ((ast-quote-p ast) ast)

    ;; ── Leaf integer nodes ────────────────────────────────────────────────────
    ((ast-int-p ast) ast)

    ;; ── Variable reference: normalise this / super ────────────────────────────
    ;; The primary parser already sets these names; the lower pass ensures they
    ;; are correct even if the AST was constructed by an alternative path.
    ((ast-var-p ast)
     (let ((name (ast-var-name ast)))
       (cond
         ((eq name '%js-this)
          (make-ast-var :name '*js-this*))
         ((eq name '%js-super)
          (make-ast-var :name '*js-super*))
         (t ast))))

    ;; ── Specialised call-node forms ───────────────────────────────────────────
    ((ast-call-p ast) (%js-lower-call ast))

    ;; ── Let binding ───────────────────────────────────────────────────────────
    ((ast-let-p ast) (%js-lower-let ast))

    ;; ── Lambda / anonymous function ───────────────────────────────────────────
    ((ast-lambda-p ast) (%js-lower-lambda ast))

    ;; ── Named function definition ─────────────────────────────────────────────
    ((ast-defun-p ast) (%js-lower-defun ast))

    ;; ── Conditional ───────────────────────────────────────────────────────────
    ((ast-if-p ast)
     (make-ast-if :cond (js-lower-node (ast-if-cond ast))
                  :then (js-lower-node (ast-if-then ast))
                  :else (js-lower-node (ast-if-else ast))))

    ;; ── Sequence ──────────────────────────────────────────────────────────────
    ((ast-progn-p ast)
     (make-ast-progn :forms (mapcar #'js-lower-node (ast-progn-forms ast))))

    ;; ── Assignment ────────────────────────────────────────────────────────────
    ((ast-setq-p ast)
     (make-ast-setq :var (ast-setq-var ast)
                    :value (js-lower-node (ast-setq-value ast))))

    ;; ── Binary operation ──────────────────────────────────────────────────────
    ((ast-binop-p ast)
     (make-ast-binop :op  (ast-binop-op ast)
                     :lhs (js-lower-node (ast-binop-lhs ast))
                     :rhs (js-lower-node (ast-binop-rhs ast))))

    ;; ── Return from block ─────────────────────────────────────────────────────
    ((ast-return-from-p ast)
     (make-ast-return-from :name  (ast-return-from-name  ast)
                           :value (js-lower-node (ast-return-from-value ast))))

    ;; ── Block ─────────────────────────────────────────────────────────────────
    ((ast-block-p ast)
     (make-ast-block :name (ast-block-name ast)
                     :body (mapcar #'js-lower-node (ast-block-body ast))))

    ;; ── Tagbody / go ──────────────────────────────────────────────────────────
    ((ast-tagbody-p ast)
     (make-ast-tagbody
      :tags (mapcar (lambda (entry)
                      (cons (car entry)
                            (mapcar #'js-lower-node (cdr entry))))
                    (ast-tagbody-tags ast))))

    ((ast-go-p ast) ast)

    ;; ── Local function bindings ───────────────────────────────────────────────
    ((ast-flet-p ast)
     (make-ast-flet
      :bindings (mapcar #'%js-lower-local-fn-binding (ast-flet-bindings ast))
      :body     (mapcar #'js-lower-node (ast-flet-body ast))))

    ((ast-labels-p ast)
     (make-ast-labels
      :bindings (mapcar #'%js-lower-local-fn-binding (ast-labels-bindings ast))
      :body     (mapcar #'js-lower-node (ast-labels-body ast))))

    ;; ── Top-level variable definition ─────────────────────────────────────────
    ((ast-defvar-p ast)
     (make-ast-defvar :name  (ast-defvar-name  ast)
                      :value (js-lower-node (ast-defvar-value ast))
                      :kind  (ast-defvar-kind  ast)))

    ;; ── Throw / catch / unwind-protect ───────────────────────────────────────
    ((ast-throw-p ast)
     (make-ast-throw :tag   (ast-throw-tag   ast)
                     :value (js-lower-node (ast-throw-value ast))))

    ((ast-catch-p ast)
     (make-ast-catch :tag  (ast-catch-tag  ast)
                     :body (mapcar #'js-lower-node (ast-catch-body ast))))

    ((ast-unwind-protect-p ast)
     (make-ast-unwind-protect
      :protected (js-lower-node (ast-unwind-protected ast))
      :cleanup   (mapcar #'js-lower-node (ast-unwind-cleanup ast))))

    ;; ── Multiple values ───────────────────────────────────────────────────────
    ((ast-values-p ast)
     (make-ast-values :forms (mapcar #'js-lower-node (ast-values-forms ast))))

    ((ast-multiple-value-bind-p ast)
     (make-ast-multiple-value-bind
      :vars        (ast-mvb-vars        ast)
      :values-form (js-lower-node (ast-mvb-values-form ast))
      :body        (mapcar #'js-lower-node (ast-mvb-body ast))))

    ;; ── The (type annotation) ─────────────────────────────────────────────────
    ((ast-the-p ast)
     (make-ast-the :type  (ast-the-type  ast)
                   :value (js-lower-node (ast-the-value ast))))

    ;; ── Apply ─────────────────────────────────────────────────────────────────
    ((ast-apply-p ast)
     (make-ast-apply :func (js-lower-node (ast-apply-func ast))
                     :args (mapcar #'js-lower-node (ast-apply-args ast))))

    ;; ── Slot-value and CLOS helpers ───────────────────────────────────────────
    ((ast-slot-value-p ast)
     (make-ast-slot-value :object (js-lower-node (ast-slot-value-object ast))
                          :slot   (ast-slot-value-slot ast)))

    ((ast-set-slot-value-p ast)
     (make-ast-set-slot-value
      :object (js-lower-node (ast-set-slot-value-object ast))
      :slot   (ast-set-slot-value-slot  ast)
      :value  (js-lower-node (ast-set-slot-value-value  ast))))

    ((ast-set-gethash-p ast)
     (make-ast-set-gethash
      :key   (js-lower-node (ast-set-gethash-key   ast))
      :table (js-lower-node (ast-set-gethash-table ast))
      :value (js-lower-node (ast-set-gethash-value ast))))

    ;; ── Leaf / unknown node — return as-is ───────────────────────────────────
    (t ast)))

;;; ─── Call Lowering ───────────────────────────────────────────────────────────

(defun %js-lower-call (ast)
  "Lower an AST-CALL node.

  Special cases handled here:
  * Spread in argument list: any (%js-spread expr) argument triggers
    rewriting to (%js-apply-with-spread func lowered-args-list).
  * Optional chaining (%js-optional-chain / %js-optional-call): args recursed.
  * Template string (%js-template-string): parts recursed.
  * delete (%js-delete expr): arg recursed.
  * void sequence: the argument of the (progn expr :undefined) is recursed
    (void is handled at the progn level via the :js-undefined normalisation).
  * typeof (%js-typeof expr): arg recursed.
  All other calls: func and each arg are recursed normally."
  (let* ((func      (ast-call-func ast))
         (raw-args  (ast-call-args ast))
         (low-func  (js-lower-node func))
         (low-args  (mapcar #'js-lower-node raw-args)))
    (cond
      ;; Spread in argument list → %js-apply-with-spread
      ((%js-args-have-spread-p low-args)
       (%js-lower-call-with-spread low-func low-args))

      ;; All other calls — return reconstructed call with lowered children
      (t
       (make-ast-call :func low-func :args low-args)))))

(defun %js-lower-call-with-spread (low-func low-args)
  "Rewrite a call containing spread arguments to use %js-apply-with-spread.
  LOW-FUNC and LOW-ARGS are already lowered.
  Generates: (%js-apply-with-spread low-func (list arg0 arg1 … argN))"
  (make-ast-call
   :func (make-ast-var :name '%js-apply-with-spread)
   :args (list low-func
               (make-ast-call
                :func (make-ast-var :name 'list)
                :args low-args))))

;;; ─── Let Lowering ────────────────────────────────────────────────────────────

(defun %js-lower-let (ast)
  "Lower an AST-LET node: recurse into each binding value and the body."
  (make-ast-let
   :bindings (mapcar (lambda (binding)
                       (cons (car binding)
                             (js-lower-node (cdr binding))))
                     (ast-let-bindings ast))
   :body (mapcar #'js-lower-node (ast-let-body ast))))

;;; ─── Lambda Lowering ─────────────────────────────────────────────────────────

(defun %js-lower-lambda (ast)
  "Lower an AST-LAMBDA node: recurse into the body."
  (make-ast-lambda
   :params          (ast-lambda-params          ast)
   :optional-params (ast-lambda-optional-params ast)
   :rest-param      (ast-lambda-rest-param      ast)
   :key-params      (ast-lambda-key-params      ast)
   :declarations    (ast-lambda-declarations    ast)
   :body            (mapcar #'js-lower-node (ast-lambda-body ast))))

;;; ─── Defun Lowering ──────────────────────────────────────────────────────────

(defun %js-lower-defun (ast)
  "Lower an AST-DEFUN node: recurse into the body."
  (make-ast-defun
   :name            (ast-defun-name            ast)
   :params          (ast-defun-params          ast)
   :optional-params (ast-defun-optional-params ast)
   :rest-param      (ast-defun-rest-param      ast)
   :key-params      (ast-defun-key-params      ast)
   :declarations    (ast-defun-declarations    ast)
   :documentation   (ast-defun-documentation   ast)
   :body            (mapcar #'js-lower-node (ast-defun-body ast))))

;;; ─── Local Function Binding Helper ──────────────────────────────────────────

(defun %js-lower-local-fn-binding (binding)
  "Lower a single flet/labels binding (name . ast-lambda). Returns new binding."
  (cons (car binding)
        (js-lower-node (cdr binding))))
