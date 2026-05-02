;;;; packages/compile/src/codegen-fold-optimize.lisp — AST constant-fold pass (optimize-ast)
;;;
;;; Contains:
;;;   - %loc         macro: inherit source location from an AST node
;;;   - optimize-ast top-level fold pass called from compile-toplevel-forms
;;;
;;; Depends on codegen-fold.lisp (%fold-ast-binop, %evaluate-ast, %compile-time-eval-depth-limit,
;;; *compile-time-value-env*, *compile-time-function-env*, %compile-time-value->ast).
;;; Load order: after codegen-fold, before codegen.

(in-package :cl-cc/compile)

;;; ── Source-location helpers ──────────────────────────────────────────────────

;;; This file reuses %clone-source from codegen-fold.lisp rather than defining a
;;; local macro wrapper.

;;; ── AST constant fold pass ───────────────────────────────────────────────────

(defparameter *optimize-ast-rebuilder-table*
  '((ast-progn       . %optimize-ast-progn-node)
    (ast-let         . %optimize-ast-let-node)
    (ast-if          . %optimize-ast-if-node)
    (ast-lambda      . %optimize-ast-lambda-node)
    (ast-defun       . %optimize-ast-defun-node)
    (ast-defvar      . %optimize-ast-defvar-node)
    (ast-block       . %optimize-ast-block-node)
    (ast-return-from . %optimize-ast-return-from-node)
    (ast-setq        . %optimize-ast-setq-node)
    (ast-the         . %optimize-ast-the-node))
  "Type symbol → rebuilder function symbol for optimize-ast's simple recursive cases.
Special folding nodes like ast-binop and ast-call stay in optimize-ast directly.")

(defun %optimize-ast-list (nodes)
  "Optimize every AST node in NODES, preserving list order."
  (mapcar #'optimize-ast nodes))

(defun %optimize-ast-binding-alist (bindings)
  "Optimize the value side of an alist of lexical BINDINGS."
  (mapcar (lambda (binding)
            (cons (car binding) (optimize-ast (cdr binding))))
          bindings))

(defun %optimize-ast-progn-node (node)
  (%clone-source node #'make-ast-progn
    :forms (%optimize-ast-list (ast-progn-forms node))))

(defun %optimize-ast-let-node (node)
  (%clone-source node #'make-ast-let
    :bindings     (%optimize-ast-binding-alist (ast-let-bindings node))
    :declarations (ast-let-declarations node)
    :body         (%optimize-ast-list (ast-let-body node))))

(defun %optimize-ast-if-node (node)
  (%clone-source node #'make-ast-if
    :cond (optimize-ast (ast-if-cond node))
    :then (optimize-ast (ast-if-then node))
    :else (optimize-ast (ast-if-else node))))

(defun %optimize-ast-lambda-node (node)
  (%clone-source node #'make-ast-lambda
    :params          (ast-lambda-params node)
    :optional-params (ast-lambda-optional-params node)
    :rest-param      (ast-lambda-rest-param node)
    :key-params      (ast-lambda-key-params node)
    :declarations    (ast-lambda-declarations node)
    :body            (%optimize-ast-list (ast-lambda-body node))
    :env             (ast-lambda-env node)))

(defun %optimize-ast-defun-node (node)
  (%clone-source node #'make-ast-defun
    :name            (ast-defun-name node)
    :params          (ast-defun-params node)
    :optional-params (ast-defun-optional-params node)
    :rest-param      (ast-defun-rest-param node)
    :key-params      (ast-defun-key-params node)
    :declarations    (ast-defun-declarations node)
    :body            (%optimize-ast-list (ast-defun-body node))))

(defun %optimize-ast-defvar-node (node)
  (%clone-source node #'make-ast-defvar
    :name  (ast-defvar-name node)
    :value (optimize-ast (ast-defvar-value node))
    :kind  (ast-defvar-kind node)))

(defun %optimize-ast-block-node (node)
  (%clone-source node #'make-ast-block
    :name (ast-block-name node)
    :body (%optimize-ast-list (ast-block-body node))))

(defun %optimize-ast-return-from-node (node)
  (%clone-source node #'make-ast-return-from
    :name  (ast-return-from-name node)
    :value (optimize-ast (ast-return-from-value node))))

(defun %optimize-ast-setq-node (node)
  (%clone-source node #'make-ast-setq
    :var   (ast-setq-var node)
    :value (optimize-ast (ast-setq-value node))))

(defun %optimize-ast-the-node (node)
  (%clone-source node #'make-ast-the
    :type  (ast-the-type node)
    :value (optimize-ast (ast-the-value node))))

(defun %optimize-ast-rebuild-node (node)
  "Rebuild NODE via the declarative rebuilder table when it is a simple recursive case." 
  (let ((rebuilder (cdr (assoc (type-of node) *optimize-ast-rebuilder-table*))))
    (when rebuilder
      (funcall rebuilder node))))

(defun optimize-ast (node)
  "Fold small pure constant expressions before VM lowering."
  (typecase node
    (ast-binop
     (%fold-ast-binop node
                      (optimize-ast (ast-binop-lhs node))
                      (optimize-ast (ast-binop-rhs node))))
    (ast-call
     (let* ((func      (optimize-ast (ast-call-func node)))
            (args      (%optimize-ast-list (ast-call-args node)))
            (call-node (%clone-source node #'make-ast-call :func func :args args)))
        (multiple-value-bind (value ok)
            (let ((*compile-time-value-env*    *compile-time-value-env*)
                 (*compile-time-function-env* *compile-time-function-env*))
             (%evaluate-ast call-node *compile-time-eval-depth-limit*))
         (if ok (%compile-time-value->ast value node) call-node))))
    (t
     (or (%optimize-ast-rebuild-node node)
         node))))
