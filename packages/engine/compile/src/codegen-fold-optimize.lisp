;;;; packages/engine/compile/src/codegen-fold-optimize.lisp — AST constant-fold pass (optimize-ast)
;;;
;;; Contains:
;;;   - %loc         macro: inherit source location from an AST node
;;;   - optimize-ast top-level fold pass called from compile-toplevel-forms
;;;
;;; Depends on codegen-fold.lisp (%fold-ast-binop, %evaluate-ast, %compile-time-eval-depth-limit,
;;; *compile-time-value-env*, *compile-time-function-env*, %compile-time-value->ast).
;;; Load order: after codegen-fold, before codegen.

(in-package :cl-cc/compile)

;;; ── Source-location macro ────────────────────────────────────────────────────

;;; Usage: (%loc node (make-ast-foo :field val ...))
;;; Expands to: (make-ast-foo :field val ... :source-file F :source-line L :source-column C)
(defmacro %loc (node &rest make-form)
  "Append source-location keyword args from NODE to MAKE-FORM constructor."
  (let ((n (gensym "node")))
    `(let ((,n ,node))
       (,(first make-form) ,@(rest make-form)
        :source-file   (ast-source-file   ,n)
        :source-line   (ast-source-line   ,n)
        :source-column (ast-source-column ,n)))))

;;; ── AST constant fold pass ───────────────────────────────────────────────────

(defun optimize-ast (node)
  "Fold small pure constant expressions before VM lowering."
  (typecase node
    (ast-binop
     (%fold-ast-binop node
                      (optimize-ast (ast-binop-lhs node))
                      (optimize-ast (ast-binop-rhs node))))
    (ast-call
     (let* ((func      (optimize-ast (ast-call-func node)))
            (args      (mapcar #'optimize-ast (ast-call-args node)))
            (call-node (%loc node make-ast-call :func func :args args)))
       (multiple-value-bind (value ok)
           (let ((*compile-time-value-env*    *compile-time-value-env*)
                 (*compile-time-function-env* *compile-time-function-env*))
             (%evaluate-ast call-node *compile-time-eval-depth-limit*))
         (if ok (%compile-time-value->ast value node) call-node))))
    (ast-progn
     (%loc node make-ast-progn
       :forms (mapcar #'optimize-ast (ast-progn-forms node))))
    (ast-let
     (%loc node make-ast-let
       :bindings     (mapcar (lambda (b) (cons (car b) (optimize-ast (cdr b))))
                             (ast-let-bindings node))
       :declarations (ast-let-declarations node)
       :body         (mapcar #'optimize-ast (ast-let-body node))))
    (ast-if
     (%loc node make-ast-if
       :cond (optimize-ast (ast-if-cond node))
       :then (optimize-ast (ast-if-then node))
       :else (optimize-ast (ast-if-else node))))
    (ast-lambda
     (%loc node make-ast-lambda
       :params          (ast-lambda-params node)
       :optional-params (ast-lambda-optional-params node)
       :rest-param      (ast-lambda-rest-param node)
       :key-params      (ast-lambda-key-params node)
       :declarations    (ast-lambda-declarations node)
       :body            (mapcar #'optimize-ast (ast-lambda-body node))
       :env             (ast-lambda-env node)))
    (ast-defun
     (%loc node make-ast-defun
       :name            (ast-defun-name node)
       :params          (ast-defun-params node)
       :optional-params (ast-defun-optional-params node)
       :rest-param      (ast-defun-rest-param node)
       :key-params      (ast-defun-key-params node)
       :declarations    (ast-defun-declarations node)
       :body            (mapcar #'optimize-ast (ast-defun-body node))))
    (ast-defvar
     (%loc node make-ast-defvar
       :name  (ast-defvar-name node)
       :value (optimize-ast (ast-defvar-value node))
       :kind  (ast-defvar-kind node)))
    (ast-block
     (%loc node make-ast-block
       :name (ast-block-name node)
       :body (mapcar #'optimize-ast (ast-block-body node))))
    (ast-return-from
     (%loc node make-ast-return-from
       :name  (ast-return-from-name node)
       :value (optimize-ast (ast-return-from-value node))))
    (ast-setq
     (%loc node make-ast-setq
       :var   (ast-setq-var node)
       :value (optimize-ast (ast-setq-value node))))
    (ast-the
     (%loc node make-ast-the
       :type  (ast-the-type node)
       :value (optimize-ast (ast-the-value node))))
    (t node)))
