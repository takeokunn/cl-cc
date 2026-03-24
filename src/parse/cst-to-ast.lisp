;;;; cst-to-ast.lisp — CST → AST lowering
;;;;
;;;; Converts CST nodes (from Pratt parser) to AST nodes (for compilation).
;;;; Uses cst-to-sexp as the conversion mechanism, then calls lower-sexp-to-ast,
;;;; with source location patched in from the original CST node.
;;;;
;;;; This eliminates all read-from-string dependencies from the compilation path.

(in-package :cl-cc)

;;; ─── Core CST → AST Lowering ────────────────────────────────────────────────

(defun lower-cst-to-ast (cst-node &key source source-file)
  "Lower a CST-NODE to an AST node.
   Uses cst-to-sexp + lower-sexp-to-ast with source location from CST.
   SOURCE is the original source string (for line/col computation)."
  (when (null cst-node) (return-from lower-cst-to-ast nil))
  ;; Get source location from CST node
  (multiple-value-bind (line col)
      (if source
          (byte-offset-to-line-col source (cst-node-start-byte cst-node))
          (values nil nil))
    ;; Convert CST → sexp → AST
    (let ((sexp (cst-to-sexp cst-node)))
      (lower-sexp-to-ast sexp
                         :source-file (or source-file (cst-node-source-file cst-node))
                         :source-line line
                         :source-column col))))

(defun lower-cst-list-to-ast (cst-nodes &key source source-file)
  "Lower a list of CST nodes to a list of AST nodes."
  (mapcar (lambda (node) (lower-cst-to-ast node :source source :source-file source-file))
          cst-nodes))

;;; ─── Convenience Entry Points ───────────────────────────────────────────────

(defun parse-and-lower (source &optional source-file)
  "Full pipeline: source string → CST → AST list.
   No read-from-string. Uses the Pratt/RecDescent CST parser."
  (multiple-value-bind (cst-list diagnostics)
      (parse-cl-source source source-file)
    ;; Report diagnostics as warnings (non-fatal)
    (when diagnostics
      (dolist (d diagnostics)
        (warn "Parse diagnostic: ~A" (diagnostic-message d))))
    ;; Lower all CST forms to AST
    (lower-cst-list-to-ast cst-list :source source :source-file source-file)))

(defun parse-and-lower-one (source &optional source-file)
  "Parse one form from SOURCE, return a single AST node."
  (let ((ast-list (parse-and-lower source source-file)))
    (when (null ast-list)
      (error "Empty source"))
    (first ast-list)))
