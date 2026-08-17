;;;; packages/prolog-tools/src/call-graph.lisp - cl-cc AST-to-call-graph adapter
;;;;
;;;; The generic call-graph construction and every analysis over it
;;;; (reachability, dead-code, mutual-recursion, FD-constraint coloring, the
;;;; edge-spec DCG grammar) now live in the external :cl-prolog-kit/callgraph
;;;; system, re-exported through this package's package.lisp facade. All
;;;; that remains here is walking :cl-cc/ast nodes to collect the
;;;; (caller . callee) edges that system needs.

(in-package :cl-cc/prolog-tools)

(defun %call-target-name (func)
  "Return the callee symbol for an AST-CALL's FUNC slot, or NIL if unresolvable."
  (cond ((symbolp func) func)
        ((cl-cc/ast:ast-var-p func) (cl-cc/ast:ast-var-name func))
        ((cl-cc/ast:ast-function-p func) (cl-cc/ast:ast-function-name func))
        (t nil)))

(defun %collect-call-targets (node)
  "Return the list of callee symbols for every AST-CALL nested under NODE."
  (let ((targets '()))
    (labels ((walk (n)
               (when (cl-cc/ast:ast-call-p n)
                 (let ((name (%call-target-name (cl-cc/ast:ast-call-func n))))
                   (when name (push name targets))))
               (dolist (child (cl-cc/ast:ast-children n))
                 (when (cl-cc/ast:ast-node-p child) (walk child)))))
      (walk node))
    (nreverse targets)))

(defun build-call-graph (defuns &key entry-points)
  "Build a CALL-GRAPH from a list of AST-DEFUN nodes by delegating the
generic graph construction to cl-prolog-kit/callgraph.

ENTRY-POINTS is a list of function-name symbols treated as always-reachable
roots (e.g. a program's toplevel entry function)."
  (let ((names (mapcar #'cl-cc/ast:ast-defun-name defuns))
        (edges (loop for defun-node in defuns
                     for caller = (cl-cc/ast:ast-defun-name defun-node)
                     nconc (mapcar (lambda (callee) (cons caller callee))
                                   (%collect-call-targets defun-node)))))
    (cl-prolog-kit/callgraph:build-call-graph-from-edges names edges :entry-points entry-points)))
