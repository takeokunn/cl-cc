;;;; packages/prolog-tools/src/package.lisp - CL-CC Prolog Tools Package
;;;;
;;;; Thin AST-adapter layer: BUILD-CALL-GRAPH walks :cl-cc/ast AST-DEFUN
;;;; nodes to collect caller/callee edges, then delegates the actual
;;;; call-graph construction and every generic analysis (reachability,
;;;; dead-code, mutual-recursion, FD-constraint graph coloring, the edge-spec
;;;; DCG grammar) to the external, AST-independent :cl-prolog-kit/callgraph
;;;; system.
;;;;
;;;; Every generic symbol below (CALL-GRAPH, REACHABLE-P, ...) is
;;;; IMPORT-FROM'd from :cl-prolog-kit/callgraph and re-exported here rather
;;;; than requiring callers to write CL-PROLOG-KIT/CALLGRAPH: prefixes — this is
;;;; a facade so existing call sites (this package's own tests included)
;;;; keep working unqualified after the split.
(defpackage :cl-cc/prolog-tools
  (:use :cl)
  (:import-from :cl-prolog-kit/callgraph
    #:call-graph
    #:call-graph-rulebase
    #:call-graph-defined
    #:call-graph-entry-points
    #:reachable-p
    #:reachable-from
    #:direct-callees
    #:find-dead-code
    #:find-mutually-recursive-pairs
    #:tokenize-edge-spec
    #:edge-spec-well-formed-p
    #:parse-edge-spec
    #:color-call-graph
    #:valid-coloring-p)
  (:export
    ;; re-exported from cl-prolog-kit/callgraph
    #:call-graph
    #:call-graph-rulebase
    #:call-graph-defined
    #:call-graph-entry-points
    #:reachable-p
    #:reachable-from
    #:direct-callees
    #:find-dead-code
    #:find-mutually-recursive-pairs
    #:tokenize-edge-spec
    #:edge-spec-well-formed-p
    #:parse-edge-spec
    #:color-call-graph
    #:valid-coloring-p
    ;; this package's own AST adapter
    #:build-call-graph))
