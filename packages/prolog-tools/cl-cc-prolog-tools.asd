;;;; cl-cc-prolog-tools.asd — cl-cc AST adapter for Prolog-based call-graph analysis
;;;;
;;;; This package used to hold BOTH the generic call-graph/reachability/
;;;; dead-code/FD-coloring/edge-DCG logic AND the small adapter that walks
;;;; :cl-cc/ast nodes to build one. The generic ~90% carried no cl-cc
;;;; knowledge at all, so it moved to the external `cl-prolog-kit/callgraph`
;;;; system (github:nerima-lisp/cl-prolog-kit); this package is now just the
;;;; thin AST-adapter layer (BUILD-CALL-GRAPH and its two helpers) that
;;;; delegates graph construction to it, plus a package.lisp facade that
;;;; re-exports the generic analysis functions so existing call sites need
;;;; no CL-PROLOG-KIT/CALLGRAPH: prefix.
;;;;
;;;; Built on the external `cl-prolog-kit` engine (github:nerima-lisp/cl-prolog-kit),
;;;; not on cl-cc's own homegrown Prolog engine in packages/prolog
;;;; (:cl-cc-prolog, used internally for peephole-optimization rules). The
;;;; two are unrelated.
;;;;
;;;; Leaf system at the cl-cc-ast tier: depends only on :cl-cc-ast and the
;;;; external :cl-prolog-kit / :cl-prolog-kit/callgraph systems. Its test system
;;;; additionally depends on the external :cl-weave testing framework and is
;;;; intentionally NOT folded into the umbrella cl-cc-test.asd aggregate,
;;;; since that aggregate is driven by cl-cc's own testing-framework runner
;;;; rather than cl-weave:run-all. Run this package's tests independently
;;;; via `(asdf:test-system :cl-cc-prolog-tools)`.

(asdf:defsystem :cl-cc-prolog-tools
  :description "cl-cc AST adapter for Prolog-based call-graph analysis, built on the external cl-prolog-kit/callgraph system"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on (:cl-cc-ast :cl-prolog-kit :cl-prolog-kit/callgraph)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "call-graph"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-cc-prolog-tools/tests))))

(asdf:defsystem :cl-cc-prolog-tools/tests
  :description "cl-weave test suite for cl-cc-prolog-tools"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on (:cl-cc-prolog-tools :cl-prolog-kit :cl-prolog-kit/callgraph :cl-weave)
  :pathname "tests"
  :serial t
  :components
  ((:file "package")
   (:file "call-graph-tests"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call :cl-weave :run-all :reporter :spec)
               (error "cl-cc-prolog-tools test suite failed."))))
