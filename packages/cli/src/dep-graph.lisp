;;;; cli/src/dep-graph.lisp — FR-361 Dependency Graph Visualization
;;;;
;;;; The ASDF dependency graph is modeled with the external cl-dataflow-kit library:
;;;; systems and their dependencies become a real cl-dataflow-kit graph, and the
;;;; DOT / Mermaid renderings and the topological build order come straight from
;;;; cl-dataflow-kit (graph->dot, graph->mermaid, topological-sort, graph-acyclic-p)
;;;; rather than hand-assembled output.

(in-package :cl-cc/cli)

;;; ─────────────────────────────────────────────────────────────────────────
;;; FR-361: Dependency Graph Visualization
;;; `./cl-cc dep-graph [--format dot|json|mermaid|topo]` outputs a dependency
;;; graph of the registered ASDF systems.
;;; ─────────────────────────────────────────────────────────────────────────

(defun %asdf-system-dependencies (system)
  "Return a list of (system-name . dep-system-name) pairs for SYSTEM."
  (let ((edges nil)
        (system-name (asdf:component-name system)))
    (when (subtypep (type-of system) 'asdf:system)
      (dolist (dep (asdf:system-depends-on system))
        (push (cons system-name dep) edges)))
    edges))

(defun %collect-asdf-dependency-edges ()
  "Walk all registered ASDF systems and collect dependency edges."
  (let ((edges nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (sys-name (asdf:registered-systems))
      (unless (gethash sys-name seen)
        (setf (gethash sys-name seen) t)
        (handler-case
            (let ((sys (asdf:find-system sys-name nil)))
              (when sys
                (setf edges (nconc edges (%asdf-system-dependencies sys)))))
          (error () nil))))
    edges))

(defun %dep-node-name (name)
  "Normalize an ASDF system/dependency designator to a lowercased node name.
Delegates to `%normalize-system-name' (handlers-ql.lisp) so that compound
dependency-def forms such as `(:version name version)` are handled
identically everywhere in the CLI rather than duplicating the logic here."
  (%normalize-system-name name))

(defun %build-dependency-graph ()
  "Build a cl-dataflow-kit graph of registered ASDF systems: every system and
dependency becomes a node, and each dependency becomes a directed edge."
  (let ((graph (cl-dataflow-kit:make-graph))
        (added (make-hash-table :test #'equal)))
    (flet ((ensure-node (name)
             (unless (gethash name added)
               (setf (gethash name added) t)
               (cl-dataflow-kit:add-node graph (cl-dataflow-kit:make-node name)))))
      (dolist (edge (%collect-asdf-dependency-edges))
        (let ((from (%dep-node-name (car edge)))
              (to   (%dep-node-name (cdr edge))))
          (ensure-node from)
          (ensure-node to)
          (ignore-errors (cl-dataflow-kit:add-edge graph from to)))))
    graph))

(defun %dep-graph-json (graph)
  "Render GRAPH as a simple JSON adjacency list, sourced from the cl-dataflow-kit
graph's edges."
  ;; cl-dataflow-kit's edge-from / edge-to already return the endpoint node name.
  (let ((nodes (make-hash-table :test #'equal)))
    (dolist (edge (cl-dataflow-kit:graph-edges graph))
      (push (cl-dataflow-kit:edge-to edge)
            (gethash (cl-dataflow-kit:edge-from edge) nodes)))
    (format t "{~%")
    (let ((first t))
      (maphash (lambda (node deps)
                 (if first (setf first nil) (format t ",~%"))
                 (format t "  ~S: [~{~S~^, ~}]"
                         node (delete-duplicates deps :test #'equal)))
               nodes))
    (format t "~%}~%")))

(defun %dep-graph-topo (graph)
  "Print the topological build order of GRAPH (a genuine cl-dataflow-kit query),
noting whether the dependency graph is acyclic."
  (if (cl-dataflow-kit:graph-acyclic-p graph)
      (progn
        (format t "; topological build order (~:[has cycles~;acyclic~]):~%"
                (cl-dataflow-kit:graph-acyclic-p graph))
        (dolist (node (cl-dataflow-kit:topological-sort graph))
          (format t "~A~%" (cl-dataflow-kit:node-name node))))
      (format t "; dependency graph has cycles; no topological order~%")))

(defun dep-graph (&key (output-format :dot))
  "Generate a dependency graph of registered ASDF systems.
OUTPUT-FORMAT is :dot, :json, :mermaid, or :topo."
  (let ((graph (%build-dependency-graph)))
    (ecase output-format
      (:dot     (princ (cl-dataflow-kit:graph->dot graph :name "ASDF_Dependencies"))
                (terpri))
      (:mermaid (princ (cl-dataflow-kit:graph->mermaid graph))
                (terpri))
      (:topo    (%dep-graph-topo graph))
      (:json    (%dep-graph-json graph)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CLI entry point
;;; ─────────────────────────────────────────────────────────────────────────

(defun %parse-dep-graph-args (args)
  "Parse --format dot|json|mermaid|topo from ARGS.  Returns a keyword."
  (let ((format :dot))
    (loop for arg in args
          for next-arg = (nth (1+ (position arg args :test #'string=)) args)
          when (string= arg "--format")
            do (cond
                 ((string-equal next-arg "json")    (setf format :json))
                 ((string-equal next-arg "mermaid") (setf format :mermaid))
                 ((string-equal next-arg "topo")    (setf format :topo))
                 ((string-equal next-arg "dot")     (setf format :dot))))
    format))

(defun %handle-dep-graph (args)
  "CLI handler for `cl-cc dep-graph [--format dot|json|mermaid|topo]`."
  (let ((format (%parse-dep-graph-args args)))
    (if (find-package :asdf)
        (progn
          (format *error-output* "; FR-361: generating dependency graph (~A)...~%" format)
          (dep-graph :output-format format))
        (progn
          (format *error-output* "ERROR: ASDF not available.~%")
          (cond
            ((eq format :json)
             (format t "{~%  \"error\": \"ASDF not loaded\"~%}~%"))
            (t
             (format t "digraph { node [shape=box]; ERROR [label=\"ASDF not loaded\"]; }~%")))))))
