;;;; cli/src/dep-graph.lisp — FR-361 Dependency Graph Visualization
(in-package :cl-cc/cli)

;;; ─────────────────────────────────────────────────────────────────────────
;;; FR-361: Dependency Graph Visualization
;;; `./cl-cc dep-graph [--format dot|json]` outputs a dependency graph
;;; of the current ASDF system in Graphviz DOT or JSON format.
;;; ─────────────────────────────────────────────────────────────────────────

(defun %dep-graph-edge-dot (from to &optional (label nil))
  (format t "  ~S -> ~S~@[ [label=~S]~];~%"
          (string-downcase (string from))
          (string-downcase (string to))
          label))

(defun %dep-graph-edge-json (from to edges-seen)
  (unless (gethash from edges-seen)
    (format t "  ~S: []~%" (string-downcase (string from)))
    (setf (gethash from edges-seen) t)))

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

(defun %dep-graph-dot (edges)
  "Output edges in Graphviz DOT format."
  (format t "digraph ASDF_Dependencies {~%")
  (format t "  node [shape=box, style=rounded];~%")
  (format t "  rankdir=TB;~%")
  (dolist (edge edges)
    (%dep-graph-edge-dot (car edge) (cdr edge)))
  (format t "}~%"))

(defun %dep-graph-json (edges)
  "Output edges in simple JSON adjacency list format."
  (let ((nodes (make-hash-table :test #'equal)))
    (dolist (edge edges)
      (let* ((from (string-downcase (string (car edge))))
             (to   (string-downcase (string (cdr edge)))))
        (push to (gethash from nodes))))
    (format t "{~%")
    (let ((first t))
      (maphash (lambda (node deps)
                 (if first
                     (setf first nil)
                     (format t ",~%"))
                 (setf deps (delete-duplicates deps :test #'equal))
                 (format t "  ~S: [~{~S~^, ~}]" node deps))
               nodes))
    (format t "~%}~%")))

(defun dep-graph (&key (output-format :dot))
  "Generate a dependency graph of registered ASDF systems.
OUTPUT-FORMAT can be :dot (Graphviz DOT) or :json (JSON adjacency list)."
  (let ((edges (%collect-asdf-dependency-edges)))
    (ecase output-format
      (:dot  (%dep-graph-dot edges))
      (:json (%dep-graph-json edges)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CLI entry point
;;; ─────────────────────────────────────────────────────────────────────────

(defun %parse-dep-graph-args (args)
  "Parse --format dot|json from ARGS. Returns :dot or :json."
  (let ((format :dot))
    (loop for arg in args
          for next-arg = (nth (1+ (position arg args :test #'string=)) args)
          when (string= arg "--format")
          do (cond
               ((string-equal next-arg "json") (setf format :json))
               ((string-equal next-arg "dot")  (setf format :dot))))
    format))

(defun %handle-dep-graph (args)
  "CLI handler for `cl-cc dep-graph [--format dot|json]`."
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
