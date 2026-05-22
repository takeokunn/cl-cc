;;;; packages/pipeline/src/pipeline-thin-lto.lisp — FR-501 ThinLTO infrastructure

(in-package :cl-cc/pipeline)

(defparameter *thin-lto-hot-call-threshold* 10
  "Call-count threshold for selective ThinLTO IR import.")

(defstruct (thin-lto-function-summary (:constructor make-thin-lto-function-summary))
  "Per-function summary used by ThinLTO's fast O(N) link-time planning."
  module-name
  label
  signature
  inline-candidate-p
  type-info
  instruction-count
  call-count)

(defstruct (thin-lto-module-summary (:constructor make-thin-lto-module-summary))
  "Compact summary for one module; intentionally excludes function bodies."
  module-name
  source-file
  functions)

(defun %thin-lto-instruction-count (body)
  (length body))

(defun %thin-lto-inline-candidate-p (body)
  (and body (<= (%thin-lto-instruction-count body) 30)))

(defun %thin-lto-edge-call-counts (graph)
  (let ((counts (make-hash-table :test #'equal)))
    (maphash (lambda (caller callees)
               (declare (ignore caller))
               (maphash (lambda (callee count)
                          (incf (gethash callee counts 0) count))
                        callees))
             (lto-call-graph-edges graph))
    counts))

(defun thin-lto-generate-module-summary (module)
  "Generate per-function ThinLTO summaries for MODULE without serializing bodies."
  (let* ((instructions (lto-module-instructions module))
         (defs (cl-cc/optimize::opt-collect-function-defs instructions))
         (graph (lto-build-cross-module-call-graph (list module)))
         (counts (%thin-lto-edge-call-counts graph))
         (summaries nil))
    (maphash (lambda (label def)
               (let ((params (getf def :params))
                     (body (getf def :body)))
                 (push (make-thin-lto-function-summary
                        :module-name (lto-module-name module)
                        :label label
                        :signature params
                        :inline-candidate-p (%thin-lto-inline-candidate-p body)
                        :type-info nil
                        :instruction-count (%thin-lto-instruction-count body)
                        :call-count (gethash label counts 0))
                       summaries)))
             defs)
    (make-thin-lto-module-summary
     :module-name (lto-module-name module)
     :source-file (lto-module-source-file module)
     :functions (nreverse summaries))))

(defun thin-lto-serialize-summary (summary)
  "Serialize a ThinLTO module SUMMARY as an S-expression."
  (with-output-to-string (out)
    (let ((*print-readably* t)
          (*print-circle* t))
      (prin1 summary out))))

(defun thin-lto-deserialize-summary (payload)
  "Deserialize a ThinLTO summary payload."
  (let ((*read-eval* nil))
    (read-from-string payload)))

(defun thin-lto-read-summaries (payloads)
  "Read only module summaries at link time; this is the O(N) ThinLTO fast path."
  (mapcar #'thin-lto-deserialize-summary payloads))

(defun thin-lto-select-imports (summaries &key (threshold *thin-lto-hot-call-threshold*))
  "Return (module-name . label) imports for hot inline candidates."
  (let ((imports nil))
    (dolist (summary summaries (nreverse imports))
      (dolist (fn (thin-lto-module-summary-functions summary))
        (when (and (thin-lto-function-summary-inline-candidate-p fn)
                   (> (thin-lto-function-summary-call-count fn) threshold))
          (push (cons (thin-lto-function-summary-module-name fn)
                      (thin-lto-function-summary-label fn))
                imports))))))

(defun thin-lto-import-hot-functions (modules summaries
                                      &key (threshold *thin-lto-hot-call-threshold*))
  "Selectively import IR for hot functions identified by SUMMARY data.

Returns an alist mapping module-name to imported instruction bodies.  The caller
can prepend/append these bodies before optimizing each module independently."
  (let ((imports (thin-lto-select-imports summaries :threshold threshold))
        (result (make-hash-table :test #'equal)))
    (dolist (module modules)
      (let ((wanted (remove-if-not (lambda (entry)
                                     (equal (car entry) (lto-module-name module)))
                                   imports))
            (defs (cl-cc/optimize::opt-collect-function-defs
                   (lto-module-instructions module))))
        (dolist (entry wanted)
          (let* ((label (cdr entry))
                 (def (gethash label defs))
                 (body (and def (getf def :body))))
            (when body
              (push (cons label (copy-list body))
                    (gethash (lto-module-name module) result)))))))
    (loop for module being the hash-keys of result
          using (hash-value bodies)
          collect (cons module (nreverse bodies)))))

(defun thin-lto-optimize-modules (modules &key (threshold *thin-lto-hot-call-threshold*))
  "ThinLTO driver: summarize globally, import hot IR, optimize modules independently."
  (let* ((summaries (mapcar #'thin-lto-generate-module-summary modules))
         (imports (thin-lto-import-hot-functions modules summaries :threshold threshold)))
    (values
     (mapcar (lambda (module)
               (let ((imported (cdr (assoc (lto-module-name module) imports :test #'equal))))
                 (make-lto-module
                  :name (lto-module-name module)
                  :source-file (lto-module-source-file module)
                  :metadata (list :thin-lto t :imports (mapcar #'car imported))
                  :instructions (cl-cc/optimize:optimize-instructions
                                 (append (mapcan #'copy-list (mapcar #'cdr imported))
                                         (copy-list (lto-module-instructions module)))))))
             modules)
     summaries)))
