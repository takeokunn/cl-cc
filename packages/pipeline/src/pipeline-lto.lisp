;;;; packages/pipeline/src/pipeline-lto.lisp — FR-500 LTO infrastructure

(in-package :cl-cc/pipeline)

(defparameter *lto-enabled* nil
  "Dynamic policy gate for Link-Time Optimization.  The CLI sets this only via --lto.")

(defparameter *lto-bitcode-section-name* "__bitcode"
  "ELF/Mach-O style section name used for serialized VM IR payloads.")

(defvar *last-lto-bitcode-section* nil
  "Most recent serialized LTO section descriptor produced by an opt-in --lto compile.")

(defstruct (lto-module (:constructor make-lto-module))
  "Serialized VM IR for one compilation module."
  name
  source-file
  instructions
  metadata)

(defstruct (lto-call-graph (:constructor make-lto-call-graph))
  "Cross-module call graph: ROOTS and EDGES are label-name hash tables."
  roots
  edges)

(defun lto-instructions-from-result (result)
  "Return the best VM instruction list carried by compilation RESULT."
  (let ((instructions (or (cl-cc/compile:compilation-result-optimized-instructions result)
                          (cl-cc/compile:compilation-result-vm-instructions result)
                          (and (cl-cc/compile:compilation-result-program result)
                               (cl-cc/vm:vm-program-instructions
                                (cl-cc/compile:compilation-result-program result))))))
    (cond ((null instructions) nil)
          ((vectorp instructions) (coerce instructions 'list))
          (t instructions))))

(defun lto-serialize-instructions (instructions)
  "Serialize VM INSTRUCTIONS into a read/print S-expression string."
  (with-output-to-string (out)
    (let ((*print-readably* t)
          (*print-circle* t))
      (prin1 (mapcar #'cl-cc/vm:instruction->sexp instructions) out))))

(defun lto-deserialize-instructions (payload)
  "Deserialize a string produced by LTO-SERIALIZE-INSTRUCTIONS."
  (mapcar #'cl-cc/vm:sexp->instruction
          (let ((*read-eval* nil))
            (read-from-string payload))))

(defun lto-serialize-module (name instructions &key source-file metadata)
  "Return a serialized LTO module payload for NAME and VM INSTRUCTIONS."
  (with-output-to-string (out)
    (let ((*print-readably* t)
          (*print-circle* t))
      (prin1 `(:cl-cc-lto-module
               :version 1
               :name ,name
               :source-file ,(and source-file (namestring source-file))
               :metadata ,metadata
               :instructions ,(mapcar #'cl-cc/vm:instruction->sexp instructions))
             out))))

(defun lto-deserialize-module (payload)
  "Read an LTO module payload and return an LTO-MODULE."
  (let* ((*read-eval* nil)
         (form (read-from-string payload)))
    (unless (and (consp form) (eq (first form) :cl-cc-lto-module))
      (error "Invalid LTO module payload: ~S" form))
    (let ((plist (rest form)))
      (make-lto-module
       :name (getf plist :name)
       :source-file (getf plist :source-file)
       :metadata (getf plist :metadata)
       :instructions (mapcar #'cl-cc/vm:sexp->instruction
                             (getf plist :instructions))))))

(defun lto-module-from-result (name result &key source-file metadata)
  "Build an LTO module from a compilation RESULT."
  (make-lto-module :name name
                   :source-file source-file
                   :metadata metadata
                   :instructions (lto-instructions-from-result result)))

(defun lto-make-bitcode-section (module-or-payload)
  "Return an object-file section descriptor for serialized LTO IR.

The native binary emitters can consume this descriptor when custom section
emission is wired in.  Keeping it as data makes the infrastructure opt-in and
non-invasive for the current non-LTO path."
  (let ((payload (if (typep module-or-payload 'lto-module)
                     (lto-serialize-module (lto-module-name module-or-payload)
                                           (lto-module-instructions module-or-payload)
                                           :source-file (lto-module-source-file module-or-payload)
                                           :metadata (lto-module-metadata module-or-payload))
                     module-or-payload)))
    (list :name *lto-bitcode-section-name*
          :type :progbits
          :flags nil
          :payload payload)))

(defun %lto-note-edge (caller callee edges)
  (when callee
    (let ((out (or (gethash caller edges)
                   (setf (gethash caller edges) (make-hash-table :test #'equal)))))
      (incf (gethash callee out 0)))))

(defun %lto-callable-labels (instructions)
  (let ((defs (make-hash-table :test #'equal)))
    (dolist (inst instructions defs)
      (when (typep inst '(or cl-cc/vm:vm-closure cl-cc/vm:vm-func-ref))
        (setf (gethash (cl-cc/vm:vm-label-name inst) defs) t)))))

(defun %lto-call-targets (instructions)
  (let ((reg-label (make-hash-table :test #'eq))
        (roots (make-hash-table :test #'equal))
        (edges (make-hash-table :test #'equal))
        (caller :toplevel))
    (dolist (inst instructions)
      (typecase inst
        (cl-cc/vm:vm-label
         (setf caller (cl-cc/vm:vm-name inst)))
        ((or cl-cc/vm:vm-closure cl-cc/vm:vm-func-ref)
         (setf (gethash (cl-cc/vm:vm-dst inst) reg-label)
               (cl-cc/vm:vm-label-name inst)))
        (cl-cc/vm:vm-register-function
         (let ((label (gethash (cl-cc/vm:vm-src inst) reg-label)))
           (when label (setf (gethash label roots) t))))
        ((or cl-cc/vm:vm-call cl-cc/vm:vm-tail-call)
         (%lto-note-edge caller (gethash (cl-cc/vm:vm-func-reg inst) reg-label) edges))
        (t
         (let ((dst (ignore-errors (cl-cc/optimize:opt-inst-dst inst))))
           (when dst (remhash dst reg-label))))))
    (values roots edges)))

(defun lto-build-cross-module-call-graph (modules-or-instructions)
  "Construct a cross-module direct-call graph from LTO modules or instruction lists."
  (let ((all-roots (make-hash-table :test #'equal))
        (all-edges (make-hash-table :test #'equal)))
    (dolist (unit modules-or-instructions)
      (let ((instructions (if (typep unit 'lto-module)
                              (lto-module-instructions unit)
                              unit)))
        (multiple-value-bind (roots edges) (%lto-call-targets instructions)
          (maphash (lambda (root value)
                     (declare (ignore value))
                     (setf (gethash root all-roots) t))
                   roots)
          (maphash (lambda (caller callees)
                     (maphash (lambda (callee count)
                                (let ((out (or (gethash caller all-edges)
                                               (setf (gethash caller all-edges)
                                                     (make-hash-table :test #'equal)))))
                                  (incf (gethash callee out 0) count)))
                              callees))
                   edges))))
    (make-lto-call-graph :roots all-roots :edges all-edges)))

(defun %lto-reachable-labels (graph)
  (let ((seen (make-hash-table :test #'equal))
        (work nil))
    (maphash (lambda (root value)
               (declare (ignore value))
               (push root work))
             (lto-call-graph-roots graph))
    (loop while work
          for label = (pop work)
          unless (gethash label seen)
            do (setf (gethash label seen) t)
               (let ((edges (gethash label (lto-call-graph-edges graph))))
                 (when edges
                   (maphash (lambda (callee count)
                              (declare (ignore count))
                              (push callee work))
                            edges))))
    seen))

(defun lto-eliminate-dead-functions (instructions graph)
  "Conservatively remove unreachable direct function bodies from INSTRUCTIONS.

Only labels known to the closed-world graph are candidates; top-level code and
unknown labels are preserved."
  (let ((defs (%lto-callable-labels instructions))
        (reachable (%lto-reachable-labels graph))
        (dropping nil)
        (result nil))
    (dolist (inst instructions (nreverse result))
      (cond
        ((typep inst 'cl-cc/vm:vm-label)
         (let ((label (cl-cc/vm:vm-name inst)))
           (setf dropping (and (gethash label defs)
                               (not (gethash label reachable))))
           (unless dropping (push inst result))))
        (dropping
         (when (typep inst '(or cl-cc/vm:vm-ret cl-cc/vm:vm-halt))
           (setf dropping nil)))
        (t (push inst result))))))

(defun lto-merge-modules (modules)
  "Deserialize/merge all LTO MODULES into one VM instruction stream."
  (mapcan (lambda (module) (copy-list (lto-module-instructions module))) modules))

(defun lto-optimize-modules (modules &key (run-ipcp t) (run-dce t))
  "Merge modules, build cross-module graph, and run enabled LTO infrastructure passes."
  (let* ((merged (lto-merge-modules modules))
         (graph (lto-build-cross-module-call-graph modules))
         (ipcp (if run-ipcp (cl-cc/optimize:opt-pass-ipcp merged) merged)))
    (values (if run-dce (lto-eliminate-dead-functions ipcp graph) ipcp)
            graph)))

(defun lto-apply-to-program (program &key module-name source-file)
  "Run the current opt-in LTO pipeline on PROGRAM and return a new VM program."
  (unless (typep program 'cl-cc/vm:vm-program)
    (return-from lto-apply-to-program program))
  (let* ((module (make-lto-module :name (or module-name "module")
                                  :source-file source-file
                                  :instructions (cl-cc/vm:vm-program-instructions program)
                                  :metadata (list :section *lto-bitcode-section-name*)))
         (section (lto-make-bitcode-section module)))
    (setf *last-lto-bitcode-section* section)
    (multiple-value-bind (instructions graph)
        (lto-optimize-modules (list module))
      (declare (ignore graph))
      (let ((new-program (cl-cc/vm::copy-vm-program program)))
        (setf (cl-cc/vm:vm-program-instructions new-program) instructions)
        new-program))))
