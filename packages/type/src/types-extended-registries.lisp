;;;; types-extended-registries.lisp — Type-interface-module, SMT solvers, type checkers, synthesis strategies
(in-package :cl-cc/type)

(defstruct (type-interface-module (:constructor %make-type-interface-module))
  "A loaded FR-2405 interface summary."
  (name nil)
  (exports nil :type list)
  (fingerprint nil)
  (exported-types nil :type list))

(defvar *type-interface-registry* (make-hash-table :test #'equal)
  "Maps module names to loaded type interface summaries.")

(defvar *type-interface-export-registry* (make-hash-table :test #'equal)
  "Maps exported names to monomorphic imported type schemes.")

(defun %type-interface-key (name)
  (if (symbolp name) (string-upcase (symbol-name name)) name))

(defun %type-interface-export-name (export)
  (if (and (consp export) (symbolp (first export)))
      (first export)
      export))

(defun %type-interface-export-type (export)
  (if (and (consp export) (second export))
      (parse-type-specifier (second export))
      type-any))

(defun register-type-interface (module exports fingerprint)
  "Register MODULE's exported type interface and return its summary."
  (let* ((exported-types (mapcar (lambda (export)
                                   (cons (%type-interface-export-name export)
                                         (%type-interface-export-type export)))
                                 exports))
         (summary (%make-type-interface-module :name module
                                               :exports (mapcar #'%type-interface-export-name exports)
                                               :fingerprint fingerprint
                                               :exported-types exported-types)))
    (setf (gethash (%type-interface-key module) *type-interface-registry*) summary)
    (dolist (entry exported-types)
      (setf (gethash (%type-interface-key (car entry)) *type-interface-export-registry*)
            (type-to-scheme (cdr entry))))
    summary))

(defun lookup-type-interface (module)
  "Return the loaded interface summary for MODULE, if present."
  (gethash (%type-interface-key module) *type-interface-registry*))

(defun lookup-type-interface-export (name)
  "Return (values scheme found-p) for an imported interface export NAME."
  (let ((scheme (gethash (%type-interface-key name) *type-interface-export-registry*)))
    (values scheme (not (null scheme)))))

(defvar *smt-solver-registry* (make-hash-table :test #'equal)
  "Maps SMT solver names to local request handlers.")

(defun register-smt-solver (name function)
  "Register FUNCTION as the SMT request handler for NAME."
  (setf (gethash (%type-interface-key name) *smt-solver-registry*) function))

(defun lookup-smt-solver (name)
  "Return the SMT solver handler for NAME, if present."
  (gethash (%type-interface-key name) *smt-solver-registry*))

(defun solve-smt-constraint (constraint solver theory)
  "Dispatch an SMT constraint request through the registered solver handler."
  (let ((handler (lookup-smt-solver solver)))
    (unless handler
      (error "No SMT solver registered for ~S" solver))
    (funcall handler constraint theory)))

(defun %default-smt-solver (constraint theory)
  "Deterministic local SMT shim used for type-checker integration tests."
  (list :status :unknown :constraint constraint :theory theory :counterexample :none))

(defvar *type-checker-plugin-registry* (make-hash-table :test #'equal)
  "Maps (plugin . phase) to type-checker plugin handlers.")

(defun %type-plugin-key (plugin phase)
  (cons (%type-interface-key plugin) (%type-interface-key phase)))

(defun register-type-checker-plugin (plugin phase function)
  "Register FUNCTION as PLUGIN's PHASE hook."
  (setf (gethash (%type-plugin-key plugin phase) *type-checker-plugin-registry*) function))

(defun lookup-type-checker-plugin (plugin phase)
  "Return PLUGIN's PHASE hook, if present."
  (gethash (%type-plugin-key plugin phase) *type-checker-plugin-registry*))

(defun run-type-checker-plugin (plugin phase ast arg-types env)
  "Run a registered type-checker plugin hook."
  (let ((handler (lookup-type-checker-plugin plugin phase)))
    (unless handler
      (error "No type-checker plugin registered for ~S phase ~S" plugin phase))
    (funcall handler ast arg-types env)))

(defun %default-type-plugin (_ast _arg-types _env)
  "Default plugin hook result for built-in plugin smoke paths."
  (declare (ignore _ast _arg-types _env))
  (list :status :ok :type type-any))

(defvar *type-synthesis-strategy-registry* (make-hash-table :test #'equal)
  "Maps synthesis strategy names to candidate search handlers.")

(defun register-type-synthesis-strategy (strategy function)
  "Register FUNCTION as the handler for synthesis STRATEGY."
  (setf (gethash (%type-interface-key strategy) *type-synthesis-strategy-registry*) function))

(defun lookup-type-synthesis-strategy (strategy)
  "Return the synthesis handler for STRATEGY, if present."
  (gethash (%type-interface-key strategy) *type-synthesis-strategy-registry*))

(defun run-type-synthesis (signature strategy fuel)
  "Run type-directed synthesis for SIGNATURE using STRATEGY and FUEL."
  (let ((handler (lookup-type-synthesis-strategy strategy)))
    (unless handler
      (error "No type synthesis strategy registered for ~S" strategy))
    (funcall handler signature fuel)))

(defun %default-type-synthesis (signature fuel)
  "Deterministic bounded synthesis shim returning a typed candidate summary."
  (list :status (if (plusp fuel) :candidate :exhausted)
        :signature signature
        :fuel fuel
        :candidate (when (plusp fuel) '(lambda (&rest args) (declare (ignore args)) nil))))

(defun %initialize-advanced-dispatch-registries ()
  "Install deterministic local handlers for advanced integration shims."
  (register-smt-solver 'z3 #'%default-smt-solver)
  (register-smt-solver 'cvc5 #'%default-smt-solver)
  (register-type-checker-plugin 'nat-normalise 'solve #'%default-type-plugin)
  (register-type-checker-plugin 'nat-normalise 'rewrite #'%default-type-plugin)
  (register-type-synthesis-strategy 'enumerative #'%default-type-synthesis)
  (register-type-synthesis-strategy 'refinement #'%default-type-synthesis)
  (register-type-synthesis-strategy 'proof-search #'%default-type-synthesis)
  t)


