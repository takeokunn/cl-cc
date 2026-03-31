;;;; compile/context.lisp - Compiler Context and Primitive Helpers
(in-package :cl-cc)

(defclass compiler-context ()
  ((instructions :initform nil :accessor ctx-instructions)
   (next-register :initform 0 :accessor ctx-next-register)
   (next-label :initform 0 :accessor ctx-next-label)
   (env :initform nil :accessor ctx-env)
   (type-env :initform nil :accessor ctx-type-env
              :documentation "Type environment accumulated during top-level compilation")
   (safety :initarg :safety :initform 1 :accessor ctx-safety
           :documentation "Compiler safety level; 0 suppresses runtime type assertions.")
   (block-env :initform nil :accessor ctx-block-env
               :documentation "Alist mapping block names to (exit-label . result-reg)")
   (tagbody-env :initform nil :accessor ctx-tagbody-env
                :documentation "Alist mapping tags to labels within current tagbody")
   (global-functions :initform (make-hash-table :test #'eq) :accessor ctx-global-functions
                     :documentation "Hash table mapping function names to their labels")
   (global-variables :initform (make-hash-table :test #'eq) :accessor ctx-global-variables
                     :documentation "Hash table mapping global variable names to their registers")
   (global-classes :initform (make-hash-table :test #'eq) :accessor ctx-global-classes
                   :documentation "Hash table mapping class names to their class descriptor registers")
   (global-generics :initform (make-hash-table :test #'eq) :accessor ctx-global-generics
                    :documentation "Hash table mapping generic function names to their GF registers")
   (top-level-p :initform t :accessor ctx-top-level-p
                :documentation "Whether we are at top-level (not inside a function body)")
   (boxed-vars :initform nil :accessor ctx-boxed-vars
               :documentation "List of variable names that are boxed (stored in cons cells for capture-by-reference)")
   (tail-position :initform nil :accessor ctx-tail-position
                  :documentation "Whether the current compilation position is a tail position.")))



;;; Pre-populate global-variables with ANSI CL special variables
;;; that the VM state initializes, so compiled code can access them
;;; without requiring explicit (defvar ...) forms.
(defparameter *builtin-special-variables*
  '(*features* *modules* *active-restarts*
    *standard-output* *standard-input* *error-output*
    *trace-output* *debug-io* *query-io*
    *print-base* *print-radix* *print-circle* *print-pretty*
    *print-level* *print-length* *print-escape* *print-readably*
    *print-gensym* *random-state* *readtable*
    internal-time-units-per-second
    *package* *%condition-handlers* *%active-restarts*)
  "Variables known to exist in the VM global environment at startup.")

(defvar *repl-global-variables* nil
  "When non-nil, a persistent hash table of global variable names accumulated
across REPL evaluations.  Merged into each new compiler-context so that
variables defined by (defvar ...) in one REPL call are visible in subsequent calls.")

(defvar *repl-label-counter* nil
  "When non-nil, the starting label counter for the next REPL compilation.
Ensures labels are globally unique across REPL calls (prevents label collision
when multiple compilations share the same instruction pool).")

(defvar *repl-capture-label-counter* nil
  "When non-nil, set to the final ctx-next-label after compilation completes.
Used by run-string-repl to persist the label counter across calls.")

(defmethod initialize-instance :after ((ctx compiler-context) &key &allow-other-keys)
  "Register built-in special variables so they compile to vm-get-global.
  Also merges any persistent REPL globals and continues label counter."
  (let ((gv (ctx-global-variables ctx)))
    (dolist (name *builtin-special-variables*)
      (setf (gethash name gv) t))
    ;; Merge persistent REPL globals
    (when *repl-global-variables*
      (maphash (lambda (k v) (setf (gethash k gv) v))
               *repl-global-variables*)))
  ;; Continue label counter from previous REPL compilation to avoid collisions
  (when *repl-label-counter*
    (setf (ctx-next-label ctx) *repl-label-counter*))
  (unless (ctx-type-env ctx)
    (setf (ctx-type-env ctx) (cl-cc/type:type-env-empty)))
  )

(defvar *labels-boxed-fns* nil
  "Alist mapping labels function names to their box registers for mutual recursion.")

(defvar *compiling-typed-fn* nil
  "When non-NIL, the name (or T for anonymous lambdas) of the typed function currently
   being compiled. Activates strict compile-time type checking at ast-the nodes.")

(defun make-register (ctx)
  (let ((reg (intern (format nil "R~D" (ctx-next-register ctx)) :keyword)))
    (incf (ctx-next-register ctx))
    reg))

(defun make-label (ctx prefix)
  (let ((name (format nil "~A_~D" prefix (ctx-next-label ctx))))
    (incf (ctx-next-label ctx))
    name))

(defun emit (ctx instruction)
  (push instruction (ctx-instructions ctx))
  instruction)

(defun lookup-var (ctx sym)
  (let ((entry (assoc sym (ctx-env ctx))))
    (unless entry
      (error "Unbound variable: ~S" sym))
    (cdr entry)))
