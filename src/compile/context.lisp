;;;; compile/context.lisp - Compiler Context and Primitive Helpers
(in-package :cl-cc)

(defclass compiler-context ()
  ((instructions :initform nil :accessor ctx-instructions)
   (next-register :initform 0 :accessor ctx-next-register)
   (next-label :initform 0 :accessor ctx-next-label)
   (env :initform nil :accessor ctx-env)
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
               :documentation "List of variable names that are boxed (stored in cons cells for capture-by-reference)")))



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

