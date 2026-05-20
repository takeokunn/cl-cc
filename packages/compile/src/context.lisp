;;;; compile/context.lisp - Compiler Context and Primitive Helpers
(in-package :cl-cc/compile)

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
     (global-function-mv-arities :initform (make-hash-table :test #'eq) :accessor ctx-global-function-mv-arities
                                 :documentation "Hash table mapping function names to statically known small multiple-value arities.")
     (function-conventions :initform (make-hash-table :test #'equal) :accessor ctx-function-conventions
                           :documentation "Hash table mapping function labels to :external or :internal calling conventions.")
    (global-variables :initform (make-hash-table :test #'eq) :accessor ctx-global-variables
                      :documentation "Hash table mapping global variable names to their registers")
    (global-var-cache :initform nil :accessor ctx-global-var-cache
                      :documentation "Function-local alist mapping cached global variable names to temporary registers.")
   (global-classes :initform (make-hash-table :test #'eq) :accessor ctx-global-classes
                   :documentation "Hash table mapping class names to their class descriptor registers")
    (global-generics :initform (make-hash-table :test #'eq) :accessor ctx-global-generics
                     :documentation "Hash table mapping generic function names to their GF registers")
    (global-generic-params :initform (make-hash-table :test #'eq) :accessor ctx-global-generic-params
                           :documentation "Hash table mapping generic function names to their lambda-list params (for congruence checking)")
    (current-function-name :initform nil :accessor ctx-current-function-name
                           :documentation "Name of the function currently being compiled, if any")
    (current-function-label :initform nil :accessor ctx-current-function-label
                            :documentation "Entry label of the function currently being compiled")
    (current-function-params :initform nil :accessor ctx-current-function-params
                             :documentation "Required parameter symbols for the current function")
     (current-function-simple-p :initform nil :accessor ctx-current-function-simple-p
                                :documentation "Whether the current function has only required parameters")
     (pending-inline-policy :initform nil :accessor ctx-pending-inline-policy
                            :documentation "Temporary inline policy for a closure-valued expression being compiled in the current lexical context")
     (top-level-p :initform t :accessor ctx-top-level-p
                  :documentation "Whether we are at top-level (not inside a function body)")
   (boxed-vars :initform nil :accessor ctx-boxed-vars
                :documentation "List of variable names that are boxed (stored in cons cells for capture-by-reference)")
   (noescape-cons-bindings :initform nil :accessor ctx-noescape-cons-bindings
                           :documentation "Alist mapping local variable names to (car-reg . cdr-reg) for conservative non-escaping cons bindings.")
   (noescape-array-bindings :initform nil :accessor ctx-noescape-array-bindings
                            :documentation "Alist mapping local variable names to (size . element-regs) for conservative non-escaping fixed-size arrays.")
   (noescape-instance-bindings :initform nil :accessor ctx-noescape-instance-bindings
                               :documentation "Alist mapping local variable names to slot-name-string → register alists for conservative non-escaping make-instance bindings.")
    (noescape-closure-bindings :initform nil :accessor ctx-noescape-closure-bindings
                               :documentation "Alist mapping local variable names to directly inlinable non-escaping lambda ASTs.")
    (hash-table-test-bindings :initform nil :accessor ctx-hash-table-test-bindings
                              :documentation "Alist mapping local hash-table variable names to statically known tests for specialized lookup lowering.")
    (tail-position :initform nil :accessor ctx-tail-position
                       :documentation "Whether the current compilation position is a tail position.")
     (target :initarg :target :initform :vm :accessor ctx-target
             :documentation "Current code generation target (:vm, :x86_64, :wasm, ...).")
      (diagnostics :initform nil :accessor ctx-diagnostics
                   :documentation "Structured compiler diagnostics accumulated during code generation.")))

(defun %make-compile-warning (message &key source-file span error-code fix-it)
  "Create a structured compiler warning diagnostic."
  (make-diagnostic :severity :warning
                   :message message
                   :span (or span (cons 0 0))
                   :source-file source-file
                   :error-code error-code
                   :fix-it fix-it))

(defun %emit-compile-warning (ctx message &key source-file span error-code fix-it)
  "Record a structured compiler warning in CTX and return it."
  (let ((diagnostic (%make-compile-warning message
                                           :source-file source-file
                                           :span span
                                           :error-code error-code
                                           :fix-it fix-it)))
    (push diagnostic (ctx-diagnostics ctx))
    diagnostic))

(defun %hash-table-keyword-ast-p (ast keyword)
  (or (and (typep ast 'ast-var) (eq (ast-var-name ast) keyword))
      (and (typep ast 'ast-quote) (eq (ast-quote-value ast) keyword))))

(defun %hash-table-test-symbol-p (symbol)
  (or (eq symbol 'eq)
      (eq symbol 'eql)
      (eq symbol 'equal)
      (eq symbol 'equalp)))

(defun %hash-table-test-symbol-from-ast (ast)
  (typecase ast
    (ast-quote
     (let ((name (ast-quote-value ast)))
       (when (and (symbolp name) (%hash-table-test-symbol-p name)) name)))
    (ast-function
     (let ((name (ast-function-name ast)))
       (when (and (symbolp name) (%hash-table-test-symbol-p name)) name)))))

(defun %hash-table-make-hash-table-call-p (ast)
  (and (typep ast 'ast-call)
       (let ((func (ast-call-func ast)))
         (or (eq func 'make-hash-table)
             (and (typep func 'ast-var)
                  (eq (ast-var-name func) 'make-hash-table))))))

(defun %hash-table-static-test-from-make-hash-table-ast (ast)
  "Return statically known MAKE-HASH-TABLE :TEST from AST, if any."
  (when (%hash-table-make-hash-table-call-p ast)
    (loop for kv on (ast-call-args ast) by #'cddr
          when (and (cdr kv) (%hash-table-keyword-ast-p (car kv) :test))
            return (%hash-table-test-symbol-from-ast (cadr kv))
          finally (return 'eql))))

(defun %merge-inline-policies (&rest policies)
  (cond
    ((some (lambda (policy) (eq policy :notinline)) policies) :notinline)
    ((some (lambda (policy) (eq policy :inline)) policies) :inline)
    (t nil)))

(defun %global-optimize-quality (quality)
  (and (symbolp quality)
       (gethash quality cl-cc/expand:*declaim-optimize-registry*)))

(defun %local-optimize-quality (declarations quality)
  (cl-cc/expand:declaration-optimize-quality declarations quality))

(defun %optimize-inline-policy-from-levels (speed debug space)
  (cond
    ((or (eql debug 3)
         (and (integerp space) (>= space 2)))
     :notinline)
    ((eql speed 3)
     :inline)
    (t nil)))

(defun %local-optimize-inline-policy (declarations)
  (%optimize-inline-policy-from-levels
   (%local-optimize-quality declarations 'speed)
   (%local-optimize-quality declarations 'debug)
   (%local-optimize-quality declarations 'space)))

(defun %global-optimize-inline-policy ()
  (%optimize-inline-policy-from-levels
   (%global-optimize-quality 'speed)
   (%global-optimize-quality 'debug)
   (%global-optimize-quality 'space)))

(defun %call-with-declaration-policies (ctx declarations thunk)
  (let ((old-safety (ctx-safety ctx))
        (old-inline-policy (ctx-pending-inline-policy ctx))
        (local-safety (%local-optimize-quality declarations 'safety))
        (local-inline-policy (%local-optimize-inline-policy declarations)))
    (unwind-protect
         (progn
           (when (not (null local-safety))
             (setf (ctx-safety ctx) local-safety))
           (setf (ctx-pending-inline-policy ctx)
                 (%merge-inline-policies old-inline-policy local-inline-policy))
           (funcall thunk))
      (setf (ctx-safety ctx) old-safety)
      (setf (ctx-pending-inline-policy ctx) old-inline-policy))))

(defun %declaration-inline-policy (declarations name)
  "Return the effective local inline policy for NAME from DECLARATIONS.
When both INLINE and NOTINLINE appear, NOTINLINE wins conservatively."
  (when (symbolp name)
    (loop with result = nil
          for decl in declarations
          when (and (consp decl)
                    (member (car decl) '(inline notinline))
                    (member name (cdr decl) :test #'eq))
            do (setf result (%merge-inline-policies
                              result
                              (if (eq (car decl) 'notinline) :notinline :inline)))
          finally (return result))))

(defun %global-inline-policy (name)
  (and (symbolp name)
       (gethash name cl-cc/expand:*declaim-inline-registry*)))

(defun %external-symbol-p (name)
  "Return T when NAME is an external symbol in its home package."
  (and (symbolp name)
       (symbol-package name)
       (multiple-value-bind (symbol status)
           (find-symbol (symbol-name name) (symbol-package name))
         (and (eq symbol name) (eq status :external)))))

(defun note-function-calling-convention (ctx label convention)
  "Record LABEL as using CONVENTION in CTX."
  (setf (gethash label (ctx-function-conventions ctx)) convention)
  convention)

(defun function-calling-convention-for-defun (name &key stored-global-p)
  "Return the conservative calling convention for function NAME."
  (if (or stored-global-p (%external-symbol-p name))
      :external
      :internal))

(defun %callable-inline-policy (declarations &key name pending-policy)
  "Return the merged inline policy for a callable.
Pending lexical policy, optimize declarations, and explicit declaim/declare
inline policy are merged with NOTINLINE taking precedence over INLINE."
  (%merge-inline-policies pending-policy
                           (%global-optimize-inline-policy)
                           (%local-optimize-inline-policy declarations)
                           (%global-inline-policy name)
                           (%declaration-inline-policy declarations name)))

(defun %safe-parse-declaration-type (type-spec)
  "Parse TYPE-SPEC for declaration processing, returning NIL when unsupported."
  (handler-case
      (parse-type-specifier type-spec)
    (error () nil)))

(defun %declaration-type-bindings (declarations)
  "Return `(NAME . TYPE-SCHEME)` bindings from local type DECLARATIONS.

Supports the standard `(type TYPE NAME...)` form and the Common Lisp shorthand
`(TYPE NAME...)` when TYPE looks like a known type specifier. Malformed clauses
are ignored conservatively, matching existing optimize declaration handling."
  (let ((bindings nil))
    (dolist (decl declarations (nreverse bindings))
      (when (consp decl)
        (let* ((head (car decl))
               (explicit-type-p (eq head 'type))
               (type-spec (if explicit-type-p (second decl) head))
               (names (if explicit-type-p (cddr decl) (cdr decl))))
          (when (and names
                     (or explicit-type-p
                         (looks-like-type-specifier-p type-spec)))
            (let ((parsed (%safe-parse-declaration-type type-spec)))
              (when parsed
                (dolist (name names)
                  (when (symbolp name)
                    (push (cons name (type-to-scheme parsed)) bindings)))))))))))

(defun %extend-type-env-from-declarations (ctx declarations)
  "Register local type declaration bindings in CTX's type environment."
  (let ((bindings (%declaration-type-bindings declarations)))
    (when bindings
      (setf (ctx-type-env ctx)
            (type-env-extend* bindings (ctx-type-env ctx))))))



;;; Pre-populate global-variables with ANSI CL special variables
;;; that the VM state initializes, so compiled code can access them
;;; without requiring explicit (defvar ...) forms.

(defparameter *builtin-package-symbol-specs*
  '(("CL-CC/PROLOG"  . "*BUILTIN-PREDICATES*")
    ("CL-CC/MIR"     . "*X86-64-TARGET*")
    ("CL-CC/MIR"     . "*AARCH64-TARGET*")
    ("CL-CC/MIR"     . "*RISCV64-TARGET*")
    ("CL-CC/MIR"     . "*WASM32-TARGET*")
    ("CL-CC/PARSE"   . "*LIST-LOWERING-TABLE*")
    ("CL-CC/EXPAND"  . "*%CONDITION-HANDLERS*")
    ("CL-CC/EXPAND"  . "*%ACTIVE-RESTARTS*")
    ("CL-CC/VM"      . "*VM-HOST-BRIDGE-FUNCTIONS*")
    ("CL-CC/VM"      . "*INSTRUCTION-CONSTRUCTORS*")
    ("CL-CC/VM"      . "*OPCODE-DISPATCH-TABLE*")
    ("CL-CC/VM"      . "*OPCODE-NAME-TABLE*")
    ("CL-CC/VM"      . "*OPCODE-ENCODER-TABLE*")
    ("CL-CC/COMPILE" . "*BUILTIN-UNARY-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-BINARY-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-STRING-CMP-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-CHAR-CMP-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-TABLE-QUERY-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-HANDLE-INPUT-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-SIDE-EFFECT-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-VOID-SIDE-EFFECT-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-NULLARY-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-STRING-TRIM-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-HANDLE-EFFECT-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-BINARY-CUSTOM-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-BINARY-MOVE-FIRST-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-BINARY-VOID-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-UNARY-CUSTOM-VOID-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-UNARY-OPT-NIL-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-BINARY-OPT-ONE-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-BINARY-OPT-NIL-SLOT-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-TERNARY-OPT-NIL-CUSTOM-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-BINARY-SYNTH-ZERO-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-UNARY-CUSTOM-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-ZERO-COMPARE-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-STREAM-INPUT-OPT-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-STREAM-VOID-OPT-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-STREAM-WRITE-VAL-ENTRIES*")
    ("CL-CC/COMPILE" . "*BUILTIN-TERNARY-CUSTOM-ENTRIES*"))
  "Package-symbol pairs for variables that must exist in the VM global environment.
Used both to build *builtin-special-variables* and to populate new compiler contexts.")

(defun %context-find-package (name)
  "Resolve package NAME through the runtime package layer when available.
Returns NIL when the runtime package layer is unavailable." 
  (when cl-cc/bootstrap:*runtime-find-package-fn*
    (funcall cl-cc/bootstrap:*runtime-find-package-fn* name)))

(defun %resolve-package-symbol-specs (specs)
  "Resolve (PACKAGE . NAME) pairs to live symbols, skipping missing packages/symbols."
  (loop for (pkg-name . sym-name) in specs
        when (find-package pkg-name)
          collect (intern sym-name pkg-name)))

(defparameter *builtin-special-variables*
  (append '(*features* *modules* *active-restarts*
            *standard-output* *standard-input* *terminal-io* *error-output*
            *trace-output* *debug-io* *query-io*
            *print-base* *print-radix* *print-circle* *print-pretty*
            *print-level* *print-length* *print-escape* *print-readably*
            *print-gensym* *random-state* *read-eval*
            internal-time-units-per-second
            *package* *%condition-handlers* *%active-restarts*)
          (%resolve-package-symbol-specs *builtin-package-symbol-specs*))
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
    ;; Re-resolve at context creation time: new packages may have loaded since startup.
    (dolist (sym (%resolve-package-symbol-specs *builtin-package-symbol-specs*))
      (setf (gethash sym gv) t))
    ;; Merge persistent REPL globals
    (when *repl-global-variables*
      (maphash (lambda (k v) (setf (gethash k gv) v))
               *repl-global-variables*)))
  ;; Continue label counter from previous REPL compilation to avoid collisions
  (when *repl-label-counter*
    (setf (ctx-next-label ctx) *repl-label-counter*))
  (unless (ctx-type-env ctx)
    (setf (ctx-type-env ctx) (type-env-empty)))
  )

(defvar *labels-boxed-fns* nil
  "Alist mapping labels function names to their box registers for mutual recursion.")

(defvar *local-tail-jump-fns* nil
  "Alist mapping non-escaping local tail functions to (label params param-regs).")

(defvar *compiling-typed-fn* nil
  "When non-NIL, the name (or T for anonymous lambdas) of the typed function currently
   being compiled. Activates strict compile-time type checking at ast-the nodes.")

(defun make-register (ctx)
  (let ((reg (intern (format nil "R~D" (ctx-next-register ctx)) :keyword)))
    (setf (ctx-next-register ctx) (+ (ctx-next-register ctx) 1))
    reg))

(defun make-label (ctx prefix)
  (let ((name (format nil "~A_~D" prefix (ctx-next-label ctx))))
    (setf (ctx-next-label ctx) (+ (ctx-next-label ctx) 1))
    name))

(defun emit (ctx instruction)
  (push instruction (ctx-instructions ctx))
  instruction)

(defun %assoc-eq (key alist)
  (assoc key alist :test #'eq))

(defun %member-eq-p (item list)
  (and (member item list :test #'eq) t))

(defun %list-union-eq (left right)
  (union left right :test #'eq))

(defun %list-intersection-eq (left right)
  (intersection left right :test #'eq))

(defun lookup-var (ctx sym)
  (let ((entry (assoc sym (ctx-env ctx) :test #'eq)))
    (if entry
        (cdr entry)
        (error "Unbound variable"))))

(defun %capture-candidates-from-env (env)
  "Return (var . reg) capture candidates from ENV in lookup precedence order."
  (loop for entry in env
        when (and (consp entry) (symbolp (car entry)))
          collect entry))

;;; ─── CPS Safety Guards ───────────────────────────────────────────────────────

(defparameter *cps-compile-unsupported-ast-types*
  '(cl-cc/ast:ast-hole
    cl-cc/ast:ast-slot-def
    cl-cc/ast:ast-block
    cl-cc/ast:ast-return-from
    cl-cc/ast:ast-tagbody
    cl-cc/ast:ast-go
    cl-cc/ast:ast-defvar
    cl-cc/ast:ast-defclass
    cl-cc/ast:ast-call
    cl-cc/ast:ast-lambda
    cl-cc/ast:ast-apply
    cl-cc/ast:ast-multiple-value-call
    cl-cc/ast:ast-catch
    cl-cc/ast:ast-throw
    cl-cc/ast:ast-unwind-protect
    cl-cc/ast:ast-handler-case
    cl-cc/ast:ast-flet
    cl-cc/ast:ast-labels)
  "AST node types excluded from CPS compilation.
Definition forms, non-local control flow, and dynamic control must run on the
direct compile path until the VM CPS route proves semantically equivalent.")

(defparameter *cps-native-compile-unsupported-ast-types*
  '(cl-cc/ast:ast-node)
  "AST node types excluded from native CPS compilation.
Definition forms, control flow, and CLOS nodes require direct x86_64/aarch64 codegen.")

(defvar *enable-cps-vm-primary-path* t
  "When true, compile-expression prefers a CPS-backed VM compilation path for safe ASTs.")

(defvar *compile-expression-cps-recursion-guard* nil
  "Internal guard to prevent compile-expression from recursively re-entering the CPS primary path.")

(defun %cps-compile-safe-ast-p (ast unsupported-types)
  "Return T when AST and all descendants avoid UNSUPPORTED-TYPES."
  (and (typep ast 'cl-cc/ast:ast-node)
       (notany (lambda (type) (typep ast type)) unsupported-types)
        (every (lambda (child)
                 (%cps-compile-safe-ast-p child unsupported-types))
               (cl-cc/ast:ast-children ast))))

(defun %cps-vm-compile-safe-ast-p (ast)
  "Return T when AST and all descendants are safe for VM CPS compilation."
  (%cps-compile-safe-ast-p ast *cps-compile-unsupported-ast-types*))

(defun %cps-native-compile-safe-ast-p (ast)
  "Return T when AST and all descendants are safe for native (x86_64/aarch64) CPS compilation."
  (%cps-compile-safe-ast-p ast *cps-native-compile-unsupported-ast-types*))

(defun %cps-identity-entry-form (cps-form)
  "Wrap CPS-FORM with an identity continuation for VM/native compilation."
  (list cps-form '(lambda (value) value)))
