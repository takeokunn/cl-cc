;;;; compile/codegen.lisp - AST to VM IR Compilation
(in-package :cl-cc)

;; Forward declarations (defined in optimize/ and emit/ loaded after this file)
(declaim (ftype function optimize-instructions emit-assembly))

(defgeneric compile-ast (node ctx))

(defmethod compile-ast ((node ast-int) ctx)
  (let ((dst (make-register ctx)))
    (emit ctx (make-vm-const :dst dst :value (ast-int-value node)))
    dst))

(defmethod compile-ast ((node ast-var) ctx)
  (let ((name (ast-var-name node)))
    (cond
      ;; Self-evaluating constants: t, nil, and keywords are immediate values
      ((or (eq name t) (eq name nil) (keywordp name))
       (let ((dst (make-register ctx)))
         (emit ctx (make-vm-const :dst dst :value name))
         dst))
      (t
       (let ((local-entry (assoc name (ctx-env ctx))))
         (cond
           ;; Boxed local: mutated AND captured variable — unbox via (car box)
           ((and local-entry (member name (ctx-boxed-vars ctx)))
            (let ((dst (make-register ctx)))
              (emit ctx (make-vm-car :dst dst :src (cdr local-entry)))
              dst))
           ;; Plain local binding: return its register directly
           (local-entry
            (cdr local-entry))
           ;; Global variable: load from the persistent global store
           ((gethash name (ctx-global-variables ctx))
            (let ((dst (make-register ctx)))
              (emit ctx (make-vm-get-global :dst dst :name name))
              dst))
           (t
            (error "Unbound variable: ~S" name))))))))

;;; Binary operator → VM instruction constructor tables (data layer)

(defparameter *typed-binop-ctors*
  '((+  . make-vm-add)
    (-  . make-vm-sub)
    (*  . make-vm-mul)
    (=  . make-vm-num-eq)
    (<  . make-vm-lt)
    (>  . make-vm-gt)
    (<= . make-vm-le)
    (>= . make-vm-ge))
  "Maps arithmetic/comparison operators to their typed (fixnum) VM instruction constructors.")

(defparameter *generic-binop-ctors*
  '((+  . make-vm-generic-add)
    (-  . make-vm-generic-sub)
    (*  . make-vm-generic-mul)
    (=  . make-vm-generic-eq)
    (<  . make-vm-generic-lt)
    (>  . make-vm-generic-gt)
    (<= . make-vm-le)
    (>= . make-vm-ge))
  "Maps arithmetic/comparison operators to their polymorphic VM instruction constructors.")

(defun binop-ctor (op &optional generic-p)
  "Return the instruction constructor for OP.
GENERIC-P selects the polymorphic variant; default is typed (fixnum)."
  (let* ((table (if generic-p *generic-binop-ctors* *typed-binop-ctors*))
         (entry (assoc op table)))
    (unless entry
      (error "Unknown binary operator: ~S" op))
    (symbol-function (cdr entry))))

(defmethod compile-ast ((node ast-binop) ctx)
  (let* ((lhs-reg (compile-ast (ast-binop-lhs node) ctx))
         (rhs-reg (compile-ast (ast-binop-rhs node) ctx))
         (dst (make-register ctx))
         (op (ast-binop-op node))
         ;; Default: typed (fixnum) instructions.
         ;; Future: when type inference shows operands are polymorphic,
         ;; pass :generic-p t to emit vm-generic-* for runtime dispatch.
         (ctor (binop-ctor op)))
    (emit ctx (funcall ctor :dst dst :lhs lhs-reg :rhs rhs-reg))
    dst))

(defmethod compile-ast ((node ast-progn) ctx)
  (let ((last nil))
    (dolist (form (ast-progn-forms node))
      (setf last (compile-ast form ctx)))
    last))

(defmethod compile-ast ((node ast-print) ctx)
  (let ((reg (compile-ast (ast-print-expr node) ctx)))
    (emit ctx (make-vm-print :reg reg))
    reg))

(defmethod compile-ast ((node ast-if) ctx)
  (let* ((cond-reg (compile-ast (ast-if-cond node) ctx))
         (dst (make-register ctx))
         (else-label (make-label ctx "else"))
         (end-label (make-label ctx "ifend")))
    (emit ctx (make-vm-jump-zero :reg cond-reg :label else-label))
    (let ((then-reg (compile-ast (ast-if-then node) ctx)))
      (emit ctx (make-vm-move :dst dst :src then-reg))
      (emit ctx (make-vm-jump :label end-label)))
    (emit ctx (make-vm-label :name else-label))
    (let ((else-reg (compile-ast (ast-if-else node) ctx)))
      (emit ctx (make-vm-move :dst dst :src else-reg)))
    (emit ctx (make-vm-label :name end-label))
    dst))

(defmethod compile-ast ((node ast-let) ctx)
  (let ((old-env (ctx-env ctx))
        (old-boxed (ctx-boxed-vars ctx)))
    (unwind-protect
         (progn
           ;; Determine which let-bound variables need boxing
           ;; A variable needs boxing if it's both:
           ;;   1. Captured by an inner lambda/defun (free variable of a nested closure)
           ;;   2. Mutated via setq anywhere in the let body
           (let* ((binding-names (mapcar #'car (ast-let-bindings node)))
                  (body-forms (ast-let-body node))
                  (mutated (reduce #'union (mapcar #'find-mutated-variables body-forms)
                                   :initial-value nil))
                  (captured (find-captured-in-children body-forms binding-names))
                  (needs-boxing (intersection mutated captured))
                  (new-bindings nil))
             (dolist (binding (ast-let-bindings node))
               (let* ((name (car binding))
                      (expr (cdr binding))
                      (val-reg (compile-ast expr ctx))
                      ;; Always copy to fresh register to avoid aliasing
                      (own-reg (make-register ctx)))
                 (emit ctx (make-vm-move :dst own-reg :src val-reg))
                 (if (member name needs-boxing)
                     ;; Box the variable: wrap in (cons val nil)
                     (let ((box-reg (make-register ctx))
                           (nil-reg (make-register ctx)))
                       (emit ctx (make-vm-const :dst nil-reg :value nil))
                       (emit ctx (make-vm-cons :dst box-reg :car-src own-reg :cdr-src nil-reg))
                       (push (cons name box-reg) new-bindings))
                     (push (cons name own-reg) new-bindings))))
             (setf (ctx-env ctx) (append (nreverse new-bindings) (ctx-env ctx)))
             ;; Register boxed variables
             (setf (ctx-boxed-vars ctx)
                   (union needs-boxing (ctx-boxed-vars ctx))))
           (let ((last nil))
             (dolist (form (ast-let-body node))
               (setf last (compile-ast form ctx)))
             last))
      (setf (ctx-env ctx) old-env)
      (setf (ctx-boxed-vars ctx) old-boxed))))

;;; Control Flow: block/return-from

(defun lookup-block (ctx name)
  "Look up a block by name, returning (exit-label . result-reg) or error."
  (let ((entry (assoc name (ctx-block-env ctx))))
    (unless entry
      (error "Unknown block: ~S" name))
    (cdr entry)))

(defmethod compile-ast ((node ast-block) ctx)
  (let* ((block-name (ast-block-name node))
         (exit-label (make-label ctx "block_exit"))
         (result-reg (make-register ctx))
         (old-block-env (ctx-block-env ctx)))
    (unwind-protect
         (progn
           ;; Register the block with its exit label and result register
           (setf (ctx-block-env ctx)
                 (cons (cons block-name (cons exit-label result-reg))
                       (ctx-block-env ctx)))
           ;; Compile the body
           (let ((body-result (let ((last nil))
                                (dolist (form (ast-block-body node))
                                  (setf last (compile-ast form ctx)))
                                last)))
             ;; Normal fall-through: copy result to result register
             (emit ctx (make-vm-move :dst result-reg :src body-result))))
      ;; Restore block environment
      (setf (ctx-block-env ctx) old-block-env))
    ;; Emit the exit label for return-from to jump to
    (emit ctx (make-vm-label :name exit-label))
    result-reg))

(defmethod compile-ast ((node ast-return-from) ctx)
  (let* ((block-name (ast-return-from-name node))
         (block-info (lookup-block ctx block-name))
         (exit-label (car block-info))
         (result-reg (cdr block-info))
         (value-reg (compile-ast (ast-return-from-value node) ctx)))
    ;; Move value to block's result register
    (emit ctx (make-vm-move :dst result-reg :src value-reg))
    ;; Jump to block's exit label
    (emit ctx (make-vm-jump :label exit-label))
    ;; Return the result register (though this won't be reached)
    result-reg))

;;; Control Flow: tagbody/go

(defun lookup-tag (ctx tag)
  "Look up a tag within the current tagbody, returning its label or error."
  (let ((entry (assoc tag (ctx-tagbody-env ctx))))
    (unless entry
      (error "Unknown tag: ~S" tag))
    (cdr entry)))

(defmethod compile-ast ((node ast-tagbody) ctx)
  (let* ((tags (ast-tagbody-tags node))
         (end-label (make-label ctx "tagbody_end"))
         (result-reg (make-register ctx))
         (old-tagbody-env (ctx-tagbody-env ctx))
         ;; Pre-generate labels for all tags
         (tag-labels (mapcar (lambda (tag-entry)
                               (cons (car tag-entry) (make-label ctx "tag")))
                             tags)))
    (unwind-protect
         (progn
           ;; Set up tagbody environment for go statements
           (setf (ctx-tagbody-env ctx)
                 (append tag-labels (ctx-tagbody-env ctx)))
           ;; First, jump to the first tag (if any) or to end if no tags
           (if tag-labels
               (emit ctx (make-vm-jump :label (cdar tag-labels)))
               (emit ctx (make-vm-const :dst result-reg :value nil)))
           ;; Emit code for each tag section
           (dolist (tag-entry tags)
             (let* ((tag (car tag-entry))
                    (forms (cdr tag-entry))
                    (label (cdr (assoc tag tag-labels))))
               ;; Emit the label for this tag
               (emit ctx (make-vm-label :name label))
               ;; Compile forms for this tag
               (when forms
                 (dolist (form forms)
                   (compile-ast form ctx))
                 ;; After last form, fall through to end
                 (emit ctx (make-vm-jump :label end-label))))))
      ;; Restore tagbody environment
      (setf (ctx-tagbody-env ctx) old-tagbody-env))
    ;; Emit the end label, then set result to NIL (tagbody always returns NIL per CL spec)
    (emit ctx (make-vm-label :name end-label))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defmethod compile-ast ((node ast-go) ctx)
  (let* ((tag (ast-go-tag node))
         (label (lookup-tag ctx tag)))
    (emit ctx (make-vm-jump :label label))
    ;; Return a dummy register (won't be reached)
    (make-register ctx)))

;;; Assignment: setq

(defmethod compile-ast ((node ast-setq) ctx)
  (let* ((var-name (ast-setq-var node))
         (value-reg (compile-ast (ast-setq-value node) ctx))
         (local-entry (assoc var-name (ctx-env ctx))))
    (cond
      ;; Local binding takes priority (even if name matches a global)
      ((and local-entry (member var-name (ctx-boxed-vars ctx)))
       ;; Boxed variable: write via (rplaca box new-val)
       (emit ctx (make-vm-rplaca :cons (cdr local-entry) :val value-reg))
       value-reg)
      (local-entry
       ;; Unboxed local: direct register move
       (emit ctx (make-vm-move :dst (cdr local-entry) :src value-reg))
       (cdr local-entry))
      ;; Global variable: write to global store (persists across function calls)
      ((gethash var-name (ctx-global-variables ctx))
       (emit ctx (make-vm-set-global :name var-name :src value-reg))
       value-reg)
      (t
       (error "Unbound variable for setq: ~S" var-name)))))

;;; Quote: literal values

(defmethod compile-ast ((node ast-quote) ctx)
  (let ((dst (make-register ctx))
        (value (ast-quote-value node)))
    ;; For simple values (integers, symbols, strings, lists of simple values)
    ;; we can emit them as constants
    ;; For complex structures, this would need proper serialization
    (emit ctx (make-vm-const :dst dst :value value))
    dst))

;;; The: type declarations (informational only)

(defun type-error-message-from-mismatch (e)
  "Extract a human-readable message from a type-mismatch-error condition."
  (format nil "expected ~A but got ~A"
          (cl-cc/type:type-to-string (cl-cc/type:type-mismatch-error-expected e))
          (cl-cc/type:type-to-string (cl-cc/type:type-mismatch-error-actual e))))

(defmethod compile-ast ((node ast-the) ctx)
  "Compile a type declaration. In typed-function mode, verifies the type at compile time."
  (let ((reg (compile-ast (ast-the-value node) ctx)))
    (when *compiling-typed-fn*
      (let ((declared (ast-the-type node)))
        (when (and declared (not (typep declared 'cl-cc/type:type-unknown)))
          (handler-case
            (let ((tenv (cl-cc/type:type-env-empty)))
              (cl-cc/type:check (ast-the-value node) declared tenv))
            (cl-cc/type:type-mismatch-error (e)
              (error 'ast-compilation-error
                     :location (format nil "~A:~A"
                                       (ast-source-file node)
                                       (ast-source-line node))
                     :format-control "Type error in ~A: ~A"
                     :format-arguments (list *compiling-typed-fn*
                                             (type-error-message-from-mismatch e))))
            ;; Unbound variables and other inference errors just mean the
            ;; type checker lacks context (e.g., function params are not in
            ;; the empty tenv). Skip the check silently.
            (cl-cc/type:type-inference-error () nil)))))
    reg))

;;; CLOS Compilation

(defmethod compile-ast ((node ast-defclass) ctx)
  "Compile a class definition.
Creates a class descriptor hash table with metadata about slots, superclasses,
and a method dispatch table. The descriptor is registered globally."
  (let* ((name (ast-defclass-name node))
         (supers (ast-defclass-superclasses node))
         (slots (ast-defclass-slots node))
         (dst (make-register ctx))
         ;; Extract slot names and build initarg->slot mapping
         (slot-names (mapcar #'ast-slot-name slots))
         (initarg-map (loop for slot in slots
                            when (ast-slot-initarg slot)
                              collect (cons (ast-slot-initarg slot)
                                            (ast-slot-name slot))))
         ;; Compile initform expressions and build slot->register mapping
         (initform-regs (loop for slot in slots
                              when (ast-slot-initform slot)
                                collect (cons (ast-slot-name slot)
                                              (compile-ast (ast-slot-initform slot) ctx)))))
    ;; Emit class definition instruction
    (emit ctx (make-vm-class-def
                            :dst dst
                            :class-name name
                            :superclasses supers
                            :slot-names slot-names
                            :slot-initargs initarg-map
                            :slot-initform-regs initform-regs))
    ;; Register globally (compiler-side)
    (setf (gethash name (ctx-global-classes ctx)) dst)
    (push (cons name dst) (ctx-env ctx))
    ;; Also persist in global variable store so cross-call REPL make-instance works
    (emit ctx (make-vm-set-global :name name :src dst))
    (setf (gethash name (ctx-global-variables ctx)) dst)
    ;; Compile reader/writer/accessor methods as closures
    (dolist (slot slots)
      (let ((slot-name (ast-slot-name slot)))
        ;; Reader method: (lambda (obj) (slot-value obj 'slot-name))
        (when (ast-slot-reader slot)
          (compile-slot-accessor ctx dst name slot-name (ast-slot-reader slot) :reader))
        ;; Writer method: (lambda (new-val obj) (setf (slot-value obj 'slot-name) new-val))
        (when (ast-slot-writer slot)
          (compile-slot-accessor ctx dst name slot-name (ast-slot-writer slot) :writer))
        ;; Accessor = reader + writer; also register in accessor-slot-map for setf expansion
        (when (ast-slot-accessor slot)
          (compile-slot-accessor ctx dst name slot-name (ast-slot-accessor slot) :reader)
          (setf (gethash (ast-slot-accessor slot) *accessor-slot-map*)
                (cons name slot-name)))))
    dst))

(defun compile-slot-accessor (ctx class-reg class-name slot-name accessor-name kind)
  "Compile a slot reader or writer as a defun-style closure."
  (let* ((func-label (make-label ctx (format nil "~A_~A" kind accessor-name)))
         (end-label (make-label ctx (format nil "~A_~A_END" kind accessor-name)))
         (closure-reg (make-register ctx)))
    (ecase kind
      (:reader
       (let ((obj-reg (make-register ctx))
             (result-reg (make-register ctx)))
         ;; Create closure with one parameter
         (emit ctx (make-vm-closure
                                 :dst closure-reg
                                 :label func-label
                                 :params (list obj-reg)
                                 :captured nil))
         ;; Register as a callable function
         (push (cons accessor-name closure-reg) (ctx-env ctx))
         (setf (gethash accessor-name (ctx-global-functions ctx)) func-label)
         ;; Persist in VM function registry for cross-call REPL access (mirrors defun)
         (emit ctx (make-vm-register-function :name accessor-name :src closure-reg))
         ;; Jump over function body
         (emit ctx (make-vm-jump :label end-label))
         ;; Function body: read slot
         (emit ctx (make-vm-label :name func-label))
         (emit ctx (make-vm-slot-read
                                 :dst result-reg
                                 :obj-reg obj-reg
                                 :slot-name slot-name))
         (emit ctx (make-vm-ret :reg result-reg))
         (emit ctx (make-vm-label :name end-label))))
      (:writer
       (let ((val-reg (make-register ctx))
             (obj-reg (make-register ctx)))
         ;; Create closure with two parameters: (new-value object)
         (emit ctx (make-vm-closure
                                 :dst closure-reg
                                 :label func-label
                                 :params (list val-reg obj-reg)
                                 :captured nil))
         (push (cons accessor-name closure-reg) (ctx-env ctx))
         (setf (gethash accessor-name (ctx-global-functions ctx)) func-label)
         ;; Persist in VM function registry for cross-call REPL access (mirrors defun)
         (emit ctx (make-vm-register-function :name accessor-name :src closure-reg))
         (emit ctx (make-vm-jump :label end-label))
         (emit ctx (make-vm-label :name func-label))
         (emit ctx (make-vm-slot-write
                                 :obj-reg obj-reg
                                 :slot-name slot-name
                                 :value-reg val-reg))
         (emit ctx (make-vm-ret :reg val-reg))
         (emit ctx (make-vm-label :name end-label)))))
    closure-reg))

(defmethod compile-ast ((node ast-defgeneric) ctx)
  "Compile a generic function definition.
Creates a dispatch table (hash table) that maps class names to method closures."
  (let* ((name (ast-defgeneric-name node))
         (dst (make-register ctx)))
    ;; A generic function is a hash table with :__name__, :__params__, :__methods__
    (emit ctx (make-vm-class-def
                            :dst dst
                            :class-name name
                            :superclasses nil
                            :slot-names nil
                            :slot-initargs nil))
    ;; Register globally
    (setf (gethash name (ctx-global-generics ctx)) dst)
    (push (cons name dst) (ctx-env ctx))
    dst))

(defmethod compile-ast ((node ast-defmethod) ctx)
  "Compile a method definition.
Compiles the method body as a closure and registers it on the generic function's
dispatch table, keyed by the composite specializer list for multiple dispatch."
  (let* ((name (ast-defmethod-name node))
         (specializers (ast-defmethod-specializers node))
         (params (ast-defmethod-params node))
         (body (ast-defmethod-body node))
         ;; Look up the generic function
         (gf-reg (or (gethash name (ctx-global-generics ctx))
                     (cdr (assoc name (ctx-env ctx)))
                     (error "Generic function ~S not defined" name)))
         ;; Build composite dispatch key from ALL specializers
         ;; Each specializer is (param . class-name) or nil for unspecialized
         (dispatch-key (mapcar (lambda (spec)
                                 (if (and spec (consp spec))
                                     (cdr spec)
                                     (or spec t)))
                               specializers))
         ;; Compile method body as a closure
         (label-suffix (format nil "~{~A~^_~}" dispatch-key))
         (func-label (make-label ctx (format nil "METHOD_~A_~A" name label-suffix)))
         (end-label (make-label ctx (format nil "METHOD_~A_~A_END" name label-suffix)))
         (closure-reg (make-register ctx))
         (param-regs (loop for i from 0 below (length params)
                           collect (make-register ctx))))
    ;; Create closure for method
    (emit ctx (make-vm-closure
                            :dst closure-reg
                            :label func-label
                            :params param-regs
                            :captured nil))
    ;; Register the method on the generic function with composite key
    (emit ctx (make-vm-register-method
                            :gf-reg gf-reg
                            :specializer dispatch-key
                            :method-reg closure-reg))
    ;; Jump over method body
    (emit ctx (make-vm-jump :label end-label))
    ;; Method body
    (emit ctx (make-vm-label :name func-label))
    (let ((old-env (ctx-env ctx)))
      (unwind-protect
           (progn
             (let ((param-bindings nil))
               (loop for param in params
                     for param-reg in param-regs
                     do (push (cons param param-reg) param-bindings))
               (setf (ctx-env ctx) (append (nreverse param-bindings) (ctx-env ctx))))
             (let ((last-reg nil))
               (dolist (form body)
                 (setf last-reg (compile-ast form ctx)))
               (emit ctx (make-vm-ret :reg last-reg))))
        (setf (ctx-env ctx) old-env)))
    (emit ctx (make-vm-label :name end-label))
    closure-reg))

(defmethod compile-ast ((node ast-make-instance) ctx)
  "Compile make-instance.
Looks up the class, compiles initarg values, and creates an instance."
  (let* ((class-ast (ast-make-instance-class node))
         ;; Extract the class name symbol from the AST node
         (class-name (etypecase class-ast
                       (ast-quote (ast-quote-value class-ast))
                       (symbol class-ast)))
         (initargs (ast-make-instance-initargs node))
         (dst (make-register ctx))
         ;; Look up the class register.
         ;; Prefer a compile-time register if available (same compilation).
         ;; Otherwise emit vm-get-global so cross-call REPL works (class was
         ;; stored as a global by vm-set-global during defclass execution).
         (class-reg (or (gethash class-name (ctx-global-classes ctx))
                        (cdr (assoc class-name (ctx-env ctx)))
                        ;; Cross-call REPL fallback: load from persistent global store
                        (let ((r (make-register ctx)))
                          (emit ctx (make-vm-get-global :dst r :name class-name))
                          r)))
         ;; Compile initarg values: initargs is ((keyword . ast-node) ...)
         (initarg-regs (loop for (key . value-ast) in initargs
                             collect (cons key (compile-ast value-ast ctx)))))
    (emit ctx (make-vm-make-obj
                            :dst dst
                            :class-reg class-reg
                            :initarg-regs initarg-regs))
    dst))

(defmethod compile-ast ((node ast-slot-value) ctx)
  "Compile slot-value access."
  (let* ((obj-reg (compile-ast (ast-slot-value-object node) ctx))
         (slot-name (ast-slot-value-slot node))
         (dst (make-register ctx)))
    (emit ctx (make-vm-slot-read
                            :dst dst
                            :obj-reg obj-reg
                            :slot-name slot-name))
    dst))

(defmethod compile-ast ((node ast-set-slot-value) ctx)
  "Compile (setf (slot-value obj 'slot) value)."
  (let* ((obj-reg (compile-ast (ast-set-slot-value-object node) ctx))
         (val-reg (compile-ast (ast-set-slot-value-value node) ctx))
         (slot-name (ast-set-slot-value-slot node)))
    (emit ctx (make-vm-slot-write
                            :obj-reg obj-reg
                            :slot-name slot-name
                            :value-reg val-reg))
    val-reg))

(defmethod compile-ast ((node ast-set-gethash) ctx)
  "Compile (setf (gethash key table) value)."
  (let* ((key-reg (compile-ast (ast-set-gethash-key node) ctx))
         (table-reg (compile-ast (ast-set-gethash-table node) ctx))
         (val-reg (compile-ast (ast-set-gethash-value node) ctx)))
    (emit ctx (make-vm-sethash
                            :key key-reg
                            :table table-reg
                            :value val-reg))
    val-reg))

;;; Top-Level Definitions: defun

(defmethod compile-ast ((node ast-defmacro) ctx)
  "Compile a top-level macro definition.
Registers the macro expander at compile time so subsequent forms can use it.
At runtime, defmacro evaluates to the macro name."
  (let* ((name (ast-defmacro-name node))
         (lambda-list (ast-defmacro-lambda-list node))
         (body (ast-defmacro-body node)))
    ;; Register the macro at compile time via make-macro-expander
    ;; which delegates to either host eval (bootstrap) or our-eval (self-hosting)
    (let ((expander (make-macro-expander lambda-list body)))
      (register-macro name expander))
    ;; At runtime, defmacro just returns the macro name
    (let ((dst (make-register ctx)))
      (emit ctx (make-vm-const :dst dst :value name))
      dst)))

(defun extract-constant-value (ast-node)
  "Extract a constant value from an AST node for use as a default parameter value.
Returns (values value is-constant-p).  IS-CONSTANT-P is true when the value
is a compile-time constant (int, string, quote, t, nil)."
  (typecase ast-node
    (ast-int (values (ast-int-value ast-node) t))
    (ast-quote (values (ast-quote-value ast-node) t))
    (ast-var (let ((name (ast-var-name ast-node)))
               (cond ((eq name 't) (values t t))
                     ((eq name 'nil) (values nil t))
                     (t (values nil nil)))))
    (t (values nil nil))))

(defvar *non-constant-default-sentinel* :__unsupplied__
  "Sentinel value used for non-constant &optional/&key defaults.
When the VM sees this as the default, the actual default is computed inline.")

(defun allocate-defaulting-params (ctx params make-entry)
  "Allocate registers for PARAMS with optional defaults.
MAKE-ENTRY is called as (name reg default-val) and returns the closure-data entry.
Returns (values closure-data bindings non-constant-defaults)."
  (let ((closure-data nil) (bindings nil) (non-constant-defaults nil))
    (dolist (param params)
      (let* ((name (first param))
             (default-ast (second param))
             (reg (make-register ctx)))
        (multiple-value-bind (default-val is-constant)
            (if default-ast (extract-constant-value default-ast) (values nil t))
          (if (and default-ast (not is-constant))
              (progn
                (push (funcall make-entry name reg *non-constant-default-sentinel*) closure-data)
                (push (cons reg default-ast) non-constant-defaults))
              (push (funcall make-entry name reg default-val) closure-data)))
        (push (cons name reg) bindings)))
    (values (nreverse closure-data) (nreverse bindings) (nreverse non-constant-defaults))))

(defun allocate-extended-params (ctx optional-params rest-param key-params)
  "Allocate registers and build metadata for extended lambda list parameters.
OPTIONAL-PARAMS and KEY-PARAMS are lists of (name ast-default) from the AST.
Returns (values opt-closure-data rest-reg key-closure-data opt-bindings rest-binding key-bindings non-constant-defaults).
NON-CONSTANT-DEFAULTS is a list of (register . ast-node) for defaults that need inline compilation."
  (let ((opt-closure-data nil) (opt-bindings nil)
        (rest-reg nil) (rest-binding nil)
        (key-closure-data nil) (key-bindings nil)
        (non-constant-defaults nil))
    (when optional-params
      (multiple-value-bind (cd binds ncds)
          (allocate-defaulting-params ctx optional-params
                                      (lambda (name reg val) (declare (ignore name)) (list reg val)))
        (setf opt-closure-data cd  opt-bindings binds)
        (setf non-constant-defaults (nconc non-constant-defaults ncds))))
    (when rest-param
      (setf rest-reg (make-register ctx))
      (setf rest-binding (cons rest-param rest-reg)))
    (when key-params
      (multiple-value-bind (cd binds ncds)
          (allocate-defaulting-params ctx key-params
                                      (lambda (name reg val)
                                        (list (intern (symbol-name name) "KEYWORD") reg val)))
        (setf key-closure-data cd  key-bindings binds)
        (setf non-constant-defaults (nconc non-constant-defaults ncds))))
    (values opt-closure-data rest-reg key-closure-data
            opt-bindings rest-binding key-bindings non-constant-defaults)))

(defun emit-non-constant-defaults (ctx non-constant-defaults)
  "Emit inline code to compute non-constant default parameter values.
For each (register . ast-node) pair, emits: if register eq sentinel, register = compile(ast)."
  (dolist (entry non-constant-defaults)
    (let* ((param-reg (car entry))
           (default-ast (cdr entry))
           (sentinel-reg (make-register ctx))
           (cmp-reg (make-register ctx))
           (skip-label (make-label ctx "DEFAULT_SKIP")))
      ;; Load sentinel value
      (emit ctx (make-vm-const :dst sentinel-reg :value *non-constant-default-sentinel*))
      ;; Compare: if param-reg != sentinel (eq), skip default computation
      (emit ctx (make-vm-eq :dst cmp-reg :lhs param-reg :rhs sentinel-reg))
      (emit ctx (make-vm-jump-zero :reg cmp-reg :label skip-label))
      ;; Compute the actual default value
      (let ((default-reg (compile-ast default-ast ctx)))
        (emit ctx (make-vm-move :dst param-reg :src default-reg)))
      (emit ctx (make-vm-label :name skip-label)))))

(defun build-all-param-bindings (params param-regs opt-bindings rest-binding key-bindings)
  "Build the combined alist of (name . register) for all lambda list parameters."
  (let ((bindings nil))
    (loop for param in params
          for param-reg in param-regs
          do (push (cons param param-reg) bindings))
    (dolist (b opt-bindings) (push b bindings))
    (when rest-binding (push rest-binding bindings))
    (dolist (b key-bindings) (push b bindings))
    (nreverse bindings)))

(defun compile-function-body (ctx params param-regs opt-bindings rest-binding
                              key-bindings non-constant-defaults body)
  "Bind parameters, emit non-constant defaults, compile BODY, emit vm-ret.
Saves and restores the compiler environment around the body via unwind-protect."
  (let ((old-env (ctx-env ctx)))
    (unwind-protect
         (progn
           (setf (ctx-env ctx)
                 (append (build-all-param-bindings params param-regs
                                                   opt-bindings rest-binding key-bindings)
                         (ctx-env ctx)))
           (when non-constant-defaults
             (emit-non-constant-defaults ctx non-constant-defaults))
           (let ((last-reg nil))
             (dolist (form body)
               (setf last-reg (compile-ast form ctx)))
             (emit ctx (make-vm-ret :reg last-reg))))
      (setf (ctx-env ctx) old-env))))

(defmethod compile-ast ((node ast-defun) ctx)
  "Compile a top-level function definition.
Generates a closure at the function's label and registers it globally."
  (let* ((name (ast-defun-name node))
         (params (ast-defun-params node))
         (body (ast-defun-body node))
         (optional-params (ast-defun-optional-params node))
         (rest-param (ast-defun-rest-param node))
         (key-params (ast-defun-key-params node))
         (func-label (make-label ctx (format nil "DEFUN_~A" name)))
         (end-label (make-label ctx (format nil "DEFUN_~A_END" name)))
         (closure-reg (make-register ctx))
         (free-vars (let ((temp-ast (make-ast-lambda
                                                   :params params
                                                   :body body)))
                      (find-free-variables temp-ast)))
         (captured-vars (mapcar (lambda (v)
                                  (cons v (lookup-var ctx v)))
                                (remove-if-not (lambda (v) (assoc v (ctx-env ctx))) free-vars)))
         (param-regs (loop for i from 0 below (length params)
                           collect (make-register ctx))))
    (multiple-value-bind (opt-closure-data rest-reg key-closure-data
                          opt-bindings rest-binding key-bindings
                          non-constant-defaults)
        (allocate-extended-params ctx optional-params rest-param key-params)
      ;; Register this function globally
      (setf (gethash name (ctx-global-functions ctx)) func-label)
      ;; Emit the closure creation
      (emit ctx (make-vm-closure
                              :dst closure-reg
                              :label func-label
                              :params param-regs
                              :optional-params opt-closure-data
                              :rest-param rest-reg
                              :key-params key-closure-data
                              :captured captured-vars))
      ;; Register in environment so it can be called
      (push (cons name closure-reg) (ctx-env ctx))
      ;; Register in VM function registry for (funcall 'name ...) resolution
      (emit ctx (make-vm-register-function :name name :src closure-reg))
      ;; Jump over the function body
      (emit ctx (make-vm-jump :label end-label))
      (emit ctx (make-vm-label :name func-label))
      (let ((*compiling-typed-fn* (or name t)))
        (compile-function-body ctx params param-regs opt-bindings rest-binding
                               key-bindings non-constant-defaults body))
      (emit ctx (make-vm-label :name end-label))
      closure-reg)))

;;; Top-Level Definitions: defvar/defparameter

(defmethod compile-ast ((node ast-defvar) ctx)
  "Compile a top-level variable definition.
Evaluates the initial value (if any) and registers the variable globally.
Global variables are stored in the VM's global variable store (not registers)
so they persist across function calls."
  (let* ((name (ast-defvar-name node))
         (value-form (ast-defvar-value node))
         (value-reg (if value-form
                        (compile-ast value-form ctx)
                        (let ((nil-reg (make-register ctx)))
                          (emit ctx (make-vm-const :dst nil-reg :value nil))
                          nil-reg))))
    ;; Store in global variable store
    (emit ctx (make-vm-set-global :name name :src value-reg))
    ;; Register as global (so ast-var and ast-setq use global get/set)
    (setf (gethash name (ctx-global-variables ctx)) t)
    value-reg))

;;; Multi-Form Compilation (for compiling entire files)

(defstruct compilation-result
  "Result of compiling expressions or top-level forms."
  (program nil)
  (assembly nil)
  (globals nil)
  (type nil)
  (cps nil))

(defun compile-toplevel-forms (forms &key (target :x86_64))
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
Returns a compilation-result struct with program, assembly, and globals."
  (let* ((ctx (make-instance 'compiler-context))
         (last-reg nil))
    ;; Compile each top-level form (with macro expansion)
    (dolist (form forms)
      (let* ((expanded (if (typep form 'ast-node)
                           form
                           (compiler-macroexpand-all form)))
             (ast (if (typep expanded 'ast-node)
                      expanded
                      (lower-sexp-to-ast expanded))))
        (setf last-reg (compile-ast ast ctx))))
    ;; Finalize with halt on the last result
    (when last-reg
      (emit ctx (make-vm-halt :reg last-reg)))
    ;; Capture label counter for REPL continuity
    (when *repl-capture-label-counter*
      (setf *repl-capture-label-counter* (ctx-next-label ctx)))
    (let* ((instructions (nreverse (ctx-instructions ctx)))
           (optimized (optimize-instructions instructions))
           (program (make-vm-program
                     :instructions optimized
                     :result-register last-reg)))
      (make-compilation-result :program program
                              :assembly (emit-assembly program :target target)
                              :globals (ctx-global-functions ctx)))))

;;; Exception Handling: catch/throw/unwind-protect

(defmethod compile-ast ((node ast-catch) ctx)
  ;; Catch establishes an exception handler for a specific tag.
  ;; The VM will need to support catch/throw semantics.
  ;; For now, we compile it as if it were a progn (no actual exception handling).
  ;; A full implementation would need:
  ;; 1. Evaluate the tag
  ;; 2. Push a catch frame onto a catch stack
  ;; 3. Execute body forms
  ;; 4. Pop the catch frame on normal exit
  ;; 5. On throw, search for matching catch and unwind to it
  (let ((tag-reg (compile-ast (ast-catch-tag node) ctx))
        (result-reg (make-register ctx))
        (end-label (make-label ctx "catch_end")))
    ;; Emit catch setup (placeholder - VM would need catch instruction)
    (emit ctx (make-vm-label :name (make-label ctx "catch_start")))
    ;; Compile body forms
    (let ((body-result (let ((last nil))
                         (dolist (form (ast-catch-body node))
                           (setf last (compile-ast form ctx)))
                         last)))
      (emit ctx (make-vm-move :dst result-reg :src body-result)))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defmethod compile-ast ((node ast-throw) ctx)
  ;; Throw transfers control to a matching catch.
  ;; This would need VM support to search the catch stack and unwind.
  (let ((tag-reg (compile-ast (ast-throw-tag node) ctx))
        (value-reg (compile-ast (ast-throw-value node) ctx)))
    ;; Emit throw (placeholder - VM would need throw instruction)
    ;; For now, we just emit the value computation
    value-reg))

(defmethod compile-ast ((node ast-unwind-protect) ctx)
  ;; Unwind-protect: cleanup forms run on both normal and error exits.
  ;; Strategy: establish catch-all handler, run protected, then cleanup.
  ;; On error: cleanup runs, then re-signal the error.
  (let ((result-reg (make-register ctx))
        (error-reg (make-register ctx))
        (error-flag-reg (make-register ctx))
        (handler-label (make-label ctx "unwind_handler"))
        (cleanup-label (make-label ctx "unwind_cleanup"))
        (end-label (make-label ctx "unwind_end"))
        (resignal-label (make-label ctx "unwind_resignal")))
    ;; Initialize error flag to nil (no error)
    (emit ctx (make-vm-const :dst error-flag-reg :value nil))
    ;; Establish catch-all handler for errors during protected form
    (emit ctx (make-vm-establish-handler
                             :handler-label handler-label
                             :result-reg error-reg
                             :error-type 'error))
    ;; Compile protected form
    (let ((protected-result (compile-ast (ast-unwind-protected node) ctx)))
      (emit ctx (make-vm-move :dst result-reg :src protected-result)))
    ;; Remove handler (normal exit)
    (emit ctx (make-vm-remove-handler))
    ;; Jump to cleanup (normal path)
    (emit ctx (make-vm-jump :label cleanup-label))
    ;; Error handler: set error flag and fall through to cleanup
    (emit ctx (make-vm-label :name handler-label))
    (emit ctx (make-vm-const :dst error-flag-reg :value t))
    ;; Cleanup section (runs on both normal and error paths)
    (emit ctx (make-vm-label :name cleanup-label))
    (dolist (form (ast-unwind-cleanup node))
      (compile-ast form ctx))
    ;; If error occurred, re-signal it
    (emit ctx (make-vm-jump-zero :reg error-flag-reg :label end-label))
    ;; Sync register state to remaining handlers so cleanup side-effects propagate
    (emit ctx (make-vm-sync-handler-regs))
    ;; Re-signal the caught error
    (emit ctx (make-vm-signal-error :error-reg error-reg))
    ;; Normal end
    (emit ctx (make-vm-label :name end-label))
    result-reg))

;;; Handler-Case

(defmethod compile-ast ((node ast-handler-case) ctx)
  "Compile handler-case: establish handlers, run protected form, handle errors."
  (let* ((clauses (ast-handler-case-clauses node))
         (result-reg (make-register ctx))
         (normal-exit-label (make-label ctx "handler_case_exit"))
         ;; Pre-generate handler labels for each clause
         (handler-infos (mapcar (lambda (clause)
                                  (declare (ignore clause))
                                  (let ((handler-label (make-label ctx "handler"))
                                        (error-reg (make-register ctx)))
                                    (list handler-label error-reg)))
                                clauses)))
    ;; 1. Emit vm-establish-handler for each clause (in reverse order so first clause is on top)
    (loop for clause in (reverse clauses)
          for info in (reverse handler-infos)
          do (let ((error-type (first clause))
                   (handler-label (first info))
                   (error-reg (second info)))
               (emit ctx (make-vm-establish-handler
                                       :handler-label handler-label
                                       :result-reg error-reg
                                       :error-type error-type))))
    ;; 2. Compile the protected form
    (let ((form-result (compile-ast (ast-handler-case-form node) ctx)))
      (emit ctx (make-vm-move :dst result-reg :src form-result)))
    ;; 3. Remove handlers (one per clause)
    (dotimes (i (length clauses))
      (emit ctx (make-vm-remove-handler)))
    ;; 4. Jump to normal exit
    (emit ctx (make-vm-jump :label normal-exit-label))
    ;; 5. Emit handler bodies
    (loop for clause in clauses
          for info in handler-infos
          do (let* ((var (second clause))
                    (body (cddr clause))
                    (handler-label (first info))
                    (error-reg (second info))
                    (old-env (ctx-env ctx)))
               ;; Emit handler label
               (emit ctx (make-vm-label :name handler-label))
               ;; Bind error variable if provided
               (unwind-protect
                    (progn
                      (when var
                        (push (cons var error-reg) (ctx-env ctx)))
                      ;; Compile handler body
                      (let ((last-reg nil))
                        (if body
                            (dolist (form body)
                              (setf last-reg (compile-ast form ctx)))
                            (setf last-reg error-reg))
                        (emit ctx (make-vm-move :dst result-reg :src last-reg))))
                 (setf (ctx-env ctx) old-env))
               ;; Jump to normal exit after handler
               (emit ctx (make-vm-jump :label normal-exit-label))))
    ;; 6. Emit normal exit label
    (emit ctx (make-vm-label :name normal-exit-label))
    result-reg))

;;; Multiple Values

(defmethod compile-ast ((node ast-values) ctx)
  (let* ((forms (ast-values-forms node))
         (src-regs (mapcar (lambda (form) (compile-ast form ctx)) forms))
         (dst (make-register ctx)))
    (emit ctx (make-vm-values :dst dst :src-regs src-regs))
    dst))

(defmethod compile-ast ((node ast-multiple-value-bind) ctx)
  (let* ((vars (ast-mvb-vars node))
         (values-form (ast-mvb-values-form node))
         (body (ast-mvb-body node))
         (old-env (ctx-env ctx)))
    ;; Compile the values-producing form
    (compile-ast values-form ctx)
    ;; Create registers for each variable and emit vm-mv-bind
    (let ((var-regs (loop for v in vars collect (make-register ctx))))
      (emit ctx (make-vm-mv-bind :dst-regs var-regs))
      ;; Extend environment with new bindings
      (unwind-protect
           (progn
             (setf (ctx-env ctx)
                   (append (mapcar #'cons vars var-regs) (ctx-env ctx)))
             ;; Compile body
             (let ((last nil))
               (dolist (form body)
                 (setf last (compile-ast form ctx)))
               last))
        (setf (ctx-env ctx) old-env)))))

(defmethod compile-ast ((node ast-apply) ctx)
  (let* ((func-reg (compile-ast (ast-apply-func node) ctx))
         (arg-regs (mapcar (lambda (arg) (compile-ast arg ctx))
                           (ast-apply-args node)))
         (result-reg (make-register ctx)))
    (emit ctx (make-vm-apply :dst result-reg :func func-reg :args arg-regs))
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-call) ctx)
  ;; Evaluate FUNC, then for each ARG form collect ALL its values via
  ;; the clear-values / ensure-values / values-to-list idiom, append the
  ;; per-form lists into one combined list, and apply FUNC to it.
  (let* ((func-reg (compile-ast (ast-mv-call-func node) ctx))
         (args (ast-mv-call-args node))
         (result-reg (make-register ctx)))
    (if (null args)
        ;; No argument forms → call func with zero arguments via empty list.
        (let ((empty-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst empty-reg :value nil))
          (emit ctx (make-vm-apply :dst result-reg :func func-reg :args (list empty-reg))))
        ;; For each arg form: (1) clear values-list, (2) compile form,
        ;; (3) ensure values-list has at least the primary value,
        ;; (4) snapshot into a list register.
        (let ((list-regs
               (mapcar (lambda (arg)
                         (emit ctx (make-vm-clear-values))
                         (let ((primary-reg (compile-ast arg ctx)))
                           (emit ctx (make-vm-ensure-values :src primary-reg))
                           (let ((lr (make-register ctx)))
                             (emit ctx (make-vm-values-to-list :dst lr))
                             lr)))
                       args)))
          ;; Concatenate all per-form value lists with vm-append.
          (let ((combined-reg
                 (reduce (lambda (acc lr)
                           (let ((new-reg (make-register ctx)))
                             (emit ctx (make-vm-append :dst new-reg :src1 acc :src2 lr))
                             new-reg))
                         (cdr list-regs)
                         :initial-value (car list-regs))))
            (emit ctx (make-vm-apply :dst result-reg :func func-reg
                                     :args (list combined-reg))))))
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-prog1) ctx)
  ;; Multiple-value-prog1 evaluates first form, saves all its values,
  ;; evaluates remaining forms, then returns the saved values.
  (let ((first-reg (compile-ast (ast-mv-prog1-first node) ctx))
        (result-reg (make-register ctx)))
    (emit ctx (make-vm-move :dst result-reg :src first-reg))
    (dolist (form (ast-mv-prog1-forms node))
      (compile-ast form ctx))
    result-reg))

;;; Functions and Closures


(defmethod compile-ast ((node ast-lambda) ctx)
  ;; Compile a lambda expression into a closure
  (let* ((params (ast-lambda-params node))
         (body (ast-lambda-body node))
         (optional-params (ast-lambda-optional-params node))
         (rest-param (ast-lambda-rest-param node))
         (key-params (ast-lambda-key-params node))
         (func-label (make-label ctx "lambda"))
         (end-label (make-label ctx "lambda_end"))
         (closure-reg (make-register ctx))
         (free-vars (find-free-variables node))
         (captured-vars (mapcar (lambda (v)
                                  (cons v (lookup-var ctx v)))
                                (remove-if-not (lambda (v) (assoc v (ctx-env ctx))) free-vars)))
         (param-regs (loop for i from 0 below (length params)
                           collect (make-register ctx))))
    (multiple-value-bind (opt-closure-data rest-reg key-closure-data
                          opt-bindings rest-binding key-bindings
                          non-constant-defaults)
        (allocate-extended-params ctx optional-params rest-param key-params)
      ;; Emit the closure creation instruction
      (emit ctx (make-vm-closure
                              :dst closure-reg
                              :label func-label
                              :params param-regs
                              :optional-params opt-closure-data
                              :rest-param rest-reg
                              :key-params key-closure-data
                              :captured captured-vars))
      (emit ctx (make-vm-jump :label end-label))
      (emit ctx (make-vm-label :name func-label))
      (compile-function-body ctx params param-regs opt-bindings rest-binding
                             key-bindings non-constant-defaults body)
      (emit ctx (make-vm-label :name end-label))
      closure-reg)))

(defmethod compile-ast ((node ast-call) ctx)
  ;; Compile a function call
  (let* ((func-expr (ast-call-func node))
         ;; Extract the symbol name for dispatch checking
         (func-sym (cond ((symbolp func-expr) func-expr)
                         ((typep func-expr 'ast-var) (ast-var-name func-expr))
                         (t nil)))
         (args (ast-call-args node))
         (result-reg (make-register ctx)))
    ;; ── Phase 1: Table-driven builtin dispatch ──────────────────────────
    ;; Look up func-sym in the unified builtin registry.  Handles ~160
    ;; standard calling conventions (unary, binary, string-cmp, char-cmp,
    ;; table-query, handle-input, side-effect, void, nullary, string-trim,
    ;; handle-effect) via a single hash-table lookup + generic emitter.
    (when func-sym
      (let ((entry (gethash (symbol-name func-sym) *builtin-registry*)))
        (when entry
          (let ((result (emit-registered-builtin entry args result-reg ctx)))
            (when result
              (return-from compile-ast result))))))
    ;; ── Phase 2: Custom builtins with non-standard argument handling ────
    (macrolet ((builtin-name-p (sym name-str)
                 `(and ,sym (string= (symbol-name ,sym) ,name-str))))
      ;; Nth / Nthcdr / Member (custom slot names: :index/:list, :item/:list)
      (when (builtin-name-p func-sym "NTH")
        (let ((idx-reg (compile-ast (first args) ctx))
              (list-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-nth :dst result-reg :index idx-reg :list list-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "NTHCDR")
        (let ((idx-reg (compile-ast (first args) ctx))
              (list-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-nthcdr :dst result-reg :index idx-reg :list list-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MEMBER")
        (let ((item-reg (compile-ast (first args) ctx))
              (list-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-member :dst result-reg :item item-reg :list list-reg))
          (return-from compile-ast result-reg)))
      ;; Arithmetic predicates (synthesize comparison against zero)
      (when (builtin-name-p func-sym "ZEROP")
        (let ((arg-reg (compile-ast (first args) ctx))
              (zero-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst zero-reg :value 0))
          (emit ctx (make-vm-num-eq :dst result-reg :lhs arg-reg :rhs zero-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "PLUSP")
        (let ((arg-reg (compile-ast (first args) ctx))
              (zero-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst zero-reg :value 0))
          (emit ctx (make-vm-gt :dst result-reg :lhs arg-reg :rhs zero-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MINUSP")
        (let ((arg-reg (compile-ast (first args) ctx))
              (zero-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst zero-reg :value 0))
          (emit ctx (make-vm-lt :dst result-reg :lhs arg-reg :rhs zero-reg))
          (return-from compile-ast result-reg)))
      ;; make-random-state (0 or 1 args)
      (when (builtin-name-p func-sym "MAKE-RANDOM-STATE")
        (let ((arg-reg (if args
                           (compile-ast (first args) ctx)
                           (let ((r (make-register ctx)))
                             (emit ctx (make-vm-const :dst r :value nil)) r))))
          (emit ctx (make-vm-make-random-state :dst result-reg :src arg-reg))
          (return-from compile-ast result-reg)))
      ;; Float rounding (ffloor, fceiling, ftruncate, fround) — 1 or 2 args
      (macrolet ((compile-float-round (name inst-maker)
                   `(when (builtin-name-p func-sym ,(symbol-name name))
                      (let* ((lhs-reg (compile-ast (first args) ctx))
                             (rhs-reg (if (= (length args) 2)
                                          (compile-ast (second args) ctx)
                                          (let ((one-reg (make-register ctx)))
                                            (emit ctx (make-vm-const :dst one-reg :value 1))
                                            one-reg))))
                        (emit ctx (,inst-maker :dst result-reg :lhs lhs-reg :rhs rhs-reg))
                        (return-from compile-ast result-reg)))))
        (compile-float-round ffloor make-vm-ffloor)
        (compile-float-round fceiling make-vm-fceiling)
        (compile-float-round ftruncate make-vm-ftruncate)
        (compile-float-round fround make-vm-fround))
      ;; Cons (custom slot names: :car-src/:cdr-src)
      (when (builtin-name-p func-sym "CONS")
        (let ((car-reg (compile-ast (first args) ctx))
              (cdr-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-cons :dst result-reg :car-src car-reg :cdr-src cdr-reg))
          (return-from compile-ast result-reg)))
      ;; Rplaca/Rplacd (mutating, return the cons)
      (when (builtin-name-p func-sym "RPLACA")
        (let ((cons-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-rplaca :cons cons-reg :val val-reg))
          (emit ctx (make-vm-move :dst result-reg :src cons-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "RPLACD")
        (let ((cons-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-rplacd :cons cons-reg :val val-reg))
          (emit ctx (make-vm-move :dst result-reg :src cons-reg))
          (return-from compile-ast result-reg)))
      ;; EQ and EQL both compile to vm-eq
      (when (or (builtin-name-p func-sym "EQ") (builtin-name-p func-sym "EQL"))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-eq :dst result-reg :lhs lhs-reg :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      ;; Append (custom slot names: :src1/:src2)
      (when (builtin-name-p func-sym "APPEND")
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-append :dst result-reg :src1 lhs-reg :src2 rhs-reg))
          (return-from compile-ast result-reg)))
      ;; %progv-enter / %progv-exit (custom slot names)
      (when (and (builtin-name-p func-sym "%PROGV-ENTER") (= (length args) 2))
        (let ((syms-reg (compile-ast (first args) ctx))
              (vals-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-progv-enter :dst result-reg :syms syms-reg :vals vals-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "%PROGV-EXIT") (= (length args) 1))
        (let ((saved-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-progv-exit :saved saved-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; error / warn (custom slot names: :error-reg, :condition-reg)
      (when (builtin-name-p func-sym "ERROR")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-signal-error :error-reg arg-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "WARN")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-warn :condition-reg arg-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; Hash table builtins (complex keyword arg handling)
      (when (builtin-name-p func-sym "MAKE-HASH-TABLE")
        (let ((test-reg nil))
          (when (>= (length args) 2)
            (let ((kw-arg (first args)))
              (when (and (typep kw-arg 'ast-var)
                         (eq (ast-var-name kw-arg) :test))
                (let ((test-arg (second args)))
                  (let ((test-sym (cond ((and (typep test-arg 'ast-quote)
                                              (symbolp (ast-quote-value test-arg)))
                                         (ast-quote-value test-arg))
                                        ((and (typep test-arg 'ast-var)
                                              (member (ast-var-name test-arg)
                                                      '(eq eql equal equalp)))
                                         (ast-var-name test-arg))
                                        ((and (typep test-arg 'ast-function)
                                              (symbolp (ast-function-name test-arg))
                                              (member (ast-function-name test-arg)
                                                      '(eq eql equal equalp)))
                                         (ast-function-name test-arg))
                                        (t nil))))
                    (when test-sym
                      (setf test-reg (make-register ctx))
                      (emit ctx (make-vm-const :dst test-reg :value test-sym))))))))
          (emit ctx (make-vm-make-hash-table :dst result-reg :test test-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "GETHASH")
        (let ((key-reg (compile-ast (first args) ctx))
              (table-reg (compile-ast (second args) ctx))
              (default-reg (when (third args) (compile-ast (third args) ctx))))
          (emit ctx (make-vm-gethash :dst result-reg :found-dst nil
                                     :key key-reg :table table-reg :default default-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "REMHASH")
        (let ((key-reg (compile-ast (first args) ctx))
              (table-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-remhash :key key-reg :table table-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MAPHASH")
        (let* ((fn-reg (compile-ast (first args) ctx))
               (table-reg (compile-ast (second args) ctx))
               (keys-reg (make-register ctx))
               (loop-start (make-label ctx "MAPHASH_START"))
               (loop-end (make-label ctx "MAPHASH_END"))
               (key-reg (make-register ctx))
               (val-reg (make-register ctx)))
          (emit ctx (make-vm-hash-table-keys :dst keys-reg :table table-reg))
          (emit ctx (make-vm-label :name loop-start))
          (emit ctx (make-vm-jump-zero :reg keys-reg :label loop-end))
          (emit ctx (make-vm-car :dst key-reg :src keys-reg))
          (emit ctx (make-vm-gethash :dst val-reg :key key-reg :table table-reg))
          (let ((call-dst (make-register ctx)))
            (emit ctx (make-vm-call :dst call-dst :func fn-reg :args (list key-reg val-reg))))
          (emit ctx (make-vm-cdr :dst keys-reg :src keys-reg))
          (emit ctx (make-vm-jump :label loop-start))
          (emit ctx (make-vm-label :name loop-end))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; make-list
      (when (builtin-name-p func-sym "MAKE-LIST")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-list :dst result-reg :size size-reg))
          (return-from compile-ast result-reg)))
      ;; Array/vector builtins (custom slot names)
      (when (builtin-name-p func-sym "MAKE-ARRAY")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-array :dst result-reg :size-reg size-reg
                                        :fill-pointer nil :adjustable nil))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MAKE-ADJUSTABLE-VECTOR")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-array :dst result-reg :size-reg size-reg
                                        :fill-pointer t :adjustable t))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "AREF")
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-aref :dst result-reg :array-reg arr-reg :index-reg idx-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "ASET")
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx))
              (val-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-aset :array-reg arr-reg :index-reg idx-reg :val-reg val-reg))
          (emit ctx (make-vm-move :dst result-reg :src val-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "VECTOR-PUSH-EXTEND")
        (let ((val-reg (compile-ast (first args) ctx))
              (arr-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-vector-push-extend :dst result-reg :val-reg val-reg :array-reg arr-reg))
          (return-from compile-ast result-reg)))
      ;; array-row-major-index — variadic
      (when (and (builtin-name-p func-sym "ARRAY-ROW-MAJOR-INDEX") (>= (length args) 1))
        (let ((arr-reg (compile-ast (first args) ctx))
              (subs-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst subs-reg :value nil))
          (dolist (sub (reverse (rest args)))
            (let ((sub-reg (compile-ast sub ctx))
                  (new-reg (make-register ctx)))
              (emit ctx (make-vm-cons :dst new-reg :car-src sub-reg :cdr-src subs-reg))
              (setf subs-reg new-reg)))
          (emit ctx (make-vm-array-row-major-index :dst result-reg :arr arr-reg :subs subs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "VECTOR-PUSH") (= (length args) 2))
        (let ((val-reg (compile-ast (first args) ctx))
              (arr-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-vector-push :dst result-reg :val-reg val-reg :array-reg arr-reg))
          (return-from compile-ast result-reg)))
      ;; Bit array access (custom slot names: :arr/:idx)
      (when (and (builtin-name-p func-sym "BIT") (= (length args) 2))
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-bit-access :dst result-reg :arr arr-reg :idx idx-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "SBIT") (= (length args) 2))
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-sbit :dst result-reg :arr arr-reg :idx idx-reg))
          (return-from compile-ast result-reg)))
      ;; adjust-array
      (when (and (builtin-name-p func-sym "ADJUST-ARRAY") (>= (length args) 2))
        (let ((arr-reg (compile-ast (first args) ctx))
              (dims-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-adjust-array :dst result-reg :arr arr-reg :dims dims-reg))
          (return-from compile-ast result-reg)))
      ;; intern (optional package arg)
      (when (builtin-name-p func-sym "INTERN")
        (let ((str-reg (compile-ast (first args) ctx))
              (pkg-reg (when (second args) (compile-ast (second args) ctx))))
          (emit ctx (make-vm-intern-symbol :dst result-reg :src str-reg :pkg pkg-reg))
          (return-from compile-ast result-reg)))
      ;; encode-universal-time (6-7 args → list)
      (when (and (builtin-name-p func-sym "ENCODE-UNIVERSAL-TIME")
                 (>= (length args) 6) (<= (length args) 7))
        (let ((arg-regs (mapcar (lambda (a) (compile-ast a ctx)) args))
              (list-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst list-reg :value nil))
          (dolist (r (reverse arg-regs))
            (let ((new-reg (make-register ctx)))
              (emit ctx (make-vm-cons :dst new-reg :car-src r :cdr-src list-reg))
              (setf list-reg new-reg)))
          (emit ctx (make-vm-encode-universal-time :dst result-reg :args-reg list-reg))
          (return-from compile-ast result-reg)))
      ;; Symbol property list operations (custom slot names)
      (when (builtin-name-p func-sym "GET")
        (let* ((sym-reg (compile-ast (first args) ctx))
               (ind-reg (compile-ast (second args) ctx))
               (def-reg (if (>= (length args) 3)
                             (compile-ast (third args) ctx)
                             (let ((r (make-register ctx)))
                               (emit ctx (make-vm-const :dst r :value nil)) r))))
          (emit ctx (make-vm-symbol-get :dst result-reg :sym sym-reg
                                        :indicator ind-reg :default def-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "REMPROP") (= (length args) 2))
        (let ((sym-reg (compile-ast (first args) ctx))
              (ind-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-remprop :dst result-reg :sym sym-reg :indicator ind-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "%SET-SYMBOL-PROP") (= (length args) 3))
        (let ((sym-reg (compile-ast (first args) ctx))
              (ind-reg (compile-ast (second args) ctx))
              (val-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-symbol-set :dst result-reg :sym sym-reg
                                        :indicator ind-reg :value val-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "%SET-SYMBOL-PLIST") (= (length args) 2))
        (let ((sym-reg (compile-ast (first args) ctx))
              (plist-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-set-symbol-plist :dst result-reg :sym sym-reg :plist-reg plist-reg))
          (return-from compile-ast result-reg)))
      ;; Setf helpers for svref and fill-pointer
      (when (and (builtin-name-p func-sym "%SVSET") (= (length args) 3))
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx))
              (val-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-svset :dst result-reg :array-reg arr-reg
                                   :index-reg idx-reg :val-reg val-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "%SET-FILL-POINTER") (= (length args) 2))
        (let ((arr-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-set-fill-pointer :dst result-reg :array-reg arr-reg :val-reg val-reg))
          (return-from compile-ast result-reg)))
      ;; Association list builtins (custom slot names)
      (when (and (builtin-name-p func-sym "ASSOC") (= (length args) 2))
        (let ((key-reg (compile-ast (first args) ctx))
              (alist-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-assoc :dst result-reg :key key-reg :alist alist-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "ACONS") (= (length args) 3))
        (let ((key-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx))
              (alist-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-acons :dst result-reg :key key-reg :value val-reg :alist alist-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "SUBST") (= (length args) 3))
        (let ((new-reg (compile-ast (first args) ctx))
              (old-reg (compile-ast (second args) ctx))
              (tree-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-subst :dst result-reg :new-val new-reg :old-val old-reg :tree tree-reg))
          (return-from compile-ast result-reg)))
      ;; char: (char string index) — custom slot names :string/:index
      (when (and (builtin-name-p func-sym "CHAR") (= (length args) 2))
        (let ((str-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-char :dst result-reg :string str-reg :index idx-reg))
          (return-from compile-ast result-reg)))
      ;; make-string — optional :initial-element keyword
      (when (builtin-name-p func-sym "MAKE-STRING")
        (let ((size-reg (compile-ast (first args) ctx)))
          (if (and (= (length args) 3)
                   (typep (second args) 'ast-var)
                   (eq (ast-var-name (second args)) :initial-element))
              (let ((char-reg (compile-ast (third args) ctx)))
                (emit ctx (make-vm-make-string :dst result-reg :src size-reg :char char-reg)))
              (emit ctx (make-vm-make-string :dst result-reg :src size-reg)))
          (return-from compile-ast result-reg)))
      ;; values-list: spread a list as multiple values
      (when (and (builtin-name-p func-sym "VALUES-LIST") (= (length args) 1))
        (let ((lst-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-spread-values :dst result-reg :src lst-reg))
          (return-from compile-ast result-reg)))
      ;; subseq: (subseq string start &optional end)
      (when (and (builtin-name-p func-sym "SUBSEQ") (>= (length args) 2))
        (let ((str-reg (compile-ast (first args) ctx))
              (start-reg (compile-ast (second args) ctx))
              (end-reg (if (third args)
                           (compile-ast (third args) ctx)
                           (let ((nil-reg (make-register ctx)))
                             (emit ctx (make-vm-const :dst nil-reg :value nil))
                             nil-reg))))
          (emit ctx (make-vm-subseq :dst result-reg :string str-reg
                                    :start start-reg :end end-reg))
          (return-from compile-ast result-reg)))
      ;; search: (search pattern string)
      (when (and (builtin-name-p func-sym "SEARCH") (>= (length args) 2))
        (let ((pat-reg (compile-ast (first args) ctx))
              (str-reg (compile-ast (second args) ctx))
              (start-reg (let ((zero-reg (make-register ctx)))
                           (emit ctx (make-vm-const :dst zero-reg :value 0))
                           zero-reg)))
          (emit ctx (make-vm-search-string :dst result-reg :pattern pat-reg
                                           :string str-reg :start start-reg))
          (return-from compile-ast result-reg)))
      ;; typep: (typep value 'type) — requires quoted type
      (when (and (builtin-name-p func-sym "TYPEP")
                 (= (length args) 2)
                 (typep (second args) 'ast-quote))
        (let ((val-reg (compile-ast (first args) ctx))
              (type-sym (ast-quote-value (second args))))
          (emit ctx (make-vm-typep :dst result-reg :src val-reg :type-name type-sym))
          (return-from compile-ast result-reg)))
      ;; CLOS slot predicates (require quoted slot name)
      (macrolet ((compile-slot-pred (name inst-maker)
                   `(when (and (builtin-name-p func-sym ,(symbol-name name))
                               (= (length args) 2)
                               (typep (second args) 'ast-quote))
                      (let ((obj-reg (compile-ast (first args) ctx))
                            (slot-sym (ast-quote-value (second args))))
                        (emit ctx (,inst-maker :dst result-reg :obj-reg obj-reg :slot-name-sym slot-sym))
                        (return-from compile-ast result-reg)))))
        (compile-slot-pred slot-boundp make-vm-slot-boundp)
        (compile-slot-pred slot-exists-p make-vm-slot-exists-p)
        (compile-slot-pred slot-makunbound make-vm-slot-makunbound))
      ;; call-next-method (variadic args → list)
      (when (builtin-name-p func-sym "CALL-NEXT-METHOD")
        (let ((args-reg (if args
                            (let* ((arg-regs (mapcar (lambda (a) (compile-ast a ctx)) args))
                                   (list-reg (make-register ctx)))
                              (emit ctx (make-vm-const :dst list-reg :value nil))
                              (dolist (r (reverse arg-regs))
                                (let ((new-reg (make-register ctx)))
                                  (emit ctx (make-vm-cons :dst new-reg :car-src r :cdr-src list-reg))
                                  (setf list-reg new-reg)))
                              list-reg)
                            nil)))
          (emit ctx (make-vm-call-next-method :dst result-reg :args-reg args-reg))
          (return-from compile-ast result-reg)))
      ;; write-to-string / prin1-to-string / princ-to-string (three names → one instruction)
      (when (or (builtin-name-p func-sym "WRITE-TO-STRING")
                (builtin-name-p func-sym "PRIN1-TO-STRING")
                (builtin-name-p func-sym "PRINC-TO-STRING"))
        (let ((src-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-write-to-string-inst :dst result-reg :src src-reg))
          (return-from compile-ast result-reg)))
      ;; write-string: optional stream argument
      (when (builtin-name-p func-sym "WRITE-STRING")
        (let ((str-reg (compile-ast (first args) ctx)))
          (if (>= (length args) 2)
              (let ((stream-reg (compile-ast (second args) ctx)))
                (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
                (emit ctx (make-vm-move :dst result-reg :src str-reg)))
              (emit ctx (make-vm-princ :src str-reg)))
          (return-from compile-ast result-reg)))
      ;; format: (format nil/t/stream fmt-string args...)
      (when (and (builtin-name-p func-sym "FORMAT") (>= (length args) 2))
        (let* ((dest-arg (first args))
               (fmt-reg (compile-ast (second args) ctx))
               (format-arg-regs (mapcar (lambda (a) (compile-ast a ctx)) (cddr args)))
               (dest-is-nil (or (and (typep dest-arg 'ast-var)
                                     (eq (ast-var-name dest-arg) 'nil))
                                (and (typep dest-arg 'ast-quote)
                                     (null (ast-quote-value dest-arg)))))
               (dest-is-t (or (and (typep dest-arg 'ast-var)
                                   (eq (ast-var-name dest-arg) 't))
                              (and (typep dest-arg 'ast-quote)
                                   (eq (ast-quote-value dest-arg) t)))))
          (cond
            (dest-is-nil
             (emit ctx (make-vm-format-inst :dst result-reg :fmt fmt-reg
                                            :arg-regs format-arg-regs))
             (return-from compile-ast result-reg))
            (dest-is-t
             (let ((str-reg (make-register ctx)))
               (emit ctx (make-vm-format-inst :dst str-reg :fmt fmt-reg
                                              :arg-regs format-arg-regs))
               (emit ctx (make-vm-princ :src str-reg))
               (emit ctx (make-vm-const :dst result-reg :value nil))
               (return-from compile-ast result-reg)))
            (t
             (let ((str-reg (make-register ctx))
                   (stream-reg (compile-ast dest-arg ctx)))
               (emit ctx (make-vm-format-inst :dst str-reg :fmt fmt-reg
                                              :arg-regs format-arg-regs))
               (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
               (emit ctx (make-vm-const :dst result-reg :value nil))
               (return-from compile-ast result-reg))))))
      ;; File I/O: open (keyword arg :direction)
      (when (builtin-name-p func-sym "OPEN")
        (let* ((path-reg (compile-ast (first args) ctx))
               (direction :input))
          (when (>= (length args) 3)
            (let ((kw-arg (second args)))
              (when (and (typep kw-arg 'ast-var)
                         (eq (ast-var-name kw-arg) :direction))
                (let ((dir-arg (third args)))
                  (when (and (typep dir-arg 'ast-var)
                             (keywordp (ast-var-name dir-arg)))
                    (setf direction (ast-var-name dir-arg)))))))
          (emit ctx (make-vm-open-file :dst result-reg :path path-reg :direction direction))
          (return-from compile-ast result-reg)))
      ;; write-char: optional stream argument
      (when (builtin-name-p func-sym "WRITE-CHAR")
        (let ((char-reg (compile-ast (first args) ctx)))
          (if (>= (length args) 2)
              (let ((handle-reg (compile-ast (second args) ctx)))
                (emit ctx (make-vm-write-char :handle handle-reg :char char-reg)))
              (let ((handle-reg (make-register ctx)))
                (emit ctx (make-vm-const :dst handle-reg :value 1))
                (emit ctx (make-vm-write-char :handle handle-reg :char char-reg))))
          (emit ctx (make-vm-move :dst result-reg :src char-reg))
          (return-from compile-ast result-reg)))
      ;; peek-char: (peek-char nil handle) or (peek-char handle)
      (when (builtin-name-p func-sym "PEEK-CHAR")
        (let ((handle-reg (if (>= (length args) 2)
                              (compile-ast (second args) ctx)
                              (compile-ast (first args) ctx))))
          (emit ctx (make-vm-peek-char :dst result-reg :handle handle-reg))
          (return-from compile-ast result-reg)))
      ;; unread-char: (unread-char char handle)
      (when (builtin-name-p func-sym "UNREAD-CHAR")
        (let ((char-reg (compile-ast (first args) ctx))
              (handle-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-unread-char :handle handle-reg :char char-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; make-string-input-stream
      (when (builtin-name-p func-sym "MAKE-STRING-INPUT-STREAM")
        (let ((str-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-string-stream :dst result-reg :direction :input
                                                :initial-string str-reg))
          (return-from compile-ast result-reg)))
      ;; read-char: optional stream argument (ANSI CL: read-char &optional stream)
      (when (builtin-name-p func-sym "READ-CHAR")
        (let ((handle-reg (if (>= (length args) 1)
                              (compile-ast (first args) ctx)
                              (let ((r (make-register ctx)))
                                (emit ctx (make-vm-const :dst r :value 0))
                                r))))
          (emit ctx (make-vm-read-char :dst result-reg :handle handle-reg))
          (return-from compile-ast result-reg)))
      ;; read-line: optional stream argument
      (when (builtin-name-p func-sym "READ-LINE")
        (let ((handle-reg (if (>= (length args) 1)
                              (compile-ast (first args) ctx)
                              (let ((r (make-register ctx)))
                                (emit ctx (make-vm-const :dst r :value 0))
                                r))))
          (emit ctx (make-vm-read-line :dst result-reg :handle handle-reg))
          (return-from compile-ast result-reg)))
      ;; write-byte: (write-byte byte &optional stream)
      (when (builtin-name-p func-sym "WRITE-BYTE")
        (let ((byte-reg (compile-ast (first args) ctx)))
          (if (>= (length args) 2)
              (let ((handle-reg (compile-ast (second args) ctx)))
                (emit ctx (make-vm-write-byte :handle handle-reg :byte-val byte-reg)))
              (let ((handle-reg (make-register ctx)))
                (emit ctx (make-vm-const :dst handle-reg :value 1))
                (emit ctx (make-vm-write-byte :handle handle-reg :byte-val byte-reg))))
          (emit ctx (make-vm-move :dst result-reg :src byte-reg))
          (return-from compile-ast result-reg)))
      ;; write-line: (write-line string &optional stream)
      (when (builtin-name-p func-sym "WRITE-LINE")
        (let ((str-reg (compile-ast (first args) ctx)))
          (if (>= (length args) 2)
              (let ((handle-reg (compile-ast (second args) ctx)))
                (emit ctx (make-vm-write-line :handle handle-reg :str str-reg)))
              (let ((handle-reg (make-register ctx)))
                (emit ctx (make-vm-const :dst handle-reg :value 1))
                (emit ctx (make-vm-write-line :handle handle-reg :str str-reg))))
          (emit ctx (make-vm-move :dst result-reg :src str-reg))
          (return-from compile-ast result-reg)))
      ;; force-output: optional stream (default stdout=1)
      (when (builtin-name-p func-sym "FORCE-OUTPUT")
        (let ((handle-reg (if (>= (length args) 1)
                              (compile-ast (first args) ctx)
                              (let ((r (make-register ctx)))
                                (emit ctx (make-vm-const :dst r :value 1))
                                r))))
          (emit ctx (make-vm-force-output :handle handle-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; finish-output: optional stream (default stdout=1)
      (when (builtin-name-p func-sym "FINISH-OUTPUT")
        (let ((handle-reg (if (>= (length args) 1)
                              (compile-ast (first args) ctx)
                              (let ((r (make-register ctx)))
                                (emit ctx (make-vm-const :dst r :value 1))
                                r))))
          (emit ctx (make-vm-finish-output :handle handle-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; clear-input: optional stream (default stdin=0)
      (when (builtin-name-p func-sym "CLEAR-INPUT")
        (let ((handle-reg (if (>= (length args) 1)
                              (compile-ast (first args) ctx)
                              (let ((r (make-register ctx)))
                                (emit ctx (make-vm-const :dst r :value 0))
                                r))))
          (emit ctx (make-vm-clear-input :handle handle-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; concatenate: only (concatenate 'string a b)
      (when (and (builtin-name-p func-sym "CONCATENATE")
                 (>= (length args) 3)
                 (typep (first args) 'ast-quote)
                 (string= (symbol-name (ast-quote-value (first args))) "STRING"))
        (let ((str1-reg (compile-ast (second args) ctx))
              (str2-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-concatenate :dst result-reg :str1 str1-reg :str2 str2-reg))
          (return-from compile-ast result-reg))))
    ;; Not a builtin — proceed with normal function call dispatch
    ;; If func-expr is a symbol or ast-var, look it up in environment
    ;; Otherwise compile it as an expression
    ;; For function calls, if the name isn't in local scope, emit as symbol
    ;; for runtime resolution via vm-resolve-function (function registry)
    (let* ((raw-func-reg (cond ((symbolp func-expr)
                                (let ((entry (assoc func-expr (ctx-env ctx)))
                                      (is-global (gethash func-expr (ctx-global-functions ctx))))
                                  (if (and entry (not is-global))
                                      ;; Local binding (flet/labels) — use register directly
                                      (cdr entry)
                                      ;; Global or not bound: emit symbol for runtime resolution
                                      ;; This ensures defun functions are resolved via the registry,
                                      ;; which is critical for cross-compilation-unit REPL calls
                                      (let ((sym-reg (make-register ctx)))
                                        (emit ctx (make-vm-const :dst sym-reg :value func-expr))
                                        sym-reg))))
                               ((typep func-expr 'ast-var)
                                (let* ((name (ast-var-name func-expr))
                                       (entry (assoc name (ctx-env ctx)))
                                       (is-global (gethash name (ctx-global-functions ctx))))
                                  (if (and entry (not is-global))
                                      ;; Local binding — use register directly
                                      (cdr entry)
                                      ;; Global or not bound: emit symbol for runtime resolution
                                      (let ((sym-reg (make-register ctx)))
                                        (emit ctx (make-vm-const :dst sym-reg :value name))
                                        sym-reg))))
                               (t (compile-ast func-expr ctx))))
           ;; Unbox labels functions: if this is a boxed labels function,
           ;; emit vm-car to extract the actual closure from the cons cell box
           (func-reg (if (and func-sym (assoc func-sym *labels-boxed-fns*))
                         (let ((unboxed (make-register ctx)))
                           (emit ctx (make-vm-car :dst unboxed :src raw-func-reg))
                           unboxed)
                         raw-func-reg)))
      ;; Compile arguments
      (let ((arg-regs (mapcar (lambda (arg) (compile-ast arg ctx)) args)))
        ;; Check if this is a generic function call
        (if (and func-sym
                 (gethash func-sym (ctx-global-generics ctx)))
            ;; Generic function dispatch
            (emit ctx (make-vm-generic-call
                                    :dst result-reg
                                    :gf-reg func-reg
                                    :args arg-regs))
            ;; Regular function call
            (emit ctx (make-vm-call
                                    :dst result-reg
                                    :func func-reg
                                    :args arg-regs)))
        result-reg))))

(defmethod compile-ast ((node ast-function) ctx)
  ;; Function reference (#'name) - look up function by name
  (let* ((name (ast-function-name node))
         (dst (make-register ctx)))
    (cond
      ;; Simple symbol name
      ((symbolp name)
       (let ((entry (assoc name (ctx-env ctx))))
         (if entry
             ;; It's a local function binding - use the register
             ;; If boxed (labels mutual recursion), unbox with car first
             (if (assoc name *labels-boxed-fns*)
                 (emit ctx (make-vm-car :dst dst :src (cdr entry)))
                 (emit ctx (make-vm-move :dst dst :src (cdr entry))))
             ;; It's a global/top-level function - create function reference
             (emit ctx (make-vm-func-ref
                                     :dst dst
                                     :label (format nil "~A" name))))))
      ;; (setf name) form
      ((and (consp name) (eq (car name) 'setf))
       (emit ctx (make-vm-func-ref
                               :dst dst
                               :label (format nil "SETF_~A" (second name)))))
      (t
       (error "Invalid function name: ~S" name)))
    dst))

(defmethod compile-ast ((node ast-flet) ctx)
  ;; Local non-recursive function bindings
  ;; Each function cannot reference itself or other flet bindings
  (let* ((bindings (ast-flet-bindings node))
         (body (ast-flet-body node))
         (end-label (make-label ctx "flet_end"))
         (old-env (ctx-env ctx)))
    (let ((body-result-reg
            (unwind-protect
                 (progn
                   ;; First, compile each function and create closures
                   (let ((func-bindings nil))
                     (dolist (binding bindings)
                       (let* ((name (first binding))
                              (params (second binding))
                              (body-forms (cddr binding))
                              (func-label (make-label ctx "flet_fn"))
                              (closure-reg (make-register ctx)))
                         ;; Find free variables in function body (excluding params)
                         (let* ((body-ast (make-ast-progn :forms body-forms))
                                (free-vars (find-free-variables body-ast))
                                (captured-vars (remove-if-not
                                                (lambda (v) (assoc v old-env))
                                                (set-difference free-vars params))))
                           ;; Create unique parameter registers
                           (let ((param-regs (loop for i from 0 below (length params)
                                                   collect (make-register ctx)))
                                 (skip-label (make-label ctx "flet_skip")))
                             ;; Create closure
                             (emit ctx (make-vm-closure
                                                     :dst closure-reg
                                                     :label func-label
                                                     :params param-regs
                                                     :captured captured-vars))
                             ;; Remember binding for body compilation
                             (push (cons name closure-reg) func-bindings)
                             ;; Jump over this function's body only
                             (emit ctx (make-vm-jump :label skip-label))
                             ;; Emit function label
                             (emit ctx (make-vm-label :name func-label))
                             ;; Compile function body with parameter bindings
                             (let ((param-bindings nil))
                               (loop for param in params
                                     for param-reg in param-regs
                                     do (push (cons param param-reg) param-bindings))
                               (setf (ctx-env ctx) (append (nreverse param-bindings) old-env))
                               ;; Compile body
                               (let ((last-reg nil))
                                 (dolist (form body-forms)
                                   (setf last-reg (compile-ast form ctx)))
                                 ;; Return
                                 (emit ctx (make-vm-ret :reg last-reg))))
                             ;; Skip label: jump lands here
                             (emit ctx (make-vm-label :name skip-label))))))
                     ;; Set up environment with function bindings for body
                     (setf (ctx-env ctx) (append (nreverse func-bindings) old-env)))
                   ;; Compile body
                   (let ((last-reg nil))
                     (dolist (form body)
                       (setf last-reg (compile-ast form ctx)))
                     last-reg))
              ;; Restore environment
              (setf (ctx-env ctx) old-env))))
      ;; Emit end label
      (emit ctx (make-vm-label :name end-label))
      body-result-reg)))

(defmethod compile-ast ((node ast-labels) ctx)
  ;; Mutually recursive local function bindings using cons-cell boxing
  ;; for mutable indirection — closures capture boxes (cons cells)
  ;; and rplaca fills them after creation, enabling mutual references.
  (let* ((bindings (ast-labels-bindings node))
         (body (ast-labels-body node))
         (end-label (make-label ctx "labels_end"))
         (old-env (ctx-env ctx))
         (old-labels-boxes *labels-boxed-fns*))
    (let ((body-result-reg
            (unwind-protect
                 (progn
                   ;; Phase 1: Create boxes (cons cells) for each function
                   (let ((func-infos nil)
                         (nil-reg (make-register ctx)))
                     (emit ctx (make-vm-const :dst nil-reg :value nil))
                     (dolist (binding bindings)
                       (let* ((name (first binding))
                              (params (second binding))
                              (func-label (make-label ctx "labels_fn"))
                              (closure-reg (make-register ctx))
                              (box-reg (make-register ctx)))
                         ;; Create a mutable box: (cons nil nil)
                         (emit ctx (make-vm-cons :dst box-reg
                                                           :car-src nil-reg :cdr-src nil-reg))
                         (push (list name func-label closure-reg box-reg params) func-infos)))
                     ;; Environment maps function names to BOX registers
                     (let ((forward-env (mapcar (lambda (info)
                                                  (cons (first info) (fourth info)))
                                                func-infos)))
                       ;; Track which names are boxed labels functions
                       (setf *labels-boxed-fns*
                             (append (mapcar (lambda (info)
                                              (cons (first info) (fourth info)))
                                            func-infos)
                                     old-labels-boxes))
                       (setf (ctx-env ctx) (append forward-env old-env))
                       ;; Phase 2: Create closures and fill boxes
                       (dolist (info (nreverse func-infos))
                         (destructuring-bind (name func-label closure-reg box-reg params) info
                           (declare (ignore name))
                           (let* ((binding-data (find (first info) bindings :key #'first))
                                  (body-forms (cddr binding-data))
                                  (body-ast (make-ast-progn :forms body-forms))
                                  (free-vars (set-difference (find-free-variables body-ast) params))
                                  ;; Captured vars: capture BOX registers for siblings
                                  (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                                         (remove-if-not (lambda (v) (assoc v (ctx-env ctx)))
                                                                        free-vars)))
                                  (param-regs (loop for i from 0 below (length params)
                                                    collect (make-register ctx)))
                                  (skip-label (make-label ctx "labels_skip")))
                             ;; Create closure
                             (emit ctx (make-vm-closure
                                                      :dst closure-reg
                                                      :label func-label
                                                      :params param-regs
                                                      :captured captured-vars))
                             ;; Fill box with closure via rplaca
                             (emit ctx (make-vm-rplaca
                                                      :cons box-reg :val closure-reg))
                             ;; Jump over function body
                             (emit ctx (make-vm-jump :label skip-label))
                             ;; Function label
                             (emit ctx (make-vm-label :name func-label))
                             ;; Compile function body
                             (let ((param-bindings nil))
                               (loop for param in params
                                     for param-reg in param-regs
                                     do (push (cons param param-reg) param-bindings))
                               (setf (ctx-env ctx) (append (nreverse param-bindings)
                                                           forward-env old-env))
                               (let ((last-reg nil))
                                 (dolist (form body-forms)
                                   (setf last-reg (compile-ast form ctx)))
                                 (emit ctx (make-vm-ret :reg last-reg))))
                             ;; Skip label
                             (emit ctx (make-vm-label :name skip-label)))))
                       ;; Phase 3: Compile body with box-env
                       (let ((func-env (mapcar (lambda (binding)
                                                 (cons (first binding)
                                                       (lookup-var ctx (first binding))))
                                               bindings)))
                         (setf (ctx-env ctx) (append func-env old-env)))
                       (let ((last-reg nil))
                         (dolist (form body)
                           (setf last-reg (compile-ast form ctx)))
                         last-reg))))
              ;; Restore environment and labels-boxes
              (setf (ctx-env ctx) old-env)
              (setf *labels-boxed-fns* old-labels-boxes))))
      (emit ctx (make-vm-label :name end-label))
      body-result-reg)))

(defun target-instance (target)
  (ecase target
    (:x86_64 (make-instance 'x86-64-target))
    (:aarch64 (make-instance 'aarch64-target))
    (:vm nil)))  ; VM-only target, no assembly needed

(defun emit-assembly (program &key (target :x86_64))
  (when (eq target :vm)
    (return-from emit-assembly ""))
  (when (eq target :wasm)
    (return-from emit-assembly (compile-to-wasm-wat program)))
  (let ((target-object (target-instance target)))
    ;; When targeting x86-64, run register allocation
    (when (and (typep target-object 'x86-64-target)
               (vm-program-instructions program))
      (let* ((instructions (vm-program-instructions program))
             (ra (allocate-registers instructions *x86-64-calling-convention*)))
        (setf (target-regalloc target-object) ra)
        ;; Use the possibly-modified instruction list (with spill code)
        (setf program (make-vm-program
                       :instructions (regalloc-instructions ra)
                       :result-register (vm-program-result-register program)))))
    (with-output-to-string (s)
      (format s "; CL-CC bootstrap assembly (~A)~%" target)
      (format s "clcc_entry:~%")
      (dolist (inst (vm-program-instructions program))
        (emit-instruction target-object inst s)))))

(defun type-check-ast (ast &optional (env (cl-cc/type:type-env-empty)))
  "Run type inference on AST. Returns inferred type or signals type error.
   ENV is an optional type-env for known bindings."
  (cl-cc/type:reset-type-vars!)
  (multiple-value-bind (type subst)
      (cl-cc/type:infer ast env)
    (cl-cc/type:type-substitute type subst)))

(defvar *function-type-registry* (make-hash-table :test #'eq)
  "Maps function names to their declared type signatures.
   Each entry is (param-types . return-type) where param-types is a list of
   type-node objects and return-type is a type-node.")

(defun register-function-type (name param-types return-type)
  "Register a typed function signature for type checking."
  (setf (gethash name *function-type-registry*)
        (cons param-types return-type)))


(defun lambda-list-has-typed-p (params)
  "Return T if required PARAMS contain typed syntax like ((x fixnum) (y string)).
   Only checks params before &optional/&rest/&key — those sections have different syntax.
   Also recognizes registered type aliases."
  (and (listp params)
       (loop for p in params
             ;; Stop checking at lambda list keywords
             when (and (symbolp p) (member p '(&optional &rest &key &body &allow-other-keys)))
               return nil
             thereis (and (consp p)
                          (= (length p) 2)
                          (symbolp (first p))
                          (let ((type-spec (second p)))
                            (or (and (symbolp type-spec)
                                     (or (member type-spec '(fixnum integer int string boolean bool
                                                             symbol cons null t number float
                                                             character list vector array
                                                             hash-table function sequence))
                                         (cl-cc/type:lookup-type-alias type-spec)))
                                (and (consp type-spec)
                                     (member (car type-spec) '(or and function values cons list vector array)))))))))

(defun expand-type-alias (type-spec)
  "Expand type aliases in TYPE-SPEC recursively."
  (if (symbolp type-spec)
      (let ((expanded (cl-cc/type:lookup-type-alias type-spec)))
        (if expanded (expand-type-alias expanded) type-spec))
      type-spec))

(defun strip-typed-params (params)
  "Strip type annotations from typed params, returning (values plain-params param-type-alist).
   ((x fixnum) (y string) z) → (values (x y z) ((x . fixnum) (y . string)))
   Type aliases are expanded for runtime check-type."
  (let ((plain nil)
        (type-alist nil))
    (dolist (p params)
      (cond
        ;; Lambda list keyword — pass through
        ((and (symbolp p) (member p '(&optional &rest &key &body &allow-other-keys)))
         (push p plain))
        ;; Typed param: (name type-spec)
        ((and (consp p) (= (length p) 2) (symbolp (first p)))
         (push (first p) plain)
         (push (cons (first p) (expand-type-alias (second p))) type-alist))
        ;; Untyped param
        ((symbolp p)
         (push p plain))
        ;; Other (e.g., &optional (x default)) — pass through
        (t
         (push p plain))))
    (values (nreverse plain) (nreverse type-alist))))

