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

;;; Accessor-to-slot mapping for setf expansion (populated by defstruct)
(defvar *accessor-slot-map* (make-hash-table :test #'eq)
  "Maps accessor function names to (class-name . slot-name) for setf expansion.")

(defvar *labels-boxed-fns* nil
  "Alist mapping labels function names to their box registers for mutual recursion.")

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

(defgeneric compile-ast (node ctx))

(defmethod compile-ast ((node ast-int) ctx)
  (let ((dst (make-register ctx)))
    (emit ctx (make-vm-const :dst dst :value (ast-int-value node)))
    dst))

(defmethod compile-ast ((node ast-var) ctx)
  (let ((name (ast-var-name node)))
    (cond
      ((eq name t)
       (let ((dst (make-register ctx)))
         (emit ctx (make-vm-const :dst dst :value t))
         dst))
      ((eq name nil)
       (let ((dst (make-register ctx)))
         (emit ctx (make-vm-const :dst dst :value nil))
         dst))
      ((keywordp name)
       (let ((dst (make-register ctx)))
         (emit ctx (make-vm-const :dst dst :value name))
         dst))
      (t
       (let ((local-entry (assoc name (ctx-env ctx))))
         (cond
           ;; Local binding takes priority over global
           ((and local-entry (member name (ctx-boxed-vars ctx)))
            ;; Boxed variable: read via (car box)
            (let ((unboxed (make-register ctx)))
              (emit ctx (make-vm-car :dst unboxed :src (cdr local-entry)))
              unboxed))
           (local-entry
            (cdr local-entry))
           ;; Global variable: load from global store (persists across function calls)
           ((gethash name (ctx-global-variables ctx))
            (let ((dst (make-register ctx)))
              (emit ctx (make-vm-get-global :dst dst :name name))
              dst))
           (t
            (error "Unbound variable: ~S" name))))))))

(defmethod compile-ast ((node ast-binop) ctx)
  (let* ((lhs (compile-ast (ast-binop-lhs node) ctx))
         (rhs (compile-ast (ast-binop-rhs node) ctx))
         (dst (make-register ctx))
         (ctor (ecase (ast-binop-op node)
                 (+ #'make-vm-add)
                 (- #'make-vm-sub)
                 (* #'make-vm-mul)
                 (= #'make-vm-num-eq)
                 (< #'make-vm-lt)
                 (> #'make-vm-gt)
                 (<= #'make-vm-le)
                 (>= #'make-vm-ge))))
    (emit ctx (funcall ctor :dst dst :lhs lhs :rhs rhs))
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

(defmethod compile-ast ((node ast-the) ctx)
  ;; The type declaration is informational - just compile the value
  (compile-ast (ast-the-value node) ctx))

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
    ;; Register globally
    (setf (gethash name (ctx-global-classes ctx)) dst)
    (push (cons name dst) (ctx-env ctx))
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
         ;; Look up the class register
         (class-reg (or (gethash class-name (ctx-global-classes ctx))
                        (cdr (assoc class-name (ctx-env ctx)))
                        (error "Class ~S not defined" class-name)))
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
         (body (ast-defmacro-body node))
         (form-var (gensym "FORM"))
         (env-var (gensym "ENV")))
    ;; Register the macro at compile time using the host CL
    (let ((expander (eval `(lambda (,form-var ,env-var)
                             (declare (ignore ,env-var))
                             (let* ,(generate-lambda-bindings lambda-list form-var)
                               ,@body)))))
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
      (compile-function-body ctx params param-regs opt-bindings rest-binding
                             key-bindings non-constant-defaults body)
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

(defun mutated-vars-of-list (nodes)
  "Union of setq-mutation targets across all AST NODES in a list."
  (reduce #'union (mapcar #'find-mutated-variables nodes) :initial-value nil))

(defun find-mutated-variables (ast)
  "Find all variable names that are targets of SETQ in AST."
  (typecase ast
    (ast-setq  (union (list (ast-setq-var ast))
                      (find-mutated-variables (ast-setq-value ast))))
    (ast-if    (union (find-mutated-variables (ast-if-cond ast))
                      (union (find-mutated-variables (ast-if-then ast))
                             (find-mutated-variables (ast-if-else ast)))))
    (ast-progn (mutated-vars-of-list (ast-progn-forms ast)))
    (ast-let   (mutated-vars-of-list
                (append (mapcar #'cdr (ast-let-bindings ast))
                        (ast-let-body ast))))
    (ast-lambda (mutated-vars-of-list (ast-lambda-body ast)))
    (ast-defun  (mutated-vars-of-list (ast-defun-body ast)))
    (ast-call   (mutated-vars-of-list
                 (cons (if (typep (ast-call-func ast) 'ast-node)
                           (ast-call-func ast)
                           nil)
                       (ast-call-args ast))))
    (ast-print  (find-mutated-variables (ast-print-expr ast)))
    (ast-binop  (union (find-mutated-variables (ast-binop-lhs ast))
                       (find-mutated-variables (ast-binop-rhs ast))))
    (t nil)))

(defun find-captured-in-children (body-forms params)
  "Find variables that inner lambdas/defuns in BODY-FORMS capture as free variables.
PARAMS are the current scope's bound variables — only those are candidates for boxing."
  (let ((captured nil))
    (flet ((add-captured! (forms)
             "Recurse into FORMS and union any newly-captured vars into CAPTURED."
             (setf captured (union captured (find-captured-in-children forms params))))
           (capture-free! (ast-node)
             "Intersect free vars of AST-NODE with PARAMS and add to CAPTURED."
             (setf captured (union captured
                                   (intersection (find-free-variables ast-node) params)))))
      (dolist (form body-forms captured)
        (typecase form
          (ast-lambda (capture-free! form))
          (ast-defun  (capture-free! (make-ast-lambda :params (ast-defun-params form)
                                                      :body   (ast-defun-body form))))
          ;; Recurse into compound forms to find nested lambdas
          (ast-if    (add-captured! (list (ast-if-cond form)
                                         (ast-if-then form)
                                         (ast-if-else form))))
          (ast-progn (add-captured! (ast-progn-forms form)))
          (ast-let   (add-captured! (append (mapcar #'cdr (ast-let-bindings form))
                                            (ast-let-body form))))
          (ast-call  (add-captured! (if (typep (ast-call-func form) 'ast-node)
                                        (cons (ast-call-func form) (ast-call-args form))
                                        (ast-call-args form))))
          (ast-print (add-captured! (list (ast-print-expr form))))
          (ast-binop (add-captured! (list (ast-binop-lhs form) (ast-binop-rhs form))))
          (ast-setq  (add-captured! (list (ast-setq-value form)))))))))

(defun free-vars-of-list (nodes)
  "Union of free variables across all AST NODES in a list."
  (reduce #'union (mapcar #'find-free-variables nodes) :initial-value nil))

(defun free-vars-of-defaults (param-list)
  "Union of free variables in the default expressions of an optional/key PARAM-LIST.
Each entry is (name default-ast); entries with no default contribute nothing."
  (free-vars-of-list (remove nil (mapcar #'second param-list))))

(defun find-free-variables (ast)
  "Find all free variables in AST that would need to be captured in a closure."
  (typecase ast
    (ast-int nil)
    (ast-var (list (ast-var-name ast)))
    (ast-binop (union (find-free-variables (ast-binop-lhs ast))
                      (find-free-variables (ast-binop-rhs ast))))
    (ast-if (union (find-free-variables (ast-if-cond ast))
                   (union (find-free-variables (ast-if-then ast))
                          (find-free-variables (ast-if-else ast)))))
    (ast-progn (free-vars-of-list (ast-progn-forms ast)))
    (ast-print (find-free-variables (ast-print-expr ast)))
    (ast-let
     (let ((binding-vars (mapcar #'car (ast-let-bindings ast)))
           (binding-free  (free-vars-of-list (mapcar #'cdr (ast-let-bindings ast))))
           (body-free     (free-vars-of-list (ast-let-body ast))))
       (set-difference (union binding-free body-free) binding-vars)))
    (ast-lambda
     (let* ((params (ast-lambda-params ast))
            (all-params (append params
                                (mapcar #'first (ast-lambda-optional-params ast))
                                (when (ast-lambda-rest-param ast)
                                  (list (ast-lambda-rest-param ast)))
                                (mapcar #'first (ast-lambda-key-params ast))))
            (body-free    (free-vars-of-list (ast-lambda-body ast)))
            (default-free (union (free-vars-of-defaults (ast-lambda-optional-params ast))
                                 (free-vars-of-defaults (ast-lambda-key-params ast)))))
       (set-difference (union body-free default-free) all-params)))
    (ast-setq (union (list (ast-setq-var ast))
                     (find-free-variables (ast-setq-value ast))))
    (ast-defun
     (set-difference (free-vars-of-list (ast-defun-body ast))
                     (ast-defun-params ast)))
    (ast-call
     (union (if (typep (ast-call-func ast) 'ast-node)
                (find-free-variables (ast-call-func ast))
                nil)
            (free-vars-of-list (ast-call-args ast))))
    (ast-quote nil)
    (t nil)))

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
    ;; Check for builtin function calls first (before looking up func-reg,
    ;; since builtins may not have variable bindings)
    (macrolet ((builtin-name-p (sym name-str)
                 `(and ,sym (string= (symbol-name ,sym) ,name-str)))
               (compile-string-cmp-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (let ((arg1-reg (compile-ast (first args) ctx))
                            (arg2-reg (compile-ast (second args) ctx)))
                        (emit ctx (,ctor :dst result-reg :str1 arg1-reg :str2 arg2-reg))
                        (return-from compile-ast result-reg)))))
               ;; char comparison builtins: (fn char1 char2) -> :dst/:char1/:char2
               (compile-char-cmp-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (let ((c1-reg (compile-ast (first args) ctx))
                            (c2-reg (compile-ast (second args) ctx)))
                        (emit ctx (,ctor :dst result-reg :char1 c1-reg :char2 c2-reg))
                        (return-from compile-ast result-reg)))))
               (compile-unary-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (let ((arg-reg (compile-ast (first args) ctx)))
                        (emit ctx (,ctor :dst result-reg :src arg-reg))
                        (return-from compile-ast result-reg)))))
               ;; 2-arg builtins using :lhs/:rhs/:dst (arithmetic, bitwise, math)
               (compile-binary-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (and (builtin-name-p func-sym ,(symbol-name cl-name))
                               (= (length args) 2))
                      (let ((lhs-reg (compile-ast (first args) ctx))
                            (rhs-reg (compile-ast (second args) ctx)))
                        (emit ctx (,ctor :dst result-reg :lhs lhs-reg :rhs rhs-reg))
                        (return-from compile-ast result-reg)))))
               ;; Print builtins: emit side-effect instruction (:src only), return the arg value
               (compile-side-effect-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (let ((arg-reg (compile-ast (first args) ctx)))
                        (emit ctx (,ctor :src arg-reg))
                        (emit ctx (make-vm-move :dst result-reg :src arg-reg))
                        (return-from compile-ast result-reg)))))
               ;; 0-arg builtins (emit instruction with :dst only, no args)
               (compile-nullary-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (emit ctx (,ctor :dst result-reg))
                      (return-from compile-ast result-reg))))
               ;; hash-table query builtins: (fn table) -> :dst/:table
               (compile-table-query-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (let ((table-reg (compile-ast (first args) ctx)))
                        (emit ctx (,ctor :dst result-reg :table table-reg))
                        (return-from compile-ast result-reg)))))
               ;; stream/file handle builtins: (fn handle) -> :dst/:handle
               (compile-handle-input-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (let ((handle-reg (compile-ast (first args) ctx)))
                        (emit ctx (,ctor :dst result-reg :handle handle-reg))
                        (return-from compile-ast result-reg)))))
               ;; string-trim variants: (fn char-bag string) -> :dst/:char-bag/:string
               (compile-string-trim-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (let ((bag-reg (compile-ast (first args) ctx))
                            (str-reg (compile-ast (second args) ctx)))
                        (emit ctx (,ctor :dst result-reg :char-bag bag-reg :string str-reg))
                        (return-from compile-ast result-reg)))))
               ;; 0-arg side-effect builtins: emit no-slot instruction, return nil
               (compile-void-side-effect-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (emit ctx (,ctor))
                      (emit ctx (make-vm-const :dst result-reg :value nil))
                      (return-from compile-ast result-reg))))
               ;; 1-arg handle side-effect builtins: (fn handle) -> side effect, return nil
               (compile-handle-effect-builtin (cl-name vm-class)
                 (let ((ctor (intern (format nil "MAKE-~A" (symbol-name vm-class)))))
                   `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                      (let ((handle-reg (compile-ast (first args) ctx)))
                        (emit ctx (,ctor :handle handle-reg))
                        (emit ctx (make-vm-const :dst result-reg :value nil))
                        (return-from compile-ast result-reg))))))
      ;; String comparison builtins (2 args, :str1/:str2 -> :dst)
      (compile-string-cmp-builtin string= vm-string=)
      (compile-string-cmp-builtin string< vm-string<)
      (compile-string-cmp-builtin string> vm-string>)
      (compile-string-cmp-builtin string<= vm-string<=)
      (compile-string-cmp-builtin string>= vm-string>=)
      (compile-string-cmp-builtin string-equal vm-string-equal)
      (compile-string-cmp-builtin string-lessp vm-string-lessp)
      (compile-string-cmp-builtin string-greaterp vm-string-greaterp)
      (compile-string-cmp-builtin string/= vm-string-not-equal)
      (compile-string-cmp-builtin string-not-equal vm-string-not-equal)
      (compile-string-cmp-builtin string-not-greaterp vm-string-not-greaterp)
      (compile-string-cmp-builtin string-not-lessp vm-string-not-lessp)
      ;; String concatenation (2 args, :str1/:str2 -> :dst)
      (compile-string-cmp-builtin string-concat vm-concatenate)
      ;; String unary builtins (1 arg, :src -> :dst)
      (compile-unary-builtin string-length vm-string-length)
      (compile-unary-builtin string-upcase vm-string-upcase)
      (compile-unary-builtin string-downcase vm-string-downcase)
      (compile-unary-builtin string-capitalize vm-string-capitalize)
      ;; Symbol/type predicate builtins (1 arg, :src -> :dst)
      (compile-unary-builtin symbolp vm-symbol-p)
      (compile-unary-builtin numberp vm-number-p)
      (compile-unary-builtin integerp vm-integer-p)
      (compile-unary-builtin consp vm-cons-p)
      (compile-unary-builtin null vm-null-p)
      (compile-unary-builtin functionp vm-function-p)
      ;; List operation builtins (1 arg, :src -> :dst)
      (compile-unary-builtin car vm-car)
      (compile-unary-builtin cdr vm-cdr)
      (compile-unary-builtin first vm-first)
      (compile-unary-builtin second vm-second)
      (compile-unary-builtin third vm-third)
      (compile-unary-builtin fourth vm-fourth)
      (compile-unary-builtin fifth vm-fifth)
      (compile-unary-builtin rest vm-rest)
      (compile-unary-builtin last vm-last)
      (compile-unary-builtin length vm-length)
      (compile-unary-builtin reverse vm-reverse)
      (compile-unary-builtin not vm-not)
      (compile-unary-builtin nreverse vm-nreverse)
      (compile-unary-builtin butlast vm-butlast)
      (compile-unary-builtin endp vm-endp)
      ;; Nth / Nthcdr / Member (2 args)
      (when (builtin-name-p func-sym "NTH")
        (let ((idx-reg (compile-ast (first args) ctx))
              (list-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-nth
                                   :dst result-reg
                                   :index idx-reg
                                   :list list-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "NTHCDR")
        (let ((idx-reg (compile-ast (first args) ctx))
              (list-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-nthcdr
                                   :dst result-reg
                                   :index idx-reg
                                   :list list-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MEMBER")
        (let ((item-reg (compile-ast (first args) ctx))
              (list-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-member
                                   :dst result-reg
                                   :item item-reg
                                   :list list-reg))
          (return-from compile-ast result-reg)))
      ;; Arithmetic predicates
      (when (builtin-name-p func-sym "ZEROP")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-num-eq
                                   :dst result-reg
                                   :lhs arg-reg
                                   :rhs (let ((zero-reg (make-register ctx)))
                                          (emit ctx (make-vm-const :dst zero-reg :value 0))
                                          zero-reg)))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "PLUSP")
        (let ((arg-reg (compile-ast (first args) ctx))
              (zero-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst zero-reg :value 0))
          (emit ctx (make-vm-gt
                                   :dst result-reg
                                   :lhs arg-reg
                                   :rhs zero-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MINUSP")
        (let ((arg-reg (compile-ast (first args) ctx))
              (zero-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst zero-reg :value 0))
          (emit ctx (make-vm-lt
                                   :dst result-reg
                                   :lhs arg-reg
                                   :rhs zero-reg))
          (return-from compile-ast result-reg)))
      ;; Extended arithmetic builtins (2-arg: :lhs/:rhs -> :dst)
      (compile-binary-builtin mod vm-mod)
      (compile-binary-builtin rem vm-rem)
      (compile-binary-builtin truncate vm-truncate)
      (compile-binary-builtin floor vm-floor-inst)
      (compile-binary-builtin ceiling vm-ceiling-inst)
      (compile-binary-builtin min vm-min)
      (compile-binary-builtin max vm-max)
      (compile-unary-builtin abs vm-abs)
      (compile-unary-builtin evenp vm-evenp)
      (compile-unary-builtin oddp vm-oddp)
      ;; FR-301: round (2-arg)
      (compile-binary-builtin round vm-round-inst)
      ;; FR-303: Bit operations (2-arg)
      (compile-binary-builtin ash vm-ash)
      (compile-binary-builtin logand vm-logand)
      (compile-binary-builtin logior vm-logior)
      (compile-binary-builtin logxor vm-logxor)
      (compile-binary-builtin logeqv vm-logeqv)
      (compile-binary-builtin logtest vm-logtest)
      (compile-binary-builtin logbitp vm-logbitp)
      (compile-unary-builtin lognot vm-lognot)
      (compile-unary-builtin logcount vm-logcount)
      (compile-unary-builtin integer-length vm-integer-length)
      ;; FR-304: Transcendentals
      (compile-binary-builtin expt vm-expt)
      (compile-unary-builtin sqrt vm-sqrt)
      (compile-unary-builtin exp vm-exp-inst)
      (compile-unary-builtin log vm-log-inst)
      (compile-unary-builtin sin vm-sin-inst)
      (compile-unary-builtin cos vm-cos-inst)
      (compile-unary-builtin tan vm-tan-inst)
      (compile-unary-builtin asin vm-asin-inst)
      (compile-unary-builtin acos vm-acos-inst)
      (compile-unary-builtin atan vm-atan-inst)
      ;; 2-arg atan -> atan2 (checked after 1-arg; only fires when (length args) = 2)
      (compile-binary-builtin atan vm-atan2-inst)
      (compile-unary-builtin sinh vm-sinh-inst)
      (compile-unary-builtin cosh vm-cosh-inst)
      (compile-unary-builtin tanh vm-tanh-inst)
      ;; Phase 2 — FR-305: Float operations
      (compile-unary-builtin float vm-float-inst)
      (compile-unary-builtin float-precision vm-float-precision)
      (compile-unary-builtin float-radix vm-float-radix)
      (compile-unary-builtin float-sign vm-float-sign)
      (compile-unary-builtin float-digits vm-float-digits)
      (compile-unary-builtin decode-float vm-decode-float)
      (compile-unary-builtin integer-decode-float vm-integer-decode-float)
      (compile-unary-builtin boundp vm-boundp)
      (compile-unary-builtin fboundp vm-fboundp)
      (compile-unary-builtin makunbound vm-makunbound)
      (compile-unary-builtin fmakunbound vm-fmakunbound)
      (compile-unary-builtin random vm-random)
      ;; FR-1205: make-random-state (0 or 1 args)
      (when (builtin-name-p func-sym "MAKE-RANDOM-STATE")
        (let ((arg-reg (if args
                           (compile-ast (first args) ctx)
                           (let ((r (make-register ctx)))
                             (emit ctx (make-vm-const :dst r :value nil)) r))))
          (emit ctx (make-vm-make-random-state :dst result-reg :src arg-reg))
          (return-from compile-ast result-reg)))
      (compile-binary-builtin scale-float vm-scale-float)
      ;; FR-301: Float rounding (ffloor, fceiling, ftruncate, fround) — 1 or 2 args
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
      ;; FR-306: Rational number functions
      (compile-unary-builtin rational vm-rational)
      (compile-unary-builtin rationalize vm-rationalize)
      (compile-unary-builtin numerator vm-numerator)
      (compile-unary-builtin denominator vm-denominator)
      ;; FR-306: GCD/LCM (2-arg)
      (compile-binary-builtin gcd vm-gcd)
      (compile-binary-builtin lcm vm-lcm)
      ;; FR-307: Complex number functions
      (compile-unary-builtin realpart vm-realpart)
      (compile-unary-builtin imagpart vm-imagpart)
      (compile-unary-builtin conjugate vm-conjugate)
      (compile-unary-builtin phase vm-phase)
      (compile-binary-builtin complex vm-complex)
      ;; Cons (2 args, :car-src/:cdr-src -> :dst)
      (when (builtin-name-p func-sym "CONS")
        (let ((car-reg (compile-ast (first args) ctx))
              (cdr-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-cons
                                   :dst result-reg
                                   :car-src car-reg
                                   :cdr-src cdr-reg))
          (return-from compile-ast result-reg)))
      ;; Rplaca/Rplacd (2 args, :cons/:val -> modifies cons in place)
      (when (builtin-name-p func-sym "RPLACA")
        (let ((cons-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-rplaca
                                   :cons cons-reg
                                   :val val-reg))
          (emit ctx (make-vm-move :dst result-reg :src cons-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "RPLACD")
        (let ((cons-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-rplacd
                                   :cons cons-reg
                                   :val val-reg))
          (emit ctx (make-vm-move :dst result-reg :src cons-reg))
          (return-from compile-ast result-reg)))
      ;; EQ and EQL both compile to vm-eq (pointer equality)
      (when (or (builtin-name-p func-sym "EQ") (builtin-name-p func-sym "EQL"))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-eq :dst result-reg :lhs lhs-reg :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      ;; Append (2 args, :src1/:src2 -> :dst)
      (when (builtin-name-p func-sym "APPEND")
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-append
                                   :dst result-reg
                                   :src1 lhs-reg
                                   :src2 rhs-reg))
          (return-from compile-ast result-reg)))
      ;; %values-to-list: capture current values-list as a list
      (compile-nullary-builtin %values-to-list vm-values-to-list)
      ;; %progv-enter: save bindings and install new ones (for progv macro)
      (when (and (builtin-name-p func-sym "%PROGV-ENTER") (= (length args) 2))
        (let ((syms-reg (compile-ast (first args) ctx))
              (vals-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-progv-enter :dst result-reg :syms syms-reg :vals vals-reg))
          (return-from compile-ast result-reg)))
      ;; %progv-exit: restore saved bindings (for progv macro cleanup)
      (when (and (builtin-name-p func-sym "%PROGV-EXIT") (= (length args) 1))
        (let ((saved-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-progv-exit :saved saved-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; error: signal an error via vm-signal-error
      (when (builtin-name-p func-sym "ERROR")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-signal-error :error-reg arg-reg))
          ;; Return a dummy register (won't be reached if error is not caught)
          (return-from compile-ast result-reg)))
      ;; warn: print warning via vm-warn
      (when (builtin-name-p func-sym "WARN")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-warn :condition-reg arg-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; Hash table builtins
      (when (builtin-name-p func-sym "MAKE-HASH-TABLE")
        ;; Parse optional :test keyword argument
        (let ((test-reg nil))
          (when (>= (length args) 2)
            (let ((kw-arg (first args)))
              (when (and (typep kw-arg 'ast-var)
                         (eq (ast-var-name kw-arg) :test))
                (let ((test-arg (second args)))
                  ;; Handle (make-hash-table :test 'equal) and (make-hash-table :test #'equal)
                  (let ((test-sym (cond ((and (typep test-arg 'ast-quote)
                                              (symbolp (ast-quote-value test-arg)))
                                         (ast-quote-value test-arg))
                                        ((and (typep test-arg 'ast-var)
                                              (member (ast-var-name test-arg)
                                                      '(eq eql equal equalp)))
                                         (ast-var-name test-arg))
                                        ;; Handle #'equal → ast-function
                                        ((and (typep test-arg 'ast-function)
                                              (symbolp (ast-function-name test-arg))
                                              (member (ast-function-name test-arg)
                                                      '(eq eql equal equalp)))
                                         (ast-function-name test-arg))
                                        (t nil))))
                    (when test-sym
                      (setf test-reg (make-register ctx))
                      (emit ctx (make-vm-const :dst test-reg :value test-sym))))))))
          (emit ctx (make-vm-make-hash-table
                                   :dst result-reg
                                   :test test-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "GETHASH")
        (let ((key-reg (compile-ast (first args) ctx))
              (table-reg (compile-ast (second args) ctx))
              (default-reg (when (third args) (compile-ast (third args) ctx))))
          (emit ctx (make-vm-gethash
                                   :dst result-reg
                                   :found-dst nil
                                   :key key-reg
                                   :table table-reg
                                   :default default-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "REMHASH")
        (let ((key-reg (compile-ast (first args) ctx))
              (table-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-remhash
                                   :key key-reg
                                   :table table-reg))
          (return-from compile-ast result-reg)))
      (compile-table-query-builtin hash-table-count vm-hash-table-count)
      (compile-unary-builtin hash-table-p vm-hash-table-p)
      (compile-table-query-builtin hash-table-keys vm-hash-table-keys)
      (compile-table-query-builtin hash-table-values vm-hash-table-values)
      (compile-table-query-builtin hash-table-test vm-hash-table-test)
      (when (builtin-name-p func-sym "MAPHASH")
        ;; Compile maphash by iterating over keys and calling fn with key,value
        (let* ((fn-reg (compile-ast (first args) ctx))
               (table-reg (compile-ast (second args) ctx))
               (keys-reg (make-register ctx))
               (loop-start (make-label ctx "MAPHASH_START"))
               (loop-end (make-label ctx "MAPHASH_END"))
               (key-reg (make-register ctx))
               (val-reg (make-register ctx)))
          ;; Get all keys
          (emit ctx (make-vm-hash-table-keys :dst keys-reg :table table-reg))
          ;; Loop over keys
          (emit ctx (make-vm-label :name loop-start))
          ;; End test: jump to end when keys-reg is nil (falsy)
          (emit ctx (make-vm-jump-zero :reg keys-reg :label loop-end))
          ;; key = (car keys)
          (emit ctx (make-vm-car :dst key-reg :src keys-reg))
          ;; val = (gethash key table)
          (emit ctx (make-vm-gethash :dst val-reg :key key-reg :table table-reg))
          ;; Call fn(key, val)
          (let ((call-dst (make-register ctx)))
            (emit ctx (make-vm-call :dst call-dst :func fn-reg :args (list key-reg val-reg))))
          ;; keys = (cdr keys)
          (emit ctx (make-vm-cdr :dst keys-reg :src keys-reg))
          (emit ctx (make-vm-jump :label loop-start))
          (emit ctx (make-vm-label :name loop-end))
          ;; maphash returns nil
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; make-list
      (when (builtin-name-p func-sym "MAKE-LIST")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-list :dst result-reg :size size-reg))
          (return-from compile-ast result-reg)))
      ;; Array/vector builtins
      (when (builtin-name-p func-sym "MAKE-ARRAY")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-array
                                   :dst result-reg
                                   :size-reg size-reg
                                   :fill-pointer nil
                                   :adjustable nil))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MAKE-ADJUSTABLE-VECTOR")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-array
                                   :dst result-reg
                                   :size-reg size-reg
                                   :fill-pointer t
                                   :adjustable t))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "AREF")
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-aref
                                   :dst result-reg
                                   :array-reg arr-reg
                                   :index-reg idx-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "ASET")
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx))
              (val-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-aset
                                   :array-reg arr-reg
                                   :index-reg idx-reg
                                   :val-reg val-reg))
          (emit ctx (make-vm-move :dst result-reg :src val-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "VECTOR-PUSH-EXTEND")
        (let ((val-reg (compile-ast (first args) ctx))
              (arr-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-vector-push-extend
                                   :dst result-reg
                                   :val-reg val-reg
                                   :array-reg arr-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin vectorp vm-vectorp)
      (compile-unary-builtin array-length vm-array-length)
      ;; FR-601: Array dimension queries
      (compile-unary-builtin array-rank vm-array-rank)
      (compile-unary-builtin array-total-size vm-array-total-size)
      (compile-unary-builtin array-dimensions vm-array-dimensions)
      (compile-binary-builtin array-dimension vm-array-dimension)
      (compile-binary-builtin row-major-aref vm-row-major-aref)
      ;; FR-602: array-row-major-index — variadic (array &rest subscripts)
      (when (and (builtin-name-p func-sym "ARRAY-ROW-MAJOR-INDEX") (>= (length args) 1))
        (let ((arr-reg (compile-ast (first args) ctx))
              (subs-reg (make-register ctx)))
          ;; Build list of subscripts
          (emit ctx (make-vm-const :dst subs-reg :value nil))
          (dolist (sub (reverse (rest args)))
            (let ((sub-reg (compile-ast sub ctx))
                  (new-reg (make-register ctx)))
              (emit ctx (make-vm-cons :dst new-reg :car-src sub-reg :cdr-src subs-reg))
              (setf subs-reg new-reg)))
          (emit ctx (make-vm-array-row-major-index :dst result-reg :arr arr-reg :subs subs-reg))
          (return-from compile-ast result-reg)))
      ;; FR-603: svref
      (compile-binary-builtin svref vm-svref)
      ;; FR-604: fill-pointer, vector-push, vector-pop
      (compile-unary-builtin fill-pointer vm-fill-pointer-inst)
      (compile-unary-builtin array-has-fill-pointer-p vm-array-has-fill-pointer-p)
      (compile-unary-builtin array-adjustable-p vm-array-adjustable-p)
      (compile-unary-builtin vector-pop vm-vector-pop)
      (when (and (builtin-name-p func-sym "VECTOR-PUSH") (= (length args) 2))
        (let ((val-reg (compile-ast (first args) ctx))
              (arr-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-vector-push :dst result-reg :val-reg val-reg :array-reg arr-reg))
          (return-from compile-ast result-reg)))
      ;; FR-606: bit array operations
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
      (compile-binary-builtin bit-and vm-bit-and)
      (compile-binary-builtin bit-or  vm-bit-or)
      (compile-binary-builtin bit-xor vm-bit-xor)
      (compile-unary-builtin bit-not vm-bit-not)
      ;; FR-605: adjust-array / array-displacement
      (when (and (builtin-name-p func-sym "ADJUST-ARRAY") (>= (length args) 2))
        (let ((arr-reg (compile-ast (first args) ctx))
              (dims-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-adjust-array :dst result-reg :arr arr-reg :dims dims-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin array-displacement vm-array-displacement)
      ;; Symbol manipulation builtins
      (compile-unary-builtin symbol-name vm-symbol-name)
      (compile-unary-builtin make-symbol vm-make-symbol)
      (when (builtin-name-p func-sym "INTERN")
        (let ((str-reg (compile-ast (first args) ctx))
              (pkg-reg (when (second args) (compile-ast (second args) ctx))))
          (emit ctx (make-vm-intern-symbol :dst result-reg
                                                     :src str-reg
                                                     :pkg pkg-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin keywordp vm-keywordp)
      (compile-nullary-builtin get-universal-time vm-get-universal-time)
      (compile-nullary-builtin get-internal-real-time vm-get-internal-real-time)
      (compile-nullary-builtin get-internal-run-time vm-get-internal-run-time)
      (compile-unary-builtin decode-universal-time vm-decode-universal-time)
      ;; FR-1204: encode-universal-time (2-7 args: sec min hour date month year [zone])
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
      (compile-nullary-builtin gensym vm-gensym-inst)
      ;; FR-1201: Symbol property list operations
      ;; (get sym indicator &optional default)
      (when (builtin-name-p func-sym "GET")
        (let* ((sym-reg (compile-ast (first args) ctx))
               (ind-reg (compile-ast (second args) ctx))
               (def-reg (if (>= (length args) 3)
                             (compile-ast (third args) ctx)
                             (let ((r (make-register ctx)))
                               (emit ctx (make-vm-const :dst r :value nil))
                               r))))
          (emit ctx (make-vm-symbol-get :dst result-reg :sym sym-reg
                                        :indicator ind-reg :default def-reg))
          (return-from compile-ast result-reg)))
      ;; (remprop sym indicator)
      (when (and (builtin-name-p func-sym "REMPROP") (= (length args) 2))
        (let ((sym-reg (compile-ast (first args) ctx))
              (ind-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-remprop :dst result-reg :sym sym-reg :indicator ind-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin symbol-plist vm-symbol-plist)
      ;; (setf (get sym ind) val) internal helper — generated by frontend setf expander
      (when (and (builtin-name-p func-sym "%SET-SYMBOL-PROP") (= (length args) 3))
        (let ((sym-reg (compile-ast (first args) ctx))
              (ind-reg (compile-ast (second args) ctx))
              (val-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-symbol-set :dst result-reg :sym sym-reg
                                        :indicator ind-reg :value val-reg))
          (return-from compile-ast result-reg)))
      ;; (setf (symbol-plist sym) plist) internal helper — generated by frontend setf expander
      (when (and (builtin-name-p func-sym "%SET-SYMBOL-PLIST") (= (length args) 2))
        (let ((sym-reg (compile-ast (first args) ctx))
              (plist-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-set-symbol-plist :dst result-reg :sym sym-reg :plist-reg plist-reg))
          (return-from compile-ast result-reg)))
      ;; (setf (svref arr idx) val) internal helper
      (when (and (builtin-name-p func-sym "%SVSET") (= (length args) 3))
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx))
              (val-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-svset :dst result-reg :array-reg arr-reg
                                   :index-reg idx-reg :val-reg val-reg))
          (return-from compile-ast result-reg)))
      ;; (setf (fill-pointer vec) n) internal helper
      (when (and (builtin-name-p func-sym "%SET-FILL-POINTER") (= (length args) 2))
        (let ((arr-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-set-fill-pointer :dst result-reg :array-reg arr-reg :val-reg val-reg))
          (return-from compile-ast result-reg)))
      ;; Association list and utility builtins
      (when (and (builtin-name-p func-sym "ASSOC")
                 (= (length args) 2))
        (let ((key-reg (compile-ast (first args) ctx))
              (alist-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-assoc
                                   :dst result-reg
                                   :key key-reg
                                   :alist alist-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "ACONS")
                 (= (length args) 3))
        (let ((key-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx))
              (alist-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-acons
                                   :dst result-reg
                                   :key key-reg
                                   :value val-reg
                                   :alist alist-reg))
          (return-from compile-ast result-reg)))
      (compile-binary-builtin equal vm-equal)
      (compile-binary-builtin nconc vm-nconc)
      (compile-unary-builtin copy-list vm-copy-list)
      (compile-unary-builtin copy-tree vm-copy-tree)
      (when (and (builtin-name-p func-sym "SUBST")
                 (= (length args) 3))
        (let ((new-reg (compile-ast (first args) ctx))
              (old-reg (compile-ast (second args) ctx))
              (tree-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-subst
                                   :dst result-reg
                                   :new-val new-reg
                                   :old-val old-reg
                                   :tree tree-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin listp vm-listp)
      (compile-unary-builtin atom vm-atom)
      (compile-unary-builtin coerce-to-string vm-coerce-to-string)
      (compile-unary-builtin coerce-to-list vm-coerce-to-list)
      (compile-unary-builtin coerce-to-vector vm-coerce-to-vector)
      (compile-unary-builtin string vm-string-coerce)
      ;; Character/string builtins
      (when (and (builtin-name-p func-sym "CHAR")
                 (= (length args) 2))
        (let ((str-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-char
                                   :dst result-reg
                                   :string str-reg
                                   :index idx-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin char-code vm-char-code)
      (compile-unary-builtin code-char vm-code-char)
      ;; Character comparison builtins (case-sensitive and case-insensitive)
      (compile-char-cmp-builtin char= vm-char=)
      (compile-char-cmp-builtin char< vm-char<)
      (compile-char-cmp-builtin char> vm-char>)
      (compile-char-cmp-builtin char<= vm-char<=)
      (compile-char-cmp-builtin char>= vm-char>=)
      (compile-char-cmp-builtin char/= vm-char/=)
      (compile-char-cmp-builtin char-equal vm-char-equal)
      (compile-char-cmp-builtin char-not-equal vm-char-not-equal)
      (compile-char-cmp-builtin char-lessp vm-char-lessp)
      (compile-char-cmp-builtin char-greaterp vm-char-greaterp)
      (compile-char-cmp-builtin char-not-greaterp vm-char-not-greaterp)
      (compile-char-cmp-builtin char-not-lessp vm-char-not-lessp)
      ;; FR-409/410: char predicates and ops
      (compile-unary-builtin both-case-p vm-both-case-p)
      (compile-unary-builtin graphic-char-p vm-graphic-char-p)
      (compile-unary-builtin standard-char-p vm-standard-char-p)
      (compile-unary-builtin digit-char vm-digit-char)
      (compile-unary-builtin char-name vm-char-name)
      (compile-unary-builtin name-char vm-name-char)
      ;; FR-410: char-int is identical to char-code per ANSI CL spec
      (compile-unary-builtin char-int vm-char-code)
      ;; FR-405: make-string — 1-arg or (make-string size :initial-element char)
      (when (builtin-name-p func-sym "MAKE-STRING")
        (let ((size-reg (compile-ast (first args) ctx)))
          (if (and (= (length args) 3)
                   (typep (second args) 'ast-var)
                   (eq (ast-var-name (second args)) :initial-element))
              (let ((char-reg (compile-ast (third args) ctx)))
                (emit ctx (make-vm-make-string :dst result-reg :src size-reg :char char-reg)))
              (emit ctx (make-vm-make-string :dst result-reg :src size-reg)))
          (return-from compile-ast result-reg)))
      ;; Existing char predicates
      (compile-unary-builtin digit-char-p vm-digit-char-p)
      (compile-unary-builtin alpha-char-p vm-alpha-char-p)
      (compile-unary-builtin alphanumericp vm-alphanumericp)
      (compile-unary-builtin upper-case-p vm-upper-case-p)
      (compile-unary-builtin lower-case-p vm-lower-case-p)
      (compile-unary-builtin char-upcase vm-char-upcase)
      (compile-unary-builtin char-downcase vm-char-downcase)
      (compile-unary-builtin stringp vm-stringp)
      (compile-unary-builtin characterp vm-characterp)
      (compile-unary-builtin parse-integer vm-parse-integer)
      ;; values-list: spread a list as multiple values
      (when (and (builtin-name-p func-sym "VALUES-LIST")
                 (= (length args) 1))
        (let ((lst-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-spread-values :dst result-reg :src lst-reg))
          (return-from compile-ast result-reg)))
      ;; subseq: (subseq string start &optional end)
      (when (and (builtin-name-p func-sym "SUBSEQ")
                 (>= (length args) 2))
        (let ((str-reg (compile-ast (first args) ctx))
              (start-reg (compile-ast (second args) ctx))
              (end-reg (if (third args)
                           (compile-ast (third args) ctx)
                           (let ((nil-reg (make-register ctx)))
                             (emit ctx (make-vm-const :dst nil-reg :value nil))
                             nil-reg))))
          (emit ctx (make-vm-subseq
                                   :dst result-reg
                                   :string str-reg
                                   :start start-reg
                                   :end end-reg))
          (return-from compile-ast result-reg)))
      (compile-string-trim-builtin string-trim vm-string-trim)
      (compile-string-trim-builtin string-left-trim vm-string-left-trim)
      (compile-string-trim-builtin string-right-trim vm-string-right-trim)
      ;; search: (search pattern string)
      (when (and (builtin-name-p func-sym "SEARCH")
                 (>= (length args) 2))
        (let ((pat-reg (compile-ast (first args) ctx))
              (str-reg (compile-ast (second args) ctx))
              (start-reg (let ((zero-reg (make-register ctx)))
                           (emit ctx (make-vm-const :dst zero-reg :value 0))
                           zero-reg)))
          (emit ctx (make-vm-search-string
                                   :dst result-reg
                                   :pattern pat-reg
                                   :string str-reg
                                   :start start-reg))
          (return-from compile-ast result-reg)))
      ;; typep: (typep value 'type)
      (when (and (builtin-name-p func-sym "TYPEP")
                 (= (length args) 2)
                 (typep (second args) 'ast-quote))
        (let ((val-reg (compile-ast (first args) ctx))
              (type-sym (ast-quote-value (second args))))
          (emit ctx (make-vm-typep
                                   :dst result-reg
                                   :src val-reg
                                   :type-name type-sym))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin type-of vm-type-of)
      ;; FR-1002: CLOS slot predicates
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
      ;; FR-1001: call-next-method — invoke next applicable method
      (when (builtin-name-p func-sym "CALL-NEXT-METHOD")
        (let ((args-reg (if args
                            ;; explicit args supplied: build a list from them
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
      ;; FR-1001: next-method-p — check if a next method exists
      (compile-nullary-builtin next-method-p vm-next-method-p)
      ;; eval — meta-circular runtime evaluation
      (compile-unary-builtin eval vm-eval)
      ;; I/O builtins (simple print/format, work with any vm-state)
      (compile-side-effect-builtin princ vm-princ)
      (compile-side-effect-builtin prin1 vm-prin1)
      (compile-side-effect-builtin print vm-print-inst)
      (compile-void-side-effect-builtin terpri     vm-terpri-inst)
      (compile-void-side-effect-builtin fresh-line vm-fresh-line-inst)
      ;; All three stringify functions use the same vm-write-to-string-inst
      (when (or (builtin-name-p func-sym "WRITE-TO-STRING")
                (builtin-name-p func-sym "PRIN1-TO-STRING")
                (builtin-name-p func-sym "PRINC-TO-STRING"))
        (let ((src-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-write-to-string-inst :dst result-reg :src src-reg))
          (return-from compile-ast result-reg)))
      ;; String output stream builtins
      (compile-nullary-builtin make-string-output-stream vm-make-string-output-stream-inst)
      (compile-unary-builtin get-output-stream-string vm-get-output-stream-string-inst)
      (when (builtin-name-p func-sym "WRITE-STRING")
        (let ((str-reg (compile-ast (first args) ctx)))
          (if (and (>= (length args) 2))
              ;; (write-string str stream)
              (let ((stream-reg (compile-ast (second args) ctx)))
                (emit ctx (make-vm-stream-write-string-inst
                                         :stream-reg stream-reg
                                         :src str-reg))
                (emit ctx (make-vm-move :dst result-reg :src str-reg)))
              ;; (write-string str) — to stdout
              (emit ctx (make-vm-princ :src str-reg)))
          (return-from compile-ast result-reg)))
      ;; format: (format nil/t/stream fmt-string args...)
      (when (and (builtin-name-p func-sym "FORMAT")
                 (>= (length args) 2))
        (let* ((dest-arg (first args))
               (fmt-reg (compile-ast (second args) ctx))
               (format-arg-regs (mapcar (lambda (a) (compile-ast a ctx)) (cddr args)))
               (dest-is-nil (and (typep dest-arg 'ast-var)
                                 (eq (ast-var-name dest-arg) 'nil)))
               (dest-is-t (and (typep dest-arg 'ast-var)
                               (eq (ast-var-name dest-arg) 't))))
          (cond
            (dest-is-nil
             (emit ctx (make-vm-format-inst
                                      :dst result-reg
                                      :fmt fmt-reg
                                      :arg-regs format-arg-regs))
             (return-from compile-ast result-reg))
            (dest-is-t
             (let ((str-reg (make-register ctx)))
               (emit ctx (make-vm-format-inst
                                        :dst str-reg
                                        :fmt fmt-reg
                                        :arg-regs format-arg-regs))
               (emit ctx (make-vm-princ :src str-reg))
               (emit ctx (make-vm-const :dst result-reg :value nil))
               (return-from compile-ast result-reg)))
            (t
             ;; Stream destination: format to string, write to stream
             (let ((str-reg (make-register ctx))
                   (stream-reg (compile-ast dest-arg ctx)))
               (emit ctx (make-vm-format-inst
                                        :dst str-reg
                                        :fmt fmt-reg
                                        :arg-regs format-arg-regs))
               (emit ctx (make-vm-stream-write-string-inst
                                        :stream-reg stream-reg
                                        :src str-reg))
               (emit ctx (make-vm-const :dst result-reg :value nil))
               (return-from compile-ast result-reg))))))
      ;; File I/O builtins
      ;; open: (open path :direction dir)
      (when (builtin-name-p func-sym "OPEN")
        (let* ((path-reg (compile-ast (first args) ctx))
               (direction :input))
          ;; Check for :direction keyword arg (keywords are ast-var with keywordp name)
          (when (>= (length args) 3)
            (let ((kw-arg (second args)))
              (when (and (typep kw-arg 'ast-var)
                         (eq (ast-var-name kw-arg) :direction))
                (let ((dir-arg (third args)))
                  (when (and (typep dir-arg 'ast-var)
                             (keywordp (ast-var-name dir-arg)))
                    (setf direction (ast-var-name dir-arg)))))))
          (emit ctx (make-vm-open-file
                                   :dst result-reg
                                   :path path-reg
                                   :direction direction))
          (return-from compile-ast result-reg)))
      ;; close: (close handle) -> side effect, return nil
      (compile-handle-effect-builtin close vm-close-file)
      (compile-handle-input-builtin read-char vm-read-char)
      (compile-handle-input-builtin read-line vm-read-line)
      ;; write-char: (write-char char &optional stream)
      (when (builtin-name-p func-sym "WRITE-CHAR")
        (let ((char-reg (compile-ast (first args) ctx)))
          (if (>= (length args) 2)
              ;; (write-char char stream)
              (let ((handle-reg (compile-ast (second args) ctx)))
                (emit ctx (make-vm-write-char
                                         :handle handle-reg
                                         :char char-reg)))
              ;; (write-char char) — to stdout (handle 1)
              (let ((handle-reg (make-register ctx)))
                (emit ctx (make-vm-const :dst handle-reg :value 1))
                (emit ctx (make-vm-write-char
                                         :handle handle-reg
                                         :char char-reg))))
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
          (emit ctx (make-vm-unread-char
                                   :handle handle-reg
                                   :char char-reg))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      (compile-handle-input-builtin file-position vm-file-position)
      (compile-handle-input-builtin file-length vm-file-length)
      ;; make-string-input-stream: (make-string-input-stream string)
      (when (builtin-name-p func-sym "MAKE-STRING-INPUT-STREAM")
        (let ((str-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-string-stream
                                   :dst result-reg
                                   :direction :input
                                   :initial-string str-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin read-from-string vm-read-from-string-inst)
      (compile-unary-builtin read vm-read-sexp-inst)
      ;; concatenate: only handle (concatenate 'string a b)
      (when (and (builtin-name-p func-sym "CONCATENATE")
                 (>= (length args) 3)
                 (typep (first args) 'ast-quote)
                 (string= (symbol-name (ast-quote-value (first args))) "STRING"))
        (let ((str1-reg (compile-ast (second args) ctx))
              (str2-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-concatenate
                                   :dst result-reg
                                   :str1 str1-reg
                                   :str2 str2-reg))
          (return-from compile-ast result-reg))))
    ;; Not a builtin — proceed with normal function call dispatch
    ;; If func-expr is a symbol or ast-var, look it up in environment
    ;; Otherwise compile it as an expression
    ;; For function calls, if the name isn't in local scope, emit as symbol
    ;; for runtime resolution via vm-resolve-function (function registry)
    (let* ((raw-func-reg (cond ((symbolp func-expr)
                                (let ((entry (assoc func-expr (ctx-env ctx))))
                                  (if entry
                                      (cdr entry)
                                      ;; Not locally bound: emit symbol for runtime resolution
                                      (let ((sym-reg (make-register ctx)))
                                        (emit ctx (make-vm-const :dst sym-reg :value func-expr))
                                        sym-reg))))
                               ((typep func-expr 'ast-var)
                                (let* ((name (ast-var-name func-expr))
                                       (entry (assoc name (ctx-env ctx))))
                                  (if entry
                                      (cdr entry)
                                      ;; Not locally bound: emit symbol for runtime resolution
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

(defun optimize-instructions (instructions)
  (if *enable-prolog-peephole*
      (let* ((sexps (mapcar #'instruction->sexp instructions))
             (optimized (apply-prolog-peephole sexps)))
        (mapcar #'sexp->instruction optimized))
      instructions))

(defun target-instance (target)
  (ecase target
    (:x86_64 (make-instance 'x86-64-target))
    (:aarch64 (make-instance 'aarch64-target))
    (:vm nil)))  ; VM-only target, no assembly needed

(defun emit-assembly (program &key (target :x86_64))
  (when (eq target :vm)
    (return-from emit-assembly ""))
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

(defun lookup-function-type (name)
  "Look up a registered function type. Returns (values param-types return-type) or nil."
  (let ((entry (gethash name *function-type-registry*)))
    (when entry
      (values (car entry) (cdr entry)))))

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

(defparameter *compiler-special-forms*
  '(if progn lambda quote setq setf
    defun defvar defparameter defmacro defclass defgeneric defmethod
    make-instance slot-value
    block return-from tagbody go
    flet labels function funcall
    the print
    catch throw unwind-protect
    handler-case eval-when defstruct
    macrolet symbol-macrolet
    multiple-value-call multiple-value-prog1
    values multiple-value-bind apply)
  "Forms handled directly by the parser/compiler — not subject to macro expansion.
   Note: declare, in-package, defpackage, export, locally, warn, coerce,
   with-open-file, copy-hash-table are registered as our-defmacro in macro.lisp.")

(defun expand-defstruct (form)
  "Expand (defstruct name-or-options slot...) to progn of defclass + defuns.
Supports :conc-name, :constructor with boa-lambda-list, slot defaults."
  (let* ((name-and-options (second form))
         (slots-raw (cddr form))
         (name (if (listp name-and-options) (first name-and-options) name-and-options))
         (options (when (listp name-and-options) (rest name-and-options)))
         (conc-name-opt (find :conc-name options :key (lambda (o) (when (listp o) (first o)))))
         (conc-name (if conc-name-opt
                        (second conc-name-opt)
                        (intern (concatenate 'string (symbol-name name) "-"))))
         (constructor-opt (find :constructor options :key (lambda (o) (when (listp o) (first o)))))
         (constructor-name (if constructor-opt
                               (second constructor-opt)
                               (intern (concatenate 'string "MAKE-" (symbol-name name)))))
         (boa-args (when (and constructor-opt (cddr constructor-opt))
                     (third constructor-opt)))
         (parsed-slots (mapcar (lambda (s)
                                 (if (listp s)
                                     (list (first s) (second s))
                                     (list s nil)))
                               (remove-if #'stringp slots-raw)))
         (predicate-name (intern (concatenate 'string (symbol-name name) "-P"))))
    (flet ((slot-accessor (slot-name)
             "Compute the accessor symbol for SLOT-NAME under CONC-NAME."
             (if conc-name
                 (intern (concatenate 'string (symbol-name conc-name) (symbol-name slot-name)))
                 slot-name)))
      ;; Register accessor→slot mappings at macro-expansion time
      (dolist (slot parsed-slots)
        (setf (gethash (slot-accessor (first slot)) *accessor-slot-map*)
              (cons name (first slot))))
      `(progn
         (defclass ,name ()
           ,(mapcar (lambda (slot)
                      `(,(first slot)
                        :initarg ,(intern (symbol-name (first slot)) "KEYWORD")
                        :initform ,(second slot)
                        :accessor ,(slot-accessor (first slot))))
                    parsed-slots))
         ,(if boa-args
              `(defun ,constructor-name ,boa-args
                 (make-instance ',name
                                ,@(mapcan (lambda (arg)
                                            (list (intern (symbol-name arg) "KEYWORD") arg))
                                          boa-args)))
              `(defun ,constructor-name (&key ,@(mapcar (lambda (slot)
                                                          (list (first slot) (second slot)))
                                                        parsed-slots))
                 (make-instance ',name
                                ,@(mapcan (lambda (slot)
                                            (list (intern (symbol-name (first slot)) "KEYWORD")
                                                  (first slot)))
                                          parsed-slots))))
         (defun ,predicate-name (obj) (typep obj ',name))
         ',name))))

(defun reduce-variadic-op (op args identity)
  "Reduce a variadic arithmetic form (OP arg...) to nested binary forms.
(OP) => IDENTITY, (OP a) => a, (OP a b) => (OP a b), (OP a b c ...) => (OP (OP a b) c) ..."
  (case (length args)
    (0 identity)
    (1 (first args))
    (2 (list op (first args) (second args)))
    (t (reduce (lambda (acc x) (list op acc x)) (cddr args)
               :initial-value (list op (first args) (second args))))))

;;; ------------------------------------------------------------
;;; Builtin Arity Classification (data layer)
;;; ------------------------------------------------------------
;;; These tables drive both the #'name → lambda wrapping and the
;;; (apply #'name list) → dolist-fold expansion in compiler-macroexpand-all.

(defparameter *variadic-fold-builtins*
  '(+ * append nconc)
  "Builtins that fold over a list with an identity: (OP a b c) = (OP (OP a b) c).")

(defparameter *binary-builtins*
  '(cons = < > <= >= mod rem eq eql equal
    nth nthcdr member assoc acons
    string= string< string> string<= string>= string-equal
    string-lessp string-greaterp string/=
    string-not-equal string-not-greaterp string-not-lessp string-concat
    char min max floor ceiling truncate round ffloor fceiling ftruncate fround
    ash logand logior logxor logeqv logtest logbitp
    expt scale-float gcd lcm complex
    array-dimension row-major-aref svref vector-push
    bit sbit bit-and bit-or bit-xor adjust-array)
  "Builtins that take exactly 2 arguments.")

(defparameter *unary-builtins*
  '(car cdr not null consp symbolp numberp integerp stringp
    atom listp characterp functionp
    first second third fourth fifth rest last
    nreverse butlast endp reverse length copy-list copy-tree
    symbol-name make-symbol intern gensym keywordp
    string-length string-upcase string-downcase
    char-code code-char
    typep hash-table-p hash-table-count
    hash-table-test hash-table-keys hash-table-values
    zerop plusp minusp evenp oddp abs lognot logcount integer-length
    sqrt exp log sin cos tan asin acos atan sinh cosh tanh float float-sign
    rational rationalize numerator denominator realpart imagpart conjugate phase
    boundp fboundp makunbound fmakunbound
    array-rank array-total-size array-dimensions
    fill-pointer array-has-fill-pointer-p array-adjustable-p vector-pop
    bit-not array-displacement char-int
    princ prin1 print write-to-string prin1-to-string princ-to-string
    type-of make-list alphanumericp eval identity)
  "Builtins that take exactly 1 argument.")

(defparameter *cxr-builtins*
  '(caar cadr cdar cddr
    caaar cdaar cadar cddar
    caadr cdadr caddr cdddr
    caaaar cadaar caadar caddar
    cdaaar cddaar cdadar cdddar
    caaadr cadadr caaddr cadddr
    cdaadr cddadr cdaddr cddddr)
  "CXR accessor builtins — all unary.")

(defparameter *all-builtin-names*
  (append *variadic-fold-builtins* '(- list) *binary-builtins* *unary-builtins* *cxr-builtins*)
  "Union of all known builtin names — used to decide whether #'name needs a lambda wrapper.")

;;; ------------------------------------------------------------
;;; compiler-macroexpand-all helper functions
;;; ------------------------------------------------------------

(defun register-defclass-accessors (class-name slot-specs)
  "Register ACCESSOR → (CLASS-NAME . SLOT-NAME) mappings in *accessor-slot-map*.
   Called during macro expansion so later (setf (accessor obj) val) forms
   can be lowered to (setf (slot-value ...)) without runtime lookup."
  (when (listp slot-specs)
    (dolist (spec slot-specs)
      (when (listp spec)
        (let ((accessor (getf (rest spec) :accessor)))
          (when accessor
            (setf (gethash accessor *accessor-slot-map*)
                  (cons class-name (first spec)))))))))

(defun expand-defclass-slot-spec (spec)
  "Expand only the :initform value inside a slot SPEC list, leaving all other
   keys (slot name, :accessor, :initarg, :reader, :writer, :type) untouched."
  (if (listp spec)
      (list* (first spec)
             (loop for (k v) on (rest spec) by #'cddr
                   append (list k (if (eq k :initform)
                                      (compiler-macroexpand-all v)
                                      v))))
      spec))

(defun expand-typed-defun-or-lambda (head name params rest-forms)
  "Strip type annotations from PARAMS, register the type signature, and
   rebuild a plain DEFUN or LAMBDA form with check-type assertions.
   HEAD is 'defun or 'lambda; NAME is the function name (nil for lambda)."
  (multiple-value-bind (plain-params type-alist)
      (strip-typed-params params)
    (let* ((has-return-type (and rest-forms
                                 (symbolp (first rest-forms))
                                 (cl-cc/type:looks-like-type-specifier-p (first rest-forms))))
           (return-type-spec (when has-return-type (first rest-forms)))
           (body-forms       (if has-return-type (cdr rest-forms) rest-forms)))
      ;; Register the function's type signature for the type checker
      (let ((param-types (mapcar (lambda (e)
                                   (cl-cc/type:parse-type-specifier (cdr e)))
                                 type-alist))
            (return-type  (if return-type-spec
                              (cl-cc/type:parse-type-specifier return-type-spec)
                              cl-cc/type:+type-unknown+)))
        (when (eq head 'defun)
          (register-function-type name param-types return-type)))
      ;; Build the annotated body
      (let* ((typed-body (if return-type-spec
                             `((the ,return-type-spec (progn ,@body-forms)))
                             body-forms))
             (checks (loop for (pname . ptype) in type-alist
                           collect `(check-type ,pname ,ptype)))
             (full-body (append checks typed-body)))
        (compiler-macroexpand-all
         (if (eq head 'defun)
             `(defun ,name ,plain-params ,@full-body)
             `(lambda ,plain-params ,@full-body)))))))

(defun make-macro-expander (lambda-list body)
  "Build a macro expander function for a LAMBDA-LIST and BODY.
   The expander destructures the macro call form and evaluates BODY."
  (let ((form-var (gensym "FORM"))
        (env-var  (gensym "ENV")))
    (eval `(lambda (,form-var ,env-var)
             (declare (ignore ,env-var))
             (let* ,(generate-lambda-bindings lambda-list form-var)
               ,@body)))))

(defun expand-macrolet-form (bindings body)
  "Register local macro BINDINGS, expand BODY under them, then restore.
   Returns the expanded BODY wrapped in PROGN."
  (let ((saved nil))
    (dolist (b bindings)
      (let ((name        (first b))
            (lambda-list (second b))
            (macro-body  (cddr b)))
        (push (cons name (lookup-macro name)) saved)
        (register-macro name (make-macro-expander lambda-list macro-body))))
    (let ((result (compiler-macroexpand-all (cons 'progn body))))
      (dolist (s saved)
        (if (cdr s)
            (register-macro (car s) (cdr s))
            (remhash (car s) (macro-env-table *macro-environment*))))
      result)))

(defun expand-progn-with-eager-defmacro (subforms)
  "Expand each form in SUBFORMS, eagerly registering any DEFMACRO/OUR-DEFMACRO forms so
   later sibling forms can immediately use the new macro."
  (let ((out nil))
    (dolist (sub subforms)
      (let ((exp (compiler-macroexpand-all sub)))
        (when (and (consp exp) (eq (car exp) 'defmacro))
          (register-macro (second exp)
                          (make-macro-expander (third exp) (cdddr exp))))
        ;; our-defmacro forms (from define-modify-macro, etc.) need to be evaluated
        ;; eagerly so that sibling forms can use the newly registered macro.
        (when (and (consp exp) (eq (car exp) 'our-defmacro))
          (eval exp))
        (push exp out)))
    (cons 'progn (nreverse out))))

(defun expand-function-builtin (name)
  "Wrap a known builtin NAME in a first-class lambda for higher-order use."
  (cond
    ((member name *variadic-fold-builtins*)
     (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X"))
           (id   (case name ((+) 0) ((*) 1) (t nil))))
       (compiler-macroexpand-all
        `(lambda (&rest ,args)
           (let ((,acc ,id))
             (dolist (,x ,args ,acc)
               (setq ,acc (,name ,acc ,x))))))))
    ((eq name '-)
     (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X")))
       (compiler-macroexpand-all
        `(lambda (&rest ,args)
           (if (null (cdr ,args))
               (- 0 (car ,args))
               (let ((,acc (car ,args)))
                 (dolist (,x (cdr ,args) ,acc)
                   (setq ,acc (- ,acc ,x)))))))))
    ((eq name 'list)
     (let ((args (gensym "ARGS")))
       (compiler-macroexpand-all `(lambda (&rest ,args) ,args))))
    ((member name *binary-builtins*)
     (let ((a (gensym "A")) (b (gensym "B")))
       (compiler-macroexpand-all `(lambda (,a ,b) (,name ,a ,b)))))
    (t
     (let ((x (gensym "X")))
       (compiler-macroexpand-all `(lambda (,x) (,name ,x)))))))

(defun expand-apply-named-fn (fn-name args-form)
  "Expand (apply 'FN-NAME args-form) where FN-NAME is a known symbol.
   Variadic builtins get a dolist fold; others normalise to (apply #'fn args)."
  (if (member fn-name (list* '- 'list *variadic-fold-builtins*))
      (let ((acc (gensym "ACC")) (x (gensym "X")) (lst (gensym "LST"))
            (id  (case fn-name ((+) 0) ((*) 1) (t nil))))
        (if (eq fn-name '-)
            (compiler-macroexpand-all
             `(let ((,lst ,args-form))
                (if (null (cdr ,lst))
                    (- 0 (car ,lst))
                    (let ((,acc (car ,lst)))
                      (dolist (,x (cdr ,lst) ,acc)
                        (setq ,acc (- ,acc ,x)))))))
            (compiler-macroexpand-all
             `(let ((,acc ,id))
                (dolist (,x ,args-form ,acc)
                  (setq ,acc (,fn-name ,acc ,x)))))))
      (list 'apply (list 'function fn-name)
            (compiler-macroexpand-all args-form))))

(defun expand-eval-when-form (situations body)
  "Handle EVAL-WHEN phase control.
   Evaluate BODY immediately if :compile-toplevel is listed; include in output
   if :execute or :load-toplevel is listed."
  (when (member :compile-toplevel situations)
    (dolist (b body)
      (our-eval (compiler-macroexpand-all b))))
  (if (or (member :execute situations)
          (member :load-toplevel situations))
      (compiler-macroexpand-all (cons 'progn body))
      nil))

(defun expand-setf-cons-place (place value)
  "Expand (setf (ACCESSOR ARGS...) value) for cons-cell accessors to rplaca/rplacd."
  (let ((v (gensym "V")))
    (case (car place)
      ((car first)
       `(let ((,v ,value)) (rplaca ,(second place) ,v) ,v))
      ((cdr rest)
       `(let ((,v ,value)) (rplacd ,(second place) ,v) ,v))
      (nth
       `(let ((,v ,value)) (rplaca (nthcdr ,(second place) ,(third place)) ,v) ,v))
      (cadr
       `(let ((,v ,value)) (rplaca (cdr ,(second place)) ,v) ,v))
      (cddr
       `(let ((,v ,value)) (rplacd (cdr ,(second place)) ,v) ,v)))))

(defun expand-make-array-form (size rest-args)
  "Expand (make-array size &rest keyword-args).
   Promotes to make-adjustable-vector when :fill-pointer or :adjustable is given."
  (let (fp adj)
    (loop for (key val) on rest-args by #'cddr
          do (case key
               (:fill-pointer (setf fp val))
               (:adjustable   (setf adj val))))
    (if (or fp adj)
        (compiler-macroexpand-all `(make-adjustable-vector ,size))
        (compiler-macroexpand-all `(make-array ,size)))))

(defun expand-setf-accessor (place value)
  "Expand (setf (ACCESSOR OBJ) VAL) via *accessor-slot-map* for known struct accessors,
   or fall back to generic (setf (slot-value obj 'accessor-name) val)."
  (let ((mapping (gethash (car place) *accessor-slot-map*)))
    (if mapping
        (compiler-macroexpand-all
         `(setf (slot-value ,(second place) ',(cdr mapping)) ,value))
        (compiler-macroexpand-all
         `(setf (slot-value ,(second place) ',(car place)) ,value)))))

(defun expand-let-binding (b)
  "Macro-expand the value in a LET binding, leaving the binding name untouched."
  (if (and (consp b) (symbolp (car b)))
      (list (car b) (compiler-macroexpand-all (cadr b)))
      b))

(defun expand-flet-labels-binding (binding)
  "Macro-expand only the body forms of an FLET/LABELS binding; leave params untouched."
  (if (and (consp binding) (>= (length binding) 3))
      (list* (first binding) (second binding)
             (mapcar #'compiler-macroexpand-all (cddr binding)))
      binding))

(defun expand-lambda-list-defaults (params)
  "Expand macro calls in &optional/&key default value positions within PARAMS.
Leaves required params, lambda-list keywords, and supplied-p vars untouched."
  (let (in-extended)
    (mapcar (lambda (p)
               (cond
                 ((member p '(&optional &rest &key &allow-other-keys &aux &body &whole))
                  (setf in-extended t) p)
                 ((and in-extended (consp p) (cdr p))
                  ;; (name default ...) — expand only the default (second element)
                  (list* (first p)
                         (compiler-macroexpand-all (second p))
                         (cddr p)))
                 (t p)))
             params)))

(defun compiler-macroexpand-all (form)
  "Expand macros in FORM for the compiler pipeline.
   Skips forms the compiler handles as special forms."
  (cond
    ;; Atoms pass through
    ((atom form) form)
    ;; Quote — never recurse into quoted forms
    ((eq (car form) 'quote) form)
    ;; (funcall 'name ...) with quoted symbol => (name ...) direct call
    ((and (consp form) (eq (car form) 'funcall)
          (>= (length form) 2)
          (consp (second form))
          (eq (car (second form)) 'quote)
          (symbolp (second (second form))))
     (compiler-macroexpand-all (cons (second (second form)) (cddr form))))
    ;; (apply fn a1 a2 ... list-form) with spread args => (apply fn (cons a1 (cons a2 ... list-form)))
    ((and (consp form) (eq (car form) 'apply)
          (> (length form) 3))
     (let* ((fn (second form))
            (spread-args (butlast (cddr form)))
            (last-arg (car (last form)))
            (combined (reduce (lambda (a rest) `(cons ,a ,rest))
                              spread-args :from-end t :initial-value last-arg)))
       (compiler-macroexpand-all `(apply ,fn ,combined))))
    ;; (apply 'name/function-ref list-form) — variadic builtins get dolist fold; others normalize to #'
    ((and (consp form) (eq (car form) 'apply)
          (= (length form) 3)
          (consp (second form))
          (or (and (eq (car (second form)) 'quote)    (symbolp (second (second form))))
              (and (eq (car (second form)) 'function) (symbolp (second (second form))))))
     (expand-apply-named-fn (second (second form)) (third form)))
    ;; (make-hash-table :test #'fn) → (make-hash-table :test 'fn)
    ((and (eq (car form) 'make-hash-table)
          (>= (length form) 3)
          (eq (second form) :test)
          (consp (third form))
          (eq (car (third form)) 'function)
          (symbolp (second (third form))))
     (compiler-macroexpand-all
      `(make-hash-table :test ',(second (third form)) ,@(cdddr form))))
    ;; (function builtin) — wrap builtins in lambda for first-class use
    ((eq (car form) 'function)
     (let ((name (second form)))
       (if (and (symbolp name) (member name *all-builtin-names*))
           (expand-function-builtin name)
           form)))
    ;; (multiple-value-list expr) — must live here (not our-defmacro) because the
    ;; %values-to-list VM intrinsic is position-sensitive: no other instruction
    ;; must execute between the multi-valued form and the capture call.
    ;; declare-ignore in the body is intentional — the binder just triggers the
    ;; VM's values-buffer fill; the primary-value binding itself is discarded.
    ((and (consp form) (eq (car form) 'multiple-value-list)
          (= (length form) 2))
     (let ((tmp (gensym "MVL")))
       (compiler-macroexpand-all
        `(let ((,tmp ,(second form)))
           (declare (ignore ,tmp))
           (%values-to-list)))))
    ;; Variadic builtins: (+ a b c) => (+ (+ a b) c), (append a b c) => (append (append a b) c)
    ((and (consp form) (member (car form) (cons '- *variadic-fold-builtins*))
          (/= (length (cdr form)) 2))
     (let ((op (car form))
           (identity (case (car form) ((+ -) 0) (* 1) (t nil))))
       (compiler-macroexpand-all
        (reduce-variadic-op op (cdr form) identity))))
    ;; (deftype name type-spec) — register type alias at expand time
    ;; (kept here rather than our-defmacro because cl-cc/type is loaded after macro.lisp)
    ((and (consp form) (eq (car form) 'deftype)
          (= (length form) 3)
          (symbolp (second form)))
     (cl-cc/type:register-type-alias (second form) (third form))
     `(quote ,(second form)))
    ;; (setf var val) => (setq var val) — plain variable assignment
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (symbolp (second form)))
     (compiler-macroexpand-all `(setq ,(second form) ,(third form))))
    ;; (setf (aref arr idx) val) => (aset arr idx val)
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (consp (second form))
          (eq (car (second form)) 'aref))
     (let ((arr (second (second form)))
           (idx (third (second form)))
           (val (third form)))
       (compiler-macroexpand-all `(aset ,arr ,idx ,val))))
    ;; (make-array size :fill-pointer fp :adjustable adj ...) => make-adjustable-vector
    ((and (consp form) (eq (car form) 'make-array)
          (>= (length form) 4))
     (expand-make-array-form (second form) (cddr form)))
    ;; (setf (car/cdr/first/rest/nth ...) val) — expand to rplaca/rplacd
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (consp (second form))
          (member (car (second form)) '(car cdr first rest nth cadr cddr)))
     (compiler-macroexpand-all
      (expand-setf-cons-place (second form) (third form))))
    ;; (setf (accessor obj) val) — expand via accessor-slot-map or to slot-value
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (consp (second form))
          (symbolp (car (second form)))
          (= (length (second form)) 2))
     (expand-setf-accessor (second form) (third form)))
    ;; (defstruct ...) — expand to defclass + constructor + predicate
    ((and (consp form) (eq (car form) 'defstruct))
     (compiler-macroexpand-all (expand-defstruct form)))
    ;; (eval-when (situations...) body...) — phase control
    ((and (consp form) (eq (car form) 'eval-when))
     (expand-eval-when-form (second form) (cddr form)))
    ;; (macrolet ((name lambda-list body)...) forms...) — local macros
    ((and (consp form) (eq (car form) 'macrolet))
     (expand-macrolet-form (second form) (cddr form)))
    ;; Typed defun: (defun name ((x fixnum) ...) return-type body...)
    ((and (consp form) (eq (car form) 'defun)
          (>= (length form) 4)
          (symbolp (second form))
          (listp (third form))
          (lambda-list-has-typed-p (third form)))
     (expand-typed-defun-or-lambda 'defun (second form) (third form) (cdddr form)))
    ;; Typed lambda: (lambda ((x fixnum) ...) return-type body...)
    ((and (consp form) (eq (car form) 'lambda)
          (>= (length form) 3)
          (listp (second form))
          (lambda-list-has-typed-p (second form)))
     (expand-typed-defun-or-lambda 'lambda nil (second form) (cddr form)))
    ;; (defclass name supers (slot-specs...)) — extract accessor mappings for setf expansion
    ((and (consp form) (eq (car form) 'defclass))
     (let ((class-name (second form))
           (slot-specs (fourth form)))
       (register-defclass-accessors class-name slot-specs)
       (list 'defclass class-name
             (mapcar #'compiler-macroexpand-all (third form))
             (when (listp slot-specs)
               (mapcar #'expand-defclass-slot-spec slot-specs)))))
    ;; progn — process forms sequentially so defmacro takes effect for later forms
    ((and (consp form) (eq (car form) 'progn))
     (expand-progn-with-eager-defmacro (cdr form)))
    ;; defun/lambda (untyped) — protect required params from expansion; expand default values and body
    ((and (consp form) (member (car form) '(defun lambda))
          (>= (length form) 3)
          (listp (second (if (eq (car form) 'lambda) form (cdr form)))))
     (if (eq (car form) 'defun)
         ;; (defun name params body...)
         (list* 'defun (second form) (expand-lambda-list-defaults (third form))
                (mapcar #'compiler-macroexpand-all (cdddr form)))
         ;; (lambda params body...)
         (list* 'lambda (expand-lambda-list-defaults (second form))
                (mapcar #'compiler-macroexpand-all (cddr form)))))
    ;; let — expand only binding VALUES, not binding names (let* is a macro, handled by our-macroexpand-1)
    ((and (consp form) (eq (car form) 'let)
          (>= (length form) 2) (listp (second form)))
     (list* 'let
            (mapcar #'expand-let-binding (second form))
            (mapcar #'compiler-macroexpand-all (cddr form))))
    ;; flet/labels — expand only function bodies, not binding structure
    ((and (consp form) (member (car form) '(flet labels))
          (>= (length form) 3) (listp (second form)))
     (list* (car form)
            (mapcar #'expand-flet-labels-binding (second form))
            (mapcar #'compiler-macroexpand-all (cddr form))))
    ;; FR-301: normalize 1-arg rounding forms to 2-arg with divisor 1
    ((and (consp form) (= (length form) 2)
          (member (car form) '(floor ceiling truncate round)))
     (compiler-macroexpand-all `(,(car form) ,(second form) 1)))
    ;; Special forms — recurse into subforms but don't expand the head
    ((and (symbolp (car form))
          (member (car form) *compiler-special-forms*))
     (cons (car form)
           (mapcar #'compiler-macroexpand-all (cdr form))))
    ;; Try macro expansion
    (t
     (multiple-value-bind (exp expanded-p)
         (our-macroexpand-1 form)
       (if expanded-p
           (compiler-macroexpand-all exp)
           (mapcar #'compiler-macroexpand-all form))))))

(defun compile-expression (expr &key (target :x86_64) type-check)
  (let* ((ctx (make-instance 'compiler-context))
         (expanded-expr (if (typep expr 'ast-node)
                            expr
                            (compiler-macroexpand-all expr)))
         (ast (if (typep expanded-expr 'ast-node)
                  expanded-expr
                  (lower-sexp-to-ast expanded-expr)))
         (inferred-type (when type-check
                          (handler-case (type-check-ast ast)
                            (error (e)
                              (if (eq type-check :strict)
                                  (error e)
                                  (warn "Type check warning: ~A" e)
                                  )))))
         (result-reg (compile-ast ast ctx))
         (instructions (nreverse (ctx-instructions ctx)))
         (full-instructions (append instructions
                                    (list (make-vm-halt
                                                         :reg result-reg))))
         (optimized-instructions (optimize-instructions full-instructions))
         (optimized-program (make-vm-program
                             :instructions optimized-instructions
                             :result-register result-reg)))
    (make-compilation-result :program optimized-program
                            :assembly (emit-assembly optimized-program :target target)
                            :type (when type-check inferred-type)
                            :cps (if (typep expr 'ast-node)
                                     nil
                                     (handler-case (cps-transform expr)
                                       (error (e) (declare (ignore e)) nil))))))

;;; Standard Library (Higher-Order Functions)

(defparameter *standard-library-source*
  "(defun mapcar (fn lst)
     (if (null lst) nil
       (cons (funcall fn (car lst))
             (mapcar fn (cdr lst)))))
   (defun mapc (fn lst)
     (if (null lst) nil
       (progn (funcall fn (car lst))
              (mapc fn (cdr lst))))
     lst)
   (defun mapcan (fn lst)
     (if (null lst) nil
       (nconc (funcall fn (car lst))
              (mapcan fn (cdr lst)))))
   (defun remove-if (pred lst)
     (if (null lst) nil
       (if (funcall pred (car lst))
           (remove-if pred (cdr lst))
           (cons (car lst) (remove-if pred (cdr lst))))))
   (defun remove-if-not (pred lst)
     (if (null lst) nil
       (if (funcall pred (car lst))
           (cons (car lst) (remove-if-not pred (cdr lst)))
           (remove-if-not pred (cdr lst)))))
   (defun find-if (pred lst)
     (if (null lst) nil
       (if (funcall pred (car lst))
           (car lst)
           (find-if pred (cdr lst)))))
   (defun every (pred lst)
     (if (null lst) t
       (if (funcall pred (car lst))
           (every pred (cdr lst))
           nil)))
   (defun some (pred lst)
     (if (null lst) nil
       (if (funcall pred (car lst))
           t
           (some pred (cdr lst)))))
   (defun reduce-init (fn lst acc)
     (if (null lst) acc
       (reduce-init fn (cdr lst) (funcall fn acc (car lst)))))
   (defun reduce (fn lst &optional initial-value has-init)
     (if has-init
         (reduce-init fn lst initial-value)
         (if (null (cdr lst)) (car lst)
           (reduce-init fn (cdr (cdr lst))
                        (funcall fn (car lst) (car (cdr lst)))))))
   (defun count-if (pred lst)
     (if (null lst) 0
       (+ (if (funcall pred (car lst)) 1 0)
          (count-if pred (cdr lst)))))
   (defun position-if (pred lst)
     (labels ((pos-helper (pred lst idx)
                (if (null lst) nil
                  (if (funcall pred (car lst)) idx
                    (pos-helper pred (cdr lst) (+ idx 1))))))
       (pos-helper pred lst 0)))
   (defun notevery (pred lst) (not (every pred lst)))
   (defun notany (pred lst) (not (some pred lst)))
   (defun member-eql (item lst)
     (if (null lst) nil
       (if (eql item (car lst)) lst
         (member-eql item (cdr lst)))))
   (defun set-difference (lst1 lst2)
     (if (null lst1) nil
       (if (member-eql (car lst1) lst2)
           (set-difference (cdr lst1) lst2)
           (cons (car lst1) (set-difference (cdr lst1) lst2)))))
   (defun union-lists (lst1 lst2)
     (if (null lst1) lst2
       (if (member-eql (car lst1) lst2)
           (union-lists (cdr lst1) lst2)
           (cons (car lst1) (union-lists (cdr lst1) lst2)))))
   (defun last-cons (lst)
     (if (null (cdr lst)) lst
       (last-cons (cdr lst))))
   (defun append-lists (lst1 lst2)
     (if (null lst1) lst2
       (cons (car lst1) (append-lists (cdr lst1) lst2))))
   (defun maphash-fn (fn ht)
     (dolist (k (hash-table-keys ht))
       (funcall fn k (gethash k ht)))
     nil)
   (defun getf (plist indicator &optional default)
     (if (null plist) default
       (if (eql (car plist) indicator) (car (cdr plist))
         (getf (cdr (cdr plist)) indicator default))))
   (defun intersection (lst1 lst2)
     (if (null lst1) nil
       (if (member-eql (car lst1) lst2)
           (cons (car lst1) (intersection (cdr lst1) lst2))
           (intersection (cdr lst1) lst2))))
   (defun remove (item lst)
     (remove-if (lambda (x) (eql x item)) lst))
   (defun find (item lst &key key test)
     (let ((test-fn (if test test (lambda (a b) (eql a b)))))
       (dolist (x lst nil)
         (let ((val (if key (funcall key x) x)))
           (when (funcall test-fn item val)
             (return x))))))
   (defun position (item lst &key key test)
     (let ((test-fn (if test test (lambda (a b) (eql a b))))
           (idx 0))
       (dolist (x lst nil)
         (let ((val (if key (funcall key x) x)))
           (when (funcall test-fn item val)
             (return idx)))
         (setq idx (+ idx 1)))))
   (defun assoc-if (pred alist)
     (if (null alist) nil
       (if (funcall pred (car (car alist))) (car alist)
         (assoc-if pred (cdr alist)))))
   (defun rassoc (item alist)
     (if (null alist) nil
       (if (eql item (cdr (car alist))) (car alist)
         (rassoc item (cdr alist)))))
   (defun pairlis (keys values &optional alist)
     (if (null keys) alist
       (cons (cons (car keys) (car values))
             (pairlis (cdr keys) (cdr values) alist))))
   (defun identity (x) x)
   (defun constantly (value) (lambda (&rest args) (declare (ignore args)) value))
   (defun complement (fn) (lambda (&rest args) (not (apply fn args))))
   (defun sort-impl (sequence predicate key)
     (if (null sequence) nil
       (let ((pivot (car sequence))
             (less nil)
             (greater nil))
         (dolist (x (cdr sequence))
           (let ((a (if key (funcall key x) x))
                 (b (if key (funcall key pivot) pivot)))
             (if (funcall predicate a b)
                 (push x less)
                 (push x greater))))
         (append (sort-impl less predicate key)
                 (cons pivot (sort-impl greater predicate key))))))
   (defun sort (sequence predicate &key key)
     (sort-impl sequence predicate key))
   (defun stable-sort (sequence predicate &key key)
     (sort-impl sequence predicate key))
   (defun remove-duplicates (lst)
     (let ((result nil))
       (dolist (x lst)
         (unless (member-eql x result)
           (push x result)))
       (nreverse result)))"
  "Standard library source defining higher-order functions and set operations.")

(defparameter *stdlib-compiled* nil
  "Cache for compiled standard library instructions.")

(defun get-stdlib-forms ()
  "Parse the standard library source into forms."
  (parse-all-forms *standard-library-source*))

(defun compile-string (source &key (target :x86_64) type-check)
  (let ((forms (parse-all-forms source)))
    (if (= (length forms) 1)
        (compile-expression (first forms) :target target :type-check type-check)
        ;; Multiple forms: use compile-toplevel-forms for sequential macro expansion
        (compile-toplevel-forms forms :target target))))

(defun run-string (source &key stdlib)
  "Compile and run SOURCE. When STDLIB is true, include standard library."
  (let* ((*package* (find-package :cl-cc))
         (*accessor-slot-map* (make-hash-table :test #'eq))
         (*labels-boxed-fns* nil)
         (result (if stdlib
                     (compile-string-with-stdlib source :target :vm)
                     (compile-string source :target :vm)))
         (program (compilation-result-program result)))
    (run-compiled program)))

(defun compile-string-with-stdlib (source &key (target :x86_64))
  "Compile SOURCE with standard library prepended."
  (let ((stdlib-forms (get-stdlib-forms))
        (user-forms (parse-all-forms source)))
    (compile-toplevel-forms (append stdlib-forms user-forms) :target target)))

(defun our-eval (form)
  "Evaluate FORM by compiling it and running it in the VM.
This is the self-hosting eval — used for compile-time macro expansion
instead of the host CL eval."
  (let* ((result (compile-expression form :target :vm))
         (program (compilation-result-program result)))
    (run-compiled program)))

(defun run-string-typed (source &key (mode :warn))
  "Compile and run SOURCE with type checking enabled.
   MODE is :WARN (default, log warnings) or :STRICT (signal errors)."
  (let* ((result (compile-string source :target :vm :type-check mode))
         (program (compilation-result-program result)))
    (values (run-compiled program) (compilation-result-type result))))

;;; Native Executable Generation (Mach-O)

(defun compile-to-native (source &key (arch :x86-64) (output-file "a.out"))
  "Compile SOURCE to a native Mach-O executable.
SOURCE can be a string (single expression) or a list of forms.
ARCH is :X86-64 or :ARM64.
OUTPUT-FILE is the path for the executable.

Returns the output file path on success."
  (let* ((target (ecase arch
                   (:x86-64 :x86_64)
                   (:arm64 :aarch64)))
         ;; Parse and compile to VM program
         (result (if (stringp source)
                     (compile-string source :target target)
                     (compile-expression source :target target)))
         (program (compilation-result-program result))
         ;; Generate machine code bytes
         (code-bytes (compile-to-x86-64-bytes program))
         ;; Build Mach-O binary
         (builder (cl-cc/binary:make-mach-o-builder arch)))
    ;; Add code as __TEXT segment
    (cl-cc/binary:add-text-segment builder code-bytes)
    ;; Add entry point symbol
    (cl-cc/binary:add-symbol builder "_main" :value 0 :type #x0F :sect 1)
    ;; Build and write
    (let ((mach-o-bytes (cl-cc/binary:build-mach-o builder code-bytes)))
      (cl-cc/binary:write-mach-o-file output-file mach-o-bytes))
    ;; Make executable
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" (namestring output-file))
                                :search nil :wait t)
    output-file))

(defun compile-file-to-native (input-file &key (arch :x86-64) (output-file nil))
  "Compile a CL-CC source file to a native Mach-O executable.
INPUT-FILE is the path to the source file.
OUTPUT-FILE defaults to INPUT-FILE with no extension."
  (let* ((output (or output-file
                     (make-pathname :type nil :defaults input-file)))
         (source (with-open-file (in input-file :direction :input)
                   (let ((forms nil))
                     (handler-case
                         (loop (push (read in) forms))
                       (end-of-file () nil))
                     (nreverse forms))))
         (result (compile-toplevel-forms source :target (ecase arch
                                                          (:x86-64 :x86_64)
                                                          (:arm64 :aarch64))))
         (program (compilation-result-program result))
         (code-bytes (compile-to-x86-64-bytes program))
         (builder (cl-cc/binary:make-mach-o-builder arch)))
    (cl-cc/binary:add-text-segment builder code-bytes)
    (cl-cc/binary:add-symbol builder "_main" :value 0 :type #x0F :sect 1)
    (let ((mach-o-bytes (cl-cc/binary:build-mach-o builder code-bytes)))
      (cl-cc/binary:write-mach-o-file output mach-o-bytes))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" (namestring output))
                                :search nil :wait t)
    output))
