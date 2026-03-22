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
    (emit ctx (make-instance 'vm-const :dst dst :value (ast-int-value node)))
    dst))

(defmethod compile-ast ((node ast-var) ctx)
  (let ((name (ast-var-name node)))
    (cond
      ((eq name t)
       (let ((dst (make-register ctx)))
         (emit ctx (make-instance 'vm-const :dst dst :value t))
         dst))
      ((eq name nil)
       (let ((dst (make-register ctx)))
         (emit ctx (make-instance 'vm-const :dst dst :value nil))
         dst))
      ((keywordp name)
       (let ((dst (make-register ctx)))
         (emit ctx (make-instance 'vm-const :dst dst :value name))
         dst))
      (t
       (let ((local-entry (assoc name (ctx-env ctx))))
         (cond
           ;; Local binding takes priority over global
           ((and local-entry (member name (ctx-boxed-vars ctx)))
            ;; Boxed variable: read via (car box)
            (let ((unboxed (make-register ctx)))
              (emit ctx (make-instance 'vm-car :dst unboxed :src (cdr local-entry)))
              unboxed))
           (local-entry
            (cdr local-entry))
           ;; Global variable: load from global store (persists across function calls)
           ((gethash name (ctx-global-variables ctx))
            (let ((dst (make-register ctx)))
              (emit ctx (make-instance 'vm-get-global :dst dst :name name))
              dst))
           (t
            (error "Unbound variable: ~S" name))))))))

(defmethod compile-ast ((node ast-binop) ctx)
  (let* ((lhs (compile-ast (ast-binop-lhs node) ctx))
         (rhs (compile-ast (ast-binop-rhs node) ctx))
         (dst (make-register ctx))
         (inst-class (ecase (ast-binop-op node)
                       (+ 'vm-add)
                       (- 'vm-sub)
                       (* 'vm-mul)
                       (= 'vm-num-eq)
                       (< 'vm-lt)
                       (> 'vm-gt)
                       (<= 'vm-le)
                       (>= 'vm-ge))))
    (emit ctx (make-instance inst-class :dst dst :lhs lhs :rhs rhs))
    dst))

(defmethod compile-ast ((node ast-progn) ctx)
  (let ((last nil))
    (dolist (form (ast-progn-forms node))
      (setf last (compile-ast form ctx)))
    last))

(defmethod compile-ast ((node ast-print) ctx)
  (let ((reg (compile-ast (ast-print-expr node) ctx)))
    (emit ctx (make-instance 'vm-print :reg reg))
    reg))

(defmethod compile-ast ((node ast-if) ctx)
  (let* ((cond-reg (compile-ast (ast-if-cond node) ctx))
         (dst (make-register ctx))
         (else-label (make-label ctx "else"))
         (end-label (make-label ctx "ifend")))
    (emit ctx (make-instance 'vm-jump-zero :reg cond-reg :label else-label))
    (let ((then-reg (compile-ast (ast-if-then node) ctx)))
      (emit ctx (make-instance 'vm-move :dst dst :src then-reg))
      (emit ctx (make-instance 'vm-jump :label end-label)))
    (emit ctx (make-instance 'vm-label :name else-label))
    (let ((else-reg (compile-ast (ast-if-else node) ctx)))
      (emit ctx (make-instance 'vm-move :dst dst :src else-reg)))
    (emit ctx (make-instance 'vm-label :name end-label))
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
                 (emit ctx (make-instance 'vm-move :dst own-reg :src val-reg))
                 (if (member name needs-boxing)
                     ;; Box the variable: wrap in (cons val nil)
                     (let ((box-reg (make-register ctx))
                           (nil-reg (make-register ctx)))
                       (emit ctx (make-instance 'vm-const :dst nil-reg :value nil))
                       (emit ctx (make-instance 'vm-cons :dst box-reg :car-src own-reg :cdr-src nil-reg))
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

;;; ----------------------------------------------------------------------------
;;; Control Flow: block/return-from
;;; ----------------------------------------------------------------------------

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
             (emit ctx (make-instance 'vm-move :dst result-reg :src body-result))))
      ;; Restore block environment
      (setf (ctx-block-env ctx) old-block-env))
    ;; Emit the exit label for return-from to jump to
    (emit ctx (make-instance 'vm-label :name exit-label))
    result-reg))

(defmethod compile-ast ((node ast-return-from) ctx)
  (let* ((block-name (ast-return-from-name node))
         (block-info (lookup-block ctx block-name))
         (exit-label (car block-info))
         (result-reg (cdr block-info))
         (value-reg (compile-ast (ast-return-from-value node) ctx)))
    ;; Move value to block's result register
    (emit ctx (make-instance 'vm-move :dst result-reg :src value-reg))
    ;; Jump to block's exit label
    (emit ctx (make-instance 'vm-jump :label exit-label))
    ;; Return the result register (though this won't be reached)
    result-reg))

;;; ----------------------------------------------------------------------------
;;; Control Flow: tagbody/go
;;; ----------------------------------------------------------------------------

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
               (emit ctx (make-instance 'vm-jump :label (cdar tag-labels)))
               (emit ctx (make-instance 'vm-const :dst result-reg :value nil)))
           ;; Emit code for each tag section
           (dolist (tag-entry tags)
             (let* ((tag (car tag-entry))
                    (forms (cdr tag-entry))
                    (label (cdr (assoc tag tag-labels))))
               ;; Emit the label for this tag
               (emit ctx (make-instance 'vm-label :name label))
               ;; Compile forms for this tag
               (when forms
                 (let ((last-reg nil))
                   (dolist (form forms)
                     (setf last-reg (compile-ast form ctx)))
                   ;; After last form, jump to end (implicit go)
                   (emit ctx (make-instance 'vm-move :dst result-reg :src last-reg))
                   (emit ctx (make-instance 'vm-jump :label end-label))))))
           ;; If no tags at all, result is nil
           (unless tag-labels
             (emit ctx (make-instance 'vm-const :dst result-reg :value nil))))
      ;; Restore tagbody environment
      (setf (ctx-tagbody-env ctx) old-tagbody-env))
    ;; Emit the end label
    (emit ctx (make-instance 'vm-label :name end-label))
    result-reg))

(defmethod compile-ast ((node ast-go) ctx)
  (let* ((tag (ast-go-tag node))
         (label (lookup-tag ctx tag)))
    (emit ctx (make-instance 'vm-jump :label label))
    ;; Return a dummy register (won't be reached)
    (make-register ctx)))

;;; ----------------------------------------------------------------------------
;;; Assignment: setq
;;; ----------------------------------------------------------------------------

(defmethod compile-ast ((node ast-setq) ctx)
  (let* ((var-name (ast-setq-var node))
         (value-reg (compile-ast (ast-setq-value node) ctx))
         (local-entry (assoc var-name (ctx-env ctx))))
    (cond
      ;; Local binding takes priority (even if name matches a global)
      ((and local-entry (member var-name (ctx-boxed-vars ctx)))
       ;; Boxed variable: write via (rplaca box new-val)
       (emit ctx (make-instance 'vm-rplaca :cons (cdr local-entry) :val value-reg))
       value-reg)
      (local-entry
       ;; Unboxed local: direct register move
       (emit ctx (make-instance 'vm-move :dst (cdr local-entry) :src value-reg))
       (cdr local-entry))
      ;; Global variable: write to global store (persists across function calls)
      ((gethash var-name (ctx-global-variables ctx))
       (emit ctx (make-instance 'vm-set-global :name var-name :src value-reg))
       value-reg)
      (t
       (error "Unbound variable for setq: ~S" var-name)))))

;;; ----------------------------------------------------------------------------
;;; Quote: literal values
;;; ----------------------------------------------------------------------------

(defmethod compile-ast ((node ast-quote) ctx)
  (let ((dst (make-register ctx))
        (value (ast-quote-value node)))
    ;; For simple values (integers, symbols, strings, lists of simple values)
    ;; we can emit them as constants
    ;; For complex structures, this would need proper serialization
    (emit ctx (make-instance 'vm-const :dst dst :value value))
    dst))

;;; ----------------------------------------------------------------------------
;;; The: type declarations (informational only)
;;; ----------------------------------------------------------------------------

(defmethod compile-ast ((node ast-the) ctx)
  ;; The type declaration is informational - just compile the value
  (compile-ast (ast-the-value node) ctx))

;;; ----------------------------------------------------------------------------
;;; CLOS Compilation
;;; ----------------------------------------------------------------------------

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
    (emit ctx (make-instance 'vm-class-def
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
         (emit ctx (make-instance 'vm-closure
                                 :dst closure-reg
                                 :label func-label
                                 :params (list obj-reg)
                                 :captured nil))
         ;; Register as a callable function
         (push (cons accessor-name closure-reg) (ctx-env ctx))
         (setf (gethash accessor-name (ctx-global-functions ctx)) func-label)
         ;; Jump over function body
         (emit ctx (make-instance 'vm-jump :label end-label))
         ;; Function body: read slot
         (emit ctx (make-instance 'vm-label :name func-label))
         (emit ctx (make-instance 'vm-slot-read
                                 :dst result-reg
                                 :obj-reg obj-reg
                                 :slot-name slot-name))
         (emit ctx (make-instance 'vm-ret :reg result-reg))
         (emit ctx (make-instance 'vm-label :name end-label))))
      (:writer
       (let ((val-reg (make-register ctx))
             (obj-reg (make-register ctx)))
         ;; Create closure with two parameters: (new-value object)
         (emit ctx (make-instance 'vm-closure
                                 :dst closure-reg
                                 :label func-label
                                 :params (list val-reg obj-reg)
                                 :captured nil))
         (push (cons accessor-name closure-reg) (ctx-env ctx))
         (setf (gethash accessor-name (ctx-global-functions ctx)) func-label)
         (emit ctx (make-instance 'vm-jump :label end-label))
         (emit ctx (make-instance 'vm-label :name func-label))
         (emit ctx (make-instance 'vm-slot-write
                                 :obj-reg obj-reg
                                 :slot-name slot-name
                                 :value-reg val-reg))
         (emit ctx (make-instance 'vm-ret :reg val-reg))
         (emit ctx (make-instance 'vm-label :name end-label)))))
    closure-reg))

(defmethod compile-ast ((node ast-defgeneric) ctx)
  "Compile a generic function definition.
Creates a dispatch table (hash table) that maps class names to method closures."
  (let* ((name (ast-defgeneric-name node))
         (dst (make-register ctx)))
    ;; A generic function is a hash table with :__name__, :__params__, :__methods__
    (emit ctx (make-instance 'vm-class-def
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
    (emit ctx (make-instance 'vm-closure
                            :dst closure-reg
                            :label func-label
                            :params param-regs
                            :captured nil))
    ;; Register the method on the generic function with composite key
    (emit ctx (make-instance 'vm-register-method
                            :gf-reg gf-reg
                            :specializer dispatch-key
                            :method-reg closure-reg))
    ;; Jump over method body
    (emit ctx (make-instance 'vm-jump :label end-label))
    ;; Method body
    (emit ctx (make-instance 'vm-label :name func-label))
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
               (emit ctx (make-instance 'vm-ret :reg last-reg))))
        (setf (ctx-env ctx) old-env)))
    (emit ctx (make-instance 'vm-label :name end-label))
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
    (emit ctx (make-instance 'vm-make-obj
                            :dst dst
                            :class-reg class-reg
                            :initarg-regs initarg-regs))
    dst))

(defmethod compile-ast ((node ast-slot-value) ctx)
  "Compile slot-value access."
  (let* ((obj-reg (compile-ast (ast-slot-value-object node) ctx))
         (slot-name (ast-slot-value-slot node))
         (dst (make-register ctx)))
    (emit ctx (make-instance 'vm-slot-read
                            :dst dst
                            :obj-reg obj-reg
                            :slot-name slot-name))
    dst))

(defmethod compile-ast ((node ast-set-slot-value) ctx)
  "Compile (setf (slot-value obj 'slot) value)."
  (let* ((obj-reg (compile-ast (ast-set-slot-value-object node) ctx))
         (val-reg (compile-ast (ast-set-slot-value-value node) ctx))
         (slot-name (ast-set-slot-value-slot node)))
    (emit ctx (make-instance 'vm-slot-write
                            :obj-reg obj-reg
                            :slot-name slot-name
                            :value-reg val-reg))
    val-reg))

(defmethod compile-ast ((node ast-set-gethash) ctx)
  "Compile (setf (gethash key table) value)."
  (let* ((key-reg (compile-ast (ast-set-gethash-key node) ctx))
         (table-reg (compile-ast (ast-set-gethash-table node) ctx))
         (val-reg (compile-ast (ast-set-gethash-value node) ctx)))
    (emit ctx (make-instance 'vm-sethash
                            :key key-reg
                            :table table-reg
                            :value val-reg))
    val-reg))

;;; ----------------------------------------------------------------------------
;;; Top-Level Definitions: defun
;;; ----------------------------------------------------------------------------

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
      (emit ctx (make-instance 'vm-const :dst dst :value name))
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
      (dolist (opt optional-params)
        (let* ((name (first opt))
               (default-ast (second opt))
               (reg (make-register ctx)))
          (multiple-value-bind (default-val is-constant)
              (if default-ast (extract-constant-value default-ast) (values nil t))
            (if (and default-ast (not is-constant))
                (progn
                  (push (list reg *non-constant-default-sentinel*) opt-closure-data)
                  (push (cons reg default-ast) non-constant-defaults))
                (push (list reg default-val) opt-closure-data)))
          (push (cons name reg) opt-bindings)))
      (setf opt-closure-data (nreverse opt-closure-data))
      (setf opt-bindings (nreverse opt-bindings)))
    (when rest-param
      (setf rest-reg (make-register ctx))
      (setf rest-binding (cons rest-param rest-reg)))
    (when key-params
      (dolist (kp key-params)
        (let* ((name (first kp))
               (default-ast (second kp))
               (reg (make-register ctx))
               (keyword (intern (symbol-name name) "KEYWORD")))
          (multiple-value-bind (default-val is-constant)
              (if default-ast (extract-constant-value default-ast) (values nil t))
            (if (and default-ast (not is-constant))
                (progn
                  (push (list keyword reg *non-constant-default-sentinel*) key-closure-data)
                  (push (cons reg default-ast) non-constant-defaults))
                (push (list keyword reg default-val) key-closure-data)))
          (push (cons name reg) key-bindings)))
      (setf key-closure-data (nreverse key-closure-data))
      (setf key-bindings (nreverse key-bindings)))
    (setf non-constant-defaults (nreverse non-constant-defaults))
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
      (emit ctx (make-instance 'vm-const :dst sentinel-reg :value *non-constant-default-sentinel*))
      ;; Compare: if param-reg != sentinel (eq), skip default computation
      (emit ctx (make-instance 'vm-eq :dst cmp-reg :lhs param-reg :rhs sentinel-reg))
      (emit ctx (make-instance 'vm-jump-zero :reg cmp-reg :label skip-label))
      ;; Compute the actual default value
      (let ((default-reg (compile-ast default-ast ctx)))
        (emit ctx (make-instance 'vm-move :dst param-reg :src default-reg)))
      (emit ctx (make-instance 'vm-label :name skip-label)))))

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
         (free-vars (let ((temp-ast (make-instance 'ast-lambda
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
      (emit ctx (make-instance 'vm-closure
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
      (emit ctx (make-instance 'vm-register-function :name name :src closure-reg))
      ;; Jump over the function body
      (emit ctx (make-instance 'vm-jump :label end-label))
      ;; Emit the function entry label
      (emit ctx (make-instance 'vm-label :name func-label))
      ;; Save old environment and extend with parameters
      (let ((old-env (ctx-env ctx)))
        (unwind-protect
             (progn
               (setf (ctx-env ctx)
                     (append (build-all-param-bindings params param-regs
                                                      opt-bindings rest-binding key-bindings)
                             (ctx-env ctx)))
               ;; Emit inline default computation for non-constant defaults
               (when non-constant-defaults
                 (emit-non-constant-defaults ctx non-constant-defaults))
               ;; Compile body forms
               (let ((last-reg nil))
                 (dolist (form body)
                   (setf last-reg (compile-ast form ctx)))
                 (emit ctx (make-instance 'vm-ret :reg last-reg))))
          ;; Restore environment
          (setf (ctx-env ctx) old-env)))
      ;; Emit end label
      (emit ctx (make-instance 'vm-label :name end-label))
      closure-reg)))

;;; ----------------------------------------------------------------------------
;;; Top-Level Definitions: defvar/defparameter
;;; ----------------------------------------------------------------------------

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
                          (emit ctx (make-instance 'vm-const :dst nil-reg :value nil))
                          nil-reg))))
    ;; Store in global variable store
    (emit ctx (make-instance 'vm-set-global :name name :src value-reg))
    ;; Register as global (so ast-var and ast-setq use global get/set)
    (setf (gethash name (ctx-global-variables ctx)) t)
    value-reg))

;;; ----------------------------------------------------------------------------
;;; Multi-Form Compilation (for compiling entire files)
;;; ----------------------------------------------------------------------------

(defun compile-toplevel-forms (forms &key (target :x86_64))
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
Returns a property list with :program, :assembly, and :globals."
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
      (emit ctx (make-instance 'vm-halt :reg last-reg)))
    (let* ((instructions (nreverse (ctx-instructions ctx)))
           (optimized (optimize-instructions instructions))
           (program (make-instance 'vm-program
                                   :instructions optimized
                                   :result-register last-reg)))
      (list :program program
            :assembly (emit-assembly program :target target)
            :globals (ctx-global-functions ctx)))))

;;; ----------------------------------------------------------------------------
;;; Exception Handling: catch/throw/unwind-protect
;;; ----------------------------------------------------------------------------

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
    (emit ctx (make-instance 'vm-label :name (make-label ctx "catch_start")))
    ;; Compile body forms
    (let ((body-result (let ((last nil))
                         (dolist (form (ast-catch-body node))
                           (setf last (compile-ast form ctx)))
                         last)))
      (emit ctx (make-instance 'vm-move :dst result-reg :src body-result)))
    (emit ctx (make-instance 'vm-label :name end-label))
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
    (emit ctx (make-instance 'vm-const :dst error-flag-reg :value nil))
    ;; Establish catch-all handler for errors during protected form
    (emit ctx (make-instance 'vm-establish-handler
                             :handler-label handler-label
                             :result-reg error-reg
                             :error-type 'error))
    ;; Compile protected form
    (let ((protected-result (compile-ast (ast-unwind-protected node) ctx)))
      (emit ctx (make-instance 'vm-move :dst result-reg :src protected-result)))
    ;; Remove handler (normal exit)
    (emit ctx (make-instance 'vm-remove-handler))
    ;; Jump to cleanup (normal path)
    (emit ctx (make-instance 'vm-jump :label cleanup-label))
    ;; Error handler: set error flag and fall through to cleanup
    (emit ctx (make-instance 'vm-label :name handler-label))
    (emit ctx (make-instance 'vm-const :dst error-flag-reg :value t))
    ;; Cleanup section (runs on both normal and error paths)
    (emit ctx (make-instance 'vm-label :name cleanup-label))
    (dolist (form (ast-unwind-cleanup node))
      (compile-ast form ctx))
    ;; If error occurred, re-signal it
    (emit ctx (make-instance 'vm-jump-zero :reg error-flag-reg :label end-label))
    ;; Sync register state to remaining handlers so cleanup side-effects propagate
    (emit ctx (make-instance 'vm-sync-handler-regs))
    ;; Re-signal the caught error
    (emit ctx (make-instance 'vm-signal-error :error-reg error-reg))
    ;; Normal end
    (emit ctx (make-instance 'vm-label :name end-label))
    result-reg))

;;; ----------------------------------------------------------------------------
;;; Handler-Case
;;; ----------------------------------------------------------------------------

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
               (emit ctx (make-instance 'vm-establish-handler
                                       :handler-label handler-label
                                       :result-reg error-reg
                                       :error-type error-type))))
    ;; 2. Compile the protected form
    (let ((form-result (compile-ast (ast-handler-case-form node) ctx)))
      (emit ctx (make-instance 'vm-move :dst result-reg :src form-result)))
    ;; 3. Remove handlers (one per clause)
    (dotimes (i (length clauses))
      (emit ctx (make-instance 'vm-remove-handler)))
    ;; 4. Jump to normal exit
    (emit ctx (make-instance 'vm-jump :label normal-exit-label))
    ;; 5. Emit handler bodies
    (loop for clause in clauses
          for info in handler-infos
          do (let* ((var (second clause))
                    (body (cddr clause))
                    (handler-label (first info))
                    (error-reg (second info))
                    (old-env (ctx-env ctx)))
               ;; Emit handler label
               (emit ctx (make-instance 'vm-label :name handler-label))
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
                        (emit ctx (make-instance 'vm-move :dst result-reg :src last-reg))))
                 (setf (ctx-env ctx) old-env))
               ;; Jump to normal exit after handler
               (emit ctx (make-instance 'vm-jump :label normal-exit-label))))
    ;; 6. Emit normal exit label
    (emit ctx (make-instance 'vm-label :name normal-exit-label))
    result-reg))

;;; ----------------------------------------------------------------------------
;;; Multiple Values
;;; ----------------------------------------------------------------------------

(defmethod compile-ast ((node ast-values) ctx)
  (let* ((forms (ast-values-forms node))
         (src-regs (mapcar (lambda (form) (compile-ast form ctx)) forms))
         (dst (make-register ctx)))
    (emit ctx (make-instance 'vm-values :dst dst :src-regs src-regs))
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
      (emit ctx (make-instance 'vm-mv-bind :dst-regs var-regs))
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
    (emit ctx (make-instance 'vm-apply :dst result-reg :func func-reg :args arg-regs))
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-call) ctx)
  ;; Multiple-value-call passes all values from arguments to function.
  ;; For simplicity, we just call with the first value of each form.
  (let ((func-reg (compile-ast (ast-mv-call-func node) ctx))
        (arg-regs (mapcar (lambda (arg) (compile-ast arg ctx))
                          (ast-mv-call-args node)))
        (result-reg (make-register ctx)))
    (declare (ignore func-reg arg-regs))
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-prog1) ctx)
  ;; Multiple-value-prog1 evaluates first form, saves all its values,
  ;; evaluates remaining forms, then returns the saved values.
  (let ((first-reg (compile-ast (ast-mv-prog1-first node) ctx))
        (result-reg (make-register ctx)))
    (emit ctx (make-instance 'vm-move :dst result-reg :src first-reg))
    (dolist (form (ast-mv-prog1-forms node))
      (compile-ast form ctx))
    result-reg))

;;; ----------------------------------------------------------------------------
;;; Functions and Closures
;;; ----------------------------------------------------------------------------

(defun find-mutated-variables (ast)
  "Find all variable names that are targets of SETQ in AST."
  (typecase ast
    (ast-setq (union (list (ast-setq-var ast))
                     (find-mutated-variables (ast-setq-value ast))))
    (ast-if (union (find-mutated-variables (ast-if-cond ast))
                   (union (find-mutated-variables (ast-if-then ast))
                          (find-mutated-variables (ast-if-else ast)))))
    (ast-progn (reduce #'union (mapcar #'find-mutated-variables (ast-progn-forms ast))
                        :initial-value nil))
    (ast-let (reduce #'union
                     (append (mapcar (lambda (b) (find-mutated-variables (cdr b)))
                                     (ast-let-bindings ast))
                             (mapcar #'find-mutated-variables (ast-let-body ast)))
                     :initial-value nil))
    (ast-lambda (reduce #'union (mapcar #'find-mutated-variables (ast-lambda-body ast))
                         :initial-value nil))
    (ast-defun (reduce #'union (mapcar #'find-mutated-variables (ast-defun-body ast))
                        :initial-value nil))
    (ast-call (reduce #'union
                      (cons (if (typep (ast-call-func ast) 'ast-node)
                                (find-mutated-variables (ast-call-func ast))
                                nil)
                            (mapcar #'find-mutated-variables (ast-call-args ast)))
                      :initial-value nil))
    (ast-print (find-mutated-variables (ast-print-expr ast)))
    (ast-binop (union (find-mutated-variables (ast-binop-lhs ast))
                      (find-mutated-variables (ast-binop-rhs ast))))
    (t nil)))

(defun find-captured-in-children (body-forms params)
  "Find variables that inner lambdas/defuns in BODY-FORMS capture as free variables.
PARAMS are the current scope's bound variables — only those are candidates for boxing."
  (let ((captured nil))
    (dolist (form body-forms captured)
      (typecase form
        (ast-lambda
         (let ((free (find-free-variables form)))
           (setf captured (union captured (intersection free params)))))
        (ast-defun
         (let* ((temp (make-instance 'ast-lambda
                                     :params (ast-defun-params form)
                                     :body (ast-defun-body form)))
                (free (find-free-variables temp)))
           (setf captured (union captured (intersection free params)))))
        ;; Recurse into compound forms to find nested lambdas
        (ast-if
         (setf captured (union captured
                               (find-captured-in-children
                                (list (ast-if-cond form) (ast-if-then form) (ast-if-else form))
                                params))))
        (ast-progn
         (setf captured (union captured
                               (find-captured-in-children (ast-progn-forms form) params))))
        (ast-let
         (setf captured (union captured
                               (find-captured-in-children (ast-let-body form) params)))
         (dolist (b (ast-let-bindings form))
           (setf captured (union captured
                                 (find-captured-in-children (list (cdr b)) params)))))
        (ast-call
         (setf captured (union captured
                               (find-captured-in-children (ast-call-args form) params)))
         (when (typep (ast-call-func form) 'ast-node)
           (setf captured (union captured
                                 (find-captured-in-children (list (ast-call-func form)) params)))))
        (ast-print
         (setf captured (union captured
                               (find-captured-in-children (list (ast-print-expr form)) params))))
        (ast-binop
         (setf captured (union captured
                               (find-captured-in-children
                                (list (ast-binop-lhs form) (ast-binop-rhs form))
                                params))))
        (ast-setq
         (setf captured (union captured
                               (find-captured-in-children
                                (list (ast-setq-value form)) params))))))))

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
    (ast-progn (reduce #'union (mapcar #'find-free-variables (ast-progn-forms ast))
                        :initial-value nil))
    (ast-print (find-free-variables (ast-print-expr ast)))
    (ast-let (let ((binding-vars (mapcar #'car (ast-let-bindings ast)))
                 (binding-exprs (mapcar #'find-free-variables
                                        (mapcar #'cdr (ast-let-bindings ast))))
                 (body-vars (reduce #'union
                                    (mapcar #'find-free-variables (ast-let-body ast))
                                    :initial-value nil)))
              ;; Remove bound variables from free set
              (set-difference (reduce #'union (append binding-exprs (list body-vars))
                                      :initial-value nil)
                              binding-vars)))
    (ast-lambda
     (let* ((params (ast-lambda-params ast))
            (all-params (append params
                                (mapcar #'first (ast-lambda-optional-params ast))
                                (when (ast-lambda-rest-param ast)
                                  (list (ast-lambda-rest-param ast)))
                                (mapcar #'first (ast-lambda-key-params ast))))
            (body-vars (reduce #'union
                               (mapcar #'find-free-variables (ast-lambda-body ast))
                               :initial-value nil))
            ;; Also find free vars in optional/key default forms
            (default-vars (reduce #'union
                                  (append
                                   (mapcar (lambda (opt)
                                             (if (second opt)
                                                 (find-free-variables (second opt))
                                                 nil))
                                           (ast-lambda-optional-params ast))
                                   (mapcar (lambda (kp)
                                             (if (second kp)
                                                 (find-free-variables (second kp))
                                                 nil))
                                           (ast-lambda-key-params ast)))
                                  :initial-value nil)))
       (set-difference (union body-vars default-vars) all-params)))
    (ast-setq (union (list (ast-setq-var ast))
                     (find-free-variables (ast-setq-value ast))))
    (ast-defun
     (let* ((params (ast-defun-params ast))
            (body-vars (reduce #'union
                               (mapcar #'find-free-variables (ast-defun-body ast))
                               :initial-value nil)))
       (set-difference body-vars params)))
    (ast-call
     (union (if (typep (ast-call-func ast) 'ast-node)
               (find-free-variables (ast-call-func ast))
               nil)
           (reduce #'union
                   (mapcar #'find-free-variables (ast-call-args ast))
                   :initial-value nil)))
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
      (emit ctx (make-instance 'vm-closure
                              :dst closure-reg
                              :label func-label
                              :params param-regs
                              :optional-params opt-closure-data
                              :rest-param rest-reg
                              :key-params key-closure-data
                              :captured captured-vars))
      ;; Jump over the function body
      (emit ctx (make-instance 'vm-jump :label end-label))
      ;; Emit the function entry label
      (emit ctx (make-instance 'vm-label :name func-label))
      ;; Save old environment and extend with parameters
      (let ((old-env (ctx-env ctx)))
        (unwind-protect
             (progn
               (setf (ctx-env ctx)
                     (append (build-all-param-bindings params param-regs
                                                      opt-bindings rest-binding key-bindings)
                             (ctx-env ctx)))
               ;; Emit inline default computation for non-constant defaults
               (when non-constant-defaults
                 (emit-non-constant-defaults ctx non-constant-defaults))
               ;; Compile body forms
               (let ((last-reg nil))
                 (dolist (form body)
                   (setf last-reg (compile-ast form ctx)))
                 (emit ctx (make-instance 'vm-ret :reg last-reg))))
          ;; Restore environment
          (setf (ctx-env ctx) old-env)))
      ;; Emit end label
      (emit ctx (make-instance 'vm-label :name end-label))
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
                 `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                    (let ((arg1-reg (compile-ast (first args) ctx))
                          (arg2-reg (compile-ast (second args) ctx)))
                      (emit ctx (make-instance ',vm-class
                                               :dst result-reg
                                               :str1 arg1-reg
                                               :str2 arg2-reg))
                      (return-from compile-ast result-reg))))
               (compile-unary-builtin (cl-name vm-class)
                 `(when (builtin-name-p func-sym ,(symbol-name cl-name))
                    (let ((arg-reg (compile-ast (first args) ctx)))
                      (emit ctx (make-instance ',vm-class
                                               :dst result-reg
                                               :src arg-reg))
                      (return-from compile-ast result-reg)))))
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
          (emit ctx (make-instance 'vm-nth
                                   :dst result-reg
                                   :index idx-reg
                                   :list list-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "NTHCDR")
        (let ((idx-reg (compile-ast (first args) ctx))
              (list-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-nthcdr
                                   :dst result-reg
                                   :index idx-reg
                                   :list list-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MEMBER")
        (let ((item-reg (compile-ast (first args) ctx))
              (list-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-member
                                   :dst result-reg
                                   :item item-reg
                                   :list list-reg))
          (return-from compile-ast result-reg)))
      ;; Arithmetic predicates
      (when (builtin-name-p func-sym "ZEROP")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-num-eq
                                   :dst result-reg
                                   :lhs arg-reg
                                   :rhs (let ((zero-reg (make-register ctx)))
                                          (emit ctx (make-instance 'vm-const :dst zero-reg :value 0))
                                          zero-reg)))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "PLUSP")
        (let ((arg-reg (compile-ast (first args) ctx))
              (zero-reg (make-register ctx)))
          (emit ctx (make-instance 'vm-const :dst zero-reg :value 0))
          (emit ctx (make-instance 'vm-gt
                                   :dst result-reg
                                   :lhs arg-reg
                                   :rhs zero-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MINUSP")
        (let ((arg-reg (compile-ast (first args) ctx))
              (zero-reg (make-register ctx)))
          (emit ctx (make-instance 'vm-const :dst zero-reg :value 0))
          (emit ctx (make-instance 'vm-lt
                                   :dst result-reg
                                   :lhs arg-reg
                                   :rhs zero-reg))
          (return-from compile-ast result-reg)))
      ;; Extended arithmetic builtins
      (when (and (builtin-name-p func-sym "MOD")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-mod
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "REM")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-rem
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "TRUNCATE")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-truncate
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "FLOOR")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-floor-inst
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "CEILING")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-ceiling-inst
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "MIN")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-min
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "MAX")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-max
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin abs vm-abs)
      (compile-unary-builtin evenp vm-evenp)
      (compile-unary-builtin oddp vm-oddp)
      ;; Cons (2 args, :car-src/:cdr-src -> :dst)
      (when (builtin-name-p func-sym "CONS")
        (let ((car-reg (compile-ast (first args) ctx))
              (cdr-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-cons
                                   :dst result-reg
                                   :car-src car-reg
                                   :cdr-src cdr-reg))
          (return-from compile-ast result-reg)))
      ;; Rplaca/Rplacd (2 args, :cons/:val -> modifies cons in place)
      (when (builtin-name-p func-sym "RPLACA")
        (let ((cons-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-rplaca
                                   :cons cons-reg
                                   :val val-reg))
          (emit ctx (make-instance 'vm-move :dst result-reg :src cons-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "RPLACD")
        (let ((cons-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-rplacd
                                   :cons cons-reg
                                   :val val-reg))
          (emit ctx (make-instance 'vm-move :dst result-reg :src cons-reg))
          (return-from compile-ast result-reg)))
      ;; Equality builtins (2 args, :lhs/:rhs -> :dst)
      (when (builtin-name-p func-sym "EQ")
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-eq
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "EQL")
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-eq
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      ;; Append (2 args, :src1/:src2 -> :dst)
      (when (builtin-name-p func-sym "APPEND")
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-append
                                   :dst result-reg
                                   :src1 lhs-reg
                                   :src2 rhs-reg))
          (return-from compile-ast result-reg)))
      ;; List (variable args)
      (when (builtin-name-p func-sym "LIST")
        (let ((arg-regs (mapcar (lambda (arg) (compile-ast arg ctx)) args)))
          (emit ctx (make-instance 'vm-list
                                   :dst result-reg
                                   :count (length arg-regs)
                                   :src-regs arg-regs))
          (return-from compile-ast result-reg)))
      ;; %values-to-list: capture current values-list as a list
      (when (builtin-name-p func-sym "%VALUES-TO-LIST")
        (emit ctx (make-instance 'vm-values-to-list :dst result-reg))
        (return-from compile-ast result-reg))
      ;; error: signal an error via vm-signal-error
      (when (builtin-name-p func-sym "ERROR")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-signal-error :error-reg arg-reg))
          ;; Return a dummy register (won't be reached if error is not caught)
          (return-from compile-ast result-reg)))
      ;; warn: print warning via vm-warn
      (when (builtin-name-p func-sym "WARN")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-warn :condition-reg arg-reg))
          (emit ctx (make-instance 'vm-const :dst result-reg :value nil))
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
                      (emit ctx (make-instance 'vm-const :dst test-reg :value test-sym))))))))
          (emit ctx (make-instance 'vm-make-hash-table
                                   :dst result-reg
                                   :test test-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "GETHASH")
        (let ((key-reg (compile-ast (first args) ctx))
              (table-reg (compile-ast (second args) ctx))
              (default-reg (when (third args) (compile-ast (third args) ctx))))
          (emit ctx (make-instance 'vm-gethash
                                   :dst result-reg
                                   :found-dst nil
                                   :key key-reg
                                   :table table-reg
                                   :default default-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "REMHASH")
        (let ((key-reg (compile-ast (first args) ctx))
              (table-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-remhash
                                   :key key-reg
                                   :table table-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "HASH-TABLE-COUNT")
        (let ((table-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-hash-table-count
                                   :dst result-reg
                                   :table table-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin hash-table-p vm-hash-table-p)
      (when (builtin-name-p func-sym "HASH-TABLE-KEYS")
        (let ((table-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-hash-table-keys
                                   :dst result-reg
                                   :table table-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "HASH-TABLE-VALUES")
        (let ((table-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-hash-table-values
                                   :dst result-reg
                                   :table table-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "HASH-TABLE-TEST")
        (let ((table-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-hash-table-test
                                   :dst result-reg
                                   :table table-reg))
          (return-from compile-ast result-reg)))
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
          (emit ctx (make-instance 'vm-hash-table-keys :dst keys-reg :table table-reg))
          ;; Loop over keys
          (emit ctx (make-instance 'vm-label :name loop-start))
          ;; End test: jump to end when keys-reg is nil (falsy)
          (emit ctx (make-instance 'vm-jump-zero :reg keys-reg :label loop-end))
          ;; key = (car keys)
          (emit ctx (make-instance 'vm-car :dst key-reg :src keys-reg))
          ;; val = (gethash key table)
          (emit ctx (make-instance 'vm-gethash :dst val-reg :key key-reg :table table-reg))
          ;; Call fn(key, val)
          (let ((call-dst (make-register ctx)))
            (emit ctx (make-instance 'vm-call :dst call-dst :func fn-reg :args (list key-reg val-reg))))
          ;; keys = (cdr keys)
          (emit ctx (make-instance 'vm-cdr :dst keys-reg :src keys-reg))
          (emit ctx (make-instance 'vm-jump :label loop-start))
          (emit ctx (make-instance 'vm-label :name loop-end))
          ;; maphash returns nil
          (emit ctx (make-instance 'vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; make-list
      (when (builtin-name-p func-sym "MAKE-LIST")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-make-list :dst result-reg :size size-reg))
          (return-from compile-ast result-reg)))
      ;; Array/vector builtins
      (when (builtin-name-p func-sym "MAKE-ARRAY")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-make-array
                                   :dst result-reg
                                   :size size-reg
                                   :fill-pointer nil
                                   :adjustable nil))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MAKE-ADJUSTABLE-VECTOR")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-make-array
                                   :dst result-reg
                                   :size size-reg
                                   :fill-pointer t
                                   :adjustable t))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "AREF")
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-aref
                                   :dst result-reg
                                   :array arr-reg
                                   :index idx-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "ASET")
        (let ((arr-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx))
              (val-reg (compile-ast (third args) ctx)))
          (emit ctx (make-instance 'vm-aset
                                   :array arr-reg
                                   :index idx-reg
                                   :val val-reg))
          (emit ctx (make-instance 'vm-move :dst result-reg :src val-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "VECTOR-PUSH-EXTEND")
        (let ((val-reg (compile-ast (first args) ctx))
              (arr-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-vector-push-extend
                                   :dst result-reg
                                   :val val-reg
                                   :array arr-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin vectorp vm-vectorp)
      (compile-unary-builtin array-length vm-array-length)
      ;; Symbol manipulation builtins
      (compile-unary-builtin symbol-name vm-symbol-name)
      (compile-unary-builtin make-symbol vm-make-symbol)
      (when (builtin-name-p func-sym "INTERN")
        (let ((str-reg (compile-ast (first args) ctx))
              (pkg-reg (when (second args) (compile-ast (second args) ctx))))
          (emit ctx (make-instance 'vm-intern-symbol :dst result-reg
                                                     :src str-reg
                                                     :pkg pkg-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin keywordp vm-keywordp)
      (when (builtin-name-p func-sym "GENSYM")
        (emit ctx (make-instance 'vm-gensym-inst :dst result-reg))
        (return-from compile-ast result-reg))
      ;; Association list and utility builtins
      (when (and (builtin-name-p func-sym "ASSOC")
                 (= (length args) 2))
        (let ((key-reg (compile-ast (first args) ctx))
              (alist-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-assoc
                                   :dst result-reg
                                   :key key-reg
                                   :alist alist-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "ACONS")
                 (= (length args) 3))
        (let ((key-reg (compile-ast (first args) ctx))
              (val-reg (compile-ast (second args) ctx))
              (alist-reg (compile-ast (third args) ctx)))
          (emit ctx (make-instance 'vm-acons
                                   :dst result-reg
                                   :key key-reg
                                   :value val-reg
                                   :alist alist-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "EQUAL")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-equal
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "NCONC")
                 (= (length args) 2))
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-nconc
                                   :dst result-reg
                                   :lhs lhs-reg
                                   :rhs rhs-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin copy-list vm-copy-list)
      (compile-unary-builtin copy-tree vm-copy-tree)
      (when (and (builtin-name-p func-sym "SUBST")
                 (= (length args) 3))
        (let ((new-reg (compile-ast (first args) ctx))
              (old-reg (compile-ast (second args) ctx))
              (tree-reg (compile-ast (third args) ctx)))
          (emit ctx (make-instance 'vm-subst
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
      (when (and (builtin-name-p func-sym "STRING")
                 (= (length args) 1))
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-string-coerce
                                   :dst result-reg
                                   :src arg-reg))
          (return-from compile-ast result-reg)))
      ;; Character/string builtins
      (when (and (builtin-name-p func-sym "CHAR")
                 (= (length args) 2))
        (let ((str-reg (compile-ast (first args) ctx))
              (idx-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-char
                                   :dst result-reg
                                   :string str-reg
                                   :index idx-reg))
          (return-from compile-ast result-reg)))
      (compile-unary-builtin char-code vm-char-code)
      (compile-unary-builtin code-char vm-code-char)
      (when (and (builtin-name-p func-sym "CHAR=")
                 (= (length args) 2))
        (let ((c1-reg (compile-ast (first args) ctx))
              (c2-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-char=
                                   :dst result-reg
                                   :char1 c1-reg
                                   :char2 c2-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "CHAR<")
                 (= (length args) 2))
        (let ((c1-reg (compile-ast (first args) ctx))
              (c2-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-char<
                                   :dst result-reg
                                   :char1 c1-reg
                                   :char2 c2-reg))
          (return-from compile-ast result-reg)))
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
      ;; subseq: (subseq string start &optional end)
      (when (and (builtin-name-p func-sym "SUBSEQ")
                 (>= (length args) 2))
        (let ((str-reg (compile-ast (first args) ctx))
              (start-reg (compile-ast (second args) ctx))
              (end-reg (if (third args)
                           (compile-ast (third args) ctx)
                           (let ((nil-reg (make-register ctx)))
                             (emit ctx (make-instance 'vm-const :dst nil-reg :value nil))
                             nil-reg))))
          (emit ctx (make-instance 'vm-subseq
                                   :dst result-reg
                                   :string str-reg
                                   :start start-reg
                                   :end end-reg))
          (return-from compile-ast result-reg)))
      ;; string-trim: (string-trim char-bag string)
      (when (and (builtin-name-p func-sym "STRING-TRIM")
                 (= (length args) 2))
        (let ((bag-reg (compile-ast (first args) ctx))
              (str-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-string-trim
                                   :dst result-reg
                                   :char-bag bag-reg
                                   :string str-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "STRING-LEFT-TRIM")
                 (= (length args) 2))
        (let ((bag-reg (compile-ast (first args) ctx))
              (str-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-string-left-trim
                                   :dst result-reg
                                   :char-bag bag-reg
                                   :string str-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "STRING-RIGHT-TRIM")
                 (= (length args) 2))
        (let ((bag-reg (compile-ast (first args) ctx))
              (str-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-string-right-trim
                                   :dst result-reg
                                   :char-bag bag-reg
                                   :string str-reg))
          (return-from compile-ast result-reg)))
      ;; search: (search pattern string)
      (when (and (builtin-name-p func-sym "SEARCH")
                 (>= (length args) 2))
        (let ((pat-reg (compile-ast (first args) ctx))
              (str-reg (compile-ast (second args) ctx))
              (start-reg (let ((zero-reg (make-register ctx)))
                           (emit ctx (make-instance 'vm-const :dst zero-reg :value 0))
                           zero-reg)))
          (emit ctx (make-instance 'vm-search-string
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
          (emit ctx (make-instance 'vm-typep
                                   :dst result-reg
                                   :src val-reg
                                   :type-name type-sym))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "TYPE-OF")
        (let ((src-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-type-of :dst result-reg :src src-reg))
          (return-from compile-ast result-reg)))
      ;; eval — meta-circular runtime evaluation
      (when (builtin-name-p func-sym "EVAL")
        (let ((form-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-eval :dst result-reg :src form-reg))
          (return-from compile-ast result-reg)))
      ;; I/O builtins (simple print/format, work with any vm-state)
      (when (builtin-name-p func-sym "PRINC")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-princ :src arg-reg))
          (emit ctx (make-instance 'vm-move :dst result-reg :src arg-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "PRIN1")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-prin1 :src arg-reg))
          (emit ctx (make-instance 'vm-move :dst result-reg :src arg-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "PRINT")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-print-inst :src arg-reg))
          (emit ctx (make-instance 'vm-move :dst result-reg :src arg-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "TERPRI")
        (emit ctx (make-instance 'vm-terpri-inst))
        (emit ctx (make-instance 'vm-const :dst result-reg :value nil))
        (return-from compile-ast result-reg))
      (when (builtin-name-p func-sym "FRESH-LINE")
        (emit ctx (make-instance 'vm-fresh-line-inst))
        (emit ctx (make-instance 'vm-const :dst result-reg :value nil))
        (return-from compile-ast result-reg))
      (when (builtin-name-p func-sym "WRITE-TO-STRING")
        (let ((arg-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-write-to-string-inst
                                   :dst result-reg
                                   :src arg-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "PRIN1-TO-STRING")
        (let ((src-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-write-to-string-inst :dst result-reg :src src-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "PRINC-TO-STRING")
        (let ((src-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-write-to-string-inst :dst result-reg :src src-reg))
          (return-from compile-ast result-reg)))
      ;; String output stream builtins
      (when (builtin-name-p func-sym "MAKE-STRING-OUTPUT-STREAM")
        (emit ctx (make-instance 'vm-make-string-output-stream-inst
                                 :dst result-reg))
        (return-from compile-ast result-reg))
      (when (builtin-name-p func-sym "GET-OUTPUT-STREAM-STRING")
        (let ((stream-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-get-output-stream-string-inst
                                   :dst result-reg
                                   :src stream-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "WRITE-STRING")
        (let ((str-reg (compile-ast (first args) ctx)))
          (if (and (>= (length args) 2))
              ;; (write-string str stream)
              (let ((stream-reg (compile-ast (second args) ctx)))
                (emit ctx (make-instance 'vm-stream-write-string-inst
                                         :stream stream-reg
                                         :src str-reg))
                (emit ctx (make-instance 'vm-move :dst result-reg :src str-reg)))
              ;; (write-string str) — to stdout
              (emit ctx (make-instance 'vm-princ :src str-reg)))
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
             (emit ctx (make-instance 'vm-format-inst
                                      :dst result-reg
                                      :fmt fmt-reg
                                      :arg-regs format-arg-regs))
             (return-from compile-ast result-reg))
            (dest-is-t
             (let ((str-reg (make-register ctx)))
               (emit ctx (make-instance 'vm-format-inst
                                        :dst str-reg
                                        :fmt fmt-reg
                                        :arg-regs format-arg-regs))
               (emit ctx (make-instance 'vm-princ :src str-reg))
               (emit ctx (make-instance 'vm-const :dst result-reg :value nil))
               (return-from compile-ast result-reg)))
            (t
             ;; Stream destination: format to string, write to stream
             (let ((str-reg (make-register ctx))
                   (stream-reg (compile-ast dest-arg ctx)))
               (emit ctx (make-instance 'vm-format-inst
                                        :dst str-reg
                                        :fmt fmt-reg
                                        :arg-regs format-arg-regs))
               (emit ctx (make-instance 'vm-stream-write-string-inst
                                        :stream stream-reg
                                        :src str-reg))
               (emit ctx (make-instance 'vm-const :dst result-reg :value nil))
               (return-from compile-ast result-reg))))))
      ;; ----------------------------------------------------------------
      ;; File I/O builtins
      ;; ----------------------------------------------------------------
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
          (emit ctx (make-instance 'vm-open-file
                                   :dst result-reg
                                   :path path-reg
                                   :direction direction))
          (return-from compile-ast result-reg)))
      ;; close: (close handle)
      (when (builtin-name-p func-sym "CLOSE")
        (let ((handle-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-close-file :handle handle-reg))
          (emit ctx (make-instance 'vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; read-char: (read-char handle)
      (when (builtin-name-p func-sym "READ-CHAR")
        (let ((handle-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-read-char
                                   :dst result-reg
                                   :handle handle-reg))
          (return-from compile-ast result-reg)))
      ;; read-line: (read-line handle)
      (when (builtin-name-p func-sym "READ-LINE")
        (let ((handle-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-read-line
                                   :dst result-reg
                                   :handle handle-reg))
          (return-from compile-ast result-reg)))
      ;; write-char: (write-char char &optional stream)
      (when (builtin-name-p func-sym "WRITE-CHAR")
        (let ((char-reg (compile-ast (first args) ctx)))
          (if (>= (length args) 2)
              ;; (write-char char stream)
              (let ((handle-reg (compile-ast (second args) ctx)))
                (emit ctx (make-instance 'vm-write-char
                                         :handle handle-reg
                                         :char char-reg)))
              ;; (write-char char) — to stdout (handle 1)
              (let ((handle-reg (make-register ctx)))
                (emit ctx (make-instance 'vm-const :dst handle-reg :value 1))
                (emit ctx (make-instance 'vm-write-char
                                         :handle handle-reg
                                         :char char-reg))))
          (emit ctx (make-instance 'vm-move :dst result-reg :src char-reg))
          (return-from compile-ast result-reg)))
      ;; peek-char: (peek-char nil handle)
      (when (builtin-name-p func-sym "PEEK-CHAR")
        (let ((handle-reg (if (>= (length args) 2)
                              (compile-ast (second args) ctx)
                              (compile-ast (first args) ctx))))
          (emit ctx (make-instance 'vm-peek-char
                                   :dst result-reg
                                   :handle handle-reg))
          (return-from compile-ast result-reg)))
      ;; unread-char: (unread-char char handle)
      (when (builtin-name-p func-sym "UNREAD-CHAR")
        (let ((char-reg (compile-ast (first args) ctx))
              (handle-reg (compile-ast (second args) ctx)))
          (emit ctx (make-instance 'vm-unread-char
                                   :handle handle-reg
                                   :char char-reg))
          (emit ctx (make-instance 'vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; file-position: (file-position handle)
      (when (builtin-name-p func-sym "FILE-POSITION")
        (let ((handle-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-file-position
                                   :dst result-reg
                                   :handle handle-reg))
          (return-from compile-ast result-reg)))
      ;; file-length: (file-length handle)
      (when (builtin-name-p func-sym "FILE-LENGTH")
        (let ((handle-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-file-length
                                   :dst result-reg
                                   :handle handle-reg))
          (return-from compile-ast result-reg)))
      ;; make-string-input-stream: (make-string-input-stream string)
      (when (builtin-name-p func-sym "MAKE-STRING-INPUT-STREAM")
        (let ((str-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-make-string-stream
                                   :dst result-reg
                                   :direction :input
                                   :initial-string str-reg))
          (return-from compile-ast result-reg)))
      ;; read-from-string: (read-from-string string)
      (when (builtin-name-p func-sym "READ-FROM-STRING")
        (let ((str-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-read-from-string-inst
                                   :dst result-reg
                                   :src str-reg))
          (return-from compile-ast result-reg)))
      ;; read: (read stream)
      (when (builtin-name-p func-sym "READ")
        (let ((stream-reg (compile-ast (first args) ctx)))
          (emit ctx (make-instance 'vm-read-sexp-inst
                                   :dst result-reg
                                   :src stream-reg))
          (return-from compile-ast result-reg)))
      ;; concatenate: only handle (concatenate 'string a b)
      (when (and (builtin-name-p func-sym "CONCATENATE")
                 (>= (length args) 3)
                 (typep (first args) 'ast-quote)
                 (string= (symbol-name (ast-quote-value (first args))) "STRING"))
        (let ((str1-reg (compile-ast (second args) ctx))
              (str2-reg (compile-ast (third args) ctx)))
          (emit ctx (make-instance 'vm-concatenate
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
                                        (emit ctx (make-instance 'vm-const :dst sym-reg :value func-expr))
                                        sym-reg))))
                               ((typep func-expr 'ast-var)
                                (let* ((name (ast-var-name func-expr))
                                       (entry (assoc name (ctx-env ctx))))
                                  (if entry
                                      (cdr entry)
                                      ;; Not locally bound: emit symbol for runtime resolution
                                      (let ((sym-reg (make-register ctx)))
                                        (emit ctx (make-instance 'vm-const :dst sym-reg :value name))
                                        sym-reg))))
                               (t (compile-ast func-expr ctx))))
           ;; Unbox labels functions: if this is a boxed labels function,
           ;; emit vm-car to extract the actual closure from the cons cell box
           (func-reg (if (and func-sym (assoc func-sym *labels-boxed-fns*))
                         (let ((unboxed (make-register ctx)))
                           (emit ctx (make-instance 'vm-car :dst unboxed :src raw-func-reg))
                           unboxed)
                         raw-func-reg)))
      ;; Compile arguments
      (let ((arg-regs (mapcar (lambda (arg) (compile-ast arg ctx)) args)))
        ;; Check if this is a generic function call
        (if (and func-sym
                 (gethash func-sym (ctx-global-generics ctx)))
            ;; Generic function dispatch
            (emit ctx (make-instance 'vm-generic-call
                                    :dst result-reg
                                    :gf-reg func-reg
                                    :args arg-regs))
            ;; Regular function call
            (emit ctx (make-instance 'vm-call
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
                 (emit ctx (make-instance 'vm-car :dst dst :src (cdr entry)))
                 (emit ctx (make-instance 'vm-move :dst dst :src (cdr entry))))
             ;; It's a global/top-level function - create function reference
             (emit ctx (make-instance 'vm-func-ref
                                     :dst dst
                                     :label (format nil "~A" name))))))
      ;; (setf name) form
      ((and (consp name) (eq (car name) 'setf))
       (emit ctx (make-instance 'vm-func-ref
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
                         (let* ((body-ast (make-instance 'ast-progn :forms body-forms))
                                (free-vars (find-free-variables body-ast))
                                (captured-vars (remove-if-not
                                                (lambda (v) (assoc v old-env))
                                                (set-difference free-vars params))))
                           ;; Create unique parameter registers
                           (let ((param-regs (loop for i from 0 below (length params)
                                                   collect (make-register ctx)))
                                 (skip-label (make-label ctx "flet_skip")))
                             ;; Create closure
                             (emit ctx (make-instance 'vm-closure
                                                     :dst closure-reg
                                                     :label func-label
                                                     :params param-regs
                                                     :captured captured-vars))
                             ;; Remember binding for body compilation
                             (push (cons name closure-reg) func-bindings)
                             ;; Jump over this function's body only
                             (emit ctx (make-instance 'vm-jump :label skip-label))
                             ;; Emit function label
                             (emit ctx (make-instance 'vm-label :name func-label))
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
                                 (emit ctx (make-instance 'vm-ret :reg last-reg))))
                             ;; Skip label: jump lands here
                             (emit ctx (make-instance 'vm-label :name skip-label))))))
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
      (emit ctx (make-instance 'vm-label :name end-label))
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
                     (emit ctx (make-instance 'vm-const :dst nil-reg :value nil))
                     (dolist (binding bindings)
                       (let* ((name (first binding))
                              (params (second binding))
                              (func-label (make-label ctx "labels_fn"))
                              (closure-reg (make-register ctx))
                              (box-reg (make-register ctx)))
                         ;; Create a mutable box: (cons nil nil)
                         (emit ctx (make-instance 'vm-cons :dst box-reg
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
                                  (body-ast (make-instance 'ast-progn :forms body-forms))
                                  (free-vars (set-difference (find-free-variables body-ast) params))
                                  ;; Captured vars: capture BOX registers for siblings
                                  (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                                         (remove-if-not (lambda (v) (assoc v (ctx-env ctx)))
                                                                        free-vars)))
                                  (param-regs (loop for i from 0 below (length params)
                                                    collect (make-register ctx)))
                                  (skip-label (make-label ctx "labels_skip")))
                             ;; Create closure
                             (emit ctx (make-instance 'vm-closure
                                                      :dst closure-reg
                                                      :label func-label
                                                      :params param-regs
                                                      :captured captured-vars))
                             ;; Fill box with closure via rplaca
                             (emit ctx (make-instance 'vm-rplaca
                                                      :cons box-reg :val closure-reg))
                             ;; Jump over function body
                             (emit ctx (make-instance 'vm-jump :label skip-label))
                             ;; Function label
                             (emit ctx (make-instance 'vm-label :name func-label))
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
                                 (emit ctx (make-instance 'vm-ret :reg last-reg))))
                             ;; Skip label
                             (emit ctx (make-instance 'vm-label :name skip-label)))))
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
      (emit ctx (make-instance 'vm-label :name end-label))
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
        (setf program (make-instance 'vm-program
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
  '(if let progn lambda quote setq setf
    defun defvar defparameter defmacro defclass defgeneric defmethod
    make-instance slot-value
    block return-from tagbody go
    flet labels function funcall
    the print
    catch throw unwind-protect
    handler-case declare eval-when defstruct
    macrolet symbol-macrolet
    in-package defpackage export
    multiple-value-call multiple-value-prog1
    values multiple-value-bind apply
    prog prog* with-slots nth-value)
  "Forms handled directly by the parser/compiler — not subject to macro expansion.")

(defun expand-defstruct (form)
  "Expand (defstruct name-or-options slot...) to progn of defclass + defuns.
Supports :conc-name, :constructor with boa-lambda-list, slot defaults."
  (let* ((name-and-options (second form))
         (slots-raw (cddr form))
         ;; Parse name and options
         (name (if (listp name-and-options) (first name-and-options) name-and-options))
         (options (when (listp name-and-options) (rest name-and-options)))
         ;; Extract :conc-name option (default is name-)
         (conc-name-opt (find :conc-name options :key (lambda (o) (when (listp o) (first o)))))
         (conc-name (if conc-name-opt
                        (second conc-name-opt)
                        (intern (concatenate 'string (symbol-name name) "-"))))
         ;; Extract :constructor option
         (constructor-opt (find :constructor options :key (lambda (o) (when (listp o) (first o)))))
         (constructor-name (if constructor-opt
                               (second constructor-opt)
                               (intern (concatenate 'string "MAKE-" (symbol-name name)))))
         (boa-args (when (and constructor-opt (cddr constructor-opt))
                     (third constructor-opt)))
         ;; Parse slots: each is either a symbol or (slot-name default :type type)
         (parsed-slots (mapcar (lambda (s)
                                 (if (listp s)
                                     (list (first s) (second s))  ; (name default)
                                     (list s nil)))               ; (name nil)
                               ;; Filter out docstrings
                               (remove-if #'stringp slots-raw)))
         ;; Generate the expansion
         (predicate-name (intern (concatenate 'string (symbol-name name) "-P")))
         (copier-name (intern (concatenate 'string "COPY-" (symbol-name name)))))
    ;; Build the expansion
    ;; Register accessor→slot mappings at macro-expansion time
    (dolist (slot parsed-slots)
      (let* ((slot-name (first slot))
             (accessor-name (if conc-name
                                (intern (concatenate 'string
                                                     (symbol-name conc-name)
                                                     (symbol-name slot-name)))
                                slot-name)))
        (setf (gethash accessor-name *accessor-slot-map*)
              (cons name slot-name))))
    ;; Return the expansion
    `(progn
       ;; Define the class
       (defclass ,name ()
         ,(mapcar (lambda (slot)
                    (let* ((slot-name (first slot))
                           (default (second slot))
                           (accessor-name (if conc-name
                                              (intern (concatenate 'string
                                                                   (symbol-name conc-name)
                                                                   (symbol-name slot-name)))
                                              slot-name))
                           (initarg (intern (symbol-name slot-name) "KEYWORD")))
                      `(,slot-name :initarg ,initarg
                                   :initform ,default
                                   :accessor ,accessor-name)))
                  parsed-slots))
       ;; Constructor
       ,(if boa-args
            ;; BOA constructor: positional args map to slots
            `(defun ,constructor-name ,boa-args
               (make-instance ',name
                              ,@(mapcan (lambda (arg)
                                          (list (intern (symbol-name arg) "KEYWORD") arg))
                                        boa-args)))
            ;; Default constructor: keyword args with defaults
            `(defun ,constructor-name (&key ,@(mapcar (lambda (slot)
                                                        (list (first slot) (second slot)))
                                                      parsed-slots))
               (make-instance ',name
                              ,@(mapcan (lambda (slot)
                                          (list (intern (symbol-name (first slot)) "KEYWORD")
                                                (first slot)))
                                        parsed-slots))))
       ;; Predicate
       (defun ,predicate-name (obj) (typep obj ',name))
       ;; Accessors are already generated by defclass via :accessor
       ;; Return the name
       ',name)))

(defun reduce-variadic-op (op args identity)
  "Reduce a variadic arithmetic form (OP arg...) to nested binary forms.
(OP) => IDENTITY, (OP a) => a, (OP a b) => (OP a b), (OP a b c ...) => (OP (OP a b) c) ..."
  (case (length args)
    (0 identity)
    (1 (first args))
    (2 (list op (first args) (second args)))
    (t (reduce (lambda (acc x) (list op acc x)) (cddr args)
               :initial-value (list op (first args) (second args))))))

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
    ;; (apply 'name list-form) with quoted symbol => expand based on function
    ((and (consp form) (eq (car form) 'apply)
          (= (length form) 3)
          (consp (second form))
          (eq (car (second form)) 'quote)
          (symbolp (second (second form))))
     (let ((fn-name (second (second form)))
           (args-form (third form)))
       (if (member fn-name '(+ - * append nconc list))
           ;; Variadic builtins: dolist fold (no reduce dependency)
           (let ((acc (gensym "ACC"))
                 (x (gensym "X"))
                 (lst (gensym "LST"))
                 (identity (case fn-name ((+) 0) ((*) 1) ((append nconc list) nil))))
             (if (eq fn-name '-)
                 (compiler-macroexpand-all
                  `(let ((,lst ,args-form))
                     (if (null (cdr ,lst))
                         (- 0 (car ,lst))
                         (let ((,acc (car ,lst)))
                           (dolist (,x (cdr ,lst) ,acc)
                             (setq ,acc (- ,acc ,x)))))))
                 (compiler-macroexpand-all
                  `(let ((,acc ,identity))
                     (dolist (,x ,args-form ,acc)
                       (setq ,acc (,fn-name ,acc ,x)))))))
           ;; Non-variadic: convert to #'name form, expand args only
           (list 'apply (list 'function fn-name)
                 (compiler-macroexpand-all args-form)))))
    ;; (apply #'name list-form) with function ref => expand variadic, pass through others
    ((and (consp form) (eq (car form) 'apply)
          (= (length form) 3)
          (consp (second form))
          (eq (car (second form)) 'function)
          (symbolp (second (second form))))
     (let ((fn-name (second (second form)))
           (args-form (third form)))
       (if (member fn-name '(+ - * append nconc list))
           ;; Variadic builtins: dolist fold (no reduce dependency)
           (let ((acc (gensym "ACC"))
                 (x (gensym "X"))
                 (lst (gensym "LST"))
                 (identity (case fn-name ((+) 0) ((*) 1) ((append nconc list) nil))))
             (if (eq fn-name '-)
                 (compiler-macroexpand-all
                  `(let ((,lst ,args-form))
                     (if (null (cdr ,lst))
                         (- 0 (car ,lst))
                         (let ((,acc (car ,lst)))
                           (dolist (,x (cdr ,lst) ,acc)
                             (setq ,acc (- ,acc ,x)))))))
                 (compiler-macroexpand-all
                  `(let ((,acc ,identity))
                     (dolist (,x ,args-form ,acc)
                       (setq ,acc (,fn-name ,acc ,x)))))))
           ;; Non-variadic: pass through, expand args only
           (list 'apply (list 'function fn-name)
                 (compiler-macroexpand-all args-form)))))
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
       (if (and (symbolp name)
                (member name '(car cdr cons list append length reverse
                               not null consp symbolp numberp integerp stringp
                               atom listp characterp functionp
                               + - * = < > <= >= mod rem
                               eq eql equal
                               first second third fourth fifth rest last
                               nreverse butlast endp nth nthcdr member
                               assoc acons nconc copy-list copy-tree subst
                               symbol-name make-symbol intern gensym keywordp
                               string-length string-upcase string-downcase
                               string= string< string> string-concat
                               char char-code code-char
                               typep hash-table-p hash-table-count
                               hash-table-test hash-table-keys hash-table-values
                               zerop plusp minusp evenp oddp abs
                               min max floor ceiling truncate
                               princ prin1 print write-to-string
                               prin1-to-string princ-to-string
                               type-of make-list alphanumericp
                               eval identity
                               caar cadr cdar cddr
                               caaar cdaar cadar cddar
                               caadr cdadr caddr cdddr
                               caaaar cadaar caadar caddar
                               cdaaar cddaar cdadar cdddar
                               caaadr cadadr caaddr cadddr
                               cdaadr cddadr cdaddr cddddr)))
           ;; Wrap builtins in lambda — variadic builtins get &rest + dolist fold
           (cond
             ;; Variadic builtins: wrap with &rest + dolist fold (no reduce dependency)
             ((member name '(+ * append nconc))
              (let ((args (gensym "ARGS"))
                    (acc (gensym "ACC"))
                    (x (gensym "X"))
                    (identity (case name ((+) 0) ((*) 1) ((append nconc) nil))))
                (compiler-macroexpand-all
                 `(lambda (&rest ,args)
                    (let ((,acc ,identity))
                      (dolist (,x ,args ,acc)
                        (setq ,acc (,name ,acc ,x))))))))
             ((eq name '-)
              (let ((args (gensym "ARGS"))
                    (acc (gensym "ACC"))
                    (x (gensym "X")))
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
             ;; Binary builtins
             ((member name '(cons = < > <= >= mod rem eq eql equal
                             nth nthcdr member assoc acons
                             string= string< string> string-concat
                             char min max floor ceiling truncate))
              (let ((a (gensym "A")) (b (gensym "B")))
                (compiler-macroexpand-all `(lambda (,a ,b) (,name ,a ,b)))))
             ;; Unary builtins
             (t
              (let ((x (gensym "X")))
                (compiler-macroexpand-all `(lambda (,x) (,name ,x))))))
           ;; Not a builtin — pass through for func-ref lookup
           form)))
    ;; Variadic builtins: (+ a b c) => (+ (+ a b) c), (append a b c) => (append (append a b) c)
    ((and (consp form) (member (car form) '(+ - * append nconc))
          (/= (length (cdr form)) 2))
     (let ((op (car form))
           (identity (case (car form) ((+ -) 0) (* 1) ((append nconc) nil))))
       (compiler-macroexpand-all
        (reduce-variadic-op op (cdr form) identity))))
    ;; car/cdr composition accessors: caar, cadr, cdar, cddr, caddr, cadddr, etc.
    ;; NOTE: first/second/third etc. are NOT included here because they shadow
    ;; valid parameter/variable names (e.g. &key (first 0) in defstruct constructors).
    ((and (consp form) (= (length form) 2)
          (member (car form) '(caar cadr cdar cddr
                               caaar cdaar cadar cddar
                               caadr cdadr caddr cdddr
                               caaaar cadaar caadar caddar
                               cdaaar cddaar cdadar cdddar
                               caaadr cadadr caaddr cadddr
                               cdaadr cddadr cdaddr cddddr)))
     (let ((ops (car form))
           (arg (second form)))
       (compiler-macroexpand-all
        (case ops
          (caar  `(car (car ,arg)))
          (cadr  `(car (cdr ,arg)))
          (cdar  `(cdr (car ,arg)))
          (cddr  `(cdr (cdr ,arg)))
          (caaar `(car (car (car ,arg))))
          (cdaar `(cdr (car (car ,arg))))
          (cadar `(car (cdr (car ,arg))))
          (cddar `(cdr (cdr (car ,arg))))
          (caadr `(car (car (cdr ,arg))))
          (cdadr `(cdr (car (cdr ,arg))))
          (caddr `(car (cdr (cdr ,arg))))
          (cdddr `(cdr (cdr (cdr ,arg))))
          (caaaar `(car (car (car (car ,arg)))))
          (cadaar `(car (cdr (car (car ,arg)))))
          (caadar `(car (car (cdr (car ,arg)))))
          (caddar `(car (cdr (cdr (car ,arg)))))
          (caaadr `(car (car (car (cdr ,arg)))))
          (cadadr `(car (cdr (car (cdr ,arg)))))
          (caaddr `(car (car (cdr (cdr ,arg)))))
          (cadddr `(car (cdr (cdr (cdr ,arg)))))
          (cdaaar `(cdr (car (car (car ,arg)))))
          (cddaar `(cdr (cdr (car (car ,arg)))))
          (cdadar `(cdr (car (cdr (car ,arg)))))
          (cdddar `(cdr (cdr (cdr (car ,arg)))))
          (cdaadr `(cdr (car (car (cdr ,arg)))))
          (cddadr `(cdr (cdr (car (cdr ,arg)))))
          (cdaddr `(cdr (car (cdr (cdr ,arg)))))
          (cddddr `(cdr (cdr (cdr (cdr ,arg)))))
          (t `(car ,arg))))))
    ;; (multiple-value-list expr) => evaluate expr then capture values-list
    ((and (consp form) (eq (car form) 'multiple-value-list)
          (= (length form) 2))
     (let ((tmp (gensym "MVL")))
       (compiler-macroexpand-all
        `(let ((,tmp ,(second form)))
           (declare (ignore ,tmp))
           (%values-to-list)))))
    ;; (deftype name type-spec) — register type alias
    ((and (consp form) (eq (car form) 'deftype)
          (= (length form) 3)
          (symbolp (second form)))
     (cl-cc/type:register-type-alias (second form) (third form))
     `(quote ,(second form)))
    ;; (declare ...) — silently ignore declarations
    ((and (consp form) (eq (car form) 'declare))
     nil)
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
    ;; (map result-type fn seq) => (coerce (mapcar fn (coerce seq 'list)) result-type)
    ((and (consp form) (eq (car form) 'map)
          (= (length form) 4))
     (let ((result-type (second form))
           (fn (third form))
           (seq (fourth form)))
       (compiler-macroexpand-all
        `(coerce (mapcar ,fn (coerce ,seq 'list)) ,result-type))))
    ;; (make-array size :fill-pointer fp :adjustable adj ...) => make-adjustable-vector
    ((and (consp form) (eq (car form) 'make-array)
          (>= (length form) 4))
     (let ((size (second form))
           (rest (cddr form))
           (fp nil) (adj nil))
       ;; Parse keyword args
       (loop for (key val) on rest by #'cddr
             do (case key
                  (:fill-pointer (setf fp val))
                  (:adjustable (setf adj val))))
       (if (or fp adj)
           (compiler-macroexpand-all `(make-adjustable-vector ,size))
           (compiler-macroexpand-all `(make-array ,size)))))
    ;; (setf (car/cdr/first/rest/nth ...) val) — expand to rplaca/rplacd
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (consp (second form))
          (member (car (second form)) '(car cdr first rest nth cadr cddr)))
     (let ((place (second form))
           (value (third form)))
       (compiler-macroexpand-all
        (case (car place)
          ((car first)
           (let ((v (gensym "V")))
             `(let ((,v ,value))
                (rplaca ,(second place) ,v)
                ,v)))
          ((cdr rest)
           (let ((v (gensym "V")))
             `(let ((,v ,value))
                (rplacd ,(second place) ,v)
                ,v)))
          (nth
           (let ((v (gensym "V")))
             `(let ((,v ,value))
                (rplaca (nthcdr ,(second place) ,(third place)) ,v)
                ,v)))
          (cadr
           (let ((v (gensym "V")))
             `(let ((,v ,value))
                (rplaca (cdr ,(second place)) ,v)
                ,v)))
          (cddr
           (let ((v (gensym "V")))
             `(let ((,v ,value))
                (rplacd (cdr ,(second place)) ,v)
                ,v)))))))
    ;; (setf (accessor obj) val) — expand via accessor-slot-map or to slot-value
    ((and (consp form) (eq (car form) 'setf)
          (= (length form) 3)
          (consp (second form))
          (symbolp (car (second form)))
          (= (length (second form)) 2))
     (let* ((place (second form))
            (accessor-name (car place))
            (obj-form (second place))
            (value (third form))
            (mapping (gethash accessor-name *accessor-slot-map*)))
       (if mapping
           ;; Known struct accessor: expand to (setf (slot-value obj 'slot) val)
           (compiler-macroexpand-all
            `(setf (slot-value ,obj-form ',(cdr mapping)) ,value))
           ;; Unknown accessor: try slot-value with accessor name as slot
           (compiler-macroexpand-all
            `(setf (slot-value ,obj-form ',accessor-name) ,value)))))
    ;; (coerce seq 'type) => (coerce-to-string/list/vector seq)
    ((and (consp form) (eq (car form) 'coerce)
          (= (length form) 3)
          (consp (third form))
          (eq (car (third form)) 'quote))
     (let ((value (second form))
           (type (second (third form))))
       (compiler-macroexpand-all
        (cond
          ((and (symbolp type)
                (member type '(string simple-string base-string)))
           `(coerce-to-string ,value))
          ((and (symbolp type) (eq type 'list))
           `(coerce-to-list ,value))
          ((and (symbolp type)
                (member type '(vector simple-vector)))
           `(coerce-to-vector ,value))
          ;; Compound types: (simple-array ...), (array ...), (vector ...)
          ((and (consp type)
                (member (car type) '(simple-array array)))
           `(coerce-to-vector ,value))
          ((and (consp type) (eq (car type) 'vector))
           `(coerce-to-vector ,value))
          (t `(coerce-to-string ,value))))))
    ;; (with-output-to-string (var) body...) => let + make-string-output-stream + body + get-output-stream-string
    ((and (consp form) (eq (car form) 'with-output-to-string)
          (consp (second form)))
     (let ((var (car (second form)))
           (body (cddr form))
           (result-var (gensym "RESULT")))
       (compiler-macroexpand-all
        `(let ((,var (make-string-output-stream)))
           ,@body
           (let ((,result-var (get-output-stream-string ,var)))
             ,result-var)))))
    ;; (with-open-file (var path :direction dir) body...)
    ;; => (let ((var (open path :direction dir)))
    ;;      (unwind-protect (progn body...)
    ;;        (close var)))
    ((and (consp form) (eq (car form) 'with-open-file)
          (consp (second form)))
     (let* ((stream-spec (second form))
            (var (first stream-spec))
            (path (second stream-spec))
            (options (cddr stream-spec))
            (body (cddr form)))
       (compiler-macroexpand-all
        `(let ((,var (open ,path ,@options)))
           (unwind-protect (progn ,@body)
             (close ,var))))))
    ;; (in-package name) — package system (currently a no-op, returns package name)
    ((and (consp form) (eq (car form) 'in-package))
     `(quote ,(second form)))
    ;; (defpackage name options...) — package definition (currently returns name)
    ((and (consp form) (eq (car form) 'defpackage))
     `(quote ,(second form)))
    ;; (export symbols &optional package) — export declaration (currently no-op)
    ((and (consp form) (eq (car form) 'export))
     nil)
    ;; (warn fmt args...) => (progn (format t "~&WARNING: " fmt args...) nil)
    ((and (consp form) (eq (car form) 'warn))
     (compiler-macroexpand-all
      `(progn (format t ,(concatenate 'string "~&WARNING: " (if (stringp (second form))
                                                                 (second form)
                                                                 "~A"))
                      ,@(cddr form))
              nil)))
    ;; (prog (bindings) body...) => (block nil (let (bindings) (tagbody body...)))
    ((and (consp form) (eq (car form) 'prog)
          (>= (length form) 2) (listp (second form)))
     (compiler-macroexpand-all
      `(block nil (let ,(second form) (tagbody ,@(cddr form))))))
    ;; (prog* (bindings) body...) => (block nil (let* (bindings) (tagbody body...)))
    ((and (consp form) (eq (car form) 'prog*)
          (>= (length form) 2) (listp (second form)))
     (compiler-macroexpand-all
      `(block nil (let* ,(second form) (tagbody ,@(cddr form))))))
    ;; (with-slots (slot...) instance body...) => let binding slot-value calls
    ((and (consp form) (eq (car form) 'with-slots)
          (>= (length form) 3) (listp (second form)))
     (let ((inst-var (gensym "INST")))
       (compiler-macroexpand-all
        `(let ((,inst-var ,(third form)))
           (let ,(mapcar (lambda (slot)
                           (if (listp slot)
                               `(,(first slot) (slot-value ,inst-var ',(second slot)))
                               `(,slot (slot-value ,inst-var ',slot))))
                         (second form))
             ,@(cdddr form))))))
    ;; (nth-value n form) => (nth n (multiple-value-list form))
    ((and (consp form) (eq (car form) 'nth-value)
          (= (length form) 3))
     (compiler-macroexpand-all
      `(nth ,(second form) (multiple-value-list ,(third form)))))
    ;; (defstruct ...) — expand to defclass + constructor + predicate
    ((and (consp form) (eq (car form) 'defstruct))
     (compiler-macroexpand-all (expand-defstruct form)))
    ;; (eval-when (situations...) body...) — phase control
    ((and (consp form) (eq (car form) 'eval-when))
     (let ((situations (second form))
           (body (cddr form)))
       ;; At compile time: if :compile-toplevel, eval body now
       (when (member :compile-toplevel situations)
         (dolist (b body)
           (our-eval (compiler-macroexpand-all b))))
       ;; For output: if :execute or :load-toplevel, include the body
       (if (or (member :execute situations)
               (member :load-toplevel situations))
           (compiler-macroexpand-all (cons 'progn body))
           nil)))
    ;; (macrolet ((name lambda-list body)...) forms...) — local macros
    ((and (consp form) (eq (car form) 'macrolet))
     (let ((bindings (second form))
           (body (cddr form))
           (saved-macros nil))
       ;; Save existing macros and register local ones
       (dolist (binding bindings)
         (let* ((name (first binding))
                (lambda-list (second binding))
                (macro-body (cddr binding))
                (old-macro (lookup-macro name))
                (form-var (gensym "FORM"))
                (env-var (gensym "ENV"))
                (expander (eval `(lambda (,form-var ,env-var)
                                   (declare (ignore ,env-var))
                                   (let* ,(generate-lambda-bindings lambda-list form-var)
                                     ,@macro-body)))))
           (push (cons name old-macro) saved-macros)
           (register-macro name expander)))
       ;; Expand the body with local macros active
       (let ((result (compiler-macroexpand-all (cons 'progn body))))
         ;; Restore saved macros
         (dolist (saved saved-macros)
           (if (cdr saved)
               (register-macro (car saved) (cdr saved))
               (remhash (car saved) (macro-env-table *macro-environment*))))
         result)))
    ;; (copy-hash-table ht) => make new HT, copy entries
    ((and (consp form) (symbolp (car form))
          (string-equal (symbol-name (car form)) "COPY-HASH-TABLE")
          (= (length form) 2))
     (let ((ht (gensym "HT")) (new (gensym "NEW")) (k (gensym "K")) (v (gensym "V")))
       (compiler-macroexpand-all
        `(let ((,ht ,(second form)))
           (let ((,new (make-hash-table :test (hash-table-test ,ht))))
             (maphash (lambda (,k ,v) (setf (gethash ,k ,new) ,v)) ,ht)
             ,new)))))
    ;; (prog1 first-form body...) => (let ((#:g first-form)) body... #:g)
    ((and (consp form) (eq (car form) 'prog1) (>= (length form) 2))
     (let ((tmp (gensym "PROG1")))
       (compiler-macroexpand-all
        `(let ((,tmp ,(second form)))
           ,@(cddr form)
           ,tmp))))
    ;; (prog2 first-form second-form body...) => (progn first-form (prog1 second-form body...))
    ((and (consp form) (eq (car form) 'prog2) (>= (length form) 3))
     (compiler-macroexpand-all
      `(progn ,(second form) (prog1 ,(third form) ,@(cdddr form)))))
    ;; (ignore-errors form...) => (handler-case (progn form...) (error (e) (values nil e)))
    ((and (consp form) (eq (car form) 'ignore-errors))
     (let ((e-var (gensym "E")))
       (compiler-macroexpand-all
        `(handler-case (progn ,@(cdr form))
           (error (,e-var) nil)))))
    ;; (reduce fn lst :initial-value val) => (reduce fn lst val t)
    ((and (consp form) (eq (car form) 'reduce)
          (= (length form) 5)
          (eq (fourth form) :initial-value))
     (compiler-macroexpand-all
      (list 'reduce (second form) (third form) (fifth form) 't)))
    ;; (mapcar fn list) => dolist-based collect loop
    ((and (consp form) (eq (car form) 'mapcar)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form))
               (,acc nil))
           (dolist (,x ,(third form) (nreverse ,acc))
             (setq ,acc (cons (funcall ,fn-var ,x) ,acc)))))))
    ;; (mapc fn list) => dolist side-effect loop, returns list
    ((and (consp form) (eq (car form) 'mapc)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (lst (gensym "LST"))
           (x (gensym "X")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form))
               (,lst ,(third form)))
           (dolist (,x ,lst ,lst)
             (funcall ,fn-var ,x))))))
    ;; (mapcan fn list) => dolist + nconc
    ((and (consp form) (eq (car form) 'mapcan)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form))
               (,acc nil))
           (dolist (,x ,(third form) ,acc)
             (setq ,acc (nconc ,acc (funcall ,fn-var ,x))))))))
    ;; (every pred list) => dolist with early exit
    ((and (consp form) (eq (car form) 'every)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (x (gensym "X")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form)))
           (block nil
             (dolist (,x ,(third form) t)
               (unless (funcall ,fn-var ,x)
                 (return nil))))))))
    ;; (some pred list) => dolist with early exit
    ((and (consp form) (eq (car form) 'some)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (result (gensym "R")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form)))
           (block nil
             (dolist (,x ,(third form) nil)
               (let ((,result (funcall ,fn-var ,x)))
                 (when ,result (return ,result)))))))))
    ;; (remove-if pred list) => dolist filter
    ((and (consp form) (eq (car form) 'remove-if)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form))
               (,acc nil))
           (dolist (,x ,(third form) (nreverse ,acc))
             (unless (funcall ,fn-var ,x)
               (setq ,acc (cons ,x ,acc))))))))
    ;; (remove-if-not pred list) => dolist keep-if filter
    ((and (consp form) (eq (car form) 'remove-if-not)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form))
               (,acc nil))
           (dolist (,x ,(third form) (nreverse ,acc))
             (when (funcall ,fn-var ,x)
               (setq ,acc (cons ,x ,acc))))))))
    ;; (find item list) => dolist with eql test
    ((and (consp form) (eq (car form) 'find)
          (= (length form) 3))
     (let ((item (gensym "ITEM"))
           (x (gensym "X")))
       (compiler-macroexpand-all
        `(let ((,item ,(second form)))
           (block nil
             (dolist (,x ,(third form) nil)
               (when (eql ,item ,x)
                 (return ,x))))))))
    ;; (find-if pred list) => dolist with predicate test
    ((and (consp form) (eq (car form) 'find-if)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (x (gensym "X")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form)))
           (block nil
             (dolist (,x ,(third form) nil)
               (when (funcall ,fn-var ,x)
                 (return ,x))))))))
    ;; (position item list) => dolist with index counter
    ((and (consp form) (eq (car form) 'position)
          (= (length form) 3))
     (let ((item (gensym "ITEM"))
           (x (gensym "X"))
           (idx (gensym "IDX")))
       (compiler-macroexpand-all
        `(let ((,item ,(second form))
               (,idx 0))
           (block nil
             (dolist (,x ,(third form) nil)
               (when (eql ,item ,x)
                 (return ,idx))
               (setq ,idx (+ ,idx 1))))))))
    ;; (count item list) => dolist with counter
    ((and (consp form) (eq (car form) 'count)
          (= (length form) 3))
     (let ((item (gensym "ITEM"))
           (x (gensym "X"))
           (cnt (gensym "CNT")))
       (compiler-macroexpand-all
        `(let ((,item ,(second form))
               (,cnt 0))
           (dolist (,x ,(third form) ,cnt)
             (when (eql ,item ,x)
               (setq ,cnt (+ ,cnt 1))))))))
    ;; (count-if pred list) => dolist with predicate counter
    ((and (consp form) (eq (car form) 'count-if)
          (= (length form) 3))
     (let ((fn-var (gensym "FN"))
           (x (gensym "X"))
           (cnt (gensym "CNT")))
       (compiler-macroexpand-all
        `(let ((,fn-var ,(second form))
               (,cnt 0))
           (dolist (,x ,(third form) ,cnt)
             (when (funcall ,fn-var ,x)
               (setq ,cnt (+ ,cnt 1))))))))
    ;; (remove item list) => dolist filter by eql
    ((and (consp form) (eq (car form) 'remove)
          (= (length form) 3))
     (let ((item (gensym "ITEM"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,item ,(second form))
               (,acc nil))
           (dolist (,x ,(third form) (nreverse ,acc))
             (unless (eql ,item ,x)
               (setq ,acc (cons ,x ,acc))))))))
    ;; (remove-duplicates list) => dolist with membership check
    ((and (consp form) (eq (car form) 'remove-duplicates)
          (= (length form) 2))
     (let ((x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,acc nil))
           (dolist (,x ,(second form) (nreverse ,acc))
             (unless (member ,x ,acc)
               (setq ,acc (cons ,x ,acc))))))))
    ;; (union list1 list2) => append elements from list1 not in list2
    ((and (consp form) (eq (car form) 'union)
          (= (length form) 3))
     (let ((l1 (gensym "L1"))
           (l2 (gensym "L2"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,l1 ,(second form))
               (,l2 ,(third form))
               (,acc nil))
           ;; Start with all of l2
           (dolist (,x ,l2)
             (setq ,acc (cons ,x ,acc)))
           ;; Add elements from l1 not already in result
           (dolist (,x ,l1 (nreverse ,acc))
             (unless (member ,x ,l2)
               (setq ,acc (cons ,x ,acc))))))))
    ;; (set-difference list1 list2) => elements in list1 not in list2
    ((and (consp form) (eq (car form) 'set-difference)
          (= (length form) 3))
     (let ((l2 (gensym "L2"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,l2 ,(third form))
               (,acc nil))
           (dolist (,x ,(second form) (nreverse ,acc))
             (unless (member ,x ,l2)
               (setq ,acc (cons ,x ,acc))))))))
    ;; (intersection list1 list2) => elements in both lists
    ((and (consp form) (eq (car form) 'intersection)
          (= (length form) 3))
     (let ((l2 (gensym "L2"))
           (x (gensym "X"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,l2 ,(third form))
               (,acc nil))
           (dolist (,x ,(second form) (nreverse ,acc))
             (when (member ,x ,l2)
               (setq ,acc (cons ,x ,acc))))))))
    ;; (rassoc item alist) => find by cdr
    ((and (consp form) (eq (car form) 'rassoc)
          (= (length form) 3))
     (let ((item (gensym "ITEM"))
           (x (gensym "X")))
       (compiler-macroexpand-all
        `(let ((,item ,(second form)))
           (block nil
             (dolist (,x ,(third form) nil)
               (when (and (consp ,x) (eql ,item (cdr ,x)))
                 (return ,x))))))))
    ;; (pairlis keys data &optional alist) => zip into alist
    ((and (consp form) (eq (car form) 'pairlis)
          (>= (length form) 3))
     (let ((ks (gensym "KS"))
           (ds (gensym "DS"))
           (acc (gensym "ACC")))
       (compiler-macroexpand-all
        `(let ((,ks ,(second form))
               (,ds ,(third form))
               (,acc ,(if (= (length form) 4) (fourth form) nil)))
           (tagbody
            pairlis-loop
              (when (and ,ks ,ds)
                (setq ,acc (cons (cons (car ,ks) (car ,ds)) ,acc))
                (setq ,ks (cdr ,ks))
                (setq ,ds (cdr ,ds))
                (go pairlis-loop)))
           ,acc))))
    ;; (sort list predicate) => inline merge sort via labels
    ((and (consp form) (eq (car form) 'sort)
          (= (length form) 3))
     (let ((lst (gensym "LST"))
           (pred (gensym "PRED"))
           (len (gensym "LEN"))
           (mid (gensym "MID"))
           (left (gensym "LEFT"))
           (right (gensym "RIGHT"))
           (acc (gensym "ACC"))
           (i (gensym "I"))
           (tmp (gensym "TMP"))
           (msort (gensym "MSORT"))
           (mmerge (gensym "MMERGE"))
           (take-n (gensym "TAKEN")))
       (compiler-macroexpand-all
        `(let ((,pred ,(third form)))
           (labels ((,take-n (lst n)
                      (if (= n 0) nil
                          (cons (car lst) (,take-n (cdr lst) (- n 1)))))
                    (,mmerge (a b)
                      (cond ((null a) b)
                            ((null b) a)
                            ((funcall ,pred (car a) (car b))
                             (cons (car a) (,mmerge (cdr a) b)))
                            (t (cons (car b) (,mmerge a (cdr b))))))
                    (,msort (lst)
                      (let ((,len (length lst)))
                        (if (<= ,len 1) lst
                            (let* ((,mid (truncate ,len 2))
                                   (,left (,take-n lst ,mid))
                                   (,right (nthcdr ,mid lst)))
                              (,mmerge (,msort ,left) (,msort ,right)))))))
             (,msort ,(second form)))))))
    ;; (stable-sort list predicate) => same as sort (merge sort is stable)
    ((and (consp form) (eq (car form) 'stable-sort)
          (= (length form) 3))
     (compiler-macroexpand-all
      `(sort ,(second form) ,(third form))))
    ;; (notany pred list) => (not (some pred list))
    ((and (consp form) (eq (car form) 'notany)
          (= (length form) 3))
     (compiler-macroexpand-all
      `(not (some ,(second form) ,(third form)))))
    ;; (notevery pred list) => (not (every pred list))
    ((and (consp form) (eq (car form) 'notevery)
          (= (length form) 3))
     (compiler-macroexpand-all
      `(not (every ,(second form) ,(third form)))))
    ;; (subsetp list1 list2) => (every (lambda (x) (member x list2)) list1)
    ((and (consp form) (eq (car form) 'subsetp)
          (= (length form) 3))
     (let ((l2 (gensym "L2"))
           (x (gensym "X")))
       (compiler-macroexpand-all
        `(let ((,l2 ,(third form)))
           (every (lambda (,x) (member ,x ,l2)) ,(second form))))))
    ;; (adjoin item list) => add if not member
    ((and (consp form) (eq (car form) 'adjoin)
          (= (length form) 3))
     (let ((item (gensym "ITEM"))
           (lst (gensym "LST")))
       (compiler-macroexpand-all
        `(let ((,item ,(second form))
               (,lst ,(third form)))
           (if (member ,item ,lst) ,lst (cons ,item ,lst))))))
    ;; (concatenate 'string s1 s2 ...) => nested string-concat
    ((and (consp form) (eq (car form) 'concatenate)
          (>= (length form) 3))
     (let ((strings (cddr form)))
       (compiler-macroexpand-all
        (if (= (length strings) 1)
            (first strings)
            (reduce (lambda (acc s) (list 'string-concat acc s))
                    (cddr strings)
                    :initial-value (list 'string-concat (first strings) (second strings)))))))
    ;; (list ...) => nested cons
    ((and (consp form) (eq (car form) 'list))
     (compiler-macroexpand-all
      (if (null (cdr form))
          'nil
          (reduce (lambda (x acc) (list 'cons x acc))
                  (cdr form) :from-end t :initial-value 'nil))))
    ;; Typed defun: (defun name ((x fixnum) (y string)) return-type body...)
    ;; → strip types, register in *function-type-registry*, transform to plain defun
    ((and (consp form) (eq (car form) 'defun)
          (>= (length form) 4)
          (symbolp (second form))
          (listp (third form))
          (lambda-list-has-typed-p (third form)))
     (let ((name (second form))
           (raw-params (third form))
           (rest-forms (cdddr form)))
       (multiple-value-bind (plain-params type-alist)
           (strip-typed-params raw-params)
         ;; Detect optional return type (first body form that's a type specifier symbol)
         (let* ((has-return-type (and rest-forms
                                     (symbolp (first rest-forms))
                                     (cl-cc/type:looks-like-type-specifier-p (first rest-forms))))
                (return-type-spec (when has-return-type (first rest-forms)))
                (body-forms (if has-return-type (cdr rest-forms) rest-forms)))
           ;; Register function type for type checking
           (let ((param-types (mapcar (lambda (entry)
                                        (cl-cc/type:parse-type-specifier (cdr entry)))
                                      type-alist))
                 (return-type (if return-type-spec
                                  (cl-cc/type:parse-type-specifier return-type-spec)
                                  cl-cc/type:+type-unknown+)))
             (register-function-type name param-types return-type))
           ;; Transform to plain defun with type assertions
           (let ((typed-body
                   (if return-type-spec
                       `((the ,return-type-spec (progn ,@body-forms)))
                       body-forms)))
             ;; Add check-type assertions for typed params
             (let ((checks (loop for (pname . ptype) in type-alist
                                 collect `(check-type ,pname ,ptype))))
               (compiler-macroexpand-all
                `(defun ,name ,plain-params
                   ,@checks
                   ,@typed-body))))))))
    ;; Typed lambda: (lambda ((x fixnum) (y string)) return-type body...)
    ((and (consp form) (eq (car form) 'lambda)
          (>= (length form) 3)
          (listp (second form))
          (lambda-list-has-typed-p (second form)))
     (let ((raw-params (second form))
           (rest-forms (cddr form)))
       (multiple-value-bind (plain-params type-alist)
           (strip-typed-params raw-params)
         (let* ((has-return-type (and rest-forms
                                     (symbolp (first rest-forms))
                                     (cl-cc/type:looks-like-type-specifier-p (first rest-forms))))
                (return-type-spec (when has-return-type (first rest-forms)))
                (body-forms (if has-return-type (cdr rest-forms) rest-forms)))
           (let ((typed-body
                   (if return-type-spec
                       `((the ,return-type-spec (progn ,@body-forms)))
                       body-forms)))
             (let ((checks (loop for (pname . ptype) in type-alist
                                 collect `(check-type ,pname ,ptype))))
               (compiler-macroexpand-all
                `(lambda ,plain-params
                   ,@checks
                   ,@typed-body))))))))
    ;; (defclass name supers (slot-specs...)) — extract accessor mappings for setf expansion
    ((and (consp form) (eq (car form) 'defclass))
     (let* ((class-name (second form))
            (slot-specs (fourth form)))
       ;; Register accessor->slot mappings so setf works in same progn
       (when (listp slot-specs)
         (dolist (spec slot-specs)
           (when (listp spec)
             (let* ((slot-name (first spec))
                    (opts (rest spec))
                    (accessor (getf opts :accessor)))
               (when accessor
                 (setf (gethash accessor *accessor-slot-map*)
                       (cons class-name slot-name)))))))
       ;; Process as normal special form
       (cons (car form)
             (mapcar #'compiler-macroexpand-all (cdr form)))))
    ;; progn — process forms sequentially so defmacro takes effect for later forms
    ((and (consp form) (eq (car form) 'progn))
     (let ((expanded-forms nil))
       (dolist (sub (cdr form))
         (let ((exp-sub (compiler-macroexpand-all sub)))
           ;; If this is a defmacro, register the macro immediately
           (when (and (consp exp-sub) (eq (car exp-sub) 'defmacro))
             (let* ((name (second exp-sub))
                    (lambda-list (third exp-sub))
                    (body (cdddr exp-sub))
                    (form-var (gensym "FORM"))
                    (env-var (gensym "ENV")))
               (let ((expander (eval `(lambda (,form-var ,env-var)
                                        (declare (ignore ,env-var))
                                        (let* ,(generate-lambda-bindings lambda-list form-var)
                                          ,@body)))))
                 (register-macro name expander))))
           (push exp-sub expanded-forms)))
       (cons 'progn (nreverse expanded-forms))))
    ;; flet/labels — expand only function bodies, not binding structure
    ((and (consp form) (member (car form) '(flet labels))
          (>= (length form) 3) (listp (second form)))
     (let ((expanded-bindings
             (mapcar (lambda (binding)
                       (if (and (consp binding) (>= (length binding) 3))
                           ;; (name (params...) body...) — only expand body
                           (list* (first binding) (second binding)
                                  (mapcar #'compiler-macroexpand-all (cddr binding)))
                           binding))
                     (second form)))
           (expanded-body (mapcar #'compiler-macroexpand-all (cddr form))))
       (list* (car form) expanded-bindings expanded-body)))
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
                                    (list (make-instance 'vm-halt
                                                         :reg result-reg))))
         (optimized-instructions (optimize-instructions full-instructions))
         (optimized-program (make-instance 'vm-program
                                           :instructions optimized-instructions
                                           :result-register result-reg)))
    (list :program optimized-program
          :assembly (emit-assembly optimized-program :target target)
          :type (when type-check inferred-type)
          :cps (if (typep expr 'ast-node)
                   nil
                   (handler-case (cps-transform expr)
                     (error (e) (declare (ignore e)) nil))))))

;;; ----------------------------------------------------------------------------
;;; Standard Library (Higher-Order Functions)
;;; ----------------------------------------------------------------------------

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
  (let* ((result (if stdlib
                     (compile-string-with-stdlib source :target :vm)
                     (compile-string source :target :vm)))
         (program (getf result :program)))
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
         (program (getf result :program)))
    (run-compiled program)))

(defun run-string-typed (source &key (mode :warn))
  "Compile and run SOURCE with type checking enabled.
   MODE is :WARN (default, log warnings) or :STRICT (signal errors)."
  (let* ((result (compile-string source :target :vm :type-check mode))
         (program (getf result :program)))
    (values (run-compiled program) (getf result :type))))

;;; ----------------------------------------------------------------------------
;;; Native Executable Generation (Mach-O)
;;; ----------------------------------------------------------------------------

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
         (program (getf result :program))
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
         (program (getf result :program))
         (code-bytes (compile-to-x86-64-bytes program))
         (builder (cl-cc/binary:make-mach-o-builder arch)))
    (cl-cc/binary:add-text-segment builder code-bytes)
    (cl-cc/binary:add-symbol builder "_main" :value 0 :type #x0F :sect 1)
    (let ((mach-o-bytes (cl-cc/binary:build-mach-o builder code-bytes)))
      (cl-cc/binary:write-mach-o-file output mach-o-bytes))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" (namestring output))
                                :search nil :wait t)
    output))
