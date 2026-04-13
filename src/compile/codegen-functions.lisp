(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Function Compilation
;;;
;;; Contains: defmacro, lambda, defun, defvar/defparameter compilation, plus
;;; all parameter-list helpers (optional/rest/key allocation, boxing, defaults)
;;; and the typed-parameter machinery (type registry, strip-typed-params).
;;;
;;; Load order: after codegen-clos, before codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Typed parameter machinery ────────────────────────────────────────────
;;;
;;; These are also called by expand/expander.lisp (expand-typed-defun-or-lambda).
;;; They live here because they describe compile-time type semantics.

(defvar *function-type-registry* (make-hash-table :test #'eq)
  "Maps function names to their declared type signatures.
Each entry is (param-types . return-type) where param-types is a list of
type-node objects and return-type is a type-node.")

(defun register-function-type (name param-types return-type)
  "Register a typed function signature for type checking."
  (setf (gethash name *function-type-registry*) (cons param-types return-type)))

(defun lambda-list-has-typed-p (params)
  "Return T if required PARAMS contain typed syntax like ((x fixnum) (y string)).
Only checks params before &optional/&rest/&key.
Also recognizes registered type aliases."
  (and (listp params)
       (loop for p in params
             when (and (symbolp p)
                       (member p '(&optional &rest &key &body &allow-other-keys)))
               return nil
             thereis (and (consp p)
                          (= (length p) 2)
                          (symbolp (first p))
                          (let ((type-spec (second p)))
                            (or (and (symbolp type-spec)
                                     (or (member type-spec
                                                 '(fixnum integer int string boolean bool
                                                   symbol cons null t number float
                                                   character list vector array
                                                   hash-table function sequence))
                                         (cl-cc/type:lookup-type-alias type-spec)))
                                (and (consp type-spec)
                                     (member (car type-spec)
                                             '(or and function values cons list vector array)))))))))

(defun expand-type-alias (type-spec)
  "Expand type aliases in TYPE-SPEC recursively."
  (if (symbolp type-spec)
      (let ((expanded (cl-cc/type:lookup-type-alias type-spec)))
        (if expanded (expand-type-alias expanded) type-spec))
      type-spec))

(defun strip-typed-params (params)
  "Strip type annotations from typed params.
((x fixnum) (y string) z) → (values (x y z) ((x . fixnum) (y . string)))"
  (let ((plain nil) (type-alist nil))
    (dolist (p params)
      (cond
        ((and (symbolp p)
              (member p '(&optional &rest &key &body &allow-other-keys)))
         (push p plain))
        ((and (consp p) (= (length p) 2) (symbolp (first p)))
         (push (first p) plain)
         (push (cons (first p) (expand-type-alias (second p))) type-alist))
        ((symbolp p) (push p plain))
        (t (push p plain))))
    (values (nreverse plain) (nreverse type-alist))))

;;; ── defmacro ─────────────────────────────────────────────────────────────

(defmethod compile-ast ((node ast-defmacro) ctx)
  "Compile a top-level macro definition.
Registers the macro expander at compile time so subsequent forms can use it.
At runtime, defmacro evaluates to the macro name."
  (setf (ctx-tail-position ctx) nil)
  (let ((name (ast-defmacro-name node))
        (lambda-list (ast-defmacro-lambda-list node))
        (body (ast-defmacro-body node)))
    (register-macro name (make-macro-expander lambda-list body))
    (let ((dst (make-register ctx)))
      (emit ctx (make-vm-const :dst dst :value name))
      dst)))

;;; Parameter-list helpers (allocate-defaulting-params, allocate-extended-params,
;;; rest-param-stack-alloc-p, emit-supplied-p-checks, emit-non-constant-defaults,
;;; build-all-param-bindings, function-param-type-bindings, compile-function-body)
;;; are in codegen-functions-params.lisp (loads next).


;;; (%emit-closure-body, compile-ast for ast-lambda/ast-defun/ast-defvar
;;;  are in codegen-functions-emit.lisp which loads after this file.)
