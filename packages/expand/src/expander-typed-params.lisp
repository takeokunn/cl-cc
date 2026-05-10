(in-package :cl-cc/expand)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expand — Typed Parameter Machinery
;;;
;;; Contains: *function-type-registry*, register-function-type,
;;;           lambda-list-has-typed-p, expand-type-alias, strip-typed-params.
;;;
;;; These helpers detect and strip type annotations from lambda-lists and
;;; register typed function signatures for later compile-time type checking.
;;; They live in the expand package because the expander calls them when
;;; recognising typed defun/lambda forms (expand-typed-defun-or-lambda).
;;; codegen-functions-params.lisp reads *function-type-registry* but does not
;;; define or write it; the one-way dependency flows expand → compile.
;;;
;;; Load order: before expander-core (which calls strip-typed-params and
;;;             register-function-type).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar *function-type-registry* (make-hash-table :test #'eq)
  "Maps function names to their declared type signatures.
Each entry is (param-types . return-type) where param-types is a list of
type-node objects and return-type is a type-node.")

(defun register-function-type (name param-types return-type)
  "Register a typed function signature for type checking."
  (setf (gethash name *function-type-registry*) (cons param-types return-type)))

(defparameter *typed-lambda-list-stop-markers*
  '(&optional &rest &key &body &allow-other-keys)
  "Lambda-list markers that terminate the required parameter section.")

(defun %typed-param-stop-marker-p (p)
  "Return T if P is a lambda-list marker that ends required params."
  (and (member p *typed-lambda-list-stop-markers* :test #'eq) t))

(defparameter *typed-primitive-type-symbols*
  '(fixnum integer int string boolean bool symbol cons null t
    number float character list vector array hash-table function sequence)
  "Primitive type symbols recognized in typed parameter syntax.")

(defparameter *typed-composite-type-heads*
  '(or and function values cons list vector array)
  "Car symbols that mark a composite type form in typed parameter syntax.")

(defun %typed-type-symbol-known-p (type-spec)
  "Return T if TYPE-SPEC is a known primitive type symbol or alias."
  (or (and (member type-spec *typed-primitive-type-symbols* :test #'eq) t)
      (and (cl-cc/type:lookup-type-alias type-spec) t)))

(defun %typed-type-composite-p (type-spec)
  "Return T if TYPE-SPEC is a supported composite type form."
  (and (consp type-spec)
       (member (car type-spec) *typed-composite-type-heads* :test #'eq)
       t))

(defun %typed-param-entry-p (p)
  "Return T if P has typed syntax like (x fixnum)."
  (and (consp p)
       (= (length p) 2)
       (symbolp (first p))
       (let ((type-spec (second p)))
         (if (symbolp type-spec)
             (%typed-type-symbol-known-p type-spec)
             (%typed-type-composite-p type-spec)))))

(defun lambda-list-has-typed-p (params)
  "Return T if required PARAMS contain typed syntax like ((x fixnum) (y string)).
Only checks params before &optional/&rest/&key.
Also recognizes registered type aliases."
  (when (listp params)
    (dolist (p params)
      (when (and (symbolp p) (%typed-param-stop-marker-p p))
        (return-from lambda-list-has-typed-p nil))
      (when (%typed-param-entry-p p)
        (return-from lambda-list-has-typed-p t))))
  nil)

(defun expand-type-alias (type-spec)
  "Expand type aliases in TYPE-SPEC recursively."
  (if (symbolp type-spec)
      (let ((expanded (cl-cc/type:lookup-type-alias type-spec)))
        (if expanded (expand-type-alias expanded) type-spec))
      type-spec))

(defun strip-typed-params (params)
  "Strip type annotations from typed params.
((x fixnum) (y string) z) → (values (x y z) ((x . fixnum) (y . string)))"
  (loop for p in params
        if (and (consp p) (= (length p) 2) (symbolp (first p)))
          collect (first p) into plain
          and collect (cons (first p) (expand-type-alias (second p))) into type-alist
        else
          collect p into plain
        finally (return (values plain type-alist))))
