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
