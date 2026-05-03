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

(defun %typed-param-stop-marker-p (p)
  "Return T if P is a lambda-list marker that ends required params."
  (if (eq p '&optional)
      t
      (if (eq p '&rest)
          t
          (if (eq p '&key)
              t
              (if (eq p '&body)
                  t
                  (if (eq p '&allow-other-keys)
                      t
                      nil))))))

(defun %typed-type-symbol-known-p (type-spec)
  "Return T if TYPE-SPEC is a known primitive type symbol or alias."
  (let ((result nil))
    (tagbody
     check
       (if (eq type-spec 'fixnum) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'integer) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'int) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'string) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'boolean) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'bool) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'symbol) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'cons) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'null) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 't) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'number) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'float) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'character) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'list) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'vector) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'array) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'hash-table) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'function) (progn (setq result t) (go done)) nil)
       (if (eq type-spec 'sequence) (progn (setq result t) (go done)) nil)
       (if (cl-cc/type:lookup-type-alias type-spec)
           (progn (setq result t) (go done))
           (setq result result))
     done)
    result))

(defun %typed-type-composite-p (type-spec)
  "Return T if TYPE-SPEC is a supported composite type form."
  (if (consp type-spec)
      (if (eq (car type-spec) 'or)
          t
          (if (eq (car type-spec) 'and)
              t
              (if (eq (car type-spec) 'function)
                  t
                  (if (eq (car type-spec) 'values)
                      t
                      (if (eq (car type-spec) 'cons)
                          t
                          (if (eq (car type-spec) 'list)
                              t
                              (if (eq (car type-spec) 'vector)
                                  t
                                  (if (eq (car type-spec) 'array)
                                      t
                                      nil))))))))
      nil))

(defun %typed-param-entry-p (p)
  "Return T if P has typed syntax like (x fixnum)."
  (let ((type-spec nil))
    (if (consp p)
        (if (= (length p) 2)
            (if (symbolp (first p))
                (progn
                  (setq type-spec (second p))
                  (if (symbolp type-spec)
                      (%typed-type-symbol-known-p type-spec)
                      (%typed-type-composite-p type-spec)))
                nil)
            nil)
        nil)))

(defun lambda-list-has-typed-p (params)
  "Return T if required PARAMS contain typed syntax like ((x fixnum) (y string)).
Only checks params before &optional/&rest/&key.
Also recognizes registered type aliases."
  (let ((tail nil)
        (p nil))
    (if (listp params)
        (progn
          (setq tail params)
          (tagbody
           scan
             (if (null tail) (go done))
             (setq p (car tail))
             (if (symbolp p)
                 (if (%typed-param-stop-marker-p p)
                     (return-from lambda-list-has-typed-p nil)
                     (setq p p))
                 (setq p p))
             (if (%typed-param-entry-p p)
                 (return-from lambda-list-has-typed-p t)
                 (setq p p))
             (setq tail (cdr tail))
             (go scan)
           done)
          nil)
        nil)))

(defun expand-type-alias (type-spec)
  "Expand type aliases in TYPE-SPEC recursively."
  (if (symbolp type-spec)
      (let ((expanded (cl-cc/type:lookup-type-alias type-spec)))
        (if expanded (expand-type-alias expanded) type-spec))
      type-spec))

(defun strip-typed-params (params)
  "Strip type annotations from typed params.
((x fixnum) (y string) z) → (values (x y z) ((x . fixnum) (y . string)))"
  (let ((plain nil)
        (type-alist nil)
        (tail params)
        (p nil)
        (name nil)
        (type-spec nil))
    (tagbody
     scan
       (if (null tail) (go done))
       (setq p (car tail))

       (if (symbolp p)
           (if (%typed-param-stop-marker-p p)
               (progn
                 (setq plain (cons p plain))
                 (setq tail (cdr tail))
                 (go scan))
               (progn
                 (setq plain (cons p plain))
                 (setq tail (cdr tail))
                 (go scan)))
           (setq p p))

       (if (consp p)
           (if (= (length p) 2)
               (if (symbolp (first p))
                   (progn
                     (setq name (first p))
                     (setq type-spec (second p))
                     (setq plain (cons name plain))
                     (setq type-alist
                           (cons (cons name (expand-type-alias type-spec))
                                 type-alist))
                     (setq tail (cdr tail))
                     (go scan))
                   (setq p p))
               (setq p p))
           (setq p p))

       (setq plain (cons p plain))
       (setq tail (cdr tail))
       (go scan)

     done)
    (values (nreverse plain) (nreverse type-alist))))
