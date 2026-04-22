;;;; parser.lisp - Type Annotation Parser
;;;;
;;;; Parses CL-style type annotation s-expressions into type-node trees.
;;;;
;;;; Supported syntax:
;;;;   fixnum, string, boolean, symbol, etc.     primitive types
;;;;   (or T1 T2 ...)                            union
;;;;   (and T1 T2 ...)                           intersection
;;;;   (function (PARAMS...) RETURN)             arrow (pure, ω)
;;;;   (-> A B)                                  pure arrow (ω)
;;;;   (->1 A B)                                 linear arrow (1)
;;;;   (->0 A B)                                 erased arrow (0)
;;;;   (-> A B ! IO State)                       effectful arrow
;;;;   (-> A B / <IO | ε>)                       effectful arrow (alt)
;;;;   (values T1 T2 ...)                        product / tuple
;;;;   (list T)                                  List T  (type-app)
;;;;   (forall a T)  or  (∀ a T)                universal quantification
;;;;   (exists a T)                              existential quantification
;;;;   (mu a T)                                  recursive type μa.T
;;;;   (=> (C a) T)                              qualified type
;;;;   (Refine T pred)                           refinement type
;;;;   (Record (l1 T1) ... | ρ)                  row-polymorphic record
;;;;   (Variant (L1 T1) ... | ρ)                 row-polymorphic variant
;;;;   (! 1 T)  /  (!1 T)  /  (!ω T)            graded modal types

(in-package :cl-cc/type)

;;; ─── Conditions ───────────────────────────────────────────────────────────

(define-condition type-parse-error (error)
  ((message :initarg :message :reader type-parse-error-message)
   (source  :initarg :source  :initform nil :reader type-parse-error-source))
  (:report (lambda (c s)
             (format s "Type parse error: ~A" (type-parse-error-message c)))))

(defun type-parse-error (msg &rest args)
  (error 'type-parse-error :message (apply #'format nil msg args)))

;;; ─── Main entry ───────────────────────────────────────────────────────────

(defun parse-type-specifier (spec)
  "Parse SPEC into a type-node."
  (typecase spec
    (null   type-null)
    (symbol (if (member (symbol-name spec) '("?" "_") :test #'string=)
                (make-type-error :message "unknown")   ; ? / _ → gradual typing hole
                (parse-primitive-type spec)))
    (cons   (parse-compound-type spec))
    (t      (type-parse-error "Invalid type specifier: ~S" spec))))

;;; ─── Primitive types ──────────────────────────────────────────────────────
;;; Data table maps groups of equivalent name strings to their type constants.
;;; Package-independent: all comparisons use string= (symbol-name is always uppercase).

(defvar *primitive-type-name-table*
  `((("FIXNUM" "INTEGER" "INT")              . ,type-int)
    (("FLOAT" "SINGLE-FLOAT" "DOUBLE-FLOAT") . ,type-float)
    (("STRING" "SIMPLE-STRING")              . ,type-string)
    (("BOOLEAN" "BOOL")                      . ,type-bool)
    (("SYMBOL")                              . ,type-symbol)
    (("CONS")                                . ,type-cons)
    (("NULL" "NIL")                          . ,type-null)
    (("T" "TOP")                             . ,type-any)
    (("CHARACTER" "CHAR")                    . ,type-char))
  "Alist of (name-strings . type-constant) for primitive type lookup.")

(defun parse-primitive-type (name)
  "Parse a symbol into a primitive type. Dispatches via *primitive-type-name-table*."
  (let* ((sname (symbol-name name))
         (entry (assoc sname *primitive-type-name-table*
                       :test (lambda (s names) (member s names :test #'string=)))))
    (if entry
        (cdr entry)
        (let ((alias (and (boundp '*type-alias-registry*)
                          (gethash name *type-alias-registry*))))
          (if alias
              (parse-type-specifier alias)
              (make-type-primitive :name name))))))

;;; ─── Compound types ───────────────────────────────────────────────────────

(defun parse-compound-type (spec)
  (let ((head (car spec))
        (args (cdr spec)))
    (case head
      ;; (or T1 T2 ...) — union
      ((or)
       (unless args (type-parse-error "or requires at least one type"))
       (make-type-union (mapcar #'parse-type-specifier args)))

      ;; (and T1 T2 ...) — intersection
      ((and)
       (unless args (type-parse-error "and requires at least one type"))
       (make-type-intersection (mapcar #'parse-type-specifier args)))

      ;; (function (PARAMS...) RETURN) — backward-compat function type
      ((function)
       (unless (and (= (length args) 2) (listp (first args)))
         (type-parse-error "function type: (function (PARAMS...) RETURN)"))
       (make-type-arrow (mapcar #'parse-type-specifier (first args))
                        (parse-type-specifier (second args))))

      ;; (values T1 T2 ...) — product / tuple
      ((values)
       (make-type-product :elems (mapcar #'parse-type-specifier args)))

      ;; (cons T1 T2) — backward compat: cons pair as 2-tuple
      ((cons)
       (unless (= (length args) 2)
         (type-parse-error "cons requires 2 types"))
       (make-type-product :elems (mapcar #'parse-type-specifier args)))

      ;; (list T) → (type-app List T)
      ((list)
       (unless (= (length args) 1)
         (type-parse-error "list requires exactly 1 type"))
       (make-type-app :fun  (make-type-primitive :name 'list)
                      :arg  (parse-type-specifier (first args))))

      ;; (vector T) / (array T)
      ((vector simple-vector)
       (unless (= (length args) 1) (type-parse-error "vector requires 1 type"))
       (make-type-app :fun (make-type-primitive :name 'vector)
                      :arg (parse-type-specifier (first args))))
      ((array simple-array)
        (unless (= (length args) 1) (type-parse-error "array requires 1 type"))
        (make-type-app :fun (make-type-primitive :name 'array)
                       :arg (parse-type-specifier (first args))))

       ;; (option T) → (or null T) — nullable/optional type sugar
       ((option)
        (unless (= (length args) 1)
          (type-parse-error "option requires exactly 1 type"))
        (make-type-union (list type-null (parse-type-specifier (first args)))
                         :constructor-name head))

      (otherwise
        (parse-compound-type-extended head args)))))

;;; Extended compound type parsing (arrow forms, quantifiers, effects,
;;; row types, graded modals) is in parser-extended.lisp (loads after this file).

;;; (parse-row-type, parse-constraint-spec, lambda-list parsing,
;;;  typed AST defstructs, and utilities are in parser-typed.lisp
;;;  which loads after parser-extended.)

