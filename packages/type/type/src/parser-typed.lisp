;;;; packages/type/type/src/parser-typed.lisp — Typed-AST parsing and lambda-list utilities
;;;
;;; Contains:
;;;   - parse-row-type (Record/Variant row-polymorphic types)
;;;   - parse-constraint-spec
;;;   - parse-lambda-list-with-types, parse-typed-parameter,
;;;     parse-typed-optional-parameter, parse-typed-rest-parameter
;;;   - extract-return-type, extract-return-type-from-body
;;;   - ast-defun-typed defstruct + ast-lambda-typed defstruct
;;;   - parse-typed-defun, parse-typed-lambda, parse-typed-lambda-list
;;;   - parse-type-specifier-maybe, extract-return-type-maybe
;;;   - *lambda-list-keywords*, make-type-function-from-spec
;;;
;;; Core type parsing (parse-type-specifier, parse-primitive-type,
;;; parse-compound-type, parse-arrow-type, parse-effect-row-spec)
;;; are in parser.lisp (loads before).
;;;
;;; Load order: after type/parser.lisp.
(in-package :cl-cc/type)

;;; ─── Row type parsing ─────────────────────────────────────────────────────

(defun parse-row-type (args kind)
  "Parse (Record (l1 T1) ... | ρ) or (Variant (L1 T1) ... | ρ)."
  (let* ((pipe-pos (position-if (lambda (x) (and (symbolp x)
                                                   (string= (symbol-name x) "|")))
                                 args))
         (field-specs (if pipe-pos (subseq args 0 pipe-pos) args))
         (row-var-spec (when pipe-pos (nth (1+ pipe-pos) args)))
         (row-var (when row-var-spec (parse-type-specifier row-var-spec)))
         (fields (mapcar (lambda (f)
                           (unless (and (consp f) (= (length f) 2))
                             (type-parse-error "Row field must be (label type), got ~S" f))
                           (cons (first f) (parse-type-specifier (second f))))
                         field-specs)))
    (ecase kind
      (:record  (make-type-record  :fields fields :row-var row-var))
      (:variant (make-type-variant :cases  fields :row-var row-var)))))

;;; ─── Constraint spec parsing ──────────────────────────────────────────────

(defun parse-constraint-spec (spec)
  "Parse a typeclass constraint spec like (Num a) into a type-class-constraint."
  (unless (and (consp spec) (>= (length spec) 2) (symbolp (first spec)))
    (type-parse-error "Constraint must be (ClassName type-arg), got ~S" spec))
  (make-type-class-constraint :class-name (first spec)
                               :type-arg   (parse-type-specifier (second spec))))

;;; ─── Lambda-list parsing ──────────────────────────────────────────────────

(defun parse-lambda-list-with-types (lambda-list)
  "Parse a typed lambda list like ((x fixnum) y (z string)).
Returns (values param-names param-types) where untyped params get type-any."
  (let (names types)
    (dolist (item lambda-list)
      (cond
        ((and (consp item) (= (length item) 2))
         (push (first item) names)
         (push (parse-type-specifier (second item)) types))
        ((symbolp item)
         (push item names)
         (push type-any types))
        (t (type-parse-error "Invalid lambda-list item: ~S" item))))
    (values (nreverse names) (nreverse types))))

(defun parse-typed-parameter (item)
  "Parse a single possibly-typed parameter (x) or (x fixnum). Returns (name . type)."
  (if (and (consp item) (= (length item) 2) (symbolp (first item)))
      (cons (first item) (parse-type-specifier (second item)))
      (cons item type-any)))

(defun parse-typed-optional-parameter (item)
  "Parse an &optional parameter possibly typed: (x fixnum default) → (name . type)."
  (if (consp item)
      (cons (first item)
            (if (>= (length item) 2)
                (parse-type-specifier (second item))
                type-any))
      (cons item type-any)))

(defun parse-typed-rest-parameter (item)
  "Parse an &rest parameter: x or (x type). Returns (name . type)."
  (parse-typed-parameter item))

;;; ─── Return type extraction ───────────────────────────────────────────────

(defun extract-return-type (body)
  "If BODY starts with a type annotation (declare (return-type T)), extract T."
  (when (and (consp body)
             (consp (first body))
             (eq (caar body) 'declare))
    (let ((decl (cdar body)))
      (when (and decl (consp (car decl))
                 (and (symbolp (caar decl))
                      (string= (symbol-name (caar decl)) "RETURN-TYPE")))
        (parse-type-specifier (cadar decl))))))

(defun extract-return-type-from-body (body)
  "Extract return type from (declare (return-type ...)) forms in BODY."
  (extract-return-type body))

;;; ─── Typed AST nodes ──────────────────────────────────────────────────────

(defstruct (ast-defun-typed (:constructor make-ast-defun-typed))
  "A defun node with type annotations."
  (name nil :type symbol)
  (params nil :type list)
  (param-types nil :type list)
  (return-type nil)
  (body nil)
  (source-location nil))

(defstruct (ast-lambda-typed (:constructor make-ast-lambda-typed))
  "A lambda node with type annotations."
  (params nil :type list)
  (param-types nil :type list)
  (return-type nil)
  (body nil)
  (env nil)
  (source-location nil))

(defun parse-typed-defun (form)
  "Parse (defun name ((x T) ...) return-type body...) into ast-defun-typed."
  (destructuring-bind (name lambda-list . rest) (cdr form)
    (multiple-value-bind (names types) (parse-lambda-list-with-types lambda-list)
      (let ((return-type (and (not (null rest))
                              (not (consp (first rest)))
                              (not (eq (first rest) 'declare))
                              (parse-type-specifier-maybe (first rest))))
            (body (if (and rest (not (null (extract-return-type-maybe rest))))
                      (cdr rest)
                      rest)))
        (make-ast-defun-typed
         :name name :params names :param-types types
         :return-type (or return-type type-any) :body body)))))

(defun parse-typed-lambda (form)
  "Parse (lambda ((x T) ...) body...) into ast-lambda-typed."
  (destructuring-bind (lambda-list . body) (cdr form)
    (multiple-value-bind (names types) (parse-lambda-list-with-types lambda-list)
      (make-ast-lambda-typed
       :params names :param-types types :return-type type-any :body body))))

(defun parse-typed-lambda-list (lambda-list)
  "Parse a typed lambda list. Returns (values names types)."
  (parse-lambda-list-with-types lambda-list))

(defun parse-type-specifier-maybe (x)
  "Return a type-node if X looks like a type specifier, else nil."
  (when (looks-like-type-specifier-p x)
    (handler-case (parse-type-specifier x)
      (type-parse-error () nil))))

(defun extract-return-type-maybe (forms)
  "Try to extract a return type from the front of FORMS list."
  (extract-return-type forms))

;;; ─── Utility ──────────────────────────────────────────────────────────────

(defvar *lambda-list-keywords*
  '(&optional &rest &key &allow-other-keys &aux)
  "Lambda list keywords to skip during typed parameter parsing.")

(defun make-type-function-from-spec (param-types return-type)
  "Create an arrow type from a list of param types and a return type."
  (make-type-arrow param-types return-type))
