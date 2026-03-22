;;;; parser.lisp - Type Annotation Parser
;;;;
;;;; This module provides parsing functions for TypeScript-style type
;;;; annotations in Common Lisp function signatures.
;;;;
;;;; Target Syntax:
;;;;   (defun my-add ((x fixnum) (y fixnum)) fixnum
;;;;     (+ x y))
;;;;
;;;;   (lambda ((x fixnum) (y string)) string
;;;;     (concat x y))
;;;;
;;;; Type Specifiers:
;;;;   fixnum                    ; primitive
;;;;   (or fixnum null)          ; union
;;;;   (function (fixnum fixnum) fixnum)  ; function type
;;;;   (values fixnum string)    ; multiple values
;;;;   ?                         ; unknown/any

(in-package :cl-cc/type)

;;; Conditions

(define-condition type-parse-error (error)
  ((message :initarg :message :reader type-parse-error-message)
   (source :initarg :source :initform nil :reader type-parse-error-source))
  (:documentation "Error condition for type parsing failures.")
  (:report (lambda (condition stream)
             (format stream "Type parse error: ~A"
                     (type-parse-error-message condition)))))

(defun type-parse-error (message &rest args)
  "Signal a type parse error with formatted MESSAGE."
  (error 'type-parse-error
         :message (apply #'format nil message args)))

;;; Type Specifier Parser

(defun parse-type-specifier (spec)
  "Parse a type specifier into a type-node.

   Examples:
     fixnum           → type-primitive fixnum
     (or fixnum null) → type-union (fixnum null)
     (function (fixnum fixnum) fixnum) → type-function
     (values fixnum string) → type-tuple
     ?                → type-unknown

   Supported type specifiers:
     - SYMBOL: primitive type (fixnum, string, boolean, t, null, etc.)
     - ?: unknown type (escape hatch for gradual typing)
     - (or T1 T2 ...): union type
     - (and T1 T2 ...): intersection type
     - (function (PARAM-TYPES...) RETURN-TYPE): function type
     - (values T1 T2 ...): multiple values as tuple type
     - (cons T1 T2): cons type (parsed as tuple)
     - (list T): list type
     - (vector T): vector type
     - (array T): array type"
  (cond
    ;; Unknown type escape hatch
    ((eq spec '?)
     +type-unknown+)

    ;; Symbol - primitive or named type
    ((symbolp spec)
     (parse-primitive-type spec))

    ;; List - compound type
    ((consp spec)
     (parse-compound-type spec))

    ;; Invalid type specifier
    (t
     (type-parse-error "Invalid type specifier: ~S" spec))))

(defun parse-primitive-type (name)
  "Parse a symbol as a primitive type name.
   Checks type alias registry before creating a named primitive."
  (check-type name symbol)
  (case name
    ;; Map Common Lisp type names to canonical forms
    ((fixnum integer int) type-int)
    ((string simple-string) type-string)
    ((boolean bool) type-bool)
    ((symbol) type-symbol)
    ((cons) type-cons)
    ((null nil) type-null)
    ((t top) type-any)
    (otherwise
     ;; Check type alias registry
     (let ((alias (lookup-type-alias name)))
       (if alias
           (parse-type-specifier alias)
           ;; Create a primitive type for any other symbol
           (make-type-primitive :name name))))))

(defun parse-compound-type (spec)
  "Parse a compound type specifier (list form)."
  (let ((head (car spec))
        (args (cdr spec)))
    (case head
      ;; Union type: (or T1 T2 ...)
      ((or)
       (unless args
         (type-parse-error "Union type requires at least one type"))
       (make-type-union (mapcar #'parse-type-specifier args)))

      ;; Intersection type: (and T1 T2 ...)
      ((and)
       (unless args
         (type-parse-error "Intersection type requires at least one type"))
       (make-type-intersection (mapcar #'parse-type-specifier args)))

      ;; Function type: (function (PARAM-TYPES...) RETURN-TYPE)
      ((function)
       (parse-function-type args))

      ;; Multiple values: (values T1 T2 ...)
      ((values)
       (if args
           (make-type-tuple (mapcar #'parse-type-specifier args))
           ;; (values) = no values = nil
           (make-type-tuple (list type-null))))

      ;; Cons type: (cons CAR-TYPE CDR-TYPE)
      ((cons)
       (unless (= (length args) 2)
         (type-parse-error "cons type requires exactly 2 types"))
       (make-type-tuple (mapcar #'parse-type-specifier args)))

      ;; List type: (list ELEMENT-TYPE) - parametric type constructor
      ((list)
       (unless (= (length args) 1)
         (type-parse-error "list type requires exactly 1 type"))
       (make-type-constructor 'list (list (parse-type-specifier (first args)))))

      ;; Vector type: (vector ELEMENT-TYPE)
      ((vector simple-vector)
       (unless (= (length args) 1)
         (type-parse-error "vector type requires exactly 1 type"))
       (make-type-constructor 'vector (list (parse-type-specifier (first args)))))

      ;; Array type: (array ELEMENT-TYPE)
      ((array simple-array)
       (unless (= (length args) 1)
         (type-parse-error "array type requires exactly 1 type"))
       (make-type-constructor 'array (list (parse-type-specifier (first args)))))

      ;; Parametric type constructor: (TypeName Arg1 Arg2 ...)
      ;; E.g. (Option fixnum), (Pair fixnum string), (Result fixnum string)
      (otherwise
       (if (and (symbolp head) args)
           (make-type-constructor head (mapcar #'parse-type-specifier args))
           (make-type-primitive :name spec))))))

(defun parse-function-type (args)
  "Parse function type arguments: ((PARAM-TYPES...) RETURN-TYPE) or (PARAM-TYPES... -> RETURN-TYPE)."
  (cond
    ;; Standard CL: (function (param-types...) return-type)
    ((and (= (length args) 2)
          (listp (first args)))
     (let ((param-types (mapcar #'parse-type-specifier (first args)))
           (return-type (parse-type-specifier (second args))))
       (make-type-function param-types return-type)))

    ;; Arrow notation: (function param1 param2 ... -> return-type)
    ;; Not standard CL, but useful for type inference display
    ((member '-> args)
     (let ((arrow-pos (position '-> args)))
       (let ((param-types (mapcar #'parse-type-specifier (subseq args 0 arrow-pos)))
             (return-type (parse-type-specifier (nth (1+ arrow-pos) args))))
         (make-type-function param-types return-type))))

    ;; Single type - nullary function returning that type
    ((= (length args) 1)
     (make-type-function nil (parse-type-specifier (first args))))

    (t
     (type-parse-error "Invalid function type specifier: ~S" args))))

;;; Lambda List Parser with Types

(defvar *lambda-list-keywords*
  '(&optional &rest &key &allow-other-keys &aux &body &whole)
  "Standard Common Lisp lambda list keywords.")

(defun parse-lambda-list-with-types (lambda-list)
  "Parse a lambda list with optional type annotations.

   Input:  ((x fixnum) (y fixnum) &optional (z string))
   Output: ((x :type fixnum) (y :type fixnum)
            &optional (z :type string))

   The function recognizes:
     - Typed parameters: (name type)
     - Untyped parameters: name
     - Optional parameters: &optional (name type) or (name type default)
     - Rest parameters: &rest name or (rest-name type)
     - Key parameters: &key (name type) or ((keyword name) type default)

   Returns a list suitable for use in typed lambda expressions."

  (let ((result nil)
        (section :required))

    (dolist (item lambda-list)
      (cond
        ;; Lambda list keyword - switch section
        ((member item *lambda-list-keywords*)
         (push item result)
         (setf section (keyword-from-lambda-keyword item)))

        ;; Handle based on current section
        (t
         (case section
           (:required
            (push (parse-typed-parameter item) result))

           ((:optional :key)
            (push (parse-typed-optional-parameter item) result))

           (:rest
            (push (parse-typed-rest-parameter item) result))

           (:body
            (push (parse-typed-parameter item) result))

           (:aux
            (push (parse-typed-aux-parameter item) result))

           (otherwise
            (push item result))))))

    (nreverse result)))

(defun keyword-from-lambda-keyword (sym)
  "Convert lambda list keyword to section keyword."
  (case sym
    (&optional :optional)
    (&rest :rest)
    (&body :body)
    (&key :key)
    (&aux :aux)
    (otherwise :unknown)))

(defun parse-typed-parameter (item)
  "Parse a required parameter that may have a type annotation.

   (x fixnum) → (x :type fixnum)
   x          → x"
  (cond
    ;; Typed parameter: (name type)
    ((and (consp item)
          (= (length item) 2)
          (symbolp (first item)))
     (list (first item) :type (second item)))

    ;; Simple symbol - no type annotation
    ((symbolp item)
     item)

    ;; Invalid parameter specification
    (t
     (type-parse-error "Invalid parameter specification: ~S" item))))

(defun parse-typed-optional-parameter (item)
  "Parse an optional parameter that may have a type annotation.

   (x fixnum)         → (x :type fixnum)
   (x fixnum nil)     → (x :type fixnum nil)
   (x fixnum nil xp)  → (x :type fixnum nil xp)
   x                  → x"
  (cond
    ;; Typed optional with default and supplied-p
    ((and (consp item)
          (>= (length item) 2)
          (symbolp (first item)))
     (let* ((name (first item))
            (type (if (and (= (length item) 2))
                      (second item)
                      nil))
            (has-type (and type (not (symbolp type)))))
       (if has-type
           (list* name :type type (if (> (length item) 2) (cddr item) nil))
           ;; No type annotation, treat as standard CL optional
           item)))

    ;; Simple symbol - no type annotation
    ((symbolp item)
     item)

    (t
     (type-parse-error "Invalid optional parameter specification: ~S" item))))

(defun parse-typed-rest-parameter (item)
  "Parse a rest parameter that may have a type annotation.

   (items list) → (items :type list)
   items        → items"
  (cond
    ;; Typed rest parameter: (name type)
    ((and (consp item)
          (= (length item) 2)
          (symbolp (first item)))
     (list (first item) :type (second item)))

    ;; Simple symbol - no type annotation
    ((symbolp item)
     item)

    (t
     (type-parse-error "Invalid rest parameter specification: ~S" item))))

(defun parse-typed-aux-parameter (item)
  "Parse an aux parameter that may have a type annotation.

   (x fixnum)     → (x :type fixnum)
   (x fixnum val) → (x :type fixnum val)
   x              → x"
  ;; For now, delegate to optional parameter parsing
  ;; as aux parameters have similar structure
  (parse-typed-optional-parameter item))

;;; Return Type Extraction

(defun extract-return-type (decl)
  "Extract return type from function declarations.

   Input:  fixnum                ; single return type
   Input:  (values fixnum string) ; multiple return values
   Output: type-node

   If DECL is nil, returns +type-unknown+."
  (cond
    ;; No return type specified
    ((null decl)
     +type-unknown+)

    ;; Multiple values return type
    ((and (consp decl)
          (eq (car decl) 'values))
     (parse-type-specifier decl))

    ;; Single value return type (possibly a list like (values ...))
    ((consp decl)
     (parse-type-specifier decl))

    ;; Symbol return type
    ((symbolp decl)
     (parse-type-specifier decl))

    (t
     +type-unknown+)))

;;; Typed Defun Parser

(defstruct ast-defun-typed
  (name nil :type symbol)
  (params nil :type list)
  (param-types nil :type list)
  (return-type nil)
  (body nil :type list)
  (source-location nil))

(defun parse-typed-defun (sexp)
  "Parse a defun with type annotations.

   (defun name ((param type) ...) return-type body)
   → ast-defun-typed instance

   Two forms are supported:
   1. Typed parameters with optional return type:
      (defun my-add ((x fixnum) (y fixnum)) fixnum
        (+ x y))

   2. Untyped parameters (standard CL):
      (defun my-add (x y)
        (+ x y))

   The return type can be:
   - A symbol: fixnum, string, etc.
   - A compound type: (values fixnum string)
   - Omitted: defaults to unknown type"

  (unless (and (consp sexp) (eq (car sexp) 'defun))
    (type-parse-error "Expected defun form, got: ~S" sexp))

  (unless (>= (length sexp) 4)
    (type-parse-error "defun requires name, parameters, and body"))

  (destructuring-bind (defun-op name params &rest body-and-maybe-type) sexp
    (declare (ignore defun-op))

    (unless (symbolp name)
      (type-parse-error "defun name must be a symbol: ~S" name))

    ;; Determine if the first element after params is a return type
    ;; or the start of the body
    (multiple-value-bind (return-type-spec body-forms)
        (extract-return-type-from-body body-and-maybe-type)

      ;; Parse parameters with types
      (multiple-value-bind (parsed-params param-types)
          (parse-typed-lambda-list params)

        ;; Parse return type
        (let ((return-type (extract-return-type return-type-spec)))

          ;; Create typed defun AST
          (make-ast-defun-typed
                         :name name
                         :params parsed-params
                         :param-types param-types
                         :return-type return-type
                         :body body-forms))))))

(defun extract-return-type-from-body (body-and-maybe-type)
  "Determine if the first element after params is a return type or body.

   Returns (values return-type-spec body-forms).

   Heuristics:
   - If first element is a type specifier (symbol starting with lowercase
     or is a compound type), it's the return type
   - Otherwise, everything is body forms"

  (when (null body-and-maybe-type)
    (type-parse-error "defun requires at least one body form"))

  (let ((first (car body-and-maybe-type)))
    (cond
      ;; Check if first looks like a return type
      ;; A type specifier is either:
      ;; 1. A symbol (not a lambda list keyword, not starting a compound form)
      ;; 2. A list starting with values, or, and, function

      ;; Compound type as return type
      ((and (consp first)
            (member (car first) '(values or and function cons list vector array)))
       (values first (cdr body-and-maybe-type)))

      ;; Symbol that could be a type (not a lambda list keyword)
      ((and (symbolp first)
            (not (member first *lambda-list-keywords*))
            (looks-like-type-specifier-p first))
       (values first (cdr body-and-maybe-type)))

      ;; Everything is body
      (t
       (values nil body-and-maybe-type)))))

(defun looks-like-type-specifier-p (sym)
  "Heuristic to determine if SYM looks like a type specifier.

   Common type names: fixnum, string, boolean, t, null, etc.
   Also checks the type alias registry.
   Returns T if SYM is likely a type name."
  (or (member sym '(fixnum integer int string simple-string
                    boolean bool symbol cons null nil t top
                    number float real rational complex
                    character base-char
                    sequence list vector array
                    function hash-table package pathname
                    stream random-state readtable))
      (lookup-type-alias sym)))

(defun parse-typed-lambda-list (lambda-list)
  "Parse a lambda list that may contain typed parameters.

   Returns (values parsed-params param-types).

   parsed-params: list suitable for binding ((x :type fixnum) y ...)
   param-types: list of type-node objects"
  (let ((parsed nil)
        (types nil)
        (section :required))

    (dolist (item lambda-list)
      (cond
        ;; Lambda list keyword
        ((member item *lambda-list-keywords*)
         (push item parsed)
         (setf section (keyword-from-lambda-keyword item)))

        ;; Handle based on section
        (t
         (case section
           (:required
            (multiple-value-bind (parsed-item type-node)
                (parse-required-param-with-type item)
              (push parsed-item parsed)
              (when type-node
                (push type-node types))))

           ((:optional :rest :key :body :aux)
            ;; For now, just pass through
            (push item parsed))

           (otherwise
            (push item parsed))))))

    (values (nreverse parsed) (nreverse types))))

(defun parse-required-param-with-type (item)
  "Parse a required parameter and extract its type.

   Returns (values parsed-item type-node).

   (x fixnum) → (values (x :type fixnum) <type-primitive fixnum>)
   x          → (values x nil)"
  (cond
    ;; Typed parameter
    ((and (consp item)
          (= (length item) 2)
          (symbolp (first item)))
     (let ((name (first item))
           (type-spec (second item)))
       (values
        (list name :type type-spec)
        (parse-type-specifier type-spec))))

    ;; Untyped parameter
    ((symbolp item)
     (values item nil))

    (t
     (type-parse-error "Invalid parameter: ~S" item))))

;;; Typed Lambda Parser

(defstruct ast-lambda-typed
  (params nil :type list)
  (param-types nil :type list)
  (return-type nil)
  (body nil :type list)
  (env nil)
  (source-location nil))

(defun parse-typed-lambda (sexp)
  "Parse a lambda with type annotations.

   (lambda ((param type) ...) return-type body)
   → ast-lambda-typed instance

   Similar to parse-typed-defun but for lambda expressions."

  (unless (and (consp sexp) (eq (car sexp) 'lambda))
    (type-parse-error "Expected lambda form, got: ~S" sexp))

  (unless (>= (length sexp) 3)
    (type-parse-error "lambda requires parameters and body"))

  (destructuring-bind (lambda-op params &rest body-and-maybe-type) sexp
    (declare (ignore lambda-op))

    ;; Determine return type
    (multiple-value-bind (return-type-spec body-forms)
        (extract-return-type-from-body body-and-maybe-type)

      ;; Parse parameters with types
      (multiple-value-bind (parsed-params param-types)
          (parse-typed-lambda-list params)

        ;; Parse return type
        (let ((return-type (extract-return-type return-type-spec)))

          ;; Create typed lambda AST
          (make-ast-lambda-typed
                         :params parsed-params
                         :param-types param-types
                         :return-type return-type
                         :body body-forms))))))

;;; Convenience Constructors

(defun make-type-function-from-spec (param-specs return-spec)
  "Create a type-function from type specifier lists.

   PARAM-SPECS: list of type specifiers for parameters
   RETURN-SPEC: return type specifier"
  (make-type-function
   (mapcar #'parse-type-specifier param-specs)
   (parse-type-specifier return-spec)))

;;; Type Specifier Unparser (for debugging/pretty-printing)

(defun unparse-type (type-node)
  "Convert a type-node back to a type specifier s-expression.

   This is the inverse of parse-type-specifier for debugging purposes."
  (cond
    ((typep type-node 'type-primitive)
     (type-primitive-name type-node))

    ((typep type-node 'type-variable)
     (if (type-variable-name type-node)
         (intern (format nil "?~A" (type-variable-name type-node)))
         (intern (format nil "?t~D" (type-variable-id type-node)))))

    ((typep type-node 'type-function)
     (let ((params (mapcar #'unparse-type (type-function-params type-node)))
           (ret (unparse-type (type-function-return type-node))))
       `(function ,params ,ret)))

    ((typep type-node 'type-tuple)
     `(values ,@(mapcar #'unparse-type (type-tuple-elements type-node))))

    ((typep type-node 'type-union)
     `(or ,@(mapcar #'unparse-type (type-union-types type-node))))

    ((typep type-node 'type-intersection)
     `(and ,@(mapcar #'unparse-type (type-intersection-types type-node))))

    ((typep type-node 'type-constructor)
     `(,(type-constructor-name type-node)
       ,@(mapcar #'unparse-type (type-constructor-args type-node))))

    ((typep type-node 'type-unknown)
     '?)

    (t
     (type-parse-error "Unknown type node: ~S" type-node))))
