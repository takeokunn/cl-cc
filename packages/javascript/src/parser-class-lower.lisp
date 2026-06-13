;;;; packages/javascript/src/parser-class-lower.lisp — JS class AST lowering
;;;;
;;;; Extracted from parser-class.lisp: the %js-lower-class-to-ast family and
;;;; the public js-parse-class-decl entry point.
;;;; Load order: after parser-class.lisp (needs %js-parse-class-body).

(in-package :cl-cc/javascript)

;;; ─── %js-lower-class-to-ast helpers ─────────────────────────────────────────

;;; Accessor shorthands — ast-slot-def uses (:conc-name ast-slot-) so slots
;;; are read as ast-slot-NAME, but :imports is inherited from ast-node which
;;; uses (:conc-name ast-) and is therefore read as ast-imports.

(defun %js-slot-method-p (slot)
  "True when SLOT is an instance method (initform is ast-defun/lambda, not a field)."
  (and slot
       (ast-slot-def-p slot)
       (or (ast-defun-p  (ast-slot-initform slot))
           (ast-lambda-p (ast-slot-initform slot)))
       (not (member :field (ast-imports slot)))))

(defun %js-slot-to-method-lambda (slot)
  "Convert a method slot's initform to an ast-lambda for %js-make-class."
  (let ((fn (ast-slot-initform slot)))
    (cond
      ((ast-lambda-p fn) fn)
      ((ast-defun-p fn)
       (make-ast-lambda :params (ast-defun-params fn)
                        :body   (ast-defun-body  fn)))
      (t nil))))

(defun %js-super-ref (super-expr)
  "A FRESH reference to SUPER-EXPR for embedding inside a method/constructor body,
so the super-class AST node is not shared between the %js-make-class call and each
wrapped body."
  (if (ast-var-p super-expr)
      (make-ast-var :name (ast-var-name super-expr))
      super-expr))

(defun %js-wrap-method-super (lambda-ast super-expr)
  "When the class has a SUPER-EXPR, wrap LAMBDA-AST's body in a let binding %js-super
to a super-binding built from the (lexical) super class and the current this, so
super(args) calls the parent constructor.  Mutates LAMBDA-AST in place (preserving
its params/optional/rest) and returns it."
  (when (and lambda-ast (ast-lambda-p lambda-ast) super-expr)
    (setf (ast-lambda-body lambda-ast)
          (list (make-ast-let
                 :bindings (list (cons '%js-super
                                       (%js-call '%js-make-super-binding
                                                 (%js-super-ref super-expr)
                                                 (make-ast-var :name '%js-this))))
                 :declarations (list :let)
                 :body (ast-lambda-body lambda-ast)))))
  lambda-ast)

(defun %js-class-member-key (slot orig-name)
  "Prototype/class key for a class member.  A get/set accessor is stored under
__get_NAME / __set_NAME so %js-get-prop / %js-set-prop dispatch it as an
accessor (invoke on read/write); a regular method keeps its plain name.  Without
this, `get v()' was stored as a plain prototype method, so `obj.v' returned the
getter FUNCTION instead of its result."
  (case (getf (ast-imports slot) :js-member-kind)
    (:getter (concatenate 'string "__get_" orig-name))
    (:setter (concatenate 'string "__set_" orig-name))
    (t orig-name)))

;;; ─── %js-lower-class-to-ast ──────────────────────────────────────────────────

(defun %js-lower-class-to-ast (name-sym super-expr members decorators)
  "Lower a parsed class to a prototype-based JS class using %js-make-class.
Emits: (defvar ClassName (%js-make-class SuperOrNil CtorOrNil 'method1 fn1 ...))

NAME-SYM   — symbol or NIL for anonymous class expressions
SUPER-EXPR — AST node for the superclass or NIL
MEMBERS    — list of ast-slot-def nodes from %js-parse-class-body
DECORATORS — list of AST nodes for class-level decorators"
  (declare (ignore decorators))
  (let* ((effective-name (or name-sym (gensym "JS-CLASS-")))
         ;; Find constructor (slot whose orig-name is \"constructor\")
         (ctor-slot (find-if (lambda (s)
                               (and (ast-slot-def-p s)
                                    (let ((n (ast-slot-name s)))
                                      (and n (string-equal (symbol-name n) "CONSTRUCTOR")))))
                             members))
         ;; Instance methods (non-constructor, non-static, non-field)
         (method-slots (remove-if (lambda (s)
                                    (or (not (%js-slot-method-p s))
                                        (getf (ast-imports s) :js-static)
                                        (and (ast-slot-name s)
                                             (string-equal (symbol-name (ast-slot-name s))
                                                           "CONSTRUCTOR"))))
                                  members))
         ;; Static methods
         (static-slots (remove-if-not (lambda (s)
                                        (and (%js-slot-method-p s)
                                             (getf (ast-imports s) :js-static)))
                                      members))
         ;; Static fields: `static x = init;' — set once on the class object.
         (static-field-slots (remove-if-not (lambda (s)
                                              (and (not (%js-slot-method-p s))
                                                   (getf (ast-imports s) :js-static)
                                                   (eq (getf (ast-imports s) :js-member-kind) :field)))
                                            members))
         ;; Instance fields (non-method, non-static): `x = init;' / `x;'.  These
         ;; initialize on every instance BEFORE the constructor body; lower each to
         ;; (%js-set-prop this "x" init) and prepend to the constructor.
         (field-slots (remove-if (lambda (s)
                                   (or (%js-slot-method-p s)
                                       (getf (ast-imports s) :js-static)
                                       (not (eq (getf (ast-imports s) :js-member-kind) :field))))
                                 members))
         (field-inits
          (loop for slot in field-slots
                for orig-name = (or (getf (ast-imports slot) :js-orig-name)
                                    (let ((n (ast-slot-name slot)))
                                      (if n (string-downcase (symbol-name n)) "")))
                collect (%js-call '%js-set-prop
                                  (make-ast-var :name '%js-this)
                                  (make-ast-quote :value orig-name)
                                  (or (ast-slot-initform slot)
                                      (make-ast-quote :value +js-undefined+)))))
         ;; Constructor lambda: explicit ctor gets field inits prepended; a class
         ;; with fields but no explicit ctor gets a synthetic one (forwarding to
         ;; super for a derived class so the parent still initializes).
         (ctor-lambda
          (cond
            (ctor-slot
             (let ((lam (%js-slot-to-method-lambda ctor-slot)))
               (when field-inits
                 (setf (ast-lambda-body lam) (append field-inits (ast-lambda-body lam))))
               (%js-wrap-method-super lam super-expr)))
            (field-inits
             (make-ast-lambda
              :params nil
              :rest-param (when super-expr 'js-ctor-rest-args)
              :body (append
                     (when super-expr
                       (list (%js-call '%js-run-constructor
                                       (%js-super-ref super-expr)
                                       (make-ast-var :name '%js-this)
                                       (make-ast-var :name 'js-ctor-rest-args))))
                     field-inits)))
            (t (make-ast-quote :value nil))))
         ;; Instance method args: ("name1" fn1 "name2" fn2 ...)
         (method-args
          (loop for slot in method-slots
                for orig-name = (or (getf (ast-imports slot) :js-orig-name)
                                    (let ((n (ast-slot-name slot)))
                                      (if n (string-downcase (symbol-name n)) "")))
                for fn = (%js-wrap-method-super (%js-slot-to-method-lambda slot) super-expr)
                when fn
                  append (list (make-ast-quote :value (%js-class-member-key slot orig-name)) fn)))
         ;; Static method args, preceded by the "@@static" marker that
         ;; %js-make-class splits on to set them on the class object itself.
         (static-args
          (loop for slot in static-slots
                for orig-name = (or (getf (ast-imports slot) :js-orig-name)
                                    (let ((n (ast-slot-name slot)))
                                      (if n (string-downcase (symbol-name n)) "")))
                for fn = (%js-slot-to-method-lambda slot)
                when fn
                  append (list (make-ast-quote :value (%js-class-member-key slot orig-name)) fn)))
         ;; Static field args (name value …): set on the class object alongside
         ;; the static methods, via the same "@@static" marker.
         (static-field-args
          (loop for slot in static-field-slots
                for orig-name = (or (getf (ast-imports slot) :js-orig-name)
                                    (let ((n (ast-slot-name slot)))
                                      (if n (string-downcase (symbol-name n)) "")))
                append (list (make-ast-quote :value orig-name)
                             (or (ast-slot-initform slot)
                                 (make-ast-quote :value +js-undefined+)))))
         ;; %js-make-class call: super ctor inst-methods... [@@static static-methods...]
         (make-class-call
          (make-ast-call
           :func (make-ast-var :name '%js-make-class)
           :args (list* (or super-expr (make-ast-quote :value nil))
                        ctor-lambda
                        (append method-args
                                (when (or static-args static-field-args)
                                  (cons (make-ast-quote :value "@@static")
                                        (append static-args static-field-args)))))))
         ;; Single primary ast-defclass node — carries full semantic info for
         ;; tools/tests AND encodes the runtime implementation. :php-kind
         ;; :javascript tells codegen-clos to compile the :metaclass make-class-call
         ;; (which now includes statics, so no separate set-prop bindings are
         ;; needed and the lowering yields exactly one top-level node).
         (class-node
          (make-ast-defclass
           :name effective-name
           :superclasses (when (and super-expr (ast-var-p super-expr))
                           (list (ast-var-name super-expr)))
           :slots members
           :php-kind :javascript
           :metaclass make-class-call)))
    (list class-node)))

;;; ─── js-parse-class-decl (public entry point) ───────────────────────────────

(defun js-parse-class-decl (stream &key expression-p decorators)
  "Parse class [Name] [extends Super] { body }.

EXPRESSION-P — when T, the class keyword was consumed as part of an expression
               context; NAME is optional.
DECORATORS   — list of decorator AST nodes collected before the class keyword.

Returns (values ast-list rest) where AST-LIST is a list of AST nodes produced
by %js-lower-class-to-ast."
  ;; The 'class' keyword has already been consumed by the caller.
  (let ((current stream)
        (class-name nil)
        (super-expr nil))
    ;; Optional class name: present in declarations, optional in expressions
    (when (eq (js-peek-type current) :T-IDENT)
      (multiple-value-bind (tok rest) (js-consume current)
        (setf class-name (js-ident-sym (js-tok-value tok))
              current rest)))
    (when (and (not expression-p) (null class-name))
      (error "JS parse error: class declaration requires a name"))
    ;; Optional: extends SuperClass
    (when (and current (eq (js-peek-type current) :T-EXTENDS))
      (setf current (cdr current))   ; consume 'extends'
      ;; Parse super-class reference (simple identifier or member expression)
      (multiple-value-bind (tok rest) (js-expect :T-IDENT current)
        (setf super-expr (make-ast-var :name (js-ident-sym (js-tok-value tok)))
              current rest)
        ;; Walk optional .Member access on super expression
        (loop while (and current (eq (js-peek-type current) :T-DOT))
              do (setf current (cdr current))   ; consume '.'
              (multiple-value-bind (mem-tok rest2) (js-expect :T-IDENT current)
                (setf super-expr
                      (make-ast-call
                       :func (make-ast-var :name (js-ident-sym "%JS-GET-PROP"))
                       :args (list super-expr
                                   (make-ast-quote
                                    :value (js-tok-value mem-tok))))
                      current rest2)))))
    ;; Class body
    (multiple-value-bind (members rest) (%js-parse-class-body current class-name)
      (values (%js-lower-class-to-ast class-name super-expr members decorators)
              rest))))
