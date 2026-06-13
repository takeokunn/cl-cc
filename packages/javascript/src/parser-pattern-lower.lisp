;;;; packages/javascript/src/parser-pattern-lower.lisp — Destructuring Pattern AST Lowering
;;;;
;;;; Converts js-binding-pattern structs (produced by parser-pattern.lisp) into
;;;; nested ast-let trees for use by the compiler backend.
;;;;
;;;; Central entry point: js-lower-binding-pattern
;;;;
;;;; For most callers, the lower-level %js-build-pattern-let is simpler:
;;;;   (%js-build-pattern-let pattern value-expr body-forms)
;;;; which threads body-forms through the entire tree without a sentinel.
;;;;
;;;; Load order: after parser-pattern.lisp (needs js-binding-pattern struct),
;;;;             before parser-stmt.lisp (needs js-lower-binding-pattern).

(in-package :cl-cc/javascript)

;;; Forward declaration — %js-build-pattern-let is defined below but called by
;;; %js-lower-element / %js-lower-property which are defined first.
(declaim (ftype (function (js-binding-pattern t list) list) %js-build-pattern-let))

(defun %js-make-get-prop (tmp-sym key-expr)
  "Build (%js-get-prop tmp key) as an ast-call."
  (make-ast-call
   :func (make-ast-var :name '%js-get-prop)
   :args (list (make-ast-var :name tmp-sym) key-expr)))

(defun %js-wrap-default (access-expr default-expr)
  "Wrap ACCESS-EXPR in a default guard: if the value is undefined use DEFAULT-EXPR.
  Lowers to:
    (let ((#:v access-expr))
      (if (eq #:v undefined) default-expr #:v))"
  (if (null default-expr)
      access-expr
      (let ((v-sym (gensym "JSPAT-V-")))
        (make-ast-let
         :bindings (list (cons v-sym access-expr))
         :body (list (make-ast-if
                      :cond (make-ast-call
                             :func (make-ast-var :name '%js-is-undefined)
                             :args (list (make-ast-var :name v-sym)))
                      :then default-expr
                      :else (make-ast-var :name v-sym)))))))

(defun %js-lower-element (element tmp-sym index body)
  "Lower a single array-pattern element into a let-wrapped BODY.

  ELEMENT is (pattern-or-nil default-or-nil rest-p).
  TMP-SYM is the symbol holding the array value.
  INDEX is the integer index of this element.
  BODY is a list of AST forms that become the inner body of the new let.

  Returns a new body (a list of forms) with the element's bindings
  wrapping the old BODY."
  (destructuring-bind (pat default rest-p) element
    (declare (ignore rest-p))
    (when (null pat)
      ;; Elision hole — skip, return body unchanged
      (return-from %js-lower-element body))
    (let* ((key-expr  (make-ast-int :value index))
           (raw-expr  (%js-make-get-prop tmp-sym key-expr))
           (val-expr  (%js-wrap-default raw-expr default)))
      (if (eq (js-binding-pattern-kind pat) :ident)
          ;; Simple name binding: (let ((name val)) body)
          (list (make-ast-let
                 :bindings (list (cons (js-binding-pattern-name pat) val-expr))
                 :body body))
          ;; Nested pattern: bind the element value to a fresh tmp,
          ;; then lower the nested pattern against that tmp with BODY as leaf.
          (let* ((nested-tmp (gensym "JSPAT-ARR-"))
                 (nested-form (%js-build-pattern-let pat (make-ast-var :name nested-tmp) body)))
            (list (make-ast-let
                   :bindings (list (cons nested-tmp val-expr))
                   :body nested-form)))))))

(defun %js-lower-property (property tmp-sym body)
  "Lower a single object-pattern property into a let-wrapped BODY.

  PROPERTY is (key-string local-pattern default-or-nil rest-p).
  TMP-SYM is the symbol holding the object value.
  BODY is a list of AST forms for the inner body.

  Returns a new body (a list of forms) with the property's bindings
  wrapping the old BODY."
  (destructuring-bind (key-str local-pat default rest-p) property
    (declare (ignore rest-p))
    (let* ((key-expr (make-ast-quote :value key-str))
           (raw-expr (%js-make-get-prop tmp-sym key-expr))
           (val-expr (%js-wrap-default raw-expr default)))
      (if (eq (js-binding-pattern-kind local-pat) :ident)
          ;; Simple rename/shorthand binding
          (list (make-ast-let
                 :bindings (list (cons (js-binding-pattern-name local-pat) val-expr))
                 :body body))
          ;; Nested pattern: same threading strategy as %js-lower-element
          (let* ((nested-tmp  (gensym "JSPAT-OBJ-"))
                 (nested-form (%js-build-pattern-let local-pat (make-ast-var :name nested-tmp) body)))
            (list (make-ast-let
                   :bindings (list (cons nested-tmp val-expr))
                   :body nested-form)))))))

(defun %js-build-pattern-let (pattern value-expr body)
  "Build an ast-let tree for PATTERN bound to VALUE-EXPR with BODY as the leaf.

  PATTERN    — a js-binding-pattern.
  VALUE-EXPR — AST node for the right-hand side value.
  BODY       — list of AST forms to place at the innermost position.

  Returns a list of AST forms (the outermost ast-let wrapping everything down
  to BODY)."
  (let ((kind (js-binding-pattern-kind pattern)))
    (cond
      ;; Simple identifier: one let binding
      ((eq kind :ident)
       (list (make-ast-let
              :bindings (list (cons (js-binding-pattern-name pattern) value-expr))
              :body body)))
      ;; Array pattern: bind tmp, then layer elements right-to-left
      ((eq kind :array)
       (let* ((tmp-sym  (gensym "JSPAT-T-"))
              (elements (js-binding-pattern-elements pattern))
              (rest-pat (js-binding-pattern-rest pattern))
              (n        (length elements))
              (inner-body
               (if rest-pat
                   (let* ((rest-key (make-ast-call
                                     :func (make-ast-var :name '%js-array-slice)
                                     :args (list (make-ast-var :name tmp-sym)
                                                 (make-ast-int :value n)))))
                     (if (eq (js-binding-pattern-kind rest-pat) :ident)
                         (list (make-ast-let
                                :bindings (list (cons (js-binding-pattern-name rest-pat) rest-key))
                                :body body))
                         (%js-build-pattern-let rest-pat rest-key body)))
                   body)))
         (let ((layered-body inner-body))
           (loop for i from (1- n) downto 0
                 for elem = (nth i elements)
                 do (setf layered-body
                          (%js-lower-element elem tmp-sym i layered-body)))
           (list (make-ast-let
                  :bindings (list (cons tmp-sym value-expr))
                  :body layered-body)))))
      ;; Object pattern: bind tmp, then layer properties right-to-left
      ((eq kind :object)
       (let* ((tmp-sym    (gensym "JSPAT-T-"))
              (properties (js-binding-pattern-properties pattern))
              (rest-pat   (js-binding-pattern-rest pattern))
              (inner-body
               (if rest-pat
                   (let* ((used-keys (mapcar #'car properties))
                          (rest-expr (make-ast-call
                                      :func (make-ast-var :name '%js-object-rest)
                                      :args (list (make-ast-var :name tmp-sym)
                                                  (make-ast-quote :value used-keys)))))
                     (if (eq (js-binding-pattern-kind rest-pat) :ident)
                         (list (make-ast-let
                                :bindings (list (cons (js-binding-pattern-name rest-pat) rest-expr))
                                :body body))
                         (%js-build-pattern-let rest-pat rest-expr body)))
                   body)))
         (let ((layered-body inner-body))
           (loop for i from (1- (length properties)) downto 0
                 for prop = (nth i properties)
                 do (setf layered-body
                          (%js-lower-property prop tmp-sym layered-body)))
           (list (make-ast-let
                  :bindings (list (cons tmp-sym value-expr))
                  :body layered-body)))))
      (t
       (error "%js-build-pattern-let: unknown pattern kind ~S" kind)))))

(defun js-lower-binding-pattern (pattern value-expr)
  "Lower PATTERN into a bindings-alist and an inner-body list for ast-let.

  PATTERN    — a js-binding-pattern struct.
  VALUE-EXPR — an AST node for the right-hand side value.

  Returns (values bindings-alist inner-body) where:

    For :ident patterns:
      bindings-alist = ((name . value-expr))
      inner-body     = NIL
      Usage: (make-ast-let :bindings bindings-alist :body real-body)

    For :array / :object patterns:
      bindings-alist = ((tmp-sym . value-expr))   — single outer binding
      inner-body     = list of ast-let nodes with a sentinel leaf
                         (ast-quote :value :__pattern-body__)
      The sentinel marks where the caller should attach real body forms.

  For most uses, the higher-level helper %js-build-pattern-let is simpler:
    (%js-build-pattern-let pattern value-expr body-forms)
  which threads body-forms through the entire tree without a sentinel."
  (let ((kind (js-binding-pattern-kind pattern)))
    (cond
      ;; Identity pattern: direct binding
      ((eq kind :ident)
       (values (list (cons (js-binding-pattern-name pattern) value-expr))
               nil))

      ;; Array / object: build the nested let tree and expose tmp binding
      ((member kind '(:array :object) :test #'eq)
       (let* ((sentinel (list (make-ast-quote :value :__pattern-body__)))
              (tree     (%js-build-pattern-let pattern value-expr sentinel))
              (outer-let (first tree)))
         (values (ast-let-bindings outer-let)
                 (ast-let-body    outer-let))))

      (t
       (error "js-lower-binding-pattern: unknown pattern kind ~S" kind)))))
