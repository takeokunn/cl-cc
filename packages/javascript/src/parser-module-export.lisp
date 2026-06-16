;;;; packages/javascript/src/parser-module-export.lisp — ES2026 export declaration parser
;;;;
;;;; export default expr/function/class
;;;; export { name, name as alias }
;;;; export { name } from "module"
;;;; export * from "module"
;;;; export * as ns from "module"
;;;; export const/let/var/function/class ...
;;;;
;;;; Needs: %js-lower-import, js-parse-import-specifiers (parser-module.lisp),
;;;;        js-parse-function-stmt, js-parse-class-decl.
;;;; Load order: after parser-module.lisp.

(in-package :cl-cc/javascript)

;;; ─── Export lowering helper ──────────────────────────────────────────────────

(defun %js-export-symbol-name (sym)
  "Return the JS source name represented by parser symbol SYM."
  (when sym
    (symbol-name sym)))

(defun %js-export-public-binding-symbol-p (sym)
  "True when SYM is a source-level JS binding, not a destructuring temp."
  (and (symbolp sym)
       (symbol-package sym)))

(defun %js-export-let-binding-names (decl)
  "Collect source-level binding names from a declaration let AST."
  (remove-duplicates
   (remove nil
           (mapcar (lambda (binding)
                     (let ((sym (car binding)))
                       (when (%js-export-public-binding-symbol-p sym)
                         (%js-export-symbol-name sym))))
                   (ast-let-bindings decl))
           :test #'equal)
   :test #'equal
   :from-end t))

(defun %js-export-declaration-names (decl)
  "Collect the public export names introduced by declaration AST node DECL."
  (cond
    ((ast-defun-p decl)
     (let ((name (%js-export-symbol-name (ast-defun-name decl))))
       (and name (list name))))
    ((ast-defclass-p decl)
     (let ((name (%js-export-symbol-name (ast-defclass-name decl))))
       (and name (list name))))
    ((ast-let-p decl)
     (%js-export-let-binding-names decl))
    (t nil)))

(defun %js-lower-export (kind specifiers-or-decl &optional from-module)
  "Lower an export declaration to an ast-call for %js-export.
KIND is one of :default :named :re-export :star :declaration.
SPECIFIERS-OR-DECL is the specifier list or a declaration AST node.
FROM-MODULE is the re-export source string or NIL."
  (let ((declaration-names (and (eq kind :declaration)
                                (%js-export-declaration-names specifiers-or-decl))))
    (make-ast-call
     :func (make-ast-var :name '%js-export)
     :args (list (make-ast-quote :value kind)
                 (if (or (listp specifiers-or-decl) (null specifiers-or-decl))
                     (make-ast-quote :value specifiers-or-decl)
                     specifiers-or-decl)           ; AST node for declarations
                 (make-ast-quote :value from-module)
                 (make-ast-quote :value declaration-names)))))

;;; ─── Export statement sub-parsers ───────────────────────────────────────────
;;;
;;; Exported declarations reuse the ordinary statement parsers so function
;;; bodies, parameters, destructuring bindings, and initializer expressions keep
;;; the same AST shape as non-exported declarations.

(defun %js-parse-export-function-decl (stream async-p)
  "Parse a function declaration in export context."
  (js-parse-function-decl stream :async-p async-p))

(defun %js-parse-export-lexical-decl (stream kind)
  "Parse const/let/var bindings in export context."
  (js-parse-var-decl stream kind))

;;; ─── js-parse-export-decl ────────────────────────────────────────────────────

(defun js-parse-export-decl (stream)
  "Parse all export statement forms (the 'export' keyword has already been
consumed by the caller, so STREAM points to the next token).

Forms handled:
  export default expr
  export default function foo() {}
  export default class Foo {}
  export { name, name as alias }
  export { name } from \"module\"
  export * from \"module\"
  export * as ns from \"module\"
  export const/let/var name [= expr]
  export function name() {}
  export async function name() {}
  export class Name {}

Lower to: (ast-call %js-export kind specifiers-or-decl from-module)

Returns (values ast rest)."
  (let ((current stream))
    (cond

      ;; export default ...
      ((eq (js-peek-type current) :T-DEFAULT)
       (setf current (cdr current))  ; consume 'default'
       (cond
         ;; export default function [name]() {}
         ((eq (js-peek-type current) :T-FUNCTION)
          (setf current (cdr current))  ; consume 'function'
          (multiple-value-bind (defun-ast rest)
              (%js-parse-export-function-decl current nil)
            (values (%js-lower-export :default defun-ast) rest)))
         ;; export default async function [name]() {}
         ((and (eq (js-peek-type current) :T-ASYNC)
               (cdr current)
               (eq (js-peek-type (cdr current)) :T-FUNCTION))
          (setf current (cddr current))  ; consume 'async' 'function'
          (multiple-value-bind (defun-ast rest)
              (%js-parse-export-function-decl current t)
            (values (%js-lower-export :default defun-ast) rest)))
         ;; export default class [Name] {}
         ((eq (js-peek-type current) :T-CLASS)
          (setf current (cdr current))  ; consume 'class'
          (multiple-value-bind (class-nodes rest)
              (js-parse-class-decl current :expression-p t)
            (values (%js-lower-export :default (first class-nodes)) rest)))
         ;; export default expr;
         (t
          (multiple-value-bind (expr-ast rest) (js-parse-expr current)
            (values (%js-lower-export :default expr-ast)
                    (js-skip-semis rest))))))

      ;; export * from "module"
      ;; export * as ns from "module"
      ((and (eq (js-peek-type current) :T-OP)
            (equal "*" (js-peek-value current)))
       (setf current (cdr current))   ; consume '*'
       (let ((ns-name nil))
         ;; optional: as ns
         (when (and current (eq (js-peek-type current) :T-AS))
           (setf current (cdr current))  ; consume 'as'
           (multiple-value-bind (ns-tok rest) (js-expect :T-IDENT current)
             (setf ns-name (js-tok-value ns-tok)
                   current rest)))
         (multiple-value-bind (_ rest) (js-expect :T-FROM current)
           (declare (ignore _))
           (multiple-value-bind (mod-tok rest2) (js-expect :T-STRING rest)
             (values (%js-lower-export :star
                                       (when ns-name (list :namespace ns-name))
                                       (js-tok-value mod-tok))
                     (js-skip-semis rest2))))))

      ;; export { a, b as c } [from "module"]
      ((eq (js-peek-type current) :T-LBRACE)
       (multiple-value-bind (specs rest) (js-parse-export-specifiers current)
         (if (and rest (eq (js-peek-type rest) :T-FROM))
             ;; re-export
             (progn
               (setf rest (cdr rest))  ; consume 'from'
               (multiple-value-bind (mod-tok rest2) (js-expect :T-STRING rest)
                 (values (%js-lower-export :re-export specs (js-tok-value mod-tok))
                         (js-skip-semis rest2))))
             ;; local re-export
             (values (%js-lower-export :named specs)
                     (js-skip-semis rest)))))

      ;; export const/let/var ...
      ((member (js-peek-type current) '(:T-CONST :T-LET :T-VAR))
       (let ((kind (case (js-peek-type current)
                     (:T-CONST :const)
                     (:T-LET :let)
                     (:T-VAR :var))))
         (setf current (cdr current))   ; consume keyword
         (multiple-value-bind (decl-ast rest)
             (%js-parse-export-lexical-decl current kind)
           (values (%js-lower-export :declaration decl-ast) rest))))

      ;; export function name() {}
      ((eq (js-peek-type current) :T-FUNCTION)
       (setf current (cdr current))  ; consume 'function'
       (multiple-value-bind (defun-ast rest)
           (%js-parse-export-function-decl current nil)
         (values (%js-lower-export :declaration defun-ast) rest)))

      ;; export async function name() {}
      ((and (eq (js-peek-type current) :T-ASYNC)
            (cdr current)
            (eq (js-peek-type (cdr current)) :T-FUNCTION))
       (setf current (cddr current))  ; consume 'async' 'function'
       (multiple-value-bind (defun-ast rest)
           (%js-parse-export-function-decl current t)
         (values (%js-lower-export :declaration defun-ast) rest)))

      ;; export class Name {}
      ((eq (js-peek-type current) :T-CLASS)
       (setf current (cdr current))  ; consume 'class'
       (multiple-value-bind (class-nodes rest)
           (js-parse-class-decl current :expression-p nil)
         (values (%js-lower-export :declaration (first class-nodes)) rest)))

      (t
       (error "JS parse error: malformed export declaration near ~S"
              (js-peek current))))))
