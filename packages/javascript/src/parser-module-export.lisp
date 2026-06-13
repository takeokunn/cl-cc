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

(defun %js-lower-export (kind specifiers-or-decl &optional from-module)
  "Lower an export declaration to an ast-call for %js-export.
KIND is one of :default :named :re-export :star :declaration.
SPECIFIERS-OR-DECL is the specifier list or a declaration AST node.
FROM-MODULE is the re-export source string or NIL."
  (make-ast-call
   :func (make-ast-var :name '%js-export)
   :args (list (make-ast-quote :value kind)
               (if (or (listp specifiers-or-decl) (null specifiers-or-decl))
                   (make-ast-quote :value specifiers-or-decl)
                   specifiers-or-decl)           ; AST node for declarations
               (make-ast-quote :value from-module))))

;;; ─── Forward declarations for export statement sub-parsers ──────────────────
;;;
;;; js-parse-class-decl is defined in parser-class.lisp (loaded before this).
;;; For function/variable declarations we emit a placeholder that the
;;; full parser.lisp / parser-stmt.lisp will flesh out.  The interface is
;;; that these helpers return (values ast-node rest).

(defun %js-parse-export-function-decl (stream async-p)
  "Stub: parse a function declaration in export context.
Returns (values ast-defun rest).  Consumes through the closing }."
  ;; Skip optional '*' for generators
  (when (and stream (eq (js-peek-type stream) :T-OP)
             (equal "*" (js-peek-value stream)))
    (setf stream (cdr stream)))
  ;; Optional function name
  (let ((fn-name nil)
        (current stream))
    (when (and current (eq (js-peek-type current) :T-IDENT))
      (multiple-value-bind (tok rest) (js-consume current)
        (setf fn-name (js-ident-sym (js-tok-value tok))
              current rest)))
    ;; Consume param list and body
    (multiple-value-bind (_ rest) (js-expect :T-LPAREN current)
      (declare (ignore _))
      ;; skip to matching )
      (let ((depth 1) (rest2 rest))
        (loop while (and rest2 (> depth 0))
              do (cond ((eq (js-peek-type rest2) :T-LPAREN) (incf depth))
                       ((eq (js-peek-type rest2) :T-RPAREN) (decf depth)))
              (setf rest2 (cdr rest2)))
        ;; consume {body}
        (multiple-value-bind (_ rest3) (js-expect :T-LBRACE rest2)
          (declare (ignore _))
          (let ((bdepth 1) (rest4 rest3))
            (loop while (and rest4 (> bdepth 0))
                  do (cond ((eq (js-peek-type rest4) :T-LBRACE) (incf bdepth))
                           ((eq (js-peek-type rest4) :T-RBRACE) (decf bdepth)))
                  (setf rest4 (cdr rest4)))
            (let ((defun-ast (make-ast-defun
                              :name (or fn-name (gensym "JS-FN-"))
                              :params nil
                              :body (list (make-ast-quote :value :stub))
                              :imports (when async-p (list :js-async t)))))
              (values defun-ast rest4))))))))

(defun %js-parse-export-lexical-decl (stream kind)
  "Stub: parse const/let/var bindings in export context.
KIND is :const :let or :var (the keyword token value keyword).
Returns (values ast-let rest)."
  (let ((bindings nil)
        (current stream))
    (loop
      ;; binding name
      (unless (and current (eq (js-peek-type current) :T-IDENT))
        (return))
      (multiple-value-bind (tok rest) (js-consume current)
        (let ((var-sym (js-ident-sym (js-tok-value tok)))
              (initform nil))
          (setf current rest)
          ;; optional = expr
          (when (and current (eq (js-peek-type current) :T-OP)
                     (equal "=" (js-peek-value current)))
            (setf current (cdr current))
            ;; skip expression tokens until ',' or ';' or '}'
            (let ((expr-toks nil))
              (loop while (and current
                               (not (member (js-peek-type current)
                                            '(:T-COMMA :T-SEMI :T-RBRACE))))
                    do (push (car current) expr-toks)
                       (setf current (cdr current)))
              (setf initform
                    (make-ast-call
                     :func (make-ast-var :name (js-ident-sym "%JS-EXPR"))
                     :args (mapcar (lambda (tok2)
                                     (make-ast-quote :value (js-tok-value tok2)))
                                   (nreverse expr-toks))))))
          (push (cons var-sym (or initform (make-ast-quote :value nil)))
                bindings)))
      ;; optional comma for multiple declarators
      (unless (and current (eq (js-peek-type current) :T-COMMA))
        (return))
      (setf current (cdr current)))
    (values (make-ast-let
             :bindings (nreverse bindings)
             :body nil
             :imports (list :js-lexical-kind kind))
            (js-skip-semis current))))

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
          ;; Collect expression tokens until ';' or end
          (let ((expr-toks nil)
                (depth 0))
            (loop while (and current
                             (not (and (eq (js-peek-type current) :T-SEMI)
                                       (= depth 0))))
                  do (cond ((member (js-peek-type current)
                                    '(:T-LPAREN :T-LBRACE :T-LBRACKET)) (incf depth))
                           ((member (js-peek-type current)
                                    '(:T-RPAREN :T-RBRACE :T-RBRACKET)) (decf depth)))
                  (push (car current) expr-toks)
                  (setf current (cdr current)))
            (let ((expr-ast
                   (make-ast-call
                    :func (make-ast-var :name (js-ident-sym "%JS-EXPR"))
                    :args (mapcar (lambda (tok)
                                    (make-ast-quote :value (js-tok-value tok)))
                                  (nreverse expr-toks)))))
              (values (%js-lower-export :default expr-ast)
                      (js-skip-semis current)))))))

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
       (let ((kind (js-peek-type current)))
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
