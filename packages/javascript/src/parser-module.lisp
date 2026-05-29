;;;; packages/javascript/src/parser-module.lisp — ES2026 JavaScript module parser
;;;;
;;;; Parses ES2026 import and export declarations into the common CL-CC AST.
;;;;
;;;; Import forms supported:
;;;;   import "module"
;;;;   import name from "module"
;;;;   import * as ns from "module"
;;;;   import { a, b as c } from "module"
;;;;   import name, { a, b } from "module"
;;;;   import name, * as ns from "module"
;;;;   import ... with { type: "json" }    (ES2025 import attributes)
;;;;   import(expr)                         (dynamic import — expression level)
;;;;
;;;; Export forms supported:
;;;;   export default expr
;;;;   export default function foo() {}
;;;;   export default class Foo {}
;;;;   export { name, name as alias }
;;;;   export { name } from "module"
;;;;   export * from "module"
;;;;   export * as ns from "module"
;;;;   export const/let/var/function/class ...
;;;;
;;;; AST lowering strategy:
;;;;   All import/export statements lower to ast-call nodes whose :func is
;;;;   ast-var naming %JS-IMPORT or %JS-EXPORT.  The arguments encode the
;;;;   module specifier and the specifier list as quoted data so that the
;;;;   VM's %js-import / %js-export host-bridge functions can interpret them
;;;;   at runtime.  This mirrors how the PHP parser lowers require/include.

(in-package :cl-cc/javascript)

;;; ─── Token stream helpers (re-used from parser-class.lisp) ──────────────────
;;;
;;; js-peek, js-peek-type, js-peek-value, js-consume, js-expect, js-at-eof-p,
;;; js-skip-semis, js-ident-sym are all defined in parser-class.lisp which is
;;; loaded before this file in the ASDF serial component list.

;;; ─── Import-attribute helpers ────────────────────────────────────────────────

(defun %js-parse-import-attributes (stream)
  "Parse an ES2025 import-attributes clause:  with { key: \"value\", ... }
Returns (values attrs-alist rest) where ATTRS-ALIST is an association list
of (string . string) pairs."
  ;; Consume 'with' — represented as a plain :T-IDENT with value \"with\"
  ;; (the JS lexer emits contextual keywords as identifiers)
  (when (and stream
             (eq (js-peek-type stream) :T-IDENT)
             (equal "with" (js-peek-value stream)))
    (setf stream (cdr stream))  ; consume 'with'
    (multiple-value-bind (_ rest) (js-expect :T-LBRACE stream)
      (declare (ignore _))
      (let ((attrs nil)
            (current rest))
        (loop until (or (js-at-eof-p current)
                        (eq (js-peek-type current) :T-RBRACE))
              do ;; key: "value"
              (let ((key nil))
                ;; key can be a string or identifier
                (cond
                  ((eq (js-peek-type current) :T-STRING)
                   (multiple-value-bind (tok rest2) (js-consume current)
                     (setf key (js-tok-value tok)
                           current rest2)))
                  ((eq (js-peek-type current) :T-IDENT)
                   (multiple-value-bind (tok rest2) (js-consume current)
                     (setf key (js-tok-value tok)
                           current rest2)))
                  (t
                   (error "JS parse error: expected attribute key, got ~S"
                          (js-peek current))))
                ;; colon
                (multiple-value-bind (_ rest2) (js-expect :T-COLON current)
                  (declare (ignore _))
                  (setf current rest2))
                ;; value (string)
                (multiple-value-bind (val-tok rest2) (js-expect :T-STRING current)
                  (push (cons key (js-tok-value val-tok)) attrs)
                  (setf current rest2)))
              ;; optional trailing comma
              (when (and current (eq (js-peek-type current) :T-COMMA))
                (setf current (cdr current))))
        (multiple-value-bind (_ rest2) (js-expect :T-RBRACE current)
          (declare (ignore _))
          (return-from %js-parse-import-attributes
            (values (nreverse attrs) rest2))))))
  ;; No 'with' clause
  (values nil stream))

;;; ─── Specifier list parsers ──────────────────────────────────────────────────

(defun js-parse-import-specifiers (stream)
  "Parse a named-imports specifier list:  { a, b as c, ... }
Consumes the enclosing braces.
Returns (values specifiers rest) where SPECIFIERS is a list of plists:
  (:imported \"a\" :local \"a\")
  (:imported \"b\" :local \"c\")"
  (multiple-value-bind (_ rest) (js-expect :T-LBRACE stream)
    (declare (ignore _))
    (let ((specifiers nil)
          (current rest))
      (loop until (or (js-at-eof-p current)
                      (eq (js-peek-type current) :T-RBRACE))
            do (let ((imported nil)
                     (local nil))
                 ;; imported name: may be string or identifier or keyword
                 (cond
                   ((eq (js-peek-type current) :T-STRING)
                    (multiple-value-bind (tok rest2) (js-consume current)
                      (setf imported (js-tok-value tok)
                            current rest2)))
                   (t
                    (multiple-value-bind (tok rest2) (js-consume current)
                      (setf imported (js-tok-value tok)
                            current rest2))))
                 ;; optional: as localName
                 (if (and current
                          (eq (js-peek-type current) :T-AS))
                     (progn
                       (setf current (cdr current))  ; consume 'as'
                       (multiple-value-bind (tok rest2) (js-consume current)
                         (setf local (js-tok-value tok)
                               current rest2)))
                     (setf local imported))
                 (push (list :imported (if (stringp imported)
                                           imported
                                           (string-downcase
                                            (if (stringp imported)
                                                imported
                                                (format nil "~A" imported))))
                             :local    (if (stringp local)
                                           local
                                           (string-downcase
                                            (if (stringp local)
                                                local
                                                (format nil "~A" local)))))
                       specifiers)
                 ;; optional trailing comma
                 (when (and current (eq (js-peek-type current) :T-COMMA))
                   (setf current (cdr current)))))
      (multiple-value-bind (_ rest2) (js-expect :T-RBRACE current)
        (declare (ignore _))
        (values (nreverse specifiers) rest2)))))

(defun js-parse-export-specifiers (stream)
  "Parse a named-exports specifier list:  { a, b as c, ... }
Consumes the enclosing braces.
Returns (values specifiers rest) where SPECIFIERS is a list of plists:
  (:local \"a\" :exported \"a\")
  (:local \"b\" :exported \"c\")"
  (multiple-value-bind (_ rest) (js-expect :T-LBRACE stream)
    (declare (ignore _))
    (let ((specifiers nil)
          (current rest))
      (loop until (or (js-at-eof-p current)
                      (eq (js-peek-type current) :T-RBRACE))
            do (let ((local nil)
                     (exported nil))
                 ;; local name (may be string for module re-exports)
                 (cond
                   ((eq (js-peek-type current) :T-STRING)
                    (multiple-value-bind (tok rest2) (js-consume current)
                      (setf local (js-tok-value tok)
                            current rest2)))
                   (t
                    (multiple-value-bind (tok rest2) (js-consume current)
                      (setf local (js-tok-value tok)
                            current rest2))))
                 ;; optional: as exportedName
                 (if (and current (eq (js-peek-type current) :T-AS))
                     (progn
                       (setf current (cdr current))  ; consume 'as'
                       (multiple-value-bind (tok rest2) (js-consume current)
                         (setf exported (js-tok-value tok)
                               current rest2)))
                     (setf exported local))
                 (push (list :local    (format nil "~A" local)
                             :exported (format nil "~A" exported))
                       specifiers)
                 ;; optional trailing comma
                 (when (and current (eq (js-peek-type current) :T-COMMA))
                   (setf current (cdr current)))))
      (multiple-value-bind (_ rest2) (js-expect :T-RBRACE current)
        (declare (ignore _))
        (values (nreverse specifiers) rest2)))))

;;; ─── Import lowering helper ──────────────────────────────────────────────────

(defun %js-lower-import (module-str specifiers attrs)
  "Lower an import declaration to an ast-call for %js-import.
SPECIFIERS is a list of plists or the keywords :default, :namespace, or NIL.
ATTRS is an alist of import attributes (or NIL)."
  (make-ast-call
   :func (make-ast-var :name '%js-import)
   :args (list (make-ast-quote :value module-str)
               (make-ast-quote :value specifiers)
               (make-ast-quote :value attrs))))

;;; ─── js-parse-import-decl ────────────────────────────────────────────────────

(defun js-parse-import-decl (stream)
  "Parse all import statement forms (the 'import' keyword has already been
consumed by the caller, so STREAM points to the next token).

Forms handled:
  import \"module\"
  import name from \"module\"
  import * as ns from \"module\"
  import { a, b as c } from \"module\"
  import name, { a, b } from \"module\"
  import name, * as ns from \"module\"
  import ... with { type: \"json\" }   (ES2025 import attributes)

Lower to: (ast-call %js-import module specifiers attrs)

Returns (values ast rest)."
  (let ((current stream)
        (specifiers nil))
    (cond
      ;; import "module"  — bare side-effect import
      ((eq (js-peek-type current) :T-STRING)
       (multiple-value-bind (tok rest) (js-consume current)
         (multiple-value-bind (attrs rest2) (%js-parse-import-attributes rest)
           (values (%js-lower-import (js-tok-value tok) nil attrs)
                   (js-skip-semis rest2)))))

      ;; import * as ns from "module"
      ((and (eq (js-peek-type current) :T-OP)
            (equal "*" (js-peek-value current)))
       (setf current (cdr current))   ; consume '*'
       (multiple-value-bind (_ rest) (js-expect :T-AS current)
         (declare (ignore _))
         (multiple-value-bind (ns-tok rest2) (js-expect :T-IDENT rest)
           (multiple-value-bind (_ rest3) (js-expect :T-FROM rest2)
             (declare (ignore _))
             (multiple-value-bind (mod-tok rest4) (js-expect :T-STRING rest3)
               (multiple-value-bind (attrs rest5) (%js-parse-import-attributes rest4)
                 (let ((ns-spec (list :namespace (js-tok-value ns-tok))))
                   (values (%js-lower-import (js-tok-value mod-tok)
                                             (list ns-spec)
                                             attrs)
                           (js-skip-semis rest5)))))))))

      ;; import { a, b as c } from "module"
      ((eq (js-peek-type current) :T-LBRACE)
       (multiple-value-bind (named-specs rest) (js-parse-import-specifiers current)
         (setf specifiers named-specs
               current rest)
         (multiple-value-bind (_ rest2) (js-expect :T-FROM current)
           (declare (ignore _))
           (multiple-value-bind (mod-tok rest3) (js-expect :T-STRING rest2)
             (multiple-value-bind (attrs rest4) (%js-parse-import-attributes rest3)
               (values (%js-lower-import (js-tok-value mod-tok) specifiers attrs)
                       (js-skip-semis rest4)))))))

      ;; import name ...
      ;; Covers:
      ;;   import name from "module"
      ;;   import name, { a } from "module"
      ;;   import name, * as ns from "module"
      ((eq (js-peek-type current) :T-IDENT)
       (multiple-value-bind (default-tok rest) (js-consume current)
         (let ((default-spec (list :default (js-tok-value default-tok))))
           (setf current rest)
           (if (and current (eq (js-peek-type current) :T-COMMA))
               ;; import name, { ... } or import name, * as ns
               (progn
                 (setf current (cdr current))   ; consume ','
                 (cond
                   ;; import name, * as ns from "module"
                   ((and (eq (js-peek-type current) :T-OP)
                         (equal "*" (js-peek-value current)))
                    (setf current (cdr current))  ; consume '*'
                    (multiple-value-bind (_ rest2) (js-expect :T-AS current)
                      (declare (ignore _))
                      (multiple-value-bind (ns-tok rest3) (js-expect :T-IDENT rest2)
                        (multiple-value-bind (_ rest4) (js-expect :T-FROM rest3)
                          (declare (ignore _))
                          (multiple-value-bind (mod-tok rest5) (js-expect :T-STRING rest4)
                            (multiple-value-bind (attrs rest6)
                                (%js-parse-import-attributes rest5)
                              (values (%js-lower-import
                                       (js-tok-value mod-tok)
                                       (list default-spec
                                             (list :namespace (js-tok-value ns-tok)))
                                       attrs)
                                      (js-skip-semis rest6))))))))
                   ;; import name, { a, b } from "module"
                   ((eq (js-peek-type current) :T-LBRACE)
                    (multiple-value-bind (named-specs rest2)
                        (js-parse-import-specifiers current)
                      (multiple-value-bind (_ rest3) (js-expect :T-FROM rest2)
                        (declare (ignore _))
                        (multiple-value-bind (mod-tok rest4) (js-expect :T-STRING rest3)
                          (multiple-value-bind (attrs rest5)
                              (%js-parse-import-attributes rest4)
                            (values (%js-lower-import
                                     (js-tok-value mod-tok)
                                     (cons default-spec named-specs)
                                     attrs)
                                    (js-skip-semis rest5)))))))
                   (t
                    (error "JS parse error: expected { or * after import default and comma"))))
               ;; import name from "module"
               (progn
                 (multiple-value-bind (_ rest2) (js-expect :T-FROM current)
                   (declare (ignore _))
                   (multiple-value-bind (mod-tok rest3) (js-expect :T-STRING rest2)
                     (multiple-value-bind (attrs rest4)
                         (%js-parse-import-attributes rest3)
                       (values (%js-lower-import
                                (js-tok-value mod-tok)
                                (list default-spec)
                                attrs)
                               (js-skip-semis rest4))))))))))

      (t
       (error "JS parse error: malformed import declaration near ~S"
              (js-peek current))))))

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
