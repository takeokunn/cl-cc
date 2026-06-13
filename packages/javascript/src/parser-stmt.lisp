;;;; packages/javascript/src/parser-stmt.lisp — ES2026 JavaScript Parser: Statement Layer
;;;;
;;;; Parses a JavaScript token stream (list of plists) into the common AST.
;;;; Token format: (:type :T-XXX :value val)
;;;;
;;;; Loop lowering uses the block/tagbody/go pattern (same as PHP frontend).
;;;; All loop constructs are lowered at parse time rather than emitting
;;;; symbolic :while/:for calls, because the compiler resolves the pre-built
;;;; AST directly without a macro expansion pass for frontend ASTs.

(in-package :cl-cc/javascript)

;;; ─── Token Stream Helpers ────────────────────────────────────────────────────
;;; %js-consume-expected and js-skip-semis are in parser-stmt-binding.lisp.
;;; js-tok-type, js-tok-value, js-peek*, js-consume, js-expect, js-at-eof-p,
;;; js-ident-sym, and js-try-consume are defined in parser.lisp (loaded first).

;;; ─── Dynamic Variables for Loop/Switch Control Flow ─────────────────────────

(defvar *js-loop-continue-target* nil
  "Dynamically bound innermost loop continue target tag (gensym).")

(defvar *js-loop-break-target* nil
  "Dynamically bound innermost loop/switch break target tag (gensym).")

(defvar *js-break-targets* nil
  "Stack of break targets for nested loops/switch, innermost first.")

(defvar *js-continue-targets* nil
  "Stack of continue targets for nested loops, innermost first.")

(defvar *js-label-break-targets* (make-hash-table :test #'equal)
  "Maps JS label name strings to their break target tags (for labeled break).")

(defvar *js-label-continue-targets* (make-hash-table :test #'equal)
  "Maps JS label name strings to their continue target tags (for labeled continue).")

;;; *js-strict-mode* and *js-module-mode* are defined in parser.lisp (loaded first).

;;; ─── Statement Dispatcher Table ──────────────────────────────────────────────
;;;
;;; Each define-js-stmt-parser call registers a handler for one token type.
;;; The handler receives STREAM positioned AFTER the keyword token was consumed.
;;; js-parse-stmt consults this table first before falling back to complex cases.

(defvar *js-stmt-parsers* (make-hash-table)
  "Maps token type keywords to (lambda (after-keyword-stream)) statement parsers.")

(defmacro define-js-stmt-parser (token-type (stream) &body body)
  "Register a statement parser for TOKEN-TYPE in *js-stmt-parsers*.
  STREAM is positioned AFTER the keyword token; BODY must return (values ast rest)."
  `(setf (gethash ,token-type *js-stmt-parsers*)
         (lambda (,stream) ,@body)))

;;; Simple token-type → parser registrations (consume keyword, parse body)
(define-js-stmt-parser :T-VAR      (s) (js-parse-var-decl s :var))
(define-js-stmt-parser :T-LET      (s) (js-parse-var-decl s :let))
(define-js-stmt-parser :T-CONST    (s) (js-parse-var-decl s :const))
(define-js-stmt-parser :T-FUNCTION (s) (js-parse-function-decl s))
(define-js-stmt-parser :T-CLASS    (s)
  (multiple-value-bind (ast-list rest) (js-parse-class-decl s)
    (values (if (and (consp ast-list) (= (length ast-list) 1))
                (first ast-list)
                (make-ast-progn :forms ast-list))
            rest)))
(define-js-stmt-parser :T-IMPORT   (s) (js-parse-import-decl s))
(define-js-stmt-parser :T-EXPORT   (s) (js-parse-export-decl s))
(define-js-stmt-parser :T-IF       (s) (js-parse-if-stmt s))
(define-js-stmt-parser :T-WHILE    (s) (js-parse-while-stmt s))
(define-js-stmt-parser :T-DO       (s) (js-parse-do-while-stmt s))
(define-js-stmt-parser :T-FOR      (s) (js-parse-for-stmt s))
(define-js-stmt-parser :T-SWITCH   (s) (js-parse-switch-stmt s))
(define-js-stmt-parser :T-RETURN   (s) (js-parse-return-stmt s))
(define-js-stmt-parser :T-BREAK    (s) (js-parse-break-stmt s))
(define-js-stmt-parser :T-CONTINUE (s) (js-parse-continue-stmt s))
(define-js-stmt-parser :T-THROW    (s) (js-parse-throw-stmt s))
(define-js-stmt-parser :T-TRY      (s) (js-parse-try-stmt s))
(define-js-stmt-parser :T-DEBUGGER (s) (js-parse-debugger-stmt s))

;;; ─── Loop Lowering Helpers ───────────────────────────────────────────────────

(defun %js-make-tagbody (items)
  "Build an AST-TAGBODY from a flat list of tags (symbols) and form AST nodes.
  Symbols start new tag sections; AST nodes accumulate under the current tag."
  (let ((tags nil)
        (current-tag nil)
        (current-forms nil))
    (labels ((flush ()
               (when current-tag
                 (push (cons current-tag (nreverse current-forms)) tags))))
      (dolist (item items)
        (if (symbolp item)
            (progn
              (flush)
              (setf current-tag item current-forms nil))
            (progn
              (unless current-tag
                (setf current-tag (gensym "TAGBODY-")))
              (push item current-forms))))
      (flush)
      (make-ast-tagbody :tags (nreverse tags)))))

(defun %js-lower-while-loop (cond-expr body)
  "Lower while(cond){body} to block/tagbody/go AST.
  The loop tag serves as both the continue target and the back-edge."
  (let ((loop-tag (gensym "WHILE-"))
        (end-tag  (or *js-loop-break-target* (gensym "WHILE-END-"))))
    (make-ast-block :name nil
      :body (list (%js-make-tagbody
                   (append (list loop-tag
                                 (make-ast-if
                                  :cond cond-expr
                                  :then (make-ast-quote :value nil)
                                  :else (make-ast-return-from
                                         :name nil
                                         :value (make-ast-quote :value nil))))
                           body
                           (list (make-ast-go :tag loop-tag)
                                 end-tag)))))))

(defun %js-lower-while-with-tags (cond-expr body loop-tag end-tag)
  "Lower while with caller-supplied LOOP-TAG (continue) and END-TAG (break)."
  (make-ast-block :name nil
    :body (list (%js-make-tagbody
                 (append (list loop-tag
                               (make-ast-if
                                :cond cond-expr
                                :then (make-ast-quote :value nil)
                                :else (make-ast-return-from
                                       :name nil
                                       :value (make-ast-quote :value nil))))
                         body
                         (list (make-ast-go :tag loop-tag)
                               end-tag))))))

(defun %js-lower-do-while-with-tags (cond-expr body loop-tag end-tag)
  "Lower do{body}while(cond) with caller-supplied LOOP-TAG and END-TAG."
  (make-ast-block :name nil
    :body (list (%js-make-tagbody
                 (append (list loop-tag)
                         body
                         (list (make-ast-if
                                :cond cond-expr
                                :then (make-ast-go :tag loop-tag)
                                :else (make-ast-quote :value nil))
                               end-tag))))))

(defun %js-truthy-call (expr)
  "Wrap EXPR in a %js-truthy coercion call."
  (make-ast-call :func (make-ast-var :name '%js-truthy)
                 :args (list expr)))

;;; ─── Block Parsing ───────────────────────────────────────────────────────────

(defun js-parse-block (stream)
  "Parse { stmt... } and return (values ast-progn rest).
  The returned AST is an ast-progn wrapping all collected statements."
  (with-js-parse-depth
  (let ((current (%js-consume-expected :T-LBRACE stream))
        (stmts nil))
    (loop
      (setf current (js-skip-semis current))
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RBRACE))
        (return))
      (multiple-value-bind (stmt rest) (js-parse-stmt current)
        (when stmt (push stmt stmts))
        (setf current rest)))
    (values (make-ast-progn :forms (%js-finish-let-bindings (nreverse stmts)))
            (%js-consume-expected :T-RBRACE current)))))

(defun %js-parse-stmt-body (stream)
  "Parse either a braced block or a single statement.
  Returns (values ast rest)."
  (if (eq (js-peek-type stream) :T-LBRACE)
      (js-parse-block stream)
      (js-parse-stmt stream)))

;;; ─── Variable / Binding Pattern Helpers ─────────────────────────────────────
;;; All binding helpers (%js-binding-sym, %js-parse-binding-pattern, etc.)
;;; and token helpers (%js-consume-expected, js-skip-semis) are in
;;; parser-stmt-binding.lisp (loaded before this file).

;;; ─── Variable Declaration Parsing ───────────────────────────────────────────

(defun js-parse-var-decl (stream kind)
  "Parse var/let/const declaration(s).
  KIND is :VAR :LET or :CONST.
  Handles: x, x = expr, {a,b}=obj, [a,b]=arr, and comma-separated lists.
  Returns (values ast rest)."
  (let ((current stream)
        (all-bindings nil))
    (loop
      ;; Parse one declarator
      (multiple-value-bind (binding rest) (%js-parse-binding-pattern current)
        (setf current rest)
        ;; A declarator with no initializer (`let x;') is undefined, NOT nil/false.
        ;; Binding it to nil made `typeof x' report "boolean" and `x === undefined'
        ;; false; the runtime undefined sentinel is :js-undefined.
        (let ((init-expr (make-ast-quote :value :js-undefined)))
          ;; Optional initializer: = expr
          (when (and current
                     (eq (js-peek-type current) :T-OP)
                     (equal (js-peek-value current) "="))
            (setf current (cdr current))
            ;; Use assignment-expr (NOT js-parse-expr) so the initializer stops at
            ;; the comma separating declarators — otherwise `const a = 1, b = 2`
            ;; parses `1, b = 2` as one comma-expression and drops the b binding.
            (multiple-value-bind (expr rest2) (js-parse-assignment-expr current)
              (setf init-expr expr
                    current rest2)))
          ;; Expand binding into bindings list
          (multiple-value-bind (bindings _extras)
              (%js-emit-destructure-bindings binding init-expr)
            (declare (ignore _extras))
            (setf all-bindings (append all-bindings bindings)))))
      ;; More declarators?
      (if (and current (eq (js-peek-type current) :T-COMMA))
          (setf current (cdr current))
          (return)))
    (setf current (js-skip-semis current))
    ;; A single empty-bodied let carrying all bindings. ast-let binds in PARALLEL,
    ;; but JS declarations are sequential (let*) — `let a = 1, b = a + 1' and
    ;; destructuring (a = tmp[0] must see tmp = init) both depend on it. The
    ;; block-finishing pass %js-finish-let-bindings expands a multi-binding
    ;; empty-bodied let into nested single-binding lets, giving that sequential
    ;; scoping (and scoping the bindings over the rest of the block).
    (values (make-ast-let :bindings all-bindings :body nil
                          :declarations (list kind))
            current)))


;;; Function declaration + if/while/do-while parsers -> see parser-stmt-fn.lisp
