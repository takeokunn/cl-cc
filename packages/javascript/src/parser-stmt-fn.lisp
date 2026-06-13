;;;; packages/javascript/src/parser-stmt-fn.lisp — function declaration + if/while/do-while parsers
;;;;
;;;; Contains: %js-parse-param-list, %js-prepend-param-destructuring,
;;;; %js-split-params-by-defaults, %js-rest-binding, js-parse-function-decl,
;;;; js-parse-function-stmt (:T-FUNCTION dispatcher registration),
;;;; %js-parse-if-tail, js-parse-if-stmt, js-parse-while-stmt, js-parse-do-while-stmt.
;;;;
;;;; Load order: after parser-stmt.lisp (needs define-js-stmt-parser macro,
;;;; js-parse-block, js-parse-var-decl, *js-stmt-parsers*, loop-lowering helpers).

(in-package :cl-cc/javascript)

;;; ─── Function Declaration Parsing ───────────────────────────────────────────

(defun %js-parse-param-list (stream)
  "Parse (param, ...) parameter list.
  Returns (values params-list rest optionals rest-sym param-patterns), where
  OPTIONALS is an alist (sym . default-ast) for parameters with `= default',
  REST-SYM is the ...rest parameter symbol (or NIL), and PARAM-PATTERNS is an
  alist (param-gensym . binding-pattern) for destructuring parameters like
  function f([a,b]) / f({x,y}) — the caller prepends a destructuring let to the
  body that unpacks the gensym param into the pattern's names.  Callers binding
  only the first values are unaffected."
  (let ((current (%js-consume-expected :T-LPAREN stream))
        (params nil) (optionals nil) (rest-sym nil) (param-patterns nil))
    (loop
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RPAREN))
        (return))
      (cond
        ;; Rest parameter: ...name — the AST rest-param, not a positional one.
        ((eq (js-peek-type current) :T-ELLIPSIS)
         (setf current (cdr current))
         (multiple-value-bind (tok rest) (js-consume current)
           (setf rest-sym (%js-binding-sym (js-tok-value tok))
                 current rest))
         (return))                ; rest must be last
        (t
         (multiple-value-bind (binding rest) (%js-parse-binding-pattern current)
           (let ((sym (%js-binding-to-sym binding)))
             (push sym params)
             ;; A destructuring param (binding is a (:array-pattern|:object-pattern
             ;; gensym desc) list, not a bare symbol): record it so the caller can
             ;; unpack the gensym into the named locals at the top of the body.
             (when (listp binding)
               (push (cons sym binding) param-patterns))
             (setf current rest)
             ;; Default parameter value: = expr — recorded so the param becomes
             ;; an &optional with that default (was parsed and discarded).
             (when (and current
                        (eq (js-peek-type current) :T-OP)
                        (equal (js-peek-value current) "="))
               (setf current (cdr current))
               (multiple-value-bind (default-ast rest2) (js-parse-expr current)
                 (push (cons sym default-ast) optionals)
                 (setf current rest2)))))))
      (when (eq (js-peek-type current) :T-COMMA)
        (setf current (cdr current))))
    (values (nreverse params)
            (%js-consume-expected :T-RPAREN current)
            (nreverse optionals)
            rest-sym
            (nreverse param-patterns))))

(defun %js-prepend-param-destructuring (param-patterns body-stmts)
  "Prepend a destructuring let for each (param-gensym . binding-pattern) in
PARAM-PATTERNS to BODY-STMTS, so function f([a,b]){…} unpacks the gensym param
into a and b at the top of the body.  Each empty-bodied let is then nested over
the rest of the body via %js-finish-let-bindings — without that the unbodied let
fails to compile, the top-level handler-case drops the whole defun, and calls hit
'Undefined function'.  Re-running the pass is idempotent: already-bodied lets in
BODY-STMTS pass through unchanged."
  (if (null param-patterns)
      body-stmts
      (%js-finish-let-bindings
       (append
        (mapcar (lambda (pp)
                  (multiple-value-bind (bindings _extras)
                      (%js-emit-destructure-bindings (cdr pp)
                                                     (make-ast-var :name (car pp)))
                    (declare (ignore _extras))
                    (make-ast-let :bindings bindings :body nil
                                  :declarations (list :let))))
                param-patterns)
        body-stmts))))

(defun %js-split-params-by-defaults (params optionals)
  "Split PARAMS into (values required optional-entries). OPTIONALS is the
(sym . default-ast) alist. A parameter with a default — and every parameter after
it — becomes an optional entry (sym default-ast nil); JS gives a missing argument
`undefined', so a trailing parameter with no explicit default defaults to
undefined. This keeps required params as the positional prefix."
  (if (null optionals)
      (values params nil)
      (let ((first-opt (loop for p in params
                             when (assoc p optionals :test #'eq) return p)))
        (let ((required nil) (opts nil) (seen nil))
          (dolist (p params)
            (when (eq p first-opt) (setf seen t))
            (if seen
                (push (list p
                            (or (cdr (assoc p optionals :test #'eq))
                                (make-ast-quote :value cl-cc/javascript::+js-undefined+))
                            nil)
                      opts)
                (push p required)))
          (values (nreverse required) (nreverse opts))))))

(defun %js-rest-binding (rest-sym body-forms)
  "When REST-SYM is non-nil, return (values rest-param-sym wrapped-body): the AST
rest-param is a fresh gensym collecting the trailing args as a CL list, and the
body is wrapped in a let binding REST-SYM to that list converted to a JS array
(JS rest parameters are arrays). Otherwise (values nil BODY-FORMS)."
  (if rest-sym
      (let ((raw (gensym "JS-REST-")))
        (values raw
                (list (make-ast-let
                       :bindings (list (cons rest-sym
                                             (make-ast-call
                                              :func (make-ast-var :name 'cl-cc/javascript::%js-list-to-array)
                                              :args (list (make-ast-var :name raw)))))
                       :body body-forms))))
      (values nil body-forms)))

(defun js-parse-function-decl (stream &key async-p generator-p)
  "Parse function [*] name (params) { body }.
  Returns (values ast-defun rest).
  ASYNC-P and GENERATOR-P affect the :js-async / :js-generator declaration metadata."
  ;; Consume optional * for generators
  (let ((current stream))
    (when (and (eq (js-peek-type current) :T-OP)
               (equal (js-peek-value current) "*"))
      (setf generator-p t
            current (cdr current)))
    ;; Function name (optional for expressions, but required for declarations)
    (let ((fn-name nil))
      (when (eq (js-peek-type current) :T-IDENT)
        (multiple-value-bind (tok rest) (js-consume current)
          (setf fn-name (js-ident-sym (js-tok-value tok))
                current rest)))
      (multiple-value-bind (params rest optionals rest-sym param-patterns)
          (%js-parse-param-list current)
        (multiple-value-bind (body-ast rest2) (js-parse-block rest)
          (let ((body-stmts (%js-prepend-param-destructuring
                             param-patterns (ast-progn-forms body-ast)))
                (decls nil))
            (when async-p     (push :js-async     decls))
            (when generator-p (push :js-generator decls))
            (multiple-value-bind (required opts)
                (%js-split-params-by-defaults params optionals)
              (multiple-value-bind (rest-param body-forms)
                  (%js-rest-binding rest-sym (%js-callable-body body-stmts))
                ;; Generator/async function declarations: wrap body so calling the
                ;; defun returns the right runtime object.  Parameters captured by
                ;; closure so the zero-arg inner lambda sees them correctly.
                ;;   generator* f(n) { yield … }  →  body = [(%js-make-generator (lambda () …))]
                ;;   async function f(…) { … }     →  body = [(%js-async (lambda () …))]
                (let ((wrapped-body
                       (cond
                         (generator-p
                          (list (%js-call '%js-make-generator
                                         (make-ast-lambda :params nil
                                                          :body body-forms))))
                         (async-p
                          (list (%js-call '%js-async
                                         (make-ast-lambda :params nil
                                                          :body body-forms))))
                         (t body-forms))))
                  (values (make-ast-defun :name (or fn-name (gensym "JS-FN-"))
                                          :params required
                                          :optional-params opts
                                          :rest-param rest-param
                                          :declarations (nreverse decls)
                                          :body wrapped-body)
                          rest2))))))))))

;;; ─── If Statement ────────────────────────────────────────────────────────────

(defun %js-parse-if-tail (stream)
  "Parse the then-branch and optional else/else-if tail.
  Returns (values then-ast rest else-ast)."
  (multiple-value-bind (then-ast rest) (%js-parse-stmt-body stream)
    (let ((else-ast (make-ast-quote :value nil)))
      (when (and rest (eq (js-peek-type rest) :T-ELSE))
        (setf rest (cdr rest))
        (if (eq (js-peek-type rest) :T-IF)
            ;; else if — recurse
            (progn
              (setf rest (cdr rest))
              (let ((rest2 (%js-consume-expected :T-LPAREN rest)))
                (multiple-value-bind (cond-expr rest3) (js-parse-expr rest2)
                  (setf rest3 (%js-consume-expected :T-RPAREN rest3))
                  (multiple-value-bind (elseif-then rest4 elseif-else)
                      (%js-parse-if-tail rest3)
                    (setf else-ast (make-ast-if
                                    :cond (%js-truthy-call cond-expr)
                                    :then elseif-then
                                    :else elseif-else)
                          rest rest4)))))
            ;; plain else
            (multiple-value-bind (else-body rest2) (%js-parse-stmt-body rest)
              (setf else-ast else-body
                    rest rest2))))
      (values then-ast rest else-ast))))

(defun js-parse-if-stmt (stream)
  "Parse if (cond) stmt [else stmt]. Returns (values ast rest)."
  (let ((rest (%js-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (cond-expr rest2) (js-parse-expr rest)
      (setf rest2 (%js-consume-expected :T-RPAREN rest2))
      (multiple-value-bind (then-ast rest3 else-ast) (%js-parse-if-tail rest2)
        (values (make-ast-if :cond (%js-truthy-call cond-expr)
                             :then then-ast
                             :else else-ast)
                rest3)))))

;;; ─── While Statement ─────────────────────────────────────────────────────────

(defun js-parse-while-stmt (stream)
  "Parse while (cond) stmt. Returns (values ast rest)."
  (let ((rest (%js-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (cond-expr rest2) (js-parse-expr rest)
      (setf rest2 (%js-consume-expected :T-RPAREN rest2))
      (let* ((loop-tag (gensym "WHILE-"))
             (end-tag  (gensym "WHILE-END-"))
             (*js-loop-continue-target* loop-tag)
             (*js-loop-break-target*    end-tag)
             (*js-break-targets*    (cons end-tag  *js-break-targets*))
             (*js-continue-targets* (cons loop-tag *js-continue-targets*)))
        (multiple-value-bind (body-ast rest3) (%js-parse-stmt-body rest2)
          (let ((body-stmts (if (ast-progn-p body-ast)
                                (ast-progn-forms body-ast)
                                (list body-ast))))
            (values (%js-lower-while-with-tags
                     (%js-truthy-call cond-expr)
                     body-stmts
                     loop-tag end-tag)
                    rest3)))))))

;;; ─── Do-While Statement ──────────────────────────────────────────────────────

(defun js-parse-do-while-stmt (stream)
  "Parse do stmt while (cond);. Returns (values ast rest)."
  (let* ((loop-tag (gensym "DO-WHILE-"))
         (end-tag  (gensym "DO-WHILE-END-"))
         (*js-loop-continue-target* loop-tag)
         (*js-loop-break-target*    end-tag)
         (*js-break-targets*    (cons end-tag  *js-break-targets*))
         (*js-continue-targets* (cons loop-tag *js-continue-targets*)))
    (multiple-value-bind (body-ast rest) (%js-parse-stmt-body stream)
      (unless (eq (js-peek-type rest) :T-WHILE)
        (error "JS parse error: expected 'while' after do-body, got ~S" (js-peek rest)))
      (setf rest (cdr rest))           ; consume 'while'
      (setf rest (%js-consume-expected :T-LPAREN rest))
      (multiple-value-bind (cond-expr rest2) (js-parse-expr rest)
        (setf rest2 (%js-consume-expected :T-RPAREN rest2))
        (setf rest2 (js-skip-semis rest2))
        (let ((body-stmts (if (ast-progn-p body-ast)
                              (ast-progn-forms body-ast)
                              (list body-ast))))
          (values (%js-lower-do-while-with-tags
                   (%js-truthy-call cond-expr)
                   body-stmts
                   loop-tag end-tag)
                  rest2))))))

;;; ─── For Statement ───────────────────────────────────────────────────────────
;;; See parser-stmt-control.lisp for: for-loop lowering, switch, break/continue,
;;; return, throw, try/catch/finally, debugger, using, js-parse-stmt dispatcher,
;;; top-level list parsers, and public entry points parse-js-source/parse-js-module.
