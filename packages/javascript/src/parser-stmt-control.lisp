;;;; packages/javascript/src/parser-stmt-control.lisp — JS Control-Flow Statement Parser
;;;;
;;;; Contains: switch/case/default, break, continue, return, throw,
;;;; try/catch/finally, debugger, using declaration, and the main
;;;; statement dispatcher (js-parse-stmt / js-parse-stmt-list).
;;;;
;;;; Load order: after parser-stmt.lisp (loop/if/for lowering helpers,
;;;; *js-stmt-parsers* variable, and early statement registrations).

(in-package :cl-cc/javascript)

;;; ─── Switch Statement ────────────────────────────────────────────────────────

(defun %js-parse-switch-body (stream break-tag)
  "Parse switch case/default sections.
  Returns (values cases default-body rest) where cases is a list of
  (case-expr . body-stmts) conses and default-body is a list of stmts or NIL."
  (let ((current (%js-consume-expected :T-LBRACE stream))
        (cases nil)
        (default-body nil))
    (loop
      (setf current (js-skip-semis current))
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RBRACE))
        (return))
      (cond
        ;; case expr:
        ((eq (js-peek-type current) :T-CASE)
         (setf current (cdr current))
         (multiple-value-bind (case-expr rest) (js-parse-expr current)
           (setf current (%js-consume-expected :T-COLON rest))
           (let ((body nil))
             (loop
               (setf current (js-skip-semis current))
               (when (or (js-at-eof-p current)
                         (eq (js-peek-type current) :T-RBRACE)
                         (eq (js-peek-type current) :T-CASE)
                         (eq (js-peek-type current) :T-DEFAULT))
                 (return))
               (let ((*js-loop-break-target* break-tag))
                 (multiple-value-bind (stmt rest2) (js-parse-stmt current)
                   (when stmt (push stmt body))
                   (setf current rest2))))
             (push (cons case-expr (nreverse body)) cases))))
        ;; default:
        ((eq (js-peek-type current) :T-DEFAULT)
         (setf current (cdr current))
         (setf current (%js-consume-expected :T-COLON current))
         (let ((body nil))
           (loop
             (setf current (js-skip-semis current))
             (when (or (js-at-eof-p current)
                       (eq (js-peek-type current) :T-RBRACE)
                       (eq (js-peek-type current) :T-CASE))
               (return))
             (let ((*js-loop-break-target* break-tag))
               (multiple-value-bind (stmt rest2) (js-parse-stmt current)
                 (when stmt (push stmt body))
                 (setf current rest2))))
           (setf default-body (nreverse body))))
        (t
         ;; Skip unexpected token
         (setf current (cdr current)))))
    (values (nreverse cases) default-body
            (%js-consume-expected :T-RBRACE current))))

(defun %js-lower-switch (switch-expr cases default-body break-tag)
  "Lower switch(expr){case x:...; default:...} to a let/tagbody dispatch form."
  (let* ((value-sym (gensym "SWITCH-VAL-"))
         (default-tag (when default-body (gensym "SWITCH-DEFAULT-")))
         (case-labels (loop repeat (length cases) collect (gensym "SWITCH-CASE-")))
         (dispatch-forms
          (append
           (loop for case in cases
                 for label in case-labels
                 collect (make-ast-if
                          :cond (make-ast-call
                                 :func (make-ast-var :name '%js-strict-eq)
                                 :args (list (make-ast-var :name value-sym)
                                             (car case)))
                          :then (make-ast-go :tag label)
                          :else (make-ast-quote :value nil)))
           (list (make-ast-go :tag (or default-tag break-tag)))))
         (case-forms
          (loop for case in cases
                for label in case-labels
                append (list* label (cdr case))))
         (default-forms
          (when default-body
            (list* default-tag default-body))))
    (make-ast-let
     :bindings (list (cons value-sym switch-expr))
     :body (list (make-ast-block :name nil
                   :body (list (%js-make-tagbody
                                (append dispatch-forms
                                        case-forms
                                        default-forms
                                        (list break-tag)))))))))

(defun js-parse-switch-stmt (stream)
  "Parse switch(expr){case x:...; default:...}.
  Lowers to ast-let + ast-if chain via %js-lower-switch.
  Returns (values ast rest)."
  (let* ((rest (%js-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (switch-expr rest2) (js-parse-expr rest)
      (setf rest2 (%js-consume-expected :T-RPAREN rest2))
      (let* ((break-tag (gensym "SWITCH-END-"))
             (*js-loop-break-target* break-tag)
             (*js-break-targets* (cons break-tag *js-break-targets*)))
        (multiple-value-bind (cases default-body rest3)
            (%js-parse-switch-body rest2 break-tag)
          (values (%js-lower-switch switch-expr cases default-body break-tag)
                  rest3))))))

;;; ─── Break / Continue ────────────────────────────────────────────────────────

(defun js-parse-break-stmt (stream)
  "Parse break [label];. Returns (values ast rest)."
  ;; Check for labelled break (not yet supported — go to innermost break target)
  (let ((current stream)
        (target nil))
    ;; Optional label on same line
    (when (and current (eq (js-peek-type current) :T-IDENT))
      ;; Labelled break: skip label, use break target (simplified)
      (setf current (cdr current)))
    (setf current (js-skip-semis current))
    (setf target (or (first *js-break-targets*) *js-loop-break-target*))
    (unless target
      (error "JS parse error: break has no matching loop or switch"))
    (values (make-ast-go :tag target) current)))

(defun js-parse-continue-stmt (stream)
  "Parse continue [label];. Returns (values ast rest)."
  (let ((current stream)
        (target nil))
    ;; Optional label on same line
    (when (and current (eq (js-peek-type current) :T-IDENT))
      ;; Labelled continue: skip label, use continue target (simplified)
      (setf current (cdr current)))
    (setf current (js-skip-semis current))
    (setf target (or (first *js-continue-targets*) *js-loop-continue-target*))
    (unless target
      (error "JS parse error: continue has no matching loop"))
    (values (make-ast-go :tag target) current)))

;;; ─── Return Statement ────────────────────────────────────────────────────────

(defun js-parse-return-stmt (stream)
  "Parse return [expr];. Returns (values ast rest)."
  (if (or (js-at-eof-p stream)
          (eq (js-peek-type stream) :T-SEMI)
          (eq (js-peek-type stream) :T-RBRACE))
      (values (make-ast-return-from :name nil :value (make-ast-quote :value nil))
              (js-skip-semis stream))
      (multiple-value-bind (expr rest) (js-parse-expr stream)
        (values (make-ast-return-from :name nil :value expr)
                (js-skip-semis rest)))))

;;; ─── Throw Statement ─────────────────────────────────────────────────────────

(defun js-parse-throw-stmt (stream)
  "Parse throw expr; -> (values (%js-throw expr) rest)."
  (multiple-value-bind (expr rest) (js-parse-expr stream)
    (values (make-ast-call :func (make-ast-var :name '%js-throw)
                           :args (list expr))
            (js-skip-semis rest))))

;;; ─── Try / Catch / Finally ───────────────────────────────────────────────────

(defun js-parse-try-stmt (stream)
  "Parse try {} catch(e) {} finally {} .
  Lowers to ast-unwind-protect wrapping a %js-try-catch-finally call.
  Returns (values ast rest)."
  (multiple-value-bind (try-ast rest) (js-parse-block stream)
    (let ((try-body (ast-progn-forms try-ast))
          (catch-clauses nil)
          (finally-body nil)
          (finally-present nil)
          (current rest))
      ;; Parse zero or more catch clauses
      (loop while (and current (eq (js-peek-type current) :T-CATCH))
            do (setf current (cdr current))
               (let ((var-sym nil))
                 ;; Optional catch binding: catch (e)
                 (when (eq (js-peek-type current) :T-LPAREN)
                   (setf current (cdr current))
                   (when (eq (js-peek-type current) :T-IDENT)
                     (multiple-value-bind (tok rest2) (js-consume current)
                       (setf var-sym (%js-binding-sym (js-tok-value tok))
                             current rest2)))
                   (setf current (%js-consume-expected :T-RPAREN current)))
                 (multiple-value-bind (catch-ast rest2) (js-parse-block current)
                   (push (list var-sym (ast-progn-forms catch-ast)) catch-clauses)
                   (setf current rest2))))
      ;; Optional finally clause. Track PRESENCE separately: an empty `finally {}`
      ;; has a nil body but is still a valid finally clause.
      (when (and current (eq (js-peek-type current) :T-FINALLY))
        (setf current (cdr current)
              finally-present t)
        (multiple-value-bind (finally-ast rest2) (js-parse-block current)
          (setf finally-body (ast-progn-forms finally-ast)
                current rest2)))
      (unless (or catch-clauses finally-present)
        (error "JS parse error: try must have catch or finally"))
      ;; Lower to ast-unwind-protect with catch dispatch
      (let* ((err-sym (gensym "JS-ERR-"))
             (clauses (nreverse catch-clauses))
             ;; Build catch dispatch: chain of let/progn for each clause
             (catch-dispatch
              (if clauses
                  ;; For simplicity, use the first catch binding (JS has one catch)
                  (let* ((clause (first clauses))
                         (var    (first clause))
                         (body   (second clause)))
                    (if var
                        (make-ast-let :bindings (list (cons var (make-ast-var :name err-sym)))
                                      :body body)
                        (make-ast-progn :forms body)))
                  (make-ast-quote :value nil)))
             ;; Wrapped try body with catch
             (protected
              (make-ast-call
               :func (make-ast-var :name '%js-try-catch-finally)
               :args (list (make-ast-lambda :params nil :body try-body)
                           (make-ast-lambda :params (list err-sym)
                                            :body (list catch-dispatch))
                           (make-ast-lambda :params nil
                                            :body (or finally-body
                                                      (list (make-ast-quote :value nil))))))))
        (values protected current)))))

;;; ─── Debugger Statement ──────────────────────────────────────────────────────

(defun js-parse-debugger-stmt (stream)
  "Parse debugger; -> (values (%js-debugger) rest)."
  (values (make-ast-call :func (make-ast-var :name '%js-debugger)
                         :args nil)
          (js-skip-semis stream)))

;;; ─── Using Declaration (ES2025 Explicit Resource Management) ─────────────────

(defun js-parse-using-decl (stream)
  "Parse using x = expr (ES2025 explicit resource management).
  Lowers to ast-let + registration of disposable resource on scope exit.
  The disposal call is wrapped as a %js-using-register call at the binding site.
  Returns (values ast rest)."
  ;; 'using' is a contextual keyword: stream starts with the binding identifier
  (multiple-value-bind (name-tok rest) (js-expect :T-IDENT stream)
    (let ((var-sym (%js-binding-sym (js-tok-value name-tok))))
      (unless (and (eq (js-peek-type rest) :T-OP)
                   (equal (js-peek-value rest) "="))
        (error "JS parse error: expected '=' after identifier in using declaration, got ~S"
               (js-peek rest)))
      (let ((rest2 (cdr rest)))
        (multiple-value-bind (init-expr rest3) (js-parse-expr rest2)
          (values
           (make-ast-let
            :bindings (list (cons var-sym
                                  (make-ast-call
                                   :func (make-ast-var :name '%js-using-register)
                                   :args (list init-expr))))
            :declarations (list :js-using)
            :body nil)
           (js-skip-semis rest3)))))))

;;; ─── Main Statement Dispatcher ───────────────────────────────────────────────
;;;
;;; Consults *js-stmt-parsers* for simple token-type dispatch (all registered
;;; above). Complex cases requiring lookahead or multi-step processing are handled
;;; inline. Adding support for a new statement type requires only a new
;;; define-js-stmt-parser call.

(defun js-parse-stmt (stream)
  "Main statement dispatcher. Returns (values ast rest)."
  (setf stream (js-skip-semis stream))
  (when (js-at-eof-p stream)
    (return-from js-parse-stmt (values nil stream)))
  (let ((type  (js-peek-type  stream))
        (value (js-peek-value stream)))
    (cond
      ;; Braced block — not in table (no keyword to consume before block)
      ((eq type :T-LBRACE)
       (js-parse-block stream))
      ;; async function / async arrow — requires 2-token lookahead
      ((and (eq type :T-ASYNC)
            (eq (js-peek-type (cdr stream)) :T-FUNCTION))
       (js-parse-function-decl (cddr stream) :async-p t))
      ;; decorated class declaration — requires parsing decorators first
      ((eq type :T-AT)
       (multiple-value-bind (decorators rest) (%js-parse-decorators stream)
         (unless (eq (js-peek-type rest) :T-CLASS)
           (error "JS parse error: decorators must precede a class declaration"))
         (multiple-value-bind (ast-list rest2)
             (js-parse-class-decl (cdr rest) :decorators decorators)
           (values (if (and (consp ast-list) (= (length ast-list) 1))
                       (first ast-list)
                       (make-ast-progn :forms ast-list))
                   rest2))))
      ;; using x = expr (ES2025 contextual keyword) — requires ident lookahead
      ((and (eq type :T-USING)
            (eq (js-peek-type (cdr stream)) :T-IDENT))
       (js-parse-using-decl (cdr stream)))
      ;; Labelled statement: ident : stmt — requires colon lookahead
      ((and (eq type :T-IDENT)
            (eq (js-peek-type (cdr stream)) :T-COLON))
       (let* ((label-sym (js-ident-sym value))
              (rest (cddr stream)))
         (multiple-value-bind (stmt rest2) (js-parse-stmt rest)
           (values (make-ast-progn
                    :forms (list (%js-make-tagbody (list label-sym stmt))))
                   rest2))))
      ;; Table-driven dispatch: token-type → registered parser
      (t
       (let ((parser (gethash type *js-stmt-parsers*)))
         (if parser
             (funcall parser (cdr stream))    ; pass stream after keyword
             ;; Expression statement (assignments, calls, etc.)
             (multiple-value-bind (expr rest) (js-parse-expr stream)
               (values expr (js-skip-semis rest)))))))))

;;; ─── Top-Level Statement List Parser ────────────────────────────────────────

(defun %js-parse-all-stmts (stream)
  "Parse all statements until EOF.
  Returns (values ast-list rest)."
  (let ((stmts nil)
        (current stream))
    (loop
      (setf current (js-skip-semis current))
      (when (js-at-eof-p current)
        (return))
      (multiple-value-bind (stmt rest) (js-parse-stmt current)
        (when stmt (push stmt stmts))
        ;; Safety guard: a statement parser must consume at least one token.
        ;; If REST did not advance past CURRENT, signal rather than spin forever.
        (when (eq rest current)
          (error "JS parse error: no progress at ~S" (js-peek current)))
        (setf current rest)))
    (values (nreverse stmts) current)))

(defun js-parse-stmt-list (stream)
  "Parse statements until a closing } (which is consumed) or EOF.
Returns (values stmt-list rest-after-rbrace). Used for function, method, and
block bodies; STREAM is positioned just after the opening {. This is the real
statement parser that js-parse-function-body (parser-expr.lisp) dispatches to
via fboundp — previously absent, so bodies fell back to a token collector."
  (let ((stmts nil)
        (current stream))
    (loop
      (setf current (js-skip-semis current))
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RBRACE))
        (return))
      (multiple-value-bind (stmt rest) (js-parse-stmt current)
        (when stmt (push stmt stmts))
        ;; Same no-progress guard as %js-parse-all-stmts: never spin forever.
        (when (eq rest current)
          (error "JS parse error: no progress in statement list at ~S"
                 (js-peek current)))
        (setf current rest)))
    (multiple-value-bind (_ rest) (js-expect :T-RBRACE current)
      (declare (ignore _))
      (values (nreverse stmts) rest))))

;;; ─── Public Entry Points ─────────────────────────────────────────────────────

(defun parse-js-source (source &key strict-mode module-p)
  "Parse JavaScript SOURCE string into a list of top-level AST nodes.
  STRICT-MODE: enable strict mode parsing.
  MODULE-P: treat as ES module (implies strict mode, enables import/export)."
  (let ((*js-strict-mode* (or strict-mode module-p))
        (*js-module-mode* module-p))
    (let ((tokens (tokenize-js-source source)))
      (multiple-value-bind (stmts _rest) (%js-parse-all-stmts tokens)
        (declare (ignore _rest))
        stmts))))

(defun parse-js-module (source)
  "Parse JavaScript SOURCE as an ES module.
  Implies strict mode and module semantics (import/export allowed)."
  (parse-js-source source :strict-mode t :module-p t))
