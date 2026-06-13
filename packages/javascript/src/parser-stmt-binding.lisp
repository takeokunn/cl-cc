;;;; packages/javascript/src/parser-stmt-binding.lisp — JS destructuring & binding helpers
;;;;
;;;; Extracted from parser-stmt.lisp to isolate the binding/destructuring machinery.
;;;; Loaded before parser-stmt.lisp so these helpers are available to all subsequent
;;;; parser files (parser-stmt, parser-stmt-control, etc.).

(in-package :cl-cc/javascript)

;;; ─── Token Stream Helpers (needed by binding parsers) ────────────────────────
;;; js-peek*, js-consume, js-expect, js-at-eof-p are in parser.lisp (loads first).

(defun %js-consume-expected (type stream)
  "Like js-expect but returns only the rest stream (discards the token)."
  (nth-value 1 (js-expect type stream)))

(defun js-skip-semis (stream)
  "Skip zero or more semicolons in a loop (unlike js-skip-semi which skips one)."
  (loop while (and stream (eq (js-peek-type stream) :T-SEMI))
        do (setf stream (cdr stream)))
  stream)

;;; ─── Variable / Binding Pattern Helpers ─────────────────────────────────────

(defun %js-binding-sym (name)
  "Intern a JS binding name (param, let/const/var, destructured name) as a symbol
in :cl-cc/javascript — using the SAME scheme as js-ident-sym so a binding and its
references resolve to the identical symbol.

Previously this prefixed names with `JS.', so a parameter or local bound the
symbol JS.X while the body referenced X (via js-ident-sym). Every `return param'
or `console.log(localVar)' then hit an unbound variable, the enclosing function
body failed to compile, the top-level handler-case silently dropped the defun,
and calls reported `Undefined function'. The package already namespaces JS
symbols, so the prefix was redundant as well as desynchronized.

Preserves CASE — must use the IDENTICAL scheme as js-ident-sym (JavaScript is
case-sensitive), so a binding and its references resolve to the same symbol."
  (intern (if (stringp name) name (symbol-name name))
          :cl-cc/javascript))

(defun %js-parse-pattern-default (stream)
  "If STREAM begins with `= expr`, consume it and return (values default-ast rest);
otherwise return (values nil stream). Used for destructuring defaults like
[a = 1] and {a = 1}."
  (if (and stream
           (eq (js-peek-type stream) :T-OP)
           (equal (js-peek-value stream) "="))
      (multiple-value-bind (_tok rest) (js-consume stream)
        (declare (ignore _tok))
        (js-parse-assignment-expr rest))
      (values nil stream)))

(defun %js-default-access (access-expr default-ast)
  "Wrap ACCESS-EXPR so it yields DEFAULT-AST when the access is undefined,
matching JS destructuring-default semantics. Returns ACCESS-EXPR unchanged when
DEFAULT-AST is nil."
  (if default-ast
      (make-ast-if
       :cond (make-ast-call
              :func (make-ast-var :name '%js-strict-eq)
              :args (list access-expr (make-ast-quote :value :js-undefined)))
       :then default-ast
       :else access-expr)
      access-expr))

(defun %js-parse-binding-pattern (stream)
  "Parse a destructuring pattern or simple identifier.
  Returns (values sym/pattern rest).
  Handles: ident, {a,b,...}, [a,b,...] — simplified to %js-destructure-object/array calls."
  (let ((type (js-peek-type stream)))
    (cond
      ;; Simple identifier
      ((eq type :T-IDENT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (%js-binding-sym (js-tok-value tok)) rest)))
      ;; Contextual keywords are valid identifiers in a binding position
      ;; (const set = …, function f(get){…}).  They only act as keywords in
      ;; specific positions (class getter/setter, for-of, import …), which are
      ;; parsed before reaching here.  Mirrors the expression-side handling.
      ((member type '(:T-GET :T-SET :T-FROM :T-AS :T-OF
                      :T-TARGET :T-META :T-USING :T-STATIC :T-LET) :test #'eq)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (%js-binding-sym (js-tok-value tok)) rest)))
      ;; Object destructuring: {a, b: c, ...rest}
      ((eq type :T-LBRACE)
       (let ((current (cdr stream))
             (keys nil))
         (loop
           (setf current (js-skip-semis current))
           (when (or (js-at-eof-p current)
                     (eq (js-peek-type current) :T-RBRACE))
             (return))
           (cond
             ;; Rest element: ...rest
             ((eq (js-peek-type current) :T-ELLIPSIS)
              (setf current (cdr current))
              (multiple-value-bind (tok rest) (js-consume current)
                (push (list :rest (%js-binding-sym (js-tok-value tok))) keys)
                (setf current rest)))
             ;; key: binding or shorthand key
             (t
              (multiple-value-bind (key-tok rest) (js-consume current)
                (let* ((key-name (js-tok-value key-tok))
                       (local-sym (%js-binding-sym key-name)))
                  (if (and rest (eq (js-peek-type rest) :T-COLON))
                      (progn
                        (setf rest (cdr rest))
                        (multiple-value-bind (local-sym2 rest2)
                            (%js-parse-binding-pattern rest)
                          ;; key: pattern [= default]
                          (multiple-value-bind (dflt rest3) (%js-parse-pattern-default rest2)
                            (push (list key-name local-sym2 dflt) keys)
                            (setf current rest3))))
                      ;; shorthand {key [= default]}
                      (multiple-value-bind (dflt rest2) (%js-parse-pattern-default rest)
                        (push (list key-name local-sym dflt) keys)
                        (setf current rest2)))))))
           (when (eq (js-peek-type current) :T-COMMA)
             (setf current (cdr current))))
         (setf current (%js-consume-expected :T-RBRACE current))
         ;; Return a gensym for the binding; destructuring is emitted as a let
         (let ((tmp (gensym "OBJ-DEST-")))
           (values (list :object-pattern tmp (nreverse keys)) current))))
      ;; Array destructuring: [a, b, ...rest]
      ((eq type :T-LBRACKET)
       (let ((current (cdr stream))
             (elements nil))
         (loop
           (setf current (js-skip-semis current))
           (when (or (js-at-eof-p current)
                     (eq (js-peek-type current) :T-RBRACKET))
             (return))
           (cond
             ;; Elision (hole): ,
             ((eq (js-peek-type current) :T-COMMA)
              (push :hole elements))
             ;; Rest element: ...rest
             ((eq (js-peek-type current) :T-ELLIPSIS)
              (setf current (cdr current))
              (multiple-value-bind (tok rest) (js-consume current)
                (push (list :rest (%js-binding-sym (js-tok-value tok))) elements)
                (setf current rest)))
             (t
              (multiple-value-bind (sym rest) (%js-parse-binding-pattern current)
                ;; element [= default]
                (multiple-value-bind (dflt rest2) (%js-parse-pattern-default rest)
                  (push (if dflt (list :default sym dflt) sym) elements)
                  (setf current rest2)))))
           (when (eq (js-peek-type current) :T-COMMA)
             (setf current (cdr current))))
         (setf current (%js-consume-expected :T-RBRACKET current))
         (let ((tmp (gensym "ARR-DEST-")))
           (values (list :array-pattern tmp (nreverse elements)) current))))
      (t
       (error "JS parse error: expected binding pattern, got ~S" (js-peek stream))))))

(defun %js-binding-to-sym (binding-or-sym)
  "Extract the primary gensym from a binding pattern or plain symbol."
  (if (listp binding-or-sym)
      (second binding-or-sym)
      binding-or-sym))

(defun %js-destructure-sub-bindings (target access-expr)
  "Return an ORDERED list of (sym . init) bindings for a destructuring sub-TARGET
initialized from ACCESS-EXPR.  When TARGET is itself a nested array/object pattern
(e.g. the [b,c] in [a,[b,c]], or the {b} in {a:{b}}), recurse so the nested names
are bound too; a plain symbol yields a single binding.  Recursion is what makes
nested destructuring work — previously the nested pattern's gensym was bound but
never unpacked, so b/c stayed undefined and the binding form failed to compile."
  (if (and (listp target)
           (member (first target) '(:array-pattern :object-pattern)))
      (multiple-value-bind (b _e) (%js-emit-destructure-bindings target access-expr)
        (declare (ignore _e))
        b)
      (list (cons (%js-binding-to-sym target) access-expr))))

(defun %js-emit-destructure-bindings (binding init-expr)
  "Emit let bindings for a destructuring BINDING initialized from INIT-EXPR.
  Returns (values bindings-alist extra-lets) where bindings-alist is
  ((sym . init-expr) ...) in let* order and extra-lets is unused (nil).
  Bindings are accumulated with APPEND to preserve order: a nested pattern's
  gensym must be bound before the bindings that read from it."
  (if (not (listp binding))
      ;; Simple symbol
      (values (list (cons binding init-expr)) nil)
      (let ((kind (first binding))
            (tmp  (second binding))
            (desc (third binding)))
        (cond
          ((eq kind :object-pattern)
           ;; tmp = init-expr, then destructure fields
           (let ((bindings (list (cons tmp init-expr)))
                 (consumed-keys nil))
             (dolist (field desc)
               (if (eq (car field) :rest)
                   ;; rest property: emit (%js-destructure-object tmp :rest k1 k2 …)
                   ;; passing the already-bound keys so they're excluded from the
                   ;; rest object.  Rest is syntactically last, so consumed-keys is
                   ;; complete here.
                   (setf bindings
                         (append bindings
                                 (list (cons (second field)
                                             (make-ast-call
                                              :func (make-ast-var :name '%js-destructure-object)
                                              :args (list* (make-ast-var :name tmp)
                                                           (make-ast-quote :value :rest)
                                                           (mapcar (lambda (k)
                                                                     (make-ast-quote :value k))
                                                                   (reverse consumed-keys))))))))
                   ;; named property, with optional default ((key local default)).
                   ;; LOCAL may itself be a nested pattern — recurse via sub-bindings.
                   (let* ((key    (first field))
                          (local  (second field))
                          (dflt   (third field))
                          (access (%js-default-access
                                   (make-ast-call
                                    :func (make-ast-var :name '%js-get-prop)
                                    :args (list (make-ast-var :name tmp)
                                                (make-ast-quote :value key)))
                                   dflt)))
                     (push key consumed-keys)
                     (setf bindings
                           (append bindings
                                   (%js-destructure-sub-bindings local access))))))
             (values bindings nil)))
          ((eq kind :array-pattern)
           ;; tmp = init-expr, then destructure by index
           (let ((bindings (list (cons tmp init-expr)))
                 (idx 0))
             (dolist (elem desc)
               (cond
                 ((eq elem :hole)
                  (incf idx))
                 ((and (listp elem) (eq (car elem) :rest))
                  (setf bindings
                        (append bindings
                                (list (cons (second elem)
                                            (make-ast-call
                                             :func (make-ast-var :name '%js-destructure-array)
                                             :args (list (make-ast-var :name tmp)
                                                         (make-ast-quote :value idx)
                                                         (make-ast-quote :value :rest))))))))
                 ;; element with default: (:default target default-ast) — TARGET may
                 ;; be a nested pattern.
                 ((and (listp elem) (eq (car elem) :default))
                  (let ((access (%js-default-access
                                 (make-ast-call
                                  :func (make-ast-var :name '%js-get-prop)
                                  :args (list (make-ast-var :name tmp)
                                              (make-ast-quote :value idx)))
                                 (third elem))))
                    (setf bindings
                          (append bindings
                                  (%js-destructure-sub-bindings (second elem) access))))
                  (incf idx))
                 ;; plain element — may itself be a nested pattern.
                 (t
                  (let ((access (make-ast-call
                                 :func (make-ast-var :name '%js-get-prop)
                                 :args (list (make-ast-var :name tmp)
                                             (make-ast-quote :value idx)))))
                    (setf bindings
                          (append bindings
                                  (%js-destructure-sub-bindings elem access))))
                  (incf idx))))
             (values bindings nil)))
          (t (values (list (cons binding init-expr)) nil))))))
