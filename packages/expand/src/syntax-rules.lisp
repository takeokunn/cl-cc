;;; ─── Phase 142 Hygienic Macros ────────────────────────────────────────
;;; FR-804: define-syntax / syntax-rules with pattern variable hygiene
;;; FR-805: with-gensyms / once-only support macros

(in-package :cl-cc/expand)

;; ── FR-805 with-gensyms / once-only ────────────────────────────────────

(defmacro with-gensyms (syms &body body)
  "Bind each symbol in SYMS to a fresh gensym.
Example: (with-gensyms (x y z) `(let ((,x 1)) (list ,x ,y ,z)))"
  `(let ,(mapcar (lambda (s) `(,s (gensym ,(string s)))) syms)
     ,@body))

(defmacro once-only (vars &body body)
  "Ensure each variable in VARS is evaluated exactly once.
Example: (once-only (x y) `(list ,x ,y ,x ,y))
Wraps symbols with gensym bindings to prevent multiple evaluation."
  (let ((gsyms (mapcar (lambda (v) (gensym (string v))) vars)))
    `(let ,(mapcar (lambda (g v) `(,g ,v)) gsyms vars)
       (let ,(mapcar (lambda (v g) `(,v ,g)) vars gsyms)
         ,@body))))

;; ── FR-804 syntax-rules implementation ─────────────────────────────────

(defvar *syntax-rules-registry* (make-hash-table :test #'eq)
  "Registry of define-syntax definitions, keyed by macro name symbol.")

(defun register-syntax-rules (name rules)
  (setf (gethash name *syntax-rules-registry*) rules)
  name)

(defun find-syntax-rules (name)
  (gethash name *syntax-rules-registry*))

;; ── Pattern matching helpers ───────────────────────────────────────────

(defun pattern-var-p (sym)
  "Return true when SYM is a pattern variable (not a keyword or literal)."
  (and (symbolp sym)
       (not (keywordp sym))
       (not (string= (symbol-name sym) "..."))))

(defun match-pattern (pattern form env)
  "Match PATTERN against FORM returning an alist of (var . value) bindings.
ENV is a list of keyword symbols to match literally.
Returns (values bindings matched-p)."
  (labels ((match-ellipsis (pat-rest form-rest)
             ;; Handle (pat ...) matching zero or more elements
             (let ((pat-var (car pat-rest)))
               (cond
                 ((endp form-rest)
                  (values `((,pat-var . nil)) t))
                 (t (values `((,pat-var . ,form-rest)) t)))))
           (match-one (pat frm)
             (cond
               ;; Pattern variable: bind to form
               ((and (symbolp pat) (not (member pat env :test #'eq))
                      (not (string= (symbol-name pat) "...")))
                 (values `((,pat . ,frm)) t))
               ;; Literal symbol from env or keyword
               ((and (symbolp pat) (or (member pat env :test #'eq)
                                       (keywordp pat)))
                (values nil (eq pat frm)))
               ;; Quoted literal
               ((and (consp pat) (eq (car pat) 'quote))
                (values nil (equal (cadr pat) frm)))
               ;; Nested list pattern
               ((and (consp pat) (consp frm))
                (let ((bindings nil)
                      (p-rest pat)
                      (f-rest frm))
                  (loop while p-rest
                        do (let ((p (car p-rest)))
                             (cond
                               ;; (var ...) ellipsis: var followed by ...
                               ((and (symbolp p)
                                     (not (string= (symbol-name p) "..."))
                                     (not (member p env :test #'eq))
                                     (consp (cdr p-rest))
                                     (symbolp (cadr p-rest))
                                     (string= (symbol-name (cadr p-rest)) "..."))
                                (setf bindings (append bindings (list (cons p f-rest))))
                                (setf p-rest (cddr p-rest)
                                      f-rest nil))
                               ;; bare ... token (legacy): match rest as list
                               ((and (symbolp p) (string= (symbol-name p) "..."))
                                (multiple-value-bind (b ok)
                                    (match-ellipsis (cdr p-rest) f-rest)
                                  (unless ok (return-from match-one (values nil nil)))
                                  (setf bindings (append bindings b))
                                  (return-from match-one (values bindings t))))
                               ;; normal element
                               ((null f-rest)
                                (return-from match-one (values nil nil)))
                               (t
                                (multiple-value-bind (b ok)
                                    (match-one p (car f-rest))
                                  (unless ok (return-from match-one (values nil nil)))
                                  (setf bindings (append bindings b))
                                  (pop p-rest)
                                  (pop f-rest))))))
                  (if (or p-rest f-rest)
                      (values nil nil)
                      (values bindings t))))
               ;; Anything else: literal match
               (t (values nil (equal pat frm))))))
    (match-one pattern form)))

(defun build-template (template bindings)
  "Substitute pattern variables in TEMPLATE with their bindings.
Pattern variables are replaced with their bound values.  Newly introduced
symbols (not in bindings, not operators) are consistently gensym'd for hygiene.
List templates with (var ...) are spliced in-place."
  (let ((gensym-map (make-hash-table :test #'eq)))
    (labels ((resolve (sym)
               (let ((binding (assoc sym bindings)))
                 (cond
                   ;; Pattern variable: substitute bound value
                   (binding (cdr binding))
                   ;; Known operator/function: keep as-is to preserve semantics
                   ((or (special-operator-p sym)
                        (fboundp sym)
                        (cl:macro-function sym))
                    sym)
                   ;; Fresh introduced identifier: stable gensym within this expansion
                   (t
                    (or (gethash sym gensym-map)
                        (setf (gethash sym gensym-map)
                              (gensym (string sym))))))))
             (do-build (tmpl)
               (cond
                 ((consp tmpl)
                  ;; Build list, handling (var ...) splicing
                  (let ((result nil))
                    (loop for cell = tmpl then (cdr cell)
                          while (consp cell)
                          for elem = (car cell)
                          for next-cell = (cdr cell)
                          do (cond
                               ;; (var ...) splicing pattern
                               ((and (symbolp elem)
                                     (consp next-cell)
                                     (symbolp (car next-cell))
                                     (string= (symbol-name (car next-cell)) "..."))
                                (let* ((binding (assoc elem bindings))
                                       (vals (if binding (cdr binding) nil)))
                                  (dolist (v vals)
                                    (push v result)))
                                ;; Advance past the ... token
                                (setf cell next-cell))
                               ;; Normal element: recurse
                               (t
                                (push (do-build elem) result)))
                          finally (when cell
                                    ;; Improper list tail
                                    (push (do-build cell) result)))
                    (nreverse result)))
                 ((symbolp tmpl)
                  (resolve tmpl))
                 (t tmpl))))
      (do-build template))))

;; ── Public API ─────────────────────────────────────────────────────────

(defmacro define-syntax (name &body clauses)
  "Define a hygienic macro using syntax-rules pattern matching.
Clauses are of the form: (syntax-rules (keywords...) (pattern template) ...)
Supports ... ellipsis for zero-or-more element matching.
Pattern variables are automatically hygienic (auto-gensym'd)."
  (flet ((expand-clause (clause)
           (let ((keywords (cadr clause))
                 (rules (cddr clause)))
             `(progn
                (register-syntax-rules
                 ',name
                 (cons ',keywords
                       (list ,@(mapcar (lambda (rule)
                                         `(cons ',(car rule) ',(cadr rule)))
                                       rules))))
                (register-macro ',name
                  (lambda (form env)
                    (declare (ignore env))
                    (or (expand-syntax-rules ',name form) form)))))))
    `(progn
       ,@(mapcar #'expand-clause clauses))))

(defmacro syntax-rules (keywords &body rules)
  "Internal: define pattern matching rules for syntax-rules.
Not called directly; used inside define-syntax clauses."
  ;; This is a marker form; the actual expansion happens in define-syntax
  (declare (ignore keywords rules))
  nil)

;; ── Macro expander integration ─────────────────────────────────────────

(defun expand-syntax-rules (name form)
  "Expand (NAME . ARGS) using registered syntax-rules.
Returns the expanded form or NIL if no rules match."
  (let ((rules (find-syntax-rules name)))
    (unless rules (return-from expand-syntax-rules nil))
    (dolist (clause (cdr rules))
      (let* ((keywords (car rules))
             (pattern (car clause))
             (template (cdr clause)))
        ;; Skip the keyword (macro name) in both pattern and form
        (multiple-value-bind (bindings matched)
            (match-pattern (cdr pattern) (cdr form) keywords)
          (when matched
            (return-from expand-syntax-rules
              (build-template template bindings))))))
    nil))

(defun syntax-rules-expander (form env)
  "Macro expander hook: try syntax-rules expansion before defmacro.
Return expanded form or FORM unchanged. To be called from our-macroexpand-1."
  (declare (ignore env))
  (when (and (consp form) (symbolp (car form)))
    (let ((expanded (expand-syntax-rules (car form) form)))
      (when expanded
        (return-from syntax-rules-expander expanded))))
  form)

;; ── syntax-case (partial, FR-804) ─────────────────────────────────────

(defun %collect-pattern-vars (pattern literals)
  "Return all pattern variable symbols in PATTERN (not literals, not ...)."
  (cond
    ((null pattern) nil)
    ((symbolp pattern)
     (when (and (not (string= (symbol-name pattern) "..."))
                (not (member pattern literals :test #'eq)))
       (list pattern)))
    ((consp pattern)
     (append (%collect-pattern-vars (car pattern) literals)
             (%collect-pattern-vars (cdr pattern) literals)))
    (t nil)))

(defmacro syntax-case (form-expr literals &rest clauses)
  "Partial syntax-case: match FORM-EXPR against each CLAUSE pattern.
Each clause is (pattern template) or (pattern guard template).
Pattern variables are bound during guard and template evaluation."
  (let ((form-var (gensym "FORM"))
        (bindings-var (gensym "BINDINGS"))
        (ok-var (gensym "OK"))
        (block-name (gensym "SYNTAX-CASE")))
    `(let ((,form-var ,form-expr))
       (block ,block-name
         ,@(mapcar
            (lambda (clause)
              (destructuring-bind (pattern &rest rest) clause
                (let* ((has-guard (= (length rest) 2))
                       (guard-expr (if has-guard (first rest) nil))
                       (tmpl-expr  (if has-guard (second rest) (first rest)))
                       (pvars (%collect-pattern-vars pattern literals)))
                  `(multiple-value-bind (,bindings-var ,ok-var)
                       (match-pattern ',pattern ,form-var ',literals)
                     (when ,ok-var
                       (let ,(mapcar (lambda (v)
                                       `(,v (cdr (assoc ',v ,bindings-var))))
                                     pvars)
                         ,(if has-guard
                              `(when ,guard-expr
                                 (return-from ,block-name ,tmpl-expr))
                              `(return-from ,block-name ,tmpl-expr))))))))
            clauses)
         nil))))

;; ── Exports ─────────────────────────────────────────────────────────────

(export '(define-syntax syntax-rules with-gensyms once-only
          syntax-rules-expander expand-syntax-rules
          *syntax-rules-registry*
          syntax-case match-pattern build-template)
        :cl-cc/expand)
