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
                  (loop while (and p-rest f-rest)
                        do (let ((p (car p-rest))
                                 (f (car f-rest)))
                             (cond
                               ((and (symbolp p) (string= (symbol-name p) "...")
                                     (cdr p-rest))
                                ;; ... at end: match rest as list
                                (multiple-value-bind (b ok)
                                    (match-ellipsis (cdr p-rest) f-rest)
                                  (unless ok (return-from match-one (values nil nil)))
                                  (setf bindings (append bindings b))
                                  (return-from match-one
                                    (values bindings t))))
                               (t
                                (multiple-value-bind (b ok)
                                    (match-one p f)
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
Pattern variables are replaced with the bound form;
newly introduced symbols are gensym'd for hygiene."
  (labels ((resolve (sym)
             (let ((binding (assoc sym bindings)))
               (if binding
                   (cdr binding)
                   ;; Unbound introduced symbol: gensym for hygiene
                   (gensym (string sym))))))
    (cond
      ((consp template)
       ;; Handle (var ...) ellipsis in template
      (if (and (consp (cdr template))
               (string= (symbol-name (cadr template)) "..."  )
                (symbolp (car template)))
           (let* ((var (car template))
                  (vals (cdr (assoc var bindings)))
                  (rest (cddr template)))
             (if (listp vals)
                 (cons 'list
                       (mapcar (lambda (v)
                                 (let ((bindings (cons (cons var v) bindings)))
                                   (build-template (car rest) bindings)))
                               (if (null (cdr rest))
                                   vals
                                   (loop for v in vals collect v))))
                 template))
           (cons (build-template (car template) bindings)
                 (build-template (cdr template) bindings))))
      ((symbolp template)
       (resolve template))
      (t template))))

;; ── Public API ─────────────────────────────────────────────────────────

(defmacro define-syntax (name &body clauses)
  "Define a hygienic macro using syntax-rules pattern matching.
Clauses are of the form: (syntax-rules (keywords...) (pattern template) ...)
Supports ... ellipsis for zero-or-more element matching.
Pattern variables are automatically hygienic (auto-gensym'd)."
  (flet ((expand-clause (clause)
           (let ((keywords (cadr clause))
                 (rules (cddr clause)))
             `(register-syntax-rules
               ',name
               (cons ',keywords
                     (list ,@(mapcar (lambda (rule)
                                       `(cons ',(car rule) ',(cadr rule)))
                                     rules)))))))
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
    (let ((form-args (cdr form)))
      (dolist (clause (cdr rules))
        (let* ((keywords (car rules))
               (pattern (car clause))
               (template (cdr clause)))
          (multiple-value-bind (bindings matched)
              (match-pattern pattern (if (= (length form-args) 1)
                                        (car form-args)
                                        (cons 'list form-args))
                             keywords)
            (when matched
              (return-from expand-syntax-rules
                (build-template template bindings)))))))
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

;; ── Exports ─────────────────────────────────────────────────────────────

(export '(define-syntax syntax-rules with-gensyms once-only
          syntax-rules-expander expand-syntax-rules
          *syntax-rules-registry*)
        :cl-cc/expand)
