;;;; tests/unit/expand/macros-plist-tests.lisp
;;;; Coverage tests for src/expand/macros-plist.lisp

(in-package :cl-cc/test)

(defsuite macros-plist-suite
  :description "Tests for macros-plist.lisp"
  :parent cl-cc-unit-suite)

(in-suite macros-plist-suite)

;;; ── GETF ─────────────────────────────────────────────────────────────────────

(deftest getf-expansion
  "GETF: outer LET, inner LET uses MEMBER, body is IF, default passes through."
  (let* ((result     (our-macroexpand-1 '(getf plist :key)))
         (inner-let  (caddr result))
         (found-init (cadar (cadr inner-let)))
         (if-form    (caddr inner-let)))
    (assert-eq 'let (car result))
    (assert-eq 'member (car found-init))
    (assert-eq 'if (car if-form)))
  ;; Default passes through to the IF fallback
  (let* ((result    (our-macroexpand-1 '(getf plist :key :missing)))
         (inner-let (caddr result))
         (if-form   (caddr inner-let)))
    (assert-equal :missing (cadddr if-form))))

;;; ── REMF ─────────────────────────────────────────────────────────────────────

(deftest remf-expansion
  "REMF: outer LET containing a TAGBODY loop."
  (let* ((result (our-macroexpand-1 '(remf plist :key)))
         (body   (cddr result)))
    (assert-eq 'let (car result))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── %PLIST-PUT ───────────────────────────────────────────────────────────────

(deftest plist-put-expansion
  "%PLIST-PUT: outer LET, body iterates with LOOP."
  ;; %plist-put is cl-cc-internal (not exported), use cl-cc:: prefix
  (let* ((result    (our-macroexpand-1 '(cl-cc::%plist-put my-plist :key 42)))
         (loop-form (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'loop (car loop-form))))
