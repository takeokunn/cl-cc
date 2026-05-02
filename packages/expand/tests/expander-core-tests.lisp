;;;; tests/unit/expand/expander-core-tests.lisp — Core expander helper tests

(in-package :cl-cc/test)

(defsuite expander-core-suite :description "Core expander helper unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-core-suite)
(deftest-each reduce-variadic-op
  "reduce-variadic-op builds left-nested call trees for any arity."
  :cases (("zero-plus"  '+ nil       0 0)
          ("zero-mul"   '* nil       1 1)
          ("one-arg"    '+ '(x)      0 'x)
          ("two-args"   '+ '(a b)    0 '(+ a b))
          ("three-args" '+ '(a b c)  0 '(+ (+ a b) c))
          ("four-args"  '* '(a b c d) 1 '(* (* (* a b) c) d)))
  (op args id expected)
  (assert-equal expected (cl-cc/expand::reduce-variadic-op op args id)))

(deftest-each expand-all-atom-passthrough
  "compiler-macroexpand-all passes atoms (int, string, symbol) through unchanged."
  :cases (("integer" 42)
          ("string"  "hello")
          ("symbol"  'x))
  (form)
  (assert-equal form (cl-cc/expand:compiler-macroexpand-all form)))

(deftest expand-all-quote
  "compiler-macroexpand-all preserves quoted forms."
  (assert-equal '(quote (1 2 3))
                (cl-cc/expand:compiler-macroexpand-all '(quote (1 2 3)))))

(deftest expand-all-if
  "compiler-macroexpand-all recurses into if branches."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(if t 1 2))))
    (assert-equal 'if (first result))
    (assert-equal t (second result))
    (assert-equal 1 (third result))
    (assert-equal 2 (fourth result))))

(deftest expand-all-let
  "compiler-macroexpand-all expands let binding values."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(let ((x 1)) x))))
    (assert-equal 'let (first result))
    (assert-equal 'x (caar (second result)))))

(deftest-each expander-variadic-fold-nesting
  "3-arg variadic forms nest left-associatively: (OP a b c) → (OP (OP a b) c)."
  :cases (("multiply" '*      '(* a b c))
          ("append"   'append '(append a b c))
          ("minus"    '-      '(- a b c)))
  (op form)
  (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (assert-eq op (car result))
    (assert-true (consp (second result)))
    (assert-eq op (car (second result)))))

(deftest-each expander-variadic-zero-arg-identity
  "(+) → 0 and (*) → 1 (their respective identity elements)."
  :cases (("plus"  '(+) 0)
          ("times" '(*) 1))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

;;; ─── define-expander-for (registration macro) ────────────────────────────

(deftest define-expander-for-installs-handler-in-table
  "define-expander-for registers a lambda in *expander-head-table* under HEAD.
After registration, compiler-macroexpand-all dispatches to the new handler."
  (let ((test-head (gensym "EXP-HEAD")))
    ;; Simulate what define-expander-for does at macro expansion
    (setf (gethash test-head cl-cc/expand::*expander-head-table*)
          (lambda (form) (list 'was-handled (second form))))
    (unwind-protect
        (let ((result (cl-cc/expand:compiler-macroexpand-all (list test-head 42))))
          (assert-equal 'was-handled (first result))
          (assert-equal 42 (second result)))
       (remhash test-head cl-cc/expand::*expander-head-table*))))

(deftest make-macro-expander-returns-descriptor
  "make-macro-expander returns a descriptor instead of a host closure."
  (let ((expander (cl-cc/expand:make-macro-expander '(&body body) '((cons 'progn body)))))
    (assert-equal :macro-expander (getf expander :kind))
    (assert-equal '(&body body) (getf expander :lambda-list))
    (assert-true (listp (getf expander :body)))))

(deftest expander-handler-returning-same-form-does-not-recurse
  "When a handler returns the identical form (equal), the expander stops recursion."
  (let ((test-head (gensym "STABLE")))
    (setf (gethash test-head cl-cc/expand::*expander-head-table*)
          (lambda (form) form))  ; idempotent — returns unchanged
    (unwind-protect
        (let ((form (list test-head 1 2)))
          (assert-equal form (cl-cc/expand:compiler-macroexpand-all form)))
      (remhash test-head cl-cc/expand::*expander-head-table*))))

;;; ─── defmethod expander handler ──────────────────────────────────────────
;;; The defmethod handler is registered by expander.lisp via define-expander-for.

(deftest expander-defmethod-no-qualifier-expands-body
  "The defmethod expander recurses into body forms when no qualifier is present."
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                 '(defmethod my-noq-method ((x integer)) (+ x 1)))))
    (assert-equal 'defmethod (first result))
    (assert-equal 'my-noq-method (second result))))

(deftest expander-defmethod-with-qualifier-preserved-verbatim
  "The defmethod expander preserves the qualifier symbol verbatim in position 3."
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                 '(defmethod my-around-method :around ((x integer)) x))))
    (assert-equal 'defmethod (first result))
    (assert-equal 'my-around-method (second result))
    (assert-equal :around (third result))))

(deftest-each expander-defmethod-all-qualifiers-preserved
  "All standard qualifiers (:before, :after, :around) are preserved by the expander."
  :cases (("before" :before)
          ("after"  :after)
          ("around" :around))
  (qualifier)
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                 (list 'defmethod 'my-qual-method qualifier '(x) 'x))))
    (assert-equal qualifier (third result))))

;;; ─── symbol-macro expansion path ─────────────────────────────────────────

(deftest compiler-macroexpand-all-expands-registered-symbol-macro
  "compiler-macroexpand-all expands symbols registered in *symbol-macro-table*."
  (let ((key (gensym "SM")))
    (setf (gethash key cl-cc/expand:*symbol-macro-table*) 99)
    (unwind-protect
        (assert-equal 99 (cl-cc/expand:compiler-macroexpand-all key))
      (remhash key cl-cc/expand:*symbol-macro-table*))))

(deftest compiler-macroexpand-all-keywords-bypass-symbol-macro
  "Keywords are not expanded even if coincidentally in *symbol-macro-table*."
  ;; Keywords are excluded by (not (keywordp form)) guard
  (let ((kw :test-keyword))
    (setf (gethash kw cl-cc/expand:*symbol-macro-table*) 'expanded)
    (unwind-protect
        (assert-equal kw (cl-cc/expand:compiler-macroexpand-all kw))
      (remhash kw cl-cc/expand:*symbol-macro-table*))))

;;; ─── accessor slot-map expansion path (FR-120) ───────────────────────────

(deftest compiler-macroexpand-all-accessor-expands-to-slot-value
  "compiler-macroexpand-all expands (accessor obj) to (slot-value obj 'slot)
via *accessor-slot-map* when the accessor is registered."
  (let ((accessor (gensym "ACC"))
        (class-sym (gensym "CLS")))
    (setf (gethash accessor cl-cc/expand:*accessor-slot-map*) (cons class-sym 'my-slot))
    (unwind-protect
        (let ((result (cl-cc/expand:compiler-macroexpand-all (list accessor 'my-obj))))
          ;; Expected: (slot-value my-obj 'my-slot)
          (assert-equal 'slot-value (first result))
          (assert-equal 'my-obj (second result))
          (assert-equal '(quote my-slot) (third result)))
      (remhash accessor cl-cc/expand:*accessor-slot-map*))))

(deftest compiler-macroexpand-all-multi-arg-accessor-does-not-expand
  "compiler-macroexpand-all does NOT expand (accessor obj extra) — 2 args required."
  (let ((accessor (gensym "ACC2")))
    (setf (gethash accessor cl-cc/expand:*accessor-slot-map*) (cons 'cls 'slot))
    (unwind-protect
        (let ((result (cl-cc/expand:compiler-macroexpand-all
                       (list accessor 'obj 'extra))))
          ;; Should NOT be slot-value (2-arg form only)
          (assert-false (eq 'slot-value (first result))))
      (remhash accessor cl-cc/expand:*accessor-slot-map*))))
