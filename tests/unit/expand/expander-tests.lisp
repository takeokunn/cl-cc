;;;; tests/unit/expand/expander-tests.lisp — Macro Expansion Helper Tests
;;;;
;;;; Tests for the pure helper functions in expander.lisp:
;;;; reduce-variadic-op, expand-let-binding, expand-flet-labels-binding,
;;;; expand-lambda-list-defaults, expand-setf-cons-place, builtin tables.

(in-package :cl-cc/test)

(defsuite expander-suite :description "Macro expansion helper unit tests")

;;; ─── reduce-variadic-op ─────────────────────────────────────────────────────

(deftest variadic-zero-args
  "reduce-variadic-op with 0 args returns identity."
  (assert-equal 0 (cl-cc::reduce-variadic-op '+ nil 0))
  (assert-equal 1 (cl-cc::reduce-variadic-op '* nil 1)))

(deftest variadic-one-arg
  "reduce-variadic-op with 1 arg returns the arg itself."
  (assert-equal 'x (cl-cc::reduce-variadic-op '+ '(x) 0)))

(deftest variadic-two-args
  "reduce-variadic-op with 2 args returns (OP a b)."
  (assert-equal '(+ a b) (cl-cc::reduce-variadic-op '+ '(a b) 0)))

(deftest variadic-three-args
  "reduce-variadic-op with 3 args returns (OP (OP a b) c)."
  (assert-equal '(+ (+ a b) c)
                (cl-cc::reduce-variadic-op '+ '(a b c) 0)))

(deftest variadic-four-args
  "reduce-variadic-op with 4 args nests left-associatively."
  (assert-equal '(* (* (* a b) c) d)
                (cl-cc::reduce-variadic-op '* '(a b c d) 1)))

;;; ─── expand-let-binding ─────────────────────────────────────────────────────

(deftest let-binding-symbol-value
  "expand-let-binding expands the value form, preserves name."
  (let ((b (cl-cc::expand-let-binding '(x 42))))
    (assert-equal 'x (first b))
    (assert-equal 42 (second b))))

(deftest let-binding-bare-symbol
  "expand-let-binding passes through a bare symbol."
  (assert-equal 'x (cl-cc::expand-let-binding 'x)))

;;; ─── expand-flet-labels-binding ─────────────────────────────────────────────

(deftest flet-binding-expands-body
  "expand-flet-labels-binding preserves name+params, expands body."
  (let ((b (cl-cc::expand-flet-labels-binding '(foo (x) (+ x 1)))))
    (assert-equal 'foo (first b))
    (assert-equal '(x) (second b))
    ;; Body was processed by compiler-macroexpand-all
    (assert-true (consp (third b)))))

(deftest flet-binding-short-form
  "expand-flet-labels-binding passes through short binding (<3 elements)."
  (let ((b (cl-cc::expand-flet-labels-binding '(foo (x)))))
    (assert-equal '(foo (x)) b)))

;;; ─── expand-lambda-list-defaults ────────────────────────────────────────────

(deftest lambda-list-no-defaults
  "expand-lambda-list-defaults passes through simple params."
  (assert-equal '(x y z) (cl-cc::expand-lambda-list-defaults '(x y z))))

(deftest lambda-list-optional-with-default
  "expand-lambda-list-defaults expands optional default values."
  (let ((result (cl-cc::expand-lambda-list-defaults '(x &optional (y 42)))))
    (assert-equal 'x (first result))
    (assert-equal '&optional (second result))
    ;; y's default should be expanded (42 stays as 42)
    (assert-true (consp (third result)))))

;;; ─── expand-setf-cons-place ─────────────────────────────────────────────────

(deftest setf-car-expands-to-rplaca
  "expand-setf-cons-place for (car x) generates rplaca."
  (let ((result (cl-cc::expand-setf-cons-place '(car x) 'val)))
    (assert-true (consp result))
    ;; Should produce (let ((gensym val)) (rplaca x gensym) gensym)
    (assert-equal 'let (first result))
    ;; The body should contain rplaca
    (let ((body-str (format nil "~S" result)))
      (assert-true (search "RPLACA" body-str)))))

(deftest setf-cdr-expands-to-rplacd
  "expand-setf-cons-place for (cdr x) generates rplacd."
  (let ((result (cl-cc::expand-setf-cons-place '(cdr x) 'val)))
    (let ((body-str (format nil "~S" result)))
      (assert-true (search "RPLACD" body-str)))))

;;; ─── Builtin arity tables ──────────────────────────────────────────────────

(deftest variadic-fold-builtins-contents
  "*variadic-fold-builtins* includes + * append nconc."
  (assert-true (member '+ cl-cc::*variadic-fold-builtins*))
  (assert-true (member '* cl-cc::*variadic-fold-builtins*))
  (assert-true (member 'append cl-cc::*variadic-fold-builtins*))
  (assert-true (member 'nconc cl-cc::*variadic-fold-builtins*)))

(deftest binary-builtins-contents
  "*binary-builtins* includes key entries."
  (assert-true (member 'cons cl-cc::*binary-builtins*))
  (assert-true (member '= cl-cc::*binary-builtins*))
  (assert-true (member 'mod cl-cc::*binary-builtins*))
  (assert-true (member 'ash cl-cc::*binary-builtins*)))

(deftest unary-builtins-contents
  "*unary-builtins* includes key entries."
  (assert-true (member 'car cl-cc::*unary-builtins*))
  (assert-true (member 'cdr cl-cc::*unary-builtins*))
  (assert-true (member 'not cl-cc::*unary-builtins*))
  (assert-true (member 'length cl-cc::*unary-builtins*)))

(deftest cxr-builtins-completeness
  "*cxr-builtins* has all 28 compositions."
  (assert-equal 28 (length cl-cc::*cxr-builtins*))
  (assert-true (member 'caar cl-cc::*cxr-builtins*))
  (assert-true (member 'cddddr cl-cc::*cxr-builtins*)))

(deftest all-builtin-names-union
  "*all-builtin-names* is the union of all sub-tables."
  (assert-true (member '+ cl-cc::*all-builtin-names*))
  (assert-true (member 'cons cl-cc::*all-builtin-names*))
  (assert-true (member 'car cl-cc::*all-builtin-names*))
  (assert-true (member 'caar cl-cc::*all-builtin-names*))
  (assert-true (member 'list cl-cc::*all-builtin-names*)))

;;; ─── compiler-macroexpand-all ───────────────────────────────────────────────

(deftest expand-all-integer-literal
  "compiler-macroexpand-all passes through integer literals."
  (assert-equal 42 (cl-cc::compiler-macroexpand-all 42)))

(deftest expand-all-string-literal
  "compiler-macroexpand-all passes through string literals."
  (assert-equal "hello" (cl-cc::compiler-macroexpand-all "hello")))

(deftest expand-all-symbol
  "compiler-macroexpand-all passes through symbols (non-macro)."
  (assert-equal 'x (cl-cc::compiler-macroexpand-all 'x)))

(deftest expand-all-quote
  "compiler-macroexpand-all preserves quoted forms."
  (assert-equal '(quote (1 2 3))
                (cl-cc::compiler-macroexpand-all '(quote (1 2 3)))))

(deftest expand-all-if
  "compiler-macroexpand-all recurses into if branches."
  (let ((result (cl-cc::compiler-macroexpand-all '(if t 1 2))))
    (assert-equal 'if (first result))
    (assert-equal t (second result))
    (assert-equal 1 (third result))
    (assert-equal 2 (fourth result))))

(deftest expand-all-let
  "compiler-macroexpand-all expands let binding values."
  (let ((result (cl-cc::compiler-macroexpand-all '(let ((x 1)) x))))
    (assert-equal 'let (first result))
    (assert-equal 'x (caar (second result)))))

(deftest expand-all-variadic-plus
  "compiler-macroexpand-all reduces (+ a b c) to nested binary."
  (let ((result (cl-cc::compiler-macroexpand-all '(+ 1 2 3))))
    ;; Should be (+ (+ 1 2) 3)
    (assert-equal '+ (first result))
    (assert-true (consp (second result)))
    (assert-equal '+ (first (second result)))))
