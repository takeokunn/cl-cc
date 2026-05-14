;;;; tests/unit/expand/expander-control-helpers-tests.lisp — Control helper tests

(in-package :cl-cc/test)

(defsuite expander-control-helpers-suite
  :description "Control-form helper unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-control-helpers-suite)

(deftest expand-let-binding-expands-value
  "expand-let-binding macro-expands the value while preserving the binding name."
  (assert-equal '(x (+ 1 2)) (cl-cc/expand::expand-let-binding '(x (+ 1 2)))))

(deftest expand-flet-labels-binding-expands-body
  "expand-flet-labels-binding expands only the body forms."
  (let ((result (cl-cc/expand::expand-flet-labels-binding '(foo (x) (1+ x) (1- x)))))
    (assert-eq 'foo (first result))
    (assert-equal '(x) (second result))
    (assert-equal '((+ x 1) (- x 1)) (cddr result))))

;;; ─── %any-destructuring-let-binding-p ────────────────────────────────────

(deftest any-destructuring-binding-detects-cons-name
  "%ANY-DESTRUCTURING-LET-BINDING-P returns T when any binding has a CONS name."
  (assert-true  (cl-cc/expand::%any-destructuring-let-binding-p '(((a b) pair) (x 1))))
  (assert-false (cl-cc/expand::%any-destructuring-let-binding-p '((x 1) (y 2)))))

;;; ─── %expand-handler-case-form ────────────────────────────────────────────

(deftest handler-case-without-no-error-passes-through
  "HANDLER-CASE without :NO-ERROR expands to HANDLER-CASE with expanded sub-forms."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(handler-case expr (error (e) e)))))
    (assert-eq 'handler-case (car result))))

(deftest handler-case-no-error-wraps-in-block
  "HANDLER-CASE with :NO-ERROR lowers to a BLOCK/LET structure."
  (let* ((result (cl-cc/expand:compiler-macroexpand-all
                  '(handler-case (ok-fn)
                     (error (e) :bad)
                     (:no-error (v) v))))
         (outer (car result)))
    (assert-eq 'block outer)))

(deftest handler-case-no-error-error-clauses-return-from-block
  "HANDLER-CASE :NO-ERROR lowering rewrites error clauses to RETURN-FROM."
  (let* ((result (cl-cc/expand:compiler-macroexpand-all
                  '(handler-case (ok-fn)
                     (error (e) :bad)
                     (:no-error (v) v))))
         (inner-let   (third result))
         (handler-form (second (first (second inner-let))))
         (error-clause (third handler-form)))
    (assert-eq 'handler-case (car handler-form))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'return-from)))
                       (cddr error-clause)))))

;;; ─── %expand-let-form (destructuring) ────────────────────────────────────

(deftest let-form-with-destructuring-binding-wraps-in-destructuring-bind
  "LET with a destructuring binding (cons name) expands using DESTRUCTURING-BIND."
  (let* ((result (cl-cc/expand:compiler-macroexpand-all '(let (((a b) some-pair) (x 1)) body)))
         (has-db (labels ((scan (form)
                            (and (consp form)
                                 (or (eq (car form) 'destructuring-bind)
                                     (some #'scan (cdr form))))))
                   (scan result))))
    (assert-true has-db)))

(deftest let-form-with-empty-bindings-becomes-progn
  "LET with empty binding list expands to PROGN wrapping the body."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(let () body1 body2))))
    (assert-eq 'progn (car result))))
