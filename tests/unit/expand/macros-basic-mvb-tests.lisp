;;;; tests/unit/expand/macros-basic-mvb-tests.lisp
;;;; Coverage for src/expand/macros-basic.lisp:
;;;;   psetq, multiple-value-bind, multiple-value-setq, multiple-value-list

(in-package :cl-cc/test)

(defsuite macros-basic-mvb-suite
  :description "Tests for macros-basic.lisp: psetq and multiple-value forms"
  :parent cl-cc-integration-suite)

(in-suite macros-basic-mvb-suite)

;;; ─── psetq ────────────────────────────────────────────────────────────────

(deftest psetq-expands-to-let
  "psetq expansion outer form is LET (parallel binding via gensyms)."
  (let ((exp (our-macroexpand-1 '(psetq x 1 y 2))))
    (assert-eq 'let (car exp))))

(deftest psetq-let-has-two-bindings
  "psetq with two vars produces two gensym bindings."
  (let* ((exp      (our-macroexpand-1 '(psetq x 1 y 2)))
         (bindings (second exp)))
    (assert-= 2 (length bindings))))

(deftest psetq-body-has-setq-forms
  "psetq body contains SETQ forms to assign back from gensyms."
  (let* ((exp  (our-macroexpand-1 '(psetq x 1 y 2)))
         (body (cddr exp)))
    ;; Two setq forms + trailing nil
    (assert-true (some (lambda (f) (and (consp f) (eq 'setq (car f)))) body))))

(deftest psetq-body-returns-nil
  "psetq expansion ends with NIL (parallel-setq returns nil)."
  (let* ((exp  (our-macroexpand-1 '(psetq x 1 y 2)))
         (body (cddr exp)))
    (assert-null (car (last body)))))

(deftest psetq-empty-is-nil
  "psetq with no args expands to NIL (no-op)."
  (assert-null (our-macroexpand-1 '(psetq))))

(deftest psetq-runtime-parallel-swap
  "psetq swaps two variables simultaneously (classic parallel-assignment test)."
  (assert-equal '(2 1)
                (run-string "(let ((a 1) (b 2))
                               (psetq a b b a)
                               (list a b))")))

(deftest psetq-runtime-three-vars
  "psetq with three vars performs all assignments from old values."
  (assert-equal '(3 1 2)
                (run-string "(let ((x 1) (y 2) (z 3))
                               (psetq x z y x z y)
                               (list x y z))")))

;;; ─── multiple-value-bind ──────────────────────────────────────────────────

(deftest mvb-expands-to-multiple-value-call
  "multiple-value-bind expands to multiple-value-call with a lambda."
  (let ((exp (our-macroexpand-1 '(multiple-value-bind (a b) (values 1 2) (+ a b)))))
    (assert-eq 'multiple-value-call (car exp))))

(deftest mvb-lambda-has-correct-params
  "multiple-value-bind lambda has the declared vars as params."
  (let* ((exp    (our-macroexpand-1 '(multiple-value-bind (x y) (values 1 2) x)))
         (lambda (second exp))
         (params (second lambda)))
    (assert-equal '(x y) params)))

(deftest mvb-runtime-captures-two-values
  "multiple-value-bind binds both returned values."
  (assert-= 3 (run-string "(multiple-value-bind (a b) (values 1 2) (+ a b))")))

(deftest mvb-runtime-single-value
  "multiple-value-bind works for a single binding."
  (assert-= 42 (run-string "(multiple-value-bind (x) (values 42) x)")))

(deftest mvb-runtime-with-body-forms
  "multiple-value-bind body can have multiple forms (implicit progn)."
  (assert-= 10
            (run-string "(multiple-value-bind (a b c)
                           (values 1 3 6)
                           (+ a b c))")))

;;; ─── multiple-value-setq ──────────────────────────────────────────────────

(deftest mvsq-expands-to-let
  "multiple-value-setq expansion outer form is LET."
  (let ((exp (our-macroexpand-1 '(multiple-value-setq (a b) (values 1 2)))))
    (assert-eq 'let (car exp))))

(deftest mvsq-body-contains-setq-for-each-var
  "multiple-value-setq body contains SETQ forms for each variable."
  (let* ((exp  (our-macroexpand-1 '(multiple-value-setq (x y) (values 1 2))))
         (body (cddr exp)))
    (let ((setq-forms (remove-if-not (lambda (f) (and (consp f) (eq 'setq (car f)))) body)))
      (assert-= 2 (length setq-forms)))))

(deftest mvsq-runtime-sets-existing-vars
  "multiple-value-setq destructively updates existing bindings."
  (assert-equal '(10 20)
                (run-string "(let ((a 0) (b 0))
                               (multiple-value-setq (a b) (values 10 20))
                               (list a b))")))

(deftest mvsq-returns-first-value
  "multiple-value-setq returns the first value (per CLHS)."
  (assert-= 10
            (run-string "(let ((a 0) (b 0))
                           (multiple-value-setq (a b) (values 10 20)))")))

;;; ─── multiple-value-list ──────────────────────────────────────────────────

(deftest mvl-expands-to-let
  "multiple-value-list expansion outer form is LET."
  (let ((exp (our-macroexpand-1 '(multiple-value-list (values 1 2 3)))))
    (assert-eq 'let (car exp))))

(deftest mvl-body-contains-multiple-value-call
  "multiple-value-list body contains multiple-value-call."
  (let* ((exp  (our-macroexpand-1 '(multiple-value-list (values 1 2 3))))
         (body (cddr exp)))
    (assert-true (some (lambda (f) (and (consp f)
                                        (eq 'multiple-value-call (car f))))
                       body))))

(deftest mvl-runtime-collects-three-values
  "multiple-value-list collects all values into a list."
  (assert-equal '(1 2 3)
                (run-string "(multiple-value-list (values 1 2 3))")))

(deftest mvl-runtime-single-value-gives-singleton
  "multiple-value-list with one value returns a singleton list."
  (assert-equal '(42)
                (run-string "(multiple-value-list (values 42))")))

(deftest mvl-runtime-with-computation
  "multiple-value-list works with complex expressions."
  (assert-equal '(5 6)
                (run-string "(multiple-value-list (floor 17 3))")))
