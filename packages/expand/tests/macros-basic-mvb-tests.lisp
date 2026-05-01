;;;; tests/unit/expand/macros-basic-mvb-tests.lisp
;;;; Coverage for src/expand/macros-basic.lisp:
;;;;   psetq, multiple-value-bind, multiple-value-setq, multiple-value-list

(in-package :cl-cc/test)

(defsuite macros-basic-mvb-suite
  :description "Tests for macros-basic.lisp: psetq and multiple-value forms"
  :parent cl-cc-integration-suite)

(in-suite macros-basic-mvb-suite)

;;; ─── psetq ────────────────────────────────────────────────────────────────

(deftest psetq-expansion-structure
  "psetq expansion: outer LET, 2 gensym bindings, setq forms in body, trailing nil."
  (let* ((exp  (our-macroexpand-1 '(psetq x 1 y 2)))
         (body (cddr exp)))
    (assert-eq 'let (car exp))
    (assert-= 2 (length (second exp)))
    (assert-true (some (lambda (f) (and (consp f) (eq 'setq (car f)))) body))
    (assert-null (car (last body)))))

(deftest psetq-empty-is-nil
  "psetq with no args expands to NIL (no-op)."
  (assert-null (our-macroexpand-1 '(psetq))))

(deftest-each psetq-runtime-parallel-behavior
  "psetq performs all assignments from old values simultaneously."
  :cases (("swap-two"     '(2 1)   "(let ((a 1) (b 2)) (psetq a b b a) (list a b))")
          ("rotate-three" '(3 1 2) "(let ((x 1) (y 2) (z 3)) (psetq x z y x z y) (list x y z))"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; ─── multiple-value-bind ──────────────────────────────────────────────────

(deftest mvb-expansion-structure
  "multiple-value-bind: explicit VALUES lowers to LET* bindings; general forms lower via values-list capture."
  (let* ((exp    (our-macroexpand-1 '(multiple-value-bind (x y) (values 1 2) x))))
    (assert-true (member (car exp) '(let let*)))))

(deftest-each mvb-runtime
  "multiple-value-bind binds values in various configurations."
  :cases (("two-values"   3  "(multiple-value-bind (a b) (values 1 2) (+ a b))")
          ("single-value" 42 "(multiple-value-bind (x) (values 42) x)")
          ("three-values" 10 "(multiple-value-bind (a b c) (values 1 3 6) (+ a b c))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; ─── multiple-value-setq ──────────────────────────────────────────────────

(deftest mvsq-expansion-structure
  "multiple-value-setq: outer LET, body has SETQ for each variable."
  (let* ((exp  (our-macroexpand-1 '(multiple-value-setq (x y) (values 1 2))))
         (body (cddr exp)))
    (assert-eq 'let (car exp))
    (assert-= 2 (length (remove-if-not (lambda (f) (and (consp f) (eq 'setq (car f)))) body)))))

(deftest-each mvsq-runtime-behavior
  "multiple-value-setq updates existing vars and returns the first value."
  :cases (("sets-vars"     '(10 20) "(let ((a 0) (b 0)) (multiple-value-setq (a b) (values 10 20)) (list a b))")
          ("returns-first" 10       "(let ((a 0) (b 0)) (multiple-value-setq (a b) (values 10 20)))"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; ─── multiple-value-list ──────────────────────────────────────────────────

(deftest mvl-expansion-structure
  "multiple-value-list: explicit VALUES lowers to LIST."
  (let* ((exp  (our-macroexpand-1 '(multiple-value-list (values 1 2 3))))
         )
    (assert-true (member (car exp) '(list let)))))

(deftest-each mvl-runtime
  "multiple-value-list collects all returned values into a list."
  :cases (("three-values" '(1 2 3) "(multiple-value-list (values 1 2 3))")
          ("single-value" '(42)    "(multiple-value-list (values 42))")
          ("with-floor"   '(5 2)   "(multiple-value-list (floor 17 3))"))
  (expected form)
  (assert-equal expected (run-string form)))
