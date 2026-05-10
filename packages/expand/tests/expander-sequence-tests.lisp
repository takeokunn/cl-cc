;;;; tests/unit/expand/expander-sequence-tests.lisp — Sequence expander tests

(in-package :cl-cc/test)

(defsuite expander-sequence-suite
  :description "Sequence expander unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-sequence-suite)

(deftest-each expander-sequence-predicates-expand-to-let
  "mapcar, every, and some expand to loop-based LET forms."
  :cases (("mapcar" '(mapcar #'1+ '(1 2)))
          ("every"  '(every #'evenp '(1 2 3)))
          ("some"   '(some #'oddp '(1 2 3))))
  (form)
  (assert-eq 'let (car (cl-cc/expand:compiler-macroexpand-all form))))

;;; ── %sequence-build-null-test ─────────────────────────────────────────────

(deftest-each sequence-build-null-test-forms
  "%sequence-build-null-test builds (or (null v1) ...) for each list var."
  :cases (("single"   '(l1)       '(or (null l1)))
          ("two"      '(l1 l2)    '(or (null l1) (null l2)))
          ("three"    '(l1 l2 l3) '(or (null l1) (null l2) (null l3))))
  (vars expected)
  (assert-equal expected (cl-cc/expand::%sequence-build-null-test vars)))

;;; ── %sequence-build-car-args ──────────────────────────────────────────────

(deftest-each sequence-build-car-args-forms
  "%sequence-build-car-args wraps each var in (car v)."
  :cases (("single"  '(l1)       '((car l1)))
          ("two"     '(l1 l2)    '((car l1) (car l2))))
  (vars expected)
  (assert-equal expected (cl-cc/expand::%sequence-build-car-args vars)))

;;; ── %sequence-build-cdr-args ──────────────────────────────────────────────

(deftest-each sequence-build-cdr-args-forms
  "%sequence-build-cdr-args wraps each var in (cdr v)."
  :cases (("single"  '(l1)       '((cdr l1)))
          ("two"     '(l1 l2)    '((cdr l1) (cdr l2))))
  (vars expected)
  (assert-equal expected (cl-cc/expand::%sequence-build-cdr-args vars)))

;;; ── %sequence-build-single-map-form ──────────────────────────────────────

(deftest sequence-build-single-map-collect-shape
  "%sequence-build-single-map-form :collect builds (let (fn) (labels ((helper (lst) ...)) ...))."
  (let ((form (cl-cc/expand::%sequence-build-single-map-form :collect '#'1+ 'xs)))
    (assert-eq 'let (first form))
    (assert-eq 'labels (first (third form)))))

(deftest sequence-build-single-map-side-effect-shape
  "%sequence-build-single-map-form :side-effect builds labels that returns the input list."
  (let ((form (cl-cc/expand::%sequence-build-single-map-form :side-effect '#'print 'xs)))
    (assert-eq 'let (first form))
    (assert-eq 'labels (first (third form)))))

(deftest sequence-build-single-map-flatmap-shape
  "%sequence-build-single-map-form :flatmap (default) builds nconc-based recursion."
  (let* ((form (cl-cc/expand::%sequence-build-single-map-form :flatmap '#'list 'xs))
         (labels-body (first (second (third form))))
         (rec-body    (third labels-body)))
    (assert-eq 'let (first form))
    (assert-eq 'nconc (first (third (third rec-body))))))

;;; ── %sequence-build-multi-map-form ───────────────────────────────────────

(deftest sequence-build-multi-map-collect-shape
  "%sequence-build-multi-map-form :collect wraps cons recursion in labels."
  (let ((form (cl-cc/expand::%sequence-build-multi-map-form :collect '#'+ '(l1 l2))))
    (assert-eq 'let (first form))
    (assert-eq 'labels (first (third form)))))

(deftest sequence-build-multi-map-side-effect-shape
  "%sequence-build-multi-map-form :side-effect wraps progn recursion in labels."
  (let* ((form (cl-cc/expand::%sequence-build-multi-map-form :side-effect '#'print '(l1)))
         (helper-body (third (first (second (third form))))))
    (assert-eq 'let (first form))
    (assert-eq 'if (first helper-body))))

;;; ── %sequence-build-single-quantifier-form ───────────────────────────────

(deftest sequence-build-single-quantifier-every-shape
  "%sequence-build-single-quantifier-form :every wraps dolist with unless/return nil."
  (let* ((form (cl-cc/expand::%sequence-build-single-quantifier-form :every '#'evenp 'xs))
         (block-form (third form))
         (dolist-form (second block-form)))
    (assert-eq 'let (first form))
    (assert-eq 'block (first block-form))
    (assert-eq 'dolist (first dolist-form))))

(deftest sequence-build-single-quantifier-some-shape
  "%sequence-build-single-quantifier-form :some wraps dolist with when/return."
  (let* ((form (cl-cc/expand::%sequence-build-single-quantifier-form :some '#'oddp 'xs))
         (block-form (third form))
         (dolist-form (second block-form)))
    (assert-eq 'let (first form))
    (assert-eq 'block (first block-form))
    (assert-eq 'dolist (first dolist-form))))

;;; ── %sequence-build-multi-quantifier-form ────────────────────────────────

(deftest sequence-build-multi-quantifier-every-shape
  "%sequence-build-multi-quantifier-form :every builds labels-based recursion."
  (let ((form (cl-cc/expand::%sequence-build-multi-quantifier-form :every '#'= '(l1 l2))))
    (assert-eq 'let (first form))
    (assert-eq 'labels (first (third form)))))

(deftest sequence-build-multi-quantifier-some-shape
  "%sequence-build-multi-quantifier-form :some builds let-binding result in branch."
  (let ((form (cl-cc/expand::%sequence-build-multi-quantifier-form :some '#'= '(l1 l2))))
    (assert-eq 'let (first form))
    (assert-eq 'labels (first (third form)))))
