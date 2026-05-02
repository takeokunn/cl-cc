;;;; tests/unit/expand/expander-setf-tests.lisp — Setf expander tests

(in-package :cl-cc/test)

(defsuite expander-setf-suite :description "Setf expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-setf-suite)
;;; ─── setf expansions ────────────────────────────────────────────────────────

(deftest expander-setf-multi-var-progn
  "compiler-macroexpand-all: (setf x 1 y 2) expands to progn of setq."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf x 1 y 2))))
    (assert-eq 'progn (car result))
    (assert-eq 'setq (car (second result)))
    (assert-eq 'setq (car (third result)))))

(deftest expander-setf-plain-var-to-setq
  "compiler-macroexpand-all: (setf x 42) expands to (setq x 42)."
  (let ((result (assert-expansion-head '(setf x 42) 'setq)))
    (assert-eq 'x (second result))
    (assert-equal 42 (third result))))

(deftest expander-setf-aref-to-aset
  "compiler-macroexpand-all: (setf (aref v i) val) expands to (aset ...)."
  (let ((result (assert-expansion-head '(setf (aref v i) val) 'cl-cc/expand::aset)))
    (assert-eq 'v (second result))
    (assert-eq 'i (third result))
    (assert-eq 'val (fourth result))))

(deftest expander-setf-multi-place-dispatches-each
  "(setf a 1 b 2) expands to progn of two individual setf/setq forms."
  (let ((result (assert-expansion-head '(setf a 1 b 2) 'progn)))
    (assert-= 2 (length (cdr result)))))
