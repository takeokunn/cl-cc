;;;; tests/unit/expand/macro-rotatef-tests.lisp — Rotatef macro tests

(in-package :cl-cc/test)

(defsuite macro-rotatef-suite
  :description "ROTATEF expansion tests"
  :parent cl-cc-suite)

(in-suite macro-rotatef-suite)

(deftest rotatef-two-var-structure
  "ROTATEF with two vars expands to a LET + two SETQs returning nil"
  (let ((result (our-macroexpand-1 '(rotatef x y))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 1)
    (let* ((binding (caadr result))
           (tmp-var (first binding))
           (tmp-val (second binding)))
      (assert-eq tmp-val 'x)
      (assert-eq (car (caddr result)) 'setq)
      (assert-eq (cadr (caddr result)) 'x)
      (assert-eq (caddr (caddr result)) 'y)
      (assert-eq (car (cadddr result)) 'setq)
      (assert-eq (cadr (cadddr result)) 'y)
      (assert-eq (caddr (cadddr result)) tmp-var)
      (assert-null (car (last result)))))
  (let ((result (our-macroexpand-1 '(rotatef a b))))
    (assert-null (car (last result)))))

(deftest rotatef-single-var-returns-nil
  "ROTATEF with a single argument returns NIL (ANSI: identity)"
  (assert-null (our-macroexpand-1 '(rotatef x))))

(deftest rotatef-three-var-structure
  "ROTATEF with three arguments expands to LET + chain of SETFs returning nil"
  (let ((result (our-macroexpand-1 '(rotatef x y z))))
    (assert-eq (car result) 'let)
    (assert-null (car (last result)))))

(deftest rotatef-preserves-places
  "ROTATEF correctly names both places in the expansion"
  (let ((result (our-macroexpand-1 '(rotatef foo bar))))
    (assert-eq (cadr (caadr result)) 'foo)
    (assert-eq (cadr (caddr result)) 'foo)
    (assert-eq (caddr (caddr result)) 'bar)
    (assert-eq (cadr (cadddr result)) 'bar)))

(deftest integration-rotatef-full-expansion
  "Full expansion of ROTATEF does not contain the ROTATEF symbol"
  (let ((result (our-macroexpand '(rotatef p q))))
    (assert-false (search "rotatef" (string-downcase (format nil "~S" result))))))
