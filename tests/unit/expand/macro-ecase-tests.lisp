;;;; tests/unit/expand/macro-ecase-tests.lisp — ECASE macro tests

(in-package :cl-cc/test)

(defsuite macro-ecase-suite
  :description "ECASE expansion tests"
  :parent cl-cc-suite)

(in-suite macro-ecase-suite)

(deftest ecase-expands-to-let-with-case
  "ECASE expands to a LET binding the key then a CASE with otherwise error"
  (let ((result (our-macroexpand-1 '(ecase x (1 'one) (2 'two)))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 1)
    (assert-eq (cadr (caadr result)) 'x)
    (let ((case-form (caddr result)))
      (assert-eq (car case-form) 'case)
      (let ((tmp-var (first (caadr result))))
        (assert-eq (cadr case-form) tmp-var)))))

(deftest ecase-has-otherwise-error-clause
  "ECASE expansion includes an OTHERWISE clause that calls ERROR"
  (let* ((result (our-macroexpand-1 '(ecase x (a 1) (b 2))))
         (case-form (caddr result))
         (clauses (cddr case-form))
         (otherwise-clause (find 'otherwise clauses :key #'car)))
    (assert-true otherwise-clause)
    (assert-eq (caadr otherwise-clause) 'error)))

(deftest ecase-preserves-user-clauses
  "ECASE preserves user clauses verbatim before the otherwise clause"
  (let* ((result (our-macroexpand-1 '(ecase val (:foo 'foo-result) (:bar 'bar-result))))
         (case-form (caddr result))
         (user-clauses (butlast (cddr case-form))))
    (assert-= (length user-clauses) 2)
    (assert-eq (car (first user-clauses)) :foo)
    (assert-eq (car (second user-clauses)) :bar)))

(deftest ecase-empty-cases-still-has-otherwise
  "ECASE with no user cases still has the error-signalling otherwise clause"
  (let* ((result (our-macroexpand-1 '(ecase x)))
         (case-form (caddr result))
         (clauses (cddr case-form)))
    (assert-= (length clauses) 1)
    (assert-eq (car (first clauses)) 'otherwise)))

(deftest integration-ecase-full-expansion
  "Full expansion of ECASE: top level is not ECASE (macro was expanded)"
  (let ((result (our-macroexpand '(ecase x (1 'one) (2 'two)))))
    (assert-eq (car result) 'let)))
