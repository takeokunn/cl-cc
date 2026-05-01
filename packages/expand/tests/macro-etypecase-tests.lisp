;;;; tests/unit/expand/macro-etypecase-tests.lisp — ETYPECASE macro tests

(in-package :cl-cc/test)

(defsuite macro-etypecase-suite
  :description "ETYPECASE expansion tests"
  :parent cl-cc-unit-suite)

(in-suite macro-etypecase-suite)

(deftest etypecase-expands-to-let-with-typecase
  "ETYPECASE expands to a LET binding the key then a TYPECASE with otherwise error"
  (let ((result (our-macroexpand-1 '(etypecase x (string 'str) (integer 'int)))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 1)
    (assert-eq (cadr (caadr result)) 'x)
    (let ((typecase-form (caddr result)))
      (assert-eq (car typecase-form) 'typecase))))

(deftest etypecase-has-otherwise-error-clause
  "ETYPECASE expansion includes an OTHERWISE clause that calls ERROR"
  (let* ((result (our-macroexpand-1 '(etypecase x (string 'str))))
         (typecase-form (caddr result))
         (clauses (cddr typecase-form))
         (otherwise-clause (find 'otherwise clauses :key #'car)))
    (assert-true otherwise-clause)
    (assert-eq (caadr otherwise-clause) 'error)))

(deftest etypecase-preserves-user-type-clauses
  "ETYPECASE preserves user type clauses before the otherwise clause"
  (let* ((result (our-macroexpand-1 '(etypecase v (string "s") (integer "i") (symbol "sym"))))
         (typecase-form (caddr result))
         (user-clauses (butlast (cddr typecase-form))))
    (assert-= (length user-clauses) 3)
    (assert-eq (car (first user-clauses)) 'string)
    (assert-eq (car (second user-clauses)) 'integer)
    (assert-eq (car (third user-clauses)) 'symbol)))

(deftest etypecase-key-var-used-in-typecase
  "ETYPECASE: the TYPECASE form tests the temp variable, not the raw keyform"
  (let* ((result (our-macroexpand-1 '(etypecase (foo-call) (integer 0))))
         (tmp-var (first (caadr result)))
         (typecase-form (caddr result)))
    (assert-eq (cadr typecase-form) tmp-var)))

(deftest integration-etypecase-full-expansion
  "Full expansion of ETYPECASE: top level is not ETYPECASE (macro was expanded)"
  (let ((result (our-macroexpand '(etypecase v (string "s") (integer "i")))))
    (assert-eq (car result) 'let)))
