;;;; tests/unit/expand/macro-multiple-value-tests.lisp — Macro multiple-value tests

(in-package :cl-cc/test)

(defsuite macro-multiple-value-suite
  :description "Macro multiple-value expansion tests"
  :parent cl-cc-suite)

(deftest multiple-value-list-expansion
  "MULTIPLE-VALUE-LIST: outer LET, body is multiple-value-call; fully expands away."
  ;; Any form: outer LET + multiple-value-call in body
  (let ((result (our-macroexpand-1 '(multiple-value-list (values 1 2 3)))))
    (assert-eq 'let (car result))
    (assert-eq 'multiple-value-call (car (caddr result))))
  (let ((result (our-macroexpand-1 '(multiple-value-list (foo)))))
    (assert-eq 'let (car result))
    (assert-eq 'multiple-value-call (caaddr result)))
  ;; Full expansion removes the macro entirely
  (let ((result (our-macroexpand '(multiple-value-list (values 1 2 3)))))
    (assert-false (search "multiple-value-list" (string-downcase (format nil "~S" result))))))

(deftest multiple-value-bind-macro-expansion
  "MULTIPLE-VALUE-BIND: expands to multiple-value-call+lambda for all input shapes."
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b c) (values 1 2 3) body1 body2))))
    (assert-eq 'multiple-value-call (car result))
    (assert-eq 'lambda (caadr result))
    (assert-equal '(a b c) (cadr (cadr result)))
    (assert-eq 'values (caaddr result)))
  (let ((result (our-macroexpand-1 '(multiple-value-bind (x) (foo) body))))
    (assert-eq 'multiple-value-call (car result))
    (assert-eq 'lambda (caadr result))
    (assert-equal '(x) (cadr (cadr result))))
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b) (values 1 2)))))
    (assert-eq 'multiple-value-call (car result))
    (assert-null (cddr (cadr result)))))

(deftest multiple-value-setq-macro-expansion
  "MULTIPLE-VALUE-SETQ: outer LET binding from multiple-value-list, for all input shapes."
  (let* ((result   (our-macroexpand-1 '(multiple-value-setq (a b c) (values 1 2 3))))
         (bindings (cadr result)))
    (assert-eq 'let (car result))
    (assert-true (consp bindings))
    (assert-eq 'multiple-value-list (caadar bindings)))
  (let ((result (our-macroexpand-1 '(multiple-value-setq (a b) (values 1 2) extra-body))))
    (assert-eq 'let (car result))
    (assert-= 5 (length result)))
  (let* ((result   (our-macroexpand-1 '(multiple-value-setq (x) (foo))))
         (bindings (cadr result)))
    (assert-eq 'let (car result))
    (assert-eq 'multiple-value-list (caadar bindings))))
