;;;; tests/unit/expand/macro-multiple-value-tests.lisp — Macro multiple-value tests

(in-package :cl-cc/test)

(defsuite macro-multiple-value-suite
  :description "Macro multiple-value expansion tests"
  :parent cl-cc-unit-suite)


(in-suite macro-multiple-value-suite)
(deftest multiple-value-list-expansion
  "MULTIPLE-VALUE-LIST: explicit VALUES becomes LIST; general forms keep the capture path."
  (let ((result (our-macroexpand-1 '(multiple-value-list (values 1 2 3)))))
    (assert-true (member (car result) '(list let))))
  (let ((result (our-macroexpand-1 '(multiple-value-list (foo)))))
    (assert-eq 'let (car result))
    (assert-true (consp (second result))))
  ;; Full expansion removes the macro entirely
  (let ((result (our-macroexpand '(multiple-value-list (values 1 2 3)))))
    (assert-false (search "multiple-value-list" (string-downcase (format nil "~S" result))))))

(deftest multiple-value-bind-macro-expansion
  "MULTIPLE-VALUE-BIND: expands through values-list capture or direct VALUES binding."
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b c) (values 1 2 3) body1 body2))))
    (assert-true (member (car result) '(let let*))))
  (let ((result (our-macroexpand-1 '(multiple-value-bind (x) (foo) body))))
    (assert-eq 'let (car result))
    (assert-eq 'multiple-value-list (caadar (second result))))
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b) (values 1 2)))))
    (assert-true (member (car result) '(let let*)))))

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
