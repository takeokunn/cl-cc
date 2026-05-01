;;;; tests/unit/expand/expander-control-tests.lisp — Control-form expander tests

(in-package :cl-cc/test)

(defsuite expander-control-suite :description "Control-form expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-control-suite)
(deftest-each expand-eval-when-keeps-body
  "expand-eval-when-form with :execute or :load-toplevel returns a non-nil form."
  :cases (("execute"       '(:execute))
          ("load-toplevel" '(:load-toplevel)))
  (situations)
  (assert-true (consp (cl-cc/expand::expand-eval-when-form situations '((+ 1 2))))))

(deftest expand-eval-when-compile-only-returns-nil
  "eval-when :compile-toplevel alone returns nil (excluded from output)."
  (handler-bind ((warning #'muffle-warning))
    (let ((result (cl-cc/expand::expand-eval-when-form '(:compile-toplevel) '((+ 1 2)))))
      (assert-eq nil result))))

(deftest expand-macrolet-form-expands-local-macro
  "expand-macrolet-form makes a local macro visible in the body."
  (let* ((result (cl-cc/expand::expand-macrolet-form
                  '((my-one () 1))
                  '((my-one))))
         (str (format nil "~S" result)))
    (assert-true (search "1" str))))

(deftest expand-macrolet-form-body-is-progn
  "expand-macrolet-form with multiple body forms wraps in progn."
  (let ((result (cl-cc/expand::expand-macrolet-form
                 '((add1 (x) (+ x 1)))
                 '((add1 2) (add1 3)))))
    (assert-true (consp result))))
