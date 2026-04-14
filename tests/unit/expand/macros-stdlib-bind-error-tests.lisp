;;;; tests/unit/expand/macros-stdlib-bind-error-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)

(defsuite macros-stdlib-bind-error-suite
  :description "Tests for macros-stdlib.lisp: bindings and error helpers"
  :parent cl-cc-unit-suite)

(in-suite macros-stdlib-bind-error-suite)

(deftest with-slots-expansion
  "WITH-SLOTS: outer LET binds instance; inner SYMBOL-MACROLET binds each slot."
  (let* ((result     (our-macroexpand-1 '(with-slots (x) obj body)))
         (inner-form (caddr result))
         (binding    (caadr inner-form)))
    (assert-eq (car result)     'let)
    (assert-eq (car inner-form) 'symbol-macrolet)
    (assert-eq (car binding)    'x)
    (assert-eq (caadr binding)  'slot-value)))

(deftest nth-value-expansion
  "NTH-VALUE with constant N expands to MULTIPLE-VALUE-BIND selecting the Nth var."
  (let* ((result (our-macroexpand-1 '(nth-value 1 form))))
    (assert-eq (car result) 'multiple-value-bind)
    (assert-= (length (second result)) 2)))

(deftest destructuring-bind-expansion
  "DESTRUCTURING-BIND outer form is LET; inner form is LET* with destructured bindings."
  (let* ((result (our-macroexpand-1 '(destructuring-bind (a b) expr body)))
         (inner  (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car inner)  'let*)))

(deftest assert-expansion
  "ASSERT: outer is UNLESS, body is CERROR (continuable); custom datum is passed through."
  (let* ((basic  (our-macroexpand-1 '(assert test)))
         (custom (our-macroexpand-1 '(assert test () "bad input"))))
    (assert-eq    (car  basic)          'unless)
    (assert-equal (cadr basic)          'test)
    (assert-eq    (car (caddr basic))   'cerror)
    (assert-eq    (car (caddr custom))  'cerror)
    (assert-equal (caddr (caddr custom)) "bad input")))

(deftest ignore-errors-expansion
  "IGNORE-ERRORS wraps in HANDLER-CASE with an ERROR handler clause."
  (let* ((result (our-macroexpand-1 '(ignore-errors expr)))
         (clause (caddr result)))
    (assert-eq (car result) 'handler-case)
    (assert-eq (car clause) 'error)))
