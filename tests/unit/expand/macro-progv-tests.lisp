;;;; tests/unit/expand/macro-progv-tests.lisp — PROGV macro tests

(in-package :cl-cc/test)

(defsuite macro-progv-suite
  :description "PROGV expansion tests"
  :parent cl-cc-suite)

(in-suite macro-progv-suite)

(deftest progv-basic-structure
  "PROGV expands to a LET* that calls %progv-enter then UNWIND-PROTECT"
  (let ((result (our-macroexpand-1 '(progv '(x y) '(1 2) body))))
    (assert-eq (car result) 'let*)
    (assert-= (length (cadr result)) 3)))

(deftest progv-binds-symbols-and-values
  "PROGV LET* bindings capture the symbols and values expressions"
  (let* ((result (our-macroexpand-1 '(progv sym-list val-list body-form)))
         (bindings (cadr result)))
    (assert-eq (cadr (first bindings)) 'sym-list)
    (assert-eq (cadr (second bindings)) 'val-list)))

(deftest progv-calls-progv-enter
  "PROGV third binding calls %progv-enter with the syms and vals temps"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(1) (print x))))
         (bindings (cadr result))
         (saved-binding (third bindings))
         (enter-call (second saved-binding)))
    (assert-eq (car enter-call) 'cl-cc::%progv-enter)))

(deftest progv-body-unwind-structure
  "PROGV body is UNWIND-PROTECT with PROGN body and %progv-exit cleanup."
  (let* ((result      (our-macroexpand-1 '(progv '(x) '(1) form1 form2)))
         (bindings    (cadr result))
         (saved-var   (first (third bindings)))
         (unwind-form (caddr result))
         (cleanup     (caddr unwind-form)))
    (assert-eq 'unwind-protect       (car unwind-form))
    (assert-eq 'progn                (car (cadr unwind-form)))
    (assert-eq 'cl-cc::%progv-exit   (car cleanup))
    (assert-eq saved-var             (cadr cleanup))))
