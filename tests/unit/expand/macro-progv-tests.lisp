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

(deftest progv-uses-unwind-protect
  "PROGV body is wrapped in UNWIND-PROTECT ensuring %progv-exit on exit"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(42) (print x))))
         (body-form (caddr result)))
    (assert-eq (car body-form) 'unwind-protect)))

(deftest progv-cleanup-calls-progv-exit
  "PROGV cleanup form calls %progv-exit with the saved bindings variable"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(1) body)))
         (bindings (cadr result))
         (saved-var (first (third bindings)))
         (unwind-form (caddr result))
         (cleanup-form (caddr unwind-form)))
    (assert-eq (car cleanup-form) 'cl-cc::%progv-exit)
    (assert-eq (cadr cleanup-form) saved-var)))

(deftest progv-body-wrapped-in-progn
  "PROGV body forms are wrapped in a PROGN inside unwind-protect"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(1) form1 form2)))
         (unwind-form (caddr result))
         (protected-form (cadr unwind-form)))
    (assert-eq (car protected-form) 'progn)))
