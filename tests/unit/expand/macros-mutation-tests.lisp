;;;; tests/unit/expand/macros-mutation-tests.lisp
;;;; Mutation macro coverage for src/expand/macros-mutation.lisp

(in-package :cl-cc/test)

(defsuite macros-mutation-suite
  :description "Tests for mutation-style stdlib macros"
  :parent cl-cc-integration-suite)

(in-suite macros-mutation-suite)

(deftest push-expands-to-setf-cons
  "PUSH expands to (setf place (cons value place)) — no gensym"
  (assert-equal (our-macroexpand-1 '(push v lst))
                '(setf lst (cons v lst))))

(deftest pop-expansion
  "POP expands to (let ((tmp place)) (setf place (cdr tmp)) (car tmp)) — reads place once."
  (let* ((result      (our-macroexpand-1 '(pop lst)))
         (bindings    (cadr result))       ; ((#:TMP lst))
         (setf-form   (caddr result))      ; (setf lst (cdr #:TMP))
         (tmp-sym     (caar bindings)))    ; the gensym bound to lst
    (assert-eq   (car result)               'let)
    ;; Binding binds tmp gensym to lst
    (assert-equal (cadar bindings)          'lst)
    ;; Setf form structure: (setf lst (cdr tmp))
    (assert-eq   (car setf-form)            'setf)
    (assert-eq   (cadr setf-form)           'lst)
    ;; Value arg to setf is (cdr tmp)
    (assert-eq   (car (caddr setf-form))    'cdr)
    (assert-eq   (cadr (caddr setf-form))   tmp-sym)))

(deftest-each incf-decf-expansion
  "incf/decf expand to (setq x (OP x delta)) for simple symbol places."
  :cases (("incf-default" '(incf x)   '(setq x (+ x 1)))
          ("incf-custom"  '(incf x 5) '(setq x (+ x 5)))
          ("decf-default" '(decf x)   '(setq x (- x 1)))
          ("decf-custom"  '(decf x 3) '(setq x (- x 3))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest pushnew-default-expansion
  "PUSHNEW with no :test uses default #'eql in MEMBER call."
  (let* ((result    (our-macroexpand-1 '(pushnew item place)))
         (unless-form (caddr result))
         (member-call (second unless-form)))
    (assert-eq (car result)      'let)
    (assert-eq (car unless-form) 'unless)
    (assert-eq (car member-call) 'member)))

(deftest pushnew-with-test-passes-test-to-member
  "PUSHNEW :test keyword is forwarded to the MEMBER call."
  (let* ((result      (our-macroexpand-1 '(pushnew item place :test #'equal)))
         (unless-form (caddr result))
         (member-call (second unless-form))
         (last-arg    (car (last member-call))))
    (assert-= (length member-call) 5)
    (assert-equal last-arg '#'equal)))

(deftest-each pushnew-runtime-behavior
  "PUSHNEW adds missing elements but skips duplicates."
  :cases (("adds-missing"  4 "(let ((lst (list 1 2 3))) (pushnew 4 lst) (length lst))")
          ("no-duplicate"  3 "(let ((lst (list 1 2 3))) (pushnew 2 lst) (length lst))"))
  (expected code)
  (assert-= expected (run-string code)))
