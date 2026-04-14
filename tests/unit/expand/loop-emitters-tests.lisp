;;;; tests/unit/expand/loop-emitters-tests.lisp — LOOP emitter layer unit tests

(in-package :cl-cc/test)

(defsuite loop-emitters-suite
  :description "LOOP emitter layer unit tests"
  :parent cl-cc-unit-suite)

(in-suite loop-emitters-suite)

(deftest loop-emitters-register-core-functions
  "Emitter dispatch tables contain the canonical LOOP emitter registrations."
  (assert-true (functionp (gethash :from cl-cc::*loop-iter-emitters*)))
  (assert-true (functionp (gethash :sum cl-cc::*loop-acc-emitters*)))
  (assert-true (functionp (gethash :while cl-cc::*loop-condition-emitters*))))

(deftest loop-iter-emitter-from-produces-boundary-tests
  "The :FROM emitter produces the expected bindings, exit tests, and step form."
  (multiple-value-bind (bindings end-tests pre-body step-forms)
      (funcall (gethash :from cl-cc::*loop-iter-emitters*)
               'i
               '(:from 1 :to 5 :by 2))
    (assert-equal '((i 1)) bindings)
    (assert-equal '((> i 5)) end-tests)
    (assert-null pre-body)
    (assert-equal '((setq i (+ i 2))) step-forms)))

(deftest loop-acc-emitter-sum-accumulates-numerically
  "The :SUM emitter initializes and updates a numeric accumulator."
  (multiple-value-bind (body bindings result-form)
      (funcall (gethash :sum cl-cc::*loop-acc-emitters*)
               'total
               'item
               nil
               nil
               nil)
    (assert-equal '(setq total (+ total item)) body)
    (assert-equal '((total 0)) bindings)
    (assert-null result-form)))

(deftest loop-condition-emitter-while-emits-goto
  "The :WHILE emitter terminates the loop by jumping to END-TAG."
  (assert-equal '(unless keep-going (go done))
                (funcall (gethash :while cl-cc::*loop-condition-emitters*)
                         'keep-going
                         'done)))
