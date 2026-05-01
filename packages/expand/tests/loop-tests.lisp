;;;; tests/unit/expand/loop-tests.lisp — LOOP generator layer unit tests

(in-package :cl-cc/test)

(defsuite loop-suite
  :description "LOOP generator layer unit tests"
  :parent cl-cc-unit-suite)

(in-suite loop-suite)

(deftest loop-build-return-forms-prefers-result-form
  "%loop-build-return-forms returns the explicit result form when present."
  (assert-equal '((values foo bar))
                (cl-cc/expand::%loop-build-return-forms '(foo bar) nil nil))
  (assert-equal '(foo)
                (cl-cc/expand::%loop-build-return-forms '(foo) nil nil)))

(deftest loop-build-return-forms-falls-back-to-accumulators-and-vacuous-truth
  "%loop-build-return-forms falls back to accumulators or vacuous truth."
  (assert-equal '((values a b))
                (cl-cc/expand::%loop-build-return-forms nil '(b a) nil))
  (assert-equal '(t)
                (cl-cc/expand::%loop-build-return-forms nil nil '((:always t))))
  (assert-null (cl-cc/expand::%loop-build-return-forms nil nil nil)))

(deftest loop-replace-finish-substitutes-loop-finish-recursively
  "%loop-replace-finish rewrites LOOP-FINISH forms but preserves quoted trees."
  (assert-equal '((go done) (quote (loop-finish)) (if x (go done) y))
                (cl-cc/expand::%loop-replace-finish
                 '((loop-finish) '(loop-finish) (if x (loop-finish) y))
                 'done)))
