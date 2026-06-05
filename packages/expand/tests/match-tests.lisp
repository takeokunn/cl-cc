;;;; packages/expand/tests/match-tests.lisp --- FR-779/FR-780 MATCH tests

(in-package :cl-cc/test)

(defsuite match-suite
  :description "FR-779 structural pattern matching and FR-780 exhaustiveness checks"
  :parent cl-cc-unit-suite)

(in-suite match-suite)

(defun %match-test-eval (form)
  (eval (our-macroexpand-all form)))

(deftest match-literal-variable-and-wildcard-patterns
  "MATCH supports literal arms, variable binding, and wildcard fallback."
  :tags '(:fr-779)
  (assert-equal :one
                (%match-test-eval '(match 1
                                     (0 :zero)
                                     (1 :one)
                                     (_ :other))))
  (assert-equal 42
                (%match-test-eval '(match 41
                                     (0 :zero)
                                     (x (+ x 1)))))
  (assert-equal :fallback
                (%match-test-eval '(match :unknown
                                     (:known :known)
                                     (_ :fallback)))))

(deftest match-cons-list-and-vector-patterns
  "MATCH destructures cons cells, proper lists, and vectors."
  :tags '(:fr-779)
  (assert-equal 3
                (%match-test-eval '(match '(1 . 2)
                                     ((cons x y) (+ x y))
                                     (_ :no))))
  (assert-equal 6
                (%match-test-eval '(match '(1 2 3)
                                     ((list a b c) (+ a b c))
                                     (_ :no))))
  (assert-equal 9
                (%match-test-eval '(match #(4 5)
                                     ((vector a b) (+ a b))
                                     (_ :no)))))

(deftest match-type-and-guard-patterns
  "MATCH supports type checks plus WHEN/AND/OR guard combinators."
  :tags '(:fr-779)
  (assert-equal 10
                (%match-test-eval '(match 5
                                     ((type integer n) (* n 2))
                                     (_ :no))))
  (assert-equal :positive-even
                (%match-test-eval '(match 4
                                     ((when (type integer n)
                                        (and (> n 0) (evenp n)))
                                      :positive-even)
                                     (_ :no))))
  (assert-equal :small
                (%match-test-eval '(match 1
                                     ((or 0 1 2) :small)
                                     (_ :large))))
  (assert-equal :non-empty-list
                (%match-test-eval '(match '(a b)
                                     ((and (type cons) (cons head tail))
                                      (declare (ignore head tail))
                                      :non-empty-list)
                                     (_ :no)))))

(deftest match-non-exhaustive-pattern-signals-compiler-style-warning
  "FR-780: non-exhaustive MATCH forms signal a compiler style-warning."
  :tags '(:fr-780)
  (let ((seen nil))
    (handler-bind ((cl-cc/expand:match-exhaustiveness-warning
                     (lambda (warning)
                       (setf seen warning)
                       (muffle-warning))))
      (our-macroexpand-1 '(match x
                           (0 :zero)
                           (1 :one))))
    (assert-true seen)
    (assert-true (typep seen 'cl-cc/vm:compiler-style-warning))
    (assert-equal "W0780" (cl-cc/vm:compiler-diagnostic-error-code seen))))

(deftest match-exhaustive-pattern-does-not-warn
  "FR-780: wildcard/variable final arms make MATCH exhaustive."
  :tags '(:fr-780)
  (let ((warnings nil))
    (handler-bind ((cl-cc/expand:match-exhaustiveness-warning
                     (lambda (warning)
                       (push warning warnings)
                       (muffle-warning))))
      (our-macroexpand-1 '(match x
                           (0 :zero)
                           (_ :other))))
    (assert-null warnings)))

(deftest match-unreachable-pattern-signals-warning
  "FR-780: duplicate or post-catch-all MATCH arms are reported as unreachable."
  :tags '(:fr-780)
  (let ((warnings nil))
    (handler-bind ((cl-cc/expand:match-unreachable-pattern-warning
                     (lambda (warning)
                       (push warning warnings)
                       (muffle-warning)))
                   (cl-cc/expand:match-exhaustiveness-warning #'muffle-warning))
      (our-macroexpand-1 '(match x
                           (1 :first)
                           (1 :duplicate)
                           (_ :other)
                           (2 :after-wildcard))))
    (assert-= 2 (length warnings))
    (assert-true (every (lambda (warning)
                          (equal "W0781"
                                 (cl-cc/vm:compiler-diagnostic-error-code warning)))
                        warnings))))
