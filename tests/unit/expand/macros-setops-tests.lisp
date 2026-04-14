;;;; tests/unit/expand/macros-setops-tests.lisp — Set operations macro tests

(in-package :cl-cc/test)

(defsuite macros-setops-suite
  :description "Set operation macro tests"
  :parent cl-cc-unit-suite)

(in-suite macros-setops-suite)

(deftest-each macros-setops-expand-to-let
  "All set-operation macros expand to a LET at the top level."
  :cases (("remove"            '(remove x xs))
          ("member"            '(member x xs))
          ("remove-duplicates" '(remove-duplicates xs))
          ("union"             '(union xs ys))
          ("set-difference"    '(set-difference xs ys))
          ("intersection"      '(intersection xs ys))
          ("subsetp"           '(subsetp xs ys))
          ("adjoin"            '(adjoin x xs))
          ("rassoc"            '(rassoc x alist))
          ("pairlis"           '(pairlis keys data)))
  (form)
  (assert-eq 'let (car (our-macroexpand-1 form))))
