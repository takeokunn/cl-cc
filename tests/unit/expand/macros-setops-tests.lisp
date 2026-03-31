;;;; tests/unit/expand/macros-setops-tests.lisp — Set operations macro tests

(in-package :cl-cc/test)

(defsuite macros-setops-suite
  :description "Set operation macro tests"
  :parent cl-cc-suite)

(in-suite macros-setops-suite)

(deftest macros-setops-remove-and-member
  "REMOVE and MEMBER expand to explicit loops."
  (assert-eq 'let (car (our-macroexpand-1 '(remove x xs))))
  (assert-eq 'let (car (our-macroexpand-1 '(member x xs)))))

(deftest macros-setops-dedup-and-union
  "REMOVE-DUPLICATES and UNION expand to accumulation forms."
  (assert-eq 'let (car (our-macroexpand-1 '(remove-duplicates xs))))
  (assert-eq 'let (car (our-macroexpand-1 '(union xs ys)))))

(deftest macros-setops-derived-operations
  "SET-DIFFERENCE, INTERSECTION, SUBSETP, ADJOIN, RASSOC, and PAIRLIS expand."
  (assert-eq 'let (car (our-macroexpand-1 '(set-difference xs ys))))
  (assert-eq 'let (car (our-macroexpand-1 '(intersection xs ys))))
  (assert-eq 'let (car (our-macroexpand-1 '(subsetp xs ys))))
  (assert-eq 'let (car (our-macroexpand-1 '(adjoin x xs))))
  (assert-eq 'let (car (our-macroexpand-1 '(rassoc x alist))))
  (assert-eq 'let (car (our-macroexpand-1 '(pairlis keys data)))))
