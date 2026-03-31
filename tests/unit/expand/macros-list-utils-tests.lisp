;;;; tests/unit/expand/macros-list-utils-tests.lisp — List utility macro tests

(in-package :cl-cc/test)

(defsuite macros-list-utils-suite
  :description "List utility macro tests"
  :parent cl-cc-suite)

(in-suite macros-list-utils-suite)

(deftest macros-list-utils-sort-expands
  "SORT expands to a merge-sort labels form."
  (assert-eq 'let (car (our-macroexpand-1 '(sort lst #'<)))))

(deftest macros-list-utils-list*-and-pushnew
  "LIST* and PUSHNEW expand to the expected core forms."
  (assert-equal 'x (our-macroexpand-1 '(list* x)))
  (assert-eq 'let (car (our-macroexpand-1 '(pushnew x xs)))))

(deftest macros-list-utils-stable-sort-and-nreconc
  "STABLE-SORT delegates to SORT and NRECONC delegates to NCONC/NREVERSE."
  (assert-equal '(sort xs #'<) (our-macroexpand-1 '(stable-sort xs #'<)))
  (assert-eq 'nconc (car (our-macroexpand-1 '(nreconc xs tail)))))
