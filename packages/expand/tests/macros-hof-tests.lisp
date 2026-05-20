;;;; tests/unit/expand/macros-hof-tests.lisp
;;;; Coverage tests for src/expand/macros-hof.lisp
;;;;
;;;; Macros tested: mapcar, every, some, notany, notevery, find,
;;;; position, count, mapcan, stable-sort

(in-package :cl-cc/test)

(defsuite macros-hof-suite
  :description "Tests for macros-hof.lisp"
  :parent cl-cc-unit-suite)


(in-suite macros-hof-suite)
;;; ── HOF macros ───────────────────────────────────────────────────────────────

(deftest-each hof-macro-outer-is-let
  "Each HOF macro expands to a LET as its outermost form."
  :cases (("mapcar"        '(mapcar fn lst))
          ("every"         '(every pred lst))
          ("some"          '(some pred lst))
          ("remove-if"     '(remove-if pred lst))
          ("remove-if-not" '(remove-if-not pred lst)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest mapcar-body-contains-dolist
  "MAPCAR's list path iterates with DOLIST, possibly under runtime TYPECASE dispatch."
  (let ((result (our-macroexpand-1 '(mapcar fn lst))))
    (assert-true (%tree-contains-head-p 'dolist result))))

(deftest every-short-circuits-on-false
  "EVERY has a BLOCK NIL short-circuit path containing DOLIST, possibly under TYPECASE."
  (let ((result (our-macroexpand-1 '(every pred lst))))
    (assert-true (%tree-contains-head-p 'block result))
    (assert-true (%tree-contains-head-p 'dolist result))))

(deftest-each notany-notevery-negation
  "notany/notevery are simple (not ...) wrappers around some/every."
  :cases (("notany"   '(notany   pred lst) '(not (some  pred lst)))
          ("notevery" '(notevery pred lst) '(not (every pred lst))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest find-no-keys-is-eql-loop
  "FIND without keyword args generates a fast EQL check loop, possibly under TYPECASE."
  (let ((result (our-macroexpand-1 '(find item lst))))
    (assert-true (%tree-contains-head-p 'block result))
    (assert-true (%tree-contains-head-p 'dolist result))))

(deftest-each sequence-search-macro-outer-is-let
  "Sequence-search HOF macros (position/count/mapcan) expand to a LET."
  :cases (("position" '(position item lst))
          ("count"    '(count item lst))
          ("mapcan"   '(mapcan fn lst)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest stable-sort-delegates-to-sort
  "(stable-sort lst pred) → (sort lst pred)"
  (assert-equal (our-macroexpand-1 '(stable-sort lst pred))
                '(sort lst pred)))
