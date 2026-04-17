;;;; tests/unit/expand/macros-stdlib-sequence-map-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)

(defsuite macros-stdlib-sequence-map-suite
  :description "Tests for macros-stdlib.lisp: coerce/map/replace/merge"
  :parent cl-cc-unit-suite)

(in-suite macros-stdlib-sequence-map-suite)

(deftest-each coerce-quoted-type-expansions
  "COERCE with quoted type dispatches to the right coerce-to-* primitive"
  :cases (("to-string"        '(coerce v 'string)        "COERCE-TO-STRING")
          ("to-simple-string" '(coerce v 'simple-string) "COERCE-TO-STRING")
          ("to-list"          '(coerce v 'list)          "COERCE-TO-LIST")
          ("to-vector"        '(coerce v 'vector)        "COERCE-TO-VECTOR"))
  (form expected-name)
  (let ((result (our-macroexpand-1 form)))
    (assert-equal (symbol-name (car result)) expected-name)
    (assert-equal (cadr result) 'v)))

(deftest coerce-unquoted-type-fallback
  "COERCE with a non-literal type dispatches to %coerce-runtime"
  (let ((result (our-macroexpand-1 '(coerce v type-var))))
    (assert-equal (symbol-name (car result)) "%COERCE-RUNTIME")
    (assert-equal (cadr result) 'v)))

(deftest map-delegates-to-mapcar-coerce
  "(map result-type fn seq) → (coerce (mapcar fn (coerce seq 'list)) result-type)"
  (assert-equal (our-macroexpand-1 '(map 'list fn seq))
                '(coerce (mapcar fn (coerce seq 'list)) 'list)))

(deftest-each dest-returning-sequence-expanders
  "Destination-returning operators: expansion contains the dest variable and returns it."
  :cases (("map-into" '(map-into dest fn src)))
  (form)
  (let* ((result    (our-macroexpand-1 form))
         (dest-var  (first (first (second result))))
         (last-form (car (last (cddr result)))))
    (assert-eq 'let (car result))
    (assert-eq last-form dest-var)))

(deftest replace-expansion-vector-path
  "REPLACE: outer LET* with runtime vector/list dispatch."
  (let* ((result (our-macroexpand-1 '(replace dest src))))
    (assert-eq 'let* (car result))))

(deftest merge-expansion
  "MERGE expands to a LET that binds inputs/predicate and a LABELS recursive helper."
  (let* ((result   (our-macroexpand-1 '(merge 'list l1 l2 pred)))
         (bindings (second result))
         (body     (caddr result)))
    (assert-eq 'let (car result))
    (assert-= 3 (length bindings))
    (assert-eq 'labels (car body))))
