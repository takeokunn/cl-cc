;;;; tests/unit/vm/vm-run-tests.lisp — VM error dispatch tests

(in-package :cl-cc/test)

(defsuite vm-run-suite
  :description "Unit tests for vm-run.lisp error matching"
  :parent cl-cc-suite)

(in-suite vm-run-suite)

(deftest-each vm-error-type-matches
  "vm-error-type-matches-p dispatch table"
  :cases
  (("string-matches-error"
    "boom"  'error   t)
   ("string-matches-condition"
    "boom"  'condition t)
   ("string-matches-t"
    "boom"  't       t)
   ("string-no-match-specific-subtype"
    "boom"  'type-error nil)
   ("condition-object-matches-t"
    (make-condition 'simple-error :format-control "x") 't t))
  (error-val handler-type expected-result)
  (let ((actual (cl-cc::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (assert-true actual)
        (assert-false actual))))
