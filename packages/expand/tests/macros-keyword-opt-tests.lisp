;;;; FR-673 keyword argument dispatch optimization tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest fr-673-static-keyword-call-becomes-positional
  "FR-673: static &key call sites lower to positional values plus a bitmask."
  (multiple-value-bind (form reason)
      (cl-cc/expand:keyword-optimize-call
       'target '(x &key (a 10) (b 20)) '(1 :b 2 :a 3))
    (assert-eq :static-keywords reason)
    (assert-equal '(%keyword-positional-call target 3 1 3 2) form)))

(deftest fr-673-allow-other-keys-forces-conservative-path
  "FR-673: &ALLOW-OTHER-KEYS must not use static keyword lowering."
  (multiple-value-bind (form reason)
      (cl-cc/expand:keyword-optimize-call
       'target '(x &key a &allow-other-keys) '(1 :a 2 :unknown 3))
    (assert-eq :allow-other-keys reason)
    (assert-true (null form))))

(deftest fr-673-runtime-bitmask-dispatch-uses-popcount-index
  "FR-673: compact value vectors are indexed by POPCNT of lower mask bits."
  (let* ((positions '((:a . 0) (:b . 1) (:c . 2)))
         (args '(:c 30 :a 10))
         (mask (cl-cc/expand:%keyword-call-bitmask args positions))
         (values (cl-cc/expand:%keyword-values-vector args positions)))
    (assert-= 5 mask)
    (assert-= 10 (cl-cc/expand:%keyword-dispatch-value 0 mask values :missing))
    (assert-eq :missing (cl-cc/expand:%keyword-dispatch-value 1 mask values :missing))
    (assert-= 30 (cl-cc/expand:%keyword-dispatch-value 2 mask values :missing))))
