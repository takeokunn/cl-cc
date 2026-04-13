;;;; tests/unit/expand/loop-data-tests.lisp — LOOP data layer unit tests

(in-package :cl-cc/test)

(defsuite loop-data-suite
  :description "LOOP data layer unit tests"
  :parent cl-cc-suite)

(in-suite loop-data-suite)

(deftest-each loop-boundary-keywords-contain-core-markers
  "The LOOP boundary keyword set contains the canonical control markers."
  :cases (("for"     "FOR")
          ("collect" "COLLECT")
          ("finally" "FINALLY")
          ("named"   "NAMED"))
  (kw)
  (assert-true (member kw cl-cc::*loop-boundary-keywords* :test #'string=)))

(deftest loop-vacuous-truth-conditions-are-the-expected-two-symbols
  "The vacuous-truth condition set is exactly the ANSI LOOP pair."
  (assert-equal '(:always :never) cl-cc::*loop-vacuous-truth-conditions*))

(deftest-each loop-accum-keyword-table-maps-canonical-and-ing-forms
  "Canonical and -ING accumulation keywords map to the same internal type."
  :cases (("collect"    "COLLECT"    :collect)
          ("collecting" "COLLECTING" :collect)
          ("summing"    "SUMMING"    :sum)
          ("nconcing"   "NCONCING"   :nconc))
  (kw expected)
  (assert-eq expected (cdr (assoc kw cl-cc::*loop-accum-keyword-table* :test #'string=))))

(deftest-each loop-hash-keyword-tables-map-both-spellings
  "HASH-KEYS/HASH-VALUES and USING aliases are registered in the data tables."
  :cases (("hash-keys"   "HASH-KEYS"  cl-cc::*loop-hash-iter-keywords* :hash-keys)
          ("hash-values" "HASH-VALUE" cl-cc::*loop-hash-iter-keywords* :hash-values)
          ("using-key"   "HASH-KEY"   cl-cc::*loop-using-keywords*     :hash-key)
          ("using-value" "HASH-VALUE" cl-cc::*loop-using-keywords*     :hash-value))
  (kw table expected)
  (assert-eq expected (cdr (assoc kw table :test #'string=))))

(deftest-each loop-emitter-dispatch-tables-are-hash-tables
  "The emitter dispatch tables are EQ hash tables ready for registration."
  :cases (("iter"      cl-cc::*loop-iter-emitters*)
          ("acc"       cl-cc::*loop-acc-emitters*)
          ("condition" cl-cc::*loop-condition-emitters*))
  (table)
  (assert-true (hash-table-p table))
  (assert-eq 'eq (hash-table-test table)))
