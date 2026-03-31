;;;; tests/unit/expand/loop-data-tests.lisp — LOOP data layer unit tests

(in-package :cl-cc/test)

(defsuite loop-data-suite
  :description "LOOP data layer unit tests"
  :parent cl-cc-suite)

(in-suite loop-data-suite)

(deftest loop-boundary-keywords-contain-core-markers
  "The LOOP boundary keyword set contains the canonical control markers."
  (assert-true (member "FOR" cl-cc::*loop-boundary-keywords* :test #'string=))
  (assert-true (member "COLLECT" cl-cc::*loop-boundary-keywords* :test #'string=))
  (assert-true (member "FINALLY" cl-cc::*loop-boundary-keywords* :test #'string=))
  (assert-true (member "NAMED" cl-cc::*loop-boundary-keywords* :test #'string=)))

(deftest loop-vacuous-truth-conditions-are-the-expected-two-symbols
  "The vacuous-truth condition set is exactly the ANSI LOOP pair."
  (assert-equal '(:always :never) cl-cc::*loop-vacuous-truth-conditions*))

(deftest loop-accum-keyword-table-maps-canonical-and-ing-forms
  "Canonical and -ING accumulation keywords map to the same internal type."
  (assert-eq :collect (cdr (assoc "COLLECT" cl-cc::*loop-accum-keyword-table* :test #'string=)))
  (assert-eq :collect (cdr (assoc "COLLECTING" cl-cc::*loop-accum-keyword-table* :test #'string=)))
  (assert-eq :sum (cdr (assoc "SUMMING" cl-cc::*loop-accum-keyword-table* :test #'string=)))
  (assert-eq :nconc (cdr (assoc "NCONCING" cl-cc::*loop-accum-keyword-table* :test #'string=))))

(deftest loop-hash-keyword-tables-map-both-spellings
  "HASH-KEYS/HASH-VALUES and USING aliases are registered in the data tables."
  (assert-eq :hash-keys (cdr (assoc "HASH-KEYS" cl-cc::*loop-hash-iter-keywords* :test #'string=)))
  (assert-eq :hash-values (cdr (assoc "HASH-VALUE" cl-cc::*loop-hash-iter-keywords* :test #'string=)))
  (assert-eq :hash-key (cdr (assoc "HASH-KEY" cl-cc::*loop-using-keywords* :test #'string=)))
  (assert-eq :hash-value (cdr (assoc "HASH-VALUE" cl-cc::*loop-using-keywords* :test #'string=))))

(deftest loop-emitter-dispatch-tables-are-hash-tables
  "The emitter dispatch tables are hash tables ready for registration."
  (assert-true (hash-table-p cl-cc::*loop-iter-emitters*))
  (assert-true (hash-table-p cl-cc::*loop-acc-emitters*))
  (assert-true (hash-table-p cl-cc::*loop-condition-emitters*))
  (assert-eq 'eq (hash-table-test cl-cc::*loop-iter-emitters*))
  (assert-eq 'eq (hash-table-test cl-cc::*loop-acc-emitters*))
  (assert-eq 'eq (hash-table-test cl-cc::*loop-condition-emitters*)))
