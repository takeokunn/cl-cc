;;;; loop-macro-runtime-ext-tests.lisp — LOOP macro advanced/edge case runtime tests
;;;
;;; Sections 28-31: unbounded FROM, maximize/minimize with negatives,
;;; accumulation keyword synonyms, additional coverage paths.
;;; Sections 32-36 (error paths, dotted destructuring, implicit body,
;;; hash-values USING, multiple accumulators) are in loop-macro-runtime-edge-tests.lisp.

(in-package :cl-cc/test)

(in-suite loop-macro-suite)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 28: FOR i FROM n without an upper bound (WHILE-terminated)
;;;; Exercises the :from emitter with an empty end-tests list.
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-from-while-terminated
  "for i from 1 with only WHILE bound (no TO/BELOW) collects the correct range"
  "(loop for i from 1 while (< i 5) collect i)"
  '(1 2 3 4))

(check-loop-equal loop-from-by-while-terminated
  "for i from 0 by 2 while (< i 10) collects even numbers up to the bound"
  "(loop for i from 0 by 2 while (< i 10) collect i)"
  '(0 2 4 6 8))

(check-loop-= loop-from-until-terminated
  "for i from 0 with only UNTIL bound sums until condition triggers"
  "(loop for i from 0 until (= i 5) sum i)"
  10)  ; 0+1+2+3+4 = 10

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 29: MAXIMIZE / MINIMIZE with negative values
;;;; The nil-init branch fires on the first element, then comparisons take over.
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-= loop-maximize-all-negative
  "maximize works correctly when every value is negative (nil-init then > compare)"
  "(loop for x in '(-5 -2 -8 -1 -9) maximize x)"
  -1)

(check-loop-= loop-minimize-all-negative
  "minimize works correctly when every value is negative"
  "(loop for x in '(-5 -2 -8 -1 -9) minimize x)"
  -9)

;; Removed loop-maximize-two-element / loop-minimize-two-element: subset of
;; loop-maximize-basic / loop-minimize-basic which already exercise the
;; compare-against-accum path on multi-element lists.

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 30: Accumulation keyword synonyms
;;;;
;;;; ANSI CL defines -ING variants for every accumulation keyword.
;;;; Each test pairs a synonym with its canonical form to confirm identical
;;;; semantics — one synonym per accumulation type, one canonical per type.
;;;; *loop-accum-keyword-table* drives both; coverage here exercises the
;;;; alternate string-key paths through the dispatch table.
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Helper — generate a pair of tests (canonical + synonym) in one call.
;;; Both arms runtime-check the produced value.
;;; Rationale: -ING synonyms are resolved by the loop normalizer, so gensym-free
;;; expansion equality is impossible. Two runtime checks is the minimal faithful
;;; test.
(defmacro check-loop-synonym-pair (base-name description canonical-code synonym-code expected)
  "Generate two deftest calls: canonical + -ING synonym, both runtime-checked."
  `(progn
     (check-loop-equal ,base-name
       ,(format nil "~A (canonical)" description)
       ,canonical-code ,expected)
     (check-loop-equal ,(intern (format nil "~A-SYNONYM" (symbol-name base-name)))
       ,(format nil "~A (-ING synonym)" description)
       ,synonym-code ,expected)))

(check-loop-synonym-pair loop-collecting-synonym
  "COLLECTING is a synonym for COLLECT"
  "(loop for x in '(1 2 3) collect x)"
  "(loop for x in '(1 2 3) collecting x)"
  '(1 2 3))

(check-loop-synonym-pair loop-appending-synonym
  "APPENDING is a synonym for APPEND"
  "(loop for x in '((1 2) (3 4)) append x)"
  "(loop for x in '((1 2) (3 4)) appending x)"
  '(1 2 3 4))

(check-loop-synonym-pair loop-nconcing-synonym
  "NCONCING is a synonym for NCONC"
  "(loop for x in (list (list 1 2) (list 3 4)) nconc x)"
  "(loop for x in (list (list 1 2) (list 3 4)) nconcing x)"
  '(1 2 3 4))

;;; Numeric synonyms — use check-loop-= for correct equality predicate.

(defmacro check-loop-=-synonym-pair (base-name description canonical-code synonym-code expected)
  `(progn
     (check-loop-= ,base-name
       ,(format nil "~A (canonical)" description)
       ,canonical-code ,expected)
     (check-loop-= ,(intern (format nil "~A-SYNONYM" (symbol-name base-name)))
       ,(format nil "~A (-ING synonym)" description)
       ,synonym-code ,expected)))

(check-loop-=-synonym-pair loop-summing-synonym
  "SUMMING is a synonym for SUM"
  "(loop for x in '(1 2 3 4) sum x)"
  "(loop for x in '(1 2 3 4) summing x)"
  10)

(check-loop-=-synonym-pair loop-counting-synonym
  "COUNTING is a synonym for COUNT"
  "(loop for x in '(1 nil 2 nil 3) count x)"
  "(loop for x in '(1 nil 2 nil 3) counting x)"
  3)

(check-loop-=-synonym-pair loop-maximizing-synonym
  "MAXIMIZING is a synonym for MAXIMIZE"
  "(loop for x in '(3 1 4 9 2) maximize x)"
  "(loop for x in '(3 1 4 9 2) maximizing x)"
  9)

(check-loop-=-synonym-pair loop-minimizing-synonym
  "MINIMIZING is a synonym for MINIMIZE"
  "(loop for x in '(3 1 4 9 2) minimize x)"
  "(loop for x in '(3 1 4 9 2) minimizing x)"
  1)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 31: Additional coverage — previously untested code paths
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; 31a — FOR i FROM n BELOW m (exclusive upper bound, no BY)
(check-loop-equal loop-from-below-no-by
  "for i from 0 below 3 collect i uses BELOW without BY"
  "(loop for i from 0 below 3 collect i)"
  '(0 1 2))

;;; 31b — FOR x = expr (no THEN) always re-evaluates init each step
;;; This exercises the %equals emitter pre-body branch (setq var init-form per step).
(check-loop-= loop-for-equals-no-then-sum
  "for x = 1 without THEN always re-evaluates; repeat 4 sums 1+1+1+1=4"
  "(loop for x = 1 repeat 4 sum x)"
  4)

;;; 31c — Multiple WITH clauses: each is independent (no cross-dependency)
(check-loop-= loop-with-nil-default
  "with var without = binds nil; (if nil 99 i) picks i each time"
  "(loop with x for i from 1 to 3 sum (if x 99 i))"
  6)

;;; 31d — FOR x ON by #'cddr with destructuring
(check-loop-equal loop-for-on-by-destructuring
  "for (a b) on list by #'cddr collects pairs"
  "(loop for (a b) on '(1 2 3 4) by #'cddr collect (list a b))"
  '((1 2) (3 4)))

;;; 31e — COLLECT INTO + FINALLY RETURN exercises named accumulator
;;; The collect emitter omits result-form when into-var is set;
;;; the user's FINALLY clause is responsible for returning the var.
(check-loop-equal loop-collect-into-finally-return
  "collect into named var, return from finally with nreverse"
  "(loop for x in '(10 20 30)
         collect x into bag
         finally (return (nreverse bag)))"
  '(10 20 30))

;;; 31f — ALWAYS over empty list (vacuous truth, drives *loop-vacuous-truth-conditions*)
;;; Exercises %loop-build-return-forms with has-vacuous-truth = T and no accumulator.
(check-loop-true loop-always-vacuous-truth-no-accum
  "always with no accumulator still returns T for empty list (vacuous truth)"
  "(loop for x in '() always t)")

;;; Sections 32-36 (error paths, dotted destructuring, implicit body,
;;; hash-values USING, multiple accumulators) + deftest-each consolidations
;;; + floor/truncate/ceiling tests are in loop-macro-runtime-edge-tests.lisp.

