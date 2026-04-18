(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(defsuite loop-macro-suite
  :description "LOOP macro expansion and runtime behaviour"
  :parent cl-cc-integration-suite)

(in-suite loop-macro-suite)

;;;; Earlier runtime sections moved to loop-macro-tests.lisp to reduce this file.

(check-loop-equal loop-if-collect
  "if (synonym for when) filters correctly"
  "(loop for x in '(1 2 3 4 5 6) if (evenp x) collect (* x x))"
  '(4 16 36))

(check-loop-= loop-when-do
  "when filter with do runs only for matching elements (3+4+5=12)"
  "(let ((s 0))
     (loop for x in '(1 2 3 4 5)
           when (> x 2) do (setq s (+ s x)))
     s)"
  12)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 12: WHILE / UNTIL termination
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-while-terminates
  "while condition stops when condition becomes false"
  "(let ((i 0)) (loop while (< i 5) collect (prog1 i (setq i (+ i 1)))))"
  '(0 1 2 3 4))

(check-loop-equal loop-until-terminates
  "until condition stops when condition becomes true"
  "(let ((i 0)) (loop until (>= i 3) collect (prog1 i (setq i (+ i 1)))))"
  '(0 1 2))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 13: ALWAYS / NEVER / THEREIS
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-true loop-always-true
  "always returns t when condition always holds"
  "(loop for x in '(2 4 6) always (evenp x))")

(check-loop-false loop-always-false
  "always returns nil when condition fails"
  "(loop for x in '(2 3 6) always (evenp x))")

(check-loop-true loop-never-true
  "never returns t when condition never holds"
  "(loop for x in '(1 3 5) never (evenp x))")

(check-loop-false loop-never-false
  "never returns nil when condition holds for some element"
  "(loop for x in '(1 2 5) never (evenp x))")

(check-loop-= loop-thereis-finds-value
  "thereis returns the first truthy value"
  "(loop for x in '(1 3 4 6) thereis (and (evenp x) x))"
  4)

(check-loop-false loop-thereis-not-found
  "thereis returns nil when condition never holds"
  "(loop for x in '(1 3 5) thereis (evenp x))")

;; Vacuous truth: ALWAYS and NEVER on the empty list return T (no element can violate).
(check-loop-true loop-always-empty-list
  "always over empty list returns t (vacuous truth)"
  "(loop for x in '() always nil)")

(check-loop-true loop-never-empty-list
  "never over empty list returns t (vacuous truth)"
  "(loop for x in '() never t)")

(check-loop-false loop-thereis-empty-list
  "thereis over empty list returns nil"
  "(loop for x in '() thereis x)")

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 14: REPEAT n
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-repeat-collect
  "repeat N collect t produces N-element list"
  "(loop repeat 5 collect t)"
  '(t t t t t))

(check-loop-= loop-repeat-sum
  "repeat N sum 1 produces N"
  "(loop repeat 4 sum 1)"
  4)

(check-loop-null loop-repeat-zero
  "repeat 0 produces empty list"
  "(loop repeat 0 collect t)")

(check-loop-null loop-repeat-negative
  "repeat N with N<0 produces empty list (already past bound)"
  "(loop repeat -3 collect t)")

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 15: Named accumulators (INTO)
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-collect-into-named
  "collect into named var; nreverse in finally for canonical order"
  "(loop for x in '(1 2 3) collect x into result finally (return (nreverse result)))"
  '(1 2 3))

(check-loop-equal loop-two-named-accumulators
  "two collect-into accumulators partition elements"
  "(loop for x in '(1 2 3 4)
         if (oddp x)  collect x into odds
         if (evenp x) collect x into evens
         finally (return (list (nreverse odds) (nreverse evens))))"
  '((1 3) (2 4)))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 16: INITIALLY / FINALLY
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(deftest loop-initially-runs-first
  "initially clause runs before iteration begins"
  (let ((result (run-string
                  "(let ((log nil))
                     (loop initially (push 'init log)
                           for x in '(1 2 3)
                           do (push x log))
                     (nreverse log))")))
    (assert-true (consp result))
    (assert-= 4 (length result))
    (assert-true (string= "INIT" (symbol-name (car result))))
    (assert-equal '(1 2 3) (cdr result))))

(check-loop-= loop-finally-runs-last
  "finally clause runs after iteration ends (sum 1+2+3+4=10, then *10=100)"
  "(let ((total 0))
     (loop for x in '(1 2 3 4)
           do (setq total (+ total x))
           finally (setq total (* total 10)))
     total)"
  100)

(check-loop-= loop-finally-return
  "finally clause can return a value"
  "(loop for x in '(1 2 3 4) sum x into s finally (return (* s 10)))"
  100)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 17: RETURN / early exit
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-= loop-return-from-do
  "do (return value) exits with that value"
  "(loop do (return 42))"
  42)

(check-loop-= loop-return-early
  "returns early when condition is met"
  "(loop for x in '(1 2 3 4 5) when (= x 3) do (return x))"
  3)

(check-loop-= loop-return-from-while
  "while loop returns via (return) inside do"
  "(let ((i 0)) (loop while (< i 100) do (incf i) when (= i 7) do (return i)))"
  7)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 18: FOR i FROM n UPTO m (synonym for TO)
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-from-upto-collect
  "for i from 1 upto 4 collect i (UPTO is synonym for TO)"
  "(loop for i from 1 upto 4 collect i)"
  '(1 2 3 4))

(check-loop-equal loop-from-upto-by-collect
  "for i from 0 upto 10 by 4 collect i"
  "(loop for i from 0 upto 10 by 4 collect i)"
  '(0 4 8))

(check-loop-null loop-from-upto-empty-range
  "for i from 5 upto 1 collects nothing (already past bound)"
  "(loop for i from 5 upto 1 collect i)")

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 19: HASH-VALUES with USING (hash-key)
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Single-entry hash table → deterministic order, safe for check-loop-equal.
(check-loop-equal loop-hash-values-using-key
  "for v being hash-values using (hash-key k) can access both value and key"
  "(let ((ht (make-hash-table)))
     (setf (gethash 'x ht) 99)
     (loop for v being the hash-values of ht
           using (hash-key k)
           collect v))"
  '(99))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 20: Multiple FOR clauses (parallel iteration)
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-two-for-in
  "two for-in clauses iterate in parallel; stops at shorter list"
  "(loop for x in '(1 2 3) for y in '(10 20 30) collect (+ x y))"
  '(11 22 33))

(check-loop-equal loop-for-in-and-from
  "for-in and for-from together: zip list with index"
  "(loop for x in '(a b c) for i from 0 collect i)"
  '(0 1 2))

(check-loop-equal loop-three-for-clauses
  "three for clauses all step together"
  "(loop for x in '(1 2 3) for y in '(4 5 6) for z from 10 collect (list x y z))"
  '((1 4 10) (2 5 11) (3 6 12)))

(check-loop-equal loop-for-in-stops-at-shorter
  "parallel for-in clauses stop when the shorter list is exhausted"
  "(loop for x in '(1 2 3 4 5) for y in '(10 20) collect (+ x y))"
  '(11 22))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 21: FOR var = expr THEN step — combined with termination
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-for-equals-then-while
  "for x = 1 then (* x 2) while (< x 100) collects powers of 2"
  "(loop for x = 1 then (* x 2) while (< x 100) collect x)"
  '(1 2 4 8 16 32 64))

(check-loop-= loop-for-equals-then-sum
  "for x = 0 then (+ x 1) repeat 5 sum x = 0+1+2+3+4=10"
  "(loop for x = 0 then (+ x 1) repeat 5 sum x)"
  10)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 22: WITH + accumulation interaction
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-= loop-with-nil-init
  "with var without = binds var to nil"
  "(loop with x for i from 1 to 3 sum (if x 1 i))"
  6)

(check-loop-equal loop-with-multiple
  "two with clauses bind independent vars"
  "(loop with a = 10 with b = 20 for i from 1 to 3 collect (+ a b i))"
  '(31 32 33))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 23: ACROSS — index-based access
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-across-with-index
  "for x across vec with parallel for i from 0 yields (index . value) pairs"
  "(loop for x across #(10 20 30) for i from 0 collect (list i x))"
  '((0 10) (1 20) (2 30)))

(check-loop-= loop-across-count
  "for x across vector count non-nil elements"
  "(loop for x across #(1 nil 2 nil 3) count x)"
  3)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 24: COLLECT with WHEN — combined filter+accumulation
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-when-collect-squares
  "when filter with collect squares only even elements"
  "(loop for x from 1 to 6 when (evenp x) collect (* x x))"
  '(4 16 36))

(check-loop-= loop-unless-sum
  "unless filter with sum sums only odd elements"
  "(loop for x from 1 to 5 unless (evenp x) sum x)"
  9)

(check-loop-= loop-when-count
  "when filter with count counts matching elements"
  "(loop for x in '(1 2 3 4 5 6) when (> x 3) count x)"
  3)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 25: Expansion structure — :repeat and :with via emitter table
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-expansion loop-repeat-expansion-has-binding
  "repeat N expansion has a counter binding initialised to N in let*"
  (repeat 4 collect t)
  (assert-eq 'block (car result))
  (assert-eq 'let* (car (caddr result)))
  (assert-true (some (lambda (b) (equal (cadr b) 4))
                     (cadr (caddr result)))))  ; counter binding with value 4

(check-loop-expansion loop-with-expansion-has-binding
  "with var = expr expansion includes the var binding in let*"
  (with x = 42 collect x)
  (assert-eq 'block (car result))
  (let ((bindings (cadr (caddr result))))
    (assert-true (some (lambda (b) (equal (cadr b) 42)) bindings))))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 26: DO body with WHEN/UNLESS filter — multi-form wrapping
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-= loop-when-do-multi-form
  "when filter wraps all DO forms together"
  "(let ((a 0) (b 0))
     (loop for x in '(1 2 3 4)
           when (evenp x) do (setq a (+ a x)) (setq b (+ b 1)))
     (+ a b))"
  ;; evens: 2,4 → a=6, b=2 → 8
  8)

(check-loop-= loop-unless-do-multi-form
  "unless filter wraps all DO forms together"
  "(let ((s 0) (c 0))
     (loop for x in '(1 2 3 4 5)
           unless (evenp x) do (setq s (+ s x)) (setq c (+ c 1)))
     (* s c))"
  ;; odds: 1,3,5 → s=9, c=3 → 27
  27)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 27: WHEN/UNLESS filter combined with non-COLLECT accumulators
;;;; Exercises %loop-wrap-filter against :sum :maximize :minimize :append
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-= loop-when-sum-filtered
  "when filter + sum adds only matching elements (1+3+5=9)"
  "(loop for x in '(1 2 3 4 5) when (oddp x) sum x)"
  9)

(check-loop-= loop-unless-sum-evens
  "unless filter + sum skips matching elements and sums rest (1+3+5=9)"
  "(loop for x in '(1 2 3 4 5) unless (evenp x) sum x)"
  9)

(check-loop-= loop-when-maximize-filtered
  "when filter + maximize finds the max only among even elements"
  "(loop for x in '(1 2 3 8 5 6) when (evenp x) maximize x)"
  8)

(check-loop-= loop-unless-minimize-filtered
  "unless filter + minimize finds the min among odd elements (1 3 5 → 1)"
  "(loop for x in '(1 4 3 2 5) unless (evenp x) minimize x)"
  1)

(check-loop-equal loop-when-append-filtered
  "when filter + append concatenates only non-nil sublists"
  "(loop for x in '((1 2) nil (3 4) nil) when x append x)"
  '(1 2 3 4))

(check-loop-equal loop-unless-append-nils-skipped
  "unless filter + append skips nil sublists"
  "(loop for x in '(nil (10 20) nil (30)) unless (null x) append x)"
  '(10 20 30))

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

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 32: Error paths — unknown / malformed clause keywords
;;;;
;;;; These tests exercise the guard clauses in the parser and emitter layers.
;;;; Each check-loop-signals test verifies that an error is signalled rather
;;;; than silently producing wrong output.
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-signals loop-unknown-for-keyword-signals-error
  "FOR with an unrecognised sub-keyword signals an error"
  (for x sideways 10 collect x))

(check-loop-signals loop-being-unknown-hash-type-signals-error
  "BEING THE with an unrecognised hash-iteration keyword signals an error"
  (for k being the buckets of ht collect k))

(check-loop-signals loop-being-missing-of-signals-error
  "BEING THE HASH-KEYS without OF/IN signals an error"
  (for k being the hash-keys ht collect k))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 33: Dotted-pair destructuring in FOR x IN / ON
;;;;
;;;; %loop-destructure-var supports (a . b) dotted patterns.
;;;; The CDR variable binds to the tail of the current element.
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-dotted-destructure-in
  "for (a . b) in alist collects CDR as the tail (dotted-pair pattern; numeric to avoid package mismatch)"
  "(loop for (a . b) in '((1 . 10) (2 . 20) (3 . 30)) collect b)"
  '(10 20 30))

(check-loop-equal loop-dotted-destructure-in-car
  "for (a . b) in alist collects CAR (head)"
  "(loop for (a . b) in '((10 . 100) (20 . 200)) collect a)"
  '(10 20))

(check-loop-equal loop-dotted-destructure-on
  "for (head . tail) on list collects the CDR tail at each step"
  "(loop for (head . tail) on '(1 2 3) collect (length tail))"
  '(2 1 0))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 34: Implicit body forms (no top-level keyword)
;;;;
;;;; When a token is not a recognised top-level keyword, parse-loop-clauses
;;;; pushes it directly onto body-forms.  This exercises the else-branch of
;;;; the main dispatch loop.
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-= loop-implicit-body-form
  "A bare expression with no keyword runs as an implicit body form"
  "(let ((n 0))
     (loop (setq n (+ n 1)) (when (= n 5) (return n))))"
  5)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 35: HASH-VALUES USING (hash-key k) — paired key access
;;;;
;;;; This exercises the USING branch inside the :hash-values emitter, which
;;;; allocates a separate keys-var alongside vals-var.
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-hash-values-using-key-pair
  "for v being hash-values using (hash-key k) returns associated value (single entry; key unused in collect)"
  "(let ((ht (make-hash-table)))
     (setf (gethash 99 ht) 42)
     (loop for v being the hash-values of ht
           using (hash-key k)
           collect v))"
  '(42))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 36: %loop-build-return-forms — multiple implicit accumulators
;;;;
;;;; When two separate named accumulators (INTO) are used, the implicit
;;;; return value is NIL (the user takes responsibility via FINALLY).
;;;; When two implicit accumulators are used (no INTO), result is (values ...).
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Two INTO-named accumulators → user controls return via FINALLY.
(check-loop-equal loop-two-into-accumulators-via-finally
  "two named INTO accumulators; FINALLY returns them as a list"
  "(loop for x in '(1 2 3 4 5)
         if (oddp x)  collect x into odds
         if (evenp x) collect x into evens
         finally (return (list (nreverse odds) (nreverse evens))))"
  '((1 3 5) (2 4)))

;;; Loop Macro Tests

;; Consolidated: the following deftest-each previously duplicated many of the
;; individual check-loop-equal tests above (for-in, for-from-to, for-below,
;; collect-expr, for-on, when-collect, unless-collect, if-collect, collect-into,
;; append, hash-key-val). Removed those cases; kept only cases that exercise
;; paths not covered by individual tests (for-in-do via push, unless-do via push,
;; when-append, collect-into-when combining INTO + filter, from-by stepping).
(deftest-each loop-collect-list
  "LOOP collect/append/do/when/unless accumulations — paths not covered individually."
  :cases (("for-in-do"      '(1 2 3)           "(let ((r nil)) (loop for x in (list 1 2 3) do (push x r)) (nreverse r))")
          ("empty"          nil                "(loop for x in nil collect x)")
          ("from-by"        '(0 2 4)           "(loop for i from 0 below 5 by 2 collect i)")
          ("on-by-cddr"     '((1 2 3 4) (3 4)) "(loop for x on (list 1 2 3 4) by (function cddr) collect x)")
          ("unless-do"      '(1 3 5)           "(let ((r nil)) (loop for x in (list 1 2 3 4 5) unless (evenp x) do (push x r)) (nreverse r))")
          ("when-append"    '(2 2 4 4)         "(loop for x in (list 1 2 3 4) when (evenp x) append (list x x))")
          ("collect-into-when" '((1 3 5 7 9) (2 4 6 8 10)) "(loop for i from 1 to 10 when (oddp i) collect i into odds when (evenp i) collect i into evens finally (return (list (nreverse odds) (nreverse evens))))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;; Consolidated: removed cases covered individually above:
;;   "repeat"/"sum"/"hash-keys" duplicate check-loop-= loop-repeat-sum /
;;   loop-from-to-sum / check-loop-length loop-hash-keys-collect.
(deftest-each loop-numeric
  "LOOP numeric accumulations — paths not covered individually."
  :cases (("repeat-zero"  0  "(let ((n 0)) (loop repeat 0 do (setq n (+ n 1))) n)")
          ("hash-values" 30  "(let ((ht (make-hash-table))) (setf (gethash 'x ht) 10) (setf (gethash 'y ht) 20) (loop for v being the hash-values of ht sum v))")
          ("with-clause" 15  "(loop with sum = 0 for i from 1 to 5 do (setq sum (+ sum i)) finally (return sum))")
          ("sum-into"    15  "(loop for i from 1 to 5 sum i into total finally (return total))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;; Removed deftest-each loop-always-never-thereis: every case is a direct
;; duplicate of individual check-loop-* tests above
;; (loop-always-true / loop-always-false / loop-never-true / loop-thereis-finds-value).

(deftest loop-edge-cases
  "LOOP: empty hash-keys returns nil; nconc concatenates sublists."
  (assert-null (run-string "(loop for k being the hash-keys of (make-hash-table) collect k)" :stdlib t))
  (assert-true (string= "(a b c d)"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(loop for x in (list (list 'a 'b) (list 'c 'd)) nconc (copy-list x))" :stdlib t)))))))

;;; Floor/Truncate/Ceiling Multiple Values Tests

(deftest-each floor-truncate-ceiling
  "floor/truncate/ceiling return quotient and remainder via multiple-value-bind."
  :cases (("floor"    '(3  2) "(multiple-value-bind (q r) (floor    17 5) (list q r))")
          ("truncate" '(3  2) "(multiple-value-bind (q r) (truncate 17 5) (list q r))")
          ("ceiling"  '(4 -3) "(multiple-value-bind (q r) (ceiling  17 5) (list q r))"))
  (expected form)
  (assert-equal expected (run-string form :stdlib t)))

