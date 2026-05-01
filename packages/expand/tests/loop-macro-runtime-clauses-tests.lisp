(in-package :cl-cc/test)

(in-suite loop-macro-suite)

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

;;; Sections 28-36 (advanced/edge cases, error paths, dotted destructuring,
;;; implicit body, hash-values USING, multiple accumulators) are in
;;; loop-macro-runtime-ext-tests.lisp.
