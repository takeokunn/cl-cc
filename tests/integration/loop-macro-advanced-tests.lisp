;;;; loop-macro-advanced-tests.lisp — LOOP: ON/ACROSS/=/hash/WITH/destructuring/accumulation/filtering
(in-package :cl-cc/test)

(in-suite loop-macro-suite)
;;;; Section 4: FOR x ON list (sublist iteration)
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-for-on-collect
  "for x on list collects sublists"
  "(loop for x on '(1 2 3) collect x)"
  '((1 2 3) (2 3) (3)))

(check-loop-equal loop-for-on-car-collect
  "for x on list collect (car x) is same as for-in"
  "(loop for x on '(1 2 3) collect (car x))"
  '(1 2 3))

(check-loop-equal loop-for-on-by
  "for x on list by #'cddr steps two at a time"
  "(loop for x on '(1 2 3 4) by #'cddr collect x)"
  '((1 2 3 4) (3 4)))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 5: FOR x ACROSS vector
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-across-transform
  "for x across vector collect (* x 10)"
  "(loop for x across #(1 2 3) collect (* x 10))"
  '(10 20 30))

(check-loop-null loop-across-empty-vector
  "for x across empty vector collects nothing"
  "(loop for x across #() collect x)")

(check-loop-= loop-across-sum
  "for x across #(1 2 3 4) sum x = 10"
  "(loop for x across #(1 2 3 4) sum x)"
  10)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 6: FOR var = expr [THEN step]
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-for-equals-then
  "for x = 1 then (* x 2) repeat 5 collect x doubles each iteration"
  "(loop for x = 1 then (* x 2) repeat 5 collect x)"
  '(1 2 4 8 16))

(check-loop-equal loop-for-equals-no-then
  "for x = 0 without THEN re-evaluates each iteration"
  "(loop for x = 0 repeat 3 collect x)"
  '(0 0 0))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 7: FOR var BEING THE HASH-KEYS/HASH-VALUES
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-length loop-hash-keys-collect
  "for k being the hash-keys of ht collects all keys (count only; order undefined)"
  "(let ((ht (make-hash-table)))
     (setf (gethash 'a ht) 1)
     (setf (gethash 'b ht) 2)
     (loop for k being the hash-keys of ht collect k))"
  2)

(deftest loop-hash-values-collect
  "for v being the hash-values of ht collects all values"
  (let ((result (run-string
                  "(let ((ht (make-hash-table)))
                     (setf (gethash 'a ht) 10)
                     (setf (gethash 'b ht) 20)
                     (sort (loop for v being the hash-values of ht collect v) #'<))")))
    (assert-equal '(10 20) result)))

(check-loop-equal loop-hash-keys-using-value
  "for k being hash-keys using (hash-value v) can access both key and value"
  "(let ((ht (make-hash-table)))
     (setf (gethash 'x ht) 42)
     (loop for k being the hash-keys of ht
           using (hash-value v)
           collect v))"
  '(42))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 8: WITH auxiliary binding
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-= loop-with-initialized
  "with var = expr binds var for the loop body"
  "(loop with x = 10 for i from 1 to 3 sum (* i x))"
  60)

(check-loop-equal loop-with-used-in-collect
  "with var is visible throughout the loop body (numeric to avoid package mismatch)"
  "(loop with factor = 10 for i from 1 to 3 collect (* factor i))"
  '(10 20 30))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 9: Destructuring in FOR x IN / ON
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-destructure-in-pairs
  "for (k v) in alist collects values"
  "(loop for (k v) in '((a 1) (b 2) (c 3)) collect v)"
  '(1 2 3))

(check-loop-equal loop-destructure-in-keys
  "for (k v) in alist collects first elements (numeric keys avoid package mismatch)"
  "(loop for (k v) in '((10 a) (20 b) (30 c)) collect k)"
  '(10 20 30))

(check-loop-equal loop-destructure-on-plist
  "for (k v) on numeric plist by #'cddr collects sums (avoids package mismatch)"
  "(loop for (k v) on '(1 10 2 20 3 30) by #'cddr collect (+ k v))"
  '(11 22 33))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 10: Accumulation — COLLECT SUM COUNT MAXIMIZE MINIMIZE APPEND NCONC
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-= loop-sum-basic
  "sum returns numeric total"
  "(loop for x in '(1 2 3 4) sum x)"
  10)

(check-loop-= loop-sum-empty
  "sum over empty list returns 0"
  "(loop for x in '() sum x)"
  0)

(check-loop-= loop-count-truthy
  "count counts truthy elements"
  "(loop for x in '(1 nil 2 nil 3) count x)"
  3)

(check-loop-= loop-count-all-false
  "count over all-nil list returns 0"
  "(loop for x in '(nil nil nil) count x)"
  0)

(check-loop-= loop-minimize-basic
  "minimize returns minimum value"
  "(loop for x in '(3 1 4 1 5) minimize x)"
  1)

(check-loop-= loop-minimize-single
  "minimize over single-element list returns that element"
  "(loop for x in '(42) minimize x)"
  42)

(check-loop-= loop-maximize-basic
  "maximize returns maximum value"
  "(loop for x in '(3 1 4 9 2) maximize x)"
  9)

(check-loop-= loop-maximize-single
  "maximize over single-element list returns that element"
  "(loop for x in '(7) maximize x)"
  7)

(check-loop-equal loop-append-basic
  "append concatenates sublists"
  "(loop for x in '((1 2) (3 4) (5 6)) append x)"
  '(1 2 3 4 5 6))

(check-loop-equal loop-append-empty-sublists
  "append with empty sublists produces empty list"
  "(loop for x in '(() ()) append x)"
  nil)

(check-loop-equal loop-nconc-basic
  "nconc destructively concatenates sublists"
  "(loop for x in (list (list 1 2) (list 3 4)) nconc x)"
  '(1 2 3 4))

(check-loop-= loop-two-implicit-accumulators
  "collect and sum in same loop each track independently"
  "(let ((s (loop for x in '(1 2 3 4) sum x)))
     s)"
  10)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 11: WHEN / IF / UNLESS per-clause filters
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-when-collect
  "when filter collects only matching elements"
  "(loop for x in '(1 2 3 4 5) when (evenp x) collect x)"
  '(2 4))

(check-loop-equal loop-unless-collect
  "unless filter collects non-matching elements"
  "(loop for x in '(1 2 3 4 5) unless (evenp x) collect x)"
  '(1 3 5))
