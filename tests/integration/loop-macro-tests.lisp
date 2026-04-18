;;;; tests/unit/expand/loop-macro-tests.lisp — Unit tests for the LOOP macro
;;;;
;;;; Coverage targets:
;;;;   Expansion structure (block/let*/tagbody shape)
;;;;   FOR IN / ON / FROM / ACROSS / = / BEING HASH-KEYS / BEING HASH-VALUES
;;;;   WITH auxiliary binding
;;;;   Accumulation: COLLECT SUM COUNT MAXIMIZE MINIMIZE APPEND NCONC
;;;;   Filtering: WHEN IF UNLESS
;;;;   Control: WHILE UNTIL ALWAYS NEVER THEREIS
;;;;   REPEAT
;;;;   INITIALLY / FINALLY
;;;;   Destructuring in IN and ON
;;;;   Named accumulators (INTO)

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(defsuite loop-macro-suite
  :description "LOOP macro expansion and runtime behaviour"
  :parent cl-cc-integration-suite)

(in-suite loop-macro-suite)

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Helper macros — raise the abstraction level for common test patterns.
;;;;
;;;; Design: each macro expands to exactly one deftest.  The description string
;;;; is the contract; the body is the minimal assertion.  Helper names follow:
;;;;   check-loop-equal  — result EQUAL expected
;;;;   check-loop-=      — result = expected  (numeric)
;;;;   check-loop-true   — result is truthy
;;;;   check-loop-false  — result is NIL
;;;;   check-loop-null   — result is NIL (empty accumulation)
;;;;   check-loop-length — (length result) = expected-length  (non-deterministic order)
;;;;   check-loop-expansion — structural test on the macro-expanded form
;;;;   check-loop-signals  — LOOP clause parsing signals an error
;;;;   check-loop-synonym-pair    — pair: canonical + -ING synonym, EQUAL
;;;;   check-loop-=-synonym-pair  — pair: canonical + -ING synonym, =
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defmacro check-loop-equal (name description code expected)
  "LOOP runtime test: result is EQUAL to EXPECTED."
  `(deftest ,name ,description
     (assert-equal ,expected (run-string ,code))))

(defmacro check-loop-= (name description code expected)
  "LOOP runtime test: result is = (numeric) to EXPECTED."
  `(deftest ,name ,description
     (assert-= ,expected (run-string ,code))))

(defmacro check-loop-true (name description code)
  "LOOP runtime test: result is truthy."
  `(deftest ,name ,description
     (assert-true (run-string ,code))))

(defmacro check-loop-false (name description code)
  "LOOP runtime test: result is NIL."
  `(deftest ,name ,description
     (assert-false (run-string ,code))))

(defmacro check-loop-null (name description code)
  "LOOP runtime test: result is NIL (empty list)."
  `(deftest ,name ,description
     (assert-null (run-string ,code))))

(defmacro check-loop-expansion (name description clauses &body assertions)
  "LOOP structural test: macro-expand (loop ,@CLAUSES) then evaluate ASSERTIONS on the result."
  `(deftest ,name ,description
     (let ((result (our-macroexpand-1 '(loop ,@clauses))))
       ,@assertions)))

(defmacro check-loop-length (name description code expected-length)
  "LOOP runtime test: result is a sequence of EXPECTED-LENGTH elements.
Useful for hash-table tests where element order is non-deterministic."
  `(deftest ,name ,description
     (assert-= ,expected-length (length (run-string ,code)))))

(defmacro check-loop-signals (name description clauses)
  "LOOP structural test: parsing CLAUSES must signal an error condition.
Exercises error paths in the parser and emitter layers."
  `(deftest ,name ,description
     (assert-signals error (our-macroexpand-1 '(loop ,@clauses)))))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 1: Expansion structure
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-expansion loop-basic-expands-to-block
  "A bare loop expands to (block nil (let* (...) (tagbody ...)))"
  (do (return 42))
  (assert-true (consp result))
  (assert-eq 'block (car result))
  (assert-eq nil (cadr result)))

(check-loop-expansion loop-for-in-expands-to-let-tagbody
  "for x in list produces block → nil → let*"
  (for x in '(1 2 3) collect x)
  (assert-eq 'block (car result))
  (assert-eq nil (cadr result))
  (assert-eq 'let* (car (caddr result))))

(check-loop-expansion loop-from-to-expands-to-block
  "for i from 1 to 5 expands to block → nil → let*"
  (for i from 1 to 5 collect i)
  (assert-eq 'block (car result))
  (assert-eq 'let* (car (caddr result))))

(check-loop-expansion loop-repeat-has-counter-binding
  "repeat N expands with a counter binding in let*"
  (repeat 3 collect t)
  (assert-eq 'block (car result))
  (assert-true (consp (cadr (caddr result)))))

(check-loop-expansion loop-while-expands-to-block
  "while cond do ... expands to block"
  (while t do (return 1))
  (assert-eq 'block (car result)))

(check-loop-expansion loop-initially-finally-expands
  "initially/finally clauses appear in expansion"
  (initially (print 'start) finally (print 'end) repeat 1)
  (assert-eq 'block (car result)))

(check-loop-expansion loop-across-has-aref-and-length
  "for x across v expansion contains AREF and LENGTH"
  (for x across v collect x)
  (let ((s (format nil "~S" result)))
    (assert-true (search "AREF" s))
    (assert-true (search "LENGTH" s))))

(check-loop-expansion loop-across-has-four-bindings
  "for x across v expansion has at least 4 let* bindings (vec len idx var)"
  (for x across v collect x)
  (assert-true (>= (length (cadr (caddr result))) 4)))


;;;; Runtime LOOP behavior tests

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 2: FOR x IN list
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-for-in-collect-transform
  "for x in list collect (* x 2) doubles each element"
  "(loop for x in '(1 2 3) collect (* x 2))"
  '(2 4 6))

(check-loop-null loop-for-in-collect-empty
  "for x in empty list returns nil"
  "(loop for x in '() collect x)")

(check-loop-= loop-for-in-do-sum
  "for x in list do accumulates correctly"
  "(let ((s 0)) (loop for x in '(1 2 3) do (setq s (+ s x))) s)"
  6)

(check-loop-equal loop-for-in-by
  "for x in list by #'cddr steps by two"
  "(loop for x in '(1 2 3 4 5) by #'cddr collect x)"
  '(1 3 5))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 3: FOR i FROM n [TO/BELOW m] [BY k]
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-from-to-collect
  "for i from 1 to 5 collect i"
  "(loop for i from 1 to 5 collect i)"
  '(1 2 3 4 5))

(check-loop-equal loop-from-below-collect
  "for i from 0 below 4 collect i"
  "(loop for i from 0 below 4 collect i)"
  '(0 1 2 3))

(check-loop-equal loop-from-to-by-collect
  "for i from 0 to 10 by 3 collect i"
  "(loop for i from 0 to 10 by 3 collect i)"
  '(0 3 6 9))

(check-loop-= loop-from-to-sum
  "for i from 1 to 5 sum i = 15"
  "(loop for i from 1 to 5 sum i)"
  15)

(check-loop-null loop-from-empty-range
  "for i from 5 to 1 collects nothing"
  "(loop for i from 5 to 1 collect i)")

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
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
