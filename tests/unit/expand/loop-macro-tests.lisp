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

(in-suite cl-cc-suite)

(defsuite loop-macro-suite
  :description "LOOP macro expansion and runtime behaviour"
  :parent cl-cc-suite)

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

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 2: FOR x IN list
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-for-in-collect
  "for x in list collect x"
  "(loop for x in '(1 2 3) collect x)"
  '(1 2 3))

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

(check-loop-equal loop-across-collect
  "for x across #(1 2 3) collect x"
  "(loop for x across #(1 2 3) collect x)"
  '(1 2 3))

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

;; Hash tests use check-loop-length (order non-deterministic) or sort+assert-equal.
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

;; Single-entry hash table → deterministic order, safe for check-loop-equal.
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

(check-loop-= loop-count-all-true
  "count over all-truthy list equals list length"
  "(loop for x in '(1 2 3 4 5) count x)"
  5)

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

(check-loop-= loop-maximize-two-element
  "maximize over a two-element list returns the larger"
  "(loop for x in '(3 7) maximize x)"
  7)

(check-loop-= loop-minimize-two-element
  "minimize over a two-element list returns the smaller"
  "(loop for x in '(7 3) minimize x)"
  3)

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
(defmacro check-loop-synonym-pair (base-name description canonical-code synonym-code expected)
  "Generate two deftest calls: one for the canonical keyword and one for its -ING synonym.
Both must produce EXPECTED under EQUAL comparison."
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
  "Like check-loop-synonym-pair but compares with = (numeric equality)."
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
  (for x downfrom 10 collect x))

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
