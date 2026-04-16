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


;;;; Runtime LOOP behavior tests moved to loop-macro-runtime-tests.lisp.
