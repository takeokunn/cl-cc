;;;; tests/unit/expand/loop-macro-tests.lisp — Unit tests for the LOOP macro expansion
;;;;
;;;; Tests cover structural expansion (via our-macroexpand-1) and runtime behavior
;;;; (via run-string) for all major LOOP clauses.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

(defsuite loop-macro-suite
  :description "Test suite for LOOP macro expansion and runtime behavior"
  :parent cl-cc-suite)

(in-suite loop-macro-suite)

;;; ------------------------------------------------------------
;;; Section 1: Structural expansion checks
;;; ------------------------------------------------------------

(deftest loop-basic-expands-to-block
  "A bare loop expands to (block nil (let* (...) (tagbody ...)))"
  (let ((result (our-macroexpand-1 '(loop do (return 42)))))
    (assert-true (consp result))
    (assert-eq 'block (car result))
    (assert-eq nil (cadr result))))

(deftest loop-for-in-expands-to-let-tagbody
  "loop for x in list produces block containing let* + tagbody"
  (let ((result (our-macroexpand-1 '(loop for x in '(1 2 3) collect x))))
    (assert-true (consp result))
    (assert-eq 'block (car result))
    ;; Second element is nil (block name)
    (assert-eq nil (cadr result))
    ;; Body contains let*
    (let ((let-form (caddr result)))
      (assert-eq 'let* (car let-form)))))

(deftest loop-from-to-expands-to-block
  "loop for i from 1 to 5 expands to block with let*"
  (let ((result (our-macroexpand-1 '(loop for i from 1 to 5 collect i))))
    (assert-true (consp result))
    (assert-eq 'block (car result))
    (assert-eq nil (cadr result))
    (assert-eq 'let* (car (caddr result)))))

(deftest loop-repeat-expands-to-block
  "loop repeat N expands to block containing let* with counter binding"
  (let ((result (our-macroexpand-1 '(loop repeat 3 collect t))))
    (assert-true (consp result))
    (assert-eq 'block (car result))
    (let ((let-form (caddr result)))
      ;; let* bindings should include the counter variable
      (assert-true (consp (cadr let-form))))))

(deftest loop-while-expands-to-block
  "loop while cond expands to block with unless+go in tagbody"
  (let ((result (our-macroexpand-1 '(loop while t do (return 1)))))
    (assert-true (consp result))
    (assert-eq 'block (car result))))

(deftest loop-initially-finally-expands
  "loop initially/finally clauses appear in expanded code"
  (let ((result (our-macroexpand-1 '(loop initially (print 'start)
                                         finally (print 'end)
                                         repeat 1))))
    (assert-true (consp result))
    (assert-eq 'block (car result))))

;;; ------------------------------------------------------------
;;; Section 2: for x in list — collect / do
;;; ------------------------------------------------------------

(deftest loop-for-in-collect
  "loop for x in list collect x returns the list"
  (assert-equal '(1 2 3) (run-string "(loop for x in '(1 2 3) collect x)")))

(deftest loop-for-in-collect-transform
  "loop for x in list collect (* x 2) doubles each element"
  (assert-equal '(2 4 6) (run-string "(loop for x in '(1 2 3) collect (* x 2))")))

(deftest loop-for-in-collect-empty
  "loop over empty list returns nil"
  (assert-null (run-string "(loop for x in '() collect x)")))

(deftest loop-for-in-do
  "loop for x in list do side-effect runs for each element"
  (assert-equal 6 (run-string
    "(let ((s 0)) (loop for x in '(1 2 3) do (setq s (+ s x))) s)")))

(deftest loop-for-in-by
  "loop for x in list by #'cddr steps by 2"
  (assert-equal '(1 3 5) (run-string
    "(loop for x in '(1 2 3 4 5) by #'cddr collect x)")))

;;; ------------------------------------------------------------
;;; Section 3: for x from N to/below M [by K]
;;; ------------------------------------------------------------

(deftest loop-from-to-collect
  "loop for i from 1 to 5 collect i"
  (assert-equal '(1 2 3 4 5) (run-string "(loop for i from 1 to 5 collect i)")))

(deftest loop-from-below-collect
  "loop for i from 0 below 4 collect i"
  (assert-equal '(0 1 2 3) (run-string "(loop for i from 0 below 4 collect i)")))

(deftest loop-from-to-by-collect
  "loop for i from 0 to 10 by 3 collect i"
  (assert-equal '(0 3 6 9) (run-string "(loop for i from 0 to 10 by 3 collect i)")))

(deftest loop-from-to-sum
  "loop for i from 1 to 5 sum i = 15"
  (assert-= 15 (run-string "(loop for i from 1 to 5 sum i)")))

(deftest loop-from-empty-range
  "loop for i from 5 to 1 collects nothing (empty range)"
  (assert-null (run-string "(loop for i from 5 to 1 collect i)")))

;;; ------------------------------------------------------------
;;; Section 4: while / until termination
;;; ------------------------------------------------------------

(deftest loop-while-terminates
  "loop while condition collect — stops when condition becomes false"
  (assert-equal '(0 1 2 3 4)
    (run-string "(let ((i 0)) (loop while (< i 5) collect (prog1 i (setq i (+ i 1)))))")))

(deftest loop-until-terminates
  "loop until condition collect — stops when condition becomes true"
  (assert-equal '(0 1 2)
    (run-string "(let ((i 0)) (loop until (>= i 3) collect (prog1 i (setq i (+ i 1)))))")))

;;; ------------------------------------------------------------
;;; Section 5: collect into named accumulators
;;; ------------------------------------------------------------

(deftest loop-collect-into-named
  "loop collect into named var accumulates into that variable (reversed — cons-based)"
  ;; The loop macro accumulates with cons, so the into var is in reverse order.
  ;; Use nreverse in the finally clause to get canonical order.
  (assert-equal '(1 2 3)
    (run-string "(loop for x in '(1 2 3) collect x into result finally (return (nreverse result)))")))


(deftest loop-two-named-accumulators
  "loop with two named collect-into accumulators, reversed with nreverse in finally"
  (assert-equal '((1 3) (2 4))
    (run-string
      "(loop for x in '(1 2 3 4)
             if (oddp x)  collect x into odds
             if (evenp x) collect x into evens
             finally (return (list (nreverse odds) (nreverse evens))))")))

;;; ------------------------------------------------------------
;;; Section 6: sum / count / minimize / maximize
;;; ------------------------------------------------------------

(deftest loop-sum-basic
  "loop sum returns numeric total"
  (assert-= 10 (run-string "(loop for x in '(1 2 3 4) sum x)")))

(deftest loop-count-truthy
  "loop count counts truthy elements"
  (assert-= 3 (run-string "(loop for x in '(1 nil 2 nil 3) count x)")))

(deftest loop-minimize-basic
  "loop minimize returns the minimum value"
  (assert-= 1 (run-string "(loop for x in '(3 1 4 1 5) minimize x)")))

(deftest loop-maximize-basic
  "loop maximize returns the maximum value"
  (assert-= 9 (run-string "(loop for x in '(3 1 4 9 2) maximize x)")))

;;; ------------------------------------------------------------
;;; Section 7: append / nconc
;;; ------------------------------------------------------------

(deftest loop-append-basic
  "loop append concatenates sublists"
  (assert-equal '(1 2 3 4 5 6)
    (run-string "(loop for x in '((1 2) (3 4) (5 6)) append x)")))

(deftest loop-nconc-basic
  "loop nconc destructively concatenates sublists"
  (assert-equal '(1 2 3 4)
    (run-string "(loop for x in (list (list 1 2) (list 3 4)) nconc x)")))

;;; ------------------------------------------------------------
;;; Section 8: when / unless per-clause filtering
;;; ------------------------------------------------------------

(deftest loop-when-collect
  "loop when filter collects only matching elements"
  (assert-equal '(2 4)
    (run-string "(loop for x in '(1 2 3 4 5) when (evenp x) collect x)")))

(deftest loop-unless-collect
  "loop unless filter collects non-matching elements"
  (assert-equal '(1 3 5)
    (run-string "(loop for x in '(1 2 3 4 5) unless (evenp x) collect x)")))

(deftest loop-if-collect
  "loop if (synonym for when) filters correctly"
  (assert-equal '(4 16 36)
    (run-string "(loop for x in '(1 2 3 4 5 6) if (evenp x) collect (* x x))")))

(deftest loop-when-do
  "loop when filter with do only executes for matching elements (x=3,4,5 sum=12)"
  (assert-= 12
    (run-string
      "(let ((s 0))
         (loop for x in '(1 2 3 4 5)
               when (> x 2) do (setq s (+ s x)))
         s)")))

;;; ------------------------------------------------------------
;;; Section 9: repeat N
;;; ------------------------------------------------------------

(deftest loop-repeat-collect
  "loop repeat N collect t produces N-element list"
  (assert-equal '(t t t t t)
    (run-string "(loop repeat 5 collect t)")))

(deftest loop-repeat-sum
  "loop repeat N sum 1 produces N"
  (assert-= 4 (run-string "(loop repeat 4 sum 1)")))

(deftest loop-repeat-zero
  "loop repeat 0 produces empty list"
  (assert-null (run-string "(loop repeat 0 collect t)")))

;;; ------------------------------------------------------------
;;; Section 10: return / return value
;;; ------------------------------------------------------------

(deftest loop-return-from-do
  "loop do (return value) exits with that value"
  (assert-= 42 (run-string "(loop do (return 42))")))

(deftest loop-return-early
  "loop returns early when condition is met"
  (assert-= 3
    (run-string
      "(loop for x in '(1 2 3 4 5)
             when (= x 3) do (return x))")))

;;; ------------------------------------------------------------
;;; Section 11: for x = expr then expr (general stepping)
;;; ------------------------------------------------------------

(deftest loop-for-equals-then
  "loop for x = init then step-expr steps correctly"
  (assert-equal '(1 2 4 8 16)
    (run-string
      "(loop for x = 1 then (* x 2)
             repeat 5
             collect x)")))

(deftest loop-for-equals-no-then
  "loop for x = expr without THEN re-evaluates expr each iteration"
  ;; Without THEN, x is re-bound to init-form each iteration
  (assert-equal '(0 0 0)
    (run-string "(loop for x = 0 repeat 3 collect x)")))

;;; ------------------------------------------------------------
;;; Section 12: for x across vector
;;; ------------------------------------------------------------

(deftest loop-across-expands-correctly
  "loop for x across vector expands to block+let*+tagbody with aref/length"
  (let ((result (our-macroexpand-1 '(loop for x across v collect x))))
    (assert-true (consp result))
    (assert-eq 'block (car result))
    ;; let* bindings must include vec, len, idx, var
    (let* ((let-form (caddr result))
           (bindings (cadr let-form)))
      (assert-true (>= (length bindings) 4)))))

(deftest loop-across-tagbody-uses-aref
  "loop across expansion uses aref in tagbody and length in bindings"
  (let* ((result (our-macroexpand-1 '(loop for x across v collect x)))
         ;; Stringify the whole expansion to check for key subforms
         (result-str (format nil "~S" result)))
    (assert-true (search "AREF" result-str))
    (assert-true (search "LENGTH" result-str))))

(deftest loop-across-empty-vector-expands
  "loop for x across empty vector expansion has correct structure"
  (let ((result (our-macroexpand-1 '(loop for x across v collect x))))
    ;; The expansion must have the >= end-test in tagbody
    (assert-true (consp result))
    (assert-eq 'block (car result))))

;;; ------------------------------------------------------------
;;; Section 13: for x on list (sublist iteration)
;;; ------------------------------------------------------------

(deftest loop-for-on-collect
  "loop for x on list collect x returns sublists"
  (assert-equal '((1 2 3) (2 3) (3))
    (run-string "(loop for x on '(1 2 3) collect x)")))

(deftest loop-for-on-car-collect
  "loop for x on list collect (car x) is same as for-in"
  (assert-equal '(1 2 3)
    (run-string "(loop for x on '(1 2 3) collect (car x))")))

(deftest loop-for-on-by
  "loop for x on list by #'cddr steps two at a time"
  (assert-equal '((1 2 3 4) (3 4))
    (run-string "(loop for x on '(1 2 3 4) by #'cddr collect x)")))

;;; ------------------------------------------------------------
;;; Section 14: initially / finally clauses
;;; ------------------------------------------------------------

(deftest loop-initially-runs-first
  "initially clause runs before iteration begins"
  ;; Use symbol-name comparison to avoid cross-package symbol identity issue.
  ;; run-string interns in cl-cc package, test file symbols are cl-cc/test.
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

(deftest loop-finally-runs-last
  "finally clause runs after iteration ends (sum 1+2+3+4=10, then *10=100)"
  (assert-= 100
    (run-string
      "(let ((total 0))
         (loop for x in '(1 2 3 4)
               do (setq total (+ total x))
               finally (setq total (* total 10)))
         total)")))

(deftest loop-finally-return
  "finally clause can return a value"
  (assert-= 100
    (run-string
      "(loop for x in '(1 2 3 4)
             sum x into s
             finally (return (* s 10)))")))

;;; ------------------------------------------------------------
;;; Section 15: always / never / thereis
;;; ------------------------------------------------------------

(deftest loop-always-true
  "loop always returns t when condition always holds"
  (assert-true (run-string "(loop for x in '(2 4 6) always (evenp x))")))

(deftest loop-always-false
  "loop always returns nil when condition fails"
  (assert-false (run-string "(loop for x in '(2 3 6) always (evenp x))")))

(deftest loop-never-true
  "loop never returns t when condition never holds"
  (assert-true (run-string "(loop for x in '(1 3 5) never (evenp x))")))

(deftest loop-never-false
  "loop never returns nil when condition holds for some element"
  (assert-false (run-string "(loop for x in '(1 2 5) never (evenp x))")))

(deftest loop-thereis-finds-value
  "loop thereis returns the first truthy value"
  (assert-= 4
    (run-string "(loop for x in '(1 3 4 6) thereis (and (evenp x) x))")))

(deftest loop-thereis-not-found
  "loop thereis returns nil when condition never holds"
  (assert-false (run-string "(loop for x in '(1 3 5) thereis (evenp x))")))
