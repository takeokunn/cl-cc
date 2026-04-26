(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

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

;;; Sections 19-27 (hash USING, parallel FOR, FOR=THEN, WITH, ACROSS,
;;; COLLECT+WHEN, expansion structure, DO+filter, filter+accum) are in
;;; loop-macro-runtime-clauses-tests.lisp.
