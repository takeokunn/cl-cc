;;;; loop-macro-runtime-edge-tests.lisp — LOOP macro edge/error/destructuring tests
;;;
;;; Sections 32-36: error paths, dotted-pair destructuring, implicit body,
;;; hash-values USING, multiple accumulators, and deftest-each consolidations.

(in-package :cl-cc/test)

(in-suite loop-macro-suite)

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
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-two-into-accumulators-via-finally
  "two named INTO accumulators; FINALLY returns them as a list"
  "(loop for x in '(1 2 3 4 5)
         if (oddp x)  collect x into odds
         if (evenp x) collect x into evens
         finally (return (list (nreverse odds) (nreverse evens))))"
  '((1 3 5) (2 4)))

;;; Consolidation tests

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

(deftest-each loop-numeric
  "LOOP numeric accumulations — paths not covered individually."
  :cases (("repeat-zero"  0  "(let ((n 0)) (loop repeat 0 do (setq n (+ n 1))) n)")
          ("hash-values" 30  "(let ((ht (make-hash-table))) (setf (gethash 'x ht) 10) (setf (gethash 'y ht) 20) (loop for v being the hash-values of ht sum v))")
          ("with-clause" 15  "(loop with sum = 0 for i from 1 to 5 do (setq sum (+ sum i)) finally (return sum))")
          ("sum-into"    15  "(loop for i from 1 to 5 sum i into total finally (return total))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

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
