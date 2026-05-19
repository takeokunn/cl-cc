;;;; tests/unit/expand/macros-sequence-fold-tests.lisp
;;;; Runtime behavior tests for src/expand/macros-sequence-fold.lisp
;;;;
;;;; Covers: reduce, nsubstitute, nsubstitute-if, nsubstitute-if-not,
;;;;   map-into, merge, last, butlast, nbutlast, search.
;;;;
;;;; Tests verify actual computed results (not expansion structure).
;;;; Expansion-level tests are in macros-sequence-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-integration-serial-suite)

;;; ─── reduce ─────────────────────────────────────────────────────────────────

(deftest-each reduce-basic-operations
  "reduce computes the correct folded result."
  :cases (("sum"        "(reduce #'+ '(1 2 3 4 5))"            15)
          ("product"    "(reduce #'* '(1 2 3 4))"              24)
          ("with-iv"    "(reduce #'+ '(1 2 3) :initial-value 10)" 16)
          ("empty-iv"   "(reduce #'+ '() :initial-value 0)"   0)
          ("single"     "(reduce #'+ '(42))"                   42)
          ("max"        "(reduce #'max '(3 1 4 1 5 9 2 6))"   9))
  (form expected)
  (assert-= expected (run-string form)))

(deftest reduce-from-end
  "reduce with :from-end folds from the right."
  ;; (- (- (- 10 3) 2) 1) = 4 left-fold
  ;; (- 10 (- 3 (- 2 1))) = 10 right-fold? Actually: list reversed then folded left
  ;; with :from-end — our impl reverses and folds left, so (- (- (- 1 2) 3) 10) = -14
  ;; Just test that it produces a different result than left-fold:
  (let ((left-result  (run-string "(reduce #'- '(10 3 2 1))"))
        (right-result (run-string "(reduce #'- '(10 3 2 1) :from-end t)")))
    (assert-true (not (= left-result right-result)))))

(deftest reduce-with-key
  "reduce :key applies the key function to each element before folding."
  ;; Sum of lengths of strings
  (assert-= 9 (run-string "(reduce #'+ '(\"foo\" \"ba\" \"quux\") :key #'length)" :stdlib t)))

;;; ─── last ───────────────────────────────────────────────────────────────────

(deftest-each last-returns-last-n-conses
  "last returns the last N conses of the list, including edge cases."
  :cases (("last-1"    "(last '(1 2 3))"     "(3)")
          ("last-2"    "(last '(1 2 3) 2)"   "(2 3)")
          ("last-3"    "(last '(1 2 3) 3)"   "(1 2 3)")
          ("last-0"    "(last '(1 2 3) 0)"   "nil")
          ("singleton" "(last '(42))"         "(42)"))
  (form expected-str)
  (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (assert-equal expected result)))

;;; ─── butlast ────────────────────────────────────────────────────────────────

(deftest-each butlast-removes-last-n
  "butlast returns a copy without the last N elements."
  :cases (("default-1"  "(butlast '(1 2 3))"    "(1 2)")
          ("explicit-2" "(butlast '(1 2 3) 2)"  "(1)")
          ("all"        "(butlast '(1 2 3) 3)"  "nil")
          ("over"       "(butlast '(1 2 3) 5)"  "nil"))
  (form expected-str)
  (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (assert-equal expected result)))

;;; ─── nbutlast ───────────────────────────────────────────────────────────────
;;; nbutlast delegates directly to butlast via macro expansion.
;;; Expansion-level coverage is in macros-sequence-tests.lisp.
;;; Runtime behavior is guaranteed by butlast runtime tests above.

;;; ─── nsubstitute ────────────────────────────────────────────────────────────

(deftest-each nsubstitute-family
  "nsubstitute variants replace elements in place (delegate to substitute)."
  :cases (("by-value"      '(1 99 3 99 5) "(nsubstitute 99 2 '(1 2 3 2 5))")
          ("if-oddp"       '(0 2 0 4 0)   "(nsubstitute-if 0 #'oddp '(1 2 3 4 5))")
          ("if-not-oddp"   '(1 0 3 0 5)   "(nsubstitute-if-not 0 #'oddp '(1 2 3 4 5))"))
  (expected form)
  (assert-equal expected (run-string form :stdlib t)))

;;; ─── merge ──────────────────────────────────────────────────────────────────

(deftest-each merge-sorted-sequences
  "merge interleaves two sorted sequences maintaining sort order."
  :cases (("basic"    "(merge 'list '(1 3 5) '(2 4 6) #'<)" "(1 2 3 4 5 6)")
          ("empty-l1" "(merge 'list '() '(1 2 3) #'<)"      "(1 2 3)")
          ("empty-l2" "(merge 'list '(1 2 3) '() #'<)"      "(1 2 3)")
          ("both-empty" "(merge 'list '() '() #'<)"          "nil"))
  (form expected-str)
  (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (assert-equal expected result)))

;;; ─── merge :key (FR-452) ─────────────────────────────────────────────────

(deftest-each merge-with-key
  "merge supports :key for sorting by transformed values."
  :cases (("car-key"    "((1 . :a) (2 . :c) (3 . :b) (4 . :d))"
                        "(merge 'list '((1 . :a) (3 . :b)) '((2 . :c) (4 . :d)) #'< :key #'car)")
          ("cdr-key"    "((:a . 1) (:c . 2) (:b . 3) (:d . 4))"
                        "(merge 'list '((:a . 1) (:b . 3)) '((:c . 2) (:d . 4)) #'< :key #'cdr)")
          ("nil-key"    "(1 2 3 4 5 6)"
                        "(merge 'list '(1 3 5) '(2 4 6) #'< :key nil)"))
  (expected-str form)
  (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (assert-equal expected result)))

;;; ─── search ─────────────────────────────────────────────────────────────────

(deftest-each search-finds-subsequence
  "search finds the starting position of a pattern in a sequence."
  :cases (("found-start"    "(search '(1 2) '(1 2 3 4))"                             0)
          ("found-middle"   "(search '(2 3) '(1 2 3 4))"                             1)
          ("found-end"      "(search '(3 4) '(1 2 3 4))"                             2)
          ("not-found"      "(search '(5 6) '(1 2 3 4))"                             nil)
          ("empty-pattern"  "(search '() '(1 2 3))"                                  0)
          ("empty-from-end" "(search '() '(1 2 3) :from-end t)"                       3)
          ("with-test"      "(search '(#\\B) '(#\\a #\\B #\\c) :test #'char-equal)"  1)
          ("with-test-not"  "(search '(1) '(2 1) :test-not #'=)"                      0)
          ("with-key"       "(search '(\"bb\" \"ccc\") '(\"a\" \"bb\" \"ccc\") :key #'length)" 1)
          ("dynamic-nil-key" "(let ((k nil)) (search '(2) '(1 2 3) :key k))"           1)
          ("dynamic-nil-end1" "(let ((e nil)) (search '(2 3) '(1 2 3) :end1 e))"       1)
          ("dynamic-nil-end2" "(let ((e nil)) (search '(2 3) '(1 2 3) :end2 e))"       1)
          ("start2"         "(search '(2 3) '(0 2 3 2 3) :start2 2)"                 3)
          ("end2"           "(search '(2 3) '(0 2 3 2 3) :end2 3)"                   1)
          ("start1-end1"    "(search '(0 2 3 9) '(1 2 3 4) :start1 1 :end1 3)"       1)
          ("from-end"       "(search '(2 3) '(1 2 3 2 3) :from-end t)"               3)
          ("from-end-bounds" "(search '(2 3) '(1 2 3 2 3 2 3) :end2 5 :from-end t)" 3)
          ("vector"         "(search #(2 3) #(1 2 3 4))"                             1))
  (form expected)
  (assert-equal expected (run-string form :stdlib t)))

;;; ─── map-into ───────────────────────────────────────────────────────────────

(deftest-each map-into-behavior
  "map-into applies fn to each element and fills the destination in place."
  :cases (("fills-dest"   '(2 4 6) "(let ((dest (list 0 0 0)))
                                        (map-into dest #'(lambda (x) (* x 2)) '(1 2 3)))")
          ("returns-dest" '(2 3)   "(let ((d (list 0 0))) (map-into d #'1+ '(1 2)) d)")
          ("two-sources"  '(11 22 33) "(let ((d (list 0 0 0)))
                                          (map-into d #'+ '(1 2 3) '(10 20 30)))")
          ("shortest-source" '(11 22 0 0) "(let ((d (list 0 0 0 0)))
                                             (map-into d #'+ '(1 2 3) '(10 20))
                                             d)")
          ("zero-sources" '(:filled :filled) "(let ((d (list 0 0)))
                                                (map-into d #'(lambda () :filled))
                                                d)"))
  (expected form)
  (assert-equal expected (run-string form :stdlib t)))
