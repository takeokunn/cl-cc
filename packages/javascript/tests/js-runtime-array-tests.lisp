;;;; packages/javascript/tests/js-runtime-array-tests.lisp
;;;;
;;;; Array operations (push/pop/shift/map/filter/reduce/sort/flat/splice …),
;;;; ES2023 non-mutating variants (toReversed/toSorted/with/toSpliced),
;;;; and TypedArray construction and element access.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Array core ──────────────────────────────────────────────────────────────

(deftest js-rt-array-make-and-index
  "make-array stores elements at integer indices."
  (let ((a (%jr-arr 10 20 30)))
    (assert-= 3  (length a))
    (assert-= 10 (aref a 0))
    (assert-= 30 (aref a 2))))

(deftest js-rt-array-push-pop
  "push appends (returning new length); pop removes and returns the last element."
  (let ((a (%jr-arr 1 2)))
    (assert-= 3 (cl-cc/javascript::%js-array-push a 3))
    (assert-= 3 (aref a 2))
    (assert-= 3 (cl-cc/javascript::%js-array-pop a))
    (assert-= 2 (length a))))

(deftest js-rt-array-pop-empty-is-undefined
  "pop on an empty array returns undefined."
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-array-pop (%jr-arr))))

(deftest js-rt-array-map-filter
  "map and filter produce correctly transformed and filtered arrays."
  (let ((doubled (cl-cc/javascript::%js-array-map
                  (%jr-arr 1 2 3)
                  (lambda (x &rest _) (declare (ignore _)) (* x 2))))
        (evens   (cl-cc/javascript::%js-array-filter
                  (%jr-arr 1 2 3 4)
                  (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (assert-equal '(2 4 6) (%jr-list doubled))
    (assert-equal '(2 4)   (%jr-list evens))))

(deftest js-rt-array-reduce
  "reduce folds over the array with an initial accumulator."
  (assert-= 10
            (cl-cc/javascript::%js-array-reduce
             (%jr-arr 1 2 3 4)
             (lambda (acc x &rest _) (declare (ignore _)) (+ acc x))
             0)))

(deftest-each js-rt-array-includes
  "includes returns t when the element is present, nil otherwise."
  :cases (("found"   2 t)
          ("missing" 9 nil))
  (needle expected)
  (assert-equal expected
                (cl-cc/javascript::%js-array-includes (%jr-arr 1 2 3) needle)))

(deftest-each js-rt-array-index-of
  "indexOf returns the first index of the element, or -1 when absent."
  :cases (("found"   6  1)
          ("missing" 9 -1))
  (needle expected)
  (assert-= expected
            (cl-cc/javascript::%js-array-index-of (%jr-arr 5 6 7) needle)))

(deftest js-rt-array-join
  "join concatenates with the given separator (default comma)."
  (assert-string= "1,2,3" (cl-cc/javascript::%js-array-join (%jr-arr 1 2 3)))
  (assert-string= "1-2-3" (cl-cc/javascript::%js-array-join (%jr-arr 1 2 3) "-")))

(deftest js-rt-array-shift-unshift
  "shift removes from front; unshift prepends."
  (let ((a (%jr-arr 1 2 3)))
    (assert-= 1   (cl-cc/javascript::%js-array-shift a))
    (assert-equal '(2 3) (%jr-list a))
    (assert-= 4  (cl-cc/javascript::%js-array-unshift a 0 1))
    (assert-equal '(0 1 2 3) (%jr-list a))))

(deftest js-rt-array-some-every
  "some short-circuits on first match; every short-circuits on first mismatch."
  (let ((pred (lambda (x &rest _) (declare (ignore _)) (evenp x))))
    (assert-true  (cl-cc/javascript::%js-array-some  (%jr-arr 1 2 3) pred))
    (assert-false (cl-cc/javascript::%js-array-some  (%jr-arr 1 3 5) pred))
    (assert-true  (cl-cc/javascript::%js-array-every (%jr-arr 2 4 6) pred))
    (assert-false (cl-cc/javascript::%js-array-every (%jr-arr 2 3 4) pred))))

(deftest js-rt-array-find-and-find-index
  "find returns the element; findIndex returns its position."
  (let ((arr (%jr-arr 1 4 9 16)))
    (assert-= 4   (cl-cc/javascript::%js-array-find
                   arr (lambda (x &rest _) (declare (ignore _)) (> x 3))))
    (assert-= 1   (cl-cc/javascript::%js-array-find-index
                   arr (lambda (x &rest _) (declare (ignore _)) (> x 3))))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-array-find
                arr (lambda (x &rest _) (declare (ignore _)) (> x 100))))))

(deftest js-rt-array-slice
  "slice extracts a sub-array without modifying the original."
  (let* ((a   (%jr-arr 10 20 30 40))
         (s1  (cl-cc/javascript::%js-array-slice a 1 3))
         (s2  (cl-cc/javascript::%js-array-slice a 2)))
    (assert-equal '(20 30) (%jr-list s1))
    (assert-equal '(30 40) (%jr-list s2))
    (assert-= 4 (length a))))    ; original untouched

(deftest js-rt-array-splice-delete-insert
  "splice removes and optionally inserts elements."
  (let ((a (%jr-arr 1 2 3 4 5)))
    (cl-cc/javascript::%js-array-splice a 1 2 9 9)
    (assert-equal '(1 9 9 4 5) (%jr-list a))))

(deftest js-rt-array-concat
  "concat merges arrays without modifying the originals."
  (let* ((a (%jr-arr 1 2))
         (b (%jr-arr 3 4))
         (r (cl-cc/javascript::%js-array-concat a b (%jr-arr 5))))
    (assert-equal '(1 2 3 4 5) (%jr-list r))
    (assert-= 2 (length a))))   ; originals untouched

(deftest js-rt-array-reverse
  "reverse mutates and returns the reversed array."
  (let ((a (%jr-arr 1 2 3)))
    (let ((r (cl-cc/javascript::%js-array-reverse a)))
      (assert-equal '(3 2 1) (%jr-list r))
      (assert-eq a r))))         ; same object

(deftest js-rt-array-sort-default
  "sort with no comparator uses lexicographic order: \"10\" < \"2\" < \"9\"."
  (let* ((a (%jr-arr 9 10 2))
         (r (cl-cc/javascript::%js-array-sort a)))
    (assert-equal '(10 2 9) (%jr-list r))   ; lexicographic
    (assert-eq a r)))

(deftest js-rt-array-sort-numeric
  "sort with numeric comparator sorts numerically."
  (let* ((a (%jr-arr 10 2 30))
         (r (cl-cc/javascript::%js-array-sort
             a (lambda (x y &rest _) (declare (ignore _)) (- x y)))))
    (assert-equal '(2 10 30) (%jr-list r))))

(deftest js-rt-array-flat
  "flat flattens one level by default; depth controls levels."
  (let* ((nested (%jr-arr 1 (%jr-arr 2 3) (%jr-arr 4 (%jr-arr 5)))))
    (assert-equal '(1 2 3 4 5)       (%jr-list (cl-cc/javascript::%js-array-flat nested 2)))
    (let ((shallow (%jr-arr 1 (%jr-arr 2 3) (%jr-arr 4 (%jr-arr 5)))))
      ;; depth 1: [1, 2, 3, 4, [5]] — inner [5] stays nested
      (assert-= 5 (length (%jr-list (cl-cc/javascript::%js-array-flat shallow 1)))))))

(deftest js-rt-array-last-index-of
  "lastIndexOf returns rightmost index of element, -1 if absent."
  (let ((a (%jr-arr 1 2 3 2 1)))
    (assert-= 3 (cl-cc/javascript::%js-array-last-index-of a 2))
    (assert-= -1 (cl-cc/javascript::%js-array-last-index-of a 9))))

(deftest js-rt-array-fill
  "fill fills a range of the array with a value."
  (let* ((a (%jr-arr 1 2 3 4 5))
         (r (cl-cc/javascript::%js-array-fill a 0 1 3)))
    (assert-equal '(1 0 0 4 5) (%jr-list r))
    (assert-eq a r)))           ; mutated in place

(deftest js-rt-array-reduce-right
  "reduceRight folds from right to left."
  (let ((result (cl-cc/javascript::%js-array-reduce-right
                 (%jr-arr 1 2 3 4)
                 (lambda (acc x &rest _) (declare (ignore _)) (cons x acc))
                 nil)))
    (assert-equal '(1 2 3 4) result)))

;;; ─── ES2023 non-mutating array methods ──────────────────────────────────────

(deftest js-rt-array-to-reversed
  "toReversed returns a new reversed array without mutating the original."
  (let* ((orig (%jr-arr 1 2 3))
         (rev  (cl-cc/javascript::%js-array-to-reversed orig)))
    (assert-equal '(3 2 1) (%jr-list rev))
    (assert-equal '(1 2 3) (%jr-list orig))))

(deftest js-rt-array-to-sorted
  "toSorted returns a sorted copy without mutating the original."
  (let* ((orig (%jr-arr 3 1 2))
         (srt  (cl-cc/javascript::%js-array-to-sorted orig)))
    (assert-equal '(1 2 3) (%jr-list srt))
    (assert-equal '(3 1 2) (%jr-list orig))))

(deftest-each js-rt-array-with
  "with(index, value) returns a copy with one element replaced."
  :cases (("mid-index"  1   99  '(10 99 30))
          ("neg-index" -1   99  '(10 20 99)))
  (idx val expected)
  (assert-equal expected (%jr-list (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) idx val))))

(deftest-each js-rt-array-at
  "at() supports both positive and negative indices."
  :cases (("positive"  1   20)
          ("negative" -1   30)
          ("oob"       9   :js-undefined))
  (idx expected)
  (let ((a (%jr-arr 10 20 30))
        (undef cl-cc/javascript::+js-undefined+))
    (let ((got (cl-cc/javascript::%js-array-at a idx)))
      (if (eq expected :js-undefined)
          (assert-eq undef got)
          (assert-= expected got)))))

(deftest js-rt-array-find-last
  "findLast returns the last element matching the predicate."
  (let ((result (cl-cc/javascript::%js-array-find-last
                 (%jr-arr 1 2 3 4)
                 (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (assert-= 4 result)))

(deftest js-rt-array-find-last-index
  "findLastIndex returns the index of the last matching element."
  (let ((result (cl-cc/javascript::%js-array-find-last-index
                 (%jr-arr 1 2 3 4)
                 (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (assert-= 3 result)))

(deftest js-rt-array-to-spliced
  "toSpliced returns a modified copy without mutating the original."
  (let* ((orig (%jr-arr 1 2 3 4))
         (result (cl-cc/javascript::%js-array-to-spliced orig 1 2 9 9)))
    (assert-equal '(1 9 9 4) (%jr-list result))
    (assert-equal '(1 2 3 4) (%jr-list orig))))

(deftest js-rt-array-of
  "Array.of creates an array from positional arguments."
  (assert-equal '(5 6 7) (%jr-list (cl-cc/javascript::%js-array-of 5 6 7))))

;;; ─── TypedArray basic operations ─────────────────────────────────────────────

(deftest js-rt-typed-array-make-get-set
  "make-typed-array, ta-get, ta-set round-trip correctly."
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" 3)))
    (cl-cc/javascript::%js-ta-set ta 0 10)
    (cl-cc/javascript::%js-ta-set ta 2 99)
    (assert-= 10 (cl-cc/javascript::%js-ta-get ta 0))
    (assert-= 0  (cl-cc/javascript::%js-ta-get ta 1))
    (assert-= 99 (cl-cc/javascript::%js-ta-get ta 2))))

(deftest js-rt-typed-array-length
  "ta-length struct accessor returns the number of elements."
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Float64Array" 4)))
    (assert-= 4 (cl-cc/javascript::js-ta-length ta))))

(deftest-each js-rt-typed-array-types
  "Various TypedArray types are constructed with the correct length."
  :cases (("Int8Array"    "Int8Array"    3 3)
          ("Uint8Array"   "Uint8Array"   5 5)
          ("Int32Array"   "Int32Array"   2 2)
          ("Float64Array" "Float64Array" 1 1))
  (type-name length expected-length)
  (let ((ta (cl-cc/javascript::%js-make-typed-array type-name length)))
    (assert-= expected-length (cl-cc/javascript::js-ta-length ta))))

(deftest js-rt-method-resolution-array
  "Calling a method through get-prop on an array invokes the correct helper."
  (let* ((arr (%jr-arr 10 20 30))
         (join-fn (cl-cc/javascript::%js-get-prop arr "join")))
    (assert-true (functionp join-fn))
    (assert-string= "10,20,30" (funcall join-fn ","))))

(deftest js-rt-method-resolution-length
  "Accessing .length on an array returns the numeric length."
  (let* ((arr (%jr-arr 1 2 3)))
    (assert-= 3 (cl-cc/javascript::%js-get-prop arr "length"))))

(deftest js-rt-array-values-via-get-prop
  "Array values() method resolves to an iterator over the elements."
  (let* ((arr   (%jr-arr 10 20))
         (fn    (cl-cc/javascript::%js-get-prop arr "values"))
         (iter  (funcall fn))
         (next  (gethash "next" iter))
         (r1    (funcall next))
         (r2    (funcall next))
         (done  (funcall next)))
    (assert-= 10 (gethash "value" r1))
    (assert-= 20 (gethash "value" r2))
    (assert-true  (gethash "done"  done))))

(deftest js-rt-array-@@iterator-via-get-prop
  "Array @@iterator() returns an independent iterator (iterable-iterator protocol)."
  (let* ((arr   (%jr-arr 5 6))
         (fn    (cl-cc/javascript::%js-get-prop arr "@@iterator"))
         (iter  (funcall fn))
         (next  (gethash "next" iter)))
    (assert-true  (functionp next))
    (assert-= 5  (gethash "value" (funcall next)))
    (assert-= 6  (gethash "value" (funcall next)))))

;;; ─── Uncovered array methods ─────────────────────────────────────────────────

(deftest js-rt-array-flat-map
  "flatMap maps then flattens one level."
  (let* ((arr    (%jr-arr 1 2 3))
         (result (cl-cc/javascript::%js-array-flat-map
                  arr (lambda (x &rest _) (declare (ignore _)) (%jr-arr x (* x 10))))))
    (assert-equal '(1 10 2 20 3 30) (%jr-list result))))

(deftest js-rt-array-entries
  "entries returns [[0,v0],[1,v1],...] pairs."
  (let* ((arr     (%jr-arr "a" "b"))
         (entries (cl-cc/javascript::%js-array-entries arr)))
    (assert-= 2 (length entries))
    (let ((e0 (aref entries 0)))
      (assert-= 0 (aref e0 0))
      (assert-string= "a" (aref e0 1)))))

(deftest js-rt-array-keys
  "keys returns a vector of indices [0, 1, 2, ...]."
  (let* ((arr  (%jr-arr "x" "y" "z"))
         (ks   (cl-cc/javascript::%js-array-keys arr)))
    (assert-equal '(0 1 2) (%jr-list ks))))

(deftest js-rt-array-copy-within
  "copyWithin copies a section to another position in-place."
  (let ((arr (%jr-arr 1 2 3 4 5)))
    (cl-cc/javascript::%js-array-copy-within arr 0 3 5)
    (assert-= 4 (aref arr 0))
    (assert-= 5 (aref arr 1))
    (assert-= 3 (aref arr 2))))
