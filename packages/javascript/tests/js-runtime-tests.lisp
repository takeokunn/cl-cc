;;;; packages/javascript/tests/js-runtime-tests.lisp — ES2026 JS runtime builtin tests
;;;;
;;;; Behavior tests for %js-* runtime helpers: Array, String, Math, Object,
;;;; type-coercion. These exercise functions directly (parser/e2e tests only
;;;; check AST shape), covering the largest untested surface of the JS frontend.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────────

(defun %jr-arr (&rest els)
  "Build a JS array (adjustable vector) from ELS."
  (apply #'cl-cc/javascript::%js-make-array els))

(defun %jr-list (vec)
  "Coerce a JS array (vector) to a CL list for easy comparison."
  (coerce vec 'list))

;;; ─── typeof ──────────────────────────────────────────────────────────────────

(deftest-each js-rt-typeof
  "typeof maps CL values to their JS type strings.
   null returns \"object\" per the historic JS quirk."
  :cases (("number"    42                                 "number")
          ("string"    "hi"                               "string")
          ("bool-true" t                                  "boolean")
          ("bool-nil"  nil                                "boolean")
          ("undefined" cl-cc/javascript::+js-undefined+  "undefined")
          ("null"      cl-cc/javascript::+js-null+        "object"))
  (value expected)
  (assert-string= expected (cl-cc/javascript::%js-typeof value)))

(deftest js-rt-typeof-array-is-object
  "typeof an array is \"object\"."
  (assert-string= "object" (cl-cc/javascript::%js-typeof (%jr-arr 1 2))))

;;; ─── ToString ────────────────────────────────────────────────────────────────

(deftest-each js-rt-to-string
  "ToString coerces CL values to JS string representations."
  :cases (("integer"   42                                "42")
          ("true"      t                                 "true")
          ("false"     nil                               "false")
          ("null"      cl-cc/javascript::+js-null+       "null")
          ("undefined" cl-cc/javascript::+js-undefined+  "undefined")
          ("nan"       :js-nan                           "NaN"))
  (value expected)
  (assert-string= expected (cl-cc/javascript::%js-to-string value)))

;;; ─── Equality / truthiness ────────────────────────────────────────────────────

(deftest js-rt-strict-eq
  "=== (strict equality) uses type+value identity."
  (assert-true  (cl-cc/javascript::%js-strict-eq 3 3))
  (assert-false (cl-cc/javascript::%js-strict-eq 3 4))
  (assert-true  (cl-cc/javascript::%js-strict-eq "a" "a"))
  (assert-false (cl-cc/javascript::%js-strict-eq "a" "b")))

(deftest js-rt-truthy
  "Truthiness: 0/\"\"/nil/undefined/null are falsy; everything else is truthy."
  (assert-true  (cl-cc/javascript::%js-truthy 1))
  (assert-true  (cl-cc/javascript::%js-truthy "x"))
  (assert-false (cl-cc/javascript::%js-truthy 0))
  (assert-false (cl-cc/javascript::%js-truthy ""))
  (assert-false (cl-cc/javascript::%js-truthy nil))
  (assert-false (cl-cc/javascript::%js-truthy cl-cc/javascript::+js-undefined+))
  (assert-false (cl-cc/javascript::%js-truthy cl-cc/javascript::+js-null+)))

;;; ─── Array ───────────────────────────────────────────────────────────────────

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

;;; ─── String ──────────────────────────────────────────────────────────────────

(deftest js-rt-string-slice
  "slice handles positive and negative index arguments."
  (assert-string= "ell" (cl-cc/javascript::%js-string-slice "hello" 1 4))
  (assert-string= "lo"  (cl-cc/javascript::%js-string-slice "hello" -2)))

(deftest js-rt-string-search
  "includes and indexOf locate substrings."
  (assert-true  (cl-cc/javascript::%js-string-includes  "hello" "ell"))
  (assert-false (cl-cc/javascript::%js-string-includes  "hello" "xyz"))
  (assert-=  2  (cl-cc/javascript::%js-string-index-of  "hello" "l"))
  (assert-= -1  (cl-cc/javascript::%js-string-index-of  "hello" "z")))

(deftest js-rt-string-case
  "toUpperCase and toLowerCase."
  (assert-string= "HELLO" (cl-cc/javascript::%js-string-to-upper-case "hello"))
  (assert-string= "hello" (cl-cc/javascript::%js-string-to-lower-case "HELLO")))

(deftest js-rt-string-repeat
  "repeat n times; repeat 0 yields empty string."
  (assert-string= "ababab" (cl-cc/javascript::%js-string-repeat "ab" 3))
  (assert-string= ""       (cl-cc/javascript::%js-string-repeat "ab" 0)))

(deftest js-rt-string-split-empty-sep
  "split with empty separator yields individual characters."
  (assert-equal '("a" "b" "c")
                (%jr-list (cl-cc/javascript::%js-string-split "abc" ""))))

;;; ─── Math ────────────────────────────────────────────────────────────────────

(deftest js-rt-math-unary
  "Math unary: abs, floor, ceil, sign."
  (assert-= 5  (cl-cc/javascript::%js-math-abs   -5))
  (assert-= 3  (cl-cc/javascript::%js-math-floor  3.7d0))
  (assert-= 4  (cl-cc/javascript::%js-math-ceil   3.2d0))
  (assert-= -1 (cl-cc/javascript::%js-math-sign  -7))
  (assert-=  0 (cl-cc/javascript::%js-math-sign   0))
  (assert-=  1 (cl-cc/javascript::%js-math-sign   7)))

(deftest js-rt-math-binary
  "Math binary: max, min, pow, sqrt."
  (assert-= 9     (cl-cc/javascript::%js-math-max  3 9 1))
  (assert-= 1     (cl-cc/javascript::%js-math-min  3 9 1))
  (assert-= 8     (cl-cc/javascript::%js-math-pow  2 3))
  (assert-= 3.0d0 (cl-cc/javascript::%js-math-sqrt 9)))

;;; ─── Object ──────────────────────────────────────────────────────────────────

(deftest js-rt-object-make-access
  "make-object stores key/value pairs accessible via get-prop."
  (let ((o (cl-cc/javascript::%js-make-object "a" 1 "b" 2)))
    (assert-= 1 (cl-cc/javascript::%js-get-prop o "a"))
    (assert-= 2 (cl-cc/javascript::%js-get-prop o "b"))))

(deftest js-rt-object-keys-values
  "keys and values return the own enumerable properties (order-independent)."
  (let* ((o  (cl-cc/javascript::%js-make-object "x" 10 "y" 20))
         (ks (sort (%jr-list (cl-cc/javascript::%js-object-keys   o)) #'string<))
         (vs (sort (%jr-list (cl-cc/javascript::%js-object-values o)) #'<)))
    (assert-equal '("x" "y") ks)
    (assert-equal '(10 20)   vs)))

(deftest js-rt-get-prop-array
  "get-prop \"length\" and numeric index work on arrays."
  (let ((a (%jr-arr 10 20 30)))
    (assert-= 3  (cl-cc/javascript::%js-get-prop a "length"))
    (assert-= 20 (cl-cc/javascript::%js-get-prop a 1))))

;;; ─── Loose equality ──────────────────────────────────────────────────────────

(deftest-each js-rt-loose-eq
  "== applies type coercion: null==undefined, booleans as numbers, etc."
  :cases (("identity"      1       1       t)
          ("null-undef"    cl-cc/javascript::+js-null+      cl-cc/javascript::+js-undefined+ t)
          ("undef-null"    cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-null+      t)
          ("num-str"       3       "3"     t)
          ("str-num"       "3"     3       t)
          ("true-1"        t       1       t)
          ("nan-nan"       :js-nan :js-nan nil)
          ("mismatch"      1       2       nil))
  (a b expected)
  (assert-equal expected (cl-cc/javascript::%js-loose-eq a b)))

;;; ─── Bitwise operators ───────────────────────────────────────────────────────

(deftest-each js-rt-bitwise-or
  "Bitwise OR: coerces to int32, ORs bits, sign-extends."
  :cases (("simple"   #b1010  #b1100  #b1110)
          ("neg"      -1      0       -1)
          ("zero"     0       0       0))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%js-bitwise-or a b)))

(deftest-each js-rt-bitwise-and
  "Bitwise AND: coerces to int32, ANDs bits."
  :cases (("simple"   #b1010  #b1100  #b1000)
          ("zero"     5       0       0)
          ("same"     7       7       7))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%js-bitwise-and a b)))

(deftest-each js-rt-bitwise-xor
  "Bitwise XOR: coerces to int32, XORs bits."
  :cases (("simple"   #b1010  #b1100  #b0110)
          ("same"     7       7       0)
          ("zero"     0       5       5))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%js-bitwise-xor a b)))

(deftest js-rt-bitwise-not
  "Bitwise NOT (~x) inverts all 32 bits and sign-extends."
  (assert-= -1 (cl-cc/javascript::%js-bitwise-not 0))
  (assert-=  0 (cl-cc/javascript::%js-bitwise-not -1))
  (assert-= -2 (cl-cc/javascript::%js-bitwise-not 1)))

(deftest-each js-rt-shift-left
  "Left shift << by various amounts."
  :cases (("shl-2"   2  2  8)
          ("shl-1"   1  4  16)
          ("shl-0"   5  0  5))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%js-shift-left a b)))

(deftest-each js-rt-shift-right
  "Arithmetic right shift >>: sign bit is preserved."
  :cases (("pos"  8  2   2)
          ("neg" -8  2  -2))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%js-shift-right a b)))

(deftest js-rt-unsigned-shift-right
  "Unsigned right shift >>> always yields a non-negative result."
  (assert-= 1073741822 (cl-cc/javascript::%js-unsigned-shift-right -8 2))
  (assert-= 2          (cl-cc/javascript::%js-unsigned-shift-right  8 2)))

;;; ─── Set built-ins ───────────────────────────────────────────────────────────

(defun %jr-set (&rest vals)
  "Build a JS Set (hash-table with t values) from VALS."
  (let ((ht (cl-cc/javascript::%js-make-ht)))
    (dolist (v vals ht)
      (cl-cc/javascript::%js-set-add ht v))))

(deftest js-rt-set-basic
  "add/has/delete/size/clear on a set."
  (let ((s (%jr-set 1 2 3)))
    (assert-= 3 (cl-cc/javascript::%js-set-size s))
    (assert-true  (cl-cc/javascript::%js-set-has s 2))
    (assert-false (cl-cc/javascript::%js-set-has s 9))
    (cl-cc/javascript::%js-set-delete s 2)
    (assert-= 2 (cl-cc/javascript::%js-set-size s))
    (cl-cc/javascript::%js-set-clear s)
    (assert-= 0 (cl-cc/javascript::%js-set-size s))))

(deftest js-rt-set-union
  "union produces a set containing elements of both."
  (let* ((a (%jr-set 1 2))
         (b (%jr-set 2 3))
         (u (cl-cc/javascript::%js-set-union a b)))
    (assert-= 3 (cl-cc/javascript::%js-set-size u))
    (assert-true (cl-cc/javascript::%js-set-has u 1))
    (assert-true (cl-cc/javascript::%js-set-has u 3))))

(deftest js-rt-set-intersection
  "intersection contains only elements in both."
  (let* ((a (%jr-set 1 2 3))
         (b (%jr-set 2 3 4))
         (i (cl-cc/javascript::%js-set-intersection a b)))
    (assert-= 2 (cl-cc/javascript::%js-set-size i))
    (assert-true  (cl-cc/javascript::%js-set-has i 2))
    (assert-false (cl-cc/javascript::%js-set-has i 1))))

(deftest js-rt-set-difference
  "difference: elements in A but not B."
  (let* ((a (%jr-set 1 2 3))
         (b (%jr-set 2))
         (d (cl-cc/javascript::%js-set-difference a b)))
    (assert-= 2 (cl-cc/javascript::%js-set-size d))
    (assert-false (cl-cc/javascript::%js-set-has d 2))))

(deftest js-rt-set-subset-disjoint
  "is-subset-of and is-disjoint-from."
  (let ((a (%jr-set 1 2))
        (b (%jr-set 1 2 3))
        (c (%jr-set 4 5)))
    (assert-true  (cl-cc/javascript::%js-set-is-subset-of   a b))
    (assert-false (cl-cc/javascript::%js-set-is-subset-of   b a))
    (assert-true  (cl-cc/javascript::%js-set-is-disjoint-from a c))
    (assert-false (cl-cc/javascript::%js-set-is-disjoint-from a b))))

;;; ─── Iterator helpers ────────────────────────────────────────────────────────

(deftest js-rt-iterator-map-filter
  "Iterator.map and Iterator.filter over a vector iterator."
  (let* ((iter  (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3 4)))
         (evens (cl-cc/javascript::%js-iterator-filter
                 iter (lambda (x &rest _) (declare (ignore _)) (evenp x))))
         (doubled (cl-cc/javascript::%js-iterator-map
                   (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3))
                   (lambda (x &rest _) (declare (ignore _)) (* x 2)))))
    (assert-equal '(2 4)
                  (%jr-list (cl-cc/javascript::%js-iterator-to-array evens)))
    (assert-equal '(2 4 6)
                  (%jr-list (cl-cc/javascript::%js-iterator-to-array doubled)))))

(deftest js-rt-iterator-take-drop
  "take and drop limit/skip elements."
  (let* ((src (%jr-arr 10 20 30 40 50))
         (taken (cl-cc/javascript::%js-iterator-to-array
                 (cl-cc/javascript::%js-iterator-take
                  (cl-cc/javascript::%js-vec-to-iter src) 3)))
         (dropped (cl-cc/javascript::%js-iterator-to-array
                   (cl-cc/javascript::%js-iterator-drop
                    (cl-cc/javascript::%js-vec-to-iter src) 2))))
    (assert-equal '(10 20 30) (%jr-list taken))
    (assert-equal '(30 40 50) (%jr-list dropped))))

(deftest js-rt-iterator-reduce
  "Iterator.reduce folds to a single value."
  (let* ((iter (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3 4)))
         (sum  (cl-cc/javascript::%js-iterator-reduce
                iter (lambda (a b &rest _) (declare (ignore _)) (+ a b)) 0)))
    (assert-= 10 sum)))

;;; ─── Promise built-ins ───────────────────────────────────────────────────────

(deftest js-rt-promise-resolve-await
  "Resolved promise: await returns its value."
  (let* ((p (cl-cc/javascript::%js-promise-resolve 42))
         (v (cl-cc/javascript::%js-await p)))
    (assert-= 42 v)))

(deftest js-rt-promise-reject-await
  "Rejected promise: await raises js-exception."
  (let ((p (cl-cc/javascript::%js-promise-reject "oops")))
    (assert-signals
     'cl-cc/javascript::js-exception
     (cl-cc/javascript::%js-await p))))

(deftest js-rt-promise-then
  "then chains fulfilled value through on-fulfilled callback."
  (let* ((p (cl-cc/javascript::%js-promise-resolve 5))
         (p2 (cl-cc/javascript::%js-promise-then
              p (lambda (v &rest _) (declare (ignore _)) (* v 2)))))
    (assert-= 10 (cl-cc/javascript::%js-await p2))))

(deftest js-rt-promise-all
  "all resolves with an array when every promise fulfills."
  (let* ((promises (%jr-arr (cl-cc/javascript::%js-promise-resolve 1)
                            (cl-cc/javascript::%js-promise-resolve 2)
                            (cl-cc/javascript::%js-promise-resolve 3)))
         (result (cl-cc/javascript::%js-await
                  (cl-cc/javascript::%js-promise-all promises))))
    (assert-equal '(1 2 3) (%jr-list result))))

;;; ─── for-of / for-in ─────────────────────────────────────────────────────────

(deftest js-rt-for-of-array
  "for-of calls body-fn for each array element in order."
  (let ((seen nil))
    (cl-cc/javascript::%js-for-of
     (%jr-arr 10 20 30)
     (lambda (x &rest _) (declare (ignore _)) (push x seen)))
    (assert-equal '(10 20 30) (nreverse seen))))

(deftest js-rt-for-of-string
  "for-of iterates a string character by character."
  (let ((chars nil))
    (cl-cc/javascript::%js-for-of
     "hi"
     (lambda (c &rest _) (declare (ignore _)) (push c chars)))
    (assert-equal '("h" "i") (nreverse chars))))

(deftest js-rt-for-in-object
  "for-in yields enumerable own keys, skipping __proto__ etc."
  (let ((o (cl-cc/javascript::%js-make-object "a" 1 "b" 2))
        (keys nil))
    (cl-cc/javascript::%js-for-in o (lambda (k &rest _) (declare (ignore _)) (push k keys)))
    (assert-equal '("a" "b") (sort keys #'string<))))

;;; ─── try-catch-finally ───────────────────────────────────────────────────────

(deftest js-rt-try-catch
  "try-catch: catch receives the thrown value."
  (let ((caught nil))
    (cl-cc/javascript::%js-try-catch-finally
     (lambda () (cl-cc/javascript::%js-throw "err"))
     (lambda (v) (setf caught v))
     nil)
    (assert-string= "err" caught)))

(deftest js-rt-try-finally-runs
  "finally runs even when no exception is thrown."
  (let ((ran nil))
    (cl-cc/javascript::%js-try-catch-finally
     (lambda () 42)
     nil
     (lambda () (setf ran t)))
    (assert-true ran)))
