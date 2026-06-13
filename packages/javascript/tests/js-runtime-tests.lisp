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
   null and arrays return \"object\" per ECMAScript."
  :cases (("number"    42                                 "number")
          ("string"    "hi"                               "string")
          ("bool-true" t                                  "boolean")
          ("bool-nil"  nil                                "boolean")
          ("undefined" cl-cc/javascript::+js-undefined+  "undefined")
          ("null"      cl-cc/javascript::+js-null+        "object")
          ("array"     (%jr-arr 1 2)                      "object"))
  (value expected)
  (assert-string= expected (cl-cc/javascript::%js-typeof value)))

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

(deftest-each js-rt-strict-eq
  "=== (strict equality) uses type+value identity."
  :cases (("same-num"  3   3   t)
          ("diff-num"  3   4   nil)
          ("same-str"  "a" "a" t)
          ("diff-str"  "a" "b" nil))
  (a b expected)
  (assert-equal expected (cl-cc/javascript::%js-strict-eq a b)))

(deftest-each js-rt-truthy
  "0/\"\"/nil/undefined/null are falsy; everything else is truthy."
  :cases (("truthy-num"  1                                    t)
          ("truthy-str"  "x"                                  t)
          ("falsy-zero"  0                                    nil)
          ("falsy-str"   ""                                   nil)
          ("falsy-nil"   nil                                  nil)
          ("falsy-undef" cl-cc/javascript::+js-undefined+     nil)
          ("falsy-null"  cl-cc/javascript::+js-null+          nil))
  (value expected)
  (assert-equal expected (cl-cc/javascript::%js-truthy value)))

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

(deftest-each js-rt-string-case
  "toUpperCase and toLowerCase normalize character case."
  :cases (("upper" #'cl-cc/javascript::%js-string-to-upper-case "hello" "HELLO")
          ("lower" #'cl-cc/javascript::%js-string-to-lower-case "HELLO" "hello"))
  (fn input expected)
  (assert-string= expected (funcall fn input)))

(deftest-each js-rt-string-repeat
  "repeat n times; repeat 0 yields empty string."
  :cases (("three-times" "ab" 3 "ababab")
          ("zero-times"  "ab" 0 ""))
  (s n expected)
  (assert-string= expected (cl-cc/javascript::%js-string-repeat s n)))

(deftest js-rt-string-split-empty-sep
  "split with empty separator yields individual characters."
  (assert-equal '("a" "b" "c")
                (%jr-list (cl-cc/javascript::%js-string-split "abc" ""))))

;;; ─── Math ────────────────────────────────────────────────────────────────────

(deftest-each js-rt-math-unary
  "Each Math unary function maps one numeric input to the expected output."
  :cases (("abs-neg"   #'cl-cc/javascript::%js-math-abs    '(-5)     5)
          ("floor"     #'cl-cc/javascript::%js-math-floor  '(3.7d0)  3)
          ("ceil"      #'cl-cc/javascript::%js-math-ceil   '(3.2d0)  4)
          ("sign-neg"  #'cl-cc/javascript::%js-math-sign   '(-7)    -1)
          ("sign-zero" #'cl-cc/javascript::%js-math-sign   '(0)      0)
          ("sign-pos"  #'cl-cc/javascript::%js-math-sign   '(7)      1))
  (fn args expected)
  (assert-= expected (apply fn args)))

(deftest-each js-rt-math-binary
  "Each Math binary/variadic function applied to args-list yields expected result."
  :cases (("max"   #'cl-cc/javascript::%js-math-max  '(3 9 1) 9)
          ("min"   #'cl-cc/javascript::%js-math-min  '(3 9 1) 1)
          ("pow"   #'cl-cc/javascript::%js-math-pow  '(2 3)   8)
          ("sqrt"  #'cl-cc/javascript::%js-math-sqrt '(9)     3.0d0))
  (fn args expected)
  (assert-= expected (apply fn args)))

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

(deftest-each js-rt-bitwise-not
  "Bitwise NOT (~x) inverts all 32 bits and sign-extends."
  :cases (("zero"     0  -1)
          ("neg-one" -1   0)
          ("one"      1  -2))
  (x expected)
  (assert-= expected (cl-cc/javascript::%js-bitwise-not x)))

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

(deftest-each js-rt-unsigned-shift-right
  "Unsigned right shift >>> always yields a non-negative result."
  :cases (("neg"  -8 2 1073741822)
          ("pos"   8 2 2))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%js-unsigned-shift-right a b)))

;;; ─── Set built-ins ───────────────────────────────────────────────────────────

(defun %jr-set (&rest vals)
  "Build a JS Set from VALS."
  (let ((s (cl-cc/javascript::%js-make-set)))
    (dolist (v vals s)
      (cl-cc/javascript::%js-set-add s v))))

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
     cl-cc/javascript:js-exception
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

(deftest-each js-rt-for-of
  "for-of collects elements in order from different iterable types."
  :cases (("array"  (%jr-arr 10 20 30)  '(10 20 30))
          ("string" "hi"                '("h" "i")))
  (iterable expected)
  (let ((seen nil))
    (cl-cc/javascript::%js-for-of
     iterable
     (lambda (x &rest _) (declare (ignore _)) (push x seen)))
    (assert-equal expected (nreverse seen))))

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

;;; ─── Number.prototype methods (via %js-resolve-number-method) ────────────────

(deftest-each js-rt-number-to-fixed
  "Number.toFixed(digits): render a number to N decimal places."
  :cases (("zero-digits"  3.14159d0 0 "3")
          ("two-digits"   3.14159d0 2 "3.14")
          ("integer"      42.0d0    0 "42"))
  (n digits expected)
  (let* ((method (cl-cc/javascript::%js-resolve-number-method n "toFixed"))
         (result (funcall method digits)))
    (assert-string= expected result)))

(deftest-each js-rt-number-to-string-radix
  "Number.toString(radix): render integer in the given base."
  :cases (("base-10"  255 10 "255")
          ("base-16"  255 16 "ff")
          ("base-2"     8  2 "1000"))
  (n radix expected)
  (let* ((method (cl-cc/javascript::%js-resolve-number-method n "toString"))
         (result (funcall method radix)))
    (assert-string= expected result)))

(deftest-each js-rt-number-to-precision
  "Number.prototype.toPrecision(p): fixed notation when exponent in [-6,p)."
  :cases (("one-sig"    123.456d0 1 "1e+2")
          ("three-sig"  123.456d0 3 "123")
          ("six-sig"    3.14159d0 6 "3.14159")
          ("zero"       0.0d0     3 "0.00"))
  (n prec expected)
  (assert-string= expected
    (cl-cc/javascript::%js-number-to-precision n prec)))

;;; ─── Map built-ins ───────────────────────────────────────────────────────────

(deftest js-rt-map-set-get-has-size
  "Map set/get/has/size/delete."
  (let ((m (cl-cc/javascript::%js-make-map)))
    (cl-cc/javascript::%js-map-set m "k" 42)
    (assert-= 1 (cl-cc/javascript::%js-map-size m))
    (assert-true  (cl-cc/javascript::%js-map-has m "k"))
    (assert-false (cl-cc/javascript::%js-map-has m "x"))
    (assert-= 42  (cl-cc/javascript::%js-map-get m "k"))
    (cl-cc/javascript::%js-map-delete m "k")
    (assert-= 0 (cl-cc/javascript::%js-map-size m))))

(deftest js-rt-map-for-each-order
  "Map.forEach visits entries in insertion order."
  (let ((m    (cl-cc/javascript::%js-make-map))
        (seen nil))
    (cl-cc/javascript::%js-map-set m "a" 1)
    (cl-cc/javascript::%js-map-set m "b" 2)
    (cl-cc/javascript::%js-map-set m "c" 3)
    (cl-cc/javascript::%js-map-for-each m
      (lambda (v k &rest _) (declare (ignore _)) (push (cons k v) seen)))
    (assert-equal '(("a" . 1) ("b" . 2) ("c" . 3)) (nreverse seen))))

(deftest js-rt-map-clear
  "Map.clear removes all entries."
  (let ((m (cl-cc/javascript::%js-make-map)))
    (cl-cc/javascript::%js-map-set m "a" 1)
    (cl-cc/javascript::%js-map-clear m)
    (assert-= 0 (cl-cc/javascript::%js-map-size m))))

;;; ─── Generator / yield ───────────────────────────────────────────────────────

(deftest js-rt-generator-basic
  "make-generator collects yield values into an iterable iterator."
  (let* ((gen (cl-cc/javascript::%js-make-generator
               (lambda ()
                 (cl-cc/javascript::%js-yield 10)
                 (cl-cc/javascript::%js-yield 20)
                 (cl-cc/javascript::%js-yield 30))))
         (arr (cl-cc/javascript::%js-iterator-to-array gen)))
    (assert-equal '(10 20 30) (%jr-list arr))))

(deftest js-rt-generator-done-after-exhaust
  "Generator's next returns done=t once all yields are consumed."
  (let* ((gen  (cl-cc/javascript::%js-make-generator
                (lambda () (cl-cc/javascript::%js-yield 1))))
         (r1   (cl-cc/javascript::%js-generator-next gen))
         (r2   (cl-cc/javascript::%js-generator-next gen)))
    (assert-false (cl-cc/javascript::%js-get-prop r1 "done"))
    (assert-true  (cl-cc/javascript::%js-get-prop r2 "done"))))

;;; ─── Object property with falsy value ────────────────────────────────────────

(deftest js-rt-object-prop-false-value
  "get-prop must return nil (CL false) when a key is explicitly set to nil.
Regression: %js-resolve-object-method used (not (eq stored nil)) which treated
nil-valued keys as missing."
  (let ((obj (cl-cc/javascript::%js-make-object "flag" nil)))
    (assert-false (cl-cc/javascript::%js-get-prop obj "flag"))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-get-prop obj "absent"))))

;;; ─── ES2023 Array methods ────────────────────────────────────────────────────

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

(deftest js-rt-array-with
  "with(index, value) returns a copy with one element replaced."
  (let ((result (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) 1 99)))
    (assert-equal '(10 99 30) (%jr-list result))))

(deftest js-rt-array-with-negative-index
  "with(-1, value) replaces the last element."
  (let ((result (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) -1 99)))
    (assert-equal '(10 20 99) (%jr-list result))))

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

;;; ─── Number strict predicates ────────────────────────────────────────────────

(deftest-each js-rt-number-is-nan
  "Number.isNaN only returns true for actual NaN (not coerced NaN)."
  :cases (("nan"     cl-cc/javascript::*js-nan-float* t)
          ("string"  "NaN"                            nil)
          ("number"  42                               nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-number-is-nan val)))

(deftest-each js-rt-number-is-finite
  "Number.isFinite returns true only for finite numbers."
  :cases (("int"      42      t)
          ("float"    3.14d0  t)
          ("nan"      cl-cc/javascript::*js-nan-float*  nil)
          ("inf"      cl-cc/javascript::*js-inf-float*  nil)
          ("string"   "42"                              nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-number-is-finite val)))

(deftest-each js-rt-number-is-integer
  "Number.isInteger returns true for integer values only."
  :cases (("int"    42       t)
          ("float"  3.0d0    t)
          ("frac"   3.5d0   nil)
          ("string" "3"     nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-number-is-integer val)))

;;; ─── parseInt / parseFloat ───────────────────────────────────────────────────

(deftest-each js-rt-parse-int
  "parseInt parses integers from strings in various radixes."
  :cases (("decimal"  "42"    10   42)
          ("hex"      "ff"    16  255)
          ("binary"   "101"    2    5)
          ("junk"     "abc"   10    :nan))
  (str radix expected)
  (let ((result (cl-cc/javascript::%js-parse-int str radix)))
    (if (eq expected :nan)
        (assert-true (cl-cc/javascript::%js-nan-p result))
        (assert-= expected result))))

(deftest-each js-rt-parse-float
  "parseFloat parses the longest leading numeric prefix."
  :cases (("simple"  "3.14"     3.14d0)
          ("prefix"  "3.14abc"  3.14d0)
          ("int"     "42"       42.0d0)
          ("junk"    "abc"      :nan))
  (str expected)
  (let ((result (cl-cc/javascript::%js-parse-float str)))
    (if (eq expected :nan)
        (assert-true (cl-cc/javascript::%js-nan-p result))
        (assert-true (< (abs (- expected result)) 1.0d-10)))))

;;; ─── Reflect helpers ─────────────────────────────────────────────────────────

(deftest js-rt-reflect-get-set
  "Reflect.get and Reflect.set wrap %js-get-prop/%js-set-prop."
  (let ((obj (cl-cc/javascript::%js-make-object "x" 10)))
    (assert-= 10 (cl-cc/javascript::%js-reflect-get obj "x"))
    (cl-cc/javascript::%js-reflect-set obj "x" 42)
    (assert-= 42 (cl-cc/javascript::%js-reflect-get obj "x"))))

(deftest js-rt-reflect-has
  "Reflect.has returns true when key exists, nil otherwise."
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (assert-true  (cl-cc/javascript::%js-reflect-has obj "a"))
    (assert-false (cl-cc/javascript::%js-reflect-has obj "b"))))

(deftest js-rt-reflect-delete-property
  "Reflect.deleteProperty removes a key from the object."
  (let ((obj (cl-cc/javascript::%js-make-object "k" 99)))
    (cl-cc/javascript::%js-reflect-delete-property obj "k")
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-reflect-get obj "k"))))

(deftest js-rt-object-define-property
  "Object.defineProperty sets a value key via descriptor."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 77)))
    (cl-cc/javascript::%js-object-define-property obj "y" desc)
    (assert-= 77 (cl-cc/javascript::%js-reflect-get obj "y"))))

(deftest js-rt-object-get-own-property-descriptor
  "getOwnPropertyDescriptor returns descriptor object for existing key."
  (let* ((obj (cl-cc/javascript::%js-make-object "v" 5))
         (desc (cl-cc/javascript::%js-object-get-own-property-descriptor obj "v")))
    (assert-= 5 (gethash "value" desc))
    (assert-true (gethash "writable" desc))))

;;; ─── Math extended (ES2015+ transcendentals) ─────────────────────────────────

(deftest-each js-rt-math-extended-unary
  "Math hyperbolic and transcendental unaries map numeric inputs correctly."
  :cases (("sinh-0"   #'cl-cc/javascript::%js-math-sinh   0     0.0d0)
          ("cosh-0"   #'cl-cc/javascript::%js-math-cosh   0     1.0d0)
          ("tanh-0"   #'cl-cc/javascript::%js-math-tanh   0     0.0d0)
          ("cbrt-8"   #'cl-cc/javascript::%js-math-cbrt   8     2.0d0)
          ("expm1-0"  #'cl-cc/javascript::%js-math-expm1  0     0.0d0)
          ("log1p-0"  #'cl-cc/javascript::%js-math-log1p  0     0.0d0))
  (fn x expected)
  (assert-true (< (abs (- expected (funcall fn x))) 1.0d-10)))

;;; ─── Map.groupBy ─────────────────────────────────────────────────────────────

(deftest js-rt-map-group-by
  "Map.groupBy groups iterable items by key-fn result."
  (let* ((arr (%jr-arr 1 2 3 4 6))
         (result (cl-cc/javascript::%js-map-group-by
                  arr
                  (lambda (x) (if (evenp x) "even" "odd"))))
         (evens (cl-cc/javascript::%js-map-get result "even"))
         (odds  (cl-cc/javascript::%js-map-get result "odd")))
    (assert-= 3 (length evens))
    (assert-= 2 (length odds))))

;;; ─── Method resolution via get-prop ──────────────────────────────────────────

(deftest js-rt-method-resolution-array
  "Calling a method through get-prop on an array invokes the correct helper."
  (let* ((arr (%jr-arr 10 20 30))
         (join-fn (cl-cc/javascript::%js-get-prop arr "join")))
    (assert-true (functionp join-fn))
    (assert-string= "10,20,30" (funcall join-fn ","))))

(deftest js-rt-method-resolution-string
  "Calling a method through get-prop on a string invokes the correct helper."
  (let* ((s "hello")
         (upper-fn (cl-cc/javascript::%js-get-prop s "toUpperCase")))
    (assert-true (functionp upper-fn))
    (assert-string= "HELLO" (funcall upper-fn))))

(deftest js-rt-method-resolution-length
  "Accessing .length on an array returns the numeric length."
  (let* ((arr (%jr-arr 1 2 3)))
    (assert-= 3 (cl-cc/javascript::%js-get-prop arr "length"))))
