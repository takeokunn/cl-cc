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

(deftest-each js-rt-string-starts-ends-with
  "startsWith and endsWith check prefix/suffix."
  :cases (("starts-t"  #'cl-cc/javascript::%js-string-starts-with "hello" "hel" t)
          ("starts-f"  #'cl-cc/javascript::%js-string-starts-with "hello" "ell" nil)
          ("ends-t"    #'cl-cc/javascript::%js-string-ends-with   "hello" "llo" t)
          ("ends-f"    #'cl-cc/javascript::%js-string-ends-with   "hello" "hel" nil))
  (fn s sub expected)
  (assert-equal expected (funcall fn s sub)))

(deftest-each js-rt-string-pad
  "padStart and padEnd extend string to the given length."
  :cases (("pad-start" #'cl-cc/javascript::%js-string-pad-start "5"  3 "005")
          ("pad-end"   #'cl-cc/javascript::%js-string-pad-end   "5"  3 "500"))
  (fn s len expected)
  (assert-string= expected (funcall fn s len "0")))

(deftest js-rt-string-trim
  "trim removes leading and trailing whitespace."
  (assert-string= "hello" (cl-cc/javascript::%js-string-trim "  hello  "))
  (assert-string= "hello  " (cl-cc/javascript::%js-string-trim-start "  hello  "))
  (assert-string= "  hello" (cl-cc/javascript::%js-string-trim-end "  hello  ")))

(deftest-each js-rt-string-at
  "String.prototype.at supports positive and negative indices."
  :cases (("first"  0  "h")
          ("last"  -1  "o")
          ("mid"    2  "l"))
  (idx expected)
  (assert-string= expected (cl-cc/javascript::%js-string-at "hello" idx)))

(deftest js-rt-string-char-at-code-at
  "charAt and charCodeAt return character and its code."
  (assert-string= "e" (cl-cc/javascript::%js-string-char-at "hello" 1))
  (assert-=      101  (cl-cc/javascript::%js-string-char-code-at "hello" 1)))

(deftest-each js-rt-string-replace
  "replace substitutes first occurrence; replaceAll substitutes all."
  :cases (("first"  #'cl-cc/javascript::%js-string-replace     "aaa" "a" "b"  "baa")
          ("all"    #'cl-cc/javascript::%js-string-replace-all  "aaa" "a" "b"  "bbb"))
  (fn s pat rep expected)
  (assert-string= expected (funcall fn s pat rep)))

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

(deftest-each js-rt-shift-ops-full
  "Shift operators: <<, >> (arithmetic), and >>> (unsigned, always ≥0)."
  :cases (("shl-2"    #'cl-cc/javascript::%js-shift-left           2  2       8)
          ("shl-1"    #'cl-cc/javascript::%js-shift-left           1  4      16)
          ("shl-0"    #'cl-cc/javascript::%js-shift-left           5  0       5)
          ("shr-pos"  #'cl-cc/javascript::%js-shift-right          8  2       2)
          ("shr-neg"  #'cl-cc/javascript::%js-shift-right         -8  2      -2)
          ("ushr-neg" #'cl-cc/javascript::%js-unsigned-shift-right -8  2 1073741822)
          ("ushr-pos" #'cl-cc/javascript::%js-unsigned-shift-right  8  2       2))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

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

;;; ─── URI encoding / base64 ───────────────────────────────────────────────────

(deftest-each js-rt-encode-uri-component
  "encodeURIComponent encodes special chars; spaces become %20."
  :cases (("space"   "hello world"  "hello%20world")
          ("slash"   "a/b"          "a%2Fb")
          ("plain"   "abc123"       "abc123"))
  (s expected)
  (assert-string= expected (cl-cc/javascript::%js-encode-uri-component s)))

(deftest js-rt-decode-uri-component
  "decodeURIComponent undoes percent-encoding."
  (assert-string= "hello world" (cl-cc/javascript::%js-decode-uri-component "hello%20world")))

(deftest js-rt-btoa-atob-roundtrip
  "btoa and atob form a roundtrip encoding."
  (let* ((s "Hello, World!")
         (encoded (cl-cc/javascript::%js-btoa s))
         (decoded (cl-cc/javascript::%js-atob encoded)))
    (assert-string= s decoded)))

;;; ─── RegExp public API ───────────────────────────────────────────────────────

(deftest js-rt-regex-test-match
  "regex-test returns t when the pattern matches."
  (let ((re (cl-cc/javascript::%js-make-regex "hello")))
    (assert-true  (cl-cc/javascript::%js-regex-test re "say hello world"))
    (assert-false (cl-cc/javascript::%js-regex-test re "goodbye"))))

(deftest js-rt-regex-exec-returns-match
  "regex-exec returns match info object with index and '0' key."
  (let* ((re (cl-cc/javascript::%js-make-regex "lo"))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (assert-false (eq m cl-cc/javascript::+js-null+))
    (assert-string= "lo" (gethash "0" m))
    (assert-= 3 (truncate (gethash "index" m)))))

(deftest js-rt-regex-exec-no-match
  "regex-exec returns null when there is no match."
  (let* ((re (cl-cc/javascript::%js-make-regex "xyz"))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (assert-eq cl-cc/javascript::+js-null+ m)))

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

;;; ─── Number format helpers ───────────────────────────────────────────────────

(deftest-each js-rt-strip-trailing-dot
  "strip-trailing-dot removes a trailing '.' from CL's ~,0F output."
  :cases (("with-dot"    "8."   "8")
          ("no-dot"      "3.14" "3.14")
          ("empty"       ""     "")
          ("only-dot"    "."    ""))
  (input expected)
  (assert-string= expected (cl-cc/javascript::%js-strip-trailing-dot input)))

(deftest-each js-rt-strip-pre-exp-dot
  "strip-pre-exp-dot removes '.' immediately before the exponent marker."
  :cases (("dot-e"     "1.e+2" "1e+2")
          ("dot-E"     "2.E+3" "2E+3")
          ("no-dot"    "1.2e+3" "1.2e+3")
          ("no-exp"    "3.14"   "3.14"))
  (input expected)
  (assert-string= expected (cl-cc/javascript::%js-strip-pre-exp-dot input)))

(deftest js-rt-bound-method-found
  "bound-method returns a closure that prepends the receiver."
  (let* ((table (list (cons "double" (lambda (n) (* 2 n)))))
         (bound (cl-cc/javascript::%js-bound-method table 5 "double")))
    (assert-true (functionp bound))
    (assert-= 10 (funcall bound))))

(deftest js-rt-bound-method-not-found
  "bound-method returns +js-undefined+ when the method is not in the table."
  (let* ((table (list (cons "existing" #'identity)))
         (result (cl-cc/javascript::%js-bound-method table 5 "missing")))
    (assert-eq cl-cc/javascript::+js-undefined+ result)))

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

;;; ─── BigInt operations ───────────────────────────────────────────────────────

(deftest-each js-rt-bigint-val
  "%js-bigint-val extracts the integer value from a BigInt struct or coerces a
plain number via truncate — the key invariant behind define-js-bigint-binop."
  :cases (("bigint-pos" 42   42)
          ("bigint-neg" -7  -7)
          ("plain-int"  10   10)
          ("float"      3    3))
  (raw expected)
  (let ((bi (cl-cc/javascript::%make-js-bigint raw)))
    (assert-= expected (cl-cc/javascript::%js-bigint-val bi))))

(deftest-each js-rt-bigint-arithmetic
  "BigInt binary ops generated by define-js-bigint-binop."
  :cases (("add"     #'cl-cc/javascript::%js-bigint-add  3  4   7)
          ("sub"     #'cl-cc/javascript::%js-bigint-sub  9  4   5)
          ("mul"     #'cl-cc/javascript::%js-bigint-mul  3  4  12)
          ("pow"     #'cl-cc/javascript::%js-bigint-pow  2  8 256)
          ("band"    #'cl-cc/javascript::%js-bigint-bitwise-and  #b1010 #b1100 #b1000)
          ("bor"     #'cl-cc/javascript::%js-bigint-bitwise-or   #b1010 #b1100 #b1110)
          ("bxor"    #'cl-cc/javascript::%js-bigint-bitwise-xor  #b1010 #b1100 #b0110))
  (fn a b expected)
  (let ((result (funcall fn (cl-cc/javascript::%make-js-bigint a)
                            (cl-cc/javascript::%make-js-bigint b))))
    (assert-= expected (cl-cc/javascript::js-bigint-value result))))

(deftest-each js-rt-bigint-as-int-n-uint-n
  "BigInt.asIntN and asUintN mask to the given width."
  :cases (("int-n-positive"  #'cl-cc/javascript::%js-bigint-as-int-n  8  127  127)
          ("int-n-wrap"      #'cl-cc/javascript::%js-bigint-as-int-n  8  128 -128)
          ("uint-n"          #'cl-cc/javascript::%js-bigint-as-uint-n 8  300   44))
  (fn width val expected)
  (let ((result (funcall fn width (cl-cc/javascript::%make-js-bigint val))))
    (assert-= expected (cl-cc/javascript::js-bigint-value result))))

;;; ─── for-in accessor-key filtering ──────────────────────────────────────────

(deftest js-rt-for-in-skips-accessor-keys
  "for-in now uses %js-internal-key-p, which also filters __get_X and __set_X
accessor slots (regression: the old inline check only matched __X__ form)."
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1 "b" 2))
        (keys nil))
    (setf (gethash "__get_foo" obj) (lambda () 99))
    (setf (gethash "__set_foo" obj) (lambda (v) v))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-for-in
     obj (lambda (k &rest _) (declare (ignore _)) (push k keys)))
    (assert-equal '("a" "b") (sort keys #'string<))))

;;; ─── Type resolver coverage (define-js-type-resolver) ────────────────────────

(deftest-each js-rt-resolve-regexp-props
  "Regexp resolver returns struct fields and bound-method closures."
  :cases (("source"      "source"     "hello")
          ("flags"       "flags"      "")
          ("global"      "global"     nil))
  (key expected)
  (let* ((re (cl-cc/javascript::%js-make-regex "hello" ""))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (assert-equal expected val)))

(deftest js-rt-resolve-regexp-test-method
  "Regexp 'test' property resolves to a closure that tests the pattern."
  (let* ((re  (cl-cc/javascript::%js-make-regex "hi" ""))
         (fn  (cl-cc/javascript::%js-resolve-regexp-method re "test")))
    (assert-true  (funcall fn "say hi there"))
    (assert-false (funcall fn "goodbye"))))

(deftest-each js-rt-resolve-typed-array-props
  "TypedArray resolver returns numeric properties from struct slots."
  :cases (("length"      "length"     3)
          ("byteLength"  "byteLength" 12)
          ("byteOffset"  "byteOffset" 0))
  (key expected)
  (let* ((ta  (cl-cc/javascript::%js-make-typed-array "Int32Array" 3))
         (val (cl-cc/javascript::%js-resolve-typed-array-method ta key)))
    (assert-= expected val)))

(deftest-each js-rt-resolve-bigint-methods
  "BigInt resolver returns method closures for toString/valueOf/toLocaleString."
  :cases (("toString"       "toString"       "42")
          ("toLocaleString" "toLocaleString" "42"))
  (key expected)
  (let* ((bi  (cl-cc/javascript::%make-js-bigint 42))
         (fn  (cl-cc/javascript::%js-resolve-bigint-method bi key))
         (result (funcall fn cl-cc/javascript::+js-undefined+)))
    (assert-string= expected result)))

(deftest js-rt-resolve-bigint-value-of
  "BigInt valueOf returns the BigInt struct itself."
  (let* ((bi  (cl-cc/javascript::%make-js-bigint 7))
         (fn  (cl-cc/javascript::%js-resolve-bigint-method bi "valueOf")))
    (assert-eq bi (funcall fn))))

(deftest-each js-rt-get-own-property-descriptors-filters-internals
  "getOwnPropertyDescriptors excludes __proto__, __get_X, __set_X, __class__ etc."
  :cases (("visible"   "real"    t)
          ("proto"     "__proto__" nil)
          ("getter"    "__get_x"   nil)
          ("setter"    "__set_x"   nil))
  (key should-appear)
  (let* ((obj (cl-cc/javascript::%js-make-object "real" 1)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+
          (gethash "__get_x"   obj) (lambda () 0)
          (gethash "__set_x"   obj) (lambda (v) v))
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (found (nth-value 1 (gethash key descs))))
      (assert-equal should-appear found))))

;;; ─── Temporal helper functions ───────────────────────────────────────────────

(deftest-each js-rt-temporal-pad
  "%temporal-pad zero-pads integers to the specified width."
  :cases (("year-4"   2025 4 "2025")
          ("month-2"  3    2 "03")
          ("day-2"    15   2 "15")
          ("narrow"   9    1 "9"))
  (n width expected)
  (assert-string= expected (cl-cc/javascript::%temporal-pad n width)))

(deftest-each js-rt-temporal-3way-compare
  "%temporal-3way-compare returns -1/0/1 for ordered numeric comparison."
  :cases (("less"    1 2 -1.0d0)
          ("equal"   5 5  0.0d0)
          ("greater" 9 3  1.0d0))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%temporal-3way-compare a b)))

(deftest-each js-rt-temporal-parse-iso-fields
  "%temporal-parse-iso-fields decomposes an ISO-8601 datetime string."
  :cases (("full"   "2025-06-13T14:30:00" 2025 6 13 14 30 0)
          ("date-only" "2025-01-01"        2025 1  1  0  0 0))
  (s exp-y exp-mo exp-d exp-h exp-mi exp-s)
  (multiple-value-bind (y mo d h mi s) (cl-cc/javascript::%temporal-parse-iso-fields s)
    (assert-= exp-y  y)
    (assert-= exp-mo mo)
    (assert-= exp-d  d)
    (assert-= exp-h  h)
    (assert-= exp-mi mi)
    (assert-= exp-s  s)))

(deftest-each js-rt-temporal-duration-to-seconds
  "%temporal-duration-to-seconds converts duration hash-tables to total seconds."
  :cases (("one-hour"   "hours"   1 3600)
          ("one-minute" "minutes" 1 60)
          ("one-second" "seconds" 1 1)
          ("one-day"    "days"    1 86400))
  (unit n expected)
  (let ((dur (cl-cc/javascript::%js-make-object unit (coerce n 'double-float))))
    (assert-= expected (cl-cc/javascript::%temporal-duration-to-seconds dur))))

(deftest js-rt-temporal-parse-time-fields
  "%temporal-parse-time-fields decomposes an HH:MM:SS string."
  (multiple-value-bind (h m s) (cl-cc/javascript::%temporal-parse-time-fields "14:30:05")
    (assert-= 14 h)
    (assert-= 30 m)
    (assert-=  5 s)))

;;; -----------------------------------------------------------------------
;;;  js-date tests
;;; -----------------------------------------------------------------------

(deftest js-rt-date-now
  "Date.now() returns a positive integer (milliseconds since Unix epoch)."
  (let ((t1 (cl-cc/javascript::%js-date-now))
        (t2 (cl-cc/javascript::%js-date-now)))
    (assert-true (integerp t1))
    (assert-true (>= t2 t1))))

(deftest js-rt-date-make-date-no-args
  "%js-make-date with no args returns a js-date struct."
  (let ((d (cl-cc/javascript::%js-make-date)))
    (assert-true (cl-cc/javascript::js-date-p d))
    (assert-true (integerp (cl-cc/javascript::js-date-ms d)))))

(deftest js-rt-date-make-date-from-ms
  "%js-make-date from a millisecond value stores the ms directly."
  (let ((d (cl-cc/javascript::%js-make-date 1000000.0d0)))
    (assert-= 1000000 (cl-cc/javascript::js-date-ms d))))

(deftest js-rt-date-make-date-copy
  "%js-make-date from another Date copies the ms."
  (let* ((orig (cl-cc/javascript::%js-make-date 42000.0d0))
         (copy (cl-cc/javascript::%js-make-date orig)))
    (assert-= 42000 (cl-cc/javascript::js-date-ms copy))))

(deftest js-rt-date-parse-string-date-only
  "%js-date-parse-string parses YYYY-MM-DD to ms."
  (let ((ms (cl-cc/javascript::%js-date-parse-string "1970-01-01")))
    (assert-= 0 ms)))

(deftest js-rt-date-parse-string-datetime
  "%js-date-parse-string parses YYYY-MM-DDTHH:MM:SS."
  (let ((ms (cl-cc/javascript::%js-date-parse-string "1970-01-01T01:00:00")))
    (assert-= 3600000 ms)))

(deftest js-rt-date-parse-string-error
  "%js-date-parse-string falls back to now on invalid input."
  (let ((result (cl-cc/javascript::%js-date-parse-string "not-a-date")))
    (assert-true (integerp result))))

;;; Date.prototype getters: 97445000 ms = 1970-01-02T03:04:05.000Z
(deftest js-rt-date-getters
  "Date.prototype getters return correct decomposed fields for a fixed epoch."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-= 1970 (cl-cc/javascript::%js-date-get-full-year d))
    (assert-= 1970 (cl-cc/javascript::%js-date-get-utc-full-year d))
    (assert-= 0    (cl-cc/javascript::%js-date-get-month d))      ; January = 0
    (assert-= 2    (cl-cc/javascript::%js-date-get-date d))
    (assert-= 3    (cl-cc/javascript::%js-date-get-hours d))
    (assert-= 4    (cl-cc/javascript::%js-date-get-minutes d))
    (assert-= 5    (cl-cc/javascript::%js-date-get-seconds d))
    (assert-= 0    (cl-cc/javascript::%js-date-get-milliseconds d))))

(deftest js-rt-date-get-time
  "Date.prototype.getTime returns ms as double-float."
  (let ((d (cl-cc/javascript::%js-make-date 12345)))
    (assert-= 12345.0d0 (cl-cc/javascript::%js-date-get-time d))))

(deftest js-rt-date-to-iso-string
  "toISOString formats as YYYY-MM-DDTHH:MM:SS.mmmZ."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "1970-01-02T03:04:05.000Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

(deftest js-rt-date-to-iso-string-with-ms
  "toISOString includes sub-second milliseconds."
  (let ((d (cl-cc/javascript::%js-make-date 97445123)))
    (assert-string= "1970-01-02T03:04:05.123Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

(deftest js-rt-date-to-local-date-string
  "toLocaleDateString returns YYYY/MM/DD."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "1970/01/02" (cl-cc/javascript::%js-date-to-local-date-string d))))

(deftest js-rt-date-to-time-string
  "toTimeString returns HH:MM:SS GMT+0000 (...)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "03:04:05 GMT+0000 (Coordinated Universal Time)"
                    (cl-cc/javascript::%js-date-to-time-string d))))

(deftest js-rt-date-set-time
  "setTime updates ms and returns the new value."
  (let ((d (cl-cc/javascript::%js-make-date 0)))
    (cl-cc/javascript::%js-date-set-time d 5000)
    (assert-= 5000 (cl-cc/javascript::js-date-ms d))))

(deftest js-rt-date-set-full-year-preserves-time
  "setFullYear preserves the existing time components (was bug: zeroed them)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; 1970-01-02T03:04:05Z
    (cl-cc/javascript::%js-date-set-full-year d 2024.0d0)
    (assert-= 3 (cl-cc/javascript::%js-date-get-hours d))
    (assert-= 4 (cl-cc/javascript::%js-date-get-minutes d))
    (assert-= 5 (cl-cc/javascript::%js-date-get-seconds d))))

(deftest js-rt-date-set-month
  "setMonth changes the month (JS 0-based)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; January
    (cl-cc/javascript::%js-date-set-month d 5.0d0)       ; June (0-based)
    (assert-= 5 (cl-cc/javascript::%js-date-get-month d))))

(deftest js-rt-date-set-date
  "setDate changes the day of month."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; day 2
    (cl-cc/javascript::%js-date-set-date d 15.0d0)
    (assert-= 15 (cl-cc/javascript::%js-date-get-date d))))

(deftest js-rt-date-rebuild-preserves-ms
  "%js-date-rebuild preserves sub-second milliseconds."
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))  ; .999 ms
    (cl-cc/javascript::%js-date-rebuild d :sec 10)
    (assert-= 999 (cl-cc/javascript::%js-date-get-milliseconds d))))

;;; -----------------------------------------------------------------------
;;;  JSON stringify tests
;;; -----------------------------------------------------------------------

(deftest js-rt-json-stringify-primitives
  "JSON.stringify handles JS primitive values."
  (assert-string= "null"   (cl-cc/javascript::%js-json-stringify cl-cc/javascript::+js-null+))
  (assert-string= "null"   (cl-cc/javascript::%js-json-stringify cl-cc/javascript::+js-undefined+))
  (assert-string= "true"   (cl-cc/javascript::%js-json-stringify t))
  (assert-string= "false"  (cl-cc/javascript::%js-json-stringify nil))
  (assert-string= "42"     (cl-cc/javascript::%js-json-stringify 42.0d0))
  (assert-string= "1.5"    (cl-cc/javascript::%js-json-stringify 1.5d0))
  (assert-string= "\"hello\"" (cl-cc/javascript::%js-json-stringify "hello"))
  (assert-string= "null"   (cl-cc/javascript::%js-json-stringify cl-cc/javascript::*js-nan-float*)))

(deftest js-rt-json-stringify-string-escapes
  "JSON.stringify escapes special characters in strings."
  (assert-string= "\"line1\\nline2\"" (cl-cc/javascript::%js-json-stringify "line1
line2"))
  (assert-string= "\"a\\tb\"" (cl-cc/javascript::%js-json-stringify "a	b"))
  (assert-string= "\"say \\\"hi\\\"\"" (cl-cc/javascript::%js-json-stringify "say \"hi\"")))

(deftest js-rt-json-stringify-array
  "JSON.stringify serializes JS arrays."
  (let ((arr (cl-cc/javascript::%js-make-array 1.0d0 2.0d0 3.0d0)))
    (assert-string= "[1,2,3]" (cl-cc/javascript::%js-json-stringify arr))))

(deftest js-rt-json-stringify-object
  "JSON.stringify serializes JS objects."
  (let ((obj (cl-cc/javascript::%js-make-object "x" 1.0d0 "y" 2.0d0)))
    (let ((result (cl-cc/javascript::%js-json-stringify obj)))
      (assert-true (cl-cc/javascript::%js-string-includes result "\"x\":1"))
      (assert-true (cl-cc/javascript::%js-string-includes result "\"y\":2")))))

(deftest js-rt-json-stringify-nested
  "JSON.stringify handles nested objects and arrays."
  (let* ((inner (cl-cc/javascript::%js-make-object "a" 1.0d0))
         (arr   (cl-cc/javascript::%js-make-array inner)))
    (let ((result (cl-cc/javascript::%js-json-stringify arr)))
      (assert-true (cl-cc/javascript::%js-string-includes result "{"))
      (assert-true (cl-cc/javascript::%js-string-includes result "\"a\":1")))))

;;; -----------------------------------------------------------------------
;;;  JSON parse tests
;;; -----------------------------------------------------------------------

(deftest-each js-rt-json-parse-non-undefined
  "JSON.parse returns non-undefined for valid JSON literals."
  :cases (("null-lit"  "null")
          ("true-lit"  "true")
          ("false-lit" "false")
          ("number-42" "42")
          ("str-hello" "\"hello\""))
  (input)
  (let ((result (cl-cc/javascript::%js-json-parse input)))
    (assert-true (not (eq result cl-cc/javascript::+js-undefined+)))))

(deftest js-rt-json-parse-null
  "JSON.parse(\"null\") returns the JS null sentinel."
  (assert-true (eq cl-cc/javascript::+js-null+ (cl-cc/javascript::%js-json-parse "null"))))

(deftest js-rt-json-parse-booleans
  "JSON.parse handles true and false."
  (assert-true  (eq t   (cl-cc/javascript::%js-json-parse "true")))
  (assert-true  (eq nil (cl-cc/javascript::%js-json-parse "false"))))

(deftest js-rt-json-parse-number
  "JSON.parse converts numeric strings to double-float."
  (assert-= 42.0d0   (cl-cc/javascript::%js-json-parse "42"))
  (assert-= 3.14d0   (cl-cc/javascript::%js-json-parse "3.14"))
  (assert-= -1.0d0   (cl-cc/javascript::%js-json-parse "-1")))

(deftest js-rt-json-parse-string
  "JSON.parse converts quoted strings."
  (assert-string= "hello" (cl-cc/javascript::%js-json-parse "\"hello\""))
  (assert-string= "a
b" (cl-cc/javascript::%js-json-parse "\"a\\nb\"")))

(deftest js-rt-json-parse-array
  "JSON.parse builds an adjustable vector for arrays."
  (let ((arr (cl-cc/javascript::%js-json-parse "[1,2,3]")))
    (assert-true (cl-cc/javascript::%js-vec-p arr))
    (assert-= 3 (length arr))
    (assert-= 1.0d0 (aref arr 0))))

(deftest js-rt-json-parse-object
  "JSON.parse builds a hash-table for objects."
  (let ((obj (cl-cc/javascript::%js-json-parse "{\"x\":1,\"y\":2}")))
    (assert-true (cl-cc/javascript::%js-ht-p obj))
    (assert-= 1.0d0 (gethash "x" obj))
    (assert-= 2.0d0 (gethash "y" obj))))

(deftest js-rt-json-parse-nested
  "JSON.parse handles nested structures."
  (let ((obj (cl-cc/javascript::%js-json-parse "{\"arr\":[1,2]}")))
    (let ((arr (gethash "arr" obj)))
      (assert-true (cl-cc/javascript::%js-vec-p arr))
      (assert-= 2 (length arr)))))

(deftest js-rt-json-parse-whitespace
  "JSON.parse skips leading/trailing whitespace."
  (assert-= 42.0d0 (cl-cc/javascript::%js-json-parse "  42  "))
  (assert-string= "x" (cl-cc/javascript::%js-json-parse "  \"x\"  ")))

(deftest js-rt-json-parse-invalid
  "JSON.parse returns +js-undefined+ on invalid input."
  (assert-true (eq cl-cc/javascript::+js-undefined+ (cl-cc/javascript::%js-json-parse "NOT_JSON"))))

(deftest js-rt-json-roundtrip
  "stringify then parse round-trips a JS object."
  (let* ((orig  (cl-cc/javascript::%js-make-object "name" "Alice" "age" 30.0d0))
         (json  (cl-cc/javascript::%js-json-stringify orig))
         (reparsed (cl-cc/javascript::%js-json-parse json)))
    (assert-string= "Alice" (gethash "name" reparsed))
    (assert-= 30.0d0 (gethash "age" reparsed))))

;;; -----------------------------------------------------------------------
;;;  Method resolver — new helpers and object fallback table
;;; -----------------------------------------------------------------------

(deftest js-rt-string-char-iter-yields-chars
  "%js-string-char-iter returns an iterator that yields each char as a 1-char string."
  (let* ((iter  (cl-cc/javascript::%js-string-char-iter "abc"))
         (next  (gethash "next" iter))
         (r1    (funcall next))
         (r2    (funcall next))
         (r3    (funcall next))
         (done  (funcall next)))
    (assert-string= "a"   (gethash "value" r1))
    (assert-string= "b"   (gethash "value" r2))
    (assert-string= "c"   (gethash "value" r3))
    (assert-true           (gethash "done"  done))))

(deftest js-rt-string-char-iter-via-get-prop
  "String @@iterator method resolves to an iterator using %js-string-char-iter."
  (let* ((fn    (cl-cc/javascript::%js-get-prop "xy" "@@iterator"))
         (iter  (funcall fn))
         (next  (gethash "next" iter))
         (r1    (funcall next))
         (r2    (funcall next))
         (done  (funcall next)))
    (assert-string= "x"   (gethash "value" r1))
    (assert-string= "y"   (gethash "value" r2))
    (assert-true           (gethash "done"  done))))

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

(deftest-each js-rt-object-fallback-methods
  "Object fallback methods are available via %js-resolve-object-method."
  :cases (("has-own-existing"  "hasOwnProperty"    "x"   t)
          ("has-own-missing"   "hasOwnProperty"    "z"   nil)
          ("prop-is-enum-yes"  "propertyIsEnumerable" "x" t)
          ("prop-is-enum-no"   "propertyIsEnumerable" "z" nil))
  (method key expected)
  (let* ((obj (cl-cc/javascript::%js-make-object "x" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj method)))
    (assert-true  (functionp fn))
    (assert-equal expected (funcall fn key))))

(deftest js-rt-object-fallback-to-string
  "Object fallback toString returns \"[object Object]\"."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "toString")))
    (assert-string= "[object Object]" (funcall fn))))

(deftest js-rt-object-fallback-value-of
  "Object fallback valueOf returns the object itself."
  (let* ((obj (cl-cc/javascript::%js-make-object "k" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "valueOf")))
    (assert-eq obj (funcall fn))))

(deftest js-rt-object-fallback-constructor
  "Object fallback constructor returns the object itself."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "constructor")))
    (assert-eq obj (funcall fn))))

(deftest js-rt-object-fallback-stored-wins
  "When obj has a stored key the stored value is returned, not the fallback."
  (let* ((obj (cl-cc/javascript::%js-make-object "toString" "custom"))
         (result (cl-cc/javascript::%js-resolve-object-method obj "toString")))
    (assert-string= "custom" result)))

(deftest js-rt-object-fallback-unknown-returns-undefined
  "Unknown method names return +js-undefined+."
  (let* ((obj    (cl-cc/javascript::%js-make-object))
         (result (cl-cc/javascript::%js-resolve-object-method obj "nonExistent")))
    (assert-eq cl-cc/javascript::+js-undefined+ result)))

;;; -----------------------------------------------------------------------
;;;  Promise.any / Promise.withResolvers
;;; -----------------------------------------------------------------------

(deftest js-rt-promise-any-first-fulfilled
  "Promise.any resolves with the first fulfilled promise."
  (let* ((p1 (cl-cc/javascript::%js-promise-reject "e1"))
         (p2 (cl-cc/javascript::%js-promise-resolve 42))
         (arr (%jr-arr p1 p2))
         (r   (cl-cc/javascript::%js-promise-any arr)))
    (assert-false (cl-cc/javascript::js-promise-rejected-p r))
    (assert-= 42  (cl-cc/javascript::js-promise-value r))))

(deftest js-rt-promise-any-all-rejected
  "Promise.any rejects when all promises reject."
  (let* ((p1 (cl-cc/javascript::%js-promise-reject "e1"))
         (p2 (cl-cc/javascript::%js-promise-reject "e2"))
         (arr (%jr-arr p1 p2))
         (r   (cl-cc/javascript::%js-promise-any arr)))
    (assert-true (cl-cc/javascript::js-promise-rejected-p r))))

(deftest js-rt-promise-with-resolvers
  "Promise.withResolvers returns an object with promise/resolve/reject."
  (let* ((trio    (cl-cc/javascript::%js-promise-with-resolvers))
         (promise (gethash "promise" trio))
         (resolve (gethash "resolve" trio))
         (reject  (gethash "reject"  trio)))
    (assert-true (cl-cc/javascript::js-promise-p promise))
    (assert-true (functionp resolve))
    (assert-true (functionp reject))
    (funcall resolve 99)
    (assert-= 99 (cl-cc/javascript::js-promise-value promise))))

;;; -----------------------------------------------------------------------
;;;  Reflect.apply / Reflect.construct
;;; -----------------------------------------------------------------------

(deftest js-rt-reflect-apply
  "Reflect.apply invokes fn with spread args."
  (let* ((fn     (lambda (a b) (+ a b)))
         (result (cl-cc/javascript::%js-reflect-apply fn nil (%jr-arr 3 4))))
    (assert-= 7 result)))

(deftest js-rt-reflect-construct
  "Reflect.construct invokes a constructor function with an args vector."
  (let* ((ctor  (lambda (&rest args) (first args)))
         (args  (%jr-arr 77))
         (obj   (cl-cc/javascript::%js-reflect-construct ctor args)))
    (assert-= 77 obj)))

;;; -----------------------------------------------------------------------
;;;  AggregateError / WeakRef / RegExp.escape
;;; -----------------------------------------------------------------------

(deftest js-rt-aggregate-error-make
  "%js-make-aggregate-error creates an object with message and errors."
  (let* ((errs (%jr-arr "e1" "e2"))
         (obj  (cl-cc/javascript::%js-make-aggregate-error errs "some errors")))
    (assert-string= "some errors"   (gethash "message" obj))
    (assert-string= "AggregateError" (gethash "name"   obj))
    (assert-eq errs                  (gethash "errors" obj))))

(deftest js-rt-weak-ref-make-deref
  "%js-make-weak-ref / %js-weak-ref-deref round-trips the target."
  (let* ((target  (cl-cc/javascript::%js-make-object "k" 1))
         (wr      (cl-cc/javascript::%js-make-weak-ref target))
         (dereffed (cl-cc/javascript::%js-weak-ref-deref wr)))
    (assert-eq target dereffed)))

(deftest js-rt-regexp-escape
  "%js-regexp-escape escapes regex-special chars in a string."
  (let ((result (cl-cc/javascript::%js-regexp-escape "a.b+c?")))
    (assert-string= "a\\.b\\+c\\?" result)))
