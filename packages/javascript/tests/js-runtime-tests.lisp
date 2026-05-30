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
