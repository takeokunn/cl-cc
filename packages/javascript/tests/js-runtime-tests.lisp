;;;; packages/javascript/tests/js-runtime-tests.lisp — ES2026 JS runtime builtin tests
;;;;
;;;; Behavior tests for the %js-* runtime helpers (Array, String, Math, Object,
;;;; type-coercion families) that lowered JS code calls. These exercise the
;;;; functions directly (the parser/e2e tests only check AST shape), covering the
;;;; largest previously-untested surface of the JS frontend.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────────

(defun %jr-arr (&rest els)
  "Build a JS array (adjustable vector) from ELS."
  (apply #'cl-cc/javascript::%js-make-array els))

(defun %jr-list (vec)
  "Coerce a JS array (vector) to a CL list for easy comparison."
  (coerce vec 'list))

;;; ─── typeof ─────────────────────────────────────────────────────────────────

(deftest js-rt-typeof-number
  "typeof a number is \"number\"."
  (assert-string= "number" (cl-cc/javascript::%js-typeof 42)))

(deftest js-rt-typeof-string
  "typeof a string is \"string\"."
  (assert-string= "string" (cl-cc/javascript::%js-typeof "hi")))

(deftest js-rt-typeof-boolean
  "typeof t/nil is \"boolean\"."
  (assert-string= "boolean" (cl-cc/javascript::%js-typeof t))
  (assert-string= "boolean" (cl-cc/javascript::%js-typeof nil)))

(deftest js-rt-typeof-undefined
  "typeof undefined is \"undefined\"."
  (assert-string= "undefined" (cl-cc/javascript::%js-typeof cl-cc/javascript::+js-undefined+)))

(deftest js-rt-typeof-null-is-object
  "typeof null is \"object\" (historic quirk)."
  (assert-string= "object" (cl-cc/javascript::%js-typeof cl-cc/javascript::+js-null+)))

(deftest js-rt-typeof-array-is-object
  "typeof an array is \"object\"."
  (assert-string= "object" (cl-cc/javascript::%js-typeof (%jr-arr 1 2))))

;;; ─── ToString ─────────────────────────────────────────────────────────────────

(deftest js-rt-to-string-integer
  "ToString(42) is \"42\"."
  (assert-string= "42" (cl-cc/javascript::%js-to-string 42)))

(deftest js-rt-to-string-booleans
  "ToString(t)/(nil) are \"true\"/\"false\"."
  (assert-string= "true"  (cl-cc/javascript::%js-to-string t))
  (assert-string= "false" (cl-cc/javascript::%js-to-string nil)))

(deftest js-rt-to-string-null-undefined
  "ToString(null)/(undefined)."
  (assert-string= "null"      (cl-cc/javascript::%js-to-string cl-cc/javascript::+js-null+))
  (assert-string= "undefined" (cl-cc/javascript::%js-to-string cl-cc/javascript::+js-undefined+)))

(deftest js-rt-to-string-nan
  "ToString(:js-nan) is \"NaN\"."
  (assert-string= "NaN" (cl-cc/javascript::%js-to-string :js-nan)))

;;; ─── Equality / truthiness ────────────────────────────────────────────────────

(deftest js-rt-strict-eq-numbers
  "=== on equal numbers is true, unequal false."
  (assert-true  (cl-cc/javascript::%js-strict-eq 3 3))
  (assert-false (cl-cc/javascript::%js-strict-eq 3 4)))

(deftest js-rt-strict-eq-strings
  "=== on equal strings is true."
  (assert-true (cl-cc/javascript::%js-strict-eq "a" "a")))

(deftest js-rt-truthy
  "Truthiness: nonzero/nonempty true; 0/\"\"/nil/undefined/null false."
  (assert-true  (cl-cc/javascript::%js-truthy 1))
  (assert-true  (cl-cc/javascript::%js-truthy "x"))
  (assert-false (cl-cc/javascript::%js-truthy 0))
  (assert-false (cl-cc/javascript::%js-truthy ""))
  (assert-false (cl-cc/javascript::%js-truthy nil))
  (assert-false (cl-cc/javascript::%js-truthy cl-cc/javascript::+js-undefined+))
  (assert-false (cl-cc/javascript::%js-truthy cl-cc/javascript::+js-null+)))

;;; ─── Array ─────────────────────────────────────────────────────────────────────

(deftest js-rt-array-make-and-index
  "make-array stores elements at integer indices."
  (let ((a (%jr-arr 10 20 30)))
    (assert-= 3 (length a))
    (assert-= 10 (aref a 0))
    (assert-= 30 (aref a 2))))

(deftest js-rt-array-push-returns-length
  "push appends and returns the new length."
  (let ((a (%jr-arr 1 2)))
    (assert-= 3 (cl-cc/javascript::%js-array-push a 3))
    (assert-= 3 (aref a 2))))

(deftest js-rt-array-pop
  "pop removes and returns the last element."
  (let ((a (%jr-arr 1 2 3)))
    (assert-= 3 (cl-cc/javascript::%js-array-pop a))
    (assert-= 2 (length a))))

(deftest js-rt-array-pop-empty-undefined
  "pop on empty array returns undefined."
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-array-pop (%jr-arr))))

(deftest js-rt-array-map
  "map applies fn to each element, returning a new array."
  (let ((r (cl-cc/javascript::%js-array-map (%jr-arr 1 2 3)
                                            (lambda (x &rest _) (declare (ignore _)) (* x 2)))))
    (assert-equal '(2 4 6) (%jr-list r))))

(deftest js-rt-array-filter
  "filter keeps elements for which fn is truthy."
  (let ((r (cl-cc/javascript::%js-array-filter (%jr-arr 1 2 3 4)
                                               (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (assert-equal '(2 4) (%jr-list r))))

(deftest js-rt-array-reduce
  "reduce folds with an initial value."
  (assert-= 10 (cl-cc/javascript::%js-array-reduce (%jr-arr 1 2 3 4)
                                                   (lambda (acc x &rest _) (declare (ignore _)) (+ acc x))
                                                   0)))

(deftest js-rt-array-includes
  "includes uses strict equality."
  (assert-true  (cl-cc/javascript::%js-array-includes (%jr-arr 1 2 3) 2))
  (assert-false (cl-cc/javascript::%js-array-includes (%jr-arr 1 2 3) 9)))

(deftest js-rt-array-index-of
  "indexOf returns the first index or -1."
  (assert-= 1  (cl-cc/javascript::%js-array-index-of (%jr-arr 5 6 7) 6))
  (assert-= -1 (cl-cc/javascript::%js-array-index-of (%jr-arr 5 6 7) 9)))

(deftest js-rt-array-join
  "join concatenates with a separator."
  (assert-string= "1,2,3" (cl-cc/javascript::%js-array-join (%jr-arr 1 2 3)))
  (assert-string= "1-2-3" (cl-cc/javascript::%js-array-join (%jr-arr 1 2 3) "-")))

;;; ─── String ─────────────────────────────────────────────────────────────────────

(deftest js-rt-string-slice
  "slice handles positive and negative indices."
  (assert-string= "ell" (cl-cc/javascript::%js-string-slice "hello" 1 4))
  (assert-string= "lo"  (cl-cc/javascript::%js-string-slice "hello" -2)))

(deftest js-rt-string-includes
  "includes finds substrings."
  (assert-true  (cl-cc/javascript::%js-string-includes "hello" "ell"))
  (assert-false (cl-cc/javascript::%js-string-includes "hello" "xyz")))

(deftest js-rt-string-index-of
  "indexOf returns position or -1."
  (assert-= 2  (cl-cc/javascript::%js-string-index-of "hello" "l"))
  (assert-= -1 (cl-cc/javascript::%js-string-index-of "hello" "z")))

(deftest js-rt-string-case
  "toUpperCase / toLowerCase."
  (assert-string= "HELLO" (cl-cc/javascript::%js-string-to-upper-case "hello"))
  (assert-string= "hello" (cl-cc/javascript::%js-string-to-lower-case "HELLO")))

(deftest js-rt-string-repeat
  "repeat duplicates the string; <=0 yields empty."
  (assert-string= "ababab" (cl-cc/javascript::%js-string-repeat "ab" 3))
  (assert-string= ""       (cl-cc/javascript::%js-string-repeat "ab" 0)))

(deftest js-rt-string-split-empty-sep
  "split with empty separator yields characters."
  (assert-equal '("a" "b" "c")
                (%jr-list (cl-cc/javascript::%js-string-split "abc" ""))))

;;; ─── Math ─────────────────────────────────────────────────────────────────────

(deftest js-rt-math-abs
  "abs of negative is positive."
  (assert-= 5 (cl-cc/javascript::%js-math-abs -5)))

(deftest js-rt-math-floor-ceil
  "floor and ceil."
  (assert-= 3 (cl-cc/javascript::%js-math-floor 3.7d0))
  (assert-= 4 (cl-cc/javascript::%js-math-ceil 3.2d0)))

(deftest js-rt-math-max-min
  "max and min over several args."
  (assert-= 9 (cl-cc/javascript::%js-math-max 3 9 1))
  (assert-= 1 (cl-cc/javascript::%js-math-min 3 9 1)))

(deftest js-rt-math-pow-sqrt
  "pow and sqrt."
  (assert-= 8   (cl-cc/javascript::%js-math-pow 2 3))
  (assert-= 3.0d0 (cl-cc/javascript::%js-math-sqrt 9)))

(deftest js-rt-math-sign
  "sign returns -1/0/1."
  (assert-= -1 (cl-cc/javascript::%js-math-sign -7))
  (assert-=  0 (cl-cc/javascript::%js-math-sign 0))
  (assert-=  1 (cl-cc/javascript::%js-math-sign 7)))

;;; ─── Object ─────────────────────────────────────────────────────────────────────

(deftest js-rt-object-make-and-get
  "make-object stores key/value pairs retrievable via get-prop."
  (let ((o (cl-cc/javascript::%js-make-object "a" 1 "b" 2)))
    (assert-= 1 (cl-cc/javascript::%js-get-prop o "a"))
    (assert-= 2 (cl-cc/javascript::%js-get-prop o "b"))))

(deftest js-rt-object-keys
  "keys returns the own string keys (order-independent)."
  (let* ((o (cl-cc/javascript::%js-make-object "x" 1 "y" 2))
         (ks (sort (%jr-list (cl-cc/javascript::%js-object-keys o)) #'string<)))
    (assert-equal '("x" "y") ks)))

(deftest js-rt-object-values
  "values returns the own values (order-independent)."
  (let* ((o (cl-cc/javascript::%js-make-object "x" 10 "y" 20))
         (vs (sort (%jr-list (cl-cc/javascript::%js-object-values o)) #'<)))
    (assert-equal '(10 20) vs)))

(deftest js-rt-get-prop-array-length
  "get-prop \"length\" on an array returns its length."
  (assert-= 3 (cl-cc/javascript::%js-get-prop (%jr-arr 1 2 3) "length")))

(deftest js-rt-get-prop-array-index
  "get-prop with a numeric key indexes the array."
  (assert-= 20 (cl-cc/javascript::%js-get-prop (%jr-arr 10 20 30) 1)))
