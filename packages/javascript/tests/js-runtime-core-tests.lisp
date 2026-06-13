;;;; packages/javascript/tests/js-runtime-core-tests.lisp
;;;;
;;;; Core type coercion, equality, truthiness, for-of/in, try-catch, Object
;;;; basic operations, and bitwise operators.
;;;;
;;;; Helper functions (%jr-arr, %jr-list, %jr-set) are defined here
;;;; and available to all subsequently-loaded js-runtime-* test files.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Shared helpers (used across all js-runtime-* test files) ─────────────────

(defun %jr-arr (&rest els)
  "Build a JS array (adjustable vector) from ELS."
  (apply #'cl-cc/javascript::%js-make-array els))

(defun %jr-list (vec)
  "Coerce a JS array (vector) to a CL list for easy comparison."
  (coerce vec 'list))

(defun %jr-set (&rest vals)
  "Build a JS Set from VALS in insertion order."
  (let ((s (cl-cc/javascript::%js-make-set)))
    (dolist (v vals s)
      (cl-cc/javascript::%js-set-add s v))))

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

;;; ─── Strict equality ────────────────────────────────────────────────────────

(deftest-each js-rt-strict-eq
  "=== (strict equality) uses type+value identity."
  :cases (("same-num"  3   3   t)
          ("diff-num"  3   4   nil)
          ("same-str"  "a" "a" t)
          ("diff-str"  "a" "b" nil))
  (a b expected)
  (assert-equal expected (cl-cc/javascript::%js-strict-eq a b)))

;;; ─── Truthiness ──────────────────────────────────────────────────────────────

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

;;; ─── Object basic operations ─────────────────────────────────────────────────

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

(deftest js-rt-object-prop-false-value
  "get-prop must return nil (CL false) when a key is explicitly set to nil.
Regression: %js-resolve-object-method used (not (eq stored nil)) which treated
nil-valued keys as missing."
  (let ((obj (cl-cc/javascript::%js-make-object "flag" nil)))
    (assert-false (cl-cc/javascript::%js-get-prop obj "flag"))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-get-prop obj "absent"))))

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

(deftest js-rt-for-in-skips-accessor-keys
  "for-in uses %js-internal-key-p, which filters __get_X and __set_X
accessor slots (regression: old check only matched __X__ form)."
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1 "b" 2))
        (keys nil))
    (setf (gethash "__get_foo" obj) (lambda () 99))
    (setf (gethash "__set_foo" obj) (lambda (v) v))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-for-in
     obj (lambda (k &rest _) (declare (ignore _)) (push k keys)))
    (assert-equal '("a" "b") (sort keys #'string<))))

;;; ─── ToNumber coercions ──────────────────────────────────────────────────────

(deftest-each js-rt-to-number
  "ToNumber coerces CL values to JS double-floats; strings that cannot parse yield NaN."
  :cases (("integer"    42                                   42.0d0)
          ("true"       t                                    1.0d0)
          ("false"      nil                                  0.0d0)
          ("null"       cl-cc/javascript::+js-null+          0.0d0)
          ("empty-str"  ""                                   0.0d0)
          ("num-str"    "3.14"                               3.14d0)
          ("int-str"    "42"                                 42.0d0)
          ("trimmed"    "  7  "                              7.0d0))
  (value expected)
  (assert-= expected (cl-cc/javascript::%js-to-number value)))

(deftest js-rt-to-number-nan-str
  "ToNumber(\"abc\") = NaN."
  (assert-true (cl-cc/javascript::%js-nan-p (cl-cc/javascript::%js-to-number "abc"))))

;;; ─── typeof: extended types ──────────────────────────────────────────────────

(deftest-each js-rt-typeof-extended
  "typeof distinguishes host functions, plain hash-tables, and BigInt values."
  :cases (("function"  (lambda () nil)                         "function")
          ("object-ht" (cl-cc/javascript::%js-make-ht)         "object")
          ("bigint"    (cl-cc/javascript::%make-js-bigint 5)   "bigint"))
  (value expected)
  (assert-string= expected (cl-cc/javascript::%js-typeof value)))

(deftest js-rt-typeof-callable-object
  "typeof returns \"function\" for a hash-table whose \"__call__\" slot is set."
  (let ((fn-obj (cl-cc/javascript::%js-make-ht)))
    (setf (gethash "__call__" fn-obj) (lambda () nil))
    (assert-string= "function" (cl-cc/javascript::%js-typeof fn-obj))))

;;; ─── Relational operators ────────────────────────────────────────────────────

(deftest-each js-rt-relational-num
  "Numeric relational operators (<, >, <=, >=) coerce operands to doubles."
  :cases (("lt-true"   #'cl-cc/javascript::%js-lt 1 2 t)
          ("lt-false"  #'cl-cc/javascript::%js-lt 2 1 nil)
          ("gt-true"   #'cl-cc/javascript::%js-gt 5 3 t)
          ("gt-false"  #'cl-cc/javascript::%js-gt 3 5 nil)
          ("le-eq"     #'cl-cc/javascript::%js-le 3 3 t)
          ("le-less"   #'cl-cc/javascript::%js-le 2 3 t)
          ("ge-eq"     #'cl-cc/javascript::%js-ge 3 3 t)
          ("ge-more"   #'cl-cc/javascript::%js-ge 4 3 t))
  (fn a b expected)
  (assert-equal expected (funcall fn a b)))

(deftest-each js-rt-relational-string
  "String relational comparison is lexicographic (code-unit order)."
  :cases (("lt-abc"   #'cl-cc/javascript::%js-lt "a" "b" t)
          ("gt-abc"   #'cl-cc/javascript::%js-gt "b" "a" t)
          ("lt-same"  #'cl-cc/javascript::%js-lt "a" "a" nil)
          ("le-same"  #'cl-cc/javascript::%js-le "a" "a" t))
  (fn a b expected)
  (assert-equal expected (funcall fn a b)))

(deftest js-rt-relational-nan-always-false
  "Any relational comparison involving NaN returns nil (JS spec)."
  (let ((nan cl-cc/javascript::+js-nan+))
    (assert-false (cl-cc/javascript::%js-lt nan 1))
    (assert-false (cl-cc/javascript::%js-gt nan 1))
    (assert-false (cl-cc/javascript::%js-le nan 1))
    (assert-false (cl-cc/javascript::%js-ge nan 1))))

;;; ─── Nullish coalescing ──────────────────────────────────────────────────────

(deftest-each js-rt-nullish-coalesce
  "?? returns RHS only when LHS is null or undefined; all other falsy values use LHS."
  :cases (("null-rhs"  cl-cc/javascript::+js-null+       "default" "default")
          ("undef-rhs" cl-cc/javascript::+js-undefined+  "default" "default")
          ("false-lhs" nil                               "default" nil)
          ("zero-lhs"  0                                 "default" 0)
          ("str-lhs"   "x"                               "default" "x"))
  (lhs rhs expected)
  (assert-equal expected (cl-cc/javascript::%js-nullish-coalesce lhs rhs)))

;;; ─── instanceof ──────────────────────────────────────────────────────────────

(deftest js-rt-instanceof-true
  "instanceof returns true when the constructor's __prototype__ is in the __proto__ chain."
  (let* ((klass (cl-cc/javascript::%js-make-class nil nil))
         (obj   (cl-cc/javascript::%js-new klass)))
    (assert-true (cl-cc/javascript::%js-instanceof obj klass))))

(deftest js-rt-instanceof-false
  "instanceof returns nil when the prototype chains do not intersect."
  (let* ((klass-a (cl-cc/javascript::%js-make-class nil nil))
         (klass-b (cl-cc/javascript::%js-make-class nil nil))
         (obj     (cl-cc/javascript::%js-new klass-a)))
    (assert-false (cl-cc/javascript::%js-instanceof obj klass-b))))

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

;;; ─── JS + operator (add / string-concat) ────────────────────────────────────

(deftest js-rt-add-numeric
  "%js-add with two numbers returns their sum."
  (assert-= 7 (cl-cc/javascript::%js-add 3 4)))

(deftest-each js-rt-add-string-concat
  "%js-add concatenates when at least one operand is a string."
  :cases (("str-str"  "a"  "b"  "ab")
          ("num-str"   1   "x"  "1x")
          ("str-num"  "x"   1   "x1"))
  (a b expected)
  (assert-string= expected (cl-cc/javascript::%js-add a b)))

;;; ─── JS /, %, ** arithmetic operators ───────────────────────────────────────

(deftest-each js-rt-arithmetic-ops
  "Divide, mod, and pow handle JS floating-point semantics."
  :cases (("div-float" #'cl-cc/javascript::%js-divide 10  4   2.5d0)
          ("mod-pos"   #'cl-cc/javascript::%js-mod    10  3   1)
          ("mod-neg"   #'cl-cc/javascript::%js-mod    -5  3  -2)
          ("pow-two"   #'cl-cc/javascript::%js-pow     2 10 1024))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

(deftest js-rt-divide-by-zero-infinity
  "1/0 yields +Infinity (IEEE 754); 0/0 yields NaN."
  (assert-true (cl-cc/javascript::%js-float-infinity-p (cl-cc/javascript::%js-divide 1 0)))
  (assert-true (cl-cc/javascript::%js-nan-p (cl-cc/javascript::%js-divide 0 0))))

(deftest js-rt-mod-zero-denominator-nan
  "x % 0 yields NaN."
  (assert-true (cl-cc/javascript::%js-nan-p (cl-cc/javascript::%js-mod 5 0))))

;;; ─── Property: delete / in / optional-chain ──────────────────────────────────

(deftest js-rt-delete-property
  "%js-delete removes the named key from an object; absent keys stay undefined."
  (let ((o (cl-cc/javascript::%js-make-object "a" 1 "b" 2)))
    (cl-cc/javascript::%js-delete o "a")
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-get-prop o "a"))
    (assert-= 2 (cl-cc/javascript::%js-get-prop o "b"))))

(deftest-each js-rt-in-operator
  "%js-in returns t when the key exists, nil otherwise."
  :cases (("present" "a" t)
          ("absent"  "z" nil))
  (key expected)
  (let ((o (cl-cc/javascript::%js-make-object "a" 1)))
    (assert-equal expected (cl-cc/javascript::%js-in key o))))

(deftest js-rt-optional-chain-present
  "a?.b returns the property value when a is a non-null object."
  (let ((o (cl-cc/javascript::%js-make-object "x" 42)))
    (assert-= 42 (cl-cc/javascript::%js-optional-chain o "x"))))

(deftest js-rt-optional-chain-null-undefined
  "a?.b short-circuits to undefined when a is null or undefined."
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-optional-chain cl-cc/javascript::+js-null+ "x"))
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-optional-chain cl-cc/javascript::+js-undefined+ "x")))
