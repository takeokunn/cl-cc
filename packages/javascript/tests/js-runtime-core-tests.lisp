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
