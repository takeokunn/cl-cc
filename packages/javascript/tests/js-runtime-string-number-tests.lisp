;;;; packages/javascript/tests/js-runtime-string-number-tests.lisp
;;;;
;;;; String prototype methods, Math (basic and transcendental), Number.prototype
;;;; methods, number format helpers, predicates (isNaN/isFinite/isInteger),
;;;; and parseInt/parseFloat.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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
  (assert-string= "hello"    (cl-cc/javascript::%js-string-trim       "  hello  "))
  (assert-string= "hello  "  (cl-cc/javascript::%js-string-trim-start "  hello  "))
  (assert-string= "  hello"  (cl-cc/javascript::%js-string-trim-end   "  hello  ")))

(deftest-each js-rt-string-at
  "String.prototype.at supports positive and negative indices."
  :cases (("first"  0  "h")
          ("last"  -1  "o")
          ("mid"    2  "l"))
  (idx expected)
  (assert-string= expected (cl-cc/javascript::%js-string-at "hello" idx)))

(deftest js-rt-string-char-at-code-at
  "charAt and charCodeAt return character and its code."
  (assert-string= "e" (cl-cc/javascript::%js-string-char-at      "hello" 1))
  (assert-=      101  (cl-cc/javascript::%js-string-char-code-at "hello" 1)))

(deftest-each js-rt-string-last-index-of
  "lastIndexOf returns the last occurrence index, or -1 when absent."
  :cases (("found-mid"  "abcabc" "b" 4)
          ("found-end"  "aabb"   "b" 3)
          ("not-found"  "abc"    "z" -1))
  (s sub expected)
  (assert-= expected (cl-cc/javascript::%js-string-last-index-of s sub)))

(deftest js-rt-string-concat
  "concat joins strings without modifying originals."
  (assert-string= "hello world"
                  (cl-cc/javascript::%js-string-concat "hello" " " "world")))

(deftest js-rt-string-from-char-code-and-code-point
  "fromCharCode and fromCodePoint convert numeric codes to strings."
  (assert-string= "A" (cl-cc/javascript::%js-string-from-char-code  65))
  (assert-string= "A" (cl-cc/javascript::%js-string-from-code-point 65)))

(deftest js-rt-string-code-point-at
  "codePointAt returns the code point at the given position."
  (assert-= 72  (cl-cc/javascript::%js-string-code-point-at "Hello" 0))
  (assert-= 101 (cl-cc/javascript::%js-string-code-point-at "Hello" 1)))

(deftest js-rt-string-is-well-formed
  "isWellFormed and toWellFormed handle well-formed strings."
  (let ((wf "hello"))
    (assert-true   (cl-cc/javascript::%js-string-is-well-formed wf))
    (assert-string= wf (cl-cc/javascript::%js-string-to-well-formed wf))))

(deftest-each js-rt-string-replace
  "replace substitutes first occurrence; replaceAll substitutes all."
  :cases (("first"  #'cl-cc/javascript::%js-string-replace     "aaa" "a" "b"  "baa")
          ("all"    #'cl-cc/javascript::%js-string-replace-all  "aaa" "a" "b"  "bbb"))
  (fn s pat rep expected)
  (assert-string= expected (funcall fn s pat rep)))

(deftest js-rt-method-resolution-string
  "Calling a method through get-prop on a string invokes the correct helper."
  (let* ((s "hello")
         (upper-fn (cl-cc/javascript::%js-get-prop s "toUpperCase")))
    (assert-true (functionp upper-fn))
    (assert-string= "HELLO" (funcall upper-fn))))

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

;;; ─── Number.prototype methods ────────────────────────────────────────────────

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

;;; ─── String coverage — uncovered functions ───────────────────────────────────

(deftest js-rt-string-length
  "String length returns the character count."
  (assert-= 0 (cl-cc/javascript::%js-string-length ""))
  (assert-= 5 (cl-cc/javascript::%js-string-length "hello")))

(deftest-each js-rt-string-split-separator
  "split with a non-empty string separator yields substrings."
  :cases (("csv"    "a,b,c" ","  '("a" "b" "c"))
          ("empty"  "a,,b"  ","  '("a" "" "b"))
          ("no-sep" "abc"   ","  '("abc")))
  (s sep expected)
  (assert-equal expected (%jr-list (cl-cc/javascript::%js-string-split s sep))))

(deftest js-rt-string-split-limit
  "split with a limit caps the number of parts."
  (let ((parts (%jr-list (cl-cc/javascript::%js-string-split "a,b,c,d" "," 2))))
    (assert-= 2 (length parts))
    (assert-string= "a" (first parts))
    (assert-string= "b" (second parts))))

(deftest-each js-rt-string-match-cases
  "%js-string-match returns first-match array or +js-null+."
  :cases (("found"     "hello world" "world"  "world")
          ("not-found" "hello"       "xyz"    :null))
  (s pat expected)
  (let ((result (cl-cc/javascript::%js-string-match s pat)))
    (if (eq expected :null)
        (assert-eq cl-cc/javascript::+js-null+ result)
        (assert-string= expected (aref result 0)))))

(deftest js-rt-string-match-all-multiple
  "%js-string-match-all returns an array of per-match arrays."
  (let ((all (cl-cc/javascript::%js-string-match-all "abab" "ab")))
    (assert-= 2 (length all))
    (assert-string= "ab" (aref (aref all 0) 0))
    (assert-string= "ab" (aref (aref all 1) 0))))

(deftest-each js-rt-string-search-index
  "%js-string-search returns the index of the first match or -1."
  :cases (("found"     "hello" "ll"  2)
          ("not-found" "hello" "xyz" -1))
  (s pat expected)
  (assert-= expected (cl-cc/javascript::%js-string-search s pat)))

(deftest js-rt-string-normalize
  "%js-string-normalize is a stub that returns the string unchanged."
  (assert-string= "cafe" (cl-cc/javascript::%js-string-normalize "cafe"))
  (assert-string= "cafe" (cl-cc/javascript::%js-string-normalize "cafe" "NFD")))

(deftest-each js-rt-string-locale-compare-order
  "localeCompare returns -1.0, 0.0, or 1.0 based on lexicographic ordering."
  :cases (("lt" "a" "b" -1.0d0)
          ("eq" "x" "x"  0.0d0)
          ("gt" "b" "a"  1.0d0))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%js-string-locale-compare a b)))

(deftest js-rt-string-raw-tag
  "String.raw accepts a raw-parts vector directly and interleaves substitutions."
  (let* ((raw    (%jr-arr "hello " " world"))
         (result (cl-cc/javascript::%js-string-raw raw "JS")))
    (assert-string= "hello JS world" result)))

(deftest-each js-rt-string-substring-cases
  "substring clamps negative/over-length indices and swaps reversed start/end."
  :cases (("basic"    "hello" 1 3   "el")
          ("clamp"    "hello" 0 100 "hello")
          ("reversed" "hello" 3 1   "el"))
  (s a b expected)
  (assert-string= expected (cl-cc/javascript::%js-string-substring s a b)))

(deftest-each js-rt-string-locale-case
  "toLocaleUpperCase and toLocaleLowerCase are locale-neutral aliases."
  :cases (("upper" #'cl-cc/javascript::%js-string-to-locale-upper-case "hello" "HELLO")
          ("lower" #'cl-cc/javascript::%js-string-to-locale-lower-case "HELLO" "hello"))
  (fn input expected)
  (assert-string= expected (funcall fn input)))
