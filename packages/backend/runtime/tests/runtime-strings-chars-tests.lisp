;;;; tests/unit/runtime/runtime-strings-chars-tests.lisp
;;;;
;;;; Tests for packages/backend/runtime/src/runtime-strings.lisp:
;;;; string ops, string comparisons, char ops, char comparisons, char predicates.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── String Operations ─────────────────────────────────────────────────────

(deftest rt-string-basic
  "rt-make-string, rt-string-length, rt-string-ref, rt-string-set."
  (let ((s (cl-cc/runtime:rt-make-string 3 #\x)))
    (assert-= 3 (cl-cc/runtime:rt-string-length s))
    (assert-equal #\x (cl-cc/runtime:rt-string-ref s 0))
    (cl-cc/runtime:rt-string-set s 1 #\y)
    (assert-equal #\y (cl-cc/runtime:rt-string-ref s 1))))

(deftest-each rt-string-comparisons
  "String comparison wrappers return 1/0."
  :cases (("=-t"    #'cl-cc/runtime:rt-string=    "abc" "abc" 1)
          ("=-f"    #'cl-cc/runtime:rt-string=    "abc" "abd" 0)
          ("<-t"    #'cl-cc/runtime:rt-string<    "abc" "abd" 1)
          ("<-f"    #'cl-cc/runtime:rt-string<    "abd" "abc" 0)
          (">-t"    #'cl-cc/runtime:rt-string>    "abd" "abc" 1)
          (">-f"    #'cl-cc/runtime:rt-string>    "abc" "abd" 0))
  (cmp-fn a b expected)
  (assert-= expected (funcall cmp-fn a b)))

(deftest-each rt-string-comparison-extended
  "String comparison wrappers: <=, >=, case-insensitive, not-equal variants."
  :cases (("<=-t"    #'cl-cc/runtime:rt-string<=             "abc" "abc" 1)
          ("<=-f"    #'cl-cc/runtime:rt-string<=             "abd" "abc" 0)
          (">=-t"    #'cl-cc/runtime:rt-string>=             "abc" "abc" 1)
          (">=-f"    #'cl-cc/runtime:rt-string>=             "abc" "abd" 0)
          ("ci-t"    #'cl-cc/runtime:rt-string-equal-ci      "ABC" "abc" 1)
          ("ci-f"    #'cl-cc/runtime:rt-string-equal-ci      "ABC" "xyz" 0)
          ("ne-t"    #'cl-cc/runtime:rt-string-not-equal     "abc" "xyz" 1)
          ("ne-f"    #'cl-cc/runtime:rt-string-not-equal     "abc" "abc" 0)
          ("lessp-t" #'cl-cc/runtime:rt-string-lessp         "abc" "abd" 1)
          ("ngp-t"   #'cl-cc/runtime:rt-string-not-greaterp  "abc" "abc" 1)
          ("nlp-t"   #'cl-cc/runtime:rt-string-not-lessp     "abc" "abc" 1))
  (cmp-fn a b expected)
  (assert-= expected (funcall cmp-fn a b)))

(deftest-each rt-string-transform-ops
  "String case and trim operations produce correct output."
  :cases (("upcase"     #'cl-cc/runtime:rt-string-upcase                        "hello"       "HELLO")
          ("downcase"   #'cl-cc/runtime:rt-string-downcase                       "HELLO"       "hello")
          ("capitalize" #'cl-cc/runtime:rt-string-capitalize                     "hello world" "Hello World")
          ("trim"       (lambda (s) (cl-cc/runtime:rt-string-trim " " s))        " hello "     "hello")
          ("left-trim"  (lambda (s) (cl-cc/runtime:rt-string-left-trim " " s))  " hello "     "hello ")
          ("right-trim" (lambda (s) (cl-cc/runtime:rt-string-right-trim " " s)) " hello "     " hello"))
  (fn input expected)
  (assert-equal expected (funcall fn input)))

(deftest rt-search-string-and-subseq
  "rt-search-string and rt-subseq."
  (assert-= 3 (cl-cc/runtime:rt-search-string "lo" "hello world"))
  (assert-equal "llo" (cl-cc/runtime:rt-subseq "hello" 2)))

(deftest rt-concatenate-seqs-string-and-list
  "rt-concatenate-seqs joins strings and lists."
  (assert-equal "hello world" (cl-cc/runtime:rt-concatenate-seqs 'string "hello" " world"))
  (assert-equal '(1 2 3 4) (cl-cc/runtime:rt-concatenate-seqs 'list '(1 2) '(3 4))))

;;; ─── Character Operations ──────────────────────────────────────────────────

(deftest rt-char-code-roundtrip
  "rt-char-code and rt-code-char roundtrip."
  (assert-equal #\A (cl-cc/runtime:rt-code-char (cl-cc/runtime:rt-char-code #\A))))

(deftest-each rt-char-predicates
  "Character predicates return 1/0."
  :cases (("alpha-t"  #'cl-cc/runtime:rt-alpha-char-p   #\a  1)
          ("alpha-f"  #'cl-cc/runtime:rt-alpha-char-p   #\1  0)
          ("digit-t"  #'cl-cc/runtime:rt-digit-char-p   #\5  1)
          ("digit-f"  #'cl-cc/runtime:rt-digit-char-p   #\a  0)
          ("alnum-t"  #'cl-cc/runtime:rt-alphanumericp  #\a  1)
          ("alnum-f"  #'cl-cc/runtime:rt-alphanumericp  #\!  0)
          ("upper-t"  #'cl-cc/runtime:rt-upper-case-p   #\A  1)
          ("upper-f"  #'cl-cc/runtime:rt-upper-case-p   #\a  0)
          ("lower-t"  #'cl-cc/runtime:rt-lower-case-p   #\a  1)
          ("lower-f"  #'cl-cc/runtime:rt-lower-case-p   #\A  0))
  (pred-fn input expected)
  (assert-= expected (funcall pred-fn input)))

(deftest-each rt-char-case-ops
  "rt-char-upcase/downcase: convert character case."
  :cases (("upcase"   #'cl-cc/runtime:rt-char-upcase   #\a #\A)
          ("downcase" #'cl-cc/runtime:rt-char-downcase  #\A #\a))
  (fn input expected)
  (assert-equal expected (funcall fn input)))

(deftest-each rt-char-comparisons-cs
  "Case-sensitive char comparison wrappers return 1/0."
  :cases (("=-t"  #'cl-cc/runtime:rt-char-equal-cs  #\a #\a 1)
          ("=-f"  #'cl-cc/runtime:rt-char-equal-cs  #\a #\b 0)
          ("<-t"  #'cl-cc/runtime:rt-char-lt-cs     #\a #\b 1)
          ("<-f"  #'cl-cc/runtime:rt-char-lt-cs     #\b #\a 0)
          (">-t"  #'cl-cc/runtime:rt-char-gt-cs     #\b #\a 1)
          (">-f"  #'cl-cc/runtime:rt-char-gt-cs     #\a #\b 0)
          ("<=-t" #'cl-cc/runtime:rt-char-le-cs     #\a #\a 1)
          (">=-t" #'cl-cc/runtime:rt-char-ge-cs     #\a #\a 1)
          ("ne-t" #'cl-cc/runtime:rt-char-ne-cs     #\a #\b 1)
          ("ne-f" #'cl-cc/runtime:rt-char-ne-cs     #\a #\a 0))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

(deftest-each rt-char-comparisons-ci
  "Case-insensitive char comparison wrappers return 1/0."
  :cases (("equal-t"     #'cl-cc/runtime:rt-char-equal-ci          #\A #\a 1)
          ("equal-f"     #'cl-cc/runtime:rt-char-equal-ci          #\A #\b 0)
          ("ne-ci-t"     #'cl-cc/runtime:rt-char-not-equal-ci      #\a #\b 1)
          ("lessp-t"     #'cl-cc/runtime:rt-char-lessp-ci          #\a #\B 1)
          ("greaterp-t"  #'cl-cc/runtime:rt-char-greaterp-ci       #\B #\a 1)
          ("nlessp-t"    #'cl-cc/runtime:rt-char-not-lessp-ci      #\A #\a 1)
          ("ngreaterp-t" #'cl-cc/runtime:rt-char-not-greaterp-ci   #\a #\A 1))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

(deftest rt-char-name-and-digit
  "rt-char-name names special chars; rt-digit-char reverses digit lookup."
  (assert-equal "Space" (cl-cc/runtime:rt-char-name #\Space))
  (assert-equal #\7 (cl-cc/runtime:rt-digit-char 7))
  (assert-equal #\A (cl-cc/runtime:rt-digit-char 10 16)))

(deftest-each rt-parse-integer
  "rt-parse-integer: decimal and hex (with :radix) parsing."
  :cases (("decimal" "42"  10 42)
          ("hex"     "FF"  16 255))
  (input radix expected)
  (assert-= expected (cl-cc/runtime:rt-parse-integer input :radix radix)))
