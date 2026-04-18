;;;; tests/unit/runtime/runtime-advanced-tests.lisp — Runtime Advanced Unit Tests
;;;;
;;;; Tests for src/runtime/runtime.lisp: bitwise ops, string/char ops,
;;;; symbol ops, hash table ops, conditions, misc, and I/O wrappers.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Bitwise ───────────────────────────────────────────────────────────────

(deftest-each rt-bitwise-ops
  "rt-logand/logior/logxor/ash: binary bitwise and shift operations."
  :cases (("and"   #'cl-cc/runtime:rt-logand #b1110  #b1011 #b1010)
          ("or"    #'cl-cc/runtime:rt-logior #b1010  #b0101 #b1111)
          ("xor"   #'cl-cc/runtime:rt-logxor #b1010  #b0101 #b1111)
          ("ash-l" #'cl-cc/runtime:rt-ash    2       2      8)
          ("ash-r" #'cl-cc/runtime:rt-ash    8      -2      2))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

(deftest-each rt-bitwise-predicate-conventions
  "rt-logtest and rt-logbitp return 1/0 (bit overlap and bit position tests)."
  :cases (("logtest-overlap"  #'cl-cc/runtime:rt-logtest  #b1010 #b1000 1)
          ("logtest-disjoint" #'cl-cc/runtime:rt-logtest  #b1010 #b0101 0)
          ("logbitp-set"      #'cl-cc/runtime:rt-logbitp  0      1      1)
          ("logbitp-clear"    #'cl-cc/runtime:rt-logbitp  1      1      0))
  (pred-fn a b expected)
  (assert-= expected (funcall pred-fn a b)))

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

(deftest-each rt-string-transform-ops
  "String case and trim operations produce correct output."
  :cases (("upcase"      #'cl-cc/runtime:rt-string-upcase                        "hello"    "HELLO")
          ("downcase"    #'cl-cc/runtime:rt-string-downcase                       "HELLO"    "hello")
          ("capitalize"  #'cl-cc/runtime:rt-string-capitalize                     "hello world" "Hello World")
          ("trim"        (lambda (s) (cl-cc/runtime:rt-string-trim " " s))        " hello "  "hello")
          ("left-trim"   (lambda (s) (cl-cc/runtime:rt-string-left-trim " " s))  " hello "  "hello ")
          ("right-trim"  (lambda (s) (cl-cc/runtime:rt-string-right-trim " " s)) " hello "  " hello"))
  (fn input expected)
  (assert-equal expected (funcall fn input)))

(deftest rt-search-string-and-subseq
  "rt-search-string and rt-subseq."
  (assert-= 3 (cl-cc/runtime:rt-search-string "lo" "hello world"))
  (assert-equal "llo" (cl-cc/runtime:rt-subseq "hello" 2)))

;;; ─── Character Operations ──────────────────────────────────────────────────

(deftest rt-char-code-roundtrip
  "rt-char-code and rt-code-char roundtrip."
  (assert-equal #\A (cl-cc/runtime:rt-code-char (cl-cc/runtime:rt-char-code #\A))))

(deftest-each rt-char-predicates
  "Character predicates return 1/0."
  :cases (("alpha-t"   #'cl-cc/runtime:rt-alpha-char-p   #\a  1)
          ("alpha-f"   #'cl-cc/runtime:rt-alpha-char-p   #\1  0)
          ("digit-t"   #'cl-cc/runtime:rt-digit-char-p   #\5  1)
          ("digit-f"   #'cl-cc/runtime:rt-digit-char-p   #\a  0)
          ("alnum-t"   #'cl-cc/runtime:rt-alphanumericp  #\a  1)
          ("alnum-f"   #'cl-cc/runtime:rt-alphanumericp  #\!  0)
          ("upper-t"   #'cl-cc/runtime:rt-upper-case-p   #\A  1)
          ("upper-f"   #'cl-cc/runtime:rt-upper-case-p   #\a  0)
          ("lower-t"   #'cl-cc/runtime:rt-lower-case-p   #\a  1)
          ("lower-f"   #'cl-cc/runtime:rt-lower-case-p   #\A  0))
  (pred-fn input expected)
  (assert-= expected (funcall pred-fn input)))

(deftest-each rt-char-case-ops
  "rt-char-upcase/downcase: convert character case."
  :cases (("upcase"   #'cl-cc/runtime:rt-char-upcase   #\a #\A)
          ("downcase" #'cl-cc/runtime:rt-char-downcase  #\A #\a))
  (fn input expected)
  (assert-equal expected (funcall fn input)))

;;; ─── Symbol Operations ─────────────────────────────────────────────────────

(deftest rt-symbol-operations
  "rt-symbol-name, rt-make-symbol (uninterned), and rt-gensym (unique)."
  (assert-equal "FOO" (cl-cc/runtime:rt-symbol-name 'foo))
  (let ((s (cl-cc/runtime:rt-make-symbol "TEST")))
    (assert-equal "TEST" (symbol-name s))
    (assert-false (symbol-package s)))
  (assert-false (eq (cl-cc/runtime:rt-gensym) (cl-cc/runtime:rt-gensym))))

(deftest rt-symbol-plist-roundtrip
  "rt-put-prop / rt-get-prop / rt-remprop roundtrip."
  (let ((sym (cl-cc/runtime:rt-make-symbol "PLIST-TEST")))
    (cl-cc/runtime:rt-put-prop sym :color 'red)
    (assert-eq 'red (cl-cc/runtime:rt-get-prop sym :color))
    (cl-cc/runtime:rt-remprop sym :color)
    (assert-false (cl-cc/runtime:rt-get-prop sym :color))))

;;; ─── Hash Table Operations ─────────────────────────────────────────────────

(deftest rt-hash-table-ops
  "rt-make-hash-table: set/get/rem/count/keys/values/clrhash all work correctly."
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :a ht 1)
    (cl-cc/runtime:rt-sethash :b ht 2)
    (assert-= 1 (cl-cc/runtime:rt-gethash :a ht))
    (assert-= 2 (cl-cc/runtime:rt-hash-count ht))
    (cl-cc/runtime:rt-remhash :a ht)
    (assert-= 1 (cl-cc/runtime:rt-hash-count ht)))
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :x ht 10)
    (cl-cc/runtime:rt-sethash :y ht 20)
    (assert-= 2 (length (cl-cc/runtime:rt-hash-keys ht)))
    (assert-= 2 (length (cl-cc/runtime:rt-hash-values ht)))
    (assert-true (member :x (cl-cc/runtime:rt-hash-keys ht)))
    (assert-true (member 10 (cl-cc/runtime:rt-hash-values ht)))
    (cl-cc/runtime:rt-clrhash ht)
    (assert-= 0 (cl-cc/runtime:rt-hash-count ht))))

;;; ─── Conditions ────────────────────────────────────────────────────────────

(deftest rt-signal-error-signals
  "rt-signal-error signals the given condition."
  (assert-signals error (cl-cc/runtime:rt-signal-error "test error")))

(deftest rt-bind-restart-calls-thunk
  "rt-bind-restart just calls the thunk (minimal stub)."
  (let ((called nil))
    (cl-cc/runtime:rt-bind-restart 'continue nil (lambda () (setf called t)))
    (assert-true called)))

;;; ─── Misc ──────────────────────────────────────────────────────────────────

(deftest-each rt-fboundp-convention
  "rt-fboundp returns 1 for bound function symbols, 0 for unbound."
  :cases (("bound"   '+ 1)
          ("unbound" (gensym "UNBOUND") 0))
  (sym expected)
  (assert-= expected (cl-cc/runtime:rt-fboundp sym)))

(deftest rt-coerce-works
  "rt-coerce delegates to CL coerce."
  (assert-equal '(1 2 3) (cl-cc/runtime:rt-coerce #(1 2 3) 'list)))

(deftest-each rt-parse-integer
  "rt-parse-integer: decimal and hex (with :radix) parsing."
  :cases (("decimal" "42"  10 42)
          ("hex"     "FF"  16 255))
  (input radix expected)
  (assert-= expected (cl-cc/runtime:rt-parse-integer input :radix radix)))

;;; ─── I/O Wrappers ──────────────────────────────────────────────────────────

(deftest rt-string-stream-creation-and-io
  "String stream creation: input-stream read-char, output-stream write+get, output-stream roundtrip."
  ;; Input stream: read-char returns first character
  (let ((s (cl-cc/runtime:rt-make-string-stream "hello")))
    (assert-equal #\h (cl-cc/runtime:rt-read-char s)))
  ;; Output stream via rt-make-string-stream: write then get-string
  (let ((s (cl-cc/runtime:rt-make-string-stream "" :direction :output)))
    (cl-cc/runtime:rt-write-string "world" s)
    (assert-equal "world" (cl-cc/runtime:rt-get-string-from-stream s)))
  ;; Output stream via rt-make-string-output-stream: stream-write-string + get-output
  (let ((s (cl-cc/runtime:rt-make-string-output-stream)))
    (cl-cc/runtime:rt-stream-write-string s "test")
    (assert-equal "test" (cl-cc/runtime:rt-get-output-stream-string s))))

(deftest-each rt-stream-predicates
  "rt-input-stream-p / rt-output-stream-p / rt-open-stream-p return 1/0."
  :cases (("input-true"  #'cl-cc/runtime:rt-input-stream-p  :input
           (lambda (pred-fn stream)
             (assert-= 1 (funcall pred-fn stream))))
          ("input-false" #'cl-cc/runtime:rt-input-stream-p  :output
           (lambda (pred-fn stream)
             (assert-= 0 (funcall pred-fn stream))))
          ("output-true" #'cl-cc/runtime:rt-output-stream-p :output
           (lambda (pred-fn stream)
             (assert-= 1 (funcall pred-fn stream))))
          ("output-false" #'cl-cc/runtime:rt-output-stream-p :input
           (lambda (pred-fn stream)
             (assert-= 0 (funcall pred-fn stream))))
          ("open-true"   #'cl-cc/runtime:rt-open-stream-p   :input
           (lambda (pred-fn stream)
             (assert-= 1 (funcall pred-fn stream)))))
  (pred-fn direction verify)
  (let ((stream (ecase direction
                   (:input  (make-string-input-stream "x"))
                   (:output (make-string-output-stream)))))
    (funcall verify pred-fn stream)))

(deftest rt-read-write-char-roundtrip
  "rt-write-char / rt-read-char via string streams."
  (let ((out (make-string-output-stream)))
    (cl-cc/runtime:rt-write-char #\Z out)
    (let ((in (make-string-input-stream (get-output-stream-string out))))
      (assert-equal #\Z (cl-cc/runtime:rt-read-char in)))))

(deftest-each rt-pathname-component-extraction
  "rt-pathname-component extracts :name and :type components."
  :cases (("name" :name "test")
          ("type" :type "lisp"))
  (component expected)
  (let ((p (cl-cc/runtime:rt-make-pathname :name "test" :type "lisp")))
    (assert-equal expected (cl-cc/runtime:rt-pathname-component p component))))
