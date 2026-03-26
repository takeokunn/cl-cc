;;;; tests/unit/runtime/runtime-tests.lisp — Runtime Library Unit Tests
;;;;
;;;; Tests for src/runtime/runtime.lisp: tagged pointers, multiple values buffer,
;;;; closure support, type predicates, list ops, array ops, arithmetic helpers,
;;;; string/char ops, symbol ops, hash table ops, and I/O wrappers.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─── Tagged Pointers ───────────────────────────────────────────────────────

(deftest rt-tag-fixnum-zero
  "rt-tag-fixnum 0 → 0 (tag bits are 000)."
  (assert-= 0 (cl-cc/runtime:rt-tag-fixnum 0)))

(deftest rt-tag-fixnum-positive
  "rt-tag-fixnum shifts left by 3 bits."
  (assert-= 8 (cl-cc/runtime:rt-tag-fixnum 1))
  (assert-= 336 (cl-cc/runtime:rt-tag-fixnum 42)))

(deftest rt-untag-fixnum-roundtrip
  "rt-untag-fixnum reverses rt-tag-fixnum."
  (dolist (n '(0 1 42 -7 1000000))
    (assert-= n (cl-cc/runtime:rt-untag-fixnum (cl-cc/runtime:rt-tag-fixnum n)))))

(deftest rt-tag-bits-extracts-low-3
  "rt-tag-bits returns the low 3 bits."
  (assert-= 0 (cl-cc/runtime:rt-tag-bits 8))   ; fixnum tag
  (assert-= 1 (cl-cc/runtime:rt-tag-bits 9))   ; cons tag
  (assert-= 7 (cl-cc/runtime:rt-tag-bits 15))  ; other tag
  (assert-= 5 (cl-cc/runtime:rt-tag-bits 5)))

(deftest rt-tag-constants-distinct
  "All 8 tag constants have distinct values 0-7."
  (let ((tags (list cl-cc/runtime:+tag-fixnum+
                    cl-cc/runtime:+rt-tag-cons+
                    cl-cc/runtime:+rt-tag-symbol+
                    cl-cc/runtime:+rt-tag-function+
                    cl-cc/runtime:+tag-character+
                    cl-cc/runtime:+tag-array+
                    cl-cc/runtime:+rt-tag-string+
                    cl-cc/runtime:+tag-other+)))
    (assert-= 8 (length (remove-duplicates tags)))
    (dolist (tag tags) (assert-true (and (>= tag 0) (<= tag 7))))))

;;; ─── Multiple Values Buffer ────────────────────────────────────────────────

(deftest rt-values-buffer-clear-empty
  "rt-values-clear resets to empty; count is 0."
  (cl-cc/runtime:rt-values-clear)
  (assert-= 0 (cl-cc/runtime:rt-values-count)))

(deftest rt-values-buffer-push-and-count
  "rt-values-push adds values; count reflects them."
  (cl-cc/runtime:rt-values-clear)
  (cl-cc/runtime:rt-values-push 10)
  (cl-cc/runtime:rt-values-push 20)
  (assert-= 2 (cl-cc/runtime:rt-values-count)))

(deftest rt-values-buffer-ref
  "rt-values-ref retrieves by index."
  (cl-cc/runtime:rt-values-clear)
  (cl-cc/runtime:rt-values-push :a)
  (cl-cc/runtime:rt-values-push :b)
  (cl-cc/runtime:rt-values-push :c)
  (assert-eq :a (cl-cc/runtime:rt-values-ref 0))
  (assert-eq :b (cl-cc/runtime:rt-values-ref 1))
  (assert-eq :c (cl-cc/runtime:rt-values-ref 2)))

(deftest rt-values-buffer-to-list
  "rt-values-to-list returns the full buffer."
  (cl-cc/runtime:rt-values-clear)
  (cl-cc/runtime:rt-values-push 1)
  (cl-cc/runtime:rt-values-push 2)
  (assert-equal '(1 2) (cl-cc/runtime:rt-values-to-list)))

(deftest rt-spread-values-list
  "rt-spread-values spreads a list into the buffer."
  (cl-cc/runtime:rt-values-clear)
  (cl-cc/runtime:rt-spread-values '(10 20 30))
  (assert-= 3 (cl-cc/runtime:rt-values-count))
  (assert-= 10 (cl-cc/runtime:rt-values-ref 0)))

(deftest rt-spread-values-atom
  "rt-spread-values on a non-list pushes one value."
  (cl-cc/runtime:rt-values-clear)
  (cl-cc/runtime:rt-spread-values 42)
  (assert-= 1 (cl-cc/runtime:rt-values-count))
  (assert-= 42 (cl-cc/runtime:rt-values-ref 0)))

(deftest rt-ensure-values-empty
  "rt-ensure-values pushes val when buffer is empty."
  (cl-cc/runtime:rt-values-clear)
  (cl-cc/runtime:rt-ensure-values 99)
  (assert-= 1 (cl-cc/runtime:rt-values-count))
  (assert-= 99 (cl-cc/runtime:rt-values-ref 0)))

(deftest rt-ensure-values-non-empty
  "rt-ensure-values is a no-op when buffer already has values."
  (cl-cc/runtime:rt-values-clear)
  (cl-cc/runtime:rt-values-push 1)
  (cl-cc/runtime:rt-ensure-values 99)
  (assert-= 1 (cl-cc/runtime:rt-values-count))
  (assert-= 1 (cl-cc/runtime:rt-values-ref 0)))

;;; ─── Closure Support ───────────────────────────────────────────────────────

(deftest rt-make-closure-creates-struct
  "rt-make-closure returns an rt-closure-obj."
  (let ((c (cl-cc/runtime:rt-make-closure #'identity '(1 2 3))))
    (assert-true (cl-cc/runtime::rt-closure-obj-p c))))

(deftest rt-closure-ref-accesses-env
  "rt-closure-ref retrieves captured values by index."
  (let ((c (cl-cc/runtime:rt-make-closure #'identity '(a b c))))
    (assert-eq 'a (cl-cc/runtime:rt-closure-ref c 0))
    (assert-eq 'b (cl-cc/runtime:rt-closure-ref c 1))
    (assert-eq 'c (cl-cc/runtime:rt-closure-ref c 2))))

(deftest rt-call-fn-closure
  "rt-call-fn calls through a closure object."
  (let ((c (cl-cc/runtime:rt-make-closure (lambda (x) (* x 2)) nil)))
    (assert-= 10 (cl-cc/runtime:rt-call-fn c 5))))

(deftest rt-call-fn-plain-function
  "rt-call-fn works with a plain function."
  (assert-= 7 (cl-cc/runtime:rt-call-fn #'+ 3 4)))

(deftest rt-apply-fn-closure
  "rt-apply-fn applies args list through a closure."
  (let ((c (cl-cc/runtime:rt-make-closure (lambda (a b) (+ a b)) nil)))
    (assert-= 15 (cl-cc/runtime:rt-apply-fn c '(10 5)))))

(deftest rt-apply-fn-plain-function
  "rt-apply-fn works with a plain function."
  (assert-= 6 (cl-cc/runtime:rt-apply-fn #'* '(2 3))))

(deftest rt-call-next-method-signals
  "rt-call-next-method signals error when no next method."
  (assert-signals error (cl-cc/runtime:rt-call-next-method)))

(deftest rt-next-method-p-returns-nil
  "rt-next-method-p returns nil (no method stack)."
  (assert-false (cl-cc/runtime:rt-next-method-p)))

;;; ─── Type Predicates (1/0 return convention) ───────────────────────────────

(deftest-each rt-type-predicates
  "Runtime type predicates return 1 for match, 0 otherwise."
  :cases (("consp-t"      #'cl-cc/runtime:rt-consp     '(1 . 2)  1)
          ("consp-f"      #'cl-cc/runtime:rt-consp     42         0)
          ("null-p-t"     #'cl-cc/runtime:rt-null-p    nil        1)
          ("null-p-f"     #'cl-cc/runtime:rt-null-p    42         0)
          ("symbolp-t"    #'cl-cc/runtime:rt-symbolp   'foo       1)
          ("symbolp-f"    #'cl-cc/runtime:rt-symbolp   42         0)
          ("numberp-t"    #'cl-cc/runtime:rt-numberp   3.14       1)
          ("numberp-f"    #'cl-cc/runtime:rt-numberp   "hi"       0)
          ("integerp-t"   #'cl-cc/runtime:rt-integerp  42         1)
          ("integerp-f"   #'cl-cc/runtime:rt-integerp  3.14       0)
          ("floatp-t"     #'cl-cc/runtime:rt-floatp    1.0        1)
          ("floatp-f"     #'cl-cc/runtime:rt-floatp    1          0)
          ("stringp-t"    #'cl-cc/runtime:rt-stringp   "hi"       1)
          ("stringp-f"    #'cl-cc/runtime:rt-stringp   42         0)
          ("characterp-t" #'cl-cc/runtime:rt-characterp #\a       1)
          ("characterp-f" #'cl-cc/runtime:rt-characterp 42        0)
          ("vectorp-t"    #'cl-cc/runtime:rt-vectorp   #(1 2)     1)
          ("vectorp-f"    #'cl-cc/runtime:rt-vectorp   42         0)
          ("listp-t"      #'cl-cc/runtime:rt-listp     '(1)       1)
          ("listp-nil"    #'cl-cc/runtime:rt-listp     nil         1)
          ("listp-f"      #'cl-cc/runtime:rt-listp     42         0)
          ("atomp-t"      #'cl-cc/runtime:rt-atomp     42         1)
          ("atomp-f"      #'cl-cc/runtime:rt-atomp     '(1)       0)
          ("keywordp-t"   #'cl-cc/runtime:rt-keywordp  :foo       1)
          ("keywordp-f"   #'cl-cc/runtime:rt-keywordp  'foo       0)
          ("hash-t"       #'cl-cc/runtime:rt-hash-table-p (make-hash-table) 1)
          ("hash-f"       #'cl-cc/runtime:rt-hash-table-p 42      0))
  (pred-fn input expected)
  (assert-= expected (funcall pred-fn input)))

(deftest rt-functionp-closure
  "rt-functionp returns 1 for closure objects too."
  (let ((c (cl-cc/runtime:rt-make-closure #'identity nil)))
    (assert-= 1 (cl-cc/runtime:rt-functionp c))))

(deftest rt-typep-integer
  "rt-typep checks CL type by name."
  (assert-= 1 (cl-cc/runtime:rt-typep 42 'integer))
  (assert-= 0 (cl-cc/runtime:rt-typep "hi" 'integer)))

(deftest rt-type-of-integer
  "rt-type-of returns the CL type."
  (let ((ty (cl-cc/runtime:rt-type-of 42)))
    (assert-true (subtypep ty 'integer))))

;;; ─── List Operations ───────────────────────────────────────────────────────

(deftest rt-cons-and-accessors
  "rt-cons, rt-car, rt-cdr."
  (let ((c (cl-cc/runtime:rt-cons 1 2)))
    (assert-= 1 (cl-cc/runtime:rt-car c))
    (assert-= 2 (cl-cc/runtime:rt-cdr c))))

(deftest rt-rplaca-rplacd
  "rt-rplaca and rt-rplacd mutate the cons."
  (let ((c (cl-cc/runtime:rt-cons 1 2)))
    (cl-cc/runtime:rt-rplaca c 10)
    (cl-cc/runtime:rt-rplacd c 20)
    (assert-= 10 (cl-cc/runtime:rt-car c))
    (assert-= 20 (cl-cc/runtime:rt-cdr c))))

(deftest rt-pop-list-returns-two-values
  "rt-pop-list returns (car . cdr) as two values."
  (multiple-value-bind (head tail)
      (cl-cc/runtime:rt-pop-list '(a b c))
    (assert-eq 'a head)
    (assert-equal '(b c) tail)))

(deftest rt-push-list-conses
  "rt-push-list conses value onto front."
  (assert-equal '(x a b) (cl-cc/runtime:rt-push-list 'x '(a b))))

(deftest rt-endp-convention
  "rt-endp returns 1 for end, 0 otherwise."
  (assert-= 1 (cl-cc/runtime:rt-endp nil))
  (assert-= 0 (cl-cc/runtime:rt-endp '(1))))

(deftest rt-equal-convention
  "rt-equal returns 1/0."
  (assert-= 1 (cl-cc/runtime:rt-equal '(1 2) '(1 2)))
  (assert-= 0 (cl-cc/runtime:rt-equal '(1 2) '(1 3))))

;;; ─── Array Operations ─────────────────────────────────────────────────────

(deftest rt-make-array-simple
  "rt-make-array creates an array."
  (let ((a (cl-cc/runtime:rt-make-array 5)))
    (assert-= 5 (cl-cc/runtime:rt-array-length a))))

(deftest rt-make-array-with-init
  "rt-make-array with :initial-element."
  (let ((a (cl-cc/runtime:rt-make-array 3 :initial-element 0)))
    (assert-= 0 (cl-cc/runtime:rt-aref a 0))))

(deftest rt-aset-sets-element
  "rt-aset sets array element (indices-then-value convention)."
  (let ((a (cl-cc/runtime:rt-make-array 3 :initial-element 0)))
    (cl-cc/runtime:rt-aset a 1 42)
    (assert-= 42 (cl-cc/runtime:rt-aref a 1))))

(deftest rt-vector-push-and-pop
  "rt-vector-push / rt-vector-pop roundtrip."
  (let ((v (make-array 5 :fill-pointer 0)))
    (cl-cc/runtime:rt-vector-push 10 v)
    (cl-cc/runtime:rt-vector-push 20 v)
    (assert-= 2 (cl-cc/runtime:rt-fill-pointer v))
    (assert-= 20 (cl-cc/runtime:rt-vector-pop v))
    (assert-= 1 (cl-cc/runtime:rt-fill-pointer v))))

(deftest rt-svref-svset
  "rt-svref / rt-svset on simple vectors."
  (let ((v (vector 1 2 3)))
    (assert-= 2 (cl-cc/runtime:rt-svref v 1))
    (cl-cc/runtime:rt-svset v 1 99)
    (assert-= 99 (cl-cc/runtime:rt-svref v 1))))

(deftest rt-bit-array-ops
  "rt-bit-access / rt-bit-set on bit vectors."
  (let ((bv (make-array 4 :element-type 'bit :initial-element 0)))
    (cl-cc/runtime:rt-bit-set bv 2 1)
    (assert-= 1 (cl-cc/runtime:rt-bit-access bv 2))
    (assert-= 0 (cl-cc/runtime:rt-bit-access bv 0))))

;;; ─── Arithmetic Helpers ────────────────────────────────────────────────────

(deftest rt-basic-arithmetic
  "rt-add/sub/mul/div/mod/rem."
  (assert-= 7 (cl-cc/runtime:rt-add 3 4))
  (assert-= -1 (cl-cc/runtime:rt-sub 3 4))
  (assert-= 12 (cl-cc/runtime:rt-mul 3 4))
  (assert-= 5/2 (cl-cc/runtime:rt-div 5 2))
  (assert-= 1 (cl-cc/runtime:rt-mod 7 3))
  (assert-= 1 (cl-cc/runtime:rt-rem 7 3)))

(deftest rt-unary-arithmetic
  "rt-neg/abs/inc/dec."
  (assert-= -5 (cl-cc/runtime:rt-neg 5))
  (assert-= 5 (cl-cc/runtime:rt-abs -5))
  (assert-= 6 (cl-cc/runtime:rt-inc 5))
  (assert-= 4 (cl-cc/runtime:rt-dec 5)))

(deftest rt-not-convention
  "rt-not returns 0 for truthy, 1 for falsy."
  (assert-= 1 (cl-cc/runtime:rt-not nil))
  (assert-= 0 (cl-cc/runtime:rt-not t))
  (assert-= 0 (cl-cc/runtime:rt-not 42)))

(deftest-each rt-numeric-predicates
  "Numeric predicates return 1/0."
  :cases (("evenp-t"  #'cl-cc/runtime:rt-evenp  4  1)
          ("evenp-f"  #'cl-cc/runtime:rt-evenp  3  0)
          ("oddp-t"   #'cl-cc/runtime:rt-oddp   3  1)
          ("oddp-f"   #'cl-cc/runtime:rt-oddp   4  0)
          ("zerop-t"  #'cl-cc/runtime:rt-zerop  0  1)
          ("zerop-f"  #'cl-cc/runtime:rt-zerop  1  0)
          ("plusp-t"  #'cl-cc/runtime:rt-plusp   5  1)
          ("plusp-f"  #'cl-cc/runtime:rt-plusp  -1  0)
          ("minusp-t" #'cl-cc/runtime:rt-minusp -1  1)
          ("minusp-f" #'cl-cc/runtime:rt-minusp  1  0))
  (pred-fn input expected)
  (assert-= expected (funcall pred-fn input)))

;;; ─── Comparisons ───────────────────────────────────────────────────────────

(deftest-each rt-comparisons
  "Comparison helpers return 1/0."
  :cases (("lt-t"     #'cl-cc/runtime:rt-lt     1 2 1)
          ("lt-f"     #'cl-cc/runtime:rt-lt     2 1 0)
          ("gt-t"     #'cl-cc/runtime:rt-gt     2 1 1)
          ("gt-f"     #'cl-cc/runtime:rt-gt     1 2 0)
          ("le-eq"    #'cl-cc/runtime:rt-le     2 2 1)
          ("ge-eq"    #'cl-cc/runtime:rt-ge     2 2 1)
          ("num-eq-t" #'cl-cc/runtime:rt-num-eq 5 5 1)
          ("num-eq-f" #'cl-cc/runtime:rt-num-eq 5 6 0)
          ("eq-t"     #'cl-cc/runtime:rt-eq     :a :a 1)
          ("eq-f"     #'cl-cc/runtime:rt-eq     :a :b 0)
          ("eql-t"    #'cl-cc/runtime:rt-eql    42 42 1)
          ("eql-f"    #'cl-cc/runtime:rt-eql    42 43 0))
  (cmp-fn a b expected)
  (assert-= expected (funcall cmp-fn a b)))

;;; ─── Bitwise ───────────────────────────────────────────────────────────────

(deftest rt-bitwise-ops
  "rt-logand/logior/logxor/lognot/ash."
  (assert-= #b1010 (cl-cc/runtime:rt-logand #b1110 #b1011))
  (assert-= #b1111 (cl-cc/runtime:rt-logior #b1010 #b0101))
  (assert-= #b1111 (cl-cc/runtime:rt-logxor #b1010 #b0101))
  (assert-= -43 (cl-cc/runtime:rt-lognot 42))
  (assert-= 8 (cl-cc/runtime:rt-ash 2 2))
  (assert-= 2 (cl-cc/runtime:rt-ash 8 -2)))

(deftest rt-logtest-convention
  "rt-logtest returns 1 if bits overlap, 0 otherwise."
  (assert-= 1 (cl-cc/runtime:rt-logtest #b1010 #b1000))
  (assert-= 0 (cl-cc/runtime:rt-logtest #b1010 #b0101)))

(deftest rt-logbitp-convention
  "rt-logbitp returns 1/0."
  (assert-= 1 (cl-cc/runtime:rt-logbitp 0 1))
  (assert-= 0 (cl-cc/runtime:rt-logbitp 1 1)))

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

(deftest rt-string-case-ops
  "rt-string-upcase/downcase/capitalize."
  (assert-equal "HELLO" (cl-cc/runtime:rt-string-upcase "hello"))
  (assert-equal "hello" (cl-cc/runtime:rt-string-downcase "HELLO"))
  (assert-equal "Hello World" (cl-cc/runtime:rt-string-capitalize "hello world")))

(deftest rt-string-trim-ops
  "rt-string-trim/left-trim/right-trim."
  (assert-equal "hello" (cl-cc/runtime:rt-string-trim " " " hello "))
  (assert-equal "hello " (cl-cc/runtime:rt-string-left-trim " " " hello "))
  (assert-equal " hello" (cl-cc/runtime:rt-string-right-trim " " " hello ")))

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

(deftest rt-char-case-ops
  "rt-char-upcase/downcase."
  (assert-equal #\A (cl-cc/runtime:rt-char-upcase #\a))
  (assert-equal #\a (cl-cc/runtime:rt-char-downcase #\A)))

;;; ─── Symbol Operations ─────────────────────────────────────────────────────

(deftest rt-symbol-name-returns-string
  "rt-symbol-name returns symbol's name."
  (assert-equal "FOO" (cl-cc/runtime:rt-symbol-name 'foo)))

(deftest rt-make-symbol-uninterned
  "rt-make-symbol creates an uninterned symbol."
  (let ((s (cl-cc/runtime:rt-make-symbol "TEST")))
    (assert-equal "TEST" (symbol-name s))
    (assert-false (symbol-package s))))

(deftest rt-gensym-unique
  "rt-gensym returns unique symbols."
  (let ((a (cl-cc/runtime:rt-gensym))
        (b (cl-cc/runtime:rt-gensym)))
    (assert-false (eq a b))))

(deftest rt-symbol-plist-roundtrip
  "rt-put-prop / rt-get-prop / rt-remprop roundtrip."
  (let ((sym (cl-cc/runtime:rt-make-symbol "PLIST-TEST")))
    (cl-cc/runtime:rt-put-prop sym :color 'red)
    (assert-eq 'red (cl-cc/runtime:rt-get-prop sym :color))
    (cl-cc/runtime:rt-remprop sym :color)
    (assert-false (cl-cc/runtime:rt-get-prop sym :color))))

;;; ─── Hash Table Operations ─────────────────────────────────────────────────

(deftest rt-hash-table-roundtrip
  "rt-make-hash-table / rt-sethash / rt-gethash / rt-remhash."
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :a ht 1)
    (cl-cc/runtime:rt-sethash :b ht 2)
    (assert-= 1 (cl-cc/runtime:rt-gethash :a ht))
    (assert-= 2 (cl-cc/runtime:rt-hash-count ht))
    (cl-cc/runtime:rt-remhash :a ht)
    (assert-= 1 (cl-cc/runtime:rt-hash-count ht))))

(deftest rt-hash-keys-and-values
  "rt-hash-keys / rt-hash-values."
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :x ht 10)
    (cl-cc/runtime:rt-sethash :y ht 20)
    (assert-= 2 (length (cl-cc/runtime:rt-hash-keys ht)))
    (assert-= 2 (length (cl-cc/runtime:rt-hash-values ht)))
    (assert-true (member :x (cl-cc/runtime:rt-hash-keys ht)))
    (assert-true (member 10 (cl-cc/runtime:rt-hash-values ht)))))

(deftest rt-clrhash-empties
  "rt-clrhash empties the hash table."
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :a ht 1)
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

(deftest rt-boundp-fboundp-convention
  "rt-boundp/rt-fboundp return 1/0."
  (assert-= 1 (cl-cc/runtime:rt-fboundp '+))
  (assert-= 0 (cl-cc/runtime:rt-fboundp (gensym "UNBOUND"))))

(deftest rt-coerce-works
  "rt-coerce delegates to CL coerce."
  (assert-equal '(1 2 3) (cl-cc/runtime:rt-coerce #(1 2 3) 'list)))

(deftest rt-parse-integer-basic
  "rt-parse-integer parses simple integer."
  (assert-= 42 (cl-cc/runtime:rt-parse-integer "42")))

(deftest rt-parse-integer-radix
  "rt-parse-integer with radix."
  (assert-= 255 (cl-cc/runtime:rt-parse-integer "FF" :radix 16)))

;;; ─── I/O Wrappers ──────────────────────────────────────────────────────────

(deftest rt-make-string-stream-input
  "rt-make-string-stream creates an input stream."
  (let ((s (cl-cc/runtime:rt-make-string-stream "hello")))
    (assert-equal #\h (cl-cc/runtime:rt-read-char s))))

(deftest rt-make-string-stream-output
  "rt-make-string-stream output and get-string roundtrip."
  (let ((s (cl-cc/runtime:rt-make-string-stream "" :direction :output)))
    (cl-cc/runtime:rt-write-string "world" s)
    (assert-equal "world" (cl-cc/runtime:rt-get-string-from-stream s))))

(deftest rt-string-output-stream-roundtrip
  "rt-make-string-output-stream / rt-stream-write-string / rt-get-output-stream-string."
  (let ((s (cl-cc/runtime:rt-make-string-output-stream)))
    (cl-cc/runtime:rt-stream-write-string s "test")
    (assert-equal "test" (cl-cc/runtime:rt-get-output-stream-string s))))

(deftest rt-stream-predicates
  "rt-input-stream-p / rt-output-stream-p / rt-open-stream-p return 1/0."
  (let ((in (make-string-input-stream "x"))
        (out (make-string-output-stream)))
    (assert-= 1 (cl-cc/runtime:rt-input-stream-p in))
    (assert-= 0 (cl-cc/runtime:rt-input-stream-p out))
    (assert-= 1 (cl-cc/runtime:rt-output-stream-p out))
    (assert-= 0 (cl-cc/runtime:rt-output-stream-p in))
    (assert-= 1 (cl-cc/runtime:rt-open-stream-p in))))

(deftest rt-read-write-char-roundtrip
  "rt-write-char / rt-read-char via string streams."
  (let ((out (make-string-output-stream)))
    (cl-cc/runtime:rt-write-char #\Z out)
    (let ((in (make-string-input-stream (get-output-stream-string out))))
      (assert-equal #\Z (cl-cc/runtime:rt-read-char in)))))

(deftest rt-pathname-component-extraction
  "rt-pathname-component extracts name, type."
  (let ((p (cl-cc/runtime:rt-make-pathname :name "test" :type "lisp")))
    (assert-equal "test" (cl-cc/runtime:rt-pathname-component p :name))
    (assert-equal "lisp" (cl-cc/runtime:rt-pathname-component p :type))))
