;;;; packages/javascript/tests/js-runtime-object-ops-tests.lisp
;;;;
;;;; Unit tests for runtime-object.lisp (Object static methods, prototype ops,
;;;; destructuring helpers) and runtime-ops.lisp (bitwise ops, shifts, BigInt
;;;; extras, URI encoding, accessor/misc stubs).
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Internal key filter ─────────────────────────────────────────────────────

(deftest-each js-rt-internal-key-p
  "%js-internal-key-p correctly classifies double-underscore and accessor keys."
  :cases (("dunder-proto"  "__proto__"   t)
          ("dunder-class"  "__class__"   t)
          ("get-prefix"    "__get_foo"   t)
          ("set-prefix"    "__set_foo"   t)
          ("plain-key"     "name"        nil)
          ("empty"         ""            nil))
  (k expected)
  (assert-equal expected (cl-cc/javascript::%js-internal-key-p k)))

;;; ─── Object.keys / values / entries ─────────────────────────────────────────

(deftest js-rt-object-keys-excludes-internals
  "Object.keys skips __proto__/__class__ and returns only enumerable string keys."
  (let* ((obj (cl-cc/javascript::%js-make-object "a" 1 "b" 2)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+)
    (let ((keys (sort (coerce (cl-cc/javascript::%js-object-keys obj) 'list) #'string<)))
      (assert-equal '("a" "b") keys))))

(deftest js-rt-object-values
  "Object.values returns the enumerable values in insertion order."
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 10 "y" 20))
         (values (sort (coerce (cl-cc/javascript::%js-object-values obj) 'list) #'<)))
    (assert-equal '(10 20) values)))

(deftest js-rt-object-entries
  "Object.entries returns [key, value] pairs as two-element arrays."
  (let* ((obj     (cl-cc/javascript::%js-make-object "k" 99))
         (entries (cl-cc/javascript::%js-object-entries obj)))
    (assert-= 1 (length entries))
    (let ((pair (aref entries 0)))
      (assert-string= "k"  (aref pair 0))
      (assert-=       99   (aref pair 1)))))

;;; ─── Object.assign ───────────────────────────────────────────────────────────

(deftest js-rt-object-assign-merges
  "Object.assign copies own enumerable properties of sources into target."
  (let* ((target (cl-cc/javascript::%js-make-object "a" 1))
         (src1   (cl-cc/javascript::%js-make-object "b" 2))
         (src2   (cl-cc/javascript::%js-make-object "c" 3))
         (result (cl-cc/javascript::%js-object-assign target src1 src2)))
    (assert-eq target result)
    (assert-= 1 (gethash "a" target))
    (assert-= 2 (gethash "b" target))
    (assert-= 3 (gethash "c" target))))

(deftest js-rt-object-spread-set-returns-obj
  "%js-object-spread-set sets a key and returns the object."
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (let ((ret (cl-cc/javascript::%js-object-spread-set obj "b" 42)))
      (assert-eq obj ret)
      (assert-= 42 (gethash "b" obj)))))

;;; ─── Object.create / prototype ops ──────────────────────────────────────────

(deftest js-rt-object-create-with-proto
  "Object.create links __proto__ to the provided prototype."
  (let* ((proto (cl-cc/javascript::%js-make-object "method" t))
         (obj   (cl-cc/javascript::%js-object-create proto)))
    (assert-eq proto (cl-cc/javascript::%js-object-get-prototype-of obj))))

(deftest js-rt-object-create-null-proto
  "Object.create(null) produces an object with no prototype."
  (let ((obj (cl-cc/javascript::%js-object-create cl-cc/javascript::+js-null+)))
    (assert-eq cl-cc/javascript::+js-null+
               (cl-cc/javascript::%js-object-get-prototype-of obj))))

(deftest js-rt-object-set-prototype-of
  "setPrototypeOf replaces the __proto__ entry."
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 1))
         (proto2 (cl-cc/javascript::%js-make-object "tag" "v2")))
    (cl-cc/javascript::%js-object-set-prototype-of obj proto2)
    (assert-eq proto2 (cl-cc/javascript::%js-object-get-prototype-of obj))))

;;; ─── Object.hasOwn ───────────────────────────────────────────────────────────

(deftest-each js-rt-object-has-own
  "Object.hasOwn returns t for own keys and nil for absent ones."
  :cases (("present" "a" t)
          ("absent"  "z" nil))
  (key expected)
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (assert-equal expected (cl-cc/javascript::%js-object-has-own obj key))))

;;; ─── Object.fromEntries ──────────────────────────────────────────────────────

(deftest js-rt-object-from-entries
  "Object.fromEntries builds an object from an array of [key,val] pairs."
  (let* ((pairs (cl-cc/javascript::%js-make-array (%jr-arr "x" 10)
                                                   (%jr-arr "y" 20)))
         (obj   (cl-cc/javascript::%js-object-from-entries pairs)))
    (assert-= 10 (gethash "x" obj))
    (assert-= 20 (gethash "y" obj))))

;;; ─── Object.withoutKeys ──────────────────────────────────────────────────────

(deftest js-rt-object-without-keys
  "%js-object-without-keys returns a copy excluding the specified keys."
  (let* ((obj  (cl-cc/javascript::%js-make-object "a" 1 "b" 2 "c" 3))
         (excl (%jr-arr "b"))
         (copy (cl-cc/javascript::%js-object-without-keys obj excl)))
    (assert-false (eq obj copy))
    (assert-= 1 (gethash "a" copy))
    (assert-false (nth-value 1 (gethash "b" copy)))
    (assert-= 3 (gethash "c" copy))))

;;; ─── Object.groupBy ──────────────────────────────────────────────────────────

(deftest js-rt-object-group-by
  "%js-object-group-by partitions an iterable by key-fn result."
  (let* ((items  (%jr-arr 1 2 3 4))
         (key-fn (lambda (x) (if (evenp x) "even" "odd")))
         (grouped (cl-cc/javascript::%js-object-group-by items key-fn)))
    (assert-= 2 (length (gethash "even" grouped)))
    (assert-= 2 (length (gethash "odd"  grouped)))))

;;; ─── Destructuring helpers ───────────────────────────────────────────────────

(deftest js-rt-destructure-array-rest
  "%js-destructure-array in :rest mode collects tail elements."
  (let* ((arr  (%jr-arr 10 20 30 40))
         (rest (cl-cc/javascript::%js-destructure-array arr 1 :rest)))
    (assert-= 3 (length rest))
    (assert-= 20 (aref rest 0))
    (assert-= 40 (aref rest 2))))

(deftest js-rt-destructure-array-value-mode
  "%js-destructure-array in value mode returns each element or its default."
  (let* ((arr (cl-cc/javascript::%js-make-array 10))
         (result (cl-cc/javascript::%js-destructure-array arr 0 99 1 42)))
    (assert-= 10 (first result))
    (assert-= 42 (second result))))

(deftest js-rt-destructure-object-rest
  "%js-destructure-object in :rest mode omits excluded keys."
  (let* ((obj    (cl-cc/javascript::%js-make-object "a" 1 "b" 2 "c" 3))
         (others (cl-cc/javascript::%js-destructure-object obj :rest "a")))
    (assert-false (nth-value 1 (gethash "a" others)))
    (assert-= 2 (gethash "b" others))
    (assert-= 3 (gethash "c" others))))

(deftest js-rt-destructure-object-value-mode
  "%js-destructure-object in value mode extracts keys with defaults."
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 7))
         (result (cl-cc/javascript::%js-destructure-object obj "x" 0 "y" 99)))
    (assert-= 7  (first result))
    (assert-= 99 (second result))))

;;; ─── 32-bit integer coercion ─────────────────────────────────────────────────

(deftest-each js-rt-to-int32
  "%js-to-int32 truncates and masks to 32-bit unsigned."
  :cases (("small"    5    5)
          ("float"    3.7d0 3)
          ("over-32"  #x100000001  1))
  (x expected)
  (assert-= expected (cl-cc/javascript::%js-to-int32 x)))

(deftest-each js-rt-sign-extend32
  "%js-sign-extend32 turns the high-bit set pattern into a negative integer."
  :cases (("positive"   5            5)
          ("max-int32"  #x7FFFFFFF   2147483647)
          ("min-int32"  #x80000000  -2147483648))
  (n expected)
  (assert-= expected (cl-cc/javascript::%js-sign-extend32 n)))

;;; ─── Bitwise operators ───────────────────────────────────────────────────────

(deftest-each js-rt-bitwise-binops
  "Bitwise AND/OR/XOR produce correct 32-bit signed results."
  :cases (("and"  #'cl-cc/javascript::%js-bitwise-and  #b1010  #b1100  #b1000)
          ("or"   #'cl-cc/javascript::%js-bitwise-or   #b1010  #b1100  #b1110)
          ("xor"  #'cl-cc/javascript::%js-bitwise-xor  #b1010  #b1100  #b0110))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

(deftest js-rt-bitwise-not
  "%js-bitwise-not inverts all 32 bits and sign-extends."
  (assert-= -1 (cl-cc/javascript::%js-bitwise-not 0))
  (assert-= -6 (cl-cc/javascript::%js-bitwise-not 5)))

;;; ─── Shift operators ─────────────────────────────────────────────────────────

(deftest-each js-rt-shift-ops
  "Shift left/right/unsigned-right produce correct results."
  :cases (("shl"   #'cl-cc/javascript::%js-shift-left          1  4   16)
          ("shr"   #'cl-cc/javascript::%js-shift-right        -8  1   -4)
          ("ushr"  #'cl-cc/javascript::%js-unsigned-shift-right -1  28  15))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

;;; ─── Unary / increment ops ───────────────────────────────────────────────────

(deftest js-rt-unary-plus
  "%js-unary-plus coerces its argument to a number."
  (assert-= 42   (cl-cc/javascript::%js-unary-plus "42"))
  (assert-= 0    (cl-cc/javascript::%js-unary-plus nil)))

(deftest-each js-rt-inc-dec-ops
  "Prefix inc/dec add 1; postfix ops return the original value."
  :cases (("prefix-inc"  #'cl-cc/javascript::%js-prefix-inc  5   6)
          ("prefix-dec"  #'cl-cc/javascript::%js-prefix-dec  5   4)
          ("postfix-inc" #'cl-cc/javascript::%js-postfix-inc 5   5)
          ("postfix-dec" #'cl-cc/javascript::%js-postfix-dec 5   5))
  (fn val expected)
  (assert-= expected (funcall fn val)))

;;; ─── BigInt extras ───────────────────────────────────────────────────────────

(deftest-each js-rt-bigint-constructor
  "%js-bigint coerces integers, floats, and strings to BigInt structs."
  :cases (("integer" 42       42)
          ("float"   3.9d0    3)
          ("string"  "100"  100))
  (x expected)
  (let ((bi (cl-cc/javascript::%js-bigint x)))
    (assert-true (cl-cc/javascript::js-bigint-p bi))
    (assert-= expected (cl-cc/javascript::js-bigint-value bi))))

(deftest-each js-rt-bigint-to-string-radix
  "%js-bigint-to-string renders BigInt value in the requested base."
  :cases (("decimal" 255 10  "255")
          ("hex"     255 16  "ff")
          ("binary"    5  2  "101"))
  (n radix expected)
  (let ((bi (cl-cc/javascript::%make-js-bigint n)))
    (assert-string= expected (cl-cc/javascript::%js-bigint-to-string bi radix))))

(deftest-each js-rt-bigint-div-mod
  "BigInt div and mod perform integer division and remainder."
  :cases (("div"  #'cl-cc/javascript::%js-bigint-div  10  3   3)
          ("mod"  #'cl-cc/javascript::%js-bigint-mod  10  3   1))
  (fn a b expected)
  (let ((result (funcall fn a b)))
    (assert-= expected (cl-cc/javascript::js-bigint-value result))))

(deftest-each js-rt-bigint-compare
  "%js-bigint-compare returns -1/0/1 like cmp."
  :cases (("lt"  3  5  -1)
          ("eq"  5  5   0)
          ("gt"  7  5   1))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%js-bigint-compare a b)))

(deftest-each js-rt-bigint-shift
  "BigInt lshift and rshift use ash semantics."
  :cases (("lshift" #'cl-cc/javascript::%js-bigint-lshift  1  3    8)
          ("rshift" #'cl-cc/javascript::%js-bigint-rshift  8  2    2))
  (fn a n expected)
  (let ((result (funcall fn a n)))
    (assert-= expected (cl-cc/javascript::js-bigint-value result))))

(deftest js-rt-bigint-negate
  "%js-bigint-negate changes the sign of the BigInt value."
  (assert-= -42 (cl-cc/javascript::js-bigint-value
                 (cl-cc/javascript::%js-bigint-negate 42)))
  (assert-= 7   (cl-cc/javascript::js-bigint-value
                 (cl-cc/javascript::%js-bigint-negate -7))))

;;; ─── URI encoding ────────────────────────────────────────────────────────────

(deftest js-rt-encode-uri-component
  "%js-encode-uri-component percent-encodes non-unreserved chars."
  (assert-string= "hello%20world"
                  (cl-cc/javascript::%js-encode-uri-component "hello world"))
  (assert-string= "abc"
                  (cl-cc/javascript::%js-encode-uri-component "abc")))

(deftest js-rt-decode-uri-component
  "%js-decode-uri-component reverses percent-encoding."
  (assert-string= "hello world"
                  (cl-cc/javascript::%js-decode-uri-component "hello%20world")))

;;; ─── Accessor / misc stubs ───────────────────────────────────────────────────

(deftest js-rt-accessor-descriptor
  "%js-accessor produces a descriptor HT with __accessor__, kind, and fn fields."
  (let* ((fn   (lambda () 42))
         (desc (cl-cc/javascript::%js-accessor "get" fn)))
    (assert-true  (gethash "__accessor__" desc))
    (assert-string= "get" (gethash "kind" desc))
    (assert-eq fn (gethash "fn" desc))))

(deftest js-rt-new-target-returns-undefined
  "%js-new-target returns +js-undefined+ outside a constructor."
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-new-target)))

(deftest js-rt-import-meta-stub
  "%js-import-meta returns a stub object with a 'url' key."
  (let ((meta (cl-cc/javascript::%js-import-meta)))
    (assert-true (cl-cc/javascript::%js-ht-p meta))
    (assert-string= "" (gethash "url" meta))))

(deftest js-rt-using-register-identity
  "%js-using-register returns its argument unchanged."
  (let ((r (list 1 2)))
    (assert-eq r (cl-cc/javascript::%js-using-register r))))

;;; ─── runtime-property.lisp: accessor-descriptor-p, put-entry, optional ops ──

(deftest-each js-rt-accessor-descriptor-p
  "%js-accessor-descriptor-p distinguishes accessor HTs from plain values."
  :cases (("get-accessor" (cl-cc/javascript::%js-accessor "get" (lambda () 1)) t)
          ("set-accessor" (cl-cc/javascript::%js-accessor "set" (lambda (v) v))  t)
          ("plain-ht"     (cl-cc/javascript::%js-make-object "x" 1)             nil)
          ("string"       "not-an-accessor"                                      nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-accessor-descriptor-p val)))

(deftest js-rt-object-put-entry-accessor-routing
  "%js-object-put-entry routes getter descriptor to __get_KEY slot."
  (let* ((ht  (cl-cc/javascript::%js-make-ht))
         (fn  (lambda () 42))
         (desc (cl-cc/javascript::%js-accessor "get" fn)))
    (cl-cc/javascript::%js-object-put-entry ht "foo" desc)
    (assert-eq fn (gethash "__get_foo" ht))
    (assert-false (nth-value 1 (gethash "foo" ht)))))

(deftest-each js-rt-optional-call
  "%js-optional-call invokes the function or returns +js-undefined+ for null/undefined."
  :cases (("real-fn"     (lambda () 99)                   99)
          ("undefined"   cl-cc/javascript::+js-undefined+ :undef)
          ("null"        cl-cc/javascript::+js-null+       :undef))
  (func expected)
  (let ((result (cl-cc/javascript::%js-optional-call func)))
    (if (eq expected :undef)
        (assert-eq cl-cc/javascript::+js-undefined+ result)
        (assert-= expected result))))

(deftest js-rt-optional-method-call-present
  "%js-optional-method-call calls the method when obj is not null/undefined."
  (let* ((obj    (cl-cc/javascript::%js-make-object "double" (lambda (n) (* 2 n))))
         (result (cl-cc/javascript::%js-optional-method-call obj "double" 5)))
    (assert-= 10 result)))

(deftest js-rt-optional-method-call-null
  "%js-optional-method-call returns +js-undefined+ when obj is null."
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-optional-method-call cl-cc/javascript::+js-null+ "double" 5)))

(deftest js-rt-concat-string-coercion
  "%js-concat coerces to string when either operand is a string."
  (assert-string= "42"    (cl-cc/javascript::%js-concat 4 "2"))
  (assert-string= "ab"    (cl-cc/javascript::%js-concat "a" "b"))
  (assert-=       6       (cl-cc/javascript::%js-concat 4 2)))
