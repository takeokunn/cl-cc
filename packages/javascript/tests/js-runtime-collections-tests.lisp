;;;; packages/javascript/tests/js-runtime-collections-tests.lisp
;;;;
;;;; Set, Iterator, Promise, Map, Generator, BigInt, URI/base64,
;;;; and AggregateError/WeakRef runtime tests.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list, %jr-set)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Set built-ins ───────────────────────────────────────────────────────────

(defun %jr-set-like (&rest values)
  (let ((entries (make-array (length values)
                             :element-type t
                             :adjustable t
                             :fill-pointer (length values)
                             :initial-contents values)))
    (cl-cc/javascript::%js-make-object
     "size" (length values)
     "has" (lambda (candidate &rest _)
             (declare (ignore _))
             (loop for value across entries
                   thereis (cl-cc/javascript::%js-same-value-zero value candidate)))
     "keys" (lambda (&rest _)
              (declare (ignore _))
              entries))))

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

(deftest js-rt-set-same-value-zero-values
  "Set values use SameValueZero: NaN matches NaN, and +0/-0 are one value."
  (let ((nan-a cl-cc/javascript::*js-nan-float*)
        (nan-b cl-cc/javascript::+js-nan+)
        (s (cl-cc/javascript::%js-make-set)))
    (cl-cc/javascript::%js-set-add s nan-a)
    (assert-true (cl-cc/javascript::%js-set-has s nan-b))
    (cl-cc/javascript::%js-set-add s nan-b)
    (assert-= 1 (cl-cc/javascript::%js-set-size s))
    (assert-true (cl-cc/javascript::%js-set-delete s nan-b))
    (assert-= 0 (cl-cc/javascript::%js-set-size s))
    (cl-cc/javascript::%js-set-add s 0.0d0)
    (cl-cc/javascript::%js-set-add s -0.0d0)
    (assert-= 1 (cl-cc/javascript::%js-set-size s))
    (assert-true (cl-cc/javascript::%js-set-has s 0.0d0))
    (assert-true (cl-cc/javascript::%js-set-has s -0.0d0))
    (let ((with-nan (%jr-set nan-b 1)))
      (cl-cc/javascript::%js-set-add s nan-a)
      (assert-false (cl-cc/javascript::%js-set-is-disjoint-from s with-nan))
      (assert-true (cl-cc/javascript::%js-set-is-superset-of s (%jr-set nan-b -0.0d0))))))

(deftest js-rt-set-union
  "union produces a set containing elements of both."
  (let* ((a (%jr-set 1 2))
         (b (%jr-set 2 3))
         (u (cl-cc/javascript::%js-set-union a b)))
    (assert-= 3 (cl-cc/javascript::%js-set-size u))
    (assert-true (cl-cc/javascript::%js-set-has u 1))
    (assert-true (cl-cc/javascript::%js-set-has u 3))))

(deftest js-rt-set-union-set-like
  "union accepts a set-like object with size/has/keys."
  (let* ((a (%jr-set 1 2))
         (b (%jr-set-like 2 3))
         (u (cl-cc/javascript::%js-set-union a b)))
    (assert-equal '(1 2 3)
                  (%jr-list (cl-cc/javascript::%js-set-keys u)))))

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

(deftest js-rt-set-filter-ops-set-like
  "intersection/difference/symmetricDifference accept set-like objects."
  (let* ((a (%jr-set 1 2 3))
         (b (%jr-set-like 2 4))
         (intersection (cl-cc/javascript::%js-set-intersection a b))
         (difference (cl-cc/javascript::%js-set-difference a b))
         (symmetric (cl-cc/javascript::%js-set-symmetric-difference a b)))
    (assert-equal '(2)
                  (%jr-list (cl-cc/javascript::%js-set-keys intersection)))
    (assert-equal '(1 3)
                  (%jr-list (cl-cc/javascript::%js-set-keys difference)))
    (assert-equal '(1 3 4)
                  (%jr-list (cl-cc/javascript::%js-set-keys symmetric)))))

(deftest js-rt-set-subset-disjoint
  "is-subset-of and is-disjoint-from."
  (let ((a (%jr-set 1 2))
        (b (%jr-set 1 2 3))
        (c (%jr-set 4 5)))
    (assert-true  (cl-cc/javascript::%js-set-is-subset-of   a b))
    (assert-false (cl-cc/javascript::%js-set-is-subset-of   b a))
    (assert-true  (cl-cc/javascript::%js-set-is-disjoint-from a c))
    (assert-false (cl-cc/javascript::%js-set-is-disjoint-from a b))))

(deftest js-rt-set-predicates-set-like
  "Set predicates accept set-like objects."
  (let ((a (%jr-set 1 2))
        (b (%jr-set-like 1 2 3))
        (c (%jr-set 1 2 3))
        (d (%jr-set-like 2 3))
        (e (%jr-set-like 4 5))
        (f (%jr-set-like 2 5)))
    (assert-true (cl-cc/javascript::%js-set-is-subset-of a b))
    (assert-true (cl-cc/javascript::%js-set-is-superset-of c d))
    (assert-true (cl-cc/javascript::%js-set-is-disjoint-from a e))
    (assert-false (cl-cc/javascript::%js-set-is-disjoint-from a f))))

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

(deftest-each js-rt-iterator-some-every
  "some returns on first truthy element; every returns on first falsy."
  :cases (("some-found"    #'cl-cc/javascript::%js-iterator-some  '(1 3 4) #'evenp t)
          ("some-not"      #'cl-cc/javascript::%js-iterator-some  '(1 3 5) #'evenp nil)
          ("every-true"    #'cl-cc/javascript::%js-iterator-every '(2 4 6) #'evenp t)
          ("every-false"   #'cl-cc/javascript::%js-iterator-every '(2 3 6) #'evenp nil))
  (fn vals pred expected)
  (let ((iter (cl-cc/javascript::%js-vec-to-iter (apply #'%jr-arr vals))))
    (assert-equal expected (funcall fn iter (lambda (x &rest _) (declare (ignore _)) (funcall pred x))))))

(deftest js-rt-iterator-find
  "find returns the first truthy element, or undefined when none match."
  (let* ((found    (cl-cc/javascript::%js-iterator-find
                    (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 3 4 5))
                    (lambda (x &rest _) (declare (ignore _)) (evenp x))))
         (not-found (cl-cc/javascript::%js-iterator-find
                     (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 3 5))
                     (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (assert-= 4 found)
    (assert-eq cl-cc/javascript::+js-undefined+ not-found)))

(deftest js-rt-iterator-for-each
  "for-each applies a side-effectful function to each element."
  (let ((seen nil))
    (cl-cc/javascript::%js-iterator-for-each
     (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10 20 30))
     (lambda (x &rest _) (declare (ignore _)) (push x seen)))
    (assert-equal '(10 20 30) (nreverse seen))))

(deftest js-rt-iterator-flat-map
  "flat-map expands each element into a sub-iterator."
  (let* ((iter   (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3)))
         (result (cl-cc/javascript::%js-iterator-to-array
                  (cl-cc/javascript::%js-iterator-flat-map
                   iter
                   (lambda (x &rest _) (declare (ignore _)) (%jr-arr x (* x 10)))))))
    (assert-equal '(1 10 2 20 3 30) (%jr-list result))))

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

(deftest js-rt-map-same-value-zero-keys
  "Map keys use SameValueZero: NaN matches NaN, and +0/-0 are one key."
  (let ((nan-a cl-cc/javascript::*js-nan-float*)
        (nan-b cl-cc/javascript::+js-nan+)
        (m (cl-cc/javascript::%js-make-map)))
    (cl-cc/javascript::%js-map-set m nan-a "first")
    (assert-true (cl-cc/javascript::%js-map-has m nan-b))
    (assert-string= "first" (cl-cc/javascript::%js-map-get m nan-b))
    (cl-cc/javascript::%js-map-set m nan-b "second")
    (assert-= 1 (cl-cc/javascript::%js-map-size m))
    (assert-string= "second" (cl-cc/javascript::%js-map-get m nan-a))
    (assert-true (cl-cc/javascript::%js-map-delete m nan-b))
    (assert-= 0 (cl-cc/javascript::%js-map-size m))
    (cl-cc/javascript::%js-map-set m 0.0d0 "zero")
    (cl-cc/javascript::%js-map-set m -0.0d0 "neg-zero")
    (assert-= 1 (cl-cc/javascript::%js-map-size m))
    (assert-string= "neg-zero" (cl-cc/javascript::%js-map-get m 0.0d0))
    (assert-string= "neg-zero" (cl-cc/javascript::%js-map-get m -0.0d0))))

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

;;; ─── AggregateError / WeakRef / RegExp.escape ────────────────────────────────

(deftest js-rt-aggregate-error-make
  "%js-make-aggregate-error creates an AggregateError instance with message and errors."
  (let* ((errs (%jr-arr "e1" "e2"))
         (obj  (cl-cc/javascript::%js-make-aggregate-error errs "some errors")))
    (assert-string= "some errors"    (gethash "message" obj))
    (assert-string= "AggregateError" (gethash "name"    obj))
    (assert-eq errs                  (gethash "errors"  obj))
    (assert-true (cl-cc/javascript::%js-instanceof
                  obj cl-cc/javascript::*js-aggregate-error-class*))))

(deftest js-rt-weak-ref-make-deref
  "%js-make-weak-ref / %js-weak-ref-deref round-trips the target."
  (let* ((target   (cl-cc/javascript::%js-make-object "k" 1))
         (wr       (cl-cc/javascript::%js-make-weak-ref target))
         (dereffed (cl-cc/javascript::%js-weak-ref-deref wr)))
    (assert-eq target dereffed)))

(deftest js-rt-weak-ref-deref-method
  "WeakRef.prototype.deref resolves to a bound method."
  (let* ((target (cl-cc/javascript::%js-make-object "k" 1))
         (wr     (cl-cc/javascript::%js-make-weak-ref target))
         (deref  (cl-cc/javascript::%js-get-prop wr "deref")))
    (assert-eq target (funcall deref))))

(deftest js-rt-regexp-escape
  "%js-regexp-escape follows the ES2025 escaping rules."
  (assert-string= "\\x61\\.b\\+c\\?"
                  (cl-cc/javascript::%js-regexp-escape "a.b+c?"))
  (assert-string= "\\x66oo\\x2Dbar"
                  (cl-cc/javascript::%js-regexp-escape "foo-bar"))
  (assert-string= "\\x20\\n\\t"
                  (cl-cc/javascript::%js-regexp-escape (format nil " ~%~C" #\Tab))))

;;; ─── Map iterators — keys / values / entries ─────────────────────────────────

(deftest js-rt-map-keys-iterator
  "%js-map-keys returns a CL iterator yielding keys in insertion order."
  (let* ((m   (cl-cc/javascript::%js-make-map))
         (acc nil))
    (cl-cc/javascript::%js-map-set m "a" 1)
    (cl-cc/javascript::%js-map-set m "b" 2)
    (cl-cc/javascript::%js-for-of
     (cl-cc/javascript::%js-map-keys m)
     (lambda (k) (push k acc)))
    (assert-equal '("b" "a") acc)))

(deftest js-rt-map-values-iterator
  "%js-map-values returns an iterator yielding values in insertion order."
  (let* ((m   (cl-cc/javascript::%js-make-map))
         (acc nil))
    (cl-cc/javascript::%js-map-set m "x" 10)
    (cl-cc/javascript::%js-map-set m "y" 20)
    (cl-cc/javascript::%js-for-of
     (cl-cc/javascript::%js-map-values m)
     (lambda (v) (push v acc)))
    (assert-equal '(20 10) acc)))

(deftest js-rt-map-entries-iterator
  "%js-map-entries returns an iterator yielding [key, value] arrays."
  (let* ((m   (cl-cc/javascript::%js-make-map))
         (acc nil))
    (cl-cc/javascript::%js-map-set m "k" 99)
    (cl-cc/javascript::%js-for-of
     (cl-cc/javascript::%js-map-entries m)
     (lambda (e) (push (list (aref e 0) (aref e 1)) acc)))
    (assert-equal '(("k" 99)) acc)))

;;; ─── WeakMap ─────────────────────────────────────────────────────────────────

(deftest js-rt-weak-map-lifecycle
  "WeakMap: set/get/has/delete on object keys with identity lookup."
  (let* ((wm  (cl-cc/javascript::%js-make-weak-map))
         (key (cl-cc/javascript::%js-make-object "x" 1)))
    (assert-true  (cl-cc/javascript::%js-weak-map-p wm))
    (cl-cc/javascript::%js-weak-map-set wm key 42)
    (assert-= 42 (cl-cc/javascript::%js-weak-map-get wm key))
    (assert-true  (cl-cc/javascript::%js-weak-map-has wm key))
    (assert-true  (cl-cc/javascript::%js-weak-map-delete wm key))
    (assert-false (cl-cc/javascript::%js-weak-map-has wm key))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-weak-map-get wm key))))

;;; ─── WeakSet ─────────────────────────────────────────────────────────────────

(deftest js-rt-weak-set-lifecycle
  "WeakSet: add/has/delete on object keys using identity equality."
  (let* ((ws  (cl-cc/javascript::%js-make-weak-set))
         (obj (cl-cc/javascript::%js-make-object "y" 2)))
    (assert-true  (cl-cc/javascript::%js-weak-set-p ws))
    (cl-cc/javascript::%js-weak-set-add ws obj)
    (assert-true  (cl-cc/javascript::%js-weak-set-has ws obj))
    (assert-false (cl-cc/javascript::%js-weak-set-has ws (cl-cc/javascript::%js-make-object)))
    (cl-cc/javascript::%js-weak-set-delete ws obj)
    (assert-false (cl-cc/javascript::%js-weak-set-has ws obj))))

;;; ─── FinalizationRegistry ────────────────────────────────────────────────────

(deftest js-rt-finalization-registry-register-unregister
  "FinalizationRegistry tracks unregister tokens deterministically."
  (let* ((reg    (cl-cc/javascript::%js-make-finalization-registry (lambda (hv) (declare (ignore hv)))))
         (tgt    (cl-cc/javascript::%js-make-object))
         (token  (cl-cc/javascript::%js-make-object "token" t))
         (token2 (cl-cc/javascript::%js-make-object "token" 2)))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-finreg-register reg tgt "held" token))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-finreg-register reg tgt "held-again" token))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-finreg-register reg tgt "held-without-token"))
    (assert-false (cl-cc/javascript::%js-finreg-unregister reg token2))
    (assert-true  (cl-cc/javascript::%js-finreg-unregister reg token))
    (assert-false (cl-cc/javascript::%js-finreg-unregister reg token))))

(deftest js-rt-finalization-registry-methods
  "FinalizationRegistry.prototype register/unregister resolve to bound methods."
  (let* ((reg        (cl-cc/javascript::%js-make-finalization-registry (lambda (hv) (declare (ignore hv)))))
         (tgt        (cl-cc/javascript::%js-make-object))
         (token      (cl-cc/javascript::%js-make-object))
         (register   (cl-cc/javascript::%js-get-prop reg "register"))
         (unregister (cl-cc/javascript::%js-get-prop reg "unregister")))
    (assert-eq cl-cc/javascript::+js-undefined+ (funcall register tgt "held" token))
    (assert-true  (funcall unregister token))
    (assert-false (funcall unregister token))))
