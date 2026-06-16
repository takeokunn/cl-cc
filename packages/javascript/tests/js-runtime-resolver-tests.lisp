;;;; packages/javascript/tests/js-runtime-resolver-tests.lisp
;;;;
;;;; Method dispatch, type resolver coverage (define-js-type-resolver),
;;;; RegExp, Reflect helpers, bound-method, Object property descriptor,
;;;; string char-iter, and Object fallback method table.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── RegExp public API ───────────────────────────────────────────────────────

(deftest js-rt-regex-test-match
  "regex-test returns t when the pattern matches."
  (let ((re (cl-cc/javascript::%js-make-regex "hello")))
    (assert-true  (cl-cc/javascript::%js-regex-test re "say hello world"))
    (assert-false (cl-cc/javascript::%js-regex-test re "goodbye"))))

(deftest js-rt-regex-exec-returns-match
  "regex-exec returns match info object with index and '0' key."
  (let* ((re (cl-cc/javascript::%js-make-regex "lo"))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (assert-false (eq m cl-cc/javascript::+js-null+))
    (assert-string= "lo" (gethash "0" m))
    (assert-= 3 (truncate (gethash "index" m)))))

(deftest js-rt-regex-exec-no-match
  "regex-exec returns null when there is no match."
  (let* ((re (cl-cc/javascript::%js-make-regex "xyz"))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (assert-eq cl-cc/javascript::+js-null+ m)))

;;; ─── Reflect helpers ─────────────────────────────────────────────────────────

(deftest js-rt-reflect-get-set
  "Reflect.get and Reflect.set wrap %js-get-prop/%js-set-prop."
  (let ((obj (cl-cc/javascript::%js-make-object "x" 10)))
    (assert-= 10 (cl-cc/javascript::%js-reflect-get obj "x"))
    (cl-cc/javascript::%js-reflect-set obj "x" 42)
    (assert-= 42 (cl-cc/javascript::%js-reflect-get obj "x"))))

(deftest js-rt-reflect-has
  "Reflect.has returns true when key exists, nil otherwise."
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (assert-true  (cl-cc/javascript::%js-reflect-has obj "a"))
    (assert-false (cl-cc/javascript::%js-reflect-has obj "b"))))

(deftest js-rt-reflect-delete-property
  "Reflect.deleteProperty removes a key from the object."
  (let ((obj (cl-cc/javascript::%js-make-object "k" 99)))
    (cl-cc/javascript::%js-reflect-delete-property obj "k")
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-reflect-get obj "k"))))

(deftest js-rt-reflect-apply
  "Reflect.apply invokes fn with spread args."
  (let* ((fn     (lambda (a b) (+ a b)))
         (result (cl-cc/javascript::%js-reflect-apply fn nil (%jr-arr 3 4))))
    (assert-= 7 result)))

(deftest js-rt-reflect-construct
  "Reflect.construct invokes a constructor function with an args vector."
  (let* ((ctor  (lambda (&rest args) (first args)))
         (args  (%jr-arr 77))
         (obj   (cl-cc/javascript::%js-reflect-construct ctor args)))
    (assert-= 77 obj)))

;;; ─── Object property descriptors ────────────────────────────────────────────

(deftest js-rt-object-define-property
  "Object.defineProperty sets a value key via descriptor."
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 77)))
    (cl-cc/javascript::%js-object-define-property obj "y" desc)
    (assert-= 77 (cl-cc/javascript::%js-reflect-get obj "y"))))

(deftest js-rt-object-get-own-property-descriptor
  "getOwnPropertyDescriptor returns descriptor object for existing key."
  (let* ((obj  (cl-cc/javascript::%js-make-object "v" 5))
         (desc (cl-cc/javascript::%js-object-get-own-property-descriptor obj "v")))
    (assert-= 5 (gethash "value" desc))
    (assert-true (gethash "writable" desc))))

(deftest-each js-rt-get-own-property-descriptors-filters-internals
  "getOwnPropertyDescriptors excludes __proto__, __get_X, __set_X, __class__ etc."
  :cases (("visible"   "real"      t)
          ("proto"     "__proto__" nil)
          ("getter"    "__get_x"   nil)
          ("setter"    "__set_x"   nil))
  (key should-appear)
  (let* ((obj (cl-cc/javascript::%js-make-object "real" 1)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+
          (gethash "__get_x"   obj) (lambda () 0)
          (gethash "__set_x"   obj) (lambda (v) v))
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (found (nth-value 1 (gethash key descs))))
      (assert-equal should-appear found))))

;;; ─── bound-method ────────────────────────────────────────────────────────────

(deftest js-rt-bound-method-found
  "bound-method returns a closure that prepends the receiver."
  (let* ((table (list (cons "double" (lambda (n) (* 2 n)))))
         (bound (cl-cc/javascript::%js-bound-method table 5 "double")))
    (assert-true (functionp bound))
    (assert-= 10 (funcall bound))))

(deftest js-rt-bound-method-not-found
  "bound-method returns +js-undefined+ when the method is not in the table."
  (let* ((table  (list (cons "existing" #'identity)))
         (result (cl-cc/javascript::%js-bound-method table 5 "missing")))
    (assert-eq cl-cc/javascript::+js-undefined+ result)))

;;; ─── Type resolver coverage (define-js-type-resolver) ────────────────────────

(deftest-each js-rt-resolve-regexp-props
  "Regexp resolver returns struct fields and bound-method closures."
  :cases (("source"  "source"  "hello")
          ("flags"   "flags"   "")
          ("global"  "global"  nil))
  (key expected)
  (let* ((re  (cl-cc/javascript::%js-make-regex "hello" ""))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (assert-equal expected val)))

(deftest js-rt-resolve-regexp-test-method
  "Regexp 'test' property resolves to a closure that tests the pattern."
  (let* ((re (cl-cc/javascript::%js-make-regex "hi" ""))
         (fn (cl-cc/javascript::%js-resolve-regexp-method re "test")))
    (assert-true  (funcall fn "say hi there"))
    (assert-false (funcall fn "goodbye"))))

(deftest-each js-rt-resolve-typed-array-props
  "TypedArray resolver returns numeric properties from struct slots."
  :cases (("int32-length"      "Int32Array"   3 "length"     3)
          ("int32-byte-length" "Int32Array"   3 "byteLength" 12)
          ("int32-byte-offset" "Int32Array"   3 "byteOffset" 0)
          ("f16-byte-length"   "Float16Array" 3 "byteLength" 6))
  (type-name length key expected)
  (let* ((ta  (cl-cc/javascript::%js-make-typed-array type-name length))
         (val (cl-cc/javascript::%js-resolve-typed-array-method ta key)))
    (assert-= expected val)))

(deftest-each js-rt-resolve-bigint-methods
  "BigInt resolver returns method closures for toString/toLocaleString."
  :cases (("toString"       "toString"       "42")
          ("toLocaleString" "toLocaleString" "42"))
  (key expected)
  (let* ((bi     (cl-cc/javascript::%make-js-bigint 42))
         (fn     (cl-cc/javascript::%js-resolve-bigint-method bi key))
         (result (funcall fn cl-cc/javascript::+js-undefined+)))
    (assert-string= expected result)))

(deftest js-rt-resolve-bigint-value-of
  "BigInt valueOf returns the BigInt struct itself."
  (let* ((bi (cl-cc/javascript::%make-js-bigint 7))
         (fn (cl-cc/javascript::%js-resolve-bigint-method bi "valueOf")))
    (assert-eq bi (funcall fn))))

;;; ─── String resolver ───────────────────────────────────────────────────────

(deftest js-rt-string-resolver-rejects-deprecated-substr
  "String.prototype.substr is intentionally absent from the method resolver."
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-resolve-string-method "abcdef" "substr")))

;;; ─── String char-iter via get-prop ──────────────────────────────────────────

(deftest js-rt-string-char-iter-yields-chars
  "%js-string-char-iter returns an iterator that yields each char as a 1-char string."
  (let* ((iter (cl-cc/javascript::%js-string-char-iter "abc"))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (r2   (funcall next))
         (r3   (funcall next))
         (done (funcall next)))
    (assert-string= "a" (gethash "value" r1))
    (assert-string= "b" (gethash "value" r2))
    (assert-string= "c" (gethash "value" r3))
    (assert-true        (gethash "done"  done))))

(deftest js-rt-string-char-iter-via-get-prop
  "String @@iterator method resolves to an iterator using %js-string-char-iter."
  (let* ((fn   (cl-cc/javascript::%js-get-prop "xy" "@@iterator"))
         (iter (funcall fn))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (r2   (funcall next))
         (done (funcall next)))
    (assert-string= "x" (gethash "value" r1))
    (assert-string= "y" (gethash "value" r2))
    (assert-true        (gethash "done"  done))))

;;; ─── Object fallback method table ────────────────────────────────────────────

(deftest-each js-rt-object-fallback-methods
  "Object fallback methods are available via %js-resolve-object-method."
  :cases (("has-own-existing"  "hasOwnProperty"       "x"   t)
          ("has-own-missing"   "hasOwnProperty"       "z"   nil)
          ("prop-is-enum-yes"  "propertyIsEnumerable" "x"   t)
          ("prop-is-enum-no"   "propertyIsEnumerable" "z"   nil))
  (method key expected)
  (let* ((obj (cl-cc/javascript::%js-make-object "x" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj method)))
    (assert-true  (functionp fn))
    (assert-equal expected (funcall fn key))))

(deftest js-rt-object-fallback-to-string
  "Object fallback toString returns \"[object Object]\"."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "toString")))
    (assert-string= "[object Object]" (funcall fn))))

(deftest js-rt-object-fallback-value-of
  "Object fallback valueOf returns the object itself."
  (let* ((obj (cl-cc/javascript::%js-make-object "k" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "valueOf")))
    (assert-eq obj (funcall fn))))

(deftest js-rt-object-fallback-constructor
  "Object fallback constructor returns the object itself."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "constructor")))
    (assert-eq obj (funcall fn))))

(deftest js-rt-object-fallback-stored-wins
  "When obj has a stored key the stored value is returned, not the fallback."
  (let* ((obj    (cl-cc/javascript::%js-make-object "toString" "custom"))
         (result (cl-cc/javascript::%js-resolve-object-method obj "toString")))
    (assert-string= "custom" result)))

(deftest js-rt-object-fallback-unknown-returns-undefined
  "Unknown method names return +js-undefined+."
  (let* ((obj    (cl-cc/javascript::%js-make-object))
         (result (cl-cc/javascript::%js-resolve-object-method obj "nonExistent")))
    (assert-eq cl-cc/javascript::+js-undefined+ result)))

;;; ─── Object.is — NaN/zero special cases ─────────────────────────────────────

(deftest-each js-rt-object-is
  "Object.is(a, b) treats NaN-equal-NaN as true, +0/-0 as distinct."
  :cases (("nan-nan"   :js-nan        :js-nan        t)   ; NaN is NaN: true
          ("nan-float" :js-nan        1.0d0          nil)  ; NaN is 1.0: false
          ("+0/-0"     0.0d0         -0.0d0          nil)  ; +0 is -0: false
          ("-0/+0"    -0.0d0          0.0d0          nil)  ; -0 is +0: false
          ("same-num"  3.0d0          3.0d0          t)
          ("same-str"  "x"            "x"            t)
          ("diff-str"  "a"            "b"            nil))
  (a b expected)
  (assert-equal expected (cl-cc/javascript::%js-object-is a b)))

;;; ─── ToString: float formatting + Infinity + BigInt ──────────────────────────

(deftest-each js-rt-to-string-extended
  "ToString handles floats (no trailing .0), Infinity, BigInt, and arrays."
  :cases (("int-float"    7.0d0                            "7")
          ("float-point"  3.14d0                           "3.14")
          ("infinity"     cl-cc/javascript::+js-infinity+  "Infinity")
          ("neg-inf"      cl-cc/javascript::+js-neg-infinity+ "-Infinity")
          ("float-nan"    cl-cc/javascript::*js-nan-float* "NaN")
          ("bigint"       (cl-cc/javascript::%make-js-bigint 99) "99"))
  (value expected)
  (assert-string= expected (cl-cc/javascript::%js-to-string value)))

;;; ─── Template string joining ─────────────────────────────────────────────────

(deftest js-rt-template-string-join
  "%js-template-string concatenates parts, coercing each to string."
  (assert-string= "hello 42 world"
                  (cl-cc/javascript::%js-template-string '("hello " 42 " world")))
  (assert-string= "true and false"
                  (cl-cc/javascript::%js-template-string '(t " and " nil))))

;;; ─── structuredClone / deep-clone ────────────────────────────────────────────

(deftest js-rt-deep-clone-array
  "%js-deep-clone copies arrays element-by-element (no shared structure)."
  (let* ((orig  (%jr-arr 1 2 3))
         (clone (cl-cc/javascript::%js-deep-clone orig)))
    (assert-true (cl-cc/javascript::%js-vec-p clone))
    (assert-= (length orig) (length clone))
    (assert-= 1 (aref clone 0))
    (assert-false (eq orig clone))))

(deftest js-rt-deep-clone-object
  "%js-deep-clone copies hash-table objects (no shared structure)."
  (let* ((orig  (cl-cc/javascript::%js-make-object "x" 10))
         (clone (cl-cc/javascript::%js-deep-clone orig)))
    (assert-true (cl-cc/javascript::%js-ht-p clone))
    (assert-= 10 (gethash "x" clone))
    (assert-false (eq orig clone))))
