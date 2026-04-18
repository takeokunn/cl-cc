;;;; tests/unit/runtime/runtime-tests.lisp — Runtime Library Unit Tests
;;;;
;;;; Tests for src/runtime/runtime.lisp: tagged pointers, multiple values buffer,
;;;; closure support, type predicates, list ops, array ops, arithmetic helpers,
;;;; string/char ops, symbol ops, hash table ops, and I/O wrappers.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Tagged Pointers ───────────────────────────────────────────────────────

(deftest-each rt-tag-fixnum
  "rt-tag-fixnum shifts left by 3 bits; tag is 000 for fixnum."
  :cases (("zero"       0   0)
          ("one"        1   8)
          ("forty-two"  42  336))
  (n expected)
  (assert-= expected (cl-cc/runtime:rt-tag-fixnum n)))

(deftest rt-untag-fixnum-roundtrip
  "rt-untag-fixnum reverses rt-tag-fixnum."
  (dolist (n '(0 1 42 -7 1000000))
    (assert-= n (cl-cc/runtime:rt-untag-fixnum (cl-cc/runtime:rt-tag-fixnum n)))))

(deftest-each rt-tag-bits-extracts-low-3
  "rt-tag-bits returns the low 3 bits."
  :cases (("fixnum-8"  8  0)
          ("cons-9"    9  1)
          ("max-15"    15 7)
          ("plain-5"   5  5))
  (tagged expected-bits)
  (assert-= expected-bits (cl-cc/runtime:rt-tag-bits tagged)))

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

(deftest rt-values-buffer-push-ops
  "rt-values-clear resets to empty; rt-values-push increments count."
  (cl-cc/runtime:rt-values-clear)
  (assert-= 0 (cl-cc/runtime:rt-values-count))
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

(deftest-each rt-spread-values-shapes
  "rt-spread-values: list spreads all elements; atom pushes one."
  :cases (("list" '(10 20 30) 3 10)
          ("atom" 42          1 42))
  (input expected-count expected-first)
  (cl-cc/runtime:rt-values-clear)
  (cl-cc/runtime:rt-spread-values input)
  (assert-= expected-count (cl-cc/runtime:rt-values-count))
  (assert-= expected-first (cl-cc/runtime:rt-values-ref 0)))

(deftest-each rt-ensure-values-behavior
  "rt-ensure-values pushes val when empty; is a no-op when buffer already has a value."
  :cases (("empty"     nil 99 1 99)
          ("non-empty" 1   99 1 1))
  (pre-val ensure-val expected-count expected-ref0)
  (cl-cc/runtime:rt-values-clear)
  (when pre-val (cl-cc/runtime:rt-values-push pre-val))
  (cl-cc/runtime:rt-ensure-values ensure-val)
  (assert-= expected-count (cl-cc/runtime:rt-values-count))
  (assert-= expected-ref0 (cl-cc/runtime:rt-values-ref 0)))

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

(deftest-each rt-call-fn-dispatch
  "rt-call-fn dispatches uniformly to closures and plain functions."
  :cases (("closure"  (cl-cc/runtime:rt-make-closure (lambda (x) (* x 2)) nil) '(5)   10)
          ("plain-fn" #'+                                                         '(3 4)  7))
  (fn args expected)
  (assert-= expected (apply #'cl-cc/runtime:rt-call-fn fn args)))

(deftest-each rt-apply-fn-dispatch
  "rt-apply-fn applies args list uniformly to closures and plain functions."
  :cases (("closure"  (cl-cc/runtime:rt-make-closure (lambda (a b) (+ a b)) nil) '(10 5) 15)
          ("plain-fn" #'*                                                          '(2 3)   6))
  (fn args expected)
  (assert-= expected (cl-cc/runtime:rt-apply-fn fn args)))

(deftest rt-next-method-absent
  "With no method stack: rt-next-method-p returns nil; rt-call-next-method signals error."
  (assert-false  (cl-cc/runtime:rt-next-method-p))
  (assert-signals error (cl-cc/runtime:rt-call-next-method)))

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

(deftest-each rt-typep-integer
  "rt-typep checks CL type by name."
  :cases (("match"    42   'integer 1)
          ("no-match" "hi" 'integer 0))
  (val type expected)
  (assert-= expected (cl-cc/runtime:rt-typep val type)))

(deftest rt-type-of-integer
  "rt-type-of returns the CL type."
  (let ((ty (cl-cc/runtime:rt-type-of 42)))
    (assert-true (subtypep ty 'integer))))

