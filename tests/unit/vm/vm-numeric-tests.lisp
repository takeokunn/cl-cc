;;;; tests/unit/vm/vm-numeric-tests.lisp — VM numeric tower instruction tests

(in-package :cl-cc/test)

(defsuite vm-numeric-suite
  :description "Unit tests for src/vm/vm-numeric.lisp"
  :parent cl-cc-suite)

(in-suite vm-numeric-suite)

(defun %vm-num-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %vm-num-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(deftest-each vm-type-of
  "vm-type-of returns the correct type symbol."
  :cases (("nil"    nil            'null)
          ("fixnum" 42             'fixnum)
          ("bignum"  (+ most-positive-fixnum 1) 'bignum)
          ("ratio"   1/2           'ratio)
          ("float"  1.0f0          'single-float)
          ("str"    "hello"      'string)
          ("char"   #\a            'character)
          ("sym"    'foo           'symbol)
          ("cons"   '(a)           'cons)
          ("pathname" #p"/tmp"     'pathname)
          ("random-state" (make-random-state t) 'random-state)
          ("readtable" *readtable* 'readtable))
  (src expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc::make-vm-type-of :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

(deftest-each vm-float-convert-and-scale
  "Float conversion and scale operations produce the expected results."
  :cases (("convert" #'cl-cc::make-vm-float-inst 42 42.0)
          ("scale"   #'cl-cc::make-vm-scale-float 1.0 8.0))
  (ctor src expected)
  (if (eq ctor #'cl-cc::make-vm-scale-float)
      (assert-= expected (%vm-num-binary ctor src 3))
      (assert-= expected (%vm-num-unary ctor src))))

(deftest-each vm-float-decode-values
  "vm-decode-float and vm-integer-decode-float both store exactly 3 multiple values."
  :cases (("decode-float"         #'cl-cc::make-vm-decode-float)
          ("integer-decode-float" #'cl-cc::make-vm-integer-decode-float))
  (ctor)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 1.0)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (assert-= 3 (length (cl-cc:vm-values-list s)))))

(deftest-each vm-float-rounding
  "Float rounding operations store quotient and set values-list."
  :cases (("ffloor"     #'cl-cc::make-vm-ffloor)
          ("fceiling"   #'cl-cc::make-vm-fceiling)
          ("ftruncate"  #'cl-cc::make-vm-ftruncate)
          ("fround"     #'cl-cc::make-vm-fround))
  (ctor)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7.0)
    (cl-cc:vm-reg-set s 2 2.0)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (assert-true (numberp (cl-cc:vm-reg-get s 0)))
    (assert-= 2 (length (cl-cc:vm-values-list s)))))

(deftest vm-rational
  "vm-rational converts 0.5 to 1/2."
  (assert-equal 1/2 (%vm-num-unary #'cl-cc::make-vm-rational 0.5)))

(deftest-each vm-rational-parts
  "vm-numerator/denominator extract the correct part of 3/4."
  :cases (("numerator"   #'cl-cc::make-vm-numerator   3)
          ("denominator" #'cl-cc::make-vm-denominator 4))
  (make-inst expected)
  (assert-= expected (%vm-num-unary make-inst 3/4)))

(deftest-each vm-gcd-lcm
  "vm-gcd and vm-lcm compute correct results for known inputs."
  :cases (("gcd" #'cl-cc::make-vm-gcd 12 8  4)
          ("lcm" #'cl-cc::make-vm-lcm  4 6 12))
  (make-inst a b expected)
  (assert-= expected (%vm-num-binary make-inst a b)))

(deftest vm-complex-construct
  "vm-complex constructs #C(3 4)."
  (assert-equal #C(3 4) (%vm-num-binary #'cl-cc::make-vm-complex 3 4)))

(deftest-each vm-complex-parts
  "vm-realpart/imagpart extract the correct component of #C(3 4)."
  :cases (("realpart" #'cl-cc::make-vm-realpart 3)
          ("imagpart" #'cl-cc::make-vm-imagpart 4))
  (make-inst expected)
  (assert-= expected (%vm-num-unary make-inst #C(3 4))))

(deftest vm-conjugate
  "vm-conjugate of #C(3 4) is #C(3 -4)."
  (assert-equal #C(3 -4) (%vm-num-unary #'cl-cc::make-vm-conjugate #C(3 4))))

(deftest vm-boundp
  "vm-boundp returns nil for unbound symbol and t after binding."
  (assert-null (%vm-num-unary #'cl-cc::make-vm-boundp 'test-prim-var))
  (let ((s (make-test-vm)))
    (setf (gethash 'test-prim-var (cl-cc:vm-global-vars s)) 42)
    (cl-cc:vm-reg-set s 1 'test-prim-var)
    (exec1 (cl-cc::make-vm-boundp :dst 0 :src 1) s)
    (assert-equal t (cl-cc:vm-reg-get s 0))))

(deftest vm-makunbound
  "vm-makunbound removes global binding."
  (let ((s (make-test-vm)))
    (setf (gethash 'test-prim-var (cl-cc:vm-global-vars s)) 42)
    (cl-cc:vm-reg-set s 1 'test-prim-var)
    (exec1 (cl-cc::make-vm-makunbound :dst 0 :src 1) s)
    (assert-false (nth-value 1 (gethash 'test-prim-var (cl-cc:vm-global-vars s))))))

(deftest vm-fboundp-unbound
  "vm-fboundp returns nil for unbound function."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'test-prim-fn)
    (exec1 (cl-cc::make-vm-fboundp :dst 0 :src 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

(deftest vm-fmakunbound
  "vm-fmakunbound removes function binding."
  (let ((s (make-test-vm)))
    (setf (gethash 'test-prim-fn (cl-cc:vm-function-registry s)) :some-closure)
    (cl-cc:vm-reg-set s 1 'test-prim-fn)
    (exec1 (cl-cc::make-vm-fmakunbound :dst 0 :src 1) s)
    (assert-false (nth-value 1 (gethash 'test-prim-fn (cl-cc:vm-function-registry s))))))

(deftest vm-random-in-range
  "vm-random returns a number in [0, 100)."
  (let ((result (%vm-num-unary #'cl-cc::make-vm-random 100)))
    (assert-true (integerp result))
    (assert-true (>= result 0))
    (assert-true (< result 100))))

(deftest vm-make-random-state
  "vm-make-random-state with t creates a fresh random state."
  (assert-true (random-state-p (%vm-num-unary #'cl-cc::make-vm-make-random-state t))))

(deftest-each vm-get-time
  "Time query instructions return non-negative integers."
  :cases (("universal-time"    #'cl-cc::make-vm-get-universal-time     #'>)
          ("internal-realtime" #'cl-cc::make-vm-get-internal-real-time #'>=))
  (ctor cmp)
  (let ((s (make-test-vm)))
    (exec1 (funcall ctor :dst 0) s)
    (assert-true (funcall cmp (cl-cc:vm-reg-get s 0) 0))))

(deftest vm-decode-universal-time-values
  "vm-decode-universal-time stores 9 values."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (get-universal-time))
    (exec1 (cl-cc::make-vm-decode-universal-time :dst 0 :src 1) s)
    (assert-= 9 (length (cl-cc:vm-values-list s)))))
