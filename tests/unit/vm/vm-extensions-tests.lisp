;;;; tests/unit/vm/vm-extensions-tests.lisp — VM language extension instruction tests

(in-package :cl-cc/test)

(defsuite vm-extensions-suite
  :description "Unit tests for src/vm/vm-extensions.lisp"
  :parent cl-cc-suite)

(in-suite vm-extensions-suite)

(defun %vm-ext-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %vm-ext-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(deftest vm-symbol-get-default
  "vm-symbol-get returns default when property absent."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'prim-test-sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 :none)
    (exec1 (cl-cc::make-vm-symbol-get :dst 0 :sym 1 :indicator 2 :default 3) s)
    (assert-equal :none (cl-cc:vm-reg-get s 0))))

(deftest vm-symbol-set-and-get
  "vm-symbol-set then vm-symbol-get round-trips."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'prim-test-sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 'red)
    (cl-cc:vm-reg-set s 4 nil)
    (exec1 (cl-cc::make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (exec1 (cl-cc::make-vm-symbol-get :dst 5 :sym 1 :indicator 2 :default 4) s)
    (assert-equal 'red (cl-cc:vm-reg-get s 5))))

(deftest vm-remprop
  "vm-remprop removes a property and returns t."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'prim-test-sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 'red)
    (exec1 (cl-cc::make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (exec1 (cl-cc::make-vm-remprop :dst 0 :sym 1 :indicator 2) s)
    (assert-equal t (cl-cc:vm-reg-get s 0))))

(deftest vm-symbol-plist-empty
  "vm-symbol-plist returns nil for symbol with no properties."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'prim-test-fresh-sym)
    (exec1 (cl-cc::make-vm-symbol-plist :dst 0 :src 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

(deftest vm-progv-enter-exit
  "vm-progv-enter binds globals and vm-progv-exit restores them."
  (let ((s (make-test-vm)))
    (setf (gethash 'prim-pv-x (cl-cc:vm-global-vars s)) 10)
    (cl-cc:vm-reg-set s 1 '(prim-pv-x prim-pv-y))
    (cl-cc:vm-reg-set s 2 '(99 100))
    (exec1 (cl-cc::make-vm-progv-enter :dst 0 :syms 1 :vals 2) s)
    (assert-= 99 (gethash 'prim-pv-x (cl-cc:vm-global-vars s)))
    (assert-= 100 (gethash 'prim-pv-y (cl-cc:vm-global-vars s)))
    (exec1 (cl-cc::make-vm-progv-exit :saved 0) s)
    (assert-= 10 (gethash 'prim-pv-x (cl-cc:vm-global-vars s)))
    (assert-false (nth-value 1 (gethash 'prim-pv-y (cl-cc:vm-global-vars s))))))

(deftest-each vm-generic-arith
  "Generic arithmetic instructions compute correct results."
  :cases (("add"  #'cl-cc::make-vm-generic-add  10 3   13)
          ("sub"  #'cl-cc::make-vm-generic-sub  10 3   7)
          ("mul"  #'cl-cc::make-vm-generic-mul  10 3   30)
          ("div"  #'cl-cc::make-vm-generic-div  10 3   3))
  (ctor lhs rhs expected)
  (assert-= expected (%vm-ext-binary ctor lhs rhs)))

(deftest-each vm-generic-comparison
  "Generic comparison instructions: eq/lt/gt over known values."
  :cases (("eq-equal"    #'cl-cc::make-vm-generic-eq "hello" "hello" t)
          ("eq-unequal"  #'cl-cc::make-vm-generic-eq "hello" "world" nil)
          ("lt-true"     #'cl-cc::make-vm-generic-lt 3       5       t)
          ("gt-false"    #'cl-cc::make-vm-generic-gt 3       5       nil))
  (ctor lhs rhs expected)
  (if expected
      (assert-equal t (%vm-ext-binary ctor lhs rhs))
      (assert-null (%vm-ext-binary ctor lhs rhs))))

(deftest vm-generic-div-by-zero
  "vm-generic-div signals error on division by zero."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 10)
    (cl-cc:vm-reg-set s 2 0)
    (assert-signals error
      (exec1 (cl-cc::make-vm-generic-div :dst 0 :lhs 1 :rhs 2) s))))
