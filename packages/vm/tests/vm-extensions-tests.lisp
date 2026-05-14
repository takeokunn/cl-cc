;;;; tests/unit/vm/vm-extensions-tests.lisp — VM language extension instruction tests

(in-package :cl-cc/test)

(defsuite vm-extensions-suite
  :description "Unit tests for src/vm/vm-extensions.lisp"
  :parent cl-cc-unit-suite)

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
  (let ((s (make-test-vm))
        (sym (gensym "VM-SYMBOL-GET-DEFAULT-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 :none)
    (exec1 (cl-cc:make-vm-symbol-get :dst 0 :sym 1 :indicator 2 :default 3) s)
    (assert-equal :none (cl-cc:vm-reg-get s 0))))

(deftest vm-symbol-set-and-get-roundtrip-with-host-sync
  "vm-symbol-set keeps multi-property roundtrips on the short plist path and syncs the host plist."
  (let ((s (make-test-vm))
        (sym (gensym "VM-SYMBOL-ROUNDTRIP-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 'red)
    (cl-cc:vm-reg-set s 4 :shape)
    (cl-cc:vm-reg-set s 5 'circle)
    (cl-cc:vm-reg-set s 6 nil)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 4 :value 5) s)
    (exec1 (cl-cc:make-vm-symbol-get :dst 7 :sym 1 :indicator 2 :default 6) s)
    (exec1 (cl-cc:make-vm-symbol-get :dst 8 :sym 1 :indicator 4 :default 6) s)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 9 :src 1) s)
    (assert-equal 'red (cl-cc:vm-reg-get s 7))
    (assert-equal 'circle (cl-cc:vm-reg-get s 8))
    (assert-equal 'red (getf (cl-cc:vm-reg-get s 9) :color))
    (assert-equal 'circle (getf (cl-cc:vm-reg-get s 9) :shape))
    (assert-true (consp (cl-cc:vm-reg-get s 9)))
    (assert-equal 'red (getf (symbol-plist sym) :color))
    (assert-equal 'circle (getf (symbol-plist sym) :shape))))

(deftest vm-set-symbol-value
  "vm-set-symbol-value updates the symbol value cell and returns the assigned value."
  (let ((s (make-test-vm)))
    (setf (symbol-value 'prim-test-global) 0)
    (cl-cc:vm-reg-set s 1 'prim-test-global)
    (cl-cc:vm-reg-set s 2 77)
    (exec1 (cl-cc:make-vm-set-symbol-value :dst 0 :lhs 1 :rhs 2) s)
    (assert-= 77 (cl-cc:vm-reg-get s 0))
    (assert-= 77 (symbol-value 'prim-test-global))))

(deftest vm-remprop
  "vm-remprop removes a property and returns t."
  (let ((s (make-test-vm))
        (sym (gensym "VM-REMPROP-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 'red)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (exec1 (cl-cc:make-vm-remprop :dst 0 :sym 1 :indicator 2) s)
    (assert-equal t (cl-cc:vm-reg-get s 0))
    (assert-null (getf (symbol-plist sym) :color))))

(deftest vm-symbol-plist-empty
  "vm-symbol-plist returns nil for symbol with no properties."
  (let ((s (make-test-vm))
        (sym (gensym "VM-SYMBOL-EMPTY-")))
    (cl-cc:vm-reg-set s 1 sym)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 0 :src 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

(deftest vm-set-symbol-plist-overwrites-and-promotes-long-plist
  "vm-set-symbol-plist overwrites stale properties and promotes long plists to the hash path."
  (let* ((s (make-test-vm))
         (sym (gensym "VM-LONG-PLIST-"))
         (long-plist '(:a 1 :b 2 :c 3 :d 4 :e 5)))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :stale)
    (cl-cc:vm-reg-set s 3 :old)
    (cl-cc:vm-reg-set s 4 long-plist)
    (cl-cc:vm-reg-set s 5 nil)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (exec1 (cl-cc:make-vm-set-symbol-plist :dst 6 :sym 1 :plist-reg 4) s)
    (exec1 (cl-cc:make-vm-symbol-get :dst 7 :sym 1 :indicator 2 :default 5) s)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 9 :src 1) s)
    (assert-null (cl-cc:vm-reg-get s 7))
    (assert-equal 1 (getf (cl-cc:vm-reg-get s 9) :a))
    (assert-equal 5 (getf (cl-cc:vm-reg-get s 9) :e))
    (assert-equal 5 (getf (symbol-plist sym) :e))
    (assert-true
     (cl-cc/vm::%vm-symbol-property-entry-p
      (gethash sym (cl-cc/vm::vm-symbol-plists s))))))

(deftest vm-system-property-storage-is-separate
  "VM system properties live outside the user-visible symbol plist."
  (let* ((s (make-test-vm))
         (sym (gensym "VM-SYSTEM-PROPS-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :user)
    (cl-cc:vm-reg-set s 3 :value)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (cl-cc/vm::vm-system-property-set s sym :function 'compiled-fn)
    (cl-cc/vm::vm-system-property-set s sym :type 'function)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 4 :src 1) s)
    (assert-equal :value (getf (cl-cc:vm-reg-get s 4) :user))
    (assert-null (getf (cl-cc:vm-reg-get s 4) :function))
    (assert-equal 'compiled-fn
                  (cl-cc/vm::vm-system-property-get s sym :function))
    (assert-equal 'function
                  (getf (cl-cc/vm::vm-system-property-plist s sym) :type))
    (assert-null (getf (symbol-plist sym) :function))))

(deftest vm-symbol-plist-lock-and-read-barrier-are-usable
  "FR-379 lock/barrier state exists and read snapshots observe atomic updates."
  (let* ((s (make-test-vm))
         (sym (gensym "VM-BARRIER-"))
         (lock (cl-cc/vm::vm-symbol-plist-lock s))
         (barrier-before (cl-cc/vm::vm-symbol-plist-read-barrier s)))
    (assert-true lock)
    (sb-thread:with-mutex (lock)
      (assert-equal barrier-before (cl-cc/vm::vm-symbol-plist-read-barrier s)))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :flag)
    (cl-cc:vm-reg-set s 3 t)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (multiple-value-bind (plist barrier-after)
        (cl-cc/vm::vm-symbol-plist-read-snapshot s sym)
      (assert-true (> barrier-after barrier-before))
      (assert-equal t (getf plist :flag)))))

(deftest vm-progv-enter-exit
  "vm-progv-enter binds globals and vm-progv-exit restores them."
  (let ((s (make-test-vm)))
    (setf (gethash 'prim-pv-x (cl-cc:vm-global-vars s)) 10)
    (cl-cc:vm-reg-set s 1 '(prim-pv-x prim-pv-y))
    (cl-cc:vm-reg-set s 2 '(99 100))
    (exec1 (cl-cc:make-vm-progv-enter :dst 0 :syms 1 :vals 2) s)
    (assert-= 99 (gethash 'prim-pv-x (cl-cc:vm-global-vars s)))
    (assert-= 100 (gethash 'prim-pv-y (cl-cc:vm-global-vars s)))
    (exec1 (cl-cc:make-vm-progv-exit :saved 0) s)
    (assert-= 10 (gethash 'prim-pv-x (cl-cc:vm-global-vars s)))
    (assert-false (nth-value 1 (gethash 'prim-pv-y (cl-cc:vm-global-vars s))))))

(deftest-each vm-generic-arith
  "Generic arithmetic instructions compute correct results."
  :cases (("add"  #'cl-cc:make-vm-generic-add  10 3   13)
          ("sub"  #'cl-cc:make-vm-generic-sub  10 3   7)
          ("mul"  #'cl-cc:make-vm-generic-mul  10 3   30)
          ("div"  #'cl-cc:make-vm-generic-div  10 3   3))
  (ctor lhs rhs expected)
  (assert-= expected (%vm-ext-binary ctor lhs rhs)))

(deftest-each vm-generic-comparison
  "Generic comparison instructions: eq/lt/gt over known values."
  :cases (("eq-equal"    #'cl-cc:make-vm-generic-eq "hello" "hello" t)
          ("eq-unequal"  #'cl-cc:make-vm-generic-eq "hello" "world" nil)
          ("lt-true"     #'cl-cc:make-vm-generic-lt 3       5       t)
          ("gt-false"    #'cl-cc:make-vm-generic-gt 3       5       nil))
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
      (exec1 (cl-cc:make-vm-generic-div :dst 0 :lhs 1 :rhs 2) s))))
