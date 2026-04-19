;;;; optimizer-store-analysis-tests.lisp — egraph, bswap/rotate, dead-store, SCCP, store-to-load
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(deftest egraph-pass-lowers-constant-subtraction
  "opt-pass-egraph rewrites a simple constant subtraction to vm-const."
  (let* ((i1 (cl-cc:make-vm-const :dst :r0 :value 7))
         (i2 (cl-cc:make-vm-sub   :dst :r1 :lhs :r0 :rhs :r0))
         (out (cl-cc/optimize::opt-pass-egraph (list i1 i2)))
         (r1  (find-if (lambda (inst)
                         (eq :r1 (ignore-errors (cl-cc/vm::vm-dst inst))))
                       out)))
    (assert-true (cl-cc::vm-const-p r1))
    (assert-equal 0 (cl-cc/vm::vm-value r1))))

(deftest bswap-recognition-collapses-byte-swap-tree
  "opt-pass-bswap-recognition collapses the canonical byte-swap tree to vm-bswap."
  (let* ((input (make-bswap-tree-instructions))
         (out   (cl-cc/optimize::opt-pass-bswap-recognition input)))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-bswap)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))

(deftest rotate-recognition-collapses-shift-or-tree
  "opt-pass-rotate-recognition collapses a two-shift + OR idiom to vm-rotate."
  (let* ((c0  (make-vm-const :dst :r1 :value 8))
         (a0  (make-vm-ash   :dst :r2 :lhs :r0 :rhs :r1))
         (c1  (make-vm-const :dst :r3 :value -56))
         (a1  (make-vm-ash   :dst :r4 :lhs :r0 :rhs :r3))
         (o0  (make-vm-logior :dst :r5 :lhs :r2 :rhs :r4))
         (out (cl-cc/optimize::opt-pass-rotate-recognition (list c0 a0 c1 a1 o0))))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-rotate)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) out))))

(deftest-each dead-store-elim-cases
  "opt-pass-dead-store-elim: overwritten stores removed, live-read stores kept, alias-tracked slot writes handled."
  :cases (("overwritten-global"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 1))
                    (s1  (make-vm-set-global :name "g" :src :r0))
                    (c2  (make-vm-const :dst :r1 :value 2))
                    (s2  (make-vm-set-global :name "g" :src :r1))
                    (ret (make-vm-ret :reg :r1)))
               (list (list c1 s1 c2 s2 ret) (list s1) (list s2 c1 c2 ret)))))
          ("keeps-live-read"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 1))
                    (s1  (make-vm-set-global :name "g" :src :r0))
                    (g1  (make-vm-get-global :dst :r2 :name "g"))
                    (c2  (make-vm-const :dst :r1 :value 2))
                    (s2  (make-vm-set-global :name "g" :src :r1))
                    (ret (make-vm-ret :reg :r2)))
               (list (list c1 s1 g1 c2 s2 ret) nil (list s1 g1 s2 ret)))))
          ("slot-overwrite-alias"
           (lambda ()
             (let* ((obj  (make-vm-cons :dst :r0 :car-src :r8 :cdr-src :r9))
                    (obj2 (make-vm-move :dst :r3 :src :r0))
                    (c1   (make-vm-const :dst :r1 :value 10))
                    (w1   (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
                    (c2   (make-vm-const :dst :r2 :value 20))
                    (w2   (cl-cc::make-vm-slot-write :obj-reg :r3 :slot-name 'x :value-reg :r2))
                    (ret  (make-vm-ret :reg :r2)))
                (list (list obj obj2 c1 w1 c2 w2 ret)
                      (list w1)
                      (list obj obj2 c1 c2 w2 ret))))))
  (make-case)
  (destructuring-bind (instrs absent present) (funcall make-case)
    (let ((out (cl-cc/optimize::opt-pass-dead-store-elim instrs)))
      (dolist (inst absent)   (assert-false (member inst out)))
      (dolist (inst present)  (assert-true  (member inst out))))))

(deftest sccp-eliminates-constant-branch-block
  "opt-pass-sccp removes the unreachable branch block after a constant condition."
  (let* ((start (make-vm-label :name "start"))
         (cond  (make-vm-const :dst :r0 :value 1))
         (br    (make-vm-jump-zero :reg :r0 :label "else"))
         (thenl (make-vm-label :name "then"))
         (thenv (make-vm-const :dst :r1 :value 10))
         (jmp   (make-vm-jump :label "end"))
         (elsel (make-vm-label :name "else"))
         (elsev (make-vm-const :dst :r1 :value 20))
         (endl  (make-vm-label :name "end"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc/optimize::opt-pass-sccp (list start cond br thenl thenv jmp elsel elsev endl ret))))
    (assert-false (member elsel out))
    (assert-false (member elsev out))
    (assert-true  (some (lambda (i) (and (typep i 'cl-cc/vm::vm-const)
                                         (eq (cl-cc/vm::vm-dst i) :r1)
                                         (eql (cl-cc/vm::vm-value i) 10)))
                        out))))

(deftest-each store-to-load-forward-cases
  "opt-pass-store-to-load-forward replaces load instructions with vm-move (global and slot variants)."
  :cases (("global-direct"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 1))
                    (s1  (make-vm-set-global :name "g" :src :r0))
                    (g1  (make-vm-get-global :dst :r1 :name "g"))
                    (ret (make-vm-ret :reg :r1)))
               (list (list c1 s1 g1 ret) g1 :r1 :r0))))
          ("global-same-dst"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 1))
                    (s1  (make-vm-set-global :name "g" :src :r0))
                    (g1  (make-vm-get-global :dst :r0 :name "g"))
                    (ret (make-vm-ret :reg :r0)))
               (list (list c1 s1 g1 ret) g1 :r0 :r0))))
          ("slot-direct"
           (lambda ()
             (let* ((obj (make-vm-const :dst :r0 :value 42))
                    (val (make-vm-const :dst :r1 :value 99))
                    (w   (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
                    (r   (cl-cc::make-vm-slot-read :dst :r2 :obj-reg :r0 :slot-name 'x))
                    (ret (make-vm-ret :reg :r2)))
               (list (list obj val w r ret) r :r2 :r1))))
          ("slot-same-dst"
           (lambda ()
             (let* ((obj (make-vm-const :dst :r0 :value 42))
                    (val (make-vm-const :dst :r1 :value 99))
                    (w   (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
                    (r   (cl-cc::make-vm-slot-read :dst :r1 :obj-reg :r0 :slot-name 'x))
                    (ret (make-vm-ret :reg :r1)))
               (list (list obj val w r ret) r :r1 :r1))))
          ("slot-through-moved-alias"
           (lambda ()
             (let* ((obj  (make-vm-cons :dst :r0 :car-src :r8 :cdr-src :r9))
                    (obj2 (make-vm-move :dst :r3 :src :r0))
                    (val  (make-vm-const :dst :r1 :value 99))
                    (w    (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
                    (r    (cl-cc::make-vm-slot-read :dst :r2 :obj-reg :r3 :slot-name 'x))
                    (ret  (make-vm-ret :reg :r2)))
               (list (list obj obj2 val w r ret) r :r2 :r1)))))
  (make-case)
  (destructuring-bind (instrs read-instr expected-dst expected-src) (funcall make-case)
    (let ((out (cl-cc/optimize::opt-pass-store-to-load-forward instrs)))
      (assert-false (member read-instr out :test #'eq))
      (assert-true (some (lambda (i)
                           (and (typep i 'cl-cc/vm::vm-move)
                                (eq (cl-cc::vm-move-dst i) expected-dst)
                                (eq (cl-cc::vm-move-src i) expected-src)))
                         out)))))
