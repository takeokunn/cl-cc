;;;; optimizer-dae-tests.lisp --- FR-606 tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest dead-argument-elimination-rewrites-known-call-site
  "FR-606 creates a specialized callee and drops unused call arguments."
  (let* ((insts (list (make-vm-closure :dst :fn :label "target" :params '(:a :b) :captured nil)
                      (make-vm-label :name "target")
                      (make-vm-const :dst :one :value 1)
                      (make-vm-add :dst :sum :lhs :a :rhs :one)
                      (make-vm-ret :reg :sum)
                      (make-vm-func-ref :dst :callee :label "target" :params '(:a :b))
                      (make-vm-move :dst :arg0 :src :x)
                      (make-vm-move :dst :arg1 :src :y)
                      (make-vm-call :dst :result :func :callee :args '(:arg0 :arg1))))
         (out (cl-cc/optimize:opt-pass-dead-argument-elimination insts))
         (calls (remove-if-not #'cl-cc/vm:vm-call-p out))
         (special-labels (remove-if-not
                          (lambda (inst)
                            (and (typep inst 'cl-cc/vm::vm-label)
                                 (search ".DAE." (cl-cc/vm::vm-name inst))))
                          out)))
    (assert-= 1 (length calls))
    (assert-equal '(:arg0) (cl-cc/vm::vm-args (first calls)))
    (assert-= 1 (length special-labels))
    (assert-false (some (lambda (inst)
                          (and (typep inst 'cl-cc/vm::vm-move)
                               (eq (cl-cc/vm::vm-dst inst) :arg1)))
                        out))))

(deftest dead-argument-elimination-keeps-captured-parameter
  "FR-606 treats parameters captured by nested closures as indirectly used."
  (let* ((insts (list (make-vm-closure :dst :outer-fn :label "outer" :params '(:a :b) :captured nil)
                      (make-vm-label :name "outer")
                      (make-vm-closure :dst :inner-fn :label "inner" :params nil :captured '(:a))
                      (make-vm-ret :reg :b)
                      (make-vm-label :name "inner")
                      (make-vm-ret :reg :a)
                      (make-vm-func-ref :dst :callee :label "outer" :params '(:a :b))
                      (make-vm-call :dst :result :func :callee :args '(:x :y))))
         (out (cl-cc/optimize:opt-pass-dead-argument-elimination insts))
         (calls (remove-if-not #'cl-cc/vm:vm-call-p out)))
    (assert-= 1 (length calls))
    (assert-equal '(:x :y) (cl-cc/vm::vm-args (first calls)))
    (assert-false (some (lambda (inst)
                          (and (typep inst 'cl-cc/vm::vm-label)
                               (search ".DAE." (cl-cc/vm::vm-name inst))))
                        out))))
