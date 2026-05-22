;;;; packages/emit/tests/mlir-tests.lisp

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest mlir-lowers-clcc-dialect-arithmetic-call-return-and-branch
  "FR-712: MIR lowers through clcc dialect ops to valid func/arith/cf MLIR text."
  (let* ((fn (mir-make-function :mlir-add-or-sub))
         (entry (mirf-entry fn))
         (then-block (mir-new-block fn :label :then))
         (else-block (mir-new-block fn :label :else))
         (lhs (mir-new-value fn :name :lhs :type :integer))
         (rhs (mir-new-value fn :name :rhs :type :integer))
         (condv (mir-new-value fn :name :cond :type :boolean))
         (sum (mir-new-value fn :name :sum :type :integer))
         (product (mir-new-value fn :name :product :type :integer))
         (called (mir-new-value fn :name :called :type :integer))
         (diff (mir-new-value fn :name :diff :type :integer)))
    (setf (mirf-params fn) (list lhs rhs))
    (mir-add-succ entry then-block)
    (mir-add-succ entry else-block)
    (mir-emit entry :const :dst condv :srcs (list (make-mir-const :value 1 :type :boolean)))
    (mir-emit entry :branch :srcs (list condv then-block else-block))
    (mir-emit then-block :add :dst sum :srcs (list lhs rhs) :type :integer)
    (mir-emit then-block :mul :dst product :srcs (list sum (make-mir-const :value 2 :type :integer)) :type :integer)
    (mir-emit then-block :call :dst called
              :srcs (list (make-mir-const :value :external-inc :type :function) product)
              :type :integer)
    (mir-emit then-block :ret :srcs (list called))
    (mir-emit else-block :sub :dst diff :srcs (list rhs lhs) :type :integer)
    (mir-emit else-block :ret :srcs (list diff))
    (let ((mlir (cl-cc/emit:emit-mlir fn :name "fr712-real")))
      (assert-true (search "module @fr712_real {" mlir))
      (assert-true (search "func.func @mlir_add_or_sub(%arg0: i64, %arg1: i64) -> i64" mlir))
      (assert-true (search "%reg2 = arith.constant 1 : i1" mlir))
      (assert-true (search "cf.cond_br %reg2, ^then, ^else" mlir))
      (assert-true (search "%reg3 = arith.addi %arg0, %arg1 : i64" mlir))
      (assert-true (search "arith.constant 2 : i64" mlir))
      (assert-true (search "%reg4 = arith.muli %reg3" mlir))
      (assert-true (search "%reg5 = func.call @external_inc(%reg4) : (i64) -> i64" mlir))
      (assert-true (search "func.func private @external_inc(i64) -> i64" mlir))
      (assert-true (search "%reg6 = arith.subi %arg1, %arg0 : i64" mlir))
      (assert-true (search "func.return %reg5 : i64" mlir))
      (assert-false (search "TODO" mlir))
      (assert-false (search "bridge-only" mlir)))))

(deftest mlir-lowers-void-call-and-return
  "FR-712: clcc.call without a destination lowers to func.call with no results."
  (let* ((fn (mir-make-function :mlir-void-call))
         (entry (mirf-entry fn)))
    (mir-emit entry :call
              :srcs (list (make-mir-const :value :notify :type :function)
                          (make-mir-const :value 7 :type :integer))
              :type :void)
    (mir-emit entry :ret)
    (let ((mlir (cl-cc/emit:emit-mlir fn :name "fr712-void")))
      (assert-true (search "func.func private @notify(i64)" mlir))
      (assert-true (search "func.call @notify" mlir))
      (assert-true (search ": (i64) -> ()" mlir))
      (assert-true (search "func.return" mlir))
      (assert-false (search "-> none" mlir)))))

(deftest mlir-unsupported-phi-signals-error
  "FR-712: MIR phi nodes are rejected explicitly rather than emitting invalid MLIR."
  (let* ((fn (mir-make-function :mlir-phi))
         (entry (mirf-entry fn))
         (merge (mir-new-block fn :label :merge))
         (value (mir-new-value fn :type :integer)))
    (mir-add-succ entry merge)
    (mir-emit entry :jump :srcs (list merge))
    (mir-emit merge :phi :dst value :srcs (list (cons entry (make-mir-const :value 1 :type :integer))))
    (mir-emit merge :ret :srcs (list value))
    (assert-signals error (cl-cc/emit:emit-mlir fn :name "fr712-phi"))))

(deftest mlir-capabilities-describe-clcc-lowering-patterns
  "The MLIR capability metadata exposes concrete clcc dialect ops and lowering patterns."
  (let ((caps (cl-cc/emit:mlir-bridge-capabilities)))
    (assert-true (member :defined-dialect caps))
    (assert-true (search "CLCC.ADD" (format nil "~S" caps)))
    (assert-true (search "ARITH.ADDI" (format nil "~S" caps)))
    (assert-true (search "FUNC.CALL" (format nil "~S" caps)))
    (assert-true (search "FUNC.RETURN" (format nil "~S" caps)))
    (assert-false (member :bridge-only caps))))
