(in-package :cl-cc/test)

(defsuite debug-suite
  :description "FR-687/688/689 debug subsystem tests"
  :parent cl-cc-unit-suite)

(in-suite debug-suite)

(defclass debug-test-object ()
  ((name :initarg :name :accessor debug-test-object-name)
   (count :initarg :count :accessor debug-test-object-count)))

(deftest swank-interactive-eval-evaluates-basic-form
  "FR-687: interactive-eval returns host values for a skeleton request."
  (let ((result (cl-cc:interactive-eval "(+ 1 2)")))
    (assert-true (getf result :ok))
    (assert-equal '(3) (getf result :values))))

(deftest swank-completions-and-arglist-are-available
  "FR-687: completions and operator-arglist provide minimal editor metadata."
  (assert-true (member "CAR" (cl-cc:completions "ca" :package :cl) :test #'string=))
  (assert-true (listp (cl-cc:operator-arglist 'cl:+))))

(deftest swank-compile-string-compiles-thunk
  "FR-687: compile-string returns a callable thunk."
  (let* ((result (cl-cc:compile-string "(+ 20 22)"))
         (function (getf result :function)))
    (assert-true (getf result :ok))
    (assert-true (functionp function))
    (assert-= 42 (funcall function))))

(deftest inspector-records-cons-and-hash-table-parts
  "FR-688: inspect tracks history and handles cons cells and hash tables."
  (let* ((before (length cl-cc:*inspected-objects*))
         (cons-result (cl-cc:inspect '(a . b)))
         (table (make-hash-table))
         (table-result nil))
    (setf (gethash :answer table) 42)
    (setf table-result (cl-cc:inspect table))
    (assert-= before (getf cons-result :id))
    (assert-equal '((:slot car :value a) (:slot cdr :value b))
                  (getf cons-result :parts))
    (assert-equal table (aref cl-cc:*inspected-objects* (getf table-result :id)))
    (assert-equal '((:key :answer :value 42)) (getf table-result :parts))))

(deftest inspector-shows-clos-slots
  "FR-688: inspect exposes CLOS instance slot names and values."
  (let* ((object (make-instance 'debug-test-object :name "n" :count 7))
         (parts (getf (cl-cc:inspect object) :parts)))
    (assert-true (find 'name parts :key (lambda (part) (getf part :slot))))
    (assert-true (find 'count parts :key (lambda (part) (getf part :slot))))))

(deftest step-debugger-signals-at-breakpoints
  "FR-689: VM-level breakpoints signal STEP-CONDITION at instruction boundaries."
  (let* ((program (cl-cc:make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)
                                       (cl-cc:make-vm-halt :reg :r0))
                   :result-register :r0))
         (seen nil))
    (cl-cc:clear-step-breakpoints)
    (cl-cc:add-step-breakpoint 0)
    (handler-bind ((cl-cc:step-condition
                     (lambda (condition)
                       (push (cl-cc:step-condition-pc condition) seen)
                       (invoke-restart 'cl-cc/vm::continue))))
      (assert-= 42 (cl-cc:run-compiled program)))
    (cl-cc:clear-step-breakpoints)
    (assert-equal '(0) (nreverse seen))))

(deftest step-macro-enables-step-into-mode
  "FR-689: step macro enables :INTO stepping for VM execution."
  (let* ((program (cl-cc:make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                       (cl-cc:make-vm-halt :reg :r0))
                   :result-register :r0))
          (count 0))
    (handler-bind ((cl-cc:step-condition
                     (lambda (_condition)
                       (declare (ignore _condition))
                       (incf count)
                       (invoke-restart 'cl-cc/vm::continue))))
      (cl-cc:step (:mode :into)
        (assert-= 1 (cl-cc:run-compiled program))))
    (assert-= 2 count)))

(deftest watchpoint-detects-register-write
  "FR-314: VM watchpoints signal VM-WATCHPOINT-CONDITION when a watched
register is written during VM execution."
  (let* ((program (cl-cc:make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 10)
                                       (cl-cc:make-vm-const :dst :r0 :value 20)
                                       (cl-cc:make-vm-halt :reg :r0))
                   :result-register :r0))
         (seen nil))
    (cl-cc:add-vm-watchpoint :r0)
    (handler-bind ((cl-cc:vm-watchpoint-condition
                     (lambda (c)
                       (push (list (cl-cc:vm-watchpoint-reg c)
                                   (cl-cc:vm-watchpoint-old-value c)
                                   (cl-cc:vm-watchpoint-new-value c))
                             seen))))
      (assert-= 20 (cl-cc:run-compiled program)))
    (cl-cc:clear-vm-watchpoints)
    ;; :r0 was written first with nil→10, then 10→20
    (assert-equal '((:r0 nil 10) (:r0 10 20)) (nreverse seen))))
