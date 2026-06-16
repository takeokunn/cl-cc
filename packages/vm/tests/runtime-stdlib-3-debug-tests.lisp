;;; runtime-stdlib-3-debug-tests.lisp — FR-944/945/1085 debug surface

(in-package :cl-cc/test)

(defsuite runtime-stdlib-3-debug-suite
  :description "Runtime-stdlib-3 rich errors, restarts, and debug helpers"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-3-debug-suite)

(deftest rich-error-report-includes-location-context-and-suggestion
  "FR-944: VM errors include source location, context, caret, and suggestions."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (loc (cl-cc/vm:make-source-location :file "foo.lisp" :line 2 :column 4))
         (condition (cl-cc/vm::make-vm-unbound-variable
                     state 'pritn
                     :source-location loc
                     :source-text (format nil "(defun f ()~%  pritn~%  42)")
                     :suggestions '(print))))
    (let ((text (format nil "~A" condition)))
      (assert-true (search "foo.lisp:2:4" text))
      (assert-true (search "Did you mean" text))
      (assert-true (search "pritn" text))
      (assert-true (search "^" text)))))

(deftest rich-error-json-output-is-structured
  "FR-944: format-condition-json emits a structured diagnostic object."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (loc (cl-cc/vm:make-source-location :file "foo.lisp" :line 1 :column 1))
         (condition (cl-cc/vm::make-vm-undefined-function
                     state 'mapcarr :source-location loc :suggestions '(mapcar))))
    (let ((json (with-output-to-string (out)
                  (cl-cc/vm:format-condition-json condition out))))
      (assert-true (search "\"location\"" json))
      (assert-true (search "foo.lisp" json))
      (assert-true (search "MAPCAR" json)))))

(deftest restart-ui-describes-and-invokes-vm-restart-interactively
  "FR-945: VM restart records expose descriptions and interactive invocation."
  (let ((called 0))
    (let ((restart (cl-cc/vm::make-vm-restart 'use-value
                                              (lambda (value)
                                                (setf called value)))))
      (setf (cl-cc/vm::vm-restart-description restart) "Use supplied value")
      (setf (cl-cc/vm::vm-restart-interactive-function restart) (lambda () (list 42)))
      (assert-equal "Use supplied value" (cl-cc/vm:describe-restart restart))
      (cl-cc/vm:vm-invoke-restart-interactively restart)
      (assert-= 42 called))))

(deftest trace-helpers-log-call-and-return
  "FR-1085: vm-with-trace logs call and return lines with nesting depth."
  (let ((output (make-string-output-stream)))
    (let ((cl-cc/vm:*trace-output* output))
      (cl-cc/vm:vm-untrace)
      (cl-cc/vm:vm-trace 'foo)
      (assert-= 3 (cl-cc/vm:vm-with-trace ('foo '(1 2)) (+ 1 2))))
    (let ((text (get-output-stream-string output)))
      (assert-true (search "0: (FOO 1 2)" text))
      (assert-true (search "0: FOO returned 3" text)))))

(deftest debugger-helper-surface-is-callable
  "FR-1085: apropos/describe/inspect/room/ed/dribble helpers are present."
  (assert-true (member 'car (cl-cc/vm:vm-apropos-list "CAR" :cl) :test #'eq))
  (assert-null (cl-cc/vm:vm-ed 'dummy))
  (let ((text (with-output-to-string (out)
                (cl-cc/vm:vm-describe 'car out))))
    (assert-true (search "CAR" text)))
  (assert-eq 'car (cl-cc/vm:vm-inspect 'car)))
