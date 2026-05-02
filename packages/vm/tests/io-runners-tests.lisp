;;;; tests/unit/vm/io-runners-tests.lisp — I/O runner convenience coverage

(in-package :cl-cc/test)

(in-suite io-suite)

(deftest run-compiled-with-io-basic
  "run-compiled-with-io executes a trivial program and returns the halt value."
  (let* ((program (cl-cc/vm::make-vm-program
                    :instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)
                                        (cl-cc:make-vm-halt :reg :r0))
                    :result-register :r0))
         (out (make-string-output-stream)))
    (assert-equal 42 (cl-cc/vm::run-compiled-with-io program :output-stream out))))

(deftest run-compiled-with-io-binds-custom-streams
  "run-compiled-with-io accepts custom input/output streams without disturbing execution."
  (let* ((program (cl-cc/vm::make-vm-program
                    :instructions (list (cl-cc:make-vm-const :dst :r0 :value 7)
                                        (cl-cc:make-vm-halt :reg :r0))
                    :result-register :r0))
         (in (make-string-input-stream "input"))
         (out (make-string-output-stream)))
    (assert-equal 7 (cl-cc/vm::run-compiled-with-io program :input-stream in :output-stream out))))

(deftest run-string-with-io-uses-compile-hook
  "run-string-with-io delegates to *vm-compile-string-hook* and runs the returned program."
  (let ((cl-cc/vm::*vm-compile-string-hook*
          (lambda (source)
            (declare (ignore source))
            (cl-cc/vm::make-vm-program
             :instructions (list (cl-cc:make-vm-const :dst :r0 :value 99)
                                 (cl-cc:make-vm-halt :reg :r0))
             :result-register :r0))))
    (assert-equal 99 (cl-cc/vm::run-string-with-io "ignored source"))))
