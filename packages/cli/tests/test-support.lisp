;;;; tests/unit/cli/test-support.lisp — shared CLI test helpers

(in-package :cl-cc/test)

(defmacro with-fake-quit (&body body)
  "Run BODY with uiop:quit intercepted via sb-int:encapsulate + catch/throw.
Encapsulation works even for SBCL-compiled core-image code where
symbol-function replacement may be bypassed by fdefinition optimisation."
  (let ((gcode (gensym "CODE")))
    `(let ((,gcode nil))
       (unwind-protect
            (progn
              (sb-int:encapsulate
               'uiop:quit 'cl-cc-test-fake-quit
               (lambda (original &rest args)
                 (declare (ignore original))
                 (setf ,gcode (car args))
                 (throw 'cl-cc-cli-quit ,gcode)))
              (catch 'cl-cc-cli-quit
                ,@body))
         (ignore-errors
          (sb-int:unencapsulate 'uiop:quit 'cl-cc-test-fake-quit))))))

(defun make-cli-parsed (&key command positional flags)
  (let ((parsed (cl-cc/cli::make-parsed-args :command command
                                             :positional positional)))
    (dolist (entry flags parsed)
      (setf (gethash (car entry) (cl-cc/cli:parsed-args-flags parsed))
            (cdr entry)))))
