;;;; tests/unit/cli/test-support.lisp — shared CLI test helpers

(in-package :cl-cc/test)

(define-condition fake-quit (error)
  ((code :initarg :code :reader fake-quit-code)))

(defmacro with-fake-quit (&body body)
  `(with-replaced-function (uiop:quit
                            (lambda (code)
                              (error 'fake-quit :code code)))
     ,@body))

(defun make-cli-parsed (&key command positional flags)
  (let ((parsed (cl-cc/cli::make-parsed-args :command command
                                             :positional positional)))
    (dolist (entry flags parsed)
      (setf (gethash (car entry) (cl-cc/cli:parsed-args-flags parsed))
            (cdr entry)))))
