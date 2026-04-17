;;;; tests/unit/cli/test-support.lisp — shared CLI test helpers

(in-package :cl-cc/test)

(define-condition fake-quit (error)
  ((code :initarg :code :reader fake-quit-code)))

(defmacro with-replaced-function ((name replacement) &body body)
  (let ((old (gensym "OLD-FN"))
        (had (gensym "HAD-FN")))
    `(let ((,had (fboundp ',name))
           (,old (ignore-errors (symbol-function ',name))))
       (unwind-protect
            (progn
              (setf (symbol-function ',name) ,replacement)
              ,@body)
         (if ,had
             (setf (symbol-function ',name) ,old)
             (fmakunbound ',name))))))

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
