(in-package :cl-cc/runtime)
(defmacro rt-async (&body body) `(let ((f (rt-make-future))) (rt-spawn (lambda () (rt-future-resolve f (progn ,@body)))) f))
(defmacro rt-await (future-form) `(rt-future-await ,future-form))
