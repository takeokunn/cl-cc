;;; example/closure.lisp — Closures and higher-order functions
;;;
;;; Usage: ./cl-cc run example/closure.lisp

(defun make-adder (n)
  (lambda (x) (+ x n)))

(let ((add5 (make-adder 5)))
  (print (funcall add5 10)))

(defun make-counter ()
  (let ((count 0))
    (lambda ()
      (setq count (+ count 1))
      count)))

(let ((c (make-counter)))
  (print (funcall c))
  (print (funcall c))
  (print (funcall c)))
