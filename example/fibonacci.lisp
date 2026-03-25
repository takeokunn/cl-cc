;;; example/fibonacci.lisp — Mutually recursive functions with labels
;;;
;;; Usage: ./cl-cc run example/fibonacci.lisp

(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(print (fib 10))
