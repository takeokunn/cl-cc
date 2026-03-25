;;; example/arithmetic.lisp — Basic arithmetic and let bindings
;;;
;;; Usage: ./cl-cc run example/arithmetic.lisp

(print (+ (* 2 3) 4))

(let ((x 10)
      (y 20))
  (print (+ x y)))

(let ((a 5))
  (let ((b (* a a)))
    (print b)))
