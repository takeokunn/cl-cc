;;; example/list.lisp — List operations with stdlib
;;;
;;; Usage: ./cl-cc run example/list.lisp --stdlib

(let ((xs (list 1 2 3 4 5)))
  (print xs)
  (print (car xs))
  (print (cdr xs))
  (print (length xs))
  (print (mapcar (lambda (x) (* x x)) xs))
  (print (remove-if (lambda (x) (< x 3)) xs)))
