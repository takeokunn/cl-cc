;;; example/control-flow.lisp — Block/return-from, tagbody/go, handler-case
;;;
;;; Usage: ./cl-cc run example/control-flow.lisp

;; block / return-from
(print (block done
         (print 1)
         (return-from done 42)
         (print 999)))

;; tagbody / go (loop 1..5)
(let ((i 0) (sum 0))
  (tagbody
   loop
     (if (>= i 5) (go end))
     (setq i (+ i 1))
     (setq sum (+ sum i))
     (go loop)
   end)
  (print sum))

;; handler-case
(print
 (handler-case
     (progn
       (error "something went wrong")
       0)
   (error (e) 99)))
