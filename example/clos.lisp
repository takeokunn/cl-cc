;;; example/clos.lisp — CLOS: classes, generic functions, methods
;;;
;;; Usage: ./cl-cc run example/clos.lisp

(defclass point ()
  ((x :initarg :x :reader point-x)
   (y :initarg :y :reader point-y)))

(defgeneric distance (p))

(defmethod distance ((p point))
  (let ((px (slot-value p 'x))
        (py (slot-value p 'y)))
    (+ (* px px) (* py py))))

(let ((p (make-instance 'point :x 3 :y 4)))
  (print (point-x p))
  (print (point-y p))
  (print (distance p)))
