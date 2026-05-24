(in-package :cl-cc/expand)
;;; FR-839: Iteration protocol.
;;;
;;; MAKE-ITERATOR is extensible so FR-838 custom sequence classes can join the
;;; protocol, while ITERATOR-NEXT stays a small struct-access hot path.

(defstruct (iterator (:constructor %make-iterator (sequence state limit next-function)))
  "Iterator protocol object for sequence-like values."
  sequence
  state
  limit
  next-function)

(defgeneric make-iterator (sequence)
  (:documentation "Create an iterator object over SEQUENCE."))

(defun %iterator-next-list (iterator)
  "Return the next value from a list ITERATOR."
  (let ((cell (iterator-state iterator)))
    (if (null cell)
        (values nil nil)
        (multiple-value-prog1
            (values (car cell) t)
          (setf (iterator-state iterator) (cdr cell))))))

(defun %iterator-next-vector (iterator)
  "Return the next value from a vector ITERATOR."
  (let ((index (iterator-state iterator))
        (limit (iterator-limit iterator)))
    (if (>= index limit)
        (values nil nil)
        (multiple-value-prog1
            (values (aref (iterator-sequence iterator) index) t)
          (setf (iterator-state iterator) (+ index 1))))))

(defun %iterator-next-string (iterator)
  "Return the next value from a string ITERATOR."
  (let ((index (iterator-state iterator))
        (limit (iterator-limit iterator)))
    (if (>= index limit)
        (values nil nil)
        (multiple-value-prog1
            (values (char (iterator-sequence iterator) index) t)
          (setf (iterator-state iterator) (+ index 1))))))

(defun %iterator-next-sequence-protocol (iterator)
  "Return the next value through the FR-838 extensible sequence protocol."
  (let ((index (iterator-state iterator))
        (limit (iterator-limit iterator)))
    (if (>= index limit)
        (values nil nil)
        (multiple-value-prog1
            (values (cl-cc/vm::elt (iterator-sequence iterator) index) t)
          (setf (iterator-state iterator) (+ index 1))))))

(defmethod make-iterator ((sequence list))
  (%make-iterator sequence sequence nil #'%iterator-next-list))

(defmethod make-iterator ((sequence vector))
  (%make-iterator sequence 0 (length sequence) #'%iterator-next-vector))

(defmethod make-iterator ((sequence string))
  (%make-iterator sequence 0 (length sequence) #'%iterator-next-string))

(defmethod make-iterator ((sequence cl-cc/vm::sequence))
  (%make-iterator sequence 0 (cl-cc/vm::length sequence)
                  #'%iterator-next-sequence-protocol))

(defun iterator-next (iterator)
  "Return (values VALUE HAS-MORE-P) for ITERATOR."
  (funcall (iterator-next-function iterator) iterator))

(our-defmacro with-iterator ((iterator sequence) &body body)
  "Bind ITERATOR to an iterator over SEQUENCE while evaluating BODY."
  `(let ((,iterator (make-iterator ,sequence)))
     ,@body))

(our-defmacro doiterator ((var sequence &optional result) &body body)
  "Iterate VAR over SEQUENCE using MAKE-ITERATOR/ITERATOR-NEXT."
  (let ((iterator (gensym "ITERATOR"))
        (has-more-p (gensym "HAS-MORE-P")))
    `(with-iterator (,iterator ,sequence)
       (loop
         (multiple-value-bind (,var ,has-more-p) (iterator-next ,iterator)
           (unless ,has-more-p
             (return ,result))
           ,@body)))))
