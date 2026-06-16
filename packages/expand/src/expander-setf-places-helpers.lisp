(in-package :cl-cc/expand)

;;; Only setf-place cons-cell expansion uses this logic.

(defun %setf-cxr-letters (head)
  "Return the a/d letters for a CXR accessor HEAD, or NIL."
  (let ((name (symbol-name head)))
    (when (and (>= (length name) 3)
               (char-equal (char name 0) #\C)
               (char-equal (char name (1- (length name))) #\R))
      (let ((letters (subseq name 1 (1- (length name)))))
        (when (every (lambda (ch) (or (char-equal ch #\A) (char-equal ch #\D)))
                     letters)
          letters)))))

(defun %expand-cxr-letters (letters arg)
  "Expand LETTERS as CXR a/d operations applied to ARG."
  (let ((acc arg))
    (loop for i from (1- (length letters)) downto 0
          do (setf acc (if (char-equal (char letters i) #\A)
                           (list 'car acc)
                           (list 'cdr acc))))
    acc))

(defun %expand-setf-cxr-place (place value letters)
  "Expand a CXR setf PLACE using LETTERS to select rplaca or rplacd."
  (let* ((v (gensym "V"))
         (parent (%expand-cxr-letters (subseq letters 1) (second place)))
         (writer (if (char-equal (char letters 0) #\A) 'rplaca 'rplacd)))
    `(let ((,v ,value)) (,writer ,parent ,v) ,v)))

(defparameter *setf-nth-list-accessor-indices*
  '((second . 1)
    (third  . 2)
    (fourth . 3)
    (fifth  . 4)
    (sixth  . 5)
    (seventh . 6)
    (eighth . 7)
    (ninth  . 8)
    (tenth  . 9))
  "List accessor heads whose SETF places write the CAR of a CDR chain.")

(defun %setf-nth-list-accessor-index (head)
  (cdr (assoc head *setf-nth-list-accessor-indices* :test #'eq)))

(defun %setf-cdr-chain (base count)
  (loop with form = base
        repeat count
        do (setf form (list 'cdr form))
        finally (return form)))

(defun expand-setf-cons-place (place value)
  "Expand (setf (ACCESSOR ARGS...) value) for cons-cell accessors to rplaca/rplacd."
  (let ((v (gensym "V")))
    (cond
      ((or (eq (car place) 'car) (eq (car place) 'first))
       `(let ((,v ,value)) (rplaca ,(second place) ,v) ,v))
      ((or (eq (car place) 'cdr) (eq (car place) 'rest))
       `(let ((,v ,value)) (rplacd ,(second place) ,v) ,v))
      ((eq (car place) 'nth)
       `(let ((,v ,value)) (rplaca (nthcdr ,(second place) ,(third place)) ,v) ,v))
      ((%setf-nth-list-accessor-index (car place))
       `(let ((,v ,value))
          (rplaca ,(%setf-cdr-chain (second place)
                                     (%setf-nth-list-accessor-index (car place)))
                  ,v)
          ,v))
      (t
       (let ((letters (%setf-cxr-letters (car place))))
         (unless letters
           (error "Unsupported cons setf place: ~S" place))
         (%expand-setf-cxr-place place value letters))))))
