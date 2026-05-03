(in-package :cl-cc/expand)

;;; Only setf-place cons-cell expansion uses this logic.

(defun %setf-cxr-letters (head)
  "Return the a/d letters for a CXR accessor HEAD, or NIL."
  (let ((name (symbol-name head)))
    (when (and (>= (length name) 3)
               (char-equal (char name 0) #\C)
               (char-equal (char name (1- (length name))) #\R))
      (let ((letters (subseq name 1 (1- (length name)))))
        (let ((i 0)
              (n (length letters)))
          (tagbody
           scan
             (if (>= i n) (return-from %setf-cxr-letters letters))
             (let ((ch (char letters i)))
               (unless (or (char-equal ch #\A)
                           (char-equal ch #\D))
                 (return-from %setf-cxr-letters nil)))
             (setf i (+ i 1))
             (go scan)))))))

(defun %expand-cxr-letters (letters arg)
  "Expand LETTERS as CXR a/d operations applied to ARG."
  (let ((i (1- (length letters)))
        (acc arg))
    (tagbody
     scan
       (if (< i 0) (return-from %expand-cxr-letters acc))
       (let ((letter (char letters i)))
         (setf acc (if (char-equal letter #\A)
                       (list 'car acc)
                       (list 'cdr acc))))
       (setf i (- i 1))
       (go scan))))

(defun %expand-setf-cxr-place (place value letters)
  "Expand a CXR setf PLACE using LETTERS to select rplaca or rplacd."
  (let* ((v (gensym "V"))
         (parent (%expand-cxr-letters (subseq letters 1) (second place)))
         (writer (if (char-equal (char letters 0) #\A) 'rplaca 'rplacd)))
    `(let ((,v ,value)) (,writer ,parent ,v) ,v)))

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
      (t
       (let ((letters (%setf-cxr-letters (car place))))
         (unless letters
           (error "Unsupported cons setf place: ~S" place))
         (%expand-setf-cxr-place place value letters))))))
