;;;; packages/javascript/src/runtime-collections.lisp — JS Set and Iterator built-ins
;;;;
;;;; Set is represented as a CL hash-table with values == t.
;;;; Iterators are represented as closures or JS objects with a "next" method.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Set built-ins (ES2025)
;;; -----------------------------------------------------------------------

;;; We represent JS Set as a hash-table with values == t

(defun %js-set-add (s val)
  (setf (gethash val s) t)
  s)

(defun %js-set-delete (s val)
  (multiple-value-bind (v found) (gethash val s)
    (declare (ignore v))
    (when found (remhash val s))
    found))

(defun %js-set-has (s val)
  (nth-value 1 (gethash val s)))

(defun %js-set-clear (s)
  (clrhash s)
  +js-undefined+)

(defun %js-set-size (s)
  (hash-table-count s))

(defun %js-set-keys (s)
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v) (declare (ignore v)) (vector-push-extend k result)) s)
    result))


(defun %js-set-entries (s)
  "Return array of [key, key] pairs (Set semantics)."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (vector-push-extend (%js-make-array k k) result))
             s)
    result))

(defun %js-set-for-each (s fn)
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall fn k k s))
           s)
  +js-undefined+)

(defun %js-set-union (a b)
  "Return new set that is the union of A and B."
  (let ((result (%js-make-ht)))
    (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k result) t)) a)
    (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k result) t)) b)
    result))

(defun %js-set-intersection (a b)
  "Return new set that is the intersection of A and B."
  (let ((result (%js-make-ht)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (nth-value 1 (gethash k b))
                 (setf (gethash k result) t)))
             a)
    result))

(defun %js-set-difference (a b)
  "Return new set: elements in A but not in B."
  (let ((result (%js-make-ht)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (nth-value 1 (gethash k b))
                 (setf (gethash k result) t)))
             a)
    result))

(defun %js-set-symmetric-difference (a b)
  "Return new set: elements in A or B but not both."
  (let ((result (%js-make-ht)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (nth-value 1 (gethash k b))
                 (setf (gethash k result) t)))
             a)
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (nth-value 1 (gethash k a))
                 (setf (gethash k result) t)))
             b)
    result))

(defun %js-set-is-subset-of (a b)
  "True if every element of A is in B."
  (block check
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (nth-value 1 (gethash k b))
                 (return-from check nil)))
             a)
    t))


(defun %js-set-is-disjoint-from (a b)
  "True if A and B share no elements."
  (block check
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (nth-value 1 (gethash k b))
                 (return-from check nil)))
             a)
    t))

;;; -----------------------------------------------------------------------
;;;  Iterator helpers (ES2025)
;;; -----------------------------------------------------------------------

;;; Iterators are represented as closures that return (:value v :done nil/t)
;;; or as JS objects with a "next" method.

(defun %js-iter-next (iter)
  "Advance iter; return (values value done-p)."
  (let ((result (if (functionp iter)
                    (funcall iter)
                    (funcall (gethash "next" iter)))))
    (values (if (%js-ht-p result)
                (gethash "value" result)
                result)
            (if (%js-ht-p result)
                (%js-truthy (gethash "done" result))
                nil))))

(defun %js-make-cl-iterator (get-next-fn)
  "Create a JS iterator object from a CL thunk that returns (value . done)."
  (let ((ht (%js-make-ht)))
    (setf (gethash "next" ht)
          (lambda ()
            (let ((pair (funcall get-next-fn)))
              (if (eq pair :done)
                  (%js-make-object "value" +js-undefined+ "done" t)
                  (%js-make-object "value" (car pair) "done" nil)))))
    ht))

(defun %js-vec-to-iter (vec)
  "Create iterator over a vector."
  (let ((i 0))
    (%js-make-cl-iterator
     (lambda ()
       (if (>= i (length vec))
           :done
           (let ((v (aref vec i)))
             (incf i)
             (cons v nil)))))))

(defun %js-iterator-map (iter fn)
  "Map FN over ITER."
  (let ((src iter))
    (%js-make-cl-iterator
     (lambda ()
       (multiple-value-bind (val done) (%js-iter-next src)
         (if done :done (cons (funcall fn val) nil)))))))

(defun %js-iterator-filter (iter fn)
  "Filter ITER by FN."
  (let ((src iter))
    (%js-make-cl-iterator
     (lambda ()
       (loop
         (multiple-value-bind (val done) (%js-iter-next src)
           (when done (return :done))
           (when (%js-truthy (funcall fn val))
             (return (cons val nil)))))))))

(defun %js-iterator-take (iter n)
  "Take at most N elements from ITER."
  (let ((src iter) (count 0))
    (%js-make-cl-iterator
     (lambda ()
       (if (>= count n)
           :done
           (multiple-value-bind (val done) (%js-iter-next src)
             (if done
                 :done
                 (progn (incf count) (cons val nil)))))))))

(defun %js-iterator-drop (iter n)
  "Skip N elements from ITER, then continue."
  (let ((src iter) (skipped 0))
    (%js-make-cl-iterator
     (lambda ()
       (loop while (< skipped n)
             do (multiple-value-bind (val done) (%js-iter-next src)
                  (declare (ignore val))
                  (if done (return-from %js-iterator-drop
                             (%js-make-cl-iterator (lambda () :done))))
                  (incf skipped)))
       (multiple-value-bind (val done) (%js-iter-next src)
         (if done :done (cons val nil)))))))

(defun %js-iterator-flat-map (iter fn)
  "FlatMap ITER with FN (FN must return an iterable)."
  (let ((src iter)
        (inner nil))
    (%js-make-cl-iterator
     (lambda ()
       (loop
         ;; drain inner iterator first
         (when inner
           (multiple-value-bind (val done) (%js-iter-next inner)
             (unless done (return (cons val nil)))
             (setf inner nil)))
         ;; advance outer
         (multiple-value-bind (val done) (%js-iter-next src)
           (when done (return :done))
           (let ((mapped (funcall fn val)))
             (setf inner (if (%js-vec-p mapped)
                             (%js-vec-to-iter mapped)
                             mapped)))))))))

(defun %js-iterator-reduce (iter fn &optional (init +js-undefined+))
  "Reduce ITER with FN."
  (let ((acc init) (first (eq init +js-undefined+)))
    (loop
      (multiple-value-bind (val done) (%js-iter-next iter)
        (when done (return acc))
        (if first
            (setf acc val first nil)
            (setf acc (funcall fn acc val)))))))

(defun %js-iterator-to-array (iter)
  "Collect ITER into an array."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (loop
      (multiple-value-bind (val done) (%js-iter-next iter)
        (when done (return result))
        (vector-push-extend val result)))))

(defun %js-iterator-for-each (iter fn)
  "Execute FN for each element of ITER."
  (loop
    (multiple-value-bind (val done) (%js-iter-next iter)
      (when done (return +js-undefined+))
      (funcall fn val))))

(defun %js-iterator-some (iter fn)
  "True if any element of ITER satisfies FN."
  (loop
    (multiple-value-bind (val done) (%js-iter-next iter)
      (when done (return nil))
      (when (%js-truthy (funcall fn val)) (return t)))))

(defun %js-iterator-every (iter fn)
  "True if all elements of ITER satisfy FN."
  (loop
    (multiple-value-bind (val done) (%js-iter-next iter)
      (when done (return t))
      (unless (%js-truthy (funcall fn val)) (return nil)))))

(defun %js-iterator-find (iter fn)
  "Return first element of ITER satisfying FN, or undefined."
  (loop
    (multiple-value-bind (val done) (%js-iter-next iter)
      (when done (return +js-undefined+))
      (when (%js-truthy (funcall fn val)) (return val)))))
