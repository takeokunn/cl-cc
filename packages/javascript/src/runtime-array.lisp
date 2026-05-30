;;;; packages/javascript/src/runtime-array.lisp — JS Array built-ins
;;;;
;;;; Array representation: CL adjustable vectors (element-type t).

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Arrays
;;; -----------------------------------------------------------------------

(defun %js-make-array (&rest elements)
  "Create a JS array from ELEMENTS."
  (let* ((n (length elements))
         (arr (make-array n :element-type t :adjustable t :fill-pointer n)))
    (loop for el in elements for i from 0
          do (setf (aref arr i) el))
    arr))

(defun %js-array-push (arr val)
  "Append VAL to ARR; return new length."
  (vector-push-extend val arr)
  (length arr))

(defun %js-array-pop (arr)
  "Remove and return last element, or undefined."
  (if (zerop (length arr))
      +js-undefined+
      (let ((val (aref arr (1- (length arr)))))
        (decf (fill-pointer arr))
        val)))

(defun %js-array-shift (arr)
  "Remove and return first element."
  (if (zerop (length arr))
      +js-undefined+
      (let ((val (aref arr 0)))
        (loop for i from 0 below (1- (length arr))
              do (setf (aref arr i) (aref arr (1+ i))))
        (decf (fill-pointer arr))
        val)))

(defun %js-array-unshift (arr &rest items)
  "Prepend ITEMS to ARR; return new length."
  (let* ((n (length items))
         (old-len (length arr))
         (new-len (+ old-len n)))
    (adjust-array arr new-len :fill-pointer new-len)
    ;; shift existing elements right
    (loop for i from (1- old-len) downto 0
          do (setf (aref arr (+ i n)) (aref arr i)))
    ;; insert new items
    (loop for item in items for i from 0
          do (setf (aref arr i) item))
    new-len))

(defun %js-array-map (arr fn)
  "Map FN over ARR, returning new array."
  (let* ((n (length arr))
         (result (%js-make-vec n)))
    (loop for i below n
          do (setf (aref result i)
                   (funcall fn (aref arr i) i arr)))
    result))

(defun %js-array-filter (arr fn)
  "Filter ARR by predicate FN."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (loop for i below (length arr)
          when (%js-truthy (funcall fn (aref arr i) i arr))
            do (vector-push-extend (aref arr i) result))
    result))

(defun %js-array-reduce (arr fn &optional (init +js-undefined+))
  "Reduce ARR with FN and optional INIT."
  (let ((acc init)
        (start 0))
    (when (eq acc +js-undefined+)
      (when (zerop (length arr))
        (error "JS TypeError: Reduce of empty array with no initial value"))
      (setf acc (aref arr 0)
            start 1))
    (loop for i from start below (length arr)
          do (setf acc (funcall fn acc (aref arr i) i arr)))
    acc))

(defun %js-array-reduce-right (arr fn &optional (init +js-undefined+))
  "Reduce ARR from right with FN."
  (let ((acc init)
        (end (1- (length arr))))
    (when (eq acc +js-undefined+)
      (when (zerop (length arr))
        (error "JS TypeError: Reduce of empty array with no initial value"))
      (setf acc (aref arr end)
            end (- end 1)))
    (loop for i from end downto 0
          do (setf acc (funcall fn acc (aref arr i) i arr)))
    acc))

(defun %js-array-find (arr fn)
  "Return first element matching FN, or undefined."
  (loop for i below (length arr)
        when (%js-truthy (funcall fn (aref arr i) i arr))
          return (aref arr i)
        finally (return +js-undefined+)))

(defun %js-array-find-index (arr fn)
  "Return index of first element matching FN, or -1."
  (loop for i below (length arr)
        when (%js-truthy (funcall fn (aref arr i) i arr))
          return i
        finally (return -1)))

(defun %js-array-some (arr fn)
  "True if any element satisfies FN."
  (loop for i below (length arr)
        when (%js-truthy (funcall fn (aref arr i) i arr))
          return t
        finally (return nil)))

(defun %js-array-every (arr fn)
  "True if all elements satisfy FN."
  (loop for i below (length arr)
        unless (%js-truthy (funcall fn (aref arr i) i arr))
          return nil
        finally (return t)))

(defun %js-array-includes (arr val &optional (from 0))
  "True if ARR contains VAL starting from FROM."
  (let* ((n (length arr))
         (start (if (< from 0) (max 0 (+ n from)) from)))
    (loop for i from start below n
          when (%js-strict-eq (aref arr i) val)
            return t
          finally (return nil))))

(defun %js-array-index-of (arr val &optional (from 0))
  "Return first index of VAL in ARR, or -1."
  (let* ((n (length arr))
         (start (if (< from 0) (max 0 (+ n from)) from)))
    (loop for i from start below n
          when (%js-strict-eq (aref arr i) val)
            return i
          finally (return -1))))

(defun %js-array-last-index-of (arr val &optional (from nil))
  "Return last index of VAL in ARR, or -1."
  (let* ((n (length arr))
         (end (if from
                  (if (< from 0) (+ n from) (min from (1- n)))
                  (1- n))))
    (loop for i from end downto 0
          when (%js-strict-eq (aref arr i) val)
            return i
          finally (return -1))))

(defun %js-array-join (arr &optional (sep ","))
  "Join array elements with separator."
  (let ((s (if (eq sep +js-undefined+) "," sep)))
    (with-output-to-string (out)
      (loop for i below (length arr)
            do (when (> i 0) (write-string s out))
               (let ((el (aref arr i)))
                 (unless (or (eq el +js-undefined+) (eq el +js-null+))
                   (write-string (%js-to-string el) out)))))))

(defun %js-array-slice (arr &optional (start 0) (end nil))
  "Return a new array slice."
  (let* ((n (length arr))
         (s (if (< start 0) (max 0 (+ n start)) (min start n)))
         (e (if (null end)
                n
                (if (< end 0) (max 0 (+ n end)) (min end n))))
         (len (max 0 (- e s)))
         (result (%js-make-vec len)))
    (loop for i from s below (+ s len)
          for j from 0
          do (setf (aref result j) (aref arr i)))
    result))

(defun %js-array-splice (arr start &optional (delete-count nil) &rest items)
  "Splice: remove DELETE-COUNT elements at START, insert ITEMS."
  (let* ((n (length arr))
         (s (if (< start 0) (max 0 (+ n start)) (min start n)))
         (dc (if (null delete-count)
                 (- n s)
                 (min (max 0 delete-count) (- n s))))
         (removed (%js-make-vec dc)))
    ;; collect removed
    (loop for i from s below (+ s dc)
          for j from 0
          do (setf (aref removed j) (aref arr i)))
    ;; build new contents
    (let* ((ni (length items))
           (new-len (+ n ni (- dc)))
           (new-arr (%js-make-vec new-len)))
      (loop for i below s do (setf (aref new-arr i) (aref arr i)))
      (loop for item in items for i from s
            do (setf (aref new-arr i) item))
      (loop for i from (+ s dc) below n
            for j from (+ s ni)
            do (setf (aref new-arr j) (aref arr i)))
      ;; copy new-arr back into arr
      (adjust-array arr new-len :fill-pointer new-len)
      (loop for i below new-len
            do (setf (aref arr i) (aref new-arr i))))
    removed))

(defun %js-array-concat (arr &rest others)
  "Concatenate ARR with OTHERS."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (flet ((extend (x)
             (if (%js-vec-p x)
                 (loop for i below (length x)
                       do (vector-push-extend (aref x i) result))
                 (vector-push-extend x result))))
      (extend arr)
      (dolist (o others) (extend o)))
    result))

(defun %js-array-reverse (arr)
  "Reverse ARR in place; return ARR."
  (let ((n (length arr)))
    (loop for i below (floor n 2)
          do (rotatef (aref arr i) (aref arr (- n 1 i)))))
  arr)

(defun %js-array-sort (arr &optional compare-fn)
  "Sort ARR in place."
  (let ((vec (coerce arr 'list)))
    (let ((sorted (if compare-fn
                      (stable-sort vec (lambda (a b)
                                         (< (funcall compare-fn a b) 0)))
                      (stable-sort vec (lambda (a b)
                                         (string< (%js-to-string a)
                                                  (%js-to-string b)))))))
      (loop for el in sorted for i from 0
            do (setf (aref arr i) el))))
  arr)

(defun %js-array-flat (arr &optional (depth 1))
  "Flatten ARR up to DEPTH levels."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (labels ((flatten (x d)
               (if (and (%js-vec-p x) (> d 0))
                   (loop for i below (length x)
                         do (flatten (aref x i) (1- d)))
                   (vector-push-extend x result))))
      (loop for i below (length arr)
            do (flatten (aref arr i) depth)))
    result))

(defun %js-array-flat-map (arr fn)
  "Map FN then flatten one level."
  (%js-array-flat (%js-array-map arr fn) 1))

(defun %js-array-fill (arr value &optional (start 0) (end nil))
  "Fill ARR[start..end] with VALUE."
  (let* ((n (length arr))
         (s (if (< start 0) (max 0 (+ n start)) (min start n)))
         (e (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n)))))
    (loop for i from s below e
          do (setf (aref arr i) value)))
  arr)

(defun %js-array-copy-within (arr target &optional (start 0) (end nil))
  "Copy elements within ARR."
  (let* ((n (length arr))
         (to   (if (< target 0) (max 0 (+ n target)) (min target n)))
         (from (if (< start 0)  (max 0 (+ n start))  (min start n)))
         (fin  (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n))))
         (count (min (- fin from) (- n to)))
         (tmp (make-array count)))
    (loop for i below count do (setf (aref tmp i) (aref arr (+ from i))))
    (loop for i below count do (setf (aref arr (+ to i)) (aref tmp i))))
  arr)

(defun %js-array-entries (arr)
  "Return vector of [index, value] pairs."
  (let* ((n (length arr))
         (result (%js-make-vec n)))
    (loop for i below n
          do (setf (aref result i) (%js-make-array i (aref arr i))))
    result))

(defun %js-array-keys (arr)
  "Return vector of indices."
  (let* ((n (length arr))
         (result (%js-make-vec n)))
    (loop for i below n do (setf (aref result i) i))
    result))


(defun %js-array-from (iterable &optional map-fn)
  "JS Array.from."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (cond
      ((%js-vec-p iterable)
       (loop for i below (length iterable)
             do (vector-push-extend (aref iterable i) result)))
      ((stringp iterable)
       (loop for ch across iterable
             do (vector-push-extend (string ch) result)))
      (t
       (%js-for-of iterable (lambda (el) (vector-push-extend el result)))))
    (if map-fn
        (%js-array-map result map-fn)
        result)))


(defun %js-array-is-array (x)
  "True if X is a JS array."
  (%js-vec-p x))
