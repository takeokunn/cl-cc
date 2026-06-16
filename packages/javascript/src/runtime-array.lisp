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

(defun %js-list-to-array (list)
  "Create a JS array (adjustable vector) from a Common Lisp LIST. Used to turn a
rest parameter's collected &rest list into a real JS array."
  (apply #'%js-make-array list))

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

;;; -----------------------------------------------------------------------
;;;  Array index coercion
;;; -----------------------------------------------------------------------

(defun %js-array-to-integer (value)
  "Coerce VALUE like ECMAScript ToIntegerOrInfinity for array indices."
  (let ((n (%js-to-number value)))
    (cond
      ((%js-nan-p n) 0)
      ((%js-float-infinity-p n)
       (if (plusp n) most-positive-fixnum most-negative-fixnum))
      (t (truncate n)))))

(defun %js-array-relative-start (index length)
  "Clamp a relative array INDEX into the inclusive range [0, LENGTH]."
  (let ((i (%js-array-to-integer index)))
    (if (< i 0)
        (max 0 (+ length i))
        (min i length))))

(defun %js-array-relative-end (index length)
  "Clamp an optional relative end INDEX into the inclusive range [0, LENGTH]."
  (if (or (null index) (eq index +js-undefined+))
      length
      (%js-array-relative-start index length)))

;;; -----------------------------------------------------------------------
;;;  Array callback iteration
;;; -----------------------------------------------------------------------

(defmacro %js-do-array-elements ((element index array &key result) &body body)
  "Iterate ARRAY once, binding ELEMENT and INDEX for callback-style methods."
  (let ((array-var (gensym "ARRAY-"))
        (length-var (gensym "LENGTH-")))
    `(let* ((,array-var ,array)
            (,length-var (length ,array-var)))
       (loop for ,index below ,length-var
             for ,element = (aref ,array-var ,index)
             do (progn ,@body)
             finally (return ,result)))))

(defmacro %js-array-callback (fn element index array)
  "Call a JS array callback with the standard (element, index, array) shape."
  `(%js-funcall ,fn ,element ,index ,array))

(defun %js-array-map (arr fn)
  "Map FN over ARR, returning new array."
  (let* ((n (length arr))
         (result (%js-make-vec n)))
    (%js-do-array-elements (element i arr :result result)
      (setf (aref result i) (%js-array-callback fn element i arr)))))

(defun %js-array-for-each (arr fn)
  "Call FN(element, index, arr) for each element of ARR; returns undefined
(JS Array.prototype.forEach)."
  (%js-do-array-elements (element i arr :result +js-undefined+)
    (%js-array-callback fn element i arr)))

(defun %js-array-filter (arr fn)
  "Filter ARR by predicate FN."
  (let ((result (%js-make-vec)))
    (%js-do-array-elements (element i arr :result result)
      (when (%js-truthy (%js-array-callback fn element i arr))
        (vector-push-extend element result)))))

(defmacro define-js-array-reducer (name direction docstring)
  "Define reduce/reduceRight from the shared accumulator prolog."
  (let ((initial-index (if (eq direction :forward) 0 '(1- (length arr))))
        (next-index (if (eq direction :forward) 1 '(- i 1)))
        (loop-clause (if (eq direction :forward)
                         '(loop for i from start below (length arr)
                                do (setf acc (%js-funcall fn acc (aref arr i) i arr)))
                         '(loop for i from start downto 0
                                do (setf acc (%js-funcall fn acc (aref arr i) i arr))))))
    `(defun ,name (arr fn &optional (init +js-undefined+))
       ,docstring
       (let ((acc init)
             (start ,initial-index))
         (when (eq acc +js-undefined+)
           (when (zerop (length arr))
             (error "JS TypeError: Reduce of empty array with no initial value"))
           (let ((i start))
             (setf acc (aref arr i)
                   start ,next-index)))
         ,loop-clause
         acc))))

(define-js-array-reducer %js-array-reduce
  :forward
  "Reduce ARR with FN and optional INIT.")

(define-js-array-reducer %js-array-reduce-right
  :reverse
  "Reduce ARR from right with FN.")

(defmacro define-js-array-find-index (name direction docstring)
  "Define forward or reverse Array find-index variants."
  (let ((loop-clause (if (eq direction :forward)
                         '(loop for i below (length arr)
                                for element = (aref arr i)
                                when (%js-truthy (%js-array-callback pred element i arr))
                                  return i
                                finally (return -1))
                         '(loop for i from (1- (length arr)) downto 0
                                for element = (aref arr i)
                                when (%js-truthy (%js-array-callback pred element i arr))
                                  return i
                                finally (return -1)))))
    `(defun ,name (arr pred)
       ,docstring
       ,loop-clause)))

(define-js-array-find-index %js-array-find-index
  :forward
  "Return index of first element satisfying FN, or -1.")

(defun %js-array-find (arr fn)
  "Return first element satisfying FN, or undefined."
  (let ((idx (%js-array-find-index arr fn)))
    (if (= idx -1) +js-undefined+ (aref arr idx))))

(defmacro define-js-array-predicate-test (name clause early-result final-result docstring)
  "Define a JS Array predicate-test (some/every) from the shared loop pattern."
  `(defun ,name (arr fn)
     ,docstring
     (loop for i below (length arr)
           for element = (aref arr i)
           ,clause (%js-truthy (%js-array-callback fn element i arr))
             return ,early-result
           finally (return ,final-result))))

(define-js-array-predicate-test %js-array-some
  when
  t
  nil
  "True if any element satisfies FN.")

(define-js-array-predicate-test %js-array-every
  unless
  nil
  t
  "True if all elements satisfy FN.")

(defun %js-array-includes (arr val &optional (from 0))
  "True if ARR contains VAL starting from FROM."
  (let* ((n (length arr))
         (start (%js-array-relative-start from n)))
    (loop for i from start below n
          when (%js-same-value-zero (aref arr i) val)
            return t
          finally (return nil))))

(defun %js-array-index-of (arr val &optional (from 0))
  "Return first index of VAL in ARR, or -1."
  (let* ((n (length arr))
         (start (%js-array-relative-start from n)))
    (loop for i from start below n
          when (%js-strict-eq (aref arr i) val)
            return i
          finally (return -1))))

(defun %js-array-last-index-of (arr val &optional (from nil))
  "Return last index of VAL in ARR, or -1."
  (let* ((n (length arr))
         (end (if from
                  (let ((i (%js-array-to-integer from)))
                    (if (< i 0) (+ n i) (min i (1- n))))
                  (1- n))))
    (if (< end 0)
        -1
        (loop for i from end downto 0
              when (%js-strict-eq (aref arr i) val)
                return i
              finally (return -1)))))

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
         (s (%js-array-relative-start start n))
         (e (%js-array-relative-end end n))
         (len (max 0 (- e s)))
         (result (%js-make-vec len)))
    (loop for i from s below (+ s len)
          for j from 0
          do (setf (aref result j) (aref arr i)))
    result))

(defun %js-array-splice (arr start &optional (delete-count nil delete-count-supplied-p) &rest items)
  "Splice: remove DELETE-COUNT elements at START, insert ITEMS."
  (let* ((n (length arr))
         (s (%js-array-relative-start start n))
         (dc (if (not delete-count-supplied-p)
                 (- n s)
                 (min (max 0 (%js-array-to-integer delete-count)) (- n s))))
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
  (let ((result (%js-make-vec)))
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

;;; Shared sort predicate builder — used by both mutating sort and toSorted.
;;; Handles nil (no compareFn supplied) and +js-undefined+ (passed from JS).
(defun %js-sort-comparator (compare-fn)
  "Build a CL sort predicate from an optional JS compareFn."
  (if (and compare-fn (not (eq compare-fn +js-undefined+)))
      (lambda (a b) (< (%js-funcall compare-fn a b) 0))
      (lambda (a b) (string< (%js-to-string a) (%js-to-string b)))))

(defun %js-array-sort (arr &optional compare-fn)
  "Sort ARR in place; return ARR."
  (replace arr (stable-sort (copy-seq arr) (%js-sort-comparator compare-fn)))
  arr)

(defun %js-array-flat (arr &optional (depth 1))
  "Flatten ARR up to DEPTH levels."
  (let ((result (%js-make-vec)))
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
         (s (%js-array-relative-start start n))
         (e (%js-array-relative-end end n)))
    (loop for i from s below e
          do (setf (aref arr i) value)))
  arr)

(defun %js-array-copy-within (arr target &optional (start 0) (end nil))
  "Copy elements within ARR."
  (let* ((n (length arr))
         (to (%js-array-relative-start target n))
         (from (%js-array-relative-start start n))
         (fin (%js-array-relative-end end n))
         (count (max 0 (min (- fin from) (- n to))))
         (tmp (make-array count)))
    (loop for i below count do (setf (aref tmp i) (aref arr (+ from i))))
    (loop for i below count do (setf (aref arr (+ to i)) (aref tmp i))))
  arr)

(defmacro define-js-array-iterator (name docstring value-form)
  `(defun ,name (arr)
     ,docstring
     (let ((i 0)
           (n (length arr)))
       (%js-make-cl-iterator
        (lambda ()
          (if (>= i n)
              :done
              (let ((value ,value-form))
                (incf i)
                (cons value nil))))))))

(define-js-array-iterator %js-array-entries
  "Return an Array Iterator yielding [index, value] pairs."
  (%js-make-array i (aref arr i)))

(define-js-array-iterator %js-array-keys
  "Return an Array Iterator yielding indices."
  i)

(defun %js-array-from-map-fn-p (map-fn)
  (and map-fn (not (eq map-fn +js-undefined+)) (not (eq map-fn +js-null+))))

(defun %js-array-from-map-value (map-fn this-arg value index)
  (if (%js-array-from-map-fn-p map-fn)
      (%js-call-with-this this-arg map-fn (list value index))
      value))

(defun %js-array-to-length (value)
  "Coerce VALUE like ECMAScript ToLength."
  (let ((n (%js-to-number value)))
    (cond
      ((or (%js-nan-p n) (<= n 0)) 0)
      ((%js-float-infinity-p n) 9007199254740991)
      (t (min (floor n) 9007199254740991)))))

(defun %js-array-iterable-p (value)
  (or (%js-vec-p value)
      (stringp value)
      (typep value 'js-map)
      (typep value 'js-set)
      (functionp value)
      (and (%js-ht-p value)
           (or (gethash "next" value)
               (gethash "@@iterator" value)))))

(defun %js-array-from (items &optional (map-fn +js-undefined+) (this-arg +js-undefined+))
  "JS Array.from — collects an iterable or array-like object into a fresh array."
  (let ((result (%js-make-vec))
        (index 0))
    (if (%js-array-iterable-p items)
        (%js-for-of
         items
         (lambda (el)
           (vector-push-extend
            (%js-array-from-map-value map-fn this-arg el index)
            result)
           (incf index)))
        (let ((length (%js-array-to-length (%js-get-prop items "length"))))
          (loop for i below length
                do (vector-push-extend
                    (%js-array-from-map-value map-fn this-arg (%js-get-prop items i) i)
                    result))))
    result))

(defun %js-array-from-async (items &optional (map-fn +js-undefined+) (this-arg +js-undefined+))
  "ES2024 Array.fromAsync in the runtime's synchronous Promise model."
  (handler-case
      (let ((result (%js-make-vec))
            (index 0))
        (if (%js-array-iterable-p items)
            (%js-for-of
             items
             (lambda (el)
               (let* ((value (%js-await el))
                      (mapped (%js-array-from-map-value map-fn this-arg value index)))
                 (vector-push-extend (%js-await mapped) result)
                 (incf index))))
            (let ((length (%js-array-to-length (%js-get-prop items "length"))))
              (loop for i below length
                    do (let* ((value (%js-await (%js-get-prop items i)))
                              (mapped (%js-array-from-map-value map-fn this-arg value i)))
                         (vector-push-extend (%js-await mapped) result)))))
        (%js-promise-resolve result))
    (js-exception (c)
      (%js-promise-reject (js-exception-value c)))))


(defun %js-array-is-array (x)
  "True if X is a JS array."
  (%js-vec-p x))
