;;;; packages/javascript/src/runtime.lisp — JavaScript runtime builtins
;;;;
;;;; Implements JS semantics in Common Lisp.
;;;; JS objects  => CL hash tables (:test #'equal), key is string
;;;; JS arrays   => CL adjustable vectors (element-type t)
;;;; JS null     => :js-null
;;;; JS undefined=> :js-undefined
;;;; JS NaN      => :js-nan
;;;; JS Infinity => :js-infinity / :js-neg-infinity
;;;;
;;;; Private class fields  => stored in a nested HT under key "__private__"

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Type constants
;;; -----------------------------------------------------------------------

(defconstant +js-undefined+ :js-undefined)
(defconstant +js-null+      :js-null)
(defconstant +js-nan+       :js-nan)
(defconstant +js-infinity+  :js-infinity)
(defconstant +js-neg-infinity+ :js-neg-infinity)

;;; Real IEEE-754 double specials, built from raw bit patterns so SBCL never
;;; constant-folds a literal (/ 0.0d0 0.0d0) — which would trap at compile time
;;; with FLOATING-POINT-INVALID-OPERATION. make-double-float does no FP math.
(declaim (type double-float *js-nan-float* *js-inf-float* *js-neg-inf-float*))
(defparameter *js-nan-float*     (sb-kernel:make-double-float #x7FF80000 0))   ; quiet NaN
(defparameter *js-inf-float*     (sb-kernel:make-double-float #x7FF00000 0))   ; +Infinity
(defparameter *js-neg-inf-float* (sb-kernel:make-double-float -1048576 0))     ; -Infinity (#xFFF00000)

;;; -----------------------------------------------------------------------
;;;  Internal helpers
;;; -----------------------------------------------------------------------

(defun %js-make-ht (&optional (size 8))
  (make-hash-table :test #'equal :size size))

(defun %js-make-vec (&optional (size 0))
  (make-array size :element-type t :adjustable t :fill-pointer size))

(defun %js-vec-p (x)
  (and (vectorp x) (not (stringp x))))

(defun %js-ht-p (x)
  (hash-table-p x))

(defun %js-float-nan-p (x)
  "Portable NaN test: NaN is the only float not numerically equal to itself."
  (and (floatp x) (/= x x)))

(defun %js-float-infinity-p (x)
  "Portable infinity test against the largest finite double magnitudes."
  (and (floatp x)
       (or (> x most-positive-double-float)
           (< x most-negative-double-float))))

(defun %js-nan-p (x)
  (or (eq x :js-nan)
      (%js-float-nan-p x)))

(defun %js-infinite-p (x)
  (or (eq x :js-infinity)
      (eq x :js-neg-infinity)
      (and (floatp x) (%js-float-infinity-p x))))

;;; Convert JS number representation to CL real (for arithmetic).
;;; :js-nan -> *js-nan-val*, :js-infinity -> most-positive-double-float, etc.
(defun %cl-number (x)
  (cond
    ((eq x :js-nan)          *js-nan-float*)
    ((eq x :js-infinity)     *js-inf-float*)
    ((eq x :js-neg-infinity) *js-neg-inf-float*)
    ((numberp x) (coerce x 'double-float))
    (t (error "JS TypeError: ~A is not a number" x))))

;;; -----------------------------------------------------------------------
;;;  Type system
;;; -----------------------------------------------------------------------

(defun %js-typeof (x)
  "Return JS typeof string for X."
  (cond
    ((eq x +js-undefined+)   "undefined")
    ((eq x +js-null+)        "object")       ; historic quirk
    ((eq x t)                "boolean")
    ((eq x nil)              "boolean")
    ((stringp x)             "string")
    ((numberp x)             "number")
    ((eq x :js-nan)          "number")
    ((eq x :js-infinity)     "number")
    ((eq x :js-neg-infinity) "number")
    ((%js-vec-p x)           "object")
    ((%js-ht-p x)
     (let ((callable (gethash "__call__" x)))
       (if callable "function" "object")))
    ((functionp x)           "function")
    (t                       "object")))

(defun %js-truthy (x)
  "JS truthiness: false, 0, NaN, \"\", null, undefined are falsy."
  (not (or (eq x nil)
           (eq x +js-undefined+)
           (eq x +js-null+)
           (eq x :js-nan)
           (eql x 0)
           (eql x 0.0d0)
           (eql x -0.0d0)
           (equal x ""))))

(defun %js-not-nullish (x)
  "True if X is not null or undefined."
  (not (or (eq x +js-null+) (eq x +js-undefined+))))

(defun %js-to-number (x)
  "JS ToNumber coercion."
  (cond
    ((numberp x)             (coerce x 'double-float))
    ((eq x +js-undefined+)  *js-nan-float*)
    ((eq x +js-null+)       0.0d0)
    ((eq x t)               1.0d0)
    ((eq x nil)             0.0d0)
    ((eq x :js-nan)         *js-nan-float*)
    ((eq x :js-infinity)    *js-inf-float*)
    ((eq x :js-neg-infinity)*js-neg-inf-float*)
    ((stringp x)
     (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) x))
            (val (if (string= trimmed "")
                     0.0d0
                     ;; SECURITY: bind *read-eval* to nil so a string such as
                     ;; "#.(...)" cannot trigger read-time code execution, and
                     ;; require the parsed datum to be a real number (read can
                     ;; otherwise yield symbols/lists) — JS ToNumber must only
                     ;; ever produce a number or NaN.
                     (handler-case
                         (let* ((*read-eval* nil)
                                (datum (read-from-string trimmed)))
                           (if (realp datum)
                               (coerce datum 'double-float)
                               *js-nan-float*))
                       (error () *js-nan-float*)))))
       val))
    (t *js-nan-float*)))

(defun %js-loose-eq (a b)
  "JS == with type coercion."
  (cond
    ;; strict identity first
    ((equal a b) t)
    ;; null == undefined
    ((and (eq a +js-null+) (eq b +js-undefined+)) t)
    ((and (eq a +js-undefined+) (eq b +js-null+)) t)
    ;; number == string  => coerce string to number
    ((and (numberp a) (stringp b))
     (%js-loose-eq a (%js-to-number b)))
    ((and (stringp a) (numberp b))
     (%js-loose-eq (%js-to-number a) b))
    ;; boolean => number
    ((eq a t)  (%js-loose-eq 1.0d0 b))
    ((eq a nil) (%js-loose-eq 0.0d0 b))
    ((eq b t)  (%js-loose-eq a 1.0d0))
    ((eq b nil) (%js-loose-eq a 0.0d0))
    ;; NaN never equals anything
    ((%js-nan-p a) nil)
    ((%js-nan-p b) nil)
    ;; numeric comparison
    ((and (numberp a) (numberp b)) (= a b))
    (t nil)))

(defun %js-strict-eq (a b)
  "JS === strict equality, no coercion."
  (cond
    ((and (%js-nan-p a) (%js-nan-p b)) nil)  ; NaN !== NaN
    ((and (numberp a) (numberp b)) (= a b))
    (t (equal a b))))

(defun %js-instanceof (obj constructor)
  "JS instanceof. constructor must be a hash-table with __prototype__."
  (when (%js-ht-p obj)
    (let ((proto (and (%js-ht-p constructor)
                      (gethash "__prototype__" constructor))))
      (when proto
        (let ((obj-proto (gethash "__proto__" obj)))
          (loop while (%js-ht-p obj-proto)
                when (eq obj-proto proto) return t
                do (setf obj-proto (gethash "__proto__" obj-proto))
                finally (return nil)))))))

;;; -----------------------------------------------------------------------
;;;  Property access
;;; -----------------------------------------------------------------------

(defun %js-get-prop (obj key)
  "Get property KEY from JS object/array/string."
  (let ((k (%js-to-string key)))
    (cond
      ((%js-vec-p obj)
       (cond
         ((string= k "length") (length obj))
         ((every #'digit-char-p k)
          (let ((idx (parse-integer k)))
            (if (< idx (length obj))
                (aref obj idx)
                +js-undefined+)))
         (t +js-undefined+)))
      ((stringp obj)
       (cond
         ((string= k "length") (length obj))
         ((every #'digit-char-p k)
          (let ((idx (parse-integer k)))
            (if (< idx (length obj))
                (string (char obj idx))
                +js-undefined+)))
         (t +js-undefined+)))
      ((%js-ht-p obj)
       (multiple-value-bind (val found) (gethash k obj)
         (if found val +js-undefined+)))
      ((eq obj +js-null+) (error "JS TypeError: Cannot read properties of null"))
      ((eq obj +js-undefined+) (error "JS TypeError: Cannot read properties of undefined"))
      (t +js-undefined+))))

(defun %js-set-prop (obj key value)
  "Set property KEY on JS object/array."
  (let ((k (%js-to-string key)))
    (cond
      ((%js-vec-p obj)
       (cond
         ((string= k "length")
          (let ((new-len (truncate value)))
            (adjust-array obj new-len :fill-pointer new-len)))
         ((every #'digit-char-p k)
          (let ((idx (parse-integer k)))
            (when (>= idx (length obj))
              (adjust-array obj (1+ idx) :fill-pointer (1+ idx)
                            :initial-element +js-undefined+))
            (setf (aref obj idx) value)))
         (t nil)))
      ((%js-ht-p obj)
       (setf (gethash k obj) value))
      (t nil)))
  value)

(defun %js-delete (obj key)
  "JS delete operator."
  (let ((k (%js-to-string key)))
    (when (%js-ht-p obj)
      (remhash k obj)))
  t)

(defun %js-in (key obj)
  "JS 'key in obj'."
  (let ((k (%js-to-string key)))
    (cond
      ((%js-ht-p obj) (nth-value 1 (gethash k obj)))
      ((%js-vec-p obj)
       (or (string= k "length")
           (and (every #'digit-char-p k)
                (< (parse-integer k) (length obj)))))
      (t nil))))

(defun %js-optional-chain (obj key)
  "Return nil if OBJ is null/undefined, else %js-get-prop."
  (if (%js-not-nullish obj)
      (%js-get-prop obj key)
      +js-undefined+))

(defun %js-optional-call (func &rest args)
  "Call FUNC with ARGS unless FUNC is null/undefined."
  (if (%js-not-nullish func)
      (apply func args)
      +js-undefined+))

;;; -----------------------------------------------------------------------
;;;  String / Template
;;; -----------------------------------------------------------------------

(defun %js-to-string (x)
  "JS ToString coercion."
  (cond
    ((stringp x)             x)
    ((eq x +js-undefined+)  "undefined")
    ((eq x +js-null+)       "null")
    ((eq x t)               "true")
    ((eq x nil)             "false")
    ((eq x :js-nan)         "NaN")
    ((eq x :js-infinity)    "Infinity")
    ((eq x :js-neg-infinity) "-Infinity")
    ((integerp x)           (format nil "~D" x))
    ((floatp x)
     (if (%js-float-nan-p x)
         "NaN"
         (if (%js-float-infinity-p x)
             (if (> x 0) "Infinity" "-Infinity")
             (let ((s (format nil "~F" x)))
               ;; Remove trailing zeros after decimal, JS style
               (string-right-trim "0" (string-right-trim "." s))))))
    ((numberp x)            (format nil "~A" x))
    ((%js-ht-p x)           "[object Object]")
    ((%js-vec-p x)
     (let ((parts (loop for i below (length x)
                        collect (%js-to-string (aref x i)))))
       (format nil "~{~A~^,~}" parts)))
    (t (format nil "~A" x))))

(defun %js-template-string (parts)
  "Concatenate template literal parts (already evaluated)."
  (apply #'concatenate 'string
         (mapcar #'%js-to-string parts)))

(defun %js-concat (a b)
  "JS + operator with string coercion."
  (if (or (stringp a) (stringp b))
      (concatenate 'string (%js-to-string a) (%js-to-string b))
      (let ((na (%js-to-number a))
            (nb (%js-to-number b)))
        (+ na nb))))

;;; -----------------------------------------------------------------------
;;;  Control flow / exceptions
;;; -----------------------------------------------------------------------

(define-condition js-exception ()
  ((value :initarg :value :reader js-exception-value)))

(defun %js-throw (value)
  (error 'js-exception :value value))

(defun %js-try-catch-finally (try-thunk catch-thunk finally-thunk)
  "Execute TRY-THUNK; on JS exception call CATCH-THUNK with the value.
   FINALLY-THUNK is always called. Returns value of try or catch."
  (let ((result +js-undefined+))
    (unwind-protect
         (handler-case
             (setf result (funcall try-thunk))
           (js-exception (c)
             (when catch-thunk
               (setf result (funcall catch-thunk (js-exception-value c))))))
      (when finally-thunk
        (funcall finally-thunk)))
    result))

(defun %js-for-in (obj body-fn)
  "Execute BODY-FN for each enumerable string key in OBJ."
  (when (%js-ht-p obj)
    (maphash (lambda (k v)
               (declare (ignore v))
               (funcall body-fn k))
             obj))
  +js-undefined+)

(defun %js-for-of (iterable body-fn)
  "Execute BODY-FN for each element of ITERABLE (array or string)."
  (cond
    ((%js-vec-p iterable)
     (loop for i below (length iterable)
           do (funcall body-fn (aref iterable i))))
    ((stringp iterable)
     (loop for ch across iterable
           do (funcall body-fn (string ch))))
    ((%js-ht-p iterable)
     ;; Check for iterator protocol: get @@iterator
     (let ((iter-fn (gethash "@@iterator" iterable)))
       (when iter-fn
         (let ((iter (funcall iter-fn)))
           (loop
             (let ((result (funcall (gethash "next" iter))))
               (when (or (not (%js-ht-p result))
                         (%js-truthy (gethash "done" result)))
                 (return))
               (funcall body-fn (gethash "value" result))))))))
    (t nil))
  +js-undefined+)

(defun %js-for-await-of (iterable body-fn)
  "Synchronous implementation of for-await-of (resolves promises eagerly)."
  (%js-for-of iterable (lambda (item)
                         (funcall body-fn (%js-await item)))))

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

(defun %js-array-values (arr)
  "Return copy of arr values."
  (%js-array-slice arr))

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

(defun %js-array-of (&rest items)
  "Create array from arguments."
  (apply #'%js-make-array items))

(defun %js-array-is-array (x)
  "True if X is a JS array."
  (%js-vec-p x))

;;; -----------------------------------------------------------------------
;;;  Objects
;;; -----------------------------------------------------------------------

(defun %js-make-object (&rest key-value-pairs)
  "Create a JS object from alternating key/value args."
  (let ((ht (%js-make-ht (max 8 (length key-value-pairs)))))
    (loop for (k v) on key-value-pairs by #'cddr
          do (setf (gethash (string k) ht) v))
    ht))

(defun %js-object-keys (obj)
  "Return array of own enumerable string keys."
  (if (%js-ht-p obj)
      (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (unless (and (> (length k) 2)
                                (string= (subseq k 0 2) "__"))
                     (vector-push-extend k result)))
                 obj)
        result)
      (%js-make-array)))

(defun %js-object-values (obj)
  "Return array of own enumerable values."
  (if (%js-ht-p obj)
      (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
        (maphash (lambda (k v)
                   (unless (and (> (length k) 2)
                                (string= (subseq k 0 2) "__"))
                     (vector-push-extend v result)))
                 obj)
        result)
      (%js-make-array)))

(defun %js-object-entries (obj)
  "Return array of [key, value] pairs."
  (if (%js-ht-p obj)
      (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
        (maphash (lambda (k v)
                   (unless (and (> (length k) 2)
                                (string= (subseq k 0 2) "__"))
                     (vector-push-extend (%js-make-array k v) result)))
                 obj)
        result)
      (%js-make-array)))

(defun %js-object-assign (target &rest sources)
  "Copy all enumerable own properties from SOURCES to TARGET."
  (dolist (src sources)
    (when (%js-ht-p src)
      (maphash (lambda (k v) (setf (gethash k target) v)) src)))
  target)

(defun %js-object-create (proto)
  "Create object with PROTO as prototype."
  (let ((ht (%js-make-ht)))
    (unless (or (eq proto +js-null+) (null proto))
      (setf (gethash "__proto__" ht) proto))
    ht))

(defun %js-object-freeze (obj)
  "Mark object as frozen (no-op at CL level; returns obj)."
  obj)

(defun %js-object-seal (obj)
  "Mark object as sealed (no-op at CL level; returns obj)."
  obj)

(defun %js-object-define-property (obj key descriptor)
  "Define property with descriptor."
  (let ((k (%js-to-string key)))
    (when (%js-ht-p obj)
      (let ((val (%js-get-prop descriptor "value")))
        (unless (eq val +js-undefined+)
          (setf (gethash k obj) val)))))
  obj)

(defun %js-object-define-properties (obj props)
  "Define multiple properties."
  (when (%js-ht-p props)
    (maphash (lambda (k descriptor)
               (%js-object-define-property obj k descriptor))
             props))
  obj)

(defun %js-object-get-prototype-of (obj)
  "Return prototype of OBJ."
  (if (%js-ht-p obj)
      (multiple-value-bind (v f) (gethash "__proto__" obj)
        (if f v +js-null+))
      +js-null+))

(defun %js-object-set-prototype-of (obj proto)
  "Set prototype of OBJ."
  (when (%js-ht-p obj)
    (if (or (eq proto +js-null+) (null proto))
        (remhash "__proto__" obj)
        (setf (gethash "__proto__" obj) proto)))
  obj)

(defun %js-object-get-own-property-names (obj)
  "Return all own property names including non-enumerable."
  (%js-object-keys obj))

(defun %js-object-get-own-property-descriptor (obj key)
  "Return property descriptor."
  (let ((k (%js-to-string key)))
    (multiple-value-bind (val found) (gethash k obj)
      (if found
          (%js-make-object "value" val "writable" t "enumerable" t "configurable" t)
          +js-undefined+))))

(defun %js-object-has-own (obj key)
  "True if OBJ has own property KEY."
  (when (%js-ht-p obj)
    (nth-value 1 (gethash (%js-to-string key) obj))))

(defun %js-object-from-entries (iterable)
  "Create object from [key, value] iterable."
  (let ((ht (%js-make-ht)))
    (%js-for-of iterable
                (lambda (entry)
                  (let ((k (%js-get-prop entry 0))
                        (v (%js-get-prop entry 1)))
                    (setf (gethash (%js-to-string k) ht) v))))
    ht))

(defun %js-object-is (a b)
  "Object.is — like === but handles NaN and -0."
  (cond
    ((and (%js-nan-p a) (%js-nan-p b)) t)
    ((and (eql a 0.0d0) (eql b -0.0d0)) nil)
    ((and (eql a -0.0d0) (eql b 0.0d0)) nil)
    (t (%js-strict-eq a b))))

(defun %js-object-without-keys (obj keys)
  "Return a copy of OBJ without the given KEYS (vector of strings)."
  (let ((ht (%js-make-ht)))
    (when (%js-ht-p obj)
      (let ((exclude (make-hash-table :test #'equal)))
        (when (%js-vec-p keys)
          (loop for i below (length keys)
                do (setf (gethash (%js-to-string (aref keys i)) exclude) t)))
        (maphash (lambda (k v)
                   (unless (gethash k exclude)
                     (setf (gethash k ht) v)))
                 obj)))
    ht))

(defun %js-object-group-by (iterable key-fn)
  "Group iterable elements by key."
  (let ((ht (%js-make-ht)))
    (%js-for-of iterable
                (lambda (item)
                  (let* ((k (%js-to-string (funcall key-fn item)))
                         (bucket (multiple-value-bind (v f) (gethash k ht)
                                   (if f v
                                       (let ((arr (make-array 0 :element-type t
                                                                :adjustable t
                                                                :fill-pointer 0)))
                                         (setf (gethash k ht) arr)
                                         arr)))))
                    (vector-push-extend item bucket))))
    ht))

;;; -----------------------------------------------------------------------
;;;  Destructuring helpers
;;; -----------------------------------------------------------------------

(defun %js-destructure-array (arr &rest indices-and-defaults)
  "Return a list of values from ARR at given indices (with defaults)."
  (loop for (idx default) on indices-and-defaults by #'cddr
        collect (let ((v (%js-get-prop arr idx)))
                  (if (eq v +js-undefined+) default v))))

(defun %js-destructure-object (obj &rest keys-and-defaults)
  "Return a list of values from OBJ for given keys (with defaults)."
  (loop for (k default) on keys-and-defaults by #'cddr
        collect (let ((v (%js-get-prop obj k)))
                  (if (eq v +js-undefined+) default v))))

;;; -----------------------------------------------------------------------
;;;  String methods
;;; -----------------------------------------------------------------------

(defun %js-string-length (s)
  (length s))

(defun %js-string-slice (s &optional (start 0) (end nil))
  "JS String.prototype.slice."
  (let* ((n (length s))
         (st (if (< start 0) (max 0 (+ n start)) (min start n)))
         (en (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n)))))
    (if (>= st en) "" (subseq s st en))))

(defun %js-string-index-of (s sub &optional (from 0))
  "JS String.prototype.indexOf."
  (let* ((n (length s))
         (st (if (< from 0) 0 (min from n)))
         (found (search sub s :start2 st)))
    (if found found -1)))

(defun %js-string-last-index-of (s sub &optional (from nil))
  "JS String.prototype.lastIndexOf."
  (let* ((n (length s))
         (end (if (null from) n (min n (+ (or from n) (length sub)))))
         (found (search sub s :from-end t :end2 end)))
    (if found found -1)))

(defun %js-string-includes (s sub &optional (from 0))
  "JS String.prototype.includes."
  (not (eql -1 (%js-string-index-of s sub from))))

(defun %js-string-starts-with (s prefix &optional (pos 0))
  (let ((plen (length prefix))
        (slen (length s)))
    (and (<= (+ pos plen) slen)
         (string= s prefix :start1 pos :end1 (+ pos plen)))))

(defun %js-string-ends-with (s suffix &optional (end-pos nil))
  (let* ((slen (length s))
         (ep (if (null end-pos) slen (min end-pos slen)))
         (suflen (length suffix)))
    (and (>= ep suflen)
         (string= s suffix :start1 (- ep suflen) :end1 ep))))

(defun %js-string-split (s &optional (sep nil) (limit nil))
  "JS String.prototype.split."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (cond
      ((or (eq sep +js-undefined+) (null sep))
       (vector-push-extend s result))
      ((string= sep "")
       (loop for ch across s
             do (vector-push-extend (string ch) result)))
      (t
       (let ((seplen (length sep))
             (pos 0)
             (slen (length s)))
         (loop
           (let ((found (search sep s :start2 pos)))
             (unless found (return))
             (vector-push-extend (subseq s pos found) result)
             (setf pos (+ found seplen))
             (when (and limit (>= (length result) limit)) (return))))
         (unless (and limit (>= (length result) limit))
           (vector-push-extend (subseq s pos) result)))))
    (if (and limit (%js-truthy limit))
        (%js-array-slice result 0 limit)
        result)))

(defun %js-string-replace (s pattern replacement)
  "JS String.prototype.replace (string pattern only)."
  (let* ((pat (%js-to-string pattern))
         (patlen (length pat))
         (found (search pat s)))
    (if found
        (concatenate 'string
                     (subseq s 0 found)
                     (%js-to-string replacement)
                     (subseq s (+ found patlen)))
        s)))

(defun %js-string-replace-all (s pattern replacement)
  "JS String.prototype.replaceAll."
  (let* ((pat (%js-to-string pattern))
         (rep (%js-to-string replacement))
         (patlen (length pat)))
    (with-output-to-string (out)
      (let ((pos 0))
        (loop
          (let ((found (search pat s :start2 pos)))
            (unless found
              (write-string (subseq s pos) out)
              (return)))
          (write-string (subseq s pos found) out)
          (write-string rep out)
          (setf pos (+ found (max 1 patlen))))))))

(defun %js-string-to-lower-case (s)
  (string-downcase s))

(defun %js-string-to-upper-case (s)
  (string-upcase s))

(defun %js-string-trim (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) s))

(defun %js-string-trim-start (s)
  (string-left-trim '(#\Space #\Tab #\Newline #\Return #\Page) s))

(defun %js-string-trim-end (s)
  (string-right-trim '(#\Space #\Tab #\Newline #\Return #\Page) s))

(defun %js-string-pad-start (s len &optional (fill " "))
  (let* ((fl (if (eq fill +js-undefined+) " " fill))
         (need (- len (length s))))
    (if (<= need 0)
        s
        (let ((pad (make-string need :initial-element #\Space)))
          (loop for i below need
                do (setf (char pad i) (char fl (mod i (max 1 (length fl))))))
          (concatenate 'string pad s)))))

(defun %js-string-pad-end (s len &optional (fill " "))
  (let* ((fl (if (eq fill +js-undefined+) " " fill))
         (need (- len (length s))))
    (if (<= need 0)
        s
        (let ((pad (make-string need :initial-element #\Space)))
          (loop for i below need
                do (setf (char pad i) (char fl (mod i (max 1 (length fl))))))
          (concatenate 'string s pad)))))

(defun %js-string-at (s index)
  "JS String.prototype.at (negative indexing)."
  (let* ((n (length s))
         (i (if (< index 0) (+ n index) index)))
    (if (or (< i 0) (>= i n))
        +js-undefined+
        (string (char s i)))))

(defun %js-string-repeat (s n)
  "Repeat S n times."
  (if (<= n 0)
      ""
      (with-output-to-string (out)
        (loop repeat n do (write-string s out)))))

(defun %js-string-char-at (s i)
  (if (or (< i 0) (>= i (length s)))
      ""
      (string (char s i))))

(defun %js-string-char-code-at (s i)
  (if (or (< i 0) (>= i (length s)))
      :js-nan
      (char-code (char s i))))

(defun %js-string-code-point-at (s i)
  "JS String.prototype.codePointAt."
  (%js-string-char-code-at s i))

(defun %js-string-concat (s &rest others)
  (apply #'concatenate 'string s (mapcar #'%js-to-string others)))

(defun %js-string-match (s pattern)
  "Simplified string match (pattern is a string)."
  (let ((found (search pattern s)))
    (if found
        (%js-make-array (subseq s found (+ found (length pattern))))
        +js-null+)))

(defun %js-string-match-all (s pattern)
  "Simplified matchAll — returns array of match arrays."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (pos 0)
        (patlen (max 1 (length pattern))))
    (loop
      (let ((found (search pattern s :start2 pos)))
        (unless found (return))
        (vector-push-extend (%js-make-array (subseq s found (+ found (length pattern)))) result)
        (setf pos (+ found patlen))))
    result))

(defun %js-string-search (s pattern)
  "Return index of first match or -1."
  (let ((found (search pattern s)))
    (if found found -1)))

(defun %js-string-normalize (s &optional (form "NFC"))
  "Return S (normalization not implemented at CL level)."
  (declare (ignore form))
  s)

(defun %js-string-from-char-code (&rest codes)
  "String.fromCharCode."
  (coerce (mapcar #'code-char codes) 'string))

(defun %js-string-from-code-point (&rest codes)
  "String.fromCodePoint."
  (coerce (mapcar #'code-char codes) 'string))

(defun %js-string-raw (template &rest substitutions)
  "String.raw tag function."
  (let ((raw (if (%js-ht-p template)
                 (gethash "raw" template)
                 template)))
    (with-output-to-string (out)
      (loop for i below (length raw)
            do (write-string (%js-to-string (aref raw i)) out)
               (when (< i (length substitutions))
                 (write-string (%js-to-string (nth i substitutions)) out))))))

;;; -----------------------------------------------------------------------
;;;  Math
;;; -----------------------------------------------------------------------

(defun %js-math-abs (x)
  (let ((n (%js-to-number x)))
    (if (%js-float-nan-p n) n (abs n))))

(defun %js-math-floor (x)
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (floor n) 'double-float))))

(defun %js-math-ceil (x)
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (ceiling n) 'double-float))))

(defun %js-math-round (x)
  "JS Math.round — rounds half away from negative infinity."
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (floor (+ n 0.5d0)) 'double-float))))

(defun %js-math-trunc (x)
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (truncate n) 'double-float))))

(defun %js-math-sign (x)
  (let ((n (%js-to-number x)))
    (cond
      ((%js-float-nan-p n) n)
      ((> n 0)  1.0d0)
      ((< n 0) -1.0d0)
      (t         0.0d0))))

(defun %js-math-max (&rest args)
  (if (null args)
      *js-neg-inf-float*
      (reduce (lambda (a b)
                (let ((na (%js-to-number a))
                        (nb (%js-to-number b)))
                  (if (or (%js-float-nan-p na)
                          (%js-float-nan-p nb))
                      *js-nan-float*
                      (max na nb))))
              args)))

(defun %js-math-min (&rest args)
  (if (null args)
      *js-inf-float*
      (reduce (lambda (a b)
                (let ((na (%js-to-number a))
                        (nb (%js-to-number b)))
                  (if (or (%js-float-nan-p na)
                          (%js-float-nan-p nb))
                      *js-nan-float*
                      (min na nb))))
              args)))

(defun %js-math-pow (base exp)
  (expt (%js-to-number base) (%js-to-number exp)))

(defun %js-math-sqrt (x)
  (let ((n (%js-to-number x)))
    (if (< n 0)
        *js-nan-float*
        (sqrt n))))

(defun %js-math-random ()
  (random 1.0d0))

(defun %js-math-log (x)
  (let ((n (%js-to-number x)))
    (if (< n 0) *js-nan-float*
        (log n))))

(defun %js-math-log2 (x)
  (let ((n (%js-to-number x)))
    (if (< n 0) *js-nan-float*
        (log n 2.0d0))))

(defun %js-math-log10 (x)
  (let ((n (%js-to-number x)))
    (if (< n 0) *js-nan-float*
        (log n 10.0d0))))

(defun %js-math-exp (x)
  (exp (%js-to-number x)))

(defun %js-math-sin (x)  (sin  (%js-to-number x)))
(defun %js-math-cos (x)  (cos  (%js-to-number x)))
(defun %js-math-tan (x)  (tan  (%js-to-number x)))
(defun %js-math-asin (x) (asin (%js-to-number x)))
(defun %js-math-acos (x) (acos (%js-to-number x)))
(defun %js-math-atan (x) (atan (%js-to-number x)))

(defun %js-math-atan2 (y x)
  (atan (%js-to-number y) (%js-to-number x)))

(defun %js-math-hypot (&rest args)
  (sqrt (reduce #'+ (mapcar (lambda (x)
                               (let ((n (%js-to-number x)))
                                 (* n n)))
                             args)
                :initial-value 0.0d0)))

(defun %js-math-clz32 (x)
  "Count leading zeros in 32-bit integer representation."
  (let* ((n (logand (truncate (%js-to-number x)) #xFFFFFFFF))
         (count 0))
    (if (zerop n)
        32
        (progn
          (loop for bit from 31 downto 0
                while (zerop (logand n (ash 1 bit)))
                do (incf count))
          count))))

(defun %js-math-fround (x)
  "Round to nearest single-precision float."
  (coerce (coerce (%js-to-number x) 'single-float) 'double-float))

(defun %js-math-imul (a b)
  "32-bit integer multiply."
  (let* ((ia (logand (truncate (%js-to-number a)) #xFFFFFFFF))
         (ib (logand (truncate (%js-to-number b)) #xFFFFFFFF))
         (result (logand (* ia ib) #xFFFFFFFF)))
    ;; sign-extend
    (if (logbitp 31 result)
        (- result #x100000000)
        result)))

;;; -----------------------------------------------------------------------
;;;  Console
;;; -----------------------------------------------------------------------

(defun %js-console-log (&rest args)
  (format t "~{~A~^ ~}~%" (mapcar #'%js-to-string args))
  +js-undefined+)

(defun %js-console-error (&rest args)
  (format *error-output* "~{~A~^ ~}~%" (mapcar #'%js-to-string args))
  +js-undefined+)

(defun %js-console-warn (&rest args)
  (format *error-output* "Warning: ~{~A~^ ~}~%" (mapcar #'%js-to-string args))
  +js-undefined+)

;;; -----------------------------------------------------------------------
;;;  Promise (simplified synchronous model)
;;; -----------------------------------------------------------------------

(defstruct (js-promise (:conc-name js-promise-))
  value
  settled-p
  rejected-p)

(defun %js-promise-resolve (value)
  "Create a resolved promise."
  (make-js-promise :value value :settled-p t :rejected-p nil))

(defun %js-promise-reject (reason)
  "Create a rejected promise."
  (make-js-promise :value reason :settled-p t :rejected-p t))

(defun %js-await (promise)
  "Synchronously unwrap a promise (for simplified async model)."
  (cond
    ((js-promise-p promise)
     (if (js-promise-rejected-p promise)
         (%js-throw (js-promise-value promise))
         (js-promise-value promise)))
    (t promise)))  ; non-promise passthrough

(defun %js-async (thunk)
  "Execute THUNK, wrapping result/exception in a promise."
  (handler-case
      (%js-promise-resolve (funcall thunk))
    (js-exception (c)
      (%js-promise-reject (js-exception-value c)))))

(defun %js-promise-then (promise on-fulfilled &optional on-rejected)
  "Chain a promise."
  (if (js-promise-rejected-p promise)
      (if on-rejected
          (handler-case
              (%js-promise-resolve (funcall on-rejected (js-promise-value promise)))
            (js-exception (c) (%js-promise-reject (js-exception-value c))))
          promise)
      (if on-fulfilled
          (handler-case
              (%js-promise-resolve (funcall on-fulfilled (js-promise-value promise)))
            (js-exception (c) (%js-promise-reject (js-exception-value c))))
          promise)))

(defun %js-promise-catch (promise on-rejected)
  (%js-promise-then promise nil on-rejected))

(defun %js-promise-finally (promise on-finally)
  "Run ON-FINALLY regardless of outcome."
  (funcall on-finally)
  promise)

(defun %js-promise-all (promises)
  "Resolve all promises; reject on first rejection."
  (let ((results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (if (%js-vec-p promises) promises (%js-array-from promises))))
    (loop for i below (length arr)
          for p = (aref arr i)
          do (let ((resolved (%js-await p)))
               (vector-push-extend resolved results)))
    (%js-promise-resolve results)))

(defun %js-promise-all-settled (promises)
  "Return array of outcome objects for all promises."
  (let ((results (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (if (%js-vec-p promises) promises (%js-array-from promises))))
    (loop for i below (length arr)
          for p = (aref arr i)
          do (let ((outcome
                    (if (and (js-promise-p p) (js-promise-rejected-p p))
                        (%js-make-object "status" "rejected"
                                         "reason" (js-promise-value p))
                        (%js-make-object "status" "fulfilled"
                                         "value"  (if (js-promise-p p)
                                                      (js-promise-value p)
                                                      p)))))
               (vector-push-extend outcome results)))
    (%js-promise-resolve results)))

(defun %js-promise-any (promises)
  "Resolve with first fulfillment; reject if all reject."
  (let ((errors (make-array 0 :element-type t :adjustable t :fill-pointer 0))
        (arr (if (%js-vec-p promises) promises (%js-array-from promises))))
    (loop for i below (length arr)
          for p = (aref arr i)
          do (if (and (js-promise-p p) (js-promise-rejected-p p))
                 (vector-push-extend (js-promise-value p) errors)
                 (return-from %js-promise-any
                   (%js-promise-resolve (if (js-promise-p p)
                                            (js-promise-value p)
                                            p)))))
    (%js-promise-reject
     (%js-make-object "errors" errors "message" "All promises were rejected"))))

(defun %js-promise-race (promises)
  "Return the first settled promise."
  (let ((arr (if (%js-vec-p promises) promises (%js-array-from promises))))
    (if (zerop (length arr))
        (make-js-promise :settled-p nil :rejected-p nil :value +js-undefined+)
        (aref arr 0))))

(defun %js-promise-with-resolvers ()
  "Return object with promise, resolve, reject."
  (let* ((p (make-js-promise :settled-p nil :rejected-p nil :value +js-undefined+))
         (resolve (lambda (v)
                    (setf (js-promise-value p) v
                          (js-promise-settled-p p) t
                          (js-promise-rejected-p p) nil)))
         (reject (lambda (r)
                   (setf (js-promise-value p) r
                         (js-promise-settled-p p) t
                         (js-promise-rejected-p p) t))))
    (%js-make-object "promise" p "resolve" resolve "reject" reject)))

;;; -----------------------------------------------------------------------
;;;  Generator (simplified coroutine model via CL closures)
;;; -----------------------------------------------------------------------

(defstruct (js-generator (:conc-name js-generator-))
  thunk
  done-p
  value)

(defun %js-make-generator (thunk)
  "Create a generator from a function that accepts a next-value continuation."
  (make-js-generator :thunk thunk :done-p nil :value +js-undefined+))

(defun %js-generator-next (gen &optional (value +js-undefined+))
  "Advance generator by one step."
  (if (js-generator-done-p gen)
      (%js-make-object "value" +js-undefined+ "done" t)
      (handler-case
          (let ((result (funcall (js-generator-thunk gen) value)))
            (if (and (%js-ht-p result)
                     (gethash "done" result))
                (progn
                  (setf (js-generator-done-p gen) t)
                  result)
                result))
        (js-exception (c)
          (setf (js-generator-done-p gen) t)
          (%js-throw (js-exception-value c))))))

(defun %js-yield (value)
  "Yield from a generator — placeholder; real yield requires compiler support."
  (%js-make-object "value" value "done" nil))

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

(defun %js-set-values (s)
  (%js-set-keys s))

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

(defun %js-set-is-superset-of (a b)
  "True if every element of B is in A."
  (%js-set-is-subset-of b a))

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

;;; -----------------------------------------------------------------------
;;;  Class / OOP
;;; -----------------------------------------------------------------------

(defun %js-new (constructor &optional (args nil))
  "Instantiate JS class.  CONSTRUCTOR is a HT with __new__ or __prototype__."
  (cond
    ((and (%js-ht-p constructor) (gethash "__new__" constructor))
     (apply (gethash "__new__" constructor) args))
    ((functionp constructor)
     (apply constructor args))
    (t
     (let ((obj (%js-make-ht)))
       (when (%js-ht-p constructor)
         (let ((proto (gethash "__prototype__" constructor)))
           (when proto (setf (gethash "__proto__" obj) proto)))
         (let ((ctor (gethash "__constructor__" constructor)))
           (when ctor
             (apply ctor obj args))))
       obj))))

(defun %js-class-private-field-get (obj field-name)
  "Read a private field from OBJ."
  (let ((privates (and (%js-ht-p obj) (gethash "__private__" obj))))
    (if (and privates (%js-ht-p privates))
        (multiple-value-bind (v f) (gethash field-name privates)
          (if f v +js-undefined+))
        +js-undefined+)))

(defun %js-class-private-field-set (obj field-name value)
  "Write a private field on OBJ."
  (when (%js-ht-p obj)
    (let ((privates (gethash "__private__" obj)))
      (unless (and privates (%js-ht-p privates))
        (setf privates (%js-make-ht)
              (gethash "__private__" obj) privates))
      (setf (gethash field-name privates) value)))
  value)

(defun %js-has-private-field (obj field-name)
  "True if OBJ has the named private field."
  (let ((privates (and (%js-ht-p obj) (gethash "__private__" obj))))
    (if (and privates (%js-ht-p privates))
        (nth-value 1 (gethash field-name privates))
        nil)))

;;; -----------------------------------------------------------------------
;;;  Nullish coalesce
;;; -----------------------------------------------------------------------

(defun %js-nullish-coalesce (a b)
  "JS ?? operator."
  (if (%js-not-nullish a) a b))

;;; -----------------------------------------------------------------------
;;;  Misc
;;; -----------------------------------------------------------------------

(defun %js-void (x)
  "JS void operator."
  (declare (ignore x))
  +js-undefined+)

(defun %js-debugger ()
  "JS debugger statement — no-op."
  +js-undefined+)

(defun %js-import (module-name &optional with-opts)
  "Dynamic import (stub — returns empty namespace object)."
  (declare (ignore with-opts))
  (%js-promise-resolve
   (%js-make-object "default" +js-undefined+ "__moduleName__" module-name)))

(defun %js-export (kind value)
  "Mark a value as exported (stub — returns value)."
  (declare (ignore kind))
  value)

(defun %js-spread (iterable)
  "Expand iterable into a list (for use with apply)."
  (cond
    ((%js-vec-p iterable)
     (loop for i below (length iterable) collect (aref iterable i)))
    ((stringp iterable)
     (loop for ch across iterable collect (string ch)))
    (t
     (%js-iterator-to-array
      (if (%js-ht-p iterable)
          (let ((iter-fn (gethash "@@iterator" iterable)))
            (if iter-fn (funcall iter-fn) iterable))
          iterable)))))

;;; -----------------------------------------------------------------------
;;;  Built-in dispatch table
;;; -----------------------------------------------------------------------

(defvar *js-builtin-map*
  (let ((ht (make-hash-table :test #'equal)))
    ;; Type
    (setf (gethash "typeof"         ht) #'%js-typeof)
    (setf (gethash "instanceof"     ht) #'%js-instanceof)
    ;; Math
    (setf (gethash "Math.abs"       ht) #'%js-math-abs)
    (setf (gethash "Math.floor"     ht) #'%js-math-floor)
    (setf (gethash "Math.ceil"      ht) #'%js-math-ceil)
    (setf (gethash "Math.round"     ht) #'%js-math-round)
    (setf (gethash "Math.trunc"     ht) #'%js-math-trunc)
    (setf (gethash "Math.sign"      ht) #'%js-math-sign)
    (setf (gethash "Math.max"       ht) #'%js-math-max)
    (setf (gethash "Math.min"       ht) #'%js-math-min)
    (setf (gethash "Math.pow"       ht) #'%js-math-pow)
    (setf (gethash "Math.sqrt"      ht) #'%js-math-sqrt)
    (setf (gethash "Math.random"    ht) #'%js-math-random)
    (setf (gethash "Math.log"       ht) #'%js-math-log)
    (setf (gethash "Math.log2"      ht) #'%js-math-log2)
    (setf (gethash "Math.log10"     ht) #'%js-math-log10)
    (setf (gethash "Math.exp"       ht) #'%js-math-exp)
    (setf (gethash "Math.sin"       ht) #'%js-math-sin)
    (setf (gethash "Math.cos"       ht) #'%js-math-cos)
    (setf (gethash "Math.tan"       ht) #'%js-math-tan)
    (setf (gethash "Math.asin"      ht) #'%js-math-asin)
    (setf (gethash "Math.acos"      ht) #'%js-math-acos)
    (setf (gethash "Math.atan"      ht) #'%js-math-atan)
    (setf (gethash "Math.atan2"     ht) #'%js-math-atan2)
    (setf (gethash "Math.hypot"     ht) #'%js-math-hypot)
    (setf (gethash "Math.clz32"     ht) #'%js-math-clz32)
    (setf (gethash "Math.fround"    ht) #'%js-math-fround)
    (setf (gethash "Math.imul"      ht) #'%js-math-imul)
    ;; Array
    (setf (gethash "Array.isArray"  ht) #'%js-array-is-array)
    (setf (gethash "Array.from"     ht) #'%js-array-from)
    (setf (gethash "Array.of"       ht) #'%js-array-of)
    ;; Object
    (setf (gethash "Object.keys"    ht) #'%js-object-keys)
    (setf (gethash "Object.values"  ht) #'%js-object-values)
    (setf (gethash "Object.entries" ht) #'%js-object-entries)
    (setf (gethash "Object.assign"  ht) #'%js-object-assign)
    (setf (gethash "Object.create"  ht) #'%js-object-create)
    (setf (gethash "Object.freeze"  ht) #'%js-object-freeze)
    (setf (gethash "Object.fromEntries" ht) #'%js-object-from-entries)
    (setf (gethash "Object.hasOwn"  ht) #'%js-object-has-own)
    (setf (gethash "Object.is"      ht) #'%js-object-is)
    (setf (gethash "Object.groupBy" ht) #'%js-object-group-by)
    ;; String
    (setf (gethash "String.fromCharCode"  ht) #'%js-string-from-char-code)
    (setf (gethash "String.fromCodePoint" ht) #'%js-string-from-code-point)
    (setf (gethash "String.raw"     ht) #'%js-string-raw)
    ;; Promise
    (setf (gethash "Promise.resolve"      ht) #'%js-promise-resolve)
    (setf (gethash "Promise.reject"       ht) #'%js-promise-reject)
    (setf (gethash "Promise.all"          ht) #'%js-promise-all)
    (setf (gethash "Promise.allSettled"   ht) #'%js-promise-all-settled)
    (setf (gethash "Promise.any"          ht) #'%js-promise-any)
    (setf (gethash "Promise.race"         ht) #'%js-promise-race)
    (setf (gethash "Promise.withResolvers" ht) #'%js-promise-with-resolvers)
    ;; console
    (setf (gethash "console.log"    ht) #'%js-console-log)
    (setf (gethash "console.error"  ht) #'%js-console-error)
    (setf (gethash "console.warn"   ht) #'%js-console-warn)
    ht)
  "Dispatch table from JS built-in name to CL function.")

;;; -----------------------------------------------------------------------
;;;  Supported-form check
;;; -----------------------------------------------------------------------

(defun js-check-supported-forms (forms)
  "Return FORMS unchanged (hook for future validation)."
  forms)
