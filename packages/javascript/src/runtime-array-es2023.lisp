;;;; packages/javascript/src/runtime-array-es2023.lisp — ES2023/ES2024 Array methods
;;;;
;;;; ES2023 non-mutating methods: toReversed, toSorted, toSpliced, with, findLast,
;;;; findLastIndex, at, flat-nested alias.
;;;; ES2024: Array.of, Array.prototype.group, Array.prototype.groupToMap.
;;;;
;;;; Load order: after runtime-array.lisp (needs %js-array-flat, %js-make-vec,
;;;; %js-make-ht, %js-make-map, %js-map-get, %js-map-set, %js-funcall, %js-truthy,
;;;; %js-to-string, +js-undefined+).

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  ES2023 non-mutating array methods (return copies)
;;; -----------------------------------------------------------------------

(defun %js-array-to-reversed (arr)
  "Array.prototype.toReversed() — return reversed copy."
  (let* ((n (length arr))
         (result (%js-make-vec n)))
    (loop for i below n
          do (setf (aref result i) (aref arr (- n 1 i))))
    result))

(defun %js-array-to-sorted (arr &optional compare-fn)
  "Array.prototype.toSorted([compareFn]) — return sorted copy (stable, non-mutating)."
  (stable-sort (copy-seq arr) (%js-sort-comparator compare-fn)))

(defun %js-array-to-spliced (arr start &optional (delete-count nil delete-count-supplied-p) &rest items)
  "Array.prototype.toSpliced(start, deleteCount, ...items) — return modified copy."
  (let* ((n (length arr))
         (st (%js-array-relative-start start n))
         (dc (if delete-count-supplied-p
                 (max 0 (min (%js-array-to-integer delete-count) (- n st)))
                 (- n st)))
         (before (subseq arr 0 st))
         (after  (subseq arr (+ st dc)))
         (result (%js-make-vec)))
    (loop for x across before do (vector-push-extend x result))
    (dolist (item items) (vector-push-extend item result))
    (loop for x across after  do (vector-push-extend x result))
    result))

(defun %js-array-with (arr index value)
  "Array.prototype.with(index, value) — return copy with element replaced."
  (let* ((n (length arr))
         (i (%js-array-to-integer index))
         (idx (if (< i 0) (+ n i) i))
         (result (copy-seq arr)))
    (unless (and (>= idx 0) (< idx n))
      (%js-throw
       (%js-make-error-instance
        *js-range-error-class*
        (format nil "Invalid array index ~A" index))))
    (setf (aref result idx) value)
    result))

(define-js-array-find-index %js-array-find-last-index
  :reverse
  "Array.prototype.findLastIndex(pred) -- index of last matching element, or -1.")

(defun %js-array-find-last (arr pred)
  "Array.prototype.findLast(pred) -- last element satisfying PRED, or undefined."
  (let ((idx (%js-array-find-last-index arr pred)))
    (if (= idx -1) +js-undefined+ (aref arr idx))))

(defun %js-array-at (arr index)
  "Array.prototype.at(index) — supports negative indices."
  (let* ((n (length arr))
         (idx (%js-array-to-integer index))
         (i (if (< idx 0) (+ n idx) idx)))
    (if (and (>= i 0) (< i n))
        (aref arr i)
        +js-undefined+)))

;;; -----------------------------------------------------------------------
;;;  Array.of (ES2015)
;;; -----------------------------------------------------------------------

(defun %js-array-of (&rest elements)
  "Array.of(...elements) — create array from positional arguments."
  (apply #'%js-make-array elements))

;;; -----------------------------------------------------------------------
;;;  ES2024 grouping methods
;;; -----------------------------------------------------------------------

(defun %js-array-group (arr key-fn)
  "Array.prototype.group(keyFn) → {key: [values...]} object (ES2024)."
  (let ((result (%js-make-ht)))
    (loop for i below (length arr)
          for el = (aref arr i)
          for key = (%js-to-string (%js-funcall key-fn el i arr))
          do (multiple-value-bind (bucket found) (gethash key result)
               (if found
                   (vector-push-extend el bucket)
                   (setf (gethash key result) (%js-make-array el)))))
    result))

(defun %js-array-group-to-map (arr key-fn)
  "Array.prototype.groupToMap(keyFn) → Map<key, [values...]> (ES2024)."
  (let ((result (%js-make-map)))
    (loop for i below (length arr)
          for el = (aref arr i)
          for key = (%js-funcall key-fn el i arr)
          do (let ((bucket (%js-map-get result key)))
               (if (eq bucket +js-undefined+)
                   (%js-map-set result key (%js-make-array el))
                   (vector-push-extend el bucket))))
    result))
