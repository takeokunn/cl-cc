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
  "Array.prototype.toSorted([compareFn]) — return sorted copy."
  (let ((result (copy-seq arr)))
    (if (and compare-fn (not (eq compare-fn +js-undefined+)))
        (sort result (lambda (a b) (%js-truthy (%js-funcall compare-fn a b))))
        (sort result (lambda (a b) (string< (%js-to-string a) (%js-to-string b)))))
    result))

(defun %js-array-to-spliced (arr start delete-count &rest items)
  "Array.prototype.toSpliced(start, deleteCount, ...items) — return modified copy."
  (let* ((n (length arr))
         (st (if (< start 0) (max 0 (+ n start)) (min start n)))
         (dc (max 0 (min (or delete-count n) (- n st))))
         (before (subseq arr 0 st))
         (after  (subseq arr (+ st dc)))
         (result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (loop for x across before do (vector-push-extend x result))
    (dolist (item items) (vector-push-extend item result))
    (loop for x across after  do (vector-push-extend x result))
    result))

(defun %js-array-with (arr index value)
  "Array.prototype.with(index, value) — return copy with element replaced."
  (let* ((n (length arr))
         (idx (if (< index 0) (+ n index) index))
         (result (copy-seq arr)))
    (when (and (>= idx 0) (< idx n))
      (setf (aref result idx) value))
    result))

(defun %js-array-find-last-index (arr pred)
  "Array.prototype.findLastIndex(pred) — index of last matching element, or -1."
  (loop for i from (1- (length arr)) downto 0
        when (%js-truthy (%js-funcall pred (aref arr i) i arr))
          return i
        finally (return -1)))

(defun %js-array-find-last (arr pred)
  "Array.prototype.findLast(pred) — last element satisfying PRED, or undefined."
  (let ((idx (%js-array-find-last-index arr pred)))
    (if (= idx -1) +js-undefined+ (aref arr idx))))

(defun %js-array-at (arr index)
  "Array.prototype.at(index) — supports negative indices."
  (let* ((n (length arr))
         (i (if (< index 0) (+ n index) index)))
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
                   (let ((new-bucket (make-array 1 :element-type t :adjustable t :fill-pointer 1
                                                   :initial-element el)))
                     (setf (gethash key result) new-bucket)))))
    result))

(defun %js-array-group-to-map (arr key-fn)
  "Array.prototype.groupToMap(keyFn) → Map<key, [values...]> (ES2024)."
  (let ((result (%js-make-map)))
    (loop for i below (length arr)
          for el = (aref arr i)
          for key = (%js-funcall key-fn el i arr)
          do (let ((bucket (%js-map-get result key)))
               (if (eq bucket +js-undefined+)
                   (%js-map-set result key (make-array 1 :element-type t :adjustable t
                                                         :fill-pointer 1 :initial-element el))
                   (vector-push-extend el bucket))))
    result))
