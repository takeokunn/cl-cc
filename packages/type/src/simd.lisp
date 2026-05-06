;;;; simd.lisp — FR-2206 data-parallel and SIMD types

(in-package :cl-cc/type)

(defstruct (simd-vector (:constructor %make-simd-vector))
  "A fixed-lane SIMD vector with homogeneous ELEMENT-TYPE."
  element-type
  (lanes 0 :type integer)
  (values nil :type list))

(defun make-simd-vector (element-type values)
  "Construct a SIMD vector after lane and element-type validation."
  (unless values
    (error "SIMD vector requires at least one lane"))
  (dolist (value values)
    (unless (%typed-channel-value-matches-p value element-type)
      (error "SIMD lane value ~S does not match element type ~S" value element-type)))
  (%make-simd-vector :element-type element-type
                     :lanes (length values)
                     :values (copy-list values)))

(defun %simd-compatible-p (left right)
  (and (simd-vector-p left)
       (simd-vector-p right)
       (= (simd-vector-lanes left) (simd-vector-lanes right))
       (equal (simd-vector-element-type left) (simd-vector-element-type right))))

(defun simd-map (function vector)
  "Map FUNCTION over VECTOR while preserving element type and lane count."
  (make-simd-vector (simd-vector-element-type vector)
                    (mapcar function (simd-vector-values vector))))

(defun simd-add (left right)
  "Lane-wise addition for compatible numeric SIMD vectors."
  (unless (%simd-compatible-p left right)
    (error "SIMD vectors must have the same lane count and element type"))
  (make-simd-vector (simd-vector-element-type left)
                    (mapcar #'+ (simd-vector-values left) (simd-vector-values right))))

(defun make-simd-type (element-type lanes)
  "Construct the FR-2206 static SIMD type."
  (make-type-advanced :feature-id "FR-2206"
                      :name 'simd-vector
                      :args (list element-type lanes)))
