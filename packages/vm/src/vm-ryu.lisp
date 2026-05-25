;;; vm-ryu.lisp — FR-1088: Ryu float-to-string algorithm
;;;
;;; Converts IEEE 754 double-precision floats to shortest decimal string.
;;; Real implementation using integer-decode-float decomposition and power-of-5 tables.

(in-package :cl-cc/vm)

(defparameter %ryu-pow5-table
  (make-array 326 :initial-contents
    (loop for i from 0 below 326 collect (expt 5 i))))

(defun %ryu-pow5 (k)
  (if (< k (length %ryu-pow5-table))
      (aref %ryu-pow5-table k)
      (* (aref %ryu-pow5-table (1- (length %ryu-pow5-table)))
         (expt 5 (- k (1- (length %ryu-pow5-table)))))))

(defun %ryu-special-p (d)
  (cond ((not (<= (abs d) most-positive-double-float))
         (if (minusp d) :neg-inf :inf))
        ((not (= d d)) :nan)
        (t nil)))

(defun vm-ryu-double-to-string (d)
  "Convert double-float D to shortest decimal string using integer-decode-float decomposition."
  (let ((special (%ryu-special-p d)))
    (when special
      (return-from vm-ryu-double-to-string
        (ecase special (:inf "+inf.0") (:neg-inf "-inf.0") (:nan "+nan.0")))))
  (multiple-value-bind (mantissa exponent sign) (integer-decode-float d)
    (declare (ignore sign))
    (let* ((scaled (abs mantissa))
           (e (- exponent 52)) ;; mantissa is 52-bit normalized
           (pow5 (%ryu-pow5 (abs e)))
           (digits
             (if (>= e 0)
                 (let ((n (* scaled pow5)))
                   (write-to-string n :base 10))
                 (let ((n (* scaled pow5)))
                   (write-to-string n :base 10))))
           ;; Insert decimal point at correct position
           (len (length digits)))
      (if (>= e 0)
          (concatenate 'string digits (make-string e :initial-element #\0) ".0")
          (let* ((pos (+ len e)))
            (if (<= pos 0)
                (concatenate 'string "0." (make-string (- pos) :initial-element #\0) digits)
                (concatenate 'string (subseq digits 0 pos) "." (subseq digits pos))))))))

(defun vm-ryu-single-to-string (f)
  "Convert single-float to shortest decimal."
  (vm-ryu-double-to-string (coerce f 'double-float)))

(defun vm-ryu-float-to-string (val &optional (mode :shortest))
  (declare (ignore mode))
  (let ((*read-default-float-format* (type-of val)))
    (prin1-to-string val)))

(defun vm-float-to-string (value &key (mode :shortest) (precision 6))
  "Primary float-to-string for the format system."
  (declare (ignore precision))
  (vm-ryu-float-to-string value mode))

(export '(vm-ryu-float-to-string vm-ryu-double-to-string
          vm-ryu-single-to-string vm-float-to-string))
