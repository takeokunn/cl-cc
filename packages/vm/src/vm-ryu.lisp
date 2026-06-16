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
  (cond ((not (= d d)) :nan)
        ((not (<= (abs d) most-positive-double-float))
         (if (minusp d) :neg-inf :inf))
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

(defun %ryu-zero-exponent-suffix-p (suffix)
  (let ((digits (string-left-trim '(#\+ #\-) suffix)))
    (and (plusp (length digits))
         (every (lambda (char) (char= char #\0)) digits))))

(defun %ryu-normalize-host-float-string (string)
  (let ((marker (position-if (lambda (char)
                               (find char "dDfFsSlL" :test #'char=))
                             string)))
    (if (not marker)
        string
        (let ((suffix (subseq string (1+ marker))))
          (if (%ryu-zero-exponent-suffix-p suffix)
              (subseq string 0 marker)
              (concatenate 'string
                           (subseq string 0 marker)
                           "e"
                           suffix))))))

(defun %ryu-trim-float-zeros (string)
  (let ((dot (position #\. string)))
    (if dot
        (let ((end (length string)))
          (loop while (and (> end (1+ dot))
                           (char= (char string (1- end)) #\0))
                do (decf end))
          (if (and (> end dot) (char= (char string (1- end)) #\.))
              (subseq string 0 (1- end))
              (subseq string 0 end)))
        string)))

(defun vm-ryu-float-to-string (val &optional (mode :shortest))
  (let ((special (%ryu-special-p val)))
    (when special
      (return-from vm-ryu-float-to-string
        (ecase special (:inf "+inf.0") (:neg-inf "-inf.0") (:nan "+nan.0")))))
  (ecase mode
    (:shortest
     (let ((*read-default-float-format* (type-of val)))
       (%ryu-normalize-host-float-string (prin1-to-string val))))
    (:exponential
     (%ryu-normalize-host-float-string
      (string-downcase (format nil "~E" val))))
    (:fixed
     (%ryu-trim-float-zeros (format nil "~,12F" val)))))

(defun vm-float-to-string (value &key (mode :shortest) (precision 6))
  "Primary float-to-string for the format system."
  (declare (ignore precision))
  (vm-ryu-float-to-string value mode))

(export '(vm-ryu-float-to-string vm-ryu-double-to-string
          vm-ryu-single-to-string vm-float-to-string))
