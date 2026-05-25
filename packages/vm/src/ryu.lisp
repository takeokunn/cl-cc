(in-package :cl-cc/vm)

;;; FR-1088 — pure-Lisp float-to-string backend with Ryu-compatible contract.

(defun %vm-float-nan-p (x)
  (and (floatp x) (not (= x x))))

(defun %vm-float-infinity-p (x)
  (and (floatp x) (not (zerop x)) (= x (+ x x))))

(defun %vm-normalize-float-exponent (string)
  (substitute #\e #\d (substitute #\e #\D string)))

(defun %vm-trim-float-zeros (string)
  (let ((e-pos (position #\e string)))
    (multiple-value-bind (mant exp)
        (if e-pos (values (subseq string 0 e-pos) (subseq string e-pos)) (values string ""))
      (let ((dot (position #\. mant)))
        (concatenate 'string
                     (if dot
                         (let ((end (length mant)))
                           (loop while (and (> end (1+ dot))
                                            (char= (char mant (1- end)) #\0))
                                 do (decf end))
                           (subseq mant 0 end))
                         mant)
                     exp)))))

(defun %vm-shortest-finite-float (x)
  (let ((*read-default-float-format* (if (typep x 'double-float)
                                          'double-float
                                          'single-float)))
    (%vm-trim-float-zeros
     (%vm-normalize-float-exponent
      (write-to-string x :escape nil :readably nil)))))

(defun vm-float-to-string (x &key (mode :shortest))
  "Return a VM decimal representation of X. MODE is :SHORTEST, :FIXED, or :EXPONENTIAL."
  (unless (floatp x)
    (return-from vm-float-to-string (princ-to-string x)))
  (cond
    ((%vm-float-nan-p x) "+nan.0")
    ((%vm-float-infinity-p x) (if (minusp x) "-inf.0" "+inf.0"))
    (t (ecase mode
         (:shortest (%vm-shortest-finite-float x))
         (:fixed (%vm-trim-float-zeros (format nil "~,12F" x)))
         (:exponential (%vm-normalize-float-exponent (format nil "~,12E" x)))))))

(eval-when (:load-toplevel :execute)
  (vm-register-host-bridge 'vm-float-to-string #'vm-float-to-string))
