;;;; Math PHP builtin helpers.

(in-package :cl-cc/php)

(defun %php-abs (number)
  "Return the absolute value of NUMBER."
  ;; (%php-abs -3) => 3
  (abs number))

(defun %php-ceil (number)
  "Return NUMBER rounded up as an integer."
  (ceiling number))

(defun %php-floor (number)
  "Return NUMBER rounded down as an integer."
  (floor number))

(defun %php-round (number &optional (precision 0))
  "Return NUMBER rounded to PRECISION decimal places."
  (let ((scale (expt 10 precision)))
    (/ (round (* number scale)) scale)))

(defun %php-max (&rest values)
  "Return the largest numeric value in VALUES."
  ;; (%php-max 1 3 2) => 3
  (if values (apply #'max values) +php-null+))

(defun %php-min (&rest values)
  "Return the smallest numeric value in VALUES."
  (if values (apply #'min values) +php-null+))

(defun %php-rand (&optional min max)
  "Return a random integer, optionally between MIN and MAX inclusive."
  (if (and min max)
      (+ min (random (1+ (- max min))))
      (random most-positive-fixnum)))

(defun %php-mt-rand (&optional min max)
  "Alias for `%php-rand`."
  (%php-rand min max))

(defun %php-sqrt (number)
  "Return the square root of NUMBER."
  (sqrt number))

(defun %php-pow (base exponent)
  "Return BASE raised to EXPONENT."
  (expt base exponent))

(defun %php-pi ()
  "Return pi as a double-float."
  ;; (%php-pi) => 3.141592653589793d0
  pi)
