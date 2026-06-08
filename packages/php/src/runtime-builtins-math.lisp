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

;;; ─── bcmath extension (arbitrary precision) ────────────────────────────────

(defun %php-bcadd (left right &optional (scale 0))
  "PHP bcadd: add two arbitrary precision numbers."
  (let ((l (if (stringp left) (read-from-string left) left))
        (r (if (stringp right) (read-from-string right) right))
        (s (or scale 0)))
    (format nil (format nil "~~,~DF" s) (+ l r))))

(defun %php-bcsub (left right &optional (scale 0))
  "PHP bcsub: subtract."
  (let ((l (if (stringp left) (read-from-string left) left))
        (r (if (stringp right) (read-from-string right) right))
        (s (or scale 0)))
    (format nil (format nil "~~,~DF" s) (- l r))))

(defun %php-bcmul (left right &optional (scale 0))
  "PHP bcmul: multiply."
  (let ((l (if (stringp left) (read-from-string left) left))
        (r (if (stringp right) (read-from-string right) right))
        (s (or scale 0)))
    (format nil (format nil "~~,~DF" s) (* l r))))

(defun %php-bcdiv (left right &optional (scale 0))
  "PHP bcdiv: divide."
  (let ((l (if (stringp left) (read-from-string left) left))
        (r (if (stringp right) (read-from-string right) right))
        (s (or scale 0)))
    (if (zerop r) (error "Division by zero")
        (format nil (format nil "~~,~DF" s) (/ l r)))))

(defun %php-bcpow (base exp &optional (scale 0))
  "PHP bcpow: power."
  (let ((b (if (stringp base) (read-from-string base) base))
        (e (if (stringp exp) (read-from-string exp) exp))
        (s (or scale 0)))
    (format nil (format nil "~~,~DF" s) (expt b e))))

(defun %php-bcmod (left right)
  "PHP bcmod: modulo."
  (let ((l (if (stringp left) (truncate (read-from-string left)) (truncate left)))
        (r (if (stringp right) (truncate (read-from-string right)) (truncate right))))
    (if (zerop r) (error "Division by zero") (format nil "~D" (mod l r)))))

(defun %php-bccomp (left right &optional (scale 0))
  "PHP bccomp: compare two numbers. Returns -1, 0, or 1."
  (declare (ignore scale))
  (let ((l (if (stringp left) (read-from-string left) left))
        (r (if (stringp right) (read-from-string right) right)))
    (cond ((< l r) -1) ((> l r) 1) (t 0))))

(defun %php-bcscale (&optional scale)
  "PHP bcscale: set/get global scale."
  (declare (ignore scale))
  t)

(defun %php-bcsqrt (operand &optional (scale 0))
  "PHP bcsqrt: square root."
  (let ((n (if (stringp operand) (read-from-string operand) operand))
        (s (or scale 0)))
    (format nil (format nil "~~,~DF" s) (sqrt (coerce n 'double-float)))))
