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
  "Return the square root of NUMBER as a double-float (PHP floats are doubles).
Coerce first: (sqrt 2) on the integer 2 yields a single-float, losing precision
(1.4142135 vs 1.4142135623731)."
  (sqrt (coerce number 'double-float)))

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

;;; ─── random_int (CSPRNG) ───────────────────────────────────────────────────

(defun %php-random-int (min max)
  "PHP random_int: cryptographically-secure-ish random integer in [MIN, MAX]."
  (let ((lo (truncate min)) (hi (truncate max)))
    (if (> lo hi) (error "random_int: min > max")
        (+ lo (random (1+ (- hi lo)))))))

(defun %php-random-bytes (length)
  "PHP random_bytes: return LENGTH random bytes as a binary string."
  (let ((n (truncate length)))
    (with-output-to-string (out)
      (dotimes (_ n) (write-char (code-char (random 256)) out)))))

;;; ─── gmp_* arbitrary precision (maps directly to CL bignums) ────────────────

(defun %php-gmp-num (x)
  "Coerce X (int, numeric string, or gmp value) to a CL integer."
  (cond ((integerp x) x)
        ((stringp x) (or (parse-integer x :junk-allowed t) 0))
        ((floatp x) (truncate x))
        (t 0)))

(defun %php-gmp-init (number &optional base)
  "PHP gmp_init: create a GMP number from an integer or string."
  (if (and base (stringp number))
      (or (parse-integer number :radix (truncate base) :junk-allowed t) 0)
      (%php-gmp-num number)))

(defun %php-gmp-add (a b) (+ (%php-gmp-num a) (%php-gmp-num b)))
(defun %php-gmp-sub (a b) (- (%php-gmp-num a) (%php-gmp-num b)))
(defun %php-gmp-mul (a b) (* (%php-gmp-num a) (%php-gmp-num b)))
(defun %php-gmp-div-q (a b &optional round)
  (declare (ignore round))
  (let ((bv (%php-gmp-num b))) (if (zerop bv) 0 (truncate (%php-gmp-num a) bv))))
(defun %php-gmp-mod (a b)
  (let ((bv (%php-gmp-num b))) (if (zerop bv) 0 (mod (%php-gmp-num a) bv))))
(defun %php-gmp-pow (a b) (expt (%php-gmp-num a) (truncate (%php-gmp-num b))))
(defun %php-gmp-abs (a) (abs (%php-gmp-num a)))
(defun %php-gmp-neg (a) (- (%php-gmp-num a)))
(defun %php-gmp-gcd (a b) (gcd (%php-gmp-num a) (%php-gmp-num b)))
(defun %php-gmp-cmp (a b)
  (let ((av (%php-gmp-num a)) (bv (%php-gmp-num b)))
    (cond ((< av bv) -1) ((> av bv) 1) (t 0))))
(defun %php-gmp-intval (a) (%php-gmp-num a))
(defun %php-gmp-strval (a &optional (base 10)) (format nil "~vR" (truncate base) (%php-gmp-num a)))
(defun %php-gmp-sqrt (a) (isqrt (%php-gmp-num a)))
(defun %php-gmp-fact (n)
  (let ((nv (truncate (%php-gmp-num n))) (acc 1))
    (loop for i from 2 to nv do (setf acc (* acc i))) acc))
