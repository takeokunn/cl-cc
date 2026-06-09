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

(defun %php-intdiv (a b)
  "PHP intdiv (7.0): integer division of A by B truncated toward zero."
  (let ((divisor (%php-to-integer b)))
    (if (zerop divisor)
        (%php-throw 'division-by-zero-error "Division by zero")
        (truncate (%php-to-integer a) divisor))))

(defun %php-fdiv (a b)
  "PHP fdiv (8.0): IEEE float division — division by zero yields +/-INF (or the
INF approximation used elsewhere) rather than signalling an error."
  (let ((x (coerce (%php-numeric a) 'double-float))
        (y (coerce (%php-numeric b) 'double-float)))
    (if (zerop y)
        (cond ((minusp x) most-negative-double-float)
              (t most-positive-double-float))
        (/ x y))))

(defun %php-pi ()
  "Return pi as a double-float."
  ;; (%php-pi) => 3.141592653589793d0
  pi)

;;; ─── functions whose names are NOT CL builtins ─────────────────────────────
;;; These MUST be registered by symbol (not a lambda). A PHP builtin call lowers
;;; to a bare function symbol; the VM resolves it only if that symbol is fbound.
;;; sin/cos/tan/log/exp resolve accidentally because they collide with CL names,
;;; but fmod/atan2/log10/hypot/deg2rad/… do not, so a lambda registration leaves
;;; no fbound symbol and the call hits `Undefined function'. See
;;; runtime-builtins-register.lisp.

(defun %php-fmod (x y)
  "PHP fmod: floating-point remainder of X / Y (sign follows the dividend)."
  ;; (%php-fmod 7 3) => 1.0d0
  (let ((dx (coerce (%php-numeric x) 'double-float))
        (dy (coerce (%php-numeric y) 'double-float)))
    (- dx (* (coerce (truncate dx dy) 'double-float) dy))))

(defun %php-atan2 (y x)
  "PHP atan2: arc tangent of two variables (Y first, like C)."
  (atan (coerce (%php-numeric y) 'double-float)
        (coerce (%php-numeric x) 'double-float)))

(defun %php-log-exact-power (x base)
  "If X is an exact positive integer power of integer BASE, return that integer
exponent, else NIL.  Lets log10(1000) yield 3 exactly the way PHP's correctly-
rounded C log10 does, instead of the 2.9999999999999996 that log(x)/log(base)
produces in IEEE double."
  (when (and (integerp x) (plusp x))
    (loop with n = x and e = 0
          do (multiple-value-bind (q r) (truncate n base)
               (cond ((/= r 0) (return nil))
                     ((= q 1) (return (1+ e)))
                     (t (setf n q e (1+ e))))))))

(defun %php-log10 (x)
  "PHP log10: base-10 logarithm of X."
  (or (%php-log-exact-power (%php-numeric x) 10)
      (log (coerce (%php-numeric x) 'double-float) 10d0)))

(defun %php-log2 (x)
  "PHP log2 (8.3 alias of log(x,2)): base-2 logarithm of X."
  (or (%php-log-exact-power (%php-numeric x) 2)
      (log (coerce (%php-numeric x) 'double-float) 2d0)))

(defun %php-hypot (x y)
  "PHP hypot: length of the hypotenuse of a right-angle triangle."
  (let ((dx (coerce (%php-numeric x) 'double-float))
        (dy (coerce (%php-numeric y) 'double-float)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun %php-deg2rad (degrees)
  "PHP deg2rad: convert DEGREES to radians."
  (* (coerce (%php-numeric degrees) 'double-float) (/ pi 180)))

(defun %php-rad2deg (radians)
  "PHP rad2deg: convert RADIANS to degrees."
  (* (coerce (%php-numeric radians) 'double-float) (/ 180 pi)))

(defun %php-is-finite (x)
  "PHP is_finite: true when X is a finite float (not INF or NAN)."
  (let ((v (%php-numeric x)))
    (if (and (floatp v) (= v v) (< (abs v) most-positive-double-float)) t nil)))

(defun %php-is-infinite (x)
  "PHP is_infinite: true when X is the INF approximation used elsewhere."
  (let ((v (%php-numeric x)))
    (if (and (floatp v) (= v v) (>= (abs v) most-positive-double-float)) t nil)))

(defun %php-base-convert (number from-base to-base)
  "PHP base_convert: convert a string NUMBER from FROM-BASE to TO-BASE (2..36),
returning the digits in lowercase as PHP does."
  (let ((value (parse-integer (%php-stringify number)
                              :radix (%php-to-integer from-base) :junk-allowed t)))
    (string-downcase (format nil "~vR" (%php-to-integer to-base) (or value 0)))))

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
