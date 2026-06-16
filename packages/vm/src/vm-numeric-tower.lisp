(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Native Number Tower (FR-952 / FR-955 / FR-956)
;;;
;;; Contains: native limb-based vm-bignum struct, integer<->bignum conversion,
;;; bignum arithmetic ops (add/sub/mul/div/gcd/expt/isqrt/to-string),
;;; vm-ratio struct + rational ops, vm-complex struct + complex ops,
;;; print-object methods, VM instructions (vm-round-inst, vm-fma,
;;; float/decode-float instructions).
;;;
;;; Load order: after vm-numeric-complex.lisp, before vm-environment.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── FR-952 / FR-955 / FR-956 Native VM Number Tower ─────────────────────

(defconstant +vm-bignum-limb-bits+ 64
  "Number of bits in each VM bignum limb.")

(defconstant +vm-bignum-limb-base+ (ash 1 +vm-bignum-limb-bits+)
  "Base represented by one VM bignum limb.")

(defconstant +vm-bignum-limb-mask+ (1- +vm-bignum-limb-base+)
  "Mask for the low 64 bits of a VM bignum limb.")

(defstruct (vm-bignum
            (:constructor %make-vm-bignum (limb-vector negative-p))
            (:copier nil))
  "Native VM signed-magnitude bignum.

LIMB-VECTOR is little-endian and contains unsigned 64-bit limbs.  NEGATIVE-P
is NIL for zero and positive values, T for negative values."
  (limb-vector (make-array 1 :element-type '(unsigned-byte 64) :initial-element 0)
               :type (simple-array (unsigned-byte 64) (*)))
  (negative-p nil :type boolean))

(defun %vm-empty-limb-vector (length &key (initial-element 0))
  (make-array length :element-type '(unsigned-byte 64) :initial-element initial-element))

(defun %vm-normalize-limbs (limbs)
  "Return LIMBS without redundant high zero limbs."
  (let ((end (1- (length limbs))))
    (loop while (and (> end 0) (zerop (aref limbs end)))
          do (decf end))
    (let ((result (%vm-empty-limb-vector (1+ end))))
      (dotimes (i (length result) result)
        (setf (aref result i) (aref limbs i))))))

(defun %vm-zero-limbs-p (limbs)
  (and (= (length limbs) 1) (zerop (aref limbs 0))))

(defun %vm-make-bignum-from-limbs (limbs negative-p)
  (let ((normalized (%vm-normalize-limbs limbs)))
    (%make-vm-bignum normalized (and negative-p (not (%vm-zero-limbs-p normalized))))))

(defun vm-integer->bignum (integer)
  "Convert INTEGER to a native VM bignum."
  (check-type integer integer)
  (let ((n (abs integer))
        (limbs nil))
    (if (zerop n)
        (%make-vm-bignum (%vm-empty-limb-vector 1) nil)
        (progn
          (loop while (plusp n)
                do (push (logand n +vm-bignum-limb-mask+) limbs)
                   (setf n (ash n (- +vm-bignum-limb-bits+))))
          (let ((vector (%vm-empty-limb-vector (length limbs))))
            (loop for limb in (nreverse limbs)
                  for i from 0
                  do (setf (aref vector i) limb))
            (%vm-make-bignum-from-limbs vector (minusp integer)))))))

(defun vm-bignum-to-integer (bignum)
  "Convert native VM BIGNUM to a host integer at the VM boundary."
  (let ((value 0)
        (limbs (vm-bignum-limb-vector bignum)))
    (loop for i from (1- (length limbs)) downto 0
          do (setf value (logior (ash value +vm-bignum-limb-bits+)
                                 (aref limbs i))))
    (if (vm-bignum-negative-p bignum) (- value) value)))

(defun %vm-coerce-bignum (value)
  (cond
    ((vm-bignum-p value) value)
    ((integerp value) (vm-integer->bignum value))
    (t (error "Not an integer or VM bignum: ~S" value))))

(defun %vm-bignum-zero-p (value)
  (%vm-zero-limbs-p (vm-bignum-limb-vector (%vm-coerce-bignum value))))

(defun %vm-compare-limbs (lhs rhs)
  "Compare unsigned little-endian limb vectors.  Return -1, 0, or 1."
  (let ((lhs (%vm-normalize-limbs lhs))
        (rhs (%vm-normalize-limbs rhs)))
    (cond
      ((< (length lhs) (length rhs)) -1)
      ((> (length lhs) (length rhs)) 1)
      (t (loop for i from (1- (length lhs)) downto 0
               for a = (aref lhs i)
               for b = (aref rhs i)
               when (< a b) do (return -1)
               when (> a b) do (return 1)
               finally (return 0))))))

(defun %vm-add-limbs-64 (lhs rhs)
  "Schoolbook O(n) unsigned limb addition with carry."
  (let* ((len (max (length lhs) (length rhs)))
         (result (%vm-empty-limb-vector (1+ len)))
         (carry 0))
    (dotimes (i len)
      (let* ((a (if (< i (length lhs)) (aref lhs i) 0))
             (b (if (< i (length rhs)) (aref rhs i) 0))
             (sum (+ a b carry)))
        (setf (aref result i) (logand sum +vm-bignum-limb-mask+)
              carry (ash sum (- +vm-bignum-limb-bits+)))))
    (setf (aref result len) carry)
    (%vm-normalize-limbs result)))

(defun %vm-sub-limbs-64 (lhs rhs)
  "Schoolbook O(n) unsigned limb subtraction.  Requires LHS >= RHS."
  (let* ((len (length lhs))
         (result (%vm-empty-limb-vector len))
         (borrow 0))
    (dotimes (i len)
      (let* ((a (aref lhs i))
             (b (if (< i (length rhs)) (aref rhs i) 0))
             (diff (- a b borrow)))
        (if (minusp diff)
            (setf (aref result i) (+ diff +vm-bignum-limb-base+)
                  borrow 1)
            (setf (aref result i) diff
                  borrow 0))))
    (%vm-normalize-limbs result)))

(defun vm-bignum-add (lhs rhs)
  "Add VM bignums using signed-magnitude schoolbook arithmetic."
  (let* ((a (%vm-coerce-bignum lhs))
         (b (%vm-coerce-bignum rhs))
         (a-limbs (vm-bignum-limb-vector a))
         (b-limbs (vm-bignum-limb-vector b)))
    (if (eq (vm-bignum-negative-p a) (vm-bignum-negative-p b))
        (%vm-make-bignum-from-limbs (%vm-add-limbs-64 a-limbs b-limbs)
                                    (vm-bignum-negative-p a))
        (case (%vm-compare-limbs a-limbs b-limbs)
          (0 (vm-integer->bignum 0))
          (1 (%vm-make-bignum-from-limbs (%vm-sub-limbs-64 a-limbs b-limbs)
                                         (vm-bignum-negative-p a)))
          (-1 (%vm-make-bignum-from-limbs (%vm-sub-limbs-64 b-limbs a-limbs)
                                          (vm-bignum-negative-p b)))))))

(defun vm-bignum-negate (value)
  (let ((b (%vm-coerce-bignum value)))
    (%vm-make-bignum-from-limbs (copy-seq (vm-bignum-limb-vector b))
                                (not (vm-bignum-negative-p b)))))

(defun vm-bignum-sub (lhs rhs)
  "Subtract RHS from LHS using VM bignum arithmetic."
  (vm-bignum-add lhs (vm-bignum-negate rhs)))

(defun %vm-schoolbook-mul-limbs-64 (lhs rhs)
  "Schoolbook limb multiplication."
  (let ((result (%vm-empty-limb-vector (+ (length lhs) (length rhs)))))
    (dotimes (i (length lhs))
      (let ((carry 0))
        (dotimes (j (length rhs))
          (let* ((index (+ i j))
                 (total (+ (aref result index)
                           (* (aref lhs i) (aref rhs j))
                           carry)))
            (setf (aref result index) (logand total +vm-bignum-limb-mask+)
                  carry (ash total (- +vm-bignum-limb-bits+)))))
        (loop for index from (+ i (length rhs))
              while (plusp carry)
              do (let ((total (+ (aref result index) carry)))
                   (setf (aref result index) (logand total +vm-bignum-limb-mask+)
                         carry (ash total (- +vm-bignum-limb-bits+)))))))
    (%vm-normalize-limbs result)))

(defun %vm-shift-left-limbs (limbs places)
  (if (or (<= places 0) (%vm-zero-limbs-p limbs))
      (copy-seq limbs)
      (let ((result (%vm-empty-limb-vector (+ places (length limbs)))))
        (dotimes (i (length limbs) result)
          (setf (aref result (+ i places)) (aref limbs i))))))

(defun %vm-karatsuba-mul-limbs-64 (lhs rhs)
  "Karatsuba multiplication for large limb vectors."
  (labels ((mul (a b)
             (let ((na (length a))
                   (nb (length b)))
               (cond
                 ((or (%vm-zero-limbs-p a) (%vm-zero-limbs-p b)) (%vm-empty-limb-vector 1))
                 ((< (max na nb) +vm-bignum-karatsuba-threshold+)
                  (%vm-schoolbook-mul-limbs-64 a b))
                 (t
                  (let* ((n (max na nb))
                         (m (floor n 2))
                         (a0 (subseq a 0 (min m na)))
                         (a1 (if (> na m) (subseq a m na) (%vm-empty-limb-vector 1)))
                         (b0 (subseq b 0 (min m nb)))
                         (b1 (if (> nb m) (subseq b m nb) (%vm-empty-limb-vector 1)))
                         (z0 (mul a0 b0))
                         (z2 (mul a1 b1))
                         (z1 (%vm-sub-limbs-64
                              (%vm-sub-limbs-64 (mul (%vm-add-limbs-64 a0 a1)
                                                     (%vm-add-limbs-64 b0 b1))
                                                z0)
                              z2)))
                    (%vm-add-limbs-64
                     (%vm-add-limbs-64 z0 (%vm-shift-left-limbs z1 m))
                     (%vm-shift-left-limbs z2 (* 2 m)))))))))
    (mul lhs rhs)))

(defun vm-bignum-mul (lhs rhs)
  "Multiply VM bignums, selecting Karatsuba above 64 limbs."
  (let* ((a (%vm-coerce-bignum lhs))
         (b (%vm-coerce-bignum rhs))
         (a-limbs (vm-bignum-limb-vector a))
         (b-limbs (vm-bignum-limb-vector b))
         (limbs (if (>= (max (length a-limbs) (length b-limbs))
                        +vm-bignum-karatsuba-threshold+)
                    (%vm-karatsuba-mul-limbs-64 a-limbs b-limbs)
                    (%vm-schoolbook-mul-limbs-64 a-limbs b-limbs))))
    (%vm-make-bignum-from-limbs limbs
                                (not (eq (vm-bignum-negative-p a)
                                         (vm-bignum-negative-p b))))))

(defun %vm-bignum-unsigned-compare (lhs rhs)
  (%vm-compare-limbs (vm-bignum-limb-vector (%vm-coerce-bignum lhs))
                     (vm-bignum-limb-vector (%vm-coerce-bignum rhs))))

(defun %vm-bignum-abs (value)
  (let ((b (%vm-coerce-bignum value)))
    (%vm-make-bignum-from-limbs (copy-seq (vm-bignum-limb-vector b)) nil)))

(defun %vm-bignum-bit-length (value)
  (let* ((b (%vm-bignum-abs value))
         (limbs (vm-bignum-limb-vector b)))
    (if (%vm-zero-limbs-p limbs)
        0
        (+ (* (1- (length limbs)) +vm-bignum-limb-bits+)
           (integer-length (aref limbs (1- (length limbs))))))))

(defun %vm-bignum-ash-left (value count)
  (let* ((b (%vm-coerce-bignum value))
         (limbs (vm-bignum-limb-vector b))
         (whole (floor count +vm-bignum-limb-bits+))
         (bits (mod count +vm-bignum-limb-bits+))
         (result (%vm-empty-limb-vector (+ (length limbs) whole 1)))
         (carry 0))
    (dotimes (i (length limbs))
      (let ((total (logior (ash (aref limbs i) bits) carry)))
        (setf (aref result (+ i whole)) (logand total +vm-bignum-limb-mask+)
              carry (ash total (- +vm-bignum-limb-bits+)))))
    (setf (aref result (+ whole (length limbs))) carry)
    (%vm-make-bignum-from-limbs result (vm-bignum-negative-p b))))

(defun %vm-bignum-set-bit (value bit-index)
  (vm-bignum-add value (%vm-bignum-ash-left 1 bit-index)))

(defun %vm-bignum-div-unsigned (dividend divisor)
  "Unsigned binary long division used by the VM bignum divider."
  (when (%vm-bignum-zero-p divisor)
    (error "Division by zero"))
  (let ((a (%vm-bignum-abs dividend))
        (b (%vm-bignum-abs divisor)))
    (if (= -1 (%vm-bignum-unsigned-compare a b))
        (values (vm-integer->bignum 0) a)
        (let ((q (vm-integer->bignum 0))
              (r a))
          (loop while (not (= -1 (%vm-bignum-unsigned-compare r b)))
                do (let* ((shift (max 0 (- (%vm-bignum-bit-length r)
                                           (%vm-bignum-bit-length b))))
                          (chunk (%vm-bignum-ash-left b shift)))
                     (when (= 1 (%vm-bignum-unsigned-compare chunk r))
                       (decf shift)
                       (setf chunk (%vm-bignum-ash-left b shift)))
                     (setf r (vm-bignum-sub r chunk)
                           q (%vm-bignum-set-bit q shift))))
          (values q r)))))

(defun vm-bignum-div (lhs rhs)
  "Divide LHS by RHS with Knuth-D-compatible quotient/remainder semantics.

The implementation uses normalized unsigned long division over the VM limb
representation and applies signs after quotient/remainder construction."
  (let* ((a (%vm-coerce-bignum lhs))
         (b (%vm-coerce-bignum rhs)))
    (when (%vm-bignum-zero-p b)
      (error "Division by zero"))
    (multiple-value-bind (q r) (%vm-bignum-div-unsigned a b)
      (values (%vm-make-bignum-from-limbs
               (vm-bignum-limb-vector q)
               (not (eq (vm-bignum-negative-p a) (vm-bignum-negative-p b))))
              (%vm-make-bignum-from-limbs
               (vm-bignum-limb-vector r)
               (vm-bignum-negative-p a))))))

(defun vm-bignum-expt (base exponent)
  "Raise integer BASE to non-negative integer EXPONENT by square-and-multiply."
  (check-type exponent (integer 0 *))
  (let ((result (vm-integer->bignum 1))
        (power (%vm-coerce-bignum base))
        (e exponent))
    (loop while (plusp e)
          do (when (oddp e)
               (setf result (vm-bignum-mul result power)))
             (setf e (ash e -1))
             (when (plusp e)
               (setf power (vm-bignum-mul power power))))
    result))

(defun %vm-bignum-even-p (value)
  (evenp (aref (vm-bignum-limb-vector (%vm-coerce-bignum value)) 0)))

(defun %vm-bignum-shift-right-one (value)
  (let* ((b (%vm-coerce-bignum value))
         (limbs (vm-bignum-limb-vector b))
         (result (%vm-empty-limb-vector (length limbs)))
         (carry 0))
    (loop for i from (1- (length limbs)) downto 0
          for limb = (aref limbs i)
          do (setf (aref result i) (logior (ash limb -1)
                                           (ash carry (1- +vm-bignum-limb-bits+)))
                   carry (logand limb 1)))
    (%vm-make-bignum-from-limbs result (vm-bignum-negative-p b))))

(defun vm-bignum-gcd (lhs rhs)
  "Binary GCD (Stein) over native VM bignums."
  (let ((u (%vm-bignum-abs lhs))
        (v (%vm-bignum-abs rhs))
        (shift 0))
    (cond
      ((%vm-bignum-zero-p u) v)
      ((%vm-bignum-zero-p v) u)
      (t
       (loop while (and (%vm-bignum-even-p u) (%vm-bignum-even-p v))
             do (incf shift)
                (setf u (%vm-bignum-shift-right-one u)
                      v (%vm-bignum-shift-right-one v)))
       (loop while (%vm-bignum-even-p u)
             do (setf u (%vm-bignum-shift-right-one u)))
       (loop until (%vm-bignum-zero-p v)
             do (loop while (%vm-bignum-even-p v)
                      do (setf v (%vm-bignum-shift-right-one v)))
                (when (= 1 (%vm-bignum-unsigned-compare u v))
                  (rotatef u v))
                (setf v (vm-bignum-sub v u)))
       (%vm-bignum-ash-left u shift)))))

(defun vm-isqrt (value)
  "Integer square root for VM integers/bignums by binary search."
  (let ((n (%vm-coerce-bignum value)))
    (when (vm-bignum-negative-p n)
      (error "ISQRT of negative integer: ~S" value))
    (let ((low (vm-integer->bignum 0))
          (high (%vm-bignum-ash-left 1 (ceiling (%vm-bignum-bit-length n) 2)))
          (one (vm-integer->bignum 1))
          (answer (vm-integer->bignum 0)))
      (loop while (not (= 1 (%vm-bignum-unsigned-compare low high)))
            do (multiple-value-bind (sum) (vm-bignum-add low high)
                 (multiple-value-bind (mid rem) (vm-bignum-div sum 2)
                   (declare (ignore rem))
                   (if (= 1 (%vm-bignum-unsigned-compare (vm-bignum-mul mid mid) n))
                       (setf high (vm-bignum-sub mid one))
                       (setf answer mid
                             low (vm-bignum-add mid one))))))
      answer)))

(defun vm-bignum-to-string (value &optional (radix 10))
  "Render VM bignum VALUE in RADIX (2..36)."
  (check-type radix (integer 2 36))
  (let ((n (%vm-bignum-abs value))
        (base (vm-integer->bignum radix))
        (digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (chars nil))
    (if (%vm-bignum-zero-p n)
        "0"
        (progn
          (loop until (%vm-bignum-zero-p n)
                do (multiple-value-bind (q r) (vm-bignum-div n base)
                     (push (char digits (vm-bignum-to-integer r)) chars)
                     (setf n q)))
          (coerce (if (vm-bignum-negative-p (%vm-coerce-bignum value))
                      (cons #\- chars)
                      chars)
                  'string)))))

(defun vm-bignum-add-integers (lhs rhs)
  "Native VM bignum addition fallback, externalized as a host integer."
  (vm-bignum-to-integer (vm-bignum-add lhs rhs)))

(defun vm-bignum-subtract-integers (lhs rhs)
  "Native VM bignum subtraction fallback, externalized as a host integer."
  (vm-bignum-to-integer (vm-bignum-sub lhs rhs)))

(defstruct (vm-ratio
            (:constructor %make-vm-ratio (numerator denominator))
            (:copier nil))
  "Native VM rational represented by normalized numerator/denominator integers."
  numerator
  denominator)

(defun %vm-number->integer (value)
  (if (vm-bignum-p value) (vm-bignum-to-integer value) value))

(defun %vm-externalize-ratio (ratio)
  (/ (vm-ratio-numerator ratio) (vm-ratio-denominator ratio)))

(defun vm-make-ratio (numerator denominator)
  "Construct a normalized VM ratio."
  (let ((n (%vm-number->integer numerator))
        (d (%vm-number->integer denominator)))
    (when (zerop d)
      (error "Ratio denominator is zero"))
    (when (minusp d)
      (setf n (- n) d (- d)))
    (let ((g (vm-bignum-to-integer (vm-bignum-gcd n d))))
      (%make-vm-ratio (/ n g) (/ d g)))))

(defun %vm-coerce-ratio (value)
  (cond
    ((vm-ratio-p value) value)
    ((integerp value) (vm-make-ratio value 1))
    ((rationalp value) (vm-make-ratio (numerator value) (denominator value)))
    (t (error "Not a rational value: ~S" value))))

(defun vm-rational-add (lhs rhs)
  (let ((a (%vm-coerce-ratio lhs))
        (b (%vm-coerce-ratio rhs)))
    (vm-make-ratio (+ (* (vm-ratio-numerator a) (vm-ratio-denominator b))
                      (* (vm-ratio-numerator b) (vm-ratio-denominator a)))
                   (* (vm-ratio-denominator a) (vm-ratio-denominator b)))))

(defun vm-rational-sub (lhs rhs)
  (let ((a (%vm-coerce-ratio lhs))
        (b (%vm-coerce-ratio rhs)))
    (vm-make-ratio (- (* (vm-ratio-numerator a) (vm-ratio-denominator b))
                      (* (vm-ratio-numerator b) (vm-ratio-denominator a)))
                   (* (vm-ratio-denominator a) (vm-ratio-denominator b)))))

(defun vm-rational-mul (lhs rhs)
  (let ((a (%vm-coerce-ratio lhs))
        (b (%vm-coerce-ratio rhs)))
    (vm-make-ratio (* (vm-ratio-numerator a) (vm-ratio-numerator b))
                   (* (vm-ratio-denominator a) (vm-ratio-denominator b)))))

(defun vm-rational-div (lhs rhs)
  (let ((a (%vm-coerce-ratio lhs))
        (b (%vm-coerce-ratio rhs)))
    (when (zerop (vm-ratio-numerator b))
      (error "Division by zero"))
    (vm-make-ratio (* (vm-ratio-numerator a) (vm-ratio-denominator b))
                   (* (vm-ratio-denominator a) (vm-ratio-numerator b)))))

(defun vm-rational (number)
  "Convert NUMBER to an exact native VM rational."
  (cond
    ((vm-ratio-p number) number)
    ((integerp number) (vm-make-ratio number 1))
    ((rationalp number) (vm-make-ratio (numerator number) (denominator number)))
    ((floatp number)
     (multiple-value-bind (mantissa exponent sign) (integer-decode-float number)
       (if (minusp exponent)
           (vm-make-ratio (* sign mantissa) (ash 1 (- exponent)))
           (vm-make-ratio (* sign mantissa (ash 1 exponent)) 1))))
    (t (error "Cannot convert to rational: ~S" number))))

(defun vm-rationalize (number &optional (tolerance 1.0d-12))
  "Return a human-friendly rational approximation as a VM ratio."
  (cond
    ((or (integerp number) (rationalp number) (vm-ratio-p number)) (vm-rational number))
    ((floatp number)
     (let* ((sign (if (minusp number) -1 1))
            (x (abs (float number 1.0d0)))
            (h1 1) (h0 0)
            (k1 0) (k0 1)
            (b x))
       (loop
         for a = (floor b)
         for h = (+ (* a h1) h0)
         for k = (+ (* a k1) k0)
         do (when (or (zerop (- b a))
                      (< (abs (- x (/ h k))) tolerance))
              (return (vm-make-ratio (* sign h) k)))
            (setf h0 h1 h1 h
                  k0 k1 k1 k
                  b (/ (- b a))))))
    (t (error "Cannot rationalize: ~S" number))))

(defun vm-floor (number &optional (divisor 1))
  "Floor returning quotient and rational remainder for VM ratios."
  (let ((ratio (vm-rational-div number divisor)))
    (multiple-value-bind (q r) (vm-bignum-burnikel-ziegler-divide
                                (vm-ratio-numerator ratio)
                                (vm-ratio-denominator ratio)
                                :rounding :floor)
      (values q (vm-make-ratio r (vm-ratio-denominator ratio))))))

(defstruct (vm-complex
            (:constructor %make-vm-complex (real imag))
            (:copier nil))
  "Native VM complex value."
  real
  imag)

(defun vm-complex-make (real imag)
  "Construct a native VM complex value."
  (if (and (zerop imag) (realp real))
      real
      (%make-vm-complex real imag)))

(defun %vm-coerce-complex (value)
  (cond
    ((vm-complex-p value) value)
    ((complexp value) (%make-vm-complex (realpart value) (imagpart value)))
    ((numberp value) (%make-vm-complex value 0))
    (t (error "Not a complex numeric value: ~S" value))))

(defun vm-realpart (number)
  (if (vm-complex-p number) (vm-complex-real number) (realpart number)))

(defun vm-imagpart (number)
  (if (vm-complex-p number) (vm-complex-imag number) (imagpart number)))

(defun vm-complex-add (lhs rhs)
  (let ((a (%vm-coerce-complex lhs))
        (b (%vm-coerce-complex rhs)))
    (vm-complex-make (+ (vm-complex-real a) (vm-complex-real b))
                     (+ (vm-complex-imag a) (vm-complex-imag b)))))

(defun vm-complex-sub (lhs rhs)
  (let ((a (%vm-coerce-complex lhs))
        (b (%vm-coerce-complex rhs)))
    (vm-complex-make (- (vm-complex-real a) (vm-complex-real b))
                     (- (vm-complex-imag a) (vm-complex-imag b)))))

(defun vm-complex-mul (lhs rhs)
  (let* ((a (%vm-coerce-complex lhs))
         (b (%vm-coerce-complex rhs))
         (ar (vm-complex-real a)) (ai (vm-complex-imag a))
         (br (vm-complex-real b)) (bi (vm-complex-imag b)))
    (vm-complex-make (- (* ar br) (* ai bi))
                     (+ (* ar bi) (* ai br)))))

(defun vm-complex-div (lhs rhs)
  (let* ((a (%vm-coerce-complex lhs))
         (b (%vm-coerce-complex rhs))
         (ar (vm-complex-real a)) (ai (vm-complex-imag a))
         (br (vm-complex-real b)) (bi (vm-complex-imag b))
         (den (+ (* br br) (* bi bi))))
    (when (zerop den)
      (error "Complex division by zero"))
    (vm-complex-make (/ (+ (* ar br) (* ai bi)) den)
                     (/ (- (* ai br) (* ar bi)) den))))

(defun vm-complex-conjugate (number)
  (vm-complex-make (vm-realpart number) (- (vm-imagpart number))))

(defun vm-complex-abs (number)
  (let ((r (vm-realpart number))
        (i (vm-imagpart number)))
    (sqrt (+ (* r r) (* i i)))))

(defun vm-complex-phase (number)
  (atan (coerce (vm-imagpart number) 'double-float)
        (coerce (vm-realpart number) 'double-float)))

(defun vm-complex-exp (number)
  "Complex exponential using Euler's formula."
  (let* ((r (vm-realpart number))
         (i (vm-imagpart number))
         (scale (exp r)))
    (vm-complex-make (* scale (cos i)) (* scale (sin i)))))

(defun vm-complex-log (number)
  (vm-complex-make (log (vm-complex-abs number)) (vm-complex-phase number)))

(defun vm-complex-sqrt (number)
  (let* ((x (vm-realpart number))
         (y (vm-imagpart number)))
    (if (zerop y)
        (if (minusp x)
            (vm-complex-make 0 (sqrt (- x)))
            (sqrt x))
        (let* ((r (vm-complex-abs number))
               (real (sqrt (/ (+ r x) 2)))
               (imag (if (minusp y)
                         (- (sqrt (/ (- r x) 2)))
                         (sqrt (/ (- r x) 2)))))
          (vm-complex-make real imag)))))

(defmethod print-object ((value vm-complex) stream)
  (format stream "#C(~S ~S)" (vm-complex-real value) (vm-complex-imag value)))

(defmethod print-object ((value vm-ratio) stream)
  (format stream "~D/~D" (vm-ratio-numerator value) (vm-ratio-denominator value)))

(defmethod print-object ((value vm-bignum) stream)
  (format stream "~D" (vm-bignum-to-integer value)))

(defun %vm-externalize-complex (value)
  (if (vm-complex-p value)
      (complex (vm-complex-real value) (vm-complex-imag value))
      value))

;;; FR-301: round (binary, returns quotient + remainder as multiple values)

(define-vm-instruction vm-round-inst (vm-instruction)
  "Round number to nearest integer (banker's rounding)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :round)
  (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-round-inst) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r)
      (round (vm-reg-get state (vm-lhs inst))
             (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r)))
  (values (1+ pc) nil nil))


;;; FR-305: Float Operations
;; define-vm-unary-instruction / define-vm-binary-instruction defined in vm.lisp.

;;; FR-099: FMA (Fused Multiply-Add)
(define-vm-instruction vm-fma (vm-instruction)
  "Fused multiply-add: dst = a * b + c.  Single rounding, no intermediate rounding.
   On x86-64: VFMADD231SD (FMA3) or VFMADD213SD (FMA4) in XMM registers."
  (dst nil :reader vm-dst)
  (a nil :reader vm-a)
  (b nil :reader vm-b)
  (c nil :reader vm-c)
  (:sexp-tag :fma)
  (:sexp-slots dst a b c))

(defmethod execute-instruction ((inst vm-fma) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (+ (* (vm-reg-get state (vm-a inst))
                    (vm-reg-get state (vm-b inst)))
                 (vm-reg-get state (vm-c inst))))
  (values (1+ pc) nil nil))

(define-vm-unary-instruction vm-float-inst       :float           "Convert number to float.")
(define-vm-unary-instruction vm-float-precision  :float-precision "Number of significant radix digits in a float.")
(define-vm-unary-instruction vm-float-radix      :float-radix     "Radix of the float representation.")
(define-vm-unary-instruction vm-float-sign       :float-sign      "Sign of a float as float.")
(define-vm-unary-instruction vm-float-digits     :float-digits    "Number of radix digits in the float mantissa.")
(define-vm-binary-instruction vm-scale-float     :scale-float     "Scale a float by a power of the radix.")

(define-simple-instruction vm-float-inst      :unary float)
(define-simple-instruction vm-float-precision :unary float-precision)
(define-simple-instruction vm-float-radix     :unary float-radix)
(define-simple-instruction vm-float-sign      :unary float-sign)
(define-simple-instruction vm-float-digits    :unary float-digits)
(define-simple-instruction vm-scale-float     :binary scale-float)

(define-vm-unary-instruction vm-decode-float :decode-float "Decode float into significand, exponent, sign (3 multiple values).")

(defmethod execute-instruction ((inst vm-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

(define-vm-unary-instruction vm-integer-decode-float :integer-decode-float "Decode float into integer significand, exponent, sign (3 multiple values).")

(defmethod execute-instruction ((inst vm-integer-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (integer-decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

;;; Environment predicates (vm-boundp, vm-fboundp, etc.) are in vm-environment.lisp (loads after).
