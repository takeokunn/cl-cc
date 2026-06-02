(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Native Bignum Implementation (FR-952/FR-955/FR-956)
;;;
;;; Provides: vm-bignum struct, limb arithmetic (add/sub/mul/div),
;;; conversion to/from host integers, and rational number support.
;;;
;;; Load order: after vm-numeric.lisp (digit-based planning layer).
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

(defmethod print-object ((value vm-bignum) stream)
  (format stream "~D" (vm-bignum-to-integer value)))

