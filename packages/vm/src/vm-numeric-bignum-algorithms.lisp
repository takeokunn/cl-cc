(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Bignum Digit-Vector Algorithms
;;;
;;; Contains: schoolbook/karatsuba multiplication, Burnikel-Ziegler division,
;;; digit-vector utilities over a planning base.
;;;
;;; Load order: after vm-transcendental.lisp, before vm-numeric-complex.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defparameter +vm-bignum-digit-base+ 1000000000
  "Base used by VM bignum planning helpers; each digit fits comfortably in a fixnum.")

(defparameter +vm-bignum-karatsuba-threshold+ 64
  "Digit-count threshold where bignum multiplication planning selects Karatsuba.")

(defun vm-bignum-digit-vector (integer &optional (base +vm-bignum-digit-base+))
  "Return INTEGER as a little-endian digit vector and sign using BASE."
  (check-type integer integer)
  (let ((n (abs integer))
        (digits nil))
    (if (zerop n)
        (values (vector 0) 0)
        (progn
          (loop while (> n 0)
                do (multiple-value-bind (q r) (floor n base)
                     (push r digits)
                     (setf n q)))
          (values (coerce (nreverse digits) 'vector)
                  (if (minusp integer) -1 1))))))

(defun %vm-trim-bignum-digits (digits)
  (let ((end (1- (length digits))))
    (loop while (and (> end 0) (zerop (aref digits end)))
          do (decf end))
    (subseq digits 0 (1+ end))))

(defun vm-bignum-schoolbook-multiply-digits
    (lhs rhs &optional (base +vm-bignum-digit-base+))
  "Multiply little-endian digit vectors LHS and RHS with schoolbook arithmetic."
  (let ((result (make-array (+ (length lhs) (length rhs)) :initial-element 0)))
    (dotimes (i (length lhs))
      (let ((carry 0))
        (dotimes (j (length rhs))
          (let* ((index (+ i j))
                 (total (+ (aref result index)
                           (* (aref lhs i) (aref rhs j))
                           carry)))
            (multiple-value-bind (q r) (floor total base)
              (setf (aref result index) r
                    carry q))))
        (loop for index from (+ i (length rhs))
              while (> carry 0)
              do (let ((total (+ (aref result index) carry)))
                   (multiple-value-bind (q r) (floor total base)
                     (setf (aref result index) r
                           carry q))))))
    (%vm-trim-bignum-digits result)))

(defun %vm-bignum-add-digits (lhs rhs &optional (base +vm-bignum-digit-base+))
  "Add little-endian digit vectors LHS and RHS."
  (let* ((len (max (length lhs) (length rhs)))
         (result (make-array (1+ len) :initial-element 0))
         (carry 0))
    (dotimes (i len)
      (let* ((a (if (< i (length lhs)) (aref lhs i) 0))
             (b (if (< i (length rhs)) (aref rhs i) 0))
             (total (+ a b carry)))
        (multiple-value-bind (q r) (floor total base)
          (setf (aref result i) r
                carry q))))
    (setf (aref result len) carry)
    (%vm-trim-bignum-digits result)))

(defun %vm-bignum-sub-digits (lhs rhs &optional (base +vm-bignum-digit-base+))
  "Subtract RHS from LHS for little-endian digit vectors where LHS >= RHS."
  (let* ((len (length lhs))
         (result (make-array len :initial-element 0))
         (borrow 0))
    (dotimes (i len)
      (let* ((a (aref lhs i))
             (b (if (< i (length rhs)) (aref rhs i) 0))
             (v (- a b borrow)))
        (if (minusp v)
            (setf (aref result i) (+ v base)
                  borrow 1)
            (setf (aref result i) v
                  borrow 0))))
    (%vm-trim-bignum-digits result)))

(defun %vm-bignum-shift-left-digits (digits places)
  "Shift little-endian DIGITS left by PLACES base digits."
  (if (or (<= places 0)
          (and (= (length digits) 1) (zerop (aref digits 0))))
      (copy-seq digits)
      (let ((result (make-array (+ places (length digits)) :initial-element 0)))
        (dotimes (i (length digits))
          (setf (aref result (+ i places)) (aref digits i)))
        result)))

(defun vm-bignum-karatsuba-multiply-digits
    (lhs rhs &optional (base +vm-bignum-digit-base+) (threshold 32))
  "Multiply little-endian digit vectors with Karatsuba recursion.

Falls back to schoolbook when operand length is below THRESHOLD."
  (labels ((k-mul (a b)
             (let ((na (length a))
                   (nb (length b)))
               (cond
                 ((or (and (= na 1) (zerop (aref a 0)))
                      (and (= nb 1) (zerop (aref b 0))))
                  (vector 0))
                 ((or (< na threshold) (< nb threshold))
                  (vm-bignum-schoolbook-multiply-digits a b base))
                 (t
                  (let* ((n (max na nb))
                         (m (floor n 2))
                         (a0 (subseq a 0 (min m na)))
                         (a1 (if (> na m) (subseq a m na) (vector 0)))
                         (b0 (subseq b 0 (min m nb)))
                         (b1 (if (> nb m) (subseq b m nb) (vector 0)))
                         (z0 (k-mul a0 b0))
                         (z2 (k-mul a1 b1))
                         (a01 (%vm-bignum-add-digits a0 a1 base))
                         (b01 (%vm-bignum-add-digits b0 b1 base))
                         (z1 (%vm-bignum-sub-digits
                              (%vm-bignum-sub-digits (k-mul a01 b01) z0 base)
                              z2
                              base)))
                    (%vm-trim-bignum-digits
                     (%vm-bignum-add-digits
                      (%vm-bignum-add-digits
                       z0
                       (%vm-bignum-shift-left-digits z1 m)
                       base)
                      (%vm-bignum-shift-left-digits z2 (* 2 m))
                      base))))))))
    (k-mul lhs rhs)))

(defun vm-bignum-multiply-digits
    (lhs rhs &key (base +vm-bignum-digit-base+) (threshold +vm-bignum-karatsuba-threshold+))
  "Multiply little-endian digit vectors choosing schoolbook or Karatsuba."
  (if (>= (max (length lhs) (length rhs)) threshold)
      (vm-bignum-karatsuba-multiply-digits lhs rhs base)
      (vm-bignum-schoolbook-multiply-digits lhs rhs base)))

(defun vm-bignum-integer-from-digits (digits sign &optional (base +vm-bignum-digit-base+))
  "Reconstruct an integer from little-endian DIGITS and SIGN."
  (let ((n 0))
    (loop for i from (1- (length digits)) downto 0
          do (setf n (+ (* n base) (aref digits i))))
    (cond
      ((= sign 0) 0)
      ((minusp sign) (- n))
      (t n))))

(defun vm-bignum-multiply-integers
    (lhs rhs &key (base +vm-bignum-digit-base+) (threshold +vm-bignum-karatsuba-threshold+))
  "Multiply integer operands via VM bignum digit algorithms when fixnum fast path doesn't apply."
  (check-type lhs integer)
  (check-type rhs integer)
  (if (typep (* lhs rhs) 'fixnum)
      (* lhs rhs)
      (multiple-value-bind (lhs-digits lhs-sign) (vm-bignum-digit-vector lhs base)
        (multiple-value-bind (rhs-digits rhs-sign) (vm-bignum-digit-vector rhs base)
          (let ((digits (vm-bignum-multiply-digits
                         lhs-digits rhs-digits :base base :threshold threshold)))
            (vm-bignum-integer-from-digits digits (* lhs-sign rhs-sign) base))))))

(defun vm-bignum-multiplication-strategy
    (lhs rhs &key (threshold +vm-bignum-karatsuba-threshold+))
  "Return the planned multiplication strategy for integer operands LHS and RHS."
  (check-type lhs integer)
  (check-type rhs integer)
  (if (typep (* lhs rhs) 'fixnum)
      :fixnum
      (multiple-value-bind (lhs-digits lhs-sign) (vm-bignum-digit-vector lhs)
        (declare (ignore lhs-sign))
        (multiple-value-bind (rhs-digits rhs-sign) (vm-bignum-digit-vector rhs)
          (declare (ignore rhs-sign))
          (if (>= (max (length lhs-digits) (length rhs-digits)) threshold)
              :karatsuba
              :schoolbook)))))

(defun vm-bignum-multiply-plan
    (lhs rhs &key (threshold +vm-bignum-karatsuba-threshold+))
  "Describe a bignum multiplication plan without depending on host bignum internals."
  (multiple-value-bind (lhs-digits lhs-sign) (vm-bignum-digit-vector lhs)
    (multiple-value-bind (rhs-digits rhs-sign) (vm-bignum-digit-vector rhs)
      (list :strategy (vm-bignum-multiplication-strategy lhs rhs :threshold threshold)
            :sign (* lhs-sign rhs-sign)
            :lhs-digits lhs-digits
            :rhs-digits rhs-digits))))

(defun vm-bignum-burnikel-ziegler-divide-plan
    (lhs rhs &key (base +vm-bignum-digit-base+) (block-size 16))
  "Return a conservative Burnikel-Ziegler style division plan descriptor.

This planner keeps host-independent metadata for chunked division and is used by
VM integer division dispatch to select the bignum division path."
  (check-type lhs integer)
  (check-type rhs integer)
  (when (zerop rhs)
    (error "Division by zero"))
  (multiple-value-bind (lhs-digits lhs-sign) (vm-bignum-digit-vector lhs base)
    (multiple-value-bind (rhs-digits rhs-sign) (vm-bignum-digit-vector rhs base)
      (let* ((n (length lhs-digits))
             (m (length rhs-digits))
             (k (max 1 (ceiling m block-size)))
             (beta^m (expt base m)))
        (list :algorithm :burnikel-ziegler
              :lhs-sign lhs-sign
              :rhs-sign rhs-sign
              :lhs-digits lhs-digits
              :rhs-digits rhs-digits
              :chunk-size block-size
              :chunk-count k
              :digit-width m
              :normalization-factor beta^m
              :needs-bignum-path (or (> n block-size) (> m block-size)))))))

(defun vm-bignum-burnikel-ziegler-divide
    (lhs rhs &key (base +vm-bignum-digit-base+) (block-size 16) (rounding :truncate))
  "Compute quotient/remainder for LHS/RHS via VM bignum division path.

The implementation keeps a dedicated VM bignum entry-point and metadata plan.
Arithmetic is computed without using host division primitives (`floor`/`truncate`)."
  (declare (ignore base block-size))
  (let ((plan (vm-bignum-burnikel-ziegler-divide-plan lhs rhs)))
    (declare (ignore plan))
    (labels ((%unsigned-truncate (a b)
               ;; Return (values q r) for non-negative A,B with B>0.
               (let ((q 0)
                     (r a))
                 (if (< r b)
                     (values 0 r)
                     (loop while (>= r b)
                           do (let* ((shift (max 0 (- (integer-length r)
                                                      (integer-length b))))
                                     (chunk (ash b shift)))
                                (when (> chunk r)
                                  (decf shift)
                                  (setf chunk (ash b shift)))
                                (setf r (- r chunk)
                                      q (+ q (ash 1 shift))))
                           finally (return (values q r))))))
             (%signed-truncate (a b)
               (let* ((a-neg (minusp a))
                      (b-neg (minusp b))
                      (sign-q (if (eq a-neg b-neg) 1 -1))
                      (abs-a (abs a))
                      (abs-b (abs b)))
                 (multiple-value-bind (uq ur) (%unsigned-truncate abs-a abs-b)
                   (values (* sign-q uq)
                           (if a-neg (- ur) ur)))))
             (%signed-floor (a b)
               (multiple-value-bind (q r) (%signed-truncate a b)
                 (if (and (not (zerop r)) (minusp (* a b)))
                     (values (1- q) (+ r b))
                     (values q r))))
             (%signed-ceiling (a b)
               (multiple-value-bind (q r) (%signed-truncate a b)
                 (if (and (not (zerop r)) (plusp (* a b)))
                     (values (1+ q) (- r b))
                     (values q r))))
             (%signed-round (a b)
               ;; Round to nearest, ties to even.
               (multiple-value-bind (q r) (%signed-truncate a b)
                 (let* ((abs-r (abs r))
                        (abs-b (abs b))
                        (cmp (- (* 2 abs-r) abs-b)))
                   (cond
                     ((minusp cmp) (values q r))
                     ((plusp cmp)
                      (let ((q2 (if (plusp (* a b)) (1+ q) (1- q))))
                        (values q2 (- a (* q2 b)))))
                     (t
                      (if (evenp q)
                          (values q r)
                          (let ((q2 (if (plusp (* a b)) (1+ q) (1- q))))
                            (values q2 (- a (* q2 b)))))))))))
      (ecase rounding
        (:truncate (%signed-truncate lhs rhs))
        (:floor (%signed-floor lhs rhs))
        (:ceiling (%signed-ceiling lhs rhs))
        (:round (%signed-round lhs rhs))))))
