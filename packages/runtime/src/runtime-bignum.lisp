;;;; runtime-bignum.lisp — Native backend bignum value representation/helpers
;;;
;;; The VM interpreter has host-backed arbitrary-precision integers.  Native
;;; x86-64 overflow stubs call the helpers in this file so checked fixnum
;;; arithmetic can promote instead of trapping.  The represented heap payload is
;;; a two-word header followed by base-1e9 little-endian digits:
;;;
;;;   word 0: +rt-bignum-type-tag+
;;;   word 1: signed digit count (sign encoded in the sign of the count)
;;;   word 2..: digits, least-significant first

(in-package :cl-cc/runtime)

(defconstant +rt-bignum-type-tag+ #xB16B16
  "Runtime object type tag used for native bignum payloads.")

(defconstant +rt-bignum-digit-base+ 1000000000
  "Base for native bignum digit arrays; each digit fits comfortably in a fixnum.")

(defconstant +rt-fixnum-min+ (- (ash 1 50)))
(defconstant +rt-fixnum-max+ (1- (ash 1 50)))

(defvar *rt-native-bignum-objects* (cl:make-hash-table :test #'eql)
  "Logical heap address -> native bignum payload vector.")

(defvar *rt-native-bignum-next-address* #x100000
  "Monotonic logical address source for native bignum objects in the pure-CL runtime.")

(defun %rt-bignum-trim-digits (digits)
  (let ((end (1- (length digits))))
    (loop while (and (> end 0) (zerop (aref digits end)))
          do (decf end))
    (subseq digits 0 (1+ end))))

(defun %rt-bignum-digit-vector (integer)
  "Return INTEGER as little-endian base-1e9 digits and a sign." 
  (let ((n (abs integer))
        (digits nil))
    (if (zerop n)
        (values (vector 0) 0)
        (progn
          (loop while (> n 0)
                do (multiple-value-bind (q r) (floor n +rt-bignum-digit-base+)
                     (push r digits)
                     (setf n q)))
          (values (coerce (nreverse digits) 'vector)
                  (if (minusp integer) -1 1))))))

(defun %rt-bignum-add-digits (lhs rhs)
  (let* ((len (max (length lhs) (length rhs)))
         (result (make-array (1+ len) :initial-element 0))
         (carry 0))
    (dotimes (i len)
      (let* ((a (if (< i (length lhs)) (aref lhs i) 0))
             (b (if (< i (length rhs)) (aref rhs i) 0))
             (total (+ a b carry)))
        (multiple-value-bind (q r) (floor total +rt-bignum-digit-base+)
          (setf (aref result i) r
                carry q))))
    (setf (aref result len) carry)
    (%rt-bignum-trim-digits result)))

(defun %rt-bignum-sub-digits (lhs rhs)
  "Subtract RHS from LHS for digit vectors satisfying LHS >= RHS." 
  (let* ((len (length lhs))
         (result (make-array len :initial-element 0))
         (borrow 0))
    (dotimes (i len)
      (let* ((a (aref lhs i))
             (b (if (< i (length rhs)) (aref rhs i) 0))
             (value (- a b borrow)))
        (if (minusp value)
            (setf (aref result i) (+ value +rt-bignum-digit-base+)
                  borrow 1)
            (setf (aref result i) value
                  borrow 0))))
    (%rt-bignum-trim-digits result)))

(defun %rt-bignum-compare-digits (lhs rhs)
  (let ((lhs (%rt-bignum-trim-digits lhs))
        (rhs (%rt-bignum-trim-digits rhs)))
    (cond
      ((> (length lhs) (length rhs)) 1)
      ((< (length lhs) (length rhs)) -1)
      (t (loop for i from (1- (length lhs)) downto 0
               for a = (aref lhs i)
               for b = (aref rhs i)
               when (> a b) do (return 1)
               when (< a b) do (return -1)
               finally (return 0))))))

(defun %rt-bignum-mul-digits (lhs rhs)
  (let ((result (make-array (+ (length lhs) (length rhs)) :initial-element 0)))
    (dotimes (i (length lhs))
      (let ((carry 0))
        (dotimes (j (length rhs))
          (let* ((index (+ i j))
                 (total (+ (aref result index)
                           (* (aref lhs i) (aref rhs j))
                           carry)))
            (multiple-value-bind (q r) (floor total +rt-bignum-digit-base+)
              (setf (aref result index) r
                    carry q))))
        (loop for index from (+ i (length rhs))
              while (> carry 0)
              do (let ((total (+ (aref result index) carry)))
                   (multiple-value-bind (q r) (floor total +rt-bignum-digit-base+)
                     (setf (aref result index) r
                           carry q))))))
    (%rt-bignum-trim-digits result)))

(defun %rt-bignum-integer-from-digits (digits sign)
  (let ((n 0))
    (loop for i from (1- (length digits)) downto 0
          do (setf n (+ (* n +rt-bignum-digit-base+) (aref digits i))))
    (cond
      ((zerop sign) 0)
      ((minusp sign) (- n))
      (t n))))

(defun %rt-bignum-layout (digits sign)
  (let* ((digits (%rt-bignum-trim-digits digits))
         (count (if (or (zerop sign)
                        (and (= (length digits) 1) (zerop (aref digits 0))))
                    0
                    (* (if (minusp sign) -1 1) (length digits))))
         (payload (make-array (+ 2 (length digits)))))
    (setf (aref payload 0) +rt-bignum-type-tag+
          (aref payload 1) count)
    (dotimes (i (length digits) payload)
      (setf (aref payload (+ i 2)) (aref digits i)))))

(defun rt-native-bignum-allocate (digits sign)
  "Allocate a native bignum object and return its NaN-boxed object pointer." 
  (let* ((addr *rt-native-bignum-next-address*)
         (payload (%rt-bignum-layout digits sign)))
    (incf *rt-native-bignum-next-address* (* 8 (length payload)))
    (setf (gethash addr *rt-native-bignum-objects*) payload)
    (encode-pointer addr +tag-object+)))

(defun rt-native-bignum-object (value)
  (and (val-object-p value)
       (let ((payload (gethash (decode-pointer value) *rt-native-bignum-objects*)))
         (and payload
              (= (aref payload 0) +rt-bignum-type-tag+)
              payload))))

(defun rt-native-bignum-p (value)
  "Return true when VALUE is a native bignum pointer." 
  (not (null (rt-native-bignum-object value))))

(defun rt-native-bignum-to-integer (value)
  "Decode a native bignum object into a host integer for tests/interop." 
  (let ((payload (rt-native-bignum-object value)))
    (unless payload
      (error "rt-native-bignum-to-integer: not a native bignum #x~16,'0X" value))
    (let* ((count (aref payload 1))
           (digit-count (abs count))
           (digits (make-array (max 1 digit-count))))
      (dotimes (i (length digits))
        (setf (aref digits i) (if (< i digit-count) (aref payload (+ i 2)) 0)))
      (%rt-bignum-integer-from-digits digits (signum count)))))

(defun %rt-native-integer-value (value)
  (cond
    ((val-fixnum-p value) (decode-fixnum value))
    ((rt-native-bignum-p value) (rt-native-bignum-to-integer value))
    ((integerp value) value)
    (t (error "Expected native integer value, got ~S" value))))

(defun rt-native-integer->value (integer)
  "Encode INTEGER as a fixnum when possible, otherwise as a native bignum." 
  (if (<= +rt-fixnum-min+ integer +rt-fixnum-max+)
      (encode-fixnum integer)
      (multiple-value-bind (digits sign) (%rt-bignum-digit-vector integer)
        (rt-native-bignum-allocate digits sign))))

(defun rt-native-bignum-add (lhs rhs)
  "Native overflow helper for checked addition." 
  (let* ((a (%rt-native-integer-value lhs))
         (b (%rt-native-integer-value rhs)))
    (multiple-value-bind (ad as) (%rt-bignum-digit-vector a)
      (multiple-value-bind (bd bs) (%rt-bignum-digit-vector b)
        (cond
          ((zerop as) (rt-native-integer->value b))
          ((zerop bs) (rt-native-integer->value a))
          ((= as bs)
           (rt-native-integer->value (%rt-bignum-integer-from-digits
                                      (%rt-bignum-add-digits ad bd) as)))
          (t
           (let ((cmp (%rt-bignum-compare-digits ad bd)))
             (cond
               ((zerop cmp) (encode-fixnum 0))
               ((plusp cmp)
                (rt-native-integer->value (%rt-bignum-integer-from-digits
                                           (%rt-bignum-sub-digits ad bd) as)))
               (t
                (rt-native-integer->value (%rt-bignum-integer-from-digits
                                           (%rt-bignum-sub-digits bd ad) bs)))))))))))

(defun rt-native-bignum-sub (lhs rhs)
  "Native overflow helper for checked subtraction." 
  (rt-native-bignum-add lhs (rt-native-integer->value (- (%rt-native-integer-value rhs)))))

(defun rt-native-bignum-mul (lhs rhs)
  "Native overflow helper for checked multiplication." 
  (let* ((a (%rt-native-integer-value lhs))
         (b (%rt-native-integer-value rhs)))
    (multiple-value-bind (ad as) (%rt-bignum-digit-vector a)
      (multiple-value-bind (bd bs) (%rt-bignum-digit-vector b)
        (rt-native-integer->value
         (%rt-bignum-integer-from-digits (%rt-bignum-mul-digits ad bd)
                                         (* as bs)))))))
