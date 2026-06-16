(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Native Complex Number Support (FR-956)
;;;
;;; Provides: vm-complex struct, vm-complex-make, additive/multiplicative
;;; operations, transcendental operations, print-object, %vm-externalize-complex.
;;;
;;; Load order: after vm-bignum-rational.lisp (depends on vm-make-ratio,
;;; %vm-coerce-ratio for the rational coercion layer).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defstruct (vm-complex
            (:constructor %make-vm-complex (real imag))
            (:copier nil))
  "Native VM complex value."
  real
  imag)

(defmethod print-object ((value vm-complex) stream)
  (format stream "#C(~S ~S)" (vm-complex-real value) (vm-complex-imag value)))

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

;;; ─── Additive operations (structurally identical except for the component operator) ───

(defmacro define-vm-complex-additive-op (name operator)
  "Define a complex additive operation NAME using OPERATOR (+ or -).
Both vm-complex-add and vm-complex-sub share the same structure:
  real = (OPERATOR ra rb)
  imag = (OPERATOR ia ib)
where ra/ia are the real/imag parts of LHS and rb/ib of RHS."
  `(defun ,name (lhs rhs)
     (let ((a (%vm-coerce-complex lhs))
           (b (%vm-coerce-complex rhs)))
       (vm-complex-make (,operator (vm-complex-real a) (vm-complex-real b))
                        (,operator (vm-complex-imag a) (vm-complex-imag b))))))

(define-vm-complex-additive-op vm-complex-add +)
(define-vm-complex-additive-op vm-complex-sub -)

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

(defun %vm-externalize-complex (value)
  (if (vm-complex-p value)
      (complex (vm-complex-real value) (vm-complex-imag value))
      value))
