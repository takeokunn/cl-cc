;;;; src/vm/vm-numeric-ext.lisp — Float rounding, rational, complex, env query instructions
;;;
;;; Extracted from vm-numeric.lisp.
;;; Contains: FR-301 float rounding (ffloor/fceiling/ftruncate/fround),
;;;           FR-306 rational number functions,
;;;           FR-307 complex number functions,
;;;           FR-507 nullary environment query functions.
;;;
;;; Depends on vm-numeric.lisp (define-vm-binary-instruction, define-vm-unary-instruction,
;;;   define-simple-instruction, %define-nullary-env-query, execute-instruction).
;;; Load order: immediately after vm-numeric.lisp.

(in-package :cl-cc)

;;; FR-301: Float rounding functions (ffloor, fceiling, ftruncate, fround)

(define-vm-binary-instruction vm-ffloor :ffloor "Float floor: returns float quotient and float remainder.")

(defmethod execute-instruction ((inst vm-ffloor) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (ffloor (vm-reg-get state (vm-lhs inst))
                                      (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-binary-instruction vm-fceiling :fceiling "Float ceiling: returns float quotient and float remainder.")

(defmethod execute-instruction ((inst vm-fceiling) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (fceiling (vm-reg-get state (vm-lhs inst))
                                        (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-binary-instruction vm-ftruncate :ftruncate "Float truncate: returns float quotient and float remainder.")

(defmethod execute-instruction ((inst vm-ftruncate) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (ftruncate (vm-reg-get state (vm-lhs inst))
                                         (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-binary-instruction vm-fround :fround "Float round: returns float quotient and float remainder.")

(defmethod execute-instruction ((inst vm-fround) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (fround (vm-reg-get state (vm-lhs inst))
                                      (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

;;; FR-306: Rational number functions

(define-vm-unary-instruction vm-rational    :rational    "Convert number to rational.")
(define-vm-unary-instruction vm-rationalize :rationalize "Rationalize a float to closest rational.")
(define-vm-unary-instruction vm-numerator   :numerator   "Return numerator of rational.")
(define-vm-unary-instruction vm-denominator :denominator "Return denominator of rational.")
(define-vm-binary-instruction vm-gcd        :gcd         "Return greatest common divisor.")
(define-vm-binary-instruction vm-lcm        :lcm         "Return least common multiple.")

(define-simple-instruction vm-rational    :unary  rational)
(define-simple-instruction vm-rationalize :unary  rationalize)
(define-simple-instruction vm-numerator   :unary  numerator)
(define-simple-instruction vm-denominator :unary  denominator)
(define-simple-instruction vm-gcd         :binary gcd)
(define-simple-instruction vm-lcm         :binary lcm)

;;; FR-307: Complex number functions

(define-vm-unary-instruction vm-realpart  :realpart  "Return real part of number.")
(define-vm-unary-instruction vm-imagpart  :imagpart  "Return imaginary part of number.")
(define-vm-unary-instruction vm-conjugate :conjugate "Return complex conjugate.")
(define-vm-unary-instruction vm-phase     :phase     "Return phase angle of complex number.")
(define-vm-binary-instruction vm-complex  :complex   "Construct a complex number from real and imaginary parts.")

(define-simple-instruction vm-realpart  :unary  realpart)
(define-simple-instruction vm-imagpart  :unary  imagpart)
(define-simple-instruction vm-conjugate :unary  conjugate)
(define-simple-instruction vm-phase     :unary  phase)
(define-simple-instruction vm-complex   :binary complex)

;;; FR-507: Environment query functions (nullary — return host CL values)

(%define-nullary-env-query vm-lisp-implementation-type :lisp-implementation-type
  (lisp-implementation-type) "Return the Lisp implementation type string.")
(%define-nullary-env-query vm-lisp-implementation-version :lisp-implementation-version
  (lisp-implementation-version) "Return the Lisp implementation version string.")
(%define-nullary-env-query vm-machine-type    :machine-type    (machine-type)    "Return the machine type string.")
(%define-nullary-env-query vm-machine-version :machine-version (machine-version) "Return the machine version string.")
(%define-nullary-env-query vm-machine-instance :machine-instance (machine-instance) "Return the machine instance string.")
(%define-nullary-env-query vm-software-type   :software-type   (software-type)   "Return the OS type string.")
(%define-nullary-env-query vm-software-version :software-version (software-version) "Return the OS version string.")
(%define-nullary-env-query vm-short-site-name :short-site-name (short-site-name) "Return the short site name string.")
(%define-nullary-env-query vm-long-site-name  :long-site-name  (long-site-name)  "Return the long site name string.")
