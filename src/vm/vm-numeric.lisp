(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Numeric Tower Extensions
;;;
;;; Contains: round (FR-301), float operations (FR-305), float decode/inspect,
;;; float rounding (ffloor/fceiling/ftruncate/fround), environment predicates
;;; (FR-1202), random (FR-1205), universal-time ops (FR-1204),
;;; rational numbers (FR-306), complex numbers (FR-307).
;;;
;;; Bitwise operations (FR-303) → src/vm/vm-bitwise.lisp
;;; Transcendental functions (FR-304) → src/vm/vm-transcendental.lisp
;;;
;;; Load order: after vm-transcendental.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ------------------------------------------------------------
;;; Phase 2: Numeric Tower (FR-301 to FR-305)
;;; ------------------------------------------------------------

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

;;; Phase 3+: FR-1202 Environment Predicates and FR-1205 Random

;; FR-1202: boundp — test if a symbol has a global variable binding
(define-vm-instruction vm-boundp (vm-instruction)
  "Test if symbol has a global variable value. Returns T/nil."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :boundp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-boundp) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (result (nth-value 1 (gethash sym (vm-global-vars state)))))
    (vm-reg-set state (vm-dst inst) (if result t nil))
    (values (1+ pc) nil nil)))

;; FR-1202: fboundp — test if a symbol names a function
(define-vm-instruction vm-fboundp (vm-instruction)
  "Test if symbol names a function in the VM function table. Returns T/nil."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :fboundp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-fboundp) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (result (gethash sym (vm-function-registry state))))
    (vm-reg-set state (vm-dst inst) (if result t nil))
    (values (1+ pc) nil nil)))

;; FR-1202: makunbound — remove a global variable binding
(define-vm-instruction vm-makunbound (vm-instruction)
  "Remove global variable binding for SYM. Returns SYM."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :makunbound)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-makunbound) state pc labels)
  (declare (ignore labels))
  (let ((sym (vm-reg-get state (vm-src inst))))
    (remhash sym (vm-global-vars state))
    (vm-reg-set state (vm-dst inst) sym)
    (values (1+ pc) nil nil)))

;; FR-1202: fmakunbound — remove a function binding
(define-vm-instruction vm-fmakunbound (vm-instruction)
  "Remove function binding for SYM. Returns SYM."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :fmakunbound)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-fmakunbound) state pc labels)
  (declare (ignore labels))
  (let ((sym (vm-reg-get state (vm-src inst))))
    (remhash sym (vm-function-registry state))
    (vm-reg-set state (vm-dst inst) sym)
    (values (1+ pc) nil nil)))

;; FR-1205: random — generate a random number
(define-vm-instruction vm-random (vm-instruction)
  "Generate a random number in [0, LIMIT)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :random)
  (:sexp-slots dst src))

(define-simple-instruction vm-random :unary random)

;; FR-1205: make-random-state
(define-vm-instruction vm-make-random-state (vm-instruction)
  "Create a random state. If SRC is nil, copy current *random-state*; if t, generate fresh."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-random-state) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-random-state) state pc labels)
  (declare (ignore labels))
  (let ((arg (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (cond ((eq arg t)  (make-random-state t))
                      ((eq arg nil) (make-random-state))
                      (t (make-random-state arg))))
    (values (1+ pc) nil nil)))

;; FR-1204: get-universal-time — seconds since 1900-01-01
(define-vm-instruction vm-get-universal-time (vm-instruction)
  "Return current time as universal time integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-universal-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-universal-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-universal-time))
  (values (1+ pc) nil nil))

;; FR-1204: get-internal-real-time
(define-vm-instruction vm-get-internal-real-time (vm-instruction)
  "Return internal real time as integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-internal-real-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-internal-real-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-internal-real-time))
  (values (1+ pc) nil nil))

;; FR-1204: get-internal-run-time
(define-vm-instruction vm-get-internal-run-time (vm-instruction)
  "Return internal run time (CPU time) as integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-internal-run-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-internal-run-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-internal-run-time))
  (values (1+ pc) nil nil))

;; FR-1204: decode-universal-time — returns 9 values via vm-values-list
(define-vm-instruction vm-decode-universal-time (vm-instruction)
  "Decode universal time into (sec min hour day month year day-of-week dst-p tz)."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :decode-universal-time) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-decode-universal-time) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sec min hour date month year day dst-p zone)
      (decode-universal-time (vm-reg-get state (vm-src inst)))
    (setf (vm-values-list state) (list sec min hour date month year day dst-p zone))
    (vm-reg-set state (vm-dst inst) sec)
    (values (1+ pc) nil nil)))

;; FR-1204: encode-universal-time — takes 6 required + optional tz in a list
(define-vm-instruction vm-encode-universal-time (vm-instruction)
  "Encode (sec min hour date month year &optional zone) as universal time."
  (dst nil :reader vm-dst) (args-reg nil :reader vm-args-reg)
  (:sexp-tag :encode-universal-time) (:sexp-slots dst args-reg))

(defmethod execute-instruction ((inst vm-encode-universal-time) state pc labels)
  (declare (ignore labels))
  (let* ((args (vm-reg-get state (vm-args-reg inst)))
         (sec   (nth 0 args))
         (min   (nth 1 args))
         (hour  (nth 2 args))
         (date  (nth 3 args))
         (month (nth 4 args))
         (year  (nth 5 args))
         (zone  (nth 6 args))
         (result (if zone
                     (encode-universal-time sec min hour date month year zone)
                     (encode-universal-time sec min hour date month year))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

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
