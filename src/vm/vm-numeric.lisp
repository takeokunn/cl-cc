(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Numeric Tower Extensions
;;;
;;; Contains: round (FR-301), bit operations (FR-303), transcendental
;;; functions (FR-304), float operations (FR-305), float decode/inspect,
;;; environment predicates (FR-1202), random (FR-1205), universal-time ops,
;;; float rounding (ffloor/fceiling/ftruncate/fround), rational (FR-306),
;;; complex numbers (FR-307).
;;;
;;; Load order: after primitives.lisp.
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

;;; FR-303: Bit Operations

(define-vm-instruction vm-ash (vm-instruction)
  "Arithmetic shift: (ash integer count). Positive count = left shift."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :ash)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-ash :binary ash)

(define-vm-instruction vm-logand (vm-instruction)
  "Bitwise AND."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logand)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logand :binary logand)

(define-vm-instruction vm-logior (vm-instruction)
  "Bitwise inclusive OR."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logior)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logior :binary logior)

(define-vm-instruction vm-logxor (vm-instruction)
  "Bitwise exclusive OR."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logxor)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logxor :binary logxor)

(define-vm-instruction vm-logeqv (vm-instruction)
  "Bitwise equivalence (XNOR)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logeqv)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logeqv :binary logeqv)

(define-vm-instruction vm-lognot (vm-instruction)
  "Bitwise complement."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :lognot)
  (:sexp-slots dst src))

(define-simple-instruction vm-lognot :unary lognot)

(define-vm-instruction vm-logtest (vm-instruction)
  "Test if any bits are set in common: (logtest j k) => t/nil."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logtest)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logtest :pred2 logtest)

(define-vm-instruction vm-logbitp (vm-instruction)
  "Test if bit INDEX is set in INTEGER: (logbitp index integer) => t/nil."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logbitp)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logbitp :pred2 logbitp)

(define-vm-instruction vm-logcount (vm-instruction)
  "Count set bits in integer."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :logcount)
  (:sexp-slots dst src))

(define-simple-instruction vm-logcount :unary logcount)

(define-vm-instruction vm-integer-length (vm-instruction)
  "Number of bits needed to represent integer."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :integer-length)
  (:sexp-slots dst src))

(define-simple-instruction vm-integer-length :unary integer-length)

;;; FR-304: Transcendental Functions

(define-vm-instruction vm-expt (vm-instruction)
  "Exponentiation: (expt base power)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :expt)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-expt :binary expt)

(define-vm-instruction vm-sqrt (vm-instruction)
  "Square root."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :sqrt)
  (:sexp-slots dst src))

(define-simple-instruction vm-sqrt :unary sqrt)

(define-vm-instruction vm-exp-inst (vm-instruction)
  "e raised to the power x."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :exp)
  (:sexp-slots dst src))

(define-simple-instruction vm-exp-inst :unary exp)

(define-vm-instruction vm-log-inst (vm-instruction)
  "Natural logarithm."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :log)
  (:sexp-slots dst src))

(define-simple-instruction vm-log-inst :unary log)

(define-vm-instruction vm-sin-inst (vm-instruction)
  "Sine (radians)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :sin)
  (:sexp-slots dst src))

(define-simple-instruction vm-sin-inst :unary sin)

(define-vm-instruction vm-cos-inst (vm-instruction)
  "Cosine (radians)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :cos)
  (:sexp-slots dst src))

(define-simple-instruction vm-cos-inst :unary cos)

(define-vm-instruction vm-tan-inst (vm-instruction)
  "Tangent (radians)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :tan)
  (:sexp-slots dst src))

(define-simple-instruction vm-tan-inst :unary tan)

(define-vm-instruction vm-asin-inst (vm-instruction)
  "Arc sine."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :asin)
  (:sexp-slots dst src))

(define-simple-instruction vm-asin-inst :unary asin)

(define-vm-instruction vm-acos-inst (vm-instruction)
  "Arc cosine."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :acos)
  (:sexp-slots dst src))

(define-simple-instruction vm-acos-inst :unary acos)

(define-vm-instruction vm-atan-inst (vm-instruction)
  "Arc tangent (1-arg: atan y)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :atan)
  (:sexp-slots dst src))

(define-simple-instruction vm-atan-inst :unary atan)

(define-vm-instruction vm-atan2-inst (vm-instruction)
  "Arc tangent (2-arg: atan y x)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :atan2)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-atan2-inst :binary atan)

(define-vm-instruction vm-sinh-inst (vm-instruction)
  "Hyperbolic sine."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :sinh)
  (:sexp-slots dst src))

(define-simple-instruction vm-sinh-inst :unary sinh)

(define-vm-instruction vm-cosh-inst (vm-instruction)
  "Hyperbolic cosine."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :cosh)
  (:sexp-slots dst src))

(define-simple-instruction vm-cosh-inst :unary cosh)

(define-vm-instruction vm-tanh-inst (vm-instruction)
  "Hyperbolic tangent."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :tanh)
  (:sexp-slots dst src))

(define-simple-instruction vm-tanh-inst :unary tanh)

;;; FR-305: Float Operations

(define-vm-instruction vm-float-inst (vm-instruction)
  "Convert number to float."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-inst :unary float)

(define-vm-instruction vm-float-precision (vm-instruction)
  "Number of significant radix digits in a float."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float-precision)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-precision :unary float-precision)

(define-vm-instruction vm-float-radix (vm-instruction)
  "Radix of the float representation."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float-radix)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-radix :unary float-radix)

(define-vm-instruction vm-float-sign (vm-instruction)
  "Sign of a float as float."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float-sign)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-sign :unary float-sign)

(define-vm-instruction vm-float-digits (vm-instruction)
  "Number of radix digits in the float mantissa."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float-digits)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-digits :unary float-digits)

(define-vm-instruction vm-scale-float (vm-instruction)
  "Scale a float by a power of the radix."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :scale-float)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-scale-float :binary scale-float)

(define-vm-instruction vm-decode-float (vm-instruction)
  "Decode float into significand, exponent, sign (3 multiple values)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :decode-float)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-integer-decode-float (vm-instruction)
  "Decode float into integer significand, exponent, sign (3 multiple values)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :integer-decode-float)
  (:sexp-slots dst src))

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

(define-vm-instruction vm-ffloor (vm-instruction)
  "Float floor: returns float quotient and float remainder."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :ffloor) (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-ffloor) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (ffloor (vm-reg-get state (vm-lhs inst))
                                      (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-fceiling (vm-instruction)
  "Float ceiling: returns float quotient and float remainder."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :fceiling) (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-fceiling) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (fceiling (vm-reg-get state (vm-lhs inst))
                                        (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-ftruncate (vm-instruction)
  "Float truncate: returns float quotient and float remainder."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :ftruncate) (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-ftruncate) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (ftruncate (vm-reg-get state (vm-lhs inst))
                                         (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-fround (vm-instruction)
  "Float round: returns float quotient and float remainder."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :fround) (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-fround) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (fround (vm-reg-get state (vm-lhs inst))
                                      (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

;;; FR-306: Rational number functions

(define-vm-instruction vm-rational (vm-instruction)
  "Convert number to rational."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :rational) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-rational) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (rational (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-rationalize (vm-instruction)
  "Rationalize a float to closest rational."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :rationalize) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-rationalize) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (rationalize (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-numerator (vm-instruction)
  "Return numerator of rational."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :numerator) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-numerator) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (numerator (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-denominator (vm-instruction)
  "Return denominator of rational."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :denominator) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-denominator) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (denominator (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-gcd (vm-instruction)
  "Return greatest common divisor."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :gcd) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-gcd) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (gcd (vm-reg-get state (vm-lhs inst)) (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-lcm (vm-instruction)
  "Return least common multiple."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :lcm) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-lcm) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (lcm (vm-reg-get state (vm-lhs inst)) (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; FR-307: Complex number functions

(define-vm-instruction vm-realpart (vm-instruction)
  "Return real part of number."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :realpart) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-realpart) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (realpart (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-imagpart (vm-instruction)
  "Return imaginary part of number."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :imagpart) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-imagpart) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (imagpart (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-conjugate (vm-instruction)
  "Return complex conjugate."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :conjugate) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-conjugate) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (conjugate (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-phase (vm-instruction)
  "Return phase angle of complex number."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :phase) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-phase) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (phase (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-complex (vm-instruction)
  "Construct a complex number from real and imaginary parts."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :complex) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-complex) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (complex (vm-reg-get state (vm-lhs inst)) (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))
