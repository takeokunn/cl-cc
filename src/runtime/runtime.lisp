;;;; src/runtime/runtime.lisp - CL-CC Runtime Library
;;;
;;; This library provides the runtime support for x86-64 native code.
;;; Each function here corresponds to a cl_rt_* assembly symbol called
;;; by the generated machine code via CALL instructions.
;;;
;;; Tagged pointer layout (3-bit tag in bits[2:0]):
;;;   000 (0) = fixnum: integer value in bits[63:3]
;;;   001 (1) = cons: pointer to cons cell
;;;   010 (2) = symbol: pointer to symbol
;;;   011 (3) = function/closure: pointer to function
;;;   100 (4) = character: char-code in bits[63:3]
;;;   101 (5) = array/vector: pointer to array
;;;   110 (6) = string: pointer to string
;;;   111 (7) = other heap object

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Tagged Pointer Constants
;;; ------------------------------------------------------------

(defconstant +tag-fixnum+     0)
(defconstant +rt-tag-cons+    1)
(defconstant +rt-tag-symbol+  2)
(defconstant +rt-tag-function+ 3)
(defconstant +tag-character+  4)
(defconstant +tag-array+      5)
(defconstant +rt-tag-string+  6)
(defconstant +tag-other+      7)

(defun rt-tag-fixnum (n)    (ash n 3))
(defun rt-untag-fixnum (x)  (ash x -3))
(defun rt-tag-bits (x)      (logand x 7))

;;; ------------------------------------------------------------
;;; Multiple Values Buffer
;;; ------------------------------------------------------------

(defvar *rt-values-buffer* nil
  "Thread-local buffer for multiple return values.")

(defun rt-values-clear ()
  (setf *rt-values-buffer* nil))

(defun rt-values-push (val)
  (setf *rt-values-buffer* (append *rt-values-buffer* (list val))))

(defun rt-values-count ()
  (length *rt-values-buffer*))

(defun rt-values-ref (idx)
  (nth idx *rt-values-buffer*))

(defun rt-values-to-list ()
  *rt-values-buffer*)

(defun rt-spread-values (val)
  "If val is a list (from multiple values), spread it; otherwise push single."
  (if (listp val)
      (dolist (v val) (rt-values-push v))
      (rt-values-push val))
  val)

(defun rt-ensure-values (val)
  "Ensure at least one value is in the buffer."
  (when (null *rt-values-buffer*)
    (rt-values-push val))
  val)

;;; ------------------------------------------------------------
;;; Closure Support
;;; ------------------------------------------------------------

(defstruct (rt-closure-obj (:conc-name rt-closure-))
  fn
  env)

(defun rt-make-closure (fn env-list)
  (make-rt-closure-obj :fn fn :env (coerce env-list 'vector)))

(defun rt-closure-ref (closure idx)
  (aref (rt-closure-env closure) idx))

(defun rt-call-fn (fn &rest args)
  (if (rt-closure-obj-p fn)
      (apply (rt-closure-fn fn) args)
      (apply fn args)))

(defun rt-apply-fn (fn args-list)
  (if (rt-closure-obj-p fn)
      (apply (rt-closure-fn fn) args-list)
      (apply fn args-list)))

(defun rt-call-next-method (&rest args)
  (declare (ignore args))
  (error "call-next-method: no next method"))

(defun rt-next-method-p ()
  nil)

(defun rt-register-function (name fn)
  (setf (symbol-function name) fn))

;;; ------------------------------------------------------------
;;; Global Bindings
;;; ------------------------------------------------------------

(defun rt-get-global (sym)
  (symbol-value sym))

(defun rt-set-global (sym val)
  (setf (symbol-value sym) val))

;;; ------------------------------------------------------------
;;; Type Predicates
;;; ------------------------------------------------------------

(defmacro define-rt-predicate (name predicate)
  "Define an rt-* unary predicate returning 1/0 based on PREDICATE applied to its argument."
  `(defun ,name (x) (if (,predicate x) 1 0)))

(defmacro define-rt-binary-predicate (name op)
  "Define an rt-* binary predicate returning 1/0 based on OP applied to (a b)."
  `(defun ,name (a b) (if (,op a b) 1 0)))

(define-rt-predicate rt-consp        consp)
(define-rt-predicate rt-null-p       null)
(define-rt-predicate rt-symbolp      symbolp)
(define-rt-predicate rt-numberp      numberp)
(define-rt-predicate rt-integerp     integerp)
(define-rt-predicate rt-floatp       floatp)
(define-rt-predicate rt-stringp      stringp)
(define-rt-predicate rt-characterp   characterp)
(define-rt-predicate rt-vectorp      vectorp)
(define-rt-predicate rt-listp        listp)
(define-rt-predicate rt-atomp        atom)
(define-rt-predicate rt-keywordp     keywordp)
(define-rt-predicate rt-hash-table-p hash-table-p)

;;; rt-functionp has a compound check — stays explicit
(defun rt-functionp (x)
  (if (or (functionp x) (rt-closure-obj-p x)) 1 0))

(defun rt-typep (x type-name)
  (if (typep x (find-symbol (string type-name) :cl)) 1 0))

(defun rt-type-of (x)
  (type-of x))

;;; ------------------------------------------------------------
;;; Cons / List Operations
;;; ------------------------------------------------------------

(defun rt-cons (car cdr) (cons car cdr))
(defun rt-car (x) (car x))
(defun rt-cdr (x) (cdr x))
(defun rt-rplaca (cons val) (rplaca cons val) nil)
(defun rt-rplacd (cons val) (rplacd cons val) nil)
(defun rt-make-list (n &optional (init nil)) (make-list n :initial-element init))
(defun rt-list-length (l) (length l))
(defun rt-append (a b) (append a b))
(defun rt-nconc (a b) (nconc a b))
(defun rt-reverse (l) (reverse l))
(defun rt-nreverse (l) (nreverse l))
(defun rt-member (x l) (member x l))
(defun rt-nth (n l) (nth n l))
(defun rt-nthcdr (n l) (nthcdr n l))
(defun rt-last (l) (last l))
(defun rt-butlast (l) (butlast l))
(defun rt-copy-list (l) (copy-list l))
(defun rt-copy-tree (l) (copy-tree l))
(defun rt-assoc (key alist) (assoc key alist))
(defun rt-acons (key val alist) (acons key val alist))
(defun rt-subst (new old tree) (subst new old tree))
(defun rt-first (l) (first l))
(defun rt-second (l) (second l))
(defun rt-third (l) (third l))
(defun rt-fourth (l) (fourth l))
(defun rt-fifth (l) (fifth l))
(defun rt-rest (l) (rest l))
(define-rt-predicate rt-endp endp)
(define-rt-predicate rt-null null)
(defun rt-push-list (val list-place) (cons val list-place))
(defun rt-pop-list (list-place) (values (car list-place) (cdr list-place)))
(define-rt-binary-predicate rt-equal equal)
(defun rt-string-coerce (x) (string x))
(defun rt-coerce-to-string (x) (if (stringp x) x (format nil "~A" x)))
(defun rt-coerce-to-list (x) (coerce x 'list))
(defun rt-coerce-to-vector (x) (coerce x 'vector))

;;; ------------------------------------------------------------
;;; Arrays / Vectors
;;; ------------------------------------------------------------

(defun rt-make-array (dims &key (element-type t) initial-element fill-pointer adjustable)
  (if initial-element
      (make-array dims :element-type element-type :initial-element initial-element
                  :fill-pointer fill-pointer :adjustable adjustable)
      (make-array dims :element-type element-type
                  :fill-pointer fill-pointer :adjustable adjustable)))

(defun rt-aref (a &rest indices) (apply #'aref a indices))
(defun rt-aset (a &rest indices-then-value)
  (let ((indices (butlast indices-then-value))
        (val (car (last indices-then-value))))
    (apply #'(setf aref) val a indices)))
(defun rt-array-length (a) (length a))
(defun rt-array-rank (a) (array-rank a))
(defun rt-array-dimension (a axis) (array-dimension a axis))
(defun rt-array-dimensions (a) (array-dimensions a))
(defun rt-array-total-size (a) (array-total-size a))
(defun rt-row-major-aref (a idx) (row-major-aref a idx))
(defun rt-array-row-major-index (a &rest indices) (apply #'array-row-major-index a indices))
(defun rt-vector-push (val vec) (vector-push val vec))
(defun rt-vector-push-extend (val vec) (vector-push-extend val vec))
(defun rt-vector-pop (vec) (vector-pop vec))
(defun rt-fill-pointer (vec) (fill-pointer vec))
(defun rt-set-fill-pointer (vec n) (setf (fill-pointer vec) n))
(define-rt-predicate rt-array-has-fill-pointer-p array-has-fill-pointer-p)
(define-rt-predicate rt-array-adjustable-p       adjustable-array-p)
(defun rt-adjust-array (arr dims &rest kwargs) (apply #'adjust-array arr dims kwargs))
(defun rt-array-displacement (arr) (array-displacement arr))
(defun rt-svref (vec idx) (svref vec idx))
(defun rt-svset (vec idx val) (setf (svref vec idx) val))

;; Bit arrays
(defun rt-bit-access (bv idx) (bit bv idx))
(defun rt-bit-set (bv idx val) (setf (bit bv idx) val))
(defun rt-bit-and (a b) (bit-and a b))
(defun rt-bit-or (a b) (bit-ior a b))
(defun rt-bit-xor (a b) (bit-xor a b))
(defun rt-bit-not (a) (bit-not a))
(defun rt-sbit (sv idx) (sbit sv idx))

;;; ------------------------------------------------------------
;;; Arithmetic
;;; ------------------------------------------------------------

(defun rt-add (a b) (+ a b))
(defun rt-sub (a b) (- a b))
(defun rt-mul (a b) (* a b))
(defun rt-div (a b) (/ a b))
(defun rt-mod (a b) (mod a b))
(defun rt-rem (a b) (rem a b))
(defun rt-neg (a) (- a))
(defun rt-abs (a) (abs a))
(defun rt-inc (a) (1+ a))
(defun rt-dec (a) (1- a))
(defun rt-min (a b) (min a b))
(defun rt-max (a b) (max a b))
(defun rt-cl-and (a b) (and a b))
(defun rt-cl-or (a b) (or a b))
(defun rt-not (x) (if x 0 1))
(define-rt-predicate rt-evenp  evenp)
(define-rt-predicate rt-oddp   oddp)
(define-rt-predicate rt-zerop  zerop)
(define-rt-predicate rt-plusp  plusp)
(define-rt-predicate rt-minusp minusp)

;;; ------------------------------------------------------------
;;; Bitwise
;;; ------------------------------------------------------------

(defun rt-ash (n count) (ash n count))
(defun rt-logand (a b) (logand a b))
(defun rt-logior (a b) (logior a b))
(defun rt-logxor (a b) (logxor a b))
(defun rt-logeqv (a b) (logeqv a b))
(defun rt-lognot (a) (lognot a))
(defun rt-logtest (a b) (if (logtest a b) 1 0))
(defun rt-logbitp (idx n) (if (logbitp idx n) 1 0))
(defun rt-logcount (n) (logcount n))
(defun rt-integer-length (n) (integer-length n))

;;; ------------------------------------------------------------
;;; Comparisons
;;; ------------------------------------------------------------

(define-rt-binary-predicate rt-eq       eq)
(define-rt-binary-predicate rt-eql      eql)
(define-rt-binary-predicate rt-equal-fn equal)
(define-rt-binary-predicate rt-lt       <)
(define-rt-binary-predicate rt-gt       >)
(define-rt-binary-predicate rt-le       <=)
(define-rt-binary-predicate rt-ge       >=)
(define-rt-binary-predicate rt-num-eq   =)

;;; ------------------------------------------------------------
;;; Math
;;; ------------------------------------------------------------

(defun rt-expt (base exp) (expt base exp))
(defun rt-sqrt (x) (sqrt x))
(defun rt-exp (x) (exp x))
(defun rt-log (x) (log x))
(defun rt-sin (x) (sin x))
(defun rt-cos (x) (cos x))
(defun rt-tan (x) (tan x))
(defun rt-asin (x) (asin x))
(defun rt-acos (x) (acos x))
(defun rt-atan (x) (atan x))
(defun rt-atan2 (y x) (atan y x))
(defun rt-sinh (x) (sinh x))
(defun rt-cosh (x) (cosh x))
(defun rt-tanh (x) (tanh x))
(defun rt-floor (x) (floor x))
(defun rt-ceiling (x) (ceiling x))
(defun rt-truncate (x) (truncate x))
(defun rt-round (x) (round x))
(defun rt-ffloor (x) (ffloor x))
(defun rt-fceiling (x) (fceiling x))
(defun rt-ftruncate (x) (ftruncate x))
(defun rt-fround (x) (fround x))
(defun rt-float (x) (float x))
(defun rt-float-precision (x) (float-precision x))
(defun rt-float-radix (x) (float-radix x))
(defun rt-float-sign (x) (float-sign x))
(defun rt-float-digits (x) (float-digits x))
(defun rt-decode-float (x) (decode-float x))
(defun rt-integer-decode-float (x) (integer-decode-float x))
(defun rt-scale-float (x k) (scale-float x k))
(defun rt-rational (x) (rational x))
(defun rt-rationalize (x) (rationalize x))
(defun rt-numerator (x) (numerator x))
(defun rt-denominator (x) (denominator x))
(defun rt-realpart (x) (realpart x))
(defun rt-imagpart (x) (imagpart x))
(defun rt-conjugate (x) (conjugate x))
(defun rt-phase (x) (phase x))
(defun rt-complex (r i) (complex r i))
(defun rt-gcd (a b) (gcd a b))
(defun rt-lcm (a b) (lcm a b))

