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
(defun rt-bit-or (a b) (bit-or a b))
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

;;; ------------------------------------------------------------
;;; Strings
;;; ------------------------------------------------------------

(defun rt-make-string (len &optional (char #\Space))
  (make-string len :initial-element char))
(defun rt-string-length (s) (length s))
(defun rt-string-ref (s idx) (char s idx))
(defun rt-string-set (s idx c) (setf (char s idx) c))
(define-rt-binary-predicate rt-string=          string=)
(define-rt-binary-predicate rt-string<          string<)
(define-rt-binary-predicate rt-string>          string>)
(define-rt-binary-predicate rt-string<=         string<=)
(define-rt-binary-predicate rt-string>=         string>=)
(define-rt-binary-predicate rt-string-equal-ci  string-equal)
(define-rt-binary-predicate rt-string-lessp     string-lessp)
(define-rt-binary-predicate rt-string-greaterp  string-greaterp)
(define-rt-binary-predicate rt-string-not-equal string-not-equal)
(define-rt-binary-predicate rt-string-not-greaterp string-not-greaterp)
(define-rt-binary-predicate rt-string-not-lessp    string-not-lessp)
(defun rt-string-upcase (s) (string-upcase s))
(defun rt-string-downcase (s) (string-downcase s))
(defun rt-string-capitalize (s) (string-capitalize s))
(defun rt-string-trim (bag s) (string-trim bag s))
(defun rt-string-left-trim (bag s) (string-left-trim bag s))
(defun rt-string-right-trim (bag s) (string-right-trim bag s))
(defun rt-search-string (pattern target) (search pattern target))
(defun rt-subseq (s start &optional end)
  (if end (subseq s start end) (subseq s start)))
(defun rt-concatenate-seqs (type &rest seqs)
  (apply #'concatenate type seqs))

;;; ------------------------------------------------------------
;;; Characters
;;; ------------------------------------------------------------

(defun rt-char (s idx) (char s idx))
(defun rt-char-code (c) (char-code c))
(defun rt-code-char (n) (code-char n))
(define-rt-binary-predicate rt-char-equal-cs      char=)
(define-rt-binary-predicate rt-char-lt-cs         char<)
(define-rt-binary-predicate rt-char-gt-cs         char>)
(define-rt-binary-predicate rt-char-le-cs         char<=)
(define-rt-binary-predicate rt-char-ge-cs         char>=)
(define-rt-binary-predicate rt-char-ne-cs         char/=)
(define-rt-binary-predicate rt-char-equal-ci      char-equal)
(define-rt-binary-predicate rt-char-not-equal-ci  char-not-equal)
(define-rt-binary-predicate rt-char-lessp-ci      char-lessp)
(define-rt-binary-predicate rt-char-greaterp-ci   char-greaterp)
(define-rt-binary-predicate rt-char-not-lessp-ci  char-not-lessp)
(define-rt-binary-predicate rt-char-not-greaterp-ci char-not-greaterp)
(defun rt-char-upcase (c) (char-upcase c))
(defun rt-char-downcase (c) (char-downcase c))
(define-rt-predicate rt-alpha-char-p    alpha-char-p)
(define-rt-predicate rt-digit-char-p    digit-char-p)
(define-rt-predicate rt-alphanumericp   alphanumericp)
(define-rt-predicate rt-upper-case-p    upper-case-p)
(define-rt-predicate rt-lower-case-p    lower-case-p)
(define-rt-predicate rt-both-case-p     both-case-p)
(define-rt-predicate rt-graphic-char-p  graphic-char-p)
(define-rt-predicate rt-standard-char-p standard-char-p)
(defun rt-digit-char (w &optional (radix 10)) (digit-char w radix))
(defun rt-char-name (c) (char-name c))
(defun rt-name-char (s) (name-char s))
(defun rt-parse-integer (s &key (start 0) end (radix 10) junk-allowed)
  (parse-integer s :start start :end end :radix radix :junk-allowed junk-allowed))

;;; ------------------------------------------------------------
;;; Symbols
;;; ------------------------------------------------------------

(defun rt-symbol-name (sym) (symbol-name sym))
(defun rt-intern (name &optional (pkg *package*)) (intern name pkg))
(defun rt-make-symbol (name) (make-symbol name))
(defun rt-gensym (&optional (prefix "G")) (gensym prefix))
(defun rt-symbol-value (sym) (symbol-value sym))
(defun rt-set-symbol-value (sym val) (setf (symbol-value sym) val))
(defun rt-symbol-plist (sym) (symbol-plist sym))
(defun rt-get-prop (sym indicator) (get sym indicator))
(defun rt-put-prop (sym indicator val) (setf (get sym indicator) val))
(defun rt-remprop (sym indicator) (remprop sym indicator))

;;; ------------------------------------------------------------
;;; Hash Tables
;;; ------------------------------------------------------------

(defun rt-make-hash-table (&key (test #'eql) size)
  (if size
      (make-hash-table :test test :size size)
      (make-hash-table :test test)))
(defun rt-gethash (key ht) (gethash key ht))
(defun rt-sethash (key ht val) (setf (gethash key ht) val))
(defun rt-remhash (key ht) (remhash key ht))
(defun rt-clrhash (ht) (clrhash ht))
(defun rt-hash-count (ht) (hash-table-count ht))
(defun rt-hash-test (ht) (hash-table-test ht))
(defun rt-maphash (fn ht) (maphash fn ht))
(defun rt-hash-keys (ht)
  (let (keys) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht) keys))
(defun rt-hash-values (ht)
  (let (vals) (maphash (lambda (k v) (declare (ignore k)) (push v vals)) ht) vals))

;;; ------------------------------------------------------------
;;; CLOS
;;; ------------------------------------------------------------

(defun rt-defclass (name direct-supers slots)
  (declare (ignore name direct-supers slots))
  nil)

(defun rt-defclass-from-reg ()
  nil)

(defun rt-make-instance (class &rest initargs)
  (apply #'make-instance class initargs))

(defun rt-make-instance-0 (class)
  (make-instance class))

(defun rt-slot-value (obj slot-name)
  (slot-value obj slot-name))

(defun rt-slot-set (obj slot-name val)
  (setf (slot-value obj slot-name) val))

(defun rt-slot-boundp (obj slot-name)
  (if (slot-boundp obj slot-name) 1 0))

(defun rt-slot-makunbound (obj slot-name)
  (slot-makunbound obj slot-name))

(defun rt-slot-exists-p (obj slot-name)
  (if (slot-exists-p obj slot-name) 1 0))

(defun rt-class-of (obj) (class-of obj))
(defun rt-find-class (name) (find-class name nil))
(defun rt-register-method (gf specs method) (declare (ignore gf specs method)) nil)
(defun rt-call-generic (gf &rest args) (apply gf args))

;;; ------------------------------------------------------------
;;; Conditions / Error Handling
;;; ------------------------------------------------------------

(defun rt-signal-error (condition)
  (error condition))

(defun rt-signal (condition)
  (signal condition))

(defun rt-warn-fn (condition)
  (warn "~A" condition))

(defun rt-cerror (continue-string condition)
  (cerror continue-string "~A" condition))

(defun rt-establish-handler ()
  nil)

(defun rt-remove-handler ()
  nil)

(defun rt-push-handler (type fn)
  (declare (ignore type fn)) nil)

(defun rt-pop-handler ()
  nil)

(defun rt-bind-restart (name fn thunk)
  (declare (ignore name fn))
  (funcall thunk))

(defun rt-invoke-restart (name &rest args)
  (apply #'invoke-restart name args))

;;; ------------------------------------------------------------
;;; Misc
;;; ------------------------------------------------------------

(defun rt-boundp (sym) (if (boundp sym) 1 0))
(defun rt-fboundp (sym) (if (fboundp sym) 1 0))
(defun rt-makunbound (sym) (makunbound sym))
(defun rt-fmakunbound (sym) (fmakunbound sym))
(defun rt-random (n) (random n))
(defun rt-make-random-state (&optional state)
  (if state (make-random-state state) (make-random-state)))
(defun rt-get-universal-time () (get-universal-time))
(defun rt-get-internal-real-time () (get-internal-real-time))
(defun rt-get-internal-run-time () (get-internal-run-time))
(defun rt-eval (x) (cl-cc:our-eval x))
(defun rt-read-from-string (s) (read-from-string s))
(defun rt-read-sexp (stream) (read stream))
(defun rt-coerce (x type) (coerce x type))
(defun rt-write-to-string (x) (write-to-string x))

;;; ------------------------------------------------------------
;;; I/O
;;; ------------------------------------------------------------

(defun rt-print (x) (print x))
(defun rt-princ (x) (princ x))
(defun rt-prin1 (x) (prin1 x))
(defun rt-terpri () (terpri))
(defun rt-fresh-line () (fresh-line))
(defun rt-write-char (c &optional stream)
  (if stream (write-char c stream) (write-char c)))
(defun rt-write-string (s &optional stream)
  (if stream (write-string s stream) (write-string s)))
(defun rt-write-line (s &optional stream)
  (if stream (write-line s stream) (write-line s)))
(defun rt-write-byte (byte &optional stream)
  (if stream (write-byte byte stream) (write-byte byte *standard-output*)))
(defun rt-format (stream fmt &rest args)
  (apply #'format stream fmt args))
(defun rt-read-char (&optional stream)
  (if stream (read-char stream) (read-char)))
(defun rt-read-line (&optional stream)
  (if stream (read-line stream) (read-line)))
(defun rt-read-byte (&optional stream)
  (if stream (read-byte stream) (read-byte *standard-input*)))
(defun rt-peek-char (&optional stream)
  (if stream (peek-char nil stream nil nil) (peek-char nil *standard-input* nil nil)))
(defun rt-unread-char (c &optional stream)
  (if stream (unread-char c stream) (unread-char c)))
(defun rt-open-file (path &key (direction :input) if-exists)
  (open path :direction direction :if-exists (or if-exists :supersede)))
(defun rt-close-file (stream) (close stream))
(defun rt-make-string-stream (s &key (direction :input))
  (if (eq direction :input)
      (make-string-input-stream s)
      (make-string-output-stream)))
(defun rt-get-string-from-stream (stream)
  (get-output-stream-string stream))
(defun rt-make-string-output-stream () (make-string-output-stream))
(defun rt-get-output-stream-string (stream) (get-output-stream-string stream))
(defun rt-stream-write-string (stream s) (write-string s stream))
(defun rt-finish-output (&optional stream)
  (if stream (finish-output stream) (finish-output)))
(defun rt-force-output (&optional stream)
  (if stream (force-output stream) (force-output)))
(defun rt-clear-output (&optional stream)
  (if stream (clear-output stream) (clear-output)))
(defun rt-input-stream-p (s) (if (input-stream-p s) 1 0))
(defun rt-output-stream-p (s) (if (output-stream-p s) 1 0))
(defun rt-open-stream-p (s) (if (open-stream-p s) 1 0))
(defun rt-interactive-stream-p (s) (if (interactive-stream-p s) 1 0))
(defun rt-stream-element-type (s) (stream-element-type s))
(defun rt-make-broadcast-stream (&rest streams) (apply #'make-broadcast-stream streams))
(defun rt-make-two-way-stream (in out) (make-two-way-stream in out))
(defun rt-make-echo-stream (in out) (make-echo-stream in out))
(defun rt-make-concatenated-stream (&rest streams) (apply #'make-concatenated-stream streams))
(defun rt-probe-file (path) (probe-file path))
(defun rt-truename (path) (truename path))
(defun rt-rename-file (old new) (rename-file old new))
(defun rt-delete-file (path) (delete-file path))
(defun rt-directory (path) (directory path))
(defun rt-make-pathname (&key host device directory name type version)
  (make-pathname :host host :device device :directory directory
                 :name name :type type :version version))
(defun rt-namestring (path) (namestring path))
(defun rt-pathname-component (path component)
  (case component
    (:host (pathname-host path))
    (:device (pathname-device path))
    (:directory (pathname-directory path))
    (:name (pathname-name path))
    (:type (pathname-type path))
    (:version (pathname-version path))))
(defun rt-merge-pathnames (path defaults) (merge-pathnames path defaults))
(defun rt-enough-namestring (path defaults) (enough-namestring path defaults))
