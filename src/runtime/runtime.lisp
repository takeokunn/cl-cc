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

;; Arrays/Vectors, Arithmetic, Bitwise, Comparisons, and Math rt-* wrappers
;; are in runtime-ops.lisp (loaded next).

