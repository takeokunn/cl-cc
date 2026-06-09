;;;; Runtime helpers for PHP lowering.

(in-package :cl-cc/php)

(defconstant +php-array-order-key+ :__php-array-order
  "Reserved hash-table key storing PHP array insertion order.")

(defconstant +php-array-next-index-key+ :__php-array-next-index
  "Reserved hash-table key storing the next PHP auto-increment index.")

(defvar +php-null+ '%php-null%
  "PHP null sentinel distinct from CL nil.")

(defun %php-null-p (x)
  "Return true when X is the PHP null sentinel."
  (eq x +php-null+))

;;; ─── Predefined constants ──────────────────────────────────────────────────
;;; PHP magic/predefined constants (PHP_EOL, M_PI, STR_PAD_LEFT, SORT_*, …) are
;;; referenced BARE — `echo PHP_EOL;`, `str_pad($s,3,'0',STR_PAD_LEFT)` — not as
;;; calls.  The parser (php-parse-primary) consults this table and lowers a hit
;;; to a literal; a miss stays an ast-var so user define()/const values still
;;; resolve at runtime.  (Previously these were zero-arg lambda builtins that
;;; were never invoked, so every bare reference read as an undefined var → "".)
(defparameter *php-predefined-constants*
  (let ((h (make-hash-table :test #'equal)))
    (flet ((c (name value) (setf (gethash name h) value)))
      ;; core
      (c "PHP_EOL" (string #\Newline))
      (c "PHP_INT_MAX" 9223372036854775807)
      (c "PHP_INT_MIN" -9223372036854775808)
      (c "PHP_INT_SIZE" 8)
      (c "PHP_FLOAT_MAX" most-positive-double-float)
      (c "PHP_FLOAT_MIN" least-positive-normalized-double-float)
      (c "PHP_FLOAT_EPSILON" 2.220446049250313d-16)
      (c "PHP_FLOAT_DIG" 15)
      (c "PHP_VERSION" "8.4.0")
      (c "PHP_VERSION_ID" 80400)
      (c "PHP_MAJOR_VERSION" 8)
      (c "PHP_MINOR_VERSION" 4)
      (c "PHP_RELEASE_VERSION" 0)
      (c "PHP_OS" "Linux")
      (c "PHP_OS_FAMILY" "Linux")
      (c "PHP_MAXPATHLEN" 4096)
      (c "PHP_SAPI" "cli")
      (c "DIRECTORY_SEPARATOR" "/")
      (c "PATH_SEPARATOR" ":")
      ;; math
      (c "M_PI" pi)
      (c "M_E" (exp 1.0d0))
      (c "M_SQRT2" (sqrt 2.0d0))
      (c "M_SQRT3" (sqrt 3.0d0))
      (c "M_SQRT1_2" (/ 1.0d0 (sqrt 2.0d0)))
      (c "M_2_SQRTPI" (/ 2.0d0 (sqrt pi)))
      (c "M_1_PI" (/ 1.0d0 pi))
      (c "M_2_PI" (/ 2.0d0 pi))
      (c "M_PI_2" (/ pi 2))
      (c "M_PI_4" (/ pi 4))
      (c "M_LN2" (log 2.0d0))
      (c "M_LN10" (log 10.0d0))
      (c "M_LOG2E" (log (exp 1.0d0) 2.0d0))
      (c "M_LOG10E" (log (exp 1.0d0) 10.0d0))
      (c "M_EULER" 0.5772156649015329d0)
      (c "INF" most-positive-double-float)
      (c "NAN" most-positive-double-float)
      ;; error reporting
      (c "E_ALL" 32767) (c "E_ERROR" 1) (c "E_WARNING" 2) (c "E_NOTICE" 8)
      (c "E_STRICT" 2048) (c "E_DEPRECATED" 8192) (c "E_USER_ERROR" 256)
      (c "E_USER_WARNING" 512) (c "E_USER_NOTICE" 1024)
      ;; sort
      (c "SORT_REGULAR" 0) (c "SORT_NUMERIC" 1) (c "SORT_STRING" 2)
      (c "SORT_DESC" 3) (c "SORT_ASC" 4) (c "SORT_NATURAL" 6) (c "SORT_FLAG_CASE" 8)
      ;; count / array_filter
      (c "COUNT_NORMAL" 0) (c "COUNT_RECURSIVE" 1)
      (c "ARRAY_FILTER_USE_BOTH" 1) (c "ARRAY_FILTER_USE_KEY" 2)
      ;; str_pad
      (c "STR_PAD_RIGHT" 1) (c "STR_PAD_LEFT" 0) (c "STR_PAD_BOTH" 2)
      ;; htmlspecialchars
      (c "ENT_HTML401" 0) (c "ENT_QUOTES" 3) (c "ENT_COMPAT" 2)
      (c "ENT_NOQUOTES" 0) (c "ENT_HTML5" 48)
      ;; json
      (c "JSON_PRETTY_PRINT" 128) (c "JSON_UNESCAPED_UNICODE" 256)
      (c "JSON_UNESCAPED_SLASHES" 64) (c "JSON_THROW_ON_ERROR" 4194304)
      (c "JSON_ERROR_NONE" 0) (c "JSON_HEX_TAG" 1)
      ;; filesystem
      (c "FILE_APPEND" 8) (c "FILE_USE_INCLUDE_PATH" 1)
      (c "FILE_IGNORE_NEW_LINES" 2) (c "FILE_SKIP_EMPTY_LINES" 4)
      (c "LOCK_SH" 1) (c "LOCK_EX" 2) (c "LOCK_UN" 3)
      (c "SEEK_SET" 0) (c "SEEK_CUR" 1) (c "SEEK_END" 2)
      ;; preg
      (c "PREG_SPLIT_NO_EMPTY" 1) (c "PREG_SPLIT_DELIM_CAPTURE" 2)
      (c "PREG_PATTERN_ORDER" 1) (c "PREG_SET_ORDER" 2) (c "PREG_OFFSET_CAPTURE" 256)
      ;; round modes
      (c "PHP_ROUND_HALF_UP" 1) (c "PHP_ROUND_HALF_DOWN" 2)
      (c "PHP_ROUND_HALF_EVEN" 3) (c "PHP_ROUND_HALF_ODD" 4))
    h)
  "Name (case-sensitive string) → value for PHP predefined constants.")

(defun %php-lookup-constant (name)
  "Return (values VALUE T) when NAME is a predefined PHP constant, else
(values NIL NIL).  Tries NAME, then its unqualified tail (after the last \\) so
\\PHP_EOL and NS\\PHP_EOL resolve to the global constant."
  (multiple-value-bind (value found) (gethash name *php-predefined-constants*)
    (if found
        (values value t)
        (let ((sep (position (code-char 92) name :from-end t)))
          (if sep
              (gethash (subseq name (1+ sep)) *php-predefined-constants*)
              (values nil nil))))))

(defun %php-array-empty-p (ht)
  "Return true when PHP ordered array HT contains no user entries."
  (check-type ht hash-table)
  (null (gethash +php-array-order-key+ ht)))

(defun %php-value-type (x)
  "Return the PHP runtime value type keyword for X."
  (cond ((%php-null-p x) :null)
        ((null x) :bool)
        ((eq x t) :bool)   ; PHP true; without this, type-of t -> BOOLEAN, so
                           ; gettype(true) was "object" and gettype(5>3) wrong
        ((integerp x) :int)
        ((floatp x) :float)
        ((stringp x) :string)
        ((hash-table-p x) :array)
        (t (type-of x))))

(define-condition php-exception (error)
  ((class-name :initarg :class-name :reader php-exception-class-name)
   (value :initarg :value :reader php-exception-value))
  (:report (lambda (condition stream)
             (format stream "PHP exception~@[ of class ~A~]: ~S"
                     (php-exception-class-name condition)
                     (php-exception-value condition)))))

;;; -----------------------------------------------------------------------
;;;  PHP Reference semantics — box pattern
;;; -----------------------------------------------------------------------
;;;
;;; PHP references (&$var) are implemented as single-element vectors (boxes).
;;; The parser emits %php-make-ref at call sites for by-reference arguments,
;;; and callee bodies use %php-deref / %php-ref-set! to read and write through
;;; the box so mutations are visible to the caller after the call returns.

(defstruct (php-ref (:conc-name php-ref-))
  (value nil))

(defun %php-ref-p (x) (php-ref-p x))

(defun %php-make-ref (initial-value)
  "Create a PHP reference box holding INITIAL-VALUE."
  (make-php-ref :value initial-value))

(defun %php-deref (ref)
  "Dereference a PHP reference box, returning its current value."
  (if (php-ref-p ref)
      (php-ref-value ref)
      ref))  ; non-ref passthrough for caller safety

(defun %php-ref-set! (ref new-value)
  "Mutate the PHP reference box REF to hold NEW-VALUE; return NEW-VALUE."
  (if (php-ref-p ref)
      (setf (php-ref-value ref) new-value)
      new-value))

;;; -----------------------------------------------------------------------
;;;  foreach by-reference iteration
;;; -----------------------------------------------------------------------

(defun %php-foreach-by-ref (arr body-fn)
  "Iterate PHP ordered array ARR calling BODY-FN(box key) for each element.
BODY-FN receives a ref box wrapping the current value and the key.
After BODY-FN returns the box is written back to the array (mutations propagate)."
  (when (hash-table-p arr)
    (dolist (key (gethash +php-array-order-key+ arr))
      (let* ((current-val (gethash key arr +php-null+))
             (box (%php-make-ref current-val)))
        (funcall body-fn box key)
        ;; Write back mutations
        (let ((new-val (%php-deref box)))
          (unless (eq new-val current-val)
            (setf (gethash key arr) new-val))))))
  +php-null+)

;;; -----------------------------------------------------------------------
;;;  PHP Generator — eager evaluation with pre-collected value queue
;;; -----------------------------------------------------------------------
;;;
;;; PHP generators (yield/yield from) collect all yielded values by running
;;; the body thunk with *current-generator* set to a collector object.
;;; This is simpler than thread-based coroutines and correct for finite generators.
;;; Infinite generators would exhaust memory, but they're rare in practice.

(defstruct (php-generator (:conc-name php-gen-))
  values        ; queue (list) of yielded values, innermost first
  return-value  ; the generator's final return value
  done-p)       ; true when all values have been consumed

(defun %php-has-method (instance name-sym)
  "Return true when INSTANCE's class defines a slot/method named NAME-SYM. Used by
the `new C(args)' lowering to call __construct only when it exists — the call
itself runs through the normal method-dispatch (vm-call) path, so the constructor
executes in the right VM context."
  (and (cl-cc/vm::%vm-vector-instance-p instance)
       (let ((class (aref instance 0)))
         (and (hash-table-p class)
              (cl-cc/vm::%vm-slot-vector-index class name-sym)
              t))))

(defun %php-generator-p (x) (php-generator-p x))

(defvar *current-generator* nil
  "Dynamically bound to the collecting generator during host-only evaluation
(e.g. %php-make-generator). Compiled PHP generators instead track the active
generator on a VM-global stack so the bridged %php-yield can find it across the
host/VM boundary — see %php-generator-enter / %php-generator-active.")

(defun %php-make-generator (body-thunk)
  "Create a PHP generator by eagerly running BODY-THUNK and collecting yields.
Host-only path (BODY-THUNK is a Common Lisp function); compiled PHP uses the
%php-generator-enter / %php-generator-exit threading instead."
  (let* ((gen (make-php-generator :values nil :return-value +php-null+ :done-p nil)))
    ;; Run the body, collecting all yielded values
    (let ((*current-generator* gen))
      (handler-case
          (let ((result (funcall body-thunk)))
            (setf (php-gen-return-value gen) result))
        (error (e) (declare (ignore e)))))
    ;; Reverse so we can pop from the front (nreverse gives FIFO order)
    (setf (php-gen-values gen) (nreverse (php-gen-values gen)))
    gen))

(defun %php-generator-stack ()
  "Return the active-generator stack stored on the current VM state's globals.
The stack lets the bridged %php-yield locate the generator being built while the
generator's body executes inside the VM. Returns NIL when no VM state is active."
  (when cl-cc/vm:*vm-current-state*
    (gethash '%php-generator-active-stack
             (cl-cc/vm:vm-global-vars cl-cc/vm:*vm-current-state*))))

(defun (setf %php-generator-stack) (new)
  (when cl-cc/vm:*vm-current-state*
    (setf (gethash '%php-generator-active-stack
                   (cl-cc/vm:vm-global-vars cl-cc/vm:*vm-current-state*))
          new)))

(defun %php-generator-active ()
  "Return the generator currently being built, or NIL.
Prefers the host dynamic binding, then the VM-global stack top."
  (or *current-generator*
      (car (%php-generator-stack))))

(defun %php-generator-enter ()
  "Create a fresh generator, push it on the VM-global active stack, and return
it. Pairs with %php-generator-exit. Threading the generator as a value keeps the
host/VM boundary value-in/value-out (no host->VM callback)."
  (let ((gen (make-php-generator :values nil :return-value +php-null+ :done-p nil)))
    (push gen (%php-generator-stack))
    gen))

(defun %php-generator-exit (gen return-value)
  "Pop GEN off the active stack, finalize its value queue (FIFO) and RETURN-VALUE,
and return GEN. RETURN-VALUE is the PHP function body's value (its `return')."
  (setf (%php-generator-stack) (cdr (%php-generator-stack)))
  (setf (php-gen-values gen) (nreverse (php-gen-values gen)))
  (setf (php-gen-return-value gen) return-value)
  gen)

(defun %php-yield (&optional (value +php-null+))
  "Collect VALUE into the current generator's value queue. Returns null (the
eager model has no `send'). When no generator is active (pure host call with no
VM state), returns a (:yield VALUE) marker for inspection."
  (let ((gen (%php-generator-active)))
    (if gen
        (progn
          (push value (php-gen-values gen))
          +php-null+)
        (list :yield value))))

(defun %php-yield-from (iterable)
  "PHP yield from — delegate to another generator/iterable."
  (let ((gen (%php-generator-active)))
    (if gen
        (if (php-generator-p iterable)
            ;; Collect remaining values from inner generator
            (loop (let ((next (%php-generator-next iterable)))
                    (when (eq next +php-null+) (return (php-gen-return-value iterable)))
                    (%php-yield next)))
            ;; Yield each element of an iterable
            (progn
              (when (hash-table-p iterable)
                (dolist (pair (%php-array-pairs iterable))
                  (%php-yield (cdr pair))))
              +php-null+))
        (list :yield-from iterable))))

(defun %php-generator-next (gen &optional (send-value +php-null+))
  "Pop the next value from GEN's queue. Returns null when done."
  (declare (ignore send-value))
  (cond
    ((php-gen-done-p gen) +php-null+)
    ((null (php-gen-values gen))
     (setf (php-gen-done-p gen) t)
     +php-null+)
    (t (pop (php-gen-values gen)))))

(defun %php-generator-send (gen value)
  "In eager model, send is equivalent to next (values are pre-computed)."
  (%php-generator-next gen value))

(defun %php-generator-valid (gen)
  "True if generator GEN has more values."
  (and (not (php-gen-done-p gen)) (not (null (php-gen-values gen)))))

(defun %php-generator-current (gen)
  "Return the next value without consuming it."
  (car (php-gen-values gen)))

(defun %php-generator-get-return (gen)
  "Return the generator's final return value."
  (php-gen-return-value gen))

(defun %php-generator-drain-values (gen)
  "Return GEN's remaining values as a CL list (consumes the generator)."
  (loop while (%php-generator-valid gen)
        collect (%php-generator-next gen)))

(defun %php-foreach-values (iterable)
  "Normalize any PHP ITERABLE into a CL list of VALUES for `foreach ($x as $v)'.
Accepts PHP arrays (hash-tables), generators, and CL lists. foreach lowering
binds its loop list to the result, so this is the single seam that makes foreach
work uniformly over every PHP iterable."
  (cond
    ((php-generator-p iterable) (%php-generator-drain-values iterable))
    ((hash-table-p iterable)    (%php-array-values-list iterable))
    ((listp iterable)           iterable)
    (t (error "PHP foreach: value is not iterable: ~S" iterable))))

(defun %php-foreach-pairs (iterable)
  "Normalize any PHP ITERABLE into a CL list of (KEY . VALUE) pairs for
`foreach ($x as $k => $v)'. PHP arrays keep their keys; generators and plain
lists get sequential integer keys 0,1,2,..."
  (cond
    ((php-generator-p iterable)
     (loop for v in (%php-generator-drain-values iterable)
           for i from 0 collect (cons i v)))
    ((hash-table-p iterable) (%php-array-pairs iterable))
    ((listp iterable)
     (loop for v in iterable for i from 0 collect (cons i v)))
    (t (error "PHP foreach: value is not iterable: ~S" iterable))))

(defun %php-throw (class-name value)
  "Signal VALUE as a PHP exception with CLASS-NAME metadata."
  (error 'php-exception :class-name class-name :value value))

(defun %php-make-exception (class-name value)
  "Return a lightweight PHP exception payload for VM catch/throw lowering."
  (list :php-exception class-name value))

(defun %php-exception-object-p (value)
  "Return true when VALUE is a PHP exception payload or condition."
  (or (typep value 'php-exception)
      (and (consp value) (eq (first value) :php-exception))))

(defun %php-exception-class (value)
  "Return VALUE's PHP exception class metadata when present."
  (cond ((typep value 'php-exception) (php-exception-class-name value))
        ((and (consp value) (eq (first value) :php-exception)) (second value))
        (t nil)))

(defun %php-exception-value (value)
  "Return the thrown PHP exception value from VALUE, preserving non-payloads."
  (cond ((typep value 'php-exception) (php-exception-value value))
        ((and (consp value) (eq (first value) :php-exception)) (third value))
        (t value)))

(defun %php-exception-matches-p (condition class-name)
  "Return true when PHP exception CONDITION matches CLASS-NAME."
  (and (%php-exception-object-p condition)
       (cond ((listp class-name)
              (some (lambda (name) (%php-exception-matches-p condition name)) class-name))
             (t
              (let ((actual (%php-exception-class condition)))
                (or (null class-name)
                    (eq class-name 'php-exception)
                    (eq class-name actual)
                    (and actual
                         (string= (symbol-name class-name) (symbol-name actual)))))))))

(defun %php-truthy (value)
  "Return VALUE interpreted according to PHP truthiness rules."
  (cond ((%php-null-p value) nil)
        ((null value) nil)
        ((and (integerp value) (zerop value)) nil)
        ((and (floatp value) (zerop value)) nil)
        ((and (stringp value) (or (string= value "") (string= value "0"))) nil)
        ((and (hash-table-p value) (%php-array-empty-p value)) nil)
        (t t)))

(defun %php-numeric (x)
  "PHP numeric coercion for an arithmetic operand: number -> itself, null -> 0,
true -> 1, false -> 0, string -> its leading numeric value (%php-to-number).
Without this, PHP +,-,* lowered to raw CL arithmetic and errored on any non-
number operand (null + 3, '5' + 3, true + 1 all signalled `not of type NUMBER')."
  (cond ((numberp x) x)
        ((%php-null-p x) 0)
        ((eq x t) 1)
        ((null x) 0)
        ((stringp x) (%php-to-number x))
        (t 0)))

(defun %php-array-union (a b)
  "PHP array + : a new array with all of A's entries (keys preserved), plus B's
entries whose keys are not already present in A — the LEFT operand wins on key
conflicts.  Unlike array_merge, integer keys are NOT reindexed."
  (let ((result (%php-array))
        (a-keys (gethash +php-array-order-key+ a)))
    (dolist (pair (%php-array-pairs a))
      (%php-array-set result (car pair) (cdr pair)))
    (dolist (pair (%php-array-pairs b))
      (unless (member (car pair) a-keys :test #'equal)
        (%php-array-set result (car pair) (cdr pair))))
    result))

(defun %php-add (a b)
  "PHP + : array UNION when both operands are arrays (left keys win, keys
preserved), otherwise numeric addition with operand coercion."
  (if (and (hash-table-p a) (hash-table-p b))
      (%php-array-union a b)
      (+ (%php-numeric a) (%php-numeric b))))

(defun %php-sub (a b) "PHP - with operand coercion." (- (%php-numeric a) (%php-numeric b)))
(defun %php-mul (a b) "PHP * with operand coercion." (* (%php-numeric a) (%php-numeric b)))

(defun %php-div (a b)
  "PHP / : returns a float, EXCEPT when both operands are integers and evenly
divisible (then an integer).  Was the CL / operator, which yielded an exact
rational (10 / 4 -> 5/2, printed \"5/2\")."
  (let ((na (%php-numeric a))
        (nb (%php-numeric b)))
    (if (and (integerp na) (integerp nb) (not (zerop nb)) (zerop (mod na nb)))
        (truncate na nb)
        (/ (coerce na 'double-float) (coerce nb 'double-float)))))

(defun %php-to-number (s)
  "Coerce PHP string S to a number for loose comparisons."
  (check-type s string)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
    (cond ((string= trimmed "") 0)
          ((find-if (lambda (ch) (member ch '(#\. #\e #\E))) trimmed)
           (handler-case
               (let ((value (read-from-string trimmed)))
                 (if (numberp value) value 0))
             (error () 0)))
          (t
           (handler-case
               (or (parse-integer trimmed :junk-allowed t) 0)
             (error () 0))))))

(defun %php-eq-strict (a b)
  "Return true when A and B are equal according to PHP === semantics."
  (let ((type-a (%php-value-type a))
        (type-b (%php-value-type b)))
    (and (eq type-a type-b)
         (case type-a
           (:null t)
           (:bool (eq a b))
           ((:int :float :string) (equal a b))
           (:array (eq a b))
           (otherwise (equal a b))))))

(defun %php-eq-loose (a b)
  "Return true when A and B are equal according to PHP == semantics."
  (let ((type-a (%php-value-type a))
        (type-b (%php-value-type b)))
    (cond ((and (eq type-a :bool) (eq type-b :bool))
           (eq a b))
          ((or (eq type-a :bool) (eq type-b :bool)
               (eq type-a :null) (eq type-b :null))
           (eq (%php-truthy a) (%php-truthy b)))
          ((and (member type-a '(:int :float)) (member type-b '(:int :float)))
           (= a b))
          ((and (eq type-a :string) (member type-b '(:int :float)))
           (= (%php-to-number a) b))
          ((and (member type-a '(:int :float)) (eq type-b :string))
           (= a (%php-to-number b)))
          ((and (eq type-a :string) (eq type-b :string))
           (if (or (string= a "") (string= b ""))
               (= (%php-to-number a) (%php-to-number b))
               (%php-eq-strict a b)))
          (t (%php-eq-strict a b)))))

(defun %php-neq-loose (a b)
  "PHP != / <> : the negation of loose equality."
  (not (%php-eq-loose a b)))

(defun %php-neq-strict (a b)
  "PHP !== : the negation of strict equality."
  (not (%php-eq-strict a b)))

(defun %php-make-array ()
  "Create an empty PHP ordered array."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash +php-array-order-key+ ht) nil
          (gethash +php-array-next-index-key+ ht) 0)
    ht))

(defun %php-array-key-present-p (array key)
  "Return true when ARRAY already contains PHP array KEY."
  (nth-value 1 (gethash key array)))

(defun %php-array-append-order-key (array key)
  "Record KEY at the end of ARRAY's insertion-order list."
  (let ((order (gethash +php-array-order-key+ array)))
    (setf (gethash +php-array-order-key+ array) (append order (list key)))))

(defun %php-array-advance-next-index (array key)
  "Advance ARRAY's next auto index if KEY is a non-negative integer."
  (when (and (integerp key) (>= key 0))
    (let ((candidate (1+ key))
          (next-index (gethash +php-array-next-index-key+ array)))
      (when (> candidate next-index)
        (setf (gethash +php-array-next-index-key+ array) candidate)))))

(defun %php-array-set (arr key value)
  "Set ARR[KEY] to VALUE, preserving PHP insertion order.

Duplicate keys overwrite their value without changing their original position.
Integer keys greater than or equal to the current auto-index advance the next
auto-increment index to one greater than the key."
  (check-type arr hash-table)
  (unless (%php-array-key-present-p arr key)
    (%php-array-append-order-key arr key))
  (setf (gethash key arr) value)
  (%php-array-advance-next-index arr key)
  value)

(defun %php-array-ref (arr key)
  "Return ARR[KEY] for a PHP ordered array helper hash-table."
  (check-type arr hash-table)
  (multiple-value-bind (value present-p) (gethash key arr)
    (if present-p value +php-null+)))

(defun %php-array-unset (arr key)
  "Delete ARR[KEY] from a PHP ordered array and preserve insertion order."
  (check-type arr hash-table)
  (remhash key arr)
  (setf (gethash +php-array-order-key+ arr)
        (remove key (gethash +php-array-order-key+ arr) :test #'equal))
  +php-null+)

(defun %php-count (arr)
  "Return the number of entries in PHP ordered array ARR."
  (check-type arr hash-table)
  (length (gethash +php-array-order-key+ arr)))

(defun %php-strlen (s)
  "Return the length of string S."
  (check-type s string)
  (length s))

(defun %php-strtolower (s)
  "Return S converted to lowercase."
  (check-type s string)
  (string-downcase s))

(defun %php-strtoupper (s)
  "Return S converted to uppercase."
  (check-type s string)
  (string-upcase s))

(defun %php-number-to-string (n)
  "Format a PHP number the way echo / string interpolation does: integers as-is,
floats (and the rationals PHP / can produce) with up to 14 significant digits,
trailing zeros trimmed, no exponent marker — and a whole-valued float prints as
an integer (3.0 -> \"3\").  princ-to-string leaked the CL form: \"1.5d0\", \"5/2\"."
  (cond
    ((integerp n) (princ-to-string n))
    (t
     (let ((d (coerce n 'double-float)))
       (cond
         ;; whole-valued within the integer-printable range
         ((and (= d (ftruncate d)) (< (abs d) 1d15))
          (princ-to-string (truncate d)))
         (t
          ;; 14 fractional digits then trim trailing zeros (whole case handled
          ;; above, so a fractional digit always remains).
          (let* ((s (format nil "~,14F" d))
                 (dot (position #\. s)))
            (if dot
                (let ((end (1+ (position-if (lambda (c) (char/= c #\0)) s :from-end t))))
                  (subseq s 0 (max end (+ dot 2))))
                s))))))))

(defun %php-stringify (value)
  "Convert VALUE to PHP's simple string representation for interpolation."
  (cond ((%php-null-p value) "")
        ((null value) "")
        ((eq value t) "1")
        ((stringp value) value)
        ((numberp value) (%php-number-to-string value))
        ((hash-table-p value) "Array")
        (t (princ-to-string value))))

(defun %php-concat (&rest values)
  "Concatenate VALUES after PHP-style string conversion."
  (apply #'concatenate 'string (mapcar #'%php-stringify values)))

(defun %php-to-integer (value)
  "Coerce VALUE to an integer for PHP arithmetic/bitwise helpers."
  (cond ((integerp value) value)
        ((floatp value) (truncate value))
        ((stringp value) (truncate (%php-to-number value)))
        ((%php-null-p value) 0)
        ((null value) 0)
        ((eq value t) 1)
        (t (truncate value))))

(defun %php-modulo (a b)
  "Return A % B using PHP-style integer truncation toward zero."
  (rem (%php-to-integer a) (%php-to-integer b)))

(defun %php-shift-left (a b)
  "Return A shifted left by B bits."
  (ash (%php-to-integer a) (%php-to-integer b)))

(defun %php-shift-right (a b)
  "Return A shifted right by B bits, preserving sign for negative integers."
  (ash (%php-to-integer a) (- (%php-to-integer b))))

(defun %php-spaceship (a b)
  "Return -1, 0, or 1 according to PHP <=> comparison ordering."
  (cond ((%php-eq-loose a b) 0)
        ((and (numberp a) (numberp b)) (if (< a b) -1 1))
        ((and (stringp a) (stringp b)) (cond ((string< a b) -1)
                                             ((string> a b) 1)
                                             (t 0)))
        ((and (stringp a) (numberp b)) (if (< (%php-to-number a) b) -1 1))
        ((and (numberp a) (stringp b)) (if (< a (%php-to-number b)) -1 1))
        (t (if (string< (%php-stringify a) (%php-stringify b)) -1 1))))

;;; Relational operators return a PHP boolean (t / nil), NOT 1 / 0.  They lower
;;; through these helpers rather than a plain ast-binop (CL <,>) because the VM
;;; comparison yields an INTEGER 1/0, so `5 > 3' had type integer — gettype(5>3)
;;; was "integer", (5>3) === true was false, and match(true){$x>3=>…} never
;;; matched.  Deriving from %php-spaceship also gives correct PHP comparison
;;; semantics (numeric strings, type juggling) for free.
(defun %php-lt (a b) "PHP a < b."  (and (< (%php-spaceship a b) 0) t))
(defun %php-gt (a b) "PHP a > b."  (and (> (%php-spaceship a b) 0) t))
(defun %php-le (a b) "PHP a <= b." (and (<= (%php-spaceship a b) 0) t))
(defun %php-ge (a b) "PHP a >= b." (and (>= (%php-spaceship a b) 0) t))

(defun %php-bitwise-and (a b)
  "Return PHP bitwise AND for integer-coerced operands."
  (logand (%php-to-integer a) (%php-to-integer b)))

(defun %php-bitwise-or (a b)
  "Return PHP bitwise OR for integer-coerced operands."
  (logior (%php-to-integer a) (%php-to-integer b)))

(defun %php-bitwise-xor (a b)
  "Return PHP bitwise XOR for integer-coerced operands."
  (logxor (%php-to-integer a) (%php-to-integer b)))

(defun %php-bitwise-not (a)
  "Return PHP bitwise NOT for an integer-coerced operand."
  (lognot (%php-to-integer a)))

(defun %php-isset (var)
  "Return true when symbol VAR is bound."
  (check-type var symbol)
  (boundp var))

(defun %php-array-key-exists (arr key)
  "Return true when KEY exists in PHP ordered array ARR."
  (check-type arr hash-table)
  (member key (gethash +php-array-order-key+ arr) :test #'equal))

(defun %php-array-next-auto-index (array)
  "Return and reserve ARRAY's current PHP auto-increment index."
  (let ((index (gethash +php-array-next-index-key+ array)))
    (setf (gethash +php-array-next-index-key+ array) (1+ index))
    index))

(defun %php-spread (array)
  "Wrap ARRAY in a spread marker for %php-array to splice in.  Lowered from
[...$a] inside an array literal.  (In a CALL, ...$a is rewritten before runtime,
so this is only reached for array-literal spreads.)"
  (cons :__php-spread__ array))

(defun %php-spread-marker-p (x)
  "True when X is a %php-spread marker."
  (and (consp x) (eq (car x) :__php-spread__)))

(defun %php-array (&rest entries)
  "Construct a PHP ordered array from flat entry descriptors.

Each entry descriptor is a list of the form (KEY-PRESENT-P KEY VALUE). When
KEY-PRESENT-P is false, KEY is ignored and VALUE is inserted at the current
auto-increment integer index. Explicit integer keys update the next auto-index
to max(existing-next-index, key + 1), matching PHP array literal semantics.
A VALUE that is a %php-spread marker ([...$a]) splices the wrapped array's
elements in — integer keys re-indexed, string keys preserved (PHP 8.1)."
  (let ((array (make-hash-table :test #'equal)))
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (dolist (entry entries array)
      (destructuring-bind (key-present-p key value) entry
        (cond
          ((%php-spread-marker-p value)
           (let ((src (cdr value)))
             (when (hash-table-p src)
               (dolist (k (gethash +php-array-order-key+ src))
                 (let ((v (gethash k src)))
                   (if (integerp k)
                       (%php-array-set array (%php-array-next-auto-index array) v)
                       (%php-array-set array k v)))))))
          (t
           (%php-array-set array
                           (if key-present-p key (%php-array-next-auto-index array))
                           value)))))))

(defun %php-enum-make-case (enum-name case-name value)
  "Create a PHP enum case singleton payload.  `name' and `value' are stored under
STRING keys so $case->name / $case->value resolve through the slot-read string-
key fallback (a symbol key would require the runtime-interned property symbol to
land in the same package, which it does not reliably)."
  ;; :test #'equal — the slot-read string-key fallback looks up "name"/"value"
  ;; with a freshly built string, which is not EQ to the stored key; equal makes
  ;; both the string property keys and the keyword internal keys resolve.
  (let ((case (make-hash-table :test #'equal)))
    (setf (gethash :__php-enum-case__ case) t
          (gethash :__enum-name__ case) enum-name
          (gethash :__case-name__ case) case-name
          (gethash "value" case) value
          (gethash "name" case) (symbol-name case-name))
    case))

(defun %php-enum-case-p (value)
  "Return true when VALUE is a PHP enum case payload."
  (and (hash-table-p value) (gethash :__php-enum-case__ value)))

(defun %php-enum-finalize (enum-class)
  "Link each of ENUM-CLASS's case singletons to the class via :__class__ so that
$case->method() dispatches through the class.  (Enum methods are stored class-
allocated, so slot-read on a case whose __class__ is the enum class resolves
them.)  Returns ENUM-CLASS."
  (when (hash-table-p enum-class)
    (dolist (case (%php-enum-case-list enum-class))
      (setf (gethash :__class__ case) enum-class)))
  enum-class)

(defun %php-enum-case-list (enum-class)
  "Return ENUM-CLASS's case singleton payloads as a CL list (insertion order)."
  (check-type enum-class hash-table)
  (loop for slot-name in (gethash :__class-slots__ enum-class)
        for slot-value = (gethash slot-name enum-class)
        when (%php-enum-case-p slot-value)
          collect slot-value))

(defun %php-enum-cases (enum-class)
  "Return ENUM::cases() — a PHP ARRAY of the case singletons (so count(), foreach,
etc. work). The previous CL list broke count()/array builtins."
  (%php-list-to-array (%php-enum-case-list enum-class)))

(defun %php-enum-case-value (enum-case)
  "Return ENUM-CASE's backed value, or PHP null for unit cases."
  (check-type enum-case hash-table)
  (gethash "value" enum-case +php-null+))

(defun %php-enum-try-from (enum-class value)
  "Return the backed enum case from ENUM-CLASS matching VALUE, or PHP null."
  (or (find value (%php-enum-case-list enum-class)
            :key #'%php-enum-case-value
            :test #'%php-eq-strict)
      +php-null+))

(defun %php-enum-from (enum-class value)
  "Return the backed enum case from ENUM-CLASS matching VALUE, or signal a PHP ValueError."
  (let ((case (%php-enum-try-from enum-class value)))
    (if (%php-null-p case)
        (%php-throw 'value-error (format nil "No enum case for value ~S" value))
        case)))

(defparameter *php-builtin-map*
  `(("count" . ,#'%php-count)
    ("strlen" . ,#'%php-strlen)
    ("strtolower" . ,#'%php-strtolower)
    ("strtoupper" . ,#'%php-strtoupper)
    ("isset" . ,#'%php-isset)
    ("array_key_exists" . ,#'%php-array-key-exists))
  "Map PHP builtin names to runtime helper functions.")
