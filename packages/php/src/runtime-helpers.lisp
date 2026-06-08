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

(defun %php-array-empty-p (ht)
  "Return true when PHP ordered array HT contains no user entries."
  (check-type ht hash-table)
  (null (gethash +php-array-order-key+ ht)))

(defun %php-value-type (x)
  "Return the PHP runtime value type keyword for X."
  (cond ((%php-null-p x) :null)
        ((null x) :bool)
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

(defun %php-yield (&optional value)
  "Return a runtime representation for a PHP yield point."
  (list :yield value))

(defun %php-yield-from (iterable)
  "Return a runtime representation for PHP yield-from delegation."
  (list :yield-from iterable))

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

(defun %php-stringify (value)
  "Convert VALUE to PHP's simple string representation for interpolation."
  (cond ((%php-null-p value) "")
        ((null value) "")
        ((eq value t) "1")
        ((stringp value) value)
        ((numberp value) (princ-to-string value))
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

(defun %php-array (&rest entries)
  "Construct a PHP ordered array from flat entry descriptors.

Each entry descriptor is a list of the form (KEY-PRESENT-P KEY VALUE). When
KEY-PRESENT-P is false, KEY is ignored and VALUE is inserted at the current
auto-increment integer index. Explicit integer keys update the next auto-index
to max(existing-next-index, key + 1), matching PHP array literal semantics."
  (let ((array (make-hash-table :test #'equal)))
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (dolist (entry entries array)
      (destructuring-bind (key-present-p key value) entry
         (%php-array-set array
                         (if key-present-p key (%php-array-next-auto-index array))
                         value)))))

(defun %php-enum-make-case (enum-name case-name value)
  "Create a PHP enum case singleton payload."
  (let ((case (make-hash-table :test #'eq)))
    (setf (gethash :__php-enum-case__ case) t
          (gethash :__enum-name__ case) enum-name
          (gethash :__case-name__ case) case-name
          (gethash 'value case) value
          (gethash 'name case) (symbol-name case-name))
    case))

(defun %php-enum-case-p (value)
  "Return true when VALUE is a PHP enum case payload."
  (and (hash-table-p value) (gethash :__php-enum-case__ value)))

(defun %php-enum-cases (enum-class)
  "Return all enum case singleton payloads stored on ENUM-CLASS."
  (check-type enum-class hash-table)
  (loop for slot-name in (gethash :__class-slots__ enum-class)
        for slot-value = (gethash slot-name enum-class)
        when (%php-enum-case-p slot-value)
          collect slot-value))

(defun %php-enum-case-value (enum-case)
  "Return ENUM-CASE's backed value, or PHP null for unit cases."
  (check-type enum-case hash-table)
  (gethash 'value enum-case +php-null+))

(defun %php-enum-try-from (enum-class value)
  "Return the backed enum case from ENUM-CLASS matching VALUE, or PHP null."
  (or (find value (%php-enum-cases enum-class)
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
