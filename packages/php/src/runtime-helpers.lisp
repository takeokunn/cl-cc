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

(defparameter *php-builtin-map*
  `(("count" . ,#'%php-count)
    ("strlen" . ,#'%php-strlen)
    ("strtolower" . ,#'%php-strtolower)
    ("strtoupper" . ,#'%php-strtoupper)
    ("isset" . ,#'%php-isset)
    ("array_key_exists" . ,#'%php-array-key-exists))
  "Map PHP builtin names to runtime helper functions.")
