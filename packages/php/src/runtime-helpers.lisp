;;;; Runtime helpers for PHP lowering.

(in-package :cl-cc/php)

(defconstant +php-array-order-key+ :__php-array-order
  "Reserved hash-table key storing PHP array insertion order.")

(defconstant +php-array-next-index-key+ :__php-array-next-index
  "Reserved hash-table key storing the next PHP auto-increment index.")

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
  (gethash key arr))

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
