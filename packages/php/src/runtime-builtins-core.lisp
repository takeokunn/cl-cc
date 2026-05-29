;;;; Core PHP builtin helpers.

(in-package :cl-cc/php)

(defun %php-empty (value)
  "Return true when VALUE is empty according to PHP truthiness rules."
  ;; (%php-empty +php-null+) => T
  ;; (%php-empty "0") => T
  ;; (%php-empty "hello") => NIL
  (not (%php-truthy value)))

(defun %php-is-null (value)
  "Return true when VALUE is PHP null."
  ;; (%php-is-null +php-null+) => T
  ;; (%php-is-null NIL) => NIL
  (%php-null-p value))

(defun %php-unset (value)
  "Return PHP null for unset VALUE."
  ;; The parser lowers array element unsets through `%php-array-unset`; this
  ;; helper exists for builtin registry completeness where an expression-like
  ;; bridge is needed.
  ;; (%php-unset 10) => +PHP-NULL+
  (declare (ignore value))
  +php-null+)

(defun %php-gettype (value)
  "Return VALUE's PHP type name as a string."
  ;; (%php-gettype 1) => "integer"
  ;; (%php-gettype "x") => "string"
  (case (%php-value-type value)
    (:null "NULL")
    (:bool "boolean")
    (:int "integer")
    (:float "double")
    (:string "string")
    (:array "array")
    (otherwise "object")))

(defun %php-array-ordered-keys (array)
  "Return ARRAY's user keys in PHP insertion order."
  ;; (%php-array-ordered-keys (%php-array (list nil nil "a"))) => (0)
  (check-type array hash-table)
  (copy-list (gethash +php-array-order-key+ array)))

(defun %php-array-pairs (array)
  "Return ARRAY's ordered (KEY . VALUE) pairs."
  ;; (%php-array-pairs (%php-array (list t "a" 1))) => (("a" . 1))
  (check-type array hash-table)
  (loop for key in (%php-array-ordered-keys array)
        collect (cons key (gethash key array))))

(defun %php-array-values-list (array)
  "Return ARRAY's values as a Common Lisp list in PHP insertion order."
  ;; (%php-array-values-list (%php-array (list nil nil "a") (list nil nil "b")))
  ;; => ("a" "b")
  (check-type array hash-table)
  (loop for key in (%php-array-ordered-keys array)
        collect (gethash key array)))

(defun %php-list-to-array (values)
  "Return a PHP ordered array containing VALUES at numeric keys."
  ;; (%php-count (%php-list-to-array '("a" "b"))) => 2
  (let ((array (%php-array)))
    (dolist (value values array)
      (%php-array-set array (%php-array-next-auto-index array) value))))

(defun %php-value-display-string (value)
  "Return a readable PHP-like representation of VALUE."
  (cond ((%php-null-p value) "NULL")
        ((eq value t) "true")
        ((null value) "false")
        ((stringp value) value)
        ((numberp value) (princ-to-string value))
        ((hash-table-p value)
         (with-output-to-string (stream)
           (format stream "Array(~D) {" (%php-count value))
           (dolist (pair (%php-array-pairs value))
             (format stream " [~A]=>~A"
                     (%php-stringify (car pair))
                     (%php-value-display-string (cdr pair))))
           (format stream " }")))
        (t (princ-to-string value))))

(defun %php-print-r (value &optional return-output)
  "Return or print a PHP-like readable representation of VALUE."
  ;; (%php-print-r (%php-array (list t "a" 1)) T) => "Array(1) { [a]=>1 }"
  (let ((text (%php-value-display-string value)))
    (if (%php-truthy return-output)
        text
        (progn (princ text) t))))

(defun %php-var-dump (&rest values)
  "Return a basic var_dump-style string for VALUES."
  ;; (%php-var-dump 1 "x") => "int(1)\nstring(1) \"x\"\n"
  (with-output-to-string (stream)
    (dolist (value values)
      (case (%php-value-type value)
        (:null (format stream "NULL~%"))
        (:bool (format stream "bool(~A)~%" (if value "true" "false")))
        (:int (format stream "int(~D)~%" value))
        (:float (format stream "float(~A)~%" value))
        (:string (format stream "string(~D) \"~A\"~%" (length value) value))
        (:array (format stream "array(~D) ~A~%" (%php-count value)
                        (%php-value-display-string value)))
        (otherwise (format stream "object(~A)~%" (%php-value-display-string value)))))))
