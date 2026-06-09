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

(defun %php-var-dump-to-stream (value stream indent)
  "Write one var_dump entry for VALUE to STREAM at INDENT spaces (recursive for
arrays).  Floats use the PHP number format, not the raw CL form."
  (let ((pad (make-string indent :initial-element #\Space)))
    (case (%php-value-type value)
      (:null   (format stream "~ANULL~%" pad))
      (:bool   (format stream "~Abool(~A)~%" pad (if value "true" "false")))
      (:int    (format stream "~Aint(~D)~%" pad value))
      (:float  (format stream "~Afloat(~A)~%" pad (%php-number-to-string value)))
      (:string (format stream "~Astring(~D) \"~A\"~%" pad (length value) value))
      (:array
       (format stream "~Aarray(~D) {~%" pad (%php-count value))
       (dolist (pair (%php-array-pairs value))
         (let ((k (car pair)))
           (if (integerp k)
               (format stream "~A  [~D]=>~%" pad k)
               (format stream "~A  [\"~A\"]=>~%" pad k)))
         (%php-var-dump-to-stream (cdr pair) stream (+ indent 2)))
       (format stream "~A}~%" pad))
      (otherwise (format stream "~Aobject(~A)~%" pad (%php-value-display-string value))))))

(defun %php-var-dump (&rest values)
  "PHP var_dump: WRITE a type-annotated dump of each value to output (it prints;
it does not return a string)."
  (dolist (value values)
    (write-string (with-output-to-string (s)
                    (%php-var-dump-to-stream value s 0))))
  nil)

(defun %php-var-export-string (value indent)
  "Build PHP var_export output for VALUE (a re-parseable representation):
ints/floats bare, strings single-quoted (with ' and \\ escaped), bools
true/false, null NULL, arrays as `array ( k => v, ... )'."
  (let ((pad (make-string indent :initial-element #\Space)))
    (case (%php-value-type value)
      (:null   "NULL")
      (:bool   (if value "true" "false"))
      (:int    (princ-to-string value))
      (:float  (%php-number-to-string value))
      (:string (with-output-to-string (s)
                 (write-char #\' s)
                 (loop for ch across value
                       do (when (or (char= ch #\') (char= ch #\\)) (write-char #\\ s))
                          (write-char ch s))
                 (write-char #\' s)))
      (:array
       (with-output-to-string (s)
         (format s "array (~%")
         (dolist (pair (%php-array-pairs value))
           (let ((k (car pair)))
             (format s "~A  ~A => ~A,~%" pad
                     (if (integerp k) (princ-to-string k)
                         (%php-var-export-string k 0))
                     (%php-var-export-string (cdr pair) (+ indent 2)))))
         (format s "~A)" pad)))
      (otherwise (%php-value-display-string value)))))

(defun %php-var-export (value &optional return-p)
  "PHP var_export: print VALUE's re-parseable representation, or return it as a
string when RETURN-P is truthy."
  (let ((s (%php-var-export-string value 0)))
    (if (%php-truthy return-p) s (progn (write-string s) nil))))
