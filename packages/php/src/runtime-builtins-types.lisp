;;;; Type checking and conversion PHP builtin helpers.

(in-package :cl-cc/php)

(defun %php-is-int (value)
  "Return true when VALUE is a PHP integer."
  ;; (%php-is-int 1) => T
  (integerp value))

(defun %php-is-integer (value)
  "Alias for `%php-is-int`."
  (%php-is-int value))

(defun %php-is-long (value)
  "Alias for `%php-is-int`."
  (%php-is-int value))

(defun %php-is-float (value)
  "Return true when VALUE is a PHP float."
  (floatp value))

(defun %php-is-double (value)
  "Alias for `%php-is-float`."
  (%php-is-float value))

(defun %php-is-real (value)
  "Alias for `%php-is-float`."
  (%php-is-float value))

(defun %php-is-string (value)
  "Return true when VALUE is a string."
  (stringp value))

(defun %php-is-bool (value)
  "Return true when VALUE is a PHP boolean."
  (or (eq value t) (null value)))

(defun %php-is-array (value)
  "Return true when VALUE is a PHP array."
  (hash-table-p value))

(defun %php-is-object (value)
  "Return true when VALUE is not a scalar, array, or PHP null."
  (not (or (%php-null-p value)
           (%php-is-bool value)
           (numberp value)
           (stringp value)
           (hash-table-p value))))

(defun %php-is-callable (value)
  "Return true when VALUE can be called by builtin callback helpers."
  (or (functionp value)
      (and (symbolp value) (fboundp value))
      (and (stringp value)
           (or (and (fboundp '%php-lookup-builtin)
                    (funcall (symbol-function '%php-lookup-builtin) value))
               (fboundp (intern (string-upcase value) :cl-cc/php))))))

(defun %php-string-numeric-p (string)
  "Return true when STRING is a simple PHP numeric string."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) string)))
    (and (> (length text) 0)
         (multiple-value-bind (value position)
             (let ((*read-eval* nil))
               (ignore-errors (read-from-string text)))
           (and (numberp value)
                (= position (length text)))))))

(defun %php-is-numeric (value)
  "Return true when VALUE is a number or numeric string."
  ;; (%php-is-numeric "1.5") => T
  (or (numberp value)
      (and (stringp value) (%php-string-numeric-p value))))

(defun %php-is-scalar (value)
  "Return true for PHP scalar values: int, float, string, bool."
  (or (numberp value) (stringp value) (%php-is-bool value)))

(defun %php-is-iterable (value)
  "Return true when VALUE is iterable by PHP array helpers."
  (hash-table-p value))

(defun %php-intval (value &optional base)
  "Convert VALUE to a PHP integer."
  ;; (%php-intval "42") => 42
  ;; (%php-intval +php-null+) => 0
  (declare (ignore base))
  (cond ((%php-null-p value) 0)
        ((eq value t) 1)
        ((null value) 0)
        ((integerp value) value)
        ((floatp value) (truncate value))
        ((stringp value) (or (parse-integer (string-trim '(#\Space #\Tab #\Newline #\Return) value)
                                             :junk-allowed t)
                             0))
        (t 1)))

(defun %php-floatval (value)
  "Convert VALUE to a PHP float."
  ;; (%php-floatval "1.5") => 1.5
  (cond ((%php-null-p value) 0.0)
        ((eq value t) 1.0)
        ((null value) 0.0)
        ((numberp value) (float value))
        ((stringp value)
         (let ((*read-eval* nil))
           (multiple-value-bind (parsed position)
               (ignore-errors (read-from-string (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
             (declare (ignore position))
             (if (numberp parsed) (float parsed) 0.0))))
        (t 1.0)))

(defun %php-strval (value)
  "Convert VALUE to a PHP string."
  (%php-stringify value))

(defun %php-boolval (value)
  "Convert VALUE to a PHP boolean."
  (%php-truthy value))
