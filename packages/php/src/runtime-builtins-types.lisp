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

(defun %php-string-to-int-base (s base)
  "Parse string S as an integer in BASE (PHP intval semantics).  BASE 0 detects
the base from a 0x / 0b / 0o / leading-0 prefix.  A leading sign and whitespace
are allowed; trailing junk is ignored."
  (let* ((str (string-left-trim '(#\Space #\Tab #\Newline #\Return) (%php-stringify s)))
         (n (length str)) (i 0) (neg nil) (radix base))
    (when (and (< i n) (member (char str i) '(#\+ #\-)))
      (setf neg (char= (char str i) #\-)) (incf i))
    (flet ((has-prefix (p) (and (<= (+ i (length p)) n)
                                (string-equal str p :start1 i :end1 (+ i (length p))))))
      (cond
        ((and (member base '(0 16)) (has-prefix "0x")) (setf radix 16 i (+ i 2)))
        ((and (member base '(0 2))  (has-prefix "0b")) (setf radix 2  i (+ i 2)))
        ((and (member base '(0 8))  (has-prefix "0o")) (setf radix 8  i (+ i 2)))
        ((and (= base 0) (< i n) (char= (char str i) #\0)) (setf radix 8))   ; leading 0 -> octal
        ((= base 0) (setf radix 10))))
    (let ((val (or (parse-integer str :start i :radix (max 2 radix) :junk-allowed t) 0)))
      (if neg (- val) val))))

(defun %php-intval (value &optional base)
  "Convert VALUE to a PHP integer.  For a string VALUE a numeric BASE (other than
10) parses it in that base — BASE 0 autodetects from a 0x/0b/0o/0 prefix."
  ;; (%php-intval "42") => 42 ; (%php-intval "0x1A" 16) => 26 ; (%php-intval "077" 8) => 63
  (cond ((%php-null-p value) 0)
        ((eq value t) 1)
        ((null value) 0)
        ((integerp value) value)
        ((floatp value) (truncate value))
        ((stringp value)
         (let ((b (and (numberp base) (truncate base))))
           (if (and b (/= b 10))
               (%php-string-to-int-base value b)
               (or (parse-integer (string-trim '(#\Space #\Tab #\Newline #\Return) value)
                                  :junk-allowed t)
                   0))))
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

(defconstant +php-filter-unsafe-raw+ 516)
(defconstant +php-filter-validate-int+ 257)
(defconstant +php-filter-validate-boolean+ 258)
(defconstant +php-filter-validate-float+ 259)
(defconstant +php-filter-validate-url+ 273)
(defconstant +php-filter-validate-email+ 274)
(defconstant +php-filter-null-on-failure+ 134217728)
(defconstant +php-filter-throw-on-failure+ 268435456)

(defun %php-filter-failed-exception-symbol ()
  (intern "FILTER\\FILTERFAILEDEXCEPTION" :cl-cc/php))

(defun %php-filter-int-value (value)
  (cond
    ((and (integerp value) (not (typep value 'boolean))) value)
    ((floatp value)
     (and (= value (truncate value)) (truncate value)))
    ((stringp value)
     (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (and (> (length text) 0)
            (multiple-value-bind (parsed position)
                (parse-integer text :junk-allowed t)
              (and parsed (= position (length text)) parsed)))))
    (t nil)))

(defun %php-filter-boolean-value (value)
  (cond
    ((eq value t) t)
    ((null value) nil)
    ((and (integerp value) (= value 1)) t)
    ((and (integerp value) (= value 0)) nil)
    ((stringp value)
     (let ((text (string-downcase
                  (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
       (cond ((member text '("1" "true" "on" "yes") :test #'string=) t)
             ((member text '("0" "false" "off" "no" "") :test #'string=) nil)
             (t :php-filter-failure))))
    (t :php-filter-failure)))

(defun %php-filter-float-value (value)
  (cond
    ((numberp value) (float value))
    ((stringp value)
     (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) value))
            (*read-eval* nil))
       (and (> (length text) 0)
            (multiple-value-bind (parsed position)
                (ignore-errors (read-from-string text))
              (and (numberp parsed) (= position (length text)) (float parsed))))))
    (t nil)))

(defun %php-filter-email-value (value)
  (when (stringp value)
    (let ((at (position #\@ value)))
      (and at
           (> at 0)
           (< at (1- (length value)))
           (position #\. value :start (1+ at))
           (not (find #\Space value))
           value))))

(defun %php-filter-url-value (value)
  (when (stringp value)
    (let ((lower (string-downcase value)))
      (and (or (uiop:string-prefix-p "http://" lower)
               (uiop:string-prefix-p "https://" lower))
           (> (length value) (length "https://"))
           value))))

(defun %php-filter-failure (flags)
  (cond
    ((not (zerop (logand flags +php-filter-throw-on-failure+)))
     (%php-throw (%php-filter-failed-exception-symbol)
                 "filter_var(): validation failed"))
    ((not (zerop (logand flags +php-filter-null-on-failure+))) +php-null+)
    (t nil)))

(defun %php-filter-var (value &optional (filter +php-filter-unsafe-raw+) (options 0))
  "Filter VALUE using a compact subset of PHP filter_var() semantics."
  (let ((flags (cond ((integerp options) options)
                     ((hash-table-p options)
                      (let ((raw (or (gethash "flags" options)
                                     (gethash :flags options)
                                     0)))
                        (if (integerp raw) raw 0)))
                     (t 0))))
    (when (and (not (zerop (logand flags +php-filter-null-on-failure+)))
               (not (zerop (logand flags +php-filter-throw-on-failure+))))
      (%php-throw 'value-error
                  "filter_var(): Argument #3 ($options) cannot use both FILTER_NULL_ON_FAILURE and FILTER_THROW_ON_FAILURE"))
    (let ((result
            (case filter
              ((516) value)
              ((257) (%php-filter-int-value value))
              ((258) (%php-filter-boolean-value value))
              ((259) (%php-filter-float-value value))
              ((273) (%php-filter-url-value value))
              ((274) (%php-filter-email-value value))
              (otherwise value))))
      (cond
        ((eq result :php-filter-failure)
         (%php-filter-failure flags))
        ((and (null result)
              (not (member filter
                           (list +php-filter-unsafe-raw+
                                 +php-filter-validate-boolean+)
                           :test #'=)))
         (%php-filter-failure flags))
        (t result)))))
