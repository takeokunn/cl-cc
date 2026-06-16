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

(defun %php-compact (&rest names)
  "Fallback for dynamic compact(); static string names are lowered by parser."
  (declare (ignore names))
  (%php-array))

(defun %php-extract (array &rest options)
  "Fallback for dynamic extract(); static array literals are lowered by parser."
  (declare (ignore array options))
  0)

(defun %php-opcache-is-script-cached-in-file-cache (filename)
  "PHP 8.5 opcache_is_script_cached_in_file_cache().

cl-cc does not maintain an OPCache file cache, so scripts are never reported as
cached by this runtime helper."
  (declare (ignore filename))
  nil)

(defvar *php-curl-persistent-share-handles* (make-hash-table :test #'equal)
  "Persistent cURL share handles keyed by user-provided identifier.")

(defun %php-curl-share-init-persistent (&optional (id "default"))
  "PHP 8.5 curl_share_init_persistent() helper.

cl-cc models the persistent share handle as a stable PHP object-like
hash-table. Repeated calls with the same ID return the same handle."
  (let ((key (%php-stringify id)))
    (multiple-value-bind (handle foundp)
        (gethash key *php-curl-persistent-share-handles*)
      (if foundp
          handle
          (let ((new-handle (make-hash-table :test #'equal)))
            (setf (gethash "__class__" new-handle) "CurlSharePersistentHandle"
                  (gethash "id" new-handle) key
                  (gethash "persistent" new-handle) t)
            (setf (gethash key *php-curl-persistent-share-handles*) new-handle)
            new-handle)))))

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

(defvar *php-missing-call-arg* (list :php-missing-call-arg))

(defun %php-call-args-with-named-spread
    (fn-name param-names default-present default-values &rest descriptors)
  "Return a runtime positional argument list for calls that mix ...spread and
named arguments when the spread width is not statically known."
  (let ((positional nil)
        (named (make-hash-table :test #'equal))
        (seen-named nil)
        (highest-named-index -1))
    (labels ((param-index (name)
               (position name param-names :test #'string-equal))
             (push-positional (value)
               (when seen-named
                 (error "PHP positional argument after named argument in call to ~A"
                        fn-name))
               (push value positional)))
      (dolist (descriptor descriptors)
        (destructuring-bind (kind &rest values) descriptor
          (case kind
            (:pos
             (push-positional (first values)))
            (:spread
             (when seen-named
               (error "PHP spread argument after named argument in call to ~A"
                      fn-name))
             (dolist (value (%php-array-values-list (first values)))
               (push value positional)))
            (:named
             (setf seen-named t)
             (destructuring-bind (name value) values
               (let ((index (param-index name)))
                 (unless index
                   (error "Unknown named argument ~A for PHP function ~A"
                          name fn-name))
                 (multiple-value-bind (old present-p) (gethash name named)
                   (declare (ignore old))
                   (when present-p
                     (error "Duplicate named argument ~A for PHP function ~A"
                            name fn-name)))
                 (setf (gethash name named) value
                       highest-named-index (max highest-named-index index)))))
            (otherwise
              (error "Unknown PHP call argument descriptor ~S" kind)))))
      (let* ((positional (nreverse positional))
             (positional-count (length positional))
             (result-size (max positional-count (1+ highest-named-index)))
             (result (make-array result-size
                                 :initial-element *php-missing-call-arg*)))
        (loop for value in positional
              for index from 0
              do (setf (aref result index) value))
        (maphash (lambda (name value)
                   (let ((index (param-index name)))
                     (when (< index positional-count)
                       (error "Duplicate PHP argument for parameter ~A in call to ~A"
                              name fn-name))
                     (setf (aref result index) value)))
                 named)
        (loop for index below result-size
              for value = (aref result index)
              collect (cond
                        ((not (eq value *php-missing-call-arg*)) value)
                        ((nth index default-present)
                         (nth index default-values))
                        (t
                         (error "Missing PHP argument ~A for function ~A"
                                (nth index param-names) fn-name))))))))

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
