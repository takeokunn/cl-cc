;;;; String PHP builtin helpers.

(in-package :cl-cc/php)

(defparameter +php-trim-characters+
  (list #\Space #\Tab #\Newline #\Return #\Null #\Page)
  "Default characters trimmed by PHP trim/ltrim/rtrim.")

(defun %php-char-bag (characters)
  "Return CHARACTERS as a list of characters for trim helpers."
  (cond ((or (null characters) (%php-null-p characters)) +php-trim-characters+)
        ((stringp characters) (coerce characters 'list))
        (t +php-trim-characters+)))

(defun %php-string-search (needle haystack &key (start 0) ignore-case from-end)
  "Search for NEEDLE in HAYSTACK and return the index or NIL."
  (let* ((subject (%php-stringify haystack))
         (query (%php-stringify needle))
         (subject* (if ignore-case (string-downcase subject) subject))
         (query* (if ignore-case (string-downcase query) query)))
    (if from-end
        (loop with pos = nil
              for next = (search query* subject* :start2 start)
                then (and next (search query* subject* :start2 (1+ next)))
              while next
              do (setf pos next)
              finally (return pos))
        (search query* subject* :start2 start))))

(defun %php-replace-string (search replace subject &key ignore-case)
  "Return SUBJECT with all SEARCH occurrences replaced by REPLACE."
  (let* ((search (%php-stringify search))
         (replace (%php-stringify replace))
         (subject (%php-stringify subject)))
    (if (string= search "")
        subject
        (with-output-to-string (stream)
          (loop with source = (if ignore-case (string-downcase subject) subject)
                with needle = (if ignore-case (string-downcase search) search)
                with start = 0
                for pos = (search needle source :start2 start)
                while pos
                do (write-string subject stream :start start :end pos)
                   (write-string replace stream)
                   (setf start (+ pos (length search)))
                finally (write-string subject stream :start start))))))

(defun %php-str-replace (search replace subject)
  "Return SUBJECT after case-sensitive replacement of SEARCH with REPLACE."
  ;; (%php-str-replace "world" "PHP" "hello world") => "hello PHP"
  (%php-replace-string search replace subject))

(defun %php-str-ireplace (search replace subject)
  "Return SUBJECT after case-insensitive replacement of SEARCH with REPLACE."
  ;; (%php-str-ireplace "WORLD" "PHP" "hello world") => "hello PHP"
  (%php-replace-string search replace subject :ignore-case t))

(defun %php-substr (string start &optional length)
  "Return a PHP substr-style slice of STRING."
  ;; (%php-substr "abcdef" 1 3) => "bcd"
  ;; (%php-substr "abcdef" -2) => "ef"
  (let* ((text (%php-stringify string))
         (size (length text))
         (begin (if (minusp start) (max 0 (+ size start)) (min start size)))
         (end (cond ((or (null length) (%php-null-p length)) size)
                    ((minusp length) (max begin (+ size length)))
                    (t (min size (+ begin length))))))
    (if (>= begin size)
        ""
        (subseq text begin end))))

(defun %php-trim (string &optional characters)
  "Trim whitespace or CHARACTERS from both ends of STRING."
  ;; (%php-trim "  x  ") => "x"
  (string-trim (%php-char-bag characters) (%php-stringify string)))

(defun %php-ltrim (string &optional characters)
  "Trim whitespace or CHARACTERS from the left side of STRING."
  (string-left-trim (%php-char-bag characters) (%php-stringify string)))

(defun %php-rtrim (string &optional characters)
  "Trim whitespace or CHARACTERS from the right side of STRING."
  (string-right-trim (%php-char-bag characters) (%php-stringify string)))

(defun %php-explode (delimiter string)
  "Split STRING on DELIMITER and return a PHP array of pieces."
  ;; (%php-array-values-list (%php-explode "," "a,b")) => ("a" "b")
  (let ((delimiter (%php-stringify delimiter))
        (text (%php-stringify string)))
    (if (string= delimiter "")
        (%php-list-to-array (list text))
        (let ((pieces nil)
              (start 0))
          (loop for pos = (search delimiter text :start2 start)
                while pos
                do (push (subseq text start pos) pieces)
                   (setf start (+ pos (length delimiter)))
                finally (push (subseq text start) pieces))
          (%php-list-to-array (nreverse pieces))))))

(defun %php-implode (glue pieces &optional maybe-pieces)
  "Join PIECES with GLUE. Also supports PHP's implode(pieces) form."
  ;; (%php-implode "," (%php-list-to-array '("a" "b"))) => "a,b"
  (let* ((actual-glue (if maybe-pieces (%php-stringify glue)
                          (if (hash-table-p glue) "" (%php-stringify glue))))
         (actual-pieces (or maybe-pieces (if (hash-table-p glue) glue pieces)))
         (strings (cond ((hash-table-p actual-pieces)
                         (mapcar #'%php-stringify (%php-array-values-list actual-pieces)))
                        ((listp actual-pieces)
                         (mapcar #'%php-stringify actual-pieces))
                        (t (list (%php-stringify actual-pieces))))))
    (with-output-to-string (stream)
      (loop for part in strings
            for first-p = t then nil
            unless first-p do (write-string actual-glue stream)
            do (write-string part stream)))))

(defun %php-join (glue pieces &optional maybe-pieces)
  "Alias for `%php-implode`."
  (%php-implode glue pieces maybe-pieces))

(defun %php-strpos (haystack needle &optional (offset 0))
  "Return first position of NEEDLE in HAYSTACK, or NIL when absent."
  ;; (%php-strpos "hello" "ll") => 2
  (%php-string-search needle haystack :start offset))

(defun %php-stripos (haystack needle &optional (offset 0))
  "Return first case-insensitive position of NEEDLE in HAYSTACK, or NIL."
  (%php-string-search needle haystack :start offset :ignore-case t))

(defun %php-strrpos (haystack needle &optional (offset 0))
  "Return last position of NEEDLE in HAYSTACK, or NIL when absent."
  (%php-string-search needle haystack :start offset :from-end t))

(defun %php-str-contains (haystack needle)
  "Return true when HAYSTACK contains NEEDLE."
  ;; (%php-str-contains "abc" "b") => T
  (not (null (%php-string-search needle haystack))))

(defun %php-str-starts-with (haystack needle)
  "Return true when HAYSTACK starts with NEEDLE."
  (let ((text (%php-stringify haystack))
        (prefix (%php-stringify needle)))
    (and (<= (length prefix) (length text))
         (string= prefix text :end2 (length prefix)))))

(defun %php-str-ends-with (haystack needle)
  "Return true when HAYSTACK ends with NEEDLE."
  (let* ((text (%php-stringify haystack))
         (suffix (%php-stringify needle))
         (start (- (length text) (length suffix))))
    (and (>= start 0)
         (string= suffix text :start2 start))))

(defun %php-strrev (string)
  "Return STRING reversed."
  ;; (%php-strrev "abc") => "cba"
  (coerce (reverse (coerce (%php-stringify string) 'list)) 'string))

(defun %php-str-repeat (string multiplier)
  "Return STRING repeated MULTIPLIER times."
  ;; (%php-str-repeat "ab" 3) => "ababab"
  (let ((text (%php-stringify string))
        (count (max 0 multiplier)))
    (with-output-to-string (stream)
      (dotimes (i count)
        (declare (ignore i))
        (write-string text stream)))))

(defun %php-sprintf (format-string &rest args)
  "Return a basic sprintf result supporting %s, %d, %f and %%."
  ;; (%php-sprintf "Hi %s" "PHP") => "Hi PHP"
  (let ((fmt (%php-stringify format-string))
        (remaining args))
    (with-output-to-string (stream)
      (loop for i from 0 below (length fmt)
            for ch = (char fmt i)
            do (if (and (char= ch #\%) (< (1+ i) (length fmt)))
                   (let ((directive (char fmt (incf i))))
                     (case directive
                       (#\% (write-char #\% stream))
                       (#\s (write-string (%php-stringify (pop remaining)) stream))
                       (#\d (format stream "~D" (or (pop remaining) 0)))
                       (#\f (format stream "~F" (or (pop remaining) 0.0)))
                       (otherwise
                        (write-char #\% stream)
                        (write-char directive stream))))
                   (write-char ch stream))))))

(defun %php-htmlspecialchars (string)
  "Escape HTML special characters in STRING."
  ;; (%php-htmlspecialchars "<a&b>") => "&lt;a&amp;b&gt;"
  (let ((text (%php-stringify string)))
    (with-output-to-string (stream)
      (loop for ch across text
            do (case ch
                 (#\& (write-string "&amp;" stream))
                 (#\< (write-string "&lt;" stream))
                 (#\> (write-string "&gt;" stream))
                 (#\" (write-string "&quot;" stream))
                 (#\' (write-string "&#039;" stream))
                 (otherwise (write-char ch stream)))))))
