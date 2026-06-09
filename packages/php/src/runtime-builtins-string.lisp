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
  "Return a sprintf result supporting %s %d %f %e %o %x %X %b %c %u %% and width/precision."
  ;; (%php-sprintf "Hi %s" "PHP") => "Hi PHP"
  ;; (%php-sprintf "%05d" 42) => "00042"
  (let ((fmt (%php-stringify format-string))
        (remaining args)
        (flen 0))
    (setf flen (length fmt))
    (with-output-to-string (stream)
      (let ((i 0))
        (loop while (< i flen)
              do (let ((ch (char fmt i)))
                   (incf i)
                   (if (char= ch #\%)
                       (progn
                         ;; parse optional argument position: %1$s
                         (let* ((arg-pos nil)
                                (flags "")
                                (width nil)
                                (precision nil)
                                (j i))
                           ;; check for N$ argument position
                           (when (and (< j flen) (digit-char-p (char fmt j)))
                             (let ((numstart j))
                               (loop while (and (< j flen) (digit-char-p (char fmt j))) do (incf j))
                               (when (and (< j flen) (char= (char fmt j) #\$))
                                 (setf arg-pos (1- (parse-integer fmt :start numstart :end j)))
                                 (incf j)
                                 (setf i j))))
                           ;; flags
                           (loop while (and (< i flen) (member (char fmt i) '(#\- #\+ #\Space #\0 #\')))
                                 do (setf flags (concatenate 'string flags (string (char fmt i))))
                                    (incf i))
                           ;; width
                           (when (and (< i flen) (digit-char-p (char fmt i)))
                             (let ((start i))
                               (loop while (and (< i flen) (digit-char-p (char fmt i))) do (incf i))
                               (setf width (parse-integer fmt :start start :end i))))
                           ;; precision
                           (when (and (< i flen) (char= (char fmt i) #\.))
                             (incf i)
                             (let ((start i))
                               (loop while (and (< i flen) (digit-char-p (char fmt i))) do (incf i))
                               (setf precision (if (= start i) 0 (parse-integer fmt :start start :end i)))))
                           ;; directive
                           (when (< i flen)
                             (let* ((dir (char fmt i))
                                    (val (if arg-pos
                                             (nth arg-pos args)
                                             (pop remaining)))
                                    (left-align (position #\- flags))
                                    (zero-pad  (and (not left-align) (position #\0 flags))))
                               (incf i)
                               (cond
                                 ((char= dir #\%)
                                  (write-char #\% stream))
                                 ((char= dir #\s)
                                  (let* ((sv (%php-stringify val))
                                         (sv (if precision (subseq sv 0 (min precision (length sv))) sv))
                                         (pad (if width (max 0 (- width (length sv))) 0)))
                                    (if left-align
                                        (progn (write-string sv stream)
                                               (dotimes (k pad) (write-char #\Space stream)))
                                        (progn (dotimes (k pad) (write-char #\Space stream))
                                               (write-string sv stream)))))
                                 ((member dir '(#\d #\i))
                                  (let* ((n (if (numberp val) (truncate val) 0))
                                         (sv (format nil "~D" n))
                                         (pad (if width (max 0 (- width (length sv))) 0))
                                         (pad-ch (if zero-pad #\0 #\Space)))
                                    (if left-align
                                        (progn (write-string sv stream)
                                               (dotimes (k pad) (write-char #\Space stream)))
                                        (progn (dotimes (k pad) (write-char pad-ch stream))
                                               (write-string sv stream)))))
                                 ((char= dir #\f)
                                  (let* ((prec (or precision 6))
                                         (sv (format nil "~,vF" prec (coerce (or val 0) 'double-float)))
                                         (pad (if width (max 0 (- width (length sv))) 0))
                                         (pad-ch (if zero-pad #\0 #\Space)))
                                    (if left-align
                                        (progn (write-string sv stream)
                                               (dotimes (k pad) (write-char #\Space stream)))
                                        (progn (dotimes (k pad) (write-char pad-ch stream))
                                               (write-string sv stream)))))
                                 ((member dir '(#\e #\E))
                                  (let* ((prec (or precision 6))
                                         (sv (if (char= dir #\e)
                                                 (format nil "~,ve" prec (coerce (or val 0) 'double-float))
                                                 (format nil "~,vE" prec (coerce (or val 0) 'double-float)))))
                                    (write-string sv stream)))
                                 ((char= dir #\o)
                                  (write-string (format nil "~O" (truncate (or val 0))) stream))
                                 ((char= dir #\x)
                                  (write-string (string-downcase (format nil "~X" (truncate (or val 0)))) stream))
                                 ((char= dir #\X)
                                  (write-string (format nil "~X" (truncate (or val 0))) stream))
                                 ((char= dir #\b)
                                  (write-string (format nil "~B" (truncate (or val 0))) stream))
                                 ((char= dir #\c)
                                  (write-char (code-char (truncate (or val 0))) stream))
                                 ((char= dir #\u)
                                  (let ((n (truncate (or val 0))))
                                    (write-string (format nil "~D" (if (minusp n) (+ n 4294967296) n)) stream)))
                                 (t
                                  (write-char #\% stream)
                                  (write-char dir stream)))))))
                       (write-char ch stream))))))))


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

;;; ─── JSON functions ───────────────────────────────────────────────────────────

(defun %php-json-encode (value &optional flags depth)
  "PHP json_encode: encode PHP value to JSON string."
  (declare (ignore flags depth))
  (%php-json-encode-value value 0))

(defun %php-json-encode-value (val depth)
  (when (> depth 512) (return-from %php-json-encode-value "null"))
  (cond
    ((%php-null-p val) "null")
    ((null val) "false")
    ((eq val t) "true")
    ((integerp val) (format nil "~D" val))
    ((floatp val) (if (= val (floor val))
                      (format nil "~D" (floor val))
                      (format nil "~F" val)))
    ((stringp val) (%php-json-quote-string val))
    ((hash-table-p val)
     (let* ((pairs (%php-array-pairs val))
            (is-array-p (loop for i from 0 for (k . v) in pairs
                              always (eql k i))))
       (if is-array-p
           (format nil "[~{~A~^,~}]"
                   (mapcar (lambda (p) (%php-json-encode-value (cdr p) (1+ depth))) pairs))
           (format nil "{~{~A~^,~}}"
                   (mapcar (lambda (p)
                             (format nil "~A:~A"
                                     (%php-json-quote-string (%php-stringify (car p)))
                                     (%php-json-encode-value (cdr p) (1+ depth))))
                           pairs)))))
    (t "null")))

(defun %php-json-quote-string (s)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across s
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char ch out))))
    (write-char #\" out)))

;;; ─── serialize / unserialize (PHP's native serialization format) ────────────
;;;
;;; N;                  null
;;; b:0; / b:1;         bool
;;; i:42;               int
;;; d:3.14;             float
;;; s:5:"hello";        string (LEN is byte length; we approximate with chars)
;;; a:2:{<k><v><k><v>}  array — count, then key/value serialized pairs

(defun %php-serialize-float-text (val)
  "Render float VAL the way PHP serialize does: integral values without a
fractional part (3.0 -> \"3\"), others in full."
  (if (= val (floor val))
      (format nil "~D" (floor val))
      (format nil "~F" val)))

(defun %php-serialize-into (value out)
  "Write the PHP-serialized form of VALUE to stream OUT."
  (cond
    ((%php-null-p value) (write-string "N;" out))
    ((eq value t) (write-string "b:1;" out))
    ((null value) (write-string "b:0;" out))
    ((integerp value) (format out "i:~D;" value))
    ((floatp value) (format out "d:~A;" (%php-serialize-float-text value)))
    ((stringp value) (format out "s:~D:\"~A\";" (length value) value))
    ((hash-table-p value)
     (let ((pairs (%php-array-pairs value)))
       (format out "a:~D:{" (length pairs))
       (dolist (pair pairs)
         (%php-serialize-into (car pair) out)
         (%php-serialize-into (cdr pair) out))
       (write-string "}" out)))
    (t (write-string "N;" out))))

(defun %php-serialize (value)
  "PHP serialize: produce a storable string representation of VALUE."
  (with-output-to-string (out)
    (%php-serialize-into value out)))

(defun %php-unserialize-at (str pos)
  "Parse one serialized value from STR at POS. Return (values value next-pos)."
  (case (char str pos)
    (#\N (values +php-null+ (+ pos 2)))                         ; N;
    (#\b (values (char= (char str (+ pos 2)) #\1) (+ pos 4)))   ; b:0; / b:1;
    (#\i (let* ((start (+ pos 2)) (semi (position #\; str :start start)))
           (values (parse-integer str :start start :end semi) (1+ semi))))
    (#\d (let* ((start (+ pos 2)) (semi (position #\; str :start start)))
           (values (let ((*read-default-float-format* 'double-float))
                     (coerce (read-from-string (subseq str start semi)) 'double-float))
                   (1+ semi))))
    (#\s (let* ((len-start (+ pos 2))
                (colon (position #\: str :start len-start))
                (len (parse-integer str :start len-start :end colon))
                (data-start (+ colon 2))            ; skip :"
                (data-end (+ data-start len)))
           (values (subseq str data-start data-end) (+ data-end 2)))) ; skip ";
    (#\a (let* ((count-start (+ pos 2))
                (colon (position #\: str :start count-start))
                (count (parse-integer str :start count-start :end colon))
                (p (+ colon 2))                     ; skip :{
                (arr (%php-make-array)))
           (dotimes (_ count)
             (multiple-value-bind (k kp) (%php-unserialize-at str p)
               (multiple-value-bind (v vp) (%php-unserialize-at str kp)
                 (%php-array-set arr k v)
                 (setf p vp))))
           (values arr (1+ p))))                    ; skip }
    ;; Unknown tag -> malformed input; PHP unserialize returns false (NIL).
    (t (values nil pos))))

(defun %php-unserialize (str)
  "PHP unserialize: reconstruct a value from its serialized STR. Returns false
(NIL) on malformed input, like PHP."
  (handler-case
      (multiple-value-bind (value pos) (%php-unserialize-at (%php-stringify str) 0)
        (declare (ignore pos))
        value)
    (error () nil)))

(defun %php-json-decode (str &optional assoc depth flags)
  "PHP json_decode: parse JSON string to PHP value."
  (declare (ignore assoc depth flags))
  (handler-case (%php-json-parse-value (%php-stringify str) 0)
    (error () +php-null+)))

(defun %php-json-skip-ws (s pos)
  (loop while (and (< pos (length s))
                   (member (char s pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun %php-json-parse-value (s pos)
  (setf pos (%php-json-skip-ws s pos))
  (when (>= pos (length s)) (return-from %php-json-parse-value +php-null+))
  (let ((ch (char s pos)))
    (cond
      ((char= ch #\") (%php-json-parse-string s (1+ pos)))
      ((char= ch #\{) (%php-json-parse-object s (1+ pos)))
      ((char= ch #\[) (%php-json-parse-array s (1+ pos)))
      ((and (>= (length s) (+ pos 4)) (string= s "null" :start1 pos :end1 (+ pos 4))) +php-null+)
      ((and (>= (length s) (+ pos 4)) (string= s "true" :start1 pos :end1 (+ pos 4))) t)
      ((and (>= (length s) (+ pos 5)) (string= s "false" :start1 pos :end1 (+ pos 5))) nil)
      ((or (digit-char-p ch) (char= ch #\-))
       (let ((end pos))
         (when (char= ch #\-) (incf end))
         (loop while (and (< end (length s))
                          (or (digit-char-p (char s end)) (member (char s end) '(#\. #\e #\E #\+ #\-))))
               do (incf end))
         (handler-case (read-from-string (subseq s pos end)) (error () 0))))
      (t +php-null+))))

(defun %php-json-parse-string (s pos)
  (with-output-to-string (buf)
    (loop while (and (< pos (length s)) (not (char= (char s pos) #\")))
          do (let ((ch (char s pos)))
               (incf pos)
               (if (char= ch #\\)
                   (let ((esc (char s pos)))
                     (incf pos)
                     (write-char (case esc (#\" #\") (#\\ #\\) (#\n #\Newline) (#\r #\Return) (#\t #\Tab) (t esc)) buf))
                   (write-char ch buf)))))
  (values (get-output-stream-string (make-string-output-stream)) (1+ pos)))

(defun %php-json-parse-object (s pos)
  (let ((result (%php-make-array)))
    (setf pos (%php-json-skip-ws s pos))
    (when (char= (char s pos) #\}) (return-from %php-json-parse-object result))
    (loop
      (setf pos (%php-json-skip-ws s pos))
      (unless (char= (char s pos) #\") (return result))
      (let* ((key-start (1+ pos))
             (key-end (position #\" s :start key-start)))
        (unless key-end (return result))
        (let ((key (subseq s key-start key-end)))
          (setf pos (1+ key-end)
                pos (%php-json-skip-ws s pos))
          (when (char= (char s pos) #\:) (incf pos))
          (setf pos (%php-json-skip-ws s pos))
          (let ((val (%php-json-parse-value s pos)))
            (%php-array-set result key val)
            (setf pos (%php-json-skip-ws s (+ pos 1)))
            (cond ((char= (char s pos) #\,) (incf pos))
                  ((char= (char s pos) #\}) (return result))
                  (t (return result))))))))
  result)

(defun %php-json-parse-array (s pos)
  (let ((result (%php-make-array)))
    (setf pos (%php-json-skip-ws s pos))
    (when (char= (char s pos) #\]) (return-from %php-json-parse-array result))
    (loop for i from 0
          do (let ((val (%php-json-parse-value s pos)))
               (%php-array-set result i val)
               (setf pos (%php-json-skip-ws s (+ pos 1)))
               (cond ((char= (char s pos) #\,) (incf pos))
                     (t (return result)))))
    result))

;;; ─── mb_* multibyte string functions ─────────────────────────────────────────
;;; In CL, strings are Unicode-aware, so mb_* is mostly identical to the regular
;;; string functions for UTF-8 content.

(defun %php-mb-strlen (str &optional encoding)
  (declare (ignore encoding))
  (length (%php-stringify str)))

(defun %php-mb-substr (str start &optional length encoding)
  (declare (ignore encoding))
  (let* ((s (%php-stringify str))
         (n (length s))
         (b (if (< start 0) (max 0 (+ n start)) (min start n)))
         (e (if (null length) n
                (if (< length 0) (max b (+ n length))
                    (min n (+ b length))))))
    (subseq s b e)))

(defun %php-mb-strtolower (str &optional encoding)
  (declare (ignore encoding))
  (string-downcase (%php-stringify str)))

(defun %php-mb-strtoupper (str &optional encoding)
  (declare (ignore encoding))
  (string-upcase (%php-stringify str)))

(defun %php-mb-strpos (haystack needle &optional (offset 0) encoding)
  (declare (ignore encoding))
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (start (if (< offset 0) (max 0 (+ (length h) offset)) offset))
         (found (search n h :start2 start)))
    (if found found nil)))

(defun %php-mb-strrpos (haystack needle &optional (offset 0) encoding)
  (declare (ignore encoding))
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (end (if (< offset 0) (max 0 (+ (length h) offset)) (length h)))
         (found (search n h :from-end t :end2 end)))
    (if found found nil)))

(defun %php-mb-substr-count (haystack needle &optional encoding)
  (declare (ignore encoding))
  (let ((h (%php-stringify haystack))
        (n (%php-stringify needle))
        (count 0) (pos 0))
    (loop (let ((found (search n h :start2 pos)))
            (unless found (return count))
            (incf count)
            (setf pos (+ found (max 1 (length n))))))))

(defun %php-mb-detect-encoding (str &optional encoding-list strict)
  (declare (ignore str encoding-list strict))
  "UTF-8")  ; Simplified: always report UTF-8

(defun %php-mb-internal-encoding (&optional enc)
  (declare (ignore enc))
  "UTF-8")

(defun %php-mb-convert-encoding (str to-encoding &optional from-encoding)
  (declare (ignore to-encoding from-encoding))
  (%php-stringify str))  ; Pass-through: CL handles Unicode natively

(defun %php-mb-strtolower-encoding (str encoding)
  (declare (ignore encoding))
  (string-downcase (%php-stringify str)))

(defun %php-mb-convert-case (str mode &optional encoding)
  "PHP mb_convert_case: convert case of multibyte string."
  (declare (ignore encoding))
  (let ((s (%php-stringify str)))
    (case mode
      (0 (string-upcase s))    ; MB_CASE_UPPER = 0
      (1 (string-downcase s))  ; MB_CASE_LOWER = 1
      (2 (string-capitalize s)); MB_CASE_TITLE = 2
      (t s))))

(defun %php-mb-str-pad (input length &optional (pad-string " ") (pad-type 1) encoding)
  "PHP 8.3 mb_str_pad: pad a multibyte string to LENGTH."
  (declare (ignore encoding))
  (let* ((s (%php-stringify input))
         (p (%php-stringify pad-string))
         (slen (length s))
         (target (max slen length))
         (deficit (- target slen)))
    (if (<= deficit 0) s
        (let* ((full-pads (floor deficit (max 1 (length p))))
               (remainder  (mod   deficit (max 1 (length p))))
               (pad-chunk  (concatenate 'string
                                        (apply #'concatenate 'string (make-list full-pads :initial-element p))
                                        (subseq p 0 remainder))))
          (cond ((= pad-type 1) (concatenate 'string s pad-chunk))          ; STR_PAD_RIGHT
                ((= pad-type 0) (concatenate 'string pad-chunk s))          ; STR_PAD_LEFT
                ((= pad-type 2)                                              ; STR_PAD_BOTH
                 (let* ((left-pad  (floor deficit 2))
                        (right-pad (- deficit left-pad))
                        (mk (lambda (n)
                              (let* ((fp (floor n (max 1 (length p))))
                                     (rm (mod   n (max 1 (length p)))))
                                (concatenate 'string
                                             (apply #'concatenate 'string (make-list fp :initial-element p))
                                             (subseq p 0 rm))))))
                   (concatenate 'string (funcall mk left-pad) s (funcall mk right-pad))))
                (t (concatenate 'string s pad-chunk)))))))

(defun %php-mb-str-split (string &optional (length 1) encoding)
  "PHP mb_str_split: split multibyte string into array of LENGTH-char chunks."
  (declare (ignore encoding))
  (let* ((s (%php-stringify string))
         (n (length s))
         (l (max 1 (if (or (null length) (%php-null-p length)) 1 length)))
         (result (%php-make-array)))
    (loop for i from 0 below n by l
          do (%php-array-set result (%php-array-next-auto-index result)
                             (subseq s i (min n (+ i l)))))
    result))

(defun %php-str-getcsv (string &optional (separator ",") (enclosure "\"") (escape "\\"))
  "PHP str_getcsv: parse a CSV string into an array."
  (declare (ignore escape))
  (let* ((s (%php-stringify string))
         (sep (%php-stringify separator))
         (enc (%php-stringify enclosure))
         (result (%php-make-array))
         (parts (%php-array-values-list (%php-explode sep s))))
    (dolist (field parts)
      (let* ((f (string-trim '(#\Space) field))
             (stripped (if (and (>= (length f) 2)
                                (string= (subseq f 0 1) enc)
                                (string= (subseq f (1- (length f))) enc))
                           (subseq f 1 (1- (length f)))
                           f)))
        (%php-array-set result (%php-array-next-auto-index result) stripped)))
    result))

;;; ─── HTML string functions ────────────────────────────────────────────────────

(defun %php-nl2br (str &optional (is-xhtml t))
  "PHP nl2br: insert <br> before newlines."
  (let ((br (if (%php-truthy is-xhtml) "<br />" "<br>")))
    (with-output-to-string (out)
      (loop for ch across (%php-stringify str)
            do (when (char= ch #\Newline)
                 (write-string br out))
               (write-char ch out)))))

(defun %php-wordwrap (str &optional (width 75) (break-str "
") (cut-long-words nil))
  "PHP wordwrap: wrap string at WIDTH characters."
  (declare (ignore cut-long-words))
  (let ((s (%php-stringify str))
        (w (or width 75))
        (brk (%php-stringify break-str)))
    (with-output-to-string (out)
      (let ((line-len 0)
            (current-word (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
        (flet ((flush-word ()
                 (let ((wlen (length current-word)))
                   (when (> wlen 0)
                     (when (and (> line-len 0) (> (+ line-len 1 wlen) w))
                       (write-string brk out)
                       (setf line-len 0))
                     (when (> line-len 0)
                       (write-char #\Space out)
                       (incf line-len))
                     (write-string current-word out)
                     (incf line-len wlen)
                     (setf (fill-pointer current-word) 0)))))
          (loop for ch across s
                do (if (char= ch #\Space)
                       (flush-word)
                       (vector-push-extend ch current-word)))
          (flush-word))))))

(defun %php-chunk-split (str &optional (chunklen 76) (end "\r\n"))
  "PHP chunk_split: split string into chunks."
  (let* ((s (%php-stringify str))
         (e (%php-stringify end))
         (n (length s))
         (cl (or chunklen 76)))
    (with-output-to-string (out)
      (loop for i from 0 below n by cl
            do (write-string (subseq s i (min n (+ i cl))) out)
               (write-string e out)))))

(defun %php-html-entity-decode (str &optional flags encoding)
  (declare (ignore flags encoding))
  (let ((s (%php-stringify str)))
    (loop for (ent . ch) in '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                               ("&quot;" . "\"") ("&#039;" . "'") ("&nbsp;" . " "))
          do (setf s (cl:with-output-to-string (out)
                       (let ((pos 0))
                         (loop (let ((found (search ent s :start2 pos)))
                                 (if found
                                     (progn (write-string s out :start pos :end found)
                                            (write-string ch out)
                                            (setf pos (+ found (length ent))))
                                     (progn (write-string s out :start pos) (return)))))))))
    s))

(defun %php-strip-tags (str &optional allowed-tags)
  "PHP strip_tags: remove HTML/PHP tags."
  (declare (ignore allowed-tags))
  (with-output-to-string (out)
    (let ((in-tag nil))
      (loop for ch across (%php-stringify str)
            do (cond ((char= ch #\<) (setf in-tag t))
                     ((char= ch #\>) (setf in-tag nil))
                     ((not in-tag) (write-char ch out)))))))

(defun %php-addslashes (str)
  "PHP addslashes: escape quotes and backslashes."
  (with-output-to-string (out)
    (loop for ch across (%php-stringify str)
          do (when (member ch '(#\\ #\' #\" #\Null))
               (write-char #\\ out))
             (write-char ch out))))

(defun %php-stripslashes (str)
  "PHP stripslashes: remove backslash escapes."
  (with-output-to-string (out)
    (let ((s (%php-stringify str))
          (i 0))
      (loop while (< i (length s))
            do (let ((ch (char s i)))
                 (if (char= ch #\\)
                     (progn (incf i)
                            (when (< i (length s)) (write-char (char s i) out)))
                     (write-char ch out))
                 (incf i))))))

(defun %php-base64-encode (str)
  "PHP base64_encode: base64 encode string."
  ;; Simple base64 implementation
  (let* ((s (%php-stringify str))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (let ((bytes (map 'vector #'char-code s))
            (n (length s)))
        (loop for i from 0 below n by 3
              do (let* ((b0 (aref bytes i))
                        (b1 (if (< (1+ i) n) (aref bytes (1+ i)) 0))
                        (b2 (if (< (+ i 2) n) (aref bytes (+ i 2)) 0))
                        (g0 (ash b0 -2))
                        (g1 (logior (ash (logand b0 3) 4) (ash b1 -4)))
                        (g2 (logior (ash (logand b1 15) 2) (ash b2 -6)))
                        (g3 (logand b2 63)))
                   (write-char (char alphabet g0) out)
                   (write-char (char alphabet g1) out)
                   (write-char (if (< (+ i 1) n) (char alphabet g2) #\=) out)
                   (write-char (if (< (+ i 2) n) (char alphabet g3) #\=) out)))))))

(defun %php-base64-decode (str &optional strict)
  "PHP base64_decode: decode base64 string."
  (declare (ignore strict))
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) (%php-stringify str)))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (flet ((decode-char (ch)
             (let ((pos (position ch alphabet)))
               (or pos 0))))
      (with-output-to-string (out)
        (loop for i from 0 below (length s) by 4
              while (< (+ i 3) (length s))
              do (let* ((g0 (decode-char (char s i)))
                        (g1 (decode-char (char s (1+ i))))
                        (g2 (if (char= (char s (+ i 2)) #\=) 0 (decode-char (char s (+ i 2)))))
                        (g3 (if (char= (char s (+ i 3)) #\=) 0 (decode-char (char s (+ i 3)))))
                        (b0 (logior (ash g0 2) (ash g1 -4)))
                        (b1 (logior (ash (logand g1 15) 4) (ash g2 -2)))
                        (b2 (logior (ash (logand g2 3) 6) g3)))
                   (write-char (code-char b0) out)
                   (when (not (char= (char s (+ i 2)) #\=))
                     (write-char (code-char b1) out))
                   (when (not (char= (char s (+ i 3)) #\=))
                     (write-char (code-char b2) out))))))))

(defun %php-md5 (str &optional raw)
  "PHP md5: return MD5 hash (simplified — returns a placeholder)."
  (declare (ignore raw))
  ;; Real MD5 would require a crypto library; return a deterministic placeholder
  (let ((s (%php-stringify str)))
    (format nil "~32,'0X" (mod (loop for ch across s sum (char-code ch)) #x100000000000000000000000000000000))))

(defun %php-sha1 (str &optional raw)
  "PHP sha1: simplified placeholder."
  (declare (ignore raw))
  (let ((s (%php-stringify str)))
    (format nil "~40,'0X" (mod (loop for ch across s sum (* (char-code ch) 31)) #x10000000000000000000000000000000000000000))))

(defun %php-crc32 (str)
  "PHP crc32: CRC-32 checksum (simplified)."
  (let ((crc #xFFFFFFFF))
    (loop for ch across (%php-stringify str)
          do (setf crc (logxor (ash crc -8)
                               (logand (logxor (logand crc #xFF) (char-code ch)) #xFF))))
    (logxor crc #xFFFFFFFF)))

(defun %php-urlencode (str)
  "PHP urlencode: percent-encode a string."
  (with-output-to-string (out)
    (loop for ch across (%php-stringify str)
          do (cond ((char= ch #\Space) (write-char #\+ out))
                   ((or (alphanumericp ch) (member ch '(#\- #\_ #\. #\~)))
                    (write-char ch out))
                   (t (format out "%~2,'0X" (char-code ch)))))))

(defun %php-urldecode (str)
  "PHP urldecode: decode percent-encoded string."
  (with-output-to-string (out)
    (let ((s (%php-stringify str))
          (i 0))
      (loop while (< i (length s))
            do (let ((ch (char s i)))
                 (cond ((char= ch #\+) (write-char #\Space out) (incf i))
                       ((and (char= ch #\%) (< (+ i 2) (length s)))
                        (write-char (code-char (parse-integer (subseq s (1+ i) (+ i 3)) :radix 16))
                                    out)
                        (incf i 3))
                       (t (write-char ch out) (incf i))))))))

(defun %php-rawurlencode (str) (%php-urlencode str))
(defun %php-rawurldecode (str) (%php-urldecode str))

;;; ─── String padding / splitting ──────────────────────────────────────────────

(defconstant +php-str-pad-right+ 1)
(defconstant +php-str-pad-left+  0)
(defconstant +php-str-pad-both+  2)

(defun %php-str-pad (string length &optional (pad-string " ") (pad-type +php-str-pad-right+))
  "PHP str_pad: pad STRING to LENGTH using PAD-STRING on the given side."
  (let* ((s (%php-stringify string))
         (p (%php-stringify (or pad-string " ")))
         (target (if (numberp length) length 0))
         (slen (length s))
         (plen (length p)))
    (if (or (<= target slen) (= plen 0))
        s
        (let* ((need (- target slen))
               (full-pads (floor need plen))
               (remainder (- need (* full-pads plen))))
          (flet ((make-pad (n)
                   (let ((r (- n (* (floor n plen) plen))))
                     (with-output-to-string (o)
                       (dotimes (i (floor n plen)) (write-string p o))
                       (when (> r 0) (write-string p o :end r))))))
            (cond ((= pad-type +php-str-pad-left+)
                   (concatenate 'string (make-pad need) s))
                  ((= pad-type +php-str-pad-both+)
                   (let* ((left-need (floor need 2))
                          (right-need (- need left-need)))
                     (concatenate 'string (make-pad left-need) s (make-pad right-need))))
                  (t
                   (concatenate 'string s (make-pad need)))))))))

(defun %php-str-split (string &optional (split-length 1))
  "PHP str_split: split STRING into chunks of SPLIT-LENGTH, returning a PHP array."
  (let* ((s (%php-stringify string))
         (n (length s))
         (cl (max 1 (or split-length 1)))
         (result (%php-make-array)))
    (loop for i from 0 below n by cl
          do (%php-array-set result (%php-array-next-auto-index result)
                             (subseq s i (min n (+ i cl)))))
    result))

(defun %php-substr-count (haystack needle &optional (offset 0) length)
  "PHP substr_count: count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (nlen (length n))
         (hlen (length h))
         (start (or offset 0))
         (end (if (and length (not (%php-null-p length))) (+ start length) hlen))
         (count 0))
    (when (> nlen 0)
      (loop for pos = (search n h :start2 start :end2 end)
            while pos
            do (incf count)
               (setf start (+ pos nlen))))
    count))

(defun %php-substr-replace (string replace start &optional length)
  "PHP substr_replace: replace a portion of STRING with REPLACE."
  (let* ((s (%php-stringify string))
         (r (%php-stringify replace))
         (slen (length s))
         (b (if (minusp start) (max 0 (+ slen start)) (min start slen)))
         (e (cond ((or (null length) (%php-null-p length)) slen)
                  ((minusp length) (max b (+ slen length)))
                  (t (min slen (+ b length))))))
    (concatenate 'string (subseq s 0 b) r (subseq s e))))

;;; ─── Case helpers ─────────────────────────────────────────────────────────────

(defun %php-ucfirst (string)
  "PHP ucfirst: uppercase first character of STRING."
  (let ((s (%php-stringify string)))
    (if (= (length s) 0) s
        (concatenate 'string (string (char-upcase (char s 0))) (subseq s 1)))))

(defun %php-lcfirst (string)
  "PHP lcfirst: lowercase first character of STRING."
  (let ((s (%php-stringify string)))
    (if (= (length s) 0) s
        (concatenate 'string (string (char-downcase (char s 0))) (subseq s 1)))))

(defun %php-ucwords (string &optional (delimiters " \t\r\n\f\v"))
  "PHP ucwords: uppercase first char of each word in STRING."
  (let* ((s (%php-stringify string))
         (delims (coerce (%php-stringify delimiters) 'list))
         (prev-delim t))
    (coerce (loop for ch across s
                  collect (if prev-delim (char-upcase ch) ch)
                  do (setf prev-delim (member ch delims)))
            'string)))

;;; ─── Char / ordinal ───────────────────────────────────────────────────────────

(defun %php-ord (string)
  "PHP ord: ASCII value of first character of STRING."
  (let ((s (%php-stringify string)))
    (if (= (length s) 0) 0 (char-code (char s 0)))))

(defun %php-chr (codepoint)
  "PHP chr: character from ASCII CODEPOINT."
  (string (code-char (logand codepoint 255))))

;;; ─── Hex / binary conversion ──────────────────────────────────────────────────

(defun %php-bin2hex (string)
  "PHP bin2hex: convert STRING to hex representation."
  (with-output-to-string (out)
    (loop for ch across (%php-stringify string)
          do (format out "~2,'0x" (char-code ch)))))

(defun %php-hex2bin (hex)
  "PHP hex2bin: convert HEX string to binary string."
  (let ((h (%php-stringify hex)))
    (with-output-to-string (out)
      (loop for i from 0 below (length h) by 2
            when (< (+ i 1) (length h))
            do (write-char (code-char (parse-integer h :start i :end (+ i 2) :radix 16)) out)))))

;;; ─── String comparison ────────────────────────────────────────────────────────

(defun %php-strcmp (s1 s2)
  "PHP strcmp: binary-safe string comparison."
  (let ((a (%php-stringify s1)) (b (%php-stringify s2)))
    (cond ((string< a b) -1) ((string> a b) 1) (t 0))))

(defun %php-strcasecmp (s1 s2)
  "PHP strcasecmp: case-insensitive string comparison."
  (let ((a (string-downcase (%php-stringify s1)))
        (b (string-downcase (%php-stringify s2))))
    (cond ((string< a b) -1) ((string> a b) 1) (t 0))))

(defun %php-strncmp (s1 s2 n)
  "PHP strncmp: first N chars comparison."
  (let ((a (subseq (%php-stringify s1) 0 (min n (length (%php-stringify s1)))))
        (b (subseq (%php-stringify s2) 0 (min n (length (%php-stringify s2))))))
    (cond ((string< a b) -1) ((string> a b) 1) (t 0))))

(defun %php-strncasecmp (s1 s2 n)
  "PHP strncasecmp: first N chars case-insensitive comparison."
  (let ((a (string-downcase (subseq (%php-stringify s1) 0 (min n (length (%php-stringify s1))))))
        (b (string-downcase (subseq (%php-stringify s2) 0 (min n (length (%php-stringify s2)))))))
    (cond ((string< a b) -1) ((string> a b) 1) (t 0))))

;;; ─── String search ────────────────────────────────────────────────────────────

(defun %php-strstr (haystack needle &optional before-needle)
  "PHP strstr: find first occurrence of NEEDLE in HAYSTACK."
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (pos (search n h)))
    (when pos
      (if (%php-truthy before-needle)
          (subseq h 0 pos)
          (subseq h pos)))))

(defun %php-stristr (haystack needle &optional before-needle)
  "PHP stristr: case-insensitive strstr."
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (pos (search (string-downcase n) (string-downcase h))))
    (when pos
      (if (%php-truthy before-needle)
          (subseq h 0 pos)
          (subseq h pos)))))

(defun %php-strchr (haystack needle &optional before-needle)
  "PHP strchr: alias for strstr."
  (%php-strstr haystack needle before-needle))

;;; ─── String word operations ───────────────────────────────────────────────────

(defun %php-str-word-count (string &optional (format 0) charlist)
  "PHP str_word_count: count words or return word array."
  (declare (ignore charlist))
  (let* ((s (%php-stringify string))
         (words nil)
         (in-word nil)
         (word-start 0))
    (loop for i from 0 below (length s)
          for ch = (char s i)
          do (cond ((and (alpha-char-p ch) (not in-word))
                    (setf in-word t word-start i))
                   ((and (not (alpha-char-p ch)) in-word)
                    (push (cons word-start (subseq s word-start i)) words)
                    (setf in-word nil)))
          finally (when in-word
                    (push (cons word-start (subseq s word-start)) words)))
    (let ((ordered (reverse words)))
      (cond ((= format 0) (length ordered))
            ((= format 1) (let ((r (%php-make-array)))
                            (loop for i from 0 for (pos . w) in ordered
                                  do (%php-array-set r i w))
                            r))
            ((= format 2) (let ((r (%php-make-array)))
                            (loop for (pos . w) in ordered
                                  do (%php-array-set r pos w))
                            r))
            (t (length ordered))))))

;;; ─── Edit distance / similarity ──────────────────────────────────────────────

(defun %php-levenshtein (s1 s2)
  "PHP levenshtein: edit distance between S1 and S2."
  (let* ((a (%php-stringify s1))
         (b (%php-stringify s2))
         (m (length a))
         (n (length b))
         (dp (make-array (list (1+ m) (1+ n)) :initial-element 0)))
    (dotimes (i (1+ m)) (setf (aref dp i 0) i))
    (dotimes (j (1+ n)) (setf (aref dp 0 j) j))
    (loop for i from 1 to m do
      (loop for j from 1 to n do
        (setf (aref dp i j)
              (if (char= (char a (1- i)) (char b (1- j)))
                  (aref dp (1- i) (1- j))
                  (1+ (min (aref dp (1- i) j)
                           (aref dp i (1- j))
                           (aref dp (1- i) (1- j))))))))
    (aref dp m n)))

(defun %php-similar-text (s1 s2 &optional percent-var)
  "PHP similar_text: compute similarity between S1 and S2."
  (declare (ignore percent-var))
  (let* ((a (%php-stringify s1))
         (b (%php-stringify s2))
         (alen (length a))
         (blen (length b)))
    (if (or (= alen 0) (= blen 0))
        0
        (let ((common 0))
          (loop for i from 0 below alen do
            (loop for j from 0 below blen do
              (when (char= (char a i) (char b j))
                (incf common)
                (return))))
          common))))

(defun %php-soundex (string)
  "PHP soundex: Soundex phonetic encoding of STRING."
  (let* ((s (string-upcase (%php-stringify string)))
         (table '((#\B . #\1) (#\F . #\1) (#\P . #\1) (#\V . #\1)
                  (#\C . #\2) (#\G . #\2) (#\J . #\2) (#\K . #\2)
                  (#\Q . #\2) (#\S . #\2) (#\X . #\2) (#\Z . #\2)
                  (#\D . #\3) (#\T . #\3)
                  (#\L . #\4)
                  (#\M . #\5) (#\N . #\5)
                  (#\R . #\6))))
    (if (= (length s) 0) ""
        (let ((result (make-array 4 :element-type 'character :initial-element #\0))
              (idx 1)
              (prev #\Nul))
          (setf (aref result 0) (char s 0))
          (loop for ch across (subseq s 1)
                while (< idx 4)
                do (let ((code (cdr (assoc ch table))))
                     (when (and code (not (eql code prev)))
                       (setf (aref result idx) code
                             prev code)
                       (incf idx))))
          (coerce result 'string)))))

;;; ─── printf family ────────────────────────────────────────────────────────────

(defun %php-printf (format-string &rest args)
  "PHP printf: formatted print, returns length written."
  (let ((out (apply #'%php-sprintf format-string args)))
    (write-string out)
    (length out)))

(defun %php-vsprintf (format-string args-array)
  "PHP vsprintf: sprintf with args from array."
  (apply #'%php-sprintf format-string (%php-array-values-list args-array)))

(defun %php-vprintf (format-string args-array)
  "PHP vprintf: printf with args from array."
  (let ((out (%php-vsprintf format-string args-array)))
    (write-string out)
    (length out)))


;;; ─── ctype_* character classification (PHP ext/ctype) ────────────────────────

(defun %php-ctype-alpha (text)
  "PHP ctype_alpha: check if all chars are alphabetic."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every #'alpha-char-p s))))

(defun %php-ctype-digit (text)
  "PHP ctype_digit: check if all chars are decimal digits."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every #'digit-char-p s))))

(defun %php-ctype-alnum (text)
  "PHP ctype_alnum: check if all chars are alphanumeric."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every #'alphanumericp s))))

(defun %php-ctype-space (text)
  "PHP ctype_space: check if all chars are whitespace."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page #\Null))) s))))

(defun %php-ctype-upper (text)
  "PHP ctype_upper: check if all chars are uppercase."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (and (alpha-char-p c) (upper-case-p c))) s))))

(defun %php-ctype-lower (text)
  "PHP ctype_lower: check if all chars are lowercase."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (and (alpha-char-p c) (lower-case-p c))) s))))

(defun %php-ctype-punct (text)
  "PHP ctype_punct: check if all chars are punctuation (visible non-alphanumeric)."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (and (graphic-char-p c) (not (alphanumericp c)) (not (char= c #\Space)))) s))))

(defun %php-ctype-graph (text)
  "PHP ctype_graph: check if all chars are visible (non-space graphic)."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (and (graphic-char-p c) (not (char= c #\Space)))) s))))

(defun %php-ctype-print (text)
  "PHP ctype_print: check if all chars are printable (including space)."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every #'graphic-char-p s))))

(defun %php-ctype-xdigit (text)
  "PHP ctype_xdigit: check if all chars are valid hexadecimal."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (digit-char-p c 16)) s))))

;;; ─── htmlspecialchars / html_entity_decode additions ────────────────────────

(defun %php-htmlspecialchars-decode (str &optional (flags 11))
  "PHP htmlspecialchars_decode: convert HTML entities to characters."
  (declare (ignore flags))
  (let ((s (%php-stringify str)))
    (cl:with-output-to-string (out)
      (let ((i 0) (n (length s)))
        (loop while (< i n)
              do (cond
                   ((and (< (+ i 3) n) (string= (subseq s i (+ i 4)) "&lt;"))
                    (write-char #\< out) (incf i 4))
                   ((and (< (+ i 3) n) (string= (subseq s i (+ i 4)) "&gt;"))
                    (write-char #\> out) (incf i 4))
                   ((and (< (+ i 4) n) (string= (subseq s i (+ i 5)) "&amp;"))
                    (write-char #\& out) (incf i 5))
                   ((and (< (+ i 5) n) (string= (subseq s i (+ i 6)) "&quot;"))
                    (write-char #\" out) (incf i 6))
                   ((and (< (+ i 5) n) (string= (subseq s i (+ i 6)) "&#039;"))
                    (write-char #\' out) (incf i 6))
                   (t
                    (write-char (char s i) out) (incf i))))))))

;;; ─── JSON validation (PHP 8.3) ───────────────────────────────────────────────

(defun %php-json-validate (json &optional (depth 512) (flags 0))
  "PHP 8.3 json_validate: check if string is valid JSON."
  (declare (ignore depth flags))
  (handler-case
      (progn (%php-json-decode json) t)
    (error () nil)))

;;; ─── mb_chr / mb_ord / mb_strimwidth / mb_ereg family ────────────────────────

(defun %php-mb-chr (code &optional encoding)
  "PHP mb_chr: get character by Unicode code point."
  (declare (ignore encoding))
  (handler-case (string (code-char (truncate code))) (error () nil)))

(defun %php-mb-ord (str &optional encoding)
  "PHP mb_ord: get Unicode code point of first character."
  (declare (ignore encoding))
  (let ((s (%php-stringify str)))
    (if (> (length s) 0) (char-code (char s 0)) nil)))

(defun %php-mb-strimwidth (str start width &optional (trim-marker "") encoding)
  "PHP mb_strimwidth: get truncated string to width."
  (declare (ignore encoding))
  (let* ((s (%php-stringify str))
         (n (length s))
         (b (min start n))
         (end (min n (+ b width))))
    (if (< end n)
        (concatenate 'string (subseq s b end) (%php-stringify trim-marker))
        (subseq s b end))))

(defun %php-mb-ereg (pattern str &optional regs)
  "PHP mb_ereg: multibyte extended regex match."
  (declare (ignore regs))
  (handler-case
      (let* ((pat (%php-stringify pattern))
             (s   (%php-stringify str))
             (fn  (%php-compile-regex pat)))
        (when fn
          (loop for i from 0 to (length s)
                for e = (funcall fn s i nil)
                when e do (return t)
                finally (return nil))))
    (error () nil)))

(defun %php-mb-eregi (pattern str &optional regs)
  "PHP mb_eregi: case-insensitive mb_ereg."
  (declare (ignore regs))
  (handler-case
      (let* ((pat (%php-stringify pattern))
             (s   (%php-stringify str))
             (fn  (%php-compile-regex pat :ic t)))
        (when fn
          (loop for i from 0 to (length s)
                for e = (funcall fn s i nil)
                when e do (return t)
                finally (return nil))))
    (error () nil)))

(defun %php-mb-ereg-replace (pattern replacement str &optional option)
  "PHP mb_ereg_replace: multibyte regex replace."
  (declare (ignore option))
  (handler-case
      (let* ((pat  (%php-stringify pattern))
             (repl (%php-stringify replacement))
             (s    (%php-stringify str))
             (fn   (%php-compile-regex pat)))
        (if fn
            (with-output-to-string (out)
              (let ((pos 0))
                (loop while (<= pos (length s))
                      do (let ((ms nil) (me nil))
                           (loop for i from pos to (length s)
                                 for e = (funcall fn s i nil)
                                 when e do (setf ms i me e) (return))
                           (if ms
                               (progn (write-string (subseq s pos ms) out)
                                      (write-string repl out)
                                      (setf pos (max (1+ ms) me)))
                               (progn (write-string (subseq s pos) out) (return)))))))
            s))
    (error () nil)))

(defun %php-mb-eregi-replace (pattern replacement str &optional option)
  "PHP mb_eregi_replace: case-insensitive mb_ereg_replace."
  (declare (ignore option))
  (handler-case
      (let* ((pat  (%php-stringify pattern))
             (repl (%php-stringify replacement))
             (s    (%php-stringify str))
             (fn   (%php-compile-regex pat :ic t)))
        (if fn
            (with-output-to-string (out)
              (let ((pos 0))
                (loop while (<= pos (length s))
                      do (let ((ms nil) (me nil))
                           (loop for i from pos to (length s)
                                 for e = (funcall fn s i nil)
                                 when e do (setf ms i me e) (return))
                           (if ms
                               (progn (write-string (subseq s pos ms) out)
                                      (write-string repl out)
                                      (setf pos (max (1+ ms) me)))
                               (progn (write-string (subseq s pos) out) (return)))))))
            s))
    (error () nil)))

(defun %php-mb-split (pattern str &optional limit)
  "PHP mb_split: split string by regex."
  (declare (ignore limit))
  (handler-case
      (let* ((pat (%php-stringify pattern))
             (s   (%php-stringify str))
             (fn  (%php-compile-regex pat))
             (result nil)
             (pos 0))
        (if fn
            (progn
              (loop while (<= pos (length s))
                    do (let ((ms nil) (me nil))
                         (loop for i from pos to (length s)
                               for e = (funcall fn s i nil)
                               when e do (setf ms i me e) (return))
                         (if ms
                             (progn (push (subseq s pos ms) result)
                                    (setf pos (max (1+ ms) me)))
                             (progn (push (subseq s pos) result) (return)))))
              (%php-list-to-array (nreverse result)))
            (%php-list-to-array (list s))))
    (error () (%php-list-to-array (list (%php-stringify str))))))

;;; ─── is_countable (PHP 7.3) ──────────────────────────────────────────────────

(defun %php-is-countable (value)
  "PHP is_countable: check if value can be counted."
  (or (hash-table-p value)
      (and (%php-null-p value) nil)))

;;; ─── quoted_printable functions ──────────────────────────────────────────────

(defun %php-quoted-printable-encode (str)
  "PHP quoted_printable_encode: encode string as quoted-printable."
  (with-output-to-string (out)
    (loop for ch across (%php-stringify str)
          do (let ((code (char-code ch)))
               (if (or (= code 9)
                       (and (>= code 32) (<= code 126) (/= code 61)))
                   (write-char ch out)
                   (format out "=~2,'0X" code))))))

(defun %php-quoted-printable-decode (str)
  "PHP quoted_printable_decode: decode quoted-printable string."
  (let ((s (%php-stringify str))
        (i 0))
    (with-output-to-string (out)
      (loop while (< i (length s))
            do (cond
                 ((and (char= (char s i) #\=)
                       (< (+ i 2) (length s))
                       (digit-char-p (char s (+ i 1)) 16)
                       (digit-char-p (char s (+ i 2)) 16))
                  (write-char (code-char (parse-integer s :start (1+ i) :end (+ i 3) :radix 16)) out)
                  (incf i 3))
                 (t (write-char (char s i) out) (incf i)))))))

;;; ─── Additional string functions (strtr, strpbrk, strspn, strcspn, etc.) ─────

(defun %php-strtr (str &rest args)
  "PHP strtr: strtr(str, from, to) translates chars; strtr(str, pairs) replaces
substrings from an associative array (longest-match-first)."
  (let ((s (%php-stringify str)))
    (cond
      ;; strtr(str, array) — pairs form
      ((and (= (length args) 1) (hash-table-p (first args)))
       (let* ((pairs (%php-array-pairs (first args)))
              ;; longest keys first (PHP semantics)
              (sorted (sort (copy-list pairs)
                            (lambda (a b) (> (length (%php-stringify (car a)))
                                             (length (%php-stringify (car b))))))))
         (with-output-to-string (out)
           (let ((i 0) (n (length s)))
             (loop while (< i n)
                   do (let ((matched nil))
                        (dolist (pair sorted)
                          (let* ((from (%php-stringify (car pair)))
                                 (flen (length from)))
                            (when (and (> flen 0) (<= (+ i flen) n)
                                       (string= s from :start1 i :end1 (+ i flen)))
                              (write-string (%php-stringify (cdr pair)) out)
                              (incf i flen) (setf matched t) (return))))
                        (unless matched (write-char (char s i) out) (incf i))))))))
      ;; strtr(str, from, to) — per-character translation
      ((= (length args) 2)
       (let* ((from (%php-stringify (first args)))
              (to (%php-stringify (second args)))
              (len (min (length from) (length to))))
         (map 'string
              (lambda (c)
                (let ((pos (position c from :end len)))
                  (if (and pos (< pos len)) (char to pos) c)))
              s)))
      (t s))))

(defun %php-strpbrk (str char-list)
  "PHP strpbrk: return substring from the first occurrence of any char in CHAR-LIST."
  (let* ((s (%php-stringify str))
         (chars (%php-stringify char-list))
         (pos (position-if (lambda (c) (find c chars)) s)))
    (if pos (subseq s pos) nil)))

(defun %php-strspn (subject mask &optional (start 0) length)
  "PHP strspn: length of the initial segment of SUBJECT consisting only of
characters in MASK."
  (let* ((s (%php-stringify subject))
         (m (%php-stringify mask))
         (begin (if (< start 0) (max 0 (+ (length s) start)) (min start (length s))))
         (end (if (and length (not (%php-null-p length)))
                  (min (length s) (+ begin (if (< length 0) (+ (length s) length) length)))
                  (length s)))
         (count 0))
    (loop for i from begin below end
          while (find (char s i) m)
          do (incf count))
    count))

(defun %php-strcspn (subject mask &optional (start 0) length)
  "PHP strcspn: length of the initial segment of SUBJECT NOT containing any
character in MASK."
  (let* ((s (%php-stringify subject))
         (m (%php-stringify mask))
         (begin (if (< start 0) (max 0 (+ (length s) start)) (min start (length s))))
         (end (if (and length (not (%php-null-p length)))
                  (min (length s) (+ begin (if (< length 0) (+ (length s) length) length)))
                  (length s)))
         (count 0))
    (loop for i from begin below end
          until (find (char s i) m)
          do (incf count))
    count))

(defun %php-quotemeta (str)
  "PHP quotemeta: backslash-escape regex metacharacters . \\ + * ? [ ^ ] $ ( )."
  (with-output-to-string (out)
    (loop for c across (%php-stringify str)
          do (when (member c '(#\. #\\ #\+ #\* #\? #\[ #\^ #\] #\$ #\( #\)))
               (write-char #\\ out))
             (write-char c out))))

(defun %php-htmlentities (str &rest args)
  "PHP htmlentities: alias to htmlspecialchars (covers the common &<>\"' entities)."
  (declare (ignore args))
  (%php-htmlspecialchars str))

(defun %php-metaphone (str &optional phonemes)
  "PHP metaphone: simplified phonetic key (uppercase consonant skeleton)."
  (declare (ignore phonemes))
  (let ((s (string-upcase (%php-stringify str))))
    (remove-if-not (lambda (c) (and (alpha-char-p c) (not (find c "AEIOU")))) s)))

(defun %php-nl-langinfo (item)
  "PHP nl_langinfo: locale information (stub returning empty string)."
  (declare (ignore item))
  "")
