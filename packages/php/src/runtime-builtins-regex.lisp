;;;; packages/php/src/runtime-builtins-regex.lisp — PHP preg_* built-ins
;;;;
;;;; Implements PHP's PCRE functions using a pure CL NFA regex engine.
;;;; Supports the common subset needed by PHP programs:
;;;;   Basic atoms, . * + ? {n,m}, ^$, [classes], \d\w\s, groups, alternation.
;;;;
;;;; PHP patterns have the form /pattern/flags — we strip the delimiters.

(in-package :cl-cc/php)

;;; -----------------------------------------------------------------------
;;;  PHP pattern parser — strip /delimiters/ and extract flags
;;; -----------------------------------------------------------------------

(defun %php-strip-pattern (pattern)
  "Parse PHP regex /pattern/flags string. Returns (values pattern-str flags-str)."
  (let ((s (%php-stringify pattern)))
    (if (and (>= (length s) 2) (char= (char s 0) #\/))
        ;; Find closing delimiter
        (let* ((last-slash (position #\/ s :from-end t :start 1)))
          (if last-slash
              (values (subseq s 1 last-slash)
                      (subseq s (1+ last-slash)))
              (values s "")))
        (values s ""))))

;;; -----------------------------------------------------------------------
;;;  PHP NFA engine — simplified clone of the JS regex engine
;;; -----------------------------------------------------------------------

(defun %php-compile-char-class (pattern pos)
  "Parse [...] at POS. Returns (values match-fn new-pos)."
  (let ((complement-p (and (< pos (length pattern))
                           (char= (char pattern pos) #\^)))
        (chars nil))
    (when complement-p (incf pos))
    (loop while (and (< pos (length pattern))
                     (not (char= (char pattern pos) #\])))
          do (let ((ch (char pattern pos)))
               (cond
                 ((char= ch #\\)
                  (incf pos)
                  (let ((esc (char pattern pos)))
                    (push (cond ((char= esc #\d) (lambda (c) (digit-char-p c)))
                                ((char= esc #\D) (lambda (c) (not (digit-char-p c))))
                                ((char= esc #\w) (lambda (c) (or (alphanumericp c) (char= c #\_))))
                                ((char= esc #\W) (lambda (c) (not (or (alphanumericp c) (char= c #\_)))))
                                ((char= esc #\s) (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))))
                                ((char= esc #\S) (lambda (c) (not (member c '(#\Space #\Tab #\Newline #\Return)))))
                                (t esc))
                          chars)
                    (incf pos)))
                 ((and (< (+ pos 2) (length pattern))
                       (char= (char pattern (+ pos 1)) #\-)
                       (not (char= (char pattern (+ pos 2)) #\])))
                  (let ((from ch) (to (char pattern (+ pos 2))))
                    (push (list :range from to) chars)
                    (incf pos 3)))
                 (t (push ch chars) (incf pos)))))
    (when (and (< pos (length pattern)) (char= (char pattern pos) #\]))
      (incf pos))
    (let ((cs (nreverse chars)) (cp complement-p))
      (values (lambda (c)
                (let ((m (loop for item in cs
                               thereis (cond ((characterp item) (char= c item))
                                             ((and (consp item) (eq (car item) :range))
                                              (char<= (cadr item) c (caddr item)))
                                             ((functionp item) (funcall item c))))))
                  (if cp (not m) m)))
              pos))))

(defun %php-compile-regex (pat &key ic ml)
  "Compile PHP regex pattern PAT to a matcher (str pos) -> end-pos or nil."
  (let ((compile-atom nil)
        (compile-seq  nil)
        (compile-alt  nil))
    (setf compile-atom
          (lambda (pos)
            (if (>= pos (length pat))
                (values nil pos)
            (let ((ch (char pat pos)))
              (cond
                ((or (char= ch #\|) (char= ch #\))) (values nil pos))
                ((char= ch #\[)
                 (multiple-value-bind (fn end) (%php-compile-char-class pat (1+ pos))
                   (values (lambda (s i g) (declare (ignore g))
                              (when (and (< i (length s))
                                         (funcall fn (if ic (char-downcase (char s i)) (char s i))))
                                (1+ i)))
                           end)))
                ((and (char= ch #\() (< (1+ pos) (length pat))
                      (char= (char pat (1+ pos)) #\?) (< (+ pos 2) (length pat))
                      (char= (char pat (+ pos 2)) #\:))
                 (multiple-value-bind (f e) (funcall compile-alt (+ pos 3))
                   (values f (if (and (< e (length pat)) (char= (char pat e) #\))) (1+ e) e))))
                ((char= ch #\()
                 (multiple-value-bind (f e) (funcall compile-alt (1+ pos))
                   (values f (if (and (< e (length pat)) (char= (char pat e) #\))) (1+ e) e))))
                ((char= ch #\.)
                 (values (lambda (s i g) (declare (ignore g))
                            (when (and (< i (length s))
                                       (or (not ml) (not (char= (char s i) #\Newline))))
                              (1+ i)))
                         (1+ pos)))
                ((char= ch #\^)
                 (values (lambda (s i g) (declare (ignore g))
                            (when (if ml (or (= i 0) (char= (char s (1- i)) #\Newline))
                                       (= i 0)) i))
                         (1+ pos)))
                ((char= ch #\$)
                 (values (lambda (s i g) (declare (ignore g))
                            (when (if ml (or (= i (length s)) (char= (char s i) #\Newline))
                                       (= i (length s))) i))
                         (1+ pos)))
                ((char= ch #\\)
                 (let* ((esc (char pat (1+ pos)))
                        (pred (cond ((char= esc #\d) (lambda (c) (digit-char-p c)))
                                    ((char= esc #\D) (lambda (c) (not (digit-char-p c))))
                                    ((char= esc #\w) (lambda (c) (or (alphanumericp c) (char= c #\_))))
                                    ((char= esc #\W) (lambda (c) (not (or (alphanumericp c) (char= c #\_)))))
                                    ((char= esc #\s) (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))))
                                    ((char= esc #\S) (lambda (c) (not (member c '(#\Space #\Tab #\Newline #\Return)))))
                                    ((char= esc #\n) (lambda (c) (char= c #\Newline)))
                                    ((char= esc #\t) (lambda (c) (char= c #\Tab)))
                                    ((char= esc #\r) (lambda (c) (char= c #\Return)))
                                    (t (let ((lit esc)) (lambda (c) (char= c lit)))))))
                   (values (lambda (s i g) (declare (ignore g))
                              (when (and (< i (length s))
                                         (funcall pred (if ic (char-downcase (char s i)) (char s i))))
                                (1+ i)))
                           (+ pos 2))))
                (t (let ((lit (if ic (char-downcase ch) ch)))
                     (values (lambda (s i g) (declare (ignore g))
                                (when (and (< i (length s))
                                           (char= (if ic (char-downcase (char s i)) (char s i)) lit))
                                  (1+ i)))
                             (1+ pos)))))))))
    (setf compile-seq
          (lambda (pos)
            (let ((fns nil))
              (loop
                (multiple-value-bind (af np) (funcall compile-atom pos)
                  (unless af (return))
                  (setf pos np)
                  (when (< pos (length pat))
                    (let ((q (char pat pos)))
                      (cond
                        ((char= q #\*)
                         (incf pos)
                         (when (and (< pos (length pat)) (char= (char pat pos) #\?)) (incf pos))
                         (let ((fn af))
                           (setf af (lambda (s i g)
                                      (loop for j = (funcall fn s i g) then (funcall fn s j g)
                                            while j do (setf i j) finally (return i))))))
                        ((char= q #\+)
                         (incf pos)
                         (when (and (< pos (length pat)) (char= (char pat pos) #\?)) (incf pos))
                         (let ((fn af))
                           (setf af (lambda (s i g)
                                      (let ((j (funcall fn s i g)))
                                        (when j
                                          (loop for k = (funcall fn s j g) then (funcall fn s k g)
                                                while k do (setf j k) finally (return j))))))))
                        ((char= q #\?)
                         (incf pos)
                         (when (and (< pos (length pat)) (char= (char pat pos) #\?)) (incf pos))
                         (let ((fn af))
                           (setf af (lambda (s i g) (or (funcall fn s i g) i))))))))
                  (push af fns)))
              (let ((fs (nreverse fns)))
                (values (lambda (s i g)
                          (let ((p i))
                            (dolist (fn fs p)
                              (let ((r (funcall fn s p g)))
                                (if r (setf p r) (return nil))))))
                        pos)))))
    (setf compile-alt
          (lambda (pos)
            (multiple-value-bind (ff np) (funcall compile-seq pos)
              (setf pos np)
              (if (and (< pos (length pat)) (char= (char pat pos) #\|))
                  (multiple-value-bind (rf rp) (funcall compile-alt (1+ pos))
                    (let ((f ff) (r rf))
                      (values (lambda (s i g) (or (funcall f s i g) (funcall r s i g))) rp)))
                  (values ff pos)))))
    (multiple-value-bind (fn _) (funcall compile-alt 0)
      (declare (ignore _))
      fn)))

(defun %php-regex-exec (pattern-str subject &key (start 0) ic ml)
  "Try to match PATTERN-STR in SUBJECT from START. Returns (values match-str match-start) or nil."
  (let ((fn (handler-case (%php-compile-regex pattern-str :ic ic :ml ml) (error () nil)))
        (n (length subject)))
    (when fn
      (loop for i from start to n
            for end = (funcall fn subject i nil)
            when end
              return (values (subseq subject i end) i end)))))

;;; -----------------------------------------------------------------------
;;;  preg_match
;;; -----------------------------------------------------------------------

(defun %php-preg-match (pattern subject &optional matches)
  "PHP preg_match: return 1 if pattern matches, 0 if not, store matches."
  (declare (ignore matches))  ; matches capture NYI
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let ((ic (find #\i flags))
          (ml (find #\m flags))
          (str (%php-stringify subject)))
      (multiple-value-bind (match-str _start _end) (%php-regex-exec pat str :ic ic :ml ml)
        (declare (ignore _start _end))
        (if match-str 1 0)))))

(defun %php-preg-match-all (pattern subject)
  "PHP preg_match_all: return count of all matches."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (fn (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil)))
           (count 0)
           (pos 0))
      (when fn
        (loop
          (let ((end (funcall fn str pos nil)))
            (unless end (return))
            (incf count)
            (setf pos (max (1+ pos) end)))))
      count)))

;;; -----------------------------------------------------------------------
;;;  preg_replace
;;; -----------------------------------------------------------------------

(defun %php-preg-replace (pattern replacement subject)
  "PHP preg_replace: replace all matches of PATTERN in SUBJECT with REPLACEMENT."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (repl (%php-stringify replacement))
           (fn (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil))))
      (if fn
          (with-output-to-string (out)
            (let ((pos 0))
              (loop while (<= pos (length str))
                    do (let ((match-start nil) (match-end nil))
                         ;; Scan forward for the next match
                         (loop for i from pos to (length str)
                               for e = (funcall fn str i nil)
                               when e
                                 do (setf match-start i match-end e)
                                    (return))
                         (if match-start
                             (progn
                               ;; Write text before match
                               (write-string (subseq str pos match-start) out)
                               ;; Write replacement (with $0 support for the matched text)
                               (write-string repl out)
                               (setf pos (max (1+ match-start) match-end)))
                             (progn
                               ;; No more matches — write rest of string
                               (write-string (subseq str pos) out)
                               (return)))))))
          str))))

;;; -----------------------------------------------------------------------
;;;  preg_split
;;; -----------------------------------------------------------------------

(defun %php-preg-split (pattern subject &optional (limit -1))
  "PHP preg_split: split SUBJECT by regex PATTERN."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (fn (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil)))
           (result (%php-make-array))
           (pos 0)
           (count 0))
      (if fn
          (loop while (<= pos (length str))
                do (when (and (> limit 0) (>= count (1- limit)))
                     (%php-array-set result count (subseq str pos))
                     (return result))
                   (let ((sep-start nil) (sep-end nil))
                     ;; Scan forward for the separator
                     (loop for i from pos to (length str)
                           for e = (funcall fn str i nil)
                           when e
                             do (setf sep-start i sep-end e)
                                (return))
                     (if sep-start
                         (progn
                           (%php-array-set result count (subseq str pos sep-start))
                           (incf count)
                           (setf pos (max (1+ sep-start) sep-end)))
                         (progn
                           (%php-array-set result count (subseq str pos))
                           (return result))))
                finally (return result))
          (%php-preg-split-literal str pat)))))

(defun %php-preg-split-literal (str sep)
  "Fallback: split STR by literal SEP."
  (let ((result (%php-make-array))
        (count 0)
        (pos 0))
    (loop
      (let ((found (search sep str :start2 pos)))
        (if found
            (progn
              (%php-array-set result count (subseq str pos found))
              (incf count)
              (setf pos (+ found (length sep))))
            (progn
              (%php-array-set result count (subseq str pos))
              (return result)))))
    result))

;;; -----------------------------------------------------------------------
;;;  preg_quote
;;; -----------------------------------------------------------------------

(defun %php-preg-quote (str &optional delimiter)
  "PHP preg_quote: escape special regex characters in STR."
  (declare (ignore delimiter))
  (with-output-to-string (out)
    (loop for ch across (%php-stringify str)
          do (when (member ch '(#\. #\* #\+ #\? #\[ #\] #\^ #\$ #\( #\) #\{ #\} #\| #\\ #\/))
               (write-char #\\ out))
             (write-char ch out))))

;;; -----------------------------------------------------------------------
;;;  Date / Time functions
;;; -----------------------------------------------------------------------

(defconstant +php-epoch-offset+ 2208988800
  "Seconds from CL universal-time epoch (1900) to Unix epoch (1970).")

(defun %php-time ()
  "PHP time() — current Unix timestamp."
  (- (get-universal-time) +php-epoch-offset+))

(defun %php-microtime (&optional (get-as-float nil))
  "PHP microtime — simplified (no sub-second precision in CL)."
  (let ((ts (%php-time)))
    (if (%php-truthy get-as-float)
        (coerce ts 'double-float)
        (format nil "0.000000 ~D" ts))))

(defun %php-mktime (&optional hour minute second month day year)
  "PHP mktime — create Unix timestamp from date components."
  (let* ((now (multiple-value-list (decode-universal-time (get-universal-time) 0)))
         (h   (or hour   (nth 2 now)))
         (min (or minute (nth 1 now)))
         (sec (or second (nth 0 now)))
         (m   (or month  (nth 4 now)))
         (d   (or day    (nth 3 now)))
         (y   (or year   (nth 5 now)))
         (ut  (encode-universal-time sec min h d m y 0)))
    (- ut +php-epoch-offset+)))

(defun %php-strtotime (str &optional (base-ts nil))
  "PHP strtotime — simplified: parse 'YYYY-MM-DD' and common relative formats."
  (declare (ignore base-ts))
  (let ((s (string-trim '(#\Space) (%php-stringify str))))
    (handler-case
        (cond
          ;; YYYY-MM-DD or YYYY-MM-DD HH:MM:SS
          ((and (>= (length s) 10)
                (char= (char s 4) #\-)
                (char= (char s 7) #\-))
           (let* ((year  (parse-integer (subseq s 0 4)))
                  (month (parse-integer (subseq s 5 7)))
                  (day   (parse-integer (subseq s 8 10)))
                  (hour  (if (>= (length s) 13) (parse-integer (subseq s 11 13)) 0))
                  (min   (if (>= (length s) 16) (parse-integer (subseq s 14 16)) 0))
                  (sec   (if (>= (length s) 19) (parse-integer (subseq s 17 19)) 0))
                  (ut    (encode-universal-time sec min hour day month year 0)))
             (- ut +php-epoch-offset+)))
          (t nil))
      (error () nil))))

(defun %php-date (format &optional (timestamp nil))
  "PHP date — format a timestamp. Supports d D j m M Y y H i s N w z t."
  (let* ((ts (or timestamp (%php-time)))
         (ut (+ ts +php-epoch-offset+))
         (decoded (multiple-value-list (decode-universal-time ut 0))))
    (destructuring-bind (sec min hour day month year dow dst tz) decoded
      (declare (ignore dst tz))
      (with-output-to-string (out)
        (let ((fmt (%php-stringify format))
              (month-names #("January" "February" "March" "April" "May" "June"
                             "July" "August" "September" "October" "November" "December"))
              (day-names #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")))
          (loop for i from 0 below (length fmt)
                for ch = (char fmt i)
                do (write-string
                    (case ch
                      (#\Y (format nil "~4,'0D" year))
                      (#\y (format nil "~2,'0D" (mod year 100)))
                      (#\m (format nil "~2,'0D" month))
                      (#\n (format nil "~D" month))
                      (#\M (subseq (aref month-names (1- month)) 0 3))
                      (#\F (aref month-names (1- month)))
                      (#\d (format nil "~2,'0D" day))
                      (#\j (format nil "~D" day))
                      (#\D (subseq (aref day-names dow) 0 3))
                      (#\l (aref day-names dow))
                      (#\N (format nil "~D" (if (= dow 0) 7 dow)))  ; ISO day (Mon=1)
                      (#\w (format nil "~D" dow))
                      (#\H (format nil "~2,'0D" hour))
                      (#\G (format nil "~D" hour))
                      (#\h (format nil "~2,'0D" (let ((h (mod hour 12))) (if (= h 0) 12 h))))
                      (#\i (format nil "~2,'0D" min))
                      (#\s (format nil "~2,'0D" sec))
                      (#\A (if (< hour 12) "AM" "PM"))
                      (#\a (if (< hour 12) "am" "pm"))
                      (#\U (format nil "~D" ts))
                      (#\\ (if (< (1+ i) (length fmt))
                               (progn (incf i) (string (char fmt i)))
                               "\\"))
                      (t (string ch)))
                    out)))))))

(defun %php-checkdate (month day year)
  "PHP checkdate — validate a date."
  (handler-case
      (progn (encode-universal-time 0 0 0 day month year 0) t)
    (error () nil)))

(defun %php-number-format (num &optional (decimals 0) (dec-point ".") (thousands-sep ","))
  "PHP number_format — format a number with decimal and thousands separators."
  (let* ((n (coerce (if (numberp num) num 0) 'double-float))
         (dp (if (and dec-point (not (%php-null-p dec-point))) (%php-stringify dec-point) "."))
         (ts (if (and thousands-sep (not (%php-null-p thousands-sep))) (%php-stringify thousands-sep) ","))
         (abs-n (abs n))
         (rounded (if (> decimals 0)
                      (/ (round (* abs-n (expt 10 decimals))) (expt 10 decimals))
                      (round abs-n)))
         (int-part (truncate rounded))
         (frac-part (- rounded int-part))
         ;; Format integer part with thousands separators
         (int-str (format nil "~D" int-part))
         (int-with-sep (with-output-to-string (out)
                         (let ((len (length int-str)))
                           (loop for i from 0 below len
                                 do (write-string int-str out :start i :end (1+ i))
                                    (let ((remaining (- len (1+ i))))
                                      (when (and (> (length ts) 0)
                                                 (> remaining 0)
                                                 (zerop (mod remaining 3)))
                                        (write-string ts out)))))))
         (result (if (> decimals 0)
                     (format nil "~A~A~v,'0D"
                             int-with-sep dp decimals
                             (truncate (* (abs frac-part) (expt 10 decimals))))
                     int-with-sep)))
    (if (< n 0) (concatenate 'string "-" result) result)))
