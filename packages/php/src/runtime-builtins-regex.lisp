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
  "Compile PHP regex pattern PAT to a matcher (str pos g) -> end-pos or nil.

Returns (values matcher group-count).  When the matcher is called with a
hash-table G, each capturing group records its matched span as
(start . end) under its 1-based group number — the greedy non-backtracking
engine never has to undo a recorded span, so capture stays simple.  Callers
that don't need groups pass NIL for G."
  (let ((compile-atom nil)
        (compile-seq  nil)
        (compile-alt  nil)
        (group-count  0))
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
                 ;; Capturing group — allocate its 1-based number BEFORE
                 ;; compiling the inner pattern so nested groups number
                 ;; left-to-right by opening paren, as PHP does.
                 (let ((gnum (incf group-count)))
                   (multiple-value-bind (f e) (funcall compile-alt (1+ pos))
                     (values (lambda (s i g)
                               (let ((end (funcall f s i g)))
                                 (when end
                                   (when g (setf (gethash gnum g) (cons i end)))
                                   end)))
                             (if (and (< e (length pat)) (char= (char pat e) #\))) (1+ e) e)))))
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
      (values fn group-count))))

(defun %php-regex-match-at (fn group-count s i)
  "Run matcher FN (from %php-compile-regex) at position I of S with capture.
Return (values match-end groups) on success, NIL on failure.  GROUPS is a
vector indexed by group number: index 0 = the full match (i . end), 1..N = each
capturing group's (start . end) span or NIL when the group did not participate."
  (let* ((g (make-hash-table))
         (end (funcall fn s i g)))
    (when end
      (let ((groups (make-array (1+ group-count) :initial-element nil)))
        (setf (aref groups 0) (cons i end))
        (loop for k from 1 to group-count
              do (setf (aref groups k) (gethash k g)))
        (values end groups)))))

(defun %php-regex-group-string (groups idx s)
  "Return the substring of S captured by group IDX, or \"\" when absent."
  (let ((span (and (< idx (length groups)) (aref groups idx))))
    (if span (subseq s (car span) (cdr span)) "")))

(defun %php-regex-expand-replacement (repl groups s)
  "Expand $N, ${N} and \\N backreferences in REPL using GROUPS (from
%php-regex-match-at) over subject S.  $0 / \\0 is the whole match.  A reference
to a group that did not match expands to the empty string."
  (with-output-to-string (out)
    (let ((i 0) (n (length repl)))
      (loop while (< i n)
            do (let ((ch (char repl i)))
                 (cond
                   ;; ${N}
                   ((and (char= ch #\$) (< (1+ i) n) (char= (char repl (1+ i)) #\{))
                    (let ((close (position #\} repl :start (+ i 2))))
                      (if close
                          (let ((num (parse-integer repl :start (+ i 2) :end close :junk-allowed t)))
                            (when num (write-string (%php-regex-group-string groups num s) out))
                            (setf i (1+ close)))
                          (progn (write-char ch out) (incf i)))))
                   ;; $N or \N — consume up to two digits, preferring the
                   ;; two-digit group when it exists.
                   ((and (or (char= ch #\$) (char= ch #\\))
                         (< (1+ i) n) (digit-char-p (char repl (1+ i))))
                    (let* ((d1 (digit-char-p (char repl (1+ i))))
                           (d2 (and (< (+ i 2) n) (digit-char-p (char repl (+ i 2)))))
                           (two (and d2 (+ (* 10 d1) d2))))
                      (if (and two (< two (length groups)))
                          (progn (write-string (%php-regex-group-string groups two s) out)
                                 (setf i (+ i 3)))
                          (progn (write-string (%php-regex-group-string groups d1 s) out)
                                 (setf i (+ i 2))))))
                   ;; \\ and other backslash escapes -> the escaped char literally
                   ((and (char= ch #\\) (< (1+ i) n))
                    (write-char (char repl (1+ i)) out) (setf i (+ i 2)))
                   (t (write-char ch out) (incf i))))))))

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
  "PHP preg_match_all: return the number of full matches.

The matcher is anchored at the position it is given, so we must SCAN forward to
the next match rather than only testing the current position — the previous loop
tested fn at successive positions and stopped at the first gap, so it counted
only matches that happened to be consecutive from index 0 (e.g. '1a2b3' / \\d
returned 1 instead of 3)."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (fn (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil)))
           (count 0)
           (pos 0))
      (when fn
        (loop while (<= pos (length str))
              do (let ((match-start nil) (match-end nil))
                   (loop for i from pos to (length str)
                         for e = (funcall fn str i nil)
                         when e do (setf match-start i match-end e) (return))
                   (if match-start
                       (progn (incf count) (setf pos (max (1+ match-start) match-end)))
                       (return)))))
      count)))

(defun %php-preg-match-matches (pattern subject)
  "Return the $matches array for preg_match($pattern,$subject): index 0 = the
full match, k = capture group k (empty string if the group did not match), or an
empty array when PATTERN does not match.  Returned BY VALUE and assigned to the
caller's $matches at the call site — the VM copies host structs across the
bridge, so a mutable ref box would not propagate."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let ((ic (find #\i flags)) (ml (find #\m flags))
          (str (%php-stringify subject))
          (arr (%php-make-array)))
      (multiple-value-bind (fn gcount)
          (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () (values nil 0)))
        (when fn
          (loop for i from 0 to (length str)
                do (multiple-value-bind (end groups) (%php-regex-match-at fn gcount str i)
                     (when end
                       (loop for k from 0 to gcount
                             do (%php-array-set arr k (%php-regex-group-string groups k str)))
                       (return))))))
      arr)))

(defun %php-preg-match-all-matches (pattern subject)
  "Return the $matches array for preg_match_all in PREG_PATTERN_ORDER: index 0 is
an array of every full match, k an array of every capture-group-k match."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let ((ic (find #\i flags)) (ml (find #\m flags))
          (str (%php-stringify subject)))
      (multiple-value-bind (fn gcount)
          (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () (values nil 0)))
        (let ((per-group (make-array (1+ gcount))) (count 0) (pos 0))
          (dotimes (k (1+ gcount)) (setf (aref per-group k) (%php-make-array)))
          (when fn
            (loop while (<= pos (length str))
                  do (let ((ms nil) (me nil) (mg nil))
                       (loop for i from pos to (length str)
                             do (multiple-value-bind (e g) (%php-regex-match-at fn gcount str i)
                                  (when e (setf ms i me e mg g) (return))))
                       (if ms
                           (progn
                             (loop for k from 0 to gcount
                                   do (%php-array-set (aref per-group k) count
                                                      (%php-regex-group-string mg k str)))
                             (incf count)
                             (setf pos (max (1+ ms) me)))
                           (return)))))
          (let ((arr (%php-make-array)))
            (loop for k from 0 to gcount do (%php-array-set arr k (aref per-group k)))
            arr))))))

;;; -----------------------------------------------------------------------
;;;  preg_replace
;;; -----------------------------------------------------------------------

(defun %php-preg-replace (pattern replacement subject)
  "PHP preg_replace: replace all matches of PATTERN in SUBJECT with REPLACEMENT."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (repl (%php-stringify replacement)))
      (multiple-value-bind (fn gcount)
          (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil))
        (if fn
            (with-output-to-string (out)
              (let ((pos 0))
                (loop while (<= pos (length str))
                      do (let ((match-start nil) (match-end nil) (groups nil))
                           ;; Scan forward for the next match, capturing groups.
                           (loop for i from pos to (length str)
                                 do (multiple-value-bind (e g) (%php-regex-match-at fn gcount str i)
                                      (when e (setf match-start i match-end e groups g) (return))))
                           (if match-start
                               (progn
                                 (write-string (subseq str pos match-start) out)
                                 ;; Expand $0/$1/${1}/\1 backreferences in the replacement.
                                 (write-string (%php-regex-expand-replacement repl groups str) out)
                                 (setf pos (max (1+ match-start) match-end)))
                               (progn
                                 (write-string (subseq str pos) out)
                                 (return)))))))
            str)))))

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

(defun %php-round-half-up (x)
  "Round non-negative double X to the nearest integer with halves rounding UP —
PHP's round-half-away-from-zero applied to a magnitude.  CL ROUND uses banker's
rounding (half-to-even), so number_format(2.5) gave 2 and number_format(1234.5)
gave 1,234 instead of PHP's 3 and 1,235."
  (values (floor (+ x 0.5d0))))

(defun %php-number-format (num &optional (decimals 0) (dec-point ".") (thousands-sep ","))
  "PHP number_format — format a number with decimal and thousands separators."
  (let* ((n (coerce (if (numberp num) num 0) 'double-float))
         (dp (if (and dec-point (not (%php-null-p dec-point))) (%php-stringify dec-point) "."))
         (ts (if (and thousands-sep (not (%php-null-p thousands-sep))) (%php-stringify thousands-sep) ","))
         (abs-n (abs n))
         (rounded (if (> decimals 0)
                      (/ (%php-round-half-up (* abs-n (expt 10 decimals))) (expt 10 decimals))
                      (%php-round-half-up abs-n)))
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

;;; ─── preg_replace_callback ────────────────────────────────────────────────────

(defun %php-preg-replace-callback (pattern callback subject &optional (limit -1) count-var)
  "PHP preg_replace_callback: replace regex matches in SUBJECT using CALLBACK.
The callback receives a PHP array whose [0] element is the full match and
returns the replacement string.  Scans like %php-preg-replace (strip the
/.../ delimiters + flags, compile with ic/ml, then scan forward); the NFA
matcher used here exposes only the full match, so the callback's array carries
$matches[0] (the engine does not yet thread capture groups through this path).

The previous implementation called the non-existent %php-regex-search and never
stripped the pattern delimiters, so every call raised
`The function %PHP-REGEX-SEARCH is undefined.'"
  (declare (ignore count-var))
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (cb (%php-callable-function callback))
           (fn (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil)))
           (max-replacements (if (and (numberp limit) (> limit 0)) limit most-positive-fixnum)))
      (if (and fn cb)
          (with-output-to-string (out)
            (let ((pos 0) (replacements 0))
              (loop while (<= pos (length str))
                    do (if (>= replacements max-replacements)
                           (progn (write-string (subseq str pos) out) (return))
                           (let ((match-start nil) (match-end nil))
                             ;; Scan forward for the next match position.
                             (loop for i from pos to (length str)
                                   for e = (funcall fn str i nil)
                                   when e do (setf match-start i match-end e) (return))
                             (if match-start
                                 (let ((match-arr (%php-make-array)))
                                   (write-string (subseq str pos match-start) out)
                                   (%php-array-set match-arr 0 (subseq str match-start match-end))
                                   (write-string (%php-stringify (funcall cb match-arr)) out)
                                   (incf replacements)
                                   (setf pos (max (1+ match-start) match-end)))
                                 (progn (write-string (subseq str pos) out) (return))))))))
          str))))

(defun %php-preg-replace-callback-array (pattern-map subject &optional (limit -1) count)
  "PHP preg_replace_callback_array: PATTERN-MAP is a single PHP array mapping
each regex pattern (key) to its callback (value); apply them in order to
SUBJECT.  The previous signature took separate patterns/callbacks arrays, so a
normal call preg_replace_callback_array(['/re/' => fn], $s) passed too few
arguments."
  (declare (ignore count))
  (let ((result subject))
    (when (hash-table-p pattern-map)
      (dolist (pair (%php-array-pairs pattern-map))
        (setf result (%php-preg-replace-callback (car pair) (cdr pair) result limit))))
    result))

;;; ─── Date helpers ────────────────────────────────────────────────────────────

(defun %php-date-create (&optional (datetime nil))
  "PHP date_create / DateTime::__construct: return a PHP DateTime-like array."
  (let* ((ts (if (or (null datetime) (%php-null-p datetime) (string= (%php-stringify datetime) "now"))
                 (%php-time)
                 (or (%php-strtotime datetime) (%php-time))))
         (obj (%php-make-array)))
    (%php-array-set obj "__class__" "DateTime")
    (%php-array-set obj "timestamp" ts)
    obj))

(defun %php-date-format (obj format)
  "PHP date_format / DateTime::format."
  (let ((ts (%php-array-ref obj "timestamp")))
    (%php-date format (if (%php-null-p ts) (%php-time) ts))))

(defun %php-date-modify (obj modifier)
  "PHP date_modify: modify datetime object."
  (declare (ignore modifier))
  obj)

(defun %php-date-diff (date1 date2 &optional absolute)
  "PHP date_diff: return interval between two DateTime objects."
  (declare (ignore absolute))
  (let* ((ts1 (or (%php-array-ref date1 "timestamp") (%php-time)))
         (ts2 (or (%php-array-ref date2 "timestamp") (%php-time)))
         (diff (abs (- ts2 ts1)))
         (result (%php-make-array)))
    (%php-array-set result "days" (truncate diff 86400))
    (%php-array-set result "h"    (truncate (mod diff 86400) 3600))
    (%php-array-set result "i"    (truncate (mod diff 3600)  60))
    (%php-array-set result "s"    (mod diff 60))
    (%php-array-set result "invert" (if (< ts2 ts1) 1 0))
    result))

(defun %php-date-timestamp (obj)
  "PHP DateTime::getTimestamp."
  (or (%php-array-ref obj "timestamp") (%php-time)))

(defun %php-gmdate (format &optional (timestamp nil))
  "PHP gmdate: same as date() but always UTC."
  (%php-date format timestamp))

(defun %php-date-default-timezone-set (tz)
  "PHP date_default_timezone_set (stub)."
  (declare (ignore tz))
  t)

(defun %php-date-default-timezone-get ()
  "PHP date_default_timezone_get (stub)."
  "UTC")

;;; ─── Array extra ─────────────────────────────────────────────────────────────

(defun %php-array-walk-recursive (array callback &optional extra-data)
  "PHP array_walk_recursive: apply callback to all leaf values."
  (let ((fn (%php-callable-function callback)))
    (when (and fn (hash-table-p array))
      (labels ((walk (arr)
                 (dolist (pair (%php-array-pairs arr))
                   (if (hash-table-p (cdr pair))
                       (walk (cdr pair))
                       (if extra-data
                           (funcall fn (cdr pair) (car pair) extra-data)
                           (funcall fn (cdr pair) (car pair)))))))
        (walk array))))
  t)

(defun %php-array-splice-in-place (array offset &optional length replacement)
  "PHP array_splice modifying the original array."
  (%php-array-splice array offset length replacement))

(defun %php-strnatcmp (s1 s2 &optional case-insensitive)
  "Natural-order comparison of S1 and S2 (PHP strnatcmp/strnatcasecmp).  Returns
-1, 0, or 1.  A run of digits in BOTH strings is compared by numeric value (so
\"img2\" < \"img10\"); other characters compare by code.  Leading whitespace is
skipped, as PHP does."
  (let* ((a (let ((x (%php-stringify s1))) (if case-insensitive (string-downcase x) x)))
         (b (let ((x (%php-stringify s2))) (if case-insensitive (string-downcase x) x)))
         (la (length a)) (lb (length b))
         (i 0) (j 0))
    (loop
      (loop while (and (< i la) (member (char a i) '(#\Space #\Tab #\Newline #\Return))) do (incf i))
      (loop while (and (< j lb) (member (char b j) '(#\Space #\Tab #\Newline #\Return))) do (incf j))
      (cond
        ((and (>= i la) (>= j lb)) (return 0))
        ((>= i la) (return -1))
        ((>= j lb) (return 1)))
      (let ((ca (char a i)) (cb (char b j)))
        (if (and (digit-char-p ca) (digit-char-p cb))
            (let ((di i) (dj j))
              (loop while (and (< di la) (digit-char-p (char a di))) do (incf di))
              (loop while (and (< dj lb) (digit-char-p (char b dj))) do (incf dj))
              (let ((na (parse-integer a :start i :end di))
                    (nb (parse-integer b :start j :end dj)))
                (cond ((< na nb) (return -1))
                      ((> na nb) (return 1))
                      (t (setf i di j dj)))))
            (cond ((char< ca cb) (return -1))
                  ((char> ca cb) (return 1))
                  (t (incf i) (incf j))))))))

(defun %php-strnatcasecmp (s1 s2)
  "PHP strnatcasecmp: case-insensitive natural-order string comparison."
  (%php-strnatcmp s1 s2 t))

(defun %php-natural-sort-in-place (array case-insensitive)
  "Sort ARRAY's values in natural order IN PLACE, preserving keys. Returns T."
  (let ((pairs (stable-sort (copy-list (%php-array-pairs array))
                            (lambda (p1 p2)
                              (< (%php-strnatcmp (cdr p1) (cdr p2) case-insensitive) 0)))))
    (clrhash array)
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (dolist (pair pairs t)
      (%php-array-set array (car pair) (cdr pair)))))

(defun %php-natsort (array)
  "PHP natsort: natural-order sort of values, preserving keys."
  (%php-natural-sort-in-place array nil))

(defun %php-natcasesort (array)
  "PHP natcasesort: case-insensitive natural-order sort, preserving keys."
  (%php-natural-sort-in-place array t))

(defun %php-array-map-null (array1 &rest arrays)
  "PHP array_map with null callback: zip arrays into array-of-arrays."
  (let ((result (%php-make-array))
        (lists (mapcar #'%php-array-values-list (cons array1 arrays)))
        (i 0))
    (loop while (every #'consp lists)
          do (let ((row (%php-make-array)))
               (loop for lst in lists for j from 0
                     do (%php-array-set row j (car lst)))
               (%php-array-set result i row)
               (incf i)
               (setf lists (mapcar #'cdr lists))))
    result))
