;;;; packages/javascript/src/runtime-date.lisp — JS Date object (ES2026)
;;;;
;;;; Date is represented as a struct wrapping a CL universal-time (seconds
;;;; since 1900-01-01 00:00:00 UTC). JS timestamps are milliseconds since the
;;;; Unix epoch (1970-01-01 00:00:00 UTC).
;;;;
;;;; CL universal-time epoch: 1900-01-01T00:00:00Z
;;;; JS epoch:                1970-01-01T00:00:00Z
;;;; Offset: 2208988800 seconds (70 years including leap years)

(in-package :cl-cc/javascript)

(defconstant +js-epoch-offset+ 2208988800
  "Seconds from CL universal-time epoch (1900) to Unix epoch (1970).")

;;; -----------------------------------------------------------------------
;;;  js-date struct
;;; -----------------------------------------------------------------------

(defstruct (js-date (:conc-name js-date-))
  (ms 0 :type integer))   ; milliseconds since Unix epoch (may be negative)

(defun %js-date-p (x) (js-date-p x))

;;; -----------------------------------------------------------------------
;;;  Constructors
;;; -----------------------------------------------------------------------

(defun %js-date-now ()
  "Return current time as milliseconds since the Unix epoch (Date.now())."
  (- (get-universal-time) +js-epoch-offset+) ; × 1000 omitted for simplicity
  (* 1000 (- (get-universal-time) +js-epoch-offset+)))

(defun %js-make-date (&optional (arg +js-undefined+))
  "Construct a JS Date object.
  - No args or undefined → current time
  - Number → Unix epoch milliseconds
  - String → parsed ISO-8601 date (simplified)
  - Another Date → copy"
  (cond
    ((or (eq arg +js-undefined+) (eq arg +js-null+))
     (make-js-date :ms (%js-date-now)))
    ((js-date-p arg)
     (make-js-date :ms (js-date-ms arg)))
    ((numberp arg)
     (make-js-date :ms (truncate arg)))
    ((stringp arg)
     (make-js-date :ms (%js-date-parse-string arg)))
    (t (make-js-date :ms (%js-date-now)))))

(defun %js-date-parse-string (s)
  "Parse a date string to milliseconds. Supports 'YYYY-MM-DD' and 'YYYY-MM-DDTHH:MM:SS'."
  (handler-case
      (let* ((trimmed (string-trim '(#\Space) s))
             ;; Extract year, month, day from YYYY-MM-DD prefix
             (year  (parse-integer (subseq trimmed 0 4)))
             (month (parse-integer (subseq trimmed 5 7)))
             (day   (parse-integer (subseq trimmed 8 10)))
             (hour  (if (>= (length trimmed) 13) (parse-integer (subseq trimmed 11 13)) 0))
             (min   (if (>= (length trimmed) 16) (parse-integer (subseq trimmed 14 16)) 0))
             (sec   (if (>= (length trimmed) 19) (parse-integer (subseq trimmed 17 19)) 0))
             ;; CL encode-universal-time: sec min hour day month year timezone(0=UTC)
             (ut (encode-universal-time sec min hour day month year 0)))
        (* 1000 (- ut +js-epoch-offset+)))
    (error () (%js-date-now))))

;;; -----------------------------------------------------------------------
;;;  Date.prototype accessors — all operate on the local time
;;; -----------------------------------------------------------------------

(defun %js-date-to-decoded (date)
  "Decode a js-date to (sec min hour day month year day-of-week)."
  (let ((ut (+ (floor (js-date-ms date) 1000) +js-epoch-offset+)))
    (multiple-value-list (decode-universal-time ut 0))))  ; UTC

(defmacro define-js-date-getter (name index &optional (scale 1) (offset 0))
  `(defun ,name (date)
     (* ,scale (+ ,offset (nth ,index (%js-date-to-decoded date))))))

;;; Date.prototype.getFullYear / getUTCFullYear
(define-js-date-getter %js-date-get-full-year    5)
(define-js-date-getter %js-date-get-utc-full-year 5)
;;; Date.prototype.getMonth (0-based)
(define-js-date-getter %js-date-get-month         4 1 -1)  ; CL months 1-12 → JS 0-11
(define-js-date-getter %js-date-get-utc-month     4 1 -1)
;;; Date.prototype.getDate (day of month, 1-based)
(define-js-date-getter %js-date-get-date          3)
(define-js-date-getter %js-date-get-utc-date      3)
;;; Date.prototype.getDay (day of week, 0=Sun)
(define-js-date-getter %js-date-get-day           6)
(define-js-date-getter %js-date-get-utc-day       6)
;;; Date.prototype.getHours / getMinutes / getSeconds
(define-js-date-getter %js-date-get-hours         2)
(define-js-date-getter %js-date-get-utc-hours     2)
(define-js-date-getter %js-date-get-minutes       1)
(define-js-date-getter %js-date-get-utc-minutes   1)
(define-js-date-getter %js-date-get-seconds       0)
(define-js-date-getter %js-date-get-utc-seconds   0)

(defun %js-date-get-milliseconds (date)
  (mod (js-date-ms date) 1000))

(defun %js-date-get-time (date)
  "Date.prototype.getTime() → milliseconds since Unix epoch."
  (coerce (js-date-ms date) 'double-float))

(defun %js-date-get-timezone-offset (date)
  "Date.prototype.getTimezoneOffset() → 0 (UTC assumed)."
  (declare (ignore date))
  0.0d0)

;;; -----------------------------------------------------------------------
;;;  Date.prototype setters
;;; -----------------------------------------------------------------------

(defun %js-date-set-time (date ms)
  (setf (js-date-ms date) (truncate ms))
  ms)

(defun %js-date-set-full-year (date year &optional month day)
  (let* ((decoded (%js-date-to-decoded date))
         (m (if month (+ month 1) (nth 4 decoded)))   ; JS 0-based → CL 1-based
         (d (or day (nth 3 decoded)))
         (ut (encode-universal-time 0 0 0 d m year 0)))
    (setf (js-date-ms date) (* 1000 (- ut +js-epoch-offset+)))
    (coerce (js-date-ms date) 'double-float)))

;;; -----------------------------------------------------------------------
;;;  Date.prototype string representations
;;; -----------------------------------------------------------------------

(defun %js-date-to-iso-string (date)
  "Date.prototype.toISOString() → 'YYYY-MM-DDTHH:MM:SS.mmmZ'."
  (let* ((decoded (%js-date-to-decoded date))
         (sec (nth 0 decoded)) (min (nth 1 decoded)) (hour (nth 2 decoded))
         (day (nth 3 decoded)) (month (nth 4 decoded)) (year (nth 5 decoded))
         (ms  (mod (js-date-ms date) 1000)))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.~3,'0DZ"
            year month day hour min sec ms)))

(defun %js-date-to-string (date)
  "Date.prototype.toString() — simplified."
  (%js-date-to-iso-string date))

(defun %js-date-to-local-date-string (date)
  "Date.prototype.toLocaleDateString() — simplified YYYY/MM/DD."
  (let* ((decoded (%js-date-to-decoded date))
         (day (nth 3 decoded)) (month (nth 4 decoded)) (year (nth 5 decoded)))
    (format nil "~4,'0D/~2,'0D/~2,'0D" year month day)))

(defun %js-date-to-utc-string (date)
  (%js-date-to-iso-string date))

(defun %js-date-value-of (date)
  (%js-date-get-time date))

;;; -----------------------------------------------------------------------
;;;  Date method dispatch table
;;; -----------------------------------------------------------------------

(defparameter *js-date-method-table*
  (list
   (cons "getTime"             #'%js-date-get-time)
   (cons "getFullYear"         #'%js-date-get-full-year)
   (cons "getUTCFullYear"      #'%js-date-get-utc-full-year)
   (cons "getMonth"            #'%js-date-get-month)
   (cons "getUTCMonth"         #'%js-date-get-utc-month)
   (cons "getDate"             #'%js-date-get-date)
   (cons "getUTCDate"          #'%js-date-get-utc-date)
   (cons "getDay"              #'%js-date-get-day)
   (cons "getUTCDay"           #'%js-date-get-utc-day)
   (cons "getHours"            #'%js-date-get-hours)
   (cons "getUTCHours"         #'%js-date-get-utc-hours)
   (cons "getMinutes"          #'%js-date-get-minutes)
   (cons "getUTCMinutes"       #'%js-date-get-utc-minutes)
   (cons "getSeconds"          #'%js-date-get-seconds)
   (cons "getUTCSeconds"       #'%js-date-get-utc-seconds)
   (cons "getMilliseconds"     #'%js-date-get-milliseconds)
   (cons "getTimezoneOffset"   #'%js-date-get-timezone-offset)
   (cons "setTime"             #'%js-date-set-time)
   (cons "setFullYear"         #'%js-date-set-full-year)
   (cons "toISOString"         #'%js-date-to-iso-string)
   (cons "toLocaleDateString"  #'%js-date-to-local-date-string)
   (cons "toUTCString"         #'%js-date-to-utc-string)
   (cons "toString"            #'%js-date-to-string)
   (cons "valueOf"             #'%js-date-value-of))
  "Alist of Date.prototype method name -> host function.")
