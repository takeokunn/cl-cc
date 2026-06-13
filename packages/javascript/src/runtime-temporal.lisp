;;;; packages/javascript/src/runtime-temporal.lisp — Temporal API (ES2026 Stage 4)
;;;;
;;;; Temporal replaces the Date API with a comprehensive, immutable datetime library.
;;;; We implement the 8 core types as hash-table objects with the standard methods.
;;;; Arithmetic operations use CL's arithmetic on universal-time seconds.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Shared helpers
;;; -----------------------------------------------------------------------

(defun %temporal-epoch-offset () 2208988800)  ; CL epoch → Unix epoch

(defun %temporal-now-unix-seconds ()
  (- (get-universal-time) (%temporal-epoch-offset)))

(defun %temporal-decode (unix-seconds)
  "Decode Unix seconds to (values sec min hour day month year dow)."
  (let ((ut (+ unix-seconds (%temporal-epoch-offset))))
    (multiple-value-bind (s mn h d m y dow) (decode-universal-time ut 0)
      (values s mn h d m y dow))))

(defun %temporal-encode (year month day &optional (hour 0) (min 0) (sec 0))
  (- (encode-universal-time sec min hour day month year 0)
     (%temporal-epoch-offset)))

(defun %temporal-pad (n width)
  (format nil "~v,'0D" width n))

(defun %temporal-3way-compare (a b)
  "Return -1.0d0, 0.0d0, or 1.0d0 for ordered comparison of numeric A and B."
  (cond ((< a b) -1.0d0) ((> a b) 1.0d0) (t 0.0d0)))

(defun %temporal-parse-time-fields (s)
  "Parse a time string \"HH:MM:SS\" into (values hour minute second)."
  (flet ((field (start end) (if (>= (length s) end) (parse-integer s :start start :end end) 0)))
    (values (field 0 2) (field 3 5) (field 6 8))))

;;; -----------------------------------------------------------------------
;;;  Temporal.Now
;;; -----------------------------------------------------------------------

(defun %js-temporal-now ()
  (%js-make-object
   "instant"         (lambda () (%js-temporal-instant (%temporal-now-unix-seconds)))
   "plainDateTimeISO" (lambda (&optional _tz)
                        (declare (ignore _tz))
                        (multiple-value-bind (s mn h d m y) (%temporal-decode (%temporal-now-unix-seconds))
                          (%js-temporal-plain-datetime y m d h mn s)))
   "plainDateISO"    (lambda (&optional _tz)
                        (declare (ignore _tz))
                        (multiple-value-bind (s mn h d m y) (%temporal-decode (%temporal-now-unix-seconds))
                          (declare (ignore s mn h))
                          (%js-temporal-plain-date y m d)))
   "plainTimeISO"    (lambda (&optional _tz)
                        (declare (ignore _tz))
                        (multiple-value-bind (s mn h) (%temporal-decode (%temporal-now-unix-seconds))
                          (%js-temporal-plain-time h mn s)))
   "zonedDateTimeISO" (lambda (&optional _tz)
                        (declare (ignore _tz))
                        (multiple-value-bind (s mn h d m y) (%temporal-decode (%temporal-now-unix-seconds))
                          (%js-temporal-zoned-datetime y m d h mn s "UTC")))
   "timeZoneId"      (lambda () "UTC")))

;;; -----------------------------------------------------------------------
;;;  Temporal.Instant
;;; -----------------------------------------------------------------------

(defun %js-temporal-instant (unix-seconds)
  (let ((ts unix-seconds))
    (%js-make-object
     "__type__"         "Temporal.Instant"
     "epochSeconds"     (coerce ts 'double-float)
     "epochMilliseconds" (coerce (* ts 1000) 'double-float)
     "epochMicroseconds" (coerce (* ts 1000000) 'double-float)
     "epochNanoseconds"  (coerce (* ts 1000000000) 'double-float)
     "toString"          (lambda (&optional _opts) (declare (ignore _opts))
                           (multiple-value-bind (s mn h d m y) (%temporal-decode ts)
                             (format nil "~A-~A-~AT~A:~A:~AZ"
                                     (%temporal-pad y 4) (%temporal-pad m 2) (%temporal-pad d 2)
                                     (%temporal-pad h 2) (%temporal-pad mn 2) (%temporal-pad s 2))))
     "toZonedDateTimeISO" (lambda (&optional _tz) (declare (ignore _tz))
                            (multiple-value-bind (s mn h d m y) (%temporal-decode ts)
                              (%js-temporal-zoned-datetime y m d h mn s "UTC")))
     "add"               (lambda (duration)
                           (%js-temporal-instant (+ ts (%temporal-duration-to-seconds duration))))
     "subtract"          (lambda (duration)
                           (%js-temporal-instant (- ts (%temporal-duration-to-seconds duration))))
     "equals"            (lambda (other)
                           (= ts (or (gethash "epochSeconds" other) 0)))
     "compare"           (lambda (other)
                           (%temporal-3way-compare ts (or (gethash "epochSeconds" other) 0))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.PlainDate
;;; -----------------------------------------------------------------------

(defun %js-temporal-plain-date (year month day)
  (let ((y year) (m month) (d day))
    (%js-make-object
     "__type__"   "Temporal.PlainDate"
     "year"       (coerce y 'double-float)
     "month"      (coerce m 'double-float)
     "day"        (coerce d 'double-float)
     "calendarId" "iso8601"
     "dayOfWeek"  (coerce (nth-value 6 (%temporal-decode (%temporal-encode y m d))) 'double-float)
     "toString"   (lambda () (format nil "~A-~A-~A"
                                     (%temporal-pad y 4) (%temporal-pad m 2) (%temporal-pad d 2)))
     "toPlainDateTime" (lambda (&optional plain-time)
                         (if (and plain-time (gethash "hour" plain-time))
                             (%js-temporal-plain-datetime y m d
                               (gethash "hour" plain-time) (gethash "minute" plain-time) (gethash "second" plain-time))
                             (%js-temporal-plain-datetime y m d 0 0 0)))
     "add"        (lambda (duration)
                    (let* ((ts (%temporal-encode y m d))
                           (ts2 (+ ts (%temporal-duration-to-seconds duration))))
                      (multiple-value-bind (s mn h nd nm ny) (%temporal-decode ts2)
                        (declare (ignore s mn h))
                        (%js-temporal-plain-date ny nm nd))))
     "subtract"   (lambda (duration)
                    (let* ((ts (%temporal-encode y m d))
                           (ts2 (- ts (%temporal-duration-to-seconds duration))))
                      (multiple-value-bind (s mn h nd nm ny) (%temporal-decode ts2)
                        (declare (ignore s mn h))
                        (%js-temporal-plain-date ny nm nd))))
     "equals"     (lambda (other) (and (= y (gethash "year" other)) (= m (gethash "month" other)) (= d (gethash "day" other))))
     "compare"    (lambda (other)
                    (%temporal-3way-compare
                     (%temporal-encode y m d)
                     (%temporal-encode (gethash "year" other) (gethash "month" other) (gethash "day" other)))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.PlainTime
;;; -----------------------------------------------------------------------

(defun %js-temporal-plain-time (hour minute second &optional (ms 0) (us 0) (ns 0))
  (let ((h hour) (mn minute) (s second))
    (declare (ignore ms us ns))
    (%js-make-object
     "__type__"   "Temporal.PlainTime"
     "hour"       (coerce h 'double-float)
     "minute"     (coerce mn 'double-float)
     "second"     (coerce s 'double-float)
     "millisecond" 0.0d0
     "microsecond" 0.0d0
     "nanosecond"  0.0d0
     "toString"   (lambda () (format nil "~A:~A:~A"
                                     (%temporal-pad h 2) (%temporal-pad mn 2) (%temporal-pad s 2)))
     "add"        (lambda (duration)
                    (let* ((total (+ (* h 3600) (* mn 60) s (%temporal-duration-to-seconds duration)))
                           (new-h (mod (floor total 3600) 24))
                           (new-mn (mod (floor (mod total 3600) 60) 60))
                           (new-s (mod total 60)))
                      (%js-temporal-plain-time new-h new-mn (floor new-s))))
     "equals"     (lambda (other) (and (= h (gethash "hour" other)) (= mn (gethash "minute" other)) (= s (gethash "second" other)))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.PlainDateTime
;;; -----------------------------------------------------------------------

(defun %js-temporal-plain-datetime (year month day hour minute second)
  (let ((y year) (m month) (d day) (h hour) (mn minute) (s second))
    (%js-make-object
     "__type__"   "Temporal.PlainDateTime"
     "year"       (coerce y 'double-float)
     "month"      (coerce m 'double-float)
     "day"        (coerce d 'double-float)
     "hour"       (coerce h 'double-float)
     "minute"     (coerce mn 'double-float)
     "second"     (coerce s 'double-float)
     "millisecond" 0.0d0
     "microsecond" 0.0d0
     "nanosecond"  0.0d0
     "calendarId" "iso8601"
     "toString"   (lambda (&optional _opts) (declare (ignore _opts))
                   (format nil "~A-~A-~AT~A:~A:~A"
                           (%temporal-pad y 4) (%temporal-pad m 2) (%temporal-pad d 2)
                           (%temporal-pad h 2) (%temporal-pad mn 2) (%temporal-pad s 2)))
     "toPlainDate" (lambda () (%js-temporal-plain-date y m d))
     "toPlainTime" (lambda () (%js-temporal-plain-time h mn s))
     "toInstant"   (lambda (&optional _tz) (declare (ignore _tz))
                    (%js-temporal-instant (%temporal-encode y m d h mn s)))
     "add"         (lambda (duration)
                     (let* ((ts (%temporal-encode y m d h mn s))
                            (ts2 (+ ts (%temporal-duration-to-seconds duration))))
                       (multiple-value-bind (ns nmn nh nd nm ny) (%temporal-decode ts2)
                         (%js-temporal-plain-datetime ny nm nd nh nmn ns))))
     "subtract"    (lambda (duration)
                     (let* ((ts (%temporal-encode y m d h mn s))
                            (ts2 (- ts (%temporal-duration-to-seconds duration))))
                       (multiple-value-bind (ns nmn nh nd nm ny) (%temporal-decode ts2)
                         (%js-temporal-plain-datetime ny nm nd nh nmn ns))))
     "equals"      (lambda (other)
                     (= (%temporal-encode y m d h mn s)
                        (%temporal-encode (gethash "year" other) (gethash "month" other) (gethash "day" other)
                                          (gethash "hour" other) (gethash "minute" other) (gethash "second" other)))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.ZonedDateTime
;;; -----------------------------------------------------------------------

(defun %js-temporal-zoned-datetime (year month day hour minute second tz)
  (let ((y year) (m month) (d day) (h hour) (mn minute) (s second))
    (%js-make-object
     "__type__"     "Temporal.ZonedDateTime"
     "year"         (coerce y 'double-float)
     "month"        (coerce m 'double-float)
     "day"          (coerce d 'double-float)
     "hour"         (coerce h 'double-float)
     "minute"       (coerce mn 'double-float)
     "second"       (coerce s 'double-float)
     "millisecond"  0.0d0
     "microsecond"  0.0d0
     "nanosecond"   0.0d0
     "timeZoneId"   tz
     "calendarId"   "iso8601"
     "epochSeconds" (coerce (%temporal-encode y m d h mn s) 'double-float)
     "toString"     (lambda (&optional _opts) (declare (ignore _opts))
                     (format nil "~A-~A-~AT~A:~A:~A+00:00[~A]"
                             (%temporal-pad y 4) (%temporal-pad m 2) (%temporal-pad d 2)
                             (%temporal-pad h 2) (%temporal-pad mn 2) (%temporal-pad s 2) tz))
     "toPlainDate"  (lambda () (%js-temporal-plain-date y m d))
     "toPlainTime"  (lambda () (%js-temporal-plain-time h mn s))
     "toPlainDateTime" (lambda () (%js-temporal-plain-datetime y m d h mn s))
     "toInstant"    (lambda () (%js-temporal-instant (%temporal-encode y m d h mn s))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.Duration
;;; -----------------------------------------------------------------------

;;; Seconds-per-unit table for %temporal-duration-to-seconds.
(defparameter *%temporal-unit-seconds*
  '(("years" . 31557600) ("months" . 2629800) ("weeks" . 604800)
    ("days"  . 86400)    ("hours"  . 3600)    ("minutes" . 60) ("seconds" . 1))
  "Alist mapping Temporal duration field names to their second equivalents.")

(defun %temporal-duration-to-seconds (duration)
  "Convert a Temporal.Duration to total seconds."
  (if (%js-ht-p duration)
      (loop for (key . scale) in *%temporal-unit-seconds*
            sum (* (or (gethash key duration) 0) scale))
      0))

(defun %js-temporal-duration (&key (years 0) (months 0) (weeks 0) (days 0)
                                    (hours 0) (minutes 0) (seconds 0)
                                    (milliseconds 0) (microseconds 0) (nanoseconds 0))
  (let ((y years) (mo months) (w weeks) (d days) (h hours) (mn minutes) (s seconds))
    (declare (ignore milliseconds microseconds nanoseconds))
    (%js-make-object
     "__type__"   "Temporal.Duration"
     "years"      (coerce y 'double-float)
     "months"     (coerce mo 'double-float)
     "weeks"      (coerce w 'double-float)
     "days"       (coerce d 'double-float)
     "hours"      (coerce h 'double-float)
     "minutes"    (coerce mn 'double-float)
     "seconds"    (coerce s 'double-float)
     "milliseconds" 0.0d0
     "microseconds" 0.0d0
     "nanoseconds"  0.0d0
     "sign"       (if (or (plusp y) (plusp mo) (plusp w) (plusp d) (plusp h) (plusp mn) (plusp s)) 1.0d0 -1.0d0)
     "toString"   (lambda ()
                    (format nil "P~AY~AM~AW~ADT~AH~AM~AS"
                            y mo w d h mn s))
     "abs"        (lambda () (%js-temporal-duration :years (abs y) :months (abs mo) :weeks (abs w)
                                                    :days (abs d) :hours (abs h) :minutes (abs mn) :seconds (abs s)))
     "negated"    (lambda () (%js-temporal-duration :years (- y) :months (- mo) :weeks (- w)
                                                    :days (- d) :hours (- h) :minutes (- mn) :seconds (- s)))
     "total"      (lambda (&optional opts)
                    (declare (ignore opts))
                    (coerce (%temporal-duration-to-seconds (%js-make-object
                                                            "years" y "months" mo "weeks" w "days" d
                                                            "hours" h "minutes" mn "seconds" s)) 'double-float)))))

;;; -----------------------------------------------------------------------
;;;  Temporal.PlainYearMonth / Temporal.PlainMonthDay
;;; -----------------------------------------------------------------------

(defun %js-temporal-plain-year-month (year month)
  (%js-make-object
   "__type__" "Temporal.PlainYearMonth"
   "year"     (coerce year 'double-float)
   "month"    (coerce month 'double-float)
   "toString" (lambda () (format nil "~A-~A" (%temporal-pad year 4) (%temporal-pad month 2)))))

(defun %js-temporal-plain-month-day (month day)
  (%js-make-object
   "__type__" "Temporal.PlainMonthDay"
   "month"    (coerce month 'double-float)
   "day"      (coerce day 'double-float)
   "toString" (lambda () (format nil "--~A-~A" (%temporal-pad month 2) (%temporal-pad day 2)))))

;;; -----------------------------------------------------------------------
;;;  Parse helpers
;;; -----------------------------------------------------------------------

(defun %temporal-parse-iso-fields (s)
  "Parse an ISO-8601 string S into (values year month day hour minute second).
Fields beyond what S contains default to 0."
  (flet ((field (start end) (if (>= (length s) end) (parse-integer s :start start :end end) 0)))
    (values (field 0 4) (field 5 7) (field 8 10) (field 11 13) (field 14 16) (field 17 19))))

(defun %js-temporal-parse-instant (str)
  (handler-case
      (multiple-value-bind (y mo d h mi s) (%temporal-parse-iso-fields (%js-to-string str))
        (%js-temporal-instant (%temporal-encode y mo d h mi s)))
    (error () (%js-temporal-instant (%temporal-now-unix-seconds)))))

(defun %js-temporal-parse-plain-date (str)
  (handler-case
      (multiple-value-bind (y mo d) (%temporal-parse-iso-fields (%js-to-string str))
        (%js-temporal-plain-date y mo d))
    (error ()
      (multiple-value-bind (s mn h d m y) (%temporal-decode (%temporal-now-unix-seconds))
        (declare (ignore s mn h))
        (%js-temporal-plain-date y m d)))))

;;; -----------------------------------------------------------------------
;;;  The Temporal global object
;;; -----------------------------------------------------------------------
