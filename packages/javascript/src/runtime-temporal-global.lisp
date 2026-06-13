;;;; packages/javascript/src/runtime-temporal-global.lisp — Temporal global namespace object
;;;;
;;;; Builds *js-temporal-global* from the 8 Temporal type constructors defined in
;;;; runtime-temporal.lisp. Separated so the large factory function does not
;;;; inflate the core types file.
;;;;
;;;; Load order: after runtime-temporal.lisp (needs all %js-temporal-* constructors
;;;; and %temporal-* helpers).

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  The Temporal global object
;;; -----------------------------------------------------------------------

(defun %js-make-temporal-global ()
  "Build the Temporal global namespace object."
  (%js-make-object
   ;; Temporal.Now
   "Now"           (%js-temporal-now)
   ;; Constructors (callable) — each takes initArgs object or positional
   "Instant"       (%js-make-object
                    "__call__" (lambda (&optional epoch-nanos)
                                 (declare (ignore epoch-nanos))
                                 (%js-temporal-instant (%temporal-now-unix-seconds)))
                    "from"     (lambda (str) (%js-temporal-parse-instant str))
                    "fromEpochSeconds" (lambda (s) (%js-temporal-instant s))
                    "fromEpochMilliseconds" (lambda (ms) (%js-temporal-instant (/ ms 1000)))
                    "fromEpochMicroseconds" (lambda (us) (%js-temporal-instant (/ us 1000000)))
                    "fromEpochNanoseconds"  (lambda (ns) (%js-temporal-instant (/ ns 1000000000)))
                    "compare" (lambda (a b)
                                (let ((ta (gethash "epochSeconds" a))
                                      (tb (gethash "epochSeconds" b)))
                                  (cond ((< ta tb) -1.0d0) ((> ta tb) 1.0d0) (t 0.0d0)))))
   "PlainDate"     (%js-make-object
                    "__call__" (lambda (y m d) (%js-temporal-plain-date y m d))
                    "from"     (lambda (str-or-obj)
                                 (if (%js-ht-p str-or-obj)
                                     (%js-temporal-plain-date (gethash "year" str-or-obj)
                                                              (gethash "month" str-or-obj)
                                                              (gethash "day" str-or-obj))
                                     (%js-temporal-parse-plain-date str-or-obj)))
                    "compare"  (lambda (a b)
                                 (let ((ta (%temporal-encode (gethash "year" a) (gethash "month" a) (gethash "day" a)))
                                       (tb (%temporal-encode (gethash "year" b) (gethash "month" b) (gethash "day" b))))
                                   (cond ((< ta tb) -1.0d0) ((> ta tb) 1.0d0) (t 0.0d0)))))
   "PlainTime"     (%js-make-object
                    "__call__" (lambda (&optional h m s)
                                 (%js-temporal-plain-time (or h 0) (or m 0) (or s 0)))
                    "from"     (lambda (str-or-obj)
                                 (if (%js-ht-p str-or-obj)
                                     (%js-temporal-plain-time (gethash "hour" str-or-obj)
                                                              (gethash "minute" str-or-obj)
                                                              (gethash "second" str-or-obj))
                                     (let ((s (%js-to-string str-or-obj)))
                                       (%js-temporal-plain-time
                                        (if (>= (length s) 2) (parse-integer s :start 0 :end 2) 0)
                                        (if (>= (length s) 5) (parse-integer s :start 3 :end 5) 0)
                                        (if (>= (length s) 8) (parse-integer s :start 6 :end 8) 0))))))
   "PlainDateTime" (%js-make-object
                    "__call__" (lambda (y m d &optional (h 0) (mn 0) (s 0))
                                 (%js-temporal-plain-datetime y m d h mn s))
                    "from"     (lambda (str-or-obj)
                                 (if (%js-ht-p str-or-obj)
                                     (%js-temporal-plain-datetime (gethash "year" str-or-obj)
                                                                  (gethash "month" str-or-obj)
                                                                  (gethash "day" str-or-obj)
                                                                  (or (gethash "hour" str-or-obj) 0)
                                                                  (or (gethash "minute" str-or-obj) 0)
                                                                  (or (gethash "second" str-or-obj) 0))
                                     (let ((inst (%js-temporal-parse-instant str-or-obj)))
                                       (let ((ts (gethash "epochSeconds" inst)))
                                         (multiple-value-bind (s mn h d m y) (%temporal-decode ts)
                                           (%js-temporal-plain-datetime y m d h mn s)))))))
   "ZonedDateTime" (%js-make-object
                    "__call__" (lambda (epoch-nanos &optional (tz "UTC") _cal)
                                 (declare (ignore _cal))
                                 (multiple-value-bind (s mn h d m y) (%temporal-decode (/ epoch-nanos 1000000000))
                                   (%js-temporal-zoned-datetime y m d h mn s tz)))
                    "from"     (lambda (str &optional _opts)
                                 (declare (ignore _opts))
                                 (let ((inst (%js-temporal-parse-instant (if (%js-ht-p str)
                                                                              (gethash "toString" str)
                                                                              str))))
                                   (let ((ts (gethash "epochSeconds" inst)))
                                     (multiple-value-bind (s mn h d m y) (%temporal-decode ts)
                                       (%js-temporal-zoned-datetime y m d h mn s "UTC"))))))
   "Duration"      (%js-make-object
                    "__call__" (lambda (&optional (y 0) (mo 0) (w 0) (d 0) (h 0) (mn 0) (s 0))
                                 (%js-temporal-duration :years y :months mo :weeks w :days d :hours h :minutes mn :seconds s))
                    "from"     (lambda (str-or-obj)
                                 (if (%js-ht-p str-or-obj)
                                     (%js-temporal-duration
                                      :years (or (gethash "years" str-or-obj) 0)
                                      :months (or (gethash "months" str-or-obj) 0)
                                      :days (or (gethash "days" str-or-obj) 0)
                                      :hours (or (gethash "hours" str-or-obj) 0)
                                      :minutes (or (gethash "minutes" str-or-obj) 0)
                                      :seconds (or (gethash "seconds" str-or-obj) 0))
                                     (%js-temporal-duration)))
                    "compare"  (lambda (a b)
                                 (let ((sa (%temporal-duration-to-seconds a))
                                       (sb (%temporal-duration-to-seconds b)))
                                   (cond ((< sa sb) -1.0d0) ((> sa sb) 1.0d0) (t 0.0d0)))))
   "PlainYearMonth" (%js-make-object
                     "__call__" (lambda (y m) (%js-temporal-plain-year-month y m))
                     "from"     (lambda (str-or-obj)
                                  (if (%js-ht-p str-or-obj)
                                      (%js-temporal-plain-year-month (gethash "year" str-or-obj) (gethash "month" str-or-obj))
                                      (%js-temporal-plain-year-month 2000 1))))
   "PlainMonthDay"  (%js-make-object
                     "__call__" (lambda (m d) (%js-temporal-plain-month-day m d))
                     "from"     (lambda (str-or-obj)
                                  (if (%js-ht-p str-or-obj)
                                      (%js-temporal-plain-month-day (gethash "month" str-or-obj) (gethash "day" str-or-obj))
                                      (%js-temporal-plain-month-day 1 1))))))

(defparameter *js-temporal-global* (%js-make-temporal-global)
  "The Temporal global namespace (immutable datetime API, ES2026).")
