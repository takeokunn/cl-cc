;;;; packages/javascript/src/runtime-builtins-globals.lisp — JS global function implementations
;;;;
;;;; Global built-in functions: parseInt, parseFloat, isNaN, isFinite,
;;;; extended Math (sinh/cosh/tanh/cbrt/expm1/log1p),
;;;; Number strict predicates (Number.isNaN/isFinite/isInteger),
;;;; structuredClone / queueMicrotask globals,
;;;; BigInt helpers, Iterator.from, Map.groupBy, String.raw,
;;;; and lightweight Intl constructors.
;;;;
;;;; Load order: after runtime-builtins.lisp (needs %js-to-number, %js-funcall,
;;;; %js-nan-p, %js-float-nan-p, %js-float-infinity-p, %js-to-string, etc.)

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Global function helpers (referenced in *js-builtin-specs* and js-program-forms)
;;; -----------------------------------------------------------------------

(defun %js-parse-int (s &optional (radix 10))
  "Parse integer from string S in the given RADIX (default 10)."
  (handler-case
    (let* ((str (string-trim '(#\Space #\Tab #\Newline) (%js-to-string s)))
           (r (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix)))))
      (or (parse-integer str :radix r :junk-allowed t) *js-nan-float*))
    (error () *js-nan-float*)))

(defun %js-parse-float (s)
  "JS parseFloat: parse the LONGEST leading numeric prefix of S (so \"3.14abc\"
-> 3.14), or NaN when there is no leading number."
  (let* ((str (string-trim '(#\Space #\Tab #\Newline #\Return) (%js-to-string s)))
         (len (length str)) (i 0) (saw-digit nil))
    (labels ((eat-digits ()
               (loop while (and (< i len) (digit-char-p (char str i)))
                     do (setf saw-digit t) (incf i))))
      (when (and (< i len) (member (char str i) '(#\+ #\-))) (incf i))
      (eat-digits)
      (when (and (< i len) (char= (char str i) #\.)) (incf i) (eat-digits))
      (unless saw-digit (return-from %js-parse-float *js-nan-float*))
      ;; optional exponent: e[+/-]digits — only consumed if well-formed
      (when (and (< i len) (member (char str i) '(#\e #\E)))
        (let ((save i))
          (incf i)
          (when (and (< i len) (member (char str i) '(#\+ #\-))) (incf i))
          (if (and (< i len) (digit-char-p (char str i)))
              (loop while (and (< i len) (digit-char-p (char str i))) do (incf i))
              (setf i save))))
      (handler-case
          (let ((*read-eval* nil)
                (*read-default-float-format* 'double-float))
            (let ((v (read-from-string (subseq str 0 i) nil *js-nan-float*)))
              (if (realp v) (coerce v 'double-float) *js-nan-float*)))
        (error () *js-nan-float*)))))

(defun %js-is-nan (x)
  "Return true if X converts to NaN."
  (%js-nan-p (%js-to-number x)))

(defun %js-is-finite (x)
  "Return true if X is finite (not NaN, not Infinity)."
  (let ((n (%js-to-number x)))
    (and (not (%js-float-nan-p n)) (not (%js-float-infinity-p n)))))

;;; -----------------------------------------------------------------------
;;;  Math coerce-unary helpers
;;; -----------------------------------------------------------------------

(defmacro define-js-math-coerce-unary (name cl-fn)
  "Define a unary JS Math helper that coerces its argument to double-float."
  `(defun ,name (x) (,cl-fn (coerce (%js-to-number x) 'double-float))))

(define-js-math-coerce-unary %js-math-sinh  sinh)
(define-js-math-coerce-unary %js-math-cosh  cosh)
(define-js-math-coerce-unary %js-math-tanh  tanh)
(define-js-math-coerce-unary %js-math-asinh asinh)
(define-js-math-coerce-unary %js-math-acosh acosh)
(define-js-math-coerce-unary %js-math-atanh atanh)

(defun %js-math-cbrt (x)
  (let ((v (coerce (%js-to-number x) 'double-float)))
    (if (minusp v)
        (- (expt (- v) (/ 1.0d0 3.0d0)))
        (expt v (/ 1.0d0 3.0d0)))))

(defun %js-math-expm1 (x) (- (exp (coerce (%js-to-number x) 'double-float)) 1.0d0))
(defun %js-math-log1p  (x) (log (+ 1.0d0 (coerce (%js-to-number x) 'double-float))))

;;; -----------------------------------------------------------------------
;;;  Number.isNaN / isFinite / isInteger strict predicates
;;; -----------------------------------------------------------------------

(defun %js-number-is-nan (x)     (%js-float-nan-p x))
(defun %js-number-is-finite (x)
  (and (numberp x) (not (%js-float-nan-p x)) (not (%js-float-infinity-p x))))
(defun %js-number-is-integer (x)
  (and (numberp x) (not (%js-float-nan-p x)) (= x (truncate x))))

;;; -----------------------------------------------------------------------
;;;  Standalone global-builtin helpers (referenced in *js-builtin-specs*)
;;; -----------------------------------------------------------------------

(defun %js-structured-clone (val &rest _opts)
  "structuredClone(value[, options]): deep clone VAL (options ignored)."
  (declare (ignore _opts))
  (%js-deep-clone val))

(defun %js-queue-microtask (fn &rest _)
  "queueMicrotask(fn): synchronous in our single-threaded model."
  (declare (ignore _))
  (%js-funcall fn)
  +js-undefined+)

;;; -----------------------------------------------------------------------
;;;  Complex built-in helpers
;;; -----------------------------------------------------------------------

(defun %js-bigint-as-int-n (width bigint)
  "BigInt.asIntN(width, bigint): mask BIGINT to WIDTH-bit signed integer."
  (let* ((w       (truncate (%js-to-number width)))
         (modulus (expt 2 w))
         (masked  (mod (%js-bigint-val bigint) modulus)))
    (%make-js-bigint (if (>= masked (expt 2 (1- w))) (- masked modulus) masked))))

(defun %js-bigint-as-uint-n (width bigint)
  "BigInt.asUintN(width, bigint): mask BIGINT to WIDTH-bit unsigned integer."
  (%make-js-bigint (mod (%js-bigint-val bigint) (expt 2 (truncate (%js-to-number width))))))

(defun %js-iterator-from-iterable (iterable)
  "Iterator.from(iterable): wrap any iterable/iterator in the Iterator protocol."
  (%js-add-iterator-helpers!
   (cond
     ((and (%js-ht-p iterable) (gethash "next" iterable))
      iterable)
     ((and (%js-ht-p iterable) (gethash "@@iterator" iterable))
      (%js-funcall (gethash "@@iterator" iterable)))
     ((%js-vec-p iterable)
      (%js-make-generator
       (lambda ()
         (loop for i below (length iterable)
               do (%js-yield (aref iterable i))))))
     ((stringp iterable)
      (%js-make-generator
       (lambda ()
         (loop for ch across iterable
               do (%js-yield (string ch))))))
     (t
      (%js-make-object "next"
                       (lambda ()
                         (%js-make-object "value" +js-undefined+ "done" t)))))))

(defun %js-map-group-by (iterable key-fn)
  "Map.groupBy(iterable, keyFn): group ITERABLE elements by KEY-FN result."
  (let ((result (%js-make-map)))
    (%js-for-of iterable
                (lambda (item)
                  (let* ((key      (%js-funcall key-fn item))
                         (existing (%js-map-get result key)))
                    (if (eq existing +js-undefined+)
                        (let ((arr (%js-make-vec 0)))
                          (vector-push-extend item arr)
                          (%js-map-set result key arr))
                        (vector-push-extend item existing)))))
    result))

;;; -----------------------------------------------------------------------
;;;  String.raw helper
;;; -----------------------------------------------------------------------

(defun %js-string-raw (strings &rest subs)
  "String.raw`...` tag: join raw literal portions with substitution values.
STRINGS is a vector of literal string parts; SUBS are the interpolated values.
Escape sequences in the literal parts are not processed."
  (with-output-to-string (out)
    (loop for i below (length strings)
          do (write-string (%js-to-string (aref strings i)) out)
             (when (< i (length subs))
               (write-string (%js-to-string (nth i subs)) out)))))

;;; -----------------------------------------------------------------------
;;;  Intl constructor helpers (locale/options ignored in synchronous model)
;;; -----------------------------------------------------------------------

(defun %js-number-format-option-integer (options name default)
  (let ((value (%js-intl-option options name +js-undefined+)))
    (if (eq value +js-undefined+)
        default
        (max 0 (truncate (%js-to-number value))))))

(defun %js-number-format-group-integer (digits)
  (let* ((negative-p (and (plusp (length digits)) (char= (char digits 0) #\-)))
         (body (if negative-p (subseq digits 1) digits))
         (len (length body)))
    (with-output-to-string (out)
      (when negative-p (write-char #\- out))
      (loop for i below len
            do (when (and (plusp i) (zerop (mod (- len i) 3)))
                 (write-char #\, out))
               (write-char (char body i) out)))))

(defun %js-number-format-compose (value options)
  (let* ((style (%js-to-string (%js-intl-option options "style" "decimal")))
         (use-grouping (not (null (%js-intl-option options "useGrouping" t))))
         (min-frac (%js-number-format-option-integer
                    options "minimumFractionDigits" 0))
         (max-default (max 3 min-frac))
         (max-frac (max min-frac
                        (%js-number-format-option-integer
                         options "maximumFractionDigits" max-default)))
         (scaled-value (if (string= style "percent")
                           (* (%js-to-number value) 100.0d0)
                           (%js-to-number value)))
         (negative-p (minusp scaled-value))
         (abs-value (abs scaled-value))
         (scale (expt 10 max-frac))
         (rounded-units (floor (+ (* abs-value scale) 0.5d0)))
         (integer-part (floor rounded-units scale))
         (fraction-number (mod rounded-units scale))
         (integer-string (format nil "~D" integer-part))
         (fraction-string (if (zerop max-frac)
                              ""
                              (format nil "~v,'0D" max-frac fraction-number))))
    (loop while (and (> (length fraction-string) min-frac)
                     (char= (char fraction-string (1- (length fraction-string))) #\0))
          do (setf fraction-string
                   (subseq fraction-string 0 (1- (length fraction-string)))))
    (let ((signed-integer (if negative-p
                              (concatenate 'string "-" integer-string)
                              integer-string)))
      (values (with-output-to-string (out)
                (write-string
                 (if use-grouping
                     (%js-number-format-group-integer signed-integer)
                     signed-integer)
                 out)
                (when (plusp (length fraction-string))
                  (write-char #\. out)
                  (write-string fraction-string out))
                (when (string= style "percent")
                  (write-char #\% out)))
              signed-integer
              fraction-string
              style))))

(defun %js-make-intl-number-format (&optional _locale options)
  "Intl.NumberFormat(locale?, options?): format with basic decimal options."
  (declare (ignore _locale))
  (%js-make-object
   "__call__" (lambda (&rest _) (declare (ignore _)) +js-undefined+)
   "format"   (lambda (n)
                (multiple-value-bind (formatted) (%js-number-format-compose n options)
                  formatted))
   "formatToParts" (lambda (n)
                     (multiple-value-bind (formatted integer-string fraction-string style)
                         (%js-number-format-compose n options)
                       (declare (ignore formatted))
                       (let ((parts (list (%js-make-object "type" "integer" "value"
                                                           integer-string))))
                         (when (plusp (length fraction-string))
                           (setf parts (append parts
                                               (list (%js-make-object "type" "decimal" "value" ".")
                                                     (%js-make-object "type" "fraction" "value"
                                                                      fraction-string)))))
                         (when (string= style "percent")
                           (setf parts (append parts
                                               (list (%js-make-object "type" "percentSign"
                                                                      "value" "%")))))
                         (apply #'%js-make-array parts))))))

(defparameter +js-date-time-format-month-long-names+
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun %js-date-time-format-option-string (options name default)
  (let ((value (%js-intl-option options name +js-undefined+)))
    (if (eq value +js-undefined+)
        default
        (%js-to-string value))))

(defun %js-date-time-format-input-date (value)
  (cond
    ((js-date-p value) value)
    ((or (numberp value) (stringp value)) (%js-make-date value))
    ((or (eq value +js-undefined+) (eq value +js-null+)) (%js-make-date))
    (t (%js-make-date value))))

(defun %js-date-time-format-two-digit (value)
  (format nil "~2,'0D" (mod (truncate value) 100)))

(defun %js-date-time-format-numeric (value style)
  (if (string= style "2-digit")
      (%js-date-time-format-two-digit value)
      (format nil "~D" value)))

(defun %js-date-time-format-month (month style)
  (cond
    ((string= style "2-digit") (%js-date-time-format-two-digit month))
    ((string= style "short") (aref +js-date-month-names+ (1- month)))
    ((string= style "long") (aref +js-date-time-format-month-long-names+ (1- month)))
    ((string= style "narrow") (subseq (aref +js-date-time-format-month-long-names+
                                             (1- month))
                                      0 1))
    (t (format nil "~D" month))))

(defun %js-date-time-format-style-date (date-style)
  (cond
    ((member date-style '("full" "long") :test #'string=)
     (values "numeric" "long" "numeric"))
    ((string= date-style "medium")
     (values "numeric" "short" "numeric"))
    ((string= date-style "short")
     (values "2-digit" "numeric" "numeric"))
    (t (values nil nil nil))))

(defun %js-date-time-format-style-time (time-style)
  (cond
    ((member time-style '("full" "long" "medium") :test #'string=)
     (values "numeric" "2-digit" "2-digit"))
    ((string= time-style "short")
     (values "numeric" "2-digit" nil))
    (t (values nil nil nil))))

(defun %js-date-time-format-parts (date year-style month-style day-style
                                        hour-style minute-style second-style hour12)
  (%with-date-fields (date :sec sec :min min :hour hour :day day :month month :year year)
    (let ((parts nil)
          (date-p (or year-style month-style day-style))
          (time-p (or hour-style minute-style second-style)))
      (labels ((add (type value)
                 (push (%js-make-object "type" type "value" value) parts))
               (literal (value)
                 (add "literal" value)))
        (when month-style
          (add "month" (%js-date-time-format-month month month-style)))
        (when (and month-style day-style)
          (literal "/"))
        (when day-style
          (add "day" (%js-date-time-format-numeric day day-style)))
        (when (and (or month-style day-style) year-style)
          (literal "/"))
        (when year-style
          (add "year"
               (if (string= year-style "2-digit")
                   (%js-date-time-format-two-digit year)
                   (format nil "~D" year))))
        (when (and date-p time-p)
          (literal ", "))
        (when hour-style
          (let ((display-hour (if hour12
                                  (let ((h (mod hour 12)))
                                    (if (zerop h) 12 h))
                                  hour)))
            (add "hour" (%js-date-time-format-numeric display-hour hour-style))))
        (when (and hour-style minute-style)
          (literal ":"))
        (when minute-style
          (add "minute" (%js-date-time-format-numeric min minute-style)))
        (when (and (or hour-style minute-style) second-style)
          (literal ":"))
        (when second-style
          (add "second" (%js-date-time-format-numeric sec second-style)))
        (when (and hour12 hour-style)
          (literal " ")
          (add "dayPeriod" (if (< hour 12) "AM" "PM")))
        (nreverse parts)))))

(defun %js-date-time-format-render (parts)
  (with-output-to-string (out)
    (loop for part in parts
          do (write-string (gethash "value" part) out))))

(defun %js-date-time-format-resolved-options (date-style time-style year-style
                                                        month-style day-style
                                                        hour-style minute-style
                                                        second-style hour12)
  (%js-make-object
   "locale" "en-US"
   "calendar" "gregory"
   "numberingSystem" "latn"
   "timeZone" "UTC"
   "dateStyle" (or date-style +js-undefined+)
   "timeStyle" (or time-style +js-undefined+)
   "year" (or year-style +js-undefined+)
   "month" (or month-style +js-undefined+)
   "day" (or day-style +js-undefined+)
   "hour" (or hour-style +js-undefined+)
   "minute" (or minute-style +js-undefined+)
   "second" (or second-style +js-undefined+)
   "hour12" hour12))

(defun %js-make-intl-date-time-format (&optional _locale options)
  "Intl.DateTimeFormat(locale?, options?): deterministic UTC English formatter."
  (declare (ignore _locale))
  (let* ((date-style (%js-date-time-format-option-string options "dateStyle" nil))
         (time-style (%js-date-time-format-option-string options "timeStyle" nil))
         (year-style (%js-date-time-format-option-string options "year" nil))
         (month-style (%js-date-time-format-option-string options "month" nil))
         (day-style (%js-date-time-format-option-string options "day" nil))
         (hour-style (%js-date-time-format-option-string options "hour" nil))
         (minute-style (%js-date-time-format-option-string options "minute" nil))
         (second-style (%js-date-time-format-option-string options "second" nil))
         (hour12 (not (null (%js-intl-option options "hour12" nil))))
         (explicit-date-p (or year-style month-style day-style))
         (explicit-time-p (or hour-style minute-style second-style)))
    (unless explicit-date-p
      (multiple-value-bind (style-year style-month style-day)
          (%js-date-time-format-style-date date-style)
        (setf year-style style-year
              month-style style-month
              day-style style-day)))
    (unless explicit-time-p
      (multiple-value-bind (style-hour style-minute style-second)
          (%js-date-time-format-style-time time-style)
        (setf hour-style style-hour
              minute-style style-minute
              second-style style-second)))
    (unless (or date-style time-style explicit-date-p explicit-time-p)
      (setf year-style "numeric"
            month-style "numeric"
            day-style "numeric"))
    (%js-make-object
     "format" (lambda (&optional (date +js-undefined+))
                (let ((parts (%js-date-time-format-parts
                              (%js-date-time-format-input-date date)
                              year-style month-style day-style
                              hour-style minute-style second-style hour12)))
                  (%js-date-time-format-render parts)))
     "formatToParts" (lambda (&optional (date +js-undefined+))
                       (apply #'%js-make-array
                              (%js-date-time-format-parts
                               (%js-date-time-format-input-date date)
                               year-style month-style day-style
                               hour-style minute-style second-style hour12)))
     "resolvedOptions" (lambda ()
                         (%js-date-time-format-resolved-options
                          date-style time-style year-style month-style day-style
                          hour-style minute-style second-style hour12)))))

(defun %js-intl-option (options name default)
  (if (hash-table-p options)
      (multiple-value-bind (value present-p) (gethash name options)
        (if present-p value default))
      default))

(defun %js-collator-strip-basic-accents (string)
  "Strip the common Latin-1 accents that matter for our lightweight collator."
  (with-output-to-string (out)
    (loop for ch across string
          do (write-char
              (case ch
                ((#\À #\Á #\Â #\Ã #\Ä #\Å #\à #\á #\â #\ã #\ä #\å) #\a)
                ((#\Ç #\ç) #\c)
                ((#\È #\É #\Ê #\Ë #\è #\é #\ê #\ë) #\e)
                ((#\Ì #\Í #\Î #\Ï #\ì #\í #\î #\ï) #\i)
                ((#\Ñ #\ñ) #\n)
                ((#\Ò #\Ó #\Ô #\Õ #\Ö #\Ø #\ò #\ó #\ô #\õ #\ö #\ø) #\o)
                ((#\Ù #\Ú #\Û #\Ü #\ù #\ú #\û #\ü) #\u)
                ((#\Ý #\Ÿ #\ý #\ÿ) #\y)
                (otherwise ch))
              out))))

(defun %js-collator-normalize (string sensitivity)
  (let ((s (%js-to-string string)))
    (cond
      ((string= sensitivity "base")
       (string-downcase (%js-collator-strip-basic-accents s)))
      ((string= sensitivity "accent")
       (string-downcase s))
      ((string= sensitivity "case")
       (%js-collator-strip-basic-accents s))
      (t s))))

(defun %js-collator-read-number (string start)
  (let ((end start)
        (length (length string)))
    (loop while (and (< end length) (digit-char-p (char string end)))
          do (incf end))
    (values (parse-integer string :start start :end end) end)))

(defun %js-collator-string-compare (a b numeric-p)
  (if numeric-p
      (let ((ia 0)
            (ib 0)
            (la (length a))
            (lb (length b)))
        (loop
          (cond
            ((and (= ia la) (= ib lb)) (return 0))
            ((= ia la) (return -1))
            ((= ib lb) (return 1)))
          (let ((ca (char a ia))
                (cb (char b ib)))
            (if (and (digit-char-p ca) (digit-char-p cb))
                (multiple-value-bind (na next-a) (%js-collator-read-number a ia)
                  (multiple-value-bind (nb next-b) (%js-collator-read-number b ib)
                    (cond
                      ((< na nb) (return -1))
                      ((> na nb) (return 1))
                      (t (setf ia next-a
                               ib next-b)))))
                (cond
                  ((char< ca cb) (return -1))
                  ((char> ca cb) (return 1))
                  (t (incf ia) (incf ib)))))))
      (cond ((string< a b) -1)
            ((string> a b) 1)
            (t 0))))

(defun %js-make-intl-collator (&optional _locale options)
  "Intl.Collator(locale?, options?): compare strings with basic options."
  (declare (ignore _locale))
  (let* ((sensitivity (%js-to-string
                       (%js-intl-option options "sensitivity" "variant")))
         (numeric (not (null (%js-intl-option options "numeric" nil)))))
    (%js-make-object
     "compare" (lambda (a b)
                 (%js-collator-string-compare
                  (%js-collator-normalize a sensitivity)
                  (%js-collator-normalize b sensitivity)
                  numeric))
     "resolvedOptions" (lambda ()
                         (%js-make-object
                          "locale" "en-US"
                          "usage" "sort"
                          "sensitivity" sensitivity
                          "numeric" numeric)))))

(defun %js-list-format-items (list)
  (cond
    ((%js-vec-p list)
     (loop for i below (length list)
           collect (%js-to-string (aref list i))))
    ((listp list)
     (loop for item in list collect (%js-to-string item)))
    (t (list (%js-to-string list)))))

(defun %js-list-format-final-literal (type)
  (cond ((string= type "disjunction") " or ")
        ((string= type "unit") ", ")
        (t " and ")))

(defun %js-list-format-parts (items type)
  (let ((count (length items))
        (parts nil))
    (labels ((push-element (value)
               (push (%js-make-object "type" "element" "value" value) parts))
             (push-literal (value)
               (push (%js-make-object "type" "literal" "value" value) parts)))
      (cond
        ((zerop count))
        ((= count 1) (push-element (first items)))
        ((= count 2)
         (push-element (first items))
         (push-literal (%js-list-format-final-literal type))
         (push-element (second items)))
        (t
         (loop for item in items
               for index from 0
               do (progn
                    (when (plusp index)
                      (push-literal
                       (if (= index (1- count))
                           (if (string= type "unit")
                               ", "
                               (concatenate 'string ","
                                            (%js-list-format-final-literal type)))
                           ", ")))
                    (push-element item)))))
      (nreverse parts))))

(defun %js-list-format-render (parts)
  (with-output-to-string (out)
    (dolist (part parts)
      (write-string (gethash "value" part) out))))

(defun %js-make-intl-list-format (&optional _locale options)
  "Intl.ListFormat(locale?, options?): format English-style conjunction lists."
  (declare (ignore _locale))
  (let ((type (%js-to-string (%js-intl-option options "type" "conjunction")))
        (style (%js-to-string (%js-intl-option options "style" "long"))))
    (%js-make-object
     "format" (lambda (list)
                (%js-list-format-render
                 (%js-list-format-parts (%js-list-format-items list) type)))
     "formatToParts" (lambda (list)
                       (apply #'%js-make-array
                              (%js-list-format-parts
                               (%js-list-format-items list) type)))
     "resolvedOptions" (lambda ()
                         (%js-make-object
                          "locale" "en-US"
                          "type" type
                          "style" style)))))

(defun %js-plural-rules-ordinal-category (n)
  (let* ((integer (truncate (abs n)))
         (mod-10 (mod integer 10))
         (mod-100 (mod integer 100)))
    (cond
      ((and (= mod-10 1) (/= mod-100 11)) "one")
      ((and (= mod-10 2) (/= mod-100 12)) "two")
      ((and (= mod-10 3) (/= mod-100 13)) "few")
      (t "other"))))

(defun %js-plural-rules-cardinal-category (n)
  (let ((abs-n (abs n)))
    (if (= abs-n 1)
        "one"
        "other")))

(defun %js-make-intl-plural-rules (&optional _locale options)
  "Intl.PluralRules(locale?, options?): select basic English plural categories."
  (declare (ignore _locale))
  (let ((type (%js-to-string (%js-intl-option options "type" "cardinal"))))
    (labels ((select-category (n)
               (let ((number (%js-to-number n)))
                 (if (string= type "ordinal")
                     (%js-plural-rules-ordinal-category number)
                     (%js-plural-rules-cardinal-category number)))))
      (%js-make-object
       "select" #'select-category
       "selectRange" (lambda (_start end)
                       (declare (ignore _start))
                       (select-category end))
       "resolvedOptions" (lambda ()
                           (%js-make-object
                            "locale" "en-US"
                            "type" type
                            "pluralCategories"
                            (if (string= type "ordinal")
                                (%js-make-array "one" "two" "few" "other")
                                (%js-make-array "one" "other"))))))))
