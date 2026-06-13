;;;; packages/javascript/src/runtime-builtins-globals.lisp — JS global function implementations
;;;;
;;;; Global built-in functions: parseInt, parseFloat, isNaN, isFinite,
;;;; extended Math (sinh/cosh/tanh/cbrt/expm1/log1p),
;;;; Number strict predicates (Number.isNaN/isFinite/isInteger),
;;;; async/timer stubs (structuredClone, queueMicrotask, setTimeout, setInterval),
;;;; BigInt helpers, Iterator.from, Map.groupBy, String.raw,
;;;; and Intl constructor stubs.
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

(defun %js-set-timeout (fn &rest _)
  "setTimeout(fn[, delay, …args]): synchronous in our model — run FN now."
  (declare (ignore _))
  (unless (or (eq fn +js-undefined+) (eq fn +js-null+))
    (%js-funcall fn))
  +js-undefined+)

(defun %js-set-interval (&rest _)
  "setInterval(...): a no-op stub (running it would never terminate)."
  (declare (ignore _))
  +js-undefined+)

(defun %js-clear-timer (&rest _)
  "clearTimeout / clearInterval: a no-op stub."
  (declare (ignore _))
  +js-undefined+)

;;; -----------------------------------------------------------------------
;;;  Complex built-in helpers
;;; -----------------------------------------------------------------------

(defun %js-bigint-as-int-n (width bigint)
  "BigInt.asIntN(width, bigint): mask BIGINT to WIDTH-bit signed integer."
  (let* ((w       (truncate (%js-to-number width)))
         (v       (if (js-bigint-p bigint) (js-bigint-value bigint) (truncate bigint)))
         (modulus (expt 2 w))
         (half    (expt 2 (1- w)))
         (masked  (mod v modulus)))
    (%make-js-bigint (if (>= masked half) (- masked modulus) masked))))

(defun %js-bigint-as-uint-n (width bigint)
  "BigInt.asUintN(width, bigint): mask BIGINT to WIDTH-bit unsigned integer."
  (let* ((w (truncate (%js-to-number width)))
         (v (if (js-bigint-p bigint) (js-bigint-value bigint) (truncate bigint))))
    (%make-js-bigint (mod v (expt 2 w)))))

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

(defun %js-make-intl-number-format (&optional _locale _options)
  "Intl.NumberFormat(locale?, options?): locale-aware number formatter stub."
  (declare (ignore _locale _options))
  (%js-make-object
   "__call__" (lambda (&rest _) (declare (ignore _)) +js-undefined+)
   "format"   (lambda (n) (format nil "~,2F" (%js-to-number n)))
   "formatToParts" (lambda (n)
                     (%js-make-array
                      (%js-make-object "type" "integer" "value"
                                       (format nil "~D" (truncate (%js-to-number n))))))))

(defun %js-make-intl-date-time-format (&optional _locale _options)
  "Intl.DateTimeFormat(locale?, options?): locale-aware date formatter stub."
  (declare (ignore _locale _options))
  (%js-make-object
   "format" (lambda (date)
               (if (js-date-p date)
                   (%js-date-to-iso-string date)
                   (format nil "~A" date)))))

(defun %js-make-intl-collator (&optional _locale _options)
  "Intl.Collator(locale?, options?): string comparison stub."
  (declare (ignore _locale _options))
  (%js-make-object
   "compare" (lambda (a b)
               (cond ((string< (%js-to-string a) (%js-to-string b)) -1.0d0)
                     ((string> (%js-to-string a) (%js-to-string b))  1.0d0)
                     (t 0.0d0)))))

(defun %js-make-intl-list-format (&optional _locale _options)
  "Intl.ListFormat(locale?, options?): list formatter stub."
  (declare (ignore _locale _options))
  (%js-make-object
   "format" (lambda (list)
               (let ((items (loop for i below (length list)
                                  collect (%js-to-string (aref list i)))))
                 (cond ((null items) "")
                       ((= (length items) 1) (first items))
                       (t (format nil "~{~A~^, ~}" items)))))))

(defun %js-make-intl-plural-rules (&optional _locale _options)
  "Intl.PluralRules(locale?, options?): plural category selector stub."
  (declare (ignore _locale _options))
  (%js-make-object
   "select" (lambda (n) (if (= (%js-to-number n) 1) "one" "other"))))
