;;;; packages/javascript/src/runtime-builtins.lisp — JS built-in dispatch table
;;;;
;;;; *js-builtin-map* maps built-in name strings to CL functions.
;;;; This file must be loaded LAST so all referenced functions are defined.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Reflect / Object property-descriptor helpers (for runtime-builtins-table.lisp)
;;; -----------------------------------------------------------------------

(defun %js-reflect-get (target key &optional _receiver)
  (declare (ignore _receiver))
  (%js-get-prop target key))

(defun %js-reflect-set (target key value &optional _receiver)
  (declare (ignore _receiver))
  (%js-set-prop target key value)
  t)

(defun %js-reflect-has (target key)
  (when (%js-ht-p target)
    (nth-value 1 (gethash (%js-to-string key) target))))

(defun %js-reflect-delete-property (target key)
  (when (%js-ht-p target)
    (remhash (%js-to-string key) target))
  t)

(defun %js-reflect-apply (fn this-arg args)
  (apply #'%js-funcall fn this-arg (coerce args 'list)))

(defun %js-reflect-construct (target args &optional new-target)
  (declare (ignore new-target))
  (%js-new target (coerce args 'list)))

(defun %js-reflect-define-property (target key descriptor)
  (when (and (%js-ht-p target) (%js-ht-p descriptor))
    (let ((val (gethash "value" descriptor +js-undefined+)))
      (unless (eq val +js-undefined+)
        (setf (gethash (%js-to-string key) target) val))))
  t)

(defun %js-reflect-get-own-property-descriptor (target key)
  (if (%js-ht-p target)
      (let ((val (gethash (%js-to-string key) target +js-undefined+)))
        (if (eq val +js-undefined+) +js-undefined+
            (%js-make-object "value" val "writable" t "enumerable" t "configurable" t)))
      +js-undefined+))

(defun %js-object-define-property (obj key descriptor)
  (when (and (%js-ht-p obj) (%js-ht-p descriptor))
    (let ((val (gethash "value" descriptor +js-undefined+))
          (get (gethash "get" descriptor +js-undefined+))
          (set (gethash "set" descriptor +js-undefined+)))
      (unless (eq val +js-undefined+)
        (setf (gethash (%js-to-string key) obj) val))
      (unless (eq get +js-undefined+)
        (setf (gethash (concatenate 'string "__get_" (%js-to-string key)) obj) get))
      (unless (eq set +js-undefined+)
        (setf (gethash (concatenate 'string "__set_" (%js-to-string key)) obj) set))))
  obj)

(defun %js-object-define-properties (obj props)
  (when (and (%js-ht-p obj) (%js-ht-p props))
    (maphash (lambda (k v)
               (when (%js-ht-p v)
                 (let ((val (gethash "value" v +js-undefined+)))
                   (unless (eq val +js-undefined+)
                     (setf (gethash k obj) val)))))
             props))
  obj)

(defun %js-object-get-own-property-descriptor (obj key)
  (if (%js-ht-p obj)
      (let ((val (gethash (%js-to-string key) obj +js-undefined+)))
        (if (eq val +js-undefined+) +js-undefined+
            (%js-make-object "value" val "writable" t "enumerable" t "configurable" t)))
      +js-undefined+))

(defun %js-object-get-own-property-descriptors (obj)
  (let ((result (%js-make-ht)))
    (when (%js-ht-p obj)
      (maphash (lambda (k v)
                 (unless (and (stringp k) (>= (length k) 2) (string= k "__" :end1 2))
                   (setf (gethash k result)
                         (%js-make-object "value" v "writable" t "enumerable" t "configurable" t))))
               obj))
    result))

(defun %js-make-typed-array-ctor (type-name)
  "Return a constructor lambda for the named TypedArray TYPE-NAME."
  (lambda (&optional arg) (%js-make-typed-array type-name arg)))

(defun %js-make-set-from-iterable (&optional (iter +js-undefined+))
  "Build a new JS Set, optionally seeded from ITER."
  (let ((s (%js-make-set)))
    (when (and (not (eq iter +js-undefined+))
               (not (eq iter +js-null+)))
      (%js-for-of iter (lambda (v) (%js-set-add s v))))
    s))

(defun %js-make-proxy-object (target handler)
  "Simplified Proxy constructor: wraps target+handler in a hash table."
  (let ((ht (%js-make-ht)))
    (setf (gethash "__proxy-target__" ht) target
          (gethash "__proxy-handler__" ht) handler)
    ht))

(defun %js-promise-try (fn &rest args)
  "ES2025 Promise.try: call FN with ARGS, wrapping synchronous throws."
  (handler-case
      (%js-promise-resolve (apply #'%js-funcall fn args))
    (js-exception (c)
      (%js-promise-reject (js-exception-value c)))))

(defun %js-math-sum-precise (iterable)
  "ES2026 Math.sumPrecise: precise sum of a numeric iterable."
  (let ((sum 0.0d0))
    (%js-for-of iterable (lambda (v)
      (incf sum (coerce (%js-to-number v) 'double-float))))
    sum))

(defun %js-error-is-error (val)
  "ES2026 Error.isError: true when VAL looks like an Error object."
  (and (%js-ht-p val)
       (or (gethash "message" val)
           (gethash "stack" val))))

(defun %js-make-finalization-registry (cleanup-fn)
  "ES2021 FinalizationRegistry stub (synchronous model — cleanup not observable)."
  (declare (ignore cleanup-fn))
  (let ((registry (%js-make-ht)))
    (setf (gethash "register" registry)
          (lambda (target value &optional _token)
            (declare (ignore target value _token))
            +js-undefined+)
          (gethash "unregister" registry)
          (lambda (_token) (declare (ignore _token)) nil))
    registry))

(defun %js-regexp-escape (str)
  "ES2024 RegExp.escape: escape all regex-special characters in STR."
  (with-output-to-string (out)
    (loop for ch across (%js-to-string str)
          do (when (member ch '(#\\ #\^ #\$ #\. #\| #\? #\* #\+ #\( #\) #\[ #\] #\{ #\} #\/ #\-))
               (write-char #\\ out))
             (write-char ch out))))

(defun %js-make-aggregate-error (errors message &optional _opts)
  "AggregateError constructor stub."
  (declare (ignore _opts))
  (%js-make-object "message" (%js-to-string (or message ""))
                   "errors"  (or errors (%js-make-array))
                   "name"    "AggregateError"))

(defun %js-make-abort-controller ()
  "AbortController constructor stub."
  (let ((sig (%js-make-object "aborted" nil "reason" +js-undefined+)))
    (%js-make-object "signal" sig
                     "abort"  (lambda (&optional reason)
                                (setf (gethash "aborted" sig) t
                                      (gethash "reason"  sig) (or reason +js-undefined+))))))

(defun %js-make-url (url &optional base)
  "URL constructor stub: minimal href/origin/pathname/search/hash."
  (declare (ignore base))
  (let ((href (%js-to-string url)))
    (%js-make-object "href" href "origin" "" "pathname" "" "search" "" "hash" ""
                     "toString" (lambda () href))))

(defun %js-make-url-search-params (&optional init)
  "URLSearchParams constructor stub."
  (declare (ignore init))
  (%js-make-object "get"      (lambda (_) (declare (ignore _)) +js-null+)
                   "set"      (lambda (_k _v) (declare (ignore _k _v)) +js-undefined+)
                   "has"      (lambda (_) (declare (ignore _)) nil)
                   "toString" (lambda () "")))

(defun %js-make-crypto ()
  "crypto global stub (getRandomValues + randomUUID)."
  (%js-make-object
   "getRandomValues" (lambda (arr)
                       (when (%js-vec-p arr)
                         (loop for i below (length arr)
                               do (setf (aref arr i) (random 256))))
                       arr)
   "randomUUID" (lambda ()
                  (format nil "~8,'0x-~4,'0x-4~3,'0x-~4,'0x-~12,'0x"
                          (random #xffffffff) (random #xffff) (random #xfff)
                          (logior #x8000 (random #x3fff)) (random #xffffffffffff)))))

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
;;;  Standalone global-builtin helpers (referenced in *js-builtin-specs* below)
;;; -----------------------------------------------------------------------
;;;
;;; These must appear BEFORE *js-builtin-specs* because ,#'fn in a defparameter
;;; evaluates (function fn) at load time — sequential file loading requires the
;;; defun to precede its reference.

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
;;;  Complex built-in helpers (named functions rather than inline lambdas)
;;; -----------------------------------------------------------------------
;;;
;;; Keeping logic here and references in *js-builtin-specs* separates data
;;; (the name→function table) from the implementation of each entry.

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

