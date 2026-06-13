;;;; packages/javascript/src/runtime-builtins.lisp — JS built-in dispatch table
;;;;
;;;; *js-builtin-map* maps built-in name strings to CL functions.
;;;; This file must be loaded LAST so all referenced functions are defined.

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
;;;  Built-in dispatch table
;;; -----------------------------------------------------------------------

(defparameter *js-builtin-specs*
  `(;; Type
    ("typeof"                  . ,#'%js-typeof)
    ("instanceof"              . ,#'%js-instanceof)
    ;; Math
    ("Math.abs"                . ,#'%js-math-abs)
    ("Math.floor"              . ,#'%js-math-floor)
    ("Math.ceil"               . ,#'%js-math-ceil)
    ("Math.round"              . ,#'%js-math-round)
    ("Math.trunc"              . ,#'%js-math-trunc)
    ("Math.sign"               . ,#'%js-math-sign)
    ("Math.max"                . ,#'%js-math-max)
    ("Math.min"                . ,#'%js-math-min)
    ("Math.pow"                . ,#'%js-math-pow)
    ("Math.sqrt"               . ,#'%js-math-sqrt)
    ("Math.random"             . ,#'%js-math-random)
    ("Math.log"                . ,#'%js-math-log)
    ("Math.log2"               . ,#'%js-math-log2)
    ("Math.log10"              . ,#'%js-math-log10)
    ("Math.exp"                . ,#'%js-math-exp)
    ("Math.sin"                . ,#'%js-math-sin)
    ("Math.cos"                . ,#'%js-math-cos)
    ("Math.tan"                . ,#'%js-math-tan)
    ("Math.asin"               . ,#'%js-math-asin)
    ("Math.acos"               . ,#'%js-math-acos)
    ("Math.atan"               . ,#'%js-math-atan)
    ("Math.atan2"              . ,#'%js-math-atan2)
    ;; ES2015 hyperbolic + extra Math (map to CL transcendentals)
    ("Math.sinh"               . ,#'%js-math-sinh)
    ("Math.cosh"               . ,#'%js-math-cosh)
    ("Math.tanh"               . ,#'%js-math-tanh)
    ("Math.asinh"              . ,#'%js-math-asinh)
    ("Math.acosh"              . ,#'%js-math-acosh)
    ("Math.atanh"              . ,#'%js-math-atanh)
    ("Math.cbrt"               . ,#'%js-math-cbrt)
    ("Math.expm1"              . ,#'%js-math-expm1)
    ("Math.log1p"              . ,#'%js-math-log1p)
    ("Math.hypot"              . ,#'%js-math-hypot)
    ("Math.clz32"              . ,#'%js-math-clz32)
    ("Math.fround"             . ,#'%js-math-fround)
    ("Math.imul"               . ,#'%js-math-imul)
    ;; Array
    ("Array.isArray"           . ,#'%js-array-is-array)
    ("Array.from"              . ,#'%js-array-from)
    ("Array.of"                . ,#'%js-array-of)
    ;; Object
    ("Object.keys"             . ,#'%js-object-keys)
    ("Object.values"           . ,#'%js-object-values)
    ("Object.entries"          . ,#'%js-object-entries)
    ("Object.assign"           . ,#'%js-object-assign)
    ("Object.create"           . ,#'%js-object-create)
    ("Object.freeze"           . ,(lambda (obj) obj))
    ("Object.fromEntries"      . ,#'%js-object-from-entries)
    ("Object.hasOwn"           . ,#'%js-object-has-own)
    ("Object.is"               . ,#'%js-object-is)
    ("Object.groupBy"          . ,#'%js-object-group-by)
    ;; String
    ("String.fromCharCode"     . ,#'%js-string-from-char-code)
    ("String.fromCodePoint"    . ,#'%js-string-from-code-point)
    ("String.raw"              . ,#'%js-string-raw)
    ;; Promise
    ("Promise.resolve"         . ,#'%js-promise-resolve)
    ("Promise.reject"          . ,#'%js-promise-reject)
    ("Promise.all"             . ,#'%js-promise-all)
    ("Promise.allSettled"      . ,#'%js-promise-all-settled)
    ("Promise.any"             . ,#'%js-promise-any)
    ("Promise.race"            . ,#'%js-promise-race)
    ("Promise.withResolvers"   . ,#'%js-promise-with-resolvers)
    ;; Console
    ("console.log"             . ,#'%js-console-log)
    ("console.error"           . ,#'%js-console-error)
    ("console.warn"            . ,#'%js-console-warn)
    ("console.info"            . ,#'%js-console-log)
    ("console.debug"           . ,#'%js-console-log)
    ;; Number globals
    ("parseInt"                . ,#'%js-parse-int)
    ("parseFloat"              . ,#'%js-parse-float)
    ("isNaN"                   . ,#'%js-is-nan)
    ("isFinite"                . ,#'%js-is-finite)
    ("Number.isNaN"            . ,#'%js-number-is-nan)
    ("Number.isFinite"         . ,#'%js-number-is-finite)
    ("Number.isInteger"        . ,#'%js-number-is-integer)
    ("Number.parseInt"         . ,#'%js-to-number)
    ("Number.parseFloat"       . ,#'%js-to-number)
    ("Number.MAX_SAFE_INTEGER" . ,(lambda () 9007199254740991.0d0))
    ("Number.MIN_SAFE_INTEGER" . ,(lambda () -9007199254740991.0d0))
    ("Number.EPSILON"          . ,(lambda () 2.220446049250313d-16))
    ("Number.MAX_VALUE"        . ,(lambda () most-positive-double-float))
    ;; JSON stubs
    ("JSON.stringify"          . ,(lambda (val &optional _replacer _space)
                                    (declare (ignore _replacer _space))
                                    (%js-json-stringify val)))
    ("JSON.parse"              . ,(lambda (str &optional _reviver)
                                    (declare (ignore _reviver))
                                    (%js-json-parse str)))
    ;; Symbol global (callable object)
    ("Symbol.for"              . ,#'%js-symbol-for)
    ("Symbol.keyFor"           . ,#'%js-symbol-key-for)
    ;; Set constructor — creates an ordered js-set struct
    ("Set"                     . ,(lambda (&optional (iter +js-undefined+))
                                    (let ((s (%js-make-set)))
                                      (when (and (not (eq iter +js-undefined+))
                                                 (not (eq iter +js-null+)))
                                        (%js-for-of iter (lambda (v) (%js-set-add s v))))
                                      s)))
    ;; Map constructor
    ("Map"                     . ,#'%js-make-map)
    ;; WeakMap/WeakSet constructors
    ("WeakMap"                 . ,(lambda (&rest _) (declare (ignore _)) (%js-make-weak-map)))
    ("WeakSet"                 . ,(lambda (&rest _) (declare (ignore _)) (%js-make-weak-set)))
    ;; WeakRef constructor
    ("WeakRef"                 . ,(lambda (target) (%js-make-weak-ref target)))
    ;; Intl API stubs (simplified locale-aware formatting)
    ("Intl.NumberFormat"   . ,(lambda (&optional _locale options)
                                (declare (ignore _locale options))
                                (%js-make-object
                                 "__call__" (lambda (&rest _) (declare (ignore _)) +js-undefined+)
                                 "format"   (lambda (n) (format nil "~,2F" (%js-to-number n)))
                                 "formatToParts" (lambda (n)
                                                   (%js-make-array
                                                    (%js-make-object "type" "integer" "value"
                                                                      (format nil "~D" (truncate (%js-to-number n)))))))))
    ("Intl.DateTimeFormat" . ,(lambda (&optional _locale options)
                                (declare (ignore _locale options))
                                (%js-make-object
                                 "format" (lambda (date)
                                            (if (js-date-p date)
                                                (%js-date-to-iso-string date)
                                                (format nil "~A" date))))))
    ("Intl.Collator"       . ,(lambda (&optional _locale options)
                                (declare (ignore _locale options))
                                (%js-make-object
                                 "compare" (lambda (a b)
                                             (cond ((string< (%js-to-string a) (%js-to-string b)) -1.0d0)
                                                   ((string> (%js-to-string a) (%js-to-string b))  1.0d0)
                                                   (t 0.0d0))))))
    ("Intl.ListFormat"     . ,(lambda (&optional _locale options)
                                (declare (ignore _locale options))
                                (%js-make-object
                                 "format" (lambda (list)
                                            (let ((items (loop for i below (length list)
                                                               collect (%js-to-string (aref list i)))))
                                              (cond ((null items) "")
                                                    ((= (length items) 1) (first items))
                                                    (t (format nil "~{~A~^, ~}" items))))))))
    ("Intl.PluralRules"    . ,(lambda (&optional _locale options)
                                (declare (ignore _locale options))
                                (%js-make-object
                                 "select" (lambda (n) (if (= (%js-to-number n) 1) "one" "other")))))

    ;; TypedArray constructors
    ("Int8Array"           . ,(lambda (&optional arg) (%js-make-typed-array "Int8Array" arg)))
    ("Uint8Array"          . ,(lambda (&optional arg) (%js-make-typed-array "Uint8Array" arg)))
    ("Uint8ClampedArray"   . ,(lambda (&optional arg) (%js-make-typed-array "Uint8ClampedArray" arg)))
    ("Int16Array"          . ,(lambda (&optional arg) (%js-make-typed-array "Int16Array" arg)))
    ("Uint16Array"         . ,(lambda (&optional arg) (%js-make-typed-array "Uint16Array" arg)))
    ("Int32Array"          . ,(lambda (&optional arg) (%js-make-typed-array "Int32Array" arg)))
    ("Uint32Array"         . ,(lambda (&optional arg) (%js-make-typed-array "Uint32Array" arg)))
    ("Float32Array"        . ,(lambda (&optional arg) (%js-make-typed-array "Float32Array" arg)))
    ("Float64Array"        . ,(lambda (&optional arg) (%js-make-typed-array "Float64Array" arg)))
    ;; ES2020 BigInt typed arrays
    ("BigInt64Array"       . ,(lambda (&optional arg) (%js-make-typed-array "BigInt64Array" arg)))
    ("BigUint64Array"      . ,(lambda (&optional arg) (%js-make-typed-array "BigUint64Array" arg)))
    ;; Proxy constructor — simplified: returns a wrapped object
    ;; Full Proxy requires all get/set/has traps integrated into %js-get-prop
    ("Proxy"                   . ,(lambda (target handler)
                                    (let ((ht (%js-make-ht)))
                                      (setf (gethash "__proxy-target__" ht) target
                                            (gethash "__proxy-handler__" ht) handler)
                                      ht)))
    ;; Reflect — static methods mirroring Object/Function operations
    ("Reflect.get"             . ,(lambda (target key &optional _receiver)
                                    (declare (ignore _receiver))
                                    (%js-get-prop target key)))
    ("Reflect.set"             . ,(lambda (target key value &optional _receiver)
                                    (declare (ignore _receiver))
                                    (%js-set-prop target key value)
                                    t))
    ("Reflect.has"             . ,(lambda (target key)
                                    (when (%js-ht-p target)
                                      (nth-value 1 (gethash (%js-to-string key) target)))))
    ("Reflect.deleteProperty"  . ,(lambda (target key)
                                    (when (%js-ht-p target)
                                      (remhash (%js-to-string key) target))
                                    t))
    ("Reflect.ownKeys"         . ,#'%js-object-keys)
    ("Reflect.apply"           . ,(lambda (fn this-arg args)
                                    (apply #'%js-funcall fn this-arg (coerce args 'list))))
    ("Reflect.construct"       . ,(lambda (target args &optional new-target)
                                    (declare (ignore new-target))
                                    (%js-new target (coerce args 'list))))
    ("Reflect.defineProperty"  . ,(lambda (target key descriptor)
                                    (when (and (%js-ht-p target) (%js-ht-p descriptor))
                                      (let ((val (gethash "value" descriptor +js-undefined+)))
                                        (unless (eq val +js-undefined+)
                                          (setf (gethash (%js-to-string key) target) val))))
                                    t))
    ("Reflect.getOwnPropertyDescriptor" . ,(lambda (target key)
                                             (if (%js-ht-p target)
                                                 (let ((val (gethash (%js-to-string key) target +js-undefined+)))
                                                   (if (eq val +js-undefined+) +js-undefined+
                                                       (%js-make-object "value" val "writable" t
                                                                         "enumerable" t "configurable" t)))
                                                 +js-undefined+)))
    ("Reflect.getPrototypeOf"  . ,(lambda (_target) (declare (ignore _target)) +js-null+))
    ("Reflect.setPrototypeOf"  . ,(lambda (target _proto) (declare (ignore target _proto)) t))
    ("Reflect.isExtensible"    . ,(lambda (_target) (declare (ignore _target)) t))
    ("Reflect.preventExtensions" . ,(lambda (_target) (declare (ignore _target)) t))
    ;; BigInt (ES2020) — arbitrary-precision integers
    ("BigInt"                  . ,#'%js-bigint)
    ("BigInt.asIntN"           . ,#'%js-bigint-as-int-n)
    ("BigInt.asUintN"          . ,#'%js-bigint-as-uint-n)
    ;; URI encoding/decoding
    ("encodeURIComponent"      . ,#'%js-encode-uri-component)
    ("decodeURIComponent"      . ,#'%js-decode-uri-component)
    ("encodeURI"               . ,#'%js-encode-uri)
    ("decodeURI"               . ,#'%js-decode-uri)
    ;; Base64 (browser globals)
    ("btoa"                    . ,#'%js-btoa)
    ("atob"                    . ,#'%js-atob)
    ;; TextEncoder/TextDecoder
    ("TextEncoder"             . ,(lambda (&rest _) (declare (ignore _)) (%js-make-text-encoder)))
    ("TextDecoder"             . ,(lambda (&optional encoding &rest _) (declare (ignore _)) (%js-make-text-decoder encoding)))
    ;; eval stub (returns undefined — full dynamic eval not supported)
    ("eval"                    . ,(lambda (x) (declare (ignore x)) +js-undefined+))
    ;; Object.defineProperty / Object.getOwnPropertyDescriptor stubs
    ("Object.defineProperty"   . ,(lambda (obj key descriptor)
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
                                    obj))
    ("Object.defineProperties" . ,(lambda (obj props)
                                    (when (and (%js-ht-p obj) (%js-ht-p props))
                                      (maphash (lambda (k v)
                                                 (when (%js-ht-p v)
                                                   (let ((val (gethash "value" v +js-undefined+)))
                                                     (unless (eq val +js-undefined+)
                                                       (setf (gethash k obj) val)))))
                                               props))
                                    obj))
    ("Object.getOwnPropertyDescriptor" . ,(lambda (obj key)
                                             (if (%js-ht-p obj)
                                                 (let ((val (gethash (%js-to-string key) obj +js-undefined+)))
                                                   (if (eq val +js-undefined+) +js-undefined+
                                                       (%js-make-object "value" val "writable" t
                                                                         "enumerable" t "configurable" t)))
                                                 +js-undefined+)))
    ("Object.getOwnPropertyNames"      . ,#'%js-object-keys)
    ("Object.getOwnPropertySymbols"    . ,(lambda (_obj) (declare (ignore _obj)) (%js-make-array)))
    ;; ES2017: Object.getOwnPropertyDescriptors (plural)
    ("Object.getOwnPropertyDescriptors" . ,(lambda (obj)
                                              (let ((result (%js-make-ht)))
                                                (when (%js-ht-p obj)
                                                  (maphash (lambda (k v)
                                                             (unless (and (stringp k) (>= (length k) 2)
                                                                          (string= k "__" :end1 2))
                                                               (setf (gethash k result)
                                                                     (%js-make-object "value" v "writable" t
                                                                                       "enumerable" t "configurable" t))))
                                                           obj))
                                                result)))
    ("Object.seal"       . ,(lambda (obj) obj))
    ("Object.preventExtensions" . ,(lambda (obj) obj))
    ("Object.isExtensible" . ,(lambda (_obj) (declare (ignore _obj)) t))
    ("Object.isFrozen"   . ,(lambda (_obj) (declare (ignore _obj)) nil))
    ("Object.isSealed"   . ,(lambda (_obj) (declare (ignore _obj)) nil))
    ("Object.setPrototypeOf" . ,(lambda (obj _proto) (declare (ignore _proto)) obj))
    ("Object.getPrototypeOf" . ,(lambda (_obj) (declare (ignore _obj)) +js-null+))
    ;; Function constructor stub
    ("Function"          . ,(lambda (&rest _) (declare (ignore _)) (lambda (&rest __) (declare (ignore __)) +js-undefined+)))
    ;; ES2024: Iterator.from (TC39 iterator protocol)
    ("Iterator.from"     . ,#'%js-iterator-from-iterable)
    ;; ES2025: Promise.try
    ("Promise.try"       . ,(lambda (fn &rest args)
                               (handler-case
                                   (%js-promise-resolve (apply #'%js-funcall fn args))
                                 (js-exception (c)
                                   (%js-promise-reject (js-exception-value c))))))
    ;; ES2026: Math.sumPrecise
    ("Math.sumPrecise"   . ,(lambda (iterable)
                               (let ((sum 0.0d0))
                                 (%js-for-of iterable (lambda (v)
                                   (incf sum (coerce (%js-to-number v) 'double-float))))
                                 sum)))
    ;; ES2026: Error.isError
    ("Error.isError"     . ,(lambda (val)
                               (and (%js-ht-p val)
                                    (or (gethash "message" val)
                                        (gethash "stack" val)))))
    ;; ES2025: FinalizationRegistry
    ("FinalizationRegistry" . ,(lambda (cleanup-fn)
                                   (let ((registry (%js-make-ht)))
                                     (setf (gethash "register" registry)
                                           (lambda (target value &optional _token)
                                             (declare (ignore _token))
                                             ;; In our synchronous model, cleanup is immediate on GC
                                             ;; which we cannot observe — store callback for completeness
                                             (declare (ignore target value cleanup-fn))
                                             +js-undefined+)
                                           (gethash "unregister" registry)
                                           (lambda (_token) (declare (ignore _token)) nil))
                                     registry)))
    ;; WeakRef.prototype.deref
    ;; (WeakRef is already registered — add deref to the method resolver below)
    ;; ES2024: Map.groupBy
    ("Map.groupBy"       . ,#'%js-map-group-by)
    ;; Temporal API (ES2026 — Stage 4)
    ("Temporal"              . ,*js-temporal-global*)
    ("Temporal.Now"          . ,(gethash "Now" *js-temporal-global*))
    ;; ES2025 Uint8Array static methods
    ("Uint8Array.fromHex"    . ,#'%js-uint8-from-hex)
    ("Uint8Array.fromBase64" . ,#'%js-uint8-from-base64)
    ;; ES2024 RegExp.escape
    ("RegExp.escape"    . ,(lambda (str)
                               (with-output-to-string (out)
                                 (loop for ch across (%js-to-string str)
                                       do (when (member ch '(#\\ #\^ #\$ #\. #\| #\? #\* #\+ #\( #\) #\[ #\] #\{ #\} #\/ #\-))
                                            (write-char #\\ out))
                                          (write-char ch out)))))
    ;; Array.prototype.toLocaleString
    ("Array.prototype.toLocaleString" . ,(lambda (arr &optional locales options)
                                             (declare (ignore locales options))
                                             (if (%js-vec-p arr)
                                                 (format nil "~{~A~^,~}" (coerce arr 'list))
                                                 (%js-to-string arr))))
    ;; Array extra statics
    ("Array.fromAsync"   . ,(lambda (iter &optional map-fn _this) (declare (ignore _this))
                               (%js-promise-resolve (%js-array-from iter map-fn))))
    ;; Number extra constants
    ("Number.POSITIVE_INFINITY" . ,(lambda () *js-inf-float*))
    ("Number.NEGATIVE_INFINITY" . ,(lambda () *js-neg-inf-float*))
    ("Number.NaN"               . ,(lambda () *js-nan-float*))
    ("Number.MIN_VALUE"         . ,(lambda () 5.0d-324))
    ;; Math constants
    ("Math.E"    . ,(lambda () (exp 1.0d0)))
    ("Math.LN2"  . ,(lambda () (log 2.0d0)))
    ("Math.LN10" . ,(lambda () (log 10.0d0)))
    ("Math.LOG2E"  . ,(lambda () (log (exp 1.0d0) 2.0d0)))
    ("Math.LOG10E" . ,(lambda () (/ 1.0d0 (log 10.0d0))))
    ("Math.PI"     . ,(lambda () pi))
    ("Math.SQRT1_2" . ,(lambda () (sqrt 0.5d0)))
    ("Math.SQRT2"   . ,(lambda () (sqrt 2.0d0)))
    ;; Error.cause, Error.stack (ES2022)
    ("AggregateError"          . ,(lambda (errors message &optional _opts)
                                    (declare (ignore _opts))
                                    (let ((err (%js-make-object
                                                "message" (%js-to-string (or message ""))
                                                "errors"  (or errors (%js-make-array))
                                                "name"    "AggregateError")))
                                      err)))
    ;; console extras
    ("console.table"    . ,(lambda (&rest args)
                              (format t "~{~A~^ ~}~%" (mapcar #'%js-to-string args))
                              +js-undefined+))
    ("console.time"     . ,(lambda (&optional _label) (declare (ignore _label)) +js-undefined+))
    ("console.timeEnd"  . ,(lambda (&optional _label) (declare (ignore _label)) +js-undefined+))
    ("console.timeLog"  . ,(lambda (&rest _) (declare (ignore _)) +js-undefined+))
    ("console.count"    . ,(lambda (&optional _label) (declare (ignore _label)) +js-undefined+))
    ("console.countReset" . ,(lambda (&optional _label) (declare (ignore _label)) +js-undefined+))
    ("console.group"    . ,(lambda (&rest _) (declare (ignore _)) +js-undefined+))
    ("console.groupEnd" . ,(lambda () +js-undefined+))
    ("console.assert"   . ,(lambda (condition &rest args)
                              (unless (%js-truthy condition)
                                (format *error-output* "Assertion failed: ~{~A~^ ~}~%"
                                        (mapcar #'%js-to-string args)))
                              +js-undefined+))
    ("console.dir"      . ,(lambda (obj &rest _) (declare (ignore _))
                              (format t "~A~%" (%js-to-string obj)) +js-undefined+))
    ("console.trace"    . ,(lambda (&rest args)
                              (format t "Trace: ~{~A~^ ~}~%" (mapcar #'%js-to-string args))
                              +js-undefined+))
    ;; globalThis extras
    ("Atomics"           . ,(lambda () +js-undefined+))
    ("SharedArrayBuffer" . ,(lambda (n) (%js-make-typed-array "Uint8Array" n)))
    ;; URL / URLSearchParams stubs
    ("URL"               . ,(lambda (url &optional base)
                               (declare (ignore base))
                               (%js-make-object "href" (%js-to-string url)
                                                "origin" ""
                                                "pathname" ""
                                                "search" ""
                                                "hash" ""
                                                "toString" (lambda () (%js-to-string url)))))
    ("URLSearchParams"   . ,(lambda (&optional init)
                               (declare (ignore init))
                               (%js-make-object "get" (lambda (_) (declare (ignore _)) +js-null+)
                                                "set" (lambda (_k _v) (declare (ignore _k _v)) +js-undefined+)
                                                "has" (lambda (_) (declare (ignore _)) nil)
                                                "toString" (lambda () ""))))
    ;; AbortController / AbortSignal
    ("AbortController"   . ,(lambda ()
                               (let ((sig (%js-make-object "aborted" nil "reason" +js-undefined+)))
                                 (%js-make-object "signal" sig
                                                  "abort"  (lambda (&optional reason)
                                                             (setf (gethash "aborted" sig) t
                                                                   (gethash "reason" sig) (or reason +js-undefined+)))))))
    ("AbortSignal"       . ,(lambda () (%js-make-object "aborted" nil "reason" +js-undefined+)))
    ;; Performance API extras
    ("performance"       . ,(%js-make-object "now" (lambda ()
                                                      (coerce (- (get-internal-real-time) 0) 'double-float))
                                             "mark" (lambda (&rest _) (declare (ignore _)) +js-undefined+)
                                             "measure" (lambda (&rest _) (declare (ignore _)) +js-undefined+)
                                             "getEntries" (lambda () (%js-make-array))
                                             "clearMarks" (lambda (&rest _) (declare (ignore _)) +js-undefined+)))
    ;; Crypto (stub)
    ("crypto"            . ,(%js-make-object
                              "getRandomValues" (lambda (arr)
                                                  (when (%js-vec-p arr)
                                                    (loop for i below (length arr)
                                                          do (setf (aref arr i) (random 256))))
                                                  arr)
                              "randomUUID" (lambda ()
                                             (format nil "~8,'0x-~4,'0x-4~3,'0x-~4,'0x-~12,'0x"
                                                     (random #xffffffff)
                                                     (random #xffff)
                                                     (random #xfff)
                                                     (logior #x8000 (random #x3fff))
                                                     (random #xffffffffffff)))))
    ;; structuredClone
    ("structuredClone"         . ,#'%js-structured-clone)
    ;; queueMicrotask / setTimeout stubs (synchronous in our model)
    ("queueMicrotask"          . ,#'%js-queue-microtask)
    ("setTimeout"              . ,#'%js-set-timeout)
    ("clearTimeout"            . ,#'%js-clear-timer)
    ("setInterval"             . ,#'%js-set-interval)
    ("clearInterval"           . ,#'%js-clear-timer)
    ;; RegExp constructor
    ("RegExp"                  . ,(lambda (pattern &optional flags)
                                    (%js-make-regex (%js-to-string pattern)
                                                    (if (eq flags +js-undefined+) "" (%js-to-string flags)))))
    ;; Date constructor
    ("Date"                    . ,#'%js-make-date)
    ("Date.now"                . ,(lambda () (coerce (%js-date-now) 'double-float)))
    ("Date.parse"              . ,#'%js-date-parse-string)
    ;; Number wrapper function / constructor
    ("Number"                  . ,#'%js-to-number)
    ;; String wrapper function
    ("String"                  . ,#'%js-to-string)
    ;; Boolean wrapper
    ("Boolean"                 . ,#'%js-truthy)
    ;; Error constructors — prototype-based (instanceof works)
    ("Error"                   . ,*js-error-class*)
    ("TypeError"               . ,*js-type-error-class*)
    ("RangeError"              . ,*js-range-error-class*)
    ("ReferenceError"          . ,*js-reference-error-class*)
    ("SyntaxError"             . ,*js-syntax-error-class*)
    ("EvalError"               . ,*js-eval-error-class*)
    ("URIError"                . ,*js-uri-error-class*))
  "Alist of (name . function) specs used to build *js-builtin-map*.")

(defun %build-js-builtin-map ()
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (spec *js-builtin-specs*)
      (setf (gethash (car spec) ht) (cdr spec)))
    ht))

(defvar *js-builtin-map* (%build-js-builtin-map)
  "Dispatch table from JS built-in name to CL function.")

(defun %js-builtin-ref (name)
  "Return the host function/value registered for builtin NAME in *js-builtin-map*,
or +js-undefined+.  Used to bind standalone global builtins to a runtime VALUE so
`typeof structuredClone' and `const f = structuredClone; f(x)' work; direct calls
`structuredClone(x)' are handled separately by *js-coercion-call-helpers*, which
lowers to the named %js-* helper the direct-call codegen can dispatch."
  (or (gethash name *js-builtin-map*) +js-undefined+))

(defun %js-make-namespace-object (prefix)
  "Build a JS namespace global object (Math, JSON, …) from *js-builtin-specs*
entries whose key is PREFIX + '.' + property. A property whose name is entirely
uppercase (a constant such as Math.PI / Number.MAX_SAFE_INTEGER) has its zero-arg
spec called to materialize the value; method properties keep their function. This
derives the object straight from the existing dispatch table, so it stays complete
and never references a helper that does not exist."
  (let ((plen (length prefix))
        (pairs nil))
    (dolist (spec *js-builtin-specs*)
      (let ((key (car spec)))
        (when (and (> (length key) (1+ plen))
                   (string= prefix key :end2 plen)
                   (char= (char key plen) #\.))
          (let* ((prop (subseq key (1+ plen)))
                 (val (cdr spec))
                 (constant-p (and (plusp (length prop))
                                  (every (lambda (c)
                                           (or (and (alpha-char-p c) (upper-case-p c))
                                               (digit-char-p c) (char= c #\_)))
                                         prop))))
            (push prop pairs)
            (push (if (and constant-p (functionp val)) (funcall val) val) pairs)))))
    (apply #'%js-make-object (nreverse pairs))))

(defun %js-make-math ()
  "Construct the JS Math global object (constants + methods)."
  (%js-make-namespace-object "Math"))

(defun %js-make-json ()
  "Construct the JS JSON global object (stringify / parse)."
  (%js-make-namespace-object "JSON"))

;;; -----------------------------------------------------------------------
;;;  JS program prelude — declarative global binding table
;;; -----------------------------------------------------------------------
;;;
;;; Each entry is (kind js-name cl-symbol-or-value):
;;;   :call      — AST call  (cl-symbol &rest no-args)    e.g. (%js-make-console)
;;;   :var       — AST var   cl-symbol                     e.g. *js-error-class*
;;;   :namespace — namespace object built from the builtin dispatch table
;;;   :quote     — AST quote of cl-symbol-or-value         e.g. :js-undefined
;;;   :builtin   — %js-builtin-ref lookup by key string     e.g. "Set"
;;;
;;; Add new globals here; js-program-forms consumes this table automatically.

(defparameter *js-prelude-global-specs*
  '((:call      "console"             %js-make-console)
    (:var        "Symbol"              *js-symbol-global*)
    (:quote      "undefined"           :js-undefined)
    (:var        "Infinity"            *js-inf-float*)
    (:var        "NaN"                 *js-nan-float*)
    (:var        "Date"                %js-make-date)
    ;; Namespace objects — built from *js-builtin-specs* entries whose key is PREFIX.PROP
    (:namespace  "JSON"                "JSON")
    (:namespace  "Math"                "Math")
    (:namespace  "Object"              "Object")
    (:namespace  "Reflect"             "Reflect")
    (:namespace  "Number"              "Number")
    (:namespace  "Array"               "Array")
    (:namespace  "String"              "String")
    (:namespace  "Promise"             "Promise")
    (:namespace  "Intl"                "Intl")
    (:call       "globalThis"          %js-make-object)
    (:var        "Temporal"            *js-temporal-global*)
    (:var        "BigInt"              %js-bigint)
    ;; URI helpers
    (:var        "encodeURIComponent"  %js-encode-uri-component)
    (:var        "decodeURIComponent"  %js-decode-uri-component)
    (:var        "encodeURI"           %js-encode-uri)
    (:var        "decodeURI"           %js-decode-uri)
    (:var        "btoa"                %js-btoa)
    (:var        "atob"                %js-atob)
    ;; Error class hierarchy
    (:var        "Error"               *js-error-class*)
    (:var        "TypeError"           *js-type-error-class*)
    (:var        "RangeError"          *js-range-error-class*)
    (:var        "ReferenceError"      *js-reference-error-class*)
    (:var        "SyntaxError"         *js-syntax-error-class*)
    ;; Collection constructors
    (:var        "Map"                 %js-make-map)
    (:var        "WeakMap"             %js-make-weak-map)
    (:var        "WeakSet"             %js-make-weak-set)
    ;; Set/timer globals are inline lambdas with no dedicated %js-* symbol;
    ;; look them up via %js-builtin-ref so they stay in sync with *js-builtin-specs*.
    (:builtin    "Set"                 "Set")
    ;; Numeric parsing globals
    (:var        "parseInt"            %js-parse-int)
    (:var        "parseFloat"          %js-parse-float)
    (:var        "isNaN"               %js-is-nan)
    (:var        "isFinite"            %js-is-finite)
    ;; Standalone global builtins — bound via %js-builtin-ref (inline lambdas)
    (:builtin    "structuredClone"     "structuredClone")
    (:builtin    "queueMicrotask"      "queueMicrotask")
    (:builtin    "setTimeout"          "setTimeout")
    (:builtin    "setInterval"         "setInterval")
    (:builtin    "clearTimeout"        "clearTimeout")
    (:builtin    "clearInterval"       "clearInterval"))
  "Declarative prelude table: (kind js-name cl-symbol) emitted as defvar AST nodes
before every compiled JS program so standard globals are available.")

(defun %js-prelude-form (spec)
  "Emit a defparameter AST node for one entry in *js-prelude-global-specs*."
  (destructuring-bind (kind js-name cl-sym) spec
    (make-ast-defvar
     :name  (js-ident-sym js-name)
     :kind  'defparameter
     :value (ecase kind
               (:call      (%js-call cl-sym))
               (:var       (make-ast-var :name cl-sym))
               (:namespace (%js-call '%js-make-namespace-object (make-ast-quote :value cl-sym)))
               (:quote     (make-ast-quote :value cl-sym))
               (:builtin   (%js-call '%js-builtin-ref (make-ast-quote :value cl-sym)))))))

(defun js-program-forms (source &key strict-mode module-p)
  "Parse JS SOURCE prepending the standard-globals prelude.
Returns top-level AST forms ready for the compiler backend."
  (append (mapcar #'%js-prelude-form *js-prelude-global-specs*)
          (parse-js-source source :strict-mode strict-mode :module-p module-p)))
