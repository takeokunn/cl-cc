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
    ("Math.sinh"               . ,(lambda (x) (sinh (coerce (%js-to-number x) 'double-float))))
    ("Math.cosh"               . ,(lambda (x) (cosh (coerce (%js-to-number x) 'double-float))))
    ("Math.tanh"               . ,(lambda (x) (tanh (coerce (%js-to-number x) 'double-float))))
    ("Math.asinh"              . ,(lambda (x) (asinh (coerce (%js-to-number x) 'double-float))))
    ("Math.acosh"              . ,(lambda (x) (acosh (coerce (%js-to-number x) 'double-float))))
    ("Math.atanh"              . ,(lambda (x) (atanh (coerce (%js-to-number x) 'double-float))))
    ("Math.cbrt"               . ,(lambda (x)
                                    (let ((v (coerce (%js-to-number x) 'double-float)))
                                      (if (minusp v)
                                          (- (expt (- v) (/ 1.0d0 3.0d0)))
                                          (expt v (/ 1.0d0 3.0d0))))))
    ("Math.expm1"              . ,(lambda (x) (- (exp (coerce (%js-to-number x) 'double-float)) 1.0d0)))
    ("Math.log1p"              . ,(lambda (x) (log (+ 1.0d0 (coerce (%js-to-number x) 'double-float)))))
    ("Math.hypot"              . ,#'%js-math-hypot)
    ("Math.clz32"              . ,#'%js-math-clz32)
    ("Math.fround"             . ,#'%js-math-fround)
    ("Math.imul"               . ,#'%js-math-imul)
    ;; Array
    ("Array.isArray"           . ,#'%js-array-is-array)
    ("Array.from"              . ,#'%js-array-from)
    ("Array.of"                . ,#'%js-make-array)
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
    ("Number.isNaN"            . ,(lambda (x) (%js-float-nan-p x)))
    ("Number.isFinite"         . ,(lambda (x)
                                    (and (numberp x) (not (%js-float-nan-p x)) (not (%js-float-infinity-p x)))))
    ("Number.isInteger"        . ,(lambda (x)
                                    (and (numberp x) (not (%js-float-nan-p x)) (= x (truncate x)))))
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
    ;; Set constructor — %js-make-ht creates a hash-table used as Set
    ("Set"                     . ,(lambda (&optional (iter +js-undefined+))
                                    (let ((s (%js-make-ht)))
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

    ;; Array.prototype.group / groupToMap (ES2024) — static-ish
    ("Array.from"          . ,#'%js-array-from)
    ("Array.isArray"       . ,#'%js-array-is-array)
    ("Array.of"            . ,#'%js-array-of)

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
    ("BigInt.asIntN"           . ,(lambda (width bigint)
                                    (let* ((w (truncate (%js-to-number width)))
                                           (v (if (js-bigint-p bigint) (js-bigint-value bigint) (truncate bigint)))
                                           (modulus (expt 2 w))
                                           (half-modulus (expt 2 (1- w)))
                                           (masked (mod v modulus)))
                                      (%make-js-bigint (if (>= masked half-modulus) (- masked modulus) masked)))))
    ("BigInt.asUintN"          . ,(lambda (width bigint)
                                    (let* ((w (truncate (%js-to-number width)))
                                           (v (if (js-bigint-p bigint) (js-bigint-value bigint) (truncate bigint))))
                                      (%make-js-bigint (mod v (expt 2 w))))))
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
    ;; ES2024: Iterator.from + Iterator helpers (TC39 iterator protocol)
    ("Iterator.from"     . ,(lambda (iterable)
                               (let ((iter
                                      (cond
                                        ;; Already an iterator (has "next")
                                        ((and (%js-ht-p iterable) (gethash "next" iterable))
                                         iterable)
                                        ;; Iterable (has @@iterator)
                                        ((and (%js-ht-p iterable) (gethash "@@iterator" iterable))
                                         (%js-funcall (gethash "@@iterator" iterable)))
                                        ;; Array
                                        ((%js-vec-p iterable)
                                         (%js-make-generator (lambda ()
                                           (loop for i below (length iterable)
                                                 do (%js-yield (aref iterable i))))))
                                        ;; String
                                        ((stringp iterable)
                                         (%js-make-generator (lambda ()
                                           (loop for ch across iterable
                                                 do (%js-yield (string ch))))))
                                        (t (%js-make-object "next"
                                             (lambda () (%js-make-object "value" +js-undefined+ "done" t)))))))
                                 ;; Add iterator helper methods
                                 (setf (gethash "map" iter)
                                       (lambda (fn)
                                         (%js-make-generator (lambda ()
                                           (%js-for-of iter (lambda (v)
                                             (%js-yield (%js-funcall fn v)))))))
                                       (gethash "filter" iter)
                                       (lambda (fn)
                                         (%js-make-generator (lambda ()
                                           (%js-for-of iter (lambda (v)
                                             (when (%js-truthy (%js-funcall fn v))
                                               (%js-yield v)))))))
                                       (gethash "take" iter)
                                       (lambda (n)
                                         (let ((count (list 0)))
                                           (%js-make-generator (lambda ()
                                             (%js-for-of iter (lambda (v)
                                               (when (< (car count) (truncate (%js-to-number n)))
                                                 (%js-yield v)
                                                 (incf (car count)))))))))
                                       (gethash "drop" iter)
                                       (lambda (n)
                                         (let ((count (list 0)))
                                           (%js-make-generator (lambda ()
                                             (%js-for-of iter (lambda (v)
                                               (if (< (car count) (truncate (%js-to-number n)))
                                                   (incf (car count))
                                                   (%js-yield v))))))))
                                       (gethash "toArray" iter)
                                       (lambda ()
                                         (let ((arr (%js-make-vec 0)))
                                           (%js-for-of iter (lambda (v) (vector-push-extend v arr)))
                                           arr))
                                       (gethash "forEach" iter)
                                       (lambda (fn) (%js-for-of iter fn) +js-undefined+)
                                       (gethash "reduce" iter)
                                       (lambda (fn &optional init)
                                         (let ((acc (if (eq init +js-undefined+) (list nil nil) (list init t))))
                                           (%js-for-of iter (lambda (v)
                                             (if (cadr acc)
                                                 (setf (car acc) (%js-funcall fn (car acc) v))
                                                 (setf (car acc) v (cadr acc) t))))
                                           (car acc)))
                                       (gethash "some" iter)
                                       (lambda (fn)
                                         (let ((found nil))
                                           (%js-for-of iter (lambda (v)
                                             (when (%js-truthy (%js-funcall fn v))
                                               (setf found t))))
                                           found))
                                       (gethash "every" iter)
                                       (lambda (fn)
                                         (let ((all t))
                                           (%js-for-of iter (lambda (v)
                                             (unless (%js-truthy (%js-funcall fn v))
                                               (setf all nil))))
                                           all))
                                       (gethash "find" iter)
                                       (lambda (fn)
                                         (let ((result +js-undefined+))
                                           (%js-for-of iter (lambda (v)
                                             (when (%js-truthy (%js-funcall fn v))
                                               (setf result v))))
                                           result))
                                       (gethash "@@iterator" iter)
                                       (lambda () iter))
                                 iter)))
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
    ;; ES2024: Object.groupBy (ensure it's registered)
    ("Map.groupBy"       . ,(lambda (iterable key-fn)
                               (let ((result (%js-make-map)))
                                 (%js-for-of iterable
                                   (lambda (item)
                                     (let* ((key (%js-funcall key-fn item))
                                            (existing (%js-map-get result key)))
                                       (if (eq existing +js-undefined+)
                                           (progn
                                             (let ((arr (%js-make-vec 0)))
                                               (vector-push-extend item arr)
                                               (%js-map-set result key arr)))
                                           (vector-push-extend item existing)))))
                                 result)))
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
    ;; Generator/iterator helpers
    ("Array.from"        . ,#'%js-array-from)  ; re-register to pick up iterator objects
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
    ("structuredClone"         . ,(lambda (val &optional _opts)
                                    (declare (ignore _opts))
                                    (%js-deep-clone val)))
    ;; queueMicrotask / setTimeout stubs (synchronous in our model)
    ("queueMicrotask"          . ,(lambda (fn) (%js-funcall fn) +js-undefined+))
    ("setTimeout"              . ,(lambda (fn &optional _delay &rest _args)
                                    (declare (ignore _delay _args))
                                    (%js-funcall fn) 0))
    ("clearTimeout"            . ,(lambda (&rest _) (declare (ignore _)) +js-undefined+))
    ("setInterval"             . ,(lambda (fn &optional _delay &rest _args)
                                    (declare (ignore _delay _args))
                                    (%js-funcall fn) 0))
    ("clearInterval"           . ,(lambda (&rest _) (declare (ignore _)) +js-undefined+))
    ;; Performance API stub
    ("performance.now"         . ,(lambda () (coerce (- (get-universal-time) 2208988800) 'double-float)))
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

(defun js-program-forms (source &key strict-mode module-p)
  "Parse JS SOURCE and prepend the runtime-global prelude so compiled programs
have the standard globals available. Returns a list of shared-AST top-level
forms ready for the compiler backend — the JS analog of PARSE-ALL-FORMS.

Currently seeds `console' as a defparameter'd global object built by the bridged
host helper %JS-MAKE-CONSOLE; member access `console.log' then resolves through
%js-get-prop to a bridged host function. Add further globals (Math, JSON, …) here."
  (list* (make-ast-defvar
          :name (js-ident-sym "console")
          :value (make-ast-call :func (make-ast-var :name '%js-make-console) :args nil)
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Symbol")
          :value (make-ast-var :name '*js-symbol-global*)
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "undefined")
          :value (make-ast-quote :value +js-undefined+)
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Infinity")
          :value (make-ast-var :name '*js-inf-float*)
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "NaN")
          :value (make-ast-var :name '*js-nan-float*)
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Date")
          :value (make-ast-var :name '%js-make-date)
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "JSON")
          :value (make-ast-call :func (make-ast-var :name '%js-make-json) :args nil)
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Math")
          :value (make-ast-call :func (make-ast-var :name '%js-make-math) :args nil)
          :kind 'defparameter)
         ;; Static-method namespaces (Object.keys, Reflect.ownKeys, Number.isInteger,
         ;; Array.isArray, String.fromCharCode, Promise.resolve, Intl.*). Each is
         ;; built straight from the *js-builtin-specs* dispatch table by
         ;; %js-make-namespace-object, so it stays complete. (The constructor/callable
         ;; forms — Number(x), new Array() — are a separate gap; these objects are not
         ;; yet callable.) Without these globals, Object.keys({a:1}) found no `Object'.
         (make-ast-defvar
          :name (js-ident-sym "Object")
          :value (make-ast-call :func (make-ast-var :name '%js-make-namespace-object)
                                :args (list (make-ast-quote :value "Object")))
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Reflect")
          :value (make-ast-call :func (make-ast-var :name '%js-make-namespace-object)
                                :args (list (make-ast-quote :value "Reflect")))
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Number")
          :value (make-ast-call :func (make-ast-var :name '%js-make-namespace-object)
                                :args (list (make-ast-quote :value "Number")))
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Array")
          :value (make-ast-call :func (make-ast-var :name '%js-make-namespace-object)
                                :args (list (make-ast-quote :value "Array")))
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "String")
          :value (make-ast-call :func (make-ast-var :name '%js-make-namespace-object)
                                :args (list (make-ast-quote :value "String")))
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Promise")
          :value (make-ast-call :func (make-ast-var :name '%js-make-namespace-object)
                                :args (list (make-ast-quote :value "Promise")))
          :kind 'defparameter)
         (make-ast-defvar
          :name (js-ident-sym "Intl")
          :value (make-ast-call :func (make-ast-var :name '%js-make-namespace-object)
                                :args (list (make-ast-quote :value "Intl")))
          :kind 'defparameter)
         ;; globalThis — reference to the global object (stub: empty object)
         (make-ast-defvar
          :name (js-ident-sym "globalThis")
          :value (make-ast-call :func (make-ast-var :name '%js-make-object) :args nil)
          :kind 'defparameter)
         ;; Temporal global (ES2026)
         (make-ast-defvar :name (js-ident-sym "Temporal")
                          :value (make-ast-var :name '*js-temporal-global*)
                          :kind 'defparameter)
         ;; BigInt global
         (make-ast-defvar :name (js-ident-sym "BigInt")
                          :value (make-ast-var :name '%js-bigint)
                          :kind 'defparameter)
         ;; URI helpers
         (make-ast-defvar :name (js-ident-sym "encodeURIComponent")
                          :value (make-ast-var :name '%js-encode-uri-component)
                          :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "decodeURIComponent")
                          :value (make-ast-var :name '%js-decode-uri-component)
                          :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "encodeURI")
                          :value (make-ast-var :name '%js-encode-uri)
                          :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "decodeURI")
                          :value (make-ast-var :name '%js-decode-uri)
                          :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "btoa")
                          :value (make-ast-var :name '%js-btoa)
                          :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "atob")
                          :value (make-ast-var :name '%js-atob)
                          :kind 'defparameter)
         ;; Error class hierarchy
         (make-ast-defvar :name (js-ident-sym "Error")          :value (make-ast-var :name '*js-error-class*)             :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "TypeError")      :value (make-ast-var :name '*js-type-error-class*)        :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "RangeError")     :value (make-ast-var :name '*js-range-error-class*)       :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "ReferenceError") :value (make-ast-var :name '*js-reference-error-class*)   :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "SyntaxError")    :value (make-ast-var :name '*js-syntax-error-class*)      :kind 'defparameter)
         ;; Well-known constructors as top-level identifiers
         (make-ast-defvar :name (js-ident-sym "Map")          :value (make-ast-var :name '%js-make-map)        :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "WeakMap")      :value (make-ast-var :name '%js-make-weak-map)   :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "WeakSet")      :value (make-ast-var :name '%js-make-weak-set)   :kind 'defparameter)
         ;; Global number-parsing functions
         (make-ast-defvar :name (js-ident-sym "parseInt")     :value (make-ast-var :name '%js-parse-int)       :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "parseFloat")   :value (make-ast-var :name '%js-parse-float)     :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "isNaN")        :value (make-ast-var :name '%js-is-nan)          :kind 'defparameter)
         (make-ast-defvar :name (js-ident-sym "isFinite")     :value (make-ast-var :name '%js-is-finite)       :kind 'defparameter)
         (parse-js-source source :strict-mode strict-mode :module-p module-p)))

;;; -----------------------------------------------------------------------
;;;  Method dispatch: obj.method resolved to a bound callable
;;; -----------------------------------------------------------------------
;;;
;;; Loaded last (after every %js-array-*/%js-string-* helper is defined) so the
;;; tables can name the functions directly. %js-get-prop delegates here via the
;;; *js-method-resolver* hook installed at the bottom — `arr.push'/`nums.map(f)'/
;;; `s.split(",")' then resolve to a closure that prepends the receiver, exactly
;;; like console.log resolves to a function value the VM can invoke.

(defparameter *js-array-method-table*
  (list (cons "push" #'%js-array-push)         (cons "pop" #'%js-array-pop)
        (cons "shift" #'%js-array-shift)        (cons "unshift" #'%js-array-unshift)
        (cons "map" #'%js-array-map)            (cons "forEach" #'%js-array-for-each)
        (cons "filter" #'%js-array-filter)      (cons "reduce" #'%js-array-reduce)
        (cons "reduceRight" #'%js-array-reduce-right)
        (cons "find" #'%js-array-find)          (cons "findIndex" #'%js-array-find-index)
        (cons "some" #'%js-array-some)          (cons "every" #'%js-array-every)
        (cons "includes" #'%js-array-includes)  (cons "indexOf" #'%js-array-index-of)
        (cons "lastIndexOf" #'%js-array-last-index-of)
        (cons "join" #'%js-array-join)          (cons "slice" #'%js-array-slice)
        (cons "splice" #'%js-array-splice)      (cons "concat" #'%js-array-concat)
        (cons "reverse" #'%js-array-reverse)    (cons "sort" #'%js-array-sort)
        (cons "flat" #'%js-array-flat)          (cons "flatMap" #'%js-array-flat-map)
        (cons "fill" #'%js-array-fill)          (cons "copyWithin" #'%js-array-copy-within)
        (cons "entries" #'%js-array-entries)    (cons "keys" #'%js-array-keys)
        (cons "values"
              (lambda (arr)
                ;; Return an iterator that yields successive elements
                (let ((i (list 0)))
                  (%js-make-generator (lambda ()
                    (loop while (< (car i) (length arr))
                          do (%js-yield (aref arr (car i)))
                             (incf (car i))))))))
        ;; @@iterator — makes arrays iterable
        (cons "@@iterator"
              (lambda (arr)
                (let ((i (list 0)))
                  (%js-make-object
                   "next" (lambda ()
                            (if (< (car i) (length arr))
                                (prog1 (%js-make-object "value" (aref arr (car i)) "done" nil)
                                  (incf (car i)))
                                (%js-make-object "value" +js-undefined+ "done" t)))
                   "@@iterator" (lambda () (gethash "@@iterator" arr))))))
        ;; ES2024
        (cons "group"          #'%js-array-group)
        (cons "groupToMap"     #'%js-array-group-to-map)
        ;; ES2023
        (cons "toReversed"      #'%js-array-to-reversed)
        (cons "toSorted"        #'%js-array-to-sorted)
        (cons "toSpliced"       #'%js-array-to-spliced)
        (cons "with"            #'%js-array-with)
        (cons "findLast"        #'%js-array-find-last)
        (cons "findLastIndex"   #'%js-array-find-last-index)
        (cons "at"              #'%js-array-at))
  "Alist of JS Array.prototype method name -> host helper (receiver is ARR, first arg).")

(defparameter *js-string-method-table*
  (list (cons "slice" #'%js-string-slice)       (cons "indexOf" #'%js-string-index-of)
        (cons "lastIndexOf" #'%js-string-last-index-of)
        (cons "includes" #'%js-string-includes) (cons "startsWith" #'%js-string-starts-with)
        (cons "endsWith" #'%js-string-ends-with)(cons "split" #'%js-string-split)
        (cons "replace" #'%js-string-replace)   (cons "replaceAll" #'%js-string-replace-all)
        (cons "padStart" #'%js-string-pad-start)(cons "padEnd" #'%js-string-pad-end)
        (cons "at" #'%js-string-at)             (cons "repeat" #'%js-string-repeat)
        (cons "charAt" #'%js-string-char-at)    (cons "charCodeAt" #'%js-string-char-code-at)
        (cons "concat" #'%js-string-concat)     (cons "match" #'%js-string-match)
        (cons "matchAll" #'%js-string-match-all)(cons "search" #'%js-string-search)
        (cons "toUpperCase" #'%js-string-to-upper-case)
        (cons "toLowerCase" #'%js-string-to-lower-case)
        (cons "trim" #'%js-string-trim)         (cons "trimStart" #'%js-string-trim-start)
        (cons "trimEnd" #'%js-string-trim-end)
        (cons "codePointAt" #'%js-string-code-point-at)
        (cons "normalize" #'%js-string-normalize)
        (cons "substr" #'%js-string-slice)       ; deprecated alias
        (cons "substring" #'%js-string-substring)
        (cons "valueOf" (lambda (s) s))
        (cons "toString" (lambda (s) s))
        ;; ES2015 iterator protocol on strings
        ;; ES2024
        (cons "toWellFormed"       #'%js-string-to-well-formed)
        (cons "isWellFormed"       #'%js-string-is-well-formed)
        ;; ES2015 locale-aware
        (cons "toLocaleLowerCase"  #'%js-string-to-locale-lower-case)
        (cons "toLocaleUpperCase"  #'%js-string-to-locale-upper-case)
        (cons "localeCompare"      #'%js-string-locale-compare)
        (cons "@@iterator"
              (lambda (s)
                (let ((i (list 0)))
                  (%js-make-object
                   "next" (lambda ()
                            (if (< (car i) (length s))
                                (prog1 (%js-make-object "value" (string (char s (car i))) "done" nil)
                                  (incf (car i)))
                                (%js-make-object "value" +js-undefined+ "done" t))))))))
  "Alist of JS String.prototype method name -> host helper (receiver is S, first arg).")

(defparameter *js-set-method-table*
  (list (cons "add" #'%js-set-add)         (cons "delete" #'%js-set-delete)
        (cons "has" #'%js-set-has)         (cons "clear" #'%js-set-clear)
        (cons "forEach" #'%js-set-for-each)
        (cons "keys" (lambda (s) (%js-set-keys s)))
        (cons "values" (lambda (s) (%js-set-keys s)))  ; Set keys = values
        (cons "entries" #'%js-set-entries)
        (cons "union" #'%js-set-union)
        (cons "intersection" #'%js-set-intersection)
        (cons "difference" #'%js-set-difference)
        (cons "symmetricDifference" #'%js-set-symmetric-difference)
        (cons "isSubsetOf" #'%js-set-is-subset-of)
        (cons "isSupersetOf" #'%js-set-is-superset-of)
        (cons "isDisjointFrom" #'%js-set-is-disjoint-from))
  "Alist of JS Set.prototype method name -> host helper.")

(defparameter *js-map-method-table*
  (list (cons "set" #'%js-map-set)     (cons "get" #'%js-map-get)
        (cons "has" #'%js-map-has)     (cons "delete" #'%js-map-delete)
        (cons "clear" #'%js-map-clear) (cons "forEach" #'%js-map-for-each)
        (cons "keys" (lambda (m) (%js-map-keys m)))
        (cons "values" (lambda (m) (%js-map-values m)))
        (cons "entries" (lambda (m) (%js-map-entries m))))
  "Alist of JS Map.prototype method name -> host helper.")

(defun %js-bound-method (table receiver name)
  "Look NAME up in TABLE; return a closure that calls the helper with RECEIVER
prepended (so `receiver.name(a,b)' becomes (helper receiver a b)), else undefined."
  (let ((entry (assoc name table :test #'string=)))
    (if entry
        (let ((fn (cdr entry)))
          (lambda (&rest args) (apply fn receiver args)))
        +js-undefined+)))

(defun %js-strip-trailing-dot (s)
  "Drop a single trailing '.' that CL's ~,0F leaves (\"8.\" -> \"8\")."
  (if (and (plusp (length s)) (char= (char s (1- (length s))) #\.))
      (subseq s 0 (1- (length s)))
      s))

(defun %js-number-to-precision (x p)
  "JS Number.prototype.toPrecision(P): X rendered to P significant digits, using
fixed notation when the decimal exponent is in [-6, P), exponential otherwise —
matching the ECMAScript algorithm."
  (cond
    ((or (< p 1) (> p 100)) (format nil "~A" x))      ; JS RangeError; degrade gracefully
    ((zerop x)
     (if (= p 1) "0" (format nil "0.~A" (make-string (1- p) :initial-element #\0))))
    (t
     (let* ((neg (minusp x))
            (ax  (abs (coerce x 'double-float)))
            (e   (floor (log ax 10d0))))
       ;; Correct e for floating-point log error so 10^e <= ax < 10^(e+1).
       (loop while (>= (/ ax (expt 10d0 e)) 10d0) do (incf e))
       (loop while (<  (/ ax (expt 10d0 e)) 1d0)  do (decf e))
       (let ((body
               (if (or (< e -6) (>= e p))
                   ;; exponential: 1 integer digit + (p-1) fraction digits, lowercase e
                   (format nil "~,v,,,,,'eE" (1- p) ax)
                   ;; fixed: (p-1-e) fraction digits, strip any trailing dot
                   (%js-strip-trailing-dot
                    (format nil "~,vF" (max 0 (- p 1 e)) ax)))))
         (if neg (concatenate 'string "-" body) body))))))

(defparameter *js-number-method-table*
  (list (cons "toFixed"
              (lambda (n digits)
                (let ((d (if (eq digits +js-undefined+) 0 (truncate (%js-to-number digits)))))
                  ;; CL ~,0F yields a trailing dot ("8."); JS toFixed(0) -> "8".
                  (%js-strip-trailing-dot (format nil "~,vF" d (%js-to-number n))))))
        (cons "toString"
              (lambda (n &optional (radix 10))
                (let ((r (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix))))
                      (ni (truncate (%js-to-number n))))
                  ;; JS uses lowercase digits for radices > 10 (ff, not FF).
                  (if (= r 10) (format nil "~A" (%js-to-number n))
                      (string-downcase (format nil "~vR" r ni))))))
        (cons "toPrecision"
              (lambda (n prec)
                (if (eq prec +js-undefined+)
                    (format nil "~A" (%js-to-number n))
                    (%js-number-to-precision (%js-to-number n)
                                             (truncate (%js-to-number prec))))))
        (cons "toExponential"
              (lambda (n &optional digits)
                ;; JS Number.prototype.toExponential([fractionDigits]) → "d.ddde±d"
                ;; ~E params: w,d,e,k,overflowchar,padchar,exptchar — set d and
                ;; force lowercase 'e' as the exponent marker.
                (let* ((v (%js-to-number n))
                       (d (if (or (null digits) (eq digits +js-undefined+))
                              6 (truncate (%js-to-number digits)))))
                  (format nil "~,v,,,,,'eE" d (coerce v 'double-float)))))
        (cons "valueOf" (lambda (n) (%js-to-number n)))
        (cons "toLocaleString" (lambda (n &rest _) (declare (ignore _)) (format nil "~A" (%js-to-number n)))))
  "Alist of Number.prototype method name -> (lambda (num args…)) helpers.")

(defparameter *js-symbol-method-table*
  (list (cons "toString"    #'%js-symbol-to-string)
        (cons "description" #'%js-symbol-description)
        (cons "valueOf"     (lambda (s) s)))
  "Alist of Symbol.prototype method name -> helpers.")

(defun %js-resolve-method (obj key)
  "Resolve OBJ.KEY to a bound method closure, or +js-undefined+.
Installed as *js-method-resolver* so %js-get-prop can offer prototype methods."
  (cond
    ;; Promise prototype methods (.then, .catch, .finally)
    ((js-promise-p obj)
     (cond ((string= key "then")
            (lambda (on-fulfilled &optional on-rejected) (%js-promise-then obj on-fulfilled on-rejected)))
           ((string= key "catch")
            (lambda (on-rejected) (%js-promise-then obj +js-undefined+ on-rejected)))
           ((string= key "finally")
            (lambda (fn)
              (%js-promise-then obj
               (lambda (v) (%js-funcall fn) (%js-promise-resolve v))
               (lambda (r) (%js-funcall fn) (%js-promise-reject r)))))
           (t +js-undefined+)))
    ;; Array prototype methods + length
    ((%js-vec-p obj)
     (cond ((string= key "length") (coerce (length obj) 'double-float))
           (t (%js-bound-method *js-array-method-table* obj key))))
    ;; String prototype methods + length
    ((stringp obj)
     (cond ((string= key "length") (coerce (length obj) 'double-float))
           (t (%js-bound-method *js-string-method-table* obj key))))
    ;; Map prototype methods + size
    ((js-map-p obj)
     (cond ((string= key "size") (coerce (%js-map-size obj) 'double-float))
           (t (%js-bound-method *js-map-method-table* obj key))))
    ;; Date prototype methods
    ((js-date-p obj)
     (%js-bound-method *js-date-method-table* obj key))
    ;; TypedArray prototype methods + length/byteLength/byteOffset
    ((js-typed-array-p obj)
     (cond ((string= key "length")     (coerce (js-ta-length obj) 'double-float))
           ((string= key "byteLength") (coerce (* (js-ta-length obj) (js-ta-element-size obj)) 'double-float))
           ((string= key "byteOffset") (coerce (js-ta-byte-offset obj) 'double-float))
           ((string= key "buffer")     (%js-make-object "byteLength" (* (js-ta-length obj) (js-ta-element-size obj))))
           (t (%js-bound-method *js-typed-array-method-table* obj key))))
    ;; RegExp prototype methods
    ((js-regexp-p obj)
     (cond ((string= key "source")    (js-regexp-source obj))
           ((string= key "flags")     (js-regexp-flags obj))
           ((string= key "global")    (js-regexp-global-p obj))
           ((string= key "ignoreCase") (js-regexp-ignore-case-p obj))
           ((string= key "multiline") (js-regexp-multiline-p obj))
           ((string= key "lastIndex") (coerce (js-regexp-last-index obj) 'double-float))
           ((string= key "test")      (let ((re obj)) (lambda (str) (%js-regex-test re str))))
           ((string= key "exec")      (let ((re obj)) (lambda (str) (%js-regex-exec re str 0))))
           (t +js-undefined+)))
    ;; Hash-table objects: both Sets and plain JS objects (distinguished by stored key presence)
    ((hash-table-p obj)
     ;; First check for stored real properties (plain JS object keys)
     (let ((stored (gethash key obj)))
       (if (not (eq stored nil))
           stored
           ;; Fall through: size property and Set prototype methods,
           ;; then Object.prototype methods available on all objects
           (cond
             ((string= key "size") (coerce (hash-table-count obj) 'double-float))
             ;; Set methods
             ((assoc key *js-set-method-table* :test #'string=)
              (%js-bound-method *js-set-method-table* obj key))
             ;; Object.prototype methods
             ((string= key "hasOwnProperty")
              (lambda (k) (nth-value 1 (gethash (%js-to-string k) obj))))
             ((string= key "toString")
              (lambda () "[object Object]"))
             ((string= key "valueOf")
              (lambda () obj))
             ((string= key "isPrototypeOf")
              (lambda (_proto) (declare (ignore _proto)) nil))
             ((string= key "propertyIsEnumerable")
              (lambda (k) (nth-value 1 (gethash (%js-to-string k) obj))))
             ((string= key "constructor")
              (lambda (&rest _) (declare (ignore _)) obj))
             (t +js-undefined+)))))
    ;; Number.prototype — numbers have methods too
    ((numberp obj)
     (let ((entry (assoc key *js-number-method-table* :test #'string=)))
       (if entry
           (let ((fn (cdr entry)))
             (lambda (&rest args) (apply fn obj args)))
           +js-undefined+)))
    ;; Symbol.prototype
    ((js-symbol-p obj)
     (let ((entry (assoc key *js-symbol-method-table* :test #'string=)))
       (if entry
           (let ((fn (cdr entry)))
             (lambda (&rest args) (apply fn obj args)))
           (cond ((string= key "description") (%js-symbol-description obj))
                 (t +js-undefined+)))))
    ;; WeakRef — deref method
    ((typep obj 'js-weak-ref)
     (cond ((string= key "deref") (lambda () (%js-weak-ref-deref obj)))
           (t +js-undefined+)))
    ;; Function.prototype — bind/call/apply/name/length on closures/functions
    ((functionp obj)
     (cond
       ((string= key "bind")
        (lambda (this-arg &rest partial-args)
          (lambda (&rest args)
            (apply #'%js-funcall obj (list* this-arg (append partial-args args))))))
       ((string= key "call")
        (lambda (this-arg &rest args)
          (apply #'%js-funcall obj (list* this-arg args))))
       ((string= key "apply")
        (lambda (this-arg args-array)
          (apply #'%js-funcall obj
                 (list* this-arg (if (%js-vec-p args-array) (coerce args-array 'list) nil)))))
       ((string= key "name")   "")
       ((string= key "length") 0.0d0)
       ((string= key "toString") (lambda () "function() { [native code] }"))
       ((string= key "call")   (lambda (this &rest args) (apply #'%js-funcall obj (list* this args))))
       (t +js-undefined+)))
    ;; BigInt.prototype — basic methods
    ((js-bigint-p obj)
     (cond ((string= key "toString")
            (lambda (&optional radix)
              (%js-bigint-to-string obj (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix))))))
           ((string= key "valueOf") (lambda () obj))
           ((string= key "toLocaleString") (lambda (&rest _) (declare (ignore _)) (%js-bigint-to-string obj)))
           (t +js-undefined+)))
    (t +js-undefined+)))

(setf *js-method-resolver* #'%js-resolve-method)
