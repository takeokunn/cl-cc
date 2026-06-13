;;;; packages/javascript/src/runtime-builtins-table.lisp — JS built-in dispatch data
;;;;
;;;; *js-builtin-specs*: alist of (js-name . cl-fn) for all built-in globals.
;;;; *js-prelude-global-specs*: declarative prelude table for js-program-forms.
;;;;
;;;; All referenced helper functions are defined in runtime-builtins.lisp which
;;;; loads before this file. The separation allows scanning the full built-in
;;;; roster at a glance without wading through implementation code.

(in-package :cl-cc/javascript)

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
    ("Set"                     . ,#'%js-make-set-from-iterable)
    ;; Map constructor
    ("Map"                     . ,#'%js-make-map)
    ;; WeakMap/WeakSet constructors
    ("WeakMap"                 . ,(lambda (&rest _) (declare (ignore _)) (%js-make-weak-map)))
    ("WeakSet"                 . ,(lambda (&rest _) (declare (ignore _)) (%js-make-weak-set)))
    ;; WeakRef constructor
    ("WeakRef"                 . ,(lambda (target) (%js-make-weak-ref target)))
    ;; Intl API stubs — named constructors defined in runtime-builtins.lisp
    ("Intl.NumberFormat"   . ,#'%js-make-intl-number-format)
    ("Intl.DateTimeFormat" . ,#'%js-make-intl-date-time-format)
    ("Intl.Collator"       . ,#'%js-make-intl-collator)
    ("Intl.ListFormat"     . ,#'%js-make-intl-list-format)
    ("Intl.PluralRules"    . ,#'%js-make-intl-plural-rules)

    ;; TypedArray constructors — %js-make-typed-array-ctor returns (lambda (&optional arg) ...)
    ("Int8Array"           . ,(%js-make-typed-array-ctor "Int8Array"))
    ("Uint8Array"          . ,(%js-make-typed-array-ctor "Uint8Array"))
    ("Uint8ClampedArray"   . ,(%js-make-typed-array-ctor "Uint8ClampedArray"))
    ("Int16Array"          . ,(%js-make-typed-array-ctor "Int16Array"))
    ("Uint16Array"         . ,(%js-make-typed-array-ctor "Uint16Array"))
    ("Int32Array"          . ,(%js-make-typed-array-ctor "Int32Array"))
    ("Uint32Array"         . ,(%js-make-typed-array-ctor "Uint32Array"))
    ("Float32Array"        . ,(%js-make-typed-array-ctor "Float32Array"))
    ("Float64Array"        . ,(%js-make-typed-array-ctor "Float64Array"))
    ;; ES2020 BigInt typed arrays
    ("BigInt64Array"       . ,(%js-make-typed-array-ctor "BigInt64Array"))
    ("BigUint64Array"      . ,(%js-make-typed-array-ctor "BigUint64Array"))
    ;; Proxy constructor — simplified: returns a wrapped object
    ("Proxy"                   . ,#'%js-make-proxy-object)
    ;; Reflect — static methods mirroring Object/Function operations
    ("Reflect.get"                      . ,#'%js-reflect-get)
    ("Reflect.set"                      . ,#'%js-reflect-set)
    ("Reflect.has"                      . ,#'%js-reflect-has)
    ("Reflect.deleteProperty"           . ,#'%js-reflect-delete-property)
    ("Reflect.ownKeys"                  . ,#'%js-object-keys)
    ("Reflect.apply"                    . ,#'%js-reflect-apply)
    ("Reflect.construct"                . ,#'%js-reflect-construct)
    ("Reflect.defineProperty"           . ,#'%js-reflect-define-property)
    ("Reflect.getOwnPropertyDescriptor" . ,#'%js-reflect-get-own-property-descriptor)
    ("Reflect.getPrototypeOf"           . ,(lambda (_target) (declare (ignore _target)) +js-null+))
    ("Reflect.setPrototypeOf"           . ,(lambda (target _proto) (declare (ignore target _proto)) t))
    ("Reflect.isExtensible"             . ,(lambda (_target) (declare (ignore _target)) t))
    ("Reflect.preventExtensions"        . ,(lambda (_target) (declare (ignore _target)) t))
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
    ;; Object property descriptor ops
    ("Object.defineProperty"            . ,#'%js-object-define-property)
    ("Object.defineProperties"          . ,#'%js-object-define-properties)
    ("Object.getOwnPropertyDescriptor"  . ,#'%js-object-get-own-property-descriptor)
    ("Object.getOwnPropertyNames"      . ,#'%js-object-keys)
    ("Object.getOwnPropertySymbols"    . ,(lambda (_obj) (declare (ignore _obj)) (%js-make-array)))
    ;; ES2017: Object.getOwnPropertyDescriptors (plural)
    ("Object.getOwnPropertyDescriptors" . ,#'%js-object-get-own-property-descriptors)
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
    ("Promise.try"          . ,#'%js-promise-try)
    ;; ES2026: Math.sumPrecise
    ("Math.sumPrecise"      . ,#'%js-math-sum-precise)
    ;; ES2026: Error.isError
    ("Error.isError"        . ,#'%js-error-is-error)
    ;; ES2021: FinalizationRegistry
    ("FinalizationRegistry" . ,#'%js-make-finalization-registry)
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
    ("RegExp.escape"    . ,#'%js-regexp-escape)
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
    ;; ES2022: AggregateError
    ("AggregateError"          . ,#'%js-make-aggregate-error)
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
    ("URL"               . ,#'%js-make-url)
    ("URLSearchParams"   . ,#'%js-make-url-search-params)
    ;; AbortController / AbortSignal
    ("AbortController"   . ,#'%js-make-abort-controller)
    ("AbortSignal"       . ,(lambda () (%js-make-object "aborted" nil "reason" +js-undefined+)))
    ;; Performance API extras
    ("performance"       . ,(%js-make-object "now" (lambda ()
                                                      (coerce (- (get-internal-real-time) 0) 'double-float))
                                             "mark" (lambda (&rest _) (declare (ignore _)) +js-undefined+)
                                             "measure" (lambda (&rest _) (declare (ignore _)) +js-undefined+)
                                             "getEntries" (lambda () (%js-make-array))
                                             "clearMarks" (lambda (&rest _) (declare (ignore _)) +js-undefined+)))
    ;; Crypto (stub)
    ("crypto"            . ,(%js-make-crypto))
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

