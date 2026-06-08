;;;; packages/javascript/src/runtime-builtins.lisp — JS built-in dispatch table
;;;;
;;;; *js-builtin-map* maps built-in name strings to CL functions.
;;;; This file must be loaded LAST so all referenced functions are defined.

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
    ;; Number globals
    ("parseInt"                . ,(lambda (s &optional (radix 10))
                                    (handler-case
                                        (let* ((str (string-trim '(#\Space #\Tab #\Newline) (%js-to-string s)))
                                               (r (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix)))))
                                          (parse-integer str :radix r :junk-allowed t))
                                      (error () *js-nan-float*))))
    ("parseFloat"              . ,(lambda (s)
                                    (handler-case
                                        (let* ((str (string-trim '(#\Space #\Tab #\Newline) (%js-to-string s)))
                                               (val (read-from-string str nil *js-nan-float*)))
                                          (if (realp val) (coerce val 'double-float) *js-nan-float*))
                                      (error () *js-nan-float*))))
    ("isNaN"                   . ,(lambda (x) (%js-nan-p (%js-to-number x))))
    ("isFinite"                . ,(lambda (x)
                                    (let ((n (%js-to-number x)))
                                      (and (not (%js-float-nan-p n)) (not (%js-float-infinity-p n))))))
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
    ;; Number wrapper function / constructor
    ("Number"                  . ,#'%js-to-number)
    ;; String wrapper function
    ("String"                  . ,#'%js-to-string)
    ;; Boolean wrapper
    ("Boolean"                 . ,#'%js-truthy)
    ;; Error constructors (simplified)
    ("Error"                   . ,(lambda (&optional (msg "") &rest _)
                                    (declare (ignore _))
                                    (%js-make-object "message" msg "name" "Error")))
    ("TypeError"               . ,(lambda (&optional (msg "") &rest _)
                                    (declare (ignore _))
                                    (%js-make-object "message" msg "name" "TypeError")))
    ("RangeError"              . ,(lambda (&optional (msg "") &rest _)
                                    (declare (ignore _))
                                    (%js-make-object "message" msg "name" "RangeError")))
    ("ReferenceError"          . ,(lambda (&optional (msg "") &rest _)
                                    (declare (ignore _))
                                    (%js-make-object "message" msg "name" "ReferenceError")))
    ("SyntaxError"             . ,(lambda (&optional (msg "") &rest _)
                                    (declare (ignore _))
                                    (%js-make-object "message" msg "name" "SyntaxError"))))
  "Alist of (name . function) specs used to build *js-builtin-map*.")

(defun %build-js-builtin-map ()
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (spec *js-builtin-specs*)
      (setf (gethash (car spec) ht) (cdr spec)))
    ht))

(defvar *js-builtin-map* (%build-js-builtin-map)
  "Dispatch table from JS built-in name to CL function.")

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
        (cons "entries" #'%js-array-entries)    (cons "keys" #'%js-array-keys))
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
        (cons "substring" (lambda (s start &optional end)
                            (let* ((n (length s))
                                   (a (max 0 (min (truncate start) n)))
                                   (b (if (eq end +js-undefined+) n (max 0 (min (truncate end) n))))
                                   (lo (min a b)) (hi (max a b)))
                              (subseq s lo hi))))
        (cons "valueOf" (lambda (s) s))
        (cons "toString" (lambda (s) s)))
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

(defparameter *js-number-method-table*
  (list (cons "toFixed"
              (lambda (n digits)
                (let ((d (if (eq digits +js-undefined+) 0 (truncate (%js-to-number digits)))))
                  (format nil "~,vF" d (%js-to-number n)))))
        (cons "toString"
              (lambda (n &optional (radix 10))
                (let ((r (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix))))
                      (ni (truncate (%js-to-number n))))
                  (if (= r 10) (format nil "~A" (%js-to-number n))
                      (format nil "~vR" r ni)))))
        (cons "toPrecision"
              (lambda (n prec)
                (if (eq prec +js-undefined+)
                    (format nil "~A" (%js-to-number n))
                    (format nil "~,vG" (truncate (%js-to-number prec)) (%js-to-number n)))))
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
    ;; Set (hash-table) prototype methods + size
    ((hash-table-p obj)
     (cond ((string= key "size") (coerce (hash-table-count obj) 'double-float))
           (t (%js-bound-method *js-set-method-table* obj key))))
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
    (t +js-undefined+)))

(setf *js-method-resolver* #'%js-resolve-method)
