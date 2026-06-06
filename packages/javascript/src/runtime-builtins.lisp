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
    ("console.warn"            . ,#'%js-console-warn))
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
  (cons (make-ast-defvar
         :name (js-ident-sym "console")
         :value (make-ast-call :func (make-ast-var :name '%js-make-console) :args nil)
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
        (cons "trimEnd" #'%js-string-trim-end))
  "Alist of JS String.prototype method name -> host helper (receiver is S, first arg).")

(defun %js-bound-method (table receiver name)
  "Look NAME up in TABLE; return a closure that calls the helper with RECEIVER
prepended (so `receiver.name(a,b)' becomes (helper receiver a b)), else undefined."
  (let ((entry (assoc name table :test #'string=)))
    (if entry
        (let ((fn (cdr entry)))
          (lambda (&rest args) (apply fn receiver args)))
        +js-undefined+)))

(defun %js-resolve-method (obj key)
  "Resolve OBJ.KEY to a bound Array/String method closure, or +js-undefined+.
Installed as *js-method-resolver* so %js-get-prop can offer prototype methods."
  (cond
    ((%js-vec-p obj) (%js-bound-method *js-array-method-table* obj key))
    ((stringp obj)   (%js-bound-method *js-string-method-table* obj key))
    (t +js-undefined+)))

(setf *js-method-resolver* #'%js-resolve-method)
