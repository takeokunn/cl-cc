;;;; packages/javascript/src/runtime-builtins-prelude.lisp — JS program prelude builder
;;;;
;;;; *js-prelude-global-specs*: declarative table of globals injected before every
;;;; compiled JS program. %js-prelude-form emits one AST defvar per entry.
;;;; js-program-forms: main entry point — parse JS source + prepend prelude.
;;;;
;;;; Load order: after runtime-builtins-table.lisp (needs *js-builtin-specs*,
;;;; %js-builtin-ref, and all the %js-make-* / *js-* symbols).

(in-package :cl-cc/javascript)

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
    ;; Map/WeakMap/WeakSet/RegExp are defparameter vars, not defun symbols,
    ;; so (boundp sym) = t and seed-js-runtime-globals seeds them into the VM.
    (:var        "Map"                 *js-map-global*)
    (:var        "WeakMap"             *js-weak-map-global*)
    (:var        "WeakSet"             *js-weak-set-global*)
    (:var        "RegExp"              *js-regexp-global*)
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
