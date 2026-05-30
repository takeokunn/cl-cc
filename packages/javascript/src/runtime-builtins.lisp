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
