;;;; Central PHP builtin registry.

(in-package :cl-cc/php)

(defparameter *php-builtin-registry* (make-hash-table :test #'equal)
  "Map lowercase PHP builtin names to Common Lisp helper functions.")

(defparameter *php-builtin-symbol-registry* (make-hash-table :test #'equal)
  "Map lowercase PHP builtin names to helper symbols for parser lowering.")

(defparameter *php-builtin-registration-order* nil
  "Builtin names in registration order, used to rebuild `*php-builtin-map*`.")

(defun %php-normalize-builtin-name (name)
  "Normalize PHP builtin NAME for case-insensitive lookup."
  (string-downcase (string name)))

(defun %php-register-builtin (name function)
  "Register PHP builtin NAME to FUNCTION and return FUNCTION."
  ;; FUNCTION may be a function object or a function designator symbol. The
  ;; public registry stores the resolved helper function, while a side registry
  ;; keeps the symbol for parser integrations that lower calls to helper vars.
  ;; (%php-register-builtin "strlen" '%php-strlen) => #<FUNCTION %PHP-STRLEN>
  (let* ((key (%php-normalize-builtin-name name))
         (symbol (and (symbolp function) function))
         (fn (if symbol (symbol-function symbol) function)))
    (unless (gethash key *php-builtin-registry*)
      (setf *php-builtin-registration-order*
            (append *php-builtin-registration-order* (list key))))
    (setf (gethash key *php-builtin-registry*) fn)
    (when symbol
      (setf (gethash key *php-builtin-symbol-registry*) symbol))
    fn))

(defun %php-lookup-builtin (name)
  "Return the helper function registered for PHP builtin NAME, or NIL."
  ;; (%php-lookup-builtin "STRLEN") => #<FUNCTION %PHP-STRLEN>
  (gethash (%php-normalize-builtin-name name) *php-builtin-registry*))

(defun %php-lookup-builtin-symbol (name)
  "Return the helper symbol registered for PHP builtin NAME, or NIL."
  (gethash (%php-normalize-builtin-name name) *php-builtin-symbol-registry*))

(defun %php-sync-legacy-builtin-map ()
  "Synchronize legacy `*php-builtin-map*` with the central registry."
  (when (boundp '*php-builtin-map*)
    (setf *php-builtin-map*
          (loop for name in *php-builtin-registration-order*
                for fn = (%php-lookup-builtin name)
                when fn collect (cons name fn)))))

(defun %php-register-all-builtins ()
  "Register all supported PHP builtin helpers and return the registry."
  ;; The registry intentionally includes common aliases such as sizeof/count,
  ;; join/implode, and is_integer/is_int.
  ;; (%php-register-all-builtins) => *PHP-BUILTIN-REGISTRY*
  (clrhash *php-builtin-registry*)
  (clrhash *php-builtin-symbol-registry*)
  (setf *php-builtin-registration-order* nil)

  ;; Core.
  (%php-register-builtin "count" '%php-count)
  (%php-register-builtin "sizeof" '%php-count)
  (%php-register-builtin "isset" '%php-isset)
  (%php-register-builtin "empty" '%php-empty)
  (%php-register-builtin "is_null" '%php-is-null)
  (%php-register-builtin "unset" '%php-unset)
  (%php-register-builtin "print_r" '%php-print-r)
  (%php-register-builtin "var_dump" '%php-var-dump)
  (%php-register-builtin "gettype" '%php-gettype)
  (%php-register-builtin "array_key_exists" '%php-array-key-exists)

  ;; String.
  (%php-register-builtin "strlen" '%php-strlen)
  (%php-register-builtin "strtolower" '%php-strtolower)
  (%php-register-builtin "strtoupper" '%php-strtoupper)
  (%php-register-builtin "str_replace" '%php-str-replace)
  (%php-register-builtin "str_ireplace" '%php-str-ireplace)
  (%php-register-builtin "substr" '%php-substr)
  (%php-register-builtin "trim" '%php-trim)
  (%php-register-builtin "ltrim" '%php-ltrim)
  (%php-register-builtin "rtrim" '%php-rtrim)
  (%php-register-builtin "explode" '%php-explode)
  (%php-register-builtin "implode" '%php-implode)
  (%php-register-builtin "join" '%php-join)
  (%php-register-builtin "strpos" '%php-strpos)
  (%php-register-builtin "stripos" '%php-stripos)
  (%php-register-builtin "strrpos" '%php-strrpos)
  (%php-register-builtin "str_contains" '%php-str-contains)
  (%php-register-builtin "str_starts_with" '%php-str-starts-with)
  (%php-register-builtin "str_ends_with" '%php-str-ends-with)
  (%php-register-builtin "strrev" '%php-strrev)
  (%php-register-builtin "str_repeat" '%php-str-repeat)
  (%php-register-builtin "sprintf" '%php-sprintf)
  (%php-register-builtin "htmlspecialchars" '%php-htmlspecialchars)

  ;; Array.
  (%php-register-builtin "array_merge" '%php-array-merge)
  (%php-register-builtin "array_keys" '%php-array-keys)
  (%php-register-builtin "array_values" '%php-array-values)
  (%php-register-builtin "array_push" '%php-array-push)
  (%php-register-builtin "array_pop" '%php-array-pop)
  (%php-register-builtin "array_shift" '%php-array-shift)
  (%php-register-builtin "array_unshift" '%php-array-unshift)
  (%php-register-builtin "in_array" '%php-in-array)
  (%php-register-builtin "array_search" '%php-array-search)
  (%php-register-builtin "array_reverse" '%php-array-reverse)
  (%php-register-builtin "array_slice" '%php-array-slice)
  (%php-register-builtin "array_unique" '%php-array-unique)
  (%php-register-builtin "array_map" '%php-array-map)
  (%php-register-builtin "array_filter" '%php-array-filter)
  (%php-register-builtin "array_reduce" '%php-array-reduce)
  (%php-register-builtin "sort" '%php-sort)
  (%php-register-builtin "rsort" '%php-rsort)
  (%php-register-builtin "asort" '%php-asort)
  (%php-register-builtin "ksort" '%php-ksort)
  (%php-register-builtin "range" '%php-range)

  ;; Math.
  (%php-register-builtin "abs" '%php-abs)
  (%php-register-builtin "ceil" '%php-ceil)
  (%php-register-builtin "floor" '%php-floor)
  (%php-register-builtin "round" '%php-round)
  (%php-register-builtin "max" '%php-max)
  (%php-register-builtin "min" '%php-min)
  (%php-register-builtin "rand" '%php-rand)
  (%php-register-builtin "mt_rand" '%php-mt-rand)
  (%php-register-builtin "sqrt" '%php-sqrt)
  (%php-register-builtin "pow" '%php-pow)
  (%php-register-builtin "pi" '%php-pi)
  (%php-register-builtin "sin" (lambda (x) (sin (coerce x 'double-float))))
  (%php-register-builtin "cos" (lambda (x) (cos (coerce x 'double-float))))
  (%php-register-builtin "tan" (lambda (x) (tan (coerce x 'double-float))))
  (%php-register-builtin "log" (lambda (x &optional (base nil))
                                 (if base (/ (log (coerce x 'double-float)) (log (coerce base 'double-float)))
                                     (log (coerce x 'double-float)))))
  (%php-register-builtin "exp" (lambda (x) (exp (coerce x 'double-float))))
  (%php-register-builtin "fmod" (lambda (x y) (mod (coerce x 'double-float) (coerce y 'double-float))))
  (%php-register-builtin "intdiv" (lambda (x y) (truncate x y)))
  (%php-register-builtin "number_format" '%php-number-format)
  (%php-register-builtin "base_convert" (lambda (num from-base to-base)
                                          (format nil "~vR" to-base (parse-integer (%php-stringify num) :radix from-base :junk-allowed t))))

  ;; Regex
  (%php-register-builtin "preg_match" '%php-preg-match)
  (%php-register-builtin "preg_match_all" '%php-preg-match-all)
  (%php-register-builtin "preg_replace" '%php-preg-replace)
  (%php-register-builtin "preg_split" '%php-preg-split)
  (%php-register-builtin "preg_quote" '%php-preg-quote)

  ;; Date/Time
  (%php-register-builtin "time" '%php-time)
  (%php-register-builtin "microtime" '%php-microtime)
  (%php-register-builtin "mktime" '%php-mktime)
  (%php-register-builtin "strtotime" '%php-strtotime)
  (%php-register-builtin "date" '%php-date)
  (%php-register-builtin "checkdate" '%php-checkdate)

  ;; Types and conversions.
  (%php-register-builtin "is_int" '%php-is-int)
  (%php-register-builtin "is_integer" '%php-is-integer)
  (%php-register-builtin "is_long" '%php-is-long)
  (%php-register-builtin "is_float" '%php-is-float)
  (%php-register-builtin "is_double" '%php-is-double)
  (%php-register-builtin "is_real" '%php-is-real)
  (%php-register-builtin "is_string" '%php-is-string)
  (%php-register-builtin "is_bool" '%php-is-bool)
  (%php-register-builtin "is_array" '%php-is-array)
  (%php-register-builtin "is_object" '%php-is-object)
  (%php-register-builtin "is_callable" '%php-is-callable)
  (%php-register-builtin "is_numeric" '%php-is-numeric)
  (%php-register-builtin "is_scalar" '%php-is-scalar)
  (%php-register-builtin "is_iterable" '%php-is-iterable)
  (%php-register-builtin "intval" '%php-intval)
  (%php-register-builtin "floatval" '%php-floatval)
  (%php-register-builtin "strval" '%php-strval)
  (%php-register-builtin "boolval" '%php-boolval)

  (%php-sync-legacy-builtin-map)
  *php-builtin-registry*)

(eval-when (:load-toplevel :execute)
  (export '(*php-builtin-registry*
            %php-register-builtin
            %php-lookup-builtin
            %php-register-all-builtins
            %php-lookup-builtin-symbol
            %php-array-pairs)
          :cl-cc/php)
  (%php-register-all-builtins))
