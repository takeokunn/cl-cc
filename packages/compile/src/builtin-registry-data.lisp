;;;; compile/builtin-registry-data.lisp — Raw builtin entry alist tables
;;;
;;; Pure data: defparameter alists mapping CL function names to vm constructor
;;; symbols (and optional slot keyword lists).  No logic or structs here.
;;; Loaded by ASDF before builtin-registry.lisp which reads these tables.
;;;
;;; Conventions are grouped by calling convention; see builtin-registry.lisp
;;; for the full convention documentation.
(in-package :cl-cc/compile)

;;; ─── Table Definitions ─────────────────────────────────────────────────────

(defparameter *builtin-unary-entries*
  '(;; String operations
    (string-length    . make-vm-string-length)
    (string-upcase    . make-vm-string-upcase)
    (string-downcase  . make-vm-string-downcase)
    (string-capitalize . make-vm-string-capitalize)
    (nstring-upcase   . make-vm-nstring-upcase)
    (nstring-downcase . make-vm-nstring-downcase)
    (nstring-capitalize . make-vm-nstring-capitalize)
    ;; Type predicates
    (symbolp          . make-vm-symbol-p)
    (numberp          . make-vm-number-p)
    (integerp         . make-vm-integer-p)
    (consp            . make-vm-cons-p)
    (null             . make-vm-null-p)
    (functionp        . make-vm-function-p)
    ;; List operations
    (car              . make-vm-car)
    (cdr              . make-vm-cdr)
    (first            . make-vm-first)
    (second           . make-vm-second)
    (third            . make-vm-third)
    (fourth           . make-vm-fourth)
    (fifth            . make-vm-fifth)
    (sixth            . make-vm-sixth)
    (seventh          . make-vm-seventh)
    (eighth           . make-vm-eighth)
    (ninth            . make-vm-ninth)
    (tenth            . make-vm-tenth)
    (rest             . make-vm-rest)
    (last             . make-vm-last)
    (length           . make-vm-length)
    (reverse          . make-vm-reverse)
    (not              . make-vm-not)
    (nreverse         . make-vm-nreverse)
    (butlast          . make-vm-butlast)
    (nbutlast         . make-vm-nbutlast)
    (endp             . make-vm-endp)
    ;; FR-597: higher-order function combinators
    (identity         . make-vm-identity)
    (constantly       . make-vm-constantly)
    (complement       . make-vm-complement)
    ;; Arithmetic
    (abs              . make-vm-abs)
    (evenp            . make-vm-evenp)
    (oddp             . make-vm-oddp)
    ;; Bitwise
    (bswap            . make-vm-bswap)
    (lognot           . make-vm-lognot)
    (logcount         . make-vm-logcount)
    (integer-length   . make-vm-integer-length)
    ;; Transcendentals
    (sqrt             . make-vm-sqrt)
    (exp              . make-vm-exp-inst)
    (log              . make-vm-log-inst)
    (sin              . make-vm-sin-inst)
    (cos              . make-vm-cos-inst)
    (tan              . make-vm-tan-inst)
    (asin             . make-vm-asin-inst)
    (acos             . make-vm-acos-inst)
    (atan             . make-vm-atan-inst)
    (sinh             . make-vm-sinh-inst)
    (cosh             . make-vm-cosh-inst)
    (tanh             . make-vm-tanh-inst)
    (asinh            . make-vm-asinh-inst)
    (acosh            . make-vm-acosh-inst)
    (atanh            . make-vm-atanh-inst)
    ;; Float operations
    (float            . make-vm-float-inst)
    (float-precision  . make-vm-float-precision)
    (float-radix      . make-vm-float-radix)
    (float-sign       . make-vm-float-sign)
    (float-digits     . make-vm-float-digits)
    (decode-float     . make-vm-decode-float)
    (integer-decode-float . make-vm-integer-decode-float)
    ;; Binding predicates
    (boundp           . make-vm-boundp)
    (fboundp          . make-vm-fboundp)
    (makunbound       . make-vm-makunbound)
    (fdefinition      . make-vm-fdefinition)
    (symbol-function  . make-vm-fdefinition)
    ;; Random
    (random           . make-vm-random)
    ;; Rational
    (rational         . make-vm-rational)
    (rationalize      . make-vm-rationalize)
    (numerator        . make-vm-numerator)
    (denominator      . make-vm-denominator)
    ;; Complex
    (realpart         . make-vm-realpart)
    (imagpart         . make-vm-imagpart)
    (conjugate        . make-vm-conjugate)
    (phase            . make-vm-phase)
    ;; Hash table
    (hash-table-p     . make-vm-hash-table-p)
    ;; Stream predicates
    (streamp           . make-vm-streamp)
    (input-stream-p    . make-vm-input-stream-p)
    (output-stream-p   . make-vm-output-stream-p)
    (open-stream-p     . make-vm-open-stream-p)
    (interactive-stream-p . make-vm-interactive-stream-p)
    (stream-element-type . make-vm-stream-element-type-inst)
    ;; Array/vector
    (vectorp          . make-vm-vectorp)
    (simple-vector-p  . make-vm-simple-vector-p)
    (array-length     . make-vm-array-length)
    (array-rank       . make-vm-array-rank)
    (array-total-size . make-vm-array-total-size)
    (array-dimensions . make-vm-array-dimensions)
    (fill-pointer     . make-vm-fill-pointer-inst)
    (array-has-fill-pointer-p . make-vm-array-has-fill-pointer-p)
    (array-adjustable-p . make-vm-array-adjustable-p)
    (vector-pop       . make-vm-vector-pop)
    (array-displacement . make-vm-array-displacement)
    ;; Bit array
    (bit-not          . make-vm-bit-not)
    ;; Symbol
    (symbol-name      . make-vm-symbol-name)
    (make-symbol      . make-vm-make-symbol)
    (find-package     . make-vm-find-package)
    (keywordp         . make-vm-keywordp)
    (symbol-plist     . make-vm-symbol-plist)
    ;; Time / system
    (sleep               . make-vm-sleep-inst)
    (decode-universal-time . make-vm-decode-universal-time)
    ;; Character predicates and operations
    (both-case-p      . make-vm-both-case-p)
    (graphic-char-p   . make-vm-graphic-char-p)
    (standard-char-p  . make-vm-standard-char-p)
    (digit-char       . make-vm-digit-char)
    (char-name        . make-vm-char-name)
    (name-char        . make-vm-name-char)
    (char-int         . make-vm-char-code)
    (digit-char-p     . make-vm-digit-char-p)
    (alpha-char-p     . make-vm-alpha-char-p)
    (alphanumericp    . make-vm-alphanumericp)
    (upper-case-p     . make-vm-upper-case-p)
    (lower-case-p     . make-vm-lower-case-p)
    (char-upcase      . make-vm-char-upcase)
    (char-downcase    . make-vm-char-downcase)
    (char-code        . make-vm-char-code)
    (code-char        . make-vm-code-char)
    (stringp          . make-vm-stringp)
    (characterp       . make-vm-characterp)
    (parse-integer    . make-vm-parse-integer)
    ;; Coercion
    (coerce-to-string . make-vm-coerce-to-string)
    (coerce-to-list   . make-vm-coerce-to-list)
    (coerce-to-vector . make-vm-coerce-to-vector)
    (string           . make-vm-string-coerce)
    ;; List utilities
    (list-length      . make-vm-list-length)
    (listp            . make-vm-listp)
    (atom             . make-vm-atom)
    (copy-list        . make-vm-copy-list)
    (copy-tree        . make-vm-copy-tree)
    ;; Type
    (type-of          . make-vm-type-of)
    ;; Eval
    (eval             . make-vm-eval)
    ;; FR-631: Macro expansion
    (macroexpand-1    . make-vm-macroexpand-1-inst)
    (macroexpand      . make-vm-macroexpand-inst)
    ;; FR-498: Hash code
    (sxhash           . make-vm-sxhash)
    ;; FR-677/FR-552: CLOS introspection
    (class-name       . make-vm-class-name-fn)
    (class-of         . make-vm-class-of-fn)
    (find-class       . make-vm-find-class)
    ;; Values
    (values-list      . make-vm-spread-values)
    ;; Write-to-string (three CL names → one instruction)
    (write-to-string  . make-vm-write-to-string-inst)
    (prin1-to-string  . make-vm-write-to-string-inst)
    (princ-to-string  . make-vm-princ-to-string-inst)
    ;; String stream
    (get-output-stream-string . make-vm-get-output-stream-string-inst)
    ;; Reader
    (read-from-string . make-vm-read-from-string-inst)
    (read             . make-vm-read-sexp-inst)
    ;; Load
    (load             . make-vm-load-file))
  "Alist of (cl-symbol . vm-constructor) for unary builtins: (fn arg) → (:dst :src).")

(defparameter *builtin-binary-entries*
  '(;; Arithmetic
    (mod              . make-vm-mod)
    (rem              . make-vm-rem)
    (truncate         . make-vm-truncate)
    (floor            . make-vm-floor-inst)
    (ceiling          . make-vm-ceiling-inst)
    (min              . make-vm-min)
    (max              . make-vm-max)
    (round            . make-vm-round-inst)
    ;; Bitwise
    (ash              . make-vm-ash)
    (logand           . make-vm-logand)
    (logior           . make-vm-logior)
    (logxor           . make-vm-logxor)
    (logeqv           . make-vm-logeqv)
    (logtest          . make-vm-logtest)
    (logbitp          . make-vm-logbitp)
    ;; Transcendental (2-arg)
    (expt             . make-vm-expt)
    ;; Float (2-arg)
    (scale-float      . make-vm-scale-float)
    ;; Rational (2-arg)
    (gcd              . make-vm-gcd)
    (lcm              . make-vm-lcm)
    ;; Complex (2-arg)
    (complex          . make-vm-complex)
    ;; Array
    (array-dimension  . make-vm-array-dimension)
    (row-major-aref   . make-vm-row-major-aref)
    (svref            . make-vm-svref)
    ;; Bit array
    (bit-and          . make-vm-bit-and)
    (bit-ior          . make-vm-bit-or)  ; ANSI CL name (inclusive or)
    (bit-or           . make-vm-bit-or)  ; cl-cc alias
    (bit-xor          . make-vm-bit-xor)
    ;; List
    (equal            . make-vm-equal)
    (set              . make-vm-set-symbol-value)
    (nconc            . make-vm-nconc)
    (nreconc          . make-vm-nreconc)
    ;; Equality (both map to vm-eq)
    (eq               . make-vm-eq)
    (eql              . make-vm-eq)
    ;; 2-arg atan → atan2 (only fires when args=2; 1-arg atan is unary above)
    (atan             . make-vm-atan2-inst))
  "Alist of (cl-symbol . vm-constructor) for binary builtins: (fn a b) → (:dst :lhs :rhs).
   Binary builtins require exactly 2 args (checked at dispatch time).")

;;; (Comparison tables: string-cmp, char-cmp, and all specialized conventions
;;;  are in builtin-registry-data-ext.lisp which loads after this file.)
