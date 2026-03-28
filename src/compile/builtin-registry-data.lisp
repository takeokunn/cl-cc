;;;; compile/builtin-registry-data.lisp — Raw builtin entry alist tables
;;;
;;; Pure data: defparameter alists mapping CL function names to vm constructor
;;; symbols (and optional slot keyword lists).  No logic or structs here.
;;; Loaded by ASDF before builtin-registry.lisp which reads these tables.
;;;
;;; Conventions are grouped by calling convention; see builtin-registry.lisp
;;; for the full convention documentation.
(in-package :cl-cc)

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
    (fmakunbound      . make-vm-fmakunbound)
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
    (nconc            . make-vm-nconc)
    (nreconc          . make-vm-nreconc)
    ;; Equality (both map to vm-eq)
    (eq               . make-vm-eq)
    (eql              . make-vm-eq)
    ;; 2-arg atan → atan2 (only fires when args=2; 1-arg atan is unary above)
    (atan             . make-vm-atan2-inst))
  "Alist of (cl-symbol . vm-constructor) for binary builtins: (fn a b) → (:dst :lhs :rhs).
   Binary builtins require exactly 2 args (checked at dispatch time).")

(defparameter *builtin-string-cmp-entries*
  '((string=            . make-vm-string=)
    (string<            . make-vm-string<)
    (string>            . make-vm-string>)
    (string<=           . make-vm-string<=)
    (string>=           . make-vm-string>=)
    (string-equal       . make-vm-string-equal)
    (string-lessp       . make-vm-string-lessp)
    (string-greaterp    . make-vm-string-greaterp)
    (string/=           . make-vm-string-not-equal)
    (string-not-equal   . make-vm-string-not-equal)
    (string-not-greaterp . make-vm-string-not-greaterp)
    (string-not-lessp   . make-vm-string-not-lessp)
    ;; String concatenation shares string-cmp slot layout
    (string-concat      . make-vm-concatenate))
  "Alist of (cl-symbol . vm-constructor) for string comparison builtins: (fn s1 s2) → (:dst :str1 :str2).")

(defparameter *builtin-char-cmp-entries*
  '((char=             . make-vm-char=)
    (char<             . make-vm-char<)
    (char>             . make-vm-char>)
    (char<=            . make-vm-char<=)
    (char>=            . make-vm-char>=)
    (char/=            . make-vm-char/=)
    (char-equal        . make-vm-char-equal)
    (char-not-equal    . make-vm-char-not-equal)
    (char-lessp        . make-vm-char-lessp)
    (char-greaterp     . make-vm-char-greaterp)
    (char-not-greaterp . make-vm-char-not-greaterp)
    (char-not-lessp    . make-vm-char-not-lessp))
  "Alist of (cl-symbol . vm-constructor) for char comparison builtins: (fn c1 c2) → (:dst :char1 :char2).")

(defparameter *builtin-table-query-entries*
  '((hash-table-count              . make-vm-hash-table-count)
    (hash-table-keys               . make-vm-hash-table-keys)
    (hash-table-values             . make-vm-hash-table-values)
    (hash-table-test               . make-vm-hash-table-test)
    (hash-table-size               . make-vm-hash-table-size)
    (hash-table-rehash-size        . make-vm-hash-table-rehash-size)
    (hash-table-rehash-threshold   . make-vm-hash-table-rehash-threshold))
  "Alist of (cl-symbol . vm-constructor) for hash-table query builtins: (fn table) → (:dst :table).")

(defparameter *builtin-handle-input-entries*
  '((file-position     . make-vm-file-position)
    (file-length       . make-vm-file-length)
    (read-byte         . make-vm-read-byte)
    (listen            . make-vm-listen-inst))
  "Alist of (cl-symbol . vm-constructor) for handle-input builtins: (fn handle) → (:dst :handle).
read-char and read-line have optional stream args, so they use :stream-input-opt instead.")

(defparameter *builtin-side-effect-entries*
  '((princ             . make-vm-princ)
    (prin1             . make-vm-prin1)
    (print             . make-vm-print-inst))
  "Alist of (cl-symbol . vm-constructor) for side-effect builtins: (fn arg) → emit (:src), move dst←src.")

(defparameter *builtin-void-side-effect-entries*
  '((terpri            . make-vm-terpri-inst)
    (fresh-line        . make-vm-fresh-line-inst))
  "Alist of (cl-symbol . vm-constructor) for void side-effect builtins: (fn) → emit (), const dst←nil.")

(defparameter *builtin-nullary-entries*
  '((%values-to-list         . make-vm-values-to-list)
    (gensym                  . make-vm-gensym-inst)
    (get-universal-time      . make-vm-get-universal-time)
    (get-internal-real-time  . make-vm-get-internal-real-time)
    (get-internal-run-time   . make-vm-get-internal-run-time)
    (next-method-p           . make-vm-next-method-p)
    (make-string-output-stream . make-vm-make-string-output-stream-inst)
    ;; FR-507: Environment query functions
    (lisp-implementation-type    . make-vm-lisp-implementation-type)
    (lisp-implementation-version . make-vm-lisp-implementation-version)
    (machine-type                . make-vm-machine-type)
    (machine-version             . make-vm-machine-version)
    (machine-instance            . make-vm-machine-instance)
    (software-type               . make-vm-software-type)
    (software-version            . make-vm-software-version)
    (short-site-name             . make-vm-short-site-name)
    (long-site-name              . make-vm-long-site-name))
  "Alist of (cl-symbol . vm-constructor) for nullary builtins: (fn) → (:dst).")

(defparameter *builtin-string-trim-entries*
  '((string-trim       . make-vm-string-trim)
    (string-left-trim  . make-vm-string-left-trim)
    (string-right-trim . make-vm-string-right-trim))
  "Alist of (cl-symbol . vm-constructor) for string-trim builtins: (fn bag str) → (:dst :char-bag :string).")

(defparameter *builtin-handle-effect-entries*
  '((close             . make-vm-close-file))
  "Alist of (cl-symbol . vm-constructor) for handle-effect builtins: (fn handle) → emit (:handle), const dst←nil.")

(defparameter *builtin-binary-custom-entries*
  '((nth       make-vm-nth       :index     :list)
    (nthcdr    make-vm-nthcdr    :index     :list)
    (member    make-vm-member    :item      :list)
    (cons      make-vm-cons      :car-src   :cdr-src)
    (append    make-vm-append    :src1      :src2)
    (assoc     make-vm-assoc    :key       :alist)
    (char      make-vm-char     :string    :index)
    (bit       make-vm-bit-access :arr     :idx)
    (sbit      make-vm-sbit     :arr       :idx)
    (aref      make-vm-aref     :array-reg :index-reg)
    (remprop   make-vm-remprop  :sym       :indicator)
    (vector-push  make-vm-vector-push         :val-reg :array-reg)
    (vector-push-extend  make-vm-vector-push-extend  :val-reg :array-reg)
    ;; Phase 2 migrations
    (%progv-enter    make-vm-progv-enter      :syms      :vals)
    (adjust-array    make-vm-adjust-array     :arr       :dims)
    (%set-symbol-plist  make-vm-set-symbol-plist :sym    :plist-reg)
    (%set-fill-pointer  make-vm-set-fill-pointer :array-reg :val-reg))
  "List of (cl-symbol vm-constructor slot1 slot2) for binary builtins with
   non-standard slot names.  The parametric emitter passes :dst + two custom slots.")

(defparameter *builtin-binary-move-first-entries*
  '((rplaca  make-vm-rplaca  :cons  :val)
    (rplacd  make-vm-rplacd  :cons  :val))
  "Binary builtins that emit a void instruction then return arg1 via move.
   (cl-sym vm-ctor slot1 slot2) — no :dst, result←arg1.")

(defparameter *builtin-binary-void-entries*
  '((remhash     make-vm-remhash     :key    :table)
    (unread-char make-vm-unread-char :char   :handle)
    (cerror      make-vm-cerror     :continue-message :condition-reg))
  "Binary builtins that emit a void instruction (no :dst) and return nil.
   (cl-sym vm-ctor slot1 slot2).")

(defparameter *builtin-unary-custom-void-entries*
  '((%progv-exit make-vm-progv-exit    :saved)
    (error       make-vm-signal-error  :error-reg)
    (signal      make-vm-signal        :condition-reg)
    (warn        make-vm-warn          :condition-reg)
    (clrhash     make-vm-clrhash       :table))
  "Unary builtins with a custom slot name (no :dst), returning nil.
   (cl-sym vm-ctor slot).")

(defparameter *builtin-unary-opt-nil-entries*
  '((make-random-state . make-vm-make-random-state))
  "Unary builtins with optional arg defaulting to nil.
   Standard :dst :src slots.  (cl-sym . vm-ctor).")

(defparameter *builtin-binary-opt-one-entries*
  '((ffloor    . make-vm-ffloor)
    (fceiling  . make-vm-fceiling)
    (ftruncate . make-vm-ftruncate)
    (fround    . make-vm-fround))
  "Binary builtins with optional 2nd arg defaulting to 1.
   Standard :dst :lhs :rhs slots.  (cl-sym . vm-ctor).")

(defparameter *builtin-binary-opt-nil-slot-entries*
  '((intern make-vm-intern-symbol :src :pkg))
  "Binary builtins with :dst + 2 custom slots. 2nd arg optional, nil slot when absent.
   (cl-sym vm-ctor slot1 slot2).")

(defparameter *builtin-ternary-opt-nil-custom-entries*
  '((get    make-vm-symbol-get :sym    :indicator :default)
    (subseq make-vm-subseq     :string :start     :end))
  "Ternary builtins with :dst + 3 custom slots. 3rd arg optional, nil when absent.
   (cl-sym vm-ctor slot1 slot2 slot3).")

(defparameter *builtin-binary-synth-zero-entries*
  '((search make-vm-search-string :pattern :string :start))
  "Binary builtins that synthesize a zero-const 3rd slot.
   (cl-sym vm-ctor slot1 slot2 slot3-for-zero).")

(defparameter *builtin-unary-custom-entries*
  '((make-list make-vm-make-list :size))
  "Unary builtins with :dst and a custom slot name.
   (cl-sym vm-ctor slot).")

(defparameter *builtin-zero-compare-entries*
  '(;; Arithmetic predicates: synthesize comparison against zero
    (zerop  . make-vm-num-eq)
    (plusp  . make-vm-gt)
    (minusp . make-vm-lt))
  "Builtins that compare a single argument against zero.
   Convention: emit (vm-const zero 0), then (vm-cmp :dst :lhs arg :rhs zero).")

;;; Stream I/O conventions — optional stream argument with sensible defaults

(defparameter *builtin-stream-input-opt-entries*
  '((read-char  make-vm-read-char  0)
    (read-line  make-vm-read-line  0)
    (peek-char  make-vm-peek-char  0))
  "List of (cl-sym vm-ctor default-handle) for stream-input-opt:
   (fn &optional stream) → (:dst :handle), handle defaults to stdin(0).")

(defparameter *builtin-stream-void-opt-entries*
  '((force-output   make-vm-force-output   1)
    (finish-output  make-vm-finish-output  1)
    (clear-input    make-vm-clear-input    0)
    (clear-output   make-vm-clear-output   1))
  "List of (cl-sym vm-ctor default-handle) for stream-void-opt:
   (fn &optional stream) → (:handle), returns nil.  Default 1=stdout, 0=stdin.")

(defparameter *builtin-stream-write-val-entries*
  '((write-char  make-vm-write-char  :char      1)
    (write-byte  make-vm-write-byte  :byte-val  1)
    (write-line  make-vm-write-line  :str       1))
  "List of (cl-sym vm-ctor value-slot default-handle) for stream-write-val:
   (fn value &optional stream) → (:handle H :val-slot V), returns value.")

;;; Ternary conventions — 3-arg builtins with custom slot names
;;; Slots = (slot1 slot2 slot3 return-style)
;;; return-style: :dst = result from instruction's :dst slot
;;;               :move-third = emit void instruction, then move result←third-arg

(defparameter *builtin-ternary-custom-entries*
  '(;; has :dst — return via :dst
    (acons             make-vm-acons      :key       :value     :alist     :dst)
    (subst             make-vm-subst      :new-val   :old-val   :tree      :dst)
    (%set-symbol-prop  make-vm-symbol-set :sym       :indicator :value     :dst)
    (%svset            make-vm-svset      :array-reg :index-reg :val-reg   :dst)
    ;; no :dst — return third arg via move
    (aset              make-vm-aset       :array-reg :index-reg :val-reg   :move-third)
    ;; string/bit array mutation — return new value via :dst
    (rt-string-set     make-vm-string-set :str       :idx       :val-reg   :dst)
    (rt-bit-set        make-vm-bit-set    :arr       :idx       :val       :dst))
  "List of (cl-sym vm-ctor slot1 slot2 slot3 return-style) for ternary builtins.")
