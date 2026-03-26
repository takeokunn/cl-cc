;;;; compile/builtin-registry.lisp — Declarative builtin dispatch tables
;;;
;;; Maps CL function names to their VM instruction constructors and calling
;;; conventions.  Used by compile-ast (ast-call) to replace imperative
;;; dispatch with a single table lookup + generic emitter (~189 entries).
;;;
;;; 26 calling conventions (keys in *builtin-emitter-table*):
;;;
;;; ── Standard (original 17) ──────────────────────────────────────────
;;;   :unary              (fn arg)              → (:dst :src)
;;;   :binary             (fn a b)              → (:dst :lhs :rhs)
;;;   :binary-custom      (fn a b)              → (:dst slot1 slot2) parametric
;;;   :string-cmp         (fn s1 s2)            → (:dst :str1 :str2)
;;;   :char-cmp           (fn c1 c2)            → (:dst :char1 :char2)
;;;   :table-query        (fn table)            → (:dst :table)
;;;   :handle-input       (fn handle)           → (:dst :handle)
;;;   :side-effect        (fn arg)              → (:src) + move dst←src
;;;   :void-side-eff      (fn)                  → () + const dst←nil
;;;   :nullary            (fn)                  → (:dst)
;;;   :string-trim        (fn bag str)          → (:dst :char-bag :string)
;;;   :handle-effect      (fn handle)           → (:handle) + const dst←nil
;;;   :zero-compare       (fn arg)              → const zero←0, (:dst :lhs arg :rhs zero)
;;;   :stream-input-opt   (fn &opt stream)      → (:dst :handle), default from slots
;;;   :stream-void-opt    (fn &opt stream)      → (:handle), const dst←nil, default from slots
;;;   :stream-write-val   (fn val &opt stream)  → (:handle :val-slot), move dst←val
;;;   :ternary-custom     (fn a b c)            → 3 custom slots + :dst
;;;
;;; ── Phase 2 additions (9 new) ───────────────────────────────────────
;;;   :binary-move-first  (fn a b)              → emit both, move dst←first
;;;   :binary-void        (fn a b)              → 2 custom slots, const dst←nil
;;;   :unary-custom-void  (fn arg)              → 1 custom slot, no :dst, const dst←nil
;;;   :unary-custom       (fn arg)              → :dst + 1 custom slot
;;;   :unary-opt-nil      (fn &opt arg)         → (:dst :src), nil default
;;;   :binary-opt-one     (fn a &opt b)         → (:dst :lhs :rhs), 1 default
;;;   :ternary-opt-nil-custom (fn a b &opt c)   → :dst + 3 custom slots, nil default
;;;   :binary-opt-nil-slot (fn a &opt b)        → :dst + 2 custom slots, nil default
;;;   :binary-synth-zero  (fn a b)              → :dst + 3 custom slots, zero synth
(in-package :cl-cc)

;;; ─── Registry Entry ────────────────────────────────────────────────────────

(defstruct (builtin-entry (:conc-name be-))
  "A single builtin dispatch entry."
  (name-str  ""     :type string)       ; uppercase CL name string
  (convention :unary :type keyword)     ; calling convention tag
  (ctor       nil    :type symbol)      ; make-vm-* constructor symbol
  (slots      nil    :type list))       ; optional: slot keywords for parametric conventions

;;; ─── Table Definitions ─────────────────────────────────────────────────────

(defparameter *builtin-unary-entries*
  '(;; String operations
    (string-length    . make-vm-string-length)
    (string-upcase    . make-vm-string-upcase)
    (string-downcase  . make-vm-string-downcase)
    (string-capitalize . make-vm-string-capitalize)
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
    (rest             . make-vm-rest)
    (last             . make-vm-last)
    (length           . make-vm-length)
    (reverse          . make-vm-reverse)
    (not              . make-vm-not)
    (nreverse         . make-vm-nreverse)
    (butlast          . make-vm-butlast)
    (endp             . make-vm-endp)
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
    ;; Time
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
    (listp            . make-vm-listp)
    (atom             . make-vm-atom)
    (copy-list        . make-vm-copy-list)
    (copy-tree        . make-vm-copy-tree)
    ;; Type
    (type-of          . make-vm-type-of)
    ;; Eval
    (eval             . make-vm-eval)
    ;; Values
    (values-list      . make-vm-spread-values)
    ;; Write-to-string (three CL names → one instruction)
    (write-to-string  . make-vm-write-to-string-inst)
    (prin1-to-string  . make-vm-write-to-string-inst)
    (princ-to-string  . make-vm-write-to-string-inst)
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
    (bit-or           . make-vm-bit-or)
    (bit-xor          . make-vm-bit-xor)
    ;; List
    (equal            . make-vm-equal)
    (nconc            . make-vm-nconc)
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
  '((hash-table-count  . make-vm-hash-table-count)
    (hash-table-keys   . make-vm-hash-table-keys)
    (hash-table-values . make-vm-hash-table-values)
    (hash-table-test   . make-vm-hash-table-test))
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
    (make-string-output-stream . make-vm-make-string-output-stream-inst))
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
    (unread-char make-vm-unread-char :char   :handle))
  "Binary builtins that emit a void instruction (no :dst) and return nil.
   (cl-sym vm-ctor slot1 slot2).")

(defparameter *builtin-unary-custom-void-entries*
  '((%progv-exit make-vm-progv-exit    :saved)
    (error       make-vm-signal-error  :error-reg)
    (warn        make-vm-warn          :condition-reg))
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
    ;; Each maps to (cl-name . vm-comparator-constructor)
    (zerop  . make-vm-num-eq)
    (plusp  . make-vm-gt)
    (minusp . make-vm-lt))
  "Builtins that compare a single argument against zero.
   Convention: emit (vm-const zero 0), then (vm-cmp :dst :lhs arg :rhs zero).")

;;; Stream I/O conventions — optional stream argument with sensible defaults
;;; Slots list encodes per-entry metadata: (default-handle-value)

(defparameter *builtin-stream-input-opt-entries*
  '((read-char  make-vm-read-char  0)
    (read-line  make-vm-read-line  0))
  "List of (cl-sym vm-ctor default-handle) for stream-input-opt:
   (fn &optional stream) → (:dst :handle), handle defaults to stdin(0).")

(defparameter *builtin-stream-void-opt-entries*
  '((force-output   make-vm-force-output   1)
    (finish-output  make-vm-finish-output  1)
    (clear-input    make-vm-clear-input    0))
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
    (aset              make-vm-aset       :array-reg :index-reg :val-reg   :move-third))
  "List of (cl-sym vm-ctor slot1 slot2 slot3 return-style) for ternary builtins.")

;;; ─── Unified Registry Hash Table ───────────────────────────────────────────

(defparameter *builtin-registry* (make-hash-table :test #'equal)
  "Maps uppercase CL function name strings to builtin-entry structs.
   Populated at load time from the category alists above.")

(defun %register-builtins (alist convention)
  "Register all entries from ALIST under CONVENTION in *builtin-registry*.
   Also emits a Prolog fact (builtin-convention cl-sym vm-ctor) for each entry,
   making the builtin classification queryable by the Prolog engine."
  (dolist (pair alist)
    (let ((name-str (symbol-name (car pair)))
          (cl-sym   (car pair))
          (ctor     (cdr pair)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention convention
                                :ctor ctor))
      ;; Emit Prolog fact: (builtin-<convention> <cl-sym> <vm-ctor>)
      (let ((pred (intern (format nil "BUILTIN-~A" (symbol-name convention)) :cl-cc)))
        (add-rule pred (make-prolog-rule :head (list pred cl-sym ctor)))))))

(defun %register-stream-builtins (entries convention)
  "Register parametric stream I/O entries in *builtin-registry*.
   Each entry is (cl-symbol vm-ctor ...metadata).  The tail is stored in be-slots."
  (dolist (e entries)
    (let* ((cl-sym  (first e))
           (ctor    (second e))
           (meta    (cddr e))
           (name-str (symbol-name cl-sym)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention convention
                                :ctor ctor
                                :slots meta))
      (let ((pred (intern (format nil "BUILTIN-~A" (symbol-name convention)) :cl-cc)))
        (add-rule pred (make-prolog-rule :head (list pred cl-sym ctor)))))))

(defun %register-binary-custom-builtins (entries &optional (convention :binary-custom))
  "Register parametric binary-slot entries in *builtin-registry*.
   Each entry is (cl-symbol vm-ctor slot1 slot2).
   CONVENTION defaults to :binary-custom but can be overridden."
  (dolist (e entries)
    (destructuring-bind (cl-sym ctor slot1 slot2) e
      (let ((name-str (symbol-name cl-sym)))
        (setf (gethash name-str *builtin-registry*)
              (make-builtin-entry :name-str name-str
                                  :convention convention
                                  :ctor ctor
                                  :slots (list slot1 slot2)))
        (let ((pred (intern (format nil "BUILTIN-~A" (symbol-name convention)) :cl-cc)))
          (add-rule pred (make-prolog-rule :head (list pred cl-sym ctor slot1 slot2))))))))

;; Populate at load time
(%register-builtins *builtin-unary-entries*            :unary)
(%register-builtins *builtin-binary-entries*           :binary)
(%register-builtins *builtin-string-cmp-entries*       :string-cmp)
(%register-builtins *builtin-char-cmp-entries*         :char-cmp)
(%register-builtins *builtin-table-query-entries*      :table-query)
(%register-builtins *builtin-handle-input-entries*     :handle-input)
(%register-builtins *builtin-side-effect-entries*      :side-effect)
(%register-builtins *builtin-void-side-effect-entries* :void-side-eff)
(%register-builtins *builtin-nullary-entries*          :nullary)
(%register-builtins *builtin-string-trim-entries*      :string-trim)
(%register-builtins *builtin-handle-effect-entries*    :handle-effect)
(%register-binary-custom-builtins *builtin-binary-custom-entries*)
(%register-binary-custom-builtins *builtin-binary-move-first-entries* :binary-move-first)
(%register-binary-custom-builtins *builtin-binary-void-entries* :binary-void)
(dolist (e *builtin-unary-custom-void-entries*)
  (destructuring-bind (cl-sym ctor slot) e
    (let ((name-str (symbol-name cl-sym)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention :unary-custom-void
                                :ctor ctor
                                :slots (list slot))))))

(%register-builtins *builtin-unary-opt-nil-entries*    :unary-opt-nil)
(%register-builtins *builtin-binary-opt-one-entries*   :binary-opt-one)

(%register-binary-custom-builtins *builtin-binary-opt-nil-slot-entries* :binary-opt-nil-slot)

(dolist (e *builtin-binary-synth-zero-entries*)
  (destructuring-bind (cl-sym ctor slot1 slot2 slot3) e
    (let ((name-str (symbol-name cl-sym)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention :binary-synth-zero
                                :ctor ctor
                                :slots (list slot1 slot2 slot3))))))

(dolist (e *builtin-ternary-opt-nil-custom-entries*)
  (destructuring-bind (cl-sym ctor slot1 slot2 slot3) e
    (let ((name-str (symbol-name cl-sym)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention :ternary-opt-nil-custom
                                :ctor ctor
                                :slots (list slot1 slot2 slot3))))))

(dolist (e *builtin-unary-custom-entries*)
  (destructuring-bind (cl-sym ctor slot) e
    (let ((name-str (symbol-name cl-sym)))
      (setf (gethash name-str *builtin-registry*)
            (make-builtin-entry :name-str name-str
                                :convention :unary-custom
                                :ctor ctor
                                :slots (list slot))))))

(%register-builtins *builtin-zero-compare-entries* :zero-compare)
(%register-stream-builtins *builtin-stream-input-opt-entries*     :stream-input-opt)
(%register-stream-builtins *builtin-stream-void-opt-entries*      :stream-void-opt)
(%register-stream-builtins *builtin-stream-write-val-entries*     :stream-write-val)
(%register-stream-builtins *builtin-ternary-custom-entries*       :ternary-custom)

;;; ─── Generic Emitters ──────────────────────────────────────────────────────
;;;
;;; Each emitter takes (entry args result-reg ctx) and emits VM instructions
;;; for the corresponding calling convention.  Returns result-reg.

(defun emit-builtin-unary (entry args result-reg ctx)
  (let ((arg-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :src arg-reg))
    result-reg))

(defun emit-builtin-binary (entry args result-reg ctx)
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :lhs lhs-reg :rhs rhs-reg))
    result-reg))

(defun emit-builtin-string-cmp (entry args result-reg ctx)
  (let ((arg1-reg (compile-ast (first args) ctx))
        (arg2-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :str1 arg1-reg :str2 arg2-reg))
    result-reg))

(defun emit-builtin-char-cmp (entry args result-reg ctx)
  (let ((c1-reg (compile-ast (first args) ctx))
        (c2-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :char1 c1-reg :char2 c2-reg))
    result-reg))

(defun emit-builtin-table-query (entry args result-reg ctx)
  (let ((table-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :table table-reg))
    result-reg))

(defun emit-builtin-handle-input (entry args result-reg ctx)
  (let ((handle-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :handle handle-reg))
    result-reg))

(defun emit-builtin-side-effect (entry args result-reg ctx)
  (let ((arg-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :src arg-reg))
    (emit ctx (make-vm-move :dst result-reg :src arg-reg))
    result-reg))

(defun emit-builtin-void-side-eff (entry _args result-reg ctx)
  (declare (ignore _args))
  (emit ctx (funcall (be-ctor entry)))
  (emit ctx (make-vm-const :dst result-reg :value nil))
  result-reg)

(defun emit-builtin-nullary (entry _args result-reg ctx)
  (declare (ignore _args))
  (emit ctx (funcall (be-ctor entry) :dst result-reg))
  result-reg)

(defun emit-builtin-string-trim (entry args result-reg ctx)
  (let ((bag-reg (compile-ast (first args) ctx))
        (str-reg (compile-ast (second args) ctx)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :char-bag bag-reg :string str-reg))
    result-reg))

(defun emit-builtin-handle-effect (entry args result-reg ctx)
  (let ((handle-reg (compile-ast (first args) ctx)))
    (emit ctx (funcall (be-ctor entry) :handle handle-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defun emit-builtin-zero-compare (entry args result-reg ctx)
  "Emit a comparison of a single argument against zero."
  (let ((arg-reg (compile-ast (first args) ctx))
        (zero-reg (make-register ctx)))
    (emit ctx (make-vm-const :dst zero-reg :value 0))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :lhs arg-reg :rhs zero-reg))
    result-reg))

(defun emit-builtin-stream-input-opt (entry args result-reg ctx)
  "Emit (fn &optional stream): instruction has :dst :handle.
   Slots = (default-handle-value).  If no arg, synthesize default handle."
  (let* ((default-handle (first (be-slots entry)))
         (handle-reg (if (>= (length args) 1)
                         (compile-ast (first args) ctx)
                         (let ((r (make-register ctx)))
                           (emit ctx (make-vm-const :dst r :value default-handle))
                           r))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :handle handle-reg))
    result-reg))

(defun emit-builtin-stream-void-opt (entry args result-reg ctx)
  "Emit (fn &optional stream): instruction has :handle only, returns nil.
   Slots = (default-handle-value)."
  (let* ((default-handle (first (be-slots entry)))
         (handle-reg (if (>= (length args) 1)
                         (compile-ast (first args) ctx)
                         (let ((r (make-register ctx)))
                           (emit ctx (make-vm-const :dst r :value default-handle))
                           r))))
    (emit ctx (funcall (be-ctor entry) :handle handle-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defun emit-builtin-stream-write-val (entry args result-reg ctx)
  "Emit (fn value &optional stream): instruction has :handle + custom value slot.
   Slots = (value-slot-keyword default-handle-value).  Returns value."
  (let* ((val-slot (first (be-slots entry)))
         (default-handle (second (be-slots entry)))
         (val-reg (compile-ast (first args) ctx))
         (handle-reg (if (>= (length args) 2)
                         (compile-ast (second args) ctx)
                         (let ((r (make-register ctx)))
                           (emit ctx (make-vm-const :dst r :value default-handle))
                           r))))
    (emit ctx (funcall (be-ctor entry) :handle handle-reg val-slot val-reg))
    (emit ctx (make-vm-move :dst result-reg :src val-reg))
    result-reg))

(defun emit-builtin-ternary-custom (entry args result-reg ctx)
  "Emit (fn a b c) with 3 custom slot names from be-slots.
   Slots = (slot1 slot2 slot3 return-style).
   :dst return — instruction has :dst, result comes from it.
   :move-third return — void instruction, result←third-arg via move."
  (let* ((slots (be-slots entry))
         (s1 (first slots)) (s2 (second slots)) (s3 (third slots))
         (ret (fourth slots))
         (a-reg (compile-ast (first args) ctx))
         (b-reg (compile-ast (second args) ctx))
         (c-reg (compile-ast (third args) ctx)))
    (ecase ret
      (:dst
       (emit ctx (funcall (be-ctor entry) :dst result-reg
                          s1 a-reg s2 b-reg s3 c-reg))
       result-reg)
      (:move-third
       (emit ctx (funcall (be-ctor entry) s1 a-reg s2 b-reg s3 c-reg))
       (emit ctx (make-vm-move :dst result-reg :src c-reg))
       result-reg))))

(defun emit-builtin-binary-custom (entry args result-reg ctx)
  "Parametric binary emitter: reads slot names from (be-slots entry)."
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx))
        (slots (be-slots entry)))
    (emit ctx (funcall (be-ctor entry) :dst result-reg
                       (first slots) lhs-reg (second slots) rhs-reg))
    result-reg))

(defun emit-builtin-binary-move-first (entry args result-reg ctx)
  "Binary emitter that emits a void instruction (no :dst), then moves arg1→result.
   Used for RPLACA/RPLACD which return the modified cons."
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx))
        (slots (be-slots entry)))
    (emit ctx (funcall (be-ctor entry) (first slots) lhs-reg (second slots) rhs-reg))
    (emit ctx (make-vm-move :dst result-reg :src lhs-reg))
    result-reg))

(defun emit-builtin-binary-void (entry args result-reg ctx)
  "Binary emitter that emits a void instruction (no :dst) and returns nil.
   Used for REMHASH, UNREAD-CHAR."
  (let ((lhs-reg (compile-ast (first args) ctx))
        (rhs-reg (compile-ast (second args) ctx))
        (slots (be-slots entry)))
    (emit ctx (funcall (be-ctor entry) (first slots) lhs-reg (second slots) rhs-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defun emit-builtin-unary-custom-void (entry args result-reg ctx)
  "Unary emitter with a custom slot name (no :dst), returns nil.
   Used for %PROGV-EXIT, ERROR, WARN."
  (let ((arg-reg (compile-ast (first args) ctx))
        (slot (first (be-slots entry))))
    (emit ctx (funcall (be-ctor entry) slot arg-reg))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defun emit-builtin-binary-synth-zero (entry args result-reg ctx)
  "Binary emitter with :dst + 3 custom slots. Synthesizes 0-const for 3rd slot.
   Used for SEARCH."
  (let* ((slots (be-slots entry))
         (a-reg (compile-ast (first args) ctx))
         (b-reg (compile-ast (second args) ctx))
         (zero-reg (make-register ctx)))
    (emit ctx (make-vm-const :dst zero-reg :value 0))
    (emit ctx (funcall (be-ctor entry) :dst result-reg
                       (first slots) a-reg (second slots) b-reg (third slots) zero-reg))
    result-reg))

(defun emit-builtin-binary-opt-nil-slot (entry args result-reg ctx)
  "Binary emitter with :dst + 2 custom slots. 2nd arg optional, nil slot when absent.
   Used for INTERN."
  (let* ((slots (be-slots entry))
         (a-reg (compile-ast (first args) ctx))
         (b-reg (when (second args) (compile-ast (second args) ctx))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg
                       (first slots) a-reg (second slots) b-reg))
    result-reg))

(defun emit-builtin-ternary-opt-nil-custom (entry args result-reg ctx)
  "Ternary emitter with :dst + 3 custom slots. 3rd arg optional, nil when absent.
   Used for GET, SUBSEQ."
  (let* ((slots (be-slots entry))
         (a-reg (compile-ast (first args) ctx))
         (b-reg (compile-ast (second args) ctx))
         (c-reg (if (>= (length args) 3)
                    (compile-ast (third args) ctx)
                    (let ((r (make-register ctx)))
                      (emit ctx (make-vm-const :dst r :value nil)) r))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg
                       (first slots) a-reg (second slots) b-reg (third slots) c-reg))
    result-reg))

(defun emit-builtin-binary-opt-one (entry args result-reg ctx)
  "Binary emitter with optional 2nd arg defaulting to 1.
   Standard :dst :lhs :rhs slots.  Used for FFLOOR/FCEILING/FTRUNCATE/FROUND."
  (let* ((lhs-reg (compile-ast (first args) ctx))
         (rhs-reg (if (= (length args) 2)
                      (compile-ast (second args) ctx)
                      (let ((r (make-register ctx)))
                        (emit ctx (make-vm-const :dst r :value 1)) r))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :lhs lhs-reg :rhs rhs-reg))
    result-reg))

(defun emit-builtin-unary-opt-nil (entry args result-reg ctx)
  "Unary emitter with optional arg defaulting to nil.
   Standard :dst :src slots.  Used for MAKE-RANDOM-STATE."
  (let ((src-reg (if args
                     (compile-ast (first args) ctx)
                     (let ((r (make-register ctx)))
                       (emit ctx (make-vm-const :dst r :value nil)) r))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg :src src-reg))
    result-reg))

(defun emit-builtin-unary-custom (entry args result-reg ctx)
  "Unary emitter with :dst and a custom slot name.
   Used for MAKE-LIST."
  (let ((arg-reg (compile-ast (first args) ctx))
        (slot (first (be-slots entry))))
    (emit ctx (funcall (be-ctor entry) :dst result-reg slot arg-reg))
    result-reg))

;;; ─── Convention Dispatcher ─────────────────────────────────────────────────

(defparameter *builtin-emitter-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :unary        ht) #'emit-builtin-unary)
    (setf (gethash :binary       ht) #'emit-builtin-binary)
    (setf (gethash :string-cmp   ht) #'emit-builtin-string-cmp)
    (setf (gethash :char-cmp     ht) #'emit-builtin-char-cmp)
    (setf (gethash :table-query  ht) #'emit-builtin-table-query)
    (setf (gethash :handle-input ht) #'emit-builtin-handle-input)
    (setf (gethash :side-effect  ht) #'emit-builtin-side-effect)
    (setf (gethash :void-side-eff ht) #'emit-builtin-void-side-eff)
    (setf (gethash :nullary      ht) #'emit-builtin-nullary)
    (setf (gethash :string-trim  ht) #'emit-builtin-string-trim)
    (setf (gethash :handle-effect ht) #'emit-builtin-handle-effect)
    (setf (gethash :binary-custom ht) #'emit-builtin-binary-custom)
    (setf (gethash :binary-move-first ht) #'emit-builtin-binary-move-first)
    (setf (gethash :binary-void ht) #'emit-builtin-binary-void)
    (setf (gethash :unary-custom-void ht) #'emit-builtin-unary-custom-void)
    (setf (gethash :unary-custom ht) #'emit-builtin-unary-custom)
    (setf (gethash :unary-opt-nil ht) #'emit-builtin-unary-opt-nil)
    (setf (gethash :binary-opt-one ht) #'emit-builtin-binary-opt-one)
    (setf (gethash :ternary-opt-nil-custom ht) #'emit-builtin-ternary-opt-nil-custom)
    (setf (gethash :binary-opt-nil-slot ht) #'emit-builtin-binary-opt-nil-slot)
    (setf (gethash :binary-synth-zero ht) #'emit-builtin-binary-synth-zero)
    (setf (gethash :zero-compare ht) #'emit-builtin-zero-compare)
    (setf (gethash :stream-input-opt ht) #'emit-builtin-stream-input-opt)
    (setf (gethash :stream-void-opt  ht) #'emit-builtin-stream-void-opt)
    (setf (gethash :stream-write-val ht) #'emit-builtin-stream-write-val)
    (setf (gethash :ternary-custom ht) #'emit-builtin-ternary-custom)
    ht)
  "Maps convention keywords to their emitter functions.")

(defparameter *convention-arity*
  '((:unary          1 . 1)  (:binary          2 . 2)  (:binary-custom    2 . 2)
    (:string-cmp     2 . 2)  (:char-cmp        2 . 2)  (:table-query      1 . 1)
    (:handle-input   1 . 1)  (:side-effect     1 . 1)  (:void-side-eff    0 . 0)
    (:nullary        0 . 0)  (:string-trim     2 . 2)  (:handle-effect    1 . 1)
    (:zero-compare   1 . 1)  (:unary-custom    1 . 1)  (:unary-custom-void 1 . 1)
    (:binary-move-first 2 . 2) (:binary-void  2 . 2)  (:binary-synth-zero 2 . 2)
    (:unary-opt-nil  0 . 1)  (:binary-opt-one  1 . 2)  (:binary-opt-nil-slot 1 . 2)
    (:ternary-custom 3 . 3)  (:ternary-opt-nil-custom 2 . 3)
    (:stream-input-opt 0 . 1) (:stream-void-opt 0 . 1) (:stream-write-val 1 . 2))
  "Alist of (convention min-args . max-args) for argument count validation.")

(defun emit-registered-builtin (entry args result-reg ctx)
  "Dispatch to the correct emitter for ENTRY's calling convention.
   Returns result-reg on success, or NIL if arg count is out of range."
  (let* ((conv (be-convention entry))
         (arity (cdr (assoc conv *convention-arity* :test #'eq)))
         (nargs (length args)))
    (when (and arity (or (< nargs (car arity)) (> nargs (cdr arity))))
      (return-from emit-registered-builtin nil))
    (let ((emitter (gethash conv *builtin-emitter-table*)))
      (when emitter
        (funcall emitter entry args result-reg ctx)))))
