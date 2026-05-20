(in-package :cl-cc/optimize)

;;; ─── Effect-Kind System ──────────────────────────────────────────────────
;;;
;;; Bridges the CL-CC type system's effect rows to the optimizer's effect
;;; classification. Every VM instruction is classified into one of:
;;;
;;;   :pure        — no observable side effects; deterministic; safe for CSE + DCE
;;;   :read-only   — reads global/heap state but does not modify it; safe for DCE only
;;;   :alloc       — allocates memory; no other observable effect; safe for DCE if unused
;;;   :io          — performs observable I/O; must not be reordered or eliminated
;;;   :write-global — modifies global state (variables, functions, objects, arrays)
;;;   :control     — affects control flow or error handling
;;;   :unknown     — conservative fallback; cannot CSE or DCE
;;;
;;; This replaces the 2-type hardcoded whitelist in opt-inst-pure-p with a
;;; data-driven classification covering 100+ VM instruction types.

;;; ─── Classification Tables ───────────────────────────────────────────────

(defparameter *opt-pure-inst-types*
  '(;; Constants and copies
    vm-const vm-move
    ;; Integer arithmetic (may signal zero-division for div/mod/rem,
    ;; but optimizer already handles folding — kept pure for DCE/CSE)
    vm-add vm-integer-add vm-add-checked vm-float-add
    vm-sub vm-integer-sub vm-sub-checked vm-float-sub
    vm-mul vm-integer-mul vm-mul-checked vm-float-mul vm-fma
    vm-neg vm-abs vm-inc vm-dec
    vm-div vm-cl-div vm-mod vm-rem
    vm-concatenate
    ;; floor/ceiling/truncate/round are :write-global — they set the
    ;; vm-values-list side-channel which DCE must not eliminate
    ;; Integer comparison
    vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq
    ;; Bitwise / shift
    vm-logand vm-logior vm-logxor vm-logeqv vm-ash vm-rotate vm-lognot vm-bswap
    vm-logtest vm-logbitp vm-logcount vm-integer-length
    ;; Boolean
    vm-not vm-and vm-or
    ;; Extrema
    vm-min vm-max
    vm-select
    ;; Numeric predicates
    vm-evenp vm-oddp
    ;; Type predicates
    vm-cons-p vm-null-p vm-null vm-symbol-p vm-number-p vm-integer-p
    vm-function-p vm-listp vm-atom vm-vectorp vm-endp
    vm-typep vm-type-of
    ;; Environment reads (closure captures, function table) — no side effects
    vm-func-ref vm-closure-ref-idx
    ;; Transcendental / floating-point math
    vm-expt vm-sqrt vm-exp-inst vm-log-inst
    vm-sin-inst vm-cos-inst vm-tan-inst
    vm-asin-inst vm-acos-inst vm-atan-inst vm-atan2-inst
    vm-sinh-inst vm-cosh-inst vm-tanh-inst
    ;; Float conversion and inspection
    vm-float-inst vm-float-precision vm-float-radix vm-float-sign
    vm-float-digits vm-scale-float vm-decode-float vm-integer-decode-float
    ;; Rational arithmetic
    vm-rational vm-rationalize vm-numerator vm-denominator
    vm-gcd vm-lcm
    ;; Complex number operations
    vm-realpart vm-imagpart vm-conjugate vm-phase vm-complex
    ;; Character comparisons
    vm-char-not-greaterp vm-char-not-lessp
    ;; Dispatch-based generic arithmetic (pure: only reads, no state change)
    vm-generic-add vm-generic-sub vm-generic-mul vm-generic-div
    vm-generic-eq vm-generic-lt vm-generic-gt)
  "VM instruction struct-type symbols classified as :pure.
   Pure instructions are safe for both CSE (deduplication) and DCE (unused removal).")

(defparameter *opt-alloc-inst-types*
  '(;; Heap allocation without initialization side-effects visible to optimizer
    vm-cons vm-make-string vm-intern-symbol vm-make-list)
  "VM instruction struct-type symbols that allocate without other side effects.
   Allocation instructions are DCE-eligible when their result is unused.")

(defparameter *opt-read-only-inst-types*
  '(;; Global reads (read but do not modify global state)
     vm-get-global vm-boundp vm-fboundp
     vm-gethash vm-gethash-eq vm-gethash-eql vm-gethash-equal
     vm-hash-table-count vm-hash-table-keys vm-hash-table-values
     vm-hash-table-test vm-hash-table-size vm-hash-table-rehash-size
     vm-hash-table-rehash-threshold
     ;; Hardware cache hints: no semantic writes, but keep them ordered around memory.
     vm-prefetch
     ;; Heap reads (non-mutating)
    vm-car vm-cdr vm-slot-read vm-slot-boundp vm-slot-exists-p
    vm-aref vm-svref vm-row-major-aref vm-bit-access vm-sbit
    ;; Sequence inspection
    vm-nth vm-nthcdr vm-first vm-second vm-third vm-fourth vm-fifth vm-rest
    vm-length vm-list-length vm-last vm-butlast
    ;; Equality / search
    vm-equal vm-assoc vm-member
    ;; Array inspection (non-mutating)
    vm-array-rank vm-array-total-size vm-array-dimensions vm-array-dimension
    vm-fill-pointer-inst vm-array-has-fill-pointer-p vm-array-adjustable-p
    ;; Symbol property reads
    vm-symbol-get vm-symbol-plist)
  "VM instruction struct-type symbols that read but do not modify observable state.
   Read-only instructions are DCE-eligible (unused reads can be dropped)
   but NOT CSE-eligible (intervening mutations could change the value).")

(defparameter *opt-io-inst-types*
  '(;; Observable I/O — must not be reordered or eliminated
    vm-print vm-format-inst
    vm-read-char vm-read-line vm-write-string vm-fresh-line vm-write-char
    ;; eval can perform arbitrary I/O and side effects
    vm-eval
    ;; Time/randomness — externally observable
    vm-random vm-make-random-state
    vm-get-universal-time vm-get-internal-real-time vm-get-internal-run-time
    vm-decode-universal-time vm-encode-universal-time)
  "VM instruction struct-type symbols that perform observable I/O.")

(defparameter *opt-write-global-inst-types*
  '(;; Global state mutation
     vm-set-global vm-register-function vm-register-method
     vm-makunbound
     vm-add-package-local-nickname vm-remove-package-local-nickname
     vm-progv-enter vm-progv-exit
     vm-symbol-set vm-remprop vm-set-symbol-plist
     vm-sethash vm-remhash vm-clrhash
    ;; Heap mutation
    vm-rplaca vm-rplacd vm-slot-write
    vm-aset vm-fill vm-copy-vector vm-svset vm-set-fill-pointer vm-vector-push vm-vector-pop
    vm-vector-push-extend vm-adjust-array vm-array-displacement
    vm-bit-set vm-bit-and vm-bit-or vm-bit-xor vm-bit-not
    vm-push vm-pop vm-nconc vm-nreverse
    ;; Multiple-values side-channel producers — write to vm-values-list global
    ;; so DCE must never remove them even if their primary dst is unused
     vm-floor-inst vm-ceiling-inst vm-truncate vm-round-inst
     vm-ffloor vm-fceiling vm-ftruncate vm-fround
     vm-values-regs
    ;; Allocation with initialization effects
    vm-make-array vm-make-hash vm-closure vm-make-closure vm-make-obj
    vm-class-def)
  "VM instruction struct-type symbols that modify global or heap state.")

(defparameter *opt-control-inst-types*
  '(;; Control flow
    vm-label vm-jump vm-jump-zero vm-ret vm-halt
    ;; Error / condition handling
    vm-signal-error
    vm-establish-handler vm-remove-handler vm-sync-handler-regs
    ;; Catch/throw
    vm-establish-catch vm-throw
    ;; CLOS method dispatch continuation
    vm-call-next-method vm-next-method-p)
  "VM instruction struct-type symbols that affect control flow or error handling.")

;;; ─── Effect-Kind Lookup Table ────────────────────────────────────────────

(defparameter *opt-effect-kind-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (tp *opt-pure-inst-types*)         (setf (gethash tp ht) :pure))
    (dolist (tp *opt-alloc-inst-types*)        (setf (gethash tp ht) :alloc))
    (dolist (tp *opt-read-only-inst-types*)    (setf (gethash tp ht) :read-only))
    (dolist (tp *opt-io-inst-types*)           (setf (gethash tp ht) :io))
    (dolist (tp *opt-write-global-inst-types*) (setf (gethash tp ht) :write-global))
    (dolist (tp *opt-control-inst-types*)      (setf (gethash tp ht) :control))
    ht)
  "Maps VM instruction struct-type symbols to their effect-kind keyword.
   Populated at load time from the classification tables above.")

;;; ─── Effect-Kind Query ───────────────────────────────────────────────────

(defun vm-inst-effect-kind (inst)
  "Return the effect-kind of VM instruction INST.
    Effect kinds: :pure :read-only :alloc :io :write-global :control :unknown.
    Unlisted types (vm-call, vm-apply, vm-generic-call, etc.) default to :unknown."
  (or (gethash (type-of inst) *opt-effect-kind-table*) :unknown))

;;; ─── Known Function Property Database (FR-183) ───────────────────────────

(defparameter *known-function-property-groups*
  '(((:pure :foldable :no-escape)
     + - * 1+ 1- abs min max evenp oddp zerop plusp minusp
     ash logand logior logxor logeqv lognot logtest logbitp logcount
     integer-length bswap expt exp sin cos tan asin acos atan sinh cosh
     tanh asinh acosh float float-precision float-radix float-sign
     float-digits rational rationalize numerator denominator gcd lcm realpart
     imagpart conjugate phase complex
     char= char< char> char<= char>= char/= char-equal char-not-equal
     char-lessp char-greaterp char-not-greaterp char-not-lessp
     both-case-p graphic-char-p standard-char-p digit-char alpha-char-p
     alphanumericp upper-case-p lower-case-p char-upcase char-downcase char-code
     char-int code-char digit-char-p type-of sxhash)
    ((:pure :foldable :always-returns :no-escape)
     eq eql not null atom consp symbolp numberp integerp functionp stringp
     characterp vectorp hash-table-p keywordp)
    ((:pure :foldable :no-escape)
     / mod rem sqrt log acosh atanh)
    ((:read-only)
     car cdr first second third fourth fifth sixth seventh eighth ninth tenth rest
     last butlast nth nthcdr member assoc get hash-table-count hash-table-keys
     hash-table-values hash-table-test hash-table-size hash-table-rehash-size
     hash-table-rehash-threshold array-length array-rank array-total-size
     array-dimensions array-dimension array-has-fill-pointer-p array-adjustable-p
     array-displacement row-major-aref svref aref bit sbit char symbol-name
     symbol-plist find-package find-symbol class-name class-of find-class
     string= string< string> string<= string>= string-equal string-lessp
     string-greaterp string/= string-not-equal string-not-greaterp
     string-not-lessp string-length string-upcase string-downcase
     string-capitalize string-concat coerce-to-string string equal listp endp)
    ((:read-only :nonneg-result)
     length string-length list-length array-length array-rank array-total-size
     hash-table-count)
    ((:alloc)
     values-list copy-list copy-tree reverse append cons acons subst subseq complement
     string-trim string-left-trim string-right-trim parse-integer make-symbol
     make-list make-random-state make-string-output-stream
     lisp-implementation-type lisp-implementation-version machine-type
     machine-version machine-instance software-type software-version
     short-site-name long-site-name get-output-stream-string read-from-string)
    ((:io)
     princ prin1 print terpri fresh-line read read-char read-line read-byte
     unread-char peek-char listen write-char write-byte write-line force-output
     finish-output clear-input clear-output file-position file-length close
     get-universal-time get-internal-real-time get-internal-run-time
     decode-universal-time encode-universal-time sleep random gensym)
    ((:write-global)
     set makunbound fdefinition symbol-function nstring-upcase nstring-downcase
     nstring-capitalize nreverse nbutlast nconc nreconc rplaca rplacd remhash
     clrhash remprop vector-pop vector-push vector-push-extend adjust-array
     aset setf-gethash rt-string-set rt-bit-set bit-and bit-ior bit-or bit-xor
     bit-not intern %set-symbol-prop %set-symbol-plist %set-fill-pointer
     %svset %progv-enter %progv-exit)
    ((:control)
     error cerror signal warn eval load macroexpand macroexpand-1 next-method-p))
  "Groups of known CL function properties used by optimizer and compiler metadata.
Properties follow FR-183: :pure, :foldable, :nonneg-result, :always-returns,
:no-escape. Conservative effect tags (:read-only, :alloc, :io, :write-global,
:control) keep impure or heap-reading builtins classified without pretending
they are CSE-safe pure functions.")

(defparameter *known-function-property-table*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (group *known-function-property-groups*)
      (destructuring-bind (properties &rest names) group
        (dolist (name names)
          (let* ((key (string-upcase (symbol-name name)))
                 (existing (gethash key table)))
            (setf (gethash key table)
                  (remove-duplicates (append existing properties) :test #'eq))))))
    table)
  "Maps uppercase CL function names to known function property keywords.")

(defun %known-function-name-key (function-name)
  "Normalize FUNCTION-NAME to the uppercase key used by the property database."
  (etypecase function-name
    (symbol (string-upcase (symbol-name function-name)))
    (string (string-upcase function-name))))

(defun register-known-function-properties (function-name properties)
  "Register or extend known PROPERTIES for FUNCTION-NAME."
  (let* ((key (%known-function-name-key function-name))
         (existing (gethash key *known-function-property-table*)))
    (setf (gethash key *known-function-property-table*)
          (remove-duplicates (append existing properties) :test #'eq))))

(defun known-function-properties (function-name)
  "Return known property keywords for FUNCTION-NAME.
Unknown functions return NIL so callers can remain conservative."
  (copy-list (gethash (%known-function-name-key function-name)
                      *known-function-property-table*)))

(defun known-function-property-p (function-name property)
  "Return true when FUNCTION-NAME is known to have PROPERTY."
  (member property (known-function-properties function-name) :test #'eq))

(defun known-function-effect-kind (function-name)
  "Return the optimizer effect-kind implied by FUNCTION-NAME properties."
  (let ((properties (known-function-properties function-name)))
    (cond
      ((member :control properties :test #'eq) :control)
      ((member :write-global properties :test #'eq) :write-global)
      ((member :io properties :test #'eq) :io)
      ((member :read-only properties :test #'eq) :read-only)
      ((member :alloc properties :test #'eq) :alloc)
      ((member :pure properties :test #'eq) :pure)
      (t :unknown))))

;;; ─── Purity Predicates ───────────────────────────────────────────────────

(defun opt-inst-pure-p (inst)
  "T if INST has no side effects and produces a deterministic result.
   Extended from the original 2-type (vm-const vm-move) whitelist to cover
   100+ instruction types.  Pure instructions are both CSE-eligible and
   DCE-eligible."
  (eq (vm-inst-effect-kind inst) :pure))

(defun opt-inst-dce-eligible-p (inst)
  "T if INST is eligible for dead code elimination when its result is unused.
   Covers :pure (no side effects) and :alloc (allocation only — if the
   allocated object is never used, the allocation can be removed)."
  (member (vm-inst-effect-kind inst) '(:pure :alloc) :test #'eq))

(defun opt-inst-cse-eligible-p (inst)
  "T if INST is eligible for common subexpression elimination.
   Only :pure instructions guarantee the same result for the same inputs
   regardless of intervening instructions.  :alloc creates distinct objects
   even with the same arguments, so it is NOT CSE-eligible."
  (eq (vm-inst-effect-kind inst) :pure))

;;; ─── Effect-Row Bridge ───────────────────────────────────────────────────

(defun effect-row->effect-kind (effect-row)
  "Convert a cl-cc/type system type-effect-row to an optimizer effect-kind.
   Used when callee effect information is available from the HM type system.
   Compares effect names by string= to avoid cross-package symbol mismatch."
  (let* ((effects (cl-cc/type:type-effect-row-effects effect-row))
          (names   (mapcar (lambda (e)
                             (string-upcase (symbol-name (cl-cc/type:type-effect-op-name e))))
                           effects)))
    (cond
      ((null effects)                          :pure)
      ((member "IO"    names :test #'string=)  :io)
      ((member "STATE" names :test #'string=)  :write-global)
      ((member "ERROR" names :test #'string=)  :control)
      (t                                       :unknown))))
