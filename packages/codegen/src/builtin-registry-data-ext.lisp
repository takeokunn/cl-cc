;;;; compile/builtin-registry-data-ext.lisp — Extended builtin entry alist tables
;;;
;;; Comparison tables (string-cmp, char-cmp) and all specialized calling-convention
;;; tables: handle I/O, stream operations, void side-effects, nullary, string-trim,
;;; binary-custom, ternary-custom, and opt/nil/move-first/synth conventions (2026).
;;;
;;; Core unary/binary entry tables are in builtin-registry-data.lisp (loads before).
;;;
;;; Load order: after builtin-registry-data.lisp, before builtin-registry.lisp.
(in-package :cl-cc/codegen)

;;; ─── Comparison Tables ──────────────────────────────────────────────────────

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

;;; ─── Specialized Convention Tables ─────────────────────────────────────────

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
  (rt-string-set     make-vm-string-set :str       :idx       :val       :dst)
    (rt-bit-set        make-vm-bit-set    :arr       :idx       :val       :dst))
  "List of (cl-sym vm-ctor slot1 slot2 slot3 return-style) for ternary builtins.")
