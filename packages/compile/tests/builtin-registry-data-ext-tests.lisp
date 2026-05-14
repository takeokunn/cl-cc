;;;; tests/unit/compile/builtin-registry-data-ext-tests.lisp
;;;; Unit tests for src/compile/builtin-registry-data-ext.lisp
;;;;
;;;; Validates all 17 extended calling-convention data tables:
;;;; table-query, handle-input, side-effect, void-side-effect, nullary,
;;;; string-trim, handle-effect, binary-custom, binary-move-first,
;;;; binary-void, unary-custom-void, unary-opt-nil, binary-opt-one,
;;;; binary-opt-nil-slot, ternary-opt-nil-custom, binary-synth-zero,
;;;; unary-custom, zero-compare, stream-input-opt, stream-void-opt,
;;;; stream-write-val, ternary-custom.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helper ──────────────────────────────────────────────────────────────

(defun %table-ctor-sym (entry)
  "Extract the constructor symbol from a data table entry.
   Handles both (sym . ctor) cons cells and (sym ctor ...) lists."
  (if (consp (cdr entry))
      (second entry)   ; list form: (sym ctor slot1 ...)
      (cdr entry)))    ; cons form: (sym . ctor)

;;; ─── Table size sanity ───────────────────────────────────────────────────

(deftest-each builtin-ext-table-non-empty
  "Every extended calling-convention table is non-empty."
  :cases (("table-query"        cl-cc/compile::*builtin-table-query-entries*)
          ("handle-input"       cl-cc/compile::*builtin-handle-input-entries*)
          ("side-effect"        cl-cc/compile::*builtin-side-effect-entries*)
          ("void-side-effect"   cl-cc/compile::*builtin-void-side-effect-entries*)
          ("nullary"            cl-cc/compile::*builtin-nullary-entries*)
          ("string-trim"        cl-cc/compile::*builtin-string-trim-entries*)
          ("handle-effect"      cl-cc/compile::*builtin-handle-effect-entries*)
          ("binary-custom"      cl-cc/compile::*builtin-binary-custom-entries*)
          ("binary-move-first"  cl-cc/compile::*builtin-binary-move-first-entries*)
          ("binary-void"        cl-cc/compile::*builtin-binary-void-entries*)
          ("unary-custom-void"  cl-cc/compile::*builtin-unary-custom-void-entries*)
          ("unary-opt-nil"      cl-cc/compile::*builtin-unary-opt-nil-entries*)
          ("binary-opt-one"     cl-cc/compile::*builtin-binary-opt-one-entries*)
          ("binary-opt-nil-slot" cl-cc/compile::*builtin-binary-opt-nil-slot-entries*)
          ("ternary-opt-nil"    cl-cc/compile::*builtin-ternary-opt-nil-custom-entries*)
          ("binary-synth-zero"  cl-cc/compile::*builtin-binary-synth-zero-entries*)
          ("unary-custom"       cl-cc/compile::*builtin-unary-custom-entries*)
          ("zero-compare"       cl-cc/compile::*builtin-zero-compare-entries*)
          ("stream-input-opt"   cl-cc/compile::*builtin-stream-input-opt-entries*)
          ("stream-void-opt"    cl-cc/compile::*builtin-stream-void-opt-entries*)
          ("stream-write-val"   cl-cc/compile::*builtin-stream-write-val-entries*)
          ("ternary-custom"     cl-cc/compile::*builtin-ternary-custom-entries*))
  (table)
  (assert-true (> (length table) 0)))

(deftest builtin-binary-opt-nil-slot-find-symbol-entry
  "find-symbol uses the binary optional package convention and VM constructor."
  (let ((entry (assoc 'find-symbol cl-cc/compile::*builtin-binary-opt-nil-slot-entries*)))
    (assert-equal '(find-symbol cl-cc::make-vm-find-symbol :src :pkg) entry)))

(deftest-each builtin-table-minimum-sizes
  "Key tables have minimum entry counts for their respective calling conventions."
  :cases (("nullary"        cl-cc/compile::*builtin-nullary-entries*        10)
          ("binary-custom"  cl-cc/compile::*builtin-binary-custom-entries*  10)
          ("ternary-custom" cl-cc/compile::*builtin-ternary-custom-entries*  5))
  (table min-size)
  (assert-true (>= (length table) min-size)))

;;; ─── *builtin-string-cmp-entries* (extended case-insensitive family) ──────

(deftest-each builtin-string-cmp-extended-representative-entries
  "Extended string comparison builtins stay wired to the expected VM constructors."
  :cases (("string-equal" 'string-equal 'cl-cc::make-vm-string-equal)
          ("string-lessp" 'string-lessp 'cl-cc::make-vm-string-lessp)
          ("string-greaterp" 'string-greaterp 'cl-cc::make-vm-string-greaterp)
          ("string-not-equal" 'string-not-equal 'cl-cc::make-vm-string-not-equal)
          ("string-not-greaterp" 'string-not-greaterp 'cl-cc::make-vm-string-not-greaterp)
          ("string-not-lessp" 'string-not-lessp 'cl-cc::make-vm-string-not-lessp))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-string-cmp-entries*))))

;;; ─── *builtin-table-query-entries* ──────────────────────────────────────

(deftest-each builtin-table-query-representative-entries
  "*builtin-table-query-entries* maps hash-table query fns to their constructors."
  :cases (("hash-table-count"  'hash-table-count                'cl-cc/vm:make-vm-hash-table-count)
          ("hash-table-keys"   'cl-cc/compile::hash-table-keys  'cl-cc/vm:make-vm-hash-table-keys)
          ("hash-table-values" 'cl-cc/vm:hash-table-values       'cl-cc/vm:make-vm-hash-table-values)
          ("hash-table-test"   'hash-table-test                  'cl-cc/vm:make-vm-hash-table-test))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-table-query-entries*))))

;;; ─── *builtin-handle-input-entries* ─────────────────────────────────────

(deftest-each builtin-handle-input-representative-entries
  "*builtin-handle-input-entries* maps handle-input fns to their constructors."
  :cases (("file-position" 'file-position 'cl-cc::make-vm-file-position)
          ("file-length"   'file-length   'cl-cc::make-vm-file-length)
          ("read-byte"     'read-byte     'cl-cc::make-vm-read-byte)
          ("listen"        'listen        'cl-cc::make-vm-listen-inst))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-handle-input-entries*))))

;;; ─── *builtin-side-effect-entries* ──────────────────────────────────────

(deftest-each builtin-side-effect-representative-entries
  "*builtin-side-effect-entries* maps print fns to their constructors."
  :cases (("princ"  'princ  'cl-cc::make-vm-princ)
          ("prin1"  'prin1  'cl-cc::make-vm-prin1)
          ("print"  'print  'cl-cc::make-vm-print-inst))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-side-effect-entries*))))

;;; ─── *builtin-void-side-effect-entries* ─────────────────────────────────

(deftest-each builtin-void-side-effect-entries
  "*builtin-void-side-effect-entries* maps terpri/fresh-line to constructors."
  :cases (("terpri"     'terpri     'cl-cc::make-vm-terpri-inst)
          ("fresh-line" 'fresh-line 'cl-cc::make-vm-fresh-line-inst))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-void-side-effect-entries*))))

;;; ─── *builtin-nullary-entries* ───────────────────────────────────────────

(deftest-each builtin-nullary-representative-entries
  "*builtin-nullary-entries* maps nullary fns to constructors."
  :cases (("gensym"               'gensym               'cl-cc::make-vm-gensym-inst)
          ("get-universal-time"   'get-universal-time   'cl-cc::make-vm-get-universal-time)
          ("next-method-p"        'next-method-p        'cl-cc::make-vm-next-method-p)
          ("lisp-implementation-type" 'lisp-implementation-type 'cl-cc::make-vm-lisp-implementation-type))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-nullary-entries*))))

;;; ─── *builtin-string-trim-entries* ──────────────────────────────────────

(deftest-each builtin-string-trim-all-variants
  "*builtin-string-trim-entries* contains all three trim variants."
  :cases (("string-trim"       'string-trim       'cl-cc::make-vm-string-trim)
          ("string-left-trim"  'string-left-trim  'cl-cc::make-vm-string-left-trim)
          ("string-right-trim" 'string-right-trim 'cl-cc::make-vm-string-right-trim))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-string-trim-entries*))))

;;; ─── *builtin-binary-custom-entries* ────────────────────────────────────

(deftest-each builtin-binary-custom-representative-entries
  "*builtin-binary-custom-entries* holds 4-element list entries for custom-slot builtins."
  :cases (("nth"    'nth    'cl-cc::make-vm-nth    :index  :list)
          ("cons"   'cons   'cl-cc::make-vm-cons   :car-src :cdr-src)
          ("hash-cons" 'cl-cc/compile::hash-cons 'cl-cc::make-vm-hash-cons :car-src :cdr-src)
          ("append" 'append 'cl-cc::make-vm-append :src1   :src2)
          ("aref"   'aref   'cl-cc::make-vm-aref   :array-reg :index-reg))
  (sym expected-ctor slot1 slot2)
  (let ((entry (assoc sym cl-cc/compile::*builtin-binary-custom-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))))

;;; ─── *builtin-binary-move-first-entries* ────────────────────────────────

(deftest-each builtin-binary-move-first-rplaca-rplacd
  "*builtin-binary-move-first-entries* contains rplaca and rplacd."
  :cases (("rplaca" 'rplaca 'cl-cc::make-vm-rplaca :cons :val)
          ("rplacd" 'rplacd 'cl-cc::make-vm-rplacd :cons :val))
  (sym expected-ctor slot1 slot2)
  (let ((entry (assoc sym cl-cc/compile::*builtin-binary-move-first-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))))

;;; ─── *builtin-binary-void-entries* ──────────────────────────────────────

(deftest-each builtin-binary-void-representative
  "*builtin-binary-void-entries* maps void binary fns to constructors."
  :cases (("remhash"     'remhash     'cl-cc::make-vm-remhash     :key    :table)
          ("unread-char" 'unread-char 'cl-cc::make-vm-unread-char :char   :handle))
  (sym expected-ctor slot1 slot2)
  (let ((entry (assoc sym cl-cc/compile::*builtin-binary-void-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))))

;;; ─── *builtin-unary-custom-void-entries* ────────────────────────────────

(deftest-each builtin-unary-custom-void-representative
  "*builtin-unary-custom-void-entries* maps void unary fns to constructors."
  :cases (("error"   'error   'cl-cc/vm:make-vm-signal-error :error-reg)
          ("signal"  'signal  'cl-cc/compile::make-vm-signal :condition-reg)
          ("clrhash" 'clrhash 'cl-cc/vm:make-vm-clrhash      :table))
  (sym expected-ctor slot)
  (let ((entry (assoc sym cl-cc/compile::*builtin-unary-custom-void-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot (third entry))))

;;; ─── *builtin-binary-opt-one-entries* ───────────────────────────────────

(deftest-each builtin-binary-opt-one-ffloor-family
  "*builtin-binary-opt-one-entries* contains ffloor/fceiling/ftruncate/fround."
  :cases (("ffloor"    'ffloor    'cl-cc::make-vm-ffloor)
          ("fceiling"  'fceiling  'cl-cc::make-vm-fceiling)
          ("ftruncate" 'ftruncate 'cl-cc::make-vm-ftruncate)
          ("fround"    'fround    'cl-cc::make-vm-fround))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-binary-opt-one-entries*))))

;;; ─── *builtin-ternary-opt-nil-custom-entries* ────────────────────────────

(deftest-each builtin-ternary-opt-nil-custom-representative
  "*builtin-ternary-opt-nil-custom-entries* maps get and subseq."
  :cases (("get"    'get    'cl-cc::make-vm-symbol-get :sym    :indicator :default)
          ("subseq" 'subseq 'cl-cc::make-vm-subseq     :string :start     :end))
  (sym expected-ctor slot1 slot2 slot3)
  (let ((entry (assoc sym cl-cc/compile::*builtin-ternary-opt-nil-custom-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))
    (assert-eq slot3 (fifth entry))))
