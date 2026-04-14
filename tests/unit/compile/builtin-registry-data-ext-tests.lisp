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
(in-suite cl-cc-suite)

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
  :cases (("table-query"        cl-cc::*builtin-table-query-entries*)
          ("handle-input"       cl-cc::*builtin-handle-input-entries*)
          ("side-effect"        cl-cc::*builtin-side-effect-entries*)
          ("void-side-effect"   cl-cc::*builtin-void-side-effect-entries*)
          ("nullary"            cl-cc::*builtin-nullary-entries*)
          ("string-trim"        cl-cc::*builtin-string-trim-entries*)
          ("handle-effect"      cl-cc::*builtin-handle-effect-entries*)
          ("binary-custom"      cl-cc::*builtin-binary-custom-entries*)
          ("binary-move-first"  cl-cc::*builtin-binary-move-first-entries*)
          ("binary-void"        cl-cc::*builtin-binary-void-entries*)
          ("unary-custom-void"  cl-cc::*builtin-unary-custom-void-entries*)
          ("unary-opt-nil"      cl-cc::*builtin-unary-opt-nil-entries*)
          ("binary-opt-one"     cl-cc::*builtin-binary-opt-one-entries*)
          ("binary-opt-nil-slot" cl-cc::*builtin-binary-opt-nil-slot-entries*)
          ("ternary-opt-nil"    cl-cc::*builtin-ternary-opt-nil-custom-entries*)
          ("binary-synth-zero"  cl-cc::*builtin-binary-synth-zero-entries*)
          ("unary-custom"       cl-cc::*builtin-unary-custom-entries*)
          ("zero-compare"       cl-cc::*builtin-zero-compare-entries*)
          ("stream-input-opt"   cl-cc::*builtin-stream-input-opt-entries*)
          ("stream-void-opt"    cl-cc::*builtin-stream-void-opt-entries*)
          ("stream-write-val"   cl-cc::*builtin-stream-write-val-entries*)
          ("ternary-custom"     cl-cc::*builtin-ternary-custom-entries*))
  (table)
  (assert-true (> (length table) 0)))

;;; ─── *builtin-table-query-entries* ──────────────────────────────────────

(deftest-each builtin-table-query-representative-entries
  "*builtin-table-query-entries* maps hash-table query fns to their constructors."
  :cases (("hash-table-count"  'cl-cc::hash-table-count  'cl-cc::make-vm-hash-table-count)
          ("hash-table-keys"   'cl-cc::hash-table-keys   'cl-cc::make-vm-hash-table-keys)
          ("hash-table-values" 'cl-cc::hash-table-values 'cl-cc::make-vm-hash-table-values)
          ("hash-table-test"   'cl-cc::hash-table-test   'cl-cc::make-vm-hash-table-test))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc::*builtin-table-query-entries*))))

;;; ─── *builtin-handle-input-entries* ─────────────────────────────────────

(deftest-each builtin-handle-input-representative-entries
  "*builtin-handle-input-entries* maps handle-input fns to their constructors."
  :cases (("file-position" 'file-position 'cl-cc::make-vm-file-position)
          ("file-length"   'file-length   'cl-cc::make-vm-file-length)
          ("read-byte"     'read-byte     'cl-cc::make-vm-read-byte)
          ("listen"        'listen        'cl-cc::make-vm-listen-inst))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc::*builtin-handle-input-entries*))))

;;; ─── *builtin-side-effect-entries* ──────────────────────────────────────

(deftest-each builtin-side-effect-representative-entries
  "*builtin-side-effect-entries* maps print fns to their constructors."
  :cases (("princ"  'princ  'cl-cc::make-vm-princ)
          ("prin1"  'prin1  'cl-cc::make-vm-prin1)
          ("print"  'print  'cl-cc::make-vm-print-inst))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc::*builtin-side-effect-entries*))))

;;; ─── *builtin-void-side-effect-entries* ─────────────────────────────────

(deftest-each builtin-void-side-effect-entries
  "*builtin-void-side-effect-entries* maps terpri/fresh-line to constructors."
  :cases (("terpri"     'terpri     'cl-cc::make-vm-terpri-inst)
          ("fresh-line" 'fresh-line 'cl-cc::make-vm-fresh-line-inst))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc::*builtin-void-side-effect-entries*))))

;;; ─── *builtin-nullary-entries* ───────────────────────────────────────────

(deftest-each builtin-nullary-representative-entries
  "*builtin-nullary-entries* maps nullary fns to constructors."
  :cases (("gensym"               'gensym               'cl-cc::make-vm-gensym-inst)
          ("get-universal-time"   'get-universal-time   'cl-cc::make-vm-get-universal-time)
          ("next-method-p"        'next-method-p        'cl-cc::make-vm-next-method-p)
          ("lisp-implementation-type" 'lisp-implementation-type 'cl-cc::make-vm-lisp-implementation-type))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc::*builtin-nullary-entries*))))

(deftest builtin-nullary-table-size
  "*builtin-nullary-entries* has at least 10 entries (env queries + core ops)."
  (assert-true (>= (length cl-cc::*builtin-nullary-entries*) 10)))

;;; ─── *builtin-string-trim-entries* ──────────────────────────────────────

(deftest-each builtin-string-trim-all-variants
  "*builtin-string-trim-entries* contains all three trim variants."
  :cases (("string-trim"       'string-trim       'cl-cc::make-vm-string-trim)
          ("string-left-trim"  'string-left-trim  'cl-cc::make-vm-string-left-trim)
          ("string-right-trim" 'string-right-trim 'cl-cc::make-vm-string-right-trim))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc::*builtin-string-trim-entries*))))

;;; ─── *builtin-binary-custom-entries* ────────────────────────────────────

(deftest-each builtin-binary-custom-representative-entries
  "*builtin-binary-custom-entries* holds 4-element list entries for custom-slot builtins."
  :cases (("nth"    'nth    'cl-cc::make-vm-nth    :index  :list)
          ("cons"   'cons   'cl-cc::make-vm-cons   :car-src :cdr-src)
          ("append" 'append 'cl-cc::make-vm-append :src1   :src2)
          ("aref"   'aref   'cl-cc::make-vm-aref   :array-reg :index-reg))
  (sym expected-ctor slot1 slot2)
  (let ((entry (assoc sym cl-cc::*builtin-binary-custom-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))))

(deftest builtin-binary-custom-table-size
  "*builtin-binary-custom-entries* has at least 10 entries."
  (assert-true (>= (length cl-cc::*builtin-binary-custom-entries*) 10)))

;;; ─── *builtin-binary-move-first-entries* ────────────────────────────────

(deftest-each builtin-binary-move-first-rplaca-rplacd
  "*builtin-binary-move-first-entries* contains rplaca and rplacd."
  :cases (("rplaca" 'rplaca 'cl-cc::make-vm-rplaca :cons :val)
          ("rplacd" 'rplacd 'cl-cc::make-vm-rplacd :cons :val))
  (sym expected-ctor slot1 slot2)
  (let ((entry (assoc sym cl-cc::*builtin-binary-move-first-entries*)))
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
  (let ((entry (assoc sym cl-cc::*builtin-binary-void-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))))

;;; ─── *builtin-unary-custom-void-entries* ────────────────────────────────

(deftest-each builtin-unary-custom-void-representative
  "*builtin-unary-custom-void-entries* maps void unary fns to constructors."
  :cases (("error"   'error   'cl-cc::make-vm-signal-error :error-reg)
          ("signal"  'signal  'cl-cc::make-vm-signal       :condition-reg)
          ("clrhash" 'clrhash 'cl-cc::make-vm-clrhash      :table))
  (sym expected-ctor slot)
  (let ((entry (assoc sym cl-cc::*builtin-unary-custom-void-entries*)))
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
  (assert-equal expected-ctor (cdr (assoc sym cl-cc::*builtin-binary-opt-one-entries*))))

;;; ─── *builtin-ternary-opt-nil-custom-entries* ────────────────────────────

(deftest-each builtin-ternary-opt-nil-custom-representative
  "*builtin-ternary-opt-nil-custom-entries* maps get and subseq."
  :cases (("get"    'get    'cl-cc::make-vm-symbol-get :sym    :indicator :default)
          ("subseq" 'subseq 'cl-cc::make-vm-subseq     :string :start     :end))
  (sym expected-ctor slot1 slot2 slot3)
  (let ((entry (assoc sym cl-cc::*builtin-ternary-opt-nil-custom-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))
    (assert-eq slot3 (fifth entry))))

;;; ─── *builtin-zero-compare-entries* ─────────────────────────────────────

(deftest-each builtin-zero-compare-zerop-plusp-minusp
  "*builtin-zero-compare-entries* maps zerop/plusp/minusp to comparison constructors."
  :cases (("zerop"  'zerop  'cl-cc::make-vm-num-eq)
          ("plusp"  'plusp  'cl-cc::make-vm-gt)
          ("minusp" 'minusp 'cl-cc::make-vm-lt))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc::*builtin-zero-compare-entries*))))

;;; ─── *builtin-stream-input-opt-entries* ──────────────────────────────────

(deftest-each builtin-stream-input-opt-representative
  "*builtin-stream-input-opt-entries* contains read-char/read-line/peek-char with default handles."
  :cases (("read-char"  'read-char  'cl-cc::make-vm-read-char  0)
          ("read-line"  'read-line  'cl-cc::make-vm-read-line  0)
          ("peek-char"  'peek-char  'cl-cc::make-vm-peek-char  0))
  (sym expected-ctor default-handle)
  (let ((entry (find sym cl-cc::*builtin-stream-input-opt-entries* :key #'first)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-= default-handle (third entry))))

;;; ─── *builtin-stream-void-opt-entries* ───────────────────────────────────

(deftest-each builtin-stream-void-opt-representative
  "*builtin-stream-void-opt-entries* contains force-output/finish-output with stdout defaults."
  :cases (("force-output"   'force-output   'cl-cc::make-vm-force-output   1)
          ("finish-output"  'finish-output  'cl-cc::make-vm-finish-output  1)
          ("clear-input"    'clear-input    'cl-cc::make-vm-clear-input    0))
  (sym expected-ctor default-handle)
  (let ((entry (find sym cl-cc::*builtin-stream-void-opt-entries* :key #'first)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-= default-handle (third entry))))

;;; ─── *builtin-stream-write-val-entries* ──────────────────────────────────

(deftest-each builtin-stream-write-val-representative
  "*builtin-stream-write-val-entries* contains write-char/write-byte/write-line."
  :cases (("write-char" 'write-char 'cl-cc::make-vm-write-char :char     1)
          ("write-byte" 'write-byte 'cl-cc::make-vm-write-byte :byte-val 1)
          ("write-line" 'write-line 'cl-cc::make-vm-write-line :str      1))
  (sym expected-ctor value-slot default-handle)
  (let ((entry (find sym cl-cc::*builtin-stream-write-val-entries* :key #'first)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq value-slot (third entry))
    (assert-= default-handle (fourth entry))))

;;; ─── *builtin-ternary-custom-entries* ───────────────────────────────────

(deftest-each builtin-ternary-custom-representative
  "*builtin-ternary-custom-entries* holds 6-element list entries."
  :cases (("acons"  'cl-cc::acons  'cl-cc::make-vm-acons   :key       :value     :alist     :dst)
          ("subst"  'cl-cc::subst  'cl-cc::make-vm-subst   :new-val   :old-val   :tree      :dst)
          ("aset"   'cl-cc::aset   'cl-cc::make-vm-aset    :array-reg :index-reg :val-reg   :move-third))
  (sym expected-ctor slot1 slot2 slot3 return-style)
  (let ((entry (assoc sym cl-cc::*builtin-ternary-custom-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))
    (assert-eq slot3 (fifth entry))
    (assert-eq return-style (sixth entry))))

(deftest builtin-ternary-custom-table-size
  "*builtin-ternary-custom-entries* has at least 5 entries."
  (assert-true (>= (length cl-cc::*builtin-ternary-custom-entries*) 5)))
