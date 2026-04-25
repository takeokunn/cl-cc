;;;; tests/unit/compile/builtin-registry-stream-tests.lisp
;;;; Continuation of builtin-registry-data-ext-tests.lisp.
;;;; Tests for stream/zero-compare/ternary-custom calling-convention tables.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── *builtin-zero-compare-entries* ─────────────────────────────────────

(deftest-each builtin-zero-compare-zerop-plusp-minusp
  "*builtin-zero-compare-entries* maps zerop/plusp/minusp to comparison constructors."
  :cases (("zerop"  'zerop  'cl-cc::make-vm-num-eq)
          ("plusp"  'plusp  'cl-cc::make-vm-gt)
          ("minusp" 'minusp 'cl-cc::make-vm-lt))
  (sym expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-zero-compare-entries*))))

;;; ─── *builtin-stream-input-opt-entries* ──────────────────────────────────

(deftest-each builtin-stream-input-opt-representative
  "*builtin-stream-input-opt-entries* contains read-char/read-line/peek-char with default handles."
  :cases (("read-char"  'read-char  'cl-cc::make-vm-read-char  0)
          ("read-line"  'read-line  'cl-cc::make-vm-read-line  0)
          ("peek-char"  'peek-char  'cl-cc::make-vm-peek-char  0))
  (sym expected-ctor default-handle)
  (let ((entry (find sym cl-cc/compile::*builtin-stream-input-opt-entries* :key #'first)))
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
  (let ((entry (find sym cl-cc/compile::*builtin-stream-void-opt-entries* :key #'first)))
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
  (let ((entry (find sym cl-cc/compile::*builtin-stream-write-val-entries* :key #'first)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq value-slot (third entry))
    (assert-= default-handle (fourth entry))))

;;; ─── *builtin-ternary-custom-entries* ───────────────────────────────────

(deftest-each builtin-ternary-custom-representative
  "*builtin-ternary-custom-entries* holds 6-element list entries."
  :cases (("acons"  'acons                  'cl-cc/vm:make-vm-acons   :key       :value     :alist     :dst)
          ("subst"  'subst                  'cl-cc/vm:make-vm-subst   :new-val   :old-val   :tree      :dst)
          ("aset"   'cl-cc/compile::aset    'cl-cc/vm:make-vm-aset   :array-reg :index-reg :val-reg   :move-third))
  (sym expected-ctor slot1 slot2 slot3 return-style)
  (let ((entry (assoc sym cl-cc/compile::*builtin-ternary-custom-entries*)))
    (assert-true entry)
    (assert-equal expected-ctor (second entry))
    (assert-eq slot1 (third entry))
    (assert-eq slot2 (fourth entry))
    (assert-eq slot3 (fifth entry))
    (assert-eq return-style (sixth entry))))
