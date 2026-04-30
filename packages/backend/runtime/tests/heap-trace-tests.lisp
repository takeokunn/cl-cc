;;;; tests/unit/runtime/heap-trace-tests.lisp — Card Table + Address Predicate Tests
;;;
;;; Tests for src/runtime/heap-trace.lisp:
;;;   - Card table helpers: rt-card-index, rt-card-dirty-p,
;;;     rt-card-mark-dirty, rt-card-clear, rt-card-clear-all
;;;   - Address predicates: rt-young-addr-p, rt-old-addr-p, rt-heap-addr-p
;;;   - Pointer slot resolver: rt-object-pointer-slots

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------

(defsuite heap-trace-suite
  :description "Card table and address predicate tests (heap-trace.lisp)"
  :parent cl-cc-unit-suite)

(in-suite heap-trace-suite)

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun %make-trace-heap ()
  "Create a minimal heap for tracing tests.
   young-size=32 → semi-size=16, young-from-base=0, young addresses 0..15.
   old-size=32  → old-base=32, old addresses 32..63, num-cards=ceiling(32/64)=1."
  (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))

(defun %write-obj-header (heap addr size tag)
  "Write an object header at ADDR with SIZE words and TYPE-TAG."
  (cl-cc/runtime:rt-heap-set-header
   heap addr
   (cl-cc/runtime:make-header size tag 0)))

;;; ------------------------------------------------------------
;;; Card Table: rt-card-index
;;; ------------------------------------------------------------

(deftest heap-trace-card-index-at-old-base-is-zero
  "rt-card-index at old-base address returns 0."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (assert-= 0 (cl-cc/runtime:rt-card-index heap old-base))))

(deftest heap-trace-card-index-within-first-card-is-zero
  "rt-card-index 10 bytes past old-base returns 0 (same card, card-size=64)."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (assert-= 0 (cl-cc/runtime:rt-card-index heap (+ old-base 10)))))

;;; ------------------------------------------------------------
;;; Card Table: mark / clear / dirty-p
;;; ------------------------------------------------------------

(deftest heap-trace-card-mark-clear-lifecycle
  "Card starts clean; mark-dirty makes dirty-p true; clear reverts it; clear-all clears all."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (assert-false (cl-cc/runtime:rt-card-dirty-p heap old-base))
    (cl-cc/runtime:rt-card-mark-dirty heap old-base)
    (assert-true (cl-cc/runtime:rt-card-dirty-p heap old-base))
    (cl-cc/runtime:rt-card-clear heap old-base)
    (assert-false (cl-cc/runtime:rt-card-dirty-p heap old-base))
    (cl-cc/runtime:rt-card-mark-dirty heap old-base)
    (cl-cc/runtime:rt-card-clear-all heap)
    (assert-false (cl-cc/runtime:rt-card-dirty-p heap old-base))))

;;; ------------------------------------------------------------
;;; Address Predicates
;;; ------------------------------------------------------------

(deftest-each heap-trace-young-addr-p-cases
  "rt-young-addr-p returns true iff addr is within young from-space [0, 16)."
  :cases (("in-range-0"  0  t)
          ("in-range-10" 10 t)
          ("in-range-15" 15 t)
          ("boundary-16" 16 nil)
          ("old-base-32" 32 nil)
          ("negative"    -1 nil))
  (addr expected)
  (let ((heap (%make-trace-heap)))
    (assert-equal expected (not (not (cl-cc/runtime:rt-young-addr-p heap addr))))))

(deftest-each heap-trace-old-addr-p-cases
  "rt-old-addr-p returns true iff addr is within old-space [32, 64)."
  :cases (("below-old-31" 31 nil)
          ("old-base-32"  32 t)
          ("old-mid-40"   40 t)
          ("old-end-63"   63 t)
          ("beyond-64"    64 nil)
          ("young-0"      0  nil))
  (addr expected)
  (let ((heap (%make-trace-heap)))
    (assert-equal expected (not (not (cl-cc/runtime:rt-old-addr-p heap addr))))))

(deftest-each heap-trace-heap-addr-p-cases
  "rt-heap-addr-p is the union of young and old: true for 0..15 and 32..63."
  :cases (("young-0"      0  t)
          ("young-15"     15 t)
          ("gap-16"       16 nil)
          ("gap-31"       31 nil)
          ("old-32"       32 t)
          ("old-63"       63 t)
          ("beyond-64"    64 nil))
  (addr expected)
  (let ((heap (%make-trace-heap)))
    (assert-equal expected (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))))))

;;; ------------------------------------------------------------
;;; rt-object-pointer-slots
;;; ------------------------------------------------------------

(deftest heap-trace-pointer-slots-cons-tag
  "Cons (size=3, tag=1): pointer slots are (1 2) — car and cdr."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 3 1)
    (assert-equal '(1 2) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-symbol-tag
  "Symbol (size=4, tag=2): pointer slots are (1 2 3) — name, pkg, value."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 2)
    (assert-equal '(1 2 3) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-closure-4-tag
  "Closure-4 (size=4, tag=3): pointer slots are (2 3) — env and code."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 3)
    (assert-equal '(2 3) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-closure-2-tag
  "Closure-2 (size=2, tag=3): pointer slots are () — no pointer fields."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 2 3)
    (assert-equal '() (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-array-4-tag
  "Array-4 (size=4, tag=5): pointer slots are (2 3) — elements."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 5)
    (assert-equal '(2 3) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-other-3-tag
  "Other-3 (size=3, tag=7): pointer slots are (1 2)."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 3 7)
    (assert-equal '(1 2) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-string-tag-returns-nil
  "String (size=5, tag=6): rt-object-pointer-slots returns nil."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 5 6)
    (assert-equal nil (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-unknown-tag-returns-nil
  "Unknown object (size=2, tag=0): rt-object-pointer-slots returns nil."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 2 0)
    (assert-equal nil (cl-cc/runtime:rt-object-pointer-slots heap 0))))
