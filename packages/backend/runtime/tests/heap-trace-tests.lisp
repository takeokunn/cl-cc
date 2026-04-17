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

(deftest heap-trace-card-index-base
  "rt-card-index returns 0 for old-base itself."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (assert-= 0 (cl-cc/runtime:rt-card-index heap old-base))))

(deftest heap-trace-card-index-mid
  "rt-card-index returns 0 for addresses within the same card (0..63 words from base)."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    ;; old-size=32 words, card-size=64 words → only card 0 exists
    (assert-= 0 (cl-cc/runtime:rt-card-index heap (+ old-base 10)))))

;;; ------------------------------------------------------------
;;; Card Table: mark / clear / dirty-p
;;; ------------------------------------------------------------

(deftest heap-trace-card-initially-clean
  "All cards are clean after heap creation."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (assert-false (cl-cc/runtime:rt-card-dirty-p heap old-base))))

(deftest heap-trace-card-mark-then-dirty
  "rt-card-mark-dirty makes rt-card-dirty-p return true."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (cl-cc/runtime:rt-card-mark-dirty heap old-base)
    (assert-true (cl-cc/runtime:rt-card-dirty-p heap old-base))))

(deftest heap-trace-card-clear-after-dirty
  "rt-card-clear makes rt-card-dirty-p return false after a mark."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (cl-cc/runtime:rt-card-mark-dirty heap old-base)
    (cl-cc/runtime:rt-card-clear heap old-base)
    (assert-false (cl-cc/runtime:rt-card-dirty-p heap old-base))))

(deftest heap-trace-card-clear-all
  "rt-card-clear-all clears all dirty cards."
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (cl-cc/runtime:rt-card-mark-dirty heap old-base)
    (assert-true  (cl-cc/runtime:rt-card-dirty-p heap old-base))
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

(deftest heap-trace-pointer-slots-cons
  "Cons objects (tag=1) have pointer slots (1 2) for car and cdr."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 3 1)   ; size=3 words (header+car+cdr), tag=1=cons
    (assert-equal '(1 2) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-symbol
  "Symbol objects (tag=2) have pointer slots (1 2 3) for name/pkg/plist."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 2)   ; size=4 words, tag=2=symbol
    (assert-equal '(1 2 3) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-closure-size4
  "Closure (tag=3) with size=4 has env slots (2 3); slot 1 is raw fn-index."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 3)   ; size=4, tag=3=closure
    (assert-equal '(2 3) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-closure-size2
  "Closure (tag=3) with size=2 has no env slots — empty env closure."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 2 3)   ; size=2 (header + fn-index only)
    (assert-equal '() (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-string
  "String objects (tag=6) have no pointer slots — packed character data."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 5 6)   ; size=5, tag=6=string
    (assert-equal nil (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-array-size4
  "Array (tag=5) with size=4 has element slots (2 3); slot 1 is rank metadata."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 5)   ; size=4, tag=5=array
    (assert-equal '(2 3) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-other-size3
  "Other heap objects (tag=7) treat all non-header slots as pointers: (1 2)."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 3 7)   ; size=3, tag=7=other
    (assert-equal '(1 2) (cl-cc/runtime:rt-object-pointer-slots heap 0))))

(deftest heap-trace-pointer-slots-unknown-tag
  "Unknown tags (e.g. 0=fixnum-immediate) return NIL — no pointer slots."
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 2 0)   ; tag=0=fixnum immediate
    (assert-equal nil (cl-cc/runtime:rt-object-pointer-slots heap 0))))
