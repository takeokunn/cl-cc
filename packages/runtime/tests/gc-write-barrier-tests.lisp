;;;; tests/unit/runtime/gc-write-barrier-tests.lisp — SATB + Card-Table Write Barrier Tests
;;;
;;; Tests for src/runtime/gc-write-barrier.lisp: rt-gc-write-barrier.
;;;
;;; Heap layout (young-size=32, old-size=32):
;;;   young-from-base=0, semi-size=16  → young addrs 0..15
;;;   old-base=32, old-size=32         → old addrs 32..63
;;;   card-size=64 words               → single card (card 0) covers all of old space
;;;
;;; rt-gc-write-barrier (heap obj-addr slot-offset new-val) semantics:
;;;   slot-addr = obj-addr + slot-offset
;;;   1. SATB: if gc-state=:major-gc AND header is marked AND old-value is a heap addr
;;;            → push old-value onto satb-queue BEFORE the write
;;;   2. Card:  if obj-addr is in old-space AND new-val is a young-space addr
;;;            → mark the card dirty
;;;   3. Always: write new-val to slot-addr

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------

(defsuite gc-write-barrier-suite
  :description "SATB pre-write barrier + card table write barrier tests"
  :parent cl-cc-unit-suite)

(in-suite gc-write-barrier-suite)

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun %make-wb-heap ()
  "Create a minimal heap for write-barrier tests.
   young-from-base=0 (addrs 0..15), old-base=32 (addrs 32..63)."
  (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))

(defun %wb-write-marked-header (heap addr size tag)
  "Write a marked (black) object header at ADDR."
  (let* ((h  (cl-cc/runtime:make-header size tag 0))
         (hm (cl-cc/runtime:header-set-mark h)))
    (cl-cc/runtime:rt-heap-set-header heap addr hm)))

(defun %wb-write-unmarked-header (heap addr size tag)
  "Write an unmarked object header at ADDR."
  (cl-cc/runtime:rt-heap-set-header
   heap addr
   (cl-cc/runtime:make-header size tag 0)))

;;; ------------------------------------------------------------
;;; Test: Write actually stores the value
;;; ------------------------------------------------------------

(deftest gc-write-barrier-performs-write
  "rt-gc-write-barrier stores new-val at (obj-addr + slot-offset)."
  (let* ((heap (%make-wb-heap))
         (obj  32)   ; old-space object
         (slot 1))
    (%wb-write-unmarked-header heap obj 3 7)
    (cl-cc/runtime:rt-gc-write-barrier heap obj slot 99)
    (assert-= 99 (cl-cc/runtime:rt-heap-ref heap (+ obj slot)))))

;;; ------------------------------------------------------------
;;; SATB Snapshot Tests
;;; ------------------------------------------------------------

(deftest gc-write-barrier-no-satb-during-normal-gc
  "During :normal state, SATB queue stays empty regardless of mark bit."
  (let* ((heap (%make-wb-heap)))
    (%wb-write-marked-header heap 32 3 7)
    (cl-cc/runtime:rt-heap-set heap 33 5)
    (cl-cc/runtime:rt-gc-write-barrier heap 32 1 99)
    (assert-null (cl-cc/runtime:rt-heap-satb-queue heap))))

(deftest gc-write-barrier-satb-snapshot-major-gc-black-object
  "During :major-gc, writing over a heap pointer in a black object snapshots the old value."
  (let* ((heap (%make-wb-heap))
         (obj  32)   ; old-space
         (slot 1))
    ;; Set up: marked object, gc-state = :major-gc, old slot value = young addr 5
    (%wb-write-marked-header heap obj 3 7)
    (cl-cc/runtime:rt-heap-set heap (+ obj slot) 5)  ; old-value = 5 (young addr)
    (setf (cl-cc/runtime:rt-heap-gc-state heap) :major-gc)
    ;; Overwrite with a different value
    (cl-cc/runtime:rt-gc-write-barrier heap obj slot 42)
    ;; Old value 5 should be in the SATB queue
    (assert-true (member 5 (cl-cc/runtime:rt-heap-satb-queue heap)))))

(deftest gc-write-barrier-no-satb-unmarked-object
  "During :major-gc, writing to an UNMARKED (white/gray) object does not snapshot."
  (let* ((heap (%make-wb-heap))
         (obj  32)
         (slot 1))
    (%wb-write-unmarked-header heap obj 3 7)
    (cl-cc/runtime:rt-heap-set heap (+ obj slot) 5)  ; old-value = young addr
    (setf (cl-cc/runtime:rt-heap-gc-state heap) :major-gc)
    (cl-cc/runtime:rt-gc-write-barrier heap obj slot 42)
    (assert-equal nil (cl-cc/runtime:rt-heap-satb-queue heap))))

(deftest gc-write-barrier-no-satb-old-value-not-heap-addr
  "During :major-gc, old value that is not a heap address is not snapshotted."
  (let* ((heap (%make-wb-heap))
         (obj  32)
         (slot 1))
    (%wb-write-marked-header heap obj 3 7)
    ;; Old value = 999 (outside heap: young 0..15, old 32..63)
    (cl-cc/runtime:rt-heap-set heap (+ obj slot) 999)
    (setf (cl-cc/runtime:rt-heap-gc-state heap) :major-gc)
    (cl-cc/runtime:rt-gc-write-barrier heap obj slot 42)
    (assert-equal nil (cl-cc/runtime:rt-heap-satb-queue heap))))

;;; ------------------------------------------------------------
;;; Card Table Tests
;;; ------------------------------------------------------------

(deftest-each gc-write-barrier-card-table-behavior
  ;; Card table truth table: old-obj × young-ptr → dirty.
  "rt-gc-write-barrier marks card dirty only when writing a young ptr into old-space."
  :cases (("old-to-young"   32 5
           (lambda (heap)
             (assert-true (cl-cc/runtime:rt-card-dirty-p heap 32)))) ; old obj + young ptr → dirty
          ("old-to-old"     32 40
           (lambda (heap)
             (assert-false (cl-cc/runtime:rt-card-dirty-p heap 32)))) ; old obj + old ptr → clean
          ("young-to-young"  0 5
           (lambda (heap)
             (assert-false (cl-cc/runtime:rt-card-dirty-p heap 32))))) ; young obj + young ptr → clean
  (obj new-val verify)
  (let* ((heap (%make-wb-heap)))
    (%wb-write-unmarked-header heap obj 3 7)
    (cl-cc/runtime:rt-gc-write-barrier heap obj 1 new-val)
    (funcall verify heap)))
