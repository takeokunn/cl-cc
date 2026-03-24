;;;; tests/gc-tests.lisp - Generational GC Tests
;;;
;;; Tests for the cl-cc/runtime generational GC:
;;; - Object header encoding/decoding
;;; - Heap creation and layout
;;; - Bump-pointer allocation
;;; - Minor GC: garbage collection, live-object preservation, promotion
;;; - Write barrier and card table
;;; - GC statistics

(in-package :cl-cc/test)

;;; Import GC symbols from the runtime package.
;;; We alias them locally via a helper rather than polluting the test package
;;; — all calls below are fully qualified as cl-cc/runtime:*.

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------

(defsuite gc-suite
  :description "Generational GC tests (heap, header, alloc, minor GC, write barrier, stats)"
  :parent cl-cc-suite)

(in-suite gc-suite)

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun %make-small-heap ()
  "Create a minimal heap: 32-word young space (16 per semi), 32-word old space."
  (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))

(defun %write-header (heap addr size tag &optional (age 0))
  "Write a freshly made header at ADDR."
  (cl-cc/runtime:rt-heap-set-header
   heap addr
   (cl-cc/runtime:make-header size tag age)))

;;; ------------------------------------------------------------
;;; Test 1: gc-header-basics
;;; ------------------------------------------------------------

(deftest gc-header-size-roundtrip
  "make-header / header-size round-trip."
  (let ((h (cl-cc/runtime:make-header 7 1 0)))
    (assert-= 7 (cl-cc/runtime:header-size h))))

(deftest gc-header-tag-roundtrip
  "make-header / header-tag round-trip."
  (let ((h (cl-cc/runtime:make-header 3 5 0)))
    (assert-= 5 (cl-cc/runtime:header-tag h))))

(deftest gc-header-age-roundtrip
  "make-header / header-age round-trip."
  (let ((h (cl-cc/runtime:make-header 3 1 9)))
    (assert-= 9 (cl-cc/runtime:header-age h))))

(deftest gc-header-mark-bit-toggle
  "header-set-mark / header-clear-mark toggle the mark bit."
  (let* ((h  (cl-cc/runtime:make-header 3 1 0))
         (hm (cl-cc/runtime:header-set-mark h))
         (hu (cl-cc/runtime:header-clear-mark hm)))
    (assert-true  (cl-cc/runtime:header-marked-p hm))
    (assert-false (cl-cc/runtime:header-marked-p h))
    (assert-false (cl-cc/runtime:header-marked-p hu))))

(deftest gc-header-gray-bit-toggle
  "header-set-gray / header-clear-gray toggle the gray bit."
  (let* ((h  (cl-cc/runtime:make-header 3 1 0))
         (hg (cl-cc/runtime:header-set-gray h))
         (hu (cl-cc/runtime:header-clear-gray hg)))
    (assert-true  (cl-cc/runtime:header-gray-p hg))
    (assert-false (cl-cc/runtime:header-gray-p h))
    (assert-false (cl-cc/runtime:header-gray-p hu))))

(deftest gc-header-forwarding-integer-is-not-forwarding
  "An integer header is not a forwarding pointer."
  (let ((h (cl-cc/runtime:make-header 3 1 0)))
    (assert-false (cl-cc/runtime:header-forwarding-p h))))

(deftest gc-header-forwarding-ptr-roundtrip
  "header-make-forwarding-ptr / header-forwarding-p / header-forwarding-ptr roundtrip."
  (let ((fwd (cl-cc/runtime:header-make-forwarding-ptr 42)))
    (assert-true (cl-cc/runtime:header-forwarding-p fwd))
    (assert-= 42 (cl-cc/runtime:header-forwarding-ptr fwd))))

(deftest gc-header-increment-age-basic
  "header-increment-age increments age by 1."
  (let* ((h  (cl-cc/runtime:make-header 3 1 2))
         (h2 (cl-cc/runtime:header-increment-age h)))
    (assert-= 3 (cl-cc/runtime:header-age h2))))

(deftest gc-header-increment-age-cap
  "header-increment-age caps at 15."
  (let* ((h   (cl-cc/runtime:make-header 3 1 15))
         (h2  (cl-cc/runtime:header-increment-age h)))
    (assert-= 15 (cl-cc/runtime:header-age h2))))

;;; ------------------------------------------------------------
;;; Test 2: gc-heap-creation
;;; ------------------------------------------------------------

(deftest gc-heap-creation-young-from-base
  "young-from-base starts at 0."
  (let ((heap (%make-small-heap)))
    (assert-= 0 (cl-cc/runtime:rt-heap-young-from-base heap))))

(deftest gc-heap-creation-young-to-base
  "young-to-base is at semi-size (half of young-size)."
  (let ((heap (%make-small-heap)))
    ;; young-size=32 -> semi-size=16; to-base = 16
    (assert-= 16 (cl-cc/runtime:rt-heap-young-to-base heap))))

(deftest gc-heap-creation-old-base
  "old-base is at 2 * semi-size."
  (let ((heap (%make-small-heap)))
    ;; 2 * 16 = 32
    (assert-= 32 (cl-cc/runtime:rt-heap-old-base heap))))

(deftest gc-heap-creation-gc-state
  "gc-state is :normal after creation."
  (let ((heap (%make-small-heap)))
    (assert-eq :normal (cl-cc/runtime:rt-heap-gc-state heap))))

(deftest gc-heap-creation-small-heap-ok
  "A small heap (16-word young, 16-word old) can be created without error."
  (let ((heap (cl-cc/runtime:make-rt-heap :young-size 16 :old-size 16)))
    (assert-= 0 (cl-cc/runtime:rt-heap-young-from-base heap))
    (assert-= 8 (cl-cc/runtime:rt-heap-young-to-base heap))
    (assert-= 16 (cl-cc/runtime:rt-heap-old-base heap))))

;;; ------------------------------------------------------------
;;; Test 3: gc-alloc-basic
;;; ------------------------------------------------------------

(deftest gc-alloc-first-object-address
  "First allocation returns young-from-base (0)."
  (let* ((heap (%make-small-heap))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
    (assert-= 0 addr)))

(deftest gc-alloc-advances-free-pointer
  "After allocating 3 words, young-free advances to 3."
  (let* ((heap (%make-small-heap)))
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (assert-= 3 (cl-cc/runtime:rt-heap-young-free heap))))

(deftest gc-alloc-second-object-address
  "Second allocation starts at the end of the first object."
  (let* ((heap (%make-small-heap)))
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (let ((addr2 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (assert-= 3 addr2))))

(deftest gc-alloc-header-size-readable
  "After writing a header, rt-heap-object-size returns the correct size."
  (let* ((heap (%make-small-heap))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
    (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (assert-= 3 (cl-cc/runtime:rt-heap-object-size heap addr))))

;;; ------------------------------------------------------------
;;; Test 4: gc-minor-gc-collects-garbage
;;; ------------------------------------------------------------

(deftest gc-minor-gc-collects-unreachable-object
  "After minor GC, the unreachable object's words are counted as collected."
  (let* ((heap (%make-small-heap)))
    ;; Allocate two 3-word objects
    (let ((addr1 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
          (addr2 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr1 3 cl-cc/runtime:+rt-tag-cons+)
      (%write-header heap addr2 3 cl-cc/runtime:+rt-tag-cons+)
      ;; Only register addr1 as a root
      (let ((root (cons nil addr1)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        ;; minor-gc-count must be 1
        (assert-= 1 (cl-cc/runtime:rt-heap-minor-gc-count heap))
        ;; words-collected must be >= 3 (the dead object's 3 words)
        (assert-true (>= (cl-cc/runtime:rt-heap-words-collected heap) 3))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

(deftest gc-minor-gc-root-address-updated
  "After minor GC, root cell's cdr is updated to the live object's new address."
  (let* ((heap (%make-small-heap)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        ;; The live object must be accessible in the new space
        (let ((new-addr (cdr root)))
          (assert-true (cl-cc/runtime:rt-young-addr-p heap new-addr)))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

;;; ------------------------------------------------------------
;;; Test 5: gc-minor-gc-preserves-live-objects
;;; ------------------------------------------------------------

(deftest gc-minor-gc-preserves-slot-data
  "After minor GC, a live object's slot values are preserved."
  (let* ((heap (%make-small-heap)))
    ;; Allocate a 3-word cons-like object: header + 2 data words
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      ;; Write slot values (non-pointer integers, safe for this test)
      (cl-cc/runtime:rt-heap-set heap (+ addr 1) 111)
      (cl-cc/runtime:rt-heap-set heap (+ addr 2) 222)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        ;; Read slots from new address
        (let ((new-addr (cdr root)))
          (assert-= 111 (cl-cc/runtime:rt-heap-ref heap (+ new-addr 1)))
          (assert-= 222 (cl-cc/runtime:rt-heap-ref heap (+ new-addr 2))))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

(deftest gc-minor-gc-header-tag-preserved
  "After minor GC, the live object's tag is preserved in its header."
  (let* ((heap (%make-small-heap)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-string+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-string+)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (let* ((new-addr (cdr root))
               (new-hdr  (cl-cc/runtime:rt-heap-object-header heap new-addr)))
          (assert-= cl-cc/runtime:+rt-tag-string+ (cl-cc/runtime:header-tag new-hdr)))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

;;; ------------------------------------------------------------
;;; Test 6: gc-promotion-threshold
;;; ------------------------------------------------------------

(deftest gc-promotion-promotes-old-object
  "An object that survives enough minor GCs is promoted to old space."
  ;; Use a larger heap to fit the object after repeated copies.
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 64)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      ;; Write header with age already at the tenuring threshold so the
      ;; very next minor GC will promote it.
      (cl-cc/runtime:rt-heap-set-header
       heap addr
       (cl-cc/runtime:make-header 3 cl-cc/runtime:+rt-tag-cons+
                                  cl-cc/runtime:*gc-tenuring-threshold*))
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        ;; words-promoted must be > 0
        (assert-true (> (cl-cc/runtime:rt-heap-words-promoted heap) 0))
        ;; The object must now live in old space
        (assert-true (cl-cc/runtime:rt-old-addr-p heap (cdr root)))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

;;; ------------------------------------------------------------
;;; Test 7: gc-write-barrier-card-dirty
;;; ------------------------------------------------------------

(deftest gc-write-barrier-marks-card-dirty
  "rt-gc-write-barrier marks the old-space card dirty when writing a young pointer."
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    ;; Allocate an object in young space (will serve as the 'new-val' pointer)
    (let ((young-addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap young-addr 3 cl-cc/runtime:+rt-tag-cons+)
      ;; Manually place an object in old space by bumping old-free
      (let* ((old-addr (cl-cc/runtime:rt-heap-old-base heap)))
        ;; Write a valid header so the object is recognisable
        (%write-header heap old-addr 3 cl-cc/runtime:+tag-other+)
        (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-addr 3))
        ;; Confirm card is not dirty before the barrier
        (assert-false (cl-cc/runtime:rt-card-dirty-p heap old-addr))
        ;; Fire the write barrier: old-space object at slot 1 receives young pointer
        (cl-cc/runtime:rt-gc-write-barrier heap old-addr 1 young-addr)
        ;; Card must be dirty now
        (assert-true (cl-cc/runtime:rt-card-dirty-p heap old-addr))
        ;; Slot must contain the young address
        (assert-= young-addr (cl-cc/runtime:rt-heap-ref heap (+ old-addr 1)))))))

(deftest gc-write-barrier-does-not-dirty-card-for-old-to-old
  "rt-gc-write-barrier does NOT dirty a card when writing an old-space pointer into old space."
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (let* ((old-base (cl-cc/runtime:rt-heap-old-base heap))
           (obj1     old-base)
           (obj2     (+ old-base 3)))
      ;; Set up two old-space objects
      (%write-header heap obj1 3 cl-cc/runtime:+tag-other+)
      (%write-header heap obj2 3 cl-cc/runtime:+tag-other+)
      (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 6))
      ;; Write barrier: obj1 slot 1 -> obj2 (both old space)
      (cl-cc/runtime:rt-gc-write-barrier heap obj1 1 obj2)
      ;; Card must remain clean
      (assert-false (cl-cc/runtime:rt-card-dirty-p heap obj1)))))

;;; ------------------------------------------------------------
;;; Test 8: gc-stats
;;; ------------------------------------------------------------

(deftest gc-stats-returns-plist
  "rt-gc-stats returns a plist with all required keys."
  (let* ((heap (%make-small-heap))
         (stats (cl-cc/runtime:rt-gc-stats heap)))
    (assert-true (getf stats :minor-gc-count))
    (assert-true (listp stats))
    (assert-true (member :minor-gc-count  stats))
    (assert-true (member :major-gc-count  stats))
    (assert-true (member :words-collected stats))
    (assert-true (member :words-promoted  stats))
    (assert-true (member :young-used      stats))
    (assert-true (member :young-total     stats))
    (assert-true (member :old-used        stats))
    (assert-true (member :old-total       stats))
    (assert-true (member :free-list-count stats))))

(deftest gc-stats-minor-gc-count-increments
  "After one minor GC, :minor-gc-count is 1."
  (let* ((heap (%make-small-heap)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (assert-= 1 (getf (cl-cc/runtime:rt-gc-stats heap) :minor-gc-count))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

(deftest gc-stats-young-total-matches-semi-size
  ":young-total matches the semi-space size from heap structure."
  (let* ((heap (%make-small-heap))
         (stats (cl-cc/runtime:rt-gc-stats heap)))
    (assert-= (cl-cc/runtime:rt-heap-young-semi-size heap)
              (getf stats :young-total))))

(deftest gc-stats-old-total-matches-old-size
  ":old-total matches the old-size slot."
  (let* ((heap (%make-small-heap))
         (stats (cl-cc/runtime:rt-gc-stats heap)))
    (assert-= (cl-cc/runtime:rt-heap-old-size heap)
              (getf stats :old-total))))

(deftest gc-stats-young-used-after-alloc
  ":young-used reflects allocated words before GC."
  (let* ((heap (%make-small-heap)))
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (assert-= 6 (getf (cl-cc/runtime:rt-gc-stats heap) :young-used))))
