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
  :parent cl-cc-unit-suite)

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

(deftest-each gc-header-field-roundtrip
  "make-header round-trips size, tag, and age fields independently."
  :cases (("size" (cl-cc/runtime:make-header 7 1 0) #'cl-cc/runtime:header-size 7)
          ("tag"  (cl-cc/runtime:make-header 3 5 0) #'cl-cc/runtime:header-tag  5)
          ("age"  (cl-cc/runtime:make-header 3 1 9) #'cl-cc/runtime:header-age  9))
  (h accessor expected)
  (assert-= expected (funcall accessor h)))

(deftest gc-header-bit-toggles
  "mark and gray bits: set makes true; clear makes false; fresh header is false."
  (let ((h (cl-cc/runtime:make-header 3 1 0)))
    ;; mark bit
    (let* ((hm (cl-cc/runtime:header-set-mark h))
           (hu (cl-cc/runtime:header-clear-mark hm)))
      (assert-true  (cl-cc/runtime:header-marked-p hm))
      (assert-false (cl-cc/runtime:header-marked-p h))
      (assert-false (cl-cc/runtime:header-marked-p hu)))
    ;; gray bit
    (let* ((hg (cl-cc/runtime:header-set-gray h))
           (hu (cl-cc/runtime:header-clear-gray hg)))
      (assert-true  (cl-cc/runtime:header-gray-p hg))
      (assert-false (cl-cc/runtime:header-gray-p h))
      (assert-false (cl-cc/runtime:header-gray-p hu)))))

(deftest gc-header-forwarding-integer-is-not-forwarding
  "An integer header is not a forwarding pointer."
  (let ((h (cl-cc/runtime:make-header 3 1 0)))
    (assert-false (cl-cc/runtime:header-forwarding-p h))))

(deftest gc-header-forwarding-ptr-roundtrip
  "header-make-forwarding-ptr / header-forwarding-p / header-forwarding-ptr roundtrip."
  (let ((fwd (cl-cc/runtime:header-make-forwarding-ptr 42)))
    (assert-true (cl-cc/runtime:header-forwarding-p fwd))
    (assert-= 42 (cl-cc/runtime:header-forwarding-ptr fwd))))

(deftest gc-header-increment-age
  "header-increment-age increments by 1; caps at 15."
  ;; basic increment
  (let* ((h  (cl-cc/runtime:make-header 3 1 2))
         (h2 (cl-cc/runtime:header-increment-age h)))
    (assert-= 3 (cl-cc/runtime:header-age h2)))
  ;; cap at 15
  (let* ((h  (cl-cc/runtime:make-header 3 1 15))
         (h2 (cl-cc/runtime:header-increment-age h)))
    (assert-= 15 (cl-cc/runtime:header-age h2))))

;;; ------------------------------------------------------------
;;; Test 2: gc-heap-creation
;;; ------------------------------------------------------------

(deftest-each gc-heap-creation-layout
  "Heap creation: young-from-base=0, young-to-base=semi-size, old-base=2*semi-size, gc-state=:normal."
  :cases (("32-word-young"
           (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32) 0 16 32)
          ("16-word-young"
           (cl-cc/runtime:make-rt-heap :young-size 16 :old-size 16) 0  8 16))
  (heap expected-from expected-to expected-old)
  (assert-= expected-from (cl-cc/runtime:rt-heap-young-from-base heap))
  (assert-= expected-to   (cl-cc/runtime:rt-heap-young-to-base heap))
  (assert-= expected-old  (cl-cc/runtime:rt-heap-old-base heap)))

;;; ------------------------------------------------------------
;;; Test 3: gc-alloc-basic
;;; ------------------------------------------------------------

(deftest gc-alloc-sequential-addresses-and-free-pointer
  "First alloc returns 0; young-free advances to 3; second alloc starts at 3."
  (let* ((heap (%make-small-heap))
         (addr1 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
    (assert-= 0 addr1)
    (assert-= 3 (cl-cc/runtime:rt-heap-young-free heap))
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

(deftest gc-minor-gc-collects-and-updates-root
  "After minor GC: unreachable object's words are counted as collected; root's cdr is updated to the live object's new young-space address."
  (let* ((heap (%make-small-heap)))
    (let ((addr1 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
          (addr2 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr1 3 cl-cc/runtime:+rt-tag-cons+)
      (%write-header heap addr2 3 cl-cc/runtime:+rt-tag-cons+)
      ;; Only register addr1 as a root; addr2 is unreachable
      (let ((root (cons nil addr1)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (assert-= 1 (cl-cc/runtime:rt-heap-minor-gc-count heap))
        (assert-true (>= (cl-cc/runtime:rt-heap-words-collected heap) 3))
        (assert-true (cl-cc/runtime:rt-young-addr-p heap (cdr root)))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

;;; ------------------------------------------------------------
;;; Test 5: gc-minor-gc-preserves-live-objects
;;; ------------------------------------------------------------

(deftest gc-minor-gc-preserves-live-object-data
  "After minor GC, a live object's slot values and header tag are preserved."
  (let* ((heap (%make-small-heap)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      ;; Write slot values (non-pointer integers, safe for this test)
      (cl-cc/runtime:rt-heap-set heap (+ addr 1) 111)
      (cl-cc/runtime:rt-heap-set heap (+ addr 2) 222)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (let ((new-addr (cdr root)))
          (assert-= 111 (cl-cc/runtime:rt-heap-ref heap (+ new-addr 1)))
          (assert-= 222 (cl-cc/runtime:rt-heap-ref heap (+ new-addr 2))))
        (cl-cc/runtime:rt-gc-remove-root heap root))))
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

(deftest gc-write-barrier-card-dirty-behavior
  "rt-gc-write-barrier marks the card dirty for old->young writes; leaves it clean for old->old writes."
  ;; old-space object receives young pointer: card must become dirty
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (let ((young-addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap young-addr 3 cl-cc/runtime:+rt-tag-cons+)
      (let* ((old-addr (cl-cc/runtime:rt-heap-old-base heap)))
        (%write-header heap old-addr 3 cl-cc/runtime:+tag-other+)
        (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-addr 3))
        (assert-false (cl-cc/runtime:rt-card-dirty-p heap old-addr))
        (cl-cc/runtime:rt-gc-write-barrier heap old-addr 1 young-addr)
        (assert-true (cl-cc/runtime:rt-card-dirty-p heap old-addr))
        (assert-= young-addr (cl-cc/runtime:rt-heap-ref heap (+ old-addr 1))))))
  ;; old-space object receives old-space pointer: card must remain clean
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (let* ((old-base (cl-cc/runtime:rt-heap-old-base heap))
           (obj1     old-base)
           (obj2     (+ old-base 3)))
      (%write-header heap obj1 3 cl-cc/runtime:+tag-other+)
      (%write-header heap obj2 3 cl-cc/runtime:+tag-other+)
      (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 6))
      (cl-cc/runtime:rt-gc-write-barrier heap obj1 1 obj2)
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

(deftest gc-stats-totals-match-heap-structure
  ":young-total matches semi-size; :old-total matches old-size slot."
  (let* ((heap  (%make-small-heap))
         (stats (cl-cc/runtime:rt-gc-stats heap)))
    (assert-= (cl-cc/runtime:rt-heap-young-semi-size heap) (getf stats :young-total))
    (assert-= (cl-cc/runtime:rt-heap-old-size heap)        (getf stats :old-total))))

(deftest gc-stats-young-used-after-alloc
  ":young-used reflects allocated words before GC."
  (let* ((heap (%make-small-heap)))
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (assert-= 6 (getf (cl-cc/runtime:rt-gc-stats heap) :young-used))))

