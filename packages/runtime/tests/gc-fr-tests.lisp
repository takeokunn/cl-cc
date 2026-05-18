;;;; tests/gc-fr-tests.lisp - GC Feature Requirement Evidence Tests
;;;;
;;;; Tests for FR implementations in the cl-cc/runtime generational GC:
;;;; - FR-331: Old-Space Free-List Reuse
;;;; - FR-333: Nursery Sizing Heuristics
;;;; - FR-335: Write Barrier Young-to-Young Elision
;;;; - FR-336: GC-NaN-Boxing Integration
;;;; - FR-341: GC Pause Time Goals / SLO

(in-package :cl-cc/test)

(defsuite gc-fr-suite
  :description "GC Feature Requirement evidence tests (FR-331, FR-333, FR-335, FR-336, FR-341)"
  :parent cl-cc-unit-suite)

(in-suite gc-fr-suite)

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun %make-small-heap-fr ()
  "Create a minimal heap: 64-word young space (32 per semi), 128-word old space."
  (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 128))

(defun %fr-write-object (heap addr size tag &optional (age 0))
  "Write a header at ADDR and return the address."
  (cl-cc/runtime:rt-heap-set-header
   heap addr
   (cl-cc/runtime:make-header size tag age))
  addr)

;;; ------------------------------------------------------------
;;; FR-331: Old-Space Free-List Allocation Reuse
;;; ------------------------------------------------------------

(deftest fr-331-free-list-exists
  "FR-331: The heap has segregated free-list bins (16 size classes)."
  (let ((heap (%make-small-heap-fr)))
    (let ((bins (cl-cc/runtime::rt-heap-free-bins heap)))
      (assert-true (vectorp bins))
      (assert-= 16 (length bins)))))

(deftest fr-331-free-list-insert-find-and-split
  "FR-331: Free-list insert and best-fit find reuse a block and retain the remainder."
  (let ((heap (%make-small-heap-fr)))
    (let ((insert-bin (cl-cc/runtime::rt-free-list-insert heap 8 1000)))
      (assert-true (integerp insert-bin))
      (multiple-value-bind (bin addr) (cl-cc/runtime::rt-free-list-find heap 4)
        (assert-= insert-bin bin)
        (assert-= 1000 addr)
        (let ((blocks (cl-cc/runtime::rt-heap-free-list-blocks heap)))
          (assert-true (member (cons 4 1004) blocks :test #'equal)))))))

(deftest fr-331-free-list-blocks-enumeration
  "FR-331: Free-list blocks can be enumerated across segregated bins."
  (let ((heap (%make-small-heap-fr)))
    (cl-cc/runtime::rt-free-list-insert heap 8 1000)
    (cl-cc/runtime::rt-free-list-insert heap 16 2000)
    (let ((blocks (cl-cc/runtime::rt-heap-free-list-blocks heap)))
      (assert-true (listp blocks))
      (assert-true (member (cons 8 1000) blocks :test #'equal))
      (assert-true (member (cons 16 2000) blocks :test #'equal)))))

(deftest fr-331-free-list-bin-index
  "FR-331: Size classes map to monotonic power-of-2 bucket indices."
  (let ((bin-3  (cl-cc/runtime::rt-free-list-bin-index 3))
        (bin-4  (cl-cc/runtime::rt-free-list-bin-index 4))
        (bin-8  (cl-cc/runtime::rt-free-list-bin-index 8))
        (bin-32 (cl-cc/runtime::rt-free-list-bin-index 32))
        (bin-64 (cl-cc/runtime::rt-free-list-bin-index 64)))
    (assert-= bin-3 bin-4)
    (assert-true (>= bin-32 bin-8))
    (assert-true (>= bin-64 bin-32))))

;;; ------------------------------------------------------------
;;; FR-333: Nursery Sizing Heuristics
;;; ------------------------------------------------------------

(deftest fr-333-high-promotion-grows-default-nursery
  "FR-333: High promotion ratio grows the default nursery for subsequently created heaps."
  (let ((cl-cc/runtime:*gc-young-size-words* 65536)
        (cl-cc/runtime::*rt-minor-gc-window-start* nil)
        (cl-cc/runtime::*rt-low-promotion-cycles* 0))
    (cl-cc/runtime::%rt-gc-tune-nursery 0.9d0)
    (assert-= 131072 cl-cc/runtime:*gc-young-size-words*)))

(deftest fr-333-low-promotion-shrinks-default-nursery-after-stable-cycles
  "FR-333: Sustained low promotion ratio shrinks the default nursery floor-aware."
  (let ((cl-cc/runtime:*gc-young-size-words* 65536)
        (cl-cc/runtime::*rt-minor-gc-window-start*
          (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
        (cl-cc/runtime::*rt-low-promotion-cycles* 0))
    (cl-cc/runtime::%rt-gc-tune-nursery 0.01d0)
    (assert-= 65536 cl-cc/runtime:*gc-young-size-words*)
    (cl-cc/runtime::%rt-gc-tune-nursery 0.01d0)
    (assert-= 65536 cl-cc/runtime:*gc-young-size-words*)
    (cl-cc/runtime::%rt-gc-tune-nursery 0.01d0)
    (assert-= 32768 cl-cc/runtime:*gc-young-size-words*)))

;;; ------------------------------------------------------------
;;; FR-335: Write Barrier Young-to-Young Elision
;;; ------------------------------------------------------------

(deftest fr-335-write-barrier-young-to-young-fast-path
  "FR-335: Young-to-young stores update the slot without dirtying cards or SATB queues."
  (let ((heap (%make-small-heap-fr)))
    (let ((obj-addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
          (target   (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%fr-write-object heap obj-addr 3 cl-cc/runtime:+rt-tag-cons+)
      (%fr-write-object heap target 3 cl-cc/runtime:+rt-tag-cons+)
      (setf (cl-cc/runtime::rt-heap-gc-state heap) :major-gc)
      (cl-cc/runtime:rt-gc-write-barrier heap obj-addr 1 target)
      (assert-= target (cl-cc/runtime:rt-heap-ref heap (+ obj-addr 1)))
      (assert-false (find-if-not #'zerop (cl-cc/runtime:rt-heap-card-table heap)))
      (assert-= 0 (length (cl-cc/runtime::rt-heap-satb-queue heap))))))

(deftest fr-335-barrier-buffer-flush
  "FR-335: Barrier buffer flush marks queued old cards and clears the buffer."
  (let ((heap (%make-small-heap-fr)))
    (let ((old-addr (cl-cc/runtime:rt-heap-old-base heap)))
      (push old-addr (cl-cc/runtime:rt-heap-barrier-buffer heap))
      (assert-false (cl-cc/runtime:rt-card-dirty-p heap old-addr))
      (cl-cc/runtime:rt-gc-flush-barrier-buffer heap)
      (assert-true (cl-cc/runtime:rt-card-dirty-p heap old-addr))
      (assert-= 0 (length (cl-cc/runtime:rt-heap-barrier-buffer heap))))))

;;; ------------------------------------------------------------
;;; FR-336: GC-NaN-Boxing Integration
;;; ------------------------------------------------------------

(deftest fr-336-val-pointer-p-detection
  "FR-336: NaN-boxing pointer predicate rejects immediate non-pointer values."
  (assert-false (cl-cc/runtime:val-pointer-p
                 (cl-cc/runtime:encode-fixnum 42)))
  (assert-false (cl-cc/runtime:val-pointer-p cl-cc/runtime:+val-t+))
  (assert-false (cl-cc/runtime:val-pointer-p cl-cc/runtime:+val-nil+)))

(deftest fr-336-decode-pointer-roundtrip
  "FR-336: Encoded pointers round-trip through decode-pointer."
  (let* ((addr #xABCD)
         (encoded (cl-cc/runtime:encode-pointer addr cl-cc/runtime:+tag-object+)))
    (assert-true (cl-cc/runtime:val-pointer-p encoded))
    (assert-= addr (cl-cc/runtime:decode-pointer encoded))))

(deftest fr-336-pointer-tag-extraction
  "FR-336: Pointer sub-tags are correctly extracted from NaN-boxed values."
  (let ((obj-ptr (cl-cc/runtime:encode-pointer #x100 cl-cc/runtime:+tag-object+))
        (cons-ptr (cl-cc/runtime:encode-pointer #x200 cl-cc/runtime:+tag-cons+))
        (sym-ptr (cl-cc/runtime:encode-pointer #x300 cl-cc/runtime:+tag-symbol+)))
    (assert-= cl-cc/runtime:+tag-object+ (cl-cc/runtime:pointer-tag obj-ptr))
    (assert-= cl-cc/runtime:+tag-cons+ (cl-cc/runtime:pointer-tag cons-ptr))
    (assert-= cl-cc/runtime:+tag-symbol+ (cl-cc/runtime:pointer-tag sym-ptr))))

(deftest fr-336-val-cons-p-detection
  "FR-336: NaN-boxing cons predicate detects cons-tagged pointers only."
  (let ((cons-ptr (cl-cc/runtime:encode-pointer #x500 cl-cc/runtime:+tag-cons+))
        (obj-ptr (cl-cc/runtime:encode-pointer #x600 cl-cc/runtime:+tag-object+)))
    (assert-true (cl-cc/runtime:val-cons-p cons-ptr))
    (assert-false (cl-cc/runtime:val-cons-p obj-ptr))))

;;; ------------------------------------------------------------
;;; FR-341: GC Pause Time Goals / SLO
;;; ------------------------------------------------------------

(deftest fr-341-pause-max-ms-configured
  "FR-341: Max pause time parameter is configured and accessible."
  (assert-true (numberp cl-cc/runtime::*gc-max-pause-ms*))
  (assert-true (>= cl-cc/runtime::*gc-max-pause-ms* 1)))

(deftest fr-341-pause-accounting-increments-on-budget-exceed
  "FR-341: GC pause accounting increments exceeded-count when the SLO budget is exceeded."
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime::*gc-max-pause-ms* 0))
    (let ((exceeded-before (cl-cc/runtime::rt-heap-pause-exceeded-count heap))
          (budget-before (cl-cc/runtime::rt-heap-incremental-work-budget heap)))
      (cl-cc/runtime::%rt-gc-note-pause
       heap
       (- (get-internal-real-time) internal-time-units-per-second))
      (assert-= (1+ exceeded-before)
                (cl-cc/runtime::rt-heap-pause-exceeded-count heap))
      (assert-true (< (cl-cc/runtime::rt-heap-incremental-work-budget heap)
                      budget-before)))))

(deftest fr-341-throughput-target
  "FR-341: Throughput target parameter is configured in the valid ratio range."
  (assert-true (numberp cl-cc/runtime::*gc-throughput-target*))
  (assert-true (>= cl-cc/runtime::*gc-throughput-target* 0.0d0))
  (assert-true (<= cl-cc/runtime::*gc-throughput-target* 1.0d0)))

;;; ------------------------------------------------------------
;;; Integrated FR Tests
;;; ------------------------------------------------------------

(deftest fr-integrated-alloc-and-gc
  "FR-331+333+336: Allocate objects, trigger minor GC, verify heap integrity."
  (let ((heap (%make-small-heap-fr)))
    (loop repeat 10
          for i from 1
          do (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
               (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
               (cl-cc/runtime:rt-heap-set heap (+ addr 1)
                                          (cl-cc/runtime:encode-fixnum i))
               (cl-cc/runtime:rt-heap-set heap (+ addr 2)
                                          (cl-cc/runtime:encode-fixnum (+ i 100)))
               (assert-true (integerp addr))
                (assert-true (>= addr 0))))
    (cl-cc/runtime:rt-gc-minor-collect heap)
    (assert-true (cl-cc/runtime:rt-gc-verify-heap heap))))

;;; ------------------------------------------------------------
;;; FR-373: Heap ASLR (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(deftest fr-373-heap-randomize-function-exists
  "FR-373: rt-heap-randomize-base is fbound (Pure CL fallback returns logical offset)."
  (assert-true (fboundp 'cl-cc/runtime:rt-heap-randomize-base)))

(deftest fr-373-heap-randomize-returns-value
  "FR-373: rt-heap-randomize-base returns a non-negative integer offset."
  (let ((offset (cl-cc/runtime:rt-heap-randomize-base)))
    (assert-true (integerp offset))
    (assert-true (>= offset 0))))

;;; ------------------------------------------------------------
;;; FR-376: Guard Pages for Stack Overflow (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(deftest fr-376-stack-guard-function-exists
  "FR-376: rt-install-stack-guard is fbound (Pure CL fallback returns plist)."
  (assert-true (fboundp 'cl-cc/runtime:rt-install-stack-guard)))

(deftest fr-376-stack-guard-returns-plist
  "FR-376: rt-install-stack-guard returns a property list describing the guard region."
  (let ((result (cl-cc/runtime:rt-install-stack-guard 0 4096)))
    (assert-true (listp result))
    (assert-true (getf result :portable-fallback))))

;;; ------------------------------------------------------------
;;; FR-377: Immortal / Permanent Objects (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(deftest fr-377-immortal-functions-exist
  "FR-377: rt-make-immortal and rt-immortal-p are fbound."
  (assert-true (fboundp 'cl-cc/runtime:rt-make-immortal))
  (assert-true (fboundp 'cl-cc/runtime:rt-immortal-p)))

(deftest fr-377-make-immortal-roundtrip
  "FR-377: Creating an immortal object returns an immortal address."
  (let* ((handle (cl-cc/runtime:rt-make-immortal cl-cc/runtime:+rt-tag-cons+ 1)))
    (assert-true handle)
    (assert-true (cl-cc/runtime:rt-immortal-p handle))
    (assert-true t)))

(deftest fr-377-immortal-count-increases
  "FR-377: Creating immortal objects increments the count."
  (let ((before (hash-table-count cl-cc/runtime::*rt-immortal-registry*)))
    (cl-cc/runtime:rt-make-immortal cl-cc/runtime:+rt-tag-cons+ 1)
    (let ((after (hash-table-count cl-cc/runtime::*rt-immortal-registry*)))
      (assert-true (> after before)))))

;;; ------------------------------------------------------------
;;; FR-371: GC Safepoints (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(deftest fr-371-gc-safepoint-interface-exists
  "FR-371: GC safepoint interface is fbound (Pure CL cooperative safe-region API)."
  (assert-true (or (multiple-value-bind (symbol status)
                       (find-symbol "RT-GC-REQUEST-STOP" "CL-CC/RUNTIME")
                     (and status (fboundp symbol)))
                   (and (find-symbol "RT-GC-ENTER-SAFE-REGION" "CL-CC/RUNTIME")
                        (multiple-value-bind (symbol status)
                            (find-symbol "RT-GC-ENTER-SAFE-REGION" "CL-CC/RUNTIME")
                          (and status (fboundp symbol)))))))
