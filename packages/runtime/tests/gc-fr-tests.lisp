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

(deftest fr-156-size-class-segregated-allocator-evidence
  "FR-156: Segregated free-list allocation reuses the best fitting bin and splits remainders."
  (let ((heap (%make-small-heap-fr)))
    (cl-cc/runtime::rt-free-list-insert heap 16 100)
    (cl-cc/runtime::rt-free-list-insert heap 64 200)
    (multiple-value-bind (bin addr) (cl-cc/runtime::rt-free-list-find heap 12)
      (assert-= (cl-cc/runtime::rt-free-list-bin-index 16) bin)
      (assert-= 100 addr)
      (assert-true (member (cons 4 112)
                           (cl-cc/runtime::rt-heap-free-list-blocks heap)
                           :test #'equal)))))

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

(deftest fr-140-symbol-immediates-roundtrip-common-symbols
  "FR-140: NIL, T, and common keywords/symbols are immediate values, not heap pointers."
  (assert-= cl-cc/runtime:+val-nil+ (cl-cc/runtime:cl-value->val nil))
  (assert-= cl-cc/runtime:+val-t+ (cl-cc/runtime:cl-value->val t))
  (let ((encoded (cl-cc/runtime:cl-value->val :key)))
    (assert-true (cl-cc/runtime::val-immediate-symbol-p encoded))
    (assert-false (cl-cc/runtime:val-pointer-p encoded))
    (assert-eq :key (cl-cc/runtime:val->cl-value encoded))))

(deftest fr-264-compressed-pointer-roundtrip
  "FR-264: Pointer compression stores heap-relative offsets and decodes to the original address."
  (let ((cl-cc/runtime:*compressed-pointers-enabled* t)
        (cl-cc/runtime:*heap-base-address* #x10000000))
    (let ((encoded (cl-cc/runtime:encode-pointer #x10001000 cl-cc/runtime:+tag-object+)))
      (assert-true (cl-cc/runtime:val-compressed-pointer-p encoded))
      (assert-= #x10001000 (cl-cc/runtime:decode-pointer encoded)))))

(deftest fr-265-small-string-optimization-roundtrip
  "FR-265: Small byte strings are encoded as SSO immediates and round-trip without heap allocation."
  (let ((encoded (cl-cc/runtime:cl-value->val "abc")))
    (assert-true (cl-cc/runtime:val-sso-string-p encoded))
    (assert-string= "abc" (cl-cc/runtime:val->cl-value encoded))))

(deftest fr-266-compressed-object-header-is-one-word
  "FR-266: Object headers pack size/tag/age metadata into one unsigned 64-bit word."
  (let ((header (cl-cc/runtime:make-header 42 cl-cc/runtime:+rt-tag-cons+ 2)))
    (assert-true (typep header '(unsigned-byte 64)))
    (assert-= 42 (cl-cc/runtime:header-size header))
    (assert-= cl-cc/runtime:+rt-tag-cons+ (cl-cc/runtime:header-tag header))
    (assert-= 2 (cl-cc/runtime:header-age header))))

(deftest fr-184-weak-reference-and-finalizer-evidence
  "FR-184: Weak references clear unreachable referents and finalizers run for unmarked objects."
  (let* ((heap (%make-small-heap-fr))
         (marked (make-hash-table :test #'eql))
         (referent (cl-cc/runtime:encode-pointer (cl-cc/runtime:rt-heap-old-base heap)
                                                  cl-cc/runtime:+tag-object+))
         (weak (cl-cc/runtime:rt-make-weak-ref referent))
         (object-addr (cl-cc/runtime:rt-heap-old-base heap))
         (finalized nil)
         (cl-cc/runtime::*rt-reference-registry* (list weak))
         (cl-cc/runtime::*rt-finalizer-registry* nil)
         (cl-cc/runtime::*rt-finalization-queue* nil))
    (cl-cc/runtime:rt-heap-set-header
     heap (cl-cc/runtime:rt-heap-old-base heap)
     (cl-cc/runtime:make-header 1 cl-cc/runtime:+rt-tag-cons+ 0))
    (cl-cc/runtime::%rt-gc-process-weak-references heap marked)
    (assert-null (cl-cc/runtime:rt-ref-get weak))
    (cl-cc/runtime:rt-register-finalizer object-addr (lambda (obj) (setf finalized obj)))
    (cl-cc/runtime::%rt-gc-process-finalizers heap marked)
    ;; After GC processing, object is queued but not yet executed
    (assert-equal (list object-addr) cl-cc/runtime::*rt-finalization-queue*)
    ;; Running pending finalizers executes them and clears the queue
    (cl-cc/runtime::rt-run-pending-finalizers)
    (assert-= object-addr finalized)))

(deftest fr-300-runtime-condition-restart-stacks
  "FR-300: Runtime handler and restart stacks establish dynamic recovery frames."
  (let ((handled nil))
    (multiple-value-bind (value foundp)
        (cl-cc/runtime:rt-establish-handler
         'error
         (lambda (condition) (setf handled condition) :handled)
         (lambda () (cl-cc/runtime:rt-dispatch-signal
                     (make-condition 'simple-error :format-control "x"))))
      (assert-true foundp)
      (assert-eq :handled value)
      (assert-true (typep handled 'simple-error))))
  (multiple-value-bind (value foundp)
      (cl-cc/runtime:rt-establish-restart
       'use-value
       (lambda (x) x)
       (lambda () (cl-cc/runtime:rt-dispatch-restart 'use-value '(42))))
    (assert-true foundp)
    (assert-= 42 value)))

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

(deftest fr-700-heap-profiler-report-has-stable-output-format
  "FR-700: heap allocation profiler emits the documented plist report shape."
  (let ((cl-cc/runtime:*gc-profile-enabled* t)
        (cl-cc/runtime::*gc-profile-interval* 16)
        (cl-cc/runtime::*gc-profile-bytes-since-sample* 0)
        (cl-cc/runtime::*gc-profile-samples* (make-hash-table :test #'equal))
        (cl-cc/runtime::*gc-profile-current-function* 'test-allocation-site))
    (cl-cc/runtime:rt-gc-profile-sample 48)
    (let ((report (cl-cc/runtime:rt-gc-profile-report)))
      (assert-eq t (getf report :enabled-p))
      (assert-= 16 (getf report :interval-bytes))
      (assert-true (listp (getf report :hot-spots)))
      (let ((spot (first (getf report :hot-spots))))
        (assert-eq 'test-allocation-site (getf spot :function))
        (assert-= 3 (getf spot :count))))))

;;; ------------------------------------------------------------
;;; FR-730..734: GC lifecycle references, finalizers, pinning
;;; ------------------------------------------------------------

(defun %gc-fr-marked-set (&rest addrs)
  (let ((marked (make-hash-table :test #'eql)))
    (dolist (addr addrs marked)
      (setf (gethash addr marked) t))))

(deftest fr-730-weak-pointer-clears-unmarked-heap-referent
  "FR-730: Weak pointers do not keep an unreachable heap referent alive."
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (weak (cl-cc/runtime:make-weak-pointer addr))
         (cl-cc/runtime::*rt-reference-registry* (list weak)))
    (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime::%rt-gc-process-weak-pointers heap (%gc-fr-marked-set))
    (assert-false (cl-cc/runtime:weak-pointer-value weak))))

(deftest fr-730-weak-pointer-keeps-marked-referent
  "FR-730: Weak pointer values survive when the strong graph marks their referent."
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (weak (cl-cc/runtime:rt-make-weak-pointer addr))
         (cl-cc/runtime::*rt-reference-registry* (list weak)))
    (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime::%rt-gc-process-weak-references heap (%gc-fr-marked-set addr))
    (assert-= addr (cl-cc/runtime:rt-weak-pointer-value weak))))

(deftest fr-731-ephemeron-marks-value-when-key-is-live
  "FR-731: Ephemeron processing conditionally marks the value when the key is live."
  (let* ((heap (%make-small-heap-fr))
         (key (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (value (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (marked (%gc-fr-marked-set key))
         (cl-cc/runtime:*rt-ephemeron-registry* nil))
    (%fr-write-object heap key 3 cl-cc/runtime:+rt-tag-cons+)
    (%fr-write-object heap value 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime:rt-make-ephemeron key value)
    (cl-cc/runtime::%rt-gc-process-ephemerons heap marked)
    (assert-true (gethash value marked))))

(deftest fr-732-finalizer-is-scheduled-after-gc-and-runs-outside-gc-pause
  "FR-732: Unreachable finalized objects are queued and finalized explicitly after GC."
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (seen nil)
         (cl-cc/runtime:*rt-finalizer-registry* nil)
         (cl-cc/runtime:*pending-finalizers* nil)
         (cl-cc/runtime:*rt-finalization-queue* nil))
    (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime:rt-register-finalizer addr (lambda (obj) (push obj seen)))
    (cl-cc/runtime::%rt-gc-process-finalizers heap (%gc-fr-marked-set))
    (assert-equal (list addr) cl-cc/runtime:*rt-finalization-queue*)
    (assert-= 1 (cl-cc/runtime:rt-run-pending-finalizers))
    (assert-equal (list addr) seen)))

(deftest fr-733-pinning-registers-and-cleans-up-relocation-barriers
  "FR-733: Pinned objects are recorded for compaction and unpinned on exit."
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
    (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime:with-pinned-objects ((pinned heap addr))
      (assert-= addr pinned)
      (assert-true (cl-cc/runtime:rt-object-pinned-p heap addr)))
    (assert-false (cl-cc/runtime:rt-object-pinned-p heap addr))))

(deftest fr-734-weak-hash-table-removes-dead-weak-keys
  "FR-734: Runtime weak hash tables remove entries whose weak key is unreachable."
  (let* ((heap (%make-small-heap-fr))
         (key (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (value :payload)
         (ht (cl-cc/runtime:rt-make-hash-table :weakness :key))
         (cl-cc/runtime::*rt-weak-hash-table-registry* (list ht)))
    (%fr-write-object heap key 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime:rt-sethash key ht value)
    (assert-= 1 (cl-cc/runtime:rt-hash-count ht))
    (cl-cc/runtime::%rt-gc-process-weak-hash-tables heap (%gc-fr-marked-set))
    (assert-= 0 (cl-cc/runtime:rt-hash-count ht))))

;;; ------------------------------------------------------------
;;; FR-373: Heap ASLR (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(deftest-each fr-gc-interface-function-exists
  "FR-373/FR-376: Pure CL GC interface entry points are fbound."
  :cases (("rt-heap-randomize-base"  'cl-cc/runtime:rt-heap-randomize-base)
          ("rt-install-stack-guard"  'cl-cc/runtime:rt-install-stack-guard))
  (sym)
  (assert-true (fboundp sym)))

(deftest fr-373-heap-randomize-returns-value
  "FR-373: rt-heap-randomize-base returns a non-negative integer offset."
  (let ((offset (cl-cc/runtime:rt-heap-randomize-base)))
    (assert-true (integerp offset))
    (assert-true (>= offset 0))))

;;; ------------------------------------------------------------
;;; FR-376: Guard Pages for Stack Overflow (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

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

;;; ------------------------------------------------------------
;;; FR-338: Parallel GC worker interface (⚠️ Pure CL/SB-THREAD interface)
;;; ------------------------------------------------------------

(deftest fr-338-parallel-root-scan-sequential-fallback
  "FR-338: root-scan worker API returns registered root addresses in fallback mode."
  (let ((heap (%make-small-heap-fr)))
    (let* ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
           (root (cons nil addr)))
      (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      (cl-cc/runtime:rt-gc-add-root heap root)
      (let ((roots (cl-cc/runtime:rt-gc-parallel-root-scan heap 1)))
        (assert-true (member addr roots :test #'eql)))
      (cl-cc/runtime:rt-gc-remove-root heap root))))

(deftest fr-338-worker-count-detection-is-non-negative
  "FR-338: worker-count detection returns a portable non-negative integer."
  (assert-true (integerp (cl-cc/runtime:rt-gc-detect-worker-count)))
  (assert-true (>= (cl-cc/runtime:rt-gc-detect-worker-count) 0)))

;;; ------------------------------------------------------------
;;; FR-343..345: TLAB and zero-fill interface (⚠️ Pure CL implementation)
;;; ------------------------------------------------------------

(deftest fr-343-tlab-alloc-bumps-private-buffer
  "FR-343: rt-gc-tlab-alloc allocates from a thread-local buffer and advances FREE."
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime::*gc-tlab-size-words* 8)
        (cl-cc/runtime::*rt-thread-local-heaps* nil))
    (let* ((addr (cl-cc/runtime:rt-gc-tlab-alloc heap :worker-a 3))
           (tlab (cl-cc/runtime::%rt-gc-tlab-for heap :worker-a)))
      (assert-= addr (cl-cc/runtime:rt-tlab-base tlab))
      (assert-= (+ addr 3) (cl-cc/runtime:rt-tlab-free tlab)))))

(deftest fr-344-tlab-retire-records-waste-and-dummy-header
  "FR-344: retiring TLABs records unused words and writes a dummy fill header."
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime::*gc-tlab-size-words* 8)
        (cl-cc/runtime::*gc-tlab-retire-fill* t)
        (cl-cc/runtime::*rt-thread-local-heaps* nil))
    (let* ((addr (cl-cc/runtime:rt-gc-tlab-alloc heap :worker-b 3))
           (tlab (cl-cc/runtime::%rt-gc-tlab-for heap :worker-b))
           (free-before (cl-cc/runtime:rt-tlab-free tlab)))
      (declare (ignore addr))
      (cl-cc/runtime:rt-gc-tlab-retire-all heap)
      (assert-true (cl-cc/runtime:rt-tlab-retired-p tlab))
      (assert-true (> (cl-cc/runtime:rt-tlab-waste-bytes tlab) 0))
      (assert-= 5 (cl-cc/runtime:header-size
                   (cl-cc/runtime:rt-heap-object-header heap free-before))))))

(deftest fr-345-tlab-allocation-returns-zero-filled-words
  "FR-345: Pure CL fallback exposes zero-initialized allocation words for SIMD zeroing sites."
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime::*gc-tlab-size-words* 8)
        (cl-cc/runtime::*rt-thread-local-heaps* nil))
    (let ((addr (cl-cc/runtime:rt-gc-tlab-alloc heap :worker-c 3)))
      (assert-= 0 (cl-cc/runtime:rt-heap-ref heap addr))
      (assert-= 0 (cl-cc/runtime:rt-heap-ref heap (+ addr 1)))
      (assert-= 0 (cl-cc/runtime:rt-heap-ref heap (+ addr 2))))))

(deftest fr-345-simd-zero-fill-returns-addr-and-is-fboundp
  "FR-345: rt-gc-simd-zero-fill is exported, callable, and returns its addr argument."
  (let ((heap (%make-small-heap-fr)))
    (assert-true (fboundp 'cl-cc/runtime:rt-gc-simd-zero-fill))
    (let ((result (cl-cc/runtime:rt-gc-simd-zero-fill heap 64 4)))
      (assert-= 64 result))
    ;; Validate argument type checks reject negative inputs
    (assert-signals error
                    (cl-cc/runtime:rt-gc-simd-zero-fill heap -1 4))
    (assert-signals error
                    (cl-cc/runtime:rt-gc-simd-zero-fill heap 0 -1))))

;;; ------------------------------------------------------------
;;; FR-347: Compressed object references (⚠️ Pure CL offset codec)
;;; ------------------------------------------------------------

(deftest fr-347-compressed-reference-roundtrip
  "FR-347: compressed object references round-trip as 32-bit heap-relative offsets."
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (offset (cl-cc/runtime:rt-compress-object-ref heap addr)))
    (assert-true (<= 0 offset #xffffffff))
    (assert-= addr (cl-cc/runtime:rt-decompress-object-ref heap offset))))

;;; ------------------------------------------------------------
;;; FR-363..365: NUMA allocation, local GC schedule, and interleaving metadata
;;; ------------------------------------------------------------

(deftest fr-363-numa-local-alloc-records-node-metadata
  "FR-363: portable NUMA local allocation records node metadata for the allocated address."
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime:*rt-numa-enabled* t))
    (let ((addr (cl-cc/runtime:rt-numa-local-alloc heap :thread-1 3)))
      (assert-true (integerp addr))
      (assert-= 0 (gethash addr (cl-cc/runtime::rt-heap-numa-node-map heap))))))

(deftest fr-364-numa-gc-affinity-records-worker-schedule
  "FR-364: NUMA-local GC API records a worker-to-node schedule."
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime:*gc-worker-count* 2))
    (let ((schedule (cl-cc/runtime:rt-gc-numa-affinity heap 0)))
      (assert-= 2 (length schedule))
      (assert-equal schedule (cl-cc/runtime::rt-heap-numa-gc-schedule heap)))))

(deftest fr-365-heap-interleave-records-shared-region
  "FR-365: interleaving API records shared-data region metadata."
  (let ((heap (%make-small-heap-fr)))
    (let ((region (cl-cc/runtime:rt-heap-interleave heap 4 8)))
      (assert-equal :interleave (getf region :policy))
      (assert-true (member region (cl-cc/runtime::rt-heap-interleaved-regions heap)
                           :test #'equal)))))

;;; ------------------------------------------------------------
;;; FR-367 / FR-371 / FR-375 / FR-391 / FR-392 additional warning-FR evidence
;;; ------------------------------------------------------------

(deftest fr-367-gc-probes-log-when-enabled
  "FR-367: DTrace/eBPF portable probe stubs emit trace lines when enabled."
  (let ((cl-cc/runtime:*gc-probes-enabled* t))
    (let ((output (with-output-to-string (*trace-output*)
                    (cl-cc/runtime:rt-gc-probe-alloc 7)
                    (cl-cc/runtime:rt-gc-probe-gc-start :minor)
                    (cl-cc/runtime:rt-gc-probe-gc-end :minor))))
      (assert-true (search "GC-PROBE-ALLOC 7" output))
      (assert-true (search "GC-PROBE-GC-START :MINOR" output))
      (assert-true (search "GC-PROBE-GC-END :MINOR" output)))))

(deftest fr-371-safe-region-depth-roundtrip
  "FR-371: cooperative safepoint safe-region depth increments and decrements."
  (let ((thread-id :fr-371-thread))
    (assert-= 1 (cl-cc/runtime:rt-gc-enter-safe-region thread-id))
    (assert-= 0 (cl-cc/runtime:rt-gc-leave-safe-region thread-id))))

(deftest fr-375-asan-shadow-memory-poisoning
  "FR-375: ASan shadow-memory fallback rejects poisoned heap addresses."
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime:*rt-asan-enabled* t))
    (cl-cc/runtime:rt-sanitizer-reset-state)
    (cl-cc/runtime:rt-sanitizer-poison-address 0)
    (assert-signals error (cl-cc/runtime:rt-heap-ref heap 0))
    (cl-cc/runtime:rt-sanitizer-unpoison-address 0)
    (assert-= 0 (cl-cc/runtime:rt-heap-ref heap 0))))

(deftest fr-391-heap-growth-policy-expands-old-space
  "FR-391: high post-GC occupancy grows old-space capacity in Pure CL fallback."
  (let ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (setf (cl-cc/runtime:rt-heap-young-free heap)
          (+ (cl-cc/runtime::rt-heap-young-from-base heap)
             (cl-cc/runtime::rt-heap-young-semi-size heap))
          (cl-cc/runtime::rt-heap-old-free heap)
          (+ (cl-cc/runtime:rt-heap-old-base heap)
             (cl-cc/runtime::rt-heap-old-size heap)))
    (let ((old-size-before (cl-cc/runtime::rt-heap-old-size heap)))
      (assert-true (cl-cc/runtime:rt-heap-maybe-grow heap))
      (assert-true (> (cl-cc/runtime::rt-heap-old-size heap) old-size-before)))))

(deftest fr-392-heap-shrink-policy-reduces-grown-heap
  "FR-392: sustained low occupancy shrinks a previously grown Pure CL heap."
  (let ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (setf (cl-cc/runtime:rt-heap-young-free heap)
          (+ (cl-cc/runtime::rt-heap-young-from-base heap)
             (cl-cc/runtime::rt-heap-young-semi-size heap))
          (cl-cc/runtime::rt-heap-old-free heap)
          (+ (cl-cc/runtime:rt-heap-old-base heap)
             (cl-cc/runtime::rt-heap-old-size heap)))
    (cl-cc/runtime:rt-heap-maybe-grow heap)
    (setf (cl-cc/runtime:rt-heap-young-free heap)
          (cl-cc/runtime::rt-heap-young-from-base heap)
          (cl-cc/runtime::rt-heap-old-free heap)
          (cl-cc/runtime:rt-heap-old-base heap))
    (let ((words-before (length (cl-cc/runtime::rt-heap-words heap))))
      (cl-cc/runtime:rt-heap-maybe-shrink heap)
      (cl-cc/runtime:rt-heap-maybe-shrink heap)
      (assert-true (cl-cc/runtime:rt-heap-maybe-shrink heap))
      (assert-true (< (length (cl-cc/runtime::rt-heap-words heap)) words-before)))))
