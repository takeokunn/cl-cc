;;;; packages/runtime/src/gc.lisp - 2-Generation Generational GC for CL-CC Native Runtime
;;;
;;; Depends on heap.lisp being loaded first (rt-heap struct, header functions,
;;; card table, address predicates, rt-object-pointer-slots, etc.).
;;;
;;; Public API:
;;;   rt-gc-alloc           - bump-pointer allocation in young from-space
;;;   rt-gc-add-root        - register a GC root cell
;;;   rt-gc-remove-root     - unregister a GC root cell
;;;   rt-gc-write-barrier   - SATB pre-write barrier + card table update
;;;   rt-gc-minor-collect   - Cheney copying minor GC (young generation)
;;;   rt-gc-major-collect   - tri-color mark-sweep major GC (old generation)
;;;   rt-gc-stats           - return a plist of GC statistics
;;;
;;; Internal helpers are prefixed with %gc-.

(in-package :cl-cc/runtime)

(defparameter *gc-verify-after-collect* nil
  "When true, verify heap invariants after each GC cycle.")

(defparameter *gc-stress-mode* nil
  "When true, trigger a minor GC after each young-space allocation.")

(defparameter *rt-use-slab-allocator* nil
  "When T, use slab/size-class allocators for fixed-size objects (FR-359).
   Default NIL keeps the bump-pointer allocator as the baseline; enable for optimized workloads.")

(defparameter *gc-conservative-roots* nil
  "When true, conservatively scan registered thread stacks for heap pointers.")

(defparameter *gc-compaction-enabled* nil
  "When true, major GC may invoke the old-space compaction trigger.

FR-089/FR-213: sliding compaction is implemented.  See
rt-gc-compact-old-space for details.  Pinned objects are hard relocation
barriers and must not be moved by compaction.")

(define-symbol-macro *compacting-gc-enabled* *gc-compaction-enabled*)

(defparameter *gc-compact-after-major-cycles* 0
  "Run old-generation mark-compact after every N major GC cycles when positive.

The default 0 disables periodic compaction.  Fragmentation-driven compaction
still uses RT-HEAP-SHOULD-COMPACT-P, and both paths remain gated by
*COMPACTING-GC-ENABLED* / *GC-COMPACTION-ENABLED*.")

(defparameter *gc-threads* '((:name :main :stack nil :frames nil))
  "Thread states scanned by the GC.  Each state may be a plist containing :STACK
and/or :FRAMES, a VM-FRAME, a vector, or a list of raw stack words.")

(defparameter *gc-inhibit-during-signals* t
  "When true, signal-safe regions may inhibit automatic GC entry.")

(defvar *rt-gc-safe-region-depths* (make-hash-table :test #'equal)
  "Logical thread id -> GC safe-region nesting depth.

A positive depth means the mutator has reached a point where the collector may
observe its roots.  The Pure CL runtime uses this as a portable safepoint model;
native backends can map the same API to thread suspension/cooperation.")

(defparameter *gc-pending* nil
  "Global cooperative GC request flag observed by safepoint checks.")

(defparameter *rt-preemption-pending* nil
  "When true, mutators yield at cooperative preemption safepoints.")

(defparameter *rt-preemption-yield-hook* nil
  "Optional function called by safepoints when cooperative preemption is pending.")

(defvar *rt-gc-stackmap-table* (make-hash-table :test #'equal)
  "Function/frame id -> RT-STACKMAP metadata for precise stack root scanning.")

(defstruct (rt-stackmap (:constructor make-rt-stackmap
                            (&key frame-id (slots nil) (source :compiler-stub))))
  "Precise stack map for one activation frame.

SLOTS is an alist of (FRAME-OFFSET . KIND), where KIND is :OBJECT for traced
heap references and scalar kinds such as :FIXNUM, :DOUBLE, and :CHAR are ignored
by the collector. SOURCE records whether the map came from the compiler or a
runtime stub."
  frame-id
  (slots nil :type list)
  (source :compiler-stub :type keyword))

(defvar *rt-current-gc-heap* nil
  "Dynamically bound heap used by signal handlers that need to inhibit GC.")

(defparameter *rt-current-thread-id* :main
  "Current logical runtime thread id used by GC safepoint helpers.")

(defparameter *rt-heap-hugepage-enabled* nil
  "When true, native heap setup may request Transparent Huge Page promotion.")

(defvar *rt-gc-root-types* (make-hash-table :test #'eq)
  "Heap -> alist mapping root cells to precise root types.")

(defvar *rt-pinned-objects* (make-hash-table :test #'eq)
  "Heap -> hash-table of pinned object addresses.

Pinning is a relocation constraint: pinned objects must not be moved by old-space
compaction.  During minor GC, a pinned young object is forced through the
promotion path so the inactive semi-space never retains the only live copy.")

(defvar *rt-current-pinning-heap* nil
  "Dynamic heap used by WITH-PINNED-OBJECTS two-element bindings.

Callers that use (WITH-PINNED-OBJECTS ((VAR OBJ) ...) ...) must bind this to the
target heap.  The macro also accepts three-element bindings (VAR HEAP OBJ) when a
heap expression should be supplied explicitly.")

(defparameter *rt-use-barrier-batching* nil
  "When true, write barriers may enqueue card marks for later flushing.")

(defvar *rt-gc-alloc-hooks* nil)
(defvar *rt-gc-death-hooks* nil)

(defparameter *vm-call-frame-pool-limit* 256
  "Maximum number of recycled VM call frames retained by RT-FREE-CALL-FRAME.")

(defvar *vm-call-frame-pool* nil
  "Pool of recycled VM call frames for short-lived calls.")

(defparameter *gc-worker-count* 0
  "Number of parallel GC worker threads.  0 keeps the collector sequential.

Pure Common Lisp has no standard thread API; this runtime uses SB-THREAD when it
is available and otherwise executes worker tasks sequentially.")

(defvar *rt-minor-gc-window-start* nil)
(defvar *rt-low-promotion-cycles* 0)
