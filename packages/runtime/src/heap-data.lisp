;;;; packages/runtime/src/heap.lisp - CL-CC Managed Heap for Generational GC
;;;
;;; This file provides the managed heap data structure for a 2-generation
;;; Generational GC:
;;;   - Young (nursery) split into 2 semi-spaces (from-space + to-space)
;;;     for Cheney copying collection.
;;;   - Old generation for tenured objects with a card table for tracking
;;;     old-to-young pointer references.
;;;
;;; Memory layout (flat simple-vector):
;;;   [0 .. semi-size-1]                  = from-space (young)
;;;   [semi-size .. 2*semi-size-1]         = to-space (young)
;;;   [2*semi-size .. 2*semi-size+old-1]   = old space
;;;
;;; Object header word bit layout (non-negative integer):
;;;   bit 63..32 = size in words (including header)
;;;   bit 31..24 = type tag (0..7, matching +tag-* constants in runtime.lisp)
;;;   bit 23..20 = age (0-15, minor GC survival cycles)
;;;   bit 19     = mark bit (set during major GC marking)
;;;   bit 18     = gray bit (SATB marking queue membership)
;;;   bit 17     = forwarding flag (Cheney copy; when set, header encodes dest addr)
;;;   bit 16..0  = reserved / forwarding address (when forwarding flag is set)

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Configuration Parameters
;;; ------------------------------------------------------------

(defparameter *gc-young-size-words* (* 128 1024)
  "Young space in 64-bit words (1MB total, 512KB per semi-space).")

(defparameter *gc-old-size-words* (* 512 1024)
  "Old space in 64-bit words (4MB).")

(defparameter *gc-max-heap-words* 0
  "Maximum live GC heap words before allocation must force a major GC.
   A value of 0 means unlimited.")

(defparameter *gc-throughput-target* 0.05d0
  "Target fraction of wall-clock time spent in GC pauses.
   The default 0.05 means GC should consume at most 5% of runtime.")

(defparameter *rt-numa-enabled* nil
  "When true, route allocation/GC scheduling through portable NUMA hooks.

The Pure CL runtime exposes the interface but has no OS NUMA bindings; every
thread maps to node 0. Native Linux builds can back this with libnuma
numa_alloc_local(3), mbind(2), and scheduler affinity. Windows builds can use
VirtualAllocExNuma plus processor-group affinity APIs.")

(defparameter *rt-stack-guard-enabled* t
  "When true, native VM stack setup should reserve a guard page at stack end.

In the Pure CL runtime this is a documented portable interface: host-specific
launchers may use mmap/mprotect (or equivalent) to mark the final stack page as
inaccessible and install a SIGSEGV/SIGBUS handler that reports stack overflow
instead of allowing adjacent memory corruption.  Without platform support the
runtime falls back to returning guard metadata from RT-INSTALL-STACK-GUARD.")

(defconstant +rt-default-system-memory-bytes+ (* 512 1024 1024)
  "Portable fallback for host memory detection (512MiB).")
