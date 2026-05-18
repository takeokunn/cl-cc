(in-package :cl-cc/optimize)

(defun opt-build-tls-plan (&key target hot-access-p)
  "Build TLS access plan for TARGET architecture.

When HOT-ACCESS-P is true, choose inline segment/thread-pointer based access."
  (let ((base (case target
                (:x86-64 :fs)
                (:aarch64 :tpidr_el0)
                (otherwise nil))))
    (make-opt-tls-plan
     :target target
     :uses-inline-tls-p (and hot-access-p (not (null base)))
     :base-register (and hot-access-p base))))

(defun opt-select-atomic-opcode (&key target operation memory-order)
  "Select a representative atomic opcode for OPERATION and MEMORY-ORDER.

This helper captures lowering intent only; exact instruction encoding remains
backend responsibility."
  (declare (ignore memory-order))
  (case target
    (:x86-64
     (case operation
       (:incf :lock-xadd)
       (:cas :lock-cmpxchg)
       (otherwise :lock-op)))
    (:aarch64
     (case operation
       (:incf :ldadd)
       (:cas :ldxr-stxr)
       (otherwise :atomic-op)))
    (otherwise :atomic-op)))

(defun opt-build-atomic-plan (&key target operation memory-order)
  "Build atomic lowering plan with selected opcode."
  (make-opt-atomic-plan
   :target target
   :operation operation
   :memory-order memory-order
   :opcode (opt-select-atomic-opcode
            :target target
            :operation operation
            :memory-order memory-order)))

(defun opt-build-htm-plan (&key target supports-htm-p low-contention-p)
  "Build an HTM lock-elision plan.

HTM path is enabled only when hardware support exists and contention is low.
Fallback lock path remains enabled conservatively."
  (let* ((enable-htm-p (and supports-htm-p low-contention-p))
         (begin (case target
                  (:x86-64 :xbegin)
                  (:power10 :tbegin)
                  (:power :tbegin)
                  (otherwise nil)))
         (end (case target
                (:x86-64 :xend)
                ((:power10 :power) :tend)
                (otherwise nil)))
         (abort (case target
                  (:x86-64 :xabort)
                  ((:power10 :power) :tabort)
                  (otherwise nil)))
         (usable (not (null (and enable-htm-p begin end abort)))))
    (make-opt-htm-plan
     :target target
     :uses-htm-p usable
     :begin-opcode (and usable begin)
     :end-opcode (and usable end)
     :abort-opcode (and usable abort)
     :fallback-lock-p t)))

(defun opt-build-concurrent-gc-plan (&key latency-sensitive-p heap-size)
  "Build conservative concurrent-GC plan for tri-color marking.

LATENCY-SENSITIVE-P keeps concurrent marking + SATB barrier.
Small heaps may disable mutator assist to avoid overhead."
  (let* ((small-heap-p (and heap-size (< heap-size (* 32 1024 1024))))
         (concurrent-mark-p (not (null latency-sensitive-p)))
         (barrier (if concurrent-mark-p :satb :incremental-update))
         (mutator-assist-p (and concurrent-mark-p (not small-heap-p))))
    (make-opt-concurrent-gc-plan
     :concurrent-mark-p concurrent-mark-p
     :write-barrier barrier
     :mutator-assist-p mutator-assist-p
     :stw-phases (if concurrent-mark-p
                      '(:initial-mark :final-remark)
                      '(:full-mark-sweep)))))
