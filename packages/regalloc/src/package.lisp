;;;; packages/regalloc/src/package.lisp — feature package for cl-cc/regalloc
;;;;
;;;; Phase 6: linear-scan register allocator extracted into its own
;;;; feature package. Files moved here from :cl-cc/emit.

(defpackage :cl-cc/regalloc
  (:use :cl :cl-cc/vm :cl-cc/mir :cl-cc/target :cl-cc/optimize)
  (:export
   ;; ─── regalloc.lisp — register allocation ──────────────────────────
   #:live-interval
   #:make-live-interval
   #:interval-vreg
   #:interval-start
   #:interval-end
   #:interval-phys-reg
   #:interval-spill-slot
   #:regalloc-result
   #:regalloc-assignment
   #:regalloc-spill-map
   #:regalloc-spill-count
   #:regalloc-instructions
   #:regalloc-lookup
   #:instruction-defs
   #:instruction-uses
   #:compute-live-intervals
   #:linear-scan-allocate
   #:allocate-registers

   ;; ─── regalloc-allocate.lisp — spill instructions ──────────────────
   #:vm-spill-store
   #:vm-spill-load
   #:make-vm-spill-store
   #:make-vm-spill-load
   #:vm-spill-src
   #:vm-spill-dst
   #:vm-spill-slot
   #:*current-regalloc*))
