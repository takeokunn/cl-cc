;;;; packages/regalloc/src/package.lisp — feature package for cl-cc/regalloc
;;;;
;;;; Phase 6: linear-scan register allocator extracted into its own
;;;; feature package. Files moved here from :cl-cc/emit.

(defpackage :cl-cc/regalloc
  (:use :cl :cl-cc/vm :cl-cc/mir :cl-cc/target :cl-cc/optimize)
  (:shadowing-import-from :cl-cc/vm
    #:get-universal-time #:get-internal-real-time #:get-internal-run-time
    #:internal-time-units-per-second #:sleep #:time
    #:encode-universal-time #:decode-universal-time
    #:random-state #:random-state-p #:make-random-state #:*random-state* #:random
    #:*print-base* #:*print-radix* #:*print-circle*
    #:*print-pretty* #:*print-level* #:*print-length*
    #:*print-readably* #:*print-pprint-dispatch*
    #:with-standard-io-syntax
    #:pprint-logical-block #:pprint-indent #:pprint-newline #:pprint-tab
    #:copy-pprint-dispatch #:set-pprint-dispatch #:get-pprint-dispatch
    #:*readtable* #:copy-readtable
    #:set-macro-character #:get-macro-character
    #:set-dispatch-macro-character #:get-dispatch-macro-character
    #:readtable-case
    #:lisp-implementation-type #:lisp-implementation-version
    #:machine-type #:machine-version #:machine-instance
    #:software-type #:software-version
    #:room #:apropos #:apropos-list)
  (:export
   ;; ─── regalloc.lisp — register allocation ──────────────────────────
   #:live-interval
   #:make-live-interval
   #:interval-vreg
   #:interval-start
   #:interval-end
    #:interval-phys-reg
    #:interval-spill-slot
    #:interval-remat-const
    #:interval-remat-inst
    #:regalloc-result
   #:regalloc-assignment
    #:regalloc-spill-map
    #:regalloc-spill-count
    #:regalloc-gpr-pressure
    #:regalloc-fp-pressure
    #:regalloc-instructions
   #:regalloc-lookup
   #:instruction-defs
   #:instruction-uses
     #:compute-live-intervals
     #:split-live-interval
      #:color-spill-slots
      #:linear-scan-allocate
      #:color-allocate
      #:*regalloc-allocation-strategy*
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
