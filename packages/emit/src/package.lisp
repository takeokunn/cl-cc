;;;; packages/emit/src/package.lisp — cl-cc/emit package definition
;;;;
;;;; Emit backend: machine code generation (x86-64, AArch64, WASM) and register allocation.

(defpackage :cl-cc/emit
  (:use :cl :cl-cc/vm :cl-cc/mir :cl-cc/optimize :cl-cc/codegen)
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
   ;; Re-export regalloc + emit symbols ---
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
    #:color-allocate
    #:allocate-registers
   #:vm-spill-store
   #:vm-spill-load
   #:make-vm-spill-store
   #:make-vm-spill-load
   #:vm-spill-src
   #:vm-spill-dst
   #:vm-spill-slot

   #:vm-reg-to-x86-with-alloc
   #:*current-regalloc*
   #:*phys-reg-to-x86-code*
   #:*phys-reg-to-asm-string*

   #:emit-instruction
   #:x86-64-target #:target-spill-base-reg
   #:aarch64-target
   #:compile-to-wasm-wat
   #:x86-64-red-zone-spill-p

    #:target-regalloc
    #:compile-to-native
    #:compile-file-to-native
    #:compile-to-x86-64-bytes
    #:compile-to-aarch64-bytes

     ;; Heterogeneous backend planning APIs (FR-442)
     #:ebpf-program-plan
     #:make-ebpf-program-plan
     #:plan-ebpf-program
     #:compile-ebpf-program
     #:emit-ebpf-bytecode))
