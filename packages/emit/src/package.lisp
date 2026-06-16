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
    #:room #:apropos #:apropos-list
    #:*read-base* #:*print-right-margin* #:*print-lines*
    #:*print-case* #:*print-escape* #:*print-gensym* #:*print-array*
    #:*trace-output*
    #:sequence #:elt #:length #:subseq
    #:stream-external-format
    #:break #:describe #:inspect #:ed #:dribble
    #:invoke-debugger #:trace #:untrace #:step
    #:*read-suppress* #:*read-default-float-format* #:*read-eval*
    #:*features* #:*modules*
    #:method-qualifiers #:compute-applicable-methods
    #:find-method #:add-method #:remove-method #:ensure-generic-function
    #:open #:*load-pathname* #:*load-truename* #:*compile-file-pathname*
    #:*compile-file-truename* #:*compile-print* #:*compile-verbose*
    #:*load-print* #:*load-verbose*
    #:exit
    #:getenv #:setenv
    #:*command-line-args* #:*default-external-format*
    #:read-preserving-whitespace #:read-delimited-list
    #:set-syntax-from-char
)
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
      #:source-map-encode-vlq
      #:build-wasm-source-map-v3
      #:write-wasm-source-map
      #:write-wasm-with-source-map
      #:*wasm-wasi-enabled*
      #:make-wasi-p2-imports
      #:wasi-p2-import->wat
       #:inject-wasi-p2-imports-into-wat
       #:*wasm-threads-enabled*
       #:*wasm-threads-worker-count*
       #:wasm-threads-flag-enabled-p
       #:emit-wasm-shared-memory
       #:emit-wasm-shared-memory-init
       #:wasm-shared-memory-wat
       #:emit-wasm-atomic-rmw-add
       #:emit-wasm-atomic-rmw-cmpxchg
       #:emit-wasm-i64-atomic-rmw-cmpxchg
       #:emit-wasm-atomic-wait
       #:emit-wasm-atomic-notify
       #:emit-wasm-fence
       #:wasm-atomic-wat
       #:wasm-cl-thread-semantic-mapping
       #:wasm-worker-runtime-initialization
       #:ebpf-program-plan
      #:make-ebpf-program-plan
      #:ebpf-program-plan-program-name
      #:ebpf-program-plan-hook-point
      #:ebpf-program-plan-instructions
      #:ebpf-program-plan-maps
      #:ebpf-program-plan-uses-helpers-p
      #:ebpf-program-plan-verifier-safe-p
      #:ebpf-map-def
      #:make-ebpf-map-def
      #:ebpf-map-def-name
      #:ebpf-map-def-type
      #:ebpf-map-def-key-size
      #:ebpf-map-def-value-size
      #:ebpf-map-def-max-entries
      #:ebpf-map-def-flags
      #:ebpf-map-def-fd
      #:plan-ebpf-program
      #:compile-ebpf-program
      #:emit-ebpf-bytecode
      #:emit-ebpf-elf-object
      #:validate-ebpf-verifier-constraints
      #:ebpf-target-flag-p
      #:+ebpf-target-flag+
      #:+bpf-helper-map-lookup-elem+
      #:+bpf-helper-map-update-elem+
      #:+bpf-map-type-array+
      #:+bpf-map-type-hash+
      #:llvm-ir-module
      #:make-llvm-ir-module
      #:llvm-ir-module-name
      #:llvm-ir-module-target-triple
      #:llvm-ir-module-data-layout
      #:llvm-ir-module-body
      #:llvm-ir-module-metadata
      #:llvm-ir-bridge-capabilities
      #:vm-program->llvm-ir-module
      #:emit-llvm-ir
      #:mlir-module
      #:make-mlir-module
      #:mlir-module-name
      #:mlir-module-dialect
      #:mlir-module-body
      #:mlir-module-metadata
      #:mlir-bridge-capabilities
      #:vm-program->mlir-module
      #:emit-mlir
      ;; RISC-V 64 backend (FR-860)
      #:make-riscv64-assembler
      #:riscv64-emit-instruction
      #:riscv64-emit-bytes
      #:riscv64-emit-function
      #:riscv64-emit-prologue
      #:riscv64-emit-epilogue
      #:riscv64-emit-load-immediate
      #:riscv64-emit-pic-call))
