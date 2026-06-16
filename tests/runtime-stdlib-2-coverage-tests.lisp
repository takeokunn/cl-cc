(in-package :cl-cc/test)

(defsuite runtime-stdlib-2-coverage-suite
  :description "Runtime-stdlib-2 FR evidence registry coverage tests"
  :parent cl-cc-documentation-suite)

(in-suite runtime-stdlib-2-coverage-suite)

(defparameter *runtime-stdlib-2-fr-ids*
  '("FR-787" "FR-788" "FR-791" "FR-792" "FR-793" "FR-796" "FR-797" "FR-800" "FR-801"
    "FR-804" "FR-805" "FR-808" "FR-809" "FR-812" "FR-813" "FR-816" "FR-817"
    "FR-820" "FR-821" "FR-824" "FR-827" "FR-828" "FR-829" "FR-830"
    "FR-833" "FR-834" "FR-835" "FR-838" "FR-839"
    "FR-842" "FR-843" "FR-844" "FR-847" "FR-848"
    "FR-851" "FR-852" "FR-853" "FR-856" "FR-857"
    "FR-860" "FR-861" "FR-864" "FR-865"
    "FR-868" "FR-869" "FR-872" "FR-873" "FR-876" "FR-877"
    "FR-880" "FR-881" "FR-884" "FR-885" "FR-888" "FR-889"
    "FR-892" "FR-895" "FR-896" "FR-899" "FR-902"
    "FR-905" "FR-908" "FR-911" "FR-914" "FR-917" "FR-920"
    "FR-923" "FR-924" "FR-927" "FR-930" "FR-931")
  "Authoritative runtime-stdlib-2 FR ID list for Phases 138-175.")

(defparameter *runtime-stdlib-2-fr-evidence-registry*
  '(("FR-787" . "packages/vm/src/string-builder.lisp")     ; Phase 138 String Builder
    ("FR-788" . "packages/vm/src/string-builder.lisp")     ; Phase 138 Rope
    ("FR-791" . "packages/runtime/src/log.lisp")            ; Phase 139 Structured Logging
    ("FR-792" . "packages/runtime/src/metrics.lisp")        ; Phase 139 Runtime Metrics
    ("FR-793" . "packages/runtime/src/perf.lisp")           ; Phase 139 Hardware Perf Counters
    ("FR-796" . "packages/tools/src/lsp-server.lisp")       ; Phase 140 LSP Server
    ("FR-797" . "packages/tools/src/dap-server.lisp")       ; Phase 140 DAP Server
    ("FR-800" . "packages/vm/src/vm-execute.lisp")          ; Phase 141 Full call/cc
    ("FR-801" . "packages/compile/src/codegen.lisp")        ; Phase 141 Escape Continuations
    ("FR-804" . "packages/expand/src/syntax-rules.lisp")    ; Phase 142 Hygienic Macros
    ("FR-805" . "packages/expand/src/macro.lisp")           ; Phase 142 Gensym Hygiene
    ("FR-808" . "packages/cli/src/main.lisp")               ; Phase 143 Shebang/Script
    ("FR-809" . "packages/cli/src/main.lisp")               ; Phase 143 CLI Arguments
    ("FR-812" . "packages/runtime/src/ffi.lisp")            ; Phase 144 C Embedding
    ("FR-813" . "packages/vm/src/vm.lisp")                  ; Phase 144 Multi-VM
    ("FR-816" . "packages/runtime/src/heap-core.lisp")      ; Phase 145 Arena Allocator
    ("FR-817" . "packages/runtime/src/heap-core.lisp")      ; Phase 145 Object Pool
    ("FR-820" . "packages/vm/src/io.lisp")                  ; Phase 146 Print-Circle
    ("FR-821" . "packages/vm/src/vm-clos.lisp")             ; Phase 146 Copy-Structure
    ("FR-824" . "packages/vm/src/persistent.lisp")          ; Phase 147 Transient Collections
    ("FR-827" . "packages/vm/src/vm-execute.lisp")          ; Phase 148 Bounds Checking
    ("FR-828" . "packages/vm/src/vm-execute.lisp")          ; Phase 148 Stack Canaries
    ("FR-829" . "packages/vm/src/primitives.lisp")          ; Phase 148 Overflow Detection
    ("FR-830" . "packages/vm/src/vm.lisp")                  ; Phase 148 Taint Tracking
    ("FR-833" . "packages/runtime/src/gc-policy.lisp")      ; Phase 149 GC Tuning
    ("FR-834" . "packages/vm/src/vm-run.lisp")              ; Phase 149 JIT Thresholds
    ("FR-835" . "packages/vm/src/vm-run.lisp")              ; Phase 149 Adaptive Runtime
    ("FR-838" . "packages/vm/src/vm-clos.lisp")             ; Phase 150 Sequence Protocol
    ("FR-839" . "packages/expand/src/macros-stdlib.lisp")   ; Phase 150 Iteration Protocol
    ("FR-842" . "packages/vm/src/primitives.lisp")          ; Phase 151 Kahan Summation
    ("FR-843" . "packages/vm/src/primitives.lisp")          ; Phase 151 Float Exception
    ("FR-844" . "packages/vm/src/primitives.lisp")          ; Phase 151 Extended Precision
    ("FR-847" . "packages/vm/src/conditions.lisp")          ; Phase 152 Mutex/Condvar
    ("FR-848" . "packages/vm/src/conditions.lisp")          ; Phase 152 Semaphore/RWLock
    ("FR-851" . "packages/vm/src/io.lisp")                  ; Phase 153 TCP/UDP Socket
    ("FR-852" . "packages/vm/src/io.lisp")                  ; Phase 153 DNS Resolution
    ("FR-853" . "packages/vm/src/io.lisp")                  ; Phase 153 TLS/SSL
    ("FR-856" . "packages/expand/src/macros-stdlib.lisp")   ; Phase 154 delay/force
    ("FR-857" . "packages/expand/src/macros-stdlib.lisp")   ; Phase 154 Memoization
    ("FR-860" . "packages/vm/src/primitives.lisp")          ; Phase 155 Numeric Contagion
    ("FR-861" . "packages/compile/src/codegen.lisp")        ; Phase 155 Inline Dispatch
    ("FR-864" . "packages/vm/src/vm.lisp")                  ; Phase 156 MV Frame Protocol
    ("FR-865" . "packages/vm/src/vm-execute.lisp")          ; Phase 156 MV through apply
    ("FR-868" . "packages/vm/src/io.lisp")                  ; Phase 157 file-position
    ("FR-869" . "packages/vm/src/io.lisp")                  ; Phase 157 Memory-Mapped Files
    ("FR-872" . "packages/vm/src/strings.lisp")             ; Phase 158 CoW Strings
    ("FR-873" . "packages/vm/src/vm.lisp")                  ; Phase 158 CoW Arrays
    ("FR-876" . "packages/runtime/src/runtime.lisp")        ; Phase 159 Segmented Stack
    ("FR-877" . "packages/runtime/src/runtime.lisp")        ; Phase 159 Copying Stack
    ("FR-880" . "packages/vm/src/hash.lisp")                ; Phase 160 Custom Hash Functions
    ("FR-881" . "packages/vm/src/hash.lisp")                ; Phase 160 Hash Resizing
    ("FR-884" . "packages/vm/src/primitives.lisp")          ; Phase 161 floor/truncate Complete
    ("FR-885" . "packages/vm/src/primitives.lisp")          ; Phase 161 ffloor family
    ("FR-888" . "packages/vm/src/vm-clos.lisp")             ; Phase 162 allocate-instance Fast Path
    ("FR-889" . "packages/vm/src/vm-clos.lisp")             ; Phase 162 default-initargs Cache
    ("FR-892" . "packages/compile/src/codegen.lisp")        ; Phase 163 load-time-value
    ("FR-895" . "packages/vm/src/vm.lisp")                  ; Phase 164 Symbol Table Compaction
    ("FR-896" . "packages/vm/src/vm.lisp")                  ; Phase 164 Package Lock
    ("FR-899" . "packages/vm/src/vm.lisp")                  ; Phase 165 FASL Demand Paging
    ("FR-902" . "packages/vm/src/vm.lisp")                  ; Phase 166 PGO Persistence
    ("FR-905" . "packages/compile/src/codegen.lisp")        ; Phase 167 TCO/unwind-protect
    ("FR-908" . "packages/expand/src/macros-stdlib.lisp")   ; Phase 168 Quasiquote Optimization
    ("FR-911" . "packages/emit/src/riscv64.lisp")           ; Phase 169 RISC-V Backend
    ("FR-914" . "packages/vm/src/vm-execute.lisp")          ; Phase 170 Delimited Continuations
    ("FR-917" . "packages/cli/src/main.lisp")               ; Phase 171 Reproducible Builds
    ("FR-920" . "packages/compile/src/codegen.lisp")        ; Phase 172 Forward References
    ("FR-923" . "packages/vm/src/io.lisp")                  ; Phase 173 I/O Buffering
    ("FR-924" . "packages/vm/src/io.lisp")                  ; Phase 173 Special Streams
    ("FR-927" . "packages/vm/src/pathname.lisp")            ; Phase 174 Pathname System
    ("FR-930" . "packages/vm/src/vm-clos.lisp")             ; Phase 175 MOP Introspection
    ("FR-931" . "packages/vm/src/vm-clos.lisp"))            ; Phase 175 CAMUC
  "Mapping from runtime-stdlib-2 FR IDs to expected evidence files.")

(deftest runtime-stdlib-2-fr-registry-covers-all-71-ids
  "The central registry maps every runtime-stdlib-2 FR ID to an expected evidence file."
  :timeout 5
  (let ((registered (mapcar #'car *runtime-stdlib-2-fr-evidence-registry*)))
    (assert-equal 71 (length *runtime-stdlib-2-fr-ids*))
    (assert-equal 71 (length registered))
    (assert-equal nil (set-difference *runtime-stdlib-2-fr-ids* registered :test #'string=))
    (assert-equal nil (set-difference registered *runtime-stdlib-2-fr-ids* :test #'string=))
    (dolist (entry *runtime-stdlib-2-fr-evidence-registry*)
      (assert-true (stringp (cdr entry)))
      (assert-true (plusp (length (cdr entry)))))))
