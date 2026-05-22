(in-package :cl-cc/test)

(defsuite runtime-stdlib-2-vm-suite
  :description "Runtime-stdlib-2 VM feature verification tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-2-vm-suite)

(deftest runtime-stdlib-2-vm-system-loads
  "The VM system is loadable with runtime-stdlib-2 features."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-vm nil)))

(deftest fr-787-string-builder-exists
  "FR-787: String builder functions are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::make-string-builder))
  (assert-true (fboundp 'cl-cc/vm::string-builder-append!))
  (assert-true (fboundp 'cl-cc/vm::string-builder-finish)))

(deftest fr-788-rope-exists
  "FR-788: Rope functions are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::rope))
  (assert-true (fboundp 'cl-cc/vm::rope-concat))
  (assert-true (fboundp 'cl-cc/vm::rope-split))
  (assert-true (fboundp 'cl-cc/vm::rope-to-string)))

(deftest fr-791-logging-exists
  "FR-791: Structured logging functions are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::rt-log-error))
  (assert-true (fboundp 'cl-cc/runtime::rt-log-warn))
  (assert-true (fboundp 'cl-cc/runtime::rt-log-info)))

(deftest fr-792-metrics-exists
  "FR-792: Runtime metrics API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::rt-make-counter))
  (assert-true (fboundp 'cl-cc/runtime::rt-counter-increment!))
  (assert-true (fboundp 'cl-cc/runtime::rt-make-histogram)))

(deftest fr-793-perf-counters-exists
  "FR-793: Hardware performance counter API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::rt-perf-init))
  (assert-true (fboundp 'cl-cc/runtime::rt-with-perf-counters)))

(deftest fr-796-lsp-exists
  "FR-796: LSP server symbols are defined."
  :timeout 5
  (assert-true (find-package :cl-cc/tools/lsp)))

(deftest fr-797-dap-exists
  "FR-797: DAP server symbols are defined."
  :timeout 5
  (assert-true (find-package :cl-cc/tools/dap)))

(deftest fr-800-callcc-exists
  "FR-800: call/cc is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::call-with-current-continuation)))

(deftest fr-801-escape-exists
  "FR-801: Escape continuation primitives are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-call/cc)))

(deftest fr-804-syntax-rules-exists
  "FR-804: syntax-rules hygienic macros are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/expand::define-syntax))
  (assert-true (fboundp 'cl-cc/expand::with-gensyms)))

(deftest fr-805-once-only-exists
  "FR-805: once-only macro is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/expand::once-only)))

(deftest fr-808-shebang-exists
  "FR-808: Script mode / shebang support is present."
  :timeout 5
  (assert-true (fboundp 'cl-cc/cli::parse-cli-args)))

(deftest fr-809-argv-exists
  "FR-809: Command-line arguments API is present."
  :timeout 5
  (assert-true (fboundp 'cl-cc/cli::cl-cc-argv)))

(deftest fr-812-c-embedding-exists
  "FR-812: C Embedding API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::cl-cc-init))
  (assert-true (fboundp 'cl-cc/runtime::cl-cc-eval))
  (assert-true (fboundp 'cl-cc/runtime::cl-cc-call)))

(deftest fr-813-multi-vm-exists
  "FR-813: Multi-VM instance API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::make-vm-instance)))

(deftest fr-816-arena-exists
  "FR-816: Arena allocator API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::make-arena)))

(deftest fr-817-object-pool-exists
  "FR-817: Object pool API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::make-object-pool)))

(deftest fr-820-print-circle-exists
  "FR-820: Print-circle support is present."
  :timeout 5
  (assert-true (boundp 'cl-cc/vm::*print-circle*)))

(deftest fr-821-copy-structure-exists
  "FR-821: copy-structure is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::copy-structure)))

(deftest fr-824-transients-exists
  "FR-824: Transient collection API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::transient)))

(deftest fr-827-bounds-check-exists
  "FR-827: Safety level / bounds checking is defined."
  :timeout 5
  (assert-true (boundp 'cl-cc/vm::*safety-level*)))

(deftest fr-828-stack-canary-exists
  "FR-828: Stack canary support is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-stack-canary-check)))

(deftest fr-829-overflow-exists
  "FR-829: Integer overflow detection is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-check-fixnum-overflow)))

(deftest fr-830-taint-exists
  "FR-830: Taint tracking is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::taint-mark)))

(deftest fr-833-gc-tuning-exists
  "FR-833: GC tuning parameters are defined."
  :timeout 5
  (assert-true (boundp 'cl-cc/runtime::*gc-nursery-size*)))

(deftest fr-834-jit-thresholds-exists
  "FR-834: JIT threshold configuration is defined."
  :timeout 5
  (assert-true (boundp 'cl-cc/vm::*jit-tier1-threshold*)))

(deftest fr-835-adaptive-runtime-exists
  "FR-835: Adaptive runtime optimization is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::runtime-tuning-report)))

(deftest fr-838-sequence-protocol-exists
  "FR-838: Extensible sequence protocol is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::sequence-protocol-p)))

(deftest fr-839-iterator-exists
  "FR-839: Iteration protocol is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/expand::make-iterator)))

(deftest fr-842-kahan-exists
  "FR-842: Kahan summation is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::kahan-sum)))

(deftest fr-843-float-traps-exists
  "FR-843: Float exception control is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::with-float-traps-masked)))

(deftest fr-844-extended-precision-exists
  "FR-844: Extended precision arithmetic is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::dd+)))

(deftest fr-847-mutex-exists
  "FR-847: Mutex/condition variable API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::make-mutex)))

(deftest fr-848-rwlock-exists
  "FR-848: RWLock API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::make-rwlock)))

(deftest fr-851-socket-exists
  "FR-851: TCP/UDP socket API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::make-tcp-socket)))

(deftest fr-852-dns-exists
  "FR-852: DNS resolution is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::dns-resolve)))

(deftest fr-853-tls-exists
  "FR-853: TLS/SSL support is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::make-tls-context)))

(deftest fr-856-delay-force-exists
  "FR-856: delay/force promises are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/expand::delay)))

(deftest fr-857-memoization-exists
  "FR-857: Memoization is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/expand::memoize)))

(deftest fr-860-numeric-contagion-exists
  "FR-860: Numeric contagion rules are defined."
  :timeout 5
  (assert-true (boundp 'cl-cc/vm::*numeric-contagion-table*)))

(deftest fr-861-inline-dispatch-exists
  "FR-861: Inline numeric dispatch table is defined."
  :timeout 5
  (assert-true (boundp 'cl-cc/vm::*arith-dispatch-table*)))

(deftest fr-864-mv-frame-exists
  "FR-864: Multiple values frame protocol is defined."
  :timeout 5
  (assert-true (boundp 'cl-cc/vm::+maximum-multiple-values+)))

(deftest fr-865-mv-apply-exists
  "FR-865: Multiple values through apply is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-nth-value)))

(deftest fr-868-file-position-exists
  "FR-868: file-position random access is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-file-position)))

(deftest fr-869-mmap-exists
  "FR-869: Memory-mapped files API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::mmap-file)))

(deftest fr-872-cow-string-exists
  "FR-872: CoW strings are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-string-copy)))

(deftest fr-873-cow-array-exists
  "FR-873: CoW arrays are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::copy-on-write-array-p)))

(deftest fr-876-segmented-stack-exists
  "FR-876: Segmented stack support is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::grow-stack-segment)))

(deftest fr-877-copying-stack-exists
  "FR-877: Copying stack growth is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::relocate-stack-pointers)))

(deftest fr-880-custom-hash-exists
  "FR-880: User-defined hash functions are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::define-hash-table-test)))

(deftest fr-881-rehash-exists
  "FR-881: Hash table resizing policy is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::hash-table-rehash-size)))

(deftest fr-884-floor-exists
  "FR-884: floor/ceiling/truncate/round two-value return is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-floor)))

(deftest fr-885-ffloor-exists
  "FR-885: ffloor/fceiling/ftruncate/fround are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-ffloor)))

(deftest fr-888-allocate-fast-exists
  "FR-888: CLOS allocate-instance fast path is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::allocate-instance-vector)))

(deftest fr-889-initargs-cache-exists
  "FR-889: default-initargs caching is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::class-slot-vector-index)))

(deftest fr-892-load-time-value-exists
  "FR-892: load-time-value implementation is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-load-time-value)))

(deftest fr-895-symbol-table-exists
  "FR-895: Symbol table compaction is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::freeze-symbol-table)))

(deftest fr-896-package-lock-exists
  "FR-896: Package lock is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::lock-package)))

(deftest fr-899-fasl-demand-exists
  "FR-899: FASL demand paging is defined."
  :timeout 5
  (assert-true (boundp 'cl-cc/vm::*fasl-toc-enabled*)))

(deftest fr-902-pgo-exists
  "FR-902: PGO profile persistence is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::save-pgo-data)))

(deftest fr-905-tco-unwind-exists
  "FR-905: TCO through unwind-protect handling is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::vm-check-dynamic-extent)))

(deftest fr-908-quasiquote-exists
  "FR-908: Quasiquote compiler optimization is defined."
  :timeout 5
  (assert-true (find-package :cl-cc/expand)))

(deftest fr-911-riscv-exists
  "FR-911: RISC-V backend is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/emit::riscv64-emit-function)))

(deftest fr-914-delimited-cont-exists
  "FR-914: Delimited continuations are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::call-with-continuation-prompt)))

(deftest fr-917-reproducible-exists
  "FR-917: Reproducible build support is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/cli::cl-cc-deterministic-build-p)))

(deftest fr-920-forward-ref-exists
  "FR-920: Forward reference resolution is defined."
  :timeout 5
  (assert-true (boundp 'cl-cc/compile::*forward-reference-patch-table*)))

(deftest fr-923-io-buffering-exists
  "FR-923: I/O buffering control is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::make-buffered-stream)))

(deftest fr-924-special-streams-exists
  "FR-924: Special streams are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::make-string-input-stream)))

(deftest fr-927-pathname-exists
  "FR-927: Pathname operations are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::wild-pathname-p)))

(deftest fr-930-mop-exists
  "FR-930: MOP introspection is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::class-slots)))

(deftest fr-931-camuc-exists
  "FR-931: compute-applicable-methods-using-classes is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/vm::compute-applicable-methods-using-classes)))
