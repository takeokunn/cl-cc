;;;; packages/vm/tests/runtime-stdlib-2-vm-tests.lisp
;;;; FR-787–FR-931: Symbol-existence smoke tests for runtime-stdlib-2 features.
;;;;
;;;; Each test asserts that the listed symbol is fboundp / boundp / a loadable package.
;;;; Groups are by check type; FR labels are encoded in the case label strings.

(in-package :cl-cc/test)

(defsuite runtime-stdlib-2-vm-suite
  :description "Runtime-stdlib-2 VM feature verification tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-2-vm-suite)

(deftest runtime-stdlib-2-vm-system-loads
  "The VM system is loadable with runtime-stdlib-2 features."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-vm nil)))

;;; ─── Multi-symbol FR groups (cannot collapse to single-entry rows) ─────────

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

(deftest fr-804-syntax-rules-exists
  "FR-804: syntax-rules hygienic macros are defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/expand::define-syntax))
  (assert-true (fboundp 'cl-cc/expand::with-gensyms)))

(deftest fr-812-c-embedding-exists
  "FR-812: C Embedding API is defined."
  :timeout 5
  (assert-true (fboundp 'cl-cc/runtime::cl-cc-init))
  (assert-true (fboundp 'cl-cc/runtime::cl-cc-eval))
  (assert-true (fboundp 'cl-cc/runtime::cl-cc-call)))

;;; ─── Package existence ───────────────────────────────────────────────────────

(deftest-each fr-package-exists
  "FR feature packages are loadable."
  :cases (("fr-796-lsp"     :cl-cc/tools/lsp)
          ("fr-797-dap"     :cl-cc/tools/dap)
          ("fr-908-expand"  :cl-cc/expand))
  (pkg)
  (assert-true (find-package pkg)))

;;; ─── Single-function fboundp checks ─────────────────────────────────────────

(deftest-each fr-function-exists
  "Each FR feature exports exactly the named function."
  :cases (("fr-800-callcc"          'cl-cc/vm::call-with-current-continuation)
          ("fr-801-escape"           'cl-cc/vm::vm-call/cc)
          ("fr-805-once-only"        'cl-cc/expand::once-only)
          ("fr-808-shebang"          'cl-cc/cli::parse-cli-args)
          ("fr-809-argv"             'cl-cc/cli::cl-cc-argv)
          ("fr-813-multi-vm"         'cl-cc/vm::make-vm-instance)
          ("fr-816-arena"            'cl-cc/runtime::make-arena)
          ("fr-817-object-pool"      'cl-cc/runtime::make-object-pool)
          ("fr-821-copy-structure"   'cl-cc/vm::copy-structure)
          ("fr-824-transients"       'cl-cc/vm::transient)
          ("fr-828-stack-canary"     'cl-cc/vm::vm-stack-canary-check)
          ("fr-829-overflow"         'cl-cc/vm::vm-check-fixnum-overflow)
          ("fr-830-taint"            'cl-cc/vm::taint-mark)
          ("fr-835-adaptive-runtime" 'cl-cc/vm::runtime-tuning-report)
          ("fr-838-sequence-proto"   'cl-cc/vm::sequence-protocol-p)
          ("fr-839-iterator"         'cl-cc/expand::make-iterator)
          ("fr-842-kahan"            'cl-cc/vm::kahan-sum)
          ("fr-843-float-traps"      'cl-cc/vm::with-float-traps-masked)
          ("fr-844-extended-prec"    'cl-cc/vm::dd+)
          ("fr-847-mutex"            'cl-cc/vm::make-mutex)
          ("fr-848-rwlock"           'cl-cc/vm::make-rwlock)
          ("fr-851-socket"           'cl-cc/vm::make-tcp-socket)
          ("fr-852-dns"              'cl-cc/vm::dns-resolve)
          ("fr-853-tls"              'cl-cc/vm::make-tls-context)
          ("fr-856-delay"            'cl-cc/expand::delay)
          ("fr-857-memoize"          'cl-cc/expand::memoize)
          ("fr-865-mv-apply"         'cl-cc/vm::vm-nth-value)
          ("fr-868-file-position"    'cl-cc/vm::vm-file-position)
          ("fr-869-mmap"             'cl-cc/runtime::mmap-file)
          ("fr-872-cow-string"       'cl-cc/vm::vm-string-copy)
          ("fr-873-cow-array"        'cl-cc/vm::copy-on-write-array-p)
          ("fr-876-segmented-stack"  'cl-cc/runtime::grow-stack-segment)
          ("fr-877-copying-stack"    'cl-cc/runtime::relocate-stack-pointers)
          ("fr-880-custom-hash"      'cl-cc/vm::define-hash-table-test)
          ("fr-881-rehash"           'cl-cc/vm::hash-table-rehash-size)
          ("fr-884-floor"            'cl-cc/vm::vm-floor)
          ("fr-885-ffloor"           'cl-cc/vm::vm-ffloor)
          ("fr-888-allocate-fast"    'cl-cc/vm::allocate-instance-vector)
          ("fr-889-initargs-cache"   'cl-cc/vm::class-slot-vector-index)
          ("fr-892-load-time-value"  'cl-cc/vm::vm-load-time-value)
          ("fr-895-symbol-table"     'cl-cc/vm::freeze-symbol-table)
          ("fr-896-package-lock"     'cl-cc/vm::lock-package)
          ("fr-902-pgo"              'cl-cc/vm::save-pgo-data)
          ("fr-905-tco-unwind"       'cl-cc/vm::vm-check-dynamic-extent)
          ("fr-911-riscv"            'cl-cc/emit::riscv64-emit-function)
          ("fr-914-delimited-cont"   'cl-cc/vm::call-with-continuation-prompt)
          ("fr-917-reproducible"     'cl-cc/cli::cl-cc-deterministic-build-p)
          ("fr-923-io-buffering"     'cl-cc/vm::make-buffered-stream)
          ("fr-924-special-streams"  'cl-cc/vm::make-string-input-stream)
          ("fr-927-pathname"         'cl-cc/vm::wild-pathname-p)
          ("fr-930-mop"              'cl-cc/vm::class-slots)
          ("fr-931-camuc"            'cl-cc/vm::compute-applicable-methods-using-classes))
  (sym)
  (assert-true (fboundp sym)))

;;; ─── Single-variable boundp checks ──────────────────────────────────────────

(deftest-each fr-variable-exists
  "Each FR feature binds the named special variable."
  :cases (("fr-820-print-circle"      'cl-cc/vm::*print-circle*)
          ("fr-827-bounds-check"      'cl-cc/vm::*safety-level*)
          ("fr-833-gc-tuning"         'cl-cc/runtime::*gc-nursery-size*)
          ("fr-834-jit-thresholds"    'cl-cc/vm::*jit-tier1-threshold*)
          ("fr-860-numeric-contagion" 'cl-cc/vm::*numeric-contagion-table*)
          ("fr-861-inline-dispatch"   'cl-cc/vm::*arith-dispatch-table*)
          ("fr-864-mv-frame"          'cl-cc/vm::+maximum-multiple-values+)
          ("fr-899-fasl-demand"       'cl-cc/vm::*fasl-toc-enabled*)
          ("fr-920-forward-ref"       'cl-cc/compile::*forward-reference-patch-table*))
  (sym)
  (assert-true (boundp sym)))
