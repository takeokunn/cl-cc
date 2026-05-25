# Changelog

All notable changes to cl-cc will be documented in this file.

## [0.1.0] — Initial Release

### Compiler Core
- Hand-written incremental lexer and CST parser
- Macro expander: `defstruct`→`defclass`, `defconstant`→`defparameter`, standard macros
- CLOS-based AST definitions
- Continuation-Passing Style (CPS) transform as preferred lowering path
- Register-based VM bytecode code generation (~220 instruction types)
- VM interpreter with meta-circular `eval` support
- MIR/SSA construction (Braun et al. 2013) for native backends
- Native code emission: x86-64 (Mach-O), AArch64, WebAssembly (wasm32)

### Type System
- Hindley-Milner type inference (Algorithm W) with error-sentinel recovery
- Union type narrowing on conditionals
- Parametric types: `(List T)`, `(Option T)`
- Partial typeclass implementation

### Optimizations
- Multi-pass optimizer: CSE, constant folding, copy propagation
- Dead code elimination, strength reduction, jump threading
- Prolog-backed peephole rewrite rules and e-graph rule discovery
- E-graph equality saturation integrated into the main optimizer pipeline
- CFG construction with SSA form
- Register allocation (ML-based + linear scan)

### Runtime
- 2-generation GC: Young (Cheney semi-space) + Old (tri-color mark-sweep)
- SATB write barrier with TLAB allocation
- GC safepoint infrastructure with precise stack maps
- `storage-condition` with heap pressure warnings (80/90/95%)
- Stack overflow guard (`*max-call-stack-depth*`)

### Runtime Subsystems
- **Inline Caches**: monomorphic, polymorphic, megamorphic call site dispatch
- **Type Feedback Vector (TFV)**: runtime type profiling for Tier-1 compilation
- **Concurrency**: green threads (work-stealing), CSP channels, actor model, STM, futures/promises, structured task groups, fibers
- **Memory Reclamation**: EBR, Hazard Pointers, RCU, QSBR, MVCC
- **Lock-Free Structures**: stack, queue, hash map, SPSC ring buffer
- **Synchronization**: mutex, RWLock, semaphore, condition variable, barrier
- **OS Layer**: file I/O, process control, signals, mmap, sockets (TCP/UDP), io_uring stubs, event loop
- **FFI**: foreign function calling, callback trampolines, native struct layout
- **Image**: heap snapshot save/restore with magic/version/CRC32 verification
- **Distributed**: Raft consensus, CRDTs (GCounter, PNCounter, LWWRegister)
- **Observability**: OpenTelemetry spans, performance counters, structured logging, vector clocks, deadlock detector
- **Atomic Operations**: CAS, swap, load, store, incf, memory barriers

### Language Support
- Full ANSI CL special forms (`if`, `progn`, `block`/`return-from`, `tagbody`/`go`, `catch`/`throw`, `unwind-protect`, `let`/`let*`, `flet`/`labels`, `setq`/`setf`, `lambda`/`defun`, `defvar`/`defparameter`, `defmacro`/`macrolet`, `quote`/`the`/`values`, `multiple-value-bind`/`multiple-value-call`, `eval-when`)
- Full CLOS: `defclass`, `defgeneric`, `defmethod`, multiple dispatch, `call-next-method`, inheritance chains
- Closures with lexical variable capture and mutation
- Conditions & error handling: `handler-case`, `ignore-errors`, `error`
- `defstruct` expanding to `defclass` + constructor + predicate
- `loop` macro
- Standard library: lists, sequences, strings, characters, hash tables, arrays, numbers, I/O, pretty printer, streams, Unicode (Latin-1 NFC/NFD), time, random (MT19937), environment queries

### Self-Hosting
- Meta-circular `eval`: `(eval '...)` calls cl-cc compiler pipeline, not host SBCL
- Macro expansion via `our-eval` dispatches to cl-cc bytecode
- Compiler-in-the-compiler: can compile programs that implement parsers, compilers, and evaluators
- Quasiquote in compiled code
- REPL with persistent function, class, and accessor definitions across forms

### CLI
- `cl-cc run <file>` — compile and run
- `cl-cc compile <file>` — compile to native binary (x86-64/AArch64)
- `cl-cc eval "<expr>"` — evaluate expression
- `cl-cc repl` — interactive REPL
- `cl-cc check <file>` — type-check without executing
- `cl-cc selfhost [file]` — self-hosting workload

### Build System
- Nix Flakes with flake-parts for reproducible builds
- ASDF umbrella system with 27 subsystem packages
- `nix develop` / `nix build` / `nix run .#test` / `nix flake check`
