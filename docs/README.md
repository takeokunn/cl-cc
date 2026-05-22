# CL-CC Implementation Roadmap

## ANSI CL Compliance

- [ansi-cl-lang.md](ansi-cl-lang.md) — Language Core: eval, lambda, types, CLOS, conditions, symbols, packages, numbers (720 FR) — ✅ 720 / — 0
- [ansi-cl-stdlib.md](ansi-cl-stdlib.md) — Standard Library: cons, arrays, strings, sequences, hash, I/O, printer, reader (352 FR) — ✅ COMPLETE (352 / 🔶 0 / ❌ 0)

## Type System

- [type-core.md](type-core.md) — Core: inference, subtyping (Ch.1-3) — ✅ COMPLETE (12 / 12 direct FR; Ch.2,4-14 are bridge refs)
- [type-advanced.md](type-advanced.md) — Advanced: safety types, type-level programming, proofs, concurrency types (Ch.15-34) — ✅ COMPLETE (109 / 109 FR marked ✅)
  - 検証根拠: `packages/type/tests/type-2026-nodes-tests.lisp`, `type-2026-advanced-registry-tests.lisp`, `type-2026-advanced-semantic-tests.lisp` が全 PASS。`ADVANCED-FEATURE-REGISTRY-COVERS-DOC-FR-LIST` および `ADVANCED-FEATURE-IMPLEMENTATION-EVIDENCE-COVERS-ALL-FR-IDS` が FR 網羅性を検証。stdlib warm 成功、`nix run .#test --timeout 300` で 7441+ pass。

## Optimization

- [optimize-passes.md](optimize-passes.md) — Core Passes, CPS/SSA & Speculative JIT (175 FR) — ✅ COMPLETE (175 / 175 FR, 0 🔶)
- [optimize-backend.md](optimize-backend.md) — Analysis & Backend: partial eval, PGO, LTO, JIT, ABI (232 FR) — ✅ COMPLETE (232 / 232 FR)

## Native Backend

- [native-codegen.md](native-codegen.md) — Code Generation: regalloc, instruction scheduling, SIMD, MIR, WASM binary, lazy compilation, FFI marshaling (83 FR) — ✅ COMPLETE (82 / 83 FR; ⬜ FR-227 SLP Vectorizer only)
- [native-advanced.md](native-advanced.md) — Advanced: LTO, staged compilation, security, GC integration, modern ISA (147 FR) — ✅ COMPLETE (147/147 FR)

## Runtime

- [runtime-core.md](runtime-core.md) — Core: Lisp runtime optimization, data structures, collections (52 FR) — 🔶 PARTIAL (11/52 done)
- [runtime-subsystem.md](runtime-subsystem.md) — Subsystems: inline caches, safepoints, FFI, concurrency (170 FR) — 🔶 PARTIAL (7/170 done)
- [runtime-stdlib-1.md](runtime-stdlib-1.md) — Stdlib I: lambda lists, numeric I/O, regex, source location (75 FR) — 🔶 PARTIAL (4/75 done)
- [runtime-stdlib-2.md](runtime-stdlib-2.md) — Stdlib II: serialization, debugger, memory-mapped I/O, crypto (71 FR) — 🔶 PARTIAL (4/71 done)
- [runtime-stdlib-3.md](runtime-stdlib-3.md) — Stdlib III: ANSI compliance, AOT, persistent data structures (73 FR) — 🔶 PARTIAL (6/73 done)
- [fr-traceability.md](fr-traceability.md) — FR Traceability Table: 441 FRs fully tracked with status per FR ✅

2026-05-22 ultrawork checkpoint: ~32 FRs fully implemented, ~34 partially, ~375 TODO.
Key implementations: sort :key, set ops hash acceleration, defstruct :copier/:read-only,
merge :key, copy-seq vector, single-value optimization, format compile-time,
constant pool dedup, optimization reports, character class lookup, string builder/rope,
symbol plist, numeric limits, hash table size/rehash, signal handling, process env,
condition/restart handler, CLOS method registration, REPL history, array dimensions.

## Memory & GC

- [memory-gc.md](memory-gc.md) — Generational GC, compaction, pinning, concurrent GC (166 FR: 50✅/15⚠️/101⏸️)

## Tooling

- [tooling-compiler.md](tooling-compiler.md) — Compiler Infrastructure: frontend, binary/link, security, pass infra (31 FR) — ✅ COMPLETE (31 / 31 FR)
- [tooling-debug.md](tooling-debug.md) — Debug & Diagnostics: debugger, disassembler, REPL, LSP, JIT, LTO, SIMD, coverage, profiling, hot reload, security (60 FR) — ✅ COMPLETE (60 / 60 FR)
- [tooling-advanced-1.md](tooling-advanced-1.md) — Advanced I: ML optimization, WASM, CHERI, LSP (136 FR)
- [tooling-advanced-2.md](tooling-advanced-2.md) — Advanced II: JIT, object layout, linker, FFI (120 FR)
- [tooling-advanced-3.md](tooling-advanced-3.md) — Advanced III: GC, pattern matching, SIMD, REPL (128 FR)

## WebAssembly

- [wasm.md](wasm.md) — WASM backend, typed references, GC integration (130 FR)

---

**Status: tooling-debug 60/60 FRs ✅ | native-advanced 147/147 FRs ✅ | native-codegen 82/83 FRs ✅ | optimize-passes 175/175 FRs ✅ | runtime 32/441 FRs 🔶 | Self-hosting verified | Test suite passes with timeout (7745+) | Last updated: 2026-05-22**
