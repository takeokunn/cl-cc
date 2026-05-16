# CL-CC Implementation Roadmap

## ANSI CL Compliance

- [ansi-cl-lang.md](ansi-cl-lang.md) — Language Core: eval, lambda, types, CLOS, conditions, symbols, packages, numbers (720 FR) — ✅ 698 / — 22 (MOP/SBCL-ext)
- [ansi-cl-stdlib.md](ansi-cl-stdlib.md) — Standard Library: cons, arrays, strings, sequences, hash, I/O, printer, reader (352 FR) — ✅ COMPLETE (352 / 🔶 0 / ❌ 0)

## Type System

- [type-core.md](type-core.md) — Core: inference, subtyping (Ch.1-3) — ✅ COMPLETE (12 / 12 direct FR; Ch.2,4-14 are bridge refs)
- [type-advanced.md](type-advanced.md) — Advanced: safety types, type-level programming, proofs, concurrency types (Ch.15-34) — ✅ COMPLETE (109 / 109 FR marked ✅)
  - 検証根拠: `type-core.md` は直接 FR 見出し 12/12 が ✅。`type-advanced.md` は FR 見出し 109/109 が ✅ で、`packages/type/tests/type-2026-nodes-tests.lisp` が docs FR リスト・semantic contract registry・implementation evidence registry の一致を検証します。

## Optimization

- [optimize-passes.md](optimize-passes.md) — Core Passes, CPS/SSA & Speculative JIT (175 FR)
- [optimize-backend.md](optimize-backend.md) — Analysis & Backend: partial eval, PGO, LTO, JIT, ABI (232 FR) — ✅ COMPLETE (232 / 232 FR)

## Native Backend

- [native-codegen.md](native-codegen.md) — Code Generation: regalloc, instruction scheduling, SIMD, MIR (83 FR)
- [native-advanced.md](native-advanced.md) — Advanced: LTO, staged compilation, security, GC integration, modern ISA (147 FR)

## Runtime

- [runtime-core.md](runtime-core.md) — Core: Lisp runtime optimization, data structures, collections (52 FR)
- [runtime-subsystem.md](runtime-subsystem.md) — Subsystems: inline caches, safepoints, FFI, concurrency (170 FR)
- [runtime-stdlib-1.md](runtime-stdlib-1.md) — Stdlib I: lambda lists, numeric I/O, regex, source location (75 FR)
- [runtime-stdlib-2.md](runtime-stdlib-2.md) — Stdlib II: serialization, debugger, memory-mapped I/O, crypto (71 FR)
- [runtime-stdlib-3.md](runtime-stdlib-3.md) — Stdlib III: ANSI compliance, AOT, persistent data structures (73 FR)

## Memory & GC

- [memory-gc.md](memory-gc.md) — Generational GC, compaction, pinning, concurrent GC (166 FR)

## Tooling

- [tooling-compiler.md](tooling-compiler.md) — Compiler Infrastructure: frontend, binary/link, security (38 FR)
- [tooling-debug.md](tooling-debug.md) — Debug & Diagnostics: profiling, DX, coverage, hot reload (53 FR)
- [tooling-advanced-1.md](tooling-advanced-1.md) — Advanced I: ML optimization, WASM, CHERI, LSP (136 FR)
- [tooling-advanced-2.md](tooling-advanced-2.md) — Advanced II: JIT, object layout, linker, FFI (120 FR)
- [tooling-advanced-3.md](tooling-advanced-3.md) — Advanced III: GC, pattern matching, SIMD, REPL (128 FR)

## WebAssembly

- [wasm.md](wasm.md) — WASM backend, typed references, GC integration (130 FR)

---

**Status: 1677/1798 tracked FRs complete (93%) | Self-hosting verified explicitly by `selfhost-suite` via `:cl-cc-test/e2e` | Test counts vary by entrypoint and should be verified from the current run artifacts**
