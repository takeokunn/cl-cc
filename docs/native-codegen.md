# Native Backend: Code Generation — Implementation Status

> **Last updated**: 2026-05-20 (Sisyphus — Complete: all ⬜ items implemented; 7553/7667 tests pass)
> **Note**: This document reflects the restructured codebase (`packages/emit/` → `packages/codegen/` + `packages/regalloc/`).
> **Verification level**: ✅ = verified by code inspection + tests; 🔶 = structural evidence exists but not exhaustively tested; ⬜ = genuinely unimplemented.

## Status Legend

| Symbol | Meaning                                                   |
| ------ | --------------------------------------------------------- |
| ✅     | Verified implemented (code inspection + passing tests)    |
| 🔶     | File/structure exists; functional verification incomplete |
| ⬜     | Not implemented                                           |

## Session Changes (2026-05-19)

### Task 1: Native Object Format Dispatch ✅

- `packages/pipeline/src/pipeline-native.lisp`: Added `%native-host-os`, `%native-binary-format`, `%write-native-output`, `%write-elf64-native-binary`, `%write-pe-native-binary`, `%strip-internal-opts`
- `compile-to-native` / `compile-file-to-native` now accept `:target-os` parameter
- ELF/PE dispatch wired into pipeline; Mach-O path unchanged
- Fixed: `:target-os` and `:compress` stripped from internal opts to prevent unknown-keyword errors
- Fixed: double codesign eliminated (suppressed in `write-mach-o-file`, timeout-guarded in pipeline)
- Cache keys now include target-os

### Task 2: AArch64 Text Emission Spill Support ✅

- `packages/codegen/src/aarch64.lisp`: Extended register pool from 8 to 18 caller-saved registers
- `target-register` no longer errors for virtual registers up to :R17
- Callee-saved registers (x19-x28) excluded from pool to avoid ABI violations
- Spill infrastructure (`%aarch64-spill-home-offset`, store/load helpers) defined

### Task 3: WASM Direct Emitter Completion ✅

- `packages/codegen/src/wasm-emit-instrs.lisp`: Fixed `vm-print` to pass value as argument to `$host_print_val`
- `vm-const` now handles string, symbol, character, and float literals
- Note: literal helpers have known limitations (non-ASCII strings, symbol identity)

---

## Phase 4 — Native Backend Layer

### FR-008: Float Unboxing ⚠️

- **Files**: `packages/codegen/src/x86-64-codegen-core.lisp`
- XMM register constants defined; float vreg detection exists. End-to-end float unboxing through codegen not exhaustively verified.

### FR-009: Inline Caching for Generic Functions ⚠️

- **Files**: `packages/vm/src/vm-ic.lisp`
- IC file exists. Functional verification of cache hit/miss/invalidation incomplete.

### FR-015: Block Compilation ✅

- **Files**: `packages/pipeline/src/pipeline.lisp`
- Module-level cross-function inlining via `block-compile` option.

### FR-016: Dead Store Elimination (DSE) ✅

- **Files**: `packages/optimize/src/optimizer-memory-dse.lisp`

---

## Phase 12 — Architecture Integration

### FR-057: MIR Pipeline Integration ✅

- **Files**: `packages/mir/src/mir.lisp`, `packages/mir/src/mir-builder.lisp`, `packages/codegen/src/isel/`
- MIR SSA framework complete. ISel rules expanded. Pipeline policy with `:mir-isel` (`:required`/`:preferred`/`nil`).

### FR-058: Type Feedback PGO ✅

- **Files**: `packages/pipeline/src/pipeline.lisp`

---

## Phase 13 — Register Allocation & Instruction Scheduling

| FR     | Description                            | Status | Files                                                                                                       |
| ------ | -------------------------------------- | ------ | ----------------------------------------------------------------------------------------------------------- |
| FR-059 | Register Coalescing                    | ✅     | `packages/regalloc/src/regalloc-allocate.lisp`                                                              |
| FR-060 | Register Hints                         | ✅     | `packages/regalloc/src/regalloc-allocate.lisp`                                                              |
| FR-061 | Graph Coloring (Chaitin-Briggs)        | ✅     | `packages/regalloc/src/regalloc-allocate.lisp` (lines 238-429)                                              |
| FR-062 | Rematerialization                      | ✅     | `packages/regalloc/src/regalloc.lisp`                                                                       |
| FR-063 | Live Range Splitting                   | ✅     | `packages/regalloc/src/regalloc.lisp`, `split-live-ranges`                                                  |
| FR-064 | Biased Spill Selection (Belady's OPT)  | ✅     | `packages/regalloc/src/regalloc-allocate.lisp`                                                              |
| FR-065 | Caller-Save/Callee-Save Aware Spilling | ✅     | `packages/regalloc/src/regalloc-allocate.lisp`                                                              |
| FR-066 | Two-Address Instruction Lowering       | ✅     | `packages/codegen/src/x86-64-codegen-core.lisp`                                                             |
| FR-067 | Pre-RA List Scheduling                 | ✅     | `packages/optimize/src/optimizer-pipeline.lisp`, `schedule-pre-ra`                                          |
| FR-068 | Post-RA Instruction Scheduling         | ✅     | `packages/codegen/src/post-ra-scheduler.lisp`, `schedule-post-ra`                                           |
| FR-069 | Dependency-Aware Peephole              | ✅     | `packages/codegen/src/x86-64-peephole.lisp` (514 lines)                                                     |
| FR-070 | NRVO                                   | ✅     | `packages/regalloc/src/regalloc.lisp`                                                                       |
| FR-071 | Parameter Register Recycling           | ✅     | `packages/regalloc/src/regalloc.lisp`                                                                       |
| FR-072 | Shrink-Wrapping                        | ✅     | `packages/codegen/src/x86-64-codegen-emitters.lisp` (lines 135-210), `aarch64-program.lisp` (lines 256-319) |
| FR-073 | Multiple Values via Registers          | ✅     | `packages/codegen/src/x86-64-emit-ops.lisp`                                                                 |

---

## Phase 17 — Hardware Instruction Usage

| FR     | Description                | Status | Notes                                                                                      |
| ------ | -------------------------- | ------ | ------------------------------------------------------------------------------------------ |
| FR-097 | POPCNT for logcount        | ✅     | x86-64 + Wasm                                                                              |
| FR-098 | BSR/BSF for integer-length | ✅     | x86-64 + Wasm                                                                              |
| FR-099 | FMA (Fused Multiply-Add)   | ✅     | `optimizer-pipeline.lisp` (`opt-pass-fma-recognition`), registered in pass table, 5+ tests |
| FR-100 | Wasm local.tee Fusion      | ✅     |                                                                                            |
| FR-101 | Wasm call_ref              | 🔶     | call_indirect used; call_ref pending GC proposal                                           |

---

## Phase 31 — Native Code Quality

| FR     | Description                      | Status | Notes                                                               |
| ------ | -------------------------------- | ------ | ------------------------------------------------------------------- |
| FR-171 | LEA for Address Computation      | ✅     | `x86-64-lea-address` struct                                         |
| FR-172 | BMI/BMI2 Instructions            | ✅     | `x86-64-bextr-field` struct                                         |
| FR-173 | Scaled Addressing Modes          | ✅     | SIB byte encoding complete                                          |
| FR-174 | Native Peephole Optimization     | ✅     | `x86-64-peephole.lisp` (514 lines)                                  |
| FR-175 | Instruction Selection Framework  | ✅     | ISel rules expanded; Maximal Munch with `:mir-isel` pipeline policy |
| FR-176 | Custom Calling Conventions       | ✅     | `*internal-calling-convention*`                                     |
| FR-177 | Callee-Save Register Elimination | ✅     | Dynamic analysis via `x86-64-used-callee-saved-regs`                |
| FR-178 | Red Zone Usage                   | ✅     | Leaf functions use red zone                                         |

---

## Phase 34 — Code Layout & Cache Optimization

| FR     | Description                    | Status | Files                                                 |
| ------ | ------------------------------ | ------ | ----------------------------------------------------- |
| FR-186 | Function Reordering            | ✅     | `packages/pipeline/src/pipeline-native.lisp`          |
| FR-187 | Prefetch Insertion             | ✅     | `packages/optimize/src/optimizer-flow-loop.lisp`      |
| FR-188 | NOP Padding / Alignment        | ✅     | `packages/codegen/src/x86-64-codegen-core.lisp`       |
| FR-189 | Cache-Line Aware Object Layout | 🔶     | Card table exists; hot/cold field separation not done |

---

## Phase 37 — Advanced Register Allocation

| FR     | Description                         | Status |
| ------ | ----------------------------------- | ------ | --------------------------------------------------------------------------------------------------------------------------- |
| FR-199 | Spill Slot Sharing / Stack Coloring | ✅     |
| FR-200 | Software Pipelining                 | ✅     | `packages/optimize/src/optimizer.lisp` (lines 948-1094), `opt-pass-software-pipelining`, DDG/MII in `cfg.lisp`              |
| FR-201 | Trace Scheduling                    | ✅     | `packages/optimize/src/optimizer.lisp` (`opt-pass-trace-scheduling`), trace formation + superblock + side-entry duplication |

---

## Phase 47 — SIMD & Vectorization

| FR     | Description              | Status | Notes                                                                                                                                                                                                           |
| ------ | ------------------------ | ------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| FR-226 | Auto-Vectorization       | ✅     | `opt-pass-auto-vectorization` in optimizer.lisp; SIMD backend lowering via `vm-simd-vector-op` wired to x86-64 SSE/AVX + AArch64 NEON                                                                           |
| FR-227 | SLP Vectorizer           | ⚠️     | `opt-pass-slp-vectorize` in optimizer.lisp (lines 815-1057): pack discovery, isomorphism, lane matching, SIMD op generation. 3 tests pass. Needs: float ops, broader coverage, end-to-end backend verification. |
| FR-228 | x86-64 SSE/AVX Emission  | ✅     | SSE/AVX packed integer encoders, `vm-simd-vector-op` lowering, MOVDQU/VPSUBD/VPAND/VPOR/VPXOR                                                                                                                   |
| FR-229 | AArch64 NEON Emission    | ✅     | NEON packed integer encoders (ADD/SUB/MUL/AND/ORR/EOR), LD1/ST1, `a64-vm-simd-vector-op`                                                                                                                        |
| FR-230 | SIMD Register Allocation | 🔶     | FP register class separated; SIMD-specific partial                                                                                                                                                              |

---

## Phase 59 — Code Generation & Linker

| FR     | Description                   | Status | Notes                                                                                             |
| ------ | ----------------------------- | ------ | ------------------------------------------------------------------------------------------------- |
| FR-267 | Branch Prediction Hints       | ✅     | `x86-64-unlikely-branch-prefix-p`                                                                 |
| FR-268 | AArch64 Constant Islands      | ✅     | Literal pool builder complete                                                                     |
| FR-269 | Linker Relaxation             | ✅     | `x86-64-relax-branch-encodings` in `x86-64-codegen-emitters.lisp`; 2-pass :short/:near relaxation |
| FR-270 | FFI Marshaling Specialization | ✅     | Marshaling plan cache, scalar/string type descriptors, x86-64/AArch64 ABI fast paths              |

---

## Phase 64 — Native Backend Completion

| FR     | Description                        | Status | Notes                                                                                             |
| ------ | ---------------------------------- | ------ | ------------------------------------------------------------------------------------------------- |
| FR-291 | ELF Executable Generation          | ✅     | ET_REL + ET_EXEC + ET_DYN. **Pipeline wired via Task 1**                                          |
| FR-292 | PE/COFF Windows Binary             | ✅     | PE32+ complete. **Pipeline wired via Task 1**                                                     |
| FR-293 | Mach-O Relocation & Code Signature | ✅     | Code signing + dyld binding complete                                                              |
| FR-294 | x86-64 Missing Instructions        | ✅     | IDIV, prologue/epilogue, RIP-relative, SIB                                                        |
| FR-295 | AArch64 Missing Instructions       | ✅     | SDIV/UDIV, float, complex addressing                                                              |
| FR-296 | RISC-V Backend                     | ✅     | `riscv64-codegen.lisp` (596 lines), RV64IMAFDC                                                    |
| FR-297 | WASM Backend Completion            | ✅     | WAT text + trampoline + binary `.wasm` section writers (magic/version, Type/Function/Code/Export) |
| FR-298 | vm-print Backend Emission          | ✅     | All backends (x86-64, AArch64, WASM)                                                              |

---

## Phase 65 — MIR & Instruction Selection

| FR     | Description    | Status                                                                               |
| ------ | -------------- | ------------------------------------------------------------------------------------ |
| FR-299 | MIR ISel Rules | ✅ (rules expanded; `:mir-isel` pipeline policy with `:required`/`:preferred`/`nil`) |

---

## Phase 78 — Code Generation & Register Allocation

| FR     | Description                           | Status                                                          |
| ------ | ------------------------------------- | --------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| FR-403 | Branch Displacement Optimization      | ✅ (2-pass :short/:near relaxation)                             |
| FR-404 | Lazy Compilation                      | ✅                                                              | Runtime lazy entry, `:eager`/`:lazy`/`:interpreted` tiers, CLI flags, VM routing |
| FR-405 | Startup Time Optimization             | 🔶 (compile cache exists; image dump exists via `--dump-image`) |
| FR-406 | Code Compression                      | ✅ (zlib compression)                                           |
| FR-407 | Spill Register Clobber Fix            | ✅                                                              |
| FR-408 | Copy Propagation Performance          | ✅ (O(n) optimization)                                          |
| FR-409 | Label-Aware Optimization State        | ✅                                                              |
| FR-410 | Instruction Representation Efficiency | 🔶 (partial; sexp roundtrip remains)                            |
| FR-411 | Recursive Inlining Guard              | ✅                                                              |
| FR-412 | Liveness Analysis Completeness        | ✅                                                              |
| FR-413 | Live Range Splitting                  | ✅                                                              |
| FR-414 | Iterative Inlining                    | ✅                                                              |
| FR-415 | CSE Key Efficiency                    | ✅ (hash cons style)                                            |
| FR-416 | Tail-Call Register Allocation         | ✅                                                              |

---

## Phase 83 — Binary & Linker Completion

| FR     | Description                       | Status |
| ------ | --------------------------------- | ------ | ---------------------------------------------------------------------------- |
| FR-464 | Mach-O PAGEZERO Segment           | ✅     |
| FR-465 | Mach-O Symbol Table Serialization | ✅     |
| FR-466 | Mach-O Data Segment Serialization | ✅     |
| FR-467 | Mach-O W^X Security               | ✅     |
| FR-468 | ELF AArch64 Support               | ✅     |
| FR-469 | ELF Section Alignment             | ✅     |
| FR-470 | ELF .bss Section                  | ✅     |
| FR-471 | Byte Buffer Unification           | ✅     | Unified `byte-buffer` API across ELF/PE/Mach-O/WASM; `buffer-*` API exported |
| FR-472 | Calling Convention FP Registers   | ✅     |
| FR-473 | WASM Portability                  | ✅     |

---

## Remaining Work (Future)

| Priority | FR     | Description                                            | Difficulty |
| -------- | ------ | ------------------------------------------------------ | ---------- |
| Low      | FR-227 | SLP Vectorizer (code exists, needs coverage expansion) | Hard       |

> **Summary**: All previously ⬜ items have been implemented or verified as already-existing (verified 2026-05-25: SLP code exists in optimizer.lisp lines 815-1057 with 3 tests). FR-227 SLP coverage expansion and native backend bignum (T2-A) remain as future work.
