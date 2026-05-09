# Audit Map: Incomplete/Partial FRs in optimize-passes.md

> **Superseded.** This document was a point-in-time grep audit and contains stale path claims that no longer match the codebase layout. For current FR status, per-FR implementation notes, and limitation descriptions, see:
>
> - `docs/optimize-passes.md` — FR headings are status-aware, and implementation notes document current evidence and known limitations where applicable.
> - `packages/optimize/src/optimizer-pipeline.lisp` — roadmap evidence registry maps FR identifiers to their state classification and public-API/test anchors.
> - Per-package test files under `packages/optimize/tests/` — verify implemented behavior.
>
> The previous tier classification and counts are **historical** and should not be used as a current status reference. Several path claims (e.g., `packages/compile/src/closure.lisp` does not exist, `packages/compile/src/cps-ast.lisp` does not exist) were correct about those specific paths but misleading about whether equivalent implementations existed elsewhere:
>
> - Closure analysis lives at `packages/ast/src/closure.lisp`, not `packages/compile/src/closure.lisp`.
> - CPS AST nodes live at `packages/cps/src/cps-ast.lisp`, not `packages/compile/src/cps-ast.lisp`.
