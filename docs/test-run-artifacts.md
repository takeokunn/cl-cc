# テスト実行アーティファクト

## canonical target と runner 対応

公開されている test entrypoint は **`nix run .#test` 一本のみ**です。

- `nix run .#test` -> `cl-cc/test:run-tests`（unit + integration + selfhost-slow + e2e の full canonical plan）
- `nix flake check` -> 同じ plan を `checks.tests` 経由で derivation として実行

`run-tests` は `run-suite 'cl-cc-suite ...` を呼び、unit / integration / e2e / PBT /
selfhost-slow を含む full canonical test plan を実行します。

## 検証チェックリスト

この文書はテスト実行面の contract と artifact を説明するためのものです。
個別セッションの「green 実績」を固定記録する場所ではありません。

### 推奨確認項目

- `nix run .#test`
  - canonical full plan が完走する
- `nix flake check`
  - sandbox derivation 内で同じ plan が完走する
- `nix build`
  - standalone binary が生成される

## 備考

- skip / pending / serial-only suites の有無は current run artifacts から確認してください。
- serial suite 化した領域は shared mutable state を持つ suite であり、deterministic boundary として分離されています。
- 高リスク修正と focused regression の対応は `docs/test-quality-verification.md` を参照してください。

## Env Vars

Runner-observed environment variables:

- `CLCC_TIMINGS_FILE` — TSV output path for per-test timings.
  Default `./test-timings.tsv`. Validated: `..` segments and paths outside
  `(uiop:getcwd)` are rejected with a warning and the default is used.
  Source: `packages/testing-framework/src/framework-runner.lisp` (`%timings-output-path`).
- `CLCC_TEST_TRACE=1` — When set, every test prints
  `# [trace] running <name>` to `*error-output*` before dispatch (hang diagnosis).
  Source: `packages/testing-framework/src/framework-runner.lisp:22-25`.
- `CLCC_TEST_TIMEOUT` — Overrides the default per-test wall-clock timeout in seconds.
  Framework default is `10` (`packages/testing-framework/src/framework-timeouts.lisp` `%default-test-timeout`),
  but the canonical Nix entrypoints (`nix run .#test`, `checks.tests`) export `CLCC_TEST_TIMEOUT=180`
  via `nix/apps.nix` and `nix/checks.nix` so individual long-running tests are not penalised.

## `test-timings.tsv`

Frozen schema (tab-separated, LC_ALL-independent integers):

```
suite\ttest-name\tduration-ns\tstatus\tbatch-id
```

Status values: `passed | failed | skipped | pending | errored | timed-out`.
`batch-id` is the parallel worker batch index as an integer, or `-` for
sequential runs. Suite and test-name are sanitized so tab/newline/CR are
replaced with space, guaranteeing exactly 5 columns per row.

Example row:

```
CL-CC-UNIT-SUITE	TIMING-PASS-CASE	123456	passed	3
```

Source: `packages/testing-framework/src/framework-runner.lisp` (`%write-timings-tsv`).

## TAP Extensions

Every TAP v13 result — pass, fail, skip, or pending — now carries a YAML
diagnostic block that includes `duration_ms: N.NNN` (double-float
milliseconds derived from `:duration-ns`). For failures, the pre-existing
failure YAML (`message:`, etc.) is preserved and `duration_ms:` is spliced
in just before the closing `  ...` sentinel.
Source: `packages/testing-framework/src/framework.lisp:455-496`.
