# テスト実行アーティファクト

## canonical target と runner 対応

公開されている test entrypoint は `nix run .#test` だけです。

- `nix run .#test` -> `run-tests`

`run-tests` は `packages/testing/framework/src/framework.lisp` 上で `run-suite 'cl-cc-suite ...`
を呼び、unit / integration / e2e / PBT を含む canonical test plan を実行します。

## final run 結果

### 最終確認

- `nix run .#test`
  - canonical plan が green
- `nix run .#build`
  - succeeded

### repeatability

- `nix run .#test`
  - repeated green
- `dcg-suite`
  - repeated green
- `macros-stdlib-io-suite`
  - repeated green

### clean-state

`nix run .#clean` 後に以下を再確認しました。

- `nix run .#build` green
- `nix run .#test` green

## 備考

- 最終状態では skip は残っていません。
- serial suite 化した領域は shared mutable state を持つ suite であり、deterministic boundary として分離されています。
- 高リスク修正と focused regression の対応は `docs/test-quality-verification.md` を参照してください。

## Env Vars

Runner-observed environment variables:

- `CLCC_TIMINGS_FILE` — TSV output path for per-test timings.
  Default `./test-timings.tsv`. Validated: `..` segments and paths outside
  `(uiop:getcwd)` are rejected with a warning and the default is used.
  Source: `packages/testing/framework/src/framework-runner.lisp` (`%timings-output-path`).
- `CLCC_TEST_TRACE=1` — When set, every test prints
  `# [trace] running <name>` to `*error-output*` before dispatch (hang diagnosis).
  Source: `packages/testing/framework/src/framework-runner.lisp:22-25`.
- `timeout` — Overrides the default per-test wall-clock timeout (seconds, default 10s).
  Source: `packages/testing/framework/src/framework.lisp:639` (`%default-test-timeout`).

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

Source: `packages/testing/framework/src/framework-runner.lisp` (`%write-timings-tsv`).

## TAP Extensions

Every TAP v13 result — pass, fail, skip, or pending — now carries a YAML
diagnostic block that includes `duration_ms: N.NNN` (double-float
milliseconds derived from `:duration-ns`). For failures, the pre-existing
failure YAML (`message:`, etc.) is preserved and `duration_ms:` is spliced
in just before the closing `  ...` sentinel.
Source: `packages/testing/framework/src/framework.lisp:455-496`.
