# テスト実行アーティファクト

## canonical target と runner 対応

公開されている主要 test entrypoint は次の 3 つです。

- `nix run .#test` -> `run-fast-tests`（fast feedback path、`selfhost-slow-suite` を除外）
- `nix run .#test-full` -> `run-tests`（full canonical plan、`:cl-cc-test/slow` をロード）
- `nix run .#coverage` -> 3 フェーズで `run-suite` を実行し、coverage HTML を生成

`run-tests` は `run-suite 'cl-cc-suite ...` を呼び、unit / integration / e2e / PBT /
slow selfhost を含む full canonical test plan を実行します。

`run-fast-tests` は day-to-day の速度優先 path で、`selfhost-slow-suite` を自動除外します。

## 検証チェックリスト

この文書はテスト実行面の contract と artifact を説明するためのものです。
個別セッションの「green 実績」を固定記録する場所ではありません。

### 推奨確認項目

- `nix run .#test`
  - fast feedback plan（selfhost-slow 除外）が完走する
- `nix run .#test-full`
  - slow selfhost を含む full canonical plan が完走する
- `nix run .#coverage`
  - 同じ計画が sb-cover 付きで完走し、HTML coverage artifact が生成される
- `nix build`
  - standalone binary が生成される

### clean-state

`nix run .#clean` 後は少なくとも以下を再確認します。

- `nix run .#test`
- `nix run .#test-full`
- `nix run .#coverage`
- `nix build`

## 備考

- skip / pending / serial-only suites の有無は current run artifacts から確認してください。
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
- `CLCC_TEST_TIMEOUT` — Overrides the default per-test wall-clock timeout (seconds, default 10s).
  Source: `packages/testing/framework/src/framework-timeouts.lisp` (`%default-test-timeout`).

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
