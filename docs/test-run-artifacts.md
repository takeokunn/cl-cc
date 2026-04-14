# テスト実行アーティファクト

## top-level target と runner 対応

`Makefile` の top-level target は以下の runner に対応します。

- `make test` -> `run-tests`
- `make test-full` -> `run-tests-extended`
- `make test-selfhost` -> `run-selfhost-tests`
- `make test-pbt` -> `run-pbt-tests`
- `make test-all` -> `run-all-tests`

`tests/framework/framework.lisp` では、これらが最終的に `cl-cc-suite` またはその子 suite を辿る形で実行されます。

- `run-tests` / `run-tests-extended` / `run-all-tests` は `run-suite 'cl-cc-suite ...`
- `run-selfhost-tests` は selfhost suite 専用
- `run-pbt-tests` は PBT suite 専用

したがって、上記 top-level target を通すことが、この repo で意図されている公開 test entrypoint の網羅検証に相当します。

## final run 結果

### 最終確認

- `make test`
  - PASS 5054 / FAIL 0 / TOTAL 5054
- `make test-full`
  - PASS 5054 / FAIL 0 / TOTAL 5054
- `make test-selfhost`
  - PASS 16 / FAIL 0 / TOTAL 16
- `make test-pbt`
  - PASS 157 / FAIL 0 / TOTAL 157
- `make test-all`
  - PASS 6607 / FAIL 0 / TOTAL 6607
- `make build`
  - succeeded

### repeatability

- `make test-full`
  - 3/3 green
  - 1.12s / 1.10s / 1.08s
- `make test-all`
  - 3/3 green
  - 41.30s / 41.80s / 41.72s
- `make test-pbt`
  - 3/3 green
  - 0.52s / 0.55s / 0.52s
- `dcg-suite`
  - 5/5 green
- `macros-stdlib-io-suite`
  - 5/5 green

### clean-state

`make clean` 後に以下を再確認しました。

- `make build` green
- `make test-full` green
- `make test-all` green
- `make test` green
- `make test-pbt` green

## 備考

- 最終状態では skip は残っていません。
- serial suite 化した領域は shared mutable state を持つ suite であり、deterministic boundary として分離されています。
- 高リスク修正と focused regression の対応は `docs/test-quality-verification.md` を参照してください。
