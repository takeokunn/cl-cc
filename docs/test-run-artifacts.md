# テスト実行アーティファクト

## canonical target と runner 対応

公開されている test entrypoint は `nix run .#test` だけです。

- `nix run .#test` -> `run-tests`

`run-tests` は `tests/framework/framework.lisp` 上で `run-suite 'cl-cc-suite ...`
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
