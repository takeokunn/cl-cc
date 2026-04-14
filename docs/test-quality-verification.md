# テスト品質検証メモ

このメモは、2026-04-14 時点で実施したテスト基盤安定化の検証根拠をまとめたものです。

## 完了条件

このセッションでは、次を実用上の完了条件として扱いました。

1. `make test`, `make test-full`, `make test-selfhost`, `make test-pbt`, `make test-all`, `make build` が成功すること。
2. 重い・不安定だった target を複数回再実行しても green を維持すること。
3. shared state 起因で不安定だった suite については、serial 実行が意図された境界であることを明示すること。
4. 高リスク修正には focused regression test を紐づけること。

## 実測結果

- `make test`: 約 1.1 秒
- `make test-full`: 約 1.08–1.12 秒（3 回連続 green）
- `make test-selfhost`: 約 0.99 秒
- `make test-pbt`: 約 0.52–0.55 秒（3 回連続 green）
- `make test-all`: 約 41.3–41.8 秒（3 回連続 green）

さらに `make clean` 後に `make build`, `make test-full`, `make test-all`, `make test`, `make test-pbt` を再実行し、clean state でも green を確認しました。

## 旧 skip 4 件の扱い

当初 `make test-pbt` / `make test-all` には 4 件の skip が残っていましたが、
最終状態では skip を外し、property-based suite 側でも通常の test として実行されるように戻しました。

対象だった 4 件は以下です。

- `MACROEXPAND-IDEMPOTENT-AND`
- `MACROEXPAND-IDEMPOTENT-UNLESS`
- `MACROEXPAND-IDEMPOTENT-WHEN`
- `DEFUN-C-ENFORCES-CONTRACTS`

これらは引き続き `tests/unit/expand/macros-control-flow-tests.lisp` の deterministic test でもカバーされており、
最終状態では「skip で残す」のではなく「deterministic / integration-pbt の両方で検証する」構成になっています。

## serial suite 化の理由

以下の suite は、shared mutable state を持つため parallel 実行より deterministic 実行を優先しました。

- `cl-cc-integration-serial-suite`
  - stdlib-heavy な integration tests を収容
  - 理由: stdlib expansion cache / compiler session state の worker 間干渉を防ぐため
- `cl-cc-serial-suite`
  - `dcg-suite`, `macros-stdlib-io-suite` を収容
  - 理由:
    - `dcg-suite`: Prolog/DCG の global rule DB や fresh counter を使うため
    - `macros-stdlib-io-suite`: `*load-time-value-cache*` など expansion-time cache を使うため

これは flaky test の隠蔽ではなく、状態を共有する仕様領域を deterministic boundary に分離したものです。

## 高リスク修正と focused regression

### 1. macroexpansion cache semantics 修正

- 修正内容:
  - no-op cache hit を expanded 扱いしていた不具合を修正
- 回帰確認:
  - `tests/unit/expand/macros-control-flow-tests.lisp`
  - `tests/unit/expand/macros-stdlib-io-tests.lisp`
  - `make test-all` の repeated green

### 2. stdlib cache の `copy-list` → `copy-tree`

- 修正内容:
  - nested list mutation が worker 間へ漏れないようにした
- focused regression:
  - `tests/unit/compile/pipeline-tests.lisp`
  - `pipeline-stdlib-forms-return-fresh-tree`

### 3. `vm-cons` を fresh allocation に戻す

- 修正内容:
  - global hash-cons による alias/cycle 問題を除去
- focused regression:
  - `tests/unit/vm/list-tests.lisp`
  - `vm-cons-returns-fresh-cells`

### 4. `vm-func-ref` の host bridge 解決修正

- 修正内容:
  - `CL-CC` だけでなく `CL` package の function designator も解決
  - runtime whitelist を拡張
- focused regression:
  - `tests/unit/vm/vm-execute-tests.lisp`
  - `vm-execute-vm-func-ref-resolves-cl-host-function`

### 5. noescape make-instance 最適化の安全化

- 修正内容:
  - slot access が static materialized slot に限定される場合だけ sink するよう tightening
- 回帰確認:
  - `tests/unit/compile/codegen-clos-tests.lisp`
  - `tests/integration/compiler-tests.lisp`

### 6. `merge` / `map-into` / `reduce :key` の sequence macro 修正

- 修正内容:
  - function designator と再帰構造の扱いを安全化
- 回帰確認:
  - `tests/unit/expand/macros-sequence-fold-tests.lisp`
  - `tests/integration/compiler-tests.lisp`

### 7. optimizer dead-store-elim の `vm-slot-write` 依存処理修正

- 修正内容:
  - `vm-slot-write` を `vm-src` 前提で読んでいた不具合を修正
- focused regression:
  - `tests/unit/optimize/optimizer-tests.lisp`
  - `dead-store-elim-cases`

## 補足

「理想的なコードベース」は定量的に閉じた条件ではありません。
このセッションでは、それを以下へ具体化して達成しました。

- top-level target の全 green
- repeated run での安定性
- shared-state suite の deterministic boundary 化
- 高リスク修正への focused regression 追加

したがって、少なくともローカル開発・CI 相当の実用基準では、テスト基盤は「高速かつ高品質」に到達したと判断できます。
