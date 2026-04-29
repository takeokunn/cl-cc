# テスト品質検証メモ

この文書は historical memo です。現在の branch / session に対する完了宣言ではありません。

## 位置づけ

- 過去にどのような instability / shared-state 問題が議論されたか
- どの種類の focused regression が有効だったか
- serial suite 化をどのような設計判断として扱ったか

を残すための補助資料です。

## 現在の扱い

この文書単体をもって「完了」「green」「CI 相当で十分」とは見なしません。
現在の検証可否は、常に以下の実ラン結果で判断してください。

1. `nix run .#test`（fast-feedback plan。unit + integration、`selfhost-slow-suite` は除外）
2. `nix run .#test-full`（canonical full plan。旧 `test-all` はこのアプリに集約済み）
3. `nix run .#coverage`（production-only coverage policy で HTML report と threshold check を実行）
4. `nix build`
5. 必要に応じて `nix flake check`

### gate の役割分担

- `nix flake check`
  - CI 向けの軽量 gate。
  - 現在は `checks.tests`（fast plan 相当）と `checks.build` を実行する。
- `nix run .#test-full`
  - selfhost-slow / e2e を含む canonical full gate。
- `nix run .#coverage`
  - production-only coverage gate。
- `nix run .#perf-smoke`
  - fast/full gate の runtime budget 検証。
- `nix run .#stability-smoke`
  - fast gate の repeat 実行による flake smoke。

## 歴史的に重要だった論点

### serial suite 化の理由

shared mutable state を持つ suite では、parallel 実行より deterministic boundary を優先する設計が採られてきました。
これは flaky test の隠蔽ではなく、状態共有を明示化するための構造化です。

### focused regression の価値

高リスク修正には focused regression を結びつける、という方針自体は現在も有効です。
ただし、そのことは end-to-end green を代替しません。

## 注意

この文書に historical な成功記録が残っていても、現在の branch が同じ状態だとは限りません。
完了判定や品質判定には、最新の run artifact と実行結果を使ってください。
