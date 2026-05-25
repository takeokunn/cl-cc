# CL-CC Compiler Implementation Tasks

Coding Agent 用逐次実装タスクリスト。
**ルール**: 最初の `[ ]` タスクを1つ実装 → テスト通過確認 → `[x]` に変更 → 次へ。

テスト実行: `nix run .#test` (canonical fast unit plan。integration / e2e は suite taxonomy で明示実行)

---

## Phase 1: Easy / Low (クイックウィン)

- [x] **FR-586** `set` 関数 (ANSI廃止予定だが必須)
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-586 セクションを参照
  - 対象: `packages/compile/src/builtin-registry-data.lisp`
  - 内容: `(set symbol value)` を `(setf (symbol-value symbol) value)` に等価な形でビルトイン登録

- [x] **FR-555** `copy-structure`
  - 詳細: `docs/ansi-cl-lang.md` の FR-555 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: `(copy-structure structure-object)` — 全スロットをシャローコピーした新インスタンスを返す

- [x] **FR-585** `handler-case :no-error` 節
  - 詳細: `docs/ansi-cl-lang.md` の FR-585 セクションを参照
  - 対象: `packages/expand/src/macros-stdlib.lisp`
  - 内容: フォームがシグナルなしに完了した場合のみ `success-body` を実行する `:no-error` 節サポート

- [x] **FR-548** `fdefinition` / `(setf fdefinition)`
  - 詳細: `docs/ansi-cl-lang.md` の FR-548 セクションを参照
  - 対象: `packages/vm/src/vm.lisp`, `packages/compile/src/builtin-registry.lisp`
  - 内容: `(fdefinition name)` — 関数名からオブジェクト取得。`(setf (fdefinition name) fn)` — 設定

- [x] **FR-552** `find-class` / `(setf find-class)`
  - 詳細: `docs/ansi-cl-lang.md` の FR-552 セクションを参照
  - 対象: `packages/vm/src/vm-clos.lisp`
  - 内容: `(find-class name &optional errorp environment)` — クラスオブジェクト取得・登録

- [x] **FR-545** `defstruct :include` 継承バグ修正
  - 詳細: `docs/ansi-cl-lang.md` の FR-545 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: `:include` 継承時に子スロットアクセサが正しく生成されるよう修正 (ANSI CL 8.1.5.5)

- [x] **FR-623** `let`/`flet` 空バインディング形式
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-623 セクションを参照
  - 対象: `packages/parse/src/cl/parser.lisp`
  - 内容: `(let () body)` / `(flet () body)` を `progn` に等価展開

- [x] **FR-625** `type-of` — float/function 型返却
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-625 セクションを参照
  - 対象: `packages/vm/src/primitives.lisp`
  - 内容: `float` → `single-float`、`vm-closure`/`vm-builtin-fn` → `function`、`nil` → `null`

- [x] **FR-626** `error`/`warn` フォーマット制御形式
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-626 セクションを参照
  - 対象: `packages/compile/src/builtin-registry-data.lisp`, `packages/vm/src/conditions.lisp`
  - 内容: `(error string &rest args)` を `(error (format nil string args...))` にデシュガー

- [x] **FR-627** `string-upcase`/`string-downcase` `:start/:end`
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-627 セクションを参照
  - 対象: `packages/vm/src/strings.lisp`
  - 内容: `:start`/`:end` パラメータを受け取り部分文字列変換。`nstring-upcase`/`nstring-downcase` も同様

- [x] **FR-440** `sort`/`stable-sort :key` サポート
  - 詳細: `docs/runtime-core.md` の FR-440 セクションを参照
  - 対象: `packages/expand/src/macros-list-utils.lisp`
  - 内容: `:key` 引数を受け付け、比較時に各要素に key 関数を適用

- [x] **FR-444** `copy-seq` ベクタ対応
  - 詳細: `docs/runtime-core.md` の FR-444 セクションを参照
  - 対象: `packages/expand/src/macros-sequence.lisp`
  - 内容: 入力型に応じてリストは `copy-list`、ベクタは `subseq` によるベクタコピー

- [x] **FR-445** `fill`/`replace` `:start/:end`
  - 詳細: `docs/runtime-core.md` の FR-445 セクションを参照
  - 対象: `packages/expand/src/macros-sequence.lisp`
  - 内容: `:start`/`:end` キーワードの処理で部分範囲への操作を可能にする

- [x] **FR-446** `defstruct :copier` オプション
  - 詳細: `docs/runtime-core.md` の FR-446 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: デフォルトで `copy-<name>` 関数を生成。`:copier nil` で抑制

- [x] **FR-449** `defstruct :read-only` スロット (実装済み。テストあり: defstruct-tests.lisp:142-158)

- [x] **FR-452** `merge :key` サポート (実装済み: macros-sequence-fold.lisp で :key パラメータを抽出・比較時にキー関数適用)

---

## Phase 2: コンパイラインフラ (Medium)

- [x] **FR-131** Non-Closure Promotion
  - 詳細: `docs/tooling-compiler.md` の FR-131 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`
  - 内容: `vm-captured-vars = nil` の lambda 定義時点でクロージャオブジェクト割り当てなしの静的関数参照に昇格

- [x] **FR-129** Compile-Time `intern` 解決
  - 詳細: `docs/tooling-compiler.md` の FR-129 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
  - 内容: シンボル名とパッケージがリテラル定数の `(intern "CAR" :cl)` をコンパイル時に解決して `vm-const` に変換

- [x] **FR-134** Named-let サポート (実装済み: lower.lisp で named LET を LABELS + 初期呼び出しへ lower)
  - 詳細: `docs/tooling-compiler.md` の FR-134 セクションを参照
  - 対象: `packages/expand/src/macros-basic.lisp`
  - 内容: named-let 構文を `labels` 内の再帰関数に展開するマクロを追加、末尾再帰ループのイディオムをサポート

- [x] **FR-133** `apply` 経由末尾呼び出し
  - 詳細: `docs/tooling-compiler.md` の FR-133 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp` (ast-apply)
  - 内容: 末尾位置の `(apply f args)` に `vm-tail-call` を発行 (現状は `vm-apply` + return)

- [x] **FR-125** Declaration Processing (`type`/`inline`/`ignore`)
  - 詳細: `docs/tooling-compiler.md` の FR-125 セクションを参照
  - 対象: `packages/expand/src/expander.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: `(declare (type fixnum x))` → register-type-map 登録、`(declare (inline f))` → インライン化フォース、`(declare (ignore x))` → 未使用警告抑制

- [x] **FR-127** Type Proclamations (`declaim`/`proclaim ftype`)
  - 詳細: `docs/tooling-compiler.md` の FR-127 セクションを参照
  - 対象: `packages/expand/src/expander.lisp`, `packages/compile/src/codegen-functions.lisp`
  - 内容: `(declaim (ftype (function (fixnum) fixnum) foo))` を解析し `*function-type-registry*` に登録

- [x] **FR-126** `define-compiler-macro`
  - 詳細: `docs/tooling-compiler.md` の FR-126 セクションを参照
  - 対象: `packages/expand/src/expander.lisp`
  - 内容: `*compiler-macro-registry*` (関数名→展開関数) の追加。呼び出しパターンに応じた特化展開をユーザが定義可能

- [x] **FR-132** Closure Environment Trimming
  - 詳細: `docs/tooling-compiler.md` の FR-132 セクションを参照
  - 対象: `packages/compile/src/closure.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: クロージャボディ内で一切読まれないキャプチャ変数を `vm-captured-vars` から除去

---

## Phase 3: ANSI 言語コア (Medium)

- [x] **FR-544** `defstruct` コンストラクタ・コピー・述語オプション
  - 詳細: `docs/ansi-cl-lang.md` の FR-544 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: ANSI CL 8.1.5〜8.1.7 の defstruct オプション処理 (`:constructor`/`:predicate` カスタム名)

- [x] **FR-546** `defstruct :type`/`:conc-name`/スロットオプション
  - 詳細: `docs/ansi-cl-lang.md` の FR-546 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: `:type list`/`:type vector`、`:conc-name`、スロット `:type`/`:read-only`

- [x] **FR-607** `defun`/`defmacro` ドキュメント文字列
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-607 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`, `packages/expand/src/expander.lisp`
  - 内容: コンパイル時に docstring を VM 関数オブジェクトのメタデータとして保存。`(documentation 'fn 'function)` で取得可能に

- [x] **FR-612** `read`/`read-char` `eof-error-p`/`eof-value`
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-612 セクションを参照
  - 対象: `packages/vm/src/io.lisp`, `packages/compile/src/builtin-registry-data.lisp`
  - 内容: 4引数形式 `(read stream eof-error-p eof-value recursive-p)` を完全サポート

- [x] **FR-606** `assert` place-list (場所付きリスタート)
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-606 セクションを参照
  - 対象: `packages/expand/src/macros-stdlib.lisp`
  - 内容: `(assert test-form (place1 place2 ...) ...)` — 失敗時に `continue` リスタートで各 place を対話修正

- [x] **FR-579** バイト列・文字列変換
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-579 セクションを参照
  - 対象: `packages/vm/src/strings.lisp`
  - 内容: `string-to-octets string &key encoding` / `octets-to-string octets &key encoding`

- [x] **FR-624** `subtypep`
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-624 セクションを参照
  - 対象: `packages/vm/src/primitives.lisp`, `packages/compile/src/builtin-registry-data.lisp`
  - 内容: `(subtypep type1 type2 &optional environment)` — 型指定子の包含関係を `(values result certainty)` で返す

- [x] **FR-630** `coerce` 実行時型ディスパッチ
  - 詳細: `docs/ansi-cl-stdlib.md` の FR-630 セクションを参照
  - 対象: `packages/expand/src/macros-runtime-support.lisp`, `packages/stdlib/src/stdlib-source.lisp`
  - 内容: literal 型は `coerce-to-*` へ直接展開し、動的型は caller package の `%coerce-runtime` へ展開して実行時分岐する

---

## Phase 4: ランタイム最適化 (Medium)

- [x] **FR-044** `apply` Fast Path (既知引数数)
  - 詳細: `docs/runtime-core.md` の FR-044 セクションを参照
  - 対象: `packages/vm/src/vm.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: 既知引数数の `apply` 呼び出しを専用高速パスでディスパッチ

- [x] **FR-047** Format 文字列コンパイル時処理
  - 詳細: `docs/runtime-core.md` の FR-047 セクションを参照
  - 対象: `packages/vm/src/io.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: `(format nil "~A ~A" x y)` のような静的フォーマット文字列をコンパイル時に解析

- [x] **FR-048** Hash Table 操作特化
  - 詳細: `docs/runtime-core.md` の FR-048 セクションを参照
  - 対象: `packages/vm/src/hash.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: `make-hash-table :test` から `gethash` のホットパスを特化し、`vm-gethash-eq`/`vm-gethash-eql`/`vm-gethash-equal` を生成

- [x] **FR-049** Equality 述語特化 (`eq`/`eql`/`equal`)
   - 詳細: `docs/runtime-core.md` の FR-049 セクションを参照
   - 対象: `packages/vm/src/primitives.lisp`, `packages/compile/src/codegen.lisp`
   - 内容: 型情報から `eq`/`eql`/`equal` の最速パスを選択するコンパイル時ディスパッチ

- [x] **FR-042** Dynamic Variable Access Caching
   - 詳細: `docs/runtime-core.md` の FR-042 セクションを参照
   - 対象: `packages/vm/src/vm.lisp`, `packages/compile/src/codegen.lisp`
   - 内容: 関数エントリ時にグローバルをレジスタにロード、変更時のみ書き戻し

- [x] **FR-043** Closure Environment Flattening
   - 詳細: `docs/runtime-core.md` の FR-043 セクションを参照
   - 対象: `packages/vm/src/vm.lisp`, `packages/vm/src/vm-execute.lisp`
   - 内容: captured-valuesをalistから並列ベクタに変換、インデックスアクセス化

- [x] **FR-046** Condition System Zero-Cost Fast Path
   - 詳細: `docs/runtime-core.md` の FR-046 セクションを参照
   - 対象: `packages/compile/src/codegen-control.lisp`
   - 内容: 保護フォーム内にsignal系なしの場合にvm-establish-handlerを省略

- [x] **FR-139** Handler Elision for Pure Bodies
   - 詳細: `docs/runtime-core.md` の FR-139 セクションを参照
   - 対象: `packages/compile/src/codegen-control.lisp`
   - 内容: 保護フォームの命令が全てpureの場合にhandler確立を省略

- [x] **FR-141** Self-Hosting Profile Feedback
   - 詳細: `docs/runtime-core.md` の FR-141 セクションを参照
   - 対象: `packages/cli/src/main.lisp`, `packages/vm/src/vm-state-init.lisp`
   - 内容: `cl-cc selfhost --profile` でVM命令頻度ヒストグラム収集・活用

- [x] **FR-156** Size-Class Segregated Allocator
   - 詳細: `docs/runtime-core.md` の FR-156 セクションを参照
   - 対象: `packages/runtime/src/heap-core.lisp`, `packages/runtime/src/heap-free-list.lisp`
   - 内容: free-listをサイズクラスバケットに分割、O(1)再利用

- [x] **FR-338** Runtime String Interning
   - 詳細: `docs/runtime-core.md` の FR-338 セクションを参照
   - 対象: `packages/vm/src/strings.lisp`, `packages/vm/src/symbols.lisp`
   - 内容: ランタイム文字列インターンテーブルで重複排除・メモリ削減

- [x] **FR-441** `sort`/`stable-sort` ベクタ対応
  - 詳細: `docs/runtime-core.md` の FR-441 セクションを参照
  - 対象: `packages/expand/src/macros-stdlib.lisp`
  - 内容: ベクタ入力時にインプレースソート (quicksort/mergesort on vector)

- [x] **FR-442** `make-array` キーワード引数コンパイル
  - 詳細: `docs/runtime-core.md` の FR-442 セクションを参照
  - 対象: `packages/compile/src/codegen-phase2.lisp`
  - 内容: `make-array` キーワード引数をコンパイルして VM 命令に渡す

- [x] **FR-447** `defstruct :print-function`/`:print-object`
  - 詳細: `docs/runtime-core.md` の FR-447 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: `:print-object` 指定時に `print-object` メソッド生成。`:print-function` は旧互換

- [x] **FR-448** `defstruct :type list`/`:type vector`
  - 詳細: `docs/runtime-core.md` の FR-448 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: `(:type list)` — 構造体をリスト表現。`(:type vector)` — ベクタ表現。`:named` オプションと組み合わせ

- [x] **FR-451** `search` 汎用シーケンス
  - 詳細: `docs/runtime-core.md` の FR-451 セクションを参照
  - 対象: `packages/expand/src/macros-sequence.lisp`
  - 内容: `(search sequence1 sequence2 &key test key start1 end1 start2 end2)` — 部分シーケンス検索

- [x] **FR-453** `map-into` 複数ソース (実装済み: map-into が任意個数ソースを並列走査し、宛先または最短ソースで停止)
   - 詳細: `docs/runtime-core.md` の FR-453 セクションを参照
   - 対象: `packages/expand/src/macros-sequence.lisp`
   - 内容: 複数ソースシーケンスから並列にマッピング。最短ソースで停止

- [x] **FR-180** Single-Value Optimization
   - 詳細: `docs/runtime-core.md` の FR-180 セクションを参照
   - 対象: `packages/compile/src/codegen-values.lisp`, `packages/compile/src/codegen-values-helpers.lisp`
   - 内容: `(values x)` が単一値の場合に `vm-values` 命令を省略し直接レジスタ移動に変換

- [x] **FR-185** Optimization Reports
   - 詳細: `docs/runtime-core.md` の FR-185 セクションを参照
   - 対象: `packages/optimize/src/optimizer.lisp`, `packages/cli/src/main.lisp`
   - 内容: `--optimization-report` フラグで最適化判断をレポート出力

- [x] **FR-339** Hash Table Size/Rehash Control
   - 詳細: `docs/runtime-core.md` の FR-339 セクションを参照
   - 対象: `packages/vm/src/hash.lisp`, `packages/compile/src/codegen-hash-table.lisp`
   - 内容: `make-hash-table` に `:size`/`:rehash-size`/`:rehash-threshold` を追加。アクセサ関数追加

- [x] **FR-340** Compile-Time Sequence/String Folding
   - 詳細: `docs/runtime-core.md` の FR-340 セクションを参照
   - 対象: `packages/optimize/src/optimizer-tables.lisp`
   - 内容: `(length "hello")` → `5` 等のシーケンス/文字列述語のコンパイル時畳み込み

- [x] **FR-343** Set Operations Hash Acceleration
   - 詳細: `docs/runtime-core.md` の FR-343 セクションを参照
   - 対象: `packages/expand/src/macros-setops.lisp`
   - 内容: `remove-duplicates`/`union`/`intersection`/`set-difference` のハッシュテーブル高速化

- [x] **FR-181** Constant Pool / Literal Deduplication
   - 詳細: `docs/runtime-core.md` の FR-181 セクションを参照
   - 対象: `packages/compile/src/codegen-core.lisp`, `packages/pipeline/src/pipeline.lisp`
   - 内容: コンパイル単位ごとの定数プール構築。同一リテラルの重複排除（FR-137の汎化）

- [x] **FR-341** HOF Macro Vector Path (mapcar vector 対応)
   - 詳細: `docs/runtime-core.md` の FR-341 セクションを参照
   - 対象: `packages/expand/src/macros-stdlib.lisp`
   - 内容: mapcar のベクタ入力対応 (dotimes+aref)

- [x] **FR-344** String/List Algebraic Simplification (一部既存)
   - 詳細: `docs/runtime-core.md` の FR-344 セクションを参照
   - 対象: `packages/optimize/src/optimizer-algebraic.lisp`
   - 内容: 型述語の生産者命令型に基づく簡約ルール追加

---

## Phase 5: デバッグ・診断ツール (Medium)

- [x] **FR-506** エラー回復 (Error Recovery)
  - 詳細: `docs/tooling-debug.md` の FR-506 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`
  - 内容: フォームレベルのエラー回復。1フォームのコンパイル失敗を記録しつつ次フォームに継続

- [x] **FR-313** コールスタック整形表示
  - 詳細: `docs/tooling-debug.md` の FR-313 セクションを参照
  - 対象: `packages/vm/src/vm.lisp`, `packages/vm/src/vm-run.lisp`
  - 内容: `vm-print-backtrace`: `vm-call-stack` を走査し、return-pc をラベルテーブル経由で関数名に解決

- [x] **FR-317** Structured Diagnostics
  - 詳細: `docs/tooling-debug.md` の FR-317 セクションを参照
  - 対象: `packages/parse/src/diagnostics.lisp`, `packages/vm/src/conditions.lisp`
  - 内容: `error-code` フィールド (e.g., `E0001`) 追加。`fix-it` フィールド (修正候補テキスト+span) 追加

- [x] **FR-318** Compiler Warning System
  - 詳細: `docs/tooling-debug.md` の FR-318 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`, `packages/parse/src/diagnostics.lisp`
  - 内容: 未使用変数/import、型不一致、到達不能コード、非推奨関数使用の警告

- [x] **FR-312** REPL 拡張 (補完・履歴)
  - 詳細: `docs/tooling-debug.md` の FR-312 セクションを参照
  - 対象: `packages/cli/src/main.lisp`
  - 内容: コマンド履歴 (上下矢印)、タブ補完 (パッケージシンボル+関数レジストリ)、複数行編集

- [x] **FR-316** ベンチマーク/プロファイリングフレームワーク
  - 詳細: `docs/tooling-debug.md` の FR-316 セクションを参照
  - 対象: `packages/testing-framework/src/framework-compiler.lisp`, `packages/vm/src/vm-run.lisp`
  - 内容: `defbenchmark` マクロ (ウォームアップ+計測反復+統計出力)。VM 命令カウンタ/プロファイラ

- [x] **FR-350** Line/Branch カバレッジ
  - 詳細: `docs/tooling-debug.md` の FR-350 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`, `packages/testing-framework/src/framework-compiler.lisp`
  - 内容: `--coverage` コンパイルフラグでソース行・分岐ごとにカウンタ命令を埋め込み

- [x] **FR-353** Property-Based Testing 深化
  - 詳細: `docs/tooling-debug.md` の FR-353 セクションを参照
  - 対象: `packages/testing-framework/src/framework-fuzz.lisp`
  - 内容: shrinking (失敗入力の最小化)、stateful PBT (コマンドシーケンス生成)

---

## Phase 6: 高度最適化 (Medium-Hard)

- [x] **FR-128** Typecase ジャンプテーブル
  - 詳細: `docs/tooling-compiler.md` の FR-128 セクションを参照
  - 対象: `packages/expand/src/macros-control-flow-case.lisp`
  - 内容: `(typecase x (fixnum ..) (cons ..))` をネスト if でなく型タグインデックスのジャンプテーブルにコンパイル

- [x] **FR-130** Perfect Hash for Compiler Dispatch
  - 詳細: `docs/tooling-compiler.md` の FR-130 セクションを参照
  - 対象: `packages/expand/src/expander-data.lisp`, `packages/expand/src/expander.lisp`, `packages/expand/src/expander-core.lisp`, `packages/expand/src/expander-basic.lisp`
  - 内容: `*compiler-special-forms*`、`*all-builtin-names*`、`*binary-builtins*`、`*variadic-fold-builtins*` などを hash table 化。compiler-macroexpand-all の hot path で `gethash` (O(1)) に置換

- [x] **FR-135** Loop マクロ品質修正
  - 詳細: `docs/tooling-compiler.md` の FR-135 セクションを参照
  - 対象: `packages/expand/src/loop-iter-emitters.lisp`
  - 内容: `:across` ベクタループの不要 nil 初期化除去。`:hash-keys`/`:hash-values` は既存実装で修正済み。

- [x] **FR-136** Character Class ルックアップテーブル (実装済み: strings.lisp:84-182 に既存)

- [x] **FR-137** String Literal Pool (実装済み: codegen-core.lisp 等に既存)

- [x] **FR-152** 推移的関数純粋性推論
  - 詳細: `docs/tooling-compiler.md` の FR-152 セクションを参照
  - 対象: `packages/optimize/src/effects.lisp`, `packages/optimize/src/package.lisp`
  - 内容: コールグラフを走査して純粋関数を推移的に伝播。`compute-function-purity`、`register-function-instructions`、`call-site-effect-kind` API 追加

- [x] **FR-183** Known Function Property Database

- [x] **FR-562** Unicode サポート

---

## Phase 7: Very Hard (別プロジェクト扱い)

以下は根幹変更が必要なため、上記フェーズ完了後に別途検討。

- [ ] Native backend bignum 値表現 — ANSI stdlib VM 実装は FR-605 で完了。x86-64 native backend の自前 bignum タグ/桁配列表現は別トラックで検討
- [x] **FR-580** FFI / CFFI 互換層 — VM interpreter の host-backed 最小 CFFI 互換 shim は完了。portable/native libffi backend は別トラックで検討
- [ ] **FR-800** 完全第一級継続 — CPS 変換全体の再設計

---

## 完了済み

_(実装後ここに移動)_
