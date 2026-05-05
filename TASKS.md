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

- [ ] **FR-585** `handler-case :no-error` 節
  - 詳細: `docs/ansi-cl-lang.md` の FR-585 セクションを参照
  - 対象: `packages/expand/src/macros-stdlib.lisp`
  - 内容: フォームがシグナルなしに完了した場合のみ `success-body` を実行する `:no-error` 節サポート

- [ ] **FR-548** `fdefinition` / `(setf fdefinition)`
  - 詳細: `docs/ansi-cl-lang.md` の FR-548 セクションを参照
  - 対象: `packages/vm/src/vm.lisp`, `packages/compile/src/builtin-registry.lisp`
  - 内容: `(fdefinition name)` — 関数名からオブジェクト取得。`(setf (fdefinition name) fn)` — 設定

- [ ] **FR-552** `find-class` / `(setf find-class)`
  - 詳細: `docs/ansi-cl-lang.md` の FR-552 セクションを参照
  - 対象: `packages/vm/src/vm-clos.lisp`
  - 内容: `(find-class name &optional errorp environment)` — クラスオブジェクト取得・登録

- [ ] **FR-545** `defstruct :include` 継承バグ修正
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

- [ ] **FR-440** `sort`/`stable-sort :key` サポート
  - 詳細: `docs/runtime-core.md` の FR-440 セクションを参照
  - 対象: `packages/expand/src/macros-stdlib.lisp`
  - 内容: `:key` 引数を受け付け、比較時に各要素に key 関数を適用

- [ ] **FR-444** `copy-seq` ベクタ対応
  - 詳細: `docs/runtime-core.md` の FR-444 セクションを参照
  - 対象: `packages/expand/src/macros-sequence.lisp`
  - 内容: 入力型に応じてリストは `copy-list`、ベクタはベクタコピー

- [ ] **FR-445** `fill`/`replace` `:start/:end`
  - 詳細: `docs/runtime-core.md` の FR-445 セクションを参照
  - 対象: `packages/expand/src/macros-sequence.lisp`
  - 内容: `:start`/`:end` キーワードの処理で部分範囲への操作を可能にする

- [ ] **FR-446** `defstruct :copier` オプション
  - 詳細: `docs/runtime-core.md` の FR-446 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: デフォルトで `copy-<name>` 関数を生成。`:copier nil` で抑制

- [ ] **FR-449** `defstruct :read-only` スロット
  - 詳細: `docs/runtime-core.md` の FR-449 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: `:read-only t` 指定時に setf アクセサを生成しない

- [ ] **FR-452** `merge :key` サポート
  - 詳細: `docs/runtime-core.md` の FR-452 セクションを参照
  - 対象: `packages/expand/src/macros-sequence.lisp`
  - 内容: `:key` 引数の処理。マージ比較時に key 関数適用

---

## Phase 2: コンパイラインフラ (Medium)

- [ ] **FR-131** Non-Closure Promotion
  - 詳細: `docs/tooling-compiler.md` の FR-131 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`
  - 内容: `vm-captured-vars = nil` の lambda 定義時点でクロージャオブジェクト割り当てなしの静的関数参照に昇格

- [ ] **FR-129** Compile-Time `intern` 解決
  - 詳細: `docs/tooling-compiler.md` の FR-129 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
  - 内容: シンボル名とパッケージがリテラル定数の `(intern "CAR" :cl)` をコンパイル時に解決して `vm-const` に変換

- [ ] **FR-134** Named-let サポート
  - 詳細: `docs/tooling-compiler.md` の FR-134 セクションを参照
  - 対象: `packages/expand/src/macros-basic.lisp`
  - 内容: named-let 構文を `labels` 内の再帰関数に展開するマクロを追加、末尾再帰ループのイディオムをサポート

- [ ] **FR-133** `apply` 経由末尾呼び出し
  - 詳細: `docs/tooling-compiler.md` の FR-133 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp` (ast-apply)
  - 内容: 末尾位置の `(apply f args)` に `vm-tail-call` を発行 (現状は `vm-apply` + return)

- [ ] **FR-125** Declaration Processing (`type`/`inline`/`ignore`)
  - 詳細: `docs/tooling-compiler.md` の FR-125 セクションを参照
  - 対象: `packages/expand/src/expander.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: `(declare (type fixnum x))` → register-type-map 登録、`(declare (inline f))` → インライン化フォース、`(declare (ignore x))` → 未使用警告抑制

- [ ] **FR-127** Type Proclamations (`declaim`/`proclaim ftype`)
  - 詳細: `docs/tooling-compiler.md` の FR-127 セクションを参照
  - 対象: `packages/expand/src/expander.lisp`, `packages/compile/src/codegen-functions.lisp`
  - 内容: `(declaim (ftype (function (fixnum) fixnum) foo))` を解析し `*function-type-registry*` に登録

- [ ] **FR-126** `define-compiler-macro`
  - 詳細: `docs/tooling-compiler.md` の FR-126 セクションを参照
  - 対象: `packages/expand/src/expander.lisp`
  - 内容: `*compiler-macro-registry*` (関数名→展開関数) の追加。呼び出しパターンに応じた特化展開をユーザが定義可能

- [ ] **FR-132** Closure Environment Trimming
  - 詳細: `docs/tooling-compiler.md` の FR-132 セクションを参照
  - 対象: `packages/compile/src/closure.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: クロージャボディ内で一切読まれないキャプチャ変数を `vm-captured-vars` から除去

---

## Phase 3: ANSI 言語コア (Medium)

- [ ] **FR-544** `defstruct` コンストラクタ・コピー・述語オプション
  - 詳細: `docs/ansi-cl-lang.md` の FR-544 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: ANSI CL 8.1.5〜8.1.7 の defstruct オプション処理 (`:constructor`/`:predicate` カスタム名)

- [ ] **FR-546** `defstruct :type`/`:conc-name`/スロットオプション
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

- [ ] **FR-044** `apply` Fast Path (既知引数数)
  - 詳細: `docs/runtime-core.md` の FR-044 セクションを参照
  - 対象: `packages/vm/src/vm.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: 既知引数数の `apply` 呼び出しを専用高速パスでディスパッチ

- [ ] **FR-047** Format 文字列コンパイル時処理
  - 詳細: `docs/runtime-core.md` の FR-047 セクションを参照
  - 対象: `packages/vm/src/io.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: `(format nil "~A ~A" x y)` のような静的フォーマット文字列をコンパイル時に解析

- [ ] **FR-048** Hash Table 操作特化
  - 詳細: `docs/runtime-core.md` の FR-048 セクションを参照
  - 対象: `packages/vm/src/hash.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: `gethash`/`(setf gethash)` のホットパス特化。デフォルト `:test #'eql` 時のインライン化

- [ ] **FR-049** Equality 述語特化 (`eq`/`eql`/`equal`)
  - 詳細: `docs/runtime-core.md` の FR-049 セクションを参照
  - 対象: `packages/vm/src/primitives.lisp`, `packages/compile/src/codegen.lisp`
  - 内容: 型情報から `eq`/`eql`/`equal` の最速パスを選択するコンパイル時ディスパッチ

- [ ] **FR-441** `sort`/`stable-sort` ベクタ対応
  - 詳細: `docs/runtime-core.md` の FR-441 セクションを参照
  - 対象: `packages/expand/src/macros-stdlib.lisp`
  - 内容: ベクタ入力時にインプレースソート (quicksort/mergesort on vector)

- [ ] **FR-442** `make-array` キーワード引数コンパイル
  - 詳細: `docs/runtime-core.md` の FR-442 セクションを参照
  - 対象: `packages/compile/src/codegen-phase2.lisp`
  - 内容: `make-array` キーワード引数をコンパイルして VM 命令に渡す

- [ ] **FR-447** `defstruct :print-function`/`:print-object`
  - 詳細: `docs/runtime-core.md` の FR-447 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: `:print-object` 指定時に `print-object` メソッド生成。`:print-function` は旧互換

- [ ] **FR-448** `defstruct :type list`/`:type vector`
  - 詳細: `docs/runtime-core.md` の FR-448 セクションを参照
  - 対象: `packages/expand/src/expander-defstruct.lisp`
  - 内容: `(:type list)` — 構造体をリスト表現。`(:type vector)` — ベクタ表現。`:named` オプションと組み合わせ

- [ ] **FR-451** `search` 汎用シーケンス
  - 詳細: `docs/runtime-core.md` の FR-451 セクションを参照
  - 対象: `packages/expand/src/macros-sequence.lisp`
  - 内容: `(search sequence1 sequence2 &key test key start1 end1 start2 end2)` — 部分シーケンス検索

- [ ] **FR-453** `map-into` 複数ソース
  - 詳細: `docs/runtime-core.md` の FR-453 セクションを参照
  - 対象: `packages/expand/src/macros-sequence.lisp`
  - 内容: 複数ソースシーケンスから並列にマッピング。最短ソースで停止

---

## Phase 5: デバッグ・診断ツール (Medium)

- [ ] **FR-506** エラー回復 (Error Recovery)
  - 詳細: `docs/tooling-debug.md` の FR-506 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`
  - 内容: フォームレベルのエラー回復。1フォームのコンパイル失敗を記録しつつ次フォームに継続

- [ ] **FR-313** コールスタック整形表示
  - 詳細: `docs/tooling-debug.md` の FR-313 セクションを参照
  - 対象: `packages/vm/src/vm.lisp`, `packages/vm/src/vm-run.lisp`
  - 内容: `vm-print-backtrace`: `vm-call-stack` を走査し、return-pc をラベルテーブル経由で関数名に解決

- [ ] **FR-317** Structured Diagnostics
  - 詳細: `docs/tooling-debug.md` の FR-317 セクションを参照
  - 対象: `packages/parse/src/diagnostics.lisp`, `packages/vm/src/conditions.lisp`
  - 内容: `error-code` フィールド (e.g., `E0001`) 追加。`fix-it` フィールド (修正候補テキスト+span) 追加

- [ ] **FR-318** Compiler Warning System
  - 詳細: `docs/tooling-debug.md` の FR-318 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`, `packages/parse/src/diagnostics.lisp`
  - 内容: 未使用変数/import、型不一致、到達不能コード、非推奨関数使用の警告

- [ ] **FR-312** REPL 拡張 (補完・履歴)
  - 詳細: `docs/tooling-debug.md` の FR-312 セクションを参照
  - 対象: `packages/cli/src/main.lisp`
  - 内容: コマンド履歴 (上下矢印)、タブ補完 (パッケージシンボル+関数レジストリ)、複数行編集

- [ ] **FR-316** ベンチマーク/プロファイリングフレームワーク
  - 詳細: `docs/tooling-debug.md` の FR-316 セクションを参照
  - 対象: `packages/testing-framework/src/framework-compiler.lisp`, `packages/vm/src/vm-run.lisp`
  - 内容: `defbenchmark` マクロ (ウォームアップ+計測反復+統計出力)。VM 命令カウンタ/プロファイラ

- [ ] **FR-350** Line/Branch カバレッジ
  - 詳細: `docs/tooling-debug.md` の FR-350 セクションを参照
  - 対象: `packages/compile/src/codegen.lisp`, `packages/testing-framework/src/framework-compiler.lisp`
  - 内容: `--coverage` コンパイルフラグでソース行・分岐ごとにカウンタ命令を埋め込み

- [ ] **FR-353** Property-Based Testing 深化
  - 詳細: `docs/tooling-debug.md` の FR-353 セクションを参照
  - 対象: `packages/testing-framework/src/framework-fuzz.lisp`
  - 内容: shrinking (失敗入力の最小化)、stateful PBT (コマンドシーケンス生成)

---

## Phase 6: 高度最適化 (Medium-Hard)

- [ ] **FR-128** Typecase ジャンプテーブル
  - 詳細: `docs/tooling-compiler.md` の FR-128 セクションを参照
  - 対象: `packages/expand/src/macros-basic.lisp`
  - 内容: `(typecase x (fixnum ..) (cons ..))` をネスト if でなく型タグインデックスのジャンプテーブルにコンパイル

- [ ] **FR-130** Perfect Hash for Compiler Dispatch
  - 詳細: `docs/tooling-compiler.md` の FR-130 セクションを参照
  - 対象: `packages/expand/src/expander.lisp`
  - 内容: `compiler-macroexpand-all` の 25 分岐 `cond` をコンパイル時生成の minimal perfect hash に変換

- [ ] **FR-136** Character Class ルックアップテーブル
  - 詳細: `docs/runtime-core.md` の FR-136 セクションを参照
  - 対象: `packages/parse/src/cl/lexer.lisp`, `packages/vm/src/strings.lisp`
  - 内容: `vm-alpha-char-p` 等を 256 バイト配列ルックアップに変換

- [ ] **FR-137** String Literal Pool
  - 詳細: `docs/runtime-core.md` の FR-137 セクションを参照
  - 対象: `packages/compile/src/codegen-core.lisp`
  - 内容: コンパイル時の `*string-literal-pool*` で同一文字列リテラルを単一 `vm-const` に重複排除

- [ ] **FR-152** 推移的関数純粋性推論
  - 詳細: `docs/tooling-compiler.md` の FR-152 セクションを参照
  - 対象: `packages/optimize/src/effects.lisp`, `packages/optimize/src/optimizer.lisp`
  - 内容: コールグラフを走査して純粋関数を推移的に伝播。DCE/CSE の対象に含める

- [ ] **FR-183** Known Function Property Database
  - 詳細: `docs/runtime-core.md` の FR-183 セクションを参照
  - 対象: `packages/compile/src/builtin-registry.lisp`, `packages/optimize/src/optimizer.lisp`
  - 内容: 各ビルトイン関数に属性付与: `:pure`、`:foldable`、`:nonneg-result`、`:always-returns`

- [ ] **FR-562** Unicode サポート
  - 詳細: `docs/ansi-cl-lang.md` の FR-562 セクションを参照
  - 対象: `packages/parse/src/cl/lexer.lisp`, `packages/vm/src/strings.lisp`
  - 内容: Unicode code point 全域 (U+0000〜U+10FFFF)。`char-code`/`code-char` の 21-bit 対応。UTF-8/UTF-16 ストリームエンコーディング

---

## Phase 7: Very Hard (別プロジェクト扱い)

以下は根幹変更が必要なため、上記フェーズ完了後に別途検討。

- [ ] Native backend bignum 値表現 — ANSI stdlib VM 実装は FR-605 で完了。x86-64 native backend の自前 bignum タグ/桁配列表現は別トラックで検討
- [x] **FR-580** FFI / CFFI 互換層 — VM interpreter の host-backed 最小 CFFI 互換 shim は完了。portable/native libffi backend は別トラックで検討
- [ ] **FR-800** 完全第一級継続 — CPS 変換全体の再設計

---

## 完了済み

_(実装後ここに移動)_
