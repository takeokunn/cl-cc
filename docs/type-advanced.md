# Type System: Advanced

Safety-oriented types, development support, type-level programming, advanced type constructors, termination/totality, types and proofs, type system tooling, concurrency/distributed types, numeric/quantitative types, implementation techniques, dynamic language integration, advanced typeclasses, domain-specific types, types and semantics, type analysis for optimization.

> 注: 本書は `type-core.md` の後続であり、コア型判断や codegen 連携の前提にはしない。ここにある要件は、前提条件が満たされた後に導入する拡張群として読むこと。
> 本シリーズにおける FR 番号は型システム文書ローカルの参照名であり、他の設計文書に現れる同番号の FR とは無関係である。
> Tier 表の `dep ref` は、この巻で定義していない前提 FR への参照を表す。この巻で本文定義を持つ FR には `dep ref` を付けない。

## 完了ステータス

- 🔶 **進行中**: この文書で追跡している advanced FR は 109 件中 5 件のみ `✅` で完了表記されています。
- `packages/type/src/` と `packages/type/tests/` には advanced 型機能の実装・テスト基盤がありますが、本文内の個別 FR 完了マーキングはまだ完了していません。
- `新ファイル` とある対象欄は初期設計時の配置案を含みます。現行実装では `types-extended.lisp` や `generics.lisp`、`channels.lisp`、`actors.lisp`、`stm.lisp`、`coroutines.lisp`、`simd.lisp`、`routing.lisp`、`utils.lisp` などへ統合されている項目があります。
- `docs/README.md` では、この文書を `IN PROGRESS` として集計します。全 FR を完了扱いにするには、各 FR の実装・テスト根拠を確認し、見出しの `✅` を更新してください。

---

## 目次

15. [安全性指向型](#15-安全性指向型)
16. [開発支援型機能](#16-開発支援型機能)
17. [型レベルプログラミング](#17-型レベルプログラミング)
18. [高度な型構成子](#18-高度な型構成子)
19. [停止性・全域性・正当性](#19-停止性全域性正当性)
20. [型と証明](#20-型と証明)
21. [型システムとツール連携](#21-型システムとツール連携)
22. [並行・分散・非同期型](#22-並行分散非同期型)
23. [数値・計量型](#23-数値計量型)
24. [型システムの実装技術](#24-型システムの実装技術)
25. [動的言語の型統合](#25-動的言語の型統合)
26. [高度な型クラス機構](#26-高度な型クラス機構)
27. [特殊ドメイン型](#27-特殊ドメイン型)
28. [型と意味論の対応](#28-型と意味論の対応)
29. [最適化のための型解析](#29-最適化のための型解析)
30. [型システムの拡張性と相互運用](#30-型システムの拡張性と相互運用)
31. [線形論理と型](#31-線形論理と型)
32. [代数的サブタイピングとパス依存型](#32-代数的サブタイピングとパス依存型)
33. [TypeScript 型システムの精髄](#33-typescript-型システムの精髄)
34. [型のエンコーディングと依存型の基盤](#34-型のエンコーディングと依存型の基盤)

---

## 15. 安全性指向型

### FR-1501: Null 安全 / Optional 型 (Null Safety)

- **対象**: `packages/type/src/inference.lisp`, `packages/vm/src/primitives.lisp`
- **内容**: `nil` / `null` を型システムに組み込んで Null 参照エラーを静的に防ぐ
  - Kotlin: `String`（非null）と `String?`（null 許容）を型レベルで区別
  - Swift: `Optional<T>` = `T?`。`if let` / `guard let` で安全にアンラップ
  - Haskell: `Maybe a` モナド — `Nothing`（null相当）を明示的に型で表現
  - CL の `nil` は多義的（空リスト・false・null）→ `(or null T)` として明示型付け
  - **`!` 演算子**: Kotlin の `!!` — プログラマが null でないと保証する型アサーション（型エラーを実行時エラーに変換）
- **難易度**: Medium

### FR-1502: 並行安全型 (Concurrency Safety Types)

- **対象**: 新ファイル `packages/type/src/concurrency.lisp`
- **内容**: スレッド間のデータ共有の安全性を型で保証
  - Rust の `Send` / `Sync` マーカートレイト:
    - `Send`: 値をスレッド間で移動できる（オーナーシップ転送が安全）
    - `Sync`: 値への参照をスレッド間で共有できる（`&T` が `Send` ならば `T: Sync`）
  - `(declare (not-send mutex-guard))` — `MutexGuard` はスレッド間移動不可
  - Java の `@NotThreadSafe` アノテーションを型レベルに昇格させた形
  - **データ競合の静的防止**: `Send`/`Sync` がなければ `spawn` に渡せない → データ競合がコンパイルエラー
- **難易度**: Hard

### FR-1503: 情報流型 / Taint 解析 (Information Flow Types)

- **対象**: 新ファイル `packages/type/src/security.lisp`
- **内容**: データの機密レベルを型で追跡し、情報漏洩を静的に防ぐ
  - **セキュリティラベル**: `(secret integer)` — 秘密の整数。`(public integer)` — 公開の整数
  - **不変条件**: `secret` な値は `public` な文脈で使えない（「高い情報が低い情報に流れない」）
  - **明示的な情報流**: `(declassify x reason)` — 意図的な機密解除。型に証跡が残る
  - DIF (Dependent Information Flow)、Jif (Java + Information Flow)
  - CL-CC 応用: ユーザー入力をすべて `(tainted string)` として型付け。SQL クエリに渡す前にサニタイズ必須
- **難易度**: Very Hard

### FR-1504: 領域型 (Region Types)

- **対象**: 新ファイル `packages/type/src/regions.lisp`
- **内容**: メモリ領域を型で表現し、GC なしで安全なメモリ解放を実現
  - Tofte & Talpin (1994) の領域推論: プログラムを解析して各値の「生存領域」を推論
  - `(region r (let ((x (alloc r 42))) ...))` — 領域 `r` のライフタイム内のみ `x` が有効
  - **領域多相**: `(forall r. (function ((list-in r :a)) :b))` — 任意の領域のリストを受け取る
  - ML Kit (SML の領域ベース実装)、Rust のライフタイムは領域型の実用版
  - **GC との比較**: 領域型は GC の完全な代替にはなれないが、短命なオブジェクトの割り当て/解放を O(1) で実現
- **難易度**: Very Hard

### FR-1505: 能力型 (Capability Types)

- **対象**: 新ファイル `packages/type/src/capabilities.lisp`
- **内容**: 操作の「権限」を型で表現するオブジェクト能力モデル
  - `(capability :file-write)` を持つ関数のみがファイルに書き込める
  - **能力の委譲**: 能力を関数の引数として明示的に渡す（グローバルな権限チェックではなく局所的）
  - **能力の制限**: `(restrict-capability cap :read-only)` — 書き込み能力を除去した能力を派生
  - Joe-E (Java + object capabilities)、Wyvern、Pony (capabilities + reference capabilities)
  - エフェクト型 (FR-401) との関係: 能力型はエフェクトを「どのオブジェクトが持つか」で表現したもの
- **難易度**: Very Hard

---

## 16. 開発支援型機能

### FR-1601: ✅ 型付きホール (Typed Holes)

- **対象**: `packages/compile/src/codegen.lisp`, REPL (`packages/cli/src/main.lisp`)
- **内容**: 未実装部分を `_` (hole) で表し、型エラーとして期待型を報告
  - GHC: `_ :: Int` → `Found hole: _ :: Int`。コンテキスト内の使用可能な変数も列挙
  - Idris 2: インタラクティブに hole を埋めるプログラミングスタイル（定理証明補助と同等）
  - `(defun foo (x) (+ x _))` → コンパイルエラー: `hole at position 2: expected type integer, available: x :: integer`
  - **型駆動開発 (Type-Driven Development)**: 型を先に書き、hole を段階的に埋める手法
  - CL-CC の REPL に `?` / `_` hole 構文を追加
- **難易度**: Medium

### FR-1602: データ型ジェネリクス (Datatype-Generic Programming)

- **対象**: 新ファイル `packages/type/src/generics.lisp`
- **内容**: データ型の代数的構造（積・和）を使って汎用的な操作を定義
  - **GHC Generics**: `class Generic a where type Rep a :: * -> *` — 型をその「表現型」に変換
  - **表現型**: `U1`（単位）、`K1`（定数）、`M1`（メタ情報）、`:*:`（積）、`:+:`（和）
  - **汎用関数**: `(generic-show x)` — 任意の `Generic` インスタンスを文字列化
  - **Scrap Your Boilerplate (SYB)**: 型を横断する汎用的な変換・クエリ操作
  - CL の `mop:class-slots` + `slot-value` による反射的操作の型安全版として実装可能
- **難易度**: Hard

### FR-1603: ✅ エフェクト推論 (Effect Inference)

- **対象**: `packages/type/src/effects.lisp` (FR-401 の拡張)
- **内容**: プログラマがエフェクトアノテーションを書かなくても、コンパイラが自動推論
  - `(defun foo (x) (print x) (* x 2))` → コンパイラが `! (:io)` を自動推論
  - **区間推論 (Interval Inference)**: 最小のエフェクトセットを下限として推論
  - **エフェクトの単純化**: `(:io :io)` → `(:io)`。エフェクトセットの正規化
  - Koka の自動エフェクト推論（人気の理由の一つ）
  - HM 型推論と並行して実行可能（エフェクト変数を型変数と同様に unification で解く）
- **難易度**: Hard

### FR-1604: ✅ 値制限 (Value Restriction)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 変更可能な参照と多相型の相互作用による健全性破壊を防ぐ
  - **問題**: `(let ((r (ref nil))) ...)` — `r` が `(ref (forall :a :a))` と推論されると型安全性が破れる
  - **ML の値制限**: 変数宣言の右辺が「値」（副作用のない式）でない場合、多相化しない
  - `(setq x (make-list 10))` — `x` は `(list :a)` でなく `(list t)` に単相化
  - CL-CC: `defvar` / `setq` への代入が型多相性をどう制限するかのポリシー定義
  - Garrigue (2004). Relaxing the value restriction.
- **難易度**: Medium

### FR-1605: 型エラーの品質 (Type Error Quality)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 型エラーメッセージを人間が理解しやすくする
  - **エラーの局所化**: HM の unification エラーは発生箇所と根本原因が離れることが多い。正確なエラー位置を特定
  - **型エラーの説明**: 「なぜこの型を期待したか」の連鎖を表示
  - **候補の提示**: 型が合わない場合に「こうすれば型が合う」候補を列挙
  - **Elm のアプローチ**: コンパイラがエラーを「診断」として丁寧に説明（2015年から業界標準に）
  - GHC の `-fdiagnostics-as-json` (GHC 9.4+): 構造化エラー情報
  - Rust のエラーコード (`E0308` 等) + 詳細説明 (`rustc --explain E0308`)
- **難易度**: Medium

### FR-1606: インクリメンタル型検査 (Incremental Type Checking)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: ファイルの一部変更時に型推論全体を再実行せず差分のみ再検査
  - **依存グラフ**: 関数・型の定義間の型依存グラフを構築。変更の影響範囲を限定
  - **キャッシュ**: 変更されていないモジュールの型推論結果をキャッシュ
  - IDE 統合: LSP (Language Server Protocol) での `textDocument/hover` に型情報を高速提供
  - Haskell Language Server (HLS) の `ghcide` ベース: 関数単位の型検査キャッシュ
  - Rust-Analyzer: Salsa フレームワークによる需要駆動インクリメンタル計算
- **難易度**: Hard

---

## 17. 型レベルプログラミング

### FR-1701: 型レベル自然数 (Type-Level Natural Numbers)

- **対象**: `packages/type/src/parser.lisp`
- **内容**: コンパイル時に自然数を型として扱い、値の長さや次元を型に埋め込む
  - GHC の `GHC.TypeNats`: `(KnownNat n) =>` で実行時に `natVal` として取り出し可能
  - `(vector 3 integer)` — 長さ 3 の整数ベクタ。長さが型に現れる
  - 型レベル算術: `(type-plus 2 3) = 5`, `(type-mul m n)` — 型レベルで計算
  - **アプリケーション**: 行列型 `(matrix m n float)` — `(matrix-mul (matrix m k float) (matrix k n float))` が型安全
  - Idris 2 の `Vect n a`, Agda の `Vec A n`
- **難易度**: Hard

### FR-1702: 型レベル文字列 (Type-Level Strings / Symbol Kind)

- **対象**: `packages/type/src/parser.lisp`
- **内容**: 文字列リテラルを型として使用
  - GHC の `Symbol` カインド: `"foo" :: Symbol`
  - `(has-field "name" string)` — `"name"` という名前のフィールドを持つレコード型
  - **型安全レコードアクセス**: `(get-field "name" record)` が `"name"` フィールドを持たない型にアクセスするとコンパイルエラー
  - Haskell の `vinyl` / `data-has` ライブラリで活用
  - TypeScript の template literal types: `` `on${Capitalize<string>}` ``
- **難易度**: Hard

### FR-1703: 多段階プログラミング (Multi-Stage Programming)

- **対象**: 新ファイル `packages/type/src/staging.lisp`
- **内容**: プログラムの「実行段階」を型で区別し、コンパイル時計算と実行時計算を型安全に混在
  - **ステージ 0**: 実行時の値。通常の式
  - **ステージ 1 (Code)**: コンパイル時に生成されるコードの型 `(code integer)` — MetaML の `<| expr |>`
  - **splice**: `(splice e)` — `(code t)` の値を実行時に展開 (`~e` に相当)
  - **run**: `(run code-expr)` — コードを実行してステージを下げる
  - MetaML (Taha & Sheard 1997)、BER MetaOCaml、Haskell の Template Haskell (`Q` モナド)
  - CL-CC 応用: マクロ展開の型安全版として実装可能。`read-time-eval` の型付き代替
- **難易度**: Very Hard

### FR-1704: 型安全メタプログラミング (Typed Metaprogramming)

- **対象**: `packages/expand/src/expander.lisp`
- **内容**: マクロ生成コードに型安全性を保証
  - Template Haskell: `Q (TExp a)` — 型 `a` のコードを生成するアクション。生成コードが型検査を通る保証
  - **Typed Template Haskell**: `[|| expr ||]` で typed splice。型が一致しない splice はコンパイルエラー
  - CL の `defmacro` との違い: 通常の CL マクロは S 式を返すため型安全性なし
  - **型付き準引用 (Typed Quasiquotation)**: `` `(+ ,x 1) `` で `x: integer` が保証されるなら生成コードも `integer`
  - CL-CC 応用: `define-vm-instruction` マクロの生成コードに型情報を付与
- **難易度**: Very Hard

### FR-1705: バインディング時解析 (Binding-Time Analysis)

- **対象**: 新ファイル `packages/type/src/binding-time.lisp`
- **内容**: 式の各部分が「コンパイル時」と「実行時」のどちらで評価されるかを型で追跡
  - **静的 (Static)**: コンパイル時に値が確定。定数畳み込みやインライン展開の前提
  - **動的 (Dynamic)**: 実行時まで値が不定
  - **BTA (Binding-Time Analysis)**: プログラムを解析して各式に Static/Dynamic ラベルを付与
  - **部分評価 (Partial Evaluation)**: BTA の結果を使ってコンパイル時に式を部分的に評価
  - Jones, Gomard, Sestoft (1993). Partial Evaluation and Automatic Program Generation.
  - CL の `eval-when` との関係: `:compile-toplevel` vs `:execute` は粗いバインディング時区別
- **難易度**: Very Hard

---

## 18. 高度な型構成子

### FR-1801: Lens / Optics (van Laarhoven Lenses)

- **対象**: 新ファイル `packages/type/src/optics.lisp`
- **内容**: 型安全なデータ構造のゲッター/セッターの合成可能な抽象
  - **Lens**: `(lens :a :b :s :t)` — `:s` 型の構造から `:a` を取り出し、`:b` で更新すると `:t` になる
  - **van Laarhoven 表現**: `(forall f. functor f => (a -> f b) -> s -> f t)` — lens は単なる高階関数
  - **合成**: `(compose lens1 lens2)` でネストした構造に対してアクセス
  - **Prism**: sum 型のフォーカス。`(prism :a :b :s :t)` — `(or :s :t)` への選択的アクセス
  - **Traversal**: 複数の要素にフォーカス。`(traversal :a :s)` — コンテナ内全要素を一括更新
  - Haskell の `lens` / `optics` パッケージ。Scala の Monocle
  - CL-CC 応用: CLOS スロットアクセスを lens として型付け
- **難易度**: Very Hard

### FR-1802: 自由モナド (Free Monads)

- **対象**: `packages/type/src/typeclasses.lisp`
- **内容**: 任意のファンクタからモナドを構築する汎用的な構造
  - `(deftype (free :f :a) (or (:pure :a) (:free (:f (free :f :a)))))` — 自由モナドの定義
  - **インタープリタパターン**: DSL をデータとして表現し、インタープリタで意味を与える
  - `(deftype io-op (or (:read-line) (:print-line string) ...))` + `(free io-op :a)` — I/O を純粋にモデル化
  - Freer Monad (Kiselyov & Ishii 2015): 型クラス制約なしに代数的エフェクトを実装
  - エフェクトシステム (FR-401/402) の代替実装経路として CL-CC に適用可能
- **難易度**: Hard

### FR-1803: 異種リスト (Heterogeneous Lists / HLists)

- **対象**: `packages/type/src/typeclasses.lisp`
- **内容**: 要素ごとに異なる型を持つ型安全リスト
  - `(hlist integer string boolean)` — 要素数と各要素の型が型に現れる
  - **HList の操作**: `hhead :: HList (a:as) -> a`, `htail :: HList (a:as) -> HList as`
  - Kiselyov et al. (2004). Strongly typed heterogeneous collections.
  - **型安全タプル**: `(hlist a b c)` は `(values a b c)` の型安全版
  - **型安全レコード**: フィールド名とフィールド型のペアのHList = 型安全レコード
  - CL-CC 応用: `multiple-values` の型付けに応用可能
- **難易度**: Hard

### FR-1804: 型安全 printf (Type-Safe printf)

- **対象**: `packages/vm/src/io.lisp`
- **内容**: フォーマット文字列から引数の型を型レベルで導出
  - Haskell の `printf` trick: `"%d %s"` から `(function (integer string) string)` を型推論
  - `(format-type "%d %s %f")` → `(function (integer string float) string)`
  - 型レベル文字列 (FR-1702) + 型族 (FR-107) で実装可能
  - CL の `format` は実行時にフォーマット文字列を解析。型安全版では `:format` フォーマット文字列を型レベルで処理
  - **Typed Format Strings**: Rust の `println!("{}", x)` — コンパイル時にフォーマット文字列を解析
- **難易度**: Very Hard

### FR-1805: Profunctor / Traversal の型

- **対象**: `packages/type/src/typeclasses.lisp`
- **内容**: Lens の一般化。入力と出力に異なる変性を持つ型構成子
  - `(type-class profunctor (:p) (dimap (b -> a) (c -> d) (p a c) -> (p b d)))` — 入力に反変、出力に共変
  - **Optics の統一的定義**: すべての Optics (Lens/Prism/Traversal/Fold) を Profunctor の制約で統一記述
  - `(type lens :s :t :a :b (forall p. strong p => p a b -> p s t))` — Strong Profunctor で Lens を定義
  - Riley (2018). Categories of Optics. — Optics の圏論的統一理論
  - Haskell の `profunctors` パッケージ
- **難易度**: Very Hard

### FR-1806: 継続型 / CPS の型 (Continuation Types)

- **対象**: `packages/compile/src/cps.lisp`
- **内容**: 継続を第一級の型として扱う
  - **継続モナド**: `(cont :r :a) = (function (function :a :r) :r)` — `Cont r a`
  - **shift/reset の型**: `shift :: ((a -> r) -> r) -> Cont r a`, `reset :: Cont a a -> a`
  - **区切られた継続の型階層**: `(prompt :a)` タグ付き reset、`(subcont :a :b)` 部分継続
  - CL-CC との直接対応: CPS 変換後のコードは継続モナドの正準的実装
  - Danvy & Filinski (1990). Abstracting Control.
  - **負の型としての継続**: `(not :a) = (function :a void)` — 継続は「未来の計算」= 負の型
- **難易度**: Hard

---

## 19. 停止性・全域性・正当性

### FR-1901: 停止性検査 (Termination Checking)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: すべての再帰関数が停止することをコンパイル時に検証
  - **構造的再帰**: 再帰呼び出しの引数が常に「より小さい」部分構造であることをチェック
  - `(defun length (xs) (if (null xs) 0 (1+ (length (cdr xs)))))` — `cdr` で常に縮小 → 停止
  - **辞書式順序**: 複数引数の辞書式順序での縮小を検査
  - **サイズ変化グラフ**: すべての呼び出しパスでサイズが減少するか確認 (Sipser 1996)
  - Agda / Idris 2 の停止性検査器
  - **非停止関数のマーキング**: 停止性が証明できない関数を `(partial defun ...)` として明示
- **難易度**: Hard

### FR-1902: 全域性検査 (Totality Checking)

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: すべての関数が任意の入力に対して値を返すことを検証（例外・無限ループなし）
  - 全域 = **停止性** (FR-1901) + **網羅性** (FR-1903)
  - **全域関数の利点**: メモ化・並列化・等式変換の候補として扱える
  - **部分関数の明示**: `(partial integer -> integer)` — 失敗する可能性のある関数型
  - Idris 2 は全域性を言語レベルで強制。GHC は `-XSafe` + totality プラグイン
  - **Turing 完全性との関係**: 停止性は決定不可能 (Halting Problem)。実用的には「十分なクラス」で検査
- **難易度**: Hard

### FR-1903: ✅ 網羅性検査 (Exhaustiveness / Coverage Checking)

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: パターンマッチが全ケースをカバーしているかをコンパイル時検証
  - `(typecase x (integer ...) (string ...))` — `symbol` 型が欠落していれば警告
  - **有用性検査 (Usefulness)**: 到達不可能なパターンを検出
  - Maranget (2007). Warnings for pattern matching. — 実用的アルゴリズム
  - **GADT パターンマッチ**: GADT の各コンストラクタに対する型精緻化を考慮した網羅性
  - CL-CC: `(our-typecase x ...)` の各 arm に対して網羅性警告を実装
- **難易度**: Medium

### FR-1904: 正値性検査 (Positivity Checking)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 帰納的データ型の定義が矛盾を生まないことを保証
  - **否定的出現 (Negative Occurrence)**: `(defgadt bad (mk (bad -> bad) => bad))` — `bad` が引数位置に出現 → 矛盾
  - **厳密正値性 (Strict Positivity)**: データコンストラクタの引数に `T` 自身が負の位置に現れないことを要求
  - 違反すると `(bad -> bad)` 型の不動点が構築でき、型システムが矛盾する (Curry のパラドックス)
  - Agda / Coq / Idris の positivity checker
- **難易度**: Hard

### FR-1905: サイズ型 (Sized Types)

- **対象**: 新ファイル `packages/type/src/sized.lisp`
- **内容**: データ構造の「サイズ」を型に記録して停止性を自動証明
  - `(sized-list :s :a)` — サイズ上限 `:s` の型変数を持つリスト
  - `(cdr xs :: sized-list (pred s) :a)` — `cdr` はサイズを 1 減らす
  - **サイズ変数の算術**: `(sized-list (plus m n) :a)` = サイズ `m+n` のリスト
  - Abel (2008). MiniAgda: Integrating Sized and Dependent Types.
  - 停止性検査器が証明できないケースをサイズ型アノテーションで補助
- **難易度**: Very Hard

### FR-1906: ガード再帰 (Guarded Recursion)

- **対象**: `packages/type/src/coinductive.lisp` (FR-1204 の余帰納型と連携)
- **内容**: 余帰納的定義が生産的 (productive) であることを型で保証
  - **後演算子 (Later Modality)**: `▶ A` — 「1 ステップ後に利用可能な `A`」
  - `(defcodata stream (:a) (head :a) (tail (later (stream :a))))` — `tail` の再帰呼び出しは `▶` で遅延
  - 生産性: 有限回の展開で常に次の要素が得られることを型で保証
  - Nakano (2000). A Modality for Recursion.
  - Clouston et al. (2015). Programming and Reasoning with Guarded Recursion for Coinductive Types.
- **難易度**: Very Hard

---

## 20. 型と証明

### FR-2001: Curry-Howard 対応の実装

- **対象**: 新ファイル `packages/type/src/props.lisp`
- **内容**: 型をそのまま論理命題として使用し、値を証明として扱う
  - **型 = 命題、値 = 証明**: `(function :a :b)` = 「`:a` ならば `:b`」
  - **積型 = 連言**: `(pair :a :b)` = `A ∧ B`
  - **和型 = 選言**: `(or :a :b)` = `A ∨ B`
  - **依存型 = 全称量化**: `(pi (:x :a) (p :x))` = `∀x:A. P(x)`
  - **単位型 = 真**: `unit` = `⊤`。**空型 = 偽**: `void` = `⊥`
  - **CL-CC への応用**: 関数の事前/事後条件を命題として型に組み込む (FR-602 との連携)
- **難易度**: Very Hard

### FR-2002: 証明付きコード (Proof-Carrying Code)

- **対象**: 新ファイル `packages/type/src/pcc.lisp`
- **内容**: コードに機械検証可能な正当性証明を添付
  - Necula (1997). Proof-Carrying Code.
  - `(defun safe-div (n d (proof (not (zerop d)))) ...)` — 分母非ゼロの証明を引数として受け取る
  - **証明の生成**: SMT ソルバ (Z3/CVC5) で自動生成。複雑なケースは手動
  - **証明の検証**: 証明チェッカーは実行する前に証明を高速検証
  - **JIT の安全性証明**: 動的生成コードが型安全であることを証明付きで保証
- **難易度**: Extremely Hard

### FR-2003: タクティックベース型検査 (Tactic-Based Type Checking)

- **対象**: 新ファイル `packages/type/src/tactics.lisp`
- **内容**: 型等値の証明をタクティックで対話的に構築
  - Coq の `simpl`, `rewrite`, `reflexivity`, `induction` 等のタクティック
  - Lean 4 の `simp`, `ring`, `omega` — 自動タクティック
  - **`omega` タクティック**: 線形算術の自動証明 (Presburger arithmetic)
  - **`ring` タクティック**: 環の等式の自動証明
  - CL-CC 応用: 型レベル自然数 (FR-1701) の等値証明を `omega` で自動化
- **難易度**: Extremely Hard

### FR-2004: ホモトピー型理論 (Homotopy Type Theory / HoTT)

- **対象**: 新ファイル `packages/type/src/hott.lisp`
- **内容**: 型理論と幾何学的ホモトピー理論の融合
  - **パス型**: `(= a b : A)` — `a` と `b` の間の「道」。証明の間の同一性を表現
  - **一価性公理 (Univalence)**: `(= A B : Type) ≃ (A ≃ B)` — 同型な型は等価
  - **高次帰納型 (Higher Inductive Types)**: `(circle)` — 2 点と 1 つのパスを持つ型 (S¹)
  - **Cubical Type Theory**: 一価性公理を計算規則として追加。Agda `--cubical`、Lean 4 のベース
  - The Univalent Foundations Program (2013). Homotopy Type Theory.
- **難易度**: Extremely Hard (理論実装)

### FR-2005: 正規化による評価 (Normalization by Evaluation, NbE)

- **対象**: `packages/type/src/dependent.lisp` (FR-303 の依存型実装に必要)
- **内容**: 型チェック時に型式を正規形に評価する手法
  - 依存型では型等値判定のために型式を正規形に簡約する必要がある
  - **Glued Evaluation**: 構文的な型式と意味論的な値を同時に保持して効率的に正規化
  - **Weakening / Substitution**: 型式の変数代入と弱化を NbE で実装
  - Abel et al. (2017). Decidability of Conversion for Type Theory in Type Theory.
  - Kovács (2020). Elaboration Zoo (NbE の実用実装チュートリアル)
  - CPS 変換との関係: CPS 変換はβ 正規化の継続渡し版として解釈可能
- **難易度**: Very Hard

---

## 21. 型システムとツール連携

### FR-2101: 型指向テスト生成 (Type-Directed Test Generation)

- **対象**: `packages/testing-framework/src/framework-fuzz.lisp`
- **内容**: 型情報から自動的にテストデータを生成
  - **QuickCheck スタイル**: `(arbitrary :a)` — 型 `:a` の値を無作為生成。型クラスで各型に実装
  - `(deftest prop-reverse (xs : (list integer)) (= (reverse (reverse xs)) xs))` — 型から `xs` を生成
  - **型駆動 fuzzing**: 関数の引数型から有効な入力を大量生成してバグを探索
  - **カバレッジ誘導**: 型情報を使って未カバーの型ブランチへの入力を優先生成
  - CL-CC の `packages/testing-framework/src/framework-fuzz.lisp` に型情報を組み込む
  - Claessen & Hughes (2000). QuickCheck: A Lightweight Tool for Random Testing.
- **難易度**: Medium

### FR-2102: 型安全シリアライゼーション (Type-Directed Serialization)

- **対象**: 新ファイル `packages/type/src/serialization.lisp`
- **内容**: 型からJSON/バイナリ/S式コーデックを自動導出
  - **`deriving (ToJSON, FromJSON)`**: Haskell の aeson ライブラリ。型構造から JSON 変換を自動生成
  - `(defstruct point (x 0.0) (y 0.0) (:deriving json))` → `{"x": 0.0, "y": 0.0}` を自動で読み書き
  - **型安全デシリアライゼーション**: `(from-json json-value : point)` が型に合わない JSON でエラー
  - **スキーマ生成**: 型からJSON Schema / Protocol Buffers スキーマを自動生成
  - CL-CC 応用: `defstruct` + `defclass` から S-expression シリアライザを自動生成
- **難易度**: Medium

### FR-2103: 型安全 FFI (Type-Safe Foreign Function Interface)

- **対象**: 新ファイル `src/ffi/type-safe-ffi.lisp`
- **内容**: C/外部言語との相互運用を型で安全に記述
  - **マーシャリング型**: `(c-int n)`, `(c-ptr :a)`, `(c-string s)` — C 側の型を CL-CC 型にマップ
  - **安全な C ポインタ**: `(c-ptr :a)` は借用検査 (FR-502) と連携してダングリングポインタを防ぐ
  - **コールバック型安全性**: `(c-callback (function (c-int) c-int))` で型を検査してから関数ポインタを渡す
  - CFFI (CL の外部関数インターフェース) の型付きラッパーとして実装可能
  - Rust の `bindgen` + `unsafe` ブロック: FFI の境界を型レベルで明示
- **難易度**: Hard

### FR-2104: 型融合 / デフォレスト化 (Type-Based Fusion / Deforestation)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **内容**: 型情報を使って中間データ構造を除去する変換
  - **ショートカット融合 (Shortcut Fusion)**: GHC の `build`/`foldr` 融合。`map f . map g = map (f . g)`
  - **stream fusion**: `Data.Vector.Fusion.Stream` — ベクタ操作の中間構造を完全除去
  - **パラメトリシティからの等式**: `(map f . filter p)` の融合則は自由定理 (FR-1403) から導出
  - Gill, Launchbury, Peyton Jones (1993). A Short Cut to Deforestation.
  - CL-CC: `(map #'f (filter #'p xs))` → 中間リストを生成しないシングルパスコードへ変換
- **難易度**: Hard

### FR-2105: 型推論のパフォーマンス (Type Inference Performance)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 型推論の計算量を管理してコンパイル時間爆発を防ぐ
  - **型変数の数**: HM の型推論は多項式時間だが、型クラスや型族で指数的に膨張する可能性
  - **GHC の `-Wtype-complexity`**: 型の複雑さが閾値を超えると警告 (GHC 9.4+)
  - **Fuel-Based Reduction**: 型族の簡約ステップに上限を設けて無限ループを防ぐ
  - **キャッシュと共有**: 同じ型変数の制約を複数箇所で共有して重複計算を除去
  - **Incremental Constraint Solving**: 前回解いた制約の解を再利用
  - Rust-Analyzer の chalk (Prolog ベースの型推論)、Salsa (需要駆動計算)
- **難易度**: Hard

### FR-2106: 型安全 DSL 埋め込み (Type-Safe Embedded DSLs)

- **対象**: `packages/expand/src/expander.lisp`
- **内容**: ホスト言語の型システムを使って DSL の正しさをコンパイル時に保証
  - **型安全 SQL**: `(query (select :id :name (from :users (where (> :age 18))))` — SQL 構造が型に現れる
  - **型安全 HTML**: `(html (div (p "hello")))` — `div` の中に `p` は有効だが `tr` は無効を型で表現
  - **Typed Tagless Final (TTF)**: Carette et al. (2009). DSL の意味を型クラスとして定義
  - **最終的エンコーディング (Final Encoding)**: 初期代数的 (Free Monad) vs 最終的 (型クラス) の対比
  - CL-CC 応用: `define-vm-instruction` を型付き DSL として再設計
- **難易度**: Hard

### FR-2107: 型安全データベースクエリ (Type-Safe Database Queries)

- **対象**: 新ファイル `src/ffi/db.lisp`
- **内容**: データベーススキーマを型に反映してクエリの正しさをコンパイル時に検証
  - **スキーマ型**: `(table :users ((:id . integer) (:name . string) (:age . integer)))` — テーブルスキーマを型に
  - **型安全 JOIN**: `(join :users :posts :on (= users.id posts.user-id))` — フィールドの型互換性をコンパイル時検査
  - Haskell の Persistent / Esqueleto: テンプレート Haskell でスキーマを型に変換
  - **SQL インジェクション防止**: 型付きパラメータ経由の構築を必須にし、直接連結を避ける
  - TypeScript の Drizzle ORM / Prisma: スキーマから TypeScript 型を自動生成
- **難易度**: Hard

### Tier 1 — 近期実装 (実用価値 High / 依存少)

> 注: 以下の Tier 表は計画上の依存関係を示す。FR-005/006/302/801 などの core FR は、advanced 側に移動したという意味ではなく、`type-core.md` または同シリーズの他章にある前提依存として参照している。

| FR                | 機能                              | 難易度 | 実用価値 | 依存   |
| ----------------- | --------------------------------- | ------ | -------- | ------ |
| FR-005 (dep ref)  | 型アノテーション → Codegen 接続   | Hard   | High     | FR-002 |
| FR-006 (dep ref)  | Fixnum Fast Path                  | Medium | High     | FR-005 |
| FR-302 (dep ref)  | 出現型 / Flow-Sensitive Narrowing | Hard   | High     | FR-005 |
| FR-105 (dep ref)  | ファントム型                      | Medium | Medium   | —      |
| FR-1202 (dep ref) | ゼロコスト抽象 (Newtype)          | Medium | Medium   | —      |
| FR-1501           | Null 安全 / Optional 型           | Medium | High     | —      |
| FR-1601           | 型付きホール                      | Medium | High     | —      |
| FR-1101 (dep ref) | 変性アノテーション                | Hard   | Medium   | FR-003 |
| FR-801 (dep ref)  | subtypep 完全化                   | Hard   | High     | —      |

### Tier 2 — 中期実装 (基盤完成後)

| FR                | 機能                   | 難易度    | 実用価値 | 依存   |
| ----------------- | ---------------------- | --------- | -------- | ------ |
| FR-203 (dep ref)  | 行多相                 | Hard      | Medium   | FR-003 |
| FR-601 (dep ref)  | 段階的型付けと責任追跡 | Very Hard | High     | FR-005 |
| FR-602 (dep ref)  | 設計による契約         | Hard      | Medium   | —      |
| FR-701 (dep ref)  | エスケープ解析         | Hard      | High     | —      |
| FR-702 (dep ref)  | SROA                   | Hard      | High     | FR-701 |
| FR-104 (dep ref)  | 存在型                 | Hard      | Medium   | FR-003 |
| FR-1001 (dep ref) | 型クラス基盤           | Hard      | High     | FR-003 |
| FR-1203 (dep ref) | 再帰型 / μ型           | Hard      | Medium   | FR-003 |
| FR-1306 (dep ref) | 型ラムダ               | Hard      | Medium   | FR-101 |
| FR-1603           | エフェクト推論         | Hard      | Medium   | FR-401 |

### Tier 3 — 長期実装 (研究的・基盤依存)

| FR                | 機能                  | 難易度    | 実用価値 | 依存        |
| ----------------- | --------------------- | --------- | -------- | ----------- |
| FR-401 (dep ref)  | エフェクト型          | Very Hard | High     | FR-003      |
| FR-402 (dep ref)  | 代数的エフェクト      | Very Hard | High     | FR-401      |
| FR-501 (dep ref)  | 線形型 / アフィン型   | Very Hard | Medium   | FR-003      |
| FR-301 (dep ref)  | 精緻型 (Liquid Types) | Very Hard | High     | FR-003, SMT |
| FR-101 (dep ref)  | 高カインド型          | Very Hard | Medium   | FR-003      |
| FR-103 (dep ref)  | GADTs                 | Very Hard | Medium   | FR-003      |
| FR-1301 (dep ref) | カインド多相          | Very Hard | Medium   | FR-101      |
| FR-1404 (dep ref) | 集合論的型システム    | Very Hard | Medium   | FR-802      |
| FR-1502           | 並行安全型            | Hard      | High     | FR-501      |
| FR-1503           | 情報流型              | Very Hard | Medium   | FR-003      |

### Tier 2 追加 (新セクション由来)

| FR      | 機能                       | 難易度 | 実用価値 | 依存    |
| ------- | -------------------------- | ------ | -------- | ------- |
| FR-1903 | 網羅性検査                 | Medium | High     | —       |
| FR-2101 | 型指向テスト生成           | Medium | High     | FR-1001 |
| FR-2102 | 型安全シリアライゼーション | Medium | High     | FR-1001 |
| FR-1604 | 値制限                     | Medium | High     | FR-003  |
| FR-1802 | 自由モナド                 | Hard   | Medium   | FR-1001 |
| FR-2104 | 型融合 / デフォレスト化    | Hard   | High     | FR-1403 |
| FR-2103 | 型安全 FFI                 | Hard   | High     | —       |

### Tier 3 追加 (新セクション由来)

| FR      | 機能                     | 難易度    | 実用価値 | 依存    |
| ------- | ------------------------ | --------- | -------- | ------- |
| FR-1901 | 停止性検査               | Hard      | Medium   | FR-1203 |
| FR-1904 | 正値性検査               | Hard      | Medium   | FR-103  |
| FR-1703 | 多段階プログラミング     | Very Hard | Medium   | FR-003  |
| FR-1801 | Lens / Optics            | Very Hard | Medium   | FR-101  |
| FR-2005 | 正規化による評価 (NbE)   | Very Hard | High     | FR-303  |
| FR-1704 | 型安全メタプログラミング | Very Hard | Medium   | FR-1703 |
| FR-1905 | サイズ型                 | Very Hard | Medium   | FR-1901 |

### Tier 4 — 研究フェーズ (Extremely Hard)

| FR                | 機能                     | 難易度         | 根拠                                  |
| ----------------- | ------------------------ | -------------- | ------------------------------------- |
| FR-303 (dep ref)  | 依存型                   | Extremely Hard | Lean 4 / Idris 2 と同等の型理論が必要 |
| FR-502 (dep ref)  | 所有権型 / 借用検査      | Extremely Hard | Polonius アルゴリズム                 |
| FR-505 (dep ref)  | セッション型             | Extremely Hard | 線形型 + 双対性の完全実装             |
| FR-1302 (dep ref) | 宇宙多相                 | Extremely Hard | 依存型基盤が前提                      |
| FR-1304 (dep ref) | 非述語的多相             | Extremely Hard | 型推論の決定不可能性と戦う            |
| FR-1205 (dep ref) | 商型                     | Extremely Hard | 依存型 + cubical TT が必要            |
| FR-1405 (dep ref) | 極性型理論               | Extremely Hard | フォーカシング証明の実装              |
| FR-2002           | 証明付きコード           | Extremely Hard | SMT + 証明チェッカーの統合            |
| FR-2003           | タクティックベース型検査 | Extremely Hard | Lean 4 相当の証明エンジン             |
| FR-2004           | ホモトピー型理論         | Extremely Hard | Cubical Type Theory が必要            |

---

## 22. 並行・分散・非同期型

### FR-2201: Async/Await 型 (Async Types)

- **対象**: 新ファイル `packages/type/src/async.lisp`
- **内容**: 非同期計算の型。将来の値をファーストクラスで表現
  - **Future / Promise**: `(future :a)` — 将来完了する `a` 型の計算
  - **async 関数の型**: `(async-function (:a) :b)` = `(function (:a) (future :b))`
  - **await の型**: `(await (future :a)) : :a` — 非同期コンテキスト内でのみ使用可能
  - **色付き関数問題 (Colored Functions)**: 非同期関数を同期文脈から呼べないことを型で保証
  - Rust の `async`/`await` (`Future<Output=T>`)、Kotlin Coroutines (`Deferred<T>`)
  - **構造化並行性 (Structured Concurrency)**: `async with scope` — スコープを超えてタスクが漏れないことを型で保証
- **難易度**: Hard

### FR-2202: チャンネル型 (Typed Channels)

- **対象**: 新ファイル `packages/type/src/channels.lisp`
- **内容**: メッセージパッシングのチャンネルを型付け
  - `(send-channel :a)` — 送信専用チャンネル。`(recv-channel :a)` — 受信専用チャンネル
  - **線形型との連携**: チャンネルを線形型 (FR-501) で表現し、同じメッセージを2回送信できないことを保証
  - **セッション型との連携**: チャンネルの通信プロトコルをセッション型 (FR-505) でエンコード
  - Rust の `mpsc::Sender<T>` / `Receiver<T>`、Go の型付きチャンネル `chan int`
  - **容量の型**: `(buffered-channel :a 32)` — バッファサイズ 32 のチャンネル（型レベル自然数 FR-1701 応用）
- **難易度**: Hard

### FR-2203: 型付きアクター (Typed Actors)

- **対象**: 新ファイル `packages/type/src/actors.lisp`
- **内容**: アクターモデルでのメッセージ型を静的に検査
  - **Akka Typed (Scala)**: `ActorRef[Message]` — `Message` 型のメッセージのみ受け付けるアクター
  - `(actor-ref (or (:get-count) (:increment integer) (:reset)))` — 受け付けるメッセージの直和型
  - **プロトコル型**: アクターのライフサイクル（初期状態→応答中→終了）を Typestate (FR-504) で表現
  - **分散透過性**: ローカルアクターとリモートアクターを同じ型で扱う
  - Erlang/OTP の型安全版 (Dialyzer の限界を超える)
- **難易度**: Very Hard

### FR-2204: ソフトウェアトランザクショナルメモリ型 (STM Types)

- **対象**: 新ファイル `packages/type/src/stm.lisp`
- **内容**: トランザクション内の操作と外部操作を型で区別
  - `(stm :a)` モナド — トランザクション内でのみ実行可能な操作
  - `(tvar :a)` — STM で管理される可変変数。`stm` モナド外から直接読み書き不可
  - `(atomically (stm :a) : :a)` — トランザクションを実行して結果を得る
  - **副作用の隔離**: `stm` コンテキスト内では I/O 禁止を型で保証（Haskell の `STM` モナドと同様）
  - Haskell の `Control.Concurrent.STM`
- **難易度**: Hard

### FR-2205: コルーチン型 / ジェネレータ型 (Coroutine / Generator Types)

- **対象**: 新ファイル `packages/type/src/coroutines.lisp`
- **内容**: 中断・再開可能な計算の型
  - **ジェネレータ**: `(generator :yield :a)` — `:yield` 型の値を順次生成し最終的に `:a` を返す
  - **双方向コルーチン**: `(coroutine :send :receive :return)` — 値を受け取りながら値を生成
  - Rust の `Generator` trait (不安定)、Python の `Generator[YieldType, SendType, ReturnType]`
  - **継続との関係**: コルーチンは区切られた継続 (shift/reset) のシンタックスシュガーとして実装可能
  - CL-CC との直接対応: CPS 変換はコルーチンの自然な実装基盤
- **難易度**: Hard

### FR-2206: データ並列型 (Data Parallel Types / SIMD Types)

- **対象**: 新ファイル `packages/type/src/simd.lisp`
- **内容**: SIMD 命令・GPU 計算などのデータ並列性を型で表現
  - `(simd-vector 4 float)` — 4要素 float のSIMDベクタ型
  - **ベクタ幅多相**: `(forall n. simd-vector n float)` — 幅非依存の SIMD 関数
  - Rust の `std::simd` (portable SIMD)、C++ の `std::experimental::simd`
  - **Shape Polymorphism (Dex/JAX)**: `(array (m n) float)` — 形状が型に現れる多次元配列
  - 型チェックで次元ミスマッチ（`(m n)` × `(p q)` の行列積でチェック）を静的に検出
- **難易度**: Very Hard

---

## 23. 数値・計量型

### FR-2301: 数値塔の型 (Numeric Tower Types)

- **対象**: `packages/type/src/subtyping.lisp`
- **内容**: CL の数値塔を型システムに完全反映
  - **数値塔**: `integer <: rational <: real <: complex`
  - **fixnum <: integer <: bignum**: 実装依存の具体型と抽象型の分離
  - **exactness**: `(exact integer)` vs `(inexact float)` — 正確数と不正確数の型区別
  - **contagion rules**: `(+ integer float) → float` — 演算の型昇格規則を型推論に組み込む
  - ANSI CL 12.1 の数値塔を型推論の subtyping lattice として実装
- **難易度**: Medium

### FR-2302: 計量単位型 (Units of Measure)

- **対象**: 新ファイル `packages/type/src/units.lisp`
- **内容**: 数値に物理単位を型として付与し、単位の不整合をコンパイル時に検出
  - `(measure float meters)` — メートル単位の浮動小数点数
  - **単位算術**: `(/ (measure v meters) (measure t seconds)) : (measure float meters/second)`
  - **次元解析**: `(+ meters seconds)` → コンパイルエラー（次元不整合）
  - F# の Units of Measure (Kennedy 1994/2009): 完全な実装例
  - **型消去**: 単位情報はコンパイル時のみ存在。実行時は通常の float
  - `(convert-unit 100.0 'centimeters 'meters)` → コンパイル時に変換係数を検証
- **難易度**: Hard

### FR-2303: 区間算術型 (Interval Arithmetic Types)

- **対象**: 新ファイル `packages/type/src/intervals.lisp`
- **内容**: 数値の取り得る範囲を型に記録して数値エラーを静的に防ぐ
  - `(interval-type integer 0 255)` — 0〜255 の整数（バイト型）
  - `(interval-type float 0.0 1.0)` — 0.0〜1.0 の確率値
  - **区間伝播**: `(+ (interval 0 10) (interval 5 15)) : (interval 5 25)` — 演算結果の範囲を追跡
  - **overflow 検出**: 結果区間が型の範囲を超えるとコンパイルエラー
  - 精緻型 (FR-301) のサブセットとして実装可能（`{v: Int | 0 <= v && v <= 255}` の特化版）
- **難易度**: Hard

### FR-2304: テンソル型 / 形状多相 (Tensor Types / Shape Polymorphism)

- **対象**: 新ファイル `packages/type/src/tensors.lisp`
- **内容**: 多次元配列の形状（各次元のサイズ）を型に記録
  - `(tensor (m n) float)` — m×n の float 行列
  - **形状多相関数**: `(map-rows (tensor (m n) float) -> (tensor (m k) float))` — `m` は固定、`n`→`k`
  - **行列積**: `(matmul (tensor (m k) float) (tensor (k n) float) -> (tensor (m n) float))` — `k` が一致することを型で保証
  - **ブロードキャスト型**: NumPy のブロードキャスト規則を型で表現 (Dex 言語の研究課題)
  - JAX の `jaxtyping`、Dex 言語 (Google Research)、Futhark の rank polymorphism
- **難易度**: Very Hard

### FR-2305: 固定精度型 / 任意精度型 (Fixed-Point / Arbitrary Precision Types)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 数値の精度・表現形式を型に組み込む
  - **固定小数点型**: `(fixed-point 8 4)` — 整数部8ビット、小数部4ビット（DSP/組み込みで重要）
  - **任意精度**: `(bignum)` / `(arbitrary-precision float)` — 精度を型パラメータで指定
  - **NaN/Inf の型**: `(finite float)` — NaN と Inf を含まない float。除算の安全性に応用
  - **Saturating 演算型**: オーバーフロー時に飽和（最大値に固定）する整数型
  - 精緻型 (FR-301) で `{v: Float | not (isNaN v) && not (isInf v)}` として表現可能
- **難易度**: Medium

---

## 24. 型システムの実装技術

### FR-2401: Unification アルゴリズム (Unification)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 型変数の単一化アルゴリズムの正確な実装
  - **Robinson's Unification (1965)**: 最も単純な一階 unification。最悪指数時間
  - **Martelli-Montanari (1982)**: 多集合ベースの効率的 unification。準線形時間
  - **Union-Find による実装**: Tarjan の Union-Find で型変数の等価クラスを管理
  - **発生検査 (Occurs Check)**: `unify a (list a)` は発生検査なしで無限型を生じる。Haskell は発生検査なし（意図的）
  - **Higher-Order Unification**: System F の型適用での unification は一般に決定不可能（Huet 1975）
- **難易度**: Medium

### FR-2402: 型変数のスコーピング / Skolem 化 (Type Variable Scoping / Skolemization)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 型変数のスコープとその具体化の管理
  - **剛性型変数 (Rigid Type Variables)**: 型アノテーションからの型変数。ユーザーが固定した型
  - **柔軟型変数 (Flexible/Wobbly Type Variables)**: 推論中の未確定変数。unification で具体化
  - **Skolem 化**: `(forall a. ...)` を具体的な Skolem 定数に置き換えて内部化
  - **エスケープ検査**: Skolem 変数がそのスコープ外に漏れていないことを確認（存在型の境界チェック）
  - GHC の `SkolemTv` vs `TauTv` の区別
- **難易度**: Hard

### FR-2403: 制約伝播 (Constraint Propagation)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 型制約を効率的に解く伝播アルゴリズム
  - **前方伝播**: 新しい制約が追加されたとき、既存の変数への影響を即座に伝播
  - **後方伝播**: 制約が矛盾を引き起こしたとき、原因を遡って特定
  - **制約グラフ**: 型変数間の制約を有向グラフで管理。Arc consistency (AC-3 アルゴリズム)
  - **型クラス制約の解決順序**: 制約を解く順序が型推論の完全性に影響
  - GHC の `TcTyVar` と constraint canonicalization
- **難易度**: Hard

### FR-2404: 可視的型適用 (Visible Type Application)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 多相関数に型引数を明示的に渡す構文
  - GHC `-XTypeApplications`: `id @Int 42` — `id` の型変数 `a` に `Int` を明示指定
  - `(the-type integer (identity 42))` — CL-CC での対応構文
  - **型引数の順序**: 型変数の出現順（左から右、外から内）で型適用の引数順が決まる
  - **部分型適用**: `(map @Int)` — `map` の最初の型引数のみ指定
  - **型推論補助**: 型エラーデバッグ時に中間型を固定して推論を誘導
- **難易度**: Medium

### FR-2405: 型インターフェースファイル (Type Interface Files)

- **対象**: `src/package.lisp`, ビルドシステム
- **内容**: モジュール間の型情報を保存して再利用するしくみ
  - GHC の `.hi` ファイル: モジュールのエクスポートした型シグネチャをキャッシュ
  - OCaml の `.mli` / `.cmi` ファイル: 型インターフェースを明示的に記述
  - **インターフェース検査**: 実装が宣言したインターフェースを満たすことをコンパイル時に検証
  - **別コンパイル (Separate Compilation)**: インターフェースファイルのみを参照して他モジュールの型を取得
  - CL-CC の ASDF ビルドシステムとの統合: `.fasl` に型情報を埋め込む
- **難易度**: Hard

### FR-2406: 型制約のソルバ統合 (SMT Solver Integration)

- **対象**: 新ファイル `packages/type/src/smt.lisp`
- **内容**: 複雑な型制約を外部 SMT ソルバで解く
  - **Z3 / CVC5 統合**: 精緻型 (FR-301) の述語を SMT 論理式に変換してソルバへ送信
  - **反例生成**: 型エラー時にソルバが反例（型違反を引き起こす具体的な値）を生成
  - **量化制約**: `(forall x. P(x))` を SMT で自動証明
  - **線形算術**: `{v: Int | 0 <= v && v < n}` の `v + 1 < n` の証明を Presburger arithmetic で解く
  - Liquid Haskell (GHC プラグイン)、F\* (Microsoft Research)、Dafny (Microsoft)
- **難易度**: Very Hard

---

## 25. 動的言語の型統合

### FR-2501: Dynamic 型 (Dynamic / Any Type)

- **対象**: `packages/vm/src/primitives.lisp`
- **内容**: 型が不明な値を型安全に扱う「型付き動的値」
  - **`Dynamic` 型** (Haskell): `data Dynamic = forall a. Typeable a => Dynamic a`
  - `(wrap-dynamic 42)` → `(dynamic integer 42)`。`(unwrap-dynamic d : integer)` — 型指定アンラップ
  - **失敗時の処理**: 型が合わない場合は `Maybe` / `Either` で型安全に失敗
  - CL の `(typep x 'integer)` の型推論版
  - **Any 型** (TypeScript/Go): すべての型のスーパータイプ。動的型との違いは implicit coercion の有無
  - **`unknown` vs `any`** (TypeScript): `unknown` は使用前に型ナローイング必須
- **難易度**: Medium

### FR-2502: ランタイム型表現 (Runtime Type Representations / TypeRep)

- **対象**: `packages/vm/src/primitives.lisp`
- **内容**: コンパイル時の型情報を実行時オブジェクトとして操作
  - **`TypeRep a`** (Haskell `Data.Typeable`): `typeOf :: a -> TypeRep a` — 型の実行時表現を取得
  - `(type-rep integer)` — `integer` 型の実行時表現。`(type-rep-eq (type-rep integer) (type-rep string))` → false
  - **型安全キャスト**: `(cast x (type-rep integer))` — `TypeRep` を使って型安全にキャスト
  - **型写像**: 型から関数へのマッピング（型ごとの異なる実装の型安全なディスパッチ）
  - CL の `class-of` / `find-class` の型安全版
- **難易度**: Medium

### FR-2503: テンプレートリテラル型 (Template Literal Types)

- **対象**: `packages/type/src/parser.lisp`
- **内容**: 文字列テンプレートを型レベルで操作（TypeScript 4.1+）
  - `(template-literal "on" (capitalize :event))` — `"onClick"` や `"onChange"` の型を生成
  - **文字列操作型関数**: `(uppercase :s)`, `(lowercase :s)`, `(capitalize :s)` — 型レベル文字列変換
  - TypeScript: `` type EventHandlers<T extends string> = `on${Capitalize<T>}` ``
  - CL-CC 応用: シンボル命名規則を型で保証 (`with-slots` のスロット名検査)
- **難易度**: Hard

### FR-2504: ✅ 型ガードと型述語 (Type Guards / Type Predicates)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: ランタイム型チェック関数の戻り値を型絞り込みに活用
  - TypeScript の型述語: `function isString(x: unknown): x is string { return typeof x === 'string' }`
  - 関数の戻り値型が `x is T` の場合、`if (isString(x))` ブランチ内で `x: string` が確定
  - CL-CC: `(defun integerp (x) : (x is integer) ...)` — 型述語関数の型シグネチャ
  - 出現型 (FR-302) の関数抽象版: `typep`/`integerp`/`consp` 等の組み込み述語を型システムに登録
- **難易度**: Medium

### FR-2505: 型付き正規表現 (Typed Regular Expressions)

- **対象**: 新ファイル `packages/type/src/regex.lisp`
- **内容**: 正規表現のキャプチャグループを型として表現
  - TypeScript の template literal types で正規表現パターンを型に
  - `(regex-type "(\d+)-(\w+)")` → `(tuple string string)` — 2つのキャプチャグループの型
  - **名前付きキャプチャ**: `(regex-type "(?P<year>\d{4})-(?P<month>\d{2})")` → `(record (:year string) (:month string))`
  - **型安全マッチング**: マッチ失敗を `Maybe` で型付け。グループ数の型安全性
  - Racket の `pregexp` + 型推論
- **難易度**: Hard

---

## 26. 高度な型クラス機構

### FR-2601: 量化制約 (Quantified Constraints)

- **対象**: `packages/type/src/typeclasses.lisp`
- **内容**: 型クラス制約に全称量化子を付与
  - GHC `-XQuantifiedConstraints` (GHC 8.6+)
  - `(forall :a. (show :a) => (show (list :a)))` — 「任意の `Show a` なら `Show [a]`」を制約として表現
  - **用途**: `(type-class traversable (:t) (:super (forall :a. functor (:t :a))))` — スーパークラスに量化制約
  - Bottu et al. (2018). Quantified Class Constraints.
  - CL-CC 応用: CLOS メソッドの特殊化条件を型クラス制約として表現
- **難易度**: Very Hard

### FR-2602: Constraint カインド (Constraint Kinds)

- **対象**: `packages/type/src/typeclasses.lisp`
- **内容**: 型クラス制約を型として第一級に扱う
  - GHC `-XConstraintKinds`: `(Constraint)` カインドで制約自体を型変数に代入可能
  - `(type-alias constraint-of (:a) (tuple (eq :a) (show :a)))` — 複合制約の別名定義
  - `(type :c (constraint) => (function ((satisfying :c :a)) :a))` — 制約を型変数として受け取る
  - **用途**: 制約の抽象化、制約の合成、型クラス制約のリストを型として扱う
- **難易度**: Hard

### FR-2603: 型クラス証人 (Type Class Witnesses / Dict)

- **対象**: `packages/type/src/typeclasses.lisp`
- **内容**: 型クラスインスタンスを実行時の「辞書オブジェクト」として第一級に扱う
  - `(dict (eq integer))` — `Eq Integer` インスタンスの辞書を実行時値として取得
  - **反射的型クラス**: `(reify-constraint :c (lambda (dict) ...))` — 制約を値として渡す
  - `(with-dict some-dict (use-eq))` — 辞書を注入して型クラスメソッドを使用
  - Haskell の `reflection` パッケージ。型クラスのランタイム選択が可能になる
  - CL-CC 応用: CLOS の `find-method` の型安全版
- **難易度**: Hard

### FR-2604: DerivingVia (戦略的 deriving)

- **対象**: `packages/expand/src/expander.lisp`
- **内容**: 既存の型のインスタンスを流用して新しい型のインスタンスを導出
  - GHC `-XDerivingVia` (GHC 8.6+)
  - `(defnewtype my-int integer (:deriving eq via integer))` — `integer` の `Eq` インスタンスをそのまま流用
  - `(defnewtype sum (:a) (:a) (:deriving monoid via (additive :a)))` — `Additive` を経由して `Monoid` を導出
  - Blöndal et al. (2018). Deriving Via: or, How to Turn Hand-Written Instances into an Anti-Pattern.
  - **実装原理**: `newtype` の `Coercible` (FR-1202) を使って辞書を変換
- **難易度**: Medium

### FR-2605: 暗黙関数型 (Implicit Function Types)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 暗黙のコンテキストを型で表現するScala 3スタイルの仕組み
  - Scala 3: `def foo(x: Int)(implicit ctx: Context): Int` → `def foo(x: Int): Context ?=> Int`
  - `(function :a (:b ?=> :c))` — `B` の暗黙インスタンスがあれば `A -> C`
  - **型クラスとの関係**: 型クラスの辞書渡しを明示化したもの。型クラスより表現力が高い（局所インスタンス）
  - **スコープ制御**: `(given (instance ...) body)` — `body` の中だけ暗黙値が有効
  - Scala 3 の `given`/`using`、Agda の instance arguments
- **難易度**: Hard

### FR-2606: マッチ型 (Match Types)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 型引数にパターンマッチして型を選択する型レベル `case` 式
  - Scala 3 の match types: `type Head[X] = X match { case h *: t => h }`
  - `(type-match :x ((cons :h :t) => :h) (null => void))` — リストの先頭要素の型を抽出
  - 型族 (FR-107) と同等の表現力だが構文が直感的
  - TypeScript の conditional types: `T extends U ? X : Y`
  - **型の deconstruction**: 型コンストラクタを型レベルで分解
- **難易度**: Hard

---

## 27. 特殊ドメイン型

### FR-2701: 量子計算型 (Quantum Types)

- **対象**: 新ファイル `packages/type/src/quantum.lisp`
- **内容**: 量子ビット・量子操作を型安全に扱う
  - **Qubit 型**: `(qubit)` — 量子ビット（|0⟩ と |1⟩ の重ね合わせ）
  - **線形型との連携**: 量子力学の no-cloning 定理 → qubit は線形型 (FR-501)。コピー不可
  - **エンタングルメント型**: `(entangled-pair qubit qubit)` — 2 つの qubit のエンタングル状態
  - **量子回路型**: `(circuit n m)` — n 入力 m 出力の量子回路
  - **測定の型**: `(measure (qubit) : (pair boolean qubit))` — 測定後の古典ビットと量子ビット
  - Quipper (Haskell ベースの量子プログラミング言語)、QSharp (Microsoft)、Silq
- **難易度**: Extremely Hard

### FR-2702: 確率的型 (Probabilistic Types)

- **対象**: 新ファイル `packages/type/src/probabilistic.lisp`
- **内容**: 確率分布を型として扱う
  - `(distribution :a)` — `a` 型の値の確率分布
  - `(bernoulli float)` — 成功確率 `p` のベルヌーイ分布。型は `(distribution boolean)`
  - **確率的プログラミング**: `(sample (normal 0.0 1.0))` → `float`。型は `(stochastic float)`
  - **型推論との統合**: 確率的プログラムの型 = 確率分布型
  - Church、WebPPL、Gen.jl (Julia) などの確率的プログラミング言語のアプローチ
  - **測度論的意味論**: 確率分布をモナドとして表現 (`Distribution` モナド)
- **難易度**: Extremely Hard

### FR-2703: 微分可能型 (Differentiable Types / AD Types)

- **対象**: 新ファイル `packages/type/src/ad.lisp`
- **内容**: 自動微分 (Automatic Differentiation) を型システムに組み込む
  - `(differentiable float)` — 微分可能な float 値
  - **前向き AD**: `(dual :a)` = `(:a, :a)` — 値と微分値のペア型
  - **後向き AD (Backprop)**: 計算グラフを型で追跡して逆伝播を型安全に実装
  - **高階微分**: `(diff^n :a)` — n 階微分可能な型
  - Dex (Google Research)、Zygote.jl (Julia)、Enzyme (LLVM)
  - **型安全ニューラルネット**: 層の入出力次元を型で管理 (FR-2304 テンソル型との統合)
- **難易度**: Very Hard

### FR-2704: ハードウェア設計型 (Hardware / Circuit Types)

- **対象**: 新ファイル `packages/type/src/hardware.lisp`
- **内容**: デジタル回路・FPGA 設計の型安全な記述
  - **Signal 型**: `(signal :a)` — クロックサイクルで進む `a` 型の値の流れ
  - **ビット幅型**: `(bits 8)` — 8 ビット整数。算術オーバーフローをビット幅で型管理
  - **クロックドメイン型**: `(in-domain clk1 :a)` vs `(in-domain clk2 :a)` — クロック境界をまたぐデータ転送を型で制御
  - Clash (Haskell ベースの FPGA 言語)、Lava、Chisel (Scala)
  - **型レベルクロック**: クロック間の同期回路と非同期回路を型で区別
- **難易度**: Extremely Hard

### FR-2705: 型安全設定言語 (Type-Safe Configuration)

- **対象**: 新ファイル `packages/type/src/config.lisp`
- **内容**: 設定ファイルの型を静的に検証
  - **Dhall**: 設定言語に型システムを組み込む。Turing 非完全（停止性保証）
  - **Nickel**: Dhall の後継。段階的型付け + 契約
  - **CUE**: 型 = バリデーション制約。データとスキーマが同一構文
  - `(config-type (:port (interval integer 1024 65535)) (:host string))` — 設定型のスキーマ定義
  - CL-CC 応用: `defconfig` マクロで設定構造と型を一体定義
- **難易度**: Medium

### FR-2706: 型安全ステートマシン (Typed State Machines)

- **対象**: `packages/type/src/typeclasses.lisp`
- **内容**: 状態遷移を型で完全に記述する
  - **Mealy 機械**: `(mealy-machine states alphabet output)` — 入力・状態・出力をすべて型パラメータで表現
  - **型安全遷移関数**: `(transition state-a :event-x -> state-b)` — 無効な遷移はコンパイルエラー
  - Typestate (FR-504) の汎化: Typestate は直線的なライフサイクル。ステートマシンは任意の状態グラフ
  - **状態機械の合成**: 2 つの型付きステートマシンを積 (`×`) で合成
  - Haskell の `machines` パッケージ、Rust の `state-machine-future`
- **難易度**: Hard

### 型推論・基礎理論

- Damas & Milner (1982). Principal type-schemes for functional programs.
- Dunfield & Krishnaswami (2013). Complete and Easy Bidirectional Typechecking.
- Sulzmann, Chakravarty, Jones, Donnelly (2007). System F with Type Equality Coercions.
- Schrijvers et al. (2009). OutsideIn(X): Modular type inference with local assumptions.
- Garrigue (2004). Relaxing the value restriction.

### 多相性・型クラス

- Girard (1972). Interprétation fonctionnelle et élimination des coupures.
- Reynolds (1974). Towards a theory of type structure.
- Wadler & Blott (1989). How to make ad-hoc polymorphism less ad hoc.
- Jones (2000). Type Classes with Functional Dependencies.
- Peyton Jones et al. (2007). Practical type inference for arbitrary-rank types.

### 精緻型・依存型

- Rondon, Kawaguchi, Jhala (2008). Liquid Types.
- Tobin-Hochstadt & Felleisen (2008). The Design and Implementation of Typed Scheme.
- The Idris Community (2021). Idris 2: Quantitative Type Theory in Practice.
- de Moura et al. (2021). The Lean 4 Theorem Prover and Programming Language.

### エフェクト型

- Plotkin & Pretnar (2009). Handlers of Algebraic Effects.
- Leijen (2014). Koka: Programming with Row Polymorphic Effect Types.
- Petricek, Orchard, Mycroft (2014). Coeffects: A calculus of context-dependent computation.

### リソース管理型

- Wadler (1990). Linear types can change the world!
- Bernardy et al. (2018). Linear Haskell: Practical Linearity in a Higher-Order Polymorphic Language.
- Tofte & Talpin (1994). Implementation of the Typed Call-by-Value λ-calculus using a Stack of Regions.
- Aldrich et al. (2009). Typestate-Oriented Programming.
- Siek & Taha (2006). Gradual Typing for Functional Languages.
- Lindley & Morris (2016). Talking Bananas: Structural Recursion for Session Types.

### 集合論・変性・構造型

- Benzaken, Castagna, Frisch (2003). CDuce: An XML-centric General-purpose Language.
- Frisch, Castagna, Benzaken (2008). Semantic Subtyping.

### 安全性・セキュリティ

- Volpano, Irvine, Smith (1996). A Sound Type System for Secure Flow Analysis.
- Miller (2006). Robust composition: Towards a unified approach to access control.

### 停止性・全域性

- Abel (2008). MiniAgda: Integrating Sized and Dependent Types.
- Nakano (2000). A Modality for Recursion.
- Clouston et al. (2015). Programming and Reasoning with Guarded Recursion for Coinductive Types.
- Maranget (2007). Warnings for pattern matching.

### 型レベルプログラミング・メタプログラミング

- Taha & Sheard (1997). Multi-Stage Programming with Explicit Annotations.
- Jones, Gomard, Sestoft (1993). Partial Evaluation and Automatic Program Generation.
- Kiselyov, Lämmel, Schupke (2004). Strongly typed heterogeneous collections.

### 型と証明・HoTT

- Necula (1997). Proof-Carrying Code.
- The Univalent Foundations Program (2013). Homotopy Type Theory.
- Abel et al. (2017). Decidability of Conversion for Type Theory in Type Theory.
- Kovács (2020). Elaboration Zoo.
- Zeilberger (2009). Focusing and Higher-Order Abstract Syntax.

### Optics・関数型パターン

- Kiselyov & Ishii (2015). Freer Monads, More Extensible Effects.
- Riley (2018). Categories of Optics.
- Carette, Kiselyov, Shan (2009). Finally Tagless, Partially Evaluated.

### 型融合・最適化

- Gill, Launchbury, Peyton Jones (1993). A Short Cut to Deforestation.
- Claessen & Hughes (2000). QuickCheck: A Lightweight Tool for Random Testing.

### 開発ツール・実装

- Wadler (1989). Theorems for free!
- Reynolds (1983). Types, Abstraction and Parametric Polymorphism.
- The Rust Reference (2024). The Rust Programming Language, 2024 Edition.
- GHC Team (2024). GHC User's Guide 9.10.
- Danvy & Filinski (1990). Abstracting Control.

### 並行・非同期型

- Wadler (2012). Propositions as Sessions.
- Marlow (2013). Parallel and Concurrent Programming in Haskell.
- Syme et al. (2011). The F# Asynchronous Programming Model.

### 数値・計量型

- Kennedy (2009). Types for Units-of-Measure: Theory and Practice.
- Rump (2010). Verification of Floating-Point Computations.

### 型システム実装技術

- Robinson (1965). A Machine-Oriented Logic Based on the Resolution Principle.
- Martelli & Montanari (1982). An Efficient Unification Algorithm.
- Bottu et al. (2018). Quantified Class Constraints.
- Blöndal, Löh, Scott (2018). Deriving Via: or, How to Turn Hand-Written Instances into an Anti-Pattern.

### 特殊ドメイン型

- Green et al. (2013). Quipper: A Scalable Quantum Programming Language.
- Elliott (2009). Beautiful Differentiation.
- Baaij (2015). Digital Circuits in CλaSH.

---

## 28. 型と意味論の対応

### FR-2801: 公理的意味論と型 (Axiomatic Semantics / Hoare Logic as Types)

- **対象**: `packages/type/src/props.lisp` (FR-2001 Curry-Howard の拡張)
- **内容**: Hoare Triple `{P} C {Q}` を型として表現し、プログラムの正当性を型検査で検証
  - `{pre: (> n 0)} (factorial n) {post: (> result 0)}` — 事前条件と事後条件を型に組み込む
  - **Hoare 型理論 (HTT)**: Nanevski et al. (2008). `(htt-type (pre :p) computation (post :q))` で命令型プログラムを型付け
  - **Separation Logic との統合**: ヒープ操作に分離論理 (`*` = ヒープ分離結合) を型で表現
  - **並行分離論理 (CSL)**: O'Hearn (2007). ロックや共有メモリを含む並行プログラムの型安全な推論
  - DBC (FR-602) との違い: DBC は実行時チェック指向。Hoare Logic は静的証明指向
  - F\* (Microsoft Research): Hoare 型理論の実用実装。`ST` エフェクトに事前/事後条件を付与
- **難易度**: Extremely Hard

### FR-2802: 圏論的型意味論 (Categorical Semantics)

- **対象**: 理論的基盤
- **内容**: 型と型変換を圏論の対象と射として解釈
  - **デカルト閉圏 (CCC)**: 単純型付きラムダ計算の圏論的意味論。積型=積、関数型=指数対象
  - **双デカルト閉圏**: 和型（余積）も持つ圏。代数的データ型の意味論
  - **モナドと随伴**: `T` モナド = `F ⊣ U` 随伴から誘導。エフェクト型 (FR-401) の圏論的基礎
  - **Grothendieck ファイブレーション**: 依存型の圏論的モデル
  - **応用**: 型変換の「等式定理」を圏論の可換図式から自動導出
  - Mac Lane (1971). Categories for the Working Mathematician.
- **難易度**: Very Hard (理論把握) / Extremely Hard (実装)

### FR-2803: ゲーム意味論 (Game Semantics)

- **対象**: 理論的基盤
- **内容**: プログラムの意味をゲーム（プレイヤーの手番の列）として定義
  - **アリーナ**: 型に対応する「ゲーム盤」。`A → B` = `A` と `B` のゲームの合成
  - **完全抽象性**: ゲーム意味論は PCF (多相ラムダ計算) の完全抽象モデルを与える
  - **型検査への応用**: ゲーム意味論ベースの型同値判定。型に含意が「本当に成立するか」をゲームで検証
  - Abramsky, Jagadeesan, Malacaria (2000). Full Abstraction for PCF.
  - **インタラクション型**: セッション型 (FR-505) はゲーム意味論の観点から自然に解釈できる
- **難易度**: Extremely Hard

### FR-2804: 抽象解釈と型推論 (Abstract Interpretation for Type Inference)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: Cousot & Cousot (1977) の抽象解釈フレームワークを型推論に適用
  - **抽象ドメイン**: 具体的な値の集合を「型」という抽象値で近似。`integer` = 「すべての整数の集合」
  - **ワイドニング (Widening)**: 解析が収束しない場合に強制的に抽象値を広げる演算 `∇`
  - **ナローイング (Narrowing)**: ワイドニングで失った精度を取り戻す演算 `△`
  - **ギャランティング**: 解析結果が「健全 (sound)」= 実際の実行を必ず近似
  - 型推論の不動点計算をワイドニングで高速化
  - Cousot & Cousot (1977). Abstract Interpretation: A Unified Lattice Model.
- **難易度**: Very Hard

### FR-2805: 行動的サブタイピング (Behavioral Subtyping / Liskov Substitution)

- **対象**: `packages/type/src/subtyping.lisp`
- **内容**: Liskov 置換原則 (LSP) を型システムに形式化
  - **LSP**: `S <: T` ならば、`T` 型の値が期待される場所に `S` 型の値を置いても振る舞いが変わらない
  - **反変的事前条件**: サブタイプのメソッドの事前条件はスーパータイプ以下
  - **共変的事後条件**: サブタイプのメソッドの事後条件はスーパータイプ以上
  - **歴史制約**: サブタイプはスーパータイプのオブジェクトの振る舞い「歴史」を保存しなければならない
  - Liskov & Wing (1994). A Behavioral Notion of Subtyping.
  - CLOS との統合: `defmethod :before/:after` によるスーパークラスとのコントラクト継承
- **難易度**: Hard

---

## 29. 最適化のための型解析

### FR-2901: 厳格性解析 (Strictness Analysis / Demand Analysis)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **内容**: 関数が引数を「必ず評価するか」をコンパイル時に解析
  - **厳格性**: `f` が引数 `x` に厳格 = `f ⊥ = ⊥`（`x` が未定義なら `f x` も未定義）
  - **活用**: 厳格な引数は遅延評価のサンクを作らずに即時評価可能 → サンク割り当て削減
  - **Demand Signatures**: `<S, 1*U>` — 最初の引数を厳格に使い、2番目を1回だけ使う
  - GHC の Demand Analyser: `StrDmd`, `UseDmd`, `DmdType` — 関数ごとの demand シグネチャ
  - CL-CC: `vm-call` での引数評価順序最適化、クロージャ内変数の遅延評価省略
- **難易度**: Hard

### FR-2902: 型ベースエイリアス解析 (Type-Based Alias Analysis, TBAA)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **内容**: 型情報から「2つのポインタが同じメモリ位置を指す可能性があるか」を判定
  - **TBAA**: 異なる型のポインタはエイリアスしない（C の strict aliasing rule に対応）
  - `(integer* a)` と `(float* b)` は絶対にエイリアスしない → 一方への書き込みが他方のロードに影響しない → 命令の並び替え可能
  - LLVM の `!tbaa` メタデータ: IR 命令に型エイリアス情報を付与
  - CL-CC: `vm-slot-ref` (CLOS スロット) と `vm-car` (コンス) はエイリアスしない → ロード/ストアの並び替え
- **難易度**: Hard

### FR-2903: ボックス化解析 (Boxity Analysis)

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: 値をヒープに「箱詰め (boxing)」するか、レジスタに「非箱詰め (unboxed)」のまま使うか解析
  - **Boxed**: ヒープにオブジェクトを割り当て、ポインタを通じてアクセス。GC 管理下
  - **Unboxed**: レジスタや機械語ワードに直接格納。GC 管理外
  - GHC の Boxity Analysis: 引数・戻り値が Boxed/Unboxed どちらで使われるか解析して最適化
  - **Worker/Wrapper 変換**: `f :: Int -> Int` を `f_worker :: Int# -> Int#` (unboxed) に分離
  - CL-CC: NaN-boxing タグの付け外しコストをボックス化解析で最小化
- **難易度**: Hard

### FR-2904: アリティ解析 (Arity Analysis)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **内容**: 関数が何引数で完全適用 (saturated) されるかをコンパイル時に解析
  - **飽和適用 (Saturated)**: `(f a b)` where `f` takes exactly 2 args → 仮想ディスパッチなし
  - **未飽和適用 (Unsaturated)**: `(f a)` where `f` takes 2 args → クロージャを作成
  - **過飽和適用 (Oversaturated)**: `(f a b c)` where `f` takes 2 args → 結果を `c` に適用
  - GHC の Arity Analysis: `ArityType` を各関数に付与してコールパターンを最適化
  - CL-CC: `vm-call` の呼び出し規約選択（引数カウントが一致する場合の fast path）
- **難易度**: Medium

### FR-2905: 使用数解析 (Usage / Occurrence Analysis)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **内容**: 変数が何回使われるかをコンパイル時に追跡
  - **単一使用 (Single-Use)**: 変数が 1 回しか使われない → インライン展開しても複製コストなし
  - **デッドコード**: 変数が 0 回使われる → バインドを除去
  - **ループ内使用**: `n` 回使用されると推定 → コピーオンライトの判断に使用
  - GHC の `OccInfo`: `Dead`, `OneOcc`, `ManyOccs` の区別
  - CL-CC: レジスタ割り当ての前処理として変数使用数を解析
- **難易度**: Medium

---

## 30. 型システムの拡張性と相互運用

### FR-3001: カスタム型エラーメッセージ (Custom Type Errors)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: ライブラリ作者がユーザー向けの型エラーメッセージをカスタマイズ
  - GHC の `TypeError` 型クラス: `(type-error (text "Cannot add ") (show :a) (text " to ") (show :b))`
  - 型クラスインスタンスが見つからない場合に、デフォルトの難解なエラーではなく人間が読みやすいメッセージを表示
  - **エラーレベル**: `TypeError` (コンパイルエラー)、`Warn` (警告)、`Info` (情報)
  - CL-CC 応用: `defgeneric` に `:type-error` オプションを追加
- **難易度**: Medium

### FR-3002: 型検査プラグイン (Type Checking Plugins)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 型推論エンジンにサードパーティの制約ソルバを差し込む拡張機構
  - GHC の `TcPlugin` インターフェース: `tcPluginSolve`, `tcPluginRewrite` をフック
  - **応用**: SMT ソルバプラグイン（精緻型のため）、ユニット型プラグイン（計量単位のため）
  - `ghc-typelits-natnormalise`: 型レベル自然数算術のプラグイン
  - `ghc-typelits-knownnat`: `KnownNat` 制約の自動導出プラグイン
  - CL-CC: `*type-checker-plugins*` リストにフック関数を登録する仕組み
- **難易度**: Hard

### FR-3003: 型指向プログラム合成 (Type-Directed Program Synthesis)

- **対象**: 新ファイル `packages/type/src/synthesis.lisp`
- **内容**: 型シグネチャからプログラムを自動生成
  - **Hoogle**: `(a -> b) -> [a] -> [b]` という型シグネチャから `map` を検索
  - **Synquid**: 精緻型 (FR-301) からプログラムを自動合成。`{v: [Int] | sorted v}` を返す sort の実装を生成
  - **型駆動補完**: REPL で型を入力すると、その型を持つ関数の候補一覧を提示
  - **プルーフ補助としての合成**: タクティック (FR-2003) と組み合わせて証明項を自動補完
  - Polikarpova et al. (2016). Program Synthesis from Polymorphic Refinement Types.
- **難易度**: Very Hard

### FR-3004: 言語間型マッピング (Cross-Language Type Mapping)

- **対象**: `src/ffi/type-safe-ffi.lisp` (FR-2103 の拡張)
- **内容**: 異なる言語の型システム間でのマッピングを体系化
  - **C ↔ CL-CC**: `c-int` ↔ `fixnum`, `c-ptr(:a)` ↔ `(foreign-pointer :a)`
  - **JSON ↔ CL-CC**: JSON の `null`/`boolean`/`number`/`string`/`array`/`object` → CL-CC の型
  - **Protobuf / MessagePack**: スキーマ定義から CL-CC 型を自動生成
  - **型の安全なシリアライゼーション**: 型情報をワイヤフォーマットに埋め込んで受信側で型チェック
  - **相互運用の健全性**: 送信側と受信側の型互換性をコンパイル時に検証できる
- **難易度**: Hard

### FR-3005: 型ベースドキュメント生成 (Type-Based Documentation)

- **対象**: `packages/cli/src/main.lisp`
- **内容**: 型シグネチャから自動的にドキュメントを生成
  - **Haddock スタイル**: `(defun map (f xs) (:doc "Apply f to each element of xs") ...)` → HTML ドキュメント
  - **型シグネチャの自動表示**: 引数名・型・戻り値型を整形して表示
  - **使用例の型チェック**: ドキュメント内のコード例が型チェックを通ることを保証
  - **型ベースの検索**: `(search-by-type '(function (list) integer))` — 型シグネチャで関数を検索
  - Rust の `rustdoc`: `///` コメント + 型情報を組み合わせたドキュメント生成
- **難易度**: Medium

---

## 31. 線形論理と型

### FR-3101: 線形論理の型 (Linear Logic Types)

- **対象**: `packages/type/src/linear-logic.lisp`
- **内容**: Girard (1987) の線形論理を型システムとして実装
  - **線形論理の結合子**:
    - `⊗` (テンソル積): 両方のリソースを使う。`A ⊗ B` = `A` と `B` を**同時に**持つ
    - `&` (加法積): どちらかを選んで使う。`A & B` = `A` か `B` を**選択**できる
    - `⊕` (加法和): どちらかが与えられる。`A ⊕ B` = `A` か `B` のどちらか**一方**が来る
    - `⅋` (パー): 双対結合子。古典的線形論理に登場
  - **線形型 (FR-501) との関係**: 線形型は線形論理の型付けシステムの実用版
  - **プロセス計算との対応**: π-計算は線形論理の観点から解釈可能
- **難易度**: Very Hard

### FR-3102: バン型 (Exponential / Bang Types `!A`)

- **対象**: `packages/type/src/linear-logic.lisp`
- **内容**: 線形論理の「無制限使用を許可するモダリティ」
  - `!A` (バン A): `A` 型の値を**任意回**コピー・破棄できる
  - **線形型と通常型の橋渡し**: `(!A → B)` — `A` を無制限に使える線形関数
  - **弱化規則**: `!A ⊢ unit` — `!A` は使わなくても良い（破棄可能）
  - **収縮規則**: `!A ⊢ !A ⊗ !A` — `!A` はコピー可能
  - Linear Haskell での対応: `a` (非制限) = `!a` in linear logic
  - CPS との関係: CPS 変換後の継続型は線形型 (1回だけ呼ばれる)。`!` で非線形な継続を表現
- **難易度**: Hard

### FR-3103: Bounded Linear Logic (BLL) と計算複雑性型

- **対象**: 新ファイル `packages/type/src/complexity-types.lisp`
- **内容**: 資源の使用回数に上限を付与して計算複雑性を型で制御
  - **BLL (Girard, Scedrov, Scott 1992)**: `!n A` — `A` を**最大 n 回**使用できる
  - **多項式時間型**: BLL の型に入るプログラムは多項式時間で動作することが保証
  - **型から複雑性クラスの導出**: `PTIME`, `PSPACE` などの複雑性クラスを型で特徴づける
  - **Soft Linear Logic**: 多項式時間関数型プログラミングのための線形論理
  - Girard, Scedrov, Scott (1992). Bounded Linear Logic.
  - **実用応用**: 組み込みシステムや実時間システムでの計算時間保証
- **難易度**: Extremely Hard

### FR-3104: 交換子・弱化・収縮の型制御 (Structural Rules as Types)

- **対象**: `packages/type/src/linear-logic.lisp`
- **内容**: 論理の構造規則を型システムで制御可能にする
  - **交換 (Exchange)**: `Γ, A, B, Δ ⊢ C` から `Γ, B, A, Δ ⊢ C`。通常の型システムは暗黙に許可
  - **弱化 (Weakening)**: 未使用の仮定を捨てる。線形型では禁止
  - **収縮 (Contraction)**: 仮定を複製する。線形型では禁止
  - **Ordered Type Theory**: 交換も禁止した型システム。文脈の順序が意味を持つ
  - **Sub-structural Type Systems**: 線形型・アフィン型・有関係型 (relevant types) の統一的分類
    - **線形型**: 弱化・収縮を禁止（1回だけ使用）
    - **アフィン型**: 収縮を禁止（高々1回使用）
    - **有関係型 (Relevant)**: 弱化を禁止（少なくとも1回使用）
    - **順序型 (Ordered)**: 交換も禁止（順序通りに使用）
  - Walker (2005). Substructural Type Systems.
- **難易度**: Very Hard

### 意味論・論理・圏論

- Cousot & Cousot (1977). Abstract Interpretation: A Unified Lattice Model.
- Abramsky, Jagadeesan, Malacaria (2000). Full Abstraction for PCF.
- Mac Lane (1971). Categories for the Working Mathematician.
- Liskov & Wing (1994). A Behavioral Notion of Subtyping.
- Nanevski, Morrisett, Birkedal (2008). Hoare Type Theory, Polymorphism and Separation.
- O'Hearn (2007). Resources, Concurrency and Local Reasoning.

### 線形論理

- Girard (1987). Linear Logic.
- Girard, Scedrov, Scott (1992). Bounded Linear Logic.
- Walker (2005). Substructural Type Systems.

### 最適化解析

- Peyton Jones & Lester (1992). Implementing Functional Languages (demand analysis).
- Diwan, McKinley, Moss (1998). Type-Based Alias Analysis.
- Polikarpova et al. (2016). Program Synthesis from Polymorphic Refinement Types.

---

## 32. 代数的サブタイピングとパス依存型

### FR-3201: 代数的サブタイピング (Algebraic Subtyping / MLsub)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: Dolan (2017) の画期的な型システム。HM 型推論とサブタイピングを初めて効率的に統合
  - **問題**: 従来の HM + サブタイプは型推論が NP 困難または非決定的になる
  - **解決**: 型を束 (lattice) として表現。型変数に上限・下限の両方を付与
  - **極性型変数 (Polar Type Variables)**: 正の位置 (共変) の型変数と負の位置 (反変) の型変数を区別
  - `(function (:a ∧ integer) (:b ∨ string))` — 引数は `integer` のサブタイプ、戻り値は `string` のスーパータイプ
  - **主型の存在**: MLsub も主型を持つ。HM と同様の型推論アルゴリズムが使える
  - **影響**: TypeScript、Flow、Kotlin の型システム設計に多大な影響
  - Dolan & Mycroft (2017). Polymorphism, Subtyping, and Type Inference in MLsub.
- **難易度**: Very Hard

### FR-3202: 極性サブタイピング (Polar Types / Positive-Negative Subtyping)

- **対象**: `packages/type/src/subtyping.lisp`
- **内容**: 型の「出現位置の極性」でサブタイピングの方向を自動的に決定
  - **正の位置 (Positive)**: 戻り値・共変位置。`A ∨ B` = 和型 (union)
  - **負の位置 (Negative)**: 引数・反変位置。`A ∧ B` = 交差型 (intersection)
  - `(function A B)` — `A` は負の位置（反変）、`B` は正の位置（共変）
  - **型の正規形**: 正の位置の型を交差で表現するのは意味がない（最大値を取るため）。極性で不必要な型を自動排除
  - **MLsub との関係**: 代数的サブタイピング (FR-3201) の理論的基盤
  - Zeilberger (2013). Refinement Types as Higher-Order Subtyping.
- **難易度**: Hard

### FR-3203: パス依存型 (Path-Dependent Types)

- **対象**: 新ファイル `packages/type/src/path-dependent.lisp`
- **内容**: 項（オブジェクト）のメンバーとして宣言された型。項への参照（パス）に依存する型
  - Scala の `a.T`: オブジェクト `a` が持つ型メンバー `T`。`a` が変われば型も変わる
  - `(type-member module-instance inner-type)` — モジュールインスタンスに依存した型
  - **DOT 計算** (Dependent Object Types): Amin et al. (2016). Scala の型システムの理論的基盤
  - **モジュール型との関係**: OCaml のファンクタ (FR-603) は関数として表現されたモジュール。パス依存型はその一般化
  - **抽象型の具体化**: `(new my-module)` して `my-module.element-type` でモジュール固有の型を得る
  - **CL-CC 応用**: `defpackage` + `deftype` のスコープ付き型アクセス
- **難易度**: Very Hard

### FR-3204: 自己型 / This 型 (Self Type / This Type)

- **対象**: `packages/vm/src/vm-clos.lisp`
- **内容**: オブジェクトのクラス自体を型変数として使う再帰的型
  - **問題**: `(defmethod clone (self) ...)` の戻り値型は `self` のクラスと同じであるべきだが、スーパークラスに定義すると型が固定してしまう
  - `(this-type)` — 現在のクラス（またはサブクラス）を表す型変数
  - **共変戻り値型**: `(override clone : (this-type))` — サブクラスでオーバーライドすると戻り値型がサブクラスに
  - **流暢インターフェース (Fluent Interface)**: `builder.setName(x).setAge(y)` でメソッドチェーンの型がサブクラスを保持
  - TypeScript の `this` 型、Scala の `this.type`、Kotlin の `T: This` パターン
  - CLOS の `call-next-method` に対応する型理論的説明
- **難易度**: Hard

### FR-3205: ブランド型 / 公称的型付け (Branded / Nominal Types in Structural Systems)

- **対象**: `packages/type/src/parser.lisp`
- **内容**: 構造的型付けシステムで公称的（名前ベース）の型区別を実現するパターン
  - **問題**: TypeScript は構造的型付けなので `type UserId = string` と `type PostId = string` は区別されない
  - **解決**: `(defbrand user-id string)` → `{__brand: 'UserId', value: string}` として実装
  - **型安全な変換**: `(as-user-id "123")` — ブランドを付与する唯一の方法。型チェックなしでは作れない
  - **ゼロコスト**: 実行時には `string` と同一。ブランドは型消去後に消える
  - Newtype (FR-1202) との違い: Newtype は型システムに組み込み。ブランド型は構造的型システム内のエンコーディング
- **難易度**: Medium

---

## 33. TypeScript 型システムの精髄

### FR-3301: マップ型 (Mapped Types)

- **対象**: `packages/type/src/parser.lisp`
- **内容**: 型のすべてのキーに変換を適用して新しい型を生成
  - TypeScript: `{ [K in keyof T]: T[K] }` — `T` の全プロパティを列挙して変換
  - `(mapped-type :t (lambda (:k) (optional (field-type :t :k))))` — 全フィールドを optional に
  - **応用パターン**:
    - `Partial<T>` = `{ [K in keyof T]?: T[K] }` — 全フィールドをオプショナルに
    - `Readonly<T>` = `{ readonly [K in keyof T]: T[K] }` — 全フィールドを読み取り専用に
    - `Record<K,V>` = `{ [k in K]: V }` — キー集合 `K` から値型 `V` へのマッピング
  - **フィルタリング**: `{ [K in keyof T as T[K] extends string ? K : never]: T[K] }` — 文字列フィールドのみ抽出
  - CL-CC 応用: `defstruct` のスロット変換マクロとして実装可能
- **難易度**: Hard

### FR-3302: 条件型と infer (Conditional Types with Infer)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 型レベルの条件分岐と型変数の抽出
  - TypeScript: `T extends U ? X : Y` — `T` が `U` のサブタイプなら `X`、さもなくば `Y`
  - **`infer` キーワード**: 条件型の中で型変数を束縛して抽出
    ```
    type ReturnType<T> = T extends (...args: any) => infer R ? R : never
    // T が関数型なら戻り値型 R を抽出
    ```
  - **分配条件型**: `T extends U ? X : Y` で `T` が Union 型なら各要素に分配
  - **`Awaited<T>`**: `T extends Promise<infer U> ? Awaited<U> : T` — 再帰的な条件型
  - 型族 (FR-107) + match 型 (FR-2606) との関係: より動的な型計算を実現
- **難易度**: Hard

### FR-3303: 読み取り専用型 / 不変型 (Readonly / Immutable Types)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: データの変更可能性を型で表現
  - **Readonly**: `(readonly (list integer))` — 要素を変更できないリスト
  - **Deep Readonly**: `(deep-readonly :t)` — ネストしたすべての値を再帰的に readonly に
  - TypeScript の `Readonly<T>` / `ReadonlyArray<T>`
  - **共変的 Readonly**: `(readonly :a) <: :a` — readonly は通常の型のサブタイプ（読み取りは常に安全）
  - **反変的書き込み**: `(writable :a) <: (readonly :a)` — writable の値は readonly として渡せる
  - **Freeze Semantics**: `(freeze obj)` でオブジェクトを不変化。型が `(readonly T)` に変わる
  - Rust の `&T` (共有参照 = immutable) vs `&mut T` との対応
- **難易度**: Medium

### FR-3304: ユーティリティ型ライブラリ (Utility Types Library)

- **対象**: `packages/type/src/utils.lisp`
- **内容**: 型変換のための標準的な高階型関数ライブラリ
  - `(partial-type :t)` — 全フィールドをオプショナルに（`Partial<T>`）
  - `(required-type :t)` — 全フィールドを必須に（`Required<T>`）
  - `(pick-type :t :keys)` — 指定したキーのみを持つ型（`Pick<T, K>`）
  - `(omit-type :t :keys)` — 指定したキーを除いた型（`Omit<T, K>`）
  - `(exclude-type :union :excluded)` — Union から特定の型を除外（`Exclude<T, U>`）
  - `(extract-type :union :target)` — Union から特定の型のみ抽出（`Extract<T, U>`）
  - `(non-nullable-type :t)` — `null`/`undefined` を除いた型（`NonNullable<T>`）
  - `(return-type-of :f)` — 関数型の戻り値型（`ReturnType<T>`）
  - 型族 (FR-107) + マップ型 (FR-3301) の組み合わせで実装可能
- **難易度**: Medium

### FR-3305: 型安全ルーティング (Type-Safe Routing)

- **対象**: 新ファイル `packages/type/src/routing.lisp`
- **内容**: Web API のルートを型でエンコードしてクライアント/サーバー間の型安全性を保証
  - Haskell の **Servant**: `(:get '[JSON] User :<|> "users" :> Capture "id" Int :> :get '[JSON] User)`
  - ルート型からクライアント関数と サーバーハンドラーを自動生成
  - `(api-type (get "/users" (list user)) (post "/users" user-input user))` → 型付きクライアント関数を自動生成
  - **型安全 URL パラメータ**: URL パスの `{id}` が `integer` 型であることを型で保証
  - **OpenAPI スキーマ生成**: ルート型から OpenAPI 3.0 スキーマを自動生成
  - TypeScript の `tRPC`、Zod + Express、Hono の型安全ルーティング
- **難易度**: Hard

---

## 34. 型のエンコーディングと依存型の基盤

### FR-3401: 量化型理論 QTT (Quantitative Type Theory)

- **対象**: 新ファイル `packages/type/src/qtt.lisp`
- **内容**: Atkey (2018) / McBride (2016) の量化型理論。Idris 2 の型システムの基盤
  - **量化 (Multiplicity)**: 型変数に使用回数を付与。`0`（消去）、`1`（線形）、`ω`（無制限）
  - **0 回使用 (Erased)**: 実行時に存在しない型引数。型検査にのみ使用
    - `(pi (n : [0] Nat) (Vect n Int))` — `n` は型検査のみに使い、実行時は消える
  - **1 回使用 (Linear)**: 線形型 (FR-501) と同等
  - **ω 回使用 (Unrestricted)**: 通常の型
  - **統一性**: 線形型・アフィン型・依存型・型消去を**単一のフレームワーク**で扱える
  - Idris 2: `(0 n : Nat) -> Vect n Int -> Int` — `n` は消去引数
  - McBride (2016). I Got Plenty o' Nuttin'. / Atkey (2018). Syntax and Semantics of QTT.
- **難易度**: Extremely Hard

### FR-3402: 段階付き型 (Graded Types / Graded Modal Types)

- **対象**: 新ファイル `packages/type/src/graded.lisp`
- **内容**: 使用回数を任意の半環 (semiring) で表現する QTT の一般化
  - **半環の例**:
    - `{0, 1, ω}` — QTT の量化（Idris 2）
    - `{0, 1}` — 線形型
    - `{0, 1, 2, ..., ω}` — 使用回数の自然数
    - セキュリティラベル `{Public ≤ Private}` — 情報流型 (FR-1503) として使用
    - コスト半環 — 計算複雑性の型による追跡
  - `(graded :r :a)` — 半環 `:r` の要素でリソース使用量を型付けした `:a`
  - Orchard, Liepelt, Eades III (2019). Quantitative Program Reasoning with Graded Modal Types.
- **難易度**: Extremely Hard

### FR-3403: Church / Scott / Parigot エンコーディング (Data as Functions)

- **対象**: `packages/compile/src/cps.lisp`
- **内容**: データ型をラムダ計算の関数として表現する古典的エンコーディング
  - **Church エンコーディング**:
    - `zero = λf.λx. x`, `succ n = λf.λx. f (n f x)` — 自然数の Church 数
    - `nil = λc.λn. n`, `cons h t = λc.λn. c h (t c n)` — リストの Church エンコーディング
  - **Scott エンコーディング**: パターンマッチに特化。`zero = λz.λs. z`, `succ n = λz.λs. s n`
  - **Parigot エンコーディング**: Church + Scott の利点を組み合わせ
  - **CPS 変換との関係**: Church 数は継続渡しスタイルのデータ表現。CL-CC の CPS 変換後のコードはスコット エンコーディングと同型
  - **依存型での応用**: 帰納型の eliminator はエンコーディングの一般化
- **難易度**: Medium (理解) / Hard (依存型での一般化)

### FR-3404: 拡張可能エフェクト (Extensible Effects / Freer Monads)

- **対象**: `packages/type/src/effects.lisp` (FR-401 の代替実装)
- **内容**: モナド変換子なしにエフェクトを合成する実用的アプローチ
  - **Open Union**: `(open-union effects)` — 型レベルのエフェクトリスト。型安全なバリアント型
  - `(eff (state integer :io :exception) :a)` — 3つのエフェクトを持つ計算
  - **インジェクション**: `(inject :state-effect computation)` — エフェクトスタックに新しいエフェクトを追加
  - **プロジェクション**: `(handle :state-effect handler remaining-computation)` — エフェクトを1つ処理して取り除く
  - **モナド変換子との比較**:
    - モナド変換子: スタックの順序が型に現れる。`StateT s (ExceptT e IO) a` vs `ExceptT e (StateT s IO) a` は型が違う
    - Extensible Effects: 順序非依存。`(eff (:state :except :io) a)` は一意
  - Kiselyov, Sabry, Swords (2013). Extensible Effects: An Alternative to Monad Transformers.
- **難易度**: Hard

### FR-3405: 内包的 vs 外延的型理論 (Intensional vs Extensional Type Theory)

- **対象**: `packages/type/src/dependent.lisp` (FR-303 の依存型実装に必須)
- **内容**: 型等価性の判定方式の選択。依存型実装の核心的設計決断
  - **内包的型理論 (ITT)**: 型等価性を「構文的・計算的に検証できる」等価性に制限
    - `refl : a = a` のみが等価性の証明。`n + 0 = n` は `refl` では証明できない（帰納法が必要）
    - **長所**: 型検査が決定可能。正規化 (NbE) で機械的に検査可能
    - **短所**: 数学的に自然な等価性（`n + 0 = 0 + n`）を証明するのに手間がかかる
    - Coq、Lean 4、Agda（デフォルト）が採用
  - **外延的型理論 (ETT)**: 命題的等価性と判断的等価性を同一視
    - `n + 0 = n` の証明があれば `n + 0` と `n` を型として同一視
    - **長所**: 数学に近い直感的な推論
    - **短所**: 型検査が決定不可能になる可能性
    - Nuprl が採用
  - **観察的型理論 (OTT)**: Altenkirch et al. (2007). ITT と ETT の折衷案
  - **高次観察的型理論 (HOTT)**: Altenkirch et al. (2022). OTT の改善版（Agda `--cubical` の代替）
- **難易度**: Extremely Hard

### FR-3406: 帰納的構成の計算体系 CIC (Calculus of Inductive Constructions)

- **対象**: 新ファイル `packages/type/src/cic.lisp`
- **内容**: Coq の基盤型理論。依存型 + 帰納型 + 宇宙 を統合した体系
  - **CIC = CoC + 帰納型**:
    - **CoC** (Calculus of Constructions): Coquand & Huet (1988). 依存型 + System F + ω
    - **帰納型の追加**: `Inductive list (A: Type) : Type := nil | cons : A -> list A -> list A`
  - **Prop と Type の区別**:
    - `Prop`: 論理命題の宇宙。証明は実行時に消去可能（proof irrelevance）
    - `Set`/`Type`: 計算的な型の宇宙
  - **大消去 (Large Elimination)**: 帰納型から型を返す消去子（`bool -> Type`）
  - **Coq の `Inductive` = CL-CC の `defgadt`** の型理論的正当化
  - Coquand & Huet (1988). The Calculus of Constructions.
  - Paulin-Mohring (1993). Inductive Definitions in the System Coq.
- **難易度**: Extremely Hard

### 代数的サブタイピング・依存型基盤

- Dolan & Mycroft (2017). Polymorphism, Subtyping, and Type Inference in MLsub.
- Amin, Rompf, Odersky (2016). Soundness of Dependent Object Types (DOT).
- Atkey (2018). Syntax and Semantics of Quantitative Type Theory.
- Orchard, Liepelt, Eades III (2019). Quantitative Program Reasoning with Graded Modal Types.
- McBride (2016). I Got Plenty o' Nuttin'.
- Coquand & Huet (1988). The Calculus of Constructions.
- Paulin-Mohring (1993). Inductive Definitions in the System Coq.
- Altenkirch, McBride, Swierstra (2007). Observational Equality, Now!
- Kiselyov, Sabry, Swords (2013). Extensible Effects: An Alternative to Monad Transformers.
