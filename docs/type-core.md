# Type System: Core

Core type system contract for the compiler: inference, bidirectional checking, constraint solving, type transport into codegen, and runtime type semantics. Chapters 15-34 continue in `type-advanced.md`.

## 完了ステータス

- ✅ **完了**: この文書で直接追跡している core FR はすべて完了しています（12 / 12）。
- 対象範囲は Ch.1「型推論エンジン」と Ch.3「サブタイピングと構造的型付け」の直接 FR です。
- Ch.2 および Ch.4-14 は `type-advanced.md` へのブリッジ参照であり、この文書の完了数には含めません。
- 実装・テストの主な根拠は `packages/type/src/`、`packages/type/tests/`、および `cl-cc-test.asd` の `type-tests` 登録です。

---

## 目次

0. [Requirements Contract](#0-requirements-contract)
1. [型推論エンジン](#1-型推論エンジン)
2. [安全性指向型 / 開発支援型機能 / 型レベルプログラミング / 高度な型構成子（続き）](#2-安全性指向型--開発支援型機能--型レベルプログラミング--高度な型構成子続き)
3. [サブタイピングと構造的型付け](#3-サブタイピングと構造的型付け)
4. [精緻型・依存型（続き）](#4-精緻型依存型)
5. [エフェクト型システム（続き）](#5-エフェクト型システム)
6. [リソース管理型（続き）](#6-リソース管理型)
7. [数値・計量型 / 型システムの実装技術（続き）](#7-数値計量型--型システムの実装技術続き)
8. [動的言語の型統合 / 高度な型クラス機構 / 特殊ドメイン型 / 型と意味論の対応 / 最適化のための型解析（続き）](#8-動的言語の型統合--高度な型クラス機構--特殊ドメイン型--型と意味論の対応--最適化のための型解析続き)
9. [型システムの拡張性と相互運用（続き）](#9-型システムの拡張性と相互運用続き)
10. [高度な型クラス機構（続き）](#10-高度な型クラス機構続き)
11. [特殊ドメイン型（続き）](#11-特殊ドメイン型続き)
12. [型と意味論の対応（続き）](#12-型と意味論の対応続き)
13. [最適化のための型解析 / 型システムの拡張性と相互運用 / 線形論理と型 / 代数的サブタイピングとパス依存型 / TypeScript 型システムの精髄（続き）](#13-最適化のための型解析--型システムの拡張性と相互運用--線形論理と型--代数的サブタイピングとパス依存型--typescript-型システムの精髄続き)
14. [型のエンコーディングと依存型の基盤（続き）](#14-型のエンコーディングと依存型の基盤続き)

---

> Ch.15-34 は [type-advanced.md](type-advanced.md) に続く。

> 注: `新ファイル` とある項目は、現時点では実装予定を示す。実在するモジュール名は `packages/type/src/` 配下の現行ファイルに合わせて読むこと。
> この文書は依存順に読むこと。第1-14章はコア型判断の基盤であり、第15-34章は `type-advanced.md` に続く。下の「続き」節は、後続文書の章立てを先読みするためのブリッジである。
> 本シリーズにおける FR 番号は型システム文書ローカルの参照名であり、他の設計文書に現れる同番号の FR とは無関係である。

## 0. Requirements Contract

### 適用範囲

- 本書は「コア型システム」の要件を定義し、型推論・サブタイピング・codegen/runtime 連携に直接関わる FR を扱う。
- 第2章および第4-14章は後続巻への依存関係を固定するためのブリッジ章であり、詳細な FR 定義は `type-advanced.md` 側に置く。
- 型推論の結果を codegen / optimizer / runtime に渡す場合は、明示的な transport 契約が必要である。

### 依存順

1. 型推論と双方向検査
2. 制約生成/解消
3. 型情報の AST / compile 境界での保持
4. 精緻型・依存型の続き
5. 型情報を使う最適化
6. runtime 型判定の整合

### 分割方針

- `type-core.md`: コア型判断と実行パイプライン接続。ここで直接定義しない章は、後続巻への依存順を示すブリッジとして残す。
- `type-advanced.md`: 安全性指向型、型レベルプログラミング、停止性、証明、並行/数値/DSL 拡張。

### 検証方針

- FR 番号の意味が他文書と衝突する場合は、文書内の依存関係を優先し、必要に応じて注記すること。

## 1. 型推論エンジン

### FR-001: ✅ Hindley-Milner 型推論 (基盤)

- **対象**: `packages/type/src/inference.lisp`
- **現状**: HM 型推論の基盤は存在する。ここでは `infer` をコア入口として定義し、S 式から型スキームを返せることを要件とする
- **アルゴリズム**: Algorithm W (Damas & Milner 1982) + unification (Robinson 1965)
- **スコープ**: let-polymorphism, 型変数の全称量化, unification による型変数束縛
- **参考実装**: OCaml, SML/NJ, GHC Core の型推論基盤

### FR-002: ✅ 双方向型検査 (Bidirectional Type Checking)

- **対象**: `packages/type/src/bidirectional.lisp` / `packages/type/src/checker.lisp`
  - **現状**: `synthesize` / `check` の入口は `bidirectional.lisp` と `checker.lisp` 側に整理されている。ここでは双方向検査をコア契約として固定し、文脈から期待型を伝播できることを要件とする
- **内容**: Dunfield & Krishnaswami (2013) の双方向型検査
  - **synthesize モード**: 式から型を合成 (`e ⇒ A`)
  - **check モード**: 期待型を文脈から渡す (`e ⇐ A`)
  - `(the T e)` が `e` を check モードで `T` に対して検査
  - `(lambda (x) (+ x 1))` に `(function (fixnum) fixnum)` を check モードで渡すと `x: fixnum` が自動推論
- **根拠**: OCaml / Rust / GHC の型推論アーキテクチャの核心
- **難易度**: Very Hard

### FR-003: ✅ 制約ベース型推論 HM(X)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: Sulzmann & Stuckey の HM(X) フレームワーク。型推論を制約生成と制約解消の 2 フェーズに分離
  - **制約生成フェーズ**: ASTを走査して型制約集合 `C` を生成
  - **制約解消フェーズ**: `C` を unification + 専用ソルバで解く
  - 拡張点: 型クラス制約、型族制約、linear 型制約を同一フレームワークで処理可能
- **利点**: 型クラス (typeclass)、型族 (type families)、GADT 制約を HM に均一に統合できる
- **参考**: GHC の OutsideIn(X) (Schrijvers et al. 2009)
- **難易度**: Hard

### FR-004: ✅ 多相再帰 (Polymorphic Recursion)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: Milner 制限を超えて再帰関数に異なる型でのモノモーフィック呼び出しを許可
  - `(defun length (x)` が `(length '(1 2))` → `integer` と `(length "str")` → `integer` を同時に型付け可能
  - HM は多相再帰を決定不可能とするため、**型アノテーション必須**（Haskell の `-XScopedTypeVariables` 相当）
  - 実装: `(declare (type (function ((list-of :a)) integer) length))` アノテーション付き関数で多相再帰を許可
- **参考**: Haskell (`-XPolymorphicComponents`), Ocaml の明示的多相アノテーション
- **難易度**: Hard

### FR-005: ✅ 型アノテーション → Codegen 接続

- **依存**: FR-002
- **対象**: `packages/type/src/inference.lisp` + `packages/compile/src/codegen.lisp`
- **内容**: `compiler-context` / `compilation-result` に型環境 `type-env` を保持し、`infer` の結果を codegen 境界へ伝達する。`compile-ast` は `ctx-type-env` を参照できる
- **効果**: fixnum fast path（型チェック命令省略）、float unboxing 選択

### FR-006: ✅ Fixnum Fast Path (型特化算術)

- **依存**: FR-005
- **内容**: `+`/`-`/`*`/`<`/`>`/`=` で両オペランドが fixnum と判明している場合、型チェック命令を生成しない
- **効果**: SBCL の `(declare (type fixnum x))` 相当の効果をアノテーション由来で実現

### FR-007: ✅ 型伝播による条件分岐特化

- **依存**: FR-005
- **内容**: `(if (numberp x) ...)` の true ブランチ内で `x` を fixnum として扱う
- **活用**: `packages/type/src/inference.lisp` の `extract-type-guard` が既に型絞り込みを実装済み → codegen 側で活用

---

## 2. 安全性指向型 / 開発支援型機能 / 型レベルプログラミング / 高度な型構成子（続き）

> 注: この章は `type-advanced.md` Ch.15-18 に続く。

- Ch.15 安全性指向型
- Ch.16 開発支援型機能
- Ch.17 型レベルプログラミング
- Ch.18 高度な型構成子

---

## 3. サブタイピングと構造的型付け

### FR-201: ✅ 公称サブタイピング (Nominal Subtyping)

- **対象**: `packages/vm/src/primitives.lisp`, `packages/type/src/subtyping.lisp`
- **内容**: 名前で同一性を判定するサブタイピング（Java/C#/CLOS 方式）
  - CLOS の `:include` / `defclass` 継承で定義される型階層を型推論に反映
  - `subtypep` の完全実装（FR-801 参照）

### FR-202: ✅ 構造的サブタイピング (Structural Subtyping)

- **対象**: `packages/type/src/subtyping.lisp`, `packages/type/src/parser-extended.lisp`, `packages/type/src/inference.lisp`
- **内容**: 名前でなく構造（スロット・メソッドシグネチャ）で互換性を判定
  - Go のインターフェース、TypeScript の structural typing に相当
  - `(has-slots :x :y)` 型を満たすオブジェクトはすべて渡せる
  - CLOS との橋渡し: `(protocol drawable)` — `draw` メソッド登録を持つクラスは準拠
- **難易度**: Hard

### FR-203: ✅ 行多相 (Row Polymorphism)

- **対象**: `packages/type/src/inference.lisp`
- **内容**: レコード（ハッシュテーブル / struct）の拡張可能なフィールド型
  - OCaml の多相レコード型: `{x: int; y: int | r}` — `r` は残りのフィールドを表す型変数
  - `(function ((row (:x integer) :r)) integer)` — `:x` フィールドを持つ任意のレコードを受け付ける
  - **開放型 vs 閉鎖型**: 行変数あり（サブタイプ受け付け）vs 行変数なし（完全一致のみ）
  - PureScript、Koka、Dotty (Scala 3) で採用
  - CL-CC 応用: `defstruct` や `make-hash-table` を使ったレコード型への適用
- **難易度**: Hard

### FR-204: ✅ 交差型 (Intersection Types) と合併型 (Union Types)

- **対象**: `packages/type/src/parser.lisp`, `packages/type/src/inference.lisp`
- **内容**:
  - **交差型**: `(and integer string)` — 両方の型の性質を持つ（TypeScript の `A & B`）
  - **合併型**: `(or integer string)` — どちらかの型（TypeScript の `A | B`、Rust の enum）
  - **非合法な型**: `(and integer string)` のような明らかに inhabitant を持たない primitive 交差をコンパイル時検出
  - **occurrence typing との連携**: `(or integer string)` 型の値に対する `(integerp x)` チェック後の自動絞り込み
- **ANSI CL**: `and`/`or`/`not` 型指定子として既存。型推論への統合を定義
- **難易度**: Medium

### FR-205: ✅ 部分型多相 (Bounded Polymorphism / Constrained Polymorphism)

- **対象**: `packages/type/src/types-core.lisp`, `packages/type/src/unification.lisp`, `packages/type/src/parser-extended.lisp`, `packages/type/src/solver.lisp`
- **内容**: 型変数に上限 (upper bound) / 下限 (lower bound) 制約を付与
  - Scala の `[A <: Comparable[A]]` — `A` は `Comparable` のサブタイプに限定
  - Java の `<T extends Number>` — ジェネリクスの bounded wildcard
  - CL-CC の型クラス制約 `(forall :a (constraint (orderable :a)) ...)` として表現
- **難易度**: Hard

---

## 4. 精緻型・依存型（続き）

> 注: この章は `type-advanced.md` Ch.19-20 に続く。

- Ch.19 停止性・全域性・正当性
- Ch.20 型と証明

---

## 5. エフェクト型システム（続き）

> 注: この章は `type-advanced.md` Ch.21 に続く。

- Ch.21 型システムとツール連携

---

## 6. リソース管理型（続き）

> 注: この章は `type-advanced.md` Ch.22 に続く。

- Ch.22 並行・分散・非同期型

---

## 7. 数値・計量型 / 型システムの実装技術（続き）

> 注: この章は `type-advanced.md` Ch.23-24 に続く。

- Ch.23 数値・計量型
- Ch.24 型システムの実装技術

---

## 8. 動的言語の型統合 / 高度な型クラス機構 / 特殊ドメイン型 / 型と意味論の対応 / 最適化のための型解析（続き）

> 注: この章は `type-advanced.md` Ch.25-29 に続く。

- Ch.25 動的言語の型統合
- Ch.26 高度な型クラス機構
- Ch.27 特殊ドメイン型
- Ch.28 型と意味論の対応
- Ch.29 最適化のための型解析

---

## 9. 型システムの拡張性と相互運用（続き）

> 注: この章は `type-advanced.md` Ch.30 に続く。

- Ch.30 型システムの拡張性と相互運用

---

## 10. 高度な型クラス機構（続き）

> 注: この章は `type-advanced.md` Ch.26 に続く。

- Ch.26 高度な型クラス機構

---

## 11. 特殊ドメイン型（続き）

> 注: この章は `type-advanced.md` Ch.27 に続く。

- Ch.27 特殊ドメイン型

---

## 12. 型と意味論の対応（続き）

> 注: この章は `type-advanced.md` Ch.28 に続く。

- Ch.28 型と意味論の対応

---

## 13. 最適化のための型解析 / 型システムの拡張性と相互運用 / 線形論理と型 / 代数的サブタイピングとパス依存型 / TypeScript 型システムの精髄（続き）

> 注: この章は `type-advanced.md` Ch.29-33 に続く。

- Ch.29 最適化のための型解析
- Ch.30 型システムの拡張性と相互運用
- Ch.31 線形論理と型
- Ch.32 代数的サブタイピングとパス依存型
- Ch.33 TypeScript 型システムの精髄

---

## 14. 型のエンコーディングと依存型の基盤（続き）

> 注: この章は理論的背景の整理であり、core の直接的な実装要件ではない。

- Ch.34 型のエンコーディングと依存型の基盤
