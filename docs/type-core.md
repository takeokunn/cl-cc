# Type System: Core

Core type system contract for the compiler: inference, bidirectional checking, constraint solving, type transport into codegen, runtime type semantics, and ANSI CL completion. Advanced research features are deferred to `type-advanced.md`.

---

## 目次

0. [Requirements Contract](#0-requirements-contract)
1. [型推論エンジン](#1-型推論エンジン)
2. [型の表現力 — 多相性と型コンストラクタ](#2-型の表現力--多相性と型コンストラクタ)
3. [サブタイピングと構造的型付け](#3-サブタイピングと構造的型付け)
4. [精緻型・依存型](#4-精緻型依存型)
5. [エフェクト型システム](#5-エフェクト型システム)
6. [リソース管理型](#6-リソース管理型)
7. [段階的型付けと契約](#7-段階的型付けと契約)
8. [実行時型システムと最適化連携](#8-実行時型システムと最適化連携)
9. [ANSI CL 型システム完全化](#9-ansi-cl-型システム完全化)
10. [型クラスとトレイト](#10-型クラスとトレイト)
11. [変性と型の互換性](#11-変性と型の互換性)
12. [型の構造的拡張](#12-型の構造的拡張)
13. [高度な多相性](#13-高度な多相性)
14. [型理論の基礎](#14-型理論の基礎)

---

> Ch.15-34 は [type-advanced.md](type-advanced.md) に続く。

> 注: `新ファイル` とある項目は、現時点では実装予定を示す。実在するモジュール名は `src/type/` 配下の現行ファイルに合わせて読むこと。
> この文書は依存順に読むこと。`1-4` は型判断の基盤、`5-8` は型情報の後段接続、`9-14` は ANSI CL / 汎用型理論との整合である。

## 0. Requirements Contract

### 適用範囲

- 本書は「コア型システム」の要件を定義する。
- 依存型、所有権/借用検査、宇宙多相、非述語的多相、証明支援系の機能はコア範囲外とする。
- 型推論の結果を codegen / optimizer / runtime に渡す場合は、明示的な transport 契約が必要である。

### 依存順

1. 型推論と双方向検査
2. 制約生成/解消
3. 型情報の AST / compile 境界での保持
4. 型駆動 codegen
5. 型情報を使う最適化
6. runtime 型判定の整合

### 分割方針

- `type-core.md`: コア型判断と実行パイプライン接続。
- `type-advanced.md`: 安全性指向型、型レベルプログラミング、停止性、証明、並行/数値/DSL 拡張。

### 検証方針

- 各 FR は少なくとも 1 つのテスト、または既存実装への参照を持つこと。
- FR 番号の意味が他文書と衝突する場合は、文書内の依存関係を優先し、必要に応じて注記すること。

## 1. 型推論エンジン

### FR-001: Hindley-Milner 型推論 (基盤)

- **対象**: `src/type/inference.lisp`
 - **現状**: HM 型推論の基盤は存在する。ここでは `infer` をコア入口として定義し、S 式から型スキームを返せることを要件とする
- **アルゴリズム**: Algorithm W (Damas & Milner 1982) + unification (Robinson 1965)
- **スコープ**: let-polymorphism, 型変数の全称量化, unification による型変数束縛
- **参考実装**: OCaml, SML/NJ, GHC Core の型推論基盤

### FR-002: 双方向型検査 (Bidirectional Type Checking)

- **対象**: `src/type/inference.lisp`
 - **現状**: `synthesize` / `check` の入口は存在する。ここでは双方向検査をコア契約として固定し、文脈から期待型を伝播できることを要件とする
- **内容**: Dunfield & Krishnaswami (2013) の双方向型検査
  - **synth モード**: 式から型を合成 (`e ⇒ A`)
  - **check モード**: 期待型を文脈から渡す (`e ⇐ A`)
  - `(the T e)` が `e` を check モードで `T` に対して検査
  - `(lambda (x) (+ x 1))` に `(function (fixnum) fixnum)` を check で渡すと `x: fixnum` が自動推論
- **根拠**: OCaml / Rust / GHC の型推論アーキテクチャの核心
- **難易度**: Very Hard

### FR-003: 制約ベース型推論 HM(X)

- **対象**: `src/type/inference.lisp`
- **内容**: Sulzmann & Stuckey の HM(X) フレームワーク。型推論を制約生成と制約解消の 2 フェーズに分離
  - **制約生成フェーズ**: ASTを走査して型制約集合 `C` を生成
  - **制約解消フェーズ**: `C` を unification + 専用ソルバで解く
  - 拡張点: 型クラス制約、型族制約、linear 型制約を同一フレームワークで処理可能
- **利点**: 型クラス (typeclass)、型族 (type families)、GADT 制約を HM に均一に統合できる
- **参考**: GHC の OutsideIn(X) (Schrijvers et al. 2009)
- **難易度**: Hard

### FR-004: 多相再帰 (Polymorphic Recursion)

- **対象**: `src/type/inference.lisp`
- **内容**: Milner 制限を超えて再帰関数に異なる型でのモノモーフィック呼び出しを許可
  - `(defun length (x)` が `(length '(1 2))` → `integer` と `(length "str")` → `integer` を同時に型付け可能
  - HM は多相再帰を決定不可能とするため、**型アノテーション必須**（Haskell の `-XScopedTypeVariables` 相当）
  - 実装: `(declare (type (function ((list-of :a)) integer) length))` アノテーション付き関数で多相再帰を許可
- **参考**: Haskell (`-XPolymorphicComponents`), Ocaml の明示的多相アノテーション
- **難易度**: Hard

### FR-005: 型アノテーション → Codegen 接続

- **依存**: FR-002
- **対象**: `src/type/inference.lisp` + `src/compile/codegen.lisp`
- **内容**: `compiler-context` / `compilation-result` に型環境 `type-env` を保持し、`infer` の結果を codegen 境界へ伝達する。`compile-ast` は `ctx-type-env` を参照できる
- **効果**: fixnum fast path（型チェック命令省略）、float unboxing 選択

### FR-006: Fixnum Fast Path (型特化算術)

- **依存**: FR-005
- **内容**: `+`/`-`/`*`/`<`/`>`/`=` で両オペランドが fixnum と判明している場合、型チェック命令を生成しない
- **効果**: SBCL の `(declare (type fixnum x))` 相当の効果をアノテーション由来で実現

### FR-007: 型伝播による条件分岐特化

- **依存**: FR-005
- **内容**: `(if (numberp x) ...)` の true ブランチ内で `x` を fixnum として扱う
- **活用**: `src/type/inference.lisp` の `extract-type-guard` が既に型絞り込みを実装済み → codegen 側で活用

---

## 2. 型の表現力 — 多相性と型コンストラクタ

### FR-101: 高カインド型 (Higher-Kinded Types, HKT)

- **対象**: `src/type/parser.lisp`, `src/type/inference.lisp`
- **内容**: 型コンストラクタを型変数として抽象化。`(type-var :F :* -> :*)` のように**カインド**（型の型）を導入
  - `Functor F` の定義: `(deftype-class functor (:f)` with `map: (function ((function (:a) :b) (:f :a)) (:f :b)))`
  - `(list integer)`, `(maybe string)`, `(tree :a)` をすべて `Functor` インスタンスとして統一的に扱う
- **実用例**: Haskell の Functor/Monad/Traversable、Scala 3 の type lambda
- **難易度**: Very Hard

### FR-102: Rank-N 多相 (Rank-N Polymorphism)

- **対象**: `src/type/inference.lisp`
- **内容**: 多相型を引数に受け取る関数を表現。HM は Rank-2 以上の多相性を自動推論できないため、型アノテーション必須
  - Rank-1 (HM): `(forall :a (function (:a) :a))` — 呼び出し時に具体化
  - Rank-2: `(function ((forall :a (function (:a) :a))) integer)` — 引数が多相関数
  - Rank-N (任意): GHC `-XRankNTypes`、`runST :: (forall s. ST s a) -> a`
- **CL-CC 応用**: `our-map` の引数 `fn` を多相関数として扱う型付け
- **難易度**: Very Hard

### FR-103: 一般化代数データ型 (GADTs)

- **対象**: `src/type/inference.lisp`, `src/parse/cl/parser.lisp`
- **内容**: データコンストラクタごとに異なる返り型を持つ代数データ型
  ```
  ;; 型インデックス付きAST (例)
  (defgadt ast-typed
    (lit-int  :n => (ast-typed integer))
    (lit-bool :b => (ast-typed boolean))
    (add      (ast-typed integer) (ast-typed integer) => (ast-typed integer))
    (if-expr  (ast-typed boolean) (ast-typed :a) (ast-typed :a) => (ast-typed :a)))
  ```
  - パターンマッチ時に型の精緻化（type refinement）が自動で起きる
  - GHC `-XGADTs`, Scala 3 の sealed family
- **応用**: CL-CC 自身の AST に型インデックスを付与して型安全な AST 変換を実現
- **難易度**: Very Hard

### FR-104: 存在型 (Existential Types)

- **対象**: `src/type/inference.lisp`
- **内容**: 型パラメータを外から隠す「パック化された多相型」
  - `(exists :a (product :a (function (:a) string)))` — 内部型 `:a` を隠蔽
  - Haskell の `data Showable = forall a. Show a => Showable a`
  - **用途**: 異種コレクション `(list showable)` — すべての要素が `show` できるが具体的型は異なる
  - CPS 変換後の継続型 `(cont :a)` の実装に応用可能
- **難易度**: Hard

### FR-105: ファントム型 (Phantom Types)

- **対象**: `src/type/parser.lisp`
- **内容**: 実行時には現れない型パラメータでコンパイル時の状態・フェーズ・単位を表現
  ```lisp
  (defstruct (handle (:phantom :state)) ...)
  ;; (handle :open) と (handle :closed) を区別
  ;; read-handle は (handle :open) のみ受け付ける
  ```
  - 単位系チェック: `(meters :tagged)` vs `(feet :tagged)` の混在をコンパイル時に検出
  - フェーズ型: `(ast :parsed)` → `(ast :expanded)` → `(ast :compiled)` の変換の正しさを保証
- **難易度**: Medium

### FR-106: シングルトン型 (Singleton Types / DataKinds)

- **対象**: `src/type/parser.lisp`
- **内容**: 値をそのまま型に昇格させる
  - Haskell の `DataKinds`: `Nat` 型を型レベルで `0, 1, 2, ...` として使用
  - `(type-of-length '(1 2 3))` → `(vector 3 integer)` — 長さが型に現れる
  - 依存型の第一歩（完全な依存型への足掛かり）
- **応用**: 固定長バッファの境界チェック除去、行列型の次元チェック
- **難易度**: Hard

### FR-107: 型族 (Type Families)

- **対象**: `src/type/inference.lisp`
- **内容**: 型レベルの関数。型コンストラクタを引数に取り別の型を返す
  - GHC の `type family Element (f :: * -> *) :: *`
  - `(type-family element-of (list :a) = :a)`, `(type-family element-of string = character)`
  - **関連型** (Associated Types): 型クラスのインスタンスに紐付いた型族
- **難易度**: Very Hard

---

## 3. サブタイピングと構造的型付け

### FR-201: 公称サブタイピング (Nominal Subtyping)

- **対象**: `src/vm/primitives.lisp`, `src/type/subtyping.lisp`
- **内容**: 名前で同一性を判定するサブタイピング（Java/C#/CLOS 方式）
  - CLOS の `:include` / `defclass` 継承で定義される型階層を型推論に反映
  - `subtypep` の完全実装（FR-801 参照）

### FR-202: 構造的サブタイピング (Structural Subtyping)

- **対象**: `src/type/inference.lisp`
- **内容**: 名前でなく構造（スロット・メソッドシグネチャ）で互換性を判定
  - Go のインターフェース、TypeScript の structural typing に相当
  - `(has-slots :x :y)` 型を満たすオブジェクトはすべて渡せる
  - CLOS との橋渡し: `(protocol drawable)` — `draw` メソッドを持つクラスはすべて準拠
- **難易度**: Hard

### FR-203: 行多相 (Row Polymorphism)

- **対象**: `src/type/inference.lisp`
- **内容**: レコード（ハッシュテーブル / struct）の拡張可能なフィールド型
  - OCaml の多相レコード型: `{x: int; y: int | r}` — `r` は残りのフィールドを表す型変数
  - `(function ((row (:x integer) :r)) integer)` — `:x` フィールドを持つ任意のレコードを受け付ける
  - **開放型 vs 閉鎖型**: 行変数あり（サブタイプ受け付け）vs 行変数なし（完全一致のみ）
  - PureScript、Koka、Dotty (Scala 3) で採用
  - CL-CC 応用: `defstruct` や `make-hash-table` を使ったレコード型への適用
- **難易度**: Hard

### FR-204: 交差型 (Intersection Types) と合併型 (Union Types)

- **対象**: `src/vm/primitives.lisp`, `src/type/inference.lisp`
- **内容**:
  - **交差型**: `(and integer string)` — 両方の型の性質を持つ（TypeScript の `A & B`）
  - **合併型**: `(or integer string)` — どちらかの型（TypeScript の `A | B`、Rust の enum）
  - **非合法な型**: `(and integer string)` が inhabitant を持たない場合をコンパイル時検出
  - **occurrence typing との連携**: `(or integer string)` 型の値に対する `(integerp x)` チェック後の自動絞り込み
- **ANSI CL**: `and`/`or`/`not` 型指定子として既存。型推論への統合が未実装
- **難易度**: Medium

### FR-205: 部分型多相 (Bounded Polymorphism / Constrained Polymorphism)

- **対象**: `src/type/inference.lisp`
- **内容**: 型変数に上限 (upper bound) / 下限 (lower bound) 制約を付与
  - Scala の `[A <: Comparable[A]]` — `A` は `Comparable` のサブタイプに限定
  - Java の `<T extends Number>` — ジェネリクスの bounded wildcard
  - CL-CC の型クラス制約 `(forall :a (constraint (orderable :a)) ...)` として表現
- **難易度**: Hard

---

## 4. 精緻型・依存型

> 注: FR-301〜304 は core の境界領域である。ここでは要件を catalog するが、実装優先度と acceptance は `type-advanced.md` 側の前提に従う。

### FR-301: 精緻型 (Refinement Types)

- **対象**: `src/type/inference.lisp`, `src/type/parser.lisp`
- **内容**: 型に述語による精緻化を付与
  - Liquid Haskell: `{v: Int | v > 0}` — 正の整数型
  - `(refine integer (lambda (v) (> v 0)))` → 実行時チェックまたは SMT ソルバによる静的検証
  - **Liquid Types** (Rondon et al. 2008): HM 型推論 + 述語変数のテンプレートから自動的に精緻型を推論
  - **SMT 連携**: Z3 / CVC5 などの SMT ソルバで精緻型の整合性を静的検証
  - CL-CC 応用: `(the (refine integer pos-p) n)` で `n > 0` をコンパイル時証明
- **難易度**: Very Hard

### FR-302: 出現型 / フロー感応型推論 (Occurrence Typing)

- **対象**: `src/type/inference.lisp`, `src/compile/codegen.lisp`
- **内容**: Typed Racket の occurrence typing (Tobin-Hochstadt & Felleisen 2008)
  - `(and (consp x) (numberp (car x)))` の true ブランチで `x: cons`, `(car x): number` を自動推論
  - `(typecase x (fixnum ...) (string ...))` の各 arm での型環境自動更新
  - **論理命題型フィルタ**: 型述語 `integerp` / `consp` / `stringp` の true/false 双方に対して型絞り込みを追跡
  - `(or (and a b) c)` をまたぐ型絞り込みを **latent predicate** として追跡
  - TypeScript の narrowing、Flow の type refinement と同等
- **難易度**: Hard

### FR-303: 依存型 (Dependent Types)

- **対象**: 予定ファイル `src/type/dependent.lisp`
- **現状**: 未実装（対応モジュールなし）
- **内容**: 型が値に依存する型システム
  - **依存関数型 (Π 型)**: `(pi (n : nat) (vector n integer))` — 長さ `n` の整数ベクタ
  - **依存ペア型 (Σ 型)**: `(sigma (n : nat) (vector n integer))` — 長さと対応するベクタのペア
  - **等価型**: `(= a b)` — `a` と `b` が等しいという命題型（Curry-Howard 対応）
  - 実装形態: **full dependent types** (Idris 2, Lean 4, Agda) vs **dependent function types only** (ATS, F*)
  - **型検査の決定可能性**: 型等値は強正規化が必要。CL の `eval` との干渉に注意
  - CL-CC 初期ターゲット: 配列の境界チェック除去 `(vector-ref v i)` where `(< i (length v))`
- **難易度**: Extremely Hard

### FR-304: 型レベル計算 (Type-Level Computation)

- **対象**: `src/type/inference.lisp`
- **内容**: 型推論時に型式の計算を実行
  - Haskell の型族 (type families) による型レベル `Nat` 演算
  - `(type-plus (lit 2) (lit 3)) = (lit 5)` — 型レベル加算
  - **型正規化**: 型等値判定のために型式を正規形に簡約
  - GHC の `TypeFamilies` + `DataKinds` の組み合わせ
- **難易度**: Very Hard

---

## 5. エフェクト型システム

### FR-401: エフェクト型 (Effect Types)

- **対象**: 予定ファイル `src/type/effects.lisp`
- **現状**: 未実装（現行実装は `src/type/effect.lisp` と `src/type/inference-effects.lisp` の一部）
- **内容**: 関数の副作用を型に記録する
  - **エフェクトの種類**: `:io` (I/O), `:state` (変更可能状態), `:exception` (例外), `:nondeterminism` (非決定性), `:divergence` (非停止)
  - 関数型: `(function ((integer) (integer)) integer ! (:io :state))` — I/O と状態変更を持つ関数
  - **純粋関数**: エフェクトセットが空 `{}` の関数は、モデル化された副作用がない範囲でメモ化・並列化の候補となる
  - **エフェクト多態性**: `(function (:a) :b ! :e)` — エフェクトを型変数 `:e` で抽象化
  - 参考実装: Koka (Microsoft Research), Frank, Eff, Helium
- **難易度**: Very Hard

### FR-402: 代数的エフェクトとハンドラ (Algebraic Effects & Handlers)

- **対象**: 予定ファイル `src/type/effects.lisp`, `src/vm/effects.lisp`
- **現状**: 未実装
- **内容**: エフェクトを first-class な操作として定義し、ハンドラで意味付け
  - **エフェクト定義**: `(define-effect state (get : (unit -> :s)) (put : (:s -> unit)))`
  - **ハンドラ**: `(with-handler state-handler (get () k → (k current-state)) ...)` でエフェクトを解釈
  - 型との連携: エフェクトが型に現れるため静的検査可能
  - OCaml 5.0 の `effect` keyword (Domain + Effect Handlers)、Koka の `with handler` 構文
  - **Continuation Passing との関係**: CL-CC の CPS 変換はエフェクトハンドラの自然な実装基盤
  - Plotkin & Pretnar (2009) の algebraic effects + handlers
- **難易度**: Very Hard

### FR-403: モナド型クラスとモナド変換子 (Monad Typeclasses)

- **対象**: `src/type/inference.lisp`
- **内容**: 副作用をモナドとして型にエンコードする Haskell 流のアプローチ
  - `(type-class monad (:m) (bind ...) (return ...) ...)` の定義
  - `IO`、`State`、`Maybe`、`Either`、`List` モナドの型クラスインスタンス
  - `do` 記法 → `bind` チェーンへのデシュガー（`src/expand/expander.lisp` に追加）
  - Haskell の `mtl` (monad transformer library) 相当の設計
- **難易度**: Hard

### FR-404: 係数型 / コエフェクト (Coeffects)

- **対象**: 予定ファイル `src/type/coeffects.lisp`
- **現状**: 未実装
- **内容**: 関数が**必要とするコンテキスト**を型で追跡（エフェクトの双対）
  - **リソース使用量**: `{uses: 2}` — 引数を 2 回使用する（線形型の一般化）
  - **データフロー**: どの入力変数がどの出力に影響するか（セキュリティ解析に応用）
  - **暗黙的パラメータ**: Haskell の `ImplicitParams` を係数として型付け
  - Petricek et al. (2014) の coeffect system
- **難易度**: Very Hard

---

## 6. リソース管理型

> 注: FR-501〜505 は resource-safety の拡張群であり、core contract の必須前提ではない。

### FR-501: 線形型 / アフィン型 (Linear / Affine Types)

- **対象**: `src/type/inference.lisp`, `src/type/parser.lisp`
- **現状**: 部分実装（`src/type/multiplicity.lisp` で等級はあるが、線形/アフィン型の型検査は未実装）
- **内容**:
  - **線形型**: 値をちょうど 1 回使用することを型で保証 (Wadler 1990)
  - **アフィン型**: 高々 1 回使用（Rust の move semantics に相当）
  - `(declare (affine stream))` で `stream` が 1 パス内で `close` されることをコンパイル時検証
  - CPS 形式では継続の線形使用（delimited continuation の 1 回実行保証）と連携
  - Linear Haskell (POPL 2018 / GHC `-XLinearTypes`)
- **難易度**: Very Hard

### FR-502: 所有権型 / 借用検査 (Ownership Types & Borrow Checker)

- **対象**: 予定ファイル `src/type/ownership.lisp`
- **現状**: 未実装
- **内容**: Rust のオーナーシップシステムを型で表現
  - **オーナーシップ移転**: `(move x)` でオーナーシップを消費。以降 `x` は型エラー
  - **共有借用**: `&x` — 読み取り専用参照。複数同時可
  - **排他借用**: `&mut x` — 書き込み可能参照。同時に 1 つのみ
  - **ライフタイム**: 参照が指す値より長生きしないことを型で保証
  - Rust 2024 edition の Non-Lexical Lifetimes (NLL) + Polonius
- **難易度**: Extremely Hard

### FR-503: 一意型 (Uniqueness Types)

- **対象**: 予定ファイル `src/type/uniqueness.lisp`
- **現状**: 未実装
- **内容**: Clean 言語の一意型システム。値に「一意性属性」を付与
  - `*a` — 一意型（この値への参照は世界で 1 つ）
  - `a` — 非一意型（共有可能）
  - **I/O との連携**: 一意型の `World` トークンを渡すことで I/O の順序を型で保証
  - **インプレース更新の最適化**: 一意型オブジェクトはコピー不要でインプレース更新可能
  - GHC の `ST` モナド（疑似的な一意型）、Mercury の determinism
- **難易度**: Very Hard

### FR-504: Typestate (型状態)

- **対象**: `src/type/inference.lisp`
- **内容**: オブジェクトのライフサイクルを型で表現
  ```lisp
  (defprotocol file-protocol
    (:state :closed)
    (open  :closed -> :open)
    (read  :open   -> :open)
    (close :open   -> :closed))
  ;; close 後に read するとコンパイル時エラー
  ```
  - ファイルハンドル・ネットワーク接続・データベーストランザクションのリソース管理
  - Rust のゼロコスト状態機械と同等の静的保証
  - Plaid 言語 (Aldrich et al. 2009) のオリジナル設計
- **難易度**: Hard

### FR-505: セッション型 (Session Types)

- **対象**: 予定ファイル `src/type/sessions.lisp`
- **現状**: 未実装
- **内容**: 通信プロトコルを型でエンコード
  - `!Int.?String.End` — 整数を送り、文字列を受け取り、終了
  - **双対性**: クライアント型とサーバー型が双対 (dual) であることをコンパイル時検証
  - 線形型との連携: チャンネルを線形に使用（同じメッセージを 2 回送れない）
  - Lindley & Morris (2016) の GV (Good Variation) 型システム
  - Rust の `crossbeam-channel` での実験的実装
- **難易度**: Extremely Hard

---

## 7. 段階的型付けと契約

### FR-601: 段階的型付けと責任追跡 (Gradual Typing with Blame Assignment)

- **対象**: `src/type/inference.lisp`, `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **内容**: Siek & Taha (2006) の段階的型付け
  - 型付きコードと型なしコードの境界に **cast** を自動挿入
  - cast 失敗時に `blame label` を含むエラーを発生させ「どちら側のコードが型契約を破ったか」を特定
  - `(declare (type fixnum x))` 付き関数を型なしコードから呼び出した場合に引数を動的チェック
  - Racket の `typed/untyped` boundary contracts
- **難易度**: Very Hard

### FR-602: 設計による契約 (Design by Contract)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **内容**: `defun/c` マクロ（または `defun` 拡張）に `:requires` / `:ensures` / `:invariant` を追加
  - `:requires`: 事前条件 `(requires (> n 0))`
  - `:ensures`: 事後条件 `(ensures (>= result 0))`
  - `defclass` の `:invariant`: スロット間の不変条件
  - `(optimize (safety 0))` で全チェック除去可能
  - **静的検証モード**: 型推論 + SMT で条件が常に真/偽と証明できる場合はコンパイル時エラー/除去
  - Eiffel の Design by Contract / SPARK Ada / Racket `racket/contract`
- **難易度**: Hard

### FR-603: モジュール型とシグネチャ (Module Types / Signatures)

- **対象**: 予定ファイル `src/type/modules.lisp`
- **現状**: 未実装
- **内容**: OCaml の module system / SML のシグネチャに相当
  - `(module-type stack-sig (push ...) (pop ...) (empty ...))` — インターフェース定義
  - `(module stack-impl : stack-sig ...)` — 実装がシグネチャを満たすことを検査
  - **ファンクタ**: `(functor (M : ordered-sig) stack-functor ...)` — 型パラメータ付きモジュール
  - **抽象型 / 不透明型**: `(abstract-type t)` でモジュール外部から内部表現を隠す
  - CL の `defpackage` + `defgeneric` の型付け版として実装可能
- **難易度**: Very Hard

---

## 8. 実行時型システムと最適化連携

### FR-701: エスケープ解析 (Escape Analysis)

- **対象**: `src/compile/closure.lisp` + 新パス
- **内容**: オブジェクト（コンスセル・クロージャ）が関数外に「逃げる」か解析
  - 逃げない場合: ヒープではなくスタック/レジスタへ割り当て
  - エスケープ条件: 他関数への引数渡し、グローバル変数への代入、クロージャキャプチャ
  - JVM の Escape Analysis (Java 17+)、Go の escape analysis と同等
- **効果**: GC プレッシャー軽減、コンス割り当てコスト削減
- **難易度**: Hard

### FR-702: スカラ置換 (Scalar Replacement of Aggregates, SROA)

- **依存**: FR-701
- **内容**: エスケープしないコンスセル・構造体をスカラレジスタに分解
  - `(cons a b)` が外部に逃げない場合、car/cdr をそれぞれ独立レジスタとして扱う
  - LLVM の SROA パスと同等
  - 効果: ヒープ割り当て完全除去 + GC プレッシャー削減
- **難易度**: Hard

### FR-703: 型特化インライン展開 (Type-Directed Specialization)

- **依存**: FR-005, FR-701
- **内容**: 引数型が判明している呼び出し箇所で型特化版のインライン展開を生成
  - GHC の worker/wrapper 変換に相当
  - ポリモーフィック関数のモノモーフィック特化 (Monomorphization)
  - Rust / C++ テンプレートのコンパイル時特化と同等
- **難易度**: Hard

### FR-704: 単相化 (Monomorphization)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 多相関数を具体的な型ごとにコピー生成して型特化版を作成
  - Rust / C++ の generics モデル（辞書渡しモデルとは逆）
  - 長所: 仮想ディスパッチ不要、インライン展開しやすい
  - 短所: コードサイズ増大（C++ テンプレートの「コードブロート」）
  - GHC の SPECIALIZE pragma 相当
- **難易度**: Hard

### FR-705: 型消去 vs 型具体化 (Type Erasure vs Reification)

- **対象**: `src/compile/codegen.lisp`
- **内容**:
  - **型消去 (Type Erasure)**: コンパイル後に型情報を削除（Java/ML の方式）。実行時オーバーヘッドなし
  - **型具体化 (Type Reification)**: 型情報を実行時オブジェクトとして保持（C# の Reified Generics）
  - CL-CC: CLOS の `class-of` / `type-of` は型具体化モデル。HM 型推論結果は消去
  - **反射的プログラミング**: 型具体化があれば `(typep x (type-of-value expected))` が可能
- **難易度**: Medium

### FR-706: 型駆動コード生成 (Type-Directed Code Generation)

- **対象**: `src/compile/codegen.lisp`
- **内容**: 型推論結果を使って最適な VM 命令を選択
  - `fixnum + fixnum` → `vm-integer-add` (型チェックなし)
  - `float + float` → `vm-float-add` (unboxed)
  - `integer` (型不明) → `vm-add` (ランタイム型ディスパッチ)
  - **NaN-boxing 最適化**: 型が判明している値は NaN-boxing のアンパック命令を省略
- **難易度**: Medium

---

## 9. ANSI CL 型システム完全化

### FR-801: subtypep 完全化

- **対象**: `src/vm/primitives.lisp`, `src/type/subtyping.lisp`
- **内容**: `subtypep` を ANSI CL 準拠で実装。`*subtype-table*` で型包含関係を宣言的に管理
- **根拠**: ANSI CL 4.3 — type relationships
- **難易度**: Hard

### FR-802: 複合型指定子 (and/or/not/satisfies/member)

- **対象**: `src/vm/primitives.lisp`
- **内容**: `and`, `or`, `not`, `satisfies`, `member`, `eql` 複合型指定子を `typep` に実装
- **根拠**: ANSI CL 4.2.3 — type specifiers
- **難易度**: Hard

### FR-803: vm-type-of 完全化

- **対象**: `src/vm/primitives.lisp`
- **内容**: NaN-boxing タグから ANSI CL 準拠の型名へのマッピングを完全化
  - `(type-of 42)` → `fixnum`, `(type-of 3.14)` → `single-float`
- **根拠**: ANSI CL 4.2 — type-of specification
- **難易度**: Medium

### FR-804: upgraded-array-element-type / upgraded-complex-part-type

- **対象**: `src/type/subtyping.lisp`
- **内容**: `upgraded-array-element-type`, `upgraded-complex-part-type` を実装
- **根拠**: ANSI CL 15.1.2, 12.1.5
- **難易度**: Medium

### FR-805: declare type 利用 (型宣言 → codegen 伝達)

- **対象**: `src/compile/codegen.lisp`, `src/compile/context.lisp`
- **内容**: `(declare (type fixnum x))` をコンパイル時に処理し codegen で fixnum 演算命令を直接選択
- **根拠**: SBCL の `(declare (type fixnum x))` → 直接 fixnum 演算
- **難易度**: Hard

### FR-806: the ランタイムアサーション

- **対象**: `src/compile/codegen-core.lisp`
- **内容**: safety ≥ 1 の場合、`(the fixnum expr)` にランタイム型チェックを挿入。safety = 0 では、静的に保証できる箇所のみ型チェック省略を許可する
- **根拠**: ANSI CL 3.4.4 — the special form semantics
- **難易度**: Medium

### FR-807: コンパイル時末尾位置追跡

- **対象**: `src/compile/codegen-core.lisp`
- **内容**: `ast-the`, `ast-catch`, `ast-block` 等で末尾位置フラグを正しく伝播させる
- **根拠**: 末尾位置の正確な追跡は TCO およびコード品質に影響
- **難易度**: Medium

### FR-808: values 型指定子

- **対象**: `src/type/parser.lisp`, `src/compile/codegen-core.lisp`
- **内容**: `(values fixnum string)` 型指定子のパース実装。`(function (fixnum) (values fixnum string))` 等の多値型記述をサポート
- **根拠**: ANSI CL 4.2.3 — values type specifier
- **難易度**: Hard

---

> 10-14 は拡張領域との境界にある参照セクションである。コア仕様としては依存関係の説明に留め、詳細な拡張要件は `type-advanced.md` 側で扱う。

## 10. 型クラスとトレイト

### FR-1001: 型クラス基盤 (Type Classes)

- **対象**: `src/type/typeclass.lisp`
- **内容**: Haskell 流の型クラス（アドホック多相の型安全な解決機構）
  - `(define-typeclass eq (:a) (= :a :a -> boolean) (/= :a :a -> boolean))`
  - **辞書渡し**: コンパイル後、型クラス制約は暗黙の辞書（レコード）引数として具体化
  - **インスタンス宣言**: `(define-instance eq integer (= (lambda (a b) (vm-eq a b))))`
  - **スーパークラス**: `(define-typeclass ord (:a) (:super eq) (< :a :a -> boolean) ...)`
  - **デフォルトメソッド**: インスタンスが実装を省略できるデフォルト定義
  - CLOS との違い: 型クラスは **開放世界**（任意の型に後からインスタンスを追加可能）
- **難易度**: Hard

### FR-1002: 多パラメータ型クラス (Multi-Parameter Type Classes)

- **対象**: `src/type/typeclass.lisp`
- **内容**: 複数の型変数にまたがる型クラス
  - `(define-typeclass coerce (:a :b) (coerce :a -> :b))` — `a` から `b` への変換
  - GHC `-XMultiParamTypeClasses`
  - **問題**: インスタンス選択が曖昧になりやすい → 関数従属性 (FR-1003) で解決
- **難易度**: Hard

### FR-1003: 関数従属性 (Functional Dependencies)

- **対象**: `src/type/typeclass.lisp`
- **内容**: 多パラメータ型クラスの型変数間の決定関係を宣言
  - `(define-typeclass collection (:c :e) (:fundep :c -> :e) (insert :e :c -> :c))`
  - `:c` が決まれば `:e` が一意に決まる → インスタンス解決の曖昧さを除去
  - GHC `-XFunctionalDependencies` (Jones 2000)
  - **代替**: 関連型 (Associated Types, FR-107) は関数従属性の型族による再表現
- **難易度**: Hard

### FR-1004: 型クラスのコヒーレンス (Typeclass Coherence)

- **対象**: `src/type/typeclass.lisp`
- **内容**: 同じ型に対して型クラスインスタンスが**世界に 1 つだけ**存在することを保証
  - **孤立インスタンス (Orphan Instances)**: 型もクラスも定義していないパッケージでのインスタンス宣言を禁止
  - **オーバーラッピングインスタンス**: GHC `-XOverlappingInstances` — より特殊化されたインスタンスを優先。コヒーレンス違反のリスクあり
  - Rust のコヒーレンス規則（孤立実装の禁止）と同等
  - **重要性**: コヒーレンスがなければ同じ型に対して異なる動作をする 2 つのインスタンスが共存できてしまう
- **難易度**: Hard

### FR-1005: deriving 機構 (Deriving Mechanism)

- **対象**: `src/expand/expander.lisp`
- **内容**: 型クラスインスタンスをデータ型の構造から自動導出
  - `(defstruct point (x 0) (y 0) (:deriving eq show ord))` — 構造から `eq`/`show`/`ord` を自動生成
  - **Generic deriving** (GHC `-XDeriveGeneric`): データ型の代数的構造を使って任意の型クラスを deriving
  - **策略的 deriving**: `(deriving via new-type instance-source)` — 既存インスタンスを転用
  - GHC `DerivingStrategies`、Scala `derives`
- **難易度**: Medium

### FR-1006: 型クラスのデフォルト規則 (Defaulting Rules)

- **対象**: `src/type/typeclass.lisp`
- **内容**: 型変数が未解決の場合にコンパイラが自動的に具体的型を選択
  - Haskell: `(show 42)` の `42` は `Num a => a` だが、デフォルト規則で `Integer` に解決
  - `(*default-numeric-type* 'fixnum)` — CL-CC での数値型デフォルト
  - **曖昧性エラー vs デフォルト**: デフォルト規則がない場合は型エラー
- **難易度**: Medium

---

## 11. 変性と型の互換性

### FR-1101: 変性アノテーション (Variance Annotations)

- **対象**: 予定ファイル `src/type/variance.lisp`
- **現状**: 未実装
- **内容**: 型コンストラクタの引数に変性（サブタイピングの方向）を付与
  - **共変 (Covariant)** `+`: `A <: B` ならば `F[A] <: F[B]`。読み取り専用コンテナ。`(covariant :a)` で宣言
  - **反変 (Contravariant)** `-`: `A <: B` ならば `F[B] <: F[A]`。関数の引数位置。`(contravariant :a)`
  - **不変 (Invariant)**: サブタイプ関係なし。読み書き両方あるコンテナ
  - **双変 (Bivariant)**: どちらの方向でも成立（危険。Java の配列の共変は型安全性を破る有名なバグ）
  - Scala の `+A`/`-A`、Kotlin の `out T`/`in T`、C# の `out`/`in`
  - **変性推論**: 明示的アノテーションなしに定義から変性を自動推論 (OCaml の方式)
- **難易度**: Hard

### FR-1102: 使用点変性 (Use-Site Variance / Wildcards)

- **対象**: 予定ファイル `src/type/variance.lisp`
- **現状**: 未実装
- **内容**: 型コンストラクタ定義時でなく使用時に変性を指定
  - Java の `? extends T` (上限ワイルドカード、共変的使用) / `? super T` (下限ワイルドカード、反変的使用)
  - Kotlin の `out T` (use-site) vs Scala の `+T` (declaration-site)
  - **トレードオフ**: 宣言点変性はより精密だが制約が強い。使用点変性は柔軟だが冗長
- **難易度**: Medium

### FR-1103: 変性推論 (Variance Inference)

- **対象**: 予定ファイル `src/type/variance.lisp`
- **現状**: 未実装
- **内容**: 型コンストラクタの定義から変性を自動計算
  - `(deftype-alias (my-list :a) (or null (cons :a (my-list :a))))` → `:a` は共変と自動推論
  - 関数型: 引数位置は反変、戻り値位置は共変
  - OCaml の変性推論アルゴリズム（型変数の positive/negative occurrence を追跡）
- **難易度**: Medium

---

## 12. 型の構造的拡張

### FR-1201: 多相ヴァリアント (Polymorphic Variants)

- **対象**: 予定ファイル `src/type/poly-variants.lisp`
- **現状**: 未実装
- **内容**: OCaml の多相ヴァリアント — 拡張可能な直和型
  - **開放ヴァリアント**: `` `Ok x | `Error e `` — 型名なしでタグを使用。型は自動推論
  - **閉鎖ヴァリアント**: `[> `A | `B]` (下限) / `[< `A | `B]` (上限) で許可タグを制限
  - **行多相との関係**: ヴァリアント型も行多相で表現可能（タグが「行」に相当）
  - **用途**: 関数が返すエラーの種類を型で正確に表現。ヴァリアントを後から追加可能
  - `(or (:ok :a) (:error string))` として CL-CC に対応表現あり
- **難易度**: Hard

### FR-1202: ゼロコスト抽象 (Newtype / Opaque Type Aliases)

- **対象**: `src/type/parser.lisp`
- **内容**: 実行時表現を変えずに型レベルで区別
  - **newtype**: `(newtype meters integer)` — `meters` は `integer` と同じ表現だが型は別
  - **opaque type alias**: Scala 3 の `opaque type Meters = Int` — モジュール外からは内部型が見えない
  - **コヒーレンス**: `(coerce (meters 5) 'integer)` のようなゼロコスト変換を型安全に実現
  - GHC の `Coercible` 型クラスと `coerce` — `newtype` 間のゼロコスト変換を型クラスで表現
  - **用途**: 単位系 `meters`/`feet` の混在防止、型安全な ID 型 `user-id`/`post-id` の区別
- **難易度**: Medium

### FR-1203: 再帰型 / μ型 (Recursive Types / μ-types)

- **対象**: `src/type/inference.lisp`
- **内容**: 自己参照する型の理論的基盤
  - **μ型**: `μX. 1 + A × X` — 「1 または A と X のペア」の不動点 = A のリスト型
  - **等帰納型 (Equirecursive)**: μX.T と T[X := μX.T] を同じ型として扱う。型チェッカーが自動展開
  - **同帰納型 (Isorecursive)**: `fold`/`unfold` 操作で明示的に巻き取り/展開。ML/Haskell の方式
  - **無限型**: 等帰納的解釈では `μX.X` のような無限型が存在できる（isorecursive では禁止）
  - CL-CC の `defstruct` / `defclass` による循環構造の型付けに直結
- **難易度**: Hard

### FR-1204: 余帰納型 (Coinductive Types / Codata)

- **対象**: 予定ファイル `src/type/coinductive.lisp`
- **現状**: 未実装
- **内容**: 潜在的に無限なデータ構造の型
  - **帰納型 (Inductive)**: 有限構造。`list` は空か `cons`。総称的再帰で「分解」
  - **余帰納型 (Coinductive)**: 無限構造。`stream` は無限に続く。`corecursion` で「生成」
  - `(codata stream (:a) (head :a) (tail (stream :a)))` — 無限ストリーム型
  - **生産性 (Productivity)**: 余帰納的定義が常に値を生産することをコンパイル時検証
  - Haskell の遅延リスト、Coq の `CoFixpoint`、Agda の `∞`/`♭`/`♯`
  - CPS 変換との関係: 継続は余帰納的計算の一形態
- **難易度**: Very Hard

### FR-1205: 商型 (Quotient Types)

- **対象**: 予定ファイル `src/type/quotient.lisp`
- **現状**: 未実装
- **内容**: 同値関係で割った型。`A / ~` — `A` の要素を `~` で同一視した型
  - **例**: 分数型 `(quotient (pair integer integer) rat-equiv)` — `1/2` と `2/4` を同一視
  - **集合論的型 (Set-Theoretic Types)**: 商型を含む型システムで型を数学的集合として解釈
  - 完全な商型は依存型が必要（Cubical Type Theory で自然に表現）
  - Haskell の `newtype` + `Coercible` は制限された商型の実用版
- **難易度**: Extremely Hard

---

## 13. 高度な多相性

> 注: この章は研究的拡張の参照領域である。core 実装の完了条件には含めない。

### FR-1301: カインド多相 (Kind Polymorphism)

- **対象**: `src/type/inference.lisp`
- **現状**: 基本構造のみ（`src/type/kind.lisp` の kind ノードはあるが、kind polymorphism は未実装）
- **内容**: 型コンストラクタのカインド自体を多相的に抽象化
  - GHC `-XPolyKinds`: `(forall k. f :: k -> *)` — カインド変数 `k` を導入
  - `Proxy :: forall k. k -> *` — 任意カインドの幽霊型引数を受け取る
  - **カインド推論**: 明示的カインドアノテーションなしにカインドを推論
  - `(type-var :f :* :-> :*)` から `(type-var :f :k :-> :*)` への一般化
  - GHC 9.x の `RequiredTypeArguments` — 型引数を実引数として明示渡し
- **難易度**: Very Hard

### FR-1302: 宇宙多相 (Universe Polymorphism)

- **対象**: 予定ファイル `src/type/universes.lisp`
- **現状**: 未実装
- **内容**: 型の型（宇宙）を階層化し、宇宙レベルを多相的に扱う
  - 動機: 「すべての型の型」は存在できない（Russell のパラドックス）
  - `Type₀ : Type₁ : Type₂ : ...` — 宇宙の無限階層
  - **宇宙多相**: `(forall u. Type u -> Type u)` — 宇宙レベル `u` を変数として抽象化
  - Lean 4 の `universe u` / `Sort u`、Agda の `Set l`、Coq の `Type@{u}`
  - **宇宙累積性 (Cumulativity)**: `Type₀ : Type₁` なら `Type₀ <: Type₁` が成立するか否か
- **難易度**: Extremely Hard

### FR-1303: 表現多相 / 浮遊性多相 (Representation Polymorphism / Levity Polymorphism)

- **対象**: `src/compile/codegen.lisp`
- **内容**: 値の実行時表現（boxed/unboxed、lifted/unlifted）を型パラメータとして抽象化
  - GHC の `RuntimeRep` カインド: `LiftedRep`, `UnliftedRep`, `IntRep`, `FloatRep`, `TupleRep` 等
  - `(forall (r : RuntimeRep). TYPE r -> TYPE r)` — 表現に依存しない多相関数
  - **制約**: 表現多相関数は `RuntimeRep` が不明な型変数をラムダで束縛できない（表現不明の値をスタックに積めないため）
  - CL-CC 応用: fixnum（unboxed）と heap オブジェクト（boxed）を統一して扱う多相関数の型付け
- **難易度**: Very Hard

### FR-1304: 非述語的多相 (Impredicative Polymorphism)

- **対象**: `src/type/inference.lisp`
- **内容**: 多相型をそのまま型変数に代入できる型システム（HM の Rank-1 制限を撤廃）
  - HM は述語的 (predicative): 型変数 `:a` には単相型しか代入できない
  - 非述語的: `:a` に `(forall :b :b -> :b)` を直接代入可能
  - `(list (forall :a :a -> :a))` — 多相関数のリスト（HM では表現不可）
  - GHC `-XImpredicativeTypes` (GHC 9.2+ で改善)。型推論が非常に困難
  - **Quick Look** (GHC 9.2): ヒューリスティックで非述語的型推論を実用化
- **難易度**: Extremely Hard

### FR-1305: 総称的関連型 (Generic Associated Types, GATs)

- **対象**: `src/type/typeclass.lisp`
- **内容**: 関連型にライフタイムや型パラメータを付与（Rust 1.65+ の目玉機能）
  - 通常の関連型: `(type-class container (:c) (type item-of :c))` — パラメータなし
  - GAT: `(type-class iterable (:c) (type iter-of (:c :lifetime)))` — ライフタイム付き関連型
  - **用途**: `LendingIterator` — 各要素がコンテナへの参照を返すイテレータ（通常の関連型では表現不可）
  - Rust RFC 1598。Haskell の型族 + HKT でも表現可能
- **難易度**: Very Hard

### FR-1306: 型ラムダ (Type Lambdas)

- **対象**: `src/type/parser.lisp`
- **内容**: 型レベルのラムダ式 — 型コンストラクタを匿名で定義
  - Scala 3: `[A] =>> Either[String, A]` — `String` で部分適用した `Either`
  - Haskell の型シノニム: `type StringEither a = Either String a`（匿名版が型ラムダ）
  - **部分適用**: `(type-lambda (:a) (or string :a))` — `Either String` 相当の型コンストラクタ
  - `(map (type-lambda (:a) (list :a)) types)` — 型のリストに型関数を適用
  - HKT (FR-101) と組み合わせて使う場面が多い
- **難易度**: Hard

### FR-1307: System F (第2階型システム)

- **対象**: `src/type/inference.lisp`
- **内容**: Girard (1972) / Reynolds (1974) の多相ラムダ計算。HM の理論的基盤
  - **Λ抽象 (型抽象)**: `(Λ :a. λx:a. x)` — 型変数 `:a` を引数として受け取るラムダ
  - **型適用**: `(id [integer] 42)` — 多相関数に型引数を明示渡し
  - HM との関係: HM は System F の「型推論可能な」部分集合（Damas-Milner の Rank-1 制限）
  - **System Fω**: 型コンストラクタを高カインドに一般化したもの（Haskell の理論的基盤）
  - CPS 変換との関係: CPS 変換後のコードは自然に System F の型を持つ
- **難易度**: Very Hard (理論実装) / Medium (理論理解)

---

## 14. 型理論の基礎

> 注: この章は理論的背景の整理であり、core の直接的な実装要件ではない。

### FR-1401: 文脈的型付け / 暗黙引数 (Contextual Typing / Implicit Arguments)

- **対象**: `src/type/inference.lisp`
- **内容**: 型クラスインスタンスや証明を自動合成して暗黙に渡す
  - Scala 3 の `given`/`using`: `def sort[A](xs: List[A])(using ord: Ordering[A])` — `ord` は自動解決
  - Coq / Lean の `[inst : TypeClass]` — 型クラスインスタンスを暗黙引数として解決
  - Haskell の型クラス制約 `(Ord a) =>` の辞書渡し実装
  - **スコープ付き暗黙解決**: 特定スコープ内でのみ有効なインスタンスを注入
  - CL の `*print-readably*` 等の動的変数は弱い形の文脈的型付け
- **難易度**: Hard

### FR-1402: 主型性と決定可能性 (Principal Types & Decidability)

- **対象**: `src/type/inference.lisp`
- **内容**: 型推論アルゴリズムの理論的保証
  - **主型性 (Principal Types)**: すべての型付けの中で「最も一般的な型」が一意に存在すること。HM は主型性を持つ
  - **健全性 (Soundness)**: 型検査が通る範囲について、仕様で保証された実行時型エラーを抑制すること
  - **完全性 (Completeness)**: 型付け可能なプログラムはすべて型検査が通ること
  - **決定可能性**: 型推論が有限時間で停止すること。System F は決定不可能。HM は決定可能
  - **段階的型付けの健全性**: 段階的型付けでは完全な健全性ではなく「型付き部分の健全性」を保証
  - Milner (1978) の主定理: Algorithm W は主型スキームを返す
- **難易度**: Medium (理論把握) / Hard (実装への適用)

### FR-1403: パラメトリシティと自由定理 (Parametricity & Free Theorems)

- **対象**: 理論的基盤（実装は最適化に応用）
- **内容**: Reynolds (1983) のパラメトリシティ — 多相型から自動的に定理が導出される
  - `(forall :a :a -> :a)` の実装は `identity` のみ（型を検査できないので）
  - `(forall :a (list :a) -> (list :a))` の関数は各要素を変換できない（自由定理）
  - **最適化応用**: 自由定理から等式変換規則を自動導出 → `map f . map g = map (f . g)` 等
  - **セキュリティ応用**: パラメトリシティがあれば型変数の内部を覗けない → 情報隠蔽の理論的保証
  - Wadler (1989). Theorems for free!
- **難易度**: Very Hard (最適化への応用)

### FR-1404: 集合論的型システム (Set-Theoretic / Semantic Subtyping)

- **対象**: `src/type/subtyping.lisp`
- **内容**: 型を値の集合として解釈し、集合演算で型の関係を定義
  - CDuce (Benzaken et al. 2003): XML 処理言語。型 = 値の集合、サブタイプ = 集合の包含
  - `(and integer (not zero))` = 0 以外の整数の集合（正確な complement を持つ）
  - **決定可能なサブタイプ検査**: 集合論的モデルにより `subtypep` を完全に決定可能に
  - Elixir の型システム (2025): set-theoretic types を採用
  - ANSI CL の `and`/`or`/`not`/`satisfies` は集合論的型の近似として解釈可能
- **難易度**: Very Hard

### FR-1405: 極性型理論 (Polarized Type Theory)

- **対象**: 理論的基盤
- **内容**: 型を「正 (positive)」と「負 (negative)」に分類して計算の方向性を型で表現
  - **正の型**: データ（値）。積型、和型。「生産」される
  - **負の型**: 計算。関数型、継続型。「消費」する
  - **フォーカシング (Focusing)**: 証明探索の効率化手法。正規化可能な証明の部分集合を選択
  - CPS との関係: CPS 変換は正の型を負の型（継続）に変換する操作として解釈可能
  - Zeilberger (2009). Focusing and Higher-Order Abstract Syntax.
- **難易度**: Extremely Hard

### FR-1406: 定義的等価性 vs 命題的等価性 (Definitional vs Propositional Equality)

- **対象**: 予定ファイル `src/type/dependent.lisp` (FR-303 の依存型実装に必要)
- **現状**: 未実装
- **内容**: 依存型における型等値の 2 種類
  - **定義的等価性 (Definitional)**: コンパイラが自動的に証明できる等価性。`2 + 2` と `4` は定義的に等価
  - **命題的等価性 (Propositional)**: 証明が必要な等価性。`n + 0 = n` は定義的ではなく命題的（帰納法が必要）
  - **一価性公理 (Univalence Axiom)**: HoTT。同型な型は等価（`A ≃ B → A = B`）
  - Lean 4 の `rfl`（定義的等価）vs `Eq.symm`（命題的等価の証明操作）
- **難易度**: Extremely Hard

---
