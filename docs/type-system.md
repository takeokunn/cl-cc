# Type System — Modern Compiler Features

2026年時点のモダンコンパイラ（GHC 9.10 / OCaml 5.x / Lean 4 / Idris 2 / Rust 2024 / TypeScript 5.x / Scala 3）に実装されている型システム機能の全体像と、CL-CC での実装計画。

---

## 目次

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

## 1. 型推論エンジン

### FR-001: Hindley-Milner 型推論 (基盤)

- **対象**: `src/type/inference.lisp`
- **現状**: HM 型推論は独立動作。`infer-type` が S 式から型スキームを返す
- **アルゴリズム**: Algorithm W (Damas & Milner 1982) + unification (Robinson 1965)
- **スコープ**: let-polymorphism, 型変数の全称量化, unification による型変数束縛
- **参考実装**: OCaml, SML/NJ, GHC Core の型推論基盤

### FR-002: 双方向型検査 (Bidirectional Type Checking)

- **対象**: `src/type/inference.lisp`
- **現状**: synthesis モードのみ。check モード（文脈から下方向に型を伝播）がない
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
- **内容**: `compile-ast` に型環境 `type-env` 引数を追加。`infer-type` の結果を AST ノードにアノテートし codegen で活用
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
  - `subtypep` の完全実装（FR-482 参照）

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

- **対象**: 新ファイル `src/type/dependent.lisp`
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

- **対象**: 新ファイル `src/type/effects.lisp`
- **内容**: 関数の副作用を型に記録する
  - **エフェクトの種類**: `:io` (I/O), `:state` (変更可能状態), `:exception` (例外), `:nondeterminism` (非決定性), `:divergence` (非停止)
  - 関数型: `(function ((integer) (integer)) integer ! (:io :state))` — I/O と状態変更を持つ関数
  - **純粋関数**: エフェクトセットが空 `{}` の関数はメモ化・並列化が安全
  - **エフェクト多態性**: `(function (:a) :b ! :e)` — エフェクトを型変数 `:e` で抽象化
  - 参考実装: Koka (Microsoft Research), Frank, Eff, Helium
- **難易度**: Very Hard

### FR-402: 代数的エフェクトとハンドラ (Algebraic Effects & Handlers)

- **対象**: 新ファイル `src/type/effects.lisp`, `src/vm/effects.lisp`
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

- **対象**: 新ファイル `src/type/coeffects.lisp`
- **内容**: 関数が**必要とするコンテキスト**を型で追跡（エフェクトの双対）
  - **リソース使用量**: `{uses: 2}` — 引数を 2 回使用する（線形型の一般化）
  - **データフロー**: どの入力変数がどの出力に影響するか（セキュリティ解析に応用）
  - **暗黙的パラメータ**: Haskell の `ImplicitParams` を係数として型付け
  - Petricek et al. (2014) の coeffect system
- **難易度**: Very Hard

---

## 6. リソース管理型

### FR-501: 線形型 / アフィン型 (Linear / Affine Types)

- **対象**: `src/type/inference.lisp`, `src/type/parser.lisp`
- **内容**:
  - **線形型**: 値をちょうど 1 回使用することを型で保証 (Wadler 1990)
  - **アフィン型**: 高々 1 回使用（Rust の move semantics に相当）
  - `(declare (affine stream))` で `stream` が 1 パス内で `close` されることをコンパイル時検証
  - CPS 形式では継続の線形使用（delimited continuation の 1 回実行保証）と連携
  - Linear Haskell (POPL 2018 / GHC `-XLinearTypes`)
- **難易度**: Very Hard

### FR-502: 所有権型 / 借用検査 (Ownership Types & Borrow Checker)

- **対象**: 新ファイル `src/type/ownership.lisp`
- **内容**: Rust のオーナーシップシステムを型で表現
  - **オーナーシップ移転**: `(move x)` でオーナーシップを消費。以降 `x` は型エラー
  - **共有借用**: `&x` — 読み取り専用参照。複数同時可
  - **排他借用**: `&mut x` — 書き込み可能参照。同時に 1 つのみ
  - **ライフタイム**: 参照が指す値より長生きしないことを型で保証
  - Rust 2024 edition の Non-Lexical Lifetimes (NLL) + Polonius
- **難易度**: Extremely Hard

### FR-503: 一意型 (Uniqueness Types)

- **対象**: 新ファイル `src/type/uniqueness.lisp`
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

- **対象**: 新ファイル `src/type/sessions.lisp`
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

- **対象**: 新ファイル `src/type/modules.lisp`
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
- **内容**: safety ≥ 1 の場合、`(the fixnum expr)` にランタイム型チェックを挿入。safety = 0 では無条件信頼
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

## 10. 型クラスとトレイト

### FR-1001: 型クラス基盤 (Type Classes)

- **対象**: 新ファイル `src/type/typeclasses.lisp`
- **内容**: Haskell 流の型クラス（アドホック多相の型安全な解決機構）
  - `(define-typeclass eq (:a) (= :a :a -> boolean) (/= :a :a -> boolean))`
  - **辞書渡し**: コンパイル後、型クラス制約は暗黙の辞書（レコード）引数として具体化
  - **インスタンス宣言**: `(define-instance eq integer (= (lambda (a b) (vm-eq a b))))`
  - **スーパークラス**: `(define-typeclass ord (:a) (:super eq) (< :a :a -> boolean) ...)`
  - **デフォルトメソッド**: インスタンスが実装を省略できるデフォルト定義
  - CLOS との違い: 型クラスは **開放世界**（任意の型に後からインスタンスを追加可能）
- **難易度**: Hard

### FR-1002: 多パラメータ型クラス (Multi-Parameter Type Classes)

- **対象**: `src/type/typeclasses.lisp`
- **内容**: 複数の型変数にまたがる型クラス
  - `(define-typeclass coerce (:a :b) (coerce :a -> :b))` — `a` から `b` への変換
  - GHC `-XMultiParamTypeClasses`
  - **問題**: インスタンス選択が曖昧になりやすい → 関数従属性 (FR-1003) で解決
- **難易度**: Hard

### FR-1003: 関数従属性 (Functional Dependencies)

- **対象**: `src/type/typeclasses.lisp`
- **内容**: 多パラメータ型クラスの型変数間の決定関係を宣言
  - `(define-typeclass collection (:c :e) (:fundep :c -> :e) (insert :e :c -> :c))`
  - `:c` が決まれば `:e` が一意に決まる → インスタンス解決の曖昧さを除去
  - GHC `-XFunctionalDependencies` (Jones 2000)
  - **代替**: 関連型 (Associated Types, FR-107) は関数従属性の型族による再表現
- **難易度**: Hard

### FR-1004: 型クラスのコヒーレンス (Typeclass Coherence)

- **対象**: `src/type/typeclasses.lisp`
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

- **対象**: `src/type/typeclasses.lisp`
- **内容**: 型変数が未解決の場合にコンパイラが自動的に具体的型を選択
  - Haskell: `(show 42)` の `42` は `Num a => a` だが、デフォルト規則で `Integer` に解決
  - `(*default-numeric-type* 'fixnum)` — CL-CC での数値型デフォルト
  - **曖昧性エラー vs デフォルト**: デフォルト規則がない場合は型エラー
- **難易度**: Medium

---

## 11. 変性と型の互換性

### FR-1101: 変性アノテーション (Variance Annotations)

- **対象**: 新ファイル `src/type/variance.lisp`
- **内容**: 型コンストラクタの引数に変性（サブタイピングの方向）を付与
  - **共変 (Covariant)** `+`: `A <: B` ならば `F[A] <: F[B]`。読み取り専用コンテナ。`(covariant :a)` で宣言
  - **反変 (Contravariant)** `-`: `A <: B` ならば `F[B] <: F[A]`。関数の引数位置。`(contravariant :a)`
  - **不変 (Invariant)**: サブタイプ関係なし。読み書き両方あるコンテナ
  - **双変 (Bivariant)**: どちらの方向でも成立（危険。Java の配列の共変は型安全性を破る有名なバグ）
  - Scala の `+A`/`-A`、Kotlin の `out T`/`in T`、C# の `out`/`in`
  - **変性推論**: 明示的アノテーションなしに定義から変性を自動推論 (OCaml の方式)
- **難易度**: Hard

### FR-1102: 使用点変性 (Use-Site Variance / Wildcards)

- **対象**: `src/type/variance.lisp`
- **内容**: 型コンストラクタ定義時でなく使用時に変性を指定
  - Java の `? extends T` (上限ワイルドカード、共変的使用) / `? super T` (下限ワイルドカード、反変的使用)
  - Kotlin の `out T` (use-site) vs Scala の `+T` (declaration-site)
  - **トレードオフ**: 宣言点変性はより精密だが制約が強い。使用点変性は柔軟だが冗長
- **難易度**: Medium

### FR-1103: 変性推論 (Variance Inference)

- **対象**: `src/type/variance.lisp`
- **内容**: 型コンストラクタの定義から変性を自動計算
  - `(deftype-alias (my-list :a) (or null (cons :a (my-list :a))))` → `:a` は共変と自動推論
  - 関数型: 引数位置は反変、戻り値位置は共変
  - OCaml の変性推論アルゴリズム（型変数の positive/negative occurrence を追跡）
- **難易度**: Medium

---

## 12. 型の構造的拡張

### FR-1201: 多相ヴァリアント (Polymorphic Variants)

- **対象**: 新ファイル `src/type/poly-variants.lisp`
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

- **対象**: 新ファイル `src/type/coinductive.lisp`
- **内容**: 潜在的に無限なデータ構造の型
  - **帰納型 (Inductive)**: 有限構造。`list` は空か `cons`。総称的再帰で「分解」
  - **余帰納型 (Coinductive)**: 無限構造。`stream` は無限に続く。`corecursion` で「生成」
  - `(codata stream (:a) (head :a) (tail (stream :a)))` — 無限ストリーム型
  - **生産性 (Productivity)**: 余帰納的定義が常に値を生産することをコンパイル時検証
  - Haskell の遅延リスト、Coq の `CoFixpoint`、Agda の `∞`/`♭`/`♯`
  - CPS 変換との関係: 継続は余帰納的計算の一形態
- **難易度**: Very Hard

### FR-1205: 商型 (Quotient Types)

- **対象**: 新ファイル `src/type/quotient.lisp`
- **内容**: 同値関係で割った型。`A / ~` — `A` の要素を `~` で同一視した型
  - **例**: 分数型 `(quotient (pair integer integer) rat-equiv)` — `1/2` と `2/4` を同一視
  - **集合論的型 (Set-Theoretic Types)**: 商型を含む型システムで型を数学的集合として解釈
  - 完全な商型は依存型が必要（Cubical Type Theory で自然に表現）
  - Haskell の `newtype` + `Coercible` は制限された商型の実用版
- **難易度**: Extremely Hard

---

## 13. 高度な多相性

### FR-1301: カインド多相 (Kind Polymorphism)

- **対象**: `src/type/inference.lisp`
- **内容**: 型コンストラクタのカインド自体を多相的に抽象化
  - GHC `-XPolyKinds`: `(forall k. f :: k -> *)` — カインド変数 `k` を導入
  - `Proxy :: forall k. k -> *` — 任意カインドの幽霊型引数を受け取る
  - **カインド推論**: 明示的カインドアノテーションなしにカインドを推論
  - `(type-var :f :* :-> :*)` から `(type-var :f :k :-> :*)` への一般化
  - GHC 9.x の `RequiredTypeArguments` — 型引数を実引数として明示渡し
- **難易度**: Very Hard

### FR-1302: 宇宙多相 (Universe Polymorphism)

- **対象**: 新ファイル `src/type/universes.lisp`
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

- **対象**: `src/type/typeclasses.lisp`
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
  - **健全性 (Soundness)**: 型検査が通れば実行時型エラーが起きないこと
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

- **対象**: `src/type/dependent.lisp` (FR-303 の依存型実装に必要)
- **内容**: 依存型における型等値の 2 種類
  - **定義的等価性 (Definitional)**: コンパイラが自動的に証明できる等価性。`2 + 2` と `4` は定義的に等価
  - **命題的等価性 (Propositional)**: 証明が必要な等価性。`n + 0 = n` は定義的ではなく命題的（帰納法が必要）
  - **一価性公理 (Univalence Axiom)**: HoTT。同型な型は等価（`A ≃ B → A = B`）
  - Lean 4 の `rfl`（定義的等価）vs `Eq.symm`（命題的等価の証明操作）
- **難易度**: Extremely Hard

---

## 15. 安全性指向型

### FR-1501: Null 安全 / Optional 型 (Null Safety)

- **対象**: `src/type/inference.lisp`, `src/vm/primitives.lisp`
- **内容**: `nil` / `null` を型システムに組み込んで Null 参照エラーを静的に防ぐ
  - Kotlin: `String`（非null）と `String?`（null 許容）を型レベルで区別
  - Swift: `Optional<T>` = `T?`。`if let` / `guard let` で安全にアンラップ
  - Haskell: `Maybe a` モナド — `Nothing`（null相当）を明示的に型で表現
  - CL の `nil` は多義的（空リスト・false・null）→ `(or null T)` として明示型付け
  - **`!` 演算子**: Kotlin の `!!` — プログラマが null でないと保証する型アサーション（型エラーを実行時エラーに変換）
- **難易度**: Medium

### FR-1502: 並行安全型 (Concurrency Safety Types)

- **対象**: 新ファイル `src/type/concurrency.lisp`
- **内容**: スレッド間のデータ共有の安全性を型で保証
  - Rust の `Send` / `Sync` マーカートレイト:
    - `Send`: 値をスレッド間で移動できる（オーナーシップ転送が安全）
    - `Sync`: 値への参照をスレッド間で共有できる（`&T` が `Send` ならば `T: Sync`）
  - `(declare (not-send mutex-guard))` — `MutexGuard` はスレッド間移動不可
  - Java の `@NotThreadSafe` アノテーションを型レベルに昇格させた形
  - **データ競合の静的防止**: `Send`/`Sync` がなければ `spawn` に渡せない → データ競合がコンパイルエラー
- **難易度**: Hard

### FR-1503: 情報流型 / Taint 解析 (Information Flow Types)

- **対象**: 新ファイル `src/type/security.lisp`
- **内容**: データの機密レベルを型で追跡し、情報漏洩を静的に防ぐ
  - **セキュリティラベル**: `(secret integer)` — 秘密の整数。`(public integer)` — 公開の整数
  - **不変条件**: `secret` な値は `public` な文脈で使えない（「高い情報が低い情報に流れない」）
  - **明示的な情報流**: `(declassify x reason)` — 意図的な機密解除。型に証跡が残る
  - DIF (Dependent Information Flow)、Jif (Java + Information Flow)
  - CL-CC 応用: ユーザー入力をすべて `(tainted string)` として型付け。SQL クエリに渡す前にサニタイズ必須
- **難易度**: Very Hard

### FR-1504: 領域型 (Region Types)

- **対象**: 新ファイル `src/type/regions.lisp`
- **内容**: メモリ領域を型で表現し、GC なしで安全なメモリ解放を実現
  - Tofte & Talpin (1994) の領域推論: プログラムを解析して各値の「生存領域」を推論
  - `(region r (let ((x (alloc r 42))) ...))` — 領域 `r` のライフタイム内のみ `x` が有効
  - **領域多相**: `(forall r. (function ((list-in r :a)) :b))` — 任意の領域のリストを受け取る
  - ML Kit (SML の領域ベース実装)、Rust のライフタイムは領域型の実用版
  - **GC との比較**: 領域型は GC の完全な代替にはなれないが、短命なオブジェクトの割り当て/解放を O(1) で実現
- **難易度**: Very Hard

### FR-1505: 能力型 (Capability Types)

- **対象**: 新ファイル `src/type/capabilities.lisp`
- **内容**: 操作の「権限」を型で表現するオブジェクト能力モデル
  - `(capability :file-write)` を持つ関数のみがファイルに書き込める
  - **能力の委譲**: 能力を関数の引数として明示的に渡す（グローバルな権限チェックではなく局所的）
  - **能力の制限**: `(restrict-capability cap :read-only)` — 書き込み能力を除去した能力を派生
  - Joe-E (Java + object capabilities)、Wyvern、Pony (capabilities + reference capabilities)
  - エフェクト型 (FR-401) との関係: 能力型はエフェクトを「どのオブジェクトが持つか」で表現したもの
- **難易度**: Very Hard

---

## 16. 開発支援型機能

### FR-1601: 型付きホール (Typed Holes)

- **対象**: `src/compile/codegen.lisp`, REPL (`src/cli/main.lisp`)
- **内容**: 未実装部分を `_` (hole) で表し、型エラーとして期待型を報告
  - GHC: `_ :: Int` → `Found hole: _ :: Int`。コンテキスト内の使用可能な変数も列挙
  - Idris 2: インタラクティブに hole を埋めるプログラミングスタイル（定理証明補助と同等）
  - `(defun foo (x) (+ x _))` → コンパイルエラー: `hole at position 2: expected type integer, available: x :: integer`
  - **型駆動開発 (Type-Driven Development)**: 型を先に書き、hole を段階的に埋める手法
  - CL-CC の REPL に `?` / `_` hole 構文を追加
- **難易度**: Medium

### FR-1602: データ型ジェネリクス (Datatype-Generic Programming)

- **対象**: 新ファイル `src/type/generics.lisp`
- **内容**: データ型の代数的構造（積・和）を使って汎用的な操作を定義
  - **GHC Generics**: `class Generic a where type Rep a :: * -> *` — 型をその「表現型」に変換
  - **表現型**: `U1`（単位）、`K1`（定数）、`M1`（メタ情報）、`:*:`（積）、`:+:`（和）
  - **汎用関数**: `(generic-show x)` — 任意の `Generic` インスタンスを文字列化
  - **Scrap Your Boilerplate (SYB)**: 型を横断する汎用的な変換・クエリ操作
  - CL の `mop:class-slots` + `slot-value` による反射的操作の型安全版として実装可能
- **難易度**: Hard

### FR-1603: エフェクト推論 (Effect Inference)

- **対象**: `src/type/effects.lisp` (FR-401 の拡張)
- **内容**: プログラマがエフェクトアノテーションを書かなくても、コンパイラが自動推論
  - `(defun foo (x) (print x) (* x 2))` → コンパイラが `! (:io)` を自動推論
  - **区間推論 (Interval Inference)**: 最小のエフェクトセットを下限として推論
  - **エフェクトの単純化**: `(:io :io)` → `(:io)`。エフェクトセットの正規化
  - Koka の自動エフェクト推論（人気の理由の一つ）
  - HM 型推論と並行して実行可能（エフェクト変数を型変数と同様に unification で解く）
- **難易度**: Hard

### FR-1604: 値制限 (Value Restriction)

- **対象**: `src/type/inference.lisp`
- **内容**: 変更可能な参照と多相型の相互作用による健全性破壊を防ぐ
  - **問題**: `(let ((r (ref nil))) ...)` — `r` が `(ref (forall :a :a))` と推論されると型安全性が破れる
  - **ML の値制限**: 変数宣言の右辺が「値」（副作用のない式）でない場合、多相化しない
  - `(setq x (make-list 10))` — `x` は `(list :a)` でなく `(list t)` に単相化
  - CL-CC: `defvar` / `setq` への代入が型多相性をどう制限するかのポリシー定義
  - Garrigue (2004). Relaxing the value restriction.
- **難易度**: Medium

### FR-1605: 型エラーの品質 (Type Error Quality)

- **対象**: `src/type/inference.lisp`
- **内容**: 型エラーメッセージを人間が理解しやすくする
  - **エラーの局所化**: HM の unification エラーは発生箇所と根本原因が離れることが多い。正確なエラー位置を特定
  - **型エラーの説明**: 「なぜこの型を期待したか」の連鎖を表示
  - **候補の提示**: 型が合わない場合に「こうすれば型が合う」候補を列挙
  - **Elm のアプローチ**: コンパイラがエラーを「診断」として丁寧に説明（2015年から業界標準に）
  - GHC の `-fdiagnostics-as-json` (GHC 9.4+): 構造化エラー情報
  - Rust のエラーコード (`E0308` 等) + 詳細説明 (`rustc --explain E0308`)
- **難易度**: Medium

### FR-1606: インクリメンタル型検査 (Incremental Type Checking)

- **対象**: `src/type/inference.lisp`
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

- **対象**: `src/type/parser.lisp`
- **内容**: コンパイル時に自然数を型として扱い、値の長さや次元を型に埋め込む
  - GHC の `GHC.TypeNats`: `(KnownNat n) =>` で実行時に `natVal` として取り出し可能
  - `(vector 3 integer)` — 長さ 3 の整数ベクタ。長さが型に現れる
  - 型レベル算術: `(type-plus 2 3) = 5`, `(type-mul m n)` — 型レベルで計算
  - **アプリケーション**: 行列型 `(matrix m n float)` — `(matrix-mul (matrix m k float) (matrix k n float))` が型安全
  - Idris 2 の `Vect n a`, Agda の `Vec A n`
- **難易度**: Hard

### FR-1702: 型レベル文字列 (Type-Level Strings / Symbol Kind)

- **対象**: `src/type/parser.lisp`
- **内容**: 文字列リテラルを型として使用
  - GHC の `Symbol` カインド: `"foo" :: Symbol`
  - `(has-field "name" string)` — `"name"` という名前のフィールドを持つレコード型
  - **型安全レコードアクセス**: `(get-field "name" record)` が `"name"` フィールドを持たない型にアクセスするとコンパイルエラー
  - Haskell の `vinyl` / `data-has` ライブラリで活用
  - TypeScript の template literal types: `` `on${Capitalize<string>}` ``
- **難易度**: Hard

### FR-1703: 多段階プログラミング (Multi-Stage Programming)

- **対象**: 新ファイル `src/type/staging.lisp`
- **内容**: プログラムの「実行段階」を型で区別し、コンパイル時計算と実行時計算を型安全に混在
  - **ステージ 0**: 実行時の値。通常の式
  - **ステージ 1 (Code)**: コンパイル時に生成されるコードの型 `(code integer)` — MetaML の `<| expr |>`
  - **splice**: `(splice e)` — `(code t)` の値を実行時に展開 (`~e` に相当)
  - **run**: `(run code-expr)` — コードを実行してステージを下げる
  - MetaML (Taha & Sheard 1997)、BER MetaOCaml、Haskell の Template Haskell (`Q` モナド)
  - CL-CC 応用: マクロ展開の型安全版として実装可能。`read-time-eval` の型付き代替
- **難易度**: Very Hard

### FR-1704: 型安全メタプログラミング (Typed Metaprogramming)

- **対象**: `src/expand/expander.lisp`
- **内容**: マクロ生成コードに型安全性を保証
  - Template Haskell: `Q (TExp a)` — 型 `a` のコードを生成するアクション。生成コードが型検査を通る保証
  - **Typed Template Haskell**: `[|| expr ||]` で typed splice。型が一致しない splice はコンパイルエラー
  - CL の `defmacro` との違い: 通常の CL マクロは S 式を返すため型安全性なし
  - **型付き準引用 (Typed Quasiquotation)**: `` `(+ ,x 1) `` で `x: integer` が保証されるなら生成コードも `integer`
  - CL-CC 応用: `define-vm-instruction` マクロの生成コードに型情報を付与
- **難易度**: Very Hard

### FR-1705: バインディング時解析 (Binding-Time Analysis)

- **対象**: 新ファイル `src/type/binding-time.lisp`
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

- **対象**: 新ファイル `src/type/optics.lisp`
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

- **対象**: `src/type/typeclasses.lisp`
- **内容**: 任意のファンクタからモナドを構築する汎用的な構造
  - `(deftype (free :f :a) (or (:pure :a) (:free (:f (free :f :a)))))` — 自由モナドの定義
  - **インタープリタパターン**: DSL をデータとして表現し、インタープリタで意味を与える
  - `(deftype io-op (or (:read-line) (:print-line string) ...))` + `(free io-op :a)` — I/O を純粋にモデル化
  - Freer Monad (Kiselyov & Ishii 2015): 型クラス制約なしに代数的エフェクトを実装
  - エフェクトシステム (FR-401/402) の代替実装経路として CL-CC に適用可能
- **難易度**: Hard

### FR-1803: 異種リスト (Heterogeneous Lists / HLists)

- **対象**: `src/type/typeclasses.lisp`
- **内容**: 要素ごとに異なる型を持つ型安全リスト
  - `(hlist integer string boolean)` — 要素数と各要素の型が型に現れる
  - **HList の操作**: `hhead :: HList (a:as) -> a`, `htail :: HList (a:as) -> HList as`
  - Kiselyov et al. (2004). Strongly typed heterogeneous collections.
  - **型安全タプル**: `(hlist a b c)` は `(values a b c)` の型安全版
  - **型安全レコード**: フィールド名とフィールド型のペアのHList = 型安全レコード
  - CL-CC 応用: `multiple-values` の型付けに応用可能
- **難易度**: Hard

### FR-1804: 型安全 printf (Type-Safe printf)

- **対象**: `src/vm/io.lisp`
- **内容**: フォーマット文字列から引数の型を型レベルで導出
  - Haskell の `printf` trick: `"%d %s"` から `(function (integer string) string)` を型推論
  - `(format-type "%d %s %f")` → `(function (integer string float) string)`
  - 型レベル文字列 (FR-1702) + 型族 (FR-107) で実装可能
  - CL の `format` は実行時にフォーマット文字列を解析。型安全版では `:format` フォーマット文字列を型レベルで処理
  - **Typed Format Strings**: Rust の `println!("{}", x)` — コンパイル時にフォーマット文字列を解析
- **難易度**: Very Hard

### FR-1805: Profunctor / Traversal の型

- **対象**: `src/type/typeclasses.lisp`
- **内容**: Lens の一般化。入力と出力に異なる変性を持つ型構成子
  - `(type-class profunctor (:p) (dimap (b -> a) (c -> d) (p a c) -> (p b d)))` — 入力に反変、出力に共変
  - **Optics の統一的定義**: すべての Optics (Lens/Prism/Traversal/Fold) を Profunctor の制約で統一記述
  - `(type lens :s :t :a :b (forall p. strong p => p a b -> p s t))` — Strong Profunctor で Lens を定義
  - Riley (2018). Categories of Optics. — Optics の圏論的統一理論
  - Haskell の `profunctors` パッケージ
- **難易度**: Very Hard

### FR-1806: 継続型 / CPS の型 (Continuation Types)

- **対象**: `src/compile/cps.lisp`
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

- **対象**: `src/type/inference.lisp`
- **内容**: すべての再帰関数が停止することをコンパイル時に検証
  - **構造的再帰**: 再帰呼び出しの引数が常に「より小さい」部分構造であることをチェック
  - `(defun length (xs) (if (null xs) 0 (1+ (length (cdr xs)))))` — `cdr` で常に縮小 → 停止
  - **辞書式順序**: 複数引数の辞書式順序での縮小を検査
  - **サイズ変化グラフ**: すべての呼び出しパスでサイズが減少するか確認 (Sipser 1996)
  - Agda / Idris 2 の停止性検査器
  - **非停止関数のマーキング**: 停止性が証明できない関数を `(partial defun ...)` として明示
- **難易度**: Hard

### FR-1902: 全域性検査 (Totality Checking)

- **対象**: `src/compile/codegen.lisp`
- **内容**: すべての関数が任意の入力に対して値を返すことを検証（例外・無限ループなし）
  - 全域 = **停止性** (FR-1901) + **網羅性** (FR-1903)
  - **全域関数の利点**: メモ化・並列化・等式変換が安全
  - **部分関数の明示**: `(partial integer -> integer)` — 失敗する可能性のある関数型
  - Idris 2 は全域性を言語レベルで強制。GHC は `-XSafe` + totality プラグイン
  - **Turing 完全性との関係**: 停止性は決定不可能 (Halting Problem)。実用的には「十分なクラス」で検査
- **難易度**: Hard

### FR-1903: 網羅性検査 (Exhaustiveness / Coverage Checking)

- **対象**: `src/compile/codegen.lisp`
- **内容**: パターンマッチが全ケースをカバーしているかをコンパイル時検証
  - `(typecase x (integer ...) (string ...))` — `symbol` 型が欠落していれば警告
  - **有用性検査 (Usefulness)**: 到達不可能なパターンを検出
  - Maranget (2007). Warnings for pattern matching. — 実用的アルゴリズム
  - **GADT パターンマッチ**: GADT の各コンストラクタに対する型精緻化を考慮した網羅性
  - CL-CC: `(our-typecase x ...)` の各 arm に対して網羅性警告を実装
- **難易度**: Medium

### FR-1904: 正値性検査 (Positivity Checking)

- **対象**: `src/type/inference.lisp`
- **内容**: 帰納的データ型の定義が矛盾を生まないことを保証
  - **否定的出現 (Negative Occurrence)**: `(defgadt bad (mk (bad -> bad) => bad))` — `bad` が引数位置に出現 → 矛盾
  - **厳密正値性 (Strict Positivity)**: データコンストラクタの引数に `T` 自身が負の位置に現れないことを要求
  - 違反すると `(bad -> bad)` 型の不動点が構築でき、型システムが矛盾する (Curry のパラドックス)
  - Agda / Coq / Idris の positivity checker
- **難易度**: Hard

### FR-1905: サイズ型 (Sized Types)

- **対象**: 新ファイル `src/type/sized.lisp`
- **内容**: データ構造の「サイズ」を型に記録して停止性を自動証明
  - `(sized-list :s :a)` — サイズ上限 `:s` の型変数を持つリスト
  - `(cdr xs :: sized-list (pred s) :a)` — `cdr` はサイズを 1 減らす
  - **サイズ変数の算術**: `(sized-list (plus m n) :a)` = サイズ `m+n` のリスト
  - Abel (2008). MiniAgda: Integrating Sized and Dependent Types.
  - 停止性検査器が証明できないケースをサイズ型アノテーションで補助
- **難易度**: Very Hard

### FR-1906: ガード再帰 (Guarded Recursion)

- **対象**: `src/type/coinductive.lisp` (FR-1204 の余帰納型と連携)
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

- **対象**: 新ファイル `src/type/props.lisp`
- **内容**: 型をそのまま論理命題として使用し、値を証明として扱う
  - **型 = 命題、値 = 証明**: `(function :a :b)` = 「`:a` ならば `:b`」
  - **積型 = 連言**: `(pair :a :b)` = `A ∧ B`
  - **和型 = 選言**: `(or :a :b)` = `A ∨ B`
  - **依存型 = 全称量化**: `(pi (:x :a) (p :x))` = `∀x:A. P(x)`
  - **単位型 = 真**: `unit` = `⊤`。**空型 = 偽**: `void` = `⊥`
  - **CL-CC への応用**: 関数の事前/事後条件を命題として型に組み込む (FR-602 との連携)
- **難易度**: Very Hard

### FR-2002: 証明付きコード (Proof-Carrying Code)

- **対象**: 新ファイル `src/type/pcc.lisp`
- **内容**: コードに機械検証可能な正当性証明を添付
  - Necula (1997). Proof-Carrying Code.
  - `(defun safe-div (n d (proof (not (zerop d)))) ...)` — 分母非ゼロの証明を引数として受け取る
  - **証明の生成**: SMT ソルバ (Z3/CVC5) で自動生成。複雑なケースは手動
  - **証明の検証**: 証明チェッカーは実行する前に証明を高速検証
  - **JIT の安全性証明**: 動的生成コードが型安全であることを証明付きで保証
- **難易度**: Extremely Hard

### FR-2003: タクティックベース型検査 (Tactic-Based Type Checking)

- **対象**: 新ファイル `src/type/tactics.lisp`
- **内容**: 型等値の証明をタクティックで対話的に構築
  - Coq の `simpl`, `rewrite`, `reflexivity`, `induction` 等のタクティック
  - Lean 4 の `simp`, `ring`, `omega` — 自動タクティック
  - **`omega` タクティック**: 線形算術の自動証明 (Presburger arithmetic)
  - **`ring` タクティック**: 環の等式の自動証明
  - CL-CC 応用: 型レベル自然数 (FR-1701) の等値証明を `omega` で自動化
- **難易度**: Extremely Hard

### FR-2004: ホモトピー型理論 (Homotopy Type Theory / HoTT)

- **対象**: 新ファイル `src/type/hott.lisp`
- **内容**: 型理論と幾何学的ホモトピー理論の融合
  - **パス型**: `(= a b : A)` — `a` と `b` の間の「道」。証明の間の同一性を表現
  - **一価性公理 (Univalence)**: `(= A B : Type) ≃ (A ≃ B)` — 同型な型は等価
  - **高次帰納型 (Higher Inductive Types)**: `(circle)` — 2 点と 1 つのパスを持つ型 (S¹)
  - **Cubical Type Theory**: 一価性公理を計算規則として追加。Agda `--cubical`、Lean 4 のベース
  - The Univalent Foundations Program (2013). Homotopy Type Theory.
- **難易度**: Extremely Hard (理論実装)

### FR-2005: 正規化による評価 (Normalization by Evaluation, NbE)

- **対象**: `src/type/dependent.lisp` (FR-303 の依存型実装に必要)
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

- **対象**: `tests/framework/framework-fuzz.lisp`
- **内容**: 型情報から自動的にテストデータを生成
  - **QuickCheck スタイル**: `(arbitrary :a)` — 型 `:a` の値を無作為生成。型クラスで各型に実装
  - `(deftest prop-reverse (xs : (list integer)) (= (reverse (reverse xs)) xs))` — 型から `xs` を生成
  - **型駆動 fuzzing**: 関数の引数型から有効な入力を大量生成してバグを探索
  - **カバレッジ誘導**: 型情報を使って未カバーの型ブランチへの入力を優先生成
  - CL-CC の `tests/framework/framework-fuzz.lisp` に型情報を組み込む
  - Claessen & Hughes (2000). QuickCheck: A Lightweight Tool for Random Testing.
- **難易度**: Medium

### FR-2102: 型安全シリアライゼーション (Type-Directed Serialization)

- **対象**: 新ファイル `src/type/serialization.lisp`
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

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 型情報を使って中間データ構造を除去する変換
  - **ショートカット融合 (Shortcut Fusion)**: GHC の `build`/`foldr` 融合。`map f . map g = map (f . g)`
  - **stream fusion**: `Data.Vector.Fusion.Stream` — ベクタ操作の中間構造を完全除去
  - **パラメトリシティからの等式**: `(map f . filter p)` の融合則は自由定理 (FR-1403) から導出
  - Gill, Launchbury, Peyton Jones (1993). A Short Cut to Deforestation.
  - CL-CC: `(map #'f (filter #'p xs))` → 中間リストを生成しないシングルパスコードへ変換
- **難易度**: Hard

### FR-2105: 型推論のパフォーマンス (Type Inference Performance)

- **対象**: `src/type/inference.lisp`
- **内容**: 型推論の計算量を管理してコンパイル時間爆発を防ぐ
  - **型変数の数**: HM の型推論は多項式時間だが、型クラスや型族で指数的に膨張する可能性
  - **GHC の `-Wtype-complexity`**: 型の複雑さが閾値を超えると警告 (GHC 9.4+)
  - **Fuel-Based Reduction**: 型族の簡約ステップに上限を設けて無限ループを防ぐ
  - **キャッシュと共有**: 同じ型変数の制約を複数箇所で共有して重複計算を除去
  - **Incremental Constraint Solving**: 前回解いた制約の解を再利用
  - Rust-Analyzer の chalk (Prolog ベースの型推論)、Salsa (需要駆動計算)
- **難易度**: Hard

### FR-2106: 型安全 DSL 埋め込み (Type-Safe Embedded DSLs)

- **対象**: `src/expand/expander.lisp`
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
  - **SQL インジェクション防止**: すべての入力が `(sql-param :a)` として型付けされ、直接連結不可
  - TypeScript の Drizzle ORM / Prisma: スキーマから TypeScript 型を自動生成
- **難易度**: Hard

### Tier 1 — 近期実装 (実用価値 High / 依存少)

| FR | 機能 | 難易度 | 実用価値 | 依存 |
|---|---|---|---|---|
| FR-005 | 型アノテーション → Codegen 接続 | Hard | High | FR-002 |
| FR-006 | Fixnum Fast Path | Medium | High | FR-005 |
| FR-302 | 出現型 / Flow-Sensitive Narrowing | Hard | High | FR-005 |
| FR-105 | ファントム型 | Medium | Medium | — |
| FR-1202 | ゼロコスト抽象 (Newtype) | Medium | Medium | — |
| FR-1501 | Null 安全 / Optional 型 | Medium | High | — |
| FR-1601 | 型付きホール | Medium | High | — |
| FR-1101 | 変性アノテーション | Hard | Medium | FR-003 |
| FR-801 | subtypep 完全化 | Hard | High | — |

### Tier 2 — 中期実装 (基盤完成後)

| FR | 機能 | 難易度 | 実用価値 | 依存 |
|---|---|---|---|---|
| FR-203 | 行多相 | Hard | Medium | FR-003 |
| FR-601 | 段階的型付けと責任追跡 | Very Hard | High | FR-005 |
| FR-602 | 設計による契約 | Hard | Medium | — |
| FR-701 | エスケープ解析 | Hard | High | — |
| FR-702 | SROA | Hard | High | FR-701 |
| FR-104 | 存在型 | Hard | Medium | FR-003 |
| FR-1001 | 型クラス基盤 | Hard | High | FR-003 |
| FR-1203 | 再帰型 / μ型 | Hard | Medium | FR-003 |
| FR-1306 | 型ラムダ | Hard | Medium | FR-101 |
| FR-1603 | エフェクト推論 | Hard | Medium | FR-401 |

### Tier 3 — 長期実装 (研究的・基盤依存)

| FR | 機能 | 難易度 | 実用価値 | 依存 |
|---|---|---|---|---|
| FR-401 | エフェクト型 | Very Hard | High | FR-003 |
| FR-402 | 代数的エフェクト | Very Hard | High | FR-401 |
| FR-501 | 線形型 / アフィン型 | Very Hard | Medium | FR-003 |
| FR-301 | 精緻型 (Liquid Types) | Very Hard | High | FR-003, SMT |
| FR-101 | 高カインド型 | Very Hard | Medium | FR-003 |
| FR-103 | GADTs | Very Hard | Medium | FR-003 |
| FR-1301 | カインド多相 | Very Hard | Medium | FR-101 |
| FR-1404 | 集合論的型システム | Very Hard | Medium | FR-802 |
| FR-1502 | 並行安全型 | Hard | High | FR-501 |
| FR-1503 | 情報流型 | Very Hard | Medium | FR-003 |

### Tier 2 追加 (新セクション由来)

| FR | 機能 | 難易度 | 実用価値 | 依存 |
|---|---|---|---|---|
| FR-1903 | 網羅性検査 | Medium | High | — |
| FR-2101 | 型指向テスト生成 | Medium | High | FR-1001 |
| FR-2102 | 型安全シリアライゼーション | Medium | High | FR-1001 |
| FR-1604 | 値制限 | Medium | High | FR-003 |
| FR-1802 | 自由モナド | Hard | Medium | FR-1001 |
| FR-2104 | 型融合 / デフォレスト化 | Hard | High | FR-1403 |
| FR-2103 | 型安全 FFI | Hard | High | — |

### Tier 3 追加 (新セクション由来)

| FR | 機能 | 難易度 | 実用価値 | 依存 |
|---|---|---|---|---|
| FR-1901 | 停止性検査 | Hard | Medium | FR-1203 |
| FR-1904 | 正値性検査 | Hard | Medium | FR-103 |
| FR-1703 | 多段階プログラミング | Very Hard | Medium | FR-003 |
| FR-1801 | Lens / Optics | Very Hard | Medium | FR-101 |
| FR-2005 | 正規化による評価 (NbE) | Very Hard | High | FR-303 |
| FR-1704 | 型安全メタプログラミング | Very Hard | Medium | FR-1703 |
| FR-1905 | サイズ型 | Very Hard | Medium | FR-1901 |

### Tier 4 — 研究フェーズ (Extremely Hard)

| FR | 機能 | 難易度 | 根拠 |
|---|---|---|---|
| FR-303 | 依存型 | Extremely Hard | Lean 4 / Idris 2 と同等の型理論が必要 |
| FR-502 | 所有権型 / 借用検査 | Extremely Hard | Polonius アルゴリズム |
| FR-505 | セッション型 | Extremely Hard | 線形型 + 双対性の完全実装 |
| FR-1302 | 宇宙多相 | Extremely Hard | 依存型基盤が前提 |
| FR-1304 | 非述語的多相 | Extremely Hard | 型推論の決定不可能性と戦う |
| FR-1205 | 商型 | Extremely Hard | 依存型 + cubical TT が必要 |
| FR-1405 | 極性型理論 | Extremely Hard | フォーカシング証明の実装 |
| FR-2002 | 証明付きコード | Extremely Hard | SMT + 証明チェッカーの統合 |
| FR-2003 | タクティックベース型検査 | Extremely Hard | Lean 4 相当の証明エンジン |
| FR-2004 | ホモトピー型理論 | Extremely Hard | Cubical Type Theory が必要 |
| FR-1702 | 型レベル文字列 | Hard | Medium | FR-1701 |

---

## 22. 並行・分散・非同期型

### FR-2201: Async/Await 型 (Async Types)

- **対象**: 新ファイル `src/type/async.lisp`
- **内容**: 非同期計算の型。将来の値をファーストクラスで表現
  - **Future / Promise**: `(future :a)` — 将来完了する `a` 型の計算
  - **async 関数の型**: `(async-function (:a) :b)` = `(function (:a) (future :b))`
  - **await の型**: `(await (future :a)) : :a` — 非同期コンテキスト内でのみ使用可能
  - **色付き関数問題 (Colored Functions)**: 非同期関数を同期文脈から呼べないことを型で保証
  - Rust の `async`/`await` (`Future<Output=T>`)、Kotlin Coroutines (`Deferred<T>`)
  - **構造化並行性 (Structured Concurrency)**: `async with scope` — スコープを超えてタスクが漏れないことを型で保証
- **難易度**: Hard

### FR-2202: チャンネル型 (Typed Channels)

- **対象**: 新ファイル `src/type/channels.lisp`
- **内容**: メッセージパッシングのチャンネルを型付け
  - `(send-channel :a)` — 送信専用チャンネル。`(recv-channel :a)` — 受信専用チャンネル
  - **線形型との連携**: チャンネルを線形型 (FR-501) で表現し、同じメッセージを2回送信できないことを保証
  - **セッション型との連携**: チャンネルの通信プロトコルをセッション型 (FR-505) でエンコード
  - Rust の `mpsc::Sender<T>` / `Receiver<T>`、Go の型付きチャンネル `chan int`
  - **容量の型**: `(buffered-channel :a 32)` — バッファサイズ 32 のチャンネル（型レベル自然数 FR-1701 応用）
- **難易度**: Hard

### FR-2203: 型付きアクター (Typed Actors)

- **対象**: 新ファイル `src/type/actors.lisp`
- **内容**: アクターモデルでのメッセージ型を静的に検査
  - **Akka Typed (Scala)**: `ActorRef[Message]` — `Message` 型のメッセージのみ受け付けるアクター
  - `(actor-ref (or (:get-count) (:increment integer) (:reset)))` — 受け付けるメッセージの直和型
  - **プロトコル型**: アクターのライフサイクル（初期状態→応答中→終了）を Typestate (FR-504) で表現
  - **分散透過性**: ローカルアクターとリモートアクターを同じ型で扱う
  - Erlang/OTP の型安全版 (Dialyzer の限界を超える)
- **難易度**: Very Hard

### FR-2204: ソフトウェアトランザクショナルメモリ型 (STM Types)

- **対象**: 新ファイル `src/type/stm.lisp`
- **内容**: トランザクション内の操作と外部操作を型で区別
  - `(stm :a)` モナド — トランザクション内でのみ実行可能な操作
  - `(tvar :a)` — STM で管理される可変変数。`stm` モナド外から直接読み書き不可
  - `(atomically (stm :a) : :a)` — トランザクションを実行して結果を得る
  - **副作用の隔離**: `stm` コンテキスト内では I/O 禁止を型で保証（Haskell の `STM` モナドと同様）
  - Haskell の `Control.Concurrent.STM`
- **難易度**: Hard

### FR-2205: コルーチン型 / ジェネレータ型 (Coroutine / Generator Types)

- **対象**: 新ファイル `src/type/coroutines.lisp`
- **内容**: 中断・再開可能な計算の型
  - **ジェネレータ**: `(generator :yield :a)` — `:yield` 型の値を順次生成し最終的に `:a` を返す
  - **双方向コルーチン**: `(coroutine :send :receive :return)` — 値を受け取りながら値を生成
  - Rust の `Generator` trait (不安定)、Python の `Generator[YieldType, SendType, ReturnType]`
  - **継続との関係**: コルーチンは区切られた継続 (shift/reset) のシンタックスシュガーとして実装可能
  - CL-CC との直接対応: CPS 変換はコルーチンの自然な実装基盤
- **難易度**: Hard

### FR-2206: データ並列型 (Data Parallel Types / SIMD Types)

- **対象**: 新ファイル `src/type/simd.lisp`
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

- **対象**: `src/type/subtyping.lisp`
- **内容**: CL の数値塔を型システムに完全反映
  - **数値塔**: `integer <: rational <: real <: complex`
  - **fixnum <: integer <: bignum**: 実装依存の具体型と抽象型の分離
  - **exactness**: `(exact integer)` vs `(inexact float)` — 正確数と不正確数の型区別
  - **contagion rules**: `(+ integer float) → float` — 演算の型昇格規則を型推論に組み込む
  - ANSI CL 12.1 の数値塔を型推論の subtyping lattice として実装
- **難易度**: Medium

### FR-2302: 計量単位型 (Units of Measure)

- **対象**: 新ファイル `src/type/units.lisp`
- **内容**: 数値に物理単位を型として付与し、単位の不整合をコンパイル時に検出
  - `(measure float meters)` — メートル単位の浮動小数点数
  - **単位算術**: `(/ (measure v meters) (measure t seconds)) : (measure float meters/second)`
  - **次元解析**: `(+ meters seconds)` → コンパイルエラー（次元不整合）
  - F# の Units of Measure (Kennedy 1994/2009): 完全な実装例
  - **型消去**: 単位情報はコンパイル時のみ存在。実行時は通常の float
  - `(convert-unit 100.0 'centimeters 'meters)` → コンパイル時に変換係数を検証
- **難易度**: Hard

### FR-2303: 区間算術型 (Interval Arithmetic Types)

- **対象**: 新ファイル `src/type/intervals.lisp`
- **内容**: 数値の取り得る範囲を型に記録して数値エラーを静的に防ぐ
  - `(interval-type integer 0 255)` — 0〜255 の整数（バイト型）
  - `(interval-type float 0.0 1.0)` — 0.0〜1.0 の確率値
  - **区間伝播**: `(+ (interval 0 10) (interval 5 15)) : (interval 5 25)` — 演算結果の範囲を追跡
  - **overflow 検出**: 結果区間が型の範囲を超えるとコンパイルエラー
  - 精緻型 (FR-301) のサブセットとして実装可能（`{v: Int | 0 <= v && v <= 255}` の特化版）
- **難易度**: Hard

### FR-2304: テンソル型 / 形状多相 (Tensor Types / Shape Polymorphism)

- **対象**: 新ファイル `src/type/tensors.lisp`
- **内容**: 多次元配列の形状（各次元のサイズ）を型に記録
  - `(tensor (m n) float)` — m×n の float 行列
  - **形状多相関数**: `(map-rows (tensor (m n) float) -> (tensor (m k) float))` — `m` は固定、`n`→`k`
  - **行列積**: `(matmul (tensor (m k) float) (tensor (k n) float) -> (tensor (m n) float))` — `k` が一致することを型で保証
  - **ブロードキャスト型**: NumPy のブロードキャスト規則を型で表現 (Dex 言語の研究課題)
  - JAX の `jaxtyping`、Dex 言語 (Google Research)、Futhark の rank polymorphism
- **難易度**: Very Hard

### FR-2305: 固定精度型 / 任意精度型 (Fixed-Point / Arbitrary Precision Types)

- **対象**: `src/type/inference.lisp`
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

- **対象**: `src/type/inference.lisp`
- **内容**: 型変数の単一化アルゴリズムの正確な実装
  - **Robinson's Unification (1965)**: 最も単純な一階 unification。最悪指数時間
  - **Martelli-Montanari (1982)**: 多集合ベースの効率的 unification。準線形時間
  - **Union-Find による実装**: Tarjan の Union-Find で型変数の等価クラスを管理
  - **発生検査 (Occurs Check)**: `unify a (list a)` は発生検査なしで無限型を生じる。Haskell は発生検査なし（意図的）
  - **Higher-Order Unification**: System F の型適用での unification は一般に決定不可能（Huet 1975）
- **難易度**: Medium

### FR-2402: 型変数のスコーピング / Skolem 化 (Type Variable Scoping / Skolemization)

- **対象**: `src/type/inference.lisp`
- **内容**: 型変数のスコープとその具体化の管理
  - **剛性型変数 (Rigid Type Variables)**: 型アノテーションからの型変数。ユーザーが固定した型
  - **柔軟型変数 (Flexible/Wobbly Type Variables)**: 推論中の未確定変数。unification で具体化
  - **Skolem 化**: `(forall a. ...)` を具体的な Skolem 定数に置き換えて内部化
  - **エスケープ検査**: Skolem 変数がそのスコープ外に漏れていないことを確認（存在型の境界チェック）
  - GHC の `SkolemTv` vs `TauTv` の区別
- **難易度**: Hard

### FR-2403: 制約伝播 (Constraint Propagation)

- **対象**: `src/type/inference.lisp`
- **内容**: 型制約を効率的に解く伝播アルゴリズム
  - **前方伝播**: 新しい制約が追加されたとき、既存の変数への影響を即座に伝播
  - **後方伝播**: 制約が矛盾を引き起こしたとき、原因を遡って特定
  - **制約グラフ**: 型変数間の制約を有向グラフで管理。Arc consistency (AC-3 アルゴリズム)
  - **型クラス制約の解決順序**: 制約を解く順序が型推論の完全性に影響
  - GHC の `TcTyVar` と constraint canonicalization
- **難易度**: Hard

### FR-2404: 可視的型適用 (Visible Type Application)

- **対象**: `src/type/inference.lisp`
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

- **対象**: 新ファイル `src/type/smt.lisp`
- **内容**: 複雑な型制約を外部 SMT ソルバで解く
  - **Z3 / CVC5 統合**: 精緻型 (FR-301) の述語を SMT 論理式に変換してソルバへ送信
  - **反例生成**: 型エラー時にソルバが反例（型違反を引き起こす具体的な値）を生成
  - **量化制約**: `(forall x. P(x))` を SMT で自動証明
  - **線形算術**: `{v: Int | 0 <= v && v < n}` の `v + 1 < n` の証明を Presburger arithmetic で解く
  - Liquid Haskell (GHC プラグイン)、F* (Microsoft Research)、Dafny (Microsoft)
- **難易度**: Very Hard

---

## 25. 動的言語の型統合

### FR-2501: Dynamic 型 (Dynamic / Any Type)

- **対象**: `src/vm/primitives.lisp`
- **内容**: 型が不明な値を型安全に扱う「型付き動的値」
  - **`Dynamic` 型** (Haskell): `data Dynamic = forall a. Typeable a => Dynamic a`
  - `(wrap-dynamic 42)` → `(dynamic integer 42)`。`(unwrap-dynamic d : integer)` — 型指定アンラップ
  - **失敗時の処理**: 型が合わない場合は `Maybe` / `Either` で型安全に失敗
  - CL の `(typep x 'integer)` の型推論版
  - **Any 型** (TypeScript/Go): すべての型のスーパータイプ。動的型との違いは implicit coercion の有無
  - **`unknown` vs `any`** (TypeScript): `unknown` は使用前に型ナローイング必須
- **難易度**: Medium

### FR-2502: ランタイム型表現 (Runtime Type Representations / TypeRep)

- **対象**: `src/vm/primitives.lisp`
- **内容**: コンパイル時の型情報を実行時オブジェクトとして操作
  - **`TypeRep a`** (Haskell `Data.Typeable`): `typeOf :: a -> TypeRep a` — 型の実行時表現を取得
  - `(type-rep integer)` — `integer` 型の実行時表現。`(type-rep-eq (type-rep integer) (type-rep string))` → false
  - **型安全キャスト**: `(cast x (type-rep integer))` — `TypeRep` を使って型安全にキャスト
  - **型写像**: 型から関数へのマッピング（型ごとの異なる実装の型安全なディスパッチ）
  - CL の `class-of` / `find-class` の型安全版
- **難易度**: Medium

### FR-2503: テンプレートリテラル型 (Template Literal Types)

- **対象**: `src/type/parser.lisp`
- **内容**: 文字列テンプレートを型レベルで操作（TypeScript 4.1+）
  - `(template-literal "on" (capitalize :event))` — `"onClick"` や `"onChange"` の型を生成
  - **文字列操作型関数**: `(uppercase :s)`, `(lowercase :s)`, `(capitalize :s)` — 型レベル文字列変換
  - TypeScript: `` type EventHandlers<T extends string> = `on${Capitalize<T>}` ``
  - CL-CC 応用: シンボル命名規則を型で保証 (`with-slots` のスロット名検査)
- **難易度**: Hard

### FR-2504: 型ガードと型述語 (Type Guards / Type Predicates)

- **対象**: `src/type/inference.lisp`
- **内容**: ランタイム型チェック関数の戻り値を型絞り込みに活用
  - TypeScript の型述語: `function isString(x: unknown): x is string { return typeof x === 'string' }`
  - 関数の戻り値型が `x is T` の場合、`if (isString(x))` ブランチ内で `x: string` が確定
  - CL-CC: `(defun integerp (x) : (x is integer) ...)` — 型述語関数の型シグネチャ
  - 出現型 (FR-302) の関数抽象版: `typep`/`integerp`/`consp` 等の組み込み述語を型システムに登録
- **難易度**: Medium

### FR-2505: 型付き正規表現 (Typed Regular Expressions)

- **対象**: 新ファイル `src/type/regex.lisp`
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

- **対象**: `src/type/typeclasses.lisp`
- **内容**: 型クラス制約に全称量化子を付与
  - GHC `-XQuantifiedConstraints` (GHC 8.6+)
  - `(forall :a. (show :a) => (show (list :a)))` — 「任意の `Show a` なら `Show [a]`」を制約として表現
  - **用途**: `(type-class traversable (:t) (:super (forall :a. functor (:t :a))))` — スーパークラスに量化制約
  - Bottu et al. (2018). Quantified Class Constraints.
  - CL-CC 応用: CLOS メソッドの特殊化条件を型クラス制約として表現
- **難易度**: Very Hard

### FR-2602: Constraint カインド (Constraint Kinds)

- **対象**: `src/type/typeclasses.lisp`
- **内容**: 型クラス制約を型として第一級に扱う
  - GHC `-XConstraintKinds`: `(Constraint)` カインドで制約自体を型変数に代入可能
  - `(type-alias constraint-of (:a) (tuple (eq :a) (show :a)))` — 複合制約の別名定義
  - `(type :c (constraint) => (function ((satisfying :c :a)) :a))` — 制約を型変数として受け取る
  - **用途**: 制約の抽象化、制約の合成、型クラス制約のリストを型として扱う
- **難易度**: Hard

### FR-2603: 型クラス証人 (Type Class Witnesses / Dict)

- **対象**: `src/type/typeclasses.lisp`
- **内容**: 型クラスインスタンスを実行時の「辞書オブジェクト」として第一級に扱う
  - `(dict (eq integer))` — `Eq Integer` インスタンスの辞書を実行時値として取得
  - **反射的型クラス**: `(reify-constraint :c (lambda (dict) ...))` — 制約を値として渡す
  - `(with-dict some-dict (use-eq))` — 辞書を注入して型クラスメソッドを使用
  - Haskell の `reflection` パッケージ。型クラスのランタイム選択が可能になる
  - CL-CC 応用: CLOS の `find-method` の型安全版
- **難易度**: Hard

### FR-2604: DerivingVia (戦略的 deriving)

- **対象**: `src/expand/expander.lisp`
- **内容**: 既存の型のインスタンスを流用して新しい型のインスタンスを導出
  - GHC `-XDerivingVia` (GHC 8.6+)
  - `(defnewtype my-int integer (:deriving eq via integer))` — `integer` の `Eq` インスタンスをそのまま流用
  - `(defnewtype sum (:a) (:a) (:deriving monoid via (additive :a)))` — `Additive` を経由して `Monoid` を導出
  - Blöndal et al. (2018). Deriving Via: or, How to Turn Hand-Written Instances into an Anti-Pattern.
  - **実装原理**: `newtype` の `Coercible` (FR-1202) を使って辞書を変換
- **難易度**: Medium

### FR-2605: 暗黙関数型 (Implicit Function Types)

- **対象**: `src/type/inference.lisp`
- **内容**: 暗黙のコンテキストを型で表現するScala 3スタイルの仕組み
  - Scala 3: `def foo(x: Int)(implicit ctx: Context): Int` → `def foo(x: Int): Context ?=> Int`
  - `(function :a (:b ?=> :c))` — `B` の暗黙インスタンスがあれば `A -> C`
  - **型クラスとの関係**: 型クラスの辞書渡しを明示化したもの。型クラスより表現力が高い（局所インスタンス）
  - **スコープ制御**: `(given (instance ...) body)` — `body` の中だけ暗黙値が有効
  - Scala 3 の `given`/`using`、Agda の instance arguments
- **難易度**: Hard

### FR-2606: マッチ型 (Match Types)

- **対象**: `src/type/inference.lisp`
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

- **対象**: 新ファイル `src/type/quantum.lisp`
- **内容**: 量子ビット・量子操作を型安全に扱う
  - **Qubit 型**: `(qubit)` — 量子ビット（|0⟩ と |1⟩ の重ね合わせ）
  - **線形型との連携**: 量子力学の no-cloning 定理 → qubit は線形型 (FR-501)。コピー不可
  - **エンタングルメント型**: `(entangled-pair qubit qubit)` — 2 つの qubit のエンタングル状態
  - **量子回路型**: `(circuit n m)` — n 入力 m 出力の量子回路
  - **測定の型**: `(measure (qubit) : (pair boolean qubit))` — 測定後の古典ビットと量子ビット
  - Quipper (Haskell ベースの量子プログラミング言語)、QSharp (Microsoft)、Silq
- **難易度**: Extremely Hard

### FR-2702: 確率的型 (Probabilistic Types)

- **対象**: 新ファイル `src/type/probabilistic.lisp`
- **内容**: 確率分布を型として扱う
  - `(distribution :a)` — `a` 型の値の確率分布
  - `(bernoulli float)` — 成功確率 `p` のベルヌーイ分布。型は `(distribution boolean)`
  - **確率的プログラミング**: `(sample (normal 0.0 1.0))` → `float`。型は `(stochastic float)`
  - **型推論との統合**: 確率的プログラムの型 = 確率分布型
  - Church、WebPPL、Gen.jl (Julia) などの確率的プログラミング言語のアプローチ
  - **測度論的意味論**: 確率分布をモナドとして表現 (`Distribution` モナド)
- **難易度**: Extremely Hard

### FR-2703: 微分可能型 (Differentiable Types / AD Types)

- **対象**: 新ファイル `src/type/ad.lisp`
- **内容**: 自動微分 (Automatic Differentiation) を型システムに組み込む
  - `(differentiable float)` — 微分可能な float 値
  - **前向き AD**: `(dual :a)` = `(:a, :a)` — 値と微分値のペア型
  - **後向き AD (Backprop)**: 計算グラフを型で追跡して逆伝播を型安全に実装
  - **高階微分**: `(diff^n :a)` — n 階微分可能な型
  - Dex (Google Research)、Zygote.jl (Julia)、Enzyme (LLVM)
  - **型安全ニューラルネット**: 層の入出力次元を型で管理 (FR-2304 テンソル型との統合)
- **難易度**: Very Hard

### FR-2704: ハードウェア設計型 (Hardware / Circuit Types)

- **対象**: 新ファイル `src/type/hardware.lisp`
- **内容**: デジタル回路・FPGA 設計の型安全な記述
  - **Signal 型**: `(signal :a)` — クロックサイクルで進む `a` 型の値の流れ
  - **ビット幅型**: `(bits 8)` — 8 ビット整数。算術オーバーフローをビット幅で型管理
  - **クロックドメイン型**: `(in-domain clk1 :a)` vs `(in-domain clk2 :a)` — クロック境界をまたぐデータ転送を型で制御
  - Clash (Haskell ベースの FPGA 言語)、Lava、Chisel (Scala)
  - **型レベルクロック**: クロック間の同期回路と非同期回路を型で区別
- **難易度**: Extremely Hard

### FR-2705: 型安全設定言語 (Type-Safe Configuration)

- **対象**: 新ファイル `src/type/config.lisp`
- **内容**: 設定ファイルの型を静的に検証
  - **Dhall**: 設定言語に型システムを組み込む。Turing 非完全（停止性保証）
  - **Nickel**: Dhall の後継。段階的型付け + 契約
  - **CUE**: 型 = バリデーション制約。データとスキーマが同一構文
  - `(config-type (:port (interval integer 1024 65535)) (:host string))` — 設定型のスキーマ定義
  - CL-CC 応用: `defconfig` マクロで設定構造と型を一体定義
- **難易度**: Medium

### FR-2706: 型安全ステートマシン (Typed State Machines)

- **対象**: `src/type/typeclasses.lisp`
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

- **対象**: `src/type/props.lisp` (FR-2001 Curry-Howard の拡張)
- **内容**: Hoare Triple `{P} C {Q}` を型として表現し、プログラムの正当性を型検査で検証
  - `{pre: (> n 0)} (factorial n) {post: (> result 0)}` — 事前条件と事後条件を型に組み込む
  - **Hoare 型理論 (HTT)**: Nanevski et al. (2008). `(htt-type (pre :p) computation (post :q))` で命令型プログラムを型付け
  - **Separation Logic との統合**: ヒープ操作に分離論理 (`*` = ヒープ分離結合) を型で表現
  - **並行分離論理 (CSL)**: O'Hearn (2007). ロックや共有メモリを含む並行プログラムの型安全な推論
  - DBC (FR-602) との違い: DBC は実行時チェック指向。Hoare Logic は静的証明指向
  - F* (Microsoft Research): Hoare 型理論の実用実装。`ST` エフェクトに事前/事後条件を付与
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

- **対象**: `src/type/inference.lisp`
- **内容**: Cousot & Cousot (1977) の抽象解釈フレームワークを型推論に適用
  - **抽象ドメイン**: 具体的な値の集合を「型」という抽象値で近似。`integer` = 「すべての整数の集合」
  - **ワイドニング (Widening)**: 解析が収束しない場合に強制的に抽象値を広げる演算 `∇`
  - **ナローイング (Narrowing)**: ワイドニングで失った精度を取り戻す演算 `△`
  - **ギャランティング**: 解析結果が「健全 (sound)」= 実際の実行を必ず近似
  - 型推論の不動点計算をワイドニングで高速化
  - Cousot & Cousot (1977). Abstract Interpretation: A Unified Lattice Model.
- **難易度**: Very Hard

### FR-2805: 行動的サブタイピング (Behavioral Subtyping / Liskov Substitution)

- **対象**: `src/type/subtyping.lisp`
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

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 関数が引数を「必ず評価するか」をコンパイル時に解析
  - **厳格性**: `f` が引数 `x` に厳格 = `f ⊥ = ⊥`（`x` が未定義なら `f x` も未定義）
  - **活用**: 厳格な引数は遅延評価のサンクを作らずに即時評価可能 → サンク割り当て削減
  - **Demand Signatures**: `<S, 1*U>` — 最初の引数を厳格に使い、2番目を1回だけ使う
  - GHC の Demand Analyser: `StrDmd`, `UseDmd`, `DmdType` — 関数ごとの demand シグネチャ
  - CL-CC: `vm-call` での引数評価順序最適化、クロージャ内変数の遅延評価省略
- **難易度**: Hard

### FR-2902: 型ベースエイリアス解析 (Type-Based Alias Analysis, TBAA)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 型情報から「2つのポインタが同じメモリ位置を指す可能性があるか」を判定
  - **TBAA**: 異なる型のポインタはエイリアスしない（C の strict aliasing rule に対応）
  - `(integer* a)` と `(float* b)` は絶対にエイリアスしない → 一方への書き込みが他方のロードに影響しない → 命令の並び替え可能
  - LLVM の `!tbaa` メタデータ: IR 命令に型エイリアス情報を付与
  - CL-CC: `vm-slot-ref` (CLOS スロット) と `vm-car` (コンス) はエイリアスしない → ロード/ストアの並び替え
- **難易度**: Hard

### FR-2903: ボックス化解析 (Boxity Analysis)

- **対象**: `src/compile/codegen.lisp`
- **内容**: 値をヒープに「箱詰め (boxing)」するか、レジスタに「非箱詰め (unboxed)」のまま使うか解析
  - **Boxed**: ヒープにオブジェクトを割り当て、ポインタを通じてアクセス。GC 管理下
  - **Unboxed**: レジスタや機械語ワードに直接格納。GC 管理外
  - GHC の Boxity Analysis: 引数・戻り値が Boxed/Unboxed どちらで使われるか解析して最適化
  - **Worker/Wrapper 変換**: `f :: Int -> Int` を `f_worker :: Int# -> Int#` (unboxed) に分離
  - CL-CC: NaN-boxing タグの付け外しコストをボックス化解析で最小化
- **難易度**: Hard

### FR-2904: アリティ解析 (Arity Analysis)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 関数が何引数で完全適用 (saturated) されるかをコンパイル時に解析
  - **飽和適用 (Saturated)**: `(f a b)` where `f` takes exactly 2 args → 仮想ディスパッチなし
  - **未飽和適用 (Unsaturated)**: `(f a)` where `f` takes 2 args → クロージャを作成
  - **過飽和適用 (Oversaturated)**: `(f a b c)` where `f` takes 2 args → 結果を `c` に適用
  - GHC の Arity Analysis: `ArityType` を各関数に付与してコールパターンを最適化
  - CL-CC: `vm-call` の呼び出し規約選択（引数カウントが一致する場合の fast path）
- **難易度**: Medium

### FR-2905: 使用数解析 (Usage / Occurrence Analysis)

- **対象**: `src/optimize/optimizer.lisp`
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

- **対象**: `src/type/inference.lisp`
- **内容**: ライブラリ作者がユーザー向けの型エラーメッセージをカスタマイズ
  - GHC の `TypeError` 型クラス: `(type-error (text "Cannot add ") (show :a) (text " to ") (show :b))`
  - 型クラスインスタンスが見つからない場合に、デフォルトの難解なエラーではなく人間が読みやすいメッセージを表示
  - **エラーレベル**: `TypeError` (コンパイルエラー)、`Warn` (警告)、`Info` (情報)
  - CL-CC 応用: `defgeneric` に `:type-error` オプションを追加
- **難易度**: Medium

### FR-3002: 型検査プラグイン (Type Checking Plugins)

- **対象**: `src/type/inference.lisp`
- **内容**: 型推論エンジンにサードパーティの制約ソルバを差し込む拡張機構
  - GHC の `TcPlugin` インターフェース: `tcPluginSolve`, `tcPluginRewrite` をフック
  - **応用**: SMT ソルバプラグイン（精緻型のため）、ユニット型プラグイン（計量単位のため）
  - `ghc-typelits-natnormalise`: 型レベル自然数算術のプラグイン
  - `ghc-typelits-knownnat`: `KnownNat` 制約の自動導出プラグイン
  - CL-CC: `*type-checker-plugins*` リストにフック関数を登録する仕組み
- **難易度**: Hard

### FR-3003: 型指向プログラム合成 (Type-Directed Program Synthesis)

- **対象**: 新ファイル `src/type/synthesis.lisp`
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
  - **相互運用の健全性**: 送信側と受信側の型が互換することをコンパイル時に保証
- **難易度**: Hard

### FR-3005: 型ベースドキュメント生成 (Type-Based Documentation)

- **対象**: `src/cli/main.lisp`
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

- **対象**: `src/type/linear-logic.lisp`
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

- **対象**: `src/type/linear-logic.lisp`
- **内容**: 線形論理の「無制限使用を許可するモダリティ」
  - `!A` (バン A): `A` 型の値を**任意回**コピー・破棄できる
  - **線形型と通常型の橋渡し**: `(!A → B)` — `A` を無制限に使える線形関数
  - **弱化規則**: `!A ⊢ unit` — `!A` は使わなくても良い（破棄可能）
  - **収縮規則**: `!A ⊢ !A ⊗ !A` — `!A` はコピー可能
  - Linear Haskell での対応: `a` (非制限) = `!a` in linear logic
  - CPS との関係: CPS 変換後の継続型は線形型 (1回だけ呼ばれる)。`!` で非線形な継続を表現
- **難易度**: Hard

### FR-3103: Bounded Linear Logic (BLL) と計算複雑性型

- **対象**: 新ファイル `src/type/complexity-types.lisp`
- **内容**: 資源の使用回数に上限を付与して計算複雑性を型で制御
  - **BLL (Girard, Scedrov, Scott 1992)**: `!n A` — `A` を**最大 n 回**使用できる
  - **多項式時間型**: BLL の型に入るプログラムは多項式時間で動作することが保証
  - **型から複雑性クラスの導出**: `PTIME`, `PSPACE` などの複雑性クラスを型で特徴づける
  - **Soft Linear Logic**: 多項式時間関数型プログラミングのための線形論理
  - Girard, Scedrov, Scott (1992). Bounded Linear Logic.
  - **実用応用**: 組み込みシステムや実時間システムでの計算時間保証
- **難易度**: Extremely Hard

### FR-3104: 交換子・弱化・収縮の型制御 (Structural Rules as Types)

- **対象**: `src/type/linear-logic.lisp`
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

- **対象**: `src/type/inference.lisp`
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

- **対象**: `src/type/subtyping.lisp`
- **内容**: 型の「出現位置の極性」でサブタイピングの方向を自動的に決定
  - **正の位置 (Positive)**: 戻り値・共変位置。`A ∨ B` = 和型 (union)
  - **負の位置 (Negative)**: 引数・反変位置。`A ∧ B` = 交差型 (intersection)
  - `(function A B)` — `A` は負の位置（反変）、`B` は正の位置（共変）
  - **型の正規形**: 正の位置の型を交差で表現するのは意味がない（最大値を取るため）。極性で不必要な型を自動排除
  - **MLsub との関係**: 代数的サブタイピング (FR-3201) の理論的基盤
  - Zeilberger (2013). Refinement Types as Higher-Order Subtyping.
- **難易度**: Hard

### FR-3203: パス依存型 (Path-Dependent Types)

- **対象**: 新ファイル `src/type/path-dependent.lisp`
- **内容**: 項（オブジェクト）のメンバーとして宣言された型。項への参照（パス）に依存する型
  - Scala の `a.T`: オブジェクト `a` が持つ型メンバー `T`。`a` が変われば型も変わる
  - `(type-member module-instance inner-type)` — モジュールインスタンスに依存した型
  - **DOT 計算** (Dependent Object Types): Amin et al. (2016). Scala の型システムの理論的基盤
  - **モジュール型との関係**: OCaml のファンクタ (FR-603) は関数として表現されたモジュール。パス依存型はその一般化
  - **抽象型の具体化**: `(new my-module)` して `my-module.element-type` でモジュール固有の型を得る
  - **CL-CC 応用**: `defpackage` + `deftype` のスコープ付き型アクセス
- **難易度**: Very Hard

### FR-3204: 自己型 / This 型 (Self Type / This Type)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**: オブジェクトのクラス自体を型変数として使う再帰的型
  - **問題**: `(defmethod clone (self) ...)` の戻り値型は `self` のクラスと同じであるべきだが、スーパークラスに定義すると型が固定してしまう
  - `(this-type)` — 現在のクラス（またはサブクラス）を表す型変数
  - **共変戻り値型**: `(override clone : (this-type))` — サブクラスでオーバーライドすると戻り値型がサブクラスに
  - **流暢インターフェース (Fluent Interface)**: `builder.setName(x).setAge(y)` でメソッドチェーンの型がサブクラスを保持
  - TypeScript の `this` 型、Scala の `this.type`、Kotlin の `T: This` パターン
  - CLOS の `call-next-method` に対応する型理論的説明
- **難易度**: Hard

### FR-3205: ブランド型 / 公称的型付け (Branded / Nominal Types in Structural Systems)

- **対象**: `src/type/parser.lisp`
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

- **対象**: `src/type/parser.lisp`
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

- **対象**: `src/type/inference.lisp`
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

- **対象**: `src/type/inference.lisp`
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

- **対象**: `src/type/utils.lisp`
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

- **対象**: 新ファイル `src/type/routing.lisp`
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

- **対象**: 新ファイル `src/type/qtt.lisp`
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

- **対象**: 新ファイル `src/type/graded.lisp`
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

- **対象**: `src/compile/cps.lisp`
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

- **対象**: `src/type/effects.lisp` (FR-401 の代替実装)
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

- **対象**: `src/type/dependent.lisp` (FR-303 の依存型実装に必須)
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

- **対象**: 新ファイル `src/type/cic.lisp`
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
