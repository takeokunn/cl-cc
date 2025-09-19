# リファレンス: CL-CC 技術仕様書

## 🎯 このセクションについて

リファレンスセクションは、CL-CCの**完全な技術仕様**を提供します。これは辞書的な参考資料として設計されており、開発者が具体的な実装詳細や API 仕様を迅速に確認できるように構成されています。

## 📚 構成概要

```
reference/
├── README.md                    # このファイル
├── core-api/                    # コアAPI仕様
├── frontend-api/                # フロントエンドAPI仕様
├── backend-api/                 # バックエンドAPI仕様
├── optimization-api/            # 最適化API仕様
├── data-structures/             # データ構造仕様
├── macro-system/                # マクロシステム仕様
├── clos-hierarchy/              # CLOS クラス階層
├── prolog-system/               # S式Prolog仕様
└── protocols/                   # プロトコル定義
```

## 🔧 コアAPI

### コンパイラインターフェース
- [**コンパイラメインAPI**](core-api/compiler-interface.md)
  - `compile-program`, `compile-expression`
  - オプション設定、エラーハンドリング
  - バッチ処理、ストリーミング対応

- [**設定管理API**](core-api/configuration-api.md)
  - 最適化レベル設定
  - ターゲットプラットフォーム指定
  - デバッグ情報制御

- [**エラーシステムAPI**](core-api/error-system-api.md)
  - `compiler-error`, `syntax-error`, `type-error`
  - エラー情報の構造化
  - 回復可能エラーの処理

### フロントエンドAPI
- [**字句解析API**](frontend-api/lexer-api.md)
  - トークナイザークラス
  - トークン型定義
  - 位置情報管理

- [**構文解析API**](frontend-api/parser-api.md)
  - パーサージェネレータ
  - AST構築インターフェース
  - エラー回復戦略

- [**意味解析API**](frontend-api/semantic-analysis-api.md)
  - シンボルテーブル管理
  - 型チェッカー
  - スコープ解析

### バックエンドAPI
- [**コード生成API**](backend-api/codegen-api.md)
  - ターゲット抽象化インターフェース
  - 命令選択アルゴリズム
  - 最適化フック

- [**レジスタ割り当てAPI**](backend-api/register-allocation-api.md)
  - アロケーター基底クラス
  - ライブネス解析
  - スピル処理

- [**アセンブラAPI**](backend-api/assembler-api.md)
  - 命令エンコーディング
  - シンボル解決
  - オブジェクトファイル生成

### 最適化API
- [**最適化フレームワークAPI**](optimization-api/framework-api.md)
  - パス管理システム
  - 依存関係解決
  - メトリクス収集

- [**データフロー解析API**](optimization-api/dataflow-api.md)
  - 抽象解釈フレームワーク
  - Worklist アルゴリズム
  - 格子理論実装

- [**変換パスAPI**](optimization-api/transformation-api.md)
  - IR変換インターフェース
  - 安全性チェック
  - 効果測定

## 📊 データ構造

### AST（抽象構文木）
- [**AST仕様**](data-structures/ast-specification.md)
  ```lisp
  (defclass ast-node ()
    ((location :initarg :location
               :accessor node-location
               :type (or null source-location))))

  (defclass expression-node (ast-node) ())
  (defclass statement-node (ast-node) ())

  ;; 具体的なノード型
  (defclass literal-node (expression-node)
    ((value :initarg :value :accessor literal-value)))

  (defclass binary-op-node (expression-node)
    ((operator :initarg :operator :accessor binary-operator)
     (left :initarg :left :accessor binary-left)
     (right :initarg :right :accessor binary-right)))
  ```

- [**ノード型一覧**](data-structures/ast-node-types.md)
  - 式ノード: リテラル、変数、演算子、関数呼び出し
  - 文ノード: 代入、制御フロー、宣言
  - 宣言ノード: 関数、変数、型定義

### IR（中間表現）
- [**IR仕様**](data-structures/ir-specification.md)
  ```lisp
  (defclass ir-instruction ()
    ((opcode :initarg :opcode :accessor ir-opcode)
     (operands :initarg :operands :accessor ir-operands)
     (result :initarg :result :accessor ir-result)
     (metadata :initform nil :accessor ir-metadata)))

  ;; SSA形式のサポート
  (defclass ssa-instruction (ir-instruction)
    ((dominance-info :accessor ssa-dominance-info)
     (use-def-chains :accessor ssa-use-def-chains)))
  ```

- [**命令セット**](data-structures/ir-instruction-set.md)
  - 算術演算: `ADD`, `SUB`, `MUL`, `DIV`
  - 制御フロー: `BRANCH`, `JUMP`, `CALL`, `RETURN`
  - メモリ操作: `LOAD`, `STORE`, `ALLOCA`
  - 型変換: `CAST`, `CONVERT`, `BITCAST`

### シンボルテーブル
- [**シンボルテーブル仕様**](data-structures/symbol-table.md)
  ```lisp
  (defclass symbol-table ()
    ((entries :initform (make-hash-table :test #'equal)
              :accessor symbol-entries)
     (parent :initarg :parent
             :initform nil
             :accessor symbol-parent-scope)))

  (defclass symbol-entry ()
    ((name :initarg :name :accessor symbol-name)
     (type :initarg :type :accessor symbol-type)
     (kind :initarg :kind :accessor symbol-kind)
     (location :initarg :location :accessor symbol-location)
     (attributes :initform '() :accessor symbol-attributes)))
  ```

### 型システム
- [**型システム仕様**](data-structures/type-system.md)
  ```lisp
  (defclass type () ())

  (defclass primitive-type (type)
    ((name :initarg :name :accessor type-name)))

  (defclass function-type (type)
    ((parameter-types :initarg :parameter-types)
     (return-type :initarg :return-type)))

  (defclass generic-type (type)
    ((base-type :initarg :base-type)
     (type-parameters :initarg :type-parameters)))
  ```

## 🔮 マクロシステム

### コンパイラマクロ
- [**コンパイラマクロAPI**](macro-system/compiler-macros.md)
  ```lisp
  (defmacro define-compiler-macro (name lambda-list &body body)
    "コンパイラマクロを定義"
    ...)

  (defmacro with-compiler-environment ((&key optimization-level target) &body body)
    "コンパイラ環境での実行"
    ...)
  ```

### DSL定義マクロ
- [**DSL構築マクロ**](macro-system/dsl-macros.md)
  ```lisp
  (defmacro define-language (name &key syntax semantics optimization)
    "新しいDSLを定義"
    ...)

  (defmacro define-syntax-rule (pattern expansion &key precedence associativity)
    "構文規則を定義"
    ...)
  ```

### 最適化マクロ
- [**最適化マクロ**](macro-system/optimization-macros.md)
  ```lisp
  (defmacro define-optimization-rule (name pattern replacement &key conditions cost)
    "最適化ルールを定義"
    ...)

  (defmacro with-optimization-context ((&key level target constraints) &body body)
    "最適化コンテキストでの実行"
    ...)
  ```

## 🏗️ CLOSクラス階層

### 基底クラス
- [**基底クラス**](clos-hierarchy/base-classes.md)
  ```lisp
  ;; すべてのCL-CCオブジェクトの基底
  (defclass cl-cc-object ()
    ((id :initform (gensym) :reader object-id)
     (metadata :initform nil :accessor object-metadata)))

  ;; 位置情報を持つオブジェクト
  (defclass located-object (cl-cc-object)
    ((location :initarg :location
               :accessor object-location
               :type (or null source-location))))
  ```

### フロントエンドクラス
- [**フロントエンドクラス階層**](clos-hierarchy/frontend-classes.md)
  ```lisp
  (defclass frontend (cl-cc-object)
    ((language :initarg :language :accessor frontend-language)
     (version :initarg :version :accessor frontend-version)))

  (defclass lexer (frontend) ())
  (defclass parser (frontend) ())
  (defclass semantic-analyzer (frontend) ())
  ```

### バックエンドクラス
- [**バックエンドクラス階層**](clos-hierarchy/backend-classes.md)
  ```lisp
  (defclass backend (cl-cc-object)
    ((target :initarg :target :accessor backend-target)
     (options :initform '() :accessor backend-options)))

  (defclass code-generator (backend) ())
  (defclass assembler (backend) ())
  (defclass linker (backend) ())
  ```

### 最適化クラス
- [**最適化クラス階層**](clos-hierarchy/optimization-classes.md)
  ```lisp
  (defclass optimization-pass (cl-cc-object)
    ((name :initarg :name :accessor pass-name)
     (dependencies :initform '() :accessor pass-dependencies)
     (cost :initform 1 :accessor pass-cost)))

  (defclass analysis-pass (optimization-pass) ())
  (defclass transformation-pass (optimization-pass) ())
  ```

## 🧠 S式Prologシステム

### 述語リファレンス
- [**組み込み述語**](prolog-system/builtin-predicates.md)
  ```prolog
  % 型関連述語
  type(+Expr, -Type).
  subtype(+Type1, +Type2).
  unify_types(+Type1, +Type2, -UnifiedType).

  % 最適化述語
  optimize(+Rule, +Input, -Output).
  cost(+Transformation, -Cost).
  safe_transformation(+Rule, +Context).
  ```

### 推論規則
- [**推論規則システム**](prolog-system/inference-rules.md)
  ```prolog
  % 型推論規則
  infer_type(literal(N), integer) :- integer(N).
  infer_type(literal(S), string) :- string(S).
  infer_type(binary_op(Op, L, R), ResultType) :-
      infer_type(L, LType),
      infer_type(R, RType),
      binary_op_result_type(Op, LType, RType, ResultType).

  % 最適化規則
  constant_fold(binary_op(+, literal(A), literal(B)), literal(C)) :-
      number(A), number(B),
      C is A + B.
  ```

### 最適化ルール
- [**最適化ルールDB**](prolog-system/optimization-rules.md)
  ```prolog
  % 代数的最適化
  algebraic_simplification(binary_op(+, X, literal(0)), X).
  algebraic_simplification(binary_op(*, X, literal(1)), X).
  algebraic_simplification(binary_op(*, _, literal(0)), literal(0)).

  % 制御フローの最適化
  dead_code_elimination(if_stmt(literal(false), _, Else), Else).
  dead_code_elimination(if_stmt(literal(true), Then, _), Then).
  ```

## 🔌 プロトコル定義

### コンパイラプロトコル
- [**コンパイレーションプロトコル**](protocols/compilation-protocol.md)
  ```lisp
  (defgeneric compile-node (node context)
    (:documentation "ASTノードをコンパイル"))

  (defgeneric optimize-ir (ir optimization-level)
    (:documentation "IR最適化"))

  (defgeneric generate-code (ir target)
    (:documentation "コード生成"))
  ```

### 拡張プロトコル
- [**プラグインプロトコル**](protocols/plugin-protocol.md)
  ```lisp
  (defgeneric plugin-initialize (plugin)
    (:documentation "プラグイン初期化"))

  (defgeneric plugin-register-hooks (plugin)
    (:documentation "フック登録"))

  (defgeneric plugin-cleanup (plugin)
    (:documentation "プラグインクリーンアップ"))
  ```

## 📖 使用方法

### 目的別索引

**API使用者向け**:
1. [コンパイラインターフェース](core-api/compiler-interface.md) - 基本的な使用方法
2. [設定管理API](core-api/configuration-api.md) - オプション設定
3. [エラーシステム](core-api/error-system-api.md) - エラーハンドリング

**フロントエンド開発者向け**:
1. [字句解析API](frontend-api/lexer-api.md)
2. [構文解析API](frontend-api/parser-api.md)
3. [意味解析API](frontend-api/semantic-analysis-api.md)

**バックエンド開発者向け**:
1. [コード生成API](backend-api/codegen-api.md)
2. [レジスタ割り当てAPI](backend-api/register-allocation-api.md)
3. [アセンブラAPI](backend-api/assembler-api.md)

**最適化開発者向け**:
1. [最適化フレームワーク](optimization-api/framework-api.md)
2. [データフロー解析](optimization-api/dataflow-api.md)
3. [S式Prolog統合](prolog-system/optimization-rules.md)

### アルファベット順索引

| A-E | F-J | K-O | P-T | U-Z |
|-----|-----|-----|-----|-----|
| [AST](data-structures/ast-specification.md) | [Frontend](clos-hierarchy/frontend-classes.md) | [Lexer](frontend-api/lexer-api.md) | [Parser](frontend-api/parser-api.md) | [Unification](prolog-system/builtin-predicates.md) |
| [Backend](clos-hierarchy/backend-classes.md) | [IR](data-structures/ir-specification.md) | [Macro](macro-system/compiler-macros.md) | [Prolog](prolog-system/inference-rules.md) | [Variables](data-structures/symbol-table.md) |
| [CLOS](clos-hierarchy/base-classes.md) | | [Optimization](optimization-api/framework-api.md) | [Protocol](protocols/compilation-protocol.md) | |
| [Compiler](core-api/compiler-interface.md) | | | [Types](data-structures/type-system.md) | |
| [Error](core-api/error-system-api.md) | | | | |

## 📊 バージョン情報

### APIバージョニング
```lisp
;; APIバージョン情報
(defconstant +cl-cc-api-version+ "1.0.0")
(defconstant +cl-cc-abi-version+ "1.0")

;; 互換性チェック
(defun check-api-compatibility (required-version)
  "APIバージョンの互換性をチェック"
  ...)
```

### 変更履歴追跡
- **v1.0.0**: 初期リリース
- **v1.1.0**: S式Prolog統合
- **v1.2.0**: 高度な最適化フレームワーク
- **v2.0.0**: プラグインシステム導入（破壊的変更）

## 🔍 検索とナビゲーション

### 機能別検索
```lisp
;; 関数検索
(find-function 'compile-program)     ; コンパイラメイン
(find-function 'parse-expression)   ; パーサー
(find-function 'optimize-ir)        ; 最適化

;; クラス検索
(find-class 'ast-node)              ; AST基底クラス
(find-class 'ir-instruction)        ; IR命令
(find-class 'optimization-pass)     ; 最適化パス
```

### クロスリファレンス
各項目は相互に関連付けられており、関連する概念や実装への直接リンクが提供されています。

## ⚠️ 注意事項

### 実装状況
- ✅ **安定**: 仕様確定、実装完了
- 🚧 **開発中**: 仕様確定、実装進行中
- 📋 **計画中**: 仕様検討中
- ❓ **未定**: 将来的な検討項目

### API安定性
- **Core API**: 安定版（破壊的変更は次のメジャーバージョンまでなし）
- **Extension API**: 開発版（マイナーバージョンで変更の可能性）
- **Experimental API**: 実験版（予告なく変更される可能性）

---

*このリファレンスは CL-CC の完全な技術仕様を提供します。実装中に不明な点があれば、該当するセクションを参照して詳細な仕様を確認してください。*