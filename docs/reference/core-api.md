# CL-CC コアAPIリファレンス

## 📚 概要

CL-CCのコアAPIは、コンパイラ構築の基盤となる包括的な機能セットを提供します。すべてのAPIは、CLOSベースの拡張可能な設計と、S式の表現力を最大限に活用しています。

## 🏗 基本構造

### パッケージ構成

```lisp
cl-cc.core           ; コア機能
cl-cc.ast            ; AST定義と操作
cl-cc.ir             ; 中間表現
cl-cc.optimization   ; 最適化エンジン
cl-cc.codegen        ; コード生成
cl-cc.prolog         ; S式Prolog統合
cl-cc.pbt            ; Property-Based Testing
cl-cc.frontend       ; 言語フロントエンド
cl-cc.backend        ; バックエンド
```

## 🎯 コンパイラ定義API

### `defcompiler` マクロ

新しいコンパイラを定義する最上位マクロ。

```lisp
(defcompiler name (&rest options) &body phases)
```

#### パラメータ

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `name` | symbol | コンパイラの名前 |
| `options` | plist | オプション設定 |
| `phases` | forms | コンパイルフェーズの定義 |

#### オプション

| オプション | デフォルト | 説明 |
|-----------|------------|------|
| `:source-language` | `:lisp` | ソース言語 |
| `:target-language` | `:native` | ターゲット言語 |
| `:optimization-level` | `2` | 最適化レベル (0-3) |
| `:debug` | `nil` | デバッグ情報生成 |
| `:parallel` | `t` | 並列コンパイル有効化 |
| `:verification` | `nil` | 形式検証の有効化 |

#### 使用例

```lisp
(defcompiler my-optimizing-compiler
    (:source-language :lisp
     :target-language :wasm
     :optimization-level 3
     :verification t)

  ;; レクサーフェーズ
  (phase :lexer
    (tokenize source))

  ;; パーサーフェーズ
  (phase :parser
    (parse-with-prolog tokens))

  ;; 意味解析フェーズ
  (phase :semantic
    (analyze-types ast)
    (check-constraints ast))

  ;; 最適化フェーズ
  (phase :optimization
    (apply-optimization-pipeline ir))

  ;; コード生成フェーズ
  (phase :codegen
    (generate-wasm-module ir)))
```

### `compile-with` 関数

指定されたコンパイラでソースをコンパイル。

```lisp
(compile-with compiler source &key output-file options)
```

#### パラメータ

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `compiler` | compiler | コンパイラインスタンス |
| `source` | string/pathname/stream | ソースコード |
| `output-file` | pathname | 出力ファイル |
| `options` | plist | 追加オプション |

#### 戻り値

コンパイル結果を表す`compilation-result`オブジェクト。

```lisp
(defclass compilation-result ()
  ((success-p :reader compilation-success-p)
   (output :reader compilation-output)
   (warnings :reader compilation-warnings)
   (errors :reader compilation-errors)
   (statistics :reader compilation-statistics)))
```

## 🌳 AST API

### AST ノードクラス階層

```
ast-node
├── expression-node
│   ├── literal-node
│   │   ├── number-literal
│   │   ├── string-literal
│   │   └── boolean-literal
│   ├── variable-node
│   ├── binary-op-node
│   ├── unary-op-node
│   └── call-node
├── statement-node
│   ├── if-statement
│   ├── loop-statement
│   └── return-statement
└── declaration-node
    ├── function-declaration
    ├── variable-declaration
    └── type-declaration
```

### `make-ast` 総称関数

S式からASTを構築。

```lisp
(defgeneric make-ast (expr &key context)
  (:documentation "式からASTノードを作成"))
```

#### メソッド

```lisp
;; 数値リテラル
(defmethod make-ast ((expr number) &key context)
  (make-instance 'number-literal :value expr))

;; リスト（関数呼び出しまたは特殊形式）
(defmethod make-ast ((expr cons) &key context)
  (parse-compound-expression expr context))

;; カスタム拡張用
(defmethod make-ast ((expr my-custom-type) &key context)
  ...)
```

### `walk-ast` マクロ

ASTを走査して変換を適用。

```lisp
(walk-ast ast
  (:enter node
    ;; ノードに入る時の処理
    )
  (:leave node
    ;; ノードから出る時の処理
    ))
```

#### 使用例

```lisp
(walk-ast my-ast
  (:enter (node :type binary-op-node)
    (when (constant-operands-p node)
      (mark-for-folding node)))
  (:leave (node :type function-declaration)
    (register-function node)))
```

## 🔄 中間表現（IR）API

### IR命令セット

```lisp
;; 基本命令
(make-ir-move dest src)           ; 移動
(make-ir-load-const dest value)   ; 定数ロード
(make-ir-binary-op op dest src1 src2) ; 二項演算
(make-ir-unary-op op dest src)    ; 単項演算
(make-ir-call dest func args)     ; 関数呼び出し
(make-ir-return value)             ; リターン

;; 制御フロー
(make-ir-jump label)               ; 無条件ジャンプ
(make-ir-branch cond true-label false-label) ; 条件分岐
(make-ir-label name)               ; ラベル

;; メモリ操作
(make-ir-load dest addr)          ; メモリロード
(make-ir-store addr value)        ; メモリストア
(make-ir-alloca dest size)        ; スタック割り当て

;; SSA形式
(make-ir-phi dest pairs)          ; φ関数
```

### `to-ssa` 関数

IRをSSA形式に変換。

```lisp
(to-ssa ir-module &key simplify)
```

#### パラメータ

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `ir-module` | ir-module | 変換対象のIRモジュール |
| `simplify` | boolean | 簡約化を行うか |

#### 使用例

```lisp
(let ((ssa-ir (to-ssa my-ir :simplify t)))
  (verify-ssa-properties ssa-ir))
```

## ⚡ 最適化API

### `define-optimization` マクロ

新しい最適化パスを定義。

```lisp
(define-optimization name (ir-node context)
  &body body)
```

#### 使用例

```lisp
(define-optimization eliminate-common-subexpressions (ir context)
  "共通部分式を除去"
  (let ((expr-map (build-expression-map ir)))
    (do-instructions (inst ir)
      (when-let ((equiv (find-equivalent inst expr-map)))
        (replace-instruction inst (make-ir-move (dest inst) (dest equiv)))))))

(define-optimization strength-reduction (ir context)
  "演算の強度削減"
  (do-instructions (inst ir)
    (when (and (ir-binary-op-p inst)
               (eq (ir-op inst) '*)
               (power-of-two-p (ir-src2 inst)))
      (replace-with-shift inst))))
```

### `optimization-pipeline` クラス

最適化パイプラインを管理。

```lisp
(defclass optimization-pipeline ()
  ((passes :initform '())
   (level :initform 2)
   (iterations :initform 1)))

(make-optimization-pipeline
  :level 3
  :passes '(constant-folding
            dead-code-elimination
            loop-invariant-code-motion
            vectorization))
```

## 🧬 S式Prolog API

### `defrel` マクロ

Prolog風の関係を定義。

```lisp
(defrel relation-name
  (clause1)
  (clause2 :- body)
  ...)
```

#### 使用例

```lisp
;; 型推論ルール
(defrel type-of
  ((type-of ?n integer) :- (integerp ?n))
  ((type-of ?s string) :- (stringp ?s))
  ((type-of (+ ?x ?y) integer) :-
   (type-of ?x integer)
   (type-of ?y integer))
  ((type-of (if ?c ?t ?e) ?type) :-
   (type-of ?c boolean)
   (type-of ?t ?type)
   (type-of ?e ?type)))

;; クエリ実行
(query (type-of (+ 1 2) ?type))
; => ((?TYPE . INTEGER))
```

### `unify` 関数

単一化を実行。

```lisp
(unify pattern1 pattern2 &optional bindings)
```

#### パラメータ

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `pattern1` | any | 第1パターン |
| `pattern2` | any | 第2パターン |
| `bindings` | alist | 既存の束縛 |

## 🧪 Property-Based Testing API

### `defproperty` マクロ

プロパティテストを定義。

```lisp
(defproperty property-name (generators)
  &body test-body)
```

#### 使用例

```lisp
(defproperty associativity-of-addition
  ((x (gen-integer))
   (y (gen-integer))
   (z (gen-integer)))
  (= (+ (+ x y) z)
     (+ x (+ y z))))

(defproperty compiler-preserves-semantics
  ((program (gen-valid-program)))
  (equal (interpret program)
         (execute (compile program))))
```

### ジェネレータAPI

```lisp
;; 基本ジェネレータ
(gen-integer &key min max)
(gen-float &key min max)
(gen-string &key max-length alphabet)
(gen-boolean)
(gen-list generator &key min-length max-length)
(gen-one-of &rest generators)
(gen-frequency &rest freq-gen-pairs)
(gen-such-that generator predicate)

;; AST生成用
(gen-expression &key depth)
(gen-statement &key complexity)
(gen-program &key size)
```

## 🎨 コード生成API

### `emit` 総称関数

IR命令からターゲットコードを生成。

```lisp
(defgeneric emit (instruction backend)
  (:documentation "命令をターゲットコードに変換"))
```

#### バックエンド実装例

```lisp
(defclass wasm-backend (backend) ())

(defmethod emit ((inst ir-binary-op) (backend wasm-backend))
  (case (ir-op inst)
    (+ `(i32.add))
    (- `(i32.sub))
    (* `(i32.mul))
    (/ `(i32.div_s))))

(defmethod emit ((inst ir-load-const) (backend wasm-backend))
  `(i32.const ,(ir-value inst)))
```

## 🔍 解析API

### データフロー解析

```lisp
(defgeneric analyze-dataflow (ir-module analysis-type)
  (:documentation "データフロー解析を実行"))

;; 到達定義解析
(analyze-dataflow module :reaching-definitions)

;; 活性解析
(analyze-dataflow module :liveness)

;; 利用可能式解析
(analyze-dataflow module :available-expressions)
```

### 制御フロー解析

```lisp
(build-cfg ir-module)           ; CFG構築
(find-dominators cfg)           ; 支配木構築
(detect-loops cfg)              ; ループ検出
(compute-loop-depth cfg)        ; ループ深度計算
```

## 🛠 ユーティリティAPI

### エラー処理

```lisp
(define-compiler-condition condition-name (parent-condition)
  ((slot :initarg :slot)))

(with-compiler-error-handling ()
  ;; コンパイル処理
  )

(compiler-error format-string &rest args)
(compiler-warning format-string &rest args)
```

### 統計とプロファイリング

```lisp
(with-compilation-statistics ()
  ;; コンパイル処理
  )

(get-compilation-statistics) ; 統計情報取得
(profile-compilation expr)   ; プロファイル実行
```

## 📋 マクロ定義API

### `defcompiler-macro` マクロ

コンパイラマクロを定義。

```lisp
(defcompiler-macro macro-name (args)
  &body expansion)
```

#### 使用例

```lisp
(defcompiler-macro inline-arithmetic (op x y)
  "算術演算をインライン展開"
  (if (and (constantp x) (constantp y))
      (funcall op x y)  ; コンパイル時に評価
      `(,op ,x ,y)))     ; 通常の展開
```

## 🔄 バージョン互換性

| APIバージョン | CL-CCバージョン | 変更内容 |
|---------------|----------------|----------|
| 1.0.0 | 1.0.0 | 初期リリース |
| 1.1.0 | 1.1.0 | S式Prolog統合 |
| 1.2.0 | 1.2.0 | PBT API追加 |
| 2.0.0 | 2.0.0 | 並列コンパイル対応 |

## 🔗 関連ドキュメント

- [IRリファレンス](./ir-specification.md)
- [最適化パスカタログ](./optimization-passes.md)
- [S式Prolog述語リファレンス](./prolog-predicates.md)
- [バックエンドAPI](./backend-api.md)

---

*このリファレンスはCL-CC v2.0.0に対応しています。*