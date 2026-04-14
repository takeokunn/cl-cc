# CL-CC: Common Lisp Compiler Collection

## プロジェクト概要

CL-CCは純粋なCommon Lispで実装された自己ホスティングコンパイラコレクションです。外部のCコードやFFIに依存せず、ANSI Common LispをレジスタベースのバイトコードVMにコンパイルし、さらにx86-64、AArch64、WebAssemblyへのネイティブコード生成をサポートしています。

### 設計目標

1. **自己ホスティング**: コンパイラ自体がコンパイルする言語機能を使用して実装
2. **マルチターゲット**: x86-64、AArch64、WebAssembly、LLVM IRへのコード生成
3. **CLOSベース**: Common Lisp Object Systemによる拡張可能なコンパイラインフラストラクチャ
4. **Prolog統合**: S式Prologによる論理推論と最適化
5. **プロパティベーステスト**: 数学的正確性の保証

## アーキテクチャ

```
ソース (.lisp)
    │
    ▼
Lexer + CST Parser (ハンドライト、インクリメンタル)
    │
    ▼
Macro Expander (defstruct→defclass, defconstant→defparameter等)
    │
    ▼
AST (CLOS defstructs: ast-defun, ast-let, ast-defclass, …)
    │
    ▼
CPS Transform (オプション)
    │
    ▼
Codegen → VM Bytecode
    │       (レジスタベース、~220命令種)
    │
    ├──► VM Interpreter  (SBCLホスト、メタ循環eval)
    │
    ├──► MIR / SSA  (Braun et al. 2013)
    │       │
    │       ├──► x86-64 アセンブリテキスト / Mach-O バイナリ
    │       ├──► AArch64 アセンブリテキスト
    │       └──► WebAssembly (wasm32)
    │
    └──► Bytecode encoder/decoder (ポータブルフォーマット)
```

## 主要コンポーネント

### 1. パーサ (src/parse/)

| ファイル | 機能 |
|---------|------|
| `cst.lisp` | Concrete Syntax Tree - 位置情報を保持する構文木 |
| `lexer.lisp` | 字句解析器 |
| `ast.lisp` | Abstract Syntax Tree基底クラス |
| `prolog.lisp` | S式Prologエンジン |
| `dcg.lisp` | Definite Clause Grammar |
| `pratt.lisp` | Prattパーサ (演算子優先順位) |
| `cl/parser.lisp` | Common Lispパーサ |
| `cl/lower.lisp` | CSTからASTへの変換 |

### 2. マクロ展開 (src/expand/)

| ファイル | 機能 |
|---------|------|
| `macro.lisp` | マクロ環境、defmacro機構 |
| `expander.lisp` | フォーム展開ディスパッチ |
| `loop.lisp` | LOOPマクロ (CPSベース) |
| `expander-defstruct.lisp` | defstruct展開ヘルパー |
| `macros-*.lisp` | 標準マクロ定義 |

### 3. VM (src/vm/)

| ファイル | 機能 |
|---------|------|
| `vm.lisp` | VM状態管理 |
| `vm-instructions.lisp` | ~220命令の定義 |
| `vm-execute.lisp` | 命令実行 |
| `vm-clos.lisp` | CLOS命令 |
| `vm-dispatch.lisp` | ジェネリック関数ディスパッチ |
| `list.lisp` | リスト操作 |
| `hash.lisp` | ハッシュテーブル |
| `format.lisp` | format指令 |

### 4. 型システム (src/type/)

| ファイル | 機能 |
|---------|------|
| `types-core.lisp` | 基本型定義 |
| `inference.lisp` | Hindley-Milner型推論 (Algorithm W) |
| `unification.lisp` | 型単一化 |
| `subtyping.lisp` | サブタイピング関係 |
| `solver.lisp` | 制約解決 |
| `effect.lisp` | 効果システム |
| `typeclass.lisp` | 型クラス |

### 5. コンパイラ (src/compile/)

| ファイル | 機能 |
|---------|------|
| `pipeline.lisp` | コンパイルパイプライン |
| `codegen.lisp` | AST→VM命令コンパイラ |
| `cps.lisp` | CPS変換 |
| `closure.lisp` | クロージャ変換 |
| `context.lisp` | コンパイルコンテキスト |
| `ir/*.lisp` | 中間表現 (SSA) |

### 6. オプティマイザ (src/optimize/)

| ファイル | 機能 |
|---------|------|
| `cfg.lisp` | CFG構築 + 支配木 |
| `ssa.lisp` | SSA構築 + 破棄 |
| `egraph.lisp` | E-graphエンジン (union-find, saturation) |
| `egraph-rules.lisp` | 書き換え規則 |
| `optimizer.lisp` | 最適化パス |

### 7. バックエンド (src/emit/)

| ファイル | 機能 |
|---------|------|
| `mir.lisp` | 中間表現 (MIR) |
| `x86-64.lisp` | x86-64 コード生成 |
| `aarch64.lisp` | AArch64 コード生成 |
| `wasm.lisp` | WebAssembly生成 |
| `regalloc.lisp` | レジスタ割り当て |
| `binary/*.lisp` | Mach-O, ELF, WASM バイナリ生成 |

## 実装状態

### 完了 (100%)

| カテゴリ | 内容 |
|---------|------|
| **パッケージシステム** | 8パッケージ (:cl-cc, :cl-cc/ast, :cl-cc/ir, etc.) |
| **パーサ** | 字句解析、CST、AST、再帰下降パーサ |
| **マクロシステム** | defmacro, macrolet, LOOP, defstruct→defclass |
| **VM命令** | ~220命令 (レジスタベース) |
| **CLOS** | defclass, defgeneric, defmethod, make-instance |
| **型推論** | Hindley-Milner (Algorithm W) |
| **最適化** | CFG, SSA, E-graph |
| **ネイティブ生成** | x86-64, AArch64, WebAssembly |
| **テスト** | 2027テストケース、0失敗 |

### 言語サポート

| 機能 | 状態 |
|-----|------|
| `if`, `progn`, `block`/`return-from` | ✓ |
| `tagbody`/`go` | ✓ |
| `catch`/`throw`, `unwind-protect` | ✓ |
| `let`, `let*`, `setq`, `setf` | ✓ |
| `flet`, `labels` (相互再帰) | ✓ |
| `lambda`, `defun`, `defvar`, `defparameter` | ✓ |
| `defmacro`, `macrolet` | ✓ |
| `quote`, `the`, `values` | ✓ |
| `multiple-value-bind`, `multiple-value-call` | ✓ |
| `eval-when` | ✓ |
| CLOS (defclass, defgeneric, defmethod) | ✓ |
| LOOP | ✓ |

## セルフホスティング

CL-CCはメタ循環的な意味で自己ホスティングです:

1. **メタ循環`eval`**: `(eval '...)` はホストSBCLではなくCL-CCのコンパイルパイプラインを呼び出し
2. **マクロ展開**: `defmacro`展開はCL-CC自体がバイトコードをコンパイル・実行
3. **コンパイラの中のコンパイラ**: CL-CCはパーサ、コンパイラ、評価器を実装するプログラムをコンパイル可能
4. **REPL状態永続化**: 定義は`run-string-repl`呼び出し間で永続化

### セルフホスティング例

```lisp
;; Mini ASTコンパイラ
(defstruct ast-node kind value children)

(defun parse-expr (sexp)
  (cond
    ((integerp sexp)
     (make-ast-node :kind :lit :value sexp :children nil))
    ((and (consp sexp) (eq (car sexp) '+))
     (make-ast-node :kind :add :value nil
       :children (mapcar #'parse-expr (cdr sexp))))))

(defun eval-ast (node)
  (case (ast-node-kind node)
    (:lit  (ast-node-value node))
    (:add  (apply #'+ (mapcar #'eval-ast (ast-node-children node))))))

(eval-ast (parse-expr '(+ 1 (+ 2 (+ 3 4)))))  ; => 10
```

## テストフレームワーク

### テスト構造

```
tests/
├── framework/           # テストインフラ
│   ├── framework.lisp   # 基本テストマクロ
│   ├── framework-compiler.lisp
│   └── framework-fuzz.lisp
├── unit/               # ユニットテスト
│   ├── vm/
│   ├── compile/
│   ├── type/
│   └── emit/
├── integration/        # 統合テスト
│   ├── compiler-tests.lisp
│   ├── clos-tests.lisp
│   └── pbt/           # integration 内のプロパティベーステスト
└── e2e/               # end-to-end / self-hosting tests
    └── selfhost-tests.lisp
```

### テスト実行

```bash
# canonical test entrypoint
make test

# 特定suiteだけ手動実行
nix run nixpkgs#sbcl -- --load cl-cc.asd \
  --eval '(ql:quickload :cl-cc/test)' \
  --eval '(cl-cc/test:run-tests)'                  ;; canonical plan
# or
  --eval '(cl-cc/test:run-suite (quote selfhost-suite))'  ;; 個別
```

ハング箇所を診断したい場合は `CLCC_TEST_TRACE=1 make test` で
各テスト名が `*error-output*` に出力されます。

## CLIリファレンス

```
cl-cc run <file>          Compile and run a .lisp file
cl-cc compile <file>      Compile to native Mach-O binary
  --arch x86-64|arm64
  -o <output>
cl-cc eval "<expr>"       Evaluate a single expression
cl-cc repl                Interactive REPL (definitions persist)
  --stdlib                Include higher-order function library
cl-cc check <file>        Type-check without executing
  --strict                Treat type warnings as errors
```

## 既知の制限

| 項目 | 制限 |
|-----|------|
| パッケージシステム | `defpackage`/`in-package`/`export`はランタイムでno-op。全シンボルが`:cl-cc`名前空間を共有 |
| Restarts | `restart-case`, `abort`, `continue`等はスタブマクロ |
| `format`指令 | VMインタプリタではホストSBCLに委譲。ネイティブバイナリでは利用不可 |
| 数値タワー | `bignum`, `ratio`, `complex`はVMで動作。ネイティブx86-64バックエンドではfixnumのみ |
| ストリーム | ファイルハンドルは整数。ファーストクラスストリームオブジェクトは未実装 |
| `load`/`compile-file` | 未実装。`cl-cc run <file>`を使用 |

## コード規約

### 命名規約

| 種類 | 規約 | 例 |
|-----|------|-----|
| パッケージ | 小文字+ハイフン | `:cl-cc/ast`, `:cl-cc/ir` |
| クラス | 小文字+ハイフン | `ast-node`, `compiler` |
| 関数 | 小文字+ハイフン | `make-literal`, `tokenize` |
| 定数 | `+`でラップ | `+default-lexer+`, `+log-level+` |
| 特殊変数 | `*`でラップ | `*log-level*`, `*default-compiler*` |
| 述語 | `-p`接尾辞 | `null-p`, `string-contains-p` |

### フォーマット

- **インデント**: 2スペース
- **行長**: 100文字推奨
- **コメント**: 単一`;`はインライン、二重`;;`はセクションヘッダ
- **docstring**: 全公開関数/クラスにドキュメント

### 設計原則

1. **単一責任**: 各コンポーネントは1つの明確な目的を持つ
2. **開放閉鎖**: 拡張に対しては開き、修正に対しては閉じる
3. **依存性逆転**: 具象ではなく抽象に依存
4. **インターフェース分離**: 小さく焦点を絞ったインターフェース

## 開発環境

### セットアップ

```bash
# 開発環境に入る
nix develop

# SBCL REPL起動
sbcl

# 履歴付きREPL
rlwrap sbcl

# プロジェクト読み込み
(ql:quickload :cl-cc)
(in-package :cl-cc)
```

### 依存関係

- **主要言語**: Common Lisp (SBCL 2.3.0+)
- **ビルドシステム**: ASDF
- **パッケージマネージャ**: Quicklisp
- **テストフレームワーク**: in-repo custom framework + プロパティベーステスト
- **開発環境**: Nix Flakes

### 外部依存

- alexandria (ユーティリティ)
- trivial-types (型ユーティリティ)
- closer-mop (MOP拡張)

## 今後の展望

### 短期目標

1. パッケージシステムの完全実装
2. `load`/`compile-file`の実装
3. ストリームオブジェクトの完全サポート

### 中期目標

1. C言語フロントエンド
2. LLVM IR バックエンド
3. S式Prolog統合の深化

### 長期目標

1. フル自己コンパイル (bootstrap)
2. 追加ターゲット (RISC-V, MIPS)
3. インクリメンタルコンパイル
