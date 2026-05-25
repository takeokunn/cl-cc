# Architecture

cl-cc は純粋な Common Lisp で実装されたセルフホスティングコンパイラです。ANSI Common Lisp をレジスタベースのバイトコードVMにコンパイルし、そこから x86-64、AArch64、WebAssembly のネイティブコードを生成します。

## コンパイラパイプライン

```
ソースコード (.lisp)
    │
    ▼
┌──────────────────────────────────────┐
│ Lexer + CST Parser                    │  packages/parse
│ ハンドライト、インクリメンタル方式      │
└──────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────┐
│ Macro Expander                        │  packages/expand
│ defstruct→defclass, defconstant→      │
│ defparameter, 標準マクロ展開           │
└──────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────┐
│ AST (CLOS defstruct)                  │  packages/ast
│ ast-defun, ast-let, ast-defclass, ... │
└──────────────────────────────────────┘
    │
    ├──► CPS Transform ──────────────────  packages/cps
    │    (優先されるloweringパス)
    │
    ▼
┌──────────────────────────────────────┐
│ Codegen → VM Bytecode               │  packages/codegen
│ レジスタベース、~220命令種             │  packages/bytecode
└──────────────────────────────────────┘
    │
    ├──► VM Interpreter ────────────────  packages/vm
    │    SBCLホスト上で動作。メタサーキュラーeval
    │
    ├──► MIR / SSA ────────────────────  packages/mir
    │    (Braun et al. 2013)           │  packages/regalloc
    │       │
    │       ├──► x86-64 (Mach-O) ────────  packages/target
    │       ├──► AArch64                 │  packages/emit
    │       └──► WebAssembly (wasm32)
    │
    └──► Bytecode Encoder/Decoder ─────  packages/binary
         ポータブルフォーマット
```

## パッケージ一覧

| パッケージ | 役割 |
|---|---|
| `cl-cc-bootstrap` | ブートストラップ基盤 |
| `cl-cc-parse` | レクサー + CSTパーサー |
| `cl-cc-expand` | マクロ展開 |
| `cl-cc-ast` | AST定義 |
| `cl-cc-cps` | CPS変換 |
| `cl-cc-type` | Hindley-Milner型推論 |
| `cl-cc-optimize` | 最適化パス |
| `cl-cc-prolog` | Prolog最適化バックエンド |
| `cl-cc-ir` | 中間表現 |
| `cl-cc-mir` | MIR / SSA表現 |
| `cl-cc-regalloc` | レジスタ割り付け |
| `cl-cc-codegen` | コード生成 |
| `cl-cc-emit` | ネイティブコード出力 |
| `cl-cc-target` | ターゲットアーキテクチャ |
| `cl-cc-bytecode` | バイトコード定義 |
| `cl-cc-binary` | バイナリフォーマット |
| `cl-cc-vm` | VMインタプリタ |
| `cl-cc-runtime` | ランタイム（GC、FFI、並行処理） |
| `cl-cc-stdlib` | 標準ライブラリ |
| `cl-cc-compile` | コンパイルパイプライン統合 |
| `cl-cc-pipeline` | 高レベルパイプライン |

## 型システム

Hindley-Milner型推論（Algorithm W）を採用。エラー耐性のためのセンチネル回復パスを持ち、条件分岐におけるUnion型の絞り込みを行う。

```
型推論: Algorithm W + error-sentinel recovery
型 narrowing: Union型の条件分岐絞り込み
パラメトリック型: (List T), (Option T)
型クラス: 部分的実装
```

型検査は `cl-cc check <file>` コマンドで実行時とは独立して実行可能。

## ランタイム

### メモリ管理
- **世代別GC**: Young世代（Cheneyセミスペース）+ Old世代（tri-color mark-sweep）
- **SATB write barrier** による並行GCサポート
- **Thread-Local Allocation Buffers (TLAB)** による高速アロケーション
- GCセーフポイント + 正確なスタックマップ
- `storage-condition` によるヒープ圧迫警告（80/90/95%）

### 実行モデル
- ヒープ割り当てクロージャ、consセル、CLOSインスタンス
- スタックオーバーフローガード（`*max-call-stack-depth*`）
- インラインキャッシュ（単相/多相/メガモーフィック）
- Type Feedback Vector (TFV) によるTier-1コンパイルのための実行時型プロファイリング

### 並行処理
グリーンスレッド（work-stealingスケジューラ）、CSPチャネル、アクターモデル、STM、futures/promises、構造化タスクグループ、ファイバー。ロックフリー構造（スタック、キュー、ハッシュマップ、SPSCリングバッファ）および各種同期プリミティブ（mutex、RWLock、セマフォ、condition variable、バリア）を含む。

### FFI
外部関数呼び出し、コールバックトランポリン、ネイティブ構造体レイアウトをサポート。VMインタプリタではSBCLホスト経由のCFFI互換シムを提供。

## 最適化パス

多段構成の最適化パイプライン。各段で異なる表現に対して最適化を適用する。

| 段階 | パス | パッケージ |
|---|---|---|
| 1 | CSE、定数畳み込み、コピー伝播 | `cl-cc-optimize` |
| 2 | デッドコード削除、強度低減、ジャンプスレッディング | `cl-cc-optimize` |
| 3 | Prologルールベース: ピープホール最適化 + e-graphルール発見 | `cl-cc-prolog` |
| 4 | E-graph equality saturation（等式飽和） | `cl-cc-optimize` |
| 5 | CFG構築 + SSA形式変換 + レジスタ割り付け | `cl-cc-mir`, `cl-cc-regalloc` |

バイトコードレベルでの従来型最適化の後に、Prologによる宣言的ルールベースのピープホール最適化とe-graphによる等式飽和を適用する。最終段ではCFGを構築しSSA形式（Braun et al. 2013）に変換した上で、MLベースのレジスタ割り付けを行いネイティブコードを出力する。
