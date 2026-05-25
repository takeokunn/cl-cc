# コントリビューションガイド

cl-cc への貢献に関心を持っていただきありがとうございます。本ドキュメントでは開発環境のセットアップからPR提出までの手順を説明します。

## 開発環境のセットアップ

Nix Flakes を使用して開発環境を構築します。Nix がインストールされていない場合は [Nix のインストールガイド](https://nixos.org/download.html) を参照してください。

```bash
# 開発シェルに入る（SBCL + 全ASDF依存 + cl-cc CLI）
nix develop

# プロジェクトのビルド
nix build --timeout 300

# コードフォーマット（nixfmt + deadnix + statix + prettier）
nix fmt

# CI相当のチェック（flake評価 + ビルド + ユニットテスト）
nix flake check
```

`nix develop` を実行すると、SBCL、cl-cc CLI（`cl-cc` コマンド）、および全ASDFシステムが利用可能な状態になります。

## テスト

```bash
# ユニットテスト（高速プラン）
nix run .#test

# テストタイムアウトの調整（デフォルト: 10秒）
CLCC_TEST_TIMEOUT=30 nix run .#test

# スイート全体のタイムアウト調整（デフォルト: 600秒）
CLCC_SUITE_TIMEOUT=1200 nix run .#test

# FASLキャッシュに起因する問題が発生した場合
rm -rf ~/.cache/common-lisp/ && mkdir -p ~/.cache/common-lisp/
```

`nix run .#test` は `cl-cc/test:run-tests` にマッピングされ、標準の高速ユニットテスト計画を実行します。統合テストおよびセルフホスティングE2Eテストはスイート分類に基づいて明示的に実行します。

## プロジェクト構造

```
cl-cc/
├── flake.nix                  # Nix Flakes エントリポイント
├── cl-cc.asd                  # アンブレラASDFシステム定義
├── cl-cc-test.asd             # テストシステム定義
├── nix/                       # Nixモジュール（ビルド、テスト、devshell）
├── packages/                  # コンパイラサブシステム（全27パッケージ）
│   ├── bootstrap/             # ブートストラップ基盤
│   ├── parse/                 # レクサー + CSTパーサー
│   ├── expand/                # マクロ展開（defstruct→defclass 他）
│   ├── ast/                   # AST定義（CLOS defstruct）
│   ├── cps/                   # CPS変換
│   ├── type/                  # Hindley-Milner型推論
│   ├── optimize/              # 最適化パス（CSE、定数畳み込み 他）
│   ├── prolog/                # Prologバックエンド最適化ルール
│   ├── ir/                    # 中間表現
│   ├── mir/                   # MIR / SSA表現
│   ├── regalloc/              # レジスタ割り付け
│   ├── codegen/               # コード生成
│   ├── emit/                  # ネイティブコード出力
│   ├── target/                # ターゲットアーキテクチャ（x86-64、AArch64、Wasm）
│   ├── bytecode/              # バイトコードエンコーダ/デコーダ
│   ├── binary/                # バイナリフォーマット
│   ├── vm/                    # VMインタプリタ
│   ├── runtime/               # ランタイム（GC、FFI、スレッド 他）
│   ├── stdlib/                # 標準ライブラリ
│   ├── compile/               # コンパイルパイプライン統合
│   ├── pipeline/              # 高レベルパイプライン
│   ├── sb-mop/ / sb-pcl/      # MOP/PCLバックエンド
│   ├── closer-mop/            # Closer to MOP 互換レイヤー
│   └── repl/ / cli/           # REPL / CLI
├── src/                       # アンブレラソース + JIT/FFI ツール
├── tests/                     # テストスイート
└── docs/                      # 設計ドキュメント
```

## PR手順

1. **Issueを作成する**: 作業を始める前に、バグ報告または機能提案のIssueを作成してください。
2. **ブランチを作成する**: `main` から機能ブランチを切ります。ブランチ名は `feat/説明` または `fix/説明` の形式を推奨します。
3. **実装する**: 変更を実装し、テストを追加または更新します。
4. **テストを実行する**: `nix run .#test` で全テストが通過することを確認します。
5. **フォーマットする**: `nix fmt` を実行してコードスタイルを統一します。
6. **コミットする**: コミットメッセージは英語で、`feat:`、`fix:`、`docs:` などのプレフィックスを使用してください。
7. **PRを作成する**: `main` ブランチに対してPull Requestを作成します。PRの説明には変更内容とテスト結果の要約を含めてください。

## コードスタイル

- **言語**: ソースコードとコミットメッセージは英語、ドキュメントは日本語で記述します。
- **Common Lisp**: ANSI Common Lisp規格に準拠したコードを記述します。CLOSの使用を推奨します。
- **フォーマット**: `nix fmt`（Emacs Lisp用のprettier設定 + Nixファイル用のnixfmt/deadnix/statix）で自動整形されます。
- **命名規則**: パッケージプレフィックス `cl-cc/` を使用します（例: `cl-cc/compile`、`cl-cc/vm`）。シンボル名はケバブケース（`make-hash-table`）を使用します。
- **テスト**: 新機能にはテストを追加してください。テストフレームワークは `packages/testing-framework/` にあります。
- **ドキュメント**: 公開APIにはドキュメント文字列を記述してください。アーキテクチャに関する大きな変更は `docs/` 配下の関連ドキュメントを更新してください。
