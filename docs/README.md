# CL-CC ドキュメント: Common Lisp コンパイラコレクション

## 🌟 プロジェクトビジョン

**CL-CC (Common Lisp Compiler Collection)** は、Common Lispを使用して構築されるコンパイラコレクションです。マクロ駆動開発、CLOS の活用、S式Prolog統合により、柔軟なコンパイラシステムを実現します。

### 🎯 核心的特徴

- **🧠 マクロ駆動開発**: コンパイル時計算により言語の境界を超越
- **🏗️ CLOS 活用**: 多重ディスパッチによる柔軟な拡張性
- **⚡ 効率的設計**: 理論と実装の融合
- **🔬 S式Prolog統合**: 論理推論による最適化の自動発見
- **🛡️ 数学的品質保証**: Property-Based Testing による証明レベルの信頼性
- **🌐 高い拡張性**: プラグインアーキテクチャによる柔軟な拡張

## 📚 ドキュメント構成（Diátaxis Framework）

このドキュメントは [Diátaxis Framework](https://diataxis.fr/) に基づいて体系化されており、学習者のニーズに応じて最適な情報を提供します。

### 🎓 [チュートリアル](tutorials/README.md) - 学習指向
**「基礎から実用技術習得まで」**

段階的な学習プロセスにより、CL-CCの技術を確実に習得できます。

- **第1章**: [最小コンパイラの実装](tutorials/01-first-compiler.md) - S式とCLOSの威力を体験
- **第2章**: [マクロシステムの理解](tutorials/02-macro-system.md) - コンパイル時計算の極致
- **第3章**: [CLOSによるアーキテクチャ設計](tutorials/03-clos-extension.md) - 高い拡張性の実現
- **第4章**: [S式Prologの統合](tutorials/04-prolog-integration.md) - 論理推論の活用
- **第5章**: [最適化パイプライン](tutorials/05-advanced-compiler.md) - プロダクション品質の実現
- **第6章**: [テスト駆動開発](tutorials/06-test-driven-development.md) - 数学的品質保証

**学習時間**: 初級20-30時間、中級30-40時間、上級15-20時間

### 🔧 [ハウツーガイド](how-to/README.md) - 問題解決指向
**「具体的な実装課題の解決手法」**

実践的な問題解決のためのステップバイステップガイド集です。

#### フロントエンド開発
- [新言語フロントエンドの追加](how-to/add-language-frontend.md)
- [字句解析器の実装](how-to/implement-lexer.md)
- [構文解析器の実装](how-to/implement-parser.md)
- [意味解析の実装](how-to/semantic-analysis.md)

#### 最適化とバックエンド
- [最適化パスの実装](how-to/implement-optimization-pass.md)
- [データフロー解析](how-to/dataflow-analysis.md)
- [新バックエンドの追加](how-to/add-backend-target.md)
- [レジスタ割り当て](how-to/register-allocation.md)

#### 品質保証
- [プロパティベーステスト](how-to/write-property-tests.md)
- [パフォーマンステスト](how-to/performance-testing.md)
- [デバッグとプロファイリング](how-to/debugging-profiling.md)

### 📖 [リファレンス](reference/README.md) - 情報指向
**「完全な技術仕様書」**

開発者向けの包括的な技術仕様とAPI詳細です。

#### コアAPI
- [コンパイラインターフェース](reference/core-api/compiler-interface.md)
- [フロントエンドAPI](reference/frontend-api/)
- [バックエンドAPI](reference/backend-api/)
- [最適化API](reference/optimization-api/)

#### データ構造
- [AST仕様](reference/data-structures/ast-specification.md)
- [IR仕様](reference/data-structures/ir-specification.md)
- [型システム](reference/data-structures/type-system.md)
- [シンボルテーブル](reference/data-structures/symbol-table.md)

#### 専門システム
- [マクロシステム](reference/macro-system/)
- [CLOSクラス階層](reference/clos-hierarchy/)
- [S式Prologシステム](reference/prolog-system/)

### 💡 [説明](explanation/README.md) - 理解指向
**「設計思想と理論的背景の深層理解」**

CL-CCのアプローチの「なぜ」を深く理解するための解説集です。

#### 設計哲学
- [なぜCommon Lispか](explanation/philosophy/why-common-lisp.md) - 言語選択の必然性
- [マクロ駆動開発の哲学](explanation/philosophy/macro-driven-philosophy.md) - コンパイル時計算の真価
- [CLOSの本質的価値](explanation/philosophy/clos-utilization.md) - 多重ディスパッチの威力
- [S式Prologの必然性](explanation/philosophy/prolog-necessity.md) - 論理推論の革新性

#### アーキテクチャ理論
- [全体設計思想](explanation/architecture/architecture-overview.md)
- [最適化戦略の理論](explanation/optimization-theory/optimization-strategy.md)
- [品質保証の数学的基盤](explanation/quality-assurance/pbt-foundations.md)
- [パフォーマンス原理](explanation/performance-principles/performance-principles.md)

## 🚀 クイックスタート

```lisp
;; CL-CCのロード
(ql:quickload :cl-cc)

;; 簡単なコンパイラの作成
(defcompiler my-compiler
  :frontend :lisp
  :backend :llvm
  :optimizations (:inline :constant-fold :dead-code))

;; コンパイル実行
(compile-file "source.lisp" :compiler my-compiler)
```

## 🎯 プロジェクトの特徴

- **📦 マクロ駆動**: Common Lispのマクロシステムを活用
- **🏗️ CLOS基盤**: 完全なオブジェクト指向による拡張可能設計
- **⚡ 効率的設計**: 効率的なコンパイル速度と実行効率
- **🔬 厳密なテスト**: Property-Based TestingとTDDの徹底
- **🧩 S式Prolog**: 論理型プログラミングによる最適化推論
- **🛠️ モジュラー設計**: フロントエンド/バックエンドの完全な分離

## 📊 ドキュメントマトリックス

| 側面 | チュートリアル | ハウツー | リファレンス | 説明 |
|------|----------------|----------|--------------|------|
| **方向性** | 学習 | ゴール | 情報 | 理解 |
| **形式** | レッスン | レシピ | 辞書 | 議論 |
| **読者** | 初心者 | 実践者 | 参照者 | 理解者 |

## 🔍 検索とナビゲーション

- [サイトマップ](sitemap.md) - 全ドキュメントの階層構造
- [用語集](glossary.md) - 専門用語の定義
- [索引](index.md) - アルファベット順の項目リスト
- [FAQ](faq.md) - よくある質問と回答

## 📝 貢献ガイドライン

ドキュメントの改善に貢献する場合は、[貢献ガイド](CONTRIBUTING.md)を参照してください。

## 🏷️ バージョン情報

- **現在のバージョン**: 0.1.0
- **ドキュメント更新**: 継続的更新
- **互換性**: Common Lisp標準準拠

---

*このドキュメントはDiátaxisフレームワークに基づいて構成されています。詳細は[diataxis.fr](https://diataxis.fr/)を参照してください。*