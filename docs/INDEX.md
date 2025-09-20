# CL-CC: Common Lisp Compiler Collection - マスターインデックス

## 🎯 プロジェクトビジョン

**コンパイラコレクション** を Common Lisp で実装し、以下を実現：

- **高性能**: 効率的な最適化技術
- **高い拡張性**: CLOSによる柔軟な拡張可能性
- **完全な抽象化**: マクロによる言語の境界を超えた表現力
- **論理的推論**: S式Prologによる最適化の自動導出
- **厳密な品質保証**: PBT/TDDによる数学的正確性

## 📚 ドキュメント構成（Diátaxis）

### 🎓 [チュートリアル](tutorials/README.md)
**学習指向** - 基礎から応用まで段階的に学ぶ

1. [第1章: 最小コンパイラの実装](tutorials/01-first-compiler.md)
2. [第2章: マクロシステムの理解](tutorials/02-macro-system.md)
3. [第3章: CLOSによるアーキテクチャ設計](tutorials/03-clos-extension.md)
4. [第4章: S式Prologの統合](tutorials/04-prolog-integration.md)
5. [第5章: 最適化パイプライン](tutorials/05-optimization-pipeline.md)
6. [第6章: テスト駆動開発](tutorials/06-test-driven-development.md)

### 🔧 [ハウツーガイド](how-to/README.md)
**タスク指向** - 具体的な実装方法

- **フロントエンド開発**
  - [新言語フロントエンドの追加](how-to/add-language-frontend.md)
  - [字句解析器の実装](how-to/implement-lexer.md)
  - [構文解析器の実装](how-to/implement-parser.md)
  - [意味解析の実装](how-to/semantic-analysis.md)

- **中間表現と最適化**
  - [IRノードの定義](how-to/define-ir-nodes.md)
  - [最適化パスの実装](how-to/implement-optimization-pass.md)
  - [データフロー解析](how-to/dataflow-analysis.md)
  - [SSA変換の実装](how-to/ssa-transformation.md)

- **バックエンド開発**
  - [新バックエンドの追加](how-to/add-backend-target.md)
  - [レジスタ割り当て](how-to/register-allocation.md)
  - [コード生成](how-to/code-generation.md)

- **品質保証**
  - [プロパティベーステスト](how-to/write-property-tests.md)
  - [パフォーマンステスト](how-to/performance-testing.md)
  - [デバッグとプロファイリング](how-to/debugging-profiling.md)

### 📖 [リファレンス](reference/README.md)
**情報指向** - 完全な技術仕様

- **コアAPI**
  - [コンパイラインターフェース](reference/compiler-interface.md)
  - [フロントエンドAPI](reference/frontend-api.md)
  - [バックエンドAPI](reference/backend-api.md)
  - [最適化API](reference/optimization-api.md)

- **データ構造**
  - [AST仕様](reference/ast-specification.md)
  - [IR仕様](reference/ir-specification.md)
  - [シンボルテーブル](reference/symbol-table.md)
  - [型システム](reference/type-system.md)

- **マクロシステム**
  - [コンパイラマクロ](reference/compiler-macros.md)
  - [DSL定義マクロ](reference/dsl-macros.md)
  - [最適化マクロ](reference/optimization-macros.md)

- **CLOSクラス階層**
  - [基底クラス](reference/base-classes.md)
  - [フロントエンドクラス](reference/frontend-classes.md)
  - [バックエンドクラス](reference/backend-classes.md)
  - [最適化クラス](reference/optimization-classes.md)

- **S式Prolog**
  - [述語リファレンス](reference/prolog-predicates.md)
  - [推論規則](reference/inference-rules.md)
  - [最適化ルール](reference/optimization-rules.md)

### 💡 [説明](explanation/README.md)
**理解指向** - 設計思想と理論的背景

- **アーキテクチャ**
  - [全体設計](explanation/architecture-overview.md)
  - [モジュール構成](explanation/module-structure.md)
  - [データフロー](explanation/data-flow.md)
  - [制御フロー](explanation/control-flow.md)

- **設計哲学**
  - [なぜCommon Lispか](explanation/why-common-lisp.md)
  - [マクロ駆動開発](explanation/macro-driven-philosophy.md)
  - [CLOSの活用](explanation/clos-utilization.md)
  - [S式Prologの必要性](explanation/prolog-necessity.md)

- **最適化理論**
  - [最適化戦略](explanation/optimization-strategy.md)
  - [パフォーマンス原理](explanation/performance-principles.md)
  - [メモリ管理](explanation/memory-management.md)
  - [並列化戦略](explanation/parallelization.md)

- **品質保証理論**
  - [PBTの数学的基礎](explanation/pbt-foundations.md)
  - [TDDの実践](explanation/tdd-practice.md)
  - [形式的検証](explanation/formal-verification.md)

## 🗺️ ナビゲーション

### 役割別ガイド
- **👨‍🎓 学習者**: [チュートリアル](tutorials/README.md) → [説明](explanation/README.md)
- **👨‍💻 開発者**: [ハウツー](how-to/README.md) → [リファレンス](reference/README.md)
- **🏗️ アーキテクト**: [説明](explanation/README.md) → [リファレンス](reference/README.md)
- **🔬 研究者**: [説明](explanation/README.md) → [チュートリアル](tutorials/README.md)

### トピック別インデックス
- [A-Z インデックス](alphabetical-index.md)
- [概念マップ](concept-map.md)
- [依存関係グラフ](dependency-graph.md)
- [用語集](glossary.md)
- [略語集](abbreviations.md)

## 📊 プロジェクトステータス

### 実装状況
| コンポーネント | 状態 | 完成度 | 備考 |
|---------------|------|--------|------|
| コアフレームワーク | 🟢 設計中 | 0% | アーキテクチャ定義 |
| フロントエンド | 🔴 未着手 | 0% | 言語パーサー |
| 中間表現 | 🔴 未着手 | 0% | IR設計 |
| 最適化 | 🔴 未着手 | 0% | 最適化パス |
| バックエンド | 🔴 未着手 | 0% | コード生成 |
| テスト | 🟢 設計中 | 0% | PBT/TDD |

### ロードマップ
1. **Phase 1**: コアアーキテクチャとCLOS基盤
2. **Phase 2**: 最小限のコンパイラ実装
3. **Phase 3**: 最適化フレームワーク
4. **Phase 4**: S式Prolog統合
5. **Phase 5**: 最適化と並列化

## 🔗 関連リソース

### 内部リンク
- [アーキテクチャ詳細](ARCHITECTURE.md)
- [貢献ガイド](CONTRIBUTING.md)
- [変更履歴](CHANGELOG.md)
- [ライセンス](LICENSE.md)

### 新着ドキュメント
- [**完全APIリファレンス**](reference/complete-api-reference.md) - 全API関数の詳細仕様
- [**理論的基盤**](explanation/theoretical-foundations.md) - 数学的・理論的基盤の解説

### 外部リソース
- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/)
- [Diátaxis Framework](https://diataxis.fr/)
- [LLVM Documentation](https://llvm.org/docs/)

## 📝 SSOT原則

このプロジェクトは **Single Source of Truth (SSOT)** を徹底：
- 各概念は1箇所でのみ定義
- 相互参照による情報の連携
- 自動生成可能な部分は生成
- バージョン管理による追跡可能性

---

*最終更新: CL-CC v0.1.0 | [フィードバック](https://github.com/cl-cc/issues)*