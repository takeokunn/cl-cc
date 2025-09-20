# CL-CC ドキュメントアーキテクチャ

## 概要

CL-CC（Common Lisp Compiler Collection）のドキュメントは、Diátaxisフレームワークに準拠し、学習・実践・参照・理解の4つの象限で構成されています。本ドキュメントは、すべてのドキュメントの構造と関係性を定義し、Single Source of Truth (SSOT) の原則を徹底します。

## Diátaxisフレームワーク構造

```mermaid
graph TB
    subgraph "学習指向 - 初学者向け"
        TUT[Tutorials<br/>体験による学習]
    end

    subgraph "タスク指向 - 実践者向け"
        HOW[How-to Guides<br/>問題解決の手順]
    end

    subgraph "情報指向 - 参照用"
        REF[Reference<br/>技術仕様の詳細]
    end

    subgraph "理解指向 - 深い洞察"
        EXP[Explanation<br/>理論と設計思想]
    end

    TUT <--> HOW
    HOW <--> REF
    REF <--> EXP
    EXP <--> TUT
    TUT <--> REF
    HOW <--> EXP
```

## ドキュメント構造マッピング

### 1. Tutorials（チュートリアル）
**目的**: 初学者が手を動かしながら学ぶ

```mermaid
graph LR
    subgraph "学習パス"
        T1[01-first-compiler<br/>最初のコンパイラ] --> T2[02-macro-power<br/>マクロの力]
        T2 --> T3[03-clos-architecture<br/>CLOSアーキテクチャ]
        T3 --> T4[04-prolog-integration<br/>Prolog統合]
        T4 --> T5[05-optimization<br/>最適化技術]
        T5 --> T6[06-testing<br/>テスト駆動開発]
    end
```

### 2. How-to Guides（ハウツーガイド）
**目的**: 特定の問題を解決する

```mermaid
graph TD
    subgraph "実践的タスク"
        H1[言語フロントエンド実装]
        H2[中間表現の設計]
        H3[最適化パスの実装]
        H4[バックエンドの構築]
        H5[デバッガの統合]
        H6[パフォーマンス分析]
    end
```

### 3. Reference（リファレンス）
**目的**: 完全で正確な技術仕様

```mermaid
graph TD
    subgraph "APIリファレンス"
        R1[Core API]
        R2[Frontend API]
        R3[IR API]
        R4[Optimization API]
        R5[Backend API]
        R6[Prolog API]
        R7[Testing API]
    end
```

### 4. Explanation（説明）
**目的**: 深い理解と設計思想

```mermaid
graph TD
    subgraph "理論と哲学"
        E1[コンパイラ理論]
        E2[CLOS設計哲学]
        E3[マクロメタプログラミング]
        E4[Prolog型推論]
        E5[最適化理論]
        E6[形式検証]
    end
```

## 相互参照マトリックス

| トピック | Tutorial | How-to | Reference | Explanation |
|---------|----------|---------|-----------|-------------|
| **コンパイラコア** | 01-first-compiler | implement-frontend | core-api | compiler-theory |
| **マクロシステム** | 02-macro-power | macro-dsl-creation | macro-api | metaprogramming |
| **CLOS** | 03-clos-architecture | class-hierarchy | clos-api | clos-philosophy |
| **Prolog統合** | 04-prolog-integration | prolog-rules | prolog-api | type-inference |
| **最適化** | 05-optimization | optimization-pass | optimization-api | optimization-theory |
| **テスト** | 06-testing | property-testing | testing-api | formal-verification |

## ドキュメント品質基準

### 必須要素

#### Tutorials
- [ ] 明確な学習目標
- [ ] ステップバイステップの手順
- [ ] 実行可能なコード例
- [ ] 期待される出力
- [ ] トラブルシューティング

#### How-to Guides
- [ ] 明確な問題定義
- [ ] 前提条件の明示
- [ ] 具体的な解決手順
- [ ] 代替アプローチ
- [ ] 関連リソースへのリンク

#### Reference
- [ ] 完全なAPI仕様
- [ ] パラメータの詳細説明
- [ ] 返り値の仕様
- [ ] エラー条件
- [ ] 使用例

#### Explanation
- [ ] 背景と文脈
- [ ] 理論的基礎
- [ ] 設計決定の根拠
- [ ] トレードオフの説明
- [ ] 将来の展望

## SSOT原則の実装

### 情報の一元管理

```mermaid
graph TB
    SSOT[MASTER-SSOT.md<br/>単一の真実の源]

    SSOT --> ARCH[ARCHITECTURE.md<br/>アーキテクチャ設計]
    SSOT --> API[API仕様]
    SSOT --> TERM[用語集]
    SSOT --> GUIDE[ガイドライン]

    ARCH --> TUT[Tutorials]
    API --> REF[Reference]
    TERM --> EXP[Explanation]
    GUIDE --> HOW[How-to]
```

### バージョン管理戦略

1. **Semantic Versioning**: すべてのドキュメントはセマンティックバージョニングに従う
2. **変更追跡**: 重要な変更はCHANGELOG.mdに記録
3. **レビュープロセス**: すべての変更は技術レビューを経る

### 更新プロトコル

```mermaid
graph LR
    CHANGE[変更要求] --> IMPACT[影響分析]
    IMPACT --> UPDATE[並行更新]
    UPDATE --> REVIEW[相互レビュー]
    REVIEW --> VALIDATE[整合性検証]
    VALIDATE --> PUBLISH[公開]
```

## ドキュメント間のナビゲーション

### クロスリファレンス規則

- `[→ Tutorial: タイトル](path/to/tutorial.md)` - チュートリアルへのリンク
- `[⚙ How-to: タイトル](path/to/howto.md)` - ハウツーガイドへのリンク
- `[📖 Reference: API名](path/to/reference.md#api)` - リファレンスへのリンク
- `[💡 Explanation: 概念](path/to/explanation.md)` - 説明へのリンク

### ナビゲーションパス

```mermaid
graph LR
    START[開始] --> LEARN{学習段階}

    LEARN -->|初学者| TUT[Tutorials]
    LEARN -->|実践者| HOW[How-to]
    LEARN -->|参照| REF[Reference]
    LEARN -->|深い理解| EXP[Explanation]

    TUT --> PRACTICE[実践]
    HOW --> PRACTICE
    PRACTICE --> DEEP[深化]
    REF --> DEEP
    EXP --> DEEP

    DEEP --> MASTER[習熟]
```

## 品質保証プロセス

### ドキュメントレビューチェックリスト

#### 技術的正確性
- [ ] コード例が動作することを確認
- [ ] API仕様が正確であることを確認
- [ ] 理論的説明が正確であることを確認

#### 一貫性
- [ ] 用語の統一性を確認
- [ ] スタイルガイドラインに準拠
- [ ] 相互参照が正しいことを確認

#### 完全性
- [ ] すべての必須セクションが存在
- [ ] 例が十分に提供されている
- [ ] エッジケースが説明されている

#### アクセシビリティ
- [ ] 明確で簡潔な文章
- [ ] 適切な図表の使用
- [ ] 段階的な複雑性の増加

## メンテナンスガイドライン

### 定期レビュースケジュール

| 頻度 | 対象 | アクション |
|------|------|------------|
| 毎週 | 新規追加ドキュメント | 技術レビューと統合 |
| 毎月 | 既存ドキュメント | 更新と改善 |
| 四半期 | 全体構造 | 構造の最適化 |
| 年次 | 完全監査 | 包括的な品質評価 |

### 責任分担

```mermaid
graph TD
    LEAD[ドキュメントリード]
    TECH[技術ライター]
    DEV[開発者]
    REV[レビュアー]

    LEAD -->|戦略| STRATEGY[全体戦略]
    TECH -->|執筆| WRITING[コンテンツ作成]
    DEV -->|技術| ACCURACY[技術的正確性]
    REV -->|品質| QUALITY[品質保証]
```

## 継続的改善

### フィードバックループ

```mermaid
graph LR
    USER[ユーザー] -->|フィードバック| COLLECT[収集]
    COLLECT -->|分析| ANALYZE[分析]
    ANALYZE -->|優先順位| PRIORITY[優先順位付け]
    PRIORITY -->|実装| IMPLEMENT[改善実装]
    IMPLEMENT -->|検証| VALIDATE[検証]
    VALIDATE -->|公開| RELEASE[リリース]
    RELEASE --> USER
```

### メトリクス

- **利用頻度**: 各ドキュメントのアクセス数
- **完了率**: チュートリアルの完了率
- **満足度**: ユーザーフィードバックスコア
- **更新頻度**: ドキュメントの鮮度
- **エラー率**: 報告されたエラーの数

## まとめ

このドキュメントアーキテクチャは、CL-CCプロジェクトの知識体系を体系的に組織化し、すべてのステークホルダーが必要な情報に効率的にアクセスできることを保証します。Diátaxisフレームワークの原則に従い、学習、実践、参照、理解の各側面から包括的なドキュメンテーションを提供します。