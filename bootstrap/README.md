# Bootstrap Directory

このディレクトリにはCL-CCの3段階ブートストラッププロセスが含まれています。

## ファイル構成

```
bootstrap/
├── README.md                # このファイル（ブートストラップの概要）
├── stage0.lisp              # Stage 0: Host CL Bootstrap
├── stage1.lisp              # Stage 1: First Self-Compilation
├── stage2.lisp              # Stage 2: Fixed Point Verification
├── minimal-subset.lisp      # 最小サブセット定義
└── scripts/
    ├── bootstrap-stage0.sh   # Stage 0実行スクリプト
    ├── bootstrap-stage1.sh   # Stage 1実行スクリプト
    └── bootstrap-stage2.sh   # Stage 2実行スクリプト
```

## 使用方法

### 完全なブートストラップ

```bash
# 全てのステージを順番に実行
cd bootstrap

# Stage 0: Host CL → Stage 1
bash scripts/bootstrap-stage0.sh

# Stage 1: Self-Compilation → Stage 2
bash scripts/bootstrap-stage1.sh

# Stage 2: Fixed Point Verification
bash scripts/bootstrap-stage2.sh
```

### 個別にステージを実行

```bash
# SBCL REPLから直接実行
sbcl --load cl-cc.asd \
      --eval '(asdf:load-system :cl-cc)' \
      --eval '(in-package :cl-cc)'

# Stage 0
(bootstrap-stage-0)

# Stage 1
(bootstrap-stage-1 "build/clcc-stage1")

# Stage 2
(bootstrap-stage-2 "build/clcc-stage2")
```

## 成功基準

### Stage 0
- [ ] Stage 1バイナリが生成される（`build/clcc-stage1`）
- [ ] 基本的な算術演算が動作する
- [ ] 基本的な制御フローが動作する

### Stage 1
- [ ] Stage 2バイナリが生成される（`build/clcc-stage2`）
- [ ] Stage 1が全てのソースファイルをコンパイル可能
- [ ] 評価、リスト操作、制御フローのテストが通過

### Stage 2
- [ ] Stage 1とStage 2のバイナリがバイト単位で一致
- [ ] Stage 1とStage 2のCソースが一致（タイムスタンプを除く）
- [ ] 全ての機能テストが通過

## トラブルシューティング

### Stage 0で失敗する場合

```bash
# デバッグモードで再実行
sbcl --load cl-cc.asd \
      --eval '(asdf:load-system :cl-cc)' \
      --eval '(in-package :cl-cc)' \
      --eval '(setf *debug-mode* t)' \
      --eval '(bootstrap-stage-0)'

# エラー詳細を確認
# 検出力で「Error:」または「Warning:」を探す
```

### Stage 1で失敗する場合

```bash
# Stage 1バイナリのパスを確認
ls -lh build/clcc-stage1

# Stage 1で手動テスト
./build/clcc-stage1 "(+ 1 2)"

# Cコードを検証
ls -lh build/stage1/*.c
cat build/stage1/compiler.c  # 例: compilerの出力を確認
```

### Stage 2でバイナリが異なる場合

```bash
# バイナリ差分を確認
diff -u <(objdump -d build/clcc-stage1) \
        <(objdump -d build/clcc-stage2)

# Cソースのみを比較
diff -u -I '^//' -I '^[0-9][0-9]*$' \
        build/stage1/*.c build/stage2/*.c
```

## 最小サブセットの検証

```lisp
;; 最小サブセットの使用状況を確認
(require :cl-cc)
(in-package :cl-cc)

;; 最小サブセット定義をロード
(load "bootstrap/minimal-subset.lisp")

;; ASTのカバレージを確認
(cl-cc/verify-minimal-coverage some-ast)

;; 最小サブセットの情報を表示
(cl-cc/get-minimal-subset-info)
```

## 参考資料

- [BOOTSTRAP.md](BOOTSTRAP.md) - 詳細なブートストラップドキュメント
- [../IMPLEMENTATION_PLAN.md](../IMPLEMENTATION_PLAN.md) - 実装計画（WAVE 6）
- [../ARCHITECTURE.md](../ARCHITECTURE.md) - システムアーキテクチャ

## 次のステップ

ブートストラップが完了したら：

1. **CLOS統合**: 完全なCLOSオブジェクトシステムを実装
2. **マクロシステム**: 完全なCommon Lispマクロを実装
3. **Prolog統合**: 完全なProlog型推論と最適化を実装
4. **パフォーマンス向上**: コンパイル速度とバイナリサイズの最適化

## 貢献

ブートストラッププロセスに改善がある場合は：

1. Issueを作成: バグ報告や機能リクエスト
2. プルリクエストを作成: 改善を提案
3. ドキュメンテーションを更新: 新たな機能や変更を記録

---

**最終更新**: 2026-03-20
**バージョン**: 0.1.0
**ステータス**: 初期版（WAVE 6実装中）
