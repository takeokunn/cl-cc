# CL-CC Bootstrap Process

## Overview

CL-CCは3段階のブートストラッププロセスを通じて自己ホスティングを達成します：

```
Stage 0 (Host CL) → Stage 1 (First Self-Compile) → Stage 2 (Fixed Point)

SBCL/CCL       cl-cc Stage 1          cl-cc Stage 2
    │                  │                      │
    │                  │                      │
    └─ Mach-O Binary ─→ Compile Self ─→ Mach-O Binary
                            │                      │
                            └─ Byte-for-Byte Identical ─┘
```

このプロセスは、**固定点（fixed point）**を達成するまで続きます：
- Stage 1が自身のソースをコンパイルしてStage 2を生成
- Stage 1とStage 2のバイナリがバイト単位で完全に一致

## Stage 0: Host Common Lisp

**目的**: ホストのCommon Lisp実装（SBCLまたはCCL）を使用して、cl-ccのソースコードを初回コンパイル

### 役割

- ホストCLのCLOS、リーダー、マクロシステムを活用
- `src/frontend.lisp`, `src/compiler.lisp` などのソースをMach-Oバイナリに変換
- x86_64またはAArch64向けのアセンブラを生成

### エントリーポイント

**メインスクリプト**:
- `scripts/bootstrap-stage0.sh` - 実行スクリプト

**コードエントリ**:
```lisp
(in-package :cl-cc)

(defun bootstrap-stage-0 ()
  "ホストCLを使用して初期コンパイルを実行"
  (let* ((*features* (list :cl-cc))
         (source-files '("src/package.lisp"
                        "src/frontend.lisp"
                        "src/reader.lisp"
                        "src/cps.lisp"
                        "src/prolog.lisp"
                        "src/vm.lisp"
                        "src/compiler.lisp"
                        "src/backend/x86-64.lisp"
                        "src/backend/aarch64.lisp"))
         (stage1-binary "build/clcc-stage1"))
    ;; 各ソースファイルをCにトランスパイル
    (dolist (source source-files)
      (let* ((ast (read-source source))
             (compiled (cl-cc:compile-expression ast :target :x86_64))
             (c-code (getf compiled :c-source)))
        (write-c-source (format nil "build/stage1/~A.c"
                            (pathname-name source))
                      c-code)))
    ;; Cコードをコンパイルしてリンク
    (compile-c-sources "build/stage1")
    (link-executable stage1-binary)))
```

### 成功基準

- [ ] cl-ccの全ソースファイルがエラーなしでCコードにトランスパイル
- [ ] CコードがGCCでコンパイル成功（`-Wall -Werror`）
- [ ] Stage 1バイナリが正常に生成
- [ ] Stage 1が基本的なLisp式を実行可能

### 依存関係

```
Stage 0 Dependencies:
├── Host CL (SBCL 2.3.0+ or CCL 1.12+)
├── ASDF (システム定義)
├── alexandria (ユーティリティ)
└── trivial-types (型ユーティリティ)
```

## Stage 1: First Self-Compile

**目的**: Stage 0で生成したバイナリを使用して、cl-cc自身のソースコードをコンパイル

### 役割

- Stage 1バイナリ = Stage 0がコンパイルしたcl-cc
- macOS上で実行し、Mach-Oバイナリを生成
- 最初は言語機能のサブセットのみサポート

### エントリーポイント

**メインスクリプト**:
- `scripts/bootstrap-stage1.sh`

**コードエントリ**:
```lisp
(in-package :cl-cc)

(defun bootstrap-stage-1 (stage1-executable)
  "Stage 1バイナリを使用して自己コンパイルを実行"
  (let* ((source-files '("src/package.lisp"
                        "src/frontend.lisp"
                        ;; ... 他のソースファイル
                        "src/compiler.lisp"
                        "src/backend/x86-64.lisp"))
         (stage2-binary "build/clcc-stage2"))
    ;; Stage 1を使用して各ファイルをコンパイル
    (dolist (source source-files)
      (run-command (format nil "~A --compile ~A --output build/stage2/~A.c"
                          stage1-executable
                          source
                          (pathname-name source))))
    ;; Stage 2バイナリをビルド
    (compile-c-sources "build/stage2")
    (link-executable stage2-binary)))
```

### 成功基準

- [ ] Stage 1がcl-ccの全ソースファイルをコンパイル可能
- [ ] 出力されたバイナリが正しく実行
- [ ] 基本的な式評価が動作（`(+ 1 2)`, `(if t 1 0)`など）
- [ ] VM命令が正しく実行

### 言語機能の制限

**Stage 1でサポートされる最小サブセット**:

- 整数、シンボル、コンスセル
- `+`, `-`, `*`, `/`（算術演算）
- `=`, `<`, `>`（比較演算）
- `if`, `let`, `progn`（制御フロー）
- `lambda`, `funcall`（関数呼び出し）

**まだサポートされていない機能**:
- マクロ展開
- CLOS
- CPS変換
- Prolog推論
- 高度な例外処理

### 依存関係

```
Stage 1 Dependencies:
├── Stage 0バイナリ (clcc-stage1)
├── C Runtime (src/runtime/*.c)
└── cl-ccのソースコード
```

## Stage 2: Fixed Point Verification

**目的**: Stage 1とStage 2がバイト単位で完全に一致することを検証し、自己ホスティングを達成

### 役割

- Stage 2バイナリ = Stage 1がコンパイルしたcl-cc
- バイナリ差分: Stage 1 == Stage 2（バイト単位）
- 全言語機能サポート

### エントリーポイント

**メインスクリプト**:
- `scripts/bootstrap-stage2.sh`

**検証コード**:
```lisp
(in-package :cl-cc)

(defun verify-fixed-point (stage1-binary stage2-binary)
  "Stage 1とStage 2が同一であることを検証"
  (let* ((source-files '("src/package.lisp"
                        ;; ... 全ソースファイル
                        "src/backend/aarch64.lisp"))
         (stage1-outputs (mapcar (lambda (src)
                                 (format nil "build/stage1/~A.c"
                                             (pathname-name src)))
                               source-files))
         (stage2-outputs (mapcar (lambda (src)
                                 (format nil "build/stage2/~A.c"
                                             (pathname-name src)))
                               source-files)))
    ;; 全Cファイルのバイト単位比較
    (dolist (s1 stage1-outputs)
               (let ((s2 (format nil "build/stage2/~A"
                                     (pathname-name s1))))
         (unless (file-identical-p s1 s2)
           (error "Fixed point not achieved: ~A differs from ~A~%"
                  s1 s2))))
    ;; 全テストを実行
    (let ((test-results (run-test-suite)))
      (unless (all-tests-passed-p test-results)
        (error "Tests failed at Stage 2")))
    t)))
```

### 成功基準

1. **バイナリ一致性**:
   - [ ] Stage 2バイナリがStage 1とバイト単位で完全に一致
   - [ ] `diff -u stage1 stage2` で差分がない（タイムスタンプ/コメントを除く）

2. **機能完全性**:
   - [ ] 全ての言語機能がサポート（マクロ、CLOS、CPS、Prolog）
   - [ ] 全テストスイートが通過
   - [ ] リグレッションなし

3. **自己ホスティング**:
   - [ ] Stage 2が自身のソースを再コンパイル可能
   - [ ] Stage 2がStage 3を生成してもStage 2と同じ

### 固定点の概念

```
Fixed Point Visualization:

Source Code (S)
    │
    ├─ Stage 0 compiles S → Binary B0
    │
    ├─ Stage 1 (B0) compiles S → Binary B1
    │
    ├─ Stage 2 (B1) compiles S → Binary B2
    │
    ├─ Stage 3 (B2) compiles S → Binary B3
    │
    └─ Stage N (BN-1) compiles S → Binary BN

    Fixed Point Reached: Bn == Bn-1 == Bn-2 == ...
```

## Bootstrapディレクトリ構造

```
bootstrap/
├── README.md           # このファイル
├── stage0.lisp         # Stage 0エントリーポイント
├── stage1.lisp         # Stage 1エントリーポイント
├── stage2.lisp         # Stage 2検証
├── minimal-subset.lisp # ブートストラップ用最小サブセット定義
└── scripts/
    ├── bootstrap-stage0.sh
    ├── bootstrap-stage1.sh
    └── bootstrap-stage2.sh
```

## Minimal Bootstrap Subset

ブートストラップ用に必要な最小限のLispサブセット：

### Core Forms（基本フォーム）

```lisp
;; データ型
- integers
- symbols
- cons cells

;; 制御フロー
- lambda (fn args body)
- if (cond then else)
- let (bindings body)
- progn (forms...)
- setq (var value)

;; 特殊な制御
- block (name body)
- return-from (name value)
- tagbody (tags...)
- go (tag)

;; 例外処理
- catch (tag body)
- throw (tag value)
- unwind-protect (protected cleanup)
```

### Functions（基本関数）

```lisp
;; 定義
- defun (name args body)
- flet (bindings body)
- labels (bindings body)

;; 関数適用
- apply (function args)
- funcall (function arg1 arg2 ...)

;; リスト操作
- car (list)
- cdr (list)
- cons (elem list)
- list (elems...)

;; 算術
- (+ a b)
- (- a b)
- (* a b)
- (/ a b)

;; 比較
- (= a b)
- (< a b)
- (> a b)
- (<= a b)
- (>= a b)

;; 等価性
- eq (a b)           ; ポインタ比較
- eql (a b)          ; 値の比較（数、シンボル）
- equal (a b)        ; 再帰的な構造比較

;; 型述語
- typep (object type)
- coerce (value type)
```

### Macros（マクロ）

```lisp
;; 条件分岐
- when (condition body...)
- unless (condition body...)
- cond ((condition1 body1) (condition2 body2)...)

;; 束縛
- let* (bindings body)      ; 逐次束縛
- letrec (bindings body)    ; 再帰的束縛

;; 複数値
- multiple-value-bind (vars values) body
- multiple-value-call (function values...)

;; マクロ定義
- defmacro (name args body)
```

### VM Instructions（VM命令）

```lisp
;; レジスタ操作
- vm-const (dst value)      ; 定数ロード
- vm-move (dst src)        ; レジスタ間移動

;; 算術
- vm-add (dst lhs rhs)
- vm-sub (dst lhs rhs)
- vm-mul (dst lhs rhs)

;; 制御フロー
- vm-jump (label)          ; 無件ジャンプ
- vm-jump-zero (reg label) ; ゼロの場合ジャンプ
- vm-label (name)           ; ラベル定義

;; 関数呼び出し
- vm-call (dst func args)
- vm-ret (reg)            ; リターン

;; 出力
- vm-print (reg)          ; 値の出力
- vm-halt (reg)           ; 終了
```

## Verification Process（検証プロセス）

### 完全なブートストラップ検証手順

```
┌─────────────────────────────────────────────────────────────┐
│ 1. Stage 0 Compilation                          │
├─────────────────────────────────────────────────────────────┤
│ - Host CL (SBCL/CCL) を起動                     │
│ - cl-ccソースをCコードにトランスパイル          │
│ - CコードをコンパイルしてStage 1バイナリ生成 │
│ - 基本的なテストを実行                          │
└──────────────────┬──────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. Stage 1 Compilation                          │
├─────────────────────────────────────────────────────────────┤
│ - Stage 1バイナリを実行                         │
│ - cl-ccソースをコンパイル                        │
│ - Stage 2バイナリを生成                        │
│ - 機能テストを実行                              │
└──────────────────┬──────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. Fixed Point Verification                      │
├─────────────────────────────────────────────────────────────┤
│ - Stage 1とStage 2のバイナリを比較            │
│   diff -u build/clcc-stage1 build/clcc-stage2     │
│ - 全テストスイートを実行                        │
│   ./build/clcc-stage2 --run-all-tests            │
│ - 機能リグレッションなしを確認                │
└──────────────────┬──────────────────────────────────────┘
                   │
                   ▼
           ┌─────────────────┐
           │  ALL PASS?    │
           └─────┬───────┘
                 │
        ┌────────┴────────┐
        │   Yes          │   No
        ▼               ▼
  ┌─────────┐    ┌─────────────────┐
  │ SUCCESS │    │   INVESTIGATE  │
  │ Fixed  │    │   - Diff output  │
  │ Point! │    │   - Debug logs   │
  └─────────┘    │   - Fix issues   │
                 └───────┬────────┘
                         │
                         ▼
                 ┌─────────────────┐
                 │   RETRY        │
                 │   (from Stage 1)│
                 └─────────────────┘
```

### 自動化検証スクリプト

```bash
#!/bin/bash
# scripts/verify-bootstrap.sh

set -e

echo "=== Stage 0: Host CL Compilation ==="
bash scripts/bootstrap-stage0.sh

echo ""
echo "=== Stage 1: Self-Compilation ==="
bash scripts/bootstrap-stage1.sh

echo ""
echo "=== Stage 2: Fixed Point Verification ==="
bash scripts/bootstrap-stage2.sh

echo ""
echo "=== Binary Comparison ==="
diff -u build/clcc-stage1 build/clcc-stage2 || {
    echo "ERROR: Binaries differ!"
    exit 1
}

echo ""
echo "=== Full Test Suite ==="
./build/clcc-stage2 --run-all-tests

echo ""
echo "=== BOOTSTRAP COMPLETE ==="
echo "✅ Fixed point achieved!"
echo "✅ All tests passing!"
```

## Troubleshooting（トラブルシューティング）

### Common Issues

#### 1. Stage 1 Compilation Fails（Stage 1コンパイル失敗）

**症状**:
```
Error: Unbound variable: *some-var*
Error: Undefined function: compile-ast
```

**原因**:
- 最小サブセットが不完全
- ランタイム関数が実装されていない
- 環境設定が正しくない

**解決策**:
```bash
# 1. 最小サブセットを確認
grep -E "^(defun|defgeneric)" bootstrap/minimal-subset.lisp

# 2. ホストCLのバージョンを確認
sbcl --version
# SBCL 2.3.0+またはCCL 1.12+が必要

# 3. デバッグモードで再実行
sbcl --load cl-cc.asd --eval '(cl-cc:bootstrap-stage-0)' --eval '(debug-on)'
```

#### 2. Stage 2 Differs from Stage 1（Stage 2がStage 1と異なる）

**症状**:
```
$ diff -u build/clcc-stage1 build/clcc-stage2
Binary files build/clcc-stage1 and build/clcc-stage2 differ
```

**原因**:
- 非決定的なコード生成
- タイムスタンプ/コメントの違い
- ランダムシードの違い
- 最適化の違い

**解決策**:
```bash
# 1. Cコードのみを比較（バイナリを除外）
diff -u <(objdump -d build/clcc-stage1) \
        <(objdump -d build/clcc-stage2)

# 2. タイムスタンプ/コメントを無視
diff -u -I '^//' -I '^[0-9][0-9]*$' \
        build/stage1/*.c build/stage2/*.c

# 3. 決定的ビルドを確認
nix-build  # Nixであれば確実に決定的
```

#### 3. Runtime Crashes（ランタイムクラッシュ）

**症状**:
```
$ ./build/clcc-stage1
Segmentation fault (core dumped)
```

**原因**:
- GCの問題
- スタックオーバーフロー
- プリミティブ実装の欠落
- メモリ破壊

**解決策**:
```bash
# 1. Valgrindでメモリチェック
valgrind --leak-check=full ./build/clcc-stage1

# 2. 最適化を無効にしてビルド
export CFLAGS="-O0 -g -fno-omit-frame-pointer"
bash scripts/bootstrap-stage0.sh

# 3. スタックサイズを増やす
sbcl --dynamic-space-size 4000 --eval '(cl-cc:bootstrap-stage-0)'
```

#### 4. Test Failures at Stage 2（Stage 2でテスト失敗）

**症状**:
```
Running tests...
FAIL: test-if-expression
Expected: 10
Got: 20
```

**原因**:
- Stage 1とStage 2でセマンティクスの違い
- 最適化パスの違い
- ランタイム環境の違い

**解決策**:
```lisp
;; 個別のテストデバッグ
(cl-cc:run-test :verbose t :test 'test-if-expression)

;; 中間表現を確認
(cl-cc:compile-string "(if t 10 20)" :target :x86_64 :debug t)

;; Stage 1とStage 2の出力を比較
(cl-cc:compare-stage-output :stage1 build/clcc-stage1
                           :stage2 build/clcc-stage2)
```

## Integration with Nix

Nixを使用した決定的なブートストラップ:

```nix
# flake.nixのブートストラップパッケージ
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        cl-cc = pkgs.callPackage ./default.nix {};
      in {
        packages.cl-cc-bootstrap = pkgs.stdenv.mkDerivation {
          name = "cl-cc-bootstrap";
          src = ./.;

          buildInputs = with pkgs; [
            sbcl
            gcc
            gnumake
          ];

          buildPhase = ''
            sbcl --load cl-cc.asd \
                 --eval '(cl-cc:bootstrap-all)' \
                 --eval '(quit)'
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp build/clcc-stage2 $out/bin/clcc
          '';
        };
      }
    );
}
```

## Performance Considerations

ブートストラッププロセスのパフォーマンス指標:

| Stage | コンパイル時間 | バイナリサイズ | メモリ使用 |
|-------|--------------|--------------|-----------|
| Stage 0 | ~30秒 | ~500KB | ~50MB |
| Stage 1 | ~45秒 | ~600KB | ~60MB |
| Stage 2 | ~45秒 | ~600KB | ~60MB |

**最適化のヒント**:
- Stage 0以降は最適化レベルを上げても安全
- `-O3`でGCCコンパイル
- リンク時最適化を有効化（`-flto`）

## Success Checklist

ブートストラップ完了の確認リスト:

### Stage 0
- [ ] ホストCLでcl-ccシステムがロード可能
- [ ] ソースコードがCコードにトランスパイル
- [ ] Stage 1バイナリが生成
- [ ] Stage 1で基本的な式が実行可能

### Stage 1
- [ ] Stage 1がソースコードをコンパイル可能
- [ ] Stage 2バイナリが生成
- [ ] 全テストの80%以上が通過
- [ ] バイナリサイズが合理的（< 1MB）

### Stage 2
- [ ] Stage 1とStage 2がバイト単位で一致
- [ ] 全テストが通過
- [ ] Stage 2が自身を再コンパイル可能
- [ ] Stage 3がStage 2と同じバイナリを生成

### 全体
- [ ] 固定点に到達
- [ ] 自己ホスティング達成
- [ ] ドキュメンテーション完了
- [ ] CIパイプラインに統合

## References

- [CL-CC Implementation Plan](../IMPLEMENTATION_PLAN.md)
- [Architecture Documentation](ARCHITECTURE.md)
- [Type System Design](TYPE-SYSTEM.md)
- [Common Lisp HyperSpec](https://lispworks.com/documentation/HyperSpec/)

---

**最終更新**: 2026-03-20
**バージョン**: 0.1.0
**ステータス**: Draft（WAVE 6実装予定）
