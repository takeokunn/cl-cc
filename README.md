# CL-CC (Bootstrap)

既存実装を全削除し、最小の再出発として以下を新規実装しました。

- Common Lispフロントエンド（S式読み込み + AST化）
- レジスタマシンVM（命令クラスはCLOS）
- x86_64 / aarch64 アセンブラ文字列出力（CLOS分派）
- Prolog風 peephole 最適化（最小）
- CPS変換（最小）
- FiveAM テスト
- Nix build/check

## サポートする最小言語

- 整数
- 変数
- `(+ a b)` `(- a b)` `(* a b)`
- `(if cond then else)`
- `(let ((x v) ...) body...)`
- `(progn ... )`
- `(print x)`

注: `print` はVM実行では動作しますが、現時点のx86_64/aarch64アセンブラ出力では未対応です。

## 使い方

```bash
nix build
nix flake check
```

REPLで手動実行:

```bash
nix develop
rlwrap sbcl
```

```lisp
(require :asdf)
(asdf:load-asd #P"./cl-cc.asd")
(asdf:load-system :cl-cc)

(cl-cc:run-string "(+ 1 2)")
(getf (cl-cc:compile-string "(+ 1 2)" :target :x86_64) :assembly)
(getf (cl-cc:compile-string "(+ 1 2)" :target :aarch64) :assembly)
```
