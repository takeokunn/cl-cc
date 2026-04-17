# CL-CC アーキテクチャ詳細

## コンパイルパイプライン

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Source    │ ──► │    Lexer    │ ──► │    CST      │
│  (.lisp)    │     │             │     │  Parser     │
└─────────────┘     └─────────────┘     └─────────────┘
                                               │
                                               ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    AST      │ ◄── │   Lower     │ ◄── │  S-expr     │
│   (CLOS)    │     │             │     │             │
└─────────────┘     └─────────────┘     └─────────────┘
       │
       │  ┌──────────────────────────────────────────────┐
       │  │              Macro Expansion                 │
       │  │  defstruct→defclass, loop→tagbody, etc.     │
       │  └──────────────────────────────────────────────┘
       │
       ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│ Type Check  │ ◄── │  Type       │ ──► │ Constraint  │
│ (Optional)  │     │  Inference  │     │   Solver    │
└─────────────┘     └─────────────┘     └─────────────┘
       │
       │  ┌──────────────────────────────────────────────┐
       │  │              CPS Transform (Optional)         │
       │  │  Continuation-Passing Style conversion        │
       │  └──────────────────────────────────────────────┘
       │
       ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  Codegen    │ ──► │    VM IR    │ ──► │  Optimize   │
│             │     │ (~220 inst) │     │   Passes    │
└─────────────┘     └─────────────┘     └─────────────┘
                                               │
                    ┌──────────────────────────┼──────────────────────────┐
                    │                          │                          │
                    ▼                          ▼                          ▼
             ┌─────────────┐           ┌─────────────┐           ┌─────────────┐
             │ VM Interpreter│          │    MIR/SSA  │           │  Bytecode   │
             │  (SBCL host) │           │             │           │  Encoder    │
             └─────────────┘           └─────────────┘           └─────────────┘
                    │                          │
                    │              ┌───────────┼───────────┐
                    │              │           │           │
                    │              ▼           ▼           ▼
                    │       ┌──────────┐ ┌──────────┐ ┌──────────┐
                    │       │  x86-64  │ │ AArch64  │ │  WASM    │
                    │       │ Codegen  │ │ Codegen  │ │ Codegen  │
                    │       └──────────┘ └──────────┘ └──────────┘
                    │              │           │           │
                    │              ▼           ▼           ▼
                    │       ┌──────────┐ ┌──────────┐ ┌──────────┐
                    │       │  Mach-O  │ │  Mach-O  │ │  .wasm   │
                    │       │  (ELF)   │ │          │ │          │
                    │       └──────────┘ └──────────┘ └──────────┘
                    │
                    ▼
             ┌─────────────┐
             │  Execution  │
             │   Result    │
             └─────────────┘
```

## AST階層構造

```lisp
;; 基底クラス
ast-node
├── ast-source-file
├── ast-source-line
├── ast-source-column
└── ast-children

;; リテラル
ast-int (value: integer)
ast-float (value: float)
ast-string (value: string)
ast-char (value: character)
ast-symbol (name: symbol)
ast-quote (value: t)

;; 変数と束縛
ast-var (name: symbol)
ast-let (bindings, body)
ast-setq (var, value)
ast-symbol-macrolet (bindings, body)

;; 関数
ast-lambda (params, optional-params, rest-param, key-params, body, env)
ast-function (name, lambda)
ast-flet (bindings, body)
ast-labels (bindings, body)
ast-call (func, args)

;; 制御フロー
ast-if (cond, then, else)
ast-progn (forms)
ast-block (name, body)
ast-return-from (name, value)
ast-tagbody (tags)
ast-go (tag)

;; 例外処理
ast-catch (tag, body)
ast-throw (tag, value)
ast-unwind-protect (protected, cleanup)
ast-handler-case (form, clauses)

;; 多値
ast-values (forms)
ast-multiple-value-bind (vars, values-form, body)
ast-multiple-value-call (func, args)
ast-multiple-value-prog1 (first, forms)

;; CLOS
ast-defclass (name, superclasses, slots, default-initargs)
ast-defgeneric (name, params)
ast-defmethod (name, qualifier, specializers, params, body)
ast-make-instance (class, initargs)
ast-slot-value (object, slot)
ast-set-slot-value (object, slot, value)

;; トップレベル定義
ast-defun (name, params, optional-params, rest-param, key-params, body)
ast-defvar (name, value)
ast-defparameter (name, value)
ast-defmacro (name, lambda-list, body)
ast-defconstant (name, value)
```

## VM命令セット (抜粋)

### 制御フロー

```lisp
vm-label (name)                    ; ラベル定義
vm-jump (label)                    ; 無条件ジャンプ
vm-jump-zero (reg label)           ; 条件付きジャンプ
vm-halt (reg)                      ; 実行終了
```

### 算術演算

```lisp
vm-const (reg value)               ; 定数ロード
vm-move (dst src)                  ; レジスタ間コピー

;; 整数演算
vm-integer-add (dst lhs rhs)
vm-integer-sub (dst lhs rhs)
vm-integer-mul (dst lhs rhs)

;; 浮動小数点演算
vm-float-add (dst lhs rhs)
vm-float-sub (dst lhs rhs)
vm-float-mul (dst lhs rhs)
vm-float-div (dst lhs rhs)

;; 超越関数
vm-sqrt (dst src)
vm-exp (dst src)
vm-log (dst src)
vm-sin (dst src)
vm-cos (dst src)
```

### リスト操作

```lisp
vm-cons (dst car cdr)              ; cons作成
vm-car (dst src)                   ; car取得
vm-cdr (dst src)                   ; cdr取得
vm-rplaca (cons value)             ; car更新
vm-rplacd (cons value)             ; cdr更新

vm-list-length (dst list)
vm-nth (dst index list)
vm-nthcdr (dst index list)
vm-first, vm-second, ... vm-tenth
vm-rest (dst list)
vm-last (dst list)
vm-butlast (dst list)
vm-append (dst list1 list2)
vm-reverse (dst list)
```

### 文字列操作

```lisp
vm-string= (dst str1 str2)
vm-string< (dst str1 str2)
vm-string-length (dst str)
vm-char (dst str index)
vm-subseq (dst str start end)
vm-concatenate (dst str1 str2)
vm-string-upcase (dst str)
vm-string-downcase (dst str)
```

### ハッシュテーブル

```lisp
vm-make-hash-table (dst test)
vm-gethash (dst key table default found-dst)
vm-sethash (key value table)
vm-remhash (key table)
vm-clrhash (table)
vm-hash-table-count (dst table)
vm-hash-table-p (dst table)
```

### 配列

```lisp
vm-make-array (dst dimensions element-type)
vm-aref (dst array index)
vm-aref-multi (dst array indices)
vm-aset (array index value)
vm-array-rank (dst array)
vm-array-dimension (dst array axis)
vm-array-total-size (dst array)
```

### 関数とクロージャ

```lisp
vm-closure (dst label params captured-vars)
vm-call (func args)
vm-tail-call (func args)
vm-ret (reg)
vm-func-ref (dst name)
vm-apply (func args-reg)

vm-register-function (name func)
vm-resolve-function (dst name)
```

### CLOS

```lisp
vm-class-def (name superclasses slots initargs)
vm-make-obj (dst class initarg-regs)
vm-slot-read (dst obj slot-name)
vm-slot-write (obj slot-name value)
vm-register-method (gf method specializer qualifier)
vm-generic-call (dst gf args)
vm-call-next-method (dst args)
vm-class-of-fn (dst obj)
vm-find-class (dst name)
```

### 例外処理

```lisp
vm-establish-handler (label error-type result-reg)
vm-remove-handler ()
vm-signal-error (error-type error-reg)
vm-sync-handler-regs (regs)

vm-establish-catch (tag-reg handler-label result-reg)
vm-throw (tag-reg value-reg)
```

### I/O

```lisp
vm-open-file (dst path direction)
vm-close-file (handle)
vm-read-char (dst handle eof-p eof-value)
vm-read-line (dst handle eof-p eof-value)
vm-write-char (char handle)
vm-write-string (str handle)
vm-peek-char (dst handle)
vm-unread-char (char handle)

vm-read-sexp (dst handle)
vm-print-sexp (value handle)
```

### グローバル変数

```lisp
vm-set-global (name value)
vm-get-global (dst name)
```

## 型システム

### 型推論 (Hindley-Milner Algorithm W)

```lisp
;; 型構成子
type-variable (name)
type-constructor (name args)
type-function (params return)
type-row (fields rest)
type-effect (kind row)

;; 推論パイプライン
1. 構文から型制約を生成
2. 制約を収集
3. 単一化で制約を解決
4. 代入を適用して具体化

;; 拡張機能
- Row polymorphism (レコード型)
- Effect tracking (副作用追跡)
- Union type narrowing (条件分岐での型絞り込み)
- Typeclass stubs (型クラス)
```

### 型クラス

```lisp
(deftype-class Eq (t)
  (eq (-> t t boolean)))

(deftype-instance Eq integer
  (eq #'=))

(deftype-class Functor (f)
  (fmap (-> (-> a b) (f a) (f b))))
```

## 最適化パイプライン

### Phase 0: 効果解析

```lisp
;; 命令を効果の種類で分類
- Pure: 副作用なし (CSE/DCE可能)
- Read: 読み取りのみ
- Write: 書き込み
- Control: 制御フロー
- IO: 入出力
```

### Phase 1: CFG + SSA

```lisp
;; CFG構築
1. 基本ブロック分割
2. エッジ追加 ( predecessors/successors)
3. RPO順序計算
4. 支配木構築
5. 支配境界計算

;; SSA構築 (Braun et al. 2013)
1. Φ関数の配置 (支配境界)
2. 変数のリネーム
3. 未定義値の処理
```

### Phase 2: E-Graph最適化

```lisp
;; E-Graph操作
1. egraph-add: ノード追加
2. egraph-merge: 等価クラス統合
3. egraph-saturate: 書き換え規則を飽和まで適用
4. egraph-extract: 最適コストの表現を抽出

;; 書き換え規則例
(+ x 0) => x
(* x 1) => x
(+ x x) => (* x 2)
```

## バックエンド

### x86-64

```lisp
;; 呼び出し規約
System V AMD64 ABI:
- 引数: RDI, RSI, RDX, RCX, R8, R9
- 戻り値: RAX
- Caller-saved: RAX, RCX, RDX, RSI, RDI, R8-R11
- Callee-saved: RBX, RBP, R12-R15

;; レジスタ割り当て
- 線形スキャンアルゴリズム
- スピルスロット管理
- ライブ区間計算
```

### AArch64

```lisp
;; 呼び出し規約
AAPCS64:
- 引数: X0-X7
- 戻り値: X0
- Caller-saved: X0-X18
- Callee-saved: X19-X28
```

### WebAssembly

```lisp
;; 構造
(module
  (func $main (result i64)
    ...)
  (memory 1)
  (export "main" (func $main)))

;; 特徴
- スタックマシン
- 線形メモリ
- 関数テーブル
```

## セルフホスティングメカニズム

### Macro評価のフック

```lisp
;; packages/frontend/expand/src/macro.lisp
(defvar *macro-eval-fn* nil
  "マクロ展開時に使用する評価関数")

(defun our-macroexpand-1 (form env)
  (let ((expander (lookup-macro (car form) env)))
    (when expander
      (funcall *macro-eval-fn*
               `(funcall ,expander ',form)))))

;; pipeline/src/pipeline.lisp
(setf *macro-eval-fn* #'our-eval)  ; セルフホスティング有効化
```

### REPL状態永続化

```lisp
;; 永続変数
*repl-vm-state*           ; VM状態 (ヒープ、レジスタ)
*repl-pool-instructions*  ; 全命令の蓄積
*repl-pool-labels*        ; グローバルラベルテーブル
*repl-global-vars-persistent*  ; グローバル変数名

;; 実行フロー
1. 新しい命令をプールに追加
2. ラベルをオフセット付きでマージ
3. 新しいスライスのみ実行
4. プール全体のラベルテーブルを使用
```

## デバッグとトレース

### ログシステム

```lisp
;; ログレベル
+log-level-debug+
+log-level-info+
+log-level-warning+
+log-level-error+

;; 使用例
(log-debug "Compiling form: ~S" form)
(log-info "Optimization pass complete: ~A reductions" count)
```

### VMトレース

```lisp
;; トレース有効化
(setf *vm-trace* t)

;; 出力例
[PC=0] vm-const r0 42
[PC=1] vm-const r1 10
[PC=2] vm-integer-add r2 r0 r1  ; r2 = 52
[PC=3] vm-halt r2               ; result = 52
```

### IRダンプ

```lisp
;; IR表示
(ir-print-function function)

;; 出力例
function foo(x: int, y: int) -> int {
entry:
  v0 = add x, y
  v1 = mul v0, 2
  return v1
}
```
