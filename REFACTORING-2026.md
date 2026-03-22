# CL-CC 2026 Modern Common Lisp Refactoring Requirements

## Overview

Refactor the CL-CC compiler from classical monolithic CL style to modern 2026 Common Lisp idioms.
Large-scale rewrite. Zero external dependencies maintained.

## Design Philosophy: 6原則

### 原則 1: Prolog と Macro に可能な限り全て寄せる

**判定基準**: 「この関数は変換ルールか？」→ Yes なら Prolog 化。
「このパターンは3回以上出現するか？」→ Yes ならマクロ化。

```
現状                              → 目標
343 defun + 601 defmethod           → ~150 defun + ~50 defmethod + ~450 Prolog rules
285 defclass                        → ~5 defclass + ~120 defstruct (Data)
手続き的パターンマッチ              → Prolog ユニフィケーション (Logic)
83 builtin-name-p チェック          → 50 Prolog facts
205 instruction->sexp defmethod     → 100 Prolog facts (双方向)
30+ compiler-macroexpand-all 分岐   → 40 Prolog rules
38 compile-ast defmethod            → 80-120 Prolog rules
```

**命令的コードとして残すもの（最小セット）:**
- Prolog エンジン自体（unify, solve-goal, backtrack）
- VM 実行エンジン（ステートフル、性能クリティカル）
- バイナリ生成（バイトレベル操作）
- I/O 操作

**記述の優先順位:**
1. **Prolog fact** — 最も簡潔（1行で1変換ルール）
2. **Prolog rule** — 条件付き変換（2-5行）
3. **Macro** — 定型パターンの抽象化（define-vm-instruction 等）
4. **CPS 関数** — 明示的制御フロー
5. **命令的関数** — 最後の手段

### 原則 2: CPS 変換を可能な限り使う

```
現状                              → 目標
return-from 135箇所                → 0（CPS 継続で全て表現）
  うち compiler.lisp 92箇所        → compile-ast(node, ctx, k) で解消
VM loop + MV-BIND 3値              → direct-threaded closure chain
同期パイプライン                    → CPS pipeline with hooks
```

**CPS 適用箇所:**
- `compile-ast(node, ctx)` → `compile-ast(node, ctx, k)` — 結果レジスタを継続に渡す
- VM 実行ループ → direct threading（各命令ハンドラが次を直接呼ぶ）
- コンパイラパイプライン → 各段階が継続を受け取る

### 原則 3: Data と Logic の分離

```
┌─────────────────────────────────────────────────┐
│                 Macro Layer                      │
│   define-vm-instruction, define-builtin, etc.    │
│   → defstruct + Prolog rule + CPS handler 一括生成│
├─────────────────────────────────────────────────┤
│              Prolog Rule Layer (Logic)            │
│   lower, compile, optimize, type-infer, inst-sexp│
│   → 「何を変換するか」を宣言的に記述              │
├─────────────────────────────────────────────────┤
│              CPS Control Layer (Control)          │
│   compile-ast(k), pipeline(k), vm-direct-thread  │
│   → 「どの順序で実行するか」を明示的に接続        │
├─────────────────────────────────────────────────┤
│           Data Definition Layer (Data)           │
│   defstruct: ast-node, vm-instruction, type-node │
│   → 純粋なデータ構造のみ、ロジックなし           │
└─────────────────────────────────────────────────┘
```

**禁止パターン:**
- defstruct/defclass 内にロジックを書かない（:initform に副作用のある式を入れない）
- Prolog ルール内に手続き的コードを書かない（`:when` 以外）
- CPS 関数内にパターンマッチを書かない（Prolog に委譲）

### 原則 4: 複雑な関数の可読性を上げる

**定量ルール:**
- 1関数/1メソッド **50行以下**（超える場合は分割 or Prolog 化）
- `cond`/`case` **10分岐以下**（超える場合は Prolog or dispatch table）
- ネスト深度 **4レベル以下**（超える場合は CPS or ヘルパー抽出）

**現状の違反箇所:**

| ファイル | 関数 | 行数 | 問題 |
|:---|:---|:---:|:---|
| compiler.lisp | `compile-ast (ast-call)` | 885行 | 83 builtin チェック |
| compiler.lisp | `compiler-macroexpand-all` | ~200行 | 30+ cond 分岐 |
| compiler.lisp | `compile-ast (ast-lambda)` | ~60行 | 深いネスト |
| macro.lisp | `our-defmacro loop` | ~230行 | 複雑なパーサ |
| vm.lisp | `execute-instruction (vm-call)` | ~65行 | 多段処理 |
| vm.lisp | `vm-resolve-gf-method` | ~45行 | 複合ディスパッチ |
| frontend.lisp | `lower-sexp-to-ast (cons)` | ~400行 | 巨大 case 分岐 |

**解決策:**
- 885行メソッド → Prolog facts + 汎用コンパイルルール（原則1）
- 30+ cond → Prolog rules（原則1）
- 400行 case → Prolog lowering rules（原則1）
- 深いネスト → CPS 化（原則2）

### 原則 5: Dead Code の削除

**即時削除対象（定義なし export / 未使用コード）:**

| シンボル | 場所 | 状態 |
|:---|:---|:---|
| `vm-frame` | package.lisp:342, tests/package.lisp:92 | export のみ、定義なし |
| `vm-restore-frame` | package.lisp:343, tests/package.lisp:93 | export のみ、定義なし |
| `vm-frame-pointer` | package.lisp:405 | export のみ、定義なし |
| `vm-alloc-size` | package.lisp:373, tests/package.lisp:133 | export のみ、定義なし |
| `*enable-prolog-peephole*` | prolog.lisp:358 | 常に `nil` → 有効化 or ルール自体を統合 |
| docs/ARCHITECTURE.md 内の vm-frame 参照 | docs/ARCHITECTURE.md:320-321 | ドキュメント更新 |

**調査が必要な候補:**
- `test-binary.lisp`, `test-generators.lisp`, `test-x86-64-codegen.lisp` — ASDF に含まれないトップレベルファイル
- `bootstrap/minimal-subset.lisp` — bootstrap プロセスで使用されているか
- `run-ci-tests.lisp` — CI で実際に使用されているか

### 原則 6: ヒューマンリーダブルか

**チェックリスト（全関数に適用）:**

- [ ] 関数名だけで目的がわかるか
- [ ] 引数名だけで意味がわかるか
- [ ] `return-from` による非ローカル脱出がないか → CPS 化
- [ ] 巨大 `cond`/`case` がないか → Prolog 化
- [ ] 同じパターンの繰り返しがないか → マクロ化
- [ ] コメントは「なぜ」のみか（「何を」はコードが語る）
- [ ] セクション区切り `;;; ---...---` がないか → 削除
- [ ] `make-instance` の繰り返しがないか → defstruct BOA constructor
- [ ] magic number がないか → defconstant +name+
- [ ] ネストが4レベルを超えないか → CPS or ヘルパー抽出

**リファクタリング後の理想的なコード例:**

```lisp
;;; ===== Data (defstruct のみ) =====
(defstruct (ast-int (:include ast-node))
  (value 0 :type integer))

;;; ===== Logic (Prolog rules のみ) =====
(def-rule (lower ?val (ast-int ?val))
  (:when (integerp ?val)))

(def-fact (builtin-unary car vm-car))

(def-rule (compile (ast-int ?val) ?ctx ?reg ?in ?out)
  (fresh-reg ?ctx ?reg)
  (= ?out (cons (vm-const ?reg ?val) ?in)))

;;; ===== Control (CPS のみ) =====
(defun compile-toplevel (forms ctx k)
  "Compile top-level forms with continuation."
  (if (null forms)
      (funcall k nil)
      (compile-ast (car forms) ctx
        (lambda (reg)
          (compile-toplevel (cdr forms) ctx
            (lambda (_) (declare (ignore _))
              (funcall k reg)))))))

;;; ===== Macro (パターン抽象化) =====
(defmacro define-builtin-unary (name vm-class)
  `(def-fact (builtin-unary ,name ,vm-class)))
```

## Phase 1: Package Modularization

### Current State
- Single monolithic package `:cl-cc` with 300+ exports in `src/package.lisp`
- All source files `(in-package :cl-cc)`

### Target State
Split into focused packages with package-local nicknames (SBCL 2.x):

```
:cl-cc/ast        — AST node types, lower-sexp-to-ast, ast-to-sexp
:cl-cc/macro      — Macro system (macro-env, compilation-env, expand)
:cl-cc/reader     — Reader (reader.lisp, reader-printer.lisp)
:cl-cc/cps        — CPS transformation
:cl-cc/prolog     — Prolog engine (unify, query, rules)
:cl-cc/vm         — VM instructions, vm-state, execute, heap
:cl-cc/vm-ext     — VM extension instructions (primitives, io, conditions, list, strings, hash)
:cl-cc/compiler   — compile-ast, compiler-context
:cl-cc/type       — Type system (already has own package, consolidate)
:cl-cc/backend    — Code generation (x86-64, aarch64, regalloc)
:cl-cc/binary     — Binary builders (macho)
:cl-cc            — Re-export facade for backward compat
```

### Requirements
- R1.1: Each package uses `:local-nicknames` for inter-package references
- R1.2: `:cl-cc` facade re-exports all public symbols for test backward compatibility
- R1.3: Minimal cross-package dependencies (DAG, no cycles)
- R1.4: Each package has its own `defpackage` in a dedicated file or section
- R1.5: ASDF `:depends-on` reflects the actual package dependency order

### Example
```lisp
(defpackage :cl-cc/vm
  (:use :cl)
  (:local-nicknames (:ast :cl-cc/ast))
  (:export #:vm-instruction #:vm-state #:execute-program ...))
```

---

## Phase 2: defstruct Migration

### Current State
- All AST nodes: `defclass` with manual `:reader`/`:accessor`
- All VM instructions: `defclass` with manual `:reader`
- All VM heap objects: `defclass`
- Construction everywhere via `(make-instance 'vm-add :dst dst :lhs lhs :rhs rhs)`

### Target State
- AST nodes → `defstruct` with `:include` hierarchy
- VM instructions → `defstruct` with `:include` hierarchy
- VM heap objects → `defstruct`
- VM state / compiler-context → keep `defclass` (stateful, complex lifecycle)

### Key Insight
SBCL `defstruct` types have `structure-class` metaclass — `defmethod` dispatch works on them.
This means all existing `defmethod compile-ast`, `defmethod execute-instruction`,
`defmethod instruction->sexp` continue to work with zero logic changes.

### Requirements
- R2.1: AST base type → `(defstruct ast-node source-file source-line source-column)`
- R2.2: AST subtypes → `(defstruct (ast-int (:include ast-node)) (value 0 :type integer))`
- R2.3: VM instruction base → `(defstruct vm-instruction)`
- R2.4: VM instruction subtypes with `:include` chains (vm-instruction → vm-binop → vm-add)
- R2.5: BOA constructors replace all `make-instance` calls
- R2.6: Slot access via struct accessors (auto-generated, e.g., `ast-int-value`)
- R2.7: Add `:type` declarations to struct slots where types are known
- R2.8: `vm-state`, `compiler-context`, `macro-env`, `compilation-env` remain `defclass`
- R2.9: `vm-program` → `defstruct` (simple data holder)
- R2.10: Heap objects (`vm-cons-cell`, `vm-closure-object`, `vm-heap-address`) → `defstruct`

### Migration Pattern
```lisp
;; Before
(defclass vm-const (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (value :initarg :value :reader vm-value)))
(make-instance 'vm-const :dst dst :value 42)

;; After
(defstruct (vm-const (:include vm-instruction))
  (dst nil :type keyword)
  (value nil))
(make-vm-const :dst dst :value 42)
```

### Accessor Naming Convention
- Struct accessors auto-generate `<struct-name>-<slot-name>`
- Current inconsistency: `vm-dst`, `vm-value`, `vm-reg`, `vm-label-name` (no struct prefix)
- Decision: accept the auto-generated names (e.g., `vm-const-dst`, `vm-const-value`)
- Provide backward-compat `declaim inline` wrappers during transition if needed

---

## Phase 3: Type Declarations and SBCL Optimization

### Requirements
- R3.1: `(declaim (ftype (function (compiler-context) keyword) make-register))`
  for all critical compiler/VM functions
- R3.2: `(declare (type ...))` in hot-path local variables
- R3.3: `(declaim (inline ...))` for small, frequently called helpers
  (e.g., `emit`, `make-register`, `make-label`, `lookup-var`)
- R3.4: `sb-ext:defglobal` for truly global constants (e.g., `*compiler-special-forms*`)
  that never change after load
- R3.5: `(declare (optimize (speed 3) (safety 1)))` in VM executor hot loop
- R3.6: `the` forms for type-known expressions in performance-critical paths

---

## Phase 4: Condition System Enhancement

### Current State
- Errors via `(error "Unbound variable: ~S" sym)` — plain strings
- No restarts, no condition classes, no handler-bind

### Target State
- Custom condition classes for compiler/VM errors
- Restarts for recoverable situations
- Signal/handle separation

### Requirements
- R4.1: Define condition hierarchy:
  ```lisp
  (define-condition cl-cc-error (error) ())
  (define-condition compile-error (cl-cc-error)
    ((form :initarg :form :reader compile-error-form)
     (context :initarg :context :reader compile-error-context)))
  (define-condition unbound-variable-error (compile-error)
    ((name :initarg :name :reader unbound-variable-name)))
  (define-condition vm-runtime-error (cl-cc-error)
    ((instruction :initarg :instruction :reader vm-error-instruction)
     (state :initarg :state :reader vm-error-state)))
  ```
- R4.2: Provide `use-value` and `store-value` restarts for unbound variable
- R4.3: Provide `skip-instruction` restart for non-fatal VM errors
- R4.4: Use `signal` for warnings, `error` for fatal conditions
- R4.5: Replace all plain `(error "string" ...)` with typed conditions

---

## Phase 5: Code Quality and Modern Idioms

### Naming Convention Cleanup
- R5.1: Consistent predicate naming: `*-p` suffix (e.g., `logic-var-p` — already good)
- R5.2: Consistent constructor naming: `make-*` prefix
- R5.3: Special variables: `*name*` earmuffs (already followed)
- R5.4: Constants: `+name+` (not currently used; evaluate need)

### Self-Hosted Utility Macros (no external deps)
- R5.5: Define `when-let` / `if-let` macros in a `cl-cc/utils` package:
  ```lisp
  (defmacro when-let ((var expr) &body body)
    `(let ((,var ,expr))
       (when ,var ,@body)))
  ```
- R5.6: Define `with-gensyms` for macro hygiene
- R5.7: Define `nlet` (named let) for tail-recursive iteration patterns

### Code Organization
- R5.8: Remove verbose section separator comments
  (`;;; ---...---` lines). Use single-line `;;;` section headers instead.
- R5.9: Group related defstruct + defmethod definitions together per concept
- R5.10: Consistent `eval-when` usage — only where truly needed

### Hash Table Improvements
- R5.11: Define `dict` / `make-dict` helper for common `(make-hash-table :test #'eq)` pattern
- R5.12: Consider `do-hash` iteration macro for `maphash` with cleaner syntax

---

## Phase 6: Compiler-Context and VM-State Modernization

### Requirements
- R6.1: `compiler-context` slots: add type declarations for all slots
- R6.2: `vm-state` slots: add type declarations, especially for `registers` hash-table
- R6.3: Extract register allocation from `compiler-context` into a protocol
- R6.4: Use `with-accessors` (standard CL) instead of repeated `(slot ctx)` patterns

---

## Phase 7: print-object による Debug 可観測性

### Current State
- `print-object` メソッドがゼロ。AST ノード、VM 命令、VM state すべて未定義
- REPL で `#<AST-INT {10043A2B03}>` のようなアドレスしか見えない
- デバッグ時に `instruction->sexp` を手動で呼ぶ必要がある

### Requirements
- R7.1: 全 AST ノードに `print-object` — `#<ast-int 42>`, `#<ast-binop + ...>` 形式
- R7.2: 全 VM 命令に `print-object` — `instruction->sexp` の結果を活用
  ```lisp
  (defmethod print-object ((inst vm-const) stream)
    (print-unreadable-object (inst stream :type t)
      (format stream "~S -> ~S" (vm-const-dst inst) (vm-const-value inst))))
  ```
- R7.3: `vm-state` に `print-object` — レジスタ数、PC、call stack depth を表示
- R7.4: `compiler-context` に `print-object` — register/label カウンタ、env depth
- R7.5: `vm-closure-object` に `print-object` — entry-label, param count, captured count

---

## Phase 8: execute-instruction ボイラープレート削減マクロ

### Current State
- 150+ `defmethod execute-instruction` が同じパターンを繰り返す:
  - 156 箇所: `(vm-reg-set state (vm-dst inst) <expr>)`
  - 200 箇所: `(values (1+ pc) nil nil)` (next PC, no jump, no halt)
  - 多くのメソッドで `(declare (ignore labels))` が重複

### Target State
DSL マクロで定義を簡潔に:

```lisp
;; Before (5 lines)
(defmethod execute-instruction ((inst vm-add) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (+ (vm-reg-get state (vm-lhs inst))
                 (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;; After (2 lines)
(define-simple-instruction vm-add (inst state)
  (+ (vm-reg-get state (vm-lhs inst))
     (vm-reg-get state (vm-rhs inst))))
```

### Requirements
- R8.1: `define-simple-instruction` マクロ — dst に値をセットし `(values (1+ pc) nil nil)` を返す
- R8.2: `define-predicate-instruction` マクロ — boolean 結果を dst にセット
- R8.3: `define-jump-instruction` マクロ — 条件付きジャンプのパターン
- R8.4: 複雑な命令（vm-call, vm-ret 等）は従来の defmethod のまま

---

## Phase 9: instruction->sexp / sexp->instruction の自動生成

### Current State
- 205 個の `defmethod instruction->sexp` — ほぼ全てが `(list :keyword slot1 slot2)` 形式
- `sexp->instruction` は巨大な単一 `case` 文で `make-instance` を呼ぶ
- defstruct 移行後は `make-instance` → `make-vm-*` への書き換えも必要

### Target State
defstruct 定義から自動的に instruction->sexp / sexp->instruction を生成:

```lisp
(define-vm-instruction vm-const (vm-instruction)
  "Load constant into register."
  (dst nil :type keyword)
  (value nil)
  (:sexp-tag :const)
  (:sexp-slots dst value))
;; → defstruct, print-object, instruction->sexp, sexp->instruction entry を自動生成
```

### Requirements
- R9.1: `define-vm-instruction` マクロで defstruct + メタデータ一括定義
- R9.2: `instruction->sexp` は sexp-tag と sexp-slots から自動生成
- R9.3: `sexp->instruction` のグローバル dispatch table を自動登録
- R9.4: 特殊なシリアライズが必要な命令はカスタム上書き可能

---

## Phase 10: Standard Library の外部ファイル化

### Current State
- `*standard-library-source*` が compiler.lisp 内に巨大な文字列リテラルとして埋め込み
- mapcar, mapc, reduce 等の標準関数が文字列内で定義
- エディタの syntax highlighting が効かない、typo 検出不能

### Target State
- `stdlib/` ディレクトリに `.lisp` ファイルとして分離
- コンパイル時に `read` して取り込み

### Requirements
- R10.1: `stdlib/core.lisp` — mapcar, mapc, reduce 等の HOF
- R10.2: `stdlib/list.lisp` — set-difference, union-lists 等
- R10.3: `stdlib/sort.lisp` — merge sort 実装
- R10.4: `(defparameter *standard-library-source* (load-stdlib-files))` で統合
- R10.5: ASDF の `:static-file` として stdlib ファイルを登録

---

## Phase 11: gensym 管理と with-gensyms

### Current State
- 210 箇所の `gensym` 呼び出し（主に macro.lisp:63, compiler.lisp:103）
- `with-gensyms` マクロは未定義
- 各所で `(let ((g (gensym "NAME"))) ...)` を手書き

### Requirements
- R11.1: `with-gensyms` マクロを `cl-cc/utils` に定義
  ```lisp
  (defmacro with-gensyms ((&rest names) &body body)
    `(let ,(mapcar (lambda (n) `(,n (gensym ,(symbol-name n)))) names)
       ,@body))
  ```
- R11.2: 複数 gensym を使う箇所を `with-gensyms` に置換

---

## Phase 12: dolist+push+nreverse → loop collect パターン

### Current State
- 126 箇所の `dolist`/`dotimes` vs 62 箇所の `loop`
- `dolist` + `push` + `nreverse` のイディオムが多数（リスト構築パターン）

### Requirements
- R12.1: リスト構築パターンを `loop for x in list collect (f x)` に置換
- R12.2: カウント付き反復を `loop for i below n collect ...` に統一
- R12.3: 副作用のみの反復は `dolist` のまま維持（適材適所）

---

## Phase 13: reader.lisp の defstruct パターン準拠確認

### Current State
- `reader.lisp` は既に `defstruct` を使用（`source-location`, `source-annotated-sexp`）
- プロジェクト内で唯一の defstruct 使用ファイル — 他のファイルとのスタイル不一致

### Requirements
- R13.1: `reader.lisp` の defstruct スタイルを Phase 2 の標準パターンとして参照
- R13.2: `:conc-name` の使用方針を統一（`source-annotated-sexp` は `sa-sexp-` を使用）
- R13.3: VM 命令は `:conc-name` デフォルト（struct-name- prefix）を使用

---

## Phase 14: Type System の defstruct 化

### Current State
- `type-node` 階層（7 クラス）が `defclass` で定義（`src/type/representation.lisp`）
- `type-primitive`, `type-variable`, `type-function`, `type-tuple`, `type-union`,
  `type-intersection`, `type-unknown`
- `make-type-variable` が `make-instance` + グローバルカウンタ

### Requirements
- R14.1: `type-node` → `defstruct` with `:include` 階層
- R14.2: BOA constructors に置換（`make-type-primitive :name 'fixnum`）
- R14.3: `*type-variable-counter*` をコンテキストオブジェクトに移動（グローバル副作用回避）

---

## Phase 15: Backend の defstruct 化 + emit-instruction DSL

### Current State
- `target`, `x86-64-target`, `aarch64-target` — defclass、インスタンス少数
- `calling-convention` — defclass、全2インスタンスのみ
- `live-interval`, `regalloc-result` — defclass、純データ構造
- `emit-instruction` が x86-64/aarch64 で並列ボイラープレート

### Requirements
- R15.1: `calling-convention` → `defstruct`（singleton-like、pure data）
- R15.2: `live-interval`, `regalloc-result` → `defstruct`
- R15.3: `target` 系は `defclass` 維持（`emit-instruction` の multi-dispatch に使用）
- R15.4: `instruction-defs` / `instruction-uses` を Phase 9 の `define-vm-instruction` DSL に統合
  ```lisp
  (define-vm-instruction vm-add (vm-binop)
    "Integer addition."
    (dst nil :type keyword)
    (lhs nil :type keyword)
    (rhs nil :type keyword)
    (:sexp-tag :add)
    (:defs dst)
    (:uses lhs rhs))
  ;; → defstruct + instruction->sexp + instruction-defs + instruction-uses を自動生成
  ```

---

## Phase 16: VM レジスタ管理の最適化

### Current State
- レジスタが `(make-hash-table :test #'equal)` で管理
- レジスタ名は `:R0`, `:R1`, ... のキーワード（`intern (format nil "R~D" n) :keyword`）
- 関数呼び出し毎に `maphash` でレジスタ全コピー（vm-call:768行目）
- `vm-reg-get` / `vm-reg-set` が毎回 `gethash`

### Target State
- レジスタを simple-vector でインデックスアクセス（O(1)）
- レジスタ名をインデックス整数にマッピング（intern + format を回避）
- 関数呼び出し時のコピーを vector copy で高速化

### Requirements
- R16.1: `make-register` がインデックス整数を返す方式に変更検討
- R16.2: `vm-state-registers` を `(simple-vector 256)` + fill-pointer に変更検討
- R16.3: 最低限: レジスタキャッシュテーブルで `intern+format` を回避
  ```lisp
  (defun make-register (ctx)
    (let ((n (ctx-next-register ctx)))
      (incf (ctx-next-register ctx))
      (or (aref *register-cache* n)
          (setf (aref *register-cache* n)
                (intern (format nil "R~D" n) :keyword)))))
  ```
- R16.4: VM call/ret のレジスタ save/restore を `copy-seq` ベースに

---

## Phase 17: VM 実行ループのリターンプロトコル簡素化

### Current State
- `execute-instruction` が `(values pc jump-target halt-value)` の3値を返す
- 200箇所で `(values (1+ pc) nil nil)` — 大半が「次の命令へ進む」
- `execute-vm-loop` で毎回 `multiple-value-bind` で3値受け取り

### Requirements
- R17.1: 通常命令は PC をインクリメント、ジャンプ/停止のみ特殊値を返す方式に
- R17.2: 案: `execute-instruction` が new-pc のみ返す（nil=halt）
  - 通常命令: `(1+ pc)` を返す
  - ジャンプ: jump 先 PC を返す
  - 停止: `nil` を返す（halt 値は state に格納）
- R17.3: Phase 8 の `define-simple-instruction` と連携して自動化

---

## Phase 18: emit-byte / emit-word / emit-dword の効率化

### Current State
- `x86-64-codegen.lisp` の `emit-byte`, `emit-word`, `emit-dword`, `emit-qword` が
  個別の `funcall stream` 呼び出し
- `with-output-to-vector` が `push` + `nreverse` + `coerce` で非効率
- バイト列生成がリスト経由

### Requirements
- R18.1: `with-output-to-vector` を adjustable `(unsigned-byte 8)` vector + `vector-push-extend` に変更
- R18.2: `emit-byte` / `emit-word` 等をインライン宣言
- R18.3: `emit-dword`, `emit-qword` を `(declare (optimize (speed 3)))` で最適化

---

## Phase 19: alist 管理パターンの統一

### Current State
- `ctx-env` (compiler-context): alist `(name . register)` — 38箇所の `assoc`
- `vm-closure-captured-values`: alist `(register . value)`
- Prolog env: alist `(var . term)`
- `acons` 11箇所、`assoc` 38箇所、散発的な `(cons (cons k v) alist)` パターン

### Requirements
- R19.1: `acons` を全ファイルで統一利用（`(push (cons k v) alist)` を `(acons k v alist)` に）
- R19.2: 頻繁な env lookup にはサイズが大きくなる場合 hash-table 切替を検討
- R19.3: `with-extended-env` マクロで env push/pop パターンを統一
  ```lisp
  (defmacro with-extended-env ((ctx bindings) &body body)
    `(let ((old-env (ctx-env ,ctx)))
       (unwind-protect
            (progn
              (setf (ctx-env ,ctx) (append ,bindings (ctx-env ,ctx)))
              ,@body)
         (setf (ctx-env ,ctx) old-env))))
  ```

---

## Phase 20: macho.lisp パターンの全体適用

### Current State
- `binary/macho.lisp` は既に **2026 スタイルのお手本**:
  - `defstruct` with `:type` 宣言
  - `defconstant` with `+name+` naming
  - `(in-package :cl-cc/binary)` — 独立パッケージ
- 残りのコードベースとスタイルが完全に乖離

### Requirements
- R20.1: `macho.lisp` のパターンをプロジェクト全体の標準として採用
- R20.2: `defconstant +name+` パターンを x86-64-codegen.lisp 以外にも展開
- R20.3: binary パッケージのスタイルを「refactoring reference」としてドキュメント化

---

## Phase 21: Compilation Result の構造化

### Current State
- `compile-expression` が plist を返す: `(list :program program :assembly asm :globals globals)`
- 5箇所で `(getf result :program)` / `(getf result :type)` で分解
- 型安全性ゼロ、キーの typo は実行時まで検出不能

### Requirements
- R21.1: `compilation-result` defstruct 定義
  ```lisp
  (defstruct compilation-result
    (program nil :type (or null vm-program))
    (assembly nil :type (or null string))
    (globals nil :type hash-table)
    (type nil))
  ```
- R21.2: `compile-expression`, `compile-string` 等の戻り値を構造体に
- R21.3: `getf` アクセスを struct accessor に一括置換

---

## Phase 22: AST ノードのスロット重複排除

### Current State
- `ast-lambda` と `ast-defun` がほぼ同一のスロットを持つ:
  `params`, `optional-params`, `rest-param`, `key-params`, `body`
- `ast-flet` と `ast-labels` も同一構造: `bindings`, `body`
- defstruct 移行後は `:include` で共通ベースを定義可能

### Requirements
- R22.1: `ast-callable` 共通ベース構造体:
  ```lisp
  (defstruct (ast-callable (:include ast-node))
    (params nil :type list)
    (optional-params nil :type list)
    (rest-param nil)
    (key-params nil :type list)
    (body nil :type list))
  ```
- R22.2: `(defstruct (ast-lambda (:include ast-callable)) (env nil))`
- R22.3: `(defstruct (ast-defun (:include ast-callable)) (name nil :type symbol))`
- R22.4: `ast-flet` / `ast-labels` 共通ベース: `ast-local-fns`

---

## Phase 23: lower-sexp-to-ast の source-location ボイラープレート削減

### Current State
- 全ての `lower-sexp-to-ast` メソッドが `&key source-file source-line source-column` を受け取る
- `make-instance` 呼び出しの度に3つのキーワード引数を繰り返す:
  ```lisp
  (make-instance 'ast-int :value node
                 :source-file source-file
                 :source-line source-line
                 :source-column source-column)
  ```
- 40+ 箇所で同じパターン

### Requirements
- R23.1: source location を動的変数にバインド:
  ```lisp
  (defvar *current-source-file* nil)
  (defvar *current-source-line* nil)
  (defvar *current-source-column* nil)
  ```
- R23.2: `make-ast` ヘルパーマクロ:
  ```lisp
  (defmacro make-ast (type &rest args)
    `(,(intern (format nil "MAKE-~A" type))
      :source-file *current-source-file*
      :source-line *current-source-line*
      :source-column *current-source-column*
      ,@args))
  ```
- R23.3: `lower-sexp-to-ast` のシグネチャ簡素化（&key 不要に）

---

## Phase 24: Test Package の `:import-from` 爆発解消

### Current State
- `tests/package.lisp` に 170+ の `:import-from :cl-cc` シンボル
- 新シンボル追加時にテストパッケージも更新が必要（二重メンテナンス）
- Phase 1 のパッケージ分割でさらに悪化する可能性

### Requirements
- R24.1: テストパッケージで `:use :cl-cc` に切り替え（全シンボルインポート）
  ```lisp
  (defpackage :cl-cc/test
    (:use :cl :fiveam :cl-cc)
    (:export #:run-tests))
  ```
- R24.2: シンボル衝突が起きる場合は `:shadow` で解決
- R24.3: PBT パッケージも同様に簡素化

---

## Phase 25: compiler-macroexpand-all のディスパッチテーブル化

### Current State
- `compiler-macroexpand-all` が巨大な `cond` 文（30+ 分岐）
- HOF マクロ展開（mapcar → dolist ループ等）がインラインで定義
- 新しいマクロ展開ルールの追加が困難

### Requirements
- R25.1: ディスパッチテーブル `*compiler-macro-expanders*`:
  ```lisp
  (defvar *compiler-macro-expanders* (make-hash-table :test #'eq))

  (defmacro define-compiler-expansion (name (form-var) &body body)
    `(setf (gethash ',name *compiler-macro-expanders*)
           (lambda (,form-var) ,@body)))
  ```
- R25.2: 各 HOF 展開を個別の `define-compiler-expansion` に分離
- R25.3: `compiler-macroexpand-all` のコア部分を簡潔に:
  ```lisp
  (let ((expander (gethash (car form) *compiler-macro-expanders*)))
    (if expander
        (compiler-macroexpand-all (funcall expander form))
        ...default-recursion...))
  ```

---

## Phase 26: expand-defstruct の self-hosting 改善

### Current State
- ホスト CL では `defstruct` → 本物の struct
- self-hosting では `expand-defstruct` が `defclass` + `defun` に展開
- インピーダンスミスマッチ: ホストとセルフホストで動作が異なる
- Phase 2 で defstruct 化すると、self-hosting 側も整合性が必要

### Requirements
- R26.1: `expand-defstruct` が `:include` オプションをサポートすることを確認
- R26.2: `expand-defstruct` の展開結果をテストで検証
- R26.3: Phase 2 の defstruct 化がセルフホスト時にも正しく動作することを保証

---

## Phase 27: compile-ast (ast-call) の分解 — 885行メソッド

### Current State
- `(defmethod compile-ast ((node ast-call) ctx)` が **885行**（L1204〜L2088）
- 83箇所の `(builtin-name-p func-sym "NAME")` によるビルトイン関数チェック
- 85箇所の `(return-from compile-ast result-reg)` による早期リターン
- パターン:
  1. unary builtin: name check → compile 1 arg → emit instruction → return
  2. binary builtin: name check → compile 2 args → emit instruction → return
  3. special case: name check → custom logic → return
  4. fallback: compile func → compile args → emit vm-call

### Target State
テーブル駆動のビルトイン関数ディスパッチ:

```lisp
(defvar *builtin-compilers* (make-hash-table :test #'equal))

(defmacro define-builtin-compiler (name (ctx-var args-var result-var) &body body)
  `(setf (gethash ,(string name) *builtin-compilers*)
         (lambda (,ctx-var ,args-var ,result-var) ,@body)))

;; Unary builtins — macro で一括登録
(defmacro define-unary-builtin (cl-name vm-class)
  `(define-builtin-compiler ,cl-name (ctx args result-reg)
     (let ((arg-reg (compile-ast (first args) ctx)))
       (emit ctx (make-instance ',vm-class :dst result-reg :src arg-reg))
       result-reg)))

(define-unary-builtin car vm-car)
(define-unary-builtin cdr vm-cdr)
(define-unary-builtin not vm-not)
;; ... 30+ more
```

### Requirements
- R27.1: `*builtin-compilers*` ディスパッチテーブル
- R27.2: `define-unary-builtin` マクロ（30+ ビルトイン）
- R27.3: `define-binary-builtin` マクロ（20+ ビルトイン）
- R27.4: `define-builtin-compiler` で特殊ケースを個別定義
- R27.5: `compile-ast (ast-call)` のコアロジックを50行以下に縮小

---

## Phase 28: グローバル mutable state の集約

### Current State
- 13+ の `defvar` がグローバルな `make-hash-table` を保持:
  - `*prolog-rules*`, `*accessor-slot-map*`, `*function-type-registry*`
  - `*class-type-registry*`, `*type-alias-registry*`, `*macro-environment*`
  - `*type-variable-counter*`, `*type-var-counter*`（2つの重複カウンタ）
  - `*vm-handler-stacks*`, `*vm-restart-bindings*`
  - `*source-file*`, `*source-input*`, `*source-locations*`
  - `*labels-boxed-fns*`, `*cut-occurred*`, `*current-regalloc*`
- テスト間で状態がリークする可能性
- スレッドセーフティなし

### Requirements
- R28.1: 型変数カウンタの重複排除: `*type-variable-counter*` と `*type-var-counter*` を統一
- R28.2: コンパイラ関連グローバルを `compiler-context` に移動検討:
  - `*accessor-slot-map*` → `ctx-accessor-slot-map`
  - `*function-type-registry*` → `ctx-function-types`
  - `*labels-boxed-fns*` → `ctx-labels-boxed-fns`
- R28.3: テスト用の `with-fresh-state` マクロ:
  ```lisp
  (defmacro with-fresh-compiler-state (&body body)
    `(let ((*accessor-slot-map* (make-hash-table :test #'eq))
           (*function-type-registry* (make-hash-table :test #'eq))
           ...)
       ,@body))
  ```
- R28.4: `defvar` → `defparameter` or `sb-ext:defglobal` の適切な使い分け

---

## Phase 29: compiler.lisp の分割（3700+ 行）

### Current State
- `compiler.lisp` が **3700+ 行** — プロジェクト最大のファイル
- 含まれる機能:
  1. `compiler-context` クラス定義（L1-50）
  2. ヘルパー関数群（L30-50）
  3. `compile-ast` 38メソッド（L54-2088）
  4. `compile-expression` / `compile-toplevel-forms`（L700-800）
  5. `*compiler-special-forms*` / `compiler-macroexpand-all`（L2386-2900）
  6. `expand-defstruct`（L2402-2480）
  7. HOF マクロ展開（L2500-2900）
  8. `*standard-library-source*`（L3457-3604）
  9. `compile-string` / `run-string` / `our-eval`（L3613-3640）
  10. Native codegen（L3650-3683）

### Requirements
- R29.1: `src/compiler/context.lisp` — compiler-context, helpers
- R29.2: `src/compiler/ast-compile.lisp` — compile-ast メソッド群
- R29.3: `src/compiler/builtins.lisp` — ビルトイン関数コンパイル（Phase 27 の DSL）
- R29.4: `src/compiler/macro-expand.lisp` — compiler-macroexpand-all, HOF 展開
- R29.5: `src/compiler/toplevel.lisp` — compile-string, run-string, our-eval, native

---

## Phase 30: Nix ビルド / CI の現代化

### Current State
- `flake.nix` に `checks.tests` が定義済みだが Makefile は `sbcl --non-interactive` を直接実行
- `Makefile` は3ターゲットのみ（test, load, clean）
- `nix flake check` でテスト実行可能だが明示的にドキュメントされていない

### Requirements
- R30.1: `Makefile` に `nix flake check` ターゲット追加
- R30.2: CI 用 `run-ci-tests.lisp` の検証と Nix 統合
- R30.3: `nix develop` 内で `make test` が正しく動作することを保証
- R30.4: flake.nix の `installPhase` を refactoring 後の新ファイル構造に更新

---

## Phase 31: CPS transformer の AST dispatch 統一

### Current State
- `cps-transform-ast` に 24 の `defmethod` — compile-ast と同じパターン
- AST ノードを defstruct 化する Phase 2 と連携が必要
- CPS 変換不要な AST ノードに対するフォールバックなし

### Requirements
- R31.1: Phase 2 の defstruct 化と同時に cps-transform-ast のシグネチャ更新
- R31.2: 未サポート AST ノードに対する `cl-cc-error` condition（Phase 4）
- R31.3: CPS 不要な trivial ノード（ast-int, ast-var）は共通メソッドに統合検討

---

## Phase 32: CPS 変換をコードベース自体に適用

CPS (Continuation-Passing Style) を「コンパイル対象のコード」ではなく
**コンパイラ実装自体**に適用する。3つのレベルで適用可能。

---

### Level A: VM 実行エンジンの CPS 化（Direct Threading）

#### Current State
```lisp
;; Indirect threading: ループ + CLOS dispatch + multiple-value-bind
(loop with pc = 0
      while (< pc (length instructions))
      do (multiple-value-bind (next-pc halted result)
             (execute-instruction (nth pc instructions) state pc labels)
           (when halted (return result))
           (setf pc next-pc)))
```
- 毎サイクル: `nth` による O(n) リスト走査 + `defmethod` CLOS dispatch + 3値 MV-BIND
- Phase 17 で3値プロトコルの簡素化を提案済みだが、CPS はさらに根本的

#### Target State: Direct Threading via CPS
```lisp
;; Pre-compile: 命令列をクロージャの配列に変換
(defun cps-link-instructions (instructions state labels)
  "命令列を直接スレッド化されたクロージャチェーンにリンクする。"
  (let* ((n (length instructions))
         (handlers (make-array n)))
    ;; 各命令のハンドラを生成（次の命令を直接参照）
    (loop for i from 0 below n
          for inst = (aref inst-vec i)
          do (setf (aref handlers i)
                   (make-cps-handler inst state labels handlers i)))
    ;; 実行開始
    (funcall (aref handlers 0))))

;; 各命令ハンドラが次のハンドラを直接呼ぶ（ループなし）
(defun make-vm-const-handler (inst state handlers pc)
  (let ((next (aref handlers (1+ pc)))
        (dst (vm-const-dst inst))
        (val (vm-const-value inst)))
    (lambda ()
      (vm-reg-set state dst val)
      (funcall next))))

;; ジャンプ命令は labels テーブルでハンドラを引く
(defun make-vm-jump-handler (inst labels handlers)
  (let ((target-pc (gethash (vm-label-name inst) labels)))
    (lambda ()
      (funcall (aref handlers target-pc)))))

;; Halt はクロージャから値を返す
(defun make-vm-halt-handler (inst state)
  (let ((reg (vm-reg inst)))
    (lambda ()
      (vm-reg-get state reg))))
```

#### 効果
| Metric | Current (Indirect) | CPS (Direct) |
|--------|:---:|:---:|
| Loop overhead | 毎サイクル | 0 (ループなし) |
| Instruction lookup | `nth` O(n) | 事前解決 O(1) |
| CLOS dispatch | 毎サイクル defmethod | 事前解決 funcall |
| Return protocol | 3値 MV-BIND | 直接 funcall |
| Branch prediction | 不可能（間接） | 可能（直接） |

#### Requirements
- R32A.1: 命令列を `simple-vector` に変換（`nth` O(n) → `aref` O(1)）
- R32A.2: `cps-link-instructions` で命令→クロージャ変換
- R32A.3: `make-cps-handler` のジェネリック dispatch（or Phase 8 DSL 連携）
- R32A.4: vm-call / vm-ret の CPS 版（call stack をクロージャキャプチャで表現）
- R32A.5: `run-compiled` の dual モード: 従来ループ + CPS direct threading

---

### Level B: compile-ast の CPS 化

#### Current State
```lisp
;; compile-ast は「レジスタを返す」パターン
(defmethod compile-ast ((node ast-if) ctx)
  (let* ((cond-reg (compile-ast (ast-if-cond node) ctx))  ; 再帰呼出し
         (dst (make-register ctx))
         ...)
    ...
    dst))  ; レジスタを返す
```
- `compile-ast` 全メソッドが「ASTノードをコンパイルし、結果レジスタを返す」
- ast-call では 85箇所の `(return-from compile-ast result-reg)` で早期リターン
- 制御フローが暗黙的（return-from による非ローカル脱出）

#### Target State: compile-ast with continuation
```lisp
;; compile-ast が継続を受け取る
(defgeneric compile-ast (node ctx k))

(defmethod compile-ast ((node ast-int) ctx k)
  (let ((dst (make-register ctx)))
    (emit ctx (make-vm-const :dst dst :value (ast-int-value node)))
    (funcall k dst)))  ; 継続に結果レジスタを渡す

(defmethod compile-ast ((node ast-if) ctx k)
  (compile-ast (ast-if-cond node) ctx
    (lambda (cond-reg)
      (let ((dst (make-register ctx))
            (else-label (make-label ctx "else"))
            (end-label (make-label ctx "ifend")))
        (emit ctx (make-vm-jump-zero :reg cond-reg :label else-label))
        (compile-ast (ast-if-then node) ctx
          (lambda (then-reg)
            (emit ctx (make-vm-move :dst dst :src then-reg))
            (emit ctx (make-vm-jump :label end-label))
            (emit ctx (make-vm-label :name else-label))
            (compile-ast (ast-if-else node) ctx
              (lambda (else-reg)
                (emit ctx (make-vm-move :dst dst :src else-reg))
                (emit ctx (make-vm-label :name end-label))
                (funcall k dst)))))))))
```

#### 効果
- `return-from compile-ast` が完全に不要に（85箇所削除）
- ビルトイン関数ディスパッチ（Phase 27）が自然に表現可能
- コンパイル途中の「次に何をするか」が明示的
- 将来の tail-call 最適化パスの実装が容易

#### Requirements
- R32B.1: `compile-ast` のシグネチャを `(node ctx k)` に変更
- R32B.2: 全38メソッドの CPS 化
- R32B.3: ast-call のビルトインディスパッチを CPS 継続で表現
- R32B.4: `compile-toplevel-forms` を CPS スタイルに
- R32B.5: テスト互換性の維持（外部 API は同じ結果を返す）

---

### Level C: コンパイラパイプラインの CPS 化

#### Current State
```lisp
;; 線形パイプライン（各段階が次の段階の入力を返す）
source → parse-all-forms → compiler-macroexpand-all → lower-sexp-to-ast
       → compile-ast → optimize-instructions → vm-program → run-compiled
```
- 各段階が同期的に次の段階の入力を生成
- エラーハンドリングが各段階で異なる
- パイプライン中間結果の観測が困難（デバッグ用）

#### Target State: Pipeline with continuations
```lisp
(defun compile-and-run (source &key on-parse on-ast on-compile on-optimize k)
  "CPS パイプライン: 各段階にフックポイント。"
  (parse-source source
    (lambda (forms)
      (when on-parse (funcall on-parse forms))
      (expand-macros forms
        (lambda (expanded)
          (lower-to-ast expanded
            (lambda (ast)
              (when on-ast (funcall on-ast ast))
              (compile-ast-pipeline ast
                (lambda (instructions)
                  (when on-compile (funcall on-compile instructions))
                  (optimize instructions
                    (lambda (optimized)
                      (when on-optimize (funcall on-optimize optimized))
                      (funcall k (run-vm optimized)))))))))))))
```

#### 効果
- パイプライン各段階にフック（ミドルウェア）を挿入可能
- デバッガ/プロファイラを非侵入的に接続
- 段階ごとのエラーリカバリが明示的
- 将来のインクリメンタルコンパイル（変更差分のみ再処理）の基盤

#### Requirements
- R32C.1: 各パイプライン段階に継続パラメータ追加
- R32C.2: フックポイント定義（on-parse, on-ast, on-compile, on-optimize）
- R32C.3: エラー継続（error-k）でパイプラインエラーを統一的に処理
- R32C.4: 従来の同期 API はラッパーとして維持

---

### 実装優先度

| Level | 難易度 | 効果 | Phase 連携 |
|:---:|:---:|:---:|:---|
| **A** (VM Direct Threading) | 高 | 性能大幅向上 | Phase 8, 17 を包含・上位互換 |
| **B** (compile-ast CPS) | 中 | return-from 85箇所削除 | Phase 27 を包含・上位互換 |
| **C** (Pipeline CPS) | 低 | 拡張性・デバッグ性 | Phase 25 と連携 |

Level B → A → C の順で実装すると依存関係がクリーン。

---

## Phase 33: Prolog-Directed Compiler Architecture

CL-CC の Prolog エンジン（unify + backtracking + cut + continuation solver）を
コンパイラパイプラインの**全段階**に拡張する。
命令的コードを宣言的な Prolog ルールに置き換え、
「コンパイラが何をするか」を論理プログラミングで記述する。

### 現在の Prolog 利用状況
- 型推論ルール（type-of, env-lookup） — 10 ルール
- ピープホール最適化（peephole-rules） — 3 ルール
- 合計: 13 ルール（全コードベースの < 1%）

### ビジョン: 「ルール駆動コンパイラ」
```
source → [DCG parse rules] → sexp
       → [lowering rules] → AST
       → [compilation rules] → VM instructions
       → [optimization rules] → optimized instructions
       → [type inference rules] → typed program
       → [register allocation rules] → physical registers
       → [code emission rules] → assembly/machine code
```

各段階が Prolog ルールの集合として記述され、
メタインタプリタが効率的に実行する。

---

### Level 1: lower-sexp-to-ast を Prolog 化

#### Current State
```lisp
;; 40+ の defmethod — 手続き的パターンマッチ
(defmethod lower-sexp-to-ast ((node integer) &key ...)
  (make-instance 'ast-int :value node ...))
(defmethod lower-sexp-to-ast ((node cons) &key ...)
  (case (car node) (if ...) (let ...) (lambda ...) ...))
```

#### Prolog Rules
```lisp
;; リテラル
(def-rule (lower ?val (ast-int ?val))
  (:when (integerp ?val)))
(def-rule (lower ?s (ast-quote ?s))
  (:when (stringp ?s)))
(def-rule (lower ?sym (ast-var ?sym))
  (:when (symbolp ?sym)))

;; Special forms
(def-rule (lower (if ?c ?t ?e) (ast-if ?ac ?at ?ae))
  (lower ?c ?ac)
  (lower ?t ?at)
  (lower ?e ?ae))

(def-rule (lower (let ?bindings . ?body) (ast-let ?ab ?abody))
  (lower-bindings ?bindings ?ab)
  (lower-body ?body ?abody))

(def-rule (lower (lambda ?params . ?body) (ast-lambda ?params nil nil nil ?abody nil))
  (lower-body ?body ?abody))

;; Bindings helper
(def-rule (lower-bindings nil nil))
(def-rule (lower-bindings (cons (?name ?val) ?rest) (cons (?name . ?aval) ?arest))
  (lower ?val ?aval)
  (lower-bindings ?rest ?arest))

;; Body helper
(def-rule (lower-body nil nil))
(def-rule (lower-body (cons ?form ?rest) (cons ?aform ?arest))
  (lower ?form ?aform)
  (lower-body ?rest ?arest))
```

#### Requirements
- R33.1a: `lower` 述語で S式→AST 変換を宣言的に定義
- R33.1b: 現在の40+ defmethod を 60-80 の Prolog ルールに変換
- R33.1c: `lower-sexp-to-ast` をラッパーとして維持:
  ```lisp
  (defun lower-sexp-to-ast (sexp &key ...)
    (let ((result (query-one `(lower ,sexp ?ast))))
      (when result (third result))))
  ```

---

### Level 2: compile-ast を Prolog 化

#### Current State
```lisp
;; 38 の defmethod — 手続き的コンパイル
(defmethod compile-ast ((node ast-int) ctx)
  (let ((dst (make-register ctx)))
    (emit ctx (make-instance 'vm-const :dst dst :value (ast-int-value node)))
    dst))
```

#### Prolog Rules（差分リスト使用）
```lisp
;; 差分リスト (instructions-in, instructions-out) でVM命令を蓄積
;; ?ctx は (register-counter . env) のペア

;; Integer literal
(def-rule (compile (ast-int ?val) ?ctx ?reg ?instrs-in ?instrs-out)
  (fresh-reg ?ctx ?reg ?ctx2)
  (= ?instrs-out (cons (vm-const ?reg ?val) ?instrs-in)))

;; Binary operation
(def-rule (compile (ast-binop ?op ?lhs ?rhs) ?ctx ?dst ?in ?out)
  (compile ?lhs ?ctx ?lreg ?in ?mid1)
  (compile ?rhs ?ctx ?rreg ?mid1 ?mid2)
  (fresh-reg ?ctx ?dst ?ctx3)
  (op-to-instruction ?op ?opclass)
  (= ?out (cons (?opclass ?dst ?lreg ?rreg) ?mid2)))

;; Op → instruction class mapping
(def-fact (op-to-instruction + vm-add))
(def-fact (op-to-instruction - vm-sub))
(def-fact (op-to-instruction * vm-mul))
(def-fact (op-to-instruction = vm-num-eq))
(def-fact (op-to-instruction < vm-lt))
(def-fact (op-to-instruction > vm-gt))

;; If expression
(def-rule (compile (ast-if ?cond ?then ?else) ?ctx ?dst ?in ?out)
  (compile ?cond ?ctx ?creg ?in ?mid1)
  (fresh-reg ?ctx ?dst ?ctx2)
  (fresh-label ?ctx else ?else-label ?ctx3)
  (fresh-label ?ctx ifend ?end-label ?ctx4)
  (= ?mid2 (cons (vm-jump-zero ?creg ?else-label) ?mid1))
  (compile ?then ?ctx ?treg ?mid2 ?mid3)
  (= ?mid4 (cons (vm-move ?dst ?treg) ?mid3))
  (= ?mid5 (cons (vm-jump ?end-label) ?mid4))
  (= ?mid6 (cons (vm-label ?else-label) ?mid5))
  (compile ?else ?ctx ?ereg ?mid6 ?mid7)
  (= ?mid8 (cons (vm-move ?dst ?ereg) ?mid7))
  (= ?out (cons (vm-label ?end-label) ?mid8)))
```

#### Requirements
- R33.2a: `compile` 述語で AST→VM 命令変換を宣言的に定義
- R33.2b: 差分リストパターンの導入（効率的な命令蓄積）
- R33.2c: `fresh-reg`, `fresh-label` を Prolog ビルトインとして追加
- R33.2d: 現在の38 defmethod を 80-120 の Prolog ルールに変換

---

### Level 3: ビルトイン関数ディスパッチを Prolog 化

#### Current State
```lisp
;; 83 の builtin-name-p チェック（885行メソッド内）
(when (builtin-name-p func-sym "CAR")
  (let ((arg-reg (compile-ast (first args) ctx)))
    (emit ctx (make-instance 'vm-car :dst result-reg :src arg-reg))
    (return-from compile-ast result-reg)))
```

#### Prolog Rules
```lisp
;; Unary builtins: 1行1ルール
(def-fact (builtin-unary car vm-car))
(def-fact (builtin-unary cdr vm-cdr))
(def-fact (builtin-unary not vm-not))
(def-fact (builtin-unary length vm-length))
(def-fact (builtin-unary reverse vm-reverse))
(def-fact (builtin-unary symbolp vm-symbol-p))
(def-fact (builtin-unary numberp vm-number-p))
;; ... 30+ more

;; Binary builtins
(def-fact (builtin-binary eq vm-eq))
(def-fact (builtin-binary cons vm-cons))
(def-fact (builtin-binary append vm-append))
(def-fact (builtin-binary mod vm-mod))
(def-fact (builtin-binary min vm-min))
(def-fact (builtin-binary max vm-max))
;; ... 20+ more

;; Generic compilation rules for builtins
(def-rule (compile-call ?name (?arg) ?ctx ?dst ?in ?out)
  (builtin-unary ?name ?vm-class)
  (compile ?arg ?ctx ?areg ?in ?mid)
  (fresh-reg ?ctx ?dst ?ctx2)
  (= ?out (cons (?vm-class ?dst ?areg) ?mid)))

(def-rule (compile-call ?name (?a1 ?a2) ?ctx ?dst ?in ?out)
  (builtin-binary ?name ?vm-class)
  (compile ?a1 ?ctx ?r1 ?in ?mid1)
  (compile ?a2 ?ctx ?r2 ?mid1 ?mid2)
  (fresh-reg ?ctx ?dst ?ctx3)
  (= ?out (cons (?vm-class ?dst ?r1 ?r2) ?mid2)))
```

#### 効果
- 885行メソッド → **50行の汎用ルール + 50行のファクト宣言**
- 新しいビルトインの追加が 1行の `def-fact` で完了
- Phase 27 を**完全に包含**

---

### Level 4: ピープホール最適化の拡張

#### Current State
```lisp
;; 3 ルールのみ
(defparameter *peephole-rules*
  '(((:add ?dst ?a ?z) (:when (:const ?z 0)) (:rewrite (:move ?dst ?a)))
    ...))
```

#### 拡張 Rules
```lisp
;; 代数的簡約
(def-rule (peephole (vm-add ?d ?a ?z) ?next ?result)
  (= ?next (vm-const ?z 0))
  (= ?result (vm-move ?d ?a)))
(def-rule (peephole (vm-mul ?d ?a ?o) ?next ?result)
  (= ?next (vm-const ?o 1))
  (= ?result (vm-move ?d ?a)))
(def-rule (peephole (vm-mul ?d ?a ?z) ?next ?result)
  (= ?next (vm-const ?z 0))
  (= ?result (vm-const ?d 0)))

;; Dead code elimination
(def-rule (peephole (vm-move ?d ?s) ?next ?result)
  (= ?d ?s)
  (= ?result :remove))

;; Constant folding
(def-rule (peephole (vm-add ?d ?a ?b) ?ctx ?result)
  (known-const ?a ?va)
  (known-const ?b ?vb)
  (:when (integerp ?va))
  (:when (integerp ?vb))
  (= ?result (vm-const ?d (+ ?va ?vb))))

;; Jump chain elimination
(def-rule (peephole (vm-jump ?l1) ?labels ?result)
  (label-target ?l1 (vm-jump ?l2))
  (= ?result (vm-jump ?l2)))

;; Redundant load after store
(def-rule (peephole (vm-move ?d ?s) ?prev ?result)
  (= ?prev (vm-move ?s ?d))
  (= ?result :remove))
```

---

### Level 5: マクロ展開を Prolog 化

#### Current State
```lisp
;; compiler-macroexpand-all の30+ 分岐 cond
(cond ((eq (car form) 'mapcar) ...)
      ((eq (car form) 'every) ...)
      ...)
```

#### Prolog Rules
```lisp
;; HOF expansion rules
(def-rule (expand-macro (mapcar ?fn ?lst) ?result)
  (= ?result (let ((acc nil))
               (dolist (x ,?lst) (push (funcall ,?fn x) acc))
               (nreverse acc))))

(def-rule (expand-macro (every ?pred ?lst) ?result)
  (= ?result (block nil
               (dolist (x ,?lst t)
                 (unless (funcall ,?pred x) (return nil))))))

;; Standard CL macros
(def-rule (expand-macro (when ?test . ?body) (if ?test (progn . ?body) nil)))
(def-rule (expand-macro (unless ?test . ?body) (if ?test nil (progn . ?body))))
(def-rule (expand-macro (cond) nil))
(def-rule (expand-macro (cond (?test . ?body) . ?rest)
                        (if ?test (progn . ?body) (cond . ?rest))))
```

---

### Level 6: instruction→sexp / sexp→instruction を Prolog 化

#### Current State
- 205 の defmethod `instruction->sexp` + 巨大な case 文 `sexp->instruction`

#### Prolog Rules（双方向！）
```lisp
;; 双方向変換: 同じルールで serialize/deserialize
(def-fact (inst-sexp (vm-const ?dst ?val) (:const ?dst ?val)))
(def-fact (inst-sexp (vm-move ?dst ?src) (:move ?dst ?src)))
(def-fact (inst-sexp (vm-add ?dst ?lhs ?rhs) (:add ?dst ?lhs ?rhs)))
(def-fact (inst-sexp (vm-sub ?dst ?lhs ?rhs) (:sub ?dst ?lhs ?rhs)))
(def-fact (inst-sexp (vm-mul ?dst ?lhs ?rhs) (:mul ?dst ?lhs ?rhs)))
(def-fact (inst-sexp (vm-label ?name) (:label ?name)))
(def-fact (inst-sexp (vm-jump ?label) (:jump ?label)))
;; ... 100+ more

;; Usage:
;; (query-one '(inst-sexp (vm-add :R0 :R1 :R2) ?sexp))  → (:add :R0 :R1 :R2)
;; (query-one '(inst-sexp ?inst (:add :R0 :R1 :R2)))     → (vm-add :R0 :R1 :R2)
```

#### 効果
- **双方向性**: 同じルールで serialize も deserialize も可能
- 205 defmethod + 1 giant case → **100 行のファクト宣言**
- Phase 9 を**完全に包含・上位互換**

---

### Level 7: レジスタ割り当てを制約論理プログラミングで

#### Current State
- `regalloc.lisp` の線形スキャンアルゴリズム（手続き的）
- `instruction-defs` / `instruction-uses` の 30+ defmethod

#### Prolog + 制約
```lisp
;; レジスタ干渉制約
(def-rule (no-conflict ?v1 ?v2)
  (/= ?v1 ?v2)
  (not (interfere ?v1 ?v2)))

;; 割り当て探索（バックトラッキングで最適解探索）
(def-rule (allocate ?vreg ?preg)
  (physical-reg ?preg)
  (not (allocated ?preg))
  (forall (interfering-vreg ?vreg ?other)
          (allocated-to ?other ?other-preg)
          (/= ?preg ?other-preg)))
```

---

### Prolog エンジン拡張の必要事項

現在のエンジンで不足する機能:

| 機能 | 現状 | 必要な拡張 |
|:---|:---:|:---|
| 差分リスト | なし | 命令蓄積に必須 |
| assert/retract | なし | 動的ルール変更（fresh-reg カウンタ等） |
| findall/bagof | なし | 全解収集（最適化候補列挙） |
| 算術評価 (is) | `(:when (integerp ?x))` のみ | `(is ?x (+ ?a ?b))` で算術計算 |
| DCG (文法規則) | なし | パーサを Prolog で書く場合に必要 |
| Tabling/Memo | なし | 型推論の収束保証 |
| not/negation-as-failure | `(/=)` のみ | 汎用否定 |
| forall | なし | 制約充足に必要 |

### Requirements
- R33.E1: 差分リストのサポート（Prolog の標準技法）
- R33.E2: `is` ビルトインで算術評価
- R33.E3: `findall` / `bagof` で全解収集
- R33.E4: `assert` / `retract` で動的ルール変更
- R33.E5: `not` (negation-as-failure) の汎用サポート
- R33.E6: tabling（メモ化）で再帰的型推論の収束保証
- R33.E7: パフォーマンス: ルール索引（first-argument indexing）

---

### 段階的移行計画

| Step | 対象 | ルール数 | 削除コード |
|:---:|:---|:---:|:---|
| 1 | instruction→sexp 双方向化 (Level 6) | ~100 facts | 205 defmethod + case 文 |
| 2 | ビルトインディスパッチ (Level 3) | ~60 facts + 3 rules | 885行メソッドの80% |
| 3 | マクロ展開 (Level 5) | ~40 rules | cond 30分岐 |
| 4 | ピープホール拡張 (Level 4) | ~20 rules | 3→20 ルール |
| 5 | lower-sexp-to-ast (Level 1) | ~80 rules | 40 defmethod |
| 6 | compile-ast (Level 2) | ~120 rules | 38 defmethod |
| 7 | レジスタ割当て (Level 7) | ~30 rules | regalloc.lisp |

### 最終形

```
Prolog ルール総数: ~450 rules/facts
削除される CL コード: ~3000+ 行（compiler.lisp のほぼ全体）
残る命令的コード: VM 実行エンジン、バイナリ生成、I/O
```

---

### Prolog 化 vs CPS 化 vs テーブル駆動の関係

| Phase | アプローチ | Prolog 包含? |
|:---:|:---|:---:|
| 8 | execute-instruction DSL マクロ | 別軸（VM実行は命令的） |
| 9 | instruction→sexp 自動生成 | **Level 6 が上位互換**（双方向） |
| 25 | macroexpand dispatch table | **Level 5 が上位互換** |
| 27 | ast-call テーブル駆動 | **Level 3 が上位互換** |
| 32B | compile-ast CPS 化 | **Level 2 と直交**（CPS + Prolog 併用可能） |

---

## Non-Goals (Explicitly Out of Scope)

- Adding external library dependencies (Alexandria, Serapeum, Trivia, etc.)
- Changing the fundamental architecture (CLOS dispatch for compile-ast stays)
- Rewriting the Prolog engine or CPS transformer algorithms
- Adding new language features or new backends
- Changing the test framework (FiveAM stays)

---

## Migration Strategy

Large-scale rewrite approach. 6原則に従い優先順位を再編成:

### Step 0: Dead Code 削除（原則5 — 即時実行）
- dead exports 削除: `vm-frame`, `vm-restore-frame`, `vm-frame-pointer`, `vm-alloc-size`
- `*enable-prolog-peephole*` → 常時有効化
- セクション区切り `;;; ---...---` 削除（原則6）
- 未使用トップレベルファイル調査

### Step 1: Data 層（原則3 — Data と Logic の分離）
- Phase 2: defstruct 化（AST, VM命令, heap objects）
- Phase 13, 14, 15: reader/type/backend の defstruct 化
- Phase 22: AST スロット重複排除（ast-callable 共通ベース）
- Phase 21: compilation-result 構造化

### Step 2: Prolog エンジン拡張（原則1 の基盤）
- Phase 33.E: 差分リスト, is, findall, assert, not, tabling
- Prolog ファクト/ルール索引の高速化

### Step 3: Prolog 化 — 宣言的ロジック（原則1 + 原則4）
- Phase 33 Level 6: inst-sexp 双方向化（205 defmethod → 100 facts）
- Phase 33 Level 3: ビルトインディスパッチ（885行 → 50 facts + 3 rules）
- Phase 33 Level 5: マクロ展開 Prolog 化（30+ cond → 40 rules）
- Phase 33 Level 4: ピープホール拡張（3→20 rules）
- Phase 33 Level 1: lower-sexp-to-ast（40 defmethod → 80 rules）
- Phase 33 Level 2: compile-ast（38 defmethod → 120 rules）

### Step 4: CPS 化 — 明示的制御フロー（原則2 + 原則4）
- Phase 32B: compile-ast CPS 化（return-from 92箇所 → 0）
- Phase 32A: VM direct threading
- Phase 32C: パイプライン CPS 化

### Step 5: Macro DSL（原則1 + 原則4）
- Phase 8, 9: define-vm-instruction マクロ
- Phase 23: make-ast ヘルパー
- Phase 11: with-gensyms, when-let 等ユーティリティ
- Phase 19: with-extended-env

### Step 6: 構造整理（原則4 + 原則6）
- Phase 1: Package 分割
- Phase 29: compiler.lisp 分割（3700行→5ファイル）
- Phase 24: Test package 簡素化
- Phase 10: stdlib 外部ファイル化

### Step 7: 品質仕上げ（原則6）
- Phase 3, 6: 型宣言, SBCL 最適化
- Phase 4: Condition system
- Phase 7: print-object
- Phase 12: loop collect 統一
- Phase 30: Nix/CI 統一

### Legacy Step 1: Infrastructure (Phase 5, 11)
- `cl-cc/utils` パッケージ作成: `when-let`, `if-let`, `with-gensyms`, `nlet`, `dict`, `do-hash`
- セクション区切り削除

### Step 2: Package Split (Phase 1)
- 新パッケージ定義、コード移動、`in-package` 更新

### Step 3: defstruct Migration (Phase 2, 13)
- VM instructions → AST nodes → heap objects を defstruct 化
- `make-instance` → `make-*` 一括置換
- accessor 名の統一

### Step 4: VM Instruction DSL (Phase 8, 9)
- `define-vm-instruction` マクロ作成
- `define-simple-instruction` で boilerplate 削減
- `instruction->sexp` / `sexp->instruction` 自動生成

### Step 5: print-object (Phase 7)
- 全 AST ノード、VM 命令、VM state に print-object 追加

### Step 6: Type Declarations (Phase 3, 6)
- `declaim`, `declare`, `inline`, `sb-ext:defglobal` 追加

### Step 7: Condition System (Phase 4)
- condition hierarchy 定義、plain error 置換

### Step 8: Code Modernization (Phase 10, 12)
- Standard library を外部ファイルに分離
- dolist+push+nreverse → loop collect 置換
- with-gensyms 適用

### Validation
- All 1144 tests pass after each step
- `nix develop --command make test` as gate
- Property-based tests (PBT) continue to hold

---

## Quantitative Summary

| Metric | Current | After |
|--------|:---:|:---:|
| Packages | 1 monolithic (+2 small) | 11 modular |
| defclass (data types) | ~100+ | ~5 (stateful objects only) |
| defstruct | 5 (reader + macho) | ~120+ |
| `make-instance` calls | ~500+ | ~10 (defclass only) |
| `(values (1+ pc) nil nil)` | 200 | 0 (macro/protocol absorbed) |
| `print-object` methods | 0 | ~50+ |
| `declaim`/`declare` | 0 | ~100+ |
| `gensym` without `with-gensyms` | 210 | ~30 |
| Section separator `---` lines | ~80 | 0 |
| Stdlib as embedded string | 1 giant string | 3+ `.lisp` files |
| `intern+format` in make-register | every call | cached |
| HT copy on vm-call | maphash loop | vector copy |
| `instruction-defs`/`uses` boilerplate | ~30 methods | DSL-generated |
| `emit-byte` push+nreverse | list-based | vector-push-extend |
| `getf result :program` | 5 plist accesses | struct accessor |
| Source-location 3-arg repeat | 40+ make-instance | `make-ast` macro |
| Test `:import-from` symbols | 170+ | `:use :cl-cc` |
| `compiler-macroexpand-all` cond | 30+ branches | dispatch table |
| AST slot duplication | lambda/defun 5 slots x2 | shared `ast-callable` |
| `compile-ast (ast-call)` | 885 lines, 83 builtin checks | table-driven, ~50 lines core |
| Global mutable `defvar` HTs | 13+ | context-local or `with-fresh-state` |
| `compiler.lisp` | 3700+ lines | 5 files (~700 lines each) |
| Type var counters | 2 (duplicated) | 1 unified |

## File Count Impact Estimate

| Category | Current | After |
|----------|:---:|:---:|
| Package definitions | 3 | ~12 (one per package) |
| Source files | ~25 | ~28 (+ utils, conditions, stdlib) |
| Test files | ~20 | ~20 (facade compat) |
| Stdlib files | 0 | 3-4 |
| New macro/DSL files | 0 | 1-2 (vm-dsl.lisp) |
