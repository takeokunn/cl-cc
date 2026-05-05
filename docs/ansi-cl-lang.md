# ✅ ANSI CL: Language Core

Evaluation/compilation, lambda lists, types/classes, data/control flow, iteration, CLOS (object system), structures, conditions/restarts, symbols, packages, numbers, characters.

このチェックマークは**文書の整備完了**を示します。各節の `✅` / `—` は cl-cc 実装状況または repo で確認できた根拠の有無を表します。

---

## 1. 評価・コンパイル (ANSI CL Ch.3)

### 1.1 特殊形式

| 形式                         | 状態 | 備考                                                                                                                    |
| ---------------------------- | ---- | ----------------------------------------------------------------------------------------------------------------------- |
| `quote`                      | ✅   |                                                                                                                         |
| `if`                         | ✅   |                                                                                                                         |
| `progn`                      | ✅   |                                                                                                                         |
| `let` / `let*`               | ✅   | `let*` はマクロ展開で `let` にデシュガー；FR-623: `(let () body)` → `(progn body)` 対応済み                             |
| `flet` / `labels`            | ✅   | FR-623: `(flet () body)` / `(labels () body)` → `(progn body)` 対応済み                                                 |
| `macrolet`                   | ✅   |                                                                                                                         |
| `symbol-macrolet`            | ✅   | FR-220: `expand-symbol-macrolet-form` で `*symbol-macro-table*` にバインド、`compiler-macroexpand-all` アトムパスで展開 |
| `lambda` / `function` (`#'`) | ✅   |                                                                                                                         |
| `setq` / `psetq`             | ✅   |                                                                                                                         |
| `block` / `return-from`      | ✅   |                                                                                                                         |
| `tagbody` / `go`             | ✅   |                                                                                                                         |
| `catch` / `throw`            | ✅   | `vm-establish-catch`/`vm-throw` VM命令実装済み; タグベースのEQ一致、ネスト対応、非局所脱出動作                          |
| `unwind-protect`             | ✅   |                                                                                                                         |
| `multiple-value-call`        | ✅   |                                                                                                                         |
| `multiple-value-prog1`       | ✅   |                                                                                                                         |
| `the`                        | ✅   |                                                                                                                         |
| `locally`                    | ✅   | FR-397: `(let () decl body)` に展開して宣言を保持                                                                       |
| `load-time-value`            | ✅   | コンパイル時 `eval` で評価し `(quote result)` に展開; `macros-compat.lisp`                                              |
| `eval-when`                  | ✅   | `:compile-toplevel`/`:execute`/`:load-toplevel` の3状況対応; `expander.lisp`                                            |
| `progv`                      | ✅   |                                                                                                                         |

### 1.2 評価・コンパイル API

| 関数・マクロ                                                 | 状態 | 備考                                                                                                          |
| ------------------------------------------------------------ | ---- | ------------------------------------------------------------------------------------------------------------- |
| `eval`                                                       | ✅   | `our-eval` で自己ホスト対応                                                                                   |
| `macroexpand` / `macroexpand-1`                              | ✅   | FR-631: VM命令 + ビルトイン登録済み                                                                           |
| `compiler-macroexpand-all`                                   | ✅   | cl-cc 独自拡張                                                                                                |
| `define-compiler-macro`                                      | ✅   | FR-365: マクロ名を返却 (コンパイル時展開なし; ANSI 互換 no-op)                                                |
| `compiler-macro-function` / `(setf compiler-macro-function)` | ✅   | FR-365: `stdlib-source.lisp` で実装 (常に nil 返却)                                                           |
| `macro-function` / `(setf macro-function)`                   | ✅   | FR-428: 読み取りはホストブリッジ; `(setf macro-function)` は `%set-macro-function` stdlib + setf ハンドラ実装 |
| `*macroexpand-hook*`                                         | ✅   | FR-429: `stdlib-source.lisp` で defvar 定義                                                                   |
| `with-compilation-unit`                                      | ✅   | FR-363: `macros-stdlib.lisp` でスタブ実装                                                                     |
| `compile` (関数)                                             | ✅   | FR-512: ホストブリッジ経由で委譲                                                                              |
| `compile-file`                                               | ✅   | `packages/expand/src/macros-stdlib.lisp: compile-file` shim; `our-load` を呼び、3値を返す                     |
| `constantp` / `special-operator-p`                           | ✅   | FR-538: `stdlib-source.lisp` で実装                                                                           |
| `fdefinition` / `(setf fdefinition)`                         | ✅   | FR-548: `fdefinition` (読み取り) + `set-fdefinition` (書き込み) 両方 `stdlib-source.lisp` で実装              |
| `function-lambda-expression`                                 | ✅   | FR-549: `stdlib-source.lisp` で実装 (常に nil 返却)                                                           |
| `identity` / `constantly` / `complement`                     | ✅   | FR-597: `builtin-registry-data.lisp` で unary ビルトイン登録済み + `stdlib-source.lisp` で定義                |
| `defun` / `defmacro` ドキュメント文字列                      | ✅   | FR-607: expander で先頭 docstring を自動ストリップ (defun/lambda 両対応)                                      |

#### FR-548: fdefinition / (setf fdefinition)

- **対象**: `packages/vm/src/vm.lisp`, `packages/compile/src/builtin-registry.lisp`
- **内容**: `(fdefinition name)` — 関数名 (シンボルまたは `(setf name)`) から関数オブジェクト取得。`(setf (fdefinition name) fn)` — 設定
- **根拠**: ANSI CL 5.3 — fdefinition
- **難易度**: Low

#### FR-549: function-lambda-expression

- **対象**: `packages/vm/src/vm.lisp`
- **内容**: `(function-lambda-expression fn)` → `(lambda-expression closure-p name)` の3値。必要な場合は `nil` を返すが、API 自体は提供される。
- **根拠**: ANSI CL 5.3 — introspection
- **難易度**: Medium

## 2. ラムダリスト (ANSI CL Ch.3.4)

| 機能                                  | 状態 | 備考                                                                                                     |
| ------------------------------------- | ---- | -------------------------------------------------------------------------------------------------------- |
| Required parameters                   | ✅   |                                                                                                          |
| `&optional` パラメータ + デフォルト値 | ✅   |                                                                                                          |
| `&optional` supplied-p 変数           | ✅   | FR-696: `allocate-defaulting-params` でレジスタ割当、`emit-supplied-p-checks` でセンチネル比較コード生成 |
| `&rest` パラメータ                    | ✅   |                                                                                                          |
| `&key` パラメータ + デフォルト値      | ✅   | `((:external internal) default supplied-p)` 形式の明示 keyword 名も保持・実行                            |
| `&key` supplied-p 変数                | ✅   | FR-696: 同上                                                                                             |
| `&allow-other-keys`                   | ✅   | `expander.lisp:295`                                                                                      |
| `&aux` パラメータ                     | ✅   |                                                                                                          |
| `&body` (マクロラムダリスト)          | ✅   |                                                                                                          |
| `&whole` (マクロラムダリスト)         | ✅   |                                                                                                          |
| `&environment` (マクロラムダリスト)   | ✅   | FR-394: `parse-lambda-list` で解析・`our-defmacro` で環境オブジェクトに束縛; 環境はマクロ定義テーブル    |
| Destructuring lambda list             | ✅   | `destructuring-bind` 実装済み                                                                            |
| Specialized lambda list (CLOS)        | ✅   | `defmethod` で使用                                                                                       |
| BOA constructor lambda list           | ✅   | `expander-defstruct.lisp`                                                                                |
| `call-arguments-limit` 定数           | ✅   | FR-551: `stdlib-source.lisp` で実装                                                                      |
| `lambda-parameters-limit` 定数        | ✅   | FR-551: `stdlib-source.lisp` で実装                                                                      |
| `lambda-list-keywords` 変数           | ✅   | FR-551: `stdlib-source.lisp` で実装                                                                      |

#### FR-551: ラムダリスト定数・変数 — ✅ COMPLETE

- **対象**: `packages/vm/src/vm.lisp`
- **実装**: `call-arguments-limit` / `lambda-parameters-limit` / `multiple-values-limit` / `lambda-list-keywords` は起動時に VM global として登録済み。
- **内容**: これらの制限値とラムダリストキーワードを標準 CL と同じ名前で提供する。
- **根拠**: ANSI CL 3.4 / 12.1 — implementation limits
- **難易度**: Easy

---

## 3. 型・クラス (ANSI CL Ch.4)

| 機能                                                  | 状態 | 備考                                                                                                      |
| ----------------------------------------------------- | ---- | --------------------------------------------------------------------------------------------------------- |
| `deftype` (2引数形式)                                 | ✅   |                                                                                                           |
| `deftype` (lambda-list 付き)                          | ✅   | FR-430: ANSI 形式 `(deftype name (params) body)` 対応; パラメータ付きはラムダ展開関数を登録               |
| `define-symbol-macro`                                 | ✅   | FR-398: `*symbol-macro-table*` にグローバルシンボルマクロを登録                                           |
| `typep`                                               | ✅   |                                                                                                           |
| `subtypep`                                            | ✅   | FR-624: ホストブリッジ経由で SBCL `subtypep` に委譲                                                       |
| `type-of`                                             | ✅   | FR-625: float は `double-float`/`single-float`、vm-closure-object は `function` を返却                    |
| `class-of`                                            | ✅   | FR-385: VM命令 `vm-class-of-fn` + ビルトイン登録済み                                                      |
| `find-class` / `(setf find-class)`                    | ✅   | FR-552: 読み取りは `vm-find-class`; `(setf find-class)` は `%set-find-class` stdlib + setf ハンドラ実装   |
| `coerce`                                              | ✅   | FR-630: クォート済み型→直接展開 + `character`/`float` 追加; 動的型→ `%coerce-runtime` stdlib ディスパッチ |
| `upgraded-array-element-type`                         | ✅   | FR-553: `stdlib-source.lisp` で実装 (常に t 返却)                                                         |
| `upgraded-complex-part-type`                          | ✅   | FR-553: `stdlib-source.lisp` で実装 (常に real 返却)                                                      |
| `standard-object` / `structure-object` 組み込みクラス | ✅   | FR-528: stdlib-source.lisp で空 defclass 定義                                                             |

#### FR-552: find-class / (setf find-class)

- **対象**: `packages/vm/src/vm-clos.lisp`
- **内容**: `(find-class name &optional errorp environment)` — クラスオブジェクト取得。`(setf (find-class name) class)` — 登録
- **根拠**: ANSI CL 7.7.6 — find-class
- **難易度**: Low

#### FR-553: upgraded-array-element-type / upgraded-complex-part-type

- **対象**: `packages/vm/src/primitives.lisp`, `packages/compile/src/builtin-registry.lisp`
- **内容**: `upgraded-array-element-type type &optional env` — 型指定子を実際のアレイ element-type に昇格。`upgraded-complex-part-type` 同様
- **根拠**: ANSI CL 15.1.2.1, 12.1.5.2
- **難易度**: Medium

### 3.2 複合型指定子 (Compound Type Specifiers)

ANSI CL の型システムは原子型だけでなく合成型指定子をサポートする。`typep` / `subtypep` / `the` / `ftype` で使用。

| 型指定子                                   | 状態 | 備考                                                                                                                                                                                                                                      |
| ------------------------------------------ | ---- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `(member obj...)`                          | ✅   | `vm-typep-check` で直接 `(member value ...)` ディスパッチ                                                                                                                                                                                 |
| `(satisfies predicate)`                    | ✅   | FR-581: `vm-typep-check` で `(satisfies pred)` → `(funcall pred value)` 直接ディスパッチ                                                                                                                                                  |
| `(and type...)`                            | ✅   | `vm-typep-check` で再帰的 `every` ディスパッチ                                                                                                                                                                                            |
| `(or type...)`                             | ✅   | `vm-typep-check` で再帰的 `some` ディスパッチ                                                                                                                                                                                             |
| `(not type)`                               | ✅   | `vm-typep-check` で再帰的 `not` ディスパッチ                                                                                                                                                                                              |
| `(eql x)`                                  | ✅   | `vm-typep-check` で直接 `eql` ディスパッチ                                                                                                                                                                                                |
| `(values type...)`                         | ✅   | FR-581: `vm-typep-check` で `(values ...)` は常に T (単一値コンテキスト)                                                                                                                                                                  |
| `(function (args) return)`                 | ✅   | FR-581: `vm-typep-check` で functionp/vm-closure-object-p チェック                                                                                                                                                                        |
| `(integer low high)` 範囲                  | ✅   | ホスト CL `typep` 委譲                                                                                                                                                                                                                    |
| `(float low high)` 範囲                    | ✅   | ホスト CL `typep` 委譲                                                                                                                                                                                                                    |
| `(rational ...)` / `(real ...)`            | ✅   | ホスト CL `typep` 委譲                                                                                                                                                                                                                    |
| `(mod n)`                                  | ✅   | ホスト CL `typep` 委譲                                                                                                                                                                                                                    |
| `(array element-type dimensions)`          | ✅   | FR-581: `array-element-type` → `T` (全配列が全型受入), `upgraded-array-element-type` → `T`, `adjustable-array-p` → `T`, `array-has-fill-pointer-p` → `NIL`; `array-rank`/`array-dimensions`/`array-dimension`/`array-total-size` 完全動作 |
| `(simple-array ...)` / `(vector ...)` 複合 | ✅   | FR-581: `vm-typep-check` の `otherwise` ブランチでホスト CL `typep` に委譲; 複合型は動作                                                                                                                                                  |
| `(complex type)`                           | ✅   | ホスト CL `typep` 委譲                                                                                                                                                                                                                    |
| `fixnum` / `bignum` / `ratio` 型           | ✅   | `fixnum` は `vm-typep-check` case ブランチ; `bignum`/`ratio` はホスト CL `typep` 委譲で動作                                                                                                                                               |

#### FR-581: 複合型指定子完全サポート

- **対象**: `packages/vm/src/primitives.lisp` の `vm-typep-check`, `packages/compile/src/codegen.lisp`
- **内容**: ANSI CL 4.2 の全複合型指定子。`satisfies` では述語関数を呼び出し。`values` では多値型チェック。`(integer low high)` 範囲テスト
- **根拠**: ANSI CL 4.2 — Type Specifiers
- **難易度**: Medium

---

## 4. データ・制御フロー (ANSI CL Ch.5)

### 4.1 条件分岐

| 形式                     | 状態 | 備考                                                                                 |
| ------------------------ | ---- | ------------------------------------------------------------------------------------ |
| `when` / `unless`        | ✅   |                                                                                      |
| `cond`                   | ✅   |                                                                                      |
| `and` / `or` / `not`     | ✅   |                                                                                      |
| `case` / `ecase`         | ✅   | FR-354: `ecase` now signals `type-error` with datum and expected-type                |
| `ccase`                  | ✅   | FR-354/FR-426: `type-error` をシグナルし、`store-value` リスタートで値を差し替え可能 |
| `typecase` / `etypecase` | ✅   | FR-354: `etypecase` now signals `type-error` with datum and expected-type            |
| `ctypecase`              | ✅   | FR-354/FR-426: `type-error` をシグナルし、`store-value` リスタートで値を差し替え可能 |

### 4.2 代入・変更

| 形式                   | 状態 | 備考                                                                                                                       |
| ---------------------- | ---- | -------------------------------------------------------------------------------------------------------------------------- |
| `setf`                 | ✅   |                                                                                                                            |
| `psetf`                | ✅   |                                                                                                                            |
| `shiftf`               | ✅   |                                                                                                                            |
| `rotatef`              | ✅   | FR-690: N引数対応; 2引数は `setq` で最適化、3+引数は `setf` chain                                                          |
| `define-modify-macro`  | ✅   |                                                                                                                            |
| `defsetf` (短/長形式)  | ✅   | FR-355: 短形式 `(defsetf acc upd)` と長形式 `(defsetf acc (args) (store) body)` を `*setf-compound-place-handlers*` に登録 |
| `define-setf-expander` | ✅   | FR-355: `*setf-compound-place-handlers*` に展開関数を登録; `get-setf-expansion` 連携                                       |
| `get-setf-expansion`   | ✅   | FR-355: stdlib-source.lisp で基本実装 (symbol → setq, compound → setf)                                                     |

### 4.3 関数呼び出し・多値

| 形式                                                       | 状態 | 備考                                                                                    |
| ---------------------------------------------------------- | ---- | --------------------------------------------------------------------------------------- |
| `apply` / `funcall`                                        | ✅   |                                                                                         |
| `values` / `values-list`                                   | ✅   |                                                                                         |
| `multiple-value-bind`                                      | ✅   |                                                                                         |
| `multiple-value-setq`                                      | ✅   |                                                                                         |
| `(setf (values a b ...) expr)` — values を setf 場所に使用 | ✅   | FR-603: `*setf-compound-place-handlers*` で `multiple-value-bind` + `setq` chain に展開 |
| `multiple-value-list`                                      | ✅   |                                                                                         |
| `nth-value`                                                | ✅   | FR-402: 定数N→`multiple-value-bind`に直接展開; 変数N→`multiple-value-list`+`nth`        |
| `multiple-values-limit` 定数                               | ✅   | FR-551: `stdlib-source.lisp` で実装                                                     |

### 4.4 等価述語

| 関数                   | 状態 | 備考                                                                             |
| ---------------------- | ---- | -------------------------------------------------------------------------------- |
| `eq`                   | ✅   | 同一オブジェクト比較                                                             |
| `eql`                  | ✅   | `eq` + 数値・文字の値比較                                                        |
| `equal`                | ✅   | 構造的等価 (cons tree / string / bit-vector)                                     |
| `equalp`               | ✅   | FR-582: `stdlib-source.lisp` で数値・文字・文字列・cons・vector・hash-table 対応 |
| `=` (数値等価)         | ✅   |                                                                                  |
| `string=` / `char=` 等 | ✅   | 型固有等価                                                                       |

#### FR-582: equalp

- **対象**: `packages/vm/src/primitives.lisp`, `packages/compile/src/builtin-registry.lisp`
- **内容**: `equalp` — 数値は `=` で比較、文字・文字列は case-insensitive、vector は要素ごと、hash-table はカウント+キー+値再帰比較
- **根拠**: ANSI CL 5.3 — equalp; `:test #'equalp` のハッシュテーブル (FR-565) に必要
- **難易度**: Medium

### 4.5 `the` の多値・型宣言

| 機能                                                                           | 状態 | 備考                                                                          |
| ------------------------------------------------------------------------------ | ---- | ----------------------------------------------------------------------------- |
| `(the type form)`                                                              | ✅   |                                                                               |
| `(the (values type...) form)`                                                  | ✅   | FR-583: `vm-values-typep` で多値列を実行時チェックし、不一致なら `type-error` |
| `(ignore-errors form)` → `(values result nil)` または `(values nil condition)` | ✅   | FR-691: `handler-case` で `(values nil condition)` を正しく返却               |

#### FR-583: the による多値型宣言

- **対象**: `packages/compile/src/codegen-core-control.lisp`, `packages/vm/src/vm-execute-mv.lisp`, `packages/vm/src/vm-instructions.lisp`, `packages/ast/src/ast.lisp`
- **内容**: `(the (values integer float) ...)` のような多値型宣言を AST で保持し、`vm-values-typep-check` が required / optional / rest の値列を実行時検証する。
- **根拠**: ANSI CL 3.4.3 — THE with values
- **難易度**: Medium

### 4.6 順次実行・繰り返し補助

| 形式              | 状態 | 備考 |
| ----------------- | ---- | ---- |
| `prog1` / `prog2` | ✅   |      |
| `prog` / `prog*`  | ✅   |      |

---

## 5. 繰り返し (ANSI CL Ch.6)

### 5.1 基本繰り返し

| 形式                 | 状態 | 備考 |
| -------------------- | ---- | ---- |
| `do` / `do*`         | ✅   |      |
| `dolist` / `dotimes` | ✅   |      |

### 5.2 LOOP マクロ

`*loop-iter-emitters*` / `*loop-acc-emitters*` データ駆動実装済み (70テスト)。

| 句                                                    | 状態 | 備考                                                     |
| ----------------------------------------------------- | ---- | -------------------------------------------------------- |
| `for`/`as :from/:to/:below/:upto/:by`                 | ✅   | `loop-parser.lisp:118-130`                               |
| `for`/`as :downfrom/:downto/:above`                   | ✅   | FR-695: `loop-parser.lisp` + `loop-emitters.lisp` で実装 |
| `for :in` / `for :on`                                 | ✅   |                                                          |
| `for :across`                                         | ✅   |                                                          |
| `for :hash-keys` / `for :hash-values`                 | ✅   |                                                          |
| `for := ...` (等式初期化)                             | ✅   |                                                          |
| `for x = expr then step` (ステップ付き等式)           | ✅   | `loop-emitters.lisp:229`                                 |
| `repeat`                                              | ✅   |                                                          |
| `collect` / `sum` / `count` / `maximize` / `minimize` | ✅   |                                                          |
| `append` / `nconc`                                    | ✅   |                                                          |
| `while` / `until`                                     | ✅   |                                                          |
| `when` / `if` / `unless` 句                           | ✅   |                                                          |
| `always` / `never` / `thereis`                        | ✅   |                                                          |
| `finally`                                             | ✅   |                                                          |
| `initially`                                           | ✅   | FR-543: 実装済み                                         |
| `with`                                                | ✅   |                                                          |
| `named loop`                                          | ✅   | FR-638: `loop-parser.lisp` + `loop.lisp` で実装          |
| `loop-finish`                                         | ✅   | FR-539: `loop.lisp` で tree-walking 実装                 |
| `do` 句 (loop内)                                      | ✅   |                                                          |
| `return` 句 (loop内)                                  | ✅   |                                                          |
| `for (a b) in list` destructuring                     | ✅   | `%loop-emit-destructuring` (`loop-emitters.lisp:121`)    |

#### FR-543: loop initially 句

- **対象**: `packages/expand/src/loop.lisp`
- **内容**: `initially` 句の本体をループ開始前に実行。`finalize-loop-state` の先頭に追加
- **根拠**: ANSI CL 6.1.7 — loop initially
- **難易度**: Easy

### 5.3 マッピング関数

| 関数                                                   | 状態 | 備考                                                                                                            |
| ------------------------------------------------------ | ---- | --------------------------------------------------------------------------------------------------------------- |
| `mapcar` / `mapc` / `mapcan` (多シーケンス)            | ✅   | FR-665: expander.lisp で 2-arg inline + 3+ args labels 再帰展開; 多シーケンス `(mapcar #'+ '(1 2) '(3 4))` 対応 |
| `maplist` / `mapl` / `mapcon`                          | ✅   | FR-360: `stdlib-source.lisp` で実装                                                                             |
| `every` / `some` / `notany` / `notevery` (1シーケンス) | ✅   |                                                                                                                 |
| `every` / `some` 等 多シーケンス並列形式               | ✅   | FR-650: expander.lisp で 3+ args labels 再帰展開; `(every #'< '(1 2) '(3 4))` 対応                              |

---

## 6. オブジェクトシステム・CLOS (ANSI CL Ch.7)

### 6.1 定義・生成

| 機能                               | 状態 | 備考                                                                                                                         |
| ---------------------------------- | ---- | ---------------------------------------------------------------------------------------------------------------------------- |
| `defclass` (基本)                  | ✅   |                                                                                                                              |
| `:allocation :class` スロット      | ✅   | FR-374: パーサー・コードジェン・VM全層実装; クラスHTにスロット値保存、全インスタンスで共有                                   |
| `:default-initargs`                | ✅   | パーサー・コードジェン・VM全層実装; 継承対応; `vm-class-def` に `default-initarg-regs` フィールド                            |
| `defgeneric` (基本)                | ✅   |                                                                                                                              |
| `defmethod` (primary)              | ✅   |                                                                                                                              |
| `defmethod :before/:after/:around` | ✅   | FR-215: standard method combination 完全実装; `:around` → `:before` → primary → `:after` の順序; `call-next-method` 連携済み |
| `eql` 特化子                       | ✅   | FR-359: パーサーで `(param (eql value))` を認識; codegen で quote 展開; VM ディスパッチで eql 一致チェック                   |
| `make-instance`                    | ✅   |                                                                                                                              |
| `find-class` / `(setf find-class)` | ✅   | FR-552: 読み取り + 書き込み両方実装済み                                                                                      |
| `class-name`                       | ✅   | FR-677: VM命令 `vm-class-name-fn` + ビルトイン登録済み                                                                       |
| `ensure-class`                     | ✅   | `:direct-superclasses`/`:direct-slots` を受理し `defclass` に委譲; `macros-compat.lisp`                                      |
| `ensure-generic-function`          | ✅   | FR-525: stdlib-source.lisp で基本実装 (名前返却)                                                                             |

### 6.2 アクセス・変更

| 機能                               | 状態 | 備考                                                                                                      |
| ---------------------------------- | ---- | --------------------------------------------------------------------------------------------------------- |
| `slot-value` / `(setf slot-value)` | ✅   |                                                                                                           |
| `slot-boundp`                      | ✅   | `vm-slot-boundp` 命令あり (`vm-clos.lisp:62`)                                                             |
| `slot-makunbound`                  | ✅   | `vm-slot-makunbound` 命令あり (`vm-clos.lisp:70`)                                                         |
| `slot-exists-p`                    | ✅   | `vm-slot-exists-p` 命令あり (`vm-clos.lisp:78`)                                                           |
| `slot-missing` プロトコル          | ✅   | FR-554: stdlib に `defgeneric slot-missing` + デフォルト `defmethod` 定義; VM エラーに operation 情報追加 |
| `slot-unbound` プロトコル          | ✅   | FR-384: stdlib-source.lisp で defgeneric + エラーシグナル defmethod 定義                                  |
| `with-slots`                       | ✅   | FR-676: `symbol-macrolet` 展開; `setf` がオブジェクトに書き戻される                                       |
| `with-accessors`                   | ✅   | FR-701: `symbol-macrolet` 展開; `setf` がアクセサ経由で書き戻される                                       |

#### FR-554: slot-missing プロトコル

- **対象**: `packages/vm/src/vm-clos.lisp`
- **内容**: `slot-missing` GF 呼び出し。デフォルトメソッドが `error`。ユーザーカスタマイズ可能
- **根拠**: ANSI CL 7.5.11 — slot-missing
- **難易度**: Low

### 6.3 ディスパッチ・メソッド

| 機能                                           | 状態 | 備考                                                                                                                                                                                                                                                                                                           |
| ---------------------------------------------- | ---- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `call-next-method` / `next-method-p`           | ✅   | 引数なし・引数あり両方動作; `vm-cnm-args-reg` で再ディスパッチ (`vm-execute.lisp:280-300`)                                                                                                                                                                                                                     |
| `initialize-instance` GF                       | ✅   | FR-379: stdlib-source.lisp で defgeneric + 基本 defmethod 定義                                                                                                                                                                                                                                                 |
| `allocate-instance` GF                         | ✅   | FR-524: stdlib-source.lisp で defgeneric + 基本 defmethod 定義                                                                                                                                                                                                                                                 |
| `change-class`                                 | ✅   | FR-380: スロット移行 (旧スロット削除・新スロット追加・共通スロット保持) + `update-instance-for-different-class` 呼出                                                                                                                                                                                           |
| `update-instance-for-different-class` GF       | ✅   | FR-1005: `defgeneric`+`defmethod` を `stdlib-source.lisp` に定義; デフォルトは `reinitialize-instance` 委譲 (ANSI 準拠)                                                                                                                                                                                        |
| `update-instance-for-changed-class` GF         | ✅   | FR-1005: `defgeneric`+`defmethod` を `stdlib-source.lisp` に定義; デフォルトは `reinitialize-instance` 委譲 (ANSI 準拠)                                                                                                                                                                                        |
| `class-of`                                     | ✅   | FR-385: VM命令 `vm-class-of-fn` + ビルトイン登録済み                                                                                                                                                                                                                                                           |
| `compute-applicable-methods`                   | ✅   | FR-377: stdlib-source.lisp で defgeneric + 基本 defmethod (nil 返却) 定義                                                                                                                                                                                                                                      |
| `no-applicable-method` GF                      | ✅   | FR-377: stdlib-source.lisp で defgeneric + エラーシグナル defmethod 定義                                                                                                                                                                                                                                       |
| `find-method` / `add-method` / `remove-method` | ✅   | FR-377: 三つとも `defgeneric`+`defmethod` 定義; `find-method` は GF ハッシュテーブル検索; `add-method`/`remove-method` はメソッド追加/削除                                                                                                                                                                     |
| Method Combination (`:before/:after/:around`)  | ✅   | FR-215: standard method combination 完全実装 (`:around`/`:before`/`:after` + `call-next-method`); `define-method-combination` 短形式も実装済み                                                                                                                                                                 |
| C3 線形化 CPL                                  | ✅   | FR-378: `%c3-merge` + `compute-class-precedence-list` を C3 線形化アルゴリズムで実装; ダイアモンド継承・矛盾検出対応                                                                                                                                                                                           |
| MOP イントロスペクション                       | ✅   | FR-523〜528: `class-direct-superclasses`, `class-direct-slots`, `class-slots`, `class-direct-default-initargs`, `class-precedence-list` をマクロ化 (macros-compat.lisp); `vm-hash-table-get-internal`/`vm-hash-table-p` がネイティブCLハッシュテーブルも受理するよう修正; `class-name`/`class-of` は既存VM命令 |
| `no-next-method` GF                            | ✅   | FR-584: stdlib に `defgeneric no-next-method` + デフォルト `defmethod` 定義; `slot-unbound` と同パターン                                                                                                                                                                                                       |
| `invalid-method-error`                         | ✅   | FR-584                                                                                                                                                                                                                                                                                                         |
| `method-combination-error`                     | ✅   | FR-584                                                                                                                                                                                                                                                                                                         |
| `print-object` GF                              | ✅   | FR-390: `defgeneric`+`defmethod` を `stdlib-source.lisp` に定義; GF ディスパッチ経由で `prin1` 委譲                                                                                                                                                                                                            |
| `describe-object` GF                           | ✅   | CLOS インスタンスのクラス名・スロット出力; 非 CLOS は `~S` フォールバック; `macros-compat.lisp`                                                                                                                                                                                                                |

#### FR-584: no-next-method / invalid-method-error / method-combination-error

- **対象**: `packages/vm/src/vm.lisp`, `packages/vm/src/vm-clos.lisp`
- **内容**: `no-next-method gf method args` — カスタマイズ可能エラーGF。`invalid-method-error method format-string args` / `method-combination-error format-string args` もシグナリング関数として実装
- **根拠**: ANSI CL 7.6.6.1 — no-next-method
- **難易度**: Low

---

**CLOS & MOP — 2026年モダンコンパイラ機能リファレンス**

---

## 7. コアオブジェクトモデル (ANSI CL Chapter 4, 7)

### 7.1 クラス定義 — `defclass`

| 機能                                 | cl-cc     | SBCL | 備考                                                                |
| ------------------------------------ | --------- | ---- | ------------------------------------------------------------------- |
| 基本スロット定義                     | ✅        | ✅   | `:reader` `:writer` `:accessor` `:initarg` `:initform`              |
| `:allocation :instance`              | ✅        | ✅   | デフォルト — インスタンスごとのスロット                             |
| `:allocation :class`                 | ✅ FR-374 | ✅   | クラス共有スロット; `vm-class-slots` フィールド + class HT redirect |
| クラスオプション `:default-initargs` | ✅ FR-375 | ✅   | `shared-initialize` で処理                                          |
| 多重継承                             | ✅        | ✅   | 複数スーパークラス指定可能                                          |
| 自動 `standard-object` 継承          | ✅ FR-528 | ✅   | スーパークラス省略時の暗黙継承                                      |

**現在の cl-cc 内部表現:**

```
クラスHT = { :__name__ → symbol
             :__superclasses__ → list
             :__slots__ → list
             :__initargs__ → alist (initarg → slot-name)
             :__initforms__ → alist (slot-name → value)
             :__methods__ → HT (dispatch-key → closure)
             :__cpl__ → list }
インスタンスHT = { :__class__ → クラスHT, slot-name → value, ... }
```

### 7.2 クラス優先順位リスト (CPL / MRO)

| アルゴリズム                | cl-cc     | SBCL | 備考                                                                      |
| --------------------------- | --------- | ---- | ------------------------------------------------------------------------- |
| **C3 線形化**               | ✅ FR-378 | ✅   | Barrett et al. 1996。`%c3-merge` + `compute-class-precedence-list` で実装 |
| ダイアモンド継承の正確なCPL | ✅        | ✅   | C3 で重複排除済み                                                         |
| CPL 矛盾検出 (エラー)       | ✅        | ✅   | `%c3-merge` が矛盾時に `error` をシグナル                                 |

```lisp
;; C3 の例
(defclass A () ())
(defclass B (A) ())
(defclass C (A) ())
(defclass D (B C) ())
;; SBCL CPL: (D B C A standard-object t)  ← C3
;; 現cl-cc:  (D B C A standard-object t)   ← C3
```

---

## 8. ジェネリック関数 (ANSI CL 7.6)

### 8.1 `defgeneric`

| 機能                               | cl-cc     | SBCL | 備考                                                                                                                                                                                  |
| ---------------------------------- | --------- | ---- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 基本定義                           | ✅        | ✅   | ディスパッチHTを生成                                                                                                                                                                  |
| `:method-combination` オプション   | ✅ FR-382 | ✅   | `standard` / `+` / `append` 等                                                                                                                                                        |
| インラインメソッド `(:method ...)` | ✅ FR-382 | ✅   | `defgeneric` 内に直接 `defmethod`                                                                                                                                                     |
| `ensure-generic-function` (公開)   | ✅ FR-525 | ✅   | `%ensure-generic-function` は内部のみ                                                                                                                                                 |
| GF 再定義セマンティクス            | ✅        | ✅   | 既存GFに `defgeneric` を再実行すると既存メソッドは保持 (`ctx-global-generics` 確認)。lambda-list 不一致は compile-time `warn` (`ctx-global-generic-params` に param 数を保存して比較) |

### 8.2 `defmethod` と特化子 (Specializers)

| 機能                                         | cl-cc     | SBCL | 備考                                                                      |
| -------------------------------------------- | --------- | ---- | ------------------------------------------------------------------------- |
| クラス特化子 `(param ClassName)`             | ✅        | ✅   | 基本ディスパッチ                                                          |
| T (全型) 特化子                              | ✅        | ✅   | フォールバックメソッド                                                    |
| **EQL 特化子** `(param (eql value))`         | ✅ FR-124 | ✅   | `(eql :keyword)` や `(eql 42)` 等; codegen で quote 展開、VM で eql 一致  |
| 多重ディスパッチ                             | ✅        | ✅   | 複合キー `(list class1 class2 ...)`                                       |
| 継承ベースのフォールバック                   | ✅        | ✅   | CPL を辿って最も特化したメソッドを選択                                    |
| `find-method`                                | ✅ FR-377 | ✅   | `(find-method gf qualifiers specializers)`                                |
| `add-method` / `remove-method`               | ✅ FR-377 | ✅   | 実行時メソッド追加・削除                                                  |
| `compute-applicable-methods`                 | ✅ FR-377 | ✅   | ユーザーから呼び出し可能                                                  |
| `no-applicable-method`                       | ✅ FR-377 | ✅   | GF として定義。デフォルトは `error`                                       |
| `no-next-method`                             | ✅        | ✅   | `call-next-method` で次がない場合                                         |
| `function-keywords`                          | ✅        | ✅   | `(function-keywords method)` → `(values keyword-list allow-other-keys-p)` |
| メソッド再定義セマンティクス (ANSI CL 7.6.3) | ✅        | ✅   | 同一 name/qualifier/specializer の `defmethod` は既存メソッドを置換       |

**`function-keywords` の使い方:**

```lisp
(defmethod f ((x integer) &key verbose debug) ...)
(function-keywords (find-method #'f '() (list (find-class 'integer))))
;; → (values '(:verbose :debug) nil)   ; nil = &allow-other-keys なし
```

### 8.3 メソッド修飾子 (Method Qualifiers)

| 修飾子                 | cl-cc     | SBCL | 備考                                    |
| ---------------------- | --------- | ---- | --------------------------------------- |
| `primary` (修飾子なし) | ✅        | ✅   | 現在のディスパッチで動作                |
| `:before`              | ✅ FR-215 | ✅   | primary の前に全 `:before` を実行       |
| `:after`               | ✅ FR-215 | ✅   | primary の後に全 `:after` を逆順実行    |
| `:around`              | ✅ FR-215 | ✅   | 全体をラップ。`call-next-method` で連鎖 |

**standard method combination 実行順序 (SBCL準拠):**

```
most-specific-around
  → next-around
    → all :before (most-specific first)
      → most-specific primary
        → call-next-method chain
    → all :after (least-specific first)
```

### 8.4 `call-next-method` と `next-method-p`

| 機能                                            | cl-cc     | SBCL      | 備考                                                                            |
| ----------------------------------------------- | --------- | --------- | ------------------------------------------------------------------------------- |
| `call-next-method` 引数なし (現引数を引き継ぐ)  | ✅        | ✅        | `vm-method-call-stack` で追跡。CPL 順に次メソッドへ                             |
| `call-next-method new-arg1 new-arg2` (引数変更) | ✅        | ✅        | 新しい引数で次メソッドを呼び出す。`call-next-method` 連鎖内で引数のみ置き換える |
| `call-next-method` with `:around` qualifier     | ✅ FR-215 | ✅        | around の連鎖。primary chain とは別のスタック                                   |
| `next-method-p`                                 | ✅        | ✅        | 次のメソッドが存在するか確認。`nil` なら `call-next-method` はエラー            |
| `no-next-method` GF                             | ✅        | ✅        | 次メソッドがない状態で `call-next-method` → この GF を呼ぶ                      |
| メソッド外での `call-next-method`               | ✅        | ✅ エラー | `vm-execute.lisp` で明示的にエラーをシグナル                                    |

**意味論の詳細 (ANSI CL 7.6.6.2):**

```lisp
;; 引数なし: 元の引数をそのまま次メソッドに渡す
(defmethod area :around ((s square))
  (* 2 (call-next-method)))  ; square の primary を元の引数で呼ぶ

;; 引数あり: 新しい引数で次メソッドを呼ぶ (再ディスパッチなし)
(defmethod normalize ((v vector))
  (call-next-method (normalize-impl v)))  ; normalize の次メソッドを new-v で呼ぶ

;; next-method-p でガード
(defmethod process ((x t))
  (when (next-method-p)
    (call-next-method)))
```

**cl-cc の現実装 (`vm-method-call-stack`):**

```
vm-method-call-stack = ((gf-ht [m2 m3] [arg1 arg2])  ; top: 現メソッド
                         (gf-ht [m3]    [arg1 arg2])   ; next
                         nil                            ; 通常 call フレーム
                         ...)
```

### 8.5 ラムダリスト適合規則 (Lambda List Congruence)

ANSI CL 7.6.4 — `defmethod` のラムダリストは `defgeneric` と適合していなければならない。

| ルール                  | cl-cc | SBCL    | 説明                                                         |
| ----------------------- | ----- | ------- | ------------------------------------------------------------ |
| 特化子は必須引数のみ    | ✅    | ✅      | `&optional`/`&rest`/`&key` 引数への特化子は不可              |
| `&key` キー名の一致     | —     | ✅ 警告 | SBCL は不整合 key を警告。ANSI は `&allow-other-keys` で回避 |
| GF なしでの `defmethod` | ✅    | ✅      | GF を自動生成 (cl-cc は `%ensure-generic-function`)          |

```lisp
;; 適合エラーの例
(defgeneric f (a b &optional c))
(defmethod f ((a integer) b) ...)  ; エラー: &optional が欠落
(defmethod f ((a integer) b &optional c &key d) ...)  ; &key は追加可 (ANSI では許容)
```

---

## 9. メソッドコンビネーション (ANSI CL 7.6.6)

### 9.0 メソッドコンビネーション内のエラーシグナル

ANSI CL では `define-method-combination` の長形式ボディ内で使う特殊なエラー関数。cl-cc はエラー報告関数を提供するが、長形式 method-combination DSL 自体は未実装。

> 注: 下の `method-qualifiers` は SBCL/AMOP の説明用例。cl-cc は method qualifier を GF 内部の `:__BEFORE__` / `:__AFTER__` / `:__AROUND__` sub-table に保持するが、公開 `method-qualifiers` accessor は未実装。

| 関数                       | cl-cc | SBCL | 備考                                                   |
| -------------------------- | ----- | ---- | ------------------------------------------------------ |
| `invalid-method-error`     | ✅    | ✅   | FR-584。無効なメソッド定義を説明付きで失敗させる       |
| `method-combination-error` | ✅    | ✅   | FR-584。コンビネーション定義や適用時の不整合を報告する |

```lisp
;; ANSI/SBCL 長形式での使用例 (cl-cc では method-combination DSL 未実装)
;; my-around qualifier しか許可しないコンビネーション
(define-method-combination my-combo ()
  ((primary () :required t)
   (around  (:my-around)))
  ;; 不正な qualifier を持つメソッドを検出してエラー
  (dolist (m (generic-function-methods (generic-function-argument-precedence-order ...)))
    (unless (or (null (method-qualifiers m))
                (equal '(:my-around) (method-qualifiers m)))
      (invalid-method-error m "Method ~S has invalid qualifier ~S" m (method-qualifiers m))))
  ...)
```

### 9.1 組み込みコンビネーション

| 種類          | cl-cc     | SBCL | 説明                                  |
| ------------- | --------- | ---- | ------------------------------------- |
| `standard`    | ✅        | ✅   | `:before`/:after`/`:around` + primary |
| `+`           | ✅ FR-376 | ✅   | 全メソッドの戻り値を `+` で集約       |
| `append`      | ✅ FR-376 | ✅   | リスト結果を `append` で結合          |
| `list`        | ✅ FR-376 | ✅   | 結果をリストに収集                    |
| `nconc`       | ✅ FR-376 | ✅   | `nconc` で結合                        |
| `progn`       | ✅ FR-376 | ✅   | 最後の値を返す                        |
| `and`         | ✅ FR-376 | ✅   | 全て真なら最後の値                    |
| `or`          | ✅ FR-376 | ✅   | 最初の真の値                          |
| `max` / `min` | ✅ FR-376 | ✅   | 数値結果の最大・最小                  |

**`:around` と非標準コンビネーションの相互作用 (ANSI CL 7.6.6.4):**

非標準コンビネーション (`+`, `append` 等) であっても、**`:around` メソッドは常に有効**。`:around` が全体を包み、`call-next-method` が非標準コンビネーションの結果を呼ぶ。

```lisp
(defgeneric total (x) (:method-combination +))
(defmethod total + ((x number)) x)
(defmethod total + ((x integer)) (* x 10))
(defmethod total :around ((x t))
  (format t "Computing...~%")
  (call-next-method))  ; ← + combination の結果 (x + x*10) を返す
```

これは `standard` コンビネーションの `:around` と同じ仕組み。非標準コンビネーションでも短形式の `define-method-combination` は `:around` を自動サポートする。

### 9.2 `method-combination` MOP クラス

cl-cc は `method-combination` 軽量クラスを公開し、GF の method-combination metadata を `generic-function-method-combination` から参照できる。ただし短形式 `define-method-combination` の macroexpansion は登録副作用を表す `(quote name)` を返す実装であり、その返り値自体が `method-combination` インスタンスではない。

| 機能                                       | cl-cc | SBCL | 備考                                                                                                               |
| ------------------------------------------ | ----- | ---- | ------------------------------------------------------------------------------------------------------------------ |
| `method-combination` 相当の公開 MOP クラス | ✅    | ✅   | `stdlib-source-ext.lisp` で軽量 MOP クラスと `method-combination-name` を提供。短形式展開の返り値は `(quote name)` |
| `generic-function-method-combination`      | ✅    | ✅   | `macros-compat.lisp` / `stdlib-source.lisp` に accessor を追加。未設定時は `standard` を返す                       |

```lisp
;; SBCL での確認
(generic-function-method-combination #'print-object)
;; → #<SB-PCL::STANDARD-METHOD-COMBINATION STANDARD {}>
(class-of *)
;; → #<SB-PCL::STANDARD-METHOD-COMBINATION>
```

### 9.3 (旧 3.2) `define-method-combination`

#### 短形式

```lisp
;; (define-method-combination name :operator op :identity-with-one-argument bool)
(define-method-combination +   :operator +   :identity-with-one-argument t)
(define-method-combination max :operator max :identity-with-one-argument t)
```

cl-cc の短形式は `packages/expand/src/macros-clos-protocol.lisp` の登録 macro として `(quote name)` へ展開する。これはコンビネーション型を登録したことを表す副作用値であり、公開 `method-combination` class の instance そのものを返す API ではない。

| 機能                          | cl-cc     | SBCL | 備考                                     |
| ----------------------------- | --------- | ---- | ---------------------------------------- |
| `:operator` — 結合演算子      | ✅ FR-376 | ✅   | 全 primary メソッドの結果をこれで fold   |
| `:identity-with-one-argument` | ✅ FR-376 | ✅   | `t` のとき単一メソッドで演算子をスキップ |

#### 長形式 (未実装)

cl-cc の `define-method-combination` は現在、短形式の登録 macro のみを実装する。ANSI/SBCL の長形式 DSL で使う method group specification、`call-method`、`make-method` は未提供。

```lisp
;; SBCL / ANSI での長形式例 (cl-cc では未実装)
(define-method-combination progn ()
  ((methods ()))   ; method-group: qualifiers = ()
  `(progn ,@(mapcar #'(lambda (m) `(call-method ,m)) methods)))
```

| 要素                       | cl-cc | SBCL | 備考                                                             |
| -------------------------- | ----- | ---- | ---------------------------------------------------------------- |
| method group specification | —     | ✅   | `((primary ()) (around (:around)))` のような節定義は未実装       |
| `call-method`              | —     | ✅   | 長形式展開結果内で個別メソッドを呼ぶ特殊 form は未提供           |
| `make-method`              | —     | ✅   | 長形式でネストしたメソッドフォームを組み立てる特殊 form は未提供 |

**SBCL での長形式完全例 (cl-cc では未実装):**

```lisp
;; :around → :before → primary (最特化) → :after の標準組み合わせ相当
(define-method-combination standard ()
  ((around   (:around))
   (before   (:before))
   (primary  ()         :required t)
   (after    (:after)))
  (flet ((call-methods (methods)
           (mapcar (lambda (m) `(call-method ,m)) methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                       (progn ,@(call-methods before)
                              (call-method ,(first primary) ,(rest primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary) ,(rest primary)))))
      (if around
          `(call-method ,(first around) (,@(rest around) (make-method ,form)))
          form))))
```

---

## 10. インスタンス管理 (ANSI CL 7.1, 7.2)

### 10.1 オブジェクト生成

| 機能                     | cl-cc     | SBCL | 備考                                                                 |
| ------------------------ | --------- | ---- | -------------------------------------------------------------------- |
| `make-instance`          | ✅        | ✅   | 静的クラス名 + 動的クラス変数 両対応                                 |
| `allocate-instance` GF   | ✅ FR-524 | ✅   | メモリ割り当てのカスタマイズポイント                                 |
| `initialize-instance` GF | ✅ FR-379 | ✅   | `:after` でカスタム初期化が可能に                                    |
| `shared-initialize` GF   | ✅        | ✅   | `initialize-instance` / `reinitialize-instance` の共通ロジック       |
| `reinitialize-instance`  | ✅        | ✅   | 既存インスタンスのスロット再初期化                                   |
| `:default-initargs` 処理 | ✅ FR-375 | ✅   | クラスオプションの `:default-initargs` を `shared-initialize` で適用 |

### 10.2 スロットアクセス

| 機能                                | cl-cc     | SBCL | 備考                                                                                                                               |
| ----------------------------------- | --------- | ---- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `slot-value` / `(setf slot-value)`  | ✅        | ✅   | `vm-slot-read` / `vm-slot-write` 命令                                                                                              |
| `slot-boundp`                       | ✅        | ✅   | `vm-slot-boundp` 命令                                                                                                              |
| `slot-makunbound`                   | ✅        | ✅   | `vm-slot-makunbound` 命令                                                                                                          |
| `slot-exists-p`                     | ✅        | ✅   | `vm-slot-exists-p` 命令                                                                                                            |
| `slot-unbound` GF                   | ✅ FR-384 | ✅   | 未束縛アクセス時のカスタマイズ。現在は直接 `error`                                                                                 |
| `slot-missing` GF                   | ✅        | ✅   | **スロット名自体が存在しない**場合。`slot-unbound` とは別 (operation引数: `:slot-value`/`:setf`/`:slot-boundp`/`:slot-makunbound`) |
| `:reader` / `:writer` / `:accessor` | ✅        | ✅   | `compile-slot-accessor` で closure 生成                                                                                            |
| accessor read inlining              | ✅ FR-120 | ✅   | `compiler-macroexpand-all` で `(accessor obj)` → `(slot-value obj 'slot)` に展開                                                   |
| `with-slots` マクロ                 | ✅        | ✅   | `macros-stdlib.lisp` 実装済み                                                                                                      |
| `with-accessors` マクロ             | ✅        | ✅   | `macros-stdlib.lisp` 実装済み                                                                                                      |

### 10.3 クラス変更プロトコル

| 機能                                     | cl-cc     | SBCL | 備考                                                                                          |
| ---------------------------------------- | --------- | ---- | --------------------------------------------------------------------------------------------- |
| `change-class`                           | ✅ FR-380 | ✅   | `macros-sequence.lisp:527` — スロット移行・`update-instance-for-different-class` 呼び出しあり |
| `update-instance-for-different-class` GF | ✅ FR-380 | ✅   | `change-class` プロトコルの一部                                                               |
| `make-instances-obsolete`                | ✅        | ✅   | クラス HT に obsolete marker を付与し、再定義時は replacement class へ遅延移行                |
| `update-instance-for-redefined-class`    | ✅        | ✅   | 軽量関数として追加スロット initarg を反映。汎用 GF 体系ではなく hash-table runtime 向け実装   |

---

## 11. 型システムとディスパッチ (ANSI CL 4.3, 7.6)

### 11.1 型分類 (`vm-classify-arg`)

現在 `vm-execute.lisp:383` の実装:

```lisp
;; 現行: 4型のみ
(typecase arg
  (integer 'integer)
  (string  'string)
  (symbol  'symbol)
  (t       t))       ; CLOS インスタンスは :__class__ で分岐
```

**SBCL / ANSI CL が要求する完全な型カバレッジ (FR-381):**

| 型                          | cl-cc | 備考                |
| --------------------------- | ----- | ------------------- |
| `integer` (fixnum / bignum) | ✅    | 現行                |
| `string`                    | ✅    | 現行                |
| `symbol` (null を含む)      | ✅    | 現行                |
| CLOS インスタンス           | ✅    | `:__class__` で判別 |

### 11.2 `class-of` / `type-of` / `typep`

| 機能                             | cl-cc     | SBCL | 備考                                                                                 |
| -------------------------------- | --------- | ---- | ------------------------------------------------------------------------------------ |
| `class-of`                       | ✅ FR-385 | ✅   | VM命令あり。`:__class__` から取得する                                                |
| `type-of`                        | ✅ FR-385 | ✅   | 標準型 + CLOSクラス両方返却                                                          |
| `typep` (クラス名)               | ✅ FR-385 | ✅   | `(typep obj 'my-class)` — CPL トラバーサルが必要                                     |
| **`typep` (クラスオブジェクト)** | ✅        | ✅   | `(typep obj (find-class 'my-class))` — ANSI CL 4.3.7。クラスオブジェクトを直接渡せる |
| `subtypep` (クラス間)            | ✅        | ✅   | `(subtypep 'child 'parent)` → T。CPL を使って判定                                    |
| クラス型としての型指定子         | ✅        | ✅   | クラス名はそのまま型指定子として使える。`(declare (type my-class x))` 等             |

### 11.3 組み込みクラス階層

**SBCL の標準クラス階層 (cl-cc での実装状況):**

```
t
└── standard-object         ✅ FR-528 (VMレジストリに登録済み)
    └── (user classes)      ✅
└── structure-object        ✅
└── condition               ✅ (vm-condition が host CL の condition を継承)
    └── serious-condition
        └── error
    └── warning
└── number
    ├── integer             ✅ (vm-classify-arg)
└── sequence
└── string                  ✅ (vm-classify-arg)
└── symbol                  ✅ (vm-classify-arg)
```

---

## 12. MOP — メタオブジェクトプロトコル (AMOP)

### 12.1 クラスイントロスペクション

| 機能                        | cl-cc     | SBCL | 備考                       |
| --------------------------- | --------- | ---- | -------------------------- |
| `class-name`                | ✅ FR-527 | ✅   | `:__name__` の公開         |
| `class-direct-superclasses` | ✅ FR-527 | ✅   | `:__superclasses__` の公開 |
| `class-precedence-list`     | ✅ FR-527 | ✅   | `:__cpl__` の公開          |
| `class-direct-slots`        | ✅ FR-527 | ✅   | 直接定義スロットのみ       |
| `class-slots`               | ✅        | ✅   | 継承含む全スロット         |

### 12.2 スロット定義イントロスペクション

| 機能                               | cl-cc | SBCL | 備考                                                                                                                   |
| ---------------------------------- | ----- | ---- | ---------------------------------------------------------------------------------------------------------------------- |
| slot definition 名の観測           | ✅    | ✅   | `class-direct-slots` / `class-slots` からスロット情報を読める                                                          |
| slot definition の MOP accessor 群 | ✅    | ✅   | `slot-definition-name` / `slot-definition-initform` / `slot-definition-initargs` / `slot-definition-allocation` を公開 |

### 12.2b Effective Slot Definition マージ規則 (ANSI CL 7.5.3)

多重継承で複数のスーパークラスが同名スロットを定義した場合、`compute-effective-slot-definition` が結合する。

| 項目                                                      | マージ規則 | cl-cc | SBCL |
| --------------------------------------------------------- | ---------- | ----- | ---- |
| `compute-effective-slot-definition` 相当の専用 MOP フック | —          | ✅    |
| 継承後スロットを `class-slots` から観測できる             | ✅         | ✅    |
| 継承後スロットに対する `slot-value` / accessor 利用       | ✅         | ✅    |
| `:initarg` 和集合を保証する専用実装根拠                   | —          | ✅    |
| `:initform` 優先順位を公開 MOP として制御                 | —          | ✅    |

```lisp
;; 例: :initargs の和集合
(defclass A () ((x :initarg :a-x)))
(defclass B () ((x :initarg :b-x)))
(defclass C (A B) ())
;; C のスロット x には :a-x と :b-x 両方が有効
(make-instance 'C :a-x 1)  ; OK
(make-instance 'C :b-x 1)  ; OK (SBCL)
```

cl-cc 側では継承済みスロットの観測と通常の `slot-value` / accessor 利用は確認できるが、SBCL のような dedicated MOP hook としての merge 規則までは repo から直接は確認できない。

### 12.3 メタクラスプロトコル

| 機能                                        | cl-cc | SBCL | 備考                                                                                        |
| ------------------------------------------- | ----- | ---- | ------------------------------------------------------------------------------------------- |
| `standard-class` 相当の通常クラス生成       | ✅    | ✅   | cl-cc はハッシュテーブル表現のクラスオブジェクトを生成                                      |
| `ensure-class` によるプログラム的クラス生成 | ✅    | ✅   | `ensure-class` は `defclass` に委譲する実装                                                 |
| `:metaclass` option の保持と参照            | ✅    | ✅   | `defclass` が `:metaclass` を保持し、`class-metaclass` で参照可能。カスタム挙動までは未提供 |
| `funcallable-standard-class`                | ✅    | ✅   | 軽量 marker class として `stdlib-source-ext.lisp` に定義                                    |
| ユーザー定義メタクラス上書き                | —     | ✅   | cl-cc では一般 MOP としては未文書化                                                         |

### 12.4 ジェネリック関数 MOP

| 機能                                           | cl-cc | SBCL | 備考                                                          |
| ---------------------------------------------- | ----- | ---- | ------------------------------------------------------------- |
| `ensure-generic-function`                      | ✅    | ✅   | 公開 API として利用可能                                       |
| `compute-applicable-methods`                   | ✅    | ✅   | 基本 defmethod あり                                           |
| `find-method` / `add-method` / `remove-method` | ✅    | ✅   | 実行時メソッド操作を提供                                      |
| `generic-function-method-combination`          | ✅    | ✅   | GF の `:__method-combination__` を返し、未設定時は `standard` |
| `generic-function-methods`                     | ✅    | ✅   | GF の `:__methods__` を `hash-table-values` 経由で公開        |

### 12.4b 継承矛盾の検出

| 挙動     | cl-cc     | SBCL      | 備考                                            |
| -------- | --------- | --------- | ----------------------------------------------- |
| 循環継承 | ✅ エラー | ✅ エラー | `%c3-merge` が矛盾を検出して `error` をシグナル |

---

## 12.5 initarg 検証プロトコル (ANSI CL 7.1.2)

`make-instance` に渡す initarg が有効か検証される。無効な場合 `error`。

**有効な initarg の条件 (いずれか):**

| 機能                                     | cl-cc | SBCL | 備考                                                                                |
| ---------------------------------------- | ----- | ---- | ----------------------------------------------------------------------------------- |
| 直接スロットの `:initarg`                | ✅    | ✅   | そのクラスの direct slot に宣言されている                                           |
| 継承スロット由来の `:initarg`            | ✅    | ✅   | `collect-inherited-initargs` が superclass 由来の initarg を `:__initargs__` に統合 |
| `:default-initargs` で供給される initarg | ✅    | ✅   | クラスオプションとして処理                                                          |
| 未知の initarg のエラー                  | ✅    | ✅   | `vm-clos.lisp` の `%vm-validate-initargs` が `Invalid initarg` をシグナル           |
| `:allow-other-keys t` による検証スキップ | ✅    | ✅   | `%vm-allow-other-keys-p` が明示的に検証をバイパス                                   |

```lisp
;; SBCL での挙動
(defclass foo () ((x :initarg :x)))
(make-instance 'foo :y 1)  ; ERROR: invalid initarg :y
(make-instance 'foo :y 1 :allow-other-keys t)  ; OK: 検証スキップ
```

---

## 13. 最適化 (Optimization)

### 13.1 ディスパッチキャッシュ

| 最適化                            | cl-cc | SBCL | 備考                                                                  |
| --------------------------------- | ----- | ---- | --------------------------------------------------------------------- |
| accessor read inlining            | ✅    | ✅   | `compiler-macroexpand-all` で `(accessor obj)` を `slot-value` へ展開 |
| multiple dispatch memoization     | —     | ✅   | cl-cc では優先度行列に FR-119 として残る                              |
| hidden class / shape ベース最適化 | —     | —    | cl-cc では FR-214 として計画のみ                                      |

### 13.2 インスタンス表現

| 最適化                      | cl-cc | SBCL | 備考                                          |
| --------------------------- | ----- | ---- | --------------------------------------------- |
| **現行: ハッシュテーブル**  | ✅    | —    | `gethash` O(1) average, memory heavy          |
| SBCL の `standard-instance` | —     | ✅   | `(instance header slots)` の3ワードレイアウト |

### 13.3 静的最適化

| 最適化                                 | cl-cc | SBCL | 備考                                     |
| -------------------------------------- | ----- | ---- | ---------------------------------------- |
| sealed / final 想定の静的ディスパッチ  | —     | ✅   | SBCL は sealed classes などの拡張で支援  |
| SBCL `deftransform` (メソッド特化変換) | —     | ✅   | コンパイラマクロ的なメソッドインライン化 |

---

## 14. `defstruct` との統合

cl-cc は `defstruct` を `defclass` への変換として実装 (`packages/expand/src/expander-defstruct.lisp`)。
SBCL は別の `structure-class` メタクラスを使う点で異なる。

**cl-cc の変換方式:**

```lisp
;; 入力
(defstruct point (x 0) (y 0))

;; 展開結果 (expand-defstruct)
(progn
  (defclass point () ((x :initarg :x :initform 0 :accessor point-x)
                      (y :initarg :y :initform 0 :accessor point-y)))
  (defun make-point (&key (x 0) (y 0)) (make-instance 'point :x x :y y))
  (defun point-p (obj) (typep obj 'point))
  'point)
```

### 14.1 `defstruct` オプション対応状況

| オプション                          | cl-cc | SBCL | 備考                                                 |
| ----------------------------------- | ----- | ---- | ---------------------------------------------------- |
| `:conc-name`                        | ✅    | ✅   | アクセサ名のプレフィックス。`nil` で無プレフィックス |
| `:constructor name`                 | ✅    | ✅   | コンストラクタ名の変更                               |
| `:constructor name boa-list`        | ✅    | ✅   | BOA (By-Order-of-Arguments) ラムダリスト             |
| `:include parent`                   | ✅    | ✅   | 親構造体の継承。`*defstruct-slot-registry*` で追跡   |
| `:predicate name`                   | ✅    | ✅   | 述語関数名の変更または `nil` で抑制                  |
| `:copier name`                      | ✅    | ✅   | `copy-NAME` 関数の生成                               |
| `:print-function` / `:print-object` | ✅    | ✅   | `print-object` メソッドの自動生成                    |
| `:type vector` / `:type list`       | ✅    | ✅   | CLOS インスタンスでなくベクタ/リストで格納           |
| `:named`                            | ✅    | ✅   | `:type` 構造体に型タグを付加                         |
| 複数 `:constructor`                 | ✅    | ✅   | 複数コンストラクタ同時定義                           |

### 14.2 SBCL `structure-class` との差異

| 項目                                       | cl-cc                         | SBCL                                            |
| ------------------------------------------ | ----------------------------- | ----------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| 内部表現                                   | `defclass` (ハッシュテーブル) | `sb-kernel:structure-object` (固定サイズベクタ) |
| `copy-structure`                           | ✅                            | ✅                                              |
| `structure-object` 組み込みクラス          | ✅ FR-528                     | ✅                                              |
| CLOS メソッド定義 (構造体への `defmethod`) | ✅                            | ✅                                              | `expander-defstruct.lisp` で `structure-object` を自動スーパークラスとして追加 (`:include` なし時); CPL に `structure-object` が入る |

---

## 15. インスタンス初期化プロトコルチェーン (ANSI CL 7.1)

`make-instance` の呼び出しは以下の GF チェーンを経由する。各 GF がカスタマイズポイント。

```
make-instance class &rest initargs
  │
  ├─ apply-default-initargs  (defgeneric: initargs に :default-initargs を適用)
  │
  └─ allocate-instance class &rest initargs
       │  (メモリ割り当て: cl-cc は vm-make-obj がインライン実行)
       │
       └─ initialize-instance instance &rest initargs
            │  (ユーザーが :after でカスタム初期化を行う主なポイント)
            │
            └─ shared-initialize instance slot-names &rest initargs
                 (initarg からスロットに値をセット。:before/:after でさらにカスタマイズ)
```

| GF                                    | cl-cc          | SBCL  | 備考                                                            |
| ------------------------------------- | -------------- | ----- | --------------------------------------------------------------- |
| `make-instance`                       | ✅ (macro展開) | ✅ GF | cl-cc は `vm-make-obj` 命令。GF ではない                        |
| `allocate-instance`                   | ✅ FR-524      | ✅ GF | メモリ割り当てのカスタマイズ。cl-cc は `vm-make-obj` 内に実装   |
| `initialize-instance`                 | ✅ FR-379      | ✅ GF | ユーザーが `:after` で初期化を追加できる                        |
| `shared-initialize`                   | ✅             | ✅ GF | `initialize-instance` と `reinitialize-instance` の共通ロジック |
| `reinitialize-instance`               | ✅             | ✅ GF | スロットへの initarg 適用を含む                                 |
| `apply-default-initargs` (内部)       | ✅             | ✅    | クラスの `:default-initargs` を initarg リストにマージ          |
| `update-instance-for-different-class` | ✅ FR-380      | ✅ GF | `reinitialize-instance` に委譲する既定動作を持つ                |

**`shared-initialize` の役割 (ANSI CL 7.1.4):**

```lisp
;; SLOT-NAMES = t なら全スロット、リストなら指定スロットのみ初期化
(defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
  (dolist (slot (class-slots (class-of instance)))
    (let* ((slot-name (slot-definition-name slot))
           (initarg   (find-initarg-in-slot slot initargs)))
      (cond
        ;; initarg 指定あり → スロットに設定
        (initarg (setf (slot-value instance slot-name) (second initarg)))
        ;; slot-names 対象 + initform あり → initform を評価して設定
        ((and (or (eq slot-names t) (member slot-name slot-names))
              (slot-definition-initfunction slot))
         (setf (slot-value instance slot-name)
               (funcall (slot-definition-initfunction slot))))))))
```

---

## 16. `ensure-class` / `ensure-class-using-class` (MOP)

`defclass` は内部的に `ensure-class` を呼ぶ。直接呼ぶことでプログラム的にクラスを作成・更新できる。

**cl-cc の現実装 (`macros-sequence.lisp:504`):**

```lisp
;; 実装は defclass に委譲する版
(our-defmacro ensure-class (name &rest options)
  (let ((direct-superclasses (or (getf options :direct-superclasses) '()))
        (direct-slots (or (getf options :direct-slots) '())))
    `(defclass ,name ,direct-superclasses ,direct-slots)))
```

| 機能                         | cl-cc | SBCL | 備考                                                                       |
| ---------------------------- | ----- | ---- | -------------------------------------------------------------------------- |
| `ensure-class name &key ...` | ✅    | ✅   | キーワード: `:direct-superclasses` `:direct-slots` `:metaclass` `:name` 等 |

**SBCL での `defclass` → `ensure-class` の流れ:**

```lisp
;; defclass マクロが展開すると:
(ensure-class 'my-class
  :direct-superclasses (list (find-class 'base))
  :direct-slots '((:name x :initarg :x :readers (my-x) ...))
  :metaclass (find-class 'standard-class))
;; → ensure-class は find-class で既存クラスを検索
;; → 存在すれば ensure-class-using-class existing-class ...
;; → なければ  ensure-class-using-class nil ...
```

---

## 17. コンディションシステムとの統合

### 17.1 `define-condition` と `:report` プロトコル

`define-condition` は `defclass` のサブセットで、コンディション固有のオプションを追加する。

**cl-cc の実装 (`conditions.lisp`) — 実際に `:report` を使用中:**

```lisp
(define-condition vm-type-error (vm-error type-error)
  ()
  (:report (lambda (condition stream)
             (format stream "VM Type Error: expected ~A, got ~S"
                     (type-error-expected-type condition)
                     (type-error-datum condition)))))
```

| 機能                              | cl-cc | SBCL | 備考                                          |
| --------------------------------- | ----- | ---- | --------------------------------------------- |
| `define-condition` 基本定義       | ✅    | ✅   | `conditions.lisp` で実用中                    |
| `:report lambda`                  | ✅    | ✅   | `(lambda (condition stream) ...)` 形式        |
| `make-condition`                  | ✅    | ✅   | `make-instance` に委譲する実装                |
| `condition-report` (内部)         | ✅    | ✅   | `format`/`print` が `print-object` 経由で呼ぶ |
| `:default-initargs` on conditions | ✅    | ✅   |                                               |

### 17.2 組み込みコンディションクラス階層

SBCL の標準コンディション階層 (cl-cc での実装状況):

```
│   ├── error                           ✅ (host CL の error)
│   │   ├── type-error                  ✅ vm-type-error が (vm-error type-error) を継承
│   │   ├── unbound-variable            ✅ vm-unbound-variable が (vm-error unbound-variable) を継承
│   │   ├── undefined-function          ✅ vm-undefined-function が (vm-error undefined-function) を継承
│   │   ├── unbound-slot               ✅ vm-slot-read が `unbound-slot` condition をシグナル (FR-384)
│   │   ├── division-by-zero            ✅ vm-division-by-zero が (vm-error division-by-zero) を継承
├── warning                             ✅ (host)
```

### 17.3 コンディションシステムのプロトコル関数

| 関数                                   | cl-cc | SBCL | 備考                                                      |
| -------------------------------------- | ----- | ---- | --------------------------------------------------------- |
| `signal` / `error` / `warn` / `cerror` | ✅    | ✅   | VM 命令として実装                                         |
| `handler-bind` / `handler-case`        | ✅    | ✅   | VM 命令として実装                                         |
| `restart-case` / `invoke-restart`      | ✅    | ✅   | `invoke-restart` と `restart-case` の基本プロトコルを実装 |
| `compute-restarts`                     | ✅    | ✅   | 現在の再起動リストを返す                                  |
| `find-restart`                         | ✅    | ✅   | 名前で再起動を検索                                        |
| `condition-report` (内部 GF)           | ✅    | ✅   | `:report` を呼ぶ                                          |
| `print-object` for conditions          | ✅    | ✅   | `#<TYPE-ERROR ...>` 形式                                  |

---

## 18. `print-object` と出力プロトコル

| 機能                                 | cl-cc | SBCL | 備考                                                            |
| ------------------------------------ | ----- | ---- | --------------------------------------------------------------- |
| `print-object` GF                    | ✅    | ✅   | オブジェクトの表示カスタマイズ                                  |
| `print-unreadable-object` マクロ     | ✅    | ✅   | `#<...>` 形式の出力                                             |
| `with-standard-io-syntax` 下での挙動 | ✅    | ✅   | print 制御変数をリセットし、`print-object` も通常どおり動作する |

**`*print-readably*` への対応パターン:**

```lisp
(defmethod print-object ((p point) stream)
  (if *print-readably*
      ;; readable: (make-instance 'point :x 1 :y 2) として出力
      (format stream "#.(make-instance '~S :x ~S :y ~S)"
              (class-name (class-of p)) (point-x p) (point-y p))
      ;; unreadable: #<POINT (1, 2)>
      (print-unreadable-object (p stream :type t)
        (format stream "(~S, ~S)" (point-x p) (point-y p)))))
```

---

## 19. MOP — Specializer プロトコル

| 機能                | cl-cc | SBCL | 備考                                                  |
| ------------------- | ----- | ---- | ----------------------------------------------------- |
| `(setf find-class)` | ✅    | ✅   | クラスの登録・置換。`defclass` の内部実装がこれを呼ぶ |

**cl-cc の内部レジストリ:**

```lisp
;; vm-state 内の vm-class-registry は HT (symbol → class-HT)
;; defclass コンパイル時に codegen-clos.lisp:39 で登録
(setf (gethash name (ctx-global-classes ctx)) dst)
```

---

## 20. MOP — `slot-value-using-class` プロトコル

この領域は AMOP / SBCL では重要だが、cl-cc 側では同等の公開プロトコルを repo から確認できた範囲が限定的である。

| 段階                                       | cl-cc | SBCL | 説明                                                                                       |
| ------------------------------------------ | ----- | ---- | ------------------------------------------------------------------------------------------ |
| `slot-value-using-class` の公開 MOP フック | —     | ✅   | cl-cc で同名フックの repo 根拠は未確認                                                     |
| `make-instances-obsolete`                  | ✅    | ✅   | `make-instances-obsolete` / `%make-instances-obsolete` が obsolete marker を設定           |
| `update-instance-for-redefined-class`      | ✅    | ✅   | 追加スロット initarg 反映用の軽量関数として提供                                            |
| クラス再定義時の遅延マイグレーション       | ✅    | ✅   | `slot-value` / `(setf slot-value)` / `slot-boundp` / `slot-makunbound` が migration を起動 |

**SBCL 側の代表的実装方式:**

```
インスタンスの先頭ワード = "wrapper" ポインタ
wrapper が無効化 (invalidated) されたら → 次のスロットアクセス時に trap → migrate
```

---

## 21. MOP — Method クラスプロトコル

cl-cc では `find-method` / `add-method` / `remove-method` のような操作は確認できる一方、SBCL/AMOP の豊富な method-class 階層までは公開されていない。

| 機能                                                           | cl-cc | SBCL | 備考                                                                      |
| -------------------------------------------------------------- | ----- | ---- | ------------------------------------------------------------------------- |
| メソッドオブジェクトの追加・削除                               | ✅    | ✅   | `add-method` / `remove-method` は利用可能                                 |
| メソッド検索                                                   | ✅    | ✅   | `find-method` を提供                                                      |
| `method-qualifiers` 等の公開 MOP accessor                      | —     | ✅   | 未実装。qualifier は VM 内部 sub-table に保持するが公開 accessor は未提供 |
| `standard-method` / `standard-reader-method` などの MOP クラス | —     | ✅   | cl-cc では公開メタクラス体系としては未文書化                              |

---

## 22. `describe-object` / `documentation` プロトコル

この節は、ユーザー向けの記述・説明 API と、それに近い公開フックを整理する。

| 機能                                | cl-cc | SBCL | 備考                                                       |
| ----------------------------------- | ----- | ---- | ---------------------------------------------------------- |
| `describe-object` GF                | ✅    | ✅   | CLOS インスタンスのクラス名・スロットを表示                |
| `describe`                          | ✅    | ✅   | `describe-object` を通じた説明出力                         |
| `documentation`                     | ✅    | ✅   | `macros-filesystem.lisp` / `vm.lisp` に公開 API の根拠あり |
| `defun` / `lambda` docstring の受理 | ✅    | ✅   | docstring 自体は expander が正しく処理                     |

`describe-object` は公開 GF として使えるが、`slot-value-using-class` のような本格的 MOP フックはこの節ではなく前節の MOP 領域で扱うのが自然である。

---

## 23. SBCL 固有拡張 (2026年現在)

| 機能                       | cl-cc | SBCL | 備考                                          |
| -------------------------- | ----- | ---- | --------------------------------------------- |
| `sb-mop`                   | —     | ✅   | SBCL 固有の MOP パッケージ                    |
| sealed classes             | —     | ✅   | 継承閉鎖を使った最適化                        |
| satiated generic functions | —     | ✅   | ディスパッチ経路を事前固定化する拡張          |
| `closer-mop`               | —     | ✅   | 互換層ライブラリであり cl-cc 組み込みではない |

---

### 23.1 `sb-mop` パッケージ

| 機能                          | cl-cc | SBCL | 備考                                                              |
| ----------------------------- | ----- | ---- | ----------------------------------------------------------------- |
| `make-load-form object` GF    | ✅    | ✅   | FASL 埋め込み用の汎用 GF。デフォルトは primitive literal を返す   |
| `make-load-form-saving-slots` | ✅    | ✅   | `hash-table` の `:__class__` メタデータからクラス名を復元する実装 |

**使用例:**

```lisp
(defclass point ()
  ((x :initarg :x) (y :initarg :y)))

(defmethod make-load-form ((p point) &optional env)
  (make-load-form-saving-slots p :slot-names '(x y) :environment env))
```

---

### 23.2 Sealed Classes (SBCL 2.3+)

| 機能                                | cl-cc | SBCL | 備考                                               |
| ----------------------------------- | ----- | ---- | -------------------------------------------------- |
| `sb-ext:define-sealed-type`         | —     | ✅   | 継承・再定義の自由度と引き換えに最適化余地を増やす |
| sealed class 前提の静的ディスパッチ | —     | ✅   | コンパイラがメソッド候補集合を閉じて扱える         |

---

### 23.3 Satiated Generic Functions (SBCL)

SBCL では GF のディスパッチ経路を事前に確定・飽和させる最適化がある。

| 機能                                     | cl-cc | SBCL | 備考                                             |
| ---------------------------------------- | ----- | ---- | ------------------------------------------------ |
| satiated generic function 最適化         | —     | ✅   | hot path の dispatch cost を下げる SBCL 固有機能 |
| `sb-pcl::satiating-gfs-p` などの内部 API | —     | ✅   | 一般の ANSI API ではない                         |

### 23.4 `closer-mop` (ポータビリティ層)

SBCL は ANSI CL にない MOP 機能を `sb-mop` で提供 (`closer-mop` 経由が推奨)。

| 機能                                       | SBCL | 備考                                |
| ------------------------------------------ | ---- | ----------------------------------- |
| `sb-mop:class-direct-slots`                | ✅   | direct slot definitions のリスト    |
| `sb-mop:class-slots`                       | ✅   | effective slot definitions のリスト |
| `sb-mop:slot-definition-location`          | ✅   | スロットのメモリ位置 (インデックス) |
| `sb-mop:funcallable-standard-class`        | ✅   |                                     |
| `sb-mop:set-funcallable-instance-function` | ✅   |                                     |

## 24. `&key` とジェネリック関数 (ANSI CL 7.6.5)

| 機能                                  | SBCL | cl-cc | 備考                                   |
| ------------------------------------- | ---- | ----- | -------------------------------------- |
| GF 自身のラムダリストに `&key` を置く | ✅   | ✅    | 通常の関数と同じくキーワード引数を受理 |
| メソッドラムダリスト側の `&key`       | ✅   | ✅    | 特化子は必須引数部にのみ置ける         |
| `&allow-other-keys`                   | ✅   | ✅    | 未知キーワードの受理を明示             |
| key 不整合への警告                    | ✅   | —     | cl-cc で SBCL 同等の警告根拠は未確認   |

### 24.1 `&key` の合法性規則

| 機能                                   | SBCL | cl-cc | 備考                                                |
| -------------------------------------- | ---- | ----- | --------------------------------------------------- |
| 適用可能メソッドが知るキーの集合を受理 | ✅   | ✅    | 実際のメソッド集合に基づくキー判定                  |
| `:allow-other-keys t` による緩和       | ✅   | ✅    | ANSI 標準の逃げ道                                   |
| 未知キーワードでのエラー               | ✅   | ✅    | 緩和指定がなければ不正 initarg / keyword として扱う |

### 24.2 `&rest` + `&key` の組み合わせ

`&rest` と `&key` を併用すると、受け取ったキーワード列を加工せず次メソッドへ転送しやすい。

| 機能                                         | cl-cc | SBCL | 備考                           |
| -------------------------------------------- | ----- | ---- | ------------------------------ |
| `&rest args &key ...` 形式                   | ✅    | ✅   | キーワード列を丸ごと保持できる |
| `(apply #'call-next-method x args)` パターン | ✅    | ✅   | around / primary の転送で有用  |
| `&allow-other-keys` との併用                 | ✅    | ✅   | 上位互換的にキー転送できる     |

---

## 25. ジェネリック関数の First-Class 利用

GF は通常の関数と同じく first-class 値として扱える。

**cl-cc での判定 (`vm.lisp:443`):**

```lisp
(defun vm-generic-function-p (value)
  ;; :__methods__ キーを持つ HT = GF と判定
  (and (hash-table-p value) (gethash :__methods__ value) t))
;; %vm-dispatch-call (vm-execute.lisp:145) が呼び出し前にこれで分岐
```

| 機能                                   | cl-cc | SBCL | 備考                                         |
| -------------------------------------- | ----- | ---- | -------------------------------------------- |
| `#'my-generic-fn` で GF を値として取得 | ✅    | ✅   | `vm-generic-function-p` で識別               |
| `(funcall #'my-gf arg)`                | ✅    | ✅   | `%vm-dispatch-call` が GF ディスパッチに分岐 |
| `(apply #'my-gf list)`                 | ✅    | ✅   | `vm-execute.lisp:505` で対応                 |

**`standard-generic-function` クラス:**

```
funcallable-standard-object  (MOP)
└── generic-function         (ANSI CL abstract)
    └── standard-generic-function  (ANSI CL concrete)
        ← defgeneric が生成するオブジェクトのクラス (SBCL)
```

| MOP アクセサ               | cl-cc | SBCL | 備考                                          |
| -------------------------- | ----- | ---- | --------------------------------------------- |
| `ensure-generic-function`  | ✅    | ✅   | 公開 API として利用可能                       |
| `find-method`              | ✅    | ✅   | GF 上のメソッド検索                           |
| `generic-function-methods` | ✅    | ✅   | `:__methods__` を公開する accessor を追加済み |

## 26. CLOS と多値・型宣言・その他の連携

| ルール                              | cl-cc | SBCL | 説明                                             |
| ----------------------------------- | ----- | ---- | ------------------------------------------------ |
| メソッドの多値返却                  | ✅    | ✅   | GF でも通常関数と同様に multiple values を返せる |
| `the` / `(values ...)` 宣言との共存 | ✅    | ✅   | 型宣言はメソッド呼び出し周辺でも使える           |
| CLOS インスタンスの等価性           | ✅    | ✅   | 既定では同一性ベース                             |
| CLOS インスタンスの複製             | ✅    | ✅   | 標準 API は構造体ほど直接的ではない              |

**重要な特別規則 (ANSI CL 7.6.5):**

```lisp
(defgeneric f (a &key))
(defmethod f ((a integer) &key x) ...)   ; :x を知る
(defmethod f ((a string)  &key y) ...)   ; :y を知る

;; 呼び出し側: (f 42 :x 1 :y 2) — 両方合法
;; effective method に含まれる全メソッドのキー名の和集合が許容される
;; → integer メソッドだけが適用されても :y は合法 (暗黙の &allow-other-keys)
```

### 26.1 多値 (`values`) とメソッド

```lisp
;; &rest と &key を組み合わせる慣用形
(defmethod process ((x t) &rest args &key verbose &allow-other-keys)
  (when verbose (format t "Processing ~S~%" x))
  (apply #'call-next-method x args))  ; 全キーワードを次メソッドに転送
```

| パターン                                    | cl-cc | SBCL | 備考                         |
| ------------------------------------------- | ----- | ---- | ---------------------------- |
| メソッドから `(values a b)` を返す          | ✅    | ✅   | 通常の `vm-ret` が多値を伝播 |
| `(nth-value 0 (my-gf x))`                   | ✅    | ✅   |                              |
| `(multiple-value-bind (a b) (my-gf x) ...)` | ✅    | ✅   |                              |

---

### 26.2 型宣言とメソッド

| 機能                                         | cl-cc | SBCL | 備考                             |
| -------------------------------------------- | ----- | ---- | -------------------------------- |
| `(the class-name (my-gf ...))`               | ✅    | ✅   | 戻り値をクラス型として注釈できる |
| `(the (values ...) (my-gf ...))`             | ✅    | ✅   | 多値型宣言を保持できる           |
| `(declare (type my-class x))` とメソッド本体 | ✅    | ✅   | クラス名を型指定子として利用可能 |

---

### 26.3 オブジェクト同一性・等価性

| 機能                                      | cl-cc | SBCL | 備考                                               |
| ----------------------------------------- | ----- | ---- | -------------------------------------------------- |
| `eq` — 同一インスタンス判定               | ✅    | ✅   | HT の `eq` 比較                                    |
| `eql` — CLOS インスタンスでは `eq` と同じ | ✅    | ✅   | CLOS インスタンスは数値でないので `eql` = `eq`     |
| `equal` / `equalp` — 既定では同一性ベース | ✅    | ✅   | ユーザーが独自比較を定義しない限り参照同一性が中心 |

### 26.4 CLOS と `copy`

| 機能                                         | cl-cc | SBCL | 備考                              |
| -------------------------------------------- | ----- | ---- | --------------------------------- |
| `copy-structure` (for `defstruct`)           | ✅    | ✅   | `defstruct` が自動生成            |
| CLOS インスタンスの `copy` (非標準)          | —     | —    | ANSI CL に `copy-instance` はない |
| `(setf (slot-value copy 'x) ...)` 手動コピー | ✅    | ✅   | 標準的な方法                      |

## 27. CLOS と `setf` の統合

| 機能                            | cl-cc | SBCL | 備考                         |
| ------------------------------- | ----- | ---- | ---------------------------- |
| `(setf (slot-value ...))`       | ✅    | ✅   | スロット書き込みの基本経路   |
| `(setf (accessor ...))`         | ✅    | ✅   | `:accessor` / `:writer` 由来 |
| `with-slots` / `with-accessors` | ✅    | ✅   | `symbol-macrolet` 展開を使う |
| `get-setf-expansion`            | ✅    | ✅   | 5 値プロトコルを提供         |

### 27.1 自動生成される `setf` 展開

| 機能                                         | cl-cc | SBCL | 備考                                       |
| -------------------------------------------- | ----- | ---- | ------------------------------------------ |
| `(setf (slot-value obj 'x) v)`               | ✅    | ✅   | `vm-slot-write` 命令。expander.lisp で展開 |
| `(setf (accessor obj) v)` — `:accessor` 由来 | ✅    | ✅   | `expand-setf-accessor` で展開済み          |
| `(writer v obj)` — `:writer` 形式            | ✅    | ✅   | writer は `(setf reader)` ではなく専用関数 |

### 27.2 `with-slots` / `with-accessors` の展開

| 機能                                         | cl-cc | SBCL | 備考                                            |
| -------------------------------------------- | ----- | ---- | ----------------------------------------------- |
| `with-slots (slot ...) obj body`             | ✅    | ✅   | `macros-stdlib.lisp` 実装済み                   |
| `with-slots` の `symbol-macrolet` 展開       | ✅    | ✅   | `(slot-value obj 'slot)` にシンボルマクロ展開   |
| `(with-slots (x y) obj (setf x 1))`          | ✅    | ✅   | `setf` も `(setf (slot-value obj 'x) 1)` に展開 |
| `with-accessors (var accessor ...) obj body` | ✅    | ✅   | `(accessor obj)` にシンボルマクロ展開           |
| `with-slots` でのネスト                      | ✅    | ✅   | 複数オブジェクトを同時バインド不可 (単一形式)   |

---

### 27.3 `setf` の5値プロトコル (`get-setf-expansion`)

`defclass` の `:writer` / `:accessor` は `setf` 展開を自動定義する。

| 機能                                         | cl-cc | SBCL | 備考                                       |
| -------------------------------------------- | ----- | ---- | ------------------------------------------ |
| `(setf (slot-value obj 'x) v)`               | ✅    | ✅   | `vm-slot-write` 命令。expander.lisp で展開 |
| `(setf (accessor obj) v)` — `:accessor` 由来 | ✅    | ✅   | `expand-setf-accessor` で展開済み          |
| `(writer v obj)` — `:writer` 形式            | ✅    | ✅   | writer は `(setf reader)` ではなく専用関数 |
| `defsetf` でのカスタム `setf` 定義           | ✅    | ✅   | 短形式・長形式とも対応                     |
| `define-setf-expander`                       | ✅    | ✅   | `get-setf-expansion` が返す5値を制御       |
| `get-setf-expansion`                         | ✅    | ✅   | 任意の場所の setf 展開を返す               |

**cl-cc の現実装 (`expander.lisp`):**

```lisp
;; *accessor-slot-map* = HT (accessor-sym → (class-name . slot-name))
;; (setf (point-x p) 42) → expand-setf-accessor → vm-slot-write
```

`with-slots` は `symbol-macrolet` を使ってスロットアクセスをローカルシンボルにバインドする。

**展開例:**

```lisp
(with-slots (x y) point-obj
  (setf x 10 y 20))
;; → (symbol-macrolet ((x (slot-value point-obj 'x))
;;                      (y (slot-value point-obj 'y)))
;;     (setf x 10 y 20))
;; → (setf (slot-value point-obj 'x) 10
;;         (slot-value point-obj 'y) 20)
```

```lisp
;; Schematic example: actual temps/vals/newvals depend on the place form.
;; (get-setf-expansion place env) → (temps vals newvals setter getter)
(get-setf-expansion '(slot-value obj 'x) env)
;; temps:   (#:G001)          ; 一時変数
;; vals:    (obj)             ; 一時変数に束縛する式
;; newvals: (#:NEW001)        ; 新しい値を受け取る変数
;; setter:  (setf (slot-value #:G001 'x) #:NEW001)
;; getter:  (slot-value #:G001 'x)
```

---

- **ANSI Common Lisp**: ANSI INCITS 226-1994 — Chapter 4 (Types), Chapter 7 (Objects)
- **AMOP**: _The Art of the Metaobject Protocol_ (Kiczales et al., 1991)
- **SBCL Internals**: `src/pcl/` — PCL (Portable Common Loops) をベースに SBCL が拡張
- **CCL CLOS**: `level-1/l1-clos-*.lisp`
- **C3 線形化**: Barrett et al., "A Monotonic Superclass Linearization for Dylan" (OOPSLA 1996)
- **closer-mop**: https://github.com/pcostanza/closer-mop — MOP ポータビリティ層
- **cl-cc 実装**: `packages/vm/src/vm-clos.lisp`, `packages/compile/src/codegen-clos.lisp`, `packages/vm/src/vm-execute.lisp:383-457`

---

## 28. 構造体 (ANSI CL Ch.8)

| 機能                                               | 状態 | 備考                                                                                                                                                                                                                |
| -------------------------------------------------- | ---- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `defstruct` (基本: コンストラクタ・述語・アクセサ) | ✅   | `expander-defstruct.lisp`                                                                                                                                                                                           |
| `:constructor` オプション (名前/BOA)               | ✅   | カスタム名・BOA (`&aux` 含む) 完全対応 (`expander-defstruct.lisp:15-62`)                                                                                                                                            |
| `:copier` / `:predicate` オプション                | ✅   | FR-544: `(:predicate nil)` / `(:constructor nil)` で抑制対応                                                                                                                                                        |
| `:print-function` / `:print-object`                | ✅   | FR-544: `expander-defstruct.lisp` でオプション解析; `defmethod print-object` を自動生成                                                                                                                             |
| `:include` 継承                                    | ✅   | FR-545: 子 defclass に `own-slots` のみ出力; 親スロットは CLOS 継承経由; アクセサ登録も own-slots のみ                                                                                                              |
| `:type` オプション (list/vector)                   | ✅   | FR-546: `(:type list)` → リスト表現 (car=型タグ, cdr=スロット値); `(:type vector)` → ベクタ表現 (aref 0=型タグ); コンストラクタ・アクセサ・述語を自動生成; `:conc-name`/`:include`/`:constructor`/`:predicate` 対応 |
| `:conc-name` オプション                            | ✅   | `expander-defstruct.lisp:79`                                                                                                                                                                                        |
| スロットオプション `:read-only`                    | ✅   | `:read-only t` で `:reader` のみ生成 (setfアクセサなし); `expander-defstruct.lisp`                                                                                                                                  |
| スロットオプション `:type`                         | ✅   | パーサーで受理; 実行時型チェックなし (ANSI では enforcement は実装定義)                                                                                                                                             |
| `copy-structure`                                   | ✅   | FR-555: `copy-hash-table` に委譲 (VM structs are hash-tables with :**class**)                                                                                                                                       |
| `structure-object` 型 (CLOS 統合)                  | ✅   | FR-528: stdlib-source.lisp で空 defclass 定義                                                                                                                                                                       |

#### FR-544: defstruct コンストラクタ・コピー・述語オプション

- **対象**: `packages/expand/src/expander-defstruct.lisp`
- **内容**: ANSI CL 8.1.5〜8.1.7 の defstruct オプション処理
- **難易度**: Medium

#### FR-545: defstruct :include 継承 — 子スロットアクセサ生成バグ

- **対象**: `packages/expand/src/expander-defstruct.lisp`
- **内容**: `:include` 継承の構造体定義を完全にサポートする
- **根拠**: ANSI CL 8.1.5.5
- **難易度**: Low

#### FR-546: defstruct :type / :conc-name / スロットオプション

- **対象**: `packages/expand/src/expander-defstruct.lisp`
- **内容**: `:type list`/`:type vector`、`:conc-name`、スロット `:type`/`:read-only`
- **根拠**: ANSI CL 8.1.5
- **難易度**: Medium

#### FR-555: copy-structure

- **対象**: `packages/expand/src/expander-defstruct.lisp`
- **内容**: `(copy-structure structure-object)` — 全スロットをシャローコピーした新インスタンスを返す
- **根拠**: ANSI CL 8.1.6 — copy-structure
- **難易度**: Easy

#### FR-556: #S 読み取り構文

- **対象**: `packages/parse/src/cl/lexer.lisp`
- **内容**: `#S(struct-name :slot val ...)` を読み取れるようにし、必要に応じて `make-struct-name` 形式へ落とし込む。
- **根拠**: ANSI CL 2.4.8.13
- **難易度**: Medium

---

## 29. コンディション・リスタート (ANSI CL Ch.9)

### 29.0 define-condition スロット仕様

`define-condition` のスロット仕様は `defclass` に準じる。

| オプション                          | 状態 | 備考                                                                                                                        |
| ----------------------------------- | ---- | --------------------------------------------------------------------------------------------------------------------------- |
| `:initarg`                          | ✅   | `defclass` 展開で `:initarg` キーワード生成; `make-instance` で動作                                                         |
| `:accessor` / `:reader` / `:writer` | ✅   | `define-condition` → `defclass` 展開; `defclass` が reader/writer/accessor メソッド生成済み                                 |
| `:initform`                         | ✅   | `defclass` 展開で `:initform` デフォルト値生成; `make-instance` で動作                                                      |
| `:type`                             | ✅   | パーサーで受理; 実行時型チェックなし (ANSI では enforcement は実装定義)                                                     |
| `:report`                           | ✅   | FR-417: `define-condition` で `:report` → `defmethod print-object` 生成 (文字列/ラムダ/関数名)                              |
| 親コンディション型からの継承        | ✅   | FR-424: stdlib-source.lisp で全 ANSI 標準コンディション型を `define-condition` で定義; 継承チェーンを `defclass` 経由で確立 |

### 29.1 コンディション定義・生成

| 機能                          | 状態 | 備考                                                                                   |
| ----------------------------- | ---- | -------------------------------------------------------------------------------------- |
| `define-condition`            | ✅   | FR-417: `:report` オプション対応; 文字列/ラムダ/関数名 → `defmethod print-object` 生成 |
| `make-condition`              | ✅   | FR-427: `stdlib-source.lisp` で `(apply #'make-instance type initargs)` に委譲         |
| ANSI 標準コンディション型階層 | ✅   | FR-424: stdlib-source.lisp に define-condition で全型定義                              |

標準コンディション型 (FR-424 詳細):

| 型                                                                  | 状態 | 備考                                                       |
| ------------------------------------------------------------------- | ---- | ---------------------------------------------------------- |
| `condition`                                                         | ✅   |
| `serious-condition`                                                 | ✅   |
| `error` (型)                                                        | ✅   | ホスト CL `error` 型として `typep`/`handler-case` で動作   |
| `warning`                                                           | ✅   | ホスト CL `warning` 型として `typep`/`handler-case` で動作 |
| `simple-condition` / `simple-error` / `simple-warning`              | ✅   |
| `type-error` / `simple-type-error`                                  | ✅   |
| `arithmetic-error` / `division-by-zero` / `floating-point-overflow` | ✅   |
| `cell-error` / `unbound-variable` / `undefined-function`            | ✅   |
| `control-error` / `program-error`                                   | ✅   |
| `package-error`                                                     | ✅   |
| `stream-error` / `end-of-file` / `file-error`                       | ✅   |
| `print-not-readable` / `storage-condition`                          | ✅   |

### 29.2 コンディション アクセサ

| アクセサ                                                   | 状態 | 備考                                                                                       |
| ---------------------------------------------------------- | ---- | ------------------------------------------------------------------------------------------ |
| `condition-report` / `princ` で報告                        | ✅   | FR-417: `define-condition :report` → `defmethod print-object` 生成; `princ` 経由で呼び出し |
| `type-error-datum` / `type-error-expected-type`            | ✅   | FR-424: type-error スロットリーダーとして定義                                              |
| `cell-error-name`                                          | ✅   | FR-424: cell-error スロットリーダーとして定義                                              |
| `arithmetic-error-operands` / `arithmetic-error-operation` | ✅   | FR-424: arithmetic-error スロットリーダーとして定義                                        |
| `stream-error-stream` / `file-error-pathname`              | ✅   | FR-424: stream-error/file-error スロットリーダーとして定義                                 |

### 29.3 シグナリング

| 機能                 | 状態 | 備考                                                                                                     |
| -------------------- | ---- | -------------------------------------------------------------------------------------------------------- |
| `error`              | ✅   | FR-626: `expander.lisp` で `(error "fmt" args...)` → `(error (format nil "fmt" args...))` 脱糖実装済み   |
| `warn`               | ✅   | FR-425: `~&WARNING: fmt` 形式で出力・nil 返却; `muffle-warning` リスタートはリスタート基盤の制約で未設置 |
| `signal`             | ✅   | FR-418: `vm-signal` VM命令 + ビルトイン登録済み                                                          |
| `cerror`             | ✅   | FR-419: `vm-cerror` VM命令 + ビルトイン登録済み                                                          |
| `break`              | ✅   | FR-557                                                                                                   |
| `invoke-debugger`    | ✅   | FR-557                                                                                                   |
| `*debugger-hook*`    | ✅   | FR-557                                                                                                   |
| `*break-on-signals*` | ✅   | FR-557                                                                                                   |

#### FR-585: handler-case :no-error 節

- **対象**: `packages/expand/src/macros-stdlib.lisp`
- **実装**: `handler-case` は `:no-error` 節を受理し、正常終了時の多値を束縛できる
- **内容**: フォームがシグナルなしに完了した場合のみ `success-body` を実行する
- **根拠**: ANSI CL 9.1.3 — handler-case :no-error
- **難易度**: Low

#### FR-557: break / invoke-debugger / _debugger-hook_ / _break-on-signals_

- **対象**: `packages/cli/src/main.lisp`, `packages/vm/src/conditions.lisp`
- **実装**: `break` / `invoke-debugger` / `*debugger-hook*` / `*break-on-signals*` は利用可能
- **内容**: `break` は `*debugger-hook*` を介したブレーク、`invoke-debugger` は条件付きデバッガ起動、`*break-on-signals*` は条件一致で自動ブレークを提供する
- **根拠**: ANSI CL 9.1.5 — Debugger
- **難易度**: Medium

### 29.4 ハンドラ・リスタート

| 機能                                | 状態 | 備考                                                                                                                                 |
| ----------------------------------- | ---- | ------------------------------------------------------------------------------------------------------------------------------------ |
| `handler-case`                      | ✅   |                                                                                                                                      |
| `handler-case :no-error` 節         | ✅   | FR-585: `expander.lisp` で実装                                                                                                       |
| `handler-bind`                      | ✅   | FR-356: `progv` 動的束縛 + `*%condition-handlers*` レジストリで非巻き戻しセマンティクス実装; `signal` マクロがハンドラチェーンを走査 |
| `ignore-errors`                     | ✅   | FR-691: `(values nil condition)` を正しく返却                                                                                        |
| `restart-case`                      | ✅   | FR-420: catch/throw ベースのリスタートプロトコル; 複数節・オプション解析対応; `*%active-restarts*` 動的レジストリ                    |
| `invoke-restart`                    | ✅   | FR-421: `*%active-restarts*` から検索 → `throw` で制御移行                                                                           |
| `invoke-restart-interactively`      | ✅   | FR-421: `invoke-restart` に委譲 (対話的引数収集なし)                                                                                 |
| `find-restart` / `compute-restarts` | ✅   | FR-421: `*%active-restarts*` alist 検索・全リスタート返却                                                                            |
| `restart-name`                      | ✅   | cons セルの car から名前抽出                                                                                                         |
| `restart-p`                         | ✅   | FR-421: `consp` チェック                                                                                                             |

### 29.5 標準リスタート関数

| 関数             | 状態 | 備考                                                                 |
| ---------------- | ---- | -------------------------------------------------------------------- |
| `abort`          | ✅   | `find-restart` + `invoke-restart` で実装; リスタートなければ `error` |
| `continue`       | ✅   | FR-421: `find-restart` + 条件付き `invoke-restart`                   |
| `use-value`      | ✅   | `find-restart` + `invoke-restart` またはパススルー                   |
| `store-value`    | ✅   | `find-restart` + `invoke-restart` またはパススルー                   |
| `muffle-warning` | ✅   | FR-425: `find-restart` + 条件付き `invoke-restart`                   |

### 29.6 コンディション チェック

| 機能         | 状態 | 備考                                                                                 |
| ------------ | ---- | ------------------------------------------------------------------------------------ |
| `check-type` | ✅   | FR-426: `type-error` をシグナルし、`store-value` リスタートで place を更新して再検査 |
| `assert`     | ✅   | FR-606: `cerror` による continuable error; ANSI 基本形は完備                         |

---

## 30. シンボル (ANSI CL Ch.10)

| 関数                                | 状態 | 備考                                                                                            |
| ----------------------------------- | ---- | ----------------------------------------------------------------------------------------------- |
| `intern` (2値返却: シンボル + 状態) | ✅   | ホストブリッジ経由でホスト CL `intern` の2値返却がそのまま動作                                  |
| `find-symbol`                       | ✅   | FR-655: ホストブリッジ登録済み (`vm.lisp`)                                                      |
| `symbol-name`                       | ✅   |                                                                                                 |
| `symbol-value`                      | ✅   | FR-647: ホストブリッジ登録済み (`vm.lisp`)                                                      |
| `symbol-function`                   | ✅   | ホストブリッジ経由 (`vm.lisp:491`)                                                              |
| `(setf symbol-value)`               | ✅   | `setq` 経由                                                                                     |
| `(setf symbol-function)`            | ✅   | setf ハンドラ → `set-fdefinition` stdlib 経由; ホスト `(setf (symbol-function name) fn)` に委譲 |
| `symbol-plist` / `get` / `remprop`  | ✅   |                                                                                                 |
| `(setf get)`                        | ✅   | FR-622: expander + lowering/runtime の両経路で `symbol-plist` を更新                            |
| `symbol-package`                    | ✅   | FR-558: ホストブリッジ経由で `symbol-package` に委譲                                            |
| `boundp` / `fboundp`                | ✅   |                                                                                                 |
| `makunbound` / `fmakunbound`        | ✅   |                                                                                                 |
| `keywordp`                          | ✅   |                                                                                                 |
| `make-symbol`                       | ✅   |                                                                                                 |
| `gensym`                            | ✅   |                                                                                                 |
| `*gensym-counter*`                  | ✅   | FR-510: `stdlib-source.lisp` で defvar 定義                                                     |
| `gentemp`                           | ✅   | FR-511                                                                                          |
| `copy-symbol`                       | ✅   | FR-536: `stdlib-source.lisp` で実装 (プロパティコピーなし)                                      |
| `set` (obsolete)                    | ✅   | FR-586: `stdlib-source.lisp` で `(defun set (sym val) (setf (symbol-value sym) val) val)` 実装  |

#### FR-586: set (廃止予定だが ANSI 標準)

- **対象**: `packages/compile/src/builtin-registry.lisp`
- **内容**: `set symbol value` をビルトイン登録。`(setf (symbol-value symbol) value)` と同義
- **根拠**: ANSI CL 10.2 — set (廃止予定だが標準準拠に必要)
- **難易度**: Easy

#### FR-558: symbol-package

- **対象**: `packages/vm/src/vm.lisp`, `packages/compile/src/builtin-registry.lisp`
- **実装**: `symbol-package` はホストブリッジ経由で利用できる
- **内容**: シンボルの所属パッケージを返す
- **根拠**: ANSI CL 11.2 — Symbol Package
- **難易度**: Hard (パッケージシステム実装に依存)

---

## 31. パッケージ (ANSI CL Ch.11)

| 機能                                                    | 状態 | 備考                                                                                                                    |
| ------------------------------------------------------- | ---- | ----------------------------------------------------------------------------------------------------------------------- |
| `in-package`                                            | ✅   | FR-437: `*package*` を `setq` で設定し `(quote name)` を返す; VM グローバルに `*package*` 登録済み                      |
| `*package*` 変数                                        | ✅   | VM `*vm-initial-globals*` + `*builtin-special-variables*` で初期化; ホスト CL-USER にバインド                           |
| `defpackage`                                            | ✅   | FR-437: `find-package` / `make-package` で実パッケージ作成; `:use` / `:export` オプション対応                           |
| `find-package`                                          | ✅   | FR-437: ホストブリッジ経由で完全動作; コンパイル済みコードから呼び出し可能                                              |
| `list-all-packages`                                     | ✅   | FR-559: ホストブリッジ経由で委譲                                                                                        |
| `make-package`                                          | ✅   | FR-437: ホストブリッジ経由で委譲                                                                                        |
| `export`                                                | ✅   | FR-437: ホストブリッジ経由で委譲 (スタブマクロ削除)                                                                     |
| `import` / `unexport` / `shadow` / `shadowing-import`   | ✅   | FR-437: ホストブリッジ経由で委譲                                                                                        |
| `use-package` / `unuse-package`                         | ✅   | FR-437: ホストブリッジ経由で委譲                                                                                        |
| `rename-package` / `delete-package`                     | ✅   | FR-437: ホストブリッジ経由で委譲                                                                                        |
| `with-package-iterator`                                 | ✅   | FR-361: `%package-external-symbols` ホストブリッジ経由で実イテレーション; `(values more sym access pkg)` プロトコル準拠 |
| `do-symbols` / `do-external-symbols` / `do-all-symbols` | ✅   | FR-361: マクロ展開で `%package-symbols`/`%package-external-symbols` ホストブリッジヘルパー経由で dolist に展開          |
| `package-name` / `package-nicknames`                    | ✅   | FR-438: ホストブリッジ経由で委譲                                                                                        |
| `package-use-list` / `package-used-by-list`             | ✅   | FR-438: ホストブリッジ経由で委譲                                                                                        |
| `package-shadowing-symbols`                             | ✅   | FR-438: ホストブリッジ経由で委譲                                                                                        |

#### FR-559: list-all-packages

- **対象**: `packages/vm/src/vm.lisp`, `packages/compile/src/builtin-registry.lisp`
- **実装**: `list-all-packages` はホストブリッジ経由で利用できる
- **内容**: 全パッケージのリストを返す
- **根拠**: ANSI CL 11.2
- **難易度**: Low (FR-437 後)

---

## 32. 数値 (ANSI CL Ch.12)

### 32.1 基本算術・比較

| 演算                                                 | 状態 | 備考                                                                                   |
| ---------------------------------------------------- | ---- | -------------------------------------------------------------------------------------- |
| `+` / `*` (可変引数)                                 | ✅   | `define-list-lowerer` で `ast-binop` に変換後 `vm-add`/`vm-mul` を生成                 |
| `-` (2引数以上)                                      | ✅   |                                                                                        |
| `-` (1引数: 単項否定)                                | ✅   | FR-682: `expander.lisp:431` で単項否定実装済み                                         |
| `-` (0引数)                                          | ✅   | FR-682: `expander.lisp:430` で0引数エラー実装済み                                      |
| `/`                                                  | ✅   | FR-661: `vm-cl-div` 命令 + expander で1/2/N引数対応; rational-preserving division      |
| `=` / `<` / `>` / `<=` / `>=` (可変引数)             | ✅   | FR-663: expander で1-arg→T, 2-arg→builtin, N-arg→AND chain with gensyms                |
| `/=`                                                 | ✅   | FR-664: `stdlib-source.lisp` で実装                                                    |
| `zerop` / `plusp` / `minusp`                         | ✅   |
| `abs`                                                | ✅   |
| `min` / `max`                                        | ✅   | FR-662: expander で1/2/N引数対応; 左畳み込みで可変引数化                               |
| `1+` / `1-` / `incf` / `decf`                        | ✅   | FR-693: シンボルplace→`setq`、複合place→delta を gensym ガード; 2重評価修正            |
| `floor` / `ceiling` / `truncate` / `round` (2値返却) | ✅   |
| `ffloor` / `fceiling` / `ftruncate` / `fround`       | ✅   |
| `mod` / `rem`                                        | ✅   |
| `expt` / `sqrt`                                      | ✅   |                                                                                        |
| `isqrt`                                              | ✅   | FR-683: 整数 Newton 法で精度保証; `(floor (sqrt (float n)))` + Newton 修正ループ       |
| `signum`                                             | ✅   | FR-684: macros-stdlib.lisp で型保存実装 (`(signum 2.5)` → `1.0`, `(signum -3)` → `-1`) |
| `evenp` / `oddp`                                     | ✅   |
| `gcd` / `lcm`                                        | ✅   | FR-662: expander で0/1/2/N引数対応; `(gcd)` → `0`, `(lcm)` → `1`, N引数は左畳み込み    |

### 32.2 浮動小数点型塔

cl-cc は内部で全浮動小数点を NaN-boxing double-float に統一。ユーザーコードからの型区別:

| 機能                                                                    | 状態 | 備考                                                                            |
| ----------------------------------------------------------------------- | ---- | ------------------------------------------------------------------------------- |
| `floatp`                                                                | ✅   | FR-386: `stdlib-source.lisp` で実装                                             |
| `short-float` / `single-float` / `double-float` / `long-float` 型指定子 | ✅   | `vm-typep-check` で全て `floatp` にマッピング; `real`/`rational`/`bit` 型も追加 |
| `short-float-epsilon` / `single-float-epsilon` 等定数                   | ✅   | FR-560: `stdlib-source.lisp` で実装                                             |
| `most-positive-short-float` 等定数群                                    | ✅   | FR-560: `stdlib-source.lisp` で実装                                             |
| `least-positive-short-float` 等定数群                                   | ✅   | FR-560: `stdlib-source.lisp` で実装                                             |
| `pi` / `most-positive-fixnum` 等                                        | ✅   | FR-561: `stdlib-source.lisp` で defvar 定義済み                                 |
| `decode-float` / `scale-float`                                          | ✅   |                                                                                 |
| `integer-decode-float`                                                  | ✅   | `builtin-registry-data.lisp:70`                                                 |
| `float-digits` / `float-precision` / `float-radix`                      | ✅   | `builtin-registry-data.lisp:65-68`                                              |
| `float-sign` (1引数)                                                    | ✅   |                                                                                 |
| `float-sign` (2引数)                                                    | ✅   | FR-685: expander で `(* (float-sign x) (abs y))` に展開                         |
| `float` (1引数: 変換)                                                   | ✅   |                                                                                 |
| `cis`                                                                   | ✅   | FR-508: `stdlib-source.lisp` で実装                                             |

#### FR-560: 浮動小数点定数群

- **対象**: `packages/vm/src/vm.lisp`
- **内容**: ANSI CL 12.1.6 の全浮動小数点定数を VM global として初期化。`short-float-epsilon`, `single-float-epsilon`, `double-float-epsilon`, `long-float-epsilon` および `most-positive-*`, `least-positive-*` 各系列
- **根拠**: ANSI CL 12.1.6 — Float Constants
- **難易度**: Easy

### 32.3 整数・ビット演算

| 演算                                                  | 状態      | 備考                                                                                   |
| ----------------------------------------------------- | --------- | -------------------------------------------------------------------------------------- |
| `ash`                                                 | ✅        |
| `logand` / `logior` / `logxor` / `logeqv`             | ✅        | FR-667: expander で0/1/2/N引数対応; identity: logand=-1, logior=0, logxor=0, logeqv=-1 |
| `lognot`                                              | ✅        |                                                                                        |
| `logtest` / `logbitp` / `logcount` / `integer-length` | ✅        |
| `logorc1` / `logorc2` / `logandc1` / `logandc2`       | ✅        | FR-529: `stdlib-source.lisp` で実装                                                    |
| `lognand` / `lognor`                                  | ✅        | FR-529: `stdlib-source.lisp` で実装                                                    |
| `ldb` / `dpb` / `byte` / `ldb-test`                   | ✅ FR-492 |
| `byte-size` / `byte-position`                         | ✅ FR-532 |
| `deposit-field` / `mask-field`                        | ✅ FR-494 |
| `boole` (+ `boole-*` 定数 16 種)                      | ✅ FR-493 |

### 32.4 有理数・複素数

| 関数                                            | 状態 | 備考                                                                                            |
| ----------------------------------------------- | ---- | ----------------------------------------------------------------------------------------------- |
| `rational` / `rationalize`                      | ✅   |
| 比率 (`ratio`) 算術演算 (`/` で自動生成)        | ✅   | `vm-rational` (`vm-numeric.lisp:568`)                                                           |
| `bignum` (多倍長整数)                           | ✅   | FR-605: ホスト CL 委譲で完全動作; `(expt 2 100)` `(* 10^11 10^11)` `(integerp bignum)` 検証済み |
| `numerator` / `denominator`                     | ✅   |
| `realpart` / `imagpart` / `conjugate` / `phase` | ✅   |
| `complex`                                       | ✅   |
| `rationalp` / `complexp` / `realp`              | ✅   | FR-386: `stdlib-source.lisp` で実装                                                             |

### 32.5 超越関数

| 関数                               | 状態 | 備考                                                          |
| ---------------------------------- | ---- | ------------------------------------------------------------- |
| `exp` / `log` (1引数)              | ✅   |
| `log` (2引数: 任意底)              | ✅   | FR-476: expander で底変換公式 `(/ (log x) (log base))` に展開 |
| `sin` / `cos` / `tan`              | ✅   |
| `asin` / `acos` / `atan` (1/2引数) | ✅   |
| `sinh` / `cosh` / `tanh`           | ✅   |
| `asinh` / `acosh` / `atanh`        | ✅   | FR-639: VM命令 + ビルトイン登録済み                           |

### 32.6 数値パース・変換

| 関数                                                       | 状態 | 備考                                                                                                       |
| ---------------------------------------------------------- | ---- | ---------------------------------------------------------------------------------------------------------- |
| `parse-integer` (基本)                                     | ✅   |
| `parse-integer` (`:start`/`:end`/`:radix`/`:junk-allowed`) | ✅   | FR-478: expander でキーワード引数解析 → `%parse-integer-impl` stdlib関数; 1引数は builtin直通              |
| `parse-number` (汎用: 浮動小数点・比率)                    | ✅   | FR-391: `stdlib-source.lisp` で `read-from-string` に委譲; レキサーは float/ratio リテラルをネイティブ解析 |
| `number-to-string` 相当 / `write-to-string`                | ✅   |

### 32.7 乱数・時刻

| 関数                                                                      | 状態      | 備考                                              |
| ------------------------------------------------------------------------- | --------- | ------------------------------------------------- |
| `random` (1引数)                                                          | ✅        | ホスト CL の `random` に委譲; 1引数形式は完全動作 |
| `make-random-state`                                                       | ✅        |                                                   |
| `random-state-p`                                                          | ✅ FR-509 |
| `*random-state*`                                                          | ✅        |
| `get-universal-time` / `get-internal-real-time` / `get-internal-run-time` | ✅        |
| `get-decoded-time`                                                        | ✅        | FR-679: `stdlib-source.lisp` で実装               |
| `decode-universal-time` / `encode-universal-time`                         | ✅        |
| `internal-time-units-per-second` 定数                                     | ✅        | FR-561: `stdlib-source.lisp` で実装               |

#### FR-561: 時刻・数値定数

- **対象**: `packages/vm/src/vm.lisp`
- **内容**: VM 起動時に登録。`get-internal-real-time` が正しいスケールで機能するために必要
- **根拠**: ANSI CL 25.1.4 / 12.1
- **難易度**: Easy

---

## 33. 文字 (ANSI CL Ch.13)

| 関数                                                               | 状態      | 備考                                                                              |
| ------------------------------------------------------------------ | --------- | --------------------------------------------------------------------------------- |
| `characterp`                                                       | ✅        |
| `char=` / `char<` / `char>` / `char<=` / `char>=` / `char/=`       | ✅        | FR-645: expander で可変引数対応; AND chain with gensyms                           |
| `char-equal` / `char-lessp` 等 (case-insensitive)                  | ✅        | FR-645: 同上、case-insensitive 版も可変引数対応                                   |
| `char-upcase` / `char-downcase`                                    | ✅        |
| `char-code` / `code-char`                                          | ✅        |
| `char-name` / `name-char`                                          | ✅        |
| `char-int`                                                         | ✅        |
| `graphic-char-p` / `alpha-char-p` / `alphanumericp`                | ✅        |
| `both-case-p` / `upper-case-p` / `lower-case-p`                    | ✅        |
| `digit-char-p`                                                     | ✅        | FR-668: 1引数→builtin、2引数→char-code算術による radix 対応 (expander)            |
| `standard-char-p`                                                  | ✅        |
| `digit-char` (1引数)                                               | ✅        |
| `digit-char` (2引数: 基数付き)                                     | ✅ FR-477 |
| 標準文字名 (`#\Space` `#\Newline` `#\Tab` `#\Return`)              | ✅        | `lexer.lisp:28`                                                                   |
| 標準文字名 (`#\Backspace` `#\Page` `#\Rubout` `#\Null` `#\Escape`) | ✅        | `lexer.lisp` で直接ハンドリング; `#\Null`/`#\Escape`/`#\Esc` 追加                 |
| `base-char` / `extended-char` 型区別                               | ✅        | FR-392: `vm-typep-check` で `base-char`/`standard-char` → `characterp` マッピング |
| Unicode 文字 (code-point > 127)                                    | ✅        | 21-bit コードポイント対応 (`value.lisp:24`); `encode-char`/`decode-char` 完全動作 |

#### FR-562: Unicode サポート

- **対象**: `packages/parse/src/cl/lexer.lisp`, `packages/vm/src/strings.lisp`
- **内容**: Unicode code point 全域 (U+0000〜U+10FFFF) のキャラクタ表現。`char-code` / `code-char` の 21-bit 対応。UTF-8/UTF-16 ストリームエンコーディング指定
- **根拠**: 2026 年モダン CL — SBCL / CCL は完全 Unicode 対応
- **難易度**: Medium

---

## Summary

| Status         | Count   | Details                                                                                                                                              |
| -------------- | ------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| ✅ Complete    | 698     | ANSI CL language core features implemented and documented                                                                                            |
| — Not in cl-cc | 22      | MOP advanced hooks (10), long-form `define-method-combination` (3), SBCL-specific extensions (7), non-standard (2)                                   |
| **Total**      | **720** | ANSI CL Ch.3–13: Evaluation, Lambda Lists, Types, Data/Control Flow, Iteration, CLOS, Structures, Conditions, Symbols, Packages, Numbers, Characters |
