# ANSI CL: Language Core

Evaluation/compilation, lambda lists, types/classes, data/control flow, iteration, CLOS (object system), structures, conditions/restarts, symbols, packages, numbers, characters.


---
## 1. 評価・コンパイル (ANSI CL Ch.3)

### 1.1 特殊形式

| 形式 | 状態 | 備考 |
|------|------|------|
| `quote` | ✅ | |
| `if` | ✅ | |
| `progn` | ✅ | |
| `let` / `let*` | ✅ | `let*` はマクロ展開で `let` にデシュガー；FR-623: `(let () body)` → `(progn body)` 対応済み |
| `flet` / `labels` | ✅ | FR-623: `(flet () body)` / `(labels () body)` → `(progn body)` 対応済み |
| `macrolet` | ✅ | |
| `symbol-macrolet` | ✅ | FR-220: `expand-symbol-macrolet-form` で `*symbol-macro-table*` にバインド、`compiler-macroexpand-all` アトムパスで展開 |
| `lambda` / `function` (`#'`) | ✅ | |
| `setq` / `psetq` | ✅ | |
| `block` / `return-from` | ✅ | |
| `tagbody` / `go` | ✅ | |
| `catch` / `throw` | ✅ | `vm-establish-catch`/`vm-throw` VM命令実装済み; タグベースのEQ一致、ネスト対応、非局所脱出動作 |
| `unwind-protect` | ✅ | |
| `multiple-value-call` | ✅ | |
| `multiple-value-prog1` | ✅ | |
| `the` | ✅ | |
| `locally` | ✅ | FR-397: `(let () decl body)` に展開して宣言を保持 |
| `load-time-value` | ✅ | コンパイル時 `eval` で評価し `(quote result)` に展開; `macros-compat.lisp` |
| `eval-when` | ✅ | `:compile-toplevel`/`:execute`/`:load-toplevel` の3状況対応; `expander.lisp` |
| `progv` | ✅ | |

### 1.2 評価・コンパイル API

| 関数・マクロ | 状態 | 備考 |
|-------------|------|------|
| `eval` | ✅ | `our-eval` で自己ホスト対応 |
| `macroexpand` / `macroexpand-1` | ✅ | FR-631: VM命令 + ビルトイン登録済み |
| `compiler-macroexpand-all` | ✅ | cl-cc 独自拡張 |
| `define-compiler-macro` | ✅ | FR-365: マクロ名を返却 (コンパイル時展開なし; ANSI 互換 no-op) |
| `compiler-macro-function` / `(setf compiler-macro-function)` | ✅ | FR-365: `stdlib-source.lisp` で実装 (常に nil 返却) |
| `macro-function` / `(setf macro-function)` | ✅ | FR-428: 読み取りはホストブリッジ; `(setf macro-function)` は `%set-macro-function` stdlib + setf ハンドラ実装 |
| `*macroexpand-hook*` | ✅ | FR-429: `stdlib-source.lisp` で defvar 定義 |
| `with-compilation-unit` | ✅ | FR-363: `macros-stdlib.lisp` でスタブ実装 |
| `compile` (関数) | ✅ | FR-512: ホストブリッジ経由で委譲 |
| `compile-file` | ✅ | `src/expand/macros-stdlib.lisp: compile-file` shim; `our-load` を呼び、3値を返す |
| `constantp` / `special-operator-p` | ✅ | FR-538: `stdlib-source.lisp` で実装 |
| `fdefinition` / `(setf fdefinition)` | ✅ | FR-548: `fdefinition` (読み取り) + `set-fdefinition` (書き込み) 両方 `stdlib-source.lisp` で実装 |
| `function-lambda-expression` | ✅ | FR-549: `stdlib-source.lisp` で実装 (常に nil 返却) |
| `identity` / `constantly` / `complement` | ✅ | FR-597: `builtin-registry-data.lisp` で unary ビルトイン登録済み + `stdlib-source.lisp` で定義 |
| `defun` / `defmacro` ドキュメント文字列 | ✅ | FR-607: expander で先頭 docstring を自動ストリップ (defun/lambda 両対応) |

#### FR-548: fdefinition / (setf fdefinition)

- **対象**: `src/vm/vm.lisp`, `src/compile/builtin-registry.lisp`
- **内容**: `(fdefinition name)` — 関数名 (シンボルまたは `(setf name)`) から関数オブジェクト取得。`(setf (fdefinition name) fn)` — 設定
- **根拠**: ANSI CL 5.3 — fdefinition
- **難易度**: Low

#### FR-549: function-lambda-expression

- **対象**: `src/vm/vm.lisp`
- **内容**: `(function-lambda-expression fn)` → `(lambda-expression closure-p name)` の3値。必要な場合は `nil` を返すが、API 自体は提供される。
- **根拠**: ANSI CL 5.3 — introspection
- **難易度**: Medium

## 2. ラムダリスト (ANSI CL Ch.3.4)

| 機能 | 状態 | 備考 |
|------|------|------|
| Required parameters | ✅ | |
| `&optional` パラメータ + デフォルト値 | ✅ | |
| `&optional` supplied-p 変数 | ✅ | FR-696: `allocate-defaulting-params` でレジスタ割当、`emit-supplied-p-checks` でセンチネル比較コード生成 |
| `&rest` パラメータ | ✅ | |
| `&key` パラメータ + デフォルト値 | ✅ | |
| `&key` supplied-p 変数 | ✅ | FR-696: 同上 |
| `&allow-other-keys` | ✅ | `expander.lisp:295` |
| `&aux` パラメータ | ✅ | |
| `&body` (マクロラムダリスト) | ✅ | |
| `&whole` (マクロラムダリスト) | ✅ | |
| `&environment` (マクロラムダリスト) | ✅ | FR-394: `parse-lambda-list` で解析・`our-defmacro` で環境オブジェクトに束縛; 環境はマクロ定義テーブル |
| Destructuring lambda list | ✅ | `destructuring-bind` 実装済み |
| Specialized lambda list (CLOS) | ✅ | `defmethod` で使用 |
| BOA constructor lambda list | ✅ | `expander-defstruct.lisp` |
| `call-arguments-limit` 定数 | ✅ | FR-551: `stdlib-source.lisp` で実装 |
| `lambda-parameters-limit` 定数 | ✅ | FR-551: `stdlib-source.lisp` で実装 |
| `lambda-list-keywords` 変数 | ✅ | FR-551: `stdlib-source.lisp` で実装 |

#### FR-551: ラムダリスト定数・変数 — ✅ COMPLETE

- **対象**: `src/vm/vm.lisp`
- **実装**: `call-arguments-limit` / `lambda-parameters-limit` / `multiple-values-limit` / `lambda-list-keywords` は起動時に VM global として登録済み。
- **内容**: これらの制限値とラムダリストキーワードを標準 CL と同じ名前で提供する。
- **根拠**: ANSI CL 3.4 / 12.1 — implementation limits
- **難易度**: Easy

---

## 3. 型・クラス (ANSI CL Ch.4)

| 機能 | 状態 | 備考 |
|------|------|------|
| `deftype` (2引数形式) | ✅ | |
| `deftype` (lambda-list 付き) | ✅ | FR-430: ANSI 形式 `(deftype name (params) body)` 対応; パラメータ付きはラムダ展開関数を登録 |
| `define-symbol-macro` | ✅ | FR-398: `*symbol-macro-table*` にグローバルシンボルマクロを登録 |
| `typep` | ✅ | |
| `subtypep` | ✅ | FR-624: ホストブリッジ経由で SBCL `subtypep` に委譲 |
| `type-of` | ✅ | FR-625: float は `double-float`/`single-float`、vm-closure-object は `function` を返却 |
| `class-of` | ✅ | FR-385: VM命令 `vm-class-of-fn` + ビルトイン登録済み |
| `find-class` / `(setf find-class)` | ✅ | FR-552: 読み取りは `vm-find-class`; `(setf find-class)` は `%set-find-class` stdlib + setf ハンドラ実装 |
| `coerce` | ✅ | FR-630: クォート済み型→直接展開 + `character`/`float` 追加; 動的型→ `%coerce-runtime` stdlib ディスパッチ |
| `upgraded-array-element-type` | ✅ | FR-553: `stdlib-source.lisp` で実装 (常に t 返却) |
| `upgraded-complex-part-type` | ✅ | FR-553: `stdlib-source.lisp` で実装 (常に real 返却) |
| `standard-object` / `structure-object` 組み込みクラス | ✅ | FR-528: stdlib-source.lisp で空 defclass 定義 |

#### FR-552: find-class / (setf find-class)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**: `(find-class name &optional errorp environment)` — クラスオブジェクト取得。`(setf (find-class name) class)` — 登録
- **根拠**: ANSI CL 7.7.6 — find-class
- **難易度**: Low

#### FR-553: upgraded-array-element-type / upgraded-complex-part-type

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry.lisp`
- **内容**: `upgraded-array-element-type type &optional env` — 型指定子を実際のアレイ element-type に昇格。`upgraded-complex-part-type` 同様
- **根拠**: ANSI CL 15.1.2.1, 12.1.5.2
- **難易度**: Medium

### 3.2 複合型指定子 (Compound Type Specifiers)

ANSI CL の型システムは原子型だけでなく合成型指定子をサポートする。`typep` / `subtypep` / `the` / `ftype` で使用。

| 型指定子 | 状態 | 備考 |
|---------|------|------|
| `(member obj...)` | ✅ | `vm-typep-check` で直接 `(member value ...)` ディスパッチ |
| `(satisfies predicate)` | ✅ | FR-581: `vm-typep-check` で `(satisfies pred)` → `(funcall pred value)` 直接ディスパッチ |
| `(and type...)` | ✅ | `vm-typep-check` で再帰的 `every` ディスパッチ |
| `(or type...)` | ✅ | `vm-typep-check` で再帰的 `some` ディスパッチ |
| `(not type)` | ✅ | `vm-typep-check` で再帰的 `not` ディスパッチ |
| `(eql x)` | ✅ | `vm-typep-check` で直接 `eql` ディスパッチ |
| `(values type...)` | ✅ | FR-581: `vm-typep-check` で `(values ...)` は常に T (単一値コンテキスト) |
| `(function (args) return)` | ✅ | FR-581: `vm-typep-check` で functionp/vm-closure-object-p チェック |
| `(integer low high)` 範囲 | ✅ | ホスト CL `typep` 委譲 |
| `(float low high)` 範囲 | ✅ | ホスト CL `typep` 委譲 |
| `(rational ...)` / `(real ...)` | ✅ | ホスト CL `typep` 委譲 |
| `(mod n)` | ✅ | ホスト CL `typep` 委譲 |
| `(array element-type dimensions)` | ✅ | FR-581: `array-element-type` → `T` (全配列が全型受入), `upgraded-array-element-type` → `T`, `adjustable-array-p` → `T`, `array-has-fill-pointer-p` → `NIL`; `array-rank`/`array-dimensions`/`array-dimension`/`array-total-size` 完全動作 |
| `(simple-array ...)` / `(vector ...)` 複合 | ✅ | FR-581: `vm-typep-check` の `otherwise` ブランチでホスト CL `typep` に委譲; 複合型は動作 |
| `(complex type)` | ✅ | ホスト CL `typep` 委譲 |
| `fixnum` / `bignum` / `ratio` 型 | ✅ | `fixnum` は `vm-typep-check` case ブランチ; `bignum`/`ratio` はホスト CL `typep` 委譲で動作 |

#### FR-581: 複合型指定子完全サポート

- **対象**: `src/vm/primitives.lisp` の `vm-typep-check`, `src/compile/codegen.lisp`
- **内容**: ANSI CL 4.2 の全複合型指定子。`satisfies` では述語関数を呼び出し。`values` では多値型チェック。`(integer low high)` 範囲テスト
- **根拠**: ANSI CL 4.2 — Type Specifiers
- **難易度**: Medium

---

## 4. データ・制御フロー (ANSI CL Ch.5)

### 4.1 条件分岐

| 形式 | 状態 | 備考 |
|------|------|------|
| `when` / `unless` | ✅ | |
| `cond` | ✅ | |
| `and` / `or` / `not` | ✅ | |
| `case` / `ecase` | ✅ | FR-354: `ecase` now signals `type-error` with datum and expected-type |
| `ccase` | ✅ | FR-354: `type-error` をシグナル; `store-value` リスタートは未設置 |
| `typecase` / `etypecase` | ✅ | FR-354: `etypecase` now signals `type-error` with datum and expected-type |
| `ctypecase` | ✅ | FR-354: `type-error` をシグナル; `store-value` リスタートは未設置 |

### 4.2 代入・変更

| 形式 | 状態 | 備考 |
|------|------|------|
| `setf` | ✅ | |
| `psetf` | ✅ | |
| `shiftf` | ✅ | |
| `rotatef` | ✅ | FR-690: N引数対応; 2引数は `setq` で最適化、3+引数は `setf` chain |
| `define-modify-macro` | ✅ | |
| `defsetf` (短/長形式) | ✅ | FR-355: 短形式 `(defsetf acc upd)` と長形式 `(defsetf acc (args) (store) body)` を `*setf-compound-place-handlers*` に登録 |
| `define-setf-expander` | ✅ | FR-355: `*setf-compound-place-handlers*` に展開関数を登録; `get-setf-expansion` 連携 |
| `get-setf-expansion` | ✅ | FR-355: stdlib-source.lisp で基本実装 (symbol → setq, compound → setf) |

### 4.3 関数呼び出し・多値

| 形式 | 状態 | 備考 |
|------|------|------|
| `apply` / `funcall` | ✅ | |
| `values` / `values-list` | ✅ | |
| `multiple-value-bind` | ✅ | |
| `multiple-value-setq` | ✅ | |
| `(setf (values a b ...) expr)` — values を setf 場所に使用 | ✅ | FR-603: `*setf-compound-place-handlers*` で `multiple-value-bind` + `setq` chain に展開 |
| `multiple-value-list` | ✅ | |
| `nth-value` | ✅ | FR-402: 定数N→`multiple-value-bind`に直接展開; 変数N→`multiple-value-list`+`nth` |
| `multiple-values-limit` 定数 | ✅ | FR-551: `stdlib-source.lisp` で実装 |

### 4.4 等価述語

| 関数 | 状態 | 備考 |
|------|------|------|
| `eq` | ✅ | 同一オブジェクト比較 |
| `eql` | ✅ | `eq` + 数値・文字の値比較 |
| `equal` | ✅ | 構造的等価 (cons tree / string / bit-vector) |
| `equalp` | ✅ | FR-582: `stdlib-source.lisp` で数値・文字・文字列・cons・vector・hash-table 対応 |
| `=` (数値等価) | ✅ | |
| `string=` / `char=` 等 | ✅ | 型固有等価 |

#### FR-582: equalp

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry.lisp`
- **内容**: `equalp` — 数値は `=` で比較、文字・文字列は case-insensitive、vector は要素ごと、hash-table はカウント+キー+値再帰比較
- **根拠**: ANSI CL 5.3 — equalp; `:test #'equalp` のハッシュテーブル (FR-565) に必要
- **難易度**: Medium

### 4.5 `the` の多値・型宣言

| 機能 | 状態 | 備考 |
|------|------|------|
| `(the type form)` | ✅ | |
| `(the (values type...) form)` | ✅ | FR-583: `ast-the` で `(values ...)` 型指定子を透過的に保持; 型チェックは未強制 |
| `(ignore-errors form)` → `(values result nil)` または `(values nil condition)` | ✅ | FR-691: `handler-case` で `(values nil condition)` を正しく返却 |

#### FR-583: the による多値型宣言

- **対象**: `src/compile/codegen.lisp`, `src/parse/ast.lisp`
- **内容**: `(the (values integer float) (floor x y))` のような多値型宣言をコンパイラが認識してレジスタ割り当てを最適化
- **根拠**: ANSI CL 3.4.3 — THE with values
- **難易度**: Medium

### 4.6 順次実行・繰り返し補助

| 形式 | 状態 | 備考 |
|------|------|------|
| `prog1` / `prog2` | ✅ | |
| `prog` / `prog*` | ✅ | |

---

## 5. 繰り返し (ANSI CL Ch.6)

### 5.1 基本繰り返し

| 形式 | 状態 | 備考 |
|------|------|------|
| `do` / `do*` | ✅ | |
| `dolist` / `dotimes` | ✅ | |

### 5.2 LOOP マクロ

`*loop-iter-emitters*` / `*loop-acc-emitters*` データ駆動実装済み (70テスト)。

| 句 | 状態 | 備考 |
|----|------|------|
| `for`/`as :from/:to/:below/:upto/:by` | ✅ | `loop-parser.lisp:118-130` |
| `for`/`as :downfrom/:downto/:above` | ✅ | FR-695: `loop-parser.lisp` + `loop-emitters.lisp` で実装 |
| `for :in` / `for :on` | ✅ | |
| `for :across` | ✅ | |
| `for :hash-keys` / `for :hash-values` | ✅ | |
| `for := ...` (等式初期化) | ✅ | |
| `for x = expr then step` (ステップ付き等式) | ✅ | `loop-emitters.lisp:229` |
| `repeat` | ✅ | |
| `collect` / `sum` / `count` / `maximize` / `minimize` | ✅ | |
| `append` / `nconc` | ✅ | |
| `while` / `until` | ✅ | |
| `when` / `if` / `unless` 句 | ✅ | |
| `always` / `never` / `thereis` | ✅ | |
| `finally` | ✅ | |
| `initially` | ✅ | FR-543: 実装済み |
| `with` | ✅ | |
| `named loop` | ✅ | FR-638: `loop-parser.lisp` + `loop.lisp` で実装 |
| `loop-finish` | ✅ | FR-539: `loop.lisp` で tree-walking 実装 |
| `do` 句 (loop内) | ✅ | |
| `return` 句 (loop内) | ✅ | |
| `for (a b) in list` destructuring | ✅ | `%loop-emit-destructuring` (`loop-emitters.lisp:121`) |

#### FR-543: loop initially 句

- **対象**: `src/expand/loop.lisp`
- **内容**: `initially` 句の本体をループ開始前に実行。`finalize-loop-state` の先頭に追加
- **根拠**: ANSI CL 6.1.7 — loop initially
- **難易度**: Easy

### 5.3 マッピング関数

| 関数 | 状態 | 備考 |
|------|------|------|
| `mapcar` / `mapc` / `mapcan` (多シーケンス) | ✅ | FR-665: expander.lisp で 2-arg inline + 3+ args labels 再帰展開; 多シーケンス `(mapcar #'+ '(1 2) '(3 4))` 対応 |
| `maplist` / `mapl` / `mapcon` | ✅ | FR-360: `stdlib-source.lisp` で実装 |
| `every` / `some` / `notany` / `notevery` (1シーケンス) | ✅ | |
| `every` / `some` 等 多シーケンス並列形式 | ✅ | FR-650: expander.lisp で 3+ args labels 再帰展開; `(every #'< '(1 2) '(3 4))` 対応 |

---

## 6. オブジェクトシステム・CLOS (ANSI CL Ch.7)


### 6.1 定義・生成

| 機能 | 状態 | 備考 |
|------|------|------|
| `defclass` (基本) | ✅ | |
| `:allocation :class` スロット | ✅ | FR-374: パーサー・コードジェン・VM全層実装; クラスHTにスロット値保存、全インスタンスで共有 |
| `:default-initargs` | ✅ | パーサー・コードジェン・VM全層実装; 継承対応; `vm-class-def` に `default-initarg-regs` フィールド |
| `defgeneric` (基本) | ✅ | |
| `defmethod` (primary) | ✅ | |
| `defmethod :before/:after/:around` | ✅ | FR-215: standard method combination 完全実装; `:around` → `:before` → primary → `:after` の順序; `call-next-method` 連携済み |
| `eql` 特化子 | ✅ | FR-359: パーサーで `(param (eql value))` を認識; codegen で quote 展開; VM ディスパッチで eql 一致チェック |
| `make-instance` | ✅ | |
| `find-class` / `(setf find-class)` | ✅ | FR-552: 読み取り + 書き込み両方実装済み |
| `class-name` | ✅ | FR-677: VM命令 `vm-class-name-fn` + ビルトイン登録済み |
| `ensure-class` | ✅ | `:direct-superclasses`/`:direct-slots` を受理し `defclass` に委譲; `macros-compat.lisp` |
| `ensure-generic-function` | ✅ | FR-525: stdlib-source.lisp で基本実装 (名前返却) |

### 6.2 アクセス・変更

| 機能 | 状態 | 備考 |
|------|------|------|
| `slot-value` / `(setf slot-value)` | ✅ | |
| `slot-boundp` | ✅ | `vm-slot-boundp` 命令あり (`vm-clos.lisp:62`) |
| `slot-makunbound` | ✅ | `vm-slot-makunbound` 命令あり (`vm-clos.lisp:70`) |
| `slot-exists-p` | ✅ | `vm-slot-exists-p` 命令あり (`vm-clos.lisp:78`) |
| `slot-missing` プロトコル | ✅ | FR-554: stdlib に `defgeneric slot-missing` + デフォルト `defmethod` 定義; VM エラーに operation 情報追加 |
| `slot-unbound` プロトコル | ✅ | FR-384: stdlib-source.lisp で defgeneric + エラーシグナル defmethod 定義 |
| `with-slots` | ✅ | FR-676: `symbol-macrolet` 展開; `setf` がオブジェクトに書き戻される |
| `with-accessors` | ✅ | FR-701: `symbol-macrolet` 展開; `setf` がアクセサ経由で書き戻される |

#### FR-554: slot-missing プロトコル

- **対象**: `src/vm/vm-clos.lisp`
- **内容**: `slot-missing` GF 呼び出し。デフォルトメソッドが `error`。ユーザーカスタマイズ可能
- **根拠**: ANSI CL 7.5.11 — slot-missing
- **難易度**: Low

### 6.3 ディスパッチ・メソッド

| 機能 | 状態 | 備考 |
|------|------|------|
| `call-next-method` / `next-method-p` | ✅ | 引数なし・引数あり両方動作; `vm-cnm-args-reg` で再ディスパッチ (`vm-execute.lisp:280-300`) |
| `initialize-instance` GF | ✅ | FR-379: stdlib-source.lisp で defgeneric + 基本 defmethod 定義 |
| `allocate-instance` GF | ✅ | FR-524: stdlib-source.lisp で defgeneric + 基本 defmethod 定義 |
| `change-class` | ✅ | FR-380: スロット移行 (旧スロット削除・新スロット追加・共通スロット保持) + `update-instance-for-different-class` 呼出 |
| `update-instance-for-different-class` GF | ✅ | FR-1005: `defgeneric`+`defmethod` を `stdlib-source.lisp` に定義; デフォルトは `reinitialize-instance` 委譲 (ANSI 準拠) |
| `update-instance-for-changed-class` GF | ✅ | FR-1005: `defgeneric`+`defmethod` を `stdlib-source.lisp` に定義; デフォルトは `reinitialize-instance` 委譲 (ANSI 準拠) |
| `class-of` | ✅ | FR-385: VM命令 `vm-class-of-fn` + ビルトイン登録済み |
| `compute-applicable-methods` | ✅ | FR-377: stdlib-source.lisp で defgeneric + 基本 defmethod (nil 返却) 定義 |
| `no-applicable-method` GF | ✅ | FR-377: stdlib-source.lisp で defgeneric + エラーシグナル defmethod 定義 |
| `find-method` / `add-method` / `remove-method` | ✅ | FR-377: 三つとも `defgeneric`+`defmethod` 定義; `find-method` は GF ハッシュテーブル検索; `add-method`/`remove-method` はメソッド追加/削除 |
| Method Combination (`:before/:after/:around`) | ✅ | FR-215: standard method combination 完全実装 (`:around`/`:before`/`:after` + `call-next-method`); `define-method-combination` 短形式も実装済み |
| C3 線形化 CPL | ✅ | FR-378: `%c3-merge` + `compute-class-precedence-list` を C3 線形化アルゴリズムで実装; ダイアモンド継承・矛盾検出対応 |
| MOP イントロスペクション | ✅ | FR-523〜528: `class-direct-superclasses`, `class-direct-slots`, `class-slots`, `class-direct-default-initargs`, `class-precedence-list` をマクロ化 (macros-compat.lisp); `vm-hash-table-get-internal`/`vm-hash-table-p` がネイティブCLハッシュテーブルも受理するよう修正; `class-name`/`class-of` は既存VM命令 |
| `no-next-method` GF | ✅ | FR-584: stdlib に `defgeneric no-next-method` + デフォルト `defmethod` 定義; `slot-unbound` と同パターン |
| `invalid-method-error` | ✅ | FR-584 |
| `method-combination-error` | ✅ | FR-584 |
| `print-object` GF | ✅ | FR-390: `defgeneric`+`defmethod` を `stdlib-source.lisp` に定義; GF ディスパッチ経由で `prin1` 委譲 |
| `describe-object` GF | ✅ | CLOS インスタンスのクラス名・スロット出力; 非 CLOS は `~S` フォールバック; `macros-compat.lisp` |

#### FR-584: no-next-method / invalid-method-error / method-combination-error

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-clos.lisp`
- **内容**: `no-next-method gf method args` — カスタマイズ可能エラーGF。`invalid-method-error method format-string args` / `method-combination-error format-string args` もシグナリング関数として実装
- **根拠**: ANSI CL 7.6.6.1 — no-next-method
- **難易度**: Low

---

# CLOS & MOP — 2026年モダンコンパイラ機能リファレンス


---

## 7. コアオブジェクトモデル (ANSI CL Chapter 4, 7)

### 7.1 クラス定義 — `defclass`

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| 基本スロット定義 | ✅ | ✅ | `:reader` `:writer` `:accessor` `:initarg` `:initform` |
| `:allocation :instance` | ✅ | ✅ | デフォルト — インスタンスごとのスロット |
| `:allocation :class` | ✅ FR-374 | ✅ | クラス共有スロット; `vm-class-slots` フィールド + class HT redirect |
| クラスオプション `:default-initargs` | ✅ FR-375 | ✅ | `shared-initialize` で処理 |
| 多重継承 | ✅ | ✅ | 複数スーパークラス指定可能 |
| 自動 `standard-object` 継承 | ✅ FR-528 | ✅ | スーパークラス省略時の暗黙継承 |

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

| アルゴリズム | cl-cc | SBCL | 備考 |
|------------|-------|------|------|
| **C3 線形化** | ✅ FR-378 | ✅ | Barrett et al. 1996。`%c3-merge` + `compute-class-precedence-list` で実装 |
| ダイアモンド継承の正確なCPL | ✅ | ✅ | C3 で重複排除済み |
| CPL 矛盾検出 (エラー) | ✅ | ✅ | `%c3-merge` が矛盾時に `error` をシグナル |

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

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| 基本定義 | ✅ | ✅ | ディスパッチHTを生成 |
| `:method-combination` オプション | ✅ FR-382 | ✅ | `standard` / `+` / `append` 等 |
| インラインメソッド `(:method ...)` | ✅ FR-382 | ✅ | `defgeneric` 内に直接 `defmethod` |
| `ensure-generic-function` (公開) | ✅ FR-525 | ✅ | `%ensure-generic-function` は内部のみ |
| GF 再定義セマンティクス | ✅ | ✅ | 既存GFに `defgeneric` を再実行すると既存メソッドは保持 (`ctx-global-generics` 確認)。lambda-list 不一致は compile-time `warn` (`ctx-global-generic-params` に param 数を保存して比較) |

### 8.2 `defmethod` と特化子 (Specializers)

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| クラス特化子 `(param ClassName)` | ✅ | ✅ | 基本ディスパッチ |
| T (全型) 特化子 | ✅ | ✅ | フォールバックメソッド |
| **EQL 特化子** `(param (eql value))` | ✅ FR-124 | ✅ | `(eql :keyword)` や `(eql 42)` 等; codegen で quote 展開、VM で eql 一致 |
| 多重ディスパッチ | ✅ | ✅ | 複合キー `(list class1 class2 ...)` |
| 継承ベースのフォールバック | ✅ | ✅ | CPL を辿って最も特化したメソッドを選択 |
| `find-method` | ✅ FR-377 | ✅ | `(find-method gf qualifiers specializers)` |
| `add-method` / `remove-method` | ✅ FR-377 | ✅ | 実行時メソッド追加・削除 |
| `compute-applicable-methods` | ✅ FR-377 | ✅ | ユーザーから呼び出し可能 |
| `no-applicable-method` | ✅ FR-377 | ✅ | GF として定義。デフォルトは `error` |
| `no-next-method` | ✅ | ✅ | `call-next-method` で次がない場合 |
| `function-keywords` | ✅ | ✅ | `(function-keywords method)` → `(values keyword-list allow-other-keys-p)` |
| メソッド再定義セマンティクス (ANSI CL 7.6.3) | ✅ | ✅ | 同一 name/qualifier/specializer の `defmethod` は既存メソッドを置換 |

**`function-keywords` の使い方:**
```lisp
(defmethod f ((x integer) &key verbose debug) ...)
(function-keywords (find-method #'f '() (list (find-class 'integer))))
;; → (values '(:verbose :debug) nil)   ; nil = &allow-other-keys なし
```

### 8.3 メソッド修飾子 (Method Qualifiers)

| 修飾子 | cl-cc | SBCL | 備考 |
|--------|-------|------|------|
| `primary` (修飾子なし) | ✅ | ✅ | 現在のディスパッチで動作 |
| `:before` | ✅ FR-215 | ✅ | primary の前に全 `:before` を実行 |
| `:after` | ✅ FR-215 | ✅ | primary の後に全 `:after` を逆順実行 |
| `:around` | ✅ FR-215 | ✅ | 全体をラップ。`call-next-method` で連鎖 |

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

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `call-next-method` 引数なし (現引数を引き継ぐ) | ✅ | ✅ | `vm-method-call-stack` で追跡。CPL 順に次メソッドへ |
| `call-next-method new-arg1 new-arg2` (引数変更) | ✅ | ✅ | 新しい引数で次メソッドを呼び出す。`call-next-method` 連鎖内で引数のみ置き換える |
| `call-next-method` with `:around` qualifier | ✅ FR-215 | ✅ | around の連鎖。primary chain とは別のスタック |
| `next-method-p` | ✅ | ✅ | 次のメソッドが存在するか確認。`nil` なら `call-next-method` はエラー |
| `no-next-method` GF | ✅ | ✅ | 次メソッドがない状態で `call-next-method` → この GF を呼ぶ |
| メソッド外での `call-next-method` | ✅ | ✅ エラー | `vm-execute.lisp` で明示的にエラーをシグナル |

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

| ルール | cl-cc | SBCL | 説明 |
|--------|-------|------|------|
| 特化子は必須引数のみ | ✅ | ✅ | `&optional`/`&rest`/`&key` 引数への特化子は不可 |
| `&key` キー名の一致 | — | ✅ 警告 | SBCL は不整合 key を警告。ANSI は `&allow-other-keys` で回避 |
| GF なしでの `defmethod` | ✅ | ✅ | GF を自動生成 (cl-cc は `%ensure-generic-function`) |

```lisp
;; 適合エラーの例
(defgeneric f (a b &optional c))
(defmethod f ((a integer) b) ...)  ; エラー: &optional が欠落
(defmethod f ((a integer) b &optional c &key d) ...)  ; &key は追加可 (ANSI では許容)
```

---

## 9. メソッドコンビネーション (ANSI CL 7.6.6)

### 9.0 メソッドコンビネーション内のエラーシグナル

`define-method-combination` の長形式ボディ内でのみ使える特殊なエラー関数。

| 関数 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

```lisp
;; 使用例: my-around qualifier しか許可しないコンビネーション
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

| 種類 | cl-cc | SBCL | 説明 |
|------|-------|------|------|
| `standard` | ✅ | ✅ | `:before`/:after`/`:around` + primary |
| `+` | ✅ FR-376 | ✅ | 全メソッドの戻り値を `+` で集約 |
| `append` | ✅ FR-376 | ✅ | リスト結果を `append` で結合 |
| `list` | ✅ FR-376 | ✅ | 結果をリストに収集 |
| `nconc` | ✅ FR-376 | ✅ | `nconc` で結合 |
| `progn` | ✅ FR-376 | ✅ | 最後の値を返す |
| `and` | ✅ FR-376 | ✅ | 全て真なら最後の値 |
| `or` | ✅ FR-376 | ✅ | 最初の真の値 |
| `max` / `min` | ✅ FR-376 | ✅ | 数値結果の最大・最小 |

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

### 9.3 `method-combination` MOP クラス

`define-method-combination` が定義したコンビネーション型は `method-combination` クラスのインスタンスとして表現される。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

```lisp
;; SBCL での確認
(generic-function-method-combination #'print-object)
;; → #<SB-PCL::STANDARD-METHOD-COMBINATION STANDARD {}>
(class-of *)
;; → #<SB-PCL::STANDARD-METHOD-COMBINATION>
```

### 9.4 (旧 3.2) `define-method-combination`

#### 短形式

```lisp
;; (define-method-combination name :operator op :identity-with-one-argument bool)
(define-method-combination +   :operator +   :identity-with-one-argument t)
(define-method-combination max :operator max :identity-with-one-argument t)
```

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `:operator` — 結合演算子 | ✅ FR-376 | ✅ | 全 primary メソッドの結果をこれで fold |
| `:identity-with-one-argument` | ✅ FR-376 | ✅ | `t` のとき単一メソッドで演算子をスキップ |

#### 長形式

```lisp
(define-method-combination progn ()
  ((methods ()))   ; method-group: qualifiers = ()
  `(progn ,@(mapcar #'(lambda (m) `(call-method ,m)) methods)))
```

| 要素 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

**長形式の完全例:**
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

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `make-instance` | ✅ | ✅ | 静的クラス名 + 動的クラス変数 両対応 |
| `allocate-instance` GF | ✅ FR-524 | ✅ | メモリ割り当てのカスタマイズポイント |
| `initialize-instance` GF | ✅ FR-379 | ✅ | `:after` でカスタム初期化が可能に |
| `shared-initialize` GF | ✅ | ✅ | `initialize-instance` / `reinitialize-instance` の共通ロジック |
| `reinitialize-instance` | ✅ | ✅ | 既存インスタンスのスロット再初期化 |
| `:default-initargs` 処理 | ✅ FR-375 | ✅ | クラスオプションの `:default-initargs` を `shared-initialize` で適用 |

### 10.2 スロットアクセス

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `slot-value` / `(setf slot-value)` | ✅ | ✅ | `vm-slot-read` / `vm-slot-write` 命令 |
| `slot-boundp` | ✅ | ✅ | `vm-slot-boundp` 命令 |
| `slot-makunbound` | ✅ | ✅ | `vm-slot-makunbound` 命令 |
| `slot-exists-p` | ✅ | ✅ | `vm-slot-exists-p` 命令 |
| `slot-unbound` GF | ✅ FR-384 | ✅ | 未束縛アクセス時のカスタマイズ。現在は直接 `error` |
| `slot-missing` GF | ✅ | ✅ | **スロット名自体が存在しない**場合。`slot-unbound` とは別 (operation引数: `:slot-value`/`:setf`/`:slot-boundp`/`:slot-makunbound`) |
| `:reader` / `:writer` / `:accessor` | ✅ | ✅ | `compile-slot-accessor` で closure 生成 |
| accessor read inlining | ✅ FR-120 | ✅ | `compiler-macroexpand-all` で `(accessor obj)` → `(slot-value obj 'slot)` に展開 |
| `with-slots` マクロ | ✅ | ✅ | `macros-stdlib.lisp` 実装済み |
| `with-accessors` マクロ | ✅ | ✅ | `macros-stdlib.lisp` 実装済み |

### 10.3 クラス変更プロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `change-class` | ✅ FR-380 | ✅ | `macros-sequence.lisp:527` — スロット移行・`update-instance-for-different-class` 呼び出しあり |
| `update-instance-for-different-class` GF | ✅ FR-380 | ✅ | `change-class` プロトコルの一部 |
| `make-instances-obsolete` | ✅ FR-523 | ✅ | クラス再定義時の既存インスタンス無効化 |
| `update-instance-for-redefined-class` GF | ✅ FR-523 | ✅ | 遅延マイグレーション。スロットアクセス時に自動起動 |

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

| 型 | cl-cc | 備考 |
|----|-------|------|
| `integer` (fixnum / bignum) | ✅ | 現行 |
| `string` | ✅ | 現行 |
| `symbol` (null を含む) | ✅ | 現行 |
| CLOS インスタンス | ✅ | `:__class__` で判別 |

### 11.2 `class-of` / `type-of` / `typep`

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `class-of` | ✅ FR-385 | ✅ | VM命令あり。`:__class__` から取得する |
| `type-of` | ✅ FR-385 | ✅ | 標準型 + CLOSクラス両方返却 |
| `typep` (クラス名) | ✅ FR-385 | ✅ | `(typep obj 'my-class)` — CPL トラバーサルが必要 |
| **`typep` (クラスオブジェクト)** | ✅ | ✅ | `(typep obj (find-class 'my-class))` — ANSI CL 4.3.7。クラスオブジェクトを直接渡せる |
| `subtypep` (クラス間) | ✅ | ✅ | `(subtypep 'child 'parent)` → T。CPL を使って判定 |
| クラス型としての型指定子 | ✅ | ✅ | クラス名はそのまま型指定子として使える。`(declare (type my-class x))` 等 |

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

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `class-name` | ✅ FR-527 | ✅ | `:__name__` の公開 |
| `class-direct-superclasses` | ✅ FR-527 | ✅ | `:__superclasses__` の公開 |
| `class-precedence-list` | ✅ FR-527 | ✅ | `:__cpl__` の公開 |
| `class-direct-slots` | ✅ FR-527 | ✅ | 直接定義スロットのみ |
| `class-slots` | ✅ | ✅ | 継承含む全スロット |

### 12.2 スロット定義イントロスペクション

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

### 12.2b Effective Slot Definition マージ規則 (ANSI CL 7.5.3)

多重継承で複数のスーパークラスが同名スロットを定義した場合、`compute-effective-slot-definition` が結合する。

| 項目 | マージ規則 | cl-cc | SBCL |
|------|-----------|-------|------|

```lisp
;; 例: :initargs の和集合
(defclass A () ((x :initarg :a-x)))
(defclass B () ((x :initarg :b-x)))
(defclass C (A B) ())
;; C のスロット x には :a-x と :b-x 両方が有効
(make-instance 'C :a-x 1)  ; OK
(make-instance 'C :b-x 1)  ; OK (SBCL)
```

### 12.3 メタクラスプロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

### 12.4 ジェネリック関数 MOP

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

---


| 挙動 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| 循環継承 | — | ✅ エラー | `(defclass A (B)) (defclass B (A))` → CPL 矛盾 |

---

## 12c. initarg 検証プロトコル (ANSI CL 7.1.2)

`make-instance` に渡す initarg が有効か検証される。無効な場合 `error`。

**有効な initarg の条件 (いずれか):**

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

```lisp
;; SBCL での挙動
(defclass foo () ((x :initarg :x)))
(make-instance 'foo :y 1)  ; ERROR: invalid initarg :y
(make-instance 'foo :y 1 :allow-other-keys t)  ; OK: 検証スキップ
```

---

## 13. 最適化 (Optimization)

### 13.1 ディスパッチキャッシュ

| 最適化 | cl-cc | SBCL | 備考 |
|--------|-------|------|------|

### 13.2 インスタンス表現

| 最適化 | cl-cc | SBCL | 備考 |
|--------|-------|------|------|
| **現行: ハッシュテーブル** | ✅ | — | `gethash` O(1) average, memory heavy |
| SBCL の `standard-instance` | — | ✅ | `(instance header slots)` の3ワードレイアウト |

### 13.3 静的最適化

| 最適化 | cl-cc | SBCL | 備考 |
|--------|-------|------|------|
| SBCL `deftransform` (メソッド特化変換) | — | ✅ | コンパイラマクロ的なメソッドインライン化 |

---

## 14. `defstruct` との統合

cl-cc は `defstruct` を `defclass` への変換として実装 (`src/expand/expander-defstruct.lisp`)。
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

| オプション | cl-cc | SBCL | 備考 |
|-----------|-------|------|------|
| `:conc-name` | ✅ | ✅ | アクセサ名のプレフィックス。`nil` で無プレフィックス |
| `:constructor name` | ✅ | ✅ | コンストラクタ名の変更 |
| `:constructor name boa-list` | ✅ | ✅ | BOA (By-Order-of-Arguments) ラムダリスト |
| `:include parent` | ✅ | ✅ | 親構造体の継承。`*defstruct-slot-registry*` で追跡 |
| `:predicate name` | ✅ | ✅ | 述語関数名の変更または `nil` で抑制 |
| `:copier name` | ✅ | ✅ | `copy-NAME` 関数の生成 |
| `:print-function` / `:print-object` | ✅ | ✅ | `print-object` メソッドの自動生成 |
| `:type vector` / `:type list` | ✅ | ✅ | CLOS インスタンスでなくベクタ/リストで格納 |
| `:named` | ✅ | ✅ | `:type` 構造体に型タグを付加 |
| 複数 `:constructor` | ✅ | ✅ | 複数コンストラクタ同時定義 |

### 14.2 SBCL `structure-class` との差異

| 項目 | cl-cc | SBCL |
|------|-------|------|
| 内部表現 | `defclass` (ハッシュテーブル) | `sb-kernel:structure-object` (固定サイズベクタ) |
| `copy-structure` | ✅ | ✅ |
| `structure-object` 組み込みクラス | ✅ FR-528 | ✅ |
| CLOS メソッド定義 (構造体への `defmethod`) | ✅ | ✅ | `expander-defstruct.lisp` で `structure-object` を自動スーパークラスとして追加 (`:include` なし時); CPL に `structure-object` が入る |

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

| GF | cl-cc | SBCL | 備考 |
|----|-------|------|------|
| `make-instance` | ✅ (macro展開) | ✅ GF | cl-cc は `vm-make-obj` 命令。GF ではない |
| `allocate-instance` | ✅ FR-524 | ✅ GF | メモリ割り当てのカスタマイズ。cl-cc は `vm-make-obj` 内に実装 |
| `initialize-instance` | ✅ FR-379 | ✅ GF | ユーザーが `:after` で初期化を追加できる |
| `shared-initialize` | ✅ | ✅ GF | `initialize-instance` と `reinitialize-instance` の共通ロジック |
| `reinitialize-instance` | ✅ | ✅ GF | スロットへの initarg 適用を含む |
| `apply-default-initargs` (内部) | ✅ | ✅ | クラスの `:default-initargs` を initarg リストにマージ |
| `update-instance-for-different-class` | ✅ FR-380 | ✅ GF | `reinitialize-instance` に委譲する既定動作を持つ |

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

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `ensure-class name &key ...` | ✅ | ✅ | キーワード: `:direct-superclasses` `:direct-slots` `:metaclass` `:name` 等 |

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

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `define-condition` 基本定義 | ✅ | ✅ | `conditions.lisp` で実用中 |
| `:report lambda` | ✅ | ✅ | `(lambda (condition stream) ...)` 形式 |
| `make-condition` | ✅ | ✅ | `make-instance` に委譲する実装 |
| `condition-report` (内部) | ✅ | ✅ | `format`/`print` が `print-object` 経由で呼ぶ |
| `:default-initargs` on conditions | ✅ | ✅ | |

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

| 関数 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `signal` / `error` / `warn` / `cerror` | ✅ | ✅ | VM 命令として実装 |
| `handler-bind` / `handler-case` | ✅ | ✅ | VM 命令として実装 |
| `restart-case` / `invoke-restart` | ✅ | ✅ | `invoke-restart` と `restart-case` の基本プロトコルを実装 |
| `compute-restarts` | ✅ | ✅ | 現在の再起動リストを返す |
| `find-restart` | ✅ | ✅ | 名前で再起動を検索 |
| `condition-report` (内部 GF) | ✅ | ✅ | `:report` を呼ぶ |
| `print-object` for conditions | ✅ | ✅ | `#<TYPE-ERROR ...>` 形式 |

---

## 18. `print-object` と出力プロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `print-object` GF | ✅ | ✅ | オブジェクトの表示カスタマイズ |
| `print-unreadable-object` マクロ | ✅ | ✅ | `#<...>` 形式の出力 |
| `with-standard-io-syntax` 下での挙動 | ✅ | ✅ | print 制御変数をリセットし、`print-object` も通常どおり動作する |

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

## 実装優先度マトリクス

| 優先度 | FR番号 | 機能 | 効果 | 難易度 |
|--------|--------|------|------|--------|
| **P0** | FR-383 | qualifier AST 保持 | FR-215 の前提条件 | Easy |
| **P0** | FR-381 | `vm-classify-arg` 全型対応 | 型安全なディスパッチ | Easy |
| **P1** | FR-215 | `:before`/`:after`/`:around` | 標準メソッドコンビネーション | Hard |
| **P1** | FR-119 | Multiple dispatch memoization | 最高ROI の性能改善 | Easy |
| **P1** | FR-120 | Accessor read inlining | `*accessor-slot-map*` 利用 | Easy |
| **P1** | FR-379 | `initialize-instance` GF | `:after` 初期化が使える | Hard |
| **P2** | FR-121 | Slot vector 表現 | スロットアクセス 10× | Hard |
| **P2** | FR-374 | `:allocation :class` | 共有スロット | Medium |
| **P2** | FR-375 | `defclass` クラスオプション | `:default-initargs` 等 | Medium |
| **P2** | FR-124 | EQL 特化子 | ディスパッチ完全性 | Medium |
| **P2** | FR-377 | `compute-applicable-methods` 公開 | MOP 基盤 | Medium |
| **P3** | FR-376 | `define-method-combination` | 高度なメソッドコンビネーション | Hard |
| **P3** | FR-523 | `make-instances-obsolete` | クラス再定義プロトコル | Very Hard |
| **P3** | FR-214 | Hidden class / Shape | V8スタイル最適化 | Very Hard |

---

---

## 22. MOP — Specializer プロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `(setf find-class)` | ✅ | ✅ | クラスの登録・置換。`defclass` の内部実装がこれを呼ぶ |

**cl-cc の内部レジストリ:**
```lisp
;; vm-state 内の vm-class-registry は HT (symbol → class-HT)
;; defclass コンパイル時に codegen-clos.lisp:39 で登録
(setf (gethash name (ctx-global-classes ctx)) dst)
```

---

## 23. MOP — `slot-value-using-class` プロトコル

既存クラスを `defclass` で再定義した場合の動作。SBCL は遅延マイグレーション (lazy migration) を採用。

| 段階 | cl-cc | SBCL | 説明 |
|------|-------|------|------|
| `make-instances-obsolete` 自動呼び出し | ✅ FR-523 | ✅ | 既存インスタンスを「陳腐化」マーク |
| 遅延スロットマイグレーション | ✅ FR-523 | ✅ | 次回スロットアクセス時に自動的に `update-instance-for-redefined-class` を呼ぶ |
| `update-instance-for-redefined-class` GF | ✅ FR-523 | ✅ | 追加スロット初期化・削除スロット後処理 |
| スロット追加時のデフォルト初期化 | ✅ | ✅ | 新スロットに initform を適用 |
| スロット削除時の廃棄済みスロット値 | ✅ | ✅ | `discarded-slots` 引数でアクセス可能 (移行中のみ) |

**SBCL の実装方式:**
```
インスタンスの先頭ワード = "wrapper" ポインタ
wrapper が無効化 (invalidated) されたら → 次のスロットアクセス時に trap → migrate
```

---

## 24. MOP — Method クラスプロトコル

CLOS の汎関数自体が CLOS オブジェクト (GF は `funcallable-standard-class` のインスタンス) である。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

**用途の例:**
```lisp
;; カスタムディスパッチャを持つ呼び出し可能オブジェクト
(defclass my-callable ()
  ((fn :initarg :fn))
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((obj my-callable) &key)
  (set-funcallable-instance-function
   obj (lambda (&rest args) (apply (slot-value obj 'fn) args))))
```

---

## 25. `describe-object` / `documentation` プロトコル

最も強力なMOPフック。スロットアクセスをメタクラスでインターセプトできる。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

**用途:** 永続化・遅延ロード・代理 (proxy) パターン:
```lisp
;; スロット読み取り時にDBから自動ロードするメタクラス
(defmethod slot-value-using-class ((class persistent-class) obj slot-def)
  (or (call-next-method)
      (load-from-db (slot-definition-name slot-def) (object-id obj))))
```

---

## 27. SBCL 固有拡張 (2026年現在)

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

---

### 27.1 `sb-mop` パッケージ

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `make-load-form object` GF | ✅ | ✅ | FASL 埋め込み用の汎用 GF。デフォルトは primitive literal を返す |
| `make-load-form-saving-slots` | ✅ | ✅ | `hash-table` の `:__class__` メタデータからクラス名を復元する実装 |


**使用例:**
```lisp
(defclass point ()
  ((x :initarg :x) (y :initarg :y)))

(defmethod make-load-form ((p point) &optional env)
  (make-load-form-saving-slots p :slot-names '(x y) :environment env))
```

---

### 27.2 Sealed Classes (SBCL 2.3+)

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `describe-object object stream` GF | ✅ | ✅ | `describe` の内部実装。デフォルトはスロット一覧表示 |
| `doc-type` の種類 | — | — | `t` / `'function` / `'variable` / `'type` / `'structure` |

---

### 27.3 Satiated Generic Functions (SBCL)

### 27.4 `closer-mop` (ポータビリティ層)

SBCL は ANSI CL にない MOP 機能を `sb-mop` で提供 (`closer-mop` 経由が推奨)。

| 機能 | SBCL | 備考 |
|------|------|------|
| `sb-mop:class-direct-slots` | ✅ | direct slot definitions のリスト |
| `sb-mop:class-slots` | ✅ | effective slot definitions のリスト |
| `sb-mop:slot-definition-location` | ✅ | スロットのメモリ位置 (インデックス) |
| `sb-mop:funcallable-standard-class` | ✅ | |
| `sb-mop:set-funcallable-instance-function` | ✅ | |

## 28. `&key` とジェネリック関数 (ANSI CL 7.6.5)

```lisp
;; クラスの継承・メソッド追加を封じて静的最適化を許可
(sb-ext:define-sealed-type point)
;; → コンパイラはディスパッチを静的解決できる
```

| 機能 | SBCL | cl-cc | 備考 |
|------|------|-------|------|

### 28.1 `&key` の合法性規則

```lisp
;; 全ディスパッチパスを事前展開してキャッシュ
(sb-pcl::satiating-gfs-p *my-gf*)
```

| 機能 | SBCL | cl-cc | 備考 |
|------|------|-------|------|

### 28.2 `&rest` + `&key` の組み合わせ

SBCL/CCL/ABCL/ECL の MOP 差異を吸収するライブラリ。

| 機能 | 備考 |
|------|------|------|
| `closer-mop:class-direct-slots` | 実装間の API 統一 |
| `closer-mop:ensure-finalized` | `finalize-inheritance` の portable ラッパー |
| `closer-mop:slot-definition-*` | 全実装で同一 API |

---

## 29. ジェネリック関数の First-Class 利用

`&key` を使うメソッドが混在する場合の規則は通常の関数より複雑。

## 30. CLOS と多値・型宣言・その他の連携

| ルール | cl-cc | SBCL | 説明 |
|--------|-------|------|------|

**重要な特別規則 (ANSI CL 7.6.5):**
```lisp
(defgeneric f (a &key))
(defmethod f ((a integer) &key x) ...)   ; :x を知る
(defmethod f ((a string)  &key y) ...)   ; :y を知る

;; 呼び出し側: (f 42 :x 1 :y 2) — 両方合法
;; effective method に含まれる全メソッドのキー名の和集合が許容される
;; → integer メソッドだけが適用されても :y は合法 (暗黙の &allow-other-keys)
```

### 30.1 多値 (`values`) とメソッド

```lisp
;; &rest と &key を組み合わせる慣用形
(defmethod process ((x t) &rest args &key verbose &allow-other-keys)
  (when verbose (format t "Processing ~S~%" x))
  (apply #'call-next-method x args))  ; 全キーワードを次メソッドに転送
```

| パターン | cl-cc | SBCL | 備考 |
|---------|-------|------|------|

---

### 30.2 型宣言とメソッド

GF は通常の関数と同じく first-class 値として使える。

**cl-cc での判定 (`vm.lisp:443`):**
```lisp
(defun vm-generic-function-p (value)
  ;; :__methods__ キーを持つ HT = GF と判定
  (and (hash-table-p value) (gethash :__methods__ value) t))
;; %vm-dispatch-call (vm-execute.lisp:145) が呼び出し前にこれで分岐
```

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `#'my-generic-fn` で GF を値として取得 | ✅ | ✅ | `vm-generic-function-p` で識別 |
| `(funcall #'my-gf arg)` | ✅ | ✅ | `%vm-dispatch-call` が GF ディスパッチに分岐 |
| `(apply #'my-gf list)` | ✅ | ✅ | `vm-execute.lisp:505` で対応 |

**`standard-generic-function` クラス:**
```
funcallable-standard-object  (MOP)
└── generic-function         (ANSI CL abstract)
    └── standard-generic-function  (ANSI CL concrete)
        ← defgeneric が生成するオブジェクトのクラス (SBCL)
```

| MOP アクセサ | cl-cc | SBCL | 備考 |
|-------------|-------|------|------|

---

### 30.3 オブジェクト同一性・等価性

### 30.4 CLOS と `copy`

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| メソッドから `(values a b)` を返す | ✅ | ✅ | 通常の `vm-ret` が多値を伝播 |
| `(nth-value 0 (my-gf x))` | ✅ | ✅ | |
| `(multiple-value-bind (a b) (my-gf x) ...)` | ✅ | ✅ | |

## 31. CLOS と `setf` の統合

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|

### 31.1 自動生成される `setf` 展開

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `eq` — 同一インスタンス判定 | ✅ | ✅ | HT の `eq` 比較 |
| `eql` — CLOS インスタンスでは `eq` と同じ | ✅ | ✅ | CLOS インスタンスは数値でないので `eql` = `eq` |
| `equal` / `equalp` — デフォルトは `eq` | ✅ | ✅ | ユーザーが `defmethod` で override しない限り同一性 |

### 31.2 `with-slots` / `with-accessors` の展開

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `copy-structure` (for `defstruct`) | ✅ | ✅ | `defstruct` が自動生成 |
| CLOS インスタンスの `copy` (非標準) | — | — | ANSI CL に `copy-instance` はない |
| `(setf (slot-value copy 'x) ...)` 手動コピー | ✅ | ✅ | 標準的な方法 |

---

### 31.3 `setf` の5値プロトコル (`get-setf-expansion`)

`defclass` の `:writer` / `:accessor` は `setf` 展開を自動定義する。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `(setf (slot-value obj 'x) v)` | ✅ | ✅ | `vm-slot-write` 命令。expander.lisp で展開 |
| `(setf (accessor obj) v)` — `:accessor` 由来 | ✅ | ✅ | `expand-setf-accessor` で展開済み |
| `(writer v obj)` — `:writer` 形式 | ✅ | ✅ | writer は `(setf reader)` ではなく専用関数 |
| `defsetf` でのカスタム `setf` 定義 | ✅ | ✅ | 短形式・長形式とも対応 |
| `define-setf-expander` | ✅ | ✅ | `get-setf-expansion` が返す5値を制御 |
| `get-setf-expansion` | ✅ | ✅ | 任意の場所の setf 展開を返す |

**cl-cc の現実装 (`expander.lisp`):**
```lisp
;; *accessor-slot-map* = HT (accessor-sym → (class-name . slot-name))
;; (setf (point-x p) 42) → expand-setf-accessor → vm-slot-write
```

`with-slots` は `symbol-macrolet` を使ってスロットアクセスをローカルシンボルにバインドする。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `with-slots (slot ...) obj body` | ✅ | ✅ | `macros-stdlib.lisp` 実装済み |
| `with-slots` の `symbol-macrolet` 展開 | ✅ | ✅ | `(slot-value obj 'slot)` にシンボルマクロ展開 |
| `(with-slots (x y) obj (setf x 1))` | ✅ | ✅ | `setf` も `(setf (slot-value obj 'x) 1)` に展開 |
| `with-accessors (var accessor ...) obj body` | ✅ | ✅ | `(accessor obj)` にシンボルマクロ展開 |
| `with-slots` でのネスト | ✅ | ✅ | 複数オブジェクトを同時バインド不可 (単一形式) |

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
- **AMOP**: *The Art of the Metaobject Protocol* (Kiczales et al., 1991)
- **SBCL Internals**: `src/pcl/` — PCL (Portable Common Loops) をベースに SBCL が拡張
- **CCL CLOS**: `level-1/l1-clos-*.lisp`
- **C3 線形化**: Barrett et al., "A Monotonic Superclass Linearization for Dylan" (OOPSLA 1996)
- **closer-mop**: https://github.com/pcostanza/closer-mop — MOP ポータビリティ層
- **cl-cc 実装**: `src/vm/vm-clos.lisp`, `src/compile/codegen-clos.lisp`, `src/vm/vm-execute.lisp:383-457`

---

## 32. 構造体 (ANSI CL Ch.8)

| 機能 | 状態 | 備考 |
|------|------|------|
| `defstruct` (基本: コンストラクタ・述語・アクセサ) | ✅ | `expander-defstruct.lisp` |
| `:constructor` オプション (名前/BOA) | ✅ | カスタム名・BOA (`&aux` 含む) 完全対応 (`expander-defstruct.lisp:15-62`) |
| `:copier` / `:predicate` オプション | ✅ | FR-544: `(:predicate nil)` / `(:constructor nil)` で抑制対応 |
| `:print-function` / `:print-object` | ✅ | FR-544: `expander-defstruct.lisp` でオプション解析; `defmethod print-object` を自動生成 |
| `:include` 継承 | ✅ | FR-545: 子 defclass に `own-slots` のみ出力; 親スロットは CLOS 継承経由; アクセサ登録も own-slots のみ |
| `:type` オプション (list/vector) | ✅ | FR-546: `(:type list)` → リスト表現 (car=型タグ, cdr=スロット値); `(:type vector)` → ベクタ表現 (aref 0=型タグ); コンストラクタ・アクセサ・述語を自動生成; `:conc-name`/`:include`/`:constructor`/`:predicate` 対応 |
| `:conc-name` オプション | ✅ | `expander-defstruct.lisp:79` |
| スロットオプション `:read-only` | ✅ | `:read-only t` で `:reader` のみ生成 (setfアクセサなし); `expander-defstruct.lisp` |
| スロットオプション `:type` | ✅ | パーサーで受理; 実行時型チェックなし (ANSI では enforcement は実装定義) |
| `copy-structure` | ✅ | FR-555: `copy-hash-table` に委譲 (VM structs are hash-tables with :__class__) |
| `structure-object` 型 (CLOS 統合) | ✅ | FR-528: stdlib-source.lisp で空 defclass 定義 |

#### FR-544: defstruct コンストラクタ・コピー・述語オプション

- **対象**: `src/expand/expander-defstruct.lisp`
- **内容**: ANSI CL 8.1.5〜8.1.7 の defstruct オプション処理
- **難易度**: Medium

#### FR-545: defstruct :include 継承 — 子スロットアクセサ生成バグ

- **対象**: `src/expand/expander-defstruct.lisp`
- **内容**: `:include` 継承の構造体定義を完全にサポートする
- **根拠**: ANSI CL 8.1.5.5
- **難易度**: Low

#### FR-546: defstruct :type / :conc-name / スロットオプション

- **対象**: `src/expand/expander-defstruct.lisp`
- **内容**: `:type list`/`:type vector`、`:conc-name`、スロット `:type`/`:read-only`
- **根拠**: ANSI CL 8.1.5
- **難易度**: Medium

#### FR-555: copy-structure

- **対象**: `src/expand/expander-defstruct.lisp`
- **内容**: `(copy-structure structure-object)` — 全スロットをシャローコピーした新インスタンスを返す
- **根拠**: ANSI CL 8.1.6 — copy-structure
- **難易度**: Easy

#### FR-556: #S 読み取り構文

- **対象**: `src/parse/cl/lexer.lisp`
- **内容**: `#S(struct-name :slot val ...)` を読み取れるようにし、必要に応じて `make-struct-name` 形式へ落とし込む。
- **根拠**: ANSI CL 2.4.8.13
- **難易度**: Medium

---

## 33. コンディション・リスタート (ANSI CL Ch.9)

### 33.0 define-condition スロット仕様

`define-condition` のスロット仕様は `defclass` に準じる。

| オプション | 状態 | 備考 |
|-----------|------|------|
| `:initarg` | ✅ | `defclass` 展開で `:initarg` キーワード生成; `make-instance` で動作 |
| `:accessor` / `:reader` / `:writer` | ✅ | `define-condition` → `defclass` 展開; `defclass` が reader/writer/accessor メソッド生成済み |
| `:initform` | ✅ | `defclass` 展開で `:initform` デフォルト値生成; `make-instance` で動作 |
| `:type` | ✅ | パーサーで受理; 実行時型チェックなし (ANSI では enforcement は実装定義) |
| `:report` | ✅ | FR-417: `define-condition` で `:report` → `defmethod print-object` 生成 (文字列/ラムダ/関数名) |
| 親コンディション型からの継承 | ✅ | FR-424: stdlib-source.lisp で全 ANSI 標準コンディション型を `define-condition` で定義; 継承チェーンを `defclass` 経由で確立 |

### 33.1 コンディション定義・生成

| 機能 | 状態 | 備考 |
|------|------|------|
| `define-condition` | ✅ | FR-417: `:report` オプション対応; 文字列/ラムダ/関数名 → `defmethod print-object` 生成 |
| `make-condition` | ✅ | FR-427: `stdlib-source.lisp` で `(apply #'make-instance type initargs)` に委譲 |
| ANSI 標準コンディション型階層 | ✅ | FR-424: stdlib-source.lisp に define-condition で全型定義 |

標準コンディション型 (FR-424 詳細):

| 型 | 状態 | 備考 |
|------|------|------|
| `condition` | ✅ |
| `serious-condition` | ✅ |
| `error` (型) | ✅ | ホスト CL `error` 型として `typep`/`handler-case` で動作 |
| `warning` | ✅ | ホスト CL `warning` 型として `typep`/`handler-case` で動作 |
| `simple-condition` / `simple-error` / `simple-warning` | ✅ |
| `type-error` / `simple-type-error` | ✅ |
| `arithmetic-error` / `division-by-zero` / `floating-point-overflow` | ✅ |
| `cell-error` / `unbound-variable` / `undefined-function` | ✅ |
| `control-error` / `program-error` | ✅ |
| `package-error` | ✅ |
| `stream-error` / `end-of-file` / `file-error` | ✅ |
| `print-not-readable` / `storage-condition` | ✅ |

### 33.2 コンディション アクセサ

| アクセサ | 状態 | 備考 |
|---------|------|------|
| `condition-report` / `princ` で報告 | ✅ | FR-417: `define-condition :report` → `defmethod print-object` 生成; `princ` 経由で呼び出し |
| `type-error-datum` / `type-error-expected-type` | ✅ | FR-424: type-error スロットリーダーとして定義 |
| `cell-error-name` | ✅ | FR-424: cell-error スロットリーダーとして定義 |
| `arithmetic-error-operands` / `arithmetic-error-operation` | ✅ | FR-424: arithmetic-error スロットリーダーとして定義 |
| `stream-error-stream` / `file-error-pathname` | ✅ | FR-424: stream-error/file-error スロットリーダーとして定義 |

### 33.3 シグナリング

| 機能 | 状態 | 備考 |
|------|------|------|
| `error` | ✅ | FR-626: `expander.lisp` で `(error "fmt" args...)` → `(error (format nil "fmt" args...))` 脱糖実装済み |
| `warn` | ✅ | FR-425: `~&WARNING: fmt` 形式で出力・nil 返却; `muffle-warning` リスタートはリスタート基盤の制約で未設置 |
| `signal` | ✅ | FR-418: `vm-signal` VM命令 + ビルトイン登録済み |
| `cerror` | ✅ | FR-419: `vm-cerror` VM命令 + ビルトイン登録済み |
| `break` | ✅ | FR-557 |
| `invoke-debugger` | ✅ | FR-557 |
| `*debugger-hook*` | ✅ | FR-557 |
| `*break-on-signals*` | ✅ | FR-557 |

#### FR-585: handler-case :no-error 節

- **対象**: `src/expand/macros-stdlib.lisp`
- **実装**: `handler-case` は `:no-error` 節を受理し、正常終了時の多値を束縛できる
- **内容**: フォームがシグナルなしに完了した場合のみ `success-body` を実行する
- **根拠**: ANSI CL 9.1.3 — handler-case :no-error
- **難易度**: Low

#### FR-557: break / invoke-debugger / *debugger-hook* / *break-on-signals*

- **対象**: `src/cli/main.lisp`, `src/vm/conditions.lisp`
- **実装**: `break` / `invoke-debugger` / `*debugger-hook*` / `*break-on-signals*` は利用可能
- **内容**: `break` は `*debugger-hook*` を介したブレーク、`invoke-debugger` は条件付きデバッガ起動、`*break-on-signals*` は条件一致で自動ブレークを提供する
- **根拠**: ANSI CL 9.1.5 — Debugger
- **難易度**: Medium

### 33.4 ハンドラ・リスタート

| 機能 | 状態 | 備考 |
|------|------|------|
| `handler-case` | ✅ | |
| `handler-case :no-error` 節 | ✅ | FR-585: `expander.lisp` で実装 |
| `handler-bind` | ✅ | FR-356: `progv` 動的束縛 + `*%condition-handlers*` レジストリで非巻き戻しセマンティクス実装; `signal` マクロがハンドラチェーンを走査 |
| `ignore-errors` | ✅ | FR-691: `(values nil condition)` を正しく返却 |
| `restart-case` | ✅ | FR-420: catch/throw ベースのリスタートプロトコル; 複数節・オプション解析対応; `*%active-restarts*` 動的レジストリ |
| `invoke-restart` | ✅ | FR-421: `*%active-restarts*` から検索 → `throw` で制御移行 |
| `invoke-restart-interactively` | ✅ | FR-421: `invoke-restart` に委譲 (対話的引数収集なし) |
| `find-restart` / `compute-restarts` | ✅ | FR-421: `*%active-restarts*` alist 検索・全リスタート返却 |
| `restart-name` | ✅ | cons セルの car から名前抽出 |
| `restart-p` | ✅ | FR-421: `consp` チェック |

### 33.5 標準リスタート関数

| 関数 | 状態 | 備考 |
|------|------|------|
| `abort` | ✅ | `find-restart` + `invoke-restart` で実装; リスタートなければ `error` |
| `continue` | ✅ | FR-421: `find-restart` + 条件付き `invoke-restart` |
| `use-value` | ✅ | `find-restart` + `invoke-restart` またはパススルー |
| `store-value` | ✅ | `find-restart` + `invoke-restart` またはパススルー |
| `muffle-warning` | ✅ | FR-425: `find-restart` + 条件付き `invoke-restart` |

### 33.6 コンディション チェック

| 機能 | 状態 | 備考 |
|------|------|------|
| `check-type` | ✅ | FR-426: `type-error` をシグナル (`make-condition` 使用); `store-value` リスタートは未設置 |
| `assert` | ✅ | FR-606: `cerror` による continuable error; ANSI 基本形は完備 |

---

## 34. シンボル (ANSI CL Ch.10)

| 関数 | 状態 | 備考 |
|------|------|------|
| `intern` (2値返却: シンボル + 状態) | ✅ | ホストブリッジ経由でホスト CL `intern` の2値返却がそのまま動作 |
| `find-symbol` | ✅ | FR-655: ホストブリッジ登録済み (`vm.lisp`) |
| `symbol-name` | ✅ | |
| `symbol-value` | ✅ | FR-647: ホストブリッジ登録済み (`vm.lisp`) |
| `symbol-function` | ✅ | ホストブリッジ経由 (`vm.lisp:491`) |
| `(setf symbol-value)` | ✅ | `setq` 経由 |
| `(setf symbol-function)` | ✅ | setf ハンドラ → `set-fdefinition` stdlib 経由; ホスト `(setf (symbol-function name) fn)` に委譲 |
| `symbol-plist` / `get` / `remprop` | ✅ | |
| `(setf get)` | ✅ | FR-622: expander + lowering/runtime の両経路で `symbol-plist` を更新 |
| `symbol-package` | ✅ | FR-558: ホストブリッジ経由で `symbol-package` に委譲 |
| `boundp` / `fboundp` | ✅ | |
| `makunbound` / `fmakunbound` | ✅ | |
| `keywordp` | ✅ | |
| `make-symbol` | ✅ | |
| `gensym` | ✅ | |
| `*gensym-counter*` | ✅ | FR-510: `stdlib-source.lisp` で defvar 定義 |
| `gentemp` | ✅ | FR-511 |
| `copy-symbol` | ✅ | FR-536: `stdlib-source.lisp` で実装 (プロパティコピーなし) |
| `set` (obsolete) | ✅ | FR-586: `stdlib-source.lisp` で `(defun set (sym val) (setf (symbol-value sym) val) val)` 実装 |

#### FR-586: set (廃止予定だが ANSI 標準)

- **対象**: `src/compile/builtin-registry.lisp`
- **内容**: `set symbol value` をビルトイン登録。`(setf (symbol-value symbol) value)` と同義
- **根拠**: ANSI CL 10.2 — set (廃止予定だが標準準拠に必要)
- **難易度**: Easy

#### FR-558: symbol-package

- **対象**: `src/vm/vm.lisp`, `src/compile/builtin-registry.lisp`
- **実装**: `symbol-package` はホストブリッジ経由で利用できる
- **内容**: シンボルの所属パッケージを返す
- **根拠**: ANSI CL 11.2 — Symbol Package
- **難易度**: Hard (パッケージシステム実装に依存)

---

## 35. パッケージ (ANSI CL Ch.11)

| 機能 | 状態 | 備考 |
|------|------|------|
| `in-package` | ✅ | FR-437: `*package*` を `setq` で設定し `(quote name)` を返す; VM グローバルに `*package*` 登録済み |
| `*package*` 変数 | ✅ | VM `*vm-initial-globals*` + `*builtin-special-variables*` で初期化; ホスト CL-USER にバインド |
| `defpackage` | ✅ | FR-437: `find-package` / `make-package` で実パッケージ作成; `:use` / `:export` オプション対応 |
| `find-package` | ✅ | FR-437: ホストブリッジ経由で完全動作; コンパイル済みコードから呼び出し可能 |
| `list-all-packages` | ✅ | FR-559: ホストブリッジ経由で委譲 |
| `make-package` | ✅ | FR-437: ホストブリッジ経由で委譲 |
| `export` | ✅ | FR-437: ホストブリッジ経由で委譲 (スタブマクロ削除) |
| `import` / `unexport` / `shadow` / `shadowing-import` | ✅ | FR-437: ホストブリッジ経由で委譲 |
| `use-package` / `unuse-package` | ✅ | FR-437: ホストブリッジ経由で委譲 |
| `rename-package` / `delete-package` | ✅ | FR-437: ホストブリッジ経由で委譲 |
| `with-package-iterator` | ✅ | FR-361: `%package-external-symbols` ホストブリッジ経由で実イテレーション; `(values more sym access pkg)` プロトコル準拠 |
| `do-symbols` / `do-external-symbols` / `do-all-symbols` | ✅ | FR-361: マクロ展開で `%package-symbols`/`%package-external-symbols` ホストブリッジヘルパー経由で dolist に展開 |
| `package-name` / `package-nicknames` | ✅ | FR-438: ホストブリッジ経由で委譲 |
| `package-use-list` / `package-used-by-list` | ✅ | FR-438: ホストブリッジ経由で委譲 |
| `package-shadowing-symbols` | ✅ | FR-438: ホストブリッジ経由で委譲 |

#### FR-559: list-all-packages

- **対象**: `src/vm/vm.lisp`, `src/compile/builtin-registry.lisp`
- **実装**: `list-all-packages` はホストブリッジ経由で利用できる
- **内容**: 全パッケージのリストを返す
- **根拠**: ANSI CL 11.2
- **難易度**: Low (FR-437 後)

---

## 36. 数値 (ANSI CL Ch.12)

### 36.1 基本算術・比較

| 演算 | 状態 | 備考 |
|------|------|------|
| `+` / `*` (可変引数) | ✅ | `define-list-lowerer` で `ast-binop` に変換後 `vm-add`/`vm-mul` を生成 |
| `-` (2引数以上) | ✅ | |
| `-` (1引数: 単項否定) | ✅ | FR-682: `expander.lisp:431` で単項否定実装済み |
| `-` (0引数) | ✅ | FR-682: `expander.lisp:430` で0引数エラー実装済み |
| `/` | ✅ | FR-661: `vm-cl-div` 命令 + expander で1/2/N引数対応; rational-preserving division |
| `=` / `<` / `>` / `<=` / `>=` (可変引数) | ✅ | FR-663: expander で1-arg→T, 2-arg→builtin, N-arg→AND chain with gensyms |
| `/=` | ✅ | FR-664: `stdlib-source.lisp` で実装 |
| `zerop` / `plusp` / `minusp` | ✅ |
| `abs` | ✅ |
| `min` / `max` | ✅ | FR-662: expander で1/2/N引数対応; 左畳み込みで可変引数化 |
| `1+` / `1-` / `incf` / `decf` | ✅ | FR-693: シンボルplace→`setq`、複合place→delta を gensym ガード; 2重評価修正 |
| `floor` / `ceiling` / `truncate` / `round` (2値返却) | ✅ |
| `ffloor` / `fceiling` / `ftruncate` / `fround` | ✅ |
| `mod` / `rem` | ✅ |
| `expt` / `sqrt` | ✅ | |
| `isqrt` | ✅ | FR-683: 整数 Newton 法で精度保証; `(floor (sqrt (float n)))` + Newton 修正ループ |
| `signum` | ✅ | FR-684: macros-stdlib.lisp で型保存実装 (`(signum 2.5)` → `1.0`, `(signum -3)` → `-1`) |
| `evenp` / `oddp` | ✅ |
| `gcd` / `lcm` | ✅ | FR-662: expander で0/1/2/N引数対応; `(gcd)` → `0`, `(lcm)` → `1`, N引数は左畳み込み |

### 36.2 浮動小数点型塔

cl-cc は内部で全浮動小数点を NaN-boxing double-float に統一。ユーザーコードからの型区別:

| 機能 | 状態 | 備考 |
|------|------|------|
| `floatp` | ✅ | FR-386: `stdlib-source.lisp` で実装 |
| `short-float` / `single-float` / `double-float` / `long-float` 型指定子 | ✅ | `vm-typep-check` で全て `floatp` にマッピング; `real`/`rational`/`bit` 型も追加 |
| `short-float-epsilon` / `single-float-epsilon` 等定数 | ✅ | FR-560: `stdlib-source.lisp` で実装 |
| `most-positive-short-float` 等定数群 | ✅ | FR-560: `stdlib-source.lisp` で実装 |
| `least-positive-short-float` 等定数群 | ✅ | FR-560: `stdlib-source.lisp` で実装 |
| `pi` / `most-positive-fixnum` 等 | ✅ | FR-561: `stdlib-source.lisp` で defvar 定義済み |
| `decode-float` / `scale-float` | ✅ | |
| `integer-decode-float` | ✅ | `builtin-registry-data.lisp:70` |
| `float-digits` / `float-precision` / `float-radix` | ✅ | `builtin-registry-data.lisp:65-68` |
| `float-sign` (1引数) | ✅ | |
| `float-sign` (2引数) | ✅ | FR-685: expander で `(* (float-sign x) (abs y))` に展開 |
| `float` (1引数: 変換) | ✅ | |
| `cis` | ✅ | FR-508: `stdlib-source.lisp` で実装 |

#### FR-560: 浮動小数点定数群

- **対象**: `src/vm/vm.lisp`
- **内容**: ANSI CL 12.1.6 の全浮動小数点定数を VM global として初期化。`short-float-epsilon`, `single-float-epsilon`, `double-float-epsilon`, `long-float-epsilon` および `most-positive-*`, `least-positive-*` 各系列
- **根拠**: ANSI CL 12.1.6 — Float Constants
- **難易度**: Easy

### 36.3 整数・ビット演算

| 演算 | 状態 | 備考 |
|------|------|------|
| `ash` | ✅ |
| `logand` / `logior` / `logxor` / `logeqv` | ✅ | FR-667: expander で0/1/2/N引数対応; identity: logand=-1, logior=0, logxor=0, logeqv=-1 |
| `lognot` | ✅ | |
| `logtest` / `logbitp` / `logcount` / `integer-length` | ✅ |
| `logorc1` / `logorc2` / `logandc1` / `logandc2` | ✅ | FR-529: `stdlib-source.lisp` で実装 |
| `lognand` / `lognor` | ✅ | FR-529: `stdlib-source.lisp` で実装 |
| `ldb` / `dpb` / `byte` / `ldb-test` | ✅ FR-492 |
| `byte-size` / `byte-position` | ✅ FR-532 |
| `deposit-field` / `mask-field` | ✅ FR-494 |
| `boole` (+ `boole-*` 定数 16 種) | ✅ FR-493 |

### 36.4 有理数・複素数

| 関数 | 状態 | 備考 |
|------|------|------|
| `rational` / `rationalize` | ✅ |
| 比率 (`ratio`) 算術演算 (`/` で自動生成) | ✅ | `vm-rational` (`vm-numeric.lisp:568`) |
| `bignum` (多倍長整数) | ✅ | FR-605: ホスト CL 委譲で完全動作; `(expt 2 100)` `(* 10^11 10^11)` `(integerp bignum)` 検証済み |
| `numerator` / `denominator` | ✅ |
| `realpart` / `imagpart` / `conjugate` / `phase` | ✅ |
| `complex` | ✅ |
| `rationalp` / `complexp` / `realp` | ✅ | FR-386: `stdlib-source.lisp` で実装 |

### 36.5 超越関数

| 関数 | 状態 | 備考 |
|------|------|------|
| `exp` / `log` (1引数) | ✅ |
| `log` (2引数: 任意底) | ✅ | FR-476: expander で底変換公式 `(/ (log x) (log base))` に展開 |
| `sin` / `cos` / `tan` | ✅ |
| `asin` / `acos` / `atan` (1/2引数) | ✅ |
| `sinh` / `cosh` / `tanh` | ✅ |
| `asinh` / `acosh` / `atanh` | ✅ | FR-639: VM命令 + ビルトイン登録済み |

### 36.6 数値パース・変換

| 関数 | 状態 | 備考 |
|------|------|------|
| `parse-integer` (基本) | ✅ |
| `parse-integer` (`:start`/`:end`/`:radix`/`:junk-allowed`) | ✅ | FR-478: expander でキーワード引数解析 → `%parse-integer-impl` stdlib関数; 1引数は builtin直通 |
| `parse-number` (汎用: 浮動小数点・比率) | ✅ | FR-391: `stdlib-source.lisp` で `read-from-string` に委譲; レキサーは float/ratio リテラルをネイティブ解析 |
| `number-to-string` 相当 / `write-to-string` | ✅ |

### 36.7 乱数・時刻

| 関数 | 状態 | 備考 |
|------|------|------|
| `random` (1引数) | ✅ | ホスト CL の `random` に委譲; 1引数形式は完全動作 |
| `make-random-state` | ✅ | |
| `random-state-p` | ✅ FR-509 |
| `*random-state*` | ✅ |
| `get-universal-time` / `get-internal-real-time` / `get-internal-run-time` | ✅ |
| `get-decoded-time` | ✅ | FR-679: `stdlib-source.lisp` で実装 |
| `decode-universal-time` / `encode-universal-time` | ✅ |
| `internal-time-units-per-second` 定数 | ✅ | FR-561: `stdlib-source.lisp` で実装 |

#### FR-561: 時刻・数値定数

- **対象**: `src/vm/vm.lisp`
- **内容**: VM 起動時に登録。`get-internal-real-time` が正しいスケールで機能するために必要
- **根拠**: ANSI CL 25.1.4 / 12.1
- **難易度**: Easy

---

## 37. 文字 (ANSI CL Ch.13)

| 関数 | 状態 | 備考 |
|------|------|------|
| `characterp` | ✅ |
| `char=` / `char<` / `char>` / `char<=` / `char>=` / `char/=` | ✅ | FR-645: expander で可変引数対応; AND chain with gensyms |
| `char-equal` / `char-lessp` 等 (case-insensitive) | ✅ | FR-645: 同上、case-insensitive 版も可変引数対応 |
| `char-upcase` / `char-downcase` | ✅ |
| `char-code` / `code-char` | ✅ |
| `char-name` / `name-char` | ✅ |
| `char-int` | ✅ |
| `graphic-char-p` / `alpha-char-p` / `alphanumericp` | ✅ |
| `both-case-p` / `upper-case-p` / `lower-case-p` | ✅ |
| `digit-char-p` | ✅ | FR-668: 1引数→builtin、2引数→char-code算術による radix 対応 (expander) |
| `standard-char-p` | ✅ |
| `digit-char` (1引数) | ✅ |
| `digit-char` (2引数: 基数付き) | ✅ FR-477 |
| 標準文字名 (`#\Space` `#\Newline` `#\Tab` `#\Return`) | ✅ | `lexer.lisp:28` |
| 標準文字名 (`#\Backspace` `#\Page` `#\Rubout` `#\Null` `#\Escape`) | ✅ | `lexer.lisp` で直接ハンドリング; `#\Null`/`#\Escape`/`#\Esc` 追加 |
| `base-char` / `extended-char` 型区別 | ✅ | FR-392: `vm-typep-check` で `base-char`/`standard-char` → `characterp` マッピング |
| Unicode 文字 (code-point > 127) | ✅ | 21-bit コードポイント対応 (`value.lisp:24`); `encode-char`/`decode-char` 完全動作 |

#### FR-562: Unicode サポート

- **対象**: `src/parse/cl/lexer.lisp`, `src/vm/strings.lisp`
- **内容**: Unicode code point 全域 (U+0000〜U+10FFFF) のキャラクタ表現。`char-code` / `code-char` の 21-bit 対応。UTF-8/UTF-16 ストリームエンコーディング指定
- **根拠**: 2026 年モダン CL — SBCL / CCL は完全 Unicode 対応
- **難易度**: Medium

---
