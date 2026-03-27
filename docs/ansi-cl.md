# ANSI CL Standard Compliance

2026年のモダンなCommon Lispコンパイラに必要なANSI CL標準機能の網羅的なカバレッジ。

凡例: ✅ 実装済み / 🔶 部分実装 / ❌ 未実装 (FR番号で追跡)

---

## 1. 評価・コンパイル (ANSI CL Ch.3)

### 1.1 特殊形式

| 形式 | 状態 | 備考 |
|------|------|------|
| `quote` | ✅ | |
| `if` | ✅ | |
| `progn` | ✅ | |
| `let` / `let*` | 🔶 | `let*` はマクロ展開で `let` にデシュガー；`(let () body)` 空バインディング形式は FR-623 |
| `flet` / `labels` | 🔶 | `(flet () body)` 空バインディング形式は FR-623 |
| `macrolet` | ✅ | |
| `symbol-macrolet` | 🔶 | FR-220: 展開ロジック未実装 |
| `lambda` / `function` (`#'`) | ✅ | |
| `setq` / `psetq` | ✅ | |
| `block` / `return-from` | ✅ | |
| `tagbody` / `go` | ✅ | |
| `catch` / `throw` | ❌ | FR-689: `codegen.lisp:54-74` で両メソッドが `(declare (ignore tag-reg))`; タグ不一致確認なし、`vm-catch`/`vm-throw` 命令なし、非局所脱出なし |
| `unwind-protect` | ✅ | |
| `multiple-value-call` | ✅ | |
| `multiple-value-prog1` | ✅ | |
| `the` | ✅ | |
| `declare` | 🔶 | 基本型宣言あり；`optimize` / `ftype` 伝播は FR-395 |
| `locally` | 🔶 | FR-397: 宣言が `progn` 展開で消失 |
| `load-time-value` | 🔶 | FR-400: 自己ホスト環境で `our-eval` が必要 |
| `eval-when` | 🔶 | FR-399: 8シチュエーション組み合わせ未完全 |
| `progv` | ✅ | |

### 1.2 評価・コンパイル API

| 関数・マクロ | 状態 | 備考 |
|-------------|------|------|
| `eval` | ✅ | `our-eval` で自己ホスト対応 |
| `macroexpand` / `macroexpand-1` | ❌ | FR-631: ビルトイン未登録; `our-macroexpand-1` はコンパイラ内部のみ |
| `compiler-macroexpand-all` | ✅ | cl-cc 独自拡張 |
| `define-compiler-macro` | 🔶 | FR-365: スタブのみ |
| `compiler-macro-function` / `(setf compiler-macro-function)` | ❌ | FR-365 |
| `macro-function` / `(setf macro-function)` | ❌ | FR-428 |
| `*macroexpand-hook*` | ❌ | FR-429 |
| `with-compilation-unit` | ❌ | FR-363 |
| `compile` (関数) | ❌ | FR-512 |
| `compile-file` | ✅ | `cl-cc compile` コマンド |
| `load` | ✅ | |
| `constantp` / `special-operator-p` | ❌ | FR-538 |
| `fdefinition` / `(setf fdefinition)` | ❌ | FR-548 |
| `function-lambda-expression` | ❌ | FR-549 |
| `make-load-form` / `make-load-form-saving-slots` | ❌ | FR-550 |
| `identity` / `constantly` / `complement` | 🔶 | FR-597: `pipeline.lisp:162-164` でプリルード定義のみ、ビルトイン未登録 |
| `defun` / `defmacro` ドキュメント文字列 | ❌ | FR-607: 文字列リテラルが本体先頭にあっても破棄される |

#### FR-548: fdefinition / (setf fdefinition)

- **対象**: `src/vm/vm.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `symbol-function` でのアクセスはあるが ANSI CL 標準の `fdefinition` API なし
- **内容**: `(fdefinition name)` — 関数名 (シンボルまたは `(setf name)`) から関数オブジェクト取得。`(setf (fdefinition name) fn)` — 設定
- **根拠**: ANSI CL 5.3 — fdefinition
- **難易度**: Low

#### FR-549: function-lambda-expression

- **対象**: `src/vm/vm.lisp`
- **現状**: vm-closure-object は `entry-label` / `captured-vars` を保持するが lambda 式本体を保存しない
- **内容**: `(function-lambda-expression fn)` → `(lambda-expression closure-p name)` の3値。コンパイラが lambda 式を VM クロージャに埋め込む
- **根拠**: ANSI CL 5.3 — introspection
- **難易度**: Medium

#### FR-550: make-load-form

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-clos.lisp`
- **現状**: CLOS インスタンスをファイルコンパイル時に定数として使用する手段なし
- **内容**: `make-load-form` GF — コンパイル時定数として CLOS インスタンスを出力可能に。`make-load-form-saving-slots` デフォルト実装
- **根拠**: ANSI CL 3.2.4 — Literal Objects in Compiled Files
- **難易度**: Hard

---

## 2. ラムダリスト (ANSI CL Ch.3.4)

| 機能 | 状態 | 備考 |
|------|------|------|
| Required parameters | ✅ | |
| `&optional` パラメータ + デフォルト値 | ✅ | |
| `&optional` supplied-p 変数 | ❌ | FR-696: `allocate-defaulting-params` (`codegen-functions.lisp:111`) が `(third param)` (supplied-p) を無視; パーサーは正しく保存するが codegen で破棄 |
| `&rest` パラメータ | ✅ | |
| `&key` パラメータ + デフォルト値 | ✅ | |
| `&key` supplied-p 変数 | ❌ | FR-696: 同上 |
| `&allow-other-keys` | ✅ | `expander.lisp:295` |
| `&aux` パラメータ | ✅ | |
| `&body` (マクロラムダリスト) | ✅ | |
| `&whole` (マクロラムダリスト) | ✅ | |
| `&environment` (マクロラムダリスト) | 🔶 | 受け取るが環境オブジェクトが不完全 (FR-394) |
| Destructuring lambda list | ✅ | `destructuring-bind` 実装済み |
| Specialized lambda list (CLOS) | ✅ | `defmethod` で使用 |
| BOA constructor lambda list | ✅ | `expander-defstruct.lisp` |
| `call-arguments-limit` 定数 | ❌ | FR-551 |
| `lambda-parameters-limit` 定数 | ❌ | FR-551 |
| `lambda-list-keywords` 変数 | ❌ | FR-551 |

#### FR-551: ラムダリスト定数・変数

- **対象**: `src/vm/vm.lisp`
- **現状**: ANSI CL 標準の `call-arguments-limit` / `lambda-parameters-limit` / `multiple-values-limit` / `lambda-list-keywords` が未定義
- **内容**: VM global 変数として起動時に登録。`call-arguments-limit` はアーキテクチャ依存の最大値
- **根拠**: ANSI CL 3.4 / 12.1 — implementation limits
- **難易度**: Easy

---

## 3. 型・クラス (ANSI CL Ch.4)

| 機能 | 状態 | 備考 |
|------|------|------|
| `deftype` (2引数形式) | ✅ | |
| `deftype` (lambda-list 付き) | ❌ | FR-430 |
| `define-symbol-macro` | ❌ | FR-398 |
| `typep` | ✅ | |
| `subtypep` | ❌ | FR-624: 全ソースに不在 |
| `type-of` | 🔶 | FR-625: `float`/`function` 型返却なし |
| `class-of` | ❌ | FR-385 (clos.md) |
| `find-class` / `(setf find-class)` | ❌ | FR-552 |
| `coerce` | 🔶 | FR-630: クォート済みリテラル型のみ対応; `(coerce x type-var)` は実行時に失敗 |
| `upgraded-array-element-type` | ❌ | FR-553 |
| `upgraded-complex-part-type` | ❌ | FR-553 |
| `standard-object` / `structure-object` 組み込みクラス | ❌ | FR-528 (clos.md) |

#### FR-552: find-class / (setf find-class)

- **対象**: `src/vm/vm-clos.lisp`
- **現状**: 内部クラスレジストリ (`*class-table*` 相当) は存在するがユーザー呼び出し可能 API なし
- **内容**: `(find-class name &optional errorp environment)` — クラスオブジェクト取得。`(setf (find-class name) class)` — 登録
- **根拠**: ANSI CL 7.7.6 — find-class
- **難易度**: Low

#### FR-553: upgraded-array-element-type / upgraded-complex-part-type

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `make-array :element-type` は無視。実際の element-type 昇格なし
- **内容**: `upgraded-array-element-type type &optional env` — 型指定子を実際のアレイ element-type に昇格。`upgraded-complex-part-type` 同様
- **根拠**: ANSI CL 15.1.2.1, 12.1.5.2
- **難易度**: Medium

### 3.2 複合型指定子 (Compound Type Specifiers)

ANSI CL の型システムは原子型だけでなく合成型指定子をサポートする。`typep` / `subtypep` / `the` / `ftype` で使用。

| 型指定子 | 状態 | 備考 |
|---------|------|------|
| `(member obj...)` | ✅ | `vm-typep-check:88` の `otherwise` → ホスト CL `typep` 委譲 |
| `(satisfies predicate)` | 🔶 | FR-581: ホスト CL 標準述語 (例: `evenp`) は動作; ユーザー定義 cl-cc 関数は SBCL 名前空間になく失敗 |
| `(and type...)` | ✅ | ホスト CL `typep` 委譲 |
| `(or type...)` | ✅ | ホスト CL `typep` 委譲 |
| `(not type)` | ✅ | ホスト CL `typep` 委譲 |
| `(eql x)` | ✅ | ホスト CL `typep` 委譲 |
| `(values type...)` | ❌ | FR-581 |
| `(function (args) return)` | ❌ | FR-581 |
| `(integer low high)` 範囲 | ✅ | ホスト CL `typep` 委譲 |
| `(float low high)` 範囲 | ✅ | ホスト CL `typep` 委譲 |
| `(rational ...)` / `(real ...)` | ✅ | ホスト CL `typep` 委譲 |
| `(mod n)` | ✅ | ホスト CL `typep` 委譲 |
| `(array element-type dimensions)` | 🔶 | FR-581: ホスト CL 委譲で `:element-type` のみ対応; VM CLOS 配列では `dimensions` チェック不完全 |
| `(simple-array ...)` / `(vector ...)` 複合 | 🔶 | FR-581: 一部はホスト CL 委譲で動作 |
| `(complex type)` | ✅ | ホスト CL `typep` 委譲 |
| `fixnum` / `bignum` / `ratio` 型 | 🔶 | `typep` で `fixnum` は対応 (`primitives.lisp:252`); `bignum`/`ratio` はホスト CL 委譲 |

#### FR-581: 複合型指定子完全サポート

- **対象**: `src/vm/primitives.lisp` の `vm-typep-check`, `src/compile/codegen.lisp`
- **現状**: `vm-typep-check` の `otherwise` ブランチ (`primitives.lisp:88`) で `(ignore-errors (typep value type-sym))` を実行 — 複合型指定子のほとんどがホスト CL SBCL に委譲されて動作。`(satisfies pred)` はホスト CL 標準述語のみ対応 (ユーザー定義関数は SBCL 名前空間にない)。`(values ...)` / `(function ...)` は SBCL で実行時エラー
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
| `case` / `ecase` | 🔶 | `ecase` は `type-error` ではなく plain `error` をシグナル (`macros-stdlib.lisp:119-125`) |
| `ccase` | ❌ | FR-354: `store-value` リスタート付き |
| `typecase` / `etypecase` | 🔶 | `etypecase` も `type-error` ではなく plain `error` をシグナル |
| `ctypecase` | ❌ | FR-354 |

### 4.2 代入・変更

| 形式 | 状態 | 備考 |
|------|------|------|
| `setf` | ✅ | |
| `psetf` | ✅ | |
| `shiftf` | ✅ | |
| `rotatef` | 🔶 | FR-690: 2引数のみ対応 (`(rotatef a b c)` 非対応); `setq` 使用で複合place非対応 (`macros-stdlib.lisp:92-101`) |
| `define-modify-macro` | ✅ | |
| `defsetf` (短/長形式) | ❌ | FR-355 |
| `define-setf-expander` | ❌ | FR-355 |
| `get-setf-expansion` | ❌ | FR-355 |

### 4.3 関数呼び出し・多値

| 形式 | 状態 | 備考 |
|------|------|------|
| `apply` / `funcall` | ✅ | |
| `values` / `values-list` | ✅ | |
| `multiple-value-bind` | ✅ | |
| `multiple-value-setq` | ✅ | |
| `(setf (values a b ...) expr)` — values を setf 場所に使用 | ❌ | FR-603 |
| `multiple-value-list` | ✅ | |
| `nth-value` | 🔶 | FR-402: 定数引数での `mvb` 最適展開未実装 |
| `multiple-values-limit` 定数 | ❌ | FR-551 |

### 4.4 等価述語

| 関数 | 状態 | 備考 |
|------|------|------|
| `eq` | ✅ | 同一オブジェクト比較 |
| `eql` | ✅ | `eq` + 数値・文字の値比較 |
| `equal` | ✅ | 構造的等価 (cons tree / string / bit-vector) |
| `equalp` | ❌ | FR-582: `equal` + 大文字小文字無視文字列・配列・ハッシュテーブル・構造体の再帰比較 |
| `=` (数値等価) | ✅ | |
| `string=` / `char=` 等 | ✅ | 型固有等価 |

#### FR-582: equalp

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `equal` は `vm-equal` として実装済み。`equalp` はコードベース全体に不在
- **内容**: `equalp` — 数値は `=` で比較、文字・文字列は case-insensitive、配列・ハッシュテーブル・構造体を再帰的に比較
- **根拠**: ANSI CL 5.3 — equalp; `:test #'equalp` のハッシュテーブル (FR-565) に必要
- **難易度**: Medium

### 4.5 `the` の多値・型宣言

| 機能 | 状態 | 備考 |
|------|------|------|
| `(the type form)` | ✅ | |
| `(the (values type...) form)` | ❌ | FR-583: 多値型宣言 |
| `(ignore-errors form)` → `(values result nil)` または `(values nil condition)` | 🔶 | FR-691: `handler-case` で `nil` のみ返却; `(values nil condition)` の condition 第2値が破棄される (`macros-stdlib.lisp:612-616`) |

#### FR-583: the による多値型宣言

- **対象**: `src/compile/codegen.lisp`, `src/parse/ast.lisp`
- **現状**: `ast-the` は単一型のみ保持
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
| `for`/`as :downfrom/:downto/:above` | ❌ | FR-695: `FROM` パーサーに `DOWNFROM`/`DOWNTO`/`ABOVE` キーワード未実装; 未知キーワードでエラーシグナル |
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
| `initially` | ❌ | FR-543 |
| `with` | ✅ | |
| `named loop` | ❌ | FR-638: `loop named foo ... (return-from foo val)` 未実装 |
| `loop-finish` | ❌ | FR-539 |
| `do` 句 (loop内) | ✅ | |
| `return` 句 (loop内) | ✅ | |
| `for (a b) in list` destructuring | ✅ | `%loop-emit-destructuring` (`loop-emitters.lisp:121`) |

#### FR-543: loop initially 句

- **対象**: `src/expand/loop.lisp`
- **現状**: `finally` は実装済みだが `initially` はゼロマッチ
- **内容**: `initially` 句の本体をループ開始前に実行。`finalize-loop-state` の先頭に追加
- **根拠**: ANSI CL 6.1.7 — loop initially
- **難易度**: Easy

### 5.3 マッピング関数

| 関数 | 状態 | 備考 |
|------|------|------|
| `mapcar` / `mapc` / `mapcan` (1シーケンス) | 🔶 | FR-665: `(our-defmacro mapcar (fn list) ...)` — 1シーケンス固定; `(mapcar #'+ '(1 2) '(3 4))` 非対応 |
| `maplist` / `mapl` / `mapcon` | ❌ | FR-360: CDR 写像関数群 |
| `every` / `some` / `notany` / `notevery` (1シーケンス) | ✅ | |
| `every` / `some` 等 多シーケンス並列形式 | ❌ | FR-650: `(every pred seq1 seq2)` 非対応; 全て2引数 `(pred list)` のみ |

---

## 6. オブジェクトシステム・CLOS (ANSI CL Ch.7)

詳細は [clos.md](clos.md) 参照。

### 6.1 定義・生成

| 機能 | 状態 | 備考 |
|------|------|------|
| `defclass` (基本) | ✅ | |
| `:allocation :class` スロット | ❌ | FR-374 (clos.md) |
| `:default-initargs` | ❌ | FR-375 (clos.md) |
| `defgeneric` (基本) | ✅ | |
| `defgeneric` オプション | ❌ | FR-382 (clos.md) |
| `defmethod` (primary) | ✅ | |
| `defmethod :before/:after/:around` | ❌ | FR-215 (clos.md) |
| `eql` 特化子 | ❌ | FR-359 |
| `make-instance` | ✅ | |
| `find-class` / `(setf find-class)` | ❌ | FR-552 |
| `class-name` | ❌ | FR-677: 完全不在; ANSI `(class-name class)` — クラスオブジェクトの名前シンボルを返す |
| `ensure-class` | 🔶 | 内部実装のみ |
| `ensure-generic-function` | ❌ | FR-525 (clos.md) |

### 6.2 アクセス・変更

| 機能 | 状態 | 備考 |
|------|------|------|
| `slot-value` / `(setf slot-value)` | ✅ | |
| `slot-boundp` | ✅ | `vm-slot-boundp` 命令あり (`vm-clos.lisp:62`) |
| `slot-makunbound` | ✅ | `vm-slot-makunbound` 命令あり (`vm-clos.lisp:70`) |
| `slot-exists-p` | ✅ | `vm-slot-exists-p` 命令あり (`vm-clos.lisp:78`) |
| `slot-missing` プロトコル | ❌ | FR-554 |
| `slot-unbound` プロトコル | ❌ | FR-384 (clos.md) |
| `with-slots` | 🔶 | FR-676: `let` バインディング展開 (ANSI は `symbol-macrolet` 必須); スロット変数への `setf` がオブジェクトに書き戻されない |
| `with-accessors` | 🔶 | FR-701: `let` バインディング展開 (`macros-stdlib.lisp:168`); ANSI は `symbol-macrolet` 必須; `setf` がオブジェクトに書き戻されない (FR-676 と同パターン) |

#### FR-554: slot-missing プロトコル

- **対象**: `src/vm/vm-clos.lisp`
- **現状**: `vm-slot-read` / `vm-slot-write` が存在しないスロット名に対して生 `error` を発生させる
- **内容**: `slot-missing` GF 呼び出し。デフォルトメソッドが `error`。ユーザーカスタマイズ可能
- **根拠**: ANSI CL 7.5.11 — slot-missing
- **難易度**: Low

### 6.3 ディスパッチ・メソッド

| 機能 | 状態 | 備考 |
|------|------|------|
| `call-next-method` / `next-method-p` | 🔶 | 引数なし `call-next-method` は動作; 引数あり再ディスパッチは未実装 (`vm-execute.lisp:270-298`) |
| `initialize-instance` GF | ❌ | FR-379 (clos.md) |
| `shared-initialize` GF | 🔶 | FR-1005: 実装済み — `:__initargs__` マップを走査して slot-names でフィルタ; MOP プロトコル非準拠 (`macros-sequence.lisp:560`) |
| `reinitialize-instance` GF | 🔶 | FR-1005: 実装済み — `:__initargs__` マップを走査して plist で一致するスロットを更新; MOP プロトコル非準拠 (`macros-sequence.lisp:532`) |
| `allocate-instance` GF | ❌ | FR-524 (clos.md) |
| `change-class` | 🔶 | FR-380 (clos.md) |
| `update-instance-for-different-class` GF | 🔶 | FR-1005: スタブマクロ — `reinitialize-instance` に委譲 (`macros-sequence.lisp:488`) |
| `update-instance-for-changed-class` GF | 🔶 | FR-1005: スタブマクロ — `reinitialize-instance` に委譲 (`macros-sequence.lisp:495`) |
| `class-of` | ❌ | FR-385 (clos.md) |
| `compute-applicable-methods` | ❌ | FR-377 (clos.md) |
| `no-applicable-method` GF | ❌ | FR-377 (clos.md) |
| `find-method` / `add-method` / `remove-method` | ❌ | FR-377 (clos.md) |
| Method Combination (`:before/:after/:around`) | ❌ | FR-215 (clos.md) |
| `define-method-combination` | ❌ | FR-376 (clos.md) |
| C3 線形化 CPL | ❌ | FR-378 (clos.md) |
| MOP イントロスペクション | ❌ | FR-523〜528 (clos.md) |
| `no-next-method` GF | ❌ | FR-584 |
| `invalid-method-error` | ❌ | FR-584 |
| `method-combination-error` | ❌ | FR-584 |
| `print-object` GF | 🔶 | FR-390: マクロ実装 (`macros-sequence.lisp:240-255`) のみ; 真の GF ディスパッチなし |
| `describe-object` GF | 🔶 | スタブ |

#### FR-584: no-next-method / invalid-method-error / method-combination-error

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-clos.lisp`
- **現状**: `call-next-method` で次のメソッドがない場合に生 `error`。`no-next-method` GF 呼び出しなし
- **内容**: `no-next-method gf method args` — カスタマイズ可能エラーGF。`invalid-method-error method format-string args` / `method-combination-error format-string args` もシグナリング関数として実装
- **根拠**: ANSI CL 7.6.6.1 — no-next-method
- **難易度**: Low

---

## 7. 構造体 (ANSI CL Ch.8)

| 機能 | 状態 | 備考 |
|------|------|------|
| `defstruct` (基本: コンストラクタ・述語・アクセサ) | ✅ | `expander-defstruct.lisp` |
| `:constructor` オプション (名前/BOA) | ✅ | カスタム名・BOA (`&aux` 含む) 完全対応 (`expander-defstruct.lisp:15-62`) |
| `:copier` / `:predicate` オプション | ❌ | FR-544 |
| `:print-function` / `:print-object` | ❌ | FR-544 |
| `:include` 継承 | 🔶 | FR-545: 親スロット継承・スーパークラス設定は動作; 子構造体固有の新スロットアクセサ生成が壊れている (`(color-point-color inst)` → Undefined function) |
| `:type` オプション (list/vector) | ❌ | FR-546 |
| `:conc-name` オプション | ✅ | `expander-defstruct.lisp:79` |
| スロットオプション `:type` / `:read-only` | ❌ | FR-546 |
| `copy-structure` | ❌ | FR-555 |
| `structure-object` 型 (CLOS 統合) | ❌ | FR-528 (clos.md) |
| `#S(...)` 読み取り構文 | ❌ | FR-556 |

#### FR-544: defstruct コンストラクタ・コピー・述語オプション

- **対象**: `src/expand/expander-defstruct.lisp`
- **現状**: `:constructor nil` / `:copier nil` / `:predicate nil` オプション未対応
- **内容**: ANSI CL 8.1.5〜8.1.7 の全オプション処理
- **難易度**: Medium

#### FR-545: defstruct :include 継承 — 子スロットアクセサ生成バグ

- **対象**: `src/expand/expander-defstruct.lisp`
- **現状**: `:include` 句は実装済み (`expander-defstruct.lisp:90`)。親スロットの継承とスーパークラス設定は動作する。しかし子構造体に追加した固有スロットのアクセサ関数が生成されない。例: `(defstruct (color-point (:include point)) (color :red))` → `(color-point-color inst)` で `Error: Undefined function: CL-CC::COLOR-POINT-COLOR`
- **内容**: 子構造体の新スロットに対してもアクセサ・setf・コンストラクタキーワードを生成する
- **根拠**: ANSI CL 8.1.5.5
- **難易度**: Low

#### FR-546: defstruct :type / :conc-name / スロットオプション

- **対象**: `src/expand/expander-defstruct.lisp`
- **内容**: `:type list`/`:type vector`、`:conc-name`、スロット `:type`/`:read-only`
- **根拠**: ANSI CL 8.1.5
- **難易度**: Medium

#### FR-555: copy-structure

- **対象**: `src/expand/expander-defstruct.lisp`
- **現状**: `defstruct` が生成する copier 関数の抑制 (`:copier nil`) は FR-544 で未対応。`copy-structure` 汎用関数も不在
- **内容**: `(copy-structure structure-object)` — 全スロットをシャローコピーした新インスタンスを返す
- **根拠**: ANSI CL 8.1.6 — copy-structure
- **難易度**: Easy

#### FR-556: #S 読み取り構文

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#S(...)` ディスパッチマクロ文字が未登録
- **内容**: `#S(struct-name :slot val ...)` を `(make-struct-name :slot val ...)` に展開するリーダーマクロ
- **根拠**: ANSI CL 2.4.8.13
- **難易度**: Medium

---

## 8. コンディション・リスタート (ANSI CL Ch.9)

### 8.0 define-condition スロット仕様

`define-condition` のスロット仕様は `defclass` に準じる。

| オプション | 状態 | 備考 |
|-----------|------|------|
| `:initarg` | 🔶 | 基本的な`:initarg`は動作する可能性あり |
| `:accessor` / `:reader` / `:writer` | ❌ | FR-417 に包含 |
| `:initform` | 🔶 | |
| `:type` | ❌ | |
| `:documentation` | ❌ | FR-417 |
| `:report` | ❌ | FR-417 |
| 親コンディション型からの継承 | ❌ | FR-424 (条件型階層が未実装) |

### 8.1 コンディション定義・生成

| 機能 | 状態 | 備考 |
|------|------|------|
| `define-condition` | 🔶 | FR-417: `:report` オプション無視 |
| `make-condition` | ❌ | FR-427 |
| ANSI 標準コンディション型階層 | ❌ | FR-424: VM 固有 5 型のみ |

標準コンディション型 (FR-424 詳細):

| 型 | 状態 |
|----|------|
| `condition` | ✅ |
| `serious-condition` | ❌ |
| `error` (型) | 🔶 |
| `warning` | 🔶 |
| `simple-condition` / `simple-error` / `simple-warning` | ❌ |
| `type-error` / `simple-type-error` | ❌ |
| `arithmetic-error` / `division-by-zero` / `floating-point-overflow` | ❌ |
| `cell-error` / `unbound-variable` / `undefined-function` | ❌ |
| `control-error` / `program-error` | ❌ |
| `package-error` | ❌ |
| `stream-error` / `end-of-file` / `file-error` | ❌ |
| `print-not-readable` / `storage-condition` | ❌ |

### 8.2 コンディション アクセサ

| アクセサ | 状態 | 備考 |
|---------|------|------|
| `condition-report` / `princ` で報告 | ❌ | FR-417 |
| `type-error-datum` / `type-error-expected-type` | ❌ | FR-424 |
| `cell-error-name` | ❌ | FR-424 |
| `arithmetic-error-operands` / `arithmetic-error-operation` | ❌ | FR-424 |
| `stream-error-stream` / `file-error-pathname` | ❌ | FR-424 |

### 8.3 シグナリング

| 機能 | 状態 | 備考 |
|------|------|------|
| `error` | 🔶 | FR-626: 文字列+引数形式 `(error "~A" x)` 未対応 (ユニット引数のみ) |
| `warn` | 🔶 | FR-425: `muffle-warning` リスタート未設置 |
| `signal` | 🔶 | FR-418: `vm-signal` VM命令あり (`conditions.lisp:145`)、ビルトイン未登録 |
| `cerror` | 🔶 | FR-419: VM 命令あり、ビルトイン未登録 |
| `break` | ❌ | FR-557 |
| `invoke-debugger` | ❌ | FR-557 |
| `*debugger-hook*` | ❌ | FR-557 |
| `*break-on-signals*` | ❌ | FR-557 |

#### FR-585: handler-case :no-error 節

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `handler-case` は `(handler-case form (type (e) handler)...)` のみ。`:no-error` 節なし
- **内容**: `(handler-case form ... (:no-error (vals...) success-body))` — フォームがシグナルなしに完了した場合のみ `success-body` を実行。多値の正常値を変数に受け取る
- **根拠**: ANSI CL 9.1.3 — handler-case :no-error
- **難易度**: Low

#### FR-557: break / invoke-debugger / *debugger-hook* / *break-on-signals*

- **対象**: `src/cli/main.lisp`, `src/vm/conditions.lisp`
- **現状**: デバッガ統合なし。`break` が全コードベースに不在
- **内容**: `break` — `*debugger-hook*` 呼び出し後に REPL に落ちる。`invoke-debugger condition` — デバッガを起動。`*break-on-signals*` — 型が一致するシグナルで自動ブレーク
- **根拠**: ANSI CL 9.1.5 — Debugger
- **難易度**: Medium

### 8.4 ハンドラ・リスタート

| 機能 | 状態 | 備考 |
|------|------|------|
| `handler-case` | ✅ | |
| `handler-case :no-error` 節 | ❌ | FR-585 |
| `handler-bind` | 🔶 | FR-356: 巻き戻しセマンティクスが誤り |
| `ignore-errors` | 🔶 | FR-691: condition 第2値が破棄される (→ FR-691 参照) |
| `restart-case` | 🔶 | FR-420: リスタート名・オプション無視 |
| `restart-bind` | ❌ | FR-692: 完全スタブ — `(declare (ignore bindings))` → `(progn ,@body)`; バインディング全無視 (`macros-stdlib.lisp:236-239`) |
| `invoke-restart` | 🔶 | FR-421: `our-defmacro` スタブ — 常に `(error "No restart named ~S is active" ...)` を発生させる (`macros-stdlib.lisp:248`) |
| `invoke-restart-interactively` | ❌ | FR-421: 全コードベースに不在 |
| `find-restart` / `compute-restarts` | 🔶 | FR-421: `register-macro` による nil 返却スタブ (`macros-stdlib.lisp:257`) |
| `restart-name` | ✅ | `macros-stdlib.lisp:251` |
| `restart-p` | ❌ | FR-421: 全コードベースに不在 |
| `with-simple-restart` | ❌ | FR-422 |
| `with-condition-restarts` | ❌ | FR-423 |
| `restart-case :interactive` / `:report` / `:test` オプション | ❌ | FR-420 |

### 8.5 標準リスタート関数

| 関数 | 状態 | 備考 |
|------|------|------|
| `abort` | 🔶 | `macros-stdlib.lisp:261`: 無条件 `error` を発生させるスタブ |
| `continue` | ❌ | FR-421 に依存 |
| `use-value` | 🔶 | `macros-stdlib.lisp:265`: 値パススルーのスタブ |
| `store-value` | 🔶 | `macros-stdlib.lisp:265`: 値パススルーのスタブ |
| `muffle-warning` | ❌ | FR-425: `nil` 返却のみ |

### 8.6 コンディション チェック

| 機能 | 状態 | 備考 |
|------|------|------|
| `check-type` | 🔶 | FR-426: `store-value` リスタート未設置 |
| `assert` | 🔶 | 基本シグナルのみ；`:place` リスト (`(setf (aref ...) ...)` 形式) なし (FR-606) |

---

## 9. シンボル (ANSI CL Ch.10)

| 関数 | 状態 | 備考 |
|------|------|------|
| `intern` (2値返却: シンボル + 状態) | 🔶 | 2値目の状態 (`nil`/`:internal`/`:external`/`:inherited`) 未確認 |
| `find-symbol` | ❌ | FR-655: コンパイラ登録なし・ホストブリッジ対象外 (`vm.lisp:491`) |
| `symbol-name` | ✅ | |
| `symbol-value` | ❌ | FR-647: ビルトイン未登録・ホストブリッジ対象外 |
| `symbol-function` | ✅ | ホストブリッジ経由 (`vm.lisp:491`) |
| `(setf symbol-value)` | ✅ | `setq` 経由 |
| `(setf symbol-function)` | 🔶 | `(setf (fdefinition name) fn)` で代替可能 (FR-548) |
| `symbol-plist` / `get` / `remprop` | ✅ | |
| `(setf get)` | ❌ | FR-622: plist への `(setf (get sym key) val)` 展開なし |
| `symbol-package` | ❌ | FR-558 |
| `boundp` / `fboundp` | ✅ | |
| `makunbound` / `fmakunbound` | ✅ | |
| `keywordp` | ✅ | |
| `make-symbol` | ✅ | |
| `gensym` | ✅ | |
| `*gensym-counter*` | ❌ | FR-510 |
| `gentemp` | ❌ | FR-511 |
| `copy-symbol` | ❌ | FR-536 |
| `set` (obsolete) | ❌ | FR-586 |

#### FR-586: set (廃止予定だが ANSI 標準)

- **対象**: `src/compile/builtin-registry.lisp`
- **現状**: `(set 'x val)` — `(setq x val)` 相当の関数形式。ANSI CL に含まれるが deprecated
- **内容**: `set symbol value` をビルトイン登録。`(setf (symbol-value symbol) value)` と同義
- **根拠**: ANSI CL 10.2 — set (廃止予定だが標準準拠に必要)
- **難易度**: Easy

#### FR-558: symbol-package

- **対象**: `src/vm/vm.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: シンボルにパッケージ帰属情報なし。`symbol-package` が全コードベースに不在
- **内容**: VM シンボルオブジェクトにパッケージ参照を格納。`symbol-package` をビルトイン登録
- **根拠**: ANSI CL 11.2 — Symbol Package
- **難易度**: Hard (パッケージシステム実装に依存)

---

## 10. パッケージ (ANSI CL Ch.11)

| 機能 | 状態 | 備考 |
|------|------|------|
| `in-package` | 🔶 | FR-437: `(quote name)` を返すスタブ; パイプライン `%prescan-in-package` でレキサー向け副作用のみ |
| `*package*` 変数 | 🔶 | ホスト CL の `*package*` に依存 |
| `defpackage` | 🔶 | FR-437: スタブ (全オプション無視) |
| `find-package` | 🔶 | FR-437: レキサー内部・ホストブリッジのみ; コンパイル済みコードから呼び出し不可 |
| `list-all-packages` | ❌ | FR-559 |
| `make-package` | ❌ | FR-437 |
| `export` | 🔶 | FR-437: スタブ — `(declare (ignore symbols package))` → `nil`; 呼び出し可能だが何もしない (`macros-sequence.lisp:277`) |
| `import` / `unexport` / `shadow` / `shadowing-import` | ❌ | FR-437 |
| `use-package` / `unuse-package` | ❌ | FR-437 |
| `rename-package` / `delete-package` | ❌ | FR-437 |
| `with-package-iterator` | 🔶 | FR-361: 常に枯渇スタブ |
| `do-symbols` / `do-external-symbols` / `do-all-symbols` | ❌ | FR-361 |
| `package-name` / `package-nicknames` | ❌ | FR-438 |
| `package-use-list` / `package-used-by-list` | ❌ | FR-438 |
| `package-shadowing-symbols` | ❌ | FR-438 |

#### FR-559: list-all-packages

- **対象**: `src/vm/vm.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: パッケージレジストリなし
- **内容**: `list-all-packages` — 全パッケージのリストを返す。パッケージ実装 (FR-437) に依存
- **根拠**: ANSI CL 11.2
- **難易度**: Low (FR-437 後)

---

## 11. 数値 (ANSI CL Ch.12)

### 11.1 基本算術・比較

| 演算 | 状態 |
|------|------|
| `+` / `*` (可変引数) | ✅ | `define-list-lowerer` で `ast-binop` に変換後 `vm-add`/`vm-mul` を生成 |
| `-` (2引数以上) | ✅ | |
| `-` (1引数: 単項否定) | ❌ | FR-682: `(- x)` が `reduce-variadic-op` により `x` を返す (否定しない); `expander.lisp:394` の1引数特殊ケース欠如 |
| `-` (0引数) | ❌ | FR-682: `(-)` が `0` を返す (ANSI はエラー要求) |
| `/` | ❌ | FR-661: パーサーが `ast-binop` に変換しない; ビルトイン未登録; `vm-div` 命令は存在するが未使用 |
| `=` / `<` / `>` / `<=` / `>=` (2引数) | 🔶 | FR-663: `(= 1 2 3)` はパース時エラー; `define-list-lowerer` で3引数を拒否 |
| `/=` | ❌ | FR-664: VM命令・ビルトイン・expander ハンドラ全て不在 |
| `zerop` / `plusp` / `minusp` | ✅ |
| `abs` | ✅ |
| `min` / `max` | 🔶 | FR-662: `:binary` 規約 (2,2); `(min a b)` は動作するが `(min a b c)` / `(min a)` は実行時エラー |
| `1+` / `1-` / `incf` / `decf` | 🔶 | FR-693: 複合place (`(incf (aref arr (random 10)))`) でサブフォームを2重評価; `setf` 展開で place を gensym ガードしていない |
| `floor` / `ceiling` / `truncate` / `round` (2値返却) | ✅ |
| `ffloor` / `fceiling` / `ftruncate` / `fround` | ✅ |
| `mod` / `rem` | ✅ |
| `expt` / `sqrt` | ✅ | |
| `isqrt` | 🔶 | FR-683: `(float n)` を使用; 大整数 (> 2^24) で精度喪失; FR-605 (bignum) に依存 |
| `signum` | 🔶 | FR-684: 常に整数 `-1`/`0`/`1` を返す; ANSI は型保存要求 (`(signum 2.5)` → `1.0`) |
| `evenp` / `oddp` | ✅ |
| `gcd` / `lcm` | 🔶 | FR-662: `:binary` 規約 (2,2); `(gcd a b)` は動作するが 0引数・3+引数は実行時エラー (ANSI: 可変引数) |

### 11.2 浮動小数点型塔

cl-cc は内部で全浮動小数点を NaN-boxing double-float に統一。ユーザーコードからの型区別:

| 機能 | 状態 | 備考 |
|------|------|------|
| `floatp` | 🔶 | FR-386: VM 命令未登録 |
| `short-float` / `single-float` / `double-float` / `long-float` 型指定子 | 🔶 | `typep` で部分対応 |
| `short-float-epsilon` / `single-float-epsilon` 等定数 | ❌ | FR-560 |
| `most-positive-short-float` 等定数群 | ❌ | FR-560 |
| `least-positive-short-float` 等定数群 | ❌ | FR-560 |
| `pi` / `most-positive-fixnum` 等 | 🔶 | FR-533: ホスト CL fallback |
| `decode-float` / `scale-float` | ✅ | |
| `integer-decode-float` | ✅ | `builtin-registry-data.lisp:70` |
| `float-digits` / `float-precision` / `float-radix` | ✅ | `builtin-registry-data.lisp:65-68` |
| `float-sign` (1引数) | ✅ | |
| `float-sign` (2引数) | ❌ | FR-685: unary のみ登録; `(float-sign x y)` — x の符号 × y の絶対値 — 未対応 |
| `float` (1引数: 変換) | ✅ | |
| `float` (2引数: `(float x prototype)`) | ❌ | FR-604 |
| `cis` | ❌ | FR-508 |

#### FR-560: 浮動小数点定数群

- **対象**: `src/vm/vm.lisp`
- **現状**: `most-positive-double-float` は `runtime/value.lisp` で使用中だが VM global 未定義
- **内容**: ANSI CL 12.1.6 の全浮動小数点定数を VM global として初期化。`short-float-epsilon`, `single-float-epsilon`, `double-float-epsilon`, `long-float-epsilon` および `most-positive-*`, `least-positive-*` 各系列
- **根拠**: ANSI CL 12.1.6 — Float Constants
- **難易度**: Easy

### 11.3 整数・ビット演算

| 演算 | 状態 |
|------|------|
| `ash` | ✅ |
| `logand` / `logior` / `logxor` / `logeqv` | 🔶 | FR-667: `*binary-builtins*` で2引数固定; `(logand a b c)` / `(logand)` は実行時エラー; `lognot` は別途 ✅ |
| `lognot` | ✅ | |
| `logtest` / `logbitp` / `logcount` / `integer-length` | ✅ |
| `logorc1` / `logorc2` / `logandc1` / `logandc2` | ❌ FR-529 |
| `lognand` / `lognor` | ❌ FR-529 |
| `ldb` / `dpb` / `byte` / `ldb-test` | ❌ FR-492 |
| `byte-size` / `byte-position` | ❌ FR-532 |
| `deposit-field` / `mask-field` | ❌ FR-494 |
| `boole` (+ `boole-*` 定数 16 種) | ❌ FR-493 |

### 11.4 有理数・複素数

| 関数 | 状態 |
|------|------|
| `rational` / `rationalize` | ✅ |
| 比率 (`ratio`) 算術演算 (`/` で自動生成) | ✅ | `vm-rational` (`vm-numeric.lisp:568`) |
| `bignum` (多倍長整数) | ❌ | FR-605 |
| `numerator` / `denominator` | ✅ |
| `realpart` / `imagpart` / `conjugate` / `phase` | ✅ |
| `complex` | ✅ |
| `rationalp` / `complexp` / `realp` | ❌ FR-386 |

### 11.5 超越関数

| 関数 | 状態 |
|------|------|
| `exp` / `log` (1引数) | ✅ |
| `log` (2引数: 任意底) | ❌ FR-476 |
| `sin` / `cos` / `tan` | ✅ |
| `asin` / `acos` / `atan` (1/2引数) | ✅ |
| `sinh` / `cosh` / `tanh` | ✅ |
| `asinh` / `acosh` / `atanh` | ❌ | FR-639: 逆双曲線関数が全コードベースに不在 |

### 11.6 数値パース・変換

| 関数 | 状態 |
|------|------|
| `parse-integer` (基本) | ✅ |
| `parse-integer` (`:start`/`:end`/`:radix`/`:junk-allowed`) | ❌ FR-478 |
| `parse-number` (汎用: 浮動小数点・比率) | ❌ FR-391 |
| `number-to-string` 相当 / `write-to-string` | ✅ |

### 11.7 乱数・時刻

| 関数 | 状態 |
|------|------|
| `random` (1引数) | 🔶 | FR-678: `:unary` 登録のみ; `(random limit state)` 第2引数 random-state 非対応 |
| `make-random-state` | ✅ | |
| `random-state-p` | ❌ FR-509 |
| `*random-state*` | ✅ |
| `get-universal-time` / `get-internal-real-time` / `get-internal-run-time` | ✅ |
| `get-decoded-time` | ❌ | FR-679: `*builtin-nullary-entries*` に未登録; `(decode-universal-time (get-universal-time))` は代替可能 |
| `decode-universal-time` / `encode-universal-time` | ✅ |
| `internal-time-units-per-second` 定数 | ❌ FR-561 |

#### FR-561: 時刻・数値定数

- **対象**: `src/vm/vm.lisp`
- **現状**: `internal-time-units-per-second` / `most-positive-fixnum` / `most-negative-fixnum` が VM global 未定義
- **内容**: VM 起動時に登録。`get-internal-real-time` が正しいスケールで機能するために必要
- **根拠**: ANSI CL 25.1.4 / 12.1
- **難易度**: Easy

---

## 12. 文字 (ANSI CL Ch.13)

| 関数 | 状態 |
|------|------|
| `characterp` | ✅ |
| `char=` / `char<` / `char>` / `char<=` / `char>=` / `char/=` | 🔶 | FR-645: 2引数のみ; ANSI CL は `(char< a b c)` を許可 |
| `char-equal` / `char-lessp` 等 (case-insensitive) | 🔶 | FR-645: 同上、case-insensitive 版も2引数のみ |
| `char-upcase` / `char-downcase` | ✅ |
| `char-code` / `code-char` | ✅ |
| `char-name` / `name-char` | ✅ |
| `char-int` | ✅ |
| `graphic-char-p` / `alpha-char-p` / `alphanumericp` | ✅ |
| `both-case-p` / `upper-case-p` / `lower-case-p` | ✅ |
| `digit-char-p` | 🔶 | FR-668: unary のみ (`(digit-char-p char)`)；`(digit-char-p char radix)` の optional radix 引数未対応 |
| `standard-char-p` | ✅ |
| `digit-char` (1引数) | ✅ |
| `digit-char` (2引数: 基数付き) | ❌ FR-477 |
| 標準文字名 (`#\Space` `#\Newline` `#\Tab` `#\Return`) | ✅ | `lexer.lisp:28` |
| 標準文字名 (`#\Backspace` `#\Page` `#\Rubout` `#\Null` `#\Escape`) | 🔶 | ホスト CL 経由 |
| `base-char` / `extended-char` 型区別 | ❌ FR-392 |
| Unicode 文字 (code-point > 127) | 🔶 FR-562 |

#### FR-562: Unicode サポート

- **対象**: `src/parse/cl/lexer.lisp`, `src/vm/strings.lisp`
- **現状**: レキサーは UTF-8 バイト列として処理するが、`char-code` > 127 のキャラクタオブジェクトの完全な取り扱い未確認
- **内容**: Unicode code point 全域 (U+0000〜U+10FFFF) のキャラクタ表現。`char-code` / `code-char` の 21-bit 対応。UTF-8/UTF-16 ストリームエンコーディング指定
- **根拠**: 2026 年モダン CL — SBCL / CCL は完全 Unicode 対応
- **難易度**: Medium

---

## 13. コンス・リスト (ANSI CL Ch.14)

| 関数 | 状態 |
|------|------|
| `cons` / `car` / `cdr` / `caar`〜`cddddr` | ✅ |
| `first`〜`tenth` / `rest` | 🔶 `first`〜`fifth` のみ確認済み |
| `nth` / `nthcdr` | ✅ |
| `(setf nth)` | ❌ | FR-621: setf 展開未定義 |
| `last` (1引数) | ✅ | |
| `last` (2引数: count) | ❌ | FR-596 |
| `butlast` (1引数) | ✅ | |
| `butlast` (2引数: count) | ❌ | FR-596 |
| `nbutlast` | ❌ | FR-596: 不在確認済み |
| `rplaca` / `rplacd` | ✅ | `list.lisp:158/165`, `builtin-registry-data.lisp:313-314` |
| `endp` / `null` / `listp` / `atom` | ✅ |
| `list` / `make-list` | ✅ |
| `list*` | ❌ | FR-609: ビルトイン未登録 (コンパイラ内部では使用) |
| `copy-list` / `copy-tree` | ✅ |
| `append` / `nconc` | ✅ |
| `nreconc` | ❌ | FR-640: 全コードベースに不在 |
| `reverse` / `nreverse` | ✅ |
| `member` (`:key` / `:test` / `:test-not`) | 🔶 基本のみ |
| `member-if` / `member-if-not` | ❌ FR-499 |
| `assoc` / `rassoc` (基本2引数) | 🔶 | FR-697: `:test`/`:key`/`:test-not` キーワード引数は arity 不一致で失敗; `*binary-builtins*` に分類 (`expander-data.lisp:54`) |
| `acons` / `pairlis` | ✅ | |
| `assoc-if` / `assoc-if-not` | ❌ | FR-660: コンパイラ内部ヘルパーとして `pipeline.lisp:150` に定義されているが、ユーザー向けビルトイン未登録 |
| `rassoc-if` / `rassoc-if-not` | ❌ FR-500 |
| `list-length` | ❌ | FR-656: `vm-list-length` VM命令あり (`list.lisp:290`) だが `builtin-registry-data.lisp` 未登録 |
| `tailp` / `ldiff` | ❌ FR-495 |
| `copy-alist` | ❌ FR-496 |
| `tree-equal` | ❌ FR-496 |
| `subst` | ✅ | `builtin-registry-data.lisp:403` (:ternary-opt-nil-custom) |
| `subst-if` / `subst-if-not` | ❌ | FR-657: `subst` のみ登録。`subst-if` / `subst-if-not` なし |
| `nsubst` / `nsubst-if` / `nsubst-if-not` | ❌ FR-496 |
| `getf` / `remf` | ✅ |
| `get-properties` | ❌ FR-540 |
| `union` / `intersection` / `set-difference` / `subsetp` / `adjoin` | ✅ |
| `nunion` / `nintersection` / `nset-difference` | ❌ FR-547 |
| `sixth` / `seventh` / `eighth` / `ninth` / `tenth` | ❌ FR-563 |
| `push` / `pop` | 🔶 | FR-693: 複合place でサブフォームを2重評価 (例: `(push x (nth (random 10) lst))` でインデックスが2回評価) |
| `pushnew` | ❌ FR-587 |

#### FR-587: pushnew

- **対象**: `src/expand/macros-basic.lisp`
- **現状**: コンパイラ内部コード (`cfg.lisp`, `ssa.lisp`) で使用されているがユーザー向けマクロ未登録
- **内容**: `(pushnew item place &key test key)` — `member` で重複チェック後に `push`
- **根拠**: ANSI CL 14.2.3 — pushnew
- **難易度**: Easy

#### FR-563: sixth〜tenth

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `first`〜`fifth` は実装済みだが `sixth`〜`tenth` が不在
- **内容**: 各アクセサを `nth` + 定数オフセットで実装。ビルトイン登録
- **根拠**: ANSI CL 14.2.1
- **難易度**: Easy

---

## 14. 配列 (ANSI CL Ch.15)

| 機能 | 状態 |
|------|------|
| `make-array` (基本形式) | ✅ | |
| `make-array :initial-contents` | ❌ | FR-654: phase2 ハンドラが `:initial-contents` キーワードを無視 (`codegen-phase2.lisp:86`) |
| `make-array :fill-pointer` / `:adjustable` (真偽値のみ) | 🔶 | FR-687: expander が存在を検出して `make-adjustable-vector` に昇格 (`expander.lisp:232-239`); 実際の値は無視 (`:fill-pointer 5` は `:fill-pointer t` と同じ) |
| `make-array :initial-element` / `:displaced-to` / `:element-type` | ❌ | FR-687: expander で静かに破棄 |
| `aref` (1次元) | 🔶 | FR-686: `:binary-custom` 登録 (1インデックスのみ); `(aref arr 0 1)` 多次元アクセス非対応 |
| `(setf aref)` | ❌ | FR-618: ビルトイン未登録 |
| 多次元配列 (`array-rank` > 1) | 🔶 VM 内部は可だがコンパイラパス未確認 |
| `array-rank` / `array-dimensions` / `array-dimension` | ✅ |
| `array-total-size` / `array-row-major-index` | ✅ |
| `array-in-bounds-p` | ❌ FR-541 |
| `array-element-type` | ❌ FR-564 |
| `adjustable-array-p` / `array-has-fill-pointer-p` | ✅ |
| `fill-pointer` / `(setf fill-pointer)` | ✅ |
| `vector-push` / `vector-push-extend` / `vector-pop` | ✅ |
| `array-displacement` | ✅ |
| `adjust-array` | ✅ | `vm-adjust-array` (`list.lisp:754`), ビルトイン登録済み |
| `vectorp` | ✅ | |
| `simple-vector-p` | ❌ | FR-648: コードベース全体に不在 (`builtin-registry-data.lisp` 未登録) |
| `simple-string-p` / `simple-bit-vector-p` / `bit-vector-p` | ❌ FR-564 |
| `svref` / `row-major-aref` | ✅ |
| `(setf svref)` | ❌ | FR-620: getter のみ登録 (`builtin-registry-data.lisp:196`) |
| `(setf row-major-aref)` | ❌ | FR-620 |
| `vector` (コンストラクタ) | ❌ | FR-651: `(vector 1 2 3)` のような可変引数コンストラクタ不在; `coerce-to-vector` は変換専用 |
| 多次元配列 (`(aref arr i j k)`) | 🔶 | VM 内部可だがコンパイルパス未確認 |
| `make-array :displaced-to` | 🔶 | ホスト CL 経由 |
| ビット配列操作 (`bit-and` / `bit-or` / `bit-xor` / `bit-not`) | ✅ |
| `bit-nor` / `bit-nand` / `bit-eqv` / `bit-andc1` / `bit-andc2` / `bit-orc1` / `bit-orc2` | ❌ | FR-635: ANSI CL 15.2 の残り論理演算 |
| `(setf bit)` / `(setf sbit)` | ❌ | FR-636: 読み取りのみ登録、書き込みなし |
| `upgraded-array-element-type` | ❌ FR-553 |

#### FR-564: array-element-type / simple-array 述語群

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `make-array :element-type` が無視される。`array-element-type` 取得不可
- **内容**: `array-element-type array` — 配列の element-type を返す。`simple-string-p` / `simple-bit-vector-p` / `bit-vector-p` 述語追加
- **根拠**: ANSI CL 15.1.2
- **難易度**: Medium

---

## 15. 文字列 (ANSI CL Ch.16)

| 関数 | 状態 |
|------|------|
| `stringp` / `string` (coerce from symbol/char) | ✅ | `builtin-registry-data.lisp:141`, `list.lisp:379` |
| `make-string n` | ✅ | |
| `make-string n :initial-element char` | ✅ | `strings.lisp:406-420` |
| `make-string n :element-type type` | 🔶 | element-type 無視 |
| `string-upcase` / `string-downcase` / `string-capitalize` | 🔶 | FR-627: `:start`/`:end` キーワード未対応 (unary のみ) |
| `nstring-upcase` / `nstring-downcase` / `nstring-capitalize` | ❌ FR-475 |
| `string-trim` / `string-left-trim` / `string-right-trim` | ✅ |
| `string=` / `string<` / `string>` / `string<=` / `string>=` / `string/=` | 🔶 | FR-669: 戻り値が `nil`/ミスマッチindex の代わりに `0`/`1` boolean; ANSI では `string/=` 等は不一致位置の整数を返す |
| `string-equal` / `string-lessp` 等 (case-insensitive) | 🔶 | FR-669: 同上; 戻り値 semantics 不正 |
| 文字列比較 `:start1`/`:end1`/`:start2`/`:end2` 部分文字列キーワード | ❌ | FR-637: 全 VM 命令が2引数 `(str1 str2)` のみ |
| `char` | ✅ | `builtin-registry-data.lisp:297` |
| `(setf char)` | ❌ | FR-614: ビルトイン未登録 |
| `schar` / `(setf schar)` | 🔶 | 構文レベルで認識 (`parser.lisp:416`) |
| `string-concat` / `concatenate 'string` | ✅ |
| `concatenate 'list` / `concatenate 'vector` (type 指定ディスパッチ) | 🔶 | FR-615: 文字列結合専用実装のみ (`macros-stdlib.lisp:621-629`) |
| `subseq` (文字列) | ✅ |
| `fill` / `replace` (文字列) | ❌ | FR-641: VM命令・ビルトイン不在 |

---

## 16. シーケンス (ANSI CL Ch.17)

| 関数 | 状態 |
|------|------|
| `length` / `elt` | ✅ |
| `(setf elt)` | ✅ | `parser.lisp:419` が `(aset seq i val)` に展開; FR-619 は誤記 |
| `copy-seq` / `subseq` | 🔶 | `copy-seq` は `(copy-list seq)` に展開 — リスト専用; ベクタ・配列非対応 (FR-652) |
| `(setf subseq)` | ❌ | FR-595 |
| `fill` / `replace` / `mismatch` | 🔶 | FR-641: リスト専用 `our-defmacro` 実装 (`macros-sequence.lisp:11/28/50`); `eql` 固定・`:start`/`:end` 無視; ベクタ・文字列は非対応 |
| `map` / `map-into` | ✅ |
| `reduce` | ✅ |
| `count` / `count-if` | 🔶 | `(count item list)` 2引数 eql のみ; `:key/:test/:from-end/:start/:end` 非対応 |
| `count-if-not` | ❌ | FR-610: 全コードベースに不在 |
| `remove` / `remove-if` / `remove-if-not` | ✅ | |
| `remove-duplicates` | 🔶 | `(remove-duplicates list)` 1引数 eql のみ; `:test/:key/:from-end` 非対応 (FR-653) |
| `delete` / `delete-if` / `delete-if-not` | 🔶 | FR-688: `&rest keys` を受け付けるが `(when keys)` は no-op; `:test/:key/:start/:end/:count` が静かに無視される |
| `delete-duplicates` | 🔶 | `&rest keys` を受け付けるが無視; `remove-duplicates` に委譲 (FR-653) |
| `substitute` / `substitute-if` / `substitute-if-not` | 🔶 | FR-688: 同上; `&rest keys` は `(when keys)` no-op パターン; 常に `eql` + 全シーケンスで動作 |
| `nsubstitute` / `nsubstitute-if` / `nsubstitute-if-not` | 🔶 | FR-688: `substitute` に委譲; keyword 無視は同様 |
| `find` / `find-if` | ✅ |
| `find-if-not` | ❌ | FR-610: 全コードベースに不在 |
| `position` | 🔶 | `(position item list)` 2引数 eql のみ; `:key/:test/:from-end/:start/:end` 非対応 |
| `position-if` | ❌ | FR-610 |
| `position-if-not` | ❌ | FR-610 |
| `search` (文字列) | ✅ | `vm-search-string` (`strings.lisp:211`), `builtin-registry-data.lisp:356` |
| `search` (一般シーケンス) | ❌ | FR-588: `search` は文字列専用実装のみ |
| `sort` / `stable-sort` (比較述語のみ) | ✅ |
| `sort` / `stable-sort` `:key` 引数 | ❌ | FR-611: `(sort list pred)` のみ；`(sort list pred :key #'fn)` 未対応 |
| `merge` | ✅ |
| `reverse` / `nreverse` | ✅ |
| `concatenate` / `coerce` (シーケンス間変換) | 🔶 | `concatenate` は `'string` のみ完全 (FR-615) |
| `:key` / `:test` / `:test-not` / `:start` / `:end` / `:from-end` キーワード | 🔶 関数依存 |
| `:count` キーワード (`substitute`/`delete` 等) | 🔶 | 関数依存 |

#### FR-588: search (一般シーケンス)

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `vm-search-string` は文字列専用。リスト・ベクタの部分シーケンス検索なし
- **内容**: `(search sequence1 sequence2 &key from-end test test-not start1 end1 start2 end2 key)` — `sequence2` 内で `sequence1` と一致する先頭位置を返す
- **根拠**: ANSI CL 17.2 — search
- **難易度**: Medium

---

## 17. ハッシュテーブル (ANSI CL Ch.18)

| 関数 | 状態 |
|------|------|
| `make-hash-table` | ✅ |
| `gethash` / `(setf gethash)` | ✅ |
| `remhash` | ✅ |
| `clrhash` | 🔶 | FR-634: `vm-clrhash` 命令あり (`hash.lisp:73`)、ビルトイン未登録 |
| `hash-table-count` | ✅ |
| `hash-table-p` | ✅ |
| `hash-table-test` | ✅ |
| `hash-table-size` | ❌ | FR-616: ビルトイン未登録 |
| `hash-table-rehash-size` / `hash-table-rehash-threshold` | ❌ | FR-616 |
| `maphash` | ❌ | FR-675: VM命令なし; ビルトイン未登録; コンパイラ内部でホスト CL `maphash` を直接使用しているのみ |
| `with-hash-table-iterator` | ❌ FR-497 |
| `sxhash` | ❌ FR-498 |
| `equalp` ハッシュ (:test #'equalp) | 🔶 | FR-565: VM の `resolve-hash-test` は `'equalp` シンボルを ホスト CL `#'equalp` にマップ済み; ただし `#'equalp` はコンパイル済みコードで失敗 (`equalp` 関数は FR-582 で ❌) |

#### FR-565: equalp ハッシュテーブル

- **対象**: `src/vm/hash.lisp`
- **現状**: `hash.lisp:116-123` の `resolve-hash-test` は `equalp` シンボルをホスト CL `#'equalp` にマップ済み。ただし、コンパイル済みコードから `(make-hash-table :test #'equalp)` と書くと `#'equalp` 参照時に「未定義関数: equalp」エラー — `equalp` 関数自体が FR-582 で ❌。`(make-hash-table :test 'equalp)` (クォート形式) なら動作可能
- **内容**: `equalp` による構造的等価をキーとするハッシュテーブル。配列・構造体の内容比較
- **根拠**: ANSI CL 18.1 — Hash Table Test
- **難易度**: Medium

---

## 18. ファイルシステム (ANSI CL Ch.19-20)

### 18.1 パス名操作

| 機能 | 状態 | 備考 |
|------|------|------|
| `make-pathname` | 🔶 | ホスト CL に委譲 |
| `pathname` / `namestring` / `file-namestring` | 🔶 | ホスト CL に委譲 |
| `pathname-name` / `pathname-type` / `pathname-host` / `pathname-device` / `pathname-directory` / `pathname-version` | ❌ | FR-566 |
| `merge-pathnames` | 🔶 | ホスト CL に委譲 |
| `truename` | ❌ | FR-566 |
| `parse-namestring` | ❌ | FR-566 |
| `wild-pathname-p` / `pathname-match-p` / `translate-pathname` | ❌ | FR-566 |
| `logical-pathname` / `logical-pathname-translations` | ❌ | FR-362 |
| `*default-pathname-defaults*` | ❌ | FR-566 |
| `compile-file-pathname` | ❌ | FR-566 |

#### FR-566: パス名 API 完全化

- **対象**: `src/runtime/runtime.lisp`, `src/vm/io.lisp`
- **現状**: パス名アクセサ群・`truename`・`parse-namestring` が全て未実装
- **内容**: ANSI CL 19.2 の全アクセサ。`truename` (実際のファイルシステムパス解決)。`*default-pathname-defaults*` VM global 初期化
- **根拠**: ANSI CL 19.2 — Pathname Functions
- **難易度**: Medium

### 18.2 ファイル操作

| 機能 | 状態 | 備考 |
|------|------|------|
| `open` | 🔶 | FR-628: `vm-open-file` 命令あり、ビルトイン未登録 (`with-open-file` マクロ展開が失敗する) |
| `close` | ✅ | `builtin-registry-data.lisp:287` |
| `open :direction` (`:input`/`:output`/`:io`/`:probe`) | 🔶 | FR-589 |
| `open :if-exists` (`:error`/`:supersede`/`:append`/`:overwrite`/`:rename`/`nil`) | ❌ | FR-589 |
| `open :if-does-not-exist` (`:error`/`:create`/`nil`) | ❌ | FR-589 |
| `open :element-type` (バイナリ/文字ストリーム) | ❌ | FR-589 |
| `open :external-format` | ❌ | FR-562/589 |
| `close :abort` | ❌ | FR-589 |
| `with-open-file` | ✅ | |
| `probe-file` | ❌ FR-479 |
| `rename-file` / `delete-file` | ❌ FR-479 |
| `file-write-date` / `file-author` | ❌ FR-479 |
| `directory` | ❌ FR-479 |
| `ensure-directories-exist` | ❌ FR-479 |
| `load` `:verbose`/`:print`/`:if-does-not-exist`/`:external-format` | ❌ FR-589 |
| `compile-file` `:output-file`/`:verbose`/`:print`/`:external-format` | ❌ FR-589 |
| `compile-file` 3値返却 (output-truename, warnings-p, failure-p) | ❌ FR-589 |

#### FR-589: open/load/compile-file 完全キーワード引数

- **対象**: `src/vm/io.lisp`, `src/compile/pipeline.lisp`
- **現状**: `vm-open-file` がキーワード引数を無視。`:if-exists :append` でファイル追記などが不可
- **内容**: ANSI CL 21.2 の `open` 全キーワード実装。`load` / `compile-file` の制御変数対応。`compile-file` の3値返却
- **根拠**: ANSI CL 21.2.1 / 24.1
- **難易度**: Medium

---

## 19. ストリーム・I/O (ANSI CL Ch.21)

### 19.1 ストリーム生成・管理

| 機能 | 状態 |
|------|------|
| `open` | 🔶 | FR-628 |
| `close` | ✅ |
| `with-open-file` | ✅ |
| `with-open-stream` | ✅ (`macros-stdlib.lisp:47`) |
| `make-string-input-stream` | ❌ | FR-629: ビルトイン未登録 |
| `make-string-output-stream` | ✅ | `builtin-registry-data.lisp:277` |
| `with-input-from-string` (基本形式) | ✅ | |
| `with-input-from-string` `:start`/`:end`/`:index` キーワード | ❌ | FR-608 |
| `with-output-to-string` (基本形式) | ✅ | |
| `with-output-to-string` オプション文字列引数 (`(var string &optional stype)`) | ❌ | FR-613 |
| `get-output-stream-string` | ✅ |
| `make-synonym-stream` | ❌ FR-387 |
| `make-broadcast-stream` / `make-two-way-stream` / `make-echo-stream` | ❌ FR-393 |
| `make-concatenated-stream` | ❌ FR-393 |
| 複合ストリームアクセサ (`broadcast-stream-streams` 等) | ❌ FR-567 |
| Gray Streams プロトコル | ❌ FR-388 |
| `with-standard-io-syntax` | 🔶 | FR-632: スタブのみ (`(progn ,@body)`); 標準変数バインドなし (`macros-stdlib.lisp:285`) |

#### FR-567: 複合ストリームアクセサ

- **対象**: `src/vm/io.lisp`
- **現状**: `make-broadcast-stream` 等の VM 命令なし (FR-393)。アクセサ `broadcast-stream-streams` / `two-way-stream-input-stream` / `two-way-stream-output-stream` / `echo-stream-input-stream` / `echo-stream-output-stream` / `concatenated-stream-streams` が不在
- **内容**: 各複合ストリームの構成ストリームを取得するアクセサ群
- **根拠**: ANSI CL 21.1
- **難易度**: Low (FR-393 後)

### 19.2 文字・バイト I/O

| 関数 | 状態 |
|------|------|
| `read-char` (基本) | ✅ | |
| `unread-char` | 🔶 | FR-671: `:binary-void` 規約 (2引数必須); `(unread-char ch)` はデフォルトストリーム非対応 |
| `peek-char` | ❌ | FR-670: VM命令 `vm-peek-char` は存在するがビルトイン未登録; `(peek-char)` はコンパイル時エラー |
| `read-char` eof-error-p / eof-value 引数 | ❌ | FR-612: hardcoded `nil +eof-value+` (`io.lisp:327`) |
| `read` eof-error-p / eof-value 引数 | ❌ | FR-612: `vm-read-sexp-inst` は dst/src のみ |
| `read-char-no-hang` | ❌ FR-568 |
| `read-sequence` | ❌ FR-590 |
| `write-sequence` | ❌ FR-590 |
| `write-byte` | ✅ | |
| `read-byte` | 🔶 | FR-672b: `:handle-input` 規約 (1引数); `eof-error-p`/`eof-value` hardcoded; FR-612 と同系統 |
| `read-line` | 🔶 | FR-672: `eof-error-p`/`eof-value` hardcoded; 第2戻り値 `missing-newline-p` が常に `nil` (ANSI は2値返却) |
| `write-char` / `write-string` / `write-line` | ✅ |
| `terpri` / `fresh-line` | 🔶 | FR-673: `*builtin-void-side-effect-entries*` で0引数固定; `(terpri *error-output*)` はデフォルトストリームに出力 (引数無視) |
| `finish-output` / `force-output` / `clear-input` | ✅ |
| `clear-output` | ❌ | FR-633: ビルトイン未登録 (`clear-input` は登録済み) |
| `listen` | 🔶 | FR-674: `:handle-input` 規約 (1引数必須); `(listen)` のデフォルトストリーム形式が非対応 |
| `file-position` / `file-length` | ✅ |

#### FR-590: read-sequence / write-sequence

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: バッファ単位の I/O が `read-line` / `write-string` のみ。`read-sequence` / `write-sequence` が全コードベースに不在 (cli/main.lisp は ホスト CL の `read-sequence` を直接使用)
- **内容**: `(read-sequence seq stream &key start end)` — `stream` からシーケンスに読み込む。`(write-sequence seq stream &key start end)` — 逆方向。バイナリ・文字ストリーム両対応
- **根拠**: ANSI CL 21.2 — read-sequence, write-sequence; バイナリファイル処理に必須
- **難易度**: Medium

#### FR-568: read-char-no-hang

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: ノンブロッキング文字読み取りが不在
- **内容**: `read-char-no-hang` — 文字が利用可能なら返却、なければ `nil` を返す。`listen` との連携
- **根拠**: ANSI CL 21.2 — read-char-no-hang
- **難易度**: Low

### 19.3 ストリーム述語・アクセサ

| 関数 | 状態 |
|------|------|
| `streamp` / `input-stream-p` / `output-stream-p` | ✅ |
| `open-stream-p` / `interactive-stream-p` | ✅ |
| `stream-element-type` | ✅ |
| `stream-external-format` | ❌ FR-562 (Unicode 関連) |
| `file-string-length` | ❌ FR-591 |

#### FR-591: file-string-length

- **対象**: `src/vm/io.lisp`
- **現状**: ファイルに文字列/文字を書き込んだ場合のバイト数を調べる手段なし
- **内容**: `(file-string-length stream object)` — `object` (文字または文字列) を `stream` に書いた場合のファイルポジション移動量を返す
- **根拠**: ANSI CL 21.2 — file-string-length
- **難易度**: Easy

### 19.4 標準ストリーム変数

| 変数 | 状態 |
|------|------|
| `*standard-input*` / `*standard-output*` / `*error-output*` | ✅ |
| `*debug-io*` / `*trace-output*` / `*query-io*` | ✅ |
| `*terminal-io*` | ✅ |

---

## 20. プリンタ (ANSI CL Ch.22)

### 20.1 出力関数

| 機能 | 状態 |
|------|------|
| `print` / `prin1` / `princ` | 🔶 | FR-666: オプション `stream` 引数なし (`:side-effect` 規約で1引数固定); `(print x *error-output*)` は実行時エラー |
| `write` (全キーワード引数形式) | ❌ FR-569 |
| `pprint` | ❌ FR-357 |
| `print-object` GF ディスパッチ | 🔶 | FR-390: マクロ部分実装あり |
| `print-unreadable-object` | ✅ |
| `write-to-string` (1引数) | ✅ | |
| `write-to-string` キーワード引数 (`:base`, `:radix`, `:escape` 等) | ❌ | FR-649: `:unary` 登録のため2引数以上でコンパイルエラー |
| `prin1-to-string` / `princ-to-string` | 🔶 | FR-481: 全て同一 `make-vm-write-to-string-inst` に対応; `*print-escape*` 区別なし |

#### FR-569: write 関数 (全キーワード形式)

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `write-to-string` はあるがキーワード引数付き `write` 関数なし
- **内容**: `(write object :stream s :escape t :radix nil :base 10 :circle nil :pretty nil :level nil :length nil :case :upcase :gensym t :array t :readably nil :right-margin nil :miser-width nil :lines nil :pprint-dispatch ...)` の完全実装
- **根拠**: ANSI CL 22.3.1
- **難易度**: Hard (pprint 実装後)

### 20.2 印字制御変数

| 変数 | 状態 |
|------|------|
| `*print-escape*` | 🔶 | FR-646: VM グローバルに初期化済みだが `write-to-string` 等の印字関数が参照しない (デッドコード) |
| `*print-radix*` / `*print-base*` | 🔶 | FR-646: 同上; `(setq *print-base* 16)` しても出力は変わらない |
| `*print-level*` / `*print-length*` | 🔶 | FR-646: 同上 |
| `*print-circle*` | ❌ FR-570 |
| `*print-pretty*` | ❌ FR-357 |
| `*print-gensym*` | ❌ FR-570 |
| `*print-array*` | ❌ FR-535 |
| `*print-readably*` | ❌ FR-535 |
| `*print-right-margin*` / `*print-miser-width*` | ❌ FR-357 |
| `*print-lines*` | ❌ FR-357 |
| `*print-pprint-dispatch*` | ❌ FR-357 |
| `*print-case*` | ❌ FR-570 |

#### FR-570: *print-circle* / *print-gensym* / *print-case*

- **対象**: `src/vm/vm.lisp`, `src/vm/io.lisp`
- **現状**: これら3変数が VM global 未初期化
- **内容**: `*print-circle*` — 循環構造検出・`#n=`/`#n#` 表記。`*print-gensym*` — `#:` プレフィックス制御。`*print-case*` — `:upcase`/`:downcase`/`:capitalize`/`:preserve` シンボル大文字小文字制御
- **根拠**: ANSI CL 22.1 — print control variables
- **難易度**: Medium

### 20.3 FORMAT 指示子

| 指示子 | 状態 | 備考 |
|--------|------|------|
| `~A` / `~S` / `~W` | ✅ | `format.lisp:123` でホスト `format` に委譲; `~W` も含め全て動作 |
| `~D` / `~B` / `~O` / `~X` | ✅ | |
| `~R` (基数/英語/序数) | ✅ | ホスト CL 委譲 |
| `~F` / `~E` / `~G` / `~$` (浮動小数点) | ✅ | ホスト CL 委譲 (FR-571 は自前実装追跡用) |
| `~%` / `~&` / `~|` / `~~` | ✅ | |
| `~T` (タブ) | ✅ | |
| `~P` (複数形) | ✅ | ホスト CL 委譲 |
| `~C` (文字) | ✅ | |
| `~[...~]` (条件) | ✅ | ホスト CL 委譲 |
| `~{...~}` (反復) | ✅ | ホスト CL 委譲 |
| `~<...~>` (正当化) | ✅ | ホスト CL 委譲 |
| `~*` (引数移動) | ✅ | ホスト CL 委譲 |
| `~?` / `~@?` (再帰フォーマット) | ✅ | ホスト CL 委譲 |
| `~/fn/` (ユーザー関数) | 🔶 | FR-694: ホスト CL 名前空間の関数のみ; VM クロージャ (ユーザー定義 `defun`) は `~/fn/` から呼び出せない |
| `~I` / `~:T` / `~_` (pprint 系) | ✅ | ホスト CL 委譲 |
| `(format nil "~A" x)` → string 返却 | ✅ | `format.lisp:123` `(apply #'format nil fmt-str arg-vals)` |
| `format` 自前実装 (ホスト非依存) | ❌ | FR-389: 自己ホスト化後に必要になる |
| `formatter` マクロ | ❌ | FR-698: フォーマット文字列をコンパイル時に関数へ変換; 全コードベースに不在 |

#### FR-571: format 浮動小数点指示子 (~F / ~E / ~G / ~$)

- **対象**: 新規 `src/vm/format.lisp`
- **現状**: 浮動小数点の書式化は全てホスト `format` に委譲
- **内容**: `~F` (固定小数点)、`~E` (指数表記)、`~G` (`~E`/`~F` 自動選択)、`~$` (通貨形式) を自前実装
- **根拠**: ANSI CL 22.3.2 — Floating-Point Directives
- **難易度**: Hard (FR-389 の一部)

---

## 21. リーダー (ANSI CL Ch.23)

| 機能 | 状態 |
|------|------|
| `read` | ✅ | `builtin-registry-data.lisp:161` |
| `read-preserving-whitespace` | ❌ | FR-658: `read` のみ登録; whitespace-preserving 変種なし |
| `read-from-string` (1値: sexp) | ✅ |
| `read-from-string` 2値目 (end-position) | ❌ | FR-617: `io.lisp:744-750` は sexp のみ返却 |
| `read-delimited-list` | ❌ | FR-659: ビルトイン未登録; リーダーマクロ定義用 ANSI 関数 |
| リーダーマクロ `#:` `#+` `#-` `#.` | ✅ |
| `#+` / `#-` 単純フィーチャーキーワード | ✅ | |
| `#+` / `#-` 複合フィーチャー式 (`#+(and x y)` / `#+(or x y)` / `#+(not x)`) | ❌ | FR-594 |
| リーダーマクロ `#'` `#(` `#\` `#|...|#` | ✅ |
| `#b` / `#o` / `#x` (基数) | ✅ | `lexer.lisp:565-585` にネイティブ実装 |
| `#nR` (任意基数: `#12R...`) | ❌ | FR-643: `otherwise` ブランチでエラー (`lexer.lisp:622`) |
| `#C(real imag)` 複素数 | ❌ | FR-644: `otherwise` ブランチでエラー; 「ホスト CL 経由」は誤記 |
| `#n=` / `#n#` (label/reference) | ❌ | FR-599 |
| `#nA` (多次元配列) | ❌ FR-572 |
| `#*` (ビットベクタ) | ❌ FR-572 |
| `#S(...)` (構造体) | ❌ FR-556 |
| `#P(...)` (パス名) | ❌ FR-572 |
| `` ` `` / `,` / `,@` (バッククォート) | ✅ |
| Readtable API (`set-macro-character` 等) | ❌ FR-358 |
| `set-dispatch-macro-character` | ❌ FR-358 |
| `copy-readtable` / `readtablep` / `readtable-case` | ❌ FR-358 |
| `*readtable*` 変数 | ❌ FR-358 |
| `set-syntax-from-char` | ❌ FR-592 |
| `make-dispatch-macro-character` | ❌ FR-592 |
| `get-macro-character` / `get-dispatch-macro-character` | ❌ FR-358 |
| `*read-base*` / `*read-default-float-format*` / `*read-suppress*` | ❌ FR-535 |
| `*read-eval*` | ❌ FR-573 |

#### FR-592: set-syntax-from-char / make-dispatch-macro-character

- **対象**: `src/parse/cl/lexer.lisp` (FR-358 の一部)
- **現状**: Readtable API が一切なく、既存文字構文のコピーも新ディスパッチ文字作成も不可
- **内容**: `set-syntax-from-char to-char from-char &optional to-readtable from-readtable` — 構文クラスをコピー。`make-dispatch-macro-character char &optional non-terminating-p readtable` — 新しいディスパッチマクロ文字を登録
- **根拠**: ANSI CL 23.1.2 — Readtable API
- **難易度**: Medium (FR-358 後)

#### FR-572: 追加リーダーマクロ (#nA / #* / #P)

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#nA` / `#*` / `#P` のディスパッチ文字登録なし
- **内容**: `#nA(...)` — 多次元配列リテラル。`#*01101` — ビットベクタリテラル。`#P"..."` — パス名リテラル
- **根拠**: ANSI CL 2.4.8 — Standard Dispatching Macro Characters
- **難易度**: Medium

#### FR-573: *read-eval*

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#.` が常に評価。`*read-eval*` 変数なし
- **内容**: `*read-eval*` が `nil` のとき `#.` を `print-not-readable` エラーにする
- **根拠**: ANSI CL 2.4.8.6 / セキュリティ
- **難易度**: Easy

---

## 22. システム構成・ロード (ANSI CL Ch.24)

| 機能 | 状態 |
|------|------|
| `provide` | 🔶 | FR-680: `pushnew` (FR-587 ❌) を呼ぶ展開; `pushnew` 未登録のためコンパイル時エラー |
| `require` | 🔶 | FR-680: `pathnames` 引数無視; モジュール不在時 `warn` のみで実ファイルロードなし |
| `*modules*` / `*features*` | 🔶 | FR-642: `vm-state` は `(:common-lisp :cl-cc)`、`vm2-state` は `(:cl-cc)` のみ — `#+common-lisp` が不一致 |
| `with-compilation-unit` | ❌ FR-363 |
| `*load-pathname*` / `*load-truename*` | ❌ FR-574 |
| `*load-verbose*` / `*load-print*` | ❌ FR-574 |
| `*compile-file-pathname*` / `*compile-file-truename*` | ❌ FR-574 |
| `*compile-verbose*` / `*compile-print*` | ❌ FR-574 |
| ASDF 統合 | ✅ (`cl-cc.asd`) |

#### FR-574: ロード・コンパイル制御変数

- **対象**: `src/compile/pipeline.lisp`, `src/vm/vm.lisp`
- **現状**: `our-load` 実行中に `*load-pathname*` 等が設定されない
- **内容**: `load` 実行中に `*load-pathname*` / `*load-truename*` を動的束縛。`compile-file` 中に `*compile-file-pathname*` 等を設定。`*load-verbose*` / `*load-print*` の参照
- **根拠**: ANSI CL 23.1.1 / 24.1
- **難易度**: Easy

---

## 23. 標準 CL グローバル変数

| 変数 | 状態 | 備考 |
|------|------|------|
| `*package*` | 🔶 | ホスト CL に依存 |
| `*standard-input*` / `*standard-output*` / `*error-output*` | ✅ | |
| `*debug-io*` / `*trace-output*` / `*query-io*` / `*terminal-io*` | ✅ | |
| `*read-base*` / `*read-default-float-format*` / `*read-suppress*` | ❌ FR-535 |
| `*read-eval*` | ❌ FR-573 |
| `*readtable*` | ❌ FR-358 |
| `*print-escape*` / `*print-base*` / `*print-radix*` | 🔶 | FR-646: 初期化済みだが印字関数が参照しない |
| `*print-level*` / `*print-length*` | 🔶 | FR-646: 同上 |
| `*print-circle*` / `*print-gensym*` / `*print-case*` | ❌ FR-570 |
| `*print-array*` / `*print-readably*` | ❌ FR-535 |
| `*print-pretty*` / `*print-right-margin*` | ❌ FR-357 |
| `*random-state*` | ✅ | |
| `*gensym-counter*` | ❌ FR-510 |
| `*default-pathname-defaults*` | ❌ FR-566 |
| `*load-pathname*` / `*load-truename*` | ❌ FR-574 |
| `*compile-file-pathname*` / `*compile-file-truename*` | ❌ FR-574 |
| `*load-verbose*` / `*load-print*` | ❌ FR-574 |
| `*compile-verbose*` / `*compile-print*` | ❌ FR-574 |
| `*break-on-signals*` | ❌ FR-557 |
| `*debugger-hook*` | ❌ FR-557 |
| `*macroexpand-hook*` | ❌ FR-429 |
| `*modules*` / `*features*` | 🔶 | FR-642: `vm-state` は `(:common-lisp :cl-cc)`、`vm2-state` は `(:cl-cc)` のみ — `#+common-lisp` が不一致 | |

---

## 24. 宣言・コンパイラポリシー (ANSI CL Ch.3.3)

| 機能 | 状態 |
|------|------|
| `declare` (基本型宣言) | ✅ |
| `declaim` | 🔶 FR-396: `pipeline.lisp` でスキップ |
| `proclaim` | ❌ | FR-700: 全コードベースに不在; `declaim` のマクロ展開形である関数形式が未実装 |
| `declare (optimize ...)` | 🔶 FR-395: 黙って無視 |
| `declare (type ...)` | 🔶 | 型推論パスには影響あり |
| `declare (ignore ...)` / `(ignorable ...)` | 🔶 | |
| `declare (inline ...)` / `(notinline ...)` | ❌ FR-396 |
| `declare (ftype ...)` | 🔶 FR-127 (部分) |
| `declare (dynamic-extent ...)` | ❌ FR-575 |
| `declare (special ...)` | 🔶 | |
| `*compiler-policy*` | ❌ FR-395 |
| 環境オブジェクト (`variable-information` 等, CLtL2 8.5) | ❌ FR-394 |
| `locally` 宣言伝播 | ❌ FR-397 |
| `compiler-let` | ❌ FR-439 |

#### FR-575: dynamic-extent 宣言

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **現状**: `dynamic-extent` 宣言が無視される。クロージャ・リスト・ベクタのスタック割り当て最適化なし
- **内容**: `(declare (dynamic-extent var))` — スコープを抜けた時点で回収可能とマーク。クロージャのスタック割り当て (escape analysis と連携)
- **根拠**: ANSI CL 3.3.4 — dynamic-extent; GC 圧力削減
- **難易度**: Very Hard

---

## 25. 環境・開発ツール (ANSI CL Ch.25)

### 25.1 デバッグ・プロファイリング

| 機能 | 状態 |
|------|------|
| `sleep` | ❌ | FR-681: 全コードベースに不在; `vm-sleep` 命令なし |
| `time` | ❌ FR-431 |
| `room` | ❌ FR-434 |
| `trace` / `untrace` | ❌ FR-432 |
| `step` | ❌ FR-433 |
| `break` | ❌ FR-557 |
| `invoke-debugger` / `*debugger-hook*` | ❌ FR-557 |
| `*break-on-signals*` | ❌ FR-557 |
| `disassemble` | ❌ FR-576 |
| `dribble` | ❌ FR-514 |

#### FR-576: disassemble

- **対象**: `src/cli/main.lisp`, `src/compile/pipeline.lisp`
- **現状**: コンパイラが VM 命令列を生成するが、ユーザーが関数の VM 命令を確認する手段なし
- **内容**: `(disassemble fn)` — VM 命令列を人間可読形式で出力。デバッグ・最適化確認に不可欠
- **根拠**: ANSI CL 25.1.4 — disassemble
- **難易度**: Medium

### 25.2 インスペクション・ドキュメント

| 機能 | 状態 |
|------|------|
| `describe` / `describe-object` | 🔶 スタブ |
| `inspect` | ❌ FR-577 |
| `documentation` / `(setf documentation)` | ❌ FR-436 |
| `apropos` / `apropos-list` | ❌ FR-435 |
| `ed` | ❌ FR-515 |

#### FR-577: inspect

- **対象**: `src/cli/main.lisp`
- **現状**: `inspect` が全コードベースに不在
- **内容**: `(inspect object)` — オブジェクトの内部構造を再帰的に閲覧するインタラクティブツール。スロット・コンポーネント表示と編集
- **根拠**: ANSI CL 25.1.4
- **難易度**: Medium

### 25.3 ユーザーインタラクション

| 機能 | 状態 |
|------|------|
| `y-or-n-p` / `yes-or-no-p` | ❌ FR-578 |

#### FR-578: y-or-n-p / yes-or-no-p

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: 両関数が不在
- **内容**: `y-or-n-p` — "y" または "n" を受け付ける。`yes-or-no-p` — "yes" / "no" フルスペル要求。`*query-io*` ストリームを使用
- **根拠**: ANSI CL 25.1.2
- **難易度**: Easy

### 25.4 環境照会

| 機能 | 状態 |
|------|------|
| `lisp-implementation-type` / `lisp-implementation-version` | ❌ FR-507 |
| `machine-type` / `machine-version` / `machine-instance` | ❌ FR-507 |
| `software-type` / `software-version` | ❌ FR-507 |
| `short-site-name` / `long-site-name` | ❌ FR-507 |
| `compiled-function-p` | ❌ FR-513 |

---

## 26. モダン拡張 (2026年コンパイラ標準)

ANSI CL 外だが 2026 年のモダンな CL 実装が提供する機能。

### 26.1 継続・コルーチン

| 機能 | 状態 |
|------|------|
| Tail Call Optimization (TCO) | ✅ CPS変換で実現 |
| Delimited Continuations (`shift`/`reset`) | ❌ FR-221 |
| Coroutines / Generators (`yield`/`resume`) | ❌ FR-222 |

### 26.2 Unicode・文字エンコーディング

| 機能 | 状態 |
|------|------|
| Unicode 文字 (U+0000〜U+10FFFF) | 🔶 FR-562 |
| UTF-8 外部フォーマット指定 (`open :external-format :utf-8`) | ❌ FR-562 |
| `string-to-octets` / `octets-to-string` (Babel 相当) | ❌ FR-579 |

#### FR-579: バイト列・文字列変換

- **対象**: `src/vm/strings.lisp`
- **現状**: 文字列のバイト列変換手段なし
- **内容**: `string-to-octets string &key encoding` / `octets-to-string octets &key encoding` の非 ANSI 標準だが事実上必須な関数
- **根拠**: 2026 年モダン CL — Babel ライブラリ相当
- **難易度**: Medium

### 26.3 並行処理

詳細は [concurrency.md](concurrency.md) 参照。

| 機能 | 状態 |
|------|------|
| Bordeaux Threads 互換 API | ❌ |
| `bt:make-thread` / `bt:join-thread` | ❌ |
| `bt:make-lock` / `bt:with-lock-held` | ❌ |
| `bt:make-condition-variable` | ❌ |
| Atomic operations | ❌ |

### 26.4 FFI (Foreign Function Interface)

| 機能 | 状態 |
|------|------|
| CFFI 互換 API | ❌ FR-580 |
| `cffi:define-foreign-library` / `cffi:load-foreign-library` | ❌ FR-580 |
| `cffi:defcfun` / `cffi:defcstruct` | ❌ FR-580 |
| `cffi:with-foreign-objects` / `cffi:mem-ref` | ❌ FR-580 |

#### FR-580: FFI / CFFI 互換層

- **対象**: 新規 `src/ffi/`
- **現状**: 外部 C ライブラリ呼び出し手段なし
- **内容**: CFFI に準拠したFFI API。`dlopen`/`dlsym` によるシンボル解決。C 型と CL 型の変換レイヤー
- **根拠**: 2026 年モダン CL — 事実上標準の FFI API
- **難易度**: Very Hard

### 26.5 オブジェクトシステム

| 機能 | 状態 |
|------|------|
| Gray Streams | ❌ FR-388 |
| MOP (Metaobject Protocol) | ❌ FR-523〜528 (clos.md) |
| `defstruct` ↔ CLOS 統合 (`structure-class`) | ❌ FR-528 (clos.md) |

### 26.6 コンパイラ品質

| 機能 | 状態 |
|------|------|
| NaN-boxing タグチェック融合 | ❌ FR-364 |
| Compiler Environment Objects (CLtL2 Ch.8.5) | ❌ FR-394 |
| `define-compiler-macro` 完全動作 | ❌ FR-365 |
| Inline expansion | ❌ FR-396 |
| `dynamic-extent` による escape analysis | ❌ FR-575 |

---

---

## 27. 追加 FR エントリ (FR-593〜FR-602)

#### FR-593: (setf subseq)

- **対象**: `src/vm/list.lisp`, `src/expand/expander.lisp`
- **現状**: `subseq` は読み取り専用。`(setf (subseq s 0 3) "abc")` が失敗
- **内容**: `(setf (subseq seq start end) new-seq)` — `replace` で実装可。`expander.lisp` の setf ハンドラに追加
- **根拠**: ANSI CL 17.3 — setf of subseq
- **難易度**: Easy

#### FR-594: #+/#- 複合フィーチャー式

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#+:sbcl` のような単純フィーチャーは動作するが `#+(and :sbcl :x86-64)` / `#+(or :sbcl :ccl)` / `#+(not :sbcl)` が未対応
- **内容**: フィーチャー式の再帰パーサを追加。`and`/`or`/`not` による合成フィーチャー条件を `*features*` と照合
- **根拠**: ANSI CL 24.1.2 — Feature Expressions
- **難易度**: Low

#### FR-595: (setf subseq) — 既 FR-593 に統合

#### FR-596: last / butlast / nbutlast の count 引数

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `last` / `butlast` はどちらも unary 登録 (`builtin-registry-data.lisp:35,40`)。count 引数形式なし。`nbutlast` は全コードベースに不在
- **内容**: `(last list n)` → 末尾 n 個の cons cell を返す。`(butlast list n)` → 末尾 n 個を除いたリスト。`nbutlast` → 破壊的版
- **根拠**: ANSI CL 14.2.6
- **難易度**: Easy

#### FR-597: identity / constantly / complement ビルトイン登録

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `pipeline.lisp:162-164` のプリルードで `defun` として定義済み。しかしビルトイン登録がないため `(identity x)` が `vm-call` 経由になりインライン化されない
- **内容**: ビルトイン登録 + `(identity x)` → noop コード生成。`(complement pred)` → `(lambda (x) (not (funcall pred x)))`
- **根拠**: ANSI CL 5.1 — function utilities
- **難易度**: Low

#### FR-598: ストリーム型指定子 (typep 対応)

- **対象**: `src/vm/primitives.lisp` の `vm-typep-check`
- **現状**: `vm-typep-check` で `stream` / `input-stream` / `output-stream` 等の型指定子が未対応。`(typep s 'file-stream)` が失敗
- **内容**: `string-stream` / `file-stream` / `broadcast-stream` / `two-way-stream` / `echo-stream` / `concatenated-stream` / `synonym-stream` 型判定を追加
- **根拠**: ANSI CL 21.1 — Stream Types
- **難易度**: Easy

#### FR-599: #n= / #n# ラベル読み取り構文 (循環構造)

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#n=` / `#n#` が未実装。`*print-circle*` (FR-570) と対になる機能
- **内容**: `#1=(a #1#)` のような循環構造のリード/プリント。`*read-eval*` と `*print-circle*` の両実装後に必要
- **根拠**: ANSI CL 2.4.8.15/16
- **難易度**: Hard

#### FR-600: defvar / defparameter / defconstant 完全セマンティクス

- **対象**: `src/expand/expander.lisp`, `src/vm/vm.lisp`
- **現状**: `defvar` は **確認済みで破綻**: `compile-ast ast-defvar` (`codegen.lisp:284-298`) が常に `vm-set-global` を emit — 既束縛チェックなし。`defconstant` は `expander.lisp:406-407` で `(compiler-macroexpand-all \`(defparameter ...))` に翻訳されるため、不変性強制・再定義エラーなし。`defparameter` の「常に設定」は正しい動作
- **内容**: `defvar` — 未束縛時のみ初期値設定。`defconstant` — 再定義時の `error` または `cerror`。`defparameter` — 常に設定
- **根拠**: ANSI CL 3.8.1
- **難易度**: Low

#### FR-601: multiple-value-bind 値数不足時の挙動

- **対象**: `src/compile/codegen.lisp`
- **現状**: `(multiple-value-bind (a b c) (values 1 2) ...)` で `c` が未定義になるか `nil` になるか不明
- **内容**: ANSI CL 仕様: 値が足りない場合は `nil` で補填。値が多い場合は余剰を捨てる
- **根拠**: ANSI CL 5.3 — multiple-value-bind semantics
- **難易度**: Low

#### FR-602: (values) 0値返却の明示的サポート

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **現状**: `(values)` で 0 値を返す関数のコンパイルが正しく機能するか未確認
- **内容**: `(values)` は0個の値を返す。`multiple-value-bind` / `multiple-value-list` / `nth-value` との組み合わせ全パターン
- **根拠**: ANSI CL 5.3 — values with no arguments
- **難易度**: Low

#### FR-603: (setf (values a b ...) expr) — values を場所として使用

- **対象**: `src/expand/expander.lisp` (`*setf-compound-place-handlers*`)
- **現状**: `setf` のコンパウンド場所ハンドラに `values` が未登録。`(setf (values a b) (floor x y))` はエラー
- **内容**: `(setf (values place1 place2 ...) expr)` → `(multiple-value-bind (tmp1 tmp2 ...) expr (setf place1 tmp1) (setf place2 tmp2) ...)` に展開。`multiple-value-setq` が単純変数限定なのに対しこちらは任意 setf 場所に対応
- **根拠**: ANSI CL 5.1.2.3 — VALUES as a Place
- **難易度**: Medium

#### FR-604: float 2引数形式 (プロトタイプ指定)

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/vm-numeric.lisp`
- **現状**: `float` は unary 登録のみ。`(float 3 1.0d0)` のプロトタイプ浮動小数点型への変換形式なし
- **内容**: `(float number prototype)` — `prototype` と同型の浮動小数点に変換。NaN-boxing double-float 統一なら実質 no-op だが ANSI 準拠には必要
- **根拠**: ANSI CL 12.1.3.3 — float (number &optional prototype)
- **難易度**: Easy

#### FR-605: bignum (多倍長整数)

- **対象**: `src/vm/vm-numeric.lisp`, `src/vm/vm.lisp` (NaN-boxing 値表現)
- **現状**: VM は NaN-boxing で 53bit 整数 (float mantissa) のみ保持。`(expt 2 64)` 等の大整数演算が精度消失
- **内容**: bignum タグを追加してホスト CL integer オブジェクトをヒープに保持。算術演算でオーバーフロー検出→bignum に昇格。`bignump` 述語。`integer-length` / `integer-decode-float` との統合
- **根拠**: ANSI CL 12.1.1 — numeric tower; fixnum/bignum 区別
- **難易度**: Very Hard (値表現の根幹変更)

#### FR-606: assert の place-list (場所付きリスタート)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `assert` の基本シグナル (`simple-error`) は実装済み。ANSI CL が定義する第3引数 (place リスト) によるインタラクティブ修正機能なし
- **内容**: `(assert test-form (place1 place2 ...) ...)` — 失敗時に `continue` リスタートで各 place の値を対話的に修正して再試行。`restart-case` と `store-value` リスタート (FR-421) に依存
- **根拠**: ANSI CL 9.1.3 — assert with places
- **難易度**: Medium (FR-421 後)

#### FR-607: defun / defmacro ドキュメント文字列

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`, `src/vm/vm.lisp`
- **現状**: `defun` / `defmacro` の本体先頭にある文字列リテラルを破棄してコンパイル続行。`(documentation 'fn 'function)` は ❌ (FR-436)
- **内容**: コンパイル時に docstring を検出し VM 関数オブジェクトのメタデータとして保存。`(documentation 'fn 'function)` / `(documentation 'mac 'macro)` で取得可能に。`define-condition` / `defclass` / `defmethod` / `defpackage` ドキュメント文字列も同様
- **根拠**: ANSI CL 5.4.1 — Documentation Strings
- **難易度**: Medium (FR-436 に先立つ基盤)

#### FR-609: list* ユーザー向けビルトイン登録

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `list*` はコンパイラ内部 (CPS変換・AST変換) で多用されているがユーザーコードのビルトイン登録がない。`(list* 1 2 '(3))` → `(1 2 3)` が機能しない
- **内容**: `(list* x1 x2 ... rest)` — 最後の引数にドットペアを生成する可変引数関数。`builtin-registry-data.lisp` に variadic 登録
- **根拠**: ANSI CL 14.2.1 — list*
- **難易度**: Easy

#### FR-610: -if-not 系シーケンス述語

- **対象**: `src/expand/macros-sequence.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `find-if-not` / `position-if-not` / `count-if-not` が全コードベースに不在。ANSI CL 17.2 が義務付ける述語の否定版
- **内容**: 各関数は `(funcall pred x)` を `(not (funcall pred x))` に変えたラッパーとして実装可能
- **根拠**: ANSI CL 17.2 — sequence functions with -not variants
- **難易度**: Easy

#### FR-611: sort / stable-sort :key 引数

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `sort` / `stable-sort` は `(list predicate)` のみ。`(sort list pred :key #'car)` が展開時にエラー
- **内容**: `:key` キーワードを受け取り、各要素比較前に `(funcall key elem)` でキー抽出
- **根拠**: ANSI CL 17.2 — sort :key argument
- **難易度**: Easy

#### FR-612: read / read-char eof-error-p / eof-value 引数

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `vm-read-sexp-inst` は dst/src スロットのみ。`vm-read-char` は `(read-char stream nil +eof-value+)` とハードコードで、ユーザーが EOF 動作を制御できない
- **内容**: `(read stream eof-error-p eof-value recursive-p)` / `(read-char stream eof-error-p eof-value recursive-p)` — 4引数形式を完全サポート。eof-error-p = nil のとき EOF で eof-value を返す
- **根拠**: ANSI CL 23.2 / 21.2 — read, read-char eof handling
- **難易度**: Medium

#### FR-613: with-output-to-string オプション文字列引数

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `with-output-to-string` は新しい文字列出力ストリームを生成して返すのみ。ANSI CL が定義する既存の `string` 引数への追記形式なし
- **内容**: `(with-output-to-string (var string &optional element-type) body)` — `string` が提供された場合、既存の fill-pointer 付き文字列に追記するストリームを生成
- **根拠**: ANSI CL 21.2 — with-output-to-string with string argument
- **難易度**: Low (fill-pointer 対応文字列実装に依存)

#### FR-608: with-input-from-string キーワード引数

- **対象**: `src/expand/macros-stdlib.lisp` または `src/vm/io.lisp`
- **現状**: `with-input-from-string` 基本形式は動作するが `:start` / `:end` (入力範囲制限) と `:index` (終端位置書き戻し) キーワードが未対応
- **内容**: `(with-input-from-string (var string :start s :end e :index idx-place) body)` — `string` の `[s, e)` 範囲のみを読み込むストリームを生成。終了時に実際の読み取り位置を `idx-place` に `setf`
- **根拠**: ANSI CL 21.2 — with-input-from-string
- **難易度**: Low

#### FR-614: (setf char) — 文字列の破壊的文字置換

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/strings.lisp`
- **現状**: `char` は読み取り用ビルトイン登録済みだが `(setf char)` が未登録。`(setf (char str i) c)` でエラー
- **内容**: `(setf (char string index) char)` — 文字列の i 番目の文字を破壊的に置換。`(setf schar)` も同様
- **根拠**: ANSI CL 16.2 — setf of char
- **難易度**: Easy

#### FR-615: concatenate 型ディスパッチ完全化

- **対象**: `src/expand/macros-stdlib.lisp:621-629`
- **現状**: `(concatenate 'string ...)` のみ動作。`(concatenate 'list seq1 seq2)` / `(concatenate 'vector ...)` が機能しない。`coerce` との組み合わせも未確認
- **内容**: result-type 引数で `'string` / `'list` / `'vector` を分岐。各型に対応する結合ロジック
- **根拠**: ANSI CL 17.2.2 — concatenate
- **難易度**: Easy

#### FR-616: hash-table-size / hash-table-rehash-size / hash-table-rehash-threshold

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/hash.lisp`
- **現状**: `hash-table-test` は登録済みだが `hash-table-size` / `hash-table-rehash-size` / `hash-table-rehash-threshold` が不在
- **内容**: ハッシュテーブルのパラメータアクセサ群をビルトイン登録。VM のハッシュテーブル表現 (CL ネイティブ HT) が持つ情報を返す
- **根拠**: ANSI CL 18.2 — hash table accessors
- **難易度**: Easy

#### FR-617: read-from-string 2値目 (end-position)

- **対象**: `src/vm/io.lisp:744-750`
- **現状**: `vm-read-from-string-inst` は sexp のみ返却。`(read-from-string "123 abc" nil nil :start 4)` での end-position (7) が取得できない
- **内容**: `(read-from-string string eof-error-p eof-value :start s :end e :preserve-whitespace p)` → `(values sexp end-pos)` の2値返却。:start/:end/:preserve-whitespace キーワードも完全対応
- **根拠**: ANSI CL 23.2 — read-from-string
- **難易度**: Medium

#### FR-618: (setf aref) — 配列要素の破壊的書き込み

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/list.lisp`
- **現状**: `aref` は読み取り専用ビルトイン登録済み。`(setf (aref arr i) val)` が失敗
- **内容**: `(setf (aref array index) value)` — 1次元配列への書き込み。多次元 `(setf (aref arr i j) val)` も対応が必要
- **根拠**: ANSI CL 15.2 — setf of aref
- **難易度**: Easy

#### FR-619: (setf elt) — 実装済みと判明 (2026-03-27 修正)

- **状態**: ✅ 実装済み (FR-619 は誤記)
- **根拠**: `src/parse/cl/parser.lisp:418-419` — `(setf (elt seq i) val)` をパーサーが `(aset seq i val)` に展開。`aset` は `builtin-registry-data.lisp:407` に登録済み

#### FR-620: (setf svref) / (setf row-major-aref)

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `svref` (`builtin-registry-data.lisp:196`) / `row-major-aref` (`builtin-registry-data.lisp:195`) は読み取り専用登録。setf 展開なし
- **内容**: `(setf (svref vector i) val)` — simple-vector への書き込み。`(setf (row-major-aref array i) val)` — row-major インデックスへの書き込み
- **根拠**: ANSI CL 15.2 — setf of svref, row-major-aref
- **難易度**: Easy (FR-618 と同パターン)

#### FR-621: (setf nth)

- **対象**: `src/expand/expander.lisp` (`*setf-compound-place-handlers*`)
- **現状**: `car`/`cdr` の setf は `rplaca`/`rplacd` に展開済み (`expander.lisp:214-227`) だが `nth` の setf 展開が未定義
- **内容**: `(setf (nth n list) val)` → `(rplaca (nthcdr n list) val)` に展開。`*setf-compound-place-handlers*` に `nth` エントリを追加
- **根拠**: ANSI CL 14.2.1 — setf of nth
- **難易度**: Easy

#### FR-622: (setf get)

- **対象**: `src/expand/expander.lisp`
- **現状**: `get` (symbol plist アクセス) は読み取りビルトイン登録済みだが `(setf (get sym key) val)` の展開なし
- **内容**: `(setf (get symbol key) value)` → `%plist-put` または内部 setf で実装
- **根拠**: ANSI CL 10.2 — setf of get
- **難易度**: Easy

#### FR-623: let / flet 空バインディング形式

- **対象**: `src/parse/cl/parser.lisp`
- **現状**: `parser.lisp` の `let` パーサーが `(>= (length node) 3)` を要求 → `(let () body)` が構文エラーになる。`flet` も同様 (line 451)
- **内容**: `(let () body)` / `(flet () body)` を ANSI CL 仕様通り空バインディングリストとして受け入れ、`progn` に等価展開
- **根拠**: ANSI CL 3.1.2.1 — special operators with empty binding lists
- **難易度**: Easy

#### FR-624: subtypep

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `src/` 全体に `subtypep` の定義・登録が一切存在しない (`grep -r subtypep src/` → 0 件)
- **内容**: `(subtypep type1 type2 &optional environment)` — 2つの型指定子の包含関係を `(values result certainty)` で返す。`typep` が実装済みなので基本的な原子型は実現可能
- **根拠**: ANSI CL 4.2.2 — subtypep
- **難易度**: Medium (合成型指定子サポートは Hard)

#### FR-625: type-of — float / function 型返却

- **対象**: `src/vm/primitives.lisp` (lines 414-428)
- **現状**: `type-of` は `integer`/`string`/`symbol`/`list`/`vector`/`hash-table`/`t` を返す。`float` は `t` にフォールスルー、`function`/`vm-closure`/`vm-builtin-fn` は未処理
- **内容**: `float` → `single-float` または `double-float`。`vm-closure`/`vm-builtin-fn` オブジェクト → `function`。`nil` → `null`
- **根拠**: ANSI CL 4.2.6 — type-of
- **難易度**: Easy

#### FR-626: error / warn — フォーマット制御形式

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/conditions.lisp`
- **現状**: `error` は `make-vm-signal-error :error-reg` 単項登録。`(error "~A" x)` のような文字列+引数の多引数形式が未対応
- **内容**: コードジェネレータ (または展開器) で `(error string &rest args)` を `(error (format nil string args...))` にデシュガーして単一コンディションオブジェクトとして渡す。`warn` も同様
- **根拠**: ANSI CL 9.1.2 — error function signature
- **難易度**: Easy

#### FR-627: string-upcase / string-downcase / string-capitalize — :start/:end キーワード

- **対象**: `src/vm/strings.lisp` (lines 164-182), `src/compile/builtin-registry-data.lisp` (lines 16-18)
- **現状**: `define-simple-instruction` による unary 登録のみ。`(string-upcase str :start 2 :end 5)` はキーワード引数が無視される
- **内容**: `:start`/`:end` パラメータを受け取り部分文字列変換を実装。`nstring-upcase`/`nstring-downcase`/`nstring-capitalize` (FR-475) の破壊的版も同様
- **根拠**: ANSI CL 16.2.1 — string-upcase
- **難易度**: Easy

#### FR-628: open — ビルトイン未登録

- **対象**: `src/vm/io.lisp` (line 27: `vm-open-file`), `src/compile/builtin-registry-data.lisp`
- **現状**: `vm-open-file` VM命令が実装済みだが `builtin-registry-data.lisp` に登録なし。`with-open-file` マクロが `(open ...)` に展開されるためコンパイル失敗
- **内容**: `close` (line 287) と同パターンで `(open . make-vm-open-file)` を登録
- **根拠**: ANSI CL 21.2.1 — open
- **難易度**: Easy (登録のみ)

#### FR-629: make-string-input-stream — ビルトイン未登録

- **対象**: `src/vm/io.lisp` (line 413: 内部使用は存在), `src/compile/builtin-registry-data.lisp`
- **現状**: `make-string-output-stream` は `builtin-registry-data.lisp:277` に登録済み。`make-string-input-stream` は未登録
- **内容**: `vm-make-string-input-stream` VM命令を定義し登録。`with-input-from-string` マクロが依存
- **根拠**: ANSI CL 21.3.5 — make-string-input-stream
- **難易度**: Easy

#### FR-630: coerce — 実行時型ディスパッチ

- **対象**: `src/expand/macros-sequence.lisp` (lines 375-390)
- **現状**: `coerce` はマクロとして実装。クォート済みリテラル `'list`/`'vector`/`'string` のみ対応。`(coerce x type-var)` のような実行時型変数は `coerce-to-string` にフォールスルーし誤動作
- **内容**: 実行時型ディスパッチ関数を追加。または `coerce-to-*` 群を呼ぶランタイム関数 `rt-coerce` を `builtin-registry-data.lisp` に登録
- **根拠**: ANSI CL 12.1 — coerce with computed type
- **難易度**: Medium

#### FR-631: macroexpand / macroexpand-1 — ユーザー呼び出し可能化

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/expand/macro.lisp`
- **現状**: `our-macroexpand-1` / `our-macroexpand-all` はコンパイラ内部関数として実装済みだが、`macroexpand` / `macroexpand-1` としてビルトイン未登録
- **内容**: `builtin-registry-data.lisp` に登録し、ユーザーコードから `(macroexpand '(when t :ok))` が呼べるようにする
- **根拠**: ANSI CL 3.1.2.1 — macroexpand
- **難易度**: Easy

#### FR-632: with-standard-io-syntax — 標準変数バインド

- **対象**: `src/expand/macros-stdlib.lisp` (line 285)
- **現状**: `(with-standard-io-syntax ,@body)` → `(progn ,@body)` のスタブ。FR-210 で追跡済み
- **内容**: ANSI CL が指定する `*print-base*`/`*print-escape*`/`*print-readably*`/`*print-*` 全変数を標準値にバインドして body を実行
- **根拠**: ANSI CL 22.1.1 — with-standard-io-syntax
- **難易度**: Medium (プリンタ変数の実装に依存)

#### FR-633: clear-output — ビルトイン未登録

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/io.lisp`
- **現状**: `clear-input` (line 384) は登録済みだが `clear-output` が不在。`force-output`/`finish-output` は登録済み
- **内容**: `vm-clear-output` 命令を定義し登録。出力バッファを破棄
- **根拠**: ANSI CL 21.2 — clear-output
- **難易度**: Easy

#### FR-634: clrhash — ビルトイン未登録

- **対象**: `src/vm/hash.lisp` (line 73: `vm-clrhash`), `src/compile/builtin-registry-data.lisp`
- **現状**: `vm-clrhash` VM命令が実装済みだが `builtin-registry-data.lisp` に登録なし。`remhash` (line 319) は登録済み
- **内容**: `remhash` と同パターンで `clrhash` を登録
- **根拠**: ANSI CL 18.1 — clrhash
- **難易度**: Easy (登録のみ)

#### FR-635: bit-nor / bit-nand / bit-eqv / bit-andc1 / bit-andc2 / bit-orc1 / bit-orc2

- **対象**: `src/vm/primitives.lisp` または `src/vm/list.lisp`
- **現状**: `bit-and`/`bit-or`/`bit-xor`/`bit-not` のみ実装。ANSI CL 15.2 の7種の追加論理演算が不在
- **内容**: 各演算をビット配列要素ごとに適用する VM 命令を追加登録
- **根拠**: ANSI CL 15.2 — bit logical operations
- **難易度**: Easy

#### FR-636: (setf bit) / (setf sbit)

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `bit` (line 298) と `sbit` (line 299) は読み取り登録済み。書き込み `(setf (bit vec i) v)` / `(setf (sbit vec i) v)` は未登録
- **内容**: `setf` 展開器に `bit`/`sbit` の place 展開を追加
- **根拠**: ANSI CL 15.2 — (setf bit)
- **難易度**: Easy

#### FR-637: 文字列比較 — :start1/:end1/:start2/:end2 部分文字列キーワード

- **対象**: `src/vm/strings.lisp` (string comparison VM instructions)
- **現状**: 全文字列比較 VM 命令 (`vm-string-equal` 等) が `(dst str1 str2)` 2引数のみ。`(string= s1 s2 :start1 2 :end1 5)` のような部分文字列比較が不可
- **内容**: `:start1`/`:end1`/`:start2`/`:end2` を受け取るよう各命令のスロットとエグゼキュータを拡張。`subseq` で代替可能だが ANSI 準拠には必要
- **根拠**: ANSI CL 16.2.3 — string comparisons with keyword arguments
- **難易度**: Medium (全比較命令の修正が必要)

#### FR-638: loop named — 名前付きループ

- **対象**: `src/expand/loop-parser.lisp`, `src/expand/loop-emitters.lisp`
- **現状**: `loop` パーサーに `named` キーワード処理がない。`loop named foo ... (return-from foo val)` がパースエラー
- **内容**: ループ名を `loop-state` に保存し、`return-from` に対応する `catch`/`throw` を生成
- **根拠**: ANSI CL 6.1.1 — named loop
- **難易度**: Medium

#### FR-639: asinh / acosh / atanh — 逆双曲線関数

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `sinh`/`cosh`/`tanh` は実装済み (lines 60-62)。逆関数 3種が全コードベースに不在
- **内容**: CL の `asinh`/`acosh`/`atanh` を VM命令として追加し登録
- **根拠**: ANSI CL 12.2.4 — asinh, acosh, atanh
- **難易度**: Easy

#### FR-640: nreconc

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `nconc` (line 203) は実装済み。`nreconc` が全コードベースに不在
- **内容**: `(nreconc list tail)` ≡ `(nconc (nreverse list) tail)` — 破壊的逆順追加
- **根拠**: ANSI CL 14.2.22 — nreconc
- **難易度**: Easy

#### FR-641: fill / replace / mismatch

- **対象**: `src/expand/macros-sequence.lisp`, `src/vm/strings.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `fill`/`replace`/`mismatch` は `our-defmacro` としてリスト専用実装あり (`macros-sequence.lisp:11/28/50`)。`fill` は `(car ptr)`/`(setf (car ptr) ...)` によるコンスセル破壊的更新; `replace` はリスト間コピー; `mismatch` は `eql` 固定の線形探索。`:start`/`:end`/`:test`/`:key` 等のキーワード引数は `(when keys)` no-op で全て無視。ベクタ・文字列への適用不可。文字列セクション (`fill`/`replace` 文字列) は VM命令・ビルトインとも完全不在 ❌ のまま
- **内容**: `(fill seq item &key start end)` — 要素を埋める。`(replace s1 s2 &key start1 end1 start2 end2)` — シーケンス間コピー。`(mismatch s1 s2 &key test key start1 end1 start2 end2)` — 最初の不一致位置を返す
- **根拠**: ANSI CL 17.3 — fill, replace, mismatch
- **難易度**: Medium

#### FR-642: *features* — vm-state / vm2-state 不整合

- **対象**: `src/vm/vm.lisp` (line ~381), `src/vm/vm-run.lisp` (line ~186)
- **現状**: `vm-state` は `*features*` を `(:common-lisp :cl-cc)` で初期化するが `vm2-state` (平坦ベクタインタープリタ) は `(:cl-cc)` のみ。実行系によって `#+common-lisp` の結果が異なる
- **内容**: `vm2-state` の `*features*` 初期化に `:common-lisp` を追加して統一
- **根拠**: ANSI CL 1.5.2 — *features* must include :common-lisp
- **難易度**: Trivial (1行修正)

#### FR-643: #nR — 任意基数整数リテラル

- **対象**: `src/parse/lexer.lisp` (`lex-read-hash-dispatch`, line 548)
- **現状**: `#b`/`#o`/`#x` は `case` ブランチで処理済み。先頭が数字の `#12R` は `otherwise` ブランチでエラー (`lexer.lisp:622`)
- **内容**: `#` の後に数字が続く場合、基数 `n` を読み、`R`/`r` を確認し `lex-read-radix-integer state n` で整数をパース
- **根拠**: ANSI CL 2.4.8 — #nR
- **難易度**: Easy

#### FR-644: #C — 複素数リテラル

- **対象**: `src/parse/lexer.lisp` (`lex-read-hash-dispatch`, line 548)
- **現状**: `#c`/`#C` が `case` ブランチになく `otherwise` でエラー。doc の「ホスト CL 経由で動作」は誤記
- **内容**: `#C(real imag)` → `(complex real imag)` に展開。lexer に `#\c`/`#\C` ブランチを追加
- **根拠**: ANSI CL 2.4.8.11 — #C
- **難易度**: Easy

#### FR-645: char= / char< 等 — 多引数形式非対応

- **対象**: `src/compile/builtin-registry.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `:char-cmp` 呼び出し規約は `(2 . 2)` 固定 (`*convention-arity*` line 394)。`emit-builtin-char-cmp` は1番目・2番目引数のみ使用
- **内容**: ANSI CL 13.2 — `(char= character &rest more-characters)` は3引数以上を許可。`(char< a b c)` は `(and (char< a b) (char< b c))` に展開すべき。`char-equal`/`char-lessp` 等 case-insensitive 版も同様
- **根拠**: ANSI CL 13.2.1 — character comparison functions
- **難易度**: Easy (コンパイル時展開で対応可能)

#### FR-646: *print-base* / *print-escape* 等 — 初期化済みだが印字関数が参照しない

- **対象**: `src/vm/vm.lisp` (lines 395-401), `src/vm/io.lisp` (line 696)
- **現状**: `*print-base*`/`*print-radix*`/`*print-escape*`/`*print-level*`/`*print-length*` は VM グローバル変数テーブル (`*vm-initial-globals*`) に初期値で登録済み。しかし `execute-instruction` for `vm-write-to-string-inst` は `(write-to-string val)` と直接ホスト CL を呼ぶだけで VM グローバルを参照しない。`(setq *print-base* 16)` しても出力が変わらないデッドコード状態
- **内容**: `vm-write-to-string-inst` 実行時に VM グローバルの `*print-base*` 等を読み取り `(write-to-string val :base ... :escape ...)` に渡す。または自前の印字ルーティン実装
- **根拠**: ANSI CL 22.1.1 — printer control variables
- **難易度**: Medium

#### FR-647: symbol-value — ビルトイン未登録・ホストブリッジ対象外

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/vm.lisp`
- **現状**: `symbol-name` は `:unary` 登録済み (`builtin-registry-data.lisp:111`)。`symbol-function` はホストブリッジ白リスト (`vm.lisp:491`) 経由で動作。しかし `symbol-value` は両方に不在。`rt-symbol-value` (`runtime/runtime.lisp:401`) はコンパイラ内部専用で、ユーザーコードからは呼び出し不可
- **内容**: `(symbol-value sym)` → `:unary` ビルトインとして `make-vm-symbol-value` 命令を登録。`(setf symbol-value)` は既存の `setq` フォールスルーで動作
- **根拠**: ANSI CL 10.1.1 — symbol-value
- **難易度**: Easy

#### FR-648: simple-vector-p — コードベース全体に不在

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `vectorp` は `:unary` 登録済み (`builtin-registry-data.lisp:98`)。`simple-vector-p` はコードベース全体に存在しない (`grep` 0件)
- **内容**: `(simple-vector-p x)` → `(and (vectorp x) (not (array-has-fill-pointer-p x)) ...)` として実装、または VM 命令を追加してビルトイン登録
- **根拠**: ANSI CL 15.2 — simple-vector-p; cl-cc の配列は全て単純ベクタ扱いの可能性あり
- **難易度**: Easy

#### FR-649: write-to-string キーワード引数 (`:base`, `:radix`, `:escape` 等)

- **対象**: `src/compile/builtin-registry-data.lisp:154`, `src/vm/io.lisp:693-696`
- **現状**: `write-to-string` は `:unary` 規約 (1引数のみ) で登録。`(write-to-string 255 :base 16)` はコンパイル時に引数数エラー。`execute-instruction` は `(write-to-string val)` を直接呼ぶだけ (FR-646 と連動)
- **内容**: `(write-to-string object &key base radix escape level length circle gensym readably pretty)` に対応した可変引数ビルトイン登録、またはオプション引数規約の追加
- **根拠**: ANSI CL 22.1.2 — write-to-string
- **難易度**: Medium (FR-646 の印字変数参照と合わせて実装が望ましい)

#### FR-650: every / some / notany / notevery — 多シーケンス並列形式非対応

- **対象**: `src/expand/macros-stdlib.lisp` (lines 364-390)
- **現状**: 全て `(pred list)` 2引数固定。`every` は `(our-defmacro every (pred list) ...)` で定義。`notany`/`notevery` は `some`/`every` への委譲のみ
- **内容**: ANSI CL 17.2.1 — `(every predicate &rest sequences+)` は複数シーケンスを並列イテレート。`(every #'< '(1 2 3) '(2 3 4))` は `(and (< 1 2) (< 2 3) (< 3 4))` と等価。`&rest sequences` の zip イテレーションが必要
- **根拠**: ANSI CL 17.2.1 — every, some, notany, notevery
- **難易度**: Medium

#### FR-651: vector (コンストラクタ)

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/list.lisp`
- **現状**: `coerce-to-vector` は既存シーケンスをベクタに変換する専用関数。`(vector 1 2 3)` のような新規ベクタ構築は未登録
- **内容**: `(vector &rest objects)` — 各 `objects` を要素とする1次元単純ベクタを返す。可変引数なので `:variadic` 規約またはマクロ展開が必要
- **根拠**: ANSI CL 15.2 — vector
- **難易度**: Easy

#### FR-652: copy-seq — ベクタ・配列非対応

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `(our-defmacro copy-seq (seq) \`(copy-list ,seq))` — `copy-list` に委譲のためリスト専用
- **内容**: `(copy-seq sequence)` は文字列・ベクタ・配列に対して正しいコピーを返す必要がある。`(etypecase seq (list (copy-list seq)) (vector (copy-vector seq)) (string (copy-string seq)))` に相当するディスパッチが必要
- **根拠**: ANSI CL 17.3 — copy-seq
- **難易度**: Easy–Medium

#### FR-653: remove-duplicates / delete-duplicates — キーワード引数非対応

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `remove-duplicates` は `(our-defmacro remove-duplicates (list) ...)` で1引数 eql のみ。`delete-duplicates` は `(our-defmacro delete-duplicates (list &rest keys) (when keys) ...)` で `when` にボディなし → `keys` を完全無視
- **内容**: `:test`, `:test-not`, `:key`, `:start`, `:end`, `:from-end` キーワード対応が必要
- **根拠**: ANSI CL 17.2.19-20 — remove-duplicates / delete-duplicates
- **難易度**: Medium

#### FR-654: make-array :initial-contents キーワード無視

- **対象**: `src/compile/codegen-phase2.lisp` (line 86)
- **現状**: `(compile-make-array ...)` ハンドラは `size-reg` のみを使用して `vm-make-array` 命令を生成。`:initial-contents` を含む追加キーワードは全て無視
- **内容**: `(make-array 3 :initial-contents '(1 2 3))` が空配列を返してしまう。コンパイル時定数リストまたは実行時 `fill` ループによる初期化が必要
- **根拠**: ANSI CL 15.2 — make-array
- **難易度**: Medium

#### FR-665: mapcar / mapc / mapcan — 複数シーケンス非対応

- **対象**: `src/expand/macros-stdlib.lisp` (lines 334-362)
- **現状**: `(our-defmacro mapcar (fn list) ...)` — `list` は単一引数。`mapc`/`mapcan` も同様。`dolist` による1シーケンスイテレーションのみ
- **内容**: `(mapcar function &rest lists+)` — 複数リストを並列処理。`(mapcar #'+ '(1 2) '(3 4))` → `(3 7)`。`mapc`/`mapcan` も同様
- **根拠**: ANSI CL 17.3 — mapcar / mapc / mapcan
- **難易度**: Medium (複数シーケンスの zip イテレーション実装が必要)

#### FR-663: = / < / > / <= / >= — 多引数形式非対応

- **対象**: `src/parse/cl/parser.lisp` (line 198)
- **現状**: `(define-list-lowerer (+ - * = < > <= >=) (node sf sl sc) (unless (= (length node) 3) (error "~S takes exactly 2 args" (car node))))` — 3引数以上はパース時にエラー
- **内容**: ANSI CL 12.1.1 — `(= 1 2 3)` ≡ `(and (= 1 2) (= 2 3))`。2引数のみ有効
- **根拠**: ANSI CL 12.2 — /=, =, <, >, <=, >=
- **難易度**: Easy (expander で `reduce-variadic-op` スタイルの `and` 折り畳みを追加)

#### FR-664: /= — 完全不在

- **対象**: `src/parse/cl/parser.lisp`, `src/compile/builtin-registry-data.lisp`, `src/vm/primitives.lisp`
- **現状**: `/=` は `define-list-lowerer` リスト・`*typed-binop-ctors*`・ビルトインレジストリ・VM命令 (vm-num-neq 等) 全てに不在。`(/= a b)` は vm-call フォールスルー → 実行時 `"Undefined function: /="`
- **内容**: `(≠ number &rest more-numbers+)` — 全引数が相互に数値的に異なる場合 true
- **根拠**: ANSI CL 12.2 — /=
- **難易度**: Easy

#### FR-661: / (除算) — コンパイラが vm-div を生成しない

- **対象**: `src/parse/cl/parser.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `+`/`-`/`*` は `parser.lisp:198` の `define-list-lowerer` で `ast-binop` に変換され `vm-add`/`vm-sub`/`vm-mul` を生成する。しかし `/` はそのリストに含まれておらず `ast-call` に変換される。ビルトインレジストリにも未登録。`vm-div` 命令は `primitives.lisp:105` に定義済みだが `make-vm-div` の呼び出し元がコンパイラに存在しない。実行時に `vm-resolve-function` でホストブリッジ白リストに不在のため `"Undefined function: /"` エラー
- **内容**: `parser.lisp:198` の `define-list-lowerer` に `/` を追加し `*typed-binop-ctors*` に `(/ . make-vm-div)` を追加
- **根拠**: ANSI CL 12.2 — /
- **難易度**: Easy

#### FR-662: min / max / gcd / lcm — 可変引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/expand/expander-data.lisp`
- **現状**: `min`/`max`/`gcd`/`lcm` は全て `:binary` 規約 (min=2, max=2) で登録。ANSI CL は `(min number &rest more-numbers+)` (1+引数)、`(gcd &rest integers)` (0+引数) を要求。3引数以上はアリティチェック失敗後に vm-call fallthrough → `"Undefined function: min"` エラー
- **内容**: `*variadic-fold-builtins*` に `min`/`max`/`gcd`/`lcm` を追加し `reduce-variadic-op` による折り畳みを有効化。または `:binary-opt-one` 規約で拡張
- **根拠**: ANSI CL 12.2 — min, max, gcd, lcm
- **難易度**: Easy–Medium

#### FR-660: assoc-if / assoc-if-not — ユーザー向け登録なし

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `assoc-if` は `src/compile/pipeline.lisp:150` にコンパイラ内部ヘルパーとして定義 (`defun assoc-if (pred alist)`) されているが、これはホスト CL の代替用であり ユーザーコードのビルトイン登録はない。`assoc-if-not` は完全不在
- **内容**: `(assoc-if test alist &key key)` — `test` を満たす最初の cons を返す。`(assoc-if-not test alist &key key)` — 満たさない最初の cons
- **根拠**: ANSI CL 14.2.3 — assoc-if / assoc-if-not
- **難易度**: Easy

#### FR-697: assoc / rassoc / member — キーワード引数 (:test / :key / :test-not) 非対応

- **対象**: `src/expand/expander-data.lisp`, `src/vm/list.lisp`
- **現状**: `assoc`/`rassoc`/`member` は `*binary-builtins*` に分類 (アリティ 2..2)。`(assoc key alist :test #'equal)` は `emit-registered-builtin` でアリティ不一致 (nargs=4) → `nil` 返却 → `vm-call` フォールスルー → 実行時エラー。ANSI CL では `assoc` / `rassoc` / `member` は全て `&key test test-not key` をサポート
- **内容**: `:test`/`:test-not`/`:key` キーワード引数付き呼び出しを VM 命令レベルで対応。または expander でキーワード引数を parse して適切な比較関数を選択
- **根拠**: ANSI CL 14.2.1 (member), 14.2.3 (assoc), 14.2.3 (rassoc)
- **難易度**: Medium

#### FR-658: read-preserving-whitespace — 未登録

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `read` は `:unary` 登録済み (`line 161`)。`read-preserving-whitespace` は全コードベースに不在
- **内容**: `(read-preserving-whitespace &optional stream eof-error-p eof-value recursive-p)` — `read` と同じだが後続の空白文字をストリームに残す
- **根拠**: ANSI CL 23.2.1 — read-preserving-whitespace
- **難易度**: Easy (ホスト CL に委譲可能)

#### FR-659: read-delimited-list — 未登録

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: 全コードベースに不在
- **内容**: `(read-delimited-list char &optional stream recursive-p)` — `char` が出現するまで sexp を読み続け、リストとして返す。カスタムリーダーマクロの定義で必須
- **根拠**: ANSI CL 23.2.1 — read-delimited-list
- **難易度**: Medium (VM read ループ拡張が必要)

#### FR-655: find-symbol — ビルトイン未登録

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/vm.lisp`
- **現状**: `find-symbol` はコンパイラ内部では使われない。`*vm-host-bridge-functions*` には `find-package` / `intern` が登録されているが `find-symbol` は対象外
- **内容**: `(find-symbol string &optional package-designator)` — パッケージからシンボルを検索し、シンボルと状態の2値を返す
- **根拠**: ANSI CL 11.2.4 — find-symbol
- **難易度**: Easy (intern と同様にホストブリッジ登録)

#### FR-656: list-length — VM命令あり・コンパイラ未登録

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/list.lisp`
- **現状**: `define-simple-instruction vm-list-length :unary list-length` (`list.lisp:290`) で VM命令は定義済み。しかし `builtin-registry-data.lisp` に登録エントリがないため、ユーザーコードからは呼べない
- **内容**: `(list-length list)` — 循環リスト対応の `length` 代替 (循環リストには `nil` を返す)
- **根拠**: ANSI CL 14.2.18 — list-length
- **難易度**: Easy (VM命令は完成済み; builtin-registry-data.lisp に1行追加のみ)

#### FR-657: subst-if / subst-if-not — ツリー置換述語版

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/list.lisp`
- **現状**: `subst` は `:ternary-opt-nil-custom` 規約で登録済み (`builtin-registry-data.lisp:403`)。`subst-if` / `subst-if-not` は VM命令・コンパイラ登録ともに不在
- **内容**: `(subst-if new pred tree &key key)` — `pred` を満たすサブツリーを `new` で置換。`(subst-if-not new pred tree &key key)` — `pred` を満たさないサブツリーを置換
- **根拠**: ANSI CL 14.2.34/35 — subst-if / subst-if-not
- **難易度**: Easy–Medium

#### FR-682: `-` 単項否定バグ / 0引数エラー欠如

- **対象**: `src/expand/expander.lisp` (lines 393-397)
- **現状**: `define-expander-for -` が `reduce-variadic-op '- (cdr form) 0` を呼ぶ。`reduce-variadic-op` の1引数ケースは `(first args)` = `x` を返すため `(- x)` → `x` (否定しない)。0引数ケースは identity `0` を返すが ANSI は `(-)` をエラーとして定義
- **内容**: 1引数特殊ケースを追加: `(1 (list '- 0 (first args)))` → `(- 0 x)`。0引数ケース: `(error "- requires at least one argument")`
- **根拠**: ANSI CL 12.2 — `(- x)` → `-x`; `(-)` is an error
- **難易度**: Easy (1行の条件追加)

#### FR-683: isqrt — 大整数で浮動小数点精度喪失

- **対象**: `src/expand/macros-stdlib.lisp` (line 43)
- **現状**: `(floor (sqrt (float n)))` に展開。`(float n)` が single-float に変換するため 2^24 を超える整数で丸め誤差が発生し `isqrt` が不正確な結果を返す
- **内容**: 整数算術の exact square root 実装 (Newton's method on integers)
- **根拠**: ANSI CL 12.1.3 — isqrt returns the greatest integer ≤ exact positive square root
- **難易度**: Medium (bignum 対応 FR-605 の後に完全化)

#### FR-684: signum — 型非保存 (常に整数を返す)

- **対象**: `src/expand/macros-stdlib.lisp` (lines 35-40)
- **現状**: `(cond ((zerop n) 0) ((plusp n) 1) (t -1))` に展開。常に整数 -1/0/1 を返す
- **内容**: `(signum 2.5)` → `1.0`、`(signum -3.0d0)` → `-1.0d0`。入力型に対応した型の 1/-1/0 を返す
- **根拠**: ANSI CL 12.1.3 — signum preserves type
- **難易度**: Medium

#### FR-685: float-sign 2引数形式非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 67), `src/vm/vm-numeric.lisp`
- **現状**: `*builtin-unary-entries*` に登録 (1引数のみ)
- **内容**: `(float-sign float1 float2)` — `float2` の絶対値 × `float1` の符号。`:binary-opt-one` 規約に変更
- **根拠**: ANSI CL 12.1.3 — `(float-sign float1 &optional float2)`
- **難易度**: Easy

#### FR-686: aref — 多次元配列非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 300), `src/vm/array.lisp` (lines 22-28)
- **現状**: `aref` は `*builtin-binary-custom-entries*` に `:array-reg :index-reg` の2スロット構成で登録。`(aref arr i j)` (2次元) はコンパイル時にエラーまたはインデックスが1つだけ使用される
- **内容**: `(aref array &rest subscripts)` — 多次元対応。`row-major-aref` + `array-row-major-index` の合成、または VM 命令スロット拡張
- **根拠**: ANSI CL 15.2 — aref with multiple subscripts
- **難易度**: Medium

#### FR-687: make-array キーワード引数 — 部分対応

- **対象**: `src/expand/expander.lisp` (lines 229-239), `src/compile/codegen-phase2.lisp` (lines 86-90)
- **現状**: expander `expand-make-array-form` は `:fill-pointer`/`:adjustable` の**存在**を検出して `make-adjustable-vector` に昇格するが、実際の値を無視 (`(make-array 10 :fill-pointer 5)` → fill-pointer=0)。`:initial-element`/`:initial-contents`/`:element-type`/`:displaced-to` は expander で静かに破棄される (検出すらされない)
- **内容**: expander で全キーワードの値を正確に解析し、VM 命令の対応スロットに渡す
- **根拠**: ANSI CL 15.2 — make-array keyword arguments
- **難易度**: Medium

#### FR-688: delete / substitute 系 — keyword 引数が no-op

- **対象**: `src/expand/macros-sequence.lisp` (lines 73-202)
- **現状**: `delete`/`delete-if`/`delete-if-not`/`substitute`/`substitute-if`/`substitute-if-not`/`nsubstitute` 系はすべて `&rest keys` を受け付けるが本体内の `(when keys ...)` が no-op。`:test`, `:key`, `:start`, `:end`, `:count`, `:from-end` が静かに無視され、常に `eql` + 全シーケンス操作になる
- **内容**: キーワード引数の解析と適用。特に `:count 1` が最も使用頻度が高い
- **根拠**: ANSI CL 17.3 — delete/substitute keyword arguments
- **難易度**: Medium (`:test`/`:key` は比較的容易; `:start`/`:end` はシーケンス型依存)

#### FR-678: random — optional random-state 引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 77), `src/vm/vm-numeric.lisp` (line 140)
- **現状**: `random` は `*builtin-unary-entries*` に登録 (1引数のみ)。`vm-random` は `define-simple-instruction :unary random` で実装
- **内容**: ANSI CL 12.1.6 — `(random limit &optional random-state)`。`(random 10 my-state)` でユーザー管理の random-state を使用できない。`:binary-opt-one` 等の規約に変更が必要
- **根拠**: ANSI CL 12.1.6 — random
- **難易度**: Easy

#### FR-679: get-decoded-time — ビルトイン未登録

- **対象**: `src/compile/builtin-registry-data.lisp` (lines 270-278)
- **現状**: `*builtin-nullary-entries*` に `get-decoded-time` が不在。`get-universal-time`/`get-internal-real-time`/`get-internal-run-time` は登録済み
- **内容**: `(get-decoded-time)` — 現在時刻を9値 (second minute hour date month year day-of-week daylight-p zone) で返す。`(decode-universal-time (get-universal-time))` との違いはタイムゾーン取得の一発性
- **根拠**: ANSI CL 25.1.4 — get-decoded-time
- **難易度**: Easy (nullary builtin + `(decode-universal-time (get-universal-time))` へのデシュガーで実装可)

#### FR-680: provide / require — 展開問題

- **対象**: `src/expand/macros-sequence.lisp` (lines 403-413)
- **現状**: `provide` が `(pushnew ,mod *modules* :test #'string=)` に展開するが `pushnew` は FR-587 で未実装 (❌)。コンパイル済みコードで `(provide "mymod")` を呼ぶと実行時エラー。`require` は `pathnames` 引数を `(declare (ignore pathnames))` で無視し、モジュール不在時 `(warn ...)` するだけで実際のファイルロードを行わない
- **内容**: (1) `provide` — `pushnew` を `(unless (member mod *modules* :test #'string=) (push mod *modules*))` に書き換え。(2) `require` — `pathnames` が指定された場合 `(dolist (p pathnames) (load p))` を実行
- **根拠**: ANSI CL 25.1.3 — provide/require
- **難易度**: Easy (provide), Medium (require — load integration)

#### FR-681: sleep — 完全不在

- **対象**: 全コードベース
- **現状**: `sleep` は VM 命令・ビルトイン登録・マクロのいずれも存在しない。コンパイル済みコードから `(sleep 1.0)` を呼ぶと vm-call フォールスルー → `"Undefined function: sleep"`
- **内容**: unary builtin として登録、`(sleep seconds)` → ホスト CL の `sleep` に委譲
- **根拠**: ANSI CL 25.1.1 — sleep
- **難易度**: Easy (trivial unary builtin wrapping host CL `sleep`)

#### FR-675: maphash — ビルトイン未登録・VM命令なし

- **対象**: `src/vm/hash.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: コンパイラ内部で `maphash` を多用 (16ファイル) しているが、これはすべてホスト CL の `maphash` を macro-expand 時に呼んでいる。`vm-maphash` 命令も存在せず、`builtin-registry-data.lisp` にも未登録。コンパイル済みユーザーコードから `(maphash fn ht)` を呼ぶと vm-call フォールスルー → `"Undefined function: maphash"`
- **内容**: VM命令 `vm-maphash-inst` を追加 (`:fn-reg` と `:ht-reg` スロット)。`vm-hash-table-keys`/`vm-hash-table-values` ペアのループとして実装するか、ホストに委譲
- **根拠**: ANSI CL 18.1 — maphash
- **難易度**: Medium

#### FR-676: with-slots — symbol-macrolet ではなく let 展開; 書き戻し不能

- **対象**: `src/expand/macros-stdlib.lisp` (lines 92-101)
- **現状**: `with-slots` が `(let ((,slot (slot-value ,inst ',slot))) ,@body)` に展開。ANSI CL は `symbol-macrolet` を使いスロット変数への書き込みが `(setf slot-value ...)` に透過されることを要求。現実装では `(setf slot new-val)` がローカル `let` 変数を書き換えるだけでオブジェクトのスロットは変わらない
- **内容**: `symbol-macrolet` で展開するよう変更: `(symbol-macrolet ((,slot (slot-value ,inst-var ',slot))) ,@body)`
- **根拠**: ANSI CL 7.6.6 — with-slots must use symbol-macrolet for place semantics
- **難易度**: Easy (but depends on FR-220 symbol-macrolet)

#### FR-677: class-name — 完全不在

- **対象**: 全コードベース
- **現状**: `(class-name class)` に対応する VM命令・ビルトイン・コードジェネレータが一切存在しない。VM のクラスハッシュテーブルには `:__name__` キーで名前が格納されているが、ユーザー呼び出し可能な `class-name` 関数がない。`(class-name (class-of x))` のような典型的 CLOS イントロスペクションコードが実行時エラー
- **内容**: unary builtin として登録、VM命令 `vm-class-name` で `(gethash :__name__ class-ht)` を実行
- **根拠**: ANSI CL 7.7.2 — class-name
- **難易度**: Easy

#### FR-667: logand / logior / logxor / logeqv — 可変引数非対応

- **対象**: `src/expand/expander-data.lisp` (line 49-51), `src/compile/builtin-registry-data.lisp` (lines 178-181)
- **現状**: `*variadic-fold-builtins*` は `(+ * append nconc)` のみ。`logand`/`logior`/`logxor`/`logeqv` は `*binary-builtins*` に分類 (アリティ 2.2)。`(logand a b c)` はコンパイル時 `emit-registered-builtin` がアリティ不一致で nil を返し vm-call にフォールスルー → 実行時エラー。`(logand)` → ANSI では `-1` だが cl-cc ではエラー。`(logior)` → ANSI では `0`
- **内容**: `logand`/`logior`/`logxor` を `*variadic-fold-builtins*` に追加 (恒等元: -1, 0, 0)。または expander で `(logand a b c)` → `(logand (logand a b) c)` にネスト展開
- **根拠**: ANSI CL 12.1.4 — logand/logior/logxor/logeqv は `&rest integers`
- **難易度**: Easy

#### FR-668: digit-char-p — optional radix 引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 125), `src/vm/symbols.lisp` (lines 84-88)
- **現状**: `digit-char-p` は `*builtin-unary-entries*` で unary 登録。VM 命令 `vm-digit-char-p` は `(digit-char-p ch)` のみ実行。`(digit-char-p #\A 16)` → ANSI では `10` だが cl-cc ではアリティエラー
- **内容**: 2引数形式 `(digit-char-p char radix)` 対応。builtin を `:binary-opt-one` 等の規約に変更、VM 命令に `:radix` スロット追加
- **根拠**: ANSI CL 13.1.4.2 — `(digit-char-p char &optional (radix 10))`
- **難易度**: Easy

#### FR-669: string comparators — 戻り値が boolean (0/1) で mismatch-index を返さない

- **対象**: `src/vm/strings.lisp` (lines 114-124), `src/compile/builtin-registry-data.lisp` (lines 212-227)
- **現状**: 全 string 比較命令 (`vm-string-equal`/`vm-string-lessp` 等) が `:pred2` 実行スタイル (1/0 boolean 返却)。ANSI CL では `string=` は nil/t だが `string<`/`string>`/`string<=`/`string>=`/`string/=` および全 case-insensitive 変種は nil (等しい/大小逆) または **不一致位置の整数** を返す
- **内容**: `(string< "abc" "abd")` → `2`、`(string/= "ab" "ab")` → `nil`。VM 命令を `:pred2` から位置返却型に変更。case-insensitive 変種も同様
- **根拠**: ANSI CL 13.2.4 — string comparators return index or nil
- **難易度**: Medium

#### FR-670: peek-char — ビルトイン未登録

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/io.lisp` (lines 69-74)
- **現状**: VM 命令 `vm-peek-char` は存在 (`io.lisp:69-74`) し `execute-instruction` も実装済み。しかし `builtin-registry-data.lisp` のいずれのエントリテーブルにも `peek-char` が登録されていない。`(peek-char)` はコンパイルフォールスルー → `"Undefined function: peek-char"` 実行時エラー
- **内容**: `(peek-char &optional peek-type input-stream eof-error-p eof-value recursive-p)` — `*builtin-stream-input-opt-entries*` 等に追加。VM 命令に `:peek-type` スロット追加が必要
- **根拠**: ANSI CL 21.2 — peek-char
- **難易度**: Easy (登録のみ)〜Medium (peek-type 完全対応)

#### FR-671: unread-char — optional stream 引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 320)
- **現状**: `unread-char` は `*builtin-binary-void-entries*` に `:binary-void` 規約 (2引数必須)。`(unread-char ch)` のデフォルト `*standard-input*` 形式はアリティ不一致でコンパイルエラー
- **内容**: `(unread-char character &optional input-stream)` — `:binary-void` を optional-stream 対応規約に変更
- **根拠**: ANSI CL 21.2 — `(unread-char character &optional input-stream)`
- **難易度**: Easy

#### FR-672: read-line — eof 引数非対応・第2戻り値欠落

- **対象**: `src/vm/io.lisp` (lines 331-341), `src/compile/builtin-registry-data.lisp` (line 377)
- **現状**: `read-line` は `:stream-input-opt` 規約 (handle のみ optional)。VM 実行は `(read-line stream nil +eof-value+)` hardcoded — `eof-error-p` 常に `nil`、`eof-value` 常に `:eof`。また `(multiple-value-bind (line eof-p) (read-line s))` の第2戻り値 `missing-newline-p` が `(declare (ignore ...))` で削除され常に `nil`
- **内容**: (1) `eof-error-p`/`eof-value` 引数を通す。(2) `(values line missing-newline-p)` として2値返却
- **根拠**: ANSI CL 21.2 — read-line returns (values string missing-newline-p)
- **難易度**: Easy〜Medium

#### FR-673: terpri / fresh-line — optional stream 引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (lines 266-268), `src/vm/format.lisp` (lines 103-111)
- **現状**: `*builtin-void-side-effect-entries*` 規約は引数なし。VM 命令はスロットなし (`vm-terpri-inst`/`vm-fresh-line-inst`)、`execute-instruction` は `(vm-output-stream state)` のみに出力。`(terpri *error-output*)` は引数がコンパイルエラーまたはデフォルト出力に書き込む
- **内容**: `(terpri &optional output-stream)` / `(fresh-line &optional output-stream)` — `:void-side-effect-opt-stream` 等の新規約を追加し、stream を通す
- **根拠**: ANSI CL 21.2 — terpri/fresh-line optional stream
- **難易度**: Easy

#### FR-674: listen — required stream 引数 (デフォルト非対応)

- **対象**: `src/compile/builtin-registry-data.lisp` (line 255)
- **現状**: `listen` は `:handle-input` 規約 (handle は必須引数)。ANSI では `(listen &optional input-stream)` — stream なしで `*standard-input*` を使用。`(listen)` はアリティエラー
- **内容**: `:handle-input` を optional-stream 対応規約に変更
- **根拠**: ANSI CL 21.2 — `(listen &optional input-stream)`
- **難易度**: Easy

#### FR-666: print / prin1 / princ — オプション stream 引数なし

- **対象**: `src/compile/builtin-registry-data.lisp` (lines 260-262)
- **現状**: 3関数は `:side-effect` 規約で登録。`*convention-arity*` における `:side-effect` のアリティは `(1 . 1)` — 1引数固定。`(print x stream)` は `emit-registered-builtin` がアリティ不一致で nil を返し vm-call にフォールスルー → 実行時 `"Undefined function: print"`
- **内容**: `(print object &optional stream)` / `(prin1 object &optional stream)` / `(princ object &optional stream)` — ANSI CL の全シグネチャ対応。`:side-effect` を `:side-effect-opt-stream` 等の新規約に拡張するか、codegen-phase2 で個別ハンドラを追加
- **根拠**: ANSI CL 22.3.3 — print / prin1 / princ
- **難易度**: Easy

#### FR-689: catch / throw — VM命令なし、非局所脱出完全破綻

- **対象**: `src/compile/codegen.lisp` (lines 54-74), `src/vm/` (全体)
- **現状**: `compile-ast` の `ast-catch` メソッド (line 54) と `ast-throw` メソッド (line 69) がともに `(declare (ignore tag-reg))` でタグ計算値を破棄。`vm-catch`/`vm-throw` 命令は `src/vm/` に一切存在しない。`(catch 'foo (throw 'foo 42))` がストレートラインコードにコンパイルされ非局所脱出が発生しない。`unwind-protect` のクリーンアップも `catch` が動作しないため意味をなさない
- **内容**: VM命令 `vm-catch-inst` (タグ+ボディ+ジャンプ先) と `vm-throw-inst` (タグ+値) を追加。実行時は動的タグスタックで一致するフレームを探して脱出
- **根拠**: ANSI CL 5.2 — catch/throw for non-local exits
- **難易度**: Hard (動的タグスタック + VM実行ループ改修)

#### FR-690: rotatef — 2引数のみ、setq で複合place非対応

- **対象**: `src/expand/macros-stdlib.lisp` (lines 92-101)
- **現状**: `(our-defmacro rotatef (a b) ...)` — 2引数固定定義。`(rotatef a b c)` は引数過多エラー。内部で `setq` 使用のため `(rotatef (aref arr i) (aref arr j))` は コンパイルエラー（compound place 非対応）。ANSI では `(rotatef place1 place2 ... placeN)` と任意個数 place をローテート
- **内容**: `&rest places` で任意個数受け取り、gensym でガード後 `setf` で書き戻す実装に変更
- **根拠**: ANSI CL 5.1.2 — rotatef must accept N places using setf
- **難易度**: Easy〜Medium

#### FR-691: ignore-errors — condition 第2値破棄

- **対象**: `src/expand/macros-stdlib.lisp` (lines 612-616)
- **現状**: `(our-defmacro ignore-errors (&body forms) ... (handler-case ... (error (,e-var) nil)))` — エラー時 `nil` のみ返却。ANSI CL では `(values nil condition)` として condition を第2値で返すことが要求される。`(multiple-value-bind (result c) (ignore-errors (error "x")))` で `c` が常に `nil`
- **内容**: `nil` を `(values nil ,e-var)` に変更
- **根拠**: ANSI CL 9.1 — ignore-errors returns (values nil condition) on error
- **難易度**: Trivial (1行修正)

#### FR-696: &optional / &key supplied-p 変数 — codegen で破棄

- **対象**: `src/compile/codegen-functions.lisp` (lines 111-123)
- **現状**: `allocate-defaulting-params` は `(first param)` (名前) と `(second param)` (デフォルト値) のみ使用。`(third param)` (supplied-p シンボル) は無視される。パーサー (`macro.lisp:78-79, 100-101`) は `supplied-p` を正しく3要素リストに格納しているが、codegen でシレントに破棄。`(defun f (&optional (x 10 x-p)) x-p)` のような関数で `x-p` が未定義変数として実行時エラー
- **内容**: `allocate-defaulting-params` の `make-entry` コールバックに `supplied-p` を追加し、gensym で sentinel 比較: `(eq val *non-constant-default-sentinel*)` → `t` / `nil` で bound-p レジスタを初期化する
- **根拠**: ANSI CL 3.4.1 — Ordinary Lambda Lists, supplied-p parameters
- **難易度**: Medium

#### FR-695: loop downfrom / downto / above — 未実装

- **対象**: `src/expand/loop-parser.lisp` (lines 118-130)
- **現状**: `FROM` パーサーは `TO`, `BELOW`, `UPTO` のみ認識。`DOWNFROM`, `DOWNTO`, `ABOVE` を受け取ると `loop-unknown-for-keyword` エラーをシグナル (テスト `loop-macro-tests.lisp:923` で確認)
- **内容**: `(loop for x downfrom 10 to 1 collect x)` が `(10 9 8 7 6 5 4 3 2 1)` を返すよう実装。`DOWNFROM` → `:from` + 負の `:by` / `DOWNTO`/`ABOVE` → 境界チェック
- **根拠**: ANSI CL 6.1.1.3 — loop numeric stepping
- **難易度**: Medium

#### FR-694: ~/fn/ format 指示子 — VM クロージャ呼び出し不可

- **対象**: `src/vm/format.lisp` (line 123)
- **現状**: `format` は `(apply #'format nil fmt-str arg-vals)` でホスト CL に委譲。`~/cl-cc-user:my-fn/` のような指示子で参照する関数は SBCL の関数名前空間で解決される。ユーザーが `(defun my-fn ...)` で定義した関数はホスト SBCL の `fboundp` には存在しないため、`~/fn/` 指示子で呼び出せない
- **内容**: ユーザー定義関数を `~/fn/` で使う場合は、VM 名前空間からホスト CL の fmakunbound/defun に橋渡しするか、format を自前実装 (FR-389) する必要がある
- **根拠**: ANSI CL 22.3.5.4 — ~/function-name/ format directive
- **難易度**: Hard (FR-389 自前 format 実装が前提)

#### FR-693: incf / decf / push / pop — 複合place でサブフォーム2重評価

- **対象**: `src/expand/macros-basic.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `incf`/`decf` は `(setq ,place (+ ,place ,delta))` 相当で place を2回展開。`push`/`pop` も同様に place を gensym でガードしていない。`(incf (aref arr (random 10)))` でランダムインデックスが2回評価され異なる要素を読み書きする可能性がある
- **内容**: `get-setf-expansion` (FR-355) を使ってサブフォームを gensym でバインドしてから読み書きするよう修正。または手動で compound place を分解して一時変数を生成
- **根拠**: ANSI CL 5.1.3 — modify macros must evaluate subforms exactly once
- **難易度**: Medium (FR-355 get-setf-expansion 実装が前提)

#### FR-692: restart-bind — 完全スタブ、バインディング全無視

- **対象**: `src/expand/macros-stdlib.lisp` (lines 236-239)
- **現状**: `(our-defmacro restart-bind (bindings &body body) (declare (ignore bindings)) \`(progn ,@body))` — `bindings` を完全無視して `body` をそのまま実行。リスタートのインストール・検索・呼び出しが一切動作しない。`restart-case` も `restart-bind` に依存するため間接的に影響
- **内容**: 動的リスタートスタック (`*restart-stack*`) と `invoke-restart` (FR-421) の実装が前提。`restart-bind` はスタックにリスタートオブジェクトを `push`、ボディ後に `pop` する `unwind-protect` ラッパーが必要
- **根拠**: ANSI CL 9.1.4 — restart-bind
- **難易度**: Hard (invoke-restart / find-restart の全システム実装が必要)

---

## 実装状況サマリー

| カテゴリ | ✅ 実装済み | 🔶 部分実装 | ❌ 未実装 |
|---------|:---:|:---:|:---:|
| 特殊形式 (Ch.3) | 13 | 7 | 1 |
| ラムダリスト | 6 | 1 | 5 |
| 評価・コンパイル API | 3 | 2 | 11 |
| 複合型指定子 | 10 | 4 | 2 |
| 型・クラス API | 1 | 4 | 4 |
| 制御フロー・等価 | 23 | 7 | 7 |
| LOOP | 14 | 0 | 4 |
| マッピング | 2 | 1 | 3 |
| CLOS | 5 | 10 | 15 |
| 構造体 | 3 | 1 | 4 |
| コンディション | 2 | 10 | 18 |
| シンボル | 6 | 1 | 8 |
| パッケージ | 0 | 4 | 9 |
| 数値 | 41 | 11 | 24 |
| 文字 | 9 | 4 | 3 |
| コンス・リスト | 19 | 4 | 17 |
| 配列 | 11 | 5 | 12 |
| 文字列 | 7 | 6 | 3 |
| シーケンス | 10 | 11 | 8 |
| ハッシュテーブル | 6 | 2 | 5 |
| ファイルシステム | 1 | 3 | 10 |
| ストリーム・I/O | 14 | 7 | 19 |
| プリンタ | 14 | 4 | 3 |
| リーダー | 6 | 1 | 12 |
| システム構成 | 1 | 3 | 5 |
| 標準変数 | 4 | 6 | 14 |
| 宣言・コンパイラ | 3 | 5 | 7 |
| 環境・開発ツール | 0 | 2 | 12 |
| モダン拡張 | 1 | 1 | 12 |
| コンス・リスト (追補) | 1 | 0 | 3 |
| 配列 (追補) | 1 | 1 | 2 |
| 文字・標準文字名 | 1 | 1 | 0 |
| リーダー (追補) | 2 | 0 | 5 |
| **合計** | **241** | **126** | **277** |

**概算カバレッジ: ~40%**

### 領域別優先度

| 優先度 | 領域 | 理由 |
|--------|------|------|
| 🔴 高 | コンディション・リスタート (FR-356/418/420/421/424) | セルフホスティング品質に直結 |
| 🔴 高 | パッケージシステム (FR-437〜438) | 標準 CL コードの互換性 |
| 🟡 中 | `defstruct` 完全化 (FR-544〜546) | CLOS と統合してこそ完結 |
| 🟡 中 | `read-sequence`/`write-sequence` (FR-590) | バイナリ I/O の基盤 |
| 🟡 中 | 複合型指定子 (FR-581) | 型システム表現力 |
| 🟢 低 | 開発ツール (FR-431〜436) | 利便性向上 |
| 🟢 低 | Easy FR 群 (FR-543/563/578/609/610/611/614/615/620/621/622/623) | 1〜2時間で実装可能 |
| 🟡 中 | FR-607: docstrings 保存 (FR-436 基盤) | `documentation` GF の前提条件 |
| 🔴 高 | FR-605: bignum | 値表現の根幹変更; 長期投資 |

**概算カバレッジ: ~45% (コア言語・算術・FORMAT指示子は強い。`catch`/`throw`・`restart-bind` が完全破綻として再分類。コンディション・パッケージ・複合型・開発ツール・モダン拡張が主な未実装領域)**
