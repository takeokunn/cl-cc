# Runtime: Standard Library — ANSI Compliance and Optimization (Phase 176-235, 73 FRs)

MOP extensions, compiler macros, GC tuning, parallel compilation, error messages, weak references, fixnum/bignum/rational/complex arithmetic, external formats, pretty printer, FORMAT/READ complete, eval-when, LOOP extensions, CLOS method combinations, dynamic binding optimization, tail calls, runtime type checking, compilation environment, character/string compliance, defstruct, type declarations, image save/load, OS interface, POSIX signals, finalizers, symbol macros, setf expanders, escape analysis, TRMC, package-local nicknames, bytecode layer, CLOS class changes, ASDF integration, serialization, class-allocated slots, inlining, printer/reader variables, numeric I/O, dynamic wind, interprocedural optimization, SBCL compatibility, block/tagbody, numeric comparison, sequence operations, hash table SIMD, source tracking, trace/step/break, numeric output, CLOS compile-time optimization, weak pointers, native threads, terminal control, circular printing, local declarations, code walker, numeric clamp, precise stack maps, typep dispatch, Gray streams, LOOP arithmetic.

---

### Phase 176 — コンパイラマクロ・宣言処理

#### FR-934: compiler-macro-function (コンパイラマクロ)

- **対象**: `packages/frontend/expand/src/expander.lisp`
- **内容**:
  - `(define-compiler-macro name lambda-list body)` — コンパイラマクロ定義
  - `(compiler-macro-function name &optional env)` — コンパイラマクロ取得
  - `(funcall (compiler-macro-function 'foo) form env)` — 手動展開
  - コンパイル時特殊化: `(define-compiler-macro + (&rest args) ...)` で型別最適化
  - `&whole form` パラメータ: 元のフォーム全体へのアクセス
  - コンパイラマクロの無効化: `(setf (compiler-macro-function 'foo) nil)`
  - 展開を断る: コンパイラマクロが `form` をそのまま返す → 通常のマクロ展開へフォールバック
- **根拠**: ANSI CL §3.2.2.1。`list`/`string` などの組み込み関数に定数最適化を付加
- **難易度**: Medium

#### FR-935: proclamation / declaim 完全実装

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(declaim (type integer *counter*))` → グローバル変数の型宣言
  - `(declaim (ftype (function (integer integer) integer) gcd))` → 関数シグネチャ宣言
  - `(declaim (inline foo))` / `(declaim (notinline foo))` — インライン指示
  - `(declaim (optimize (speed 3) (safety 0) (debug 1)))` — 最適化ポリシー宣言
  - `(declaim (special *global*))` — スペシャル変数宣言
  - `*global-proclamations*` — `defpackage` の前から有効なグローバル宣言テーブル
  - `(the type form)` との統合: `declaim type` + `the` で型チェック除去
- **根拠**: ANSI CL §3.3。コンパイラへのヒント提供の主要手段
- **難易度**: Medium

---

### Phase 177 — ガベージコレクタ チューニング API

#### FR-938: GC Tuning Interface (GCチューニングインターフェース)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**:
  - `(gc &key full)` — 明示的GC起動（`full=t` でフルGC）
  - `(gc-statistics)` → plist: `(:collections-minor N :collections-major N :bytes-allocated N :bytes-freed N :pause-ms-p99 F)`
  - `(set-gc-threshold :nursery-size bytes)` — ナーサリサイズ調整
  - `(set-gc-threshold :tenuring-threshold n)` — オブジェクト昇格世代数
  - `*gc-notify-function*` — GCコールバック（モニタリング用）
  - `(disable-gc)` / `(enable-gc)` — リアルタイムセクション用GC抑制
  - `(with-gc-disabled body)` — GC禁止区間マクロ
  - `(room &optional detail)` — ANSI CL `room` — メモリ使用量表示
- **根拠**: SBCL `sb-ext:gc` / `sb-ext:bytes-consed`。GCチューニングは高スループットアプリで重要
- **難易度**: Easy

---

### Phase 178 — 並列コンパイル詳細

#### FR-941: Parallel Compilation Pipeline (並列コンパイルパイプライン)

- **対象**: `packages/cli/src/main.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - ファイルレベル並列化: `(compile-files files :parallel t)` — 依存グラフ解析後に独立ファイルを並列コンパイル
  - 依存グラフ構築: `(build-compile-graph files)` → DAG (package定義・マクロ定義を追跡)
  - ワーカープール: `(make-compile-worker-pool n)` — n個のCPUコアを利用
  - フォームレベル並列化: 副作用なし判定後に `(mappar #'compile-form forms)`
  - 進捗報告: `*compile-progress-fn*` コールバック
  - ASDF並列: `(asdf:operate 'asdf:compile-op system :parallel t)`
  - キャッシュ活用: `(compile-with-cache file cache-dir)` — FASL変更なし時スキップ
- **根拠**: GNU Make `-j` / Bazel / Buck2 の並列ビルド。大規模コードベースのビルド時間短縮
- **難易度**: Hard

---

### Phase 179 — エラーメッセージ品質

#### FR-944: Rich Error Messages (リッチエラーメッセージ)

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/frontend/parse/src/cl/parser.lisp`
- **内容**:
  - ソース位置の保持: 全AST ノードに `(source-location :file :line :column)` スロット
  - エラー表示: `"error at foo.lisp:42:10: undefined variable 'x'"`
  - コンテキスト行の表示: エラー箇所前後3行 + カラム指示 `^` カーソル
  - Did-you-mean 提案: Levenshtein距離でシンボル名候補を提示
  - `undefined-function` / `unbound-variable` での候補リスト
  - スタックトレースのフォーマット: `(format-stack-trace condition stream :depth 10)`
  - `*error-output*` への構造化出力 (JSON / ANSI カラー切り替え)
- **根拠**: Rust / Elm のエラーメッセージ品質。開発体験の根本的改善
- **難易度**: Medium

#### FR-945: Condition Restart UI (条件リスタートUI)

- **対象**: `packages/engine/vm/src/conditions.lisp`
- **内容**:
  - `(describe-restart restart)` — リスタートの説明文取得
  - `(restart-interactive restart)` — 対話的リスタート起動（REPL でユーザーに入力を促す）
  - `(compute-restarts &optional condition)` → active restart list
  - `(invoke-restart-interactively restart)` — インタラクティブ関数を呼び出してから invoke
  - REPL デバッガ: `(invoke-debugger condition)` → `*debugger-hook*` → 対話的修復
  - `(with-simple-restart (name format-string &rest args) body)` マクロ
  - `store-value` / `use-value` / `continue` / `abort` 標準リスタートの完全実装
- **根拠**: ANSI CL §9.1 restart system。CL の対話的デバッグの中核機能
- **難易度**: Medium

---

### Phase 180 — Weak Reference / Ephemeron 完全実装

#### FR-948: Ephemeron (エフェメロン)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/hash.lisp`
- **内容**:
  - `(make-ephemeron key value)` → ephemeron オブジェクト
  - セマンティクス: key が到達不能になった場合、value も回収可能になる（通常の弱参照と異なる）
  - `(ephemeron-key e)` / `(ephemeron-value e)` — アクセサ（GC後は nil）
  - `(ephemeron-alive-p e)` → boolean
  - GCサイクル中の処理: gray set scan → ephemeron の key が白なら value を nil に
  - `(make-weak-hash-table :weakness :key-and-value)` — ephemeron テーブル（SBCL互換）
  - `:weakness :key` / `:weakness :value` / `:weakness :key-and-value` / `:weakness :key-or-value` の全モード
- **根拠**: McCLIM / CLOCC ライブラリ。キャッシュ・メモ化でのメモリリーク防止に必須
- **難易度**: Hard

---

### Phase 181 — 整数演算特殊化

#### FR-951: Fixnum Fast Paths (Fixnum高速パス)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - `(+f x y)` / `(-f x y)` / `(*f x y)` — fixnum専用演算（オーバーフローチェックなし）
  - `(logand x y)` / `(logior x y)` / `(logxor x y)` / `(lognot x)` — ビット演算
  - `(ash x n)` — 算術シフト（`n>0`=左、`n<0`=右）; `(ash x -1)` = `(floor x 2)`
  - `(integer-length x)` → ビット幅（`(integer-length 255)` = 8）
  - `(logbitp i x)` — ビット `i` のテスト
  - `(logcount x)` — セットビット数 (popcount); x86-64 `POPCNT` 命令に直接マッピング
  - `(deposit-field value bytespec integer)` / `(ldb bytespec integer)` — バイト操作 (ANSI §12.9)
  - `(dpb value bytespec integer)` — ビットフィールド挿入
- **根拠**: ANSI CL §12.9 byte operations。システムプログラミング・ビットマスク操作に必須
- **難易度**: Easy

#### FR-952: Bignum Arithmetic (多倍長整数演算)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **内容**:
  - Bignum 表現: `limb-vector` (64ビット符号なし整数の配列) + sign フラグ
  - 加減算: schoolbook O(n)、繰り越し処理
  - 乗算: Karatsuba O(n^1.585) (n>64 limbs 時自動切り替え)
  - 除算: Knuth Algorithm D
  - `(expt n m)` での bignum: 平方乗算法 O(log m 乗算)
  - `(gcd a b)` — バイナリGCD アルゴリズム
  - `(isqrt n)` — 整数平方根
  - 出力: `(write-to-string bignum :radix 16)` — 任意基数出力
  - SBCL / GMP との比較: 純CL実装 vs FFI依存
- **根拠**: Common Lisp の整数は任意精度が必須要件。`(expt 2 1000)` が正しく動作すること
- **難易度**: Hard

---

### Phase 182 — 有理数・複素数演算

#### FR-955: Rational Number Arithmetic (有理数演算)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **内容**:
  - `rational` 型: `numerator/denominator` ペア（bignum 対応）
  - `(/ 1 3)` → `#1/3`（自動正規化: GCDで約分）
  - `(+ 1/3 1/6)` → `1/2`（共通分母への変換）
  - `(rational 0.1)` → 正確な有理数近似（IEEE 754 binary fraction の有理数表現）
  - `(rationalize 0.1)` → 人間に読みやすい近似（`1/10` に近い最小分数）
  - `(numerator r)` / `(denominator r)` アクセサ
  - 数値タワー統合: rational は float に昇格（contagion rules）
  - `(floor r)` で整数部分 (quotient) + 有理数余り (remainder)
- **根拠**: ANSI CL §12.1.3。CL の有理数は完全精度の分数演算を提供
- **難易度**: Medium

#### FR-956: Complex Number Arithmetic (複素数演算)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **内容**:
  - `complex` 型: real + imaginary ペア（floatまたはrational）
  - `(complex 3 4)` → `#C(3 4)`; `(realpart c)` / `(imagpart c)`
  - `(abs #C(3 4))` → `5.0` (絶対値 = sqrt(real^2 + imag^2))
  - `(conjugate #C(3 4))` → `#C(3 -4)`
  - `(phase #C(0 1))` → `π/2`（偏角）
  - `(* #C(1 2) #C(3 4))` → `#C(-5 10)` (複素乗算)
  - `(exp #C(0 π))` → `#C(-1 0)` (Euler の公式)
  - 数値タワー: complex > double-float > single-float > rational > integer
- **根拠**: ANSI CL §12.1.5。科学計算・フーリエ変換での複素数演算
- **難易度**: Medium

---

### Phase 183 — ストリーム外部形式

#### FR-959: External Format / Encoding (外部形式・文字エンコーディング)

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/engine/vm/src/strings.lisp`
- **内容**:
  - `(open path :external-format :utf-8)` — UTF-8 エンコードストリーム
  - サポート形式: `:utf-8`, `:utf-16le`, `:utf-16be`, `:utf-32le`, `:utf-32be`, `:latin-1`, `:ascii`
  - BOM検出: UTF-8/UTF-16/UTF-32 BOM の自動検出と除去
  - `(stream-external-format stream)` → 現在のエンコーディング
  - `(setf (stream-external-format stream) :utf-8)` — 動的切り替え
  - `*default-external-format*` — デフォルト外部形式変数
  - エンコードエラー処理: `:error`(例外) / `:replace`(代替文字) / `:ignore`(スキップ)
  - `(encode-string string format)` / `(decode-bytes bytes format)` — 低レベルAPI
- **根拠**: ANSI CL §13.1.10 + SBCL `:external-format`。国際化対応の基盤
- **難易度**: Medium

---

### Phase 184 — Pretty Printer 完全実装

#### FR-962: XP Pretty Printer (整形出力システム)

- **対象**: `packages/engine/vm/src/io.lisp`
- **内容**:
  - `(pprint form &optional stream)` — 整形出力
  - `(pprint-indent :block n stream)` / `(pprint-indent :current n stream)` — インデント制御
  - `(pprint-newline :linear stream)` / `:fill` / `:miser` / `:mandatory` — 改行種別
  - `(pprint-logical-block (stream list :prefix "(" :suffix ")") body)` — 論理ブロック
  - `(pprint-tab :section n m stream)` — タブ揃え
  - `(pprint-pop)` / `(pprint-exit-if-list-exhausted)` — リスト要素の反復出力
  - `*print-pretty*` / `*print-right-margin*` / `*print-miser-width*` 変数
  - ディスパッチテーブル: `*print-pprint-dispatch*` — 型別整形関数の登録
  - `(set-pprint-dispatch type function &optional priority)` — カスタム整形器の登録
- **根拠**: ANSI CL §22.2。Lisp コードの可読性のある出力に必須。XP (Richard Waters 1989) に基づく
- **難易度**: Hard

---

### Phase 185 — FORMAT ディレクティブ完全実装

#### FR-965: FORMAT Complete Directives (FORMAT完全実装)

- **対象**: `packages/engine/vm/src/io.lisp`
- **内容**:
  - `~A` / `~S` / `~W` — 基本出力
  - `~D` / `~B` / `~O` / `~X` — 整数（10進/2進/8進/16進）
  - `~F` / `~E` / `~G` / `~$` — 浮動小数点出力
  - `~C` — 文字出力 (`~:C` = 名前付き表示、`~@C` = 読み戻し可能形式)
  - `~P` — 複数形 (`"~D item~:P"`)
  - `~R` — 英語基数 (`~:R` = 序数、`~@R` = ローマ数字)
  - `~T` — タブ移動 (`~,T` = 相対、`~:T` = カラム揃え)
  - `~%` / `~&` / `~~` / `~|` / `~_` — 改行・ページ送り
  - `~(` / `~)` — 大文字小文字変換 (`~:@(`=全大文字等)
  - `~[`/`~]` — 条件分岐; `~{`/`~}` — 反復; `~<`/`~>` — 幅揃え; `~?` — 間接
  - `~*` — 引数のスキップ; `~^` — 反復の脱出
  - `*print-format-errors*` — フォーマットエラーの詳細報告
- **根拠**: ANSI CL §22.3。`format` は CL の主要出力手段、全ディレクティブの完全実装が必要
- **難易度**: Medium

---

### Phase 186 — READ マクロ完全実装

#### FR-968: Readtable Complete Implementation (Readtable完全実装)

- **対象**: `packages/frontend/parse/src/lexer.lisp`
- **内容**:
  - `(make-readtable)` / `(copy-readtable &optional from to)` — readtable 管理
  - `(set-macro-character char function &optional non-terminating-p readtable)` — マクロ文字登録
  - `(get-macro-character char &optional readtable)` → `(values function non-terminating-p)`
  - `(set-dispatch-macro-character disp-char sub-char function)` — `#` ディスパッチ文字
  - `(get-dispatch-macro-character disp-char sub-char)` → function
  - `(set-syntax-from-char to-char from-char &optional to-readtable from-readtable)` — 構文コピー
  - `*readtable*` — 現在のリードテーブル
  - `(with-readtable (rt) body)` — 一時的なリードテーブル切り替えマクロ
  - 標準ディスパッチマクロ文字: `#'`, `#(`, `#*`, `#:`, `#.`, `#+`, `#-`, `#|`, `#\`, `#b`, `#o`, `#x`, `#r`, `#c`, `#a`, `#s`, `#p`
- **根拠**: ANSI CL §23。`#'`, `#(vector)`, `#\char` 等のリードマクロは CL コードの読み取りに必須
- **難易度**: Medium

---

### Phase 187 — EVAL-WHEN の完全セマンティクス

#### FR-971: eval-when Full Semantics (eval-when完全セマンティクス)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(eval-when (:compile-toplevel :load-toplevel :execute) body)` — 3段階制御
  - `:compile-toplevel`: `compile-file` 中にコンパイラが実行 (マクロ定義に使用)
  - `:load-toplevel`: `load` 時に実行 (通常の `defun`/`defvar` と同じ)
  - `:execute`: `eval` または非コンパイル時に実行
  - ネスト処理: `eval-when` の内部 `eval-when` の正確なセマンティクス (ANSI §3.2.3.1)
  - `compile-file` ステージ: processing mode (compile/not-compile/eval) の伝播
  - `(eval-when (:compile-toplevel) (defmacro m ...))` — コンパイル時マクロ定義の標準パターン
  - `*compile-time-too*` フラグ — コンパイル時実行が必要かどうかの追跡
- **根拠**: ANSI CL §3.2.3。`eval-when` のセマンティクスは複雑で実装バグが多い箇所
- **難易度**: Hard

---

### Phase 188 — LOOP マクロ拡張

#### FR-974: LOOP Extended Clauses (LOOPクローズ拡張)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`
- **内容**:
  - `(loop for x :from 0 :below 10 :by 2)` — `:by` ステップの完全サポート
  - `(loop for (a b) :in list)` — デストラクチャリングバインディング
  - `(loop for x :being the hash-keys of ht :using (hash-value v))` — ハッシュテーブル反復
  - `(loop for x :being the symbols of *package*)` — パッケージシンボル反復
  - `(loop with x = init then update)` — 複数 `:with` クローズの相互参照
  - `(loop named outer ... (loop-finish))` — 名前付きループ + `loop-finish` からのネスト脱出
  - `(loop initially body)` / `(loop finally body)` — 初期化・後処理クローズ
  - `(loop for x :in seq :thereis (pred x))` — 短絡評価収集
  - `(loop for x :from 0 :downto -10)` — 逆方向反復
- **根拠**: ANSI CL §6.1。LOOP は CL の最も複雑なマクロの一つで、多くのエッジケースがある
- **難易度**: Medium

---

### Phase 189 — CLOS メソッドコンビネーション拡張

#### FR-977: Method Combination Types (メソッドコンビネーション種別)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
- **内容**:
  - 標準コンビネーション: `standard` (call-next-method / :before :after :around)
  - `(define-method-combination + :identity-with-one-argument t)` — `+` コンビネーション
  - 同様: `and` / `or` / `list` / `append` / `nconc` / `max` / `min` / `progn` コンビネーション
  - `(define-method-combination name &lambda-list options &body body)` — カスタム定義
  - `:most-specific-first` / `:most-specific-last` — メソッド順序
  - `(call-method method &optional next-methods)` — コンビネーション内のメソッド呼び出し
  - `(make-method form)` — コンビネーション内インラインメソッド
  - `(method-combination-error format &rest args)` — コンビネーションエラー報告
- **根拠**: ANSI CL §7.6.6 / AMOP §5。`+`/`append` コンビネーションは mixins パターンで多用
- **難易度**: Medium

---

### Phase 190 — 動的束縛の最適化

#### FR-980: Special Variable Lookup Optimization (スペシャル変数探索最適化)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - 現状: `(symbol-value sym)` → `*dynamic-env*` ハッシュテーブル検索 O(1) avg
  - 最適化1: シャドウスタック方式 — `(let ((*x* val)) body)` で `*x*` のスタックに push/pop
  - 最適化2: スレッドローカルストレージ (TLS) セル — 各スレッドが独立したセルを持つ
  - `(locally (declare (special *x*)) ...)` — スペシャル変数の局所宣言
  - `(boundp '*x*)` / `(makunbound '*x*)` — バインディング存在チェック・解除
  - `(progv '(*x* *y*) '(1 2) body)` — 動的変数リストのバインド（ANSI準拠）
  - プロファイリング: スペシャル変数アクセスのホットスポット検出
- **根拠**: SBCL の TLS cell + shadow stack。多用スペシャル変数のアクセスコストをO(1)に
- **難易度**: Medium

---

### Phase 191 — Tail Call 最適化 詳細

#### FR-983: Proper Tail Calls (適切な末尾呼び出し)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **内容**:
  - `(funcall fn args)` が末尾位置なら `vm-tail-call` 命令
  - `(apply fn args)` の末尾最適化
  - `let`/`let*`/`labels`/`flet` の本体部分の末尾位置認識
  - `cond`/`case`/`typecase` の全ブランチの末尾位置認識
  - `progn` の最後のフォームの末尾位置認識
  - `and`/`or` の末尾要素の末尾位置認識
  - 相互再帰の末尾最適化: `labels` 内での相互再帰をループに変換
  - `(declare (optimize (debug 0)))` での TCO 強制
  - 末尾位置の正確な定義 (ANSI §3.1.2.1.3): `tagbody` 内は末尾位置でない
- **根拠**: Scheme R7RS では適切な末尾再帰が必須要件。CL では宣言依存だが実用上重要
- **難易度**: Medium

---

### Phase 192 — 型システム 実行時チェック

#### FR-986: Runtime Type Checking (実行時型チェック)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(the type form)` — 型宣言（`(safety 0)` でチェック除去、`(safety 3)` でランタイム検証）
  - `(check-type place type &optional string)` — 実行時型チェック + `type-error` 条件
  - `(typep x 'type)` の高速パス: NaN-boxing タグで O(1) プリミティブ型判定
  - `(subtypep type1 type2)` — 型包含関係チェック（コンパイル時）
  - `(type-of x)` — 実行時の具体的な型を返す
  - `(satisfies pred)` 型: `(typep x '(satisfies evenp))` → `(evenp x)` の実行
  - 型エラーの詳細化: `(type-error-datum e)` / `(type-error-expected-type e)`
  - `(upgraded-array-element-type type)` — 配列要素型の実際のストレージ型
- **根拠**: ANSI CL §4。`check-type` は引数バリデーションの標準パターン
- **難易度**: Medium

---

### Phase 193 — コンパイル環境オブジェクト

#### FR-989: Compilation Environment (コンパイル環境オブジェクト)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/frontend/expand/src/expander.lisp`
- **内容**:
  - `(compile-file-pathname src &key output-file)` → FASL パス
  - `(compiled-function-p fn)` → boolean
  - `(compile name &optional definition)` — 単一関数のコンパイル
  - `(disassemble fn)` — コンパイル済み関数の逆アセンブル出力
  - `*compile-verbose*` / `*compile-print*` — コンパイル時出力制御
  - `(with-compilation-unit (:override t) body)` — 警告を集約してから出力
  - Environment オブジェクト: `(environment-p env)` / `(make-null-environment)`
  - `(variable-information name env)` → `(values kind local-p decls)`
  - `(function-information name env)` → `(values kind local-p decls)`
  - `(augment-environment env &key variable symbol-macro function macro declare)`
- **根拠**: ANSI CL §3.2 / CLTL2 環境API。ツールチェーン・IDE・コードウォーカーに必要
- **難易度**: Hard

---

### Phase 194 — 文字・文字列の完全準拠

#### FR-992: Character Names and Predicates (文字名・述語)

- **対象**: `packages/frontend/parse/src/lexer.lisp`, `packages/engine/vm/src/strings.lisp`
- **内容**:
  - 標準文字名: `#\Space` / `#\Newline` / `#\Tab` / `#\Return` / `#\Backspace` / `#\Rubout` / `#\Delete` / `#\Escape` / `#\Null` / `#\Altmode` / `#\Page`
  - `(char-name char)` → 文字名文字列または `nil`; `(name-char string)` → 文字
  - `(char-upcase c)` / `(char-downcase c)` — 大文字小文字変換
  - `(char= c1 c2)` / `(char< c1 c2)` 等 — 文字比較（case-sensitive）
  - `(char-equal c1 c2)` / `(char-lessp c1 c2)` 等 — case-insensitive比較
  - `(alpha-char-p c)` / `(digit-char-p c)` / `(alphanumericp c)` / `(graphic-char-p c)` / `(standard-char-p c)` / `(upper-case-p c)` / `(lower-case-p c)` / `(both-case-p c)`
  - `(char-code c)` / `(code-char n)` — Unicodeコードポイント変換
  - `(digit-char n &optional radix)` → 数字文字
- **根拠**: ANSI CL §13。文字述語はパーサ・スキャナの基本構成要素
- **難易度**: Easy

#### FR-993: String Operations Complete (文字列操作完全実装)

- **対象**: `packages/engine/vm/src/strings.lisp`
- **内容**:
  - `(string-upcase s)` / `(string-downcase s)` / `(string-capitalize s)` + `:start`/`:end`
  - `(string-trim chars s)` / `(string-left-trim chars s)` / `(string-right-trim chars s)`
  - `(string= s1 s2)` / `(string< s1 s2)` etc. + case-insensitive variants
  - `(string-search pattern s &key :start1 :end1 :start2 :end2 :from-end)` (= `search` on strings)
  - `(parse-integer s &key :radix :start :end :junk-allowed)` → integer + end-position
  - `(read-from-string s &optional eof-error-p eof-value &key :start :end :preserve-whitespace)`
  - `(write-to-string obj &key :base :radix :case ...)` — 全 `*print-*` 変数の尊重
  - `(format nil ...)` との統合
- **根拠**: ANSI CL §16。文字列操作は最も頻繁に使われる標準関数群
- **難易度**: Easy

---

### Phase 195 — Structure / Record 型

#### FR-996: defstruct Complete Implementation (defstruct完全実装)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**:
  - `(defstruct point x y)` — 基本構造体定義
  - `(make-point :x 1 :y 2)` — コンストラクタ（デフォルト名: `make-NAME`）
  - `(point-p obj)` — 型述語; `(point-x p)` / `(point-y p)` — アクセサ
  - `:conc-name` / `:constructor` / `:copier` / `:predicate` — オプション
  - スロットオプション: `(defstruct foo (x 0 :type fixnum :read-only t))`
  - `:include` 継承: `(defstruct (colored-point (:include point)) color)` — 単一継承
  - `:type` オプション: `(:type vector)` / `(:type list)` — ストレージ型選択
  - `(copy-point p)` — コピーコンストラクタ（デフォルトで生成）
  - `print-object` メソッドの自動生成
  - `*structure-types*` — 登録済み構造体型のハッシュテーブル
- **根拠**: ANSI CL §8。`defstruct` は CLOS以前からの軽量レコード型。CLOS より高速
- **難易度**: Medium

---

### Phase 196 — 型宣言の最適化統合

#### FR-999: Type Declaration Optimization (型宣言最適化統合)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(declare (type fixnum i))` → ループ内の型チェック除去
  - `(declare (type (simple-array double-float (*)) v))` → 配列アクセスの境界チェック除去
  - `(declare (dynamic-extent x))` → スタック割り当て（ヒープ不要）
  - `(declare (dynamic-extent #'fn))` → ローカル関数クロージャのスタック確保
  - `(declare (values fixnum))` → 多値の型情報伝播
  - 型伝播: 宣言された型を dataflow analysis で全使用箇所に伝播
  - `(safety 0)` + `(type fixnum x)` の組み合わせ: 全チェック除去
  - `(speed 3) (safety 0)` モード: Java HotSpot の -server フラグに相当する最速モード
- **根拠**: SBCL の型宣言効果。`declare` は CL の主要最適化ヒント機構
- **難易度**: Hard

---

### Phase 197 — イメージ保存・コアダンプ

#### FR-1002: save-lisp-and-die / Image Dump (イメージ保存)

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/runtime/src/runtime.lisp`
- **内容**:
  - `(save-lisp-and-die path &key toplevel executable compression)` — SBCL互換イメージ保存
  - 全ヒープ・スタック・コードをシリアライズしてファイルに書き出す
  - `executable = t`: ELF/Mach-O バイナリとして直接実行可能なイメージ
  - `toplevel` 関数: 次回起動時に呼び出すエントリポイント
  - `compression`: `zlib` / `lz4` / `zstd` でイメージを圧縮（50-70% 削減）
  - コアフォーマット: ヘッダ (magic, version, GC roots offset) + セグメント群 (code/data/heap)
  - `*saved-core-pathname*` — 現在実行中のコアファイルパス
  - `(lisp-implementation-version)` / `(lisp-implementation-type)` — ANSI CL §25.1
- **根拠**: SBCL `save-lisp-and-die`。Lisp の killer feature の一つ — 起動時間ゼロの実行ファイル生成
- **難易度**: Very Hard

#### FR-1003: Core File Loading (コアファイルロード)

- **対象**: `packages/cli/src/main.lisp`
- **内容**:
  - `./cl-cc --core path.core` — 保存済みイメージからの高速起動
  - `mmap` でコアをメモリにマップ → ページフォルト駆動の遅延ロード
  - GC roots の再スキャン: 保存済みヒープへの内部ポインタを修正 (relocation)
  - ASLR対応: position-independent core (全ポインタをベースアドレスからのオフセットで保存)
  - `--dynamic-space-size N` — ダイナミックスペースサイズ上書き
  - 起動時間目標: < 10ms（Erlang VM / LuaJIT 並み）
- **根拠**: Lisp イメージの高速起動。初回コンパイル後のロード時間をほぼゼロに
- **難易度**: Hard

---

### Phase 198 — OS インターフェース

#### FR-1006: Environment Variables / CLI Args (環境変数・コマンドライン引数)

- **対象**: `packages/cli/src/main.lisp`, `packages/engine/vm/src/io.lisp`
- **内容**:
  - `(getenv name)` → string または nil (`getenv(3)` ラッパー)
  - `(setenv name value)` / `(unsetenv name)` — 環境変数設定・削除 (`setenv(3)`)
  - `(environ)` → `((name . value) ...)` alist 全環境変数
  - `sb-ext:*posix-argv*` 互換: `*command-line-args*` — プログラム名除いた引数リスト
  - `(exit &optional (code 0))` — プロセス終了 (`_exit(2)`)
  - `(quit &optional code)` — `exit` のエイリアス (SBCL互換)
  - `(machine-instance)` / `(machine-type)` / `(machine-version)` — ANSI §25.1
  - `(software-type)` / `(software-version)` — OS情報
- **根拠**: ANSI CL §25.1 + SBCL 拡張。CLIツール・スクリプトの基本インフラ
- **難易度**: Easy

#### FR-1007: Process Management (プロセス管理)

- **対象**: `packages/engine/vm/src/io.lisp`
- **内容**:
  - `(run-program command args &key input output error wait)` — 外部プロセス起動
  - `:input` / `:output` / `:error`: `:pipe` (ストリーム) / `:null` / `:inherit` / stream
  - `(process-output process)` / `(process-input process)` — I/Oストリームアクセス
  - `(process-wait process &optional check-for-stopped)` — 終了待機
  - `(process-exit-code process)` → integer
  - `(process-kill process signal)` — シグナル送信
  - `(process-alive-p process)` → boolean
  - `(shell command)` → output-string (シェルコマンド実行の便利関数)
  - `fork(2)` + `execvp(2)` + `waitpid(2)` の直接ラッパー
- **根拠**: UIOP `:run-program` / SBCL `sb-ext:run-program`。ビルドシステム・CI パイプラインに必須
- **難易度**: Medium

---

### Phase 199 — POSIX シグナルハンドリング

#### FR-1010: Signal Handling (シグナルハンドリング)

- **対象**: `packages/backend/runtime/src/runtime.lisp`
- **内容**:
  - `(set-signal-handler signal function)` — シグナルハンドラ登録 (`sigaction(2)`)
  - `(get-signal-handler signal)` → function または `:default` / `:ignore`
  - `(signal-mask &rest signals)` → 旧マスク; `(unblock-signal signal)` — マスク解除
  - 非同期シグナルの安全な処理: handler は `*pending-signals*` に記録し、次のセーフポイントで実行
  - 標準シグナル: `SIGINT`(2), `SIGTERM`(15), `SIGHUP`(1), `SIGUSR1`(10), `SIGUSR2`(12), `SIGWINCH`(28)
  - `(with-signal-handler (sig fn) body)` — 一時的なハンドラ設定マクロ
  - `SIGINT` デフォルト: `keyboard-interrupt` condition を raise
  - `SIGTERM` デフォルト: グレースフルシャットダウン (`*shutdown-hooks*` を順次実行)
- **根拠**: POSIX.1-2017。サーバーアプリケーションのシグナル駆動ライフサイクル管理に必須
- **難易度**: Medium

#### FR-1011: Timer / Alarm (タイマー・アラーム)

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/backend/runtime/src/runtime.lisp`
- **内容**:
  - `(sleep seconds)` — ANSI CL §25.1.4（小数秒対応）; `nanosleep(2)` ベース
  - `(get-internal-real-time)` → 単調時刻 (ANSI §25.1.4)
  - `(get-internal-run-time)` → CPU時間
  - `(get-universal-time)` → ANSI universal time (1900年起算秒数)
  - `(decode-universal-time time &optional tz)` → 9値 (second minute hour date month year day dst tz)
  - `(encode-universal-time second minute hour date month year &optional tz)` → universal-time
  - `(get-decoded-time)` → 現在時刻を9値で返す
  - `(with-timeout (seconds) body)` — タイムアウトマクロ (`SIGALRM` / `setitimer` ベース)
- **根拠**: ANSI CL §25.1.4 全時刻関数。サーバー・ベンチマーク・スケジューリングの基盤
- **難易度**: Easy

---

### Phase 200 — ファイナライザ

#### FR-1014: Finalizers (ファイナライザ)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**:
  - `(finalize object function)` — obj が GC されるとき `(funcall function)` を実行
  - `(cancel-finalization object)` — ファイナライザの取り消し
  - ファイナライズキュー: GC後、死んだオブジェクトのファイナライザを専用スレッドで実行
  - 実行タイミング: major GC の sweep フェーズ後、GC スレッド外で安全に実行
  - 循環参照とファイナライザ: ファイナライザ内で対象オブジェクトを蘇生させる場合の処理
  - `*finalizer-thread*` — ファイナライザ実行スレッド
  - 弱参照との統合: `(make-weak-pointer obj)` + `(weak-pointer-value wp)` + finalize の組み合わせ
  - SBCL `sb-ext:finalize` / Trivia `trivial-garbage:finalize` との互換性
- **根拠**: C リソースラッパー（FFI で確保したメモリ・ファイルハンドル等）の自動解放に必須
- **難易度**: Hard

---

### Phase 201 — Symbol Macro / Setf Expander

#### FR-1017: define-symbol-macro / symbol-macrolet (シンボルマクロ)

- **対象**: `packages/frontend/expand/src/expander.lisp`
- **内容**:
  - `(define-symbol-macro name expansion)` — グローバルシンボルマクロ定義
  - `(symbol-macrolet ((name expansion) ...) body)` — ローカルシンボルマクロ
  - 展開: `name` が式として現れるとき `expansion` に置換
  - `(macroexpand name)` で展開可能（`macroexpand` は symbol-macro を処理）
  - `setf` との統合: `(setf name val)` → `(setf expansion val)` に展開
  - `(macroexpand-1 name)` → `(values expansion t)` when symbol-macro
  - CLOS との連携: CLOS スロットアクセスで `with-slots` の内部実装に使用
  - 環境オブジェクト中の symbol-macro情報: `(variable-information name env)` → `:symbol-macro`
- **根拠**: ANSI CL §3.1.2.1.2 `symbol-macrolet`。`with-slots` / `with-accessors` の実装基盤
- **難易度**: Medium

#### FR-1018: define-setf-expander (setf展開器)

- **対象**: `packages/frontend/expand/src/expander.lisp`
- **内容**:
  - `(define-setf-expander place-name lambda-list &body body)` — カスタムplace定義
  - 5値返却: `(get-setf-expansion place &optional env)` → `(vars vals stores store-form access-form)`
  - `vars`: テンポラリ変数リスト; `vals`: 評価値リスト
  - `stores`: ストア変数 (新しい値を受け取る); `store-form`: 書き込みフォーム; `access-form`: 読み取りフォーム
  - `(setf (ldb bytespec integer) value)` の実装例
  - `(incf place)` → `get-setf-expansion` → 展開して `(setq temp (1+ temp))` 等
  - `(define-setf-expander nth (n list) ...)` — `(setf (nth n list) val)` の正しい実装
  - 副作用の評価回数制御: 各サブフォームを正確に1回だけ評価する保証
- **根拠**: ANSI CL §5.1.2。カスタムplace の setf を可能にする機構
- **難易度**: Medium

---

### Phase 202 — エスケープ解析・クロージャ最適化

#### FR-1021: Escape Analysis (エスケープ解析)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - ヒープ割り当て不要の検出: `(let ((x (cons 1 2))) (car x))` → x は脱出しないのでスタック割り当て
  - 解析スコープ: 関数内ローカル + インライン済み呼び出し先
  - エスケープ条件: 関数外への return / global への store / `funcall` 引数 / ヒープ構造への格納
  - スタック割り当てへの変換: `vm-stack-alloc-cons` 命令
  - `(declare (dynamic-extent list))` との統合: ユーザー宣言でエスケープ解析を補助
  - `list` / `vector` / クロージャ / 構造体インスタンスに適用
  - インタープロシージャ解析: インライン展開後の再解析でエスケープ範囲を縮小
- **根拠**: HotSpot のスカラ置換 / SBCL の dynamic-extent。GC 負荷を根本的に削減
- **難易度**: Hard

#### FR-1022: Closure Conversion Optimization (クロージャ変換最適化)

- **対象**: `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - フラットクロージャ: 捕捉変数を環境ベクタにコピー（閉包チェーンなし）— 現状実装
  - 最適化: 捕捉変数が1つなら box 不要 — クロージャ本体にインライン
  - read-only 捕捉: 変数が変更されないなら値コピー（cell 不要）
  - 変更される捕捉変数: `cell` (cons ベースの間接参照) で実装
  - `(make-closure fn env)` での環境クロージャと `vm-closure` の対応
  - 環境共有の検出: 同一 `labels` 内の複数クロージャが同じ変数を捕捉 → 共有セル
  - `(flet ((f () x)))` での `x` の捕捉: セル不要（flet 内で変更なし）
- **根拠**: SML/NJ の closure conversion。クロージャ呼び出しの間接参照コスト削減
- **難易度**: Hard

---

### Phase 203 — Tail Recursion Modulo Cons (TRMC)

#### FR-1025: Tail Recursion Modulo Cons (末尾再帰Cons最適化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - TRMC パターン: `(defun copy-list (l) (if l (cons (car l) (copy-list (cdr l))) nil))`
  - 変換: 末尾cons をループに変換 — スタック O(n) → O(1)
  - 実装: `copy-list` の最後の引数が自己再帰で `cons` の cdr 位置にある場合を検出
  - forward pointer テクニック: cons セルを事前確保して cdr を後から書き込む
  - 適用可能パターン: `(cons a (self-recursive-call ...))` / `(list* a b (self-recursive-call ...))`
  - `(declare (optimize (tail-recursion-modulo-cons t)))` で有効化
  - `map` / `filter` / `append` の自動TRMC変換
- **根拠**: OCaml 5 のTRMC最適化 (2022)。`map`/`filter` のスタックオーバーフロー問題を解決
- **難易度**: Hard

---

### Phase 204 — パッケージローカルニックネーム

#### FR-1028: Package-Local Nicknames (パッケージローカルニックネーム)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/frontend/parse/src/lexer.lisp`
- **内容**:
  - `(defpackage :my-pkg (:local-nicknames (:a :alexandria) (:s :serapeum)))` — ローカル略称
  - `a:iota` → `alexandria:iota` (`:my-pkg` 内でのみ有効)
  - `(add-package-local-nickname nick pkg &optional in-package)` — 動的追加
  - `(remove-package-local-nickname nick &optional in-package)` — 削除
  - `(package-local-nicknames package)` → `((nick . full-pkg) ...)`
  - リーダー統合: `*readtable*` とパッケージのローカルニックネームを組み合わせて解決
  - SBCL / CCL / ECL / ABCL 全てが実装済みの事実上の標準拡張
  - `:global-nicknames` と `:local-nicknames` の優先順位
- **根拠**: SBCL `sb-ext:add-package-local-nickname`。大規模コードベースでのパッケージ名衝突回避
- **難易度**: Medium

---

### Phase 205 — バイトコード中間層

#### FR-1031: Bytecode Interpreter Layer (バイトコードインタープリタ層)

- **対象**: `packages/engine/vm/src/vm.lisp` (新規: `packages/engine/vm/src/bytecode.lisp`)
- **内容**:
  - コンパクトバイトコード形式: 1バイトオペコード + 可変長オペランド
  - 命令セット: `CONST u16` / `LOAD u8` / `STORE u8` / `CALL u8` / `TCALL u8` / `RET` / `JMP i16` / `JNIL i16` / `CLOSURE u16` / `MAKE-ENV u8`
  - バイトコードはネイティブコードの 1/5 サイズ
  - インタープリタ: C の computed goto (threaded dispatch) に相当する Lisp ループ
  - `(compile-to-bytecode form)` → bytecode-function
  - `(bytecode-disassemble fn)` → 人間が読める形式
  - JIT ティアリング: 実行回数 > 閾値でネイティブコードに昇格
  - `*bytecode-threshold*` — JIT 昇格まで何回バイトコードで実行するか
- **根拠**: LuaJIT の bytecode VM / CPython のバイトコード。起動速度と実行速度のトレードオフ
- **難易度**: Hard

---

### Phase 206 — CLOS クラス変更・再初期化

#### FR-1034: change-class (クラス変更)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
- **内容**:
  - `(change-class instance new-class &rest initargs)` — インスタンスのクラスを動的変更
  - AMOP 準拠: `update-instance-for-different-class` の呼び出し
  - `(update-instance-for-different-class previous current &rest initargs)` — メソッドフック
  - スロットマイグレーション: 共通スロットは値を保持、新規スロットは `initargs` または `initform`
  - 削除されたスロットの値は `previous` の shallow copy で参照可能
  - `(defmethod update-instance-for-different-class :after (prev curr &key) ...)` — カスタム処理
  - `change-class` 後の型述語: `(typep instance 'new-class)` → t
- **根拠**: ANSI CL §7.3 / AMOP §2。プロトタイプベース開発・ライブアップグレードに使用
- **難易度**: Hard

#### FR-1035: reinitialize-instance (インスタンス再初期化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
- **内容**:
  - `(reinitialize-instance instance &rest initargs)` — 既存インスタンスを再初期化
  - `update-instance-for-redefined-class` との区別: 同一クラス内の再初期化
  - `(defmethod reinitialize-instance :after ((obj my-class) &key)` — カスタムフック
  - `shared-initialize` への委譲: `reinitialize-instance` → `shared-initialize` の呼び出し
  - initargs 検証: `:allow-other-keys` なしの場合、不明なキーは `error`
  - `(initialize-instance obj &rest initargs)` との違い: `reinitialize-instance` はオブジェクトを受け取る
- **根拠**: ANSI CL §7.3.4。設定オブジェクトのリセット・スロット再設定に使用
- **難易度**: Medium

---

### Phase 207 — ASDF インテグレーション

#### FR-1038: ASDF System Definition (ASDFシステム定義)

- **対象**: `cl-cc.asd`
- **内容**:
  - `defsystem` の完全処理: `:components` / `:depends-on` / `:serial` / `:in-order-to`
  - コンポーネント種別: `:file` / `:module` / `:system` / `:static-file`
  - `:around-compile-hook` — コンパイル時処理のカスタマイズ（宣言の自動挿入等）
  - `:default-component-class` — デフォルトコンポーネント型の変更
  - `operate` プロトコル: `asdf:compile-op` / `asdf:load-op` / `asdf:test-op`
  - `(asdf:system-source-directory system)` → pathname
  - `(asdf:find-system name)` → system オブジェクト
  - `(asdf:component-pathname component)` → pathname
  - システム依存グラフの DAG 構築 + トポロジカルソート
  - `(asdf:oos 'asdf:load-op "my-system" :verbose nil)` — ロード最適化
- **根拠**: ASDF は CL の事実上の標準ビルドシステム。完全サポートで Quicklisp 互換性確保
- **難易度**: Medium

#### FR-1039: System Dependency Resolution (システム依存解決)

- **対象**: `packages/cli/src/main.lisp`
- **内容**:
  - `(ql:quickload "system-name")` — Quicklisp互換ロード API
  - `*quicklisp-client-directory*` / `*local-project-directories*` — 検索パス
  - `(ql:system-apropos "search-term")` — システム名検索
  - `(ql:update-all-dists)` — ディストリビューション更新
  - ローカルオーバーライド: `~/quicklisp/local-projects/` 優先
  - `(ql:uninstall "system")` — アンインストール
  - `qlfile` / `qlfile.lock` フォーマット: Roswell / qlot 互換
  - HTTP ダウンロード + SHA256 検証 + tar.gz 展開の実装
- **根拠**: Quicklisp は CL エコシステムの中心。互換 API でライブラリエコシステムへのアクセスを提供
- **難易度**: Hard

---

### Phase 208 — オブジェクトシリアライゼーション

#### FR-1042: Object Serialization (オブジェクトシリアライズ)

- **対象**: `packages/engine/vm/src/io.lisp`
- **内容**:
  - `*print-readably*` = t でのオブジェクト出力: `write` が再読み取り可能な形式を生成
  - `(make-load-form object &optional environment)` — ロード可能フォームの生成 (ANSI §3.2.4.4)
  - `make-load-form-saving-slots` — スロット値を保存する汎用実装
  - `(write-to-fasl object stream)` / `(read-from-fasl stream)` — FASL シリアライゼーション
  - `print-unreadable-object` マクロ: `#<Class :at address>` 形式の出力
  - `(with-standard-io-syntax body)` — 全 `*print-*` 変数を標準値に束縛
  - `*print-length*` / `*print-level*` / `*print-circle*` / `*print-array*` / `*print-gensym*`
  - S式シリアライゼーション: `(read)` で再構築可能な形式への変換ルール
- **根拠**: ANSI CL §22.1。開発環境での inspect/describe、FASL生成の基盤
- **難易度**: Medium

---

### Phase 209 — クラス割当スロット (class-allocated slots)

#### FR-1045: Class-Allocated Slots (クラス割当スロット)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
- **内容**:
  - `(defclass foo () ((counter :allocation :class :initform 0)))` — クラスレベルスロット
  - `:allocation :class`: 全インスタンスで共有される1つの値
  - `:allocation :instance` (デフォルト): 各インスタンスが独立した値
  - `(slot-value instance 'counter)` — インスタンス経由でクラススロットにアクセス
  - `(setf (slot-value instance 'counter) n)` — 全インスタンスに影響
  - クラスストレージ: クラスメタオブジェクト内の `class-slots-storage` ハッシュテーブル
  - `(class-slot-value class 'counter)` — クラス直接アクセス (MOP拡張)
  - サブクラスでの継承: `:allocation :class` スロットはサブクラスでも共有
- **根拠**: ANSI CL §7.5.3。グローバルカウンタ・共有キャッシュ・デフォルト設定の実装に使用
- **難易度**: Medium

---

### Phase 210 — インライン展開詳細制御

#### FR-1048: Inlining Policy and Expansion (インライン展開ポリシー)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(declaim (inline fn))` — コンパイル時インライン宣言
  - `(declaim (notinline fn))` — インライン禁止（デバッグ・プロファイリング用）
  - インライン展開: 呼び出しサイトにfn の本体をコピー → α変換（変数名衝突回避）
  - 展開サイズ制限: `*inline-expansion-limit*` (デフォルト200ノード) — 巨大関数のインライン防止
  - 再帰関数のインライン: 深さ制限 (`*inline-recursion-depth*` = 2)
  - コンパイラノート: インライン展開の成否をユーザーに報告
  - `define-compiler-macro` との違い: `inline` は定義をコピー、compiler-macro は変換を定義
  - `sb-ext:maybe-inline` 相当: ヒューリスティック判断でのインライン
- **根拠**: ANSI CL `inline` 宣言。小さなユーティリティ関数の関数呼び出しオーバーヘッド除去
- **難易度**: Medium

---

### Phase 211 — プリンタ変数完全実装

#### FR-1051: Print Control Variables (プリンタ制御変数完全実装)

- **対象**: `packages/engine/vm/src/io.lisp`
- **内容**:
  - `*print-case*` → `:upcase` / `:downcase` / `:capitalize` — シンボル出力の大文字小文字
  - `*print-base*` — 整数出力の基数 (2-36); `*print-radix*` — 基数接頭辞 `#b` 等
  - `*print-length*` — リスト要素の最大表示数 (`...` で省略)
  - `*print-level*` — ネスト深さの最大表示数 (`#` で省略)
  - `*print-circle*` — 循環構造の `#n=` / `#n#` 表記
  - `*print-escape*` — エスケープ文字の出力 (`nil` で `princ` 相当)
  - `*print-gensym*` — gensym の `#:` 接頭辞
  - `*print-array*` — 配列内容の出力 (`nil` で `#<Array>` 表示)
  - `*print-lines*` — 出力行数上限
  - `*print-right-margin*` — pretty-printer の折り返し列
  - `(write obj &key stream :case :base :radix :level :length :circle :escape :gensym :array :pretty)` — キーワードで全変数を上書き
- **根拠**: ANSI CL §22.1.3 全 `*print-*` 変数。デバッグ出力の完全制御に必要
- **難易度**: Medium

---

### Phase 212 — READ 制御変数・数値入力

#### FR-1054: READ Control Variables (READ制御変数)

- **対象**: `packages/frontend/parse/src/lexer.lisp`
- **内容**:
  - `*read-base*` — 整数のデフォルト読み取り基数 (デフォルト10)
  - `*read-eval*` — `#.` の有効化フラグ (セキュリティ: デフォルト `t`)
  - `*read-suppress*` — `#+`/`#-` で抑制されたフォームの読み飛ばし時 `t`
  - `*read-default-float-format*` — 指数なし浮動小数点の型 (`single-float`/`double-float`)
  - `(read &optional stream eof-error-p eof-value recursive-p)` — 完全シグネチャ
  - `(read-preserving-whitespace ...)` — 区切り文字を読み戻す
  - `(read-delimited-list char &optional stream recursive-p)` — `)` 等の区切り文字まで読む
  - `(read-char-no-hang &optional stream eof-error-p eof-value recursive-p)` — ノンブロッキング文字読み取り
  - `(peek-char &optional peek-type stream eof-error-p eof-value recursive-p)` — 先読み
  - `(unread-char char &optional stream)` — 1文字プッシュバック
- **根拠**: ANSI CL §23.1 全 `read` 関数・変数。REPL・パーサ・DSLの基盤
- **難易度**: Easy

---

### Phase 213 — Numeric I/O 完全対応

#### FR-1057: Numeric Literal Reader (数値リテラルリーダー)

- **対象**: `packages/frontend/parse/src/lexer.lisp`
- **内容**:
  - `#b1010` — 2進数; `#o17` — 8進数; `#xFF` — 16進数
  - `#36rZZZ` — 任意基数 (2-36): `#nrNNN` 形式
  - `1.5e3` / `1.5d3` / `1.5f3` / `1.5l3` — 指数表記 (s=short, f=single, d=double, l=long)
  - `1/3` — 有理数リテラル
  - `#C(1.0 2.0)` — 複素数リテラル
  - `#*10110` — bit-vector リテラル
  - `1_000_000` — アンダースコア区切り (読みやすさ用、非ANSI拡張)
  - `+inf.0` / `-inf.0` / `+nan.0` — 無限大・非数リテラル (SBCL拡張)
  - `(float-precision x)` — 仮数部ビット数; `most-positive-double-float` 等の定数
- **根拠**: ANSI CL §2.3 数値シンタックス。全数値リテラル形式の正確な読み取り
- **難易度**: Easy

---

### Phase 214 — 動的ウィンド / リソース管理

#### FR-1060: dynamic-wind Semantics (動的ウィンドセマンティクス)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`
- **内容**:
  - `(dynamic-wind before thunk after)` — Scheme 相当の before/after フック
  - CL の `unwind-protect` との対応: `(dynamic-wind pre body post)` ≈ `(unwind-protect (progn (pre) (body)) (post))`
  - 相違点: `dynamic-wind` は継続の再入 (re-entry) でも `before` を再実行
  - `call/cc` との統合: 継続を escape して再呼び出しするたびに before/after が実行される
  - `(with-dynamic-extent form)` — `dynamic-wind` ベースのリソース管理マクロ
  - `(unwind-protect-with-restarts form cleanup)` — リスタートを保持したままのクリーンアップ
  - Racket `dynamic-wind` / R7RS `dynamic-wind` との互換性
- **根拠**: Scheme R7RS §6.10。フルcall/ccと組み合わせたリソース管理の正確なセマンティクス
- **難易度**: Hard

---

### Phase 215 — インタープロシージャ最適化

#### FR-1063: Interprocedural Optimization (インタープロシージャ最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 呼び出しグラフ構築: `(build-call-graph toplevel-fns)` → call-graph DAG
  - 呼び出し側への定数伝播: `(foo 42)` で `foo` の本体が `x=42` として最適化される
  - 純粋関数の検出: 副作用なし + 参照透明 → `(foo 42)` を複数箇所からインライン
  - 使用されない関数の除去 (dead function elimination): 呼び出しグラフで到達不能な関数を削除
  - 部分インライン: 条件分岐の一方のブランチだけをインライン（ガード条件の外側）
  - モノモーフィゼーション: 型既知の呼び出しサイト用に特殊化版を生成
  - `*ipo-budget*` — インタープロシージャ解析の計算予算（時間制限）
- **根拠**: GCC `-fwhole-program` / LLVM LTO。モジュール境界を越えた最適化
- **難易度**: Very Hard

---

### Phase 216 — SBCL 互換拡張 API

#### FR-1066: SBCL-Compatible Extensions (SBCL互換拡張API)

- **対象**: `packages/engine/vm/src/vm.lisp`
- **内容**:
  - `sb-ext:without-package-locks` → `cl-cc:without-package-locks` エイリアス
  - `sb-ext:gc` → `cl-cc:gc` (Phase 177)
  - `sb-ext:*heap-size*` → `cl-cc:*heap-size*`
  - `sb-ext:native-namestring` → `cl-cc:native-namestring`
  - `sb-ext:run-program` → `cl-cc:run-program` (Phase 198)
  - `sb-ext:finalize` → `cl-cc:finalize` (Phase 200)
  - `sb-ext:add-package-local-nickname` → `cl-cc:add-package-local-nickname` (Phase 204)
  - `sb-impl::*default-external-format*` → `cl-cc:*default-external-format*`
  - `*features*` リスト: `:cl-cc` / `:common-lisp` / `:ansi-cl` / `:x86-64` / `:unix` / `:darwin` / `:linux`
  - `(lisp-implementation-type)` → `"CL-CC"` / `(lisp-implementation-version)` → "0.1.0"
- **根拠**: SBCL は最も広く使われる CL 処理系。互換 API でポータビリティを確保
- **難易度**: Easy

---

### Phase 217 — ブロック・非局所脱出の完全実装

#### FR-1069: block / return-from Across Closures (クロージャ越し非局所脱出)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(block name ...)` / `(return-from name value)` の完全セマンティクス
  - クロージャ越し `return-from`: `(block outer (let ((f (lambda () (return-from outer 42)))) (funcall f)))` → 42
  - 実装: block は `catch` タグに変換; `return-from` は `throw` に変換（現状）
  - 最適化: 同一関数内での `return-from` はジャンプ命令に直接変換（`throw` 不要）
  - `(block nil ...)` + `(return ...)` の等価性: `(return val)` = `(return-from nil val)`
  - 非局所脱出と `unwind-protect` の相互作用: 脱出経路上の cleanup の実行
  - 無効な `return-from`: ブロックが既にリターンした後の `return-from` → `control-error`
- **根拠**: ANSI CL §5.2。CL の `loop` / `dolist` / `dotimes` は内部で `(block nil ...)` を使用
- **難易度**: Medium

#### FR-1070: tagbody / go Complete Semantics (tagbody/go完全セマンティクス)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(tagbody tag1 form1 tag2 form2 ...)` — タグ付きステートメント
  - `(go tag)` — 非ローカルジャンプ（`tagbody` 内の任意のタグへ）
  - クロージャ越しの `go`: `(let ((f (lambda () (go tag2)))) ...)` — キャプチャされた `go`
  - タグの種類: シンボル または 整数
  - `tagbody` の戻り値は常に `nil`
  - 末尾位置の定義: `tagbody` 内の `go` は末尾位置でない (TCO 不可)
  - `(do ...)` / `(dotimes ...)` / `(dolist ...)` の内部実装: `tagbody` への変換
  - `prog` / `prog*` の実装: `let`/`let*` + `tagbody` のシュガー
- **根拠**: ANSI CL §5.3。`do`/`loop` の基盤。CPS変換後のラベルジャンプとの対応
- **難易度**: Medium

---

### Phase 218 — 数値比較・等値の完全実装

#### FR-1073: Numeric Equality and Comparison (数値比較完全実装)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **内容**:
  - `(= a b)` / `(/= a b)` / `(< a b)` / `(> a b)` / `(<= a b)` / `(>= a b)` — 任意型間の比較
  - 多引数版: `(< a b c d)` = `(and (< a b) (< b c) (< c d))` のショートサーキット評価
  - `(eql a b)` — 同一オブジェクト or 数値同値 (fixnum / float / complex)
  - `(equal a b)` — 再帰的構造比較 (cons/string/vector)
  - `(equalp a b)` — case-insensitive文字列 + 配列要素比較
  - `(= 1 1.0)` → t (型を超えた数値同値); `(eql 1 1.0)` → nil (型が異なる)
  - `(zerop x)` / `(plusp x)` / `(minusp x)` / `(oddp x)` / `(evenp x)` — 述語
  - `(max a b ...)` / `(min a b ...)` — 数値最大最小 (contagion rules 適用)
- **根拠**: ANSI CL §12.1.4。CL の等値述語の階層 (eq/eql/equal/equalp) は重要な設計概念
- **難易度**: Easy

---

### Phase 219 — 一般シーケンス操作完全実装

#### FR-1076: Sequence Operations Complete (シーケンス操作完全実装)

- **対象**: `packages/engine/vm/src/list.lisp`
- **内容**:
  - `(find item seq &key :key :test :test-not :start :end :from-end)` — 要素検索
  - `(position item seq &key ...)` → integer or nil
  - `(count item seq &key ...)` → integer
  - `(find-if pred seq &key ...)` / `(find-if-not pred seq &key ...)`
  - `(remove-duplicates seq &key :key :test :from-end :start :end)` — 重複除去
  - `(substitute new old seq &key :test :count :from-end)` / `(substitute-if new pred seq ...)`
  - `(nsubstitute ...)` / `(nsubstitute-if ...)` — 破壊的版
  - `(every pred seq &rest more-seqs)` / `(some ...)` / `(notany ...)` / `(notevery ...)` — 量化
  - `(mismatch seq1 seq2 &key :test :start1 :end1 :start2 :end2 :from-end)` → position
  - `(concatenate result-type &rest seqs)` — 任意のシーケンスの結合
  - `(coerce seq result-type)` — シーケンス型変換
- **根拠**: ANSI CL §17。シーケンス関数は CL の最も使われる標準ライブラリ関数群の一つ
- **難易度**: Medium

---

### Phase 220 — ハッシュテーブル SIMD 最適化

#### FR-1079: Hash Table SIMD Lookup (ハッシュテーブルSIMD高速化)

- **対象**: `packages/engine/vm/src/hash.lisp`
- **内容**:
  - 現状: オープンアドレス法 (linear probing)
  - 最適化: Swiss Table (Abseil) / F14 (Facebook) — SIMD による16スロット並列比較
  - x86-64 `SSE4.2 PCMPEQB` / ARM `NEON vceq` で16バイトのタグを一括比較
  - メタデータバイト: 各スロットに7ビットハッシュ値を格納 → ミスヒット時の早期脱出
  - 負荷率 87.5% まで効率的（通常のopenaddressing の70%より高い）
  - `(hash-table-size ht)` → 現在の容量; `(hash-table-count ht)` → 要素数
  - `(maphash fn ht)` — 反復関数; `(with-hash-table-iterator (next ht) body)` — イテレータ
  - `*hash-table-rehash-policy*` — `:swiss` / `:robin-hood` / `:chaining` ポリシー選択
- **根拠**: Swiss Table は Abseil/C++17 の標準。ルックアップ速度2-4x向上、キャッシュ効率大幅改善
- **難易度**: Hard

---

### Phase 221 — ソース位置追跡・デバッグ情報

#### FR-1082: Source Location Tracking (ソース位置追跡)

- **対象**: `packages/frontend/parse/src/cl/parser.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `*load-pathname*` / `*load-truename*` — 現在ロード中のファイルパス (ANSI §23.1.2)
  - `*compile-file-pathname*` / `*compile-file-truename*` — コンパイル中ファイルパス
  - `(ed &optional x)` — エディタ起動 (ANSI §25.1.4)
  - `(dribble &optional pathname)` — セッションログ記録
  - ソース位置オブジェクト: `(make-source-location file line column)` + アクセサ
  - DWARF `.debug_line` セクション: ネイティブコードのPC → ソース行マッピング
  - `(source-location function)` → ソース位置 (Emacs `M-.` の基盤)
  - `(inspect object)` — 対話的オブジェクト調査 (ANSI §25.1.4)
  - `(describe object &optional stream)` — オブジェクト説明出力
- **根拠**: ANSI CL §25.1 デバッグ関連関数。開発環境の基本インフラ
- **難易度**: Medium

---

### Phase 222 — Trace / Step / Break デバッガ

#### FR-1085: trace / step / break (トレース・ステップ・ブレーク)

- **対象**: `packages/engine/vm/src/conditions.lisp`
- **内容**:
  - `(trace function-name ...)` — 関数呼び出しの自動ログ
  - `(untrace function-name ...)` / `(untrace)` — トレース解除
  - トレース出力: `0: (FOO 1 2)` → `0: FOO returned 3` の入れ子インデント
  - `*trace-output*` — トレース出力ストリーム
  - `(step form)` — ステップ実行モード (ANSI §25.1.4)
  - `(break &optional format-control &rest args)` — デバッガへのブレーク
  - `(invoke-debugger condition)` — `*debugger-hook*` 経由でデバッガ起動
  - `*debugger-hook*` — カスタムデバッガ関数
  - SLYNK/Swank 互換デバッグプロトコル: `slynk:create-server` の互換実装基盤
- **根拠**: ANSI CL §25.1 `trace`/`step`/`break`/`inspect`/`describe`。CL開発の必須デバッグツール
- **難易度**: Medium

---

### Phase 223 — 数値出力アルゴリズム

#### FR-1088: Ryu Float-to-String (Ryu浮動小数点→文字列)

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/engine/vm/src/primitives.lisp`
- **内容**:
  - Ryu アルゴリズム (Ulf Adams, 2018): double → 最短の十進数表現
  - 性質: `(= (parse-float (float-to-string x)) x)` が常に成立する最短表現
  - `Grisu3` / `Dragon4` より 2-3x 高速
  - 特殊値: `+inf.0` / `-inf.0` / `+nan.0` の出力
  - `(format t "~F" x)` / `(write x)` のバックエンドとして使用
  - `:e` (指数) / `:f` (固定) / `:g` (最短) モード
  - 単精度 `float` と倍精度 `double` の両対応
  - ベンチマーク比較: Ryu vs Grisu3 vs Dragon4 vs Steele&White
- **根拠**: Ryu は現在最速・最短の double→string アルゴリズム。V8/Dart/Swift が採用
- **難易度**: Medium

---

### Phase 224 — コンパイル時 CLOS 最適化

#### FR-1091: Monomorphic Inline Cache for slot-value (スロットアクセス単相インラインキャッシュ)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(slot-value obj 'name)` の呼び出しサイトに MIC (Monomorphic Inline Cache) を設置
  - キャッシュヒット: `class-of(obj) == cached-class` → スロットインデックス直接参照
  - キャッシュミス: 通常のスロット検索 + キャッシュ更新
  - PIC (Polymorphic Inline Cache): 最大4クラスをキャッシュ
  - メガモーフィック: 5クラス以上 → グローバルハッシュテーブルへ格帰
  - `(with-slots (a b c) obj body)` の展開先も MIC に変換
  - `(slot-value-using-class class obj slotd)` MOP フック経由でも同様に最適化
- **根拠**: Self VM の Polymorphic Inline Cache。CLOS スロットアクセスのホットスポット削減
- **難易度**: Hard

---

### Phase 225 — 弱ポインタ完全実装

#### FR-1094: Weak Pointers (弱ポインタ完全実装)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**:
  - `(make-weak-pointer object)` → weak-pointer
  - `(weak-pointer-value wp)` → `(values object valid-p)` — GC済みなら `(values nil nil)`
  - `(weak-pointer-p x)` → boolean
  - GC 処理: major GC sweep フェーズで weak pointer の target が白なら nil に設定
  - 弱ハッシュテーブル:
    - `(make-weak-hash-table :weakness :key)` — キーが死ぬとエントリ削除
    - `(make-weak-hash-table :weakness :value)` — 値が死ぬとエントリ削除
    - `(make-weak-hash-table :weakness :key-and-value)` — 両方死ぬと削除 (ephemeron)
    - `(make-weak-hash-table :weakness :key-or-value)` — どちらか死ぬと削除
  - `trivial-garbage:make-weak-pointer` との互換性
- **根拠**: SBCL `sb-ext:make-weak-pointer`。インターニングキャッシュ・イベントリスナーのメモリリーク防止
- **難易度**: Hard

---

### Phase 226 — ネイティブスレッド完全実装

#### FR-1097: Native Thread API (ネイティブスレッドAPI)

- **対象**: `packages/engine/vm/src/vm.lisp`
- **内容**:
  - `(make-thread function &key name)` → thread オブジェクト
  - `(thread-join thread &optional timeout)` → 戻り値
  - `(thread-name thread)` → string
  - `(current-thread)` → 現在スレッド
  - `(thread-alive-p thread)` → boolean
  - `(all-threads)` → thread list
  - `(interrupt-thread thread function)` — 他スレッドへの割り込み (SBCL互換)
  - `(destroy-thread thread)` — 強制終了（非推奨）
  - `(thread-yield)` — スケジューラへのCPU譲渡
  - Bordeaux-Threads API 互換: `bt:make-thread` / `bt:join-thread` / `bt:current-thread`
  - GIL (Global Interpreter Lock) 排除: 各スレッドが独立した GC state を持つ
- **根拠**: Bordeaux-Threads は CL の事実上の標準スレッドライブラリ。互換API で移植性確保
- **難易度**: Hard

---

### Phase 227 — 標準入出力・ターミナル制御

#### FR-1100: Terminal Control (ターミナル制御)

- **対象**: `packages/engine/vm/src/io.lisp`
- **内容**:
  - `(make-ansi-stream stream)` — ANSI エスケープシーケンス対応ストリーム
  - `(ansi-color stream :red)` / `(ansi-reset stream)` — 文字色・スタイル設定
  - `(terminal-size)` → `(values columns rows)` (`TIOCGWINSZ` ioctl)
  - `(isatty stream)` → boolean — ターミナル接続確認
  - `(with-raw-terminal body)` — raw モード (行バッファリング無効): `tcsetattr(3)` 使用
  - `(read-key)` → keypress event — single keystroke 読み取り
  - `*terminal-width*` — `*print-right-margin*` のデフォルト設定に使用
  - Readline互換行編集: `(readline prompt)` → string（履歴・補完付き）
- **根拠**: CLIツール・REPL・TUIアプリケーションのターミナル操作基盤
- **難易度**: Medium

---

### Phase 228 — Printing 循環構造

#### FR-1103: Circular Structure Printing (循環構造印字)

- **対象**: `packages/engine/vm/src/io.lisp`
- **内容**:
  - `*print-circle*` = t での循環・共有構造の検出と印字
  - 検出アルゴリズム: 2パス — 第1パスで共有オブジェクトを `#n=` でラベル付け、第2パスで印字
  - `#1=(a b . #1#)` — 循環リストの表現
  - `(let ((x (list 1 2))) (list x x))` → `(#1=(1 2) #1#)` — 構造共有の表現
  - `write-circle-table` — 循環/共有オブジェクトのインデックステーブル
  - `*print-circle*` = nil でも循環リストを安全に印字（`...` で打ち切り）
  - `(print-object ...)` カスタムメソッドと `*print-circle*` の統合
- **根拠**: ANSI CL §22.1.2。循環構造のデバッグ・シリアライズに必須
- **難易度**: Medium

---

### Phase 229 — ローカル宣言の完全実装

#### FR-1106: Local Declarations Complete (ローカル宣言完全実装)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(declare (ignore x))` — 未使用変数の警告抑制
  - `(declare (ignorable x))` — 使用されなくても OK
  - `(declare (type fixnum x))` — ローカル型宣言
  - `(declare (special *x*))` — ローカルスペシャル変数宣言
  - `(declare (optimize (speed 2) (safety 1)))` — ローカル最適化ポリシー
  - `(declare (dynamic-extent x))` — スタック割り当て宣言
  - `(declare (notinline f))` — ローカルインライン抑制
  - `locally` 特殊形: `(locally (declare (optimize (speed 3))) body)` — 宣言のみの局所スコープ
  - 複数 `declare` フォームの合成: 同一 `let` 内の複数 `declare` は全て有効
- **根拠**: ANSI CL §3.3 `declare`。警告制御・型最適化・スタック割り当ての標準手段
- **難易度**: Medium

---

### Phase 230 — コードウォーカー・マクロ展開完全実装

#### FR-1109: macroexpand-all / Code Walker (コードウォーカー)

- **対象**: `packages/frontend/expand/src/expander.lisp`
- **内容**:
  - `(macroexpand form &optional env)` — ANSI CL `macroexpand` (再帰展開、マクロでなくなるまで)
  - `(macroexpand-1 form &optional env)` — 1ステップのみ展開
  - `(macroexpand-all form &optional env)` — 全サブフォームを再帰的に展開 (SBCL `sb-cltl2:macroexpand-all`)
  - コードウォーカー: `(walk-form form env visitor)` — 全サブフォームを訪問
  - `visitor` コールバック: `(fn form env context)` → `(values new-form walk-children-p)`
  - `context`: `:eval` / `:macro` / `:function` / `:variable` — 出現コンテキスト
  - `:macro-function` と `:compiler-macro-function` の両方を展開
  - `symbol-macrolet` / `macrolet` のスコープを正確に追跡
- **根拠**: SBCL `sb-cltl2:macroexpand-all` / CLTL2 code walker API。静的解析・最適化・変換ツールの基盤
- **難易度**: Hard

---

### Phase 231 — 数値最大最小・クランプ

#### FR-1112: Numeric Clamp / Range (数値クランプ・範囲)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **内容**:
  - `(clamp x min max)` = `(max min (min max x))` — 範囲クランプ（非ANSI拡張）
  - `(wrap x min max)` — 範囲外を折り返す: `(mod (- x min) (- max min)) + min`
  - `(lerp a b t)` = `(+ a (* t (- b a)))` — 線形補間
  - `(abs x)` — ANSI CL §12.2 (全数値型)
  - `(signum x)` → -1 / 0 / 1 (整数) or -1.0 / 0.0 / 1.0 (浮動小数点)
  - `(expt base power)` — 整数・有理数・複素数・浮動小数点の全組み合わせ
  - `(log x)` / `(log x base)` — 自然対数・任意底対数
  - `(sqrt x)` / `(isqrt x)` — 平方根（複素数 `(sqrt -1)` = `#C(0 1)`）
- **根拠**: ANSI CL §12.2。数値演算関数の完全実装
- **難易度**: Easy

---

### Phase 232 — ガベージコレクタ 精密スタックマップ

#### FR-1115: Precise Stack Maps (精密スタックマップ)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - 各コールサイト/safepoint での「どのレジスタ・スタックスロットがポインタか」の情報
  - `.gc_map` セクション: PC → ポインタビットマップのテーブル
  - JIT コード生成時: ポインタを持つレジスタに印をつけた stack map を生成
  - GC スキャン時: RIP でテーブルをルックアップ → 精密なルート集合
  - 保守的GC (Boehm GC) との比較: 偽陽性ポインタなし = GCアキュレート
  - `(emit-gc-safepoint)` — JIT コードに safepoint を挿入するプリミティブ
  - compressed stack map: 差分エンコーディングで `.gc_map` サイズを削減
- **根拠**: HotSpot の OopMap / V8 の SafepointTable。精密GCの必須要件
- **難易度**: Very Hard

---

### Phase 233 — ランタイム型検査最適化

#### FR-1118: typep Fast Dispatch (typep高速ディスパッチ)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`
- **内容**:
  - `(typep x 'fixnum)` → NaN-boxing タグを1ビットマスクで確認 (1命令)
  - `(typep x 'cons)` / `(typep x 'null)` / `(typep x 'symbol)` — タグ直接比較
  - `(typep x 'standard-object)` — インスタンスヘッダの class-id チェック
  - `typecase` → ジャンプテーブル最適化: 型タグをインデックスに変換
  - `(typecase x (fixnum ...) (float ...) (cons ...) (t ...))` → switch文相当
  - compound types: `(typep x '(integer 0 100))` → 2回の比較
  - `(typep x '(or fixnum float))` → タグのビットOR
  - `(typep x '(satisfies evenp))` → `(evenp x)` 呼び出し（最適化なし）
- **根拠**: SBCL の型ディスパッチ最適化。`typecase` はパーサ・ディスパッチャのホットパス
- **難易度**: Medium

---

### Phase 234 — ストリーム完全実装 (Gray Streams)

#### FR-1121: Gray Streams Protocol (Grayストリームプロトコル)

- **対象**: `packages/engine/vm/src/io.lisp`
- **内容**:
  - Gray Streams: ユーザー定義ストリームの標準拡張プロトコル
  - `stream-read-char` / `stream-unread-char` / `stream-read-char-no-hang` / `stream-peek-char`
  - `stream-read-line` / `stream-read-sequence`
  - `stream-write-char` / `stream-write-string` / `stream-write-sequence`
  - `stream-fresh-line` / `stream-finish-output` / `stream-force-output` / `stream-clear-output`
  - `stream-advance-to-column` — pretty-printer 連携
  - `(defclass my-stream (fundamental-character-input-stream) ...)`
  - デフォルト実装: `stream-read-line` → `stream-read-char` の繰り返しなど
  - `:trivial-gray-streams` ライブラリとの互換性
- **根拠**: Gray Streams は SBCL/CCL/ECL/ABCL 全てが実装。カスタムストリームの標準API
- **難易度**: Medium

---

### Phase 235 — 完全な数列反復子

#### FR-1124: LOOP Arithmetic Sequence Optimization (LOOPシーケンス最適化)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`
- **内容**:
  - `(loop for i from 0 below n collect i)` → `(iota n)` への変換（最適化）
  - `(loop for i of-type fixnum from 0 below n ...)` — 型宣言付き反復変数
  - `(loop for (a . b) in alist do ...)` — cons デストラクチャリング
  - `(loop for x across vector ...)` の配列アクセス最適化: `(aref v i)` 直接生成
  - `(loop sum (* x x) for x in list)` — `sum` 蓄積の型推論 (fixnum sum → fixnum)
  - `(loop for key being each hash-key of ht using (hash-value val))` — フル構文
  - `(loop with result = nil do ... finally (return result))` — finally/return統合
  - ネストループの最適化: 内側の i/j 反復変数をレジスタに保持
- **根拠**: ANSI CL §6.1.2 LOOP の細かい最適化。数値集約ループの性能が重要
- **難易度**: Medium
