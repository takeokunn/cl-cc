# Runtime: Standard Library — Core Runtime (Phase 111-137, 75 FRs)

Lambda lists, dynamic variables, numeric I/O, regex, source location, serialization, debugger/SLIME, memory-mapped I/O, floating point, external formats, terminal/readline, dynamic library loading, GC finalization, CLOS dispatch cache, closure optimization, crypto/compression, AOT compilation, persistent data structures, type specifier runtime, function objects, environment API, async I/O, STM, CSP, pattern matching.

---

### Phase 111 — λリスト・関数プロトコル最適化

#### FR-657: Efficient &key Argument Parsing (&key引数の高速パース)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: `&key`引数を毎回`dolist`でリスト走査。キー数に比例するO(n)コスト
- **内容**:
  - `&key`パラメータをコンパイル時にパーフェクトハッシュに変換
  - 実行時: `gethash`1回でキー→スロットのマッピング
  - 未知キーの`:allow-other-keys`チェックをガードに変換
  - SBCLの`sb-c::keyword-dispatch`相当の実装
- **根拠**: LuaJIT hash dispatch / SBCL keyword arg fast path。関数呼び出しのホットパス
- **難易度**: Hard

#### FR-658: &rest List Allocation Optimization (&restリスト割り当て最適化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: `&rest args`が毎回`(list ...)`でコンスセルリストを構築。GCプレッシャー大
- **内容**:
  - `&rest`引数を`apply`に直接渡す場合（最終引数として使うだけ）: リスト構築をスキップ
  - `dynamic-extent`宣言がある場合: スタック上の配列として格納
  - `(declare (dynamic-extent args))` + `(apply fn required args)` パターンの特化
- **根拠**: SBCL `&rest` stack allocation / GHC `seq` spine strictness
- **難易度**: Hard

#### FR-659: Optional Argument Default Evaluation (オプション引数デフォルト評価)

- **対象**: `packages/engine/compile/src/codegen.lisp`
- **現状**: `&optional`/`&key`のデフォルト形式が毎回評価される（`(make-array 10)`等の高コスト式）
- **内容**:
  - デフォルト形式が定数の場合: コンパイル時に評価して`vm-const`に変換
  - デフォルト形式が副作用なしの場合: 条件付きで評価（供給時はスキップ）
  - `-p`サプライドフラグを`vm-const nil`に変換（供給なし確定の場合）
- **根拠**: SBCL optional arg constant folding
- **難易度**: Medium

#### FR-660: Dynamic-Extent Optimization (動的エクステント最適化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(declare (dynamic-extent fn))` — クロージャのスタック割り当て（エスケープ不可と保証）
  - `(declare (dynamic-extent list))` — `(list ...)` のスタック割り当て
  - エスケープ解析（FR-007）と連携: 宣言がなくてもエスケープ不可と証明できれば適用
- **根拠**: ANSI CL `dynamic-extent` / SBCL stack allocation。高頻度一時オブジェクトのGC回避
- **難易度**: Hard

---

### Phase 112 — 動的変数バインディング最適化

#### FR-663: Per-Thread Binding Stack (スレッド別バインディングスタック)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: スペシャル変数のバインディングが`vm-global-vars`ハッシュテーブルに格納。スレッドセーフでない
- **内容**:
  - 各スレッドが独立したバインディングスタックを持つ: `((sym . old-val) ...)`のスタック
  - `let`/`progv`でスペシャル変数をバインドする際: スタックにpush + グローバルを新値に更新
  - アンワインド時: スタックからpopして旧値を復元
  - スレッドローカルストレージ（TLS）としてバインディングスタックのポインタを格納
- **根拠**: SBCL per-thread binding stack / HotSpot ThreadLocalStorage。マルチスレッドの正確性
- **難易度**: Hard

#### FR-664: Fluid-Let / LetDynamic Fast Path

- **対象**: `packages/engine/vm/src/vm-execute.lisp`
- **依存**: FR-663
- **内容**:
  - バインディングスタックのpush/popを`vm-bind-special`/`vm-unbind-special`命令に特化
  - `unwind-protect`の cleanup フォーム内でアンワインドを保証
  - ネストした`let`が同一変数を繰り返しバインドする場合: スタック深さが1の特化パス
- **根拠**: SBCL `let`/`progv` dynamic binding。`*print-pretty*`等の標準変数のバインドコスト削減
- **難易度**: Medium

#### FR-665: Global Variable Type Specialization (グローバル変数型特化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**:
  - `(defvar *count* 0)` の`*count*`が常にfixnumと判明している場合、アクセスを特化
  - `(declaim (type fixnum *count*))` → グローバル変数アクセス命令を型特化版に変換
  - 型チェックをロード時にのみ実行（ストア時ガード）
- **根拠**: SBCL `(declaim (type t *x*))` global type declarations
- **難易度**: Medium

---

### Phase 113 — 数値I/O・解析

#### FR-668: parse-integer / parse-float (数値文字列パーサ)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/engine/vm/src/primitives.lisp`
- **現状**: `parse-float`はホストCL委譲のスタブ。`parse-integer`なし
- **内容**:
  - `parse-integer string &key start end radix junk-allowed` → (integer, position)
    - `:radix`で基数指定（2〜36）、`:junk-allowed t`で不完全文字列を許容
  - `parse-float string &key start end` → (float, position)
    - IEEE 754準拠の浮動小数点パース（`strtod`相当を純CLで実装）
  - 両者とも2値返却（`values`）: 解析結果＋消費した位置
- **根拠**: ANSI CL 13.2 / SBCL `parse-integer`。`read-from-string`の内部実装
- **難易度**: Medium

#### FR-669: Float Decoding Functions (浮動小数点分解関数)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **現状**: 浮動小数点の内部構造へのアクセス関数なし
- **内容**:
  - `decode-float float` → (significand, exponent, sign) — 仮数・指数・符号に分解
  - `scale-float float integer` → float × 2^integer — 高速指数スケーリング
  - `float-sign float1 &optional float2` — 符号の取り出し・付与
  - `float-digits float` — 仮数の精度（bit数）
  - `integer-decode-float float` → (mantissa, exponent, sign) — 整数形式
  - `float-radix float` → 基数（常に2）
- **根拠**: ANSI CL 12.1.3。数値アルゴリズム・シリアライゼーションの基盤
- **難易度**: Easy

#### FR-670: Integer Bit Operations (整数ビット操作)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **現状**: `logand`/`logior`/`logxor`/`lognot`は実装済み。拡張操作なし
- **内容**:
  - `logeqv`/`lognand`/`lognor`/`logandc1`/`logandc2`/`logorc1`/`logorc2` — 完全16論理演算
  - `integer-length integer` — 最上位ビット位置（`(integer-length 4)` → 3）
  - `logcount integer` — 1ビット数（Population Count）
  - `logbitp index integer` — ビット位置の確認
  - `logtest integer1 integer2` — AND結果が0でないかを確認
  - `(setf ldb)` / `(setf dpb)` — ビットフィールド書き込み
- **根拠**: ANSI CL 12.1.3 / SBCL bit operations。ビットマスク・フラグ管理
- **難易度**: Easy

---

### Phase 114 — 正規表現エンジン

#### FR-672: Regular Expression Engine Core (正規表現エンジン中核)

- **対象**: 新ファイル `packages/engine/vm/src/regex.lisp`
- **現状**: 正規表現機能なし。ホストCLの`cl-ppcre`に依存
- **内容**:
  - NFA/DFA変換コンパイラ: 正規表現文字列 → NFA → DFA → VM命令列
  - 基本パターン: `.`/`*`/`+`/`?`/`{n,m}`/`[...]`/`[^...]`/`\d`/`\w`/`\s`/`^`/`$`
  - `scan pattern string &key start end` → (match-start, match-end)
  - `all-matches pattern string` → マッチ位置リスト
  - `regex-replace pattern string replacement` / `regex-replace-all`
  - キャプチャグループ: `(scan "(foo)(bar)" "foobar")` → groups vector
- **根拠**: cl-ppcre / PCRE2 / Python `re` / Java `Pattern`。文字列処理の必須ツール
- **難易度**: Very Hard

#### FR-673: Regex JIT Compilation (正規表現JITコンパイル)

- **対象**: `packages/engine/vm/src/regex.lisp`
- **依存**: FR-672
- **内容**:
  - DFAをVM命令列ではなくx86-64機械語に直接コンパイル（FR-587トレースJITと同機構）
  - 文字クラス判定をSSE4.2の`pcmpestri`/`pcmpistrm`命令にマッピング
  - ホットパターン（同じregexが繰り返し使われる場合）を自動JIT
- **根拠**: PCRE2 JIT / Hyperscan (Intel)。大量テキストスキャンで10〜50倍高速化
- **難易度**: Very Hard

#### FR-674: Unicode-Aware Regex (Unicode対応正規表現)

- **対象**: `packages/engine/vm/src/regex.lisp`
- **依存**: FR-672, FR-590
- **内容**:
  - `\d` → Unicode `Decimal_Number`カテゴリ（ASCIIの0-9のみでなく全Unicode数字）
  - `\w` → Unicode `Letter`/`Number`/`Connector_Punctuation`
  - `\p{Category}` → Unicodeプロパティ指定（`\p{Lu}` = 大文字Letter等）
  - `(?i)` — Unicodeケース非依存マッチング（Turkishの`i`の扱いも正確に）
- **根拠**: PCRE2 Unicode support / Python `re.UNICODE`
- **難易度**: Hard

---

### Phase 115 — ソースロケーション・コンパイラ警告

#### FR-677: Source Location Objects (ソースロケーションオブジェクト)

- **対象**: `packages/frontend/parse/src/cl/parser.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: AST ノードにソース位置情報なし。エラーメッセージに行番号が出ない
- **内容**:
  - `source-location` 構造体: `(file, line, column, offset)`
  - パーサがトークン取得時にソース位置を記録
  - AST ノードに `:location` スロットを追加
  - エラー発生時に `condition` に `:source-location` を付与
  - `error at foo.lisp:42:7: unbound variable X` 形式のエラー出力
- **根拠**: SBCL source location / Rust `Span`。プロダクション品質のエラーメッセージの基盤
- **難易度**: Hard

#### FR-678: Compiler Warning Infrastructure (コンパイラ警告インフラ)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: コンパイル時警告は標準エラー出力への文字列出力のみ。condition型でない
- **内容**:
  - `compiler-note` / `compiler-warning` / `style-warning` condition型の定義
  - `redefinition-warning` — 既存関数/変数の再定義時
  - `unused-variable-warning` — `declare (ignore ...)` なしで未使用変数
  - `unreachable-code-note` — DCEで除去されたコードの通知
  - `*compiler-warning-policy*` — 警告→エラー変換ポリシー
  - `with-compilation-unit` との統合（複数ファイルの警告を集約して最後に報告）
- **根拠**: SBCL compiler notes / GCC `-Wall`。コード品質向上の基盤
- **難易度**: Medium

#### FR-679: Caret Diagnostics (キャレット診断)

- **対象**: `packages/engine/vm/src/conditions.lisp`, エラー出力系
- **依存**: FR-677, FR-678
- **内容**:
  - Rustスタイルのエラー表示:
    ```
    error: unbound variable X
     --> src/foo.lisp:42:7
      |
    42 |   (+ x undefined-var)
      |          ^^^^^^^^^^^^^ variable not found
    ```
  - ソース行の取得（ファイルキャッシュ or インラインバッファ）
  - 関連する注記（`note: variable X defined here`等）の複数行表示
- **根拠**: Rust / Clang error messages。最高水準のDX
- **難易度**: Hard

---

### Phase 116 — シリアライゼーション・永続化

#### FR-682: make-load-form Protocol

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: `ansi-cl.md` FR-550でスタブとして言及。定数CLOSインスタンスのFASL格納が不可能
- **内容**:
  - `make-load-form object &optional environment` — オブジェクトの再構築フォームを返す
  - `make-load-form-saving-slots object &key slot-names environment`
  - コンパイル時定数（`defconstant`）にCLOSインスタンスを使用可能にする
  - FASLファイル内のリテラルオブジェクトの格納・復元
- **根拠**: ANSI CL 3.2.4.4 / SBCL `make-load-form`。コンパイル時定数オブジェクトの基盤
- **難易度**: Hard

#### FR-683: Fast Binary Serialization (高速バイナリシリアライゼーション)

- **対象**: 新ファイル `packages/backend/runtime/src/serialize.lisp`
- **内容**:
  - `serialize object stream` — Lispオブジェクトをバイナリ形式でストリームに書き込み
  - `deserialize stream` — バイナリ形式から復元
  - サポート型: fixnum/float/string/symbol/list/vector/hash-table/CLOS-instance
  - 循環参照の保存・復元（`*print-circle*`相当のシリアライズ）
  - FASLキャッシュ（FR-564）の内部フォーマットとして活用
- **根拠**: cl-store / Java Serialization / Python pickle。イメージスナップショット（FR-563）の基盤
- **難易度**: Hard

#### FR-684: JSON Reader/Writer (JSON読み書き)

- **対象**: 新ファイル `packages/engine/vm/src/json.lisp`
- **内容**:
  - `json:parse string` → Lispオブジェクト（object→hash-table, array→vector, string→string, number→number, bool→t/nil）
  - `json:stringify object` → JSON文字列
  - `json:parse-stream stream` / `json:stringify-stream object stream`
  - インクリメンタルパーサ（大容量JSONをストリームから逐次処理）
  - `*json-null*` — `null`の変換先（デフォルト`:null`キーワード）
- **根拠**: JSOWN / cl-json / Shasht。現代のWebサービス・API統合に必須
- **難易度**: Medium

---

### Phase 117 — インタラクティブデバッガ・SLIME統合

#### FR-687: Swank/Slynk Protocol (SLIME/SLY統合プロトコル)

- **対象**: 新ファイル `src/debug/swank.lisp`
- **現状**: デバッガは標準エラー出力へのテキスト出力のみ。エディタ統合なし
- **内容**:
  - Swankプロトコル（S式ベースのRPCプロトコル）のサーバ実装
  - ソケットサーバ起動: `(swank:create-server :port 4005)`
  - 基本操作: `compile-and-load-file`, `compile-string`, `interactive-eval`
  - 自動補完: `completions prefix package`
  - ドキュメント: `describe-symbol`、引数リスト取得: `operator-arglist`
- **根拠**: SLIME swank / SLY slynk。Emacs/VS Code統合の標準プロトコル
- **難易度**: Very Hard

#### FR-688: Object Inspector (オブジェクトインスペクタ)

- **対象**: 新ファイル `src/debug/inspector.lisp`
- **内容**:
  - `inspect object` — オブジェクトの内部構造を対話的に表示
  - CLOS インスタンス: スロット名→値の一覧
  - コンスセル: car/cdr 再帰表示
  - クロージャ: キャプチャ変数・エントリポイント表示
  - ハッシュテーブル: キー/値ペアの表示
  - `*inspected-objects*` — インスペクト履歴
- **根拠**: SBCL inspector / SLIME inspector。ランタイムデバッグの中核ツール
- **難易度**: Medium

#### FR-689: Step Debugger (ステップデバッガ)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `src/debug/`
- **内容**:
  - `step form` — フォームをステップ実行モードで評価
  - `*step-mode*` — `:into`（関数内に入る）/ `:over`（関数を飛ばす）/ `:out`（関数から出る）
  - VM命令レベルのブレークポイント（FR-543と統合）
  - `*step-condition*` — ステップ発生時のcondition型
- **根拠**: SBCL step / SLDB (SLIME debugger backend)
- **難易度**: Hard

#### FR-690: Backtrace Collection (バックトレース収集)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: エラー時のバックトレース表示なし。呼び出しスタックが不透明
- **内容**:
  - `(backtrace n)` — 最大N個のスタックフレームを収集
  - フレーム情報: 関数名・引数値・ソース位置（FR-677）
  - `(print-backtrace &optional stream n)` — 人間可読形式で出力
  - `(debugger-frame n)` — n番目のフレームへのアクセス
  - `*error-output*` への自動出力（未ハンドルエラー時）
- **根拠**: SBCL `(sb-debug:backtrace)` / Python `traceback`
- **難易度**: Hard

---

### Phase 118 — メモリマップI/O・高度なOS統合

#### FR-693: Memory-Mapped Files (メモリマップトファイル)

- **対象**: `packages/backend/runtime/src/os.lisp` (FR-570)
- **内容**:
  - `(mmap-file path &key protection flags offset length)` — ファイルをアドレス空間にマップ
  - `mmap`/`munmap`/`msync` syscallの薄いラッパ
  - 用途: 大容量FASLファイルのゼロコピー読み込み、イメージスナップショット（FR-563）
  - 読み取り専用マップ（`PROT_READ`）でCOW（Copy-on-Write）ページ共有
- **根拠**: SBCL immobile space / Zig `mmap`。OS管理メモリのゼロコピーアクセス
- **難易度**: Medium

#### FR-694: Executable Memory Management (実行可能メモリ管理)

- **対象**: `packages/backend/runtime/src/runtime.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: JITコード（FR-587）やコールバック（FR-531）の実行可能メモリ確保の仕組みなし
- **内容**:
  - `(alloc-exec-memory size)` — `mmap(PROT_READ|PROT_WRITE|PROT_EXEC)`
  - `(seal-exec-memory ptr size)` — 書き込み禁止にして実行専用に変換（W^X policy）
  - `(free-exec-memory ptr size)` — `munmap`
  - macOS: Apple Silicon の `pthread_jit_write_protect_np` / `MAP_JIT`対応
- **根拠**: V8 CodeAllocator / LLVM JIT ExecutorProcessControl。Apple Silicon JITに必須
- **難易度**: Hard

#### FR-695: Shared Memory IPC (プロセス間共有メモリ)

- **対象**: `packages/backend/runtime/src/os.lisp`
- **内容**:
  - `shm-open name flags mode` / `shm-unlink name` — POSIX共有メモリオブジェクト
  - `ftruncate fd size` — 共有メモリサイズ設定
  - `mmap`（FR-693）との組み合わせで複数プロセスがメモリを共有
  - セマフォ（`sem-open`/`sem-wait`/`sem-post`）との連携
- **根拠**: Erlang `shm` / Python `multiprocessing.shared_memory`。並列コンパイル（FR-585）の基盤
- **難易度**: Hard

---

### Phase 119 — 数値タワー特殊値・定数

#### FR-700: Numeric Limit Constants (数値限界定数)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/primitives.lisp`
- **現状**: `most-positive-fixnum` 等の定数がホストCL値をそのまま使用。cl-ccのVM固有の値で再定義すべき
- **内容**:
  - Fixnum範囲: `most-positive-fixnum` = 2^62-1 (64bit NaN-boxing)、`most-negative-fixnum` = -2^62
  - Short/Single/Double float limits: `most-positive-single-float` / `most-negative-single-float` / `least-positive-single-float` / `least-negative-single-float`
  - Double: `most-positive-double-float` = 1.7976931348623157e308、`least-positive-double-float` = 5.0e-324
  - Normalized variants: `least-positive-normalized-single-float` / `least-positive-normalized-double-float`
  - Epsilon: `single-float-epsilon` / `double-float-epsilon` / `single-float-negative-epsilon` / `double-float-negative-epsilon`
  - `pi` = `3.141592653589793d0`
  - 特殊float値: `float-infinity` (正/負) / `float-nan`
- **根拠**: ANSI CL 12.1.3.3。数値計算・境界テストの必須定数
- **難易度**: Easy

#### FR-701: Rational Arithmetic Runtime (有理数算術ランタイム)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **現状**: `3/4` 等の有理数リテラルをパースした後のVM内計算が未実装。ホストCLに委譲
- **内容**:
  - Rational オブジェクト: `(numerator . denominator)` 形式でヒープ割り当て
  - 四則演算: 分母の最小公倍数・最大公約数を使って正規化
  - `(+ 1/3 1/6)` → `1/2`: gcd計算→通分→加算→約分をVM内で完結
  - `(/ 3 4)` → 整数同士の割り算が割り切れない場合に rational を返す
  - `numerator` / `denominator` — アクセサ
  - `rational` / `rationalize` — float → 有理数変換
- **根拠**: ANSI CL 12.1 Numbers。完全な数値タワーの必須要素
- **難易度**: Hard

#### FR-702: Complex Arithmetic Runtime (複素数算術ランタイム)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **現状**: `#C(1.0 2.0)` パース後のVM内計算が未実装
- **内容**:
  - Complex オブジェクト: `(real-part . imag-part)` のヒープ構造体
  - `+`/`-`/`*`/`/`: 複素数の四則演算をVM内で実装
  - `realpart` / `imagpart` / `conjugate` / `phase` / `abs` (複素絶対値) / `signum`
  - `(complex 1 2)` — 構築。純虚数ゼロの場合に実数へ自動縮退
  - 型特化: `(complex single-float)` vs `(complex integer)`
- **根拠**: ANSI CL 12.1 Numbers / ANSI CL 12.1.5 Complex Numbers
- **難易度**: Hard

---

### Phase 120 — 浮動小数点出力アルゴリズム

#### FR-705: Ryu Float-to-String Algorithm (Ryu最短十進変換)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/engine/vm/src/format.lisp`
- **現状**: 浮動小数点の`princ`/`prin1`出力がホストCL委譲。round-trip保証なし
- **内容**:
  - Ryu アルゴリズム (Ulf Adams 2018) の実装: 最短の十進表現で出力
  - `(= (read-from-string (prin1-to-string 1.0d0)) 1.0d0)` が保証される
  - `format ~F`/`~E`/`~G` の内部実装として活用
  - 特殊値の出力: `1.0d+INF`, `-1.0d+INF`, `1.0d+NaN`
  - 処理速度: Dragon4の10〜30倍（固定幅整数演算のみ使用）
- **根拠**: Ryu (PLDI 2018) / Grisu3 / Dragon4。`(read (print x))` = `x` のCL基本保証
- **難易度**: Hard

#### FR-706: Bignum to String (Bignum文字列変換)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **内容**:
  - 分割統治法 (divide-and-conquer): `N` を `sqrt(N)` で分割して再帰変換
  - 大基数への変換: `10^k` (kはビット数の半分) を一度計算してキャッシュ
  - `*print-base*` 対応: 2〜36進数への変換
  - 逆変換: `parse-integer` の bignum 対応 (FR-668)
  - O(n²)の単純除算 vs O(n log² n)の分割統治の切り替え閾値 (≥1000桁で高速版)
- **根拠**: GMP mp_get_str / Knuth TAOCP 4.4§。大きな整数の印刷が実用速度になる
- **難易度**: Hard

#### FR-707: Print-Object Default Methods (print-objectデフォルトメソッド)

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: `print-object` の標準実装がない。`#<HASH-TABLE>` 等が出ない
- **内容**:
  - `#<HASH-TABLE :TEST EQL :COUNT 3 {addr}>` — hash-table の print-object
  - `#<ARRAY (3 4) DOUBLE-FLOAT {addr}>` — 配列の print-object
  - `#<COMPILED-FUNCTION FOO {addr}>` — コンパイル済み関数
  - `#<STREAM type {addr}>` / `#<PACKAGE "CL" {addr}>`
  - `#<CLASS POINT {addr}>` — CLOSクラスオブジェクト
  - `readably-printable-p` — `#.` なしで読み戻せるオブジェクトか判定
- **根拠**: ANSI CL 22.1.3。REPLでの全型の可読出力
- **難易度**: Medium

---

### Phase 121 — 外部フォーマット・ストリームエンコーディング

#### FR-710: Stream External Format (ストリーム外部フォーマット)

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/engine/vm/src/stream.lisp`
- **現状**: ストリームのエンコーディングはホストCL依存。`:external-format`キーワードを無視
- **内容**:
  - `open` / `make-string-input-stream` / `make-string-output-stream` の `:external-format` 対応
  - サポートフォーマット: `:utf-8` / `:latin-1` / `:utf-16` / `:utf-16-le` / `:utf-16-be` / `:ascii`
  - `stream-external-format stream` — 現在のエンコーディングを返す
  - BOMの自動検出: UTF-16ストリームの先頭 `#xFEFF` / `#xFFFE` を解析して自動判定
- **根拠**: ANSI CL 21.2 / SBCL `:external-format`。国際化テキスト処理の基盤
- **難易度**: Hard

#### FR-711: UTF-16 Codec (UTF-16コーデック)

- **対象**: `packages/engine/vm/src/unicode.lisp` (FR-592)
- **依存**: FR-592, FR-710
- **内容**:
  - UTF-16 エンコード: BMP文字は2バイト、補助文字はサロゲートペア（4バイト）
  - UTF-16 デコード: サロゲートペアの U+D800〜U+DFFF から code point 復元
  - `string-to-octets string &key external-format` — string → byte vector
  - `octets-to-string octets &key external-format` — byte vector → string
  - Shift-JIS / EUC-JP (オプション、拡張フォーマットとして)
- **根拠**: Windows API / Java String (UTF-16内部)。Windows FFI・ファイルシステム統合
- **難易度**: Hard

---

### Phase 122 — ターミナル・Readline統合

#### FR-714: Terminal Detection and Control (ターミナル検出・制御)

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/runtime/src/os.lisp`
- **現状**: 出力先がターミナルかファイルかを区別しない。パイプでもカラー出力が出る
- **内容**:
  - `(isatty stream)` — OS の `isatty(fd)` 呼び出し。パイプ/ファイルと TTY を区別
  - `(terminal-size)` — `TIOCGWINSZ` ioctl で `(width . height)` 取得
  - ANSI エスケープシーケンス: `\e[31m` (赤) / `\e[1m` (太字) / `\e[0m` (リセット)
  - `*terminal-colors-enabled*` — isatty が false の場合に自動無効化
  - コンパイラ警告のカラー出力（FR-679 キャレット診断との統合）
- **根拠**: SBCL / GCC / Cargo の自動カラー出力。DXの基本
- **難易度**: Easy

#### FR-715: Readline / Line Editor (ライン入力エディタ)

- **対象**: `packages/cli/src/main.lisp`
- **現状**: REPL 入力が `read-line` のみ。カーソル移動・履歴・補完なし
- **内容**:
  - GNU Readline / libedit へのFFI接続（FR-530）
  - 代替: 純CLでの最小ラインエディタ (Ctrl-A/E/K/U, 左右矢印, バックスペース)
  - 履歴: `~/.cl-cc-history` への永続保存、上下矢印での履歴閲覧
  - Tab補完: 現パッケージのシンボルリストから前方一致補完
- **根拠**: Python `readline` / SBCL readline integration。REPLの基本UX
- **難易度**: Hard

#### FR-716: REPL Completion and Help (REPL補完・ヘルプ)

- **対象**: `packages/cli/src/main.lisp`
- **依存**: FR-715
- **内容**:
  - `(describe sym)` — シンボルの文書・型・値・バインディングを表示
  - `(documentation sym type)` — docstringの取得
  - `(inspect obj)` — FR-688 オブジェクトインスペクタ統合
  - `,` コマンドプレフィックス: `,quit`, `,cd /tmp`, `,load foo.lisp`
  - `(help)` — 組み込みコマンド一覧
- **根拠**: SBCL REPL / Python `help()` / Racket REPL
- **難易度**: Easy

---

### Phase 123 — 動的ライブラリロード・プラグイン

#### FR-719: Dynamic Library Loading (動的ライブラリロード)

- **対象**: 新ファイル `src/ffi/dynlib.lisp`
- **現状**: FFI（FR-530）は静的リンクのみ想定。実行時の動的ライブラリ読み込み不可
- **内容**:
  - `(load-shared-library "libfoo.dylib" &key if-not-found)` — `dlopen` 呼び出し
  - `(find-foreign-symbol "printf" lib)` — `dlsym` でシンボル解決
  - `(unload-shared-library lib)` — `dlclose`
  - デフォルト検索パス: `$DYLD_LIBRARY_PATH` / `$LD_LIBRARY_PATH` / `/usr/lib`
  - macOS framework 対応: `(load-framework "CoreFoundation")`
- **根拠**: CFFI `load-foreign-library` / SBCL `sb-alien`。実行時のCライブラリ統合
- **難易度**: Medium

#### FR-720: Plugin Architecture (プラグインアーキテクチャ)

- **対象**: `packages/cli/src/main.lisp`
- **依存**: FR-719
- **内容**:
  - `~/.cl-cc/plugins/` ディレクトリからの自動プラグインロード
  - プラグインAPI: `(define-cl-cc-plugin :name "my-plugin" :version "1.0" :load-fn #'init)`
  - 組み込み拡張点: REPL コマンド追加、コンパイラパス追加、VM命令追加
  - ASDF `cl-cc-plugin` システム定義テンプレート
- **根拠**: Emacs package.el / Vim plugin system。エコシステム拡張の基盤
- **難易度**: Medium

---

### Phase 124 — 高度なGC型・フィナライゼーション

#### FR-723: Ephemerons (エフェメロン)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **依存**: FR-184（Weak Reference）
- **現状**: FR-184でweak pointerは追加予定だがエフェメロンなし
- **内容**:
  - Ephemeron = `(key . value)` のペア。keyが GC で回収されると value も回収される
  - キー生存 → 値も生存が保証される（weak hash tableのキー/値独立weakとの違い）
  - `make-ephemeron key value` / `ephemeron-key` / `ephemeron-value`
  - GCがephemeronを2フェーズで処理: 通常マーク後にephemeronキーの生存を確認
- **根拠**: SBCL ephemerons / .NET ConditionalWeakTable。CLOS slot-valueのキャッシュに最適
- **難易度**: Hard

#### FR-724: GC Notification Hooks (GC通知フック)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**:
  - `*before-gc-hooks*` — GC前に呼ばれる関数リスト（ハンドル解放等）
  - `*after-gc-hooks*` — GC後に呼ばれる関数リスト（キャッシュ更新等）
  - `(add-gc-hook :before fn)` / `(remove-gc-hook :before fn)`
  - フックはGCスレッドではなくミューテータスレッドで実行（ファイナライザと同様）
- **根拠**: SBCL `sb-ext:*after-gc-hooks*` / `tg:gc-run-time`
- **難易度**: Easy

#### FR-725: Finalization Queue (フィナライゼーションキュー)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-184
- **内容**:
  - フィナライゼーションキュー: GC回収対象オブジェクトをキューに積む
  - 専用フィナライザスレッド（または GC後フック FR-724）がキューを処理
  - `(finalize obj callback)` — objが回収されるときcallbackを呼ぶ
  - `(cancel-finalization obj)` — フィナライザの取り消し
  - ファイルハンドル・外部リソースの自動解放に使用
- **根拠**: Java Cleaner API / Python `__del__` / SBCL `sb-ext:finalize`
- **難易度**: Hard

---

### Phase 125 — CLOSディスパッチキャッシュ最適化

#### FR-728: Effective Method Cache (有効メソッドキャッシュ)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
- **現状**: `vm-generic-call`が毎回`compute-applicable-methods`相当の処理を実行
- **内容**:
  - 各ジェネリック関数に「引数型タプル → 有効メソッド」のキャッシュを添付
  - `(list (class-of arg1) (class-of arg2) ...)` をキーとしたハッシュテーブル
  - キャッシュヒット時は `compute-effective-method` をスキップ
  - クラス再定義（`defclass`再実行）時にキャッシュを無効化
- **根拠**: SBCL PCL emf cache / CLISP dispatch cache。ジェネリック関数の呼び出しコストを5〜10倍削減
- **難易度**: Hard

#### FR-729: Next-Method Chain Optimization (次メソッド連鎖最適化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
- **内容**:
  - `call-next-method` の連鎖をコンパイル時に展開（静的に型が確定している場合）
  - `:before`/`:after` メソッドがない場合: primary のみの直接呼び出しにコンパイル
  - メソッド連鎖をベクタとして格納: `next-methods[0]`, `next-methods[1]`, ... でインデックスアクセス
  - `(next-method-p)` → コンパイル時定数に変換可能な場合は最適化
- **根拠**: SBCL PCL `make-fast-method-function`
- **難易度**: Hard

#### FR-730: Generic Function Sealing (ジェネリック関数シール)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
- **内容**:
  - `(seal-generic-function #'foo)` — 新メソッドの追加を禁止し、ディスパッチを静的化
  - シール後: FR-728のキャッシュが永続（無効化不要）、FR-501のPICが永続化
  - コンパイル時にシール済みGFへの呼び出しを直接呼び出しに変換可能
  - `:seal` オプション: `(defgeneric foo (x) (:seal t))`
- **根拠**: Julia method sealing / Swift `@_optimize(speed, size)` protocol witnesses
- **難易度**: Medium

---

### Phase 126 — クロージャ表現最適化

#### FR-733: Mutable Capture Cell (可変キャプチャセル)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: 可変変数のキャプチャが毎回 `(register . value)` alist 全体を保存。変更時に非効率
- **内容**:
  - 可変キャプチャ変数を `box` オブジェクト（1要素のヒープセル）としてラップ
  - クロージャは `box` へのポインタを保持。変更は `(setf (box-value cell) new-val)`
  - 複数クロージャが同じ `box` を共有することで `(let ((x 0)) (lambda () (incf x)))` が正確に動作
  - 不変キャプチャ（`setq`で書き換えない変数）はboxなしで直接値を保持
- **根拠**: GHC thunk cells / Closure conversion (Appel, 1988)
- **難易度**: Medium

#### FR-734: Cell Lifting / Unboxed Capture (セルリフティング・Unboxedキャプチャ)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **依存**: FR-733
- **内容**:
  - 単一のクロージャからしかアクセスされない可変変数はboxを不要にできる
  - エスケープ解析（FR-007）がクロージャが脱出しないと判明した場合: 値を直接embed
  - `(let ((x 0)) (lambda () (incf x) x))` で `x` が外部に漏れない場合: スタック上の変数
  - 不変キャプチャの型特化: fixnumと確定した変数はunboxed int として保持
- **根拠**: GHC worker/wrapper transformation / ML Kit region inference
- **難易度**: Hard

#### FR-735: Closure Cloning (クロージャクローニング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - クロージャに定数環境が渡される呼び出しパターンを検出
  - `(mapcar (lambda (x) (+ x k)) list)` で `k` が定数の場合: `k` を埋め込んだ特化クロージャを生成
  - クローニングしきい値: 環境サイズ≤4かつ呼び出し回数≥100
  - FR-583（部分評価）の軽量版として実装
- **根拠**: GHC specialisation / OCaml partial application optimization
- **難易度**: Hard

---

### Phase 127 — 暗号・圧縮原語

#### FR-738: SHA-256 / SHA-512 Implementation

- **対象**: 新ファイル `packages/backend/runtime/src/crypto.lisp`
- **内容**:
  - SHA-256: FIPS 180-4準拠の純CL実装（fixnum演算のみ使用）
  - SHA-512: 64-bit ワードで実装（bignum利用）
  - `(sha256 octets)` → 32バイトのベクタ
  - `(sha512 octets)` → 64バイトのベクタ
  - HMAC-SHA256: `(hmac-sha256 key message)` → 32バイトMAC
  - 用途: FASL整合性チェック（FR-564）、イメージ署名（FR-563）
- **根拠**: FIPS 180-4 / RFC 2104。自己完結ランタイムの安全なハッシュ基盤
- **難易度**: Medium

#### FR-739: Base64 Codec (Base64コーデック)

- **対象**: `packages/backend/runtime/src/crypto.lisp`
- **内容**:
  - `(base64-encode octets)` → 文字列
  - `(base64-decode string)` → byte vector
  - URL-safe variant: `-` と `_` を使用 (RFC 4648 §5)
  - Streaming API: `make-base64-input-stream` / `make-base64-output-stream`
- **根拠**: RFC 4648。HTTPヘッダ・JSON・FASL転送の基本エンコーディング
- **難易度**: Easy

#### FR-740: zlib / Deflate (zlib圧縮)

- **対象**: `packages/backend/runtime/src/compress.lisp`
- **内容**:
  - DEFLATE アルゴリズム (RFC 1951) の実装: LZ77 + Huffman 符号化
  - `(zlib-compress octets &key level)` → 圧縮バイト列 (RFC 1950 zlib wrapper)
  - `(zlib-decompress octets)` → 元バイト列
  - `(gzip-compress octets)` / `(gzip-decompress octets)` — gzip wrapper (RFC 1952)
  - FASL ファイルの圧縮保存（FR-564 増分コンパイルキャッシュとの統合）
- **根拠**: zlib 1.3 / Python `zlib` module。FASLの圧縮で10〜50%サイズ削減
- **難易度**: Hard

---

### Phase 128 — AOT コンパイル・起動最適化

#### FR-743: Whole-Program AOT Compilation (全プログラムAOTコンパイル)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: `./cl-cc compile foo.lisp` はVM命令列を生成するが最終的なネイティブ出力は未完成
- **内容**:
  - 全ソースファイルを一度に読み込み、クロスモジュール最適化を適用
  - VM命令列 → x86-64/AArch64 機械語の完全変換（インタープリタを使わない）
  - デッドコード除去: `main` から到達不可能な全関数を除去
  - 標準ライブラリの静的リンク（FR-571の完全版）
  - 出力: 完全スタンドアロン ELF/Mach-O バイナリ
- **根拠**: Go `go build` / Zig AOT / Kotlin Native。インタープリタ不要の本物のネイティブバイナリ
- **難易度**: Very Hard

#### FR-744: Startup Time Optimization (起動時間最適化)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: `./cl-cc run foo.lisp` 起動時に全標準ライブラリをコンパイル・ロード
- **内容**:
  - 標準ライブラリを事前コンパイルした FASL イメージとして data セグメントに埋め込む
  - `vm-state` の初期化をコンパイル時に実行し、結果を静的データとして格納
  - `*cold-init-functions*` リスト: 最低限の初期化のみ実行 (Lisp image boot sequence)
  - 目標: 50ms以内のHello World実行（現状 500ms〜2s）
- **根拠**: SBCL `save-lisp-and-die` / GHC `-prof` startup / Go init function chain
- **難易度**: Very Hard

#### FR-745: Dead Code Elimination at Link Time (リンク時デッドコード除去)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/backend/binary/src/macho.lisp`
- **依存**: FR-743
- **内容**:
  - エントリポイントからの到達可能性解析（関数コールグラフのDFS）
  - 未到達関数・変数を最終バイナリから除外
  - `--dead-code-report` フラグで除去したシンボルリストを出力
  - 標準ライブラリの多くが除去される: "Hello World" バイナリが 1MB 以下に
- **根拠**: ld の `--gc-sections` / Zig `--strip` / Kotlin/Native TreeShaking
- **難易度**: Hard

---

### Phase 129 — 永続データ構造

#### FR-748: Hash Array Mapped Trie (HAMT - 永続ハッシュマップ)

- **対象**: 新ファイル `packages/engine/vm/src/persistent.lisp`
- **内容**:
  - 永続（不変）ハッシュマップ: `assoc`/`dissoc`/`get` が O(log₃₂ n)
  - 構造共有: 変更時に変更パス上のノードだけをコピー（コピーオンライト）
  - `(persistent-map :key1 val1 :key2 val2)` — 構築
  - `(assoc map :new-key new-val)` — 新マップを返す（元は変更なし）
  - Clojure `PersistentHashMap` / Scala `HashMap` と互換アルゴリズム
- **根拠**: Bagwell 2001 HAMT / Clojure persistent collections。並行処理での安全な共有状態
- **難易度**: Hard

#### FR-749: Persistent Vector (永続ベクタ)

- **対象**: `packages/engine/vm/src/persistent.lisp`
- **内容**:
  - RRB-Tree (Relaxed Radix Balanced Tree): O(log₃₂ n) のランダムアクセス・更新・挿入
  - `(pvec 1 2 3)` — 構築
  - `(pvec-get v i)` — インデックスアクセス
  - `(pvec-assoc v i val)` — 新ベクタを返す（元は不変）
  - `(pvec-conj v val)` — 末尾追加 O(log n) amortized
  - Clojure PersistentVector との互換性
- **根拠**: Stucki et al. 2015 RRB-Trees。不変シーケンスの最適データ構造
- **難易度**: Hard

#### FR-750: Lazy Sequences (遅延シーケンス)

- **対象**: `packages/engine/vm/src/persistent.lisp`
- **内容**:
  - `(lazy-seq thunk)` — thunkが遅延評価されるシーケンス
  - `(force lazy-seq)` — 最初の要素を評価して通常のconspairを返す
  - `(take n lazy-seq)` — 最初のn要素を強制評価
  - `(lazy-map fn lazy-seq)` / `(lazy-filter pred lazy-seq)` — 遅延変換
  - 無限シーケンス: `(iterate f init)` → `(init, f(init), f(f(init)), ...)`
  - FR-179 (sequence fusion) の代替: 中間シーケンス自体を作らない
- **根拠**: Clojure lazy-seq / Haskell lazy lists / Python generators
- **難易度**: Medium

---

### Phase 130 — 型指定子ランタイム・型変換

#### FR-753: Type Specifier Runtime (型指定子ランタイム)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/foundation/type/src/inference.lisp`
- **現状**: `typep` は部分実装。`(satisfies pred)` / `(member ...)` / `(not t)` 等が未対応
- **内容**:
  - 完全型指定子構文:
    - `(integer low high)` — 範囲付き整数型
    - `(float low high)` — 範囲付き浮動小数点
    - `(array element-type dimensions)` — 特化配列型
    - `(member elem1 elem2 ...)` — 有限集合型
    - `(satisfies predicate-name)` — 述語による型
    - `(not t1)` / `(and t1 t2)` / `(or t1 t2)` — 論理型合成
    - `(values t1 t2 &rest ts)` — 多値型
  - `subtypep type1 type2` → (boolean, deterministic-p)
  - `upgrade-array-element-type type` — 最も近い実装型への強制
- **根拠**: ANSI CL 4.3 Type Specifiers。型安全なコードの実行時検証基盤
- **難易度**: Hard

#### FR-754: coerce Runtime (coerceランタイム)

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **現状**: `coerce` がホストCL委譲のスタブ
- **内容**:
  - `(coerce x 'single-float)` — integer/rational → float
  - `(coerce x 'integer)` — float → integer (truncate)
  - `(coerce x 'list)` — vector/string → list
  - `(coerce x 'vector)` — list → simple-vector
  - `(coerce x 'string)` — character/list-of-char → string
  - `(coerce x 'character)` — 1文字の文字列/整数 → character
  - `(coerce x '(complex double-float))` — 複素数型変換
- **根拠**: ANSI CL 12.1.5.2 / 4.3.2 Type Coercion。型変換の統一インターフェース
- **難易度**: Medium

---

### Phase 131 — 関数オブジェクトランタイム

#### FR-757: fdefinition / Function Cell Management

- **対象**: `packages/engine/vm/src/vm.lisp`
- **現状**: 関数定義はハッシュテーブル（`function-registry`）に格納。`fdefinition`/`fmakunbound`なし
- **内容**:
  - `fdefinition name` — 関数名から関数オブジェクトを取得 (`symbol-function` と同等)
  - `(setf fdefinition)` — 関数セルに任意の関数オブジェクトをセット
  - `fmakunbound name` — 関数定義を削除
  - `fboundp name` — 関数が定義済みかを確認
  - `(setf symbol-function)` — `fdefinition`のsetf版との統一
- **根拠**: ANSI CL 5.3。動的な関数再定義・ホットコードリロード（FR-562）の基盤
- **難易度**: Easy

#### FR-758: function-lambda-expression (関数ラムダ式)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `function-lambda-expression function` → `(lambda-expression, closure-p, name)`
  - コンパイル済み関数からソースフォームを取得（デバッグビルド時のみ格納）
  - クロージャの場合 `closure-p` = `t`
  - `--debug`フラグ時: 全関数のソースをハッシュテーブルに保存
- **根拠**: ANSI CL 5.3 / SBCL `function-lambda-expression`。`describe`・デバッガの基盤
- **難易度**: Medium

#### FR-759: Higher-Order Function Optimization (高階関数最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(funcall #'foo x)` → `(foo x)` に変換（`#'`は静的に解決可能）
  - `(mapcar #'car list)` → `#'car` をインライン展開
  - `(apply fn args)` で `fn` が定数の場合: `fn` の型に基づいて特化
  - `compose`/`complement`/`constantly` の静的展開
- **根拠**: GHC rule-based rewriting / SBCL compiler transform
- **難易度**: Medium

---

### Phase 132 — Environment API (CLTL2 / コンパイラAPI)

#### FR-762: Lexical Environment API (CLTL2環境API)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: レキシカル環境はコンパイラ内部のalistとして管理。外部APIなし
- **内容**:
  - `variable-information symbol env` → `(kind, local-p, alist)` 。kindは `:lexical`/`:special`/`:symbol-macro`/`nil`
  - `function-information name env` → `(kind, local-p, alist)` 。kindは `:function`/`:macro`/`:special-form`/`nil`
  - `declaration-information decl-name env` — `optimize`等の宣言情報
  - `augment-environment env &key variable symbol-macro function macro declare` — 環境の拡張
  - `parse-macro name lambda-list body env` — マクロ展開関数の構築
  - `enclose lambda-expression env` — 環境を閉包した関数オブジェクトを生成
- **根拠**: CLTL2 §8 / Common Lisp the Language 2nd edition。コード変換ツール・マクロの必須API
- **難易度**: Hard

#### FR-763: Compiler Intrinsic Registry (コンパイライントリンシクスレジストリ)

- **対象**: `packages/engine/compile/src/builtin-registry.lisp`
- **現状**: `builtin-registry.lisp`は呼び出し規約のみ。型情報・副作用・純粋性のDB不完全
- **内容**:
  - 各関数に対する完全なアノテーション:
    - `:result-type` — 戻り値型 (`(function (fixnum fixnum) fixnum)`)
    - `:effect-free` — 副作用なし（定数畳み込み可能）
    - `:no-throw` — 例外を投げない（handler-bind省略可）
    - `:commutative` — 引数順序不問（`(+ a b)` → `(+ b a)` が等価）
    - `:associative` — 結合法則（`(+ a (+ b c))` → `(+ (+ a b) c)`）
    - `:inlinable` — インライン展開を推奨
  - `builtin-info symbol` — アノテーション取得
  - FR-183 拡張版（属性セットの大幅拡充）
- **根拠**: LLVM `FunctionAttr` / SBCL `defknown`。コンパイラ最適化の知識データベース
- **難易度**: Medium

---

### Phase 133 — 非同期I/O・イベントループ

#### FR-766: Event Loop Infrastructure (イベントループ基盤)

- **対象**: 新ファイル `packages/backend/runtime/src/event-loop.lisp`
- **内容**:
  - `epoll`（Linux）/ `kqueue`（macOS）を使ったI/O多重化
  - `(event-loop:run)` — シングルスレッドの非同期イベントループ
  - `(event-loop:register-fd fd events callback)` — ファイルディスクリプタの監視登録
  - `(event-loop:set-timeout ms callback)` — タイムアウトコールバック
  - FR-574（ソケット）との統合: accept/read/writeを非同期化
- **根拠**: libuv / Node.js event loop / Python asyncio。非同期サーバの基盤
- **難易度**: Very Hard

#### FR-767: Async/Await Syntax (async/await構文)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`
- **依存**: FR-766, FR-552（グリーンスレッド）
- **内容**:
  - `(async (&key name) &body body)` — 非同期タスクを生成（グリーンスレッド上で実行）
  - `(await future)` — Future/Promiseの完了を待つ（現スレッドをサスペンド）
  - `(make-future)` / `(resolve-future f val)` / `(reject-future f err)`
  - `(async-let ((x (await a)) (y (await b))) body)` — 並列await
  - JavaScript `async/await` / Python `asyncio` / Rust `async/await`
- **根拠**: 現代の非同期プログラミングモデルの標準。REPLサーバ・Webサーバに必要
- **難易度**: Very Hard

#### FR-768: Promise / Future (Promise・Future型)

- **対象**: `packages/backend/runtime/src/event-loop.lisp`
- **内容**:
  - `future` 型: 未完了/完了/エラーの3状態
  - `(then future callback)` — 完了時コールバック登録
  - `(catch-error future handler)` — エラー時ハンドラ
  - `(all futures)` — 全Future完了を待つFutureを返す
  - `(race futures)` — 最初に完了したFutureを返す
  - JS Promise / Rust Future / Java CompletableFuture相当
- **根拠**: JavaScript Promise / Rust tokio::join。非同期コードの構成要素
- **難易度**: Hard

---

### Phase 134 — ソフトウェアトランザクショナルメモリ (STM)

#### FR-771: STM Core (STMコア)

- **対象**: 新ファイル `packages/backend/runtime/src/stm.lisp`
- **内容**:
  - `tvar` 型: トランザクショナル変数。`(make-tvar init)` で生成
  - `(tvar-read tv)` / `(tvar-write! tv val)` — トランザクション内での読み書き
  - `(atomically &body)` マクロ — トランザクショナルブロック。全操作をアトミックに実行
  - 実装: ログベース楽観的並行制御 — 読み書きをトランザクションログに記録し、コミット時に競合検出
  - 競合時の自動リトライ（指数バックオフ付き）
- **根拠**: Haskell STM (`Control.Concurrent.STM`) / Clojure `(dosync)` / GHC TVar
- **難易度**: Very Hard

#### FR-772: STM Retry / OrElse

- **対象**: `packages/backend/runtime/src/stm.lisp`
- **依存**: FR-771
- **内容**:
  - `(retry)` — トランザクション内で呼ぶと、読んだ tvar のどれかが変わるまでブロック
  - `(orelse tx1 tx2)` — tx1 が `retry` した場合に tx2 を試みる。両方 retry なら合成してブロック
  - ブロッキングキュー実装例: `(atomically (if (empty? q) (retry) (dequeue! q)))`
  - `(make-tvar-list)` / `(make-tvar-hash)` — コレクション型 tvar
- **根拠**: Harris et al. 2005 "Composable Memory Transactions"。STM の真価はcomposability
- **難易度**: Very Hard

---

### Phase 135 — CSPチャネル・Actorモデル

#### FR-775: CSP Channels (CSPチャネル)

- **対象**: 新ファイル `packages/backend/runtime/src/channel.lisp`
- **依存**: FR-552（グリーンスレッド）
- **内容**:
  - `(make-channel &key buffer-size)` — バッファ付き/なしチャネル
  - `(send! ch val)` — 送信（バッファ満時にブロック）
  - `(recv! ch &optional default)` — 受信（バッファ空時にブロック）
  - `(close-channel ch)` — チャネルをクローズ（受信側は nil/EOF を受け取る）
  - `(select! (ch1 v) body1 ... :default default-body)` — 複数チャネルのノンブロッキング選択
  - パイプライン構築: `(pipeline n in-ch fn out-ch)` — N並列ワーカー
- **根拠**: Go channels / Clojure core.async / Erlang `!` / CSP (Hoare 1978)
- **難易度**: Hard

#### FR-776: Actor Model (Actorモデル)

- **対象**: `packages/backend/runtime/src/channel.lisp`
- **依存**: FR-552
- **内容**:
  - `(spawn-actor fn &rest init-args)` — 独立したメッセージボックスを持つアクターを生成
  - `(send actor msg)` — アクターのメールボックスにメッセージを非同期送信
  - `(receive &body pattern-clauses)` — パターンマッチで受信メッセージを処理
  - `(self)` — 現アクターへの参照
  - `(link actor)` / `(monitor actor)` — アクター監視（Erlang互換）
  - Supervisor ツリー: `(make-supervisor :strategy :one-for-one :children [...])`
- **根拠**: Erlang OTP / Akka / Elixir。フォールトトレラントな並行システムの基盤
- **難易度**: Very Hard

---

### Phase 136 — パターンマッチング

#### FR-779: Structural Pattern Matching (構造的パターンマッチング)

- **対象**: 新ファイル `packages/frontend/expand/src/match.lisp`
- **内容**:
  - `(match expr (pattern1 body1) (pattern2 body2) ...)` マクロ
  - パターン種別:
    - リテラル: `42`, `"foo"`, `t`, `nil`
    - 変数束縛: `x` (任意値にマッチして `x` に束縛)
    - ワイルドカード: `_` (マッチするが束縛しない)
    - コンストラクタ: `(cons car-pat cdr-pat)`, `(list p1 p2 p3)`
    - ベクタ: `#(p1 p2 p3)` または `(vector p1 p2 p3)`
    - 型チェック: `(type integer n)` — integer かつ n に束縛
    - ガード: `(when pred pat)` / `(and p1 p2)` / `(or p1 p2)` — 複合パターン
    - ネスト: `(list (cons a b) (vector x y))`
  - 効率的なコンパイル: パターン決定木 (pattern decision tree) による最小分岐
- **根拠**: ML pattern matching / Rust `match` / Scala `case` / Haskell guards
- **難易度**: Hard

#### FR-780: Match Exhaustiveness Checking (網羅性チェック)

- **対象**: `packages/frontend/expand/src/match.lisp`
- **依存**: FR-779
- **内容**:
  - `(match x (t body))` — ワイルドカードなしのパターンが型全体を網羅しているかコンパイル時検査
  - 未網羅パターンの警告: `style-warning "match is not exhaustive; missing pattern (integer ...)"`
  - CLOSクラス階層との統合: 全サブクラスのパターンがあれば網羅と判定
  - 到達不可能パターンの検出と警告
- **根拠**: ML / Haskell / Rust の exhaustiveness analysis
- **難易度**: Hard

---

### Phase 137 — 代数的エフェクト・エフェクトハンドラ

#### FR-783: Effect System Runtime (エフェクトシステムランタイム)

- **対象**: 新ファイル `packages/backend/runtime/src/effects.lisp`
- **内容**:
  - `(define-effect effect-name ((op-name args...) result-type) ...)` — エフェクト定義
  - `(perform effect-name op-name args...)` — エフェクトの発動（条件システムに類似）
  - `(with-effects ((effect-name handler-clause...)) body)` — エフェクトハンドラの設置
  - ハンドラは継続 `k` を受け取り `(resume k value)` で計算を再開可能
  - CL の condition/restart との実装共有（`restart-case` をエフェクトの特殊ケースとして統一）
- **根拠**: Koka language / OCaml 5 effects / Eff language / Frank。例外・I/O・状態・非決定性の統一抽象
- **難易度**: Very Hard

#### FR-784: Effect-based Coroutines (エフェクトベースのコルーチン)

- **対象**: `packages/backend/runtime/src/effects.lisp`
- **依存**: FR-783
- **内容**:
  - `yield`/`next` をエフェクトとして定義し、ジェネレータを実装
  - `(define-generator fib () (yield 0) (yield 1) ...)` → `(next gen)` で逐次取得
  - Python `yield` / JavaScript `function*` と同等の表現力をエフェクトで実現
  - Pythonとの違い: `resume` が任意の値を `yield` 点に注入可能
- **根拠**: Plotkin & Power 2003 Algebraic Operations and Generic Effects
- **難易度**: Very Hard

---
