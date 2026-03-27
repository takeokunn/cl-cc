# Tooling, Debug & Developer Experience

Compiler infrastructure, debugging, diagnostics, developer tools, security hardening, and compilation pipeline.

---

### Phase 21 — コンパイラ・言語フロントエンド最適化

#### FR-125: Declaration Processing (type/inline/ignore/optimize)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **内容**: `(declare (type fixnum x))` → register-type-mapへの登録、`(declare (inline f))` → インライン化フォース、`(declare (ignore x))` → 未使用警告抑制
- **根拠**: expander.lispにdeclare処理ハンドラが存在しない
- **難易度**: Medium

#### FR-126: define-compiler-macro

- **対象**: `src/expand/expander.lisp`
- **内容**: `*compiler-macro-registry*` (関数名→展開関数) の追加。呼び出しパターンに応じた特化展開をユーザが定義可能
- **根拠**: expander.lispにcompiler-macroの仕組みが存在しない
- **難易度**: Medium

#### FR-127: Type Proclamations (declaim/proclaim ftype)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen-functions.lisp:17-21`
- **内容**: `(declaim (ftype (function (fixnum) fixnum) foo))` を解析し`*function-type-registry*`に登録、呼び出し時の型チェック省略に活用
- **根拠**: `*function-type-registry*`は存在するがdeclaim処理なし
- **難易度**: Medium

#### FR-128: Typecase Jump Table Dispatch

- **対象**: `src/expand/macros-basic.lisp:271-291`
- **内容**: `(typecase x (fixnum ..) (cons ..))` をネストifでなく型タグインデックスのジャンプテーブルにコンパイル
- **根拠**: 現状は単純な `if (typep x ...)` チェーンに展開
- **難易度**: Medium

#### FR-129: Compile-Time `intern` Resolution

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **内容**: シンボル名とパッケージがリテラル定数の`(intern "CAR" :cl)`をコンパイル時に解決して`vm-const`に変換
- **根拠**: 多くのソースファイルがこのパターンを多用、毎回ランタイムinternが発生
- **難易度**: Low

#### FR-130: Perfect Hash for Compiler Dispatch

- **対象**: `src/expand/expander.lisp`
- **内容**: `compiler-macroexpand-all`の25分岐condをコンパイル時生成のminimal perfect hashに変換
- **根拠**: 全フォームがこの関数を通過する最内ループ
- **難易度**: Medium

#### FR-131: Non-Closure Promotion

- **対象**: `src/compile/codegen.lisp`
- **内容**: `vm-captured-vars = nil`のlambda定義時点でクロージャオブジェクト割り当てなしの静的関数参照に昇格
- **根拠**: 現状インライナーがcall siteでのみこれを検出するが、定義時点での除去がない
- **難易度**: Low

#### FR-132: Closure Environment Trimming

- **対象**: `src/compile/closure.lisp`, `src/compile/codegen.lisp`
- **内容**: クロージャボディ内で一切読まれないキャプチャ変数を`vm-captured-vars`から除去
- **根拠**: `find-free-variables`が存在するが`vm-captured-vars`フィルタリングに未活用
- **難易度**: Medium

#### FR-133: Tail Call Through `apply`

- **対象**: `src/compile/codegen.lisp` (ast-apply)
- **内容**: 末尾位置の`(apply f args)`に`vm-tail-call`を発行（現状は`vm-apply` + returnになる）
- **根拠**: `ctx-tail-position`が利用可能だが`ast-apply`コンパイルで未使用
- **難易度**: Medium

#### FR-134: Named-let Support

- **対象**: `src/expand/macros-basic.lisp`
- **内容**: named-let構文を`labels`内の再帰関数に展開するマクロを追加、末尾再帰ループのイディオムをサポート
- **根拠**: 現状named-let構文が存在しない
- **難易度**: Medium

#### FR-135: Loop Macro Quality Fixes

- **対象**: `src/expand/loop-emitters.lisp:173-247`
- **内容**: 特定されたサブオプティマリティを修正:
  1. `:across` ベクタループの不要nil初期化除去 (line 173)
  2. `:hash-keys`/`:hash-values`のCAR重複抽出除去 (line 223)
  3. `:repeat`のデクリメント前チェックに変更 (line 247)
- **難易度**: Easy

---

### Phase 24 — 孤立インフラ統合 (Orphaned Infrastructure Activation)

#### FR-146: E-graph Equality Saturation → optimizer pipeline統合

- **対象**: `src/optimize/egraph.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `egraph.lisp` に完全なe-graph実装（union-find, pattern matching, cost extraction）が存在するが `optimize-instructions` から一切呼ばれていない
- **内容**: `opt-pass-egraph` パスを追加し、等価飽和ルール（代数的恒等式・分配法則・融合則）を宣言的に記述して既存パイプラインに統合
- **根拠**: `optimizer.lisp` に `egraph` への参照がゼロ。`egraph-match-pattern`・`egraph-apply-rule` が未使用のまま
- **難易度**: Hard

#### FR-147: SSA/CFG → optimizer pipeline統合

- **対象**: `src/optimize/ssa.lisp`, `src/optimize/cfg.lisp`, `src/optimize/optimizer.lisp`
- **現状**: Cytron SSA構築・支配木・支配フロンティアがすべて実装済みだが `optimize-instructions` から未呼び出し。`loop-depth` フィールドも未使用
- **内容**: `optimize-instructions` の前段でCFG構築・SSA変換を行い、φノードを活用したデータフロー解析（到達定義・ライブ変数）でCSE/DCEの精度を向上。最後にSSA破壊して既存IR形式に戻す
- **根拠**: `ssa.lisp:1`, `cfg.lisp:1` — 完全実装だが孤立。`loop-depth` 未使用（`cfg.lisp:32`）
- **難易度**: Very Hard

---

### Phase 26 — モジュール・コンパイル速度最適化

#### FR-151: インクリメンタルコンパイル / ユーザーコードFASLキャッシュ

- **対象**: `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: `*stdlib-compiled*`（`pipeline.lisp:190`）で標準ライブラリのみキャッシュ。ユーザーコードは毎回フルリコンパイル
- **内容**: ソースファイルのSHA-256ハッシュ+依存関係グラフに基づくFASLキャッシュ。変更なしファイルはキャッシュから復元、変更ファイルと依存ファイルのみ再コンパイル
- **根拠**: `pipeline.lisp:258-348` — `*stdlib-compiled*`以外のキャッシュ機構ゼロ
- **難易度**: Hard

#### FR-152: 推移的関数純粋性推論

- **対象**: `src/optimize/effects.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `effects.lisp` の効果分類は**命令レベル**のみ。ユーザー定義関数の純粋性は追跡されない
- **内容**: コールグラフを走査して「純粋命令のみからなる関数」→「その関数を呼ぶ関数も純粋」と推移的に伝播。純粋関数呼び出しをDCE/CSEの対象に含める
- **根拠**: `effects.lisp:1-150` — 効果テーブルは命令のみ。関数呼び出し`vm-call`は`:unknown`扱い
- **難易度**: Medium

#### FR-153: マクロ展開メモ化

- **対象**: `src/expand/expander.lisp`
- **現状**: `make-macro-expander`（`expander.lisp:96`）は毎回クロージャを生成。展開結果はキャッシュなし
- **内容**: `(macro-name . s-expr)` をキーとするハッシュテーブルで展開結果をメモ化。副作用のないマクロ（`defun`/`let`/`cond`等の組み込みマクロ）は展開結果を再利用
- **根拠**: `expander.lisp:134` — `expand-progn-with-eager-defmacro` が毎フォームを完全再展開
- **難易度**: Easy

---

### Phase 36 — バイナリ・リンク・FFI

#### FR-194: FFI (Foreign Function Interface)

- **対象**: `src/compile/codegen.lisp`, `src/emit/calling-convention.lisp`
- **現状**: FFIサポートゼロ。dlopen/dlsym/cffi/callback等一切なし
- **内容**: `(cl-cc:foreign-funcall "printf" :string "hello" :int)` 形式のFFI。Cの型定義（`:int`→i32, `:pointer`→i64等）、コールバック（Lisp関数→C関数ポインタ）、外部ライブラリロード
- **根拠**: SBCL/CCL/ECLすべてがFFI提供。システムライブラリ（libc、POSIX）呼び出しに必須
- **難易度**: Very Hard

#### FR-195: DWARF Debug Information Generation

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`
- **現状**: バイナリ出力にデバッグ情報セクションなし。ソース位置→機械語アドレスのマッピング不可
- **内容**: `.debug_line`（行番号テーブル）、`.debug_info`（変数・関数メタデータ）をMach-O/ELFに出力。`lldb`/`gdb`でソースレベルデバッグ可能に
- **根拠**: AST nodeに`source-file`/`source-line`/`source-column`が既に保持されている（`ast.lisp:14-16`）が未活用
- **難易度**: Hard

#### FR-196: Dynamic Linking / Shared Library Support

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`
- **現状**: Mach-Oは`+mh-dyldlink+`フラグ（`macho.lisp:46`）を定義するがLC_LOAD_DYLIB等のロードコマンド未実装。ELFは`.o`のみで`.so`未対応
- **内容**: Mach-OにLC_LOAD_DYLIB（dylib参照）、遅延バインドスタブを追加。ELFにPLT/GOTセクション、`.dynamic`セクションを追加。共有ライブラリ（`.dylib`/`.so`）生成
- **根拠**: FFI（FR-194）のランタイム解決に必須。libc等の外部依存解決
- **難易度**: Very Hard

#### FR-197: PIC Code Generation (RIP-relative, GOT/PLT)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: PIEフラグ設定済み（`macho.lisp:428`）だが実際のコード生成は絶対アドレス（`macho.lisp:326`の固定ロードアドレス`#x100000000`）
- **内容**: x86-64でRIP-relative addressing生成。グローバルデータアクセスをGOT経由に変更。関数呼び出しをPLT経由に変更
- **根拠**: ASLRとPIEの正しい動作に必須。現状はPIEフラグのみで実質PIC非対応
- **難易度**: Hard

#### FR-198: Mach-O Symbol Table Serialization

- **対象**: `src/emit/binary/macho.lisp`
- **現状**: `add-symbol`関数（`macho.lisp:376-396`）でシンボル構造を定義するが、`build-mach-o`でシンボルテーブルをバイナリに**書き出していない**
- **内容**: nlistエントリとstring tableをバイナリに正しくシリアライズ。`nm`コマンドでシンボル一覧表示、`dsymutil`でデバッグシンボル抽出を可能に
- **根拠**: `macho.lisp:116-122` — nlist構造定義済みだがLC_SYMTABのfileoff/nof設定が不完全
- **難易度**: Medium

---

### Phase 39 — コンパイラ品質・検証

#### FR-205: Translation Validation

- **対象**: `src/optimize/optimizer.lisp`, `tests/`
- **現状**: オプティマイザの正当性はユニットテスト（`optimizer-tests.lisp:779行`）とPBT（`ast-pbt-tests.lisp`等）で検証。形式的な意味保存証明なし
- **内容**: 最適化前後のIRに対してビセクション検証器を実装。入力プログラムの観測可能な振る舞い（戻り値・副作用順序）が最適化で不変であることを自動検査
- **根拠**: CompCert/Alive2スタイルの検証。最適化バグの早期検出に不可欠
- **難易度**: Very Hard

#### FR-206: Coverage-Guided Fuzzing

- **対象**: `tests/framework/framework-fuzz.lisp`
- **現状**: 文法ベースのfuzzing（`%gen-expr`によるランダムCLプログラム生成）。カバレッジフィードバックなし
- **内容**: `(deftest-fuzz)`にカバレッジ計測（基本ブロック到達率）を追加。未到達パスへの入力生成を優先するフィードバックループ。AFL/libFuzzer方式のミューテーション
- **根拠**: 既存PBT（72,000行）は充実しているがカバレッジ盲点あり。CSmith等のコンパイラfuzzerで発見される深いバグを検出
- **難易度**: Medium

#### FR-207: Reproducible / Deterministic Builds

- **対象**: `src/compile/pipeline.lisp`, `src/emit/binary/macho.lisp`
- **現状**: REPLステートがmutable hash table（CLの反復順序非決定的）。`*pbt-random-state*`が`(make-random-state t)`で真乱数シード
- **内容**: hash-table反復をソート済みキーリストに統一。タイムスタンプ・アドレス情報をバイナリに埋め込まない。定数シードでの決定的コンパイル
- **根拠**: Reproducible builds projectの要件。同一ソースから常に同一バイナリを生成
- **難易度**: Medium

#### FR-208: Cross-Compilation Infrastructure

- **対象**: `src/emit/target.lisp`, `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: `target.lisp`に4ターゲット定義（x86-64, aarch64, riscv64, wasm32）があるがホスト≠ターゲットの分離なし。`compile-expression`に`:target`パラメータあるがビルドシステムは未対応
- **内容**: `./cl-cc compile --target=aarch64 input.lisp -o output`形式のクロスコンパイル。ホストのランタイム定数（ポインタサイズ、エンディアン）をターゲット値で上書き
- **根拠**: `target.lisp`のRISC-V定義（`*riscv64-target*`）は存在するがバックエンド未実装。クロスコンパイル基盤があれば段階的にバックエンド追加可能
- **難易度**: Hard

---

### Phase 50 — セキュリティ硬化

#### FR-237: Stack Canary / Stack Protector (スタックカナリア)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: 関数プロローグ/エピローグにスタック保護機構なし。バッファオーバーフロー検出不可
- **内容**: `-fstack-protector`相当の機能。関数プロローグでカナリア値をスタックに配置、エピローグで検証。破壊検出時にabort。配列を含む関数のみにデフォルト適用（`-fstack-protector-strong`相当）
- **根拠**: GCC/Clang -fstack-protector / MSVC /GS。セキュリティ基本要件
- **難易度**: Medium

#### FR-238: Control-Flow Integrity (CFI)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/compile/codegen.lisp`
- **現状**: 間接呼び出し（`vm-call`→ネイティブ`call rax`）に検証なし。ROP/JOP攻撃に対する保護なし
- **内容**: 間接呼び出しターゲットの正当性検証。前方エッジCFI（間接call/jump先の関数シグネチャ検証）＋後方エッジCFI（shadow stack / CET）。Intel CETのENDBR64命令生成
- **根拠**: LLVM CFI / Clang -fsanitize=cfi / Intel CET。モダンバイナリのセキュリティ標準
- **難易度**: Hard

#### FR-239: Runtime Sanitizer Instrumentation (サニタイザ計装)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/runtime/heap.lisp`
- **現状**: 実行時メモリエラー検出機構なし。ヒープオーバーフロー・use-after-free・未初期化メモリアクセスが検出されない
- **内容**: `--sanitize=address`モードでのred zone挿入（ヒープ割り当て前後にポイズンバイト）、free後のquarantine、shadow memoryによるアクセス検証。`--sanitize=undefined`で整数オーバーフロー・null参照の計装
- **根拠**: LLVM AddressSanitizer / GCC -fsanitize=address。開発時のメモリバグ早期検出
- **難易度**: Very Hard

---

### Phase 51 — コンパイラ診断・デバッグ基盤

#### FR-240: Source Location Propagation (ソース位置伝播)

- **対象**: `src/compile/codegen.lisp`, `src/parse/ast.lisp`
- **現状**: `ast.lisp:14-16` — ASTノードに`source-file`, `source-line`, `source-column`フィールドが存在するがcodegenで一切参照されない。VM命令やネイティブコードにソース位置が紐づかない
- **内容**: codegen各フェーズでAST→VM命令→MIR→機械語にソース位置情報を伝播。エラーメッセージ・スタックトレース・デバッガにファイル名:行番号を表示。FR-195（DWARF）の前提条件
- **根拠**: 全モダンコンパイラの基本機能。デバッグ体験の根幹
- **難易度**: Medium

#### FR-241: Macro Expansion Tracing (マクロ展開トレース)

- **対象**: `src/expand/expander.lisp`, `src/cli/main.lisp`
- **現状**: `compiler-macroexpand-all`がサイレントに全マクロを展開。中間段階の確認手段なし
- **内容**: `./cl-cc compile --trace-macros input.lisp`で各マクロ展開ステップ（展開前→展開後）をインデント付きで出力。`macroexpand-1`のフック機構で展開深度・展開回数を計測
- **根拠**: SBCL sb-ext:*macroexpand-hook* / Clojure macroexpand-all。マクロデバッグの基本ツール
- **難易度**: Easy

#### FR-242: Compilation Time Profiling (コンパイル時間プロファイリング)

- **対象**: `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: コンパイルパイプライン各フェーズの実行時間計測なし。ボトルネック特定不可
- **内容**: `./cl-cc compile --time-phases input.lisp`でparse→expand→CPS→codegen→optimize→regalloc→emit各フェーズの経過時間を表示。selfhost時のコンパイル時間分布を可視化
- **根拠**: GCC -ftime-report / Clang -ftime-trace / Rust -Ztime-passes。コンパイラ自体の性能改善サイクルに必須
- **難易度**: Easy

#### FR-243: Incremental Parsing Integration (インクリメンタルパース統合)

- **対象**: `src/parse/incremental.lisp`, `src/compile/pipeline.lisp`
- **現状**: `incremental.lisp:1-176` — tree-sitterスタイルのインクリメンタルパーサが完全実装済みだがパイプライン未接続。`*parse-cache*`（`incremental.lisp:132-150`）のcache-lookup/cache-storeがexportされるが未呼び出し
- **内容**: `pipeline.lisp`のコンパイルフローに`cache-lookup`を挿入し、ファイル変更部分のみ再パース。REPL/LSPモードでの差分コンパイルの基盤
- **根拠**: 既存インフラの活用。tree-sitter / Roslyn incremental parsing。IDE連携の基盤
- **難易度**: Medium

---

### Phase 61 — コンパイラパスインフラ

#### FR-276: Optimization Levels (-O0 to -O3) (最適化レベル)

- **対象**: `src/cli/main.lisp`, `src/optimize/optimizer.lisp`, `src/compile/pipeline.lisp`
- **現状**: 全最適化が無条件実行。インライン閾値=15（`optimizer.lisp:1018`）、最大反復=20（`optimizer.lisp:1042`）がハードコード。CLIに最適化レベルオプションなし
- **内容**: `-O0`（fold+DCEのみ、inline=0、iters=1）、`-O1`（fold+jump+DCE、inline=15、iters=5）、`-O2`（全パス、inline=30、iters=20）、`-O3`（全パス+E-graph飽和、inline=60、iters=40）の4段階。`(declare (optimize ...))`との連携
- **根拠**: GCC/Clang/SBCL全てが`-O`フラグを提供。開発時の高速コンパイルと本番の積極的最適化の切り替えに必須
- **難易度**: Medium

#### FR-277: Pass Dependency Resolution (パス依存関係解決)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `*opt-convergence-passes*`（`optimizer.lisp:1020`）に8パスが固定順序で列挙。パス間の依存メタデータなし。前提解析の自動注入なし
- **内容**: `defpass`マクロで各パスが`:requires`（必要な解析）`:invalidates`（無効化する解析）`:preserves`（保持する解析）を宣言。トポロジカルソートでパス順序を自動決定。CFG/SSA/支配木を必要に応じて再計算
- **根拠**: LLVM PassManager / GCC pass manager。FR-147（SSA統合）・FR-146（E-graph統合）の前提インフラ
- **難易度**: Hard

#### FR-278: IR Verification Suite (IR検証スイート)

- **対象**: `src/compile/ir/block.lisp`, 新規`src/compile/ir/verify.lisp`
- **現状**: `ir-verify-ssa`（`block.lisp:129-148`）がSSA単一定義性のみ検証。CFG整合性・use-def連鎖・Phi正当性・支配関係の検証なし
- **内容**: 各パス間に挿入可能な包括的IR検証: (1) CFGの前任/後任の双方向一致、(2) 支配木の整合性、(3) use-def連鎖の有効性、(4) Phi引数数=先行ブロック数、(5) 活性情報の正確性。`-O0`では各パス後に実行、`-O2`以上ではdebugビルドのみ
- **根拠**: LLVM `-verify-each` / GCC `--enable-checking`。最適化パスのバグを早期検出
- **難易度**: Medium

#### FR-279: IR Dump CLI Integration (IRダンプCLI統合)

- **対象**: `src/cli/main.lisp`, `src/compile/ir/printer.lisp`
- **現状**: `ir/printer.lisp`にIRプリンタが実装済みだが、CLIからの呼び出し手段なし。パス間IRの比較不可
- **内容**: `--dump-ir`（全パス後にIR出力）、`--dump-ir-before <pass>`・`--dump-ir-after <pass>`（特定パス前後のIR出力）。ファイル出力モード（`.ir`拡張子）。diff可能なテキスト形式
- **根拠**: LLVM `-print-before-all`/`-print-after-all` / GCC `-fdump-tree-all`。最適化デバッグの標準手法
- **難易度**: Easy

#### FR-280: Per-Pass Statistics Collection (パス別統計収集)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: E-graphのみ`egraph-stats`（`egraph.lisp:420-424`）で`:classes`/`:memo-size`/`:worklist`を返す。メインオプティマイザに統計収集なし
- **内容**: `(defstruct pass-stats name instructions-removed instructions-added instructions-changed time-ms)`。各パスの実行前後で命令数を比較。収束ループ全体の統計サマリ。`--opt-stats`フラグで出力
- **根拠**: GCC `-fopt-info` / LLVM `-stats`。どのパスが効果的かの定量評価に必須
- **難易度**: Easy

#### FR-281: Optimization Bisection Support (最適化二分探索)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: E-graphに`fuel`パラメータ（`egraph.lisp:299-317`）があるが、メインオプティマイザにfuelなし。最適化バグの原因パス特定手段なし
- **内容**: `--opt-bisect-limit N`フラグ。N番目の最適化変換まで適用し、以降はスキップ。二分探索でバグを引き起こすパス・変換を特定。環境変数`CL_CC_OPT_BISECT`でも設定可能
- **根拠**: LLVM `-opt-bisect-limit` / GCC `--param=max-iterations`。最適化リグレッションの効率的デバッグ
- **難易度**: Medium

---

### Phase 67 — デバッグ・診断

#### FR-310: Interactive VM Debugger (対話型VMデバッガ)

- **対象**: `src/vm/vm-run.lisp`, `src/vm/vm.lisp`, `src/cli/main.lisp`
- **現状**: `run-compiled`（`vm-run.lisp:124`）と`run-vm`（`vm-run.lisp:265`）はタイトループでフック挿入点なし。ステップ実行・ブレークポイント・インスペクト不可
- **内容**: `vm-step`（命令レベル単一ステップ）、`vm-break`（ブレークポイント命令）、`vm-inspect`（レジスタ/ヒープインスペクタ）。エラー時にインタラクティブデバッグREPLにドロップ。条件付きブレークポイント
- **根拠**: GDB / LLDB / SBCL `(break)`。デバッグなしでの開発は非現実的
- **難易度**: Hard

#### FR-311: Native Code Disassembler (ネイティブコード逆アセンブラ)

- **対象**: 新規`src/emit/disasm/x86-64-disasm.lisp`, `src/emit/disasm/aarch64-disasm.lisp`
- **現状**: バイトコードISA逆アセンブラ（`src/bytecode/decode.lisp:202`、`disassemble-instruction`）は存在。x86-64/ARM64のネイティブ命令デコーダなし
- **内容**: x86-64命令デコーダ（可変長、prefix+opcode+ModRM+SIB+disp+imm）。AArch64命令デコーダ（固定32bit）。Mach-O/ELFのコードセクションを逆アセンブル可能に。`disassemble`関数としてCLI統合
- **根拠**: SBCL `(disassemble #'foo)` / GCC `objdump`。コンパイラ開発に逆アセンブル機能は必須
- **難易度**: Very Hard

#### FR-312: REPL Enhancements (REPL拡張)

- **対象**: `src/cli/main.lisp`
- **現状**: REPL（`main.lisp:256-302`）はベア`read-line`ループ。括弧バランス（`%count-parens`、`main.lisp:243`）のみ。履歴・タブ補完・readline統合なし
- **内容**: コマンド履歴（上下矢印）、タブ補完（パッケージシンボル+関数レジストリ）、複数行編集、readline/linedit/pure-CLラインエディタ統合
- **根拠**: SBCL + linedit / SLIME / rlwrap。対話型開発の基本機能
- **難易度**: Medium

#### FR-313: Call Stack Pretty-Printer (コールスタック整形表示)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-run.lisp`
- **現状**: `vm-call-stack`（`vm.lisp:345`）は`(return-pc dst-reg old-closure-env saved-regs)`の生リスト。フォーマッタなし。`vm-signal-error`（`vm-run.lisp:74-98`）はバックトレースを出力しない
- **内容**: `vm-print-backtrace`：`vm-call-stack`を走査し、return-pcをラベルテーブル経由で関数名に解決。フレーム番号・関数名・引数値を人間可読形式で表示
- **根拠**: SBCL `(sb-debug:print-backtrace)` / GDB `bt`。エラー診断の基本
- **難易度**: Medium

#### FR-314: VM Watchpoints/Tracepoints (VMウォッチポイント/トレースポイント)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-run.lisp`
- **現状**: `vm-reg-set`（`vm-run.lisp:306-310`）と`vm-reg-get`（`vm-run.lisp:301-304`）にフックなし。トレース/ウォッチインフラなし
- **内容**: `vm-watch-register`（レジスタ書き込み時にブレーク/ログ）、`vm-trace-function`（名前付き関数のentry/exitをargs/return値付きでログ）、`vm-trace-instruction`（各命令実行前にログ）
- **根拠**: GDB watchpoints / DTrace / SBCL `(trace)`。実行時動作の観測に必須
- **難易度**: Medium

#### FR-315: Object Inspector (オブジェクトインスペクタ)

- **対象**: `src/vm/vm.lisp`
- **現状**: VMクロージャ（`vm.lisp:19-31`、6スロット）・consセル・ヒープオブジェクトに人間可読インスペクションAPIなし。CLOSオブジェクトは`:__class__`付きハッシュテーブルだがプリンタなし
- **内容**: `vm-inspect`：クロージャのentry-label/params/captured-values、ヒープオブジェクト、クラスインスタンス（スロット名+値）、ジェネリック関数ディスパッチテーブルを再帰的に表示
- **根拠**: SBCL `(inspect obj)` / SLIME Inspector。オブジェクト構造の理解に必須
- **難易度**: Easy

#### FR-316: Benchmark/Profiling Framework (ベンチマーク/プロファイリングフレームワーク)

- **対象**: `tests/framework/framework-compiler.lisp`, `src/vm/vm-run.lisp`, 新規`benchmarks/`
- **現状**: `assert-faster-than`（`framework-compiler.lisp:238`）と`assert-no-consing`（`framework-compiler.lisp:257`）はテストアサーション用のみ。スタンドアロンベンチマークランナーなし。VM命令カウンタなし
- **内容**: `defbenchmark`マクロ（ウォームアップ+計測反復+統計出力）。VM命令カウンタ/プロファイラ（opcode別頻度カウント）。関数別呼び出し回数+累積時間追跡。JSON結果出力によるリグレッション追跡
- **根拠**: Criterion (Haskell/Rust) / Google Benchmark。性能回帰の定量検出
- **難易度**: Medium

#### FR-317: Structured Diagnostics (構造化診断)

- **対象**: `src/parse/diagnostics.lisp`, `src/vm/conditions.lisp`
- **現状**: `diagnostic`構造体（`diagnostics.lisp:7-14`）にseverity/span/message/hints/notesあり。しかしエラーコード・fix-itサジェスト・機械可読IDなし。VM conditions（`conditions.lisp:17-67`）はプレーンテキスト`:report`。`ast-error`（`ast.lisp:355-360`）はフォーマット文字列
- **内容**: `error-code`フィールド（e.g., `E0001`）追加。`fix-it`フィールド（修正候補テキスト+span）追加。エラーカタログドキュメント。`--json-diagnostics` CLIフラグでIDE統合
- **根拠**: Rust compiler diagnostics / Clang `-fdiagnostics-format=json`。IDE統合の前提
- **難易度**: Medium

#### FR-318: Compiler Warning System (コンパイラ警告システム)

- **対象**: `src/compile/codegen.lisp`, `src/parse/diagnostics.lisp`
- **現状**: `diagnostic`構造体は`:warning`重大度対応（`diagnostics.lisp:9`）。パーサは`make-parse-warning`（`diagnostics.lisp:85`）を使用。しかしcodegen.lispは構造化警告を一切出力しない。未使用変数・型不一致・非推奨関数の警告なし
- **内容**: `diagnostic`インフラをcodegenに接続。未使用変数/import、型不一致（型チェッカーから）、到達不能コード、非推奨関数使用の警告。`format-diagnostic-list`による一括報告
- **根拠**: SBCL style-warning / GCC `-Wall`。コード品質の自動検出
- **難易度**: Medium

---

### Phase 68 — 開発者エコシステム

#### FR-319: LSP Server (Language Server Protocol サーバー)

- **対象**: 新規`src/lsp/`モジュール, `cl-cc.asd`, `src/cli/main.lisp`
- **現状**: LSP関連コードなし。JSON-RPCなし。CLIに`lsp`サブコマンドなし（`main.lisp:480-497`）
- **内容**: JSON-RPC 2.0トランスポート。`textDocument/didOpen|didChange|completion|hover|definition|diagnostics`ハンドラ。既存インフラ活用: インクリメンタルパーサ（`incremental.lisp:1-177`）、CST（`cst.lisp`）、診断（`diagnostics.lisp`）、型推論（`inference.lisp:74-194`のAlgorithm W）
- **根拠**: LSP Specification / rust-analyzer / clangd。モダンIDE統合の標準
- **難易度**: Very Hard

#### FR-320: Code Formatter (コードフォーマッタ)

- **対象**: 新規`src/format/formatter.lisp`
- **現状**: `type-to-string`（型プリンタ）、`cst-to-sexp`（`cst.lisp:96-122`）は存在。ソースコードフォーマッタなし。CST trivia構造体（`cst.lisp:10-16`）は空白/コメントを保持するがinverse未実装
- **内容**: CST保持pretty-printer: trivia-aware indentation/alignment。CLIの`format`サブコマンド。設定可能なインデントスタイル
- **根拠**: gofmt / rustfmt / Prettier。コードスタイル統一の標準ツール
- **難易度**: Medium

#### FR-321: API Documentation Generator (APIドキュメント生成)

- **対象**: 新規`src/tools/docgen.lisp`
- **現状**: docstring抽出・ドキュメント生成コードなし。多くの関数にdocstring存在（e.g., `cst.lisp:21`, `incremental.lisp:12`）だが抽出ツールなし
- **内容**: `defun`/`defvar`/`defstruct`/`defclass`からdocstring抽出。HTML/Markdown APIリファレンス生成。既存パーサ+ASTを活用
- **根拠**: Staple / MGL-PAX (CL doc generators)。APIドキュメントの自動生成
- **難易度**: Medium

#### FR-322: Continuous Benchmarking CI (継続的ベンチマークCI)

- **対象**: `.github/workflows/benchmark.yml`, ベンチマーク結果ストレージ
- **現状**: CIはテスト実行のみ（`.github/workflows/test.yml:1-25`）。性能追跡なし
- **内容**: GitHub Actionsワークフローでpush/PR時にベンチマーク実行。結果を保存しリグレッション検出。PRに性能差分コメント。FR-316のベンチマークスイートに依存
- **根拠**: bencher.dev / GitHub Action benchmark-action。性能リグレッションの自動検出
- **難易度**: Medium

#### FR-323: Compilation Cache (コンパイルキャッシュ)

- **対象**: `src/compile/pipeline.lisp`, 新規`src/compile/cache.lisp`
- **現状**: `compile-expression`（`pipeline.lisp:4-37`）は常にゼロからリコンパイル。パースキャッシュ（`incremental.lisp:132-150`、`*parse-cache*`）はsxhashベースのインメモリのみで永続化なし
- **内容**: 永続コンパイルキャッシュ（コンテンツハッシュ→コンパイル済みVMプログラム）。依存関係対応無効化。`--incremental`フラグ
- **根拠**: ccache / sccache / SBCL FASL。リビルド時間の大幅削減
- **難易度**: Hard

#### FR-324: Static Analysis / Linting (静的解析/リンティング)

- **対象**: 新規`src/analysis/`モジュール, `src/cli/main.lisp`
- **現状**: 型チェッカー（`src/type/`）、変数解析（`ast.lisp`の`find-free-variables`等）、診断インフラ（`diagnostics.lisp`）、DCE（オプティマイザ）が個別に存在。統合リンティングツールなし
- **内容**: CLIの`lint`サブコマンド。未使用変数検出（`find-free-variables`拡張）、シャドウイングバインディング警告、到達不能コード検出（オプティマイザのDCEパス活用）、スタイル警告（`(if x y nil)` → `(when x y)`）。`format-diagnostic`経由で出力
- **根拠**: ESLint / clippy / SBCL style-warnings。コード品質の自動保証
- **難易度**: Medium

#### FR-325: ASDF System Parallelization (ASDFシステム並列化)

- **対象**: `cl-cc.asd`
- **現状**: ルートおよびモジュール内で`:serial t`（`cl-cc.asd:16`）。独立モジュール（`type/`, `vm/io.lisp`, `vm/list.lisp`, `vm/strings.lisp`等）も直列コンパイル
- **内容**: `:serial t`除去+明示的`:depends-on`追加。`type/`と`vm/`の並列コンパイル。`:around-compile`で最適化宣言。独立性の高いファイル群を識別し並列ビルド可能に
- **根拠**: ASDF best practices / poiu (parallel ASDF)。ビルド時間短縮
- **難易度**: Easy

---

### Phase 87 — コンパイルパイプライン改善

#### FR-502: our-load AST pipeline

- **対象**: `src/compile/pipeline.lisp`
- **現状**: `our-load`がソースを文字列→S式→`eval`のパスを取り、AST変換・最適化をバイパスする場合がある
- **内容**: `our-load`を完全AST pipeline経由 (parse→expand→CPS→codegen→optimize) に統一
- **根拠**: セルフホスティングの安定化。文字列roundtripによる情報消失を防ぐ
- **難易度**: Medium

#### FR-503: declaim / deftype 処理

- **対象**: `src/compile/pipeline.lisp`
- **現状**: `declaim`がprescan時に無視され、`deftype`が型エイリアスとして機能しない
- **内容**: `declaim`の主要なdeclaration (`ftype`, `type`, `inline`, `optimize`) をpipelineで処理。`deftype`をマクロとして展開可能にする
- **根拠**: ANSI CL 3.3.4 — declaim, deftype
- **難易度**: Medium

#### FR-504: prescan 多重パッケージ

- **対象**: `src/compile/pipeline.lisp`
- **現状**: `%prescan-in-package`が単一パッケージの切り替えのみ対応。同一ファイルに複数`in-package`が出現する場合に誤動作
- **内容**: prescanループをパッケージスタックで管理。`in-package`の出現ごとに正しく切り替え
- **根拠**: 複数パッケージを跨ぐソースファイルのセルフホスティング対応
- **難易度**: Medium

#### FR-505: compile-file セルフホスト読み込み

- **対象**: `src/compile/pipeline.lisp`
- **現状**: `compile-file`相当の機能でセルフホスト読み込みが`#.`/`#+`/`#-`の一部リーダーマクロで誤動作する可能性
- **内容**: リーダーマクロ処理の完全化と`compile-file`相当パスのセルフホスト対応テスト追加
- **根拠**: `./cl-cc selfhost`の安定性向上
- **難易度**: Medium

#### FR-506: エラー回復 (Error Recovery)

- **対象**: `src/compile/codegen.lisp`
- **現状**: コンパイル時エラーが例外として伝播し、後続フォームのコンパイルが停止する
- **内容**: フォームレベルのエラー回復。1フォームのコンパイル失敗を記録しつつ次フォームに継続
- **根拠**: セルフホスティング安定性。大規模ファイルのデバッグ効率
- **難易度**: Low

---

### Phase 70 — JIT・動的コンパイル

#### FR-330: Baseline JIT (ベースラインJIT)

- **対象**: `src/vm/vm-run.lisp`, 新規`src/jit/baseline.lisp`
- **現状**: VMはフラットベクタのインタープリタ（`run-vm`、`vm-run.lisp:265`）。ネイティブコード生成は`src/backend/x86-64-codegen.lisp`に存在するがVM実行とは分離されている
- **内容**: VM命令列を**ウォームアップなし**で直接x86-64機械語に変換するベースラインJIT。register allocationは簡略（1命令1スタックスロット）で可。最適化は行わず変換速度優先。VMインタープリタとのスイッチング機構（`--jit=off/baseline/opt`フラグ）
- **根拠**: V8 Liftoff / SpiderMonkey Baseline / LuaJIT。インタープリタの10〜30x高速化が初回コンパイルコストなしに得られる
- **難易度**: Hard

#### FR-331: Tiered Compilation (階層型コンパイル)

- **対象**: 新規`src/jit/tiered.lisp`, `src/jit/baseline.lisp`, `src/compile/pipeline.lisp`
- **現状**: コンパイルパスは1段階のみ（`our-eval`→`run-vm`）
- **内容**: Tier-0 (インタープリタ) → Tier-1 (ベースラインJIT、FR-330) → Tier-2 (最適化JIT、既存optimizer+regalloc+x86-64backend) の3段階。関数呼び出しカウンタ（`*invocation-count-table*`）で閾値（例: 10回→Tier-1, 1000回→Tier-2）に達した時点で昇格。コンパイル非同期実行でダウンタイムゼロ
- **根拠**: V8 Ignite→Maglev→Turbofan / HotSpot C1→C2。モダンランタイムのデファクト構成
- **難易度**: Very Hard

#### FR-332: On-Stack Replacement / OSR (スタック上置換)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-run.lisp`, `src/jit/baseline.lisp`
- **現状**: `vm-call-stack`（`vm.lisp:345`）でフレーム構造は確立。実行中フレームの置換機構なし
- **内容**: ループバックエッジのホットカウント到達時に、**実行中のフレームをそのまま最適化済みコードに切り替える**。フレームのレジスタ・ローカル変数状態を新コードのABIにマッピング（OSR entry generation）。長時間実行ループでのティアアップを可能にする
- **根拠**: HotSpot OSR / LuaJIT OSR / V8 OSR。ループ集中型ワークロードのJIT効果に必須
- **難易度**: Very Hard

#### FR-333: Deoptimization Support (非最適化・ガード失敗回復)

- **対象**: `src/jit/`, `src/vm/vm.lisp`
- **現状**: 最適化されたコードで型ガード違反が発生した場合の回復機構なし
- **内容**: **deopt point** を最適化コードに埋め込み（`vm-deopt`疑似命令）、ガード違反時に最適化前のインタープリタ状態を再現。`deopt-map`（最適化コードオフセット→仮想レジスタ状態の表）をコンパイル時に生成。speculative type specialization（FR-334）の前提
- **根拠**: V8 Deoptimizer / HotSpot Uncommon Trap / Graal Deopt。投機的最適化の安全網
- **難易度**: Very Hard

#### FR-334: Polymorphic Inline Caches / PIC (多相インラインキャッシュ)

- **対象**: `src/vm/vm-execute.lisp`, `src/jit/baseline.lisp`
- **現状**: `execute-instruction`でのジェネリック関数ディスパッチはハッシュテーブル検索（`vm-clos.lisp`）。呼び出しサイトごとのキャッシュなし
- **内容**: 呼び出しサイトごとに**最後に観測したレシーバー型→メソッド直接ポインタ**をキャッシュ（Monomorphic IC）。ミス時に最大4エントリのPICに昇格。メガモーフィック（4超）はグローバルキャッシュにフォールバック。JITコード内でのinline typecheck+direct call生成
- **根拠**: Self言語起源の技術。V8/SpiderMonkey/LuaJIT全て採用。CLOS動的ディスパッチのホットパス高速化
- **難易度**: Hard

---

### Phase 71 — リンク時・全プログラム最適化

#### FR-335: Link-Time Optimization / LTO (リンク時最適化)

- **対象**: `src/emit/binary/macho.lisp`, `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: コンパイル単位はファイル単位で独立。リンク時には機械語バイト列のみ接続（`macho.lisp`）
- **内容**: コンパイル済みVMプログラム（IR形式）をbitcode相当の中間表現として`.o`に埋め込み。リンカ起動時に全コンパイル単位のIRをマージして**全プログラム最適化**（クロスモジュールインライン化・DCE・型特化）を適用後に機械語生成。`--lto=thin`（並列インデックスベース）と`--lto=full`の2モード
- **根拠**: LLVM ThinLTO / GCC LTO / Rust LTO。モジュール境界を越えたインライン化でlibcall撤廃などの効果
- **難易度**: Very Hard

#### FR-336: Interprocedural Constant Propagation / IPCP (手続き間定数伝播)

- **対象**: `src/optimize/optimizer.lisp`, 新規`src/optimize/ipa.lisp`
- **現状**: 定数畳み込みは関数内のみ（`opt-pass-fold-constants`、`optimizer.lisp`）。引数に定数が渡される呼び出しサイトを活用した特化版生成なし
- **内容**: コールグラフを走査し、特定の呼び出しサイトで定数引数を持つ関数の**クローン特化版**を生成。クローン内で定数伝播+DCEを実施。GCCのIPA-CP / LLVM SCCP同様のパターン
- **根拠**: `./cl-cc selfhost`内のconstant-heavy呼び出しで特に効果的。ループ内の呼び出しコスト削減
- **難易度**: Hard

#### FR-337: Devirtualization (仮想ディスパッチ解消)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: CLOS`defmethod`呼び出しは常にディスパッチテーブル検索（`vm-clos.lisp`）。型情報があっても直接呼び出しに変換されない
- **内容**: 型推論（`src/type/inference.lisp`）の結果でレシーバー型が単態の場合、`vm-generic-call`を`vm-call`に変換（確実なdevirt）。型が確率的に単態の場合は**投機的devirt**（PICガード+deoptバックエッジ）
- **根拠**: GCC `-fdevirtualize-speculatively` / LLVM devirt pass。CLOSのメソッド呼び出しが最大のオーバーヘッド源の一つ
- **難易度**: Hard

#### FR-338: Whole-Program Dead Code Elimination (全プログラムDCE)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/binary/macho.lisp`
- **現状**: DCEは関数内レベルのみ（`opt-pass-dead-code`）。未呼び出し関数はバイナリに残留
- **内容**: コールグラフの到達可能性解析（エントリポイントから到達できない関数・グローバル変数を除去）。`--gc-sections`相当の機能。LTO（FR-335）との連携で最大効果
- **根拠**: GCC `--gc-sections` / LLVM GlobalDCE。stdlibの未使用部分除去でバイナリサイズ大幅削減
- **難易度**: Medium

---

### Phase 72 — 高度静的解析

#### FR-340: Alias Analysis (エイリアス解析)

- **対象**: 新規`src/optimize/alias.lisp`, `src/optimize/optimizer.lisp`
- **現状**: オプティマイザは保守的にヒープアクセスを副作用ありと仮定。エイリアス情報なし
- **内容**: **May/Must/No-Alias** の3分類。`vm-slot-read`/`vm-slot-write`の引数解析でオブジェクトIDが異なる場合はno-aliasと判定。ポインタ引数の`noalias`アノテーション。エイリアス情報をCSE・ロードストア融合・命令スケジューリングに活用
- **根拠**: LLVM AliasAnalysis / GCC points-to analysis (Andersen/Steensgaard)。メモリ最適化の基盤
- **難易度**: Hard

#### FR-341: Escape Analysis (エスケープ解析)

- **対象**: `src/compile/codegen.lisp`, `src/runtime/heap.lisp`
- **現状**: ヒープ割り当ては常にGC管理ヒープへ（`heap.lisp`の`rt-alloc`）。スタック割り当てなし
- **内容**: オブジェクトが生成関数の外に出ない（エスケープしない）ことを証明した場合、ヒープ割り当ての代わりに**スタック割り当て**に変換。GCプレッシャー削減。クロージャのキャプチャ変数にも適用（FR-132との連携）
- **根拠**: Java HotSpot EA / Go escape analysis / Rust所有権の静的版。GCポーズ削減の鍵
- **難易度**: Very Hard

#### FR-342: Points-to Analysis (ポインタ指示先解析)

- **対象**: 新規`src/optimize/points-to.lisp`
- **現状**: `vm-closure`のcaptured-vars・`vm-slot`のオブジェクトが実際にどのアロケーション元を指すか不明
- **内容**: Andersenスタイルの包含ベースポインタ解析。`*points-to-sets*` グローバルマップ。インタープロシージャルエスケープ解析（FR-341）・エイリアス解析（FR-340）・devirt（FR-337）の精度向上に活用
- **根拠**: LLVM DataStructureAnalysis / GCC IPA-PTA。複数の最適化パスの精度基盤
- **難易度**: Very Hard

#### FR-343: Effect System Integration (エフェクトシステム統合)

- **対象**: `src/type/inference.lisp`, `src/optimize/effects.lisp`
- **現状**: `effects.lisp`は命令レベルの副作用分類のみ。型システム（`inference.lisp`）と連携していない
- **内容**: 関数型に**エフェクト行** `(→ (fixnum) fixnum ! (io))` を追加（Koka / OCaml 5.0 effects スタイル）。pure関数の自動識別・エフェクト封じ込め（`with-effect-handler`）によるCSE/並列化根拠の強化。段階的採用（既存型シグネチャへの影響なし）
- **根拠**: OCaml 5.0 algebraic effects / Koka / Haskell IO monad。副作用の型レベル追跡はモダン型システムの主流
- **難易度**: Very Hard

---

### Phase 73 — SIMD・ベクトル化

#### FR-345: Auto-Vectorization (自動ベクトル化)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: SIMD命令（SSE/AVX/AVX-512）生成なし。スカラループのみ
- **内容**: ループベクトル化パス: (1) ループ依存性解析（配列アクセスのstride/aliasチェック）、(2) `(loop for i below n ...)` 形式のベクトル幅決定（AVX2=8 floats）、(3) `vm-simd-*` 疑似命令生成、(4) x86-64でのVEX-prefixed SSE/AVX命令列エミット。`(declare (optimize (speed 3)))` でのみ発動
- **根拠**: GCC `-O2` / Clang `-O2`の自動ベクトル化。数値演算・文字列操作で5〜10x高速化
- **難易度**: Very Hard

#### FR-346: SLP Vectorization / スーパーワードレベル並列化

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: ループ外の独立したスカラ演算のベクトル化なし
- **内容**: 隣接するストア/ロード命令を**SLPツリー**として識別し、`vpacked*` 命令にバンドル。例: 4つの連続`vm-const`+`vm-add`を`vm-simd-add-4f`に変換。LLVM SLPVectorizer相当
- **根拠**: LLVM SLP Vectorizer (2012〜)。ループのないDSPスタイルコードで効果的
- **難易度**: Hard

#### FR-347: SIMD Intrinsics API (SIMDイントリンシックAPI)

- **対象**: 新規`src/compile/simd-builtins.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `builtin-registry.lisp`の~210エントリにSIMD演算なし
- **内容**: `(cl-cc:simd:f32x4-add a b)` 形式の明示的SIMDイントリンシック。`f32x4`, `f64x2`, `i32x8`等のSIMDベクタ型。`src/compile/simd-builtins.lisp`でbuiltinエントリとして登録。CL配列↔SIMDベクタ変換ヘルパ
- **根拠**: C intrinsics (`_mm256_add_ps`) / Rust `std::arch` / Java Vector API。自動ベクトル化が効かないケースの手動制御
- **難易度**: Hard

---

### Phase 74 — コードカバレッジ・テスト品質

#### FR-350: Line/Branch Coverage (行・分岐カバレッジ)

- **対象**: `src/compile/codegen.lisp`, `tests/framework/framework.lisp`
- **現状**: テストフレームワーク（FiveAM）にカバレッジ計測機構なし。`deftest-each`/`deftest`マクロにカバレッジフックなし
- **内容**: `--coverage`コンパイルフラグでソース行・分岐ごとにカウンタ命令を埋め込み。`(cl-cc:with-coverage ...)` フォームで範囲指定計測。HTML/LCOVカバレッジレポート生成。`make test-coverage`ターゲット。`tests/`内の全ユニットテストへのカバレッジ統合
- **根拠**: GCC `--coverage` / LLVM source-based coverage / Istanbul。4322テストのカバレッジ計測でデッドパス特定
- **難易度**: Medium

#### FR-351: MC/DC Coverage (変更条件/判定カバレッジ)

- **対象**: `src/compile/codegen.lisp`, テストフレームワーク
- **現状**: 分岐カバレッジ（FR-350）も未実装
- **内容**: `(and a b c)` / `(or a b c)` / `(if (and ..) ..)` の各条件の独立的な真偽を計測。DO-178C / IEC 61508準拠の航空・安全重要システム向けカバレッジ基準。`--coverage=mcdc`フラグ
- **根拠**: GCC `-fcondition-coverage` (GCC 13+) / LLVM MC/DC Coverage (LLVM 18+)。2026年コンパイラのMC/DC対応はデファクト標準化
- **難易度**: Hard

#### FR-352: Mutation Testing (ミューテーションテスト)

- **対象**: `tests/framework/framework-fuzz.lisp`, 新規`src/tools/mutator.lisp`
- **現状**: PBT（`framework-fuzz.lisp`）はランダム入力生成。プログラム変異によるテスト有効性検証なし
- **内容**: ソースASTへのミューテーション演算子適用（条件反転`not`、算術演算子交換`+→-`、定数変更）。各ミューテーントに対してテストスイートを実行し**殺傷率**（mutation score）を算出。Pitest (Java) / mutmut (Python) 相当
- **根拠**: テストの質の定量指標。高カバレッジでも低mutation scoreならテストが弱い
- **難易度**: Hard

#### FR-353: Property-Based Testing Integration (PBT深化)

- **対象**: `tests/framework/framework-fuzz.lisp`
- **現状**: `%gen-expr`（`framework-fuzz.lisp`）は文法ベース生成。`deftest-fuzz`マクロは実装済み
- **内容**: **shrinking**: 失敗入力の最小化（バイナリサーチ型）。**stateful PBT**: コマンドシーケンス生成でVMステートマシンをテスト。**typeclass-based generators**: 型アノテーションから入力生成器を自動導出。QuickCheck-2.0 / Hypothesis スタイル
- **根拠**: Hypothesis shrinking / Erlang QuickCheck stateful testing。現在のPBTにshrinkingがないため失敗デバッグが困難
- **難易度**: Medium

---

### Phase 75 — メモリ・プロファイリング

#### FR-355: Heap Profiler (ヒーププロファイラ)

- **対象**: `src/runtime/heap.lisp`, `src/vm/vm-run.lisp`, `src/cli/main.lisp`
- **現状**: `rt-alloc`（`heap.lisp`）でオブジェクト割り当てをカウントする仕組みなし。GCサイクル統計なし
- **内容**: `--heap-profile`フラグで割り当てサイトごとのカウント/バイト記録。**アロケーションサイト**（スタックトレース付き）のホット順ランキング。Firefox DevTools/pprof形式でのレポート出力。SBCL `(sb-profile:report)` / Valgrind massif相当
- **根拠**: GCプレッシャーのホットスポット特定。セルフホスティング時のコンパイラ自身のメモリ使用プロファイリング
- **難易度**: Medium

#### FR-356: GC Statistics & Tuning API (GC統計・チューニングAPI)

- **対象**: `src/runtime/gc.lisp`
- **現状**: 2世代GC（`gc.lisp`）の統計出力・チューニングパラメータ公開なし
- **内容**: `(cl-cc:gc-stats)` → `(:minor-gcs N :major-gcs M :total-collected-bytes B :pause-ms-p99 T)`。ヒープサイズ閾値・昇格比率・フィンガープリント間隔のランタイム設定。`--gc-min-heap`/`--gc-max-heap` CLIフラグ
- **根拠**: JVM `-XX:+PrintGCDetails` / Go runtime.ReadMemStats。GCチューニングなしでは長時間実行のレイテンシ予測不可
- **難易度**: Medium

#### FR-357: Allocation-Free Hot Path Verification (割り当てフリーホットパス検証)

- **対象**: `src/compile/codegen.lisp`, テストフレームワーク
- **現状**: `assert-no-consing`（`framework-compiler.lisp:257`）は実行時チェックのみ
- **内容**: `(declare (cl-cc:no-allocation))` アノテーション付き関数が**コンパイル時**にヒープ割り当てを含むことを静的検出してエラー。逃げ場のないエスケープ解析（FR-341）との連携で保証を強化
- **根拠**: Java `@MemoryRestricted` / Rust `#[no_std]`。リアルタイム・レイテンシ敏感なコードの正確性保証
- **難易度**: Hard

---

### Phase 76 — パッケージ管理・依存解決

#### FR-360: Built-in Package Manager (組み込みパッケージマネージャ)

- **対象**: 新規`src/package-manager/`, `src/cli/main.lisp`
- **現状**: 依存管理はASDFとQuicklispに完全依存。`cl-cc.asd`の`:depends-on`は手動管理
- **内容**: `./cl-cc add <package>` / `./cl-cc remove` / `./cl-cc update`。依存ロックファイル（`cl-cc.lock`）によるSHA-256ピン留め。HTTP/HTTPS取得 + 署名検証。Quicklisp dist互換ダウンロード。`package.json` / `Cargo.toml` スタイルの`cl-cc.toml`マニフェスト
- **根拠**: Cargo / npm / uv（Python）。パッケージ管理の内製化はコンパイラエコシステムの自立性に直結
- **難易度**: Very Hard

#### FR-361: Dependency Graph Visualization (依存グラフ可視化)

- **対象**: `cl-cc.asd`, 新規`src/tools/dep-graph.lisp`
- **現状**: `cl-cc.asd`の`:depends-on`チェーンは手動把握のみ
- **内容**: `./cl-cc graph` でモジュール依存グラフをGraphviz DOT / Mermaid形式で出力。循環依存検出。コンパイル順序のクリティカルパス表示。モジュール間の輸入/輸出シンボル数ヒートマップ
- **根拠**: `cargo tree` / `npm ls` / `gradle dependencies`。大規模プロジェクトの依存管理可視化
- **難易度**: Easy

---

### Phase 77 — ホットリロード・ライブ開発

#### FR-363: Hot Code Reload (ホットコードリロード)

- **対象**: `src/vm/vm.lisp`, `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: 実行中VMへのコード変更には再起動が必要。REPL（`main.lisp:256-302`）は`compile-expression`→`run-vm`の逐次実行
- **内容**: **関数テーブルのatomic更新**: `*function-registry*`（`vm.lisp`）のエントリをCAS操作で置き換え。実行中の古い関数インスタンスは完走させ、次回呼び出しから新実装を使用。ファイル変更をinotify/FSEventsで監視する`--watch`モード。Erlang hot code loading / Clojure `clojure.tools.namespace/refresh` スタイル
- **根拠**: セルフホスティングコンパイラの開発サイクル加速。LispのREPL駆動開発の本質的機能
- **難易度**: Hard

#### FR-364: Image-Based Development (イメージベース開発)

- **対象**: `src/cli/main.lisp`, `src/vm/vm.lisp`
- **現状**: プロセス終了でVM状態（`*function-registry*`, `*class-registry*`, `*macro-env*`）は消失
- **内容**: `./cl-cc save-image foo.image` でVMヒープ・レジストリをシリアライズ。`./cl-cc load-image foo.image` で即座に復元（コンパイルなし起動）。SBCL `save-lisp-and-die` / Smalltalk image 相当。セルフホスティングコンパイラのコールドスタート時間を大幅削減
- **根拠**: Lisp imageはLisp開発の中核概念。コンパイラ自体がセルフホストなので起動時間問題が顕在化
- **難易度**: Hard

#### FR-365: REPL-in-Production / Remote REPL (本番環境REPL)

- **対象**: `src/cli/main.lisp`, 新規`src/lsp/repl-server.lisp`
- **現状**: REPLはローカルstdin/stdoutのみ（`main.lisp:256-302`）
- **内容**: `--remote-repl=:4005` でTCPソケットREPLサーバ。TLS + パスフレーズ認証。接続中の評価はサンドボックス化（read-only VMビュー）。SLIMEプロトコル互換。`(cl-cc:enable-remote-repl :port 4005 :tls t)` API
- **根拠**: Clojure nREPL / Erlang remote shell / SBCL swank。本番障害のライブ診断に不可欠
- **難易度**: Hard

---

### Phase 78 — 最新セキュリティ (2026)

#### FR-367: ARM Memory Tagging Extension / MTE (ARMメモリタギング拡張)

- **対象**: `src/emit/aarch64-codegen.lisp`, `src/runtime/heap.lisp`
- **現状**: AArch64バックエンドはMTE命令非対応。ヒープ割り当てにタグなし
- **内容**: AArch64 MTE命令生成（`irg`/`addg`/`stg`/`ldg`）。ヒープ割り当て時にランダムタグを上位4ビットに付与（`rt-alloc`改修）。ポインタ解放後のタグクリア。ミスマッチでのSIGSEGV生成。Linux `prctl(PR_SET_TAGGED_ADDR_CTRL)`初期化
- **根拠**: ARMv8.5-A MTE / Android 11+ MTE有効化。use-after-free・ヒープオーバーフローのハードウェア検出。GCC `-fsanitize=memtag`相当
- **難易度**: Hard

#### FR-368: Intel CET Shadow Stack (Intelシャドウスタック)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/binary/macho.lisp`
- **現状**: FR-238（CFI）でENDBR64の前方CFIは計画済み。後方エッジCFI（リターンアドレス保護）なし
- **内容**: x86-64 CET Shadow Stack対応: `ENDBR64`命令を全関数エントリに挿入（前方エッジ）。Mach-O/ELFの`GNU_PROPERTY_X86_FEATURE_1_SHSTK`フラグ設定。`syscall(ARCH_SHSTK_ENABLE)`初期化コード生成。Linux 6.6+ / macOS 15+でのCET有効化
- **根拠**: Intel CET（Control-flow Enforcement Technology）はLinux 6.6でデフォルト有効化。ROP攻撃への根本的対策
- **難易度**: Hard

#### FR-369: Supply Chain Security / SBOM生成

- **対象**: `cl-cc.asd`, 新規`src/tools/sbom.lisp`
- **現状**: 依存ライブラリのサプライチェーン追跡なし
- **内容**: `./cl-cc sbom` でSoftware Bill of Materials（SPDX 2.3 / CycloneDX 1.5形式）を生成。依存ライブラリのSHA-256ハッシュ・ライセンス・PURL。`--sign-sbom`でED25519署名付きSBOM生成。GitHub Dependency Graph API連携
- **根拠**: EO 14028（Biden Executive Order on Cybersecurity 2021）以降、SBOM提出が政府調達要件化。2026年時点でISO/IEC 5962として標準化
- **難易度**: Medium

#### FR-370: Constant-Time Code Generation (定数時間コード生成)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/compile/codegen.lisp`
- **現状**: タイミングサイドチャネル対策なし。条件分岐はデータ依存あり
- **内容**: `(declare (cl-cc:constant-time))` アノテーションで暗号実装向け定数時間コード生成。条件分岐→`cmov`変換。シークレット値の分岐インデックス禁止。`ct-verif` / `binsec` ツールとの連携検証
- **根拠**: Spectre以降のサイドチャネル対策。TLS/暗号ライブラリ実装の必須要件。GCC `-mbranches-within-32B-boundaries` / Clang constant-time policies
- **難易度**: Hard

---

### Phase 79 — ML駆動最適化

#### FR-372: ML-Guided Inlining (機械学習駆動インライン化)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: インライン閾値は固定値15（`optimizer.lisp:1018`）のヒューリスティック
- **内容**: **特徴量抽出**: 呼び出しサイトのcallee命令数・ループ深度・引数定数率・呼び出し頻度（PGOデータ）をベクトル化。**推論**: 軽量二値分類モデル（決定木 or 小規模NN）をコンパイル時に実行してインライン化可否判定。`cl-cc selfhost`ワークロードでトレーニングデータ収集。MLGO (Google 2021) / LLVM ML inliner相当
- **根拠**: Google MLGOプロジェクトでChromium +1.5%、SPEC CPU +0.8%改善。固定閾値より文脈依存判定が優れる
- **難易度**: Hard

#### FR-373: Neural Cost Model (ニューラルコストモデル)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: 命令コスト推定なし（後段で実行頻度独立のコスト計算なし）。ループ展開係数・ベクトル化閾値が静的
- **内容**: 実測スループット（nanobenchmark）とコードパターンから**命令列コストの回帰モデル**を学習。`(estimated-cycles instruction-sequence)` API。最適化可否判断に活用。IACA / llvm-mca相当の内製版
- **根拠**: LLVM MachineLearningRegalloc / Halide auto-scheduler。ハードウェア実測に基づくコスト推定は静的テーブルより正確
- **難易度**: Very Hard

#### FR-374: AI-Powered Error Messages (AI強化エラーメッセージ)

- **対象**: `src/parse/diagnostics.lisp`, `src/vm/conditions.lisp`
- **現状**: `format-diagnostic`（`diagnostics.lisp`）は静的テンプレートメッセージ
- **内容**: コンパイルエラー・型エラー発生時に**LLMベースの修正提案**を生成。エラーコード + コンテキスト（前後3行 + スコープ内変数名）をプロンプトとしてローカルLLM（Ollama / llama.cpp）に送信。`--ai-diagnostics`フラグで有効化。Rust compiler `rustc --explain E0xxx` の進化版
- **根拠**: 2026年時点でClang/Rustがexperimentalで導入開始。コンパイラエラーの学習コスト大幅削減
- **難易度**: Medium

---

### Phase 80 — 並列・分散コンパイル

#### FR-376: Parallel Compilation Pipeline (並列コンパイルパイプライン)

- **対象**: `src/compile/pipeline.lisp`, `cl-cc.asd`
- **現状**: コンパイルパイプラインはシングルスレッド逐次実行。ASDFの`:serial t`（`cl-cc.asd:16`）で全ファイルも直列
- **内容**: **関数レベル並列化**: 相互依存のないトップレベルフォーム（`defun`/`defclass`等）をlparallel/bordeaux-threadsのワーカープールで並列コンパイル。`compile-expression`呼び出しをスレッドセーフ化（`*function-registry*`をCAS操作で保護）。`--jobs N`フラグ（デフォルト=CPUコア数）。独立ファイル間の並列ASDFビルド（FR-325との連携）
- **根拠**: GCC `-j` / Ninja / Bazel並列実行。cl-ccのselfhost（84ファイル）をCPUコア数倍速化
- **難易度**: Hard

#### FR-377: Remote Build Execution (リモートビルド実行)

- **対象**: `src/compile/pipeline.lisp`, 新規`src/build/remote.lisp`
- **現状**: コンパイルはローカルマシンのみ
- **内容**: Bazel Remote Execution API（REAPI v2）互換のビルドキャッシュプロトコル。コンテンツアドレス型リモートキャッシュ（Action Cache + CAS）。`--remote-cache=grpc://host:8980`でCI/CDサーバのキャッシュを共用。buildbuddy / engflow等のOSSバックエンドと互換
- **根拠**: Bazel RBE / sccache --dist。CI環境での初回ビルド0秒（全ヒット）を実現
- **難易度**: Very Hard

#### FR-378: Build Artifact Signing (ビルド成果物署名)

- **対象**: `src/cli/main.lisp`, `src/emit/binary/macho.lisp`
- **現状**: 生成バイナリに暗号署名なし
- **内容**: ED25519秘密鍵によるバイナリ署名（`./cl-cc compile --sign-key=key.pem`）。Mach-O Code Signing（`LC_CODE_SIGNATURE`）。SLSA（Supply Chain Levels for Software Artifacts）Provenance記録。`sigstore` / `cosign`互換の署名形式
- **根拠**: Apple notarization要件（macOS 13+）/ GitHub artifact attestations（2024〜）。SLSA Level 3達成に必要
- **難易度**: Medium

---

### Phase 81 — WebAssembly・異種ターゲット

#### FR-380: WASM Component Model / WIT (WASMコンポーネントモデル)

- **対象**: `src/emit/wasm/`, `src/cli/main.lisp`
- **現状**: WASM32バイナリ出力は`target.lisp`に定義済みだが、コンポーネントモデル（`.wasm`コンポーネント）非対応
- **内容**: **WIT（WebAssembly Interface Types）** IDLからCL型マッピングを自動生成。コンポーネントバイナリ形式（`.wasm` v2 + `component` section）エンコーダ。Canonical ABI（lift/lower関数）のコード生成。`wasm-tools compose`互換の出力
- **根拠**: WASI Preview 2（2024安定化）/ WIT IDL（2025 W3C標準化）。WASMコンポーネント間でのCLコード再利用がFFIなしに可能
- **難易度**: Very Hard

#### FR-381: WASI Support (WASIサポート)

- **対象**: `src/emit/wasm/`, `src/vm/io.lisp`
- **現状**: `src/vm/io.lisp`のファイルI/OはSBCL CLOSを直接呼ぶ。WASM実行環境ではcl-ioが使えない
- **内容**: `wasi:filesystem`/`wasi:cli`/`wasi:sockets` (Preview 2) への呼び出し生成。`cl-cc:with-wasi-env`マクロ。`(open "file.txt")` → `wasi_filesystem_open_at` インポート変換。wasmtime / Deno WASI実行確認
- **根拠**: WASI Preview 2は2024年安定化。サーバーレス/エッジ環境でのLispバイナリ実行の基盤
- **難易度**: Hard

#### FR-382: SPIR-V / GPU Target (SPIR-Vターゲット)

- **対象**: `src/emit/target.lisp`, 新規`src/emit/spirv/`
- **現状**: ターゲット定義は x86-64/AArch64/RISC-V/WASM32 の4つのみ
- **内容**: SPIR-V 1.6（Vulkan 1.3 / OpenCL 3.0対応）バイナリエンコーダ。`(cl-cc:define-kernel)` マクロで数値集約ループをSPIR-V関数にコンパイル。スカラ型の自動SIMD幅推定。CL配列↔GPU バッファ転送ヘルパ。MoltenVK（macOS）/ ROCm（AMD）/ CUDA（NVIDIA via `nvptx`）バックエンド
- **根拠**: Julia GPU.jl / FUTHARK / Halide。HPC・機械学習ワークロードのGPUオフロード
- **難易度**: Very Hard

#### FR-383: Source Maps for WASM/JS (ソースマップ)

- **対象**: `src/emit/wasm/`, 新規`src/emit/js/`
- **現状**: DWARF情報（FR-195）はネイティブバイナリ向け。WASM/JSデバッグ情報なし
- **内容**: WASM出力に`sourceMappingURL`カスタムセクション付与。Source Map v3 JSON生成（`.map`ファイル）。CL S-式の行列番号→WASM byte offset マッピング。Chromeデバッガ / `wasm-sourcemap`ツールとの互換性
- **根拠**: Emscripten / wasm-bindgen のソースマップ出力。WASM上でCLコードをChromeで直接デバッグ可能に
- **難易度**: Medium

#### FR-384: Universal Binary / Fat Binary (ユニバーサルバイナリ)

- **対象**: `src/emit/binary/macho.lisp`, `src/cli/main.lisp`
- **現状**: Mach-Oバイナリは単一アーキテクチャのみ
- **内容**: Mach-O Universal Binary（`FAT_MAGIC = 0xcafebabe`）エンコーダ。`./cl-cc compile --arch=universal input.lisp` でx86-64 + AArch64スライスを1ファイルに格納。スライスのアライメント（ページ境界）とオフセット計算。`lipo -info`互換の出力
- **根拠**: macOS Monterey以降のApple Silicon/Intel両対応バイナリ要件。`/usr/bin`ツール群はすべてFat Binary
- **難易度**: Medium

---

### Phase 82 — 構造化並行性・コルーチン

#### FR-386: Coroutine / Generator Compilation (コルーチン・ジェネレータコンパイル)

- **対象**: `src/expand/macros-sequence.lisp`, `src/compile/cps.lisp`
- **現状**: CPS変換（`cps.lisp`）はすべてのcontinuationをクロージャで表現。コルーチン専用構文なし
- **内容**: `(cl-cc:define-generator (fibonacci) ...)` + `(cl-cc:yield value)` マクロ。CPS変換でyield pointを自動抽出し**状態機械**に変換。`(cl-cc:next gen)` でステップ進行。`for ... in (fibonacci)` のloop統合。Python generators / Rust `impl Iterator` 相当
- **根拠**: CPS変換がすでに存在するため、yieldは本質的に「current continuationの保存」として自然に実装できる。`(call/cc)` 的アプローチも可能
- **難易度**: Medium

#### FR-387: Green Threads / Fiber Runtime (グリーンスレッド/ファイバーランタイム)

- **対象**: `src/vm/vm-run.lisp`, `src/runtime/`, 新規`src/concurrent/`
- **現状**: VMはシングルスレッドの逐次実行器
- **内容**: M:N スケジューラ（OS Nスレッド上でMグリーンスレッド）。スタック切り替え（`setjmp`/`longjmp` or `ucontext_t`、ネイティブバイナリ向け）。VMレベルでのfiberサポート（スタックポインタ保存+復元）。`(cl-cc:spawn (lambda () ...))` + `(cl-cc:yield)` API。`(cl-cc:channel)` でCommunicating Sequential Processes（CSP）スタイル
- **根拠**: Go goroutine / Erlang process / Kotlin coroutines。並行I/Oサーバー・並列コンパイルの基盤
- **難易度**: Very Hard

#### FR-388: Async/Await Compilation (async/awaitコンパイル)

- **対象**: `src/expand/expander.lisp`, `src/compile/cps.lisp`
- **現状**: 非同期I/Oパターンなし。`src/vm/io.lisp`はブロッキングI/O
- **内容**: `(cl-cc:async (defun fetch (url) ... (cl-cc:await (http-get url)) ...))` マクロ。`async`関数をCPS変換でステートマシンに変換（async/awaitはCPSの糖衣構文）。`Promise<T>`型相当の`future`オブジェクト。イベントループ（libuv/io_uring統合またはピュアCL実装）
- **根拠**: CPS変換（FR-386の基盤）がすでに存在するため、async/awaitはほぼ「CPSをユーザー向けに公開する」操作に相当する
- **難易度**: Hard

---

### Phase 83 — 抽象解釈・形式検証

#### FR-390: Value Range Propagation / VRP (値域伝播)

- **対象**: `src/optimize/optimizer.lisp`, 新規`src/optimize/vrp.lisp`
- **現状**: 定数畳み込みは`vm-const`に限定。変数の値域情報なし
- **内容**: SSA変数に`[min, max]`区間を付与し**区間演算**で伝播。例: `(< n 256)`分岐後のthenブランチで`n`の区間が`[0,255]`に収束→配列境界チェック除去・タグ検査省略。ループの`(loop for i from 0 below n)`でi∈[0,n-1]を静的確立。GCC VRP / LLVM LazyValueInfo相当
- **根拠**: FR-147（SSA統合）の実践的成果。境界チェック除去は数値集約コードで最大30%高速化
- **難易度**: Hard

#### FR-391: Abstract Interpretation Framework (抽象解釈フレームワーク)

- **対象**: 新規`src/analyze/abstract-interp.lisp`
- **現状**: 型推論（HM型）とエフェクト分類のみ。格子理論ベースの汎用解析フレームワークなし
- **内容**: **抽象ドメイン**（定数格子・区間格子・符号格子・null安全性格子）の宣言的定義。`(defabsdomain interval ...)` マクロ。固定点計算（widening/narrowing）の汎用実装。複数ドメインの直積合成。VRP（FR-390）・null安全性解析・符号解析をこのフレームワーク上で実装
- **根拠**: Astrée静的解析器（バグゼロ証明）/ IKOS（NASA）/ Infer（Meta）の技術基盤
- **難易度**: Very Hard

#### FR-392: SMT Solver Integration (SMTソルバー統合)

- **対象**: 新規`src/analyze/smt.lisp`
- **現状**: 型チェックと最適化は構文的マッチングのみ。決定可能な論理式への還元なし
- **内容**: Z3 / CVC5へのFFI経由呼び出し。`(declare (cl-cc:ensures (> result 0)))` → SMT検証条件生成。ループ不変条件の自動導出。配列境界の静的証明。`--verify`フラグでアサーション検証
- **根拠**: Dafny / Why3 / F*。Critical systemsでのコードの部分正当性証明に向けた基盤
- **難易度**: Very Hard

---

### Phase 84 — CHERI・最先端ハードウェアセキュリティ

#### FR-395: CHERI Capability Pointer Support (CHERIケイパビリティポインタ)

- **対象**: `src/emit/aarch64-codegen.lisp`, `src/runtime/heap.lisp`
- **現状**: ポインタは単純な64ビット整数。メタデータなし
- **内容**: ARM Morello / RISC-V CHERI向けの**fat pointer**生成（128ビット: アドレス64bit + メタデータ64bit）。`rt-alloc`でallocated rangeをケイパビリティに埋め込み。範囲外アクセスでのハードウェアトラップ。`--target=morello`コンパイルフラグ。CheriBSD上でのバイナリ動作確認
- **根拠**: CHERI研究プロジェクト（Cambridge大学）+ ARM Morello（2022〜）。2026年時点で組み込み・サーバーへの採用拡大。C言語の空間安全性問題を根本解決するハードウェア
- **難易度**: Very Hard

#### FR-396: RISC-V Vector Extension / RVV (RISC-VベクタISA)

- **対象**: 新規`src/emit/riscv64-codegen.lisp`
- **現状**: `*riscv64-target*`（`target.lisp`）は定義済みだがバックエンド実装なし
- **内容**: RISC-V Vector Extension（RVV 1.0）命令生成。可変長VLEN対応のvl/vtype設定。`vle32.v`/`vfmacc.vv`等のベクタロード・演算命令エンコーダ。FR-345（自動ベクトル化）のRISC-Vバックエンドとして統合
- **根拠**: SiFive / SpacemiT-X60（RISC-V V extension）/ C908。2026年時点でRISC-Vは組み込み〜HPC領域で急速普及。RVVはx86 AVXに相当
- **難易度**: Hard

#### FR-397: DWARF 5 / Variable Location Quality (DWARF 5・変数位置品質)

- **対象**: `src/emit/binary/macho.lisp`, FR-195（DWARF基本）の拡張
- **現状**: FR-195でDWARF基本情報生成を計画。最適化後の変数追跡は未考慮
- **内容**: **DWARF 5固有機能**: `DW_OP_entry_value`（関数エントリ時の引数値復元）、`DW_AT_default_value`（最適化で消えた変数のデフォルト値）、`DW_TAG_skeleton_unit`（薄いデバッグ情報+外部`.dwo`分離）。最適化後もデバッガで変数が`<optimized out>`にならないよう変数ロケーションリスト（`DW_AT_location` + ロケーションリスト）を生成
- **根拠**: DWARF 5（2017 / LLVM採用2019〜）/ GDB 10+。最適化コードのデバッグ体験が大幅改善
- **難易度**: Hard

#### FR-398: Compiler Plugin / Extension API (コンパイラプラグインAPI)

- **対象**: `src/expand/expander.lisp`, `src/optimize/optimizer.lisp`, `src/cli/main.lisp`
- **現状**: マクロ拡張は可能（`defmacro`）だが最適化パス・コード生成フックの外部登録API なし
- **内容**: `(cl-cc:define-compiler-pass :after :fold-constants ...)` でユーザー定義最適化パスを登録。`(cl-cc:define-emit-hook :before :function-entry ...)` でコード生成フック。ダイナミックロード可能なプラグイン（`.so`）。GCC Plugin API / LLVM PassPlugin / Rust compiler plugins（rustc_private）相当
- **根拠**: ドメイン特化最適化（DSL用）・プロファイルインストルメンテーション・独自ABI生成など、コンパイラ本体を変更せずに拡張できる
- **難易度**: Hard

#### FR-399: Thread Sanitizer (スレッドサニタイザ)

- **対象**: `src/vm/vm-run.lisp`, `src/runtime/`, `src/emit/x86-64-codegen.lisp`
- **現状**: FR-239はAddressSanitizer/UBSanのみ。データ競合検出なし
- **内容**: **Shadow memory方式**: 各メモリアドレスに最終アクセスしたスレッドIDとロック状態を記録（8バイト→2バイトシャドウ）。ロック取得/解放のインストルメンテーション。同期なしでの同一アドレスへの競合アクセス検出。FR-387（グリーンスレッド）の安全性検証に利用。Helgrind / LLVM TSan相当
- **根拠**: データ競合は未定義動作でありデバッグが極めて困難。並行ランタイム（FR-387）の開発に必須
- **難易度**: Hard

---

### Phase 85 — 高度最適化パス

#### FR-400: Loop Invariant Code Motion / LICM (ループ不変コード移動)

- **対象**: `src/optimize/optimizer.lisp`, FR-147（SSA統合）前提
- **現状**: ループ内の不変式はループごとに再評価される。ループボディから外への移動なし
- **内容**: **支配木ベースのホイスト**: ループヘッダを支配するブロックにループ不変命令を移動。副作用なし（FR-152の純粋性推論）かつ定義がループ内のみの命令を対象。逆方向にはシンク（cold pathへ押し込み）も実施。CFG構築（FR-147）の副産物として支配木が利用可能
- **根拠**: GCC `-fmove-loop-invariants` / LLVM `loop-rotate` + `licm`。ループ集中型コードでの最重要最適化の一つ
- **難易度**: Medium

#### FR-401: Loop Unrolling (ループ展開)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: ループ展開なし。カウントが定数のloopも1回ずつ実行
- **内容**: **完全展開（unroll-all）**: トリップカウント≤8かつ定数の場合にループ本体をN回複製してジャンプ除去。**部分展開（unroll-factor）**: 可変長ループに対して展開係数2/4を適用しパイプラインハザード削減。末尾処理ループ（peel）自動生成。`(declare (cl-cc:unroll 4))` 明示指示
- **根拠**: GCC `-funroll-loops` / LLVM `loop-unroll`. 小ループのオーバーヘッド除去・SIMD幅合わせに必須
- **難易度**: Medium

#### FR-402: Loop Fusion / Loop Fission (ループ融合・分割)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 隣接する独立ループは個別に実行される
- **内容**: **Loop Fusion**: 同一反復空間の隣接ループを1ループに統合（キャッシュ局所性向上、ループオーバーヘッド削減）。**Loop Fission**: 依存関係が複雑なループを独立部分に分割してベクトル化阻害を除去。依存グラフ解析（FR-342ポインタ解析との連携）
- **根拠**: GCC loop-fusion / Polly / LLVM LoopFuse。数値計算・データ変換ループで顕著な効果
- **難易度**: Hard

#### FR-403: Loop Tiling / Blocking (ループタイル化)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 多重ループのイテレーション順序変換なし
- **内容**: 2重以上のネストループを**タイル（ブロック）**単位で実行順序を変換。内ループのアクセスパターンがキャッシュラインに収まるようタイルサイズを自動決定（L1/L2サイズから計算）。行列乗算・画像フィルタ等への適用。PLUTO / Polly (LLVM) 相当の多面体変換の簡略版
- **根拠**: キャッシュミス削減で行列演算が5〜10x高速化。数値演算系の最重要変換の一つ
- **難易度**: Hard

#### FR-404: Global Value Numbering / GVN (大域値番号付け)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: CSEパス（`opt-pass-common-subexpressions`）は局所的なブロック内のみ
- **内容**: **SSAベースGVN**: 各SSA値に**値番号**を付与（意味的同値な式は同一番号）。支配木を辿ってDOMツリー全体で重複式を検出・除去。`vm-phi`ノードの合体。`x + 0`/`x * 1`等の代数的恒等式もGVNルールで統一的に処理。LLVM GVN / Click's 1995 GVN相当
- **根拠**: ブロックローカルCSEより大幅に強力。関数全体を通じた重複計算の除去
- **難易度**: Hard

#### FR-405: Sparse Conditional Constant Propagation / SCCP (疎条件定数伝播)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 定数畳み込みはシングルパス（`opt-pass-fold-constants`）。条件分岐による到達不能パス除去との連携なし
- **内容**: **CFG + 定数伝播の同時実行**: 条件分岐の条件が定数と判明した時点で到達不能パスをDCEし、その結果新たな定数が発生する連鎖を一括処理。WorlistアルゴリズムでSSA値とCFGエッジの状態（unreachable/constant/varying）を伝播。Wegman-Zadeck (1991) アルゴリズム
- **根拠**: LLVM `sccp` / GCC `ccp`. 通常のCP+DCEの個別適用より発見できる定数が格段に多い
- **難易度**: Hard

#### FR-406: Partial Redundancy Elimination / PRE (部分冗長除去)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: CSEはすべてのパスで重複する式のみ除去（完全冗長性）
- **内容**: **一部のパスで重複する式**も除去対象に拡張。critical edgeへのinsertionで非冗長パスに「補完計算」を挿入してすべてのパスで冗長にした後CSE。ループ不変コード移動（FR-400）をPREの特殊ケースとして統一。Knoop-Rüthing-Steffen (1992) lazy code motionアルゴリズム
- **根拠**: LLVMはGVN+PREを統合（`gvn-hoist`/`gvn-sink`）。CSE+LICMより強力な汎用最適化
- **難易度**: Very Hard

#### FR-407: Strength Reduction (強度低減)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 乗算・除数がべき乗の場合のシフト変換のみ（`opt-pass-fold-constants`の一部）
- **内容**: **誘導変数に対する強度低減**: ループ内の`i * constant`を加算の列に変換。`i * 7`→ `s0=0; loop: s0+=7`。`mod power-of-2`→ビットマスク。`/ constant`→乗法逆数+シフト（`imul`+`sar`命令列）。GCC `-fstrength-reduce` 相当。整数除算は最大5倍高速化
- **根拠**: 整数除算はx86-64で60〜80クロックかかる最重量命令。コンパイラによる除算→乗法変換は基本中の基本
- **難易度**: Medium

#### FR-408: Instruction Scheduling (命令スケジューリング)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: 命令はVM→IR→機械語の変換順に出力。パイプライン考慮なし
- **内容**: **命令レベル並列性（ILP）抽出**: データ依存グラフ（DDG）構築→トポロジカルソート→レイテンシ考慮スケジューリング（list scheduling）。x86-64のAgner Fog命令表（レイテンシ/スループット）参照。`fmul`後の`fadd`（latency=4）間に独立命令を挟む。out-of-order CPUでも効果的なin-order発行を補助
- **根拠**: GCC `-fschedule-insns` / LLVM MachineScheduler。浮動小数点集約コードで15〜25%高速化
- **難易度**: Hard

#### FR-409: Memory SSA (メモリSSA)

- **対象**: 新規`src/optimize/memory-ssa.lisp`, `src/optimize/optimizer.lisp`
- **現状**: ヒープアクセス（`vm-slot-read`/`vm-slot-write`）はSSAの枠外で依存関係が保守的
- **内容**: **MemorySSA**: 各ヒープ書き込みを仮想SSAメモリ定義（MemoryDef）、読み込みをMemoryUse、phi合流点をMemoryPhiとして表現。GVN（FR-404）・PRE（FR-406）・LICM（FR-400）のメモリ操作への適用を可能にする。`(slot-read obj :x)` のループ不変ホイストをエイリアス解析（FR-340）と組み合わせて実施
- **根拠**: LLVM MemorySSA（2017〜）。メモリ最適化の精度を飛躍的に向上させるインフラ。FR-340〜FR-342の解析を統合するハブ
- **難易度**: Very Hard

#### FR-410: If-Conversion / Predication (分岐→選択命令変換)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `if`は常に条件分岐命令（`jcc`）にコンパイル。branch mispredictionのペナルティあり
- **内容**: **If-conversion**: 短い`(if cond then else)`で`then`/`else`ともに副作用なし・命令数≤4の場合、両方を無条件実行して結果を`cmov`（条件付き移動）で選択。**Predication**: AArch64のcondition codesを活用した条件実行命令生成。分岐予測ミス（~15クロック）を`cmov`（1クロック）で置換
- **根拠**: GCC `-fif-conversion` / LLVM SelectionDAG if-conv. 予測困難な短い条件分岐で顕著な効果
- **難易度**: Medium

---

### Phase 86 — 高度コード生成・ABI

#### FR-412: NaN Boxing / Pointer Tagging Optimization (NaNボクシング)

- **対象**: `src/vm/vm.lisp`, `src/runtime/heap.lisp`
- **現状**: タグ付きポインタは下位2〜3ビットを使用（`vm.lisp`のタグ定数）。64bit全体の活用なし
- **内容**: **NaN boxing**: IEEE 754 doubleのsilent NaN空間（`0x7FF8000000000000`〜`0x7FFFFFFFFFFFFFFF`）にfixnum・pointer・booleanを格納。floatはそのままで他の型はNaN空間にエンコード。メモリ帯域50%削減（8バイト/値を維持しつつboxingゼロ）。LuaJIT・JavaScriptCore・Duktapeの採用実績
- **根拠**: 動的型付け言語での最も効率的な値表現。現在のタグ付きポインタより値の解釈が1命令高速
- **難易度**: Hard

#### FR-413: Register Coalescing (レジスタ合体)

- **対象**: `src/compile/regalloc.lisp`
- **現状**: コピー命令（`vm-move`）をレジスタ割り当て後に除去する仕組みなし
- **内容**: **Interference Graph合体**: コピー命令の src/dst が干渉しない場合に同一物理レジスタに割り当て、`mov rax, rbx` を除去。Conservative coalescing（Chaitin-Briggs）+ Aggressive coalescing（George-Appel）の2段階。`vm-move`命令の大幅削減（関数体に平均10〜20%のコピーが存在）
- **根拠**: LLVM RegisterCoalescer / GCC regmove. コピー命令は最も除去コストパフォーマンスの高い最適化
- **難易度**: Hard

#### FR-414: Custom Calling Convention (カスタム呼び出し規約)

- **対象**: `src/emit/calling-convention.lisp`, `src/compile/codegen.lisp`
- **現状**: すべての内部関数呼び出しがシステムABI（System V AMD64 / AAPCS64）を使用
- **内容**: **fast-call規約**: 引数・戻り値を最大限レジスタで渡す内部専用規約（呼び出し側と被呼び出し側が同じコンパイラ管轄の場合）。**tailcc**: 末尾呼び出し最適化専用の規約（スタックフレーム不変）。`(declare (cl-cc:calling-convention :fast))` アノテーション。FFI境界はシステムABIを維持
- **根拠**: LLVM fastcc / GCC `__attribute__((regparm))`. インタープロシージャル呼び出しのオーバーヘッドを30〜50%削減
- **難易度**: Hard

#### FR-415: Frame Pointer Omission / FPO (フレームポインタ省略)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: 関数プロローグ/エピローグにRBP保存・復元が常に含まれるか不明
- **内容**: リーフ関数（内部呼び出しなし）または固定スタックフレームサイズの関数でRBPを通常レジスタとして使用。`-O2`以上でデフォルト有効、`-fno-omit-frame-pointer`で無効化。DWARF CFI（Call Frame Information）を`DW_CFA_def_cfa_expression`で正確に記述してデバッガ互換性維持
- **根拠**: GCC `-fomit-frame-pointer` / Clang. RBP1本分のレジスタ節約はx86-64（16本中）で~6%の差
- **難易度**: Medium

#### FR-416: Stack Frame Packing (スタックフレームパッキング)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: ローカル変数のスタック配置にアライメント最適化なし
- **内容**: ローカル変数をサイズ降順にソートして配置（大きいものから並べてパディング最小化）。`sizeof(fixnum)=8` / `sizeof(bool)=1` の混在時にギャップを詰める。スタックフレームサイズ削減→キャッシュライン占有率低下
- **根拠**: GCC `-fpack-struct` の関連技術。スタックフレームの無駄なパディングはキャッシュ汚染の原因
- **難易度**: Easy

#### FR-417: Monomorphization (単相化)

- **対象**: `src/compile/codegen.lisp`, `src/type/inference.lisp`
- **現状**: ジェネリック関数は常に動的ディスパッチ（CLOSメソッドテーブル）。型特化版の静的生成なし
- **内容**: `(declaim (ftype (function (fixnum fixnum) fixnum) +))` 等の型宣言がある関数呼び出しで、**型特化クローン**を生成（`add-fixnum-fixnum`等の命名規則）。型引数の組み合わせ数が閾値以内の場合に展開。ボックス化解除（fixnum引数のuntagging）と組み合わせて純粋整数演算に変換。Rust/C++テンプレートの動的言語版
- **根拠**: 型情報を最大活用したコード特化。CLの`(the fixnum x)`宣言と組み合わせて効果的
- **難易度**: Hard

---

### Phase 88 — GC・メモリランタイム高度化

#### FR-420: Concurrent / Incremental GC (並行・インクリメンタルGC)

- **対象**: `src/runtime/gc.lisp`
- **現状**: 2世代GC（`gc.lisp`）はStop-The-World方式。コレクション中はVM実行停止
- **内容**: **インクリメンタルマーク**: マークフェーズを小ステップに分割してVM実行と交互実行。**Tri-color marking**（白/灰/黒）+ write barrier（書き込み時に参照先を灰に再マーク）。**並行スイープ**: スイープをバックグラウンドスレッドで実施。最大停止時間をパラメータ化（`--gc-max-pause 5ms`）。Dijkstra Tri-color algorithm
- **根拠**: Go GC / V8 Orinoco / OpenJDK G1。リアルタイム応答性の基本要件。セルフホスティングコンパイラの長時間実行時のレイテンシ改善
- **難易度**: Very Hard

#### FR-421: Compacting GC / Moving GC (コンパクティングGC)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: 現GCは非移動型（オブジェクトのアドレスが固定）
- **内容**: **Mark-Compact**: フルGC時にライブオブジェクトをヒープ先頭に詰め直してフラグメンテーション解消。ポインタ更新のための**forwarding pointer**テーブル。**Semi-space GC**（Cheney copying）をYoung generationに適用。コンパクション後のキャッシュ局所性向上
- **根拠**: JVM Shenandoah / ZGC / OCaml GC。長時間実行でのヒープフラグメンテーション解消。フラグメントが激しいと実効ヒープが理論値の50%以下に
- **難易度**: Very Hard

#### FR-422: Weak References & Finalizers (弱参照・ファイナライザ)

- **対象**: `src/vm/vm.lisp`, `src/runtime/gc.lisp`
- **現状**: CLオブジェクトへの参照はすべて強参照。GCによる回収を阻む
- **内容**: `(cl-cc:make-weak-pointer obj)` + `(cl-cc:weak-pointer-value wp)`。GCマークフェーズ後に弱参照リストをスキャンして未マークオブジェクトをnilに差し替え。**Finalizers** (`cl-cc:finalize obj callback`): 回収直前にコールバック呼び出し（ファイル・ネットワーク等のリソース解放）。`weak-hash-table`実装（キャッシュの自動エビクション）
- **根拠**: SBCL weak-pointer / Haskell `System.Mem.Weak` / Python `weakref`。メモリリーク回避のためのキャッシュ実装に不可欠
- **難易度**: Medium

#### FR-423: Arena / Region-Based Memory (アリーナ・領域ベースメモリ)

- **対象**: `src/runtime/heap.lisp`, 新規`src/runtime/arena.lisp`
- **現状**: すべての割り当てはGC管理ヒープへ。一括解放の仕組みなし
- **内容**: `(cl-cc:with-arena (ar) ...)` マクロでアリーナスコープを定義。スコープ内の`rt-alloc`はアリーナバンプポインタから確保。スコープ終了時に**一括解放**（GCなし、O(1)）。コンパイルフェーズ（CPS変換・最適化）の一時オブジェクトに適用してGCプレッシャー大幅削減。Zig arena allocator / Rust `bumpalo` クレート相当
- **根拠**: コンパイラの各パスは大量の一時ASTノードを生成後に全廃棄するため、アリーナが最適なアロケーションパターン
- **難易度**: Medium

#### FR-424: Pinned Objects / GC-Safe FFI (ピン留めオブジェクト)

- **対象**: `src/runtime/gc.lisp`, `src/compile/codegen.lisp` (FFI)
- **現状**: FFI呼び出し（FR-194）中にGCが動くとLispオブジェクトのアドレスが変わる可能性（FR-421コンパクティングGC導入後）
- **内容**: `(cl-cc:with-pinned-object (obj) ...)` マクロ。GCコレクション中のオブジェクト移動を一時禁止（pinセット管理）。FFIハンドシェイク（`gc-safe-point`フック）。JNI Critical Region / .NET `fixed` ステートメント相当
- **根拠**: FR-421（Moving GC）と FR-194（FFI）の共存に必須。Moving GCなしでも将来の拡張に備えて設計すべき
- **難易度**: Hard

#### FR-425: Reference Counting Mode (参照カウントモード)

- **対象**: `src/runtime/`, `src/compile/codegen.lisp`
- **現状**: GCのみでオブジェクト管理。決定論的な破棄のタイミングなし
- **内容**: `--memory-model=arc`フラグで**自動参照カウント**（ARC）モード。`vm-retain`/`vm-release`命令の自動挿入（コンパイル時挿入）。エスケープ解析（FR-341）でstatic所有権が証明できる場合はRC操作省略。循環参照用weak-pointer（FR-422）との組み合わせ。Objective-C ARC / Swift ARC / Rust Arc<T>相当
- **根拠**: 組み込み環境でのGC不使用・決定論的リソース管理に必須。IoT/リアルタイム系への展開に向けた選択肢
- **難易度**: Very Hard

---

### Phase 89 — 型システム拡張

#### FR-428: Occurrence Typing (オカレンス型付け)

- **対象**: `src/type/inference.lisp`, `src/compile/codegen.lisp`
- **現状**: 型推論はHindley-Milner（`inference.lisp`）。`(typep x 'fixnum)` 分岐後に型が絞り込まれない
- **内容**: `(if (typep x 'fixnum) <then> <else>)` のthenブランチでxを`fixnum`型に絞り込み。`(if (null x) <then> <else>)` のelseブランチで`x`からnilを除く。typecase/cond-typeのパターンに対して**型環境の分岐**を管理。型絞り込み後のdevirt（FR-337）・unboxing（FR-417）に連携
- **根拠**: TypeScript / Typed Racket / Flow。動的型チェックを静的最適化のヒントに変換する最重要機能
- **難易度**: Hard

#### FR-429: Row Polymorphism (行多相性)

- **対象**: `src/type/inference.lisp`, `src/type/types.lisp`
- **現状**: CLOSオブジェクトの型はclass名による公称型。構造的部分型なし
- **内容**: `{:x fixnum, :y fixnum | r}` 形式のrow型変数。スロット`:x`と`:y`を持つ任意のオブジェクトを受け付ける多相型。OCaml object types / PureScript / Elm と同等。`(defun distance (p) (sqrt (+ (slot-value p :x)² (slot-value p :y)²)))` が任意のxy-スロット保持オブジェクトに機能
- **根拠**: CLのCLOSは公称型だがrow多相を加えると構造的サブタイピングが可能。duck typingを型安全にする
- **難易度**: Very Hard

#### FR-430: Unboxed Representations (アンボックス表現)

- **対象**: `src/compile/codegen.lisp`, `src/runtime/heap.lisp`
- **現状**: fixnum・float含むすべての値がタグ付き表現（boxed）またはヒープオブジェクト
- **内容**: `(declare (type fixnum x))` 宣言のある局所変数を**untagged 64-bit integer**として扱い、タグ付け/外し命令を除去。`(simple-array fixnum (*))` を unboxed fixnum配列として格納（8バイト/要素、GCスキャン不要）。float配列も同様（double-float = 8バイト生値）。GCルートとしてのマーキングから除外
- **根拠**: SBCL `(declare (type (simple-array fixnum) arr))` は既にunboxed最適化を行う。数値集約コードで2〜4x高速化
- **難易度**: Hard

#### FR-431: Staged Compilation / Multi-Stage Programming (段階的コンパイル)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: コンパイル時と実行時の2フェーズのみ。コンパイル時に実行時コードを生成するメタプログラミングが`defmacro`に限定
- **内容**: `(cl-cc:$` ... `)` でコンパイル時に実行される式を明示。`(cl-cc:~` ... `)` でコード引用（引用されたコードはコンパイル先コードに挿入）。MetaOCaml `.<` / `>.` ブラケット / BER MetaOCaml相当。`(cl-cc:run ...)` で段階式を実行時コードとして解釈実行
- **根拠**: MetaOCaml / Scala LMS / Terra。高性能DSL・特化コンパイラ・解釈器の特化に不可欠
- **難易度**: Very Hard

#### FR-432: Null Safety Analysis (null安全性解析)

- **対象**: `src/type/inference.lisp`, `src/compile/codegen.lisp`
- **現状**: nilとnon-nilの型レベル区別なし。`(car nil)`等のnilデリファレンスが実行時まで検出されない
- **内容**: 型に**nullable/non-null**修飾子を追加（`fixnum?`=nullable-fixnum, `fixnum`=non-null）。nilリテラルの型を`null`に。関数宣言で`(declaim (ftype (function (fixnum?) fixnum) safe-add))` が可能に。(if (null x) ...)ブランチ後の絞り込み（FR-428 Occurrence typingとの統合）。Kotlin `?` / Swift Optional / Rust `Option<T>` 相当
- **根拠**: Lispでのnilバグは最頻出エラーのひとつ。型レベルでの検出は実行時コスト0で安全性向上
- **難易度**: Hard

---

### Phase 90 — LSP深化・IDE統合

#### FR-435: Debug Adapter Protocol / DAP (デバッグアダプタプロトコル)

- **対象**: 新規`src/lsp/dap.lisp`, FR-319（LSP）のサイドカー
- **現状**: LSP（FR-319）は言語機能のみ。デバッグプロトコルなし
- **内容**: DAP（Debug Adapter Protocol、Microsoft）実装。`launch`/`attach`リクエスト→VM実行制御。`setBreakpoints`→FR-310（インタラクティブデバッガ）との連携。`stackTrace`→FR-313（コールスタック整形）連携。`variables`→FR-315（オブジェクトインスペクタ）連携。VS Code / Emacs dap-mode / Vim から透過的にcl-ccプログラムをデバッグ可能に
- **根拠**: DAP は2017年 Microsoft標準化、2026年時点で主要IDEすべてが対応。LSP+DAPで完全なIDE体験を提供
- **難易度**: Hard

#### FR-436: Semantic Tokens (セマンティックトークン)

- **対象**: `src/lsp/`, `src/expand/expander.lisp`
- **現状**: LSP（FR-319）での構文ハイライトはTextMate文法ベースのみ
- **内容**: LSP `textDocument/semanticTokens`ハンドラ。パース済みASTから各シンボルのセマンティックカテゴリを返す（`function`/`variable`/`macro`/`type`/`parameter`/`keyword`/`builtin`）。未定義変数・シャドウイング変数に`modifiers: ["deprecated"]`/`["readonly"]`付与。テーマに依存しない意味的ハイライト
- **根拠**: LSP 3.16（2021〜）のセマンティックトークン。Rust-analyzer / clangd ともに実装済み。`defmacro`と`defun`を色分けするのは構文的ハイライトでは不可能
- **難易度**: Medium

#### FR-437: Inlay Hints (インレイヒント)

- **対象**: `src/lsp/`, `src/type/inference.lisp`
- **現状**: 型推論結果（FR-127等）はコンパイルメッセージでのみ確認可能
- **内容**: LSP `textDocument/inlayHint`ハンドラ。**型ヒント**: `(let ((x (+ 1 2))) ...)` の `x` 隣に `: fixnum` を表示。**パラメータ名ヒント**: `(foo 1 2)` を `(foo a: 1 b: 2)` 表示。**戻り値型ヒント**: 関数定義の閉じ括弧後に推論された型を表示。VS Code で表示/非表示をトグル可能
- **根拠**: LSP 3.17（2022〜）. Rust-analyzer のインレイヒントが好評を博し、Kotlin/C#/Java も追随。型推論結果の可視化でコードの理解が大幅に向上
- **難易度**: Medium

#### FR-438: Code Actions / Quick Fixes (コードアクション・クイックフィックス)

- **対象**: `src/lsp/`, `src/parse/diagnostics.lisp`
- **現状**: 構造化診断（FR-317）のfix-itサジェストはテキスト形式のみ
- **内容**: LSP `textDocument/codeAction`ハンドラ。**Quick fixes**: 未使用変数→`(declare (ignore x))`挿入、型不一致→`(the type x)`ラップ、deprecated関数→推奨代替への自動置換。**Refactoring actions**: rename symbol（LSP `rename`）、extract function、inline function。IDEの電球マークから1クリックで適用
- **根拠**: LSP Code Actions は rust-analyzer / clangd の最重要機能の一つ。FR-317の診断インフラを活用して追加コストが低い
- **難易度**: Medium

#### FR-439: Call Hierarchy (コール階層)

- **対象**: `src/lsp/`
- **現状**: 定義ジャンプ（LSP `definition`）のみ。誰がその関数を呼ぶかの逆引き不可
- **内容**: LSP `callHierarchy/incomingCalls` + `outgoingCalls`ハンドラ。`*function-registry*`の呼び出しグラフを逆引きインデックス化。再帰・相互再帰の視覚化。VS Code Call Hierarchy ビュー対応
- **根拠**: LSP 3.16（2020〜）. 大規模CLプロジェクトでの「どこからこの関数が呼ばれるか」確認が困難な問題を解決
- **難易度**: Medium

#### FR-440: Workspace Symbols / Fuzzy Search (ワークスペースシンボル検索)

- **対象**: `src/lsp/`
- **現状**: ファイル内シンボルのみ検索可能（存在するかも未確認）
- **内容**: LSP `workspace/symbol`ハンドラ。プロジェクト全体の`defun`/`defclass`/`defvar`/`defmacro`をインデックス化。ファジー検索（Levenshtein距離またはfzf-style scoring）。`*function-registry*`を常時更新インデックスとして活用
- **根拠**: `(grep-find "defun my-func")` の代替。シンボル数1000+のプロジェクトでのナビゲーション
- **難易度**: Easy

---

### Phase 91 — オブザーバビリティ

#### FR-442: OpenTelemetry Integration (OpenTelemetry統合)

- **対象**: `src/vm/vm-run.lisp`, `src/cli/main.lisp`
- **現状**: 実行トレース・メトリクス・ログの標準出力形式なし
- **内容**: **OTLP（OpenTelemetry Protocol）**: コンパイルパイプライン各フェーズをspanとして記録（`parse` → `expand` → `cps` → `codegen` → `optimize` → `emit`）。VM関数呼び出しをspanとしてトレース。`--otlp-endpoint=http://localhost:4317`でJaeger/Tempo等に送信。`(cl-cc:with-span "my-operation" ...)` ユーザーAPIも提供
- **根拠**: OpenTelemetry 1.0（2021〜）/ CNCF graduated（2023）。2026年時点で本番可観測性のデファクト標準
- **難易度**: Medium

#### FR-443: eBPF Profiling Integration (eBPFプロファイリング統合)

- **対象**: `src/cli/main.lisp`, `src/emit/binary/`
- **現状**: プロファイリングはVM命令カウンタ（FR-316）のみ。カーネルレベルのサンプリングプロファイラなし
- **内容**: **Linux perf_event + BPF**: `./cl-cc run --ebpf-profile` でLinux eBPFプローブを使ったオーバーヘッドほぼゼロのサンプリング。バイナリの`.debug_frame`（DWARF）を活用してJavaScript/Rustと同様にフレームを解決。bpftrace / bcc連携。macOS Instrumentsへの対応（DTrace）
- **根拠**: perf + DWARF unwinding は2026年のLinux標準プロファイリング手法。プロダクション環境での継続プロファイリングが可能
- **難易度**: Hard

#### FR-444: Flame Graph Generation (フレームグラフ生成)

- **対象**: `src/cli/main.lisp`, FR-316（ベンチマークフレームワーク）
- **現状**: ベンチマーク結果はテキスト/JSON出力のみ。可視化なし
- **内容**: サンプリングプロファイラ出力（FR-443またはVM命令カウンタ）から**Brendan Gregg Flame Graph**形式（SVG）を生成。`./cl-cc run --profile foo.lisp | ./cl-cc flamegraph -o foo.svg`。折り畳み可能なインタラクティブSVG。`(cl-cc:profile-report :format :flamegraph)`
- **根拠**: Flame Graph（Brendan Gregg, 2011〜）は2026年で最も普及したプロファイリング可視化形式。Rustperfなどすべての主要ツールが対応
- **難易度**: Easy

#### FR-445: Hardware Performance Counters (ハードウェア性能カウンタ)

- **対象**: `src/cli/main.lisp`, `src/vm/vm-run.lisp`
- **現状**: ウォールクロック時間のみ計測可能
- **内容**: `perf_event_open`syscall（Linux）/ `kpc_set_config`（macOS）でCPUカウンタ取得。**IPC（Instruction Per Cycle）**: キャッシュミス（L1/L2/L3）数、分岐予測ミス数、TLBミス数をベンチマーク結果に併記。`./cl-cc bench --counters=cache-misses,branch-misses foo.lisp`。数値計算最適化のボトルネック同定に使用
- **根拠**: `perf stat` / Google Benchmark の`--perf_counters` フラグ。「遅いがなぜか」を説明する最直接な情報
- **難易度**: Medium

---

### Phase 92 — インターオペラビリティ

#### FR-447: C Header Generation (Cヘッダ生成)

- **対象**: `src/compile/codegen.lisp`, `src/cli/main.lisp`
- **現状**: Cコードからcl-cc関数を呼び出す方法なし。逆方向（cl-cc→C）はFFI（FR-194）で対応
- **内容**: `./cl-cc compile --emit-header foo.lisp -o foo.h` でエクスポート関数の`.h`自動生成。`(export-to-c foo (fixnum fixnum) fixnum)` アノテーション。関数プロトタイプ・型定義（`typedef struct cl_object* cl_object_t;`）・初期化関数（`cl_cc_init()`）を生成。`extern "C"` ガード付き
- **根拠**: CL関数をCから呼べると既存Cライブラリとの統合が可能。SBCL `sb-alien:alien-funcall` の逆方向
- **難易度**: Medium

#### FR-448: Python/ctypes ABI Compatibility (Python interop)

- **対象**: `src/emit/calling-convention.lisp`, FR-447との連携
- **現状**: Python cftypes / cffi からcl-cc共有ライブラリを呼ぶためのABI保証なし
- **内容**: `--abi=ctypes` フラグで Python `ctypes.CDLL` 互換のC ABIを強制。`int`/`double`/`char*`へのCL型自動変換（boxing/unboxing）。`cffi` パッケージ定義自動生成（FR-447のヘッダから）。`pip install cl-cc-wheel` 形式のwheelパッケージング対応
- **根拠**: データサイエンス/ML系Pythonコードとの統合。数値計算コアをcl-ccで書いてPythonから呼ぶユースケース
- **難易度**: Hard

#### FR-449: JVM Bytecode Target (JVMバイトコードターゲット)

- **対象**: `src/emit/target.lisp`, 新規`src/emit/jvm/`
- **現状**: x86-64/AArch64/RISC-V/WASM32/SPIR-Vのみ
- **内容**: JVM .class ファイル生成（JVM SE 21+ bytecode）。CLオブジェクト→`java.lang.Object`マッピング。CLOSクラス→`java.lang.record`生成。JNIを介した既存Javaライブラリ呼び出し。`Maven/Gradle`依存として配布可能なJARファイル生成
- **根拠**: JVM上のLisp（Clojure）が実証した市場。Android(ART)/GraalVM Native Imageにも応用可能
- **難易度**: Very Hard

---

### Phase 93 — バイナリサイズ・起動時間最適化

#### FR-452: Dead Symbol Stripping (デッドシンボル除去)

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`
- **現状**: バイナリにすべてのコンパイル済み関数が含まれる。未呼び出し関数が最終バイナリに残留
- **内容**: **Section-level DCE**: 各関数を個別セクション（`__TEXT,__text.function_name`）に配置。リンカのDead Section除去（Mach-O `-dead_strip` / ELF `--gc-sections`）を活用。FR-338（全プログラムDCE）と連携して未到達関数セクションを除去
- **根拠**: GCC `-ffunction-sections -Wl,--gc-sections`. 大きなランタイムライブラリで最大70%のバイナリサイズ削減事例あり
- **難易度**: Medium

#### FR-453: Binary Compression / UPX-style Packing (バイナリ圧縮)

- **対象**: `src/cli/main.lisp`
- **現状**: バイナリはアンパックのまま
- **内容**: `./cl-cc compile --compress-binary` で出力バイナリにLZ4/Zstd圧縮+デコンプレッサスタブを付加（UPX方式）。`__TEXT`セグメントのread-only部分のみ圧縮（self-modifying防止）。起動時にページフォールトベースのオンデマンドデコンプレッション。組み込み向けに特に有効
- **根拠**: UPX / elf-packer。フラッシュ容量が限られる組み込みデバイスでのバイナリサイズ削減
- **難易度**: Hard

#### FR-454: Startup Time Optimization (起動時間最適化)

- **対象**: `src/cli/main.lisp`, `src/compile/pipeline.lisp`
- **現状**: `./cl-cc run`起動時にSBCLランタイム + ASDF + 全srcファイルのFASLロードが発生
- **内容**: **Pre-linked stdlib**: 標準ライブラリの機械語を`__DATA_CONST`セグメントにpre-link（FR-364 Image-Based Developmentと連携）。**Lazy loading**: 未使用モジュールの初期化を初回呼び出しまで遅延（`__attribute__((constructor))` + guard変数）。**BOLT PGO layout**（FR-508）でコールドスタートパスを`.text.cold`に隔離
- **根拠**: Clangのstartup 時間最適化。`cl-cc selfhost`の実行時間測定で起動が律速になっている可能性
- **難易度**: Hard

#### FR-455: Segment Merging / Section Layout (セグメント統合・セクションレイアウト)

- **対象**: `src/emit/binary/macho.lisp`
- **現状**: セクション構成が固定（`__TEXT/__text`, `__DATA/__data`等）
- **内容**: 小さなセクション（<4KB）のマージでページ境界パディング削減。Read-Only dataを`__TEXT`セグメントに移動してCOW（Copy-On-Write）共有を最大化。`__LINKEDIT`セグメントの最後配置（macOS要件）の自動調整。`size`コマンドで計測可能なセクション別サイズ報告
- **根拠**: Apple mach-o-linker最適化手法。複数プロセスが同じバイナリを実行する際のメモリ共有率向上
- **難易度**: Medium

---

### Phase 94 — 追加セキュリティ・解析

#### FR-458: Spectre/Meltdown Mitigations (Spectre/Meltdown緩和策)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/jit/`
- **現状**: 投機的実行によるサイドチャネル攻撃への対策なし
- **内容**: **Retpoline**: 間接分岐・間接呼び出しをretpolineシーケンス（`call/pause/lfence/ret`ループ）に変換してBTB毒を無効化。**IBRS/STIBP**: 関数エントリに`IBRS`バリア挿入（`--spectre-mitigation=ibrs`）。**Speculative Load Hardening（SLH）**: 投機的ロードにCMOVガード。JITコード（FR-330〜334）でのretpoline生成
- **根拠**: GCC `-mindirect-branch=thunk` / Clang `-mretpoline`. 2018年Spectre公開以降、全セキュリティ重要コードに必須。JITコードはBTB攻撃面として特に重要
- **難易度**: Hard

#### FR-459: Taint Analysis (テイント解析)

- **対象**: 新規`src/analyze/taint.lisp`
- **現状**: セキュリティ指向のデータフロー解析なし
- **内容**: ユーザー入力（`read-line`/FFI引数/ネットワーク受信）を**taintソース**としてマーク。tainted値が危険なシンク（`eval`/`shell`/SQLクエリ生成）に到達するパスを**コンパイル時に検出**。`(cl-cc:sanitize-taint x :type :sql)` でtaintを解除。静的テイント伝播（Andersen解析ベース）と動的テイントトラッキングの2モード
- **根拠**: Perl `-T` taint mode / Ruby $SAFE / OWASP Top 10。コードインジェクション・SQLインジェクション等の脆弱性の早期検出
- **難易度**: Hard

#### FR-460: Information Flow Control / IFC (情報フロー制御)

- **対象**: `src/type/inference.lisp`, `src/analyze/`
- **現状**: セキュリティラベルなし。高機密値から低機密チャネルへの情報漏洩を検出不可
- **内容**: 型に**機密性ラベル** `(High)` / `(Low)` を付与。`High`ラベル付き値が`Low`チャネル（ログ出力・ネットワーク送信）に流れる場合にコンパイルエラー。**Declassification**: `(cl-cc:declassify val)` で意図的な降格を明示。SIF（Simple Information Flow）/ FlowCaml / IFDS algorithm
- **根拠**: Jif（Java Information Flow）/ FlowCaml。暗号キー・個人情報の意図しない漏洩をコンパイル時に証明する
- **難易度**: Very Hard

#### FR-461: Integer Overflow Detection (整数オーバーフロー検出)

- **対象**: `src/vm/primitives.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: 算術演算のオーバーフロー検出なし。fixnumオーバーフロー時の動作未定義
- **内容**: `--sanitize=integer`モードで各算術命令にOF（Overflow Flag）チェックを付加。`jo <abort>`命令でオーバーフロー時にトラップ。`(declare (cl-cc:no-overflow))` で検証省略指示。コンパイル時のVRP（FR-390）で到達不能なオーバーフローチェックを除去（zero overhead）
- **根拠**: GCC/Clang `-fsanitize=signed-integer-overflow`. CLのfixnumはタグビット使用でオーバーフロー境界がMOST-POSITIVE-FIXNUMとなり通常の整数と異なる
- **難易度**: Medium

#### FR-462: Symbolic Execution Engine (シンボリック実行エンジン)

- **対象**: 新規`src/analyze/symbolic-exec.lisp`
- **現状**: テストはconcrete実行のみ。入力の制約推論なし
- **内容**: 変数を**シンボル値**として実行し、分岐条件を制約として収集（パス条件）。Z3（FR-392）で充足可能性を確認。到達可能パスごとの入力例を自動生成（テスト自動生成）。バッファ境界・nullポインタ・ゼロ除算への到達可能性を証明。KLEE / angr / symCC相当
- **根拠**: CMU CERT / DARPA CGC。テストケース自動生成・脆弱性探索・不変条件証明への応用が広い。CLの純粋関数はシンボリック実行に特に適している
- **難易度**: Very Hard

---

### Phase 96 — 高度最適化パス III

#### FR-465: Zero-Cost Exception Handling (テーブルベースゼロコスト例外処理)

- **対象**: `src/vm/conditions.lisp`, `src/emit/x86-64-codegen.lisp`, `src/emit/binary/macho.lisp`
- **現状**: `handler-case`/`condition`はVM命令レベルでの動的フレーム積み上げ方式。例外を投げない正常パスにもオーバーヘッドあり
- **内容**: **LSDA（Language Specific Data Area）**テーブルをバイナリの`__TEXT,__gcc_except_tab`セクションに生成。各`handler-case`/`restart-case`の範囲とハンドラアドレスをテーブル化。Itanium ABI `_Unwind_RaiseException` + libunwindを呼び出す例外ディスパッチ。正常パス（conditionを投げない場合）のコストをゼロに。Windows SEH（Structured Exception Handling）形式にも対応
- **根拠**: C++ `try/catch` の標準実装（GCC/Clang）。正常パスのEHコストは文字通りゼロになりcondition systemの普及を妨げなくなる
- **難易度**: Very Hard

#### FR-466: Jump Threading (ジャンプスレッディング)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 条件分岐先がさらに無条件分岐・別の条件分岐になっているパスの短絡なし
- **内容**: **パス条件の伝播**: 条件分岐のthen/elseパスでどちらの値が確定しているかを記録。次の条件分岐の条件が既知の場合、中間ブロックをバイパスして直接ジャンプ先にスレッド（経路複製 + DCEで実現）。`(if a (if b ...) ...)` のネストをフラット化。GCC `-fthread-jumps` / LLVM JumpThreading pass
- **根拠**: 条件分岐の約20〜30%はジャンプスレッディングで除去可能。分岐予測ミスも減少
- **難易度**: Medium

#### FR-467: Tail Merging / Code Deduplication (末尾コード統合)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 複数の分岐末尾に同一命令列が重複している場合も個別にコンパイル
- **内容**: 複数のCFGブロック末尾で同一命令列を検出し、**共通末尾ブロック**に統合してジャンプを一箇所に収束。逆向きのCSEとして実装（後方スキャン）。バイナリサイズ削減 + I-キャッシュ局所性向上。GCC `-freorder-blocks` / LLVM MergeFunctions pass
- **根拠**: SBCL/LispWorksのコンパイラでも実施。コード重複はコンパイラが生成するswitch/caseパターンで特に多い
- **難易度**: Medium

#### FR-468: Bounds Check Elimination / BCE (配列境界チェック除去)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: ベクタアクセス（`vm-aref`）は毎回境界チェックを発行。VRPとの連携なし
- **内容**: VRP（FR-390）の値域情報を活用。`i`の区間が`[0, n-1]`と確定していれば`(aref arr i)`の境界チェックを除去。ループ事前チェック（loop pre-check）: ループ前に一度だけ境界検証を行い、ループ内チェックを全削除。`(declare (type (simple-array fixnum) arr))` + `(declare (type fixnum i))` との相乗効果
- **根拠**: JVM JIT BCE / .NET JIT BCE。配列集約ループで10〜20%高速化。数値計算の主要ボトルネック除去
- **難易度**: Hard

#### FR-469: Global Dead Store Elimination (大域デッドストア除去)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: DCEパス（`opt-pass-dead-code`）は読まれない値の計算を除去。書かれた後に読まれないストアの除去なし
- **内容**: MemorySSA（FR-409）ベース: 後続するメモリ使用のないMemoryDefを「デッドストア」として除去。エイリアス解析（FR-340）でストア先が独立していると証明できる場合のみ除去（安全性保証）。`(setf (slot-value obj :x) val)` が以降読まれない場合に削除
- **根拠**: LLVM DeadStoreElimination pass。メモリ書き込み集約コードでの効果大。MemSSAなしでは安全な実装が困難
- **難易度**: Hard

#### FR-470: Shrink-Wrapping (プロローグ/エピローグ縮小配置)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: カリー保存レジスタの保存/復元は関数エントリ/リターンに固定配置
- **内容**: 実際にそのレジスタを使うコードパスの直前にプロローグを、使い終わった直後にエピローグを移動。例外パス（cold path）では callee-save registers を保存しないで早期リターン。支配木解析でセーブ/リストア挿入点を最適化。LLVM ShrinkWrap / GCC shrink-wrapping
- **根拠**: 早期リターンパスが多い関数（エラーチェック多数）でプロローグコストをゼロに。callee-save保存は約4命令/レジスタ
- **難易度**: Hard

#### FR-471: Function Outlining / Cold Path Extraction (関数アウトライン化)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: インライン化の逆（コード抽出）なし。コールドパスがホットパスと同じ関数内に同居
- **内容**: 実行頻度の低いコードブロック（PGOデータ or 静的ヒューリスティック: errorハンドラ・フォールバック処理）を**新たな関数**として抽出。抽出された関数を`.text.cold`セクションに配置（FR-508との連携）。ホット関数のI-キャッシュフットプリントを削減。GCC `-freorder-functions` / LLVM機能
- **根拠**: Googleの研究でShrink-wrapping + Outliningの組み合わせがL1 I-cache miss率を15〜25%削減
- **難易度**: Medium

#### FR-472: Partial Inlining (部分インライン化)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: インライン化は関数全体を複製するか全くしないかの2択
- **内容**: 関数の**ホットパスのみ**を呼び出しサイトにインライン化し、コールドパスへの分岐を元の関数エントリへのテール呼び出しに変換。例: バリデーション失敗パスを呼び元に残し成功パスのみインライン。インライン閾値超えの大関数への適用。LLVM PartialInlining pass
- **根拠**: 大きな関数（VTABLEディスパッチ等）を完全インライン化するとコードサイズ爆発。部分インライン化はサイズとパフォーマンスのトレードオフを最適化
- **難易度**: Hard

#### FR-473: Loop Rotation & Peeling (ループ回転・ピーリング)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: ループは記述通りの形で生成。ループ入口でのヘッダチェックが毎回発生
- **内容**: **Loop Rotation**: `while (cond) { body }` を `if (cond) { do { body } while (cond) }` に変換。ループボディ内でのバックエッジを末尾に配置し分岐予測を改善。**Loop Peeling**: 最初の1〜N回のイテレーションをループ前に展開（境界特殊ケースの最適化）。LLVM LoopRotate + LoopPeel
- **根拠**: Loop rotationはLICM（FR-400）・ベクトル化（FR-345）の前提変換。ループ最適化パイプラインの最初のステップ
- **難易度**: Medium

#### FR-474: Critical Edge Splitting (クリティカルエッジ分割)

- **対象**: `src/optimize/optimizer.lisp`, FR-147（SSA構築）
- **現状**: SSA phiノードにコピー挿入が必要なクリティカルエッジ（複数出口→複数入口）の処理が不明確
- **内容**: **クリティカルエッジ**（複数後任を持つブロック→複数前任を持つブロック）に空のダミーブロックを挿入。SSA破壊（phi除去）・loop rotation・コードモーションの正確性確保。全最適化パスの前処理として実施
- **根拠**: SSAに基づく最適化パスの正確性の基盤。GCC / LLVMのSSAパイプラインの必須ステップ
- **難易度**: Medium

---

### Phase 97 — コード生成・バックエンド拡張

#### FR-476: LLVM IR Emission Backend (LLVM IRバックエンド)

- **対象**: 新規`src/emit/llvm/`, `src/emit/target.lisp`
- **現状**: ネイティブコード生成は独自x86-64/AArch64バックエンドのみ
- **内容**: cl-cc IR→LLVM IR（`.ll`テキスト / bitcode `.bc`）への変換。`llvm-as`/`llc`/`opt`ツールチェーンへの橋渡し。LLVMの全最適化パス（auto-vectorization・polly等）を活用可能に。`--emit=llvm-ir`フラグ。`wasm-ld`によるWASMリンク。LLVM IRはcl-ccにとって**第5のターゲット**として機能
- **根拠**: cl-ccのフロントエンド品質を活かしつつバックエンドはLLVMに委譲できる。新アーキテクチャ対応がLLVM経由で無償に得られる
- **難易度**: Hard

#### FR-477: CPU Feature Dispatch / Function Multiversioning (CPU機能ディスパッチ)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/compile/codegen.lisp`
- **現状**: コンパイル時に単一のISAレベルを想定。実行時CPUのSSE4.2/AVX2/AVX-512活用なし
- **内容**: `(cl-cc:multiversion my-fn (:avx2 #'my-fn-avx2) (:sse4.2 #'my-fn-sse42) (:baseline #'my-fn-base))` マクロ。**CPUID検出**: バイナリ起動時に`cpuid`命令で対応ISAを検査し関数ポインタテーブルを初期化。GCC `__attribute__((target_clones("avx2","sse4.2","default")))` / Clang `__attribute__((target_clones))` 相当
- **根拠**: 同一バイナリでSSE2必須機と最新AVX-512機の両方で最適性能を発揮。数値計算ライブラリの標準手法
- **難易度**: Hard

#### FR-478: Tree-Based Instruction Selection / BURS (木リライト命令選択)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: VM命令→x86-64命令は1対1の手書きマッピング（`*x86-64-emitter-entries*`alist 45エントリ）
- **内容**: IR式ツリーに対してBottom-Up Rewrite System（BURS）で**最小コスト命令列**を選択。`vm-add(vm-mul(a,b),c)` → `lea rax,[rbx*c+rdx]`のような複合命令への畳み込み。パターン: コスト付き書き換えルール `(add (mul r c)) → lea [r*c]`。BURG / IBURG ツール相当の手実装
- **根拠**: LLVMのDAGISel / GCCのRTL最適化。1対1マッピングより明らかに優れた命令列を生成できる（特に LEA活用、FMA命令統合）
- **難易度**: Hard

#### FR-479: Intrinsic Function Registry (イントリンシック関数レジストリ)

- **対象**: `src/compile/builtin-registry.lisp`, `src/compile/codegen.lisp`
- **現状**: `builtin-registry.lisp`の~210エントリはVM命令への直接マッピング。コンパイラ特殊インライン（例: `(length "abc")` → 定数3）なし
- **内容**: **コンパイラ組み込み関数**: `(cl-cc:define-intrinsic length (string) fixnum ...)` でコンパイル時に特殊処理される関数を登録。`(length "abc")` → `vm-const 3`、`(car (cons a b))` → `vm-move a`（consアロケーション除去）、`(not (not x))` → `x`。`(abs x)` → `vm-abs`命令（x86-64 `andpd`）。FR-334（PIC）でホットパスに適用
- **根拠**: SBCL compiler transforms / GCC builtin_expect. 標準ライブラリの重要関数を既知のパターンとして特殊化
- **難易度**: Medium

#### FR-480: Sample-Based PGO / AutoFDO (サンプルベースPGO)

- **対象**: `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: FR-508（BOLT）は後処理バイナリ最適化。フロントエンドへのプロファイルフィードバックなし
- **内容**: `Linux perf record` の出力（`perf.data`）からコンパイラが利用できるプロファイルデータへの変換。**AutoFDO**: ソースコード行番号→サンプルカウントのマッピング（FDO profile format）。インライン閾値・ループ展開係数・アウトライン判定をプロファイルで調整。インストルメンテーション不要でプロダクション実行データを活用。Google AutoFDO / GCC `-fauto-profile`
- **根拠**: Instrumentation PGOと異なりプロダクション環境での計測が可能。Google社内でChromeの5〜10%速度向上を達成
- **難易度**: Hard

#### FR-481: Vectorized Reduction Optimization (リダクションループ最適化)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: sum/max/min等のリダクションループはスカラ逐次実行
- **内容**: **自動リダクション認識**: `(loop for x in arr sum x)` → SIMD並列部分和生成 + 最終集約。**水平加算** (`_mm256_hadd_ps`) / **水平最大** (`_mm256_max_ps`) のSIMD命令生成。コンパイラによるloop accumulatorの依存性解析でリオーダー可能と証明。`(declare (cl-cc:reduction sum))` ヒント
- **根拠**: GCC/Clangのリダクション自動ベクトル化。数値集約の典型パターンで4〜8x高速化
- **難易度**: Hard

---

### Phase 98 — 診断・コンパイラ品質向上

#### FR-484: "Did You Mean?" Suggestion Engine (候補提示エラーエンジン)

- **対象**: `src/parse/diagnostics.lisp`, `src/compile/codegen.lisp`
- **現状**: 未定義変数/関数のエラーメッセージはシンボル名のみ。類似シンボル候補なし
- **内容**: **編集距離（Levenshtein）ベース候補検索**: `*function-registry*`・`*class-registry*`・ローカル変数スコープから類似名を最大3候補提示。スコアリング（先頭文字一致ボーナス）。`"undefined function: LENGHT; did you mean LENGTH, LENGTHY?"` スタイルの出力。Rust/Clangの`did you mean`機能の実装
- **根拠**: SBCL のtypoエラーメッセージは現在未定義の名前をそのまま表示するだけ。Rustが採用して以来コンパイラのデファクト機能に
- **難易度**: Easy

#### FR-485: Warnings as Errors / -Werror (警告をエラーに昇格)

- **対象**: `src/parse/diagnostics.lisp`, `src/cli/main.lisp`
- **現状**: 警告（`:warning` severity）と診断は分離されているが、ビルド失敗への変換機構なし
- **内容**: `--Werror` フラグで全警告をエラーに昇格（ビルド失敗）。`--Werror=unused-variable` で特定カテゴリのみ昇格。`--Wno-error=deprecated` で特定カテゴリをエラー昇格から除外。CI環境での「警告ゼロポリシー」強制。FR-318（警告システム）・FR-317（構造化診断）との統合
- **根拠**: GCC/Clang `-Werror`. CI/CDでの品質ゲートとして標準的。cl-ccのテストスイート自体にも適用すると品質向上
- **難易度**: Easy

#### FR-486: Diagnostic Categories & Filtering (診断カテゴリ・フィルタリング)

- **対象**: `src/parse/diagnostics.lisp`, `src/cli/main.lisp`
- **現状**: 警告の種類を個別に有効/無効化できない
- **内容**: 診断カテゴリ定義（`-Wunused-variable`/`-Wtype-mismatch`/`-Wshadowing`/`-Wdeprecated`等）。`--Wall`（全警告有効）/`--Wextra`（追加警告）/`--Wno-<category>`（個別無効化）。`diagnostic`構造体に`category`フィールド追加。ソース行コメント `;;; cl-cc:ignore-warning unused-variable` で行単位抑制。`clang-tidy` / `cargo clippy` スタイルの設定ファイル（`.cl-cc-lint.toml`）
- **根拠**: GCC `-W` フラグ体系。ユーザーが自分のコードに適した警告レベルを設定できる
- **難易度**: Medium

#### FR-487: Optimization Remarks (最適化説明レポート)

- **対象**: `src/optimize/optimizer.lisp`, `src/cli/main.lisp`
- **現状**: 最適化が適用されたか失敗したかのフィードバックなし
- **内容**: `--Rpass=inline` でインライン化の成否を報告（「function foo inlined at bar.lisp:42」「not inlined: too large (20 > 15)」）。`--Rpass-missed=vectorize` でベクトル化できなかった理由を報告（「loop has dependency on x」）。YAML/JSON形式での出力（CI統合用）。最適化レベル変更・`declare`追加のガイダンスを含む。Clang `-Rpass` / LLVM OptimizationRemarkEmitter相当
- **根拠**: 「なぜこのコードが遅いのか」「どう書き直せば最適化されるか」をコンパイラが直接教えてくれる。Rust `-Copt-level` + `cargo-llvm-lines`の中間
- **難易度**: Medium

#### FR-488: 3-Stage Bootstrap Verification (3段階ブートストラップ検証)

- **対象**: `src/cli/main.lisp`, `tests/`, `Makefile`
- **現状**: `./cl-cc selfhost`は1段階の自己ロード検証（9チェック）。コンパイラ出力の一致検証なし
- **内容**: **Stage1**: ホストSBCLでcl-ccをビルド → バイナリA。**Stage2**: バイナリAでcl-ccをコンパイル → バイナリB。**Stage3**: バイナリBでcl-ccをコンパイル → バイナリC。**検証**: B == C（バイナリ一致）。差分があればコンパイラのバグ。`make bootstrap`ターゲット。GCC 3-stage bootstrap / Rust `./x.py test --stage 2` と同等の正しさ保証
- **根拠**: セルフホスティングコンパイラの金標準テスト。バイナリB≠Cはコンパイラが自分自身を正確にコンパイルできていない証拠
- **難易度**: Medium

#### FR-489: Differential Testing vs SBCL (SBCL差分テスト)

- **対象**: `tests/integration/`, `tests/framework/`
- **現状**: テストはcl-cc VMの出力のみ検証。SBCLとの結果比較なし
- **内容**: `(deftest-differential (expr) ...)` マクロ: 同一式をSBCL `eval`とcl-cc VMの両方で実行し結果を比較。ランダム入力生成（FR-353 PBT）と組み合わせたファジング。差分発見時に最小化（shrinking）。コンパイラバグの**自動検出**。CSmith / CompCert differential testing手法
- **根拠**: ANSI CL準拠の自動検証。cl-ccとSBCLで結果が違えばcl-ccのバグ。4322テストをdifferential化すると準拠性が定量評価できる
- **難易度**: Medium

#### FR-490: Function Attribute Inference (関数属性自動推論)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: `pure`（副作用なし・グローバル不参照）/`const`（引数のみに依存）/`noreturn`の自動推論なし。手動`declaim`のみ
- **内容**: コールグラフ解析で自動推論: **noreturn**: 全パスが`error`/`abort`で終わる関数。**pure**: 副作用なし・グローバル読み取りのみ（FR-152との連携）。**const**: 引数以外への依存なし（MemSSAでメモリ操作なしと証明）。推論結果を`*function-attr-table*`に登録してCSE/DCE/LICM/並列化の対象判定に活用
- **根拠**: GCC `-O2`でも`__attribute__((pure))`の自動推論は行われない（手動のみ）。LLVMは部分的に自動推論。cl-ccの純粋性推論（FR-152）をこれで完成させる
- **難易度**: Medium

#### FR-491: Macro Hygiene Checker (マクロ衛生チェッカー)

- **対象**: `src/expand/expander.lisp`, `src/expand/macro.lisp`
- **現状**: マクロ展開時の変数キャプチャを検出する機構なし
- **内容**: マクロ展開時に導入されるバインディング名が、マクロ呼び出しスコープの変数名と衝突しているかをチェック。`(gensym)`を使用していないマクロ本体内の変数バインディングを警告。**Hygienic macros自動変換**: 非衛生的マクロをgensymベースに自動変換するリファクタリング提案。`--Whygienic`フラグ
- **根拠**: SchemeのR5RS衛生マクロ / Racket `syntax-rules`。CLのdefmacroは非衛生的だが意図しないキャプチャは重大なバグ源
- **難易度**: Medium

---

### Phase 99 — 数値・文字列・型特化最適化

#### FR-494: Numeric Tower Optimization (数値タワー最適化)

- **対象**: `src/vm/primitives.lisp`, `src/compile/codegen.lisp`
- **現状**: bignum/rational/complexの演算は汎用パス。fixnum特化のみ一部実装
- **内容**: **bignum最適化**: 小さいbignumをスタック割り当て（escape解析連携）。`(* big1 big2)` のKaratsuba乗算（閾値以上）。`(expt 2 n)` → ビットシフト。**rational**: `(+ 1/3 1/6)` のコンパイル時評価。**complex**: `(abs #C(3.0 4.0))` → `5.0`定数畳み込み。`(realpart #C(x y))` → `x`（一段目の素早い展開）
- **根拠**: CLの数値タワーはANSI仕様だが多くの処理系がfixnum以外を遅くしている。科学技術計算への適用に必須
- **難易度**: Medium

#### FR-495: String Interning & Small String Optimization (文字列インターン・SSO)

- **対象**: `src/vm/strings.lisp`, `src/runtime/heap.lisp`
- **現状**: 文字列は毎回新規ヒープオブジェクト割り当て。同一内容の文字列が複数存在
- **内容**: **Small String Optimization (SSO)**: 15バイト以下の文字列をヒープポインタなしでオブジェクトヘッダ内にインライン格納。**文字列インターン**: `(intern-string "key")` でグローバルプール管理、`eq`比較可能に。**コンパイル時文字列定数プール**: 同一定数文字列を`rodata`セクションで共有（FR-207 Reproducible buildsと連携）
- **根拠**: C++ `std::string` SSO（libstdc++/libc++とも採用）/ Java String.intern(). ハッシュテーブルキーとして文字列を多用するコードでヒープ割り当て大幅削減
- **難易度**: Medium

#### FR-496: Auto-Differentiation (自動微分)

- **対象**: 新規`src/autodiff/`, `src/compile/codegen.lisp`
- **現状**: 微分演算のコンパイルサポートなし
- **内容**: **Forward mode AD**: `(cl-cc:with-dual-numbers ...)` で二重数（値+微分値）を使った前進自動微分。関数に対して`(gradient f x)` が導関数値を返す。**Reverse mode AD (backprop)**: 計算グラフのテープ記録による逆伝播。`(cl-cc:gradient-tape (tape) ... (gradient tape f x))` API。`(declare (cl-cc:differentiable))` で対象関数を指定
- **根拠**: JAX / Julia Zygote / Enzyme LLVM pass。機械学習・数値最適化・物理シミュレーションへの応用。CLの数値計算能力と組み合わせると強力
- **難易度**: Hard

#### FR-497: Polyhedral Loop Optimization (多面体ループ最適化)

- **対象**: `src/optimize/optimizer.lisp`, FR-403（Loop Tiling）の拡張
- **現状**: Loop Tiling（FR-403）は単純なキャッシュ局所性改善。複雑な多重ループ変換なし
- **内容**: **多面体モデル**: アフィンループ（インデックスが線形式のループ）をPolytope（多面体）で表現。合法変換（Loop Fusion/Fission/Permutation/Skewing/Reversal）の自動適用。依存グラフに基づく合法性検証。Pluto / LLVM Polly相当の実装。`(declare (cl-cc:polyhedral))` でアフィン性アノテーション
- **根拠**: 行列演算・畳み込み・FFTで劇的な最適化（10x以上）。FR-403のタイル化を多面体モデルで理論的に正確に実装
- **難易度**: Very Hard

#### FR-498: Global Constant Propagation (大域定数伝播)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: 定数伝播はローカル変数のみ。一度だけ代入されるグローバル変数（`defparameter`）の値は伝播されない
- **内容**: **compile-time invariant globals**: `(defconstant +pi+ 3.14159...)` は全参照を定数に置換（現在も一部実施）。`(defparameter *max-size* 1024)` が変更されない場合のinter-moduleDCE。`(defvar *debug-mode* nil)` が常にnilなら関連コードをDCE。修正検出はCFG上のall-paths解析
- **根拠**: CLの`defconstant`はコンパイラに変更なしを保証する。`defparameter`の不変性推論で標準ライブラリの大量の設定変数を定数化できる
- **難易度**: Medium

#### FR-499: Precompiled Modules / PCH (プリコンパイルモジュール)

- **対象**: `src/compile/pipeline.lisp`, `cl-cc.asd`
- **現状**: FASLキャッシュ（FR-151）はSBCLのもの。cl-ccのコンパイル済みIR（VM program）のヘッダプリコンパイルなし
- **内容**: **PCH（Precompiled Header）形式**: よく使われるインポート群（`cl-cc:`, `cl:`の共通部分）を事前にコンパイル・バイナリにシリアライズ。`#include <stdcl.pcl>` 的な自動検出。ASDF `:defsystem-depends-on` との統合。コンパイル時の重複parse/expand工程をスキップ。clang PCH / GCC PCH相当
- **根拠**: cl-cc標準ライブラリ（`src/vm/*.lisp`全体）の毎回再parseが省略でき、セルフホスト時間を大幅削減
- **難易度**: Medium

#### FR-500: Whole-Program Type Inference (全プログラム型推論)

- **対象**: `src/type/inference.lisp`, `src/compile/pipeline.lisp`
- **現状**: 型推論はファイル単位。`src/compile/cps.lisp`が`src/compile/codegen.lisp`の関数の型を知らない
- **内容**: **Module-level type inference**: コールグラフ解析で呼び出し先の型シグネチャを呼び出し元に伝播。`(defun foo (x) (bar x))` でfoo引数の型はbarの引数型制約から推論。LTO（FR-335）と連携したリンク時型推論。Hindley-Milner + let-polymorphism のモジュール間拡張。OCaml cross-module inlining + 型推論と同等
- **根拠**: 全プログラム型推論で単相化（FR-417）・devirt（FR-337）・BCE（FR-468）の精度が大幅向上
- **難易度**: Very Hard

#### FR-501: Loop Auto-Parallelization (ループ自動並列化)

- **対象**: `src/optimize/optimizer.lisp`, FR-387（グリーンスレッド）との連携
- **現状**: ループは常にシングルスレッド実行
- **内容**: データ依存解析（FR-342ポインタ解析）でループ反復間の依存がないと証明された場合に**並列ループに変換**。`(cl-cc:parallel-for i 0 n ...)` のランタイムディスパッチ生成。スレッドプール（FR-387）にタスク分割。並列オーバーヘッドが利益を上回る場合は直列化（閾値: トリップカウント * body命令数）。OpenMP `#pragma omp parallel for` の自動版
- **根拠**: GCC `-ftree-parallelize-loops` / Intel OpenMP auto-parallelization. 大規模配列処理で線形スケール達成
- **難易度**: Very Hard

---

### Phase 100 — ループ解析・高度変換 II

#### FR-510: Scalar Evolution Analysis / SCEV (スカラ進化解析)

- **対象**: 新規`src/optimize/scev.lisp`, FR-147（SSA）前提
- **現状**: ループ変数が何回目の反復で何の値を持つかの代数的記述なし。LICM/BCE/ベクトル化が保守的推論に頼っている
- **内容**: ループ変数を**閉形式の加算連鎖**（Additive Recurrence: `{i₀, +, step}`）として解析。`(loop for i from 0 below n)` → `i = {0, +, 1}₍ₗₒₒₚ₎`、トリップカウント `= n`。`(* i 4)` → `{0, +, 4}`。多段ネストにも対応（Chrec: Chain of Recurrences）。LICM・BCE（FR-468）・ループ展開（FR-401）・ストレングスリダクション（FR-407）の精度を大幅向上
- **根拠**: LLVM ScalarEvolution / GCC SCEV。ループ最適化パイプラインの中核解析。これなしでBCEとLICMは保守的になる
- **難易度**: Hard

#### FR-511: Loop Interchange (ループ交換)

- **対象**: `src/optimize/optimizer.lisp`, FR-510（SCEV）前提
- **現状**: ネストループのイテレーション順序は変更不可
- **内容**: 多重ネストループの内外ループ順序を交換。外側ループが列方向・内側ループが行方向の場合（行優先メモリ上）に交換してキャッシュ局所性改善。**合法性**: 依存グラフのLevel Vector解析で交換が安全と証明された場合のみ実施。`(loop for i ... for j ...)` のネスト順最適化
- **根拠**: GCC loop-interchange / Polly。行列演算のキャッシュミス率を数十分の一に削減。Loop Tiling（FR-403）の前処理として適用
- **難易度**: Hard

#### FR-512: Loop Unswitching (ループアンスイッチング)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: ループ内の不変条件分岐を外に出す変換なし（LICMは命令移動のみ、条件分岐は対象外）
- **内容**: `(loop ... (if invariant-cond then else) ...)` を `(if invariant-cond (loop ... then ...) (loop ... else ...))` に変換。ループを条件分岐の外側に**2つ複製**。ループ内の分岐予測ミスを完全排除。コードサイズと速度のトレードオフ（`-O2`でのみ適用）。LLVM LoopUnswitch / GCC `-funswitch-loops`
- **根拠**: `(loop for x in list (if debug (log x)) (process x))` 形式が多数。デバッグフラグ検査など不変条件がループ内に多い
- **難易度**: Medium

#### FR-513: Software Pipelining / Modulo Scheduling (ソフトウェアパイプライン)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: 命令スケジューリング（FR-408）は基本ブロック内のみ。ループ反復間のオーバーラップなし
- **内容**: **Modulo Scheduling**: ループ本体をInitiation Interval（II）で割り当て、前の反復の命令と次の反復の命令を**オーバーラップ**実行。`prologue + kernel + epilogue`に展開。FP演算（レイテンシ4〜8クロック）のスループットを最大化。メモリアクセスのパイプライン化（ロードの投機的先行実行）。GCC `-fmodulo-sched` / Itanium EPIC
- **根拠**: 高レイテンシ命令が多い数値計算ループでスループット2〜4x向上。FMA（Fused Multiply-Add）をフルに活用できる
- **難易度**: Very Hard

#### FR-514: Software Prefetch Insertion (ソフトウェアプリフェッチ挿入)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: メモリアクセスパターンの事前予測なし。キャッシュミスは実アクセス時まで隠蔽されない
- **内容**: ループ内で**N反復先**のメモリアドレスを計算し`prefetcht0`/`prfm`命令を挿入（ストリーミングアクセスパターンを検出）。SCEV（FR-510）でアクセスパターンがアフィンと判明した場合に適用。プリフェッチ距離はL2/L3レイテンシから自動計算。`(declare (cl-cc:prefetch-distance 8))` 手動ヒント。GCC `-fprefetch-loop-arrays`
- **根拠**: キャッシュミスは100〜300クロック。プリフェッチでストリーミングアクセスを完全隠蔽できる
- **難易度**: Hard

#### FR-515: Load-Store Forwarding & Elimination (ロードストア転送・除去)

- **対象**: `src/optimize/optimizer.lisp`, FR-409（Memory SSA）前提
- **現状**: メモリ書き込み直後の同アドレス読み取りに余分なロード命令が残る
- **内容**: **Store-to-Load Forwarding**: `(setf (slot-value obj :x) v)` の直後の `(slot-value obj :x)` を `v` に置換（ロード除去）。**Redundant Load Elimination**: 同じアドレスへの2回目のロードをMemorySSAで1回目の結果で置換（MemSSAのuse-def連鎖を辿る）。x86-64のAGU（Address Generation Unit）ストールも軽減
- **根拠**: CLOSスロットアクセスのget/setが連続するパターンで頻発。MemorySSAなしでは安全な実装が困難（FR-409の活用先）
- **難易度**: Medium

#### FR-516: AoS to SoA Transformation (配列構造体→構造体配列変換)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: CLOSオブジェクトの配列（AoS: Array of Structures）は各オブジェクトが散在したメモリレイアウト
- **内容**: `(make-array n :initial-element (make-instance 'point))` パターンを検出し、**SoA（Structure of Arrays）**に変換：`:x`フィールドの配列 + `:y`フィールドの配列。SIMD操作がSoAで最大効率（隣接x値のvpacked-add）。`(declare (cl-cc:soa point))` アノテーションで明示指定も可
- **根拠**: Cゲームエンジン（Unity DOTS / Unreal ECS）がSoAに全面移行。AutoVec（FR-345）の効果を最大化する前提変換
- **難易度**: Hard

#### FR-517: Gather/Scatter Vectorization (ギャザー/スキャタベクトル化)

- **対象**: `src/emit/x86-64-codegen.lisp`, FR-345（自動ベクトル化）の拡張
- **現状**: ベクトル化は連続メモリアクセスのみ対応
- **内容**: **非連続アクセス**のSIMDベクトル化：`(aref arr index-vec)` → `vpgatherdq`（AVX2）/ `vpgatherqq`（AVX-512）命令生成。インデックスベクタが既知の場合のシャッフル最適化（`vpermq`）。SpMV（疎行列ベクトル積）への適用。LLVM VectorCombine gather/scatter support
- **根拠**: AVX2（2013〜）がgather命令を導入。疎データ構造・間接参照パターンのSIMD化に必須
- **難易度**: Hard

#### FR-518: Global Code Motion / GCM (大域コード移動)

- **対象**: `src/optimize/optimizer.lisp`, FR-404（GVN）+ FR-147（SSA）前提
- **現状**: LICM（FR-400）はループ不変式の移動のみ。CFG全体を通じた最適配置なし
- **内容**: **Cliff Click's GCM**: 各命令の**最早配置**（dominance frontier上方）と**最遅配置**（使用箇所直前）を計算し、ループ深度が最小の位置に配置。LICMより汎用（ループ外コードにも適用）。GVNと組み合わせることで冗長計算の最適配置を実現。LLVM GVNHoist / Click's 1995 GCM algorithm
- **根拠**: LICMの一般化。CFG上の任意の「不必要に内側にある計算」を移動できる
- **難易度**: Hard

---

### Phase 101 — 並行性プリミティブ・メモリモデル

#### FR-520: Atomic Operations (アトミック操作)

- **対象**: 新規`src/concurrent/atomic.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: CAS操作・フェッチアンドアドなしで、ロックフリーアルゴリズムが実装不可能
- **内容**: `(cl-cc:atomic-compare-and-swap place expected new)` → `cmpxchg`命令。`(cl-cc:atomic-fetch-add place delta)` → `lock xadd`命令。`(cl-cc:atomic-load place :order :acquire)` / `(cl-cc:atomic-store place value :order :release)`。メモリオーダリング: `:relaxed`/`:acquire`/`:release`/`:acq-rel`/`:seq-cst`（C++20 `std::atomic` 相当）。AArch64は`ldadd`/`stlr`命令
- **根拠**: ロックフリーGC（FR-420）、JIT コードキャッシュ更新（FR-330）、関数レジストリ更新（FR-363）の基盤。アトミックなしで並行性は実現不可
- **難易度**: Medium

#### FR-521: Memory Barriers & Fences (メモリバリア・フェンス)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: 命令リオーダーに関するバリア命令なし
- **内容**: `(cl-cc:memory-fence :type :seq-cst)` → `mfence`（x86-64）/ `dmb ish`（AArch64）。`(cl-cc:store-fence)` → `sfence` / `dmb ishst`。`(cl-cc:load-fence)` → `lfence` / `dmb ishld`。**コンパイラバリア**（リオーダー禁止のみ、ハードウェアフェンスなし）の区別。C++ `std::atomic_thread_fence` 相当。JITコード同期（`icache flush` + IMB）も含む
- **根拠**: x86-64は比較的強いメモリモデルだがAArch64はweakモデル。マルチコア上でのデータ競合を防ぐためにバリアが必要
- **難易度**: Medium

#### FR-522: Lock-Free Data Structures (ロックフリーデータ構造)

- **対象**: 新規`src/concurrent/lockfree.lisp`, FR-520（アトミック）前提
- **現状**: `*function-registry*` 等のグローバルハッシュテーブルはロックなしで共有不可
- **内容**: **Michael-Scott ロックフリーキュー**: 並列コンパイル（FR-376）のタスクキュー。**Harris-Michael ロックフリーリスト**: シンボルテーブルの並列更新。**Split-Ordered Hash Table**: ロックフリーハッシュテーブル（`*function-registry*`の並列化）。**Epoch-based Reclamation（EBR）**: ハザードポインタによる安全なメモリ解放（GCとの協調）
- **根拠**: ロック使用時の並列コンパイル（FR-376）のスケーラビリティボトルネックを除去。理論上コア数に線形スケール
- **難易度**: Very Hard

#### FR-523: Software Transactional Memory / STM (ソフトウェアトランザクショナルメモリ)

- **対象**: 新規`src/concurrent/stm.lisp`
- **現状**: 共有状態の更新はロックのみ。ネストした更新の原子性保証なし
- **内容**: `(cl-cc:atomically ...)` トランザクションブロック。TL2（Transactional Locking 2）アルゴリズム: 読み書きセットの記録→コミット時の検証→競合時リトライ。`(cl-cc:retry)` / `(cl-cc:or-else ...)` のComposable transactionセマンティクス。Haskell STM / Clojure refs と同等のAPIと意味論
- **根拠**: ロックによる並行制御は合成不可能（デッドロック）。STMはネスト可能で合成可能な並行制御。Clojureの成功事例あり
- **難易度**: Hard

#### FR-524: Structured Concurrency / Nurseries (構造化並行性)

- **対象**: `src/concurrent/`, FR-387（グリーンスレッド）前提
- **現状**: `(cl-cc:spawn)` でタスク起動するが、ライフタイム管理・エラー伝播が非構造的
- **内容**: `(cl-cc:with-nursery (n) (cl-cc:spawn-in n task1) (cl-cc:spawn-in n task2))` セマンティクス: スコープ終了時に全子タスクの完了を保証（join-all）。子タスクの例外は親に伝播。**キャンセル**: 一つの子が失敗すると他の子をキャンセル。タスクグラフの健全なライフタイム管理。Python Trio / Kotlin structured concurrency / Swift `async let`
- **根拠**: goroutine leakの根本的解決。スコープに束縛された並行性はライフタイム推論（FR-341エスケープ解析）と相性が良い
- **難易度**: Hard

#### FR-525: Huge Pages / THP Support (ヒュージページサポート)

- **対象**: `src/runtime/heap.lisp`, `src/cli/main.lisp`
- **現状**: 通常の4KBページでmalloc/mmap。TLBスラッシングの対策なし
- **内容**: **madvise MADV_HUGEPAGE** をヒープ領域に設定（Linux THP）。**mmap(MAP_HUGETLB)** で2MBページを直接割り当て（`--huge-pages`フラグ）。macOS `VM_FLAGS_SUPERPAGE_SIZE_2MB` 対応。Javaヒープのような大規模ヒープでのTLBミス削減（通常4KB→2MB: TLBエントリ512倍の効率）
- **根拠**: ヒープが数百MB以上になると4KBページのTLBカバレッジが不足。HotSpot `-XX:+UseLargePages` / jemalloc huge page arena相当
- **難易度**: Easy

---

### Phase 102 — 言語機能・型システム拡張 II

#### FR-528: First-Class Continuations / call/cc (一級継続)

- **対象**: `src/compile/cps.lisp`, `src/vm/vm.lisp`
- **現状**: CPS変換（`cps.lisp`）は内部的にcontinuationを使用するが、ユーザーに公開する`call/cc`/`call-with-current-continuation`なし
- **内容**: `(call-with-current-continuation #'(lambda (k) ...))` API。CPS変換済みのコードではkが直接継続クロージャ。`(funcall k value)` でキャプチャした継続に戻る。**脱出継続**（escape continuation）を効率的に実装（フルキャプチャより高速）。コルーチン（FR-386）・非局所脱出・バックトラッキングの統一的実装基盤
- **根拠**: SchemeのR7RS必須機能。cl-ccのCPS変換インフラは継続が「ただのクロージャ」として直接実装できる理想的構造
- **難易度**: Medium

#### FR-529: Delimited Continuations / shift-reset (限定継続)

- **対象**: `src/compile/cps.lisp`, `src/vm/vm.lisp`, FR-528前提
- **現状**: `call/cc`（FR-528）は現在の継続全体をキャプチャ。部分的なキャプチャなし
- **内容**: `(reset ...)` で継続のスコープを区切り、`(shift k ...)` でその範囲の継続をキャプチャ。**動的効果のエンコード**: 例外・状態・非決定性・非同期I/OをDelimited Continuationで統一表現。OCaml 5.0 effects / Racket `call-with-continuation-barrier` との相互運用。FR-343（エフェクトシステム）のランタイム実装基盤
- **根拠**: Full continuationより制限されるが型付けが容易。OCaml 5.0が限定継続をネイティブ採用（2022〜）、2026年時点で主流化
- **難易度**: Hard

#### FR-530: Pattern Matching with Exhaustiveness (網羅性検査付きパターンマッチング)

- **対象**: `src/expand/macros-basic.lisp`, `src/type/inference.lisp`
- **現状**: `cond`/`typecase`/`case`は網羅性検査なし。未ハンドルパターンが実行時エラー
- **内容**: `(cl-cc:match x ((list a b) ...) ((cons h t) ...) (:else ...))` マクロ。**網羅性チェック**: 型情報からすべてのケースがカバーされているかをコンパイル時検証。**到達不能ケース検出**: 先行パターンが後続を包含する場合に警告。**Decision tree compilation**: ネストdecision treeに最適変換（ジャンプテーブル / switch lower）。Rust `match` / OCaml `match` / Haskell case exhaustiveness と同等
- **根拠**: Lispのmatchは多数の実装（trivia / optima / etc.）があるがコンパイラ組み込みがない。網羅性はコンパイル時検証が安全の要
- **難易度**: Hard

#### FR-531: Refinement Types (精緻型)

- **対象**: `src/type/inference.lisp`, `src/type/types.lisp`
- **現状**: 型は集合論的基底型のみ（fixnum/string等）。述語付き部分型なし
- **内容**: `(cl-cc:define-refinement-type positive-integer fixnum (> x 0))` で述語付き型定義。`(declare (type positive-integer n))` で使用。VRP（FR-390）・SMTソルバ（FR-392）との連携で精緻型の充足を検証。`(defun sqrt (x positive-integer) ...)` の戻り値型推論。LiquidHaskell / F* / Stainless（Scala）相当
- **根拠**: 配列インデックス型 `(and fixnum (>= 0) (< array-length))` で境界チェックを型レベルで除去できる。bcE（FR-468）の完全静的版
- **難易度**: Very Hard

#### FR-532: Linear Types / Resource Types (線形型・リソース型)

- **対象**: `src/type/inference.lisp`, `src/compile/codegen.lisp`
- **現状**: ファイルハンドル・ネットワーク接続等のリソースの二重解放・未解放をコンパイル時に検出不可
- **内容**: `(cl-cc:define-linear-type file-handle)` で「正確に1回使用」が型レベルで保証されるリソース型。`(cl-cc:consume handle)` で消費をマーク。**Drop checker**: スコープ終了時に未消費の線形値があればコンパイルエラー。`(cl-cc:move val)` で所有権移転。Rust `ownership` / Linear Haskell / Clean uniqueness typingの軽量版
- **根拠**: CL条件システムでのリソースリーク（ファイル未close、ロック未解放）をコンパイル時に根絶。`with-open-file`マクロのより強力な代替
- **難易度**: Very Hard

#### FR-533: Compile-Time Format String Verification (コンパイル時フォーマット文字列検証)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/compile/codegen.lisp`
- **現状**: `(format t "~A ~D" x)` の引数数・型の正当性はコンパイル時未検証
- **内容**: `(format destination control-string args...)` のcontrol-stringがリテラルの場合、コンパイル時に解析。`~A`/`~D`/`~S`等の指示子数と実引数数の一致検査。`~*`（引数スキップ）・`~@`（引数変更）も解析。型不一致（`~D`に文字列等）を警告。GCC `__attribute__((format(printf,...)))` / Rust `format!` マクロの静的検証相当
- **根拠**: ANSI CL `format`は27種類以上の指示子を持つ複雑なDSL。引数ミスによるランタイムエラーが頻出
- **難易度**: Medium

#### FR-534: Compile-Time Regex Compilation (コンパイル時正規表現コンパイル)

- **対象**: 新規`src/compile/regex.lisp`, `src/expand/expander.lisp`
- **現状**: 正規表現マッチングは実行時のNFA/DFA構築
- **内容**: `(cl-cc:regex "pattern" string)` でパターンがリテラルの場合、コンパイル時にNFA→DFA変換→VM命令に直接コンパイル。`(cl-cc:regex-bind (m1 m2) "(\w+)\s+(\d+)" string) ...` で型付きキャプチャグループ。DFAの状態遷移表をコンパイル済みコード内にインライン展開。Rust `lazy_regex` / PERL compile-time regex相当
- **根拠**: 起動時DFA構築のオーバーヘッドゼロ。コンパイル時に正規表現の構文エラーを検出
- **難易度**: Hard

#### FR-535: Type Narrowing via Assert (アサートによる型絞り込み)

- **対象**: `src/type/inference.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `(assert (typep x 'fixnum))` の後でもxの型が絞り込まれない
- **内容**: `(assert (typep x 'fixnum))` → アサート後のパスでxを`fixnum`型として扱う（FR-428 Occurrence typingの拡張）。`(check-type x fixnum)` も同様。`(assert (> n 0))` → VRP（FR-390）にn∈[1,∞)を伝達。`(assert (not (null x)))` → xからnilを除く。ユーザー定義アサートへの適用は`(declare (cl-cc:asserts (> x 0)))` 形式
- **根拠**: CLの`assert`/`check-type`は実行時検証専用。型絞り込みへの接続でdevirt・unboxingの追加機会を得る
- **難易度**: Medium

---

### Phase 103 — ABI・エコシステム・開発者ヒント

#### FR-538: ABI Stability & Symbol Versioning (ABI安定性・シンボルバージョニング)

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`
- **現状**: コンパイル済みライブラリのABIバージョン管理なし。マイナーバージョンアップでABI破壊の危険
- **内容**: ELF **GNU Symbol Versioning**（`.gnu.version`/`.gnu.version_d`セクション）：`cl_eval@@CL_CC_2.0` 形式の版付きシンボル。旧バージョンのシンボルを互換エントリとして保持。Mach-O `install_name`/`current_version`/`compatibility_version` フィールド設定。`./cl-cc compile --abi-version=2.0 --compat-version=1.0`
- **根拠**: GCC `__attribute__((symver))` / ELF symbol versioning。共有ライブラリのメジャーバージョン非互換変更を安全に管理
- **難易度**: Medium

#### FR-539: Deprecation API (廃止予定API管理)

- **対象**: `src/expand/expander.lisp`, `src/parse/diagnostics.lisp`
- **現状**: 廃止予定関数を示す仕組みなし。`(declare (ignore x))`程度の宣言のみ
- **内容**: `(cl-cc:deprecate foo :since "2.0" :removed-in "3.0" :replacement #'bar :message "use bar instead")` 宣言。廃止予定関数の呼び出しサイトでコンパイル警告（FR-318連携）。`--Wdeprecated`で有効化。移行ガイドURL添付。**段階的廃止**: `--Wno-deprecated-v1`で旧バージョン非推奨のみ抑制
- **根拠**: SBCL `(deprecated :early "1.2.3")` / GCC `__attribute__((deprecated))`. APIの進化と後方互換性の管理に必須
- **難易度**: Easy

#### FR-540: Branch Probability Hints / likely-unlikely (分岐確率ヒント)

- **対象**: `src/expand/macros-basic.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: 分岐確率情報なし。コンパイラは全分岐を50/50と仮定
- **内容**: `(cl-cc:likely condition)` / `(cl-cc:unlikely condition)` マクロ。展開後の条件分岐に**静的分岐予測ヒント**付与（x86-64: `PT`/`PN` prefix、AArch64: future HINT encoding）。コード配置に反映: likely側をフォールスルー、unlikely側をジャンプターゲットに。PGOデータ（FR-480）がある場合はPGOを優先
- **根拠**: GCC `__builtin_expect` / Clang `[[likely]]`（C++20）/ Linux `likely()/unlikely()`。エラーハンドラが`unlikely`なら最適なコード配置が可能
- **難易度**: Easy

#### FR-541: Inline Assembly (インラインアセンブリ)

- **対象**: `src/compile/codegen.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: ネイティブアセンブリ命令に直接アクセスする手段なし
- **内容**: `(cl-cc:asm "cpuid" :inputs (:eax n) :outputs (:eax out :ebx brand) :clobbers (:rcx :rdx))` フォーム。GCC `asm volatile` スタイルの制約記述（`=r`, `r`, `m` 制約）。**型安全**: 入出力変数はCLの型でラップ。JIT（FR-330）コードにも適用可能。x86-64とAArch64の両方に対応
- **根拠**: `rdtsc`（高精度タイマー）・`cpuid`（CPU情報）・`pause`（spin-wait最適化）等のコンパイラが直接生成しない命令へのアクセスに不可欠
- **難易度**: Hard

#### FR-542: Hot/Cold Code Annotation (ホット/コールドコードアノテーション)

- **対象**: `src/compile/codegen.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: PGO（FR-480/FR-508）なしではコードの温度推定不可
- **内容**: `(cl-cc:declare-hot)` / `(cl-cc:declare-cold)` 関数宣言アノテーション。`(cl-cc:cold-path ...)` ブロックアノテーション（エラーハンドラ等に使用）。ホット関数を`.text.hot`に集約（I-キャッシュ局所性）。コールド関数を`.text.cold`（FR-471 Outliningとの統合）。`__attribute__((hot))`/`__attribute__((cold))` GCC相当
- **根拠**: PGOデータがない場合の手動最適化ガイド。エラーパスは確実にコールドであり手動指定が有効
- **難易度**: Easy

#### FR-543: Pointer Compression (ポインタ圧縮)

- **対象**: `src/runtime/heap.lisp`, `src/vm/vm.lisp`
- **現状**: ポインタは64ビット整数。ヒープが4GB以下でも8バイト/ポインタ
- **内容**: **CompressedOops（Java HotSpot方式）**: ヒープベースアドレスを別途保持し、ヒープ内ポインタを32ビットオフセットとして格納（4GB未満のヒープ）。`(cl-cc:compress-heap :limit 4gb)` 起動オプション。CLOSスロット・Consセル・ベクタ要素の圧縮。ポインタ解決は `base + offset * alignment` の1命令。V8 CompressedPointers / JVM CompressedOops実績あり
- **根拠**: 64ビットシステムでもヒープを4GB以下に収めることが多い。ポインタサイズ50%削減でキャッシュ密度向上、GCスキャン速度2x向上
- **難易度**: Hard

#### FR-544: Build Event Protocol / BEP (ビルドイベントプロトコル)

- **対象**: `src/cli/main.lisp`, `src/compile/pipeline.lisp`
- **現状**: ビルド進捗はstdoutへのフリーテキスト出力のみ。IDE/CI連携の構造化なし
- **内容**: **Bazel Build Event Protocol**互換のJSON/Protobuf構造化イベントストリーム: `BuildStarted`/`SourceFileAdded`/`CompilationStarted`/`CompilationFinished`/`TestResult`/`BuildFinished`。`--build-event-json-file=events.json`でファイル出力。`--build-event-publish-all-actions`でCI連携。VS Code `tasks.json`のproblemMatcherと統合可能
- **根拠**: Bazel BEP / Buck2 build events。IDE・CI・ダッシュボードとの統合標準プロトコル。2026年でBazel BEPがデファクト化
- **難易度**: Medium

#### FR-545: Documentation String Preservation (ドキュメント文字列保存)

- **対象**: `src/compile/codegen.lisp`, `src/emit/binary/`
- **現状**: `defun`本体のdocstringはコンパイル時に捨てられる可能性
- **内容**: docstringをコンパイル済みVMプログラムに保存（`vm-docstring`メタデータ命令）。`(documentation #'foo 'function)` が実行時にdocstringを返す。FASL（FR-499 PCH）にdocstring含む。`--strip-docstrings`で明示的に除去（プロダクション最適化）。SBCL `sb-ext:*restrict-compiler-policy*` 類似の制御
- **根拠**: CLの`documentation`はANSI仕様。セルフホスティングコンパイラ自身のdocstringがREPLで参照可能になる
- **難易度**: Easy

#### FR-546: Register Pressure Reduction (レジスタプレッシャー削減)

- **対象**: `src/compile/regalloc.lisp`, `src/optimize/optimizer.lisp`
- **現状**: レジスタ割り当てはスピルコスト最小化のみ。レジスタプレッシャーを意識した命令スケジューリングなし
- **内容**: **Belady's algorithm（MIN）**: 最遠将来使用点を基準としたスピル選択（最適なオフラインアルゴリズム）。**Pressure-aware scheduling**: 命令スケジューリング（FR-408）にレジスタプレッシャーを副目的関数として追加（ILP最小化とのバランス）。**Live range splitting**: ロングライブレンジを短く分割して干渉グラフのエッジ数削減
- **根拠**: 多くのスピルの根本原因は不適切な命令順序。プレッシャー考慮スケジューリングでスピル数を30〜50%削減できる
- **難易度**: Hard

---

### Phase 104 — ランタイムインフラ基盤

#### FR-550: Stack Map Generation (スタックマップ生成)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/runtime/gc.lisp`, FR-421（Moving GC）前提
- **現状**: GCはVM命令レベルのスタック（`vm-call-stack`）のみスキャン。JITコンパイル後の機械語スタックフレーム内ポインタを識別不可
- **内容**: 各JIT関数のコンパイル時に**スタックマップテーブル**を生成：`(pc-offset → {live-registers: {R1,R3}, live-stack-slots: {sp+8, sp+24}})` の対応表。GC時にこのテーブルでスタックフレームを走査してすべてのヒープ参照を発見。LLVM StatepointGC / JVM GC map / V8 StackMaps相当。Moving GC（FR-421）・Concurrent GC（FR-420）の必須前提インフラ
- **根拠**: スタックマップなしでMoving GCは実装不可能。JITコンパイラとGCの連携の要
- **難易度**: Hard

#### FR-551: Safepoints (セーフポイント)

- **対象**: `src/vm/vm-run.lisp`, `src/emit/x86-64-codegen.lisp`, FR-420（Concurrent GC）前提
- **現状**: GCはVM命令間の暗黙の停止点のみ。JITコード実行中の安全な停止点がない
- **内容**: **ポーリングベース**: JITコードのバックエッジ・関数エントリ・一定命令間隔にポーリング命令（`test [safepoint_flag], 0`）を挿入。GCがflagをセットすると次のポーリングで停止。**スタックウォーキング**: 停止時に全スレッドのフレームをスタックマップ（FR-550）で走査。**Stop-the-World実装**: Concurrent GC（FR-420）のSTWフェーズを実現。JVM Safepoint / HotSpot polling page / V8 safepoints
- **根拠**: Concurrent GCがスタックスキャンを行う唯一安全な方法。ランタイムの根幹インフラ
- **難易度**: Hard

#### FR-552: Write Barrier Optimization (書き込みバリア最適化)

- **対象**: `src/runtime/gc.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: 2世代GC（`gc.lisp`）の世代間参照追跡（remembered set）にwrite barrierが必要だが最適化なし
- **内容**: **Card Table barrier**: ヒープを512バイト「カード」に分割し、スロット書き込み時に対応カードをダーティマーク（1バイト書き込みのみ）。**Remembered set barrier**: Old→Youngの参照のみ記録（Young→Old不要）。**Barrier elision**: コンパイル時にYoung世代オブジェクトへの書き込みと証明された場合はバリア省略（エスケープ解析FR-341連携）。**Snapshot-at-the-beginning (SATB)**: Concurrent GC用のバリア実装
- **根拠**: write barrierは頻繁に実行されるコード（スロット書き込み毎）。最適化なしでは5〜10%オーバーヘッドが常に発生
- **難易度**: Hard

#### FR-553: Lazy JIT Compilation / Call Stubs (遅延JITコンパイル)

- **対象**: `src/jit/baseline.lisp`, `src/vm/vm.lisp`
- **現状**: JIT（FR-330〜331）は呼び出しカウント到達時にコンパイル開始。初回呼び出し時の遅延コンパイルなし
- **内容**: **コールスタブ**: 未コンパイル関数エントリに「コンパイルしてから再呼び出し」する5バイトのtrampoline stub（`call compile_stub`）を配置。初回呼び出し時のみJITコンパイルを起動し、完了後にstubを実際のコードへの`jmp`で上書き（atomic patch）。**バックグラウンドコンパイル**: コンパイルをバックグラウンドスレッドで実行し、インタープリタで実行継続。V8 Ignition stubs / HotSpot interpreter stubs
- **根拠**: 全関数を起動時にコンパイルすると初回レイテンシが爆発。Lazy compilationで使われた関数のみをコンパイルする省エネ戦略
- **難易度**: Hard

#### FR-554: JIT Code Cache Persistence (JITコードキャッシュ永続化)

- **対象**: `src/jit/`, `src/cli/main.lisp`
- **現状**: JITコンパイル結果はプロセス終了時に消失。次回起動で再コンパイルが必要
- **内容**: JITコンパイル済み機械語をファイルにシリアライズ（`~/.cache/cl-cc/jit/<hash>.jit`）。コンテンツハッシュ（ソースハッシュ + CPUフィーチャー + 最適化レベル）でキャッシュキー決定。次回起動時にキャッシュヒットすればJITコンパイルをスキップ。セキュリティ: キャッシュファイルに署名（FR-378）。Chromium V8 Code Cache / Android ART AOT compiler相当
- **根拠**: JITウォームアップ時間の削減。セルフホスティングコンパイラのJIT起動を2回目以降でほぼゼロに
- **難易度**: Hard

#### FR-555: Class Hierarchy Analysis / CHA (クラス階層解析)

- **対象**: 新規`src/analyze/cha.lisp`, `src/vm/vm-clos.lisp`
- **現状**: devirt（FR-337）はPIC（FR-334）の観測データに依存。クラス階層情報を使った静的解析なし
- **内容**: `*class-registry*` からCLOSクラスの継承木を構築（`class → [subclasses]`）。あるgeneric functionのメソッドが「特定クラスのサブクラスが存在しない」場合は**単相と証明**しdevirt。`(defgeneric area (shape))` で`shape`のサブクラスが`circle`のみなら`area`呼び出しを`area-circle`への直接呼び出しに変換。CHA + PIC = 最強のdevirt。Java HotSpot CHA / .NET CLR devirt
- **根拠**: PICは実行時観測に依存するため静的証明できない。CHAは「このクラスにはサブクラスがない」という不変条件から永続的な最適化が可能
- **難易度**: Medium

---

### Phase 105 — JIT高度化

#### FR-558: Trace JIT / Hot Trace Recording (トレースJIT)

- **対象**: `src/jit/`, `src/vm/vm-run.lisp`
- **現状**: JIT（FR-330〜334）はメソッド（関数）単位のコンパイル。ホットトレース（パス）単位のコンパイルなし
- **内容**: **トレース記録**: ループバックエッジのホットカウント到達時に実行パスを**線形命令列（トレース）**として記録。呼び出し先関数も含めてインライン化した超長直線コードを生成。**ガード命令**: トレースが想定した型・分岐方向と異なる場合に脱出（side exit）するガード挿入。**トレースツリー**: 複数のsideexitから派生するトレースをツリー構造で管理。LuaJIT / Mozilla TraceMonkey (Firefox 3.5〜) / PyPy
- **根拠**: LuaJITがトレースJITで他のLuaより10〜50x高速化を実現。メソッドJITより呼び出しオーバーヘッドがなく、ループ集中型ワークロードに特に有効
- **難易度**: Very Hard

#### FR-559: Type Feedback Collection (型フィードバック収集)

- **対象**: `src/vm/vm-clos.lisp`, `src/jit/baseline.lisp`
- **現状**: PIC（FR-334）はメソッドキャッシュのみ。汎用な型フィードバック（引数型・戻り値型の実行時観測）なし
- **内容**: **ICベース型プロファイリング**: 各呼び出しサイト・各演算に**型プロファイルスロット**を付与。Tier-0/Tier-1実行時に型観測データを蓄積（`fixnum/float/string/cons`の出現率）。**フィードバックベクタ**: 関数ごとのICデータを構造化して保持。Tier-2コンパイル時にフィードバックデータを入力としてspecialized codeを生成。V8 Maglev / HotSpot type profiling
- **根拠**: PICだけではカバーできない「変数の型」の観測。型フィードバックはJIT投機的最適化の全ての基盤
- **難易度**: Hard

#### FR-560: Speculative Inlining with Guards (ガード付き投機的インライン化)

- **対象**: `src/jit/`, FR-559（型フィードバック）前提
- **現状**: インライン化（ML-guided FR-372）は静的サイズ閾値。型フィードバックに基づく動的インライン化なし
- **内容**: 型フィードバック（FR-559）で「この呼び出しサイトで受信したオブジェクトの98%がclass Xである」と判明した場合、**型ガード + インライン済みコード + deoptスローパス**を生成。型ガード（`typep obj 'X`）が失敗した場合のみdeopt（FR-333）。ホットパスは型チェック1命令 + インライン済みメソッド本体。V8 TurboFan / HotSpot speculative inlining
- **根拠**: 一般的なCLOSコードで呼び出しの95%以上が同一型。投機的インライン化でgeneric dispatch overhead完全除去
- **難易度**: Hard

#### FR-561: Megamorphic IC Handling (メガモーフィックICハンドリング)

- **対象**: `src/vm/vm-clos.lisp`, `src/jit/baseline.lisp`, FR-334（PIC）の拡張
- **現状**: PIC（FR-334）は最大4エントリでオーバーフロー後はグローバルキャッシュにフォールバック
- **内容**: PICが4エントリを超えた**メガモーフィック**状態での専用最適化: **グローバル型フィードバックキャッシュ**: 全コールサイト横断でのメソッドキャッシュ共有。**Inlining caches for megamorphic**: ハッシュベースのO(1)ディスパッチを維持しつつ予測不能な分岐を除去。**型分布に基づく確率的speculative inlining**: 最頻出1〜2型のみガード付きインライン化
- **根拠**: GCキャラクタリゼーション的コード（多相コレクション操作）では避けられないメガモーフィック状態。専用ハンドリングで未最適化フォールバックを回避
- **難易度**: Hard

#### FR-562: JIT Warmup / AOT Pre-warming (JITウォームアップ最適化)

- **対象**: `src/jit/`, `src/cli/main.lisp`
- **現状**: JIT（FR-330〜331）はcall countが閾値到達後に初めてコンパイル。最初の数千回呼び出しはインタープリタ
- **内容**: **プロファイル付きAOTコンパイル**: 事前プロファイル実行（`./cl-cc run --warmup-profile app.lisp`）でホット関数を特定→`--aot-warm`でそれらをプロセス起動前にコンパイル済み状態にする。**Lazy compilation ordering**: 依存関係順にコンパイルして初期化シーケンスを最適化。JIT閾値を0に設定して即時Tier-2へのオプション（`--jit-threshold=0`）
- **根拠**: サーバープロセスの「ウォームアップ期間」でのスループット低下問題（数分間の低速フェーズ）を解消。Javaの-server flagとAOTコンパイルの折衷案
- **難易度**: Medium

---

### Phase 106 — オブジェクト・レイアウト・演算最適化

#### FR-565: Type-Based Alias Analysis / TBAA (型ベースエイリアス解析)

- **対象**: `src/optimize/optimizer.lisp`, 新規`src/optimize/tbaa.lisp`, FR-340（エイリアス解析）の拡張
- **現状**: 汎用エイリアス解析（FR-340）はポインタ解析ベース。型情報を使ったエイリアス証明なし
- **内容**: **CLOS型に基づくno-alias証明**: 異なるクラスのインスタンスはスロットが重なりえない（`circle`の`:radius`と`rect`の`:width`はno-alias）。**スロット型計算**: `(slot-value c1 :x)` と `(slot-value r1 :x)` がno-aliasと証明されれば命令リオーダーが可能。LLVM TBAA metadata（`!tbaa`）/ C `restrict` の型システム版。汎用エイリアス解析より低コストで高精度
- **根拠**: CLOSコードのほとんどのスロットアクセスはTBAAで解決できる（クラスが明確な場合）。MemSSA（FR-409）+ TBAAでloop-carried依存の大半を除去
- **難易度**: Medium

#### FR-566: Object Layout Optimization (オブジェクトレイアウト最適化)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **現状**: CLOSオブジェクトはスロット名→値の汎用ハッシュテーブル。フィールドオフセットが実行時計算
- **内容**: **固定オフセット化**: `(defclass point (x y))` → `x`をoffset 0、`y`をoffset 8の**C構造体スタイル**に変換。スロットアクセスが`[base + offset]`の直接メモリ参照に。**ホット/コールドフィールド分離**: 頻繁アクセススロットを先頭に配置（プロファイル駆動）。**ネイティブ構造体としてのコンパイル**: `defstruct`/`defclass`の宣言でハッシュテーブルなし構造体生成。**CLOS互換維持**: MOP経由のダイナミックアクセスにはフォールバック
- **根拠**: 現在のハッシュテーブルベースCLOSオブジェクトに対してslot-value一回が~30命令。固定オフセット化で2命令に
- **難易度**: Hard

#### FR-567: Dead Slot Elimination (デッドスロット除去)

- **対象**: `src/compile/codegen.lisp`, FR-566（Object layout）と連携
- **現状**: `defclass`で定義した全スロットがオブジェクトに存在。読まれないスロットも割り当てられる
- **内容**: **全プログラム解析**: コールグラフ全体を走査し、特定クラスのスロットが一度も`slot-value`で読まれない場合に「デッドスロット」と判定。LTO（FR-335）モードでの全プログラム解析。`--strip-dead-slots`フラグ。デッドスロットは構造体から除去（メモリ節約 + キャッシュ改善）。`(declare (cl-cc:keep-slot x))` でエクスポートスロットを明示保護
- **根拠**: デバッグ用・将来用スロットが残留するのはよくあるパターン。Large objectsでのメモリ削減効果大
- **難易度**: Medium

#### FR-568: Copy Elision / NRVO (戻り値最適化・コピー省略)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **現状**: 関数の戻り値は常にスタック/レジスタ経由でコピー。大きなオブジェクト返却のコピーが常に発生
- **内容**: **Named Return Value Optimization (NRVO)**: 関数内で作成して返す単一オブジェクトを呼び出し元の変数に**直接構築**（コピーなし）。**Return Value Optimization (RVO)**: 匿名一時オブジェクトのin-place構築。`(let ((result (make-array n))) ... result)` パターンの検出と変換。C++ RVO/NRVO / Rust move semantics + `return` optimization
- **根拠**: 大きな配列・多スロットCLOSオブジェクトの返却コストをゼロに。数値計算での中間結果コピー除去
- **難易度**: Hard

#### FR-569: Multiple Return Values via Registers (多値レジスタ返却)

- **対象**: `src/compile/codegen.lisp`, `src/emit/calling-convention.lisp`
- **現状**: `(values a b c)` は複数値オブジェクト（ヒープ割り当て）またはスタック経由で実装
- **内容**: 最大2〜4個の多値を**複数レジスタ**（x86-64: RAX+RDX+RCX+R8）で直接返却。`(multiple-value-bind (a b) (floor x y) ...)` でデスチネーションが確定している場合はヒープ割り当てゼロ。呼び出し側でのmv-call展開時にも複数レジスタを直接受け取るABIを使用。SBCL already does this for 2 values / LuaJIT multiple returns via fixed slots
- **根拠**: `(floor x y)` / `(truncate x y)` / `(round x y)` は数値コードの頻出多値関数。毎回ヒープ割り当てが発生するのは過剰
- **難易度**: Medium

#### FR-570: Branch-Free Arithmetic (分岐フリー算術)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `(abs x)` / `(min a b)` / `(max a b)` / `(signum x)` が条件分岐にコンパイルされる可能性
- **内容**: **整数abs**: `(abs x)` → `sar reg, 63; xor rax, reg; sub rax, reg`（分岐ゼロ）。**整数min/max**: CMOV命令（`cmovl`/`cmovg`）使用。**float abs/min/max**: `andps`/`andnps`/`maxss`/`minss` SIMD命令。**signum**: ゼロ比較 + 符号ビット抽出の組み合わせ。FR-410（If-Conversion）の特殊ケースとして実装可能
- **根拠**: 分岐予測が難しい数値コード（データ依存のmin/max）でのパフォーマンス。コンパイラが自動変換すれば手書き最適化不要
- **難易度**: Easy

#### FR-571: Interprocedural Register Allocation (手続き間レジスタ割り当て)

- **対象**: `src/compile/regalloc.lisp`, FR-414（Custom Calling Convention）連携
- **現状**: レジスタ割り当ては関数ごと独立。関数境界でのcallee-save保存/復元が常に発生
- **内容**: **呼び出しグラフベースのRA**: リーフ関数（再帰なし・内部呼び出しのみ）について呼び出し側と被呼び出し側が**同一レジスタ集合を共用**するRA。caller-saved拡張（calling convention merging）。非リーフ関数には適用せずABI互換を維持。IPRAアノテーション（`(declare (cl-cc:ipra))`）で明示指定
- **根拠**: GCC IPRA (`-fipa-ra`). 内部呼び出しの多い関数ファミリ（相互再帰）でcallee-save保存を削減
- **難易度**: Hard

---

### Phase 107 — エコシステム・診断追加

#### FR-574: Compilation Database / compile_commands.json (コンパイルデータベース)

- **対象**: `src/cli/main.lisp`, `cl-cc.asd`
- **現状**: 各ファイルのコンパイルオプション・インクルードパスの構造化記録なし
- **内容**: `./cl-cc compile --emit-compile-commands *.lisp` で **`compile_commands.json`** を生成（Clang tooling標準形式）。`[{"file": "src/vm/vm.lisp", "command": "cl-cc compile ...", "directory": "/..."}]` 形式。clangd / clang-tidy の代替ツール（lsp-mode / eglot）が参照可能。`ASDF:compile-system`フックで自動生成
- **根拠**: IDE・静的解析ツール・フォーマッタが「どのオプションでコンパイルするか」を知るための標準インターフェース。2026年でClang tooling chainが標準化
- **難易度**: Easy

#### FR-575: Core Dump / Crash Report Analysis (コアダンプ・クラッシュレポート解析)

- **対象**: `src/vm/vm-run.lisp`, `src/cli/main.lisp`
- **現状**: VM例外は`handler-case`でキャッチ。未捕捉例外でのクラッシュ情報が不十分
- **内容**: **構造化クラッシュレポート**: 未捕捉conditionでのVM状態（レジスタ・スタックトレース・ヒープ統計）を`crash-TIMESTAMP.cl-cc-dump`に自動保存。`./cl-cc analyze-crash crash.cl-cc-dump` でインタラクティブなpost-mortem調査（FR-310デバッガの読み込みモード）。**シグナルハンドラ**: SIGSEGV/SIGABRTでCクラッシュをキャッチしてbacktrace収集。macOS CrashReporter / Sentry crash reporting形式での出力
- **根拠**: 本番環境でのVMクラッシュは再現が困難。クラッシュ時の完全状態を保存することでデバッグを可能にする
- **難易度**: Medium

#### FR-576: Work-Stealing Scheduler (ワークスティーリングスケジューラ)

- **対象**: `src/concurrent/`, FR-387（グリーンスレッド）、FR-524（構造化並行性）前提
- **現状**: グリーンスレッドのスケジューラは未実装（FR-387で計画のみ）
- **内容**: **Cilk-like Work Stealing**: 各OSスレッドが**deque**（両端キュー）でタスクを管理。スレッドが暇になると他スレッドのdeque末尾からタスクを**スチール**。`with-nursery`（FR-524）のspawn-inはローカルdequeの先頭に積む。**Continuation Stealing**: 継続を盗む版（call/cc FR-528との統合）。**Randomized victim selection**でデッドロック回避。Intel TBB / Cilk++ / Rust Rayon
- **根拠**: 構造化並行性（FR-524）のスケジューラとして最適。理論的にwork-stealing はO(P × T∞ + T₁/P)の最適実行時間を保証（P: プロセッサ数、T₁: 逐次時間、T∞: 並列深度）
- **難易度**: Hard

#### FR-577: Continuation Marks (継続マーク)

- **対象**: `src/vm/vm.lisp`, `src/compile/cps.lisp`
- **現状**: デバッガ・プロファイラがスタック情報を得るためにfull call stack（`vm-call-stack`）を走査。実行中のフレームに任意メタデータを添付する手段なし
- **内容**: `(cl-cc:with-continuation-mark key value body)` フォーム：現在のテール位置に**キー・値ペア**をマーク。`(cl-cc:current-continuation-marks key)` で最新マークを取得。テール呼び出し時はマークを置き換える（スタックスペース O(1)）。**用途**: プロファイラ（関数名マーク）・デバッガ（ブレークポイント条件）・動的変数（FLUIDバインディング）。Racket `with-continuation-mark` / SRFI-157
- **根拠**: full call/cc（FR-528）なしでデバッガ・プロファイラが必要な情報を効率的に取得できる。スタックを汚染しないメタデータ伝達機構
- **難易度**: Medium

#### FR-578: Compile-Time SQL DSL (コンパイル時SQLドメイン特化言語)

- **対象**: 新規`src/compile/sql-dsl.lisp`, `src/expand/expander.lisp`
- **現状**: SQLクエリは実行時文字列結合。型安全性・インジェクション防止なし
- **内容**: `(cl-cc:sql select (user.name user.email) from user where (= user.age (param age fixnum)))` マクロ: **コンパイル時SQL検証**（構文・テーブル名・カラム名）。`param`でバインドパラメータ（SQLインジェクション防止）。スキーマ定義（`cl-cc:defsql-schema`）からの型チェック。プリペアドステートメントへの自動変換。Haskell Persistent / TypeScript Prisma / Rust SQLx の型安全クエリに相当するCL実装
- **根拠**: CL製Webアプリでのプリペアドステートメントの一貫した使用を強制。コンパイル時エラーでSQLインジェクション脆弱性を排除
- **難易度**: Hard

#### FR-579: REPL Session Recording & Replay (REPLセッション記録・再生)

- **対象**: `src/cli/main.lisp`, FR-312（REPL拡張）の拡張
- **現状**: REPLの入力/出力は揮発性。セッションの再現不可
- **内容**: `./cl-cc repl --record session.cl-cc-repl` でREPL入力・出力・VM状態差分を構造化JSON/Lispデータとして記録。`./cl-cc repl --replay session.cl-cc-repl` で再生（テスト化）。`(cl-cc:save-session "file.cl-cc-repl")` REPLコマンド。**REPLセッション→テストスクリプト変換**: 記録したセッションをFiveAMテストに自動変換（`--export-as-tests`）
- **根拠**: SBCL REPLでの探索的開発の結果をテストに変換するワークフロー。バグ再現セッションの共有にも有効
- **難易度**: Medium

#### FR-580: GC Safepoint-Free Regions (GCセーフポイントフリー領域)

- **対象**: `src/runtime/gc.lisp`, FR-551（Safepoints）の拡張
- **現状**: 全コードパスにセーフポイントポーリングが挿入される（FR-551）
- **内容**: `(cl-cc:without-gc-preemption ...)` フォームでGCセーフポイントチェックを一時無効化。**リアルタイムコード**: オーディオコールバック・割り込みハンドラ等のGCポーズ許容不可なパスに使用。無効化中のアロケーションは禁止（コンパイル時検証、FR-357 Allocation-free verificationと連携）。Azul Zing / OpenJDK `@Restricted` annotation相当
- **根拠**: ソフトリアルタイム要件（レイテンシ保証）のコードでGCセーフポイントの確定的除去が必要
- **難易度**: Medium

---

### Phase 108 — バイナリ・リンカ高度化 II

#### FR-582: Identical Code Folding / ICF (同一コード折り畳み)

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`
- **現状**: 内容が同一の関数が異なるシンボル名で複数コンパイル済みバイナリに存在しうる
- **内容**: リンク時に全関数のバイト列ハッシュを計算し、**内容が同一の関数を1つに統合**（別名シンボルを同一アドレスに向ける）。セーフICF（`--icf=safe`）: 呼び出し可能なアドレス比較を行うコードがある場合は除外。アグレッシブICF（`--icf=all`）: 関数ポインタ比較も無視して統合。Mach-O `.subsections_via_symbols` + LD64 `-object_path_lto`、ELF LLD `--icf=safe`
- **根拠**: Chromeでの計測で3〜6%のバイナリサイズ削減。cl-ccが生成するCLOS accessor stub・ジェネリック dispatch wrapper に多数の同一コードが存在
- **難易度**: Medium

#### FR-583: Split Debug Info / .dwo Files (分離デバッグ情報)

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`, FR-195（DWARF）拡張
- **現状**: DWARF情報は本体バイナリに直接埋め込み（計画段階）
- **内容**: **DWARF Skeleton CU + .dwo**: バイナリ本体には「スケルトン」（アドレス→ファイル名+行番号のマッピングのみ）を残し、詳細デバッグ情報（型情報・変数名）を別ファイル`foo.dwo`に分離。`dwp`（DWP packager）で複数.dwoを`foo.dwp`に集約。本番バイナリの肥大化なし + デバッガは.dwpを参照。**DWARF zstd圧縮**（DWARF 5 `DW_FORM_strp_sup` + ZSTD）: デバッグ情報を1/5〜1/10に圧縮
- **根拠**: 実用的なLispバイナリでDWARFが本体の数倍のサイズになることも。Clang `-gsplit-dwarf` / GCC `-gsplit-dwarf` は本番デプロイの標準
- **難易度**: Medium

#### FR-584: Patchable Function Entry Points (パッチ可能関数エントリ)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: 関数エントリは最小命令。実行中の差し替えのための予約バイトなし
- **内容**: `--patchable-function-entry=N` フラグで各関数エントリ前にN個の**NOP命令**を挿入（x86-64: `nop` / AArch64: `nop`）。実行時にNOP列を`jmp patch_handler`で上書き可能（dtrace / SystemTap / eBPF uprobe方式）。**Hot patching**: ライブラリの関数を再起動なしに差し替え（`-fpatchable-function-entry=8,4`でエントリ前4バイト + 後4バイト）。GCC/Clang `-fpatchable-function-entry`
- **根拠**: 本番環境での動的計装（profiling・hot fix）に必要。FR-576（ワークスティーリング）のスケジューラフックにも使用
- **難易度**: Medium

#### FR-585: Cache Line Alignment Annotations (キャッシュライン整列アノテーション)

- **対象**: `src/compile/codegen.lisp`, `src/runtime/heap.lisp`
- **現状**: データ構造のキャッシュライン境界へのアライメント指定なし
- **内容**: `(declare (cl-cc:align 64))` でCLOSオブジェクト・配列を64バイトキャッシュライン境界に配置。`(cl-cc:define-aligned-struct name 64 ...)` でアライメント付き構造体定義。`(cl-cc:cache-line-size)` → 実行時キャッシュライン検出（`cpuid`）。SIMD命令の`vmovaps`（aligned）使用でペナルティゼロ。ヒープアロケータ（FR-423アリーナ）のデフォルトアライメント設定
- **根拠**: SSE/AVX aligned load/storeは非アライメントより最大10%高速。ホットデータ構造の意図的アライメントがL1キャッシュ効率を大幅改善
- **難易度**: Easy

#### FR-586: False Sharing Elimination (偽共有除去)

- **対象**: 新規`src/concurrent/padded.lisp`, `src/runtime/heap.lisp`
- **現状**: 並列スレッドが同一キャッシュライン内の異なる変数を更新した場合の偽共有問題への対策なし
- **内容**: `(cl-cc:defpadded-var *counter* 0)` で隣接変数と別キャッシュラインに強制配置（64バイトパディング）。スレッドローカルストレージ（TLS）との連携: `(cl-cc:thread-local *local-count* 0)` で変数をスレッドローカルに。**False sharing detector**: Thread Sanitizer（FR-399）拡張で同一キャッシュラインへの競合アクセスを警告。Linux `perf c2c`（Cache to Cache）相当の分析
- **根拠**: マルチコア並列コードの主要なパフォーマンス罠。並列カウンタ・並列アロケータで実測5〜30x減速事例あり
- **難易度**: Medium

#### FR-587: GOT/PLT Lazy Binding Optimization (GOT/PLT遅延バインディング最適化)

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`, FR-197（PIC）拡張
- **現状**: PIC（FR-197）でGOT/PLTを生成するが遅延バインディング最適化なし
- **内容**: **PLT IBTPLT**: 関数が同一バイナリ内で定義されている場合はPLTをバイパスして直接RIP-relative呼び出し。**GOTPCRELX relocation**: `mov rax, [rip+got@gotpcrelx]` → `lea rax, [rip+symbol@plt]` への最適化（linker relaxation）。**Eager binding** (`RTLD_NOW`): 起動時に全シンボル解決（セキュリティ向上）。ELF `-z now` / Mach-O `-bind_at_load`
- **根拠**: PLT indirectionは関数呼び出し毎に余分なジャンプ1回。ライブラリ呼び出し頻度が高い場合に顕著
- **難易度**: Hard

---

### Phase 109 — 型システム拡張 III

#### FR-590: Algebraic Data Types / ADTs (代数的データ型)

- **対象**: `src/expand/macros-basic.lisp`, `src/type/types.lisp`
- **現状**: CLの型システムはCLOSクラス階層（積型）のみ。和型（tagged union / sum types）の組み込みサポートなし
- **内容**: `(cl-cc:defdata shape (circle :radius float) (rect :width float :height float) (triangle :base float :height float))` マクロ。**タグ付きユニオン**として効率的に表現（最大バリアントサイズ + 1タグワード）。`(cl-cc:match)` との統合（FR-530 Pattern matchingとの連携）で**コンパイル時網羅性保証**。型推論で各バリアントを識別。Haskell ADT / Rust enum / OCaml variant
- **根拠**: CLのdefclassはopen（後付けサブクラス可能）で網羅性保証が難しい。ADTはclosed（定義時に全バリアント固定）で網羅性をコンパイル時に検証できる
- **難易度**: Hard

#### FR-591: Newtype / Zero-Cost Wrappers (ニュータイプ・ゼロコストラッパ)

- **対象**: `src/type/types.lisp`, `src/compile/codegen.lisp`
- **現状**: 型エイリアス（`(deftype positive-integer () ...)`)はコンパイル時に消去され名目型（nominal type）が使えない
- **内容**: `(cl-cc:defnewtype user-id fixnum)` でfixnumと同一実装だが**型検査は別物**の新型定義。`(user-id 42)` が`fixnum`を取る関数に誤渡しされるとコンパイルエラー。**ゼロコスト**: ランタイム表現はラップ元と同一（ボックス化なし）。`(cl-cc:unwrap x)` で明示的にアンラップ。Haskell `newtype` / Rust newtype pattern / Kotlin value class
- **根拠**: `user-id`と`product-id`が両方fixnumの場合に混同をコンパイル時に検出。API設計の安全性向上
- **難易度**: Medium

#### FR-592: Higher-Kinded Types / HKT (高カインド型)

- **対象**: `src/type/inference.lisp`, `src/type/types.lisp`
- **現状**: 型パラメータは具体型のみ（`(list fixnum)` 等）。型コンストラクタを型引数にできない
- **内容**: `(cl-cc:definterface Functor (F) (fmap (fn (a) b) (F a) → (F b)))` のような**型コンストラクタ上の抽象**。`*` / `* → *` / `(* → *) → *` 等のカインドアノテーション。型推論にカインドチェックを統合。**型クラス/プロトコル**: 複数の型にわたる共通インターフェースをHKTで表現（FunctorをListにもMaybeにも適用）。Haskell type classes / Scala implicits / Rust traits の型システム基盤
- **根拠**: `(map f (list 1 2 3))` と `(map f (maybe 5))` を同一プロトコルで書けるようになる。CLのCLOS generic functionとは別の静的なポリモーフィズム
- **難易度**: Very Hard

#### FR-593: Bidirectional Type Checking (双方向型検査)

- **対象**: `src/type/inference.lisp`
- **現状**: MEMORY.mdに「Bidirectional type inference (check mode vs synth mode)」が「Remaining Work」として記載。未実装
- **内容**: **Check mode（下向き）**: 期待型が既知の文脈（`(the fixnum expr)` / `(declare (type fixnum x))` / 関数引数位置）でexpression typeを型に対してcheckする。**Synth mode（上向き）**: 型が不明な文脈でexpressionから型を合成する。現在の単方向HMより多くの型を推論できる（特に高階関数・ADT）。Dunfield-Krishnaswami ICFP 2013 Bidiretional Typing
- **根拠**: MEMORY.mdに明記されたRemainingWork。双方向化によりアノテーション数が大幅に削減され、複雑なHOF・ADTパターンの型推論が可能に
- **難易度**: Hard

#### FR-594: Gradual Typing Improvements (段階的型付け改善)

- **対象**: `src/type/inference.lisp`, `src/compile/codegen.lisp`
- **現状**: 型付きコードと非型付きコードの境界での型変換（キャスト）の最適化なし
- **内容**: **Cast insertion optimization**: 型境界でのランタイムチェック(`typep`)挿入を最小化（隣接するキャストのキャンセル検出）。**Blame tracking**: 型違反が発生した場合の責任帰属（型付きコードが悪いか、非型付きコードが悪いか）を追跡してエラーメッセージに表示。**Concrete type propagation**: 型付き関数が非型付き関数を呼ぶ場合の型情報伝播の改善。Typed Racket / Reticulated Python / Transient semantics
- **根拠**: CLの段階的型付けは`:type`宣言と`check-type`の組み合わせに依存。型境界の最適化なしではパフォーマンスペナルティが大きい
- **難易度**: Hard

#### FR-595: Type-Level Computation (型レベル計算)

- **対象**: `src/type/inference.lisp`
- **現状**: 型は静的な集合論的記述のみ。型レベルの計算（条件・算術）なし
- **内容**: **Type families**: `(cl-cc:deftype-family ElementType (T) ...)` で型パラメータから別の型を計算。`(ElementType (list fixnum))` → `fixnum`。**条件型**: `(cl-cc:if-type (subtype-p T fixnum) T float)` で型レベルのif式。**型レベル自然数**: `(array-length-type 5)` → `5-element-fixnum-array`の型。Haskell TypeFamilies / TypeLits / C++ `std::conditional_t`
- **根拠**: ADT（FR-590）・HKT（FR-592）の実用的活用に型レベル計算が必要。配列長を型に埋め込んで境界チェックをゼロコスト化できる
- **難易度**: Very Hard

---

### Phase 110 — 並行モデル拡張

#### FR-598: Actor Model (アクターモデル)

- **対象**: `src/concurrent/`, FR-387（グリーンスレッド）+ FR-524（構造化並行性）前提
- **現状**: グリーンスレッドは共有メモリ型。メッセージパッシング抽象なし
- **内容**: `(cl-cc:defactor my-actor (state) (on :increment (delta) (+ state delta)) (on :get () state))` マクロ。**軽量アクタープロセス**（Erlang-style）: メールボックス付きgreem thread。`(cl-cc:send actor :increment 5)` でメッセージ送信。`(cl-cc:receive &key timeout)` で受信。**Supervision tree**: 子アクタのクラッシュを親が監視・再起動。**Link/monitor**: `(cl-cc:link actor)` でアクタ間の死活監視。Erlang/OTP / Akka / Elixir GenServer
- **根拠**: 共有メモリ並行性（STM/lock-free）より推論が容易な並行モデル。CLのイメージベース開発（FR-364）とアクタのhot upgrade（FR-363）の統合が自然
- **難易度**: Hard

#### FR-599: Channel-Based Concurrency (チャネルベース並行性)

- **対象**: `src/concurrent/`, FR-387（グリーンスレッド）前提
- **現状**: グリーンスレッド間の通信抽象なし
- **内容**: `(cl-cc:make-channel :type fixnum :buffer 10)` でバッファ付きチャネル生成。`(cl-cc:send ch value)` / `(cl-cc:receive ch)` でブロッキング送受信。`(cl-cc:select (ch1 v1 ...) (ch2 v2 ...) (:default ...))` でノンブロッキング多重待機。**方向付きチャネル型**: `(send-channel fixnum)` / `(recv-channel fixnum)`。Go channels / Kotlin Channel / Rust mpsc + sync_channel / CSP（Hoare 1978）
- **根拠**: 共有状態なしの純粋メッセージパッシング。「メモリを共有するな。通信で共有せよ」のGoスタイル。STM（FR-523）との使い分けが明確
- **難易度**: Hard

#### FR-600: io_uring / kqueue Integration (非同期I/O統合)

- **対象**: `src/vm/io.lisp`, `src/concurrent/`, `src/cli/main.lisp`
- **現状**: `src/vm/io.lisp` はSBCLのブロッキングI/O呼び出し。イベント駆動I/Oなし
- **内容**: **Linux io_uring**: `io_uring_setup` / `io_uring_enter` syscallをFFI（FR-194）経由で呼び出し。`(cl-cc:async-read fd buffer callback)` → io_uring SQE投入。**macOS kqueue / kevent**: `kqueue()`+ `kevent()`でI/O多重化。`(cl-cc:async-accept socket callback)` でノンブロッキングacceptループ。async/await（FR-388）のバックエンドとしてio_uringを使用。**ゼロコピー**: `io_uring_prep_read_fixed` + registered buffers
- **根拠**: Linux io_uring（Jens Axboe, 2019〜）は2026年でシステムコールオーバーヘッドを1/10に削減。Node.js libuv / Tokio（Rust）が採用。ネットワークサーバー性能の決定要因
- **難易度**: Hard

#### FR-601: Reactive Streams / FRP (リアクティブストリーム)

- **対象**: 新規`src/reactive/`, FR-387（グリーンスレッド）+ FR-599（チャネル）前提
- **現状**: データパイプラインの組み合わせ抽象なし
- **内容**: `(cl-cc:observable (from-list '(1 2 3)) (map #'*2) (filter #'evenp) (reduce #'+ 0))` パイプライン。**バックプレッシャー**: 下流の処理速度に合わせて上流の生成を制御（`(cl-cc:throttle n)` / `(cl-cc:buffer n)`）。**冷たい/熱いObservable**: 遅延評価ストリームvs即時ブロードキャスト。**エラー伝播**: ストリーム内エラーのハンドリング（`(cl-cc:catch-error handler)`）。RxJava / Kotlin Flow / Reactor / Akka Streams
- **根拠**: コンパイラパイプライン自体がデータ変換ストリームであり（パース→展開→CPS→最適化）、Reactive Streamsで表現可能。バックプレッシャー制御でメモリ効率が良い
- **難易度**: Hard

#### FR-602: Structured Logging Macros (構造化ロギングマクロ)

- **対象**: 新規`src/compile/logging.lisp`, `src/expand/expander.lisp`
- **現状**: `format`ベースのフリーテキストログのみ。構造化ログ（JSON/ECS）の生成なし
- **内容**: `(cl-cc:log :info "request processed" :user-id user-id :duration-ms elapsed)` マクロ。コンパイル時に**キー名の重複・型チェック**（FR-533フォーマット検証との類比）。`--log-level=debug/info/warn/error`での動的フィルタ。無効ログレベルのログ呼び出しをコンパイル時に**ゼロコスト**に（dead-code elimination）。ECS（Elastic Common Schema）/ OpenTelemetry logs形式のJSON出力。Slog（Go 1.21）/ tracing（Rust）相当
- **根拠**: フリーテキストログは検索・集計が困難。構造化ログのコンパイル時型検査でキー名タイポを排除
- **難易度**: Medium

---

### Phase 111 — 組み込み・特殊ターゲット

#### FR-605: Bare Metal / No-OS Support (ベアメタル・OS不使用サポート)

- **対象**: `src/runtime/`, `src/cli/main.lisp`, `cl-cc.asd`
- **現状**: ランタイムはSBCLのOSサービス（mmap/mprotect等）に依存
- **内容**: `--target=x86-64-baremetal` / `--target=aarch64-none-elf` ターゲット追加。**POSIX依存ゼロ**: `write()`/`mmap()`/`pthread()`を使わない自己完結ランタイム。**ブートストラップコード**: x86-64向けMultiboot2ヘッダ + GDT/IDT初期化。**ページアロケータ**: 物理メモリマップからのヒープ初期化（E820/UEFI memory map）。`(declare (cl-cc:no-runtime-services))` で全OS依存をコンパイルエラー化
- **根拠**: Lisp Machine的な「OS上ではなくLisp上で動く」環境の構築。IoT/組み込みデバイスへのCL展開
- **難易度**: Very Hard

#### FR-606: No-Allocator Mode (割り当てゼロモード)

- **対象**: `src/runtime/heap.lisp`, `src/compile/codegen.lisp`
- **現状**: 全オブジェクトがGC管理ヒープに割り当てられる
- **内容**: `--no-allocator` フラグ: ヒープ割り当てを**コンパイルエラー**にする（FR-357 Allocation-free verificationの極端版）。スタック割り当て（FR-341エスケープ解析）・静的割り当て（グローバル変数）のみ許可。配列は`(make-static-array n)` で静的サイズのみ。固定サイズメモリプール（`(cl-cc:defpool point-pool point :size 1024)`）。RT-Linux / Zephyr RTOS / AUTOSAR向けコード生成
- **根拠**: 安全規格（ISO 26262 ASIL-D / IEC 61508 SIL4）でのGC使用禁止要件。自動運転・医療機器での使用
- **難易度**: Hard

#### FR-607: Partial Evaluation / Futamura Projections (部分評価・フタムラ投影)

- **対象**: `src/compile/pipeline.lisp`, `src/expand/expander.lisp`
- **現状**: 段階的コンパイル（FR-431）はユーザー記述のmulti-stageプログラム。自動部分評価なし
- **内容**: `(cl-cc:specialize interpreter program)` でインタープリタをプログラムで**自動部分評価**し特化版を生成（Futamura第一投影）。静的値（コンパイル時定数）に関する全分岐・ループを展開。**混合計算解析（Binding-Time Analysis）**: 各変数が静的（コンパイル時既知）か動的かを自動分類。混合計算モナド（BTA）による変換。Jones-Gomard-Sestoft自動部分評価器の実装
- **根拠**: cl-ccのVM（インタープリタ）+ CLプログラムでFutamura第一投影を実証できる。「cl-ccで書いたLISPをcl-ccがネイティブコードにコンパイルする」という再帰的な美しさ
- **難易度**: Very Hard

#### FR-608: Deforestation / Stream Fusion (中間データ構造除去)

- **対象**: `src/optimize/optimizer.lisp`, `src/expand/macros-sequence.lisp`
- **現状**: `(mapcar f (mapcar g lst))` が中間リストを生成してGCプレッシャー
- **内容**: **Deforestation**: `(mapcar f (mapcar g lst))` → `(mapcar (compose f g) lst)` への自動変換（中間リスト生成ゼロ）。**Stream fusion**: `(loop for x in list collect (f x))` の内部表現を`unfold/fold`型に変換してfusion可能化。`(reduce f (map g (filter p lst)))` の完全fusion。GHC Stream Fusion / Haskell rewrite rules / OCaml sequence fusion
- **根拠**: Lispの高階リスト操作は中間データ構造を大量生成。deforestationで`n`回の`mapcar`チェーンが1パスに統合でき、アロケーション数をn倍削減
- **難易度**: Hard

#### FR-609: Compiler-as-a-Library API (コンパイラAPIライブラリ)

- **対象**: `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: cl-ccはCLIのみ。プログラムから呼び出せるAPIなし（`compile-expression`は内部用）
- **内容**: **Public Compiler API**: `(cl-cc:compile-form form &key optimize-level target)` → `vm-program`。`(cl-cc:compile-file path &key ...)` → binary。`(cl-cc:parse-form str)` → `ast-node`。`(cl-cc:expand-macros form)` → expanded-form。**IR construction API**: `(cl-cc:make-ir-function name params body)` でプログラマティックなIR構築。LSP（FR-319）・IDEプラグイン（FR-398）・REPL（FR-312）のバックエンドとして統合。Clang libclang / LLVM C API / Roslyn API
- **根拠**: 外部ツールがcl-ccの解析・変換能力を利用できるようにする。静的解析ツール・フォーマッタ・リンタのすべてがこのAPIを使える
- **難易度**: Medium

#### FR-610: Adaptive Recompilation (適応的再コンパイル)

- **対象**: `src/jit/`, `src/compile/pipeline.lisp`
- **現状**: 各関数は一度だけTier-1→Tier-2にコンパイルされ、以後変化なし
- **内容**: **実行プロファイルの継続収集**: Tier-2でも型フィードバック（FR-559）を継続収集。型分布の**ドリフト検出**（初期プロファイルと現在の差異が閾値超過）。ドリフト検出時に新しいプロファイルに基づいてTier-2コードを**再コンパイル**（古いコードへのデオプトを経由）。long-running server processでのJVM C2最適化同様の動作。Graal/GraalVM Adaptive Compilation / JVM Adaptive Optimization
- **根拠**: 長時間実行プロセスでは実行パターンが変化する（スタートアップvs安定稼働）。初期プロファイルに基づくJITコードが後に非最適になる問題を解決
- **難易度**: Hard

#### FR-611: Persistent Data Structures (永続データ構造)

- **対象**: 新規`src/data/persistent.lisp`
- **現状**: CLの標準データ構造は全てmutable。不変データ構造の共有コピーなし
- **内容**: **Persistent Vector** (RRB-Tree: Bagwell 2012): `(cl-cc:pvec-assoc vec idx val)` がO(log₃₂n)でコピーオンライト。**Persistent Hash Map** (HAMT: Hash Array Mapped Trie): `(cl-cc:pmap-assoc map key val)`。**Structural sharing**: 変更されたパスのみ新ノードを作成し残りを共有。STM（FR-523）のトランザクション内でpersistent DSを使うと競合なしに並行更新可能。Clojure persistent collections / Scala Vector / Haskell Map
- **根拠**: 純粋関数型スタイル・STM・並行プログラミングの基盤。コピーオンライトより効率的（O(log n) vs O(n)）
- **難易度**: Hard

#### FR-612: Hash Consing (ハッシュコンシング)

- **対象**: `src/runtime/heap.lisp`, `src/vm/vm.lisp`
- **現状**: `(cons a b)` は常に新規ヒープオブジェクト。同一内容のconsが複数存在
- **内容**: **正準化テーブル**: `cons`/`list`/CLOSオブジェクト生成時に内容ハッシュで既存オブジェクトを検索。同一内容が存在すれば新規割り当てなしで既存を返す。`(eq (list 1 2 3) (list 1 2 3))` → `t`（通常は`nil`）。`(cl-cc:with-hash-consing ...)` スコープで有効化。`(declare (cl-cc:hashcons))` で個別構造体に適用。Lisp-1.5の歴史的機能 / Guile hash-consing / BDD実装
- **根拠**: コンパイラ内部の型表現・AST共有で大幅なメモリ削減効果。同一型シグネチャが何千も重複生成される最適化パスに有効
- **難易度**: Medium

---

### Phase 95 — 高度デバッグ・バイナリ最適化

#### FR-507: Time-Travel Debugging / Record-Replay (タイムトラベルデバッグ)

- **対象**: `src/vm/vm-run.lisp`, `src/cli/main.lisp`
- **現状**: VM 実行は前向きのみ。デバッガ（FR-319 LSP DAP）はステップ実行可能だが後退実行不可。セルフホスティング中のハイゼンバグを「発生直前まで巻き戻す」手段がない
- **内容**: VM の全命令実行を **execution log** に記録（命令インデックス・レジスタ差分・ヒープ書き込みアドレス）。`./cl-cc run --record foo.lisp` でログを `foo.clcc-trace` に保存。デバッガで `step-back` / `reverse-continue` コマンドでログを逆走させて過去の VM 状態を再現。Debug Adapter Protocol（DAP）の `reverseContinue` / `stepBack` リクエストに対応することで VSCode 等から透過的に利用可能
- **根拠**: Mozilla rr（Linux ptrace ベース）/ WinDbg TTD / UDB（Undo Software）。cl-cc 自身のセルフホスティングデバッグでの実用価値が高い。ハイゼンバグ解析の唯一の確実な手段
- **難易度**: Hard

#### FR-508: Post-Link Binary Layout Optimization (プロファイル駆動バイナリ再配置)

- **対象**: `src/binary/macho.lisp`, `src/backend/x86-64-codegen.lisp`, `src/cli/main.lisp`
- **現状**: FR-036（Hot/Cold レイアウト）と FR-186（関数並べ替え）は静的ヒューリスティック。実行プロファイルに基づくバイナリレイアウト最適化なし
- **内容**: `./cl-cc compile --instrument` で関数・基本ブロック単位の実行カウンタを埋め込んだバイナリ生成。`./cl-cc run foo` でプロファイルデータ（`foo.clcc-profile`）を収集。`./cl-cc optimize foo.clcc-profile foo` でホット関数を `.text.hot` 先頭に集約・コールドコードを `.text.cold` に分離してリンク。TLB / I-cache スラッシングを削減。BOLT（Meta 2018）/ Google Propeller と同等の手法
- **根拠**: BOLT paper: HHVM に適用して 7〜20% スループット向上。`./cl-cc selfhost` が現実的なプロファイルワークロードとなる
- **難易度**: Very Hard

---

### Phase 112 — コードサイズ・電力最適化

#### FR-615: Code Size Optimization Mode / -Os / -Oz (コードサイズ最適化モード)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`, `src/cli/main.lisp`
- **現状**: 最適化は速度優先。`-O2` 相当の最適化が唯一の選択肢。コードサイズを犠牲にする展開（ループアンローリング・インライン化・アウトライン抑制）の制御不可
- **内容**: `--optimize-for size` フラグを追加。`-Os`モード: インライン化コスト閾値を大幅に引き下げ（≤8命令のみ）、ループアンローリングを無効化、重複コードをアウトライン化（FR-042逆適用）。`-Oz`モード: さらに圧縮命令優先（x86-64の短いエンコーディング: `xor rax, rax` vs `mov rax, 0`）、ホットパス外の定数畳み込みを抑制。`(declare (cl-cc:optimize-for :size))` で関数単位適用
- **根拠**: 組み込みターゲット（FR-605 Bare Metal）や WASM バイナリサイズ削減に直結。LLVM -Os/-Oz / GCC -Os の標準的実践
- **難易度**: Medium

#### FR-616: Function Multi-Versioning / CPU Feature Dispatching (CPU機能ディスパッチ)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/compile/codegen.lisp`, `src/binary/macho.lisp`
- **現状**: コンパイル時に固定の命令セットを選択。実行時 CPU に依存した命令（AVX-512 / AVX2 / SSE4.2）の動的選択不可
- **内容**: `(defun foo (x) (declare (cl-cc:target-versions (:avx512 :avx2 :baseline))) ...)` でマルチバージョン関数を生成。コンパイラが3つのコード片を出力し、起動時 `cpuid` 結果でポインタを書き換える IFUNCリゾルバ（ELF `STT_GNU_IFUNC` / Mach-O `__TEXT.__stubs` resolver）を挿入。GNU `__attribute__((target_clones(...)))` / LLVM `@llvm.x86.cpuid` と同等
- **根拠**: FFT / BLAS カーネルで最大3倍の性能差。実行バイナリが対象 CPU のフル能力を引き出せる
- **難易度**: Hard

#### FR-617: Energy-Aware Compilation (エネルギー認識コンパイル)

- **対象**: `src/optimize/optimizer.lisp`, `src/cli/main.lisp`
- **現状**: 最適化目標は実行速度のみ。エネルギー消費・電力効率の観点なし
- **内容**: `--optimize-for energy` モード追加。電力コストモデル（各命令の average power consumption 表を内蔵: 除算≫乗算≫加算）を参照した命令選択。高電力命令（64-bit IDIV, FP sqrt）の代替列への置換。P-coreとE-coreで異なるコードパスを生成するhybrid-CPU awareness。`(declare (cl-cc:power-budget 5))` で最大5ワット制約を宣言的に記述。ARM Neoverse N1 / Intel Hybrid architecture 対応
- **根拠**: データセンター電力コスト削減（10%省電力 ≈ 数億円/年）。モバイル・IoT バッテリー寿命最大化。Green Software Foundation の2026年標準要件
- **難易度**: Hard

#### FR-618: Dead Argument Elimination / DAE (デッド引数除去)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: 関数引数は宣言された通りに全て渡す。呼び出し側で定数が渡される引数・内部で未使用の引数もレジスタ/スタック占有
- **内容**: LTO スコープ内（FR-040）で全呼び出しサイトを解析。引数 `x` が（a）全呼び出しサイトで同一定数 → クローン関数に定数埋め込み、（b）全サイトで未使用 → シグネチャから削除。`make-register-frame` のレジスタ保存コスト削減。LLVM `DeadArgumentElimination` パスと同等。スペシャライズ版関数は元関数のラッパーとしてABI維持
- **根拠**: LTOとIPCPの補完。コール規約のオーバーヘッド削減。selfhostingコードでの`(defun %compile-ast (ast env ... unused-debug-ctx))` のような内部関数に効果
- **難易度**: Medium

---

### Phase 113 — ポリヘドラル・自動並列化

#### FR-620: Polyhedral Model Optimization (多面体モデル最適化)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: ループ最適化はループアンローリング・LICM（FR-031）・ループ交換（FR-360）の個別パスのみ。ネストしたループの連携最適化なし
- **内容**: `do`/`loop`マクロが生成するネストループをStaticControl Part (SCoP)として検出。整数線形計画法ベースの依存解析（ISL: Integer Set Library相当を純CLで実装）でデータ依存グラフを構築。Plutoアルゴリズムでループタイリング・ループ融合・ループ分散・ループスキューイングを自動適用。生成コードはFR-622 SLPと連携してSIMD化
- **根拠**: 行列乗算・画像処理・数値シミュレーションで10〜100倍の性能向上。LLVM Polly / GCC Graphite / Pluto の手法。FFT実装の自動最適化に直結
- **難易度**: Very Hard

#### FR-621: Auto-Parallelization (自動並列化)

- **対象**: `src/optimize/optimizer.lisp`, `src/vm/vm.lisp`, `src/vm/vm-execute.lisp`
- **現状**: `cl-cc`のループは全てシリアル実行。FR-576（Work-Stealing Scheduler）は並行タスクAPIを提供するが自動適用なし
- **内容**: SCoP解析（FR-620）の依存グラフが並列化可能な外側ループを特定。ループ本体をFR-576のワークスティーリングタスクに自動変換。`(declare (cl-cc:parallel))` アノテーションで手動ヒント。依存違反時はコンパイル時警告。ループ繰り返し数が閾値未満（<1024）の場合はシリアル実行にフォールバック。OpenMP `#pragma omp parallel for` の意味論と同等
- **根拠**: マルチコア CPU を自動活用。並列化対応のループコードをユーザーが明示的に書く必要なし
- **難易度**: Very Hard

#### FR-622: Superword-Level Parallelism / SLP (スーパーワードレベル並列性)

- **対象**: `src/optimize/optimizer.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: SIMD自動ベクトル化（FR-035）はループ全体を対象。ループ外の隣接スカラー演算のパック化なし
- **内容**: 基本ブロック内の独立スカラー演算列（`(+ a0 b0)` `(+ a1 b1)` `(+ a2 b2)` `(+ a3 b3)` → `vaddps xmm0, ...`）を検出し128/256bit SIMD命令にパック。データ型整合・アライメント確認・レジスタ割り当て統合。LLVM SLPVectorizer / GCC SLP と同等。FR-035 LoopVecで対応できない非ループコード（構造体フィールド初期化など）に効果
- **根拠**: 3D座標演算・カラー処理・CLMS行列要素初期化など、展開後の隣接演算に広く適用可能
- **難易度**: Hard

#### FR-623: Loop Fusion (ループ融合)

- **対象**: `src/optimize/optimizer.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: 複数の独立したループが同一配列を順次処理する場合、それぞれ別個にコンパイル。キャッシュ再利用機会を逃す
- **内容**: 連続する2つのループが同一イテレーション空間かつデータ依存がない場合、ループ本体を統合（`(dotimes (i n) (setf (aref a i) ...)) (dotimes (i n) (setf (aref b i) ...))` → 1ループ）。FR-620 SCoP依存グラフで合法性確認。融合後の中間配列が消滅する場合はFR-608 Deforestationと協調。`map`/`mapcar`連鎖の融合（stream fusion）も対象
- **根拠**: キャッシュラインの再利用でL1ヒット率向上。ループオーバーヘッド（カウンタ更新・分岐）削減。GHC deforestation / Haskell stream-fusion の Lisp版
- **難易度**: Medium

---

### Phase 114 — マルチレベルIR・形式検証

#### FR-626: Multi-Level IR / MLIR-Style Dialect Lowering (多段階IR・段階的降下)

- **対象**: `src/emit/mir.lisp`, `src/emit/target.lisp`, `src/compile/codegen.lisp`
- **現状**: コンパイルパイプラインはAST → VM instructions → x86-64の2段階変換。中間IRが固定的で新ターゲット追加時の再利用困難
- **内容**: MIR層を拡張して「Dialect」概念を導入。`cl-cc.affine` dialect（ループ/アフィン演算）→ `cl-cc.arith` dialect（スカラー演算）→ `cl-cc.llvm` / `cl-cc.x86` dialectの段階的降下パイプライン。各dialect間の変換パターンを`define-dialect-conversion`マクロで宣言。MLIR（LLVM 2020）のConversionPatternRewriter と同等の設計。WASM・GPU・AArch64ターゲットが同一フロントエンドから派生可能
- **根拠**: MLIR paper（PLDI 2020）: 言語固有最適化を適切な抽象レベルで適用可能。ターゲット追加コストを指数→線形に削減
- **難易度**: Very Hard

#### FR-627: Formal Verification Integration / Coq-Lean Extraction (形式検証統合)

- **対象**: `src/type/`, `src/compile/codegen.lisp`, `src/cli/main.lisp`
- **現状**: 型システム（FR-type系）は型安全性を保証するが、プログラムの機能的正しさは検証不可
- **内容**: `(defun foo (x) (declare (cl-cc:spec (-> (and integer (>= 0)) integer))) ...)` で前後条件を宣言。コンパイラが仕様をSMTソルバー（FR-246 Z3統合）に送出して検証。検証失敗時はコンパイルエラー。`--emit-lean` フラグでLean 4証明ターゲットに変換（F*スタイル）。副作用追跡（FR-244 Effect System）と統合してIOを型レベルで分離
- **根拠**: CompCert（INRIA）: 検証済みCコンパイラ。セキュリティクリティカルコード（暗号・OS kernel）の信頼性向上。cl-ccコンパイラ自身の健全性検証
- **難易度**: Very Hard

#### FR-628: Proof-Carrying Code / PCC (証明付きコード)

- **対象**: `src/binary/macho.lisp`, `src/vm/vm-execute.lisp`, `src/type/`
- **現状**: 生成バイナリの安全性は動的チェック（境界検査・型タグ）に依存。ロード時の型安全性証明なし
- **内容**: バイナリに型証明（proof term）を埋め込む。VM実行前に証明検証器（proof checker）が証明を検査し、メモリ安全性・型安全性・制御フロー整合性を静的に確認。`--emit-pcc` フラグでPCC付きバイナリ生成。Foundational PCC（Appel & Felten 1999）/ TAL（Typed Assembly Language）の手法。信頼ベースを最小化（証明検証器のみ信頼）
- **根拠**: 動的チェックオーバーヘッドをゼロに近づけながら安全性を維持。プラグインシステムで未検証コードのロードを拒否するセキュリティモデル
- **難易度**: Very Hard

#### FR-629: Certified Compilation / Bisimulation Proofs (認証済みコンパイル)

- **対象**: `src/compile/codegen.lisp`, `src/compile/cps.lisp`, `src/optimize/optimizer.lisp`
- **現状**: コンパイル変換の正しさはテストで担保（4322テスト）。変換の意味論的等価性の形式的証明なし
- **内容**: 主要変換（CPS変換・クロージャ変換・レジスタ割り当て）に対してシミュレーション関係を定義。`--verify-transform` モードで変換前後の意味論をランダムプログラムで差分テスト（FR-453 Differential Testing の強化版）。CompCert の verification methodology に準拠。将来的にCoq/Leanで変換正当性証明を形式化
- **根拠**: コンパイラのバグが最も発見困難なバグ（最適化による意味変化）。認証済みバックエンドはセキュリティコンパイル保証の基盤
- **難易度**: Very Hard

---

### Phase 115 — FFI・クロスコンパイル

#### FR-632: FFI Binding Generation / Bindgen (FFIバインディング自動生成)

- **対象**: `src/cli/main.lisp`, 新規 `src/ffi/bindgen.lisp`
- **現状**: C関数呼び出しは手動で`(cl-cc:foreign-call "printf" :int :string)` と記述。Cヘッダーからの自動生成なし
- **内容**: `./cl-cc bindgen foo.h` でCヘッダーを解析し、cl-cc外部関数宣言を自動生成。C型→cl-cc型マッピング（`int*` → `(cl-cc:ptr cl-cc:int32)`）。`struct`定義 → `defstruct`生成。`enum` → `defconstant`群生成。libclang / tree-sitter-cで構文解析。`--with-header /usr/include/stdio.h` でシステムヘッダー対応。Rust bindgen / CFFI (Common Lisp) と同等
- **根拠**: POSIX API・OpenSSL・GTK等のライブラリを手動FFI記述なしに利用可能。エコシステム拡張の加速
- **難易度**: Hard

#### FR-633: Cross-Compilation Toolchain (クロスコンパイルツールチェーン)

- **対象**: `src/cli/main.lisp`, `src/backend/`, `src/binary/`
- **現状**: ホスト環境（macOS/Linux x86-64）向けにのみコンパイル可能。`--target` フラグはアーキテクチャ切り替えのみ
- **内容**: `--target triple` 形式（`aarch64-linux-musl`, `x86_64-windows-gnu`, `riscv64-linux-gnu`）で完全なクロスコンパイルを実現。Sysroot管理（`--sysroot /path/to/arm-sysroot`）。クロス用リンカスクリプト（ELF/PE/Mach-O）の自動選択。標準ライブラリのターゲット向けビルド済みアーカイブ配布。Nix flake との統合（`devenv.nix` の `cross.aarch64-linux`）
- **根拠**: Raspberry Pi・組み込み Linux・Windows バイナリを開発機から直接生成。CI/CD での多ターゲットリリース自動化
- **難易度**: Hard

#### FR-634: Custom Calling Conventions (カスタム呼び出し規約)

- **対象**: `src/compile/codegen.lisp`, `src/backend/x86-64-codegen.lisp`, `src/vm/vm-execute.lisp`
- **現状**: 全関数が統一的なvm-call規約（レジスタ保存・引数スタック）を使用。呼び出しオーバーヘッド削減の余地あり
- **内容**: `(defun hot-fn (a b) (declare (cl-cc:calling-convention :register-only)) ...)` でカスタム規約指定。`:register-only`: 全引数をレジスタ渡し（R0-R7）、呼び出し側保存なし（leaf関数前提）。`:tailcall-optimized`: TCO専用規約（継続渡しスタイル）。`:interrupt-handler`: 全レジスタ保存（ISRルーティン用）。LLVM `cc` attribute / GCC `__attribute__((regparm))` 相当。FR-618 DAEと協調
- **根拠**: JITコンパイルされたホットパスで呼び出しオーバーヘッドを90%削減可能。コンパイラ内部の再帰関数（CPS変換・コード生成）に即座に適用可能
- **難易度**: Hard

#### FR-635: COMDAT Deduplication (COMDATセクション重複除去)

- **対象**: `src/binary/macho.lisp`, `src/emit/target.lisp`
- **現状**: テンプレート/generic関数の複数インスタンスが別々のコンパイル単位に重複して存在する場合、リンク時に全コピーを保持
- **内容**: 関数・データを COMDAT グループ（ELF `SHF_GROUP` / Mach-O `S_ATTR_NO_DEAD_STRIP` + `.weak_definition`）に配置。リンカが同名 COMDAT グループを1コピーに統合。`(defun foo (x) (declare (cl-cc:comdat :any)) ...)` で明示指定。ジェネリック関数のspecializer組み合わせ爆発を防止。FR-582 ICFの補完（ICFは内容一致、COMDATは名前一致）
- **根拠**: C++テンプレートと同様に、cl-ccのgeneric function specializer重複によるバイナリ肥大化を防止
- **難易度**: Medium

---

### Phase 116 — 自動微分・ハードウェア並行

#### FR-638: Automatic Differentiation / AD (自動微分)

- **対象**: 新規 `src/ad/forward.lisp`, `src/ad/reverse.lisp`, `src/compile/codegen.lisp`
- **現状**: 数値微分は手動実装のみ。機械学習・最適化アルゴリズムへの対応なし
- **内容**: `(cl-cc:grad f)` でスカラー関数`f`の微分関数を返す。**前向きモード（ForwardAD）**: 双数（dual numbers `(value . derivative)`）を使ったオペレータオーバーロード。**逆向きモード（ReverseAD）**: 計算グラフをテープに記録しバックプロパゲーション。`(cl-cc:jacobian f)` でヤコビアン行列計算。`(declare (cl-cc:differentiable))` でAD対応関数を宣言。JAX-style transformations: `grad`, `jvp`, `vjp`, `hessian`
- **根拠**: 機械学習ライブラリ（FR-597 ML-Guided Inlining）の内部実装基盤。科学計算・最適化ソルバーへの応用。Enzyme（LLVM AD）/ JAX / Zygote（Julia）の手法
- **難易度**: Hard

#### FR-639: Hardware Transactional Memory / HTM (ハードウェアトランザクショナルメモリ)

- **対象**: `src/vm/vm-execute.lisp`, `src/vm/conditions.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: FR-165（STM Software Transactional Memory）は純ソフトウェア実装。Intel TSX / ARM TME ハードウェア命令未活用
- **内容**: `(cl-cc:with-htm ...)` マクロがhardware transactionを試行。Intel `XBEGIN`/`XEND`/`XABORT` (RTM) または ARM `TSTART`/`TCOMMIT`/`TCANCEL` を発行。中断時はFR-165 STMソフトウェア実装にフォールバック。競合率が高い場合は自動的にロック実装に降格。トランザクション中のIO・syscall・例外はコンパイル時に禁止
- **根拠**: ロックフリーデータ構造（FR-163）より高いスループット。Intel TSX搭載CPU（Haswell以降）で投機的並行性を直接活用
- **難易度**: Hard

#### FR-640: NUMA-Aware Memory Allocation (NUMA対応メモリ割り当て)

- **対象**: `src/runtime/heap.lisp`, `src/runtime/gc.lisp`, `src/vm/vm-execute.lisp`
- **現状**: ヒープ割り当ては単一アリーナ（FR-228 arena allocator）。NUMA topologyを無視した割り当てでリモートメモリアクセスが発生
- **内容**: 起動時に `/sys/devices/system/node/` (Linux) / IOKit NUMA API (macOS) でNUMAトポロジーを取得。スレッド（FR-576 work-stealing）がローカルNUMAノードのアリーナから優先的に割り当て。GCスキャンもNUMAノード単位で並列化。`numactl` / `mbind` / `set_mempolicy` syscall を活用。`(declare (cl-cc:numa-local))` で関数内割り当てをローカルノードに固定
- **根拠**: 64コア以上のサーバーでNUMA効果が顕著（リモートアクセス: 2〜4倍遅延）。大規模コンパイルジョブでのメモリスループット改善
- **難易度**: Hard

#### FR-641: Transparent Huge Pages / THP (透明な大ページ)

- **対象**: `src/runtime/heap.lisp`, `src/binary/macho.lisp`
- **現状**: 標準 4KB ページでメモリ割り当て。JITコードおよびヒープが多数のTLBエントリを消費
- **内容**: JITコードバッファ（FR-060）と大規模ヒープアリーナ（>2MB）を `mmap(MAP_HUGETLB)` / `madvise(MADV_HUGEPAGE)` で 2MB huge page に配置。macOS では `MAP_JIT | MAP_SHARED` + `vm_map` の適切なアライメント。TLBミス率をperfカウンタで計測し、big-enough ヒープのみに適用（小アリーナはオーバーヘッドが逆効果）。`--huge-pages` フラグで制御
- **根拠**: JITコードとヒープで TLB miss が最も多い。MongoDB/JVM で THP 有効化により 10-15% スループット向上の実績
- **難易度**: Medium

---

### Phase 117 — 新興ターゲット

#### FR-644: WASM GC Proposal Support (WASMガベージコレクション提案対応)

- **対象**: `src/backend/wasm.lisp`, `src/emit/target.lisp`
- **現状**: FR-049（WASM backend）は線形メモリモデルのみ。WASM GC提案（struct/array型）未対応
- **内容**: WASM GC提案（W3C 2024勧告）の `struct.new`, `array.new`, `ref.cast`, `extern.convert_any` 命令を活用。cl-ccのCLOSオブジェクトをWASM `struct` 型にマッピング。リスト/配列を WASM `array` 型に変換。ブラウザ内蔵GCに管理を委譲（cl-ccのカスタムGCが不要）。V8/SpiderMonkey/JavaScriptCore のWASM GC実装と互換。`--target wasm-gc` フラグで有効化
- **根拠**: WASM GCにより生成バイナリサイズが30-50%削減（手製メモリ管理コード不要）。ブラウザでの cl-cc プログラム実行の現実化
- **難易度**: Hard

#### FR-645: GPU Compute Kernel Compilation (GPUコンピュートカーネルコンパイル)

- **対象**: 新規 `src/backend/gpu-kernel.lisp`, `src/emit/target.lisp`
- **現状**: FR-053（SPIR-V backend）は基本的な shader 生成。CUDA/ROCm/Metal Compute の高水準抽象なし
- **内容**: `(cl-cc:defkernel matrix-mul ((a :buffer) (b :buffer) (c :buffer :write)) ...)` でGPUカーネルを定義。コンパイラがSPIR-V（Vulkan Compute）/ PTX（NVIDIA CUDA）/ AIR（Apple Metal）/ ROCm LLVM IRへのコード生成。自動的なthread block / workgroup サイズ選択。スカラー演算のwarp-levelベクトル化。`cl-cc:launch-kernel` でホストからディスパッチ。oneAPI Level Zero / Vulkan Compute API との統合
- **根拠**: AI推論・科学計算・コンパイラ内部の並列最適化パスをGPUオフロード。M1/M2 Mac のunified memory architectureでとくに効果的
- **難易度**: Very Hard

#### FR-646: FPGA High-Level Synthesis / HLS (FPGA高水準合成)

- **対象**: 新規 `src/backend/fpga-hls.lisp`, `src/emit/target.lisp`
- **現状**: ターゲットはCPU/GPU/WASM。FPGA向け合成パスなし
- **内容**: `(defun foo (a b) (declare (cl-cc:target :fpga-hls)) ...)` でFPGA合成対象を指定。コンパイラがCIRCT（Circuit IR Compiler Tools）互換のHLS IRを出力。パイプライン化・II（Initiation Interval）最適化・リソース共有の制御。制御フロー（if/case）をMux/FSMに変換。`(declare (cl-cc:ii 1))` でII=1（毎サイクル新入力受付）を指定。Vivado HLS / Intel HLS Compiler / Bambu と同等
- **根拠**: FPGA上での高性能DSP・ネットワーク処理・暗号化アクセラレータを高水準言語で記述可能
- **難易度**: Very Hard

#### FR-647: NPU / ML Accelerator Code Generation (NPU/MLアクセラレータコード生成)

- **対象**: 新規 `src/backend/npu-codegen.lisp`, `src/emit/target.lisp`
- **現状**: FR-645でGPUカーネル対応。専用ML推論アクセラレータ（Apple Neural Engine / Google TPU / Qualcomm HTP）未対応
- **内容**: `(cl-cc:defnn-op conv2d ...)` でニューラルネットワーク演算を宣言的に定義。コンパイラがMLIRの`linalg`/`tosa`/`nnef` dialectを経由してTFLite FlatBuffer / CoreML Model / QNN SDK 形式に降下。フュージョン（Conv+BN+ReLUを1カーネルに）・量子化（fp32→int8）・テンソルレイアウト変換（NCHW↔NHWC）を自動適用。Apple ANE（Core ML）/ Google Edge TPU / Hexagon DSP 対応
- **根拠**: エッジAI推論（FR-597 ML-Guided Inlining の実行デバイス）の電力効率が GPU比10〜100倍。2026年以降の主要デプロイ先
- **難易度**: Very Hard

---

### Phase 118 — パーサ高度化・多言語

#### FR-650: Incremental Parsing / Error-Resilient Parser (インクリメンタルパーシング)

- **対象**: `src/parse/cl/parser.lisp`, `src/parse/lexer.lisp`
- **現状**: ソース全体を再解析。1文字変更でもAST全再構築。LSP補完（FR-070）でのレスポンス遅延の原因
- **内容**: Tree-sitter スタイルのインクリメンタルパーシング。変更されたソース範囲のみを再解析し既存ASTと差分マージ。`edit-tree(old-tree, start-byte, old-end-byte, new-end-byte)` APIを提供。変更範囲を `(changed-ranges old new)` で取得し、LSPサーバーがインクリメンタルにセマンティクス更新。タイピング中の未完成コードでも部分ASTを生成（エラーノード`ast-error`で表現）
- **根拠**: Tree-sitter（GitHub 2018）: エディタ組み込みパーシングの標準。LSPレスポンスを O(ファイルサイズ) から O(変更サイズ) に削減
- **難易度**: Hard

#### FR-651: Error Recovery in Parsing (パーシングエラー回復)

- **対象**: `src/parse/cl/parser.lisp`, `src/parse/lexer.lisp`
- **現状**: パース中の構文エラーで解析中断。以降のエラーが全て報告されない（最初のエラーで止まる）
- **内容**: **パニックモード回復**: エラー発生後、同期トークン（`(`, `)`, `defun`, `defvar`）まで入力を読み飛ばして解析継続。**エラープロダクション**: `(if cond then)` のような不完全フォームをエラーASTノードとして保持し後続解析継続。`(cl-cc:parse-resilient "...")` がエラーリストと部分ASTの両方を返す。LSP（FR-070）との統合でエディタが複数エラーを一括表示
- **根拠**: `gcc`/`clang` は10個以上のエラーを同時報告。cl-ccの現在の「最初のエラーで停止」動作は開発体験を著しく損なう
- **難易度**: Medium

#### FR-652: Polyglot Compilation / Multi-Language Interop (多言語コンパイル)

- **対象**: `src/cli/main.lisp`, `src/ffi/`, 新規 `src/polyglot/`
- **現状**: cl-ccソースのみ処理。他言語との相互運用はCFFI手動記述のみ
- **内容**: `./cl-cc build --polyglot project.toml` で混合言語プロジェクトのビルド。サポート言語: C（clang経由）、Python（CPython C-API）、Rust（cargo build → `.a` リンク）。各言語の型 → cl-cc型の自動変換スキーム。GraalVM polyglot / Nix mkDerivation との統合。`(cl-cc:import-from :python "numpy" :as numpy)` のような高水準インポート構文
- **根拠**: 実世界のシステムは単一言語で構成されない。科学計算（Python/NumPy）・システムライブラリ（C/Rust）との協調が必須
- **難易度**: Hard

#### FR-653: Lazy Evaluation / Call-by-Need (遅延評価・必要時呼び出し)

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`, `src/vm/vm-execute.lisp`
- **現状**: 全式が正格評価（call-by-value）。無限リスト・遅延シーケンスの表現不可
- **内容**: `(cl-cc:lazy expr)` でサンク（thunk）生成。初回 `(cl-cc:force thunk)` でeval・結果をメモ化（以後同値返却）。`(cl-cc:delay-seq ...)` で無限遅延シーケンス生成。`define-lazy-syntax` マクロで`and`/`or`/`if`の短絡評価をlazy semanticsで再定義可能。コンパイラがサンクを値タグで表現しforceで自動透過。Haskell thunks / Scheme `delay`/`force` / SRFI-41 と同等
- **根拠**: 無限リスト（素数の無限列・フィボナッチ数列）の自然な表現。短絡評価マクロの透明な実装基盤
- **難易度**: Medium

---

### Phase 119 — 契約・リフレクション・合成

#### FR-656: Contract Programming / Design by Contract (契約プログラミング)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/compile/codegen.lisp`, `src/type/`
- **現状**: `assert`マクロは実行時検査のみ。前提条件・事後条件・不変条件の宣言的記述と静的検証なし
- **内容**: `(defun divide (x y) (require (not (zerop y)) "y≠0") (ensure (numberp result) "result is number") ...)` でEiffel/Racket-styleの契約を記述。**静的検証**: コンパイル時にSMTソルバー（FR-246）で検証試行。**動的検証**: `--contracts :check` で実行時検査有効化、`--contracts :none` で本番無効化。クラス不変条件: `(definvariant stack-class (>= (length items) 0))`。Racket Contracts / CLIM preconditions / Cofoja（Java）と同等
- **根拠**: cl-ccコンパイラ内部の不変条件（CPS変換後の継続ノード構造など）を宣言的に検証可能。バグの早期発見
- **難易度**: Medium

#### FR-657: Runtime Reflection API (実行時リフレクションAPI)

- **対象**: `src/vm/vm-execute.lisp`, `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **現状**: `class-of` / `type-of` は基本的な型情報のみ返す。メソッド一覧・スロット名・ソース位置の実行時取得なし
- **内容**: `(cl-cc:reflect:methods obj)` でオブジェクトの全適用可能メソッドを返す。`(cl-cc:reflect:slots class)` でスロット名/型/初期値のリスト。`(cl-cc:reflect:source-location fn)` でソースファイル・行番号。`(cl-cc:reflect:disassemble fn)` でVM命令列を返す（デバッグ用）。`(cl-cc:reflect:type fn)` で推論済み型シグネチャ取得。コンパイラが`--emit-reflection-metadata`フラグでメタデータセクションをバイナリに埋め込み
- **根拠**: REPL・デバッガ・テストフレームワーク・シリアライザが内省APIに依存。現在のFiveAMテストがメソッドを動的発見できない制約の解消
- **難易度**: Medium

#### FR-658: Program Synthesis / CEGIS (プログラム合成・反例誘導合成)

- **対象**: `src/cli/main.lisp`, 新規 `src/synthesis/cegis.lisp`
- **現状**: プログラムは手動記述のみ。仕様から実装を自動導出する機能なし
- **内容**: `(cl-cc:synthesize (fn (integer) integer) :spec (lambda (n result) (= result (* n n))))` で仕様から関数を合成。**CEGIS**: 候補プログラムを生成 → SMTで反例確認 → 反例を制約追加 → 再合成のループ。スケッチ（`cl-cc:??` でホール）: `(+ x (cl-cc:?? integer))` の `??` を合成。Sketching（Armando Solar-Lezama 2006）/ Rosette（Racket）/ SyGuS 競技形式と同等。合成スコープは純関数（副作用なし）に限定
- **根拠**: テストケース（入出力例）から関数実装を自動生成。小規模ユーティリティ関数の自動記述
- **難易度**: Very Hard

#### FR-659: Copy-on-Write Semantics (コピーオンライトセマンティクス)

- **対象**: `src/vm/vm-execute.lisp`, `src/vm/list.lisp`, `src/runtime/heap.lisp`
- **現状**: `copy-list` / `copy-seq` は常に即時コピー。大規模データ構造の不要なコピーでメモリ使用量増大
- **内容**: `(cl-cc:cow-copy obj)` でCOW参照を作成（実際のコピーは書き込み時まで遅延）。内部的に参照カウント + `is-shared` フラグで管理。配列/文字列への書き込み時に `copy-on-write-check` を自動挿入（コンパイラが検出）。`(declare (cl-cc:cow))` スコープで暗黙的COW有効化。FR-611（Persistent Data Structures）との相違: COWはミュータブル操作を透過的に最適化、PDSは不変性を保証
- **根拠**: 文字列処理・大規模リストのスライシング・フォークベースの並行処理でメモリ効率が大幅改善。Unix fork の `MAP_PRIVATE` と同等のセマンティクス
- **難易度**: Medium

---

### Phase 120 — ビルド信頼性・診断

#### FR-662: Reproducible Builds (再現可能ビルド)

- **対象**: `src/cli/main.lisp`, `src/compile/codegen.lisp`, `src/binary/macho.lisp`
- **現状**: 生成バイナリにビルド時刻・ホスト情報が埋め込まれる。同一ソースから異なるバイナリが生成されサプライチェーン検証不可
- **内容**: `--reproducible` フラグで完全決定論的ビルドを保証。タイムスタンプを固定値（`SOURCE_DATE_EPOCH`環境変数）に置換。ハッシュマップ・セットの反復順序を決定論的に固定（ソートキーを使用）。デバッグ情報のパスを `--remap-path-prefix` で正規化。コンパイル並列処理の順序をトポロジカルソートで固定。Reproducible Builds project（Debian/NixOS）との互換性。`./cl-cc build --check-reproducible` で2回ビルドしてSHA256比較
- **根拠**: NixOS/Guix のコンテンツアドレス型ビルドシステムとの統合必須。サプライチェーン攻撃対策（SLSA Level 3要件）
- **難易度**: Medium

#### FR-663: Build System Integration (ビルドシステム統合)

- **対象**: `src/cli/main.lisp`, 新規 `src/build/integration.lisp`
- **現状**: `cl-cc.asd`（ASDF）のみ。CMake/Meson/Bazel/Nix との統合なし
- **内容**: **CMake**: `FindCL-CC.cmake` モジュール提供、`add_cl_cc_library(name SOURCES ...)` CMake関数。**Meson**: `cl_cc.dependency()` wrap。**Bazel**: `cl_cc_binary` / `cl_cc_library` Starlark ルール。**Nix**: `buildCLCCPackage` nixpkgsヘルパー（devenv.nix統合）。**Buck2**: `cl_cc_library()` target。全ビルドシステムが`compile_commands.json`（FR-574）を出力し、LSP/clangd-compatible
- **根拠**: C/C++プロジェクトへのcl-ccコンポーネント組み込み。企業規模のモノレポ（Bazel/Buck2）でのcl-cc利用
- **難易度**: Hard

#### FR-664: Compiler Directives / Pragma System (コンパイラディレクティブシステム)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`, `src/parse/cl/parser.lisp`
- **現状**: `(declare ...)` フォームは型宣言と最適化ヒントに限定。汎用コンパイラ制御ディレクティブなし
- **内容**: `(cl-cc:pragma ...)` マクロで汎用ディレクティブシステム実装。`(cl-cc:pragma :assume (> x 0))` で前提条件をコンパイラに伝達（`__builtin_assume` 相当）。`(cl-cc:pragma :no-inline fn)` で特定関数のインライン化抑制。`(cl-cc:pragma :restrict ptr)` でポインタエイリアスなしを宣言（C `restrict` 相当）。`(cl-cc:pragma :loop-count 1000)` でループ回数ヒント。`(cl-cc:pragma :cold)` でコールドコードマーク。未知プラグマは警告のみで無視（前方互換性）
- **根拠**: `__builtin_expect` / `__builtin_unreachable` / `__builtin_assume_aligned` の宣言的Lisp版。最適化ヒントをソースコードに埋め込む標準的手段
- **難易度**: Easy

#### FR-665: Inlining Cost Model Tuning (インライン化コストモデルチューニング)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: インライン化決定はヒューリスティック（命令数閾値）のみ。呼び出し頻度・コードサイズ増加・レジスタ圧力の総合評価なし
- **内容**: 多因子インライン化コストモデル: `score = benefit(call_savings + specialization_gain + loop_invariant_removal) - cost(code_size_increase + register_pressure + compilation_time)`。JITプロファイル（FR-559 Type Feedback）による呼び出し頻度重み付け。再帰関数のインライン展開回数上限（デフォルト2）。`--inline-budget N` でコードサイズ予算制約（GCC `-finline-limit` 相当）。インライン化決定ログ（`--report-inlining`）でチューニング支援。FR-597 ML-Guided Inliningとの統合
- **根拠**: 過剰インライン化はI-cacheスラッシング・コンパイル時間増大を招く。適切なコストモデルがコンパイラ性能の鍵
- **難易度**: Medium

---

### Phase 121 — スカラー置換・例外・コンパイル時評価

#### FR-668: Scalar Replacement of Aggregates / SROA (集合体のスカラー置換)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: 構造体・CLOS インスタンスはヒープオブジェクトとして生成。エスケープしない局所オブジェクトのスタック/レジスタ分解なし
- **内容**: FR-231（Escape Analysis）でスタックエスケープしないオブジェクトを特定後、SROAを適用。`(let ((p (make-point :x 1 :y 2))) (+ (point-x p) (point-y p)))` → `(let ((p.x 1) (p.y 2)) (+ p.x p.y))`。構造体全体がレジスタ変数群に分解され、ヒープ割り当てゼロ。CLOSインスタンスのスロット配列もSROA対象（ただしメソッドディスパッチが静的に解決できる場合のみ）。LLVM `ScalarReplAggregates` / GCC `SRA` パスと同等
- **根拠**: 内部的にStructを多用するコンパイラパス（AST ノード、CPS フレーム）でGCプレッシャーを大幅削減。ホットパスのアロケーションを0に近づける最重要最適化の一つ
- **難易度**: Hard

#### FR-669: Zero-Cost Exceptions / EH Table Optimization (ゼロコスト例外処理)

- **対象**: `src/vm/conditions.lisp`, `src/backend/x86-64-codegen.lisp`, `src/binary/macho.lisp`
- **現状**: `handler-case` / `handler-bind` は実行時セットアップコスト（スタックフレーム登録）あり。例外がスローされない正常パスでも毎回コストを支払う
- **内容**: **ゼロコスト例外テーブル方式**: 正常パスのコードからハンドラ登録コードを除去し、EH（Exception Handling）テーブルをバイナリ別セクション（`.eh_frame` / Mach-O `__DATA.__eh_frame`）に配置。例外スロー時のみDWARF unwind tableをウォークしてハンドラを発見。正常パスのコストはゼロ（メモリアクセスなし）。`_Unwind_RaiseException` / libunwind との統合。C++ ABI互換のLSDA（Language Specific Data Area）フォーマット
- **根拠**: Itanium C++ ABI EH: 正常パスに実行コストなし（ただしコードサイズは増加）。cl-ccのcondition systemのオーバーヘッドを実測で確認・排除
- **難易度**: Very Hard

#### FR-670: Compile-Time Evaluation / Constexpr (コンパイル時評価)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: 定数畳み込み（FR-002）は基本算術演算のみ。任意の純粋関数のコンパイル時実行なし
- **内容**: `(cl-cc:eval-at-compile-time expr)` で純粋式をコンパイル時に完全評価し結果をリテラルに置換。評価可能条件: 副作用なし・全引数がコンパイル時定数・IOなし・実行時間上限あり（デフォルト1秒）。`(defconstant +fib-10+ (cl-cc:eval-at-compile-time (fib 10)))` → `55`。C++17 `constexpr` / Zig `comptime` / D `enum` の手法。コンパイラが内部で`our-eval`をサンドボックス実行。再帰深さ・アロケーション量に上限
- **根拠**: ルックアップテーブルの事前計算・フォーマット文字列解析・暗号定数の生成をコンパイル時に完了。実行時の計算コストゼロ
- **難易度**: Medium

#### FR-671: Superoptimization / Peephole Superoptimizer (スーパー最適化)

- **対象**: `src/optimize/optimizer.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: ピープホール最適化はプリセットパターンの照合。探索ベースの最適命令列発見なし
- **内容**: 小さな命令ウィンドウ（3〜6命令）に対して等価な短い命令列を **完全列挙+SMT検証** で探索。`(vm-move :r0 :r1) (vm-add :r0 5)` より短い等価列があればSMTで確認して置換。Souper（LLVM IR superoptimizer）/ Stochastic Superoptimization / STOKE の手法。探索はオフラインで行い結果をルールテーブルにコンパイル済みとして内蔵。`--superoptimize-window 5` で探索ウィンドウ幅を指定
- **根拠**: 人間が思いつかない命令列の短縮（`x * 15 = (x << 4) - x` など）。ホット関数に集中適用で実測性能向上
- **難易度**: Hard

---

### Phase 122 — メモリ表現最適化

#### FR-674: NaN Boxing / Pointer Tagging (NaNボクシング・ポインタタグ付け)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-execute.lisp`, `src/runtime/heap.lisp`
- **現状**: 値は個別の型タグ付きstructとして表現。`vm-integer` / `vm-float` / `vm-cons` それぞれがヒープオブジェクト
- **内容**: **NaN Boxing**: IEEE 754 倍精度浮動小数点の NaN ビットパターン（指数部全1）を利用し、64ビット値に型タグ+ペイロードを圧縮。`0x0000...` = fixnum（48bit符号付き整数）、`0x7FFF0001...` = cons pointer、`0x7FFF0002...` = symbol pointer、NaN以外の全bitパターン = double。JavaScriptエンジン（V8 / SpiderMonkey / JavaScriptCore）標準手法。OR **Pointer Tagging**: 最下位2-3ビットを型タグに使用（アライメント保証のため常に0）。fixnum=0b01、char=0b10、heap-ptr=0b00
- **根拠**: Boxed値のメモリ使用量を50-75%削減。型チェックがビットマスク1命令。V8のNaN-boxing実装でJSベンチマーク30%高速化の実績
- **難易度**: Very Hard

#### FR-675: Pointer Compression (ポインタ圧縮)

- **対象**: `src/runtime/heap.lisp`, `src/vm/vm-execute.lisp`
- **現状**: ヒープポインタは64ビット絶対アドレス。8GBヒープのポインタが全て64ビットを消費
- **内容**: ヒープを4GB以下の連続領域に固定（`mmap` の `MAP_FIXED_NOREPLACE`）。ヒープ内ポインタを32ビットオフセット（base relative）で表現。ポインタderef時に `base + compressed_ptr` で復元（1命令）。V8 Pointer Compression（2020）: V8ヒープのメモリ使用量40%削減を実現。`--max-heap 4g` 制約下で有効化。GC中の移動時はオフセット更新のみ
- **根拠**: ポインタサイズ半減によりキャッシュライン当たりの参照数2倍。ヒープスキャン速度向上。大規模Lispプログラムのメモリ効率改善
- **難易度**: Hard

#### FR-676: Thread-Local Allocation Buffers / TLAB (スレッドローカル割り当てバッファ)

- **対象**: `src/runtime/heap.lisp`, `src/vm/vm-execute.lisp`
- **現状**: ヒープ割り当てはグローバルアリーナへのatomic CAS。高並行時にキャッシュライン競合が発生
- **内容**: 各ワーカースレッド（FR-576 Work-Stealing）が専用のアロケーションバッファ（TLAB: 64-256KB）を保持。バッファ内割り当ては bump-pointer のみ（同期不要、2命令）。バッファ満杯時のみグローバルGCアリーナからrefill（ロック取得）。JVM TLAB / .NET SOH thread-local allocation の手法。`--tlab-size` で調整可能
- **根拠**: マルチスレッド割り当てのスケーラビリティをコア数に比例させる。JVM の実測: スレッド数増加時の割り当てスループット10倍向上
- **難易度**: Medium

#### FR-677: Object Pooling / Free-List Allocator (オブジェクトプーリング・フリーリストアロケータ)

- **対象**: `src/runtime/heap.lisp`, `src/compile/codegen.lisp`
- **現状**: オブジェクト生成は常に新規割り当て。コンパイラ内部で多用する短命AST/IR オブジェクトが毎サイクルGCプレッシャーを生む
- **内容**: `(cl-cc:with-object-pool (ast-call ast-var ast-const) ...)` スコープ内でプールからオブジェクトを再利用。スコープ脱出時に一括解放（GC圧力なし）。固定サイズオブジェクトをフリーリストで管理（`car` フィールドをnextポインタとして再利用）。コンパイラのhotパス（コード生成ループ）に`(declare (cl-cc:use-pool))`で自動適用。FR-668 SROAと協調してスタック割り当て可能オブジェクトをプールから除外
- **根拠**: コンパイラ自体のコンパイル速度向上（selfhosting高速化）。短命オブジェクトのGCパウズを排除
- **難易度**: Medium

---

### Phase 123 — 高度型システム IV

#### FR-680: Algebraic Effects and Handlers (代数的エフェクト・ハンドラ)

- **対象**: `src/vm/conditions.lisp`, `src/type/`, `src/expand/macros-stdlib.lisp`
- **現状**: 副作用はCLOSのdynamic-wind/handler-bind で管理。エフェクトの型安全な合成・分離なし
- **内容**: `(defeffect IO (read-char () char) (write-char (char) unit))` でエフェクト型を定義。`(with-handler (IO ...) body)` でハンドラを提供。コンパイラがエフェクトをFR-244（Effect System）の row polymorphism で型付け: `(-> unit char ! IO)`。`resume` でハンドラから継続を呼び出し（one-shot/multi-shot制御）。Koka（Microsoft Research）/ Eff / Frank / OCaml 5 effects と同等
- **根拠**: モナドトランスフォーマーより合成しやすい副作用管理。IOとエラーと状態を直交的に型付け可能。cl-ccのVM実行エフェクト（I/O、例外、状態）の型安全な抽象
- **難易度**: Very Hard

#### FR-681: GADTs / Generalized Algebraic Data Types (一般化代数的データ型)

- **対象**: `src/type/`, `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: ADTs（FR-590）は全コンストラクタが同一型を返す。型インデックスによる精緻化なし
- **内容**: `(defgadt expr (int) (lit-int () (expr integer)) (add () (expr integer) (expr integer) (expr integer)) (lit-bool () (expr boolean)) (if-expr () (expr boolean) (expr a) (expr a) (expr a)))` で型インデックス付きADTを定義。パターンマッチで型インデックスが精緻化され、`add`ブランチ内では`x`が`(expr integer)`と確定。HM型推論（FR-type系）のWAMへの拡張（型等式制約）。Haskell GHC GADTs / OCaml GADTs / Coq inductive types と同等
- **根拠**: 型安全なASTを定義でき、型チェックの不変条件をコンパイル時に保証。Well-typed interpretersパターンのcl-cc実装
- **難易度**: Very Hard

#### FR-682: Rank-N Polymorphism / Higher-Rank Types (高階ランク多相)

- **対象**: `src/type/`, `src/expand/expander.lisp`
- **現状**: HM型推論はRank-1多相（全称量化子はトップレベルのみ）。`(forall a. a -> a) -> int` のような型は表現不可
- **内容**: System F スタイルのRank-N型を型アノテーションで受け付け。`(: run-st (forall s. (forall a. (st s a)) -> a))` のような型宣言。型推論はRank-1のままで、Rank-N箇所は明示アノテーション必須（GHC `RankNTypes` と同等の設計）。型チェックに双方向型検査（FR-593）を活用。`(declare (cl-cc:type (forall a. (-> (-> a a) a a)) twice))` で宣言
- **根拠**: STモナド（可変状態の型安全なカプセル化）・CPS変換後の継続型・高階コールバックに必要
- **難易度**: Very Hard

#### FR-683: Dependent Types / Pi Types (依存型・Pi型)

- **対象**: `src/type/`, `src/compile/codegen.lisp`
- **現状**: 型は値に依存しない。`(vector n)` の長さ`n`は型に反映されない
- **内容**: `(: make-vector (Pi (n : Nat) -> (vector n)))` で型が値に依存する関数を宣言。`(: vzip (Pi (n : Nat) -> (vector n) -> (vector n) -> (vector n)))` で長さ整合性をコンパイル時保証。型検査はSMTソルバー（FR-246）に長さ制約を送出して解決。完全な依存型（CIC）ではなく **液体型（Liquid Types）** スタイルの部分的依存型（整数・長さ・非零に限定）。Agda / Idris2 / Coq / Liquid Haskell と比較してスコープ限定
- **根拠**: 配列境界違反・整数オーバーフロー・ゼロ除算をコンパイル時に排除。証明支援システムへの橋渡し
- **難易度**: Very Hard

---

### Phase 124 — 関数変換技法

#### FR-686: Worker-Wrapper Transformation (ワーカー・ラッパー変換)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: 再帰関数は外部インターフェースと内部ループが一体化。boxed引数/戻り値の変換が毎再帰で発生
- **内容**: 再帰関数を **wrapper**（ユーザー向けインターフェース、型変換）と **worker**（unboxed引数でタイトループ）の2関数に分割。`(defun sum (xs) (labels ((worker (xs acc) ...)) (worker xs 0)))` の構造を自動導出。workerはunboxed整数引数（FR-type 系の unboxed representation 活用）。GHC の W/W split（最重要最適化の一つ）と同等。FR-618 DAE の補完
- **根拠**: 再帰関数のhotループからboxing/unboxingを排除。GHC実績: 多くの数値関数で2〜5倍高速化
- **難易度**: Hard

#### FR-687: Administrative Normal Form / ANF Transformation (行政正規形変換)

- **対象**: `src/compile/cps.lisp`, `src/compile/codegen.lisp`
- **現状**: CPS変換（FR-CPS系）が中間表現。ANFはCPSと表現力等価だがより直接的なSSA的性質を持つ
- **内容**: CPSパイプラインと並列にANF変換器を実装。`(+ (* a b) (* c d))` → `(let ((t1 (* a b)) (t2 (* c d))) (+ t1 t2))`。全部分式が名前付き一時変数に展開され、評価順序が明示的。ANF のプロパティ: SSAと同型（Kelsey 1995）、最適化パス（GVN・CSE）の実装が簡単化。`--emit-anf` フラグでANFダンプ。将来的にCPSとANFのどちらかに統一可能な設計
- **根拠**: SSAとCPS/ANFの等価性（Kelsey 1995 / Appel 1998）は型理論上重要。MLIR lower passへの入力として有用
- **難易度**: Medium

#### FR-688: Defunctionalization (脱関数化)

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`
- **現状**: 高階関数はクロージャとして表現。特定パターン（CPS継続の固定セット）で関数オブジェクトをデータに変換可能な場合も常にクロージャ割り当て
- **内容**: Reynolds（1972）の脱関数化: 関数値をsumタイプ（`cont-add`/`cont-mul`/`cont-done`など）のデータに変換し`apply-cont`ディスパッチャで解釈。CLOSのgeneric functionとして実装（specializerがdefunction tag）。静的解析でclose-worldの関数セットが確定できる場合にのみ適用。`(declare (cl-cc:defunctionalize))` で手動指定
- **根拠**: クロージャ割り当てゼロの一階CPS実行。インタプリタのメイン継続ループを最適化。Reynolds defunctionalization のCPS + tail callとの組み合わせが非常に強力
- **難易度**: Hard

#### FR-689: Lambda Lifting (ラムダリフティング)

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`
- **現状**: ネストした `flet`/`labels` 定義はクロージャとしてコンパイル。自由変数がキャプチャされ毎呼び出しでクロージャ生成
- **内容**: 自由変数を明示的な引数として追加してローカル関数をトップレベルに持ち上げる。`(labels ((add (x) (+ x n))) (add 5))` → `(defun lifted-add (n x) (+ x n))` `(lifted-add n 5)`。クロージャ変換との使い分け: キャプチャ変数が少なく呼び出し頻度が高い場合はリフティングが優位（クロージャ割り当てなし）。FR-031 LICMと協調（ループ不変クロージャをリフト）
- **根拠**: ホットループ内のlocal function呼び出しからクロージャ割り当てを除去。selfhostingコードの内部ヘルパー関数に多数適用可能
- **難易度**: Medium

---

### Phase 125 — テスト計装・品質

#### FR-692: Code Coverage Instrumentation (コードカバレッジ計装)

- **対象**: `src/compile/codegen.lisp`, `src/cli/main.lisp`
- **現状**: テスト実行時のカバレッジ計測なし。どのコードパスがテストされているか不明
- **内容**: `--coverage` フラグでライン/ブランチ/MC/DCカバレッジ計装を追加。各基本ブロックの先頭にカウンタインクリメント命令を挿入。実行後 `foo.clcc-cov` ファイルに集計データを出力。`./cl-cc coverage report` でHTML/LCOVレポート生成。LLVM `InstrProfiling` / gcov (GCC) / Istanbul (JS) と同等形式。`(declare (cl-cc:no-coverage))` で特定関数を除外。FiveAMとの統合でテスト実行時に自動収集
- **根拠**: 4322テストのカバレッジ把握が困難。死んだコードの発見・テスト不足領域の特定に不可欠
- **難易度**: Medium

#### FR-693: Heap Profiler (ヒーププロファイラ)

- **対象**: `src/runtime/heap.lisp`, `src/vm/vm-execute.lisp`, `src/cli/main.lisp`
- **現状**: `--verbose-gc` でGC統計のみ。どの関数・コードパスが最もメモリを消費するか不明
- **内容**: `./cl-cc run --heap-profile foo.lisp` でサンプリングベースヒーププロファイリング。割り当て時にPC（プログラムカウンタ）をサンプリングしてコールスタック記録。`./cl-cc heapprof report foo.clcc-heap` でフレームグラフ（Brendan Gregg形式）/ allocation breakdown ツリー生成。型別割り当て量の集計。`(cl-cc:with-heap-profile ...)` スコープで特定区間をプロファイル。Heaptrack / jemalloc heap profiler / Go pprof と同等
- **根拠**: selfhostingコンパイルの最大メモリ消費箇所の特定。GCチューニングの根拠データ収集
- **難易度**: Medium

#### FR-694: Mutation Testing (ミューテーションテスト)

- **対象**: `src/cli/main.lisp`, 新規 `src/testing/mutation.lisp`
- **現状**: テストスイート（4322テスト）の品質はカバレッジ指標なし。テストが「通る」だけでなく「変更を検出できるか」不明
- **内容**: `./cl-cc muttest src/compile/cps.lisp` でソースにミューテーション（`+`→`-`、`<`→`<=`、定数変更、ブランチ反転、関数呼び出し削除）を自動適用しテスト実行。**生き残ったミュータント**（テストが通るが実際は壊れたコード）を報告。ミューテーションスコア = 殺されたミュータント / 全ミュータント数。`--timeout 10s` で各ミュータントのテスト実行タイムアウト。PiTest（Java）/ Mutant（Ruby）/ cosmic-ray（Python）と同等
- **根拠**: 高カバレッジでも検出力が低いテストを発見。テストスイートの実質的な強さを定量化
- **難易度**: Hard

#### FR-695: Benchmarking Framework (ベンチマークフレームワーク)

- **対象**: 新規 `src/testing/benchmark.lisp`, `tests/bench/`
- **現状**: パフォーマンス回帰の検出手段なし。コード変更がコンパイラ速度・生成コード速度に与える影響を定量化できない
- **内容**: `(defbench matrix-mul-3x3 (b) (declare (iterations 10000)) (run-once b (matrix-mul a b)))` でベンチマーク定義。**統計的に頑健な計測**: warm-up・外れ値除去・Mann-Whitney U検定で回帰検出。`./cl-cc bench --compare old.json` でベースラインとの差分表示。`--emit-flamegraph` でCPUプロファイルをGnuplotフレームグラフ出力。Criterion（Rust）/ Google Benchmark（C++）/ Hyperfine と同等の統計的精度
- **根拠**: 最適化パスの追加・変更時にパフォーマンス回帰を自動検出。selfhostingコンパイル時間のベースライン管理
- **難易度**: Medium

---

### Phase 126 — ビルドスケーラビリティ

#### FR-698: Parallel Compilation (並列コンパイル)

- **対象**: `src/cli/main.lisp`, `src/compile/codegen.lisp`
- **現状**: ファイルのコンパイルはシリアル。86ソースファイルの selfhosting が全て順次処理
- **内容**: ASDF依存グラフを解析してトポロジカルソート後、独立コンパイル単位を並列実行。`./cl-cc build --jobs 8` で最大8並列コンパイル。ファイル間依存（パッケージ定義・マクロ）の依存追跡と並列安全性確認。共有マクロ環境の読み取りロック・書き込みロック（SBCL `bt:make-lock`）。`make -j` / Bazel 並列ビルド / Cargo `--jobs` と同等
- **根拠**: selfhostingコンパイル時間の削減。8コアマシンで最大4〜6倍のビルド高速化（I/O待ちで線形にはならない）
- **難易度**: Hard

#### FR-699: Distributed Build Cache (分散ビルドキャッシュ)

- **対象**: `src/cli/main.lisp`, 新規 `src/build/cache.lisp`
- **現状**: FR-452（コンパイルキャッシュ）はローカルディスクキャッシュのみ。CI/CDの並列ジョブ間でキャッシュ共有不可
- **内容**: コンテンツアドレス型リモートキャッシュ（入力ハッシュ→コンパイル済み成果物）。バックエンド: HTTP(S) RESTful API / Redis / S3 互換ストレージ。`--remote-cache https://cache.example.com` で有効化。アップロード/ダウンロードの並列化。FR-662（Reproducible Builds）前提（非決定論的出力はキャッシュ不可）。Bazel Remote Cache API（REAPI）互換プロトコル。GitHub Actions / GitLab CI との統合例を文書化
- **根拠**: CI/CDでPRごとに全ファイル再コンパイルを回避。大規模チームでのビルド時間を数十分→数秒に短縮
- **難易度**: Hard

#### FR-700: Compiler Plugin API (コンパイラプラグインAPI)

- **対象**: `src/cli/main.lisp`, `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: コンパイラのパスは全て内部実装。外部からIR変換・最適化・診断を注入する拡張点なし
- **内容**: `(cl-cc:define-compiler-plugin my-plugin (:after :cps-transform) (fn ir) ...)` でコンパイルパイプラインへのフックを定義。`--load-plugin my-plugin.lisp` でロード。プラグインがアクセスできるAPI: IR読み取り・変換・診断発行・新命令登録。プラグインのサンドボックス化（危険操作は別パーミッション）。GCC plugin API / LLVM pass plugin / rustc `proc-macro` の設計を参考
- **根拠**: ドメイン固有最適化（DSPコード用ベクトル化ヒント、セキュリティポリシー強制）を外部から追加可能。cl-ccの拡張性の核心
- **難易度**: Hard

#### FR-701: Live Code Update / Hot Patching (ライブコード更新・ホットパッチ)

- **対象**: `src/vm/vm-execute.lisp`, `src/cli/main.lisp`, `src/vm/vm-run.lisp`
- **現状**: コード変更にはプロセス再起動が必要。REPL（FR-098）はトップレベル式の評価のみ
- **内容**: 実行中VMへの関数定義の動的差し替え。`./cl-cc live-update --pid 1234 new-fn.lisp` で実行中プロセスの関数テーブルを更新。`*function-table*` ハッシュの原子的更新（実行中フレームは旧版を完了、新呼び出しから新版使用）。インライン化済みJITコードの無効化（FR-060 JITキャッシュ連携）。Erlang Hot Code Loading / Clojure nREPL / Common Lisp image hot patch と同等
- **根拠**: Webサービス・長時間デーモンのゼロダウンタイム更新。cl-cc自身のセルフホスティング開発サイクルの高速化（再起動なし）
- **難易度**: Very Hard

---

### Phase 127 — 低レベル制御

#### FR-704: Inline Assembly (インラインアセンブリ)

- **対象**: `src/expand/expander.lisp`, `src/backend/x86-64-codegen.lisp`, `src/compile/codegen.lisp`
- **現状**: x86-64バックエンドは内部命令セット（FR-x86-64系）のみ。ユーザーが任意のアセンブリを挿入できない
- **内容**: `(cl-cc:asm "movq %rax, %rbx" :outputs ((dst :r)) :inputs ((src :r)) :clobbers (:r0))` でGCC extended assembly構文相当のインラインasmを提供。制約文字列（`"r"`, `"m"`, `"=r"`）でレジスタ/メモリ指定。レジスタアロケータとの統合（clobber宣言で使用レジスタを保護）。`(cl-cc:asm :volatile "cpuid" ...)` でmemory barrier / serialize として使用。SBCL `%primitive` / C `__asm__` 相当
- **根拠**: 暗号アルゴリズムの定数時間実装（FR-453 Constant-Time）・OS kernel サポート・カスタムABI呼び出しでインラインasmが不可欠
- **難易度**: Hard

#### FR-705: Bit Manipulation Intrinsics (ビット操作組み込み関数)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-bitwise.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: bitwise演算（FR-304系）は汎用。`popcount`/`clz`/`ctz`/`bswap` などの1命令CPU操作をエミュレート
- **内容**: `(cl-cc:popcount x)` → `POPCNT` x86命令（1サイクル）。`(cl-cc:clz x)` → `BSR`/`LZCNT`（先頭ゼロビット数）。`(cl-cc:ctz x)` → `BSF`/`TZCNT`（末尾ゼロビット数）。`(cl-cc:bswap x)` → `BSWAP`（バイト順反転）。`(cl-cc:pdep mask val)` → `PDEP`（ビット散布）。`(cl-cc:pext mask val)` → `PEXT`（ビット収集）。ARMでは `RBIT`/`CLZ`/`REV` にマップ。コンパイラが使用箇所を自動認識してintrinsicに置換する パターンマッチも実装
- **根拠**: ハッシュテーブル（popcount）・圧縮データ構造（pdep/pext）・暗号実装（bswap）で手書きより100倍速い単一命令実行
- **難易度**: Medium

#### FR-706: Memory Model Specification (メモリモデル仕様)

- **対象**: `src/vm/vm-execute.lisp`, `src/backend/x86-64-codegen.lisp`, `src/backend/aarch64.lisp`
- **現状**: マルチスレッドのメモリ可視性（FR-160系）はhardwareに依存。形式的なメモリモデルの宣言なし
- **内容**: C11/C++11/Java Memory Model 準拠のメモリ順序を宣言的に扱う。`(cl-cc:load :acquire ptr)` / `(cl-cc:store :release ptr val)` / `(cl-cc:fence :seq-cst)` の原子操作APIにセマンティクスを付与。x86-64は TSO（Total Store Order）なので `acquire`/`release` はコード生成上のバリアなし、AArch64では `LDAR`/`STLR` 命令を発行。`(declare (cl-cc:memory-model :tso))` でターゲットのメモリモデルを宣言し最適化の適法性チェック
- **根拠**: ロックフリーデータ構造（FR-163）のバグの最大原因がメモリモデル誤解。形式的仕様による安全性保証
- **難易度**: Hard

#### FR-707: Dynamic Loading / dlopen (動的ローディング)

- **対象**: `src/vm/vm-execute.lisp`, `src/cli/main.lisp`, `src/ffi/`
- **現状**: バイナリは静的リンクのみ。実行時の共有ライブラリロード・プラグイン機能なし
- **内容**: `(cl-cc:load-library "libfoo.so")` → `dlopen`（Linux/macOS）/ `LoadLibraryA`（Windows）呼び出し。`(cl-cc:foreign-symbol lib "foo_init")` → `dlsym` でシンボル解決。アンロード: `(cl-cc:unload-library lib)` → `dlclose`。ライブラリのエラー処理（`dlerror`）。型安全なwrapper: `(cl-cc:define-foreign-callable lib "process" (-> integer integer))` でFFI型注釈付きロード。FR-632（Bindgen）との統合で型付きwrapperを自動生成
- **根拠**: プラグインシステム・ゲームMOD・ライブラリの遅延ロードに必須。cl-ccのREPL（FR-098）でのライブラリ動的ロードに直結
- **難易度**: Medium

---

### Phase 128 — 型システム V

#### FR-710: Phantom Types (ファントム型)

- **対象**: `src/type/`, `src/expand/macros-stdlib.lisp`
- **現状**: 型パラメータは全て実行時値を持つ。コンパイル時専用の型マーカーなし
- **内容**: `(deftype tagged (a) ...)` で型パラメータ`a`が実行時には消去される「ファントム型」を定義。`(: make-safe-ptr (-> raw-ptr (tagged :validated raw-ptr)))` で検証済みポインタを型レベルでマーク。`(: use-safe-ptr (-> (tagged :validated raw-ptr) result))` で検証済みのみ受け付け。状態機械をファントム型でエンコード: `(tagged :open file-handle)` / `(tagged :closed file-handle)`。Haskell phantom types / Rust zero-sized type markers と同等
- **根拠**: SQLインジェクション防止（`(tagged :sanitized string)` のみSQL関数に渡せる）・未初期化データ保護・プロトコル状態の静的検証
- **難易度**: Medium

#### FR-711: Type Classes / Coherent Overloading (型クラス・コヒーレントオーバーロード)

- **対象**: `src/type/`, `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: CLOSのgeneric functionがアドホック多相を提供するが型推論との統合なし。型クラス制約の推論不可
- **内容**: `(define-class (Eq a) (= (-> a a boolean)))` でHaskellスタイル型クラスを定義。`(define-instance (Eq integer) (= (lambda (a b) (cl:= a b))))` でインスタンス実装。型推論が型クラス制約を自動推論: `(defun member (x xs) ...)` → 型 `(forall a. (Eq a) => a -> (list a) -> boolean)` を導出。**コヒーレンス保証**: 同一型に複数の矛盾するインスタンスが存在しないことをコンパイル時検証。辞書渡し（dictionary passing）でコンパイル
- **根拠**: CLOSよりも型安全なオーバーロード。型推論システムとの統合でアドホック多相の型エラーを早期検出
- **難易度**: Very Hard

#### FR-712: Monomorphization (単態化)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: generic functionはディスパッチテーブル経由の動的ディスパッチ。型が静的に確定している場合も動的コスト発生
- **内容**: 型推論（FR-type系）で型引数が完全に確定した generic function 呼び出しを特定。`(foo 1 2)` の `foo` が `(-> integer integer integer)` と確定したら、整数特化版 `foo/integer/integer` を生成しダイレクト呼び出しに置換。Rust のゼロコスト抽象 / C++ テンプレート特殊化 / MLton の whole-program monomorphization と同等。コードサイズとランタイム性能のトレードオフを `--mono-threshold` で制御
- **根拠**: 動的ディスパッチを直接呼び出しに変換し間接参照コストを排除。JITなしで数値演算を最大5倍高速化可能
- **難易度**: Hard

#### FR-713: Termination Checking (停止性検査)

- **対象**: `src/type/`, `src/compile/codegen.lisp`
- **現状**: 全再帰関数は停止するかどうか不明。無限ループと意図的な非停止が区別不可
- **内容**: **構造的再帰チェック**: 再帰呼び出しの引数が元の引数の「構造的に小さい」部分項であることを確認（`cdr`・`rest`・整数デクリメント）。`(declare (cl-cc:terminates))` で停止性を宣言しコンパイラが検証。停止性不明時は警告。**サイズ変化分析**: 整数引数の減少を追跡（`(> n 0)` ガード + `(- n 1)` 再帰）。Agda / Coq / Idris の termination checker と同等（簡易版）。`(declare (cl-cc:non-terminating))` でジェネレータ・サーバーループを明示
- **根拠**: 型理論の健全性には停止性が必要。証明支援システム統合（FR-627）の前提条件
- **難易度**: Hard

---

### Phase 129 — GC高度化

#### FR-716: Colored Pointer GC / ZGC-Style (カラードポインタGC)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`, `src/vm/vm-execute.lisp`
- **現状**: 2世代GC（FR-GC系）はstop-the-world。FR-552（Concurrent GC）は基本的な並行マーキング
- **内容**: 64ビットポインタの上位ビット（x86-64の未使用46〜63ビット）に **GCメタデータ**（marked/remapped/finalizable フラグ）を格納。参照のたびにカラービットを確認（**ロードバリア**: 1命令）。リロケーション中もミュータントは古いアドレスを使え、ロードバリアが自動的に新アドレスに転送。STWポーズがサブミリ秒（<1ms）を実現。OpenJDK ZGC（2018）/ C4 GC（Azul Systems）と同等の設計
- **根拠**: selfhostingコンパイルのGCポーズ削除。長時間実行サーバー（REPL・LSP server）でのレイテンシ安定化
- **難易度**: Very Hard

#### FR-717: SATB Write Barrier (スナップショット書き込みバリア)

- **対象**: `src/runtime/gc.lisp`, `src/vm/vm-execute.lisp`
- **現状**: FR-552（Write Barrier）はcard-table方式の世代間参照追跡。並行マーキング中の浮きゴミ問題への対策なし
- **内容**: **SATB (Snapshot-At-The-Beginning)** バリア: 並行マーキング開始時の到達可能集合スナップショットを基準に収集。書き込み `(setf (slot-value obj :ref) new-val)` 時に **旧値**をSATBキューにバッファリング。マーキングスレッドがSATBキューをドレインして旧値を遅延マーク。三色不変条件（white/gray/black）の維持。G1 GC / Shenandoah のSATBバリアと同等。FR-716（ZGC）のロードバリアと比較: SATBはストアバリア
- **根拠**: 並行GCの正確性保証。マーキング中に死んだオブジェクトへの参照が見落とされる「Yuasa bug」を防止
- **難易度**: Very Hard

#### FR-718: Region-Based GC / G1-Style (リージョンベースGC)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: ヒープは連続する若/老世代の2分割。大規模ヒープでは単一のfull GCポーズが長大化
- **内容**: ヒープを等サイズの **リージョン**（1MB）に分割。各リージョンが独立した世代を持ち（eden/survivor/old）、ガベージが多いリージョンを優先して収集（**Garbage First**）。STWポーズを予測可能な上限時間内（`--gc-pause-target 50ms`）に制限。Remembered Sets でリージョン間参照を追跡。大オブジェクトは専用Humongous リージョンに配置。OpenJDK G1 GC（2006）/ Microsoft CLR region GC と同等
- **根拠**: 大規模Lispプログラム（数GBヒープ）でのGCポーズ予測可能化。現在の単純2世代GCは8GB超でポーズが秒単位になる可能性
- **難易度**: Very Hard

#### FR-719: Epsilon GC / No-Op GC (ノーオプGC)

- **対象**: `src/runtime/gc.lisp`, `src/cli/main.lisp`
- **現状**: GCは常に有効。テスト・ベンチマーク時のGCオーバーヘッドがノイズになる場合がある
- **内容**: `--gc epsilon` フラグでGC完全無効化（アロケートするだけで収集しない）。メモリ枯渇時に `Out of Memory` エラー終了。用途: (a) 短命プロセス（コンパイル1回実行）でGCオーバーヘッドゼロ、(b) GCなしのベンチマーク基準測定、(c) FR-606（No-Allocator Mode）との比較検証。OpenJDK Epsilon GC（JEP 318, 2018）/ .NET NoGC region と同等。ヒープ上限（`--max-heap 2g`）との組み合わせが前提
- **根拠**: コンパイル時間ベンチマークにGCポーズのノイズが混入することを完全排除。単発コンパイルタスクで最高スループット達成
- **難易度**: Easy

---

### Phase 130 — 文字列・シンボル・数値最適化

#### FR-722: String Interning (文字列インターン)

- **対象**: `src/vm/strings.lisp`, `src/vm/symbols.lisp`, `src/runtime/heap.lisp`
- **現状**: 文字列は毎回新規ヒープオブジェクト。`(equal "foo" "foo")` は内容比較（O(n)）。コンパイラ内部での識別子文字列が多数重複
- **内容**: `(cl-cc:intern-string s)` でグローバルインターンテーブルに登録し正準化オブジェクトを返す。インターン済み文字列同士の等価判定は `eq`（ポインタ比較、O(1)）。コンパイラが文字列リテラルをコンパイル時に自動インターン。GCはインターンテーブルを弱参照で保持（参照なし文字列は回収可能）。Java `String.intern()` / Lisp `intern`（シンボル用）の文字列版 / Python str interning と同等
- **根拠**: パーサ・シンボルテーブルで同一文字列が数千コピー存在する問題を解消。識別子比較を`eq`化してハッシュテーブルのkeyとして効率化
- **難易度**: Easy

#### FR-723: Small String Optimization / SSO (小文字列最適化)

- **対象**: `src/vm/strings.lisp`, `src/runtime/heap.lisp`
- **現状**: 全文字列がヒープオブジェクト（ポインタ + 長さ + データバッファ）。1〜15文字の短い文字列でもヒープ割り当てが必要
- **内容**: 15バイト以下の文字列を **インラインで** ポインタ幅（64ビット）に収める。タグビットで「short string」を識別。`(string-length s)` と `(char s i)` が共通APIで透過的に動作。C++ `std::string` SSO / Rust `SmallString` / JavaScript V8 SeqOneByteString の手法。16バイト以上は通常ヒープ文字列にフォールバック
- **根拠**: Lispプログラムでは短い識別子文字列（変数名・スロット名）が大半。ヒープ割り当てとGCスキャンを排除しキャッシュ効率向上
- **難易度**: Hard

#### FR-724: Numeric Tower Optimization (数値塔最適化)

- **対象**: `src/vm/primitives.lisp`, `src/vm/vm-execute.lisp`, `src/compile/codegen.lisp`
- **現状**: 整数演算は全てboxed fixnum/bignum。fixnumのオーバーフロー検出後のbignum昇格コストが毎演算で発生
- **内容**: **タグ付きfixnum高速パス**: 63ビット符号付き整数（最下位ビット=0）として表現し、加算/乗算後のオーバーフローをCF/OF フラグで検出（`JO` 命令）、オーバーフロー時のみbignum昇格パスへ。**専用fixnum演算**: `vm-add-fixnum` / `vm-mul-fixnum` で型チェック不要な高速パス。`(declare (fixnum n))` で型アノテーション付き変数は常にfixnum命令を使用。SBCL fixnum inline expansion / MIT Scheme fixnum optimization と同等
- **根拠**: 整数集中コード（ループカウンタ・配列インデックス・ハッシュ計算）でboxingコストをゼロに。コンパイラ内部の整数演算に即座に効果
- **難易度**: Hard

#### FR-725: Symbol Table Optimization (シンボルテーブル最適化)

- **対象**: `src/vm/symbols.lisp`, `src/vm/vm-execute.lisp`, `src/package.lisp`
- **現状**: シンボルルックアップはハッシュテーブル（`gethash`）。コンパイラの主要ホットパスがシンボル解決に費やすコストが大きい
- **内容**: **完全ハッシュ（perfect hashing）**: コンパイル済みパッケージの既知シンボルセットに対してMinimal Perfect Hash Function（MPHF）を生成（CHD algorithm）。ビルド時に `*package*` の全エクスポートシンボルからMPHFテーブルを事前計算しFASLに埋め込み。ルックアップはO(1)で衝突ゼロ。動的`intern`はフォールバックHT経由。`--emit-perfect-hash-tables` フラグ。gperf / CMPH / rust phf crate と同等
- **根拠**: 大規模パッケージ（cl-cc自身の~600エクスポート）でのシンボルルックアップ高速化。マクロ展開のhotパスがシンボル比較で多くの時間を消費
- **難易度**: Medium

---

### Phase 131 — パターンマッチ最適化

#### FR-728: Decision Tree Compilation for Pattern Matching (パターンマッチ決定木コンパイル)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: `cond` / `typecase` / パターンマッチはフラットな連続比較に変換。共通テストの重複・最適な分岐木の構築なし
- **内容**: パターンマッチ式をSwitch Dispatch Matrix（SDM）として表現し、**Maranget のアルゴリズム**（ML 2008）で最適決定木にコンパイル。共通テスト（型タグ確認など）を引き上げて一度だけ実行。CLOSのmulti-dispatch（FR-CLOS系）のdispatcher生成にも適用。生成決定木の分岐回数を最小化（NP完全だが小規模パターンには厳密最適）。Maranget's "Compiling Pattern Matching to Good Decision Trees" (ICFP 2008) の実装
- **根拠**: 多くのCLOSメソッドや`ccase`/`etypecase`でテスト重複を排除。特に型タグが共通する分岐群でキャッシュ効率大幅改善
- **難易度**: Hard

#### FR-729: Pattern Usefulness / Redundancy Checking (パターン有用性・冗長性チェック)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: FR-085（パターンマッチ網羅性検査）は未カバーパターンを検出。到達不能パターン（redundant/useless）の警告なし
- **内容**: 各パターン節 `pᵢ` が `p₁...pᵢ₋₁` で既に網羅されていないか（**有用性検査**）を実行。`(case x (:foo ...) (:foo ...))` の2番目`:foo`は冗長 → コンパイル警告。**相互排他性検査**: 全パターンが互いに排他か確認。Maranget's usefulness algorithm / Haskell GHC exhaustiveness+redundancy / Rust match arms の実装を参考
- **根拠**: リファクタリング時に残留した到達不能パターンをゼロコストで検出。コードの信頼性向上
- **難易度**: Medium

#### FR-730: Tail Recursion Modulo Cons / TRMC (末尾再帰モジュロコンス最適化)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/cps.lisp`
- **現状**: TCO（FR-005）は末尾位置の関数呼び出しのみ対象。`(cons x (recurse ...))` のように`cons`の中の再帰はスタック消費
- **内容**: `(defun map (f xs) (if (null xs) nil (cons (f (car xs)) (map f (cdr xs)))))` の末尾呼び出しでない再帰を最適化。コンスセルを事前割り当てし「穴」（CDRスロット）を残したまま次の反復に進み、後から埋める。結果: スタック増加なしで `map` を反復実行。`(declare (cl-cc:trmc))` で明示指定。Guy Steele / OCaml 5.1 TRMC (`[@tail_mod_cons]`) の実装
- **根拠**: `map`/`filter`/`append` 等の定義がTCOと同等の空間効率を得る。ユーザーが継続渡しスタイルに書き直す必要なし
- **難易度**: Hard

#### FR-731: Join Points (結合点)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 複数のブランチが同一コードに合流する場合、ブランチごとにコードを複製またはラベルジャンプ。最適化の障壁になる場合がある
- **内容**: **Join point** (`j`): 末尾位置でのみ呼ばれる局所関数として導入。`(if c (join k a) (join k b))` を `(letjoin k (x) ... x ...) (if c (k a) (k b))` に変換。join pointへのジャンプはスタックフレームなしの直接`jmp`命令にコンパイル（TCO的）。GHC join points（POPL 2017, Maurer et al.）のVM版。CPS継続 + ラベルジャンプの中間点として機能
- **根拠**: if-then-elseの合流コードを1コピーに保つ。インライン化後の爆発的コード複製を防止。最適化パスがjoin pointを超えて適用可能
- **難易度**: Hard

---

### Phase 132 — レジスタ割り当て高度化

#### FR-734: Register Coalescing (レジスタ合体)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/emit/mir.lisp`
- **現状**: コード生成後に`MOVE r1, r2`命令が多数残存。レジスタ割り当て後の冗長コピー除去なし
- **内容**: `(move dst src)` 命令を除去するため `dst` と `src` の live range を合体。**コンサーバティブ合体**: Chaitin-Briggs: 合体後の干渉グラフが彩色可能な場合のみ合体（着色数を増やさない）。**アグレッシブ合体**: George-Appel: 干渉がなければ無条件合体。合体後スピル増加を許容する場合は後退。FR-MIR層（FR-626 MLIR）のSSA形式とのBriggs/Cooper SSA-aware coalescing統合
- **根拠**: selfhostingコード生成後のコピー命令を50〜70%削減（実測値）。コンパイラ内でのレジスタスラッシング排除
- **難易度**: Hard

#### FR-735: Rematerialization (再実体化)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: レジスタスピル時は値をスタックに保存して後で復元。再計算が保存より安い場合でも常にspill/reload
- **内容**: スピル候補の値が「再実体化可能」（定数・単一演算・ループ不変）か判定。再実体化可能なら `reload` の代わりにspill siteで再計算。`(vm-const 42)` / `(vm-add base offset)` のような cheap 演算はspillよりremat有利。LLVM `MachineSink` + rematerialization / GCC `REG_REMATERIALIZABLE` と同等。FR-670（Constexpr）で評価済みの値は自動remat候補
- **根拠**: ループ内のspill/reloadをメモリアクセスゼロに。定数配列のベースアドレスなど、毎イテレーション再計算の方がL1ミスより速い
- **難易度**: Medium

#### FR-736: Instruction Selection via BURS (BURS命令選択)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/emit/target.lisp`
- **現状**: 命令選択はASTノードからの手続き的なコード生成。最適な命令列の系統的探索なし
- **内容**: **BURS（Bottom-Up Rewrite System）**: VM IR をツリーとして表現し、ターゲット命令パターンをリライトルール（`(add (load addr) reg) → ADD reg, [addr]` コスト2）として定義。動的計画法（DP）で各ノードのコスト最小カバリングを計算。x86-64の複合命令（`IMUL r, [mem]`・`LEA`・`CMOV`）を正しく選択。iburg / LLVM SelectionDAG / GCC RTL と同等の設計
- **根拠**: 手作りのコード生成よりも系統的に最適命令列を選択。LEAによる乗算・加算の統合など人間が見落とすパターンを自動発見
- **難易度**: Hard

#### FR-737: Anti-Dependence Breaking / Register Renaming (アンチ依存解消・レジスタ名前変更)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 命令スケジューリング（FR-033）はtrue-dependenceのみ追跡。アンチ依存（WAR: write-after-read）と出力依存（WAW: write-after-write）がスケジューリングを阻害
- **内容**: コンパイラによる **静的レジスタ名前変更**: WAR/WAW依存を新しい仮想レジスタに書き換えて偽依存を除去。`r0 = r1 + r2; r1 = r3 + r4` → `r0 = r1 + r2; r1_new = r3 + r4` で WAR解消。SSA形式に昇格すれば自動的に達成。スケジューリングウィンドウを3〜5命令から20命令に拡大。OoO CPU の動的リネーミング（ROB）が静的には重複しない最適なスケジュールを生成できる前提条件
- **根拠**: Out-of-Order CPU でのスケジューリング効果を最大化。静的リネームで実行ユニット利用率が10〜20%向上
- **難易度**: Medium

---

### Phase 133 — V8スタイルオブジェクト最適化

#### FR-740: Hidden Classes / Object Shapes (隠しクラス・オブジェクトシェイプ)

- **対象**: `src/vm/vm-clos.lisp`, `src/vm/vm-execute.lisp`, `src/compile/codegen.lisp`
- **現状**: CLOSインスタンスはスロットをハッシュテーブルで管理。スロットアクセスごとにハッシュ検索が発生（O(1)平均だがキャッシュ非友好）
- **内容**: 同一スロットセット（同一クラスの全インスタンス）を共有する **Shape オブジェクト** を導入。インスタンスはshapeポインタ + 固定オフセット配列で表現。`(slot-value obj :x)` → `(aref (instance-slots obj) (shape-slot-offset (instance-shape obj) :x))`。スロットの追加/削除時にshapeを遷移（transition tree）。JITで特定shapeを想定した高速スロットアクセスを生成（FR-JIT系）。V8 Hidden Classes / SpiderMonkey Shapes / WebKit Structure と同等
- **根拠**: CLOSインスタンスのスロットアクセスをハッシュO(1)から配列O(1)（キャッシュフレンドリー）に変換。JITの投機的インライン化（FR-560）とシナジー
- **難易度**: Hard

#### FR-741: Inline Method Dispatch Tables / IMT (インラインメソッドディスパッチテーブル)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **現状**: generic functionのディスパッチは型タグでspecializerを順次照合。大きなディスパッチテーブルでの線形探索
- **内容**: クラスごとに **インターフェースメソッドテーブル（IMT）** を生成。64エントリの固定サイズハッシュテーブル（メソッドセレクタ → 実装ポインタ）。ハッシュ衝突は conflict stub（linker-time dispatch）で解決。生成されたネイティブコード: `call [(class-imt obj)[selector-hash * 8]]`（2命令）。JVM Interface Method Table / .NET Interface Dispatch Stub と同等。FR-740（Shapes）と組み合わせてモノモーフィック呼び出しへの特化
- **根拠**: generic functionのディスパッチをO(n)からO(1)に。multi-dispatch（FR-CLOS多重ディスパッチ）のhotpathをJIT特化
- **難易度**: Hard

#### FR-742: Object Header Layout Optimization (オブジェクトヘッダレイアウト最適化)

- **対象**: `src/vm/vm.lisp`, `src/runtime/heap.lisp`, `src/vm/vm-clos.lisp`
- **現状**: ヒープオブジェクトはLispのハッシュテーブルで実装。オブジェクトヘッダのレイアウト（GCマークビット・型タグ・ハッシュコード）が最適化されていない
- **内容**: 全ヒープオブジェクトに64ビット **mark word** を先頭フィールドとして配置: bits[0:1]=GC色（white/gray/black）, bits[2:3]=オブジェクト種別, bits[4:31]=identity hash code（遅延計算）, bits[32:63]=lock state / class index。GCがmark wordのビット操作のみでフェーズを管理（ヒープスキャン高速化）。FR-716（Colored Pointer GC）との協調: pointer-tag方式とmark-word方式を設定で切り替え
- **根拠**: Java HotSpot mark word / JVM object header の設計が30年の実績。単一64bit読み取りでGC状態・型・ロックを同時取得
- **難易度**: Hard

#### FR-743: Value Types / Inline Structs (値型・インライン構造体)

- **対象**: `src/type/`, `src/compile/codegen.lisp`, `src/vm/vm-execute.lisp`
- **現状**: 全オブジェクトはヒープ参照（ポインタ）。複合値（2D点・色・複素数）のフィールドがインライン格納できない
- **内容**: `(defvalue-type point2d (x :type single-float) (y :type single-float))` でスタック/レジスタにインライン格納可能な値型を宣言。コンストラクタがヒープ割り当てなしで2レジスタを返す（FR-569 Multiple Return Values）。配列内では `(array point2d 100)` が200要素の連続float配列としてレイアウト（AoS→SoA不要）。同一性比較は値比較。Java Project Valhalla（JEP 401）/ Swift value types / .NET struct と同等
- **根拠**: 座標・色・クォータニオン等の小さな複合値のヒープ割り当てゼロ。数値集中コードの根本的なメモリ効率改善
- **難易度**: Hard

---

### Phase 134 — セキュリティ高度化 II

#### FR-746: JIT Hardening / Code Integrity Protection (JIT強化・コード整合性保護)

- **対象**: `src/vm/vm-execute.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: JITコードバッファ（FR-060）はW^Xで基本保護。JITスプレー攻撃・定数埋め込みのexploitation対策なし
- **内容**: **定数ブラインディング（Constant Blinding）**: JITコード内の即値定数を `const XOR random_key` として埋め込み、使用直前にXOR復元（Blazakis 2010）。**NOP sled 除去**: JIT生成コードに予測可能なpadding NOP列を生成しない。**Guard pages**: JITコードバッファの前後にGUARDページを配置しバッファオーバーランを即座に検出。**W⊕X 強化**: JITコード書き込み中は実行不可（mprotect切り替え）。V8 JIT hardening / WebKit JSC JIT protections と同等
- **根拠**: JIT-spray攻撃（CVE-2016-4622等）の緩和。セキュリティ重視環境でcl-ccを使用する前提条件
- **難易度**: Hard

#### FR-747: Runtime Sandboxing / Seccomp (ランタイムサンドボックス)

- **対象**: `src/cli/main.lisp`, `src/vm/vm-execute.lisp`
- **現状**: cl-cc プロセスは全syscallにアクセス可能。ユーザー提供コードの実行時にsyscall制限なし
- **内容**: `./cl-cc run --sandbox foo.lisp` で untrusted コードをサンドボックス内実行。Linux: `seccomp(SECCOMP_SET_MODE_FILTER)` で許可syscallをwhitelist（read/write/mmap/brk/exit のみ）。macOS: `sandbox_init(3)` / `pledge(2)` (OpenBSD互換レイヤ)。サンドボックス内でのネットワーク・ファイル書き込み・execveを禁止。違反時は`SIGKILL`ではなく`SIGSYS`でcontext情報付き終了。Seccomp-BPF / WASM sandbox / Deno の permission モデルを参考
- **根拠**: cl-cc REPL・オンライン評価サービス・プラグインシステムでの安全なコード実行。Turing-complete言語の評価に不可欠
- **難易度**: Hard

#### FR-748: Code Signing (コード署名)

- **対象**: `src/binary/macho.lisp`, `src/cli/main.lisp`
- **現状**: 生成バイナリに署名なし。macOS Gatekeeper・iOS / tvOSへのデプロイ不可
- **内容**: `./cl-cc compile --sign "Developer ID: ..." foo.lisp` で生成バイナリにコード署名。**macOS**: `codesign --sign` ラッパー、Mach-Oの`LC_CODE_SIGNATURE`ロードコマンド埋め込み。**Apple Silicon**: `MAP_JIT` フラグ使用時はentitlements（`com.apple.security.cs.allow-jit`）必須。**Windows**: Authenticode署名（`signtool.exe`）の呼び出しラッパー。**Linux**: IMA（Integrity Measurement Architecture）署名サポート。署名検証モード（`--verify-signature`）でロード時チェック
- **根拠**: macOS/iOS配布の必須要件。企業セキュリティポリシーへの準拠（署名なしバイナリの実行拒否）
- **難易度**: Medium

#### FR-749: Constant Blinding in AOT Code (AOTコードの定数ブラインディング)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/binary/macho.lisp`
- **現状**: 即値定数が生成機械語にそのまま埋め込まれる。静的解析でアドレス・鍵・定数が直接読み取れる
- **内容**: AOT生成コードにおける敏感な定数（暗号鍵・ポインタ定数）をXORマスクで難読化。`MOV rax, 0xDEADBEEF` → `MOV rax, (0xDEADBEEF XOR 0x12345678); XOR rax, 0x12345678`。`--blind-constants` フラグで有効化。`(declare (cl-cc:sensitive-constant))` で個別定数に適用。FR-746（JIT Hardening）のAOT版。ASLR と組み合わせてコード内定数の予測困難化
- **根拠**: リバースエンジニアリング・バイナリパッチング・定数スキャンによるシークレット抽出への対策
- **難易度**: Medium

---

### Phase 135 — 静的解析高度化 II

#### FR-752: k-CFA / Context-Sensitive Analysis (k-CFA・文脈依存解析)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: エスケープ解析（FR-231）・ポインタ解析（FR-236）は文脈非依存（0-CFA相当）。同一関数が異なる呼び出し文脈で異なる挙動をする場合に精度不足
- **内容**: **1-CFA**: 呼び出しサイトを文脈として追跡（各呼び出し箇所で関数を別々に解析）。**Object Sensitivity**: 受信オブジェクトの割り当てサイトを文脈とする（OO言語に効果的）。解析精度と計算コストのトレードオフを `--cfa-depth 1` で制御。false positivesの削減によりエスケープ解析・エイリアス解析の精度向上。Shivers' k-CFA（1988）/ Milanova Object Sensitivity（2002）の実装
- **根拠**: 0-CFAでエスケープありと誤検出されるクロージャをスタック割り当て可能と正確に判定。最適化精度の根本的向上
- **難易度**: Very Hard

#### FR-753: Datalog-Based Static Analysis / Soufflé (データログベース静的解析)

- **対象**: `src/optimize/optimizer.lisp`, 新規 `src/analysis/datalog.lisp`
- **現状**: 静的解析は手続き的なVisitorパターンで実装。複数の解析間のデータ流通が複雑でメンテナンス困難
- **内容**: Souffle / Doop スタイルの **Datalog エンジン**を内蔵。解析を宣言的なDatalogルールとして記述: `reachable(A, B) :- call(A, B). reachable(A, C) :- reachable(A, B), call(B, C).`。底辺不動点計算で到達可能関数集合・エスケープ関係・エイリアス集合を同時計算。並列BFS/SCC計算で大規模プログラムにスケール。ルールファイルのホットリロード（`--reload-analysis`）で解析の拡張が容易
- **根拠**: コールグラフ構築・エスケープ解析・情報フロー解析を統一されたDeclarativeフレームワークで記述。解析の合成（compositionality）が自然
- **難易度**: Very Hard

#### FR-754: Ownership Inference / Borrow Checker (所有権推論・借用チェック)

- **対象**: `src/type/`, `src/compile/codegen.lisp`
- **現状**: メモリ管理はGCに依存（FR-GC系）。所有権・借用の概念なし。GCを使わない安全なメモリ管理の検証不可
- **内容**: Rustスタイルの所有権型システムをオプトイン機能として追加。`(declare (cl-cc:owned x))` で所有権変数を宣言。`(cl-cc:borrow x)` で不変借用、`(cl-cc:borrow-mut x)` で可変借用（同時に1つのみ）。ライフタイムパラメータ: `(defun foo (x &lifetime 'a) (declare (cl-cc:lifetime x 'a)) ...)` で明示的ライフタイム。借用チェッカーはuse-after-free・double-freeをコンパイル時に検出。`--no-gc` モードと組み合わせてGCフリーモジュールを実現
- **根拠**: ヒープGCに依存しないリアルタイム・組み込み（FR-605）コードの安全性保証。Rust の安全性モデルをLispに段階的に導入
- **難易度**: Very Hard

#### FR-755: Region Inference (リージョン推論)

- **対象**: `src/type/`, `src/runtime/heap.lisp`
- **現状**: FR-228（Arena Allocator）はスコープ単位の手動アリーナ管理。プログラム解析によるリージョン自動割り当て推論なし
- **内容**: ML Kit（Tofte & Talpin 1994）のリージョン推論: 各割り当てサイトに **リージョン注釈** を自動付与。リージョンの生存区間を解析してスタックベースの割り当て/解放を生成。GCなしで安全なメモリ管理。`(cl-cc:with-region r1 r2 ...)` でリージョンを明示。自動推論: `let r = inferRegion(expr) in allocate(in=r) ...`。リージョンプロファイリング（実行時リージョンサイズ統計）で推論精度を改善
- **根拠**: GCポーズなしの予測可能なメモリ管理。ML Kit実証: SMLプログラムの60〜80%でGCなし実行が可能
- **難易度**: Very Hard

---

### Phase 136 — GHCスタイル変換

#### FR-758: Demand / Strictness Analysis (需要・正格性解析)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: 関数引数の評価タイミング（正格/遅延）をコンパイラが判定していない。FR-653（Lazy Eval）のthunkが常に生成される
- **内容**: **正格性解析**: 関数 `f x` が `x` を常に評価するか（**正格**）を解析。正格な引数はthunk生成なしに評価済み値を渡せる。**需要解析**: 引数が一度だけ評価されるか・全く評価されないかを追跡（シングル使用→移動、未使用→削除）。GHC Demand Analyzer（Peyton Jones et al.）の実装。`(declare (cl-cc:strict x))` で手動正格性宣言
- **根拠**: 遅延評価（FR-653）のサンク生成コストを正格引数で排除。GHC実績: 多くの関数の引数がほぼ全て正格と判定され、thunk生成を90%削減可能
- **難易度**: Hard

#### FR-759: Case-of-Case Transformation (case-of-case変換)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: ネストしたcond/typecase/パターンマッチがそのままコンパイル。外側のmatch結果を内側のmatchが再度検査する冗長性
- **内容**: `(case (case e p1→e1 p2→e2) q1→body1 q2→body2)` を `(case e p1→(case e1 q1→body1 q2→body2) p2→(case e2 q1→body1 q2→body2))` に変換（caseをpushdown）。変換後はFR-728（Decision Tree）がより良い決定木を生成。body重複はFR-731（Join Points）で共有。GHC case-of-case transformation（Simon Peyton Jones 1996）の実装
- **根拠**: CLOSディスパッチのネスト・型タグ検査の重複をコンパイル時に統合。JIT非依存で静的最適化可能
- **難易度**: Medium

#### FR-760: Let Floating / Binding Migration (let浮かし・束縛移動)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: `let`束縛はソースコードの位置にコンパイル。ループ不変式をループ外に移動するLICM（FR-031）とは別にlet束縛レベルでの移動なし
- **内容**: **Float-Out（outward）**: 不変`let`をより外側のスコープ（ループ外・関数先頭）に移動。`(dotimes (i n) (let ((k (heavy-compute))) ...))` → `(let ((k (heavy-compute))) (dotimes (i n) ...))`。**Float-In（inward）**: 使用箇所に近い場所に束縛を移動し、使われないブランチでの評価を回避。`(let ((x e)) (if c x 0))` → `(if c (let ((x e)) x) 0)`（`c`が偽の場合`e`を評価しない）。GHC let-floating transformation / LICMの補完
- **根拠**: LICM がVM命令単位で動くのに対し、let-floatingはAST/CPS段階で高水準な動作。両者の協調で最大の効果
- **難易度**: Medium

#### FR-761: Simplification via Rewrite Rules (書き換えルールによる単純化)

- **対象**: `src/optimize/optimizer.lisp`, `src/expand/expander.lisp`
- **現状**: 最適化ルールは`optimizer.lisp`の手続き的コード。ルール追加にソース変更が必要
- **内容**: `(define-rewrite-rule :simplify (+ x 0) x)` でユーザー定義書き換えルールを登録。GHC `RULES` pragma / Term Rewriting System（TRS）の設計。ルール: 左辺パターン → 右辺変換、適用条件（guard）付き。マッチングはE-graph（FR-egraph）またはLinear scan。組み込みルール: `(* x 0) → 0`, `(not (not x)) → x`, `(append nil x) → x`。ルールの合流性・停止性を検査する`--verify-rules`フラグ。Knuth-Bendix completion の簡易版
- **根拠**: ドメイン固有最適化（暗号・DSP・線形代数）をコンパイラ本体変更なしに追加可能。プラグイン（FR-700）との統合でユーザー定義最適化
- **難易度**: Medium

---

### Phase 137 — スタック・スレッド管理

#### FR-764: Segmented / Growable Stacks (セグメント化・伸長可能スタック)

- **対象**: `src/vm/vm-execute.lisp`, `src/vm/vm-run.lisp`
- **現状**: 各スレッド（FR-576 Work-Stealing）は固定サイズスタック。深い再帰でスタックオーバーフロー
- **内容**: **セグメント化スタック（Split Stacks）**: スタックフレームが現在セグメントに収まらない場合、新セグメントをヒープから割り当て。スタック上限なし（メモリ許す限り再帰可能）。各関数エントリに stack-overflow-check prologue を挿入（`cmp rsp, [tls_stack_limit]`）。**コピースタック（Copying Stack）**: スタック不足時に2倍サイズにrealloc（継続のアドレスを更新）。Go goroutine の growable stack / GCC Split Stacks (-fsplit-stack) / Chicken Scheme のcheney-on-the-mta と同等
- **根拠**: FR-576 Green Thread の根本的問題（スタックサイズ事前確定）を解消。相互再帰の深さ制限なし
- **難易度**: Hard

#### FR-765: Stack Overflow Detection / Guard Pages (スタックオーバーフロー検出)

- **対象**: `src/vm/vm-execute.lisp`, `src/cli/main.lisp`
- **現状**: 深い再帰でSEGFAULT（シグナルハンドラなし）。デバッグ情報なしにクラッシュ
- **内容**: スタックの末尾に **ガードページ**（`mmap(PROT_NONE)`）を配置。アクセス時に`SIGSEGV` / `SIGBUS`を補足し、`(stack-overflow-error "Stack overflow at depth N: call chain ...")`に変換。バックトレース付きのエラーメッセージ生成。`--stack-size 8mb` でスタックサイズ設定。代替スタック（`sigaltstack`）でシグナルハンドラ自体がスタックオーバーフローしない設計。SBCL / JVM stack overflow handling と同等
- **根拠**: 現状の即死クラッシュをデバッグ可能なエラーに変換。テスト中の無限再帰バグの診断を劇的に改善
- **難易度**: Medium

#### FR-766: Trampolining (トランポリン最適化)

- **対象**: `src/compile/cps.lisp`, `src/compile/codegen.lisp`
- **現状**: TCO（FR-005）は末尾呼び出しのみ対応。相互再帰関数（A→B→A→B...）でTCOが適用できない実装がある
- **内容**: **トランポリン**: 末尾呼び出しを返り値として包み、ループで実行するパターンを自動生成。`(defun even (n) (if (= n 0) t (odd (- n 1))))` / `(defun odd (n) ...)` → `trampoline-loop`が`(lambda () (even n))`を繰り返し呼び出す。コンパイラが相互再帰を検出してトランポリンに変換。`(declare (cl-cc:trampoline))` で手動適用。`(cl-cc:make-thunk f)` / `(cl-cc:trampoline thunk)` の手動APIも提供。Scheme trampolining / Clojure `trampoline` 関数と同等
- **根拠**: 相互再帰での完全スタック定数化。Odd-Even テスト・状態機械実装・CPS変換後継続チェーンに直結
- **難易度**: Medium

#### FR-767: Coroutine Stack Management (コルーチンスタック管理)

- **対象**: `src/vm/vm-execute.lisp`, `src/vm/vm-run.lisp`
- **現状**: FR-166（Coroutines）は基本的なco-routine yield。スタック状態の完全保存/復元の詳細実装なし
- **内容**: コルーチン（FR-166）の**スタックフレーム完全スナップショット**: yield時にrsp/rbp/全callee-savedレジスタをコルーチンオブジェクトに保存。resume時に完全復元。**対称コルーチン** (`transfer-to`): 呼び出し元に戻らずに別コルーチンへ直接スイッチ。スタックコピー vs スタックポインタスワップの実装比較。`ucontext_t` / `longjmp` / POSIX makecontext/swapcontext との比較設計。spill map（FR-550 Stack Maps）との統合でGCがコルーチンスタックをスキャン可能
- **根拠**: 非同期I/O（FR-167 Async/Await）・Generator（FR-653 Lazy Eval）の実装基盤。スタック保存の詳細が正確でないとコルーチン跨ぎのGCが危険
- **難易度**: Hard

---

### Phase 138 — SIMD・スケジューリング拡張

#### FR-770: Masked Vector Operations / AVX-512 (マスクベクトル演算・AVX-512)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: SIMD（FR-035）は全要素処理のみ。条件付き要素処理（if-vectorize）が難しくif-conversionで諦めるケースあり
- **内容**: AVX-512マスクレジスタ（`k0`〜`k7`）を活用した条件付きベクトル演算。`(cl-cc:vmask (> a 0) (vm-add a b))` → `VADDPS zmm0{k1}, zmm1, zmm2` のように条件をマスクレジスタで表現。`VPMASKMOVD` でスキャッター/ギャザーのマスク付き版。FR-621（Auto-Parallelization）のif-body ベクトル化を可能に。ARM SVE `WHILELT`/`PTEST` 命令との統一インターフェース
- **根拠**: if-bodyを持つループの自動ベクトル化が2倍以上の適用範囲拡大。AVX-512搭載Intel Ice Lake / SPR での実性能向上
- **難易度**: Hard

#### FR-771: Scalable Vector Extension / SVE and RVV (スケーラブルベクトル拡張)

- **対象**: `src/backend/aarch64.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: SIMD（FR-035）は固定幅（128/256bit）。ARM SVE / RISC-V Vector（RVV）のスケーラブルベクトル幅未対応
- **内容**: **ARM SVE**: ベクトル長非依存コード生成（vscale=vector length / 128bit）。`PTRUE p0.s, ALL` + `LD1W z0.s, ...` + `FADD z0.s, ...` の VL-agnostic命令列。**RISC-V RVV**: `vsetvli t0, a0, e32, m1` でvlを動的設定し、`vle32.v v0, (a1)` / `vfadd.vv v2, v0, v1`。ループ後処理（epilogue）なしの自然なVL制御。ARMv9 SVE2 / RVV 1.0 spec 完全準拠
- **根拠**: Apple M4（SVE2）/ SiFive P870（RVV）での実行で128bit固定より2〜8倍高いSIMD幅を自動活用
- **難易度**: Hard

#### FR-772: Modulo Scheduling (モジュロスケジューリング)

- **対象**: `src/optimize/optimizer.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: 命令スケジューリング（FR-033）は基本ブロック単位。ループカーネルの反復間命令オーバーラップなし
- **内容**: **ソフトウェアパイプライニングのカーネル生成**: 複数ループ反復の命令を **インターリーブ**して実行ユニット利用率を最大化。II（Initiation Interval）= `max(ResourceII, RecurrenceII)` を計算。Modulo Instruction Scheduling（MIS）アルゴリズムでprolog/kernel/epilogのコード生成。`(declare (cl-cc:software-pipeline))` で明示指定。FR-036（Software Pipelining）の精度向上版：完全なモジュロスケジューラ
- **根拠**: 数値計算ループでFPユニット・ロードユニットを同時フル活用。実測で2〜4倍のループスループット向上（レイテンシ隠蔽）
- **難易度**: Very Hard

#### FR-773: Instruction Throughput vs Latency Optimization (スループット・レイテンシトレードオフ最適化)

- **対象**: `src/optimize/optimizer.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: 命令スケジューリングはレイテンシ最小化のみ。スループット（1サイクル当たりの命令完了数）の最大化なし
- **内容**: CPU実行ポートのモデル（Agner Fog's microarchitecture tables相当）を内蔵。AMD Zen4 / Intel Raptor Lake のポートマッピングデータ。スケジューラが **critical path length** vs **resource utilization** のバランスを最適化。実行ポートが偏る命令列を分散（`div`/`sqrt`は1ポートのみ: 前後に他ポート命令を配置）。LLVM `MachineCombiner` / GCC `-fsched-pressure` と同等
- **根拠**: Out-of-Order CPUの全実行ポートを均等に使うことで、シングルポートボトルネックを排除。FP集中コードで15〜30%の実効スループット向上
- **難易度**: Hard

---

### Phase 139 — ABI・シンボル管理

#### FR-776: Name Mangling (名前マングリング)

- **対象**: `src/compile/codegen.lisp`, `src/backend/x86-64-codegen.lisp`, `src/binary/macho.lisp`
- **現状**: 関数シンボル名は`cl_cc__function_name`形式の単純変換。generic function specializer・パッケージ・型パラメータのエンコードなし
- **内容**: Itanium C++ ABI マングリング規則（`_Z` prefix）に準拠したcl-cc独自マングリングスキーム定義。`(cl-cc:foo integer)` → `_Z5fooi`相当。パッケージ名: `cl_cc::bar::baz` → `_ZN6cl_cc3bar3bazE`。generic specializer: `(method add (integer integer))` → `_ZN3add_method_integer_integerE`。`./cl-cc demangle _Z5fooi` で逆変換。C++との相互運用時はC++ ABI マングリング規則に従ったシンボルも生成可能
- **根拠**: デバッガ（GDB/LLDB）・プロファイラ・objdumpでのシンボル可読性。Cライブラリとのリンク時の名前衝突回避
- **難易度**: Medium

#### FR-777: ABI Stability Manifest (ABI安定性マニフェスト)

- **対象**: `src/compile/codegen.lisp`, `src/cli/main.lisp`, `src/binary/macho.lisp`
- **現状**: ライブラリの公開APIのABIが変更された場合でも自動検出なし。破壊的ABI変更がサイレントに発生する可能性
- **内容**: `./cl-cc abi-dump foo.lisp > foo.abi` で公開関数のシグネチャ・struct レイアウト・enum値をABIダンプ。`./cl-cc abi-check foo.abi foo-new.lisp` でABI互換性検査: 関数シグネチャ変更・struct サイズ変更・enumの値変更を検出してエラー報告。セマンティックバージョン（SemVer）に対応: MAJOR変更 = ABI破壊、MINOR = ABI後方互換追加。`(declare (cl-cc:stable-abi))` で明示的ABI安定性宣言。libabigail / ABI Compliance Checker と同等
- **根拠**: cl-ccをライブラリとして使うプロジェクトへのABI保証。major versionをまたぐアップグレードの安全性確認
- **難易度**: Medium

#### FR-778: Debug Symbol Stripping Modes (デバッグシンボル除去モード)

- **対象**: `src/binary/macho.lisp`, `src/cli/main.lisp`
- **現状**: `--strip` フラグで全シンボル削除のみ。デバッグ情報の選択的保持・外部ファイル分離なし
- **内容**: `--strip all`: 全シンボルとデバッグ情報削除（最小バイナリ）。`--strip debug`: デバッグ情報のみ削除（公開シンボル保持）。`--strip unneeded`: 未定義参照に不要なシンボルのみ削除。`--split-debug`: FR-583（Split Debug Info）でバイナリとデバッグ情報を分離し、本番デプロイ+後からデバッグ可能に。`dSYM` (macOS) / `.dwp` (Linux) 形式での出力。`--debuglink` でバイナリとdebug fileのリンクを埋め込み
- **根拠**: 本番バイナリのサイズ最小化とデバッガビリティの両立。クラッシュレポートから後追いデバッグするワークフロー
- **難易度**: Easy

#### FR-779: Symbol Namespace Management (シンボル名前空間管理)

- **対象**: `src/package.lisp`, `src/compile/codegen.lisp`, `src/vm/symbols.lisp`
- **現状**: パッケージシステム（CL標準）は基本的な名前空間管理のみ。階層的名前空間・プライベートシンボル・リエクスポートの精緻な制御なし
- **内容**: `(cl-cc:define-namespace cl-cc.compiler.backend.x86 ...)` で階層的名前空間定義。`(cl-cc:private-symbol foo)` でパッケージ内限定シンボル（エクスポート不可）。`(cl-cc:reexport-from :dep :only (bar baz))` で選択的再エクスポート。循環依存検出: `(cl-cc:check-namespace-deps)` でパッケージ依存グラフのSCC検出。`--namespace-graph` でGraphviz出力。Java module system / Racket module system / OCaml module aliases の概念を統合
- **根拠**: cl-cc自身の600+エクスポートシンボルの管理が困難。内部実装シンボルと公開APIの明確な分離
- **難易度**: Medium

---

### Phase 140 — Lisp固有最適化

#### FR-782: Uncurrying / Eta Reduction (アンカリー化・イータ簡約)

- **対象**: `src/optimize/optimizer.lisp`, `src/expand/expander.lisp`
- **現状**: `(lambda (x) (f x))` のようなラッパーラムダがインライン化前に残存。不要な関数オブジェクト生成
- **内容**: **イータ簡約**: `(lambda (x) (f x))` → `#'f`（ラムダが引数をそのまま転送するのみ）をコンパイル時に検出して削除。`(mapcar (lambda (x) (1+ x)) xs)` → `(mapcar #'1+ xs)`。**アンカリー化**: 多引数関数を単一引数カリー化形式から変換（Haskellのuncurry変換のLisp版）。`(funcall (curry f a) b)` → `(f a b)`。FR-689（Lambda Lifting）と組み合わせでクロージャを完全排除
- **根拠**: `mapcar` / `reduce` / `sort` のような高階関数呼び出しで暗黙ラムダのクロージャ割り当てを排除。熱いループでのGCプレッシャー削減
- **難易度**: Easy

#### FR-783: Closure Shrinking (クロージャ縮小)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: クロージャは定義時スコープの全自由変数をキャプチャ。使用しない変数もキャプチャリストに含まれる
- **内容**: クロージャの自由変数リストをコード解析で精緻化し、実際に使用される変数のみをキャプチャ。`(let ((a 1) (b 2) (c 3)) (lambda () (+ a b)))` → cのキャプチャ不要。FR-231（Escape Analysis）との統合でキャプチャ変数がエスケープするかも解析。キャプチャ変数が0個 → クロージャから関数ポインタへ変換（FR-689 Lambda Lifting と協調）。クロージャのメモリフットプリントを最小化
- **根拠**: cl-ccのマクロ展開コード（多くのlet束縛を持つclosure）でキャプチャ変数を50%以上削減できるケースが多い
- **難易度**: Medium

#### FR-784: Predicate Dispatch Optimization (述語ディスパッチ最適化)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **現状**: CLOSのmethod specializerは型・`eql`のみ。述語ベースのdispatch（Clojureのmultimethod like）の最適化なし
- **内容**: `(defmethod process ((x (and integer (> x 0)))) ...)` のような **述語specializer** をサポート。コンパイラが述語を事前評価し、型チェックと述語チェックの最適実行順序を決定木（FR-728）で生成。条件が排他的な場合は`cond`チェーンに変換、重複する場合は前向き連鎖（forward chaining）で効率化。Clojure multimethods / ELisp `cl-defmethod :extra` / Dylan sealing との比較設計
- **根拠**: 数値の範囲・文字列のパターン・複合条件でのdispatchを型ベースdispatchと同等の速度で実行
- **難易度**: Hard

#### FR-785: Uniqueness Types (一意型)

- **対象**: `src/type/`, `src/compile/codegen.lisp`
- **現状**: 参照共有は無制限。同一オブジェクトへの複数参照があると**インプレース更新**が安全に行えない（副作用が予期せず波及）
- **内容**: `(: sort-unique! ((unique (array integer)) -> (unique (array integer))))` で `unique` 型アノテーション付き関数を定義。`unique` 値は最大1つの参照のみ許可（コンパイル時チェック）。`unique` 値への破壊的操作（`setf`・`aref=`）が安全（他の参照がない保証）。GCなしでインプレース更新を安全化。`copy-out`: 一意性を諦めて共有可能にする変換。Clean 言語の uniqueness types / Idris 2 linear types との比較
- **根拠**: 配列ソート・文字列処理・バイトバッファ操作でコピー不要の破壊的更新を型安全に実現。FR-659（COW）の型システムレベルでの強化
- **難易度**: Hard

---

### Phase 141 — 数値・浮動小数点拡張

#### FR-788: FMA / Fused Multiply-Add Instructions (融合積和演算)

- **対象**: `src/vm/primitives.lisp`, `src/backend/x86-64-codegen.lisp`, `src/backend/aarch64.lisp`
- **現状**: `(+ (* a b) c)` は乗算命令 + 加算命令の2命令。中間値の丸め誤差あり
- **内容**: `(cl-cc:fma a b c)` → `VFMADD213SD xmm0, xmm1, xmm2`（x86-64 FMA3）/ `FMADD d0, d1, d2, d3`（AArch64）で単一命令実行。丸めが1回のみ（IEEE 754-2008 fusedMultiplyAdd 準拠）で数値精度向上。コンパイラが `(+ (* a b) c)` パターンを自動認識してFMAに変換（`--fuse-fma` フラグ）。SIMD版: `VFMADD213PS ymm0` で8要素同時FMA（FR-035 SIMD統合）。`--strict-fp` 時はFMA融合を禁止（精度優先）
- **根拠**: 行列乗算・ドット積・多項式評価でFMAが乗算+加算の2命令を1命令に置換。スループット2倍 + 丸め誤差半減
- **難易度**: Medium

#### FR-789: FP Reassociation / Fast-Math Mode (浮動小数点再結合・高速数学モード)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: 浮動小数点演算はIEEE 754厳密セマンティクス。結合則不成立（`(a+b)+c ≠ a+(b+c)` for FP）のため再配置不可
- **内容**: `--fast-math` フラグで非厳密FP最適化を有効化。`-fno-signed-zeros`: `-0.0 == 0.0`。`-ffinite-math-only`: NaN/Inf発生なし前提。`-fassociative-math`: 和の順序変更許可 → 自動ベクトル化（FR-035）の精度条件を緩和。`-fno-rounding-math`: 丸めモード変更なし前提。`(declare (cl-cc:fast-math))` で関数単位適用。GCC `-ffast-math` / Clang `-ffast-math` / Julia `@fastmath` と同等
- **根拠**: 科学計算コードで`--fast-math`により20〜40%高速化が一般的。数値精度より速度優先の用途に
- **難易度**: Easy

#### FR-790: Half-Precision / BF16 Support (半精度・BF16浮動小数点サポート)

- **対象**: `src/vm/primitives.lisp`, `src/backend/x86-64-codegen.lisp`, `src/backend/aarch64.lisp`
- **現状**: 浮動小数点は64ビット倍精度のみ。AI推論・グラフィクス向けの16ビット浮動小数点なし
- **内容**: `cl-cc:float16` (IEEE 754 half-precision, 5指数+10仮数ビット) と `cl-cc:bfloat16` (Google BF16, 8指数+7仮数ビット) を型システム（FR-type系）に追加。x86-64: AVX-512 FP16命令 (`VADDPH`, `VCVTPS2PH`) を活用。AArch64: ARMv8.2 `FADD h0, h1, h2` / SVE2 FP16。`(cl-cc:the float16 x)` アノテーションで自動変換。推論専用モード: `(declare (cl-cc:quantize :bf16))` でモデル全体をBF16化
- **根拠**: LLM推論でBF16はFP32比メモリ半減・演算2倍速。NPU/GPU ターゲット（FR-647）との型整合性
- **難易度**: Medium

#### FR-791: Interval Arithmetic (区間演算)

- **対象**: 新規 `src/numeric/interval.lisp`, `src/type/`
- **現状**: 浮動小数点誤差の上界が不明。数値計算の信頼性保証なし
- **内容**: `(cl-cc:interval lo hi)` で区間値を表現。演算: `(+ [a,b] [c,d]) = [a+c, b+d]`、`(* [a,b] [c,d]) = [min(ac,ad,bc,bd), max(...)]`。IEEE 754 丸め方向制御（`FLDCW` / `FRNDINT`）で厳密な上界・下界を計算。`(cl-cc:guaranteed-result expr)` で式の値の誤差上界を返す。MPFI / Arith ライブラリ相当を純CLで実装。`(declare (cl-cc:verified-fp))` で自動区間追跡モード
- **根拠**: 数値検証・物理シミュレーション・金融計算での丸め誤差保証。「計算結果が真値の ε 以内」を証明可能に
- **難易度**: Hard

---

### Phase 142 — コンパイラ堅牢性

#### FR-794: Compiler Fuzzing / Random Program Generation (コンパイラファジング)

- **対象**: `src/cli/main.lisp`, 新規 `src/testing/fuzzer.lisp`
- **現状**: コンパイラのバグ発見はユーザー報告・既存テストのみ。ランダム入力によるコンパイラクラッシュ・誤コード生成の体系的探索なし
- **内容**: `./cl-cc fuzz --seed 42 --count 10000` でランダムなCL式を生成してコンパイル。生成戦略: 文法ベース生成（産生規則から再帰的に構築）+ 変異ベース（既存テストケースを変異）。**差分テスト**: 同じ式をSBCL・cl-ccの両方で実行し結果不一致を報告（FR-453 Differential Testingの強化版）。`--oss-fuzz` モードでGoogle OSS-Fuzz統合（libFuzzer ABI互換）。見つかったバグをminimize（FR-796 Creduce）して自動バグレポート生成
- **根拠**: GCC/Clang は継続的なコンパイラファジングで年間数百のバグを発見。selfhosting後のコンパイラ正当性保証に不可欠
- **難易度**: Hard

#### FR-795: Translation Validation (翻訳検証)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 最適化パスの正しさはテスト（4322件）で担保。個々の変換ステップの意味論的等価性確認なし
- **内容**: 各コンパイルパス（CPS変換・最適化・コード生成）の**入出力ペアを自動検証**。Necula（1998）の Translation Validation: 変換前後のプログラムをSMTソルバー（FR-246）に送出し等価性をチェック。等価でない場合はコンパイラバグとして即座に報告。`--validate-transforms` フラグで有効化（コンパイル時間3〜10倍）。CI環境でサンプリング実行（全入力の1%）
- **根拠**: 最適化バグは「正しく動いていた関数が最適化後に壊れる」という最も発見困難な種類。変換ごとの自動検証が根本解
- **難易度**: Very Hard

#### FR-796: Test Case Reduction / C-Reduce Style (テストケース縮小)

- **対象**: `src/cli/main.lisp`, 新規 `src/testing/reducer.lisp`
- **現状**: コンパイラバグ報告時、ユーザーが手動でテストケースを縮小。数百行のコードが最小化されないまま報告
- **内容**: `./cl-cc reduce --crash bug.lisp` でバグを再現しつつソースを自動縮小。縮小戦略: トップレベルフォーム削除 → 式の単純化（`(complex-expr)` → `nil`）→ 識別子の短縮 → 定数の縮小。Δ-デバッグ（Andreas Zeller 1999）/ C-Reduce（PLDI 2012）のアルゴリズムを実装。`--property "crash"` / `--property "wrong-output"` で縮小目標を指定。縮小済みケースを `./cl-cc bug-report` で自動提出
- **根拠**: コンパイラバグの再現コードが数百行→数行に縮小されることで修正速度が10倍向上。GCC / Clang での実証済み
- **難易度**: Medium

#### FR-797: Compile-Time Assertions / static_assert (コンパイル時アサーション)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: `assert` は実行時のみ。型システム（FR-type系）の制約以外のコンパイル時検証なし
- **内容**: `(cl-cc:static-assert (= (cl-cc:size-of my-struct) 16) "struct must be 16 bytes")` でコンパイル時評価。評価失敗時はコンパイルエラー（ファイル・行番号付き）。対象: 型サイズ検証・定数式の範囲チェック・機能フラグ依存チェック・プラットフォーム仮定。`(cl-cc:static-assert-type x integer)` で変数の型推論結果の確認。`(cl-cc:static-assert-pure fn)` で関数が副作用なしを確認。C11 `_Static_assert` / C++17 `static_assert` / Rust `const { assert!(...) }` と同等
- **根拠**: 構造体レイアウト・整列・定数値の仮定をコンパイル時に文書化・検証。ポーティング時の前提条件違反を即座に検出
- **難易度**: Easy

---

### Phase 143 — フロントエンド拡張

#### FR-800: Staged Metaprogramming / MetaML Style (段階的メタプログラミング)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: マクロは展開時(stage-0)のみ。コード生成の段階（ステージ）を型システムで追跡できない
- **内容**: `(cl-cc:bracket expr)` で次ステージのコードを引用、`(cl-cc:splice expr)` で現ステージに埋め込み。`(cl-cc:run expr)` で次ステージコードを現ステージで実行。型システムがステージ番号を追跡: `(code 0 integer)` = stage-0の整数コード片。BER MetaML（Taha & Sheard 1997）/ MetaOCaml / Scala LMS の設計。コンパイル時に任意のコード生成プログラムを安全に実行。`(declare (cl-cc:stage 1))` で2ステージコンパイルを宣言
- **根拠**: 行列演算・フォーマットパーサ・暗号ラウンド関数のコンパイル時特化生成。実行時オーバーヘッドゼロの高度な部分評価（FR-607 Futamura 投影の型安全版）
- **難易度**: Very Hard

#### FR-801: First-Class Modules / Module Functors (一級モジュール・ファンクタ)

- **対象**: `src/package.lisp`, `src/expand/expander.lisp`, `src/vm/vm-execute.lisp`
- **現状**: パッケージは静的な名前空間。関数で受け渡せるモジュールなし
- **内容**: `(defmodule Ordered (type t) (val compare (-> t t ordering)))` でモジュール型（signature）定義。`(defstruct-module IntOrdered (type t integer) (val compare (lambda (a b) (cmp a b))))` でモジュール実装。`(defmodule-functor (Map (Ord : Ordered)) ...)` でパラメータ化モジュール（functor）定義。`(instantiate Map IntOrdered)` でインスタンス化。SML / OCaml module system の設計を踏襲。コンパイラが functor instantiation を特化（monomorphization FR-712と協調）
- **根拠**: 型クラス（FR-711）の代替として、より表現力豊かなモジュール抽象。コレクション実装（Map/Set）の型安全な交換可能性
- **難易度**: Very Hard

#### FR-802: String Interpolation (文字列補間)

- **対象**: `src/parse/lexer.lisp`, `src/expand/expander.lisp`
- **現状**: 文字列フォーマットは `(format nil "~A ~A" x y)` のみ。コンパイル時構文検査なし
- **内容**: `#"Hello, #{name}! You have #{count} messages."` リーダーマクロで文字列補間。`#{}` 内は任意のCL式（コンパイル時型チェック）。`(cl-cc:format-string "~A #{x}")` で format 指令と補間の混在。コンパイル時に `format` 指令の引数数・型整合を検査（FR-085 compile-time format 強化）。マルチライン文字列: `#"""..."""` 構文。Kotlin `"$name"` / Scala `s"..."` / Haskell QuasiQuotes と同等
- **根拠**: `format nil` の書き間違い（引数不足・型不一致）がコンパイル時に検出可能。コード可読性の大幅改善
- **難易度**: Medium

#### FR-803: Source Location Propagation (ソース位置情報伝播)

- **対象**: `src/parse/cl/parser.lisp`, `src/compile/codegen.lisp`, `src/expand/expander.lisp`
- **現状**: AST ノードにソース位置（ファイル・行・列）の格納あり。マクロ展開・CPS変換後に位置情報が失われるケースあり
- **内容**: 全 AST ノードに `source-span` (file, start-line, start-col, end-line, end-col) を付与し、全変換パスで保持。マクロ展開後のノードは展開元フォームの位置を継承（`macro-expansion-of` 逆引き）。CPS変換・クロージャ変換後も継続して位置情報を保持し DWARF（FR-070系）へ出力。`(with-source-location loc body)` でランタイムエラーに位置情報を付加。LSP（FR-070）の `goto-definition` / `find-references` の精度向上に直結
- **根拠**: マクロ由来のエラーが「マクロ展開後のコード」ではなく「元のソース位置」を指すようになる。デバッグ体験の根本改善
- **難易度**: Medium

---

### Phase 144 — デバッグ・プロファイリング拡張

#### FR-806: Statistical CPU Profiler (統計的CPUプロファイラ)

- **対象**: `src/cli/main.lisp`, 新規 `src/profiling/cpu-profiler.lisp`
- **現状**: FR-693（Heap Profiler）はメモリ割り当て計測のみ。CPU時間の消費箇所不明
- **内容**: `./cl-cc run --cpu-profile foo.lisp` でサンプリングベースCPUプロファイリング。シグナル `SIGPROF` / `SIGALRM`（100Hz）でスタックウォーク（`backtrace(3)` / libunwind）。関数ごとの自己時間（self time）と総時間（total time）集計。`./cl-cc profview foo.clcc-prof` でフレームグラフ（Brendan Gregg SVG形式）出力。`--callgrind` フラグでValgrind Callgrind互換フォーマット出力。`--perf` フラグでLinux `perf script` 互換出力（Firefox Profiler UIで閲覧可）
- **根拠**: selfhostingコンパイルのCPUホットスポット特定。コード生成・最適化パスのどの関数が最も時間を消費するかを実測
- **難易度**: Medium

#### FR-807: PMU / Performance Counter Access (PMUパフォーマンスカウンタアクセス)

- **対象**: `src/cli/main.lisp`, 新規 `src/profiling/pmu.lisp`
- **現状**: プロファイリングは時間計測のみ。キャッシュミス・分岐予測失敗・TLBミスなどのハードウェアイベントカウンタ非対応
- **内容**: `./cl-cc run --perf-events L1-dcache-miss,branch-misses,tlb-misses foo.lisp` でハードウェアカウンタを計測。Linux: `perf_event_open(2)` syscallで`perf_event_attr`を設定。macOS: `kpc_*` framework（`ktrace` / Instruments Instruments API）。カウンタ: サイクル数・命令数・CPI（Cycles Per Instruction）・L1/L2/L3キャッシュミス・分岐予測失敗率・TLBミス・メモリ帯域幅。`(cl-cc:with-perf-counters (cycles l1-miss) body)` でコード区間を計測
- **根拠**: 「遅い」の原因がキャッシュミスなのか分岐予測失敗なのかを特定するための唯一の手段。最適化効果の定量的検証
- **難易度**: Hard

#### FR-808: Binary Analysis Tools (バイナリ解析ツール)

- **対象**: `src/binary/macho.lisp`, `src/cli/main.lisp`
- **現状**: 生成バイナリの内部構造を検査するツールなし。`objdump`/`readelf`/`nm` 相当が外部依存
- **内容**: `./cl-cc objdump foo` でMach-O/ELFセクション・シンボル・リロケーションを表示（`objdump -d` 相当）。`./cl-cc nm foo` でシンボルテーブル表示（型・サイズ・アドレス）。`./cl-cc size foo` でセクション別サイズ表示。`./cl-cc disasm --fn bar foo` で特定関数のx86-64逆アセンブル（Zydis/XED相当を純CLで実装）。`./cl-cc strings foo` で埋め込み文字列抽出。`./cl-cc headers foo` でロードコマンド・ELFヘッダ詳細表示
- **根拠**: 生成コードの検証・最適化効果の確認・セキュリティ審査に外部ツール依存を排除。cl-cc自己完結型toolchainの完成
- **難易度**: Medium

#### FR-809: Compiler Regression Bisection (コンパイラ回帰二分探索)

- **対象**: `src/cli/main.lisp`, 新規 `src/testing/bisect.lisp`
- **現状**: パフォーマンス回帰・バグ導入のコミットを特定するには手動の`git bisect`が必要
- **内容**: `./cl-cc bisect --metric "compile-time" --threshold 20% HEAD~100 HEAD` で回帰を導入したコミットを自動二分探索。各コミットでcl-ccをビルドし計測を実行。`--metric "test-pass-rate"` で正当性回帰も検出。`git bisect run` スクリプトとして動作。回帰ポイント特定後に `--analyze` フラグでdiff・コミットログを表示。FR-695（Benchmarking Framework）と統合しベンチマーク結果を自動記録・比較
- **根拠**: コンパイラ開発での性能回帰は発見が困難（数コミット前から蓄積）。自動二分探索で問題箇所を数分で特定
- **難易度**: Medium

---

### Phase 145 — ビルドシステム高度化 II

#### FR-812: Conditional Compilation / Feature Flags (条件付きコンパイル・機能フラグ)

- **対象**: `src/cli/main.lisp`, `src/expand/expander.lisp`
- **現状**: CL標準の `#+`/`#-` リーダーマクロは基本的な機能フラグのみ。ビルド設定のスコープ・継承・検証なし
- **内容**: `(cl-cc:feature-flag :jit-enabled :default t :type boolean :description "Enable JIT compilation")` で型付き機能フラグを宣言。`./cl-cc build --feature jit-enabled=false` でビルド時設定。`(cl-cc:when-feature :jit-enabled ...)` / `(cl-cc:unless-feature ...)` でコンパイル時分岐。フラグ依存グラフ（`(cl-cc:requires-feature :jit-enabled :x86-64)`）。`./cl-cc features list` で全フラグ・デフォルト値・説明を表示。Rust cargo features / C++ CMake options / Nix `enableJIT` と同等
- **根拠**: JIT・GC種別・ターゲットアーキテクチャ・セキュリティ機能を同一ソースから条件ビルドで制御
- **難易度**: Medium

#### FR-813: Package Lockfiles (パッケージロックファイル)

- **対象**: `src/cli/main.lisp`, 新規 `src/build/lock.lisp`
- **現状**: 依存関係の解決はビルド時に毎回実行。チーム内・CI環境で異なるバージョンが使われる可能性
- **内容**: `./cl-cc build` 実行時に `cl-cc.lock` ファイルを生成。全直接・間接依存のバージョン・SHA256ハッシュ・ダウンロードURL を記録。次回ビルドはlockファイルを優先し完全再現性保証（FR-662 Reproducible Builds と統合）。`./cl-cc update [package]` で選択的更新とlockファイル更新。lockファイルはgit管理対象（変更がPRで可視化）。Cargo.lock / package-lock.json / poetry.lock / Nix flake.lock と同等設計
- **根拠**: 「自分の環境では動く」問題の根絶。CIとローカルで完全同一の依存バージョンを保証
- **難易度**: Medium

#### FR-814: Dependency Vulnerability Scanning (依存関係脆弱性スキャン)

- **対ότ**: `src/cli/main.lisp`, 新規 `src/build/security-scan.lisp`
- **現状**: 依存ライブラリにCVEが存在しても検出手段なし
- **内容**: `./cl-cc audit` でFR-813 lockファイルの依存関係をOSV（Open Source Vulnerabilities）データベース / GitHub Advisory Database に照会。既知CVEが存在する依存を警告（CVSS スコア・影響範囲・修正バージョン情報付き）。`--deny high` で高リスクCVEをビルドエラーに。SBOM（FR-455 Software Bill of Materials）と統合しSPDX/CycloneDX形式でCVEマッピングを出力。Cargo audit / npm audit / pip-audit / OWASP Dependency-Check と同等
- **根拠**: ソフトウェアサプライチェーン攻撃への対策。2026年以降の企業セキュリティ要件（SLSA / EO 14028）で義務化傾向
- **難易度**: Medium

#### FR-815: Build-Time Source Code Generation (ビルド時ソースコード生成)

- **対象**: `src/cli/main.lisp`, 新規 `src/build/codegen-step.lisp`
- **現状**: ソースコードは全て手書き。ビルド時に外部スキーマ・プロトコル定義から自動生成するパイプラインなし
- **内容**: `cl-cc.build.lisp` ビルドスクリプトに `(generate-from-schema "schema.proto" :target "src/generated/proto.lisp")` を記述。生成ステップはFR-698（並列コンパイル）の依存グラフに統合。生成ファイルは再生成可能とマーク（gitignore推奨・手動編集禁止）。サポートソース: Protocol Buffers `.proto` / JSON Schema / OpenAPI YAML / DWARF type tables / SQL DDL。`./cl-cc generate` で明示的な生成実行。生成コードのFR-797（static_assert）での検証
- **根拠**: プロトコル定義の変更時にFFIバインディング・シリアライザを手動更新する作業を自動化
- **難易度**: Medium

---

### Phase 146 — I/O・OS統合

#### FR-818: Zero-Copy I/O / sendfile (ゼロコピーI/O)

- **対象**: `src/vm/io.lisp`, `src/vm/vm-execute.lisp`
- **現状**: ファイルI/OはCLの `read-sequence`/`write-sequence` 経由（ユーザー空間バッファ経由のコピーあり）
- **内容**: `(cl-cc:sendfile dst-fd src-fd count)` → Linux `sendfile(2)` / macOS `sendfile(2)` syscallを直接呼び出し。カーネル空間でのファイル→ソケット転送（ユーザー空間コピーゼロ）。`(cl-cc:splice src-pipe dst-pipe count)` → Linux `splice(2)` でパイプ間ゼロコピー。`(cl-cc:tee src dst count)` → `tee(2)` でパイプ分岐。`(cl-cc:vmsplice buf-list pipe)` → ユーザー空間バッファをパイプにゼロコピー。io_uring（FR-600）との統合でゼロコピー非同期I/O
- **根拠**: Webサーバーでの静的ファイル配信でsendfileが通常転送比2〜10倍高速（カーネル空間でのDMA転送）
- **難易度**: Medium

#### FR-819: Memory-Mapped Files / mmap (メモリマップドファイル)

- **対象**: `src/vm/io.lisp`, `src/vm/vm-execute.lisp`
- **現状**: ファイルI/OはFOPEN/READ/WRITE syscall経由。大規模ファイルのランダムアクセスが非効率
- **内容**: `(cl-cc:mmap file :read :size n)` → `mmap(2)` でファイルをアドレス空間にマッピング。返値は`cl-cc:mapped-region`オブジェクト（`(aref region i)` でバイトアクセス）。`(cl-cc:mmap-sync region)` → `msync(MS_SYNC)` で変更をディスクに同期。`(cl-cc:munmap region)` でアンマップ。`MAP_SHARED`（プロセス間共有）/ `MAP_PRIVATE`（COW）/ `MAP_ANONYMOUS`（無名マッピング）オプション。`MAP_POPULATE` で先読み。`madvise(MADV_SEQUENTIAL/RANDOM/WILLNEED)` ヒント
- **根拠**: コンパイラのソースファイル読み込み・FASLキャッシュ・大規模データ処理でmmap経由が read()より大幅に高速
- **難易度**: Medium

#### FR-820: UNIX Signal Handling (UNIXシグナルハンドリング)

- **対象**: `src/vm/conditions.lisp`, `src/vm/vm-execute.lisp`, `src/cli/main.lisp`
- **現状**: `SIGINT`（Ctrl-C）はSBCLのデフォルト処理。他のUNIXシグナルをLispのconditionとして扱う手段なし
- **内容**: `(cl-cc:handle-signal SIGTERM (lambda (sig) (cleanup-and-exit)))` でシグナルハンドラを登録。`SIGINT` → `(cl-cc:keyboard-interrupt condition)`、`SIGHUP` → `(cl-cc:hangup condition)`、`SIGCHLD` → `(cl-cc:child-status-change condition)` として CL condition systemに統合。`SIGUSR1`/`SIGUSR2` でユーザー定義シグナルを使った動的ロギングレベル変更。`SA_RESTART` フラグ制御。`sigprocmask` / `sigwaitinfo` / `signalfd`（Linux）の高水準ラッパー
- **根拠**: デーモンプロセス・長時間実行サーバー（LSP server / REPL server）でのグレースフルシャットダウン・設定リロードに必須
- **難易度**: Medium

#### FR-821: Shared Memory IPC (共有メモリIPC)

- **対象**: `src/vm/io.lisp`, `src/cli/main.lisp`
- **現状**: プロセス間通信はソケット/パイプのみ。同一マシン上の高速データ共有手段なし
- **内容**: `(cl-cc:shm-open "/my-seg" :create :size (* 4 1024 1024))` → POSIX `shm_open(3)` + `ftruncate` + `mmap`。`(cl-cc:shm-open "/my-seg" :attach)` で既存セグメントに接続。`(cl-cc:with-spinlock (shm :offset 0) body)` で共有メモリ上のスピンロック（atomic CAS）。`(cl-cc:semaphore-post sem)` / `(cl-cc:semaphore-wait sem)` → POSIX `sem_post`/`sem_wait`。Sysv IPC 代替として POSIX IPC のみ実装。ユースケース: 並列コンパイル（FR-698）プロセス間の型情報共有・LSPクライアント/サーバー間高速通信
- **根拠**: パイプ比100〜1000倍のIPC帯域幅（カーネル経由なし）。分散コンパイラキャッシュ（FR-699）のノード間通信
- **難易度**: Medium

---

### Phase 147 — 命令レベル最適化 II

#### FR-824: Macro-op Fusion Detection (マクロ命令融合検出)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 命令スケジューリング（FR-033）・スループット最適化（FR-773）はポートモデルを使用。CPUの自動macro-op fusionの利用最大化なし
- **内容**: Intel/AMD CPUが自動融合するペア（`cmp + jcc`、`test + jcc`、`add + jcc`、`lea + cmp`）を意図的に生成。融合条件: 隣接命令・同一デコードグループ・フラグ依存。コンパイラが`CMP + JE`の代わりに`TEST + JNE`を選択する場合も融合性を考慮。AMD Zen4 / Intel Raptor Lake の融合テーブルを参照。融合成功でfront-end bandwidthが実質2倍
- **根拠**: ループ制御（compare + branch）がほぼ全ループに存在。フュージョン1回でデコード帯域を1命令分節約。高頻度ループでの効果が大きい
- **難易度**: Medium

#### FR-825: SWAR / SIMD Within A Register (レジスタ内SIMD)

- **対象**: `src/optimize/optimizer.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: バイト・ハーフワード単位の並列処理はSIMD命令依存。ベクトル命令が使えない文脈でのスカラー並列化なし
- **内容**: 64ビット整数レジスタを複数の小型整数（8×byte / 4×16bit / 2×32bit）として扱う技法を自動適用。`(dotimes (i 8) (aref chars i))` の文字処理 → 8バイトを一括pack → `AND 0x0101010101010101` 等のbitwise演算で並列処理。`(cl-cc:swar8 ...)` / `(cl-cc:swar16 ...)` で明示的SWAR演算を提供。`popcount` / `parity` / `byte-reverse` の効率的SWAR実装を標準ライブラリに内蔵
- **根拠**: SSE/AVXが使えない環境（スカラーループ・WASM）での文字列処理・バイトマップ操作の高速化。SWAR（Hacker's Delight）技法は無料の2〜8倍高速化
- **難易度**: Medium

#### FR-826: Branchless Code Generation (分岐レスコード生成)

- **対象**: `src/optimize/optimizer.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: `if`式は条件分岐命令（JE/JNE）に変換。分岐予測失敗で5〜20サイクルのペナルティ
- **内容**: `(if (> a b) a b)` → `CMOVG rax, rbx`（Conditional MOV、分岐なし）に変換。変換条件: thenとelseが副作用なし・同型・短い演算。`(cl-cc:clamp x lo hi)` → `MAX(MIN(x,hi),lo)` をCMOV2命令で実装。絶対値: `(abs x)` → `SAR + XOR + SUB`（branchless abs）。選択関数: `(cl-cc:select cond a b)` でCMOV強制。コンパイラが分岐予測困難（乱数・外部データ依存）なifを自動検出してCMOV変換
- **根拠**: ソートアルゴリズムの比較関数でbranchlessが2〜5倍高速。コンパイラ内部の型タグチェックに多数適用可能
- **難易度**: Medium

#### FR-827: Instruction Count Minimization (命令数最小化)

- **対象**: `src/optimize/optimizer.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: コード生成は命令の意味的正しさを優先。命令数最小化を目標とした最適化モードなし
- **内容**: `--optimize-for instruction-count` モード。LEA命令による乗算代替: `3*x` → `LEA rax, [rax + 2*rax]`（MUL命令不要）。XOR-zeroing: `(setq r 0)` → `XOR rax, rax`（MOV命令より1バイト短い）。`INC`/`DEC` vs `ADD 1`/`SUB 1`の選択。Load-with-operation: `(incf (aref arr i))` → `ADD [arr+i*8], 1`（load+add+store→1命令）。FR-671（Superoptimizer）のルールをオンラインで適用
- **根拠**: I-cache制約が厳しいホットループで命令数削減がキャッシュヒット率向上に直結。コードサイズ最適化（FR-615）の補完
- **難易度**: Medium

---

### Phase 148 — 型システム VI

#### FR-830: Session Types (セッション型)

- **対象**: `src/type/`, `src/vm/conditions.lisp`
- **現状**: チャネル（FR-599）は型なし。通信プロトコルの順序・双方向性の型安全保証なし
- **内容**: `(defsession ClientProtocol (! (request string)) (? (response integer)) end)` でセッション型を定義。`!`=送信、`?`=受信、`&`=外部選択、`⊕`=内部選択、`end`=終了、`μ`=再帰。チャネル（FR-599）にセッション型を付与: `(cl-cc:open-channel ClientProtocol)` で型付きチャネルを取得。型検査がプロトコル違反（順序・方向・型不一致）をコンパイル時に検出。線形型（FR-linear）との統合でチャネルの使い捨て保証。Honda et al. / Vasconcelos / Frank Pfenning のセッション型理論の実装
- **根拠**: 通信プロトコルのバグ（順序違反・デッドロック）をコンパイル時に検出。cl-ccのLSPサーバー（FR-070）のJSON-RPC通信をセッション型で安全化
- **難易度**: Very Hard

#### FR-831: Singleton Types (シングルトン型)

- **対象**: `src/type/`, `src/expand/expander.lisp`
- **現状**: 型は値の集合を記述。特定の値1つだけを持つ型（singleton）なし
- **内容**: `(cl-cc:the-value 42)` で値42だけを持つ型`(singleton 42)`を生成。`(defun f (x) (declare (type (singleton :ok) x)) ...)` でxが`:ok`であることを型レベルで保証。定数畳み込み（FR-002）との統合: `(singleton 42)`型の変数は定数として伝播。`eql-specializer` との統合: `(eql :ok)` CLOSスペシャライザと同型。TypeScript `"literal"` 型 / Haskell `Proxy n` / C++ `std::integral_constant` と同等
- **根拠**: 状態機械の状態を値ではなく型で表現。関数の戻り値が特定定数のみであることを型で文書化・検証
- **難易度**: Medium

#### FR-832: Liquid Type Inference (液体型推論)

- **対象**: `src/type/`, `src/optimize/optimizer.lisp`
- **現状**: VRP（FR-245）は整数範囲を解析するがそれを型システムに反映しない。精緻化型（FR-225）は手動アノテーション必須
- **内容**: **Liquid Haskell スタイルの自動精緻化型推論**: プログラムの制御フローから自動的に精緻化型を導出。`(if (> n 0) (sqrt n) ...)` → then分岐でnの型を`{n : integer | n > 0}`に自動精緻化。SMTソルバー（FR-246）に送出して検証。`(declare (cl-cc:liquid-types))` で関数単位有効化。境界検査除去（FR-037 BCE）の精度を大幅向上: 液体型推論で境界内と証明された配列アクセスのチェックを削除
- **根拠**: Liquid Haskell実測: 配列境界検査の70〜90%を静的除去。実行時の安全性チェックを大幅削減
- **難易度**: Very Hard

#### FR-833: Existential Types (存在型)

- **対象**: `src/type/`, `src/expand/expander.lisp`
- **現状**: 全称型（`forall a. ...`、FR-682 Rank-N）はあるが、存在型（`exists a. ...`）なし
- **内容**: `(cl-cc:pack val (type a integer) (val show (lambda (x) (format t "~A" x))))` で存在型のパッキング。`(cl-cc:unpack (a val show) packed-val body)` でアンパッキング（`a`は本体内でのみ参照可能）。**実装**: 辞書渡し（dictionary passing）でモジュール/型クラスインスタンスとして表現。`(forall a. (Eq a) => ...)` の `a` をその場で存在型として局所化。OCaml `module type` の first-class 版 / Haskell `ExistentialQuantification` と同等
- **根拠**: 動的ディスパッチ（CLOS）の型安全なエンコード。プラグイン（FR-700）のインターフェース型を存在型で表現し型安全なプラグインシステムを実現
- **難易度**: Hard

---

### Phase 149 — マクロ・メタプログラミング高度化

#### FR-836: Macro Debugger / Expansion Stepper (マクロデバッガ・展開ステッパー)

- **対象**: `src/expand/expander.lisp`, `src/cli/main.lisp`
- **現状**: マクロ展開エラーのデバッグは`macroexpand-1`の手動呼び出しのみ。展開過程の可視化なし
- **内容**: `./cl-cc macrostep foo.lisp` でマクロ展開を1ステップずつ可視化。各ステップで「適用されたマクロ名・入力フォーム・展開結果」を表示。`--max-depth 5` で展開深さを制限。DrRacket Macro Stepper / SLIME macroexpand-all との比較設計。LSP統合（FR-070）: `textDocument/macroExpand` カスタムリクエストでIDEからマクロ展開を段階表示。差分ハイライト（展開前後のdiff）でどの部分が変化したかを明示
- **根拠**: cl-ccのマクロ（loop・dolist・define-vm-instruction等）のデバッグに直接効果。マクロ展開過程が不透明なのは開発者の最大の苦痛源
- **難易度**: Medium

#### FR-837: Hygienic Macros / Syntax-Rules (衛生的マクロ)

- **対象**: `src/expand/expander.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: CLの`defmacro`は非衛生的（変数キャプチャの問題）。`gensym`による手動衛生性確保が必要
- **内容**: `(define-syntax my-swap (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))` でR7RS/R6RS`syntax-rules`スタイルの衛生的マクロを提供。`tmp`が呼び出し元のシンボルを捕捉しない（自動gensym化）。`syntax-case`（R6RS）スタイルのパターンマッチ付き衛生的マクロも実装。`(cl-cc:define-syntax ...)` で従来の`defmacro`と共存。Scheme SRFI-72 / Racket `define-syntax` との互換性
- **根拠**: 複雑なマクロでの変数キャプチャバグを根本排除。初心者がgensymを意識せずにマクロを書ける
- **難易度**: Hard

#### FR-838: Compile-Time Unit Tests (コンパイル時ユニットテスト)

- **対象**: `src/expand/expander.lisp`, `src/cli/main.lisp`
- **現状**: テストはビルド後に別途`make test`で実行。型チェック・定数評価結果のコンパイル時検証なし
- **内容**: `(cl-cc:compile-time-test "fib-10 = 55" (= (cl-cc:eval-at-compile-time (fib 10)) 55))` でコンパイル中に評価・検証。失敗時はコンパイルエラー（テスト名・期待値・実際値付き）。型に関するcompile-time test: `(cl-cc:compile-time-test "add is pure" (cl-cc:static-assert-pure #'add))`。`--compile-tests` フラグで全compile-time testを実行（CI用）。D言語の `unittest` / Rust の `#[test]` (doctest) / Zig の `comptime testing` と同等
- **根拠**: 定数テーブル・型推論結果・マクロ展開の正しさをビルド中に保証。「コンパイルが通れば一定の正しさが保証される」水準の向上
- **難易度**: Medium

#### FR-839: Macro Expansion Memoization (マクロ展開メモ化)

- **対象**: `src/expand/expander.lisp`
- **現状**: 同一フォームのマクロ展開が複数箇所で発生する場合（LTOやインライン化後）、毎回再展開
- **内容**: マクロ展開結果を入力フォームのハッシュ（内容ハッシュ）をキーとしてキャッシュ。純粋なマクロ（副作用なし・環境非依存）のみキャッシュ対象。`(declare (cl-cc:pure-macro))` で明示指定。`*macro-expansion-cache*` ハッシュテーブル（弱値参照でGC可能）。展開結果をFASLにシリアライズして次回ビルドでも再利用（FR-452 コンパイルキャッシュと統合）。キャッシュヒット率をデバッグ出力（`--verbose-macros`）
- **根拠**: `define-vm-instruction`・`deftest`・`(loop ...)`などの繰り返しマクロ展開がselfhostingコンパイル時間の大きな割合を占める
- **難易度**: Medium

---

### Phase 150 — 関数特化・クローニング

#### FR-842: Function Versioning / Argument Specialization (関数バージョニング・引数特化)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: IPCPとLTO（FR-040）で定数引数を伝播。関数クローン生成による特化版の自動生成なし
- **内容**: 全呼び出しサイトで引数 `x` が特定の定数値のサブセット（例: 常にtまたはnilの2値）であると分かった場合、クローン関数 `foo/x=t` と `foo/x=nil` を生成。各呼び出しサイトは適切なクローンにリダイレクト。特化後に定数畳み込み（FR-002）・デッドコード除去（FR-003）が大幅に適用可能。`--clone-threshold 3` でクローン生成の最大コピー数制限。LLVM `ArgumentPromotion` + cloning / GCC IPCP cloning と同等
- **根拠**: コンパイラ内部のフラグ引数（`(compile-ast ast env debug?)` の `debug?`）が常に`nil`なら特化版でデバッグパスが完全消去される
- **難易度**: Hard

#### FR-843: SIMD Auto-Specialization (SIMD自動特化)

- **対象**: `src/optimize/optimizer.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: SIMD化（FR-035）は配列ループのみ。スカラー関数の引数型・サイズが実行時に確定した後でのSIMD特化なし
- **内容**: 配列処理関数 `(defun sum-array (arr n) ...)` を呼び出す際、`n`が16の倍数と分かった場合にSIMD特化版を自動選択。FR-842（Function Versioning）の拡張: サイズ・アライメント・型のサブセット特化。`(cl-cc:specialize sum-array :simd :when (multiple-of n 16))` で手動特化ヒント。特化版と汎用版を両方生成しディスパッチコードで選択。FR-616（Multi-Versioning）のSIMD特化版
- **根拠**: コンパイル時にSIMD条件を確認できない関数でも、呼び出し時の引数情報でSIMD最適化を適用可能
- **難易度**: Hard

#### FR-844: Loop Count Specialization (ループカウント特化)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: ループは一般的な反復数を仮定。小さな固定回数（1・2・4・8回）の特化版なし
- **内容**: ループカウントが小さな定数になる呼び出しサイト（`(dotimes (i 4) ...)`）を検出し、完全展開版（FR-034 アンローリング）を自動生成。ランタイム特化: カウントが小さい場合に特化版、大きい場合に汎用版をディスパッチ。`(cl-cc:specialize-loop :counts (1 2 4 8 16))` で特化カウントを明示。3×3行列 / 4×4行列 / RGBA4チャンネル処理などのDSP定型処理に効果的
- **根拠**: 小固定カウントループの完全展開でブランチとカウンタ更新をゼロに。グラフィクス・信号処理コードで3〜5倍高速化
- **難易度**: Medium

#### FR-845: Hot/Cold Function Splitting (ホット/コールド関数分割)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`, `src/binary/macho.lisp`
- **現状**: FR-036（Hot/Cold レイアウト）は関数全体をhot/coldに分類。関数内のhot/coldパスの分離なし
- **内容**: プロファイルデータ（FR-508 AutoFDO）またはJITフィードバック（FR-559 Type Feedback）でhot basicブロックとcoldブロック（エラー処理・稀な分岐）を特定。coldブロックを関数外に抽出し`__cold`セクションに配置（`__TEXT.__text.cold`）。hot部分が連続したI-cache lineに収まりTLBエントリを節約。GCCの`__attribute__((cold))`ブロック分離 / LLVM `llvm.cold_call` / V8 cold code separation と同等。`(declare (cl-cc:cold))` で手動coldマーク
- **根拠**: エラー処理コード（assert・type-error・bounds-check失敗）がhotパスから除外されI-cache効率が大幅向上。selfhostingコンパイルのhotパス特定でコンパイル速度向上
- **難易度**: Hard

---

### Phase 151 — TLS・特殊命令

#### FR-848: Thread-Local Storage / TLS (スレッドローカルストレージ)

- **対象**: `src/vm/vm-execute.lisp`, `src/backend/x86-64-codegen.lisp`, `src/backend/aarch64.lisp`
- **現状**: グローバル変数はプロセス共有。スレッドごとに独立した値を持つ変数の宣言・アクセスなし
- **内容**: `(defvar-tls *current-thread-state*)` でスレッドローカル変数を宣言。x86-64: ELF TLS (`%fs:` セグメント相対アドレッシング) / Mach-O TLS (`__DATA.__thread_vars` セクション + `tlv_get_addr` stub)。`MOVQ %fs:0, %rax` + オフセット方式。AArch64: `MRS x0, TPIDR_EL0` + オフセット。FR-576（Work-Stealing）ワーカーごとの`*current-worker*` / FR-676（TLAB）per-thread バッファ pointer の実装基盤。`(declare (cl-cc:thread-local))` で宣言
- **根拠**: TLABポインタ・JITコード生成バッファ・エラーハンドラスタックなど、スレッドローカルに保持すべき状態が多数。`pthread_getspecific` より1〜2桁高速
- **難易度**: Hard

#### FR-849: AES-NI / SHA-NI Hardware Cryptography (AES-NI・SHA-NI ハードウェア暗号命令)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/vm/vm-execute.lisp`
- **現状**: 暗号処理はソフトウェア実装のみ。AES/SHA の CPU 組み込み命令未活用
- **内容**: `(cl-cc:aes-enc block key)` → `AESENC xmm0, xmm1`（1サイクル / Intel Westmere以降）。`(cl-cc:aes-keygen key rcon)` → `AESKEYGENASSIST`。SHA-256: `(cl-cc:sha256-rounds state msg)` → `SHA256RNDS2` / `SHA256MSG1` / `SHA256MSG2`（AMD Zen / Intel Ice Lake 以降）。CLMUL: `(cl-cc:clmul a b)` → `VPCLMULQDQ` でガロア体GF(2^128)乗算（AES-GCM認証タグ計算）。`cpuid` で命令サポートを確認し、非サポート CPU はソフトウェアフォールバック
- **根拠**: AES-CTR / AES-GCM でAES-NI使用時はソフト実装比8〜30倍高速。FR-453（Constant-Time Security）との統合でタイミング攻撃耐性も保証
- **難易度**: Medium

#### FR-850: Atomic 128-bit Operations / CMPXCHG16B (128ビット不可分操作)

- **対象**: `src/vm/vm-execute.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: 原子操作（FR-160）は64ビット幅まで。タグ付きポインタ（値+バージョンカウンタ）のABA問題を解決する128ビット CAS なし
- **内容**: `(cl-cc:cmpxchg128 addr expected-lo expected-hi new-lo new-hi)` → `CMPXCHG16B [addr]`（RCX:RBX / RDX:RAX ペア操作）。ABA問題対策: ポインタ64bit + バージョンカウンタ64bit を1原子操作で更新。ロックフリースタック（FR-163）のABAバグを根本解消。AArch64: `CASP x0, x1, x2, x3, [addr]`（Load-Pair atomic）。`LOCK` prefix が必要（`LOCK CMPXCHG16B`）
- **根拠**: `(cl-cc:atomic-cons head new-element)` でABAフリーのロックフリーリスト先頭更新が可能。マルチコアGCのロックフリーアロケータでの競合状態を根本解消
- **難易度**: Medium

#### FR-851: Vector Permutation Instructions (ベクトル置換命令)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: SIMDコード生成（FR-035）は算術・比較命令中心。要素の並べ替え（shuffle/gather）命令の活用なし
- **内容**: `(cl-cc:vpshufb dst ctrl src)` → `VPSHUFB ymm0, ymm1, ymm2`（バイト単位シャッフル、AES MixColumns・文字変換テーブルに使用）。`(cl-cc:vpermd idx src)` → `VPERMD`（32bit要素を任意順に並べ替え）。`(cl-cc:vperm2i128 src1 src2 imm)` → 128bitレーン間シャッフル。自動適用: AoS→SoA変換（FR-038）のシャッフルコストモデルにpermutation命令を使用。AArch64: `TBL v0.16b, {v1.16b}, v2.16b`
- **根拠**: ハッシュ関数・暗号・行列転置でpermutation命令がループ比10倍以上高速。SIMD自動ベクトル化の表現力を大幅拡張
- **難易度**: Medium

---

### Phase 152 — GC高度化 III

#### FR-854: Cycle Detection for Reference Counting (参照カウントの循環検出)

- **対象**: `src/runtime/gc.lisp`, `src/vm/vm-execute.lisp`
- **現状**: FR-229（Reference Counting）は基本的なRC実装。循環参照（`(let ((a (cons nil nil))) (setf (car a) a))`）がメモリリーク
- **内容**: Bacon & Rajan（OOPSLA 2001）の **同期的循環GC**: RC=0でないが unreachable なオブジェクトをサイクル候補バッファに記録。定期的に候補から深さ優先探索で孤立サイクルを検出・回収。`mark-gray`→`scan`→`collect-white` の3フェーズ。CPython の Cyclic GC / Ruby の Mark-and-Sweep 補完と同等の設計。`--cycle-gc-threshold 1000` でサイクル検出頻度を調整
- **根拠**: CLOSインスタンスが互いに参照し合うケース（双方向リンクリスト・Observer パターン）でメモリリーク防止。RCとトレーシングGCのハイブリッドで最高のスループットを実現
- **難易度**: Hard

#### FR-855: Deferred Reference Counting (遅延参照カウント)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: RC（FR-229）は全ての参照変化で即座にカウンタ更新。スタック上の一時参照でも毎回インクリメント/デクリメントが発生
- **内容**: **Deutsch-Bobrow 方式**: スタックフレーム（ローカル変数）からの参照変化を **遅延**し、ヒープからヒープへの参照変化のみ即座に更新。スタック参照のRCはGCサイクル時に一括処理（安全点でスタックスキャン）。RC更新オーバーヘッドを70〜90%削減。「Zero Count Table」（ZCT）でRC=0になったオブジェクトを遅延回収。Levanoni-Petrank（PLDI 2001）のRC最適化と組み合わせ
- **根拠**: Pythonの主要GCボトルネックはRC更新（全代入で2操作）。遅延RC でインタプリタ比30%高速化の実測値あり
- **難易度**: Hard

#### FR-856: Pinned Objects for FFI (FFI用ピン留めオブジェクト)

- **対象**: `src/runtime/gc.lisp`, `src/vm/vm-execute.lisp`, `src/ffi/`
- **現状**: GC（コンパクティング・FR-226）がオブジェクトを移動可能。C関数に渡したLispオブジェクトのアドレスが無効化されるリスク
- **内容**: `(cl-cc:with-pinned-object (obj) (foreign-call "cfn" (object-address obj)))` でC関数呼び出し中のGC移動を禁止。pinされたオブジェクトは `pinned-objects` セットに登録しGCのcompaction対象から除外。C関数からコールバック（FR-884）で再入可能: pin は呼び出しスタック全体に継承。`(cl-cc:pin obj)` / `(cl-cc:unpin obj)` で手動制御。FR-550（Stack Maps）と統合しGCがpinnedを把握。Java JNI `GetPrimitiveArrayCritical` / .NET `fixed` 相当
- **根拠**: GCコンパクション（FR-226）とFFI（FR-632 Bindgen）の共存に不可欠。ピン忘れはサイレントなメモリ破壊を起こす最悪のバグ
- **難易度**: Medium

#### FR-857: GC External Roots / Foreign GC Roots (GC外部ルート)

- **対象**: `src/runtime/gc.lisp`, `src/vm/vm-execute.lisp`
- **現状**: GCルートはVMレジスタとスタックのみ。C側に保持されたLispオブジェクト参照がGCに見えない
- **内容**: `(cl-cc:register-gc-root ptr-to-lisp-obj)` でCデータ構造内のLispオブジェクト参照をGCルートとして登録。GCマーク相（FR-552 Concurrent GC）でexternal rootsを走査対象に追加。`(cl-cc:unregister-gc-root ptr)` で登録解除。C++コンテナ内のLispオブジェクト群を一括登録: `(cl-cc:register-gc-root-range start count)`。スレッドセーフ: `roots-lock` で保護。JVM `AddVMInitArgs` roots / V8 `Persistent<T>` / Mono `mono_gchandle_new` と同等設計
- **根拠**: C++ゲームエンジン・Pythonモジュール・GTKウィジェット内にLispオブジェクトを埋め込む用途で必須。登録忘れはGCによる即死バグ
- **難易度**: Medium

---

### Phase 153 — 最適化パス管理

#### FR-860: Pass Pipeline Configuration (パスパイプライン設定)

- **対象**: `src/optimize/optimizer.lisp`, `src/cli/main.lisp`
- **現状**: 最適化パスの実行順序はハードコード。特定パスの無効化・順序変更・パラメータ調整が困難
- **内容**: `--pass-pipeline "licm,gvn,sccp,cse,dce"` でパス実行順を文字列指定。`--disable-pass licm` で特定パスを無効化（デバッグ用）。`--pass-param inlining:threshold=50` でパスパラメータを CLI から設定。`(cl-cc:define-pass-pipeline :O2 (licm gvn sccp cse dce inlining loop-unroll))` でプリセット定義。パスの実行ログ（`--print-passes`）で各パスの入出力を表示。LLVM `opt -passes="..."` / GCC `-fdump-tree-` と同等
- **根拠**: 最適化バグの二分探索（FR-809 Bisection）でパス単位の有効/無効切り替えが必須。新パス追加時の順序依存性のデバッグに不可欠
- **難易度**: Medium

#### FR-861: Analysis Preservation Tracking (解析保存追跡)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 各最適化パスが実行されると全ての解析結果（エイリアス解析・支配木・live range）が無効化され再計算。不要な再計算が多い
- **内容**: 各パスが「保存する解析」を宣言: `(define-opt-pass licm :preserves (alias-analysis dominance-tree))`. パスマネージャが宣言を追跡し、保存されている解析を再計算なしに後続パスへ渡す。「解析の依存グラフ」で何が何を必要とするかを自動管理。`--print-preserved-analyses` でどの解析が有効かをダンプ。LLVM `PreservedAnalyses` / GCC `PROP_cfg` と同等
- **根拠**: 大規模プログラムのコンパイルでエイリアス解析・支配木の再計算コストが無視できない。保存追跡でコンパイル時間20〜40%削減の実測値あり
- **難易度**: Medium

#### FR-862: Value Profiling (値プロファイリング)

- **対象**: `src/compile/codegen.lisp`, `src/cli/main.lisp`
- **現状**: PGO（FR-508 AutoFDO）は関数呼び出し頻度・分岐頻度のみ計測。実際の引数値の分布不明
- **内容**: `./cl-cc run --value-profile foo.lisp` で関数引数・分岐条件・インダイレクト呼び出し先の実際の値をサンプリング。「引数 x は90%の確率で 0」「間接呼び出しは85%が `foo`」などの値プロファイルを収集。FR-842（Function Versioning）: 高頻度値でのクローン生成の根拠として使用。FR-560（Speculative Inlining）: 高頻度呼び出し先への投機的インライン化の確率根拠。LLVM `InstrProfValueData` / JVM Value Profiling と同等
- **根拠**: 型フィードバック（FR-559）の拡張。「この関数の引数は常に小さな整数」という情報を最適化の根拠にする
- **難易度**: Medium

#### FR-863: Loop Versioning with Runtime Alias Checks (ランタイムエイリアスチェック付きループバージョニング)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: SIMD自動ベクトル化（FR-035）は静的エイリアス解析（FR-234）でセーフと確認できたループのみ対象。解析が不確定なループはベクトル化不可
- **内容**: エイリアス解析が「不確定」なループで **ランタイムチェック** を挿入。`if (|src - dst| >= vector_width) { SIMD版 } else { スカラー版 }` の分岐生成。チェックコスト < ベクトル化利益 の場合にのみ適用（ループ回数推定が十分大きい場合）。複数のエイリアスペアのチェックを1条件に結合。LLVM `LoopVersioning` / GCC polyvect runtime check と同等
- **根拠**: エイリアス不確定な配列操作ループ（外部から渡された配列ペア）の70〜80%がランタイムで非エイリアスと確認されSIMD化可能
- **難易度**: Hard

---

### Phase 154 — 型システム VII

#### FR-866: Recursive Types / μ-Types (再帰型・μ型)

- **対象**: `src/type/`, `src/expand/expander.lisp`
- **現状**: 型定義に相互再帰（`list = nil | (cons value list)`）はCLOSで表現可能だが型推論システムの対象外
- **内容**: `(deftype-rec (list a) (or nil (cons a (list a))))` で再帰型を宣言。型検査はμ型の展開（unrolling）で等価性判定。`μX.F[X]` 表現で型システム内部に保持。無限型（equi-recursive）と有限展開（iso-recursive）の両モード。`(cl-cc:fold val)` / `(cl-cc:unfold val)` でiso-recursive型の明示的コンバージョン。Amadio & Cardelli（1993）の再帰型等価アルゴリズム実装
- **根拠**: 型推論が再帰型（リスト・ツリー・グラフ）に対して正確な型を導出できる。現在の「any」フォールバックを解消
- **難易度**: Hard

#### FR-867: Codata / Coinductive Types (余データ・余帰納的型)

- **対象**: `src/type/`, `src/compile/codegen.lisp`
- **現状**: 帰納的型（有限データ構造）のみ型システムで表現。無限データ構造（無限ストリーム）の型安全な表現なし
- **内容**: `(defcodata stream (head (-> stream a)) (tail (-> stream a (stream a))))` で余帰納的型を定義。コンストラクタは遅延（thunk化）。`(cl-cc:coiterate f seed)` で無限ストリームを生成。型チェックは **余帰納的等価** （bisimulation）で判定。停止性チェック（FR-713）の余データ版: **生産性チェック** で corecursive 関数が無限に要素を生産することを保証。Haskell lazy lists / Agda コパターンマッチ / Coq CoInductive と同等
- **根拠**: 無限ストリーム・リアクティブシグナル（FR-601 FRP）・I/O チャネルの型安全な表現基盤
- **難易度**: Very Hard

#### FR-868: Heterogeneous Lists / HList (異種リスト)

- **対象**: `src/type/`, `src/expand/expander.lisp`
- **現状**: `list` 型は全要素が同一型。異なる型の要素を持つ固定長リスト（タプル的）の型レベル表現なし
- **内容**: `(deftype hlist () nil)` `(deftype (hlist a . rest) () (cons a (hlist . rest)))` で型レベルリストとして HList を定義。`(cl-cc:hnil)` / `(cl-cc:hcons x xs)` でコンストラクタ。`(cl-cc:hhead xs)` / `(cl-cc:htail xs)` で型安全アクセス（型が自動的に `integer` / `string` に特定）。型レベルプログラミング（FR-595）との統合: `(cl-cc:length-type hlist)` で HList の長さを型レベル自然数として取得。Haskell `HList` library / GHC `DataKinds` TypeLists と同等
- **根拠**: 可変引数関数の型安全な実装基盤。`(cl-cc:zip hl1 hl2)` が要素ごとの型ペアを正確に推論できる
- **難易度**: Hard

#### FR-869: Opaque Types / Abstract Type Boundaries (不透明型・抽象型境界)

- **対象**: `src/type/`, `src/package.lisp`, `src/compile/codegen.lisp`
- **現状**: 型定義はパッケージ外から構造が見える。内部表現を隠蔽した「抽象型」の型システムレベル強制なし
- **内容**: `(defopaque-type password string)` で`password`型を宣言。`password`の内部が`string`であることはモジュール外から不可視。型チェッカーが`(the string (the password x))`を型エラーにする（`password`と`string`は区別される）。`(cl-cc:coerce-opaque x password)` でモジュール内限定の変換。ML `abstype` / Haskell `newtype` + `module` export control / Java `private` の型システムレベル版。FR-710（Phantom Types）の強化: Phantomは実行時型消去、Opaqueは型システムレベル分離
- **根拠**: APIの表現型（raw string）が意味型（password）と混同されない。型安全な設計によるセキュリティバグの防止
- **難易度**: Medium

---

### Phase 155 — 言語構文拡張

#### FR-872: Do-Notation / Monadic Syntax (do記法・モナド構文)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/expand/expander.lisp`
- **現状**: モナド的なコードは `(bind m (lambda (x) ...))` の入れ子で記述。可読性が低い
- **内容**: `(cl-cc:do (x <- (read-int)) (y <- (read-int)) (return (+ x y)))` でHaskell-style do記法を提供。マクロが `bind`/`then` 呼び出しへ脱糖。`(cl-cc:do-maybe ...)` / `(cl-cc:do-result ...)` / `(cl-cc:do-io ...)` でモナド特化構文。代数的エフェクト（FR-680）との統合: `(cl-cc:do-effect (IO ...) ...)` でエフェクトハンドラ付きdo記法。Scheme `do` とは別物（こちらはループ記法）。`<-` は Lisp リーダーで `←` 相当のシンボルとして扱う
- **根拠**: エフェクト（IO・エラー・状態）の合成コードが`bind`の深いネストから平坦な逐次記述に変換。可読性を保ちつつFR-680代数的エフェクトの実用性向上
- **難易度**: Medium

#### FR-873: List Comprehensions (リスト内包表記)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/expand/expander.lisp`
- **現状**: `(loop for x in xs when (pred x) collect (f x))` が最短表記。Python/Haskellスタイルの内包表記なし
- **内容**: `[f x | x <- xs, (pred x)]` を`(cl-cc:list-of (f x) (x <- xs) (pred x))` のマクロ呼び出しへ展開。ネスト: `(cl-cc:list-of (* x y) (x <- xs) (y <- ys))` → デカルト積。ジェネレータ列の遅延評価バリアント: `(cl-cc:lazy-of ...)` で無限リスト生成。集合内包表記: `(cl-cc:set-of ...)` でハッシュセット生成。FR-608（Deforestation）と統合して中間リスト生成を排除（`map compose filter`に変換）
- **根拠**: `loop`マクロより意図が明確で数学的記法に近い。コンパイラ内部のフィルタリング処理の可読性向上
- **難易度**: Easy

#### FR-874: Generator / yield Syntax (ジェネレータ・yield構文)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/vm-execute.lisp`
- **現状**: FR-653（Lazy Eval）でthunkベースの遅延シーケンス。Python的な`yield`構文なし
- **内容**: `(cl-cc:defgenerator fibonacci () (let ((a 0) (b 1)) (loop (cl-cc:yield a) (psetq a b b (+ a b)))))` でジェネレータ関数を定義。`(cl-cc:next gen)` で次の値を取得（`nil` で終了）。コンパイラがジェネレータをコルーチン（FR-166）に変換: `yield`が`(coroutine-yield val)` に展開。`(cl-cc:for-each (x gen) body)` でジェネレータのイテレーション。FR-867（Codata）との統合: ジェネレータは余帰納的型として型付け可能
- **根拠**: 大規模データの遅延処理・無限シーケンスの自然な記述。Python generators / JavaScript generators と同等の開発体験
- **難易度**: Medium

#### FR-875: View Patterns / As-Patterns (ビューパターン・Asパターン)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **現状**: パターンマッチは構造的分解のみ。関数適用結果でのマッチ・元の値を別名で参照する構文なし
- **内容**: **Asパターン** (`@`): `(match x ((@ whole (cons h t)) ...))` で `whole` に元のリスト、`h`/`t` に分解した結果を同時バインド。再割り当てなし: `whole` は `x` と同一オブジェクト（コピーなし）。**ビューパターン**: `(match x (((view abs-value) v) ...))` で `(abs-value x)` の結果 `v` にマッチ。Haskell `@` patterns / GHC `ViewPatterns` / Rust `binding @ pattern` と同等。FR-728（Decision Tree）での最適化: Asパターンは分岐コスト増加なし
- **根拠**: データ構造の一部を分解しつつ全体も参照するパターンが多い（リストの先頭確認+全体渡し）。コードの明確化とコピー排除を同時実現
- **難易度**: Medium

---

### Phase 156 — REPL・インタラクティブ機能

#### FR-878: REPL Tab Completion (REPLタブ補完)

- **対象**: `src/cli/main.lisp`, `src/vm/symbols.lisp`
- **現状**: REPL（FR-098）は基本的な読み込み・評価・印刷のループ。タブキーでの補完なし
- **内容**: GNU Readline / libedit 統合でタブ補完を実装。補完対象: 現在の`*package*`の全シンボル・グローバル変数・マクロ名・CLOSクラス名・スロット名（`(slot-value obj <TAB>`→スロット名一覧）。S式文脈解析: `(defun <TAB>` → 関数名補完、`(make-instance '<TAB>` → クラス名補完。Fuzzy matching: `fcl` → `funcall`。補完候補はFR-725（Perfect Hash Symbol Table）から高速取得。LSP補完（FR-070）と同一ロジックを共有
- **根拠**: 開発者がシンボル名・スロット名を記憶せずにインタラクティブ開発可能。SLIME/SLY の最重要機能の独立実装
- **難易度**: Medium

#### FR-879: REPL Multiline Input Detection (REPL複数行入力検出)

- **対象**: `src/cli/main.lisp`, `src/parse/cl/parser.lisp`
- **現状**: REPLは1行ずつ読み取り。`(defun foo (x)\n  (+ x 1))` のような複数行入力が改行で即評価される
- **内容**: パーサが**括弧の均衡**を追跡し、未閉じの括弧がある場合は継続プロンプト (`..>`) を表示して次行を読み込み。`(read-balanced-form)` が EOF / 完全なS式 / 不完全なS式を区別して返す。インデントヒント: 継続行の自動インデント（開き括弧の次列に揃える）。マルチライン入力のヒストリーは1エントリとして記録（FR-880）。Emacs SLIME / sbcl --interactive の動作を独立実装
- **根拠**: 現状のREPLで複数行defunを入力するには特別な工夫が必要。基本的なインタラクティブ使用体験の向上
- **難易度**: Easy

#### FR-880: REPL Syntax Highlighting (REPLシンタックスハイライト)

- **対象**: `src/cli/main.lisp`, `src/parse/lexer.lisp`
- **現状**: REPL出力は全て同一色。型エラー・シンボル・数値・文字列が視覚的に区別できない
- **内容**: ANSI エスケープコード（`\x1b[...m`）でターミナル上のシンタックスハイライト。色スキーム: キーワード（青）・特殊フォーム（紫）・文字列（緑）・数値（シアン）・コメント（グレー）・エラー（赤太字）。Readline callbacks で**入力中**のハイライトもリアルタイム更新。`--no-color` / `NO_COLOR` 環境変数でオフ。REPL出力のプリティプリント（FR-657 Reflection APIと統合）でCLOSオブジェクトを構造的に表示
- **根拠**: シンタックスハイライトは現代の開発ツールの最低限の要件。特にエラーメッセージの赤色強調で即座に問題箇所を認識
- **難易度**: Easy

#### FR-881: REPL Undo / Side-Effect Journal (REPLアンドゥ・副作用ジャーナル)

- **対象**: `src/vm/vm-execute.lisp`, `src/cli/main.lisp`
- **現状**: REPLでの`defun`/`defvar`/`setf`は不可逆。誤って実行した定義を取り消す手段なし
- **内容**: REPLセッション中の全**グローバル状態変化**をジャーナルに記録: `(defun foo ...)` → 旧定義を保存、`(defvar x 10)` → 旧値を保存、`(setf (gethash :k ht) v)` → 旧エントリを保存。`(cl-cc:undo)` で直前の状態変化を逆適用。`(cl-cc:undo-to 5)` でREPL入力5番目の状態まで巻き戻し。ファイルI/O・外部プロセス呼び出しはジャーナル外（不可逆とマーク）。FR-507（Time-Travel Debugging）のREPL特化版
- **根拠**: `(defun+` の誤タイプで関数を壊した後に `Ctrl-Z` で戻れる体験。REPLベースの探索的開発での試行錯誤コストを劇的に削減
- **難易度**: Hard

---

### Phase 157 — FFI高度化

#### FR-884: C Callbacks from Lisp (LispからCへのコールバック)

- **対象**: `src/ffi/`, `src/compile/codegen.lisp`, `src/vm/vm-execute.lisp`
- **現状**: LispからC関数を呼び出し可能（FR-707 dlopen）。C関数がLisp関数をコールバックとして呼ぶ逆方向のFFIなし
- **内容**: `(cl-cc:make-callback #'my-handler :ctype (-> int int))` でC互換な関数ポインタを生成。コンパイラがLisp関数をC ABI（System V AMD64 / Win64 ABI）でラップするtrampoline コードを生成。コールバック中のGCは一時停止（FR-551 Safepoint）。コールバックからLispシグナルが投げられた場合の処理（`longjmp` ベース脱出）。CFFI `defcallback` / JNI `CallbackObjectV` / Python `ctypes.CFUNCTYPE` と同等
- **根拠**: GUI ツールキット（GTK/Qt）・イベントループ・ソートアルゴリズム（qsort の比較関数）・libusb等のasync APIがコールバック必須
- **難易度**: Hard

#### FR-885: Variadic C Function Support (可変長引数C関数サポート)

- **対象**: `src/ffi/`, `src/backend/x86-64-codegen.lisp`
- **現状**: FFI（FR-632）は固定引数の関数のみ。`printf`・`open`等の可変長引数C関数の呼び出し不可
- **内容**: `(cl-cc:foreign-call-varargs "printf" :int (:string "%d %s\n") (list 42 "hello"))` で可変長引数呼び出し。System V AMD64 ABI: `%al` = 使用するSSEレジスタ数を設定してから `call`。Win64 ABI: シャドウスペース + スタック渡し。型マッピング: Lisp integer → C `int`/`long`、Lisp float → C `double`（デフォルト昇格）。Lispで可変長引数関数を**定義**する場合（Cから呼ばれる）: `(cl-cc:define-varargs-callback f (fixed-arg &rest args) ...)` で `va_list` をLispシーケンスとして受け取り
- **根拠**: POSIX API (`open(2)` の O_CREAT時3引数)・フォーマット系関数（`printf` / `sprintf` / `syslog`）に必須
- **難易度**: Hard

#### FR-886: Platform-Specific API Integration (プラットフォーム固有API統合)

- **対象**: `src/vm/io.lisp`, `src/cli/main.lisp`, 新規 `src/platform/`
- **現状**: POSIX API のみ。macOS GCD・Windows IOCP・Linux BPF などのプラットフォーム固有の高性能APIへのアクセスなし
- **内容**: **macOS**: `dispatch_queue_create` / `dispatch_async` (Grand Central Dispatch) のLispラッパー。`kern_return_t`型・Mach メッセージ。**Linux**: `io_uring`（FR-600 統合拡張）・`eBPF` プログラム作成/ロード・`netlink` ソケット。**Windows**: `CreateIoCompletionPort` / `GetQueuedCompletionStatusEx`（IOCP）。全プラットフォームで `(cl-cc:with-platform (:macos gcd-impl :linux epoll-impl :windows iocp-impl) ...)` で統一API
- **根拠**: 高性能ネットワークサーバーや並行I/Oシステムでプラットフォーム固有APIへのアクセスが不可欠
- **難易度**: Hard

#### FR-887: ABI-Stable C Public Interface Generation (ABI安定C公開インターフェース生成)

- **対象**: `src/compile/codegen.lisp`, `src/binary/macho.lisp`, `src/cli/main.lisp`
- **現状**: cl-ccはライブラリとして使えるが、公開するC APIのヘッダファイル・ABI保証なし
- **内容**: `(cl-cc:export-c-api foo :sig (-> integer integer))` でC互換の公開関数を宣言。`./cl-cc build --emit-header foo.h` でC/C++ヘッダーを自動生成（型マッピング・`extern "C"` ラッパー・Doxygen コメント付き）。`--abi-version 2` でAPIバージョン管理（FR-777 ABI Stability Manifest統合）。FR-635（COMDAT）/ シンボルバージョニングとの統合で `.so` の `foo@@V2` 形式シンボルを生成。Rust `#[no_mangle] pub extern "C" fn` / Swift `@_cdecl` と同等
- **根拠**: cl-ccを他言語（C/Python/Ruby）から動的ライブラリとして使用できるエコシステム基盤。selfhostingしつつCライブラリも提供
- **難易度**: Medium

---

### Phase 158 — 文字列・テキスト処理

#### FR-890: Unicode Normalization (Unicode正規化)

- **対象**: `src/vm/strings.lisp`, `src/vm/symbols.lisp`
- **現状**: 文字列はUTF-8バイト列として保持。NFC/NFD/NFKC/NFKD正規化なし。同一文字を表す異なるコードポイント列が`equal`でfalseになる
- **内容**: `(cl-cc:unicode-normalize s :nfc)` で NFC（合成済み正規形）に変換。`(cl-cc:unicode-normalize s :nfd)` で NFD（分解正規形）。`(cl-cc:unicode-normalize s :nfkc)` / `:nfkd` で互換正規化。Unicode Derivation Tables（CompositionExclusions.txt / UnicodeData.txt）を内蔵ルックアップテーブルとして コンパイル時生成（FR-670 Constexpr活用）。シンボルインターン（FR-722）時に自動NFC正規化を選択可能。ICU / Java `Normalizer2` / Python `unicodedata.normalize` と同等
- **根拠**: 国際化テキスト処理でNFC/NFD混在による比較バグを防止。Lisp識別子のUnicode対応（NFC正規化）
- **難易度**: Medium

#### FR-891: SIMD String Searching (SIMD文字列探索)

- **対象**: `src/vm/strings.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: `(search needle haystack)` はバイト単位の線形探索。大きな文字列での検索が遅い
- **内容**: **SIMD加速 Boyer-Moore-Horspool**: `VPCMPEQB` で16/32バイト同時比較、ミスマッチを `VPMOVMSKB` でビットマスク取得。**SWAR（FR-825）版**: 64ビット整数に4バイトパターンを詰めてXOR+ANDで検索。`PCMPESTRI`（x86 SSE4.2）: 文字クラス検索・部分文字列検索を専用命令で高速化。`(cl-cc:string-contains-simd haystack needle)` でSIMD版を明示的に使用。`(cl-cc:find-char-class s #'digit-char-p)` でSIMD文字クラス検索。glibc `memmem` / Hyperscan（Intel）の手法
- **根拠**: テキストパーサ（selfhostingパーサ・LSPの全文検索）での文字列探索がホットスポット。SIMD版でスカラー比10〜50倍高速
- **難易度**: Hard

#### FR-892: Cryptographically Secure PRNG / CSPRNG (暗号学的擬似乱数生成)

- **対象**: 新規 `src/crypto/random.lisp`, `src/vm/vm-execute.lisp`
- **現状**: 乱数生成は`(random n)`（SBCL Mersenne Twister）のみ。暗号学的安全性なし・状態予測可能
- **内容**: `(cl-cc:crypto-random n)` でCSPRNG（暗号学的安全な疑似乱数）を提供。実装: OS乱数（`getrandom(2)` Linux / `arc4random(3)` macOS / `BCryptGenRandom` Windows）をシードとしたChaCha20-based PRNG（RFC 7539）。`(cl-cc:random-bytes n)` でnバイトのランダムバイト列。`(cl-cc:random-uuid)` でUUID v4生成。FR-849（AES-NI）との統合でAES-CTR-based CSPRNG も選択可能。`with-random-seed`でテスト用に固定シード設定可能
- **根拠**: セキュリティトークン生成・暗号キー導出・セキュアなセッションID生成にCSPRNG必須。Mersenne Twisterは暗号用途に使用不可
- **難易度**: Medium

#### FR-893: Hashing Library / Universal Hash Functions (ハッシュライブラリ・汎用ハッシュ関数)

- **対象**: `src/vm/hash.lisp`, `src/runtime/heap.lisp`
- **現状**: ハッシュテーブル（`gethash`）のキーハッシュは`sxhash`（CL標準）のみ。高速ハッシュ・衝突耐性ハッシュなし
- **内容**: 複数のハッシュアルゴリズムを提供: **FNV-1a**: 高速、小さな値向け。**xxHash64**: 大バッファ向け最速（SIMD対応）。**SipHash-2-4**: HashDoS（意図的衝突攻撃）耐性・デフォルト採用（Python3・Rust の標準）。**MurmurHash3**: 分散処理向け良好な分布。`(cl-cc:hash-with :siphash key seed)` でアルゴリズム選択。ハッシュテーブルのresize時に自動的にseedをランダム化（HashDoS対策）。`--hash-seed random`（デフォルト）/ `--hash-seed 0`（デバッグ用固定）
- **根拠**: 現状のsxhashは遅く分布も均一でない。SipHashでHashDoS攻撃を防止しつつxxHashで大バッファを高速処理
- **難易度**: Medium

---

### Phase 159 — ライブラリ・配布形式

#### FR-896: Shared Library Compilation (.so / .dylib 生成)

- **対象**: `src/binary/macho.lisp`, `src/cli/main.lisp`, `src/backend/x86-64-codegen.lisp`
- **現状**: `./cl-cc compile` は実行可能バイナリのみ生成。動的ライブラリ（`.so` / `.dylib`）形式での出力なし
- **内容**: `./cl-cc compile --shared -o libfoo.so foo.lisp` で共有ライブラリを生成。ELF `.so`: PIC コード（FR-051 PIC）+ `.dynsym` / `.dynstr` / `DT_NEEDED` セクション。Mach-O `.dylib`: `LC_ID_DYLIB` / `LC_DYLD_INFO_ONLY` / `__DATA_CONST.__got` 生成。FR-887（ABI-Stable C API）と統合して公開シンボルのみexport。`--soname` / `--compatibility-version` オプション。`install_name_tool` 相当の rpath 操作機能
- **根拠**: cl-ccコードをPython・Ruby・Node.jsから`ctypes`/`ffi`でロード可能に。組み込み・プラグイン配布の標準形式
- **難易度**: Hard

#### FR-897: Static Library Compilation (.a 生成)

- **対象**: `src/binary/macho.lisp`, `src/cli/main.lisp`
- **現状**: バイナリのみ生成。スタティックリンクのためのオブジェクトアーカイブなし
- **内容**: `./cl-cc compile --static -o libfoo.a foo.lisp` で `.a` アーカイブを生成。各コンパイル単位を `.o`（ELF relocatable）として出力し `ar cr libfoo.a *.o` でまとめる。`--whole-archive` フラグ対応でLTO（FR-040）との統合。`./cl-cc ar` コマンドで `ar` 相当の操作（create/add/list/extract）。**Thin Archive**: `.a` に実オブジェクトを格納せずパスのみ記録（Xcode thin archive 相当）でビルド速度向上
- **根拠**: 組み込み（FR-605 Bare Metal）・カーネル（FR-898）への静的リンクに必須。C/C++プロジェクトへのcl-ccコードの組み込み
- **難易度**: Medium

#### FR-898: Kernel Module Support (カーネルモジュールサポート)

- **対象**: `src/cli/main.lisp`, `src/backend/x86-64-codegen.lisp`, `src/compile/codegen.lisp`
- **現状**: FR-605（Bare Metal）はOS不使用環境向け。Linuxカーネル空間でのコード実行未対応
- **内容**: `./cl-cc compile --target linux-kernel-module foo.lisp` でLKM（Loadable Kernel Module）を生成。制約モード: GCなし（FR-719 Epsilon GC）・例外なし（代わりに`IS_ERR`スタイル）・動的割り当て制限（`kmalloc` ラッパーのみ）。`module_init` / `module_exit` エントリポイントを自動生成。`(cl-cc:kernel-func ...)` / `(cl-cc:kernel-printk ...)` のカーネルAPI高水準ラッパー。eBPF（FR-886の拡張）との連携でuserspace↔kernel通信
- **根拠**: デバイスドライバ・ファイルシステム・ネットワークフィルタをLispで記述するニッチだが重要な用途
- **難易度**: Very Hard

#### FR-899: Unikernel / Library OS Target (ユニカーネル・ライブラリOSターゲット)

- **対象**: `src/cli/main.lisp`, `src/backend/`, `src/vm/io.lisp`
- **現状**: Linux/macOS のOSサービスに依存した実行のみ。カーネルなし単独実行形式なし
- **内容**: `./cl-cc compile --target unikernel-xen foo.lisp` でXen PVMハイパーバイザー上で直接起動するイメージを生成。MirageOS / Unikraft / HermiCore の設計に倣い OS サービス（ネットワーク / ファイルシステム / タイマー）をライブラリとして静的リンク。ネットワークスタック: `lwip` または minimalistic TCPスタック の静的リンク。不要なOSコード（プロセス管理・ユーザー権限）をゼロにし攻撃面を最小化。FR-605（Bare Metal）の拡張で仮想マシンサポートを追加
- **根拠**: クラウドFaaS（Function as a Service）環境でコールドスタート時間が100ms→数ms に改善。セキュリティ分離をハイパーバイザーに委譲しTCBを最小化
- **難易度**: Very Hard

---

### Phase 160 — ドキュメント・品質ツール

#### FR-902: Docstring Extraction / API Doc Generation (ドキュメント文字列抽出・API文書生成)

- **対象**: `src/cli/main.lisp`, 新規 `src/docs/extractor.lisp`
- **現状**: `defun`のdocstringは`documentation`関数で取得可能。APIドキュメントの自動生成ツールなし
- **内容**: `./cl-cc doc --format html src/` で全ソースからdocstringを抽出しHTML/Markdown APIドキュメントを生成。抽出対象: `defun` / `defmacro` / `defclass` / `defgeneric` / `defmethod` / `defconstant` の docstring。型アノテーション（FR-type系）・パラメータ説明・戻り値型を自動抽出。`(cl-cc:doc-example ...)` でインラインコード例（FR-903 Doctest用）。生成ドキュメントに FR-657（Reflection API）で取得したメソッド一覧・スロット情報を自動追記。Rustdoc / Haddock / JavaDoc と同等
- **根拠**: cl-cc自身の~600エクスポートシンボルのAPIドキュメントが現在存在しない。セルフホスティング後の公開ライブラリとしての利用性向上
- **難易度**: Medium

#### FR-903: Example Code Testing / Doctest (ドキュメントコード例のテスト)

- **対象**: `src/cli/main.lisp`, `src/testing/`
- **現状**: docstringのコード例が正しいか検証する手段なし。ドキュメントとコードの乖離が発生する
- **内容**: `./cl-cc doctest src/` でdocstring内の`(cl-cc:doc-example ...)`ブロックを抽出して実行・検証。書式: `(cl-cc:doc-example (fib 10) => 55)` → `(assert (equal (fib 10) 55))`に変換して実行。エラー時のコード例: `(cl-cc:doc-example (/ 1 0) => (cl-cc:signals division-by-zero))`。CIパイプラインでの`--doctest-fail-fast`でドキュメントバグを即座に検出。Python doctest / Haskell doctest / Rust `///` tests と同等
- **根拠**: ドキュメントのコード例が古くなってバグを含むケースを自動検出。ドキュメントをテストとして扱い常に最新状態を保証
- **難易度**: Medium

#### FR-904: Type Signature Documentation (型シグネチャ文書化)

- **対象**: `src/cli/main.lisp`, `src/type/`, `src/docs/extractor.lisp`
- **現状**: 型推論（FR-type系）の結果はコンパイル時のみ利用。推論された型シグネチャの外部文書化・表示なし
- **内容**: `./cl-cc show-types src/compile/codegen.lisp` で推論済み型シグネチャを一覧表示。`./cl-cc check-types src/` で型エラーのみを報告（コンパイルせず）。型シグネチャをFR-902（docstring抽出）に自動付加。`(cl-cc:print-inferred-type fn)` でREPLから型を確認。Haskell `:type` / TypeScript `--declaration` / mypy `--show-column-numbers` と同等
- **根拠**: Lispの動的型付けで失われがちな型情報を文書化。API利用者が型シグネチャなしに関数を使う困難を解消
- **難易度**: Medium

#### FR-905: Assertion Density Analysis (アサーション密度解析)

- **対象**: `src/cli/main.lisp`, 新規 `src/analysis/assertion-density.lisp`
- **現状**: テスト品質評価はFR-694（Mutation Testing）のみ。ソースコード中のアサーション・契約の密度を測定しない
- **内容**: `./cl-cc assert-density src/` でコードのアサーション密度を計測。対象: `assert`・`check-type`・`(cl-cc:require ...)`（FR-656 Contract Programming）・型アノテーション。密度指標: アサーション/関数数・アサーション/LOC・境界チェック/配列アクセス比。「アサーション密度が低いモジュール」を警告（テスト補強候補）。FR-692（Coverage）と組み合わせ: カバレッジ高×アサーション密度低 → 「テストは通るが検証が弱い」を発見。NASAの Assertion Density Metric（JPL コーディング標準）に準拠
- **根拠**: カバレッジ100%でも各テストが何を検証しているかを定量化できない問題を解消。コードの「防御度」を数値化
- **難易度**: Medium

---
