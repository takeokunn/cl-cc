# Tooling: Compiler Infrastructure

Compiler frontend optimization, isolated infrastructure, binary/link/FFI, compiler quality, security hardening, diagnostics, compiler pass infrastructure.

---

### Phase 21 — コンパイラ・言語フロントエンド最適化

#### FR-125: Declaration Processing (type/inline/ignore/optimize)

- **対象**: `packages/expand/src/expander.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: `(declare (type fixnum x))` → register-type-mapへの登録、`(declare (inline f))` → インライン化フォース、`(declare (ignore x))` → 未使用警告抑制
- **根拠**: expander.lispにdeclare処理ハンドラが存在しない
- **難易度**: Medium

#### FR-126: define-compiler-macro

- **対象**: `packages/expand/src/expander.lisp`
- **内容**: `*compiler-macro-registry*` (関数名→展開関数) の追加。呼び出しパターンに応じた特化展開をユーザが定義可能
- **根拠**: expander.lispにcompiler-macroの仕組みが存在しない
- **難易度**: Medium

#### FR-127: Type Proclamations (declaim/proclaim ftype)

- **対象**: `packages/expand/src/expander.lisp`, `packages/compile/src/codegen-functions.lisp:17-21`
- **内容**: `(declaim (ftype (function (fixnum) fixnum) foo))` を解析し`*function-type-registry*`に登録、呼び出し時の型チェック省略に活用
- **根拠**: `*function-type-registry*`は存在するがdeclaim処理なし
- **難易度**: Medium

#### FR-128: Typecase Jump Table Dispatch

- **対象**: `packages/expand/src/macros-basic.lisp:271-291`
- **内容**: `(typecase x (fixnum ..) (cons ..))` をネストifでなく型タグインデックスのジャンプテーブルにコンパイル
- **根拠**: 現状は単純な `if (typep x ...)` チェーンに展開
- **難易度**: Medium

#### FR-129: Compile-Time `intern` Resolution

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **内容**: シンボル名とパッケージがリテラル定数の`(intern "CAR" :cl)`をコンパイル時に解決して`vm-const`に変換
- **根拠**: 多くのソースファイルがこのパターンを多用、毎回ランタイムinternが発生
- **難易度**: Low

#### FR-130: Perfect Hash for Compiler Dispatch

- **対象**: `packages/expand/src/expander.lisp`
- **内容**: `compiler-macroexpand-all`の25分岐condをコンパイル時生成のminimal perfect hashに変換
- **根拠**: 全フォームがこの関数を通過する最内ループ
- **難易度**: Medium

#### FR-131: Non-Closure Promotion

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: `vm-captured-vars = nil`のlambda定義時点でクロージャオブジェクト割り当てなしの静的関数参照に昇格
- **根拠**: 現状インライナーがcall siteでのみこれを検出するが、定義時点での除去がない
- **難易度**: Low

#### FR-132: Closure Environment Trimming

- **対象**: `packages/compile/src/closure.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: クロージャボディ内で一切読まれないキャプチャ変数を`vm-captured-vars`から除去
- **根拠**: `find-free-variables`が存在するが`vm-captured-vars`フィルタリングに未活用
- **難易度**: Medium

#### FR-133: Tail Call Through `apply`

- **対象**: `packages/compile/src/codegen.lisp` (ast-apply)
- **内容**: 末尾位置の`(apply f args)`に`vm-tail-call`を発行（現状は`vm-apply` + returnになる）
- **根拠**: `ctx-tail-position`が利用可能だが`ast-apply`コンパイルで未使用
- **難易度**: Medium

#### FR-134: Named-let Support

- **対象**: `packages/expand/src/macros-basic.lisp`
- **内容**: named-let構文を`labels`内の再帰関数に展開するマクロを追加、末尾再帰ループのイディオムをサポート
- **根拠**: 現状named-let構文が存在しない
- **難易度**: Medium

#### FR-135: Loop Macro Quality Fixes

- **対象**: `packages/expand/src/loop-emitters.lisp:173-247`
- **内容**: 特定されたサブオプティマリティを修正:
  1. `:across` ベクタループの不要nil初期化除去 (line 173)
  2. `:hash-keys`/`:hash-values`のCAR重複抽出除去 (line 223)
  3. `:repeat`のデクリメント前チェックに変更 (line 247)
- **難易度**: Easy

---

### Phase 24 — 孤立インフラ統合 (Orphaned Infrastructure Activation)

#### FR-146: E-graph Equality Saturation → optimizer pipeline統合

- **対象**: `packages/optimize/src/egraph.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: `egraph.lisp` に完全なe-graph実装（union-find, pattern matching, cost extraction）が存在するが `optimize-instructions` から一切呼ばれていない
- **内容**: `opt-pass-egraph` パスを追加し、等価飽和ルール（代数的恒等式・分配法則・融合則）を宣言的に記述して既存パイプラインに統合
- **根拠**: `optimizer.lisp` に `egraph` への参照がゼロ。`egraph-match-pattern`・`egraph-apply-rule` が未使用のまま
- **難易度**: Hard

#### FR-147: SSA/CFG → optimizer pipeline統合

- **対象**: `packages/optimize/src/ssa.lisp`, `packages/optimize/src/cfg.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: Cytron SSA構築・支配木・支配フロンティアがすべて実装済みだが `optimize-instructions` から未呼び出し。`loop-depth` フィールドも未使用
- **内容**: `optimize-instructions` の前段でCFG構築・SSA変換を行い、φノードを活用したデータフロー解析（到達定義・ライブ変数）でCSE/DCEの精度を向上。最後にSSA破壊して既存IR形式に戻す
- **根拠**: `ssa.lisp:1`, `cfg.lisp:1` — 完全実装だが孤立。`loop-depth` 未使用（`cfg.lisp:32`）
- **難易度**: Very Hard

---

### Phase 26 — モジュール・コンパイル速度最適化

#### FR-151: インクリメンタルコンパイル / ユーザーコードFASLキャッシュ

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **現状**: `*stdlib-compiled*`（`pipeline.lisp:190`）で標準ライブラリのみキャッシュ。ユーザーコードは毎回フルリコンパイル
- **内容**: ソースファイルのSHA-256ハッシュ+依存関係グラフに基づくFASLキャッシュ。変更なしファイルはキャッシュから復元、変更ファイルと依存ファイルのみ再コンパイル
- **根拠**: `pipeline.lisp:258-348` — `*stdlib-compiled*`以外のキャッシュ機構ゼロ
- **難易度**: Hard

#### FR-152: 推移的関数純粋性推論

- **対象**: `packages/optimize/src/effects.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: `effects.lisp` の効果分類は**命令レベル**のみ。ユーザー定義関数の純粋性は追跡されない
- **内容**: コールグラフを走査して「純粋命令のみからなる関数」→「その関数を呼ぶ関数も純粋」と推移的に伝播。純粋関数呼び出しをDCE/CSEの対象に含める
- **根拠**: `effects.lisp:1-150` — 効果テーブルは命令のみ。関数呼び出し`vm-call`は`:unknown`扱い
- **難易度**: Medium

#### FR-153: マクロ展開メモ化

- **対象**: `packages/expand/src/expander.lisp`
- **現状**: `make-macro-expander`（`expander.lisp:96`）は毎回クロージャを生成。展開結果はキャッシュなし
- **内容**: `(macro-name . s-expr)` をキーとするハッシュテーブルで展開結果をメモ化。副作用のないマクロ（`defun`/`let`/`cond`等の組み込みマクロ）は展開結果を再利用
- **根拠**: `expander.lisp:134` — `expand-progn-with-eager-defmacro` が毎フォームを完全再展開
- **難易度**: Easy

---

### Phase 36 — バイナリ・リンク・FFI

#### FR-194: FFI (Foreign Function Interface)

- **対象**: `packages/compile/src/codegen.lisp`, `packages/emit/src/calling-convention.lisp`
- **現状**: FFIサポートゼロ。dlopen/dlsym/cffi/callback等一切なし
- **内容**: `(cl-cc:foreign-funcall "printf" :string "hello" :int)` 形式のFFI。Cの型定義（`:int`→i32, `:pointer`→i64等）、コールバック（Lisp関数→C関数ポインタ）、外部ライブラリロード
- **根拠**: SBCL/CCL/ECLすべてがFFI提供。システムライブラリ（libc、POSIX）呼び出しに必須
- **難易度**: Very Hard

#### FR-195: DWARF Debug Information Generation

- **対象**: `packages/binary/src/macho.lisp`, `packages/binary/src/elf.lisp`
- **現状**: バイナリ出力にデバッグ情報セクションなし。ソース位置→機械語アドレスのマッピング不可
- **内容**: `.debug_line`（行番号テーブル）、`.debug_info`（変数・関数メタデータ）をMach-O/ELFに出力。`lldb`/`gdb`でソースレベルデバッグ可能に
- **根拠**: AST nodeに`source-file`/`source-line`/`source-column`が既に保持されている（`ast.lisp:14-16`）が未活用
- **難易度**: Hard

#### FR-196: Dynamic Linking / Shared Library Support

- **対象**: `packages/binary/src/macho.lisp`, `packages/binary/src/elf.lisp`
- **現状**: Mach-Oは`+mh-dyldlink+`フラグ（`macho.lisp:46`）を定義するがLC_LOAD_DYLIB等のロードコマンド未実装。ELFは`.o`のみで`.so`未対応
- **内容**: Mach-OにLC_LOAD_DYLIB（dylib参照）、遅延バインドスタブを追加。ELFにPLT/GOTセクション、`.dynamic`セクションを追加。共有ライブラリ（`.dylib`/`.so`）生成
- **根拠**: FFI（FR-194）のランタイム解決に必須。libc等の外部依存解決
- **難易度**: Very Hard

#### FR-197: PIC Code Generation (RIP-relative, GOT/PLT)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **現状**: PIEフラグ設定済み（`macho.lisp:428`）だが実際のコード生成は絶対アドレス（`macho.lisp:326`の固定ロードアドレス`#x100000000`）
- **内容**: x86-64でRIP-relative addressing生成。グローバルデータアクセスをGOT経由に変更。関数呼び出しをPLT経由に変更
- **根拠**: ASLRとPIEの正しい動作に必須。現状はPIEフラグのみで実質PIC非対応
- **難易度**: Hard

#### FR-198: Mach-O Symbol Table Serialization

- **対象**: `packages/binary/src/macho.lisp`
- **現状**: `add-symbol`関数（`macho.lisp:376-396`）でシンボル構造を定義するが、`build-mach-o`でシンボルテーブルをバイナリに**書き出していない**
- **内容**: nlistエントリとstring tableをバイナリに正しくシリアライズ。`nm`コマンドでシンボル一覧表示、`dsymutil`でデバッグシンボル抽出を可能に
- **根拠**: `macho.lisp:116-122` — nlist構造定義済みだがLC_SYMTABのfileoff/nof設定が不完全
- **難易度**: Medium

---

### Phase 39 — コンパイラ品質・検証

#### FR-205: Translation Validation

- **対象**: `packages/optimize/src/optimizer.lisp`, `tests/`
- **現状**: オプティマイザの正当性はユニットテスト（`optimizer-tests.lisp:779行`）とPBT（`ast-pbt-tests.lisp`等）で検証。形式的な意味保存証明なし
- **内容**: 最適化前後のIRに対してビセクション検証器を実装。入力プログラムの観測可能な振る舞い（戻り値・副作用順序）が最適化で不変であることを自動検査
- **根拠**: CompCert/Alive2スタイルの検証。最適化バグの早期検出に不可欠
- **難易度**: Very Hard

#### FR-206: Coverage-Guided Fuzzing

- **対象**: `packages/testing-framework/src/framework-fuzz.lisp`
- **現状**: 文法ベースのfuzzing（`%gen-expr`によるランダムCLプログラム生成）。カバレッジフィードバックなし
- **内容**: `(deftest-fuzz)`にカバレッジ計測（基本ブロック到達率）を追加。未到達パスへの入力生成を優先するフィードバックループ。AFL/libFuzzer方式のミューテーション
- **根拠**: 既存PBT（72,000行）は充実しているがカバレッジ盲点あり。CSmith等のコンパイラfuzzerで発見される深いバグを検出
- **難易度**: Medium

#### FR-207: Reproducible / Deterministic Builds

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/binary/src/macho.lisp`
- **現状**: REPLステートがmutable hash table（CLの反復順序非決定的）。`*pbt-random-state*`が`(make-random-state t)`で真乱数シード
- **内容**: hash-table反復をソート済みキーリストに統一。タイムスタンプ・アドレス情報をバイナリに埋め込まない。定数シードでの決定的コンパイル
- **根拠**: Reproducible builds projectの要件。同一ソースから常に同一バイナリを生成
- **難易度**: Medium

#### FR-208: Cross-Compilation Infrastructure

- **対象**: `packages/mir/src/target.lisp`, `packages/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **現状**: `target.lisp`に4ターゲット定義（x86-64, aarch64, riscv64, wasm32）があるがホスト≠ターゲットの分離なし。`compile-expression`に`:target`パラメータあるがビルドシステムは未対応
- **内容**: `./cl-cc compile --target=aarch64 input.lisp -o output`形式のクロスコンパイル。ホストのランタイム定数（ポインタサイズ、エンディアン）をターゲット値で上書き
- **根拠**: `target.lisp`のRISC-V定義（`*riscv64-target*`）は存在するがバックエンド未実装。クロスコンパイル基盤があれば段階的にバックエンド追加可能
- **難易度**: Hard

---

### Phase 50 — セキュリティ硬化

#### FR-237: Stack Canary / Stack Protector (スタックカナリア)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: 関数プロローグ/エピローグにスタック保護機構なし。バッファオーバーフロー検出不可
- **内容**: `-fstack-protector`相当の機能。関数プロローグでカナリア値をスタックに配置、エピローグで検証。破壊検出時にabort。配列を含む関数のみにデフォルト適用（`-fstack-protector-strong`相当）
- **根拠**: GCC/Clang -fstack-protector / MSVC /GS。セキュリティ基本要件
- **難易度**: Medium

#### FR-238: Control-Flow Integrity (CFI)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: 間接呼び出し（`vm-call`→ネイティブ`call rax`）に検証なし。ROP/JOP攻撃に対する保護なし
- **内容**: 間接呼び出しターゲットの正当性検証。前方エッジCFI（間接call/jump先の関数シグネチャ検証）＋後方エッジCFI（shadow stack / CET）。Intel CETのENDBR64命令生成
- **根拠**: LLVM CFI / Clang -fsanitize=cfi / Intel CET。モダンバイナリのセキュリティ標準
- **難易度**: Hard

#### FR-239: Runtime Sanitizer Instrumentation (サニタイザ計装)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: 実行時メモリエラー検出機構なし。ヒープオーバーフロー・use-after-free・未初期化メモリアクセスが検出されない
- **内容**: `--sanitize=address`モードでのred zone挿入（ヒープ割り当て前後にポイズンバイト）、free後のquarantine、shadow memoryによるアクセス検証。`--sanitize=undefined`で整数オーバーフロー・null参照の計装
- **根拠**: LLVM AddressSanitizer / GCC -fsanitize=address。開発時のメモリバグ早期検出
- **難易度**: Very Hard

---

### Phase 51 — コンパイラ診断・デバッグ基盤

#### FR-240: Source Location Propagation (ソース位置伝播)

- **対象**: `packages/compile/src/codegen.lisp`, `packages/ast/src/ast.lisp`
- **現状**: `ast.lisp:14-16` — ASTノードに`source-file`, `source-line`, `source-column`フィールドが存在するがcodegenで一切参照されない。VM命令やネイティブコードにソース位置が紐づかない
- **内容**: codegen各フェーズでAST→VM命令→MIR→機械語にソース位置情報を伝播。エラーメッセージ・スタックトレース・デバッガにファイル名:行番号を表示。FR-195（DWARF）の前提条件
- **根拠**: 全モダンコンパイラの基本機能。デバッグ体験の根幹
- **難易度**: Medium

#### FR-241: Macro Expansion Tracing (マクロ展開トレース)

- **対象**: `packages/expand/src/expander.lisp`, `packages/cli/src/main.lisp`
- **現状**: `compiler-macroexpand-all`がサイレントに全マクロを展開。中間段階の確認手段なし
- **内容**: `./cl-cc compile --trace-macros input.lisp`で各マクロ展開ステップ（展開前→展開後）をインデント付きで出力。`macroexpand-1`のフック機構で展開深度・展開回数を計測
- **根拠**: SBCL sb-ext:_macroexpand-hook_ / Clojure macroexpand-all。マクロデバッグの基本ツール
- **難易度**: Easy

#### FR-242: Compilation Time Profiling (コンパイル時間プロファイリング)

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **現状**: コンパイルパイプライン各フェーズの実行時間計測なし。ボトルネック特定不可
- **内容**: `./cl-cc compile --time-phases input.lisp`でparse→expand→CPS→codegen→optimize→regalloc→emit各フェーズの経過時間を表示。selfhost時のコンパイル時間分布を可視化
- **根拠**: GCC -ftime-report / Clang -ftime-trace / Rust -Ztime-passes。コンパイラ自体の性能改善サイクルに必須
- **難易度**: Easy

#### FR-243: Incremental Parsing Integration (インクリメンタルパース統合)

- **対象**: `packages/parse/src/incremental.lisp`, `packages/pipeline/pipeline.lisp`
- **現状**: `incremental.lisp:1-176` — tree-sitterスタイルのインクリメンタルパーサが完全実装済みだがパイプライン未接続。`*parse-cache*`（`incremental.lisp:132-150`）のcache-lookup/cache-storeがexportされるが未呼び出し
- **内容**: `pipeline.lisp`のコンパイルフローに`cache-lookup`を挿入し、ファイル変更部分のみ再パース。REPL/LSPモードでの差分コンパイルの基盤
- **根拠**: 既存インフラの活用。tree-sitter / Roslyn incremental parsing。IDE連携の基盤
- **難易度**: Medium

---

### Phase 61 — コンパイラパスインフラ

#### FR-276: Optimization Levels (-O0 to -O3) (最適化レベル)

- **対象**: `packages/cli/src/main.lisp`, `packages/optimize/src/optimizer.lisp`, `packages/pipeline/pipeline.lisp`
- **現状**: 全最適化が無条件実行。インライン閾値=15（`optimizer.lisp:1018`）、最大反復=20（`optimizer.lisp:1042`）がハードコード。CLIに最適化レベルオプションなし
- **内容**: `-O0`（fold+DCEのみ、inline=0、iters=1）、`-O1`（fold+jump+DCE、inline=15、iters=5）、`-O2`（全パス、inline=30、iters=20）、`-O3`（全パス+E-graph飽和、inline=60、iters=40）の4段階。`(declare (optimize ...))`との連携
- **根拠**: GCC/Clang/SBCL全てが`-O`フラグを提供。開発時の高速コンパイルと本番の積極的最適化の切り替えに必須
- **難易度**: Medium

#### FR-277: Pass Dependency Resolution (パス依存関係解決)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: `*opt-convergence-passes*`（`optimizer.lisp:1020`）に8パスが固定順序で列挙。パス間の依存メタデータなし。前提解析の自動注入なし
- **内容**: `defpass`マクロで各パスが`:requires`（必要な解析）`:invalidates`（無効化する解析）`:preserves`（保持する解析）を宣言。トポロジカルソートでパス順序を自動決定。CFG/SSA/支配木を必要に応じて再計算
- **根拠**: LLVM PassManager / GCC pass manager。FR-147（SSA統合）・FR-146（E-graph統合）の前提インフラ
- **難易度**: Hard

#### FR-278: IR Verification Suite (IR検証スイート)

- **対象**: `packages/ir/src/block.lisp`, 新規`packages/ir/src/verify.lisp`
- **現状**: `ir-verify-ssa`（`block.lisp:129-148`）がSSA単一定義性のみ検証。CFG整合性・use-def連鎖・Phi正当性・支配関係の検証なし
- **内容**: 各パス間に挿入可能な包括的IR検証: (1) CFGの前任/後任の双方向一致、(2) 支配木の整合性、(3) use-def連鎖の有効性、(4) Phi引数数=先行ブロック数、(5) 活性情報の正確性。`-O0`では各パス後に実行、`-O2`以上ではdebugビルドのみ
- **根拠**: LLVM `-verify-each` / GCC `--enable-checking`。最適化パスのバグを早期検出
- **難易度**: Medium

#### FR-279: IR Dump CLI Integration (IRダンプCLI統合)

- **対象**: `packages/cli/src/main.lisp`, `packages/ir/src/printer.lisp`
- **現状**: `ir/printer.lisp`にIRプリンタが実装済みだが、CLIからの呼び出し手段なし。パス間IRの比較不可
- **内容**: `--dump-ir`（全パス後にIR出力）、`--dump-ir-before <pass>`・`--dump-ir-after <pass>`（特定パス前後のIR出力）。ファイル出力モード（`.ir`拡張子）。diff可能なテキスト形式
- **根拠**: LLVM `-print-before-all`/`-print-after-all` / GCC `-fdump-tree-all`。最適化デバッグの標準手法
- **難易度**: Easy

#### FR-280: Per-Pass Statistics Collection (パス別統計収集)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: E-graphのみ`egraph-stats`（`egraph.lisp:420-424`）で`:classes`/`:memo-size`/`:worklist`を返す。メインオプティマイザに統計収集なし
- **内容**: `(defstruct pass-stats name instructions-removed instructions-added instructions-changed time-ms)`。各パスの実行前後で命令数を比較。収束ループ全体の統計サマリ。`--opt-stats`フラグで出力
- **根拠**: GCC `-fopt-info` / LLVM `-stats`。どのパスが効果的かの定量評価に必須
- **難易度**: Easy

#### FR-281: Optimization Bisection Support (最適化二分探索)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: E-graphに`fuel`パラメータ（`egraph.lisp:299-317`）があるが、メインオプティマイザにfuelなし。最適化バグの原因パス特定手段なし
- **内容**: `--opt-bisect-limit N`フラグ。N番目の最適化変換まで適用し、以降はスキップ。二分探索でバグを引き起こすパス・変換を特定。環境変数`CL_CC_OPT_BISECT`でも設定可能
- **根拠**: LLVM `-opt-bisect-limit` / GCC `--param=max-iterations`。最適化リグレッションの効率的デバッグ
- **難易度**: Medium

---
