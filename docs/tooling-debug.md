# Tooling: Debug, Diagnostics & Developer Ecosystem

Debugging, diagnostics, developer ecosystem, JIT, LTO, static analysis, SIMD, code coverage, memory profiling, package management, hot reload, security.

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
- **現状**: 現行の in-repo テストフレームワークにカバレッジ計測機構なし。`deftest-each`/`deftest`マクロにカバレッジフックなし
- **内容**: `--coverage`コンパイルフラグでソース行・分岐ごとにカウンタ命令を埋め込み。`(cl-cc:with-coverage ...)` フォームで範囲指定計測。HTML/LCOVカバレッジレポート生成。canonical `make test` フローと統合し、`tests/`内の全ユニットテストへカバレッジを結び付ける
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
