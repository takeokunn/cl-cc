# Tooling: Advanced Compilation I

ML-driven optimization, parallel/distributed compilation, WebAssembly targets, structured concurrency, formal verification, CHERI security, advanced optimization passes, ABI.

---
### Phase 79 — ML駆動最適化

#### FR-372: ML-Guided Inlining (機械学習駆動インライン化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: インライン閾値は固定値15（`optimizer.lisp:1018`）のヒューリスティック
- **内容**: **特徴量抽出**: 呼び出しサイトのcallee命令数・ループ深度・引数定数率・呼び出し頻度（PGOデータ）をベクトル化。**推論**: 軽量二値分類モデル（決定木 or 小規模NN）をコンパイル時に実行してインライン化可否判定。`cl-cc selfhost`ワークロードでトレーニングデータ収集。MLGO (Google 2021) / LLVM ML inliner相当
- **根拠**: Google MLGOプロジェクトでChromium +1.5%、SPEC CPU +0.8%改善。固定閾値より文脈依存判定が優れる
- **難易度**: Hard

#### FR-373: Neural Cost Model (ニューラルコストモデル)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 命令コスト推定なし（後段で実行頻度独立のコスト計算なし）。ループ展開係数・ベクトル化閾値が静的
- **内容**: 実測スループット（nanobenchmark）とコードパターンから**命令列コストの回帰モデル**を学習。`(estimated-cycles instruction-sequence)` API。最適化可否判断に活用。IACA / llvm-mca相当の内製版
- **根拠**: LLVM MachineLearningRegalloc / Halide auto-scheduler。ハードウェア実測に基づくコスト推定は静的テーブルより正確
- **難易度**: Very Hard

#### FR-374: AI-Powered Error Messages (AI強化エラーメッセージ)

- **対象**: `packages/frontend/parse/src/diagnostics.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: `format-diagnostic`（`diagnostics.lisp`）は静的テンプレートメッセージ
- **内容**: コンパイルエラー・型エラー発生時に**LLMベースの修正提案**を生成。エラーコード + コンテキスト（前後3行 + スコープ内変数名）をプロンプトとしてローカルLLM（Ollama / llama.cpp）に送信。`--ai-diagnostics`フラグで有効化。Rust compiler `rustc --explain E0xxx` の進化版
- **根拠**: 2026年時点でClang/Rustがexperimentalで導入開始。コンパイラエラーの学習コスト大幅削減
- **難易度**: Medium

---

### Phase 80 — 並列・分散コンパイル

#### FR-376: Parallel Compilation Pipeline (並列コンパイルパイプライン)

- **対象**: `pipeline/src/pipeline.lisp`, `cl-cc.asd`
- **現状**: コンパイルパイプラインはシングルスレッド逐次実行。ASDFの`:serial t`（`cl-cc.asd:16`）で全ファイルも直列
- **内容**: **関数レベル並列化**: 相互依存のないトップレベルフォーム（`defun`/`defclass`等）をlparallel/bordeaux-threadsのワーカープールで並列コンパイル。`compile-expression`呼び出しをスレッドセーフ化（`*function-registry*`をCAS操作で保護）。`--jobs N`フラグ（デフォルト=CPUコア数）。独立ファイル間の並列ASDFビルド（FR-325との連携）
- **根拠**: GCC `-j` / Ninja / Bazel並列実行。cl-ccのselfhost（84ファイル）をCPUコア数倍速化
- **難易度**: Hard

#### FR-377: Remote Build Execution (リモートビルド実行)

- **対象**: `pipeline/src/pipeline.lisp`, 新規`src/build/remote.lisp`
- **現状**: コンパイルはローカルマシンのみ
- **内容**: Bazel Remote Execution API（REAPI v2）互換のビルドキャッシュプロトコル。コンテンツアドレス型リモートキャッシュ（Action Cache + CAS）。`--remote-cache=grpc://host:8980`でCI/CDサーバのキャッシュを共用。buildbuddy / engflow等のOSSバックエンドと互換
- **根拠**: Bazel RBE / sccache --dist。CI環境での初回ビルド0秒（全ヒット）を実現
- **難易度**: Very Hard

#### FR-378: Build Artifact Signing (ビルド成果物署名)

- **対象**: `cli/src/main.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: 生成バイナリに暗号署名なし
- **内容**: ED25519秘密鍵によるバイナリ署名（`./cl-cc compile --sign-key=key.pem`）。Mach-O Code Signing（`LC_CODE_SIGNATURE`）。SLSA（Supply Chain Levels for Software Artifacts）Provenance記録。`sigstore` / `cosign`互換の署名形式
- **根拠**: Apple notarization要件（macOS 13+）/ GitHub artifact attestations（2024〜）。SLSA Level 3達成に必要
- **難易度**: Medium

---

### Phase 81 — WebAssembly・異種ターゲット

#### FR-380: WASM Component Model / WIT (WASMコンポーネントモデル)

- **対象**: `packages/backend/emit/src/wasm/`, `cli/src/main.lisp`
- **現状**: WASM32バイナリ出力は`target.lisp`に定義済みだが、コンポーネントモデル（`.wasm`コンポーネント）非対応
- **内容**: **WIT（WebAssembly Interface Types）** IDLからCL型マッピングを自動生成。コンポーネントバイナリ形式（`.wasm` v2 + `component` section）エンコーダ。Canonical ABI（lift/lower関数）のコード生成。`wasm-tools compose`互換の出力
- **根拠**: WASI Preview 2（2024安定化）/ WIT IDL（2025 W3C標準化）。WASMコンポーネント間でのCLコード再利用がFFIなしに可能
- **難易度**: Very Hard

#### FR-381: WASI Support (WASIサポート)

- **対象**: `packages/backend/emit/src/wasm/`, `packages/engine/vm/src/io.lisp`
- **現状**: `packages/engine/vm/src/io.lisp`のファイルI/OはSBCL CLOSを直接呼ぶ。WASM実行環境ではcl-ioが使えない
- **内容**: `wasi:filesystem`/`wasi:cli`/`wasi:sockets` (Preview 2) への呼び出し生成。`cl-cc:with-wasi-env`マクロ。`(open "file.txt")` → `wasi_filesystem_open_at` インポート変換。wasmtime / Deno WASI実行確認
- **根拠**: WASI Preview 2は2024年安定化。サーバーレス/エッジ環境でのLispバイナリ実行の基盤
- **難易度**: Hard

#### FR-382: SPIR-V / GPU Target (SPIR-Vターゲット)

- **対象**: `packages/foundation/mir/src/target.lisp`, 新規`packages/backend/emit/src/spirv/`
- **現状**: ターゲット定義は x86-64/AArch64/RISC-V/WASM32 の4つのみ
- **内容**: SPIR-V 1.6（Vulkan 1.3 / OpenCL 3.0対応）バイナリエンコーダ。`(cl-cc:define-kernel)` マクロで数値集約ループをSPIR-V関数にコンパイル。スカラ型の自動SIMD幅推定。CL配列↔GPU バッファ転送ヘルパ。MoltenVK（macOS）/ ROCm（AMD）/ CUDA（NVIDIA via `nvptx`）バックエンド
- **根拠**: Julia GPU.jl / FUTHARK / Halide。HPC・機械学習ワークロードのGPUオフロード
- **難易度**: Very Hard

#### FR-383: Source Maps for WASM/JS (ソースマップ)

- **対象**: `packages/backend/emit/src/wasm/`, 新規`packages/backend/emit/src/js/`
- **現状**: DWARF情報（FR-195）はネイティブバイナリ向け。WASM/JSデバッグ情報なし
- **内容**: WASM出力に`sourceMappingURL`カスタムセクション付与。Source Map v3 JSON生成（`.map`ファイル）。CL S-式の行列番号→WASM byte offset マッピング。Chromeデバッガ / `wasm-sourcemap`ツールとの互換性
- **根拠**: Emscripten / wasm-bindgen のソースマップ出力。WASM上でCLコードをChromeで直接デバッグ可能に
- **難易度**: Medium

#### FR-384: Universal Binary / Fat Binary (ユニバーサルバイナリ)

- **対象**: `packages/backend/binary/src/macho.lisp`, `cli/src/main.lisp`
- **現状**: Mach-Oバイナリは単一アーキテクチャのみ
- **内容**: Mach-O Universal Binary（`FAT_MAGIC = 0xcafebabe`）エンコーダ。`./cl-cc compile --arch=universal input.lisp` でx86-64 + AArch64スライスを1ファイルに格納。スライスのアライメント（ページ境界）とオフセット計算。`lipo -info`互換の出力
- **根拠**: macOS Monterey以降のApple Silicon/Intel両対応バイナリ要件。`/usr/bin`ツール群はすべてFat Binary
- **難易度**: Medium

---

### Phase 82 — 構造化並行性・コルーチン

#### FR-386: Coroutine / Generator Compilation (コルーチン・ジェネレータコンパイル)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`, `packages/engine/compile/src/cps.lisp`
- **現状**: CPS変換（`cps.lisp`）はすべてのcontinuationをクロージャで表現。コルーチン専用構文なし
- **内容**: `(cl-cc:define-generator (fibonacci) ...)` + `(cl-cc:yield value)` マクロ。CPS変換でyield pointを自動抽出し**状態機械**に変換。`(cl-cc:next gen)` でステップ進行。`for ... in (fibonacci)` のloop統合。Python generators / Rust `impl Iterator` 相当
- **根拠**: CPS変換がすでに存在するため、yieldは本質的に「current continuationの保存」として自然に実装できる。`(call/cc)` 的アプローチも可能
- **難易度**: Medium

#### FR-387: Green Threads / Fiber Runtime (グリーンスレッド/ファイバーランタイム)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/backend/runtime/src/`, 新規`src/concurrent/`
- **現状**: VMはシングルスレッドの逐次実行器
- **内容**: M:N スケジューラ（OS Nスレッド上でMグリーンスレッド）。スタック切り替え（`setjmp`/`longjmp` or `ucontext_t`、ネイティブバイナリ向け）。VMレベルでのfiberサポート（スタックポインタ保存+復元）。`(cl-cc:spawn (lambda () ...))` + `(cl-cc:yield)` API。`(cl-cc:channel)` でCommunicating Sequential Processes（CSP）スタイル
- **根拠**: Go goroutine / Erlang process / Kotlin coroutines。並行I/Oサーバー・並列コンパイルの基盤
- **難易度**: Very Hard

#### FR-388: Async/Await Compilation (async/awaitコンパイル)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/cps.lisp`
- **現状**: 非同期I/Oパターンなし。`packages/engine/vm/src/io.lisp`はブロッキングI/O
- **内容**: `(cl-cc:async (defun fetch (url) ... (cl-cc:await (http-get url)) ...))` マクロ。`async`関数をCPS変換でステートマシンに変換（async/awaitはCPSの糖衣構文）。`Promise<T>`型相当の`future`オブジェクト。イベントループ（libuv/io_uring統合またはピュアCL実装）
- **根拠**: CPS変換（FR-386の基盤）がすでに存在するため、async/awaitはほぼ「CPSをユーザー向けに公開する」操作に相当する
- **難易度**: Hard

---

### Phase 83 — 抽象解釈・形式検証

#### FR-390: Value Range Propagation / VRP (値域伝播)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, 新規`packages/engine/optimize/src/vrp.lisp`
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

- **対象**: `packages/backend/emit/src/aarch64-codegen.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: ポインタは単純な64ビット整数。メタデータなし
- **内容**: ARM Morello / RISC-V CHERI向けの**fat pointer**生成（128ビット: アドレス64bit + メタデータ64bit）。`rt-alloc`でallocated rangeをケイパビリティに埋め込み。範囲外アクセスでのハードウェアトラップ。`--target=morello`コンパイルフラグ。CheriBSD上でのバイナリ動作確認
- **根拠**: CHERI研究プロジェクト（Cambridge大学）+ ARM Morello（2022〜）。2026年時点で組み込み・サーバーへの採用拡大。C言語の空間安全性問題を根本解決するハードウェア
- **難易度**: Very Hard

#### FR-396: RISC-V Vector Extension / RVV (RISC-VベクタISA)

- **対象**: 新規`packages/backend/emit/src/riscv64-codegen.lisp`
- **現状**: `*riscv64-target*`（`target.lisp`）は定義済みだがバックエンド実装なし
- **内容**: RISC-V Vector Extension（RVV 1.0）命令生成。可変長VLEN対応のvl/vtype設定。`vle32.v`/`vfmacc.vv`等のベクタロード・演算命令エンコーダ。FR-345（自動ベクトル化）のRISC-Vバックエンドとして統合
- **根拠**: SiFive / SpacemiT-X60（RISC-V V extension）/ C908。2026年時点でRISC-Vは組み込み〜HPC領域で急速普及。RVVはx86 AVXに相当
- **難易度**: Hard

#### FR-397: DWARF 5 / Variable Location Quality (DWARF 5・変数位置品質)

- **対象**: `packages/backend/binary/src/macho.lisp`, FR-195（DWARF基本）の拡張
- **現状**: FR-195でDWARF基本情報生成を計画。最適化後の変数追跡は未考慮
- **内容**: **DWARF 5固有機能**: `DW_OP_entry_value`（関数エントリ時の引数値復元）、`DW_AT_default_value`（最適化で消えた変数のデフォルト値）、`DW_TAG_skeleton_unit`（薄いデバッグ情報+外部`.dwo`分離）。最適化後もデバッガで変数が`<optimized out>`にならないよう変数ロケーションリスト（`DW_AT_location` + ロケーションリスト）を生成
- **根拠**: DWARF 5（2017 / LLVM採用2019〜）/ GDB 10+。最適化コードのデバッグ体験が大幅改善
- **難易度**: Hard

#### FR-398: Compiler Plugin / Extension API (コンパイラプラグインAPI)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/optimize/src/optimizer.lisp`, `cli/src/main.lisp`
- **現状**: マクロ拡張は可能（`defmacro`）だが最適化パス・コード生成フックの外部登録API なし
- **内容**: `(cl-cc:define-compiler-pass :after :fold-constants ...)` でユーザー定義最適化パスを登録。`(cl-cc:define-emit-hook :before :function-entry ...)` でコード生成フック。ダイナミックロード可能なプラグイン（`.so`）。GCC Plugin API / LLVM PassPlugin / Rust compiler plugins（rustc_private）相当
- **根拠**: ドメイン特化最適化（DSL用）・プロファイルインストルメンテーション・独自ABI生成など、コンパイラ本体を変更せずに拡張できる
- **難易度**: Hard

#### FR-399: Thread Sanitizer (スレッドサニタイザ)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/backend/runtime/src/`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: FR-239はAddressSanitizer/UBSanのみ。データ競合検出なし
- **内容**: **Shadow memory方式**: 各メモリアドレスに最終アクセスしたスレッドIDとロック状態を記録（8バイト→2バイトシャドウ）。ロック取得/解放のインストルメンテーション。同期なしでの同一アドレスへの競合アクセス検出。FR-387（グリーンスレッド）の安全性検証に利用。Helgrind / LLVM TSan相当
- **根拠**: データ競合は未定義動作でありデバッグが極めて困難。並行ランタイム（FR-387）の開発に必須
- **難易度**: Hard

---

### Phase 85 — 高度最適化パス

#### FR-400: Loop Invariant Code Motion / LICM (ループ不変コード移動)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, FR-147（SSA統合）前提
- **現状**: ループ内の不変式はループごとに再評価される。ループボディから外への移動なし
- **内容**: **支配木ベースのホイスト**: ループヘッダを支配するブロックにループ不変命令を移動。副作用なし（FR-152の純粋性推論）かつ定義がループ内のみの命令を対象。逆方向にはシンク（cold pathへ押し込み）も実施。CFG構築（FR-147）の副産物として支配木が利用可能
- **根拠**: GCC `-fmove-loop-invariants` / LLVM `loop-rotate` + `licm`。ループ集中型コードでの最重要最適化の一つ
- **難易度**: Medium

#### FR-401: Loop Unrolling (ループ展開)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: ループ展開なし。カウントが定数のloopも1回ずつ実行
- **内容**: **完全展開（unroll-all）**: トリップカウント≤8かつ定数の場合にループ本体をN回複製してジャンプ除去。**部分展開（unroll-factor）**: 可変長ループに対して展開係数2/4を適用しパイプラインハザード削減。末尾処理ループ（peel）自動生成。`(declare (cl-cc:unroll 4))` 明示指示
- **根拠**: GCC `-funroll-loops` / LLVM `loop-unroll`. 小ループのオーバーヘッド除去・SIMD幅合わせに必須
- **難易度**: Medium

#### FR-402: Loop Fusion / Loop Fission (ループ融合・分割)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 隣接する独立ループは個別に実行される
- **内容**: **Loop Fusion**: 同一反復空間の隣接ループを1ループに統合（キャッシュ局所性向上、ループオーバーヘッド削減）。**Loop Fission**: 依存関係が複雑なループを独立部分に分割してベクトル化阻害を除去。依存グラフ解析（FR-342ポインタ解析との連携）
- **根拠**: GCC loop-fusion / Polly / LLVM LoopFuse。数値計算・データ変換ループで顕著な効果
- **難易度**: Hard

#### FR-403: Loop Tiling / Blocking (ループタイル化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 多重ループのイテレーション順序変換なし
- **内容**: 2重以上のネストループを**タイル（ブロック）**単位で実行順序を変換。内ループのアクセスパターンがキャッシュラインに収まるようタイルサイズを自動決定（L1/L2サイズから計算）。行列乗算・画像フィルタ等への適用。PLUTO / Polly (LLVM) 相当の多面体変換の簡略版
- **根拠**: キャッシュミス削減で行列演算が5〜10x高速化。数値演算系の最重要変換の一つ
- **難易度**: Hard

#### FR-404: Global Value Numbering / GVN (大域値番号付け)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: CSEパス（`opt-pass-common-subexpressions`）は局所的なブロック内のみ
- **内容**: **SSAベースGVN**: 各SSA値に**値番号**を付与（意味的同値な式は同一番号）。支配木を辿ってDOMツリー全体で重複式を検出・除去。`vm-phi`ノードの合体。`x + 0`/`x * 1`等の代数的恒等式もGVNルールで統一的に処理。LLVM GVN / Click's 1995 GVN相当
- **根拠**: ブロックローカルCSEより大幅に強力。関数全体を通じた重複計算の除去
- **難易度**: Hard

#### FR-405: Sparse Conditional Constant Propagation / SCCP (疎条件定数伝播)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 定数畳み込みはシングルパス（`opt-pass-fold-constants`）。条件分岐による到達不能パス除去との連携なし
- **内容**: **CFG + 定数伝播の同時実行**: 条件分岐の条件が定数と判明した時点で到達不能パスをDCEし、その結果新たな定数が発生する連鎖を一括処理。WorlistアルゴリズムでSSA値とCFGエッジの状態（unreachable/constant/varying）を伝播。Wegman-Zadeck (1991) アルゴリズム
- **根拠**: LLVM `sccp` / GCC `ccp`. 通常のCP+DCEの個別適用より発見できる定数が格段に多い
- **難易度**: Hard

#### FR-406: Partial Redundancy Elimination / PRE (部分冗長除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: CSEはすべてのパスで重複する式のみ除去（完全冗長性）
- **内容**: **一部のパスで重複する式**も除去対象に拡張。critical edgeへのinsertionで非冗長パスに「補完計算」を挿入してすべてのパスで冗長にした後CSE。ループ不変コード移動（FR-400）をPREの特殊ケースとして統一。Knoop-Rüthing-Steffen (1992) lazy code motionアルゴリズム
- **根拠**: LLVMはGVN+PREを統合（`gvn-hoist`/`gvn-sink`）。CSE+LICMより強力な汎用最適化
- **難易度**: Very Hard

#### FR-407: Strength Reduction (強度低減)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 乗算・除数がべき乗の場合のシフト変換のみ（`opt-pass-fold-constants`の一部）
- **内容**: **誘導変数に対する強度低減**: ループ内の`i * constant`を加算の列に変換。`i * 7`→ `s0=0; loop: s0+=7`。`mod power-of-2`→ビットマスク。`/ constant`→乗法逆数+シフト（`imul`+`sar`命令列）。GCC `-fstrength-reduce` 相当。整数除算は最大5倍高速化
- **根拠**: 整数除算はx86-64で60〜80クロックかかる最重量命令。コンパイラによる除算→乗法変換は基本中の基本
- **難易度**: Medium

#### FR-408: Instruction Scheduling (命令スケジューリング)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **現状**: 命令はVM→IR→機械語の変換順に出力。パイプライン考慮なし
- **内容**: **命令レベル並列性（ILP）抽出**: データ依存グラフ（DDG）構築→トポロジカルソート→レイテンシ考慮スケジューリング（list scheduling）。x86-64のAgner Fog命令表（レイテンシ/スループット）参照。`fmul`後の`fadd`（latency=4）間に独立命令を挟む。out-of-order CPUでも効果的なin-order発行を補助
- **根拠**: GCC `-fschedule-insns` / LLVM MachineScheduler。浮動小数点集約コードで15〜25%高速化
- **難易度**: Hard

#### FR-409: Memory SSA (メモリSSA)

- **対象**: 新規`packages/engine/optimize/src/memory-ssa.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: ヒープアクセス（`vm-slot-read`/`vm-slot-write`）はSSAの枠外で依存関係が保守的
- **内容**: **MemorySSA**: 各ヒープ書き込みを仮想SSAメモリ定義（MemoryDef）、読み込みをMemoryUse、phi合流点をMemoryPhiとして表現。GVN（FR-404）・PRE（FR-406）・LICM（FR-400）のメモリ操作への適用を可能にする。`(slot-read obj :x)` のループ不変ホイストをエイリアス解析（FR-340）と組み合わせて実施
- **根拠**: LLVM MemorySSA（2017〜）。メモリ最適化の精度を飛躍的に向上させるインフラ。FR-340〜FR-342の解析を統合するハブ
- **難易度**: Very Hard

#### FR-410: If-Conversion / Predication (分岐→選択命令変換)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: `if`は常に条件分岐命令（`jcc`）にコンパイル。branch mispredictionのペナルティあり
- **内容**: **If-conversion**: 短い`(if cond then else)`で`then`/`else`ともに副作用なし・命令数≤4の場合、両方を無条件実行して結果を`cmov`（条件付き移動）で選択。**Predication**: AArch64のcondition codesを活用した条件実行命令生成。分岐予測ミス（~15クロック）を`cmov`（1クロック）で置換
- **根拠**: GCC `-fif-conversion` / LLVM SelectionDAG if-conv. 予測困難な短い条件分岐で顕著な効果
- **難易度**: Medium

---

### Phase 86 — 高度コード生成・ABI

#### FR-412: NaN Boxing / Pointer Tagging Optimization (NaNボクシング)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: タグ付きポインタは下位2〜3ビットを使用（`vm.lisp`のタグ定数）。64bit全体の活用なし
- **内容**: **NaN boxing**: IEEE 754 doubleのsilent NaN空間（`0x7FF8000000000000`〜`0x7FFFFFFFFFFFFFFF`）にfixnum・pointer・booleanを格納。floatはそのままで他の型はNaN空間にエンコード。メモリ帯域50%削減（8バイト/値を維持しつつboxingゼロ）。LuaJIT・JavaScriptCore・Duktapeの採用実績
- **根拠**: 動的型付け言語での最も効率的な値表現。現在のタグ付きポインタより値の解釈が1命令高速
- **難易度**: Hard

#### FR-413: Register Coalescing (レジスタ合体)

- **対象**: `packages/engine/compile/src/regalloc.lisp`
- **現状**: コピー命令（`vm-move`）をレジスタ割り当て後に除去する仕組みなし
- **内容**: **Interference Graph合体**: コピー命令の src/dst が干渉しない場合に同一物理レジスタに割り当て、`mov rax, rbx` を除去。Conservative coalescing（Chaitin-Briggs）+ Aggressive coalescing（George-Appel）の2段階。`vm-move`命令の大幅削減（関数体に平均10〜20%のコピーが存在）
- **根拠**: LLVM RegisterCoalescer / GCC regmove. コピー命令は最も除去コストパフォーマンスの高い最適化
- **難易度**: Hard

#### FR-414: Custom Calling Convention (カスタム呼び出し規約)

- **対象**: `packages/backend/emit/src/calling-convention.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: すべての内部関数呼び出しがシステムABI（System V AMD64 / AAPCS64）を使用
- **内容**: **fast-call規約**: 引数・戻り値を最大限レジスタで渡す内部専用規約（呼び出し側と被呼び出し側が同じコンパイラ管轄の場合）。**tailcc**: 末尾呼び出し最適化専用の規約（スタックフレーム不変）。`(declare (cl-cc:calling-convention :fast))` アノテーション。FFI境界はシステムABIを維持
- **根拠**: LLVM fastcc / GCC `__attribute__((regparm))`. インタープロシージャル呼び出しのオーバーヘッドを30〜50%削減
- **難易度**: Hard

#### FR-415: Frame Pointer Omission / FPO (フレームポインタ省略)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 関数プロローグ/エピローグにRBP保存・復元が常に含まれるか不明
- **内容**: リーフ関数（内部呼び出しなし）または固定スタックフレームサイズの関数でRBPを通常レジスタとして使用。`-O2`以上でデフォルト有効、`-fno-omit-frame-pointer`で無効化。DWARF CFI（Call Frame Information）を`DW_CFA_def_cfa_expression`で正確に記述してデバッガ互換性維持
- **根拠**: GCC `-fomit-frame-pointer` / Clang. RBP1本分のレジスタ節約はx86-64（16本中）で~6%の差
- **難易度**: Medium

#### FR-416: Stack Frame Packing (スタックフレームパッキング)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: ローカル変数のスタック配置にアライメント最適化なし
- **内容**: ローカル変数をサイズ降順にソートして配置（大きいものから並べてパディング最小化）。`sizeof(fixnum)=8` / `sizeof(bool)=1` の混在時にギャップを詰める。スタックフレームサイズ削減→キャッシュライン占有率低下
- **根拠**: GCC `-fpack-struct` の関連技術。スタックフレームの無駄なパディングはキャッシュ汚染の原因
- **難易度**: Easy

#### FR-417: Monomorphization (単相化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/type/type/src/inference.lisp`
- **現状**: ジェネリック関数は常に動的ディスパッチ（CLOSメソッドテーブル）。型特化版の静的生成なし
- **内容**: `(declaim (ftype (function (fixnum fixnum) fixnum) +))` 等の型宣言がある関数呼び出しで、**型特化クローン**を生成（`add-fixnum-fixnum`等の命名規則）。型引数の組み合わせ数が閾値以内の場合に展開。ボックス化解除（fixnum引数のuntagging）と組み合わせて純粋整数演算に変換。Rust/C++テンプレートの動的言語版
- **根拠**: 型情報を最大活用したコード特化。CLの`(the fixnum x)`宣言と組み合わせて効果的
- **難易度**: Hard

---

### Phase 88 — GC・メモリランタイム高度化

#### FR-420: Concurrent / Incremental GC (並行・インクリメンタルGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: 2世代GC（`gc.lisp`）はStop-The-World方式。コレクション中はVM実行停止
- **内容**: **インクリメンタルマーク**: マークフェーズを小ステップに分割してVM実行と交互実行。**Tri-color marking**（白/灰/黒）+ write barrier（書き込み時に参照先を灰に再マーク）。**並行スイープ**: スイープをバックグラウンドスレッドで実施。最大停止時間をパラメータ化（`--gc-max-pause 5ms`）。Dijkstra Tri-color algorithm
- **根拠**: Go GC / V8 Orinoco / OpenJDK G1。リアルタイム応答性の基本要件。セルフホスティングコンパイラの長時間実行時のレイテンシ改善
- **難易度**: Very Hard

#### FR-421: Compacting GC / Moving GC (コンパクティングGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 現GCは非移動型（オブジェクトのアドレスが固定）
- **内容**: **Mark-Compact**: フルGC時にライブオブジェクトをヒープ先頭に詰め直してフラグメンテーション解消。ポインタ更新のための**forwarding pointer**テーブル。**Semi-space GC**（Cheney copying）をYoung generationに適用。コンパクション後のキャッシュ局所性向上
- **根拠**: JVM Shenandoah / ZGC / OCaml GC。長時間実行でのヒープフラグメンテーション解消。フラグメントが激しいと実効ヒープが理論値の50%以下に
- **難易度**: Very Hard

#### FR-422: Weak References & Finalizers (弱参照・ファイナライザ)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: CLオブジェクトへの参照はすべて強参照。GCによる回収を阻む
- **内容**: `(cl-cc:make-weak-pointer obj)` + `(cl-cc:weak-pointer-value wp)`。GCマークフェーズ後に弱参照リストをスキャンして未マークオブジェクトをnilに差し替え。**Finalizers** (`cl-cc:finalize obj callback`): 回収直前にコールバック呼び出し（ファイル・ネットワーク等のリソース解放）。`weak-hash-table`実装（キャッシュの自動エビクション）
- **根拠**: SBCL weak-pointer / Haskell `System.Mem.Weak` / Python `weakref`。メモリリーク回避のためのキャッシュ実装に不可欠
- **難易度**: Medium

#### FR-423: Arena / Region-Based Memory (アリーナ・領域ベースメモリ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, 新規`packages/backend/runtime/src/arena.lisp`
- **現状**: すべての割り当てはGC管理ヒープへ。一括解放の仕組みなし
- **内容**: `(cl-cc:with-arena (ar) ...)` マクロでアリーナスコープを定義。スコープ内の`rt-alloc`はアリーナバンプポインタから確保。スコープ終了時に**一括解放**（GCなし、O(1)）。コンパイルフェーズ（CPS変換・最適化）の一時オブジェクトに適用してGCプレッシャー大幅削減。Zig arena allocator / Rust `bumpalo` クレート相当
- **根拠**: コンパイラの各パスは大量の一時ASTノードを生成後に全廃棄するため、アリーナが最適なアロケーションパターン
- **難易度**: Medium

#### FR-424: Pinned Objects / GC-Safe FFI (ピン留めオブジェクト)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/compile/src/codegen.lisp` (FFI)
- **現状**: FFI呼び出し（FR-194）中にGCが動くとLispオブジェクトのアドレスが変わる可能性（FR-421コンパクティングGC導入後）
- **内容**: `(cl-cc:with-pinned-object (obj) ...)` マクロ。GCコレクション中のオブジェクト移動を一時禁止（pinセット管理）。FFIハンドシェイク（`gc-safe-point`フック）。JNI Critical Region / .NET `fixed` ステートメント相当
- **根拠**: FR-421（Moving GC）と FR-194（FFI）の共存に必須。Moving GCなしでも将来の拡張に備えて設計すべき
- **難易度**: Hard

#### FR-425: Reference Counting Mode (参照カウントモード)

- **対象**: `packages/backend/runtime/src/`, `packages/engine/compile/src/codegen.lisp`
- **現状**: GCのみでオブジェクト管理。決定論的な破棄のタイミングなし
- **内容**: `--memory-model=arc`フラグで**自動参照カウント**（ARC）モード。`vm-retain`/`vm-release`命令の自動挿入（コンパイル時挿入）。エスケープ解析（FR-341）でstatic所有権が証明できる場合はRC操作省略。循環参照用weak-pointer（FR-422）との組み合わせ。Objective-C ARC / Swift ARC / Rust Arc<T>相当
- **根拠**: 組み込み環境でのGC不使用・決定論的リソース管理に必須。IoT/リアルタイム系への展開に向けた選択肢
- **難易度**: Very Hard

---

### Phase 89 — 型システム拡張

#### FR-428: Occurrence Typing (オカレンス型付け)

- **対象**: `packages/type/type/src/inference.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 型推論はHindley-Milner（`inference.lisp`）。`(typep x 'fixnum)` 分岐後に型が絞り込まれない
- **内容**: `(if (typep x 'fixnum) <then> <else>)` のthenブランチでxを`fixnum`型に絞り込み。`(if (null x) <then> <else>)` のelseブランチで`x`からnilを除く。typecase/cond-typeのパターンに対して**型環境の分岐**を管理。型絞り込み後のdevirt（FR-337）・unboxing（FR-417）に連携
- **根拠**: TypeScript / Typed Racket / Flow。動的型チェックを静的最適化のヒントに変換する最重要機能
- **難易度**: Hard

#### FR-429: Row Polymorphism (行多相性)

- **対象**: `packages/type/type/src/inference.lisp`, `packages/type/type/src/types.lisp`
- **現状**: CLOSオブジェクトの型はclass名による公称型。構造的部分型なし
- **内容**: `{:x fixnum, :y fixnum | r}` 形式のrow型変数。スロット`:x`と`:y`を持つ任意のオブジェクトを受け付ける多相型。OCaml object types / PureScript / Elm と同等。`(defun distance (p) (sqrt (+ (slot-value p :x)² (slot-value p :y)²)))` が任意のxy-スロット保持オブジェクトに機能
- **根拠**: CLのCLOSは公称型だがrow多相を加えると構造的サブタイピングが可能。duck typingを型安全にする
- **難易度**: Very Hard

#### FR-430: Unboxed Representations (アンボックス表現)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: fixnum・float含むすべての値がタグ付き表現（boxed）またはヒープオブジェクト
- **内容**: `(declare (type fixnum x))` 宣言のある局所変数を**untagged 64-bit integer**として扱い、タグ付け/外し命令を除去。`(simple-array fixnum (*))` を unboxed fixnum配列として格納（8バイト/要素、GCスキャン不要）。float配列も同様（double-float = 8バイト生値）。GCルートとしてのマーキングから除外
- **根拠**: SBCL `(declare (type (simple-array fixnum) arr))` は既にunboxed最適化を行う。数値集約コードで2〜4x高速化
- **難易度**: Hard

#### FR-431: Staged Compilation / Multi-Stage Programming (段階的コンパイル)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: コンパイル時と実行時の2フェーズのみ。コンパイル時に実行時コードを生成するメタプログラミングが`defmacro`に限定
- **内容**: `(cl-cc:$` ... `)` でコンパイル時に実行される式を明示。`(cl-cc:~` ... `)` でコード引用（引用されたコードはコンパイル先コードに挿入）。MetaOCaml `.<` / `>.` ブラケット / BER MetaOCaml相当。`(cl-cc:run ...)` で段階式を実行時コードとして解釈実行
- **根拠**: MetaOCaml / Scala LMS / Terra。高性能DSL・特化コンパイラ・解釈器の特化に不可欠
- **難易度**: Very Hard

#### FR-432: Null Safety Analysis (null安全性解析)

- **対象**: `packages/type/type/src/inference.lisp`, `packages/engine/compile/src/codegen.lisp`
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

- **対象**: `src/lsp/`, `packages/frontend/expand/src/expander.lisp`
- **現状**: LSP（FR-319）での構文ハイライトはTextMate文法ベースのみ
- **内容**: LSP `textDocument/semanticTokens`ハンドラ。パース済みASTから各シンボルのセマンティックカテゴリを返す（`function`/`variable`/`macro`/`type`/`parameter`/`keyword`/`builtin`）。未定義変数・シャドウイング変数に`modifiers: ["deprecated"]`/`["readonly"]`付与。テーマに依存しない意味的ハイライト
- **根拠**: LSP 3.16（2021〜）のセマンティックトークン。Rust-analyzer / clangd ともに実装済み。`defmacro`と`defun`を色分けするのは構文的ハイライトでは不可能
- **難易度**: Medium

#### FR-437: Inlay Hints (インレイヒント)

- **対象**: `src/lsp/`, `packages/type/type/src/inference.lisp`
- **現状**: 型推論結果（FR-127等）はコンパイルメッセージでのみ確認可能
- **内容**: LSP `textDocument/inlayHint`ハンドラ。**型ヒント**: `(let ((x (+ 1 2))) ...)` の `x` 隣に `: fixnum` を表示。**パラメータ名ヒント**: `(foo 1 2)` を `(foo a: 1 b: 2)` 表示。**戻り値型ヒント**: 関数定義の閉じ括弧後に推論された型を表示。VS Code で表示/非表示をトグル可能
- **根拠**: LSP 3.17（2022〜）. Rust-analyzer のインレイヒントが好評を博し、Kotlin/C#/Java も追随。型推論結果の可視化でコードの理解が大幅に向上
- **難易度**: Medium

#### FR-438: Code Actions / Quick Fixes (コードアクション・クイックフィックス)

- **対象**: `src/lsp/`, `packages/frontend/parse/src/diagnostics.lisp`
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

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `cli/src/main.lisp`
- **現状**: 実行トレース・メトリクス・ログの標準出力形式なし
- **内容**: **OTLP（OpenTelemetry Protocol）**: コンパイルパイプライン各フェーズをspanとして記録（`parse` → `expand` → `cps` → `codegen` → `optimize` → `emit`）。VM関数呼び出しをspanとしてトレース。`--otlp-endpoint=http://localhost:4317`でJaeger/Tempo等に送信。`(cl-cc:with-span "my-operation" ...)` ユーザーAPIも提供
- **根拠**: OpenTelemetry 1.0（2021〜）/ CNCF graduated（2023）。2026年時点で本番可観測性のデファクト標準
- **難易度**: Medium

#### FR-443: eBPF Profiling Integration (eBPFプロファイリング統合)

- **対象**: `cli/src/main.lisp`, `packages/backend/binary/src/`
- **現状**: プロファイリングはVM命令カウンタ（FR-316）のみ。カーネルレベルのサンプリングプロファイラなし
- **内容**: **Linux perf_event + BPF**: `./cl-cc run --ebpf-profile` でLinux eBPFプローブを使ったオーバーヘッドほぼゼロのサンプリング。バイナリの`.debug_frame`（DWARF）を活用してJavaScript/Rustと同様にフレームを解決。bpftrace / bcc連携。macOS Instrumentsへの対応（DTrace）
- **根拠**: perf + DWARF unwinding は2026年のLinux標準プロファイリング手法。プロダクション環境での継続プロファイリングが可能
- **難易度**: Hard

#### FR-444: Flame Graph Generation (フレームグラフ生成)

- **対象**: `cli/src/main.lisp`, FR-316（ベンチマークフレームワーク）
- **現状**: ベンチマーク結果はテキスト/JSON出力のみ。可視化なし
- **内容**: サンプリングプロファイラ出力（FR-443またはVM命令カウンタ）から**Brendan Gregg Flame Graph**形式（SVG）を生成。`./cl-cc run --profile foo.lisp | ./cl-cc flamegraph -o foo.svg`。折り畳み可能なインタラクティブSVG。`(cl-cc:profile-report :format :flamegraph)`
- **根拠**: Flame Graph（Brendan Gregg, 2011〜）は2026年で最も普及したプロファイリング可視化形式。Rustperfなどすべての主要ツールが対応
- **難易度**: Easy

#### FR-445: Hardware Performance Counters (ハードウェア性能カウンタ)

- **対象**: `cli/src/main.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: ウォールクロック時間のみ計測可能
- **内容**: `perf_event_open`syscall（Linux）/ `kpc_set_config`（macOS）でCPUカウンタ取得。**IPC（Instruction Per Cycle）**: キャッシュミス（L1/L2/L3）数、分岐予測ミス数、TLBミス数をベンチマーク結果に併記。`./cl-cc bench --counters=cache-misses,branch-misses foo.lisp`。数値計算最適化のボトルネック同定に使用
- **根拠**: `perf stat` / Google Benchmark の`--perf_counters` フラグ。「遅いがなぜか」を説明する最直接な情報
- **難易度**: Medium

---

### Phase 92 — インターオペラビリティ

#### FR-447: C Header Generation (Cヘッダ生成)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `cli/src/main.lisp`
- **現状**: Cコードからcl-cc関数を呼び出す方法なし。逆方向（cl-cc→C）はFFI（FR-194）で対応
- **内容**: `./cl-cc compile --emit-header foo.lisp -o foo.h` でエクスポート関数の`.h`自動生成。`(export-to-c foo (fixnum fixnum) fixnum)` アノテーション。関数プロトタイプ・型定義（`typedef struct cl_object* cl_object_t;`）・初期化関数（`cl_cc_init()`）を生成。`extern "C"` ガード付き
- **根拠**: CL関数をCから呼べると既存Cライブラリとの統合が可能。SBCL `sb-alien:alien-funcall` の逆方向
- **難易度**: Medium

#### FR-448: Python/ctypes ABI Compatibility (Python interop)

- **対象**: `packages/backend/emit/src/calling-convention.lisp`, FR-447との連携
- **現状**: Python cftypes / cffi からcl-cc共有ライブラリを呼ぶためのABI保証なし
- **内容**: `--abi=ctypes` フラグで Python `ctypes.CDLL` 互換のC ABIを強制。`int`/`double`/`char*`へのCL型自動変換（boxing/unboxing）。`cffi` パッケージ定義自動生成（FR-447のヘッダから）。`pip install cl-cc-wheel` 形式のwheelパッケージング対応
- **根拠**: データサイエンス/ML系Pythonコードとの統合。数値計算コアをcl-ccで書いてPythonから呼ぶユースケース
- **難易度**: Hard

#### FR-449: JVM Bytecode Target (JVMバイトコードターゲット)

- **対象**: `packages/foundation/mir/src/target.lisp`, 新規`packages/backend/emit/src/jvm/`
- **現状**: x86-64/AArch64/RISC-V/WASM32/SPIR-Vのみ
- **内容**: JVM .class ファイル生成（JVM SE 21+ bytecode）。CLオブジェクト→`java.lang.Object`マッピング。CLOSクラス→`java.lang.record`生成。JNIを介した既存Javaライブラリ呼び出し。`Maven/Gradle`依存として配布可能なJARファイル生成
- **根拠**: JVM上のLisp（Clojure）が実証した市場。Android(ART)/GraalVM Native Imageにも応用可能
- **難易度**: Very Hard

---

### Phase 93 — バイナリサイズ・起動時間最適化

#### FR-452: Dead Symbol Stripping (デッドシンボル除去)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/backend/binary/src/elf.lisp`
- **現状**: バイナリにすべてのコンパイル済み関数が含まれる。未呼び出し関数が最終バイナリに残留
- **内容**: **Section-level DCE**: 各関数を個別セクション（`__TEXT,__text.function_name`）に配置。リンカのDead Section除去（Mach-O `-dead_strip` / ELF `--gc-sections`）を活用。FR-338（全プログラムDCE）と連携して未到達関数セクションを除去
- **根拠**: GCC `-ffunction-sections -Wl,--gc-sections`. 大きなランタイムライブラリで最大70%のバイナリサイズ削減事例あり
- **難易度**: Medium

#### FR-453: Binary Compression / UPX-style Packing (バイナリ圧縮)

- **対象**: `cli/src/main.lisp`
- **現状**: バイナリはアンパックのまま
- **内容**: `./cl-cc compile --compress-binary` で出力バイナリにLZ4/Zstd圧縮+デコンプレッサスタブを付加（UPX方式）。`__TEXT`セグメントのread-only部分のみ圧縮（self-modifying防止）。起動時にページフォールトベースのオンデマンドデコンプレッション。組み込み向けに特に有効
- **根拠**: UPX / elf-packer。フラッシュ容量が限られる組み込みデバイスでのバイナリサイズ削減
- **難易度**: Hard

#### FR-454: Startup Time Optimization (起動時間最適化)

- **対象**: `cli/src/main.lisp`, `pipeline/src/pipeline.lisp`
- **現状**: `./cl-cc run`起動時にSBCLランタイム + ASDF + 全srcファイルのFASLロードが発生
- **内容**: **Pre-linked stdlib**: 標準ライブラリの機械語を`__DATA_CONST`セグメントにpre-link（FR-364 Image-Based Developmentと連携）。**Lazy loading**: 未使用モジュールの初期化を初回呼び出しまで遅延（`__attribute__((constructor))` + guard変数）。**BOLT PGO layout**（FR-508）でコールドスタートパスを`.text.cold`に隔離
- **根拠**: Clangのstartup 時間最適化。`cl-cc selfhost`の実行時間測定で起動が律速になっている可能性
- **難易度**: Hard

#### FR-455: Segment Merging / Section Layout (セグメント統合・セクションレイアウト)

- **対象**: `packages/backend/binary/src/macho.lisp`
- **現状**: セクション構成が固定（`__TEXT/__text`, `__DATA/__data`等）
- **内容**: 小さなセクション（<4KB）のマージでページ境界パディング削減。Read-Only dataを`__TEXT`セグメントに移動してCOW（Copy-On-Write）共有を最大化。`__LINKEDIT`セグメントの最後配置（macOS要件）の自動調整。`size`コマンドで計測可能なセクション別サイズ報告
- **根拠**: Apple mach-o-linker最適化手法。複数プロセスが同じバイナリを実行する際のメモリ共有率向上
- **難易度**: Medium

---

### Phase 94 — 追加セキュリティ・解析

#### FR-458: Spectre/Meltdown Mitigations (Spectre/Meltdown緩和策)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `src/jit/`
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

- **対象**: `packages/type/type/src/inference.lisp`, `src/analyze/`
- **現状**: セキュリティラベルなし。高機密値から低機密チャネルへの情報漏洩を検出不可
- **内容**: 型に**機密性ラベル** `(High)` / `(Low)` を付与。`High`ラベル付き値が`Low`チャネル（ログ出力・ネットワーク送信）に流れる場合にコンパイルエラー。**Declassification**: `(cl-cc:declassify val)` で意図的な降格を明示。SIF（Simple Information Flow）/ FlowCaml / IFDS algorithm
- **根拠**: Jif（Java Information Flow）/ FlowCaml。暗号キー・個人情報の意図しない漏洩をコンパイル時に証明する
- **難易度**: Very Hard

#### FR-461: Integer Overflow Detection (整数オーバーフロー検出)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
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

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: `handler-case`/`condition`はVM命令レベルでの動的フレーム積み上げ方式。例外を投げない正常パスにもオーバーヘッドあり
- **内容**: **LSDA（Language Specific Data Area）**テーブルをバイナリの`__TEXT,__gcc_except_tab`セクションに生成。各`handler-case`/`restart-case`の範囲とハンドラアドレスをテーブル化。Itanium ABI `_Unwind_RaiseException` + libunwindを呼び出す例外ディスパッチ。正常パス（conditionを投げない場合）のコストをゼロに。Windows SEH（Structured Exception Handling）形式にも対応
- **根拠**: C++ `try/catch` の標準実装（GCC/Clang）。正常パスのEHコストは文字通りゼロになりcondition systemの普及を妨げなくなる
- **難易度**: Very Hard

#### FR-466: Jump Threading (ジャンプスレッディング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 条件分岐先がさらに無条件分岐・別の条件分岐になっているパスの短絡なし
- **内容**: **パス条件の伝播**: 条件分岐のthen/elseパスでどちらの値が確定しているかを記録。次の条件分岐の条件が既知の場合、中間ブロックをバイパスして直接ジャンプ先にスレッド（経路複製 + DCEで実現）。`(if a (if b ...) ...)` のネストをフラット化。GCC `-fthread-jumps` / LLVM JumpThreading pass
- **根拠**: 条件分岐の約20〜30%はジャンプスレッディングで除去可能。分岐予測ミスも減少
- **難易度**: Medium

#### FR-467: Tail Merging / Code Deduplication (末尾コード統合)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 複数の分岐末尾に同一命令列が重複している場合も個別にコンパイル
- **内容**: 複数のCFGブロック末尾で同一命令列を検出し、**共通末尾ブロック**に統合してジャンプを一箇所に収束。逆向きのCSEとして実装（後方スキャン）。バイナリサイズ削減 + I-キャッシュ局所性向上。GCC `-freorder-blocks` / LLVM MergeFunctions pass
- **根拠**: SBCL/LispWorksのコンパイラでも実施。コード重複はコンパイラが生成するswitch/caseパターンで特に多い
- **難易度**: Medium

#### FR-468: Bounds Check Elimination / BCE (配列境界チェック除去)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: ベクタアクセス（`vm-aref`）は毎回境界チェックを発行。VRPとの連携なし
- **内容**: VRP（FR-390）の値域情報を活用。`i`の区間が`[0, n-1]`と確定していれば`(aref arr i)`の境界チェックを除去。ループ事前チェック（loop pre-check）: ループ前に一度だけ境界検証を行い、ループ内チェックを全削除。`(declare (type (simple-array fixnum) arr))` + `(declare (type fixnum i))` との相乗効果
- **根拠**: JVM JIT BCE / .NET JIT BCE。配列集約ループで10〜20%高速化。数値計算の主要ボトルネック除去
- **難易度**: Hard

#### FR-469: Global Dead Store Elimination (大域デッドストア除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: DCEパス（`opt-pass-dead-code`）は読まれない値の計算を除去。書かれた後に読まれないストアの除去なし
- **内容**: MemorySSA（FR-409）ベース: 後続するメモリ使用のないMemoryDefを「デッドストア」として除去。エイリアス解析（FR-340）でストア先が独立していると証明できる場合のみ除去（安全性保証）。`(setf (slot-value obj :x) val)` が以降読まれない場合に削除
- **根拠**: LLVM DeadStoreElimination pass。メモリ書き込み集約コードでの効果大。MemSSAなしでは安全な実装が困難
- **難易度**: Hard

#### FR-470: Shrink-Wrapping (プロローグ/エピローグ縮小配置)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: カリー保存レジスタの保存/復元は関数エントリ/リターンに固定配置
- **内容**: 実際にそのレジスタを使うコードパスの直前にプロローグを、使い終わった直後にエピローグを移動。例外パス（cold path）では callee-save registers を保存しないで早期リターン。支配木解析でセーブ/リストア挿入点を最適化。LLVM ShrinkWrap / GCC shrink-wrapping
- **根拠**: 早期リターンパスが多い関数（エラーチェック多数）でプロローグコストをゼロに。callee-save保存は約4命令/レジスタ
- **難易度**: Hard

#### FR-471: Function Outlining / Cold Path Extraction (関数アウトライン化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: インライン化の逆（コード抽出）なし。コールドパスがホットパスと同じ関数内に同居
- **内容**: 実行頻度の低いコードブロック（PGOデータ or 静的ヒューリスティック: errorハンドラ・フォールバック処理）を**新たな関数**として抽出。抽出された関数を`.text.cold`セクションに配置（FR-508との連携）。ホット関数のI-キャッシュフットプリントを削減。GCC `-freorder-functions` / LLVM機能
- **根拠**: Googleの研究でShrink-wrapping + Outliningの組み合わせがL1 I-cache miss率を15〜25%削減
- **難易度**: Medium

#### FR-472: Partial Inlining (部分インライン化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: インライン化は関数全体を複製するか全くしないかの2択
- **内容**: 関数の**ホットパスのみ**を呼び出しサイトにインライン化し、コールドパスへの分岐を元の関数エントリへのテール呼び出しに変換。例: バリデーション失敗パスを呼び元に残し成功パスのみインライン。インライン閾値超えの大関数への適用。LLVM PartialInlining pass
- **根拠**: 大きな関数（VTABLEディスパッチ等）を完全インライン化するとコードサイズ爆発。部分インライン化はサイズとパフォーマンスのトレードオフを最適化
- **難易度**: Hard

#### FR-473: Loop Rotation & Peeling (ループ回転・ピーリング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: ループは記述通りの形で生成。ループ入口でのヘッダチェックが毎回発生
- **内容**: **Loop Rotation**: `while (cond) { body }` を `if (cond) { do { body } while (cond) }` に変換。ループボディ内でのバックエッジを末尾に配置し分岐予測を改善。**Loop Peeling**: 最初の1〜N回のイテレーションをループ前に展開（境界特殊ケースの最適化）。LLVM LoopRotate + LoopPeel
- **根拠**: Loop rotationはLICM（FR-400）・ベクトル化（FR-345）の前提変換。ループ最適化パイプラインの最初のステップ
- **難易度**: Medium

#### FR-474: Critical Edge Splitting (クリティカルエッジ分割)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, FR-147（SSA構築）
- **現状**: SSA phiノードにコピー挿入が必要なクリティカルエッジ（複数出口→複数入口）の処理が不明確
- **内容**: **クリティカルエッジ**（複数後任を持つブロック→複数前任を持つブロック）に空のダミーブロックを挿入。SSA破壊（phi除去）・loop rotation・コードモーションの正確性確保。全最適化パスの前処理として実施
- **根拠**: SSAに基づく最適化パスの正確性の基盤。GCC / LLVMのSSAパイプラインの必須ステップ
- **難易度**: Medium

---

### Phase 97 — コード生成・バックエンド拡張

#### FR-476: LLVM IR Emission Backend (LLVM IRバックエンド)

- **対象**: 新規`packages/backend/emit/src/llvm/`, `packages/foundation/mir/src/target.lisp`
- **現状**: ネイティブコード生成は独自x86-64/AArch64バックエンドのみ
- **内容**: cl-cc IR→LLVM IR（`.ll`テキスト / bitcode `.bc`）への変換。`llvm-as`/`llc`/`opt`ツールチェーンへの橋渡し。LLVMの全最適化パス（auto-vectorization・polly等）を活用可能に。`--emit=llvm-ir`フラグ。`wasm-ld`によるWASMリンク。LLVM IRはcl-ccにとって**第5のターゲット**として機能
- **根拠**: cl-ccのフロントエンド品質を活かしつつバックエンドはLLVMに委譲できる。新アーキテクチャ対応がLLVM経由で無償に得られる
- **難易度**: Hard

#### FR-477: CPU Feature Dispatch / Function Multiversioning (CPU機能ディスパッチ)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: コンパイル時に単一のISAレベルを想定。実行時CPUのSSE4.2/AVX2/AVX-512活用なし
- **内容**: `(cl-cc:multiversion my-fn (:avx2 #'my-fn-avx2) (:sse4.2 #'my-fn-sse42) (:baseline #'my-fn-base))` マクロ。**CPUID検出**: バイナリ起動時に`cpuid`命令で対応ISAを検査し関数ポインタテーブルを初期化。GCC `__attribute__((target_clones("avx2","sse4.2","default")))` / Clang `__attribute__((target_clones))` 相当
- **根拠**: 同一バイナリでSSE2必須機と最新AVX-512機の両方で最適性能を発揮。数値計算ライブラリの標準手法
- **難易度**: Hard

#### FR-478: Tree-Based Instruction Selection / BURS (木リライト命令選択)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: VM命令→x86-64命令は1対1の手書きマッピング（`*x86-64-emitter-entries*`alist 45エントリ）
- **内容**: IR式ツリーに対してBottom-Up Rewrite System（BURS）で**最小コスト命令列**を選択。`vm-add(vm-mul(a,b),c)` → `lea rax,[rbx*c+rdx]`のような複合命令への畳み込み。パターン: コスト付き書き換えルール `(add (mul r c)) → lea [r*c]`。BURG / IBURG ツール相当の手実装
- **根拠**: LLVMのDAGISel / GCCのRTL最適化。1対1マッピングより明らかに優れた命令列を生成できる（特に LEA活用、FMA命令統合）
- **難易度**: Hard

#### FR-479: Intrinsic Function Registry (イントリンシック関数レジストリ)

- **対象**: `packages/engine/compile/src/builtin-registry.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `builtin-registry.lisp`の~210エントリはVM命令への直接マッピング。コンパイラ特殊インライン（例: `(length "abc")` → 定数3）なし
- **内容**: **コンパイラ組み込み関数**: `(cl-cc:define-intrinsic length (string) fixnum ...)` でコンパイル時に特殊処理される関数を登録。`(length "abc")` → `vm-const 3`、`(car (cons a b))` → `vm-move a`（consアロケーション除去）、`(not (not x))` → `x`。`(abs x)` → `vm-abs`命令（x86-64 `andpd`）。FR-334（PIC）でホットパスに適用
- **根拠**: SBCL compiler transforms / GCC builtin_expect. 標準ライブラリの重要関数を既知のパターンとして特殊化
- **難易度**: Medium

#### FR-480: Sample-Based PGO / AutoFDO (サンプルベースPGO)

- **対象**: `pipeline/src/pipeline.lisp`, `cli/src/main.lisp`
- **現状**: FR-508（BOLT）は後処理バイナリ最適化。フロントエンドへのプロファイルフィードバックなし
- **内容**: `Linux perf record` の出力（`perf.data`）からコンパイラが利用できるプロファイルデータへの変換。**AutoFDO**: ソースコード行番号→サンプルカウントのマッピング（FDO profile format）。インライン閾値・ループ展開係数・アウトライン判定をプロファイルで調整。インストルメンテーション不要でプロダクション実行データを活用。Google AutoFDO / GCC `-fauto-profile`
- **根拠**: Instrumentation PGOと異なりプロダクション環境での計測が可能。Google社内でChromeの5〜10%速度向上を達成
- **難易度**: Hard

#### FR-481: Vectorized Reduction Optimization (リダクションループ最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: sum/max/min等のリダクションループはスカラ逐次実行
- **内容**: **自動リダクション認識**: `(loop for x in arr sum x)` → SIMD並列部分和生成 + 最終集約。**水平加算** (`_mm256_hadd_ps`) / **水平最大** (`_mm256_max_ps`) のSIMD命令生成。コンパイラによるloop accumulatorの依存性解析でリオーダー可能と証明。`(declare (cl-cc:reduction sum))` ヒント
- **根拠**: GCC/Clangのリダクション自動ベクトル化。数値集約の典型パターンで4〜8x高速化
- **難易度**: Hard

---

### Phase 98 — 診断・コンパイラ品質向上

#### FR-484: "Did You Mean?" Suggestion Engine (候補提示エラーエンジン)

- **対象**: `packages/frontend/parse/src/diagnostics.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 未定義変数/関数のエラーメッセージはシンボル名のみ。類似シンボル候補なし
- **内容**: **編集距離（Levenshtein）ベース候補検索**: `*function-registry*`・`*class-registry*`・ローカル変数スコープから類似名を最大3候補提示。スコアリング（先頭文字一致ボーナス）。`"undefined function: LENGHT; did you mean LENGTH, LENGTHY?"` スタイルの出力。Rust/Clangの`did you mean`機能の実装
- **根拠**: SBCL のtypoエラーメッセージは現在未定義の名前をそのまま表示するだけ。Rustが採用して以来コンパイラのデファクト機能に
- **難易度**: Easy

#### FR-485: Warnings as Errors / -Werror (警告をエラーに昇格)

- **対象**: `packages/frontend/parse/src/diagnostics.lisp`, `cli/src/main.lisp`
- **現状**: 警告（`:warning` severity）と診断は分離されているが、ビルド失敗への変換機構なし
- **内容**: `--Werror` フラグで全警告をエラーに昇格（ビルド失敗）。`--Werror=unused-variable` で特定カテゴリのみ昇格。`--Wno-error=deprecated` で特定カテゴリをエラー昇格から除外。CI環境での「警告ゼロポリシー」強制。FR-318（警告システム）・FR-317（構造化診断）との統合
- **根拠**: GCC/Clang `-Werror`. CI/CDでの品質ゲートとして標準的。cl-ccのテストスイート自体にも適用すると品質向上
- **難易度**: Easy

#### FR-486: Diagnostic Categories & Filtering (診断カテゴリ・フィルタリング)

- **対象**: `packages/frontend/parse/src/diagnostics.lisp`, `cli/src/main.lisp`
- **現状**: 警告の種類を個別に有効/無効化できない
- **内容**: 診断カテゴリ定義（`-Wunused-variable`/`-Wtype-mismatch`/`-Wshadowing`/`-Wdeprecated`等）。`--Wall`（全警告有効）/`--Wextra`（追加警告）/`--Wno-<category>`（個別無効化）。`diagnostic`構造体に`category`フィールド追加。ソース行コメント `;;; cl-cc:ignore-warning unused-variable` で行単位抑制。`clang-tidy` / `cargo clippy` スタイルの設定ファイル（`.cl-cc-lint.toml`）
- **根拠**: GCC `-W` フラグ体系。ユーザーが自分のコードに適した警告レベルを設定できる
- **難易度**: Medium

#### FR-487: Optimization Remarks (最適化説明レポート)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `cli/src/main.lisp`
- **現状**: 最適化が適用されたか失敗したかのフィードバックなし
- **内容**: `--Rpass=inline` でインライン化の成否を報告（「function foo inlined at bar.lisp:42」「not inlined: too large (20 > 15)」）。`--Rpass-missed=vectorize` でベクトル化できなかった理由を報告（「loop has dependency on x」）。YAML/JSON形式での出力（CI統合用）。最適化レベル変更・`declare`追加のガイダンスを含む。Clang `-Rpass` / LLVM OptimizationRemarkEmitter相当
- **根拠**: 「なぜこのコードが遅いのか」「どう書き直せば最適化されるか」をコンパイラが直接教えてくれる。Rust `-Copt-level` + `cargo-llvm-lines`の中間
- **難易度**: Medium

#### FR-488: 3-Stage Bootstrap Verification (3段階ブートストラップ検証)

- **対象**: `cli/src/main.lisp`, `tests/`, `flake.nix`
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

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `pure`（副作用なし・グローバル不参照）/`const`（引数のみに依存）/`noreturn`の自動推論なし。手動`declaim`のみ
- **内容**: コールグラフ解析で自動推論: **noreturn**: 全パスが`error`/`abort`で終わる関数。**pure**: 副作用なし・グローバル読み取りのみ（FR-152との連携）。**const**: 引数以外への依存なし（MemSSAでメモリ操作なしと証明）。推論結果を`*function-attr-table*`に登録してCSE/DCE/LICM/並列化の対象判定に活用
- **根拠**: GCC `-O2`でも`__attribute__((pure))`の自動推論は行われない（手動のみ）。LLVMは部分的に自動推論。cl-ccの純粋性推論（FR-152）をこれで完成させる
- **難易度**: Medium

#### FR-491: Macro Hygiene Checker (マクロ衛生チェッカー)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/frontend/expand/src/macro.lisp`
- **現状**: マクロ展開時の変数キャプチャを検出する機構なし
- **内容**: マクロ展開時に導入されるバインディング名が、マクロ呼び出しスコープの変数名と衝突しているかをチェック。`(gensym)`を使用していないマクロ本体内の変数バインディングを警告。**Hygienic macros自動変換**: 非衛生的マクロをgensymベースに自動変換するリファクタリング提案。`--Whygienic`フラグ
- **根拠**: SchemeのR5RS衛生マクロ / Racket `syntax-rules`。CLのdefmacroは非衛生的だが意図しないキャプチャは重大なバグ源
- **難易度**: Medium

---

### Phase 99 — 数値・文字列・型特化最適化

#### FR-494: Numeric Tower Optimization (数値タワー最適化)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: bignum/rational/complexの演算は汎用パス。fixnum特化のみ一部実装
- **内容**: **bignum最適化**: 小さいbignumをスタック割り当て（escape解析連携）。`(* big1 big2)` のKaratsuba乗算（閾値以上）。`(expt 2 n)` → ビットシフト。**rational**: `(+ 1/3 1/6)` のコンパイル時評価。**complex**: `(abs #C(3.0 4.0))` → `5.0`定数畳み込み。`(realpart #C(x y))` → `x`（一段目の素早い展開）
- **根拠**: CLの数値タワーはANSI仕様だが多くの処理系がfixnum以外を遅くしている。科学技術計算への適用に必須
- **難易度**: Medium

#### FR-495: String Interning & Small String Optimization (文字列インターン・SSO)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 文字列は毎回新規ヒープオブジェクト割り当て。同一内容の文字列が複数存在
- **内容**: **Small String Optimization (SSO)**: 15バイト以下の文字列をヒープポインタなしでオブジェクトヘッダ内にインライン格納。**文字列インターン**: `(intern-string "key")` でグローバルプール管理、`eq`比較可能に。**コンパイル時文字列定数プール**: 同一定数文字列を`rodata`セクションで共有（FR-207 Reproducible buildsと連携）
- **根拠**: C++ `std::string` SSO（libstdc++/libc++とも採用）/ Java String.intern(). ハッシュテーブルキーとして文字列を多用するコードでヒープ割り当て大幅削減
- **難易度**: Medium

#### FR-496: Auto-Differentiation (自動微分)

- **対象**: 新規`src/autodiff/`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 微分演算のコンパイルサポートなし
- **内容**: **Forward mode AD**: `(cl-cc:with-dual-numbers ...)` で二重数（値+微分値）を使った前進自動微分。関数に対して`(gradient f x)` が導関数値を返す。**Reverse mode AD (backprop)**: 計算グラフのテープ記録による逆伝播。`(cl-cc:gradient-tape (tape) ... (gradient tape f x))` API。`(declare (cl-cc:differentiable))` で対象関数を指定
- **根拠**: JAX / Julia Zygote / Enzyme LLVM pass。機械学習・数値最適化・物理シミュレーションへの応用。CLの数値計算能力と組み合わせると強力
- **難易度**: Hard

#### FR-497: Polyhedral Loop Optimization (多面体ループ最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, FR-403（Loop Tiling）の拡張
- **現状**: Loop Tiling（FR-403）は単純なキャッシュ局所性改善。複雑な多重ループ変換なし
- **内容**: **多面体モデル**: アフィンループ（インデックスが線形式のループ）をPolytope（多面体）で表現。合法変換（Loop Fusion/Fission/Permutation/Skewing/Reversal）の自動適用。依存グラフに基づく合法性検証。Pluto / LLVM Polly相当の実装。`(declare (cl-cc:polyhedral))` でアフィン性アノテーション
- **根拠**: 行列演算・畳み込み・FFTで劇的な最適化（10x以上）。FR-403のタイル化を多面体モデルで理論的に正確に実装
- **難易度**: Very Hard

#### FR-498: Global Constant Propagation (大域定数伝播)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 定数伝播はローカル変数のみ。一度だけ代入されるグローバル変数（`defparameter`）の値は伝播されない
- **内容**: **compile-time invariant globals**: `(defconstant +pi+ 3.14159...)` は全参照を定数に置換（現在も一部実施）。`(defparameter *max-size* 1024)` が変更されない場合のinter-moduleDCE。`(defvar *debug-mode* nil)` が常にnilなら関連コードをDCE。修正検出はCFG上のall-paths解析
- **根拠**: CLの`defconstant`はコンパイラに変更なしを保証する。`defparameter`の不変性推論で標準ライブラリの大量の設定変数を定数化できる
- **難易度**: Medium

#### FR-499: Precompiled Modules / PCH (プリコンパイルモジュール)

- **対象**: `pipeline/src/pipeline.lisp`, `cl-cc.asd`
- **現状**: FASLキャッシュ（FR-151）はSBCLのもの。cl-ccのコンパイル済みIR（VM program）のヘッダプリコンパイルなし
- **内容**: **PCH（Precompiled Header）形式**: よく使われるインポート群（`cl-cc:`, `cl:`の共通部分）を事前にコンパイル・バイナリにシリアライズ。`#include <stdcl.pcl>` 的な自動検出。ASDF `:defsystem-depends-on` との統合。コンパイル時の重複parse/expand工程をスキップ。clang PCH / GCC PCH相当
- **根拠**: cl-cc標準ライブラリ（`packages/engine/vm/src/*.lisp`全体）の毎回再parseが省略でき、セルフホスト時間を大幅削減
- **難易度**: Medium

#### FR-500: Whole-Program Type Inference (全プログラム型推論)

- **対象**: `packages/type/type/src/inference.lisp`, `pipeline/src/pipeline.lisp`
- **現状**: 型推論はファイル単位。`packages/engine/compile/src/cps.lisp`が`packages/engine/compile/src/codegen.lisp`の関数の型を知らない
- **内容**: **Module-level type inference**: コールグラフ解析で呼び出し先の型シグネチャを呼び出し元に伝播。`(defun foo (x) (bar x))` でfoo引数の型はbarの引数型制約から推論。LTO（FR-335）と連携したリンク時型推論。Hindley-Milner + let-polymorphism のモジュール間拡張。OCaml cross-module inlining + 型推論と同等
- **根拠**: 全プログラム型推論で単相化（FR-417）・devirt（FR-337）・BCE（FR-468）の精度が大幅向上
- **難易度**: Very Hard

#### FR-501: Loop Auto-Parallelization (ループ自動並列化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, FR-387（グリーンスレッド）との連携
- **現状**: ループは常にシングルスレッド実行
- **内容**: データ依存解析（FR-342ポインタ解析）でループ反復間の依存がないと証明された場合に**並列ループに変換**。`(cl-cc:parallel-for i 0 n ...)` のランタイムディスパッチ生成。スレッドプール（FR-387）にタスク分割。並列オーバーヘッドが利益を上回る場合は直列化（閾値: トリップカウント * body命令数）。OpenMP `#pragma omp parallel for` の自動版
- **根拠**: GCC `-ftree-parallelize-loops` / Intel OpenMP auto-parallelization. 大規模配列処理で線形スケール達成
- **難易度**: Very Hard

---

### Phase 100 — ループ解析・高度変換 II

#### FR-510: Scalar Evolution Analysis / SCEV (スカラ進化解析)

- **対象**: 新規`packages/engine/optimize/src/scev.lisp`, FR-147（SSA）前提
- **現状**: ループ変数が何回目の反復で何の値を持つかの代数的記述なし。LICM/BCE/ベクトル化が保守的推論に頼っている
- **内容**: ループ変数を**閉形式の加算連鎖**（Additive Recurrence: `{i₀, +, step}`）として解析。`(loop for i from 0 below n)` → `i = {0, +, 1}₍ₗₒₒₚ₎`、トリップカウント `= n`。`(* i 4)` → `{0, +, 4}`。多段ネストにも対応（Chrec: Chain of Recurrences）。LICM・BCE（FR-468）・ループ展開（FR-401）・ストレングスリダクション（FR-407）の精度を大幅向上
- **根拠**: LLVM ScalarEvolution / GCC SCEV。ループ最適化パイプラインの中核解析。これなしでBCEとLICMは保守的になる
- **難易度**: Hard

#### FR-511: Loop Interchange (ループ交換)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, FR-510（SCEV）前提
- **現状**: ネストループのイテレーション順序は変更不可
- **内容**: 多重ネストループの内外ループ順序を交換。外側ループが列方向・内側ループが行方向の場合（行優先メモリ上）に交換してキャッシュ局所性改善。**合法性**: 依存グラフのLevel Vector解析で交換が安全と証明された場合のみ実施。`(loop for i ... for j ...)` のネスト順最適化
- **根拠**: GCC loop-interchange / Polly。行列演算のキャッシュミス率を数十分の一に削減。Loop Tiling（FR-403）の前処理として適用
- **難易度**: Hard

#### FR-512: Loop Unswitching (ループアンスイッチング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: ループ内の不変条件分岐を外に出す変換なし（LICMは命令移動のみ、条件分岐は対象外）
- **内容**: `(loop ... (if invariant-cond then else) ...)` を `(if invariant-cond (loop ... then ...) (loop ... else ...))` に変換。ループを条件分岐の外側に**2つ複製**。ループ内の分岐予測ミスを完全排除。コードサイズと速度のトレードオフ（`-O2`でのみ適用）。LLVM LoopUnswitch / GCC `-funswitch-loops`
- **根拠**: `(loop for x in list (if debug (log x)) (process x))` 形式が多数。デバッグフラグ検査など不変条件がループ内に多い
- **難易度**: Medium

#### FR-513: Software Pipelining / Modulo Scheduling (ソフトウェアパイプライン)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **現状**: 命令スケジューリング（FR-408）は基本ブロック内のみ。ループ反復間のオーバーラップなし
- **内容**: **Modulo Scheduling**: ループ本体をInitiation Interval（II）で割り当て、前の反復の命令と次の反復の命令を**オーバーラップ**実行。`prologue + kernel + epilogue`に展開。FP演算（レイテンシ4〜8クロック）のスループットを最大化。メモリアクセスのパイプライン化（ロードの投機的先行実行）。GCC `-fmodulo-sched` / Itanium EPIC
- **根拠**: 高レイテンシ命令が多い数値計算ループでスループット2〜4x向上。FMA（Fused Multiply-Add）をフルに活用できる
- **難易度**: Very Hard

#### FR-514: Software Prefetch Insertion (ソフトウェアプリフェッチ挿入)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **現状**: メモリアクセスパターンの事前予測なし。キャッシュミスは実アクセス時まで隠蔽されない
- **内容**: ループ内で**N反復先**のメモリアドレスを計算し`prefetcht0`/`prfm`命令を挿入（ストリーミングアクセスパターンを検出）。SCEV（FR-510）でアクセスパターンがアフィンと判明した場合に適用。プリフェッチ距離はL2/L3レイテンシから自動計算。`(declare (cl-cc:prefetch-distance 8))` 手動ヒント。GCC `-fprefetch-loop-arrays`
- **根拠**: キャッシュミスは100〜300クロック。プリフェッチでストリーミングアクセスを完全隠蔽できる
- **難易度**: Hard

#### FR-515: Load-Store Forwarding & Elimination (ロードストア転送・除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, FR-409（Memory SSA）前提
- **現状**: メモリ書き込み直後の同アドレス読み取りに余分なロード命令が残る
- **内容**: **Store-to-Load Forwarding**: `(setf (slot-value obj :x) v)` の直後の `(slot-value obj :x)` を `v` に置換（ロード除去）。**Redundant Load Elimination**: 同じアドレスへの2回目のロードをMemorySSAで1回目の結果で置換（MemSSAのuse-def連鎖を辿る）。x86-64のAGU（Address Generation Unit）ストールも軽減
- **根拠**: CLOSスロットアクセスのget/setが連続するパターンで頻発。MemorySSAなしでは安全な実装が困難（FR-409の活用先）
- **難易度**: Medium

#### FR-516: AoS to SoA Transformation (配列構造体→構造体配列変換)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: CLOSオブジェクトの配列（AoS: Array of Structures）は各オブジェクトが散在したメモリレイアウト
- **内容**: `(make-array n :initial-element (make-instance 'point))` パターンを検出し、**SoA（Structure of Arrays）**に変換：`:x`フィールドの配列 + `:y`フィールドの配列。SIMD操作がSoAで最大効率（隣接x値のvpacked-add）。`(declare (cl-cc:soa point))` アノテーションで明示指定も可
- **根拠**: Cゲームエンジン（Unity DOTS / Unreal ECS）がSoAに全面移行。AutoVec（FR-345）の効果を最大化する前提変換
- **難易度**: Hard

#### FR-517: Gather/Scatter Vectorization (ギャザー/スキャタベクトル化)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, FR-345（自動ベクトル化）の拡張
- **現状**: ベクトル化は連続メモリアクセスのみ対応
- **内容**: **非連続アクセス**のSIMDベクトル化：`(aref arr index-vec)` → `vpgatherdq`（AVX2）/ `vpgatherqq`（AVX-512）命令生成。インデックスベクタが既知の場合のシャッフル最適化（`vpermq`）。SpMV（疎行列ベクトル積）への適用。LLVM VectorCombine gather/scatter support
- **根拠**: AVX2（2013〜）がgather命令を導入。疎データ構造・間接参照パターンのSIMD化に必須
- **難易度**: Hard

#### FR-518: Global Code Motion / GCM (大域コード移動)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, FR-404（GVN）+ FR-147（SSA）前提
- **現状**: LICM（FR-400）はループ不変式の移動のみ。CFG全体を通じた最適配置なし
- **内容**: **Cliff Click's GCM**: 各命令の**最早配置**（dominance frontier上方）と**最遅配置**（使用箇所直前）を計算し、ループ深度が最小の位置に配置。LICMより汎用（ループ外コードにも適用）。GVNと組み合わせることで冗長計算の最適配置を実現。LLVM GVNHoist / Click's 1995 GCM algorithm
- **根拠**: LICMの一般化。CFG上の任意の「不必要に内側にある計算」を移動できる
- **難易度**: Hard

---

### Phase 101 — 並行性プリミティブ・メモリモデル

#### FR-520: Atomic Operations (アトミック操作)

- **対象**: 新規`src/concurrent/atomic.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: CAS操作・フェッチアンドアドなしで、ロックフリーアルゴリズムが実装不可能
- **内容**: `(cl-cc:atomic-compare-and-swap place expected new)` → `cmpxchg`命令。`(cl-cc:atomic-fetch-add place delta)` → `lock xadd`命令。`(cl-cc:atomic-load place :order :acquire)` / `(cl-cc:atomic-store place value :order :release)`。メモリオーダリング: `:relaxed`/`:acquire`/`:release`/`:acq-rel`/`:seq-cst`（C++20 `std::atomic` 相当）。AArch64は`ldadd`/`stlr`命令
- **根拠**: ロックフリーGC（FR-420）、JIT コードキャッシュ更新（FR-330）、関数レジストリ更新（FR-363）の基盤。アトミックなしで並行性は実現不可
- **難易度**: Medium

#### FR-521: Memory Barriers & Fences (メモリバリア・フェンス)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
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

- **対象**: `packages/backend/runtime/src/heap.lisp`, `cli/src/main.lisp`
- **現状**: 通常の4KBページでmalloc/mmap。TLBスラッシングの対策なし
- **内容**: **madvise MADV_HUGEPAGE** をヒープ領域に設定（Linux THP）。**mmap(MAP_HUGETLB)** で2MBページを直接割り当て（`--huge-pages`フラグ）。macOS `VM_FLAGS_SUPERPAGE_SIZE_2MB` 対応。Javaヒープのような大規模ヒープでのTLBミス削減（通常4KB→2MB: TLBエントリ512倍の効率）
- **根拠**: ヒープが数百MB以上になると4KBページのTLBカバレッジが不足。HotSpot `-XX:+UseLargePages` / jemalloc huge page arena相当
- **難易度**: Easy

---

### Phase 102 — 言語機能・型システム拡張 II

#### FR-528: First-Class Continuations / call/cc (一級継続)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: CPS変換（`cps.lisp`）は内部的にcontinuationを使用するが、ユーザーに公開する`call/cc`/`call-with-current-continuation`なし
- **内容**: `(call-with-current-continuation #'(lambda (k) ...))` API。CPS変換済みのコードではkが直接継続クロージャ。`(funcall k value)` でキャプチャした継続に戻る。**脱出継続**（escape continuation）を効率的に実装（フルキャプチャより高速）。コルーチン（FR-386）・非局所脱出・バックトラッキングの統一的実装基盤
- **根拠**: SchemeのR7RS必須機能。cl-ccのCPS変換インフラは継続が「ただのクロージャ」として直接実装できる理想的構造
- **難易度**: Medium

#### FR-529: Delimited Continuations / shift-reset (限定継続)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/vm/src/vm.lisp`, FR-528前提
- **現状**: `call/cc`（FR-528）は現在の継続全体をキャプチャ。部分的なキャプチャなし
- **内容**: `(reset ...)` で継続のスコープを区切り、`(shift k ...)` でその範囲の継続をキャプチャ。**動的効果のエンコード**: 例外・状態・非決定性・非同期I/OをDelimited Continuationで統一表現。OCaml 5.0 effects / Racket `call-with-continuation-barrier` との相互運用。FR-343（エフェクトシステム）のランタイム実装基盤
- **根拠**: Full continuationより制限されるが型付けが容易。OCaml 5.0が限定継続をネイティブ採用（2022〜）、2026年時点で主流化
- **難易度**: Hard

#### FR-530: Pattern Matching with Exhaustiveness (網羅性検査付きパターンマッチング)

- **対象**: `packages/frontend/expand/src/macros-basic.lisp`, `packages/type/type/src/inference.lisp`
- **現状**: `cond`/`typecase`/`case`は網羅性検査なし。未ハンドルパターンが実行時エラー
- **内容**: `(cl-cc:match x ((list a b) ...) ((cons h t) ...) (:else ...))` マクロ。**網羅性チェック**: 型情報からすべてのケースがカバーされているかをコンパイル時検証。**到達不能ケース検出**: 先行パターンが後続を包含する場合に警告。**Decision tree compilation**: ネストdecision treeに最適変換（ジャンプテーブル / switch lower）。Rust `match` / OCaml `match` / Haskell case exhaustiveness と同等
- **根拠**: Lispのmatchは多数の実装（trivia / optima / etc.）があるがコンパイラ組み込みがない。網羅性はコンパイル時検証が安全の要
- **難易度**: Hard

#### FR-531: Refinement Types (精緻型)

- **対象**: `packages/type/type/src/inference.lisp`, `packages/type/type/src/types.lisp`
- **現状**: 型は集合論的基底型のみ（fixnum/string等）。述語付き部分型なし
- **内容**: `(cl-cc:define-refinement-type positive-integer fixnum (> x 0))` で述語付き型定義。`(declare (type positive-integer n))` で使用。VRP（FR-390）・SMTソルバ（FR-392）との連携で精緻型の充足を検証。`(defun sqrt (x positive-integer) ...)` の戻り値型推論。LiquidHaskell / F* / Stainless（Scala）相当
- **根拠**: 配列インデックス型 `(and fixnum (>= 0) (< array-length))` で境界チェックを型レベルで除去できる。bcE（FR-468）の完全静的版
- **難易度**: Very Hard

#### FR-532: Linear Types / Resource Types (線形型・リソース型)

- **対象**: `packages/type/type/src/inference.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: ファイルハンドル・ネットワーク接続等のリソースの二重解放・未解放をコンパイル時に検出不可
- **内容**: `(cl-cc:define-linear-type file-handle)` で「正確に1回使用」が型レベルで保証されるリソース型。`(cl-cc:consume handle)` で消費をマーク。**Drop checker**: スコープ終了時に未消費の線形値があればコンパイルエラー。`(cl-cc:move val)` で所有権移転。Rust `ownership` / Linear Haskell / Clean uniqueness typingの軽量版
- **根拠**: CL条件システムでのリソースリーク（ファイル未close、ロック未解放）をコンパイル時に根絶。`with-open-file`マクロのより強力な代替
- **難易度**: Very Hard

#### FR-533: Compile-Time Format String Verification (コンパイル時フォーマット文字列検証)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `(format t "~A ~D" x)` の引数数・型の正当性はコンパイル時未検証
- **内容**: `(format destination control-string args...)` のcontrol-stringがリテラルの場合、コンパイル時に解析。`~A`/`~D`/`~S`等の指示子数と実引数数の一致検査。`~*`（引数スキップ）・`~@`（引数変更）も解析。型不一致（`~D`に文字列等）を警告。GCC `__attribute__((format(printf,...)))` / Rust `format!` マクロの静的検証相当
- **根拠**: ANSI CL `format`は27種類以上の指示子を持つ複雑なDSL。引数ミスによるランタイムエラーが頻出
- **難易度**: Medium

#### FR-534: Compile-Time Regex Compilation (コンパイル時正規表現コンパイル)

- **対象**: 新規`packages/engine/compile/src/regex.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: 正規表現マッチングは実行時のNFA/DFA構築
- **内容**: `(cl-cc:regex "pattern" string)` でパターンがリテラルの場合、コンパイル時にNFA→DFA変換→VM命令に直接コンパイル。`(cl-cc:regex-bind (m1 m2) "(\w+)\s+(\d+)" string) ...` で型付きキャプチャグループ。DFAの状態遷移表をコンパイル済みコード内にインライン展開。Rust `lazy_regex` / PERL compile-time regex相当
- **根拠**: 起動時DFA構築のオーバーヘッドゼロ。コンパイル時に正規表現の構文エラーを検出
- **難易度**: Hard

#### FR-535: Type Narrowing via Assert (アサートによる型絞り込み)

- **対象**: `packages/type/type/src/inference.lisp`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: `(assert (typep x 'fixnum))` の後でもxの型が絞り込まれない
- **内容**: `(assert (typep x 'fixnum))` → アサート後のパスでxを`fixnum`型として扱う（FR-428 Occurrence typingの拡張）。`(check-type x fixnum)` も同様。`(assert (> n 0))` → VRP（FR-390）にn∈[1,∞)を伝達。`(assert (not (null x)))` → xからnilを除く。ユーザー定義アサートへの適用は`(declare (cl-cc:asserts (> x 0)))` 形式
- **根拠**: CLの`assert`/`check-type`は実行時検証専用。型絞り込みへの接続でdevirt・unboxingの追加機会を得る
- **難易度**: Medium

---

### Phase 103 — ABI・エコシステム・開発者ヒント

#### FR-538: ABI Stability & Symbol Versioning (ABI安定性・シンボルバージョニング)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/backend/binary/src/elf.lisp`
- **現状**: コンパイル済みライブラリのABIバージョン管理なし。マイナーバージョンアップでABI破壊の危険
- **内容**: ELF **GNU Symbol Versioning**（`.gnu.version`/`.gnu.version_d`セクション）：`cl_eval@@CL_CC_2.0` 形式の版付きシンボル。旧バージョンのシンボルを互換エントリとして保持。Mach-O `install_name`/`current_version`/`compatibility_version` フィールド設定。`./cl-cc compile --abi-version=2.0 --compat-version=1.0`
- **根拠**: GCC `__attribute__((symver))` / ELF symbol versioning。共有ライブラリのメジャーバージョン非互換変更を安全に管理
- **難易度**: Medium

#### FR-539: Deprecation API (廃止予定API管理)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/frontend/parse/src/diagnostics.lisp`
- **現状**: 廃止予定関数を示す仕組みなし。`(declare (ignore x))`程度の宣言のみ
- **内容**: `(cl-cc:deprecate foo :since "2.0" :removed-in "3.0" :replacement #'bar :message "use bar instead")` 宣言。廃止予定関数の呼び出しサイトでコンパイル警告（FR-318連携）。`--Wdeprecated`で有効化。移行ガイドURL添付。**段階的廃止**: `--Wno-deprecated-v1`で旧バージョン非推奨のみ抑制
- **根拠**: SBCL `(deprecated :early "1.2.3")` / GCC `__attribute__((deprecated))`. APIの進化と後方互換性の管理に必須
- **難易度**: Easy

#### FR-540: Branch Probability Hints / likely-unlikely (分岐確率ヒント)

- **対象**: `packages/frontend/expand/src/macros-basic.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 分岐確率情報なし。コンパイラは全分岐を50/50と仮定
- **内容**: `(cl-cc:likely condition)` / `(cl-cc:unlikely condition)` マクロ。展開後の条件分岐に**静的分岐予測ヒント**付与（x86-64: `PT`/`PN` prefix、AArch64: future HINT encoding）。コード配置に反映: likely側をフォールスルー、unlikely側をジャンプターゲットに。PGOデータ（FR-480）がある場合はPGOを優先
- **根拠**: GCC `__builtin_expect` / Clang `[[likely]]`（C++20）/ Linux `likely()/unlikely()`。エラーハンドラが`unlikely`なら最適なコード配置が可能
- **難易度**: Easy

#### FR-541: Inline Assembly (インラインアセンブリ)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: ネイティブアセンブリ命令に直接アクセスする手段なし
- **内容**: `(cl-cc:asm "cpuid" :inputs (:eax n) :outputs (:eax out :ebx brand) :clobbers (:rcx :rdx))` フォーム。GCC `asm volatile` スタイルの制約記述（`=r`, `r`, `m` 制約）。**型安全**: 入出力変数はCLの型でラップ。JIT（FR-330）コードにも適用可能。x86-64とAArch64の両方に対応
- **根拠**: `rdtsc`（高精度タイマー）・`cpuid`（CPU情報）・`pause`（spin-wait最適化）等のコンパイラが直接生成しない命令へのアクセスに不可欠
- **難易度**: Hard

#### FR-542: Hot/Cold Code Annotation (ホット/コールドコードアノテーション)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: PGO（FR-480/FR-508）なしではコードの温度推定不可
- **内容**: `(cl-cc:declare-hot)` / `(cl-cc:declare-cold)` 関数宣言アノテーション。`(cl-cc:cold-path ...)` ブロックアノテーション（エラーハンドラ等に使用）。ホット関数を`.text.hot`に集約（I-キャッシュ局所性）。コールド関数を`.text.cold`（FR-471 Outliningとの統合）。`__attribute__((hot))`/`__attribute__((cold))` GCC相当
- **根拠**: PGOデータがない場合の手動最適化ガイド。エラーパスは確実にコールドであり手動指定が有効
- **難易度**: Easy

#### FR-543: Pointer Compression (ポインタ圧縮)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: ポインタは64ビット整数。ヒープが4GB以下でも8バイト/ポインタ
- **内容**: **CompressedOops（Java HotSpot方式）**: ヒープベースアドレスを別途保持し、ヒープ内ポインタを32ビットオフセットとして格納（4GB未満のヒープ）。`(cl-cc:compress-heap :limit 4gb)` 起動オプション。CLOSスロット・Consセル・ベクタ要素の圧縮。ポインタ解決は `base + offset * alignment` の1命令。V8 CompressedPointers / JVM CompressedOops実績あり
- **根拠**: 64ビットシステムでもヒープを4GB以下に収めることが多い。ポインタサイズ50%削減でキャッシュ密度向上、GCスキャン速度2x向上
- **難易度**: Hard

#### FR-544: Build Event Protocol / BEP (ビルドイベントプロトコル)

- **対象**: `cli/src/main.lisp`, `pipeline/src/pipeline.lisp`
- **現状**: ビルド進捗はstdoutへのフリーテキスト出力のみ。IDE/CI連携の構造化なし
- **内容**: **Bazel Build Event Protocol**互換のJSON/Protobuf構造化イベントストリーム: `BuildStarted`/`SourceFileAdded`/`CompilationStarted`/`CompilationFinished`/`TestResult`/`BuildFinished`。`--build-event-json-file=events.json`でファイル出力。`--build-event-publish-all-actions`でCI連携。VS Code `tasks.json`のproblemMatcherと統合可能
- **根拠**: Bazel BEP / Buck2 build events。IDE・CI・ダッシュボードとの統合標準プロトコル。2026年でBazel BEPがデファクト化
- **難易度**: Medium

#### FR-545: Documentation String Preservation (ドキュメント文字列保存)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/binary/src/`
- **現状**: `defun`本体のdocstringはコンパイル時に捨てられる可能性
- **内容**: docstringをコンパイル済みVMプログラムに保存（`vm-docstring`メタデータ命令）。`(documentation #'foo 'function)` が実行時にdocstringを返す。FASL（FR-499 PCH）にdocstring含む。`--strip-docstrings`で明示的に除去（プロダクション最適化）。SBCL `sb-ext:*restrict-compiler-policy*` 類似の制御
- **根拠**: CLの`documentation`はANSI仕様。セルフホスティングコンパイラ自身のdocstringがREPLで参照可能になる
- **難易度**: Easy

#### FR-546: Register Pressure Reduction (レジスタプレッシャー削減)

- **対象**: `packages/engine/compile/src/regalloc.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: レジスタ割り当てはスピルコスト最小化のみ。レジスタプレッシャーを意識した命令スケジューリングなし
- **内容**: **Belady's algorithm（MIN）**: 最遠将来使用点を基準としたスピル選択（最適なオフラインアルゴリズム）。**Pressure-aware scheduling**: 命令スケジューリング（FR-408）にレジスタプレッシャーを副目的関数として追加（ILP最小化とのバランス）。**Live range splitting**: ロングライブレンジを短く分割して干渉グラフのエッジ数削減
- **根拠**: 多くのスピルの根本原因は不適切な命令順序。プレッシャー考慮スケジューリングでスピル数を30〜50%削減できる
- **難易度**: Hard

---

