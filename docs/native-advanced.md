# Native Backend: Advanced Optimization & Platform

LTO, advanced optimization passes, staged compilation, security hardening, GC integration, debug info, zero-cost exceptions, modern ISA, ML-driven optimization, WASM edge runtime, value range analysis, threading, developer experience, binary hardening, post-link optimization, functional language optimization, standard optimization passes, target/platform completion, profiling, modern IR, code quality, GC lifecycle, advanced concurrency, formal verification, tooling/IDE, security completion.

---

### Phase 90 — リンク時最適化 (LTO)

#### FR-500: LTO (Link-Time Optimization) — モジュール間最適化基盤

- **対象**: `pipeline/src/pipeline.lisp`, `packages/backend/binary/src/macho.lisp`, `packages/backend/binary/src/elf.lisp`
- **内容**:
  - コンパイル済みモジュールを独立したIR（VMまたはMIR）形式でバイナリに埋め込み
  - リンク段階で全モジュールのIRを統合し、クロスモジュールインライン化・定数伝播・デッドコード除去を実行
  - `--lto` フラグで有効化、IR埋め込み用 `__bitcode` / `.llvm_bitcode` セクション相当を生成
- **根拠**: LLVM LTO / GCC `-flto`。単一ファイルでは見えない関数が呼び出し側から特化できる
- **難易度**: Very Hard

#### FR-501: ThinLTO — スケーラブルな並列LTO

- **対象**: `pipeline/src/pipeline.lisp`
- **内容**:
  - モジュールごとに軽量サマリ（関数シグネチャ・インライン候補・型情報）を生成
  - リンク時はサマリのみ読み込み、必要な関数のIRのみ選択的にインポート
  - 各モジュールを独立に並列最適化（LTOと異なり全IR統合不要）
- **根拠**: LLVM ThinLTO。コンパイル時間O(N²)→O(N)に削減しながらLTOの大部分の恩恵を得る
- **難易度**: Very Hard

#### FR-502: Interprocedural Constant Propagation (IPCP)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - 呼び出しサイトで定数引数が渡される関数を検出し、その引数を定数として特化版を生成
  - 特化版にはIPCPサフィックスを付与し、元関数は汎用版として残す
  - `(defun foo (x) (* x 2))` を `(foo 42)` で呼ぶ場合 → `(foo-ipcp-42 () 84)` に特化
- **根拠**: LLVM IPSCCP / GCC ipa-cp。ホット関数の定数伝播を関数境界越えに拡張
- **難易度**: Hard

#### FR-503: Whole-Program Devirtualization (仮想関数の完全脱仮想化)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - プログラム全体のCLOS総称関数呼び出しを解析し、メソッド解決が一意に決まる呼び出しサイトを直接呼び出しに変換
  - 型セット解析（CHA: Class Hierarchy Analysis）で `vm-dispatch-generic-call` を直接 `vm-call` に置換
  - LTOと連携して単一ファイルでは見えないサブクラスも考慮
- **根拠**: LLVM DevirtVCalls / HotSpot CHA。CLOS dispatch の最大ボトルネックを除去
- **難易度**: Hard

---

### Phase 91 — 高度な最適化パス

#### ✅ FR-510: SCCP (Sparse Conditional Constant Propagation)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - CFGの実行可能性（reachability）を考慮した定数伝播
  - 現状の `opt-pass-constant-fold` はデータフローのみ; SCCPは「この分岐は絶対に実行されない」という情報を伝播
  - 例: `(if (= x 0) (foo x) (bar x))` で then節の `x` が `0` と確定し `(foo 0)` に特化
  - Lattice値 (undef / constant / overdefined) で管理、worklist駆動の反復解析
- **根拠**: Wegman & Zadeck (1991) SCCP。基本的な定数畳み込みより格段に精密
- **難易度**: Hard

#### ✅ FR-511: GVN (Global Value Numbering)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 現状の CSE は局所的 (ハッシュテーブル、ラベルでリセット)。GVNはSSA形式を利用してCFG全体で同値式を検出
  - Dominator tree上でvalue numberを伝播し、支配ブロックで計算済みの式を再利用
  - PRE (Partial Redundancy Elimination) と組み合わせて部分的に冗長な式も除去
- **根拠**: Click & Cooper GVN / LLVM NewGVN。LLVMの中核最適化パスの一つ
- **難易度**: Hard

#### FR-512: Alias Analysis (エイリアス解析)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - TBAA (Type-Based Alias Analysis): CL型情報を利用してメモリ読み書きの別名関係を判定
  - ポインタ解析 (Andersen / Steensgaard): ヒープオブジェクト間のポイント先関係を静的解析
  - エイリアス情報を活用したロード-ストア転送、ストア-ロード除去、ループ不変式移動の精度向上
  - `vm-slot-ref` / `vm-slot-set` に型タグを付与して alias class を区別
- **根拠**: LLVM AliasAnalysis infrastructure。エイリアスが不明だと多くの最適化が保守的になる
- **難易度**: Very Hard

#### FR-513: Polyhedral Optimization (多面体最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/optimize/src/cfg.lisp`
- **内容**:
  - アフィンループ（`dotimes`, `loop :for i :from 0 :below n`）をPolyhedral Model（整数線形計画）で表現
  - ループタイリング（キャッシュ局所性）、ループ交換（ベクトル化準備）、ループ融合/分割をPolyhedronの変換として統一的に扱う
  - isl (Integer Set Library) 相当の整数多面体演算を CL で実装、または FFI で isl を呼び出す
- **根拠**: LLVM Polly / GCC Graphite。数値・科学計算ループで数倍の高速化
- **難易度**: Very Hard

#### FR-514: Loop Fusion / Loop Fission (ループ融合・分割)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - **融合**: 同じ反復空間の隣接ループを一つに統合。ループオーバーヘッド削減 + D-cache 局所性向上
  - **分割**: データ依存のため融合できないループを逆に分割し、各パートでベクトル化を可能にする
  - 依存性チェック（GCD test, Banerjee test）で変換の合法性を確認
- **根拠**: GCC `-floop-nest-optimize`. FR-513の多面体最適化の部分実装として単独でも有用
- **難易度**: Hard

#### FR-515: Loop Tiling / Blocking (ループタイリング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 二重以上のループを L1/L2 キャッシュサイズに合わせたタイルに分割
  - 行列積・畳み込みなどのO(N³)ループで L1キャッシュ内に作業セットを収める
  - タイルサイズをコンパイル時の `(declare (optimize (speed 3)))` 時に自動選択
- **根拠**: GCC `-floop-block` / LLVM LoopTiling。行列演算で2-10x高速化
- **難易度**: Hard

#### FR-516: Escape Analysis (エスケープ解析)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - ヒープ割り当てされるオブジェクト（cons, list, closure 環境）が関数スコープ外に脱出しないか解析
  - 脱出しないオブジェクトはスタック上に割り当て（stack allocation）、GC 圧力を除去
  - CL の `let` + `cons` / `list` の短命オブジェクトが主な対象
  - 接続解析 (connectivity analysis) でクロージャキャプチャによる脱出も追跡
- **根拠**: Java HotSpot scalar replacement / Go escape analysis。GCポーズ削減の最重要手法
- **難易度**: Hard

#### FR-517: Memory SSA (メモリSSA)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/foundation/mir/src/mir.lisp`
- **内容**:
  - レジスタのSSAに加え、メモリ状態もSSAで表現（MemoryDef / MemoryUse / MemoryPhi）
  - ロード-ストア最適化（LSM: Load Store Motion）、デッドストア除去、ロード転送の精度をCFG全体に拡大
  - MIR層（`mir.lisp`）のSSA基盤と統合
- **根拠**: LLVM MemorySSA。エイリアス解析と組み合わせてメモリ最適化の精度を大幅向上
- **難易度**: Very Hard

---

### Phase 92 — 段階的コンパイル・実行時最適化

#### FR-520: Tiered Compilation (段階的コンパイル)

- **対象**: `packages/engine/vm/src/vm.lisp`, `pipeline/src/pipeline.lisp`, `cli/src/main.lisp`
- **内容**:
  - **Tier 0**: VM インタープリタで即実行（起動時間ゼロ）
  - **Tier 1**: 呼び出し回数カウンタが閾値（例: 100回）を超えたらベースラインJIT（最適化なし、高速コンパイル）
  - **Tier 2**: ホット関数（1000回+）を最適化コンパイル（インライン化・定数畳み込み・レジスタ割り当て）
  - 各 `vm-func-ref` に呼び出しカウンタスロットを追加、閾値チェックは `vm-call` で実施
- **根拠**: V8 (Ignition → Maglev → Turbofan) / HotSpot (C1 → C2)。起動時間と最大性能を両立
- **難易度**: Very Hard

#### FR-521: On-Stack Replacement (OSR) — スタック上置換

- **対象**: `packages/engine/vm/src/vm.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - ループ実行中にそのループがホットと判定された場合、現在実行中のインタープリタフレームをJITコンパイル済みフレームに動的置換
  - OSR エントリポイントを各ループヘッダに生成し、インタープリタの仮想レジスタを物理レジスタにマッピング
  - 「コンパイルが間に合わなかった長いループ」でのインタープリタ実行を回避
- **根拠**: HotSpot OSR / V8 OSR。長寿命ループが多い CL プログラムで重要
- **難易度**: Very Hard

#### FR-522: Deoptimization (脱最適化)

- **対象**: `packages/engine/vm/src/vm.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - 型推測に基づく最適化（例: 整数加算を `FIXNUM` と仮定してオーバーフローチェックを省略）が失敗した場合、インタープリタフレームへ安全に復帰
  - 各 deopt ポイントでの仮想レジスタマッピング（deopt マップ）をコンパイル時に生成
  - インライン化された関数のアンインライン化（インラインスタックの再構築）
- **根拠**: V8 Deoptimize / Graal DeoptimizeNode。型特化JITの安全性を保証
- **難易度**: Very Hard

#### FR-523: Speculative Inlining with Inline Cache (ICベース推測インライン化)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - IC（Inline Cache, FR-009）の観測型頻度から最頻メソッドを推測し、ガード付きでインライン展開
  - `(if (typep obj 'Foo) <inlined-method> <slow-path>)` パターンを生成
  - ガード失敗時は deopt（FR-522）またはスロウパスへフォールバック
- **根拠**: V8 Turbofan speculative inlining / HotSpot speculative devirtualization。CLOSの動的ディスパッチを事実上の直接呼び出しに変換
- **難易度**: Hard

#### FR-524: Value Profiling (値プロファイリング)

- **対象**: `packages/engine/vm/src/vm.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - 実行時に特定命令のオペランド値（整数定数、型タグ）の分布を記録
  - `vm-generic-call` の receiver 型頻度（ICと兼用）、整数値の頻度（IPCP特化のヒント）
  - プロファイルデータをTier2コンパイル時に活用（FR-058 Type Feedback PGOと統合）
- **根拠**: HotSpot value profiling / HHVM type profiling
- **難易度**: Medium

#### FR-525: AutoFDO / Sample-Based PGO (サンプリングベースPGO)

- **対象**: `pipeline/src/pipeline.lisp`
- **内容**:
  - `perf record` / `dtrace` 等のハードウェアサンプラーで収集したプロファイルを CL ソース行にマッピング
  - 特定のコンパイル時フラグなしに実運用バイナリからPGOデータを収集可能
  - AutoFDO形式（`.afdo`）または Linux perf データを入力としてインライン・分岐ヒントに変換
- **根拠**: Google AutoFDO / LLVM SamplePGO。計測用バイナリが不要で本番ワークロードのプロファイルを使える
- **難易度**: Hard

---

### Phase 93 — セキュリティ・ハードニング

#### FR-530: Control Flow Integrity (CFI) — 制御フロー完全性

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - **Forward-edge CFI**: 間接呼び出し（`call [reg]`）の前に呼び出し先が合法なエントリポイントかを検証
    - x86-64: `ENDBR64` 命令を全関数エントリに挿入 (CET: Control-flow Enforcement Technology)
    - AArch64: BTI (Branch Target Identification) で `BTI c` / `BTI j` を挿入
  - **Backward-edge CFI**: リターンアドレス保護
    - x86-64: Intel CET Shadow Stack (`SAVEPREVSSP`/`RSTORSSP`)
    - AArch64: GCS (Guarded Control Stack、ARMv9.4-A)
- **根拠**: clang `-fsanitize=cfi` / GCC CET support。ROP/JOPチェーン攻撃を防止
- **難易度**: Medium

#### FR-531: Pointer Authentication (PAC) — AArch64ポインタ認証

- **対象**: `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - ARMv8.3-A PAC命令でリターンアドレスと関数ポインタに署名
  - プロローグ: `PACIASP`（IA鍵でSP混合署名）、エピローグ: `AUTIASP`（検証）
  - クロージャポインタ、`vm-func-ref` の関数アドレスにも `PACIA`/`AUTIA` を適用
  - Apple M-series は PAC をハードウェア強制（`PAC_TRAP` on failure）
- **根拠**: ARMv8.3-A Architecture Reference Manual。Apple Silicon の標準セキュリティ機能
- **難易度**: Medium

#### FR-532: Stack Canary / Stack Protection (スタックカナリア)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - 関数プロローグでカナリア値（`FS:[0x28]` / TLS経由）をスタックフレームに書き込み
  - エピローグでカナリア値を検証し、改ざんがあれば `__stack_chk_fail` を呼び出す
  - バッファを含む関数（`alloca`相当を使う関数）を優先的に保護
- **根拠**: GCC `-fstack-protector-strong` / clang SSP。スタックバッファオーバーフローの検出
- **難易度**: Easy

#### FR-533: Stack Clash Protection (スタッククラッシュ保護)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - 大きなスタックフレーム確保時（4KB超）に1ページごとにプローブを実行
  - `SUB RSP, large_value` の代わりに複数の `SUB RSP, 4096` + `MOV [RSP], 0` のシーケンスに展開
  - ガードページを飛び越えるスタック成長を防止
- **根拠**: GCC `-fstack-clash-protection`。Qualys 発見の Stack Clash 脆弱性クラスを緩和
- **難易度**: Easy

#### FR-534: Speculative Execution Mitigation (投機実行緩和)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - Spectre Variant 1 (bounds check bypass) 緩和: 配列アクセス前に `LFENCE` を挿入
  - SLH (Speculative Load Hardening): 分岐条件を投機状態マスクとして伝播し、投機パスでのロード値をマスク
  - JIT コンパイルされたコードへの Spectre v2 (indirect branch) 対策: `RETPOLINE` テンプレート適用
- **根拠**: Spectre/Meltdown 論文 (2018) 以降、全主要コンパイラが実装済み
- **難易度**: Hard

---

### Phase 94 — GC統合・ランタイムインターフェース

#### FR-540: Safepoint Polling (セーフポイントポーリング)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**:
  - GC起動・スレッド停止のためのセーフポイントポーリング命令をJIT出力に挿入
  - ループバックエッジと関数エントリで専用ポーリングページの読み取り（ポーリングページを `mprotect` で書き込み不可にするとSIGSEGVでGCトリガー）
  - ポーリングページ方式はTLSポーリングより分岐オーバーヘッドが小さい
- **根拠**: HotSpot safepoint polling / GraalVM safepoints。GCが全スレッドを安全停止させる前提条件
- **難易度**: Hard

#### FR-541: Stack Maps for Precise GC (精密GCのためのスタックマップ)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**:
  - 各セーフポイントで「どの物理レジスタとスタックスロットがGCルートか」を記述するスタックマップを生成
  - スタックマップをコード末尾または専用セクション（LLVM `__llvm_stackmaps`相当）に埋め込み
  - GCが正確にルートを追跡し、保守的GCの誤検知（偽ポインタ）を除去
- **根拠**: LLVM Statepoints / GraalVM reference maps。保守的GCから精密GCへの移行の前提条件
- **難易度**: Very Hard

#### FR-542: Write Barrier Optimization (書き込みバリア最適化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**:
  - 現状の `vm-slot-set` が毎回フルバリアを実行するか未実装。世代別GCのカードテーブルバリア（FR-541のスタックマップと連携）
  - 同一オブジェクトへの連続書き込みでバリアを1回に統合（barrier merging）
  - ローカル変数（スタック上オブジェクト, FR-516 escape analysis の結果）へのストアはバリア省略
  - Shenandoah/ZGC スタイルのロードバリア（concurrent GC 向け）を将来拡張として設計
- **根拠**: GC write barrier elimination in HotSpot C2。書き込みバリアが全ストアの5-20%のコストになることがある
- **難易度**: Hard

#### FR-543: GC-Aware Inlining (GC認識インライン化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - インライン化でセーフポイントが消えないように、インライン後もセーフポイントカウントを保持
  - セーフポイントがゼロになるインライン化は抑制（GCが止まれない無限ループが生まれる）
  - `vm-alloc` 命令を含む関数はセーフポイントを持つと保守的に扱う
- **根拠**: HotSpot JIT policy。完全にセーフポイントのないJITコードはGCに対して危険
- **難易度**: Medium

---

### Phase 95 — デバッグ情報・観測可能性

#### FR-550: DWARF 5 Debug Information Generation (DWARF5デバッグ情報生成)

- **対象**: `packages/backend/binary/src/elf.lisp`, `packages/backend/binary/src/macho.lisp`
- **内容**:
  - `.debug_info` (DIE: Debug Information Entry): 関数・変数・型の階層的記述
  - `.debug_line` (行テーブル): マシン命令アドレス → ソース行マッピング。最適化コードの複数アドレス→1行マッピングも表現
  - `.debug_abbrev` (省略テーブル): DIEエンコーディング圧縮
  - `.debug_str` / `.debug_str_offsets`: 文字列テーブル（DWARF5形式）
  - `DW_AT_language` に `DW_LANG_Common_Lisp`（0x001c）を設定
- **根拠**: DWARF 5 specification (2017)。`gdb`, `lldb`, `valgrind` での使用に必須
- **難易度**: Hard

#### FR-551: Source Map Generation for Wasm (Wasm用ソースマップ生成)

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **内容**:
  - Wasm バイナリの各命令オフセット → CL ソースファイル行・列のマッピングを生成
  - `.wasm` + `.map` ファイルペアまたは `name` セクションに埋め込み
  - ブラウザデベロッパーツールでの CL ソースレベルデバッグを可能にする
- **根拠**: Source Map Specification (V3). Chrome/Firefox DevTools の標準機能
- **難易度**: Medium

#### FR-552: Variable Location Tracking (変数位置追跡)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/regalloc.lisp`
- **内容**:
  - レジスタ割り当て後の各変数の物理位置（レジスタ番号またはスタックオフセット）の変化を追跡
  - `DW_OP_reg0`〜`DW_OP_reg31` / `DW_OP_breg` / `DW_OP_fbreg` を使った DWARF ロケーション式生成
  - 最適化後も変数が「存在する範囲」(`DW_AT_location` の `loclist`) を正確に記述
- **根拠**: GCC `-g3` / clang Debug Info. デバッガでの変数ウォッチに必要
- **難易度**: Hard

#### FR-553: PerfMap / JIT Map File Output (perf 用マップファイル出力)

- **対象**: `pipeline/src/pipeline.lisp`
- **内容**:
  - `/tmp/perf-<pid>.map` 形式でJITコンパイルされた関数のアドレス範囲とシンボル名を出力
  - `perf report` / `perf annotate` がCL関数名でホットスポットを表示可能に
  - macOS: `dtrace` の `pid` プロバイダ用にも対応
- **根拠**: V8 `--perf-prof` / JVM `-XX:+PrintPerfMap`。JIT コードのプロファイリングに必須
- **難易度**: Easy

#### FR-554: Sanitizer Instrumentation Integration (サニタイザ計装統合)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - **ASan (AddressSanitizer)**: ヒープ・スタックの境界外アクセス検出のためにメモリアクセスにシャドウチェックを挿入
  - **UBSan (UndefinedBehaviorSanitizer)**: 整数オーバーフロー・型ミスマッチ検出のためにガードを挿入
  - `(declare (optimize (safety 3)))` 時に自動有効化
  - `--sanitize=address,undefined` コンパイルフラグで制御
- **根拠**: clang/GCC sanitizers。デバッグ用バイナリの問題検出を大幅強化
- **難易度**: Hard

---

### Phase 96 — ゼロコスト例外処理

#### FR-560: Table-Based Zero-Cost Exception Handling (テーブルベースゼロコスト例外処理)

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/binary/src/elf.lisp`
- **内容**:
  - 現状: VM レベルでの `handler-case`/`restart-case` は `CL:HANDLER-CASE` マクロ展開に依存（ホスト CL のスタックアンワインド）
  - ネイティブバックエンド向けに DWARF `.eh_frame` / Mach-O `__unwind_info` セクションを生成
  - ハッピーパス（例外なし）のコストゼロを保証: `try` ブロック内での追加命令なし
  - アンワインドテーブルに各コールサイトの `landing pad` アドレスとフレーム復元情報を記述
  - `_Unwind_RaiseException` (libunwind) をランタイム依存として使用
- **根拠**: C++ zero-cost EH (Itanium ABI) / Rust panic unwinding。`setjmp`/`longjmp` ベースと比べて正常パスでゼロオーバーヘッド
- **難易度**: Very Hard

#### FR-561: SJLJ vs Table-Based Exception Selection (SJLJ vs テーブルEH選択)

- **対象**: `packages/engine/vm/src/conditions.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - 現状: `handler-case` の低コスト実装として `setjmp`/`longjmp` を選択可能
  - `--eh-model=sjlj` (デバッグ・低頻度例外向け) と `--eh-model=table` (ホットパス向け) の選択機構
  - SJLJ は `setjmp` のセットアップコストがあるが表が不要; TABLE は逆
  - Wasm 向けは `WebAssembly Exception Handling` proposal の `try`/`catch`/`throw` を使用
- **根拠**: ARM embedded SJLJ, LLVM EH model selection
- **難易度**: Medium

#### FR-562: Landing Pad Generation (ランディングパッド生成)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - 各 `handler-case` / `restart-case` に対してランディングパッドコードを生成
  - `_Unwind_GetException()` でコンディションオブジェクトを取得し、型チェック後に対応ハンドラへ分岐
  - `personality function` (`__gxx_personality_v0` 相当の CL 版) を実装してアンワインダに登録
- **根拠**: Itanium C++ ABI Exception Handling。テーブルベースEH（FR-560）の実行時コンポーネント
- **難易度**: Very Hard

---

### Phase 97 — 最新ISA対応 (2024-2026)

#### FR-570: AVX-512 / AVX-10 Support (x86-64 512-bitベクトル)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/regalloc.lisp`
- **内容**:
  - ZMM0-ZMM31 (512-bit) レジスタとマスクレジスタ K0-K7 の管理
  - `EVEX` プレフィックスエンコーディング（4バイト）の実装
  - AVX-512F 基本命令 (`VPADDD zmm`, `VMOVDQU64 zmm` 等) + AVX-512BW (バイト/ワード) + AVX-512VL (128/256-bit subvector)
  - AVX-10.2 (Intel Granite Rapids, 2024): 完全な512-bit幅を全コアで保証、FrequencyThrottling問題を解決
  - CPUIDでアーキテクチャ検出し、AVX-10.2 > AVX-512 > AVX2 の優先順でコード選択
- **根拠**: Intel AVX-10 Architecture Specification (2023). HPC・AI推論ワークロードで8倍のスループット
- **難易度**: Hard

#### FR-571: Intel AMX (Advanced Matrix Extensions) — 行列演算アクセラレータ

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - AMX タイルレジスタ (TMM0-TMM7, 最大8KB) の管理
  - `TILELOADD` / `TILESTORED` でタイルロード/ストア
  - `TDPBSSD` (整数行列積) / `TDPFP16PS` (FP16行列積, Sapphire Rapids+) の生成
  - `XSAVE`/`XRSTOR` によるタイルレジスタの保存・復元（コンテキストスイッチ対応）
  - `ARCH_REQ_XCOMP_PERM` syscall でタイルのカーネル許可を取得
- **根拠**: Intel AMX Architecture Specification (Sapphire Rapids, 2023). 深層学習推論・行列演算で10-100x高速化
- **難易度**: Very Hard

#### FR-572: ARM SVE / SVE2 (Scalable Vector Extension)

- **対象**: `packages/backend/emit/src/aarch64-codegen.lisp`, `packages/backend/emit/src/regalloc.lisp`
- **内容**:
  - Z0-Z31 スケーラブルベクトルレジスタ (128〜2048-bit、実行時に `RDVL` で長さ取得)
  - P0-P15 プレディケートレジスタによるマスク付き命令
  - SVE 基本命令 (`ADD Zd.S, Zn.S, Zm.S`, `LD1D`, `ST1W` 等)
  - SVE2 拡張 (`MATCH`, `HISTCNT`, `PMULLB` 等, ARMv9-A 全プロセッサに義務化)
  - ベクトル長非依存コード (VLA: Vector Length Agnostic) を生成し、Neoverse N2/V2/Apple M4 等で自動的に最適幅で動作
- **根拠**: ARM Architecture Reference Manual SVE2 (ARMv9-A). AWS Graviton 3/4, Apple M4 で利用可能
- **難易度**: Hard

#### FR-573: RISC-V Vector Extension (RVV 1.0)

- **対象**: `packages/backend/emit/src/riscv64-codegen.lisp` (FR-296と連携)
- **内容**:
  - V0-V31 ベクトルレジスタ (最小128-bit, `vlenb` CSR で実行時取得)
  - `VSETVLI` / `VSETIVLI` でベクトル長と要素幅の動的設定
  - 算術 (`VADD.VV`, `VMUL.VV`)、メモリ (`VLE64.V`, `VSE32.V`)、マスク演算
  - セグメントロード/ストア (`VLSEG8E32.V`) でAoS→SoA変換を1命令で
  - RVV 1.0 は RISC-V Ratified spec (2021); SiFive X280/T-Head TH1520/Ventana Veyron で利用可能
- **根拠**: RISC-V V Extension Specification 1.0. RISC-V ベースの組み込み・HPC プロセッサ向け
- **難易度**: Hard

#### FR-574: ARM SME (Scalable Matrix Extension) — 行列演算

- **対象**: `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - ZA 行列ストレージ (2D タイル、SVE ベクトル幅の2乗) の管理
  - `SMSTART` / `SMSTOP` でストリーミングSVEモード切り替え
  - `FMOPA` (outer product accumulate) による行列積加算
  - `PSEL` + `LD1ROW` / `ST1ROW` でZA行アクセス
  - Apple M4 以降に搭載 (SME2 対応)
- **根拠**: ARM SME Architecture Supplement (ARMv9.2-A). 行列演算を SVE2 の4-16x高速化
- **難易度**: Very Hard

#### FR-575: x86-64 APX (Advanced Performance Extensions, Intel 2024)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - 汎用レジスタを16本から32本に拡張 (R16-R31): EVEX プレフィックスで符号化
  - NDD (New Data Destination): 2オペランドを3オペランドに拡張し `dst = src1 op src2` を1命令で
  - NF (No-Flags update) バリアント: フラグレジスタを破壊しない命令でフラグ依存チェーンを短縮
  - CFCMOV (Conditional Fused CMOV): 条件付きのフラグフリームーブ命令
  - Intel Lunar Lake (2024) / Arrow Lake で利用可能; CPUID `CPUID[24H].EAX[21]` で検出
- **根拠**: Intel APX Architecture Specification (2023). レジスタ圧力を大幅削減しスピルを削減
- **難易度**: Hard

#### FR-576: RISC-V Zicond (Integer Conditional Operations)

- **対象**: `packages/backend/emit/src/riscv64-codegen.lisp`
- **内容**:
  - `CZERO.EQZ rd, rs1, rs2`: rs2==0 なら rd=0, 否なら rd=rs1
  - `CZERO.NEZ rd, rs1, rs2`: rs2!=0 なら rd=0, 否なら rd=rs1
  - これら2命令を組み合わせて分岐なし条件選択: `cmov rd, cond, rs1, rs2` → 2命令シーケンス
  - `vm-jump-zero` + `vm-move` パターンをZicondに変換し分岐予測ミスを除去
- **根拠**: RISC-V Zicond ratified spec (2023). 条件移動が RISC-V の長年の欠落機能だった
- **難易度**: Easy

---

### Phase 98 — ML駆動最適化

#### FR-580: MLGO (ML-Guided Optimization) — インライン化コスト予測

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 現状のインライン閾値は静的ルール (`opt-pass-inline` のサイズ比較)
  - 関数の特徴量（呼び出し回数、命令数、ループ深度、引数数、ホットネス）から TFLite / ONNX 軽量モデルでインライン利益を予測
  - トレーニングデータ: cl-cc 自身のコンパイル結果を用いた self-training
  - 推論はコンパイル時に組み込みモデルから即時実行（外部プロセス不要）
- **根拠**: Google MLGO (LLVM upstream 2021)。llvm/lib/Analysis/InlineAdvisor.cpp。インライン決定の精度をヒューリスティックより改善
- **難易度**: Very Hard

#### FR-581: ML-Based Register Allocation (ML駆動レジスタ割り当て)

- **対象**: `packages/backend/emit/src/regalloc.lisp`
- **内容**:
  - スピルコスト予測モデル: 変数の使用頻度・ループ深度・ライブ区間長から最悪スピルコストを予測
  - スピル選択を強化学習（RL）で最適化（DeepMind/Google の RegAlloc RL 手法）
  - モデルサイズは < 1MB に制限し、コンパイル時間への影響を最小化
- **根拠**: "Register Allocation via Hierarchical Graph Coloring" (2022) / "Learning to Optimize Halide" 系の手法
- **難易度**: Very Hard

#### FR-582: Auto-Tuning for SIMD Tile Sizes (SIMDタイルサイズ自動チューニング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, SIMD backend
- **内容**:
  - FR-515（ループタイリング）のタイルサイズをコンパイル時に実測データから自動選択
  - ターゲットマシンの L1/L2/L3 キャッシュサイズをCPUID/sysctlで取得し、タイルが各キャッシュに収まるよう自動調整
  - Halide / TVM のオートスケジューラに相当する軽量版をコンパイラに内蔵
- **根拠**: TVM AutoTVM / ATLAS autotuning。静的タイルサイズは異なるマシンで最適でない
- **難易度**: Hard

---

### Phase 99 — WebAssembly・エッジランタイム (2024-2026)

#### FR-590: WASI Preview 2 / Component Model (WASI p2対応)

- **対象**: `packages/backend/emit/src/wasm.lisp`, `cli/src/main.lisp`
- **内容**:
  - WASI Preview 1 (`fd_read`, `fd_write` 等の syscall) から WASI Preview 2 (Component Model ベースのインターフェース型) へ移行
  - WIT (Wasm Interface Types) 定義から CL 型とのバインディングを自動生成
  - `wasi:filesystem`, `wasi:sockets`, `wasi:http` インターフェースの実装
  - `wasm-tools compose` でコンポーネントを組み合わせ可能なモジュールを生成
- **根拠**: WASI Preview 2 stabilized (2024). Wasmtime 15+ / WAMR 対応
- **難易度**: Hard

#### FR-591: Wasm Threads & Shared Memory (Wasmスレッド・共有メモリ)

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **内容**:
  - `shared` メモリ型 (`(memory 1 1 shared)`) と `atomic` 命令 (`i32.atomic.rmw.add` 等) のエミッション
  - `wait` / `notify` (futex 相当) 命令で CL の `bt:make-thread` 相当を Wasm スレッドにマッピング
  - CL のマルチスレッドセマンティクスを WebAssembly atomics で表現するランタイム設計
- **根拠**: WebAssembly Threads Proposal (ratified 2022). SharedArrayBuffer ベースの並列実行
- **難易度**: Very Hard

#### FR-592: Wasm SIMD128 (Wasm固定幅SIMD)

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **内容**:
  - `v128` 型と128-bit SIMD 命令 (`i32x4.add`, `f64x2.mul`, `v128.load` 等) のエミッション
  - FR-226（自動ベクトル化）の結果を Wasm SIMD128 に lowering
  - AVX2/NEON の128-bit サブセットを Wasm SIMD128 に移植するパターンテーブル
- **根拠**: WebAssembly SIMD Proposal (ratified 2021). Node.js 16+ / Chrome 91+ で利用可能
- **難易度**: Medium

#### FR-593: Wasm Relaxed SIMD (リラックスSIMD)

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **内容**:
  - Relaxed SIMD proposal (ratified 2023): `f32x4.relaxed_fma`, `i8x16.relaxed_swizzle` 等
  - ネイティブ `VFMADD` / `VTBL` に直接マッピングされる「ベストエフォート」セマンティクス命令
  - 通常の SIMD128 より 10-30% 高速（NaN の扱いやブレンド境界の決定論的保証を緩和）
- **根拠**: WebAssembly Relaxed SIMD Specification (2023)
- **難易度**: Easy

#### FR-594: Wasm Garbage Collection Proposal 完全実装

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **内容**:
  - WasmGC (ratified 2023): `struct.new <typeidx>`, `array.new <typeidx>`, `ref.cast`, `br_on_cast` のフル実装
  - CL CLOS インスタンスを WasmGC `struct` にマッピング（`__class__` フィールドは `externref`）
  - GC roots はランタイムが管理するため `vm-alloc` が不要になる
  - 継承は WasmGC の `sub` 型階層で表現
- **根拠**: WebAssembly GC Proposal (Chrome 119+, Firefox 120+). Java/Kotlin/Dart が既に採用
- **難易度**: Very Hard

---

### Phase 100 — 基礎最適化パス補完

現状の `optimizer.lisp` にはCSE・コピー伝播・定数畳み込み・DCE・インライン化があるが、以下の **-O2 相当の基礎パス** が欠落している。

#### ✅ FR-600: LICM (Loop Invariant Code Motion) — ループ不変式移動

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/optimize/src/cfg.lisp`
- **内容**:
  - ループ内の式のうち、ループ反復によって値が変わらないもの（ループ不変式）をループ前のプレヘッダーブロックに移動
  - 支配関係チェック: 不変式の移動先がすべての出口を支配する場合のみ合法
  - セーフ移動: 例外を発生しうる式（除算、型チェック）は保守的に移動しない
  - `vm-const`, `vm-add` (両オペランドがループ外定義), `vm-slot-ref` (読み取り専用) が主な候補
- **根拠**: GCC `-fmove-loop-invariants` / LLVM `LoopInvariantCodeMotion`. 数値ループで最も効果的な基礎最適化の一つ
- **難易度**: Medium

#### FR-601: Loop Unrolling (ループアンロール)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 固定回数ループ（`dotimes` で静的判明する `n`）を本体複製で展開しループオーバーヘッドを削減
  - **完全アンロール**: 反復回数が小定数（≤8）の場合にループ制御命令を完全除去
  - **部分アンロール**: 反復回数が不明な場合に unroll factor=4 で展開し剰余ループを生成
  - アンロール後に定数畳み込み・CSE が再適用されてコードが縮小することが多い
- **根拠**: GCC `-funroll-loops` / LLVM `LoopUnroll`. ベクトル化（FR-226）の前処理としても機能
- **難易度**: Medium

#### FR-602: Loop Unswitching (ループアンスイッチング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - ループ内のループ不変な条件分岐 `(if loop-invariant-cond body-a body-b)` をループ外に持ち出す
  - 結果: `(if loop-invariant-cond (loop body-a) (loop body-b))` という2つの特化ループに変換
  - 各ループ内の分岐消去によりILPが向上し、ベクトル化も容易になる
  - コードサイズが増加するため `(declare (optimize (space 0) (speed 3)))` 時のみ積極適用
- **根拠**: GCC `-floop-unswitch` / LLVM `LoopUnswitch`. 型チェック・NULL チェックを含む CL ループに特に有効
- **難易度**: Medium

#### ✅ FR-603: Jump Threading (ジャンプスレッディング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 分岐先に別の分岐があり、その条件が到達パスから既知の場合に直接ターゲットにショートカット
  - 例: `(if (> x 0) (if (> x 0) a b) c)` → `(if (> x 0) a c)`
  - CFG エッジを `(predecessor, condition-value)` ペアでアノテートし、既知値から冗長分岐を除去
  - SCCP（FR-510）の判定結果をジャンプスレッディングのヒントとして再利用
- **根拠**: GCC `-fthread-jumps` / LLVM `JumpThreading`. -O1 相当の基本パス
- **難易度**: Medium

#### ✅ FR-604: If Conversion / CMOV Selection (if変換・条件移動命令選択)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - 分岐予測ミスコストが高い短い条件分岐を条件移動命令に変換
  - `(if cond a b)` → `vm-select(cond, a, b)` MIR命令 → `CMOVNE` (x86-64) / `CSEL` (AArch64)
  - 変換条件: then/else 両ブロックに副作用がなく、1-3 命令程度で完結する場合
  - プロファイル情報（FR-058）があれば偏りが小さい分岐を優先変換
- **根拠**: GCC `-fif-conversion2` / LLVM `IfConversion`. 分岐予測ミスペナルティ（10-20サイクル）を除去
- **難易度**: Medium

#### ✅ FR-605: Reassociation (再結合)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 加算・乗算など交換則・結合則が成立する演算の順序を再整理し、CSE・定数畳み込みの機会を増やす
  - `(+ a (+ b c))` → `(+ (+ a b) c)`: `a+b` が別のパスでも計算されていれば CSE でヒット
  - 定数を右にまとめる: `(+ 1 (+ x 2))` → `(+ x 3)` を定数畳み込みで実現
  - ランクベース順序付け（LLVM方式）: 演算ツリーのオペランドをランク順に整列
- **根拠**: GCC `-fassociative-math` / LLVM `Reassociate`. CSEの命中率を大幅改善する前処理
- **難易度**: Medium

#### FR-606: Dead Argument Elimination (DAE) — デッド引数除去

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - 関数の引数が関数本体内で一切使用されない場合、その引数を除去した特化版を生成
  - 呼び出し側も対応する引数の計算・レジスタ渡しを除去
  - LTO（FR-500）と連携するとクロスモジュールでも適用可能
  - `(defun foo (x _unused y) (+ x y))` → `(defun foo (x y) (+ x y))` 相当の変換
- **根拠**: LLVM `DeadArgumentElimination` pass. レジスタ圧力削減とインライン化後のクリーンアップに有効
- **難易度**: Medium

#### FR-607: Identical Code Folding / Function Merging (同一コード折り畳み)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/backend/binary/src/elf.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - コンパイル済み関数のバイト列を比較し、完全一致する関数を単一エントリにマージ
  - テンプレート展開やコード生成で重複が生まれやすい数値特化関数が主な対象
  - リンカレベル ICF（LLVM LLD `--icf=all`, GNU gold `--icf=safe`）と VM IR レベルでの実施
  - Mach-O: `__text` セクションを `-function-sections` 相当で関数単位に分割してマージを可能に
- **根拠**: LLVM LLD ICF / gold ICF. コードサイズを数%〜数十%削減（大規模テンプレート展開時）
- **難易度**: Medium

#### FR-608: Tail Duplication (末尾複製)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/optimize/src/cfg.lisp`
- **内容**:
  - 複数の前任者を持つ小さな後継ブロックを各前任者にコピーし、CFG を単純化
  - 複製後の各コピーは前任者固有の定数・型情報を持つため、定数畳み込み・DCE が適用可能になる
  - コードサイズ増加と最適化利得のトレードオフを命令数閾値（≤ 5命令）で制御
- **根拠**: GCC tail-duplication / LLVM `TailDuplication`. ジャンプスレッディング（FR-603）と相補的
- **難易度**: Medium

---

### Phase 101 — 値範囲解析・安全性最適化

#### FR-610: VRP (Value Range Propagation) — 値範囲伝播

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 各仮想レジスタの実行時値の範囲 `[lo, hi]` を静的に追跡
  - 分岐条件 `(> x 0)` の then 節では `x ∈ [1, +∞)` と精密化（条件付き定数伝播）
  - 整数型の定義（`fixnum` ≡ `[most-negative-fixnum, most-positive-fixnum]`）をベースに範囲初期化
  - SCCP（FR-510）のラティスを区間ラティスに拡張したもの
- **根拠**: GCC `-ftree-vrp` / LLVM `LazyValueInfo`. BCE・Null チェック除去・オーバーフロー除去の基盤
- **難易度**: Hard

#### FR-611: Bounds Check Elimination (BCE) — 配列境界チェック除去

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(aref arr i)` → `vm-array-ref(arr, i)` で毎回 `0 ≤ i < length` チェックを実行
  - VRP（FR-610）で `i` の範囲が `[0, n-1]` かつ `n ≤ length` と証明できる場合にチェックを省略
  - `dotimes` ループ変数は自動的に `[0, n-1]` 範囲が証明済みなので内部の `aref` チェックを全除去可能
  - ホイスト: ループ外でチェックを1回だけ実行するバージョンも生成（安全性を保ちながらループ内チェック除去）
- **根拠**: Java HotSpot BCE / JVM JIT. 型安全言語での最重要最適化の一つ; 配列アクセスを C 相当の速度に
- **難易度**: Hard

#### ✅ FR-612: Null / NIL Check Elimination (NILチェック除去)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(car x)` / `(cdr x)` の前に暗黙的に実行される `(null x)` チェックを、VRP で `x ≠ NIL` が証明された場合に省略
  - 型チェック `(consp x)` が通過した直後のパスでは `x` が cons と確定しているため以降のチェック除去
  - SBCL の `(declare (type cons x))` 相当の効果をランタイムプロファイルから導出
- **根拠**: V8 null check elimination / Java JIT null check folding
- **難易度**: Medium

#### FR-613: Integer Overflow Check Elimination (整数オーバーフローチェック除去)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - CL の `+` は `BIGNUM` へのオーバーフローを検査する必要があるが、VRP で両オペランドが安全範囲内と証明できれば除去
  - `fixnum + fixnum ≤ most-positive-fixnum` の証明: VRP で `a ∈ [0, 1000]`, `b ∈ [0, 1000]` → 和が `fixnum` 範囲内
  - オーバーフローチェック除去後は `ADD` 単命令に縮退（現状は `vm-add` + `vm-overflow-check` の2命令）
- **根拠**: SBCL の fixnum 最適化と同等の効果。タイトな数値ループで最大2倍の高速化
- **難易度**: Hard

#### FR-614: Bitwidth Reduction / Integer Narrowing (ビット幅削減)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - VRP で値が `[0, 255]` 範囲に収まると証明された変数を `i64` から `i32` または `i8` に縮退
  - x86-64 では 32-bit 演算が 64-bit より若干高速（REX プレフィックス不要）かつ即値エンコーディングが短縮
  - ループカウンタが `[0, n]` で `n < 2^31` なら `i32` に縮退してレジスタ使用も削減
  - AArch64 では `W` レジスタ（32-bit）使用でコード密度向上
- **根拠**: LLVM `InstCombine` bitwidth narrowing. 微小だが累積効果は大きい
- **難易度**: Medium

---

### Phase 102 — GC高度化・メモリ管理

#### FR-620: Concurrent GC — 並行マーキング (Shenandoah / ZGC スタイル)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - 現状: Stop-The-World 2世代GC（`gc.lisp`）
  - ミューテータスレッドと並行してマーキングフェーズを実行（Tri-color marking + write barrier）
  - **SATB (Snapshot-At-The-Beginning) write barrier**: マーキング中に上書きされる旧ポインタをマークキューに追加
  - 並行スイープ: マーキング完了後の解放をミューテータと並行実行
  - セーフポイント（FR-540）でミューテータを短時間停止してルートスキャンのみ実施（STW < 1ms）
- **根拠**: Shenandoah GC (OpenJDK) / ZGC. GCポーズを10ms以下に抑えるレイテンシ重視アーキテクチャ
- **難易度**: Very Hard

#### FR-621: Compacting GC / Object Moving (コンパクションGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - 生存オブジェクトをヒープ前方に移動してフラグメンテーションを解消
  - **Cheney's copying GC**: 半空間コピーで若世代を整理（現状の Minor GC を置き換え）
  - **Mark-Compact**: 老世代の mark-compact（Lisp2 アルゴリズム: フォワードポインタテーブル）
  - オブジェクト移動後にスタックマップ（FR-541）を使って全ルートポインタを更新
- **根拠**: Standard GC algorithm. ヒープ断片化による性能劣化（malloc 検索コスト増大）を防止
- **難易度**: Very Hard

#### FR-622: Large Object Space (LOS) — 大オブジェクト専用アリーナ

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - 一定サイズ（例: 8KB）以上のオブジェクト（大配列、大文字列）を通常ヒープではなく LOS に割り当て
  - LOS はページ単位で管理し、コンパクションの対象外（大オブジェクトの移動コストが高いため）
  - LOS はカード単位（`heap.lisp:39` の512Bカードテーブル）でのみライトバリアを管理
  - 閾値は `*large-object-threshold*` で調整可能
- **根拠**: Java G1GC LOS / Hotspot large object handling. 大オブジェクトのコピーコストを除去
- **難易度**: Hard

#### FR-623: Huge Pages Support (ヒュージページ対応)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/binary/src/macho.lisp`, `packages/backend/binary/src/elf.lisp`
- **内容**:
  - **コード領域**: JIT コンパイルしたホットコードを 2MB ヒュージページ（x86-64）または 16MB ページ（AArch64）に配置。TLB カバレッジを劇的改善
  - **ヒープ領域**: `mmap(MAP_HUGETLB)` (Linux) / `MAP_ALIGNED_SUPER` (FreeBSD) / Transparent Huge Pages (THP)
  - macOS: `mmap(MAP_JIT)` + `vm_map` で 4MB スーパーページを要求
  - `*use-huge-pages*` フラグで実行時に有効/無効を切り替え
- **根拠**: Linux hugetlbfs / macOS superpage. TLB ミスを1/512 に削減; JVM `-XX:+UseHugePagesInMetaspace` と同等
- **難易度**: Medium

#### FR-624: NUMA-Aware Memory Allocation (NUMAアウェア割り当て)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - マルチソケットシステムで各スレッドが自ノードのメモリから割り当てるよう `mbind`/`numa_alloc_onnode` を使用
  - GC 世代をNUMAノードに対応付け: 若世代はスレッドローカルノードから割り当て
  - `numactl` / `/proc/self/numa_maps` でNUMAトポロジを実行時に検出
- **根拠**: Linux NUMA API. 4ソケット以上のサーバーで10-30%のスループット改善
- **難易度**: Hard

#### FR-625: Arena Allocation for Compiler Phases (コンパイラ内アリーナ割り当て)

- **対象**: `pipeline/src/pipeline.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - コンパイル1関数ごとに専用アリーナを確保し、最適化パス内の一時オブジェクトをアリーナから割り当て
  - 関数コンパイル完了時にアリーナを一括解放（個別 GC 不要）
  - CL では `(let ((*arena* (make-arena))) ...)` + カスタムアロケータとして実装
  - コンパイル速度の GC 圧力を大幅削減（AST ノード・MIR 命令の大量一時生成が主な対象）
- **根拠**: LLVM BumpPtrAllocator / GCC obstack. コンパイラ内部の GC 停止が問題になる規模では必須
- **難易度**: Medium

---

### Phase 103 — スレッド・並行性モデル

#### FR-630: Thread-Local Storage Code Generation (TLSコード生成)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - CL の `defvar` に `:thread-local t` 相当の宣言を追加し、スレッドローカル変数をサポート
  - **x86-64 Linux**: `FS`セグメントレジスタ経由のTLSアクセス (`MOV RAX, FS:[offset]`)、`R_X86_64_TPOFF32` リロケーション
  - **x86-64 macOS**: `__thread` 変数は `__DATA,__thread_bss` + `tlv_get_addr` スタブ経由
  - **AArch64**: `TPIDR_EL0` システムレジスタ経由 + `MRS X0, TPIDR_EL0; LDR X1, [X0, #offset]`
  - TLS モデル: Local Exec（実行可能形式）/ Initial Exec（共有ライブラリ）/ General Dynamic（dlopen対応）の3モード
- **根拠**: ELF TLS Specification / Drepper "ELF Handling For Thread-Local Storage". マルチスレッド CL の基礎
- **難易度**: Hard

#### FR-631: Atomic Operations / Memory Model (アトミック操作・メモリモデル)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - CL の `bt:with-lock-held` 等の下層に必要なアトミック命令の生成
  - **x86-64**: `LOCK CMPXCHG`（CAS）、`LOCK XADD`（fetch-add）、`MFENCE`/`SFENCE`/`LFENCE`（メモリバリア）
  - **AArch64**: `LDXR`/`STXR`（Load-Exclusive/Store-Exclusive）、`CASAL`（Compare-and-Swap ARMv8.1-A LSE）、`DMB ISH`（バリア）
  - C11 メモリオーダー（`seq_cst`, `acquire`, `release`, `relaxed`）を CL から指定可能な API
- **根拠**: C11 stdatomic / Java Memory Model / Arm A-profile Memory Model
- **難易度**: Hard

#### FR-632: Parallel Compilation (並列コンパイル)

- **対象**: `pipeline/src/pipeline.lisp`, `cli/src/main.lisp`
- **内容**:
  - ファイル間依存グラフ（`our-load` が構築するロード順）を解析し、依存関係のないファイルを並列コンパイル
  - `lparallel` / `bt:make-thread` を用いた worker スレッドプール（スレッド数 = CPU コア数）
  - 関数内コンパイルも独立関数は並列化可能: `defun` 間に依存がなければ同時最適化
  - 共有状態（マクロ環境、型テーブル）のロック最小化設計
- **根拠**: LLVM parallel compilation / Rust `cargo` の並列コンパイル. ビルド時間をコア数に比例して短縮
- **難易度**: Hard

#### FR-633: Lock-Free VM Instruction Dispatch (ロックフリーVMディスパッチ)

- **対象**: `packages/engine/vm/src/vm.lisp`
- **内容**:
  - 複数スレッドが同一VMで実行される場合の命令ディスパッチをロックフリーに設計
  - 各スレッドが独立した `vm-state` を保持（現状 `*vm-state*` はグローバル変数）
  - `*vm-state*` を TLS 変数（FR-630）に移行し、スレッド切り替えコストをゼロに
  - GC セーフポイント（FR-540）での全スレッド停止はハードウェアアトミック経由で協調
- **根拠**: V8 Isolate / Ruby Ractors. スレッドごとの独立VMは並行実行の前提条件
- **難易度**: Hard

---

### Phase 104 — 開発者体験・ビルドインフラ

#### FR-640: Incremental Compilation (インクリメンタルコンパイル)

- **対象**: `pipeline/src/pipeline.lisp`, `cli/src/main.lisp`
- **内容**:
  - ファイルの内容ハッシュ（SHA-256）と依存ファイルのハッシュを記録し、変更がないファイルのコンパイルをスキップ
  - `.deps` ファイルに各ソースファイルが使うマクロ・型・グローバル変数の依存関係を記録
  - 依存関係の変更（マクロ再定義、型シグネチャ変更）は下流ファイルの再コンパイルをトリガー
  - SBCL の FASL キャッシュより精密: cl-cc 独自の依存グラフで最小限の再コンパイルを実現
- **根拠**: Rust `cargo` incremental / Bazel dependency tracking. 大規模プロジェクトでのビルド時間をO(変更量)に
- **難易度**: Hard

#### FR-641: Hot Reload / Live Patching (ホットリロード)

- **対象**: `packages/engine/vm/src/vm.lisp`, `cli/src/main.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - 実行中の CL プロセスに対して `(our-load "file.lisp")` で関数定義を上書き可能（現状は再起動が必要な場合あり）
  - JIT コンパイル済みコードの場合: 新バージョンをコンパイルし、古い `vm-func-ref` エントリをアトミックに差し替え
  - 実行中の関数が完了してから切り替え（関数の世代管理、OSR と逆方向の操作）
  - REPL 統合: `:reload` コマンドでファイル全体を差分適用
- **根拠**: Erlang hot code loading / Common Lisp image-based development. CL の live coding 文化の核心機能
- **難易度**: Hard

#### FR-642: IR Verification Passes (IR検証パス)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/foundation/mir/src/mir.lisp`
- **内容**:
  - 各最適化パスの前後で IR の不変条件（invariants）を検証するデバッグモードを追加
  - 検証項目: SSA の dom 制約（各 use が def を支配するか）、レジスタが未定義のまま使用されていないか、ライブ区間の一貫性
  - `(declare (optimize (debug 3)))` または `--verify-ir` フラグで有効化
  - 検証失敗時は pass 名・違反箇所・IR dump を出力してエラー
- **根拠**: LLVM `verifyFunction` / GCC `verify_ssa`. 最適化バグの早期発見に不可欠
- **難易度**: Medium

#### FR-643: Optimization Pass Pipeline Configuration (最適化パイプライン設定)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `cli/src/main.lisp`
- **内容**:
  - ユーザーが最適化パスの順序・有効/無効を設定可能にする
  - `--pass-pipeline="dce,cse,inline,sccp,gvn"` 形式のコマンドライン引数
  - `(declare (optimize ...))` の `speed`/`space`/`debug` 値に対応するプリセットパイプライン
  - `--print-pass-timings` でパスごとの実行時間を表示（コンパイラ開発・デバッグ用）
- **根拠**: LLVM `opt -passes=...` / GCC pass framework. パス開発・実験・チューニングの基盤
- **難易度**: Medium

#### FR-644: Compiler Diagnostics / Optimization Remarks (最適化レマーク)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - 最適化の成功・失敗をソース位置付きで出力する仕組み
  - 例: `note: foo.lisp:42: inlined call to bar (3 instructions)` / `note: foo.lisp:55: loop not vectorized: data dependency`
  - YAML / LLVM OptRecord 形式でファイルに出力し、外部ツールで可視化可能
  - `--opt-remarks=all` / `--opt-remarks=missed` で詳細度を制御
- **根拠**: Clang `-Rpass` / LLVM OptimizationRemarks. 開発者がコードを最適化ヒントに沿って書けるようになる
- **難易度**: Medium

---

### Phase 105 — バイナリ強化・ABI補完

#### FR-650: ELF RELRO / BIND_NOW (ELFランタイム強化)

- **対象**: `packages/backend/binary/src/elf.lisp`
- **内容**:
  - **RELRO (Relocation Read-Only)**: ロード時リロケーション完了後に `.got` セクションを読み取り専用に `mprotect`
  - **Full RELRO**: `BIND_NOW` フラグ付きで起動時に全シンボルを解決し、`.got.plt` も読み取り専用化
  - `PT_GNU_RELRO` プログラムヘッダセグメントを生成
  - `DT_FLAGS: DF_BIND_NOW` を `.dynamic` セクションに追加
- **根拠**: Linker hardening best practices (Fedora, Debian hardening). GOT overwrite 攻撃を防止
- **難易度**: Medium

#### FR-651: GOT / PLT Generation (動的リンクテーブル生成)

- **対象**: `packages/backend/binary/src/elf.lisp`, `packages/backend/binary/src/macho.lisp`
- **内容**:
  - **GOT (Global Offset Table)**: 外部シンボルの実行時アドレスを格納するテーブルを生成
  - **PLT (Procedure Linkage Table)**: 共有ライブラリ関数の遅延バインディングスタブを生成
  - ELF: `R_X86_64_GLOB_DAT` / `R_X86_64_JUMP_SLOT` リロケーションエントリ
  - Mach-O: `__stub` セクション + `__got` セクション + `dyld_stub_binder` 連携
  - PIE モード（`-fPIE`）との統合: RIP 相対 GOT アクセス (`R_X86_64_REX_GOTPCRELX`)
- **根拠**: System V ABI / ELF specification. 共有ライブラリを使用する実行形式の必須コンポーネント
- **難易度**: Hard

#### FR-652: Split DWARF / DWO Files (デバッグ情報分離)

- **対象**: `packages/backend/binary/src/elf.lisp`, `packages/backend/binary/src/macho.lisp`
- **内容**:
  - デバッグ情報（`.debug_info`, `.debug_line` 等）を実行可能形式から分離した `.dwo` ファイルに格納
  - 実行可能形式には `.debug_info` へのスケルトン CU（Compilation Unit）参照のみを保持
  - **GNU debuglink**: `.gnu_debuglink` セクションでデバッガに `.dwo` の場所を通知
  - ビルド時間: デバッグ情報のリンカへの入力が激減し、最終リンクが高速化
- **根拠**: DWARF 5 DWO / GCC `-gsplit-dwarf`. 大規模プロジェクトでリンク時間を50%削減することがある
- **難易度**: Medium

#### FR-653: C ABI Compatibility / `export-cl-function` (C ABI互換エクスポート)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/calling-convention.lisp`
- **内容**:
  - `(defun foo (x y) ... :export :c)` 相当の宣言で C から呼び出し可能な関数を生成
  - System V AMD64 ABI / Windows x64 ABI に完全準拠したプロローグ・エピローグ
  - CL型 → C型の自動マーシャリング: `fixnum` → `int64_t`, `simple-vector` → `void*` + サイズ引数
  - ヘッダファイル（`.h`）の自動生成
- **根拠**: SBCL `sb-alien` / ECL native interface. C ライブラリとしての利用や Python/Ruby から FFI で呼び出す用途
- **難易度**: Hard

#### FR-654: eBPF Backend (eBPFバックエンド)

- **対象**: 新規 `packages/backend/emit/src/ebpf-codegen.lisp`, `packages/foundation/mir/src/target.lisp`
- **内容**:
  - Linux カーネル内で実行される eBPF プログラム（64-bit RISC 命令セット、11レジスタ）の生成
  - eBPF 命令セット: `BPF_ADD`, `BPF_LD`, `BPF_CALL`、Map アクセスヘルパー呼び出し
  - **ターゲット用途**: kprobe/tracepoint アタッチ関数、XDP ネットワークフィルタ、TC (Traffic Control) プログラム
  - BPF Verifier 制約準拠: 有界ループ、スタック深度制限 (512B)、null ポインタチェック
  - `bpftool` / `libbpf` でロード可能な ELF オブジェクト（`BPF_PROG_TYPE_*`セクション付き）を出力
- **根拠**: Linux eBPF (kernel 5.x+). 2024-2026年に最も成長しているシステムプログラミング領域の一つ
- **難易度**: Very Hard

---

### Phase 106 — ポストリンク最適化

#### FR-660: BOLT (Binary Optimization and Layout Tool) スタイル最適化

- **対象**: `packages/backend/binary/src/elf.lisp`, `packages/backend/binary/src/macho.lisp`
- **内容**:
  - リンク済みバイナリを `perf.data` / `dtrace` プロファイルを入力として再最適化
  - **Basic Block Reordering**: ホットなフォールスルーパスをジャンプなしの連続アドレスに再配置
  - **Function Splitting**: ホット / コールドコードを分離し、`__text_hot` + `__text_cold` セクションに分ける
  - **Function Reordering**: コールグラフ順で関数をソート（FR-186 のポストリンク版）
  - バイナリ入出力: ELF の `.text` セクションを in-place 書き換えまたは新規バイナリ生成
- **根拠**: Facebook/Meta BOLT (LLVM upstream 2022) / Google Propeller. Meta の報告では Facebook サーバーで 2-7% CPU 削減
- **難易度**: Very Hard

#### FR-661: Propeller / Profile-Guided Layout (プロファイルガイドレイアウト)

- **対象**: `pipeline/src/pipeline.lisp`, `packages/backend/binary/src/elf.lisp`
- **内容**:
  - BOLT はポストリンクのバイナリ書き換えだが、Propeller はコンパイル・リンク段階でレイアウトを最適化
  - `--propeller-profile=perf.data` でコンパイル時に関数・基本ブロックの配置を決定
  - 各 `defun` を独立セクション（`-ffunction-sections` 相当）に配置し、リンカスクリプトで順序を制御
  - BOLT との使い分け: Propeller はビルドシステム統合型、BOLT は CI の後処理型
- **根拠**: Google Propeller (2019, LLVM integration 2023). BOLT が難しい環境（セキュアブート、再署名必要）での代替
- **難易度**: Hard

#### FR-662: Basic Block Versioning / Path Profiling (基本ブロックバージョニング)

- **対象**: `packages/engine/optimize/src/cfg.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 実行パスの頻度を基本ブロック単位でなくエッジ（パス）単位で計測（Ball-Larus path profiling）
  - 頻繁に実行されるパスを「スーパートレース」として複製し、パス固有の最適化（定数伝播、型特化）を適用
  - FR-201（トレーススケジューリング）のプロファイルガイド版
- **根拠**: Ball-Larus path profiling (1996) / HotSpot path-based profiling
- **難易度**: Hard

---

### Phase 107 — 関数型言語固有の最適化

CL・Scheme・ML 系コンパイラ特有の最適化。汎用コンパイラドキュメントには現れないが cl-cc にとって最も性能インパクトが大きい領域。

#### FR-670: Flat Closure / Lambda Lifting (フラットクロージャ・ラムダリフティング)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**:
  - **現状**: クロージャは `vm-closure` 構造体に `(register . value)` ペアリストを保持（`vm.lisp` コメント参照）。クロージャ変数アクセスは毎回リスト線形探索またはハッシュ検索
  - **フラットクロージャ**: キャプチャ変数をオフセット固定の配列（ベクタ）に格納。アクセスを `LDR [closure_ptr + offset*8]` の単命令に
  - **ラムダリフティング**: 自由変数をすべてパラメータとして追加し、クロージャオブジェクト自体を除去。再帰的でない小クロージャはスタック割り当てを最大化（FR-516 escape analysis と連携）
  - **クロージャ共有**: 同一スコープの複数クロージャが同じ変数をキャプチャする場合、共有環境オブジェクトを1つ生成
- **根拠**: MLton / CHICKEN Scheme / GHC closure conversion. クロージャアクセスコストをO(n)→O(1)に削減
- **難易度**: Hard

#### FR-671: CLOS Slot-Vector Optimization (CLOSスロットベクタ最適化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - **現状**: CLOS インスタンスはスロット名→値のハッシュテーブル（`vm-clos.lisp`）。`slot-value` はハッシュ検索
  - **スロットベクタ化**: クラス定義時にスロット名→固定オフセットのテーブルをクラスメタオブジェクトに記録。インスタンスをオフセット付き単純ベクタに変換
  - `(slot-value obj 'x)` → `(aref (instance-slots obj) 3)` に変換。`vm-slot-ref(obj, 3)` 単命令
  - インスタンスレイアウト変更（`change-class`）時のスロット再マッピングは MOP 経由で処理
- **根拠**: SBCL/PCL のインスタンス表現。ハッシュテーブル検索 → 配列インデックス化でスロットアクセスが 10-20x 高速化
- **難易度**: Hard

#### FR-672: CLOS Vtable Dispatch (CLOSメソッドvtable化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
- **内容**:
  - **現状**: `vm-dispatch-generic-call` が毎回 `vm-get-all-applicable-methods` でメソッドリストを検索
  - **Vtable**: クラスごとに「総称関数 → 最適適用メソッドのクロージャ」のベクタを生成（C++ vtable 相当）
  - 単ディスパッチ（引数1つの型で決定）: vtable の固定オフセットへの単一インダイレクト呼び出しに縮退
  - 多重ディスパッチ（引数2つ以上）: 多次元ディスパッチテーブル（行=specializer1, 列=specializer2）をクラスに紐付け
  - `defmethod` 追加時はvtableをダーティマークし次の呼び出し時に再構築（インクリメンタル再構築）
- **根拠**: Dylan / Cecil / SBCL discriminating functions. CLOS dispatch の最大ボトルネックを直接解消
- **難易度**: Hard

#### FR-673: Keyword Argument Dispatch Optimization (キーワード引数ディスパッチ最適化)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - **現状**: `&key` 引数は呼び出し側がキーワードシンボルのリストを構築し、呼ばれた側が `getf` でスキャン。O(n) コスト
  - **コンパイル時解決**: 呼び出しサイトで全キーワードが静的判明な場合、キーワードリストを省略して位置引数渡しに変換
  - **ビットマスク方式**: 各 `&key` 引数に bit 位置を割り当て、渡されたキーワードセットを整数ビットマスクで表現。`POPCNT` + テーブルルックアップで O(1) dispatch
  - `allow-other-keys` を持つ関数は保守的フォールバック維持
- **根拠**: SBCL の keyword arg optimization / ECL. ライブラリ API で多用される `&key` のオーバーヘッドを除去
- **難易度**: Medium

#### FR-674: Continuation / CPS Reduction (継続・CPS削減)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - CPS 変換後の管理的継続（administrative continuations）を除去し、直接スタイルに近いコードを生成
  - **η-削減**: `(λ (x) (k x))` は `k` と等価 → 継続渡し呼び出しをインライン化
  - **β-削減**: 継続 `(let ((k (λ (x) body))) (k val))` → `(let ((x val)) body)` に展開
  - **末尾継続除去**: 末尾位置の継続適用を直接リターンに変換（fr-073 と連携）
  - 削減後は通常の DCE・定数畳み込みパスが追加の効果を発揮
- **根拠**: Appel "Compiling with Continuations" / Flanagan "The Essence of Compiling with Continuations". CPS後の不要なクロージャ生成を除去
- **難易度**: Hard

#### ✅ FR-675: Purity Analysis / Effect Tracking (純粋性解析・作用追跡)

- **対象**: `pipeline/src/pipeline.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 副作用のない関数（純粋関数）を静的に検出し、呼び出し間のCSEとモーションを許可
  - **効果アノテーション**: `no-side-effects`, `reads-only`, `writes-global` の3レベルを関数に付与
  - 純粋な `defun` は: (a) 同一引数での重複呼び出しをCSEで除去、(b) ループ外に移動（LICM, FR-600）
  - 組み込み関数テーブル（`builtin-registry.lisp`）にeffectアノテーションを追加
  - `declare (pure)` 等のユーザーアノテーションも受け入れ
- **根拠**: GCC `__attribute__((pure))` / LLVM `readonly`/`readnone`. 純粋関数の判定は関数間CSEの前提
- **難易度**: Medium

#### FR-676: Defunctionalization (脱関数化)

- **対象**: `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - 高階関数（関数を引数に取る関数）をファーストクラス関数なしで実現する変換
  - `(mapcar f list)` で `f` が呼び出しサイトで静的判明な場合、クロージャ生成を完全に除去
  - `apply` / `funcall` の引数が定数関数の場合は直接 `call` に変換
  - `(sort list #'<)` → `(sort list :comparator :fixnum-less-than)` の特化版に変換
- **根拠**: Reynolds defunctionalization (1972) / MLton whole-program defunctionalization. `funcall` / `apply` のオーバーヘッド除去
- **難易度**: Hard

#### FR-677: Delimited Continuations / Shift-Reset (区切り継続)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/cps.lisp`
- **内容**:
  - `shift` / `reset` (Danvy & Filinski) または `call/cc` の効率的な実装
  - セグメントスタック（segmented stack）または スタックコピー方式でキャプチャ
  - 継続を first-class オブジェクトとして `vm-continuation` 型で表現
  - `restart-case` / `handler-case` の実装を区切り継続で統一し、EH（FR-560）と統合
- **根拠**: Racket delimited continuations / Gambit `call/cc`. CL の `restart-case` / `throw`/`catch` の意味論的基盤
- **難易度**: Very Hard

---

### Phase 108 — 標準最適化パス補完

#### ✅ FR-680: PRE (Partial Redundancy Elimination) — 部分冗長除去

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - CSE（完全冗長除去）を一般化: 一部のパスでのみ冗長な計算を、全パスで計算済みになるよう「挿入」して除去
  - Morel-Renvoise アルゴリズム: 前向き/後ろ向きデータフロー方程式で insertion/deletion 点を計算
  - PRE は LICM（FR-600）と CSE の両方を特殊ケースとして包含
  - 実装: Lazy Code Motion (Knoop, Rüthing, Steffen 1992) が実用的
- **根拠**: GCC `-fgcse` (global CSE = PRE) / LLVM `EarlyCSE` + `GVN-PRE`. -O2 の核心最適化
- **難易度**: Hard

#### FR-681: Strength Reduction for Induction Variables (帰納変数強度削減)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - ループ帰納変数 `i` に基づく `i * stride` を加算列 `base, base+stride, base+2*stride, ...` に変換
  - `(aref arr (* i 8))` のアドレス計算を `ptr += 8` のインクリメントに変換（乗算除去）
  - **除算の乗算化**: `(/ x c)` で `c` が定数の場合、乗算+シフトシーケンスに変換（例: `/10` → `mulq 0x6666...`）
  - SCEV（Scalar Evolution）情報から帰納変数の線形関係を抽出
- **根拠**: GCC `-fstrength-reduce` / LLVM `IndVarSimplify`. 配列走査ループで乗算命令を完全除去
- **難易度**: Medium

#### FR-682: Loop Peeling (ループピーリング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - **先頭ピーリング**: ループの最初の1-N回の反復を本体外に複製。ループ先頭の条件分岐（境界チェック・初期化）をループ外で解決
  - **末尾ピーリング**: ループの最後の反復を分離し、ベクトル化残余ループをスカラー化
  - **アライメントピーリング**: メモリアクセスが16/32バイト境界にアラインするまでの反復を前ピーリング（FR-226 ベクトル化の準備）
  - ピーリング後の各反復コピーで定数畳み込み・BCE（FR-611）が適用可能に
- **根拠**: GCC `-fpeel-loops` / LLVM `LoopPeel`. ベクトル化の前処理として LLVM が内部的に多用
- **難易度**: Medium

#### FR-683: Loop Rotation (ループ回転)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/optimize/src/cfg.lisp`
- **内容**:
  - `while (cond) { body }` 形式のループ（ヘッダで条件チェック）を `do { body } while (cond)` 形式（末尾で条件チェック）に変換
  - 変換後: (a) ループヘッダへの分岐が1つ減る、(b) 最初の反復前に事前チェックを1回追加
  - 回転後のループはバックエッジが1本になり、パイプライン化・ベクトル化が容易
  - CL の `(loop :while cond :do body)` → `(when cond (loop :until (not cond) :do body))` 相当
- **根拠**: GCC `-floop-optimize` の一部 / LLVM `LoopRotate`. ベクトル化可能性の前提条件として LLVM が必ず実行
- **難易度**: Easy

#### FR-684: Idiom Recognition (イディオム認識)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - よく使われる操作パターンを単一の効率的な命令またはランタイム関数呼び出しに変換
  - **memset認識**: `(dotimes (i n) (setf (aref arr i) 0))` → `(fill arr 0)` → `memset(arr, 0, n*8)`
  - **memcpy認識**: `(dotimes (i n) (setf (aref dst i) (aref src i)))` → `memcpy(dst, src, n*8)`
  - **strlen認識**: `(loop :for i :from 0 :until (= (schar s i) #\Nul) :finally (return i))` → `strlen(s)`
  - **population count**: `(loop :for x = n :then (ash x -1) :until (= x 0) :sum (logand x 1))` → `POPCNT`
- **根拠**: LLVM `LoopIdiomRecognize` pass. `memset`/`memcpy` の認識だけで多くのプログラムで顕著な高速化
- **難易度**: Medium

#### FR-685: Integer Division by Constant → Multiply+Shift (定数除算の乗算化)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - `(/ x c)` で `c` がコンパイル時定数の場合、除算命令（20-80サイクル）を乗算+シフト（3-5サイクル）に変換
  - **符号なし**: `floor(x/c)` = `floor(x * m / 2^p)` で魔法定数 `m`, `p` を事前計算（GCC ハッセ・モンゴメリ法）
  - **符号付き**: 修正された除算マジック（Hacker's Delight 第10章）
  - **2の冪**: `(/ x (expt 2 k))` → `SAR/SHR` のみ
  - `(mod x c)` も同様に `x - c * (x / c)` として乗算化
- **根拠**: GCC/LLVM が -O1 以上で必ず実行。除算命令は最も遅いスカラー算術命令の一つ
- **難易度**: Medium

#### FR-686: Dead Loop Elimination (デッドループ除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - ループが副作用を持たず、ループの結果が使用されない場合にループ全体を除去
  - 副作用の判定: FRループ内の全命令が `vm-add`/`vm-const`/`vm-move` 等の純粋命令のみで構成される場合
  - 純粋性解析（FR-675）の結果を利用してループ本体の副作用を判定
  - `(dotimes (i 1000000) (+ i i))` のような計測ベンチマークで誤って最適化されないよう、`volatile` 相当マーカーを尊重
- **根拠**: GCC `-fdelete-null-pointer-checks` 等の null 効果ループ除去 / LLVM `LoopDeletion`
- **難易度**: Easy

#### ✅ FR-687: Algebraic Simplification (代数的単純化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 現状の定数畳み込みを超えた代数恒等式の系統的適用
  - `(+ x 0)` → `x`、`(* x 1)` → `x`、`(* x 0)` → `0`、`(- x x)` → `0`（現状は部分的に実装済み）
  - `(ash x 0)` → `x`、`(logand x -1)` → `x`、`(logand x 0)` → `0`
  - `(not (not x))` → `x`、`(if (not cond) a b)` → `(if cond b a)`
  - `(expt x 2)` → `(* x x)`、`(sqrt (expt x 2))` → `(abs x)`（型が非負の場合）
- **根拠**: LLVM `InstCombine`. 個々は自明だが網羅的に適用すると後続パスの入力が大幅に単純化される
- **難易度**: Easy

---

### Phase 109 — ターゲット・プラットフォーム補完

#### FR-690: LLVM IR Backend (LLVMバックエンド)

- **対象**: 新規 `packages/backend/emit/src/llvm-ir.lisp`, `packages/foundation/mir/src/target.lisp`
- **内容**:
  - MIR → LLVM IR テキスト形式（`.ll` ファイル）または bitcode（`.bc`）を出力
  - LLVM の全最適化パス・全ターゲット（x86-64/AArch64/RISC-V/MIPS/SPARC/wasm）を活用可能に
  - `llc` に渡してネイティブバイナリ生成 or `lli` でJIT実行
  - `!dbg` メタデータで DWARF 情報をそのまま LLVM に渡す
  - **戦略的意味**: cl-cc 独自バックエンドの不完全な部分を LLVM に委譲でき、フルネイティブ動作への最短経路
- **根拠**: Emscripten / Kotlin Native / Crystal の LLVM IR バックエンド. 未対応ターゲットへの即時対応
- **難易度**: Hard

#### FR-691: Mach-O Universal Binary / Fat Binary (ユニバーサルバイナリ)

- **対象**: `packages/backend/binary/src/macho.lisp`
- **内容**:
  - x86-64 と AArch64 の両バイナリを単一 `.dylib` / 実行可能ファイルに格納する Fat Binary 形式
  - `FAT_MAGIC` (0xcafebabe) ヘッダ + `fat_arch` エントリ × 2（x86-64 + arm64）
  - `lipo` 相当の統合処理を cl-cc コンパイラ内で実行
  - Apple Silicon 移行期（2020-2026）の macOS で x86-64 Rosetta2 互換を維持しながら arm64 最適化を提供
- **根拠**: Apple Mach-O Runtime ABI. macOS 向け配布バイナリの標準形式
- **難易度**: Medium

#### FR-692: BTF (BPF Type Format) — eBPF型情報

- **対象**: `packages/backend/emit/src/ebpf-codegen.lisp` (FR-654と連携)
- **内容**:
  - eBPF プログラム（FR-654）の型情報を `.BTF` セクションに埋め込み
  - BTF 型エントリ: `BTF_KIND_INT`, `BTF_KIND_STRUCT`, `BTF_KIND_FUNC`, `BTF_KIND_DATASEC`
  - `bpftool prog dump` でソースレベルの型情報付きダンプが可能に
  - CO-RE (Compile-Once Run-Everywhere): BTF リロケーション（`BPF_CORE_*`）で異なるカーネルバージョン間でバイナリ互換
- **根拠**: Linux BTF specification / libbpf CO-RE. eBPF の主要デプロイ手法は CO-RE が標準化（2022+）
- **難易度**: Hard

#### FR-693: ARM MTE (Memory Tagging Extension) — メモリタグ

- **対象**: `packages/backend/emit/src/aarch64-codegen.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - ARMv8.5-A MTE: ポインタの上位4ビットにタグを埋め込み、アロケーション時にメモリにもタグを付与
  - `IRG` (Insert Random Tag) でアロケーションごとにランダムタグ生成
  - `STGM` / `LDG` でタグ付きメモリ読み書き
  - タグ不一致時に `SIGSEGV` / `SIGBUS`（非同期検出モードでは精度低いが性能オーバーヘッドほぼゼロ）
  - ASan（FR-554）と比較: MTE はハードウェア実装のため **実運用バイナリで常時有効化可能**
- **根拠**: ARM MTE Architecture Specification (ARMv8.5-A). Google Android 15 / Samsung Galaxy S24 以降で HW 搭載済み
- **難易度**: Hard

#### FR-694: ELF W^X Enforcement (ELF W^X 強制)

- **対象**: `packages/backend/binary/src/elf.lisp`
- **内容**:
  - FR-467 は Mach-O の W^X のみ。ELF 側も対応
  - `PT_GNU_STACK` プログラムヘッダで実行可能スタックを禁止（`PF_X` フラグなし）
  - `.text` セグメント: `PF_R | PF_X`（書き込み不可）
  - `.data` / `.bss` セグメント: `PF_R | PF_W`（実行不可）
  - JIT コード用には別途 `mmap(PROT_READ|PROT_EXEC)` / `mprotect` でページ単位に管理（書き込み後に保護切り替え）
- **根拠**: Linux カーネル NX bit / PaX W^X. 書き込み可能かつ実行可能なメモリを排除
- **難易度**: Easy

#### FR-695: PowerPC / POWER10 Backend

- **対象**: 新規 `packages/backend/emit/src/ppc64-codegen.lisp`, `packages/foundation/mir/src/target.lisp`
- **内容**:
  - IBM POWER10 (2021) 向け PPC64LE 命令セット
  - `MMA` (Matrix Math Assist) アクセラレータ命令: `xvf64gerpp`, `pmxvf64ger` 等
  - ELFv2 ABI (Little-Endian): `r3`-`r10` 引数レジスタ、`f1`-`f13` FP 引数
  - VSX (Vector Scalar eXtension) 64 × 128-bit ベクタレジスタ
  - IBM Cloud / HPC / PostgreSQL サーバーの主要 ISA
- **根拠**: IBM OpenPOWER ISA 3.1 (POWER10). AIX / Linux on POWER でのサーバーサイドCL需要
- **難易度**: Very Hard

---

### Phase 110 — プロファイリング・観測可能性補完

#### FR-700: Heap Profiler / Allocation Site Tracking (ヒーププロファイラ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - すべてのヒープ割り当てにコールサイト情報（ソースファイル:行番号）を記録するプロファイリングモード
  - `--heap-profile` フラグで有効化。割り当てのサンプリングレート（1/N）を設定可能
  - 出力形式: `pprof` 互換の protobuf 形式（`heap.pb.gz`）/ Brendan Gregg の折り畳みスタックテキスト形式
  - ライブヒープダンプ: `SIGUSR1` を受けて現在の全生存オブジェクトのスナップショットを出力
- **根拠**: Google pprof heap profiler / jemalloc heap profiling. メモリ使用量の最適化に必須
- **難易度**: Medium

#### FR-701: Continuous Profiling Integration (継続的プロファイリング統合)

- **対象**: `pipeline/src/pipeline.lisp`, `cli/src/main.lisp`
- **内容**:
  - **eBPF ベース**: `perf_event_open` + eBPF でランタイムオーバーヘッドなしに本番環境でサンプリング
  - **Pyroscope / parca 互換**: OpenTelemetry Profiling Signal（OTEP-0239, 2024 draft）形式で継続的プロファイルを送信
  - **JFR (JDK Flight Recorder) スタイル**: 低オーバーヘッド（< 2%）のオンザフライイベントバッファリング
  - プロファイルデータを FR-525（AutoFDO）にリアルタイムフィードバック
- **根拠**: Google Cloud Profiler / Elastic Universal Profiler. 2024-2026年の可観測性スタンダード
- **難易度**: Hard

#### FR-702: Flame Graph Generation (フレームグラフ生成)

- **対象**: `cli/src/main.lisp`
- **内容**:
  - `perf script` 出力または独自サンプリングデータから Brendan Gregg フレームグラフ形式の SVG を生成
  - `--flamegraph=output.svg` フラグでワンコマンド生成
  - ホット関数の色分け（赤=CPU ホット、青=GC、橙=JIT コンパイル時間）
  - 差分フレームグラフ（before/after 比較）のサポート
- **根拠**: Brendan Gregg FlameGraph (2013). プロファイリング結果の最も直感的な可視化手法
- **難易度**: Easy

#### FR-703: Compiler Self-Profiling / Build Analytics (コンパイラ自己プロファイリング)

- **対象**: `pipeline/src/pipeline.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `--time-passes` フラグ: 各最適化パスの CPU 時間・GC 回数・IR 命令数変化を計測
  - `--stats` フラグ: 各パスの統計（インライン化回数、定数畳み込み回数、除去した命令数等）を集計出力
  - `--trace-emit` フラグ: 各関数のコード生成過程を段階的に出力（最適化バグ調査用）
  - Chrome Tracing 形式（`trace.json`）で可視化可能なタイムライン出力
- **根拠**: LLVM `--time-passes` / GCC `-ftime-report`. コンパイル速度ボトルネックの特定に必須
- **難易度**: Easy

#### FR-704: SBOM (Software Bill of Materials) / Reproducible Build (再現可能ビルド)

- **対象**: `packages/backend/binary/src/elf.lisp`, `packages/backend/binary/src/macho.lisp`, `cli/src/main.lisp`
- **内容**:
  - **Reproducible Build**: 同一入力から常に同一バイナリを生成。タイムスタンプ・ランダム値・スタックアドレス等の非決定的要素を排除
  - **SBOM 生成**: SPDX / CycloneDX 形式でバイナリの依存関係（ソースファイル、stdlib バージョン）を出力
  - ELF `.note.gnu.build-id`: ソースハッシュからの決定的ビルドIDを埋め込み
  - Debian/Red Hat の再現可能ビルドプロジェクトとの互換性
- **根拠**: CISA SBOM mandate (2021) / Linux Foundation reproducible-builds.org. サプライチェーンセキュリティの2024年要件
- **難易度**: Medium

---

### Phase 111 — モダンIR表現

#### FR-710: E-graphs / Equality Saturation (等価性飽和)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/foundation/mir/src/mir.lisp`
- **内容**:
  - **E-graph**: 複数の等価な式を単一グラフ構造で表現。`(* 2 x)` と `(+ x x)` と `(ash x 1)` が同一 e-class に共存
  - **Equality Saturation**: rewriting ルール（`x * 2 → x << 1`, `x + 0 → x` 等）を収束まで全適用し、最後にコスト関数で最良表現を選択
  - 従来のフェーズ順依存問題（「定数畳み込みの後にCSEを走らせると、順序が逆だと取り逃す」）を根本解決
  - `egg` (e-graphs good) ライブラリのアルゴリズムを CL で実装
  - ルール記述: `(defrule (* x 2) (ash x 1))` のような宣言的形式
- **根拠**: `egg` paper (POPL 2021) / Cranelift e-graphs integration (2023). LLVM の次世代最適化フレームワークとして注目
- **難易度**: Very Hard

#### FR-711: Sea of Nodes IR (ノードの海IR)

- **対象**: 新規 `packages/backend/emit/src/son-ir.lisp`, `packages/foundation/mir/src/mir.lisp`
- **内容**:
  - V8 Turbofan / HotSpot C2 で採用される IR。基本ブロックを持たず全命令がデータ・制御依存グラフのノード
  - **利点**: 命令スケジューリング・CSE・LICM が「グラフの位置」として統一的に表現。フェーズ分離が不要
  - データ依存エッジ（値フロー）と制御依存エッジ（実行順序）を明示的に分離
  - `Region` ノード（基本ブロック合流）、`Phi` ノード（SSAφ関数）、`Projection` ノード（複数戻り値）
  - スケジューリング: 各ノードをその依存関係を満たす最も遅い位置（early/late schedule）に配置
- **根拠**: Cliff Click "A Simple Graph-Based IR" (1995) / V8 Turbofan. 現代JITコンパイラの主流IR設計
- **難易度**: Very Hard

#### FR-712: MLIR Integration (MLIRマルチレベルIR統合)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - LLVM MLIR の dialect システム: 複数の抽象度レベルのIRを同一フレームワークで扱う
  - **cl-cc 用 dialect**: `cl-cc` dialect（高レベル CL 演算）→ `linalg` dialect（配列演算）→ `llvm` dialect（低レベル）の段階的 lowering
  - `mlir-opt` ツールとのパイプライン統合で MLIR の最適化パスを流用
  - `arith` dialect を通じた整数/浮動小数点最適化パスの再利用
  - **Polyhedral**: `affine` dialect で FR-513（多面体最適化）を MLIR 上で実現
- **根拠**: LLVM MLIR (2019, production 2021+). TensorFlow/PyTorch のバックエンドとして標準化
- **難易度**: Very Hard

#### ✅ FR-713: Critical Edge Splitting (クリティカルエッジ分割)

- **対象**: `packages/engine/optimize/src/cfg.lisp`, `packages/foundation/mir/src/mir.lisp`
- **内容**:
  - **クリティカルエッジ**: 複数の後継を持つブロックから複数の前任者を持つブロックへのエッジ
  - PRE（FR-680）・φ挿入・コードモーションの多くはクリティカルエッジが存在すると正確な変換ができない
  - クリティカルエッジを検出し、空の「スプリットブロック」を挿入して分割
  - LCSSA（Loop-Closed SSA）形式への変換前提条件としても必要
- **根拠**: SSA 構築の前提条件として GCC/LLVM が必ず実行。FR-680 PRE・FR-510 SCCP の前処理
- **難易度**: Easy

#### FR-714: Loop-Closed SSA Form (LCSSA)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - ループから外に出る値を全てループ出口で φ ノードを介するよう変換
  - LCSSA 後は「ループ内の値参照はループ内のみ」という不変条件が成立し、ループ最適化の解析が単純化
  - ループアンロール（FR-601）・ベクトル化（FR-226）・強度削減（FR-681）の前提条件
  - FR-713（クリティカルエッジ分割）後に適用
- **根拠**: LLVM `formLCSSA`. ループ最適化パス群の標準前処理
- **難易度**: Medium

---

### Phase 112 — コード品質最適化補完

#### ✅ FR-720: Tail Merging / Cross-Jumping (末尾マージ・クロスジャンプ)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/optimize/src/cfg.lisp`
- **内容**:
  - 複数の基本ブロックの末尾が同一命令列で終わる場合、共通部分を単一ブロックに統合
  - `(if cond (progn a b ret) (progn c b ret))` → `b` と `ret` を共有ブロックに引き出す
  - **クロスジャンプ**: 異なる場所の同一命令列をバックトラッキングでマッチングして統合
  - コードサイズ削減に特に有効（FR-607 ICF と相補的; ICF は関数全体、Tail Merging は関数内）
- **根拠**: GCC `-fcrossjumping` / LLVM `SimplifyCFG`. -O2 デフォルトパス
- **難易度**: Medium

#### ✅ FR-721: Macro-Fusion Awareness (マクロフュージョン認識)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, FR-068（命令スケジューリング）と連携
- **内容**:
  - 現代 x86-64 CPU（Haswell 以降）は `CMP`/`TEST` + `Jcc` の連続ペアを単一 μop にフュージョン
  - 命令スケジューリング時に `CMP`+`Jcc` ペアを隣接配置し、フュージョン機会を最大化
  - `CMP`/`TEST` と `Jcc` の間に無関係な命令を挿入しない（フュージョンを壊す）
  - AArch64: `CBZ`/`CBNZ`/`TBZ`/`TBNZ` は compare+branch の融合命令として直接エミット（現状は `CMP` + `B.EQ` の2命令）
- **根拠**: Intel Optimization Reference Manual §2.4 / ARM Cortex-X4 optimization guide
- **難易度**: Easy

#### ✅ FR-722: Rotate Instruction Recognition (ローテート命令認識)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - `(logior (ash x n) (ash x (- n width)))` パターンを `ROL`/`ROR` 命令に変換
  - x86-64: `ROL r64, cl` / `ROL r64, imm8`
  - AArch64: `ROR Xd, Xn, imm6` (alias: `EXTR Xd, Xn, Xn, imm6`)
  - 暗号・ハッシュ関数（SHA-256, ChaCha20 等）で頻出するパターン
- **根拠**: GCC/LLVM rotate pattern matching. 多くの暗号ライブラリがこの変換に依存
- **難易度**: Easy

#### FR-723: Load Widening / Store Coalescing (ロード幅拡大・ストア統合)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - **ロード幅拡大**: 隣接アドレスへの複数の狭いロードを単一幅広ロードに統合
  - **ストア統合**: 隣接アドレスへの複数ストアを `MOVDQU`/`STP`（AArch64）等で一括書き込み
  - `(setf (aref arr 0) a (aref arr 1) b ...)` → `MOVQ [arr], packed_value`
  - エイリアス解析（FR-512）との整合性チェック必須
- **根拠**: LLVM `MemCpyOpt` / GCC memory access combining. メモリ帯域利用率を改善
- **難易度**: Medium

#### FR-724: Constant Pool / Literal Deduplication (定数プール重複除去)

- **対象**: `packages/backend/binary/src/elf.lisp`, `packages/backend/binary/src/macho.lisp`
- **内容**:
  - 同一の定数値（FP 定数、文字列リテラル）を単一アドレスにマージ
  - `.rodata` / `__const` セクションで同一バイト列を重複なく格納
  - ELF `SHF_MERGE | SHF_STRINGS` フラグで文字列セクションをリンカが自動マージ
- **根拠**: LLD/GNU ld の `.rodata` merging / LLVM constant merging pass
- **難易度**: Easy

#### ✅ FR-725: Parallel Copy Sequentialization (並列コピー逐次化)

- **対象**: `packages/backend/emit/src/regalloc.lisp`
- **内容**:
  - SSA 分解（φ関数除去）時に生成される並列コピー `{r1←r2, r2←r3, r3←r1}` を依存関係を壊さずに逐次コピー列に変換
  - **サイクルなし**: トポロジカルソートで逐次化
  - **サイクルあり**: 一時レジスタまたは `XCHG` 命令を使ってサイクルを解消
- **根拠**: Braun et al. SSA deconstruction algorithm. 正しさに関わる基礎処理
- **難易度**: Medium

#### ✅ FR-726: Algebraic Simplification (代数的単純化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(+ x 0)` → `x`、`(* x 1)` → `x`、`(* x 0)` → `0`、`(- x x)` → `0`
  - `(ash x 0)` → `x`、`(logand x -1)` → `x`、`(logand x 0)` → `0`
  - `(not (not x))` → `x`、`(if (not cond) a b)` → `(if cond b a)`
  - `(expt x 2)` → `(* x x)`、`(expt 2 n)` → `(ash 1 n)`
- **根拠**: LLVM `InstCombine`. 個々は自明だが網羅的に適用すると後続パスの入力が大幅に単純化される
- **難易度**: Easy

---

### Phase 113 — GCライフサイクル管理

#### FR-730: Weak References (弱参照)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - `make-weak-pointer` / `weak-pointer-value` の実装: GC がオブジェクトを回収すると弱参照が `NIL` に
  - GC マークフェーズ後に弱参照テーブルをスキャンし、非マークオブジェクトへの弱参照を NIL 化
  - 弱参照テーブル（`*weak-references*`）をGCルートとは別に管理
- **根拠**: SBCL `make-weak-pointer`. キャッシュ・メモ化テーブルの自動クリーンアップに必要
- **難易度**: Medium

#### FR-731: Ephemerons (エフェメロン)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**:
  - キーが生存している間だけ値が生存する特殊な弱参照ペア
  - `(make-ephemeron key value)`: key が到達不能になると value も回収対象に
  - 弱参照（FR-730）との違い: キーと値の生存を正確に連動させ、弱ハッシュテーブルでの誤回収を防止
  - GC マーキングに固定点反復が必要: エフェメロンのキーが他のエフェメロンの値から到達可能な場合
- **根拠**: Hayes (1997) "Ephemerons: A new finalization mechanism". MOP の `make-instance` でのクラス・インスタンス関係に有用
- **難易度**: Hard

#### FR-732: Finalizers (ファイナライザ)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - `(register-finalizer obj fn)`: `obj` が GC 回収される直前に `fn` を呼び出す
  - ファイナライズキュー: GC が対象オブジェクトを発見したらキューに追加、回収を1GCサイクル延期
  - 別スレッドまたはGC後にキューを処理してファイナライザ関数を実行
- **根拠**: Boehm-Demers-Weiser GC finalizers. OS リソース（ファイルディスクリプタ、外部メモリ）の確実な解放
- **難易度**: Hard

#### FR-733: Object Pinning for FFI (FFI用オブジェクトピン留め)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**:
  - コンパクションGC（FR-621）実装後、移動するとC関数に渡したポインタが無効になる
  - `(with-pinned-object (obj) (foreign-call fn (pointer-to obj)))` 構文でGC移動を一時禁止
  - ピン留めオブジェクトリストを GC に通知し、マーク-コンパクト時にスキップ
- **根拠**: .NET GCHandle.Pinned / JVM JNI GetPrimitiveArrayCritical. コンパクションGCとFFIの共存に必須
- **難易度**: Hard

#### FR-734: Weak Hash Tables (弱ハッシュテーブル)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/hash.lisp`
- **内容**:
  - `(make-hash-table :weakness :key)` / `:value` / `:key-and-value` / `:key-or-value` の4モード
  - GC マーク後にハッシュテーブルをスキャンし、キー非生存エントリを自動削除
  - MOP の `find-class` テーブル・メソッドキャッシュ（FR-009 IC）の自動クリーンアップに活用
- **根拠**: SBCL `make-hash-table :weakness`. インターン済みオブジェクトキャッシュのメモリリーク防止
- **難易度**: Medium

---

### Phase 114 — 高度な並行性モデル

#### FR-740: Software Transactional Memory (STM)

- **対象**: `packages/engine/vm/src/vm.lisp`, 新規 `packages/backend/runtime/src/stm.lisp`
- **内容**:
  - `(atomically body)` ブロック内の全メモリ操作をトランザクションとして実行。衝突時は自動リトライ
  - **TVar** (Transactional Variable): `(make-tvar initial)` / `(read-tvar tv)` / `(write-tvar tv val)`
  - 楽観的実行: ログに変更を記録し、コミット時に衝突がないか検証
  - Haskell STM 互換の `retry` / `orElse` をサポート
- **根拠**: Harris et al. "Composable Memory Transactions" (2005) / Haskell STM. ロック地獄を回避する宣言的並行性
- **難易度**: Very Hard

#### FR-741: Async/Await / Stackless Coroutines (非同期コルーチン)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**:
  - `(async (defun fetch (url) (await (http-get url))))` 構文で非同期関数を定義
  - CPS 変換（`packages/engine/compile/src/cps.lisp`）を活用: `await` 点で継続を切り取り、イベントループに登録
  - スタックレス: 継続をヒープ上の状態機械として表現
  - Python `asyncio` / Rust `async-std` / JavaScript `Promise` 互換のランタイムモデル
- **根拠**: CPS 変換は async/await の意味論的基盤。cl-cc の CPS 基盤を直接流用可能
- **難易度**: Hard

#### FR-742: Work-Stealing Scheduler (ワークスティーリングスケジューラ)

- **対象**: 新規 `packages/backend/runtime/src/scheduler.lisp`
- **内容**:
  - 並列コンパイル（FR-632）・並行GC（FR-620）・並列タスクの共通スケジューラ基盤
  - 各スレッドがローカルデキューを持ち、空になったら他スレッドのキューから末尾を盗む
  - **Chase-Lev アルゴリズム**: ロックフリーなワークスティーリングデキュー
- **根拠**: Blumofe & Leiserson (1999) / Go runtime / Java ForkJoinPool
- **難易度**: Hard

#### FR-743: Green Threads / Fibers (グリーンスレッド)

- **対象**: 新規 `packages/backend/runtime/src/fiber.lisp`
- **内容**:
  - OS スレッドよりはるかに軽量な M:N スレッドモデル
  - スタックは初期 8KB → 需要に応じて成長（segmented stack または guard page + stack copying）
  - `(cl-cc:spawn (lambda () body))` API でグリーンスレッド生成
  - Erlang プロセス / Go goroutine / Rust tokio task に相当
- **根拠**: Erlang's lightweight process model. CL の高並行サーバー（Hunchentoot 等）に必要
- **難易度**: Very Hard

---

### Phase 115 — 形式検証・スーパー最適化

#### FR-750: Superoptimization (スーパー最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - 短い命令列（3-5命令）に対して網羅的探索または確率的探索で最適な等価命令列を発見
  - **Stochastic Superoptimization** (Schkufza et al. 2013): MCMC でランダム変異し、SMT ソルバ（Z3）で等価性を検証
  - 生成したルールを `packages/prolog/prolog/src/prolog.lisp` の Prolog ファクトとして登録
- **根拠**: GNU Superoptimizer (1992) / LLVM STOKE. 手書きでは発見できない命令削減を自動発見
- **難易度**: Very Hard

#### FR-751: Abstract Interpretation Framework (抽象解釈フレームワーク)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/type/type/src/` 型システム
- **内容**:
  - Cousot & Cousot (1977) の抽象解釈: プログラムの意味を具体域からガロア接続で写した抽象域で近似
  - **interval domain**: 値範囲解析（FR-610 VRP の理論的基盤）
  - **sign domain**: 正/零/負の3値ドメイン
  - **type domain**: CL 型の格子（`fixnum ⊑ integer ⊑ number ⊑ t`）
  - `packages/prolog/prolog/src/prolog.lisp` の Prolog エンジンを抽象解釈エンジンの基盤として流用可能
- **根拠**: Cousot & Cousot. Astree (Airbus A380 コード検証). 型安全性・数値範囲の静的保証
- **難易度**: Very Hard

#### FR-752: Translation Validation (翻訳検証)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/foundation/mir/src/mir.lisp`
- **内容**:
  - 最適化前後の IR が同等の意味を持つかを SMT ソルバで自動検証（LLVM Alive2 相当）
  - 各最適化ルール（`opt-pass-*`）に事前条件・事後条件を記述し、Z3 FFI で妥当性を確認
  - `--verify-transforms` フラグで有効化（開発・テスト時のみ）
- **根拠**: LLVM Alive / Alive2 (Nuno P. Lopes et al.). LLVM のバグを多数発見した実績
- **難易度**: Very Hard

#### FR-753: Compiler Fuzzing / Differential Testing (コンパイラファジング)

- **対象**: `tests/` フレームワーク
- **内容**:
  - ランダムな CL フォームを生成し、インタープリタ実行とコンパイル実行の結果を比較
  - AFL++ スタイルのカバレッジガイドファジング: 新しいコードパスを探索
  - 既存の `framework-fuzz.lisp` を拡張して最適化パスのコレクトネス検証に使用
- **根拠**: CSmith (Yang et al. 2011). GCC/LLVM で多数のバグを発見した実績
- **難易度**: Hard

---

### Phase 116 — ツーリング・IDE統合

#### FR-760: Language Server Protocol (LSP)

- **対象**: 新規 `src/tooling/lsp-server.lisp`
- **内容**:
  - LSP 3.17 準拠のサーバー実装。JSON-RPC over stdio / TCP
  - **補完** (`textDocument/completion`): CLOS スロット名・関数名・パッケージ名
  - **定義ジャンプ** (`textDocument/definition`): `defun`/`defclass`/`defmacro` の定義位置へ
  - **ホバー** (`textDocument/hover`): 型推論結果・引数リスト・docstring
  - **診断** (`textDocument/publishDiagnostics`): コンパイルエラー・型エラーをリアルタイム表示
- **根拠**: VS Code / Emacs lsp-mode / Neovim Mason. 2024年のコンパイラは LSP なしでは IDE に採用されない
- **難易度**: Hard

#### FR-761: Debug Adapter Protocol (DAP)

- **対象**: 新規 `src/tooling/dap-server.lisp`
- **内容**:
  - DAP 1.51 準拠。VS Code / Emacs dape / Neovim nvim-dap と統合
  - ブレークポイント・ステップ実行・変数検査・`evaluate` リクエスト（停止フレームで式評価）
  - DWARF デバッグ情報（FR-550）と統合してソースレベルデバッグを実現
- **根拠**: Microsoft Debug Adapter Protocol. Emacs の `sldb` の標準化版
- **難易度**: Hard

#### FR-762: REPL Enhanced (REPL高度化)

- **対象**: `cli/src/main.lisp`
- **内容**:
  - Tab 補完・構文ハイライト・多行入力・ヒストリ永続化（`~/.cl-cc_history`）
  - `:inspect obj` コマンドでオブジェクト構造を対話的に探索
  - エラー時に `sldb` スタイルの再起動オプションメニューを表示
- **根拠**: SBCL REPL / SLY / Clojure nREPL. CL の最大の強みはインタラクティブ開発
- **難易度**: Medium

#### FR-763: Quicklisp / ASDF Integration (パッケージマネージャ統合)

- **対象**: `cli/src/main.lisp`, `cl-cc.asd`
- **内容**:
  - `./cl-cc install hunchentoot` → Quicklisp から依存関係を取得して cl-cc でコンパイル
  - ASDF システム定義（`.asd`）を解析して依存グラフを構築、インクリメンタルコンパイル（FR-640）と統合
  - `./cl-cc build --system myapp` で ASDF システム全体をネイティブバイナリにコンパイル
- **根拠**: Cargo / npm の使い勝手を CL エコシステムで実現
- **難易度**: Hard

---

### Phase 117 — セキュリティ補完

#### ✅ FR-770: Shadow Call Stack (SCS) — AArch64

- **対象**: `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - リターンアドレスを通常スタックとは別の「シャドウスタック」（X18 レジスタが指す専用ページ）にも保存
  - ✅ 実装済み: プロローグで `STR LR, [X18], #8`、エピローグで `LDR X17, [X18, #-8]!` → `CMP X17, LR` → `B.EQ` → `BRK` → `RET` を生成
  - ARM GCS（FR-531）が使えない ARMv8.0 デバイスでの代替
- **根拠**: Clang Shadow Call Stack specification. Cortex-A55 等での ROP 防御
- **難易度**: Easy

#### FR-771: SafeStack (セーフスタック)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **内容**:
  - スタックフレームを「安全スタック」（リターンアドレス・フレームポインタ）と「危険スタック」（可変長バッファ）に分割
  - 危険スタックは TLS（FR-630）経由で別ページに配置し、バッファオーバーフロー時のリターンアドレス上書きを防止
  - Clang `-fsanitize=safe-stack` 相当
- **根拠**: Kuznetsov et al. "Code-Pointer Integrity" (OSDI 2014). スタックカナリア（FR-532）より強力
- **難易度**: Medium

#### FR-772: Execute-Only Memory / XOM (実行専用メモリ)

- **対象**: `packages/backend/emit/src/aarch64-codegen.lisp`, `packages/backend/binary/src/elf.lisp`
- **内容**:
  - コードページを実行専用に設定し、データとして読み取れなくする
  - AArch64: `mprotect(PROT_EXEC)` のみでユーザー空間のコードページ読み取りを禁止
  - x86-64: `PKU (Protection Keys for Userspace)` で `.text` に実行専用鍵を設定
  - JIT スプレー攻撃・ROP ガジェット探索を困難に
- **根拠**: Apple iOS/ARM64 XOM enforcement (iOS 12+)
- **難易度**: Medium

#### FR-773: Control Flow Guard for Windows (CFG — Windows)

- **対象**: `packages/backend/binary/src/pe.lisp` (FR-292と連携)
- **内容**:
  - Windows 8.1 以降の CFG: 間接呼び出しの前に `_guard_check_icall` を挿入
  - PE ヘッダの `IMAGE_GUARD_CF_INSTRUMENTED` フラグを設定
  - `.gfids` テーブルに合法な呼び出しターゲットを列挙
- **根拠**: Microsoft Control Flow Guard specification. Win32 アプリ配布時に Microsoft 推奨
- **難易度**: Medium

---
