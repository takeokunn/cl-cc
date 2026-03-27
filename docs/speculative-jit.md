# Speculative Optimization & JIT

Speculative optimization, inline caching, JIT compilation, PGO, and profiling infrastructure.

---

### Phase 18 — LTO・PGO・コンパイラ速度

#### FR-102: LTO Whole-Program Call Graph

- **対象**: コンパイルパイプライン全体
- **内容**: リンク時の全プログラム呼び出しグラフ構築による手続き間解析
- **難易度**: Hard

#### FR-103: Cross-Module Constant Folding

- **対象**: コンパイルパイプライン
- **内容**: `defconstant`/`defvar` のリテラル初期値をモジュール境界を越えて伝播
- **難易度**: Medium

#### FR-104: PGO Edge Profiling Instrumentation

- **対象**: `src/optimize/cfg.lisp` + バックエンド
- **内容**: CFGエッジにカウンタを挿入して実行頻度プロファイルを収集
- **難易度**: Medium

#### FR-105: Profile-Guided Inlining Thresholds

- **対象**: `src/optimize/optimizer.lisp:729-820`
- **内容**: プロファイルデータに基づいてcall-site毎にインライン化コスト閾値を調整
- **依存**: FR-104
- **難易度**: Low (FR-104完了後)

#### FR-106: Parallel File Compilation

- **対象**: `src/cli/main.lisp`
- **内容**: 依存関係のないソースファイルを並列コンパイル (`./cl-cc selfhost` 84ファイルの高速化)
- **難易度**: Medium

#### FR-107: Incremental Recompilation with Dependency Graph

- **対象**: コンパイルパイプライン
- **内容**: ソースハッシュ/タイムスタンプによる変更ファイルのみ再コンパイル
- **根拠**: 現状は毎回全84ファイルを再コンパイル
- **難易度**: Medium

#### FR-108: Work-List Driven Optimizer Pass Scheduling

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 前パスで変更された命令のみを次パスで処理するwork-list方式
- **根拠**: 現状は全命令を複数回フルスキャン
- **難易度**: Medium

---

### Phase 46 — IC状態機械・型フィードバック高度化

#### FR-223: IC State Machine (Mono→Poly→Mega遷移)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-clos.lisp`
- **現状**: FR-009でMonomorphic IC、FR-023でPolymorphic IC + Megamorphic fallbackを定義しているが、状態遷移ロジック（mono→poly→mega）の明示的な状態機械定義なし
- **内容**: 各call-siteにIC状態（uninitialized/monomorphic/polymorphic/megamorphic）を付与し、ミス率に基づいて昇格・降格を制御する状態機械を実装。Megamorphic→deoptimize（VM解釈に戻す）のパス含む
- **根拠**: V8 IC state machine / HotSpot C1→C2 deoptimization。FR-009/FR-023の実装品質を制御する中核機構
- **難易度**: Hard

#### FR-224: VM Sampling Profiler (VMサンプリングプロファイラ)

- **対象**: `src/vm/vm.lisp`, `src/cli/main.lisp`
- **現状**: `defopcode`実行時の統計収集なし。ホットスポット検出にはホストCLのprofilerを使用するしかない
- **内容**: VMインタプリタループに定期的なPC（プログラムカウンタ）サンプリング挿入。命令カウンタまたは時間ベースでサンプリング。`./cl-cc run --profile` でフレームグラフ出力。FR-058（Type Feedback PGO）のデータソース
- **根拠**: V8 --prof / perf / async-profiler。自前VMの性能分析基盤としてPGO（FR-104/FR-105）の前提条件
- **難易度**: Medium

#### FR-225: Allocation Elimination via Escape Analysis (エスケープ解析による割り当て完全除去)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: FR-007でエスケープ解析、FR-018でスタック割り当て、FR-020でallocation sinkingを定義。しかし「割り当て自体の完全除去」（スカラー置換による）は明示的に定義されていない
- **内容**: エスケープ解析でローカルにのみ使用される`cons`/`list`/`vector`を検出し、SROA（FR-014）と組み合わせて個別レジスタに分解。割り当て命令・GCルート登録・ヒープアクセスを完全除去
- **根拠**: GraalVM partial escape analysis / HotSpot C2 scalar replacement。FR-007/FR-014/FR-018の統合による最大効果
- **難易度**: Hard

---

### Phase 48 — 投機的最適化基盤

#### FR-231: Stack Map Construction (スタックマップ構築)

- **対象**: `src/emit/mir.lisp`, `src/emit/regalloc.lisp`
- **現状**: MIRに`:safepoint`キーワード定義済み（`mir.lisp:13,158`）だがスタックマップデータ構造なし。GCルートの位置情報を機械語アドレスに紐づける仕組みがない
- **内容**: 各safepoint位置でのGCルートマップ（どのレジスタ/スタックスロットがGCトレース可能な参照を保持するか）を構築。`.llvm_stackmaps`互換のコンパクトなバイナリエンコーディング
- **根拠**: LLVM StackMaps / HotSpot OopMap / GraalVM ReferenceMap。正確なGC（FR-190）とdeopt（FR-155）の前提条件
- **難易度**: Hard

#### FR-232: Uncommon Trap Instructions (アンコモントラップ命令)

- **対象**: `src/emit/mir.lisp`, `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **現状**: 型ガード（`extract-type-guard` in `inference.lisp:231`）は型推論層のみ。コード生成に投機的型チェック＋失敗時の脱出パスなし
- **内容**: `vm-guard-type`/`vm-guard-fixnum`等のガード命令を追加。ガード失敗時にuncommon trapハンドラへジャンプし、FR-155のdeoptimization経路に接続。投機的型仮定の「賭け」と「保険」の分離
- **根拠**: HotSpot uncommon_trap / V8 deopt_reason / GraalVM SpeculationLog。投機的最適化の安全ネット
- **難易度**: Hard

#### FR-233: Safepoint Polling Mechanism (セーフポイントポーリング)

- **対象**: `src/runtime/gc.lisp`, `src/vm/vm.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: STW GC（`gc.lisp:200-263,331-392`）にスレッド停止要求メカニズムなし。VMインタプリタループにポーリングポイントなし
- **内容**: 関数エントリ・ループバックエッジ・アロケーションサイトにポーリングチェック挿入。ポーリングページ方式（メモリページの保護属性変更でシグナルトラップ）またはフラグチェック方式。FR-090/FR-091のsafepoint最適化の前提
- **根拠**: HotSpot polling page / Go runtime preemption / Chez Scheme interrupt check。並行GC（FR-190）の基盤
- **難易度**: Medium

---

### Phase 52 — 動的コンパイル・トレース

#### FR-244: Trace-Based Dynamic JIT (トレースベースJIT)

- **対象**: `src/vm/vm.lisp`, `src/compile/pipeline.lisp`
- **現状**: FR-154（Tiered Compilation）はAOT的な2段階コンパイル。実行時のホットパス記録・コンパイル基盤なし。FR-224（Sampling Profiler）はサンプリングであってトレース記録ではない
- **内容**: VMインタプリタループにトレース記録モードを追加。ホットループのバックエッジでトレース記録開始、ループ出口・サイドイグジットでトレース終了。記録されたトレースを型特殊化してネイティブコードにコンパイル。サイドトレース（ガード失敗時の分岐パス）の遅延コンパイル
- **根拠**: LuaJIT / TraceMonkey / PyPy。ループ中心のワークロードでインタプリタ比100x高速化の実績
- **難易度**: Very Hard

#### FR-245: Basic Block Versioning (基本ブロックバージョニング)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 型に基づくコード特殊化なし。FR-232（Uncommon Trap）はガードベースの投機だがブロック複製による多型対応なし
- **内容**: 基本ブロックの型コンテキスト（各変数の推論型の組み合わせ）毎に特殊化コピーを生成。`(if (fixnump x) (+ x 1) ...)` の真分岐ではx:fixnum版のブロックを、偽分岐では汎用版を使用。型コンテキストの爆発をN個（例: 4）に制限
- **根拠**: Chevalier-Boisvert & Feeley (2015)。ガードベース投機の代替として、静的コンパイルでも多型対応が可能
- **難易度**: Hard

---

### Phase 57 — プロファイル高度化

#### FR-261: Value Profiling (値プロファイリング)

- **対象**: `src/vm/vm.lisp`, `src/compile/pipeline.lisp`
- **現状**: FR-058（Type Feedback PGO）は型頻度カウンタのみ。実際の値（定数、範囲）の収集なし。FR-224（Sampling Profiler）はPC位置のみ
- **内容**: IC hit時に型だけでなく実際の値のヒストグラムを記録。頻出定数→定数畳み込み、値範囲→範囲解析（FR-038）にフィードバック。テーブルサイズ上限付きTop-K値記録
- **根拠**: V8 value profiling / HotSpot -XX:+ProfileReturnOnly。型プロファイリングの精密化
- **難易度**: Medium

#### FR-262: Call-Chain Profiling (コンテキスト感応プロファイリング)

- **対象**: `src/vm/vm.lisp`, `src/cli/main.lisp`
- **現状**: FR-224（VM Sampling Profiler）はフラット（PC位置のみ）。呼び出し元コンテキストを区別しない
- **内容**: サンプリング時にコールスタック上位N段（デフォルト8）を記録。呼び出しコンテキスト毎の型分布・実行頻度を収集。context-sensitive inlining（FR-053）の判定精度向上
- **根拠**: Google AutoFDO / BOLT / HotSpot -XX:+CallChainProfiling。コンテキスト無視のフラットプロファイルでは見えない最適化機会を検出
- **難易度**: Medium

#### FR-263: Allocation Site Profiling (割り当てサイトプロファイリング)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`, `src/cli/main.lisp`
- **現状**: `heap.lisp`にアロケーションサイトのトラッキングなし。どの関数/行がメモリ消費の主因か不明
- **内容**: `rt-gc-alloc`にソース位置メタデータを付与。`./cl-cc run --alloc-profile`でアロケーションサイト毎のバイト数・回数・型を出力。GCチューニング・エスケープ解析（FR-007）の優先度決定に使用
- **根拠**: Go runtime pprof alloc / Java Flight Recorder allocation profiling。メモリ最適化の起点
- **難易度**: Medium

---

### Phase 58 — 投機的JIT高度化

#### FR-267: ThinLTO (モジュール並行リンク時最適化)

- **対象**: `src/compile/pipeline.lisp`
- **現状**: FR-102（LTO Whole-Program Call Graph）は全モジュールを逐次処理するフル LTO のみ想定。モジュール数が増えるとコンパイル時間が O(N²) 増大
- **内容**: LLVM ThinLTO（2015）に相当する 2 フェーズ並行 LTO。**Phase 1**: 全モジュールから要約（関数シグネチャ・エクスポート一覧・型サマリ）を並行抽出してグローバルサマリを構築。**Phase 2**: 各モジュールをサマリのみ参照しながら並行最適化（フル IR 読み込み不要）。呼び出しグラフのエッジを超えたインライン展開はサマリベースで対象を絞り込む。フル LTO の 1/10〜1/4 のコンパイル時間で 70〜90% の効果
- **根拠**: LLVM ThinLTO paper (2016) / Swift コンパイラの標準 LTO モード。大規模プロジェクトでのフル LTO の現実的代替
- **難易度**: Hard

#### FR-264: Background / Concurrent JIT Compilation (バックグラウンドJITコンパイル)

- **対象**: `src/compile/pipeline.lisp`, `src/vm/vm-run.lisp`
- **現状**: JITコンパイル（FR-058/FR-244）はインタプリタ実行をブロックして行う。コンパイル中はVMが停止
- **内容**: JITコンパイルをバックグラウンドスレッドで実行しつつ、インタプリタ（Tier-0）が継続して動作する並行コンパイル。コンパイル完了後にコードポインタをアトミック置換（`vm-call` のディスパッチテーブルエントリを CAS で更新）。コンパイル中の関数変更（再定義）に対するキャンセル機構も必要
- **根拠**: V8 の並行コンパイラスレッド / HotSpot `-XX:+TieredCompilation` のC1/C2並行化 / JavaScriptCore DFG concurrent compilation。スループットを犠牲にしない JIT
- **難易度**: Very Hard

#### FR-265: Class Hierarchy Analysis for CLOS (CHA — クラス階層解析)

- **対象**: `src/vm/vm-clos.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `vm-generic-call` は常に実行時ディスパッチ（MOP `find-method` 相当）。メソッドが1つしか存在しない場合でも多態ディスパッチを経由する
- **内容**: コンパイル時にクラス階層グラフを構築し、`(defgeneric f)` に対して「クラス C のサブクラスが存在しない」「メソッド特化が1つだけ」の条件を静的に検証。条件成立時は `vm-generic-call` を直接呼び出し（`vm-call`）に降格。`sealed` クラスや `final` メソッドの宣言で CHA 精度向上。クラス追加時はCHA結果を無効化して再解析
- **根拠**: Java CHA (Dean et al. 1995) / GraalVM CLOS 最適化 / SBCL `(declare (optimize (safety 0)))` 下のディスパッチ除去。CLOSの主要な性能ボトルネック解消
- **難易度**: Hard

#### FR-266: Speculative Inlining with Type Guard (型ガード付き投機的インライン展開)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: インライン展開（FR-040）は静的に確定した callee のみ対象。`vm-generic-call` や高階関数はインライン不可
- **内容**: IC 型プロファイル（FR-058/FR-261）から最頻出 callee クロージャを特定し、「このオブジェクトが型 T なら callee = F である」という型ガードを挿入してインライン展開。ガード失敗時は uncommon trap（FR-159）経由でインタプリタへのデオプティマイズ。ガードが頻繁に失敗する場合は megemorphic（多態）パスへ降格
- **根拠**: V8 TurboFan speculative inlining / HotSpot `-XX:+InlineVirtualCalls`。`(mapcar fn list)` で fn が毎回同じクロージャなら完全インライン可能
- **難易度**: Hard

---

### Phase 60 — デオプティマイゼーション・OSR

#### FR-280: Structured Deoptimization Framework (構造化デオプティマイゼーション)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-run.lisp`, `src/compile/codegen.lisp`
- **現状**: FR-232（Uncommon Trap）・FR-266（型ガード）が脱出先を参照しているが、JITコード→VMインタプリタへのバイルアウト（レジスタ→VMレジスタへの逆マッピング）の実体がない
- **内容**: deopt時に現在の機械語レジスタ状態を `vm-state` に再構成する **materializer**。各deoptポイントに `deopt-frame`（対応するVM PCとレジスタマッピング表）を紐づける。eager deopt（即時バイルアウト）と lazy deopt（次のサーフェスでバイルアウト）の両方をサポート。`./cl-cc run --deopt-trace` で脱出頻度ログ出力
- **根拠**: V8 deoptimizer / HotSpot nmethod deopt / GraalVM FrameState。投機的最適化の「保険」機能の実体。FR-231（Stack Map）が前提
- **難易度**: Very Hard

#### FR-281: On-Stack Replacement — Interpreter→JIT (OSR昇格)

- **対象**: `src/vm/vm-run.lisp`, `src/compile/pipeline.lisp`
- **現状**: JITコンパイルされた関数への切り替えは関数エントリ時のみ。長時間実行中のホットループはインタプリタで動き続ける
- **内容**: ループバックエッジのバックエッジカウンタが閾値を超えたとき、ループ途中で現在のVMフレームをJITフレームに**オンスタック置換**。OSR入口ブロック（ループヘッダに対応するJIT BBへの直接ジャンプ）を生成。VM変数→JITレジスタへの変数マッピングをOSR入口点で確立
- **根拠**: HotSpot OSR / V8 OSR from Ignition / PyPy OSR。バッチ処理・数値計算系ループで最大の恩恵
- **難易度**: Very Hard

#### FR-282: OSR Exit — JIT→Interpreter (OSR降格)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-run.lisp`
- **現状**: FR-281はインタプリタ→JITの昇格のみ。型仮定が崩れた場合にループ途中でインタプリタに戻る経路がない
- **内容**: ループ内のガード失敗時にJITフレームをVMフレームに逆変換してインタプリタに再入する OSR exit。deopt frameに「どのループ反復まで完了したか」の情報を保持。FR-280（Deopt Framework）の`materializer`をOSR文脈で呼び出す
- **根拠**: V8 deopt inside loop / HotSpot uncommon_trap in compiled loop。ループ内の型仮定を安全に保護
- **難易度**: Hard

#### FR-283: Speculation Log (投機ログ)

- **対象**: `src/compile/pipeline.lisp`, `src/vm/vm.lisp`
- **現状**: 型ガード（FR-232）の失敗記録なし。同一ガードが毎回コンパイル時に同じ投機を行い、実行時に何度でも失敗し続ける可能性がある
- **内容**: `*speculation-log*` — call-site IDをキーとして「この投機は過去に失敗した」フラグを保持するグローバルテーブル。再コンパイル時にログを参照し、失敗実績のある投機は採用しない。ログはプロセス間で `.prof` ファイルに永続化可能
- **根拠**: GraalVM SpeculationLog / HotSpot replay compile。同一の有害な投機を繰り返すコンパイル・デコンパイルループ（deopt storm）を防ぐ
- **難易度**: Medium

---

### Phase 61 — オブジェクト形状・型特殊化

#### FR-284: Object Shape / Hidden Class Tracking (オブジェクト形状追跡)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **現状**: CLOSインスタンスはスロットを `(gethash :slot-name ht)` で参照するため、スロットオフセットが実行時に確定する。インライン化もフィールドアクセスの定数化も不可能
- **内容**: クラス定義時に `shape-id`（単調増加整数）を各クラスに付与し、`shape-id → slot-offset-table` の配列ルックアップでスロットアクセスを O(1) に。インスタンスにshape-idを埋め込み、アクセスコード生成時に `(guard-shape inst shape-id) (vm-slot-load inst offset)` を発行。`defclass` 再評価・スロット追加で shape-id が変わりガードが外れる
- **根拠**: V8 Hidden Classes / JSC Structures / LuaJIT table shape。ハッシュテーブルアクセスから定数オフセットアクセスへの変換でCLOS性能10〜50x向上
- **難易度**: Hard

#### FR-285: Unboxed Fixnum / Float Arithmetic (アンボックス演算)

- **対象**: `src/compile/codegen.lisp`, `src/vm/primitives.lisp`
- **現状**: 演算命令（`vm-add` 等）は実行時タグチェック→ボックス値unwrap→演算→ボックス化という手順。数値ループでボックス化コストが大きい
- **内容**: 型推論（src/type/inference.lisp）で fixnum と判定された変数は unboxed レジスタ（`reg-fixnum` タグ付きMIR値）で管理。`vm-add-fixnum`/`vm-add-float` の特殊化命令を生成し、タグチェックとボックス化を省略。オーバーフロー時は `overflow-trap` でboxed演算にフォールバック
- **根拠**: SBCL type-driven code generation / V8 Maglev unboxed Int32 / GraalVM primitive specialization。数値演算ループで2〜5x高速化
- **難易度**: Hard

#### FR-286: Tagged Integer Range Analysis (タグ付き整数範囲解析)

- **対象**: `src/type/inference.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 型システム（src/type/）は fixnum/float/cons の区別はするが、整数の値範囲（0≤n<256等）を追跡しない
- **内容**: SSA値に整数範囲アノテーション `[lo, hi]` を付与し、CFG上で前向き伝播。`(the (integer 0 255) x)` → `x ∈ [0,255]`。範囲が fixnum に収まると証明できればオーバーフローガード不要。ループインダクション変数の範囲をループ境界から導出（FR-038と統合）
- **根拠**: LLVM ScalarEvolution / V8 TurboFan range analysis / HotSpot C2 range check elimination。配列境界チェック除去の前提条件
- **難易度**: Medium

---

### Phase 62 — ループ最適化

#### FR-287: Loop Invariant Code Motion (LICM — ループ不変コード移動)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/cfg.lisp`
- **現状**: 最適化パスはフラット命令列に対して動作。CFGループ構造を利用したループ外ホイスティングなし
- **内容**: ループ検出（back-edge + dominator tree）→ループ不変命令の検出（全オペランドがループ外で定義）→プリヘッダブロックへのホイスト。副作用のある命令（ヒープ書き込み・IO）はホイスト禁止。エイリアス解析（FR-030）と連携して副作用を精密に判定
- **根拠**: 全主要コンパイラの基本最適化。`(loop for i from 0 to n sum (length fixed-list))` で `length`計算をループ外に移動
- **難易度**: Medium

#### FR-288: Loop Unrolling (ループ展開)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: ループ展開なし。小ループでもバックエッジジャンプのオーバーヘッドが残る
- **内容**: 反復回数が静的に判明するループ（`dotimes` + 定数）を N 倍展開してバックエッジ削減。反復回数不明でも `unroll-factor=4` でピーリング（残余処理）を追加した部分展開。展開後に冗長コピー命令をDCEで除去
- **根拠**: GCC -funroll-loops / LLVM LoopUnroll / SBCL。ループ本体が小さいときレジスタ使用率とILP改善
- **難易度**: Medium

#### FR-289: Loop Fusion (ループ融合)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 隣接するループ（`(dotimes (i n) ...)` が2つ連続）は独立に実行。キャッシュ効率が悪い
- **内容**: 同一反復範囲・副作用が独立な隣接ループを1つのループにマージ。依存チェック（読み書きエイリアス解析）を経てバリアフリーなループのみ融合。メモリアクセスのlocality向上でキャッシュミス削減
- **根拠**: GCC -floop-interchange / Polly / LLVM LoopFusion。メモリ帯域律速なワークロードで1.5〜3x高速化
- **難易度**: Hard

#### FR-290: Loop Peeling (ループピーリング)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: ループ先頭に条件チェックがある場合、毎反復でチェックを実行
- **内容**: ループ本体の最初の1〜2回分を「ピーリング」（ループ外に複製）。ループ先頭の境界チェック・NULL チェック・型チェックが定数畳み込みで除去できる場合に適用。残余ループは無条件で実行可能に
- **根拠**: LLVM LoopPeel / HotSpot C2。`(car (first list))` のnullチェックをピーリングで除去
- **難易度**: Medium

#### FR-291: Auto-Vectorization / SLP Vectorizer (SLPベクトル化)

- **対象**: `src/emit/mir.lisp`, `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64.lisp`
- **現状**: SIMD命令生成なし。数値ループは1要素ずつスカラー処理
- **内容**: **SLP（Superword Level Parallelism）ベクトル化**: 隣接メモリアクセス＋同種演算のスカラー命令群を SSE2/AVX2/NEON 128〜256bit SIMD 命令に置換。`(loop for i below n do (setf (aref out i) (+ (aref a i) (aref b i))))` → `VADDPS ymm0, ymm1, ymm2`。型情報（floatベクトル・fixnum配列）が必要。アライメントチェック付き
- **根拠**: LLVM SLP Vectorizer / GCC auto-vectorization / ARM NEON。数値計算で4〜16x高速化。Common Lisp の `simple-array` 操作が主なターゲット
- **難易度**: Very Hard

---

### Phase 63 — コードレイアウト最適化

#### FR-292: Hot/Cold Code Splitting (ホット/コールドコード分離)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/compile/pipeline.lisp`
- **現状**: 関数内の全基本ブロックが隣接配置。頻繁に実行されないエラー処理パスがホットパスのI$を汚染
- **内容**: プロファイルデータ（FR-104）で実行頻度の低い基本ブロック（エラーハンドラ・rare-branch）を関数末尾または別セクションに移動。ホットパスのブロックを直線的に配置してI$ミス削減。`unlikely` アノテーション（Common LispのCMU CLスタイル）で静的ヒントも受け付ける
- **根拠**: HotSpot `-XX:+ProfileCompiledMethods` / LLVM `-fprofile-use` cold section / V8 hot/cold block layout。I$利用率15〜30%改善
- **難易度**: Medium

#### FR-293: Profile-Guided Code Layout (プロファイル誘導コードレイアウト)

- **対象**: `src/compile/pipeline.lisp`, `src/binary/macho.lisp`
- **現状**: 関数の配置順はASDFロード順。呼び出し関係に基づく局所性最適化なし
- **内容**: 実行プロファイルから関数呼び出しグラフ（コールグラフ）上のエッジ重みを収集し、頻繁に連続呼び出される関数群を隣接配置。Mach-Oの`__TEXT,__text` セクション内で関数の物理的順序を最適化。`clang -forder-file` / Facebook BOLTに相当するポストリンク最適化
- **根拠**: Google AutoFDO / Meta BOLT / Apple PGO order file。大規模プログラムでのI$ミス20〜40%削減
- **難易度**: Hard

#### FR-294: Function Outlining (関数アウトライン化)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: インライン展開（FR-040）の逆操作未実装。コードサイズが増大しI$を圧迫
- **内容**: 複数箇所に重複する共通コード（エラー処理・型チェック列）を切り出して共有関数化（インライン展開の逆）。コードサイズ閾値（デフォルト32バイト）以上の重複コードを検出し `_outlined_func_N` として分離。`-Os` 相当のコードサイズ最適化モード
- **根拠**: LLVM MachineOutliner / Apple LLVM outliner (iOS バイナリサイズ削減)。I$フットプリント削減で間接的に性能改善
- **難易度**: Medium

---

### Phase 64 — 命令スケジューリング・レジスタ圧力

#### FR-295: Instruction Scheduling (命令スケジューリング)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64.lisp`
- **現状**: 命令はMIRの順序通りに発行。レイテンシ隠蔽・アウトオブオーダー活用なし
- **内容**: 依存グラフ（Def-Use チェーン）を構築し、クリティカルパスを短縮するようにリスト型スケジューラで命令を並び替え。x86-64: Alder Lake / Zen 4 のレイテンシテーブルを参照。ロードレイテンシ隠蔽（load→use の間に無関係命令を挿入）が主目的
- **根拠**: GCC `-fschedule-insns` / LLVM MachineScheduler / SBCL vop scheduling。スーパースカラーCPUでの IPC 向上
- **難易度**: Hard

#### FR-296: Register Pressure Reduction via Rematerialization (再実体化によるレジスタ圧力削減)

- **対象**: `src/emit/regalloc.lisp`
- **現状**: レジスタ枯渇時はすべてスタックにスピル（load/store）。再計算コストの低い値もスピル対象
- **内容**: スピル候補が「定数」「単純な算術式」「pure関数呼び出し（副作用なし）」のとき、レジスタに再ロードする代わりに命令を再発行（rematerialize）。スピル/リロードのメモリ帯域使用を削減。コスト関数：rematerialize cost < load + store cost の場合に採用
- **根拠**: LLVM Rematerialization / GCC REG_DEAD / Briggs et al. (1994)。スピル回数の10〜30%削減
- **難易度**: Medium

---

### Phase 65 — ML誘導最適化

#### FR-297: ML-Guided Inlining (機械学習誘導インライン展開)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/pipeline.lisp`
- **現状**: インライン化判定はヒューリスティック（コード行数・呼び出し深度・FR-105プロファイル閾値）。最適な閾値の設定はブラックアート
- **内容**: 関数ペア（呼び出し元・callee）の特徴量ベクトル（命令数、ループ深度、型特殊化度、呼び出し頻度、引数パターン）を入力に、インライン化の利益を予測する小規模 MLP モデル（隠れ層256次元、パラメータ数〜50K）。推論は `./cl-cc compile` 実行中に数μsで完了。モデルは cl-cc 自身の selfhost プロファイルで事前学習
- **根拠**: Google MLGO (2021) / Meta Inliner ML / ARM NN-guided compiler。ヒューリスティックより10〜15%コードサイズ削減＋性能向上。2024〜2026年のLLVM/GCC本流に統合済み
- **難易度**: Very Hard

#### FR-298: Feedback-Directed Optimization via Corpus PGO (コーパスPGO)

- **対象**: `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: PGO（FR-104/FR-105）はユーザー提供のプロファイルデータに依存。代表的な入力セットがない場合は効果なし
- **内容**: cl-cc 自身の selfhost 実行（84ファイル）をコーパスとして自動プロファイル収集→最適化の **bootstrap PGO**。`make pgo-build`: (1) instrumented binary でselfhost実行してプロファイル生成、(2) プロファイルを使ってrelease build。CI に統合して毎ビルドでプロファイルを更新
- **根拠**: Clang `-fprofile-generate` → `-fprofile-use` / GCC `-fprofile-generate` ワークフロー。Rustcも同様のbootstrap PGOを採用（2022〜）。通常5〜20%のコンパイル時間短縮
- **難易度**: Medium

---

### Phase 66 — セキュリティ・サンドボックス

#### FR-299: Spectre/Meltdown Mitigations in JIT (JITコードのSpectre対策)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/compile/codegen.lisp`
- **現状**: JIT生成コードにSpectre v1（境界チェックバイパス）対策なし。JIT-to-JIT の間接ジャンプにretpoline未適用
- **内容**: (1) 配列境界チェックの前に `LFENCE` を挿入して投機的実行をバリア。(2) 間接呼び出し（`vm-generic-call`、クロージャディスパッチ）を retpoline シーケンス（`call thunk; jmp` パターン）に置換。(3) JITコード領域を W^X（書き込みと実行の排他）で管理 — コード生成時はmprotect RW、実行時はRX
- **根拠**: V8 Spectre mitigations (2018) / Firefox SpiderMonkey retpoline / Chrome site isolation。サンドボックス実行環境（Wasm実行等）での必須要件
- **難易度**: Medium

#### FR-300: JIT Code Region Isolation (JITコード領域隔離)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/runtime/runtime.lisp`
- **現状**: JIT生成コードのメモリ管理（mmap/mprotect）なし。コードバッファの書き込み可能性が実行中も残る
- **内容**: JITコード専用のメモリプール（`jit-code-arena`）を `mmap(PROT_READ|PROT_EXEC)` で確保。コード書き込み時のみ `mprotect` で一時的に RW に変更し、書き込み完了後即 RX に戻す。`icache_invalidate` （Apple Silicon: `sys_icache_invalidate` / Linux: `__builtin___clear_cache`）を忘れずに呼び出す。コードキャッシュ上限設定とLRU退避
- **根拠**: V8 CodePageAllocator / JavaScriptCore JITWriteSeparation / Apple PAC signed JIT。W^X の実施はmacOS 14+ / iOS では必須
- **難易度**: Hard

---

### Phase 67 — WebAssembly JIT

#### FR-301: Tiered Wasm Compilation (段階的Wasmコンパイル)

- **対象**: `src/backend/wasm.lisp`, `src/compile/pipeline.lisp`
- **現状**: Wasm バックエンドは1段階のAOTコンパイルのみ（FR-080）
- **内容**: **Tier-0**: Wasm バイトコードを直接インタプリタ実行（低レイテンシ起動）。**Tier-1**: ホット関数を Cranelift / LLVM-MC ベースの Baseline JIT でコンパイル（最適化なし、1ms以下）。**Tier-2**: 実行頻度上位5%をオプティマイジングJIT（FR-105ベースPGO適用）でコンパイル。WasmGC（reference types, structs, arrays）に対応したGC統合
- **根拠**: V8 Liftoff→TurboFan / Firefox Baseline→Ion / Wasmtime Cranelift。Wasm 起動時間をAOTコンパイル待ちなしで提供
- **難易度**: Very Hard

#### FR-302: Wasm SIMD Code Generation (Wasm SIMD命令生成)

- **対象**: `src/backend/wasm.lisp`, `src/emit/mir.lisp`
- **現状**: Wasm バックエンドはスカラー命令のみ。Wasm SIMD 128 仕様（2022年標準化）未対応
- **内容**: MIR のベクトル値型（FR-291で追加するvec128）をWasm `v128.load` / `f32x4.add` 等の SIMD 命令にマップ。x86-64はSSE2経由、AArch64はNEON経由でWasm runtimeがSIMDを実行。ホストが SIMD 非対応の場合はスカラーフォールバック
- **根拠**: Wasm SIMD proposal (Phase 4, 2022) / Emscripten SIMD / V8 Wasm SIMD。数値ワークロードで4〜8x高速化
- **難易度**: Hard

---

### Phase 68 — ガード精錬・コードキャッシュ管理・適応的再コンパイル

#### FR-303: Guard Strength Reduction (ガード弱体化)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: FR-232（Uncommon Trap）・FR-266（型ガード付きインライン）が生成するガードはすべて同コスト（型チェック命令）。失敗率が極低くても high-cost なガードのまま
- **内容**: Speculation Log（FR-283）の失敗カウンタを参照し、ガードが N 回実行されて一度も失敗していない場合により安価な形式に降格。降格の段階: `full-type-check` → `tag-bit-test`（タグビット1命令）→ `shape-id-compare`（FR-284, 整数比較1命令）→ `nop`（完全除去、クラス階層変化時に再挿入）。ガード除去後のクラス変更・メソッド再定義時は IC 無効化（FR-265 と同メカニズム）でガードを復元
- **根拠**: V8 `--stress-compaction` guard weakening / GraalVM SpeculationLog confidence score。ホットループ内の型ガードを nop まで落とせれば数%の命令数削減
- **難易度**: Medium

#### FR-304: JIT Code Cache Eviction (JITコードキャッシュ退避)

- **対象**: `src/compile/pipeline.lisp`, `src/vm/vm-run.lisp`
- **現状**: FR-300（JIT Code Region Isolation）でコードアリーナを確保するが、上限到達時の退避ポリシー未定義。コードキャッシュが満杯になると新規コンパイルが失敗する
- **内容**: JITコードエントリに **warmth counter**（呼び出し回数の指数平滑移動平均）を付与。キャッシュ使用率が閾値（デフォルト80%）を超えたとき、warmthが最低のエントリからLRU退避。退避対象のコードポインタを `vm-call` ディスパッチテーブルから Interpreter stub に差し戻し（CAS）。退避後に再度ホットになれば再コンパイル。`./cl-cc run --jit-cache-stats` でヒット率・退避回数を出力
- **根拠**: V8 code flushing / HotSpot `-XX:ReservedCodeCacheSize` + code cache sweeper / JavaScriptCore JIT memory pressure eviction。長時間稼働プロセスでのメモリリーク防止
- **難易度**: Medium

#### FR-305: Adaptive Recompilation Thresholds (適応的再コンパイル閾値)

- **対象**: `src/compile/pipeline.lisp`, `src/vm/vm-run.lisp`
- **現状**: Tier昇格の閾値（バックエッジカウンタ・呼び出し回数）はコンパイル時定数。コールドスタート時も安定稼働時も同じ閾値を使用するため、起動直後に必要以上にコンパイルが走るか、逆にホット関数の昇格が遅延する
- **内容**: プロセス起動からの経過時間・総コンパイル時間・CPU 使用率をモニタし、Tier昇格閾値を動的に調整。**warm-up フェーズ**（起動後30秒）: 閾値を通常の1/3に下げて積極コンパイル。**安定フェーズ**: コードキャッシュ使用率が60%超なら閾値を2倍に引き上げてコンパイル抑制。**メモリ逼迫時**: Tier-2への昇格を停止しTier-1のみ稼働。閾値変更は `*jit-compilation-budget*` パラメータ経由で外部からも制御可能
- **根拠**: HotSpot `-XX:CompileThreshold` 動的調整 / V8 compilation budget / GraalVM adaptive compilation policy。起動レイテンシとスループットのトレードオフを実行時に自動調整
- **難易度**: Medium
