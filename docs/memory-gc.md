# Memory Management & GC

Garbage collection, memory management, heap optimization, and cache efficiency.

---

### Phase 5 — メモリ・GC最適化

#### FR-017: Alias Analysis / Memory Disambiguation

- **対象**: `packages/engine/optimize/src/optimizer.lisp` + `packages/engine/optimize/src/cfg.lisp`
- **内容**:
  - 2つのヒープ参照が同じオブジェクトを指しうるか解析
  - LICM・DSEの安全性判定を強化 (現状は保守的すぎる可能性)
  - Type-Based Alias Analysis (TBAA): 型情報から別オブジェクトと証明
  - 依存: FR-004 (型情報が必要)
- **効果**: LICM/DSEの適用範囲拡大、ロード巻き上げの安全性向上

#### FR-018: Stack Allocation of Non-Escaping Objects

- **依存**: FR-007 (エスケープ解析)
- **内容**:
  - エスケープしないと証明されたコンスセル・クロージャをスタックに直接配置
  - GCヒープをバイパスすることでGC停止を回避
  - SBCLが `&rest` リストと一部の `make-array` で実施しているものと同等
  - FR-007 (エスケープ解析) の「検出」を「変換」まで完結させる

#### FR-019: Write Barrier Elimination

- **依存**: FR-007, FR-018
- **対象**: `packages/backend/runtime/src/gc.lisp` + codegen
- **内容**:
  - 世代別GCにおいて old→young ポインタ書き込みに必要なwrite barrierを除去
  - エスケープ解析で「新規割り当てオブジェクトへの書き込み」と判明した場合はバリア不要
  - HotSpot C2・GHC GCが同様の最適化を実施
  - 効果: 頻繁なコンス操作ループでのGCオーバーヘッド大幅削減

#### FR-020: Allocation Sinking (割り当てシンキング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(cons a b)` 等の割り当てを可能な限り遅延させる (LICMの双対)
  - 条件分岐で片方のパスしか実行されない場合、そのパスの中へ移動
  - 高速パスでのGCプレッシャーを削減

---

### Phase 15 — GC・メモリシステム最適化

#### FR-084: Card Table Summarization

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: カードテーブルの1レベルビットマップサマリを追加し、クリーンカードのスキャンをスキップ
- **根拠**: 現状 `%gc-scan-dirty-cards` は全 `num-cards` を毎回イテレート
- **難易度**: Easy

#### FR-085: Dynamic GC Age Threshold Tuning

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: `*gc-tenuring-threshold*` をプロモーション率・旧世代占有に基づいて動的調整
- **根拠**: 現状ハードコード値3; 動的チューニングでGC品質向上
- **難易度**: Easy

#### FR-086: Large Object Space (LOS)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: 閾値超オブジェクトを独立管理領域に直接割り当てNursery迂回
- **難易度**: Easy

#### FR-087: rt-heap Field Reordering

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: `young-free` をホットフィールド群 (`young-from-base`, `young-limit`) と同一キャッシュラインに配置
- **難易度**: Easy

#### FR-088: Incremental GC Marking

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: メジャーGCのマークフェーズをミューテータ作業とインタリーブして一時停止を短縮
- **根拠**: SATB インフラが既に存在するため自然な拡張
- **難易度**: Medium

#### FR-089: Compacting Old Space

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: マーク後にスライディングコンパクションで旧世代のフラグメンテーション解消
- **難易度**: Hard

#### FR-090: Safepoint Dominance Pruning

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**: 同一ルートセットを持つ支配されたsafepointを除去
- **根拠**: MIRに `:safepoint` ノードが存在するが冗長性解析なし
- **難易度**: Medium

#### FR-091: Safepoint Hoisting to Loop Back-Edges

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**: ループボディのsafepointをバックエッジにのみ移動、ポーリング頻度削減
- **難易度**: Medium

---

### Phase 41 — GC高度化 (ピン・コンパクション)

#### FR-212: Object Pinning (オブジェクトピン)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: Cheney copying GC（`gc.lisp:200-263`）が全オブジェクトを移動。FFI呼び出し中のオブジェクトアドレス安定性保証なし
- **内容**: GCルートにピンフラグを追加し、ピンされたオブジェクトはコピー対象から除外。FFI境界（`with-pinned-objects` マクロ）でGCに通知。ピンされたオブジェクトは移動せずフラグメンテーション管理
- **根拠**: JVM JEP 423 / .NET pinned objects / SBCL with-pinned-objects。FFI安全性に必須
- **難易度**: Hard

#### FR-213: GC Compaction / Defragmentation (ヒープコンパクション)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: major GC（`gc.lisp:331-392`）はマーク&スイープのみ。`gc.lisp:158`のfree-listは未使用。解放後のフラグメンテーション対策なし
- **内容**: major GC後にスライディングコンパクションを実行。生存オブジェクトを連続配置してフラグメンテーション解消。前方参照テーブルでポインタ更新。FR-212のピンされたオブジェクト周辺はスキップ
- **根拠**: G1 GC / Shenandoah / ZGC。長時間実行コンパイルでのヒープフラグメンテーション防止
- **難易度**: Very Hard

---

### Phase 53 — GC高度化 (Ephemeron・Unwind)

#### FR-246: Ephemerons (エフェメロン)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: FR-184で弱参照・ファイナライザを定義しているが、エフェメロン（キーが到達不能になった場合のみ値も回収されるkey-valueペア）は未定義。`gc.lisp`に弱参照トラッキングなし
- **内容**: エフェメロン型を追加。GCマーキングフェーズでキーの到達可能性を判定し、キーが到達不能なら値もマークしない。固定点反復で連鎖エフェメロンを正しく処理。弱ハッシュテーブルの基盤
- **根拠**: Racket / Java WeakHashMap / JavaScript WeakRef+FinalizationRegistry。SBCL sb-ext:make-ephemeron。シンボルテーブル・キャッシュの正確なGC
- **難易度**: Hard

#### FR-247: Unwind Tables / .eh_frame Generation (アンワインドテーブル)

- **対象**: `packages/backend/binary/src/elf.lisp`, `packages/backend/binary/src/macho.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: ELF出力（`elf.lisp`）は`.text`, `.rela.text`, `.symtab`, `.strtab`, `.shstrtab`のみ。Mach-O出力にも例外テーブルセクションなし。FR-195（DWARF debug info）は定義済みだがアンワインド情報は別
- **内容**: `.eh_frame`セクション（DWARF CFI）を生成し、各関数のプロローグ/エピローグでのスタックフレームレイアウト変化を記述。外部例外ハンドラ（OS signal handler、C++ interop）がCLスタックを正しくアンワインド可能に。`__unwind_info`（compact unwind）のMach-O版も生成
- **根拠**: LSB (Linux Standard Base) / System V ABI。ネイティブデバッガ・プロファイラ連携に必須。FR-195（DWARF）の前提条件
- **難易度**: Hard

---

### Phase 63 — キャッシュ・メモリ最適化

#### FR-287: Loop Tiling / Strip Mining (ループタイリング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, 新規`packages/engine/optimize/src/loop-transforms.lisp`
- **現状**: ループ最適化はFR-003（LICM）・FR-022（Unrolling）が計画済みだが、タイリング/ブロッキングは未計画。ネストループの空間的局所性最適化なし
- **内容**: 行列演算等の多重ループをキャッシュラインサイズ（64B）に合わせたタイルに分割。`(dotimes (i N) (dotimes (j M) ...))` → タイルサイズTでブロッキング。L1/L2キャッシュ容量に基づくタイルサイズ自動決定
- **根拠**: LLVM LoopTiling / GCC `-floop-nest-optimize` / Polly。行列演算で10-100x高速化の事例
- **難易度**: Very Hard

#### FR-288: Large Page Support (ラージページサポート)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: ヒープ割り当てに標準4KBページのみ使用。`madvise`/`mmap`フラグの最適化なし
- **内容**: ヒープ領域に2MB huge page（`MAP_HUGETLB`/`VM_FLAGS_SUPERPAGE_SIZE_2MB`）を使用。`madvise(MADV_HUGEPAGE)`でTHPヒント。GCリージョンのページサイズ対応。ヒープサイズに応じた1GB huge page対応
- **根拠**: HotSpot `-XX:+UseLargePages` / V8 `--huge-max-old-generation-size`。TLBミス削減でメモリ集約型ワークロード10-30%高速化
- **難易度**: Medium

#### FR-289: Stride-Based Automatic Prefetch (ストライドベース自動プリフェッチ)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: FR-187（手動プリフェッチヒント挿入）が計画済みだが、自動ストライド検出なし
- **内容**: ループ内のメモリアクセスパターンを静的解析し、一定ストライドでのアクセスを検出。プリフェッチ距離をストライド×ループ反復回数から自動計算。ポインタチェイス（リスト走査・ツリー走査）パターンの自動検出とプリフェッチ挿入
- **根拠**: LLVM LoopDataPrefetch / GCC `-fprefetch-loop-arrays`。FR-187の自動化版
- **難易度**: Hard

#### FR-290: False Sharing Detection (フォールスシェアリング検出)

- **対象**: `packages/engine/compile/src/codegen-clos.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: FR-189（キャッシュライン対応オブジェクトレイアウト）でホットフィールド集約を計画。しかしスレッド間フォールスシェアリングの静的検出は未計画
- **内容**: 異なるスレッドが同一キャッシュライン上のフィールドに書き込むパターンを静的解析で検出。検出時にフィールド間にパディング挿入（`__attribute__((aligned(64)))`相当）。`@thread-local`アノテーションとの連携
- **根拠**: Intel VTune false sharing detection / perf c2c。並行プログラムの性能劣化の主因
- **難易度**: Hard

#### FR-307: Object Co-Location Hints (オブジェクト近接配置ヒント)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: `rt-gc-alloc`（`gc.lisp:23-44`）は単純なバンプアロケータ。`%gc-cheney-scan`（`gc.lisp:154-178`）はBFS順コピーで部分的な走査順局所性あり。しかしユーザー可視のヒントなし。オブジェクトヘッダ（`heap.lisp:16-22`）にアフィニティビットなし
- **内容**: 割り当てサイトカラーリングまたは`co-locate(a, b)`ヒントで関連オブジェクトを同一キャッシュライン/隣接ラインに配置。GCコピー時にアフィニティを維持。CLOSインスタンスとそのクラス記述子の近接配置
- **根拠**: HotSpot TLAB (Thread-Local Allocation Buffers) / Zing C4 co-location。親子オブジェクトの空間的局所性確保
- **難易度**: Hard

#### FR-308: Data Structure Flattening (データ構造フラット化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: CLOSインスタンスはハッシュテーブル→ハッシュテーブル（`vm-make-obj`、`vm-clos.lisp:165-183`）。VMレジスタもハッシュテーブル（`vm-state-registers`、`vm.lisp:343`）。スロット読み取りは2+回のハッシュテーブルルックアップ。クロージャの捕捉値はalist（`vm-closure-captured-values`、`vm.lisp:31`）
- **内容**: CLOSインスタンスを連続配列（struct-like inline slot storage）にフラット化。レジスタファイルを固定サイズベクタに置換。小クロージャの捕捉値をインライン化（alistチェイン廃止）。各ホップ=潜在的キャッシュミスの除去
- **根拠**: V8 hidden class + inline slots / SpiderMonkey NativeObject。ポインタ追跡回避で10x高速化
- **難易度**: Hard

#### FR-309: Memory Access Pattern Analysis (メモリアクセスパターン解析)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/foundation/mir/src/mir.lisp`
- **現状**: オプティマイザの8パス（`*opt-convergence-passes*`、`optimizer.lisp:1020-1031`）は全てレジスタレベル。メモリアクセス追跡なし。エイリアス解析なし（FR-017として計画のみ）。MIRに`:load`/`:store`演算（`mir.lisp:142-143`）あるが解析パスなし
- **内容**: `vm-slot-read`/`vm-slot-write`・配列操作を追跡し、シーケンシャル・ストライド・ランダムのアクセスパターンを検出。FR-289（プリフェッチ挿入）とFR-287（ループタイリング）のデータソース
- **根拠**: LLVM MemorySSA / GCC alias oracle。メモリレベル最適化の基盤解析
- **難易度**: Very Hard

---

### Phase 70 — GC高度化（追加）

#### FR-331: Old-Space Free-List Allocation Reuse (旧世代フリーリスト再利用)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: `%gc-sweep-old-space`（`gc.lisp:301-329`）がメジャーGCスイープ時にフリーリストを構築。`rt-gc-alloc`（`gc.lisp:23-44`）はヤング空間バンプポインタのみ使用。旧空間昇格（`gc.lisp:78-82`）もバンプポインタ（`old-free`）で`rt-heap-free-list`を完全無視
- **内容**: 旧空間昇格時にまず`rt-heap-free-list`からfirst-fit/best-fit割り当てを試行。フリーリスト枯渇時にバンプポインタにフォールバック。旧空間の断片化削減
- **根拠**: JVM CMS free-list / G1 region-based allocation。メジャーGC後のメモリ再利用
- **難易度**: Easy

#### FR-332: Precise GC Root Scanning (正確GCルートスキャニング)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: `rt-gc-add-root`（`gc.lisp:50-54`）は全ルートを型情報なしの不透明consセルとして扱う。`gc.lisp:232-236`のルートスキャンは`(integerp val)`チェックのみ。NaN-boxing（`value.lisp`）の型述語を活用せず
- **内容**: 各GCルートにポインタ/fixnum/doubleの型メタデータを付与。非ポインタルートのスキャンスキップ。`value.lisp:109-131`の型述語を活用したルート分類。FR-231（スタックマップ）と相補的
- **根拠**: HotSpot OopMap / V8 tagged pointer root scanning。GCルートスキャン高速化
- **難易度**: Medium

#### FR-333: Nursery Sizing Heuristics (ナーサリサイズヒューリスティクス)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **現状**: `*gc-young-size-words*`=128Kワード（1MB）、`*gc-old-size-words*`=512Kワード（4MB）がハードコード（`heap.lisp:30-34`）。ランタイムリサイズなし。GC統計（`gc.lisp:398-421`）は収集するが消費なし
- **内容**: 割り当てレート・昇格レート（`words-promoted`）・マイナーGC頻度に基づくナーサリサイズ適応。頻繁なGCではナーサリ拡大、昇格レートが高い場合は縮小。ヒープ拡張（`rt-heap-words`ベクタの再割り当て）
- **根拠**: HotSpot adaptive sizing / Go GC pacer。GCオーバーヘッドの自動最適化
- **難易度**: Medium

#### FR-334: Memory Pressure Callbacks (メモリ逼迫コールバック)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: ヤング空間枯渇時（`gc.lisp:40-42`）と旧空間枯渇時（`gc.lisp:80-81`）にハードエラー。OOM前にユーザーコードがキャッシュ解放等の対応を行うフックなし
- **内容**: `rt-heap-memory-pressure-hooks`スロット追加。設定可能な閾値（80%/90%占有率）でコールバック呼び出し。コールバック復帰でメモリ解放されていれば処理続行
- **根拠**: .NET GC.RegisterForFullGCNotification / Android onTrimMemory。OOM回避のユーザーフック
- **難易度**: Easy

#### FR-335: Write Barrier Young-to-Young Elision (Young→Youngストアバリア省略)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: `rt-gc-write-barrier`（`gc.lisp:269-295`）が毎ストアで旧空間チェック。Young→Youngストアはカードマーキング不要だがチェックコストを支払う
- **内容**: `obj-addr`がヤング空間内ならSATBスナップショット・カードマーキング両方をスキップするファストパス。`heap.lisp:256-260`の`rt-young-addr-p`をバリア入口で使用。エスケープ解析でナーサリ割り当て確定なら静的にバリア除去
- **根拠**: G1 GC / ZGC young-gen skip。バリアオーバーヘッド削減
- **難易度**: Easy

#### FR-336: GC-NaN-Boxing Integration (GCとNaN-Boxing値表現の統合)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/value.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 3つの独立した値表現: (1) `value.lisp` NaN-boxing、(2) `heap.lisp`ヘッダベースタグ、(3) `vm.lisp`ハッシュテーブルヒープ+CLオブジェクト。GCは`(integerp val)`でポインタ判定（`gc.lisp:145-148, 232-234`）。`value.lisp:132-140`の`val-pointer-p`を使わない
- **内容**: GCスロットスキャンで`val-pointer-p`によるNaN-boxed値のポインタ判定。非ポインタ（fixnum/double/char）の正確な識別とスキャン省略。ヒープオブジェクトのスロットタイプ対応
- **根拠**: V8 tagged pointer GC / SpiderMonkey NaN-boxing GC。GCスキャン精度向上
- **難易度**: Medium

#### FR-337: Finalizer Ordering Guarantees (ファイナライザ実行順序保証)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: FR-184がファイナライゼーションキュー概念を定義するが、順序セマンティクスなし。現在`gc.lisp`にファイナライザインフラ自体が未実装
- **内容**: 参照グラフに基づくトポロジカルソート。AがBを参照し両方到達不能ならAのファイナライザが先に実行。ファイナライザ復活防止（ファイナライザキューから到達可能なオブジェクトはファイナライザ完了まで未回収）。ファイナライザスレッド/遅延実行
- **根拠**: Java `PhantomReference` ordering / Python PEP-442。ファイナライズの正確性保証
- **難易度**: Hard

---

### Phase 72 — 並列・並行GC

#### FR-338: Parallel GC Worker Threads (並列GCワーカースレッド)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: minor GC（Cheney copying、`gc.lisp:200-263`）およびmajor GC（tri-color mark-sweep、`gc.lisp:331-392`）はシングルスレッド逐次実行。CPUコア数にかかわらずGC時間はO(ヒープ使用量)
- **内容**: GCフェーズをタスクキューで分割し複数ワーカースレッドで並列化。ルートスキャン並列化（各スレッドのルートセットを独立スキャン）。マークフェーズのwork-stealing: `%gc-mark-grey` キューをN分割しワーカーが盗む。スイープフェーズの領域分割並列化
- **根拠**: HotSpot Parallel GC / G1 Parallel Marking / Go GC parallel marking (GOMAXPROCS workers)。マルチコア環境でGC停止時間をコア数に反比例させる標準技術
- **難易度**: Hard

#### FR-339: Concurrent Marking with Tri-Color Invariant (並行マーキング・三色不変式)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: SATBバリア（`gc.lisp:282-289`）は実装済みだがマーキング自体はSTW。ミューテータ停止中に全グレーオブジェクトを処理
- **内容**: マーキングをミューテータ実行中に行うConcurrent Mark。スナップショット対象はGC開始時点の生存オブジェクト集合（SATB保証）。マーキング完了後にSTW remark フェーズ（短時間）でSATBキューをドレイン。`*gc-satb-queue*` をper-thread化してロック不要にする
- **根拠**: G1 GC Concurrent Mark / ZGC Concurrent Mark / CMS。STWポーズをremark+cleanup（数ms）まで短縮
- **難易度**: Very Hard

#### FR-340: Concurrent Sweeping (並行スイープ)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: `%gc-sweep-old-space`（`gc.lisp:301-329`）はマーキング完了後にSTWで全旧空間をスキャン。ヒープサイズ比例の停止
- **内容**: スイープをミューテータ再開後に並行実行。未スイープ領域への割り当て要求はオンデマンドスイープ（lazy sweep）でカバー。スイープ完了前に当該領域が必要になった場合のみ同期スイープ。フリーリスト構築も並行化
- **根拠**: Go GC concurrent sweep / CMS concurrent sweep。スイープ時間をほぼゼロに短縮
- **難易度**: Hard

#### FR-341: GC Pause Time Goals / SLO (GC停止時間SLO)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: GCはヒープ枯渇時にのみ発動し停止時間の上限なし。`*gc-tenuring-threshold*`等のパラメータはあるが時間目標なし
- **内容**: `*gc-max-pause-ms*` 目標値（デフォルト200ms）を設定。Concurrent/Incremental GCの作業量を停止時間測定（`get-internal-real-time`）でスライス。目標超過時に作業を中断しミューテータに制御を返す。Adaptive Sized Regions（G1方式）で目標達成精度を向上
- **根拠**: G1 `-XX:MaxGCPauseMillis` / Shenandoah pause target。低レイテンシアプリケーションの要件
- **難易度**: Hard

#### FR-342: GC Concurrent Relocation (並行オブジェクト移動)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: Cheney コピーGCは完全STW。オブジェクト移動中はミューテータ停止必須
- **内容**: ロードバリア（FR-349）と組み合わせてオブジェクト移動をミューテータ実行中に実施。移動先アドレスをforwarding tableに記録。古いアドレスへのアクセスをバリアがインターセプトし新アドレスを返す。ZGCの「concurrent relocate」フェーズ相当
- **根拠**: ZGC Concurrent Relocation / Shenandoah Brooks Pointers。移動中のSTWポーズをほぼゼロに
- **難易度**: Very Hard

---

### Phase 73 — スレッドローカル割り当て (TLAB)

#### FR-343: Thread-Local Allocation Buffers (TLAB)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: `rt-gc-alloc`（`gc.lisp:23-44`）はグローバルバンプポインタ操作。マルチスレッド環境では`young-free`更新にロックが必要
- **内容**: 各スレッドがヒープの専有チャンク（TLAB）を保有し、TLABが枯渇するまでロックフリーで割り当て。TLABサイズはスレッドの割り当てレートに応じて動的調整（最小1KB〜最大512KB）。TLAB外の大オブジェクトはグローバルロック経由。GC時はすべてのTLABを一斉リタイア
- **根拠**: HotSpot TLAB / GraalVM TLAB / Go `mcache`。スレッドあたり割り当てコストをほぼゼロに（CAS不要）
- **難易度**: Medium

#### FR-344: TLAB Waste Minimization (TLABウェイスト最小化)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **依存**: FR-343
- **内容**: TLAB残余領域の「Dummy Fill」: TLABリタイア時に残余をダミーオブジェクトで埋めてヒープ走査可能状態に保つ。統計ベースのTLABサイズ予測（EMA）で残余ウェイストを最小化。`gc-tlab-waste-limit`を超えたTLABは早期リタイア
- **根拠**: HotSpot TLAB refill waste heuristics。ヒープ線形走査の正確性維持
- **難易度**: Easy

#### FR-345: Bump-Pointer Allocation with SIMD Zeroing (SIMD高速ゼロ初期化)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**: 新規割り当てオブジェクトのゼロ初期化をSIMD命令（`VMOVDQA`/`VMOVNTDQ`）で実施。16バイトアライメント保証の上で32〜64バイト/命令の高スループット初期化。Non-temporal storeでキャッシュ汚染を回避
- **根拠**: HotSpot `Copy::fill_to_aligned_words` / V8 MemsetPointer with SIMD。割り当て＋ゼロ化コストの削減
- **難易度**: Medium

---

### Phase 74 — ロードバリア・カラードポインタ

#### FR-346: Pointer Tagging for Type Dispatch (ポインタタグによる型ディスパッチ)

- **対象**: `packages/backend/runtime/src/value.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: `value.lisp`のNaN-boxing方式とvm.lispのハッシュテーブルオブジェクトが混在。型判定は複数段階のチェック
- **内容**: ポインタ下位3ビットをタグに使用（8バイトアライメント前提）。`000`=fixnum, `001`=cons, `010`=closure, `011`=vector, `100`=string, `101`=symbol, `110`=float/box, `111`=other。型チェックをAND+比較1命令に削減。GCもタグでポインタ/非ポインタを即判定
- **根拠**: SBCL/Clozure CL lowtag system / Racket immediate values / Ruby VALUE。型ディスパッチのゼロコスト化
- **難易度**: Very Hard

#### FR-347: Compressed Object References (ポインタ圧縮)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 全ポインタが64bit。ヒープオブジェクト内ポインタスロットが8バイト/個
- **内容**: ヒープを最大32GBに制限し、ヒープ内参照を32bitオフセットで表現（`heap_base + offset`）。ロード時に`LEA rax, [heap_base + r32*1]`で復元。スロット密度が2倍に向上しキャッシュ効率改善。GCも32bitオフセットで移動量を記録
- **根拠**: JVM `-XX:+UseCompressedOops` / V8 pointer compression (2020)。ヒープ密度向上でL3キャッシュ効率10-30%向上
- **難易度**: Very Hard

#### FR-348: Colored Pointers (カラードポインタ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: ポインタ上位ビット（x86-64の48〜63bit未使用領域）にGCカラービット（marked/remapped/finalizable）を埋め込む。カラービット変更でGCフェーズ遷移を表現し、ページテーブルtrickでロードバリアを実現。メモリマッピング切り替えでオブジェクト移動をミューテータに透過化
- **根拠**: ZGC Colored Pointers (JEP 333)。並行GCに必要なメタデータをポインタ自身に持たせてロードバリアを最小化
- **難易度**: Very Hard

#### FR-349: Load Barriers (ロードバリア)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 書き込みバリア（`rt-gc-write-barrier`）のみ。読み取りバリアなし
- **内容**: ヒープポインタのロード時に実行されるバリア。FR-348のカラービット確認: 未マーク→自己修復マーク。並行relocation時: 古いアドレス→forwarding table経由で新アドレス返却。「self-healing」で次回以降はバリアコストなし。ZGC: 1命令test + 低確率のスロウパス
- **根拠**: ZGC load barrier / Shenandoah Brooks pointer barrier。ライトバリア不要の完全並行GCを実現
- **難易度**: Very Hard

#### FR-350: Barrier Elision in JIT Compiled Loops (JITループ内バリア除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **依存**: FR-346〜FR-349
- **内容**: ループ内で同一オブジェクトを繰り返し参照する場合、最初のロードバリア通過後はバリアスキップ。GCフェーズ変化はsafepointで検出するため、safepoint間はカラー不変と仮定してバリアを静的除去。HoistしたロードへのCSEと組み合わせ
- **根拠**: GraalVM Partial Barrier Elision / HotSpot C2 barrier optimization
- **難易度**: Hard

---

### Phase 75 — 参照カウント・ARC

#### FR-351: Automatic Reference Counting (ARC) モード

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**: トレーシングGCと切り替え可能なARC実行モードを追加。オブジェクトヘッダ（`heap.lisp:16-22`）にrefcountフィールド。`vm-slot-write`でincrement/decrement。ゼロ到達で即時解放（deterministic destruction）。循環参照はバックグラウンドのトレーシングGCが補完（Swift Runtime方式）
- **根拠**: Swift ARC / Python CPython refcount / Rust `Rc<T>`/`Arc<T>`。組み込み・リアルタイムドメインでのdestructorセマンティクス
- **難易度**: Hard

#### FR-352: Deferred Reference Counting (遅延参照カウント)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-351
- **内容**: スタック上のポインタ（ローカル変数）はrefcountを変更しない。ヒープ間ポインタ書き込みのみカウント（Levanoni-Petrank方式）。スタック参照のincrement/decrementはGCスキャン時に一括処理。refcountコストを60-80%削減
- **根拠**: CPython 3.12+ deferred RC / Levanoni-Petrank DRC。スタック変数への頻繁なrc変更コストを排除
- **難易度**: Hard

#### FR-353: Biased Reference Counting (バイアス参照カウント)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-351, FR-343 (TLAB)
- **内容**: オブジェクトを生成したスレッドがオーナー。オーナースレッドからのRC変更は非アトミックfastpath。別スレッドからのアクセスは「バイアス解除」後にアトミックopへ移行。Swift 5.9+ Biased RC / LXR RC方式
- **根拠**: Swift Biased Reference Counting (2023) / LXR (Language eXtension Runtime)。スレッドローカルオブジェクトのRC操作をほぼゼロコストに
- **難易度**: Hard

#### FR-354: Cycle Detector for Reference Counting (RC用サイクル検出)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-351
- **内容**: refcountがゼロにならない循環参照グループを定期スキャンで検出（Bacon-Rajan Concurrent Cycle Collector）。候補オブジェクトを「purple」マーク。Trial-deletionアルゴリズムで循環グループ内の内部RC減算後にゼロになるグループを特定して回収。CPython の `gc.collect()` と同等
- **根拠**: Python gc module / PHP cycle collector。ARCの致命的弱点（循環参照）の解決
- **難易度**: Hard

---

### Phase 76 — リージョンベースヒープ (G1方式)

#### FR-355: Region-Based Heap Layout (リージョンベースヒープ)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **現状**: 半空間2つ（young from/to）+ 旧空間の固定3領域。サイズ変更不可
- **内容**: ヒープ全体を等サイズリージョン（例: 1MB/2MB）の配列として管理。各リージョンにメタデータ（age、liveness、remembered set、allocation cursor）。Youngリージョン群とOldリージョン群を動的に再割り当て。大オブジェクトは複数の連続リージョン（Humongous Region）
- **根拠**: G1 GC Region-Based Heap / Shenandoah / GraalVM Serial GC。ヒープサイズ変化への適応、GC対象リージョンの選択的回収
- **難易度**: Very Hard

#### FR-356: Per-Region Remembered Sets (リージョン別Remembered Set)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **依存**: FR-355
- **現状**: カードテーブルはグローバル一枚。特定リージョンへの外部参照を効率的に列挙不可能
- **内容**: 各リージョンにRemembered Set（RSSet）を持つ。他リージョンからの参照カードをRSSに記録。Evacuation時にRSSを参照元情報として使用しrootsを完全列挙。RSSet更新はwrite barrier（`rt-gc-write-barrier`）に追加
- **根拠**: G1 GC Remembered Sets / C4 GC RSets。リージョン単位の独立GCの前提条件
- **難易度**: Very Hard

#### FR-357: Mixed GC Collections (混合GCコレクション)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-355, FR-356
- **内容**: Youngリージョンのみ回収するminor GCと、高liveness-waste旧リージョンも混ぜて回収するMixed GCを区別。`%gc-select-collection-set`でliveness/waste統計を元にGC対象リージョン集合を選択。Mixed GCはpause time目標（FR-341）を守りながら徐々に旧空間を回収
- **根拠**: G1 Mixed Collection / Shenandoah。旧空間の断片化をSTW Compaction不要で解消
- **難易度**: Very Hard

#### FR-358: Evacuation Failure Handling (Evacuation失敗処理)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-355〜FR-357
- **内容**: GC対象リージョンのEvacuation中に移動先空間が枯渇した場合の処理。Evacuation失敗時は元リージョンを保持しfull GCにエスカレート。失敗統計を記録しリージョンサイズや回収集合の選択に反映
- **根拠**: G1 Evacuation Failure handling (to-space overflow)。コーナーケースの安全性保証
- **難易度**: Hard

---

### Phase 77 — スラブ・アリーナアロケータ

#### FR-359: Slab Allocator for Fixed-Size Objects (固定サイズスラブアロケータ)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: cons・symbol・closure等の頻出固定サイズオブジェクト専用のスラブプール。各スラブクラスにフリーリストを持ち、同サイズオブジェクトが連続配置される。GCスキャン高速化（同一スラブ内は同型なのでスキャンコード共有）。外部フラグメンテーション最小化
- **根拠**: Linux SLAB/SLUB allocator / jemalloc size-class bins / tcmalloc。cons操作が支配的なLispワークロードに特に有効
- **難易度**: Medium

#### FR-360: Arena / Region Allocator for Compiler Passes (コンパイラパス用アリーナ)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/cps.lisp`
- **内容**: コンパイル1パス中に生成されるAST・MIR・VM命令列をアリーナ（単調増加バンプポインタ）から割り当て。パス完了後にアリーナ全体を一括解放（個別GCコストなし）。中間表現の寿命が明確なためスコープ付きアリーナが最適
- **根拠**: LLVM BumpPtrAllocator / GCC obstack / Clang ASTContext arena。コンパイラ内中間データ構造のGCプレッシャー排除
- **難易度**: Easy

#### FR-361: Pool Allocator with Object Recycling (オブジェクトリサイクルプール)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: `vm-call-frame`等の短命オブジェクトをフリープールから再利用。解放時にゼロ初期化してプールへ返却。コールフレームは関数呼び出しのたびに生成・破棄されるため、プールによりalloc/GCコストを排除。プールサイズ上限を設けGC圧力と再利用コストのバランスを取る
- **根拠**: HotSpot JNI handle pools / SBCL stack-allocated binding frames。短命オブジェクトのGCプレッシャー削減
- **難易度**: Easy

#### FR-362: Segregated Free Lists by Size Class (サイズクラス別フリーリスト)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: 旧空間フリーリスト（`rt-heap-free-list`）を単一リストから16サイズクラス別（8B/16B/32B/.../32KB/large）に分離。first-fit探索O(N)→O(1)ルックアップ。サイズクラスは2倍刻みの指数分布。jemalloc/tcmalloc方式のbin管理
- **根拠**: jemalloc size-class bins / tcmalloc central free list / Hoard allocator。フリーリスト検索の定数時間化
- **難易度**: Medium

---

### Phase 78 — NUMA対応メモリ管理

#### FR-363: NUMA-Aware Heap Allocation (NUMA対応ヒープ割り当て)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: `numa_alloc_local()`（Linux）/ `VirtualAllocExNuma()`（Windows）でヒープ領域をNUMAノードローカルに確保。スレッドのNUMAノードをOSに照会し（`sched_getcpu()` + `/sys/devices/system/cpu/cpuN/node`）、TLABをローカルNUMAノードのページから割り当て。cross-NUMAアクセスのメモリ帯域幅競合を排除
- **根拠**: JVM NUMA-Aware Allocator (`-XX:+UseNUMA`) / Go NUMA hints。8ソケット以上のサーバーで2-5x帯域幅向上
- **難易度**: Hard

#### FR-364: NUMA-Local GC (NUMAローカルGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-363
- **内容**: GCワーカースレッドをオブジェクトの所在NUMAノードに配置してリモートアクセスを排除。Minor GCをNUMAノード単位で独立実行。オブジェクト移動先もソースと同一NUMAノードを優先
- **根拠**: JVM NUMA-Aware GC / GraalVM NUMA GC。GCスループットのNUMAスケーラビリティ
- **難易度**: Hard

#### FR-365: Memory Interleaving for Shared Data (共有データのメモリインタリーブ)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: 全スレッドから頻繁にアクセスされるグローバルデータ（シンボルテーブル・メソッドキャッシュ・コードキャッシュ）を`mbind(MPOL_INTERLEAVE)`でNUMAノード間にインタリーブ配置。特定ノードへのホットスポットを分散
- **根拠**: Linux `numactl --interleave`。グローバル共有データのアクセス競合解消
- **難易度**: Medium

---

### Phase 79 — ヒーププロファイリング・観測可能性

#### FR-366: Sampling Heap Profiler (サンプリングヒーププロファイラ)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `cli/src/main.lisp`
- **内容**: 割り当て毎に一定確率（デフォルト: 512KB毎に1回）でコールスタックをサンプリング。Poisson samplingで統計的に正確なオブジェクト割り当て量推定。`--heap-profile=output.pb`でpprof互換フォーマット出力。Flameグラフ生成対応
- **根拠**: Go `pprof` heap profiler / Chromium v8 heap profiler / JVM JFR heap sampling。本番環境でも1%以下のオーバーヘッドで割り当てホットスポット特定
- **難易度**: Medium

#### FR-367: Allocation Tracing with DTrace/eBPF (割り当てトレーシング)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: USDT（User Statically-Defined Tracing）プローブを`rt-gc-alloc`・GCフェーズ開始/終了・TLAB refillに挿入。`dtrace -n 'cl_cc*:::alloc'`でゼロオーバーヘッド計装。eBPF uprobe対応でLinuxの`perf`からも観測可能
- **根拠**: JVM DTrace probes / Ruby USDT probes / Node.js tracing。プロダクション計装の標準
- **難易度**: Medium

#### FR-368: Live Heap Snapshot (ライブヒープスナップショット)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**: GCルートから到達可能なオブジェクトを全列挙しJSONまたはバイナリ形式でスナップショット出力。オブジェクトタイプ・サイズ・参照先・割り当てサイト情報を記録。差分スナップショット比較でメモリリーク特定
- **根拠**: JVM `-XX:+HeapDumpOnOutOfMemoryError` / V8 `--heapsnapshot`。メモリリーク調査の標準ツール
- **難易度**: Medium

#### FR-369: GC Metrics Export (GCメトリクスエクスポート)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: GCイベントをPrometheus互換メトリクスとして公開。`gc_pause_seconds`, `gc_collections_total`, `heap_used_bytes`, `heap_available_bytes`, `gc_promoted_bytes` 等の標準メトリクス。Prometheus `/metrics` HTTP エンドポイント or OpenTelemetry OTLP出力
- **根拠**: Go runtime/metrics / JVM JMX GC metrics / Prometheus GC exporter。クラウドネイティブ環境での可観測性
- **難易度**: Easy

---

### Phase 80 — スタックマップ・精確GCルートスキャン

#### FR-370: Stack Map Generation (スタックマップ生成)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/foundation/mir/src/mir.lisp`
- **現状**: GCルートはグローバル`rt-heap-roots`リストのみ。コールスタック上のローカル変数はスキャン対象外（保守的GCに頼るか未サポート）
- **内容**: 各safepointでのスタックフレームレイアウトを記録したスタックマップテーブルを生成。GCレジスタ（ポインタ保持レジスタ）のビットマスクをprogram counter毎に記録。GCルートスキャン時にコールスタックを逆順にwalknしてすべてのスタック上ポインタを精確にスキャン
- **根拠**: HotSpot OopMap / LLVM StackMaps / GHC info tables。精確GCの前提条件でスタック変数のポインタを保守的にスキャンするコストを排除
- **難易度**: Very Hard

#### FR-371: GC Safepoints: Signal-Based vs Polling (GCセーフポイント実装)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: セーフポイントの概念はMIRに存在するが実際のSTW機構未実装
- **内容**: 2実装の選択: (1) Polling方式: ループバックエッジ・関数入口にセーフポイントポーリング命令（`test [safept_page], 0`）挿入。GC開始時にページをreadonly→SIGSEGV → シグナルハンドラで全スレッドをSTW。(2) シグナル方式: POSIX `pthread_kill`で各スレッドにUSR1を送りシグナルハンドラで停止。Polling方式がJVM/Go/V8の標準
- **根拠**: JVM safepoint polling page / Go signal-based async preemption (1.14+)。STW協調の基盤機構
- **難易度**: Hard

#### FR-372: Deoptimization Stack Maps (非最適化スタックマップ)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**: 投機的最適化（型特殊化・インライン化等）が無効化される際にJIT最適化フレームを未最適化フレームに変換するためのスタックマップ。各deoptポイントで「物理レジスタ → 論理変数名」のマッピングを保持。deopt時に仮想マシン状態を再構成
- **根拠**: HotSpot C2 Deoptimization / V8 Deoptimizer / GraalVM deopt。speculative JIT（FR-specculative）の前提
- **難易度**: Very Hard

---

### Phase 81 — メモリ安全性・タギング

#### FR-373: Address Space Layout Randomization for Heap (ヒープASLR)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: ヒープ割り当てアドレスを`mmap(MAP_FIXED_NOREPLACE)`でランダム化。`getentropy()`/`/dev/urandom`からシード取得。ヒープアドレスの予測不可能性によりメモリ安全性向上。スタックASLRはOSが提供するがヒープは言語ランタイム側での対応が必要
- **根拠**: OpenBSD malloc ASLR / hardened malloc。UAF exploit mitigation
- **難易度**: Easy

#### FR-374: Memory Tagging Extension (MTE) サポート

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **現状**: AArch64バックエンドあり（`aarch64.lisp`）。メモリアクセス検証なし
- **内容**: ARMv8.5-A MTE命令（`IRG`, `STGM`, `LDG`, `STG`）でポインタにアロケーションタグを付与。タグ不一致アクセスで即時SIGSEGV。use-after-free・バッファオーバーフローをハードウェアで検出。MTE対応AArch64マシン上で自動有効化
- **根拠**: Linux MTE support (5.10+) / Android MTE deployment (Pixel 8+)。ハードウェアASANとして本番環境でのメモリ安全性検出
- **難易度**: Hard

#### FR-375: Shadow Memory for AddressSanitizer Integration (ASan統合)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: ASanシャドウメモリマッピングを割り当て/解放時に更新。`rt-gc-alloc`でアロケーション時に当該アドレスをASan "accessible"マーク。GC回収時に"freed"マーク（redzoneで隣接アクセスも検出）。`-fsanitize=address`でコンパイルされたCコードとのinteropで一貫したメモリ安全性
- **根拠**: LLVM ASan integration / Rust sanitizer support。use-after-free・out-of-bounds検出
- **難易度**: Medium

#### FR-376: Guard Pages for Stack Overflow Detection (スタックオーバーフロー検出)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**: コールスタック末尾にguard page（`mprotect(PROT_NONE)`）を配置。スタックオーバーフロー時にSIGSEGVが確実に発生し検出可能（guard pageなし→ヒープ破壊になる）。シグナルハンドラで`alternate signal stack`（`sigaltstack`）を使用しスタック上でハンドラが動作できるようにする
- **根拠**: POSIX `sigaltstack` / JVM stack overflow protection / Go goroutine stack guard。デバッグ困難なスタック破壊バグの防止
- **難易度**: Easy

---

### Phase 82 — 不死オブジェクト・オフヒープデータ

#### FR-377: Immortal / Permanent Objects (不死オブジェクト・永続領域)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: 一度生成されたら決してGCされないオブジェクト（組み込みシンボル・組み込み関数・コードオブジェクト）を独立した永続領域に配置。GCルートスキャン対象外（`rt-gc-add-root`不要）。永続領域はmmap固定アドレスでプロセス再起動後もアドレス不変（AOTキャッシュの基盤）
- **根拠**: CPython 3.12 Immortal Objects (PEP 683) / JVM Metaspace / Ruby YJIT code region。GCルートスキャン量の削減とARC biasの単純化
- **難易度**: Medium

#### FR-378: Off-Heap Native Memory Management (オフヒープネイティブメモリ)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: GC管理ヒープ外のネイティブメモリ割り当てAPI。`rt-native-alloc`/`rt-native-free`でFFIバッファ・IO作業メモリを管理。オフヒープ割り当て量をGCに通知（`rt-gc-register-external-memory`）し、外部メモリ圧力が高い場合にGCをより積極的に実行。Java `ByteBuffer.allocateDirect` / Go `cgo`相当
- **根拠**: JVM DirectByteBuffer / .NET `System.Runtime.InteropServices.NativeMemory`。GCプレッシャーなしの大規模バッファ
- **難易度**: Easy

#### FR-379: Code Cache Management (コードキャッシュ管理)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**: JIT生成コードを専用コードキャッシュ領域（rx: 実行可能/読み取り専用領域）に配置。コードキャッシュが満杯になった場合のEviction（LRU or reference-counting）。コンパイル済み関数のunload（deoptimization後）。コードキャッシュサイズの動的調整
- **根拠**: JVM CodeCache / V8 CodeSpace / SpiderMonkey JitCode region。コードメモリのOOM防止
- **難易度**: Medium

#### FR-380: Heap Compaction Trigger Heuristics (コンパクションtriggerヒューリスティクス)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: フラグメンテーション率（`free-bytes / heap-size`）が閾値（デフォルト: 50%）を超えた場合にのみCompaction（FR-089/FR-213）を発動。通常GCとCompactingGCを分離することでCompactionコストを必要時のみ支払う。`gc-fragmentation-ratio`統計を`rt-gc-stats`に追加
- **根拠**: G1 GC mixed collection trigger / .NET GC compaction threshold。不要なコンパクションによるSTWを回避
- **難易度**: Easy

---

### Phase 83 — 参照強度階層 (Reference Strength Hierarchy)

#### FR-381: Soft References (ソフト参照)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: FR-246でエフェメロン（key弱参照）を定義。しかしメモリ逼迫時のみGCされる「キャッシュ向け参照」は未定義
- **内容**: `rt-soft-ref`型を追加。通常GC時はソフト参照先を回収しない。ヒープ占有率が閾値を超えた場合のみ回収候補に昇格（LRU順で最も古いものから）。アクセス時刻`last-access-time`フィールドでLRU管理。インメモリキャッシュの正確なサイズ制御に利用
- **根拠**: Java `SoftReference` / Guava `SoftValues` cache。GC主導のキャッシュ退出でOOM防止
- **難易度**: Medium

#### FR-382: Weak References (弱参照)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: `rt-weak-ref`型。参照先が到達不能になった次のGCサイクルで`nil`にクリアされる。クリア時にオプションの通知コールバック（referenceキュー相当）を呼び出し。シンボルインターン表・メモ化テーブルの正確なキー管理に使用。CLの`make-weak-pointer`相当
- **根拠**: Java `WeakReference` / SBCL `sb-ext:make-weak-pointer` / Python `weakref`。インターンテーブルのメモリリーク防止
- **難易度**: Easy

#### FR-383: Phantom References (ファントム参照)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: `rt-phantom-ref`型。ファントム参照経由でオブジェクトへはアクセス不能（常に`nil`を返す）。ファイナライズ後・回収前にReferenceキューに入る。外部リソース（FFIバッファ・ファイルハンドル）の解放タイミング制御に使用。ファントム参照を持つオブジェクトはファントム参照がenqueueされるまで回収されない
- **根拠**: Java `PhantomReference` / JEP 421 Finalization deprecation → Cleaner API。ファイナライザより安全なリソース管理
- **難易度**: Medium

#### FR-384: Reference Queue Processing Thread (参照キュー処理スレッド)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-337, FR-381〜383
- **内容**: GCがweak/soft/phantomクリア済み参照をenqueueする`rt-reference-queue`。専用の参照処理スレッドがキューをポーリングしてコールバック実行。バッチ処理でコールバックのオーバーヘッドを削減。GCスレッドはenqueueのみ行い、コールバック実行は別スレッドに委譲
- **根拠**: Java `ReferenceQueue` + `Cleaner` thread / .NET `GC.RegisterForFinalization`。GCスレッドをコールバック実行から解放
- **難易度**: Easy

---

### Phase 84 — 部分エスケープ解析 (Partial Escape Analysis)

#### FR-385: Partial Escape Analysis (PEA)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: FR-007でエスケープ解析を定義。「オブジェクトがいずれかのパスでエスケープ → 全体をヒープ割り当て」という保守的判定
- **内容**: 分岐ごとのエスケープ性を個別追跡。`(if cond (escape obj) (use-locally obj))` のような場合、エスケープパスのみヒープ割り当てし非エスケープパスでスタック割り当て維持。Graal PEAの「materialization point」: エスケープするパスに到達した時点でヒープ昇格。スカラー置換と組み合わせた段階的マテリアライゼーション
- **根拠**: GraalVM Partial Escape Analysis (Stadler et al. 2014) / Graal JIT。フルエスケープ解析より大幅に多くの割り当てを除去
- **難易度**: Very Hard

#### FR-386: Scalar Replacement (スカラー置換)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **依存**: FR-385 (PEA)
- **内容**: エスケープしないことが確認されたオブジェクトのフィールドをすべて個別のスカラーSSA変数に置換。オブジェクトの割り当て自体を除去。`(let ((p (make-point x y))) (+ (point-x p) (point-y p)))` → `(+ x y)`。値型（record like structs）に特に有効
- **根拠**: LLVM mem2reg / HotSpot C2 EliminateAllocations / GraalVM Scalar Replacement。オブジェクト割り当てのゼロコスト化
- **難易度**: Hard

#### FR-387: Allocation Merge (割り当てマージ)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **依存**: FR-385
- **内容**: ループ内で繰り返し割り当てられ前回値を捨てるオブジェクトを単一の再利用オブジェクトに変換。`(loop for i from 0 below n collect (make-point i 0))` のような場合、ループ本体の`make-point`を1回ヒープ割り当てしてフィールドを毎回更新（リストへのconsは残る）。短命オブジェクトのGCプレッシャー削減
- **根拠**: GraalVM PEA loop-aware materialization。繰り返し割り当てパターンの最適化
- **難易度**: Hard

---

### Phase 85 — ファイバー・軽量スレッドGC

#### FR-388: Fiber / Goroutine Stack GC (ファイバースタックGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: GCルートはグローバルリストとメインスレッドのスタックのみ想定。並行実行ファイバー（FR-193: Green Threads）のスタックはGCルート未登録
- **内容**: 各ファイバーのコールスタックをGCルートセットとして管理。ファイバー一時停止時（セーフポイント）にスタックマップ（FR-370）を使用してスタック上のポインタを精確にスキャン。ファイバースタックがshrink可能な場合にGCがスタック縮小を実施（Go stack shrinking）
- **根拠**: Go goroutine stack scanning / JVM Project Loom virtual thread GC / Erlang process heap GC。軽量スレッドをGCが正しく処理するための基盤
- **難易度**: Hard

#### FR-389: Segmented / Growable Stacks (セグメントスタック・伸縮スタック)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**: ファイバーごとの初期スタックを小さく（4KB程度）確保し、スタックオーバーフロー時に新セグメントを追加（Go 1.3以前の方式）またはより大きな連続スタックにコピー（Go 1.4以降のcontiguous stacks方式）。GCがスタックリサイズのタイミングでスタック上ポインタを更新（移動時）
- **根拠**: Go contiguous stack growth / Erlang process heap growth / Green Threads runtime stack。100万ファイバー規模でのメモリ効率
- **難易度**: Hard

#### FR-390: Per-Fiber Minor GC (ファイバーローカルGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-388
- **内容**: Erlangモデル: 各ファイバーが独立した小ヒープ（nursery）を保有。ファイバーローカルオブジェクトはファイバー内で完結しGCが独立して行われる。ファイバー間通信はディープコピー or メッセージパッシング（移動）。グローバルGC負荷を分散
- **根拠**: Erlang per-process GC / Racket place-local GC。ファイバー間での相互GC停止を排除
- **難易度**: Very Hard

---

### Phase 86 — ヒープ動的リサイズ

#### FR-391: Heap Growth Policy (ヒープ成長ポリシー)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **現状**: `*gc-young-size-words*`/`*gc-old-size-words*`が起動時固定。実行中のヒープ拡張はOOMエラー
- **内容**: GC後の占有率が閾値（デフォルト: 70%）超で次のGCまでにヒープを倍増。拡張は`mmap(MAP_ANONYMOUS | MAP_FIXED_NOREPLACE)`でアドレス空間連続性を保ちつつ領域を追加。最大ヒープサイズ`*gc-max-heap-words*`で上限設定。`rt-heap-words`ベクタの再割り当て
- **根拠**: JVM `-Xmx`/`-Xms` / Go `runtime.ReadMemStats` growth。OOMの前にヒープを自動拡張
- **難易度**: Medium

#### FR-392: Heap Shrink Policy (ヒープ縮小ポリシー)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **依存**: FR-391
- **内容**: GC後の占有率が低閾値（デフォルト: 20%）以下が連続Nサイクル続いた場合にヒープをOSに返却。`madvise(MADV_DONTNEED)` / `mmap(MAP_FIXED, PROT_NONE)`で物理ページ解放。コンテナ環境でのメモリ使用量最小化
- **根拠**: JVM Adaptive GC heap shrink / Go `runtime.FreeOSMemory` (`MADV_DONTNEED` on idle)。コンテナの実メモリ返却
- **難易度**: Medium

#### FR-393: Uncommit Unused Heap Regions (未使用リージョンのページ返却)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-355 (リージョンベースヒープ), FR-392
- **内容**: GC後に空になったリージョンのページを`madvise(MADV_FREE)`または`MADV_DONTNEED`で即時OS返却（commitは維持してアドレス空間は保持）。リージョン単位で行うためdecommit粒度が細かく迅速。仮想アドレス空間は維持しつつ物理メモリをOS返却
- **根拠**: ZGC uncommit / G1 `G1PeriodicGCInterval` uncommit。コンテナOrchestration環境での精確なメモリ使用量制御
- **難易度**: Medium

---

### Phase 87 — オブジェクトヘッダ設計

#### FR-394: Object Header Mark Word Encoding (ヘッダマークワードエンコーディング)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **現状**: `rt-object-header`（`heap.lisp:16-22`）はtype-tag + generation-age + marked + pinnedフィールドの素朴な構造体
- **内容**: 64bit mark wordに複数の状態を多重化。通常時: identityハッシュコード(25bit) + age(4bit) + GCビット(3bit) + タグ(3bit)。ロック膨張時: fat-lockポインタ。GCコピー中: forwarding pointer（下位1bitで判別）。1ワードに多状態を多重化してオブジェクトヘッダを1ワードに圧縮
- **根拠**: JVM object header mark word / V8 object map word。オブジェクトのメタデータ密度向上
- **難易度**: Hard

#### FR-395: Identity Hash Code Stability (identityハッシュコードの安定性)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: オブジェクト移動後もidentityハッシュコードを不変に保つ。初回`(sxhash obj)` 呼び出し時にアドレスベースのハッシュをmark wordに固定記録。GCコピー時にmark wordをコピー先に転送。forwarding pointerとハッシュコードの競合を「inflated header」で解決（外部スロットに退避）
- **根拠**: JVM identityHashCode stability / .NET `RuntimeHelpers.GetHashCode`。HashTableのキーとしてオブジェクトを使う際の正確性保証
- **難易度**: Medium

#### FR-396: Object Size Accounting and Padding Elimination (オブジェクトサイズ最適化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**: CLOSインスタンスのスロットレイアウトをフィールドサイズ降順でソートしアライメントパディングを最小化（JVM JOL / C++ `__attribute__((packed))` 相当）。スロット型情報（int/float/pointer）に基づいてアライメント要件を計算。オブジェクトサイズの統計的分析ツール
- **根拠**: JVM JOL (Java Object Layout) / .NET `StructLayout`. オブジェクトサイズ削減でGCスキャン量と帯域幅を削減
- **難易度**: Medium

---

### Phase 88 — 保守的GC・フリーリスト結合

#### FR-397: Conservative Stack Scanning Fallback (保守的スタックスキャンフォールバック)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: スタックマップ（FR-370）は未実装。現状スタック上のポインタはGCルート未登録
- **内容**: FR-370（スタックマップ）実装前の安全なフォールバック。スタックフレームを全ワードスキャンし、ヒープアドレス範囲内の値をポインタ候補として保守的に処理。BDWGC（Boehm GC）方式。不正確なため移動GCと組み合わせ不可だが、インタープリタモードでは十分安全
- **根拠**: Boehm-Demers-Weiser conservative GC / SBCL conservative roots on x86。正確スタックマップ実装前の安全な橋渡し
- **難易度**: Medium

#### FR-398: Free List Coalescing (フリーリスト結合)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **現状**: `%gc-sweep-old-space`（`gc.lisp:301-329`）でフリーリストを構築するが、隣接フリーブロックの結合処理なし
- **内容**: スイープ時に隣接するフリーブロックを1つの大きなブロックにマージ。ブロック間のfooter/headerに「free flag」を持つboundary tag方式（Knuth）。結合によりサイズクラス越えの大割り当てをフラグメンテーション後も可能にする
- **根拠**: dlmalloc coalescing / jemalloc extent coalescing / Doug Lea's malloc。ヒープフラグメンテーションの根本対策
- **難易度**: Easy

#### FR-399: Remembered Set Refinement Threads (Remembered Set洗練スレッド)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-356 (Per-Region RS)
- **内容**: write barrier（`rt-gc-write-barrier`）は「ダーティカード」をグローバルバッファに追記するだけ。バックグラウンドの「RS洗練スレッド」が非GC中にバッファを処理し、各リージョンのRSSを逐次更新。GC時はRSS洗練済みのためroot enumeration時間を短縮
- **根拠**: G1 GC Refinement Threads / Shenandoah RS update threads。GC停止時間からRS更新コストを除去
- **難易度**: Hard

#### FR-400: Concurrent Root Scanning (並行ルートスキャン)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: STW root enumeration中にすべてのスレッドを停止するかわりに、各スレッドが自身のルート（スタック・レジスタ）を安全なsafepointで自スキャン。スキャン完了スレッドはミューテータを再開。全スレッドのスキャンは並行して進行。STW時間をroot scanning分だけ短縮
- **根拠**: G1 Concurrent Root Processing / Shenandoah parallel root scanning。コア数×スタックサイズがSTWに影響する問題を解決
- **難易度**: Hard

---

### Phase 89 — 世代別ZGC・Shenandoah (2021-2024技術)

#### FR-401: Generational ZGC (世代別ZGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 現行GCは2世代（young/old）。ZGCスタイルの全世代並行GCなし
- **内容**: ZGC Generational（JDK 21 GA）方式を採用。若い世代のminor GCをより頻繁かつ低コストで実行。カラードポインタ（FR-348）を世代別に拡張: youngポインタ・oldポインタをカラーで区別。若い世代のremembered setをload barrier経由で維持。Youngオブジェクトの大部分を旧世代GCなしに回収
- **根拠**: JEP 439 Generational ZGC (JDK 21) / Generational Shenandoah (JDK 21)。ZGCの低レイテンシを維持しながらスループットを大幅向上
- **難易度**: Very Hard

#### FR-402: Generational Shenandoah (世代別Shenandoah)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: Shenandoah GCの世代別拡張。Brooks Pointer（FR-349のロードバリア）をYoung/Old世代で独立管理。Young GCはold空間を一切スキャンせず記憶集合（remembered set）のみ参照。Young GCサイクルをold GCの10-100倍頻繁に実行
- **根拠**: JEP 404 Shenandoah Generational (実験的, JDK 21+)。Shenandoahの完全非同期特性を世代別最適化と組み合わせ
- **難易度**: Very Hard

#### FR-403: SATB vs Incremental Update Write Barrier Selection (SATBとIUバリアの選択)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: SATB（Snapshot-At-The-Beginning）バリアのみ実装（`gc.lisp:282-289`）。Incremental Update（IU）バリア未実装
- **内容**: 2方式の実装と選択機構。SATB: ポインタ上書き前の古い値をグレーキューに追加（G1/ZGC方式）。IU: ポインタ書き込み後の新しい参照先をグレーキューに追加（CMS/Shenandoah方式）。リマークフェーズのコストがSATBは大、IUは小だが誤検出リスクあり。並行GCの種類に応じて自動選択
- **根拠**: G1 SATB barrier / CMS Incremental Update / Shenandoah IU+SATB hybrid。バリアコストとリマークコストのトレードオフ
- **難易度**: Hard

---

### Phase 90 — AOT・ネイティブイメージGC

#### FR-404: AOT Snapshot Heap (AOTスナップショットヒープ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `cli/src/main.lisp`
- **内容**: コンパイル時に確定するオブジェクト（組み込みシンボル・組み込みメソッドテーブル・コンパイル済みコード）を実行可能バイナリの読み取り専用データセクションに焼き込む。起動時にこれらのオブジェクトをヒープにコピーせず直接使用。GC対象外（immortal、FR-377と連携）。起動時間とメモリ使用量を大幅削減
- **根拠**: GraalVM Native Image Heap / SBCL core image / CL `save-lisp-and-die`。AOTコンパイル後の起動をミリ秒以下に
- **難易度**: Hard

#### FR-405: GC Root Minimization for AOT (AOT向けGCルート最小化)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `cli/src/main.lisp`
- **依存**: FR-377, FR-404
- **内容**: AOTバイナリではスナップショットヒープ内オブジェクトがimmutable + immortalのためGCルート登録不要。ランタイム割り当てオブジェクトのみGC対象とするルートフィルタリング。AOTで確定したメソッドキャッシュ・クラス記述子もGCルートから除外。GCルートスキャン時間をランタイム生成オブジェクト数に比例させる
- **根拠**: GraalVM Native Image GC roots / SubstrateVM GC。AOT後の初期GC停止時間を最小化
- **難易度**: Medium

---

### Phase 91 — GC FFIハンドルテーブル

#### FR-406: GC Handle Tables for FFI (FFI向けGCハンドルテーブル)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: FFIコード（Cからの呼び出し）はCLオブジェクトへの直接ポインタを持てない（GCによる移動でポインタ無効化）
- **内容**: `rt-gc-handle-table`: GCハンドル（整数インデックス）→ CLオブジェクト のマッピングテーブル。FFIコードはハンドルのみ保持しオブジェクトにアクセスする際は`rt-gc-handle-deref`経由。GCがオブジェクトを移動した場合はテーブルを更新するだけでCコードへの通知不要。ローカルハンドル（FFI呼び出し単位）・グローバルハンドル（永続）・弱ハンドル（GCで消えてもOK）の3種
- **根拠**: JNI local/global handles / .NET GCHandle / V8 Local/Persistent handles。移動GCとFFIの安全な共存
- **難易度**: Medium

#### FR-407: Pinning API for FFI Safety (FFI安全性のためのピンAPI)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-212 (Object Pinning), FR-406
- **内容**: `with-pinned-objects (obj1 obj2) &body body` マクロ。bodyの実行中はobj1/obj2のGCによる移動を禁止。CコードにLispオブジェクトの生アドレスを直接渡せるようになる。ピン期間中はそのオブジェクトをGCコピー対象から除外（FR-212機構を使用）。bodyの動的範囲がCスタックに現れる間はピン有効
- **根拠**: SBCL `with-pinned-objects` / JVM JNI `GetPrimitiveArrayCritical` / Python buffer protocol。FFI呼び出し中のアドレス安定性保証
- **難易度**: Medium

---

### Phase 92 — 異種メモリ・新世代メモリ技術

#### FR-408: Persistent Memory (PMEM) Heap Support (永続メモリヒープ)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: Intel Optane / CXL attached DRAM等の不揮発性メモリをヒープ領域として使用。`pmdk`（PMDK: Persistent Memory Development Kit）経由の`pmem_mmap`でPMEM領域をヒープにマップ。ランタイム停止後もヒープ状態を保持し再起動時に即座に復元（FR-404 AOTスナップショットと組み合わせ）。ライトバリアでPMEMへのflushを制御（`CLFLUSH`/`CLWB`命令）
- **根拠**: PMDK libpmemobj / Microsoft Project Cerberus / CXL Memory。プロセス再起動後のヒープ状態継続
- **難易度**: Very Hard

#### FR-409: HBM / Near-Memory Allocation (高帯域幅メモリ対応)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: HBM (High Bandwidth Memory) / CXL memory tiering環境でのヒープ階層化。ホットオブジェクト（頻繁にアクセスされるCLOSインスタンス・クロージャ）をHBMに配置。コールド/ラージオブジェクトを標準DRAMに配置。GC統計（アクセス頻度）に基づくHBM↔DRAM間の自動昇格/降格
- **根拠**: Linux Tiered Memory / HBM-DRAM tiering (Pond, ASPLOS 2023) / Intel AMX HBM。数値計算・行列演算ワークロードでの帯域幅活用
- **難易度**: Hard

#### FR-410: GC for WebAssembly (WasmGC対応)

- **対象**: `packages/backend/emit/src/wasm.lisp` (新規: `packages/backend/emit/src/wasm-gc.lisp`)
- **内容**: WasmGC proposal（2023 W3C標準化）の型と命令を使用してCLオブジェクトをWasmの管理対象GCオブジェクトとして表現。`struct.new` / `array.new` でCLOSインスタンス・ベクタを生成。Wasmランタイム（V8/SpiderMonkey/Wasmtime）がGCを代行。カスタムGCランタイムのWasm出力時のフットプリントを大幅削減
- **根拠**: WasmGC proposal (Phase 4) / Kotlin Wasm target / Dart Wasm target。Wasm上でCLを動かす際のGCランタイム不要化
- **難易度**: Hard

---

### Phase 93 — GC文字列最適化

#### FR-411: String Deduplication (文字列重複排除)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/strings.lisp`
- **内容**: GCマーキング時に文字列オブジェクトの内容ハッシュを計算し、同一内容の文字列が複数存在する場合にうち1つのバッキング配列のみ保持。他の文字列オブジェクトのバッキング配列ポインタを正規文字列へ付け替え（文字列オブジェクト自体は残る）。コンパイル済みコード中のリテラル文字列に特に有効
- **根拠**: G1 String Deduplication (JEP 192) / V8 string deduplication。文字列ヘビーなワークロードで5-20%ヒープ削減
- **難易度**: Medium

#### FR-412: String Interning Table Weak References (文字列インターンテーブルの弱参照化)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: `intern`はシンボルテーブルに強参照。文字列インターンテーブルは未実装
- **内容**: `string-intern`で登録した文字列をweakereferenceテーブルに保持。同一文字列の重複interningを防ぎつつ、参照がなくなれば自動回収。GCがweak tableを定期的に掃引して到達不能エントリを削除（エフェメロン、FR-246と連携）
- **根拠**: Java `String.intern()` / Ruby string freeze / SBCL symbol package table。インターンテーブルのメモリリーク防止
- **難易度**: Easy

---

### Phase 94 — GCデバッグ・検証インフラ

#### FR-413: GC Verification Pass (GC検証パス)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: デバッグビルド専用のGC後ヒープ整合性チェック。全オブジェクトのヘッダフォーマット検証。全ポインタスロットが有効ヒープアドレスを指しているか確認。mark word状態の一貫性チェック。旧空間→若空間ポインタが全てRS/カードテーブルで追跡されているか検証。`*gc-verify-after-collect*`フラグで有効化
- **根拠**: HotSpot `VerifyAfterGC` / V8 `--verify-heap` / Go `GODEBUG=gccheckmark=1`。GC実装バグの早期検出
- **難易度**: Easy

#### FR-414: GC Stress Testing Mode (GCストレステストモード)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: `*gc-stress-mode*`フラグで全割り当て毎にGCを強制発動。テスト実行時に全てのGCフェーズを毎命令触れるため、GCセーフでないコードを即座に検出。「間違ったアドレスで動くが稀にしか壊れない」バグを確実に再現。`nix run .#test` 経由の canonical テストスイートとの統合
- **根拠**: JVM `-XX:+StressGC` / Go `GOGC=off` + `runtime.GC()` loops / Rust miri。GC協調バグの確実な検出
- **難易度**: Easy

#### FR-415: Heap Object Graph Visualizer (ヒープオブジェクトグラフ可視化)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `cli/src/main.lisp`
- **内容**: ヒープスナップショット（FR-368）をDOT形式または`.hprof`形式でダンプするデバッグコマンド。オブジェクト参照グラフを可視化しメモリリークのルートパスを特定。`./cl-cc heap-dump --format=dot > heap.dot && dot -Tsvg heap.dot > heap.svg`
- **根拠**: JVM `jmap -histo` / V8 heap snapshot / heaptrack。開発者向けメモリ問題診断ツール
- **難易度**: Easy

---

### Phase 95 — 特化型配列・非ボックス化要素

#### FR-416: Unboxed Specialized Arrays (非ボックス化特化配列)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: `vm-make-array`はCLベクタとして表現。全要素が`t`型でGCポインタとして扱われ毎GCでスキャン対象
- **内容**: `(make-array n :element-type 'fixnum)` / `'double-float` / `'character` / `'bit`の特化配列をヒープに配置。ヘッダに要素型タグを記録。GCマークフェーズで特化配列のスキャンを**完全スキップ**（ポインタを含まないため）。要素アクセスはボックス化なしの生データ読み書き。ANSI CLの`array-element-type`要件を満たす最小限の型セット（fixnum/double/char/bit/byte）
- **根拠**: SBCL specialized arrays (`(vector fixnum)` = unboxed) / JVM primitive arrays / .NET value-type arrays。Lispの数値計算ワークロードでGCスキャン量を劇的に削減。行列・バイト列・文字列に直接効く
- **難易度**: Medium

#### FR-417: Pinned Unboxed Array Passing to FFI (非ボックス配列のFFI直接渡し)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-416, FR-407 (Pinning API)
- **内容**: `(with-pinned-objects (arr) (foreign-call arr))` で特化配列のデータポインタをCコードに直接渡す。特化配列はポインタ要素なしのため、ピン中もGCがヒープ内他オブジェクトを移動可能（ピンの局所的影響）。コピーレスFFIバッファとして機能
- **根拠**: SBCL `sb-sys:vector-sap` / JVM JNI `GetPrimitiveArrayCritical`。配列コピーなしのFFI呼び出し
- **難易度**: Easy

#### FR-418: Bit Vector GC Optimization (ビットベクタGC最適化)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**: `(make-array n :element-type 'bit)` の内部表現をワードパック（64要素/ワード）で最小化。GCスキャン不要フラグをヘッダに設定。ビット演算命令（AND/OR/XOR/NOT）をVM命令として追加しCPUのビット操作命令に直結。Bloom filterや集合演算の高速化
- **根拠**: SBCL simple-bit-vector / Java `BitSet` / .NET `BitArray`。ビット操作はメモリ効率最大化の典型
- **難易度**: Easy

---

### Phase 96 — オブジェクト不変性・GCバリア排除

#### FR-419: Immutable Object Declarations (不変オブジェクト宣言)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: `(declare (immutable x))` または `defconstant-struct` 相当の不変性宣言。コンパイラが不変と判断したオブジェクトへの全書き込みバリアを静的除去。不変オブジェクトはold→young問題を起こさないためカードテーブル更新不要。不変ステータスをオブジェクトヘッダのビットで表現し実行時チェックで保証
- **根拠**: Clojure persistent data structures (内部的に不変) / Racket `immutable?` / Java `record` (shallow immutability)。関数型スタイルのLispコードで書き込みバリアを大量排除
- **難易度**: Medium

#### FR-420: Functional Data Structure GC Optimization (永続データ構造最適化)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**: `cons`・リスト・ベクタで純関数的操作（`cons`, `append`, `map`等）のみ使用されるオブジェクトグラフを検出。このグラフ内のポインタは常にold→youngの方向（新しいオブジェクトが古いオブジェクトを参照する構造になりえない）なのでカードテーブル更新不要。Immutable Region内はwrite barrier完全フリー
- **根拠**: GHC's immutable heap (RTS GC write barrier elimination for CAFs) / Clojure persistent vectors。純関数型スタイルのGCオーバーヘッドをほぼゼロに
- **難易度**: Hard

#### FR-421: Read-Only Memory Mapping for Constants (定数読み取り専用マッピング)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/binary/src/macho.lisp`
- **内容**: コンパイル時に確定する定数データ（シンボル名文字列・コンパイル済みラムダリスト・コード定数プール）をバイナリの`__DATA_CONST`（Mach-O）/`.rodata`（ELF）セクションに格納し`mprotect(PROT_READ)`で保護。書き込み試みを即座にSIGSEGVで検出。GCスキャン不要（不変のためポインタ更新なし）
- **根拠**: SBCL static space / V8 read-only heap / HotSpot `MetaspaceRoots`。定数データへのGCコストゼロ化とメモリ保護の同時実現
- **難易度**: Medium

---

### Phase 97 — GC人間工学・自動設定

#### FR-422: GC Ergonomics / Auto-Configuration (GC自動設定)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `cli/src/main.lisp`
- **現状**: `*gc-young-size-words*` / `*gc-old-size-words*`をユーザーが手動設定する必要がある
- **内容**: 起動時にシステムRAM量（`sysctl hw.memsize` / `/proc/meminfo`）を照会し自動設定。デフォルトヒープ上限 = システムRAMの25%。JVM `-XX:+UseAdaptiveSizePolicy`相当の自動チューニングを有効化。コンテナ環境ではcgroupのメモリ制限（`/sys/fs/cgroup/memory.limit_in_bytes`）を優先。`--heap-max=4g` CLI引数でオーバーライド可能
- **根拠**: JVM ergonomics heuristics / Go `GOMEMLIMIT` (1.19+) / .NET GC auto-configuration。デフォルト設定でほとんどのワークロードが最適動作するように
- **難易度**: Easy

#### FR-423: Container-Aware Heap Sizing (コンテナ対応ヒープサイズ設定)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **依存**: FR-422
- **内容**: Docker / Kubernetes環境では`/sys/fs/cgroup/memory.limit_in_bytes`（v1）または`/sys/fs/cgroup/memory.max`（v2）からコンテナメモリ制限を読み取り。cgroupメモリ制限の70%をヒープ上限に設定（残り30%はメタスペース・JIT・スタック等）。`/proc/1/cgroup`の有無でコンテナ検出
- **根拠**: JVM JEP 415 Container-Aware GC / Go container awareness。OOMkillによるコンテナ強制終了の防止
- **難易度**: Easy

#### FR-424: GC Policy Selection (GCポリシー選択)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `cli/src/main.lisp`
- **内容**: `--gc=throughput|latency|memory` CLI引数でGCポリシー選択。`throughput`: 大きなヒープ・少ないGC頻度（バッチ処理向け）。`latency`: 小さなナーサリ・増分GC・Concurrent GC（対話的・サービス向け）。`memory`: 積極的GC・小ヒープ（組み込み・メモリ制限環境向け）。各ポリシーはパラメータセット（`*gc-young-size-words*`等）の構成済みプリセット
- **根拠**: JVM GC selector (`-XX:+UseG1GC` etc.) / .NET GC modes (workstation/server/background)。用途に合わせた一発設定
- **難易度**: Easy

---

### Phase 98 — GCペーサー・スループット制御

#### FR-425: GC Pacer / Allocation Pacing (GCペーサー)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: 割り当てはGC停止まで無制限。GCが追いつかない場合のメカニズムなし
- **内容**: Go GCペーサー方式: 割り当てレートとGCスループットを監視し、GCが「追いついている」状態を維持するために割り当てペースを動的制御。`*gc-target-utilization*`（デフォルト: GCP CPU使用率25%）目標。GC遅延時はTLAB割り当てに「assist work」（GCマーキング作業への参加）を混在させて割り当て速度を自然に制限
- **根拠**: Go GC Pacer (Go 1.5〜1.18で改良) / GHC GC work-based pacing。割り当てバーストによるGC停止の平滑化
- **難易度**: Hard

#### FR-426: GC Throughput Target (GCスループット目標)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: `*gc-time-ratio*` パラメータ（デフォルト: 99 = GCは総実行時間の1/99以下）。GC時間が目標を超えるとヒープを拡大してGC頻度を下げる（FR-391との連携）。目標に収まる場合はヒープを縮小してメモリを節約。JVM `-XX:GCTimeRatio=99`相当
- **根拠**: JVM Adaptive Size Policy GCTimeRatio / G1 pause fraction。メモリとスループットのトレードオフ自動最適化
- **難易度**: Medium

#### FR-427: Allocation Back-Pressure (割り当てバックプレッシャー)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: 割り当てレートが極端に高くGCが追いつかない場合（ヒープ残量<5%）にミューテータスレッドを一時的にブロック（`sleep`または条件変数待機）してGCに追いつく機会を与える。ブロック時間は指数バックオフ。完全OOM前の最後の防衛ライン。ブロック発生を`gc-assist-time`メトリクスに記録
- **根拠**: JVM GC pause-if-heap-full / Go mutator assist / Erlang GC back-pressure。OOMエラーよりもパフォーマンス一時低下を選ぶ
- **難易度**: Medium

---

### Phase 99 — GCフリー領域・クリティカルセクション

#### FR-428: without-gcing Critical Sections (GC禁止クリティカルセクション)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**: `(without-gcing &body body)` マクロ。body実行中は`*gc-inhibit*`フラグをセットしGCトリガーを抑制。body中に割り当て枯渇が発生した場合は実際のGCを保留し、bodyを抜けた直後に実行。リアルタイム処理・シグナルハンドラ・内部GCコード自体が使用する保護機構
- **根拠**: SBCL `without-gcing` / CCL `without-interrupts`。GC再入防止とシグナルハンドラの安全性
- **難易度**: Easy

#### FR-429: GC Safe Regions in Native Code (ネイティブコードのGCセーフ領域)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**: FFI呼び出し中（CコードがLisp VM外で動作中）はスレッドが「GCセーフ」状態であることをランタイムに通知。GCセーフスレッドはSTWで停止させずGCを続行可能（スタックスキャン不要のため）。FFI呼び出しの前後に`enter-gc-safe` / `leave-gc-safe`フェンスを生成。`leave-gc-safe`でGC開始確認とsafepoint処理
- **根拠**: JVM JNI critical sections / Go `runtime.entersyscall` / Python GIL release。長時間FFI中のGCを他スレッドが待たない
- **難易度**: Medium

#### FR-430: GC Inhibit During Signal Handlers (シグナルハンドラ中のGC抑制)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: POSIX シグナルハンドラ（SIGSEGV, SIGUSR1等）実行中は自動的に`*gc-inhibit*`を設定。シグナルハンドラがヒープを操作した場合でも一貫したGC状態を維持。ハンドラ復帰時にペンディングGCを確認して実行
- **根拠**: SBCL signal handler GC inhibit / Go signal handling during GC。シグナルハンドラでのGC再入を防ぐ
- **難易度**: Easy

---

### Phase 100 — メモリ順序付け・並行GC正確性

#### FR-431: Acquire-Release Barriers for GC (GC用Acquire-Releaseバリア)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **内容**: 並行GCの正確性に必要なメモリ順序付け。オブジェクト初期化完了後に**release store**（`MFENCE` x86 / `stlr` AArch64）でオブジェクトを参照可能に。オブジェクトポインタのロードに**acquire load**（`LDAR` AArch64 / x86は自動acquire）を使用。「オブジェクトが参照可能 → 内容も参照可能」の因果性保証
- **根拠**: Java Memory Model JEP 188 / C++11 `std::memory_order_acquire/release` / Arm Memory Model。並行GCでのデータレースによるポインタ破壊防止
- **難易度**: Hard

#### FR-432: Atomic Reference Updates for Concurrent GC (並行GC用アトミック参照更新)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: 並行マーキング中にミューテータがポインタを更新する場合、GCのマークビット更新と競合する可能性。ポインタの書き込みと読み取りをアトミック操作（CAS / atomic store/load）で実施。特にforwarding pointer設定（移動GC中）をCASで行い「誰が最初にforwarding ptrを書いたか」をCASの成功/失敗で判定
- **根拠**: ZGC atomic pointer update / Shenandoah CAS forwarding。並行relocate中のデータ競合解消
- **難易度**: Hard

#### FR-433: Memory Fence Placement in Write Barriers (ライトバリアのメモリフェンス配置)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**: SATB ライトバリア（`rt-gc-write-barrier`）のメモリ順序付けを最小化。x86-64はTotal Store Orderのためwriteバリアにfence不要（store-storeは自動順序付け）。AArch64はweaker modelのため`stlr`が必要な箇所を特定。バリアのfastpath（カードテーブル更新のみ）にfenceを入れずslowpath（SATB enqueue）のみfence
- **根拠**: G1 barrier fence analysis / ZGC no-barrier fastpath。バリアコストの最小化
- **難易度**: Hard

---

### Phase 101 — バリュータイプ・インライン割り当て

#### FR-434: Value Types / Inline Classes (バリュータイプ・インラインクラス)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/backend/runtime/src/heap.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**: `(defstruct (point (:inline t)) x y)` 相当の宣言で、インスタンスが配列やオブジェクトのスロット内に**ポインタなしでインライン展開**される型。`(make-array n :element-type 'point)` → Nコ分の(x,y)が連続した非ボックス配列。インラインのためGC参照なし、ヘッダなし、高いキャッシュ効率。identityなし（== は値比較）
- **根拠**: JEP 401 Project Valhalla Value Classes (JDK 23 preview) / C# `struct` / Rust stack-allocated structs。Javaが30年越しで解決しようとした「プリミティブの呪い」をLispで先行解決
- **難易度**: Very Hard

#### FR-435: Flattened Array of Inline Types (インライン型の平坦化配列)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **依存**: FR-434
- **内容**: `(make-array n :element-type 'point)` をインラインレイアウト（x0,y0,x1,y1,...）として格納。ポインタチェインなし・ヘッダなし・GCスキャン不要（FR-416と同様）。アクセスはオフセット計算のみ（`base + index * sizeof(point)`）。SIMD演算との親和性が高い
- **根拠**: JEP 401 flattened arrays / C# `Span<T>` value type arrays / Rust `[Point; N]`。数値計算・物理シミュレーション・ECSパターンでの性能向上
- **難易度**: Hard

---

### Phase 102 — クラス・コードアンロード

#### FR-436: Class Unloading (クラスアンロード)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: `defclass`定義はグローバルハッシュテーブル（`*class-table*`相当）に永続保持。クラス定義がGCされることはない
- **内容**: クラス定義オブジェクトを弱参照で保持。そのクラスのインスタンスが全て消滅し、かつクラスオブジェクト自体への強参照がなくなった場合にGCがクラス定義を回収。クラス定義回収時にメソッドキャッシュ・ディスパッチテーブル・コンパイル済みコードも連鎖回収。動的ロード/アンロードを繰り返すプラグインシステムに必要
- **根拠**: JVM classloader unloading / .NET AssemblyLoadContext unload / Ruby ObjectSpace。長時間稼働サーバーでのメタデータ肥大化防止
- **難易度**: Hard

#### FR-437: Method Cache Invalidation on Redefinition (再定義時メソッドキャッシュ無効化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: `defmethod`再定義時のキャッシュ無効化メカニズム不明
- **内容**: メソッドキャッシュ（ディスパッチテーブル）を世代カウンタで管理。`defmethod` / `defclass`の再定義時にグローバル世代カウンタをインクリメント。キャッシュヒット時に世代チェック（ミスマッチ→再解決）。インラインキャッシュ（JITコード中）への変更通知（コードパッチまたは`deoptimize`）
- **根拠**: V8 hidden class transition invalidation / SpiderMonkey inline cache invalidation / SBCL ctor cache flush。CLの動的再定義セマンティクスとJIT最適化の両立
- **難易度**: Medium

#### FR-438: JIT Code Reclamation on Function Redefinition (関数再定義時JITコード回収)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-379 (Code Cache Management)
- **内容**: `defun`で関数を再定義した場合、旧JITコードを参照するアクティブスタックフレームがなくなった時点でコードキャッシュから回収。スタックフレームのプログラムカウンタ範囲チェックで「使用中」を判定。GCが`code-object`の弱参照カウントを追跡し、全スタックフレームが退場したら回収
- **根拠**: JVM CodeCache sweeper / V8 code flushing / SpiderMonkey IonCode GC。REPLでの繰り返し再定義によるコードキャッシュ肥大化防止
- **難易度**: Hard

---

### Phase 103 — アイドル時GC・バックグラウンドGC

#### FR-439: Idle-Time Background GC (アイドル時バックグラウンドGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: プログラムがI/O待機・sleep・イベントループのidleハンドラにいる間にバックグラウンドGCを実行。`epoll_wait` / `kqueue`の待機時間をGC作業に充てる。Go `runtime.GC()` 的な「ミューテータが暇なときにGCを進める」方式。アイドルGCの進捗を`*gc-idle-work-fraction*`で制御（例: idle時間の50%をGCに）
- **根拠**: V8 Idle-time GC / Chromium idle tasks / Go background GC。GCがワークロードと重ならないアイドル期間を有効活用
- **難易度**: Medium

#### FR-440: GC Triggered by Memory Pressure from OS (OS起因メモリ圧迫によるGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: macOS `dispatch_source_t DISPATCH_SOURCE_TYPE_MEMORYPRESSURE` / Linux `eventfd` + cgroupメモリイベントで外部メモリ圧迫を検知。検知時にfull GCを実施しOSにメモリを返却（FR-392, FR-393と連携）。`SIGTERM`前の自主的クリーンアップにも利用
- **根拠**: iOS `UIApplicationDelegate applicationDidReceiveMemoryWarning` / Android `onTrimMemory` / macOS memory pressure API。コンテナOOMkill前の自主的メモリ解放
- **難易度**: Easy

#### FR-441: Periodic GC for Long-Running Processes (長時間プロセス定期GC)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: `*gc-periodic-interval-ms*`（デフォルト: 0=無効）で定期的なminor GCを発動。REPLセッション・長期間割り当てが少ない処理中でも定期的にナーサリをクリーンアップ。老化してnurseryに残り続けるオブジェクトの昇格機会を提供。タイマーはバックグラウンドスレッドまたはシグナル（SIGALRM）ベース
- **根拠**: Go runtime periodic GC trigger / JVM `-XX:GCTimeLimit` / .NET `GC.Collect(0, GCCollectionMode.Optimized)`。長時間idle後の最初のGCが巨大にならないように
- **難易度**: Easy

---

### Phase 104 — GC・型システム統合

#### FR-442: GC-Aware Type Inference Integration (GC-型推論統合)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/type/type/src/` (HMシステム)
- **内容**: 型システム（FR: HM gradual typing）からの型情報をGCが活用。型が`fixnum`と確定したスロットはGCポインタとして追跡しない。`(cons fixnum fixnum)` 型のconsセルはcar/cdrスキャン不要。型情報→GCスキャンマップの自動生成でスキャン量を削減
- **根拠**: GHC GC with type-based scavenging / MLton GC with type-precise heap。型情報とGCの統合で精確かつ高速なスキャン
- **難易度**: Hard

#### FR-443: Generational Hypothesis Validation (世代別仮説の検証・適応)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: 「若いオブジェクトは若くして死ぬ」仮説が成立しているかを実行時統計で継続検証。`promotion-ratio` = 若GC後に旧世代に昇格した割合を追跡。昇格率が異常に高い（例: >40%）場合は若世代サイズを縮小し旧世代GCをより頻繁に実行する適応的切り替え。世代別仮説が崩れるワークロード（全オブジェクトが長命）では単一世代GCに自動フォールバック
- **根拠**: JVM adaptive generation sizing / Go GC tuning heuristics。コンパイラ自身のような「多くのオブジェクトが長命」なワークロードへの適応
- **難易度**: Medium

#### FR-444: GC Interaction with Speculative JIT (投機JITとGC連携)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**: 型特殊化JITコードが「このオブジェクトはfixnum」と仮定してバリアを省略した場合、GCが型変更を検知したときにdeopt（FR-372）を発動。GCのmark phaseで型情報のinvalidationを確認し、stale assumptionを持つJITコードをdeoptimizerにキックする。GC統計が型仮定の妥当性フィードバックにもなる
- **根拠**: V8 type feedback + GC deopt / HotSpot C2 type assumptions invalidation。投機的最適化の安全性をGCが保証
- **難易度**: Very Hard


---

### Phase 105 — ヒープ内省・Walking API

#### FR-445: Heap Walking API (ヒープWalking API)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `cli/src/main.lisp`
- **内容**: `(do-heap-objects (obj &optional filter-type) &body body)` マクロ。GCルートから到達可能な全オブジェクトをトレースしながら`body`を実行。GCの tri-color マーキングを利用した安全な走査（走査中はSTW or GC協調）。`filter-type`で型絞り込み。ヒープ統計・メモリリーク検出・オブジェクトグラフ探索のための公開API
- **根拠**: JVM `HeapWalker` (JVMTI) / SBCL `sb-vm::map-allocated-objects` / Python `gc.get_objects()`。全ライブオブジェクトへの反復アクセスはGCツーリングの基盤
- **難易度**: Medium

#### FR-446: room / heap-census (ヒープセンサス)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `cli/src/main.lisp`
- **内容**: ANSI CL `room` 関数の実装: 各型・サイズクラス別のオブジェクト数・バイト数を集計して標準出力に表示。`(room)` = 概要、`(room t)` = 詳細（型別ブレークダウン）。ヒープウォーク（FR-445）を使って全オブジェクトを型タグ別に集計。`gc-census` コマンド: 型別ヒストグラムをJSON出力
- **根拠**: ANSI CL spec `room` / SBCL `room` / Clojure `clojure.core/count-objects`。メモリ使用量の最初の診断ツールとして不可欠
- **難易度**: Easy

#### FR-447: Object Lifecycle Hooks (オブジェクトライフサイクルフック)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: `rt-gc-register-alloc-hook` / `rt-gc-register-death-hook` で割り当て・死亡時コールバックを登録。割り当てフックは型フィルタ付きで特定型のオブジェクト生成を追跡。死亡フックはファイナライザ（FR-337）より低レベルで、GCスキャン中に到達不能と判定された直後に呼ばれる。デバッグ用途（アロケーション追跡・リーク検出）に特化
- **根拠**: JVM JVMTI `ObjectFree` event / .NET `GC.RegisterForFinalization` observer / Valgrind object tracking。プロファイラ・デバッガとの統合点
- **難易度**: Easy

---

### Phase 106 — 弱ハッシュテーブル・GC協調コレクション

#### FR-448: GC-Cooperative Weak Hash Tables (GC協調弱ハッシュテーブル)

- **対象**: `packages/engine/vm/src/hash.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: `vm-make-hash-table`は通常の強参照ハッシュテーブルのみ。FR-412は文字列インターン専用の弱テーブル
- **内容**: `(make-hash-table :weakness :key)` / `:value` / `:key-and-value` / `:key-or-value` の4モードをサポート。内部実装はエフェメロン（FR-246）の配列として表現。GCマーキングフェーズでキー到達可能性を判定し、到達不能エントリをスイープフェーズで自動削除。`hash-table-weakness`アクセサ追加
- **根拠**: SBCL `make-hash-table :weakness` / Java `WeakHashMap` / Python `weakref.WeakKeyDictionary`。メモ化・キャッシュ・属性テーブルのメモリリーク防止の標準機能
- **難易度**: Medium

#### FR-449: Identity-Based Weak Tables (同一性ベース弱テーブル)

- **対象**: `packages/engine/vm/src/hash.lisp`, `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-448
- **内容**: `eq`ハッシュテーブル（同一性比較）の弱参照版。GCがオブジェクトを移動した場合、テーブルのキーアドレスを新アドレスに更新（FR-395のidentityハッシュコードと連携）。移動GCとeqハッシュの整合性維持。外部プロパティリスト（オブジェクトに外部からプロパティを付与するパターン）の実装基盤
- **根拠**: SBCL `make-hash-table :test 'eq :weakness :key` / Common Lisp image persistence。CLのシンボルattribute tableに必要
- **難易度**: Medium

---

### Phase 107 — CPS継続オブジェクトのGC最適化

#### FR-450: Continuation Object Lifetime Analysis (継続オブジェクト寿命解析)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: cl-ccはCPS変換（`packages/engine/compile/src/cps.lisp`）で全関数をcontinuation-passing styleに変換。継続オブジェクトは通常クロージャとしてヒープ割り当て
- **内容**: CPSグラフ解析で継続の寿命を推定。末尾位置の継続（tail continuation）は呼び出し元が消滅後に不要 → スタック割り当て候補。再帰関数の継続チェーンが深くなる場合の早期GC機会検出。CPS変換生成コードに継続特有のメタデータを付与してGCが活用できるようにする
- **根拠**: SML/NJ CPS closure analysis / Chicken Scheme continuation GC / Gambit Scheme continuation lifetime。CPS採用コンパイラに特有の継続オブジェクト爆発問題への対処
- **難易度**: Hard

#### FR-451: Stack Allocation for Linear Continuations (線形継続のスタック割り当て)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/compile/src/codegen.lisp`
- **依存**: FR-450
- **内容**: 「1回しか呼ばれない継続」（線形継続 = `call-with-current-continuation`を使わない通常の関数呼び出しから生成される継続）をスタックフレームとして割り当て。通常の`defun`から生成されるCPS継続は全て線形。スタック割り当てにより継続オブジェクトのヒープGCコストをゼロに。Chicken Schemeの「stack-allocated continuation」相当
- **根拠**: Chicken Scheme stack allocation / Larceny SML/NJ continuation optimization / Chez Scheme CPS optimization。CPS変換の主要コスト（継続ヒープ割り当て）を排除
- **難易度**: Hard

#### FR-452: Continuation Stack Overflow Handling (継続スタックオーバーフロー処理)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: スタック割り当て継続（FR-451）がスタックオーバーフローする深い再帰の場合のフォールバック。スタック残量が閾値以下でヒープ割り当てに切り替え（Cheney on the MTA方式）。スタックオーバーフロー時に現在のスタック全体をヒープにコピーして処理継続。GCがコピー後のスタック内ポインタを更新
- **根拠**: Chicken Scheme "Cheney on the MTA" / Gambit Scheme stack overflow recovery。深い再帰でもCPS継続の安全性保証
- **難易度**: Very Hard

---

### Phase 108 — ライトバリア最適化追加

#### FR-453: Write Barrier Batching / Coalescing (ライトバリア一括処理)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: `rt-gc-write-barrier`は毎ストアでカードテーブルを即時更新
- **内容**: per-thread「バリアバッファ」（固定サイズキューorログ）にポインタ書き込みアドレスを蓄積。バッファ満杯時に一括でカードテーブル更新（ソート+重複除去）。GCサイクル開始時にもフラッシュ。キャッシュラインへの重複アクセスを排除しカードテーブル更新コストを削減。SATBキューとバリアバッファを統合
- **根拠**: G1 Dirty Card Queue / Shenandoah SATB buffer / ZGC store buffer。バリア1回あたりのコストをキャッシュフレンドリーに削減
- **難易度**: Medium

#### FR-454: Remembered Set Representation Selection (RS表現の選択)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-356
- **内容**: リージョン別Remembered Setの内部表現を動的に選択。スパース時: sorted array（低メモリ、O(log N)探索）。中密度: open-addressing hash set（O(1)平均）。高密度（>50%充填率）: bitmap（最速スキャン、固定メモリ）。充填率モニタリングで自動切り替え。表現切り替え時のRS再構築コストを償却
- **根拠**: G1 Sparse/Fine/Coarse RS representation / Shenandoah RSSet tiering。メモリ使用量とスキャン速度のトレードオフを動的最適化
- **難易度**: Medium

---

### Phase 109 — ヒープ隔離・マルチテナント

#### FR-455: Isolated Heaps for Sandboxing (サンドボックス用隔離ヒープ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: 複数の独立した`rt-heap`インスタンスを並行して運用。各プラグイン・スクリプト実行コンテキストが専用ヒープを持つ。コンテキスト終了時にヒープ全体を一括解放（オブジェクト単位のGCなし）。ヒープ間ポインタを禁止（静的チェック）することでヒープ独立性を保証。Worker Isolates（Dart）/ Realm（Swift）相当
- **根拠**: Dart Isolates / V8 Isolate / .NET `LoadContext` / WebAssembly module per-instance memory。マルチテナント環境でのメモリ漏洩・干渉防止
- **難易度**: Hard

#### FR-456: Cross-Heap Message Passing with Deep Copy (ヒープ間ディープコピー)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-455
- **内容**: 隔離ヒープ間でオブジェクトを渡す場合のディープコピーAPI。`rt-copy-to-heap`がオブジェクトグラフを再帰的に新ヒープへコピー。文字列・シンボル・数値は共有（不変のため）、mutableオブジェクトはコピー。Transferable Objects（ArrayBuffer等）はゼロコピーで所有権移転
- **根拠**: Dart Isolate message passing / Web Workers `postMessage` / Erlang process message passing。隔離ヒープ間通信の安全性と効率
- **難易度**: Hard

---

### Phase 110 — 動的変数束縛スタックのGC

#### FR-457: Dynamic Binding Stack as GC Roots (動的束縛スタックのGCルート)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: `vm-state`の特殊変数束縛（dynamic variables）がGCルートとして適切に登録されているか不明。`vm-state-special-bindings`スロット（もしあれば）がGCルートに含まれる必要
- **内容**: CL動的変数（`defvar` / `let` でのspecial binding）の束縛スタックをGCルートに追加。束縛スタックは`(symbol . value)`のalistまたは専用スタック構造。各`let`/`dynamic-binding`のエントリをGCがスキャン。スレッド切り替え時に束縛スタックが正しくsave/restoreされることをGCが保証
- **根拠**: SBCL binding stack as GC roots / CCL special binding GC / LispWorks dynamic variables。CLの特殊変数機構の正確なGC処理
- **難易度**: Medium

#### FR-458: Special Variable Binding Stack Optimization (特殊変数束縛スタック最適化)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**: 特殊変数束縛のunwind時コスト削減。束縛スタックのtop-of-stack pointerを1回のデクリメントでunwind（個別エントリのGCルート解除なし）。スレッドローカルな束縛はGCバリア不要（per-thread束縛はそのスレッドのルートに自動含まれる）。束縛スタックの最大深度統計を`rt-gc-stats`に追加
- **根拠**: SBCL binding stack unwind optimization / ECL binding stack。特殊変数ヘビーなコードのGCオーバーヘッド削減
- **難易度**: Easy

---

### Phase 111 — 二段階ファイナライゼーション・復活防止

#### FR-459: Two-Phase Finalization Algorithm (二段階ファイナライゼーション)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **現状**: FR-337でファイナライザ実行順序を定義するが、「ファイナライザ登録オブジェクトの到達不能検出 → ファイナライズキューへの移動 → ファイナライザ実行 → 2回目のGCで回収」という2サイクル処理が明示されていない
- **内容**: フェーズ1（マーキング）: 到達不能だがファイナライザ登録済みオブジェクトを「finalizable」としてマーク。これらを「一時的に到達可能」に戻し（復活）ファイナライゼーションキューに入れる。フェーズ2（次のGCサイクル）: ファイナライザ実行済みで再度到達不能のオブジェクトを実際に回収。2サイクル必要なことを`rt-gc-stats`に記録
- **根拠**: Java GC finalization two-pass / .NET suppressed finalization / Python `tp_finalize`。ファイナライザ実行中の安全性とオブジェクト復活セマンティクスの正確な実装
- **難易度**: Medium

#### FR-460: Finalization Resurrection Prevention (ファイナライゼーション復活防止)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-459
- **内容**: ファイナライザが自身を別のデータ構造に登録して「復活」した場合の検出と再ファイナライゼーション防止。各オブジェクトに「ファイナライゼーション済み」フラグをヘッダに追加。復活後に再度到達不能になったオブジェクトはファイナライザ再実行なしで回収。`suppress-finalization`相当のAPIでファイナライゼーション無効化
- **根拠**: Java `Object.finalize()` resurrection / .NET `GC.SuppressFinalize` / Python resurrection semantics。ファイナライザの無限復活ループ防止
- **難易度**: Easy

---

### Phase 112 — madvise・OS ヒントの活用

#### FR-461: madvise Sequential Hints for GC Scan (GCスキャン用madviseシーケンシャルヒント)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: GCのマーキング・スキャンフェーズでヒープ領域を線形走査する直前に`madvise(MADV_SEQUENTIAL)`を発行。カーネルの先読みを最大化（デフォルトはMADV_NORMAL）。スキャン後に`madvise(MADV_NORMAL)`で復元。AArch64では`madvise(MADV_WILLNEED)`で対象ページを明示的にプリフェッチ。GCスキャンのページフォルトによる遅延を排除
- **根拠**: JVM CMSの`madvise`利用 / Go `sysHugePage`ヒント / V8 heap scanner hints。メモリスキャン帯域幅の最大活用
- **難易度**: Easy

#### FR-462: Huge Page Auto-Promotion for Hot Regions (ホットリージョンのHuge Page自動昇格)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **依存**: FR-288 (Large Page Support)
- **内容**: 頻繁にアクセスされるヒープリージョン（コードキャッシュ・若世代hot zone）を`madvise(MADV_HUGEPAGE)`でTHP（Transparent Huge Pages）対象に指定。アクセス頻度の低いコールドリージョンは`MADV_NOHUGEPAGE`で標準4KBに維持。khugepaged（Linuxカーネル）との協調でTHP使用量を最適化。`/proc/[pid]/smaps`でHuge Page使用状況を監視
- **根拠**: PostgreSQL `madvise(MADV_HUGEPAGE)` / MongoDB THP tuning / JVM `-XX:+UseTransparentHugePages`。hot regionのTLBミスを2MB粒度で削減
- **難易度**: Easy

#### FR-463: Memory Defragmentation via mremap (mremapによるメモリデフラグ)

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**: Linux `mremap(MREMAP_MAYMOVE)`でヒープ領域を仮想アドレス空間内で移動。フラグメントした仮想アドレス空間を物理コピーなしで再配置。リージョンベースヒープ（FR-355）でリージョンを連続アドレスに集約する際に活用。物理メモリコピーなし（ページテーブル操作のみ）で高速
- **根拠**: Linux `mremap` / QEMU memory ballooning / KVM virtio-mem。仮想アドレス空間フラグメンテーションの解消
- **難易度**: Medium


---

### Phase 113 — ヒープイメージ保存・復元

#### FR-464: Heap Image Save (ヒープイメージ保存)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`, `cli/src/main.lisp`
- **現状**: FR-404でコンパイル時AOTスナップショットを定義。しかし**実行中のLispイメージ**（ライブオブジェクト・マクロ定義・ロード済みコード込み）をファイルに保存する`save-lisp-and-die`相当機能は未定義
- **内容**: GCルートから到達可能な全オブジェクトをファイルにシリアライズ。ポインタをファイル内オフセットに変換（relocation table）。シンボル・パッケージ・クラス・メソッドテーブルを含む完全なランタイム状態を保存。`./cl-cc --save-image cl-cc.image` で実行可能イメージ生成
- **根拠**: SBCL `save-lisp-and-die` / CLISP `saveinitmem` / Clozure CL `save-application`。CLにとって起動時間短縮の最重要機能。全ライブラリをロード済みのイメージをダンプして次回起動を数ms以下に
- **難易度**: Hard

#### FR-465: Heap Image Restore / Relocating Loader (ヒープイメージ復元・再配置ローダー)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `cli/src/main.lisp`
- **依存**: FR-464
- **内容**: 保存イメージをメモリにロードし、ポインタをロードアドレスに合わせて再配置（ASLR対応のrelocation）。シンボルテーブル・クラス階層・メソッドキャッシュをGCに再登録。JITコード（FR-379）のアドレスも再配置。イメージのマジックナンバー・バージョン・アーキテクチャ互換性チェック
- **根拠**: SBCL core file relocating loader / ECL `.fas` image loading。イメージ保存とセットで完結する機能
- **難易度**: Hard

#### FR-466: Incremental Heap Snapshot for Live Systems (ライブシステムの増分スナップショット)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **依存**: FR-464
- **内容**: 実行中のプロセスを停止せずに差分ヒープスナップショットを取得（Copy-on-Write fork方式 or GC協調方式）。前回スナップショットから変更されたオブジェクトのみをファイルに追記。長時間稼働サービスのホットリロード・デプロイ（Erlang `code:load_file`相当）の基盤
- **根拠**: Erlang hot code reload / CRuby `ObjectSpace.dump_all` / V8 snapshot incremental。無停止デプロイの前提インフラ
- **難易度**: Very Hard

---

### Phase 114 — ホット/コールドオブジェクト分離

#### FR-467: Profile-Guided Object Placement (プロファイル誘導オブジェクト配置)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**: PGO（Profile-Guided Optimization）データを使いGCコピー時にホットオブジェクトを同一キャッシュライン・ページに集約。頻繁にアクセスされるオブジェクト群をヒープ先頭側に配置しTLBカバレッジを最大化。コールドオブジェクト（稀にアクセス）をヒープ末尾に分離しGCスキャンで早期終了可能に
- **根拠**: HotSpot G1 Eden survivor ordering / V8 object placement heuristics / Facebook HHVM hot/cold splitting。アクセス頻度に応じたデータ局所性の最大化
- **難易度**: Hard

#### FR-468: GC Copy Order as Cache Warm-Up (GCコピー順序によるキャッシュウォームアップ)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: Cheneyコピーの幅優先スキャン順を深さ優先または呼び出しグラフトポロジカル順に変更。関数とその直接呼び出し先を同一キャッシュライン近傍に配置。CLOSジェネリック関数・メソッド・次呼び出しされる確率が高いオブジェクトを近接配置。GC後の「初回実行キャッシュミスストーム」を抑制
- **根拠**: Deutsch-Schorr-Waite DFS copying / HotSpot CMS foreground GC ordering / JikesRVM GC copy ordering。GC直後のキャッシュウォームアップコスト削減
- **難易度**: Hard

#### FR-469: Generation-Based Object Aging Statistics (世代別オブジェクト老化統計)

- **対象**: `packages/backend/runtime/src/gc.lisp`
- **内容**: 各オブジェクトが何回minor GCを生き残ったかを`age`フィールド（既存FR-394 mark word）で追跡。age分布ヒストグラムを`rt-gc-stats`に追加。age > threshold で旧世代昇格する現行ロジックに加え、「同一割り当てサイトから生成されたオブジェクトの平均age」を統計化。割り当てサイト別長命オブジェクト特定
- **根拠**: JVM GC age histogram / G1 `-XX:+PrintGCDetails`。世代別仮説（FR-443）の実測検証ツール
- **難易度**: Easy

---

### Phase 115 — CL固有GC課題

#### FR-470: Adjustable Array GC Handling (可変長配列のGC処理)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: `vm-make-array`は固定サイズ配列のみ。ANSI CLの`make-array :adjustable t`・`adjust-array`・fill pointerは未定義
- **内容**: `adjust-array`で配列サイズ変更時の旧バッキングストア回収。新旧両方の配列オブジェクトが同一GCサイクルに存在する過渡状態を正しく処理。displaced array（`make-array :displaced-to`）の参照先が回収されないようにGCルート追加。fill pointer配列のfill pointer以降の要素はスキャン不要（明示的nil化 or スキャン範囲制限）
- **根拠**: SBCL adjustable vector / ANSI CL spec §15.1.2。CLの標準配列機能とGCの整合性
- **難易度**: Medium

#### FR-471: Open Stream / Port as GC Finalizable Resource (オープンストリームのGC管理)

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: `(open ...)` で作成したストリームオブジェクトが到達不能になったときに自動`close`を実行するファイナライザを登録。ファイルディスクリプタリーク防止。ただし`close`のタイミングは非決定的なため、`with-open-file`の使用を推奨する警告機構も追加。ファイナライザ経由でcloseされたストリームをリソース統計に記録
- **根拠**: SBCL stream finalization / CPython `ResourceWarning` / Java `FileInputStream.finalize()`。GCによるリソースリーク防止の最後の砦
- **難易度**: Easy

#### FR-472: Hash Consing / Structure Sharing via GC (GCによる構造共有・ハッシュコンシング)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**: 同一内容のimmutableオブジェクト（リスト・ベクタ・文字列）を1つのヒープ表現に統合するハッシュコンシング。`(cons a b)` 生成時に内容ハッシュテーブルを検索し既存等値オブジェクトがあれば新規割り当てなしでその参照を返す。GCがweak hash tableでハッシュコンシングテーブルを管理（到達不能になれば自動削除）。コンパイラ中間表現の重複ASTノード共有に特に有効
- **根拠**: SBCL cons hashing / Lean4 hash consing / Coq kernel hash consing。コンパイラ内部での同一サブ式の重複排除。cl-ccのAST・CPS変換結果に直接適用可能
- **難易度**: Medium

#### FR-473: Package System GC Integration (パッケージシステムとGCの統合)

- **対象**: `src/package.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: パッケージ（`defpackage`）とそのシンボルテーブルのGC処理。パッケージ自体が到達不能になった場合（`delete-package`後）にインターンシンボルを連鎖回収。シンボルへの弱参照でシンボルが「パッケージのみから到達可能」になったら回収（unintern不要化）。gensymで生成した一時シンボルの自動回収
- **根拠**: SBCL package GC / Chicken Scheme symbol GC / gensym cleanup。大量gensymを使うマクロ展開後のシンボルテーブル肥大化防止
- **難易度**: Medium

