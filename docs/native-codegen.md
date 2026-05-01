# Native Backend: Code Generation

Native backend, architecture integration, register allocation, instruction scheduling, hardware instructions, native code quality, code layout, cache optimization, SIMD, code generation/linker, MIR/instruction selection.

---

### Phase 4 — ネイティブバックエンド層

#### FR-008: Float Unboxing (ネイティブ層)

- **対象**: `packages/runtime/src/value.lisp` + `packages/emit/src/x86-64-codegen.lisp`
- **既存**: NaN-boxingスキームが完備 (`value.lisp`)、float演算命令が充実 (`vm/primitives.lisp`)
- **内容**:
  - float型と推論されたレジスタはXMMレジスタに直接配置
  - ヒープオブジェクトとして boxしない
- **依存**: FR-004

#### FR-009: Inline Caching for Generic Functions

- **対象**: `packages/vm/src/vm.lisp` の `vm-dispatch-generic-call`
- **現状**: 毎回フルメソッド解決 (`vm-get-all-applicable-methods`)
- **設計**:
  - `vm-generic-call` 構造体に `:ic-cache` スロット追加 (`(cons specializer-key method-closure)`)
  - キャッシュヒット: specializer-key一致 → 直接メソッドクロージャ呼び出し
  - キャッシュミス: フル解決 + キャッシュ更新
  - 無効化: `register-method` 呼び出し時にキャッシュクリア
- **形式**: Monomorphic IC (1エントリ) → 将来 Polymorphic IC (N=4) へ拡張可能

#### FR-015: Block Compilation (モジュールレベルインライン化)

- **対象**: `packages/cli/src/main.lisp` + コンパイルパイプライン
- **内容**:
  - ファイル単位でのクロス関数インライン化
  - `flet`/`labels` の呼び出しを関数境界を跨いでインライン展開
  - SBCLの Block Compilation (`sb-c:*block-compile*`) 相当
  - 効果: ローカル関数の呼び出しオーバーヘッド完全除去

#### ✅ FR-016: Dead Store Elimination (DSE)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **内容**:
  - 書き込み後に読まれることなく上書き・破棄される変数代入を除去
  - DCE (`opt-pass-dce`) の補完: DCEは「使われない計算」、DSEは「上書きされる書き込み」を対象
  - ループ内での冗長なストア除去に特に有効

---

### Phase 12 — アーキテクチャ統合

#### FR-057: MIR Pipeline Integration

- **対象**: `packages/mir/src/mir.lisp` + `packages/compile/src/codegen.lisp`
- **内容**:
  - MIR層 (`packages/mir/src/mir.lisp`) はSSA CFGインフラが完備しているが本番パイプラインから切断されている
  - VM命令 → MIR lowering → MIR最適化 → ターゲット命令生成のパイプラインを接続
  - 接続により: 命令スケジューリング、レジスタ割り当て改善、SSAベース最適化が全て利用可能に
  - 最大30%のネイティブコード性能改善が見込まれる
- **難易度**: Very Hard

#### FR-058: Type Feedback PGO (プロファイルガイド最適化)

- **対象**: `packages/vm/src/vm.lisp` (IC計測) + コンパイルパイプライン
- **内容**:
  - IC (Inline Cache) に型出現頻度カウンタを追加
  - プロファイルデータを第2コンパイルパスにフィードバック
  - 最も頻繁に出現する型を優先してインライン化・特化
  - AOT版PGO: 初回実行→プロファイル収集→再コンパイル
- **難易度**: Hard

---

### Phase 13 — レジスタ割り当て・命令スケジューリング

#### ✅ FR-059: Register Coalescing

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: `vm-move` 命令のソースと宛先に同一物理レジスタを割り当て、move命令を消去
- **根拠**: 現状のregalloc は多数の `vm-move` を機械語レベルに残す
- **難易度**: Medium

#### ✅ FR-060: Register Hints / Preference-Based Allocation

- **対象**: `packages/emit/src/regalloc.lisp`, `packages/emit/src/calling-convention.lisp`
- **内容**: `calling-convention` の `:arg-registers` を割り当て時に優先考慮
- **根拠**: CC定義は存在するが `linear-scan-allocate` が無視している
- **難易度**: Medium

#### FR-061: Graph Coloring Register Allocation (Chaitin-Briggs)

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: 干渉グラフ彩色による最適レジスタ割り当て（線形スキャンより高品質）
- **難易度**: Hard

#### ✅ FR-062: Rematerialization

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: スピルの代わりに `vm-const` 等の安価な命令を使用箇所で再計算
- **難易度**: Easy

#### FR-063: Live Range Splitting

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: 長いライブ区間を分割し、ホット領域のみスピルを最小化
- **難易度**: Hard

#### ✅ FR-064: Biased Spill Selection (Belady's OPT)

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: 次の使用箇所が最も遠い区間を優先スピル（現状は最長末尾を選択）
- **難易度**: Easy

#### ✅ FR-065: Caller-Save/Callee-Save Aware Spilling

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: call命令を跨ぐ長寿命値をcallee-savedレジスタに優先配置
- **根拠**: `regalloc.lisp:274` 付近にTODOコメントあり
- **難易度**: Medium

#### ✅ FR-066: Two-Address Instruction Lowering

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **内容**: `dst==lhs` が必要なx86-64命令の前にcopyを挿入してRA時の混乱を防止
- **難易度**: Easy

#### FR-067: Pre-RA List Scheduling

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64.lisp`
- **内容**: レジスタ割り当て前の命令並べ替えでILPを露出
- **難易度**: Hard

#### FR-068: Post-RA Instruction Scheduling

- **対象**: バックエンド
- **内容**: 物理レジスタ確定後にメモリレイテンシを隠蔽する命令並べ替え
- **難易度**: Hard

#### FR-069: Dependency-Aware Peephole Scheduling

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/prolog/src/prolog.lisp`
- **内容**: Read-after-Writeに基づく命令ペア/トリプルの局所的並べ替え
- **難易度**: Medium

#### ✅ FR-070: Return Value in Return Register (NRVO)

- **対象**: `packages/emit/src/regalloc.lisp`, `packages/emit/src/calling-convention.lisp`
- **内容**: 関数の戻り値をABIのリターンレジスタに直接配置し、post-call moveを除去
- **根拠**: CC構造体に `:return-register` 定義あるが割り当て時に未活用
- **難易度**: Easy

#### ✅ FR-071: Parameter Register Recycling

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: 引数レジスタがdead後にローカル一時変数として再利用
- **難易度**: Easy

#### FR-072: Shrink-Wrapping (Callee-Saved Register)

- **対象**: バックエンド
- **内容**: callee-savedレジスタのsave/restoreを実際の使用箇所の支配点まで遅延
- **難易度**: Hard

#### FR-073: Multiple Values via Registers (≤3 values)

- **対象**: `packages/vm/src/vm.lisp:260-291`, `packages/compile/src/codegen.lisp`
- **内容**: `(values a b)` の静的既知2値返却をリスト割り当てなしのレジスタ渡しに変換
- **根拠**: 現状 `vm-values-list` に毎回リスト生成 (`vm.lisp:714`)
- **難易度**: Medium

---

### Phase 17 — ハードウェア命令活用

#### ✅ FR-097: POPCNT / i64.popcnt for logcount

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/wasm.lisp`
- **内容**: ANSI CL `(logcount x)` を `POPCNT`(x86-64) / `i64.popcnt`(Wasm) に直接マッピング
- **難易度**: Low

#### ✅ FR-098: BSR/BSF / clz/ctz for integer-length

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/wasm.lisp`
- **内容**: `(integer-length x)` → `BSR+adjust`(x86) / `i64.clz`(Wasm)
- **難易度**: Low

#### FR-099: FMA (Fused Multiply-Add)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64.lisp`
- **内容**: `(+ (* a b) c)` パターンを単一 `VFMADD`/`FMADD` 命令に変換
- **難易度**: Low-Medium

#### ✅ FR-100: Wasm `local.tee` Fusion

- **対象**: `packages/emit/src/wasm-trampoline.lisp`
- **内容**: `local.set` + 即時 `local.get` ペアを `local.tee` 単命令に融合
- **難易度**: Low

#### FR-101: Wasm `call_ref` vs `call_indirect`

- **対象**: `packages/emit/src/wasm.lisp`
- **内容**: グローバルfuncrefテーブル経由の `call_indirect` をGC提案の型付き `call_ref` に置換
- **難易度**: Medium

---

### Phase 31 — ネイティブコード品質

#### FR-171: LEA for Address Computation (x86-64)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **現状**: アドレス計算はMOVベースのみ（`x86-64-codegen.lisp:100-111`）。LEA命令の生成なし
- **内容**: `a + b*scale + disp`パターンをLEA命令に変換。フラグレジスタを破壊せず加算+シフトを1命令で実行
- **根拠**: LEAはx86-64で最も多用途な命令。GCC/LLVMは算術にもLEAを積極使用
- **難易度**: Medium

#### FR-172: BMI/BMI2 Bit Manipulation Instructions

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **現状**: ビット操作は手動シフト+マスクのみ（`x86-64-codegen.lisp:699-726`）。POPCNT/LZCNT/TZCNT/PEXT/PDEP未対応
- **内容**: `integer-length` → LZCNT、`logcount` → POPCNT、ビットフィールド抽出 → PEXT に変換。CPUID/機能フラグでBMI対応を検出
- **根拠**: `vm-integer-length`/`vm-logcount`（`vm-numeric.lisp:98-134`）がホストCL委譲のまま
- **難易度**: Medium

#### FR-173: Scaled Addressing Modes [base+index*scale+disp]

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **現状**: `[base]`と`[base+disp8]`のみ対応。スケールドインデックス`[base+index*scale]`未実装
- **内容**: 配列アクセス`(aref arr idx)`のコードパターンで`[base+index*8+header_offset]`を1命令で表現。追加のADD命令を除去
- **根拠**: x86-64はscale={1,2,4,8}のSIBバイトをネイティブサポート。cl-ccは配列アクセスで別途加算命令を生成
- **難易度**: Medium

#### FR-174: Native-Level Peephole Optimization (Post-Emit)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: ペアホール最適化はVMレベルのみ（`prolog.lisp`）。機械語生成後のペアホールなし
- **内容**: emit後の命令列から冗長パターンを除去: `MOV R,R`の除去、`MOV R1,R2; MOV R2,R1`の除去、連続PUSH/POPの除去、CMP+分岐の融合
- **根拠**: GCCのpass2ペアホール、LLVMのMachineInstCombine。VMペアホールでは機械語レベルの冗長性を検出不可
- **難易度**: Medium

#### FR-175: Instruction Selection Framework (BURS/Maximal Munch)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: VM命令→機械語の1:1ナイーブマッピング。複数VM命令をまたぐパターンマッチなし
- **内容**: ツリーパターンマッチング（Maximal Munch方式）によるVM→機械語変換。`vm-add(R1, vm-mul(R2, 8))`→`LEA R1, [R2*8]`のように木パターンを認識して最適命令を選択
- **根拠**: LLVM SelectionDAG / GCC BURS。命令選択は最適コード生成の核心
- **難易度**: Very Hard

#### FR-176: Custom Calling Conventions for Internal Functions

- **対象**: `packages/emit/src/calling-convention.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: すべての関数がSystem V AMD64 ABI / AAPCS準拠（`calling-convention.lisp`）。内部関数もフルABI遵守
- **内容**: モジュール内部の非公開関数にカスタム呼び出し規約を適用。callee-savedレジスタの削減、引数の柔軟なレジスタ配置、不要なフレームポインタ省略
- **根拠**: LLVMの`fastcc`/`coldcc`。内部関数は外部ABIに従う必要がない
- **難易度**: Hard

#### ✅ FR-177: Callee-Save Register Elimination

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: x86-64は常に6レジスタPUSH（`x86-64-codegen.lisp:1105-1111`）、AArch64は常に6 STP（`aarch64-codegen.lisp:369-382`）
- **内容**: 関数本体のレジスタ使用状況を解析し、使用されないcallee-savedレジスタのsave/restoreを省略
- **根拠**: 葉関数や短い関数では大半のcallee-savedレジスタが未使用。FR-002の葉関数検出と連携
- **難易度**: Easy

#### ✅ FR-178: Red Zone Usage (x86-64)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/x86-64-encoding.lisp`
- **現状**: ✅ 完了 — x86-64エンコーダが `RSP` ベースメモリオペランド用の SIB バイトを生成し、葉関数かつ小規模 spill（16 slot 以下）の場合は spill load/store を `RSP` 基準の red zone に配置して `RBP` フレームを省略する。
- **内容**: 葉関数でRSP以下128バイトのred zone（System V AMD64 ABI保証）を活用。フレームポインタ・RSP調整を完全省略
- **根拠**: x86-64 ABIがred zoneを保証。葉関数のプロローグ/エピローグを0命令化
- **難易度**: Easy

---

### Phase 34 — コードレイアウト・キャッシュ最適化

#### FR-186: Function Reordering for Cache Locality

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/binary/src/macho.lisp`
- **現状**: 関数はソース順に配置。コールグラフに基づく配置最適化なし
- **内容**: コールグラフの頻度情報（PGO）またはトポロジカル順に基づいて、相互呼び出し頻度の高い関数をメモリ上で隣接配置。I-cache局所性を改善
- **根拠**: LLVMの`-function-sections` + リンカの`--call-graph-profile-sort`。cl-ccでは関数間のジャンプ距離が非最適
- **難易度**: Medium

#### FR-187: Prefetch Instruction Insertion

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: prefetch命令の生成なし。ヒープポインタ間接参照やリスト走査でキャッシュミスが発生
- **内容**: リスト走査ループでの`PREFETCHT0 [next_cons]`、配列アクセスでの`PREFETCHNTA [arr+stride]`を挿入。AArch64では`PRFM PLDL1KEEP`
- **根拠**: GCCの`-fprefetch-loop-arrays`。`dolist`/`dotimes`等のループで効果大
- **難易度**: Medium

#### ✅ FR-188: NOP Padding / Alignment Directives

- **対象**: `packages/emit/src/x86-64.lisp`, `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: アセンブリテキスト出力に`.align`ディレクティブなし（`x86-64.lisp:73-91`）。ループヘッダ・関数エントリにNOP paddingなし
- **内容**: ループバックエッジのターゲットを16バイト境界にアラインし、関数エントリを16バイト境界にパディング。分岐予測ヒットレート向上
- **根拠**: Intel最適化マニュアル: ループヘッダアラインで3-5%性能改善
- **難易度**: Easy

#### FR-189: Cache-Line Aware Object Layout

- **対象**: `packages/runtime/src/heap.lisp`, `packages/compile/src/codegen-clos.lisp`
- **現状**: オブジェクトフィールドはソース順配置。ホット/コールドフィールドの分離なし。カードテーブルは512B（`heap.lisp:39`）だがオブジェクトサイズとの整合性なし
- **内容**: CLOS instanceのホットフィールド（頻繁アクセス）を先頭64バイト（1 cache line）に集約。コールドフィールドは後方に配置。GCメタデータとユーザーフィールドのfalse sharing回避
- **根拠**: HotSpot VMのfield packing。L1キャッシュライン=64Bへの最適配置
- **難易度**: Hard

---

### Phase 37 — レジスタ割り当て・スケジューリング高度化

#### FR-199: Spill Slot Sharing / Stack Coloring

- **対象**: `packages/emit/src/regalloc.lisp`
- **現状**: 各vregが一意のスピルスロット（`regalloc.lisp:351-375`、`[RBP - slot*8]`）。ライブ範囲が重複しないvregのスロット共有なし
- **内容**: スピルされたvregのライブ範囲を干渉グラフに投影し、非干渉のvregに同一スタックスロットを割り当て。スタックフレームサイズをO(N)→O(χ(G))に削減
- **根拠**: LLVM StackSlotColoring。現状N個のスピルvregがN×8バイトのスタックを消費
- **難易度**: Medium

#### FR-200: Software Pipelining

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **現状**: ループ反復間の命令重複なし。各反復が完全にシーケンシャル
- **内容**: ループボディの命令をモジュロスケジューリングで反復間パイプライン化。反復Nの後半と反復N+1の前半をオーバーラップ実行
- **根拠**: GCCの`-fmodulo-sched`。タイトな数値ループで2-4倍のスループット改善
- **難易度**: Very Hard

#### FR-201: Trace Scheduling / Superblock Formation

- **対象**: `packages/optimize/src/cfg.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **現状**: 基本ブロック単位の命令生成のみ。ブロック境界を超えたスケジューリングなし
- **内容**: 最頻実行パスに沿ってトレース（基本ブロック列）を形成し、トレース内で命令スケジューリング。ブロック境界の制約を除去してILP（命令レベル並列性）を最大化
- **根拠**: Fisherのtrace scheduling。FR-068のpost-RAスケジューリングを基本ブロック境界越えに拡張
- **難易度**: Very Hard

---

### Phase 47 — SIMD・ベクトル化

#### FR-226: Auto-Vectorization / Loop Vectorization (ループ自動ベクトル化)

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/optimize/src/cfg.lisp`
- **現状**: 全演算がスカラー。ループ解析（FR-021 SCEV）は定義済みだがベクトル化解析パスなし
- **内容**: ループ内の独立したスカラー演算を検出し、SIMD幅（4xf32, 2xf64等）のベクトル演算に変換。ループストリップマイニング（ベクトル幅でループ分割）＋残余ループ生成。依存関係解析でベクトル化可能性を判定
- **根拠**: LLVM LoopVectorize / GCC tree-vect-loop。配列処理ワークロードで4-8x高速化
- **難易度**: Very Hard

#### FR-227: SLP Vectorizer (Superword Level Parallelism)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: 直線コード内の並列実行可能なスカラー命令を検出する仕組みなし
- **内容**: 連続メモリアクセスパターン（`(aref arr i)`, `(aref arr (+ i 1))`等）を検出し、パック可能な独立演算をSIMD命令にまとめる。ループ外の直線コードにも適用可能
- **根拠**: LLVM SLPVectorizer / GCC tree-vect-slp。ループベクトル化と相補的
- **難易度**: Hard

#### FR-228: x86-64 SSE/AVX命令エミッション

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/regalloc.lisp`
- **現状**: スカラー整数命令のみ（MOV, ADD, SUB, IMUL, CMP等）。XMM/YMMレジスタ管理なし
- **内容**: SSE2（PADDD, MOVDQA等）/ SSE4（PMULLD等）/ AVX2（VPADDD, VPERM等）命令エンコーディング。XMM0-XMM15レジスタの割り当て管理。AVX VEXプレフィックスエンコーディング
- **根拠**: x86-64 ISA。FR-226/FR-227のベクトル化結果をネイティブ命令に変換するために必須
- **難易度**: Hard

#### FR-229: AArch64 NEON命令エミッション

- **対象**: `packages/emit/src/aarch64-codegen.lisp`, `packages/emit/src/regalloc.lisp`
- **現状**: スカラー整数命令のみ（MOVZ, ADD, SUB, MUL等）。V0-V31 NEONレジスタ管理なし
- **内容**: NEON SIMD命令（VADD, VMUL, VLD1, VST1, VDUP等）エンコーディング。V0-V31レジスタ管理。Advanced SIMD（128-bit）サポート
- **根拠**: ARMv8-A ISA。Apple Silicon最適化に重要
- **難易度**: Hard

#### FR-230: SIMD Register Allocation (SIMDレジスタ割り当て)

- **対象**: `packages/emit/src/regalloc.lisp`
- **現状**: 整数/ポインタレジスタのみの線形スキャン。XMM/YMM（x86-64）およびV0-V31（AArch64）のレジスタ圧力解析なし
- **内容**: 浮動小数点/SIMDレジスタクラスを追加し、整数レジスタとは独立にスピル/再ロードを管理。レジスタクラス間の移動命令（MOVD/MOVQ等）生成
- **根拠**: LLVM RegisterClass / GCC reg-alloc。FR-228/FR-229の前提条件
- **難易度**: Hard

---

### Phase 59 — コード生成・リンカ高度化

#### FR-267: Branch Prediction Hints (分岐予測ヒント)

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **現状**: 全分岐が等確率として扱われる。エラーパス・例外パスも通常パスと同じコードレイアウト
- **内容**: 静的ヒューリスティック: `signal`/`error`呼び出しを含むブロック→cold（unlikely）。ループバックエッジ→taken。`(declare (optimize ...))`と連携した分岐重みアノテーション。FR-036（Hot/Cold Layout）のメタデータソース
- **根拠**: GCC \_\_builtin_expect / LLVM branch_weights metadata。分岐予測ミス削減
- **難易度**: Easy

#### FR-268: AArch64 Constant Islands / Literal Pools (ARM定数プール)

- **対象**: `packages/emit/src/aarch64-codegen.lisp`
- **現状**: AArch64バックエンドは最小限（8命令型のみ）。大きな即値のロード戦略なし。MOVZ/MOVKの2-4命令シーケンスでフルアドレスをロード
- **内容**: ADR/ADRP + LDR でPC相対定数プールからロード。定数プール（literal pool）を関数末尾に配置。4KBを超える関数では中間にconstant islandを挿入
- **根拠**: ARM Architecture Reference Manual。AArch64の即値制約（12-bit/16-bit）を回避する標準手法
- **難易度**: Medium

#### FR-269: Linker Relaxation (リンカリラクゼーション)

- **対象**: `packages/binary/src/elf.lisp`, `packages/binary/src/macho.lisp`
- **現状**: ELF出力は基本的なリロケーション（`R_X86_64_PLT32`等）のみ。リンク時の命令緩和なし
- **内容**: リンク時に間接呼び出し（`call [GOT]`）を直接呼び出し（`call rel32`）に変換（ターゲットが同一リンク単位にある場合）。RISC-V向けにはlong branch→short branchの緩和。リロケーションエントリにリラクゼーションメタデータ追加
- **根拠**: LLD / GNU ld linker relaxation / RISC-V linker relaxation specification。呼び出しオーバーヘッドの自動削減
- **難易度**: Hard

#### FR-270: FFI Call Marshaling Specialization (FFIマーシャリング特殊化)

- **対象**: `packages/compile/src/codegen.lisp`, `packages/emit/src/calling-convention.lisp`
- **現状**: FR-194でFFI基盤を定義しているが、マーシャリングコード生成の最適化なし
- **内容**: FFI呼び出しの型シグネチャに基づいて専用マーシャリングスタブをコンパイル時生成。POD型（int, double, pointer）はインラインでレジスタ直接渡し（ゼロコピー）。構造体型は型レイアウトに基づくメモリコピー最適化
- **根拠**: CFFI/PyPy JIT FFI / Java JNI critical natives。FFIオーバーヘッド5-10x削減
- **難易度**: Hard

---

### Phase 64 — ネイティブバックエンド補完

#### FR-291: ELF Executable Generation (ELF実行可能形式生成)

- **対象**: `packages/binary/src/elf.lisp`
- **現状**: `elf.lisp:3` — `ET_REL`（`.o`再配置可能ファイル）のみ生成。`+elf-type-rel+`定数（`elf.lisp:23`）のみ定義。実行可能形式なし
- **内容**: `ET_EXEC`（静的リンク実行可能形式）生成: プログラムヘッダ（`PT_LOAD`セグメント）、エントリポイント設定、`.text`+`.data`+`.bss`セグメント配置。`ET_DYN`（PIE実行可能/共有ライブラリ）: `.dynamic`セクション、`.dynsym`/`.dynstr`
- **根拠**: System V ABI / ELF Specification。`cc`→`.o`→外部リンカの依存を除去し、単体でネイティブバイナリ生成可能に
- **難易度**: Very Hard

#### FR-292: PE/COFF Windows Binary Support (PE/COFF Windowsバイナリ)

- **対象**: 新規`packages/binary/src/pe.lisp`
- **現状**: 完全未実装。Mach-O（macOS）とELF（Linux）のみ
- **内容**: PE/COFF（Portable Executable）形式の`.exe`/`.dll`生成。DOSスタブ、PEヘッダ、セクションテーブル（`.text`/`.rdata`/`.data`/`.reloc`）。インポートテーブル（IAT）・エクスポートテーブル生成。x86-64 Windows ABI（rcx/rdx/r8/r9レジスタ渡し、シャドウスペース）対応
- **根拠**: Microsoft PE/COFF Specification。クロスプラットフォーム対応に必須
- **難易度**: Very Hard

#### FR-293: Mach-O Relocation & Code Signature (Mach-O再配置・コード署名)

- **対象**: `packages/binary/src/macho.lisp`
- **現状**: ヘッダ・セグメント・セクション書き出し済み（`macho.lisp`）。`add-symbol`定義済み（`macho.lisp:376-396`）だがシリアライズ未実装。PIEフラグ設定済み（`macho.lisp:428`）だがPICコード未生成。再配置エントリなし。`LC_DYLD_INFO`/`LC_LOAD_DYLIB`（dyld動的リンク）なし。`LC_CODE_SIGNATURE`（コード署名）なし
- **内容**: 再配置エントリ（`GENERIC_RELOC_*`/`X86_64_RELOC_*`）生成。`LC_DYLD_INFO_ONLY`（バインド/リバインド情報）。`LC_LOAD_DYLIB`（libSystem.B.dylib等）。`LC_CODE_SIGNATURE`（Apple ad-hoc署名 or codesign連携）
- **根拠**: Apple Mach-O Reference / dyld source。macOS Catalina以降はコード署名必須
- **難易度**: Hard

#### FR-294: x86-64 Missing Instruction Encoding (x86-64命令エンコーディング補完)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/x86-64.lisp`
- **現状**: IDIV命令未エミッション。関数プロローグ/エピローグ（push rbp/mov rbp,rsp/pop rbp/ret）なし。RIP相対アドレッシング未使用（`x86-64-codegen.lisp:100-111`でMOVベースのみ）
- **内容**: (1) IDIV/DIV命令エンコーディング（CDQ/CQO+IDIV）。(2) 標準プロローグ/エピローグ生成（push rbp, sub rsp, N, ..., leave, ret）。(3) RIP相対データ参照（`[rip+disp32]`）。(4) SIB（Scale-Index-Base）アドレッシング
- **根拠**: Intel 64 ISA Reference。実用的なネイティブコード生成に必須
- **難易度**: Hard

#### FR-295: AArch64 Missing Instruction Encoding (AArch64命令エンコーディング補完)

- **対象**: `packages/emit/src/aarch64-codegen.lisp`, `packages/emit/src/aarch64.lisp`
- **現状**: 除算命令（SDIV/UDIV）未実装。浮動小数点命令なし。複雑アドレッシングモード（pre/post-index、register offset）なし。スピルコード生成なし（`aarch64.lisp:12`でエラー）
- **内容**: (1) SDIV/UDIV整数除算。(2) FMOV/FADD/FSUB/FMUL/FDIV浮動小数点。(3) LDR/STR pre-index `[Xn, #imm]!`・post-index `[Xn], #imm`・register offset `[Xn, Xm]`。(4) STP/LDPペアロード/ストア
- **根拠**: ARM Architecture Reference Manual。AArch64バックエンドの実用化に必須
- **難易度**: Hard

#### FR-296: RISC-V Backend Implementation (RISC-Vバックエンド実装)

- **対象**: 新規`packages/emit/src/riscv64-codegen.lisp`, `packages/mir/src/target.lisp`
- **現状**: `target.lisp:150-165`にRISC-V 64ターゲット記述子（`*riscv64-target*`）が存在するが、命令エミッション・レジスタ割り当て・コード生成が完全未実装
- **内容**: RV64IMAFDC（整数+乗除算+アトミック+浮動小数点+圧縮）命令セットのエミッション。x0-x31レジスタ・f0-f31 FPレジスタの割り当て。RISC-V ABIに準拠した呼び出し規約（a0-a7引数、ra戻りアドレス）。リラクゼーション対応リロケーション
- **根拠**: RISC-V ISA Specification / RISC-V ELF psABI。オープンISAへの対応
- **難易度**: Very Hard

#### FR-297: WASM Backend Completion (WASMバックエンド完成)

- **対象**: `packages/emit/src/wasm.lisp`
- **現状**: グローバル変数アクセス（`wasm.lisp:309-314`）・関数呼び出し（`wasm.lisp:323-329`）・クロージャ生成（`wasm.lisp:331-337`）が全てスタブ（コメント出力のみ）。`ref.null`を返すだけの実装
- **内容**: (1) `global.get`/`global.set`によるグローバル変数アクセス。(2) `call`/`call_indirect`による関数呼び出し。(3) `struct.new`/`array.new`（GC proposal）によるクロージャ生成。(4) `try`/`catch`/`throw`（exception handling proposal、定数`+wasm-try/catch/throw+`は`wasm-types.lisp:89-91`に定義済み）
- **根拠**: WebAssembly Specification / GC Proposal。ブラウザ・Edge環境での実行に必須
- **難易度**: Very Hard

#### ✅ FR-298: vm-print Backend Emission (vm-print バックエンドエミッション)

- **対象**: `packages/emit/src/x86-64.lisp`, `packages/emit/src/aarch64.lisp`, `packages/emit/src/wasm.lisp`
- **現状**: `x86-64.lisp:84-86`・`aarch64.lisp:54-56`の両方で`(error "print backend emission is not implemented yet")`。ネイティブコンパイルされたプログラムが`print`/`format`を使用すると即クラッシュ
- **内容**: ランタイムの`write`/`write-string`関数への呼び出し生成。フォーマット文字列のコンパイル時解析（`format`ディレクティブの静的展開）。バッファリングI/Oへの最適化
- **根拠**: 全ネイティブバックエンドの基本I/O機能。`print`なしでは実用プログラムが動作しない
- **難易度**: Medium

---

### Phase 65 — MIR・命令選択

#### FR-299: MIR Instruction Selection Rules (MIR命令選択ルール)

- **対象**: 新規`packages/emit/src/isel/x86-64-rules.lisp`, `packages/emit/src/isel/aarch64-rules.lisp`, `packages/mir/src/mir.lisp`
- **現状**: `mir.lisp`にSSAベースのMIRフレームワークが完成。`target.lisp:8-10`でターゲット別命令選択ルールファイルを参照するが、対応ファイルが全ターゲットで存在しない。MIR→ネイティブ命令のマッピングが欠落=バックエンドパイプラインが接続されていない
- **内容**: ターゲット別パターンマッチング: MIR命令→ネイティブ命令の変換ルール定義。Maximal Munch / BURS（Bottom-Up Rewrite System）アルゴリズム。FR-175（命令選択フレームワーク）の具体的ルール実装
- **根拠**: LLVM SelectionDAG / GCC RTL patterns。FR-175で定義されたフレームワークの実体化
- **難易度**: Very Hard

---

### Phase 78 — コード生成・レジスタ割り当て改善

#### FR-403: Branch Displacement Optimization (分岐変位最適化)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **現状**: 全ジャンプが固定`rel32`エンコーディング（JMP=5バイト, Jcc=6バイト）。`emit-jmp-rel32`(`x86-64-codegen.lisp:177`)、`emit-je-rel32`(`x86-64-codegen.lisp:181`)。短条件ジャンプ(`JE rel8`=2バイト)はハンドクラフトシーケンスのみ
- **内容**: 2パスまたは反復緩和アルゴリズム。初回パスで全分岐を`rel8`(2バイト)。オーバーフロー時`rel32`(5-6バイト)に拡大・オフセット再解決。`*x86-64-instruction-sizes*`を動的に
- **根拠**: LLVM/GCC標準 — branch relaxation
- **難易度**: Medium

#### FR-404: Lazy Compilation / Compile-on-First-Call (遅延コンパイル)

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/vm/src/vm.lisp`
- **現状**: `compile-toplevel-forms`が全フォームを一括コンパイル。`our-load`(`pipeline.lisp:373`)がロード時に全フォームを順次コンパイル。スタブ/トランポリン機構なし
- **内容**: `defun`コンパイルをソース記録の軽量スタブに置換。初回呼び出し時にフルコンパイルしスタブをパッチ。VM用スタブ命令型、自己書き換え呼び出しサイトパッチ
- **根拠**: HotSpot/V8の標準手法 — lazy compilation
- **難易度**: Hard

#### FR-405: Startup Time Optimization / Image Dump (起動時間最適化)

- **対象**: `packages/cli/src/main.lisp`, `packages/pipeline/pipeline.lisp`
- **現状**: `*stdlib-compiled*`(`pipeline.lisp:191`)がnil（キャッシュ未使用）。`get-stdlib-forms`(`pipeline.lisp:193`)が毎回再パース。REPL stdlib読み込み(`main.lisp:263`)が最初のプロンプト前に全stdlibを同期コンパイル
- **内容**: (a) プリコンパイルstdlibキャッシュ（FASL様シリアライズ）。(b) 遅延stdlibロード（初回参照時コンパイル）。(c) `--no-stdlib`デフォルト+未定義関数auto-require。(d) イメージダンプ(`save-lisp-and-die`相当)で全初期化スキップ
- **根拠**: SBCL save-lisp-and-die, ECL image dump
- **難易度**: Medium

#### FR-406: Code Compression for Distribution (配布用コード圧縮)

- **対象**: `packages/binary/src/macho.lisp`, `packages/binary/src/elf.lisp`
- **現状**: `compile-to-native`(`pipeline.lisp:426`)が生のバイト列を直接`__TEXT`セグメントに書き込み。セグメント/セクションの圧縮なし
- **内容**: (a) zstd/lz4で`__DATA`セグメント圧縮（ロード時解凍）。(b) バイトコード形式にvarint+辞書圧縮。(c) デバッグ情報セクション圧縮
- **根拠**: LLVM LLD, GNU gold — compressed debug sections
- **難易度**: Medium

#### ✅ FR-407: Spill Register Clobber Fix (スピルレジスタ上書き修正)

- **対象**: `packages/emit/src/regalloc.lisp`
- **現状**: `regalloc.lisp:358-364` — 全スピル済みvregが単一scratchレジスタを共有。同一命令内の2スピルオペランドが黙ってclobber
- **内容**: 複数scratch割り当てまたはスピル競合検出。同一命令内でsrc/dstの両方がスピルの場合、2つの異なるscratchレジスタを使用
- **根拠**: 正当性バグ — サイレントデータ破壊
- **難易度**: Medium

#### ✅ FR-408: Copy Propagation Performance (コピー伝播性能改善)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: `optimizer.lisp:197-199` — `kill`が毎回`maphash`でコピーテーブル全走査。O(n×m) (n命令, mコピー数)
- **内容**: コピーテーブルを逆引きマップ付きに再構築。レジスタ書き込み時にそのレジスタを含むコピーのみ無効化。O(n)に改善
- **根拠**: 標準コンパイラ実装 — efficient copy propagation kill
- **難易度**: Low-Medium

#### ✅ FR-409: Label-Aware Optimization State (ラベル考慮最適化状態)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: `optimizer.lisp:86,203,601,677-678` — 全`vm-label`で`clrhash`（env/copies/gen/val-env/memo全消去）。フォールスルーラベルでも最適化コンテキスト破棄
- **内容**: フォールスルーラベル（分岐ターゲットでない）ではclrhash省略。分岐合流点のみ保守的消去。CFGのpredecessor情報を利用した精密な状態合流
- **根拠**: GCC/LLVM — label-aware dataflow analysis
- **難易度**: Medium

#### FR-410: Instruction Representation Efficiency (命令表現効率化)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: `opt-rewrite-inst-regs`(`optimizer.lisp:229-246`)が非自明命令を毎回sexpシリアライズ→ツリー走査→再構築。命令数×パス反復回数分のアロケーション
- **内容**: 命令オブジェクトのスロット直接書き換え。sexp往復を除去し、`sexp-slots`メタデータで直接レジスタフィールド操作
- **根拠**: GC圧力削減、最適化パス高速化
- **難易度**: Medium

#### ✅ FR-411: Recursive Inlining Guard (再帰インライニングガード)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: `opt-pass-inline`(`optimizer.lisp:504-576`)が再帰・相互再帰呼び出しを検出せず。ボディ閾値を満たせば自分自身にインライン可能
- **内容**: インライン候補判定時にコールグラフを参照。再帰・相互再帰の場合インライン抑制（または深度制限）
- **根拠**: 無限展開防止 — 正当性バグ
- **難易度**: Low-Medium

#### ✅ FR-412: Liveness Analysis Completeness (活性解析完全化)

- **対象**: `packages/emit/src/regalloc.lisp`
- **現状**: `compute-live-intervals`(`regalloc.lisp:244-273`)が後方ジャンプのみ区間延長。前方`vm-jump-zero`のラベルへのジャンプでギャップ越しのレジスタ活性が未延長
- **内容**: 前方分岐もlive interval延長対象に含める。反復データフロー解析またはSSA情報利用
- **根拠**: 正当性バグ — 前方分岐でレジスタ上書きの可能性
- **難易度**: Medium

#### FR-413: Live Range Splitting (ライブレンジ分割)

- **対象**: `packages/emit/src/regalloc.lisp`
- **現状**: `regalloc.lisp:219-283` — 各vregが単一連続区間。中間で使用されない期間も物理レジスタを占有
- **内容**: ライブレンジを使用点間のホール（穴）で分割。ホール期間は物理レジスタを解放。スピル圧力大幅削減
- **根拠**: Poletto & Sarkar (1999) Linear Scan Register Allocation
- **難易度**: Medium

#### ✅ FR-414: Iterative Inlining (反復インライニング)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: `opt-pass-inline`(`optimizer.lisp:765`)が収束ループ外で1回のみ実行。インライン後のfold/DCEが新たなインライン機会を露出しても再発見不可
- **内容**: `opt-pass-inline`を収束パス（`*opt-convergence-passes*`）に統合。インライン→fold→DCE→再インラインの反復サイクル。深度制限付き
- **根拠**: LLVM CGSCC inline — iterative inlining
- **難易度**: Low-Medium

#### ✅ FR-415: CSE Key Efficiency (CSEキー効率化)

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: `val<`比較(`optimizer.lisp:658`)が`(format nil "~S" ...)`で毎CSEキー比較にフレッシュ文字列生成。高GC圧力
- **内容**: CSEキーを構造的比較（opcode + operands tuple）またはハッシュコンスに変更。文字列アロケーション除去
- **根拠**: GC圧力削減
- **難易度**: Low

#### ✅ FR-416: Tail-Call Register Allocation (末尾呼び出しレジスタ割り当て)

- **対象**: `packages/emit/src/regalloc.lisp`
- **現状**: `vm-tail-call`が`regalloc.lisp:85`でdstレジスタ定義。末尾呼び出しは復帰しないためdstは使用されず、物理レジスタを無駄に割り当て
- **内容**: `vm-tail-call`のdstをregalloc対象から除外。または`vm-tail-call`のdst定義自体を削除
- **根拠**: レジスタ圧力削減
- **難易度**: Easy

---

### Phase 83 — バイナリ・リンカー完全化

#### ✅ FR-464: Mach-O **PAGEZERO Segment (Mach-O **PAGEZEROセグメント)

- **対象**: `packages/binary/src/macho.lisp`
- **現状**: `build-mach-o`(`macho.lisp:406-464`)が`__PAGEZERO`ロードコマンド未出力。macOS実行可能形式の必須要件
- **内容**: `__PAGEZERO`セグメント（vmaddr=0, vmsize=4GB nullガード）のLC_SEGMENT_64追加
- **根拠**: macOS ABI — 全実行可能形式に必須
- **難易度**: Low

#### ✅ FR-465: Mach-O Symbol Table Serialization (Mach-Oシンボルテーブルシリアライズ)

- **対象**: `packages/binary/src/macho.lisp`
- **現状**: `add-symbol`/`serialize-nlist`(`macho.lisp:376-396,280-288`)が定義済みだが`build-mach-o`から未呼び出し。LC_SYMTABロードコマンドもシリアライズされない(`macho.lisp:419-422`)
- **内容**: `build-mach-o`でnlistエントリをシリアライズ。LC_SYMTAB/LC_DYSYMTABロードコマンド出力。デバッガ/dlsym互換
- **根拠**: macOS実行可能形式 — シンボルテーブルはデバッグ・リンクに必須
- **難易度**: Medium

#### ✅ FR-466: Mach-O Data Segment Serialization (Mach-Oデータセグメントシリアライズ)

- **対象**: `packages/binary/src/macho.lisp`
- **現状**: `add-data-segment`(`macho.lisp:351-374`)が定義済みだが`build-mach-o`がcode-bytesのみ書き込み。データセグメント内容が出力バッファに反映されない
- **内容**: `__DATA`セグメント内容をバイナリ出力に含める。グローバル変数・定数テーブルの配置
- **根拠**: グローバルデータの実行可能形式への配置
- **難易度**: Medium

#### ✅ FR-467: Mach-O W^X Security (Mach-O W^X セキュリティ)

- **対象**: `packages/binary/src/macho.lisp`
- **現状**: `add-data-segment`(`macho.lisp:369-370`)が`maxprot=7`(rwx)設定。W^Xセキュリティポリシー違反
- **内容**: `__DATA`セグメントの`maxprot`を`6`(rw-)に変更。実行可能ページと書き込み可能ページを分離
- **根拠**: macOSセキュリティ — W^X (Write XOR Execute)
- **難易度**: Easy

#### ✅ FR-468: ELF AArch64 Support (ELF AArch64対応)

- **対象**: `packages/binary/src/elf.lisp`
- **現状**: `elf.lisp:24,313`がx86-64(`+elf-machine-x86-64+` #x3e)ハードコード。AArch64 codegen存在にもかかわらず`EM_AARCH64`(#xB7)未対応
- **内容**: ELFヘッダのe_machineをターゲットに応じて設定。AArch64再配置型の追加
- **根拠**: AArch64ネイティブコンパイル完全化
- **難易度**: Medium

#### ✅ FR-469: ELF Section Alignment (ELFセクションアライメント)

- **対象**: `packages/binary/src/elf.lisp`
- **現状**: `.text`オフセットがELFヘッダ直後にパディングなしで開始(`elf.lisp:284`)。リンカが期待するページ/16バイトアライメント不足
- **内容**: `.text`セクションを16バイトまたはページ境界にアライン。`.data`/`.bss`も同様
- **根拠**: ELF仕様 — セクションアライメント要件
- **難易度**: Low

#### ✅ FR-470: ELF .bss Section (ELF .bssセクション)

- **対象**: `packages/binary/src/elf.lisp`
- **現状**: `.text`のみ。未初期化データ用`.bss`セクションAPIなし(`elf.lisp:150-161`)
- **内容**: `.bss`セクション追加。SHT_NOBITS型、ファイルサイズ0でメモリサイズ指定
- **根拠**: グローバル変数の効率的配置
- **難易度**: Low

#### ✅ FR-471: Byte Buffer Unification (バイトバッファ統一)

- **対象**: `packages/binary/src/macho.lisp`, `packages/binary/src/elf.lisp`, `packages/emit/src/wasm.lisp`
- **現状**: `byte-buffer`クラス(`macho.lisp:126-156`)、`elf-make-buffer`/`elf-buf-*`(`elf.lisp:68-111`)、`make-wasm-buffer`/`wasm-buf-*`(`wasm.lisp:44-86`)が3つの独立したほぼ同一のバイトバッファAPI
- **内容**: 共通`binary-buffer`クラスに統合。write-u8/u16/u32/u64/bytes/string/leb128メソッドを共通化
- **根拠**: コード重複削減
- **難易度**: Medium

#### ✅ FR-472: Calling Convention FP Registers (呼び出し規約浮動小数点レジスタ)

- **対象**: `packages/emit/src/calling-convention.lisp`
- **現状**: `calling-convention`構造体(`calling-convention.lisp:8-14`)がGPRプールのみ。XMM/NEONフロートレジスタフィールドなし
- **内容**: System V ABI準拠のfloat引数渡し。XMM0-7(x86-64)/V0-7(AArch64)レジスタプール追加
- **根拠**: System V ABI — 浮動小数点引数はFPレジスタで渡す
- **依存**: FR-008 (float unboxing)
- **難易度**: Medium

#### ✅ FR-473: WASM Portability (WASMポータビリティ)

- **対象**: `packages/emit/src/wasm.lisp`
- **現状**: `wasm-buf-write-f64`(`wasm.lisp:74-75`)がSBCL専用コード（`sb-kernel:double-float-bits`）。他CL実装でエラー
- **内容**: ポータブルなIEEE754 double→bytesシリアライズ。`#+(or sbcl ccl ecl)`分岐または純CLビット操作
- **根拠**: ポータビリティ — 自己ホスティングで必要
- **難易度**: Low

---
