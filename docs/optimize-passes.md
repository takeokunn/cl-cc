# Optimization: Core Passes, CPS/SSA & Speculative JIT

VM optimizer, loop optimization, control flow, range analysis, interprocedural optimization, higher-order function optimization, code size, CPS transformation, SSA construction, speculative JIT compilation.

**実装状況**: 進行中。完了済みの項目は各FR見出しの `✅` で明示する。

---

### Phase 1 — VM Optimizer Layer (Quick wins)（実装済み）

#### FR-001: ペアホール最適化ルール拡充 ✅

- **対象**: `packages/foundation/prolog/src/prolog.lisp` の `*peephole-rules*`
- **現状**: 4ルール → 目標30+ルール
- **エンジン変更不要**: ルール追加のみ

追加ルール例:

| パターン                         | 変換後            | 説明           |
| -------------------------------- | ----------------- | -------------- |
| `(vm-load R1 X)(vm-load R2 X)`   | `(vm-move R2 R1)` | 重複ロード除去 |
| `(vm-add R1 R2 0)`               | `(vm-move R1 R2)` | 0加算除去      |
| `(vm-mul R1 R2 1)`               | `(vm-move R1 R2)` | 1乗算除去      |
| `(vm-not R1 (vm-lt ...))`        | `(vm-ge ...)`     | 否定+比較融合  |
| `(vm-const R1 X)(vm-const R1 Y)` | `(vm-const R1 Y)` | 上書き定数除去 |
| `L1→L2→L3`                       | `L1→L3`           | Jump chain短縮 |

#### FR-002: 葉関数最適化 (Leaf Function Optimization) ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **検出基準**: 命令列中に `vm-call` / `vm-generic-call` が存在しない関数
- **最適化内容**:
  - セーブ/リストア不要なレジスタの省略
  - フレームプッシュの軽量化フラグ付与
- **実装**: 新パス `opt-pass-leaf-detect` → codegen側フラグ参照

#### FR-003: Loop Invariant Code Motion (LICM) ✅

- **対象**: `packages/engine/optimize/src/cfg.lisp` + `packages/engine/optimize/src/optimizer.lisp`
- **前提**: `loop-depth` フィールドが `cfg.lisp` に存在するが未使用
- **実装手順**:
  1. バックエッジ検出 (逆ポストオーダーでの祖先へのエッジ)
  2. 自然ループ識別 (ヘッダ + バックエッジからの逆可達性)
  3. ループ不変命令の判定: 全入力レジスタがループ外で定義 + `opt-inst-pure-p` が真
  4. ループ前置ブロック (preheader) への移動
- **活用**: 既存 `opt-inst-pure-p` 関数

#### FR-010: Sparse Conditional Constant Propagation (SCCP) ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: Constant Folding はある (`opt-pass-fold`) が、条件分岐を跨いだ伝播がない
- **内容**:
  - 到達可能な実行パスのみを解析対象とする (条件分岐の定数化による枝刈り)
  - `opt-pass-fold` の拡張または独立パスとして実装
  - 効果: `(if (= x 0) ... ...)` で x=0 が証明可能なら片方の枝を除去

#### FR-011: Global Value Numbering (GVN) ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: CSE (`opt-pass-cse`) はローカルな値番号付け
- **内容**:
  - CFGを跨いだグローバルな値番号付け
  - 支配木を利用した冗長計算除去
  - CSEより広いスコープで重複計算を除去

#### FR-012: Partial Redundancy Elimination (PRE) ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**: LICM + CSE の一般化
  - ループ不変コード移動と共通部分式除去を統一フレームワークで扱う
  - 「一部のパスでのみ冗長」な計算を最適化 (LICMで扱えないケースを補完)

---

### Phase 6 — ループ高度最適化（未実装）

#### FR-021: Scalar Evolution (SCEV) / 帰納変数解析

- **依存**: FR-003 (LICM)
- **対象**: `packages/engine/optimize/src/cfg.lisp` + `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - ループ内の帰納変数 (`i = i + 1` パターン) を認識・特性記述
  - 強度低減の拡張: ループカウンタを使った演算をシフト・加算に置き換え
  - LICM拡張: SCEVで不変と証明できる計算を巻き上げ
  - LLVM の ScalarEvolution パスに相当

#### FR-022: Loop Unrolling

- **依存**: FR-021 (SCEV でトリップカウント推定)
- **内容**:
  - ループボディを N 回複製してループオーバーヘッドを削減
  - 定数トリップカウントが判明している場合は完全展開
  - 不明な場合は剰余エピローグ付き部分展開
  - 効果: ILP露出、分岐オーバーヘッド削減

#### FR-023: Polymorphic Inline Cache (PIC) with Megamorphic Fallback

- **依存**: FR-009 (Monomorphic IC)
- **内容**:
  - FR-009 の Monomorphic IC (1エントリ) を拡張
  - Polymorphic IC: 2-4型を線形リストでキャッシュ
  - Megamorphic fallback: キャッシュ溢れ時は共有グローバルキャッシュに降格
  - V8・SpiderMonkeyの3段階IC階層と同等
  - 効果: 多相的呼び出し箇所でのディスパッチ高速化

---

### Phase 7 — 制御フロー最適化（一部実装: FR-036）

#### FR-032: Jump Threading

- **対象**: `packages/engine/optimize/src/optimizer.lisp` + `packages/engine/optimize/src/cfg.lisp`
- **内容**:
  - CFGエッジを跨いで既知の条件を伝播し、冗長な分岐を除去
  - `typecase`/`etypecase`/`cond` の型チェック連鎖に特に有効
  - 例: ある分岐で `x` が fixnum と判明していれば、後続ブロックの fixnum チェックを除去
  - LLVM `JumpThreadingPass` に相当
- **難易度**: Medium

- **関連実装**: `packages/engine/optimize/src/optimizer.lisp` の `opt-pass-jump` は jump-chain threading と直後 fall-through への不要 jump 除去を実装済み。条件値そのものを CFG edge 越しに伝播して後続 predicate を定数化する完全版 jump threading は未実装。

#### FR-033: Case-of-Case ✅

- **対象**: `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - ネストした `if` で、外側条件と内側条件が構造的に同一な場合に内側分岐を簡約する
  - 保守的な case-of-case スタイル最適化
  - `(if p (if p a b) c)` の内側 `if` を簡約する
- **難易度**: Medium

#### FR-034: If-Conversion (分岐→条件移動) ✅

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - `vm-select` を `CMOV` 命令(x86-64) / `CSEL`(AArch64) に変換
  - VM/バックエンド段での branchless selection を提供
  - 高レベル `if` 全般の自動 if-conversion は未実装
- **難易度**: Medium

#### FR-035: Hot/Cold Splitting ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp` + バックエンド
- **内容**:
  - エラー処理・型エラー・条件シグナル等の「コールドパス」をホット関数の末尾/別セクションへ移動
  - ホット関数のI-キャッシュ占有を削減
  - 静的ヒューリスティック: `signal`/`error` 呼び出しを含む基本ブロックはコールド
- **難易度**: Medium

#### FR-036: Hot/Cold Code Layout (基本ブロック並べ替え) ✅

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **内容**:
  - 頻繁に実行される基本ブロックを連続したアドレスに配置
  - fall-through条件（JZ/JNZ → fall-through is hot path）をヒューリスティックで決定
  - プロファイルなしでも分岐方向ヒューリスティックで効果あり
- **難易度**: Easy

#### FR-037: Call Site Splitting

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 特定の分岐アームでのみ引数型が確定している間接呼び出しを、その分岐内でインライン化/特化
  - `(funcall fn x)` で fn が型判明している分岐アームでは直接呼び出しへ変換
  - LLVM `CallSiteSplittingPass` に相当
- **難易度**: Medium

- **完了済みFR**: FR-036

---

### Phase 8 — 範囲解析・チェック除去（未実装）

#### FR-038: Value Range / Interval Analysis

- **対象**: `packages/engine/optimize/src/optimizer.lisp` + `packages/foundation/type/src/inference.lisp`
- **内容**:
  - 整数変数の値域 (`[lo, hi]`) をCFGを通じて伝播
  - SCCP (FR-010) が定数を伝播するのと同様に範囲を伝播
  - ループ変数 `i` が `0..n-1` の範囲であることを証明
  - HotSpot C2・LLVM `CorrelatedValuePropagationPass` と同等
- **難易度**: Medium

#### FR-039: Array Bounds Check Elimination (BCE)

- **依存**: FR-038 (範囲解析) or FR-021 (SCEV)
- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(aref v i)` のインデックス `i` が `0 <= i < (length v)` と証明可能な場合、境界検査命令を除去
  - ループ内の `aref` が最も恩恵を受ける
- **難易度**: Medium

#### FR-040: Nil Check Elimination ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - nil チェック後の支配されたブロックで同じ値への再チェックを除去
  - `(if x ...)` の truthy ブランチでは `x` が non-nil と確定
  - Lisp は nil を多用するため効果が大きい
- **難易度**: Easy

#### FR-041: Tag/Type Check Elimination (支配木ベース) ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 型チェック命令 (`vm-integer-p`, `vm-cons-p` 等) が、同じ値に対して支配ブロックで既に実行済みの場合は除去
  - DCE/GVNでは除去できない「条件分岐後の冗長チェック」を対象
  - 現状: `vm-integer-p` が同一値に対して複数回生成されるケースあり (`vm/primitives.lisp:47-52`)
- **難易度**: Medium

---

### Phase 10 — 手続き間最適化（未実装）

#### FR-050: Interprocedural SCCP (IPSCCP)

- **対象**: `packages/engine/optimize/src/optimizer.lisp` + コール グラフ解析
- **内容**:
  - 関数境界を跨いだSparse Conditional Constant Propagation
  - 呼び出し元で引数が定数の場合、呼び出し先で定数として伝播
  - FR-010 (SCCP) の手続き間版
  - LLVM `IPSCCPPass` に相当
- **難易度**: Hard

#### FR-051: Called Value Propagation / Devirtualization

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - 間接呼び出し (`funcall`/`apply`) の呼び出し先を静的に絞り込む
  - 単一の呼び出し先と証明された場合は直接呼び出しへ変換
  - LLVM `CalledValuePropagationPass` に相当
  - Lispの `funcall` は全てこの最適化の対象
- **難易度**: Hard

- **関連実装**: `packages/engine/optimize/src/optimizer-inline.lisp` に `opt-known-callee-labels` / `opt-known-callee-label` を追加済み。現状は `vm-closure` / `vm-func-ref` / `vm-const` / `vm-move` を追跡して関数 designator レジスタの既知 label を解決する helper 層のみで、実際の devirtualization rewrite は未実装。

#### FR-052: Global DCE (Dead Function/Method Elimination) ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp` + コンパイルパイプライン
- **内容**:
  - 到達不能な関数・メソッドをリンク時に除去
  - `defun` で定義されたが一切呼ばれない関数を除去
  - コード生成済み命令列から未参照エントリを削除
  - LLVM `GlobalDCEPass` に相当
- **難易度**: Easy

#### FR-053: Partial Inlining (ホットパスのみインライン)

- **依存**: FR-013 (型特化インライン展開)
- **対象**: `packages/engine/optimize/src/optimizer.lisp:729-820`
- **内容**:
  - 現状のインライン化は関数全体を展開 (≤15命令)
  - 早期リターンパスだけをインライン展開し、残りは元の呼び出しへフォールスルー
  - fixnum fast path のみインライン → bignum path は呼び出し継続
  - LLVM `PartialInlinerPass` に相当
- **難易度**: Hard

---

### Phase 11 — 高階関数・ストリーム最適化（未実装）

#### FR-054: Stream Fusion / Deforestation

- **対象**: `packages/frontend/expand/src/macro.lisp` or `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(mapcar f (remove-if g xs))` → 中間リストを生成しない単一走査に変換
  - コンパイラ自身が `mapcar`/`remove-if`/`reduce` チェーンを多用しているため効果大
  - GHCのbuild/foldr fusion rules と同等のアプローチ
  - 実装: パターンマッチルールとして `*peephole-rules*` または独立パス
- **難易度**: Hard (一般) / Medium (パターン限定)

#### FR-055: Loop Idiom Recognition

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(fill vec 0)` → memset相当命令
  - `(copy-seq vec)` → memcpy相当命令
  - ループパターンを認識してランタイムのバルク操作に置き換え
  - LLVM `LoopIdiomRecognizePass` に相当
- **難易度**: Medium

#### FR-056: Worker/Wrapper Transformation (部分適用)

- **対象**: `packages/engine/compile/src/codegen.lisp` + `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 再帰関数を「ラッパー (boxing/unboxing担当)」と「ワーカー (unboxed値で再帰)」に分割
  - 再帰呼び出し間でfloat/fixnumをunboxedのまま引き回す
  - FR-008 (Float Unboxing) の再帰関数への拡張
  - GHCの主要最適化の一つ
- **難易度**: Hard

---

### Phase 14 — コードサイズ・コード品質最適化（一部実装: FR-077）

#### FR-074: Function Outlining

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**: 複数関数間の繰り返し命令列を共有サブルーチンに抽出 (インライン化の逆)
- **根拠**: CLOSディスパッチ列は構造的に類似したものが多い
- **難易度**: Medium

#### FR-075: Tail Merging ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**: 複数基本ブロック末尾の同一命令列を単一共有ブロックに統合
- **難易度**: Medium

#### FR-076: Identical Code Folding (ICF)

- **対象**: `packages/backend/binary/src/macho.lisp`
- **内容**: バイト列が同一の関数を単一コピーに重複排除
- **難易度**: Hard

#### FR-077: Dead Basic Block Elimination ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp` + `packages/engine/optimize/src/cfg.lisp`
- **内容**: jump threading後も残存した到達不能ブロックの除去
- **難易度**: Easy

#### FR-078: Block Merging ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**: 単一後継ブロックを持つブロックの統合でラベルオーバーヘッド削減
- **難易度**: Easy

#### FR-079: Closure Thunk Sharing

- **対象**: `packages/engine/compile/src/codegen.lisp`
- **内容**: コードが同一で環境のみ異なるクロージャを単一コード+環境ポインタに統合
- **難易度**: Medium

- **関連実装**: `packages/engine/compile/src/closure.lisp` に `closure-sharing-key` / `group-shareable-closures` を追加済み。現状は entry-label と capture 集合が同一な sibling closure 候補をグループ化する分析 helper 層のみで、実際の shared thunk / shared environment codegen は未実装。

#### FR-080: Car/Cdr/Cons Inlining

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/list.lisp`
- **内容**: `car`/`cdr`/`cons` をVM命令ディスパッチなしの直接レジスタ操作にインライン化
- **根拠**: 現状は `define-simple-instruction` で毎回 `execute-instruction` ディスパッチが発生
- **難易度**: Medium

#### FR-081: Macro Expansion Memoization ✅

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/frontend/expand/src/macro.lisp`
- **内容**: `(form . env) → 展開結果` のweakメモテーブルを追加、同一フォームの再展開を回避
- **根拠**: 両ファイルに "cache" / "memoiz" が存在しない
- **難易度**: Medium

#### FR-082: Slab Allocator for Cons Cells

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`
- **内容**: 固定サイズ(3ワード)のコンスセル専用フリーリストプール
- **根拠**: コンスはLispランタイムで最頻繁な割り当てオブジェクト
- **難易度**: Medium

#### FR-083: Inline Bump Pointer Allocation

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/runtime/src/heap.lisp`
- **内容**: `rt-gc-alloc` 関数呼び出しを除去し、`young-free` の増分比較をコールサイトにインライン展開
- **根拠**: 現状は割り当てのたびに関数呼び出しが発生
- **難易度**: Medium

- **完了済みFR**: FR-077

---

### Phase 16 — 制御フロー・比較最適化（未実装）

#### FR-092: EFLAGS Reuse After CMP (x86-64)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**: CMP命令後のEFLAGSを直接分岐に使用し、bool値のレジスタ化+タグチェックを省略
- **難易度**: Medium

#### FR-093: Jump Table for Dense Integer Case ✅

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**: 密なinteger `case`/`ecase` を `JMP [rax*8+table]` の間接ジャンプに変換
- **難易度**: Medium

#### FR-094: Wasm `br_table` for User `case`

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp` (インフラ既存)
- **内容**: ユーザレベルの `case`/`etypecase` に `br_table` を適用
- **根拠**: trampolineで既に `br_table` を使用、ユーザ向けへの拡張が自然
- **難易度**: Low

- **関連実装**: `packages/backend/emit/src/wasm-trampoline.lisp` の PC-dispatch trampoline 自体は `br_table` を用いて全 basic block の再ディスパッチを行う。user-level `case` / `etypecase` を専用に `br_table` へ lower する変換は未実装。

#### FR-095: Binary Search for Sparse Case ✅

- **対象**: `packages/frontend/expand/src/expander.lisp`
- **内容**: 疎なinteger集合の `case` を線形チェーンでなく二分探索木で生成
- **難易度**: Low

#### FR-096: Power-of-2 Divisor Strength Reduction ✅

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **内容**: コンパイル時定数が2の冪の `floor`/`mod` を算術シフト/ビットANDに変換
- **難易度**: Low

---

### Phase 25 — Fixnum / 整数最適化（未実装）

#### FR-148: Fixnum演算整数範囲追跡 (VM全体)

- **対象**: `packages/foundation/type/src/inference.lisp`, `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `type-int` は存在するが範囲情報なし。`infer-type` が `ast-int → type-int` を返すのみで区間情報ゼロ
- **内容**: 型表現に `(integer lo hi)` 区間を追加。定数折り畳みで演算結果の区間を伝播し、オーバーフローしない演算を確認してタグチェックを省略
- **根拠**: `packages/foundation/type/src/inference.lisp:74-100` — 整数リテラルに区間なし。SBCL の `sb-c:interval` に相当する機能が欠如
- **難易度**: Hard

#### FR-149: Fixnum→Bignum Overflow Trap分岐

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `vm-div`（`primitives.lisp:105`）はゼロ除算のみチェック。加減乗算はオーバーフロー無検出でホストCL任せ
- **内容**: `vm-add`/`vm-sub`/`vm-mul` に fixnum範囲（51-bit）を超えた場合の bignum fallback ブランチを追加。タグ済み整数演算のオーバーフロー検出を明示的に行う
- **根拠**: `packages/backend/runtime/src/value.lisp:82-86` — fixnumは51-bit signed。演算結果が範囲外の場合の処理が未定義
- **難易度**: Medium

#### FR-150: Adaptive Optimization Thresholds

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: インライン閾値=15固定（`optimizer.lisp:1018`）、最大反復=20固定（`optimizer.lisp:1015`）
- **内容**: 実行時プロファイルカウンタ（呼び出し頻度・ループ深度）に基づいてインライン閾値とパス反復回数を動的調整。ホット関数には閾値を緩和（最大50命令）、コールドパスには反復を削減
- **根拠**: モダンコンパイラ（V8のTurboFan、GraalVM）はすべてフィードバック駆動の動的閾値を持つ
- **難易度**: Medium

- **関連実装**: `packages/engine/optimize/src/optimizer-inline.lisp` に `opt-adaptive-inline-threshold` を追加済み。現状は body cost と cheap-instruction 比率、call-heavy かどうかに基づいて inline threshold を 8..50 の範囲で調整し、`opt-pass-inline-iterative` はこの adaptive threshold を使う。実行時プロファイルカウンタや loop 深度を用いた完全な feedback-driven thresholding は未実装。

---

### Phase 28 — CPS高度最適化（未実装）

#### FR-159: Administrative Redex Elimination ✅

- **対象**: `packages/engine/compile/src/cps.lisp`
- **現状**: CPS変換が`(λ k. M) K`形式の冗長な継続ラッパーを大量生成（`cps.lisp:145-156`）。後段パスで除去されない
- **内容**: CPS変換直後にadmin redex `((λ k body) K)`を検出し`body[K/k]`に置換するパスを追加。FR-027のbeta reductionより先に適用して変換ノイズを除去
- **根拠**: MLtonはCPS変換時にadmin redexを即座に除去。cl-ccではネストしたラムダラッパーがそのまま`vm-closure`割り当てになる
- **難易度**: Medium

#### FR-160: Lambda Lifting

- **対象**: `packages/engine/compile/src/closure.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `find-free-variables`（`closure.lisp:48-81`）で自由変数を解析するが、内部関数を外部に持ち上げる変換なし。すべての内部`lambda`がクロージャ割り当てを発生
- **内容**: 自由変数が少ない（≤4）内部関数をトップレベルに持ち上げ、自由変数を追加引数として渡す。クロージャ割り当てをゼロにする
- **根拠**: `codegen.lisp:581-582`が空キャプチャでも常に`make-vm-closure`を生成。MLton/SML#はlambda liftingで大半のクロージャを除去
- **難易度**: Hard

#### FR-161: Arity Raising / Uncurrying

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: CPS変換後のカリー化された関数適用がそのまま複数回の`vm-call`に展開される。連続適用をバッチ化する機構なし
- **内容**: `(f a b)`が`((f a) b)`とCPS展開される場合、呼び出しサイトのarity解析でバッチ引数渡しに変換。中間クロージャ生成を除去
- **根拠**: GHCのarity analysis / MLtonのuncurrying。CPS形式で特に顕著な最適化
- **難易度**: Hard

#### FR-162: Known Continuation Optimization

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: すべての継続を汎用的に扱う（`cps.lisp:160-167`）。継続が1回だけ呼ばれるか、常に同じ継続が渡されるかを追跡しない
- **内容**: 継続の使用回数と呼び出しパターンを解析。線形継続（1回呼び出し）はインライン展開、非エスケープ継続はジャンプに変換（FR-028 contificationの前提解析）
- **根拠**: GHCのdemand analysis on continuations。cl-ccでは全継続がfirst-classクロージャとして生存
- **難易度**: Hard

---

### Phase 29 — 高度コードモーション（未実装）

#### FR-163: Code Sinking (逆LICM)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/optimize/src/cfg.lisp`
- **現状**: `optimizer.lisp`に"sink"/"hoist"関連の処理なし（grep 0マッチ）
- **内容**: 命令を定義位置から使用位置の直前まで移動。レジスタ圧力を削減し、使用されない分岐パスの命令を除去可能にする。LICM（FR-003）の逆操作
- **根拠**: LLVMの`MachineSink`パス。特に分岐の片方でのみ使われる値の移動に効果大
- **難易度**: Medium

#### FR-164: Partial Dead Code Elimination (PDCE)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: `opt-pass-dce`（`optimizer.lisp:494-513`）は全パスで未使用の命令のみ除去。一部パスでのみ使用される命令は残存
- **内容**: 一部の実行パスでのみ生存する値を検出し、不要パスの計算を除去またはsink。FR-165のpost-dominator解析が前提
- **根拠**: LLVMの`PartialDCE`。現行DCEでは`if`の片方でのみ使われる計算が両方のパスで実行される
- **難易度**: Hard

#### FR-165: Post-Dominator Analysis ✅

- **対象**: `packages/engine/optimize/src/cfg.lisp`
- **現状**: 支配木（dominator tree）は`cfg.lisp:210-243`に実装済みだが、逆支配木（post-dominator）は存在しない
- **内容**: CFGの逆グラフに対して`cfg-compute-dominators`を適用してpost-dominator treeを構築。制御依存グラフ (CDG) の基盤
- **根拠**: PDCE（FR-164）・code sinking（FR-163）・制御依存解析の前提インフラ
- **難易度**: Easy

#### FR-166: Constant Hoisting ✅

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: `opt-pass-fold`は定義位置で定数を畳み込むが、ループ内の高コスト定数を関数入口に移動しない
- **内容**: ループ内で繰り返し使用される高コスト定数（大きな整数リテラル、浮動小数点定数、シンボルルックアップ）を関数入口のプレヘッダに移動
- **根拠**: LLVMの`ConstantHoisting`パス。LICM（FR-003）とは別に定数専用のhoistingが効率的
- **難易度**: Easy

#### FR-167: Tail Duplication

- **対象**: `packages/engine/optimize/src/cfg.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 基本ブロックの重複機構なし。CFGは構築のみで変形操作なし
- **内容**: 複数の先行ブロックから分岐するブロックの末尾を各先行ブロックに複製。分岐予測改善とジャンプスレッディング機会拡大
- **根拠**: LLVMの`TailDuplication`。cl-ccのジャンプスレッディング（FR-001のpeephole、`opt-pass-jump`）の効果を増幅
- **難易度**: Medium

#### FR-168: Branch Correlation

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 同一条件が複数箇所でテストされる場合、それぞれ独立に評価。先行テスト結果の再利用なし
- **内容**: 値述語を支配木に沿って伝播し、先行分岐で確定した条件を後続分岐で定数化。`(if (integerp x) ... (if (integerp x) ...)` → 2回目を定数`t`に
- **根拠**: LLVMの`CorrelatedValuePropagation`。型チェック分岐が頻出するLispコードで特に効果大
- **難易度**: Medium

- **関連実装**: `packages/engine/optimize/src/optimizer.lisp` に `opt-pass-branch-correlation` を追加済み。現状は「単一 predecessor の `vm-jump-zero` が直前の foldable predicate / `vm-not` を条件にしている」ケースに限定し、後続ブロック内の同一 predicate 再評価を `vm-const` へ置換する保守的実装。一般の複数 predecessor 合流や非述語条件の相関伝播は未実装。

---

### Phase 30 — ループ構造最適化（未実装）

#### FR-169: Loop Rotation

- **対象**: `packages/engine/optimize/src/cfg.lisp`
- **現状**: `bb-loop-depth`フィールド（`cfg.lisp:32`）は存在するが未使用。自然ループの検出・回転なし
- **内容**: `while(cond) { body }` を `if(cond) { do { body } while(cond) }` に変換。ループ末尾にバックエッジを配置してCPUのループ分岐予測を改善
- **根拠**: LLVMの`LoopRotate`パス。cl-ccのLICM（FR-003）とloop unrolling（FR-022）の効果を最大化する前提変換
- **難易度**: Medium

#### FR-170: Loop Peeling

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/optimize/src/cfg.lisp`
- **現状**: ループの初回反復の分離なし。初回のみ特殊な処理が必要なケース（初期化ガード等）でも全反復が同一コード
- **内容**: ループの最初のN回（通常1回）を剥離してループ外にコピー。初回の型チェックや初期化コストをループ外に追い出す
- **根拠**: LLVMの`LoopPeel`。`(dolist (x list) ...)`の初回nil チェック除去に有効
- **難易度**: Medium

---

### Phase 6 — CPS・継続特化最適化 (cl-cc固有・高効果)（未実装）

#### FR-026: Eta Reduction ✅

- **対象**: `packages/engine/compile/src/cps.lisp` + `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `(lambda (x) (f x))` → `f` へのCPS変換後の冗長ラッパー除去
  - CPS変換が生成する多くの継続ラッパーはeta-reducible
  - GHCコア言語と同様のアプローチ
- **難易度**: Easy

#### FR-027: CPS Beta Reduction / Continuation Sharing ✅

- **対象**: `packages/engine/compile/src/cps.lisp` + `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - 継続が一度しか使われない場合、その場でインライン展開 (beta-reduce)
  - クロージャ割り当てと呼び出しオーバーヘッドを完全除去
  - CPS変換が生成するsingle-use継続ラッパーを大幅に削減
- **難易度**: Medium

#### FR-028: Contification

- **対象**: `packages/engine/compile/src/codegen.lisp` + `packages/engine/optimize/src/optimizer.lisp`
- **内容**:
  - `labels` で束縛された関数が常に末尾位置から呼ばれる場合、クロージャではなくローカルラベル(JMP先)に変換
  - クロージャ割り当て・環境設定のオーバーヘッドを完全除去
  - MLKit・Chicken Schemeが実装する主要最適化
  - 証拠: `packages/engine/vm/src/vm.lisp` の vm-closure-object を完全にバイパス
- **難易度**: Medium

#### FR-029: Join Points (共有継続) ✅

- **対象**: `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `if`/`cond` コンパイル時に複数の分岐が同じ合流点を持つ場合、重複したコードを共有ラベルに統合
  - GHCのJoin Point最適化と同等
  - 現状: 各分岐が独立して継続をコピーするため命令数が膨張する
- **難易度**: Medium

#### FR-030: Known-Call Optimization (既知アリティ直接ジャンプ)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
- **内容**:
  - 呼び出し先の引数数がコンパイル時に判明している場合、vm-callの汎用アリティチェックをスキップして直接エントリにジャンプ
  - 現状: `vm-call` は常にランタイムアリティ検査を行う (`vm.lisp:603-622`)
  - `labels`・`flet` の局所関数呼び出しはほぼ全てこの最適化が適用可能
- **難易度**: Medium

- **関連実装**: `packages/engine/compile/src/codegen.lisp` では、(1) 単純 self-tail call を `vm-jump` へ直接変換、(2) zero-capture の no-escape `flet` を `vm-closure` ではなく `vm-func-ref` で束縛、(3) tail position では `vm-tail-call` を選択、までは実装済み。一般の既知呼び出しを専用 `vm-direct-call` / アリティ検査省略パスへ落とす統一機構は未実装。

#### FR-031: One-Shot Lambda / Cardinality Analysis

- **対象**: `packages/engine/compile/src/closure.lisp` + `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - クロージャが正確に一度しか呼ばれない場合、ヒープにクロージャオブジェクトを割り当てない
  - CPS変換後の継続ラッパーの大半は one-shot
  - スタックフレームに直接格納することでGCプレッシャー削減
- **難易度**: Medium

- **関連実装**: `packages/engine/compile/src/closure.lisp` に `binding-direct-call-count-in-body` / `binding-one-shot-p` を追加済み。現状は局所関数束縛が「非エスケープかつ direct call 1 回だけ」かを判定する分析 helper 層のみで、実際の closure allocation 省略や stack storage への codegen 統合は未実装。

---

### Phase 19 — VMディスパッチ・SSA改善（一部実装）

#### FR-109: Superoperator Synthesis (超命令合成) ✅

- **対象**: `packages/engine/vm/src/vm-run.lisp` (`defopcode` DSL)
- **内容**: 頻出命令ペアを単一複合命令に融合。例: `vm-const r1 42; vm-add r0 r0 r1` → `vm-add-const r0 42`
- **根拠**: `defopcode` インフラが最適なフック。selfhostで頻度プロファイルを収集してターゲット列を特定
- **難易度**: Medium

- **関連実装**: `packages/engine/vm/src/vm-run.lisp` に `vm2-fuse-immediate-superinstructions` を追加済み。現状は flat VM2 bytecode 上で `const + add2/sub2/mul2`、`const + num-eq2/num-lt2/num-gt2/num-le2/num-ge2`、および `const + halt2` を局所融合し、既存の immediate/fused opcode へ置換する保守的 helper で、`run-vm` 実行前に自動適用される。一般化された superinstruction 合成や頻出トリプル融合は未実装。

#### FR-110: Inline Constant Arithmetic Opcodes ✅

- **対象**: `packages/engine/vm/src/vm-run.lisp`
- **内容**: `ADD-IMM`/`CMP-IMM-ZERO`等の即値バリアント命令を追加。ループカウンタ`(incf i)`, `(> i n)`の最頻出パターンを1ディスパッチサイクルに削減
- **難易度**: Easy

#### FR-111: Dispatch Loop Specialization ✅

- **対象**: `packages/engine/vm/src/vm-run.lisp`
- **内容**: 実行時間の80%を占めるホット命令5-10種を`run-vm`ループ本体にインライン展開、残りをテーブルディスパッチに
- **根拠**: CPython 3.11+ adaptive interpreter と同様のアプローチ
- **難易度**: Medium

- **関連実装**: `packages/engine/vm/src/vm-run.lisp` の `run-vm` は `const` / `move` / `add-imm2` / `sub-imm2` / `mul-imm2` / `halt2` をループ本体で直 dispatch し、それ以外を既存 `*opcode-dispatch-table*` へフォールバックする partial specialization を実装済み。完全な hot-opcode 適応化や wider opcode coverage は未実装。

#### FR-112: Critical Edge Splitting ✅

- **対象**: `packages/engine/optimize/src/cfg.lisp`
- **内容**: 後継が複数のブロックから前継が複数のブロックへのエッジに空のランディングパッドブロックを挿入
- **根拠**: PRE・コード移動・SSA破壊の正確性に必要な前提条件。約30行で実装可能
- **難易度**: Easy

#### FR-113: Loop-Closed SSA (LCSSA)

- **対象**: `packages/engine/optimize/src/cfg.lisp`, `packages/engine/optimize/src/ssa.lisp`
- **内容**: ループ内で定義されループ外で使用される全SSA値に対してループ出口ブロックにPhiノードを挿入
- **根拠**: `basic-block`に`loop-depth`フィールドは存在するが未使用
- **難易度**: Medium

#### FR-114: Pruned / Semi-Pruned SSA

- **対象**: `packages/engine/optimize/src/ssa.lisp`
- **内容**: ブロックローカル変数へのPhi挿入を省略。完全Cytronアルゴリズムの冗長Phi削減
- **根拠**: `ssa-place-phis`が実際の活性チェックなしでPhiを挿入している (`ssa.lisp` 参照)
- **難易度**: Medium

#### FR-115: May-Alias / Must-Alias Oracle

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**: 既存のcopy-prop `reg-track` を拡張してヒープポインタのエイリアス関係を軽量追跡
- **根拠**: `opt-pass-copy-prop`がスカラコピーのみ追跡、ヒープ参照は未追跡
- **難易度**: Easy-Medium

- **関連実装**: `packages/engine/optimize/src/optimizer.lisp` に `opt-compute-heap-aliases` / `opt-must-alias-p` / `opt-may-alias-p` を追加済み。現状は `vm-cons` / `vm-make-array` / `vm-closure` / `vm-make-closure` を fresh root として扱い、`vm-move` による must-alias 伝播を行う軽量 oracle で、slot store-to-load forwarding と dead-store elimination は moved-alias 越しの同一オブジェクトも扱える。より広い alias consumer への統合は未実装。

#### FR-116: Flow-Sensitive Type Narrowing ✅

- **対象**: `packages/foundation/type/src/checker.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**: `(if (typep x 'integer) ...)` のthenブランチで`x`をintegerに絞り込み、elseブランチで`(not integer)`に絞り込む
- **根拠**: `type-meet`/`type-join`/`extract-type-guard` は存在するが条件分岐後の型環境伝播がない
- **難易度**: Medium

#### FR-117: Superoperator Frequency Synthesis (自動検出)

- **対象**: `packages/engine/vm/src/vm-run.lisp` + selfhostパイプライン
- **内容**: `./cl-cc selfhost`実行中に命令バイグラム頻度を計測、上位20ペアを自動的にsuperoperator候補として出力
- **難易度**: Medium

- **関連実装**: `packages/engine/vm/src/vm-run.lisp` に `vm2-collect-opcode-bigrams` / `vm2-top-superoperator-candidates` / `run-vm-with-opcode-bigrams` を追加済み。現状は flat VM2 bytecode の静的 bigram 集計に加えて、実行時に実際に踏まれた opcode bigram の頻度も取得できる。`selfhost` 実行への自動接続や候補からの自動 superinstruction 生成は未実装。

#### FR-118: Polyvariant (k-CFA) Type Analysis

- **対象**: `packages/foundation/type/src/inference.lisp`
- **内容**: call-site感度付き型解析。`(funcall f 1)` と `(funcall f "a")` で`f`の型を独立して追跡
- **根拠**: 現状のHMは高階関数引数が単一モノモーフィック型に束縛される
- **難易度**: Hard

---

### Phase 60 — SSA高度化・言語機能（未実装）

#### FR-271: Trivial Phi Elimination (自明Phi除去) ✅

- **対象**: `packages/engine/optimize/src/ssa.lisp`
- **現状**: `ssa.lisp:68-98` — `ssa-place-phis`が活性チェックなしで全支配フロンティアにPhiを挿入。冗長なPhi（全引数が同一値のPhi）の除去パスなし
- **内容**: Phi最適化3パス: (1) 全引数同一のPhiを単一値に置換、(2) Phi-of-Phi連鎖の短絡、(3) 未使用Phiの除去。FR-114（Pruned SSA）と相補的。FR-147（SSA統合）後に適用
- **根拠**: Cytron et al. (1991) Section 5。SSA構築直後の標準クリーンアップ。GVN/SCCPの精度向上に寄与
- **難易度**: Easy

#### FR-272: Read Barrier Optimization (読み取りバリア最適化)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `gc.lisp:269-295` — SATB書き込みバリア実装済み。読み取りバリアなし。FR-190（並行GC）はマーキングの並行化だが、読み取りバリアの戦略未定
- **内容**: 並行コンパクション（FR-213）実行中のオブジェクト参照に読み取りバリアを挿入。Brooks pointer方式（各オブジェクトに転送ポインタ）またはload-reference-barrier方式（Shenandoah）。コンパクション非実行中はバリアをNOP化
- **根拠**: Shenandoah LRB / ZGC colored pointers / Azul C4。低レイテンシGCに必須
- **難易度**: Very Hard

#### FR-273: Rational Arithmetic Specialization (有理数演算特殊化)

- **対象**: `packages/engine/vm/src/vm-numeric.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `vm-numeric.lisp:566-621` — 有理数演算（rational, numerator, denominator, gcd, lcm）は全てホストCLにデリゲート。型特殊化なし
- **内容**: p/q ⊕ p'/q' を直接計算する特殊化パス。`(+ 1/3 1/4)` → コンパイル時にGCD計算して`7/12`に畳み込み。実行時はfixnum分子/分母の場合にfast path。fixnum + rational の混合演算最適化
- **根拠**: SBCL rational arithmetic / GMP mpq。数値計算の精度保証パス
- **難易度**: Medium

- **関連実装**: `packages/engine/optimize/src/optimizer-tables.lisp` の fold table に `vm-rational` / `vm-rationalize` / `vm-numerator` / `vm-denominator` / `vm-gcd` / `vm-lcm` を追加済み。現状は定数入力に対する compile-time folding までをカバーし、runtime fast path や mixed fixnum/rational specialization は未実装。

#### FR-274: Extensible Sequences Protocol (拡張可能シーケンスプロトコル)

- **対象**: `packages/engine/vm/src/list.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: シーケンス操作（map, reduce, find等）はlist/vectorのみ対応。ユーザー定義シーケンス型へのディスパッチ不可
- **内容**: SBCL `sb-sequence` プロトコルの実装。`sequence`クラスを継承したユーザー定義型に対して標準シーケンス関数が動作。`elt`, `length`, `make-sequence-like`, `adjust-sequence`のジェネリック関数基盤
- **根拠**: SBCL sb-sequence / Christophe Rhodes (2007)。ユーザー定義コレクションの相互運用性
- **難易度**: Hard

- **関連実装**: `packages/engine/vm/src/list.lisp` に `vm-sequence-elt` / `vm-sequence-length` / `vm-make-sequence-like` / `vm-adjust-sequence` の generic entry points を追加済み。現状は list/vector の built-in methods と user-defined type が method 追加できる最小 protocol slice を提供し、`vm-length` / `vm-nth` はこの protocol を経由して dispatch する。標準シーケンス関数全体のこの protocol への接続は未実装。

#### FR-275: Package-Local Nicknames (パッケージローカルニックネーム) ✅

- **対象**: `packages/engine/vm/src/packages.lisp`, `packages/frontend/parse/src/cl/parser.lisp`
- **現状**: パッケージシステムはグローバルニックネームのみ。パッケージ毎のローカルエイリアスなし
- **内容**: `(defpackage :foo (:local-nicknames (:a :alexandria)))` でパッケージ`foo`内でのみ`:a`が`:alexandria`を指す。シンボル解決時に現在パッケージのlocal-nickname-alistを参照。`add-package-local-nickname` / `remove-package-local-nickname` API
- **根拠**: SBCL / ECL / CCL / ABCL 全実装済み。de facto標準のCL拡張。CDR-10
- **難易度**: Medium

- **関連実装**: `packages/frontend/expand/src/macros-compat.lisp` の `defpackage` 展開は `:local-nicknames` オプションを解釈し、host CL の `add-package-local-nickname` / `remove-package-local-nickname` を呼び出すようになった。現状は package object への登録までで、reader/loader 全経路での互換性検証や parser 側専用解決は未確認。

---

### Phase 69 — 呼び出し規約・VM高速化（一部実装: callee-saved trim）

#### FR-326: Register Snapshot Elimination for Known Calls (既知呼び出しのレジスタスナップショット省略)

- **対象**: `packages/engine/vm/src/vm.lisp`
- **現状**: `vm-save-registers`（`vm.lisp:777-794`）が全関数呼び出しでレジスタハッシュテーブル全体をコピー。`vm-restore-registers`は`clrhash`+全`maphash`コピーバック。呼び出しごとにO(n)
- **内容**: 既知関数への呼び出し（アリティと捕捉変数が静的に判明）では、呼び出し地点で生存するレジスタのみを保存。`regalloc.lisp`の`compute-live-intervals`から活性情報を`vm-call`に注釈
- **根拠**: LLVM callee-saved analysis / V8 register liveness。呼び出しオーバーヘッドの大幅削減
- **難易度**: Medium

- **関連実装**: `packages/engine/vm/src/vm-dispatch.lisp` に `vm-save-registers-subset` / `vm-restore-registers-subset` を追加済み。現状は selective snapshot の helper 層のみで、known-call path へ live-reg ベースで自動適用する統合は未実装。

#### FR-327: VM Argument Dedicated Slots (VM引数専用スロット)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `vm-bind-closure-args`（`vm.lisp:796-835`）で引数をレジスタ名リストから`mapcar`でリスト収集（`vm.lisp:625`、毎回フレッシュリスト割り当て）、パラメータへの個別`vm-reg-set`
- **内容**: 固定引数レジスタ（`:ARG0`〜`:ARG7`）を導入し、一般レジスタファイルをバイパス。8引数以下の関数はリスト構築不要で直接レジスタ→レジスタ転送。`vm2-state`（`vm-run.lisp:181-201`）の固定256スロットベクタとも整合
- **根拠**: JVM invokestatic / x86-64 ABI引数レジスタ。呼び出しオーバーヘッド削減
- **難易度**: Medium

- **関連実装**: `packages/engine/vm/src/vm.lisp` に `+vm-arg-slot-count+` / `vm-arg-slot-name` / `vm-bind-arg-slots` を追加済み。`packages/engine/vm/src/vm-dispatch.lisp` の `vm-bind-closure-args` は現在、既存の通常パラメータ束縛に加えて `:ARG0..:ARG7` へも先頭引数をミラーする。専用スロットだけで一般レジスタファイルを完全にバイパスする call fast path への全面移行は未実装。

#### FR-328: Native CALL/RET Instruction Emission (ネイティブCALL/RET命令エミッション)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: `*x86-64-instruction-sizes*`で`vm-closure`=0、`vm-call`=0（`x86-64-codegen.lisp:909-911`）。ネイティブバックエンドに関数呼び出しエミッションが完全欠落。`calling-convention.lisp:8-24`の呼び出し規約インフラはレジスタ割り当てのみで使用
- **内容**: x86-64 `CALL rel32`命令エミッション。スタックフレームセットアップ（push return address, callee-saved regs）。`vm-ret`→`RET`命令+フレーム解体。FR-294（x86-64命令補完）の上位機能
- **根拠**: 全ネイティブコンパイラの基本機能。関数呼び出しなしではネイティブ実行が成立しない
- **難易度**: Hard

#### FR-329: Callee-Saved Register Usage Analysis (使用済みCallee-Savedレジスタ解析) ✅

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/regalloc.lisp`
- **現状**: `x86-64-codegen.lisp:1105-1111` — プロローグが常にRBX/RBP/R12/R13/R14/R15の6レジスタをPUSH。使用有無に関係なく全保存。`regalloc.lisp:295-298`の割り当て結果はプロローグ生成に参照されない
- **内容**: レジスタ割り当て後に使用済みcallee-savedレジスタを走査。使用されたもののみPUSH/POP。リーフ関数やレジスタ使用の少ない関数で2-10バイト+スタック圧力削減
- **根拠**: GCC/LLVM/SBCL全実装。FR-072（Shrink-Wrapping）・FR-177（Callee-Save Elimination）の前提となる基本分析
- **難易度**: Easy

#### FR-330: Closure Capture Deduplication (クロージャキャプチャ重複排除)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/compile/src/closure.lisp`
- **現状**: `codegen.lisp:333-335`（flet）・`codegen.lisp:405-408`（labels）で各クロージャが独立に捕捉変数リストを持つ。同スコープの兄弟クロージャ間でのキャプチャスロット共有なし
- **内容**: 同スコープの複数クロージャが同じ変数を捕捉する場合、共有環境レコードを割り当て各クロージャからそれを参照。割り当て削減とキャッシュ局所性向上。FR-043（環境フラット化）・FR-132（キャプチャトリミング）と相補的
- **根拠**: V8 shared function info / Chez Scheme shared environments。兄弟クロージャの環境共有
- **難易度**: Medium

- **関連実装**: `packages/engine/compile/src/closure.lisp` に `closure-capture-key` / `group-shared-sibling-captures` を追加済み。現状は sibling closure 群の capture 集合を canonical key でグループ化する分析 helper 層のみで、実際の共有環境レコード割り当てや codegen 統合は未実装。

- **完了済みFR**: FR-329

---

### Phase 74 — CPS/IR高度化（未実装）

#### FR-366: Selective CPS Transformation (選択的CPS変換)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: `cps.lisp:400-406` — `cps-transform*`が全式に一律CPS適用。直線コード（算術・データフロー）にも不要な継続ラムダを生成。`cps.lisp:106-107`で`ast-int`/`ast-var`も`(funcall ,k ...)`でラップ
- **内容**: 事前パスでAST各ノードを「CPS必要」(非局所脱出・第一級継続含む) vs「直接スタイル安全」に分類。CPS変換は前者のみに適用。~80%の直線コードで継続ラムダ生成を回避
- **根拠**: Kennedy (2007) "Compiling with Continuations, Continued" — selective CPS
- **難易度**: Medium

#### FR-367: ANF代替IR (A正規形によるCPS代替)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/compile/src/codegen-core.lisp`
- **現状**: CPS変換が過剰なadmin lambda生成（`cps.lisp:145-156`）。`codegen-core.lisp:126-165`の`compile-ast` for `ast-let`は既にlet列を効率処理可能
- **内容**: `anf-transform-ast`パスをCPS代替として追加。中間値をlet束縛で直列化し、継続ラムダを不要にする。FR-159のadmin redex除去自体が不要になる。パイプラインスイッチ`:ir-style :anf`/`:cps`
- **根拠**: Flanagan et al. (1993) "The Essence of Compiling with Continuations"
- **難易度**: Medium

#### FR-368: CPS Shrinking Reductions (CPS縮約最適化)

- **対象**: `packages/engine/compile/src/cps.lisp`
- **現状**: FR-026(η縮約)/FR-027(β縮約)は個別記載だが、Appel/Jim統合ワークリストアルゴリズム未記載。`cps.lisp:92-102`の`cps-transform-sequence`が多数の単一使用継続束縛を生成
- **内容**: Appel/Jim (1997)の縮約を統合CPS-levelパスとして実装。継続変数の使用回数censusを保持し、(a) 単一使用β収縮、(b) ηリダクション、(c) 死継続除去、(d) 定数畳み込み — をワークリスト駆動で準線形時間実行
- **根拠**: Appel & Jim (1997) "Shrinking Lambda Expressions in Linear Time"
- **難易度**: Medium

#### FR-369: SSA-CPS等価性ブリッジ (SSA-CPS Equivalence Bridge)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/engine/compile/src/cps.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: MIR層（`mir.lisp`）はBraunアルゴリズムでSSA構築。CPS（`cps.lisp`）は独立IR。数学的等価（Kelsey 1995）だが相互変換・解析共有なし
- **内容**: CPS↔SSA双方向変換レイヤー構築。CPS単一呼び出し継続→SSA基本ブロックパラメータ、CPS合流点→SSAφノード。オプティマイザパスをIR横断で共有可能にする
- **根拠**: Kelsey (1995) "A Correspondence between CPS and SSA"
- **難易度**: Hard

#### FR-370: CPS AST Node Coverage (CPS ASTノード網羅)

- **対象**: `packages/engine/compile/src/cps.lisp`
- **現状**: `ast-defun`, `ast-defvar`, `ast-defmacro`, `ast-values`, `ast-multiple-value-bind`, `ast-apply`, `ast-handler-case`, `ast-defclass`, `ast-defgeneric`, `ast-defmethod`, `ast-make-instance`, `ast-slot-value`, `ast-set-slot-value`, `ast-set-gethash`の13ノードに`cps-transform-ast`メソッドなし — 呼び出し時にno applicable methodエラー
- **内容**: 13ノード型全てにCPS変換メソッド追加。`ast-node`にデフォルトフォールバックメソッドも追加
- **根拠**: CPS変換の完全性要件
- **難易度**: Medium

- **関連実装**: `packages/engine/compile/src/cps-ast.lisp` に `ast-values` / `ast-multiple-value-bind` / `ast-apply` / `ast-defvar` / `ast-handler-case` / `ast-make-instance` / `ast-slot-value` / `ast-set-slot-value` / `ast-defclass` / `ast-defgeneric` / `ast-defmethod` / `ast-set-gethash` の保守的 CPS メソッドを追加済み。現状は host `multiple-value-call` / `multiple-value-bind` / `apply` / `defparameter` / `handler-case` / `make-instance` / `slot-value` / `setf` / `defclass` / `defgeneric` / `defmethod` / `gethash` を用いて対応 AST 範囲を広げている。全 13 ノード網羅や高精度な多値意味保存の完成版は未実装。

#### FR-371: CPS Host Control Flow Elimination (CPSホスト制御フロー除去)

- **対象**: `packages/engine/compile/src/cps.lisp`
- **現状**: `ast-block`/`ast-return-from`(`cps.lisp:182-197`)がホスト`block`/`return-from`を出力。`ast-tagbody`/`ast-go`(`cps.lisp:216-229`)がホスト`tagbody`/`go`を出力。`ast-catch`/`ast-throw`/`ast-unwind-protect`(`cps.lisp:233-279`)もホスト特殊形式を出力。ホスト以外のターゲットでCPS無効
- **内容**: 全非局所制御フローをCPS継続で実現。block→継続キャプチャ、return-from→継続呼び出し、tagbody→ラベル継続、go→ジャンプ継続。ネイティブバックエンド前提
- **根拠**: CPS変換の本来の目的 — 制御フローの明示化
- **難易度**: Hard

#### FR-372: CPS Continuation Deduplication in ast-if (CPS継続重複除去) ✅

- **対象**: `packages/engine/compile/src/cps.lisp`
- **現状**: `cps.lisp:124-130` — `ast-if`で継続`k`がthen/else両方にテキスト複製。`k`が大きなラムダの場合コードサイズ爆発
- **内容**: 分岐前に`k`をlet束縛し、then/else両方で変数参照。標準的なCPS最適化
- **根拠**: コードサイズ削減、GHC/MLtonの標準手法
- **難易度**: Easy

#### FR-373: CPS Boolean Correctness (CPS真偽値修正) ✅

- **対象**: `packages/engine/compile/src/cps.lisp`
- **現状**: `cps.lisp:129` — `ast-if`のCPSが`(if (zerop ,v) ...)`で条件判定。Common Lispの偽は`nil`のみ（0は真）
- **内容**: `(if (zerop ,v) ...)`を`(if (null ,v) ...)`または`(if ,v then else)`に修正
- **根拠**: ANSI CL仕様 — `nil`のみがfalse
- **難易度**: Easy

---

### Phase 82 — VMアーキテクチャ効率化（一部実装: vm-select load-order fix）

#### FR-454: VM Dispatch Optimization (VMディスパッチ最適化)

- **対象**: `packages/engine/vm/src/vm-run.lisp`
- **現状**: `run-compiled`(`vm-run.lisp:131-138`)が毎命令`execute-instruction` CLOSジェネリック関数ディスパッチ。GFメソッドルックアップのオーバーヘッド
- **内容**: typecase/defopcode方式に変換。または命令タイプごとのjump-tableディスパッチ
- **根拠**: V8/LuaJIT — fast interpreter dispatch
- **難易度**: Medium

#### FR-455: O(1) Instruction Fetch (定数時間命令フェッチ) ✅

- **対象**: `packages/engine/vm/src/vm-run.lisp`
- **現状**: `run-compiled`(`vm-run.lisp:133`)が`(nth pc instructions)`でリスト上のO(n)フェッチ。`run-program-slice`は`aref`でベクタO(1)
- **内容**: 命令リストをベクタに変換し`svref`/`aref`でO(1)アクセス
- **根拠**: 基本的なインタプリタ性能要件
- **難易度**: Easy

#### FR-456: Bytecode ISA v2 Completion (バイトコードISA v2完成)

- **対象**: `packages/backend/bytecode/src/encode.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: `encode.lisp:26-108`に50オペコードISA定義・エンコーダ・デコーダ・逆アセンブラ。`run-vm`(`vm-run.lisp:214-261`)が6オペコードのみ実装（const, move, add2, sub2, mul2, halt2）。コンパイラからバイトコードへの出力パスなし
- **内容**: 残り44オペコードのdefopcode実装。コンパイラからバイトコード出力パス構築。vm-state/vm2-state統合
- **根拠**: バイトコード実行エンジン完成
- **難易度**: Hard

- **関連実装**: `packages/engine/vm/src/vm-run.lisp` の VM2 実行器は当初の `const/move/add2/sub2/mul2/halt2` に加え、`nop` / `load-const` / `load-nil` / `load-true` / `load-fixnum`、`neg` / `inc` / `dec`、`eq` / `eql` / `equal`、`fixnump` / `consp` / `symbolp` / `functionp` / `stringp`、`div` / `mod`、`jump` / `jump-if-nil` / `jump-if-true`、`values` / `recv-values`、`return` / `return-nil`、`cons` / `car` / `cdr` / `make-vector` / `vector-ref` / `vector-set`、`make-hash` / `hash-ref` / `hash-set`、`get-global` / `set-global`、`make-instance`、`add-imm2` / `sub-imm2` / `mul-imm2`、`num-eq2` / `num-lt2` / `num-gt2` / `num-le2` / `num-ge2`、`num-eq-imm2` / `num-lt-imm2` / `num-gt-imm2` / `num-le-imm2` / `num-ge-imm2`、`const-halt2` まで実装済み。ISA 全体 50 opcode の完成やコンパイラからの bytecode 出力 path は未実装。

#### FR-457: vm-falsep Correctness Fix (vm-falsep正当性修正) ✅

- **対象**: `packages/engine/vm/src/vm.lisp`
- **現状**: `vm-falsep`(`vm.lisp:533-535`)が`nil`と整数`0`の両方をfalse扱い。CL仕様では`nil`のみがfalse
- **内容**: `(defun vm-falsep (v) (null v))` — `nil`のみfalse。全`vm-jump-zero`使用箇所を確認
- **根拠**: ANSI CL仕様 — 正当性バグ（FR-373のCPS版と同根）
- **難易度**: Low

#### FR-458: handler-case Stack Copy Optimization (handler-caseスタックコピー最適化) ✅

- **対象**: `packages/engine/vm/src/vm-run.lisp`
- **現状**: `vm-establish-handler`(`vm-run.lisp:36-48`)が`(copy-list (vm-call-stack state))`と全レジスタHTスナップショットをハンドラ設置毎に実行。O(stack-depth + register-count)
- **内容**: コールスタックの遅延コピー（COWまたはスナップショット不要化）。レジスタはフレーム境界でのみ保存
- **根拠**: handler-case性能問題 — ホットパスでの不要コピー
- **難易度**: Medium

#### FR-459: Superinstruction / Opcode Fusion (スーパー命令/オペコード融合)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: CLOSインタプリタもdefopcodeエンジンも命令融合なし。頻出シーケンス（const+jump-zero, move+ret, const+add等）を個別ディスパッチ
- **内容**: 頻出命令ペア/トリプルのプロファイリング。融合スーパー命令の定義と最適化パスでの置換
- **根拠**: CPython 3.12 superinstructions, LuaJIT
- **難易度**: Hard

- **関連実装**: `packages/engine/vm/src/vm-run.lisp` の `vm2-fuse-immediate-superinstructions` は `const` と算術/比較 opcode、および `halt2` の隣接ペアを既存 fused opcode へ置換し、`run-vm` は実行前にこれを適用する。頻出ペア/トリプルの一般プロファイリングと自動融合は未実装。

#### FR-460: Label Table Integer Keys (ラベルテーブル整数キー) ✅

- **対象**: `packages/engine/vm/src/vm-run.lisp`
- **現状**: `build-label-table`(`vm-run.lisp:102-109`)が`#'equal`テスト（文字列比較）のHTを生成。毎ジャンプで文字列ハッシュ計算
- **内容**: ラベルを整数ID化し`#'eql`テストHTまたは直接配列インデックスに変更
- **根拠**: ジャンプ性能改善
- **難易度**: Low

#### FR-461: Closure Captured-Values Vector (クロージャキャプチャ値ベクタ化) ✅

- **対象**: `packages/engine/vm/src/vm.lisp`
- **現状**: `vm-closure-ref-idx`(`vm.lisp:688-693`)が`(nth idx values-list)`でO(n)アクセス
- **内容**: captured-valuesをリストからベクタに変更。`svref`でO(1)アクセス
- **根拠**: クロージャ変数アクセス性能
- **難易度**: Low

#### FR-462: vm-apply Efficient Argument Spreading (vm-apply効率的引数展開) ✅

- **対象**: `packages/engine/vm/src/vm.lisp`
- **現状**: `vm-apply`(`vm.lisp:965-968`)が`(append (butlast arg-values) ...)`で2回走査+新リスト割り当て
- **内容**: 最終引数のspliceを1パスで実行。または`nconc`＋最終要素直接リンク
- **根拠**: apply性能改善
- **難易度**: Low

#### FR-463: vm-sync-handler-regs Batching (ハンドラレジスタ同期バッチ化) ✅

- **対象**: `packages/engine/vm/src/vm-run.lisp`
- **現状**: `vm-sync-handler-regs`(`vm-run.lisp:56-62`)が全ハンドラ×全レジスタのネストmaphash。O(handlers × registers)
- **内容**: ハンドラ設置時のレジスタスナップショットを遅延化。例外発生時のみ復元計算
- **根拠**: handler-case非例外パス性能
- **難易度**: Medium

---

### Phase 90 — Sea of Nodes IR & スケジュール自由IR（未実装）

#### FR-530: Sea of Nodes IR (スケジュール自由IR)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: MIR層はCFGベースのSSA（Braunアルゴリズムでφ挿入）。制御フローとデータフローが分離されていない従来型表現
- **内容**: Cliff Click (1995) の Sea of Nodes IR。制御ノードとデータノードを統一グラフに混在させ、命令順序を遅延してコード生成時に決定。φノードを Region/Phi ノードペアに置換。Loop/Start/End ノードで自然ループを表現。スケジューリングの自由度が高く、GVN・型推論・インライン化を単一パスで実行可能
- **根拠**: V8 TurboFan / GraalVM Graal compiler。HotSpot C2 も同系統。スケジュール独立性により GVN 精度が大幅向上
- **難易度**: Very Hard

#### FR-531: Schedule-Early / Schedule-Late (命令スケジューリング戦略)

- **対象**: `packages/foundation/mir/src/mir.lisp`
- **現状**: MIRに固定の命令順序
- **内容**: Sea of Nodes (FR-530) 上の2段階スケジューリング: (1) Schedule-Early — dominance最小ノード（支配木上で最も浅い位置）に命令を置く。(2) Schedule-Late — 使用箇所に最も近い位置に命令を置く。実際のスケジューリングは Early と Late の間の「最適位置」（最も外側のループかつ liveness 考慮）を選択
- **根拠**: Cliff Click (1995) "Global Code Motion / Global Value Numbering"。SSAベース GVN とコードモーションの統一手法
- **難易度**: Hard

---

### Phase 91 — SSA形式変換・破壊（未実装）

#### FR-532: Out-of-SSA / Phi Node Coalescing (SSA破壊・Phi合体)

- **対象**: `packages/engine/optimize/src/ssa.lisp`, `packages/backend/emit/src/regalloc.lisp`
- **現状**: SSA構築（`ssa.lisp`）は完備。SSA破壊（Phi→コピー命令への変換）なし。レジスタ割り当て（`regalloc.lisp`）との接続なし
- **内容**: SSA破壊3ステップ: (1) 各 Phi をその引数の数だけのコピー命令に展開。(2) Phi-coalescing — 干渉しないコピーを同一物理変数に統合してコピー命令を消去。(3) Parallel copy sequentialization — 同一基本ブロック内の並行コピー群を Swap-based/一時変数方式でシーケンシャル化。Sreedhar et al. (1999) または Boissinot et al. (2009) のアルゴリズム
- **根拠**: SSAベース RA の標準最終ステップ。cl-cc では SSA→RA の橋渡しが欠如
- **難易度**: Hard

#### FR-533: Parallel Copy Sequentialization (並行コピー逐次化)

- **対象**: `packages/backend/emit/src/regalloc.lisp`
- **現状**: SSA破壊後の並行コピー（`{r1←r2, r2←r1}` のような循環依存）を正しく逐次化する機構なし
- **内容**: 依存グラフ（DAG）を構築。非循環部分はトポロジカル順にコピー生成。循環部分は XOR-swap または一時レジスタで解消。`vm-move` 命令列への展開
- **根拠**: Boissinot et al. (2009) / GCC out-of-SSA。正当性に関わる基礎コンポーネント
- **難易度**: Medium

#### FR-534: Interprocedural SSA / Function Summaries (手続き間SSA・関数サマリー)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: SSAは関数内のみ。関数境界を越えたSSA値の追跡なし
- **内容**: 関数ごとに「入力条件 → 出力特性」のサマリー（副作用集合・戻り値型・純粋性フラグ）を構築。サマリーを用いた手続き間定数伝播・エイリアス解析。IPSCCP（FR-050）の基盤
- **根拠**: LLVM GlobalsModRef / Inliner function attrs。手続き間最適化の標準インフラ
- **難易度**: Hard

#### FR-535: Gated SSA / E-SSA (述語付きSSA)

- **対象**: `packages/engine/optimize/src/ssa.lisp`, `packages/foundation/type/src/inference.lisp`
- **現状**: SSAの Phi ノードに値条件の述語情報なし。分岐条件と Phi の関係が失われる
- **内容**: Gated SSA (Tu & Padua 1995): `γ-node`（条件付き Phi）と `μ-node`（ループ帰納 Phi）を区別。E-SSA: Phi 引数に述語コンテキスト（`x > 0` のような条件）を付与。Flow-sensitive type narrowing（FR-116）の精度向上。配列範囲解析（FR-039）の基盤
- **根拠**: LLVM LazyValueInfo / GCC VRP の述語追跡。Bourdoncle widening operator
- **難易度**: Hard

---

### Phase 92 — 代数的エフェクト・限定継続（未実装）

#### FR-536: Algebraic Effects / Effect Handlers (代数的エフェクト)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/vm/src/vm.lisp`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: CL `handler-case`/`restart-case` は `vm-conditions.lisp` で実装。代数的エフェクト（再開可能なエラー処理より一般的な機構）なし
- **内容**: `(perform eff args)` / `(handle body (eff (args k) handler-body))` 形式の代数的エフェクトシステム。継続 `k` は再開可能。CPS変換でエフェクトの穿孔（effect tunneling）を実現。one-shot継続の場合はスタック複製不要。非決定性・状態・例外・I/Oを統一的に扱う
- **根拠**: OCaml 5 multicore effects / Koka / Eff。2024-2026年のモダン言語ランタイムの主要機能。CPS自体がエフェクトエンコーディングと等価なため CPS 層との統合が自然
- **難易度**: Very Hard

#### FR-537: Delimited Continuations (限定継続 — shift/reset)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: `block`/`return-from` による非限定脱出のみ。`call/cc` なし
- **内容**: `(reset body)` / `(shift k body)` または `(call-with-prompt tag body handler)` / `(abort-to-prompt tag val)` の限定継続プリミティブ実装。CPS変換での表現: プロンプトを継続スタックのセグメント境界として表現。`dynamic-wind` との統合。コルーチン・非同期I/O・バックトラッキングの基盤
- **根拠**: Racket `call-with-continuation-prompt` / Guile delimited continuations / R6RS `(rnrs control)`。Common Lispの `call/cc` 代替として機能
- **難易度**: Very Hard

#### FR-538: Continuation Marks (継続マーク — SRFI-157)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: コールスタックにユーザ定義メタデータを付与する機構なし
- **内容**: `(with-continuation-mark key value body)` / `(current-continuation-marks)` プリミティブ。各コールフレームに key-value マップを付与。現在の継続チェーンのマーク集合を O(stack-depth) で取得。デバッガ・プロファイラ・動的スコーピング・スタック検査に活用
- **根拠**: SRFI-157 / Racket continuation marks / Java StackWalker API。軽量なスタック注釈機構
- **難易度**: Medium

---

### Phase 93 — VMアーキテクチャ高度化（未実装）

#### FR-539: On-Stack Replacement (OSR — スタック上での差し替え)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: 関数の再コンパイルは次の呼び出しから有効。実行中の関数を最適化版に切り替える OSR なし
- **内容**: VMインタプリタのループバックエッジに OSR チェックポイントを挿入。ホットループ検出時に現在の実行状態（レジスタ・スタック・PC）を最適化コンテキストにマッピングしてネイティブコードに飛び込む「OSR-in」。最適化コードが投機的仮定を破った場合に VM に戻る「OSR-out」（deoptimization）
- **根拠**: HotSpot C1/C2 OSR / V8 OSR。長時間実行ループの最適化に必須。FR-244（Trace JIT）と相補的
- **難易度**: Very Hard

#### FR-540: Direct Threaded / Computed-Goto Dispatch (直接スレッドディスパッチ)

- **対象**: `packages/engine/vm/src/vm-run.lisp`
- **現状**: `run-compiled` が `execute-instruction` CLOS メソッドディスパッチ（FR-454で改善予定）。`run-vm` が defopcode ベースの `case` ディスパッチ
- **内容**: GNU C 拡張の `&&label`（computed goto）またはダイレクトスレッディング方式。各命令ハンドラの末尾に次命令のアドレスへの直接ジャンプを埋め込む。CPython 3.11+ / LuaJIT / Dalvik が採用。Common Lisp 実装では CFFI + callback 経由またはホスト SBCL の jump-table 生成を利用可能
- **根拠**: CPython 3.11 specializing adaptive interpreter。switch ディスパッチ比 20-30% 高速化。FR-111（Dispatch Loop Specialization）の実装基盤
- **難易度**: Hard

#### FR-541: Precise Deoptimization Frame Reconstruction (精密デオプト・フレーム復元)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: ガード失敗時（FR-232 Uncommon Trap）の正確な VM 状態復元機構なし
- **内容**: 各最適化コンパイルポイントに「逆マッピングテーブル」（物理レジスタ→仮想レジスタ対応）を付与。ガード失敗時に CPU レジスタから VM 状態を復元し、対応する VM PC から解釈実行を再開。インライン展開された関数のフレームも個別復元（inlined frame materialization）
- **根拠**: HotSpot deopt / V8 deopt / GraalVM deopt。投機的最適化の安全ネット。FR-231（Stack Map）の上位機能
- **難易度**: Very Hard

#### FR-542: Coroutine / Fiber VM Support (コルーチン/ファイバーVMサポート)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: VM は単一実行スレッド。コルーチン・非同期I/O・generator のプリミティブなし
- **内容**: VM レベルの `vm-yield` / `vm-resume` 命令追加。各コルーチンは独立した VM 状態（レジスタ・スタック）を保持。M:N スケジューリング（多数のファイバーを OS スレッドにマッピング）。`cl:generators`/`cl:lazy-sequences` の実装基盤。FR-537（限定継続）がある場合はそれで実装可能
- **根拠**: Python generators / Lua coroutines / Go goroutines / OCaml 5 Domain+Effect。非同期I/O とストリーム処理の基盤
- **難易度**: Hard

#### FR-543: Polymorphic Inline Cache Patching (PICパッチング — 自己書き換えIC)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: IC（FR-009 Monomorphic）はハッシュテーブルベース。コンパイル済みコードの命令列を書き換える self-modifying 型 IC なし
- **内容**: 生成済みネイティブコードの呼び出しサイトに IC stub をパッチ。初回呼び出し時は「IC miss stub」→ タイプチェック+分岐を挿入してコードを書き換え。V8 の Inline Cache は CodeStub Arch を使用。ネイティブバックエンドに適用可能
- **根拠**: V8 IC patching / HotSpot inline caches。IC ルックアップコストをゼロに近づける
- **難易度**: Very Hard

---

### Phase 94 — CPS変換・継続表現高度化（一部実装: CPS基盤）

#### FR-544: CPS to Direct Style Transformation (CPS→直接スタイル逆変換)

- **対象**: `packages/engine/compile/src/cps.lisp`
- **現状**: CPS変換は一方向のみ。CPS形式で最適化後に Direct Style に戻す変換なし
- **内容**: CPS形式の検査で「継続が単純な関数呼び出し継続でありエスケープしない」場合を検出し、直接スタイルに逆変換。`(let ((k (lambda (v) (f v)))) ...)` → `(f ...)` 形式への縮退。管理ラムダ（administrative lambda）の大部分を除去。Kennedy (2007) selective CPS変換の後処理
- **根拠**: MLton / Chez Scheme。CPS変換→最適化→直接スタイル変換のパイプラインで最終コードの継続ラムダを除去
- **難易度**: Hard

#### FR-545: Trampoline-Based TCO (トランポリンTCO)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: 末尾呼び出し最適化は `vm-tail-call` 命令（`codegen-functions.lisp`）で対応。しかし CPS 変換後の継続ラムダが末尾位置にある場合のトランポリン変換なし
- **内容**: CPS変換後に末尾呼び出し以外の位置での継続呼び出しをトランポリン形式に変換。`(lambda () (k v))` サンクを返し、外部ループで強制実行。スタックオーバーフローを根絶。Chicken Scheme の CPS+ヒープスタック方式とは別アプローチ
- **根拠**: Clojure `recur` / Python trampoline pattern。CPS末尾呼び出しの完全なスタック使用量制御
- **難易度**: Medium

#### FR-546: Chicken-Style Stack Copying Continuations (スタックコピー継続)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: 継続はCPS変換でクロージャとしてヒープ割り当て。スタック全体のコピーによる第一級継続未実装
- **内容**: Chicken Scheme 方式: 実行スタックを VM 専用スタックに配置。スタックが溢れたら生きているデータをヒープにコピーして GC（"Baker's Cheney on the MTA"）。`call/cc` は現在のスタックフレームを継続オブジェクトとしてヒープにコピー。一貫した CPS + GC ベース設計
- **根拠**: Chicken Scheme "A practical use of 'Cheney on the MTA'" / Gambit-C。第一級継続の効率的実装
- **難易度**: Very Hard

---

### Phase 95 — 型付きIR・エフェクトシステム（未実装）

#### FR-547: Effect System in IR (IRエフェクトシステム)

- **対象**: `packages/engine/optimize/src/effects.lisp`, `packages/foundation/mir/src/mir.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `effects.lisp` に命令別副作用分類（`inst-effects`）が存在。しかし MIR/CPS IR ノードへのエフェクト注釈なし
- **内容**: 各 IR 命令に `{read-heap, write-heap, alloc, io, raise}` のエフェクトセットを付与。エフェクトベースのコードモーション判定（純粋命令のみ hoist/sink）。エフェクト推論（関数シグネチャにエフェクトを伝播）。Koka / OCaml 5 の effect inference に相当
- **根拠**: エフェクト情報で最適化の保守性を排除。現行 `opt-inst-pure-p` が未完全な純粋性チェックをしているが、エフェクトシステムで完全化
- **難易度**: Hard

#### FR-548: Typed SSA / Bidirectional Type Checking in IR (型付きSSA・双方向型検査)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/foundation/type/src/checker.lisp`
- **現状**: MIR 命令に型注釈なし。型チェック（`checker.lisp`）は AST レベルで実行され IR に伝播しない
- **内容**: MIR 命令の各引数と結果に ML 風型を付与。Phi 命令には合流型（join type）を付与。型伝播で SSA use-def 鎖に沿って型情報を伝播。双方向型検査（synthesis mode + checking mode）を `check/synth` モードで実装。不要な型チェック命令（`vm-integer-p` 等）を型証明で消去
- **根拠**: LLVM typed IR / Typed Assembly Language (TAL)。型情報を IR に保持することで型ベース最適化の精度向上
- **難易度**: Hard

#### FR-549: Multi-Level IR / Progressive Lowering (マルチレベルIR・段階的降下)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/foundation/mir/src/mir.lisp`
- **現状**: コンパイルパイプラインは `parse → expand → CPS → codegen → VM命令 → [MIR] → native` の不連続なステージ。各ステージ間の変換が大きすぎてデバッグが困難
- **内容**: MLIR スタイルの段階的降下: High-level IR（defun/let/if 等） → Mid-level IR（関数呼び出し・クロージャ展開済み） → Low-level IR（レジスタ・メモリ明示） → Machine IR（ターゲット命令）。各レベルで独立した検証・最適化が可能
- **根拠**: MLIR (Lattner et al. 2020) / LLVM-IR → SelectionDAG → MachineIR。コンパイラの保守性・拡張性を大幅改善
- **難易度**: Very Hard

---

### Phase 96 — 高度VM・JIT最適化（未実装）

#### FR-550: Object Shape / Hidden Class Tracking (オブジェクトシェイプ追跡)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: CLOS インスタンスはスロット名→値のハッシュテーブル（`vm-clos.lisp`）。シェイプ（スロット順序）固定の最適化なし
- **内容**: V8 の Hidden Class / SpiderMonkey の Shape に相当する機構。CLOS インスタンスのスロット集合を「シェイプID」で識別。同シェイプのインスタンスはスロットオフセットを固定配列アクセスに最適化（ハッシュ不要）。メソッド追加/削除時のシェイプ遷移グラフ管理。IC（FR-009/FR-023）との連携でメソッド探索をシェイプチェック+直接オフセットに変換
- **根拠**: V8 hidden classes / SpiderMonkey shape tree / LuaJIT table shapes。動的言語 VM 最適化の核心
- **難易度**: Hard

#### FR-551: Tiered VM Interpreter (段階的VMインタプリタ)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: 単一の VM インタプリタ（CLOS ベース `run-compiled` または defopcode `run-vm`）。バイトコード V2（FR-456）も独立実装。段階的コンパイルなし
- **内容**: 3段階実行: (1) Tier-0 — CLOS インタプリタ（現行、起動コスト最小）。(2) Tier-1 — バイトコード JIT（FR-456 完成後、速度改善）。(3) Tier-2 — 型フィードバック付き最適化 JIT（FR-058/FR-244、最大性能）。呼び出し回数カウンタで昇格トリガー。V8 の Ignition→Maglev→TurboFan、HotSpot の C1→C2 に相当
- **根拠**: V8 tiered compilation / HotSpot tiered compilation。起動時間とピーク性能の両立
- **難易度**: Very Hard

#### FR-552: Speculative Monomorphization (投機的モノモーフィゼーション)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: ジェネリック関数は常に多態ディスパッチ（`vm-generic-call`）。型フィードバック情報があっても特化コードを生成しない
- **内容**: IC 型プロファイル（FR-058）から最頻出型を取得し、その型に特化したモノモーフィックな関数クローンを生成。実行時型ガード（FR-232）を先頭に置き、ガード成功時は特化版へ、失敗時は汎用版にフォールバック。`(defgeneric +)` の `(fixnum, fixnum) → fixnum` 特化が代表例
- **根拠**: GraalVM SpeculativeMonomorphization / V8 Maglev モノモーフィック IC 最適化。CLOS パフォーマンスの主要ボトルネック解消
- **難易度**: Hard

#### FR-553: Bytecode AOT Serialization (バイトコードAOTシリアライズ)

- **対象**: `packages/backend/bytecode/src/encode.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: `encode.lisp`（50オペコード ISA 定義・エンコーダ・デコーダ）が実装済みだが、コンパイル済みバイトコードの永続化なし。毎起動時に全ソースを再コンパイル
- **内容**: コンパイル済み VM 命令列をバイトコードフォーマット（encode.lisp の ISA を使用）にシリアライズして `.clcc` ファイルに保存。次回起動時はデシリアライズのみでソースコンパイルをスキップ。ソースハッシュ検証付き（変更時に再コンパイル）。`./cl-cc compile foo.lisp → foo.clcc`、`./cl-cc run foo.clcc`
- **根拠**: Python `.pyc` / Java `.class` / Ruby `.rbc`。起動時間をソースコンパイルから分離。FR-405（Startup Optimization）と連携
- **難易度**: Medium

---

### Phase 97 — SSA高度化・末尾最適化補完（未実装）

#### FR-554: Tail Modulo Cons (TMC — 末尾Cons変換)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: TCO（`vm-tail-call`）は末尾位置の関数呼び出しのみ対象。`(cons x (f rest))` のような「末尾Consセル構築」はスタックを消費し続ける
- **内容**: `(cons head (f tail))` パターンを検出し、アキュムレータセルへの「後置き」に変換。具体的には事前にConsセルを確保してCDRスロットをホールとして残し、再帰呼び出しをそのスロットへの書き込み先として渡す。末尾再帰版 `map`/`filter`/`append` が定数スタックで動作するようになる
- **根拠**: OCaml 4.14 `[@tail_mod_cons]` annotation / GHC `-O2` の `build/foldr` fusion。Lisp のリスト処理関数群を完全末尾再帰化する唯一の手段
- **難易度**: Hard

#### FR-555: Demand-Driven SSA Construction (需要駆動SSA構築)

- **対象**: `packages/engine/compile/src/ssa.lisp`
- **現状**: SSA構築は `ssa-insert-phi-nodes` でCFG全体にφ関数を挿入（Cytron et al. 全量挿入法）。利用されないφ関数も生成される
- **内容**: 最適化パスが特定変数のSSAフォームを要求したときだけφ関数を挿入する遅延構築。Cooper & Harvey "A Simple, Fast Dominance Algorithm"の支配辺境界を利用しつつ、実際に参照される変数のみ処理。不要φ関数の初期生成ゼロでコンパイル時間短縮
- **根拠**: LLVM `mem2reg` の需要駆動アルゴリズム / GCC の LazySSA。小関数多数の Common Lisp スタイルに特に有効
- **難易度**: Medium

#### FR-556: Incremental SSA Maintenance (増分SSA更新)

- **対象**: `packages/engine/compile/src/ssa.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 最適化パス（インライン展開、定数畳み込み等）がIRを変更するたびに `ssa-transform` を再実行して完全再構築。変更点が局所的でも全関数を再処理
- **内容**: φ関数の追加・削除・書き換えを追跡する差分ログを維持。各最適化パスが「変更されたCFGエッジ」を報告し、影響を受けるφ関数のみを再計算。Braun et al. (2013) の増分アルゴリズム（本プロジェクトのSSA基盤と同系統）を拡張して差分更新対応
- **根拠**: LLVM `SSAUpdater` / GCC `update_ssa()`。大規模インライン展開後の再SSA化コストを O(変更量) に抑制
- **難易度**: Hard

---

---

### Phase 18 — LTO・PGO・コンパイラ速度（未実装）

#### FR-102: LTO Whole-Program Call Graph

- **対象**: コンパイルパイプライン全体
- **内容**: リンク時の全プログラム呼び出しグラフ構築による手続き間解析
- **難易度**: Hard

#### FR-103: Cross-Module Constant Folding

- **対象**: コンパイルパイプライン
- **内容**: `defconstant`/`defvar` のリテラル初期値をモジュール境界を越えて伝播
- **難易度**: Medium

#### FR-104: PGO Edge Profiling Instrumentation

- **対象**: `packages/engine/optimize/src/cfg.lisp` + バックエンド
- **内容**: CFGエッジにカウンタを挿入して実行頻度プロファイルを収集
- **難易度**: Medium

#### FR-105: Profile-Guided Inlining Thresholds

- **対象**: `packages/engine/optimize/src/optimizer.lisp:729-820`
- **内容**: プロファイルデータに基づいてcall-site毎にインライン化コスト閾値を調整
- **依存**: FR-104
- **難易度**: Low (FR-104完了後)

#### FR-106: Parallel File Compilation

- **対象**: `packages/cli/src/main.lisp`
- **内容**: 依存関係のないソースファイルを並列コンパイル (`./cl-cc selfhost` 84ファイルの高速化)
- **難易度**: Medium

#### FR-107: Incremental Recompilation with Dependency Graph

- **対象**: コンパイルパイプライン
- **内容**: ソースハッシュ/タイムスタンプによる変更ファイルのみ再コンパイル
- **根拠**: 現状は毎回全84ファイルを再コンパイル
- **難易度**: Medium

#### FR-108: Work-List Driven Optimizer Pass Scheduling

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **内容**: 前パスで変更された命令のみを次パスで処理するwork-list方式
- **根拠**: 現状は全命令を複数回フルスキャン
- **難易度**: Medium

---

### Phase 46 — IC状態機械・型フィードバック高度化（未実装）

#### FR-223: IC State Machine (Mono→Poly→Mega遷移)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: FR-009でMonomorphic IC、FR-023でPolymorphic IC + Megamorphic fallbackを定義しているが、状態遷移ロジック（mono→poly→mega）の明示的な状態機械定義なし
- **内容**: 各call-siteにIC状態（uninitialized/monomorphic/polymorphic/megamorphic）を付与し、ミス率に基づいて昇格・降格を制御する状態機械を実装。Megamorphic→deoptimize（VM解釈に戻す）のパス含む
- **根拠**: V8 IC state machine / HotSpot C1→C2 deoptimization。FR-009/FR-023の実装品質を制御する中核機構
- **難易度**: Hard

#### FR-224: VM Sampling Profiler (VMサンプリングプロファイラ)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/cli/src/main.lisp`
- **現状**: `defopcode`実行時の統計収集なし。ホットスポット検出にはホストCLのprofilerを使用するしかない
- **内容**: VMインタプリタループに定期的なPC（プログラムカウンタ）サンプリング挿入。命令カウンタまたは時間ベースでサンプリング。`./cl-cc run --profile` でフレームグラフ出力。FR-058（Type Feedback PGO）のデータソース
- **根拠**: V8 --prof / perf / async-profiler。自前VMの性能分析基盤としてPGO（FR-104/FR-105）の前提条件
- **難易度**: Medium

#### FR-225: Allocation Elimination via Escape Analysis (エスケープ解析による割り当て完全除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: FR-007でエスケープ解析、FR-018でスタック割り当て、FR-020でallocation sinkingを定義。しかし「割り当て自体の完全除去」（スカラー置換による）は明示的に定義されていない
- **内容**: エスケープ解析でローカルにのみ使用される`cons`/`list`/`vector`を検出し、SROA（FR-014）と組み合わせて個別レジスタに分解。割り当て命令・GCルート登録・ヒープアクセスを完全除去
- **根拠**: GraalVM partial escape analysis / HotSpot C2 scalar replacement。FR-007/FR-014/FR-018の統合による最大効果
- **難易度**: Hard

---

### Phase 48 — 投機的最適化基盤（未実装）

#### FR-231: Stack Map Construction (スタックマップ構築)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/backend/emit/src/regalloc.lisp`
- **現状**: MIRに`:safepoint`キーワード定義済み（`mir.lisp:13,158`）だがスタックマップデータ構造なし。GCルートの位置情報を機械語アドレスに紐づける仕組みがない
- **内容**: 各safepoint位置でのGCルートマップ（どのレジスタ/スタックスロットがGCトレース可能な参照を保持するか）を構築。`.llvm_stackmaps`互換のコンパクトなバイナリエンコーディング
- **根拠**: LLVM StackMaps / HotSpot OopMap / GraalVM ReferenceMap。正確なGC（FR-190）とdeopt（FR-155）の前提条件
- **難易度**: Hard

#### FR-232: Uncommon Trap Instructions (アンコモントラップ命令)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 型ガード（`extract-type-guard` in `inference.lisp:231`）は型推論層のみ。コード生成に投機的型チェック＋失敗時の脱出パスなし
- **内容**: `vm-guard-type`/`vm-guard-fixnum`等のガード命令を追加。ガード失敗時にuncommon trapハンドラへジャンプし、FR-155のdeoptimization経路に接続。投機的型仮定の「賭け」と「保険」の分離
- **根拠**: HotSpot uncommon_trap / V8 deopt_reason / GraalVM SpeculationLog。投機的最適化の安全ネット
- **難易度**: Hard

#### FR-233: Safepoint Polling Mechanism (セーフポイントポーリング)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: STW GC（`gc.lisp:200-263,331-392`）にスレッド停止要求メカニズムなし。VMインタプリタループにポーリングポイントなし
- **内容**: 関数エントリ・ループバックエッジ・アロケーションサイトにポーリングチェック挿入。ポーリングページ方式（メモリページの保護属性変更でシグナルトラップ）またはフラグチェック方式。FR-090/FR-091のsafepoint最適化の前提
- **根拠**: HotSpot polling page / Go runtime preemption / Chez Scheme interrupt check。並行GC（FR-190）の基盤
- **難易度**: Medium

---

### Phase 52 — 動的コンパイル・トレース（未実装）

#### FR-244: Trace-Based Dynamic JIT (トレースベースJIT)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: FR-154（Tiered Compilation）はAOT的な2段階コンパイル。実行時のホットパス記録・コンパイル基盤なし。FR-224（Sampling Profiler）はサンプリングであってトレース記録ではない
- **内容**: VMインタプリタループにトレース記録モードを追加。ホットループのバックエッジでトレース記録開始、ループ出口・サイドイグジットでトレース終了。記録されたトレースを型特殊化してネイティブコードにコンパイル。サイドトレース（ガード失敗時の分岐パス）の遅延コンパイル
- **根拠**: LuaJIT / TraceMonkey / PyPy。ループ中心のワークロードでインタプリタ比100x高速化の実績
- **難易度**: Very Hard

#### FR-245: Basic Block Versioning (基本ブロックバージョニング)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 型に基づくコード特殊化なし。FR-232（Uncommon Trap）はガードベースの投機だがブロック複製による多型対応なし
- **内容**: 基本ブロックの型コンテキスト（各変数の推論型の組み合わせ）毎に特殊化コピーを生成。`(if (fixnump x) (+ x 1) ...)` の真分岐ではx:fixnum版のブロックを、偽分岐では汎用版を使用。型コンテキストの爆発をN個（例: 4）に制限
- **根拠**: Chevalier-Boisvert & Feeley (2015)。ガードベース投機の代替として、静的コンパイルでも多型対応が可能
- **難易度**: Hard

---

### Phase 57 — プロファイル高度化（未実装）

#### FR-261: Value Profiling (値プロファイリング)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: FR-058（Type Feedback PGO）は型頻度カウンタのみ。実際の値（定数、範囲）の収集なし。FR-224（Sampling Profiler）はPC位置のみ
- **内容**: IC hit時に型だけでなく実際の値のヒストグラムを記録。頻出定数→定数畳み込み、値範囲→範囲解析（FR-038）にフィードバック。テーブルサイズ上限付きTop-K値記録
- **根拠**: V8 value profiling / HotSpot -XX:+ProfileReturnOnly。型プロファイリングの精密化
- **難易度**: Medium

#### FR-262: Call-Chain Profiling (コンテキスト感応プロファイリング)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/cli/src/main.lisp`
- **現状**: FR-224（VM Sampling Profiler）はフラット（PC位置のみ）。呼び出し元コンテキストを区別しない
- **内容**: サンプリング時にコールスタック上位N段（デフォルト8）を記録。呼び出しコンテキスト毎の型分布・実行頻度を収集。context-sensitive inlining（FR-053）の判定精度向上
- **根拠**: Google AutoFDO / BOLT / HotSpot -XX:+CallChainProfiling。コンテキスト無視のフラットプロファイルでは見えない最適化機会を検出
- **難易度**: Medium

#### FR-263: Allocation Site Profiling (割り当てサイトプロファイリング)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`, `packages/cli/src/main.lisp`
- **現状**: `heap.lisp`にアロケーションサイトのトラッキングなし。どの関数/行がメモリ消費の主因か不明
- **内容**: `rt-gc-alloc`にソース位置メタデータを付与。`./cl-cc run --alloc-profile`でアロケーションサイト毎のバイト数・回数・型を出力。GCチューニング・エスケープ解析（FR-007）の優先度決定に使用
- **根拠**: Go runtime pprof alloc / Java Flight Recorder allocation profiling。メモリ最適化の起点
- **難易度**: Medium

---

### Phase 58 — 投機的JIT高度化（未実装）

#### FR-267: ThinLTO (モジュール並行リンク時最適化)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: FR-102（LTO Whole-Program Call Graph）は全モジュールを逐次処理するフル LTO のみ想定。モジュール数が増えるとコンパイル時間が O(N²) 増大
- **内容**: LLVM ThinLTO（2015）に相当する 2 フェーズ並行 LTO。**Phase 1**: 全モジュールから要約（関数シグネチャ・エクスポート一覧・型サマリ）を並行抽出してグローバルサマリを構築。**Phase 2**: 各モジュールをサマリのみ参照しながら並行最適化（フル IR 読み込み不要）。呼び出しグラフのエッジを超えたインライン展開はサマリベースで対象を絞り込む。フル LTO の 1/10〜1/4 のコンパイル時間で 70〜90% の効果
- **根拠**: LLVM ThinLTO paper (2016) / Swift コンパイラの標準 LTO モード。大規模プロジェクトでのフル LTO の現実的代替
- **難易度**: Hard

#### FR-264: Background / Concurrent JIT Compilation (バックグラウンドJITコンパイル)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: JITコンパイル（FR-058/FR-244）はインタプリタ実行をブロックして行う。コンパイル中はVMが停止
- **内容**: JITコンパイルをバックグラウンドスレッドで実行しつつ、インタプリタ（Tier-0）が継続して動作する並行コンパイル。コンパイル完了後にコードポインタをアトミック置換（`vm-call` のディスパッチテーブルエントリを CAS で更新）。コンパイル中の関数変更（再定義）に対するキャンセル機構も必要
- **根拠**: V8 の並行コンパイラスレッド / HotSpot `-XX:+TieredCompilation` のC1/C2並行化 / JavaScriptCore DFG concurrent compilation。スループットを犠牲にしない JIT
- **難易度**: Very Hard

#### FR-265: Class Hierarchy Analysis for CLOS (CHA — クラス階層解析)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: `vm-generic-call` は常に実行時ディスパッチ（MOP `find-method` 相当）。メソッドが1つしか存在しない場合でも多態ディスパッチを経由する
- **内容**: コンパイル時にクラス階層グラフを構築し、`(defgeneric f)` に対して「クラス C のサブクラスが存在しない」「メソッド特化が1つだけ」の条件を静的に検証。条件成立時は `vm-generic-call` を直接呼び出し（`vm-call`）に降格。`sealed` クラスや `final` メソッドの宣言で CHA 精度向上。クラス追加時はCHA結果を無効化して再解析
- **根拠**: Java CHA (Dean et al. 1995) / GraalVM CLOS 最適化 / SBCL `(declare (optimize (safety 0)))` 下のディスパッチ除去。CLOSの主要な性能ボトルネック解消
- **難易度**: Hard

#### FR-266: Speculative Inlining with Type Guard (型ガード付き投機的インライン展開)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: インライン展開（FR-040）は静的に確定した callee のみ対象。`vm-generic-call` や高階関数はインライン不可
- **内容**: IC 型プロファイル（FR-058/FR-261）から最頻出 callee クロージャを特定し、「このオブジェクトが型 T なら callee = F である」という型ガードを挿入してインライン展開。ガード失敗時は uncommon trap（FR-159）経由でインタプリタへのデオプティマイズ。ガードが頻繁に失敗する場合は megemorphic（多態）パスへ降格
- **根拠**: V8 TurboFan speculative inlining / HotSpot `-XX:+InlineVirtualCalls`。`(mapcar fn list)` で fn が毎回同じクロージャなら完全インライン可能
- **難易度**: Hard

---

### Phase 60 — デオプティマイゼーション・OSR（未実装）

#### FR-280: Structured Deoptimization Framework (構造化デオプティマイゼーション)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/vm-run.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: FR-232（Uncommon Trap）・FR-266（型ガード）が脱出先を参照しているが、JITコード→VMインタプリタへのバイルアウト（レジスタ→VMレジスタへの逆マッピング）の実体がない
- **内容**: deopt時に現在の機械語レジスタ状態を `vm-state` に再構成する **materializer**。各deoptポイントに `deopt-frame`（対応するVM PCとレジスタマッピング表）を紐づける。eager deopt（即時バイルアウト）と lazy deopt（次のサーフェスでバイルアウト）の両方をサポート。`./cl-cc run --deopt-trace` で脱出頻度ログ出力
- **根拠**: V8 deoptimizer / HotSpot nmethod deopt / GraalVM FrameState。投機的最適化の「保険」機能の実体。FR-231（Stack Map）が前提
- **難易度**: Very Hard

#### FR-281: On-Stack Replacement — Interpreter→JIT (OSR昇格)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: JITコンパイルされた関数への切り替えは関数エントリ時のみ。長時間実行中のホットループはインタプリタで動き続ける
- **内容**: ループバックエッジのバックエッジカウンタが閾値を超えたとき、ループ途中で現在のVMフレームをJITフレームに**オンスタック置換**。OSR入口ブロック（ループヘッダに対応するJIT BBへの直接ジャンプ）を生成。VM変数→JITレジスタへの変数マッピングをOSR入口点で確立
- **根拠**: HotSpot OSR / V8 OSR from Ignition / PyPy OSR。バッチ処理・数値計算系ループで最大の恩恵
- **難易度**: Very Hard

#### FR-282: OSR Exit — JIT→Interpreter (OSR降格)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: FR-281はインタプリタ→JITの昇格のみ。型仮定が崩れた場合にループ途中でインタプリタに戻る経路がない
- **内容**: ループ内のガード失敗時にJITフレームをVMフレームに逆変換してインタプリタに再入する OSR exit。deopt frameに「どのループ反復まで完了したか」の情報を保持。FR-280（Deopt Framework）の`materializer`をOSR文脈で呼び出す
- **根拠**: V8 deopt inside loop / HotSpot uncommon_trap in compiled loop。ループ内の型仮定を安全に保護
- **難易度**: Hard

#### FR-283: Speculation Log (投機ログ)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: 型ガード（FR-232）の失敗記録なし。同一ガードが毎回コンパイル時に同じ投機を行い、実行時に何度でも失敗し続ける可能性がある
- **内容**: `*speculation-log*` — call-site IDをキーとして「この投機は過去に失敗した」フラグを保持するグローバルテーブル。再コンパイル時にログを参照し、失敗実績のある投機は採用しない。ログはプロセス間で `.prof` ファイルに永続化可能
- **根拠**: GraalVM SpeculationLog / HotSpot replay compile。同一の有害な投機を繰り返すコンパイル・デコンパイルループ（deopt storm）を防ぐ
- **難易度**: Medium

---

### Phase 61 — オブジェクト形状・型特殊化（未実装）

#### FR-284: Object Shape / Hidden Class Tracking (オブジェクト形状追跡)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: CLOSインスタンスはスロットを `(gethash :slot-name ht)` で参照するため、スロットオフセットが実行時に確定する。インライン化もフィールドアクセスの定数化も不可能
- **内容**: クラス定義時に `shape-id`（単調増加整数）を各クラスに付与し、`shape-id → slot-offset-table` の配列ルックアップでスロットアクセスを O(1) に。インスタンスにshape-idを埋め込み、アクセスコード生成時に `(guard-shape inst shape-id) (vm-slot-load inst offset)` を発行。`defclass` 再評価・スロット追加で shape-id が変わりガードが外れる
- **根拠**: V8 Hidden Classes / JSC Structures / LuaJIT table shape。ハッシュテーブルアクセスから定数オフセットアクセスへの変換でCLOS性能10〜50x向上
- **難易度**: Hard

#### FR-285: Unboxed Fixnum / Float Arithmetic (アンボックス演算)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/primitives.lisp`
- **現状**: 演算命令（`vm-add` 等）は実行時タグチェック→ボックス値unwrap→演算→ボックス化という手順。数値ループでボックス化コストが大きい
- **内容**: 型推論（packages/foundation/type/src/inference.lisp）で fixnum と判定された変数は unboxed レジスタ（`reg-fixnum` タグ付きMIR値）で管理。`vm-add-fixnum`/`vm-add-float` の特殊化命令を生成し、タグチェックとボックス化を省略。オーバーフロー時は `overflow-trap` でboxed演算にフォールバック
- **根拠**: SBCL type-driven code generation / V8 Maglev unboxed Int32 / GraalVM primitive specialization。数値演算ループで2〜5x高速化
- **難易度**: Hard

#### FR-286: Tagged Integer Range Analysis (タグ付き整数範囲解析)

- **対象**: `packages/foundation/type/src/inference.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 型システム（packages/foundation/type/src/）は fixnum/float/cons の区別はするが、整数の値範囲（0≤n<256等）を追跡しない
- **内容**: SSA値に整数範囲アノテーション `[lo, hi]` を付与し、CFG上で前向き伝播。`(the (integer 0 255) x)` → `x ∈ [0,255]`。範囲が fixnum に収まると証明できればオーバーフローガード不要。ループインダクション変数の範囲をループ境界から導出（FR-038と統合）
- **根拠**: LLVM ScalarEvolution / V8 TurboFan range analysis / HotSpot C2 range check elimination。配列境界チェック除去の前提条件
- **難易度**: Medium

---

### Phase 62 — ループ最適化（未実装）

#### FR-287: Loop Invariant Code Motion (LICM — ループ不変コード移動)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/cfg.lisp`
- **現状**: 最適化パスはフラット命令列に対して動作。CFGループ構造を利用したループ外ホイスティングなし
- **内容**: ループ検出（back-edge + dominator tree）→ループ不変命令の検出（全オペランドがループ外で定義）→プリヘッダブロックへのホイスト。副作用のある命令（ヒープ書き込み・IO）はホイスト禁止。エイリアス解析（FR-030）と連携して副作用を精密に判定
- **根拠**: 全主要コンパイラの基本最適化。`(loop for i from 0 to n sum (length fixed-list))` で `length`計算をループ外に移動
- **難易度**: Medium

#### FR-288: Loop Unrolling (ループ展開)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: ループ展開なし。小ループでもバックエッジジャンプのオーバーヘッドが残る
- **内容**: 反復回数が静的に判明するループ（`dotimes` + 定数）を N 倍展開してバックエッジ削減。反復回数不明でも `unroll-factor=4` でピーリング（残余処理）を追加した部分展開。展開後に冗長コピー命令をDCEで除去
- **根拠**: GCC -funroll-loops / LLVM LoopUnroll / SBCL。ループ本体が小さいときレジスタ使用率とILP改善
- **難易度**: Medium

#### FR-289: Loop Fusion (ループ融合)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 隣接するループ（`(dotimes (i n) ...)` が2つ連続）は独立に実行。キャッシュ効率が悪い
- **内容**: 同一反復範囲・副作用が独立な隣接ループを1つのループにマージ。依存チェック（読み書きエイリアス解析）を経てバリアフリーなループのみ融合。メモリアクセスのlocality向上でキャッシュミス削減
- **根拠**: GCC -floop-interchange / Polly / LLVM LoopFusion。メモリ帯域律速なワークロードで1.5〜3x高速化
- **難易度**: Hard

#### FR-290: Loop Peeling (ループピーリング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: ループ先頭に条件チェックがある場合、毎反復でチェックを実行
- **内容**: ループ本体の最初の1〜2回分を「ピーリング」（ループ外に複製）。ループ先頭の境界チェック・NULL チェック・型チェックが定数畳み込みで除去できる場合に適用。残余ループは無条件で実行可能に
- **根拠**: LLVM LoopPeel / HotSpot C2。`(car (first list))` のnullチェックをピーリングで除去
- **難易度**: Medium

#### FR-291: Auto-Vectorization / SLP Vectorizer (SLPベクトル化)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **現状**: SIMD命令生成なし。数値ループは1要素ずつスカラー処理
- **内容**: **SLP（Superword Level Parallelism）ベクトル化**: 隣接メモリアクセス＋同種演算のスカラー命令群を SSE2/AVX2/NEON 128〜256bit SIMD 命令に置換。`(loop for i below n do (setf (aref out i) (+ (aref a i) (aref b i))))` → `VADDPS ymm0, ymm1, ymm2`。型情報（floatベクトル・fixnum配列）が必要。アライメントチェック付き
- **根拠**: LLVM SLP Vectorizer / GCC auto-vectorization / ARM NEON。数値計算で4〜16x高速化。Common Lisp の `simple-array` 操作が主なターゲット
- **難易度**: Very Hard

---

### Phase 63 — コードレイアウト最適化（一部実装: hot-cold layout）

#### FR-292: Hot/Cold Code Splitting (ホット/コールドコード分離)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: 関数内の全基本ブロックが隣接配置。頻繁に実行されないエラー処理パスがホットパスのI$を汚染
- **内容**: プロファイルデータ（FR-104）で実行頻度の低い基本ブロック（エラーハンドラ・rare-branch）を関数末尾または別セクションに移動。ホットパスのブロックを直線的に配置してI$ミス削減。`unlikely` アノテーション（Common LispのCMU CLスタイル）で静的ヒントも受け付ける
- **根拠**: HotSpot `-XX:+ProfileCompiledMethods` / LLVM `-fprofile-use` cold section / V8 hot/cold block layout。I$利用率15〜30%改善
- **難易度**: Medium

#### FR-293: Profile-Guided Code Layout (プロファイル誘導コードレイアウト)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: 関数の配置順はASDFロード順。呼び出し関係に基づく局所性最適化なし
- **内容**: 実行プロファイルから関数呼び出しグラフ（コールグラフ）上のエッジ重みを収集し、頻繁に連続呼び出される関数群を隣接配置。Mach-Oの`__TEXT,__text` セクション内で関数の物理的順序を最適化。`clang -forder-file` / Facebook BOLTに相当するポストリンク最適化
- **根拠**: Google AutoFDO / Meta BOLT / Apple PGO order file。大規模プログラムでのI$ミス20〜40%削減
- **難易度**: Hard

#### FR-294: Function Outlining (関数アウトライン化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: インライン展開（FR-040）の逆操作未実装。コードサイズが増大しI$を圧迫
- **内容**: 複数箇所に重複する共通コード（エラー処理・型チェック列）を切り出して共有関数化（インライン展開の逆）。コードサイズ閾値（デフォルト32バイト）以上の重複コードを検出し `_outlined_func_N` として分離。`-Os` 相当のコードサイズ最適化モード
- **根拠**: LLVM MachineOutliner / Apple LLVM outliner (iOS バイナリサイズ削減)。I$フットプリント削減で間接的に性能改善
- **難易度**: Medium

---

### Phase 64 — 命令スケジューリング・レジスタ圧力（未実装）

#### FR-295: Instruction Scheduling (命令スケジューリング)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **現状**: 命令はMIRの順序通りに発行。レイテンシ隠蔽・アウトオブオーダー活用なし
- **内容**: 依存グラフ（Def-Use チェーン）を構築し、クリティカルパスを短縮するようにリスト型スケジューラで命令を並び替え。x86-64: Alder Lake / Zen 4 のレイテンシテーブルを参照。ロードレイテンシ隠蔽（load→use の間に無関係命令を挿入）が主目的
- **根拠**: GCC `-fschedule-insns` / LLVM MachineScheduler / SBCL vop scheduling。スーパースカラーCPUでの IPC 向上
- **難易度**: Hard

#### FR-296: Register Pressure Reduction via Rematerialization (再実体化によるレジスタ圧力削減)

- **対象**: `packages/backend/emit/src/regalloc.lisp`
- **現状**: レジスタ枯渇時はすべてスタックにスピル（load/store）。再計算コストの低い値もスピル対象
- **内容**: スピル候補が「定数」「単純な算術式」「pure関数呼び出し（副作用なし）」のとき、レジスタに再ロードする代わりに命令を再発行（rematerialize）。スピル/リロードのメモリ帯域使用を削減。コスト関数：rematerialize cost < load + store cost の場合に採用
- **根拠**: LLVM Rematerialization / GCC REG_DEAD / Briggs et al. (1994)。スピル回数の10〜30%削減
- **難易度**: Medium

---

### Phase 65 — ML誘導最適化（未実装）

#### FR-297: ML-Guided Inlining (機械学習誘導インライン展開)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: インライン化判定はヒューリスティック（コード行数・呼び出し深度・FR-105プロファイル閾値）。最適な閾値の設定はブラックアート
- **内容**: 関数ペア（呼び出し元・callee）の特徴量ベクトル（命令数、ループ深度、型特殊化度、呼び出し頻度、引数パターン）を入力に、インライン化の利益を予測する小規模 MLP モデル（隠れ層256次元、パラメータ数〜50K）。推論は `./cl-cc compile` 実行中に数μsで完了。モデルは cl-cc 自身の selfhost プロファイルで事前学習
- **根拠**: Google MLGO (2021) / Meta Inliner ML / ARM NN-guided compiler。ヒューリスティックより10〜15%コードサイズ削減＋性能向上。2024〜2026年のLLVM/GCC本流に統合済み
- **難易度**: Very Hard

#### FR-298: Feedback-Directed Optimization via Corpus PGO (コーパスPGO)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **現状**: PGO（FR-104/FR-105）はユーザー提供のプロファイルデータに依存。代表的な入力セットがない場合は効果なし
- **内容**: cl-cc 自身の selfhost 実行（84ファイル）をコーパスとして自動プロファイル収集→最適化の **bootstrap PGO**。`make pgo-build`: (1) instrumented binary でselfhost実行してプロファイル生成、(2) プロファイルを使ってrelease build。CI に統合して毎ビルドでプロファイルを更新
- **根拠**: Clang `-fprofile-generate` → `-fprofile-use` / GCC `-fprofile-generate` ワークフロー。Rustcも同様のbootstrap PGOを採用（2022〜）。通常5〜20%のコンパイル時間短縮
- **難易度**: Medium

---

### Phase 66 — セキュリティ・サンドボックス（未実装）

#### FR-299: Spectre/Meltdown Mitigations in JIT (JITコードのSpectre対策)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: JIT生成コードにSpectre v1（境界チェックバイパス）対策なし。JIT-to-JIT の間接ジャンプにretpoline未適用
- **内容**: (1) 配列境界チェックの前に `LFENCE` を挿入して投機的実行をバリア。(2) 間接呼び出し（`vm-generic-call`、クロージャディスパッチ）を retpoline シーケンス（`call thunk; jmp` パターン）に置換。(3) JITコード領域を W^X（書き込みと実行の排他）で管理 — コード生成時はmprotect RW、実行時はRX
- **根拠**: V8 Spectre mitigations (2018) / Firefox SpiderMonkey retpoline / Chrome site isolation。サンドボックス実行環境（Wasm実行等）での必須要件
- **難易度**: Medium

#### FR-300: JIT Code Region Isolation (JITコード領域隔離)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/runtime/src/runtime.lisp`
- **現状**: JIT生成コードのメモリ管理（mmap/mprotect）なし。コードバッファの書き込み可能性が実行中も残る
- **内容**: JITコード専用のメモリプール（`jit-code-arena`）を `mmap(PROT_READ|PROT_EXEC)` で確保。コード書き込み時のみ `mprotect` で一時的に RW に変更し、書き込み完了後即 RX に戻す。`icache_invalidate` （Apple Silicon: `sys_icache_invalidate` / Linux: `__builtin___clear_cache`）を忘れずに呼び出す。コードキャッシュ上限設定とLRU退避
- **根拠**: V8 CodePageAllocator / JavaScriptCore JITWriteSeparation / Apple PAC signed JIT。W^X の実施はmacOS 14+ / iOS では必須
- **難易度**: Hard

---

### Phase 67 — WebAssembly JIT（未実装）

#### FR-301: Tiered Wasm Compilation (段階的Wasmコンパイル)

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: Wasm バックエンドは1段階のAOTコンパイルのみ（FR-080）
- **内容**: **Tier-0**: Wasm バイトコードを直接インタプリタ実行（低レイテンシ起動）。**Tier-1**: ホット関数を Cranelift / LLVM-MC ベースの Baseline JIT でコンパイル（最適化なし、1ms以下）。**Tier-2**: 実行頻度上位5%をオプティマイジングJIT（FR-105ベースPGO適用）でコンパイル。WasmGC（reference types, structs, arrays）に対応したGC統合
- **根拠**: V8 Liftoff→TurboFan / Firefox Baseline→Ion / Wasmtime Cranelift。Wasm 起動時間をAOTコンパイル待ちなしで提供
- **難易度**: Very Hard

#### FR-302: Wasm SIMD Code Generation (Wasm SIMD命令生成)

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/foundation/mir/src/mir.lisp`
- **現状**: Wasm バックエンドはスカラー命令のみ。Wasm SIMD 128 仕様（2022年標準化）未対応
- **内容**: MIR のベクトル値型（FR-291で追加するvec128）をWasm `v128.load` / `f32x4.add` 等の SIMD 命令にマップ。x86-64はSSE2経由、AArch64はNEON経由でWasm runtimeがSIMDを実行。ホストが SIMD 非対応の場合はスカラーフォールバック
- **根拠**: Wasm SIMD proposal (Phase 4, 2022) / Emscripten SIMD / V8 Wasm SIMD。数値ワークロードで4〜8x高速化
- **難易度**: Hard

---

### Phase 68 — ガード精錬・コードキャッシュ管理・適応的再コンパイル（未実装）

#### FR-303: Guard Strength Reduction (ガード弱体化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: FR-232（Uncommon Trap）・FR-266（型ガード付きインライン）が生成するガードはすべて同コスト（型チェック命令）。失敗率が極低くても high-cost なガードのまま
- **内容**: Speculation Log（FR-283）の失敗カウンタを参照し、ガードが N 回実行されて一度も失敗していない場合により安価な形式に降格。降格の段階: `full-type-check` → `tag-bit-test`（タグビット1命令）→ `shape-id-compare`（FR-284, 整数比較1命令）→ `nop`（完全除去、クラス階層変化時に再挿入）。ガード除去後のクラス変更・メソッド再定義時は IC 無効化（FR-265 と同メカニズム）でガードを復元
- **根拠**: V8 `--stress-compaction` guard weakening / GraalVM SpeculationLog confidence score。ホットループ内の型ガードを nop まで落とせれば数%の命令数削減
- **難易度**: Medium

#### FR-304: JIT Code Cache Eviction (JITコードキャッシュ退避)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: FR-300（JIT Code Region Isolation）でコードアリーナを確保するが、上限到達時の退避ポリシー未定義。コードキャッシュが満杯になると新規コンパイルが失敗する
- **内容**: JITコードエントリに **warmth counter**（呼び出し回数の指数平滑移動平均）を付与。キャッシュ使用率が閾値（デフォルト80%）を超えたとき、warmthが最低のエントリからLRU退避。退避対象のコードポインタを `vm-call` ディスパッチテーブルから Interpreter stub に差し戻し（CAS）。退避後に再度ホットになれば再コンパイル。`./cl-cc run --jit-cache-stats` でヒット率・退避回数を出力
- **根拠**: V8 code flushing / HotSpot `-XX:ReservedCodeCacheSize` + code cache sweeper / JavaScriptCore JIT memory pressure eviction。長時間稼働プロセスでのメモリリーク防止
- **難易度**: Medium

#### FR-305: Adaptive Recompilation Thresholds (適応的再コンパイル閾値)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: Tier昇格の閾値（バックエッジカウンタ・呼び出し回数）はコンパイル時定数。コールドスタート時も安定稼働時も同じ閾値を使用するため、起動直後に必要以上にコンパイルが走るか、逆にホット関数の昇格が遅延する
- **内容**: プロセス起動からの経過時間・総コンパイル時間・CPU 使用率をモニタし、Tier昇格閾値を動的に調整。**warm-up フェーズ**（起動後30秒）: 閾値を通常の1/3に下げて積極コンパイル。**安定フェーズ**: コードキャッシュ使用率が60%超なら閾値を2倍に引き上げてコンパイル抑制。**メモリ逼迫時**: Tier-2への昇格を停止しTier-1のみ稼働。閾値変更は `*jit-compilation-budget*` パラメータ経由で外部からも制御可能
- **根拠**: HotSpot `-XX:CompileThreshold` 動的調整 / V8 compilation budget / GraalVM adaptive compilation policy。起動レイテンシとスループットのトレードオフを実行時に自動調整
- **難易度**: Medium
