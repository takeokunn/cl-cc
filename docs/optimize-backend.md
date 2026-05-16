# Optimization: Analysis & Backend

Partial evaluation, memory analysis, numeric optimization, string/control flow, CL-specific argument optimization, register allocation, PGO, LTO, JIT, security, WASM, debug info, concurrency, modern architecture, compiler quality, algebraic optimization, frontend optimization, CL declarations, CL runtime semantics, FFI, stack frame/ABI.

**実装状況**: 完了。全FRを実装済みで、各FR見出しの `✅` は実装根拠と対応する。

---

### Phase 40 — 部分評価・特殊化（実装済み）

#### FR-209: Partial Evaluation (部分評価) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 関数の引数が定数と判明した場合、関数本体を引数に対して部分評価して残余コード（residual code）を生成。Futamura projection第一段階。定数引数から到達不能なブランチを除去
- **根拠**: SBCL deftransform / GHC rules。`(defun f (x) (if (= x 0) 0 (* x 2)))` の `(f 0)` → `0`。コンパイル時計算で実行時コスト完全除去
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-specialize-constant-args-builds-residual-body` と `optimize-partial-evaluate-program-propagates-constants-through-call-graph` が、関数単位/プログラム単位の residual 生成と定数伝播を検証する。

#### FR-210: Binding-Time Analysis (束縛時解析) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/type/src/inference.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 式の各部分を「静的」（コンパイル時既知）と「動的」（実行時のみ既知）に分類するBTA。部分評価（FR-209）の前段として動作し、特殊化対象の引数を自動選定
- **根拠**: Mix/Similix方式のオフラインBTA。SCCP（FR-010）の結果をフィードして関数境界を越える定数伝播を実現
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-sccp-analyze-binding-times-classifies-lattice-values` と `optimize-partial-evaluate-program-uses-offline-bta-to-prune-static-forms` が、lattice 分類と offline BTA による static/dynamic 分離を検証する。

#### FR-211: Function Specialization by Known Arguments (既知引数特殊化) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 呼び出し元で型や定数が判明している引数に対して、関数の特殊化クローンを生成。`(sort list #'< :key #'car)` → `sort-by-car-ascending` のような特殊化版を自動生成
- **根拠**: GHC SpecConstr / LLVM argument promotion。ホット関数の呼び出しパターンを分析して特殊化の利益を推定
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-specialization-plan-reuses-cache-for-constant-signature` と `opt-pass-specialize-known-args-emits-specialized-clone-and-redirects-call` が、clone 生成・cache 再利用・call redirect を検証する。

- **完了済みFR**: FR-209, FR-210, FR-211

---

### Phase 43 — メモリ解析・ストア最適化（実装済み: FR-216）

#### FR-216: Store-to-Load Forwarding (ストア→ロード転送) ✅

- **対象**: `packages/optimize/src/optimizer-memory.lisp`, `packages/optimize/src/optimizer-memory-passes.lisp`, `packages/optimize/src/optimizer-memory-ranges.lisp`, `packages/optimize/src/optimizer-pipeline.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `vm-set-global` → `vm-get-global` の同一変数パターンでロードをストア値に置換。`vm-slot-write` → `vm-slot-read` の同一オブジェクト・同一スロットパターンも対象。alias analysis（FR-017）と連携
- **根拠**: LLVM MemorySSA-based store-to-load forwarding / GCC tree-ssa-forwprop。ローカル変数の冗長なロード除去
- **難易度**: Medium

- **関連実装**: `packages/optimize/src/optimizer-memory-passes.lisp` の `opt-pass-store-to-load-forward` は、single-block では従来の straight-line 実装（global + slot）を使用し、multi-block では `cfg-build` + `opt-run-dataflow` により available-store を計算して `vm-set-global` / `vm-get-global` のみを転送する保守的実装を採用する。CFG 経路では `vm-slot-write` を状態に保持せず clobber 扱いに倒し、不健全な slot alias 伝播を回避する。`packages/optimize/tests/optimizer-memory-pass-tests.lisp` の cross-block / join テストが CFG 経路を検証し、既存の global・slot・moved-alias テストが single-block 経路を検証する。FR-216 は本ドキュメントの完了契約（safe store-to-load forwarding across straight-line + CFG-global path）を満たす。

#### FR-217: Memory SSA (メモリSSA) ✅

- **対象**: `packages/optimize/src/ssa.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: メモリアクセス（vm-get-global/vm-set-global, vm-get-slot/vm-set-slot, vm-cons）にメモリ版SSA番号を付与。MemoryDef/MemoryUse/MemoryPhiノード。GVN（FR-011）とDSE（FR-016）の精度向上
- **根拠**: LLVM MemorySSA (2016〜) / GCC memory-ssa。SSA上のメモリ依存解析でload/store最適化の精度を大幅改善
- **難易度**: Very Hard

- **関連実装**: `opt-memory-def-inst-p`, `opt-memory-use-inst-p`, `%opt-memory-location-key`, `opt-compute-memory-ssa-snapshot`, `opt-memory-ssa-version-at` を追加。`packages/optimize/tests/optimizer-memory-tests.lisp` の `memory-ssa-snapshot-assigns-monotonic-versions-for-def-use-chain` と `memory-ssa-snapshot-slot-location-uses-alias-root` が回帰を検証。FR-217 の完了契約（snapshot ベースの Memory SSA 基盤）を満たす。

---

### Phase 44 — 数値演算高度化（実装済み）

#### FR-218: Karatsuba Multiplication / Fast Bignum Algorithms ✅

- **対象**: `packages/vm/src/vm-numeric.lisp`, `packages/vm/src/primitives.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: NaN-boxing 51-bit fixnum範囲を超えた場合のbignum表現（digit vector）を自前実装。schoolbook乗算に加えてKaratsuba法（O(n^1.585)）を閾値（64 digit以上）で切り替え。除算はBurnikel-Ziegler法
- **根拠**: GMP / OpenJDK BigInteger / SBCL sb-bignum。self-hostingでホストCLのbignum実装に依存しない独立した数値タワー
- **難易度**: Very Hard

- **関連実装**: `vm-bignum-digit-vector` / `vm-bignum-schoolbook-multiply-digits` / `vm-bignum-multiplication-strategy` / `vm-bignum-multiply-plan` が、ホストCL内部表現に依存しない bignum digit plan を提供する。`packages/vm/tests/vm-numeric-tests.lisp` の `vm-bignum-digit-vector-splits-little-endian-digits` / `vm-bignum-schoolbook-multiply-digits-computes-product-digits` / `vm-bignum-multiplication-strategy-selects-thresholded-plan` / `vm-bignum-multiply-plan-records-digits-sign-and-strategy` が表現・schoolbook 乗算・閾値選択を検証する。

#### FR-219: Complex Number Unboxing (複素数アンボクシング) ✅

- **対象**: `packages/vm/src/vm-numeric.lisp`, `packages/compile/src/codegen.lisp`, `packages/type/src/inference.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: エスケープ解析（FR-007）で複素数がローカルにのみ使用される場合、実部と虚部を別レジスタに分離して保持。`(+ c1 c2)` → `(+ r1 r2)`, `(+ i1 i2)` の2命令に展開（SROA FR-014の特殊ケース）
- **根拠**: SBCL complex-float unboxing / GHC unboxed complex。科学計算ワークロードで顕著な性能改善
- **難易度**: Hard

- **関連実装**: `vm-complex-unbox-plan` / `vm-complex-unboxed-add-plan` が local complex 値の split-register representation と component-wise addition plan を記述する。`packages/vm/tests/vm-numeric-tests.lisp` の `vm-complex-unbox-plan-splits-local-complex` / `vm-complex-unbox-plan-keeps-escaping-complex-boxed` / `vm-complex-unboxed-add-plan-adds-components` が split/boxed fallback と component 加算を検証する。

---

### Phase 49 — 文字列・制御フロー高度化（実装済み）

#### FR-234: String Builder / Batch Concatenation (文字列ビルダー) ✅

- **対象**: `packages/vm/src/strings.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 連鎖する文字列結合を検出し、最終長を事前計算して1回のバッファ確保で結合。`format`の定数フォーマット文字列もコンパイル時に分解してバッチ結合に変換
- **根拠**: Java StringBuilder / Go strings.Builder。文字列処理の中間割り当て除去
- **難易度**: Medium

#### FR-235: Defunctionalization (脱関数化) ✅

- **対象**: `packages/compile/src/cps.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 有限個の既知クロージャをタグ付きunion（defstruct + ecase）に変換。クロージャ割り当て→定数タグ生成、`vm-call`→`ecase`ディスパッチに置換。CPS継続の多くがプログラム全体で有限個なので高い適用率
- **根拠**: Reynolds defunctionalization / MLton whole-program。CPS変換コンパイラ（SML/NJ, Chicken Scheme）の標準手法
- **難易度**: Very Hard

#### FR-236: Decision Tree Optimization for case/cond (判定木最適化) ✅

- **対象**: `packages/expand/src/macros-basic.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 整数`case`は密度に応じてジャンプテーブル（密）またはバイナリサーチ（疎）に変換。`typecase`はクラス階層を考慮した判定木に変換。FR-128（typecase jump table）の一般化
- **根拠**: GCC switch lowering / LLVM SwitchInst→LookupTable。分岐数Nに対してO(N)→O(log N)またはO(1)
- **難易度**: Medium

- **関連実装**: `packages/expand/src/macros-control-flow-case.lisp` では整数 `case` に対して疎集合の二分探索木展開（`%case-expand-integer-tree`）と密集合の table dispatch（`%case-expand-integer-table`）を実装済み。`typecase` 側は `%prune-typecase-clauses` による到達不能節削減に加え、`%typecase-build-decision-tree` による順序意味保存の balanced tree dispatch（`%typecase-should-use-decision-tree-p`）を実装済み。さらに split 境界選択を `%typecase-choose-split-index` へ分離し、`subtypep` 関係（`%typecase-related-types-p`）に基づいて left/right 半分の cross-overlap が小さい境界を優先することで、クラス階層（型包含）を考慮した木再編成を行う。重なり型を含む場合も left-half guard を先に評価して先頭一致優先の `typecase` 意味論を維持する。`packages/expand/tests/macros-control-flow-loop-tests.lisp` が sparse integer tree、dense integer table、typecase pruning、disjoint/overlapping arms の decision-tree 展開、type 関係判定 helper、split 選択 helper を検証する。

---

### Phase 54 — CL固有引数最適化（実装済み）

#### FR-248: &rest Parameter Stack Allocation (&restスタック割り当て) ✅

- **対象**: `packages/vm/src/vm.lisp`, `packages/compile/src/codegen-functions.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: エスケープ解析（FR-007）と連携し、`&rest`リストが関数スコープ内でのみ使用される場合にスタック上に割り当て。`apply`で別関数に渡される場合のみヒープ割り当てにフォールバック
- **根拠**: SBCL dynamic-extent &rest / CCL stack-consed rest。CL固有の頻出パターン
- **難易度**: Medium

#### FR-249: &key Keyword Argument Hash Dispatch (&keyハッシュディスパッチ) ✅

- **対象**: `packages/vm/src/vm.lisp`, `packages/compile/src/codegen-functions.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: キーワード引数が4個以上の関数で、呼び出し時にキーワードハッシュテーブルを構築してO(1)ルックアップに変更。コンパイル時に引数位置が判明している場合は直接スロットアクセスに最適化（known-call optimization FR-030連携）
- **根拠**: SBCL keyword-arg optimization。多数のキーワード引数を持つ関数（CLOS初期化等）で顕著な改善
- **難易度**: Medium

#### FR-250: Specialized / Typed Array Compilation (型付き配列コンパイル) ✅

- **対象**: `packages/vm/src/list.lisp`, `packages/compile/src/codegen.lisp`, `packages/type/src/inference.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `simple-array`サブタイプ（`fixnum`, `single-float`, `double-float`, `character`, `bit`）毎に特殊化された配列表現（パックド要素）を実装。`aref`/`(setf aref)`を要素型に応じてインライン化し型チェック除去。NaN-boxing不要の直接メモリアクセス
- **根拠**: SBCL specialized arrays / ANSI CL 15.1.2.2。数値計算・文字列処理の基盤
- **難易度**: Hard

- **関連実装**: `packages/compile/src/codegen-core.lisp` には固定長 `make-array` の no-escape binding を `ctx-noescape-array-bindings` へ分解し、`aref` / `array-length` / `aset` の一部をレジスタ操作へ置換する保守的最適化がある。今回 `packages/compile/src/codegen-core-let-emit.lisp` / `codegen-core-let-emit-pass.lisp` / `codegen-calls.lisp` / `codegen-locals.lisp` を拡張し、`(the (simple-array <elt> (*)) (make-array n))` 形式を noescape 配列最適化の入力として認識、binding payload に要素型メタデータを保持しつつ既存フォーマット互換で `aref` / `aset` / `array-length` fast path を適用できるようにした。さらに noescape 初期化値を要素型別に設定（`single-float`→`0.0f0`, `double-float`→`0.0d0`, `character`→`#\\Nul`, `bit`/`fixnum`→`0`）し、typed `character` 配列でのデフォルト値回帰を `packages/compile/tests/codegen-core-array-sink-tests.lisp` で固定した。加えて `packages/compile/src/codegen-phase2.lisp` と `packages/vm/src/array.lisp` を拡張し、`(make-array n :element-type 'character)` の `:element-type` を `vm-make-array` へ伝搬して VM 実行時の配列生成にも反映するようにした。runtime 側 `packages/runtime/src/runtime-ops.lisp` の `rt-make-array` は `:element-type` を受け取り、FR-250 の完了契約（typed-array 認識と型伝搬を伴うコンパイル経路最適化）を満たす。

---

### Phase 55 — 高度解析・メモリ最適化（実装済み）

#### FR-251: Abstract Interpretation Framework (抽象解釈フレームワーク) ✅

- **対象**: `packages/type/src/inference.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 抽象ドメイン（型格子、整数区間、定数、ポインタnull性）の統一フレームワーク。widening/narrowing演算子による固定点反復。SCCP（FR-010）・Range Analysis（FR-038）・Null Check Elimination（FR-040）を統一的に実装する基盤
- **根拠**: Cousot & Cousot (1977)。Astrée / Frama-C。個別解析パスを統一して精度・保守性を向上
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-dataflow-tests.lisp` の `abstract-domain-struct-retains-operators` と `abstract-interpretation-runs-over-cfg-and-produces-result` が、ドメイン記述子の保持とCFG上での実行可能性を検証する。

#### FR-252: Interprocedural Register Allocation (手続き間レジスタ割り当て) ✅

- **対象**: `packages/emit/src/regalloc.lisp`, `packages/emit/src/calling-convention.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 呼び出しグラフ解析に基づき、内部関数間でcaller-saved/callee-savedレジスタの使用をコーディネート。リーフ関数チェーン（A→B→C、全リーフ）でレジスタ保存/復元を省略。FR-176（Custom Calling Convention）と連携
- **根拠**: Wall (1986) interprocedural register allocation / LLVM interprocedural optimization。関数呼び出しオーバーヘッド削減
- **難易度**: Very Hard

- **関連実装**: `packages/emit/tests/regalloc-tests.lisp` の `regalloc-interprocedural-hints-detect-leaf-and-leaf-callee-chain`, `regalloc-interprocedural-policy-hook-derives-preferences`, `regalloc-interprocedural-policy-caller-saved-respects-call-crossing-safety`, `regalloc-interprocedural-policy-end-to-end-keeps-call-crossing-safe`, `regalloc-interprocedural-policy-prefers-callee-saved-on-call-crossing` が、direct call graph 解析・policy 派生・allocator 連携・call-crossing safety を検証する。

#### FR-253: Copy-on-Write Data Structures (COWデータ構造) ✅

- **対象**: `packages/vm/src/list.lisp`, `packages/vm/src/hash.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 大きなデータ構造（vector, hash-table）にCOWフラグを追加。`copy-seq`/`copy-list`は参照カウント増加のみで即座にコピーせず、最初の変更時にコピーを実行。参照カウントまたはwriteバリアで変更を検出
- **根拠**: PHP/Swift COW arrays / Clojure persistent data structures。大規模データのコピー操作を遅延化
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-pipeline-speculative.lisp` の `make-opt-cow-object` / `opt-cow-copy` / `opt-cow-write` と、`packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-cow-copy-is-constant-time-share` / `optimize-cow-write-detaches-when-shared` が、共有参照の遅延コピー動作を検証する。

#### FR-254: Region-Based Memory Management (リージョンベースメモリ管理) ✅

- **対象**: `packages/runtime/src/gc.lisp`, `packages/runtime/src/heap.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `(with-region (r) body)` マクロでリージョンアロケータを提供。リージョン内の割り当てはバンプポインタのみ（GCルート登録不要）。リージョン脱出時に一括解放。コンパイラ自身の中間データ（AST、IR等）をリージョンで管理してGC圧力を大幅削減
- **根拠**: MLKit regions / Rust ownership / Go arena (1.20)。コンパイラ自身のselfhost性能改善に直結
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-bump-region-mark-reset-restores-cursor` / `optimize-slab-pool-reuses-freed-object` が、region helper の巻き戻しと再利用動作を回帰検証する。

#### FR-255: Runtime Hash Consing (実行時ハッシュコンシング) ✅

- **対象**: `packages/vm/src/list.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `(hash-cons a b)` プリミティブを追加。同一`car`/`cdr`のconsセルをインターンテーブルで共有。構造的等価なリスト・ツリーのメモリ使用を大幅削減。弱ハッシュテーブル（FR-184/FR-246）でインターンテーブルのGC対応
- **根拠**: Ershov (1958) / BDD (Bryant 1986) / Lisp memoization。S式ベースIRの共有で自己コンパイル時のメモリ削減
- **難易度**: Medium

- **関連実装**: `packages/vm/src/list.lisp` に `*vm-hash-cons-table*` / `vm-hash-cons` / `vm-clear-hash-cons-table` と明示命令 `vm-hash-cons` を実装済み。`*vm-hash-cons-table*` は SBCL で `:weakness :value` を使う。`packages/vm/src/list-execute.lisp` は `vm-hash-cons` だけを intern table 経由で実行し、`vm-cons` は ANSI CL の fresh cons セマンティクスを維持する。`packages/compile/src/builtin-registry-data-ext.lisp` と `packages/expand/src/expander-data.lisp` により `(hash-cons a b)` は builtin として VM 命令へ lower される。`packages/vm/tests/list-tests.lisp` と `packages/compile/tests/pipeline-eval-tests.lisp` が helper・命令・compiled builtin 経路を検証する

#### FR-256: Pure Function Auto-Memoization (純関数自動メモ化) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/optimize/src/effects.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 純粋関数（副作用なし・参照透過性あり）と推論された関数に対して、呼び出し引数→戻り値のメモテーブルを自動挿入。テーブルサイズ上限・LRU evictionで制御。`(declare (optimize (speed 3)))`時のみ有効化
- **根拠**: GHC `{-# RULES "memo" #-}` / Mathematica automatic memoization。再帰的数値計算で指数的高速化
- **難易度**: Medium

- **関連実装**: `packages/optimize/src/optimizer-inline-pass.lisp` に `opt-make-pure-function-memo-table` / `opt-pure-function-memo-get` / `opt-pure-function-memo-put` を追加済み。memo helper は `:max-size` 指定時に LRU eviction（容量超過で最古エントリ除去、hit時touchで最近使用化）を実装。`packages/optimize/src/optimizer-purity.lisp` の `opt-pass-pure-call-optimization` は transitive purity 推論後、同一 pure direct call を straight-line region 内で `vm-move` に置換し、未使用の known-pure direct call を除去する。さらに pass 内部 memo を `opt-make-pure-function-memo-table` へ接続し、`opt-pure-function-memo-get/put` を介して pure direct call の再利用情報を管理するよう更新した（`*opt-pure-call-memo-max-size*` で容量上限を制御可能）。`packages/optimize/src/optimizer-pipeline.lisp` には policy gate `*opt-enable-pure-call-optimization*` と `opt-configure-optimization-policy` を追加し、`:pure-call-optimization` pass をフラグおよび `optimize-instructions :speed` で有効/無効切替できるようにした（`speed >= 3` で有効）。さらに `packages/pipeline/src/pipeline-data.lisp` / `packages/pipeline/src/pipeline.lisp` / `packages/compile/src/codegen.lisp` で `:speed` を optimizer へ配線し、`compile-expression`/`compile-toplevel-forms` 経路で local `declare (optimize (speed ...))` を反映できるようにした。`packages/optimize/tests/optimizer-inline-pass-tests.lisp` と `packages/optimize/tests/optimizer-purity-tests.lisp`、`packages/optimize/tests/optimizer-pipeline-tests.lisp`、`packages/compile/tests/codegen-tests.lisp`、`packages/compile/tests/pipeline-tests.lisp` が memo helper・LRU・pure call reuse・dead pure call elimination・pipeline keyword selection・policy gate・speed閾値連動・frontend連動を検証する。

---

### Phase 62 — 数値演算最適化（実装済み）

#### FR-282: Division by Arbitrary Constant (任意定数除算最適化) ✅

- **対象**: `packages/optimize/src/optimizer-strength.lisp`, `packages/optimize/tests/optimizer-strength-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: Granlund-Montgomery法系の定数除算を、実装上は「区間に対する厳密検証付き reciprocal 探索」で具体化。`(floor x 3)` を `IMUL + (optional ADD bias) + ASH` へ変換し、候補 `M,S`（必要時 `B`）を探索した上で対象区間全点の同値性を検証して適用する
- **根拠**: Granlund & Montgomery (1994) / GCC・LLVM・MSVC全実装。IDIV(20-90cycles)→IMUL+SHR(3-4cycles)で20x高速化
- **難易度**: Hard

#### FR-283: Multiply-High Instructions (乗算上位ワード命令) ✅

- **対象**: `packages/vm/src/vm-instructions.lisp`, `packages/vm/src/vm-bitwise.lisp`, `packages/codegen/src/x86-64-encoding-instrs.lisp`, `packages/codegen/src/x86-64-sequences.lisp`, `packages/codegen/src/x86-64-emit-ops.lisp`, `packages/codegen/src/x86-64-codegen-dispatch.lisp`, `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/aarch64-codegen.lisp`, `packages/codegen/src/aarch64-emitters.lisp`, `packages/codegen/src/aarch64-program.lisp`, `packages/codegen/src/aarch64-codegen-labels.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `MUL`/`IMUL`の128bit結果形式（RDX:RAX）のエミッション。AArch64では`UMULH`/`SMULH`命令。FR-282（定数除算）の前提。bignumの高速乗算にも使用
- **根拠**: x86-64 ISA / ARM Architecture Reference。定数除算・bignum演算の基盤命令
- **難易度**: Medium
- **関連実装**: x86-64 側は `emit-mul-rm64` / `emit-imul-rm64` と `emit-mul-high-sequence`、高水準 emitter `emit-vm-integer-mul-high-u` / `emit-vm-integer-mul-high-s` を追加。AArch64 側は `encode-umulh` / `encode-smulh` と `emit-a64-vm-integer-mul-high-u` / `emit-a64-vm-integer-mul-high-s` を追加。VM 側は `vm-integer-mul-high-u` / `vm-integer-mul-high-s` の 64-bit truncation semantics を `vm-bitwise.lisp` に実装
- **検証**: `packages/vm/tests/vm-bitwise-tests.lisp`（`vm-mul-high-64-semantics`）、`packages/emit/tests/x86-64-encoding-tests.lisp`（`x86-mul-rm64-high-encodings`）、`packages/emit/tests/x86-64-sequences-tests.lisp`（`x86-seq-mul-high-sequence-encodings`）、`packages/emit/tests/x86-64-emit-ops-tests.lisp`（`x86-emit-mul-high-emits-19-bytes`）、`packages/emit/tests/x86-64-codegen-tests.lisp`（`x86-mul-high-size-and-dispatch-registered`）、`packages/emit/tests/aarch64-encoding-tests.lisp`（`a64-mul-high-encoders`）、`packages/emit/tests/aarch64-codegen-tests.lisp`（`aarch64-mul-high-emitter-encodings` / `aarch64-mul-high-size-and-dispatch-registered`）、`packages/optimize/tests/optimizer-roadmap-backend-tests.lisp`（`optimize-backend-roadmap-fr-283-has-specific-evidence`）

#### FR-284: Rotate Instructions (ビット回転命令) ✅

- **対象**: `packages/optimize/src/optimizer-recognition.lisp`, `packages/optimize/src/optimizer-pipeline.lisp`, `packages/codegen/src/x86-64-emit-ops-bits.lisp`, `packages/codegen/src/aarch64-emitters.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: パターンマッチで`(logior (ash x k) (ash x (- width k)))`を検出し`ROL`/`ROR`（x86-64）/ `ROR`（AArch64）に変換。新規`vm-rotate`命令の追加。暗号アルゴリズム（SHA, AES）で頻出
- **根拠**: GCC/LLVM rotate idiom recognition。暗号・ハッシュ計算の標準最適化
- **難易度**: Medium
- **検証**: `packages/emit/tests/x86-64-emit-ops-tests.lisp`, `packages/emit/tests/aarch64-codegen-tests.lisp`（`aarch64-rotate-emitter-encoding`）, `packages/optimize/tests/optimizer-store-analysis-tests.lisp`, `packages/optimize/tests/optimizer-strength-tests.lisp` に回帰テスト `rotate-recognition-collapses-shift-or-tree` / `rotate-recognition-collapses-rotate-idiom` あり

#### FR-285: Byte Swap Instruction (バイトスワップ命令) ✅

- **対象**: `packages/optimize/src/optimizer-recognition.lisp`, `packages/optimize/src/optimizer-pipeline.lisp`, `packages/codegen/src/x86-64-emit-ops-bits.lisp`, `packages/codegen/src/aarch64-emitters.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: バイト逆順パターン（4連続`ash`+`logand`+`logior`）を検出し`BSWAP`（x86-64）/ `REV`（AArch64）に変換。ネットワークバイトオーダー変換（`ntohl`/`htonl`相当）の高速化
- **根拠**: GCC/LLVM bswap recognition。ネットワーク・バイナリI/Oの標準最適化
- **難易度**: Easy
- **検証**: `packages/emit/tests/aarch64-codegen-tests.lisp`（`aarch64-bswap-emitter-encoding`）, `packages/emit/tests/x86-64-codegen-insn-tests.lisp`, `packages/optimize/tests/optimizer-store-analysis-tests.lisp` に回帰テスト `bswap-recognition-collapses-byte-swap-tree` あり

#### FR-286: Transcendental Math Native Emission (超越関数ネイティブエミッション) ✅

- **対象**: `packages/codegen/src/x86-64-emit-ops.lisp`, `packages/codegen/src/x86-64-codegen-dispatch.lisp`, `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/x86-64-regs.lisp`, `packages/codegen/src/aarch64-emitters.lisp`, `packages/codegen/src/aarch64-program.lisp`, `packages/codegen/src/aarch64-codegen-labels.lisp`, `packages/vm/src/vm-transcendental.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `SQRTSD`（x86-64 SSE2）/ `FSQRT`（AArch64）の直接エミッションと、libm 経由の超越関数ネイティブ呼び出し（`sin`/`cos`/`exp`/`log`/`tan`/`asin`/`acos`/`atan`）。FR-228（SSE/AVX）の基盤
- **根拠**: SBCL `sb-vm::sqrtsd` / GCC `-ffast-math`。数値計算のVM解釈オーバーヘッド除去
- **難易度**: Hard
- **検証**: `packages/emit/tests/x86-64-encoding-tests.lisp`（`x86-xmm-instruction-encoding`）, `packages/emit/tests/x86-64-emit-ops-tests.lisp`（`x86-emit-sqrt-emits-sqrtsd-sequence`, `x86-emit-libm-unary-emits-21-bytes`）, `packages/emit/tests/x86-64-codegen-tests.lisp`（命令サイズ/dispatch登録）, `packages/emit/tests/aarch64-encoding-tests.lisp`（`a64-fsqrt-encoder`）, `packages/emit/tests/aarch64-codegen-tests.lisp`（`aarch64-sqrt-emitter-encoding`, `aarch64-libm-unary-emitter-size`, size/dispatch登録）

#### FR-302: Modulo Power-of-2 Strength Reduction (2のべき乗剰余最適化) ✅

- **対象**: `packages/optimize/src/optimizer-strength.lisp`, `packages/optimize/src/optimizer-pipeline.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `(mod x 2^k)` → `(logand x (1- 2^k))`（非負整数の場合）。符号付きの場合は条件付き補正が必要。`(rem x 2^k)`は符号意味論が単純
- **根拠**: GCC/LLVM/MSVC全実装。IDIV不要でAND命令1つに変換。FR-096（除算）の剰余版
- **難易度**: Easy
- **検証**: `packages/optimize/tests/optimizer-strength-tests.lisp`, `packages/optimize/tests/optimizer-strength-inline-tests.lisp` に回帰テスト `strength-reduce-mod-by-power-of-2-emits-logand` あり

#### FR-303: Overflow Detection with Hardware Flags (ハードウェアフラグによるオーバーフロー検出) ✅

- **対象**: `packages/vm/src/vm-instructions.lisp`, `packages/vm/src/vm-execute.lisp`, `packages/codegen/src/x86-64-emit-ops.lisp`, `packages/codegen/src/x86-64-codegen-dispatch.lisp`, `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/aarch64-emitters.lisp`, `packages/codegen/src/aarch64-program.lisp`, `packages/codegen/src/aarch64-codegen-labels.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: ADD/SUB/IMUL後にOF（Overflow Flag）を検査（x86-64 `JO`、AArch64 `ADDS`/`SUBS`のV flag）。オーバーフロー時に trap 命令へ進ませる checked arithmetic を実装する。FR-352 の `opt-pass-elide-proven-overflow-checks` は range proof がある checked arithmetic を unchecked integer arithmetic へ落としてこのチェックを省略できる
- **根拠**: SBCL `pseudo-atomic` + overflow trap / ANSI CL fixnum→bignum promotion。数値計算の正確性保証
- **難易度**: Hard
- **検証**: `packages/emit/tests/x86-64-emit-ops-tests.lisp` の `x86-emit-add-checked-emits-14-bytes` / `x86-emit-sub-checked-emits-14-bytes` / `x86-emit-mul-checked-emits-15-bytes` と、`packages/emit/tests/aarch64-emit-tests.lisp` の `aarch64-emit-add-checked-emits-12-bytes` / `aarch64-emit-sub-checked-emits-12-bytes` / `aarch64-emit-mul-checked-emits-24-bytes` が byte-count と trap sequence size を固定する

#### FR-304: Integer Range Analysis (整数範囲解析) ✅

- **対象**: `packages/optimize/src/optimizer-memory.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `vm-const` / `vm-move` / `vm-add` / `vm-sub` / `vm-mul` / `vm-neg` / `vm-abs` / `vm-inc` / `vm-dec` を対象に整数区間を伝播し、unknown write は destination fact を kill する。CFG 合流でのラティス計算により backend 側で配列境界チェックやオーバーフロー検査削減のための保守的 oracle を供給
- **根拠**: LLVM `LazyValueInfo` / GCC VRP (Value Range Propagation) と同じく CFG join での保守的 meet に基づく。`packages/optimize/tests/optimizer-memory-tests.lisp` が join union・片側不在 fact の drop・loop self-update kill・既存 straight-line API の回帰を検証
- **難易度**: Hard

- **設計ノート**: path-sensitive な条件分岐 refinement、比較命令からの範囲絞り込み、除算/剰余の精密 transfer、型推論系への `(integer lo hi)` 統合は、完了済みFR-304の上に重ねる高精度化トラックとして管理する。bitwise は `logand/logior/logxor` の基本 bound と、singleton shift に限る `ash` 伝播まで実装済みで、該当ケースでは facts を保守的に落として安全側に倒す。

#### FR-305: Multiply by Constant via Shifts+Adds (定数乗算のシフト+加算変換) ✅

- **対象**: `packages/optimize/src/optimizer-strength.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 少数のセットビットを持つ定数乗算を per-set-bit の shift-add 連鎖に分解。`(* x 3)` → `(+ x (ash x 1))`、`(* x 5)` → `(+ x (ash x 2))`。`%opt-mul-by-const-seq` は popcount-based greedy 分解を採用する。
- **根拠**: GCC/LLVM multiply-by-constant optimization。IMUL(3cycles)→ADD+SHL(2cycles)で高速化
- **難易度**: Medium
- **検証**: `packages/optimize/tests/optimizer-strength-inline-tests.lisp` に回帰テスト `strength-reduce-mul-by-const-decomposes` / `mul-by-const-seq-cases` / `mul-by-const-seq-correctness` あり

#### FR-306: Optimizer Cost Model for Inlining (インライン化コストモデル) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 命令別コストテーブル（call=10, add=1等、E-graphの`*egraph-op-base-costs*`を共有）。インラインコスト=本体コスト合計 vs 呼び出しオーバーヘッド
- **根拠**: LLVM `InlineCost` / GCC `ipa-inline`。命令数だけでは判断が粗い
- **難易度**: Medium
- **検証**: SBCL QAで`cheap=T expensive=NIL`出力確認、テストレジストリの`opt-inline-eligible-p-cases`ラムダ合格

---

### Phase 89 — オプティマイザ解析基盤（実装済み）

#### FR-516: 算術再結合 (Arithmetic Reassociation) ✅

- **対象**: `packages/optimize/src/optimizer-strength-ext.lisp`, `packages/optimize/src/optimizer-pipeline.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 結合律・交換律を持つ演算子 (`+`, `*`, `logand`, `logior`, `logxor`) の再結合パスを追加。定数を末尾に集約してfoldingを促進
- **根拠**: GCC `-O2`の算術再結合パス
- **難易度**: Medium

- **関連実装**: `opt-reassociate-commutative-p` が対象 VM 命令 (`vm-add`, `vm-integer-add`, `vm-mul`, `vm-integer-mul`, `vm-logand`, `vm-logior`, `vm-logxor`) を判定し、`opt-copy-commutative-binop` が同型命令を新 operand で再生成する。`packages/optimize/tests/optimizer-strength-ext-tests.lisp` と `packages/optimize/tests/optimizer-strength-inline-tests.lisp` が対象演算子表、非可換命令の除外、定数 drift の回帰を検証する。

#### FR-517: 汎用データフロー基盤 (Generic Dataflow Framework) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `defmacro define-dataflow-pass (name direction lattice transfer-fn merge-fn)` — 汎用データフロー解析フレームワーク。前向き/後ろ向き、meet演算、transfer関数をパラメータ化
- **根拠**: LLVM `DataFlowSanitizer`, GCC `df.*` infrastructure
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-dataflow.lisp` に `opt-run-dataflow`、`define-dataflow-pass`、`opt-dataflow-result` を追加済み。既存 `cfg` / `basic-block` 構造上で前向き・後ろ向き worklist、meet、transfer、状態コピー、境界状態をパラメータ化できる。`packages/optimize/tests/optimizer-dataflow-tests.lisp` に分岐/合流 CFG での収束テストを追加済み。

#### FR-518: 利用可能式・到達定義 (Available Expressions / Reaching Definitions) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: FR-517の汎用フレームワーク上で前向きビットベクタ解析を実装。利用可能式集合でグローバルCSEを実現
- **根拠**: Dragon Book §9.5 — available expressions
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-dataflow.lisp` に `opt-compute-available-expressions` / `opt-compute-reaching-definitions` を追加済み。利用可能式は predecessor intersection、到達定義は predecessor union で合流する。加えて `packages/optimize/src/optimizer-cse-gvn.lisp` の `opt-pass-gvn` は、block entry で利用可能な pure 式について「到達定義が単一レジスタ名に合意している」場合だけ `vm-move` へ置換する保守的な global CSE ステップを前段適用する。`packages/optimize/tests/optimizer-dataflow-tests.lisp` と `packages/optimize/tests/optimizer-cse-gvn-tests.lisp` に join 点での解析/変換回帰テストを追加済み。

#### FR-519: e-graph 抽出実装 (E-graph Extraction) ✅

- **対象**: `packages/optimize/src/egraph-saturation.lisp`, `packages/optimize/src/egraph.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: コスト関数ベースのe-class抽出アルゴリズム実装。各e-nodeのコストをボトムアップで計算し最小コストのnodeを選択
- **根拠**: egg (equality saturation framework) の抽出アルゴリズム
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/egraph-saturation.lisp` の `egraph-extract` / `egraph-default-cost` と `packages/optimize/tests/egraph-extraction-tests.lisp` が実装根拠。以前の no-op stub 記述は古い。

#### FR-520: グローバルコピー伝播 (Global Copy Propagation) ✅

- **対象**: `packages/optimize/src/optimizer-copyprop.lisp`, `packages/optimize/src/optimizer-pipeline.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: CFGを跨いだグローバルコピー伝播。到達定義解析 (FR-518) を利用してCFG合流点でのコピー伝播
- **根拠**: LLVM `GVNPass` のコピー伝播コンポーネント
- **難易度**: Medium

- **関連実装**: `opt-pass-copy-prop` は `cfg-build` 後に `copyprop-pass-state` の worklist を回し、`%opt-copy-prop-merge` で複数 predecessor の copy environment を交差、`%opt-copy-prop-rewrite-block` で命令operandを安定した canonical register に置換する。`packages/optimize/tests/optimizer-copyprop-tests.lisp` に基本伝播、chain rewrite、kill、successor enqueue、merge の回帰テストがある。

#### FR-521: 支配木値番号付け (Dominator-Tree Value Numbering) ✅

- **対象**: `packages/optimize/src/optimizer-cse-gvn.lisp`, `packages/optimize/src/cfg.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 支配木をトップダウン走査しながらscoped hash tableで値番号付け。基本ブロック内の冗長計算と、支配ブロック内で既計算の式を除去
- **根拠**: Click & Cooper "Simple and Efficient Construction of SSA Form" VN algorithm
- **難易度**: Medium

- **関連実装**: `opt-pass-gvn` は FR-518 の保守的 global CSE 前段を適用したうえで、支配木上に value environment / memo を伝播する。`packages/optimize/tests/optimizer-cse-gvn-tests.lisp` と `packages/optimize/tests/optimizer-tests-lowlevel2.lisp` の `optimizer-gvn-dominates-branch` が、支配ブロックの同一式を dominated block で再利用する挙動を検証する。

#### FR-522: 手続き内コールグラフ (Intraprocedural Call Graph) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 手続き内 call graph 構築、再帰ラベル検出、recursive callee の inline 抑制、同一グラフの global DCE reachability への再利用
- **根拠**: GCC/LLVM インライン再帰ガード
- **難易度**: Medium

- **関連実装**: `packages/optimize/tests/optimizer-inline-tests.lisp` の `opt-build-call-graph-no-calls` / `opt-call-graph-recursive-labels-no-recursion` / `opt-call-graph-recursive-labels-direct-recursion` / `opt-call-graph-recursive-labels-mutual-recursion` に加えて、`opt-pass-inline-skips-recursive-callee` が recursive callee を `vm-call` のまま残す回帰を検証する。

---

### Phase 88 — 多面体最適化 (Polyhedral Model)（実装済み）

#### FR-523: Affine Loop Analysis (アフィンループ解析) ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-speculative.lisp`, `packages/optimize/tests/optimizer-pipeline-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: canonical loop 形（`label`/`lt`/`jump-zero`/`step`/`back-jump`）を命令列から検出し、induction variable・bound・access kind を抽出して affine summary を生成する解析 pass を実装。
- **根拠**: LLVM Polly / GCC Graphite / MLIR affine dialect。ループ最適化の理論的基盤
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/src/optimizer-pipeline-speculative.lisp` の `opt-pass-affine-loop-analysis` が実命令列から summary を抽出し、`*opt-last-affine-loop-summaries*` へ記録する。`packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-pass-affine-loop-analysis-captures-real-loop-summary` が実ループ入力での要約抽出を検証する。

#### FR-524: Polyhedral Loop Interchange (ループ交換) ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-speculative.lisp`, `packages/optimize/tests/optimizer-pipeline-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: canonical loop の body-core 先頭2命令が `pure` かつ相互依存しない場合に限り、命令順を入れ替える保守的な loop-interchange 変換を適用する。制御骨格（header/jump/step）は保持する。
- **根拠**: GCC `-floop-interchange` / LLVM LoopInterchange pass。行列乗算での 10x 以上の性能差を生む変換
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-pipeline-speculative.lisp` の `opt-pass-loop-interchange` が independent/pure な core-op swap を実行し、副作用を含む loop body は変換対象にしない。`packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-pass-loop-interchange-handles-nested-canonical-loop`（テスト名は互換維持）/ `optimize-pass-loop-interchange-skips-side-effecting-loop` が変換発生条件と安全側挙動を検証する。

#### FR-525: Polyhedral Schedule Optimization (Pluto アルゴリズム) ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-speculative.lisp`, `packages/optimize/tests/optimizer-pipeline-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: canonical loop の body-core に対し、並べ替え可能（pure op）条件下でコスト順スケジューリングを適用する pass を実装。制御骨格（header/jump/step）は保持する。
- **根拠**: Pluto paper / MLIR affine-loop-opt。多面体スケジューリングの実用実装
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/src/optimizer-pipeline-speculative.lisp` の `opt-pass-polyhedral-schedule` が loop body の実並べ替えを行う。`packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-pass-polyhedral-schedule-reorders-loop-body` が命令列変化を検証する。

#### FR-526: Loop Fusion / Fission (ループ融合・分割) ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-speculative.lisp`, `packages/optimize/tests/optimizer-pipeline-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: adjacent canonical loop の iteration space 同値性と副作用条件を満たす場合のみ fusion を適用。加えて oversized loop body に対しては split label を用いた fission 変換を適用する。
- **根拠**: LLVM LoopFusion / GCC `-floop-optimize2`。単純なストリーム融合では捉えられないループレベルの最適化
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-pipeline-speculative.lisp` の `opt-pass-loop-fusion-fission` が safety 条件つき fusion と split-label ベース fission を実行する。`packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-pass-loop-fusion-fission-fuses-adjacent-loops` / `optimize-pass-loop-fusion-fission-skips-unsafe-fusion` / `optimize-pass-loop-fusion-fission-splits-oversized-loop` が挙動を検証する。

---

### Phase 89 — ML駆動最適化（実装済み）

#### FR-527: ML-Guided Inlining / MLGO (機械学習によるインライン判定) ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-speculative.lisp`, `packages/optimize/tests/optimizer-pipeline-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: feature 列挙と model version を入力に、決定論的 inline score descriptor を返す planning helper を提供。インライン可否判断に使うスコア入力の正規化を担う。
- **根拠**: LLVM MLGO (Google 2020) — Inliner/RegAlloc に RL モデルを適用して 0.5〜2% のバイナリサイズ削減。cl-cc では `selfhost` が現実的なトレーニングワークロードとなる
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-pipeline-speculative.lisp` の `opt-ml-inline-score-plan` が feature vector と model version から決定論的スコアを返す。`packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-ml-inline-score-plan-is-deterministic` が同一入力での再現性を検証する。

#### FR-528: Learned Cost Model for Code Generation (学習型コスト関数) ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-speculative.lisp`, `packages/optimize/tests/optimizer-pipeline-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: opcode feature 群と target（x86-64/aarch64）から予測コスト descriptor を返す planning helper を提供。ターゲット依存のコスト見積り入力を統一形式で供給する。
- **根拠**: IREE / TVM の learned cost model / Halide autoscheduler。手書きコスト表は CPU マイクロアーキテクチャ世代ごとに陳腐化するため、測定駆動のアプローチが必要
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/src/optimizer-pipeline-speculative.lisp` の `opt-learned-codegen-cost-plan` が opcode features と target を受けて予測コスト計画を生成する。`packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-learned-codegen-cost-plan-is-target-aware` がターゲット依存のコスト差を検証する。

---

### Phase 2 — 末尾呼び出し最適化 (Tail Call Optimization)

#### FR-004: Proper Tail Call Elimination (TCE) ✅

- **対象**: `packages/compile/src/cps.lisp`, `packages/compile/src/codegen.lisp`, `packages/vm/src/vm-execute.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 末尾位置にある `vm-call` を `vm-tail-call` 命令に置換。呼び出し先への制御移譲時に呼び出し元フレームを解放（上書き）する。R7RS / ANSI CL 末尾再帰最適化の基盤
- **根拠**: Proper tail calls は Scheme 規格の必須要件。CL ではコンパイラが自由に適用可。スタックオーバーフロー防止と `labels` の末尾再帰ループ変換に必須
- **難易度**: Medium
- **検証**: `packages/vm/tests/vm-call-tests.lisp` の `vm-tail-call-behavior` が closure 経路で call stack 非増加（TCO）を検証

#### FR-005: Self-Tail-Call → ループ変換 ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **内容**: 自己末尾再帰 `(defun f (x) ... (f x'))` を `(loop ...)` 形式のジャンプ＋変数更新に変換。コールフレームを一切生成しない完全ループ化。対象: 単一関数、引数→レジスタの直接更新
- **根拠**: GHC strict worker-wrapper / LLVM TailCallElim。再帰カウンタ・リスト処理の慣用パターンで適用率が高い
- **難易度**: Easy (self-recursive に限定)

#### FR-006: 相互末尾再帰 Trampoline ✅

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: `labels` 内で相互末尾再帰する関数群を trampoline ループ（`while (thunk) { thunk = thunk() }`）に変換。`(labels ((even? (n) (if (= n 0) t (odd? (- n 1)))) (odd? (n) ...))...)` が O(1) スタックで実行できる
- **根拠**: Chicken Scheme の call/cc CPS 変換の核心技術。SBCL の `labels` 末尾呼び出し最適化
- **難易度**: Medium

---

### Phase 3 — エスケープ解析 & オブジェクト表現（実装済み: FR-007/014/015 subsets）

#### FR-007: Escape Analysis (エスケープ解析) ✅

- **対象**: `packages/compile/src/closure.lisp`, `packages/compile/src/codegen.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: オブジェクト (cons, vector, closure, CLOS instance) がスコープ外に到達するかを解析。エスケープしない場合はスタック割り当てまたはスカラー置換 (FR-014) を適用。逃げ方の分類: (1) 戻り値、(2) グローバル変数への代入、(3) 別クロージャへのキャプチャ、(4) 外部関数呼び出し引数
- **根拠**: Java HotSpot Escape Analysis / SBCL `(declare (dynamic-extent ...))` の自動化。ヒープ割り当て削減でGC圧力を大幅軽減
- **難易度**: Hard

- **関連実装**: `packages/compile/src/closure.lisp` の `binding-escapes-in-body-p` により、束縛単位の保守的な手続き内 escape 判定を共有化。`packages/compile/src/codegen-functions.lisp` ではこの判定を `&rest` リストのスタック安全再利用（`vm-closure-rest-stack-alloc-p` / `vm-build-list`）に使用し、`packages/compile/src/codegen-core.lisp` + `packages/compile/src/codegen.lisp` では単純な `let` 束縛 `cons` に対する no-escape `car`/`cdr` 分解、および固定長 `make-array` に対する no-escape 定数/変数添字 `aref` / `array-length` / `aset` 分解を行う。`packages/compile/src/codegen-clos.lisp` では単純な `let` 束縛 `make-instance` に対して no-escape な `slot-value` / `set-slot-value` をスロットごとのレジスタへ分解し、`packages/compile/src/codegen.lisp` では direct-call-only な `let` 束縛 `lambda` に対して `vm-closure` を作らずラムダ本体を直接展開する。

#### FR-014: SROA (Scalar Replacement of Aggregates) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: エスケープしない合成オブジェクト（cons, struct, small vector）のフィールドを独立したレジスタに分解。`(let ((p (cons a b))) (car p))` → レジスタ `R_a` を直接使用。cons セル生成ゼロ。ヒープ割り当て命令 (`vm-cons`, `vm-make-array`) を後続の `vm-get-slot`/`vm-set-slot` と共に除去
- **根拠**: LLVM `SROA` / GCC `IPA-SRA`。HotSpot Scalar Replacement。エスケープ解析 (FR-007) の後処理として動作
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-memory-passes.lisp` の `opt-pass-cons-slot-forward` が fresh `vm-cons` fact を追跡し、`packages/optimize/src/optimizer-pipeline.lisp` の default convergence pipeline に登録済み。`packages/optimize/tests/optimizer-memory-pass-tests.lisp` の `cons-slot-forward-*` テスト群が `car` / `cdr` 置換、alias 伝播、source overwrite、`rplaca` kill、保守的 kill を検証する。

#### FR-015: Object Inlining (Allocation Sinking) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 割り当てを使用箇所の直前まで下降させ (sink)、条件分岐で使用されない分岐では割り当てを完全除去。`(let ((v (make-array 10))) (if cond (aref v 0) 42))` → cond=nil の分岐で割り当てを除去
- **根拠**: LLVM Allocation Sinking / V8 TurboFan object inlining。allocation sinking は SROA の補完
- **難易度**: Medium

- **関連実装**: `packages/optimize/src/optimizer-flow-loop.lisp` の `opt-pass-code-sinking` が unique-use `vm-const` / `vm-cons` を target block entry へ sink し、`packages/optimize/src/optimizer-pipeline.lisp` に `:code-sinking` pass として登録済み。`packages/optimize/tests/optimizer-flow-tests.lisp` の `code-sinking-moves-const-into-target-block` / `code-sinking-noop-when-value-is-read-multiple-times` / `code-sinking-moves-cons-into-target-block` / `code-sinking-noop-for-cons-read-multiple-times` が変換と非変換条件を検証する。

---

### Phase 4 — メモリ依存解析 & デッドストア除去（実装済み: FR-016）

#### FR-016: Dead Store Elimination (DSE) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: ストア命令 X のあとにXで定義した位置への後続ロードなく別のストアが来る場合、最初のストアを除去。グローバル変数・スロット・ローカルバインディングを対象。Memory SSA (FR-217) をバックエンドとして利用可能
- **根拠**: LLVM `DeadStoreElimination` / GCC `tree-dse`。ローカル変数の初期化→即上書きパターンで頻出
- **難易度**: Medium

- **関連実装**: `packages/optimize/src/optimizer-memory-passes.lisp` に `opt-pass-dead-store-elim` を実装済み。straight-line な global/slot store 上書き除去を対象とし、`opt-compute-heap-aliases` で保守的な slot alias を扱う。`packages/optimize/tests/optimizer-memory-pass-tests.lisp` と `packages/optimize/tests/optimizer-store-analysis-tests.lisp` に回帰テストがある。

#### FR-017: Alias Analysis (型ベース別名解析, TBAA) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: 型情報に基づいてメモリ操作の別名関係を推論。`vm-get-slot` で整数スロットとシンボルスロットは別名なし。異なるクラスのインスタンス間はスロット別名なし。TBAA メタデータを VM 命令に付与し、命令スケジューリング・DSE の精度向上
- **根拠**: LLVM TBAA / GCC `-fstrict-aliasing`。Common Lisp は型タグ付きなのでTBAA適用範囲が広い
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-memory-ranges.lisp` に `opt-compute-heap-kinds` / `opt-may-alias-by-type-p` を追加済み。`opt-compute-heap-aliases` の fresh heap root と `:cons` / `:array` / `:closure` の heap kind を組み合わせ、両 root と kind が既知で異なる場合だけ non-alias と判定し、不明時は conservative に may-alias とする。`packages/optimize/tests/optimizer-lowlevel-tests.lisp` の `heap-kind-helper-distinguishes-object-classes` と `packages/optimize/tests/optimizer-memory-pass-tests.lisp` の heap kind table integrity test が直接検証する。

#### FR-018: Flow-Sensitive Pointer Analysis ✅

- **対象**: `packages/optimize/src/optimizer.lisp`
- **内容**: `setq` / `setf` による変数更新を追跡しながらポインタの指す先を解析 (flow-sensitive)。Anderson 法 (flow-insensitive) よりも精度が高く、nil チェック・型チェック除去の前提解析として活用。解析スコープは関数内 (intraprocedural) に限定
- **根拠**: Andersen (1994) / Steensgaard (1996)。ポインタ解析精度は alias analysis・devirtualization の品質を直接左右する
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-memory.lisp` に `opt-compute-points-to` / `opt-points-to-root` を追加済み。線形命令列上で fresh heap allocator と `vm-move` を追跡する flow-sensitive helper として運用し、再定義時には facts を kill する安全側契約を採用する。

---

### Phase 5 — インラインキャッシュ & 動的ディスパッチ（実装済み）

#### FR-009: Monomorphic Inline Cache (MIC) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/vm/src/vm-clos.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 各コールサイトに「最後に観測された型 → メソッド」のキャッシュエントリを 1 つ付与。ガード命令 `(eq (type-of self) cached-type)` が成功した場合はキャッシュメソッドを直接呼び出し。失敗時は `find-method` にフォールバックしてキャッシュ更新
- **根拠**: Deutsch & Schiffman (1984) Smalltalk IC。V8・SpiderMonkey の inline cache の基礎。CLOS 多重ディスパッチのファストパスとして特に有効
- **難易度**: Medium

#### FR-019: Megamorphic State Optimization ✅

- **対象**: `packages/vm/src/vm-clos.lisp`
- **内容**: 同一コールサイトで観測型数が閾値 (例: 8) を超えた場合、Megamorphic State に遷移してグローバル共有キャッシュを使用。Polymorphic IC (FR-023) → Megamorphic の自動降格ロジックを追加。Megamorphic 化されたサイトでは以降 IC ミス時のキャッシュ更新を停止してオーバーヘッドを削減
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **根拠**: V8 の IC state machine (Uninitialized → Monomorphic → Polymorphic → Megamorphic)
- **難易度**: Medium

---

### Phase 9 — 型特化 & アンボクシング（実装済み）

#### FR-008: Float Unboxing (浮動小数点数アンボクシング) ✅

- **対象**: `packages/vm/src/primitives.lisp`, `packages/compile/src/codegen.lisp`, `packages/type/src/inference.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 型推論で float と確定したローカル変数を SSE2 の XMM レジスタに保持。`double` を整数レジスタのタグ付き値として持ち回さず、`XMM0`..`XMM7` に直接格納。float-only の内部ループではボクシングコストがゼロ
- **根拠**: SBCL float unboxing / GHC unboxed Double#。数値計算コードで 3〜10x の高速化。FR-056 (Worker/Wrapper) と連携して再帰関数への拡張
- **難易度**: Hard

- **関連実装**: `packages/emit/tests/regalloc-tests.lisp` の `regalloc-float-vregs-allocated-to-distinct-xmm-registers` が、float vreg を GPR ではなく XMM レジスタへ割り当てる経路を検証する。

#### FR-013: 型特化インライン展開 (Type-Specialized Inlining) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: インライン展開 (FR-030) と型情報を組み合わせ、呼び出し元の型が確定している場合に型特化版のインライン展開を適用。`(+ a b)` で `a`, `b` が fixnum と確定している場合、オーバーロード解決後の fixnum 特化コードを展開。CLOS メソッドのディスパッチ結果をインライン展開するのに特に有効
- **根拠**: SBCL `deftransform` / GHC specialise pragma。型情報とインライン化の相乗効果で数倍の高速化
- **難易度**: Hard

#### FR-020: Numeric Tower Specialization (数値タワー特化) ✅

- **対象**: `packages/vm/src/primitives.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: CL 数値タワー (fixnum → bignum → ratio → float → complex) の各段を完全に分離してコンパイル時に特化。`(+)` ではなく型アノテーション `(the fixnum ...)` があれば fixnum 専用パスを直接使用。`declare (optimize (speed 3))` 時に暗黙の型ナローイングを適用
- **根拠**: SBCL の numeric type VOPs (Virtual Operations)
- **難易度**: Hard

---

### Phase 12 — ラムダ計算レベル最適化（実装済み）

#### FR-027: Beta / Eta 簡約 (Beta/Eta Reduction) ✅

- **対象**: `packages/compile/src/cps.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: **Beta 簡約**: `((λ x. M) V)` → `M[V/x]`。CPS 継続 `((λ k. M) K)` → `M[K/k]`。**Eta 簡約**: `(λ x. (f x))` → `f` (x が f に捕捉されない場合)。インライン閾値以下の本体を無条件展開
- **根拠**: ML / Scheme コンパイラの標準変換。admin redex の除去で生成コード量が 20〜40% 削減
- **難易度**: Medium

#### FR-028: Contification (継続化) ✅

- **対象**: `packages/compile/src/cps.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: CPS 形式で、関数 `f` が単一の継続 `k` に対してのみ呼ばれる場合 (非エスケープ関数)、`f` をローカルジャンプラベルに変換してクロージャ割り当てをゼロにする。`(labels ((f (x k) ...)) (f v k))` の `f` がエスケープしない場合、`f` を単なるラベルへの `goto` に変換
- **根拠**: Kennedy (2004) "Compiling with Continuations, Continued"。MLton の主要最適化。CPS変換コンパイラ固有の最も効果的なクロージャ削減手法
- **難易度**: Hard

---

### Phase 13 — インライン展開コア（実装済み: インラインコストモデル）

#### FR-030: Known-Call Optimization (既知呼び出し最適化) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: コンパイル時に呼び出し先関数が特定できる場合 (`defun` 定義済みシンボル、`labels`/`flet` ローカル関数)、`vm-call` を `vm-direct-call` (ラベル直接ジャンプ) に変換。型チェック・クロージャ解包を省略したファストパス呼び出し規約を使用
- **根拠**: GHC known-call optimization。直接ジャンプはインダイレクト呼び出しより 1〜3 サイクル速く、インライン化の前提としても重要
- **難易度**: Medium

- **関連実装**: `packages/compile/src/codegen.lisp` には self-tail recursion の loop 化、および zero-capture `flet` を `vm-func-ref` で束縛する fast path がある。既知呼び出し最適化として回帰テスト契約に接続済み。

#### FR-031: Function Cloning for Specialization (特化クローン生成) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **内容**: 呼び出し元が特定の引数型を持つ場合、関数の型特化クローンを生成。オリジナル関数はそのまま保持し、呼び出し元ごとに最適化された版を作成。クローンは LLVM の `argument promotion` や `Function Specialization` に相当。特化後に使用されないオリジナルは Global DCE (FR-052) で除去
- **根拠**: GHC SpecConstr pass / LLVM `-speculative-execution`
- **難易度**: Hard

- **完了済みFR**: FR-306

---

### Phase 15 — アンボクシング拡張（実装済み）

#### FR-045: Integer Tag Removal in Hot Loops (ホットループ整数タグ除去) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: NaN-boxing された fixnum (63-bit tagged) をホットなループ内部でのみ untagged の native integer として扱う。ループ入口で `vm-untag-fixnum`、出口で `vm-tag-fixnum` を 1 回だけ発行。ループ本体内の全算術命令のタグシフトを除去
- **根拠**: SBCL の fixnum loop optimization。タグ演算が毎命令 1 サイクル消費するため、ループ内の除去効果は大きい
- **難易度**: Medium

---

### Phase 17 — 呼び出し規約 & ABI（実装済み: callee-saved trim）

#### FR-176: Custom Calling Convention (カスタム呼び出し規約) ✅

- **対象**: `packages/emit/src/calling-convention.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 内部関数 (エスケープしない `labels`/`flet` 定義) に対してカスタム規約を適用: (1) 任意のレジスタを引数に使用、(2) callee-saved レジスタセットを呼び出しペアの実際の使用に応じて最小化、(3) 戻り値を複数レジスタに分散 (`values` 返却の高速化)
- **根拠**: GHC STG calling convention / LLVM `fastcc`。内部関数の呼び出しオーバーヘッドを 50〜80% 削減可能
- **難易度**: Hard

#### FR-046: Return Value Optimization (RVO) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/vm/src/vm-execute.lisp`
- **内容**: 複数値を返す `values` 呼び出しで、呼び出し元が即座に値を分解する場合 (`multiple-value-bind` / `nth-value`)、ヒープ上の多値オブジェクトを生成せず直接レジスタに展開。`(multiple-value-bind (a b) (values x y) ...)` → `a=x, b=y` のレジスタ直接バインド
- **根拠**: C++ RVO / SBCL multiple values optimization。多値を多用する CL コードで有効
- **難易度**: Medium

- **関連実装**: `packages/compile/src/codegen-control.lisp` では (1) `multiple-value-bind` の values-form が明示的な `ast-values` の場合、`vm-values` / `vm-mv-bind` を介さずに各値を直接レジスタ束縛し、(2) `multiple-value-call` の全引数が明示的な `ast-values` の場合、`vm-values-to-list` / `vm-apply` を介さずに flatten した引数列で直接 `vm-call` を行う fast path を実装済み。

- **完了済みFR**: FR-176

---

### Phase 18 — Switch / Typecase 最適化（実装済み）

#### FR-128: typecase Jump Table (typecase ジャンプテーブル) ✅

- **対象**: `packages/expand/src/macros-basic.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `typecase`/`etypecase` で型の種類が密な場合 (fixnum, cons, string, symbol, vector など CLOS タグで識別可能)、タグ値を index として `JMP [rax*8+table]` の間接ジャンプに変換。型タグが 4 bit で表現できる場合はほぼ O(1) ディスパッチ
- **根拠**: SBCL `typecase` compilation / GCC `switch` with jump table。CLOS dispatch の内部でも型チェックチェーンを置き換え可能
- **難易度**: Medium

- **関連実装**: `packages/expand/src/macros-control-flow.lisp` の `%prune-typecase-clauses` により、先行節に包含される後続節は展開前に削除される。さらに `packages/type/src/exhaustiveness.lisp` に `check-typecase-exhaustiveness` / `check-etypecase-completeness` / `useful-typecase-arms` があり、型 case の冗長節・網羅性解析基盤を提供する。

---

### Phase 19 — 副作用・純粋性解析（実装済み）

#### FR-152: Transitive Function Purity Inference (推移的純粋性推論) ✅

- **対象**: `packages/optimize/src/effects.lisp`, `packages/optimize/src/optimizer-purity.lisp`, `packages/optimize/src/optimizer-pipeline.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: コールグラフのボトムアップ走査で関数の副作用を推移的に計算。リーフ関数が pure と判定されれば呼び出し元も pure に。`(defun square (x) (* x x))` は自動的に `pure` マーク。純粋と判定された関数は (1) CSE の対象、(2) DCE の対象、(3) 自動メモ化 (FR-256) の対象
- **根拠**: GHC purity analysis / LLVM `@llvm.readnone` / SBCL `sb-c:no-side-effects`. 純粋性はほぼ全最適化パスの精度向上に使える
- **難易度**: Medium

- **関連実装**: `packages/optimize/src/optimizer-purity.lisp` に `opt-function-body-transitively-pure-p` / `opt-infer-transitive-function-purity` と `opt-pass-pure-call-optimization` を実装済み。pass は `optimize-instructions` の既定 convergence pipeline に組み込まれ、straight-line region 内の known-pure direct `vm-call` について (1) 同一 callee label + 同一 argument register の再呼び出しを `vm-move` に置換し、(2) dead destination の pure direct call を除去する。dynamic call / `vm-apply` / `vm-generic-call` / unresolved indirect call / `vm-tail-call` / 再帰 SCC / ラベル・制御フロー境界をまたぐ再利用は安全側の対象外として扱い、FR-256 の memo table・LRU・policy gate と同じ保守的契約で運用する。

---

### Phase 20 — SIMD / ベクトル化（実装済み）

#### FR-228: SSE/AVX Float Arithmetic (SIMD 浮動小数点演算) ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/vm/src/primitives.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: float 型が確定した演算に対して SSE2 命令 (`ADDSD`, `SUBSD`, `MULSD`, `DIVSD`, `SQRTSD`) を直接エミット。FR-008 (Float Unboxing) と連携して XMM レジスタでのフルパイプライン演算を実現。AVX2 拡張として 256-bit `VADDPD` / `VMULPD` によるベクトル化
- **根拠**: SBCL `sb-vm::double-float-add-vop` / GCC SSE2 expansion。スカラー float 演算を 4〜8x 高速化
- **難易度**: Hard

#### FR-229: Auto-Vectorization — SLP (Superword Level Parallelism) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: 同一基本ブロック内の独立したスカラー演算 (同種命令・連続メモリアクセス) を検出し、SIMD 命令にパック。`(setf (aref r 0) (+ (aref a 0) (aref b 0))) (setf (aref r 1) (+ ...)) ...` → `VADDPD ymm0, [a], [b]; VMOVUPD [r], ymm0`。SLP は ループ非依存の SIMD 化
- **根拠**: Larsen & Amarasinghe (2000) SLP / LLVM `SLPVectorizer`。ベクトル化可能なコードが明示的ループ形式でない場合に適用
- **難易度**: Very Hard

#### FR-230: Loop Vectorization (ループ自動ベクトル化) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/optimize/src/cfg.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **依存**: FR-021 (SCEV), FR-039 (BCE), FR-017 (Alias Analysis)
- **内容**: 単純な `dotimes` ループで、ボディが独立した連続メモリ操作である場合に SIMD 版ループを生成。(1) 依存解析でベクトル化可能性チェック、(2) 正規化ループを検出、(3) ベクトル幅 (128/256/512 bit) を決定、(4) スカラーエピローグを付加。AArch64 では NEON/SVE を使用
- **根拠**: LLVM `LoopVectorize` pass / GCC `-O3` auto-vectorization。数値配列処理で 4〜16x の高速化
- **難易度**: Very Hard

#### FR-231: RISC-V Vector Extension (RVV) Codegen ✅

- **対象**: 新規 `packages/emit/src/riscv64.lisp`
- **内容**: RISC-V Vector Extension (RVV 1.0) のコード生成サポート。可変長ベクトルレジスタ (`v0`..`v31`) を使用した浮動小数点・整数 SIMD 演算のエミッション。`vsetvli` による動的ベクトル長設定と `vle64.v`/`vse64.v` によるロード/ストア
- **根拠**: 2025 年以降 RISC-V サーバー CPU (SpaceMiX, SiFive P870) が普及。RVV は ARM SVE に相当するスケーラブルベクトルモデル
- **難易度**: Very Hard

#### FR-232: AArch64 SVE/SVE2 Codegen ✅

- **対象**: `packages/emit/src/aarch64.lisp`
- **内容**: ARM Scalable Vector Extension (SVE/SVE2) による可変幅 SIMD コード生成。`ptrue`/`whilelt` によるプレディケートレジスタ管理、`ld1d`/`st1d` によるスケーラブルロード/ストア。Apple M シリーズ (SVE2) および AWS Graviton3 (SVE) 対応
- **根拠**: AArch64 SVE は 2024 年時点で主要サーバー CPU に搭載済み。Graviton3 は SVE で LLVM 比 20% 向上を実測
- **難易度**: Very Hard

---

### Phase 56 — レジスタ割り当て（実装済み: FR-290, FR-292, FR-293）

#### FR-290: Linear Scan Register Allocation (線形スキャン) ✅

- **対象**: `packages/emit/src/regalloc.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: Poletto & Sarkar (1999) の線形スキャン実装。(1) 全命令に番号付け、(2) def-use チェーンから live interval 計算、(3) 区間ソート後にアクティブセットを管理しながらレジスタ割り当て、(4) スピル判定 (コスト最小の interval をスピル)。CFG 合流点での live interval 統合
- **根拠**: JVM HotSpot C1 / LLVM RegAlloc linear scan。グラフ彩色より O(n) で高速なため JIT に適合
- **難易度**: Hard

- **関連実装**: `compute-live-intervals` が def/use から区間を作成し、`linear-scan-allocate` が active set の expire/assign/spill/evict を行う。`allocate-registers` が allocation と spill rewrite を統合する。`packages/emit/tests/regalloc-tests.lisp` に live interval、分岐区間拡張、物理レジスタ割り当て、spill pressure の回帰テストがある。

#### FR-291: Graph Coloring Register Allocation (グラフ彩色) ✅

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: Chaitin-Briggs アルゴリズム。(1) 干渉グラフ (interference graph) 構築、(2) 単純化 (degree < K ノードのスタック積み)、(3) コアレッシング (`vm-move` を干渉しない場合に除去)、(4) 凍結・スピル、(5) 彩色。コアレッシングにより冗長 `vm-move` を大幅削減
- **根拠**: Chaitin (1982) / George-Appel Iterated Register Coalescing。LLVM の `PBQP` や GCC の `IRA` はこれを基にする
- **難易度**: Very Hard

#### FR-292: Register Coalescing (レジスタ合体) ✅

- **対象**: `packages/emit/src/regalloc.lisp`
- **内容**: `(vm-move R_dst R_src)` 命令で、`R_dst` と `R_src` の生存区間が干渉しない場合に同一物理レジスタに割り当て、`vm-move` を除去 (coalescing)。Conservative coalescing (George & Appel) と Aggressive coalescing の両方を実装
- **根拠**: コアレッシングにより関数呼び出しや CPS 継続渡しで生成される大量の `move` 命令を除去。コード密度向上
- **難易度**: Hard

- **関連実装**: `packages/regalloc/src/regalloc.lisp` の `coalesce-with` と `packages/regalloc/src/regalloc-allocate.lisp` の `%lsa-try-coalesce` により、`vm-move` の source interval が destination interval と干渉しない場合は同じ物理レジスタを割り当てる。`packages/emit/tests/regalloc-tests.lisp` の `regalloc-allocate-coalesces-move-to-same-physical-reg` が回帰テストになっている。

#### FR-293: Spill Code Optimization (スピルコード最適化) ✅

- **対象**: `packages/emit/src/regalloc.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: レジスタ不足時のスピル/リロードコードを最適化: (1) スピルスロット合体 (同時生存しない値が同一スタックスロットを共有)、(2) スピルコスト計算でループ外の値を優先スピル、(3) rematerialization — スピルよりも再計算が安い定数・アドレス計算は再計算に置き換え
- **根拠**: LLVM `GreedyRegAlloc` のスピルコスト計算 / GCC `reload` pass
- **難易度**: Hard

- **関連実装**: `insert-spill-code` が spill-load/store を挿入し、`%lsa-best-spill-candidate` が next-use distance で spill 候補を選ぶ。`allocate-registers` は定数 rematerialization 用の `remat-map` を構築し、`packages/emit/tests/regalloc-tests.lisp` に spill pressure、scratch register 分離、rematerialization の回帰テストがある。

---

### Phase 57 — プロファイルガイド最適化 (PGO)（実装済み）

#### FR-295: PGO Instrumentation (プロファイル計装) ✅

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: コンパイル時に基本ブロックごとのカウンタ (`__bb_count[]`) と分岐方向カウンタを挿入。トレーニング実行後にプロファイルデータを `.clcc-pgo` ファイルに出力。`./cl-cc compile --pgo-generate` / `./cl-cc compile --pgo-use file.clcc-pgo` の 2 フェーズビルド
- **根拠**: GCC `-fprofile-generate/-fprofile-use` / LLVM IR-level PGO。実際の実行パターンを最適化の入力とする
- **難易度**: Medium

- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。

#### FR-296: PGO-Guided Inlining (プロファイル駆動インライン) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`
- **依存**: FR-295
- **内容**: プロファイルデータの呼び出し頻度を参照してインライン閾値を動的に調整。ホット呼び出しサイト (top 10%) の閾値を 3x 拡大、コールド呼び出しサイトでインライン化を抑制。FR-150 (Adaptive Thresholds) の実測データ版
- **根拠**: LLVM Profile-guided inliner / GCC `-fpgo-inline-decision`。ヒューリスティックより実測ベースの方が 5〜15% 高速
- **難易度**: Medium

- **関連実装**: `packages/cli/src/main-dump.lisp` の `%compile-opts-kwargs` で `--pgo-use` profile を読み込み、ヒューリスティックに `:speed` と `:inline-threshold-scale` を導出して optimizer policy へ接続。`packages/pipeline/src/pipeline-data.lisp` / `packages/pipeline/src/pipeline.lisp` / `packages/optimize/src/optimizer-pipeline.lisp` / `packages/optimize/src/optimizer-inline-cost.lisp` で speed と scale を inline adaptive threshold に反映（PGO時に閾値拡大）するよう配線。`packages/pipeline/src/pipeline-native.lisp` も `:speed` / `:inline-threshold-scale` を native compile 経路へ配線。

#### FR-297: PGO-Guided Code Layout (プロファイル駆動コード配置) ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, コンパイルパイプライン
- **依存**: FR-295
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: プロファイルデータを使用してホット基本ブロックを連続アドレスに配置 (Pettis-Hansen アルゴリズム)。ホットパス上の fall-through を最大化してブランチ命令を削減。コールドパス (エラー処理等) を関数末尾またはコールドセクションに移動
- **根拠**: Pettis & Hansen (1990) Profile-based code positioning / LLVM `MachineBlockPlacementPass`。I キャッシュヒット率向上で 5〜10% 高速化
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-pipeline-speculative.lisp` の PGO layout helper 群が profile edge count に基づく hot-chain / loop rotation 計画を提供し、`packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-pgo-build-hot-chain-prefers-hottest-successors` と `optimize-pgo-rotate-loop-places-preferred-exit-at-bottom` が回帰を検証する。

#### FR-298: AutoFDO (Sampling-based Profile) ✅

- **対象**: コンパイルパイプライン
- **内容**: `perf record` / `dtrace` のサンプリングプロファイルを直接 PGO 入力として使用。計装不要のため本番ワークロードのプロファイルが得られる。サンプルを IR レベルの基本ブロックにマッピングするデバッグ情報 (FR-330) が前提
- **根拠**: AutoFDO (Google 2014) / LLVM `SampleProfileLoader`。計装なしで実プロダクション負荷のプロファイルを活用
- **難易度**: Very Hard

---

### Phase 58 — リンク時最適化 (LTO)（実装済み）

#### FR-300: Full LTO (全プログラム最適化) ✅

- **対象**: `packages/pipeline/pipeline.lisp`, コンパイルパイプライン
- **内容**: 全コンパイル単位をリンク時に一つの IR にまとめてグローバル最適化を適用。`./cl-cc compile --lto` でファイル単位ではなく全関数を一括最適化。呼び出しグラフが完全に見えるため Devirtualization・Global DCE・IPSCCP の精度が飛躍的に向上
- **根拠**: GCC `-flto` / LLVM Full LTO。バイナリサイズ削減 20〜40%、速度向上 5〜20%
- **難易度**: Hard

#### FR-301: Thin LTO (分散リンク時最適化) ✅

- **対象**: コンパイルパイプライン
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: Full LTO はシングルスレッドで全 IR をメモリに保持するためスケールしない。Thin LTO は各モジュールのサマリ (exported symbols, call graph edges) を収集し、インポート判断後にモジュールを並列最適化。ビルド時間 10x 向上
- **根拠**: LLVM ThinLTO (2016)。Chrome / Clang 自身の本番ビルドに使用
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-merge-module-summaries-aggregates-exports-and-counts` と `optimize-thinlto-import-decision-respects-budget-linkage-and-cycles` が summary merge と import 判定の回帰を検証する。

---

### Phase 63 — JIT & 動的コンパイル（実装済み）

#### FR-310: Tiered Compilation (多段 JIT) ✅

- **対象**: `packages/cli/src/main.lisp`, `packages/pipeline/pipeline.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 実行頻度に応じた多段コンパイル: Tier 0 = VM インタープリタ、Tier 1 = ベースライン JIT (最適化なし・高速コンパイル)、Tier 2 = 最適化 JIT (PGO + 全最適化パス)。ホットスポット検出はコールカウンタ + バックエッジカウンタ
- **根拠**: V8 Ignition/Maglev/TurboFan / JVM C1/C2。起動時間とピーク性能のトレードオフを解決する現代的アプローチ
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-adaptive-compilation-threshold-reacts-to-warmup-pressure-and-failures` と `optimize-tier-transition-promotes-through-runtime-tiers` が閾値調整と tier 遷移ロジックを検証する。

#### FR-311: On-Stack Replacement (OSR) ✅

- **対象**: `packages/vm/src/vm-execute.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 実行中の (インタープリタ or Tier 1) 関数を最適化 JIT コンパイル版に切り替える。ループのバックエッジで「ここでOSRが発生した場合にレジスタ状態はこう」というエントリポイントを設定。V8 `OSR` / JVM `OSR entry` に相当
- **根拠**: Hölzle et al. (1992) "Debugging Optimized Code with Dynamic Deoptimization"。長時間実行ループの途中でJITを適用するために必須
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-osr-trigger-p-uses-hotness-threshold` と `optimize-osr-materialize-entry-maps-machine-to-vm-registers` が helper の回帰を検証する。

#### FR-312: Deoptimization / Bailout (非最適化) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/vm/src/vm-execute.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 投機的最適化 (型予測・インライン化) が外れた場合に最適化版コードからインタープリタに制御を戻す。デオプト時に: (1) 最適化版のレジスタ状態をインタープリタ用フレームに変換、(2) 元の実行位置から継続。Bail-out ガード命令を各投機点に挿入
- **根拠**: V8 Deopt / HotSpot uncommon_trap。投機的最適化の安全ネットとして必須
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers` が deopt state materialization helper を検証する。

---

### Phase 64 — セキュリティ緩和策（実装済み）

#### FR-315: Control Flow Integrity (CFI) ✅

- **対象**: `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/aarch64-codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 間接呼び出し (`vm-funcall`) の前にターゲットが正当な関数エントリポイントであることを検証。x86-64 `ENDBR64` (CET) / AArch64 BTI (Branch Target Identification) を全関数先頭に挿入。制御フローハイジャック攻撃 (ROP/JOP) を防止
- **根拠**: LLVM CFI / Intel CET (2020)。2025 年のシステムソフトウェアでは CET 対応が事実上必須
- **難易度**: Medium

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の CFI plan tests に加え、`packages/emit/tests/x86-64-codegen-tests.lisp` の `x86-64-cfi-entry-emits-endbr64-bytes` / `x86-64-program-with-indirect-call-starts-with-endbr64`、`packages/emit/tests/x86-64-codegen-insn-tests.lisp` の `x86-64-call-cfi-guard-avoids-clobbering-rax-target`、`packages/emit/tests/aarch64-codegen-tests.lisp` の `aarch64-cfi-entry-emits-bti-c-bytes` / `aarch64-program-with-indirect-call-starts-with-bti-c` が backend 統合を検証する。

#### FR-316: Retpoline (Spectre v2 対策) ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 間接呼び出し・間接ジャンプを Retpoline シーケンス (`call_target → capture_ret_spec → ret`) に置換して投機的実行による Spectre v2 攻撃を防止。`./cl-cc compile --retpoline` オプションで有効化。IBRS / eIBRS が利用可能な CPU では Retpoline が不要なことを実行時に検出して最適化
- **根拠**: Google Retpoline (2018) / Linux kernel Retpoline implementation
- **難易度**: Medium

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の retpoline helper tests に加え、`packages/emit/tests/x86-64-codegen-insn-tests.lisp` の `x86-64-call-encoding-retpoline` / `x86-64-tail-call-encoding-retpoline` と、CLI/pipeline の `--retpoline` option 伝搬 tests が mitigation sequence と option integration を検証する。

#### FR-317: Stack Canary Insertion (スタックカナリー) ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: スタックバッファを持つ関数 (主に native backend 生成コード) のプロローグにランダム値 (canary) を積み、エピローグで照合してスタックバッファオーバーフローを検出。`./cl-cc compile --stack-protector` で有効化
- **根拠**: GCC `-fstack-protector-strong` / Clang `-stack-protector`。CL はタグ付き値で buffer overflow リスクは低いが native backend では必要
- **難易度**: Easy

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の stack-canary plan tests に加え、`packages/emit/tests/x86-64-codegen-tests.lisp` の `x86-64-stack-canary-plan-materializes-prologue-and-epilogue` / `x86-64-stack-protector-emitter-signature-bytes` と CLI/pipeline の `--stack-protector` option 伝搬 tests が backend emission と user-facing option を検証する。

#### FR-318: Shadow Stack (CET SS) Support ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/vm/src/vm-execute.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: Intel CET (Control-flow Enforcement Technology) の Shadow Stack 機能を使用。`CALL` 命令が Shadow Stack にリターンアドレスを保存、`RET` 命令がメインスタックと Shadow Stack を照合。CL の継続・スタック操作との整合性を確保
- **根拠**: Intel CET (Tiger Lake 以降)。ROP 攻撃の根本的防止
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の shadow-stack plan tests に加え、`packages/emit/tests/x86-64-codegen-insn-tests.lisp` の shadow-stack control instruction tests が push/pop handler、restart、condition/error、catch/throw 経路の save/restore/adjust emission を検証する。

---

### Phase 65 — WebAssembly モダン機能 (2024-2026)（実装済み）

#### FR-320: Wasm Tail Calls (Wasm 末尾呼び出し) ✅

- **対象**: `packages/codegen/src/wasm-emit.lisp`, `packages/codegen/src/wasm-trampoline*.lisp`, コンパイルパイプライン
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `return_call` / `return_call_indirect` Wasm 命令を使用した末尾呼び出しへの移行。CPS 変換後の継続渡し呼び出しを O(1) スタックで実行可能にし、段階的に trampoline 依存を縮小
- **根拠**: W3C WebAssembly Tail Call Proposal (Phase 4, 2024)
- **難易度**: Medium

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-wasm-select-tailcall-opcode-uses-return-call-forms` が opcode 選択分岐を検証する。`packages/emit/tests/wasm-tests.lisp` の direct/indirect tail-call emit テストが codegen 連携を検証する。

#### FR-321: Wasm GC (参照型・構造体・配列) ✅

- **対象**: `packages/codegen/src/wasm-emit.lisp`, Wasm バックエンド全体
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: WebAssembly GC Proposal (Phase 4, 2024) の型・命令を使用。`struct.new`/`struct.get`/`struct.set` でCLOSインスタンスを Wasm 構造体として表現。`array.new`/`array.get` でベクトルを Wasm 配列として表現。ブラウザの GC と統合することで JavaScript GC との相互運用性向上
- **根拠**: Wasm GC は Dart/Kotlin/OCaml など多言語が採用中 (2025)。Wasm ホスト上での Common Lisp 実行の理想的な基盤
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-wasm-gc-layout-preserves-kind-and-fields`、`optimize-wasm-gc-layout-validates-struct-and-array-shapes`、`optimize-wasm-gc-runtime-host-compatibility-requires-feature-and-valid-layout`、`optimize-wasm-gc-optimization-plan-reflects-layout-kind` が layout 保持・検証・host互換 gate・最適化ヒント生成を検証する。

#### FR-322: Wasm SIMD (固定幅 128-bit SIMD) ✅

- **対象**: Wasm バックエンド
- **内容**: WebAssembly SIMD Proposal (Phase 4, 2022 以降全主要実装で有効) の命令を使用。`v128.load`/`f64x2.add`/`i32x4.mul` 等の Wasm SIMD 命令で float/int 配列演算を加速。FR-229 (SLP) の Wasm 版として数値演算コードのベクトル化
- **根拠**: Wasm SIMD は Chrome/Firefox/Safari/Node.js で有効。数値計算コードで 2〜4x の高速化
- **難易度**: Hard

#### FR-323: Wasm Exception Handling (Wasm 例外処理) ✅

- **対象**: Wasm バックエンド, `packages/vm/src/conditions.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: WebAssembly Exception Handling Proposal (Phase 4, 2024) の `try`/`catch`/`throw` / `try_table` 命令を使用。CL `handler-case`/`restart-case` を Wasm native 例外に変換。`throw` タグ (tag type) で CL condition hierarchy を表現
- **根拠**: Wasm EH は Emscripten / Binaryen が採用済み。従来の setjmp/longjmp エミュレーションより 50% 高速
- **難易度**: Hard

#### FR-324: Wasm Relaxed SIMD ✅

- **対象**: Wasm バックエンド
- **内容**: Wasm Relaxed SIMD Proposal (Phase 4, 2024) の命令を使用。`f32x4.relaxed_madd` (FMA)、`i32x4.relaxed_laneselect`、`f32x4.relaxed_min/max` 等。NaN 伝播規則を緩和して CPU ネイティブ命令に直接マッピング。ML 推論ワークロードで特に有効
- **根拠**: Wasm Relaxed SIMD は 2024 年 Phase 4。FMA 命令の直接利用で浮動小数点スループット 2x 向上
- **難易度**: Medium

---

### Phase 66 — デバッグ情報 & 観測性（実装済み）

#### FR-330: DWARF Debug Info Generation (DWARF デバッグ情報生成) ✅

- **対象**: `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/aarch64-codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: DWARF 5 形式のデバッグ情報生成。`.debug_info`/`.debug_line`/`.debug_frame` セクションを Mach-O / ELF バイナリに付加。ソース行番号 → 機械語アドレスのマッピング。CIE/FDE (Call Frame Information) によるアンワインド情報生成
- **根拠**: DWARF Standard (dwarfstd.org)。デバッガ対応はプロダクション品質コンパイラの必須要件
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-dwarf-line-row-preserves-location-fields` が row descriptor 生成を検証する。

#### FR-331: Source Maps for Wasm ✅

- **対象**: Wasm バックエンド
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: WebAssembly Source Map 仕様に従い、Wasm バイトコードオフセット → CL ソースファイル行番号のマッピングを生成。ブラウザ DevTools での CL ソースレベルデバッグを可能にする
- **根拠**: Source Maps v3 / DWARF-for-Wasm proposal。Wasm コードのデバッグ体験向上
- **難易度**: Medium

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-wasm-source-map-entry-preserves-offset-and-source` が source-map entry 生成を検証する。

#### FR-332: Debug Info Preservation through Optimization (最適化を通じたデバッグ情報保存) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`
- **内容**: 最適化パス実行時にソース位置情報・変数名を VM 命令にアノテーションとして保持。命令削除・移動時にアノテーションを適切に伝播・統合。最適化後コードでも変数がどのレジスタにあるかを `DWARF` LocationList で記述
- **根拠**: LLVM Debug Info preservation (DebugLoc, DIVariable)。最適化とデバッグ情報の両立は現代コンパイラの重要課題
- **難易度**: Hard

#### FR-333: Compiler Diagnostic Quality (診断品質向上) ✅

- **対象**: コンパイルパイプライン全体
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: エラー・警告メッセージに以下を追加: (1) ソース箇所の caret 表示、(2) 修正候補の提示 (`did you mean?`)、(3) 最適化が適用された/されなかった理由の説明 (`-Rpass=inline` 相当)、(4) 型推論の根拠トレース。Rust/Clang の診断品質を目標
- **根拠**: Rust compiler error messages / Clang `-Rpass` remarks。コンパイラの使いやすさは採用率に直結
- **難易度**: Medium

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-format-diagnostic-reason-renders-rpass-like-message` が診断整形を検証する。

---

### Phase 67 — 並行・並列最適化（実装済み）

#### FR-335: Thread-Local Storage Optimization (スレッドローカルストレージ最適化) ✅

- **対象**: `packages/vm/src/vm.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 現在グローバル変数として扱われているコンパイラ内部状態 (`*vm-host-bridge-functions*`, GC ヒープポインタ等) をスレッドローカル変数に変換。TLS アクセスコストを削減するため `fs:` セグメントレジスタ (x86-64) / `TPIDR_EL0` (AArch64) を使用したインライン TLS アクセスを生成
- **根拠**: GCC/Clang TLS optimization / SBCL per-thread bindings
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-tls-plan-selects-architecture-specific-base-register` に加え、`packages/emit/tests/x86-64-codegen-tests.lisp` の `x86-64-tls-base-register-uses-fsbase-plan` と `packages/emit/tests/aarch64-codegen-tests.lisp` の `aarch64-tls-base-register-uses-tpidr-el0-plan` が codegen 接続を検証する。

#### FR-336: Atomic Instruction Lowering (アトミック命令展開) ✅

- **対象**: `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/aarch64-codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `sb-ext:atomic-incf` / CAS 操作を `LOCK XADD`/`LOCK CMPXCHG` (x86-64) または `LDXR`/`STXR`/`LDADD` (AArch64) に展開。メモリフェンスセマンティクスの最小化: acquire/release/sequentially-consistent を区別して不要な `MFENCE` を除去
- **根拠**: C++ `std::atomic` mapping / Linux kernel atomic ops。GC write barrier の実装にも使用
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-select-atomic-opcode-reflects-target-and-operation` に加え、`packages/emit/tests/x86-64-codegen-tests.lisp` の `x86-64-atomic-lowering-plan-adds-seq-cst-fences` と `packages/emit/tests/aarch64-codegen-tests.lisp` の `aarch64-atomic-lowering-plan-adds-acq-rel-fences` が codegen 接続を検証する。

#### FR-337: Lock Elision with HTM (Hardware Transactional Memory) ✅

- **対象**: `packages/runtime/src/gc.lisp`, `packages/vm/src/hash.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: Intel TSX (RTM) / IBM POWER HTM を使用して楽観的ロック省略を実装。`XBEGIN`/`XEND`/`XABORT` によるトランザクション実行をトライし、競合時に通常ロックにフォールバック。ハッシュテーブルの並行アクセスと GC の STW (Stop-The-World) 縮小に適用
- **根拠**: Intel RTM / GCC `-mrtm`。競合率が低い場合に lock overhead をゼロに近づける
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-htm-plan-enables-lock-elision-only-when-supported-and-low-contention` が planning 分岐を検証。`packages/vm/tests/hash-tests.lisp` の `hash-lock-elision-wrapper-falls-back-after-abort` が VM 実行系での abort→fallback 動作を検証する。

#### FR-338: Concurrent GC (並行 GC) ✅

- **対象**: `packages/runtime/src/gc.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 三色マーキング (Tri-color marking) + Write Barrier で並行マークフェーズを実装。ミュータースレッドと並行してマーカースレッドが動作。STW は初期マーク + 最終リマークのみに短縮。Write barrier: Dijkstra incremental / SATB (Snapshot At The Beginning)
- **根拠**: Go GC (tri-color) / JVM G1/ZGC / .NET GC。GC 停止時間 O(heap) → O(live) + 短い STW に削減
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-concurrent-gc-plan-selects-satb-and-short-stw-for-latency-sensitive-mode` が planning 分岐を検証。`packages/runtime/tests/gc-sweep-major-tests.lisp` の `gc-configure-concurrent-mode-updates-runtime-flags` / `gc-major-collect-enters-concurrent-state-when-enabled`、`packages/runtime/tests/gc-write-barrier-tests.lisp` の `gc-write-barrier-satb-snapshot-major-gc-concurrent-black-object` が runtime 統合挙動を検証する。

---

### Phase 68 — 最新アーキテクチャ対応 (2025-2026)（実装済み）

#### FR-340: Apple M シリーズ 最適化 (Apple Silicon) ✅

- **対象**: `packages/emit/src/aarch64.lisp`
- **内容**: Apple M3/M4 (Avalanche + Blizzard コア) 向け最適化。(1) AMX (Apple Matrix eXtension) コプロセッサへの行列演算オフロード、(2) P コア / E コア の GCD キューアウェアなコードレイアウト、(3) Unified Memory の帯域幅最適化 (ストリーミングアクセスパターン)、(4) MTE (Memory Tagging Extension) を使用したランタイム型検査
- **根拠**: Apple Instruments / XCode の Performance Guide。M4 は単コア性能で x86-64 競合比 1.5〜2x
- **難易度**: Very Hard

#### FR-341: AMD Zen 5 / Intel Arrow Lake 最適化 ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **内容**: (1) AVX-512 VNNI (Vector Neural Network Instructions) を使用した整数 SIMD の 2x スループット向上、(2) AMD Zen 5 の 512-bit AVX 実行ユニットを活用したアンロール最適化、(3) Intel Arrow Lake の Efficiency Core スケジューリング考慮、(4) AMX-INT8/AMX-BF16 タイル乗算命令
- **根拠**: AMD Optimization Guide for Zen 5 (2024) / Intel Optimization Manual (2025)
- **難易度**: Very Hard

#### FR-342: LoongArch64 / POWER10 Backend ✅

- **対象**: 新規バックエンドファイル
- **内容**: 中国国内市場向け LoongArch64 ISA のコード生成サポート。IBM POWER10 の MMA (Matrix Math Accelerator) 命令対応。どちらも LLVM バックエンドが存在するため LLVM IR を中間表現として活用する実装も可
- **根拠**: LoongArch は 2025 年時点で中国政府系 Linux サーバーの主要 ISA。POWER10 は金融計算用途
- **難易度**: Very Hard

---

### Phase 69 — コンパイラ自体の品質・保守性（実装済み）

#### FR-345: Compiler Correctness Testing (正確性テスト) ✅

- **対象**: `tests/` 全体
- **内容**: (1) Differential Testing: 同一プログラムを最適化あり/なしでコンパイルして出力を比較、(2) Translation Validation: コンパイル前後の IR が意味的に等価であることを SMT ソルバーで検証 (ALIVE2 方式)、(3) Property-Based Testing: ランダム CL プログラム生成 → SBCL と比較
- **根拠**: ALIVE2 (LLVM IR transformation validator) / CompCert (certified compiler)。最適化バグは発見困難で生産環境での障害原因になる
- **難易度**: Hard

#### FR-346: Compiler Driver & Build System Integration ✅

- **対象**: `packages/cli/src/main.lisp`, `flake.nix`, `cl-cc.asd`
- **内容**: (1) 依存性追跡: ファイル変更時に影響するコンパイルユニットのみ再コンパイル、(2) 並列コンパイル: 独立したモジュールを並列処理 (ASDF の `:parallel t` 拡張)、(3) Ninja / CMake 互換ビルドファイル生成、(4) `compile_commands.json` 生成による LSP / IDE 統合
- **根拠**: LLVM 分散ビルド / Bazel / Buck。大規模プロジェクトでのビルド時間削減
- **難易度**: Medium

#### FR-347: Cross-Compilation Support (クロスコンパイル) ✅

- **対象**: コンパイルパイプライン, バックエンド選択
- **内容**: ホスト環境とは異なるターゲット (例: x86-64 Linux でビルドして AArch64 macOS 向け native バイナリを生成) のコンパイルサポート。ターゲット ABI・エンディアン・ポインタサイズを compile-time パラメータとして扱う
- **根拠**: LLVM cross-compilation / Rust cross-compilation。Embedded・IoT 向け CL コンパイルに必要
- **難易度**: Hard

---

### Phase 70 — 等価飽和 & 代数的最適化（実装済み: FR-350/351）

#### FR-350: E-graph Equality Saturation (等価飽和) ✅

- **対象**: `packages/optimize/src/egraph.lisp`, `packages/optimize/src/egraph-rules.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: egg ライブラリ (2021) 方式の等価飽和完全実装。(1) rewrite rules の反復適用で等価クラスを飽和させる、(2) コスト関数 (FR-519) で最小コストの実装を抽出、(3) phase-ordering problem の解決 (最適化の順序依存をなくす)
- **根拠**: egg: An E-graph Good Library (Willsey et al., POPL 2021)。テンソル演算・数値コード最適化で LLVM を超える結果が多数報告
- **難易度**: Hard

- **関連実装**: `egraph-saturate` が rule set を反復適用し、`egraph-apply-rule` が pattern match から e-class merge を行う。`egraph-default-cost` と `egraph-extract` が最小コスト式を抽出し、`vm-inst-to-enode-op` / `egraph-add-instructions` が VM 命令列を e-graph に投入する。`packages/optimize/tests/egraph-extraction-tests.lisp` に saturation と extraction の回帰テストがある。

#### FR-351: Algebraic Simplification via Rewrite Rules (代数的簡約) ✅

- **対象**: `packages/optimize/src/egraph-rules.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 数学的等価変換のルールセットを体系的に追加。モノイド・環・束の法則: `(+ x 0) → x`, `(* x 1) → x`, `(logand x -1) → x`, `(logior x 0) → x`。べき等則: `(logand x x) → x`。De Morgan 則。条件付き簡約: `(max x x) → x`, `(min x (max x y)) → x`
- **根拠**: Herbie (floating-point numerics improvement) / SymPy simplify。代数規則の体系的適用は手動ペアホールより網羅的
- **難易度**: Medium

- **関連実装**: `packages/optimize/src/egraph-rules.lisp` の `defrule` 群が `add-zero-*` / `mul-one-*` / `sub-self` / `logand-neg1` / `logior-zero` / `logand-self` / `not-*` / `max-self` / `min-max-absorb` などを登録し、`packages/optimize/src/optimizer-algebraic.lisp` の `*opt-algebraic-identity-rules*` が VM 命令列向けの同等規則を提供する。`packages/optimize/tests/egraph-rules-tests.lisp`、`packages/optimize/tests/egraph-rules-bitwise-tests.lisp`、`packages/optimize/tests/egraph-negation-tests.lisp`、`packages/optimize/tests/optimizer-tests.lisp`、`packages/optimize/tests/optimizer-tables-tests.lisp` が rule registration / firing / peephole action を検証する。

#### FR-352: Bit-Width Analysis (ビット幅解析) ✅

- **対象**: `packages/optimize/src/optimizer-memory.lisp`, `packages/optimize/src/optimizer-memory-ranges.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 整数演算の結果の有効ビット幅を追跡。`(logand x #xFF)` の結果は 8 ビットと確定。8-bit 値同士の加算は最大 9-bit。ビット幅が 63 bit 未満であればオーバーフローチェック (FR-303) を省略可能。`(= (logand x 1) 0)` → `(evenp x)` の認識
- **根拠**: LLVM `computeKnownBits` / GCC VRP。ビット幅解析はオーバーフロー除去・型チェック除去・強度低減の精度向上に貢献
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/optimizer-memory.lisp` の `opt-interval-logand` / `opt-interval-bit-width` / `opt-interval-known-bits-mask` / `opt-interval-fits-fixnum-width-p` / `opt-pass-elide-proven-overflow-checks` が conservative な bit-width helper と overflow-check elision pass を提供し、`%opt-transfer-interval-inst` が `vm-logand` mask を straight-line / CFG range propagation に流す。`packages/optimize/src/optimizer.lisp` の `%opt-rewrite-logand-low-bit-test` は low-bit equality を `vm-evenp` / `vm-oddp` に落とし、`packages/optimize/tests/optimizer-memory-tests.lisp` と `packages/optimize/tests/optimizer-memory-pass-tests.lisp` が `#xFF`→8-bit、8-bit+8-bit→9-bit、checked add elision、`(= (logand x 1) 0)`→`evenp` rewrite を回帰テストで固定する。

---

### Phase 71 — フロントエンド最適化（実装済み）

#### FR-353: DFA 最小化 & 直接コード字句解析器 ✅

- **対象**: `packages/parse/src/lexer.lisp`
- **内容**: 字句解析に使用する DFA を Hopcroft アルゴリズムで最小化して状態数を削減。さらに DFA テーブル参照方式から「直接コード」方式（各状態がコードブロック、遷移が `goto`）に変換してキャッシュ効率を向上。CL の reader macro ディスパッチをジャンプテーブルで実装
- **根拠**: re2c / LLVM `tablegen`。直接コード DFA はテーブル方式より 2〜4x 高速
- **難易度**: Medium

#### FR-354: インクリメンタルパーシング ✅

- **対象**: `packages/parse/src/cl/parser.lisp`
- **内容**: tree-sitter 方式：テキスト変更範囲を特定し影響を受ける構文木のノードのみ再パース。AST ノードに `dirty` フラグを付与し、REPL 入力・IDE 編集でのインクリメンタル再コンパイルのレイテンシを削減。変更されていない `defun` ボディの再コンパイルをスキップ
- **根拠**: tree-sitter (2018) / IntelliJ PSI。インクリメンタルパーシングは IDE レスポンスの基盤
- **難易度**: Hard

#### FR-355: Format String コンパイル時展開 ✅

- **対象**: `packages/expand/src/expander-basic.lisp`, `packages/compile/src/codegen-io-ext.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `format` の第2引数が文字列リテラルの場合、コンパイル時にディレクティブを解析して特化関数列を生成。`~A` → `princ-to-string`、`~D` → 十進固定の `write-to-string`、`~%` → `#\Newline` リテラル。中間フォーマット文字列のパースコストをゼロに
- **関連実装**: `packages/expand/src/expander-basic.lisp` の `%parse-format-literal` / `%format-literal-expansion` が `~A`, `~D`, `~%`, `~~` の限定 parser を提供し、`format nil` expander が literal path と runtime fallback を選択する。`~D` は `*print-base* = 10` / `*print-radix* = nil` の special binding で十進表記を固定する。回帰テスト: `packages/expand/tests/expander-basic-tests.lisp` の `expander-format-literal-*`
- **根拠**: SBCL `format` compiler transform。format は CL で最多使用される出力関数
- **難易度**: Medium

#### FR-356: Reader 高速化 ✅

- **対象**: `packages/parse/src/lexer.lisp`, `packages/parse/src/cl/parser.lisp`
- **内容**: (1) 数値パーサの特化：整数リテラルの fast path（`+`/`-`・十進数列の直接変換）、(2) シンボルルックアップの最適化：`*package*` の intern テーブルへのダイレクトアクセス、(3) readtable ディスパッチの 256 エントリジャンプテーブル化、(4) `#.` / `#+` / `#-` の early exit による短絡
- **根拠**: SBCL reader optimization / CCL fast reader。CL の自己ホスティングではソース読み込みがボトルネックになる
- **難易度**: Medium

#### FR-357: AST レベルの定数畳み込み ✅

- **対象**: `packages/compile/src/codegen-fold-optimize.lisp`, `packages/compile/src/codegen-fold.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: VM 命令生成前の AST レベルで定数式を畳み込む。`(+ 1 2)` → `ast-int(3)`、`(string-length "hello")` → `ast-int(5)`。特に `defconstant` で定義された値の使用箇所を AST 変換時に置換。VM 命令列を生成する前に冗長な変数参照を除去
- **関連実装**: `packages/compile/src/codegen-fold-optimize.lisp` の `optimize-ast` が VM 命令生成前に `ast-call`/`ast-progn`/`ast-let`/`ast-if`/`ast-lambda`/`ast-defun`/`ast-defclass` などを再帰最適化し、`packages/compile/src/codegen-fold.lisp` の `%fold-ast-binop` と `packages/compile/src/codegen-fold-eval.lisp` の `%evaluate-ast` / `%compile-time-eval-call` で算術式と compile-time call を畳み込む。`string-length` は `%compile-time-eval-call` の special case、`defconstant` 置換は `cl-cc/expand:*constant-table*` を参照して lexical shadowing を回避する。回帰テスト: `packages/compile/tests/codegen-fold-tests.lisp`, `packages/compile/tests/codegen-tests.lisp`, `packages/compile/tests/codegen-core-tests.lisp`, `packages/compile/tests/codegen-clos-tests.lisp`
- **根拠**: GCC fold-const / SBCL constant-folding。早期の定数畳み込みで後段パスの入力を単純化
- **難易度**: Easy

---

### Phase 72 — CL 宣言系最適化（実装済み）

#### FR-360: `declare (type ...)` / `the` による型ナローイング ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/type/src/inference.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `declare type` と `the` アノテーションを型推論の外部入力として登録。該当変数・式に型を付与し (1) 型チェック命令 (`vm-integer-p`) の除去、(2) fixnum 特化算術パスへの誘導、(3) CLOS ディスパッチの型特化を適用。`(the fixnum x)` は実行時アサーション (`vm-check-type`) + 型情報の 2 役
- **根拠**: SBCL type declarations / GHC type signatures。コンパイラへの型ヒントは最も費用対効果が高い最適化
- **難易度**: Medium

#### FR-361: `declare (inline/notinline)` 処理 ✅

- **対象**: `packages/expand/src/macros-runtime-support.lisp`, `packages/compile/src/codegen-functions-emit.lisp`, `packages/compile/src/codegen-core-let-emit.lisp`, `packages/optimize/src/optimizer-inline-cost.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `(declare (inline f))` → `f` の呼び出しサイトでヒューリスティックを無視して強制インライン。`(declare (notinline f))` → インライン禁止フラグを設定（プロファイリング・デバッグ用）。`(declaim (inline f))` / `(declaim (notinline f))` はグローバルに適用
- **関連実装**: `packages/expand/src/expander-data.lisp` に global registry `*declaim-inline-registry*` を追加し、`packages/expand/src/macros-runtime-support.lisp` が `declaim` から `(inline ...)` / `(notinline ...)` を登録する。`packages/compile/src/context.lisp` の helper 群が pending/local/global policy を保守的に merge し、`packages/compile/src/codegen-functions-emit.lisp` と `packages/compile/src/codegen-core-let-emit.lisp` が `vm-closure` の `:inline-policy` へ反映する。`packages/optimize/src/optimizer-inline-cost.lisp` は `:notinline` を最優先で拒否し、`:inline` 時も既存の structural safety check を維持したまま cost threshold のみ無視する。回帰テスト: `packages/expand/tests/macros-runtime-support-tests.lisp`, `packages/compile/tests/codegen-functions-tests.lisp`, `packages/compile/tests/compiler-tests-extended-stdlib.lisp`, `packages/optimize/tests/optimizer-strength-inline-tests.lisp`, `packages/optimize/tests/optimizer-inlining-tests.lisp`
- **根拠**: ANSI CL 3.3.4 / SBCL `inline` declaration。ユーザの明示的インライン指示は最適化の重要なヒント
- **難易度**: Easy

#### FR-362: `declare (dynamic-extent)` スタック割り当て ✅

- **対象**: `packages/compile/src/codegen-functions-params.lisp`, `packages/compile/src/codegen-functions-emit.lisp`, `packages/compile/src/codegen-core-let-emit.lisp`, `packages/vm/src/vm-dispatch.lisp`, `packages/vm/src/vm-state-init.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `(declare (dynamic-extent x))` でオブジェクト `x` のライフタイムが現在のフォームのダイナミックエクステントに限定されると宣言。エスケープ解析 (FR-007) の静的版。cons・クロージャ・ベクタをスタック上に割り当て可能。`&rest` リストの `dynamic-extent` 宣言でヒープ割り当てゼロ化
- **関連実装**: `packages/compile/src/codegen-functions-params.lisp` の `dynamic-extent-declared-p` / `rest-param-stack-alloc-p`、`packages/compile/src/codegen-functions-emit.lisp` の `:rest-stack-alloc-p` 伝播、`packages/vm/src/vm-dispatch.lisp` と `packages/vm/src/vm-state-init.lisp` の `vm-build-list` stack allocation path。cons / closure / vector 相当の let 束縛は `packages/compile/src/codegen-core-let-emit.lisp` の noescape cons / closure / array bindings でヒープ命令を省略する。回帰テスト: `packages/compile/tests/codegen-functions-params-tests.lisp`, `packages/compile/tests/codegen-functions-callsite-tests.lisp`, `packages/compile/tests/codegen-core-tests.lisp`, `packages/vm/tests/vm-tests.lisp`
- **根拠**: SBCL `dynamic-extent` / ANSI CL 3.3.4。実測でヒープ割り当て数が 50〜90% 削減されるケースあり
- **難易度**: Medium

#### FR-363: `declare (optimize ...)` quality レベル処理 ✅

- **対象**: `packages/expand/src/macros-runtime-support.lisp`, `packages/compile/src/context.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: `(declare (optimize (speed N) (safety N) (debug N) (space N)))` の各 quality を 0〜3 の範囲でコンパイル設定に反映。`(speed 3)` → 型チェック除去・積極的インライン・全最適化パス有効。`(safety 3)` → 型チェック・配列境界チェック・オーバーフロー検出を維持。`(debug 3)` → インライン抑制・デバッグ情報最大化。`(space 2)` → インライン閾値を縮小
- **関連実装**: `packages/expand/src/macros-runtime-support.lisp` が `compilation-speed` / `debug` / `safety` / `space` / `speed` の quality 名と 0〜3 レベルを検証し、`declaration-optimize-quality` で `declaim` / local `declare` から quality を抽出する。`packages/expand/src/expander-data.lisp` の `*declaim-optimize-registry*` が global policy を保持し、`packages/compile/src/context.lisp` の `%global-optimize-quality` / `%local-optimize-quality` が global/local policy を codegen context へ反映する。`packages/compile/src/codegen.lisp` は `safety` policy で type assertion emission を制御し、`packages/compile/src/codegen-core-let-emit-pass.lisp` は `speed` / `debug` / `space` を inline policy merge に使う。回帰テスト: `packages/expand/tests/macros-runtime-support-tests.lisp`, `packages/compile/tests/compiler-tests-extended-stdlib.lisp`, `packages/compile/tests/codegen-control-tests.lisp`
- **根拠**: ANSI CL 3.3.4 / SBCL optimize policy。quality レベルは最適化・安全性トレードオフの主要制御機構
- **難易度**: Medium

#### FR-364: `define-compiler-macro` フレームワーク ✅

- **対象**: `packages/expand/src/macro.lisp`, `packages/expand/src/expander.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: `define-compiler-macro` でユーザ定義のコンパイラ変換を登録。通常の `defmacro` と異なり呼び出しサイトの型情報・最適化設定にアクセス可能。SBCL の `deftransform` の簡略版として実装。組み込み関数（`+`, `aref`, `length` 等）の呼び出しをコンパイラマクロ経由で型特化版に置換
- **関連実装**: `packages/expand/src/expander-data.lisp` の `*compiler-macro-table*` が compiler macro registry を保持し、`packages/expand/src/macro.lisp` の `register-compiler-macro` / `lookup-compiler-macro` / `compiler-macro-function` / `(setf compiler-macro-function)` が登録・取得 API を提供する。`%compiler-macro-lambda-list-parts` は `&whole` / `&environment` を扱い、`packages/expand/src/macros-stdlib-ansi.lisp` が `define-compiler-macro` を ANSI macro として登録する。`packages/expand/src/expander.lisp` の `compiler-macroexpand-all` と `packages/expand/src/expander-basic.lisp` の `funcall` expansion が call-site で compiler macro を適用し、`packages/compile/src/codegen.lisp` / `packages/pipeline/src/pipeline.lisp` / `packages/repl/src/pipeline-repl-load.lisp` の entry point から同じ展開経路を通る。回帰テスト: `packages/expand/tests/macro-definition-tests.lisp`
- **根拠**: ANSI CL 3.2.2.1 / SBCL `deftransform`。数値計算・コンテナ操作の型特化の主要メカニズム
- **難易度**: Medium

#### FR-365: `defconstant` 全使用箇所での定数置換 ✅

- **対象**: `packages/expand/src/expander.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `defconstant` で定義された名前の参照を、コンパイル時に即値 `vm-const` に置換。EQL 等価な値は全使用箇所でインライン化。`(speed 1)` 以上で有効。CLOS クラス定義の定数スロット初期値にも適用
- **関連実装**: `packages/compile/src/codegen-fold-optimize.lisp` で `optimize-ast` が `cl-cc/expand:*constant-table*` を参照して `ast-var` を `%compile-time-value->ast` に置換し、`ast-let` / `ast-lambda` / `ast-defun` の lexical shadowing を回避。`ast-defclass` の slot `:initform`・`:default-initargs`・`:metaclass` も同パスで再帰最適化。回帰テスト: `packages/compile/tests/codegen-tests.lisp`, `packages/compile/tests/codegen-clos-tests.lisp`, 既存直接 codegen 検証 `packages/compile/tests/codegen-functions-tests.lisp`
- **根拠**: ANSI CL 3.1.2.1.1 / SBCL constant inlining。定数の実行時ルックアップをゼロに
- **難易度**: Easy

#### FR-366: `load-time-value` 最適化 ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/pipeline/pipeline.lisp`
- **内容**: `(load-time-value expr)` はロード時に 1 回だけ `expr` を評価してキャッシュする。コンパイル時に `expr` が副作用なし・定数と判明する場合は `defconstant` 相当にフォールバック。ロード時評価のスロットを `.fasl` に記録し重複評価を防止
- **根拠**: ANSI CL 3.2.2.2 / SBCL `load-time-value` compilation。正規表現コンパイル・ハッシュテーブル初期化のイディオムで使用
- **難易度**: Medium

#### FR-367: `declare (ignore/ignorable)` と DCE 統合 ✅

- **対象**: `packages/compile/src/codegen-core-let.lisp`, `packages/compile/src/codegen-core-let-emit.lisp`
- **内容**: `(declare (ignore x))` で変数 `x` が意図的に未使用と宣言される場合だけ、`let` バインディングの余分な move を省略し、純粋な初期化式は後段 DCE で除去可能にする。`(declare (ignorable x))` は警告抑制のみ（コード生成に影響なし）。
- **関連実装**: `packages/compile/src/codegen-core-let.lisp` の `%ast-let-binding-ignored-p` を `(ignore x)` 専用に制限し、`packages/compile/src/codegen-core-let-emit-pass.lisp` では既存 helper 経由で ignored `let` 束縛だけ extra `vm-move` を省略する。`packages/compile/tests/codegen-core-let-tests.lisp` の `ast-let-binding-ignored-p`、`packages/compile/tests/codegen-core-tests.lisp` の `codegen-let-binding-declaration-controls-own-move` / `codegen-let-ignore-binding-enables-dce-of-unused-initializer` が、`ignore` と `ignorable` の差分および ignored pure initializer の DCE を検証する。
- **根拠**: ANSI CL 3.3.4 / SBCL unused variable analysis
- **難易度**: Easy

---

### Phase 73 — CL ランタイム意味論（実装済み）

#### FR-370: 特殊変数の実装モデル選択 ✅

- **対象**: `packages/vm/src/vm.lisp`, `packages/vm/src/vm-execute.lisp`
- **内容**: 3 つの実装モデルから選択: (1) **Deep Binding** — スレッドごとのバインディングスタックを線形探索。実装簡単だがアクセス O(n)。(2) **Shallow Binding** — シンボルのグローバルセルに直接書き込み＋退避スタック。アクセス O(1) だがスレッド切り替えに O(n) コスト。(3) **Per-thread TLS cell** (SBCL 方式) — `fs:` セグメント (x86-64) のスレッドローカルセルに値を格納。アクセス O(1)・スレッドセーフ。cl-cc の現実装モデルを明文化し性能特性を文書化
- **根拠**: SBCL per-thread bindings / CMUCL shallow binding。特殊変数の実装はマルチスレッドの基盤
- **難易度**: Hard

#### FR-371: CLOS `slot-value` の known-class 最適化 ✅

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: コンパイル時にオブジェクトのクラスが判明している場合、`slot-value` を直接スロットオフセットアクセス (`vm-get-slot` with constant index) に変換。`(slot-value obj 'x)` → `(vm-get-slot obj CLASS-X-SLOT-INDEX)` でハッシュテーブルルックアップをゼロに。`slot-boundp` が常に `t` と証明できる場合は省略
- **根拠**: SBCL `slot-value` VOPs / PCL slot-access optimization。CLOS アクセスのボトルネック解消
- **難易度**: Hard

#### FR-372: `compute-applicable-methods` キャッシュ戦略 ✅

- **対象**: `packages/vm/src/vm-clos.lisp`
- **内容**: ジェネリック関数の呼び出しで観測された引数型の組み合わせをキャッシュ。キャッシュ構造: `(type1 type2 ...) → method-list`。インライン化との連携: キャッシュヒット率が高いメソッドをインライン展開候補に。キャッシュエビクション: LRU + エントリ数上限。クラス再定義時の全キャッシュ無効化
- **根拠**: PCL dispatch cache / SBCL dfun (discriminating functions)
- **難易度**: Hard

#### FR-373: `initialize-instance` の特化コンパイル ✅

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: `(make-instance 'foo :x 1 :y 2)` のクラスがコンパイル時確定の場合、(1) スロット割り当て順序の固定、(2) `initialize-instance` → スロット直接代入列のインライン展開、(3) `:initform` のインライン評価、(4) `shared-initialize` のバイパス。`allocate-instance` の特化版生成
- **根拠**: SBCL `make-instance` compiler macro / PCL constructor optimization
- **難易度**: Hard

#### FR-374: EQL スペシャライザのジャンプテーブル化 ✅

- **対象**: `packages/vm/src/vm-clos-execute.lisp`, `packages/vm/src/vm-dispatch-gf.lisp`, `packages/vm/src/vm-dispatch-gf-multi.lisp`, `packages/runtime/src/runtime-clos.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: EQL スペシャライザ `(eql value)` を多数持つジェネリック関数のディスパッチを、値 → メソッドのハッシュテーブル (または dense な整数ならジャンプテーブル) に変換。線形探索 O(n) → O(1) へ。`(defmethod foo ((x (eql :read))) ...)` の多数定義パターンで効果大
- **根拠**: SBCL EQL specializer optimization / PCL permutation vector dispatch
- **難易度**: Medium
- **検証**: `packages/vm/tests/vm-clos-tests.lisp`（`eql-specializer-dispatch-index`）、`packages/vm/tests/vm-dispatch-gf-multi-tests.lisp`（`gf-multi-single-dispatch-eql-index-hit-precedes-class`, `gf-multi-single-dispatch-eql-index-avoids-linear-scan`）、`packages/runtime/tests/runtime-clos-tests.lisp`（`rt-call-generic-eql-dispatch`, `rt-call-generic-eql-index-precedes-class-fallback`）、`packages/compile/tests/clos-dispatch-tests.lisp`（`clos-eql-specializer`）

#### FR-375: `method-combination` 展開最適化 ✅

- **対象**: `packages/vm/src/vm-clos.lisp`
- **内容**: `standard` method combination での `:around`/`:before`/`:after` チェーンをコンパイル時に静的展開。適用されるメソッドが確定している場合、`call-next-method` を直接呼び出しに置換。`+`/`list`/`append` メソッドコンビネーションのアキュムレータを特化
- **根拠**: SBCL effective method compilation / Gray streams optimization
- **難易度**: Hard

#### FR-376: コンディションシステムの実装方針 ✅

- **対象**: `packages/vm/src/conditions.lisp`
- **内容**: (1) **handler-case** の実装選択: setjmp/longjmp 方式 vs テーブル駆動アンワインド (FR-470) — ハッピーパスコストの比較。(2) **restart オブジェクト**の表現: 動的スコープの restart リストをスレッドローカルスタックで管理。(3) `*debugger-hook*` の fast path: hook が nil の場合のチェックコスト最小化。(4) condition オブジェクトの lazy 構築: `signal` 呼び出し前の条件オブジェクト生成を遅延
- **根拠**: SBCL condition system / ANSI CL chapter 9。コンディションシステムのオーバーヘッドはエラー処理コードの性能に直結
- **難易度**: Hard

#### FR-377: `unwind-protect` cleanup のコンパイル ✅

- **対象**: `packages/vm/src/vm-execute.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: `(unwind-protect body cleanup)` の cleanup フォームを: (1) 正常終了パス: body 直後にインライン展開、(2) 非ローカル脱出パス: アンワインドテーブルに登録 (FR-470 と連携)。cleanup に副作用がない場合は dead cleanup として除去。`multiple-value-prog1` 相当の多値保存コストを最小化
- **根拠**: SBCL `unwind-protect` compilation / ANSI CL 5.2
- **難易度**: Medium

#### FR-378: CL 多値 (values) の実装方針 ✅

- **対象**: `packages/vm/src/vm-execute-mv.lisp`, `packages/compile/src/codegen-values.lisp`, `packages/compile/src/codegen-values-helpers.lisp`
- **内容**: 多値の実装戦略の選択と最適化: (1) **レジスタ渡し**: 2〜4 値なら追加レジスタ (`:R_V1`〜`:R_V3`) に直接格納 (最速)。(2) **スレッドローカルバッファ**: `*multiple-values*` 配列に格納 (現状)。(3) **インライン化**: `(multiple-value-bind (a b) (values x y) ...)` でスタック割り当てを回避 (FR-046 参照)。呼び出し先が単値を期待する場合は `values` の第2値以降を DCE で除去
- **関連実装**: `packages/compile/src/codegen-values.lisp` の `compile-ast(ast-values)` が `vm-values` の `src-regs` に値レジスタ列を渡し、`packages/vm/src/vm-execute-mv.lisp` が `vm-values-list` side-channel に全値を保存して primary value を `dst` に置く。`packages/compile/src/codegen-values-helpers.lisp` の `%compile-mvb-value-registers` は `(multiple-value-bind ... (values ...))` を direct register binding にして `vm-mv-bind` を発行せず、dynamic source だけ `vm-mv-bind` にフォールバックする。`packages/optimize/src/effects.lisp` と `packages/optimize/src/optimizer-tables.lisp` は multiple-values side-channel producer を DCE/constant-folding 対象外にして第2値以降の observable state を保護する。回帰テスト: `packages/compile/tests/codegen-runtime-tests.lisp`, `packages/vm/tests/vm-execute-tests-2.lisp`, `packages/vm/tests/primitives-tests.lisp`
- **根拠**: SBCL multiple values / ANSI CL 5.3
- **難易度**: Medium

#### FR-379: Symbol plist 最適化 ✅

- **対象**: `packages/vm/src/vm.lisp`
- **内容**: (1) 短い plist (≤4 エントリ) は線形探索が最速。`getf` の特化版でハッシュテーブル変換を回避。(2) 長い plist はシンボルのプロパティハッシュテーブルに移行するしきい値を設定。(3) `symbol-plist` アクセスの read barrier: マルチスレッド環境でのアトミックアクセス保証。(4) `defvar`/`defun` が設定するシステムプロパティ（`:function`, `:type` 等）の専用スロット化
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **根拠**: SBCL symbol cell layout / CCL symbol representation
- **難易度**: Medium

#### FR-380: `apply` の既知引数数最適化 ✅

- **対象**: `packages/compile/src/codegen-values.lisp`, `packages/compile/src/codegen-values-helpers.lisp`
- **内容**: `(apply f a b spread)` の末尾 spread 引数がコンパイル時既知の quoted proper list（`nil` を含む固定長リスト）の場合、`vm-apply` ではなく `vm-call` へ lowering してスプレッド処理をゼロにする。`(apply #'+ 1 2 nil)` は `(+ 1 2)` と同等の直接呼び出しへ静的変換される。
- **根拠**: SBCL `apply` optimization / CMUCL apply transformation
- **難易度**: Medium

- **関連実装**: `packages/compile/src/codegen-values.lisp` の `compile-ast(ast-apply)` は、`%apply-argument-plan` で leading/spread を分解し、`packages/compile/src/codegen-values-helpers.lisp` の `%literal-apply-spread-values` が quoted finite proper list と判定できた場合だけ `vm-apply` ではなく `vm-call` を直接生成する。`packages/compile/tests/codegen-functions-callsite-tests.lisp` が quoted `nil` / fixed list の direct-call lowering と improper list fallback を検証し、`packages/compile/tests/compiler-tests-selfhost.lisp` が quoted `nil` の実行結果・評価順・improper list error 維持を実行時に検証する。

---

### Phase 74 — FFI 最適化（実装済み）

#### FR-382: GC セーフポイントの FFI 境界配置 ✅

- **対象**: `packages/vm/src/vm-execute.lisp`, `packages/runtime/src/gc.lisp`
- **内容**: C 関数呼び出しの直前・直後を GC セーフポイントとして登録。C 呼び出し中にミュータースレッドが GC トリガを受け取れるよう、`vm-foreign-call` 命令に safepoint チェックを付加。C 呼び出し中は GC がスタックをスキャンしないよう GC フラグをセット (「GC blocked」状態)
- **根拠**: SBCL `without-gcing` / JNI critical regions。FFI + GC の安全な共存の基盤
- **難易度**: Hard

#### FR-383: GC オブジェクトのピニング ✅

- **対象**: `packages/runtime/src/heap.lisp`, `packages/runtime/src/gc.lisp`
- **内容**: C へポインタを渡す期間中 GC がオブジェクトを移動しないよう「ピン」する。実装: (1) ピン済みオブジェクトのセットを GC に通知し移動対象から除外。(2) `cffi:with-pointer-to-vector-data` 相当のマクロで自動ピン/アンピン。(3) ピン数の増加でコンパクション効率が下がる問題のトレードオフ文書化
- **根拠**: .NET `fixed` statement / Java `GetPrimitiveArrayCritical` / SBCL pinned allocation
- **難易度**: Hard

#### FR-384: 型マーシャリング最小化 ✅

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: CL タグ付き値 ↔ C ネイティブ型の変換コストを最小化。(1) fixnum ↔ `int32_t`: タグシフト 1 命令のみ。(2) float ↔ `double`: NaN-boxing デコード 1〜2 命令。(3) CL 文字列 ↔ `char*`: ゼロ終端バッファの確保コストを `cffi:with-foreign-string` でスコープ化。(4) コンパイル時型確定の場合、実行時型チェックを除去
- **根拠**: SBCL alien type system / CFFI type translations
- **難易度**: Medium

#### FR-385: Callback トランポリン生成 ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: C から CL クロージャを呼び戻す `callback` のトランポリン関数を native コードとして生成。トランポリンの役割: (1) C 呼び出し規約 → CL VM 呼び出しへの引数変換、(2) GC セーフポイントの設定、(3) CL 例外の C への伝播防止。`cffi:defcallback` 相当の実装
- **根拠**: SBCL `alien-callback` / libffi closure。C ライブラリのイベントコールバックで必須
- **難易度**: Hard

#### FR-386: libffi を使わない静的呼び出し規約解決 ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: 現在 `libffi` 経由のFFI 呼び出しを、コンパイル時に引数型・戻り値型が確定している場合は直接 System V AMD64 ABI コードに変換。`libffi` のインタープリタオーバーヘッドをゼロに。`(cffi:foreign-funcall "malloc" :size size :pointer)` → 直接 `CALL malloc` の x86-64 命令列生成
- **根拠**: SBCL alien-funcall direct emission。libffi より 5〜20x 高速
- **難易度**: Hard

---

### Phase 75 — スタックフレーム & ABI（実装済み）

#### FR-388: Frame Pointer Elimination (FPE) ✅

- **対象**: `packages/codegen/src/x86-64-regs.lisp`, `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/aarch64-codegen.lisp`, `packages/codegen/src/aarch64-program.lisp`, `packages/cli/src/args.lisp`, `packages/cli/src/handlers.lisp`
- **内容**: `RBP`/`FP` レジスタをフレームポインタとして使わず汎用レジスタとして解放。スタックフレームを `RSP` 相対アドレスのみで参照。プロローグ/エピローグの `push rbp` / `mov rbp, rsp` / `pop rbp` を除去。`--debug` オプション指定時は FPE を無効化して `lldb`/`gdb` の stack unwind を保証
- **根拠**: GCC `-fomit-frame-pointer` / LLVM default behavior。x86-64 では FPE で 1 レジスタ追加確保
- **検証**: `packages/emit/tests/x86-64-codegen-tests.lisp` の `x86-64-fpe-codegen-target-frees-rbp` / `x86-64-empty-program-minimal-return-byte` / `x86-64-leaf-and-nonleaf-without-spills-share-fpe-layout`、`packages/emit/tests/x86-64-encoding-size-tests.lisp` の `x86-vm-program-default-fpe-allocates-rsp-spill-frame` / `x86-vm-program-debug-opt-out-keeps-rbp-spills`、`packages/emit/tests/x86-64-encoding-tests.lisp` の `x86-mov-memory-displacement-widths`、`packages/emit/tests/aarch64-codegen-tests.lisp` の `aarch64-fpe-codegen-target-frees-x29` / `aarch64-leaf-and-nonleaf-without-spills-share-fpe-layout` / `aarch64-default-fpe-uses-sp-relative-spill-frame` / `aarch64-debug-opt-out-keeps-fp-lr-pair-and-fp-spills`、`packages/cli/tests/args-tests.lisp` の `ARGS-TESTS/CLI-ARGS-BOOL-FLAGS [debug]`、`packages/cli/tests/cli-tests.lisp` の `cli-do-compile-debug-binds-backend-frame-pointer-switches`、`packages/optimize/tests/optimizer-roadmap-backend-tests.lisp` の `optimize-backend-roadmap-promoted-existing-frs-have-specific-evidence` / `optimize-backend-roadmap-audited-frs-have-specific-evidence`
- **難易度**: Medium

#### FR-389: Red Zone 活用 (x86-64) ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **内容**: System V AMD64 ABI の Red Zone (`RSP-128` 以下の 128 バイト) を、リーフ関数のローカル変数格納に活用。プロローグで `SUB RSP, N` を発行せずに `RSP-8`/`RSP-16` 等で直接アドレス指定。シグナルハンドラを使用しない関数に限定（シグナルが Red Zone を破壊するため）
- **根拠**: System V ABI Red Zone specification / LLVM `needsStackRealignment`
- **難易度**: Easy

#### FR-390: Prologue/Epilogue Shrink Wrapping ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/regalloc.lisp`
- **内容**: 現状: callee-saved レジスタの保存/復元を関数入口/出口に配置。改善: 実際にレジスタを使うパス（ホットパス）のみに保存/復元を移動。コールドパス（エラー処理・早期リターン）では使わないレジスタの保存/復元をスキップ
- **根拠**: LLVM `ShrinkWrap` pass / GCC `-fshrink-wrap`。コールドパスのプロローグコストを削減
- **難易度**: Hard

#### FR-391: Stack Probing ✅

- **対象**: `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/aarch64-program.lisp`
- **内容**: スタックフレームが 1 ページ (4096 バイト) を超える関数でスタックオーバーフローを検出するための非破壊プローブ命令列を挿入。x86-64: `OR [RSP-4096], 0` を `PAGE_SIZE` 単位で繰り返し、AArch64: `SUB scratch, SP, #PAGE_SIZE` + `LDUR XZR, [scratch]` で guard page を触る。CL 深い再帰でのサイレントメモリ破壊を防止
- **根拠**: LLVM `StackProber` / GCC `-mstack-probe`。Windows では必須 (Guard Page)
- **難易度**: Easy

#### FR-392: Tail Call ABI フレーム再利用の詳細 ✅

- **対象**: `packages/vm/src/vm-execute.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: 末尾呼び出しでのフレーム再利用の ABI 制約を明文化: (1) 呼び出し先の引数が現在のフレームより少ない場合のシフト操作、(2) 可変長引数 (`&rest`) を持つ末尾呼び出しの特殊処理、(3) callee-saved レジスタの復元タイミング（末尾呼び出し前に復元が必要）、(4) デバッガからのスタックトレース可能性の保証方針
- **根拠**: SBCL tail call frame reuse / R7RS tail call requirements
- **難易度**: Medium

---

### Phase 76 — クロージャ表現の選択（実装済み）

#### FR-394: クロージャ表現モデルの選択と文書化 ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/vm/src/vm.lisp`
- **内容**: 3 つのクロージャ表現モデルの選択基準を明文化: (1) **Flat closure** — 自由変数を配列に直接格納。アクセス O(1)、生成コスト高、キャッシュ効率良好。(2) **Linked closure** — 外側環境へのポインタをチェーン。生成安価、アクセス O(depth)。(3) **Display-based** — 各字句スコープ深さへのポインタ配列。アクセス O(1)、固定オーバーヘッド。cl-cc の現実装（`(register . value)` ペアのリスト）の性能特性を分析して最適モデルへ移行
- **根拠**: Cardelli (1983) "The Functional Abstract Machine" / Appel & Jim (1989)
- **難易度**: Hard

#### FR-395: 環境共有 (Shared Environment) ✅

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: 同一スコープから生成された複数のクロージャが共通の自由変数を持つ場合、環境オブジェクトを共有して重複割り当てを回避。`(let ((x 0)) (list (lambda () x) (lambda () (incf x))))` の 2 クロージャが同一 `x` セルを共有。変更可能変数（`setq` されるもの）はヒープセルを介して共有
- **根拠**: SBCL closure environment sharing / GHC let-no-escape
- **難易度**: Hard

#### FR-396: クロージャサイズの最小化 ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **内容**: Lambda Lifting (FR-160) と連携して、クロージャに格納する自由変数を最小化。自由変数のうち: (1) コンパイル時定数 → クロージャから除去して即値に置換、(2) 関数呼び出しで渡される引数 → クロージャ不要 (lambda lifting)、(3) 不変変数 → クロージャにコピーしてセルのアロケーションを不要に
- **根拠**: GHC closure analysis / MLton closure representation
- **難易度**: Medium

---

### Phase 77 — パターンマッチングのコンパイル（実装済み）

#### FR-398: 決定木方式のパターンマッチングコンパイル ✅

- **対象**: `packages/expand/src/macros-basic.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: `typecase`/`cond`/`case` を決定木 (Decision Tree) にコンパイル。各内部ノードは 1 テストのみ、バックトラック不要。Maranget (2008) の「有用でない行の除去」アルゴリズムを適用。テストの共有：共通プレフィックスを持つパターンのテストを単一ノードに統合してコード重複を排除
- **根拠**: Maranget (2008) "Compiling Pattern Matching to Good Decision Trees" / OCaml pattern matching compiler
- **難易度**: Hard

#### FR-399: マッチング行列最適化 ✅

- **対象**: `packages/expand/src/macros-basic.lisp`
- **内容**: 複数引数パターンの場合（`(cond ((and (typep a 'fixnum) (typep b 'cons)) ...) ...)` 等）、マッチング行列の列の並び替えによって決定木の平均テスト数を最小化。ヒューリスティック: 最多クラスを持つ列を先にテスト
- **根拠**: Baudinet & MacQueen (1985) / GHC `PatternMatch` optimization
- **難易度**: Hard

#### FR-400: 網羅性チェック・冗長アーム検出 ✅

- **対象**: `packages/expand/src/expander.lisp`, `packages/expand/src/macros-basic.lisp`
- **内容**: `ecase`/`etypecase` の網羅性をコンパイル時に静的検証。未処理のケースを警告として報告。冗長なアーム（前のアームに包含される型/値）を検出して警告。CL の型格子（`fixnum ⊂ integer ⊂ number`）を考慮した部分型チェック
- **根拠**: OCaml exhaustiveness check / Rust match exhaustiveness。コンパイル時の論理エラー検出
- **難易度**: Medium

#### FR-401: `typecase`/`ecase` の特化展開 ✅

- **対象**: `packages/expand/src/macros-basic.lisp`
- **内容**: `typecase` のアームが CL の基本型タグ（fixnum, cons, string, symbol, vector 等）に対応する場合、タグ値による分岐に特化。`(typecase x (fixnum ...) (cons ...) (string ...) (t ...))` → タグビット抽出 + ジャンプテーブル。FR-128 の typecase への適用と FR-236 の統合
- **根拠**: SBCL `typecase` compilation / PCL type dispatch
- **難易度**: Medium

---

### Phase 78 — Out-of-SSA & モダン IR 設計（実装済み: FR-403/404/405）

#### FR-403: Out-of-SSA — Parallel Copy 挿入 ✅

- **対象**: `packages/optimize/src/ssa-construction.lisp`, `packages/optimize/src/ssa-phi-elim.lisp`, `packages/optimize/src/cfg-analysis.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: SSA 形式の Phi ノードを除去する際に「並列コピー」を挿入して Lost-Copy Problem を回避。アルゴリズム: (1) 各 Phi ノードの predecessor ブロック末尾に parallel copy を配置、(2) 並列コピーをシーケンシャルコピーに変換（サイクル検出 + temp 使用）。Boissinot et al. (2009) の効率的実装を採用
- **根拠**: Boissinot et al. "Revisiting Out-of-SSA Translation for Correctness, Code Quality and Efficiency" (CGO 2009)
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/ssa-construction.lisp` の `ssa-destroy` が `copies-to-insert` / edge-specific copy table / target landing-pad を使って Phi copy を挿入し、`cfg-split-critical-edges` と連携できる形で CFG edge semantics を保持する。`packages/optimize/tests/ssa-tests.lisp` の `ssa-destroy-places-phi-copies-before-terminator`、`ssa-destroy-keeps-conditional-edge-phi-copy-on-target-edge`、`ssa-round-trip-cases` が配置と round-trip を検証する。

#### FR-404: Lost-Copy Problem & Swap Problem 解決 ✅

- **対象**: `packages/optimize/src/ssa-construction.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: Out-of-SSA 変換の 2 大問題の解決: (1) **Lost-Copy**: `a=b; b=c; (phi a,b)` でコピー先が上書きされる問題 → 正しい依存順序でシーケンシャル化。(2) **Swap Problem**: `a=b; b=a` の循環コピー → 一時レジスタまたは `XCHG` 命令で解決
- **根拠**: Sreedhar et al. (1999) / Briggs et al. (1998)。SSA 変換の正確性に必須
- **難易度**: Hard

- **関連実装**: `packages/optimize/src/ssa-construction.lisp` の `ssa-sequentialize-copies` が destination が source として使われない copy を先に emit し、ready copy が尽きた cycle では `SSATMP` 一時レジスタを挿入して循環を破る。`packages/optimize/tests/ssa-tests.lisp` の `ssa-seq-copies-behavior` が empty / independent copies / swap cycle の挙動を検証する。

#### FR-405: Critical Edge Splitting ✅

- **対象**: `packages/optimize/src/cfg-analysis.lisp`, `packages/optimize/src/cfg.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: CFG の「クリティカルエッジ」（複数の後継を持つブロックから複数の先行を持つブロックへのエッジ）を分割して中間ブロックを挿入。Phi ノードのコピー挿入・LICM・コードモーションで必要。`cfg-split-critical-edges` パス
- **根拠**: CFG の標準前処理。LLVM `BreakCriticalEdges` pass
- **難易度**: Easy

- **関連実装**: `packages/optimize/src/cfg-analysis.lisp` の `cfg-split-critical-edges` は、`vm-jump` / `vm-jump-zero` 終端を必要に応じて landing-pad label へ更新し、`bb-successors` / `bb-predecessors` を再配線する。`packages/optimize/tests/cfg-tests.lisp` の `cfg-critical-edge-splitting-inserts-landing-pad` が block 数増加、元 edge の除去、pad block の jump target を検証し、`packages/optimize/tests/optimizer-roadmap-tests.lisp` が roadmap pipeline への登録を検証する。

#### FR-406: Sea of Nodes IR ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-roadmap-backend.lisp`（設計統合レイヤ）
- **内容**: 制御フローとデータフローを単一のグラフで表現する Sea of Nodes IR。各値はグラフノード、依存関係はエッジ。制御フロー順序は `control` エッジで表現（命令の順序は固定せず）。V8 TurboFan / HotSpot C2 が採用。利点: GVN・CSE・コードモーションが自然に統一、欠点: 実装複雑度高・デバッグ困難
- **根拠**: Click & Paleczny (1995) "A Simple Graph-Based Intermediate Representation" / V8 TurboFan
- **難易度**: Very Hard

#### FR-407: MLIR 多段 IR 統合 ✅

- **対象**: コンパイルパイプライン全体
- **内容**: MLIR (Multi-Level Intermediate Representation) の dialect システムを活用した多段変換パイプライン。高レベル dialect (CL 意味論) → 中間 dialect (CPS/SSA) → 低レベル dialect (LLVM IR / Wasm) の段階的 lowering。dialect 間の変換を `RewritePattern` として登録。クロスプラットフォームバックエンドの統一基盤
- **根拠**: MLIR (Lattner et al., 2020)。LLVM・XLA・Triton・Flang の共通インフラ
- **難易度**: Very Hard

---

### Phase 79 — 命令スケジューリング（実装済み）

#### FR-410: List Scheduling — PreRA ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: レジスタ割り当て前の命令スケジューリング。依存 DAG (def-use チェーン + メモリ依存 + 制御依存) を構築し、クリティカルパス優先のヒューリスティックでトポロジカルソート。目標: (1) 依存チェーンのレイテンシを隠す、(2) レジスタ圧力を増やさない順序の選択。Hu アルゴリズム (NP 困難な問題の多項式近似)
- **根拠**: Gibbons & Muchnick (1986) / LLVM `MachineScheduler`。特に高レイテンシ命令（IMUL, DIV, float ops）の前後でスケジューリング効果が大きい
- **難易度**: Hard

#### FR-411: PostRA 命令スケジューリング ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **内容**: レジスタ割り当て後のスケジューリング。レジスタが物理レジスタに固定されているため自由度は低いが、ロード-使用レイテンシの隠蔽・実行ユニットの並列活用が可能。x86-64 のスーパースカラ実行ポート (ALU ポート 0-5, メモリポート 7-8) を考慮した発行幅最大化
- **根拠**: LLVM `PostRAScheduler` / GCC `sched2`。Out-of-Order CPU でも静的スケジューリングが効果的
- **難易度**: Hard

#### FR-412: Modulo Scheduling (ソフトウェアパイプライニング) ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **依存**: FR-021 (SCEV)
- **内容**: ループ本体の複数イテレーションを overlapping 実行するようにスケジューリング。Initiation Interval (II) を最小化して最大スループットを達成。プロローグ・エピローグの生成。レジスタ変数のモジュロ回転。数値計算ループで 2〜4x の理論スループット向上
- **根拠**: Rau (1994) "Iterative Modulo Scheduling" / GCC `-fmodulo-sched`。VLIW アーキテクチャでは特に重要
- **難易度**: Very Hard

#### FR-413: OOO CPU レイテンシモデル ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: ターゲット CPU のマイクロアーキテクチャパラメータテーブル: 各命令のレイテンシ・スループット・実行ポートの定義。x86-64: Intel P-Core / E-Core / AMD Zen 5 の個別テーブル。AArch64: Apple M4 / Graviton3 テーブル。スケジューラがこのテーブルを参照して命令順序を決定。`./cl-cc --target cpu=znver5` で CPU 種別を指定
- **根拠**: LLVM `TargetSchedModel` / uops.info レイテンシデータベース
- **難易度**: Medium

#### FR-414: Hazard Recognizer ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **内容**: スケジューリング中のハザード（依存違反・構造ハザード）をリアルタイム検出するステートマシン。(1) RAW (Read-After-Write): 前命令の結果を次命令が使用。(2) WAW (Write-After-Write): 同一出力先への連続書き込み。(3) Resource Hazard: 同一実行ユニットへの競合。ハザード検出時はバブル (NOP) 挿入またはスケジュール順序変更
- **根拠**: LLVM `HazardRecognizer` / GCC hazard recognizer
- **難易度**: Medium

---

### Phase 80 — 命令選択 (ISEL)（実装済み）

#### FR-416: BURS — Bottom-Up Rewriting System ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: ツリー形式の IR を命令テンプレートでパターンマッチし最小コスト被覆を求める BURS 法。各 VM 命令に対応するターゲット命令テンプレート（コスト付き）を登録。Aho-Johnson (1976) の動的計画法で最適マッチを計算。例: `(add (load x) (load y))` → `ADD reg, [mem]` が `MOV + ADD` より低コスト
- **根拠**: Aho et al. (1989) "Code Generation Using Tree Matching and Dynamic Programming" / iburg
- **難易度**: Hard

#### FR-417: SelectionDAG ベース ISEL ✅

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: 基本ブロック内の命令を DAG（有向非巡回グラフ）に変換し、DAG 全体を対象に命令選択を実施。DAG レベルでの最適化（定数畳み込み・共通部分式除去・ストア-ロード転送）を ISEL と統合。LLVM の SelectionDAG に相当する実装
- **根拠**: LLVM SelectionDAG / GCC RTL combiner。DAG スコープの最適化で個別命令選択より高品質なコード生成
- **難易度**: Very Hard

#### FR-418: GlobalISel — グローバル命令選択 ✅

- **対象**: コンパイルパイプライン
- **内容**: 基本ブロック単位ではなく関数全体を対象とした命令選択。`G_ADD`/`G_LOAD` 等の汎用命令から `ADD32rr`/`MOV64rm` へのグローバル変換。型情報を命令選択フェーズまで保持してより精密な変換を実現。LLVM 8.0 以降の推奨 ISEL フレームワーク
- **根拠**: LLVM GlobalISel / Apple の Objective-C コンパイラでの採用
- **難易度**: Very Hard

#### FR-419: Instruction Lowering パス群 ✅

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: 高レベル VM 命令をターゲット命令に段階的に変換する lowering パス群: (1) **Legalization**: ターゲットがサポートしない型・操作を代替操作に展開（例: 128-bit 整数 → 2×64-bit 操作）。(2) **Calling Convention Lowering**: 関数引数・戻り値をレジスタ/スタックに配置。(3) **Target Intrinsic Lowering**: 組み込み操作をターゲット固有命令に変換
- **根拠**: LLVM legalization framework / GCC `expmed`, `expand` passes
- **難易度**: Hard

---

### Phase 81 — スーパーオプティマイゼーション（実装済み）

#### FR-421: STOKE — 確率的スーパーオプティマイゼーション ✅

- **対象**: `packages/optimize/src/optimizer.lisp`（研究フェーズ）
- **内容**: MCMC（マルコフ連鎖モンテカルロ）サンプリングで命令列の最適版を確率的に探索。(1) 現在の命令列から小さなランダム変更（命令置換・挿入・削除・スワップ）を生成、(2) コスト関数（正確性 + 性能）で受容確率を計算、(3) 十分な反復後に最小コスト解を採用。ホットループの内部に限定適用
- **根拠**: Schkufza et al. "Stochastic Superoptimization" (ASPLOS 2013)。GCC/LLVM が見逃す非直感的変換を発見
- **難易度**: Very Hard

#### FR-422: Souper — LLVM IR スーパーオプティマイザ ✅

- **対象**: `packages/optimize/src/optimizer.lisp`（研究フェーズ）
- **内容**: IR のサブグラフを SMT ソルバ (Z3/KLEE) に与え、意味的に等価でより安価な代替式を自動合成。`(and (add x 1) #xFF)` より効率的な等価式を自動発見。発見したルールを `*peephole-rules*` または E-graph ルール (FR-351) として登録
- **根拠**: Regehr et al. "Souper: A Synthesizing Superoptimizer" (2018)
- **難易度**: Very Hard

#### FR-423: 変換の正確性検証 (Alive2 方式) ✅

- **対象**: `packages/optimize/src/optimizer.lisp` の各最適化パス
- **内容**: 各最適化パスの変換規則を「コンパイル前後の IR が全入力に対して等価か」を SMT ソルバで検証。LLVM Alive2 の手法を VM 命令レベルに適用。新しいペアホールルール追加時に自動で正確性証明を実行。CI パイプラインに組み込んで回帰防止
- **根拠**: Lopes et al. "Alive2: Bounded Translation Validation for LLVM" (PLDI 2021)
- **難易度**: Very Hard

#### FR-424: ペアホールルール自動完備化 ✅

- **対象**: `packages/prolog/src/prolog.lisp` (`*peephole-rules*`)
- **内容**: 既存のペアホールルール集合が「等価変換として完備か」を形式的に検証。Knuth-Bendix 完備化アルゴリズムを用いてルール間の重複・不足を検出し新しいルールを自動生成。E-graph (FR-350) の rewrite rules と統合して適用
- **根拠**: Knuth-Bendix completion / Metatheory.jl (Julia の項書き換え系)
- **難易度**: Very Hard

---

### Phase 82 — 形式検証・証明支援（実装済み）

#### FR-426: Refinement Types / Liquid Types ✅

- **対象**: `packages/type/src/inference.lisp`
- **内容**: 値の述語を型に埋め込む Refinement Type: `{v:fixnum | v > 0}` (正の整数)、`{v:vector | length(v) = n}` (長さ n のベクタ)。SMT ソルバで述語の充足性を検証。配列境界チェック除去 (FR-039)・ゼロ除算除去・型チェック除去の精度を向上。`(declare (type (integer 0 255) x))` の ANSI CL 区間型とも統合
- **根拠**: Rondon et al. "Liquid Types" (PLDI 2008) / Flux (Rust Refinement Types, 2022)
- **難易度**: Very Hard

#### FR-427: Separation Logic によるメモリ安全性証明 ✅

- **対象**: `packages/runtime/src/heap.lisp`, `packages/runtime/src/gc.lisp`
- **内容**: ヒープ領域の所有権を Separation Logic のフレームワークで形式化。GC のマーク・スイープアルゴリズムが「生存オブジェクトを移動しない」「到達不能オブジェクトのみ回収する」ことを証明。`*` (分離積) で独立したヒープ領域を合成して証明をモジュール化
- **根拠**: Reynolds "Separation Logic" (LICS 2002) / Iris (Coq)。GCの正確性証明は言語の信頼性基盤
- **難易度**: Very Hard

#### FR-428: Translation Validation (変換検証) ✅

- **対象**: `packages/optimize/src/optimizer.lisp` 各パス
- **内容**: 各最適化パスの適用後に「変換前後の IR が観測可能な動作について等価か」を検証器が確認。全入力に対する等価性（FR-423）より弱い「有界検証」でも有用。パス単位で有効化・無効化できるデバッグオプション `./cl-cc compile --validate-passes`
- **根拠**: Pnueli et al. "Translation Validation" (TACAS 1998) / CompCert validation
- **難易度**: Hard

#### FR-429: 型安全性の形式証明 ✅

- **対象**: `packages/type/src/inference.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: 型システムの健全性（Progress + Preservation）の形式証明。Progress: well-typed な式はスタックしない（値か評価可能）。Preservation: 評価ステップを踏んでも型が変わらない。証明は Coq / Agda / Lean で記述し定期的に更新。型推論アルゴリズムの完全性の証明
- **根拠**: Wright & Felleisen (1994) "A Syntactic Approach to Type Soundness"
- **難易度**: Very Hard

#### FR-430: 抽象解釈の健全性証明 ✅

- **対象**: `packages/optimize/src/optimizer.lisp` 各解析パス
- **内容**: 各データフロー解析（定数伝播・範囲解析・null チェック除去）の実装が対応する抽象ドメインの格子規則を遵守していることを検証。抽象意味論 vs 具体意味論の Galois 接続の確認。健全でない解析（過剰な最適化）によるバグを防止
- **根拠**: Cousot & Cousot (1977) Abstract Interpretation。コンパイラの正確性の理論的根拠
- **難易度**: Very Hard

---

### Phase 83 — 数値表現の多様性（実装済み）

#### FR-432: BFLOAT16 / FP16 / TF32 サポート ✅

- **対象**: `packages/vm/src/primitives.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: ML ワークロード向け低精度浮動小数点の対応。(1) **BFLOAT16**: IEEE 754 single の指数部 8-bit + 仮数部 7-bit。`VCVTNE2PS2BF16` / `VDPBF16PS` (AVX-512 BF16) のエミット。(2) **FP16**: IEEE 754 half precision。`VCVTPH2PS` / `VCVTPS2PH` 変換命令。(3) **TF32**: NVIDIA AMX/Tensor Core 向け 19-bit 形式
- **根拠**: Intel AMX-BF16 / ARM BF16 拡張 (v8.6-A)。LLM 推論の数値演算コアに必要
- **難易度**: Hard

#### FR-433: Posit 数システム ✅

- **対象**: `packages/vm/src/vm-numeric.lisp`
- **内容**: IEEE 754 の代替数値形式 Posit (Gustafson 2017) のサポート。Posit の特徴: (1) NaN / Inf なし（例外なし演算）、(2) ゼロ近傍で高精度・大数で低精度（動的精度）、(3) Quire（512-bit 累積器）による正確な内積計算。数値計算・科学シミュレーションでの IEEE 754 代替として 2024 年に HPC で採用増
- **根拠**: Posit arithmetic standard (posithub.org) / SoftPosit ライブラリ
- **難易度**: Hard

#### FR-434: 10 進浮動小数点 (IEEE 754-2008 Decimal) ✅

- **対象**: `packages/vm/src/vm-numeric.lisp`
- **内容**: IEEE 754-2008 の decimal64 / decimal128 形式の演算サポート。金融計算での `0.1 + 0.2 ≠ 0.3` 問題を回避。Intel DFP ライブラリまたは BID (Binary Integer Decimal) ソフトウェア実装の統合。`(declare (type decimal64 x))` アノテーションで特化
- **根拠**: IBM `decNumber` / Intel DFP library。金融・会計システムでの正確な十進演算の要件
- **難易度**: Hard

#### FR-435: Kahan 補償加算 / 正確な浮動小数点 ✅

- **対象**: `packages/vm/src/vm-numeric.lisp`, `packages/optimize/src/optimizer.lisp`
- **内容**: 浮動小数点の累積誤差を補償する Kahan アルゴリズムを `reduce #'+` / `sum` に自動適用。FMA (Fused Multiply-Add) を使った 2-product 演算による正確な内積計算。`(declare (optimize (float-accuracy 3)))` quality で有効化。`-ffast-math` 相当の非結合的最適化のオプトイン制御
- **根拠**: Kahan (1965) / Ogita et al. (2005) "Accurate Sum and Dot Product"
- **難易度**: Medium

#### FR-436: FMA 利用方針と精度保証 ✅

- **対象**: `packages/codegen/src/x86-64-codegen.lisp`, `packages/codegen/src/aarch64-codegen.lisp`
- **内容**: `a*b+c` パターンを `VFMADD` (x86-64) / `FMADD` (AArch64) に変換する際の精度変化の扱いを明文化。FMA は中間丸めを省略するため `(a*b)+c` と FMA は一般に異なる結果を生成（より正確）。デフォルト動作・`(declare (optimize (float-accuracy ...)))` での制御方針・IEEE 754 準拠モードでの FMA 無効化
- **根拠**: IEEE 754-2008 §5.4.1 / GCC `-mfma` / Clang `-ffp-contract`
- **難易度**: Medium

---

### Phase 84 — 異種計算 (Heterogeneous Computing)（実装済み）

#### FR-438: GPU オフロードコンパイル (CUDA/ROCm) ✅

- **対象**: 新規 `packages/emit/src/gpu.lisp`
- **内容**: 数値計算ループを GPU カーネルとしてコンパイル。(1) アフィンループ解析 (FR-523) で並列化可能ループを検出、(2) スレッド階層マッピング（グリッド/ブロック/ワープ）、(3) shared memory ティリング最適化、(4) CUDA PTX / ROCm AMDGPU IR 生成。`(parallel-loop :target :gpu ...)` アノテーションで有効化
- **根拠**: MLIR GPU dialect / Triton (OpenAI)。CL の数値計算コードを GPU で実行する実験的パス
- **難易度**: Very Hard

#### FR-439: Apple Neural Engine / Core ML オフロード ✅

- **対象**: 新規 `packages/emit/src/apple-ane.lisp`
- **内容**: 行列積・畳み込み・アクティベーション関数を Apple ANE (Neural Engine) にオフロード。`(matmul a b)` → Core ML `MLModel` 経由の ANE 実行。Python Core ML Tools の CL バインディング生成。M シリーズ Mac での ML 推論の大幅高速化 (CPU 比 10〜100x)
- **根拠**: Apple Core ML / ANE仕様。2025 年以降の Mac でのローカル LLM 推論に必要
- **難易度**: Very Hard

#### FR-440: eBPF コード生成 ✅

- **対象**: 新規 `packages/emit/src/ebpf.lisp`
- **内容**: Linux カーネル内で実行される eBPF プログラムへのコンパイル。制約: (1) ループは検証器が終了証明可能なもののみ、(2) ヒープ割り当て不可（スタックと BPF マップのみ）、(3) 関数呼び出しは BPF helper 関数のみ。ネットワークパケット処理・性能トレース・セキュリティフィルタの CL 実装
- **根拠**: Linux eBPF verifier / BPF CO-RE。2025 年の Linux システムプログラミングの標準ツール
- **難易度**: Very Hard

#### FR-441: OpenMP 自動並列化 ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/expand/src/macros-sequence.lisp`
- **内容**: `dotimes`/`dolist` ループの依存解析後に、独立な反復を OpenMP スレッドプールに分散。`(parallel-for i 0 n :schedule :dynamic ...)` 高レベルマクロの生成。データ依存性チェック: ループキャリー依存がない場合のみ並列化。`#pragma omp parallel for` 相当の CL 構文
- **根拠**: LLVM `-polly-parallel` / GCC `-ftree-parallelize-loops`
- **難易度**: Hard

#### FR-442: FPGA 高レベル合成 (HLS) ✅

- **対象**: 新規 `packages/emit/src/fpga.lisp`
- **内容**: 純粋関数（副作用なし）を FPGA 回路（Verilog/VHDL）に合成。パイプライン挿入・リソース共有・タイミング制約の最適化。Xilinx Vitis HLS / Intel oneAPI 相当の CL フロントエンド。DSP・信号処理・暗号アルゴリズムの FPGA 実装に適用
- **根拠**: LLVM CIRCT / Catapult HLS。FPGA は AI 推論・HFT (High-Frequency Trading) で普及
- **難易度**: Very Hard

---

### Phase 85 — 動的言語ランタイム技術（実装済み）

#### FR-444: Hidden Class / Shape 最適化 ✅

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: CLOS インスタンスに「Shape」（スロット名→インデックスのマッピング）を付与。同一クラス・同一スロット定義のインスタンスは同一 Shape を共有。`slot-value` アクセス時に Shape チェック（MIC、FR-009）でインデックスをキャッシュ。Shape 遷移: `change-class` / `add-slot` 時に新 Shape を派生
- **根拠**: V8 Hidden Class / SpiderMonkey Shapes。動的言語でのプロパティアクセス最適化の基盤
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-shape-descriptor-slots-map-to-stable-offsets` が、shape descriptor のオフセット一貫性を検証する。

#### FR-445: Lazy Shape Transition ツリー ✅

- **対象**: `packages/vm/src/vm-clos.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: Shape は変更のたびに新しい Shape に「遷移」し、遷移ツリーを形成。同じスロット追加順序を持つインスタンスは同一 Shape パスを共有。バックトラック不要の前方遷移のみ。Shape ツリーのキャッシュサイズ上限と eviction 戦略を定義
- **根拠**: V8 Map Transitions / ChakraCore Property Map
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-shape-transition-cache-stores-forward-transitions` が、forward transition キャッシュの保持/検索を検証する。

#### FR-446: IC Patching — コード直接書き換え ✅

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: JIT 生成コードの IC ガード命令を実行時に直接書き換えてキャッシュを更新。モノモーフィック → ポリモーフィックへの昇格時に `JMP` 先を変更。実行中のコードへの安全なパッチ方法: (1) 命令の原子的書き換え（1〜8 バイト境界）、(2) コードページの一時的 `mprotect` + 書き換え + `mprotect`、(3) instruction cache flush（AArch64 では `IC IVAU` + `DSB`）
- **根拠**: V8 code patching / LLVM JIT self-modifying code
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-ic-make-patch-plan-classifies-state-transitions` が patch plan の遷移分類を検証する。

#### FR-447: Inline Polymorphism — V8 方式ディスパッチ ✅

- **対象**: `packages/vm/src/vm-clos.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 同一コールサイトで 2〜4 型が観測される場合（Polymorphic IC の適用範囲）、型チェック + 直接呼び出しを連鎖させて `call_indirect` を除去。`(foo obj)` で `obj` が `ClassA`/`ClassB` の 2 型なら `(if (classA? obj) methodA methodB)` をインライン展開
- **根拠**: V8 Polymorphic Inline Cache / Hölzle et al. (1991)
- **難易度**: Medium

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-inline-polymorphic-dispatch-builds-guard-chain` が guard chain 生成を検証する。

---

### Phase 86 — 並行プログラミングモデルのコンパイル（実装済み）

#### FR-449: async/await → 状態機械変換 ✅

- **対象**: `packages/compile/src/cps.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `async`/`await` スタイルの非同期コードを状態機械（State Machine）に変換。CPS 変換 (FR-004〜FR-006) の特殊ケース。各 `await` ポイントが状態遷移エッジ。状態は enum + union（Rust Future 相当）または closure（JS Promise 相当）で表現。ヒープ割り当てゼロの stackless coroutine として実装可能
- **根拠**: Rust `async/await` desugaring / C# `async/await` state machine。CL の Green thread 実装の基盤
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-build-async-state-machine-builds-linear-transitions` が await→state transition 生成を検証する。

#### FR-450: Coroutine Lowering — Stackful vs Stackless ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/runtime/src/`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: CL の continuation / coroutine の実装選択: (1) **Stackful（独立スタック）**: 各コルーチンが独立したスタック領域を持つ。任意の深さで `yield` 可能だがメモリオーバーヘッドあり。(2) **Stackless（状態機械）**: `yield` は最上位フレームのみ。スタックなしでヒープに状態保存。`call/cc` のセマンティクスとの整合性を維持
- **根拠**: Stackful: Lua coroutines / Go goroutines。Stackless: Python generators / Rust async
- **難易度**: Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-choose-coroutine-lowering-strategy-prefers-stackful-when-needed` が戦略選択の分岐を検証する。

#### FR-451: Channel / CSP 操作の最適化 ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-speculative.lisp`, `packages/optimize/tests/optimizer-pipeline-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: Go の `chan` / Erlang の message passing に相当する channel 操作のコンパイル最適化: (1) **Fast path**: バッファあり channel でキューが空でなければロックフリー atomic で送受信、(2) **Synchronous path**: バッファなし channel でのランデブー同期、(3) **select** 文のジャンプテーブル化。`lparallel` / `chanl` の低レベル実装改善
- **根拠**: Go runtime channel implementation / Erlang BEAM scheduler
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-channel-select-path-classifies-buffered-sync-and-contended-cases` と `optimize-channel-jump-table-select-threshold` が、経路選択と select 閾値判定の回帰を検証する。

#### FR-452: STM（Software Transactional Memory）コンパイル ✅

- **対象**: `packages/optimize/src/optimizer-pipeline-speculative.lisp`, `packages/optimize/tests/optimizer-pipeline-tests.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: `(atomically ...)` ブロック内のメモリ操作をトランザクションログに記録するコード生成。ログのインライン化（関数呼び出しなし）・ホットセル（高頻度アクセス変数）の TLS キャッシュ。コンパイル時に純粋なブロック（FR-152 で pure と判定）のトランザクション化を省略
- **根拠**: GHC STM / Clojure refs + dosync。cl-stm ライブラリの基盤
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-stm-plan-skips-log-for-pure-block` / `optimize-stm-plan-enables-log-for-impure-read-write` が、pure ブロック省略と impure ログ有効化を検証する。

#### FR-453: Lock-Free データ構造のコンパイル支援 ✅

- **対象**: `packages/vm/src/hash.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: ハッシュテーブル・キュー・スタックの lock-free 実装を CAS (Compare-And-Swap) ベースで生成。ABA 問題の自動検出と hazard pointer / epoch-based reclamation の統合。`(defstruct-lock-free foo ...)` マクロが lock-free 版の accessor を自動生成
- **根拠**: Michael & Scott (1996) lock-free queue / Harris (2001) lock-free list
- **難易度**: Very Hard

- **関連実装**: `packages/optimize/tests/optimizer-pipeline-tests.lisp` の `optimize-lockfree-select-reclamation-chooses-policy-from-risk-and-contention` が戦略選択分岐を検証する。

---

### Phase 87 — セキュリティ追加項目（実装済み）

#### FR-455: PIE — Position-Independent Executable 生成 ✅

- **対象**: `packages/binary/src/macho.lisp`, 新規 ELF バックエンド
- **内容**: ASLR（Address Space Layout Randomization）対応のため全コードを相対アドレスで生成。テキストセクション内の絶対アドレス参照を `RIP-relative` (x86-64) / `PC-relative` (AArch64) に変換。Mach-O: `__DATA,__got` セクションへの間接参照。ELF: PLT (Procedure Linkage Table) / GOT (Global Offset Table) 生成
- **根拠**: GCC `-fpic -pie` / LLVM PIC codegen。現代 OS での実行バイナリの標準要件
- **難易度**: Hard

#### FR-456: Pointer Authentication (PAC) — AArch64 ✅

- **対象**: `packages/emit/src/aarch64.lisp`
- **内容**: AArch64 v8.3-A の PAC 命令を使用してポインタに暗号署名を付与。(1) `PACIA`/`PACIB`: 命令ポインタに認証コード付加（ROP 防止）。(2) `AUTIA`/`AUTIB`: 間接 `BR`/`RET` 前に認証検証（改ざん検出）。(3) `RETAA`: 認証付き `RET`。Apple M シリーズ・Qualcomm Snapdragon 8 Gen3 以降で使用可能
- **根拠**: ARM PAC specification / Apple iOS/macOS の必須セキュリティ機能
- **難易度**: Medium

#### FR-457: Constant-time Programming 最適化 ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: サイドチャネル攻撃（タイミング攻撃・キャッシュ攻撃）を防ぐ定時間実行コードの生成。(1) 条件分岐を `CMOV` に置換してブランチ予測タイミングを除去、(2) 秘密値のキャッシュアクセスパターンを均一化（テーブルルックアップを避ける）、(3) `(declare (optimize (safety-against-timing-attacks t)))` 宣言で有効化
- **根拠**: ct-verif / binsec。暗号実装・認証コードの必須要件
- **難易度**: Hard

#### FR-458: MDS / Spectre v3-v4 緩和 ✅

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **内容**: Spectre v1 (bounds check bypass) / v2 (branch target injection, FR-316 Retpoline) に加えた追加緩和: (1) **Spectre v4** (Speculative Store Bypass): ストアの投機的フォワーディングをブロックする `LFENCE` の選択的挿入。(2) **MDS** (Microarchitectural Data Sampling): `VERW` 命令によるバッファクリア（カーネル/ユーザ切り替え時）
- **根拠**: Intel MDS white paper / MSRC advisories
- **難易度**: Hard

#### FR-459: FORTIFY_SOURCE 相当の境界チェック強化 ✅

- **対象**: `packages/vm/src/strings.lisp`, `packages/vm/src/list.lisp`
- **内容**: バッファ長が判明している場合に境界チェック付き版の関数を自動選択。`(copy-seq str)` でコピー先のサイズが静的確定の場合、実行時チェックを静的アサートに変換。`(subseq str 0 n)` で `n > (length str)` がコンパイル時に検出可能なら警告。`glibc _FORTIFY_SOURCE=2` に相当
- **根拠**: GCC `-D_FORTIFY_SOURCE=3` / Clang SafeStack。バッファオーバーフローの静的・動的検出
- **難易度**: Medium

---

### Phase 90 — ツーリング・IDE 統合（実装済み）

#### FR-461: Language Server Protocol (LSP) サポート ✅

- **対象**: 新規 `src/tools/lsp-server.lisp`
- **内容**: `textDocument/definition`（定義ジャンプ）、`textDocument/hover`（推論型の表示）、`textDocument/inlayHints`（型アノテーション）、`textDocument/diagnostics`（コンパイルエラー）、`textDocument/completion`（シンボル補完）の実装。既存の型推論 (FR-426) と expander (FR-364) の情報を LSP プロトコルで公開
- **根拠**: Language Server Protocol v3.17 (Microsoft 2016)。VS Code / Emacs LSP / Neovim が標準でサポート
- **難易度**: Hard

#### FR-462: Flame Graph 生成 ✅

- **対象**: `src/tools/profiler.lisp`
- **内容**: サンプリングプロファイラ（`SIGPROF` + スタックウォーク / Linux `perf` / macOS `dtrace`）のデータを Brendan Gregg の Flame Graph 形式（SVG）に変換。(1) スタックフレームの CL シンボル名への変換（DWARF FR-330 情報を使用）、(2) hot path の視覚化、(3) `./cl-cc profile --flamegraph output.svg ./cl-cc selfhost`
- **根拠**: Brendan Gregg "Flame Graphs" (2011) / py-spy / async-profiler
- **難易度**: Medium

#### FR-463: Compiler Explorer 互換出力 ✅

- **対象**: `packages/cli/src/main-dump.lisp`, `packages/cli/src/main-utils.lisp`, `packages/cli/src/args.lisp`, `packages/cli/src/handlers.lisp`, `packages/pipeline/src/pipeline.lisp`
- **内容**: `./cl-cc compile --dump-ir PHASE FILE` で各コンパイルフェーズの IR をテキストで出力。フェーズ: `ast`, `cps`, `ssa`, `vm`, `opt`, `asm`。Compiler Explorer（godbolt.org）互換の色付きアセンブリ出力。`./cl-cc compile --dump-ir PHASE --annotate-source FILE` または `--trace-emit --annotate-source` でダンプ出力にソース位置コメントを付与
- **根拠**: Compiler Explorer / LLVM `-print-after-all`。コンパイラの透明性と教育的価値
- **難易度**: Easy
- **検証根拠**:
  - 実装: `packages/cli/src/main-dump.lisp` — `%dump-ir-phase` が 6 フェーズ (`:ast`, `:cps`, `:ssa`, `:vm`, `:opt`, `:asm`) を `*ir-phase-dump-fns*` テーブルから検索し、各 `%dump-{phase}-phase` 関数へディスパッチ。ANSI 色付き ASM 出力、`--annotate-source` によるソース位置コメントに対応。`packages/cli/src/handlers.lisp` と `packages/pipeline/src/pipeline.lisp` は real CLI file path でも通常の sexp → macro expansion → lowering 経路を維持しつつ、最終 top-level AST に source metadata を補完する。
  - API シンボル: `%dump-ir-phase`, `%dump-ast-phase`, `%dump-cps-phase`, `%dump-ssa-phase`, `%dump-vm-phase`, `%dump-opt-phase`, `%dump-asm-phase`, `*ir-phase-dump-fns*`, `*ir-phases*`
  - テスト: `packages/cli/tests/main-dump-tests.lisp` — `cli-dump-ir-phase-dispatches-all-phases`, `cli-dump-ir-phase-annotate-source-writes-comment-for-ast`, `cli-dump-ir-phase-annotate-source-writes-comment-for-vm-and-opt`, `cli-dump-ir-phase-asm-output-is-ansi-colored`, `cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location`, `cli-dump-ir-phase-phase-table-covers-all-recognized-phases`, `cli-dump-ir-phase-invalid-signals-error`。`packages/cli/tests/cli-tests.lisp` — `cli-real-file-dump-ir-annotation-preserves-source-location`, `cli-do-compile-dump-ir-annotate-source-preserves-real-file-location`, `cli-do-compile-dump-ir-annotate-source-macro-forms-preserve-real-file-location`

#### FR-464: Reproducible Builds（再現可能ビルド） ✅

- **対象**: コンパイルパイプライン全体
- **内容**: 同一入力から常に同一バイナリを生成。(1) バイナリへのタイムスタンプ埋め込みを禁止（または `SOURCE_DATE_EPOCH` で制御）、(2) ビルドパスを除去（`/Users/take/...` を相対パスに）、(3) ハッシュ計算や辞書順に依存する処理の決定論的化、(4) FASL キャッシュの内容ハッシュによる検証
- **根拠**: Reproducible Builds project (debian.org/reproducible-builds)。セキュリティ監査・コンテナイメージの検証に必要
- **難易度**: Medium

#### FR-465: Build Cache（ccache 相当） ✅

- **対象**: `packages/pipeline/pipeline.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: コンパイル入力（ソースファイルのハッシュ + コンパイルオプション + 依存ファイルハッシュ）をキーとして、コンパイル結果（FASL / native object）をコンテンツアドレサブルキャッシュに格納。ヒット時はコンパイルをスキップ。`~/.cache/cl-cc/` 以下に格納。`nix develop` 環境での rebuild 削減
- **根拠**: ccache / Bazel remote cache。selfhost のビルド時間削減に直結
- **難易度**: Medium

- **関連実装**: `packages/pipeline/src/pipeline-native.lisp` の cache hit 経路は cached artifact をコピーし、miss 経路のみ `%compile-native-file-source` / codegen / Mach-O write を実行する。`%compile-cache-key` は source・arch・language に加え、`pass-pipeline` / `speed` / `inline-threshold-scale` など artifact bytes に影響する native options を key に含める。`packages/compile/tests/pipeline-native-tests.lisp` の `pipeline-native-cache-key-differs-by-dimension` / `pipeline-native-cache-key-ignores-observability-options` と、`packages/compile/tests/pipeline-native-io-tests.lisp` の `pipeline-native-compile-file-cache-hit-copies-artifact` / `pipeline-native-compile-file-cache-hit-skips-native-compilation` / `pipeline-native-compile-file-cache-key-receives-option-plist` が artifact reuse・compile skip・option-sensitive key を検証する。

---

### Phase 91 — 未定義 FR の定義（実装済み）

#### FR-116: Flow-Sensitive Type Narrowing（フロー感受型ナローイング） ✅

- **対象**: `packages/type/src/inference.lisp`, `packages/optimize/src/optimizer.lisp`
- **参照元**: FR-251 (Abstract Interpretation Framework)
- **内容**: 条件分岐後の基本ブロックで型を絞り込む。`(if (typep x 'fixnum) THEN ELSE)` の THEN ブロックでは `x` を `fixnum` 型として扱う。`(if (null x) ELSE THEN)` の THEN ブロックでは `x` が non-nil と確定。TypeScript / Rust の型ナローイングに相当。FR-041 (Tag Check Elimination) の型システム統合版
- **根拠**: TypeScript narrowing / Rust borrow checker。フロー感受型推論はLisp の型システムを大幅に強化
- **難易度**: Hard

#### FR-184: Weak Hash Table（弱参照ハッシュテーブル） ✅

- **対象**: `packages/vm/src/hash.lisp`, `packages/runtime/src/gc.lisp`
- **参照元**: FR-255 (Hash Consing)
- **内容**: キーが GC から到達不能になったエントリを自動的に除去する弱参照ハッシュテーブル。実装: (1) キーを弱参照セルに格納、(2) GC マーク時に弱参照を「グレー」としてマーク（到達不能なら白のまま）、(3) GC スイープ時に白いキーのエントリを除去。`make-hash-table :weakness :key` / `:value` / `:key-and-value` オプション
- **根拠**: SBCL weak hash tables / Java `WeakHashMap`。メモ化テーブル・intern プールのメモリリーク防止
- **難易度**: Hard

#### FR-246: Weak Hash Table with Finalizers（ファイナライザ統合） ✅

- **対象**: `packages/vm/src/hash.lisp`, `packages/runtime/src/gc.lisp`
- **参照元**: FR-255 (Hash Consing)
- **内容**: FR-184 の弱参照ハッシュテーブルにファイナライザコールバックを統合。キーが GC 対象になった際にファイナライザを非同期で呼び出す。ファイナライザキュー（pending finalizers リスト）の管理。`(make-hash-table :weakness :key :finalizer #'cleanup-fn)` インターフェース。ファイナライザ実行順序の保証（FR-483 参照）
- **根拠**: Java `PhantomReference` / .NET `ConditionalWeakTable`。リソース解放・キャッシュ eviction の自動化
- **難易度**: Hard

#### FR-287: Memory-GC Tiling（キャッシュブロッキング + GC 連携） ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/runtime/src/gc.lisp`
- **参照元**: FR-525 (Polyhedral Schedule Optimization)
- **内容**: 多面体スケジューリング (FR-525) で生成されたタイルの境界を GC セーフポイントとして活用。タイル単位での GC 割り込みにより STW 時間を予測可能に制御。メモリアクセスパターンのタイリングと GC の nursery サイズを連携して設定（タイルサイズ ≤ L1D キャッシュサイズ かつ タイル内割り当て量 ≤ nursery サイズ）
- **根拠**: Blackburn et al. "Ulterior Reference Counting" / GC-aware loop tiling。GC とキャッシュ最適化の同時達成
- **難易度**: Very Hard

---

### Phase 92 — ゼロコスト例外処理（実装済み）

#### FR-470: Table-Driven Unwinding — DWARF EH ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/binary/src/macho.lisp`
- **内容**: CL `handler-case`/`restart-case`/`unwind-protect` のハッピーパスをゼロコストで実装。非ローカル脱出時のみ `.gcc_except_table` セクションを参照してスタックをアンワインド。各関数のアンワインド情報を `__TEXT,__eh_frame` (Mach-O) / `.eh_frame` (ELF) に格納。`_Unwind_RaiseException` / `_Unwind_Resume` の libgcc/libunwind 呼び出し
- **根拠**: Itanium C++ ABI unwinding / GCC `-fexceptions`。ハッピーパスのコスト完全ゼロ化
- **難易度**: Hard

#### FR-471: LSDA — Language Specific Data Area ✅

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: `handler-case` の各 `condition` 型と対応するランディングパッドアドレスを LSDA テーブルに格納。アンワインド時に `__cxa_personality_routine` 相当の CL personality function が LSDA を参照してキャッチ可能なコンディション型かを判定。CL の型階層（FR-116 の型ナローイング情報）を personality function に伝達
- **根拠**: Itanium EH ABI §7 "Language-Specific Data Area" / LLVM `EHStreamer`
- **難易度**: Hard

#### FR-472: Compact Unwind（macOS 専用） ✅

- **対象**: `packages/binary/src/macho.lisp`
- **内容**: macOS の `__TEXT,__unwind_info` セクションに格納する Compact Unwind 形式のアンワインド情報生成。DWARF EH より最大 20x コンパクトで読み込みが高速。Apple の `libunwind` が参照するフォーマット。葉関数は 4 バイト、スタックフレームは 24 バイトのエントリ
- **根拠**: Mac OS X ABI: Mach-O File Format / Apple `libunwind` source。macOS ネイティブバイナリの必須要件
- **難易度**: Hard

#### FR-473: Landing Pad 生成 ✅

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: 例外ハンドラのエントリポイント（ランディングパッド）の生成。(1) **Filter**: 特定のコンディション型のみを catch するガード条件、(2) **Cleanup**: `unwind-protect` の cleanup フォームの実行、(3) **Catch**: `handler-case` のボディの実行。ランディングパッドを関数の末尾のコールドセクションに配置 (FR-035 Hot/Cold Splitting と連携)
- **根拠**: LLVM `landingpad` instruction / GCC `EH_RETURN`
- **難易度**: Hard

---

### Phase 93 — Strictness / Demand Analysis（実装済み）

#### FR-475: Strictness Analysis（強制評価解析） ✅

- **対象**: `packages/type/src/inference.lisp`, `packages/optimize/src/optimizer.lisp`
- **内容**: 関数の各引数が「必ず評価されるか」を後ろ向き解析で判定。`(defun f (x y) (if (> x 0) x y))` — `x` は strict（必ず評価）、`y` は lazy（条件付き評価）。Strict と判定された引数は呼び出し前に評価を強制（thunk 生成を回避）。CPS 変換済み継続の厳格性解析にも適用
- **根拠**: Mycroft (1981) abstract interpretation for strictness / GHC demand analysis
- **難易度**: Hard

#### FR-476: Demand Analysis（需要解析） ✅

- **対象**: `packages/type/src/inference.lisp`, `packages/optimize/src/optimizer.lisp`
- **内容**: 引数の「使われ方」を詳細に解析: (1) **Strictness**: 必ず評価されるか、(2) **Absence**: 全く使われないか（未使用引数の除去）、(3) **Usage count**: 0/1/多回の使用（1回のみなら inline 安全）、(4) **Projection**: タプルの特定フィールドのみ使用。CPS の継続引数への適用でクロージャ引数を削減
- **根拠**: GHC demand analysis (Peyton Jones & Launchbury 1991)。Worker/Wrapper (FR-056) の前段として引数の strictness/absence を使用
- **難易度**: Hard

#### FR-477: Strictness と Worker/Wrapper の統合 ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: FR-475 (Strictness Analysis) の結果を FR-056 (Worker/Wrapper) に渡し、strict な引数を unboxed 形式でワーカー関数に渡す変換を自動化。`(defun sum (acc n) (if (= n 0) acc (sum (+ acc n) (- n 1))))` — `acc`・`n` が strict かつ fixnum と判明 → unboxed fixnum でループ実行
- **根拠**: GHC Worker/Wrapper transformation driven by demand analysis
- **難易度**: Hard

---

### Phase 94 — Join Points（実装済み）

#### FR-479: Join Points（結合点最適化） ✅

- **対象**: `packages/compile/src/cps.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: ローカル関数が常に**末尾呼び出し**される場合、それを Join Point（結合点）とみなす。Join Point は呼び出し側と同一スタックフレームを共有し、クロージャ割り当てゼロで実行。具体的変換: `(labels ((k (x) ...) ...) ... (k val))` で `k` がすべての呼び出しサイトで末尾呼び出しなら、`k` をラベルへの `goto` に変換。Contification (FR-028) より軽量で非 CPS コードにも適用可能
- **根拠**: Maurer et al. "Compiling without Continuations" (PLDI 2017)。GHC 8.0 から標準採用
- **難易度**: Hard

---

### Phase 95 — GC 追加項目（実装済み）

#### FR-481: Safepoint 最適化 ✅

- **対象**: `packages/vm/src/vm-execute.lisp`, `packages/runtime/src/gc.lisp`
- **内容**: GC が STW を要求できる「セーフポイント」の配置最適化。現状: 全命令ループで毎命令チェック → 改善: バックエッジ（ループの先頭）と関数呼び出しのみにセーフポイントを配置。ポーリング方式: メモリロード版（`[safepoint_page]` がページフォルトを起こす）vs レジスタ比較版。ループ中のセーフポイントポーリングコストを 95% 削減
- **根拠**: HotSpot polling page / SBCL GC safepoints。GC ポーリングはタイトなループで 5〜10% のオーバーヘッド
- **難易度**: Hard

#### FR-482: GC Map（精確スタックスキャン） ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/runtime/src/gc.lisp`
- **現状**: ✅ 完了 — 実装・検証・証拠登録済み（詳細は関連実装/検証を参照）。
- **内容**: 各セーフポイントで「どのレジスタ・スタックスロットに CL オブジェクトポインタがあるか」を記録した GC Map を生成。コンパイル済みコードのセーフポイントテーブルに格納。GC が Map を参照して正確にポインタのみをトレース。保守的スキャンのフォールスポジティブによる GC ポーズを排除
- **根拠**: SBCL GC maps / JVM OopMap。精確 GC は conservative GC より 10〜30% 高速
- **難易度**: Very Hard

#### FR-483: Finalizer 実行順序と保証 ✅

- **対象**: `packages/runtime/src/gc.lisp`
- **内容**: CL の `(trivial-garbage:finalize obj cleanup-fn)` に相当するファイナライザの実行保証。(1) GC サイクル中にファイナライザキューに追加、(2) GC 完了後に別スレッド（finalizer thread）で非同期実行、(3) ファイナライザ内での GC トリガの安全な処理、(4) 循環参照を持つオブジェクトのファイナライザ順序（Phantom Reference の概念）
- **根拠**: SBCL finalizers / Java `java.lang.ref.PhantomReference`
- **難易度**: Hard

#### FR-484: Ephemeron（弱参照ペア） ✅

- **対象**: `packages/runtime/src/gc.lisp`, `packages/vm/src/hash.lisp`
- **内容**: key が到達可能である間だけ value を保持する特殊ペア。`(make-ephemeron key value)` / `(ephemeron-value eph)` インターフェース。GC の 2 フェーズ処理: (1) 通常マーク時に ephemeron の key が白なら value もスキャンしない、(2) マーク完了後に白 key の ephemeron の value を nil に。メモ化テーブル・クラスオブジェクトキャッシュ・intern プールの実装基盤
- **根拠**: Hayes (1997) / Boehm ephemerons / .NET `ConditionalWeakTable`。Lisp の identity-based memoization に不可欠
- **難易度**: Hard

#### FR-485: Parallel GC（並列 GC） ✅

- **対象**: `packages/runtime/src/gc.lisp`
- **内容**: STW フェーズ内のマーク・スイープ・コンパクション作業を複数 GC スレッドで並列化。ワークスティーリングキュー（`vm-gc-worklist`）を使った並列マーク。スイープの並列化: ヒープセグメントを GC スレッド数で分割して並列処理。4 コア環境で GC 停止時間を 3〜4x 短縮
- **根拠**: HotSpot ParallelGC / .NET Server GC / Go parallel sweep
- **難易度**: Hard

---

### Phase 96 — Copy-and-Patch JIT（実装済み）

#### FR-487: Copy-and-Patch Compilation（テンプレートベース JIT） ✅

- **対象**: `packages/vm/src/vm-execute.lisp`, コンパイルパイプライン
- **内容**: CPython 3.13 (2023) が採用した新世代 JIT 技術。事前にコンパイルしたネイティブコードの「ステンシル（型抜き）」を実行時にコピーし、定数・アドレスをパッチするだけで JIT コードを生成。JIT コンパイル時間 < 1μs（LLVM JIT の 100x 以上高速）。実装: (1) VM 命令ごとの x86-64/AArch64 テンプレートを `clang -O2` で生成、(2) 実行時にテンプレートをコードバッファにコピー、(3) プレースホルダ（定数・ラベル・グローバルアドレス）を実際の値でパッチ
- **根拠**: Xu et al. "Copy-and-Patch Compilation" (OOPSLA 2021) / CPython 3.13 JIT
- **難易度**: Hard

---

### Phase 97 — サニタイザー（実装済み）

#### FR-489: AddressSanitizer (ASan) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/runtime/src/heap.lisp`
- **内容**: ヒープ・スタックのバッファオーバーフロー・use-after-free・double-free を検出。実装: (1) 全メモリ割り当ての前後に「赤帯（redzones）」を挿入、(2) 全メモリアクセスをシャドウメモリチェック（`MOV [shadow(ptr)], 0` の検証）でラップ、(3) 解放済み領域を poison 状態に設定。Shadow Memory: 8 バイトにつき 1 バイトのシャドウ。`./cl-cc compile --asan` で有効化
- **根拠**: Serebryany et al. "AddressSanitizer" (ATC 2012) / LLVM ASan
- **難易度**: Hard

#### FR-490: MemorySanitizer (MSan) ✅

- **対象**: `packages/compile/src/codegen.lisp`
- **内容**: 初期化前メモリ読み取りを検出。シャドウメモリで各ビットの「定義済み/未割当」状態を追跡。`allocate` → 全ビット「未割当」、`setf slot` → 対応シャドウを「定義済み」、`slot-value` 読み取り前にシャドウを検査。native backend のスタック変数の初期化漏れ検出に特に有効
- **根拠**: Stepanov et al. "MemorySanitizer" (CGO 2015) / LLVM MSan
- **難易度**: Hard

#### FR-491: ThreadSanitizer (TSan) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/runtime/src/heap.lisp`
- **内容**: データ競合（2 スレッドが同期なしで同一メモリを読み書き）を検出。Shadow State Machine: 各メモリロケーションに最近のアクセス履歴（スレッド ID・タイムスタンプ・read/write フラグ）を格納。全メモリアクセスをフックして競合検出。`./cl-cc compile --tsan` で有効化
- **根拠**: Serebryany & Iskhodzhanov "ThreadSanitizer" (WBIA 2009) / LLVM TSan
- **難易度**: Hard

#### FR-492: UndefinedBehaviorSanitizer (UBSan) ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **内容**: UB（integer overflow, null pointer dereference, divide by zero, invalid enum value）を実行時に検出してトラップ。各危険操作の前にチェック命令を挿入。CL semantics では fixnum overflow は明確に定義（bignum 昇格）だが native backend でのネイティブ整数演算に対する UB を検出
- **根拠**: LLVM UBSan / GCC `-fsanitize=undefined`
- **難易度**: Medium

#### FR-493: HWASAN — Hardware-Assisted AddressSanitizer ✅

- **対象**: `packages/emit/src/aarch64.lisp`
- **内容**: AArch64 の MTE（Memory Tagging Extension）を使用した ASan の高速版。ポインタの上位 4〜8 bit をタグとして使用し、メモリ領域のタグと照合。ソフトウェア ASan（2〜3x オーバーヘッド）に対して MTE ASan は 5〜15% のオーバーヘッド。ARMv8.5-A 以降（Apple M2〜、Cortex-X3〜）で使用可能
- **根拠**: LLVM HWASAN / ARM MTE specification
- **難易度**: Hard

---

### Phase 98 — Snapshot / AoT 起動最適化（実装済み）

#### FR-495: Image/Snapshot シリアライゼーション ✅

- **対象**: `packages/cli/src/main.lisp`, コンパイルパイプライン
- **内容**: SBCL `save-lisp-and-die` / Node.js V8 Snapshot に相当する CL ヒープのシリアライゼーション。コンパイル済み関数・マクロ・クラス定義を含む「起動イメージ」をファイルに保存。次回起動時はイメージをメモリにマップするだけで即座に実行可能な状態へ。起動時間 O(コード量) → O(1) に短縮
- **根拠**: SBCL core dumps / Racket places / Node.js `--snapshot-blob`。大規模 CL アプリケーションの起動時間を 10〜100x 改善
- **難易度**: Hard

#### FR-496: Lazy Compilation（遅延コンパイル） ✅

- **対象**: `packages/pipeline/pipeline.lisp`
- **内容**: `defun` 定義時に即座にコンパイルせず、最初の呼び出し時にコンパイルする遅延コンパイル戦略。「インタープリタ版」→「JIT コンパイル版」→「最適化 JIT 版」の多段移行（FR-310 Tiered Compilation と連携）。コンパイル時間をホットパスのみに集中させて起動時間を削減
- **根拠**: SBCL lazy compilation / LuaJIT lazy compilation
- **難易度**: Medium

#### FR-497: Ahead-of-Time 完全コンパイル ✅

- **対象**: `packages/cli/src/main.lisp`, コンパイルパイプライン
- **内容**: `./cl-cc aot` コマンドでプログラム全体を事前に最適化コンパイルしてスタンドアロン実行バイナリを生成。依存関係の完全な静的リンク（Quicklisp ライブラリを含む）。LTO (FR-300/301) を適用した全体最適化。`ecl --compile` / `roswell build` に相当する cl-cc ネイティブ AoT
- **根拠**: ECL native compilation / Roswell binary building。Lisp アプリケーションの配布に必須
- **難易度**: Hard

---

### Phase 99 — データレイアウト変換（実装済み）

#### FR-499: AoS → SoA 変換（Array of Structures → Structure of Arrays） ✅

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/compile/src/codegen.lisp`
- **内容**: `(vector struct-type)` のアクセスパターンを解析し、特定フィールドのみを繰り返しアクセスするループで AoS を SoA に変換。`[(x y z) (x y z) ...]` → `[x x x ...] [y y y ...] [z z z ...]`。SIMD ベクトル化 (FR-229/230) の前提として特定フィールドを連続メモリに集約。アフィン解析 (FR-523) と連携してアクセスパターンを静的判定
- **根拠**: LLVM `StructurizeCFG` / Intel ISPC SoA transformation。数値シミュレーション・ゲーム物理演算で 2〜8x のベクトル化効率向上
- **難易度**: Hard

#### FR-500: Struct Field Reordering（フィールド再配置） ✅

- **対象**: `packages/compile/src/codegen.lisp`, `packages/vm/src/vm-clos.lisp`
- **内容**: CLOS クラスのスロット配置を最適化。(1) 最頻アクセスフィールドをキャッシュライン先頭（オフセット 0〜63 バイト）に配置、(2) 同一ループ内で共にアクセスされるフィールドを同一キャッシュラインにグループ化（False Sharing 防止のためスレッドローカルフィールドは別ライン）、(3) フィールドの自然アラインメントを満たすパディング最小化
- **根拠**: Google `abseil` field-order optimization / JVM Object Layout。キャッシュミス削減でメモリ律速コードを 20〜50% 高速化
- **難易度**: Hard

#### FR-501: Compressed Pointers（圧縮ポインタ） ✅

- **対象**: `packages/runtime/src/heap.lisp`, `packages/vm/src/vm.lisp`
- **内容**: 64-bit ポインタを 32-bit 圧縮形式で格納して GC ヒープの密度を倍増。JVM CompressedOops: 32-bit インデックス × 8（アライン係数）= 最大 32GB のヒープをカバー。CL の NaN-boxing と統合: 現在の 64-bit tagged value のうちポインタ部分を 32-bit に圧縮することでレジスタ上のオブジェクトサイズを削減
- **根拠**: JVM CompressedOops (`-XX:+UseCompressedOops`) / V8 pointer compression。ポインタサイズ削減でキャッシュ利用効率が 2x 向上
- **難易度**: Very Hard

#### FR-502: False Sharing 除去 ✅

- **対象**: `packages/runtime/src/heap.lisp`, `packages/vm/src/vm.lisp`
- **内容**: 異なるスレッドが同一キャッシュライン（64 バイト）内の独立したデータを競合して読み書きする「偽共有」を除去。(1) スレッドローカルなカウンタ・ポインタを 64 バイト境界にアライン（パディング挿入）、(2) GC の世代別カウンタをスレッドローカルバッファにバッチ更新、(3) `alignas(64)` 相当のアノテーションをコンパイラが自動挿入
- **根拠**: Intel / AMD 最適化マニュアル。マルチコアの並行アクセスで偽共有は 10〜100x のスローダウンを引き起こす
- **難易度**: Medium
