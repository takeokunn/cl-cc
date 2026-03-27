# Modern Compiler Optimizations — Requirements & Execution Plan

## Summary

TCO以外の2026年モダンコンパイラ標準最適化を網羅的に実装する。VMオプティマイザ層・ネイティブバックエンド層の両方を対象とし、型推論→codegen接続を軸に効果順で展開する。

---

## 現状 (実装済み)

| 最適化 | 場所 |
|--------|------|
| Constant Folding + 代数的恒等式 | `src/optimize/optimizer.lisp:46-414` |
| Dead Code Elimination (DCE) | `optimizer.lisp:494-513` |
| Common Subexpression Elimination (CSE) | `optimizer.lisp:877-968` |
| Copy Propagation | `optimizer.lisp:427-471` |
| Strength Reduction | `optimizer.lisp:828-873` |
| Function Inlining (≤15命令) | `optimizer.lisp:729-820` |
| Peephole (4ルール) | `src/parse/prolog.lisp:366-428` |
| Multiple Values | `src/vm/vm.lisp:260-291` |
| Virtual Register Allocation | `src/emit/regalloc.lisp` |
| NaN-boxing Tagged Pointers | `src/runtime/value.lisp` |
| TCO | 別sessionで対応中 |

---

## 機能要件

### Phase 1 — VM Optimizer Layer (Quick wins)

#### FR-001: ペアホール最適化ルール拡充

- **対象**: `src/parse/prolog.lisp` の `*peephole-rules*`
- **現状**: 4ルール → 目標30+ルール
- **エンジン変更不要**: ルール追加のみ

追加ルール例:

| パターン | 変換後 | 説明 |
|---------|--------|------|
| `(vm-load R1 X)(vm-load R2 X)` | `(vm-move R2 R1)` | 重複ロード除去 |
| `(vm-add R1 R2 0)` | `(vm-move R1 R2)` | 0加算除去 |
| `(vm-mul R1 R2 1)` | `(vm-move R1 R2)` | 1乗算除去 |
| `(vm-not R1 (vm-lt ...))` | `(vm-ge ...)` | 否定+比較融合 |
| `(vm-const R1 X)(vm-const R1 Y)` | `(vm-const R1 Y)` | 上書き定数除去 |
| `L1→L2→L3` | `L1→L3` | Jump chain短縮 |

#### FR-002: 葉関数最適化 (Leaf Function Optimization)

- **対象**: `src/optimize/optimizer.lisp`
- **検出基準**: 命令列中に `vm-call` / `vm-generic-call` が存在しない関数
- **最適化内容**:
  - セーブ/リストア不要なレジスタの省略
  - フレームプッシュの軽量化フラグ付与
- **実装**: 新パス `opt-pass-leaf-detect` → codegen側フラグ参照

#### FR-003: Loop Invariant Code Motion (LICM)

- **対象**: `src/optimize/cfg.lisp` + `src/optimize/optimizer.lisp`
- **前提**: `loop-depth` フィールドが `cfg.lisp` に存在するが未使用
- **実装手順**:
  1. バックエッジ検出 (逆ポストオーダーでの祖先へのエッジ)
  2. 自然ループ識別 (ヘッダ + バックエッジからの逆可達性)
  3. ループ不変命令の判定: 全入力レジスタがループ外で定義 + `opt-inst-pure-p` が真
  4. ループ前置ブロック (preheader) への移動
- **活用**: 既存 `opt-inst-pure-p` 関数

#### FR-010: Sparse Conditional Constant Propagation (SCCP)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: Constant Folding はある (`opt-pass-fold`) が、条件分岐を跨いだ伝播がない
- **内容**:
  - 到達可能な実行パスのみを解析対象とする (条件分岐の定数化による枝刈り)
  - `opt-pass-fold` の拡張または独立パスとして実装
  - 効果: `(if (= x 0) ... ...)` で x=0 が証明可能なら片方の枝を除去

#### FR-011: Global Value Numbering (GVN)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: CSE (`opt-pass-cse`) はローカルな値番号付け
- **内容**:
  - CFGを跨いだグローバルな値番号付け
  - 支配木を利用した冗長計算除去
  - CSEより広いスコープで重複計算を除去

#### FR-012: Partial Redundancy Elimination (PRE)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: LICM + CSE の一般化
  - ループ不変コード移動と共通部分式除去を統一フレームワークで扱う
  - 「一部のパスでのみ冗長」な計算を最適化 (LICMで扱えないケースを補完)

---

### Phase 2 — 型推論 → Codegen 接続

#### FR-004: 型アノテーション付きコンパイルパス (基盤)

- **対象**: `src/type/inference.lisp` + `src/compile/codegen.lisp`
- **現状**: 型推論は独立して動作、codegen側に型情報が届いていない
- **設計**:
  - `compile-ast` に型環境 `type-env` 引数を追加 (オプショナル、省略時は従来動作)
  - 型環境は `src/type/inference.lisp` の `infer-type` から生成
  - レジスタごとに型タグを付与する `register-type-map` を codegen context に追加
- **影響範囲**: `src/compile/codegen.lisp` の `compile-ast` シグネチャ変更

#### FR-005: Fixnum Fast Path (型特化算術)

- **依存**: FR-004
- **対象**: `src/compile/codegen.lisp` + `src/compile/builtin-registry.lisp`
- **内容**: `+`/`-`/`*`/`<`/`>`/`=` でオペランドが両方 fixnum と判明している場合、型チェック命令を生成しない
- **実装**: `register-type-map` で `:fixnum` タグを確認し `vm-integer-p` チェック命令を省略
- **効果**: SBCLの `(declare (type fixnum x))` 相当の効果をアノテーション由来で実現

#### FR-006: 型伝播による条件分岐特化

- **依存**: FR-004
- **内容**: `(if (numberp x) ...)` のtrueブランチ内で `x` を fixnum として扱う
- **活用**: `src/type/inference.lisp` の `extract-type-guard` が既に型絞り込みを実装済み → codegen側で活用

#### FR-013: 型特化インライン展開 (Specialization)

- **依存**: FR-004, FR-002 (Inlining)
- **内容**:
  - 既存インライン化パス (`opt-pass-inline`) を型情報で拡張
  - 引数型が判明している呼び出し箇所で、型特化版のインライン展開を生成
  - GHCの worker/wrapper 変換に相当
  - 効果: ポリモーフィック関数のモノモーフィック特化

---

### Phase 3 — エスケープ解析 & スタック割り当て

#### FR-007: Escape Analysis

- **対象**: `src/compile/closure.lisp` + 新パス
- **現状**: `find-free-variables` / `find-captured-in-children` が存在
- **拡張内容**:
  - オブジェクト（コンスセル・クロージャ）が関数外に「逃げる」か解析
  - 逃げない場合: ヒープではなくスタック/レジスタへ割り当て
  - エスケープ条件: 他関数への引数渡し、グローバル変数への代入、クロージャキャプチャ
- **実装**: `escape-analysis` パスを optimizer pipeline に追加
- **効果**: GCプレッシャー軽減、コンス割り当てコスト削減

#### FR-014: Scalar Replacement of Aggregates (SROA)

- **依存**: FR-007 (エスケープ解析)
- **内容**:
  - エスケープしないコンスセル・構造体をスカラレジスタに分解
  - `(cons a b)` が外部に逃げない場合、car/cdrをそれぞれ独立したレジスタとして扱う
  - LLVM の SROA パスと同等
  - 効果: ヒープ割り当て完全除去 + GCプレッシャー削減

---

### Phase 4 — ネイティブバックエンド層

#### FR-008: Float Unboxing (ネイティブ層)

- **対象**: `src/runtime/value.lisp` + `src/backend/x86-64-codegen.lisp`
- **既存**: NaN-boxingスキームが完備 (`value.lisp`)、float演算命令が充実 (`vm/primitives.lisp`)
- **内容**:
  - float型と推論されたレジスタはXMMレジスタに直接配置
  - ヒープオブジェクトとして boxしない
- **依存**: FR-004

#### FR-009: Inline Caching for Generic Functions

- **対象**: `src/vm/vm.lisp` の `vm-dispatch-generic-call`
- **現状**: 毎回フルメソッド解決 (`vm-get-all-applicable-methods`)
- **設計**:
  - `vm-generic-call` 構造体に `:ic-cache` スロット追加 (`(cons specializer-key method-closure)`)
  - キャッシュヒット: specializer-key一致 → 直接メソッドクロージャ呼び出し
  - キャッシュミス: フル解決 + キャッシュ更新
  - 無効化: `register-method` 呼び出し時にキャッシュクリア
- **形式**: Monomorphic IC (1エントリ) → 将来 Polymorphic IC (N=4) へ拡張可能

#### FR-015: Block Compilation (モジュールレベルインライン化)

- **対象**: `src/cli/main.lisp` + コンパイルパイプライン
- **内容**:
  - ファイル単位でのクロス関数インライン化
  - `flet`/`labels` の呼び出しを関数境界を跨いでインライン展開
  - SBCLの Block Compilation (`sb-c:*block-compile*`) 相当
  - 効果: ローカル関数の呼び出しオーバーヘッド完全除去

#### FR-016: Dead Store Elimination (DSE)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - 書き込み後に読まれることなく上書き・破棄される変数代入を除去
  - DCE (`opt-pass-dce`) の補完: DCEは「使われない計算」、DSEは「上書きされる書き込み」を対象
  - ループ内での冗長なストア除去に特に有効

---

### Phase 5 — メモリ・GC最適化

#### FR-017: Alias Analysis / Memory Disambiguation

- **対象**: `src/optimize/optimizer.lisp` + `src/optimize/cfg.lisp`
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
- **対象**: `src/runtime/gc.lisp` + codegen
- **内容**:
  - 世代別GCにおいて old→young ポインタ書き込みに必要なwrite barrierを除去
  - エスケープ解析で「新規割り当てオブジェクトへの書き込み」と判明した場合はバリア不要
  - HotSpot C2・GHC GCが同様の最適化を実施
  - 効果: 頻繁なコンス操作ループでのGCオーバーヘッド大幅削減

#### FR-020: Allocation Sinking (割り当てシンキング)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - `(cons a b)` 等の割り当てを可能な限り遅延させる (LICMの双対)
  - 条件分岐で片方のパスしか実行されない場合、そのパスの中へ移動
  - 高速パスでのGCプレッシャーを削減

---

### Phase 6 — ループ高度最適化

#### FR-021: Scalar Evolution (SCEV) / 帰納変数解析

- **依存**: FR-003 (LICM)
- **対象**: `src/optimize/cfg.lisp` + `src/optimize/optimizer.lisp`
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

### Phase 7 — CLOS特化最適化

#### FR-024: 型指定スロットアクセス除去 (Type-Directed Slot Access)

- **依存**: FR-004 (型情報), FR-005 (Fixnum Fast Path)
- **対象**: `src/vm/vm.lisp` のCLOSディスパッチ部
- **内容**:
  - レシーバの型が静的に判明している場合、クラスディスパッチを除去して直接スロットオフセットアクセスを生成
  - `(slot-value obj 'x)` → 型が確定なら直接オフセット load
  - SBCLの VOP (Virtual Operations) に相当
  - FR-009 (IC) との違い: ICは動的キャッシュ、これは静的証明による完全除去

#### FR-025: Dead Argument Elimination

- **対象**: `src/optimize/optimizer.lisp` + Block Compilation (FR-015)
- **内容**:
  - 関数引数が関数内で一切使われない、または常に同じ定数値の場合、引数を削除
  - LLVM の `deadargelim` パスに相当
  - Block Compilation と組み合わせることで最大効果
  - 効果: 呼び出し規約の軽量化、不要なレジスタ退避の除去

---

### Phase 6 — CPS・継続特化最適化 (cl-cc固有・高効果)

#### FR-026: Eta Reduction

- **対象**: `src/compile/cps.lisp` + `src/optimize/optimizer.lisp`
- **内容**:
  - `(lambda (x) (f x))` → `f` へのCPS変換後の冗長ラッパー除去
  - CPS変換が生成する多くの継続ラッパーはeta-reducible
  - GHCコア言語と同様のアプローチ
- **難易度**: Easy

#### FR-027: CPS Beta Reduction / Continuation Sharing

- **対象**: `src/compile/cps.lisp` + `src/optimize/optimizer.lisp`
- **内容**:
  - 継続が一度しか使われない場合、その場でインライン展開 (beta-reduce)
  - クロージャ割り当てと呼び出しオーバーヘッドを完全除去
  - CPS変換が生成するsingle-use継続ラッパーを大幅に削減
- **難易度**: Medium

#### FR-028: Contification

- **対象**: `src/compile/codegen.lisp` + `src/optimize/optimizer.lisp`
- **内容**:
  - `labels` で束縛された関数が常に末尾位置から呼ばれる場合、クロージャではなくローカルラベル(JMP先)に変換
  - クロージャ割り当て・環境設定のオーバーヘッドを完全除去
  - MLKit・Chicken Schemeが実装する主要最適化
  - 証拠: `src/vm/vm.lisp` の vm-closure-object を完全にバイパス
- **難易度**: Medium

#### FR-029: Join Points (共有継続)

- **対象**: `src/compile/codegen.lisp`
- **内容**:
  - `if`/`cond` コンパイル時に複数の分岐が同じ合流点を持つ場合、重複したコードを共有ラベルに統合
  - GHCのJoin Point最適化と同等
  - 現状: 各分岐が独立して継続をコピーするため命令数が膨張する
- **難易度**: Medium

#### FR-030: Known-Call Optimization (既知アリティ直接ジャンプ)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **内容**:
  - 呼び出し先の引数数がコンパイル時に判明している場合、vm-callの汎用アリティチェックをスキップして直接エントリにジャンプ
  - 現状: `vm-call` は常にランタイムアリティ検査を行う (`vm.lisp:603-622`)
  - `labels`・`flet` の局所関数呼び出しはほぼ全てこの最適化が適用可能
- **難易度**: Medium

#### FR-031: One-Shot Lambda / Cardinality Analysis

- **対象**: `src/compile/closure.lisp` + `src/compile/codegen.lisp`
- **内容**:
  - クロージャが正確に一度しか呼ばれない場合、ヒープにクロージャオブジェクトを割り当てない
  - CPS変換後の継続ラッパーの大半は one-shot
  - スタックフレームに直接格納することでGCプレッシャー削減
- **難易度**: Medium

---

### Phase 7 — 制御フロー最適化

#### FR-032: Jump Threading

- **対象**: `src/optimize/optimizer.lisp` + `src/optimize/cfg.lisp`
- **内容**:
  - CFGエッジを跨いで既知の条件を伝播し、冗長な分岐を除去
  - `typecase`/`etypecase`/`cond` の型チェック連鎖に特に有効
  - 例: ある分岐で `x` が fixnum と判明していれば、後続ブロックの fixnum チェックを除去
  - LLVM `JumpThreadingPass` に相当
- **難易度**: Medium

#### FR-033: Case-of-Case

- **対象**: `src/compile/codegen.lisp`
- **内容**:
  - ネストした `cond`/`typecase` で、外側の scrutinee を内側に押し込んで型を絞り込む
  - GHCのcase-of-case変換と同等
  - `(if (consp x) (if (consp (car x)) ...))` で内側を `car x` が cons と確定した状態でコンパイル
- **難易度**: Medium

#### FR-034: If-Conversion (分岐→条件移動)

- **対象**: `src/backend/x86-64-codegen.lisp`
- **内容**:
  - 副作用のない短い if-then-else を `CMOV` 命令(x86-64) / `CSEL`(AArch64) に変換
  - 分岐予測ミスのペナルティを除去
  - fixnum算術の型チェック後の値選択に特に有効
- **難易度**: Medium

#### FR-035: Hot/Cold Splitting

- **対象**: `src/optimize/optimizer.lisp` + バックエンド
- **内容**:
  - エラー処理・型エラー・条件シグナル等の「コールドパス」をホット関数の末尾/別セクションへ移動
  - ホット関数のI-キャッシュ占有を削減
  - 静的ヒューリスティック: `signal`/`error` 呼び出しを含む基本ブロックはコールド
- **難易度**: Medium

#### FR-036: Hot/Cold Code Layout (基本ブロック並べ替え)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/backend/aarch64.lisp`
- **内容**:
  - 頻繁に実行される基本ブロックを連続したアドレスに配置
  - fall-through条件（JZ/JNZ → fall-through is hot path）をヒューリスティックで決定
  - プロファイルなしでも分岐方向ヒューリスティックで効果あり
- **難易度**: Easy

#### FR-037: Call Site Splitting

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - 特定の分岐アームでのみ引数型が確定している間接呼び出しを、その分岐内でインライン化/特化
  - `(funcall fn x)` で fn が型判明している分岐アームでは直接呼び出しへ変換
  - LLVM `CallSiteSplittingPass` に相当
- **難易度**: Medium

---

### Phase 8 — 範囲解析・チェック除去

#### FR-038: Value Range / Interval Analysis

- **対象**: `src/optimize/optimizer.lisp` + `src/type/inference.lisp`
- **内容**:
  - 整数変数の値域 (`[lo, hi]`) をCFGを通じて伝播
  - SCCP (FR-010) が定数を伝播するのと同様に範囲を伝播
  - ループ変数 `i` が `0..n-1` の範囲であることを証明
  - HotSpot C2・LLVM `CorrelatedValuePropagationPass` と同等
- **難易度**: Medium

#### FR-039: Array Bounds Check Elimination (BCE)

- **依存**: FR-038 (範囲解析) or FR-021 (SCEV)
- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **内容**:
  - `(aref v i)` のインデックス `i` が `0 <= i < (length v)` と証明可能な場合、境界検査命令を除去
  - ループ内の `aref` が最も恩恵を受ける
- **難易度**: Medium

#### FR-040: Nil Check Elimination

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - nil チェック後の支配されたブロックで同じ値への再チェックを除去
  - `(if x ...)` の truthy ブランチでは `x` が non-nil と確定
  - Lisp は nil を多用するため効果が大きい
- **難易度**: Easy

#### FR-041: Tag/Type Check Elimination (支配木ベース)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - 型チェック命令 (`vm-integer-p`, `vm-cons-p` 等) が、同じ値に対して支配ブロックで既に実行済みの場合は除去
  - DCE/GVNでは除去できない「条件分岐後の冗長チェック」を対象
  - 現状: `vm-integer-p` が同一値に対して複数回生成されるケースあり (`vm/primitives.lisp:47-52`)
- **難易度**: Medium

---

### Phase 9 — Lisp固有ランタイム最適化

#### FR-042: Dynamic Variable Access Caching

- **対象**: `src/vm/vm.lisp:998-1007`, `src/compile/codegen.lisp`
- **内容**:
  - `(defvar *x* ...)` へのアクセスが毎回 `(gethash name (vm-global-vars state))` を実行している
  - 関数エントリ時にグローバルをレジスタにロード、変更した場合のみ書き戻す
  - 特にループ内での `*x*` アクセスで3-5倍の高速化見込み
- **難易度**: Medium

#### FR-043: Closure Environment Flattening

- **対象**: `src/vm/vm.lisp:19-31, 550-572`
- **内容**:
  - 現状: captured-values を alist `((reg . val) ...)` で格納、復元時に `dolist` で走査
  - 平坦ベクタ化: `captured-regs` = vector + `captured-vals` = vector に変換
  - インデックスアクセスで car/cdr 操作を除去、キャッシュ局所性を改善
- **難易度**: Medium

#### FR-044: Apply Fast Path (既知引数数)

- **対象**: `src/vm/vm.lisp:852-876`, `src/compile/codegen.lisp`
- **内容**:
  - `(apply fn a b (list c d))` パターンを検出し引数リスト構築を除去
  - コンパイル時に引数数が判明している `apply` を `vm-call` と同等に変換
  - 現状: 常に `butlast`/`append` でリスト操作が発生
- **難易度**: Low

#### FR-045: Tail Recursion Modulo Cons (TRMC)

- **対象**: `src/compile/codegen.lisp` + `src/compile/cps.lisp`
- **内容**:
  - `(cons x (f y))` が末尾位置にある場合、アキュムレータ変換で反復に変換
  - `map`/`filter` 相当の再帰リスト構築がO(n)スタック消費しなくなる
  - GCC の `-foptimize-sibling-calls` の Lisp版
- **難易度**: Hard

#### FR-046: Condition System Zero-Cost Fast Path

- **対象**: `src/compile/codegen.lisp:100-146`, `src/vm/conditions.lisp`
- **内容**:
  - 保護フォーム内に `signal` がないことをコンパイル時に証明できる場合、ハンドラ確立命令を省略
  - 現状: `handler-bind` は常に `vm-establish-handler` を emit
  - 静的解析: 保護フォームのcallee一覧にsignal系がなければ省略可
- **難易度**: Medium

#### FR-047: Format String Compile-Time Processing

- **対象**: `src/vm/io.lisp:247-251`, `src/compile/codegen.lisp`
- **内容**:
  - 定数フォーマット文字列 `"~A~%"` をコンパイル時に解析してバイトコードへ変換
  - 実行時の `format` 文字列パースを除去
  - コンパイラ自身のデバッグ出力等で大きな恩恵
- **難易度**: Low

#### FR-048: Hash Table Operation Specialization

- **対象**: `src/vm/hash.lisp:116-140`, `src/compile/codegen.lisp`
- **内容**:
  - `(make-hash-table :test 'eql)` 等でテスト関数をコンパイル時に確定
  - `vm-gethash-eq` / `vm-gethash-eql` / `vm-gethash-equal` の特化命令を生成
  - 現状: `resolve-hash-test` が毎回 case ディスパッチを実行
- **難易度**: Low

#### FR-049: Equality Predicate Specialization

- **対象**: `src/vm/primitives.lisp:11-17`, `src/compile/codegen.lisp`
- **内容**:
  - `eq`/`eql`/`equal` を型情報に基づいて特化
  - fixnum同士: 直接整数比較命令
  - symbol同士: ポインタ比較
  - 型不明時のみ汎用パス
- **難易度**: Medium

---

### Phase 10 — 手続き間最適化

#### FR-050: Interprocedural SCCP (IPSCCP)

- **対象**: `src/optimize/optimizer.lisp` + コール グラフ解析
- **内容**:
  - 関数境界を跨いだSparse Conditional Constant Propagation
  - 呼び出し元で引数が定数の場合、呼び出し先で定数として伝播
  - FR-010 (SCCP) の手続き間版
  - LLVM `IPSCCPPass` に相当
- **難易度**: Hard

#### FR-051: Called Value Propagation / Devirtualization

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - 間接呼び出し (`funcall`/`apply`) の呼び出し先を静的に絞り込む
  - 単一の呼び出し先と証明された場合は直接呼び出しへ変換
  - LLVM `CalledValuePropagationPass` に相当
  - Lispの `funcall` は全てこの最適化の対象
- **難易度**: Hard

#### FR-052: Global DCE (Dead Function/Method Elimination)

- **対象**: `src/optimize/optimizer.lisp` + コンパイルパイプライン
- **内容**:
  - 到達不能な関数・メソッドをリンク時に除去
  - `defun` で定義されたが一切呼ばれない関数を除去
  - コード生成済み命令列から未参照エントリを削除
  - LLVM `GlobalDCEPass` に相当
- **難易度**: Easy

#### FR-053: Partial Inlining (ホットパスのみインライン)

- **依存**: FR-013 (型特化インライン展開)
- **対象**: `src/optimize/optimizer.lisp:729-820`
- **内容**:
  - 現状のインライン化は関数全体を展開 (≤15命令)
  - 早期リターンパスだけをインライン展開し、残りは元の呼び出しへフォールスルー
  - fixnum fast path のみインライン → bignum path は呼び出し継続
  - LLVM `PartialInlinerPass` に相当
- **難易度**: Hard

---

### Phase 11 — 高階関数・ストリーム最適化

#### FR-054: Stream Fusion / Deforestation

- **対象**: `src/expand/macro.lisp` or `src/optimize/optimizer.lisp`
- **内容**:
  - `(mapcar f (remove-if g xs))` → 中間リストを生成しない単一走査に変換
  - コンパイラ自身が `mapcar`/`remove-if`/`reduce` チェーンを多用しているため効果大
  - GHCのbuild/foldr fusion rules と同等のアプローチ
  - 実装: パターンマッチルールとして `*peephole-rules*` または独立パス
- **難易度**: Hard (一般) / Medium (パターン限定)

#### FR-055: Loop Idiom Recognition

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - `(fill vec 0)` → memset相当命令
  - `(copy-seq vec)` → memcpy相当命令
  - ループパターンを認識してランタイムのバルク操作に置き換え
  - LLVM `LoopIdiomRecognizePass` に相当
- **難易度**: Medium

#### FR-056: Worker/Wrapper Transformation (部分適用)

- **対象**: `src/compile/codegen.lisp` + `src/optimize/optimizer.lisp`
- **内容**:
  - 再帰関数を「ラッパー (boxing/unboxing担当)」と「ワーカー (unboxed値で再帰)」に分割
  - 再帰呼び出し間でfloat/fixnumをunboxedのまま引き回す
  - FR-008 (Float Unboxing) の再帰関数への拡張
  - GHCの主要最適化の一つ
- **難易度**: Hard

---

### Phase 12 — アーキテクチャ統合

#### FR-057: MIR Pipeline Integration

- **対象**: `src/emit/mir.lisp` + `src/compile/codegen.lisp`
- **内容**:
  - MIR層 (`src/emit/mir.lisp`) はSSA CFGインフラが完備しているが本番パイプラインから切断されている
  - VM命令 → MIR lowering → MIR最適化 → ターゲット命令生成のパイプラインを接続
  - 接続により: 命令スケジューリング、レジスタ割り当て改善、SSAベース最適化が全て利用可能に
  - 最大30%のネイティブコード性能改善が見込まれる
- **難易度**: Very Hard

#### FR-058: Type Feedback PGO (プロファイルガイド最適化)

- **対象**: `src/vm/vm.lisp` (IC計測) + コンパイルパイプライン
- **内容**:
  - IC (Inline Cache) に型出現頻度カウンタを追加
  - プロファイルデータを第2コンパイルパスにフィードバック
  - 最も頻繁に出現する型を優先してインライン化・特化
  - AOT版PGO: 初回実行→プロファイル収集→再コンパイル
- **難易度**: Hard

---

### Phase 13 — レジスタ割り当て・命令スケジューリング

#### FR-059: Register Coalescing

- **対象**: `src/emit/regalloc.lisp`
- **内容**: `vm-move` 命令のソースと宛先に同一物理レジスタを割り当て、move命令を消去
- **根拠**: 現状のregalloc は多数の `vm-move` を機械語レベルに残す
- **難易度**: Medium

#### FR-060: Register Hints / Preference-Based Allocation

- **対象**: `src/emit/regalloc.lisp`, `src/emit/calling-convention.lisp`
- **内容**: `calling-convention` の `:arg-registers` を割り当て時に優先考慮
- **根拠**: CC定義は存在するが `linear-scan-allocate` が無視している
- **難易度**: Medium

#### FR-061: Graph Coloring Register Allocation (Chaitin-Briggs)

- **対象**: `src/emit/regalloc.lisp`
- **内容**: 干渉グラフ彩色による最適レジスタ割り当て（線形スキャンより高品質）
- **難易度**: Hard

#### FR-062: Rematerialization

- **対象**: `src/emit/regalloc.lisp`
- **内容**: スピルの代わりに `vm-const` 等の安価な命令を使用箇所で再計算
- **難易度**: Easy

#### FR-063: Live Range Splitting

- **対象**: `src/emit/regalloc.lisp`
- **内容**: 長いライブ区間を分割し、ホット領域のみスピルを最小化
- **難易度**: Hard

#### FR-064: Biased Spill Selection (Belady's OPT)

- **対象**: `src/emit/regalloc.lisp`
- **内容**: 次の使用箇所が最も遠い区間を優先スピル（現状は最長末尾を選択）
- **難易度**: Easy

#### FR-065: Caller-Save/Callee-Save Aware Spilling

- **対象**: `src/emit/regalloc.lisp`
- **内容**: call命令を跨ぐ長寿命値をcallee-savedレジスタに優先配置
- **根拠**: `regalloc.lisp:274` 付近にTODOコメントあり
- **難易度**: Medium

#### FR-066: Two-Address Instruction Lowering

- **対象**: `src/backend/x86-64-codegen.lisp`
- **内容**: `dst==lhs` が必要なx86-64命令の前にcopyを挿入してRA時の混乱を防止
- **難易度**: Easy

#### FR-067: Pre-RA List Scheduling

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/backend/aarch64.lisp`
- **内容**: レジスタ割り当て前の命令並べ替えでILPを露出
- **難易度**: Hard

#### FR-068: Post-RA Instruction Scheduling

- **対象**: バックエンド
- **内容**: 物理レジスタ確定後にメモリレイテンシを隠蔽する命令並べ替え
- **難易度**: Hard

#### FR-069: Dependency-Aware Peephole Scheduling

- **対象**: `src/optimize/optimizer.lisp`, `src/parse/prolog.lisp`
- **内容**: Read-after-Writeに基づく命令ペア/トリプルの局所的並べ替え
- **難易度**: Medium

#### FR-070: Return Value in Return Register (NRVO)

- **対象**: `src/emit/regalloc.lisp`, `src/emit/calling-convention.lisp`
- **内容**: 関数の戻り値をABIのリターンレジスタに直接配置し、post-call moveを除去
- **根拠**: CC構造体に `:return-register` 定義あるが割り当て時に未活用
- **難易度**: Easy

#### FR-071: Parameter Register Recycling

- **対象**: `src/emit/regalloc.lisp`
- **内容**: 引数レジスタがdead後にローカル一時変数として再利用
- **難易度**: Easy

#### FR-072: Shrink-Wrapping (Callee-Saved Register)

- **対象**: バックエンド
- **内容**: callee-savedレジスタのsave/restoreを実際の使用箇所の支配点まで遅延
- **難易度**: Hard

#### FR-073: Multiple Values via Registers (≤3 values)

- **対象**: `src/vm/vm.lisp:260-291`, `src/compile/codegen.lisp`
- **内容**: `(values a b)` の静的既知2値返却をリスト割り当てなしのレジスタ渡しに変換
- **根拠**: 現状 `vm-values-list` に毎回リスト生成 (`vm.lisp:714`)
- **難易度**: Medium

---

### Phase 14 — コードサイズ・コード品質最適化

#### FR-074: Function Outlining

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 複数関数間の繰り返し命令列を共有サブルーチンに抽出 (インライン化の逆)
- **根拠**: CLOSディスパッチ列は構造的に類似したものが多い
- **難易度**: Medium

#### FR-075: Tail Merging

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 複数基本ブロック末尾の同一命令列を単一共有ブロックに統合
- **難易度**: Medium

#### FR-076: Identical Code Folding (ICF)

- **対象**: `src/emit/binary/macho.lisp`
- **内容**: バイト列が同一の関数を単一コピーに重複排除
- **難易度**: Hard

#### FR-077: Dead Basic Block Elimination

- **対象**: `src/optimize/optimizer.lisp` + `src/optimize/cfg.lisp`
- **内容**: jump threading後も残存した到達不能ブロックの除去
- **難易度**: Easy

#### FR-078: Block Merging

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 単一後継ブロックを持つブロックの統合でラベルオーバーヘッド削減
- **難易度**: Easy

#### FR-079: Closure Thunk Sharing

- **対象**: `src/compile/codegen.lisp`
- **内容**: コードが同一で環境のみ異なるクロージャを単一コード+環境ポインタに統合
- **難易度**: Medium

#### FR-080: Car/Cdr/Cons Inlining

- **対象**: `src/compile/codegen.lisp`, `src/vm/list.lisp`
- **内容**: `car`/`cdr`/`cons` をVM命令ディスパッチなしの直接レジスタ操作にインライン化
- **根拠**: 現状は `define-simple-instruction` で毎回 `execute-instruction` ディスパッチが発生
- **難易度**: Medium

#### FR-081: Macro Expansion Memoization

- **対象**: `src/expand/expander.lisp`, `src/expand/macro.lisp`
- **内容**: `(form . env) → 展開結果` のweakメモテーブルを追加、同一フォームの再展開を回避
- **根拠**: 両ファイルに "cache" / "memoiz" が存在しない
- **難易度**: Medium

#### FR-082: Slab Allocator for Cons Cells

- **対象**: `src/runtime/heap.lisp`, `src/runtime/gc.lisp`
- **内容**: 固定サイズ(3ワード)のコンスセル専用フリーリストプール
- **根拠**: コンスはLispランタイムで最頻繁な割り当てオブジェクト
- **難易度**: Medium

#### FR-083: Inline Bump Pointer Allocation

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/runtime/heap.lisp`
- **内容**: `rt-gc-alloc` 関数呼び出しを除去し、`young-free` の増分比較をコールサイトにインライン展開
- **根拠**: 現状は割り当てのたびに関数呼び出しが発生
- **難易度**: Medium

---

### Phase 15 — GC・メモリシステム最適化

#### FR-084: Card Table Summarization

- **対象**: `src/runtime/gc.lisp`
- **内容**: カードテーブルの1レベルビットマップサマリを追加し、クリーンカードのスキャンをスキップ
- **根拠**: 現状 `%gc-scan-dirty-cards` は全 `num-cards` を毎回イテレート
- **難易度**: Easy

#### FR-085: Dynamic GC Age Threshold Tuning

- **対象**: `src/runtime/gc.lisp`
- **内容**: `*gc-tenuring-threshold*` をプロモーション率・旧世代占有に基づいて動的調整
- **根拠**: 現状ハードコード値3; 動的チューニングでGC品質向上
- **難易度**: Easy

#### FR-086: Large Object Space (LOS)

- **対象**: `src/runtime/heap.lisp`
- **内容**: 閾値超オブジェクトを独立管理領域に直接割り当てNursery迂回
- **難易度**: Easy

#### FR-087: rt-heap Field Reordering

- **対象**: `src/runtime/heap.lisp`
- **内容**: `young-free` をホットフィールド群 (`young-from-base`, `young-limit`) と同一キャッシュラインに配置
- **難易度**: Easy

#### FR-088: Incremental GC Marking

- **対象**: `src/runtime/gc.lisp`
- **内容**: メジャーGCのマークフェーズをミューテータ作業とインタリーブして一時停止を短縮
- **根拠**: SATB インフラが既に存在するため自然な拡張
- **難易度**: Medium

#### FR-089: Compacting Old Space

- **対象**: `src/runtime/gc.lisp`
- **内容**: マーク後にスライディングコンパクションで旧世代のフラグメンテーション解消
- **難易度**: Hard

#### FR-090: Safepoint Dominance Pruning

- **対象**: `src/emit/mir.lisp`, `src/optimize/optimizer.lisp`
- **内容**: 同一ルートセットを持つ支配されたsafepointを除去
- **根拠**: MIRに `:safepoint` ノードが存在するが冗長性解析なし
- **難易度**: Medium

#### FR-091: Safepoint Hoisting to Loop Back-Edges

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: ループボディのsafepointをバックエッジにのみ移動、ポーリング頻度削減
- **難易度**: Medium

---

### Phase 16 — 制御フロー・比較最適化

#### FR-092: EFLAGS Reuse After CMP (x86-64)

- **対象**: `src/backend/x86-64-codegen.lisp`
- **内容**: CMP命令後のEFLAGSを直接分岐に使用し、bool値のレジスタ化+タグチェックを省略
- **難易度**: Medium

#### FR-093: Jump Table for Dense Integer Case

- **対象**: `src/backend/x86-64-codegen.lisp`
- **内容**: 密なinteger `case`/`ecase` を `JMP [rax*8+table]` の間接ジャンプに変換
- **難易度**: Medium

#### FR-094: Wasm `br_table` for User `case`

- **対象**: `src/emit/wasm-trampoline.lisp` (インフラ既存)
- **内容**: ユーザレベルの `case`/`etypecase` に `br_table` を適用
- **根拠**: trampolineで既に `br_table` を使用、ユーザ向けへの拡張が自然
- **難易度**: Low

#### FR-095: Binary Search for Sparse Case

- **対象**: `src/expand/expander.lisp`
- **内容**: 疎なinteger集合の `case` を線形チェーンでなく二分探索木で生成
- **難易度**: Low

#### FR-096: Power-of-2 Divisor Strength Reduction

- **対象**: `src/vm/primitives.lisp`, `src/optimize/optimizer.lisp`
- **内容**: コンパイル時定数が2の冪の `floor`/`mod` を算術シフト/ビットANDに変換
- **難易度**: Low

---

### Phase 17 — ハードウェア命令活用

#### FR-097: POPCNT / i64.popcnt for logcount

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/emit/wasm.lisp`
- **内容**: ANSI CL `(logcount x)` を `POPCNT`(x86-64) / `i64.popcnt`(Wasm) に直接マッピング
- **難易度**: Low

#### FR-098: BSR/BSF / clz/ctz for integer-length

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/emit/wasm.lisp`
- **内容**: `(integer-length x)` → `BSR+adjust`(x86) / `i64.clz`(Wasm)
- **難易度**: Low

#### FR-099: FMA (Fused Multiply-Add)

- **対象**: `src/backend/x86-64-codegen.lisp`, `src/backend/aarch64.lisp`
- **内容**: `(+ (* a b) c)` パターンを単一 `VFMADD`/`FMADD` 命令に変換
- **難易度**: Low-Medium

#### FR-100: Wasm `local.tee` Fusion

- **対象**: `src/emit/wasm-trampoline.lisp`
- **内容**: `local.set` + 即時 `local.get` ペアを `local.tee` 単命令に融合
- **難易度**: Low

#### FR-101: Wasm `call_ref` vs `call_indirect`

- **対象**: `src/emit/wasm.lisp`
- **内容**: グローバルfuncrefテーブル経由の `call_indirect` をGC提案の型付き `call_ref` に置換
- **難易度**: Medium

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

### Phase 19 — VMディスパッチ・SSA改善

#### FR-109: Superoperator Synthesis (超命令合成)

- **対象**: `src/vm/vm-run.lisp` (`defopcode` DSL)
- **内容**: 頻出命令ペアを単一複合命令に融合。例: `vm-const r1 42; vm-add r0 r0 r1` → `vm-add-const r0 42`
- **根拠**: `defopcode` インフラが最適なフック。selfhostで頻度プロファイルを収集してターゲット列を特定
- **難易度**: Medium

#### FR-110: Inline Constant Arithmetic Opcodes

- **対象**: `src/vm/vm-run.lisp`
- **内容**: `ADD-IMM`/`CMP-IMM-ZERO`等の即値バリアント命令を追加。ループカウンタ`(incf i)`, `(> i n)`の最頻出パターンを1ディスパッチサイクルに削減
- **難易度**: Easy

#### FR-111: Dispatch Loop Specialization

- **対象**: `src/vm/vm-run.lisp`
- **内容**: 実行時間の80%を占めるホット命令5-10種を`run-vm`ループ本体にインライン展開、残りをテーブルディスパッチに
- **根拠**: CPython 3.11+ adaptive interpreter と同様のアプローチ
- **難易度**: Medium

#### FR-112: Critical Edge Splitting

- **対象**: `src/optimize/cfg.lisp`
- **内容**: 後継が複数のブロックから前継が複数のブロックへのエッジに空のランディングパッドブロックを挿入
- **根拠**: PRE・コード移動・SSA破壊の正確性に必要な前提条件。約30行で実装可能
- **難易度**: Easy

#### FR-113: Loop-Closed SSA (LCSSA)

- **対象**: `src/optimize/cfg.lisp`, `src/optimize/ssa.lisp`
- **内容**: ループ内で定義されループ外で使用される全SSA値に対してループ出口ブロックにPhiノードを挿入
- **根拠**: `basic-block`に`loop-depth`フィールドは存在するが未使用
- **難易度**: Medium

#### FR-114: Pruned / Semi-Pruned SSA

- **対象**: `src/optimize/ssa.lisp`
- **内容**: ブロックローカル変数へのPhi挿入を省略。完全Cytronアルゴリズムの冗長Phi削減
- **根拠**: `ssa-place-phis`が実際の活性チェックなしでPhiを挿入している (`ssa.lisp` 参照)
- **難易度**: Medium

#### FR-115: May-Alias / Must-Alias Oracle

- **対象**: `src/optimize/optimizer.lisp`
- **内容**: 既存のcopy-prop `reg-track` を拡張してヒープポインタのエイリアス関係を軽量追跡
- **根拠**: `opt-pass-copy-prop`がスカラコピーのみ追跡、ヒープ参照は未追跡
- **難易度**: Easy-Medium

#### FR-116: Flow-Sensitive Type Narrowing

- **対象**: `src/type/checker.lisp`, `src/compile/codegen.lisp`
- **内容**: `(if (typep x 'integer) ...)` のthenブランチで`x`をintegerに絞り込み、elseブランチで`(not integer)`に絞り込む
- **根拠**: `type-meet`/`type-join`/`extract-type-guard` は存在するが条件分岐後の型環境伝播がない
- **難易度**: Medium

#### FR-117: Superoperator Frequency Synthesis (自動検出)

- **対象**: `src/vm/vm-run.lisp` + selfhostパイプライン
- **内容**: `./cl-cc selfhost`実行中に命令バイグラム頻度を計測、上位20ペアを自動的にsuperoperator候補として出力
- **難易度**: Medium

#### FR-118: Polyvariant (k-CFA) Type Analysis

- **対象**: `src/type/inference.lisp`
- **内容**: call-site感度付き型解析。`(funcall f 1)` と `(funcall f "a")` で`f`の型を独立して追跡
- **根拠**: 現状のHMは高階関数引数が単一モノモーフィック型に束縛される
- **難易度**: Hard

---

### Phase 20 — CLOS・オブジェクトシステム最適化

#### FR-119: Multiple Dispatch Memoization (最高効果)

- **対象**: `src/vm/vm.lisp` の `vm-dispatch-generic-call`
- **内容**: GF毎に`dispatch-cache` HT (`(list arg-class-names...) → sorted-method-list`) を追加。`vm-register-method`時にフラッシュ
- **根拠**: `vm-get-all-applicable-methods`が毎回CPL walkを実行、キャッシュ皆無
- **難易度**: Easy (最高ROI)

#### FR-120: Accessor Read Inlining

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`
- **内容**: `*accessor-slot-map*` (compile-time構築済み) を使い、`(accessor obj)` → `vm-slot-read` に直接変換
- **根拠**: setf方向は既に`expand-setf-accessor`で実装済み。read方向が未実装
- **難易度**: Easy

#### FR-121: Slot Index / Vector Instance Representation

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-clos.lisp`
- **内容**: インスタンスをハッシュテーブルからsimple-vectorに変更。スロットアクセスを`(gethash slot-name ht)` → `(svref instance slot-index)`に変換
- **根拠**: 最大のCLOS性能向上。`vm-slot-read`/`vm-slot-write`等の全命令に影響
- **難易度**: Hard

#### FR-122: Effective Method Caching

- **対象**: `src/vm/vm.lisp`
- **内容**: `(gf-identity . arg-class-tuple)` → 実効メソッドのキャッシュ。`:before`/`:after`/`:around` qualifier実装後の基盤
- **根拠**: 現状はprimaryメソッドのみで combinator なし。まずprimary cacheとしてFR-119の一般化
- **難易度**: Medium

#### FR-123: make-instance Optimization

- **対象**: `src/compile/codegen.lisp:195` (ast-make-instance)
- **内容**: 静的既知クラス+固定initargsの`make-instance`を固定サイズ割り当て+定数スロット書き込みに特化
- **根拠**: `codegen-clos.lisp`で静的クラス名は既に検出済み
- **依存**: FR-121 (slot index)
- **難易度**: Medium

#### FR-124: EQL Specializer Fast Path

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **内容**: EQLスペシャライザーのサポート追加 + クラスディスパッチ前のswitch生成
- **根拠**: 現状EQLスペシャライザー未サポート
- **難易度**: Medium

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

### Phase 22 — 文字列・シンボル・例外最適化

#### FR-136: Character Class Lookup Table

- **対象**: `src/parse/cl/lexer.lisp`, `src/vm/strings.lisp`
- **内容**: `vm-alpha-char-p`等をインデックスロード1回で結果を返す256バイト配列ルックアップに変換
- **根拠**: レクサが内ループでこれらの述語を多用
- **難易度**: Medium

#### FR-137: String Literal Pool

- **対象**: `src/compile/codegen-core.lisp:270-273`
- **内容**: コンパイル時の`*string-literal-pool*`で同一文字列リテラルを単一`vm-const`に重複排除
- **根拠**: `ast-quote`ハンドラが同一文字列でも毎回別々のvm-constを発行
- **難易度**: Medium

#### FR-138: Zero-Cost Exception Table

- **対象**: `src/vm/conditions.lisp`, `src/compile/codegen.lisp:100-146`
- **内容**: `vm-establish-handler`/`vm-remove-handler`をホットパスから除去してPC→ハンドラのサイドテーブルに変換
- **根拠**: 現状は保護フォームの全入退ごとにスタックpush/popが発生 (`conditions.lisp:69-141`)
- **難易度**: Hard

#### FR-139: Handler Elision for Pure Bodies

- **対象**: `src/compile/codegen.lisp:100-146`
- **内容**: 保護フォームの命令が全て`opt-inst-pure-p`=trueなら`vm-establish-handler`を省略
- **根拠**: `effects.lisp`に100+型の純粋性テーブルが存在、自然な拡張
- **難易度**: Medium

#### FR-140: Symbol Immediate Encoding

- **対象**: `src/vm/vm.lisp`, `src/runtime/value.lisp`
- **内容**: `:key`/`nil`/`t`/`quote`等の頻出シンボルをヒープポインタでなく即値インデックスとしてエンコード
- **根拠**: `nil`/`t`は既に即値実装済み(`+val-nil+`, `+val-t+`)。パーサ内ループの`vm-intern-symbol`コストを削減
- **難易度**: Hard

#### FR-141: Self-Hosting Profile Feedback

- **対象**: `src/cli/main.lisp`, `src/vm/vm-run.lisp`
- **内容**: `./cl-cc selfhost`実行時にVM命令頻度ヒストグラムを収集し、次回コンパイルの最適化優先度決定に活用
- **根拠**: cl-cc固有の最適化機会 — selfhostが現実的なワークロードプロファイルを提供
- **難易度**: Medium

---

### Phase 23 — Wasmバックエンド最適化

#### FR-142: Wasm `ref.cast` Elimination

- **対象**: `src/emit/wasm-trampoline.lisp`
- **内容**: 直前に`struct.new $closure_t`で割り当てられたレジスタへの`ref.cast`を除去（型が静的に確定済み）
- **根拠**: `vm-call`のたびに型が判明しているクロージャへも`ref.cast`を発行
- **難易度**: Medium

#### FR-143: Wasm `return_call` / `return_call_indirect`

- **対象**: `src/emit/wasm-trampoline.lisp`
- **内容**: selfループ+`br_table`によるPC-dispatchをWasm tail-call proposal の`return_call_indirect`に置換
- **根拠**: tail callをPC更新+ループとして扱う現状の非効率を解消
- **難易度**: Medium

#### FR-144: Wasm Typed Closure Environment Array

- **対象**: `src/emit/wasm-trampoline.lisp`
- **内容**: クロージャ環境を`$env_t`参照でなくWasm GC `array.new_fixed eqref`として具体化、`array.get`で直接インデックスアクセス
- **根拠**: 現状`(ref.null $env_t)` のTODO状態
- **難易度**: Hard

#### FR-145: Wasm Integer Range Annotation

- **対象**: `src/emit/wasm-trampoline.lisp`, `src/compile/codegen.lisp`
- **内容**: fixnum範囲と証明された`vm-const`にアノテーション付与、Wasmバックエンドの`wasm-fixnum-unbox`を省略
- **根拠**: `wasm-fixnum-unbox`が全整数演算で無条件発行されている
- **難易度**: Medium

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

### Phase 25 — Fixnum / 整数最適化

#### FR-148: Fixnum演算整数範囲追跡 (VM全体)

- **対象**: `src/type/inference.lisp`, `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: `type-int` は存在するが範囲情報なし。`infer-type` が `ast-int → type-int` を返すのみで区間情報ゼロ
- **内容**: 型表現に `(integer lo hi)` 区間を追加。定数折り畳みで演算結果の区間を伝播し、オーバーフローしない演算を確認してタグチェックを省略
- **根拠**: `src/type/inference.lisp:74-100` — 整数リテラルに区間なし。SBCL の `sb-c:interval` に相当する機能が欠如
- **難易度**: Hard

#### FR-149: Fixnum→Bignum Overflow Trap分岐

- **対象**: `src/vm/primitives.lisp`, `src/compile/codegen.lisp`
- **現状**: `vm-div`（`primitives.lisp:105`）はゼロ除算のみチェック。加減乗算はオーバーフロー無検出でホストCL任せ
- **内容**: `vm-add`/`vm-sub`/`vm-mul` に fixnum範囲（51-bit）を超えた場合の bignum fallback ブランチを追加。タグ済み整数演算のオーバーフロー検出を明示的に行う
- **根拠**: `src/runtime/value.lisp:82-86` — fixnumは51-bit signed。演算結果が範囲外の場合の処理が未定義
- **難易度**: Medium

#### FR-150: Adaptive Optimization Thresholds

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: インライン閾値=15固定（`optimizer.lisp:1018`）、最大反復=20固定（`optimizer.lisp:1015`）
- **内容**: 実行時プロファイルカウンタ（呼び出し頻度・ループ深度）に基づいてインライン閾値とパス反復回数を動的調整。ホット関数には閾値を緩和（最大50命令）、コールドパスには反復を削減
- **根拠**: モダンコンパイラ（V8のTurboFan、GraalVM）はすべてフィードバック駆動の動的閾値を持つ
- **難易度**: Medium

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

### Phase 27 — ランタイム最適化基盤

#### FR-154: Tiered Compilation基盤

- **対象**: `src/compile/pipeline.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 単一最適化パスのみ（フル最適化）。高頻度呼び出し前の起動コストが大きい
- **内容**: Tier-0（最適化なし、高速コンパイル）とTier-1（フル最適化）の2段構成。初回呼び出しはTier-0で実行し、呼び出しカウンタが閾値超過でTier-1にエスカレート
- **根拠**: V8・HotSpot・PyPy等の現代JITはすべてtiered。単一最適化パスは起動遅延を引き起こす
- **難易度**: Very Hard

#### FR-155: Deoptimization / On-Stack Replacement (OSR) 基盤

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **現状**: 型前提条件を取り崩せない（一度最適化したら型変更に対応不可）。OSRエントリポイントなし
- **内容**: 最適化コード中に型チェックガードと deoptimization チェックポイントを埋め込む。ガード失敗時にインタープリタへフォールバック。ループ内からの OSR エントリ (ループバックエッジにエントリポイント付与)
- **根拠**: FR-154のTiered実行を安全に行うための必須インフラ
- **難易度**: Very Hard

#### FR-156: Size-Class Segregated Allocator (旧世代)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: major GC sweepでfree-listを構築（`gc.lisp:158,326`）するが**割り当て時に再利用しない**。旧世代はbump-pointerのみ
- **内容**: free-listをサイズクラス（8, 16, 32, 64, 128, 256, 512, 1024 bytes）にバケット分けし、`rt-gc-alloc`で適切バケットからO(1)再利用
- **根拠**: `gc.lisp:158` — free-listは定義済みだが`rt-gc-alloc`から参照されていない
- **難易度**: Medium

#### FR-157: Managed Cons Cell Allocation (ホストCL依存解消)

- **対象**: `src/vm/list.lisp`, `src/vm/vm.lisp`, `src/runtime/gc.lisp`
- **現状**: `vm-cons`命令（`list.lisp`）がホストCL の `cons` を呼んでいる。マネージドヒープを使わずホストGCに依存
- **内容**: `vm-cons`をマネージドヒープ(`rt-gc-alloc`)から2ワードのconsセルを割り当てるように変更。NaN-boxingのpointerタグを付与してVM内で一貫管理
- **根拠**: `src/vm/list.lisp` — `(cons car-val cdr-val)` がホストCL呼び出し。GC境界が2つ存在する
- **難易度**: Hard

#### FR-158: コールグラフベースインライン展開

- **対象**: `src/optimize/optimizer.lisp`, `src/cli/main.lisp`
- **現状**: `opt-pass-inline`（`optimizer.lisp:729`）は単一命令列内の局所的定義のみ参照。モジュール境界を越えたインライン不可
- **内容**: FR-102のLTOコールグラフを活用してモジュール境界を越えた小関数（≤30命令、非再帰）をインライン展開。呼び出しグラフの葉から順にボトムアップ処理
- **根拠**: `optimizer.lisp:781` — インライン対象が同一命令列内の`vm-closure`定義のみに限定。外部defunは不可
- **難易度**: Hard

### Phase 28 — CPS高度最適化

#### FR-159: Administrative Redex Elimination

- **対象**: `src/compile/cps.lisp`
- **現状**: CPS変換が`(λ k. M) K`形式の冗長な継続ラッパーを大量生成（`cps.lisp:145-156`）。後段パスで除去されない
- **内容**: CPS変換直後にadmin redex `((λ k body) K)`を検出し`body[K/k]`に置換するパスを追加。FR-027のbeta reductionより先に適用して変換ノイズを除去
- **根拠**: MLtonはCPS変換時にadmin redexを即座に除去。cl-ccではネストしたラムダラッパーがそのまま`vm-closure`割り当てになる
- **難易度**: Medium

#### FR-160: Lambda Lifting

- **対象**: `src/compile/closure.lisp`, `src/compile/codegen.lisp`
- **現状**: `find-free-variables`（`closure.lisp:48-81`）で自由変数を解析するが、内部関数を外部に持ち上げる変換なし。すべての内部`lambda`がクロージャ割り当てを発生
- **内容**: 自由変数が少ない（≤4）内部関数をトップレベルに持ち上げ、自由変数を追加引数として渡す。クロージャ割り当てをゼロにする
- **根拠**: `codegen.lisp:581-582`が空キャプチャでも常に`make-vm-closure`を生成。MLton/SML#はlambda liftingで大半のクロージャを除去
- **難易度**: Hard

#### FR-161: Arity Raising / Uncurrying

- **対象**: `src/compile/cps.lisp`, `src/optimize/optimizer.lisp`
- **現状**: CPS変換後のカリー化された関数適用がそのまま複数回の`vm-call`に展開される。連続適用をバッチ化する機構なし
- **内容**: `(f a b)`が`((f a) b)`とCPS展開される場合、呼び出しサイトのarity解析でバッチ引数渡しに変換。中間クロージャ生成を除去
- **根拠**: GHCのarity analysis / MLtonのuncurrying。CPS形式で特に顕著な最適化
- **難易度**: Hard

#### FR-162: Known Continuation Optimization

- **対象**: `src/compile/cps.lisp`, `src/optimize/optimizer.lisp`
- **現状**: すべての継続を汎用的に扱う（`cps.lisp:160-167`）。継続が1回だけ呼ばれるか、常に同じ継続が渡されるかを追跡しない
- **内容**: 継続の使用回数と呼び出しパターンを解析。線形継続（1回呼び出し）はインライン展開、非エスケープ継続はジャンプに変換（FR-028 contificationの前提解析）
- **根拠**: GHCのdemand analysis on continuations。cl-ccでは全継続がfirst-classクロージャとして生存
- **難易度**: Hard

---

### Phase 29 — 高度コードモーション

#### FR-163: Code Sinking (逆LICM)

- **対象**: `src/optimize/optimizer.lisp`, `src/optimize/cfg.lisp`
- **現状**: `optimizer.lisp`に"sink"/"hoist"関連の処理なし（grep 0マッチ）
- **内容**: 命令を定義位置から使用位置の直前まで移動。レジスタ圧力を削減し、使用されない分岐パスの命令を除去可能にする。LICM（FR-003）の逆操作
- **根拠**: LLVMの`MachineSink`パス。特に分岐の片方でのみ使われる値の移動に効果大
- **難易度**: Medium

#### FR-164: Partial Dead Code Elimination (PDCE)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `opt-pass-dce`（`optimizer.lisp:494-513`）は全パスで未使用の命令のみ除去。一部パスでのみ使用される命令は残存
- **内容**: 一部の実行パスでのみ生存する値を検出し、不要パスの計算を除去またはsink。FR-165のpost-dominator解析が前提
- **根拠**: LLVMの`PartialDCE`。現行DCEでは`if`の片方でのみ使われる計算が両方のパスで実行される
- **難易度**: Hard

#### FR-165: Post-Dominator Analysis

- **対象**: `src/optimize/cfg.lisp`
- **現状**: 支配木（dominator tree）は`cfg.lisp:210-243`に実装済みだが、逆支配木（post-dominator）は存在しない
- **内容**: CFGの逆グラフに対して`cfg-compute-dominators`を適用してpost-dominator treeを構築。制御依存グラフ (CDG) の基盤
- **根拠**: PDCE（FR-164）・code sinking（FR-163）・制御依存解析の前提インフラ
- **難易度**: Easy

#### FR-166: Constant Hoisting

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `opt-pass-fold`は定義位置で定数を畳み込むが、ループ内の高コスト定数を関数入口に移動しない
- **内容**: ループ内で繰り返し使用される高コスト定数（大きな整数リテラル、浮動小数点定数、シンボルルックアップ）を関数入口のプレヘッダに移動
- **根拠**: LLVMの`ConstantHoisting`パス。LICM（FR-003）とは別に定数専用のhoistingが効率的
- **難易度**: Easy

#### FR-167: Tail Duplication

- **対象**: `src/optimize/cfg.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 基本ブロックの重複機構なし。CFGは構築のみで変形操作なし
- **内容**: 複数の先行ブロックから分岐するブロックの末尾を各先行ブロックに複製。分岐予測改善とジャンプスレッディング機会拡大
- **根拠**: LLVMの`TailDuplication`。cl-ccのジャンプスレッディング（FR-001のpeephole、`opt-pass-jump`）の効果を増幅
- **難易度**: Medium

#### FR-168: Branch Correlation

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 同一条件が複数箇所でテストされる場合、それぞれ独立に評価。先行テスト結果の再利用なし
- **内容**: 値述語を支配木に沿って伝播し、先行分岐で確定した条件を後続分岐で定数化。`(if (integerp x) ... (if (integerp x) ...)` → 2回目を定数`t`に
- **根拠**: LLVMの`CorrelatedValuePropagation`。型チェック分岐が頻出するLispコードで特に効果大
- **難易度**: Medium

---

### Phase 30 — ループ構造最適化

#### FR-169: Loop Rotation

- **対象**: `src/optimize/cfg.lisp`
- **現状**: `bb-loop-depth`フィールド（`cfg.lisp:32`）は存在するが未使用。自然ループの検出・回転なし
- **内容**: `while(cond) { body }` を `if(cond) { do { body } while(cond) }` に変換。ループ末尾にバックエッジを配置してCPUのループ分岐予測を改善
- **根拠**: LLVMの`LoopRotate`パス。cl-ccのLICM（FR-003）とloop unrolling（FR-022）の効果を最大化する前提変換
- **難易度**: Medium

#### FR-170: Loop Peeling

- **対象**: `src/optimize/optimizer.lisp`, `src/optimize/cfg.lisp`
- **現状**: ループの初回反復の分離なし。初回のみ特殊な処理が必要なケース（初期化ガード等）でも全反復が同一コード
- **内容**: ループの最初のN回（通常1回）を剥離してループ外にコピー。初回の型チェックや初期化コストをループ外に追い出す
- **根拠**: LLVMの`LoopPeel`。`(dolist (x list) ...)`の初回nil チェック除去に有効
- **難易度**: Medium

---

### Phase 31 — ネイティブコード品質

#### FR-171: LEA for Address Computation (x86-64)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: アドレス計算はMOVベースのみ（`x86-64-codegen.lisp:100-111`）。LEA命令の生成なし
- **内容**: `a + b*scale + disp`パターンをLEA命令に変換。フラグレジスタを破壊せず加算+シフトを1命令で実行
- **根拠**: LEAはx86-64で最も多用途な命令。GCC/LLVMは算術にもLEAを積極使用
- **難易度**: Medium

#### FR-172: BMI/BMI2 Bit Manipulation Instructions

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: ビット操作は手動シフト+マスクのみ（`x86-64-codegen.lisp:699-726`）。POPCNT/LZCNT/TZCNT/PEXT/PDEP未対応
- **内容**: `integer-length` → LZCNT、`logcount` → POPCNT、ビットフィールド抽出 → PEXT に変換。CPUID/機能フラグでBMI対応を検出
- **根拠**: `vm-integer-length`/`vm-logcount`（`vm-numeric.lisp:98-134`）がホストCL委譲のまま
- **難易度**: Medium

#### FR-173: Scaled Addressing Modes [base+index*scale+disp]

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: `[base]`と`[base+disp8]`のみ対応。スケールドインデックス`[base+index*scale]`未実装
- **内容**: 配列アクセス`(aref arr idx)`のコードパターンで`[base+index*8+header_offset]`を1命令で表現。追加のADD命令を除去
- **根拠**: x86-64はscale={1,2,4,8}のSIBバイトをネイティブサポート。cl-ccは配列アクセスで別途加算命令を生成
- **難易度**: Medium

#### FR-174: Native-Level Peephole Optimization (Post-Emit)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: ペアホール最適化はVMレベルのみ（`prolog.lisp`）。機械語生成後のペアホールなし
- **内容**: emit後の命令列から冗長パターンを除去: `MOV R,R`の除去、`MOV R1,R2; MOV R2,R1`の除去、連続PUSH/POPの除去、CMP+分岐の融合
- **根拠**: GCCのpass2ペアホール、LLVMのMachineInstCombine。VMペアホールでは機械語レベルの冗長性を検出不可
- **難易度**: Medium

#### FR-175: Instruction Selection Framework (BURS/Maximal Munch)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: VM命令→機械語の1:1ナイーブマッピング。複数VM命令をまたぐパターンマッチなし
- **内容**: ツリーパターンマッチング（Maximal Munch方式）によるVM→機械語変換。`vm-add(R1, vm-mul(R2, 8))`→`LEA R1, [R2*8]`のように木パターンを認識して最適命令を選択
- **根拠**: LLVM SelectionDAG / GCC BURS。命令選択は最適コード生成の核心
- **難易度**: Very Hard

#### FR-176: Custom Calling Conventions for Internal Functions

- **対象**: `src/emit/calling-convention.lisp`, `src/compile/codegen.lisp`
- **現状**: すべての関数がSystem V AMD64 ABI / AAPCS準拠（`calling-convention.lisp`）。内部関数もフルABI遵守
- **内容**: モジュール内部の非公開関数にカスタム呼び出し規約を適用。callee-savedレジスタの削減、引数の柔軟なレジスタ配置、不要なフレームポインタ省略
- **根拠**: LLVMの`fastcc`/`coldcc`。内部関数は外部ABIに従う必要がない
- **難易度**: Hard

#### FR-177: Callee-Save Register Elimination

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: x86-64は常に6レジスタPUSH（`x86-64-codegen.lisp:1105-1111`）、AArch64は常に6 STP（`aarch64-codegen.lisp:369-382`）
- **内容**: 関数本体のレジスタ使用状況を解析し、使用されないcallee-savedレジスタのsave/restoreを省略
- **根拠**: 葉関数や短い関数では大半のcallee-savedレジスタが未使用。FR-002の葉関数検出と連携
- **難易度**: Easy

#### FR-178: Red Zone Usage (x86-64)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: RSP調整なしの一時領域利用なし。常にPUSH/POPまたはSUB RSPでスタックフレーム確保
- **内容**: 葉関数でRSP以下128バイトのred zone（System V AMD64 ABI保証）を活用。フレームポインタ・RSP調整を完全省略
- **根拠**: x86-64 ABIがred zoneを保証。葉関数のプロローグ/エピローグを0命令化
- **難易度**: Easy

---

### Phase 32 — データ構造・シーケンス最適化

#### FR-179: Sequence Operation Fusion (map+filter → single loop)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `mapcar`（`macros-stdlib.lisp:352`）は`dolist+cons+nreverse`に展開。`map`（line 632）は`(coerce (mapcar fn (coerce seq 'list)) result-type)` で3重走査
- **内容**: 連鎖するシーケンス操作（mapcar→remove-if→mapcar等）をマクロ展開時または最適化パスで単一ループに融合。中間リスト割り当てを除去
- **根拠**: GHCのstream fusion / Rustのiterator fusion。cl-ccでは`(mapcar f (mapcar g xs))`が2回走査
- **難易度**: Hard

#### FR-180: Single-Value Optimization

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **現状**: `(values x)`が`vm-values`命令を生成（`codegen.lisp:154-160`）し、`vm-values-list`にリスト割り当て。単一値でもリスト構築
- **内容**: コンパイル時に`values`の引数が1個の場合、`vm-values`を省略して直接レジスタ移動に変換。`multiple-value-bind`で1値のみ使用する場合も同様に簡素化
- **根拠**: `vm.lisp:267` — `vm-values`は常にリスト構築。単一値はCL仕様上bare valueと同値
- **難易度**: Easy

#### FR-181: Constant Pool / Literal Deduplication

- **対象**: `src/compile/codegen-core.lisp`, `src/compile/pipeline.lisp`
- **現状**: 各リテラルが個別の`vm-const`命令として生成（`codegen-core.lisp:44`）。同一値の重複排除なし
- **内容**: コンパイル単位ごとの定数プールを構築し、同一値のリテラル（整数、浮動小数点、文字列、シンボル）を共有。FR-137の文字列プールを汎化
- **根拠**: JVM/CLR/Python VMはすべて定数プールを持つ。cl-ccでは`42`が10箇所で使われると10個の`vm-const`が生成される
- **難易度**: Easy

---

### Phase 33 — 解析基盤・ランタイム拡張

#### FR-182: Demand Analysis / Strictness Analysis

- **対象**: `src/optimize/optimizer.lisp`, `src/type/inference.lisp`
- **現状**: 値の使用パターン解析なし。すべての式が即時評価される前提
- **内容**: 関数の各引数について「必ず使用される(strict)」「条件付き使用(lazy)」「未使用(absent)」を判定。strictな引数はunbox可能、absentな引数は計算省略可能
- **根拠**: GHCのdemand analyzer。CPS形式のcl-ccでは特に継続の使用パターン解析が効果的
- **難易度**: Hard

#### FR-183: Known Function Property Database

- **対象**: `src/compile/builtin-registry.lisp`, `src/optimize/effects.lisp`
- **現状**: `builtin-registry.lisp`は189個のビルトインの呼び出し規約のみ管理。副作用分類・戻り値型・引数制約は未記録
- **内容**: 各ビルトイン関数に属性を付与: `:pure`（副作用なし）、`:foldable`（定数畳み込み可能）、`:nonneg-result`（`length`は≥0）、`:always-returns`（`car`は常に返る）、`:no-escape`（引数が脱出しない）
- **根拠**: LLVMの`FunctionAttrs`パス、SBCLの`fun-info`。FR-152の推移的純粋性推論の基盤データ
- **難易度**: Medium

#### FR-184: Weak Reference / Finalization Support

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`, `src/vm/vm.lisp`
- **現状**: weak pointerおよびfinalizationの実装なし。GCは強参照のみ追跡
- **内容**: weak pointer型（GCルートから除外）、weak hash table（キー/値のweak参照選択）、finalization queue（GC回収前のクリーンアップコールバック）を追加
- **根拠**: ANSI CL仕様外だがSBCL/CCL/LispWorksすべてが提供。自己完結ランタイムに必須
- **難易度**: Hard

#### FR-185: Optimization Reports

- **対象**: `src/optimize/optimizer.lisp`, `src/cli/main.lisp`
- **現状**: 最適化は完全にサイレント。どの関数がインライン化されたか、どの定数が畳み込まれたかの情報出力なし
- **内容**: `--optimization-report`フラグで最適化判断をレポート: インライン展開（関数名・命令数・判断理由）、定数畳み込み（元の式・結果）、DCE除去数、CSE統合数
- **根拠**: LLVMの`-Rpass=.*`、GCCの`-fopt-info`。コンパイラ開発・デバッグに不可欠
- **難易度**: Easy

---

### Phase 34 — コードレイアウト・キャッシュ最適化

#### FR-186: Function Reordering for Cache Locality

- **対象**: `src/compile/pipeline.lisp`, `src/emit/binary/macho.lisp`
- **現状**: 関数はソース順に配置。コールグラフに基づく配置最適化なし
- **内容**: コールグラフの頻度情報（PGO）またはトポロジカル順に基づいて、相互呼び出し頻度の高い関数をメモリ上で隣接配置。I-cache局所性を改善
- **根拠**: LLVMの`-function-sections` + リンカの`--call-graph-profile-sort`。cl-ccでは関数間のジャンプ距離が非最適
- **難易度**: Medium

#### FR-187: Prefetch Instruction Insertion

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: prefetch命令の生成なし。ヒープポインタ間接参照やリスト走査でキャッシュミスが発生
- **内容**: リスト走査ループでの`PREFETCHT0 [next_cons]`、配列アクセスでの`PREFETCHNTA [arr+stride]`を挿入。AArch64では`PRFM PLDL1KEEP`
- **根拠**: GCCの`-fprefetch-loop-arrays`。`dolist`/`dotimes`等のループで効果大
- **難易度**: Medium

#### FR-188: NOP Padding / Alignment Directives

- **対象**: `src/emit/x86-64.lisp`, `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: アセンブリテキスト出力に`.align`ディレクティブなし（`x86-64.lisp:73-91`）。ループヘッダ・関数エントリにNOP paddingなし
- **内容**: ループバックエッジのターゲットを16バイト境界にアラインし、関数エントリを16バイト境界にパディング。分岐予測ヒットレート向上
- **根拠**: Intel最適化マニュアル: ループヘッダアラインで3-5%性能改善
- **難易度**: Easy

#### FR-189: Cache-Line Aware Object Layout

- **対象**: `src/runtime/heap.lisp`, `src/compile/codegen-clos.lisp`
- **現状**: オブジェクトフィールドはソース順配置。ホット/コールドフィールドの分離なし。カードテーブルは512B（`heap.lisp:39`）だがオブジェクトサイズとの整合性なし
- **内容**: CLOS instanceのホットフィールド（頻繁アクセス）を先頭64バイト（1 cache line）に集約。コールドフィールドは後方に配置。GCメタデータとユーザーフィールドのfalse sharing回避
- **根拠**: HotSpot VMのfield packing。L1キャッシュライン=64Bへの最適配置
- **難易度**: Hard

---

### Phase 35 — 並行性・スレッド基盤

#### FR-190: Concurrent / Incremental GC

- **対象**: `src/runtime/gc.lisp`
- **現状**: Stop-the-world GC（minor: `gc.lisp:200-263`、major: `gc.lisp:331-392`）。SATBバリアは実装済みだが並行マーキング未対応
- **内容**: major GCのマーキングフェーズを並行化。SATB事前書き込みバリア（既存`gc.lisp:282-289`）を活用してミューテータと並行してマーク。スイープも並列化可能
- **根拠**: Go/JVM/ZGC はすべて並行GC。STWポーズ時間がヒープサイズに比例する現状は大規模プログラムで致命的
- **難易度**: Very Hard

#### FR-191: Atomic Operations (CAS, Memory Fences)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`, `src/vm/vm.lisp`
- **現状**: アトミック命令ゼロ。x86-64にmfence/lfence/sfence、AArch64にdmb/dsb/isbの生成なし
- **内容**: `vm-cas`（compare-and-swap）、`vm-atomic-add`命令を追加。ネイティブバックエンドで`LOCK CMPXCHG`（x86-64）、`LDXR/STXR`（AArch64）を生成。メモリフェンス命令も追加
- **根拠**: ロックフリーデータ構造・並行GC（FR-190）の前提インフラ
- **難易度**: Hard

#### FR-192: Thread-Local Storage (TLS)

- **対象**: `src/vm/vm.lisp`, `src/runtime/heap.lisp`
- **現状**: `vm-state`はシングルインスタンス前提（`vm.lisp:342-372`）。全レジスタ・ヒープ・グローバル変数が共有
- **内容**: per-thread `vm-state`分離: 各スレッドが独自のレジスタファイル・コールスタック・GCルートを持つ。special変数のper-thread binding（CLの動的束縛セマンティクス保持）
- **根拠**: SBCL/CCLはすべてper-thread binding stack。マルチスレッドLispの基本要件
- **難易度**: Very Hard

#### FR-193: Thread-Safe Global Variable Access

- **対象**: `src/vm/vm.lisp`
- **現状**: `vm-set-global`（`vm.lisp:991-996`）と`vm-get-global`（`vm.lisp:998-1005`）がロックなしでhash-tableを直接操作
- **内容**: グローバル変数ストアにread-write lockまたはCAS-based concurrent hash tableを適用。`function-registry`（line 360）、`class-registry`（line 353）も同様に保護
- **根拠**: 複数スレッドからの`defvar`/`setf`がデータ競合を引き起こす
- **難易度**: Medium

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

### Phase 37 — レジスタ割り当て・スケジューリング高度化

#### FR-199: Spill Slot Sharing / Stack Coloring

- **対象**: `src/emit/regalloc.lisp`
- **現状**: 各vregが一意のスピルスロット（`regalloc.lisp:351-375`、`[RBP - slot*8]`）。ライブ範囲が重複しないvregのスロット共有なし
- **内容**: スピルされたvregのライブ範囲を干渉グラフに投影し、非干渉のvregに同一スタックスロットを割り当て。スタックフレームサイズをO(N)→O(χ(G))に削減
- **根拠**: LLVM StackSlotColoring。現状N個のスピルvregがN×8バイトのスタックを消費
- **難易度**: Medium

#### FR-200: Software Pipelining

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: ループ反復間の命令重複なし。各反復が完全にシーケンシャル
- **内容**: ループボディの命令をモジュロスケジューリングで反復間パイプライン化。反復Nの後半と反復N+1の前半をオーバーラップ実行
- **根拠**: GCCの`-fmodulo-sched`。タイトな数値ループで2-4倍のスループット改善
- **難易度**: Very Hard

#### FR-201: Trace Scheduling / Superblock Formation

- **対象**: `src/optimize/cfg.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: 基本ブロック単位の命令生成のみ。ブロック境界を超えたスケジューリングなし
- **内容**: 最頻実行パスに沿ってトレース（基本ブロック列）を形成し、トレース内で命令スケジューリング。ブロック境界の制約を除去してILP（命令レベル並列性）を最大化
- **根拠**: Fisherのtrace scheduling。FR-068のpost-RAスケジューリングを基本ブロック境界越えに拡張
- **難易度**: Very Hard

---

### Phase 38 — Wasm高度機能

#### FR-202: Wasm SIMD Support

- **対象**: `src/emit/wasm.lisp`, `src/emit/wasm-trampoline.lisp`
- **現状**: Wasm SIMD命令なし。ベクトル型（v128）未定義
- **内容**: Wasm SIMD128拡張（v128.load, i32x4.add, f64x2.mul等）。数値配列操作のベクトル化コード生成。自動ベクトル化との連携
- **根拠**: Wasm SIMD Proposalは全主要ブラウザで出荷済み（Chrome/Firefox/Safari/Edge）
- **難易度**: Hard

#### FR-203: Wasm Threads / Shared Memory

- **対象**: `src/emit/wasm.lisp`
- **現状**: shared memoryなし。atomic命令なし。Worker間データ共有不可
- **内容**: `shared` memory宣言、`memory.atomic.wait`/`memory.atomic.notify`、`i32.atomic.rmw.cmpxchg`等のatomic操作。SharedArrayBuffer経由のWorker間通信
- **根拠**: Wasm Threads Proposalは主要ブラウザで出荷済み。並行Webアプリケーションに必須
- **難易度**: Very Hard

#### FR-204: Wasm Exception Handling Codegen

- **対象**: `src/emit/wasm-trampoline.lisp`, `src/emit/wasm-types.lisp`
- **現状**: `+wasm-try+`/`+wasm-catch+`/`+wasm-throw+`定数は定義済み（`wasm-types.lisp:89-91`）だがコード生成なし
- **内容**: `handler-case`/`handler-bind`をWasmの`try`/`catch`/`throw`に変換。現状のPC-dispatchベースの例外処理をネイティブWasm例外に置換
- **根拠**: 定数は実装済みだがemitルールが未接続。ネイティブ例外で性能10-100倍改善
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

### Phase 40 — 部分評価・特殊化

#### FR-209: Partial Evaluation (部分評価)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **現状**: 定数畳み込み（`optimizer.lisp:46-414`）は単一命令レベルのみ。引数が定数の場合でも関数呼び出しを除去しない
- **内容**: 関数の引数が定数と判明した場合、関数本体を引数に対して部分評価して残余コード（residual code）を生成。Futamura projection第一段階。定数引数から到達不能なブランチを除去
- **根拠**: SBCL deftransform / GHC rules。`(defun f (x) (if (= x 0) 0 (* x 2)))` の `(f 0)` → `0`。コンパイル時計算で実行時コスト完全除去
- **難易度**: Hard

#### FR-210: Binding-Time Analysis (束縛時解析)

- **対象**: `src/optimize/optimizer.lisp`, `src/type/inference.lisp`
- **現状**: 定数性判定は `vm-const` 命令の存在のみで判定。引数や中間値の定数伝播が関数境界を越えない
- **内容**: 式の各部分を「静的」（コンパイル時既知）と「動的」（実行時のみ既知）に分類するBTA。部分評価（FR-209）の前段として動作し、特殊化対象の引数を自動選定
- **根拠**: Mix/Similix方式のオフラインBTA。SCCP（FR-010）の結果をフィードして関数境界を越える定数伝播を実現
- **難易度**: Hard

#### FR-211: Function Specialization by Known Arguments (既知引数特殊化)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `codegen.lisp:527-531` — 全非ビルトイン呼び出しが`vm-call`間接ディスパッチ。呼び出し元の引数情報を利用した特殊化バージョン生成なし
- **内容**: 呼び出し元で型や定数が判明している引数に対して、関数の特殊化クローンを生成。`(sort list #'< :key #'car)` → `sort-by-car-ascending` のような特殊化版を自動生成
- **根拠**: GHC SpecConstr / LLVM argument promotion。ホット関数の呼び出しパターンを分析して特殊化の利益を推定
- **難易度**: Very Hard

---

### Phase 41 — GC高度化 (ピン・コンパクション)

#### FR-212: Object Pinning (オブジェクトピン)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: Cheney copying GC（`gc.lisp:200-263`）が全オブジェクトを移動。FFI呼び出し中のオブジェクトアドレス安定性保証なし
- **内容**: GCルートにピンフラグを追加し、ピンされたオブジェクトはコピー対象から除外。FFI境界（`with-pinned-objects` マクロ）でGCに通知。ピンされたオブジェクトは移動せずフラグメンテーション管理
- **根拠**: JVM JEP 423 / .NET pinned objects / SBCL with-pinned-objects。FFI安全性に必須
- **難易度**: Hard

#### FR-213: GC Compaction / Defragmentation (ヒープコンパクション)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: major GC（`gc.lisp:331-392`）はマーク&スイープのみ。`gc.lisp:158`のfree-listは未使用。解放後のフラグメンテーション対策なし
- **内容**: major GC後にスライディングコンパクションを実行。生存オブジェクトを連続配置してフラグメンテーション解消。前方参照テーブルでポインタ更新。FR-212のピンされたオブジェクト周辺はスキップ
- **根拠**: G1 GC / Shenandoah / ZGC。長時間実行コンパイルでのヒープフラグメンテーション防止
- **難易度**: Very Hard

---

### Phase 42 — CLOS高度化・オブジェクトモデル

#### FR-214: Object Shape / Hidden Class System (V8スタイル)

- **対象**: `src/vm/vm-clos.lisp`, `src/runtime/heap.lisp`
- **現状**: `vm-clos.lisp:165-183` — CLOSインスタンスはハッシュテーブル（オブジェクト毎に1個）。同クラスの全インスタンスが同一キーセットを持つにもかかわらず毎回ハッシュルックアップ
- **内容**: クラスのスロットレイアウトから「シェイプ」（hidden class）を導出。同一シェイプのインスタンスはfixed-size vectorでスロット格納。スロット追加/削除でシェイプ遷移。ICのキーをシェイプIDに変更してO(1)スロットアクセス
- **根拠**: V8 hidden classes / SpiderMonkey shapes / Self maps。ハッシュテーブル→ベクタでスロットアクセス10x高速化
- **難易度**: Very Hard

#### FR-215: CLOS Method Combination (:before/:after/:around)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen-clos.lisp`
- **現状**: `vm-clos.lisp` — standard method combinationのみ。primary methodのみディスパッチ。`call-next-method`は`vm.lisp:751-769`で実装済み
- **内容**: `:before`/`:after`/`:around` qualifier付き`defmethod`のコンパイルとディスパッチ。effective method computation: around → before → primary → after の実行順序。`compute-effective-method`のキャッシュ（FR-120連携）
- **根拠**: ANSI CL 7.6.6.2 standard method combination。SBCL/CCL標準。self-hostingでCLOS完全サポートに必要
- **難易度**: Hard

---

### Phase 43 — メモリ解析・ストア最適化

#### FR-216: Store-to-Load Forwarding (ストア→ロード転送)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: DSE（`optimizer.lisp` FR-016）は死んだストアを除去するが、直前のストアと同一アドレスへのロードを検出してストア値を直接転送する最適化なし
- **内容**: `vm-set-global` → `vm-get-global` の同一変数パターンでロードをストア値に置換。`vm-set-slot` → `vm-get-slot` の同一オブジェクト・同一スロットパターンも対象。alias analysis（FR-017）と連携
- **根拠**: LLVM MemorySSA-based store-to-load forwarding / GCC tree-ssa-forwprop。ローカル変数の冗長なロード除去
- **難易度**: Medium

#### FR-217: Memory SSA (メモリSSA)

- **対象**: `src/optimize/ssa.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `ssa.lisp` — レジスタのSSA構築は実装済み（Cytron法）だがメモリ操作のSSA表現なし。メモリ依存関係は命令順序でしか推論できない
- **内容**: メモリアクセス（vm-get-global/vm-set-global, vm-get-slot/vm-set-slot, vm-cons）にメモリ版SSA番号を付与。MemoryDef/MemoryUse/MemoryPhiノード。GVN（FR-011）とDSE（FR-016）の精度向上
- **根拠**: LLVM MemorySSA (2016〜) / GCC memory-ssa。SSA上のメモリ依存解析でload/store最適化の精度を大幅改善
- **難易度**: Very Hard

---

### Phase 44 — 数値演算高度化

#### FR-218: Karatsuba Multiplication / Fast Bignum Algorithms

- **対象**: `src/vm/vm-numeric.lisp`, `src/vm/primitives.lisp`
- **現状**: `vm-numeric.lisp` — 671行。全数値演算をホストCLにデリゲート。自前のbignum表現・演算なし
- **内容**: NaN-boxing 51-bit fixnum範囲を超えた場合のbignum表現（digit vector）を自前実装。schoolbook乗算に加えてKaratsuba法（O(n^1.585)）を閾値（64 digit以上）で切り替え。除算はBurnikel-Ziegler法
- **根拠**: GMP / OpenJDK BigInteger / SBCL sb-bignum。self-hostingでホストCLのbignum実装に依存しない独立した数値タワー
- **難易度**: Very Hard

#### FR-219: Complex Number Unboxing (複素数アンボクシング)

- **対象**: `src/vm/vm-numeric.lisp`, `src/compile/codegen.lisp`, `src/type/inference.lisp`
- **現状**: `vm-numeric.lisp:662-669` — `vm-complex`命令が存在しホストCLの`complex`関数を呼ぶ。複素数は常にboxed（cons/struct）で実部・虚部アクセスに毎回アンボクシング
- **内容**: エスケープ解析（FR-007）で複素数がローカルにのみ使用される場合、実部と虚部を別レジスタに分離して保持。`(+ c1 c2)` → `(+ r1 r2)`, `(+ i1 i2)` の2命令に展開（SROA FR-014の特殊ケース）
- **根拠**: SBCL complex-float unboxing / GHC unboxed complex。科学計算ワークロードで顕著な性能改善
- **難易度**: Hard

---

### Phase 45 — 言語拡張・継続

#### FR-220: symbol-macrolet Compilation

- **対象**: `src/expand/expander.lisp`, `src/parse/cst.lisp`, `src/compile/codegen.lisp`
- **現状**: `cst.lisp:182` — CST parserで`:symbol-macrolet`として認識されるがコンパイルされない。`expander-data.lisp:38`にリストされるが展開ロジック未実装
- **内容**: `symbol-macrolet`の展開をexpander.lispに実装。環境にシンボル→展開形のバインディングを追加し、変数参照時にルックアップして展開形に置換。`macrolet`の実装（既存）をモデルに
- **根拠**: ANSI CL 3.1.2.1.1 — symbol-macroletは標準特殊形式。`setf`展開やwith-accessorsマクロの基盤
- **難易度**: Medium

#### FR-221: Delimited Continuations (shift/reset)

- **対象**: `src/compile/cps.lisp`, `src/vm/vm.lisp`
- **現状**: CPS変換（`cps.lisp`）はundelimited continuationのみ。`call/cc`相当なし。VMのコールスタック（`vm-state:call-stack`）は1本のスタックで分割不可
- **内容**: `reset` (プロンプトを設置) と `shift` (限定継続を捕獲) のプリミティブ実装。CPS変換で`reset`をメタ継続に、`shift`を継続具象化に変換。VMスタックの部分コピーで限定継続をファーストクラスオブジェクト化
- **根拠**: Racket / Scala 3 / OCaml 5.0 effect handlers。モダンな例外処理・コルーチン・非同期IOの統一基盤
- **難易度**: Very Hard

#### FR-222: Coroutines / Generators (コルーチン・ジェネレータ)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **現状**: VMに`yield`/`resume`プリミティブなし。ループ内の遅延列挙は`mapcar`+リスト中間生成が必須
- **内容**: `(defgenerator name (args) body)` マクロで`yield`可能な関数を定義。FR-221（限定継続）上に構築するか、stackful coroutineとして独立実装。`yield`はVM状態のスナップショット保存＋呼び出し元への値返却
- **根拠**: Python generators / Kotlin coroutines / Lua coroutines。ストリーム処理・遅延評価の効率化基盤
- **難易度**: Hard

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

### Phase 47 — SIMD・ベクトル化

#### FR-226: Auto-Vectorization / Loop Vectorization (ループ自動ベクトル化)

- **対象**: `src/optimize/optimizer.lisp`, `src/optimize/cfg.lisp`
- **現状**: 全演算がスカラー。ループ解析（FR-021 SCEV）は定義済みだがベクトル化解析パスなし
- **内容**: ループ内の独立したスカラー演算を検出し、SIMD幅（4xf32, 2xf64等）のベクトル演算に変換。ループストリップマイニング（ベクトル幅でループ分割）＋残余ループ生成。依存関係解析でベクトル化可能性を判定
- **根拠**: LLVM LoopVectorize / GCC tree-vect-loop。配列処理ワークロードで4-8x高速化
- **難易度**: Very Hard

#### FR-227: SLP Vectorizer (Superword Level Parallelism)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: 直線コード内の並列実行可能なスカラー命令を検出する仕組みなし
- **内容**: 連続メモリアクセスパターン（`(aref arr i)`, `(aref arr (+ i 1))`等）を検出し、パック可能な独立演算をSIMD命令にまとめる。ループ外の直線コードにも適用可能
- **根拠**: LLVM SLPVectorizer / GCC tree-vect-slp。ループベクトル化と相補的
- **難易度**: Hard

#### FR-228: x86-64 SSE/AVX命令エミッション

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/regalloc.lisp`
- **現状**: スカラー整数命令のみ（MOV, ADD, SUB, IMUL, CMP等）。XMM/YMMレジスタ管理なし
- **内容**: SSE2（PADDD, MOVDQA等）/ SSE4（PMULLD等）/ AVX2（VPADDD, VPERM等）命令エンコーディング。XMM0-XMM15レジスタの割り当て管理。AVX VEXプレフィックスエンコーディング
- **根拠**: x86-64 ISA。FR-226/FR-227のベクトル化結果をネイティブ命令に変換するために必須
- **難易度**: Hard

#### FR-229: AArch64 NEON命令エミッション

- **対象**: `src/emit/aarch64-codegen.lisp`, `src/emit/regalloc.lisp`
- **現状**: スカラー整数命令のみ（MOVZ, ADD, SUB, MUL等）。V0-V31 NEONレジスタ管理なし
- **内容**: NEON SIMD命令（VADD, VMUL, VLD1, VST1, VDUP等）エンコーディング。V0-V31レジスタ管理。Advanced SIMD（128-bit）サポート
- **根拠**: ARMv8-A ISA。Apple Silicon最適化に重要
- **難易度**: Hard

#### FR-230: SIMD Register Allocation (SIMDレジスタ割り当て)

- **対象**: `src/emit/regalloc.lisp`
- **現状**: 整数/ポインタレジスタのみの線形スキャン。XMM/YMM（x86-64）およびV0-V31（AArch64）のレジスタ圧力解析なし
- **内容**: 浮動小数点/SIMDレジスタクラスを追加し、整数レジスタとは独立にスピル/再ロードを管理。レジスタクラス間の移動命令（MOVD/MOVQ等）生成
- **根拠**: LLVM RegisterClass / GCC reg-alloc。FR-228/FR-229の前提条件
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

### Phase 49 — 文字列・制御フロー高度化

#### FR-234: String Builder / Batch Concatenation (文字列ビルダー)

- **対象**: `src/vm/strings.lisp`, `src/optimize/optimizer.lisp`
- **現状**: `vm-string-concat`（`strings.lisp:156-162`）は毎回新規文字列を生成。`(concatenate 'string a b c d)`は3回の中間文字列生成
- **内容**: 連鎖する文字列結合を検出し、最終長を事前計算して1回のバッファ確保で結合。`format`の定数フォーマット文字列もコンパイル時に分解してバッチ結合に変換
- **根拠**: Java StringBuilder / Go strings.Builder。文字列処理の中間割り当て除去
- **難易度**: Medium

#### FR-235: Defunctionalization (脱関数化)

- **対象**: `src/compile/cps.lisp`, `src/compile/codegen.lisp`
- **現状**: CPS変換（`cps.lisp`）で生成される継続はすべてクロージャとしてヒープ割り当て。クロージャの動的ディスパッチ（`vm-call`）が必要
- **内容**: 有限個の既知クロージャをタグ付きunion（defstruct + ecase）に変換。クロージャ割り当て→定数タグ生成、`vm-call`→`ecase`ディスパッチに置換。CPS継続の多くがプログラム全体で有限個なので高い適用率
- **根拠**: Reynolds defunctionalization / MLton whole-program。CPS変換コンパイラ（SML/NJ, Chicken Scheme）の標準手法
- **難易度**: Very Hard

#### FR-236: Decision Tree Optimization for case/cond (判定木最適化)

- **対象**: `src/expand/macros-basic.lisp`, `src/compile/codegen.lisp`
- **現状**: `macros-basic.lisp:271-291` — `typecase`は線形if-elseチェーンに展開。`case`もcond→if変換で線形探索
- **内容**: 整数`case`は密度に応じてジャンプテーブル（密）またはバイナリサーチ（疎）に変換。`typecase`はクラス階層を考慮した判定木に変換。FR-128（typecase jump table）の一般化
- **根拠**: GCC switch lowering / LLVM SwitchInst→LookupTable。分岐数Nに対してO(N)→O(log N)またはO(1)
- **難易度**: Medium

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

### Phase 53 — GC高度化 (Ephemeron・Unwind)

#### FR-246: Ephemerons (エフェメロン)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: FR-184で弱参照・ファイナライザを定義しているが、エフェメロン（キーが到達不能になった場合のみ値も回収されるkey-valueペア）は未定義。`gc.lisp`に弱参照トラッキングなし
- **内容**: エフェメロン型を追加。GCマーキングフェーズでキーの到達可能性を判定し、キーが到達不能なら値もマークしない。固定点反復で連鎖エフェメロンを正しく処理。弱ハッシュテーブルの基盤
- **根拠**: Racket / Java WeakHashMap / JavaScript WeakRef+FinalizationRegistry。SBCL sb-ext:make-ephemeron。シンボルテーブル・キャッシュの正確なGC
- **難易度**: Hard

#### FR-247: Unwind Tables / .eh_frame Generation (アンワインドテーブル)

- **対象**: `src/emit/binary/elf.lisp`, `src/emit/binary/macho.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: ELF出力（`elf.lisp`）は`.text`, `.rela.text`, `.symtab`, `.strtab`, `.shstrtab`のみ。Mach-O出力にも例外テーブルセクションなし。FR-195（DWARF debug info）は定義済みだがアンワインド情報は別
- **内容**: `.eh_frame`セクション（DWARF CFI）を生成し、各関数のプロローグ/エピローグでのスタックフレームレイアウト変化を記述。外部例外ハンドラ（OS signal handler、C++ interop）がCLスタックを正しくアンワインド可能に。`__unwind_info`（compact unwind）のMach-O版も生成
- **根拠**: LSB (Linux Standard Base) / System V ABI。ネイティブデバッガ・プロファイラ連携に必須。FR-195（DWARF）の前提条件
- **難易度**: Hard

---

### Phase 54 — CL固有引数最適化

#### FR-248: &rest Parameter Stack Allocation (&restスタック割り当て)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen-functions.lisp`
- **現状**: `vm.lisp:823-825` — `&rest`パラメータは常にヒープにリスト割り当て（`vm-build-list`呼び出し）。restリストがエスケープしない場合でもGC圧力を発生
- **内容**: エスケープ解析（FR-007）と連携し、`&rest`リストが関数スコープ内でのみ使用される場合にスタック上に割り当て。`apply`で別関数に渡される場合のみヒープ割り当てにフォールバック
- **根拠**: SBCL dynamic-extent &rest / CCL stack-consed rest。CL固有の頻出パターン
- **難易度**: Medium

#### FR-249: &key Keyword Argument Hash Dispatch (&keyハッシュディスパッチ)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen-functions.lisp`
- **現状**: `vm.lisp:829-832` — 各キーワードパラメータに対して`(position keyword kw-args)`で線形探索。N個のキーワード引数でO(N²)
- **内容**: キーワード引数が4個以上の関数で、呼び出し時にキーワードハッシュテーブルを構築してO(1)ルックアップに変更。コンパイル時に引数位置が判明している場合は直接スロットアクセスに最適化（known-call optimization FR-030連携）
- **根拠**: SBCL keyword-arg optimization。多数のキーワード引数を持つ関数（CLOS初期化等）で顕著な改善
- **難易度**: Medium

#### FR-250: Specialized / Typed Array Compilation (型付き配列コンパイル)

- **対象**: `src/vm/list.lisp`, `src/compile/codegen.lisp`, `src/type/inference.lisp`
- **現状**: `list.lisp:458-777` — 配列操作は要素型に関わらず汎用パス。`(make-array 10 :element-type 'fixnum)`と`(make-array 10)`が同一コードを生成
- **内容**: `simple-array`サブタイプ（`fixnum`, `single-float`, `double-float`, `character`, `bit`）毎に特殊化された配列表現（パックド要素）を実装。`aref`/`(setf aref)`を要素型に応じてインライン化し型チェック除去。NaN-boxing不要の直接メモリアクセス
- **根拠**: SBCL specialized arrays / ANSI CL 15.1.2.2。数値計算・文字列処理の基盤
- **難易度**: Hard

---

### Phase 55 — 高度解析・メモリ最適化

#### FR-251: Abstract Interpretation Framework (抽象解釈フレームワーク)

- **対象**: `src/type/inference.lisp`, `src/optimize/optimizer.lisp`
- **現状**: HM型推論（`inference.lisp`）にad-hocな型絞り込み（`extract-type-guard`）あり。FR-116（Flow-Sensitive Narrowing）、FR-038（Range Analysis）は個別定義だが統一的な抽象ドメイン・格子構造なし
- **内容**: 抽象ドメイン（型格子、整数区間、定数、ポインタnull性）の統一フレームワーク。widening/narrowing演算子による固定点反復。SCCP（FR-010）・Range Analysis（FR-038）・Null Check Elimination（FR-040）を統一的に実装する基盤
- **根拠**: Cousot & Cousot (1977)。Astrée / Frama-C。個別解析パスを統一して精度・保守性を向上
- **難易度**: Very Hard

#### FR-252: Interprocedural Register Allocation (手続き間レジスタ割り当て)

- **対象**: `src/emit/regalloc.lisp`, `src/emit/calling-convention.lisp`
- **現状**: `regalloc.lisp` — 線形スキャンは関数単位。関数間のレジスタ使用パターン解析なし。呼び出し規約（`calling-convention.lisp`）はSystem V AMD64固定
- **内容**: 呼び出しグラフ解析に基づき、内部関数間でcaller-saved/callee-savedレジスタの使用をコーディネート。リーフ関数チェーン（A→B→C、全リーフ）でレジスタ保存/復元を省略。FR-176（Custom Calling Convention）と連携
- **根拠**: Wall (1986) interprocedural register allocation / LLVM interprocedural optimization。関数呼び出しオーバーヘッド削減
- **難易度**: Very Hard

#### FR-253: Copy-on-Write Data Structures (COWデータ構造)

- **対象**: `src/vm/list.lisp`, `src/vm/hash.lisp`, `src/runtime/heap.lisp`
- **現状**: cons, vector, hash-tableの変更操作は常にin-place変更。構造共有・遅延コピーなし
- **内容**: 大きなデータ構造（vector, hash-table）にCOWフラグを追加。`copy-seq`/`copy-list`は参照カウント増加のみで即座にコピーせず、最初の変更時にコピーを実行。参照カウントまたはwriteバリアで変更を検出
- **根拠**: PHP/Swift COW arrays / Clojure persistent data structures。大規模データのコピー操作を遅延化
- **難易度**: Hard

#### FR-254: Region-Based Memory Management (リージョンベースメモリ管理)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`, `src/compile/codegen.lisp`
- **現状**: 全割り当てが世代別GC管理。一時データ（コンパイル中間表現等）もGCトレース対象
- **内容**: `(with-region (r) body)` マクロでリージョンアロケータを提供。リージョン内の割り当てはバンプポインタのみ（GCルート登録不要）。リージョン脱出時に一括解放。コンパイラ自身の中間データ（AST、IR等）をリージョンで管理してGC圧力を大幅削減
- **根拠**: MLKit regions / Rust ownership / Go arena (1.20)。コンパイラ自身のselfhost性能改善に直結
- **難易度**: Hard

#### FR-255: Runtime Hash Consing (実行時ハッシュコンシング)

- **対象**: `src/vm/list.lisp`, `src/runtime/heap.lisp`
- **現状**: `egraph.lisp:42-49` — コンパイル時IR最適化にハッシュコンシング実装済み。しかしランタイムのcons操作は常に新規セル割り当て
- **内容**: `(hash-cons a b)` プリミティブを追加。同一`car`/`cdr`のconsセルをインターンテーブルで共有。構造的等価なリスト・ツリーのメモリ使用を大幅削減。弱ハッシュテーブル（FR-184/FR-246）でインターンテーブルのGC対応
- **根拠**: Ershov (1958) / BDD (Bryant 1986) / Lisp memoization。S式ベースIRの共有で自己コンパイル時のメモリ削減
- **難易度**: Medium

#### FR-256: Pure Function Auto-Memoization (純関数自動メモ化)

- **対象**: `src/optimize/optimizer.lisp`, `src/optimize/effects.lisp`
- **現状**: FR-152（Transitive Function Purity Inference）で関数の純粋性を推論する計画あり。`effects.lisp`に100+命令の副作用分類あり。しかし純粋と判明した関数の自動メモ化メカニズムなし
- **内容**: 純粋関数（副作用なし・参照透過性あり）と推論された関数に対して、呼び出し引数→戻り値のメモテーブルを自動挿入。テーブルサイズ上限・LRU evictionで制御。`(declare (optimize (speed 3)))`時のみ有効化
- **根拠**: GHC `{-# RULES "memo" #-}` / Mathematica automatic memoization。再帰的数値計算で指数的高速化
- **難易度**: Medium

---

### Phase 56 — 軽量並行性・スケジューラ

#### FR-257: Green Threads / M:N Threading (軽量スレッド)

- **対象**: `src/vm/vm.lisp`, `src/runtime/` (新規 scheduler.lisp)
- **現状**: `vm.lisp:342-372` — `vm-state`はシングルトン。OSスレッドのみ、ユーザースペースタスクスケジューラなし。FR-192（TLS）は定義済みだが軽量スレッド基盤なし
- **内容**: M:N スレッドモデル（M個のOSスレッド上にN個の軽量タスクをマルチプレクス）。協調プリエンプション（safepoint FR-233でyield）。FR-221（限定継続）上にコンテキストスイッチを構築。work-stealing対応のランキュー
- **根拠**: Go goroutines / Erlang processes / Tokio tasks。高並行性サーバーワークロードに必須
- **難易度**: Very Hard

#### FR-258: Work-Stealing Scheduler for Parallel GC (GC用ワークスティーリング)

- **対象**: `src/runtime/gc.lisp`
- **現状**: FR-190（並行GC）は並列マーキングを定義しているがタスク分配戦略なし。マークスタックの負荷分散メカニズムなし
- **内容**: 各GCスレッドにwork-stealing deque。マークスタックが空になったスレッドは他スレッドのdequeから盗む。Chase-Lev dequeアルゴリズム。アイドルスレッドの再分配
- **根拠**: Java G1 GC / OpenJDK CMS / Go GC。並列マーキング効率化に不可欠
- **難易度**: Hard

#### FR-259: GC Pause Time Budget / Incremental Scheduling (GCポーズ予算)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: STW GC（`gc.lisp:200-263,331-392`）にポーズ時間制御なし。ヒープサイズに比例してポーズ時間が線形増加
- **内容**: `*max-gc-pause-ms*`パラメータ（デフォルト10ms）。マーキング/スイープを複数インクリメンタルフェーズに分割し、各フェーズで予算超過検査。アロケーション速度に基づくGCペーシング（Go GOGC方式）
- **根拠**: Go runtime GOGC / HotSpot -XX:MaxGCPauseMillis / ZGC。大規模プログラムのポーズスケーリング対策
- **難易度**: Hard

#### FR-260: Lock Elision via Intel RTM (ロック省略)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: FR-191でアトミック操作（CAS, メモリフェンス）のみ定義。トランザクショナルメモリなし
- **内容**: ロック取得前に`XBEGIN`でトランザクション開始を試行。成功時はロック取得をスキップしクリティカルセクションを投機実行。`XABORT`時はフォールバックで従来ロックを使用。非競合パスで25-50%高速化
- **根拠**: Intel Haswell以降のRTM / AMD Ryzen TSX。非競合ロックのファストパス最適化
- **難易度**: Medium

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

### Phase 58 — データ表現最適化

#### FR-264: Pointer Compression (ポインタ圧縮)

- **対象**: `src/runtime/value.lisp`, `src/runtime/heap.lisp`
- **現状**: NaN-boxing（`value.lisp`）は48-bitポインタを使用。64-bitシステムでオブジェクト参照がフルサイズ
- **内容**: ヒープを4GBリージョンに制限し、ポインタを32-bitオフセットとして格納。ロード時にベースアドレス加算で復元。メモリ使用量を約40%削減。NaN-boxingのポインタフィールドを32-bit化
- **根拠**: V8 CompressedPointers / HotSpot CompressedOops (-XX:+UseCompressedOops)。メモリ効率の基本最適化
- **難易度**: Very Hard

#### FR-265: Small String Optimization (SSO / 短文字列最適化)

- **対象**: `src/vm/strings.lisp`, `src/runtime/value.lisp`
- **現状**: 全文字列がヒープ割り当て。1文字の文字列でもオブジェクトヘッダ＋ポインタ＋長さ＋バッファを消費
- **内容**: 7バイト以下の文字列をNaN-boxingのペイロード内にインライン格納（ヒープ割り当て回避）。タグビットで短文字列/ヒープ文字列を区別。シンボル名・キーワード等の短文字列が大幅に高速化
- **根拠**: C++ std::string SSO / Swift String inline storage。文字列操作のGC圧力を大幅削減
- **難易度**: Hard

#### FR-266: Object Header Compression (オブジェクトヘッダ圧縮)

- **対象**: `src/vm/vm-clos.lisp`, `src/runtime/heap.lisp`
- **現状**: CLOSインスタンスはハッシュテーブルベース（FR-214で改善予定）。オブジェクトメタデータ（型タグ、GCビット、サイズ）が分散
- **内容**: オブジェクトヘッダを1ワードに圧縮: `[type-tag(8bit) | gc-bits(4bit) | shape-id(20bit) | size(32bit)]`。FR-214（Object Shape）のshape IDをヘッダに埋め込み、クラスポインタ不要に。GCスキャン高速化
- **根拠**: HotSpot mark word / V8 Map pointer / GraalVM hub pointer。オブジェクトあたり8-16バイト節約
- **難易度**: Hard

---

### Phase 59 — コード生成・リンカ高度化

#### FR-267: Branch Prediction Hints (分岐予測ヒント)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: 全分岐が等確率として扱われる。エラーパス・例外パスも通常パスと同じコードレイアウト
- **内容**: 静的ヒューリスティック: `signal`/`error`呼び出しを含むブロック→cold（unlikely）。ループバックエッジ→taken。`(declare (optimize ...))`と連携した分岐重みアノテーション。FR-036（Hot/Cold Layout）のメタデータソース
- **根拠**: GCC __builtin_expect / LLVM branch_weights metadata。分岐予測ミス削減
- **難易度**: Easy

#### FR-268: AArch64 Constant Islands / Literal Pools (ARM定数プール)

- **対象**: `src/emit/aarch64-codegen.lisp`
- **現状**: AArch64バックエンドは最小限（8命令型のみ）。大きな即値のロード戦略なし。MOVZ/MOVKの2-4命令シーケンスでフルアドレスをロード
- **内容**: ADR/ADRP + LDR でPC相対定数プールからロード。定数プール（literal pool）を関数末尾に配置。4KBを超える関数では中間にconstant islandを挿入
- **根拠**: ARM Architecture Reference Manual。AArch64の即値制約（12-bit/16-bit）を回避する標準手法
- **難易度**: Medium

#### FR-269: Linker Relaxation (リンカリラクゼーション)

- **対象**: `src/emit/binary/elf.lisp`, `src/emit/binary/macho.lisp`
- **現状**: ELF出力は基本的なリロケーション（`R_X86_64_PLT32`等）のみ。リンク時の命令緩和なし
- **内容**: リンク時に間接呼び出し（`call [GOT]`）を直接呼び出し（`call rel32`）に変換（ターゲットが同一リンク単位にある場合）。RISC-V向けにはlong branch→short branchの緩和。リロケーションエントリにリラクゼーションメタデータ追加
- **根拠**: LLD / GNU ld linker relaxation / RISC-V linker relaxation specification。呼び出しオーバーヘッドの自動削減
- **難易度**: Hard

#### FR-270: FFI Call Marshaling Specialization (FFIマーシャリング特殊化)

- **対象**: `src/compile/codegen.lisp`, `src/emit/calling-convention.lisp`
- **現状**: FR-194でFFI基盤を定義しているが、マーシャリングコード生成の最適化なし
- **内容**: FFI呼び出しの型シグネチャに基づいて専用マーシャリングスタブをコンパイル時生成。POD型（int, double, pointer）はインラインでレジスタ直接渡し（ゼロコピー）。構造体型は型レイアウトに基づくメモリコピー最適化
- **根拠**: CFFI/PyPy JIT FFI / Java JNI critical natives。FFIオーバーヘッド5-10x削減
- **難易度**: Hard

---

### Phase 60 — SSA高度化・言語機能

#### FR-271: Trivial Phi Elimination (自明Phi除去)

- **対象**: `src/optimize/ssa.lisp`
- **現状**: `ssa.lisp:68-98` — `ssa-place-phis`が活性チェックなしで全支配フロンティアにPhiを挿入。冗長なPhi（全引数が同一値のPhi）の除去パスなし
- **内容**: Phi最適化3パス: (1) 全引数同一のPhiを単一値に置換、(2) Phi-of-Phi連鎖の短絡、(3) 未使用Phiの除去。FR-114（Pruned SSA）と相補的。FR-147（SSA統合）後に適用
- **根拠**: Cytron et al. (1991) Section 5。SSA構築直後の標準クリーンアップ。GVN/SCCPの精度向上に寄与
- **難易度**: Easy

#### FR-272: Read Barrier Optimization (読み取りバリア最適化)

- **対象**: `src/runtime/gc.lisp`, `src/compile/codegen.lisp`
- **現状**: `gc.lisp:269-295` — SATB書き込みバリア実装済み。読み取りバリアなし。FR-190（並行GC）はマーキングの並行化だが、読み取りバリアの戦略未定
- **内容**: 並行コンパクション（FR-213）実行中のオブジェクト参照に読み取りバリアを挿入。Brooks pointer方式（各オブジェクトに転送ポインタ）またはload-reference-barrier方式（Shenandoah）。コンパクション非実行中はバリアをNOP化
- **根拠**: Shenandoah LRB / ZGC colored pointers / Azul C4。低レイテンシGCに必須
- **難易度**: Very Hard

#### FR-273: Rational Arithmetic Specialization (有理数演算特殊化)

- **対象**: `src/vm/vm-numeric.lisp`, `src/compile/codegen.lisp`
- **現状**: `vm-numeric.lisp:566-621` — 有理数演算（rational, numerator, denominator, gcd, lcm）は全てホストCLにデリゲート。型特殊化なし
- **内容**: p/q ⊕ p'/q' を直接計算する特殊化パス。`(+ 1/3 1/4)` → コンパイル時にGCD計算して`7/12`に畳み込み。実行時はfixnum分子/分母の場合にfast path。fixnum + rational の混合演算最適化
- **根拠**: SBCL rational arithmetic / GMP mpq。数値計算の精度保証パス
- **難易度**: Medium

#### FR-274: Extensible Sequences Protocol (拡張可能シーケンスプロトコル)

- **対象**: `src/vm/list.lisp`, `src/compile/codegen.lisp`
- **現状**: シーケンス操作（map, reduce, find等）はlist/vectorのみ対応。ユーザー定義シーケンス型へのディスパッチ不可
- **内容**: SBCL `sb-sequence` プロトコルの実装。`sequence`クラスを継承したユーザー定義型に対して標準シーケンス関数が動作。`elt`, `length`, `make-sequence-like`, `adjust-sequence`のジェネリック関数基盤
- **根拠**: SBCL sb-sequence / Christophe Rhodes (2007)。ユーザー定義コレクションの相互運用性
- **難易度**: Hard

#### FR-275: Package-Local Nicknames (パッケージローカルニックネーム)

- **対象**: `src/vm/packages.lisp`, `src/parse/cl/parser.lisp`
- **現状**: パッケージシステムはグローバルニックネームのみ。パッケージ毎のローカルエイリアスなし
- **内容**: `(defpackage :foo (:local-nicknames (:a :alexandria)))` でパッケージ`foo`内でのみ`:a`が`:alexandria`を指す。シンボル解決時に現在パッケージのlocal-nickname-alistを参照。`add-package-local-nickname` / `remove-package-local-nickname` API
- **根拠**: SBCL / ECL / CCL / ABCL 全実装済み。de facto標準のCL拡張。CDR-10
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

### Phase 62 — 数値演算最適化

#### FR-282: Division by Arbitrary Constant (任意定数除算最適化)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: `optimizer.lisp:847-898`で2のべき乗除算のみシフトに変換。任意定数（3, 5, 7等）による除算はIDIV命令のまま
- **内容**: Granlund-Montgomery法による魔法数乗算。`(floor x 3)` → `IMUL + SHR` の2命令に変換。コンパイル時に魔法数M・シフト量Sを計算し、`x*M >> S`で商を得る。符号付き/符号なし両対応
- **根拠**: Granlund & Montgomery (1994) / GCC・LLVM・MSVC全実装。IDIV(20-90cycles)→IMUL+SHR(3-4cycles)で20x高速化
- **難易度**: Hard

#### FR-283: Multiply-High Instructions (乗算上位ワード命令)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: x86-64 codegenは`IMUL`（符号付き乗算、単一レジスタ結果）のみ。`MUL RDX:RAX`（128bit結果）のエミッションなし
- **内容**: `MUL`/`IMUL`の128bit結果形式（RDX:RAX）のエミッション。AArch64では`UMULH`/`SMULH`命令。FR-282（定数除算）の前提。bignumの高速乗算にも使用
- **根拠**: x86-64 ISA / ARM Architecture Reference。定数除算・bignum演算の基盤命令
- **難易度**: Medium

#### FR-284: Rotate Instructions (ビット回転命令)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: `x86-64-codegen.lisp`にシフト命令（`emit-vm-ash`）のみ。回転命令なし。`(logior (ash x k) (ash x (- 64 k)))` パターンの検出なし
- **内容**: パターンマッチで`(logior (ash x k) (ash x (- width k)))`を検出し`ROL`/`ROR`（x86-64）/ `ROR`（AArch64）に変換。新規`vm-rotate`命令の追加。暗号アルゴリズム（SHA, AES）で頻出
- **根拠**: GCC/LLVM rotate idiom recognition。暗号・ハッシュ計算の標準最適化
- **難易度**: Medium

#### FR-285: Byte Swap Instruction (バイトスワップ命令)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: バイト順変換パターンの検出なし。`BSWAP`命令のエミッションなし
- **内容**: バイト逆順パターン（4連続`ash`+`logand`+`logior`）を検出し`BSWAP`（x86-64）/ `REV`（AArch64）に変換。ネットワークバイトオーダー変換（`ntohl`/`htonl`相当）の高速化
- **根拠**: GCC/LLVM bswap recognition。ネットワーク・バイナリI/Oの標準最適化
- **難易度**: Easy

#### FR-286: Transcendental Math Native Emission (超越関数ネイティブエミッション)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`, `src/vm/vm-numeric.lisp`
- **現状**: `vm-numeric.lisp:148-264`に`vm-sqrt`, `vm-sin-inst`, `vm-cos-inst`, `vm-exp-inst`, `vm-log-inst`等のVM命令が定義済み。`builtin-registry.lisp:89-100`に登録済み。しかしx86-64/AArch64バックエンドにエミッタなし（`x86-64-codegen.lisp:1024-1076`のエミッタテーブルに未登録）
- **内容**: `SQRTSD`（x86-64 SSE2）/ `FSQRT`（AArch64）の直接エミッション。`sin`/`cos`/`exp`/`log`はlibm呼び出しまたはMinimax多項式近似。FR-228（SSE/AVX）の基盤
- **根拠**: SBCL `sb-vm::sqrtsd` / GCC `-ffast-math`。数値計算のVM解釈オーバーヘッド除去
- **難易度**: Hard

#### FR-302: Modulo Power-of-2 Strength Reduction (2のべき乗剰余最適化)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `opt-pass-strength-reduce`（`optimizer.lisp:853-898`）は`vm-mul`のみ処理。`vm-mod`/`vm-rem`分岐なし。代数恒等式テーブル（`optimizer.lisp:265`）に`(mod 0 x) -> 0`のみ
- **内容**: `(mod x 2^k)` → `(logand x (1- 2^k))`（非負整数の場合）。符号付きの場合は条件付き補正が必要。`(rem x 2^k)`は符号意味論が単純
- **根拠**: GCC/LLVM/MSVC全実装。IDIV不要でAND命令1つに変換。FR-096（除算）の剰余版
- **難易度**: Easy

#### FR-303: Overflow Detection with Hardware Flags (ハードウェアフラグによるオーバーフロー検出)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: `emit-vm-add`（`x86-64-codegen.lisp:255-262`）は`MOV+ADD`のみでフラグチェックなし。全算術演算がオーバーフロー時にサイレントラップアラウンド。`JO`/`INTO`/`ADDS`命令の使用なし
- **内容**: ADD/SUB/IMUL後にOF（Overflow Flag）を検査（x86-64 `JO`、AArch64 `ADDS`/`SUBS`のV flag）。オーバーフロー時にbignum昇格スローパスに分岐。CL仕様のfixnum→bignum自動昇格に必須
- **根拠**: SBCL `pseudo-atomic` + overflow trap / ANSI CL fixnum→bignum promotion。数値計算の正確性保証
- **難易度**: Hard

#### FR-304: Integer Range Analysis (整数範囲解析)

- **対象**: `src/optimize/optimizer.lisp`, `src/type/inference.lisp`
- **現状**: `opt-pass-fold`（`optimizer.lisp:341-439`）は定数値のみ追跡。範囲/区間の伝播なし。型システム（`src/type/`）にHM型推論はあるが整数範囲型（`(integer 0 255)`）なし
- **内容**: 値範囲解析: 算術演算を通じて`[min, max]`区間を伝播。両オペランドがfixnum範囲内かつ結果が確実にfixnum範囲なら、オーバーフロー検査をスキップ（FR-303と相補的）。制御フロー合流点でのラティスベース解析
- **根拠**: LLVM `LazyValueInfo` / GCC VRP (Value Range Propagation)。オーバーフロー検査除去で分岐を削減
- **難易度**: Hard

#### FR-305: Multiply by Constant via Shifts+Adds (定数乗算のシフト+加算変換)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `opt-pass-strength-reduce`（`optimizer.lisp:869-897`）は`opt-power-of-2-p`チェックのみ。`(* x 3)` → `(+ x (ash x 1))`等の分解なし
- **内容**: 少数のセットビットを持つ定数乗算をshift-add連鎖に分解。`(* x 3)` → `(+ x (ash x 1))`、`(* x 5)` → `(+ x (ash x 2))`、`(* x 10)` → `(ash (+ x (ash x 2)) 1)`。Bernstein法/Lefèvre法による最適連鎖探索
- **根拠**: GCC/LLVM multiply-by-constant optimization。IMUL(3cycles)→ADD+SHL(2cycles)で高速化
- **難易度**: Medium

#### FR-306: Optimizer Cost Model for Inlining (インライン化コストモデル)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: インライン閾値は生の命令数15（`optimizer.lisp:773`、`(1- (length body)) <= threshold`）。命令コスト/利益分析なし。E-graphにはコストモデル（`egraph.lisp:319-335`）があるがメインオプティマイザには無関係
- **内容**: 命令別コストテーブル（call=10, add=1等、E-graphの`*egraph-op-base-costs*`を共有可能）。インラインコスト=本体コスト合計 vs 呼び出しオーバーヘッド。レジスタ圧力（呼び出し地点の生存レジスタ数）を考慮。ループ内呼び出しはbonus加算
- **根拠**: LLVM `InlineCost` / GCC `ipa-inline`。命令数だけでは判断が粗い
- **難易度**: Medium

---

### Phase 63 — キャッシュ・メモリ最適化

#### FR-287: Loop Tiling / Strip Mining (ループタイリング)

- **対象**: `src/optimize/optimizer.lisp`, 新規`src/optimize/loop-transforms.lisp`
- **現状**: ループ最適化はFR-003（LICM）・FR-022（Unrolling）が計画済みだが、タイリング/ブロッキングは未計画。ネストループの空間的局所性最適化なし
- **内容**: 行列演算等の多重ループをキャッシュラインサイズ（64B）に合わせたタイルに分割。`(dotimes (i N) (dotimes (j M) ...))` → タイルサイズTでブロッキング。L1/L2キャッシュ容量に基づくタイルサイズ自動決定
- **根拠**: LLVM LoopTiling / GCC `-floop-nest-optimize` / Polly。行列演算で10-100x高速化の事例
- **難易度**: Very Hard

#### FR-288: Large Page Support (ラージページサポート)

- **対象**: `src/runtime/heap.lisp`, `src/runtime/gc.lisp`
- **現状**: ヒープ割り当てに標準4KBページのみ使用。`madvise`/`mmap`フラグの最適化なし
- **内容**: ヒープ領域に2MB huge page（`MAP_HUGETLB`/`VM_FLAGS_SUPERPAGE_SIZE_2MB`）を使用。`madvise(MADV_HUGEPAGE)`でTHPヒント。GCリージョンのページサイズ対応。ヒープサイズに応じた1GB huge page対応
- **根拠**: HotSpot `-XX:+UseLargePages` / V8 `--huge-max-old-generation-size`。TLBミス削減でメモリ集約型ワークロード10-30%高速化
- **難易度**: Medium

#### FR-289: Stride-Based Automatic Prefetch (ストライドベース自動プリフェッチ)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: FR-187（手動プリフェッチヒント挿入）が計画済みだが、自動ストライド検出なし
- **内容**: ループ内のメモリアクセスパターンを静的解析し、一定ストライドでのアクセスを検出。プリフェッチ距離をストライド×ループ反復回数から自動計算。ポインタチェイス（リスト走査・ツリー走査）パターンの自動検出とプリフェッチ挿入
- **根拠**: LLVM LoopDataPrefetch / GCC `-fprefetch-loop-arrays`。FR-187の自動化版
- **難易度**: Hard

#### FR-290: False Sharing Detection (フォールスシェアリング検出)

- **対象**: `src/compile/codegen-clos.lisp`, `src/runtime/heap.lisp`
- **現状**: FR-189（キャッシュライン対応オブジェクトレイアウト）でホットフィールド集約を計画。しかしスレッド間フォールスシェアリングの静的検出は未計画
- **内容**: 異なるスレッドが同一キャッシュライン上のフィールドに書き込むパターンを静的解析で検出。検出時にフィールド間にパディング挿入（`__attribute__((aligned(64)))`相当）。`@thread-local`アノテーションとの連携
- **根拠**: Intel VTune false sharing detection / perf c2c。並行プログラムの性能劣化の主因
- **難易度**: Hard

#### FR-307: Object Co-Location Hints (オブジェクト近接配置ヒント)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: `rt-gc-alloc`（`gc.lisp:23-44`）は単純なバンプアロケータ。`%gc-cheney-scan`（`gc.lisp:154-178`）はBFS順コピーで部分的な走査順局所性あり。しかしユーザー可視のヒントなし。オブジェクトヘッダ（`heap.lisp:16-22`）にアフィニティビットなし
- **内容**: 割り当てサイトカラーリングまたは`co-locate(a, b)`ヒントで関連オブジェクトを同一キャッシュライン/隣接ラインに配置。GCコピー時にアフィニティを維持。CLOSインスタンスとそのクラス記述子の近接配置
- **根拠**: HotSpot TLAB (Thread-Local Allocation Buffers) / Zing C4 co-location。親子オブジェクトの空間的局所性確保
- **難易度**: Hard

#### FR-308: Data Structure Flattening (データ構造フラット化)

- **対象**: `src/vm/vm-clos.lisp`, `src/vm/vm.lisp`
- **現状**: CLOSインスタンスはハッシュテーブル→ハッシュテーブル（`vm-make-obj`、`vm-clos.lisp:165-183`）。VMレジスタもハッシュテーブル（`vm-state-registers`、`vm.lisp:343`）。スロット読み取りは2+回のハッシュテーブルルックアップ。クロージャの捕捉値はalist（`vm-closure-captured-values`、`vm.lisp:31`）
- **内容**: CLOSインスタンスを連続配列（struct-like inline slot storage）にフラット化。レジスタファイルを固定サイズベクタに置換。小クロージャの捕捉値をインライン化（alistチェイン廃止）。各ホップ=潜在的キャッシュミスの除去
- **根拠**: V8 hidden class + inline slots / SpiderMonkey NativeObject。ポインタ追跡回避で10x高速化
- **難易度**: Hard

#### FR-309: Memory Access Pattern Analysis (メモリアクセスパターン解析)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/mir.lisp`
- **現状**: オプティマイザの8パス（`*opt-convergence-passes*`、`optimizer.lisp:1020-1031`）は全てレジスタレベル。メモリアクセス追跡なし。エイリアス解析なし（FR-017として計画のみ）。MIRに`:load`/`:store`演算（`mir.lisp:142-143`）あるが解析パスなし
- **内容**: `vm-slot-read`/`vm-slot-write`・配列操作を追跡し、シーケンシャル・ストライド・ランダムのアクセスパターンを検出。FR-289（プリフェッチ挿入）とFR-287（ループタイリング）のデータソース
- **根拠**: LLVM MemorySSA / GCC alias oracle。メモリレベル最適化の基盤解析
- **難易度**: Very Hard

---

### Phase 64 — ネイティブバックエンド補完

#### FR-291: ELF Executable Generation (ELF実行可能形式生成)

- **対象**: `src/emit/binary/elf.lisp`
- **現状**: `elf.lisp:3` — `ET_REL`（`.o`再配置可能ファイル）のみ生成。`+elf-type-rel+`定数（`elf.lisp:23`）のみ定義。実行可能形式なし
- **内容**: `ET_EXEC`（静的リンク実行可能形式）生成: プログラムヘッダ（`PT_LOAD`セグメント）、エントリポイント設定、`.text`+`.data`+`.bss`セグメント配置。`ET_DYN`（PIE実行可能/共有ライブラリ）: `.dynamic`セクション、`.dynsym`/`.dynstr`
- **根拠**: System V ABI / ELF Specification。`cc`→`.o`→外部リンカの依存を除去し、単体でネイティブバイナリ生成可能に
- **難易度**: Very Hard

#### FR-292: PE/COFF Windows Binary Support (PE/COFF Windowsバイナリ)

- **対象**: 新規`src/emit/binary/pe.lisp`
- **現状**: 完全未実装。Mach-O（macOS）とELF（Linux）のみ
- **内容**: PE/COFF（Portable Executable）形式の`.exe`/`.dll`生成。DOSスタブ、PEヘッダ、セクションテーブル（`.text`/`.rdata`/`.data`/`.reloc`）。インポートテーブル（IAT）・エクスポートテーブル生成。x86-64 Windows ABI（rcx/rdx/r8/r9レジスタ渡し、シャドウスペース）対応
- **根拠**: Microsoft PE/COFF Specification。クロスプラットフォーム対応に必須
- **難易度**: Very Hard

#### FR-293: Mach-O Relocation & Code Signature (Mach-O再配置・コード署名)

- **対象**: `src/emit/binary/macho.lisp`
- **現状**: ヘッダ・セグメント・セクション書き出し済み（`macho.lisp`）。`add-symbol`定義済み（`macho.lisp:376-396`）だがシリアライズ未実装。PIEフラグ設定済み（`macho.lisp:428`）だがPICコード未生成。再配置エントリなし。`LC_DYLD_INFO`/`LC_LOAD_DYLIB`（dyld動的リンク）なし。`LC_CODE_SIGNATURE`（コード署名）なし
- **内容**: 再配置エントリ（`GENERIC_RELOC_*`/`X86_64_RELOC_*`）生成。`LC_DYLD_INFO_ONLY`（バインド/リバインド情報）。`LC_LOAD_DYLIB`（libSystem.B.dylib等）。`LC_CODE_SIGNATURE`（Apple ad-hoc署名 or codesign連携）
- **根拠**: Apple Mach-O Reference / dyld source。macOS Catalina以降はコード署名必須
- **難易度**: Hard

#### FR-294: x86-64 Missing Instruction Encoding (x86-64命令エンコーディング補完)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/x86-64.lisp`
- **現状**: IDIV命令未エミッション。関数プロローグ/エピローグ（push rbp/mov rbp,rsp/pop rbp/ret）なし。RIP相対アドレッシング未使用（`x86-64-codegen.lisp:100-111`でMOVベースのみ）
- **内容**: (1) IDIV/DIV命令エンコーディング（CDQ/CQO+IDIV）。(2) 標準プロローグ/エピローグ生成（push rbp, sub rsp, N, ..., leave, ret）。(3) RIP相対データ参照（`[rip+disp32]`）。(4) SIB（Scale-Index-Base）アドレッシング
- **根拠**: Intel 64 ISA Reference。実用的なネイティブコード生成に必須
- **難易度**: Hard

#### FR-295: AArch64 Missing Instruction Encoding (AArch64命令エンコーディング補完)

- **対象**: `src/emit/aarch64-codegen.lisp`, `src/emit/aarch64.lisp`
- **現状**: 除算命令（SDIV/UDIV）未実装。浮動小数点命令なし。複雑アドレッシングモード（pre/post-index、register offset）なし。スピルコード生成なし（`aarch64.lisp:12`でエラー）
- **内容**: (1) SDIV/UDIV整数除算。(2) FMOV/FADD/FSUB/FMUL/FDIV浮動小数点。(3) LDR/STR pre-index `[Xn, #imm]!`・post-index `[Xn], #imm`・register offset `[Xn, Xm]`。(4) STP/LDPペアロード/ストア
- **根拠**: ARM Architecture Reference Manual。AArch64バックエンドの実用化に必須
- **難易度**: Hard

#### FR-296: RISC-V Backend Implementation (RISC-Vバックエンド実装)

- **対象**: 新規`src/emit/riscv64-codegen.lisp`, `src/emit/target.lisp`
- **現状**: `target.lisp:150-165`にRISC-V 64ターゲット記述子（`*riscv64-target*`）が存在するが、命令エミッション・レジスタ割り当て・コード生成が完全未実装
- **内容**: RV64IMAFDC（整数+乗除算+アトミック+浮動小数点+圧縮）命令セットのエミッション。x0-x31レジスタ・f0-f31 FPレジスタの割り当て。RISC-V ABIに準拠した呼び出し規約（a0-a7引数、ra戻りアドレス）。リラクゼーション対応リロケーション
- **根拠**: RISC-V ISA Specification / RISC-V ELF psABI。オープンISAへの対応
- **難易度**: Very Hard

#### FR-297: WASM Backend Completion (WASMバックエンド完成)

- **対象**: `src/emit/wasm.lisp`
- **現状**: グローバル変数アクセス（`wasm.lisp:309-314`）・関数呼び出し（`wasm.lisp:323-329`）・クロージャ生成（`wasm.lisp:331-337`）が全てスタブ（コメント出力のみ）。`ref.null`を返すだけの実装
- **内容**: (1) `global.get`/`global.set`によるグローバル変数アクセス。(2) `call`/`call_indirect`による関数呼び出し。(3) `struct.new`/`array.new`（GC proposal）によるクロージャ生成。(4) `try`/`catch`/`throw`（exception handling proposal、定数`+wasm-try/catch/throw+`は`wasm-types.lisp:89-91`に定義済み）
- **根拠**: WebAssembly Specification / GC Proposal。ブラウザ・Edge環境での実行に必須
- **難易度**: Very Hard

#### FR-298: vm-print Backend Emission (vm-print バックエンドエミッション)

- **対象**: `src/emit/x86-64.lisp`, `src/emit/aarch64.lisp`, `src/emit/wasm.lisp`
- **現状**: `x86-64.lisp:84-86`・`aarch64.lisp:54-56`の両方で`(error "print backend emission is not implemented yet")`。ネイティブコンパイルされたプログラムが`print`/`format`を使用すると即クラッシュ
- **内容**: ランタイムの`write`/`write-string`関数への呼び出し生成。フォーマット文字列のコンパイル時解析（`format`ディレクティブの静的展開）。バッファリングI/Oへの最適化
- **根拠**: 全ネイティブバックエンドの基本I/O機能。`print`なしでは実用プログラムが動作しない
- **難易度**: Medium

---

### Phase 65 — MIR・命令選択

#### FR-299: MIR Instruction Selection Rules (MIR命令選択ルール)

- **対象**: 新規`src/emit/isel/x86-64-rules.lisp`, `src/emit/isel/aarch64-rules.lisp`, `src/emit/mir.lisp`
- **現状**: `mir.lisp`にSSAベースのMIRフレームワークが完成。`target.lisp:8-10`でターゲット別命令選択ルールファイルを参照するが、対応ファイルが全ターゲットで存在しない。MIR→ネイティブ命令のマッピングが欠落=バックエンドパイプラインが接続されていない
- **内容**: ターゲット別パターンマッチング: MIR命令→ネイティブ命令の変換ルール定義。Maximal Munch / BURS（Bottom-Up Rewrite System）アルゴリズム。FR-175（命令選択フレームワーク）の具体的ルール実装
- **根拠**: LLVM SelectionDAG / GCC RTL patterns。FR-175で定義されたフレームワークの実体化
- **難易度**: Very Hard

---

### Phase 66 — ランタイム補完

#### FR-300: Runtime Condition/Restart Handler (ランタイムcondition/restartハンドラ)

- **対象**: `src/runtime/runtime.lisp`
- **現状**: `runtime.lisp:462` — `rt-register-method`は`(declare (ignore ...)) nil`のスタブ。`runtime.lisp:481-490` — `rt-establish-handler`/`rt-remove-handler`/`rt-push-handler`/`rt-pop-handler`が全て`nil`を返すno-op。condition/restartシステムのランタイムサポートが完全に未実装
- **内容**: (1) ハンドラスタック: `rt-push-handler`でcondition型→ハンドラ関数のバインディングをスタックにpush。(2) `rt-establish-handler`でハンドラフレームを設置（`setjmp`相当のセーブポイント）。(3) `signal`時にハンドラスタックを走査しマッチするハンドラを呼び出し。(4) `restart-case`/`restart-bind`のリスタート登録・呼び出し
- **根拠**: ANSI CL 9.1 Condition System。CL標準の必須機能
- **難易度**: Hard

#### FR-301: rt-register-method Implementation (rt-register-methodの実装)

- **対象**: `src/runtime/runtime.lisp`
- **現状**: `runtime.lisp:462` — `(defun rt-register-method (gf specs method) (declare (ignore gf specs method)) nil)` — 引数を全て無視してnilを返す。ネイティブコンパイルされたCLOSメソッドの登録が不可能
- **内容**: ジェネリック関数のディスパッチテーブルにメソッドを登録。specializer（型）リストをキーとしてメソッドクロージャをハッシュテーブルに格納。`compute-applicable-methods`の実装。メソッドコンビネーション（`:before`/`:after`/`:around`）の呼び出し連鎖構築
- **根拠**: ANSI CL 7.6 Generic Functions。CLOS標準メソッドディスパッチ
- **難易度**: Hard

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

### Phase 69 — 呼び出し規約・VM高速化

#### FR-326: Register Snapshot Elimination for Known Calls (既知呼び出しのレジスタスナップショット省略)

- **対象**: `src/vm/vm.lisp`
- **現状**: `vm-save-registers`（`vm.lisp:777-794`）が全関数呼び出しでレジスタハッシュテーブル全体をコピー。`vm-restore-registers`は`clrhash`+全`maphash`コピーバック。呼び出しごとにO(n)
- **内容**: 既知関数への呼び出し（アリティと捕捉変数が静的に判明）では、呼び出し地点で生存するレジスタのみを保存。`regalloc.lisp`の`compute-live-intervals`から活性情報を`vm-call`に注釈
- **根拠**: LLVM callee-saved analysis / V8 register liveness。呼び出しオーバーヘッドの大幅削減
- **難易度**: Medium

#### FR-327: VM Argument Dedicated Slots (VM引数専用スロット)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **現状**: `vm-bind-closure-args`（`vm.lisp:796-835`）で引数をレジスタ名リストから`mapcar`でリスト収集（`vm.lisp:625`、毎回フレッシュリスト割り当て）、パラメータへの個別`vm-reg-set`
- **内容**: 固定引数レジスタ（`:ARG0`〜`:ARG7`）を導入し、一般レジスタファイルをバイパス。8引数以下の関数はリスト構築不要で直接レジスタ→レジスタ転送。`vm2-state`（`vm-run.lisp:181-201`）の固定256スロットベクタとも整合
- **根拠**: JVM invokestatic / x86-64 ABI引数レジスタ。呼び出しオーバーヘッド削減
- **難易度**: Medium

#### FR-328: Native CALL/RET Instruction Emission (ネイティブCALL/RET命令エミッション)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: `*x86-64-instruction-sizes*`で`vm-closure`=0、`vm-call`=0（`x86-64-codegen.lisp:909-911`）。ネイティブバックエンドに関数呼び出しエミッションが完全欠落。`calling-convention.lisp:8-24`の呼び出し規約インフラはレジスタ割り当てのみで使用
- **内容**: x86-64 `CALL rel32`命令エミッション。スタックフレームセットアップ（push return address, callee-saved regs）。`vm-ret`→`RET`命令+フレーム解体。FR-294（x86-64命令補完）の上位機能
- **根拠**: 全ネイティブコンパイラの基本機能。関数呼び出しなしではネイティブ実行が成立しない
- **難易度**: Hard

#### FR-329: Callee-Saved Register Usage Analysis (使用済みCallee-Savedレジスタ解析)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/emit/regalloc.lisp`
- **現状**: `x86-64-codegen.lisp:1105-1111` — プロローグが常にRBX/RBP/R12/R13/R14/R15の6レジスタをPUSH。使用有無に関係なく全保存。`regalloc.lisp:295-298`の割り当て結果はプロローグ生成に参照されない
- **内容**: レジスタ割り当て後に使用済みcallee-savedレジスタを走査。使用されたもののみPUSH/POP。リーフ関数やレジスタ使用の少ない関数で2-10バイト+スタック圧力削減
- **根拠**: GCC/LLVM/SBCL全実装。FR-072（Shrink-Wrapping）・FR-177（Callee-Save Elimination）の前提となる基本分析
- **難易度**: Easy

#### FR-330: Closure Capture Deduplication (クロージャキャプチャ重複排除)

- **対象**: `src/compile/codegen.lisp`, `src/compile/closure.lisp`
- **現状**: `codegen.lisp:333-335`（flet）・`codegen.lisp:405-408`（labels）で各クロージャが独立に捕捉変数リストを持つ。同スコープの兄弟クロージャ間でのキャプチャスロット共有なし
- **内容**: 同スコープの複数クロージャが同じ変数を捕捉する場合、共有環境レコードを割り当て各クロージャからそれを参照。割り当て削減とキャッシュ局所性向上。FR-043（環境フラット化）・FR-132（キャプチャトリミング）と相補的
- **根拠**: V8 shared function info / Chez Scheme shared environments。兄弟クロージャの環境共有
- **難易度**: Medium

---

### Phase 70 — GC高度化（追加）

#### FR-331: Old-Space Free-List Allocation Reuse (旧世代フリーリスト再利用)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: `%gc-sweep-old-space`（`gc.lisp:301-329`）がメジャーGCスイープ時にフリーリストを構築。`rt-gc-alloc`（`gc.lisp:23-44`）はヤング空間バンプポインタのみ使用。旧空間昇格（`gc.lisp:78-82`）もバンプポインタ（`old-free`）で`rt-heap-free-list`を完全無視
- **内容**: 旧空間昇格時にまず`rt-heap-free-list`からfirst-fit/best-fit割り当てを試行。フリーリスト枯渇時にバンプポインタにフォールバック。旧空間の断片化削減
- **根拠**: JVM CMS free-list / G1 region-based allocation。メジャーGC後のメモリ再利用
- **難易度**: Easy

#### FR-332: Precise GC Root Scanning (正確GCルートスキャニング)

- **対象**: `src/runtime/gc.lisp`, `src/vm/vm.lisp`
- **現状**: `rt-gc-add-root`（`gc.lisp:50-54`）は全ルートを型情報なしの不透明consセルとして扱う。`gc.lisp:232-236`のルートスキャンは`(integerp val)`チェックのみ。NaN-boxing（`value.lisp`）の型述語を活用せず
- **内容**: 各GCルートにポインタ/fixnum/doubleの型メタデータを付与。非ポインタルートのスキャンスキップ。`value.lisp:109-131`の型述語を活用したルート分類。FR-231（スタックマップ）と相補的
- **根拠**: HotSpot OopMap / V8 tagged pointer root scanning。GCルートスキャン高速化
- **難易度**: Medium

#### FR-333: Nursery Sizing Heuristics (ナーサリサイズヒューリスティクス)

- **対象**: `src/runtime/heap.lisp`
- **現状**: `*gc-young-size-words*`=128Kワード（1MB）、`*gc-old-size-words*`=512Kワード（4MB）がハードコード（`heap.lisp:30-34`）。ランタイムリサイズなし。GC統計（`gc.lisp:398-421`）は収集するが消費なし
- **内容**: 割り当てレート・昇格レート（`words-promoted`）・マイナーGC頻度に基づくナーサリサイズ適応。頻繁なGCではナーサリ拡大、昇格レートが高い場合は縮小。ヒープ拡張（`rt-heap-words`ベクタの再割り当て）
- **根拠**: HotSpot adaptive sizing / Go GC pacer。GCオーバーヘッドの自動最適化
- **難易度**: Medium

#### FR-334: Memory Pressure Callbacks (メモリ逼迫コールバック)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: ヤング空間枯渇時（`gc.lisp:40-42`）と旧空間枯渇時（`gc.lisp:80-81`）にハードエラー。OOM前にユーザーコードがキャッシュ解放等の対応を行うフックなし
- **内容**: `rt-heap-memory-pressure-hooks`スロット追加。設定可能な閾値（80%/90%占有率）でコールバック呼び出し。コールバック復帰でメモリ解放されていれば処理続行
- **根拠**: .NET GC.RegisterForFullGCNotification / Android onTrimMemory。OOM回避のユーザーフック
- **難易度**: Easy

#### FR-335: Write Barrier Young-to-Young Elision (Young→Youngストアバリア省略)

- **対象**: `src/runtime/gc.lisp`
- **現状**: `rt-gc-write-barrier`（`gc.lisp:269-295`）が毎ストアで旧空間チェック。Young→Youngストアはカードマーキング不要だがチェックコストを支払う
- **内容**: `obj-addr`がヤング空間内ならSATBスナップショット・カードマーキング両方をスキップするファストパス。`heap.lisp:256-260`の`rt-young-addr-p`をバリア入口で使用。エスケープ解析でナーサリ割り当て確定なら静的にバリア除去
- **根拠**: G1 GC / ZGC young-gen skip。バリアオーバーヘッド削減
- **難易度**: Easy

#### FR-336: GC-NaN-Boxing Integration (GCとNaN-Boxing値表現の統合)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/value.lisp`, `src/runtime/heap.lisp`
- **現状**: 3つの独立した値表現: (1) `value.lisp` NaN-boxing、(2) `heap.lisp`ヘッダベースタグ、(3) `vm.lisp`ハッシュテーブルヒープ+CLオブジェクト。GCは`(integerp val)`でポインタ判定（`gc.lisp:145-148, 232-234`）。`value.lisp:132-140`の`val-pointer-p`を使わない
- **内容**: GCスロットスキャンで`val-pointer-p`によるNaN-boxed値のポインタ判定。非ポインタ（fixnum/double/char）の正確な識別とスキャン省略。ヒープオブジェクトのスロットタイプ対応
- **根拠**: V8 tagged pointer GC / SpiderMonkey NaN-boxing GC。GCスキャン精度向上
- **難易度**: Medium

#### FR-337: Finalizer Ordering Guarantees (ファイナライザ実行順序保証)

- **対象**: `src/runtime/gc.lisp`
- **現状**: FR-184がファイナライゼーションキュー概念を定義するが、順序セマンティクスなし。現在`gc.lisp`にファイナライザインフラ自体が未実装
- **内容**: 参照グラフに基づくトポロジカルソート。AがBを参照し両方到達不能ならAのファイナライザが先に実行。ファイナライザ復活防止（ファイナライザキューから到達可能なオブジェクトはファイナライザ完了まで未回収）。ファイナライザスレッド/遅延実行
- **根拠**: Java `PhantomReference` ordering / Python PEP-442。ファイナライズの正確性保証
- **難易度**: Hard

---

### Phase 71 — コレクション・文字列最適化

#### FR-338: Runtime String Interning (ランタイム文字列インターニング)

- **対象**: `src/vm/strings.lisp`, `src/vm/vm.lisp`
- **現状**: `strings.lisp:435-441` — `vm-concatenate`は毎回新規文字列割り当て。`strings.lisp:517` — `vm-symbol-name`はホスト`symbol-name`にデリゲートしキャッシュなし。FR-137（定数プール）・FR-181（リテラルプール）はコンパイル時のみ
- **内容**: ランタイム文字列インターンテーブル。`symbol-name`結果・頻出文字列を重複排除。V8の内部文字列化に類似。GC統合（弱参照テーブル）
- **根拠**: V8 internalized strings / Java string interning。文字列メモリ使用量削減
- **難易度**: Medium

#### FR-339: Hash Table Size/Rehash Control (ハッシュテーブルサイズ/再ハッシュ制御)

- **対象**: `src/vm/hash.lisp`
- **現状**: `vm-make-hash-table`（`hash.lisp:132-140`）は`:test`のみ受理。`:size`/`:rehash-size`/`:rehash-threshold`未サポート。ANSI CL必須のアクセサ（`hash-table-size`等）もなし
- **内容**: `:size`ヒントを`make-hash-table`に伝達。`hash-table-size`/`hash-table-rehash-size`/`hash-table-rehash-threshold`/`hash-table-count`アクセサ追加。ANSI CL 18.1準拠
- **根拠**: ANSI CL 18.1必須仕様。大規模テーブルの初期再ハッシュ回避
- **難易度**: Easy

#### FR-340: Compile-Time Sequence/String Folding (コンパイル時シーケンス/文字列畳み込み)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `*opt-unary-fold-table*`（`optimizer.lisp:83-92`）は6エントリ（全て数値）。`*opt-type-pred-fold-table*`（`optimizer.lisp:94-104`）も6エントリ。`vm-string-length`/`vm-length`/`vm-car`/`vm-cdr`/`vm-stringp`/`vm-listp`のコンパイル時畳み込みなし
- **内容**: `(length "hello")` → `5`、`(string-upcase "foo")` → `"FOO"`、`(char "abc" 1)` → `#\b`、`(car '(1 2 3))` → `1`。定数引数でのシーケンス/文字列述語の静的評価
- **根拠**: GCC/LLVM `strlen`定数畳み込み / SBCL compile-time type folding。定数式の静的評価
- **難易度**: Easy

#### FR-341: HOF Macro Vector Path (高階関数マクロのベクタパス)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `mapcar`（`macros-stdlib.lisp:352`）・`every`（`382`）・`some`（`392`）・`find`（`431`）・`position`（`463`）・`count-if`（`487`）等が全て`dolist`専用。ベクタ引数での動作不可または非効率
- **内容**: マクロ展開時にtypecase分岐: リスト→`dolist`、ベクタ→`dotimes`+`aref`。ANSI CLシーケンス関数はリスト/ベクタ両対応必須。FR-274（拡張可能シーケンス）のユーザー定義型とは別に、標準型の基本対応
- **根拠**: ANSI CL 17.3 — シーケンス関数はリスト/ベクタ両方で動作必須。SBCL/CCL全実装
- **難易度**: Medium

#### FR-342: Dead Store Elimination for Collections (コレクション操作のデッドストア除去)

- **対象**: `src/optimize/optimizer.lisp`, `src/optimize/effects.lisp`
- **現状**: DCE（`optimizer.lisp:519-538`）は`vm-dst`未読の命令のみ除去。`vm-sethash`/`vm-aset`/`vm-rplaca`/`vm-rplacd`はdstなし（副作用専用）。同一キー/インデックスへの冗長書き込み除去不可
- **内容**: ストアターゲット（ハッシュキー・配列インデックス）追跡。後続ストアが同ターゲットを上書きし中間読み取りがなければ先行ストアを除去
- **根拠**: LLVM DeadStoreElimination / HotSpot C2 memory scheduling。冗長書き込み除去
- **難易度**: Hard

#### FR-343: Set Operations Hash Acceleration (集合演算ハッシュ高速化)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `remove-duplicates`（`macros-stdlib.lisp:509-515`）が`(member x acc)`でO(n²)。`union`（`518-530`）・`intersection`（`543-552`）・`set-difference`（`533-541`）も同様のO(n²)
- **内容**: 閾値長超過時にハッシュテーブルベースのメンバーシップテストに切り替え（全体O(n)）。SBCL内部実装と同等
- **根拠**: SBCL internal hash-based set ops。大リストでの集合演算O(n²)→O(n)
- **難易度**: Easy

#### FR-344: String/List Algebraic Simplification (文字列・リスト述語の代数的簡約)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `*opt-algebraic-identity-rules*`（`optimizer.lisp:254-282`）は数値/論理/比較のみ。`(null (cons x y))` → `nil`、`(consp nil)` → `nil`、`(listp (cons x y))` → `t`のルールなし
- **内容**: 型述語の生産者命令型に基づく簡約。`vm-cons`の出力は常にcons→下流の`vm-consp`/`vm-listp`/`vm-null-p`を定数に置換。既存定数畳み込みとは異なるメカニズム（型フロー解析）
- **根拠**: LLVM InstCombine type-based simplification。不要な型チェック除去
- **難易度**: Medium

---

### Phase 72 — 並行性・OS統合

#### FR-345: Atomic Operations (アトミック操作)

- **対象**: `src/vm/vm.lisp`, 新規`src/vm/atomics.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: 全VM状態アクセスがシングルスレッドのハッシュテーブル操作（`vm-state-registers`/`vm-global-vars`/`vm-function-registry`、`vm.lisp:343-371`で`make-hash-table`）。`vm-heap-alloc`（`vm.lisp:408-412`）は非アトミック`incf`
- **内容**: `vm-cas`/`vm-atomic-add`/`vm-atomic-load`/`vm-atomic-store`命令。ランタイム`rt-cas`/`rt-atomic-incf`。x86-64 `LOCK CMPXCHG`/`LOCK XADD`エミッション。FR-191（Lock Elision）の前提
- **根拠**: Java `java.util.concurrent.atomic` / C++ `<atomic>`。並行プログラミングの基盤
- **難易度**: Medium

#### FR-346: Memory Ordering Fences (メモリオーダリングフェンス)

- **対象**: `src/emit/target.lisp`, `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp`
- **現状**: フェンス/バリア命令なし。`target-desc`の`features`（`target.lisp:38-41`）にメモリモデル記述なし
- **内容**: `vm-fence`命令（`:acquire`/`:release`/`:seq-cst`セマンティクス）。x86-64 `MFENCE`/`LFENCE`/`SFENCE`。AArch64 `DMB`/`DSB`/`ISB`。FR-345（アトミック操作）と相補的
- **根拠**: C++11 memory model / Java Memory Model。弱順序アーキテクチャ（AArch64）での正確性保証
- **難易度**: Hard

#### FR-347: Thread-Safe VM Registries (スレッドセーフVMレジストリ)

- **対象**: `src/vm/vm.lisp`, `src/runtime/runtime.lisp`
- **現状**: `vm-function-registry`（`vm.lisp:361`）、`vm-global-vars`（`vm.lisp:364`）、`vm-class-registry`（`vm.lisp:353`）が全て非同期ハッシュテーブル。`rt-intern`（`runtime.lisp:398`）もロックなし
- **内容**: 読み書きロックまたはロックフリー並行ハッシュテーブル。`intern`/`find-symbol`の同期化。FR-192（TLS）・FR-345（Atomics）に依存
- **根拠**: JVM ConcurrentHashMap / SBCL package lock。マルチスレッド環境での正確性
- **難易度**: Hard

#### FR-348: OS Signal to CL Condition Mapping (OSシグナル→CLコンディション変換)

- **対象**: `src/runtime/runtime.lisp`, 新規`src/runtime/signals.lisp`
- **現状**: `runtime.lisp:469-498`にCLレベルcondition関数のみ。OSレベルシグナルハンドラなし。SIGINT→break、SIGSEGV→storage-condition、SIGFPE→arithmetic-error等の変換なし。CLIのREPLはread-line EOF依存（`main.lisp:274`）
- **内容**: POSIX `sigaction`ベースのハンドラ登録（FFI経由）。SIGFPE→`arithmetic-error`、SIGSEGV→`storage-condition`、SIGINT→`break`/デバッガ。ネイティブコード用ガードページによるスタックオーバーフロー検出
- **根拠**: SBCL signal handling / CCL `%install-signal-handler`。ハードウェア例外のCL統合
- **難易度**: Hard

#### FR-349: Memory-Mapped I/O (メモリマップドI/O)

- **対象**: `src/runtime/runtime.lisp`, 新規`src/runtime/mmap.lisp`
- **現状**: ファイルI/OはCLストリームのみ（`runtime.lisp:549-594`の`rt-open-file`は`(open path ...)`）。`mmap`/`munmap`/`mprotect`なし
- **内容**: `rt-mmap`/`rt-munmap`/`rt-mprotect`（FFIまたはsyscallエミッション経由）。大規模データのメモリマップドアクセス。ネイティブランタイムのヒープバッキングにmmap使用
- **根拠**: SBCL `sb-posix:mmap` / GC heap backing via mmap。大規模ファイル処理の標準手法
- **難易度**: Medium

#### FR-350: Core Image Save/Restore (コアイメージ保存/復元)

- **対象**: 新規`src/runtime/image.lisp`, `src/vm/vm.lisp`, `src/cli/main.lisp`
- **現状**: VM状態（`vm.lisp:342-373`）にハッシュテーブル・CLOSオブジェクト・ラベル付きクロージャが含まれるが全て非シリアライザブル。SBCL `save-lisp-and-die`相当の機能なし
- **内容**: VM状態シリアライザ（レジスタ・ヒープ・関数レジストリ・グローバル変数・クラスレジストリ）。イメージファイルフォーマット定義。`cl-cc save-image`/`cl-cc load-image` CLIコマンド。起動高速化
- **根拠**: SBCL `save-lisp-and-die` / ECL `ext:save-lisp-and-die`。デプロイ・配布の標準手法
- **難易度**: Hard

#### FR-351: Sandboxing / Resource Limits (サンドボックス/リソース制限)

- **対象**: `src/vm/vm.lisp`, `src/cli/main.lisp`
- **現状**: VM実行に命令数制限・メモリ上限・タイムアウトなし。`vm-heap-alloc`（`vm.lisp:408`）は枯渇時のみエラー。ソフトリミットなし
- **内容**: 命令カウント制限、ウォールクロックタイムアウト、ヒープサイズ上限（設定可能）、ファイルI/Oホワイトリスト。ネイティブコード向け`setrlimit`
- **根拠**: Cloudflare Workers / Deno / Lua sandbox。信頼されないコードの安全な実行
- **難易度**: Medium

#### FR-352: Hot Code Reloading (ホットコードリロード)

- **対象**: `src/vm/vm.lisp`, `src/compile/pipeline.lisp`
- **現状**: `vm-register-function`（`vm.lisp:988`）はベアな`(setf (gethash ...))`で、バージョニング・ロールバック・アトミックスワップなし
- **内容**: アトミック関数置換（旧バージョン保持）。実行中呼び出しの完了待ち後のスワップ。モジュールレベルのホットリロード（依存追跡付き）
- **根拠**: Erlang hot code loading / SBCL interactive redefinition。ライブシステムの更新
- **難易度**: Medium

#### FR-353: Foreign Thread Callbacks (外部スレッドコールバック)

- **対象**: `src/runtime/runtime.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: FFI機構なし。`macho.lisp:44`は`+mh-noundefs+`（動的リンクなし）。ELFにも`.dynsym`/`.dynamic`セクションなし。コールバックトランポリン生成なし
- **内容**: C呼び出し可能な関数ポインタ（CLクロージャをラップするトランポリン）生成。外部スレッド用スレッドローカルVM状態初期化。FR-194（FFI基盤）・FR-196（動的リンク）に依存
- **根拠**: CFFI `defcallback` / JNA `Callback`。C/C++ライブラリとの双方向相互運用
- **難易度**: Very Hard

---

### Phase 73 — ANSI CL標準準拠

#### FR-354: CCASE / CTYPECASE (修正可能case/typecase)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `ecase`（`macros-stdlib.lisp:111`）・`etypecase`（`macros-stdlib.lisp:120`）は存在。`ccase`/`ctypecase`は全codebase中ゼロマッチ
- **内容**: `ccase`はマッチ失敗時に`store-value`リスタート付き修正可能`type-error`をシグナル。`ctypecase`は型ベースの同等品。ANSI CL 5.3必須
- **根拠**: ANSI CL 5.3 — `ccase`/`ctypecase`は標準マクロ
- **難易度**: Easy

#### FR-355: DEFSETF / DEFINE-SETF-EXPANDER (setf展開プロトコル)

- **対象**: `src/expand/expander.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `setf`ハンドラ（`expander.lisp:410-432`）は`*setf-compound-place-handlers*`にハードコードされたplace分岐。ユーザーレベルの`defsetf`/`define-setf-expander`なし。`get-setf-expansion` APIなし
- **内容**: `(defsetf accessor updater)` 短縮形、`(defsetf accessor lambda-list (store-var) body)` 長形式、`(define-setf-expander place ...)` 完全プロトコル、`get-setf-expansion` API
- **根拠**: ANSI CL 5.1.2.5 — setf展開はユーザー拡張可能であるべき
- **難易度**: Medium

#### FR-356: HANDLER-BIND Non-Unwinding Semantics (handler-bind非巻き戻しセマンティクス)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/conditions.lisp`
- **現状**: `handler-bind`（`macros-stdlib.lisp:212-221`）が`handler-case`に展開（スタック巻き戻し）。ANSI CLの`handler-bind`はシグナル発生者の動的コンテキストでハンドラを実行し巻き戻さない
- **内容**: 非巻き戻しハンドラディスパッチ。VMがハンドラスタック上でハンドラクロージャを巻き戻しなしで実行。`signal`が複数ハンドラを通過可能に。FR-300（condition/restartランタイム）に依存
- **根拠**: ANSI CL 9.1.4.1 — handler-bindはスタックを巻き戻さない
- **難易度**: Hard

#### FR-357: Pretty Printer (プリティプリンタ)

- **対象**: 新規`src/vm/pprint.lisp`
- **現状**: `pprint`関連コードがcodebase中ゼロマッチ。`format`の`~W`/`~<`/`~:>`（論理ブロック）ディレクティブ未サポート。FR-320（コードフォーマッタ）はソースコードフォーマッティングでANSI CLプリティプリンタとは別
- **内容**: `pprint`、`pprint-logical-block`、`pprint-newline`、`pprint-indent`、`pprint-tab`、`set-pprint-dispatch`、`*print-pprint-dispatch*`。ANSI CL 22.2完全実装
- **根拠**: ANSI CL 22.2 — プリティプリンタは標準機能。SLIME/IDE統合に必要
- **難易度**: Hard

#### FR-358: Readtable API (リードテーブルAPI)

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: レキサーがリーダーマクロ（`#:`, `#+`, `#-`, `#.`）をハードコード分岐で処理。`set-macro-character`/`get-macro-character`/`set-dispatch-macro-character`/`copy-readtable`/`*readtable*`が全て未実装
- **内容**: リードテーブルオブジェクトモデル。`set-macro-character`/`set-dispatch-macro-character`によるユーザー定義リーダーマクロ。`copy-readtable`、`readtablep`、`readtable-case`。ANSI CL 23
- **根拠**: ANSI CL 23 — ユーザープログラマブルなリーダーはCLの特徴的機能
- **難易度**: Hard

#### FR-359: EQL Specializers in DEFMETHOD (defmethodのeql特化子)

- **対象**: `src/compile/codegen-clos.lisp`, `src/vm/vm.lisp`
- **現状**: `codegen-clos.lisp:156-160`がcons形式の特化子を一律処理。`vm-dispatch-generic-call`（`vm.lisp:596`）がクラス名でマッチ。`(eql value)`形式の処理なし。FR-124はEQL特化子の高速パス（最適化）だが基本機能自体が未実装
- **内容**: `(defmethod foo ((x (eql :keyword))) ...)`をサポート。ディスパッチテーブルにeql特化子エントリを別途格納。ディスパッチ時にeqlテスト→クラスマッチの順で探索
- **根拠**: ANSI CL 7.6.2 — eql特化子は標準CLOS機能
- **難易度**: Medium

#### FR-360: MAPLIST / MAPL / MAPCON (CDR写像関数)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `mapcar`/`mapc`/`mapcan`は実装済み（`macros-stdlib.lisp:352-379`）。`maplist`/`mapl`/`mapcon`はcodebase中ゼロマッチ
- **内容**: `maplist`はCARではなくサブリスト全体を関数に渡し結果を収集。`mapl`は副作用版。`mapcon`は結果を`nconc`。既存`mapcar`/`mapc`/`mapcan`と同パターンで`dolist`の代わりにCDR走査
- **根拠**: ANSI CL 17.2 — 標準マッピング関数
- **難易度**: Easy

#### FR-361: DO-SYMBOLS / DO-EXTERNAL-SYMBOLS (パッケージシンボル反復)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/`パッケージサブシステム
- **現状**: `with-package-iterator`（`macros-stdlib.lisp:306`）はスタブ（常に枯渇、`(lambda () (values nil nil nil nil))`を返す、line 309）。`do-symbols`/`do-external-symbols`/`do-all-symbols`はゼロマッチ
- **内容**: パッケージシンボルテーブルの実反復。VMレベルのパッケージ内部アクセス。ANSI CL 11.1
- **根拠**: ANSI CL 11.1 — パッケージ反復マクロは標準機能
- **難易度**: Medium

#### FR-362: Logical Pathnames (論理パス名)

- **対象**: `src/runtime/runtime.lisp`
- **現状**: `rt-make-pathname`/`rt-namestring`/`rt-merge-pathnames`（`runtime.lisp:581-594`）はホストCLにデリゲート。`logical-pathname`/`translate-logical-pathname`/`logical-pathname-translations`はゼロマッチ
- **内容**: 論理パス名ホスト定義、翻訳、解決。ANSI CL 19.3
- **根拠**: ANSI CL 19.3 — 論理パス名はポータブルシステム構成の標準機構
- **難易度**: Medium

#### FR-363: WITH-COMPILATION-UNIT (コンパイル単位)

- **対象**: `src/compile/pipeline.lisp`
- **現状**: `compile-file-to-native`（`pipeline.lisp:456`）は存在するが`with-compilation-unit`マクロなし。codebase中ゼロマッチ
- **内容**: `(with-compilation-unit (&key override) body)` — 未定義関数警告をコンパイル単位終了まで遅延。ANSI CL 3.2.2.2
- **根拠**: ANSI CL 3.2.2.2 — コンパイル単位は相互参照のある定義群の標準メカニズム
- **難易度**: Easy

#### FR-364: NaN-Boxing Tag Check Fusion (NaN-Boxingタグチェック融合)

- **対象**: `src/runtime/value.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: `value.lisp:109-166`の各型述語（`val-fixnum-p`/`val-cons-p`等）が独立にビットマスキング。連続型チェック（`typecase`等）で上位16ビット抽出を反復
- **内容**: 融合型スイッチコード生成: 上位16ビットを1回抽出し、タグ値でジャンプテーブル分岐。連続型述語間の冗長`logand`/`ash`をCSE除去
- **根拠**: V8 map-check fusion / SpiderMonkey type barrier。typecase高速化
- **難易度**: Medium

#### FR-365: COMPILER-MACRO-FUNCTION API (コンパイラマクロ関数API)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/expand/expander.lisp`
- **現状**: `define-compiler-macro`（`macros-stdlib.lisp:313-316`）は`nil`を返すスタブ。FR-126（コンパイラマクロレジストリ）は計画済みだが、`compiler-macro-function`/`(setf compiler-macro-function)` API及び`compiler-macroexpand-all`での呼び出しフックも欠落
- **内容**: `compiler-macro-function`/`(setf compiler-macro-function)` API。`compiler-macroexpand-all`（`expander.lisp:543`）でコンパイラマクロレジストリをチェック。FR-126の拡張
- **根拠**: ANSI CL 3.2.2.1.3 — コンパイラマクロは標準機能
- **難易度**: Medium

### Phase 74 — CPS/IR高度化

#### FR-366: Selective CPS Transformation (選択的CPS変換)

- **対象**: `src/compile/cps.lisp`, `src/compile/pipeline.lisp`
- **現状**: `cps.lisp:400-406` — `cps-transform*`が全式に一律CPS適用。直線コード（算術・データフロー）にも不要な継続ラムダを生成。`cps.lisp:106-107`で`ast-int`/`ast-var`も`(funcall ,k ...)`でラップ
- **内容**: 事前パスでAST各ノードを「CPS必要」(非局所脱出・第一級継続含む) vs「直接スタイル安全」に分類。CPS変換は前者のみに適用。~80%の直線コードで継続ラムダ生成を回避
- **根拠**: Kennedy (2007) "Compiling with Continuations, Continued" — selective CPS
- **難易度**: Medium

#### FR-367: ANF代替IR (A正規形によるCPS代替)

- **対象**: `src/compile/cps.lisp`, `src/compile/codegen-core.lisp`
- **現状**: CPS変換が過剰なadmin lambda生成（`cps.lisp:145-156`）。`codegen-core.lisp:126-165`の`compile-ast` for `ast-let`は既にlet列を効率処理可能
- **内容**: `anf-transform-ast`パスをCPS代替として追加。中間値をlet束縛で直列化し、継続ラムダを不要にする。FR-159のadmin redex除去自体が不要になる。パイプラインスイッチ`:ir-style :anf`/`:cps`
- **根拠**: Flanagan et al. (1993) "The Essence of Compiling with Continuations"
- **難易度**: Medium

#### FR-368: CPS Shrinking Reductions (CPS縮約最適化)

- **対象**: `src/compile/cps.lisp`
- **現状**: FR-026(η縮約)/FR-027(β縮約)は個別記載だが、Appel/Jim統合ワークリストアルゴリズム未記載。`cps.lisp:92-102`の`cps-transform-sequence`が多数の単一使用継続束縛を生成
- **内容**: Appel/Jim (1997)の縮約を統合CPS-levelパスとして実装。継続変数の使用回数censusを保持し、(a) 単一使用β収縮、(b) ηリダクション、(c) 死継続除去、(d) 定数畳み込み — をワークリスト駆動で準線形時間実行
- **根拠**: Appel & Jim (1997) "Shrinking Lambda Expressions in Linear Time"
- **難易度**: Medium

#### FR-369: SSA-CPS等価性ブリッジ (SSA-CPS Equivalence Bridge)

- **対象**: `src/emit/mir.lisp`, `src/compile/cps.lisp`, `src/optimize/optimizer.lisp`
- **現状**: MIR層（`mir.lisp`）はBraunアルゴリズムでSSA構築。CPS（`cps.lisp`）は独立IR。数学的等価（Kelsey 1995）だが相互変換・解析共有なし
- **内容**: CPS↔SSA双方向変換レイヤー構築。CPS単一呼び出し継続→SSA基本ブロックパラメータ、CPS合流点→SSAφノード。オプティマイザパスをIR横断で共有可能にする
- **根拠**: Kelsey (1995) "A Correspondence between CPS and SSA"
- **難易度**: Hard

#### FR-370: CPS AST Node Coverage (CPS ASTノード網羅)

- **対象**: `src/compile/cps.lisp`
- **現状**: `ast-defun`, `ast-defvar`, `ast-defmacro`, `ast-values`, `ast-multiple-value-bind`, `ast-apply`, `ast-handler-case`, `ast-defclass`, `ast-defgeneric`, `ast-defmethod`, `ast-make-instance`, `ast-slot-value`, `ast-set-slot-value`, `ast-set-gethash`の13ノードに`cps-transform-ast`メソッドなし — 呼び出し時にno applicable methodエラー
- **内容**: 13ノード型全てにCPS変換メソッド追加。`ast-node`にデフォルトフォールバックメソッドも追加
- **根拠**: CPS変換の完全性要件
- **難易度**: Medium

#### FR-371: CPS Host Control Flow Elimination (CPSホスト制御フロー除去)

- **対象**: `src/compile/cps.lisp`
- **現状**: `ast-block`/`ast-return-from`(`cps.lisp:182-197`)がホスト`block`/`return-from`を出力。`ast-tagbody`/`ast-go`(`cps.lisp:216-229`)がホスト`tagbody`/`go`を出力。`ast-catch`/`ast-throw`/`ast-unwind-protect`(`cps.lisp:233-279`)もホスト特殊形式を出力。ホスト以外のターゲットでCPS無効
- **内容**: 全非局所制御フローをCPS継続で実現。block→継続キャプチャ、return-from→継続呼び出し、tagbody→ラベル継続、go→ジャンプ継続。ネイティブバックエンド前提
- **根拠**: CPS変換の本来の目的 — 制御フローの明示化
- **難易度**: Hard

#### FR-372: CPS Continuation Deduplication in ast-if (CPS継続重複除去)

- **対象**: `src/compile/cps.lisp`
- **現状**: `cps.lisp:124-130` — `ast-if`で継続`k`がthen/else両方にテキスト複製。`k`が大きなラムダの場合コードサイズ爆発
- **内容**: 分岐前に`k`をlet束縛し、then/else両方で変数参照。標準的なCPS最適化
- **根拠**: コードサイズ削減、GHC/MLtonの標準手法
- **難易度**: Easy

#### FR-373: CPS Boolean Correctness (CPS真偽値修正)

- **対象**: `src/compile/cps.lisp`
- **現状**: `cps.lisp:129` — `ast-if`のCPSが`(if (zerop ,v) ...)`で条件判定。Common Lispの偽は`nil`のみ（0は真）
- **内容**: `(if (zerop ,v) ...)`を`(if (null ,v) ...)`または`(if ,v then else)`に修正
- **根拠**: ANSI CL仕様 — `nil`のみがfalse
- **難易度**: Easy

---

### Phase 75 — CLOS/MOP完全化

#### FR-374: Shared Slots / :allocation :class (クラス割り当てスロット)

- **対象**: `src/parse/cl/parser.lisp`, `src/parse/ast.lisp`, `src/vm/vm-clos.lisp`
- **現状**: `parser.lisp:136`で`:allocation`を黙って破棄。`ast-slot-def`(`ast.lisp:193-201`)にallocationフィールドなし。`vm-make-obj`(`vm-clos.lisp:165-183`)が全スロットをインスタンスHTにコピー
- **内容**: `ast-slot-def`にallocationスロット追加。`:allocation :class`のスロットはクラスHTに格納。`vm-slot-read`/`vm-slot-write`でクラス割り当てスロットをクラスHTから参照
- **根拠**: ANSI CL 7.5.3 — Slot Allocation
- **難易度**: Medium

#### FR-375: defclass Class Options (defclassクラスオプション)

- **対象**: `src/parse/cl/parser.lisp`, `src/parse/ast.lisp`, `src/vm/vm-clos.lisp`
- **現状**: `parser.lisp:512-522`がスロット仕様のみパース、5番目以降のクラスオプション完全無視。`ast-defclass`(`ast.lisp:203-207`)にdefault-initargs/metaclass/documentationフィールドなし
- **内容**: defclassローワーでクラスオプションをパース。`ast-defclass`にフィールド追加。`:default-initargs`をvm-class-def/vm-make-objで統合
- **根拠**: ANSI CL 7.7 — defclass
- **難易度**: Medium

#### FR-376: define-method-combination (ユーザ定義メソッド結合)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen-clos.lisp`
- **現状**: FR-215は`:before`/`:after`/`:around`のみ。`define-method-combination`がコードベースに一切なし。組み込み結合（`+`, `append`, `list`, `progn`, `and`, `or`, `max`, `min`, `nconc`）も欠落
- **内容**: 短形式`define-method-combination`（例: `(define-method-combination + :operator +)`）。長形式（`:arguments`, `:generic-function`）。GF-HTに結合タイプ格納。effective method構築で演算子適用
- **根拠**: ANSI CL 7.6.6 — Method Combination
- **依存**: FR-215
- **難易度**: Hard

#### FR-377: compute-applicable-methods / find-method / no-applicable-method (適用可能メソッドプロトコル)

- **対象**: `src/vm/vm.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `vm-get-all-applicable-methods`(`vm.lisp:574-594`)は内部関数でCLから呼び出し不可。`vm-resolve-gf-method`(`vm.lisp:867-911`)がメソッド不在時にハード`error`（`no-applicable-method` GF呼び出しなし）。`find-method`/`remove-method`/`add-method`関数なし
- **内容**: `compute-applicable-methods`をユーザコードから呼び出し可能に公開。`no-applicable-method`をカスタマイズ可能なGFとして実装。`find-method`/`remove-method`/`add-method`追加
- **根拠**: ANSI CL 7.6.6, 7.7.{14,30,35}
- **難易度**: Medium

#### FR-378: C3 Linearization for CPL (C3線形化によるCPL計算)

- **対象**: `src/vm/vm-clos.lisp`
- **現状**: `compute-class-precedence-list`(`vm-clos.lisp:119-133`)が単純深さ優先走査。ダイアモンド継承で不正順序を生成
- **内容**: C3線形化(Barrett et al. 1996)に置換。矛盾時エラーシグナル。SBCL/CCL互換
- **根拠**: ANSI CL 4.3.5 — Determining the Class Precedence List
- **難易度**: Medium

#### FR-379: initialize-instance as Generic Function (GFとしてのinitialize-instance)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen-clos.lisp`
- **現状**: `vm-make-obj`(`vm-clos.lisp:165-183`)が直接HTに設定。`initialize-instance` GF呼び出しなし。ユーザが`:after`メソッドでインスタンス初期化カスタマイズ不可
- **内容**: オブジェクト生成後に`initialize-instance` GFディスパッチ。デフォルトprimaryメソッドが現`vm-make-obj`ロジック実行。ユーザ`:after`メソッドが後続実行
- **根拠**: ANSI CL 7.1 — Object Creation and Initialization
- **依存**: FR-215, FR-375
- **難易度**: Hard

#### FR-380: change-class Slot Migration (change-classスロット移行)

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `macros-sequence.lisp:527-537` — `:__class__`の差し替えのみ。新クラスの新スロット追加なし、旧スロット削除なし、`update-instance-for-different-class`呼び出しなし
- **内容**: `:__class__`更新後に新クラスの`:__slots__`を走査し欠落スロット追加（initformで初期化）。旧クラス固有スロット削除。`update-instance-for-different-class`を旧コピーと新インスタンスで呼び出し
- **根拠**: ANSI CL 7.2 — Changing the Class of an Instance
- **難易度**: Medium

#### FR-381: vm-classify-arg Type Coverage (型分類拡張)

- **対象**: `src/vm/vm.lisp`
- **現状**: `vm-classify-arg`(`vm.lisp:854-865`)が`integer`, `string`, `symbol`, HT(CLOS)の4型のみ。`float`, `ratio`, `complex`, `character`, `cons`/`list`, `array`/`vector`, `function`, `null`等でメソッド特化失敗
- **内容**: `typecase`を全標準CL型指定子カバーに拡張
- **根拠**: CLOS仕様 — 型に基づくメソッドディスパッチ
- **難易度**: Easy

#### FR-382: defgeneric Options (defgenericオプション)

- **対象**: `src/parse/cl/parser.lisp`, `src/parse/ast.lisp`
- **現状**: `parser.lisp:526-532`がlambda-listのみパース。`:method-combination`, `:argument-precedence-order`, `:generic-function-class`, インラインメソッド定義を破棄。`ast-defgeneric`(`ast.lisp:209-212`)にname/paramsのみ
- **内容**: defgenericオプションのパース。GF記述子にmethod-combination型/argument-precedence-order格納。インライン`(:method ...)`をdefmethod生成に変換
- **根拠**: ANSI CL 7.7.5 — defgeneric
- **難易度**: Medium

#### FR-383: Method Qualifier AST Preservation (メソッド修飾子AST保持)

- **対象**: `src/parse/cl/parser.lisp`, `src/parse/ast.lisp`
- **現状**: `parser.lisp:548`で`(declare (ignore qualifier))`。`ast-defmethod`(`ast.lisp:214-219`)にqualifierスロットなし。FR-215のディスパッチ実装にAST情報が不足
- **内容**: `ast-defmethod`にqualifierスロット追加。パーサでqualifierを保持し、codegen-clos.lispでvm-register-methodに渡す
- **根拠**: FR-215実装の前提条件
- **依存**: FR-215
- **難易度**: Easy

#### FR-384: slot-unbound Protocol (slot-unboundプロトコル)

- **対象**: `src/vm/vm-clos.lisp`
- **現状**: `vm-slot-read`(`vm-clos.lisp:185-193`)が未設定スロットで生`error`シグナル。ANSI CL標準の`slot-unbound` GF呼び出しなし
- **内容**: 未束縛スロットアクセス時に`slot-unbound` GFを呼び出し。デフォルトメソッドが`unbound-slot`コンディションをシグナル。ユーザカスタマイズ可能
- **根拠**: ANSI CL 7.5.11 — slot-unbound
- **難易度**: Medium

#### FR-385: class-of / type-of for VM Objects (VMオブジェクトのclass-of/type-of)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: VMオブジェクトは`:__class__`キー付きHT。`class-of` VM命令なし。`typep`はユーザ定義クラスへのディスパッチ不可
- **内容**: `vm-class-of`命令追加 — HTの`:__class__`チェック。`vm-type-of`命令 — 標準CL型とCLOSクラス両方返却。`typep`でユーザ定義クラスのサブタイプ判定（CPLトラバーサル）
- **根拠**: ANSI CL 4.4.2 — class-of, type-of
- **難易度**: Medium

---

### Phase 76 — 数値・IO・ストリーム

#### FR-386: Numeric Type Predicates VM Instructions (数値型述語VM命令)

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `vm-number-p`/`vm-integer-p`は存在(`primitives.lisp:40-52`)。`floatp`/`rationalp`/`complexp`/`realp`のVM命令・builtin登録なし
- **内容**: `vm-floatp`, `vm-rationalp`, `vm-complexp`, `vm-realp`命令定義。builtin-registryに`:pred1`パターンで登録
- **根拠**: ANSI CL標準型述語
- **難易度**: Easy

#### FR-387: make-synonym-stream (同義ストリーム)

- **対象**: `src/vm/io.lisp`, `src/runtime/runtime.lisp`
- **現状**: `runtime.lisp:572-575`に`rt-make-broadcast-stream`等4種あるが`make-synonym-stream`がコードベース全体に不在
- **内容**: `rt-make-synonym-stream`をランタイムに追加。`vm-make-synonym-stream`命令をVMに追加。builtin-registry接続
- **根拠**: ANSI CL 21.1 — Stream Types
- **難易度**: Easy

#### FR-388: Gray Streams Protocol (Grayストリームプロトコル)

- **対象**: 新規`src/vm/gray-streams.lisp`, `src/compile/codegen.lisp`
- **現状**: `fundamental-stream`/`stream-read-char`/`stream-write-char`がsrc/に一切なし。全I/O操作(`io.lisp:253-266`)がホストCLストリームに委譲。ユーザ拡張可能なストリーム階層なし
- **内容**: CLOSベースストリームクラス（`fundamental-input-stream`等）とジェネリック関数（`stream-read-char`等）。VMディスパッチでGrayストリーム検出時にGFメソッド呼び出し
- **根拠**: Gray Streams (de facto standard, SBCL/CCL/LW全対応)
- **依存**: FR-215, FR-379
- **難易度**: Hard

#### FR-389: Format Directive Self-Implementation (FORMAT指示子自前実装)

- **対象**: 新規`src/vm/format.lisp`
- **現状**: `vm-format-inst`(`io.lisp:699-705`)がホストCLの`format`に100%委譲。自前フォーマット文字列パーサなし。FR-047はコンパイル時最適化のみ
- **内容**: 自前フォーマット文字列インタプリタ: `~R`(基数/英語), `~P`(複数形), `~[`(条件), `~{`(反復), `~/`(ユーザ関数), `~?`(再帰), `~*`(goto), `~;`(節区切り)
- **根拠**: 自己ホスティング完全化 — ホスト依存除去
- **難易度**: Very Hard

#### FR-390: print-object Generic Dispatch (print-objectジェネリックディスパッチ)

- **対象**: `src/vm/io.lisp`, `src/compile/codegen-clos.lisp`
- **現状**: `vm-princ`/`vm-prin1`/`vm-print-inst`(`io.lisp:594-610`)がホスト`princ`/`prin1`/`print`に委譲。VM CLOSインスタンス印刷時に`print-object` GFディスパッチなし
- **内容**: `:__class__`付きHTを印刷する際、登録済み`print-object`メソッドにディスパッチ。デフォルトメソッドが`#<ClassName>`出力
- **根拠**: ANSI CL 22.1.3 — print-object
- **依存**: FR-215
- **難易度**: Medium

#### FR-391: vm-parse-number (汎用数値パースVM命令)

- **対象**: `src/vm/strings.lisp`
- **現状**: `vm-parse-integer`(`strings.lisp:596-618`)のみ。浮動小数点・比率パースのVM命令なし。レキサー(`lexer.lisp:168-235`)は全数値型を処理可能だがVMから公開されていない
- **内容**: `vm-parse-number`命令 — 整数・浮動小数点・比率を文字列からパース
- **根拠**: ANSI CL 12.2 — Number Parsing
- **難易度**: Easy

#### FR-392: base-char / extended-char Type Distinction (文字サブタイプ区別)

- **対象**: `src/vm/primitives.lisp`, `src/vm/strings.lisp`
- **現状**: `vm-characterp`(`strings.lisp:589-594`)が全文字を一律扱い。`vm-typep-check`(`primitives.lisp:259`)が`character`のみ。`base-char`/`extended-char`区別なし
- **内容**: `vm-base-char-p`述語追加。`vm-typep-check`で`base-char`/`extended-char`型指定子ハンドリング
- **根拠**: ANSI CL 13.1.1 — Character Types
- **難易度**: Easy

#### FR-393: Composite Stream VM Instructions (複合ストリームVM命令)

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: ランタイムに`rt-make-broadcast-stream`等4種定義(`runtime.lisp:572-575`)。対応するVM命令なし — ランタイム関数はネイティブコンパイルコード専用でVMインタプリタから使用不可
- **内容**: `vm-make-broadcast-stream`, `vm-make-two-way-stream`, `vm-make-echo-stream`, `vm-make-concatenated-stream`命令追加。builtin-registry接続
- **根拠**: ANSI CL 21.1 — Composite Stream Types
- **難易度**: Medium

---

### Phase 77 — 宣言・環境・コンパイラポリシー

#### FR-394: Compiler Environment Objects (コンパイラ環境オブジェクト)

- **対象**: `src/compile/context.lisp`
- **現状**: `compiler-context`(`context.lisp:4-26`)の`env`が名前→レジスタのフラットalist。変数種別（lexical/special/symbol-macro/constant）区別なし。`inline`/`notinline`関数情報なし。宣言情報格納なし
- **内容**: `variable-information`, `function-information`, `declaration-information` (CLtL2 Ch.8.5)。変数種別タグ付き環境エントリ。宣言情報（type, ignore, optimize, dynamic-extent）格納スロット追加
- **根拠**: CLtL2 8.5 — Environments, SBCL sb-cltl2
- **難易度**: Medium

#### FR-395: Compiler Policy / Optimize Qualities (コンパイラポリシー)

- **対象**: `src/compile/context.lisp`, `src/expand/expander.lisp`
- **現状**: `(declare (optimize (speed 3) (safety 0)))`が黙って無視。`context.lisp`にpolicy/optimizeスロットなし
- **内容**: `*compiler-policy*` alist（`speed`, `safety`, `debug`, `space`, `compilation-speed` 各0-3, default 1）。`(declaim (optimize ...))`でグローバル設定。`(declare (optimize ...))`でローカルスコープ設定。codegen参照: 型チェック挿入(safety), デバッグ情報(debug)
- **根拠**: ANSI CL 3.3.4 — Optimize Declaration
- **難易度**: Medium

#### FR-396: DECLAIM Processing (declaim処理)

- **対象**: `src/compile/pipeline.lisp`, `src/expand/expander.lisp`
- **現状**: `pipeline.lisp:405` — `declaim`フォームを明示的にスキップ。expanderにdeclaimハンドラなし
- **内容**: `(declaim (optimize ...))` → グローバルコンパイラポリシー更新。`(declaim (special x))` → 変数をspecial宣言。`(declaim (inline f))` → インライン指示登録。FR-127(`ftype`のみ)を汎用declaimに拡張
- **根拠**: ANSI CL 3.8.1 — declaim
- **依存**: FR-395
- **難易度**: Low-Medium

#### FR-397: LOCALLY Declaration Propagation (locally宣言伝播)

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `macros-sequence.lisp:335-337` — `locally`マクロが`(progn body-without-declares)`に展開し全`declare`フォームを除去。`(declare (optimize (speed 3)))`等が黙って消失
- **内容**: `locally`内の宣言をコンパイラ環境に伝播。`ast-locally`ノード追加でコンパイラが`locally`と`progn`を区別可能にする
- **根拠**: ANSI CL 3.8.1 — locally
- **依存**: FR-394, FR-395
- **難易度**: Low

#### FR-398: define-symbol-macro (グローバルシンボルマクロ)

- **対象**: `src/expand/expander.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `symbol-macrolet`(FR-220)のローカル版のみ計画。`define-symbol-macro`がソース全体に不在。トップレベルグローバルシンボルマクロ定義不可
- **内容**: `(define-symbol-macro name expansion)` — グローバルシンボルマクロ登録。`compiler-macroexpand-all`で変数参照時にexpansion置換
- **根拠**: ANSI CL 3.8.1 — define-symbol-macro
- **依存**: FR-220
- **難易度**: Low

#### FR-399: eval-when Semantics Completeness (eval-whenセマンティクス完全化)

- **対象**: `src/expand/expander.lisp`
- **現状**: `expand-eval-when-form`(`expander.lisp:195-212`)が不完全。ファイルコンパイル vs eval文脈の区別なし。`:compile-toplevel`ブランチが`(handler-case (error () nil))`でエラーを黙って飲み込む（`expander.lisp:200,208`）
- **内容**: ANSI CL 3.2.3.1の8シチュエーション組み合わせを正確に処理。ファイルコンパイラ/evalモード区別。コンパイル時エラー伝播（黙殺停止）
- **根拠**: ANSI CL 3.2.3.1 — Processing of Top Level Forms
- **難易度**: Medium

#### FR-400: LOAD-TIME-VALUE Self-Hosted Eval (load-time-value自己ホスト化)

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `macros-sequence.lisp:412` — `load-time-value`マクロが`(eval form)`呼び出し（ホストSBCLに委譲）。自己ホスト文脈でcl-cc定義関数がホストevalから見えず失敗
- **内容**: `(eval form)`を`(our-eval form)`に置換。自己ホスト環境でのload-time-value正常動作
- **根拠**: 自己ホスティング完全化
- **難易度**: Low

#### FR-401: Tail Position Detection Completeness (末尾位置検出完全化)

- **対象**: `src/compile/codegen-core.lisp`
- **現状**: `codegen-core.lisp:177`で`block`ボディが無条件に`(setf (ctx-tail-position ctx) nil)`。`codegen-core.lisp:281-283`で`ast-the`が末尾位置を保持/復元しない。`block`と`the`はANSI CL上末尾位置であるべき
- **内容**: `block`ボディの最終式で末尾位置を維持（return-from対象が存在する場合のみ非末尾化）。`the`フォームで末尾位置を保持・伝播
- **根拠**: ANSI CL 3.2.2.1 — Tail Position (TCO前提の末尾位置)
- **難易度**: Low

#### FR-402: NTH-VALUE Optimization (nth-value最適化)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `macros-stdlib.lisp:104-108` — `(nth-value n form)`が`(nth n (multiple-value-list form))`に展開。全値のリスト割り当て後にn番目取得
- **内容**: 定数`n`の場合、`(multiple-value-bind (v0 ... vn) form vn)`に展開。ヒープ割り当て回避
- **根拠**: SBCL/CCLの標準最適化手法
- **難易度**: Low

---

### Phase 78 — コード生成・レジスタ割り当て改善

#### FR-403: Branch Displacement Optimization (分岐変位最適化)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **現状**: 全ジャンプが固定`rel32`エンコーディング（JMP=5バイト, Jcc=6バイト）。`emit-jmp-rel32`(`x86-64-codegen.lisp:177`)、`emit-je-rel32`(`x86-64-codegen.lisp:181`)。短条件ジャンプ(`JE rel8`=2バイト)はハンドクラフトシーケンスのみ
- **内容**: 2パスまたは反復緩和アルゴリズム。初回パスで全分岐を`rel8`(2バイト)。オーバーフロー時`rel32`(5-6バイト)に拡大・オフセット再解決。`*x86-64-instruction-sizes*`を動的に
- **根拠**: LLVM/GCC標準 — branch relaxation
- **難易度**: Medium

#### FR-404: Lazy Compilation / Compile-on-First-Call (遅延コンパイル)

- **対象**: `src/compile/pipeline.lisp`, `src/vm/vm.lisp`
- **現状**: `compile-toplevel-forms`が全フォームを一括コンパイル。`our-load`(`pipeline.lisp:373`)がロード時に全フォームを順次コンパイル。スタブ/トランポリン機構なし
- **内容**: `defun`コンパイルをソース記録の軽量スタブに置換。初回呼び出し時にフルコンパイルしスタブをパッチ。VM用スタブ命令型、自己書き換え呼び出しサイトパッチ
- **根拠**: HotSpot/V8の標準手法 — lazy compilation
- **難易度**: Hard

#### FR-405: Startup Time Optimization / Image Dump (起動時間最適化)

- **対象**: `src/cli/main.lisp`, `src/compile/pipeline.lisp`
- **現状**: `*stdlib-compiled*`(`pipeline.lisp:191`)がnil（キャッシュ未使用）。`get-stdlib-forms`(`pipeline.lisp:193`)が毎回再パース。REPL stdlib読み込み(`main.lisp:263`)が最初のプロンプト前に全stdlibを同期コンパイル
- **内容**: (a) プリコンパイルstdlibキャッシュ（FASL様シリアライズ）。(b) 遅延stdlibロード（初回参照時コンパイル）。(c) `--no-stdlib`デフォルト+未定義関数auto-require。(d) イメージダンプ(`save-lisp-and-die`相当)で全初期化スキップ
- **根拠**: SBCL save-lisp-and-die, ECL image dump
- **難易度**: Medium

#### FR-406: Code Compression for Distribution (配布用コード圧縮)

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`
- **現状**: `compile-to-native`(`pipeline.lisp:426`)が生のバイト列を直接`__TEXT`セグメントに書き込み。セグメント/セクションの圧縮なし
- **内容**: (a) zstd/lz4で`__DATA`セグメント圧縮（ロード時解凍）。(b) バイトコード形式にvarint+辞書圧縮。(c) デバッグ情報セクション圧縮
- **根拠**: LLVM LLD, GNU gold — compressed debug sections
- **難易度**: Medium

#### FR-407: Spill Register Clobber Fix (スピルレジスタ上書き修正)

- **対象**: `src/emit/regalloc.lisp`
- **現状**: `regalloc.lisp:358-364` — 全スピル済みvregが単一scratchレジスタを共有。同一命令内の2スピルオペランドが黙ってclobber
- **内容**: 複数scratch割り当てまたはスピル競合検出。同一命令内でsrc/dstの両方がスピルの場合、2つの異なるscratchレジスタを使用
- **根拠**: 正当性バグ — サイレントデータ破壊
- **難易度**: Medium

#### FR-408: Copy Propagation Performance (コピー伝播性能改善)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `optimizer.lisp:197-199` — `kill`が毎回`maphash`でコピーテーブル全走査。O(n×m) (n命令, mコピー数)
- **内容**: コピーテーブルを逆引きマップ付きに再構築。レジスタ書き込み時にそのレジスタを含むコピーのみ無効化。O(n)に改善
- **根拠**: 標準コンパイラ実装 — efficient copy propagation kill
- **難易度**: Low-Medium

#### FR-409: Label-Aware Optimization State (ラベル考慮最適化状態)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `optimizer.lisp:86,203,601,677-678` — 全`vm-label`で`clrhash`（env/copies/gen/val-env/memo全消去）。フォールスルーラベルでも最適化コンテキスト破棄
- **内容**: フォールスルーラベル（分岐ターゲットでない）ではclrhash省略。分岐合流点のみ保守的消去。CFGのpredecessor情報を利用した精密な状態合流
- **根拠**: GCC/LLVM — label-aware dataflow analysis
- **難易度**: Medium

#### FR-410: Instruction Representation Efficiency (命令表現効率化)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `opt-rewrite-inst-regs`(`optimizer.lisp:229-246`)が非自明命令を毎回sexpシリアライズ→ツリー走査→再構築。命令数×パス反復回数分のアロケーション
- **内容**: 命令オブジェクトのスロット直接書き換え。sexp往復を除去し、`sexp-slots`メタデータで直接レジスタフィールド操作
- **根拠**: GC圧力削減、最適化パス高速化
- **難易度**: Medium

#### FR-411: Recursive Inlining Guard (再帰インライニングガード)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `opt-pass-inline`(`optimizer.lisp:504-576`)が再帰・相互再帰呼び出しを検出せず。ボディ閾値を満たせば自分自身にインライン可能
- **内容**: インライン候補判定時にコールグラフを参照。再帰・相互再帰の場合インライン抑制（または深度制限）
- **根拠**: 無限展開防止 — 正当性バグ
- **難易度**: Low-Medium

#### FR-412: Liveness Analysis Completeness (活性解析完全化)

- **対象**: `src/emit/regalloc.lisp`
- **現状**: `compute-live-intervals`(`regalloc.lisp:244-273`)が後方ジャンプのみ区間延長。前方`vm-jump-zero`のラベルへのジャンプでギャップ越しのレジスタ活性が未延長
- **内容**: 前方分岐もlive interval延長対象に含める。反復データフロー解析またはSSA情報利用
- **根拠**: 正当性バグ — 前方分岐でレジスタ上書きの可能性
- **難易度**: Medium

#### FR-413: Live Range Splitting (ライブレンジ分割)

- **対象**: `src/emit/regalloc.lisp`
- **現状**: `regalloc.lisp:219-283` — 各vregが単一連続区間。中間で使用されない期間も物理レジスタを占有
- **内容**: ライブレンジを使用点間のホール（穴）で分割。ホール期間は物理レジスタを解放。スピル圧力大幅削減
- **根拠**: Poletto & Sarkar (1999) Linear Scan Register Allocation
- **難易度**: Medium

#### FR-414: Iterative Inlining (反復インライニング)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `opt-pass-inline`(`optimizer.lisp:765`)が収束ループ外で1回のみ実行。インライン後のfold/DCEが新たなインライン機会を露出しても再発見不可
- **内容**: `opt-pass-inline`を収束パス（`*opt-convergence-passes*`）に統合。インライン→fold→DCE→再インラインの反復サイクル。深度制限付き
- **根拠**: LLVM CGSCC inline — iterative inlining
- **難易度**: Low-Medium

#### FR-415: CSE Key Efficiency (CSEキー効率化)

- **対象**: `src/optimize/optimizer.lisp`
- **現状**: `val<`比較(`optimizer.lisp:658`)が`(format nil "~S" ...)`で毎CSEキー比較にフレッシュ文字列生成。高GC圧力
- **内容**: CSEキーを構造的比較（opcode + operands tuple）またはハッシュコンスに変更。文字列アロケーション除去
- **根拠**: GC圧力削減
- **難易度**: Low

#### FR-416: Tail-Call Register Allocation (末尾呼び出しレジスタ割り当て)

- **対象**: `src/emit/regalloc.lisp`
- **現状**: `vm-tail-call`が`regalloc.lisp:85`でdstレジスタ定義。末尾呼び出しは復帰しないためdstは使用されず、物理レジスタを無駄に割り当て
- **内容**: `vm-tail-call`のdstをregalloc対象から除外。または`vm-tail-call`のdst定義自体を削除
- **根拠**: レジスタ圧力削減
- **難易度**: Easy

### Phase 79 — コンディション・リスタートシステム完全化

#### FR-417: define-condition :report Option (define-condition :reportオプション)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `macros-stdlib.lisp:204-209` — `define-condition`マクロが`(declare (ignore options))`で`:report`/`:documentation`オプションを完全無視。ユーザ定義コンディションにカスタムレポートメソッドなし
- **内容**: `:report`オプションからreportメソッド生成。文字列の場合は直接出力、関数の場合はGFディスパッチ
- **根拠**: ANSI CL 9.1 — Condition Type Definition
- **難易度**: Medium

#### FR-418: signal Function Builtin (signal関数ビルトイン)

- **対象**: `src/compile/builtin-registry.lisp`, `src/vm/conditions.lisp`
- **現状**: `builtin-registry.lisp:362-365`に`error`/`warn`のみ登録。CL標準`signal`関数のコンパイルパスなし
- **内容**: `signal`をビルトインに登録。ハンドラスタック走査によるコンディション通知（巻き戻しなし）
- **根拠**: ANSI CL 9.1.4 — signal
- **難易度**: Medium

#### FR-419: cerror Function Builtin (cerror関数ビルトイン)

- **対象**: `src/compile/builtin-registry.lisp`
- **現状**: `vm-cerror`命令(`conditions.lisp:159`)は存在するがbuiltin-registry未登録。ユーザコードから`(cerror ...)`呼び出し不可
- **内容**: `cerror`をビルトインに登録。`continue`リスタートの自動設置
- **根拠**: ANSI CL 9.1.4 — cerror
- **難易度**: Low

#### FR-420: restart-case Functional Implementation (restart-case機能実装)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `macros-stdlib.lisp:224-234` — 全リスタート名/ラムダリスト無視。`error`のみcatch。最初の節ボディのみ実行
- **内容**: 名前付きリスタートの実際のスタック登録。各節のリスタート関数を設置。`invoke-restart`から呼び出し可能にする
- **根拠**: ANSI CL 9.1.4 — restart-case
- **依存**: FR-301 (ランタイムハンドラスタック)
- **難易度**: Hard

#### FR-421: invoke-restart / find-restart / compute-restarts (リスタート呼び出し・検索)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `invoke-restart`(`macros-stdlib.lisp:246-247`)が無条件`error`。`find-restart`/`compute-restarts`(`macros-stdlib.lisp:249-255`)が常に`nil`返却
- **内容**: VMリスタートスタックを走査して名前付きリスタートを検索・呼び出し
- **根拠**: ANSI CL 9.1.4.2 — Restarts
- **依存**: FR-420
- **難易度**: Medium

#### FR-422: with-simple-restart (with-simple-restart実装)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `with-simple-restart`がsrc/全体に不在
- **内容**: `(with-simple-restart (name format-string) &body body)` — 名前付きリスタート設置マクロ。invoke時にbodyから脱出しnil返却
- **根拠**: ANSI CL 9.1.4 — with-simple-restart
- **依存**: FR-420
- **難易度**: Low

#### FR-423: with-condition-restarts (with-condition-restarts実装)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `with-condition-restarts`がsrc/全体に不在
- **内容**: コンディションとリスタートの関連付け。`find-restart`がcondition引数で絞り込み可能
- **根拠**: ANSI CL 9.1.4 — with-condition-restarts
- **依存**: FR-421
- **難易度**: Medium

#### FR-424: ANSI Condition Type Hierarchy (ANSI標準コンディション型階層)

- **対象**: `src/vm/conditions.lisp`
- **現状**: VM固有5型のみ定義(`conditions.lisp:17-67`)。`simple-condition`, `simple-warning`, `simple-error`, `simple-type-error`, `arithmetic-error`, `cell-error`, `unbound-slot`, `package-error`, `stream-error`, `file-error`, `print-not-readable`, `storage-condition`, `serious-condition`等が欠落
- **内容**: ANSI CL 9.1標準コンディション型階層の完全実装
- **根拠**: ANSI CL 9.1 — Condition Types
- **難易度**: Medium

#### FR-425: muffle-warning Functional (muffle-warning機能化)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/conditions.lisp`
- **現状**: `muffle-warning`(`macros-stdlib.lisp:272-274`)が`nil`返却のみ。`vm-warn`(`conditions.lisp:277-285`)が常に出力。警告抑制不可
- **内容**: `vm-warn`がmuffle-warningリスタートを設置。`muffle-warning`呼び出し時にwarn本体から脱出
- **根拠**: ANSI CL 9.1.4 — muffle-warning
- **依存**: FR-420
- **難易度**: Medium

#### FR-426: check-type Correctable Error (check-type修正可能エラー)

- **対象**: `src/expand/macros-basic.lisp`
- **現状**: `macros-basic.lisp:68-72` — 生`error`シグナル。`store-value`リスタート未設置。インタラクティブ修正不可
- **内容**: `store-value`リスタートを設置。デバッガから新しい値を入力して変数を更新可能
- **根拠**: ANSI CL 9.2 — check-type
- **依存**: FR-420
- **難易度**: Medium

#### FR-427: make-condition User-Callable (make-condition公開)

- **対象**: `src/vm/conditions.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `make-condition`(`conditions.lisp:334-355`)がホストCL内部使用のみ。ビルトイン登録なし
- **内容**: `make-condition`をビルトイン登録。ユーザコードから`(make-condition 'my-error :slot val)`呼び出し可能
- **根拠**: ANSI CL 9.1.1 — make-condition
- **難易度**: Low

---

### Phase 80 — マクロ・リーダー・パッケージシステム

#### FR-428: macro-function / (setf macro-function) (マクロ関数API)

- **対象**: `src/expand/macro.lisp`
- **現状**: 内部`register-macro`/`macro-env-table`のみ。CL標準`macro-function`/`(setf macro-function)`なし
- **内容**: `(macro-function name &optional env)` — マクロ展開関数を返す。`(setf (macro-function name) fn)` — マクロ展開関数を設定
- **根拠**: ANSI CL 3.8.1 — macro-function
- **難易度**: Low

#### FR-429: *macroexpand-hook* (マクロ展開フック)

- **対象**: `src/expand/expander.lisp`
- **現状**: FR-241がマクロ展開トレースとして言及するが`*macroexpand-hook*`変数自体は未実装
- **内容**: `*macroexpand-hook*`変数（デフォルト`#'funcall`）。マクロ展開時にこのフックを介して展開関数を呼び出し。デバッグ・計測に利用
- **根拠**: ANSI CL 3.8 — *macroexpand-hook*
- **難易度**: Low

#### FR-430: deftype with Lambda-List (パラメトリックdeftype)

- **対象**: `src/expand/expander.lisp`
- **現状**: `expander.lisp:400-403` — `(deftype name type-specifier)`の2引数形式のみ。ANSI CLの`(deftype name lambda-list &body body)`未対応
- **内容**: lambda-list付きdeftype。型展開器がlambda-list引数を受け取りbody評価で型指定子を返す。`(deftype list-of (element-type) \`(cons ,element-type t))`等
- **根拠**: ANSI CL 4.4.2 — deftype
- **難易度**: Medium

#### FR-431: time Macro (timeマクロ)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `get-internal-real-time` VM命令存在(`vm-numeric.lisp:452`)。CL標準`(time form)`マクロなし
- **内容**: `(time form)` — 実行時間・GC回数・コンシング量を出力するラッパーマクロ
- **根拠**: ANSI CL 25.1 — time
- **難易度**: Low

#### FR-432: trace / untrace Macros (trace/untraceマクロ)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/vm.lisp`
- **現状**: FR-314がVMレベルトレースポイントをカバーするが、CL標準`(trace fn)`/`(untrace fn)` APIなし
- **内容**: `(trace fn-name)` — 関数呼び出し/返却のトレース出力。`(untrace fn-name)` — トレース解除。関数ラッパー方式またはVMディスパッチフック方式
- **根拠**: ANSI CL 25.1 — trace, untrace
- **難易度**: Medium

#### FR-433: step Macro (stepマクロ)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/vm-run.lisp`
- **現状**: FR-312がVMレベルシングルステップをカバーするが、CL標準`(step form)` APIなし
- **内容**: `(step form)` — フォーム評価をインタラクティブにステップ実行するラッパーマクロ
- **根拠**: ANSI CL 25.1.2 — step
- **依存**: FR-312
- **難易度**: Medium

#### FR-434: room Function (room関数)

- **対象**: `src/vm/vm.lisp`, `src/runtime/heap.lisp`
- **現状**: `room`がsrc/全体に不在。ヒープ/GC統計情報の報告手段なし
- **内容**: `(room &optional verbosity)` — ヒープ使用量、GC統計、レジストリサイズ等をレポート
- **根拠**: ANSI CL 25.1 — room
- **難易度**: Low

#### FR-435: apropos / apropos-list (シンボル検索)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/vm.lisp`
- **現状**: `apropos`/`apropos-list`がsrc/全体に不在
- **内容**: `(apropos string &optional package)` — 名前に文字列を含むシンボルを検索・表示。`(apropos-list string &optional package)` — リスト返却版
- **根拠**: ANSI CL 25.1 — apropos
- **難易度**: Low

#### FR-436: documentation / (setf documentation) (ドキュメンテーション関数)

- **対象**: `src/vm/vm.lisp`
- **現状**: FR-321がAPIドキュメント生成ツールをカバーするが、CL標準`documentation` GFなし。ランタイムドキュメンテーション格納・取得手段なし
- **内容**: `(documentation x doc-type)` — ドキュメンテーション文字列取得。`(setf (documentation x doc-type) string)` — 設定。関数/変数/型のdocstring格納テーブル
- **根拠**: ANSI CL 25.1 — documentation
- **難易度**: Medium

#### FR-437: Package Mutation Operations (パッケージ変更操作)

- **対象**: `src/expand/macros-sequence.lisp`, `src/vm/vm.lisp`
- **現状**: `defpackage`(`macros-sequence.lisp:288-290`)が`(quote name)`返却のスタブ。`export`(`macros-sequence.lisp:292-294`)がno-opスタブ。`make-package`, `rename-package`, `delete-package`, `unexport`, `unuse-package`, `shadowing-import`, `shadow`のVM命令・マクロなし
- **内容**: パッケージ操作関数群のVM命令実装。`defpackage`マクロの完全展開（`:use`, `:export`, `:import-from`, `:shadow`等）
- **根拠**: ANSI CL 11.2 — Package System Operations
- **難易度**: Hard

#### FR-438: Package Introspection Accessors (パッケージ内省アクセサ)

- **対象**: `src/vm/vm.lisp`
- **現状**: `package-use-list`, `package-used-by-list`, `package-shadowing-symbols`がsrc/全体に不在
- **内容**: パッケージ内省アクセサ群: `package-name`, `package-nicknames`, `package-use-list`, `package-used-by-list`, `package-shadowing-symbols`
- **根拠**: ANSI CL 11.2 — Package Accessors
- **依存**: FR-437
- **難易度**: Medium

#### FR-439: compiler-let (compiler-let特殊形式)

- **対象**: `src/expand/expander.lisp`
- **現状**: `compiler-let`がsrc/全体に不在。コンパイル時束縛なし
- **内容**: `(compiler-let ((var val) ...) &body body)` — コンパイル時に変数を束縛し、マクロ展開中に参照可能にする
- **根拠**: CLtL2 8.1 — compiler-let
- **難易度**: Medium

---

### Phase 81 — 配列・シーケンス・構造体完全化

#### FR-440: sort/stable-sort :key Support (sort :keyサポート)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `sort`が`(list predicate)`のみ受け付け。`:key`引数なし(`macros-stdlib.lisp:597`)
- **内容**: `:key`引数を受け付け、比較時に各要素にkey関数を適用
- **根拠**: ANSI CL 17.3 — sort, stable-sort
- **難易度**: Low

#### FR-441: sort/stable-sort Vector Support (sortベクタ対応)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `sort`/`stable-sort`(`macros-stdlib.lisp:597-627`)が`car`/`cdr`/`cons`専用。ベクタソート不可
- **内容**: ベクタ入力時にインプレースソート（quicksort/mergesort on vector）
- **根拠**: ANSI CL 17.3 — sort accepts sequence, not just list
- **難易度**: Medium

#### FR-442: make-array Keyword Args Compilation (make-arrayキーワード引数コンパイル)

- **対象**: `src/compile/codegen-phase2.lisp`
- **現状**: `codegen-phase2.lisp:86-90` — `(make-array size)`のみコンパイル。`:initial-element`, `:element-type`, `:displaced-to`, `:initial-contents`がコンパイル時に破棄
- **内容**: make-arrayキーワード引数をコンパイルしてVM命令に渡す。VM側`vm-make-array`は`:initial-element`対応済み
- **根拠**: ANSI CL 15.2 — make-array
- **難易度**: Medium

#### FR-443: make-array Multi-Dimensional (多次元make-array)

- **対象**: `src/compile/codegen-phase2.lisp`, `src/vm/list.lisp`
- **現状**: `codegen-phase2.lisp:87`/`vm-make-array`(`list.lisp:510`)が単一整数サイズのみ受け付け。次元リスト未対応
- **内容**: `(make-array '(3 4))` — 多次元配列生成。`aref`の多次元インデックス計算（row-major-aref変換）
- **根拠**: ANSI CL 15.2 — Multi-dimensional arrays
- **難易度**: Hard

#### FR-444: copy-seq Vector Support (copy-seqベクタ対応)

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `macros-sequence.lisp:7-8` — `copy-seq`が`copy-list`に展開。ベクタ非対応
- **内容**: 入力型に応じてリストはcopy-list、ベクタはベクタコピー
- **根拠**: ANSI CL 17.3 — copy-seq
- **難易度**: Low

#### FR-445: fill/replace :start/:end Support (fill/replace境界引数)

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `macros-sequence.lisp:11-47` — `fill`/`replace`が`:start`/`:end`キーワードを黙って無視
- **内容**: `:start`/`:end`キーワードの処理。部分範囲への操作を可能にする
- **根拠**: ANSI CL 17.3 — fill, replace
- **難易度**: Low

#### FR-446: defstruct :copier Option (defstruct :copierオプション)

- **対象**: `src/expand/expander-defstruct.lisp`
- **現状**: `expander-defstruct.lisp:66-123` — `copy-<name>`関数が未生成。`:copier nil`も未処理
- **内容**: デフォルトで`copy-<name>`関数を生成（構造体の浅いコピー）。`:copier nil`で抑制
- **根拠**: ANSI CL 8.1 — defstruct :copier
- **難易度**: Low

#### FR-447: defstruct :print-function / :print-object (defstruct印刷オプション)

- **対象**: `src/expand/expander-defstruct.lisp`
- **現状**: `expander-defstruct.lisp:66-123` — `:print-function`/`:print-object`オプションを黙って無視
- **内容**: `:print-object`指定時にprint-objectメソッド生成。`:print-function`は旧互換
- **根拠**: ANSI CL 8.1 — defstruct print options
- **依存**: FR-390
- **難易度**: Medium

#### FR-448: defstruct :type list/vector (defstruct型指定)

- **対象**: `src/expand/expander-defstruct.lisp`
- **現状**: `expander-defstruct.lisp:66-123` — `:type`オプション未実装
- **内容**: `(:type list)` — 構造体をリスト表現。`(:type vector)` — ベクタ表現。`:named`オプションと組み合わせ
- **根拠**: ANSI CL 8.1 — defstruct :type
- **難易度**: Medium

#### FR-449: defstruct :read-only Slot Option (defstruct :read-onlyスロット)

- **対象**: `src/expand/expander-defstruct.lisp`
- **現状**: `expander-defstruct.lisp:94-96` — `:read-only`スロットオプション未パース・未強制
- **内容**: `:read-only t`指定時にsetfアクセサを生成しない。コンパイル時にsetf使用を検出してエラー
- **根拠**: ANSI CL 8.1 — defstruct slot options
- **難易度**: Low

#### FR-450: Sequence Function :key/:test/:test-not (シーケンス関数キーワード引数)

- **対象**: `src/expand/macros-sequence.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `find`, `count`, `position`, `remove`, `delete`, `substitute`, `mismatch`等が`&rest keys`を受け取るが全て黙って無視
- **内容**: `:key`, `:test`, `:test-not`, `:start`, `:end`, `:from-end`キーワードの実処理
- **根拠**: ANSI CL 17.3 — Sequence Functions
- **難易度**: Hard

#### FR-451: search General Sequence (汎用searchシーケンス検索)

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `rt-search-string`(`runtime.lisp:352`)のみ。汎用シーケンスのsearch未実装
- **内容**: `(search sequence1 sequence2 &key test key start1 end1 start2 end2)` — 部分シーケンス検索
- **根拠**: ANSI CL 17.3 — search
- **難易度**: Medium

#### FR-452: merge :key Support (merge :keyサポート)

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `merge`マクロ(`macros-sequence.lisp:245-281`)が`&rest keys`を黙って無視
- **内容**: `:key`引数の処理。マージ比較時にkey関数適用
- **根拠**: ANSI CL 17.3 — merge
- **難易度**: Low

#### FR-453: map-into Multi-Source (map-into複数ソース)

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `map-into`(`macros-sequence.lisp:222-242`)が複数ソースの場合`(progn dest)`(no-op)にフォールスルー
- **内容**: 複数ソースシーケンスから並列にマッピング。最短ソースで停止
- **根拠**: ANSI CL 17.3 — map-into
- **難易度**: Medium

---

### Phase 82 — VMアーキテクチャ効率化

#### FR-454: VM Dispatch Optimization (VMディスパッチ最適化)

- **対象**: `src/vm/vm-run.lisp`
- **現状**: `run-compiled`(`vm-run.lisp:131-138`)が毎命令`execute-instruction` CLOSジェネリック関数ディスパッチ。GFメソッドルックアップのオーバーヘッド
- **内容**: typecase/defopcode方式に変換。または命令タイプごとのjump-tableディスパッチ
- **根拠**: V8/LuaJIT — fast interpreter dispatch
- **難易度**: Medium

#### FR-455: O(1) Instruction Fetch (定数時間命令フェッチ)

- **対象**: `src/vm/vm-run.lisp`
- **現状**: `run-compiled`(`vm-run.lisp:133`)が`(nth pc instructions)`でリスト上のO(n)フェッチ。`run-program-slice`は`aref`でベクタO(1)
- **内容**: 命令リストをベクタに変換し`svref`/`aref`でO(1)アクセス
- **根拠**: 基本的なインタプリタ性能要件
- **難易度**: Easy

#### FR-456: Bytecode ISA v2 Completion (バイトコードISA v2完成)

- **対象**: `src/bytecode/encode.lisp`, `src/vm/vm-run.lisp`
- **現状**: `encode.lisp:26-108`に50オペコードISA定義・エンコーダ・デコーダ・逆アセンブラ。`run-vm`(`vm-run.lisp:214-261`)が6オペコードのみ実装（const, move, add2, sub2, mul2, halt2）。コンパイラからバイトコードへの出力パスなし
- **内容**: 残り44オペコードのdefopcode実装。コンパイラからバイトコード出力パス構築。vm-state/vm2-state統合
- **根拠**: バイトコード実行エンジン完成
- **難易度**: Hard

#### FR-457: vm-falsep Correctness Fix (vm-falsep正当性修正)

- **対象**: `src/vm/vm.lisp`
- **現状**: `vm-falsep`(`vm.lisp:533-535`)が`nil`と整数`0`の両方をfalse扱い。CL仕様では`nil`のみがfalse
- **内容**: `(defun vm-falsep (v) (null v))` — `nil`のみfalse。全`vm-jump-zero`使用箇所を確認
- **根拠**: ANSI CL仕様 — 正当性バグ（FR-373のCPS版と同根）
- **難易度**: Low

#### FR-458: handler-case Stack Copy Optimization (handler-caseスタックコピー最適化)

- **対象**: `src/vm/vm-run.lisp`
- **現状**: `vm-establish-handler`(`vm-run.lisp:36-48`)が`(copy-list (vm-call-stack state))`と全レジスタHTスナップショットをハンドラ設置毎に実行。O(stack-depth + register-count)
- **内容**: コールスタックの遅延コピー（COWまたはスナップショット不要化）。レジスタはフレーム境界でのみ保存
- **根拠**: handler-case性能問題 — ホットパスでの不要コピー
- **難易度**: Medium

#### FR-459: Superinstruction / Opcode Fusion (スーパー命令/オペコード融合)

- **対象**: `src/vm/vm-run.lisp`, `src/optimize/optimizer.lisp`
- **現状**: CLOSインタプリタもdefopcodeエンジンも命令融合なし。頻出シーケンス（const+jump-zero, move+ret, const+add等）を個別ディスパッチ
- **内容**: 頻出命令ペア/トリプルのプロファイリング。融合スーパー命令の定義と最適化パスでの置換
- **根拠**: CPython 3.12 superinstructions, LuaJIT
- **難易度**: Hard

#### FR-460: Label Table Integer Keys (ラベルテーブル整数キー)

- **対象**: `src/vm/vm-run.lisp`
- **現状**: `build-label-table`(`vm-run.lisp:102-109`)が`#'equal`テスト（文字列比較）のHTを生成。毎ジャンプで文字列ハッシュ計算
- **内容**: ラベルを整数ID化し`#'eql`テストHTまたは直接配列インデックスに変更
- **根拠**: ジャンプ性能改善
- **難易度**: Low

#### FR-461: Closure Captured-Values Vector (クロージャキャプチャ値ベクタ化)

- **対象**: `src/vm/vm.lisp`
- **現状**: `vm-closure-ref-idx`(`vm.lisp:688-693`)が`(nth idx values-list)`でO(n)アクセス
- **内容**: captured-valuesをリストからベクタに変更。`svref`でO(1)アクセス
- **根拠**: クロージャ変数アクセス性能
- **難易度**: Low

#### FR-462: vm-apply Efficient Argument Spreading (vm-apply効率的引数展開)

- **対象**: `src/vm/vm.lisp`
- **現状**: `vm-apply`(`vm.lisp:965-968`)が`(append (butlast arg-values) ...)`で2回走査+新リスト割り当て
- **内容**: 最終引数のspliceを1パスで実行。または`nconc`＋最終要素直接リンク
- **根拠**: apply性能改善
- **難易度**: Low

#### FR-463: vm-sync-handler-regs Batching (ハンドラレジスタ同期バッチ化)

- **対象**: `src/vm/vm-run.lisp`
- **現状**: `vm-sync-handler-regs`(`vm-run.lisp:56-62`)が全ハンドラ×全レジスタのネストmaphash。O(handlers × registers)
- **内容**: ハンドラ設置時のレジスタスナップショットを遅延化。例外発生時のみ復元計算
- **根拠**: handler-case非例外パス性能
- **難易度**: Medium

---

### Phase 83 — バイナリ・リンカー完全化

#### FR-464: Mach-O __PAGEZERO Segment (Mach-O __PAGEZEROセグメント)

- **対象**: `src/emit/binary/macho.lisp`
- **現状**: `build-mach-o`(`macho.lisp:406-464`)が`__PAGEZERO`ロードコマンド未出力。macOS実行可能形式の必須要件
- **内容**: `__PAGEZERO`セグメント（vmaddr=0, vmsize=4GB nullガード）のLC_SEGMENT_64追加
- **根拠**: macOS ABI — 全実行可能形式に必須
- **難易度**: Low

#### FR-465: Mach-O Symbol Table Serialization (Mach-Oシンボルテーブルシリアライズ)

- **対象**: `src/emit/binary/macho.lisp`
- **現状**: `add-symbol`/`serialize-nlist`(`macho.lisp:376-396,280-288`)が定義済みだが`build-mach-o`から未呼び出し。LC_SYMTABロードコマンドもシリアライズされない(`macho.lisp:419-422`)
- **内容**: `build-mach-o`でnlistエントリをシリアライズ。LC_SYMTAB/LC_DYSYMTABロードコマンド出力。デバッガ/dlsym互換
- **根拠**: macOS実行可能形式 — シンボルテーブルはデバッグ・リンクに必須
- **難易度**: Medium

#### FR-466: Mach-O Data Segment Serialization (Mach-Oデータセグメントシリアライズ)

- **対象**: `src/emit/binary/macho.lisp`
- **現状**: `add-data-segment`(`macho.lisp:351-374`)が定義済みだが`build-mach-o`がcode-bytesのみ書き込み。データセグメント内容が出力バッファに反映されない
- **内容**: `__DATA`セグメント内容をバイナリ出力に含める。グローバル変数・定数テーブルの配置
- **根拠**: グローバルデータの実行可能形式への配置
- **難易度**: Medium

#### FR-467: Mach-O W^X Security (Mach-O W^X セキュリティ)

- **対象**: `src/emit/binary/macho.lisp`
- **現状**: `add-data-segment`(`macho.lisp:369-370`)が`maxprot=7`(rwx)設定。W^Xセキュリティポリシー違反
- **内容**: `__DATA`セグメントの`maxprot`を`6`(rw-)に変更。実行可能ページと書き込み可能ページを分離
- **根拠**: macOSセキュリティ — W^X (Write XOR Execute)
- **難易度**: Easy

#### FR-468: ELF AArch64 Support (ELF AArch64対応)

- **対象**: `src/emit/binary/elf.lisp`
- **現状**: `elf.lisp:24,313`がx86-64(`+elf-machine-x86-64+` #x3e)ハードコード。AArch64 codegen存在にもかかわらず`EM_AARCH64`(#xB7)未対応
- **内容**: ELFヘッダのe_machineをターゲットに応じて設定。AArch64再配置型の追加
- **根拠**: AArch64ネイティブコンパイル完全化
- **難易度**: Medium

#### FR-469: ELF Section Alignment (ELFセクションアライメント)

- **対象**: `src/emit/binary/elf.lisp`
- **現状**: `.text`オフセットがELFヘッダ直後にパディングなしで開始(`elf.lisp:284`)。リンカが期待するページ/16バイトアライメント不足
- **内容**: `.text`セクションを16バイトまたはページ境界にアライン。`.data`/`.bss`も同様
- **根拠**: ELF仕様 — セクションアライメント要件
- **難易度**: Low

#### FR-470: ELF .bss Section (ELF .bssセクション)

- **対象**: `src/emit/binary/elf.lisp`
- **現状**: `.text`のみ。未初期化データ用`.bss`セクションAPIなし(`elf.lisp:150-161`)
- **内容**: `.bss`セクション追加。SHT_NOBITS型、ファイルサイズ0でメモリサイズ指定
- **根拠**: グローバル変数の効率的配置
- **難易度**: Low

#### FR-471: Byte Buffer Unification (バイトバッファ統一)

- **対象**: `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`, `src/emit/wasm.lisp`
- **現状**: `byte-buffer`クラス(`macho.lisp:126-156`)、`elf-make-buffer`/`elf-buf-*`(`elf.lisp:68-111`)、`make-wasm-buffer`/`wasm-buf-*`(`wasm.lisp:44-86`)が3つの独立したほぼ同一のバイトバッファAPI
- **内容**: 共通`binary-buffer`クラスに統合。write-u8/u16/u32/u64/bytes/string/leb128メソッドを共通化
- **根拠**: コード重複削減
- **難易度**: Medium

#### FR-472: Calling Convention FP Registers (呼び出し規約浮動小数点レジスタ)

- **対象**: `src/emit/calling-convention.lisp`
- **現状**: `calling-convention`構造体(`calling-convention.lisp:8-14`)がGPRプールのみ。XMM/NEONフロートレジスタフィールドなし
- **内容**: System V ABI準拠のfloat引数渡し。XMM0-7(x86-64)/V0-7(AArch64)レジスタプール追加
- **根拠**: System V ABI — 浮動小数点引数はFPレジスタで渡す
- **依存**: FR-008 (float unboxing)
- **難易度**: Medium

#### FR-473: WASM Portability (WASMポータビリティ)

- **対象**: `src/emit/wasm.lisp`
- **現状**: `wasm-buf-write-f64`(`wasm.lisp:74-75`)がSBCL専用コード（`sb-kernel:double-float-bits`）。他CL実装でエラー
- **内容**: ポータブルなIEEE754 double→bytesシリアライズ。`#+(or sbcl ccl ecl)`分岐または純CLビット操作
- **根拠**: ポータビリティ — 自己ホスティングで必要
- **難易度**: Low

---

### Phase 84 — 数値・ファイルシステム追加

#### FR-474: signum / isqrt VM Instructions (signum/isqrt VM命令)

- **対象**: `src/vm/vm-numeric.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `signum`/`isqrt`のVM命令・ビルトイン登録なし
- **内容**: `vm-signum`（符号関数）、`vm-isqrt`（整数平方根）VM命令追加・ビルトイン登録
- **根拠**: ANSI CL 12.2 — signum, isqrt
- **難易度**: Easy

#### FR-475: Destructive String Operations (破壊的文字列操作)

- **対象**: `src/vm/strings.lisp`
- **現状**: `nstring-upcase`/`nstring-downcase`/`nstring-capitalize`が不在。非破壊版のみ
- **内容**: 破壊的バージョン追加（元の文字列をインプレース変更）
- **根拠**: ANSI CL 16.2 — nstring-upcase等
- **難易度**: Easy

#### FR-476: log Two-Argument Form (log 2引数形式)

- **対象**: `src/vm/vm-numeric.lisp`
- **現状**: `vm-log-inst`(`vm-numeric.lisp:166-173`)が単項のみ。`(log x base)`未対応
- **内容**: 2引数形式`(log x base)` = `(/ (log x) (log base))`。ビルトイン登録更新
- **根拠**: ANSI CL 12.2 — log
- **難易度**: Easy

#### FR-477: digit-char Radix Support (digit-char基数サポート)

- **対象**: `src/vm/strings.lisp`
- **現状**: `vm-digit-char`(`strings.lisp:386`)が単項。`(digit-char n radix)`未対応
- **内容**: 2引数形式。基数に応じた文字変換（基数>10で英字使用）
- **根拠**: ANSI CL 13.2 — digit-char
- **難易度**: Easy

#### FR-478: parse-integer Keyword Args (parse-integerキーワード引数)

- **対象**: `src/vm/strings.lisp`
- **現状**: `vm-parse-integer`(`strings.lisp:596-601`)が文字列のみ受け付け。`:start`/`:end`/`:radix`/`:junk-allowed`なし
- **内容**: ANSI CL準拠のキーワード引数サポート
- **根拠**: ANSI CL 12.2 — parse-integer
- **難易度**: Low

#### FR-479: Filesystem Operations (ファイルシステム操作)

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `rename-file`, `delete-file`, `probe-file`, `file-write-date`, `directory`, `ensure-directories-exist`のVM命令なし
- **内容**: ファイルシステム操作VM命令群の追加。ビルトイン登録
- **根拠**: ANSI CL 20.2 — File System Operations
- **難易度**: Medium

#### FR-480: with-open-file Macro (with-open-fileマクロ)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `with-input-from-string`/`with-output-to-string`のみ存在。`with-open-file`なし
- **内容**: `(with-open-file (stream path &rest open-args) &body body)` — ファイルストリームのopen/close自動管理マクロ
- **根拠**: ANSI CL 21.2 — with-open-file
- **依存**: FR-479
- **難易度**: Low

#### FR-481: prin1-to-string / princ-to-string Fidelity (prin1/princ-to-string区別)

- **対象**: `src/compile/builtin-registry.lisp`
- **現状**: `prin1-to-string`/`princ-to-string`が共に`make-vm-write-to-string-inst`にエイリアス(`builtin-registry.lisp:193-194`)。エスケープ有無の区別消失
- **内容**: `prin1-to-string`は`*print-escape*`=t、`princ-to-string`は`*print-escape*`=nilで出力。別VM命令またはフラグ引数
- **根拠**: ANSI CL 22.3 — prin1 vs princ
- **難易度**: Low

---

## 非機能要件

| 項目 | 要件 |
|------|------|
| テスト | 各最適化パスに既存FiveAMフレームワークでユニットテスト追加 |
| 後方互換性 | 既存3667テストが全pass維持 |
| セルフホスティング | `./cl-cc selfhost` 9/9 維持 |
| パイプライン統合 | 既存 `optimize-instructions` パイプラインに統合 |

---

## タスク分解

### 依存グラフ

```
Phase 1 — 独立 (並列実装可能):
  [T1]  FR-001: ペアホール30+ルール追加
  [T2]  FR-002: 葉関数検出パス
  [T3]  FR-003: LICM
  [T10] FR-010: SCCP
  [T11] FR-011: GVN
  [T16] FR-016: DSE

Phase 2 — 型基盤 (T4完了後に並列):
  [T4]  FR-004: 型アノテーション基盤
  [T5]  FR-005: Fixnum Fast Path       (T4依存)
  [T6]  FR-006: 条件分岐型特化         (T4依存)
  [T13] FR-013: 型特化インライン展開    (T4依存)
  [T17] FR-017: Alias Analysis         (T4依存)

Phase 3 — エスケープ解析 + ループ高度化 (Phase 1完了後):
  [T7]  FR-007: エスケープ解析
  [T12] FR-012: PRE                    (T3, T11依存)
  [T14] FR-014: SROA                   (T7依存)
  [T21] FR-021: SCEV                   (T3依存)
  [T22] FR-022: Loop Unrolling         (T21依存)

Phase 4 — GC最適化 (T7完了後):
  [T18] FR-018: Stack Allocation       (T7依存)
  [T19] FR-019: Write Barrier Elim     (T7, T18依存)
  [T20] FR-020: Allocation Sinking     (T7依存)

Phase 5 — ネイティブ + 高度 (T4, T7完了後):
  [T8]  FR-008: Float Unboxing         (T4依存)
  [T9]  FR-009: Monomorphic IC
  [T15] FR-015: Block Compilation
  [T23] FR-023: Polymorphic IC + Mega  (T9依存)
  [T24] FR-024: 型指定スロットアクセス  (T4, T5依存)
  [T25] FR-025: Dead Argument Elim     (T15依存)

T1, T2, T3, T10, T11, T16 (並列) ────────────────────────────────────────────────────┐
T26, T27, T28, T29, T30, T31 (CPS、並列) ────────────────────────────────────────────┤
T32, T33, T36, T40, T47, T48, T52 (独立、並列) ──────────────────────────────────────┤
T4 ──→ T5, T6, T13, T17, T34, T41 (並列) ──→ T7 ──→ T14, T18, T39 ──→ T19, T20      │
T3 ──→ T21 ──→ T22, T39                                                                 │
T9 ──→ T23                                                                               │
T15 ──→ T25, T51                                                                         │
T38 ──→ T39                                                                              └→ T8, T24, T56, T57
T37 ──→ T50, T51, T53
T54 ──→ T55 (Stream Fusion)

Phase 40 — 部分評価:
  T10 (SCCP) ──→ T123 (BTA) ──→ T122 (部分評価) ──→ T124 (関数特殊化)

Phase 41 — GC高度化:
  T125 (Object Pinning) ──→ T126 (Compaction, T125依存)

Phase 42 — CLOS高度化:
  T127 (Object Shape) ── 独立
  T128 (Method Combination) ── 独立

Phase 43 — メモリ解析:
  T60 (SSA) ──→ T130 (Memory SSA) ──→ T129 (Store-to-Load Forwarding)

Phase 44 — 数値演算:
  T131 (Karatsuba) ── 独立
  T7 (Escape Analysis) ──→ T132 (Complex Unboxing, T7+T14依存)

Phase 45 — 言語拡張:
  T133 (symbol-macrolet) ── 独立
  T134 (Delimited Continuations) ──→ T135 (Coroutines, T134依存)

Phase 46 — IC・プロファイル:
  T9, T23 ──→ T136 (IC State Machine)
  T137 (VM Sampling Profiler) ── 独立 (FR-058/FR-104の前提)
  T7, T14 ──→ T138 (Allocation Elimination)

Phase 47 — SIMD・ベクトル化:
  T143 (SIMD RegAlloc) ──→ T141 (x86-64 SSE/AVX), T142 (AArch64 NEON)
  T21 (SCEV) ──→ T139 (Auto-Vectorization, T143依存)
  T140 (SLP Vectorizer, T143依存)

Phase 48 — 投機的最適化基盤:
  T60 (SSA) ──→ T144 (Stack Map) ──→ T145 (Uncommon Trap) ──→ FR-155連携
  T146 (Safepoint Polling) ── 独立 (FR-090/FR-091の前提)

Phase 49 — 文字列・制御フロー:
  T147 (String Builder) ── 独立
  T148 (Defunctionalization, CPS依存)
  T149 (Decision Tree) ── 独立

Phase 50 — セキュリティ:
  T150 (Stack Canary) ── 独立
  T151 (CFI) ── 独立
  T152 (Sanitizer) ── 独立

Phase 51 — コンパイラ診断:
  T153 (Source Location) ── 独立 (FR-195の前提)
  T154 (Macro Tracing) ── 独立
  T155 (Compile Time Profiling) ── 独立
  T156 (Incremental Parsing, インフラ既存) ── 独立

Phase 52 — 動的コンパイル:
  T67 (Tiered Compilation) ──→ T157 (Trace JIT, T67依存)
  T4 (型基盤) ──→ T158 (Basic Block Versioning)

Phase 53 — GC高度化:
  T70 (Managed Heap) ──→ T159 (Ephemerons)
  T153 (Source Location) ──→ T160 (Unwind Tables, FR-195の前提)

Phase 54 — CL固有引数:
  T7 (Escape Analysis) ──→ T161 (&rest Stack Alloc)
  T162 (&key Hash Dispatch) ── 独立
  T4 (型基盤) ──→ T163 (Specialized Arrays)

Phase 55 — 高度解析・メモリ:
  T164 (Abstract Interpretation) ── 独立 (FR-010/FR-038/FR-040の統合基盤)
  T165 (Interprocedural RegAlloc, T15 Block Compilation依存)
  T166 (COW) ── 独立
  T167 (Region-Based Memory) ── 独立
  T159 (Ephemerons) ──→ T168 (Hash Consing, 弱HTが前提)
  T65 (Purity Inference) ──→ T169 (Auto-Memoization)

Phase 56 — 軽量並行性:
  T146 (Safepoint) ──→ T170 (Green Threads, FR-221+FR-233依存)
  T103 (Concurrent GC) ──→ T171 (Work-Stealing GC)
  T103 (Concurrent GC) ──→ T172 (GC Pause Budget)
  T173 (Lock Elision RTM, FR-191依存)

Phase 57 — プロファイル:
  T137 (VM Profiler) ──→ T174 (Value Profiling)
  T137 (VM Profiler) ──→ T175 (Call-Chain Profiling)
  T176 (Allocation Site Profiling) ── 独立

Phase 58 — データ表現:
  T177 (Pointer Compression) ── 独立
  T178 (Small String Opt) ── 独立
  T127 (Object Shape) ──→ T179 (Object Header Compression)

Phase 59 — コード生成・リンカ:
  T180 (Branch Hints) ── 独立
  T181 (ARM Constant Islands) ── 独立
  T182 (Linker Relaxation) ── 独立
  T109 (FFI) ──→ T183 (FFI Marshaling Specialization)

Phase 60 — SSA・言語機能:
  T60 (SSA統合) ──→ T184 (Trivial Phi Elimination)
  T103 (Concurrent GC) + T126 (Compaction) ──→ T185 (Read Barrier)
  T186 (Rational Specialization) ── 独立
  T187 (Extensible Sequences) ── 独立
  T188 (Package-Local Nicknames) ── 独立

Phase 61 — コンパイラパスインフラ:
  T189 (Optimization Levels) ── 独立
  T190 (Pass Dependency Resolution) ── 独立 (FR-147/FR-146の前提)
  T191 (IR Verification Suite) ── 独立
  T192 (IR Dump CLI) ── 独立
  T193 (Per-Pass Statistics) ── 独立
  T194 (Optimization Bisection) ── 独立

Phase 62 — 数値演算最適化:
  T195 (Division by Constant) ──→ T196 (Multiply-High, T195の前提)
  T197 (Rotate Instructions) ── 独立
  T198 (Byte Swap) ── 独立
  T141 (SSE/AVX) ──→ T199 (Transcendental Native, FR-228依存)

Phase 63 — キャッシュ・メモリ最適化:
  T3 (LICM) ──→ T200 (Loop Tiling, FR-003依存)
  T201 (Large Page Support) ── 独立
  T202 (Stride-Based Prefetch, FR-187拡張) ── 独立
  T203 (False Sharing Detection) ── 独立

Phase 64 — ネイティブバックエンド補完:
  T204 (ELF Executable) ── 独立
  T205 (PE/COFF) ── 独立
  T206 (Mach-O Relocation) ── 独立
  T207 (x86-64 Missing Instructions) ── 独立
  T208 (AArch64 Missing Instructions) ── 独立
  T209 (RISC-V Backend) ── 独立
  T210 (WASM Backend Completion) ── 独立
  T211 (vm-print Backend) ── 独立

Phase 65 — MIR・命令選択:
  T190 (Pass Deps) + T207/T208 ──→ T212 (MIR Instruction Selection Rules)

Phase 66 — ランタイム補完:
  T213 (Condition/Restart Handler) ── 独立
  T214 (rt-register-method) ── 独立

Phase 62b — 数値演算追加:
  T215 (Mod Power-of-2) ── 独立
  T216 (Overflow Detection) ── 独立 (FR-304の前提)
  T217 (Integer Range Analysis) ──→ T216 (Overflow Detection, 範囲解析で検査除去)
  T218 (Multiply by Constant Shift+Add) ── 独立
  T219 (Optimizer Cost Model) ── 独立

Phase 63b — キャッシュ追加:
  T220 (Object Co-Location) ── T70 (Managed Heap) 依存
  T221 (Data Structure Flattening) ── 独立
  T222 (Memory Access Pattern Analysis) ── T190 (Pass Deps) 依存

Phase 67 — デバッグ・診断:
  T223 (Interactive Debugger) ── 独立
  T224 (Native Disassembler) ── 独立
  T225 (REPL Enhancements) ── 独立
  T226 (Call Stack Pretty-Printer) ── 独立
  T227 (Watchpoints/Tracepoints) ── 独立
  T228 (Object Inspector) ── 独立
  T229 (Benchmark Framework) ── 独立
  T230 (Structured Diagnostics) ── 独立
  T231 (Warning System) ── T230 依存

Phase 68 — 開発者エコシステム:
  T232 (LSP Server) ── T230 (Diagnostics) + T156 (Incremental Parsing) 依存
  T233 (Code Formatter) ── 独立
  T234 (API Documentation Generator) ── 独立
  T229 (Benchmark) ──→ T235 (Continuous Benchmarking CI)
  T236 (Compilation Cache) ── 独立
  T237 (Static Analysis/Linting) ── T231 (Warning System) 依存
  T238 (ASDF Parallelization) ── 独立

Phase 69 — 呼び出し規約・VM高速化:
  T239 (Register Snapshot Elimination) ── 独立
  T240 (VM Argument Dedicated Slots) ── 独立
  T207 (x86-64 Missing Instr) ──→ T241 (Native CALL/RET)
  T242 (Callee-Saved Usage Analysis) ── 独立
  T243 (Closure Capture Dedup) ── 独立

Phase 70 — GC高度化 (追加):
  T244 (Free-List Reuse) ── 独立
  T245 (Precise GC Root) ── 独立
  T246 (Nursery Sizing) ── 独立
  T247 (Memory Pressure Callbacks) ── 独立
  T248 (Write Barrier Y2Y Elision) ── 独立
  T249 (GC-NaN-Boxing Integration) ── T245 依存
  T250 (Finalizer Ordering) ── T184 (Weak Ref) 依存

Phase 71 — コレクション・文字列最適化:
  T251 (Runtime String Interning) ── 独立
  T252 (Hash Table Size/Rehash) ── 独立
  T253 (Compile-Time Seq Folding) ── 独立
  T254 (HOF Macro Vector Path) ── 独立
  T255 (Dead Store Elimination Collections) ── T17 (Alias Analysis) 依存
  T256 (Set Ops Hash Accel) ── 独立
  T257 (String/List Algebraic Simplification) ── 独立

Phase 72 — 並行性・OS統合:
  T258 (Atomic Operations) ── 独立 (FR-191 Lock Elisionの前提)
  T259 (Memory Ordering Fences) ── T258 依存
  T258 + T259 ──→ T260 (Thread-Safe Registries)
  T261 (OS Signal Integration) ── 独立
  T262 (Memory-Mapped I/O) ── 独立
  T263 (Core Image Save) ── 独立
  T264 (Sandboxing) ── 独立
  T265 (Hot Code Reload) ── T258 依存
  T109 (FFI) + T258 ──→ T266 (Foreign Thread Callbacks)

Phase 73 — ANSI CL標準準拠:
  T267 (CCASE/CTYPECASE) ── 独立
  T268 (DEFSETF) ── 独立
  T213 (Condition/Restart) ──→ T269 (HANDLER-BIND Non-Unwinding)
  T270 (Pretty Printer) ── 独立
  T271 (Readtable API) ── 独立
  T272 (EQL Specializers) ── 独立
  T273 (MAPLIST/MAPL/MAPCON) ── 独立
  T274 (DO-SYMBOLS) ── 独立
  T275 (Logical Pathnames) ── 独立
  T276 (WITH-COMPILATION-UNIT) ── 独立
  T277 (NaN-Boxing Tag Fusion) ── 独立
  T278 (COMPILER-MACRO-FUNCTION API) ── T126 (Compiler Macro Registry) 依存
```

### 各タスクの実装ファイル

| タスク | FR | 対象ファイル | 難易度 |
|--------|-----|-------------|--------|
| T1  | FR-001 | `src/parse/prolog.lisp` | Easy |
| T2  | FR-002 | `src/optimize/optimizer.lisp` | Easy |
| T3  | FR-003 | `src/optimize/cfg.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T4  | FR-004 | `src/compile/codegen.lisp`, `src/type/inference.lisp` | Hard |
| T5  | FR-005 | `src/compile/codegen.lisp`, `src/compile/builtin-registry.lisp` | Medium |
| T6  | FR-006 | `src/compile/codegen.lisp` | Medium |
| T7  | FR-007 | `src/compile/closure.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T8  | FR-008 | `src/backend/x86-64-codegen.lisp`, `src/runtime/value.lisp` | Hard |
| T9  | FR-009 | `src/vm/vm.lisp` | Hard |
| T10 | FR-010 | `src/optimize/optimizer.lisp` | Medium |
| T11 | FR-011 | `src/optimize/optimizer.lisp` | Medium |
| T12 | FR-012 | `src/optimize/optimizer.lisp` | Medium |
| T13 | FR-013 | `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T14 | FR-014 | `src/optimize/optimizer.lisp`, `src/vm/vm.lisp` | Hard |
| T15 | FR-015 | `src/cli/main.lisp`, コンパイルパイプライン | Hard |
| T16 | FR-016 | `src/optimize/optimizer.lisp` | Easy |
| T17 | FR-017 | `src/optimize/optimizer.lisp`, `src/optimize/cfg.lisp` | Medium |
| T18 | FR-018 | `src/compile/codegen.lisp`, `src/runtime/` | Hard |
| T19 | FR-019 | `src/runtime/gc.lisp`, codegen | Hard |
| T20 | FR-020 | `src/optimize/optimizer.lisp` | Medium |
| T21 | FR-021 | `src/optimize/cfg.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T22 | FR-022 | `src/optimize/optimizer.lisp` | Medium |
| T23 | FR-023 | `src/vm/vm.lisp` | Medium |
| T24 | FR-024 | `src/vm/vm.lisp`, `src/compile/codegen.lisp` | Hard |
| T25 | FR-025 | `src/optimize/optimizer.lisp` | Medium |
| T26 | FR-026 | `src/compile/cps.lisp`, `src/optimize/optimizer.lisp` | Easy |
| T27 | FR-027 | `src/compile/cps.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T28 | FR-028 | `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T29 | FR-029 | `src/compile/codegen.lisp` | Medium |
| T30 | FR-030 | `src/compile/codegen.lisp`, `src/vm/vm.lisp` | Medium |
| T31 | FR-031 | `src/compile/closure.lisp`, `src/compile/codegen.lisp` | Medium |
| T32 | FR-032 | `src/optimize/optimizer.lisp`, `src/optimize/cfg.lisp` | Medium |
| T33 | FR-033 | `src/compile/codegen.lisp` | Medium |
| T34 | FR-034 | `src/backend/x86-64-codegen.lisp` | Medium |
| T35 | FR-035 | `src/optimize/optimizer.lisp` + バックエンド | Medium |
| T36 | FR-036 | `src/backend/x86-64-codegen.lisp`, `src/backend/aarch64.lisp` | Easy |
| T37 | FR-037 | `src/optimize/optimizer.lisp` | Medium |
| T38 | FR-038 | `src/optimize/optimizer.lisp`, `src/type/inference.lisp` | Medium |
| T39 | FR-039 | `src/optimize/optimizer.lisp` + codegen | Medium |
| T40 | FR-040 | `src/optimize/optimizer.lisp` | Easy |
| T41 | FR-041 | `src/optimize/optimizer.lisp` | Medium |
| T42 | FR-042 | `src/vm/vm.lisp:998`, `src/compile/codegen.lisp` | Medium |
| T43 | FR-043 | `src/vm/vm.lisp:19-31,550` | Medium |
| T44 | FR-044 | `src/vm/vm.lisp:852`, `src/compile/codegen.lisp` | Low |
| T45 | FR-045 | `src/compile/codegen.lisp`, `src/compile/cps.lisp` | Hard |
| T46 | FR-046 | `src/compile/codegen.lisp:100`, `src/vm/conditions.lisp` | Medium |
| T47 | FR-047 | `src/vm/io.lisp:247`, `src/compile/codegen.lisp` | Low |
| T48 | FR-048 | `src/vm/hash.lisp:116`, `src/compile/codegen.lisp` | Low |
| T49 | FR-049 | `src/vm/primitives.lisp:11`, `src/compile/codegen.lisp` | Medium |
| T50 | FR-050 | `src/optimize/optimizer.lisp` | Hard |
| T51 | FR-051 | `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp` | Hard |
| T52 | FR-052 | `src/optimize/optimizer.lisp` | Easy |
| T53 | FR-053 | `src/optimize/optimizer.lisp:729-820` | Hard |
| T54 | FR-054 | `src/expand/macro.lisp` or `src/optimize/optimizer.lisp` | Hard |
| T55 | FR-055 | `src/optimize/optimizer.lisp` | Medium |
| T56 | FR-056 | `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T57 | FR-057 | `src/emit/mir.lisp`, `src/compile/codegen.lisp` | Very Hard |
| T58 | FR-058 | `src/vm/vm.lisp` (IC計測), コンパイルパイプライン | Hard |
| T59 | FR-146 | `src/optimize/egraph.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T60 | FR-147 | `src/optimize/ssa.lisp`, `src/optimize/cfg.lisp`, `src/optimize/optimizer.lisp` | Very Hard |
| T61 | FR-148 | `src/type/inference.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T62 | FR-149 | `src/vm/primitives.lisp`, `src/compile/codegen.lisp` | Medium |
| T63 | FR-150 | `src/optimize/optimizer.lisp` | Medium |
| T64 | FR-151 | `src/compile/pipeline.lisp`, `src/cli/main.lisp` | Hard |
| T65 | FR-152 | `src/optimize/effects.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T66 | FR-153 | `src/expand/expander.lisp` | Easy |
| T67 | FR-154 | `src/compile/pipeline.lisp`, `src/optimize/optimizer.lisp` | Very Hard |
| T68 | FR-155 | `src/vm/vm.lisp`, `src/compile/codegen.lisp` | Very Hard |
| T69 | FR-156 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Medium |
| T70 | FR-157 | `src/vm/list.lisp`, `src/runtime/gc.lisp` | Hard |
| T71 | FR-158 | `src/optimize/optimizer.lisp`, `src/cli/main.lisp` | Hard |
| T72 | FR-159 | `src/compile/cps.lisp` | Medium |
| T73 | FR-160 | `src/compile/closure.lisp`, `src/compile/codegen.lisp` | Hard |
| T74 | FR-161 | `src/compile/cps.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T75 | FR-162 | `src/compile/cps.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T76 | FR-163 | `src/optimize/optimizer.lisp`, `src/optimize/cfg.lisp` | Medium |
| T77 | FR-164 | `src/optimize/optimizer.lisp` | Hard |
| T78 | FR-165 | `src/optimize/cfg.lisp` | Easy |
| T79 | FR-166 | `src/optimize/optimizer.lisp` | Easy |
| T80 | FR-167 | `src/optimize/cfg.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T81 | FR-168 | `src/optimize/optimizer.lisp` | Medium |
| T82 | FR-169 | `src/optimize/cfg.lisp` | Medium |
| T83 | FR-170 | `src/optimize/optimizer.lisp`, `src/optimize/cfg.lisp` | Medium |
| T84 | FR-171 | `src/emit/x86-64-codegen.lisp` | Medium |
| T85 | FR-172 | `src/emit/x86-64-codegen.lisp` | Medium |
| T86 | FR-173 | `src/emit/x86-64-codegen.lisp` | Medium |
| T87 | FR-174 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Medium |
| T88 | FR-175 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Very Hard |
| T89 | FR-176 | `src/emit/calling-convention.lisp`, `src/compile/codegen.lisp` | Hard |
| T90 | FR-177 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Easy |
| T91 | FR-178 | `src/emit/x86-64-codegen.lisp` | Easy |
| T92 | FR-179 | `src/expand/macros-stdlib.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T93 | FR-180 | `src/compile/codegen.lisp`, `src/vm/vm.lisp` | Easy |
| T94 | FR-181 | `src/compile/codegen-core.lisp`, `src/compile/pipeline.lisp` | Easy |
| T95 | FR-182 | `src/optimize/optimizer.lisp`, `src/type/inference.lisp` | Hard |
| T96 | FR-183 | `src/compile/builtin-registry.lisp`, `src/optimize/effects.lisp` | Medium |
| T97 | FR-184 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Hard |
| T98 | FR-185 | `src/optimize/optimizer.lisp`, `src/cli/main.lisp` | Easy |
| T99 | FR-186 | `src/compile/pipeline.lisp`, `src/emit/binary/macho.lisp` | Medium |
| T100 | FR-187 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Medium |
| T101 | FR-188 | `src/emit/x86-64.lisp`, `src/emit/x86-64-codegen.lisp` | Easy |
| T102 | FR-189 | `src/runtime/heap.lisp`, `src/compile/codegen-clos.lisp` | Hard |
| T103 | FR-190 | `src/runtime/gc.lisp` | Very Hard |
| T104 | FR-191 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Hard |
| T105 | FR-192 | `src/vm/vm.lisp`, `src/runtime/heap.lisp` | Very Hard |
| T106 | FR-193 | `src/vm/vm.lisp` | Medium |
| T107 | FR-194 | `src/compile/codegen.lisp`, `src/emit/calling-convention.lisp` | Very Hard |
| T108 | FR-195 | `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp` | Hard |
| T109 | FR-196 | `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp` | Very Hard |
| T110 | FR-197 | `src/emit/x86-64-codegen.lisp` | Hard |
| T111 | FR-198 | `src/emit/binary/macho.lisp` | Medium |
| T112 | FR-199 | `src/emit/regalloc.lisp` | Medium |
| T113 | FR-200 | `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp` | Very Hard |
| T114 | FR-201 | `src/optimize/cfg.lisp`, `src/emit/x86-64-codegen.lisp` | Very Hard |
| T115 | FR-202 | `src/emit/wasm.lisp`, `src/emit/wasm-trampoline.lisp` | Hard |
| T116 | FR-203 | `src/emit/wasm.lisp` | Very Hard |
| T117 | FR-204 | `src/emit/wasm-trampoline.lisp`, `src/emit/wasm-types.lisp` | Medium |
| T118 | FR-205 | `src/optimize/optimizer.lisp`, `tests/` | Very Hard |
| T119 | FR-206 | `tests/framework/framework-fuzz.lisp` | Medium |
| T120 | FR-207 | `src/compile/pipeline.lisp`, `src/emit/binary/macho.lisp` | Medium |
| T121 | FR-208 | `src/emit/target.lisp`, `src/compile/pipeline.lisp` | Hard |
| T122 | FR-209 | `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp` | Hard |
| T123 | FR-210 | `src/optimize/optimizer.lisp`, `src/type/inference.lisp` | Hard |
| T124 | FR-211 | `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp` | Very Hard |
| T125 | FR-212 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Hard |
| T126 | FR-213 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Very Hard |
| T127 | FR-214 | `src/vm/vm-clos.lisp`, `src/runtime/heap.lisp` | Very Hard |
| T128 | FR-215 | `src/vm/vm-clos.lisp`, `src/compile/codegen-clos.lisp` | Hard |
| T129 | FR-216 | `src/optimize/optimizer.lisp` | Medium |
| T130 | FR-217 | `src/optimize/ssa.lisp`, `src/optimize/optimizer.lisp` | Very Hard |
| T131 | FR-218 | `src/vm/vm-numeric.lisp`, `src/vm/primitives.lisp` | Very Hard |
| T132 | FR-219 | `src/vm/vm-numeric.lisp`, `src/compile/codegen.lisp` | Hard |
| T133 | FR-220 | `src/expand/expander.lisp`, `src/parse/cst.lisp` | Medium |
| T134 | FR-221 | `src/compile/cps.lisp`, `src/vm/vm.lisp` | Very Hard |
| T135 | FR-222 | `src/vm/vm.lisp`, `src/compile/codegen.lisp` | Hard |
| T136 | FR-223 | `src/vm/vm.lisp`, `src/vm/vm-clos.lisp` | Hard |
| T137 | FR-224 | `src/vm/vm.lisp`, `src/cli/main.lisp` | Medium |
| T138 | FR-225 | `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp` | Hard |
| T139 | FR-226 | `src/optimize/optimizer.lisp`, `src/optimize/cfg.lisp` | Very Hard |
| T140 | FR-227 | `src/optimize/optimizer.lisp` | Hard |
| T141 | FR-228 | `src/emit/x86-64-codegen.lisp`, `src/emit/regalloc.lisp` | Hard |
| T142 | FR-229 | `src/emit/aarch64-codegen.lisp`, `src/emit/regalloc.lisp` | Hard |
| T143 | FR-230 | `src/emit/regalloc.lisp` | Hard |
| T144 | FR-231 | `src/emit/mir.lisp`, `src/emit/regalloc.lisp` | Hard |
| T145 | FR-232 | `src/emit/mir.lisp`, `src/vm/vm.lisp`, `src/compile/codegen.lisp` | Hard |
| T146 | FR-233 | `src/runtime/gc.lisp`, `src/vm/vm.lisp` | Medium |
| T147 | FR-234 | `src/vm/strings.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T148 | FR-235 | `src/compile/cps.lisp`, `src/compile/codegen.lisp` | Very Hard |
| T149 | FR-236 | `src/expand/macros-basic.lisp`, `src/compile/codegen.lisp` | Medium |
| T150 | FR-237 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Medium |
| T151 | FR-238 | `src/emit/x86-64-codegen.lisp`, `src/compile/codegen.lisp` | Hard |
| T152 | FR-239 | `src/emit/x86-64-codegen.lisp`, `src/runtime/heap.lisp` | Very Hard |
| T153 | FR-240 | `src/compile/codegen.lisp`, `src/parse/ast.lisp` | Medium |
| T154 | FR-241 | `src/expand/expander.lisp`, `src/cli/main.lisp` | Easy |
| T155 | FR-242 | `src/compile/pipeline.lisp`, `src/cli/main.lisp` | Easy |
| T156 | FR-243 | `src/parse/incremental.lisp`, `src/compile/pipeline.lisp` | Medium |
| T157 | FR-244 | `src/vm/vm.lisp`, `src/compile/pipeline.lisp` | Very Hard |
| T158 | FR-245 | `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T159 | FR-246 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Hard |
| T160 | FR-247 | `src/emit/binary/elf.lisp`, `src/emit/binary/macho.lisp` | Hard |
| T161 | FR-248 | `src/vm/vm.lisp`, `src/compile/codegen-functions.lisp` | Medium |
| T162 | FR-249 | `src/vm/vm.lisp`, `src/compile/codegen-functions.lisp` | Medium |
| T163 | FR-250 | `src/vm/list.lisp`, `src/compile/codegen.lisp` | Hard |
| T164 | FR-251 | `src/type/inference.lisp`, `src/optimize/optimizer.lisp` | Very Hard |
| T165 | FR-252 | `src/emit/regalloc.lisp`, `src/emit/calling-convention.lisp` | Very Hard |
| T166 | FR-253 | `src/vm/list.lisp`, `src/vm/hash.lisp` | Hard |
| T167 | FR-254 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Hard |
| T168 | FR-255 | `src/vm/list.lisp`, `src/runtime/heap.lisp` | Medium |
| T169 | FR-256 | `src/optimize/optimizer.lisp`, `src/optimize/effects.lisp` | Medium |
| T170 | FR-257 | `src/vm/vm.lisp`, `src/runtime/` (新規) | Very Hard |
| T171 | FR-258 | `src/runtime/gc.lisp` | Hard |
| T172 | FR-259 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Hard |
| T173 | FR-260 | `src/emit/x86-64-codegen.lisp` | Medium |
| T174 | FR-261 | `src/vm/vm.lisp`, `src/compile/pipeline.lisp` | Medium |
| T175 | FR-262 | `src/vm/vm.lisp`, `src/cli/main.lisp` | Medium |
| T176 | FR-263 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Medium |
| T177 | FR-264 | `src/runtime/value.lisp`, `src/runtime/heap.lisp` | Very Hard |
| T178 | FR-265 | `src/vm/strings.lisp`, `src/runtime/value.lisp` | Hard |
| T179 | FR-266 | `src/vm/vm-clos.lisp`, `src/runtime/heap.lisp` | Hard |
| T180 | FR-267 | `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp` | Easy |
| T181 | FR-268 | `src/emit/aarch64-codegen.lisp` | Medium |
| T182 | FR-269 | `src/emit/binary/elf.lisp`, `src/emit/binary/macho.lisp` | Hard |
| T183 | FR-270 | `src/compile/codegen.lisp`, `src/emit/calling-convention.lisp` | Hard |
| T184 | FR-271 | `src/optimize/ssa.lisp` | Easy |
| T185 | FR-272 | `src/runtime/gc.lisp`, `src/compile/codegen.lisp` | Very Hard |
| T186 | FR-273 | `src/vm/vm-numeric.lisp`, `src/compile/codegen.lisp` | Medium |
| T187 | FR-274 | `src/vm/list.lisp`, `src/compile/codegen.lisp` | Hard |
| T188 | FR-275 | `src/vm/packages.lisp`, `src/parse/cl/parser.lisp` | Medium |
| T189 | FR-276 | `src/cli/main.lisp`, `src/optimize/optimizer.lisp` | Medium |
| T190 | FR-277 | `src/optimize/optimizer.lisp` | Hard |
| T191 | FR-278 | `src/compile/ir/block.lisp`, `src/compile/ir/verify.lisp` | Medium |
| T192 | FR-279 | `src/cli/main.lisp`, `src/compile/ir/printer.lisp` | Easy |
| T193 | FR-280 | `src/optimize/optimizer.lisp` | Easy |
| T194 | FR-281 | `src/optimize/optimizer.lisp` | Medium |
| T195 | FR-282 | `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp` | Hard |
| T196 | FR-283 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Medium |
| T197 | FR-284 | `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp` | Medium |
| T198 | FR-285 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Easy |
| T199 | FR-286 | `src/emit/x86-64-codegen.lisp`, `src/vm/vm-numeric.lisp` | Hard |
| T200 | FR-287 | `src/optimize/optimizer.lisp`, `src/optimize/loop-transforms.lisp` | Very Hard |
| T201 | FR-288 | `src/runtime/heap.lisp`, `src/runtime/gc.lisp` | Medium |
| T202 | FR-289 | `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp` | Hard |
| T203 | FR-290 | `src/compile/codegen-clos.lisp`, `src/runtime/heap.lisp` | Hard |
| T204 | FR-291 | `src/emit/binary/elf.lisp` | Very Hard |
| T205 | FR-292 | `src/emit/binary/pe.lisp` | Very Hard |
| T206 | FR-293 | `src/emit/binary/macho.lisp` | Hard |
| T207 | FR-294 | `src/emit/x86-64-codegen.lisp`, `src/emit/x86-64.lisp` | Hard |
| T208 | FR-295 | `src/emit/aarch64-codegen.lisp`, `src/emit/aarch64.lisp` | Hard |
| T209 | FR-296 | `src/emit/riscv64-codegen.lisp`, `src/emit/target.lisp` | Very Hard |
| T210 | FR-297 | `src/emit/wasm.lisp` | Very Hard |
| T211 | FR-298 | `src/emit/x86-64.lisp`, `src/emit/aarch64.lisp` | Medium |
| T212 | FR-299 | `src/emit/isel/x86-64-rules.lisp`, `src/emit/mir.lisp` | Very Hard |
| T213 | FR-300 | `src/runtime/runtime.lisp` | Hard |
| T214 | FR-301 | `src/runtime/runtime.lisp` | Hard |
| T215 | FR-302 | `src/optimize/optimizer.lisp` | Easy |
| T216 | FR-303 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Hard |
| T217 | FR-304 | `src/optimize/optimizer.lisp`, `src/type/inference.lisp` | Hard |
| T218 | FR-305 | `src/optimize/optimizer.lisp` | Medium |
| T219 | FR-306 | `src/optimize/optimizer.lisp` | Medium |
| T220 | FR-307 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Hard |
| T221 | FR-308 | `src/vm/vm-clos.lisp`, `src/vm/vm.lisp` | Hard |
| T222 | FR-309 | `src/optimize/optimizer.lisp`, `src/emit/mir.lisp` | Very Hard |
| T223 | FR-310 | `src/vm/vm-run.lisp`, `src/cli/main.lisp` | Hard |
| T224 | FR-311 | `src/emit/disasm/x86-64-disasm.lisp` | Very Hard |
| T225 | FR-312 | `src/cli/main.lisp` | Medium |
| T226 | FR-313 | `src/vm/vm.lisp`, `src/vm/vm-run.lisp` | Medium |
| T227 | FR-314 | `src/vm/vm-run.lisp` | Medium |
| T228 | FR-315 | `src/vm/vm.lisp` | Easy |
| T229 | FR-316 | `tests/framework/`, `benchmarks/` | Medium |
| T230 | FR-317 | `src/parse/diagnostics.lisp`, `src/vm/conditions.lisp` | Medium |
| T231 | FR-318 | `src/compile/codegen.lisp`, `src/parse/diagnostics.lisp` | Medium |
| T232 | FR-319 | `src/lsp/`, `src/cli/main.lisp` | Very Hard |
| T233 | FR-320 | `src/format/formatter.lisp` | Medium |
| T234 | FR-321 | `src/tools/docgen.lisp` | Medium |
| T235 | FR-322 | `.github/workflows/benchmark.yml` | Medium |
| T236 | FR-323 | `src/compile/pipeline.lisp`, `src/compile/cache.lisp` | Hard |
| T237 | FR-324 | `src/analysis/`, `src/cli/main.lisp` | Medium |
| T238 | FR-325 | `cl-cc.asd` | Easy |
| T239 | FR-326 | `src/vm/vm.lisp` | Medium |
| T240 | FR-327 | `src/vm/vm.lisp`, `src/compile/codegen.lisp` | Medium |
| T241 | FR-328 | `src/emit/x86-64-codegen.lisp` | Hard |
| T242 | FR-329 | `src/emit/x86-64-codegen.lisp`, `src/emit/regalloc.lisp` | Easy |
| T243 | FR-330 | `src/compile/codegen.lisp`, `src/compile/closure.lisp` | Medium |
| T244 | FR-331 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Easy |
| T245 | FR-332 | `src/runtime/gc.lisp`, `src/vm/vm.lisp` | Medium |
| T246 | FR-333 | `src/runtime/heap.lisp` | Medium |
| T247 | FR-334 | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` | Easy |
| T248 | FR-335 | `src/runtime/gc.lisp` | Easy |
| T249 | FR-336 | `src/runtime/gc.lisp`, `src/runtime/value.lisp` | Medium |
| T250 | FR-337 | `src/runtime/gc.lisp` | Hard |
| T251 | FR-338 | `src/vm/strings.lisp`, `src/vm/vm.lisp` | Medium |
| T252 | FR-339 | `src/vm/hash.lisp` | Easy |
| T253 | FR-340 | `src/optimize/optimizer.lisp` | Easy |
| T254 | FR-341 | `src/expand/macros-stdlib.lisp` | Medium |
| T255 | FR-342 | `src/optimize/optimizer.lisp`, `src/optimize/effects.lisp` | Hard |
| T256 | FR-343 | `src/expand/macros-stdlib.lisp` | Easy |
| T257 | FR-344 | `src/optimize/optimizer.lisp` | Medium |
| T258 | FR-345 | `src/vm/vm.lisp`, `src/emit/x86-64-codegen.lisp` | Medium |
| T259 | FR-346 | `src/emit/x86-64-codegen.lisp`, `src/emit/aarch64-codegen.lisp` | Hard |
| T260 | FR-347 | `src/vm/vm.lisp`, `src/runtime/runtime.lisp` | Hard |
| T261 | FR-348 | `src/runtime/runtime.lisp`, `src/runtime/signals.lisp` | Hard |
| T262 | FR-349 | `src/runtime/runtime.lisp`, `src/runtime/mmap.lisp` | Medium |
| T263 | FR-350 | `src/runtime/image.lisp`, `src/vm/vm.lisp` | Hard |
| T264 | FR-351 | `src/vm/vm.lisp`, `src/cli/main.lisp` | Medium |
| T265 | FR-352 | `src/vm/vm.lisp`, `src/compile/pipeline.lisp` | Medium |
| T266 | FR-353 | `src/runtime/runtime.lisp`, `src/emit/x86-64-codegen.lisp` | Very Hard |
| T267 | FR-354 | `src/expand/macros-stdlib.lisp` | Easy |
| T268 | FR-355 | `src/expand/expander.lisp`, `src/expand/macros-stdlib.lisp` | Medium |
| T269 | FR-356 | `src/expand/macros-stdlib.lisp`, `src/vm/conditions.lisp` | Hard |
| T270 | FR-357 | `src/vm/pprint.lisp` | Hard |
| T271 | FR-358 | `src/parse/cl/lexer.lisp` | Hard |
| T272 | FR-359 | `src/compile/codegen-clos.lisp`, `src/vm/vm.lisp` | Medium |
| T273 | FR-360 | `src/expand/macros-stdlib.lisp` | Easy |
| T274 | FR-361 | `src/expand/macros-stdlib.lisp`, `src/vm/` | Medium |
| T275 | FR-362 | `src/runtime/runtime.lisp` | Medium |
| T276 | FR-363 | `src/compile/pipeline.lisp` | Easy |
| T277 | FR-364 | `src/runtime/value.lisp`, `src/emit/x86-64-codegen.lisp` | Medium |
| T278 | FR-365 | `src/expand/expander.lisp`, `src/expand/macros-stdlib.lisp` | Medium |
| T279 | FR-366 | `src/compile/cps.lisp`, `src/compile/pipeline.lisp` | Medium |
| T280 | FR-367 | `src/compile/cps.lisp`, `src/compile/codegen-core.lisp` | Medium |
| T281 | FR-368 | `src/compile/cps.lisp` | Medium |
| T282 | FR-369 | `src/emit/mir.lisp`, `src/compile/cps.lisp` | Hard |
| T283 | FR-370 | `src/compile/cps.lisp` | Medium |
| T284 | FR-371 | `src/compile/cps.lisp` | Hard |
| T285 | FR-372 | `src/compile/cps.lisp` | Easy |
| T286 | FR-373 | `src/compile/cps.lisp` | Easy |
| T287 | FR-374 | `src/parse/cl/parser.lisp`, `src/parse/ast.lisp`, `src/vm/vm-clos.lisp` | Medium |
| T288 | FR-375 | `src/parse/cl/parser.lisp`, `src/parse/ast.lisp`, `src/vm/vm-clos.lisp` | Medium |
| T289 | FR-376 | `src/vm/vm-clos.lisp`, `src/compile/codegen-clos.lisp` | Hard |
| T290 | FR-377 | `src/vm/vm.lisp`, `src/expand/macros-stdlib.lisp` | Medium |
| T291 | FR-378 | `src/vm/vm-clos.lisp` | Medium |
| T292 | FR-379 | `src/vm/vm-clos.lisp`, `src/compile/codegen-clos.lisp` | Hard |
| T293 | FR-380 | `src/expand/macros-sequence.lisp` | Medium |
| T294 | FR-381 | `src/vm/vm.lisp` | Easy |
| T295 | FR-382 | `src/parse/cl/parser.lisp`, `src/parse/ast.lisp` | Medium |
| T296 | FR-383 | `src/parse/cl/parser.lisp`, `src/parse/ast.lisp` | Easy |
| T297 | FR-384 | `src/vm/vm-clos.lisp` | Medium |
| T298 | FR-385 | `src/vm/vm-clos.lisp`, `src/compile/builtin-registry.lisp` | Medium |
| T299 | FR-386 | `src/vm/primitives.lisp`, `src/compile/builtin-registry.lisp` | Easy |
| T300 | FR-387 | `src/vm/io.lisp`, `src/runtime/runtime.lisp` | Easy |
| T301 | FR-388 | `src/vm/gray-streams.lisp`(新規), `src/compile/codegen.lisp` | Hard |
| T302 | FR-389 | `src/vm/format.lisp`(新規) | Very Hard |
| T303 | FR-390 | `src/vm/io.lisp`, `src/compile/codegen-clos.lisp` | Medium |
| T304 | FR-391 | `src/vm/strings.lisp` | Easy |
| T305 | FR-392 | `src/vm/primitives.lisp`, `src/vm/strings.lisp` | Easy |
| T306 | FR-393 | `src/vm/io.lisp`, `src/compile/builtin-registry.lisp` | Medium |
| T307 | FR-394 | `src/compile/context.lisp` | Medium |
| T308 | FR-395 | `src/compile/context.lisp`, `src/expand/expander.lisp` | Medium |
| T309 | FR-396 | `src/compile/pipeline.lisp`, `src/expand/expander.lisp` | Low-Medium |
| T310 | FR-397 | `src/expand/macros-sequence.lisp` | Low |
| T311 | FR-398 | `src/expand/expander.lisp`, `src/expand/macros-stdlib.lisp` | Low |
| T312 | FR-399 | `src/expand/expander.lisp` | Medium |
| T313 | FR-400 | `src/expand/macros-sequence.lisp` | Low |
| T314 | FR-401 | `src/compile/codegen-core.lisp` | Low |
| T315 | FR-402 | `src/expand/macros-stdlib.lisp` | Low |
| T316 | FR-403 | `src/emit/x86-64-codegen.lisp` | Medium |
| T317 | FR-404 | `src/compile/pipeline.lisp`, `src/vm/vm.lisp` | Hard |
| T318 | FR-405 | `src/cli/main.lisp`, `src/compile/pipeline.lisp` | Medium |
| T319 | FR-406 | `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp` | Medium |
| T320 | FR-407 | `src/emit/regalloc.lisp` | Medium |
| T321 | FR-408 | `src/optimize/optimizer.lisp` | Low-Medium |
| T322 | FR-409 | `src/optimize/optimizer.lisp` | Medium |
| T323 | FR-410 | `src/optimize/optimizer.lisp` | Medium |
| T324 | FR-411 | `src/optimize/optimizer.lisp` | Low-Medium |
| T325 | FR-412 | `src/emit/regalloc.lisp` | Medium |
| T326 | FR-413 | `src/emit/regalloc.lisp` | Medium |
| T327 | FR-414 | `src/optimize/optimizer.lisp` | Low-Medium |
| T328 | FR-415 | `src/optimize/optimizer.lisp` | Low |
| T329 | FR-416 | `src/emit/regalloc.lisp` | Easy |
| T330 | FR-417 | `src/expand/macros-stdlib.lisp` | Medium |
| T331 | FR-418 | `src/compile/builtin-registry.lisp`, `src/vm/conditions.lisp` | Medium |
| T332 | FR-419 | `src/compile/builtin-registry.lisp` | Low |
| T333 | FR-420 | `src/expand/macros-stdlib.lisp` | Hard |
| T334 | FR-421 | `src/expand/macros-stdlib.lisp` | Medium |
| T335 | FR-422 | `src/expand/macros-stdlib.lisp` | Low |
| T336 | FR-423 | `src/expand/macros-stdlib.lisp` | Medium |
| T337 | FR-424 | `src/vm/conditions.lisp` | Medium |
| T338 | FR-425 | `src/expand/macros-stdlib.lisp`, `src/vm/conditions.lisp` | Medium |
| T339 | FR-426 | `src/expand/macros-basic.lisp` | Medium |
| T340 | FR-427 | `src/vm/conditions.lisp`, `src/compile/builtin-registry.lisp` | Low |
| T341 | FR-428 | `src/expand/macro.lisp` | Low |
| T342 | FR-429 | `src/expand/expander.lisp` | Low |
| T343 | FR-430 | `src/expand/expander.lisp` | Medium |
| T344 | FR-431 | `src/expand/macros-stdlib.lisp` | Low |
| T345 | FR-432 | `src/expand/macros-stdlib.lisp`, `src/vm/vm.lisp` | Medium |
| T346 | FR-433 | `src/expand/macros-stdlib.lisp`, `src/vm/vm-run.lisp` | Medium |
| T347 | FR-434 | `src/vm/vm.lisp`, `src/runtime/heap.lisp` | Low |
| T348 | FR-435 | `src/expand/macros-stdlib.lisp`, `src/vm/vm.lisp` | Low |
| T349 | FR-436 | `src/vm/vm.lisp` | Medium |
| T350 | FR-437 | `src/expand/macros-sequence.lisp`, `src/vm/vm.lisp` | Hard |
| T351 | FR-438 | `src/vm/vm.lisp` | Medium |
| T352 | FR-439 | `src/expand/expander.lisp` | Medium |
| T353 | FR-440 | `src/expand/macros-stdlib.lisp` | Low |
| T354 | FR-441 | `src/expand/macros-stdlib.lisp` | Medium |
| T355 | FR-442 | `src/compile/codegen-phase2.lisp` | Medium |
| T356 | FR-443 | `src/compile/codegen-phase2.lisp`, `src/vm/list.lisp` | Hard |
| T357 | FR-444 | `src/expand/macros-sequence.lisp` | Low |
| T358 | FR-445 | `src/expand/macros-sequence.lisp` | Low |
| T359 | FR-446 | `src/expand/expander-defstruct.lisp` | Low |
| T360 | FR-447 | `src/expand/expander-defstruct.lisp` | Medium |
| T361 | FR-448 | `src/expand/expander-defstruct.lisp` | Medium |
| T362 | FR-449 | `src/expand/expander-defstruct.lisp` | Low |
| T363 | FR-450 | `src/expand/macros-sequence.lisp`, `src/expand/macros-stdlib.lisp` | Hard |
| T364 | FR-451 | `src/expand/macros-sequence.lisp` | Medium |
| T365 | FR-452 | `src/expand/macros-sequence.lisp` | Low |
| T366 | FR-453 | `src/expand/macros-sequence.lisp` | Medium |
| T367 | FR-454 | `src/vm/vm-run.lisp` | Medium |
| T368 | FR-455 | `src/vm/vm-run.lisp` | Easy |
| T369 | FR-456 | `src/bytecode/encode.lisp`, `src/vm/vm-run.lisp` | Hard |
| T370 | FR-457 | `src/vm/vm.lisp` | Low |
| T371 | FR-458 | `src/vm/vm-run.lisp` | Medium |
| T372 | FR-459 | `src/vm/vm-run.lisp`, `src/optimize/optimizer.lisp` | Hard |
| T373 | FR-460 | `src/vm/vm-run.lisp` | Low |
| T374 | FR-461 | `src/vm/vm.lisp` | Low |
| T375 | FR-462 | `src/vm/vm.lisp` | Low |
| T376 | FR-463 | `src/vm/vm-run.lisp` | Medium |
| T377 | FR-464 | `src/emit/binary/macho.lisp` | Low |
| T378 | FR-465 | `src/emit/binary/macho.lisp` | Medium |
| T379 | FR-466 | `src/emit/binary/macho.lisp` | Medium |
| T380 | FR-467 | `src/emit/binary/macho.lisp` | Easy |
| T381 | FR-468 | `src/emit/binary/elf.lisp` | Medium |
| T382 | FR-469 | `src/emit/binary/elf.lisp` | Low |
| T383 | FR-470 | `src/emit/binary/elf.lisp` | Low |
| T384 | FR-471 | `src/emit/binary/macho.lisp`, `src/emit/binary/elf.lisp`, `src/emit/wasm.lisp` | Medium |
| T385 | FR-472 | `src/emit/calling-convention.lisp` | Medium |
| T386 | FR-473 | `src/emit/wasm.lisp` | Low |
| T387 | FR-474 | `src/vm/vm-numeric.lisp`, `src/compile/builtin-registry.lisp` | Easy |
| T388 | FR-475 | `src/vm/strings.lisp` | Easy |
| T389 | FR-476 | `src/vm/vm-numeric.lisp` | Easy |
| T390 | FR-477 | `src/vm/strings.lisp` | Easy |
| T391 | FR-478 | `src/vm/strings.lisp` | Low |
| T392 | FR-479 | `src/vm/io.lisp`, `src/compile/builtin-registry.lisp` | Medium |
| T393 | FR-480 | `src/expand/macros-stdlib.lisp` | Low |
| T394 | FR-481 | `src/compile/builtin-registry.lisp` | Low |

---

## 制約事項

- TCOは別sessionで実装中のため本計画から除外
- `register-method` との連携によるIC無効化戦略は実装時に詳細確認が必要
- Float unboxingの呼び出し規約変更範囲は実装時に再評価
- Phase 1は独立して着手可能

---

## 参照

- `src/optimize/optimizer.lisp` — 既存最適化パイプライン (`optimize-instructions`)
- `src/parse/prolog.lisp` — ペアホールエンジン (`*peephole-rules*`)
- `src/optimize/cfg.lisp` — CFG / 支配木 / `loop-depth` フィールド
- `src/type/inference.lisp` — HM型推論 (`infer-type`, `extract-type-guard`)
- `src/compile/closure.lisp` — 自由変数解析 (`find-free-variables`)
- `src/vm/vm.lisp:589-601` — ジェネリック関数ディスパッチ
- `src/runtime/value.lisp` — NaN-boxingスキーム
- `src/optimize/egraph.lisp` — E-graph equality saturation (孤立インフラ)
- `src/optimize/ssa.lisp` — SSA構築/破壊 Cytron法 (孤立インフラ)
- `src/optimize/cfg.lisp:32` — `bb-loop-depth` フィールド (未使用)
- `src/optimize/effects.lisp` — 命令レベル効果分類 (関数レベル拡張対象)
- `src/expand/expander.lisp:96` — `make-macro-expander` (メモ化なし)
- `src/runtime/gc.lisp:158` — free-list定義 (未使用)
- `src/vm/list.lisp` — `vm-cons` → ホストCL `cons` 使用 (managed heap移行対象)
- `src/vm/primitives.lisp:105` — `vm-div` zero-checkのみ (fixnum overflow未検出)
- `src/compile/cps.lisp:145-167` — CPS変換 (admin redex・lambda wrapper残存)
- `src/compile/closure.lisp:48-81` — `find-free-variables` (lambda lifting未実装)
- `src/compile/codegen.lisp:527-531` — 全非ビルトイン呼び出しが`vm-call`間接ディスパッチ
- `src/emit/x86-64-codegen.lisp:100-111` — MOVベースアドレッシングのみ (LEA/SIB未使用)
- `src/emit/x86-64-codegen.lisp:1105-1111` — 常に6レジスタPUSH (callee-save最適化なし)
- `src/expand/macros-stdlib.lisp:352` — `mapcar`展開 (fusion対象)
- `src/compile/codegen.lisp:154-160` — `values`コンパイル (単一値最適化なし)
- `src/compile/codegen-core.lisp:44` — リテラル個別`vm-const`生成 (定数プールなし)
- `src/emit/x86-64.lisp:73-91` — アセンブリテキスト出力 (`.align`ディレクティブなし)
- `src/emit/binary/macho.lisp:428` — PIEフラグ設定済み (PICコード生成未対応)
- `src/emit/binary/macho.lisp:376-396` — `add-symbol`定義済み (シリアライズ未実装)
- `src/runtime/gc.lisp:200-263,331-392` — STW GC (並行マーキング/スイープなし)
- `src/vm/vm.lisp:991-1005` — グローバル変数アクセス (ロックなし)
- `src/emit/target.lisp` — 4ターゲット定義 (x86-64, aarch64, riscv64, wasm32)
- `src/emit/wasm-types.lisp:89-91` — `+wasm-try/catch/throw+`定数 (codegen未接続)
- `tests/framework/framework-fuzz.lisp` — 文法ベースfuzzing (カバレッジフィードバックなし)
- `src/vm/vm-numeric.lisp:662-669` — `vm-complex`命令 (ホストCL `complex`デリゲート、アンボクシングなし)
- `src/vm/vm-clos.lisp:165-183` — CLOSインスタンス=ハッシュテーブル (hidden class/shape未実装)
- `src/parse/cst.lisp:182` — `symbol-macrolet`パース済み (コンパイル未実装)
- `src/expand/expander-data.lisp:38` — `symbol-macrolet`特殊形式リスト (展開ロジック未実装)
- `src/vm/vm.lisp:751-769` — `call-next-method`実装済み (method combination未実装)
- `src/emit/mir.lisp:13,158` — `:safepoint`キーワード定義済み (スタックマップ未構築)
- `src/type/inference.lisp:231-257` — `extract-type-guard` (型推論層のみ、codegen未接続)
- `src/vm/strings.lisp:156-162` — `vm-string-concat` (バイナリ結合のみ、バッチ結合なし)
- `src/expand/macros-basic.lisp:271-291` — `typecase`展開 (線形if-else、判定木/ジャンプテーブルなし)
- `src/parse/ast.lisp:14-16` — `source-file/source-line/source-column` (存在するが未伝播)
- `src/parse/incremental.lisp:1-176` — インクリメンタルパーサ (完全実装済みだがパイプライン未接続)
- `src/parse/incremental.lisp:132-150` — `*parse-cache*`, `cache-lookup/cache-store` (未呼び出し)
- `src/vm/vm.lisp:823-825` — `&rest`パラメータのヒープリスト割り当て (スタック割り当て未対応)
- `src/vm/vm.lisp:829-832` — `&key`引数の線形探索 O(N²) (ハッシュディスパッチなし)
- `src/vm/list.lisp:458-777` — 配列操作 (要素型特殊化なし)
- `src/optimize/egraph.lisp:42-49` — コンパイル時ハッシュコンシング (ランタイム版なし)
- `src/emit/mir.lisp:9` — CFG-based SSA選択 (Sea-of-Nodes明示的却下、2024年V8も廃止)
- `src/emit/binary/elf.lisp` — .text/.symtab等のみ (.eh_frame/unwindテーブルなし)
- `src/vm/vm.lisp:342-372` — `vm-state`シングルトン (軽量スレッド/M:N threading未対応)
- `src/runtime/gc.lisp:269-295` — SATB書き込みバリア実装済み (読み取りバリアなし)
- `src/optimize/ssa.lisp:68-98` — `ssa-place-phis` (活性チェックなしPhi挿入、trivial phi除去なし)
- `src/vm/vm-numeric.lisp:566-621` — 有理数演算 (ホストCLデリゲート、特殊化なし)
- `src/runtime/value.lisp:62-63` — 48-bitポインタ (ポインタ圧縮なし)
- `src/optimize/optimizer.lisp:1018-1042` — インライン閾値/反復回数ハードコード (最適化レベルなし)
- `src/optimize/optimizer.lisp:1020-1031` — `*opt-convergence-passes*`固定順序 (パス依存メタデータなし)
- `src/compile/ir/block.lisp:129-148` — `ir-verify-ssa`のみ (包括的IR検証なし)
- `src/compile/ir/printer.lisp` — IRプリンタ実装済み (CLI統合なし)
- `src/optimize/egraph.lisp:299-317` — E-graph fuelパラメータ (メインオプティマイザにfuelなし)
- `src/optimize/optimizer.lisp:847-898` — 2のべき乗除算のみ最適化 (任意定数Granlund-Montgomeryなし)
- `src/emit/x86-64-codegen.lisp` — IMUL単一レジスタのみ (MUL RDX:RAX 128bit未対応)
- `src/emit/x86-64-codegen.lisp` — SHL/SHR/SARのみ (ROL/ROR回転命令なし)
- `src/emit/x86-64-codegen.lisp` — BSWAP命令未エミッション
- `src/vm/vm-numeric.lisp:148-264` — vm-sqrt/sin/cos/exp/log定義済み (ネイティブエミッタ未接続)
- `src/emit/binary/elf.lisp:23` — `+elf-type-rel+`のみ (ET_EXEC/ET_DYN未対応)
- `src/emit/binary/macho.lisp:376-396` — `add-symbol`定義済み (再配置エントリ未生成)
- `src/emit/binary/macho.lisp:428` — PIEフラグ済み (LC_DYLD_INFO/LC_CODE_SIGNATURE なし)
- `src/emit/x86-64.lisp:39` — レジスタ不足時エラー (AArch64: `aarch64.lisp:12`も同様)
- `src/emit/x86-64.lisp:84-86` — vm-print `(error "not implemented yet")`
- `src/emit/aarch64.lisp:54-56` — vm-print `(error "not implemented yet")`
- `src/emit/wasm.lisp:309-337` — set-global/get-global/call/closureが全てスタブ
- `src/emit/target.lisp:150-165` — RISC-Vターゲット記述子のみ (codegen未実装)
- `src/emit/mir.lisp` + `src/emit/target.lisp:8-10` — MIRフレームワーク完成 (iselルールファイル全ターゲットで未存在)
- `src/runtime/runtime.lisp:462` — `rt-register-method`スタブ (ignore+nil)
- `src/runtime/runtime.lisp:481-490` — `rt-establish/remove/push/pop-handler`全てno-op
- `src/optimize/optimizer.lisp:869-897` — strength-reduceは`vm-mul`のみ (`vm-mod`/`vm-rem`分岐なし)
- `src/optimize/optimizer.lisp:265` — 代数恒等式に`(mod 0 x)->0`のみ
- `src/emit/x86-64-codegen.lisp:255-262` — `emit-vm-add`はMOV+ADDのみ (JOフラグチェックなし)
- `src/optimize/optimizer.lisp:773` — インライン閾値=生命令数15 (コストモデルなし)
- `src/vm/vm-run.lisp:124,265` — `run-compiled`/`run-vm`タイトループ (デバッグフックなし)
- `src/vm/vm-run.lisp:301-310` — `vm-reg-get`/`vm-reg-set`プレーンR/W (トレースフックなし)
- `src/vm/vm.lisp:345` — `vm-call-stack`生リスト (フォーマッタなし)
- `src/parse/diagnostics.lisp:7-14` — diagnostic構造体 (error-code/fix-itフィールドなし)
- `src/parse/diagnostics.lisp:85` — `make-parse-warning`はパーサのみ (codegenから未使用)
- `src/cli/main.lisp:256-302` — ベア`read-line` REPL (履歴/補完なし)
- `src/cli/main.lisp:480-497` — CLIサブコマンド (lsp/lint/format未実装)
- `cl-cc.asd:16` — `:serial t`でモジュール直列コンパイル (並列化余地あり)
- `src/vm/vm.lisp:777-794` — `vm-save-registers`全HT毎回コピー (活性フィルタなし)
- `src/vm/vm.lisp:625` — `(mapcar ... arg-regs)`で呼び出し毎にフレッシュリスト割り当て
- `src/emit/x86-64-codegen.lisp:909-911` — vm-closure/vm-call命令サイズ=0 (ネイティブCALL/RET未エミッション)
- `src/compile/codegen.lisp:333-339` — flet各クロージャが独立にcaptured-vals保持 (兄弟間共有なし)
- `src/runtime/gc.lisp:78-82` — 旧空間昇格がold-freeバンプポインタのみ (free-list無視)
- `src/runtime/gc.lisp:50-54` — `rt-gc-add-root`型メタデータなし (ポインタ/非ポインタ未区別)
- `src/runtime/heap.lisp:30-34` — ナーサリ/旧世代サイズハードコード (適応リサイズなし)
- `src/runtime/gc.lisp:291-293` — Young→YoungストアでもSATB/カードマーキングを実行
- `src/vm/strings.lisp:435-441` — `vm-concatenate`毎回新規割り当て (ランタイムインターニングなし)
- `src/vm/hash.lisp:132-140` — `vm-make-hash-table`は`:test`のみ (`:size`/`:rehash-*`なし)
- `src/optimize/optimizer.lisp:83-104` — fold/type-predテーブルに文字列/リスト操作なし
- `src/expand/macros-stdlib.lisp:352-487` — mapcar/every/some/find等が`dolist`専用 (ベクタ非対応)
- `src/expand/macros-stdlib.lisp:509-552` — remove-duplicates/union/intersection等がO(n²)
- `src/vm/vm.lisp:343-371` — 全レジストリが非同期`make-hash-table` (アトミック操作なし)
- `src/runtime/runtime.lisp:469-498` — CLレベルconditionのみ (OSシグナルマッピングなし)
- `src/vm/vm.lisp:988` — `vm-register-function`はベア`setf gethash` (バージョニングなし)
- `src/expand/macros-stdlib.lisp:111-120` — ecase/etypecaseのみ (ccase/ctypecaseなし)
- `src/expand/expander.lisp:410-432` — setfハンドラがハードコード (defsetf/define-setf-expanderなし)
- `src/expand/macros-stdlib.lisp:212-221` — handler-bindがhandler-caseに展開 (巻き戻しセマンティクス)
- `src/expand/macros-stdlib.lisp:306-309` — `with-package-iterator`スタブ (常に枯渇)
- `src/parse/cl/lexer.lisp` — リーダーマクロがハードコード (readtable APIなし)
- `src/runtime/value.lisp:109-166` — 各型述語が独立ビットマスキング (タグチェック融合なし)
- `src/compile/cps.lisp:106-107` — `ast-int`/`ast-var`が一律CPS化 (直線コードでも継続ラムダ生成)
- `src/compile/cps.lisp:124-130` — `ast-if`で継続`k`テキスト複製 (コードサイズ爆発リスク)
- `src/compile/cps.lisp:129` — CPS条件判定`(zerop ,v)` (CL偽は`nil`のみ)
- `src/compile/cps.lisp:182-197` — `ast-block`/`ast-return-from`がホスト`block`/`return-from`出力
- `src/compile/cps.lisp:216-229` — `ast-tagbody`/`ast-go`がホスト`tagbody`/`go`出力
- `src/compile/cps.lisp:233-279` — `ast-catch`/`ast-throw`/`ast-unwind-protect`がホスト特殊形式出力
- `src/parse/cl/parser.lisp:136` — `:allocation`を黙って破棄 (共有スロット未対応)
- `src/parse/cl/parser.lisp:512-522` — defclassクラスオプション完全無視
- `src/parse/cl/parser.lisp:548` — `(declare (ignore qualifier))` (メソッド修飾子破棄)
- `src/vm/vm-clos.lisp:119-133` — CPL計算が深さ優先走査 (C3線形化ではない)
- `src/vm/vm-clos.lisp:165-183` — `vm-make-obj`が`initialize-instance` GF未呼び出し
- `src/vm/vm-clos.lisp:185-193` — 未束縛スロットで生`error` (`slot-unbound` GF未呼び出し)
- `src/vm/vm.lisp:854-865` — `vm-classify-arg`が4型のみ (float/ratio/complex/character等なし)
- `src/vm/primitives.lisp:40-52` — `floatp`/`rationalp`/`complexp`/`realp` VM命令なし
- `src/vm/io.lisp:699-705` — `vm-format-inst`がホストCL `format`に100%委譲
- `src/vm/io.lisp:594-610` — `vm-princ`/`vm-prin1`にCLOSインスタンス`print-object`ディスパッチなし
- `src/compile/context.lisp:8` — `env`がフラットalist (変数種別・宣言情報なし)
- `src/expand/macros-sequence.lisp:335-337` — `locally`が全declare除去
- `src/expand/macros-sequence.lisp:412` — `load-time-value`が`(eval form)` (ホスト委譲)
- `src/compile/codegen-core.lisp:177` — `block`ボディが無条件に末尾位置クリア
- `src/compile/codegen-core.lisp:281-283` — `ast-the`が末尾位置非伝播
- `src/expand/macros-stdlib.lisp:104-108` — `nth-value`が`multiple-value-list`でフルリスト割り当て
- `src/emit/x86-64-codegen.lisp:177,181` — 全ジャンプが固定`rel32` (短分岐未使用)
- `src/emit/regalloc.lisp:358-364` — 全スピルvregが単一scratch共有 (clobberリスク)
- `src/emit/regalloc.lisp:244-273` — 活性区間計算が後方ジャンプのみ延長
- `src/optimize/optimizer.lisp:197-199` — copy-prop killがmaphash全走査O(n×m)
- `src/optimize/optimizer.lisp:86,203,601` — vm-labelで全最適化状態clrhash
- `src/optimize/optimizer.lisp:229-246` — sexp往復で命令レジスタ書き換え
- `src/optimize/optimizer.lisp:504-576` — inline再帰/相互再帰ガードなし
- `src/optimize/optimizer.lisp:658` — CSEキーが`format ~S`でフレッシュ文字列生成
- `src/optimize/optimizer.lisp:765` — inline収束ループ外で1回のみ実行
- `src/expand/macros-stdlib.lisp:204-209` — `define-condition`が`:report`オプション無視 (`(declare (ignore options))`)
- `src/compile/builtin-registry.lisp:362-365` — `error`/`warn`のみ登録 (`signal`/`cerror`なし)
- `src/expand/macros-stdlib.lisp:224-234` — `restart-case`が全リスタート名・ラムダリスト無視
- `src/expand/macros-stdlib.lisp:246-255` — `invoke-restart`常にerror、`find-restart`/`compute-restarts`常にnil
- `src/vm/conditions.lisp:17-67` — VM固有5型のみ (ANSI標準コンディション型階層欠落)
- `src/expand/macros-stdlib.lisp:272-274` — `muffle-warning`がnil返却のみ
- `src/expand/macros-basic.lisp:68-72` — `check-type`が生error (`store-value`リスタートなし)
- `src/expand/macro.lisp` — `macro-function`/`(setf macro-function)` API不在
- `src/expand/expander.lisp:400-403` — `deftype`が2引数形式のみ (lambda-listなし)
- `src/expand/macros-sequence.lisp:288-294` — `defpackage`スタブ/`export` no-op
- `src/expand/macros-stdlib.lisp:597-627` — `sort`/`stable-sort`がリスト専用 (`:key`なし、ベクタ非対応)
- `src/compile/codegen-phase2.lisp:86-90` — `make-array`がsizeのみコンパイル (キーワード引数破棄)
- `src/expand/macros-sequence.lisp:7-8` — `copy-seq`が`copy-list`エイリアス
- `src/expand/macros-sequence.lisp:11-47` — `fill`/`replace`が`:start`/`:end`無視
- `src/expand/expander-defstruct.lisp:66-123` — defstructに`:copier`/`:print-object`/`:type`/`:read-only`なし
- `src/expand/macros-sequence.lisp,macros-stdlib.lisp` — シーケンス関数が`:key`/`:test`キーワード黙殺
- `src/vm/vm-run.lisp:131-138` — 毎命令CLOSジェネリック関数ディスパッチ
- `src/vm/vm-run.lisp:133` — `(nth pc instructions)`でO(n)命令フェッチ
- `src/vm/vm.lisp:533-535` — `vm-falsep`が`nil`と`0`を両方false扱い
- `src/vm/vm-run.lisp:36-48` — handler設置毎にcall-stack全コピー
- `src/vm/vm-run.lisp:102-109` — ラベルテーブルが文字列キー`#'equal`
- `src/vm/vm.lisp:688-693` — `vm-closure-ref-idx`が`(nth idx)`でO(n)
- `src/vm/vm.lisp:965-968` — `vm-apply`が`butlast+append`で2回走査
- `src/emit/binary/macho.lisp:406-464` — `__PAGEZERO`セグメント未出力
- `src/emit/binary/macho.lisp:376-396` — nlistシリアライズ未呼び出し (dead code)
- `src/emit/binary/macho.lisp:369-370` — `__DATA` maxprot=7 (W^X違反)
- `src/emit/binary/elf.lisp:24` — x86-64ハードコード (AArch64 ELF未対応)
- `src/emit/binary/elf.lisp:284` — セクションアライメントなし
- `src/emit/calling-convention.lisp:8-14` — GPRのみ (FPレジスタなし)
- `src/emit/wasm.lisp:74-75` — `wasm-buf-write-f64`がSBCL専用
- `src/compile/builtin-registry.lisp:193-194` — `prin1-to-string`/`princ-to-string`が同一命令にエイリアス
