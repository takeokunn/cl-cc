# Runtime: Core & Data Structures

Lisp runtime optimization, string/symbol/exception optimization, runtime infrastructure, data structure/sequence optimization, runtime extensions, collection optimization.

---

# Runtime & Data Structures

Runtime system, data structure operations, string/symbol handling, and sequence operations.

---

### Phase 9 — Lisp固有ランタイム最適化

#### FR-042: Dynamic Variable Access Caching

- **対象**: `packages/engine/vm/src/vm.lisp:998-1007`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(defvar *x* ...)` へのアクセスが毎回 `(gethash name (vm-global-vars state))` を実行している
  - 関数エントリ時にグローバルをレジスタにロード、変更した場合のみ書き戻す
  - 特にループ内での `*x*` アクセスで3-5倍の高速化見込み
- **難易度**: Medium

#### FR-043: Closure Environment Flattening

- **対象**: `packages/engine/vm/src/vm.lisp:19-31, 550-572`
- **内容**:
  - 現状: captured-values を alist `((reg . val) ...)` で格納、復元時に `dolist` で走査
  - 平坦ベクタ化: `captured-regs` = vector + `captured-vals` = vector に変換
  - インデックスアクセスで car/cdr 操作を除去、キャッシュ局所性を改善
- **難易度**: Medium

#### FR-044: Apply Fast Path (既知引数数)

- **対象**: `packages/engine/vm/src/vm.lisp:852-876`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(apply fn a b (list c d))` パターンを検出し引数リスト構築を除去
  - コンパイル時に引数数が判明している `apply` を `vm-call` と同等に変換
  - 現状: 常に `butlast`/`append` でリスト操作が発生
- **難易度**: Low

#### FR-045: Tail Recursion Modulo Cons (TRMC)

- **対象**: `packages/engine/compile/src/codegen.lisp` + `packages/engine/compile/src/cps.lisp`
- **内容**:
  - `(cons x (f y))` が末尾位置にある場合、アキュムレータ変換で反復に変換
  - `map`/`filter` 相当の再帰リスト構築がO(n)スタック消費しなくなる
  - GCC の `-foptimize-sibling-calls` の Lisp版
- **難易度**: Hard

#### FR-046: Condition System Zero-Cost Fast Path

- **対象**: `packages/engine/compile/src/codegen.lisp:100-146`, `packages/engine/vm/src/conditions.lisp`
- **内容**:
  - 保護フォーム内に `signal` がないことをコンパイル時に証明できる場合、ハンドラ確立命令を省略
  - 現状: `handler-bind` は常に `vm-establish-handler` を emit
  - 静的解析: 保護フォームのcallee一覧にsignal系がなければ省略可
- **難易度**: Medium

#### FR-047: Format String Compile-Time Processing

- **対象**: `packages/engine/vm/src/io.lisp:247-251`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - 定数フォーマット文字列 `"~A~%"` をコンパイル時に解析してバイトコードへ変換
  - 実行時の `format` 文字列パースを除去
  - コンパイラ自身のデバッグ出力等で大きな恩恵
- **難易度**: Low

#### FR-048: Hash Table Operation Specialization

- **対象**: `packages/engine/vm/src/hash.lisp:116-140`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `(make-hash-table :test 'eql)` 等でテスト関数をコンパイル時に確定
  - `vm-gethash-eq` / `vm-gethash-eql` / `vm-gethash-equal` の特化命令を生成
  - 現状: `resolve-hash-test` が毎回 case ディスパッチを実行
- **難易度**: Low

#### FR-049: Equality Predicate Specialization

- **対象**: `packages/engine/vm/src/primitives.lisp:11-17`, `packages/engine/compile/src/codegen.lisp`
- **内容**:
  - `eq`/`eql`/`equal` を型情報に基づいて特化
  - fixnum同士: 直接整数比較命令
  - symbol同士: ポインタ比較
  - 型不明時のみ汎用パス
- **難易度**: Medium

---

### Phase 22 — 文字列・シンボル・例外最適化

#### FR-136: Character Class Lookup Table

- **対象**: `packages/frontend/parse/src/cl/lexer.lisp`, `packages/engine/vm/src/strings.lisp`
- **内容**: `vm-alpha-char-p`等をインデックスロード1回で結果を返す256バイト配列ルックアップに変換
- **根拠**: レクサが内ループでこれらの述語を多用
- **難易度**: Medium

#### FR-137: String Literal Pool

- **対象**: `packages/engine/compile/src/codegen-core.lisp:270-273`
- **内容**: コンパイル時の`*string-literal-pool*`で同一文字列リテラルを単一`vm-const`に重複排除
- **根拠**: `ast-quote`ハンドラが同一文字列でも毎回別々のvm-constを発行
- **難易度**: Medium

#### FR-138: Zero-Cost Exception Table

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/engine/compile/src/codegen.lisp:100-146`
- **内容**: `vm-establish-handler`/`vm-remove-handler`をホットパスから除去してPC→ハンドラのサイドテーブルに変換
- **根拠**: 現状は保護フォームの全入退ごとにスタックpush/popが発生 (`conditions.lisp:69-141`)
- **難易度**: Hard

#### FR-139: Handler Elision for Pure Bodies

- **対象**: `packages/engine/compile/src/codegen.lisp:100-146`
- **内容**: 保護フォームの命令が全て`opt-inst-pure-p`=trueなら`vm-establish-handler`を省略
- **根拠**: `effects.lisp`に100+型の純粋性テーブルが存在、自然な拡張
- **難易度**: Medium

#### FR-140: Symbol Immediate Encoding

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/value.lisp`
- **内容**: `:key`/`nil`/`t`/`quote`等の頻出シンボルをヒープポインタでなく即値インデックスとしてエンコード
- **根拠**: `nil`/`t`は既に即値実装済み(`+val-nil+`, `+val-t+`)。パーサ内ループの`vm-intern-symbol`コストを削減
- **難易度**: Hard

#### FR-141: Self-Hosting Profile Feedback

- **対象**: `packages/cli/src/main.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **内容**: `./cl-cc selfhost`実行時にVM命令頻度ヒストグラムを収集し、次回コンパイルの最適化優先度決定に活用
- **根拠**: cl-cc固有の最適化機会 — selfhostが現実的なワークロードプロファイルを提供
- **難易度**: Medium

---

### Phase 27 — ランタイム最適化基盤

#### FR-154: Tiered Compilation基盤

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 単一最適化パスのみ（フル最適化）。高頻度呼び出し前の起動コストが大きい
- **内容**: Tier-0（最適化なし、高速コンパイル）とTier-1（フル最適化）の2段構成。初回呼び出しはTier-0で実行し、呼び出しカウンタが閾値超過でTier-1にエスカレート
- **根拠**: V8・HotSpot・PyPy等の現代JITはすべてtiered。単一最適化パスは起動遅延を引き起こす
- **難易度**: Very Hard

#### FR-155: Deoptimization / On-Stack Replacement (OSR) 基盤

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 型前提条件を取り崩せない（一度最適化したら型変更に対応不可）。OSRエントリポイントなし
- **内容**: 最適化コード中に型チェックガードと deoptimization チェックポイントを埋め込む。ガード失敗時にインタープリタへフォールバック。ループ内からの OSR エントリ (ループバックエッジにエントリポイント付与)
- **根拠**: FR-154のTiered実行を安全に行うための必須インフラ
- **難易度**: Very Hard

#### FR-156: Size-Class Segregated Allocator (旧世代)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: major GC sweepでfree-listを構築（`gc.lisp:158,326`）するが**割り当て時に再利用しない**。旧世代はbump-pointerのみ
- **内容**: free-listをサイズクラス（8, 16, 32, 64, 128, 256, 512, 1024 bytes）にバケット分けし、`rt-gc-alloc`で適切バケットからO(1)再利用
- **根拠**: `gc.lisp:158` — free-listは定義済みだが`rt-gc-alloc`から参照されていない
- **難易度**: Medium

#### FR-157: Managed Cons Cell Allocation (ホストCL依存解消)

- **対象**: `packages/engine/vm/src/list.lisp`, `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/gc.lisp`
- **現状**: `vm-cons`命令（`list.lisp`）がホストCL の `cons` を呼んでいる。マネージドヒープを使わずホストGCに依存
- **内容**: `vm-cons`をマネージドヒープ(`rt-gc-alloc`)から2ワードのconsセルを割り当てるように変更。NaN-boxingのpointerタグを付与してVM内で一貫管理
- **根拠**: `packages/engine/vm/src/list.lisp` — `(cons car-val cdr-val)` がホストCL呼び出し。GC境界が2つ存在する
- **難易度**: Hard

#### FR-158: コールグラフベースインライン展開

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/cli/src/main.lisp`
- **現状**: `opt-pass-inline`（`optimizer.lisp:729`）は単一命令列内の局所的定義のみ参照。モジュール境界を越えたインライン不可
- **内容**: FR-102のLTOコールグラフを活用してモジュール境界を越えた小関数（≤30命令、非再帰）をインライン展開。呼び出しグラフの葉から順にボトムアップ処理
- **根拠**: `optimizer.lisp:781` — インライン対象が同一命令列内の`vm-closure`定義のみに限定。外部defunは不可
- **難易度**: Hard

---

### Phase 32 — データ構造・シーケンス最適化

#### FR-179: Sequence Operation Fusion (map+filter → single loop)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: `mapcar`（`macros-stdlib.lisp:352`）は`dolist+cons+nreverse`に展開。`map`（line 632）は`(coerce (mapcar fn (coerce seq 'list)) result-type)` で3重走査
- **内容**: 連鎖するシーケンス操作（mapcar→remove-if→mapcar等）をマクロ展開時または最適化パスで単一ループに融合。中間リスト割り当てを除去
- **根拠**: GHCのstream fusion / Rustのiterator fusion。cl-ccでは`(mapcar f (mapcar g xs))`が2回走査
- **難易度**: Hard

#### FR-180: Single-Value Optimization

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: `(values x)`が`vm-values`命令を生成（`codegen.lisp:154-160`）し、`vm-values-list`にリスト割り当て。単一値でもリスト構築
- **内容**: コンパイル時に`values`の引数が1個の場合、`vm-values`を省略して直接レジスタ移動に変換。`multiple-value-bind`で1値のみ使用する場合も同様に簡素化
- **根拠**: `vm.lisp:267` — `vm-values`は常にリスト構築。単一値はCL仕様上bare valueと同値
- **難易度**: Easy

#### FR-181: Constant Pool / Literal Deduplication

- **対象**: `packages/engine/compile/src/codegen-core.lisp`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: 各リテラルが個別の`vm-const`命令として生成（`codegen-core.lisp:44`）。同一値の重複排除なし
- **内容**: コンパイル単位ごとの定数プールを構築し、同一値のリテラル（整数、浮動小数点、文字列、シンボル）を共有。FR-137の文字列プールを汎化
- **根拠**: JVM/CLR/Python VMはすべて定数プールを持つ。cl-ccでは`42`が10箇所で使われると10個の`vm-const`が生成される
- **難易度**: Easy

---

### Phase 33 — 解析基盤・ランタイム拡張

#### FR-182: Demand Analysis / Strictness Analysis

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/foundation/type/src/inference.lisp`
- **現状**: 値の使用パターン解析なし。すべての式が即時評価される前提
- **内容**: 関数の各引数について「必ず使用される(strict)」「条件付き使用(lazy)」「未使用(absent)」を判定。strictな引数はunbox可能、absentな引数は計算省略可能
- **根拠**: GHCのdemand analyzer。CPS形式のcl-ccでは特に継続の使用パターン解析が効果的
- **難易度**: Hard

#### FR-183: Known Function Property Database

- **対象**: `packages/engine/compile/src/builtin-registry.lisp`, `packages/engine/optimize/src/effects.lisp`
- **現状**: `builtin-registry.lisp`は189個のビルトインの呼び出し規約のみ管理。副作用分類・戻り値型・引数制約は未記録
- **内容**: 各ビルトイン関数に属性を付与: `:pure`（副作用なし）、`:foldable`（定数畳み込み可能）、`:nonneg-result`（`length`は≥0）、`:always-returns`（`car`は常に返る）、`:no-escape`（引数が脱出しない）
- **根拠**: LLVMの`FunctionAttrs`パス、SBCLの`fun-info`。FR-152の推移的純粋性推論の基盤データ
- **難易度**: Medium

#### FR-184: Weak Reference / Finalization Support

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: weak pointerおよびfinalizationの実装なし。GCは強参照のみ追跡
- **内容**: weak pointer型（GCルートから除外）、weak hash table（キー/値のweak参照選択）、finalization queue（GC回収前のクリーンアップコールバック）を追加
- **根拠**: ANSI CL仕様外だがSBCL/CCL/LispWorksすべてが提供。自己完結ランタイムに必須
- **難易度**: Hard

#### FR-185: Optimization Reports

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/cli/src/main.lisp`
- **現状**: 最適化は完全にサイレント。どの関数がインライン化されたか、どの定数が畳み込まれたかの情報出力なし
- **内容**: `--optimization-report`フラグで最適化判断をレポート: インライン展開（関数名・命令数・判断理由）、定数畳み込み（元の式・結果）、DCE除去数、CSE統合数
- **根拠**: LLVMの`-Rpass=.*`、GCCの`-fopt-info`。コンパイラ開発・デバッグに不可欠
- **難易度**: Easy

---

### Phase 58 — データ表現最適化

#### FR-264: Pointer Compression (ポインタ圧縮)

- **対象**: `packages/backend/runtime/src/value.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: NaN-boxing（`value.lisp`）は48-bitポインタを使用。64-bitシステムでオブジェクト参照がフルサイズ
- **内容**: ヒープを4GBリージョンに制限し、ポインタを32-bitオフセットとして格納。ロード時にベースアドレス加算で復元。メモリ使用量を約40%削減。NaN-boxingのポインタフィールドを32-bit化
- **根拠**: V8 CompressedPointers / HotSpot CompressedOops (-XX:+UseCompressedOops)。メモリ効率の基本最適化
- **難易度**: Very Hard

#### FR-265: Small String Optimization (SSO / 短文字列最適化)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/runtime/src/value.lisp`
- **現状**: 全文字列がヒープ割り当て。1文字の文字列でもオブジェクトヘッダ＋ポインタ＋長さ＋バッファを消費
- **内容**: 7バイト以下の文字列をNaN-boxingのペイロード内にインライン格納（ヒープ割り当て回避）。タグビットで短文字列/ヒープ文字列を区別。シンボル名・キーワード等の短文字列が大幅に高速化
- **根拠**: C++ std::string SSO / Swift String inline storage。文字列操作のGC圧力を大幅削減
- **難易度**: Hard

#### FR-266: Object Header Compression (オブジェクトヘッダ圧縮)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: CLOSインスタンスはハッシュテーブルベース（FR-214で改善予定）。オブジェクトメタデータ（型タグ、GCビット、サイズ）が分散
- **内容**: オブジェクトヘッダを1ワードに圧縮: `[type-tag(8bit) | gc-bits(4bit) | shape-id(20bit) | size(32bit)]`。FR-214（Object Shape）のshape IDをヘッダに埋め込み、クラスポインタ不要に。GCスキャン高速化
- **根拠**: HotSpot mark word / V8 Map pointer / GraalVM hub pointer。オブジェクトあたり8-16バイト節約
- **難易度**: Hard

---

### Phase 66 — ランタイム補完

#### FR-300: Runtime Condition/Restart Handler (ランタイムcondition/restartハンドラ)

- **対象**: `packages/backend/runtime/src/runtime.lisp`
- **現状**: `runtime.lisp:462` — `rt-register-method`は`(declare (ignore ...)) nil`のスタブ。`runtime.lisp:481-490` — `rt-establish-handler`/`rt-remove-handler`/`rt-push-handler`/`rt-pop-handler`が全て`nil`を返すno-op。condition/restartシステムのランタイムサポートが完全に未実装
- **内容**: (1) ハンドラスタック: `rt-push-handler`でcondition型→ハンドラ関数のバインディングをスタックにpush。(2) `rt-establish-handler`でハンドラフレームを設置（`setjmp`相当のセーブポイント）。(3) `signal`時にハンドラスタックを走査しマッチするハンドラを呼び出し。(4) `restart-case`/`restart-bind`のリスタート登録・呼び出し
- **根拠**: ANSI CL 9.1 Condition System。CL標準の必須機能
- **難易度**: Hard

#### FR-301: rt-register-method Implementation (rt-register-methodの実装)

- **対象**: `packages/backend/runtime/src/runtime.lisp`
- **現状**: `runtime.lisp:462` — `(defun rt-register-method (gf specs method) (declare (ignore gf specs method)) nil)` — 引数を全て無視してnilを返す。ネイティブコンパイルされたCLOSメソッドの登録が不可能
- **内容**: ジェネリック関数のディスパッチテーブルにメソッドを登録。specializer（型）リストをキーとしてメソッドクロージャをハッシュテーブルに格納。`compute-applicable-methods`の実装。メソッドコンビネーション（`:before`/`:after`/`:around`）の呼び出し連鎖構築
- **根拠**: ANSI CL 7.6 Generic Functions。CLOS標準メソッドディスパッチ
- **難易度**: Hard

---

### Phase 71 — コレクション・文字列最適化

#### FR-338: Runtime String Interning (ランタイム文字列インターニング)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: `strings.lisp:435-441` — `vm-concatenate`は毎回新規文字列割り当て。`strings.lisp:517` — `vm-symbol-name`はホスト`symbol-name`にデリゲートしキャッシュなし。FR-137（定数プール）・FR-181（リテラルプール）はコンパイル時のみ
- **内容**: ランタイム文字列インターンテーブル。`symbol-name`結果・頻出文字列を重複排除。V8の内部文字列化に類似。GC統合（弱参照テーブル）
- **根拠**: V8 internalized strings / Java string interning。文字列メモリ使用量削減
- **難易度**: Medium

#### FR-339: Hash Table Size/Rehash Control (ハッシュテーブルサイズ/再ハッシュ制御)

- **対象**: `packages/engine/vm/src/hash.lisp`
- **現状**: `vm-make-hash-table`（`hash.lisp:132-140`）は`:test`のみ受理。`:size`/`:rehash-size`/`:rehash-threshold`未サポート。ANSI CL必須のアクセサ（`hash-table-size`等）もなし
- **内容**: `:size`ヒントを`make-hash-table`に伝達。`hash-table-size`/`hash-table-rehash-size`/`hash-table-rehash-threshold`/`hash-table-count`アクセサ追加。ANSI CL 18.1準拠
- **根拠**: ANSI CL 18.1必須仕様。大規模テーブルの初期再ハッシュ回避
- **難易度**: Easy

#### FR-340: Compile-Time Sequence/String Folding (コンパイル時シーケンス/文字列畳み込み)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: `*opt-unary-fold-table*`（`optimizer.lisp:83-92`）は6エントリ（全て数値）。`*opt-type-pred-fold-table*`（`optimizer.lisp:94-104`）も6エントリ。`vm-string-length`/`vm-length`/`vm-car`/`vm-cdr`/`vm-stringp`/`vm-listp`のコンパイル時畳み込みなし
- **内容**: `(length "hello")` → `5`、`(string-upcase "foo")` → `"FOO"`、`(char "abc" 1)` → `#\b`、`(car '(1 2 3))` → `1`。定数引数でのシーケンス/文字列述語の静的評価
- **根拠**: GCC/LLVM `strlen`定数畳み込み / SBCL compile-time type folding。定数式の静的評価
- **難易度**: Easy

#### FR-341: HOF Macro Vector Path (高階関数マクロのベクタパス)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: `mapcar`（`macros-stdlib.lisp:352`）・`every`（`382`）・`some`（`392`）・`find`（`431`）・`position`（`463`）・`count-if`（`487`）等が全て`dolist`専用。ベクタ引数での動作不可または非効率
- **内容**: マクロ展開時にtypecase分岐: リスト→`dolist`、ベクタ→`dotimes`+`aref`。ANSI CLシーケンス関数はリスト/ベクタ両対応必須。FR-274（拡張可能シーケンス）のユーザー定義型とは別に、標準型の基本対応
- **根拠**: ANSI CL 17.3 — シーケンス関数はリスト/ベクタ両方で動作必須。SBCL/CCL全実装
- **難易度**: Medium

#### FR-342: Dead Store Elimination for Collections (コレクション操作のデッドストア除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/optimize/src/effects.lisp`
- **現状**: DCE（`optimizer.lisp:519-538`）は`vm-dst`未読の命令のみ除去。`vm-sethash`/`vm-aset`/`vm-rplaca`/`vm-rplacd`はdstなし（副作用専用）。同一キー/インデックスへの冗長書き込み除去不可
- **内容**: ストアターゲット（ハッシュキー・配列インデックス）追跡。後続ストアが同ターゲットを上書きし中間読み取りがなければ先行ストアを除去
- **根拠**: LLVM DeadStoreElimination / HotSpot C2 memory scheduling。冗長書き込み除去
- **難易度**: Hard

#### FR-343: Set Operations Hash Acceleration (集合演算ハッシュ高速化)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: `remove-duplicates`（`macros-stdlib.lisp:509-515`）が`(member x acc)`でO(n²)。`union`（`518-530`）・`intersection`（`543-552`）・`set-difference`（`533-541`）も同様のO(n²)
- **内容**: 閾値長超過時にハッシュテーブルベースのメンバーシップテストに切り替え（全体O(n)）。SBCL内部実装と同等
- **根拠**: SBCL internal hash-based set ops。大リストでの集合演算O(n²)→O(n)
- **難易度**: Easy

#### FR-344: String/List Algebraic Simplification (文字列・リスト述語の代数的簡約)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: `*opt-algebraic-identity-rules*`（`optimizer.lisp:254-282`）は数値/論理/比較のみ。`(null (cons x y))` → `nil`、`(consp nil)` → `nil`、`(listp (cons x y))` → `t`のルールなし
- **内容**: 型述語の生産者命令型に基づく簡約。`vm-cons`の出力は常にcons→下流の`vm-consp`/`vm-listp`/`vm-null-p`を定数に置換。既存定数畳み込みとは異なるメカニズム（型フロー解析）
- **根拠**: LLVM InstCombine type-based simplification。不要な型チェック除去
- **難易度**: Medium

---

### Phase 81 — 配列・シーケンス・構造体完全化

#### FR-440: sort/stable-sort :key Support (sort :keyサポート)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: `sort`が`(list predicate)`のみ受け付け。`:key`引数なし(`macros-stdlib.lisp:597`)
- **内容**: `:key`引数を受け付け、比較時に各要素にkey関数を適用
- **根拠**: ANSI CL 17.3 — sort, stable-sort
- **難易度**: Low

#### FR-441: sort/stable-sort Vector Support (sortベクタ対応)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: `sort`/`stable-sort`(`macros-stdlib.lisp:597-627`)が`car`/`cdr`/`cons`専用。ベクタソート不可
- **内容**: ベクタ入力時にインプレースソート（quicksort/mergesort on vector）
- **根拠**: ANSI CL 17.3 — sort accepts sequence, not just list
- **難易度**: Medium

#### FR-442: make-array Keyword Args Compilation (make-arrayキーワード引数コンパイル)

- **対象**: `packages/engine/compile/src/codegen-phase2.lisp`
- **現状**: `codegen-phase2.lisp:86-90` — `(make-array size)`のみコンパイル。`:initial-element`, `:element-type`, `:displaced-to`, `:initial-contents`がコンパイル時に破棄
- **内容**: make-arrayキーワード引数をコンパイルしてVM命令に渡す。VM側`vm-make-array`は`:initial-element`対応済み
- **根拠**: ANSI CL 15.2 — make-array
- **難易度**: Medium

#### FR-443: make-array Multi-Dimensional (多次元make-array)

- **対象**: `packages/engine/compile/src/codegen-phase2.lisp`, `packages/engine/vm/src/list.lisp`
- **現状**: `codegen-phase2.lisp:87`/`vm-make-array`(`list.lisp:510`)が単一整数サイズのみ受け付け。次元リスト未対応
- **内容**: `(make-array '(3 4))` — 多次元配列生成。`aref`の多次元インデックス計算（row-major-aref変換）
- **根拠**: ANSI CL 15.2 — Multi-dimensional arrays
- **難易度**: Hard

#### FR-444: copy-seq Vector Support (copy-seqベクタ対応)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`
- **現状**: `macros-sequence.lisp:7-8` — `copy-seq`が`copy-list`に展開。ベクタ非対応
- **内容**: 入力型に応じてリストはcopy-list、ベクタはベクタコピー
- **根拠**: ANSI CL 17.3 — copy-seq
- **難易度**: Low

#### FR-445: fill/replace :start/:end Support (fill/replace境界引数)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`
- **現状**: `macros-sequence.lisp:11-47` — `fill`/`replace`が`:start`/`:end`キーワードを黙って無視
- **内容**: `:start`/`:end`キーワードの処理。部分範囲への操作を可能にする
- **根拠**: ANSI CL 17.3 — fill, replace
- **難易度**: Low

#### FR-446: defstruct :copier Option (defstruct :copierオプション)

- **対象**: `packages/frontend/expand/src/expander-defstruct.lisp`
- **現状**: `expander-defstruct.lisp:66-123` — `copy-<name>`関数が未生成。`:copier nil`も未処理
- **内容**: デフォルトで`copy-<name>`関数を生成（構造体の浅いコピー）。`:copier nil`で抑制
- **根拠**: ANSI CL 8.1 — defstruct :copier
- **難易度**: Low

#### FR-447: defstruct :print-function / :print-object (defstruct印刷オプション)

- **対象**: `packages/frontend/expand/src/expander-defstruct.lisp`
- **現状**: `expander-defstruct.lisp:66-123` — `:print-function`/`:print-object`オプションを黙って無視
- **内容**: `:print-object`指定時にprint-objectメソッド生成。`:print-function`は旧互換
- **根拠**: ANSI CL 8.1 — defstruct print options
- **依存**: FR-390
- **難易度**: Medium

#### FR-448: defstruct :type list/vector (defstruct型指定)

- **対象**: `packages/frontend/expand/src/expander-defstruct.lisp`
- **現状**: `expander-defstruct.lisp:66-123` — `:type`オプション未実装
- **内容**: `(:type list)` — 構造体をリスト表現。`(:type vector)` — ベクタ表現。`:named`オプションと組み合わせ
- **根拠**: ANSI CL 8.1 — defstruct :type
- **難易度**: Medium

#### FR-449: defstruct :read-only Slot Option (defstruct :read-onlyスロット)

- **対象**: `packages/frontend/expand/src/expander-defstruct.lisp`
- **現状**: `expander-defstruct.lisp:94-96` — `:read-only`スロットオプション未パース・未強制
- **内容**: `:read-only t`指定時にsetfアクセサを生成しない。コンパイル時にsetf使用を検出してエラー
- **根拠**: ANSI CL 8.1 — defstruct slot options
- **難易度**: Low

#### FR-450: Sequence Function :key/:test/:test-not (シーケンス関数キーワード引数)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: `find`, `count`, `position`, `remove`, `delete`, `substitute`, `mismatch`等が`&rest keys`を受け取るが全て黙って無視
- **内容**: `:key`, `:test`, `:test-not`, `:start`, `:end`, `:from-end`キーワードの実処理
- **根拠**: ANSI CL 17.3 — Sequence Functions
- **難易度**: Hard

#### FR-451: search General Sequence (汎用searchシーケンス検索)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`
- **現状**: `rt-search-string`(`runtime.lisp:352`)のみ。汎用シーケンスのsearch未実装
- **内容**: `(search sequence1 sequence2 &key test key start1 end1 start2 end2)` — 部分シーケンス検索
- **根拠**: ANSI CL 17.3 — search
- **難易度**: Medium

#### FR-452: merge :key Support (merge :keyサポート)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`
- **現状**: `merge`マクロ(`macros-sequence.lisp:245-281`)が`&rest keys`を黙って無視
- **内容**: `:key`引数の処理。マージ比較時にkey関数適用
- **根拠**: ANSI CL 17.3 — merge
- **難易度**: Low

#### FR-453: map-into Multi-Source (map-into複数ソース)

- **対象**: `packages/frontend/expand/src/macros-sequence.lisp`
- **現状**: `map-into`(`macros-sequence.lisp:222-242`)が複数ソースの場合`(progn dest)`(no-op)にフォールスルー
- **内容**: 複数ソースシーケンスから並列にマッピング。最短ソースで停止
- **根拠**: ANSI CL 17.3 — map-into
- **難易度**: Medium

---
