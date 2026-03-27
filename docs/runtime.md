# Runtime & Data Structures

Runtime system, data structure operations, string/symbol handling, and sequence operations.

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

### Phase 90 — インラインキャッシュ・動的ディスパッチ最適化

2026年のモダンJIT（V8 Maglev/Turbofan、GraalVM、HotSpot C2）が実装する動的ディスパッチの最適化インフラ。

#### FR-500: Monomorphic Inline Cache (MIC) for Generic Functions

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **現状**: `vm-generic-call`が毎回ディスパッチテーブルをフルスキャン（`vm-clos.lisp:execute-instruction vm-generic-call`）
- **内容**:
  - 各call siteに「最後に見た型→メソッドクロージャ」のインラインキャッシュを添付
  - 同じ型で呼ばれた場合はディスパッチテーブル検索をスキップ
  - キャッシュミス時のみフォールバック（フルスキャン）
  - 型変化を検出したら Polymorphic IC (FR-501) にエスカレート
- **根拠**: V8 IC / HotSpot inline cache。ジェネリック関数呼び出しコストを1〜2命令に削減
- **難易度**: Hard

#### FR-501: Polymorphic Inline Cache (PIC) for Generic Functions

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **依存**: FR-500
- **現状**: MIC実装後、複数型から呼ばれるcall siteで毎回キャッシュミスが発生する
- **内容**:
  - キャッシュを最大N件（デフォルト4）の`(type . method)`ペア配列に拡張
  - 線形スキャンでO(N)の型マッチ、N超過でメガモーフィック状態に移行
  - PIC配列をVM命令のスロット（`:sexp-slots`）に格納してGC管理
- **根拠**: SpiderMonkey PIC / V8 polymorphic IC。2〜4型のジェネリック関数呼び出しを高速化
- **難易度**: Hard

#### FR-502: Megamorphic Call Site Handling

- **対象**: `src/vm/vm-clos.lisp`
- **依存**: FR-501
- **内容**:
  - PICがN型を超えた場合、グローバルメガモーフィックキャッシュ（全call site共有のハッシュテーブル）に移行
  - メガモーフィック状態は「最適化を諦めて安定した低コスト実装に落とす」戦略
  - インライン化候補から除外（FR-158への入力として利用）
- **根拠**: V8 megamorphic stub / HotSpot MegamorphicCache。多態性の高いcall siteを安定処理
- **難易度**: Medium

#### FR-503: Type Feedback Vector (TFV)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-run.lisp`
- **内容**:
  - 各関数に対してcall site数分のフィードバックスロットを持つベクタを割り当て
  - 実行時に型情報（引数型、分岐方向、ループカウント）をスロットに記録
  - Tier-1コンパイル時（FR-154）にTFVを参照して特化コード生成
- **根拠**: V8 FeedbackVector / HotSpot MethodData。型プロファイリングの中核データ構造
- **難易度**: Hard

#### FR-504: Call Site Speculation (呼び出し先特化)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **依存**: FR-503
- **内容**:
  - TFVから「このcall siteは常に関数Fを呼ぶ」と判明した場合、直接呼び出しにコンパイル
  - ガードチェック（`(eq callee F)`）＋直接call＋ガード失敗時のfallback
  - クロージャ変数を介した高階関数の直接インライン展開が可能になる
- **根拠**: V8 function monomorphism optimization。高階関数の呼び出しオーバーヘッドを除去
- **難易度**: Very Hard

---

### Phase 91 — セーフポイント・スタックマップ

#### FR-510: GC Safepoint Infrastructure (GCセーフポイント基盤)

- **対象**: `src/vm/vm-run.lisp`, `src/runtime/gc.lisp`
- **現状**: GCは任意のタイミングで`rt-gc-collect`を呼び出せる前提。スタック上のVM値がGCルートとして登録されていない
- **内容**:
  - セーフポイント: 関数呼び出し・ループバックエッジ・メモリ割り当て直前を「安全点」とする
  - セーフポイント到達時のみGCが起動（コオペラティブ方式）
  - `*gc-pending*`フラグをセーフポイントチェック命令でポーリング
- **根拠**: HotSpot Safepoint / V8 GC safepoints。正確なGCの必須インフラ
- **難易度**: Hard

#### FR-511: Precise Stack Map (精確スタックマップ)

- **対象**: `src/compile/codegen.lisp`, `src/runtime/gc.lisp`
- **依存**: FR-510
- **現状**: GCはスタックフレームのどのスロットがLispオブジェクトかを知らない（保守的スキャン）
- **内容**:
  - 各セーフポイントでレジスタ・スタックスロット中のオブジェクト参照の位置情報を記録
  - `stackmap: ((frame-offset . :object) ...)` テーブルをコンパイル時に生成
  - GCがスタックマップを参照して精確にオブジェクトを追跡
- **根拠**: JVM StackMapTable / GHC info tables。保守的GCによる偽参照保持を排除
- **難易度**: Very Hard

#### FR-512: Return Address Poisoning (リターンアドレス保護)

- **対象**: `src/vm/vm-execute.lisp`, `src/runtime/runtime.lisp`
- **内容**:
  - GCがフレームをスキャンする際、リターンアドレスとLispオブジェクトポインタを区別
  - リターンアドレスにタグビットを付与（または別フィールドに移動）してGCスキャンから除外
  - セキュリティ上も有益: Shadow Stack（FR-513）との連携
- **根拠**: V8 / HotSpot frame layout。スタックスキャンの正確性向上
- **難易度**: Medium

#### FR-513: Shadow Stack for CLOS Dispatch (CLOSディスパッチ用シャドウスタック)

- **対象**: `src/vm/vm-clos.lisp`, `src/vm/vm-execute.lisp`
- **内容**:
  - CLOS `call-next-method` の連鎖をスタックとは別の「シャドウスタック」で管理
  - メソッドコンビネーション（:before/:after/:around）の呼び出し順を明示的なデータ構造で表現
  - 現状の暗黙的な再帰呼び出しを排除し、デバッグ・プロファイリングを容易にする
- **根拠**: CLOS next-method chain visualization。SBCL PCL実装の参照実装
- **難易度**: Medium

---

### Phase 92 — 数値タワー・Unboxed演算

#### FR-520: Fixnum Unboxed Arithmetic Path

- **対象**: `src/vm/primitives.lisp`, `src/compile/codegen.lisp`
- **現状**: 全算術演算が`vm-value-fixnum-p`チェック後にunboxして計算、再boxして返す
- **内容**:
  - 型推論（FR-004）でfixnumと確定した変数に対してunboxedレジスタを割り当て
  - `vm-add-fixnum`/`vm-mul-fixnum`等の型特化命令を追加
  - box/unboxコストを完全除去。内側ループでの数値演算が2-5倍高速化
- **根拠**: SBCL `(declare (type fixnum n))` unboxed / HotSpot int intrinsics
- **難易度**: Hard

#### FR-521: Float Unboxed Arithmetic (浮動小数点Unboxed演算)

- **対象**: `src/vm/primitives.lisp`, `src/compile/codegen.lisp`
- **依存**: FR-520
- **内容**:
  - NaN-boxingの64-bit doubleペイロードを直接XMMレジスタに保持
  - `vm-fadd`/`vm-fmul`/`vm-fsqrt`等のfloat専用命令
  - float配列に対するループでのスカラー展開
- **根拠**: SBCL `(declare (type double-float x))` / LuaJIT float unboxing
- **難易度**: Hard

#### FR-522: Bignum Fast Path (Bignum高速パス)

- **対象**: `src/vm/primitives.lisp`
- **現状**: bignum演算はホストCL `+`/`*`等に完全委譲
- **内容**:
  - 小bignum（2ワード以内）をfixnum隣接の特化パスで処理
  - Karatsuba乗算（FR-523の基盤）
  - コンパイル時bignum畳み込みをFR-181の定数プールに統合
- **根拠**: GMP / Java BigInteger fast path。数値演算ベンチマークの底上げ
- **難易度**: Hard

#### FR-523: SIMD Intrinsics (SIMDイントリンシクス)

- **対象**: `src/emit/x86-64-codegen.lisp`, `src/vm/vm.lisp`
- **内容**:
  - `(simd:+ vec1 vec2)` / `(simd:dot vec1 vec2)` 等のSIMD組み込み関数
  - SSE2/AVX2命令への直接マッピング (`vmovdqu`, `vaddps`, `vmulps`)
  - `make-array :element-type 'single-float` の配列を SIMD バッファとして扱う
  - 自動ベクタライザ（FR-524）の低レベル基盤
- **根拠**: LLVM vector intrinsics / LuaJIT FFI SIMD。数値計算の10-20倍高速化
- **難易度**: Very Hard

#### FR-524: Auto-Vectorization (自動ベクタライズ)

- **対象**: `src/optimize/optimizer.lisp`, `src/emit/x86-64-codegen.lisp`
- **依存**: FR-521, FR-523
- **内容**:
  - float配列に対する単純ループ（`(dotimes (i n) (aset r i (f+ (aref a i) (aref b i))))`）をSIMDループに変換
  - ループ長が4の倍数でない場合のスカラーエピローグ自動生成
  - エイリアス解析（FR-017）でa/b/rが重ならないことを確認してからベクタライズ
- **根拠**: GCC/LLVM auto-vectorization (-O3 -march=native)
- **難易度**: Very Hard

#### FR-525: Number Tower Rationalization (数値タワー整理)

- **対象**: `src/vm/primitives.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `ratio`/`complex`型はホストCLに完全委譲。VM内での数値タワー一貫処理なし
- **内容**:
  - `ratio`: `(/ 3 4)` → VM内 ratio オブジェクト（分子/分母2ワード）
  - `complex`: `(complex 1.0 2.0)` → VM内 complex オブジェクト
  - `number`型の`+`/`-`/`*`/`/`の型ディスパッチをVMで一元化
- **根拠**: ANSI CL 12.1 Number Tower。cl-ccが完全自立ランタイムになるための必須条件
- **難易度**: Hard

---

### Phase 93 — FFI・ネイティブブリッジ

#### FR-530: C FFI Layer (C言語インターフェース)

- **対象**: `src/runtime/runtime.lisp`, `src/vm/vm.lisp` (新ファイル `src/ffi/cffi.lisp`)
- **現状**: ホストCLの`cffi`に完全依存。ネイティブバイナリビルド後のCライブラリ呼び出し手段なし
- **内容**:
  - `(define-foreign-function "printf" (:pointer :string) :int)` のような宣言マクロ
  - 引数のVM値 → C ABI変換（NaN-unbox → C型）
  - 戻り値のC型 → VM値変換
  - macOS: `dlopen`/`dlsym` 経由。Linux: `ld.so` 経由
- **根拠**: CFFI / SBCL SB-ALIEN。ネイティブバイナリのシステムライブラリ呼び出し必須
- **難易度**: Very Hard

#### FR-531: Callback Support (コールバック登録)

- **対象**: `src/ffi/cffi.lisp` (FR-530)
- **依存**: FR-530
- **内容**:
  - `(make-callback fn :int (:int :int))` — CLクロージャをC関数ポインタとして登録
  - x86-64 ABI準拠のトランポリン生成（`mmap`+`mprotect`で実行可能メモリ確保）
  - GCとの統合: コールバック経由でCLクロージャがGCルートとして保持される
- **根拠**: SBCL SB-ALIEN callback / LuaJIT FFI callback
- **難易度**: Very Hard

#### FR-532: Native Struct Mapping (ネイティブ構造体マッピング)

- **対象**: `src/ffi/cffi.lisp`
- **依存**: FR-530
- **内容**:
  - `(define-foreign-struct stat ...)` — Cの`struct`をVMオブジェクトとしてマッピング
  - フィールドアクセサの自動生成（`stat-st-size`等）
  - アライメント・パディングのABI準拠計算
- **根拠**: CFFI defcstruct / SBCL SB-ALIEN define-alien-type
- **難易度**: Hard

#### FR-533: Inline Assembly (インラインアセンブリ)

- **対象**: `src/emit/x86-64-codegen.lisp`
- **内容**:
  - `(asm :mov :rax 42)` — VM命令シーケンス中にアセンブリ命令を直接埋め込む
  - システムコール発行(`syscall`命令)のためのインターフェース
  - CPUID/RDTSC等の特殊命令へのアクセス
- **根拠**: GCC `__asm__` / SBCL `%primitive`。低レベルランタイム操作の基盤
- **難易度**: Medium

---

### Phase 94 — デバッグ・プロファイリングインフラ

#### FR-540: DWARF Debug Information Generation

- **対象**: `src/binary/macho.lisp`, `src/compile/codegen.lisp`
- **現状**: Mach-Oバイナリ生成時にデバッグ情報なし。`lldb`/`gdb`でソースマップ不可
- **内容**:
  - DWARF 5形式でデバッグ情報を生成（`.debug_info`, `.debug_line`, `.debug_abbrev`セクション）
  - VM命令→元のLispフォームのソース位置マッピング
  - 変数名・型情報の埋め込み（デバッガからスロットが見える）
  - `--debug`ビルドフラグで有効化
- **根拠**: DWARF standard / LLVM debug info。プロダクションデバッグの必須条件
- **難易度**: Very Hard

#### FR-541: Sampling Profiler (サンプリングプロファイラ)

- **対象**: `src/vm/vm-run.lisp`, `src/cli/main.lisp`
- **現状**: プロファイリング機能なし。ホットスポット特定が不可能
- **内容**:
  - `SIGPROF`シグナルハンドラでスタックトレースをサンプリング（macOS: `setitimer`）
  - 関数名→カウントのヒストグラムを収集
  - `./cl-cc profile myscript.lisp` でフレームグラフ出力
  - FR-503（TFV）のプロファイルデータと統合
- **根拠**: Linux perf / py-spy / rbspy。実行時ホットスポット特定の標準ツール
- **難易度**: Hard

#### FR-542: Allocation Profiler (アロケーションプロファイラ)

- **対象**: `src/runtime/gc.lisp`, `src/vm/vm-run.lisp`
- **内容**:
  - `rt-gc-alloc`呼び出し時に呼び出し元情報（関数名・行番号）を記録
  - 型別・サイズ別の割り当て統計（`cons`が全割り当ての何%か等）
  - GCプレッシャーの高い関数を特定してTRMC/スタック割り当て変換の対象を絞る
- **根拠**: JVM allocation profiling (-agentlib:hprof) / Heaptrack
- **難易度**: Medium

#### FR-543: VM Instruction Tracer (VM命令トレーサ)

- **対象**: `src/vm/vm-run.lisp`
- **内容**:
  - `*vm-trace-mode*` フラグで実行命令をリアルタイム出力
  - 条件付きブレークポイント: `(vm-break-on 'vm-generic-call)` で特定命令型で停止
  - デバッガREPLへの接続（`slynk`/`swank`プロトコル拡張）
- **根拠**: SBCL `(trace)` / ChakraCore instruction tracing
- **難易度**: Medium

#### FR-544: Coverage Instrumentation (カバレッジ計測)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-run.lisp`
- **内容**:
  - `--coverage`フラグで各フォームの実行カウンタを埋め込み
  - `lcov`互換カバレッジレポートをHTMLで出力
  - テストスイートの未カバーパス特定（FiveAMとの統合）
- **根拠**: SBCL sb-cover / Istanbul / gcov
- **難易度**: Medium

---

### Phase 95 — コンカレントランタイム基盤

#### FR-550: Thread-Local Allocation Buffer (TLAB)

- **対象**: `src/runtime/heap.lisp`, `src/runtime/gc.lisp`
- **現状**: 単一バンプポインタ（`young-free`）へのアクセスがシリアライズされる
- **内容**:
  - 各スレッドにナーサリの一部（デフォルト256KB）を専有させる
  - スレッドローカルなバンプポインタでアロケーション
  - TLABが枯渇したらGCロックを取ってリフィル
- **根拠**: HotSpot TLAB / GraalVM TLAB。マルチスレッドアロケーションのスケールアップ
- **難易度**: Hard

#### FR-551: Atomic Value Operations (アトミック値操作)

- **対象**: `src/vm/vm.lisp`, `src/vm/primitives.lisp`
- **内容**:
  - `(atomic-swap! place new-val)` — CAS (Compare-And-Swap)
  - `(atomic-incf! place delta)` — アトミックインクリメント
  - `(memory-barrier)` — フェンス命令
  - x86-64: `lock cmpxchg` / AArch64: `ldxr`/`stxr`
- **根拠**: SBCL `sb-ext:atomic-push` / Java `AtomicInteger`。ロックフリーデータ構造の基盤
- **難易度**: Hard

#### FR-552: Green Thread / Continuation (グリーンスレッド/継続)

- **対象**: `src/vm/vm-execute.lisp`, `src/vm/vm-run.lisp`
- **内容**:
  - `(spawn fn args)` — VMレジスタファイルをスタックに保存して新コルーチンを作成
  - `(yield)` — 現スレッドをサスペンドしてスケジューラに制御を返す
  - `(resume co val)` — 保存されたレジスタファイルを復元して実行再開
  - M:Nスレッドスケジューラ（Mグリーンスレッド : NホストOSスレッド）
- **根拠**: Go goroutine / Erlang process / Ruby Fiber。Lispのcall/ccを軽量実装する基盤
- **難易度**: Very Hard

#### FR-553: Delimited Continuations (限定継続)

- **対象**: `src/vm/vm-execute.lisp`, `src/compile/cps.lisp`
- **依存**: FR-552
- **内容**:
  - `(reset thunk)` / `(shift k body)` — Filinski演算子
  - CPS変換済みコードとの統合（現在のCPS変換をshift/resetのコンパイルターゲットとして活用）
  - `call/cc` のより効率的な代替実装
- **根拠**: Racket delimited continuations / Koka effect handlers
- **難易度**: Very Hard

#### FR-554: Lock-Free Symbol Table (ロックフリーシンボルテーブル)

- **対象**: `src/vm/vm.lisp` (`vm-intern-symbol`)
- **現状**: シンボルインターンはハッシュテーブル（`vm-intern-table`）へのシリアルアクセス
- **内容**:
  - ロックフリーなhash map（Cliff Click's lock-free hash map / Swiss Table）への置換
  - `(intern "foo")` がCASループのみで完結
  - インターン済みシンボルの読み取りがノーロック
- **根拠**: Hotspot StringTable lock-free / Clojure persistent HashMap
- **難易度**: Hard

---

### Phase 96 — 動的コード管理・モジュールシステム

#### FR-560: JIT Code Cache Management (JITコードキャッシュ管理)

- **対象**: `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: コンパイル済みVM命令列はメモリ上に保持されるが容量上限なし。再コンパイル時の古いコード回収なし
- **内容**:
  - コードキャッシュサイズ上限（デフォルト256MB）を設定
  - LRU / 呼び出し頻度ベースの退避ポリシー
  - 退避されたコードを再JIT可能な状態で保持（ソースは保持）
- **根拠**: HotSpot code cache / V8 code flushing。長時間動作プロセスのメモリ安定化
- **難易度**: Hard

#### FR-561: Lazy Module Loading (遅延モジュールロード)

- **対象**: `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **内容**:
  - `(require "module")` を遅延実行：参照時点でコンパイル・ロード
  - モジュール依存グラフの構築と循環依存検出
  - コンパイル済みFASLキャッシュとの統合（タイムスタンプで再コンパイル判定）
- **根拠**: Python `importlib` lazy loading / Node.js lazy require
- **難易度**: Medium

#### FR-562: Hot Code Reload (ホットコードリロード)

- **対象**: `src/compile/pipeline.lisp`, `src/vm/vm.lisp`
- **内容**:
  - `(reload-function 'foo)` — 実行中プロセスの関数定義を差し替え
  - 実行中フレームの古い定義への参照を新定義にリダイレクト（Erlang hot_code_replace相当）
  - REPL・開発環境での高速イテレーション
- **根拠**: Erlang hot code reload / SBCL `(compile 'foo)`。Lisp開発体験の中核
- **難易度**: Hard

#### FR-563: Image Snapshot / Heap Dump (イメージスナップショット)

- **対象**: `src/runtime/heap.lisp`, `src/cli/main.lisp`
- **内容**:
  - `(save-image "myapp.img")` — 現在のヒープ状態をバイナリ化してディスクに保存
  - `./cl-cc --image myapp.img` — イメージをロードして実行再開
  - すべてのグローバル変数・定義済み関数・インターン済みシンボルを保存
- **根拠**: SBCL `save-lisp-and-die` / Smalltalk image。起動コスト0化の必殺技
- **難易度**: Very Hard

#### FR-564: Incremental Compilation Cache (増分コンパイルキャッシュ)

- **対象**: `src/compile/pipeline.lisp`
- **現状**: `./cl-cc selfhost`が毎回全84ファイルを再コンパイル（フルビルド）
- **内容**:
  - ファイルレベルのコンテンツハッシュ（SHA-256）で変更検出
  - 依存関係グラフ（`cl-cc.asd`のモジュール順序）を保持
  - 変更ファイルとその依存のみを再コンパイル
- **根拠**: `ccache` / Buck2 / Bazel incrementality。selfhostサイクルを10秒→1秒に短縮
- **難易度**: Medium

---

### Phase 97 — ランタイム完全自立化

#### FR-570: OS Abstraction Layer (OS抽象化レイヤ)

- **対象**: `src/runtime/runtime.lisp` (新ファイル `src/runtime/os.lisp`)
- **内容**:
  - ファイルIO: `rt-open`/`rt-read`/`rt-write`/`rt-close` — `read`/`write` syscallの薄いラッパ
  - プロセス: `rt-fork`/`rt-exec`/`rt-waitpid`
  - タイマー: `rt-gettime`（`clock_gettime`）/ `rt-sleep`（`nanosleep`）
  - macOS/Linux共通インターフェース（syscall番号をプラットフォーム別定数で分岐）
- **根拠**: musl libc相当の最小OSインターフェース。ホストCL依存を段階的に解消する基盤
- **難易度**: Hard

#### FR-571: Standalone Binary Bootstrap

- **対象**: `src/cli/main.lisp`, `src/binary/macho.lisp`, Makefile
- **依存**: FR-570
- **内容**:
  - `./cl-cc compile --standalone foo.lisp -o foo` で依存ゼロのネイティブバイナリを生成
  - ランタイム（GC + VM interpreter + 標準ライブラリ）をバイナリに静的リンク
  - エントリポイント: `_start` → ランタイム初期化 → `main`関数呼び出し
- **根拠**: Go `go build` の静的バイナリ / Zig `zig build-exe`
- **難易度**: Very Hard

#### FR-572: Signal Handling (シグナルハンドリング)

- **対象**: `src/runtime/os.lisp` (FR-570)
- **内容**:
  - `(signal-handler SIGINT (lambda () ...))` — OSシグナルをLispハンドラに変換
  - `SIGFPE` → `arithmetic-error` condition
  - `SIGSEGV` → `storage-condition` condition（スタックオーバーフロー検出）
  - セーフポイント（FR-510）との統合: シグナルはセーフポイントでのみ配送
- **根拠**: SBCL `sb-sys:enable-interrupt` / Erlang signal handling
- **難易度**: Hard

#### FR-573: Process/Environment Interface (プロセス・環境インターフェース)

- **対象**: `src/cli/main.lisp`, `src/runtime/runtime.lisp`
- **現状**: `(uiop:getenv "PATH")` 等のホストCL経由。ネイティブバイナリでは使用不可
- **内容**:
  - `(getenv "PATH")` — `environ`配列からの環境変数取得
  - `(argv)` — コマンドライン引数リスト
  - `(exit code)` — `_exit` syscall
  - `(getcwd)` / `(chdir path)`
- **根拠**: POSIX.1 process model。完全自立バイナリの必須インターフェース
- **難易度**: Easy

#### FR-574: Socket/Network Primitives (ソケット・ネットワーク原語)

- **対象**: 新ファイル `src/runtime/net.lisp`
- **内容**:
  - `(socket :tcp)` / `(bind s addr port)` / `(connect s addr port)` / `(listen s)` / `(accept s)`
  - ノンブロッキングIO: `(set-nonblocking s)` + `(select fds timeout)`
  - `epoll`（Linux）/ `kqueue`（macOS）イベントループ統合
- **根拠**: SBCL `sb-bsd-sockets` / Erlang gen_tcp。Webサーバ・REPLサーバの基盤
- **難易度**: Hard

---

### Phase 98 — 高度なランタイム最適化 (2026年最先端)

#### FR-580: Shape-Based Object Layout (シェイプ最適化)

- **対象**: `src/vm/vm-clos.lisp`, `src/runtime/heap.lisp`
- **現状**: CLOSインスタンスはハッシュテーブル（スロット名→値）。`slot-value`が毎回ハッシュ参照
- **内容**:
  - Hidden Class / Object Shape: 同じスロット集合を持つインスタンスは同一「シェイプ」を共有
  - シェイプIDをオブジェクトヘッダ（FR-266）に格納
  - `slot-value` → 固定オフセット配列アクセスに変換（ハッシュ参照不要）
  - `(slot-value obj 'x)` → `(aref (object-slots obj) shape-offset-of-x)`
- **根拠**: V8 Hidden Classes / PyPy map cache / SBCL structure slots
- **難易度**: Very Hard

#### FR-581: Speculative Slot Access Optimization

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **依存**: FR-580, FR-503
- **内容**:
  - TFVから「このオブジェクトは常にシェイプSを持つ」と判明した場合、固定オフセットアクセスにコンパイル
  - シェイプチェックガード（1命令）＋固定オフセットロード
  - シェイプ変化時はdeopt（FR-155）
- **根拠**: V8 Maglev property access / SpiderMonkey JIT slot load
- **難易度**: Very Hard

#### FR-582: Escape Analysis → Stack Allocation

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **依存**: FR-007（エスケープ解析）
- **内容**:
  - エスケープしないと証明されたCLOSインスタンス・クロージャをVMスタックフレームに直接展開
  - `(let ((p (make-instance 'point :x 1 :y 2))) (+ (slot-value p 'x) (slot-value p 'y)))` → スタック上の2変数に展開
  - GCヒープ割り当てを完全回避
- **根拠**: HotSpot Escape Analysis + Scalar Replacement / GraalVM PE
- **難易度**: Very Hard

#### FR-583: Partial Evaluation / Supercompilation

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `(defun make-adder (n) (lambda (x) (+ x n)))` → `(make-adder 5)` → `(lambda (x) (+ x 5))` に部分評価
  - 定数引数が既知の関数呼び出しを特化クロージャとしてコンパイル
  - メモ化付き特化（同じ定数引数の組み合わせは1回だけコンパイル）
- **根拠**: Futamura projections / GraalVM partial evaluation / PyPy tracing JIT
- **難易度**: Very Hard

#### FR-584: Profile-Guided Optimization (PGO)

- **対象**: `src/compile/pipeline.lisp`, `src/optimize/optimizer.lisp`
- **依存**: FR-541（サンプリングプロファイラ）, FR-503（TFV）
- **内容**:
  - Tier-1コンパイル時にTFVのプロファイルデータを参照して最適化判断
  - ホットブランチ予測情報をコード配置（hot/cold分離）に反映
  - `./cl-cc pgo-train myscript.lisp` → プロファイルデータ生成 → `./cl-cc compile --pgo`
- **根拠**: LLVM PGO / GCC -fprofile-use / HotSpot tiered profiling
- **難易度**: Very Hard

#### FR-585: Concurrent Compilation (並列コンパイル)

- **対象**: `src/compile/pipeline.lisp`, `src/cli/main.lisp`
- **現状**: selfhostが84ファイルをシリアルにコンパイル
- **内容**:
  - ASDF依存グラフのトポロジカルソートから並列化可能なファイルセットを抽出
  - `lparallel`/OSスレッドプールで独立ファイルを並列コンパイル
  - コンパイル中のマクロ定義が後続ファイルに伝播するセマンティクスを保持
- **根拠**: `make -jN` / Buck2 parallel actions / Gradle parallel tasks
- **難易度**: Hard

#### FR-586: Compressed Instructions (命令圧縮)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-run.lisp`
- **現状**: 全VM命令がCLOSオブジェクト（デフストラクト）としてリスト/ベクタに格納
- **内容**:
  - 高頻度命令（`vm-move`, `vm-const`, `vm-add`, `vm-call`）をfixed-width バイト列にエンコード
  - 命令デコーダ（`decode-instruction`）をdispatch-tableで実装
  - メモリフットプリントを最大70%削減、icache効率向上
- **根拠**: JVM bytecode / WASM binary format / RISC-V C拡張（16-bit圧縮命令）
- **難易度**: Hard

#### FR-587: Tracing JIT Prototype (トレースJITプロトタイプ)

- **対象**: `src/vm/vm-run.lisp`, `src/emit/x86-64-codegen.lisp`
- **依存**: FR-154, FR-155, FR-503
- **内容**:
  - ループバックエッジでのホットループ検出（カウンタ閾値100回）
  - トレース記録: ループ1周分のVM命令列をそのまま記録
  - トレース最適化: 型ガード挿入→定数畳み込み→レジスタ割り当て→x86-64ネイティブ出力
  - LuaJITに近い「インタープリタ主体、ホットループだけネイティブ」の戦略
- **根拠**: LuaJIT tracing JIT / TraceMonkey。実装コストに対してROIが高い
- **難易度**: Very Hard

---

### Phase 99 — Unicode・文字システム

#### FR-590: Full Unicode Character Database

- **対象**: `src/vm/strings.lisp`, `src/vm/vm.lisp` (新ファイル `src/vm/unicode.lisp`)
- **現状**: 文字述語（`alpha-char-p`等）がASCII範囲のみ動作。Unicode 15+の多バイト文字で誤動作
- **内容**:
  - Unicode Character Database (UCD) を静的テーブルとして埋め込み（約200KB）
  - `char-upcase`/`char-downcase` — Unicode case folding (Turkish dotless-i等の特殊ケース含む)
  - `alpha-char-p`/`digit-char-p`/`alphanumericp`/`upper-case-p`/`lower-case-p` — UCD General Category準拠
  - `char-code` — Unicode code point (U+0000〜U+10FFFF)、`code-char` — code point → character
- **根拠**: ANSI CL 13章 / Unicode 15.0 standard。多言語テキスト処理の必須基盤
- **難易度**: Hard

#### FR-591: Unicode String Normalization (Unicode正規化)

- **対象**: `src/vm/strings.lisp`
- **依存**: FR-590
- **内容**:
  - NFC (Canonical Decomposition, Canonical Composition) — 最も一般的な形式
  - NFD (Canonical Decomposition)
  - NFKC/NFKD (Compatibility forms)
  - `(string-normalize str :nfc)` — 正規化API
  - `string=` / `string<` でのユニコード照合順序（UCA: Unicode Collation Algorithm）
- **根拠**: ICU (International Components for Unicode) / Python `unicodedata.normalize`
- **難易度**: Very Hard

#### FR-592: UTF-8 Internal Encoding (内部エンコーディング)

- **対象**: `src/vm/strings.lisp`, `src/runtime/value.lisp`
- **現状**: 文字列がホストCLの内部表現に依存（SBCLはUTF-32等）
- **内容**:
  - VM内部文字列をUTF-8バイト列として格納（最もメモリ効率が高い）
  - `vm-char`命令 → コードポイント単位のインデックス（O(n)だが一般的）
  - `vm-string-byte-length` vs `vm-string-char-length` の区別
  - FR-265（SSO）との統合：7バイト以下のUTF-8シーケンスをインライン格納
- **根拠**: Go `string` / Rust `str` / Python 3.12 PEP 623 compact encoding
- **難易度**: Hard

#### FR-593: Character Syntax Classes (文字構文クラス)

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: FR-136のASCII 256バイトテーブル。リードテーブルの文字構文クラス（whitespace/constituent/macro-char等）がハードコード
- **内容**:
  - `readtable-syntax-type`: `:constituent` / `:whitespace` / `:terminating-macro` / `:non-terminating-macro` / `:single-escape` / `:multiple-escape`
  - `set-syntax-from-char` — 文字の構文クラスをコピー
  - Unicode空白文字（U+00A0, U+2003等）を`:whitespace`として認識
- **根拠**: ANSI CL 2.1 Character Syntax Types
- **難易度**: Medium

---

### Phase 100 — パスネーム・ファイルシステム

#### FR-595: ANSI CL Pathname System (パスネームシステム)

- **対象**: `src/vm/io.lisp` (新ファイル `src/vm/pathname.lisp`)
- **現状**: ファイルパスをそのまま文字列として扱う。`pathname`型なし
- **内容**:
  - `pathname` オブジェクト: `host`/`device`/`directory`/`name`/`type`/`version` コンポーネント
  - `make-pathname` / `pathname-host` / `pathname-directory` / `pathname-name` / `pathname-type`
  - `merge-pathnames` — 相対パスの解決
  - `namestring` / `file-namestring` / `directory-namestring` / `enough-namestring`
  - Unix形式: `/foo/bar/baz.lisp` ↔ `(:absolute "foo" "bar")` + name `"baz"` + type `"lisp"`
- **根拠**: ANSI CL 19章 Filenames。`compile-file` / `load` / `open` の基盤
- **難易度**: Medium

#### FR-596: Logical Pathnames (論理パスネーム)

- **対象**: `src/vm/pathname.lisp` (FR-595)
- **依存**: FR-595
- **内容**:
  - `logical-pathname` 型: `"SRC:COMPILE;CODEGEN.LISP"` 形式
  - `logical-pathname-translations` — 論理→物理パスのマッピングテーブル
  - `translate-logical-pathname` — 変換の実行
  - ASDF統合: `:cl-cc` ホストを自動登録
- **根拠**: ANSI CL 19.3 Logical Pathnames。ポータブルなソース参照に必要
- **難易度**: Medium

#### FR-597: Filesystem Operations (ファイルシステム操作)

- **対象**: `src/vm/io.lisp`
- **現状**: `open`/`close`/`read`/`write`は部分実装。ディレクトリ操作なし
- **内容**:
  - `probe-file` — ファイル存在確認
  - `file-write-date` / `file-author` — ファイルメタデータ
  - `directory` — ディレクトリ一覧（ワイルドカードパスネーム対応）
  - `rename-file` / `delete-file` / `ensure-directories-exist`
  - `file-length` — ファイルサイズ取得
- **根拠**: ANSI CL 20章 Files。ビルドシステム・コンパイラの必須インターフェース
- **難易度**: Medium

---

### Phase 101 — ストリーム階層

#### FR-600: ANSI CL Stream Types (ストリーム型完全実装)

- **対象**: `src/vm/io.lisp` (新ファイル `src/vm/stream.lisp`)
- **現状**: ファイルストリームと文字列ストリームの部分実装のみ
- **内容**:
  - `broadcast-stream` — 複数ストリームへの同時書き込み (`make-broadcast-stream`)
  - `concatenated-stream` — 複数ストリームからの順次読み込み (`make-concatenated-stream`)
  - `echo-stream` — 入力を出力にコピーしながら読む (`make-echo-stream`)
  - `synonym-stream` — シンボルが指すストリームへの委譲 (`make-synonym-stream`)
  - `two-way-stream` — 入力ストリームと出力ストリームの組み合わせ (`make-two-way-stream`)
  - `string-input-stream` / `string-output-stream` / `get-output-stream-string`
- **根拠**: ANSI CL 21章 Streams。`*standard-output*` 等の標準変数の基盤
- **難易度**: Medium

#### FR-601: Gray Streams (Grayストリーム拡張)

- **対象**: `src/vm/stream.lisp` (FR-600)
- **依存**: FR-600
- **内容**:
  - `fundamental-stream` / `fundamental-character-input-stream` / `fundamental-character-output-stream`
  - ユーザーが `stream-read-char` / `stream-write-char` 等のジェネリック関数をオーバーライドしてカスタムストリームを作成
  - `with-input-from-string` / `with-output-to-string` の内部実装として活用
- **根拠**: Gray Streams proposal (STREAM-DEFINITION-BY-USER) / SBCL Gray streams
- **難易度**: Medium

#### FR-602: Bivalent Streams (バイバレントストリーム)

- **対象**: `src/vm/stream.lisp`
- **内容**:
  - `read-byte` / `write-byte` — バイナリモード
  - `read-char` / `write-char` — 文字モード
  - `:element-type '(unsigned-byte 8)` の `open` でバイナリストリーム
  - `:external-format :utf-8` — エンコーディング指定
- **根拠**: ANSI CL 21.1。バイナリファイルとテキストファイルの統一的な扱い
- **難易度**: Medium

---

### Phase 102 — プリティプリンタ・リードテーブル

#### FR-605: ANSI CL Pretty Printer (プリティプリンタ)

- **対象**: `src/vm/io.lisp` (新ファイル `src/vm/pprint.lisp`)
- **現状**: `print`/`princ`/`prin1` は基本実装のみ。インデント・改行なし
- **内容**:
  - `pprint-logical-block` / `pprint-indent` / `pprint-newline` / `pprint-tab`
  - `pprint-dispatch-table` — 型→プリンタ関数のディスパッチテーブル
  - `copy-pprint-dispatch` / `set-pprint-dispatch` / `get-pprint-dispatch`
  - `*print-pretty*` = `t` 時に自動使用
  - Lispコードの標準的なインデント（`defun`/`let`/`if`等のフォーム別ルール）
- **根拠**: ANSI CL 22.3 Pretty Printer。REPL出力の可読性に直結
- **難易度**: Hard

#### FR-606: Print Control Variables (印刷制御変数)

- **対象**: `src/vm/io.lisp`, `src/vm/vm.lisp`
- **現状**: `*print-escape*` のみ部分実装。循環検出・深さ制限なし
- **内容**:
  - `*print-level*` — 構造の最大深さ（超えたら `#` を出力）
  - `*print-length*` — リストの最大表示要素数（超えたら `...` を出力）
  - `*print-circle*` — 循環構造の `#1=`/`#1#` 表現
  - `*print-readably*` — read-backできる形式を保証
  - `*print-base*` / `*print-radix*` — 整数の基数
  - `with-standard-io-syntax` — 上記を標準値にバインド
- **根拠**: ANSI CL 22.1。デバッグ出力・シリアライズの制御
- **難易度**: Medium

#### FR-607: Multiple Readtables (複数リードテーブル)

- **対象**: `src/parse/cl/lexer.lisp`, `src/vm/vm.lisp`
- **現状**: 単一グローバルリードテーブル。`*readtable*` 変数なし
- **内容**:
  - `make-readtable` / `copy-readtable` — リードテーブルオブジェクト
  - `set-macro-character` / `get-macro-character` — マクロ文字の登録
  - `set-dispatch-macro-character` / `get-dispatch-macro-character` — `#X` ディスパッチ文字
  - `*readtable*` — 現在のリードテーブルを保持するスペシャル変数
  - `readtable-case` — `:upcase`/`:downcase`/`:preserve`/`:invert`
- **根拠**: ANSI CL 2.1 / SBCL named-readtables。DSLやリーダーマクロライブラリの基盤
- **難易度**: Hard

---

### Phase 103 — 時刻・乱数・環境API

#### FR-610: Time API (時刻API)

- **対象**: `src/vm/vm.lisp` (新ファイル `src/vm/time.lisp`)
- **現状**: `get-universal-time` / `get-internal-real-time` がホストCL経由のスタブ
- **内容**:
  - `get-universal-time` — Unix epoch からの秒数（`clock_gettime(CLOCK_REALTIME)`）
  - `get-internal-real-time` / `internal-time-units-per-second` — 高精度タイマー
  - `sleep` — `nanosleep` syscall
  - `encode-universal-time year month day hour minute second &optional timezone`
  - `decode-universal-time universal-time &optional timezone` → 7値返却
  - `time` マクロ — フォーム実行時間の計測と出力
- **根拠**: ANSI CL 25.1 Time。ベンチマーク・スケジューラ・ログの基盤
- **難易度**: Easy

#### FR-611: Random Number Generator (乱数生成器)

- **対象**: `src/vm/primitives.lisp` (新ファイル `src/vm/random.lisp`)
- **現状**: `random` がホストCLに委譲。`make-random-state` なし
- **内容**:
  - `random-state` オブジェクト（Mersenne Twister MT19937の内部状態）
  - `make-random-state &optional state` — `t` で現在時刻からシード生成
  - `*random-state*` — デフォルトのスペシャル変数
  - `random n &optional random-state` — [0, n) の一様乱数
  - `(random 1.0)` — float乱数も対応
  - スレッドセーフ: 各スレッドが独立した `*random-state*` を持つ
- **根拠**: ANSI CL 12.1.7 Random Number Generation / MT19937 (1998)
- **難易度**: Easy

#### FR-612: Environment Introspection API (環境内省API)

- **対象**: `src/vm/vm.lisp`, `src/cli/main.lisp`
- **内容**:
  - `lisp-implementation-type` → `"cl-cc"`
  - `lisp-implementation-version` → バージョン文字列
  - `machine-type` / `machine-version` / `machine-instance`
  - `software-type` / `software-version`
  - `room` — ヒープ使用状況の表示
  - `apropos` / `apropos-list` — シンボル名の部分一致検索
- **根拠**: ANSI CL 25.2 Environment Inquiry。REPL・開発ツールの基盤
- **難易度**: Easy

---

### Phase 104 — 末尾呼び出し・スタック安全

#### FR-615: Full Proper Tail Calls (完全末尾呼び出し最適化)

- **対象**: `src/compile/cps.lisp`, `src/compile/codegen.lisp`
- **現状**: FR-045 (TRMC) は `(cons x (f y))` パターンのみ。相互再帰の末尾呼び出し最適化は限定的
- **内容**:
  - CPS変換後の全末尾位置を検出（`ast-tail-position-p`述語）
  - 末尾呼び出し位置の `vm-call` → `vm-tail-call` 命令に変換
  - `vm-tail-call`: 現スタックフレームを使い回し（新フレームをpushしない）
  - 相互再帰 `(f → g → f → ...)` がスタックオーバーフローしない
  - `named-let` / `labels` の相互再帰が定数スタックで動作
- **根拠**: ANSI CL 5.3 TAIL-RECURSIVE / Scheme R7RS TCO要件 / SBCL proper tail calls
- **難易度**: Hard

#### FR-616: Stack Overflow Guard (スタックオーバーフロー保護)

- **対象**: `src/vm/vm-run.lisp`, `src/runtime/runtime.lisp`
- **現状**: 深い再帰でホストCLのスタックオーバーフローが発生し、Lispレベルのエラーでなくクラッシュ
- **内容**:
  - `*max-call-stack-depth*` — 最大呼び出し深さ（デフォルト10000）
  - 各 `vm-call` でデプスカウンタをインクリメント、閾値で `stack-overflow` conditionを signal
  - ネイティブバックエンド（FR-571）ではガードページ（`mmap`で無効ページ配置）
  - `sb-kernel:*max-trace-depth*` 相当のデバッガ制御
- **根拠**: SBCL `sb-kernel:stack-overflow` / JVM `-Xss` スタックサイズ指定
- **難易度**: Medium

#### FR-617: OOM Condition / GC Pressure Threshold (メモリ不足ハンドリング)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: メモリ枯渇時にホストCLのエラーが発生。Lispレベルでの回復不可
- **内容**:
  - `storage-condition` — ANSI CL標準のメモリ不足condition
  - `rt-gc-alloc`失敗時（旧世代もフル）に`signal 'storage-condition`
  - ヒープ使用量 80% / 90% / 95% 閾値でのwarning condition
  - `*heap-limit*` — プロセス全体のヒープ上限
  - `with-memory-limit (n) body` — スコープ内のメモリ使用量制限
- **根拠**: ANSI CL 09.1 / SBCL `storage-condition` / JVM `-Xmx` / Node.js `--max-old-space-size`
- **難易度**: Medium

---

### Phase 105 — シンボル・パッケージシステム

#### FR-620: Symbol Property Lists (シンボルプロパティリスト)

- **対象**: `src/vm/vm.lisp`
- **現状**: `vm-symbol`はname/packageのみ保持。プロパティリストなし
- **内容**:
  - `symbol-plist` — シンボルのプロパティリスト（`(:key1 val1 :key2 val2 ...)`形式）
  - `get symbol indicator &optional default` — プロパティ取得
  - `(setf (get sym :prop) val)` — プロパティ設定
  - `remprop symbol indicator` — プロパティ削除
  - `symbol-value` / `symbol-function` — Lispレベルのアクセサ
- **根拠**: ANSI CL 10.1 Symbol Concepts。マクロ展開情報・型宣言の格納先
- **難易度**: Easy

#### FR-621: Package System Completeness (パッケージシステム完全化)

- **対象**: `src/vm/vm.lisp` (`vm-intern-symbol`, `vm-find-package`)
- **現状**: パッケージの基本インターン機能のみ。パッケージ操作API不完全
- **内容**:
  - `make-package` / `delete-package` / `find-package` / `list-all-packages`
  - `package-name` / `package-nicknames` / `package-use-list` / `package-used-by-list`
  - `use-package` / `unuse-package`
  - `shadow` / `shadowing-import` / `unexport` / `unintern`
  - `do-symbols` / `do-external-symbols` / `do-all-symbols`
  - `with-package-iterator`
- **根拠**: ANSI CL 11章 Packages。モジュール分離・名前空間管理の基盤
- **難易度**: Medium

#### FR-622: Package Locks (パッケージロック)

- **対象**: `src/vm/vm.lisp`
- **依存**: FR-621
- **内容**:
  - `lock-package` / `unlock-package` — パッケージのシンボル追加を禁止
  - `:cl` / `:cl-user` パッケージのデフォルトロック
  - `with-unlocked-packages` — ロックを一時解除
  - `package-locked-error` condition
- **根拠**: SBCL package locks / CMUCL。標準パッケージの保護
- **難易度**: Easy

---

### Phase 106 — 数値パーサ・formatディレクティブ完全化

#### FR-625: Number Reader Completeness (数値リーダ完全化)

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: integer / float の基本パース。Rational・Complex・N進数が未実装
- **内容**:
  - Rational: `3/4` → `(make-rational 3 4)` — スラッシュで分子/分母を分割
  - Complex: `#C(1.0 2.0)` / `#C(1 2)` — ディスパッチマクロ `#C`
  - N進数: `#2r1010` (2進)、`#8r17` (8進)、`#16rFF` (16進)、`#Nrdigits` (N=2〜36)
  - `#b`/`#o`/`#x` ショートハンド
  - `*read-base*` — デフォルト基数（標準は10）
- **根拠**: ANSI CL 2.3 Interpretation of Tokens。数値リテラルの完全サポート
- **難易度**: Medium

#### FR-626: Format Directive Completeness (formatディレクティブ完全化)

- **対象**: `src/vm/format.lisp` (または `src/vm/io.lisp`)
- **現状**: `~A`/`~S`/`~%`/`~&`/`~T` の基本実装のみ
- **内容**:
  - 数値系: `~D`(10進) / `~B`(2進) / `~O`(8進) / `~X`(16進) / `~R`(任意基数/英語序数)
  - 浮動小数: `~F`(固定) / `~E`(指数) / `~G`(適応) / `~$`(通貨)
  - 文字: `~C` (char名)
  - 繰り返し: `~*`(引数スキップ) / `~?`(間接参照) / `~{...~}`(リストループ) / `~<...~>`(詰め込み)
  - 条件: `~[...~]`(選択) / `~^`(ループ終了)
  - 複数値: `~/pkg:fn/`(ユーザー定義ディレクティブ)
- **根拠**: ANSI CL 22.3 Formatted Output。CLコードの出力の大部分がformatを使用
- **難易度**: Hard

#### FR-627: READ-EVAL-PRINT Loop Completeness (REPL完全化)

- **対象**: `src/cli/main.lisp`, `src/vm/vm.lisp`
- **現状**: 基本的なread-eval-printループ。`*` / `**` / `***` / `+` / `++` / `+++` なし
- **内容**:
  - `*`/`**`/`***` — 直近3つの一次返却値
  - `+`/`++`/`+++` — 直近3つの入力フォーム
  - `/`/`//`/`///` — 直近3つの返却値リスト（`values`対応）
  - `*terminal-io*` / `*query-io*` / `*debug-io*` の独立管理
  - `:exit` / `:help` コマンドのREPL組み込み
- **根拠**: ANSI CL 25.1 The Read-Eval-Print Loop / SBCL REPL variables
- **難易度**: Easy

---

### Phase 107 — 配列完全化 (Adjustable/Fill-Pointer/Displaced/Bit)

#### FR-630: Adjustable Arrays (可変長配列)

- **対象**: `src/vm/array.lisp`, `src/vm/list.lisp`
- **現状**: `make-array`は固定長のみ。`:adjustable t`オプションを無視
- **内容**:
  - `(make-array n :adjustable t)` — サイズ変更可能フラグをオブジェクトヘッダに格納
  - `adjust-array array new-size &key initial-element` — 配列のin-place拡張/縮小
  - 拡張時: 新バッファを`rt-gc-alloc`で確保してコピー（`realloc`相当）
  - `adjustable-array-p` — アジャスタブルフラグの問い合わせ
  - `array-element-type` — 要素型の取得
- **根拠**: ANSI CL 15.2 — adjustable arrays。動的サイズのバッファに必須
- **難易度**: Medium

#### FR-631: Fill Pointer (フィルポインタ)

- **対象**: `src/vm/array.lisp`
- **現状**: 配列のフィルポインタなし。`vector-push`/`vector-pop`未実装
- **内容**:
  - `(make-array 16 :fill-pointer 0)` — フィルポインタ初期値を指定
  - `vector-push element vector` — フィルポインタ位置に書き込み、インクリメント
  - `vector-push-extend element vector &optional extension` — フィルポインタ超過時に自動拡張
  - `vector-pop vector` — フィルポインタをデクリメントして末尾要素を返す
  - `fill-pointer vector` / `(setf fill-pointer)` — フィルポインタの読み書き
  - `array-has-fill-pointer-p` — フィルポインタ有無の確認
- **根拠**: ANSI CL 15.2。スタック・キュー・動的バッファの基本操作
- **難易度**: Medium

#### FR-632: Displaced Arrays (ディスプレースト配列)

- **対象**: `src/vm/array.lisp`
- **内容**:
  - `(make-array n :displaced-to base-array :displaced-index-offset k)` — 別配列の一部を共有
  - `aref`/`aset` が透過的にベース配列のオフセット位置にアクセス
  - `array-displacement` — ベース配列とオフセットの取得
  - 多次元配列のスライス操作に利用（行列の行ベクタ抽出等）
- **根拠**: ANSI CL 15.1.2。数値計算・行列演算の基盤
- **難易度**: Hard

#### FR-633: Bit Arrays / Bit Vectors (ビット配列)

- **対象**: `src/vm/array.lisp`
- **現状**: `bit-vector-p` は部分実装。ビット演算命令なし
- **内容**:
  - `(make-array n :element-type 'bit)` — ビットベクタ（64bits/ワードで圧縮格納）
  - `bit-and`/`bit-or`/`bit-xor`/`bit-eqv`/`bit-nand`/`bit-nor`/`bit-andc1`/`bit-andc2`/`bit-orc1`/`bit-orc2`/`bit-not`
  - `bit`/`sbit` — ビットベクタのインデックスアクセス
  - ビット演算は`unsigned-long`単位で実装し、要素ループを最大64倍高速化
- **根拠**: ANSI CL 15.3。集合演算・フィルタマスク・GCビットマップに活用
- **難易度**: Medium

#### FR-634: Array Dimension Queries (配列次元問い合わせ)

- **対象**: `src/vm/array.lisp`
- **内容**:
  - `array-rank` — 次元数（1次元ベクタ→1）
  - `array-dimensions` — 各次元のサイズリスト
  - `array-dimension array axis-number` — 特定次元のサイズ
  - `array-total-size` — 全要素数
  - `array-in-bounds-p array &rest subscripts` — インデックス境界チェック
  - `row-major-aref` / `(setf row-major-aref)` — 線形インデックスアクセス
- **根拠**: ANSI CL 15.2。多次元配列操作の必須述語群
- **難易度**: Easy

---

### Phase 108 — I/O操作完全化

#### FR-638: Read Operations Completeness (read系操作完全化)

- **対象**: `src/vm/io.lisp`, `src/vm/format.lisp`
- **現状**: `read-char`は部分実装。`unread-char`/`peek-char`/`read-line`/`read-sequence`なし
- **内容**:
  - `read-char &optional stream eof-error-p eof-value recursive-p`
  - `unread-char character &optional stream` — 1文字プッシュバック（1文字保証）
  - `peek-char &optional peek-type stream eof-error-p eof-value recursive-p`
  - `read-line &optional stream eof-error-p eof-value recursive-p` → (string . missing-newline-p)
  - `read-sequence sequence stream &key start end` — バッファ一括読み込み
  - `listen &optional stream` — ノンブロッキング入力確認
  - `clear-input &optional stream` — 入力バッファフラッシュ
- **根拠**: ANSI CL 21.2 / 13.2 Stream Input。パーサ・REPLの基盤
- **難易度**: Medium

#### FR-639: Write Operations Completeness (write系操作完全化)

- **対象**: `src/vm/io.lisp`, `src/vm/format.lisp`
- **内容**:
  - `write-sequence sequence stream &key start end` — バッファ一括書き込み
  - `write-char character &optional stream` — 単一文字出力
  - `write-string string &optional stream &key start end`
  - `write-line string &optional stream &key start end` — 末尾改行付き
  - `clear-output &optional stream` / `finish-output &optional stream` / `force-output &optional stream`
  - `write-to-string object &rest keys` / `prin1-to-string object` / `princ-to-string object`
- **根拠**: ANSI CL 21.2 Stream Output。`format`以外の出力基盤
- **難易度**: Easy

#### FR-640: with-open-file Full Options (with-open-fileオプション完全化)

- **対象**: `src/vm/io.lisp`
- **現状**: `:input`/`:output`の基本モードのみ。`:if-exists`/`:if-does-not-exist`を無視
- **内容**:
  - `:direction` — `:input`/`:output`/`:io`/`:probe`
  - `:if-exists` — `:error`/`:new-version`/`:rename`/`:rename-and-delete`/`:overwrite`/`:append`/`:supersede`/`nil`
  - `:if-does-not-exist` — `:error`/`:create`/`nil`
  - `:external-format` — `:utf-8`/`:latin-1`/`:default`
  - `:element-type` — `'character`/`'(unsigned-byte 8)`
- **根拠**: ANSI CL 21.2 open。ファイル操作の全ユースケースをカバー
- **難易度**: Medium

---

### Phase 109 — コンディションシステム完全化

#### FR-643: Standard Condition Type Hierarchy (標準コンディション型階層)

- **対象**: `src/vm/conditions.lisp`
- **現状**: `error`/`simple-error`の基本型のみ。標準型階層が未完全
- **内容**:
  ```
  condition
  ├── serious-condition
  │   ├── error
  │   │   ├── simple-error
  │   │   ├── type-error (datum, expected-type)
  │   │   ├── arithmetic-error (operation, operands)
  │   │   │   ├── division-by-zero
  │   │   │   ├── floating-point-overflow
  │   │   │   └── floating-point-underflow
  │   │   ├── cell-error (name)
  │   │   │   ├── unbound-variable
  │   │   │   ├── undefined-function
  │   │   │   └── unbound-slot (instance)
  │   │   ├── control-error
  │   │   │   └── program-error
  │   │   ├── stream-error (stream)
  │   │   │   ├── end-of-file
  │   │   │   └── reader-error
  │   │   └── package-error (package)
  │   └── storage-condition
  └── warning
      ├── simple-warning
      └── style-warning
  ```
- **根拠**: ANSI CL 9.2 Condition Types。型安全なエラー処理の必須基盤
- **難易度**: Medium

#### FR-644: Standard Restarts (標準リスタート)

- **対象**: `src/vm/conditions.lisp`
- **内容**:
  - `abort` — 現在の操作を中止して最も近い`restart-case`へ
  - `continue` — 警告等を無視して継続
  - `muffle-warning` — `warning`を黙殺（`handler-bind`との組み合わせ）
  - `use-value value` — 代替値を提供して継続
  - `store-value value` — 値を格納して継続（`cell-error`の修正）
  - `retry` — 操作を最初からリトライ
  - `invoke-restart`/`find-restart`/`restart-name`/`compute-restarts`
- **根拠**: ANSI CL 9.1.4 Standard Restart Functions
- **難易度**: Medium

#### FR-645: Debugger Hook / Interactive Debugger (デバッガフック)

- **対象**: `src/vm/conditions.lisp`, `src/cli/main.lisp`
- **内容**:
  - `*debugger-hook*` — デバッガ起動時に呼ばれるフック関数 `(lambda (condition hook) ...)`
  - `invoke-debugger condition` — デバッガを明示的に起動
  - `break &optional format-string &rest args` — デバッガに入る（`continue`リスタート付き）
  - `*break-on-signals*` — 特定conditionでbreak
  - デバッガレベル: `*debugger-level*` のネスト管理
- **根拠**: ANSI CL 9.2 / SBCL debugger。本番バグの対話的修復
- **難易度**: Hard

#### FR-646: Condition Report / Print-Object Integration

- **対象**: `src/vm/conditions.lisp`, `src/vm/io.lisp`
- **内容**:
  - `define-condition` の `:report` オプション — condition→文字列の変換関数
  - `print-object` メソッドで `#<ERROR: message>` 形式のデフォルト出力
  - `condition-report condition stream` — condition型のhuman-readable報告
  - バックトレース付きエラー出力: `print-backtrace &optional stream n-frames`
- **根拠**: ANSI CL 9.1.3。`(format t "~A" err)` で読みやすいエラーメッセージ
- **難易度**: Medium

---

### Phase 110 — 文字・文字列操作完全化

#### FR-650: Character Comparison Functions (文字比較関数)

- **対象**: `src/vm/strings.lisp`
- **現状**: `char=`/`char/=`のみ実装。大小比較・ケース非依存比較なし
- **内容**:
  - ケース依存: `char<`/`char>`/`char<=`/`char>=`
  - ケース非依存: `char-equal`/`char-not-equal`/`char-lessp`/`char-greaterp`/`char-not-greaterp`/`char-not-lessp`
  - 全て可変引数対応: `(char< a b c)` → `(and (char< a b) (char< b c))`
  - Unicode照合順序（FR-591 UCA）との統合
- **根拠**: ANSI CL 13.2 Character Comparisons
- **難易度**: Easy

#### FR-651: Character Name Functions (文字名関数)

- **対象**: `src/vm/strings.lisp`, `src/parse/cl/lexer.lisp`
- **現状**: `#\Space`/`#\Newline` 等のリードは実装済みだが、`char-name`/`name-char`なし
- **内容**:
  - `char-name character` → 名前文字列 (`#\Space` → `"Space"`, `#\A` → `nil`)
  - `name-char name` → 文字 (`"Space"` → `#\Space`, `"Newline"` → `#\Newline`)
  - 標準名前: `Space`, `Newline`, `Tab`, `Return`, `Rubout`, `Backspace`, `Page`, `Null`, `Altmode`, `Delete`, `Escape`
  - Unicode拡張名: U+XXXX形式の名前もサポート
- **根拠**: ANSI CL 13.2
- **難易度**: Easy

#### FR-652: String Trim Functions (文字列トリム関数)

- **対象**: `src/vm/strings.lisp`
- **現状**: `string-trim`/`string-left-trim`/`string-right-trim`なし
- **内容**:
  - `string-trim bag string` — 両端からbag中の文字を除去
  - `string-left-trim bag string` — 先頭のみ
  - `string-right-trim bag string` — 末尾のみ
  - bagは文字バッグ（文字リスト/文字列）
  - `(string-trim " " "  hello  ")` → `"hello"`
- **根拠**: ANSI CL 16.2 String Trimming
- **難易度**: Easy

#### FR-653: Destructive String Case (破壊的文字列大小変換)

- **対象**: `src/vm/strings.lisp`
- **現状**: `string-upcase`/`string-downcase`/`string-capitalize`は非破壊版のみ
- **内容**:
  - `nstring-upcase`/`nstring-downcase`/`nstring-capitalize` — in-place変換
  - `:start`/`:end`キーワードによる部分範囲操作
  - `string-upcase`/`string-downcase`/`string-capitalize` にも`:start`/`:end`を追加
- **根拠**: ANSI CL 16.2
- **難易度**: Easy

#### FR-654: String Comparison Functions (文字列比較関数)

- **対象**: `src/vm/strings.lisp`
- **現状**: `string=`/`string/=`のみ
- **内容**:
  - ケース依存: `string<`/`string>`/`string<=`/`string>=`
  - ケース非依存: `string-equal`/`string-not-equal`/`string-lessp`/`string-greaterp`/`string-not-greaterp`/`string-not-lessp`
  - 全て `:start1`/`:end1`/`:start2`/`:end2` キーワード対応
  - 不一致位置のインデックスを返す（`string<` は最初の相違点の位置）
- **根拠**: ANSI CL 16.2 String Comparisons
- **難易度**: Easy

---

### Phase 111 — λリスト・関数プロトコル最適化

#### FR-657: Efficient &key Argument Parsing (&key引数の高速パース)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-execute.lisp`
- **現状**: `&key`引数を毎回`dolist`でリスト走査。キー数に比例するO(n)コスト
- **内容**:
  - `&key`パラメータをコンパイル時にパーフェクトハッシュに変換
  - 実行時: `gethash`1回でキー→スロットのマッピング
  - 未知キーの`:allow-other-keys`チェックをガードに変換
  - SBCLの`sb-c::keyword-dispatch`相当の実装
- **根拠**: LuaJIT hash dispatch / SBCL keyword arg fast path。関数呼び出しのホットパス
- **難易度**: Hard

#### FR-658: &rest List Allocation Optimization (&restリスト割り当て最適化)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-execute.lisp`
- **現状**: `&rest args`が毎回`(list ...)`でコンスセルリストを構築。GCプレッシャー大
- **内容**:
  - `&rest`引数を`apply`に直接渡す場合（最終引数として使うだけ）: リスト構築をスキップ
  - `dynamic-extent`宣言がある場合: スタック上の配列として格納
  - `(declare (dynamic-extent args))` + `(apply fn required args)` パターンの特化
- **根拠**: SBCL `&rest` stack allocation / GHC `seq` spine strictness
- **難易度**: Hard

#### FR-659: Optional Argument Default Evaluation (オプション引数デフォルト評価)

- **対象**: `src/compile/codegen.lisp`
- **現状**: `&optional`/`&key`のデフォルト形式が毎回評価される（`(make-array 10)`等の高コスト式）
- **内容**:
  - デフォルト形式が定数の場合: コンパイル時に評価して`vm-const`に変換
  - デフォルト形式が副作用なしの場合: 条件付きで評価（供給時はスキップ）
  - `-p`サプライドフラグを`vm-const nil`に変換（供給なし確定の場合）
- **根拠**: SBCL optional arg constant folding
- **難易度**: Medium

#### FR-660: Dynamic-Extent Optimization (動的エクステント最適化)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **内容**:
  - `(declare (dynamic-extent fn))` — クロージャのスタック割り当て（エスケープ不可と保証）
  - `(declare (dynamic-extent list))` — `(list ...)` のスタック割り当て
  - エスケープ解析（FR-007）と連携: 宣言がなくてもエスケープ不可と証明できれば適用
- **根拠**: ANSI CL `dynamic-extent` / SBCL stack allocation。高頻度一時オブジェクトのGC回避
- **難易度**: Hard

---

### Phase 112 — 動的変数バインディング最適化

#### FR-663: Per-Thread Binding Stack (スレッド別バインディングスタック)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-execute.lisp`
- **現状**: スペシャル変数のバインディングが`vm-global-vars`ハッシュテーブルに格納。スレッドセーフでない
- **内容**:
  - 各スレッドが独立したバインディングスタックを持つ: `((sym . old-val) ...)`のスタック
  - `let`/`progv`でスペシャル変数をバインドする際: スタックにpush + グローバルを新値に更新
  - アンワインド時: スタックからpopして旧値を復元
  - スレッドローカルストレージ（TLS）としてバインディングスタックのポインタを格納
- **根拠**: SBCL per-thread binding stack / HotSpot ThreadLocalStorage。マルチスレッドの正確性
- **難易度**: Hard

#### FR-664: Fluid-Let / LetDynamic Fast Path

- **対象**: `src/vm/vm-execute.lisp`
- **依存**: FR-663
- **内容**:
  - バインディングスタックのpush/popを`vm-bind-special`/`vm-unbind-special`命令に特化
  - `unwind-protect`の cleanup フォーム内でアンワインドを保証
  - ネストした`let`が同一変数を繰り返しバインドする場合: スタック深さが1の特化パス
- **根拠**: SBCL `let`/`progv` dynamic binding。`*print-pretty*`等の標準変数のバインドコスト削減
- **難易度**: Medium

#### FR-665: Global Variable Type Specialization (グローバル変数型特化)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **内容**:
  - `(defvar *count* 0)` の`*count*`が常にfixnumと判明している場合、アクセスを特化
  - `(declaim (type fixnum *count*))` → グローバル変数アクセス命令を型特化版に変換
  - 型チェックをロード時にのみ実行（ストア時ガード）
- **根拠**: SBCL `(declaim (type t *x*))` global type declarations
- **難易度**: Medium

---

### Phase 113 — 数値I/O・解析

#### FR-668: parse-integer / parse-float (数値文字列パーサ)

- **対象**: `src/vm/strings.lisp`, `src/vm/primitives.lisp`
- **現状**: `parse-float`はホストCL委譲のスタブ。`parse-integer`なし
- **内容**:
  - `parse-integer string &key start end radix junk-allowed` → (integer, position)
    - `:radix`で基数指定（2〜36）、`:junk-allowed t`で不完全文字列を許容
  - `parse-float string &key start end` → (float, position)
    - IEEE 754準拠の浮動小数点パース（`strtod`相当を純CLで実装）
  - 両者とも2値返却（`values`）: 解析結果＋消費した位置
- **根拠**: ANSI CL 13.2 / SBCL `parse-integer`。`read-from-string`の内部実装
- **難易度**: Medium

#### FR-669: Float Decoding Functions (浮動小数点分解関数)

- **対象**: `src/vm/primitives.lisp`
- **現状**: 浮動小数点の内部構造へのアクセス関数なし
- **内容**:
  - `decode-float float` → (significand, exponent, sign) — 仮数・指数・符号に分解
  - `scale-float float integer` → float × 2^integer — 高速指数スケーリング
  - `float-sign float1 &optional float2` — 符号の取り出し・付与
  - `float-digits float` — 仮数の精度（bit数）
  - `integer-decode-float float` → (mantissa, exponent, sign) — 整数形式
  - `float-radix float` → 基数（常に2）
- **根拠**: ANSI CL 12.1.3。数値アルゴリズム・シリアライゼーションの基盤
- **難易度**: Easy

#### FR-670: Integer Bit Operations (整数ビット操作)

- **対象**: `src/vm/primitives.lisp`
- **現状**: `logand`/`logior`/`logxor`/`lognot`は実装済み。拡張操作なし
- **内容**:
  - `logeqv`/`lognand`/`lognor`/`logandc1`/`logandc2`/`logorc1`/`logorc2` — 完全16論理演算
  - `integer-length integer` — 最上位ビット位置（`(integer-length 4)` → 3）
  - `logcount integer` — 1ビット数（Population Count）
  - `logbitp index integer` — ビット位置の確認
  - `logtest integer1 integer2` — AND結果が0でないかを確認
  - `(setf ldb)` / `(setf dpb)` — ビットフィールド書き込み
- **根拠**: ANSI CL 12.1.3 / SBCL bit operations。ビットマスク・フラグ管理
- **難易度**: Easy

---

### Phase 114 — 正規表現エンジン

#### FR-672: Regular Expression Engine Core (正規表現エンジン中核)

- **対象**: 新ファイル `src/vm/regex.lisp`
- **現状**: 正規表現機能なし。ホストCLの`cl-ppcre`に依存
- **内容**:
  - NFA/DFA変換コンパイラ: 正規表現文字列 → NFA → DFA → VM命令列
  - 基本パターン: `.`/`*`/`+`/`?`/`{n,m}`/`[...]`/`[^...]`/`\d`/`\w`/`\s`/`^`/`$`
  - `scan pattern string &key start end` → (match-start, match-end)
  - `all-matches pattern string` → マッチ位置リスト
  - `regex-replace pattern string replacement` / `regex-replace-all`
  - キャプチャグループ: `(scan "(foo)(bar)" "foobar")` → groups vector
- **根拠**: cl-ppcre / PCRE2 / Python `re` / Java `Pattern`。文字列処理の必須ツール
- **難易度**: Very Hard

#### FR-673: Regex JIT Compilation (正規表現JITコンパイル)

- **対象**: `src/vm/regex.lisp`
- **依存**: FR-672
- **内容**:
  - DFAをVM命令列ではなくx86-64機械語に直接コンパイル（FR-587トレースJITと同機構）
  - 文字クラス判定をSSE4.2の`pcmpestri`/`pcmpistrm`命令にマッピング
  - ホットパターン（同じregexが繰り返し使われる場合）を自動JIT
- **根拠**: PCRE2 JIT / Hyperscan (Intel)。大量テキストスキャンで10〜50倍高速化
- **難易度**: Very Hard

#### FR-674: Unicode-Aware Regex (Unicode対応正規表現)

- **対象**: `src/vm/regex.lisp`
- **依存**: FR-672, FR-590
- **内容**:
  - `\d` → Unicode `Decimal_Number`カテゴリ（ASCIIの0-9のみでなく全Unicode数字）
  - `\w` → Unicode `Letter`/`Number`/`Connector_Punctuation`
  - `\p{Category}` → Unicodeプロパティ指定（`\p{Lu}` = 大文字Letter等）
  - `(?i)` — Unicodeケース非依存マッチング（Turkishの`i`の扱いも正確に）
- **根拠**: PCRE2 Unicode support / Python `re.UNICODE`
- **難易度**: Hard

---

### Phase 115 — ソースロケーション・コンパイラ警告

#### FR-677: Source Location Objects (ソースロケーションオブジェクト)

- **対象**: `src/parse/cl/parser.lisp`, `src/compile/codegen.lisp`, `src/vm/conditions.lisp`
- **現状**: AST ノードにソース位置情報なし。エラーメッセージに行番号が出ない
- **内容**:
  - `source-location` 構造体: `(file, line, column, offset)`
  - パーサがトークン取得時にソース位置を記録
  - AST ノードに `:location` スロットを追加
  - エラー発生時に `condition` に `:source-location` を付与
  - `error at foo.lisp:42:7: unbound variable X` 形式のエラー出力
- **根拠**: SBCL source location / Rust `Span`。プロダクション品質のエラーメッセージの基盤
- **難易度**: Hard

#### FR-678: Compiler Warning Infrastructure (コンパイラ警告インフラ)

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`
- **現状**: コンパイル時警告は標準エラー出力への文字列出力のみ。condition型でない
- **内容**:
  - `compiler-note` / `compiler-warning` / `style-warning` condition型の定義
  - `redefinition-warning` — 既存関数/変数の再定義時
  - `unused-variable-warning` — `declare (ignore ...)` なしで未使用変数
  - `unreachable-code-note` — DCEで除去されたコードの通知
  - `*compiler-warning-policy*` — 警告→エラー変換ポリシー
  - `with-compilation-unit` との統合（複数ファイルの警告を集約して最後に報告）
- **根拠**: SBCL compiler notes / GCC `-Wall`。コード品質向上の基盤
- **難易度**: Medium

#### FR-679: Caret Diagnostics (キャレット診断)

- **対象**: `src/vm/conditions.lisp`, エラー出力系
- **依存**: FR-677, FR-678
- **内容**:
  - Rustスタイルのエラー表示:
    ```
    error: unbound variable X
     --> src/foo.lisp:42:7
      |
    42 |   (+ x undefined-var)
      |          ^^^^^^^^^^^^^ variable not found
    ```
  - ソース行の取得（ファイルキャッシュ or インラインバッファ）
  - 関連する注記（`note: variable X defined here`等）の複数行表示
- **根拠**: Rust / Clang error messages。最高水準のDX
- **難易度**: Hard

---

### Phase 116 — シリアライゼーション・永続化

#### FR-682: make-load-form Protocol

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **現状**: `ansi-cl.md` FR-550でスタブとして言及。定数CLOSインスタンスのFASL格納が不可能
- **内容**:
  - `make-load-form object &optional environment` — オブジェクトの再構築フォームを返す
  - `make-load-form-saving-slots object &key slot-names environment`
  - コンパイル時定数（`defconstant`）にCLOSインスタンスを使用可能にする
  - FASLファイル内のリテラルオブジェクトの格納・復元
- **根拠**: ANSI CL 3.2.4.4 / SBCL `make-load-form`。コンパイル時定数オブジェクトの基盤
- **難易度**: Hard

#### FR-683: Fast Binary Serialization (高速バイナリシリアライゼーション)

- **対象**: 新ファイル `src/runtime/serialize.lisp`
- **内容**:
  - `serialize object stream` — Lispオブジェクトをバイナリ形式でストリームに書き込み
  - `deserialize stream` — バイナリ形式から復元
  - サポート型: fixnum/float/string/symbol/list/vector/hash-table/CLOS-instance
  - 循環参照の保存・復元（`*print-circle*`相当のシリアライズ）
  - FASLキャッシュ（FR-564）の内部フォーマットとして活用
- **根拠**: cl-store / Java Serialization / Python pickle。イメージスナップショット（FR-563）の基盤
- **難易度**: Hard

#### FR-684: JSON Reader/Writer (JSON読み書き)

- **対象**: 新ファイル `src/vm/json.lisp`
- **内容**:
  - `json:parse string` → Lispオブジェクト（object→hash-table, array→vector, string→string, number→number, bool→t/nil）
  - `json:stringify object` → JSON文字列
  - `json:parse-stream stream` / `json:stringify-stream object stream`
  - インクリメンタルパーサ（大容量JSONをストリームから逐次処理）
  - `*json-null*` — `null`の変換先（デフォルト`:null`キーワード）
- **根拠**: JSOWN / cl-json / Shasht。現代のWebサービス・API統合に必須
- **難易度**: Medium

---

### Phase 117 — インタラクティブデバッガ・SLIME統合

#### FR-687: Swank/Slynk Protocol (SLIME/SLY統合プロトコル)

- **対象**: 新ファイル `src/debug/swank.lisp`
- **現状**: デバッガは標準エラー出力へのテキスト出力のみ。エディタ統合なし
- **内容**:
  - Swankプロトコル（S式ベースのRPCプロトコル）のサーバ実装
  - ソケットサーバ起動: `(swank:create-server :port 4005)`
  - 基本操作: `compile-and-load-file`, `compile-string`, `interactive-eval`
  - 自動補完: `completions prefix package`
  - ドキュメント: `describe-symbol`、引数リスト取得: `operator-arglist`
- **根拠**: SLIME swank / SLY slynk。Emacs/VS Code統合の標準プロトコル
- **難易度**: Very Hard

#### FR-688: Object Inspector (オブジェクトインスペクタ)

- **対象**: 新ファイル `src/debug/inspector.lisp`
- **内容**:
  - `inspect object` — オブジェクトの内部構造を対話的に表示
  - CLOS インスタンス: スロット名→値の一覧
  - コンスセル: car/cdr 再帰表示
  - クロージャ: キャプチャ変数・エントリポイント表示
  - ハッシュテーブル: キー/値ペアの表示
  - `*inspected-objects*` — インスペクト履歴
- **根拠**: SBCL inspector / SLIME inspector。ランタイムデバッグの中核ツール
- **難易度**: Medium

#### FR-689: Step Debugger (ステップデバッガ)

- **対象**: `src/vm/vm-run.lisp`, `src/debug/`
- **内容**:
  - `step form` — フォームをステップ実行モードで評価
  - `*step-mode*` — `:into`（関数内に入る）/ `:over`（関数を飛ばす）/ `:out`（関数から出る）
  - VM命令レベルのブレークポイント（FR-543と統合）
  - `*step-condition*` — ステップ発生時のcondition型
- **根拠**: SBCL step / SLDB (SLIME debugger backend)
- **難易度**: Hard

#### FR-690: Backtrace Collection (バックトレース収集)

- **対象**: `src/vm/vm-run.lisp`, `src/vm/conditions.lisp`
- **現状**: エラー時のバックトレース表示なし。呼び出しスタックが不透明
- **内容**:
  - `(backtrace n)` — 最大N個のスタックフレームを収集
  - フレーム情報: 関数名・引数値・ソース位置（FR-677）
  - `(print-backtrace &optional stream n)` — 人間可読形式で出力
  - `(debugger-frame n)` — n番目のフレームへのアクセス
  - `*error-output*` への自動出力（未ハンドルエラー時）
- **根拠**: SBCL `(sb-debug:backtrace)` / Python `traceback`
- **難易度**: Hard

---

### Phase 118 — メモリマップI/O・高度なOS統合

#### FR-693: Memory-Mapped Files (メモリマップトファイル)

- **対象**: `src/runtime/os.lisp` (FR-570)
- **内容**:
  - `(mmap-file path &key protection flags offset length)` — ファイルをアドレス空間にマップ
  - `mmap`/`munmap`/`msync` syscallの薄いラッパ
  - 用途: 大容量FASLファイルのゼロコピー読み込み、イメージスナップショット（FR-563）
  - 読み取り専用マップ（`PROT_READ`）でCOW（Copy-on-Write）ページ共有
- **根拠**: SBCL immobile space / Zig `mmap`。OS管理メモリのゼロコピーアクセス
- **難易度**: Medium

#### FR-694: Executable Memory Management (実行可能メモリ管理)

- **対象**: `src/runtime/runtime.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: JITコード（FR-587）やコールバック（FR-531）の実行可能メモリ確保の仕組みなし
- **内容**:
  - `(alloc-exec-memory size)` — `mmap(PROT_READ|PROT_WRITE|PROT_EXEC)`
  - `(seal-exec-memory ptr size)` — 書き込み禁止にして実行専用に変換（W^X policy）
  - `(free-exec-memory ptr size)` — `munmap`
  - macOS: Apple Silicon の `pthread_jit_write_protect_np` / `MAP_JIT`対応
- **根拠**: V8 CodeAllocator / LLVM JIT ExecutorProcessControl。Apple Silicon JITに必須
- **難易度**: Hard

#### FR-695: Shared Memory IPC (プロセス間共有メモリ)

- **対象**: `src/runtime/os.lisp`
- **内容**:
  - `shm-open name flags mode` / `shm-unlink name` — POSIX共有メモリオブジェクト
  - `ftruncate fd size` — 共有メモリサイズ設定
  - `mmap`（FR-693）との組み合わせで複数プロセスがメモリを共有
  - セマフォ（`sem-open`/`sem-wait`/`sem-post`）との連携
- **根拠**: Erlang `shm` / Python `multiprocessing.shared_memory`。並列コンパイル（FR-585）の基盤
- **難易度**: Hard

---

### Phase 119 — 数値タワー特殊値・定数

#### FR-700: Numeric Limit Constants (数値限界定数)

- **対象**: `src/vm/vm.lisp`, `src/vm/primitives.lisp`
- **現状**: `most-positive-fixnum` 等の定数がホストCL値をそのまま使用。cl-ccのVM固有の値で再定義すべき
- **内容**:
  - Fixnum範囲: `most-positive-fixnum` = 2^62-1 (64bit NaN-boxing)、`most-negative-fixnum` = -2^62
  - Short/Single/Double float limits: `most-positive-single-float` / `most-negative-single-float` / `least-positive-single-float` / `least-negative-single-float`
  - Double: `most-positive-double-float` = 1.7976931348623157e308、`least-positive-double-float` = 5.0e-324
  - Normalized variants: `least-positive-normalized-single-float` / `least-positive-normalized-double-float`
  - Epsilon: `single-float-epsilon` / `double-float-epsilon` / `single-float-negative-epsilon` / `double-float-negative-epsilon`
  - `pi` = `3.141592653589793d0`
  - 特殊float値: `float-infinity` (正/負) / `float-nan`
- **根拠**: ANSI CL 12.1.3.3。数値計算・境界テストの必須定数
- **難易度**: Easy

#### FR-701: Rational Arithmetic Runtime (有理数算術ランタイム)

- **対象**: `src/vm/primitives.lisp`
- **現状**: `3/4` 等の有理数リテラルをパースした後のVM内計算が未実装。ホストCLに委譲
- **内容**:
  - Rational オブジェクト: `(numerator . denominator)` 形式でヒープ割り当て
  - 四則演算: 分母の最小公倍数・最大公約数を使って正規化
  - `(+ 1/3 1/6)` → `1/2`: gcd計算→通分→加算→約分をVM内で完結
  - `(/ 3 4)` → 整数同士の割り算が割り切れない場合に rational を返す
  - `numerator` / `denominator` — アクセサ
  - `rational` / `rationalize` — float → 有理数変換
- **根拠**: ANSI CL 12.1 Numbers。完全な数値タワーの必須要素
- **難易度**: Hard

#### FR-702: Complex Arithmetic Runtime (複素数算術ランタイム)

- **対象**: `src/vm/primitives.lisp`
- **現状**: `#C(1.0 2.0)` パース後のVM内計算が未実装
- **内容**:
  - Complex オブジェクト: `(real-part . imag-part)` のヒープ構造体
  - `+`/`-`/`*`/`/`: 複素数の四則演算をVM内で実装
  - `realpart` / `imagpart` / `conjugate` / `phase` / `abs` (複素絶対値) / `signum`
  - `(complex 1 2)` — 構築。純虚数ゼロの場合に実数へ自動縮退
  - 型特化: `(complex single-float)` vs `(complex integer)`
- **根拠**: ANSI CL 12.1 Numbers / ANSI CL 12.1.5 Complex Numbers
- **難易度**: Hard

---

### Phase 120 — 浮動小数点出力アルゴリズム

#### FR-705: Ryu Float-to-String Algorithm (Ryu最短十進変換)

- **対象**: `src/vm/strings.lisp`, `src/vm/format.lisp`
- **現状**: 浮動小数点の`princ`/`prin1`出力がホストCL委譲。round-trip保証なし
- **内容**:
  - Ryu アルゴリズム (Ulf Adams 2018) の実装: 最短の十進表現で出力
  - `(= (read-from-string (prin1-to-string 1.0d0)) 1.0d0)` が保証される
  - `format ~F`/`~E`/`~G` の内部実装として活用
  - 特殊値の出力: `1.0d+INF`, `-1.0d+INF`, `1.0d+NaN`
  - 処理速度: Dragon4の10〜30倍（固定幅整数演算のみ使用）
- **根拠**: Ryu (PLDI 2018) / Grisu3 / Dragon4。`(read (print x))` = `x` のCL基本保証
- **難易度**: Hard

#### FR-706: Bignum to String (Bignum文字列変換)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - 分割統治法 (divide-and-conquer): `N` を `sqrt(N)` で分割して再帰変換
  - 大基数への変換: `10^k` (kはビット数の半分) を一度計算してキャッシュ
  - `*print-base*` 対応: 2〜36進数への変換
  - 逆変換: `parse-integer` の bignum 対応 (FR-668)
  - O(n²)の単純除算 vs O(n log² n)の分割統治の切り替え閾値 (≥1000桁で高速版)
- **根拠**: GMP mp_get_str / Knuth TAOCP 4.4§。大きな整数の印刷が実用速度になる
- **難易度**: Hard

#### FR-707: Print-Object Default Methods (print-objectデフォルトメソッド)

- **対象**: `src/vm/io.lisp`, `src/vm/vm-clos.lisp`
- **現状**: `print-object` の標準実装がない。`#<HASH-TABLE>` 等が出ない
- **内容**:
  - `#<HASH-TABLE :TEST EQL :COUNT 3 {addr}>` — hash-table の print-object
  - `#<ARRAY (3 4) DOUBLE-FLOAT {addr}>` — 配列の print-object
  - `#<COMPILED-FUNCTION FOO {addr}>` — コンパイル済み関数
  - `#<STREAM type {addr}>` / `#<PACKAGE "CL" {addr}>`
  - `#<CLASS POINT {addr}>` — CLOSクラスオブジェクト
  - `readably-printable-p` — `#.` なしで読み戻せるオブジェクトか判定
- **根拠**: ANSI CL 22.1.3。REPLでの全型の可読出力
- **難易度**: Medium

---

### Phase 121 — 外部フォーマット・ストリームエンコーディング

#### FR-710: Stream External Format (ストリーム外部フォーマット)

- **対象**: `src/vm/io.lisp`, `src/vm/stream.lisp`
- **現状**: ストリームのエンコーディングはホストCL依存。`:external-format`キーワードを無視
- **内容**:
  - `open` / `make-string-input-stream` / `make-string-output-stream` の `:external-format` 対応
  - サポートフォーマット: `:utf-8` / `:latin-1` / `:utf-16` / `:utf-16-le` / `:utf-16-be` / `:ascii`
  - `stream-external-format stream` — 現在のエンコーディングを返す
  - BOMの自動検出: UTF-16ストリームの先頭 `#xFEFF` / `#xFFFE` を解析して自動判定
- **根拠**: ANSI CL 21.2 / SBCL `:external-format`。国際化テキスト処理の基盤
- **難易度**: Hard

#### FR-711: UTF-16 Codec (UTF-16コーデック)

- **対象**: `src/vm/unicode.lisp` (FR-592)
- **依存**: FR-592, FR-710
- **内容**:
  - UTF-16 エンコード: BMP文字は2バイト、補助文字はサロゲートペア（4バイト）
  - UTF-16 デコード: サロゲートペアの U+D800〜U+DFFF から code point 復元
  - `string-to-octets string &key external-format` — string → byte vector
  - `octets-to-string octets &key external-format` — byte vector → string
  - Shift-JIS / EUC-JP (オプション、拡張フォーマットとして)
- **根拠**: Windows API / Java String (UTF-16内部)。Windows FFI・ファイルシステム統合
- **難易度**: Hard

---

### Phase 122 — ターミナル・Readline統合

#### FR-714: Terminal Detection and Control (ターミナル検出・制御)

- **対象**: `src/cli/main.lisp`, `src/runtime/os.lisp`
- **現状**: 出力先がターミナルかファイルかを区別しない。パイプでもカラー出力が出る
- **内容**:
  - `(isatty stream)` — OS の `isatty(fd)` 呼び出し。パイプ/ファイルと TTY を区別
  - `(terminal-size)` — `TIOCGWINSZ` ioctl で `(width . height)` 取得
  - ANSI エスケープシーケンス: `\e[31m` (赤) / `\e[1m` (太字) / `\e[0m` (リセット)
  - `*terminal-colors-enabled*` — isatty が false の場合に自動無効化
  - コンパイラ警告のカラー出力（FR-679 キャレット診断との統合）
- **根拠**: SBCL / GCC / Cargo の自動カラー出力。DXの基本
- **難易度**: Easy

#### FR-715: Readline / Line Editor (ライン入力エディタ)

- **対象**: `src/cli/main.lisp`
- **現状**: REPL 入力が `read-line` のみ。カーソル移動・履歴・補完なし
- **内容**:
  - GNU Readline / libedit へのFFI接続（FR-530）
  - 代替: 純CLでの最小ラインエディタ (Ctrl-A/E/K/U, 左右矢印, バックスペース)
  - 履歴: `~/.cl-cc-history` への永続保存、上下矢印での履歴閲覧
  - Tab補完: 現パッケージのシンボルリストから前方一致補完
- **根拠**: Python `readline` / SBCL readline integration。REPLの基本UX
- **難易度**: Hard

#### FR-716: REPL Completion and Help (REPL補完・ヘルプ)

- **対象**: `src/cli/main.lisp`
- **依存**: FR-715
- **内容**:
  - `(describe sym)` — シンボルの文書・型・値・バインディングを表示
  - `(documentation sym type)` — docstringの取得
  - `(inspect obj)` — FR-688 オブジェクトインスペクタ統合
  - `,` コマンドプレフィックス: `,quit`, `,cd /tmp`, `,load foo.lisp`
  - `(help)` — 組み込みコマンド一覧
- **根拠**: SBCL REPL / Python `help()` / Racket REPL
- **難易度**: Easy

---

### Phase 123 — 動的ライブラリロード・プラグイン

#### FR-719: Dynamic Library Loading (動的ライブラリロード)

- **対象**: 新ファイル `src/ffi/dynlib.lisp`
- **現状**: FFI（FR-530）は静的リンクのみ想定。実行時の動的ライブラリ読み込み不可
- **内容**:
  - `(load-shared-library "libfoo.dylib" &key if-not-found)` — `dlopen` 呼び出し
  - `(find-foreign-symbol "printf" lib)` — `dlsym` でシンボル解決
  - `(unload-shared-library lib)` — `dlclose`
  - デフォルト検索パス: `$DYLD_LIBRARY_PATH` / `$LD_LIBRARY_PATH` / `/usr/lib`
  - macOS framework 対応: `(load-framework "CoreFoundation")`
- **根拠**: CFFI `load-foreign-library` / SBCL `sb-alien`。実行時のCライブラリ統合
- **難易度**: Medium

#### FR-720: Plugin Architecture (プラグインアーキテクチャ)

- **対象**: `src/cli/main.lisp`
- **依存**: FR-719
- **内容**:
  - `~/.cl-cc/plugins/` ディレクトリからの自動プラグインロード
  - プラグインAPI: `(define-cl-cc-plugin :name "my-plugin" :version "1.0" :load-fn #'init)`
  - 組み込み拡張点: REPL コマンド追加、コンパイラパス追加、VM命令追加
  - ASDF `cl-cc-plugin` システム定義テンプレート
- **根拠**: Emacs package.el / Vim plugin system。エコシステム拡張の基盤
- **難易度**: Medium

---

### Phase 124 — 高度なGC型・フィナライゼーション

#### FR-723: Ephemerons (エフェメロン)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **依存**: FR-184（Weak Reference）
- **現状**: FR-184でweak pointerは追加予定だがエフェメロンなし
- **内容**:
  - Ephemeron = `(key . value)` のペア。keyが GC で回収されると value も回収される
  - キー生存 → 値も生存が保証される（weak hash tableのキー/値独立weakとの違い）
  - `make-ephemeron key value` / `ephemeron-key` / `ephemeron-value`
  - GCがephemeronを2フェーズで処理: 通常マーク後にephemeronキーの生存を確認
- **根拠**: SBCL ephemerons / .NET ConditionalWeakTable。CLOS slot-valueのキャッシュに最適
- **難易度**: Hard

#### FR-724: GC Notification Hooks (GC通知フック)

- **対象**: `src/runtime/gc.lisp`
- **内容**:
  - `*before-gc-hooks*` — GC前に呼ばれる関数リスト（ハンドル解放等）
  - `*after-gc-hooks*` — GC後に呼ばれる関数リスト（キャッシュ更新等）
  - `(add-gc-hook :before fn)` / `(remove-gc-hook :before fn)`
  - フックはGCスレッドではなくミューテータスレッドで実行（ファイナライザと同様）
- **根拠**: SBCL `sb-ext:*after-gc-hooks*` / `tg:gc-run-time`
- **難易度**: Easy

#### FR-725: Finalization Queue (フィナライゼーションキュー)

- **対象**: `src/runtime/gc.lisp`
- **依存**: FR-184
- **内容**:
  - フィナライゼーションキュー: GC回収対象オブジェクトをキューに積む
  - 専用フィナライザスレッド（または GC後フック FR-724）がキューを処理
  - `(finalize obj callback)` — objが回収されるときcallbackを呼ぶ
  - `(cancel-finalization obj)` — フィナライザの取り消し
  - ファイルハンドル・外部リソースの自動解放に使用
- **根拠**: Java Cleaner API / Python `__del__` / SBCL `sb-ext:finalize`
- **難易度**: Hard

---

### Phase 125 — CLOSディスパッチキャッシュ最適化

#### FR-728: Effective Method Cache (有効メソッドキャッシュ)

- **対象**: `src/vm/vm-clos.lisp`
- **現状**: `vm-generic-call`が毎回`compute-applicable-methods`相当の処理を実行
- **内容**:
  - 各ジェネリック関数に「引数型タプル → 有効メソッド」のキャッシュを添付
  - `(list (class-of arg1) (class-of arg2) ...)` をキーとしたハッシュテーブル
  - キャッシュヒット時は `compute-effective-method` をスキップ
  - クラス再定義（`defclass`再実行）時にキャッシュを無効化
- **根拠**: SBCL PCL emf cache / CLISP dispatch cache。ジェネリック関数の呼び出しコストを5〜10倍削減
- **難易度**: Hard

#### FR-729: Next-Method Chain Optimization (次メソッド連鎖最適化)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - `call-next-method` の連鎖をコンパイル時に展開（静的に型が確定している場合）
  - `:before`/`:after` メソッドがない場合: primary のみの直接呼び出しにコンパイル
  - メソッド連鎖をベクタとして格納: `next-methods[0]`, `next-methods[1]`, ... でインデックスアクセス
  - `(next-method-p)` → コンパイル時定数に変換可能な場合は最適化
- **根拠**: SBCL PCL `make-fast-method-function`
- **難易度**: Hard

#### FR-730: Generic Function Sealing (ジェネリック関数シール)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - `(seal-generic-function #'foo)` — 新メソッドの追加を禁止し、ディスパッチを静的化
  - シール後: FR-728のキャッシュが永続（無効化不要）、FR-501のPICが永続化
  - コンパイル時にシール済みGFへの呼び出しを直接呼び出しに変換可能
  - `:seal` オプション: `(defgeneric foo (x) (:seal t))`
- **根拠**: Julia method sealing / Swift `@_optimize(speed, size)` protocol witnesses
- **難易度**: Medium

---

### Phase 126 — クロージャ表現最適化

#### FR-733: Mutable Capture Cell (可変キャプチャセル)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **現状**: 可変変数のキャプチャが毎回 `(register . value)` alist 全体を保存。変更時に非効率
- **内容**:
  - 可変キャプチャ変数を `box` オブジェクト（1要素のヒープセル）としてラップ
  - クロージャは `box` へのポインタを保持。変更は `(setf (box-value cell) new-val)`
  - 複数クロージャが同じ `box` を共有することで `(let ((x 0)) (lambda () (incf x)))` が正確に動作
  - 不変キャプチャ（`setq`で書き換えない変数）はboxなしで直接値を保持
- **根拠**: GHC thunk cells / Closure conversion (Appel, 1988)
- **難易度**: Medium

#### FR-734: Cell Lifting / Unboxed Capture (セルリフティング・Unboxedキャプチャ)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **依存**: FR-733
- **内容**:
  - 単一のクロージャからしかアクセスされない可変変数はboxを不要にできる
  - エスケープ解析（FR-007）がクロージャが脱出しないと判明した場合: 値を直接embed
  - `(let ((x 0)) (lambda () (incf x) x))` で `x` が外部に漏れない場合: スタック上の変数
  - 不変キャプチャの型特化: fixnumと確定した変数はunboxed int として保持
- **根拠**: GHC worker/wrapper transformation / ML Kit region inference
- **難易度**: Hard

#### FR-735: Closure Cloning (クロージャクローニング)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - クロージャに定数環境が渡される呼び出しパターンを検出
  - `(mapcar (lambda (x) (+ x k)) list)` で `k` が定数の場合: `k` を埋め込んだ特化クロージャを生成
  - クローニングしきい値: 環境サイズ≤4かつ呼び出し回数≥100
  - FR-583（部分評価）の軽量版として実装
- **根拠**: GHC specialisation / OCaml partial application optimization
- **難易度**: Hard

---

### Phase 127 — 暗号・圧縮原語

#### FR-738: SHA-256 / SHA-512 Implementation

- **対象**: 新ファイル `src/runtime/crypto.lisp`
- **内容**:
  - SHA-256: FIPS 180-4準拠の純CL実装（fixnum演算のみ使用）
  - SHA-512: 64-bit ワードで実装（bignum利用）
  - `(sha256 octets)` → 32バイトのベクタ
  - `(sha512 octets)` → 64バイトのベクタ
  - HMAC-SHA256: `(hmac-sha256 key message)` → 32バイトMAC
  - 用途: FASL整合性チェック（FR-564）、イメージ署名（FR-563）
- **根拠**: FIPS 180-4 / RFC 2104。自己完結ランタイムの安全なハッシュ基盤
- **難易度**: Medium

#### FR-739: Base64 Codec (Base64コーデック)

- **対象**: `src/runtime/crypto.lisp`
- **内容**:
  - `(base64-encode octets)` → 文字列
  - `(base64-decode string)` → byte vector
  - URL-safe variant: `-` と `_` を使用 (RFC 4648 §5)
  - Streaming API: `make-base64-input-stream` / `make-base64-output-stream`
- **根拠**: RFC 4648。HTTPヘッダ・JSON・FASL転送の基本エンコーディング
- **難易度**: Easy

#### FR-740: zlib / Deflate (zlib圧縮)

- **対象**: `src/runtime/compress.lisp`
- **内容**:
  - DEFLATE アルゴリズム (RFC 1951) の実装: LZ77 + Huffman 符号化
  - `(zlib-compress octets &key level)` → 圧縮バイト列 (RFC 1950 zlib wrapper)
  - `(zlib-decompress octets)` → 元バイト列
  - `(gzip-compress octets)` / `(gzip-decompress octets)` — gzip wrapper (RFC 1952)
  - FASL ファイルの圧縮保存（FR-564 増分コンパイルキャッシュとの統合）
- **根拠**: zlib 1.3 / Python `zlib` module。FASLの圧縮で10〜50%サイズ削減
- **難易度**: Hard

---

### Phase 128 — AOT コンパイル・起動最適化

#### FR-743: Whole-Program AOT Compilation (全プログラムAOTコンパイル)

- **対象**: `src/compile/pipeline.lisp`, `src/emit/x86-64-codegen.lisp`
- **現状**: `./cl-cc compile foo.lisp` はVM命令列を生成するが最終的なネイティブ出力は未完成
- **内容**:
  - 全ソースファイルを一度に読み込み、クロスモジュール最適化を適用
  - VM命令列 → x86-64/AArch64 機械語の完全変換（インタープリタを使わない）
  - デッドコード除去: `main` から到達不可能な全関数を除去
  - 標準ライブラリの静的リンク（FR-571の完全版）
  - 出力: 完全スタンドアロン ELF/Mach-O バイナリ
- **根拠**: Go `go build` / Zig AOT / Kotlin Native。インタープリタ不要の本物のネイティブバイナリ
- **難易度**: Very Hard

#### FR-744: Startup Time Optimization (起動時間最適化)

- **対象**: `src/vm/vm.lisp`, `src/compile/pipeline.lisp`
- **現状**: `./cl-cc run foo.lisp` 起動時に全標準ライブラリをコンパイル・ロード
- **内容**:
  - 標準ライブラリを事前コンパイルした FASL イメージとして data セグメントに埋め込む
  - `vm-state` の初期化をコンパイル時に実行し、結果を静的データとして格納
  - `*cold-init-functions*` リスト: 最低限の初期化のみ実行 (Lisp image boot sequence)
  - 目標: 50ms以内のHello World実行（現状 500ms〜2s）
- **根拠**: SBCL `save-lisp-and-die` / GHC `-prof` startup / Go init function chain
- **難易度**: Very Hard

#### FR-745: Dead Code Elimination at Link Time (リンク時デッドコード除去)

- **対象**: `src/compile/pipeline.lisp`, `src/binary/macho.lisp`
- **依存**: FR-743
- **内容**:
  - エントリポイントからの到達可能性解析（関数コールグラフのDFS）
  - 未到達関数・変数を最終バイナリから除外
  - `--dead-code-report` フラグで除去したシンボルリストを出力
  - 標準ライブラリの多くが除去される: "Hello World" バイナリが 1MB 以下に
- **根拠**: ld の `--gc-sections` / Zig `--strip` / Kotlin/Native TreeShaking
- **難易度**: Hard

---

### Phase 129 — 永続データ構造

#### FR-748: Hash Array Mapped Trie (HAMT - 永続ハッシュマップ)

- **対象**: 新ファイル `src/vm/persistent.lisp`
- **内容**:
  - 永続（不変）ハッシュマップ: `assoc`/`dissoc`/`get` が O(log₃₂ n)
  - 構造共有: 変更時に変更パス上のノードだけをコピー（コピーオンライト）
  - `(persistent-map :key1 val1 :key2 val2)` — 構築
  - `(assoc map :new-key new-val)` — 新マップを返す（元は変更なし）
  - Clojure `PersistentHashMap` / Scala `HashMap` と互換アルゴリズム
- **根拠**: Bagwell 2001 HAMT / Clojure persistent collections。並行処理での安全な共有状態
- **難易度**: Hard

#### FR-749: Persistent Vector (永続ベクタ)

- **対象**: `src/vm/persistent.lisp`
- **内容**:
  - RRB-Tree (Relaxed Radix Balanced Tree): O(log₃₂ n) のランダムアクセス・更新・挿入
  - `(pvec 1 2 3)` — 構築
  - `(pvec-get v i)` — インデックスアクセス
  - `(pvec-assoc v i val)` — 新ベクタを返す（元は不変）
  - `(pvec-conj v val)` — 末尾追加 O(log n) amortized
  - Clojure PersistentVector との互換性
- **根拠**: Stucki et al. 2015 RRB-Trees。不変シーケンスの最適データ構造
- **難易度**: Hard

#### FR-750: Lazy Sequences (遅延シーケンス)

- **対象**: `src/vm/persistent.lisp`
- **内容**:
  - `(lazy-seq thunk)` — thunkが遅延評価されるシーケンス
  - `(force lazy-seq)` — 最初の要素を評価して通常のconspairを返す
  - `(take n lazy-seq)` — 最初のn要素を強制評価
  - `(lazy-map fn lazy-seq)` / `(lazy-filter pred lazy-seq)` — 遅延変換
  - 無限シーケンス: `(iterate f init)` → `(init, f(init), f(f(init)), ...)`
  - FR-179 (sequence fusion) の代替: 中間シーケンス自体を作らない
- **根拠**: Clojure lazy-seq / Haskell lazy lists / Python generators
- **難易度**: Medium

---

### Phase 130 — 型指定子ランタイム・型変換

#### FR-753: Type Specifier Runtime (型指定子ランタイム)

- **対象**: `src/vm/vm.lisp`, `src/type/inference.lisp`
- **現状**: `typep` は部分実装。`(satisfies pred)` / `(member ...)` / `(not t)` 等が未対応
- **内容**:
  - 完全型指定子構文:
    - `(integer low high)` — 範囲付き整数型
    - `(float low high)` — 範囲付き浮動小数点
    - `(array element-type dimensions)` — 特化配列型
    - `(member elem1 elem2 ...)` — 有限集合型
    - `(satisfies predicate-name)` — 述語による型
    - `(not t1)` / `(and t1 t2)` / `(or t1 t2)` — 論理型合成
    - `(values t1 t2 &rest ts)` — 多値型
  - `subtypep type1 type2` → (boolean, deterministic-p)
  - `upgrade-array-element-type type` — 最も近い実装型への強制
- **根拠**: ANSI CL 4.3 Type Specifiers。型安全なコードの実行時検証基盤
- **難易度**: Hard

#### FR-754: coerce Runtime (coerceランタイム)

- **対象**: `src/vm/primitives.lisp`
- **現状**: `coerce` がホストCL委譲のスタブ
- **内容**:
  - `(coerce x 'single-float)` — integer/rational → float
  - `(coerce x 'integer)` — float → integer (truncate)
  - `(coerce x 'list)` — vector/string → list
  - `(coerce x 'vector)` — list → simple-vector
  - `(coerce x 'string)` — character/list-of-char → string
  - `(coerce x 'character)` — 1文字の文字列/整数 → character
  - `(coerce x '(complex double-float))` — 複素数型変換
- **根拠**: ANSI CL 12.1.5.2 / 4.3.2 Type Coercion。型変換の統一インターフェース
- **難易度**: Medium

---

### Phase 131 — 関数オブジェクトランタイム

#### FR-757: fdefinition / Function Cell Management

- **対象**: `src/vm/vm.lisp`
- **現状**: 関数定義はハッシュテーブル（`function-registry`）に格納。`fdefinition`/`fmakunbound`なし
- **内容**:
  - `fdefinition name` — 関数名から関数オブジェクトを取得 (`symbol-function` と同等)
  - `(setf fdefinition)` — 関数セルに任意の関数オブジェクトをセット
  - `fmakunbound name` — 関数定義を削除
  - `fboundp name` — 関数が定義済みかを確認
  - `(setf symbol-function)` — `fdefinition`のsetf版との統一
- **根拠**: ANSI CL 5.3。動的な関数再定義・ホットコードリロード（FR-562）の基盤
- **難易度**: Easy

#### FR-758: function-lambda-expression (関数ラムダ式)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `function-lambda-expression function` → `(lambda-expression, closure-p, name)`
  - コンパイル済み関数からソースフォームを取得（デバッグビルド時のみ格納）
  - クロージャの場合 `closure-p` = `t`
  - `--debug`フラグ時: 全関数のソースをハッシュテーブルに保存
- **根拠**: ANSI CL 5.3 / SBCL `function-lambda-expression`。`describe`・デバッガの基盤
- **難易度**: Medium

#### FR-759: Higher-Order Function Optimization (高階関数最適化)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - `(funcall #'foo x)` → `(foo x)` に変換（`#'`は静的に解決可能）
  - `(mapcar #'car list)` → `#'car` をインライン展開
  - `(apply fn args)` で `fn` が定数の場合: `fn` の型に基づいて特化
  - `compose`/`complement`/`constantly` の静的展開
- **根拠**: GHC rule-based rewriting / SBCL compiler transform
- **難易度**: Medium

---

### Phase 132 — Environment API (CLTL2 / コンパイラAPI)

#### FR-762: Lexical Environment API (CLTL2環境API)

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`
- **現状**: レキシカル環境はコンパイラ内部のalistとして管理。外部APIなし
- **内容**:
  - `variable-information symbol env` → `(kind, local-p, alist)` 。kindは `:lexical`/`:special`/`:symbol-macro`/`nil`
  - `function-information name env` → `(kind, local-p, alist)` 。kindは `:function`/`:macro`/`:special-form`/`nil`
  - `declaration-information decl-name env` — `optimize`等の宣言情報
  - `augment-environment env &key variable symbol-macro function macro declare` — 環境の拡張
  - `parse-macro name lambda-list body env` — マクロ展開関数の構築
  - `enclose lambda-expression env` — 環境を閉包した関数オブジェクトを生成
- **根拠**: CLTL2 §8 / Common Lisp the Language 2nd edition。コード変換ツール・マクロの必須API
- **難易度**: Hard

#### FR-763: Compiler Intrinsic Registry (コンパイライントリンシクスレジストリ)

- **対象**: `src/compile/builtin-registry.lisp`
- **現状**: `builtin-registry.lisp`は呼び出し規約のみ。型情報・副作用・純粋性のDB不完全
- **内容**:
  - 各関数に対する完全なアノテーション:
    - `:result-type` — 戻り値型 (`(function (fixnum fixnum) fixnum)`)
    - `:effect-free` — 副作用なし（定数畳み込み可能）
    - `:no-throw` — 例外を投げない（handler-bind省略可）
    - `:commutative` — 引数順序不問（`(+ a b)` → `(+ b a)` が等価）
    - `:associative` — 結合法則（`(+ a (+ b c))` → `(+ (+ a b) c)`）
    - `:inlinable` — インライン展開を推奨
  - `builtin-info symbol` — アノテーション取得
  - FR-183 拡張版（属性セットの大幅拡充）
- **根拠**: LLVM `FunctionAttr` / SBCL `defknown`。コンパイラ最適化の知識データベース
- **難易度**: Medium

---

### Phase 133 — 非同期I/O・イベントループ

#### FR-766: Event Loop Infrastructure (イベントループ基盤)

- **対象**: 新ファイル `src/runtime/event-loop.lisp`
- **内容**:
  - `epoll`（Linux）/ `kqueue`（macOS）を使ったI/O多重化
  - `(event-loop:run)` — シングルスレッドの非同期イベントループ
  - `(event-loop:register-fd fd events callback)` — ファイルディスクリプタの監視登録
  - `(event-loop:set-timeout ms callback)` — タイムアウトコールバック
  - FR-574（ソケット）との統合: accept/read/writeを非同期化
- **根拠**: libuv / Node.js event loop / Python asyncio。非同期サーバの基盤
- **難易度**: Very Hard

#### FR-767: Async/Await Syntax (async/await構文)

- **対象**: `src/expand/macros-stdlib.lisp`
- **依存**: FR-766, FR-552（グリーンスレッド）
- **内容**:
  - `(async (&key name) &body body)` — 非同期タスクを生成（グリーンスレッド上で実行）
  - `(await future)` — Future/Promiseの完了を待つ（現スレッドをサスペンド）
  - `(make-future)` / `(resolve-future f val)` / `(reject-future f err)`
  - `(async-let ((x (await a)) (y (await b))) body)` — 並列await
  - JavaScript `async/await` / Python `asyncio` / Rust `async/await`
- **根拠**: 現代の非同期プログラミングモデルの標準。REPLサーバ・Webサーバに必要
- **難易度**: Very Hard

#### FR-768: Promise / Future (Promise・Future型)

- **対象**: `src/runtime/event-loop.lisp`
- **内容**:
  - `future` 型: 未完了/完了/エラーの3状態
  - `(then future callback)` — 完了時コールバック登録
  - `(catch-error future handler)` — エラー時ハンドラ
  - `(all futures)` — 全Future完了を待つFutureを返す
  - `(race futures)` — 最初に完了したFutureを返す
  - JS Promise / Rust Future / Java CompletableFuture相当
- **根拠**: JavaScript Promise / Rust tokio::join。非同期コードの構成要素
- **難易度**: Hard

---

### Phase 134 — ソフトウェアトランザクショナルメモリ (STM)

#### FR-771: STM Core (STMコア)

- **対象**: 新ファイル `src/runtime/stm.lisp`
- **内容**:
  - `tvar` 型: トランザクショナル変数。`(make-tvar init)` で生成
  - `(tvar-read tv)` / `(tvar-write! tv val)` — トランザクション内での読み書き
  - `(atomically &body)` マクロ — トランザクショナルブロック。全操作をアトミックに実行
  - 実装: ログベース楽観的並行制御 — 読み書きをトランザクションログに記録し、コミット時に競合検出
  - 競合時の自動リトライ（指数バックオフ付き）
- **根拠**: Haskell STM (`Control.Concurrent.STM`) / Clojure `(dosync)` / GHC TVar
- **難易度**: Very Hard

#### FR-772: STM Retry / OrElse

- **対象**: `src/runtime/stm.lisp`
- **依存**: FR-771
- **内容**:
  - `(retry)` — トランザクション内で呼ぶと、読んだ tvar のどれかが変わるまでブロック
  - `(orelse tx1 tx2)` — tx1 が `retry` した場合に tx2 を試みる。両方 retry なら合成してブロック
  - ブロッキングキュー実装例: `(atomically (if (empty? q) (retry) (dequeue! q)))`
  - `(make-tvar-list)` / `(make-tvar-hash)` — コレクション型 tvar
- **根拠**: Harris et al. 2005 "Composable Memory Transactions"。STM の真価はcomposability
- **難易度**: Very Hard

---

### Phase 135 — CSPチャネル・Actorモデル

#### FR-775: CSP Channels (CSPチャネル)

- **対象**: 新ファイル `src/runtime/channel.lisp`
- **依存**: FR-552（グリーンスレッド）
- **内容**:
  - `(make-channel &key buffer-size)` — バッファ付き/なしチャネル
  - `(send! ch val)` — 送信（バッファ満時にブロック）
  - `(recv! ch &optional default)` — 受信（バッファ空時にブロック）
  - `(close-channel ch)` — チャネルをクローズ（受信側は nil/EOF を受け取る）
  - `(select! (ch1 v) body1 ... :default default-body)` — 複数チャネルのノンブロッキング選択
  - パイプライン構築: `(pipeline n in-ch fn out-ch)` — N並列ワーカー
- **根拠**: Go channels / Clojure core.async / Erlang `!` / CSP (Hoare 1978)
- **難易度**: Hard

#### FR-776: Actor Model (Actorモデル)

- **対象**: `src/runtime/channel.lisp`
- **依存**: FR-552
- **内容**:
  - `(spawn-actor fn &rest init-args)` — 独立したメッセージボックスを持つアクターを生成
  - `(send actor msg)` — アクターのメールボックスにメッセージを非同期送信
  - `(receive &body pattern-clauses)` — パターンマッチで受信メッセージを処理
  - `(self)` — 現アクターへの参照
  - `(link actor)` / `(monitor actor)` — アクター監視（Erlang互換）
  - Supervisor ツリー: `(make-supervisor :strategy :one-for-one :children [...])`
- **根拠**: Erlang OTP / Akka / Elixir。フォールトトレラントな並行システムの基盤
- **難易度**: Very Hard

---

### Phase 136 — パターンマッチング

#### FR-779: Structural Pattern Matching (構造的パターンマッチング)

- **対象**: 新ファイル `src/expand/match.lisp`
- **内容**:
  - `(match expr (pattern1 body1) (pattern2 body2) ...)` マクロ
  - パターン種別:
    - リテラル: `42`, `"foo"`, `t`, `nil`
    - 変数束縛: `x` (任意値にマッチして `x` に束縛)
    - ワイルドカード: `_` (マッチするが束縛しない)
    - コンストラクタ: `(cons car-pat cdr-pat)`, `(list p1 p2 p3)`
    - ベクタ: `#(p1 p2 p3)` または `(vector p1 p2 p3)`
    - 型チェック: `(type integer n)` — integer かつ n に束縛
    - ガード: `(when pred pat)` / `(and p1 p2)` / `(or p1 p2)` — 複合パターン
    - ネスト: `(list (cons a b) (vector x y))`
  - 効率的なコンパイル: パターン決定木 (pattern decision tree) による最小分岐
- **根拠**: ML pattern matching / Rust `match` / Scala `case` / Haskell guards
- **難易度**: Hard

#### FR-780: Match Exhaustiveness Checking (網羅性チェック)

- **対象**: `src/expand/match.lisp`
- **依存**: FR-779
- **内容**:
  - `(match x (t body))` — ワイルドカードなしのパターンが型全体を網羅しているかコンパイル時検査
  - 未網羅パターンの警告: `style-warning "match is not exhaustive; missing pattern (integer ...)"` 
  - CLOSクラス階層との統合: 全サブクラスのパターンがあれば網羅と判定
  - 到達不可能パターンの検出と警告
- **根拠**: ML / Haskell / Rust の exhaustiveness analysis
- **難易度**: Hard

---

### Phase 137 — 代数的エフェクト・エフェクトハンドラ

#### FR-783: Effect System Runtime (エフェクトシステムランタイム)

- **対象**: 新ファイル `src/runtime/effects.lisp`
- **内容**:
  - `(define-effect effect-name ((op-name args...) result-type) ...)` — エフェクト定義
  - `(perform effect-name op-name args...)` — エフェクトの発動（条件システムに類似）
  - `(with-effects ((effect-name handler-clause...)) body)` — エフェクトハンドラの設置
  - ハンドラは継続 `k` を受け取り `(resume k value)` で計算を再開可能
  - CL の condition/restart との実装共有（`restart-case` をエフェクトの特殊ケースとして統一）
- **根拠**: Koka language / OCaml 5 effects / Eff language / Frank。例外・I/O・状態・非決定性の統一抽象
- **難易度**: Very Hard

#### FR-784: Effect-based Coroutines (エフェクトベースのコルーチン)

- **対象**: `src/runtime/effects.lisp`
- **依存**: FR-783
- **内容**:
  - `yield`/`next` をエフェクトとして定義し、ジェネレータを実装
  - `(define-generator fib () (yield 0) (yield 1) ...)` → `(next gen)` で逐次取得
  - Python `yield` / JavaScript `function*` と同等の表現力をエフェクトで実現
  - Pythonとの違い: `resume` が任意の値を `yield` 点に注入可能
- **根拠**: Plotkin & Power 2003 Algebraic Operations and Generic Effects
- **難易度**: Very Hard

---

### Phase 138 — 文字列ビルダ・Rope

#### FR-787: String Builder (文字列ビルダ)

- **対象**: 新ファイル `src/vm/string-builder.lisp`
- **現状**: `(concatenate 'string s1 s2 s3)` は毎回コピーが発生。O(n²)の文字列構築
- **内容**:
  - `(make-string-builder &optional initial-capacity)` — 可変バッファ
  - `(string-builder-append! sb value)` — 文字列/文字/数値を O(1) amortized で追加
  - `(string-builder-finish sb)` — 最終文字列を O(n) で生成（1回だけコピー）
  - `(with-string-builder (sb) body)` — スコープ付きビルダ
  - `format nil` の内部実装として活用（現状は不明な中間バッファ挙動）
- **根拠**: Java `StringBuilder` / C++ `std::ostringstream` / Python `io.StringIO`
- **難易度**: Easy

#### FR-788: Rope Data Structure (Rope文字列構造)

- **対象**: `src/vm/string-builder.lisp`
- **内容**:
  - Rope = 文字列の二分木。O(log n) での concat/split、O(k) での substring
  - `(rope-concat r1 r2)` — ルートノードを1つ追加するだけ (O(1))
  - `(rope-split r i)` — 位置 i で分割 O(log n)
  - `(rope-to-string r)` — O(n) で flat string に変換
  - 閾値（例: 1KB以下）の短文字列は通常 string のまま扱う（ハイブリッド）
- **根拠**: SGI rope / Boehm et al. 1995。大規模テキスト編集バッファの標準データ構造
- **難易度**: Medium

---

### Phase 139 — 構造化ロギング・メトリクス

#### FR-791: Structured Logging System (構造化ロギング)

- **対象**: 新ファイル `src/runtime/logging.lisp`
- **内容**:
  - ログレベル: `:trace`/`:debug`/`:info`/`:warn`/`:error`/`:fatal`
  - `(log-info "message" :key1 val1 :key2 val2)` — 構造化フィールド付きログ
  - JSON 出力モード: `{"level":"info","message":"...","key1":"val1","time":"..."}` 形式
  - `*log-level*` — 最小出力レベル（`*log-level*` 以上のみ出力）
  - `*log-output*` — 出力先ストリーム（デフォルト `*error-output*`）
  - `(with-log-context (:request-id id) body)` — コンテキスト付きログ（スレッドローカル）
- **根拠**: log4j / Python logging / Rust tracing。プロダクション運用の基盤
- **難易度**: Easy

#### FR-792: Runtime Metrics (ランタイムメトリクス)

- **対象**: 新ファイル `src/runtime/metrics.lisp`
- **内容**:
  - `(make-counter :name "requests" :labels '(:method :path))` — カウンタ
  - `(increment! counter &rest label-values)` — O(1) アトミックインクリメント
  - `(make-histogram :name "latency" :buckets '(1 5 10 50 100))` — ヒストグラム
  - `(observe! histogram value)` — 観測値を記録
  - `(make-gauge :name "heap-size")` / `(set-gauge! g val)` — ゲージ
  - Prometheus テキストフォーマット出力: `/metrics` エンドポイント（FR-574ソケットと統合）
- **根拠**: Prometheus / StatsD / OpenTelemetry。SREのゴールデンシグナル監視
- **難易度**: Medium

#### FR-793: Hardware Performance Counters (ハードウェアパフォーマンスカウンタ)

- **対象**: `src/runtime/metrics.lisp`
- **内容**:
  - `perf_event_open` syscall ラッパ（Linux）/ `kpc_set_config` (macOS)
  - 計測可能イベント: CPU cycles, instructions, cache-misses, branch-mispredictions, TLB-misses
  - `(with-perf-counters ((:cycles c) (:cache-misses m)) body)` — スコープ計測
  - `(rdtsc)` / `(rdtscp)` — x86-64 タイムスタンプカウンタ直接読み取り
  - JIT コードのサイクル数測定 → FR-503 TFV への正確なコスト情報を提供
- **根拠**: Linux perf_event / PAPI / Intel VTune。プロファイラの精度向上に不可欠
- **難易度**: Hard

---

### Phase 140 — Language Server Protocol (LSP)

#### FR-796: LSP Server Core (LSPサーバ中核)

- **対象**: 新ファイル `src/tools/lsp-server.lisp`
- **内容**:
  - LSP 3.17 プロトコルの JSON-RPC サーバ実装
  - `textDocument/hover` — シンボルのドキュメント・型情報を返す
  - `textDocument/completion` — 補完候補（シンボル名・関数引数リスト）
  - `textDocument/definition` — 定義箇所へのジャンプ (`(defun foo ...)` の位置)
  - `textDocument/publishDiagnostics` — リアルタイムエラー/警告通知
  - `workspace/symbol` — ワークスペース全体のシンボル検索
- **根拠**: LSP 3.17 specification。VS Code / Emacs eglot / Neovim nvim-lspconfig 統合
- **難易度**: Very Hard

#### FR-797: Debug Adapter Protocol (DAP)

- **対象**: 新ファイル `src/tools/dap-server.lisp`
- **依存**: FR-689（ステップデバッガ）
- **内容**:
  - DAP 1.51 プロトコルの JSON-RPC サーバ
  - `setBreakpoints` — ソースファイル・行番号でのブレークポイント設定
  - `stackTrace` / `scopes` / `variables` — 実行状態のインスペクション
  - `continue` / `next` / `stepIn` / `stepOut` — ステップ制御
  - VS Code の Run & Debug パネルから直接 cl-cc プロセスをデバッグ
- **根拠**: DAP specification / LLDB-DAP / Python debugpy。VS Code デバッガ統合の標準
- **難易度**: Very Hard

---

### Phase 141 — 完全継続 (call/cc)

#### FR-800: Full First-Class Continuations (完全第一級継続)

- **対象**: `src/vm/vm-execute.lisp`, `src/compile/cps.lisp`
- **現状**: FR-553 は限定継続（reset/shift）のみ。完全継続（call/cc）は実装されていない
- **内容**:
  - `(call-with-current-continuation fn)` = `(call/cc fn)` — 現在の継続を第一級関数として取り出す
  - 実装戦略: **スタックコピー方式** — 呼び出し時点のVMスタックをヒープにコピー
  - 継続の呼び出し: コピーしたスタックを復元してジャンプ
  - 複数回呼び出し可能（コルーチン・バックトラッキングに利用）
  - CPS 変換済みコードとの統合（継続がすでに明示的なため実装が容易）
- **根拠**: Scheme R7RS / SBCL `call/cc`。非局所脱出・コルーチン・AMB演算子の統一基盤
- **難易度**: Very Hard

#### FR-801: Escape Continuations Fast Path (エスケープ継続高速パス)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-execute.lisp`
- **内容**:
  - `block`/`return-from`/`catch`/`throw` をエスケープ専用継続として最適化
  - エスケープ継続: 脱出専用（複数回呼び出し不可）なのでスタックコピー不要
  - `(block nil (return-from nil val))` → 単純なジャンプ命令に変換
  - FR-800 の完全継続と統一インターフェースだが実装は分岐
- **根拠**: SBCL escape analysis for block/return-from
- **難易度**: Medium

---

### Phase 142 — 衛生的マクロ

#### FR-804: Syntax-Rules / Define-Syntax (衛生的マクロ)

- **対象**: `src/expand/macro.lisp` (新ファイル `src/expand/syntax-rules.lisp`)
- **内容**:
  - `(define-syntax name (syntax-rules (keywords...) (pattern template) ...))` — Scheme R7RS互換
  - パターン変数の衛生性: マクロ展開で導入される変数が呼び出し側の変数と衝突しない
  - `...` エリプシス: ゼロ個以上の要素のパターンマッチ
  - `syntax-case` (R6RS) の部分サポート: 複雑なパターンと条件式
  - 既存の `defmacro` との共存（`defmacro` はデフォルト、衛生性が必要な場合に `define-syntax`）
- **根拠**: Scheme R7RS §4.3 / Kohlbecker et al. 1986 hygienic macros。マクロのコード汚染を排除
- **難易度**: Hard

#### FR-805: Gensym-based Hygiene (gensymベースの衛生性)

- **対象**: `src/expand/macro.lisp`
- **現状**: `defmacro` のマクロでは手動 `gensym` が必要。自動化なし
- **内容**:
  - `(with-gensyms (x y z) body)` — 複数 gensym の一括生成マクロ
  - `(once-only (a b) body)` — 引数の副作用を1回だけ評価するパターン
  - `*gensym-counter*` のスレッドローカル化（FR-663と統合）
  - `make-symbol` vs `gensym` の使い分けガイドライン
- **根拠**: SBCL `with-gensyms` / alexandria `once-only`。実用的なマクロ記述の補助ツール
- **難易度**: Easy

---

### Phase 143 — スクリプトモード・シェバン

#### FR-808: Shebang / Script Mode (シェバン・スクリプトモード)

- **対象**: `src/cli/main.lisp`, `src/parse/cl/lexer.lisp`
- **内容**:
  - `#!/usr/bin/env cl-cc` を含むファイルを `./script.lisp` として実行可能にする
  - レクサが行頭の `#!` を行コメントとして無視（ANSI CL外の拡張）
  - `(cl-cc:main args)` — `main`関数をエントリポイントとして自動呼び出し
  - `--script` フラグ: デバッグ出力を抑制して純粋な stdout のみ
  - パイプライン: `echo "input" | cl-cc --script process.lisp`
- **根拠**: SBCL `--script` / Chicken Scheme `csi -s` / Guile shebang
- **難易度**: Easy

#### FR-809: Command-Line Arguments API (コマンドライン引数API)

- **対象**: `src/cli/main.lisp`
- **内容**:
  - `(cl-cc:argv)` — コマンドライン引数リスト（スクリプト名以降）
  - `(cl-cc:getopt "my-script" '(:flag #\v "verbose" "Enable verbose output") argv)` — GNU getopt互換
  - `--` セパレータ以降は引数として扱う
  - `(cl-cc:exit code)` — 終了コードの指定
  - 環境変数との統合（FR-573）
- **根拠**: SBCL `sb-ext:*posix-argv*` / Python `argparse`
- **難易度**: Easy

---

### Phase 144 — C 埋め込み API

#### FR-812: C Embedding API (C埋め込みAPI)

- **対象**: 新ファイル `src/ffi/embedding.lisp`, `src/ffi/cl-cc.h`
- **内容**:
  - `cl_cc_state_t* cl_cc_init(void)` — VMの初期化
  - `cl_cc_value_t cl_cc_eval(cl_cc_state_t* s, const char* code)` — 文字列評価
  - `cl_cc_value_t cl_cc_call(cl_cc_state_t* s, const char* fn, ...)` — 関数呼び出し
  - `void cl_cc_cleanup(cl_cc_state_t* s)` — リソース解放
  - エラーハンドリング: `cl_cc_error_t`型、`cl_cc_last_error(s)` で詳細取得
  - Cからのコールバック: CL関数をCの関数ポインタとして登録（FR-531と統合）
- **根拠**: Lua `lua_State` / Python `Py_Initialize` / Ruby `ruby_init`。ゲームエンジン・エディタへの組み込み
- **難易度**: Very Hard

#### FR-813: Multiple VM Instances (複数VMインスタンス)

- **対象**: `src/vm/vm.lisp`, `src/ffi/embedding.lisp`
- **依存**: FR-812
- **現状**: `vm-state` はシングルトン前提。並列埋め込みに対応できない
- **内容**:
  - `(make-vm-instance &key parent-env)` — 独立した vm-state を生成
  - 各インスタンスが独自のヒープ・グローバル変数・シンボルテーブルを持つ
  - `parent-env`: 読み取り専用の共有環境（標準ライブラリを共有しつつ独立実行）
  - インスタンス間値の転送: `(transfer-value val from-vm to-vm)` (シリアライゼーション経由)
- **根拠**: Lua `lua_State` (per-instance) / QuickJS `JSRuntime`。セキュアなサンドボックスの基盤
- **難易度**: Very Hard

---

### Phase 145 — メモリプール・アリーナアロケータ

#### FR-816: Arena Allocator (アリーナアロケータ)

- **対象**: `src/runtime/heap.lisp`
- **内容**:
  - `(make-arena &optional size-hint)` — GC管理外の専用アリーナ確保
  - `(arena-alloc arena size)` — アリーナからのバンプポインタ割り当て O(1)
  - `(with-arena (a) &body)` — スコープ終了時にアリーナを一括解放（GC停止なし）
  - 用途: パーサ中間ノード、JSON パース、一時バッファの高速割り当て・一括解放
  - `arena-reset arena` — アリーナを先頭に巻き戻す（メモリ返却なし）
- **根拠**: Apache APR pools / Zig ArenaAllocator / Rust Bump。GCプレッシャーゼロの一時割り当て
- **難易度**: Medium

#### FR-817: Object Pool (オブジェクトプール)

- **対象**: `src/runtime/heap.lisp`
- **内容**:
  - `(make-object-pool type &key min-size max-size)` — 固定サイズオブジェクトのフリーリスト
  - `(pool-acquire pool)` — フリーリストからO(1)取得（なければ新規割り当て）
  - `(pool-release pool obj)` — フリーリストに返却（GCに頼らない）
  - `vm-closure` / `vm-call-frame` 等のホットパスオブジェクトに適用
  - スレッドローカルプール + グローバルプールの2段階構成
- **根拠**: HotSpot TLAB + object pools / Unity C# memory pools。GCの停止頻度を大幅削減
- **難易度**: Medium

---

### Phase 146 — 循環構造印刷・共有構造

#### FR-820: Print-Circle Implementation (*print-circle*実装)

- **対象**: `src/vm/io.lisp`
- **現状**: `*print-circle*` = `t` が未実装。循環リスト印刷で無限ループ
- **内容**:
  - フェーズ1（ラベリング）: DFS で全サブ構造を訪問し、2回以上訪問された構造に `#n=` 番号を付与
  - フェーズ2（印刷）: 初回出現時に `#n=` を付けて印刷、再出現時に `#n#` を出力
  - 循環リスト: `#1=(a b . #1#)` の正確な出力
  - ベクタ・ハッシュテーブル・CLOSインスタンスの循環も検出
  - `*print-circle*` が nil の場合は現状のコード（高速パス）を使用
- **根拠**: ANSI CL 22.1.3 / SBCL `*print-circle*`。デバッグ出力の正確性
- **難易度**: Medium

#### FR-821: Copy-Structure (構造体コピー)

- **対象**: `src/expand/expander-defstruct.lisp`, `src/vm/vm-clos.lisp`
- **内容**:
  - `copy-structure struct` — ANSI CL標準の構造体浅コピー（FR-446の完成版）
  - `deep-copy obj` — cl-cc独自の深コピー（循環参照はFR-820の共有構造検出を活用）
  - CLOS インスタンスのコピー: `(copy-instance obj)` → 全スロットをコピーした新インスタンス
  - `(make-load-form-saving-slots obj)` との連携（FR-682）
- **根拠**: ANSI CL `copy-structure` / SBCL `cl:copy-structure`
- **難易度**: Easy

---

### Phase 147 — 一時的データ構造 (Transients)

#### FR-824: Transient Collections (一時的コレクション)

- **対象**: `src/vm/persistent.lisp`
- **依存**: FR-748（HAMT）, FR-749（永続ベクタ）
- **内容**:
  - `(transient pvec)` — 永続ベクタを一時的可変ベクタに変換（O(1)）
  - `(transient! tv i val)` — インプレース更新（通常の aset と同等、構造共有なし）
  - `(conj! tv val)` — 末尾追加（フィルポインタ更新のみ）
  - `(persistent! tv)` — 一時ベクタを永続に変換（O(1)、以降の変更を禁止）
  - 同様に `(transient pmap)` / `(assoc! tm k v)` / `(dissoc! tm k)` / `(persistent! tm)`
  - バッチ更新のパターン: `(persistent! (reduce #'assoc! (transient {}) pairs))`
- **根拠**: Clojure transients (Hickey 2009)。大量更新時に永続DSの10〜20倍の速度
- **難易度**: Hard

---

### Phase 148 — セキュリティハードニング

#### FR-827: Bounds Checking Mode (バウンドチェックモード)

- **対象**: `src/vm/vm-execute.lisp`, `src/vm/array.lisp`
- **現状**: `aref`/`schar`等の境界チェックが部分的。`(safety 0)`相当で全てスキップ
- **内容**:
  - `*safety-level*` = 0/1/2/3 — ANSI CL `(optimize (safety N))`と連動
  - safety ≥ 1: 配列・文字列のインデックスチェック（`type-error`をシグナル）
  - safety = 3: 全型チェック・全境界チェック・スタックオーバーフロー保護を有効化
  - safety = 0: チェック完全省略（信頼済みコードの最大速度）
  - コンパイル時の`(declare (safety 0))`で局所的に切り替え
- **根拠**: ANSI CL `optimize (safety)` / SBCL safety levels
- **難易度**: Medium

#### FR-828: Stack Canaries (スタックカナリア)

- **対象**: `src/vm/vm-execute.lisp`, `src/runtime/runtime.lisp`
- **内容**:
  - スタックフレーム確立時にカナリア値（ランダム整数）を配置
  - 関数リターン時にカナリア値の一致を確認
  - 不一致検出時に `memory-fault` condition をシグナル
  - `--security-canaries` フラグで有効化（パフォーマンスオーバーヘッド約5%）
  - `SIGILL`/`SIGABRT` でのクラッシュをLispレベルでキャッチ
- **根拠**: GCC/Clang `-fstack-protector-strong`。バッファオーバーフロー検出の基本防御
- **難易度**: Medium

#### FR-829: Integer Overflow Detection (整数オーバーフロー検出)

- **対象**: `src/vm/primitives.lisp`, `src/emit/x86-64-codegen.lisp`
- **内容**:
  - fixnum算術のオーバーフローを x86-64 `jo`（overflow flag）命令で検出
  - オーバーフロー時: fixnum → bignum への自動昇格（`arithmetic-error`を投げない）
  - `(declare (optimize (safety 0)))` で昇格チェックを省略（C言語の `int` 動作）
  - AArch64: `adds`/`subs`命令の overflow flag 利用
- **根拠**: SBCL fixnum overflow → bignum promotion / GCC `-ftrapv`
- **難易度**: Hard

#### FR-830: Taint Tracking (テイントトラッキング)

- **対象**: `src/vm/vm.lisp`, `src/vm/strings.lisp`
- **内容**:
  - 外部入力（ファイル読み込み・ネットワーク受信・環境変数）をテイント済みとマーク
  - テイント済みデータがSQL/シェル/パスに使われる場合に警告
  - `*taint-mode*` フラグで有効化
  - `(untaint str)` — 検証済みとして明示的に解除
  - Ruby の `$SAFE` / Perl の taint mode に類似
- **根拠**: Ruby taint system / Perl taint mode。インジェクション攻撃の検出支援
- **難易度**: Hard

---

### Phase 149 — ランタイム設定・自動チューニング

#### FR-833: GC Tuning Parameters (GCチューニングパラメータ)

- **対象**: `src/runtime/gc.lisp`, `src/runtime/heap.lisp`
- **現状**: GCパラメータがコード中にハードコード（nursery size = 4MB等）
- **内容**:
  - `*gc-nursery-size*` — ナーサリサイズ（デフォルト4MB、変更可能）
  - `*gc-major-threshold*` — メジャーGCトリガー比率（デフォルト0.8）
  - `*gc-tenuring-threshold*` — プロモーション世代数（デフォルト3）
  - `*gc-parallelism*` — 並行GCスレッド数（0=シングルスレッド）
  - 環境変数から読み込み: `CL_CC_GC_NURSERY=8m`
  - `--gc-profile` フラグ: GC統計を収集して最適なパラメータを提案
- **根拠**: JVM `-Xmn`/`-Xmx`/`-XX:MaxGCPauseMillis` / Go `GOGC`
- **難易度**: Easy

#### FR-834: JIT Threshold Configuration (JIT閾値設定)

- **対象**: `src/vm/vm-run.lisp`, `src/compile/pipeline.lisp`
- **内容**:
  - `*jit-tier1-threshold*` — Tier-0 → Tier-1 エスカレートの呼び出し回数（デフォルト100）
  - `*jit-trace-threshold*` — トレース記録開始の閾値（デフォルト100ループ反復）
  - `*jit-inline-size-limit*` — インライン展開する関数の最大命令数（デフォルト30）
  - `*jit-code-cache-size*` — コードキャッシュサイズ（デフォルト256MB）
  - プロファイル誘導自動調整: `--adaptive-jit` フラグで閾値を実行時に最適化
- **根拠**: V8 `--trace-opt` / HotSpot `-XX:CompileThreshold`
- **難易度**: Easy

#### FR-835: Adaptive Runtime Optimization (適応的ランタイム最適化)

- **対象**: `src/vm/vm-run.lisp`, `src/optimize/optimizer.lisp`
- **内容**:
  - GCポーズ時間を監視し、`*gc-nursery-size*`を動的調整（ポーズ > 10ms で縮小）
  - JIT コンパイルキューの優先度: TFVのホット度に基づいて最優先関数を先にコンパイル
  - ヒープ使用率に応じたアロケーションレート制限（GCスラッシングの防止）
  - `(runtime-tuning-report)` — 現在の適応パラメータと推奨値を表示
- **根拠**: GraalVM adaptive compilation / ZGC adaptive heap sizing
- **難易度**: Hard

---

### Phase 150 — 汎用シーケンスプロトコル

#### FR-838: Extensible Sequence Protocol (拡張可能シーケンスプロトコル)

- **対象**: `src/vm/vm-clos.lisp`, `src/expand/macros-stdlib.lisp`
- **内容**:
  - ユーザーが独自のシーケンス型を定義し、`length`/`elt`/`(setf elt)`/`subseq`/`make-sequence-like` を実装することで全シーケンス関数が動作する
  - `sequence` 抽象クラスを `standard-class` の上位に定義
  - `(defclass my-deque (sequence) ...)` → `mapcar`/`find`/`sort`等が自動で動作
  - `elt` / `length` / `make-sequence-like` / `sequence-protocol-p` の基本ジェネリック関数定義
- **根拠**: SBCL extensible sequences (Rhodes 2007)。リングバッファ・双方向リスト等をシームレスに使用
- **難易度**: Hard

#### FR-839: Iteration Protocol (反復プロトコル)

- **対象**: `src/expand/macros-stdlib.lisp`
- **内容**:
  - `(make-iterator sequence)` → iterator オブジェクト
  - `(iterator-next it)` → `(values value has-more-p)`
  - `(doiterator (var sequence) body)` マクロ — 任意のイテラブルを統一的に走査
  - `with-iterator` でカスタムシーケンスが `loop :in` に参加可能
  - FR-838 の拡張可能シーケンスとの統合
- **根拠**: Python iterator protocol / Java Iterable / C++ Range concept
- **難易度**: Medium

---

### Phase 151 — 数値安定性・高精度演算

#### FR-842: Kahan Summation (Kahan加算)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - `(kahan-sum sequence)` — 誤差補正付き浮動小数点加算
  - `make-kahan-accumulator` / `kahan-add! acc val` / `kahan-result acc`
  - `(loop :sum :kahan x :into acc)` — loop マクロの `:kahan` 蓄積クォーリファイア
  - `(pairwise-sum seq)` — 分割統治による数値安定加算 O(n) but O(log n) 誤差増大
- **根拠**: Kahan 1965 / Higham "Accuracy and Stability"。数値計算の基本精度保証
- **難易度**: Easy

#### FR-843: Floating-Point Exception Control (浮動小数点例外制御)

- **対象**: `src/vm/primitives.lisp`, `src/runtime/runtime.lisp`
- **内容**:
  - `(with-float-traps-masked (:divide-by-zero :overflow :underflow :inexact) body)` — FPU例外マスク
  - `(get-float-traps)` — 現在の例外マスク状態を取得
  - x86-64 MXCSR / x87 FCW レジスタへのアクセス
  - `float-features:with-float-features` 互換API
  - `*floating-point-modes*` — デフォルト丸めモード (:round-to-nearest/:round-to-zero/etc.)
- **根拠**: IEEE 754-2019 §9 / SBCL `sb-int:with-float-traps-masked`
- **難易度**: Medium

#### FR-844: Extended Precision Arithmetic (拡張精度演算)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - `double-double` 型: 2つの double の和として128ビット精度を実現（Dekker 1971）
  - `(dd+ a b)` / `(dd* a b)` — double-double 演算
  - `(dd-to-string dd digits)` — 任意精度での文字列変換
  - `(cl-cc:with-precision n body)` — MPFR互換の任意精度演算ブロック
- **根拠**: QD library / GCC `__float128`。科学計算での桁落ち防止
- **難易度**: Hard

---

### Phase 152 — スレッド同期プリミティブ

#### FR-847: Mutex / Condition Variable (ミューテックス・条件変数)

- **対象**: `src/vm/conditions.lisp`, `src/runtime/runtime.lisp`
- **内容**:
  - `(make-mutex &key name)` → mutex オブジェクト
  - `(with-mutex (mutex) body)` — 取得・解放を保証するマクロ（unwind-protect ベース）
  - `(mutex-lock mutex)` / `(mutex-unlock mutex)` — 低レベルAPI
  - `(make-condition-variable &key name)` → CV オブジェクト
  - `(condition-wait cv mutex &key timeout)` — アトミックリリース＆ウェイト
  - `(condition-notify cv)` / `(condition-notify-all cv)` — シグナル送信
  - Pthread マッピング: `pthread_mutex_t` + `pthread_cond_t` (FFI経由)
  - ロック階層 (`*lock-order*`) によるデッドロック検出（デバッグモード）
- **根拠**: POSIX Threads / Bordeaux-Threads API。CL標準ではない実装依存機能だが全メジャー処理系が提供
- **難易度**: Medium

#### FR-848: Semaphore / Read-Write Lock (セマフォ・RWロック)

- **対象**: `src/vm/conditions.lisp`
- **内容**:
  - `(make-semaphore &key (count 0) name)` → semaphore オブジェクト
  - `(semaphore-wait sem &key timeout)` — P操作（カウントをデクリメント、0なら待機）
  - `(semaphore-signal sem &optional (n 1))` — V操作（カウントをインクリメント）
  - `(make-rwlock)` → rwlock オブジェクト
  - `(with-read-lock (rwlock) body)` / `(with-write-lock (rwlock) body)`
  - Reader-writer fairness: writer-prefer / reader-prefer ポリシー選択
  - `rwlock-read-count` / `rwlock-write-waiting` — モニタリング
- **根拠**: 並行読み取りを許可するRWロックはDB接続プール等で必須
- **難易度**: Medium

---

### Phase 153 — ネットワーキング基盤

#### FR-851: TCP/UDP Socket API (ソケットAPI)

- **対象**: `src/vm/io.lisp`, `src/runtime/runtime.lisp`
- **内容**:
  - `(make-tcp-socket)` / `(make-udp-socket)` → socket オブジェクト
  - `(socket-connect sock host port)` — TCP接続
  - `(socket-bind sock host port)` / `(socket-listen sock backlog)` — サーバーソケット
  - `(socket-accept sock)` → client-socket
  - `(socket-send sock data &key start end)` / `(socket-receive sock buffer)`
  - `(socket-close sock)` / `(with-socket (sock ...) body)`
  - `(make-socket-stream sock)` → CL stream (Gray Streams 互換)
  - TCP オプション: `SO_REUSEADDR`, `TCP_NODELAY`, `SO_KEEPALIVE`
  - IPv4/IPv6 デュアルスタック対応
  - syscall マッピング: `socket(2)`, `connect(2)`, `bind(2)`, `listen(2)`, `accept(2)`, `send(2)`, `recv(2)`, `close(2)`
- **根拠**: ネットワーク対応は現代ランタイムの必須機能。SBCL には `sb-bsd-sockets` が存在
- **難易度**: Medium

#### FR-852: DNS Resolution (DNS解決)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `(dns-resolve host)` → list of IP strings (A/AAAA レコード)
  - `(dns-reverse-resolve ip)` → hostname string (PTR レコード)
  - `(getaddrinfo host service)` → addrinfo 構造体リスト (POSIX `getaddrinfo(3)` ラッパー)
  - `/etc/resolv.conf` 読み込みによるシステムDNS設定
  - キャッシュ: TTL付きDNSキャッシュ、`*dns-cache-ttl*` 変数
  - 非同期DNS解決 (`dns-resolve-async host callback`)
- **根拠**: ホスト名解決はソケットAPIの実用上必須
- **難易度**: Easy

#### FR-853: TLS/SSL サポート

- **対象**: `src/vm/io.lisp`
- **内容**:
  - OpenSSL/LibreSSL をFFI経由でラップ: `SSL_new`, `SSL_connect`, `SSL_read`, `SSL_write`
  - `(make-tls-context &key verify-peer ca-bundle)` → TLS context
  - `(tls-wrap-socket sock context)` → tls-socket
  - `(tls-socket-stream tls-sock)` → CL stream
  - Server-side: `(tls-server-context cert-file key-file)` → server context
  - SNI (Server Name Indication) サポート
  - TLS 1.2 / 1.3 プロトコル選択
  - 証明書検証: PEM バンドル, システム証明書ストア
- **根拠**: HTTPS/wss:// は現代アプリケーションの基本インフラ
- **難易度**: Hard

---

### Phase 154 — 遅延評価プリミティブ

#### FR-856: Promises — delay/force (遅延評価)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/vm.lisp`
- **内容**:
  - `(delay expr)` → promise オブジェクト（`force` 呼び出しまで評価を延期）
  - `(force promise)` — 評価を強制し結果をキャッシュ（idempotent）
  - `(promisep x)` → boolean
  - スレッドセーフ `force`: double-checked locking パターン
  - `(make-promise value)` — 既に解決済み promise の作成
  - `(delay-force expr)` — 末尾再帰的 force 合成（trampoline、SRFI-45互換）
  - lazy list: `(lazy-cons head tail-promise)` / `(lazy-car lc)` / `(lazy-cdr lc)`
  - `(lazy-take n lazy-seq)` → list 最初の N 要素を強制評価
- **根拠**: R5RS `delay`/`force` / SRFI-45。遅延リストは無限シーケンスの基本実装手法
- **難易度**: Easy

#### FR-857: Memoization (メモ化)

- **対象**: `src/expand/macros-stdlib.lisp`
- **内容**:
  - `(memoize fn &key (test #'equal) (size 1024))` → memoized-fn
  - `(defun/memo name args body)` — メモ化defun ショートハンド
  - キャッシュ実装: `equal` ハッシュテーブル（引数タプルをキーに）
  - キャッシュクリア: `(memoize-clear memoized-fn)`
  - 統計: `(memoize-stats memoized-fn)` → `(:hits N :misses N :size N)`
  - 弱参照キャッシュオプション: `(:weak-values t)` でGC回収可能
  - スレッドセーフオプション: `(:thread-safe t)` でmutex保護
- **根拠**: 動的計画法・再帰的計算の性能改善パターン。Haskell `Data.Map.Lazy` / Python `functools.lru_cache`
- **難易度**: Easy

---

### Phase 155 — 数値演算ディスパッチ最適化

#### FR-860: Numeric Contagion Rules (数値伝播規則)

- **対象**: `src/vm/primitives.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - ANSI CL §12.1.4 "Floating-point Contagion" の完全実装
  - 伝播階層: `(integer < rational < single-float < double-float < complex)`
  - `(+ fixnum float)` → `float` の自動昇格
  - `(+ fixnum fixnum)` → オーバーフロー時 bignum 昇格
  - 伝播テーブル: `*numeric-contagion-table*` — (type × type) → result-type マトリックス
  - コンパイル時推論: 両オペランドの型が既知なら静的ディスパッチ（分岐不要）
  - 実行時ディスパッチ: `vm-arith-dispatch` — NaN-boxed タグで O(1) 型選択
- **根拠**: ANSI CL 数値タワーのコア仕様。標準準拠と性能の両立
- **難易度**: Medium

#### FR-861: Inline Numeric Dispatch Table (インライン数値ディスパッチ)

- **対象**: `src/compile/codegen.lisp`, `src/vm/primitives.lisp`
- **内容**:
  - `+` / `-` / `*` / `/` の各演算子に `*arith-dispatch-table*` ベクタ
  - エントリ: `(fixnum×fixnum . %ff+)` / `(fixnum×float . %fi+)` / `(float×float . %dd+)` etc.
  - ビット操作で型タグを抽出 → テーブルインデックス → 直接ジャンプ（typecase より高速）
  - ホットパス: fixnum×fixnum → インライン加算（2命令、ブランチなし）
  - `declare (type fixnum x y)` で静的パス選択
- **根拠**: LuaJIT / V8 の型スペシャライズドパス。数値集約コードで3-5x高速化
- **難易度**: Medium

---

### Phase 156 — Multiple Values ABI 最適化

#### FR-864: Multiple Values Frame Protocol (多値フレームプロトコル)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-execute.lisp`
- **内容**:
  - 現状: `vm-values` が `(list val1 val2 ...)` でラップ → `vm-multiple-value-bind` でアンパック
  - 最適化: VM call frame に `mv-buffer[N]` スロット追加 — ヒープアロケーションゼロ
  - `*maximum-multiple-values*` (64) — バッファ上限
  - `vm-nth-value` 命令: `(nth-value n form)` のコンパイル → `nth-value-reg` ← `mv-buffer[n]`
  - `(values)` — `mv-buffer[0]` に `nil` を書き込み、`mv-count` = 0
  - `(values-list list)` — リストをバッファに展開
  - `(multiple-value-call #'fn form)` — バッファをそのまま `fn` の引数として渡す
- **根拠**: ANSI CL §5.3。現状のリストアロケーションは多値多用コードでGC負荷が大
- **難易度**: Hard

#### FR-865: Multiple Values Through apply/funcall (applyを通じた多値伝播)

- **対象**: `src/vm/vm-execute.lisp`
- **内容**:
  - `(multiple-value-call fn form1 form2)` — 各 form の多値を連結して fn を呼出
  - `(multiple-value-list form)` — mv-buffer → list 変換（唯一の合法的ヒープアロケーション）
  - `(receive-multiple-values (a b c) form)` — バッファから直接レジスタへ展開
  - TCO (末尾呼び出し最適化) を通じた多値の透過伝播
  - `mv-prog1`: 最初の form の多値を保存し後続形式を実行後に復元
  - `nth-value` の O(1) アクセス: バッファインデックス直接参照
- **根拠**: `(multiple-value-bind (a b) (floor x y) ...)` は Common Lisp の慣用句。性能が重要
- **難易度**: Medium

---

### Phase 157 — ランダムアクセスI/O

#### FR-868: file-position / Random Access (ファイルランダムアクセス)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `(file-position stream)` → integer (現在位置) または `nil`
  - `(file-position stream position)` → boolean (成功/失敗)
  - `:start` / `:end` / `:current` シンボルによるシーク基準
  - `(file-length stream)` → integer (バイト数)
  - バイナリストリームでの `(read-byte)`/`(write-byte)` との組み合わせ
  - `(with-binary-file (stream path :direction :io) body)` — 読み書き両用オープン
  - syscall マッピング: `lseek(2)` (`SEEK_SET`=0, `SEEK_CUR`=1, `SEEK_END`=2)
  - ファイルマッピング: `mmap(2)` を使った大規模ファイルのゼロコピーアクセス
- **根拠**: ANSI CL §21.2 `file-position`。データベースや大規模バイナリ処理の基礎
- **難易度**: Easy

#### FR-869: Memory-Mapped Files (メモリマップトファイル)

- **対象**: `src/vm/io.lisp`, `src/runtime/runtime.lisp`
- **内容**:
  - `(mmap-file path &key (protection :read) (flags :private))` → mmap オブジェクト
  - `(mmap-array mmap element-type)` → 直接アクセス可能な displaced array
  - `(mmap-close mm)` — `munmap(2)` 呼び出し
  - `(with-mmap (mm path) body)` — 自動クリーンアップ
  - 保護フラグ: `:read` / `:read-write` / `:exec`
  - `(mmap-sync mm &key (async t))` — `msync(2)` によるディスク同期
  - `(mmap-advice mm advice)` — `madvise(2)`: `:normal`/`:sequential`/`:random`/`:willneed`/`:dontneed`
- **根拠**: FASL の需要ページング、大規模コーパス処理、共有メモリ IPC の基盤
- **難易度**: Medium

---

### Phase 158 — Copy-on-Write 文字列・配列

#### FR-872: CoW String Semantics (CoW文字列)

- **対象**: `src/vm/strings.lisp`, `src/vm/vm.lisp`
- **内容**:
  - 文字列ヘッダに `cow-flag` ビット追加（NaN-boxing の上位ビット活用）
  - `(string-copy s)` → 内部的には参照コピー（shallow）、書き込み時にのみ deep copy
  - `(subseq string start end)` — displaced string として実装（コピー不要）
  - `displaced-string-p` / `string-cow-copy` — 内部プリミティブ
  - write 時 CoW トリガー: `(setf (char string i) c)` が shared 文字列に対して呼ばれた際にコピー
  - `string-freeze` / `string-unfreeze` — 不変フラグの明示的制御
- **根拠**: Java `String.intern()` / Python `str` immutability。大量の substring 操作でコピーコストゼロ
- **難易度**: Medium

#### FR-873: CoW Array / Bit-Vector (CoW配列・ビットベクタ)

- **対象**: `src/vm/vm.lisp`
- **内容**:
  - `(make-array n :initial-contents src :copy-on-write t)` — structural sharing
  - `displaced-to` + `displaced-index-offset` の完全実装（ANSI CL §15.1.2.4）
  - ビットベクタの CoW: `(bit-vector-copy bv)` → shared bv、書き込み時にのみコピー
  - `(array-displacement array)` → `(values displaced-to offset)` or `(values nil nil)`
  - `(array-total-size array)` / `(adjustable-array-p array)` — ANSI準拠
  - `adjust-array` による CoW 解消
- **根拠**: 配列スライスの参照セマンティクス。NumPy の `view` に相当する操作
- **難易度**: Medium

---

### Phase 159 — グリーンスレッド スタック管理

#### FR-876: Segmented Stack (セグメント化スタック)

- **対象**: `src/runtime/runtime.lisp`, `src/vm/vm.lisp`
- **内容**:
  - デフォルトスタックサイズ: 8KB セグメント（mmap）
  - スタック上限チェック: 各関数エントリで `sp < stack-limit` → スタック拡張
  - セグメントリンクリスト: `stack-segment` 構造体 `(base size next prev)`
  - スタック拡張: `(grow-stack-segment current-seg)` — 新セグメントを mmap し連結
  - スタック縮小: フレームリターン時に空セグメントを free または pool に戻す
  - `*stack-segment-pool*` — 再利用プール（mmap/munmap コスト削減）
  - Go goroutine コピースタック方式との比較: セグメント方式 vs copying 方式
- **根拠**: Go 1.3以前のセグメントスタック / Rust async stack。軽量スレッドの基盤
- **難易度**: Hard

#### FR-877: Copying Stack Growth (コピー式スタック拡張)

- **対象**: `src/runtime/runtime.lisp`
- **内容**:
  - スタックオーバーフロー検出: guard page (mprotect) + SIGSEGV ハンドラ
  - コピー式拡張: 2倍サイズの新スタックを mmap → 全フレームをコピー → ポインタ修正
  - `(relocate-stack-pointers old-base new-base size)` — スタック内ポインタのリベース
  - `frame-pointer-chain` の追跡: RBP チェーンを辿って全フレームの保存済みRBPを修正
  - Go 1.4+ 方式: セグメント方式より空間効率良好（ホットスプリット問題なし）
  - `*initial-stack-size*` (16KB) / `*max-stack-size*` (64MB)
- **根拠**: Go コピースタック。スタックが小さく始まりオーバーフロー時に透過的拡張
- **難易度**: Very Hard

---

### Phase 160 — カスタムハッシュテーブル

#### FR-880: User-Defined Hash/Test Functions (ユーザー定義ハッシュ関数)

- **対象**: `src/vm/hash.lisp`
- **内容**:
  - `(make-hash-table :test #'my-equal :hash-function #'my-hash)` — カスタム述語+ハッシュ関数
  - `sxhash` の完全実装: list / vector / string / symbol / number の再帰的ハッシュ
  - `(define-hash-table-test name test-fn hash-fn)` — 新しい `:test` 名前の登録
  - 登録済みテスト: `equal` / `equalp` に加えユーザー定義テストを `*hash-table-tests*` に格納
  - `(hash-table-test ht)` / `(hash-table-hash-function ht)` — 現在の設定取得
  - タイプ別最適ハッシュ: fixnum は identity hash、string は FNV-1a/xxHash
- **根拠**: SBCL `sb-ext:define-hash-table-test` / CLISP `ext:hash-table-hash-function`。複合キーの辞書に必須
- **難易度**: Medium

#### FR-881: Hash Table Resizing Policy (ハッシュテーブルリサイズポリシー)

- **対象**: `src/vm/hash.lisp`
- **内容**:
  - `(make-hash-table :rehash-size 1.5 :rehash-threshold 0.75)` — ANSI CL準拠パラメータ
  - `:rehash-size` が整数なら絶対増加量、浮動小数点なら乗算係数
  - `(hash-table-rehash-size ht)` / `(hash-table-rehash-threshold ht)` アクセサ
  - Robin Hood hashing による負荷率向上（0.9 まで効率的）
  - 縮小ポリシー: `*hash-table-shrink-threshold*` 以下になった場合の自動縮小
  - `(clrhash ht)` — 内容クリア（テーブル構造は保持）
- **根拠**: ANSI CL §18.1。`make-hash-table` の全パラメータ準拠
- **難易度**: Easy

---

### Phase 161 — floor/truncate/round 完全実装

#### FR-884: floor/ceiling/truncate/round 二値返却 (ANSI §12.2)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - `(floor number &optional divisor)` → `(values quotient remainder)`
  - `(ceiling number &optional divisor)` → `(values quotient remainder)`
  - `(truncate number &optional divisor)` → `(values quotient remainder)`
  - `(round number &optional divisor)` → `(values quotient remainder)` (banker's round)
  - 単引数: `divisor` = 1、`quotient` が整数、`remainder` が `number - quotient`
  - 二引数: `quotient = (floor (/ number divisor))` 等、`remainder = number - quotient*divisor`
  - 数値型別特殊化: fixnum / float / rational / bignum それぞれの高速パス
  - `(mod a b)` = `(nth-value 1 (floor a b))` として実装
  - `(rem a b)` = `(nth-value 1 (truncate a b))` として実装
  - `mod` vs `rem` の符号規則の相違: floor ベース vs truncate ベース
- **根拠**: ANSI CL §12.2。多くの数値アルゴリズムで floor/truncate の2値戻りが必要
- **難易度**: Easy

#### FR-885: ffloor / fceiling / ftruncate / fround (浮動小数点版)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - `(ffloor number &optional divisor)` → `(values float-quotient remainder)`
  - `(fceiling ...)` / `(ftruncate ...)` / `(fround ...)` — 同様
  - `floor` との違い: quotient が整数でなく **float** として返る
  - 実装: C の `floor()` / `ceil()` / `trunc()` / `rint()` への直接マッピング
  - コンパイラ最適化: `(declare (type double-float x))` があれば SSE2 `roundsd` 1命令
- **根拠**: ANSI CL §12.2.2。float→float の演算が必要な数値ルーティンで使用
- **難易度**: Easy

---

### Phase 162 — CLOS make-instance 高速パス

#### FR-888: allocate-instance Fast Path (高速インスタンス割当)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - 現状: `make-instance` → `allocate-instance` → hash-table ベースのスロット保持
  - 最適化1: クラスが固定レイアウト（インスタンス変数リスト変化なし）なら vector ベースストレージ
  - スロットインデックスキャッシュ: `class-slot-vector-index` — シンボル→整数インデックス
  - `(allocate-instance-vector class)` — `(make-array n)` 1回で全スロット確保
  - `(slot-value-by-index instance i)` / `(setf (slot-value-by-index instance i) val)` — O(1)
  - 型タグ: vector ヘッダに class-id を埋め込み（`typep` / `class-of` を O(1) に）
- **根拠**: SBCL の PCL インスタンス実装。ハッシュテーブルより vector が 3-5x 高速
- **難易度**: Hard

#### FR-889: default-initargs / initialize-instance Caching (初期化キャッシュ)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - `default-initargs` の事前計算: `class-finalize` 時にデフォルトイニットフォームを評価済みリストにキャッシュ
  - `(make-instance class :x 1)` — キャッシュ済みデフォルトとマージした initargs リストを1回のパスで構築
  - `shared-initialize` のインライン化: 標準的なクラスでは `initialize-instance` → `shared-initialize` のメソッドディスパッチをバイパス
  - `(make-instance 'foo)` ショートカット: 引数ゼロならテンプレートコピー（最速パス）
  - `make-instance` キャッシュ: `(class . initargs-signature)` → 特殊化コードのキャッシュ（SBCL の Constructor Caching に相当）
- **根拠**: SBCL constructor caching。`make-instance` はアプリケーションのホットパスになりやすい
- **難易度**: Hard

---

### Phase 163 — load-time-value ランタイムセマンティクス

#### FR-892: load-time-value Implementation (load-time-value実装)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **内容**:
  - `(load-time-value form &optional read-only-p)` — ANSI CL §3.2.2.2
  - コンパイル時: form を切り出してロード時実行リストに追加
  - ロード時: ファイルロード完了直前に各 load-time-value form を評価
  - 結果をグローバルセルに格納 → 実行時は定数参照と同等速度
  - `read-only-p = t`: コンパイラが fold 可能（副作用なしの保証）
  - `*load-time-value-cells*` — ロード中の未解決セルリスト
  - FASL シリアライゼーション: セルIDと評価コードをFASLに埋め込み
- **根拠**: ANSI CL §3.8 `load-time-value`。コンパイル時計算をロード時に遅延評価する仕組み
- **難易度**: Medium

---

### Phase 164 — Symbol Table 最適化

#### FR-895: Symbol Table Compaction (シンボルテーブル圧縮)

- **対象**: `src/vm/vm.lisp`
- **内容**:
  - 現状: `*symbol-table*` はハッシュテーブル（symbol-name → symbol）
  - 最適化: perfect hash（`gperf` / MPHF）によるコンパイル時シンボルテーブル凍結
  - `(freeze-symbol-table)` — 現在のシンボル集合から MPHF を生成し読み取り専用テーブルに変換
  - `(thaw-symbol-table)` — 新しいシンボル追加が必要な場合の動的テーブルへの切り替え
  - 弱参照シンボル: 未参照シンボルをGCで回収（`make-weak-hash-table` 使用）
  - `(symbol-index sym)` — 連番インデックス（プロファイラやデバッガ用）
- **根拠**: V8 の Symbol table / SBCL の package internals。シンボル集約コードでのハッシュ衝突削減
- **難易度**: Medium

#### FR-896: Package Lock / Sealed Package (パッケージロック)

- **対象**: `src/vm/vm.lisp`, `src/expand/expander.lisp`
- **内容**:
  - `(lock-package package)` — パッケージ内シンボルの追加・削除・再定義を禁止
  - `(package-locked-p package)` → boolean
  - ロックされたパッケージへの `intern` → `package-locked-error` condition
  - `(with-unlocked-packages (pkg1 pkg2) body)` — 一時的なロック解除
  - `*locked-packages*` — デフォルトでロックされるパッケージリスト（`:cl`など）
  - SBCL の `sb-ext:lock-package` との互換性
- **根拠**: 標準パッケージの意図しない破壊的変更を防ぐ安全機構
- **難易度**: Easy

---

### Phase 165 — FASL 需要ページング

#### FR-899: Demand-Paged FASL Loading (FASL需要ロード)

- **対象**: `src/vm/vm.lisp`, `src/cli/main.lisp`
- **内容**:
  - FASL ファイルをメモリマップ（`mmap`）してページング単位でロード
  - `fasl-toc` (Table of Contents): FASL先頭にトップレベルフォームのオフセット表を配置
  - 遅延デシリアライズ: `require` / `(load fasl)` 時は TOC のみ読み込み
  - フォーム実行時に初めて対応ページを fault-in（OS のページフォルト機構利用）
  - `*fasl-preload-forms*` — プリロードすべき最小フォームセット（import/defpackage等）
  - 差分再コンパイル: ソースのチェックサムが変わったフォームのみ再コンパイル
- **根拠**: SBCL の incremental FASL / Racket の bytecode persistence。大型ライブラリの起動時間短縮
- **難易度**: Hard

---

### Phase 166 — PGO データ永続化

#### FR-902: PGO Profile Persistence (PGOプロファイルデータ永続化)

- **対象**: `src/vm/vm.lisp`, `src/cli/main.lisp`
- **内容**:
  - プロファイルデータのシリアライズ: `(save-pgo-data path)` → MessagePack or CBOR 形式
  - デシリアライズ: `(load-pgo-data path)` — 関数名 + バージョンハッシュで照合
  - データ形式: `{function-id: u64, call-count: u64, type-feedback: [(site-id, type-vec)], branch-bias: [(site-id, taken-ratio)]}`
  - バージョン管理: ソースハッシュが異なる場合は古いプロファイルを自動廃棄
  - `*pgo-data-path*` — デフォルトパス (`~/.cache/cl-cc/pgo/`)
  - `./cl-cc run --pgo-profile foo.pgo` — プロファイルを使ってコンパイル
  - インクリメンタル更新: 新しい実行データを既存プロファイルにマージ（指数移動平均）
- **根拠**: LLVM Clang の `-fprofile-instr-generate` / V8 の profile-guided optimization。実行間でプロファイルを保持
- **難易度**: Medium

---

### Phase 167 — 末尾呼び出しと例外ハンドラの相互作用

#### FR-905: TCO through unwind-protect (unwind-protect越しのTCO)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-execute.lisp`
- **内容**:
  - ANSI CL §5.3.3: `unwind-protect` の cleanup form は末尾位置でも TCO 不可
  - 現状確認: `(unwind-protect (tail-call) cleanup)` が誤ってTCOされないことの保証
  - `unwind-protect` フレームをスタックに積み、cleanup 実行後にフレームを除去
  - `handler-case` / `handler-bind` を通じた末尾呼び出しの適切な処理
  - `(restart-case (tail-call) ...)` — restart フレームをポップしてからTCO
  - Dynamic extent チェック: 動的スコープのクリーンアップが全て完了してから末尾ジャンプ
- **根拠**: ANSI CL 準拠の条件システムとTCOの正確な相互作用
- **難易度**: Hard

---

### Phase 168 — バックquote コンパイル最適化

#### FR-908: Quasiquote Compiler Optimization (準引用コンパイル最適化)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - 静的部分の定数折りたたみ: `` `(a b ,@nil c) `` → `(list 'a 'b 'c)` に最適化
  - 単一スプライスの特殊化: `` `(,@list) `` → `(copy-list list)` または直接返却
  - ネストしたバックquote の深さトラッキング: `%bq-depth` カウンタ
  - `` `,@x `` at depth=1 → `x` に直接展開（listラップなし）
  - バックquote → `list*` / `cons` / `append` への最適マッピング
  - コンパイル時定数検出: `,` の引数がリテラルなら `quote` 形式に変換
  - 出力: `(list 'a b 'c)` 形式（最短表現）に正規化
- **根拠**: SBCL quasiquote compiler。マクロ多用コードでの `list`/`cons` アロケーション削減
- **難易度**: Medium

---

### Phase 169 — RISC-V バックエンド

#### FR-911: RISC-V 64-bit Backend (RISC-V 64ビットバックエンド)

- **対象**: `src/backend/riscv64.lisp` (新規), `src/emit/target.lisp`
- **内容**:
  - RV64GC (G=IMAFD, C=圧縮命令) の命令エンコーディング
  - レジスタ規約: `ra`(x1), `sp`(x2), `a0`-`a7`(x10-x17 引数), `t0`-`t6`(一時), `s0`-`s11`(保存)
  - 基本整数命令: R型(add/sub/and/or/xor)、I型(addi/ld/lw)、S型(sd/sw)、B型(beq/bne/blt)
  - 関数呼び出し規約: `jal ra, offset` / `jalr zero, 0(ra)` (return)
  - 即値ロード: `lui` + `addi` による32ビット定数、`auipc` + `jalr` による位置独立コード
  - 圧縮命令(RVC): `c.addi`, `c.ld`, `c.sd` — コードサイズ 20-30% 削減
  - NaN-boxing: RISC-V では RV64F/D で double を `fa0`-`fa7` 浮動小数点レジスタに
  - `make-riscv64-assembler` / `riscv64-emit-*` 関数群
- **根拠**: RISC-V は組み込み・サーバーで急速普及。SiFive/StarFive/Milk-V 等の開発ボード対応
- **難易度**: Hard

---

### Phase 170 — 継続プロンプト (Delimited Continuation Prompts)

#### FR-914: Named Continuation Prompts (名前付き継続プロンプト)

- **対象**: `src/vm/vm-execute.lisp`, `src/expand/macros-stdlib.lisp`
- **内容**:
  - `(make-prompt-tag &optional name)` → prompt-tag オブジェクト
  - `(call-with-prompt tag thunk handler)` — Racket の `call-with-continuation-prompt` に相当
  - `(abort-to-prompt tag value)` — タグまでのスタックを巻き戻してハンドラを呼ぶ
  - `(call-with-current-continuation/prompt tag)` — プロンプト区切り付き call/cc
  - 複数プロンプトの入れ子: タグスタックで管理
  - `(with-prompt tag body handler)` — マクロ形式
  - effect/resume のエミュレーション: `(prompt-tag-effect tag)` で algebraic effects をエミュレート
  - Racket `#lang racket` の `call-with-continuation-prompt` 完全互換
- **根拠**: Racket の継続プロンプト / R6RS `call-with-continuation-prompt`。コルーチン・effects の基盤
- **難易度**: Very Hard

---

### Phase 171 — 再現可能ビルド

#### FR-917: Reproducible Build Support (再現可能ビルド)

- **対象**: `src/cli/main.lisp`, `cl-cc.asd`
- **内容**:
  - `*build-seed*` 固定: ハッシュテーブルのシード、gensym カウンタ、ランダム性のソース全て固定
  - ソートされたシンボル出力: パッケージの `do-symbols` を決定論的順序で実行
  - タイムスタンプ除去: FASL/バイナリに埋め込むビルド時刻を `SOURCE_DATE_EPOCH` で上書き
  - パス正規化: ビルドパスをFASLに記録しない（または相対パスで記録）
  - `(build-fingerprint)` → ビルドの SHA256 ダイジェスト（入力ファイル全体の内容ハッシュ）
  - Nix flakes との統合: `nix build` で bit-for-bit 再現可能なバイナリを生成
- **根拠**: Reproducible Builds プロジェクト / Nix。ビルド成果物の改ざん検出と配布の信頼性
- **難易度**: Medium

---

### Phase 172 — 前方参照・宣言前定義

#### FR-920: Forward Reference Resolution (前方参照解決)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **内容**:
  - `(declare (forward-reference foo bar))` — まだ定義されていない関数の宣言
  - 前方参照シンボルを「未解決シンボルセル」として記録
  - ロード完了時に `(resolve-forward-references)` を自動呼び出し
  - 相互再帰: `flet`/`labels` なしのトップレベルでの相互再帰サポート
  - `(defun-forward foo (x) ...)` — 定義と前方宣言を一体化
  - 未解決シンボルへの呼び出し: indirection cell 経由（定義後にセルを更新）
  - `*unresolved-forward-refs*` — ロード後も未解決のシンボルリスト（警告用）
- **根拠**: Common Lisp のトップレベルロード順序依存性の緩和。大規模ファイルの循環依存対応
- **難易度**: Medium

---

### Phase 173 — I/O バッファリング詳細制御

#### FR-923: Buffered I/O Control (I/Oバッファ制御)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `(make-buffered-stream stream &key (buffer-size 4096) (strategy :full))` — バッファリング戦略
  - `:full` (全バッファリング) / `:line` (行バッファリング) / `:none` (非バッファリング)
  - `(finish-output stream)` / `(force-output stream)` — バッファのフラッシュ
  - `(clear-input stream)` / `(clear-output stream)` — バッファクリア
  - `(stream-element-type stream)` → `character` / `(unsigned-byte 8)` / etc.
  - `(open-stream-p stream)` — ストリームオープン確認
  - `(interactive-stream-p stream)` — isatty(3) ラッパー（端末接続かどうか）
  - バイナリモード: `(make-binary-stream path)` / `read-sequence` / `write-sequence` 高速版
- **根拠**: ANSI CL §21。`finish-output`/`force-output`/`clear-input` の完全準拠
- **難易度**: Easy

#### FR-924: String / Broadcast / Echo Streams (特殊ストリーム)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `(make-string-input-stream string &optional start end)` → string-stream
  - `(make-string-output-stream)` → string-output-stream; `(get-output-stream-string)` で取得
  - `(with-output-to-string (var) body)` — マクロ版
  - `(make-broadcast-stream &rest streams)` → broadcast-stream (複数ストリームへ同時書き込み)
  - `(make-two-way-stream input output)` → two-way-stream
  - `(make-echo-stream input output)` → echo-stream (読み取りを output にエコー)
  - `(make-concatenated-stream &rest streams)` → concatenated-stream (複数入力を連結)
  - `(make-synonym-stream symbol)` → synonym-stream (`*standard-output*` の動的バインド)
- **根拠**: ANSI CL §21.1 ストリームの完全種類一覧。テスト・ロギング・パイプラインに必須
- **難易度**: Medium

---

### Phase 174 — Pathname システム完全実装

#### FR-927: Pathname Operations (パス名操作)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `(make-pathname &key host device directory name type version)` — ANSI CL パス名コンストラクタ
  - `(pathname-host p)` / `(pathname-directory p)` / `(pathname-name p)` / `(pathname-type p)` アクセサ
  - `(merge-pathnames pathname defaults)` — デフォルト値のマージ
  - `(namestring pathname)` / `(file-namestring p)` / `(directory-namestring p)` — 文字列変換
  - `(parse-namestring string &optional host defaults)` → pathname
  - `(enough-namestring pathname defaults)` — デフォルトに対する相対表現
  - UNIX logical pathnames: `"src:"` → `/home/user/projects/` マッピング
  - `(wild-pathname-p p)` / `(pathname-match-p p wildcard)` — ワイルドカード対応
  - `(directory pattern)` → pathname list (glob展開)
- **根拠**: ANSI CL §19。`load` / `compile-file` / `probe-file` の基盤
- **難易度**: Medium

---

### Phase 175 — CLOS メタオブジェクトプロトコル 拡張

#### FR-930: MOP Introspection (MOPイントロスペクション)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - `(class-slots class)` → list of slot-definition objects
  - `(slot-definition-name sd)` / `(slot-definition-type sd)` / `(slot-definition-initform sd)`
  - `(slot-definition-allocation sd)` → `:instance` / `:class`
  - `(class-direct-superclasses class)` / `(class-direct-subclasses class)`
  - `(class-precedence-list class)` — CPL 取得
  - `(generic-function-methods gf)` → list of methods
  - `(method-specializers method)` → list of specializers
  - `(method-qualifiers method)` → `(:before :after :around)` リスト
  - `(compute-applicable-methods gf args)` → applicable method list
  - `(method-combination-type gf)` → method combination object
- **根拠**: AMOP (The Art of the Metaobject Protocol)。CL ツールチェーンの基盤（IDEのオートコンプリート等）
- **難易度**: Medium

#### FR-931: compute-applicable-methods-using-classes (クラスベースメソッド選択)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - `(compute-applicable-methods-using-classes gf classes)` → `(values methods definitivep)`
  - `definitivep = t`: クラスが変わらない限りキャッシュ可能
  - `(find-method gf qualifiers specializers &optional errorp)` — メソッド直接検索
  - `(add-method gf method)` / `(remove-method gf method)` — 動的なメソッド変更
  - `(ensure-generic-function name &key lambda-list method-class documentation)`
  - `slot-value` の MOP フック: `slot-value-using-class` / `(setf slot-value-using-class)`
  - `slot-bound-using-class-p` / `slot-makunbound-using-class`
- **根拠**: AMOP §2。カスタムメタクラス・aspect-oriented programming の実装基盤
- **難易度**: Hard

---

### Phase 176 — コンパイラマクロ・宣言処理

#### FR-934: compiler-macro-function (コンパイラマクロ)

- **対象**: `src/expand/expander.lisp`
- **内容**:
  - `(define-compiler-macro name lambda-list body)` — コンパイラマクロ定義
  - `(compiler-macro-function name &optional env)` — コンパイラマクロ取得
  - `(funcall (compiler-macro-function 'foo) form env)` — 手動展開
  - コンパイル時特殊化: `(define-compiler-macro + (&rest args) ...)` で型別最適化
  - `&whole form` パラメータ: 元のフォーム全体へのアクセス
  - コンパイラマクロの無効化: `(setf (compiler-macro-function 'foo) nil)`
  - 展開を断る: コンパイラマクロが `form` をそのまま返す → 通常のマクロ展開へフォールバック
- **根拠**: ANSI CL §3.2.2.1。`list`/`string` などの組み込み関数に定数最適化を付加
- **難易度**: Medium

#### FR-935: proclamation / declaim 完全実装

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `(declaim (type integer *counter*))` → グローバル変数の型宣言
  - `(declaim (ftype (function (integer integer) integer) gcd))` → 関数シグネチャ宣言
  - `(declaim (inline foo))` / `(declaim (notinline foo))` — インライン指示
  - `(declaim (optimize (speed 3) (safety 0) (debug 1)))` — 最適化ポリシー宣言
  - `(declaim (special *global*))` — スペシャル変数宣言
  - `*global-proclamations*` — `defpackage` の前から有効なグローバル宣言テーブル
  - `(the type form)` との統合: `declaim type` + `the` で型チェック除去
- **根拠**: ANSI CL §3.3。コンパイラへのヒント提供の主要手段
- **難易度**: Medium

---

### Phase 177 — ガベージコレクタ チューニング API

#### FR-938: GC Tuning Interface (GCチューニングインターフェース)

- **対象**: `src/runtime/gc.lisp`
- **内容**:
  - `(gc &key full)` — 明示的GC起動（`full=t` でフルGC）
  - `(gc-statistics)` → plist: `(:collections-minor N :collections-major N :bytes-allocated N :bytes-freed N :pause-ms-p99 F)`
  - `(set-gc-threshold :nursery-size bytes)` — ナーサリサイズ調整
  - `(set-gc-threshold :tenuring-threshold n)` — オブジェクト昇格世代数
  - `*gc-notify-function*` — GCコールバック（モニタリング用）
  - `(disable-gc)` / `(enable-gc)` — リアルタイムセクション用GC抑制
  - `(with-gc-disabled body)` — GC禁止区間マクロ
  - `(room &optional detail)` — ANSI CL `room` — メモリ使用量表示
- **根拠**: SBCL `sb-ext:gc` / `sb-ext:bytes-consed`。GCチューニングは高スループットアプリで重要
- **難易度**: Easy

---

### Phase 178 — 並列コンパイル詳細

#### FR-941: Parallel Compilation Pipeline (並列コンパイルパイプライン)

- **対象**: `src/cli/main.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - ファイルレベル並列化: `(compile-files files :parallel t)` — 依存グラフ解析後に独立ファイルを並列コンパイル
  - 依存グラフ構築: `(build-compile-graph files)` → DAG (package定義・マクロ定義を追跡)
  - ワーカープール: `(make-compile-worker-pool n)` — n個のCPUコアを利用
  - フォームレベル並列化: 副作用なし判定後に `(mappar #'compile-form forms)` 
  - 進捗報告: `*compile-progress-fn*` コールバック
  - ASDF並列: `(asdf:operate 'asdf:compile-op system :parallel t)`
  - キャッシュ活用: `(compile-with-cache file cache-dir)` — FASL変更なし時スキップ
- **根拠**: GNU Make `-j` / Bazel / Buck2 の並列ビルド。大規模コードベースのビルド時間短縮
- **難易度**: Hard

---

### Phase 179 — エラーメッセージ品質

#### FR-944: Rich Error Messages (リッチエラーメッセージ)

- **対象**: `src/vm/conditions.lisp`, `src/parse/cl/parser.lisp`
- **内容**:
  - ソース位置の保持: 全AST ノードに `(source-location :file :line :column)` スロット
  - エラー表示: `"error at foo.lisp:42:10: undefined variable 'x'"`
  - コンテキスト行の表示: エラー箇所前後3行 + カラム指示 `^` カーソル
  - Did-you-mean 提案: Levenshtein距離でシンボル名候補を提示
  - `undefined-function` / `unbound-variable` での候補リスト
  - スタックトレースのフォーマット: `(format-stack-trace condition stream :depth 10)`
  - `*error-output*` への構造化出力 (JSON / ANSI カラー切り替え)
- **根拠**: Rust / Elm のエラーメッセージ品質。開発体験の根本的改善
- **難易度**: Medium

#### FR-945: Condition Restart UI (条件リスタートUI)

- **対象**: `src/vm/conditions.lisp`
- **内容**:
  - `(describe-restart restart)` — リスタートの説明文取得
  - `(restart-interactive restart)` — 対話的リスタート起動（REPL でユーザーに入力を促す）
  - `(compute-restarts &optional condition)` → active restart list
  - `(invoke-restart-interactively restart)` — インタラクティブ関数を呼び出してから invoke
  - REPL デバッガ: `(invoke-debugger condition)` → `*debugger-hook*` → 対話的修復
  - `(with-simple-restart (name format-string &rest args) body)` マクロ
  - `store-value` / `use-value` / `continue` / `abort` 標準リスタートの完全実装
- **根拠**: ANSI CL §9.1 restart system。CL の対話的デバッグの中核機能
- **難易度**: Medium

---

### Phase 180 — Weak Reference / Ephemeron 完全実装

#### FR-948: Ephemeron (エフェメロン)

- **対象**: `src/runtime/gc.lisp`, `src/vm/hash.lisp`
- **内容**:
  - `(make-ephemeron key value)` → ephemeron オブジェクト
  - セマンティクス: key が到達不能になった場合、value も回収可能になる（通常の弱参照と異なる）
  - `(ephemeron-key e)` / `(ephemeron-value e)` — アクセサ（GC後は nil）
  - `(ephemeron-alive-p e)` → boolean
  - GCサイクル中の処理: gray set scan → ephemeron の key が白なら value を nil に
  - `(make-weak-hash-table :weakness :key-and-value)` — ephemeron テーブル（SBCL互換）
  - `:weakness :key` / `:weakness :value` / `:weakness :key-and-value` / `:weakness :key-or-value` の全モード
- **根拠**: McCLIM / CLOCC ライブラリ。キャッシュ・メモ化でのメモリリーク防止に必須
- **難易度**: Hard


---

### Phase 181 — 整数演算特殊化

#### FR-951: Fixnum Fast Paths (Fixnum高速パス)

- **対象**: `src/vm/primitives.lisp`, `src/emit/x86-64-codegen.lisp`
- **内容**:
  - `(+f x y)` / `(-f x y)` / `(*f x y)` — fixnum専用演算（オーバーフローチェックなし）
  - `(logand x y)` / `(logior x y)` / `(logxor x y)` / `(lognot x)` — ビット演算
  - `(ash x n)` — 算術シフト（`n>0`=左、`n<0`=右）; `(ash x -1)` = `(floor x 2)`
  - `(integer-length x)` → ビット幅（`(integer-length 255)` = 8）
  - `(logbitp i x)` — ビット `i` のテスト
  - `(logcount x)` — セットビット数 (popcount); x86-64 `POPCNT` 命令に直接マッピング
  - `(deposit-field value bytespec integer)` / `(ldb bytespec integer)` — バイト操作 (ANSI §12.9)
  - `(dpb value bytespec integer)` — ビットフィールド挿入
- **根拠**: ANSI CL §12.9 byte operations。システムプログラミング・ビットマスク操作に必須
- **難易度**: Easy

#### FR-952: Bignum Arithmetic (多倍長整数演算)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - Bignum 表現: `limb-vector` (64ビット符号なし整数の配列) + sign フラグ
  - 加減算: schoolbook O(n)、繰り越し処理
  - 乗算: Karatsuba O(n^1.585) (n>64 limbs 時自動切り替え)
  - 除算: Knuth Algorithm D
  - `(expt n m)` での bignum: 平方乗算法 O(log m 乗算)
  - `(gcd a b)` — バイナリGCD アルゴリズム
  - `(isqrt n)` — 整数平方根
  - 出力: `(write-to-string bignum :radix 16)` — 任意基数出力
  - SBCL / GMP との比較: 純CL実装 vs FFI依存
- **根拠**: Common Lisp の整数は任意精度が必須要件。`(expt 2 1000)` が正しく動作すること
- **難易度**: Hard

---

### Phase 182 — 有理数・複素数演算

#### FR-955: Rational Number Arithmetic (有理数演算)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - `rational` 型: `numerator/denominator` ペア（bignum 対応）
  - `(/ 1 3)` → `#1/3`（自動正規化: GCDで約分）
  - `(+ 1/3 1/6)` → `1/2`（共通分母への変換）
  - `(rational 0.1)` → 正確な有理数近似（IEEE 754 binary fraction の有理数表現）
  - `(rationalize 0.1)` → 人間に読みやすい近似（`1/10` に近い最小分数）
  - `(numerator r)` / `(denominator r)` アクセサ
  - 数値タワー統合: rational は float に昇格（contagion rules）
  - `(floor r)` で整数部分 (quotient) + 有理数余り (remainder)
- **根拠**: ANSI CL §12.1.3。CL の有理数は完全精度の分数演算を提供
- **難易度**: Medium

#### FR-956: Complex Number Arithmetic (複素数演算)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - `complex` 型: real + imaginary ペア（floatまたはrational）
  - `(complex 3 4)` → `#C(3 4)`; `(realpart c)` / `(imagpart c)`
  - `(abs #C(3 4))` → `5.0` (絶対値 = sqrt(real^2 + imag^2))
  - `(conjugate #C(3 4))` → `#C(3 -4)`
  - `(phase #C(0 1))` → `π/2`（偏角）
  - `(* #C(1 2) #C(3 4))` → `#C(-5 10)` (複素乗算)
  - `(exp #C(0 π))` → `#C(-1 0)` (Euler の公式)
  - 数値タワー: complex > double-float > single-float > rational > integer
- **根拠**: ANSI CL §12.1.5。科学計算・フーリエ変換での複素数演算
- **難易度**: Medium

---

### Phase 183 — ストリーム外部形式

#### FR-959: External Format / Encoding (外部形式・文字エンコーディング)

- **対象**: `src/vm/io.lisp`, `src/vm/strings.lisp`
- **内容**:
  - `(open path :external-format :utf-8)` — UTF-8 エンコードストリーム
  - サポート形式: `:utf-8`, `:utf-16le`, `:utf-16be`, `:utf-32le`, `:utf-32be`, `:latin-1`, `:ascii`
  - BOM検出: UTF-8/UTF-16/UTF-32 BOM の自動検出と除去
  - `(stream-external-format stream)` → 現在のエンコーディング
  - `(setf (stream-external-format stream) :utf-8)` — 動的切り替え
  - `*default-external-format*` — デフォルト外部形式変数
  - エンコードエラー処理: `:error`(例外) / `:replace`(代替文字) / `:ignore`(スキップ)
  - `(encode-string string format)` / `(decode-bytes bytes format)` — 低レベルAPI
- **根拠**: ANSI CL §13.1.10 + SBCL `:external-format`。国際化対応の基盤
- **難易度**: Medium

---

### Phase 184 — Pretty Printer 完全実装

#### FR-962: XP Pretty Printer (整形出力システム)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `(pprint form &optional stream)` — 整形出力
  - `(pprint-indent :block n stream)` / `(pprint-indent :current n stream)` — インデント制御
  - `(pprint-newline :linear stream)` / `:fill` / `:miser` / `:mandatory` — 改行種別
  - `(pprint-logical-block (stream list :prefix "(" :suffix ")") body)` — 論理ブロック
  - `(pprint-tab :section n m stream)` — タブ揃え
  - `(pprint-pop)` / `(pprint-exit-if-list-exhausted)` — リスト要素の反復出力
  - `*print-pretty*` / `*print-right-margin*` / `*print-miser-width*` 変数
  - ディスパッチテーブル: `*print-pprint-dispatch*` — 型別整形関数の登録
  - `(set-pprint-dispatch type function &optional priority)` — カスタム整形器の登録
- **根拠**: ANSI CL §22.2。Lisp コードの可読性のある出力に必須。XP (Richard Waters 1989) に基づく
- **難易度**: Hard

---

### Phase 185 — FORMAT ディレクティブ完全実装

#### FR-965: FORMAT Complete Directives (FORMAT完全実装)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `~A` / `~S` / `~W` — 基本出力
  - `~D` / `~B` / `~O` / `~X` — 整数（10進/2進/8進/16進）
  - `~F` / `~E` / `~G` / `~$` — 浮動小数点出力
  - `~C` — 文字出力 (`~:C` = 名前付き表示、`~@C` = 読み戻し可能形式)
  - `~P` — 複数形 (`"~D item~:P"`)
  - `~R` — 英語基数 (`~:R` = 序数、`~@R` = ローマ数字)
  - `~T` — タブ移動 (`~,T` = 相対、`~:T` = カラム揃え)
  - `~%` / `~&` / `~~` / `~|` / `~_` — 改行・ページ送り
  - `~(` / `~)` — 大文字小文字変換 (`~:@(`=全大文字等)
  - `~[`/`~]` — 条件分岐; `~{`/`~}` — 反復; `~<`/`~>` — 幅揃え; `~?` — 間接
  - `~*` — 引数のスキップ; `~^` — 反復の脱出
  - `*print-format-errors*` — フォーマットエラーの詳細報告
- **根拠**: ANSI CL §22.3。`format` は CL の主要出力手段、全ディレクティブの完全実装が必要
- **難易度**: Medium

---

### Phase 186 — READ マクロ完全実装

#### FR-968: Readtable Complete Implementation (Readtable完全実装)

- **対象**: `src/parse/lexer.lisp`
- **内容**:
  - `(make-readtable)` / `(copy-readtable &optional from to)` — readtable 管理
  - `(set-macro-character char function &optional non-terminating-p readtable)` — マクロ文字登録
  - `(get-macro-character char &optional readtable)` → `(values function non-terminating-p)`
  - `(set-dispatch-macro-character disp-char sub-char function)` — `#` ディスパッチ文字
  - `(get-dispatch-macro-character disp-char sub-char)` → function
  - `(set-syntax-from-char to-char from-char &optional to-readtable from-readtable)` — 構文コピー
  - `*readtable*` — 現在のリードテーブル
  - `(with-readtable (rt) body)` — 一時的なリードテーブル切り替えマクロ
  - 標準ディスパッチマクロ文字: `#'`, `#(`, `#*`, `#:`, `#.`, `#+`, `#-`, `#|`, `#\`, `#b`, `#o`, `#x`, `#r`, `#c`, `#a`, `#s`, `#p`
- **根拠**: ANSI CL §23。`#'`, `#(vector)`, `#\char` 等のリードマクロは CL コードの読み取りに必須
- **難易度**: Medium

---

### Phase 187 — EVAL-WHEN の完全セマンティクス

#### FR-971: eval-when Full Semantics (eval-when完全セマンティクス)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `(eval-when (:compile-toplevel :load-toplevel :execute) body)` — 3段階制御
  - `:compile-toplevel`: `compile-file` 中にコンパイラが実行 (マクロ定義に使用)
  - `:load-toplevel`: `load` 時に実行 (通常の `defun`/`defvar` と同じ)
  - `:execute`: `eval` または非コンパイル時に実行
  - ネスト処理: `eval-when` の内部 `eval-when` の正確なセマンティクス (ANSI §3.2.3.1)
  - `compile-file` ステージ: processing mode (compile/not-compile/eval) の伝播
  - `(eval-when (:compile-toplevel) (defmacro m ...))` — コンパイル時マクロ定義の標準パターン
  - `*compile-time-too*` フラグ — コンパイル時実行が必要かどうかの追跡
- **根拠**: ANSI CL §3.2.3。`eval-when` のセマンティクスは複雑で実装バグが多い箇所
- **難易度**: Hard

---

### Phase 188 — LOOP マクロ拡張

#### FR-974: LOOP Extended Clauses (LOOPクローズ拡張)

- **対象**: `src/expand/macros-sequence.lisp`
- **内容**:
  - `(loop for x :from 0 :below 10 :by 2)` — `:by` ステップの完全サポート
  - `(loop for (a b) :in list)` — デストラクチャリングバインディング
  - `(loop for x :being the hash-keys of ht :using (hash-value v))` — ハッシュテーブル反復
  - `(loop for x :being the symbols of *package*)` — パッケージシンボル反復
  - `(loop with x = init then update)` — 複数 `:with` クローズの相互参照
  - `(loop named outer ... (loop-finish))` — 名前付きループ + `loop-finish` からのネスト脱出
  - `(loop initially body)` / `(loop finally body)` — 初期化・後処理クローズ
  - `(loop for x :in seq :thereis (pred x))` — 短絡評価収集
  - `(loop for x :from 0 :downto -10)` — 逆方向反復
- **根拠**: ANSI CL §6.1。LOOP は CL の最も複雑なマクロの一つで、多くのエッジケースがある
- **難易度**: Medium

---

### Phase 189 — CLOS メソッドコンビネーション拡張

#### FR-977: Method Combination Types (メソッドコンビネーション種別)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - 標準コンビネーション: `standard` (call-next-method / :before :after :around)
  - `(define-method-combination + :identity-with-one-argument t)` — `+` コンビネーション
  - 同様: `and` / `or` / `list` / `append` / `nconc` / `max` / `min` / `progn` コンビネーション
  - `(define-method-combination name &lambda-list options &body body)` — カスタム定義
  - `:most-specific-first` / `:most-specific-last` — メソッド順序
  - `(call-method method &optional next-methods)` — コンビネーション内のメソッド呼び出し
  - `(make-method form)` — コンビネーション内インラインメソッド
  - `(method-combination-error format &rest args)` — コンビネーションエラー報告
- **根拠**: ANSI CL §7.6.6 / AMOP §5。`+`/`append` コンビネーションは mixins パターンで多用
- **難易度**: Medium

---

### Phase 190 — 動的束縛の最適化

#### FR-980: Special Variable Lookup Optimization (スペシャル変数探索最適化)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - 現状: `(symbol-value sym)` → `*dynamic-env*` ハッシュテーブル検索 O(1) avg
  - 最適化1: シャドウスタック方式 — `(let ((*x* val)) body)` で `*x*` のスタックに push/pop
  - 最適化2: スレッドローカルストレージ (TLS) セル — 各スレッドが独立したセルを持つ
  - `(locally (declare (special *x*)) ...)` — スペシャル変数の局所宣言
  - `(boundp '*x*)` / `(makunbound '*x*)` — バインディング存在チェック・解除
  - `(progv '(*x* *y*) '(1 2) body)` — 動的変数リストのバインド（ANSI準拠）
  - プロファイリング: スペシャル変数アクセスのホットスポット検出
- **根拠**: SBCL の TLS cell + shadow stack。多用スペシャル変数のアクセスコストをO(1)に
- **難易度**: Medium

---

### Phase 191 — Tail Call 最適化 詳細

#### FR-983: Proper Tail Calls (適切な末尾呼び出し)

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm-execute.lisp`
- **内容**:
  - `(funcall fn args)` が末尾位置なら `vm-tail-call` 命令
  - `(apply fn args)` の末尾最適化
  - `let`/`let*`/`labels`/`flet` の本体部分の末尾位置認識
  - `cond`/`case`/`typecase` の全ブランチの末尾位置認識
  - `progn` の最後のフォームの末尾位置認識
  - `and`/`or` の末尾要素の末尾位置認識
  - 相互再帰の末尾最適化: `labels` 内での相互再帰をループに変換
  - `(declare (optimize (debug 0)))` での TCO 強制
  - 末尾位置の正確な定義 (ANSI §3.1.2.1.3): `tagbody` 内は末尾位置でない
- **根拠**: Scheme R7RS では適切な末尾再帰が必須要件。CL では宣言依存だが実用上重要
- **難易度**: Medium

---

### Phase 192 — 型システム 実行時チェック

#### FR-986: Runtime Type Checking (実行時型チェック)

- **対象**: `src/vm/vm-execute.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `(the type form)` — 型宣言（`(safety 0)` でチェック除去、`(safety 3)` でランタイム検証）
  - `(check-type place type &optional string)` — 実行時型チェック + `type-error` 条件
  - `(typep x 'type)` の高速パス: NaN-boxing タグで O(1) プリミティブ型判定
  - `(subtypep type1 type2)` — 型包含関係チェック（コンパイル時）
  - `(type-of x)` — 実行時の具体的な型を返す
  - `(satisfies pred)` 型: `(typep x '(satisfies evenp))` → `(evenp x)` の実行
  - 型エラーの詳細化: `(type-error-datum e)` / `(type-error-expected-type e)`
  - `(upgraded-array-element-type type)` — 配列要素型の実際のストレージ型
- **根拠**: ANSI CL §4。`check-type` は引数バリデーションの標準パターン
- **難易度**: Medium

---

### Phase 193 — コンパイル環境オブジェクト

#### FR-989: Compilation Environment (コンパイル環境オブジェクト)

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`
- **内容**:
  - `(compile-file-pathname src &key output-file)` → FASL パス
  - `(compiled-function-p fn)` → boolean
  - `(compile name &optional definition)` — 単一関数のコンパイル
  - `(disassemble fn)` — コンパイル済み関数の逆アセンブル出力
  - `*compile-verbose*` / `*compile-print*` — コンパイル時出力制御
  - `(with-compilation-unit (:override t) body)` — 警告を集約してから出力
  - Environment オブジェクト: `(environment-p env)` / `(make-null-environment)`
  - `(variable-information name env)` → `(values kind local-p decls)`
  - `(function-information name env)` → `(values kind local-p decls)`
  - `(augment-environment env &key variable symbol-macro function macro declare)`
- **根拠**: ANSI CL §3.2 / CLTL2 環境API。ツールチェーン・IDE・コードウォーカーに必要
- **難易度**: Hard

---

### Phase 194 — 文字・文字列の完全準拠

#### FR-992: Character Names and Predicates (文字名・述語)

- **対象**: `src/parse/lexer.lisp`, `src/vm/strings.lisp`
- **内容**:
  - 標準文字名: `#\Space` / `#\Newline` / `#\Tab` / `#\Return` / `#\Backspace` / `#\Rubout` / `#\Delete` / `#\Escape` / `#\Null` / `#\Altmode` / `#\Page`
  - `(char-name char)` → 文字名文字列または `nil`; `(name-char string)` → 文字
  - `(char-upcase c)` / `(char-downcase c)` — 大文字小文字変換
  - `(char= c1 c2)` / `(char< c1 c2)` 等 — 文字比較（case-sensitive）
  - `(char-equal c1 c2)` / `(char-lessp c1 c2)` 等 — case-insensitive比較
  - `(alpha-char-p c)` / `(digit-char-p c)` / `(alphanumericp c)` / `(graphic-char-p c)` / `(standard-char-p c)` / `(upper-case-p c)` / `(lower-case-p c)` / `(both-case-p c)`
  - `(char-code c)` / `(code-char n)` — Unicodeコードポイント変換
  - `(digit-char n &optional radix)` → 数字文字
- **根拠**: ANSI CL §13。文字述語はパーサ・スキャナの基本構成要素
- **難易度**: Easy

#### FR-993: String Operations Complete (文字列操作完全実装)

- **対象**: `src/vm/strings.lisp`
- **内容**:
  - `(string-upcase s)` / `(string-downcase s)` / `(string-capitalize s)` + `:start`/`:end`
  - `(string-trim chars s)` / `(string-left-trim chars s)` / `(string-right-trim chars s)`
  - `(string= s1 s2)` / `(string< s1 s2)` etc. + case-insensitive variants
  - `(string-search pattern s &key :start1 :end1 :start2 :end2 :from-end)` (= `search` on strings)
  - `(parse-integer s &key :radix :start :end :junk-allowed)` → integer + end-position
  - `(read-from-string s &optional eof-error-p eof-value &key :start :end :preserve-whitespace)`
  - `(write-to-string obj &key :base :radix :case ...)` — 全 `*print-*` 変数の尊重
  - `(format nil ...)` との統合
- **根拠**: ANSI CL §16。文字列操作は最も頻繁に使われる標準関数群
- **難易度**: Easy

---

### Phase 195 — Structure / Record 型

#### FR-996: defstruct Complete Implementation (defstruct完全実装)

- **対象**: `src/expand/macros-stdlib.lisp`, `src/vm/vm.lisp`
- **内容**:
  - `(defstruct point x y)` — 基本構造体定義
  - `(make-point :x 1 :y 2)` — コンストラクタ（デフォルト名: `make-NAME`）
  - `(point-p obj)` — 型述語; `(point-x p)` / `(point-y p)` — アクセサ
  - `:conc-name` / `:constructor` / `:copier` / `:predicate` — オプション
  - スロットオプション: `(defstruct foo (x 0 :type fixnum :read-only t))`
  - `:include` 継承: `(defstruct (colored-point (:include point)) color)` — 単一継承
  - `:type` オプション: `(:type vector)` / `(:type list)` — ストレージ型選択
  - `(copy-point p)` — コピーコンストラクタ（デフォルトで生成）
  - `print-object` メソッドの自動生成
  - `*structure-types*` — 登録済み構造体型のハッシュテーブル
- **根拠**: ANSI CL §8。`defstruct` は CLOS以前からの軽量レコード型。CLOS より高速
- **難易度**: Medium

---

### Phase 196 — 型宣言の最適化統合

#### FR-999: Type Declaration Optimization (型宣言最適化統合)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **内容**:
  - `(declare (type fixnum i))` → ループ内の型チェック除去
  - `(declare (type (simple-array double-float (*)) v))` → 配列アクセスの境界チェック除去
  - `(declare (dynamic-extent x))` → スタック割り当て（ヒープ不要）
  - `(declare (dynamic-extent #'fn))` → ローカル関数クロージャのスタック確保
  - `(declare (values fixnum))` → 多値の型情報伝播
  - 型伝播: 宣言された型を dataflow analysis で全使用箇所に伝播
  - `(safety 0)` + `(type fixnum x)` の組み合わせ: 全チェック除去
  - `(speed 3) (safety 0)` モード: Java HotSpot の -server フラグに相当する最速モード
- **根拠**: SBCL の型宣言効果。`declare` は CL の主要最適化ヒント機構
- **難易度**: Hard


---

### Phase 197 — イメージ保存・コアダンプ

#### FR-1002: save-lisp-and-die / Image Dump (イメージ保存)

- **対象**: `src/cli/main.lisp`, `src/runtime/runtime.lisp`
- **内容**:
  - `(save-lisp-and-die path &key toplevel executable compression)` — SBCL互換イメージ保存
  - 全ヒープ・スタック・コードをシリアライズしてファイルに書き出す
  - `executable = t`: ELF/Mach-O バイナリとして直接実行可能なイメージ
  - `toplevel` 関数: 次回起動時に呼び出すエントリポイント
  - `compression`: `zlib` / `lz4` / `zstd` でイメージを圧縮（50-70% 削減）
  - コアフォーマット: ヘッダ (magic, version, GC roots offset) + セグメント群 (code/data/heap)
  - `*saved-core-pathname*` — 現在実行中のコアファイルパス
  - `(lisp-implementation-version)` / `(lisp-implementation-type)` — ANSI CL §25.1
- **根拠**: SBCL `save-lisp-and-die`。Lisp の killer feature の一つ — 起動時間ゼロの実行ファイル生成
- **難易度**: Very Hard

#### FR-1003: Core File Loading (コアファイルロード)

- **対象**: `src/cli/main.lisp`
- **内容**:
  - `./cl-cc --core path.core` — 保存済みイメージからの高速起動
  - `mmap` でコアをメモリにマップ → ページフォルト駆動の遅延ロード
  - GC roots の再スキャン: 保存済みヒープへの内部ポインタを修正 (relocation)
  - ASLR対応: position-independent core (全ポインタをベースアドレスからのオフセットで保存)
  - `--dynamic-space-size N` — ダイナミックスペースサイズ上書き
  - 起動時間目標: < 10ms（Erlang VM / LuaJIT 並み）
- **根拠**: Lisp イメージの高速起動。初回コンパイル後のロード時間をほぼゼロに
- **難易度**: Hard

---

### Phase 198 — OS インターフェース

#### FR-1006: Environment Variables / CLI Args (環境変数・コマンドライン引数)

- **対象**: `src/cli/main.lisp`, `src/vm/io.lisp`
- **内容**:
  - `(getenv name)` → string または nil (`getenv(3)` ラッパー)
  - `(setenv name value)` / `(unsetenv name)` — 環境変数設定・削除 (`setenv(3)`)
  - `(environ)` → `((name . value) ...)` alist 全環境変数
  - `sb-ext:*posix-argv*` 互換: `*command-line-args*` — プログラム名除いた引数リスト
  - `(exit &optional (code 0))` — プロセス終了 (`_exit(2)`)
  - `(quit &optional code)` — `exit` のエイリアス (SBCL互換)
  - `(machine-instance)` / `(machine-type)` / `(machine-version)` — ANSI §25.1
  - `(software-type)` / `(software-version)` — OS情報
- **根拠**: ANSI CL §25.1 + SBCL 拡張。CLIツール・スクリプトの基本インフラ
- **難易度**: Easy

#### FR-1007: Process Management (プロセス管理)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `(run-program command args &key input output error wait)` — 外部プロセス起動
  - `:input` / `:output` / `:error`: `:pipe` (ストリーム) / `:null` / `:inherit` / stream
  - `(process-output process)` / `(process-input process)` — I/Oストリームアクセス
  - `(process-wait process &optional check-for-stopped)` — 終了待機
  - `(process-exit-code process)` → integer
  - `(process-kill process signal)` — シグナル送信
  - `(process-alive-p process)` → boolean
  - `(shell command)` → output-string (シェルコマンド実行の便利関数)
  - `fork(2)` + `execvp(2)` + `waitpid(2)` の直接ラッパー
- **根拠**: UIOP `:run-program` / SBCL `sb-ext:run-program`。ビルドシステム・CI パイプラインに必須
- **難易度**: Medium

---

### Phase 199 — POSIX シグナルハンドリング

#### FR-1010: Signal Handling (シグナルハンドリング)

- **対象**: `src/runtime/runtime.lisp`
- **内容**:
  - `(set-signal-handler signal function)` — シグナルハンドラ登録 (`sigaction(2)`)
  - `(get-signal-handler signal)` → function または `:default` / `:ignore`
  - `(signal-mask &rest signals)` → 旧マスク; `(unblock-signal signal)` — マスク解除
  - 非同期シグナルの安全な処理: handler は `*pending-signals*` に記録し、次のセーフポイントで実行
  - 標準シグナル: `SIGINT`(2), `SIGTERM`(15), `SIGHUP`(1), `SIGUSR1`(10), `SIGUSR2`(12), `SIGWINCH`(28)
  - `(with-signal-handler (sig fn) body)` — 一時的なハンドラ設定マクロ
  - `SIGINT` デフォルト: `keyboard-interrupt` condition を raise
  - `SIGTERM` デフォルト: グレースフルシャットダウン (`*shutdown-hooks*` を順次実行)
- **根拠**: POSIX.1-2017。サーバーアプリケーションのシグナル駆動ライフサイクル管理に必須
- **難易度**: Medium

#### FR-1011: Timer / Alarm (タイマー・アラーム)

- **対象**: `src/vm/io.lisp`, `src/runtime/runtime.lisp`
- **内容**:
  - `(sleep seconds)` — ANSI CL §25.1.4（小数秒対応）; `nanosleep(2)` ベース
  - `(get-internal-real-time)` → 単調時刻 (ANSI §25.1.4)
  - `(get-internal-run-time)` → CPU時間
  - `(get-universal-time)` → ANSI universal time (1900年起算秒数)
  - `(decode-universal-time time &optional tz)` → 9値 (second minute hour date month year day dst tz)
  - `(encode-universal-time second minute hour date month year &optional tz)` → universal-time
  - `(get-decoded-time)` → 現在時刻を9値で返す
  - `(with-timeout (seconds) body)` — タイムアウトマクロ (`SIGALRM` / `setitimer` ベース)
- **根拠**: ANSI CL §25.1.4 全時刻関数。サーバー・ベンチマーク・スケジューリングの基盤
- **難易度**: Easy

---

### Phase 200 — ファイナライザ

#### FR-1014: Finalizers (ファイナライザ)

- **対象**: `src/runtime/gc.lisp`
- **内容**:
  - `(finalize object function)` — obj が GC されるとき `(funcall function)` を実行
  - `(cancel-finalization object)` — ファイナライザの取り消し
  - ファイナライズキュー: GC後、死んだオブジェクトのファイナライザを専用スレッドで実行
  - 実行タイミング: major GC の sweep フェーズ後、GC スレッド外で安全に実行
  - 循環参照とファイナライザ: ファイナライザ内で対象オブジェクトを蘇生させる場合の処理
  - `*finalizer-thread*` — ファイナライザ実行スレッド
  - 弱参照との統合: `(make-weak-pointer obj)` + `(weak-pointer-value wp)` + finalize の組み合わせ
  - SBCL `sb-ext:finalize` / Trivia `trivial-garbage:finalize` との互換性
- **根拠**: C リソースラッパー（FFI で確保したメモリ・ファイルハンドル等）の自動解放に必須
- **難易度**: Hard

---

### Phase 201 — Symbol Macro / Setf Expander

#### FR-1017: define-symbol-macro / symbol-macrolet (シンボルマクロ)

- **対象**: `src/expand/expander.lisp`
- **内容**:
  - `(define-symbol-macro name expansion)` — グローバルシンボルマクロ定義
  - `(symbol-macrolet ((name expansion) ...) body)` — ローカルシンボルマクロ
  - 展開: `name` が式として現れるとき `expansion` に置換
  - `(macroexpand name)` で展開可能（`macroexpand` は symbol-macro を処理）
  - `setf` との統合: `(setf name val)` → `(setf expansion val)` に展開
  - `(macroexpand-1 name)` → `(values expansion t)` when symbol-macro
  - CLOS との連携: CLOS スロットアクセスで `with-slots` の内部実装に使用
  - 環境オブジェクト中の symbol-macro情報: `(variable-information name env)` → `:symbol-macro`
- **根拠**: ANSI CL §3.1.2.1.2 `symbol-macrolet`。`with-slots` / `with-accessors` の実装基盤
- **難易度**: Medium

#### FR-1018: define-setf-expander (setf展開器)

- **対象**: `src/expand/expander.lisp`
- **内容**:
  - `(define-setf-expander place-name lambda-list &body body)` — カスタムplace定義
  - 5値返却: `(get-setf-expansion place &optional env)` → `(vars vals stores store-form access-form)`
  - `vars`: テンポラリ変数リスト; `vals`: 評価値リスト
  - `stores`: ストア変数 (新しい値を受け取る); `store-form`: 書き込みフォーム; `access-form`: 読み取りフォーム
  - `(setf (ldb bytespec integer) value)` の実装例
  - `(incf place)` → `get-setf-expansion` → 展開して `(setq temp (1+ temp))` 等
  - `(define-setf-expander nth (n list) ...)` — `(setf (nth n list) val)` の正しい実装
  - 副作用の評価回数制御: 各サブフォームを正確に1回だけ評価する保証
- **根拠**: ANSI CL §5.1.2。カスタムplace の setf を可能にする機構
- **難易度**: Medium

---

### Phase 202 — エスケープ解析・クロージャ最適化

#### FR-1021: Escape Analysis (エスケープ解析)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - ヒープ割り当て不要の検出: `(let ((x (cons 1 2))) (car x))` → x は脱出しないのでスタック割り当て
  - 解析スコープ: 関数内ローカル + インライン済み呼び出し先
  - エスケープ条件: 関数外への return / global への store / `funcall` 引数 / ヒープ構造への格納
  - スタック割り当てへの変換: `vm-stack-alloc-cons` 命令
  - `(declare (dynamic-extent list))` との統合: ユーザー宣言でエスケープ解析を補助
  - `list` / `vector` / クロージャ / 構造体インスタンスに適用
  - インタープロシージャ解析: インライン展開後の再解析でエスケープ範囲を縮小
- **根拠**: HotSpot のスカラ置換 / SBCL の dynamic-extent。GC 負荷を根本的に削減
- **難易度**: Hard

#### FR-1022: Closure Conversion Optimization (クロージャ変換最適化)

- **対象**: `src/compile/codegen.lisp`
- **内容**:
  - フラットクロージャ: 捕捉変数を環境ベクタにコピー（閉包チェーンなし）— 現状実装
  - 最適化: 捕捉変数が1つなら box 不要 — クロージャ本体にインライン
  - read-only 捕捉: 変数が変更されないなら値コピー（cell 不要）
  - 変更される捕捉変数: `cell` (cons ベースの間接参照) で実装
  - `(make-closure fn env)` での環境クロージャと `vm-closure` の対応
  - 環境共有の検出: 同一 `labels` 内の複数クロージャが同じ変数を捕捉 → 共有セル
  - `(flet ((f () x)))` での `x` の捕捉: セル不要（flet 内で変更なし）
- **根拠**: SML/NJ の closure conversion。クロージャ呼び出しの間接参照コスト削減
- **難易度**: Hard

---

### Phase 203 — Tail Recursion Modulo Cons (TRMC)

#### FR-1025: Tail Recursion Modulo Cons (末尾再帰Cons最適化)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **内容**:
  - TRMC パターン: `(defun copy-list (l) (if l (cons (car l) (copy-list (cdr l))) nil))`
  - 変換: 末尾cons をループに変換 — スタック O(n) → O(1)
  - 実装: `copy-list` の最後の引数が自己再帰で `cons` の cdr 位置にある場合を検出
  - forward pointer テクニック: cons セルを事前確保して cdr を後から書き込む
  - 適用可能パターン: `(cons a (self-recursive-call ...))` / `(list* a b (self-recursive-call ...))`
  - `(declare (optimize (tail-recursion-modulo-cons t)))` で有効化
  - `map` / `filter` / `append` の自動TRMC変換
- **根拠**: OCaml 5 のTRMC最適化 (2022)。`map`/`filter` のスタックオーバーフロー問題を解決
- **難易度**: Hard

---

### Phase 204 — パッケージローカルニックネーム

#### FR-1028: Package-Local Nicknames (パッケージローカルニックネーム)

- **対象**: `src/vm/vm.lisp`, `src/parse/lexer.lisp`
- **内容**:
  - `(defpackage :my-pkg (:local-nicknames (:a :alexandria) (:s :serapeum)))` — ローカル略称
  - `a:iota` → `alexandria:iota` (`:my-pkg` 内でのみ有効)
  - `(add-package-local-nickname nick pkg &optional in-package)` — 動的追加
  - `(remove-package-local-nickname nick &optional in-package)` — 削除
  - `(package-local-nicknames package)` → `((nick . full-pkg) ...)`
  - リーダー統合: `*readtable*` とパッケージのローカルニックネームを組み合わせて解決
  - SBCL / CCL / ECL / ABCL 全てが実装済みの事実上の標準拡張
  - `:global-nicknames` と `:local-nicknames` の優先順位
- **根拠**: SBCL `sb-ext:add-package-local-nickname`。大規模コードベースでのパッケージ名衝突回避
- **難易度**: Medium

---

### Phase 205 — バイトコード中間層

#### FR-1031: Bytecode Interpreter Layer (バイトコードインタープリタ層)

- **対象**: `src/vm/vm.lisp` (新規: `src/vm/bytecode.lisp`)
- **内容**:
  - コンパクトバイトコード形式: 1バイトオペコード + 可変長オペランド
  - 命令セット: `CONST u16` / `LOAD u8` / `STORE u8` / `CALL u8` / `TCALL u8` / `RET` / `JMP i16` / `JNIL i16` / `CLOSURE u16` / `MAKE-ENV u8`
  - バイトコードはネイティブコードの 1/5 サイズ
  - インタープリタ: C の computed goto (threaded dispatch) に相当する Lisp ループ
  - `(compile-to-bytecode form)` → bytecode-function
  - `(bytecode-disassemble fn)` → 人間が読める形式
  - JIT ティアリング: 実行回数 > 閾値でネイティブコードに昇格
  - `*bytecode-threshold*` — JIT 昇格まで何回バイトコードで実行するか
- **根拠**: LuaJIT の bytecode VM / CPython のバイトコード。起動速度と実行速度のトレードオフ
- **難易度**: Hard

---

### Phase 206 — CLOS クラス変更・再初期化

#### FR-1034: change-class (クラス変更)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - `(change-class instance new-class &rest initargs)` — インスタンスのクラスを動的変更
  - AMOP 準拠: `update-instance-for-different-class` の呼び出し
  - `(update-instance-for-different-class previous current &rest initargs)` — メソッドフック
  - スロットマイグレーション: 共通スロットは値を保持、新規スロットは `initargs` または `initform`
  - 削除されたスロットの値は `previous` の shallow copy で参照可能
  - `(defmethod update-instance-for-different-class :after (prev curr &key) ...)` — カスタム処理
  - `change-class` 後の型述語: `(typep instance 'new-class)` → t
- **根拠**: ANSI CL §7.3 / AMOP §2。プロトタイプベース開発・ライブアップグレードに使用
- **難易度**: Hard

#### FR-1035: reinitialize-instance (インスタンス再初期化)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - `(reinitialize-instance instance &rest initargs)` — 既存インスタンスを再初期化
  - `update-instance-for-redefined-class` との区別: 同一クラス内の再初期化
  - `(defmethod reinitialize-instance :after ((obj my-class) &key)` — カスタムフック
  - `shared-initialize` への委譲: `reinitialize-instance` → `shared-initialize` の呼び出し
  - initargs 検証: `:allow-other-keys` なしの場合、不明なキーは `error`
  - `(initialize-instance obj &rest initargs)` との違い: `reinitialize-instance` はオブジェクトを受け取る
- **根拠**: ANSI CL §7.3.4。設定オブジェクトのリセット・スロット再設定に使用
- **難易度**: Medium

---

### Phase 207 — ASDF インテグレーション

#### FR-1038: ASDF System Definition (ASDFシステム定義)

- **対象**: `cl-cc.asd`
- **内容**:
  - `defsystem` の完全処理: `:components` / `:depends-on` / `:serial` / `:in-order-to`
  - コンポーネント種別: `:file` / `:module` / `:system` / `:static-file`
  - `:around-compile-hook` — コンパイル時処理のカスタマイズ（宣言の自動挿入等）
  - `:default-component-class` — デフォルトコンポーネント型の変更
  - `operate` プロトコル: `asdf:compile-op` / `asdf:load-op` / `asdf:test-op`
  - `(asdf:system-source-directory system)` → pathname
  - `(asdf:find-system name)` → system オブジェクト
  - `(asdf:component-pathname component)` → pathname
  - システム依存グラフの DAG 構築 + トポロジカルソート
  - `(asdf:oos 'asdf:load-op "my-system" :verbose nil)` — ロード最適化
- **根拠**: ASDF は CL の事実上の標準ビルドシステム。完全サポートで Quicklisp 互換性確保
- **難易度**: Medium

#### FR-1039: System Dependency Resolution (システム依存解決)

- **対象**: `src/cli/main.lisp`
- **内容**:
  - `(ql:quickload "system-name")` — Quicklisp互換ロード API
  - `*quicklisp-client-directory*` / `*local-project-directories*` — 検索パス
  - `(ql:system-apropos "search-term")` — システム名検索
  - `(ql:update-all-dists)` — ディストリビューション更新
  - ローカルオーバーライド: `~/quicklisp/local-projects/` 優先
  - `(ql:uninstall "system")` — アンインストール
  - `qlfile` / `qlfile.lock` フォーマット: Roswell / qlot 互換
  - HTTP ダウンロード + SHA256 検証 + tar.gz 展開の実装
- **根拠**: Quicklisp は CL エコシステムの中心。互換 API でライブラリエコシステムへのアクセスを提供
- **難易度**: Hard

---

### Phase 208 — オブジェクトシリアライゼーション

#### FR-1042: Object Serialization (オブジェクトシリアライズ)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `*print-readably*` = t でのオブジェクト出力: `write` が再読み取り可能な形式を生成
  - `(make-load-form object &optional environment)` — ロード可能フォームの生成 (ANSI §3.2.4.4)
  - `make-load-form-saving-slots` — スロット値を保存する汎用実装
  - `(write-to-fasl object stream)` / `(read-from-fasl stream)` — FASL シリアライゼーション
  - `print-unreadable-object` マクロ: `#<Class :at address>` 形式の出力
  - `(with-standard-io-syntax body)` — 全 `*print-*` 変数を標準値に束縛
  - `*print-length*` / `*print-level*` / `*print-circle*` / `*print-array*` / `*print-gensym*`
  - S式シリアライゼーション: `(read)` で再構築可能な形式への変換ルール
- **根拠**: ANSI CL §22.1。開発環境での inspect/describe、FASL生成の基盤
- **難易度**: Medium

---

### Phase 209 — クラス割当スロット (class-allocated slots)

#### FR-1045: Class-Allocated Slots (クラス割当スロット)

- **対象**: `src/vm/vm-clos.lisp`
- **内容**:
  - `(defclass foo () ((counter :allocation :class :initform 0)))` — クラスレベルスロット
  - `:allocation :class`: 全インスタンスで共有される1つの値
  - `:allocation :instance` (デフォルト): 各インスタンスが独立した値
  - `(slot-value instance 'counter)` — インスタンス経由でクラススロットにアクセス
  - `(setf (slot-value instance 'counter) n)` — 全インスタンスに影響
  - クラスストレージ: クラスメタオブジェクト内の `class-slots-storage` ハッシュテーブル
  - `(class-slot-value class 'counter)` — クラス直接アクセス (MOP拡張)
  - サブクラスでの継承: `:allocation :class` スロットはサブクラスでも共有
- **根拠**: ANSI CL §7.5.3。グローバルカウンタ・共有キャッシュ・デフォルト設定の実装に使用
- **難易度**: Medium

---

### Phase 210 — インライン展開詳細制御

#### FR-1048: Inlining Policy and Expansion (インライン展開ポリシー)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **内容**:
  - `(declaim (inline fn))` — コンパイル時インライン宣言
  - `(declaim (notinline fn))` — インライン禁止（デバッグ・プロファイリング用）
  - インライン展開: 呼び出しサイトにfn の本体をコピー → α変換（変数名衝突回避）
  - 展開サイズ制限: `*inline-expansion-limit*` (デフォルト200ノード) — 巨大関数のインライン防止
  - 再帰関数のインライン: 深さ制限 (`*inline-recursion-depth*` = 2)
  - コンパイラノート: インライン展開の成否をユーザーに報告
  - `define-compiler-macro` との違い: `inline` は定義をコピー、compiler-macro は変換を定義
  - `sb-ext:maybe-inline` 相当: ヒューリスティック判断でのインライン
- **根拠**: ANSI CL `inline` 宣言。小さなユーティリティ関数の関数呼び出しオーバーヘッド除去
- **難易度**: Medium

---

### Phase 211 — プリンタ変数完全実装

#### FR-1051: Print Control Variables (プリンタ制御変数完全実装)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `*print-case*` → `:upcase` / `:downcase` / `:capitalize` — シンボル出力の大文字小文字
  - `*print-base*` — 整数出力の基数 (2-36); `*print-radix*` — 基数接頭辞 `#b` 等
  - `*print-length*` — リスト要素の最大表示数 (`...` で省略)
  - `*print-level*` — ネスト深さの最大表示数 (`#` で省略)
  - `*print-circle*` — 循環構造の `#n=` / `#n#` 表記
  - `*print-escape*` — エスケープ文字の出力 (`nil` で `princ` 相当)
  - `*print-gensym*` — gensym の `#:` 接頭辞
  - `*print-array*` — 配列内容の出力 (`nil` で `#<Array>` 表示)
  - `*print-lines*` — 出力行数上限
  - `*print-right-margin*` — pretty-printer の折り返し列
  - `(write obj &key stream :case :base :radix :level :length :circle :escape :gensym :array :pretty)` — キーワードで全変数を上書き
- **根拠**: ANSI CL §22.1.3 全 `*print-*` 変数。デバッグ出力の完全制御に必要
- **難易度**: Medium

---

### Phase 212 — READ 制御変数・数値入力

#### FR-1054: READ Control Variables (READ制御変数)

- **対象**: `src/parse/lexer.lisp`
- **内容**:
  - `*read-base*` — 整数のデフォルト読み取り基数 (デフォルト10)
  - `*read-eval*` — `#.` の有効化フラグ (セキュリティ: デフォルト `t`)
  - `*read-suppress*` — `#+`/`#-` で抑制されたフォームの読み飛ばし時 `t`
  - `*read-default-float-format*` — 指数なし浮動小数点の型 (`single-float`/`double-float`)
  - `(read &optional stream eof-error-p eof-value recursive-p)` — 完全シグネチャ
  - `(read-preserving-whitespace ...)` — 区切り文字を読み戻す
  - `(read-delimited-list char &optional stream recursive-p)` — `)` 等の区切り文字まで読む
  - `(read-char-no-hang &optional stream eof-error-p eof-value recursive-p)` — ノンブロッキング文字読み取り
  - `(peek-char &optional peek-type stream eof-error-p eof-value recursive-p)` — 先読み
  - `(unread-char char &optional stream)` — 1文字プッシュバック
- **根拠**: ANSI CL §23.1 全 `read` 関数・変数。REPL・パーサ・DSLの基盤
- **難易度**: Easy

---

### Phase 213 — Numeric I/O 完全対応

#### FR-1057: Numeric Literal Reader (数値リテラルリーダー)

- **対象**: `src/parse/lexer.lisp`
- **内容**:
  - `#b1010` — 2進数; `#o17` — 8進数; `#xFF` — 16進数
  - `#36rZZZ` — 任意基数 (2-36): `#nrNNN` 形式
  - `1.5e3` / `1.5d3` / `1.5f3` / `1.5l3` — 指数表記 (s=short, f=single, d=double, l=long)
  - `1/3` — 有理数リテラル
  - `#C(1.0 2.0)` — 複素数リテラル
  - `#*10110` — bit-vector リテラル
  - `1_000_000` — アンダースコア区切り (読みやすさ用、非ANSI拡張)
  - `+inf.0` / `-inf.0` / `+nan.0` — 無限大・非数リテラル (SBCL拡張)
  - `(float-precision x)` — 仮数部ビット数; `most-positive-double-float` 等の定数
- **根拠**: ANSI CL §2.3 数値シンタックス。全数値リテラル形式の正確な読み取り
- **難易度**: Easy

---

### Phase 214 — 動的ウィンド / リソース管理

#### FR-1060: dynamic-wind Semantics (動的ウィンドセマンティクス)

- **対象**: `src/vm/vm-execute.lisp`
- **内容**:
  - `(dynamic-wind before thunk after)` — Scheme 相当の before/after フック
  - CL の `unwind-protect` との対応: `(dynamic-wind pre body post)` ≈ `(unwind-protect (progn (pre) (body)) (post))`
  - 相違点: `dynamic-wind` は継続の再入 (re-entry) でも `before` を再実行
  - `call/cc` との統合: 継続を escape して再呼び出しするたびに before/after が実行される
  - `(with-dynamic-extent form)` — `dynamic-wind` ベースのリソース管理マクロ
  - `(unwind-protect-with-restarts form cleanup)` — リスタートを保持したままのクリーンアップ
  - Racket `dynamic-wind` / R7RS `dynamic-wind` との互換性
- **根拠**: Scheme R7RS §6.10。フルcall/ccと組み合わせたリソース管理の正確なセマンティクス
- **難易度**: Hard

---

### Phase 215 — インタープロシージャ最適化

#### FR-1063: Interprocedural Optimization (インタープロシージャ最適化)

- **対象**: `src/optimize/optimizer.lisp`
- **内容**:
  - 呼び出しグラフ構築: `(build-call-graph toplevel-fns)` → call-graph DAG
  - 呼び出し側への定数伝播: `(foo 42)` で `foo` の本体が `x=42` として最適化される
  - 純粋関数の検出: 副作用なし + 参照透明 → `(foo 42)` を複数箇所からインライン
  - 使用されない関数の除去 (dead function elimination): 呼び出しグラフで到達不能な関数を削除
  - 部分インライン: 条件分岐の一方のブランチだけをインライン（ガード条件の外側）
  - モノモーフィゼーション: 型既知の呼び出しサイト用に特殊化版を生成
  - `*ipo-budget*` — インタープロシージャ解析の計算予算（時間制限）
- **根拠**: GCC `-fwhole-program` / LLVM LTO。モジュール境界を越えた最適化
- **難易度**: Very Hard

---

### Phase 216 — SBCL 互換拡張 API

#### FR-1066: SBCL-Compatible Extensions (SBCL互換拡張API)

- **対象**: `src/vm/vm.lisp`
- **内容**:
  - `sb-ext:without-package-locks` → `cl-cc:without-package-locks` エイリアス
  - `sb-ext:gc` → `cl-cc:gc` (Phase 177)
  - `sb-ext:*heap-size*` → `cl-cc:*heap-size*`
  - `sb-ext:native-namestring` → `cl-cc:native-namestring`
  - `sb-ext:run-program` → `cl-cc:run-program` (Phase 198)
  - `sb-ext:finalize` → `cl-cc:finalize` (Phase 200)
  - `sb-ext:add-package-local-nickname` → `cl-cc:add-package-local-nickname` (Phase 204)
  - `sb-impl::*default-external-format*` → `cl-cc:*default-external-format*`
  - `*features*` リスト: `:cl-cc` / `:common-lisp` / `:ansi-cl` / `:x86-64` / `:unix` / `:darwin` / `:linux`
  - `(lisp-implementation-type)` → `"CL-CC"` / `(lisp-implementation-version)` → "0.1.0"
- **根拠**: SBCL は最も広く使われる CL 処理系。互換 API でポータビリティを確保
- **難易度**: Easy

---

### Phase 217 — ブロック・非局所脱出の完全実装

#### FR-1069: block / return-from Across Closures (クロージャ越し非局所脱出)

- **対象**: `src/vm/vm-execute.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `(block name ...)` / `(return-from name value)` の完全セマンティクス
  - クロージャ越し `return-from`: `(block outer (let ((f (lambda () (return-from outer 42)))) (funcall f)))` → 42
  - 実装: block は `catch` タグに変換; `return-from` は `throw` に変換（現状）
  - 最適化: 同一関数内での `return-from` はジャンプ命令に直接変換（`throw` 不要）
  - `(block nil ...)` + `(return ...)` の等価性: `(return val)` = `(return-from nil val)`
  - 非局所脱出と `unwind-protect` の相互作用: 脱出経路上の cleanup の実行
  - 無効な `return-from`: ブロックが既にリターンした後の `return-from` → `control-error`
- **根拠**: ANSI CL §5.2。CL の `loop` / `dolist` / `dotimes` は内部で `(block nil ...)` を使用
- **難易度**: Medium

#### FR-1070: tagbody / go Complete Semantics (tagbody/go完全セマンティクス)

- **対象**: `src/vm/vm-execute.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `(tagbody tag1 form1 tag2 form2 ...)` — タグ付きステートメント
  - `(go tag)` — 非ローカルジャンプ（`tagbody` 内の任意のタグへ）
  - クロージャ越しの `go`: `(let ((f (lambda () (go tag2)))) ...)` — キャプチャされた `go`
  - タグの種類: シンボル または 整数
  - `tagbody` の戻り値は常に `nil`
  - 末尾位置の定義: `tagbody` 内の `go` は末尾位置でない (TCO 不可)
  - `(do ...)` / `(dotimes ...)` / `(dolist ...)` の内部実装: `tagbody` への変換
  - `prog` / `prog*` の実装: `let`/`let*` + `tagbody` のシュガー
- **根拠**: ANSI CL §5.3。`do`/`loop` の基盤。CPS変換後のラベルジャンプとの対応
- **難易度**: Medium

---

### Phase 218 — 数値比較・等値の完全実装

#### FR-1073: Numeric Equality and Comparison (数値比較完全実装)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - `(= a b)` / `(/= a b)` / `(< a b)` / `(> a b)` / `(<= a b)` / `(>= a b)` — 任意型間の比較
  - 多引数版: `(< a b c d)` = `(and (< a b) (< b c) (< c d))` のショートサーキット評価
  - `(eql a b)` — 同一オブジェクト or 数値同値 (fixnum / float / complex)
  - `(equal a b)` — 再帰的構造比較 (cons/string/vector)
  - `(equalp a b)` — case-insensitive文字列 + 配列要素比較
  - `(= 1 1.0)` → t (型を超えた数値同値); `(eql 1 1.0)` → nil (型が異なる)
  - `(zerop x)` / `(plusp x)` / `(minusp x)` / `(oddp x)` / `(evenp x)` — 述語
  - `(max a b ...)` / `(min a b ...)` — 数値最大最小 (contagion rules 適用)
- **根拠**: ANSI CL §12.1.4。CL の等値述語の階層 (eq/eql/equal/equalp) は重要な設計概念
- **難易度**: Easy

---

### Phase 219 — 一般シーケンス操作完全実装

#### FR-1076: Sequence Operations Complete (シーケンス操作完全実装)

- **対象**: `src/vm/list.lisp`
- **内容**:
  - `(find item seq &key :key :test :test-not :start :end :from-end)` — 要素検索
  - `(position item seq &key ...)` → integer or nil
  - `(count item seq &key ...)` → integer
  - `(find-if pred seq &key ...)` / `(find-if-not pred seq &key ...)`
  - `(remove-duplicates seq &key :key :test :from-end :start :end)` — 重複除去
  - `(substitute new old seq &key :test :count :from-end)` / `(substitute-if new pred seq ...)`
  - `(nsubstitute ...)` / `(nsubstitute-if ...)` — 破壊的版
  - `(every pred seq &rest more-seqs)` / `(some ...)` / `(notany ...)` / `(notevery ...)` — 量化
  - `(mismatch seq1 seq2 &key :test :start1 :end1 :start2 :end2 :from-end)` → position
  - `(concatenate result-type &rest seqs)` — 任意のシーケンスの結合
  - `(coerce seq result-type)` — シーケンス型変換
- **根拠**: ANSI CL §17。シーケンス関数は CL の最も使われる標準ライブラリ関数群の一つ
- **難易度**: Medium

---

### Phase 220 — ハッシュテーブル SIMD 最適化

#### FR-1079: Hash Table SIMD Lookup (ハッシュテーブルSIMD高速化)

- **対象**: `src/vm/hash.lisp`
- **内容**:
  - 現状: オープンアドレス法 (linear probing)
  - 最適化: Swiss Table (Abseil) / F14 (Facebook) — SIMD による16スロット並列比較
  - x86-64 `SSE4.2 PCMPEQB` / ARM `NEON vceq` で16バイトのタグを一括比較
  - メタデータバイト: 各スロットに7ビットハッシュ値を格納 → ミスヒット時の早期脱出
  - 負荷率 87.5% まで効率的（通常のopenaddressing の70%より高い）
  - `(hash-table-size ht)` → 現在の容量; `(hash-table-count ht)` → 要素数
  - `(maphash fn ht)` — 反復関数; `(with-hash-table-iterator (next ht) body)` — イテレータ
  - `*hash-table-rehash-policy*` — `:swiss` / `:robin-hood` / `:chaining` ポリシー選択
- **根拠**: Swiss Table は Abseil/C++17 の標準。ルックアップ速度2-4x向上、キャッシュ効率大幅改善
- **難易度**: Hard

---

### Phase 221 — ソース位置追跡・デバッグ情報

#### FR-1082: Source Location Tracking (ソース位置追跡)

- **対象**: `src/parse/cl/parser.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `*load-pathname*` / `*load-truename*` — 現在ロード中のファイルパス (ANSI §23.1.2)
  - `*compile-file-pathname*` / `*compile-file-truename*` — コンパイル中ファイルパス
  - `(ed &optional x)` — エディタ起動 (ANSI §25.1.4)
  - `(dribble &optional pathname)` — セッションログ記録
  - ソース位置オブジェクト: `(make-source-location file line column)` + アクセサ
  - DWARF `.debug_line` セクション: ネイティブコードのPC → ソース行マッピング
  - `(source-location function)` → ソース位置 (Emacs `M-.` の基盤)
  - `(inspect object)` — 対話的オブジェクト調査 (ANSI §25.1.4)
  - `(describe object &optional stream)` — オブジェクト説明出力
- **根拠**: ANSI CL §25.1 デバッグ関連関数。開発環境の基本インフラ
- **難易度**: Medium

---

### Phase 222 — Trace / Step / Break デバッガ

#### FR-1085: trace / step / break (トレース・ステップ・ブレーク)

- **対象**: `src/vm/conditions.lisp`
- **内容**:
  - `(trace function-name ...)` — 関数呼び出しの自動ログ
  - `(untrace function-name ...)` / `(untrace)` — トレース解除
  - トレース出力: `0: (FOO 1 2)` → `0: FOO returned 3` の入れ子インデント
  - `*trace-output*` — トレース出力ストリーム
  - `(step form)` — ステップ実行モード (ANSI §25.1.4)
  - `(break &optional format-control &rest args)` — デバッガへのブレーク
  - `(invoke-debugger condition)` — `*debugger-hook*` 経由でデバッガ起動
  - `*debugger-hook*` — カスタムデバッガ関数
  - SLYNK/Swank 互換デバッグプロトコル: `slynk:create-server` の互換実装基盤
- **根拠**: ANSI CL §25.1 `trace`/`step`/`break`/`inspect`/`describe`。CL開発の必須デバッグツール
- **難易度**: Medium


---

### Phase 223 — 数値出力アルゴリズム

#### FR-1088: Ryu Float-to-String (Ryu浮動小数点→文字列)

- **対象**: `src/vm/io.lisp`, `src/vm/primitives.lisp`
- **内容**:
  - Ryu アルゴリズム (Ulf Adams, 2018): double → 最短の十進数表現
  - 性質: `(= (parse-float (float-to-string x)) x)` が常に成立する最短表現
  - `Grisu3` / `Dragon4` より 2-3x 高速
  - 特殊値: `+inf.0` / `-inf.0` / `+nan.0` の出力
  - `(format t "~F" x)` / `(write x)` のバックエンドとして使用
  - `:e` (指数) / `:f` (固定) / `:g` (最短) モード
  - 単精度 `float` と倍精度 `double` の両対応
  - ベンチマーク比較: Ryu vs Grisu3 vs Dragon4 vs Steele&White
- **根拠**: Ryu は現在最速・最短の double→string アルゴリズム。V8/Dart/Swift が採用
- **難易度**: Medium

---

### Phase 224 — コンパイル時 CLOS 最適化

#### FR-1091: Monomorphic Inline Cache for slot-value (スロットアクセス単相インラインキャッシュ)

- **対象**: `src/vm/vm-clos.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `(slot-value obj 'name)` の呼び出しサイトに MIC (Monomorphic Inline Cache) を設置
  - キャッシュヒット: `class-of(obj) == cached-class` → スロットインデックス直接参照
  - キャッシュミス: 通常のスロット検索 + キャッシュ更新
  - PIC (Polymorphic Inline Cache): 最大4クラスをキャッシュ
  - メガモーフィック: 5クラス以上 → グローバルハッシュテーブルへ格帰
  - `(with-slots (a b c) obj body)` の展開先も MIC に変換
  - `(slot-value-using-class class obj slotd)` MOP フック経由でも同様に最適化
- **根拠**: Self VM の Polymorphic Inline Cache。CLOS スロットアクセスのホットスポット削減
- **難易度**: Hard

---

### Phase 225 — 弱ポインタ完全実装

#### FR-1094: Weak Pointers (弱ポインタ完全実装)

- **対象**: `src/runtime/gc.lisp`
- **内容**:
  - `(make-weak-pointer object)` → weak-pointer
  - `(weak-pointer-value wp)` → `(values object valid-p)` — GC済みなら `(values nil nil)`
  - `(weak-pointer-p x)` → boolean
  - GC 処理: major GC sweep フェーズで weak pointer の target が白なら nil に設定
  - 弱ハッシュテーブル:
    - `(make-weak-hash-table :weakness :key)` — キーが死ぬとエントリ削除
    - `(make-weak-hash-table :weakness :value)` — 値が死ぬとエントリ削除
    - `(make-weak-hash-table :weakness :key-and-value)` — 両方死ぬと削除 (ephemeron)
    - `(make-weak-hash-table :weakness :key-or-value)` — どちらか死ぬと削除
  - `trivial-garbage:make-weak-pointer` との互換性
- **根拠**: SBCL `sb-ext:make-weak-pointer`。インターニングキャッシュ・イベントリスナーのメモリリーク防止
- **難易度**: Hard

---

### Phase 226 — ネイティブスレッド完全実装

#### FR-1097: Native Thread API (ネイティブスレッドAPI)

- **対象**: `src/vm/vm.lisp`
- **内容**:
  - `(make-thread function &key name)` → thread オブジェクト
  - `(thread-join thread &optional timeout)` → 戻り値
  - `(thread-name thread)` → string
  - `(current-thread)` → 現在スレッド
  - `(thread-alive-p thread)` → boolean
  - `(all-threads)` → thread list
  - `(interrupt-thread thread function)` — 他スレッドへの割り込み (SBCL互換)
  - `(destroy-thread thread)` — 強制終了（非推奨）
  - `(thread-yield)` — スケジューラへのCPU譲渡
  - Bordeaux-Threads API 互換: `bt:make-thread` / `bt:join-thread` / `bt:current-thread`
  - GIL (Global Interpreter Lock) 排除: 各スレッドが独立した GC state を持つ
- **根拠**: Bordeaux-Threads は CL の事実上の標準スレッドライブラリ。互換API で移植性確保
- **難易度**: Hard

---

### Phase 227 — 標準入出力・ターミナル制御

#### FR-1100: Terminal Control (ターミナル制御)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `(make-ansi-stream stream)` — ANSI エスケープシーケンス対応ストリーム
  - `(ansi-color stream :red)` / `(ansi-reset stream)` — 文字色・スタイル設定
  - `(terminal-size)` → `(values columns rows)` (`TIOCGWINSZ` ioctl)
  - `(isatty stream)` → boolean — ターミナル接続確認
  - `(with-raw-terminal body)` — raw モード (行バッファリング無効): `tcsetattr(3)` 使用
  - `(read-key)` → keypress event — single keystroke 読み取り
  - `*terminal-width*` — `*print-right-margin*` のデフォルト設定に使用
  - Readline互換行編集: `(readline prompt)` → string（履歴・補完付き）
- **根拠**: CLIツール・REPL・TUIアプリケーションのターミナル操作基盤
- **難易度**: Medium

---

### Phase 228 — Printing 循環構造

#### FR-1103: Circular Structure Printing (循環構造印字)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - `*print-circle*` = t での循環・共有構造の検出と印字
  - 検出アルゴリズム: 2パス — 第1パスで共有オブジェクトを `#n=` でラベル付け、第2パスで印字
  - `#1=(a b . #1#)` — 循環リストの表現
  - `(let ((x (list 1 2))) (list x x))` → `(#1=(1 2) #1#)` — 構造共有の表現
  - `write-circle-table` — 循環/共有オブジェクトのインデックステーブル
  - `*print-circle*` = nil でも循環リストを安全に印字（`...` で打ち切り）
  - `(print-object ...)` カスタムメソッドと `*print-circle*` の統合
- **根拠**: ANSI CL §22.1.2。循環構造のデバッグ・シリアライズに必須
- **難易度**: Medium

---

### Phase 229 — ローカル宣言の完全実装

#### FR-1106: Local Declarations Complete (ローカル宣言完全実装)

- **対象**: `src/expand/expander.lisp`, `src/compile/codegen.lisp`
- **内容**:
  - `(declare (ignore x))` — 未使用変数の警告抑制
  - `(declare (ignorable x))` — 使用されなくても OK
  - `(declare (type fixnum x))` — ローカル型宣言
  - `(declare (special *x*))` — ローカルスペシャル変数宣言
  - `(declare (optimize (speed 2) (safety 1)))` — ローカル最適化ポリシー
  - `(declare (dynamic-extent x))` — スタック割り当て宣言
  - `(declare (notinline f))` — ローカルインライン抑制
  - `locally` 特殊形: `(locally (declare (optimize (speed 3))) body)` — 宣言のみの局所スコープ
  - 複数 `declare` フォームの合成: 同一 `let` 内の複数 `declare` は全て有効
- **根拠**: ANSI CL §3.3 `declare`。警告制御・型最適化・スタック割り当ての標準手段
- **難易度**: Medium

---

### Phase 230 — コードウォーカー・マクロ展開完全実装

#### FR-1109: macroexpand-all / Code Walker (コードウォーカー)

- **対象**: `src/expand/expander.lisp`
- **内容**:
  - `(macroexpand form &optional env)` — ANSI CL `macroexpand` (再帰展開、マクロでなくなるまで)
  - `(macroexpand-1 form &optional env)` — 1ステップのみ展開
  - `(macroexpand-all form &optional env)` — 全サブフォームを再帰的に展開 (SBCL `sb-cltl2:macroexpand-all`)
  - コードウォーカー: `(walk-form form env visitor)` — 全サブフォームを訪問
  - `visitor` コールバック: `(fn form env context)` → `(values new-form walk-children-p)`
  - `context`: `:eval` / `:macro` / `:function` / `:variable` — 出現コンテキスト
  - `:macro-function` と `:compiler-macro-function` の両方を展開
  - `symbol-macrolet` / `macrolet` のスコープを正確に追跡
- **根拠**: SBCL `sb-cltl2:macroexpand-all` / CLTL2 code walker API。静的解析・最適化・変換ツールの基盤
- **難易度**: Hard

---

### Phase 231 — 数値最大最小・クランプ

#### FR-1112: Numeric Clamp / Range (数値クランプ・範囲)

- **対象**: `src/vm/primitives.lisp`
- **内容**:
  - `(clamp x min max)` = `(max min (min max x))` — 範囲クランプ（非ANSI拡張）
  - `(wrap x min max)` — 範囲外を折り返す: `(mod (- x min) (- max min)) + min`
  - `(lerp a b t)` = `(+ a (* t (- b a)))` — 線形補間
  - `(abs x)` — ANSI CL §12.2 (全数値型)
  - `(signum x)` → -1 / 0 / 1 (整数) or -1.0 / 0.0 / 1.0 (浮動小数点)
  - `(expt base power)` — 整数・有理数・複素数・浮動小数点の全組み合わせ
  - `(log x)` / `(log x base)` — 自然対数・任意底対数
  - `(sqrt x)` / `(isqrt x)` — 平方根（複素数 `(sqrt -1)` = `#C(0 1)`）
- **根拠**: ANSI CL §12.2。数値演算関数の完全実装
- **難易度**: Easy

---

### Phase 232 — ガベージコレクタ 精密スタックマップ

#### FR-1115: Precise Stack Maps (精密スタックマップ)

- **対象**: `src/runtime/gc.lisp`, `src/backend/x86-64-codegen.lisp`
- **内容**:
  - 各コールサイト/safepoint での「どのレジスタ・スタックスロットがポインタか」の情報
  - `.gc_map` セクション: PC → ポインタビットマップのテーブル
  - JIT コード生成時: ポインタを持つレジスタに印をつけた stack map を生成
  - GC スキャン時: RIP でテーブルをルックアップ → 精密なルート集合
  - 保守的GC (Boehm GC) との比較: 偽陽性ポインタなし = GCアキュレート
  - `(emit-gc-safepoint)` — JIT コードに safepoint を挿入するプリミティブ
  - compressed stack map: 差分エンコーディングで `.gc_map` サイズを削減
- **根拠**: HotSpot の OopMap / V8 の SafepointTable。精密GCの必須要件
- **難易度**: Very Hard

---

### Phase 233 — ランタイム型検査最適化

#### FR-1118: typep Fast Dispatch (typep高速ディスパッチ)

- **対象**: `src/vm/vm-execute.lisp`
- **内容**:
  - `(typep x 'fixnum)` → NaN-boxing タグを1ビットマスクで確認 (1命令)
  - `(typep x 'cons)` / `(typep x 'null)` / `(typep x 'symbol)` — タグ直接比較
  - `(typep x 'standard-object)` — インスタンスヘッダの class-id チェック
  - `typecase` → ジャンプテーブル最適化: 型タグをインデックスに変換
  - `(typecase x (fixnum ...) (float ...) (cons ...) (t ...))` → switch文相当
  - compound types: `(typep x '(integer 0 100))` → 2回の比較
  - `(typep x '(or fixnum float))` → タグのビットOR
  - `(typep x '(satisfies evenp))` → `(evenp x)` 呼び出し（最適化なし）
- **根拠**: SBCL の型ディスパッチ最適化。`typecase` はパーサ・ディスパッチャのホットパス
- **難易度**: Medium

---

### Phase 234 — ストリーム完全実装 (Gray Streams)

#### FR-1121: Gray Streams Protocol (Grayストリームプロトコル)

- **対象**: `src/vm/io.lisp`
- **内容**:
  - Gray Streams: ユーザー定義ストリームの標準拡張プロトコル
  - `stream-read-char` / `stream-unread-char` / `stream-read-char-no-hang` / `stream-peek-char`
  - `stream-read-line` / `stream-read-sequence`
  - `stream-write-char` / `stream-write-string` / `stream-write-sequence`
  - `stream-fresh-line` / `stream-finish-output` / `stream-force-output` / `stream-clear-output`
  - `stream-advance-to-column` — pretty-printer 連携
  - `(defclass my-stream (fundamental-character-input-stream) ...)`
  - デフォルト実装: `stream-read-line` → `stream-read-char` の繰り返しなど
  - `:trivial-gray-streams` ライブラリとの互換性
- **根拠**: Gray Streams は SBCL/CCL/ECL/ABCL 全てが実装。カスタムストリームの標準API
- **難易度**: Medium

---

### Phase 235 — 完全な数列反復子

#### FR-1124: LOOP Arithmetic Sequence Optimization (LOOPシーケンス最適化)

- **対象**: `src/expand/macros-sequence.lisp`
- **内容**:
  - `(loop for i from 0 below n collect i)` → `(iota n)` への変換（最適化）
  - `(loop for i of-type fixnum from 0 below n ...)` — 型宣言付き反復変数
  - `(loop for (a . b) in alist do ...)` — cons デストラクチャリング
  - `(loop for x across vector ...)` の配列アクセス最適化: `(aref v i)` 直接生成
  - `(loop sum (* x x) for x in list)` — `sum` 蓄積の型推論 (fixnum sum → fixnum)
  - `(loop for key being each hash-key of ht using (hash-value val))` — フル構文
  - `(loop with result = nil do ... finally (return result))` — finally/return統合
  - ネストループの最適化: 内側の i/j 反復変数をレジスタに保持
- **根拠**: ANSI CL §6.1.2 LOOP の細かい最適化。数値集約ループの性能が重要
- **難易度**: Medium

