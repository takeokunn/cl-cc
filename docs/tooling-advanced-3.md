# Tooling: Advanced Compilation III

GC enhancements, string/symbol/numeric optimization, pattern matching, register allocation, V8-style objects, security, static analysis, GHC-style transforms, stack/thread management, SIMD, ABI/symbol management, Lisp-specific optimization, compiler robustness, frontend extensions, debug/profiling, I/O/OS integration, macros/metaprogramming, REPL, FFI, library/distribution, documentation/quality tools.

---
### Phase 129 — GC高度化

#### FR-716: Colored Pointer GC / ZGC-Style (カラードポインタGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: 2世代GC（FR-GC系）はstop-the-world。FR-552（Concurrent GC）は基本的な並行マーキング
- **内容**: 64ビットポインタの上位ビット（x86-64の未使用46〜63ビット）に **GCメタデータ**（marked/remapped/finalizable フラグ）を格納。参照のたびにカラービットを確認（**ロードバリア**: 1命令）。リロケーション中もミュータントは古いアドレスを使え、ロードバリアが自動的に新アドレスに転送。STWポーズがサブミリ秒（<1ms）を実現。OpenJDK ZGC（2018）/ C4 GC（Azul Systems）と同等の設計
- **根拠**: selfhostingコンパイルのGCポーズ削除。長時間実行サーバー（REPL・LSP server）でのレイテンシ安定化
- **難易度**: Very Hard

#### FR-717: SATB Write Barrier (スナップショット書き込みバリア)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: FR-552（Write Barrier）はcard-table方式の世代間参照追跡。並行マーキング中の浮きゴミ問題への対策なし
- **内容**: **SATB (Snapshot-At-The-Beginning)** バリア: 並行マーキング開始時の到達可能集合スナップショットを基準に収集。書き込み `(setf (slot-value obj :ref) new-val)` 時に **旧値**をSATBキューにバッファリング。マーキングスレッドがSATBキューをドレインして旧値を遅延マーク。三色不変条件（white/gray/black）の維持。G1 GC / Shenandoah のSATBバリアと同等。FR-716（ZGC）のロードバリアと比較: SATBはストアバリア
- **根拠**: 並行GCの正確性保証。マーキング中に死んだオブジェクトへの参照が見落とされる「Yuasa bug」を防止
- **難易度**: Very Hard

#### FR-718: Region-Based GC / G1-Style (リージョンベースGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: ヒープは連続する若/老世代の2分割。大規模ヒープでは単一のfull GCポーズが長大化
- **内容**: ヒープを等サイズの **リージョン**（1MB）に分割。各リージョンが独立した世代を持ち（eden/survivor/old）、ガベージが多いリージョンを優先して収集（**Garbage First**）。STWポーズを予測可能な上限時間内（`--gc-pause-target 50ms`）に制限。Remembered Sets でリージョン間参照を追跡。大オブジェクトは専用Humongous リージョンに配置。OpenJDK G1 GC（2006）/ Microsoft CLR region GC と同等
- **根拠**: 大規模Lispプログラム（数GBヒープ）でのGCポーズ予測可能化。現在の単純2世代GCは8GB超でポーズが秒単位になる可能性
- **難易度**: Very Hard

#### FR-719: Epsilon GC / No-Op GC (ノーオプGC)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `cli/src/main.lisp`
- **現状**: GCは常に有効。テスト・ベンチマーク時のGCオーバーヘッドがノイズになる場合がある
- **内容**: `--gc epsilon` フラグでGC完全無効化（アロケートするだけで収集しない）。メモリ枯渇時に `Out of Memory` エラー終了。用途: (a) 短命プロセス（コンパイル1回実行）でGCオーバーヘッドゼロ、(b) GCなしのベンチマーク基準測定、(c) FR-606（No-Allocator Mode）との比較検証。OpenJDK Epsilon GC（JEP 318, 2018）/ .NET NoGC region と同等。ヒープ上限（`--max-heap 2g`）との組み合わせが前提
- **根拠**: コンパイル時間ベンチマークにGCポーズのノイズが混入することを完全排除。単発コンパイルタスクで最高スループット達成
- **難易度**: Easy

---

### Phase 130 — 文字列・シンボル・数値最適化

#### FR-722: String Interning (文字列インターン)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/engine/vm/src/symbols.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 文字列は毎回新規ヒープオブジェクト。`(equal "foo" "foo")` は内容比較（O(n)）。コンパイラ内部での識別子文字列が多数重複
- **内容**: `(cl-cc:intern-string s)` でグローバルインターンテーブルに登録し正準化オブジェクトを返す。インターン済み文字列同士の等価判定は `eq`（ポインタ比較、O(1)）。コンパイラが文字列リテラルをコンパイル時に自動インターン。GCはインターンテーブルを弱参照で保持（参照なし文字列は回収可能）。Java `String.intern()` / Lisp `intern`（シンボル用）の文字列版 / Python str interning と同等
- **根拠**: パーサ・シンボルテーブルで同一文字列が数千コピー存在する問題を解消。識別子比較を`eq`化してハッシュテーブルのkeyとして効率化
- **難易度**: Easy

#### FR-723: Small String Optimization / SSO (小文字列最適化)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 全文字列がヒープオブジェクト（ポインタ + 長さ + データバッファ）。1〜15文字の短い文字列でもヒープ割り当てが必要
- **内容**: 15バイト以下の文字列を **インラインで** ポインタ幅（64ビット）に収める。タグビットで「short string」を識別。`(string-length s)` と `(char s i)` が共通APIで透過的に動作。C++ `std::string` SSO / Rust `SmallString` / JavaScript V8 SeqOneByteString の手法。16バイト以上は通常ヒープ文字列にフォールバック
- **根拠**: Lispプログラムでは短い識別子文字列（変数名・スロット名）が大半。ヒープ割り当てとGCスキャンを排除しキャッシュ効率向上
- **難易度**: Hard

#### FR-724: Numeric Tower Optimization (数値塔最適化)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 整数演算は全てboxed fixnum/bignum。fixnumのオーバーフロー検出後のbignum昇格コストが毎演算で発生
- **内容**: **タグ付きfixnum高速パス**: 63ビット符号付き整数（最下位ビット=0）として表現し、加算/乗算後のオーバーフローをCF/OF フラグで検出（`JO` 命令）、オーバーフロー時のみbignum昇格パスへ。**専用fixnum演算**: `vm-add-fixnum` / `vm-mul-fixnum` で型チェック不要な高速パス。`(declare (fixnum n))` で型アノテーション付き変数は常にfixnum命令を使用。SBCL fixnum inline expansion / MIT Scheme fixnum optimization と同等
- **根拠**: 整数集中コード（ループカウンタ・配列インデックス・ハッシュ計算）でboxingコストをゼロに。コンパイラ内部の整数演算に即座に効果
- **難易度**: Hard

#### FR-725: Symbol Table Optimization (シンボルテーブル最適化)

- **対象**: `packages/engine/vm/src/symbols.lisp`, `packages/engine/vm/src/vm-execute.lisp`, `src/package.lisp`
- **現状**: シンボルルックアップはハッシュテーブル（`gethash`）。コンパイラの主要ホットパスがシンボル解決に費やすコストが大きい
- **内容**: **完全ハッシュ（perfect hashing）**: コンパイル済みパッケージの既知シンボルセットに対してMinimal Perfect Hash Function（MPHF）を生成（CHD algorithm）。ビルド時に `*package*` の全エクスポートシンボルからMPHFテーブルを事前計算しFASLに埋め込み。ルックアップはO(1)で衝突ゼロ。動的`intern`はフォールバックHT経由。`--emit-perfect-hash-tables` フラグ。gperf / CMPH / rust phf crate と同等
- **根拠**: 大規模パッケージ（cl-cc自身の~600エクスポート）でのシンボルルックアップ高速化。マクロ展開のhotパスがシンボル比較で多くの時間を消費
- **難易度**: Medium

---

### Phase 131 — パターンマッチ最適化

#### FR-728: Decision Tree Compilation for Pattern Matching (パターンマッチ決定木コンパイル)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `cond` / `typecase` / パターンマッチはフラットな連続比較に変換。共通テストの重複・最適な分岐木の構築なし
- **内容**: パターンマッチ式をSwitch Dispatch Matrix（SDM）として表現し、**Maranget のアルゴリズム**（ML 2008）で最適決定木にコンパイル。共通テスト（型タグ確認など）を引き上げて一度だけ実行。CLOSのmulti-dispatch（FR-CLOS系）のdispatcher生成にも適用。生成決定木の分岐回数を最小化（NP完全だが小規模パターンには厳密最適）。Maranget's "Compiling Pattern Matching to Good Decision Trees" (ICFP 2008) の実装
- **根拠**: 多くのCLOSメソッドや`ccase`/`etypecase`でテスト重複を排除。特に型タグが共通する分岐群でキャッシュ効率大幅改善
- **難易度**: Hard

#### FR-729: Pattern Usefulness / Redundancy Checking (パターン有用性・冗長性チェック)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: FR-085（パターンマッチ網羅性検査）は未カバーパターンを検出。到達不能パターン（redundant/useless）の警告なし
- **内容**: 各パターン節 `pᵢ` が `p₁...pᵢ₋₁` で既に網羅されていないか（**有用性検査**）を実行。`(case x (:foo ...) (:foo ...))` の2番目`:foo`は冗長 → コンパイル警告。**相互排他性検査**: 全パターンが互いに排他か確認。Maranget's usefulness algorithm / Haskell GHC exhaustiveness+redundancy / Rust match arms の実装を参考
- **根拠**: リファクタリング時に残留した到達不能パターンをゼロコストで検出。コードの信頼性向上
- **難易度**: Medium

#### FR-730: Tail Recursion Modulo Cons / TRMC (末尾再帰モジュロコンス最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/cps.lisp`
- **現状**: TCO（FR-005）は末尾位置の関数呼び出しのみ対象。`(cons x (recurse ...))` のように`cons`の中の再帰はスタック消費
- **内容**: `(defun map (f xs) (if (null xs) nil (cons (f (car xs)) (map f (cdr xs)))))` の末尾呼び出しでない再帰を最適化。コンスセルを事前割り当てし「穴」（CDRスロット）を残したまま次の反復に進み、後から埋める。結果: スタック増加なしで `map` を反復実行。`(declare (cl-cc:trmc))` で明示指定。Guy Steele / OCaml 5.1 TRMC (`[@tail_mod_cons]`) の実装
- **根拠**: `map`/`filter`/`append` 等の定義がTCOと同等の空間効率を得る。ユーザーが継続渡しスタイルに書き直す必要なし
- **難易度**: Hard

#### FR-731: Join Points (結合点)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 複数のブランチが同一コードに合流する場合、ブランチごとにコードを複製またはラベルジャンプ。最適化の障壁になる場合がある
- **内容**: **Join point** (`j`): 末尾位置でのみ呼ばれる局所関数として導入。`(if c (join k a) (join k b))` を `(letjoin k (x) ... x ...) (if c (k a) (k b))` に変換。join pointへのジャンプはスタックフレームなしの直接`jmp`命令にコンパイル（TCO的）。GHC join points（POPL 2017, Maurer et al.）のVM版。CPS継続 + ラベルジャンプの中間点として機能
- **根拠**: if-then-elseの合流コードを1コピーに保つ。インライン化後の爆発的コード複製を防止。最適化パスがjoin pointを超えて適用可能
- **難易度**: Hard

---

### Phase 132 — レジスタ割り当て高度化

#### FR-734: Register Coalescing (レジスタ合体)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/foundation/mir/src/mir.lisp`
- **現状**: コード生成後に`MOVE r1, r2`命令が多数残存。レジスタ割り当て後の冗長コピー除去なし
- **内容**: `(move dst src)` 命令を除去するため `dst` と `src` の live range を合体。**コンサーバティブ合体**: Chaitin-Briggs: 合体後の干渉グラフが彩色可能な場合のみ合体（着色数を増やさない）。**アグレッシブ合体**: George-Appel: 干渉がなければ無条件合体。合体後スピル増加を許容する場合は後退。FR-MIR層（FR-626 MLIR）のSSA形式とのBriggs/Cooper SSA-aware coalescing統合
- **根拠**: selfhostingコード生成後のコピー命令を50〜70%削減（実測値）。コンパイラ内でのレジスタスラッシング排除
- **難易度**: Hard

#### FR-735: Rematerialization (再実体化)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: レジスタスピル時は値をスタックに保存して後で復元。再計算が保存より安い場合でも常にspill/reload
- **内容**: スピル候補の値が「再実体化可能」（定数・単一演算・ループ不変）か判定。再実体化可能なら `reload` の代わりにspill siteで再計算。`(vm-const 42)` / `(vm-add base offset)` のような cheap 演算はspillよりremat有利。LLVM `MachineSink` + rematerialization / GCC `REG_REMATERIALIZABLE` と同等。FR-670（Constexpr）で評価済みの値は自動remat候補
- **根拠**: ループ内のspill/reloadをメモリアクセスゼロに。定数配列のベースアドレスなど、毎イテレーション再計算の方がL1ミスより速い
- **難易度**: Medium

#### FR-736: Instruction Selection via BURS (BURS命令選択)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/foundation/mir/src/target.lisp`
- **現状**: 命令選択はASTノードからの手続き的なコード生成。最適な命令列の系統的探索なし
- **内容**: **BURS（Bottom-Up Rewrite System）**: VM IR をツリーとして表現し、ターゲット命令パターンをリライトルール（`(add (load addr) reg) → ADD reg, [addr]` コスト2）として定義。動的計画法（DP）で各ノードのコスト最小カバリングを計算。x86-64の複合命令（`IMUL r, [mem]`・`LEA`・`CMOV`）を正しく選択。iburg / LLVM SelectionDAG / GCC RTL と同等の設計
- **根拠**: 手作りのコード生成よりも系統的に最適命令列を選択。LEAによる乗算・加算の統合など人間が見落とすパターンを自動発見
- **難易度**: Hard

#### FR-737: Anti-Dependence Breaking / Register Renaming (アンチ依存解消・レジスタ名前変更)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 命令スケジューリング（FR-033）はtrue-dependenceのみ追跡。アンチ依存（WAR: write-after-read）と出力依存（WAW: write-after-write）がスケジューリングを阻害
- **内容**: コンパイラによる **静的レジスタ名前変更**: WAR/WAW依存を新しい仮想レジスタに書き換えて偽依存を除去。`r0 = r1 + r2; r1 = r3 + r4` → `r0 = r1 + r2; r1_new = r3 + r4` で WAR解消。SSA形式に昇格すれば自動的に達成。スケジューリングウィンドウを3〜5命令から20命令に拡大。OoO CPU の動的リネーミング（ROB）が静的には重複しない最適なスケジュールを生成できる前提条件
- **根拠**: Out-of-Order CPU でのスケジューリング効果を最大化。静的リネームで実行ユニット利用率が10〜20%向上
- **難易度**: Medium

---

### Phase 133 — V8スタイルオブジェクト最適化

#### FR-740: Hidden Classes / Object Shapes (隠しクラス・オブジェクトシェイプ)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: CLOSインスタンスはスロットをハッシュテーブルで管理。スロットアクセスごとにハッシュ検索が発生（O(1)平均だがキャッシュ非友好）
- **内容**: 同一スロットセット（同一クラスの全インスタンス）を共有する **Shape オブジェクト** を導入。インスタンスはshapeポインタ + 固定オフセット配列で表現。`(slot-value obj :x)` → `(aref (instance-slots obj) (shape-slot-offset (instance-shape obj) :x))`。スロットの追加/削除時にshapeを遷移（transition tree）。JITで特定shapeを想定した高速スロットアクセスを生成（FR-JIT系）。V8 Hidden Classes / SpiderMonkey Shapes / WebKit Structure と同等
- **根拠**: CLOSインスタンスのスロットアクセスをハッシュO(1)から配列O(1)（キャッシュフレンドリー）に変換。JITの投機的インライン化（FR-560）とシナジー
- **難易度**: Hard

#### FR-741: Inline Method Dispatch Tables / IMT (インラインメソッドディスパッチテーブル)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: generic functionのディスパッチは型タグでspecializerを順次照合。大きなディスパッチテーブルでの線形探索
- **内容**: クラスごとに **インターフェースメソッドテーブル（IMT）** を生成。64エントリの固定サイズハッシュテーブル（メソッドセレクタ → 実装ポインタ）。ハッシュ衝突は conflict stub（linker-time dispatch）で解決。生成されたネイティブコード: `call [(class-imt obj)[selector-hash * 8]]`（2命令）。JVM Interface Method Table / .NET Interface Dispatch Stub と同等。FR-740（Shapes）と組み合わせてモノモーフィック呼び出しへの特化
- **根拠**: generic functionのディスパッチをO(n)からO(1)に。multi-dispatch（FR-CLOS多重ディスパッチ）のhotpathをJIT特化
- **難易度**: Hard

#### FR-742: Object Header Layout Optimization (オブジェクトヘッダレイアウト最適化)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/backend/runtime/src/heap.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: ヒープオブジェクトはLispのハッシュテーブルで実装。オブジェクトヘッダのレイアウト（GCマークビット・型タグ・ハッシュコード）が最適化されていない
- **内容**: 全ヒープオブジェクトに64ビット **mark word** を先頭フィールドとして配置: bits[0:1]=GC色（white/gray/black）, bits[2:3]=オブジェクト種別, bits[4:31]=identity hash code（遅延計算）, bits[32:63]=lock state / class index。GCがmark wordのビット操作のみでフェーズを管理（ヒープスキャン高速化）。FR-716（Colored Pointer GC）との協調: pointer-tag方式とmark-word方式を設定で切り替え
- **根拠**: Java HotSpot mark word / JVM object header の設計が30年の実績。単一64bit読み取りでGC状態・型・ロックを同時取得
- **難易度**: Hard

#### FR-743: Value Types / Inline Structs (値型・インライン構造体)

- **対象**: `packages/type/type/src/`, `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: 全オブジェクトはヒープ参照（ポインタ）。複合値（2D点・色・複素数）のフィールドがインライン格納できない
- **内容**: `(defvalue-type point2d (x :type single-float) (y :type single-float))` でスタック/レジスタにインライン格納可能な値型を宣言。コンストラクタがヒープ割り当てなしで2レジスタを返す（FR-569 Multiple Return Values）。配列内では `(array point2d 100)` が200要素の連続float配列としてレイアウト（AoS→SoA不要）。同一性比較は値比較。Java Project Valhalla（JEP 401）/ Swift value types / .NET struct と同等
- **根拠**: 座標・色・クォータニオン等の小さな複合値のヒープ割り当てゼロ。数値集中コードの根本的なメモリ効率改善
- **難易度**: Hard

---

### Phase 134 — セキュリティ高度化 II

#### FR-746: JIT Hardening / Code Integrity Protection (JIT強化・コード整合性保護)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: JITコードバッファ（FR-060）はW^Xで基本保護。JITスプレー攻撃・定数埋め込みのexploitation対策なし
- **内容**: **定数ブラインディング（Constant Blinding）**: JITコード内の即値定数を `const XOR random_key` として埋め込み、使用直前にXOR復元（Blazakis 2010）。**NOP sled 除去**: JIT生成コードに予測可能なpadding NOP列を生成しない。**Guard pages**: JITコードバッファの前後にGUARDページを配置しバッファオーバーランを即座に検出。**W⊕X 強化**: JITコード書き込み中は実行不可（mprotect切り替え）。V8 JIT hardening / WebKit JSC JIT protections と同等
- **根拠**: JIT-spray攻撃（CVE-2016-4622等）の緩和。セキュリティ重視環境でcl-ccを使用する前提条件
- **難易度**: Hard

#### FR-747: Runtime Sandboxing / Seccomp (ランタイムサンドボックス)

- **対象**: `cli/src/main.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: cl-cc プロセスは全syscallにアクセス可能。ユーザー提供コードの実行時にsyscall制限なし
- **内容**: `./cl-cc run --sandbox foo.lisp` で untrusted コードをサンドボックス内実行。Linux: `seccomp(SECCOMP_SET_MODE_FILTER)` で許可syscallをwhitelist（read/write/mmap/brk/exit のみ）。macOS: `sandbox_init(3)` / `pledge(2)` (OpenBSD互換レイヤ)。サンドボックス内でのネットワーク・ファイル書き込み・execveを禁止。違反時は`SIGKILL`ではなく`SIGSYS`でcontext情報付き終了。Seccomp-BPF / WASM sandbox / Deno の permission モデルを参考
- **根拠**: cl-cc REPL・オンライン評価サービス・プラグインシステムでの安全なコード実行。Turing-complete言語の評価に不可欠
- **難易度**: Hard

#### FR-748: Code Signing (コード署名)

- **対象**: `packages/backend/binary/src/macho.lisp`, `cli/src/main.lisp`
- **現状**: 生成バイナリに署名なし。macOS Gatekeeper・iOS / tvOSへのデプロイ不可
- **内容**: `./cl-cc compile --sign "Developer ID: ..." foo.lisp` で生成バイナリにコード署名。**macOS**: `codesign --sign` ラッパー、Mach-Oの`LC_CODE_SIGNATURE`ロードコマンド埋め込み。**Apple Silicon**: `MAP_JIT` フラグ使用時はentitlements（`com.apple.security.cs.allow-jit`）必須。**Windows**: Authenticode署名（`signtool.exe`）の呼び出しラッパー。**Linux**: IMA（Integrity Measurement Architecture）署名サポート。署名検証モード（`--verify-signature`）でロード時チェック
- **根拠**: macOS/iOS配布の必須要件。企業セキュリティポリシーへの準拠（署名なしバイナリの実行拒否）
- **難易度**: Medium

#### FR-749: Constant Blinding in AOT Code (AOTコードの定数ブラインディング)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: 即値定数が生成機械語にそのまま埋め込まれる。静的解析でアドレス・鍵・定数が直接読み取れる
- **内容**: AOT生成コードにおける敏感な定数（暗号鍵・ポインタ定数）をXORマスクで難読化。`MOV rax, 0xDEADBEEF` → `MOV rax, (0xDEADBEEF XOR 0x12345678); XOR rax, 0x12345678`。`--blind-constants` フラグで有効化。`(declare (cl-cc:sensitive-constant))` で個別定数に適用。FR-746（JIT Hardening）のAOT版。ASLR と組み合わせてコード内定数の予測困難化
- **根拠**: リバースエンジニアリング・バイナリパッチング・定数スキャンによるシークレット抽出への対策
- **難易度**: Medium

---

### Phase 135 — 静的解析高度化 II

#### FR-752: k-CFA / Context-Sensitive Analysis (k-CFA・文脈依存解析)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: エスケープ解析（FR-231）・ポインタ解析（FR-236）は文脈非依存（0-CFA相当）。同一関数が異なる呼び出し文脈で異なる挙動をする場合に精度不足
- **内容**: **1-CFA**: 呼び出しサイトを文脈として追跡（各呼び出し箇所で関数を別々に解析）。**Object Sensitivity**: 受信オブジェクトの割り当てサイトを文脈とする（OO言語に効果的）。解析精度と計算コストのトレードオフを `--cfa-depth 1` で制御。false positivesの削減によりエスケープ解析・エイリアス解析の精度向上。Shivers' k-CFA（1988）/ Milanova Object Sensitivity（2002）の実装
- **根拠**: 0-CFAでエスケープありと誤検出されるクロージャをスタック割り当て可能と正確に判定。最適化精度の根本的向上
- **難易度**: Very Hard

#### FR-753: Datalog-Based Static Analysis / Soufflé (データログベース静的解析)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, 新規 `src/analysis/datalog.lisp`
- **現状**: 静的解析は手続き的なVisitorパターンで実装。複数の解析間のデータ流通が複雑でメンテナンス困難
- **内容**: Souffle / Doop スタイルの **Datalog エンジン**を内蔵。解析を宣言的なDatalogルールとして記述: `reachable(A, B) :- call(A, B). reachable(A, C) :- reachable(A, B), call(B, C).`。底辺不動点計算で到達可能関数集合・エスケープ関係・エイリアス集合を同時計算。並列BFS/SCC計算で大規模プログラムにスケール。ルールファイルのホットリロード（`--reload-analysis`）で解析の拡張が容易
- **根拠**: コールグラフ構築・エスケープ解析・情報フロー解析を統一されたDeclarativeフレームワークで記述。解析の合成（compositionality）が自然
- **難易度**: Very Hard

#### FR-754: Ownership Inference / Borrow Checker (所有権推論・借用チェック)

- **対象**: `packages/type/type/src/`, `packages/engine/compile/src/codegen.lisp`
- **現状**: メモリ管理はGCに依存（FR-GC系）。所有権・借用の概念なし。GCを使わない安全なメモリ管理の検証不可
- **内容**: Rustスタイルの所有権型システムをオプトイン機能として追加。`(declare (cl-cc:owned x))` で所有権変数を宣言。`(cl-cc:borrow x)` で不変借用、`(cl-cc:borrow-mut x)` で可変借用（同時に1つのみ）。ライフタイムパラメータ: `(defun foo (x &lifetime 'a) (declare (cl-cc:lifetime x 'a)) ...)` で明示的ライフタイム。借用チェッカーはuse-after-free・double-freeをコンパイル時に検出。`--no-gc` モードと組み合わせてGCフリーモジュールを実現
- **根拠**: ヒープGCに依存しないリアルタイム・組み込み（FR-605）コードの安全性保証。Rust の安全性モデルをLispに段階的に導入
- **難易度**: Very Hard

#### FR-755: Region Inference (リージョン推論)

- **対象**: `packages/type/type/src/`, `packages/backend/runtime/src/heap.lisp`
- **現状**: FR-228（Arena Allocator）はスコープ単位の手動アリーナ管理。プログラム解析によるリージョン自動割り当て推論なし
- **内容**: ML Kit（Tofte & Talpin 1994）のリージョン推論: 各割り当てサイトに **リージョン注釈** を自動付与。リージョンの生存区間を解析してスタックベースの割り当て/解放を生成。GCなしで安全なメモリ管理。`(cl-cc:with-region r1 r2 ...)` でリージョンを明示。自動推論: `let r = inferRegion(expr) in allocate(in=r) ...`。リージョンプロファイリング（実行時リージョンサイズ統計）で推論精度を改善
- **根拠**: GCポーズなしの予測可能なメモリ管理。ML Kit実証: SMLプログラムの60〜80%でGCなし実行が可能
- **難易度**: Very Hard

---

### Phase 136 — GHCスタイル変換

#### FR-758: Demand / Strictness Analysis (需要・正格性解析)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 関数引数の評価タイミング（正格/遅延）をコンパイラが判定していない。FR-653（Lazy Eval）のthunkが常に生成される
- **内容**: **正格性解析**: 関数 `f x` が `x` を常に評価するか（**正格**）を解析。正格な引数はthunk生成なしに評価済み値を渡せる。**需要解析**: 引数が一度だけ評価されるか・全く評価されないかを追跡（シングル使用→移動、未使用→削除）。GHC Demand Analyzer（Peyton Jones et al.）の実装。`(declare (cl-cc:strict x))` で手動正格性宣言
- **根拠**: 遅延評価（FR-653）のサンク生成コストを正格引数で排除。GHC実績: 多くの関数の引数がほぼ全て正格と判定され、thunk生成を90%削減可能
- **難易度**: Hard

#### FR-759: Case-of-Case Transformation (case-of-case変換)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: ネストしたcond/typecase/パターンマッチがそのままコンパイル。外側のmatch結果を内側のmatchが再度検査する冗長性
- **内容**: `(case (case e p1→e1 p2→e2) q1→body1 q2→body2)` を `(case e p1→(case e1 q1→body1 q2→body2) p2→(case e2 q1→body1 q2→body2))` に変換（caseをpushdown）。変換後はFR-728（Decision Tree）がより良い決定木を生成。body重複はFR-731（Join Points）で共有。GHC case-of-case transformation（Simon Peyton Jones 1996）の実装
- **根拠**: CLOSディスパッチのネスト・型タグ検査の重複をコンパイル時に統合。JIT非依存で静的最適化可能
- **難易度**: Medium

#### FR-760: Let Floating / Binding Migration (let浮かし・束縛移動)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `let`束縛はソースコードの位置にコンパイル。ループ不変式をループ外に移動するLICM（FR-031）とは別にlet束縛レベルでの移動なし
- **内容**: **Float-Out（outward）**: 不変`let`をより外側のスコープ（ループ外・関数先頭）に移動。`(dotimes (i n) (let ((k (heavy-compute))) ...))` → `(let ((k (heavy-compute))) (dotimes (i n) ...))`。**Float-In（inward）**: 使用箇所に近い場所に束縛を移動し、使われないブランチでの評価を回避。`(let ((x e)) (if c x 0))` → `(if c (let ((x e)) x) 0)`（`c`が偽の場合`e`を評価しない）。GHC let-floating transformation / LICMの補完
- **根拠**: LICM がVM命令単位で動くのに対し、let-floatingはAST/CPS段階で高水準な動作。両者の協調で最大の効果
- **難易度**: Medium

#### FR-761: Simplification via Rewrite Rules (書き換えルールによる単純化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: 最適化ルールは`optimizer.lisp`の手続き的コード。ルール追加にソース変更が必要
- **内容**: `(define-rewrite-rule :simplify (+ x 0) x)` でユーザー定義書き換えルールを登録。GHC `RULES` pragma / Term Rewriting System（TRS）の設計。ルール: 左辺パターン → 右辺変換、適用条件（guard）付き。マッチングはE-graph（FR-egraph）またはLinear scan。組み込みルール: `(* x 0) → 0`, `(not (not x)) → x`, `(append nil x) → x`。ルールの合流性・停止性を検査する`--verify-rules`フラグ。Knuth-Bendix completion の簡易版
- **根拠**: ドメイン固有最適化（暗号・DSP・線形代数）をコンパイラ本体変更なしに追加可能。プラグイン（FR-700）との統合でユーザー定義最適化
- **難易度**: Medium

---

### Phase 137 — スタック・スレッド管理

#### FR-764: Segmented / Growable Stacks (セグメント化・伸長可能スタック)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: 各スレッド（FR-576 Work-Stealing）は固定サイズスタック。深い再帰でスタックオーバーフロー
- **内容**: **セグメント化スタック（Split Stacks）**: スタックフレームが現在セグメントに収まらない場合、新セグメントをヒープから割り当て。スタック上限なし（メモリ許す限り再帰可能）。各関数エントリに stack-overflow-check prologue を挿入（`cmp rsp, [tls_stack_limit]`）。**コピースタック（Copying Stack）**: スタック不足時に2倍サイズにrealloc（継続のアドレスを更新）。Go goroutine の growable stack / GCC Split Stacks (-fsplit-stack) / Chicken Scheme のcheney-on-the-mta と同等
- **根拠**: FR-576 Green Thread の根本的問題（スタックサイズ事前確定）を解消。相互再帰の深さ制限なし
- **難易度**: Hard

#### FR-765: Stack Overflow Detection / Guard Pages (スタックオーバーフロー検出)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `cli/src/main.lisp`
- **現状**: 深い再帰でSEGFAULT（シグナルハンドラなし）。デバッグ情報なしにクラッシュ
- **内容**: スタックの末尾に **ガードページ**（`mmap(PROT_NONE)`）を配置。アクセス時に`SIGSEGV` / `SIGBUS`を補足し、`(stack-overflow-error "Stack overflow at depth N: call chain ...")`に変換。バックトレース付きのエラーメッセージ生成。`--stack-size 8mb` でスタックサイズ設定。代替スタック（`sigaltstack`）でシグナルハンドラ自体がスタックオーバーフローしない設計。SBCL / JVM stack overflow handling と同等
- **根拠**: 現状の即死クラッシュをデバッグ可能なエラーに変換。テスト中の無限再帰バグの診断を劇的に改善
- **難易度**: Medium

#### FR-766: Trampolining (トランポリン最適化)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: TCO（FR-005）は末尾呼び出しのみ対応。相互再帰関数（A→B→A→B...）でTCOが適用できない実装がある
- **内容**: **トランポリン**: 末尾呼び出しを返り値として包み、ループで実行するパターンを自動生成。`(defun even (n) (if (= n 0) t (odd (- n 1))))` / `(defun odd (n) ...)` → `trampoline-loop`が`(lambda () (even n))`を繰り返し呼び出す。コンパイラが相互再帰を検出してトランポリンに変換。`(declare (cl-cc:trampoline))` で手動適用。`(cl-cc:make-thunk f)` / `(cl-cc:trampoline thunk)` の手動APIも提供。Scheme trampolining / Clojure `trampoline` 関数と同等
- **根拠**: 相互再帰での完全スタック定数化。Odd-Even テスト・状態機械実装・CPS変換後継続チェーンに直結
- **難易度**: Medium

#### FR-767: Coroutine Stack Management (コルーチンスタック管理)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: FR-166（Coroutines）は基本的なco-routine yield。スタック状態の完全保存/復元の詳細実装なし
- **内容**: コルーチン（FR-166）の**スタックフレーム完全スナップショット**: yield時にrsp/rbp/全callee-savedレジスタをコルーチンオブジェクトに保存。resume時に完全復元。**対称コルーチン** (`transfer-to`): 呼び出し元に戻らずに別コルーチンへ直接スイッチ。スタックコピー vs スタックポインタスワップの実装比較。`ucontext_t` / `longjmp` / POSIX makecontext/swapcontext との比較設計。spill map（FR-550 Stack Maps）との統合でGCがコルーチンスタックをスキャン可能
- **根拠**: 非同期I/O（FR-167 Async/Await）・Generator（FR-653 Lazy Eval）の実装基盤。スタック保存の詳細が正確でないとコルーチン跨ぎのGCが危険
- **難易度**: Hard

---

### Phase 138 — SIMD・スケジューリング拡張

#### FR-770: Masked Vector Operations / AVX-512 (マスクベクトル演算・AVX-512)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: SIMD（FR-035）は全要素処理のみ。条件付き要素処理（if-vectorize）が難しくif-conversionで諦めるケースあり
- **内容**: AVX-512マスクレジスタ（`k0`〜`k7`）を活用した条件付きベクトル演算。`(cl-cc:vmask (> a 0) (vm-add a b))` → `VADDPS zmm0{k1}, zmm1, zmm2` のように条件をマスクレジスタで表現。`VPMASKMOVD` でスキャッター/ギャザーのマスク付き版。FR-621（Auto-Parallelization）のif-body ベクトル化を可能に。ARM SVE `WHILELT`/`PTEST` 命令との統一インターフェース
- **根拠**: if-bodyを持つループの自動ベクトル化が2倍以上の適用範囲拡大。AVX-512搭載Intel Ice Lake / SPR での実性能向上
- **難易度**: Hard

#### FR-771: Scalable Vector Extension / SVE and RVV (スケーラブルベクトル拡張)

- **対象**: `packages/backend/emit/src/aarch64.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: SIMD（FR-035）は固定幅（128/256bit）。ARM SVE / RISC-V Vector（RVV）のスケーラブルベクトル幅未対応
- **内容**: **ARM SVE**: ベクトル長非依存コード生成（vscale=vector length / 128bit）。`PTRUE p0.s, ALL` + `LD1W z0.s, ...` + `FADD z0.s, ...` の VL-agnostic命令列。**RISC-V RVV**: `vsetvli t0, a0, e32, m1` でvlを動的設定し、`vle32.v v0, (a1)` / `vfadd.vv v2, v0, v1`。ループ後処理（epilogue）なしの自然なVL制御。ARMv9 SVE2 / RVV 1.0 spec 完全準拠
- **根拠**: Apple M4（SVE2）/ SiFive P870（RVV）での実行で128bit固定より2〜8倍高いSIMD幅を自動活用
- **難易度**: Hard

#### FR-772: Modulo Scheduling (モジュロスケジューリング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 命令スケジューリング（FR-033）は基本ブロック単位。ループカーネルの反復間命令オーバーラップなし
- **内容**: **ソフトウェアパイプライニングのカーネル生成**: 複数ループ反復の命令を **インターリーブ**して実行ユニット利用率を最大化。II（Initiation Interval）= `max(ResourceII, RecurrenceII)` を計算。Modulo Instruction Scheduling（MIS）アルゴリズムでprolog/kernel/epilogのコード生成。`(declare (cl-cc:software-pipeline))` で明示指定。FR-036（Software Pipelining）の精度向上版：完全なモジュロスケジューラ
- **根拠**: 数値計算ループでFPユニット・ロードユニットを同時フル活用。実測で2〜4倍のループスループット向上（レイテンシ隠蔽）
- **難易度**: Very Hard

#### FR-773: Instruction Throughput vs Latency Optimization (スループット・レイテンシトレードオフ最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 命令スケジューリングはレイテンシ最小化のみ。スループット（1サイクル当たりの命令完了数）の最大化なし
- **内容**: CPU実行ポートのモデル（Agner Fog's microarchitecture tables相当）を内蔵。AMD Zen4 / Intel Raptor Lake のポートマッピングデータ。スケジューラが **critical path length** vs **resource utilization** のバランスを最適化。実行ポートが偏る命令列を分散（`div`/`sqrt`は1ポートのみ: 前後に他ポート命令を配置）。LLVM `MachineCombiner` / GCC `-fsched-pressure` と同等
- **根拠**: Out-of-Order CPUの全実行ポートを均等に使うことで、シングルポートボトルネックを排除。FP集中コードで15〜30%の実効スループット向上
- **難易度**: Hard

---

### Phase 139 — ABI・シンボル管理

#### FR-776: Name Mangling (名前マングリング)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: 関数シンボル名は`cl_cc__function_name`形式の単純変換。generic function specializer・パッケージ・型パラメータのエンコードなし
- **内容**: Itanium C++ ABI マングリング規則（`_Z` prefix）に準拠したcl-cc独自マングリングスキーム定義。`(cl-cc:foo integer)` → `_Z5fooi`相当。パッケージ名: `cl_cc::bar::baz` → `_ZN6cl_cc3bar3bazE`。generic specializer: `(method add (integer integer))` → `_ZN3add_method_integer_integerE`。`./cl-cc demangle _Z5fooi` で逆変換。C++との相互運用時はC++ ABI マングリング規則に従ったシンボルも生成可能
- **根拠**: デバッガ（GDB/LLDB）・プロファイラ・objdumpでのシンボル可読性。Cライブラリとのリンク時の名前衝突回避
- **難易度**: Medium

#### FR-777: ABI Stability Manifest (ABI安定性マニフェスト)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `cli/src/main.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: ライブラリの公開APIのABIが変更された場合でも自動検出なし。破壊的ABI変更がサイレントに発生する可能性
- **内容**: `./cl-cc abi-dump foo.lisp > foo.abi` で公開関数のシグネチャ・struct レイアウト・enum値をABIダンプ。`./cl-cc abi-check foo.abi foo-new.lisp` でABI互換性検査: 関数シグネチャ変更・struct サイズ変更・enumの値変更を検出してエラー報告。セマンティックバージョン（SemVer）に対応: MAJOR変更 = ABI破壊、MINOR = ABI後方互換追加。`(declare (cl-cc:stable-abi))` で明示的ABI安定性宣言。libabigail / ABI Compliance Checker と同等
- **根拠**: cl-ccをライブラリとして使うプロジェクトへのABI保証。major versionをまたぐアップグレードの安全性確認
- **難易度**: Medium

#### FR-778: Debug Symbol Stripping Modes (デバッグシンボル除去モード)

- **対象**: `packages/backend/binary/src/macho.lisp`, `cli/src/main.lisp`
- **現状**: `--strip` フラグで全シンボル削除のみ。デバッグ情報の選択的保持・外部ファイル分離なし
- **内容**: `--strip all`: 全シンボルとデバッグ情報削除（最小バイナリ）。`--strip debug`: デバッグ情報のみ削除（公開シンボル保持）。`--strip unneeded`: 未定義参照に不要なシンボルのみ削除。`--split-debug`: FR-583（Split Debug Info）でバイナリとデバッグ情報を分離し、本番デプロイ+後からデバッグ可能に。`dSYM` (macOS) / `.dwp` (Linux) 形式での出力。`--debuglink` でバイナリとdebug fileのリンクを埋め込み
- **根拠**: 本番バイナリのサイズ最小化とデバッガビリティの両立。クラッシュレポートから後追いデバッグするワークフロー
- **難易度**: Easy

#### FR-779: Symbol Namespace Management (シンボル名前空間管理)

- **対象**: `src/package.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/symbols.lisp`
- **現状**: パッケージシステム（CL標準）は基本的な名前空間管理のみ。階層的名前空間・プライベートシンボル・リエクスポートの精緻な制御なし
- **内容**: `(cl-cc:define-namespace cl-cc.compiler.backend.x86 ...)` で階層的名前空間定義。`(cl-cc:private-symbol foo)` でパッケージ内限定シンボル（エクスポート不可）。`(cl-cc:reexport-from :dep :only (bar baz))` で選択的再エクスポート。循環依存検出: `(cl-cc:check-namespace-deps)` でパッケージ依存グラフのSCC検出。`--namespace-graph` でGraphviz出力。Java module system / Racket module system / OCaml module aliases の概念を統合
- **根拠**: cl-cc自身の600+エクスポートシンボルの管理が困難。内部実装シンボルと公開APIの明確な分離
- **難易度**: Medium

---

### Phase 140 — Lisp固有最適化

#### FR-782: Uncurrying / Eta Reduction (アンカリー化・イータ簡約)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: `(lambda (x) (f x))` のようなラッパーラムダがインライン化前に残存。不要な関数オブジェクト生成
- **内容**: **イータ簡約**: `(lambda (x) (f x))` → `#'f`（ラムダが引数をそのまま転送するのみ）をコンパイル時に検出して削除。`(mapcar (lambda (x) (1+ x)) xs)` → `(mapcar #'1+ xs)`。**アンカリー化**: 多引数関数を単一引数カリー化形式から変換（Haskellのuncurry変換のLisp版）。`(funcall (curry f a) b)` → `(f a b)`。FR-689（Lambda Lifting）と組み合わせでクロージャを完全排除
- **根拠**: `mapcar` / `reduce` / `sort` のような高階関数呼び出しで暗黙ラムダのクロージャ割り当てを排除。熱いループでのGCプレッシャー削減
- **難易度**: Easy

#### FR-783: Closure Shrinking (クロージャ縮小)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: クロージャは定義時スコープの全自由変数をキャプチャ。使用しない変数もキャプチャリストに含まれる
- **内容**: クロージャの自由変数リストをコード解析で精緻化し、実際に使用される変数のみをキャプチャ。`(let ((a 1) (b 2) (c 3)) (lambda () (+ a b)))` → cのキャプチャ不要。FR-231（Escape Analysis）との統合でキャプチャ変数がエスケープするかも解析。キャプチャ変数が0個 → クロージャから関数ポインタへ変換（FR-689 Lambda Lifting と協調）。クロージャのメモリフットプリントを最小化
- **根拠**: cl-ccのマクロ展開コード（多くのlet束縛を持つclosure）でキャプチャ変数を50%以上削減できるケースが多い
- **難易度**: Medium

#### FR-784: Predicate Dispatch Optimization (述語ディスパッチ最適化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: CLOSのmethod specializerは型・`eql`のみ。述語ベースのdispatch（Clojureのmultimethod like）の最適化なし
- **内容**: `(defmethod process ((x (and integer (> x 0)))) ...)` のような **述語specializer** をサポート。コンパイラが述語を事前評価し、型チェックと述語チェックの最適実行順序を決定木（FR-728）で生成。条件が排他的な場合は`cond`チェーンに変換、重複する場合は前向き連鎖（forward chaining）で効率化。Clojure multimethods / ELisp `cl-defmethod :extra` / Dylan sealing との比較設計
- **根拠**: 数値の範囲・文字列のパターン・複合条件でのdispatchを型ベースdispatchと同等の速度で実行
- **難易度**: Hard

#### FR-785: Uniqueness Types (一意型)

- **対象**: `packages/type/type/src/`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 参照共有は無制限。同一オブジェクトへの複数参照があると**インプレース更新**が安全に行えない（副作用が予期せず波及）
- **内容**: `(: sort-unique! ((unique (array integer)) -> (unique (array integer))))` で `unique` 型アノテーション付き関数を定義。`unique` 値は最大1つの参照のみ許可（コンパイル時チェック）。`unique` 値への破壊的操作（`setf`・`aref=`）が安全（他の参照がない保証）。GCなしでインプレース更新を安全化。`copy-out`: 一意性を諦めて共有可能にする変換。Clean 言語の uniqueness types / Idris 2 linear types との比較
- **根拠**: 配列ソート・文字列処理・バイトバッファ操作でコピー不要の破壊的更新を型安全に実現。FR-659（COW）の型システムレベルでの強化
- **難易度**: Hard

---

### Phase 141 — 数値・浮動小数点拡張

#### FR-788: FMA / Fused Multiply-Add Instructions (融合積和演算)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **現状**: `(+ (* a b) c)` は乗算命令 + 加算命令の2命令。中間値の丸め誤差あり
- **内容**: `(cl-cc:fma a b c)` → `VFMADD213SD xmm0, xmm1, xmm2`（x86-64 FMA3）/ `FMADD d0, d1, d2, d3`（AArch64）で単一命令実行。丸めが1回のみ（IEEE 754-2008 fusedMultiplyAdd 準拠）で数値精度向上。コンパイラが `(+ (* a b) c)` パターンを自動認識してFMAに変換（`--fuse-fma` フラグ）。SIMD版: `VFMADD213PS ymm0` で8要素同時FMA（FR-035 SIMD統合）。`--strict-fp` 時はFMA融合を禁止（精度優先）
- **根拠**: 行列乗算・ドット積・多項式評価でFMAが乗算+加算の2命令を1命令に置換。スループット2倍 + 丸め誤差半減
- **難易度**: Medium

#### FR-789: FP Reassociation / Fast-Math Mode (浮動小数点再結合・高速数学モード)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 浮動小数点演算はIEEE 754厳密セマンティクス。結合則不成立（`(a+b)+c ≠ a+(b+c)` for FP）のため再配置不可
- **内容**: `--fast-math` フラグで非厳密FP最適化を有効化。`-fno-signed-zeros`: `-0.0 == 0.0`。`-ffinite-math-only`: NaN/Inf発生なし前提。`-fassociative-math`: 和の順序変更許可 → 自動ベクトル化（FR-035）の精度条件を緩和。`-fno-rounding-math`: 丸めモード変更なし前提。`(declare (cl-cc:fast-math))` で関数単位適用。GCC `-ffast-math` / Clang `-ffast-math` / Julia `@fastmath` と同等
- **根拠**: 科学計算コードで`--fast-math`により20〜40%高速化が一般的。数値精度より速度優先の用途に
- **難易度**: Easy

#### FR-790: Half-Precision / BF16 Support (半精度・BF16浮動小数点サポート)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **現状**: 浮動小数点は64ビット倍精度のみ。AI推論・グラフィクス向けの16ビット浮動小数点なし
- **内容**: `cl-cc:float16` (IEEE 754 half-precision, 5指数+10仮数ビット) と `cl-cc:bfloat16` (Google BF16, 8指数+7仮数ビット) を型システム（FR-type系）に追加。x86-64: AVX-512 FP16命令 (`VADDPH`, `VCVTPS2PH`) を活用。AArch64: ARMv8.2 `FADD h0, h1, h2` / SVE2 FP16。`(cl-cc:the float16 x)` アノテーションで自動変換。推論専用モード: `(declare (cl-cc:quantize :bf16))` でモデル全体をBF16化
- **根拠**: LLM推論でBF16はFP32比メモリ半減・演算2倍速。NPU/GPU ターゲット（FR-647）との型整合性
- **難易度**: Medium

#### FR-791: Interval Arithmetic (区間演算)

- **対象**: 新規 `src/numeric/interval.lisp`, `packages/type/type/src/`
- **現状**: 浮動小数点誤差の上界が不明。数値計算の信頼性保証なし
- **内容**: `(cl-cc:interval lo hi)` で区間値を表現。演算: `(+ [a,b] [c,d]) = [a+c, b+d]`、`(* [a,b] [c,d]) = [min(ac,ad,bc,bd), max(...)]`。IEEE 754 丸め方向制御（`FLDCW` / `FRNDINT`）で厳密な上界・下界を計算。`(cl-cc:guaranteed-result expr)` で式の値の誤差上界を返す。MPFI / Arith ライブラリ相当を純CLで実装。`(declare (cl-cc:verified-fp))` で自動区間追跡モード
- **根拠**: 数値検証・物理シミュレーション・金融計算での丸め誤差保証。「計算結果が真値の ε 以内」を証明可能に
- **難易度**: Hard

---

### Phase 142 — コンパイラ堅牢性

#### FR-794: Compiler Fuzzing / Random Program Generation (コンパイラファジング)

- **対象**: `cli/src/main.lisp`, 新規 `src/testing/fuzzer.lisp`
- **現状**: コンパイラのバグ発見はユーザー報告・既存テストのみ。ランダム入力によるコンパイラクラッシュ・誤コード生成の体系的探索なし
- **内容**: `./cl-cc fuzz --seed 42 --count 10000` でランダムなCL式を生成してコンパイル。生成戦略: 文法ベース生成（産生規則から再帰的に構築）+ 変異ベース（既存テストケースを変異）。**差分テスト**: 同じ式をSBCL・cl-ccの両方で実行し結果不一致を報告（FR-453 Differential Testingの強化版）。`--oss-fuzz` モードでGoogle OSS-Fuzz統合（libFuzzer ABI互換）。見つかったバグをminimize（FR-796 Creduce）して自動バグレポート生成
- **根拠**: GCC/Clang は継続的なコンパイラファジングで年間数百のバグを発見。selfhosting後のコンパイラ正当性保証に不可欠
- **難易度**: Hard

#### FR-795: Translation Validation (翻訳検証)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 最適化パスの正しさはテスト（4322件）で担保。個々の変換ステップの意味論的等価性確認なし
- **内容**: 各コンパイルパス（CPS変換・最適化・コード生成）の**入出力ペアを自動検証**。Necula（1998）の Translation Validation: 変換前後のプログラムをSMTソルバー（FR-246）に送出し等価性をチェック。等価でない場合はコンパイラバグとして即座に報告。`--validate-transforms` フラグで有効化（コンパイル時間3〜10倍）。CI環境でサンプリング実行（全入力の1%）
- **根拠**: 最適化バグは「正しく動いていた関数が最適化後に壊れる」という最も発見困難な種類。変換ごとの自動検証が根本解
- **難易度**: Very Hard

#### FR-796: Test Case Reduction / C-Reduce Style (テストケース縮小)

- **対象**: `cli/src/main.lisp`, 新規 `src/testing/reducer.lisp`
- **現状**: コンパイラバグ報告時、ユーザーが手動でテストケースを縮小。数百行のコードが最小化されないまま報告
- **内容**: `./cl-cc reduce --crash bug.lisp` でバグを再現しつつソースを自動縮小。縮小戦略: トップレベルフォーム削除 → 式の単純化（`(complex-expr)` → `nil`）→ 識別子の短縮 → 定数の縮小。Δ-デバッグ（Andreas Zeller 1999）/ C-Reduce（PLDI 2012）のアルゴリズムを実装。`--property "crash"` / `--property "wrong-output"` で縮小目標を指定。縮小済みケースを `./cl-cc bug-report` で自動提出
- **根拠**: コンパイラバグの再現コードが数百行→数行に縮小されることで修正速度が10倍向上。GCC / Clang での実証済み
- **難易度**: Medium

#### FR-797: Compile-Time Assertions / static_assert (コンパイル時アサーション)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `assert` は実行時のみ。型システム（FR-type系）の制約以外のコンパイル時検証なし
- **内容**: `(cl-cc:static-assert (= (cl-cc:size-of my-struct) 16) "struct must be 16 bytes")` でコンパイル時評価。評価失敗時はコンパイルエラー（ファイル・行番号付き）。対象: 型サイズ検証・定数式の範囲チェック・機能フラグ依存チェック・プラットフォーム仮定。`(cl-cc:static-assert-type x integer)` で変数の型推論結果の確認。`(cl-cc:static-assert-pure fn)` で関数が副作用なしを確認。C11 `_Static_assert` / C++17 `static_assert` / Rust `const { assert!(...) }` と同等
- **根拠**: 構造体レイアウト・整列・定数値の仮定をコンパイル時に文書化・検証。ポーティング時の前提条件違反を即座に検出
- **難易度**: Easy

---

### Phase 143 — フロントエンド拡張

#### FR-800: Staged Metaprogramming / MetaML Style (段階的メタプログラミング)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: マクロは展開時(stage-0)のみ。コード生成の段階（ステージ）を型システムで追跡できない
- **内容**: `(cl-cc:bracket expr)` で次ステージのコードを引用、`(cl-cc:splice expr)` で現ステージに埋め込み。`(cl-cc:run expr)` で次ステージコードを現ステージで実行。型システムがステージ番号を追跡: `(code 0 integer)` = stage-0の整数コード片。BER MetaML（Taha & Sheard 1997）/ MetaOCaml / Scala LMS の設計。コンパイル時に任意のコード生成プログラムを安全に実行。`(declare (cl-cc:stage 1))` で2ステージコンパイルを宣言
- **根拠**: 行列演算・フォーマットパーサ・暗号ラウンド関数のコンパイル時特化生成。実行時オーバーヘッドゼロの高度な部分評価（FR-607 Futamura 投影の型安全版）
- **難易度**: Very Hard

#### FR-801: First-Class Modules / Module Functors (一級モジュール・ファンクタ)

- **対象**: `src/package.lisp`, `packages/frontend/expand/src/expander.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: パッケージは静的な名前空間。関数で受け渡せるモジュールなし
- **内容**: `(defmodule Ordered (type t) (val compare (-> t t ordering)))` でモジュール型（signature）定義。`(defstruct-module IntOrdered (type t integer) (val compare (lambda (a b) (cmp a b))))` でモジュール実装。`(defmodule-functor (Map (Ord : Ordered)) ...)` でパラメータ化モジュール（functor）定義。`(instantiate Map IntOrdered)` でインスタンス化。SML / OCaml module system の設計を踏襲。コンパイラが functor instantiation を特化（monomorphization FR-712と協調）
- **根拠**: 型クラス（FR-711）の代替として、より表現力豊かなモジュール抽象。コレクション実装（Map/Set）の型安全な交換可能性
- **難易度**: Very Hard

#### FR-802: String Interpolation (文字列補間)

- **対象**: `packages/frontend/parse/src/lexer.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: 文字列フォーマットは `(format nil "~A ~A" x y)` のみ。コンパイル時構文検査なし
- **内容**: `#"Hello, #{name}! You have #{count} messages."` リーダーマクロで文字列補間。`#{}` 内は任意のCL式（コンパイル時型チェック）。`(cl-cc:format-string "~A #{x}")` で format 指令と補間の混在。コンパイル時に `format` 指令の引数数・型整合を検査（FR-085 compile-time format 強化）。マルチライン文字列: `#"""..."""` 構文。Kotlin `"$name"` / Scala `s"..."` / Haskell QuasiQuotes と同等
- **根拠**: `format nil` の書き間違い（引数不足・型不一致）がコンパイル時に検出可能。コード可読性の大幅改善
- **難易度**: Medium

#### FR-803: Source Location Propagation (ソース位置情報伝播)

- **対象**: `packages/frontend/parse/src/cl/parser.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: AST ノードにソース位置（ファイル・行・列）の格納あり。マクロ展開・CPS変換後に位置情報が失われるケースあり
- **内容**: 全 AST ノードに `source-span` (file, start-line, start-col, end-line, end-col) を付与し、全変換パスで保持。マクロ展開後のノードは展開元フォームの位置を継承（`macro-expansion-of` 逆引き）。CPS変換・クロージャ変換後も継続して位置情報を保持し DWARF（FR-070系）へ出力。`(with-source-location loc body)` でランタイムエラーに位置情報を付加。LSP（FR-070）の `goto-definition` / `find-references` の精度向上に直結
- **根拠**: マクロ由来のエラーが「マクロ展開後のコード」ではなく「元のソース位置」を指すようになる。デバッグ体験の根本改善
- **難易度**: Medium

---

### Phase 144 — デバッグ・プロファイリング拡張

#### FR-806: Statistical CPU Profiler (統計的CPUプロファイラ)

- **対象**: `cli/src/main.lisp`, 新規 `src/profiling/cpu-profiler.lisp`
- **現状**: FR-693（Heap Profiler）はメモリ割り当て計測のみ。CPU時間の消費箇所不明
- **内容**: `./cl-cc run --cpu-profile foo.lisp` でサンプリングベースCPUプロファイリング。シグナル `SIGPROF` / `SIGALRM`（100Hz）でスタックウォーク（`backtrace(3)` / libunwind）。関数ごとの自己時間（self time）と総時間（total time）集計。`./cl-cc profview foo.clcc-prof` でフレームグラフ（Brendan Gregg SVG形式）出力。`--callgrind` フラグでValgrind Callgrind互換フォーマット出力。`--perf` フラグでLinux `perf script` 互換出力（Firefox Profiler UIで閲覧可）
- **根拠**: selfhostingコンパイルのCPUホットスポット特定。コード生成・最適化パスのどの関数が最も時間を消費するかを実測
- **難易度**: Medium

#### FR-807: PMU / Performance Counter Access (PMUパフォーマンスカウンタアクセス)

- **対象**: `cli/src/main.lisp`, 新規 `src/profiling/pmu.lisp`
- **現状**: プロファイリングは時間計測のみ。キャッシュミス・分岐予測失敗・TLBミスなどのハードウェアイベントカウンタ非対応
- **内容**: `./cl-cc run --perf-events L1-dcache-miss,branch-misses,tlb-misses foo.lisp` でハードウェアカウンタを計測。Linux: `perf_event_open(2)` syscallで`perf_event_attr`を設定。macOS: `kpc_*` framework（`ktrace` / Instruments Instruments API）。カウンタ: サイクル数・命令数・CPI（Cycles Per Instruction）・L1/L2/L3キャッシュミス・分岐予測失敗率・TLBミス・メモリ帯域幅。`(cl-cc:with-perf-counters (cycles l1-miss) body)` でコード区間を計測
- **根拠**: 「遅い」の原因がキャッシュミスなのか分岐予測失敗なのかを特定するための唯一の手段。最適化効果の定量的検証
- **難易度**: Hard

#### FR-808: Binary Analysis Tools (バイナリ解析ツール)

- **対象**: `packages/backend/binary/src/macho.lisp`, `cli/src/main.lisp`
- **現状**: 生成バイナリの内部構造を検査するツールなし。`objdump`/`readelf`/`nm` 相当が外部依存
- **内容**: `./cl-cc objdump foo` でMach-O/ELFセクション・シンボル・リロケーションを表示（`objdump -d` 相当）。`./cl-cc nm foo` でシンボルテーブル表示（型・サイズ・アドレス）。`./cl-cc size foo` でセクション別サイズ表示。`./cl-cc disasm --fn bar foo` で特定関数のx86-64逆アセンブル（Zydis/XED相当を純CLで実装）。`./cl-cc strings foo` で埋め込み文字列抽出。`./cl-cc headers foo` でロードコマンド・ELFヘッダ詳細表示
- **根拠**: 生成コードの検証・最適化効果の確認・セキュリティ審査に外部ツール依存を排除。cl-cc自己完結型toolchainの完成
- **難易度**: Medium

#### FR-809: Compiler Regression Bisection (コンパイラ回帰二分探索)

- **対象**: `cli/src/main.lisp`, 新規 `src/testing/bisect.lisp`
- **現状**: パフォーマンス回帰・バグ導入のコミットを特定するには手動の`git bisect`が必要
- **内容**: `./cl-cc bisect --metric "compile-time" --threshold 20% HEAD~100 HEAD` で回帰を導入したコミットを自動二分探索。各コミットでcl-ccをビルドし計測を実行。`--metric "test-pass-rate"` で正当性回帰も検出。`git bisect run` スクリプトとして動作。回帰ポイント特定後に `--analyze` フラグでdiff・コミットログを表示。FR-695（Benchmarking Framework）と統合しベンチマーク結果を自動記録・比較
- **根拠**: コンパイラ開発での性能回帰は発見が困難（数コミット前から蓄積）。自動二分探索で問題箇所を数分で特定
- **難易度**: Medium

---

### Phase 145 — ビルドシステム高度化 II

#### FR-812: Conditional Compilation / Feature Flags (条件付きコンパイル・機能フラグ)

- **対象**: `cli/src/main.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: CL標準の `#+`/`#-` リーダーマクロは基本的な機能フラグのみ。ビルド設定のスコープ・継承・検証なし
- **内容**: `(cl-cc:feature-flag :jit-enabled :default t :type boolean :description "Enable JIT compilation")` で型付き機能フラグを宣言。`./cl-cc build --feature jit-enabled=false` でビルド時設定。`(cl-cc:when-feature :jit-enabled ...)` / `(cl-cc:unless-feature ...)` でコンパイル時分岐。フラグ依存グラフ（`(cl-cc:requires-feature :jit-enabled :x86-64)`）。`./cl-cc features list` で全フラグ・デフォルト値・説明を表示。Rust cargo features / C++ CMake options / Nix `enableJIT` と同等
- **根拠**: JIT・GC種別・ターゲットアーキテクチャ・セキュリティ機能を同一ソースから条件ビルドで制御
- **難易度**: Medium

#### FR-813: Package Lockfiles (パッケージロックファイル)

- **対象**: `cli/src/main.lisp`, 新規 `src/build/lock.lisp`
- **現状**: 依存関係の解決はビルド時に毎回実行。チーム内・CI環境で異なるバージョンが使われる可能性
- **内容**: `./cl-cc build` 実行時に `cl-cc.lock` ファイルを生成。全直接・間接依存のバージョン・SHA256ハッシュ・ダウンロードURL を記録。次回ビルドはlockファイルを優先し完全再現性保証（FR-662 Reproducible Builds と統合）。`./cl-cc update [package]` で選択的更新とlockファイル更新。lockファイルはgit管理対象（変更がPRで可視化）。Cargo.lock / package-lock.json / poetry.lock / Nix flake.lock と同等設計
- **根拠**: 「自分の環境では動く」問題の根絶。CIとローカルで完全同一の依存バージョンを保証
- **難易度**: Medium

#### FR-814: Dependency Vulnerability Scanning (依存関係脆弱性スキャン)

- **対ότ**: `cli/src/main.lisp`, 新規 `src/build/security-scan.lisp`
- **現状**: 依存ライブラリにCVEが存在しても検出手段なし
- **内容**: `./cl-cc audit` でFR-813 lockファイルの依存関係をOSV（Open Source Vulnerabilities）データベース / GitHub Advisory Database に照会。既知CVEが存在する依存を警告（CVSS スコア・影響範囲・修正バージョン情報付き）。`--deny high` で高リスクCVEをビルドエラーに。SBOM（FR-455 Software Bill of Materials）と統合しSPDX/CycloneDX形式でCVEマッピングを出力。Cargo audit / npm audit / pip-audit / OWASP Dependency-Check と同等
- **根拠**: ソフトウェアサプライチェーン攻撃への対策。2026年以降の企業セキュリティ要件（SLSA / EO 14028）で義務化傾向
- **難易度**: Medium

#### FR-815: Build-Time Source Code Generation (ビルド時ソースコード生成)

- **対象**: `cli/src/main.lisp`, 新規 `src/build/codegen-step.lisp`
- **現状**: ソースコードは全て手書き。ビルド時に外部スキーマ・プロトコル定義から自動生成するパイプラインなし
- **内容**: `cl-cc.build.lisp` ビルドスクリプトに `(generate-from-schema "schema.proto" :target "src/generated/proto.lisp")` を記述。生成ステップはFR-698（並列コンパイル）の依存グラフに統合。生成ファイルは再生成可能とマーク（gitignore推奨・手動編集禁止）。サポートソース: Protocol Buffers `.proto` / JSON Schema / OpenAPI YAML / DWARF type tables / SQL DDL。`./cl-cc generate` で明示的な生成実行。生成コードのFR-797（static_assert）での検証
- **根拠**: プロトコル定義の変更時にFFIバインディング・シリアライザを手動更新する作業を自動化
- **難易度**: Medium

---

### Phase 146 — I/O・OS統合

#### FR-818: Zero-Copy I/O / sendfile (ゼロコピーI/O)

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: ファイルI/OはCLの `read-sequence`/`write-sequence` 経由（ユーザー空間バッファ経由のコピーあり）
- **内容**: `(cl-cc:sendfile dst-fd src-fd count)` → Linux `sendfile(2)` / macOS `sendfile(2)` syscallを直接呼び出し。カーネル空間でのファイル→ソケット転送（ユーザー空間コピーゼロ）。`(cl-cc:splice src-pipe dst-pipe count)` → Linux `splice(2)` でパイプ間ゼロコピー。`(cl-cc:tee src dst count)` → `tee(2)` でパイプ分岐。`(cl-cc:vmsplice buf-list pipe)` → ユーザー空間バッファをパイプにゼロコピー。io_uring（FR-600）との統合でゼロコピー非同期I/O
- **根拠**: Webサーバーでの静的ファイル配信でsendfileが通常転送比2〜10倍高速（カーネル空間でのDMA転送）
- **難易度**: Medium

#### FR-819: Memory-Mapped Files / mmap (メモリマップドファイル)

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: ファイルI/OはFOPEN/READ/WRITE syscall経由。大規模ファイルのランダムアクセスが非効率
- **内容**: `(cl-cc:mmap file :read :size n)` → `mmap(2)` でファイルをアドレス空間にマッピング。返値は`cl-cc:mapped-region`オブジェクト（`(aref region i)` でバイトアクセス）。`(cl-cc:mmap-sync region)` → `msync(MS_SYNC)` で変更をディスクに同期。`(cl-cc:munmap region)` でアンマップ。`MAP_SHARED`（プロセス間共有）/ `MAP_PRIVATE`（COW）/ `MAP_ANONYMOUS`（無名マッピング）オプション。`MAP_POPULATE` で先読み。`madvise(MADV_SEQUENTIAL/RANDOM/WILLNEED)` ヒント
- **根拠**: コンパイラのソースファイル読み込み・FASLキャッシュ・大規模データ処理でmmap経由が read()より大幅に高速
- **難易度**: Medium

#### FR-820: UNIX Signal Handling (UNIXシグナルハンドリング)

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/engine/vm/src/vm-execute.lisp`, `cli/src/main.lisp`
- **現状**: `SIGINT`（Ctrl-C）はSBCLのデフォルト処理。他のUNIXシグナルをLispのconditionとして扱う手段なし
- **内容**: `(cl-cc:handle-signal SIGTERM (lambda (sig) (cleanup-and-exit)))` でシグナルハンドラを登録。`SIGINT` → `(cl-cc:keyboard-interrupt condition)`、`SIGHUP` → `(cl-cc:hangup condition)`、`SIGCHLD` → `(cl-cc:child-status-change condition)` として CL condition systemに統合。`SIGUSR1`/`SIGUSR2` でユーザー定義シグナルを使った動的ロギングレベル変更。`SA_RESTART` フラグ制御。`sigprocmask` / `sigwaitinfo` / `signalfd`（Linux）の高水準ラッパー
- **根拠**: デーモンプロセス・長時間実行サーバー（LSP server / REPL server）でのグレースフルシャットダウン・設定リロードに必須
- **難易度**: Medium

#### FR-821: Shared Memory IPC (共有メモリIPC)

- **対象**: `packages/engine/vm/src/io.lisp`, `cli/src/main.lisp`
- **現状**: プロセス間通信はソケット/パイプのみ。同一マシン上の高速データ共有手段なし
- **内容**: `(cl-cc:shm-open "/my-seg" :create :size (* 4 1024 1024))` → POSIX `shm_open(3)` + `ftruncate` + `mmap`。`(cl-cc:shm-open "/my-seg" :attach)` で既存セグメントに接続。`(cl-cc:with-spinlock (shm :offset 0) body)` で共有メモリ上のスピンロック（atomic CAS）。`(cl-cc:semaphore-post sem)` / `(cl-cc:semaphore-wait sem)` → POSIX `sem_post`/`sem_wait`。Sysv IPC 代替として POSIX IPC のみ実装。ユースケース: 並列コンパイル（FR-698）プロセス間の型情報共有・LSPクライアント/サーバー間高速通信
- **根拠**: パイプ比100〜1000倍のIPC帯域幅（カーネル経由なし）。分散コンパイラキャッシュ（FR-699）のノード間通信
- **難易度**: Medium

---

### Phase 147 — 命令レベル最適化 II

#### FR-824: Macro-op Fusion Detection (マクロ命令融合検出)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 命令スケジューリング（FR-033）・スループット最適化（FR-773）はポートモデルを使用。CPUの自動macro-op fusionの利用最大化なし
- **内容**: Intel/AMD CPUが自動融合するペア（`cmp + jcc`、`test + jcc`、`add + jcc`、`lea + cmp`）を意図的に生成。融合条件: 隣接命令・同一デコードグループ・フラグ依存。コンパイラが`CMP + JE`の代わりに`TEST + JNE`を選択する場合も融合性を考慮。AMD Zen4 / Intel Raptor Lake の融合テーブルを参照。融合成功でfront-end bandwidthが実質2倍
- **根拠**: ループ制御（compare + branch）がほぼ全ループに存在。フュージョン1回でデコード帯域を1命令分節約。高頻度ループでの効果が大きい
- **難易度**: Medium

#### FR-825: SWAR / SIMD Within A Register (レジスタ内SIMD)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: バイト・ハーフワード単位の並列処理はSIMD命令依存。ベクトル命令が使えない文脈でのスカラー並列化なし
- **内容**: 64ビット整数レジスタを複数の小型整数（8×byte / 4×16bit / 2×32bit）として扱う技法を自動適用。`(dotimes (i 8) (aref chars i))` の文字処理 → 8バイトを一括pack → `AND 0x0101010101010101` 等のbitwise演算で並列処理。`(cl-cc:swar8 ...)` / `(cl-cc:swar16 ...)` で明示的SWAR演算を提供。`popcount` / `parity` / `byte-reverse` の効率的SWAR実装を標準ライブラリに内蔵
- **根拠**: SSE/AVXが使えない環境（スカラーループ・WASM）での文字列処理・バイトマップ操作の高速化。SWAR（Hacker's Delight）技法は無料の2〜8倍高速化
- **難易度**: Medium

#### FR-826: Branchless Code Generation (分岐レスコード生成)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: `if`式は条件分岐命令（JE/JNE）に変換。分岐予測失敗で5〜20サイクルのペナルティ
- **内容**: `(if (> a b) a b)` → `CMOVG rax, rbx`（Conditional MOV、分岐なし）に変換。変換条件: thenとelseが副作用なし・同型・短い演算。`(cl-cc:clamp x lo hi)` → `MAX(MIN(x,hi),lo)` をCMOV2命令で実装。絶対値: `(abs x)` → `SAR + XOR + SUB`（branchless abs）。選択関数: `(cl-cc:select cond a b)` でCMOV強制。コンパイラが分岐予測困難（乱数・外部データ依存）なifを自動検出してCMOV変換
- **根拠**: ソートアルゴリズムの比較関数でbranchlessが2〜5倍高速。コンパイラ内部の型タグチェックに多数適用可能
- **難易度**: Medium

#### FR-827: Instruction Count Minimization (命令数最小化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: コード生成は命令の意味的正しさを優先。命令数最小化を目標とした最適化モードなし
- **内容**: `--optimize-for instruction-count` モード。LEA命令による乗算代替: `3*x` → `LEA rax, [rax + 2*rax]`（MUL命令不要）。XOR-zeroing: `(setq r 0)` → `XOR rax, rax`（MOV命令より1バイト短い）。`INC`/`DEC` vs `ADD 1`/`SUB 1`の選択。Load-with-operation: `(incf (aref arr i))` → `ADD [arr+i*8], 1`（load+add+store→1命令）。FR-671（Superoptimizer）のルールをオンラインで適用
- **根拠**: I-cache制約が厳しいホットループで命令数削減がキャッシュヒット率向上に直結。コードサイズ最適化（FR-615）の補完
- **難易度**: Medium

---

### Phase 148 — 型システム VI

#### FR-830: Session Types (セッション型)

- **対象**: `packages/type/type/src/`, `packages/engine/vm/src/conditions.lisp`
- **現状**: チャネル（FR-599）は型なし。通信プロトコルの順序・双方向性の型安全保証なし
- **内容**: `(defsession ClientProtocol (! (request string)) (? (response integer)) end)` でセッション型を定義。`!`=送信、`?`=受信、`&`=外部選択、`⊕`=内部選択、`end`=終了、`μ`=再帰。チャネル（FR-599）にセッション型を付与: `(cl-cc:open-channel ClientProtocol)` で型付きチャネルを取得。型検査がプロトコル違反（順序・方向・型不一致）をコンパイル時に検出。線形型（FR-linear）との統合でチャネルの使い捨て保証。Honda et al. / Vasconcelos / Frank Pfenning のセッション型理論の実装
- **根拠**: 通信プロトコルのバグ（順序違反・デッドロック）をコンパイル時に検出。cl-ccのLSPサーバー（FR-070）のJSON-RPC通信をセッション型で安全化
- **難易度**: Very Hard

#### FR-831: Singleton Types (シングルトン型)

- **対象**: `packages/type/type/src/`, `packages/frontend/expand/src/expander.lisp`
- **現状**: 型は値の集合を記述。特定の値1つだけを持つ型（singleton）なし
- **内容**: `(cl-cc:the-value 42)` で値42だけを持つ型`(singleton 42)`を生成。`(defun f (x) (declare (type (singleton :ok) x)) ...)` でxが`:ok`であることを型レベルで保証。定数畳み込み（FR-002）との統合: `(singleton 42)`型の変数は定数として伝播。`eql-specializer` との統合: `(eql :ok)` CLOSスペシャライザと同型。TypeScript `"literal"` 型 / Haskell `Proxy n` / C++ `std::integral_constant` と同等
- **根拠**: 状態機械の状態を値ではなく型で表現。関数の戻り値が特定定数のみであることを型で文書化・検証
- **難易度**: Medium

#### FR-832: Liquid Type Inference (液体型推論)

- **対象**: `packages/type/type/src/`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: VRP（FR-245）は整数範囲を解析するがそれを型システムに反映しない。精緻化型（FR-225）は手動アノテーション必須
- **内容**: **Liquid Haskell スタイルの自動精緻化型推論**: プログラムの制御フローから自動的に精緻化型を導出。`(if (> n 0) (sqrt n) ...)` → then分岐でnの型を`{n : integer | n > 0}`に自動精緻化。SMTソルバー（FR-246）に送出して検証。`(declare (cl-cc:liquid-types))` で関数単位有効化。境界検査除去（FR-037 BCE）の精度を大幅向上: 液体型推論で境界内と証明された配列アクセスのチェックを削除
- **根拠**: Liquid Haskell実測: 配列境界検査の70〜90%を静的除去。実行時の安全性チェックを大幅削減
- **難易度**: Very Hard

#### FR-833: Existential Types (存在型)

- **対象**: `packages/type/type/src/`, `packages/frontend/expand/src/expander.lisp`
- **現状**: 全称型（`forall a. ...`、FR-682 Rank-N）はあるが、存在型（`exists a. ...`）なし
- **内容**: `(cl-cc:pack val (type a integer) (val show (lambda (x) (format t "~A" x))))` で存在型のパッキング。`(cl-cc:unpack (a val show) packed-val body)` でアンパッキング（`a`は本体内でのみ参照可能）。**実装**: 辞書渡し（dictionary passing）でモジュール/型クラスインスタンスとして表現。`(forall a. (Eq a) => ...)` の `a` をその場で存在型として局所化。OCaml `module type` の first-class 版 / Haskell `ExistentialQuantification` と同等
- **根拠**: 動的ディスパッチ（CLOS）の型安全なエンコード。プラグイン（FR-700）のインターフェース型を存在型で表現し型安全なプラグインシステムを実現
- **難易度**: Hard

---

### Phase 149 — マクロ・メタプログラミング高度化

#### FR-836: Macro Debugger / Expansion Stepper (マクロデバッガ・展開ステッパー)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `cli/src/main.lisp`
- **現状**: マクロ展開エラーのデバッグは`macroexpand-1`の手動呼び出しのみ。展開過程の可視化なし
- **内容**: `./cl-cc macrostep foo.lisp` でマクロ展開を1ステップずつ可視化。各ステップで「適用されたマクロ名・入力フォーム・展開結果」を表示。`--max-depth 5` で展開深さを制限。DrRacket Macro Stepper / SLIME macroexpand-all との比較設計。LSP統合（FR-070）: `textDocument/macroExpand` カスタムリクエストでIDEからマクロ展開を段階表示。差分ハイライト（展開前後のdiff）でどの部分が変化したかを明示
- **根拠**: cl-ccのマクロ（loop・dolist・define-vm-instruction等）のデバッグに直接効果。マクロ展開過程が不透明なのは開発者の最大の苦痛源
- **難易度**: Medium

#### FR-837: Hygienic Macros / Syntax-Rules (衛生的マクロ)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: CLの`defmacro`は非衛生的（変数キャプチャの問題）。`gensym`による手動衛生性確保が必要
- **内容**: `(define-syntax my-swap (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))` でR7RS/R6RS`syntax-rules`スタイルの衛生的マクロを提供。`tmp`が呼び出し元のシンボルを捕捉しない（自動gensym化）。`syntax-case`（R6RS）スタイルのパターンマッチ付き衛生的マクロも実装。`(cl-cc:define-syntax ...)` で従来の`defmacro`と共存。Scheme SRFI-72 / Racket `define-syntax` との互換性
- **根拠**: 複雑なマクロでの変数キャプチャバグを根本排除。初心者がgensymを意識せずにマクロを書ける
- **難易度**: Hard

#### FR-838: Compile-Time Unit Tests (コンパイル時ユニットテスト)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `cli/src/main.lisp`
- **現状**: テストはビルド後に別途`nix run .#test`で実行。型チェック・定数評価結果のコンパイル時検証なし
- **内容**: `(cl-cc:compile-time-test "fib-10 = 55" (= (cl-cc:eval-at-compile-time (fib 10)) 55))` でコンパイル中に評価・検証。失敗時はコンパイルエラー（テスト名・期待値・実際値付き）。型に関するcompile-time test: `(cl-cc:compile-time-test "add is pure" (cl-cc:static-assert-pure #'add))`。`--compile-tests` フラグで全compile-time testを実行（CI用）。D言語の `unittest` / Rust の `#[test]` (doctest) / Zig の `comptime testing` と同等
- **根拠**: 定数テーブル・型推論結果・マクロ展開の正しさをビルド中に保証。「コンパイルが通れば一定の正しさが保証される」水準の向上
- **難易度**: Medium

#### FR-839: Macro Expansion Memoization (マクロ展開メモ化)

- **対象**: `packages/frontend/expand/src/expander.lisp`
- **現状**: 同一フォームのマクロ展開が複数箇所で発生する場合（LTOやインライン化後）、毎回再展開
- **内容**: マクロ展開結果を入力フォームのハッシュ（内容ハッシュ）をキーとしてキャッシュ。純粋なマクロ（副作用なし・環境非依存）のみキャッシュ対象。`(declare (cl-cc:pure-macro))` で明示指定。`*macro-expansion-cache*` ハッシュテーブル（弱値参照でGC可能）。展開結果をFASLにシリアライズして次回ビルドでも再利用（FR-452 コンパイルキャッシュと統合）。キャッシュヒット率をデバッグ出力（`--verbose-macros`）
- **根拠**: `define-vm-instruction`・`deftest`・`(loop ...)`などの繰り返しマクロ展開がselfhostingコンパイル時間の大きな割合を占める
- **難易度**: Medium

---

### Phase 150 — 関数特化・クローニング

#### FR-842: Function Versioning / Argument Specialization (関数バージョニング・引数特化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: IPCPとLTO（FR-040）で定数引数を伝播。関数クローン生成による特化版の自動生成なし
- **内容**: 全呼び出しサイトで引数 `x` が特定の定数値のサブセット（例: 常にtまたはnilの2値）であると分かった場合、クローン関数 `foo/x=t` と `foo/x=nil` を生成。各呼び出しサイトは適切なクローンにリダイレクト。特化後に定数畳み込み（FR-002）・デッドコード除去（FR-003）が大幅に適用可能。`--clone-threshold 3` でクローン生成の最大コピー数制限。LLVM `ArgumentPromotion` + cloning / GCC IPCP cloning と同等
- **根拠**: コンパイラ内部のフラグ引数（`(compile-ast ast env debug?)` の `debug?`）が常に`nil`なら特化版でデバッグパスが完全消去される
- **難易度**: Hard

#### FR-843: SIMD Auto-Specialization (SIMD自動特化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: SIMD化（FR-035）は配列ループのみ。スカラー関数の引数型・サイズが実行時に確定した後でのSIMD特化なし
- **内容**: 配列処理関数 `(defun sum-array (arr n) ...)` を呼び出す際、`n`が16の倍数と分かった場合にSIMD特化版を自動選択。FR-842（Function Versioning）の拡張: サイズ・アライメント・型のサブセット特化。`(cl-cc:specialize sum-array :simd :when (multiple-of n 16))` で手動特化ヒント。特化版と汎用版を両方生成しディスパッチコードで選択。FR-616（Multi-Versioning）のSIMD特化版
- **根拠**: コンパイル時にSIMD条件を確認できない関数でも、呼び出し時の引数情報でSIMD最適化を適用可能
- **難易度**: Hard

#### FR-844: Loop Count Specialization (ループカウント特化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: ループは一般的な反復数を仮定。小さな固定回数（1・2・4・8回）の特化版なし
- **内容**: ループカウントが小さな定数になる呼び出しサイト（`(dotimes (i 4) ...)`）を検出し、完全展開版（FR-034 アンローリング）を自動生成。ランタイム特化: カウントが小さい場合に特化版、大きい場合に汎用版をディスパッチ。`(cl-cc:specialize-loop :counts (1 2 4 8 16))` で特化カウントを明示。3×3行列 / 4×4行列 / RGBA4チャンネル処理などのDSP定型処理に効果的
- **根拠**: 小固定カウントループの完全展開でブランチとカウンタ更新をゼロに。グラフィクス・信号処理コードで3〜5倍高速化
- **難易度**: Medium

#### FR-845: Hot/Cold Function Splitting (ホット/コールド関数分割)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: FR-036（Hot/Cold レイアウト）は関数全体をhot/coldに分類。関数内のhot/coldパスの分離なし
- **内容**: プロファイルデータ（FR-508 AutoFDO）またはJITフィードバック（FR-559 Type Feedback）でhot basicブロックとcoldブロック（エラー処理・稀な分岐）を特定。coldブロックを関数外に抽出し`__cold`セクションに配置（`__TEXT.__text.cold`）。hot部分が連続したI-cache lineに収まりTLBエントリを節約。GCCの`__attribute__((cold))`ブロック分離 / LLVM `llvm.cold_call` / V8 cold code separation と同等。`(declare (cl-cc:cold))` で手動coldマーク
- **根拠**: エラー処理コード（assert・type-error・bounds-check失敗）がhotパスから除外されI-cache効率が大幅向上。selfhostingコンパイルのhotパス特定でコンパイル速度向上
- **難易度**: Hard

---

### Phase 151 — TLS・特殊命令

#### FR-848: Thread-Local Storage / TLS (スレッドローカルストレージ)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **現状**: グローバル変数はプロセス共有。スレッドごとに独立した値を持つ変数の宣言・アクセスなし
- **内容**: `(defvar-tls *current-thread-state*)` でスレッドローカル変数を宣言。x86-64: ELF TLS (`%fs:` セグメント相対アドレッシング) / Mach-O TLS (`__DATA.__thread_vars` セクション + `tlv_get_addr` stub)。`MOVQ %fs:0, %rax` + オフセット方式。AArch64: `MRS x0, TPIDR_EL0` + オフセット。FR-576（Work-Stealing）ワーカーごとの`*current-worker*` / FR-676（TLAB）per-thread バッファ pointer の実装基盤。`(declare (cl-cc:thread-local))` で宣言
- **根拠**: TLABポインタ・JITコード生成バッファ・エラーハンドラスタックなど、スレッドローカルに保持すべき状態が多数。`pthread_getspecific` より1〜2桁高速
- **難易度**: Hard

#### FR-849: AES-NI / SHA-NI Hardware Cryptography (AES-NI・SHA-NI ハードウェア暗号命令)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: 暗号処理はソフトウェア実装のみ。AES/SHA の CPU 組み込み命令未活用
- **内容**: `(cl-cc:aes-enc block key)` → `AESENC xmm0, xmm1`（1サイクル / Intel Westmere以降）。`(cl-cc:aes-keygen key rcon)` → `AESKEYGENASSIST`。SHA-256: `(cl-cc:sha256-rounds state msg)` → `SHA256RNDS2` / `SHA256MSG1` / `SHA256MSG2`（AMD Zen / Intel Ice Lake 以降）。CLMUL: `(cl-cc:clmul a b)` → `VPCLMULQDQ` でガロア体GF(2^128)乗算（AES-GCM認証タグ計算）。`cpuid` で命令サポートを確認し、非サポート CPU はソフトウェアフォールバック
- **根拠**: AES-CTR / AES-GCM でAES-NI使用時はソフト実装比8〜30倍高速。FR-453（Constant-Time Security）との統合でタイミング攻撃耐性も保証
- **難易度**: Medium

#### FR-850: Atomic 128-bit Operations / CMPXCHG16B (128ビット不可分操作)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 原子操作（FR-160）は64ビット幅まで。タグ付きポインタ（値+バージョンカウンタ）のABA問題を解決する128ビット CAS なし
- **内容**: `(cl-cc:cmpxchg128 addr expected-lo expected-hi new-lo new-hi)` → `CMPXCHG16B [addr]`（RCX:RBX / RDX:RAX ペア操作）。ABA問題対策: ポインタ64bit + バージョンカウンタ64bit を1原子操作で更新。ロックフリースタック（FR-163）のABAバグを根本解消。AArch64: `CASP x0, x1, x2, x3, [addr]`（Load-Pair atomic）。`LOCK` prefix が必要（`LOCK CMPXCHG16B`）
- **根拠**: `(cl-cc:atomic-cons head new-element)` でABAフリーのロックフリーリスト先頭更新が可能。マルチコアGCのロックフリーアロケータでの競合状態を根本解消
- **難易度**: Medium

#### FR-851: Vector Permutation Instructions (ベクトル置換命令)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: SIMDコード生成（FR-035）は算術・比較命令中心。要素の並べ替え（shuffle/gather）命令の活用なし
- **内容**: `(cl-cc:vpshufb dst ctrl src)` → `VPSHUFB ymm0, ymm1, ymm2`（バイト単位シャッフル、AES MixColumns・文字変換テーブルに使用）。`(cl-cc:vpermd idx src)` → `VPERMD`（32bit要素を任意順に並べ替え）。`(cl-cc:vperm2i128 src1 src2 imm)` → 128bitレーン間シャッフル。自動適用: AoS→SoA変換（FR-038）のシャッフルコストモデルにpermutation命令を使用。AArch64: `TBL v0.16b, {v1.16b}, v2.16b`
- **根拠**: ハッシュ関数・暗号・行列転置でpermutation命令がループ比10倍以上高速。SIMD自動ベクトル化の表現力を大幅拡張
- **難易度**: Medium

---

### Phase 152 — GC高度化 III

#### FR-854: Cycle Detection for Reference Counting (参照カウントの循環検出)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: FR-229（Reference Counting）は基本的なRC実装。循環参照（`(let ((a (cons nil nil))) (setf (car a) a))`）がメモリリーク
- **内容**: Bacon & Rajan（OOPSLA 2001）の **同期的循環GC**: RC=0でないが unreachable なオブジェクトをサイクル候補バッファに記録。定期的に候補から深さ優先探索で孤立サイクルを検出・回収。`mark-gray`→`scan`→`collect-white` の3フェーズ。CPython の Cyclic GC / Ruby の Mark-and-Sweep 補完と同等の設計。`--cycle-gc-threshold 1000` でサイクル検出頻度を調整
- **根拠**: CLOSインスタンスが互いに参照し合うケース（双方向リンクリスト・Observer パターン）でメモリリーク防止。RCとトレーシングGCのハイブリッドで最高のスループットを実現
- **難易度**: Hard

#### FR-855: Deferred Reference Counting (遅延参照カウント)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: RC（FR-229）は全ての参照変化で即座にカウンタ更新。スタック上の一時参照でも毎回インクリメント/デクリメントが発生
- **内容**: **Deutsch-Bobrow 方式**: スタックフレーム（ローカル変数）からの参照変化を **遅延**し、ヒープからヒープへの参照変化のみ即座に更新。スタック参照のRCはGCサイクル時に一括処理（安全点でスタックスキャン）。RC更新オーバーヘッドを70〜90%削減。「Zero Count Table」（ZCT）でRC=0になったオブジェクトを遅延回収。Levanoni-Petrank（PLDI 2001）のRC最適化と組み合わせ
- **根拠**: Pythonの主要GCボトルネックはRC更新（全代入で2操作）。遅延RC でインタプリタ比30%高速化の実測値あり
- **難易度**: Hard

#### FR-856: Pinned Objects for FFI (FFI用ピン留めオブジェクト)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm-execute.lisp`, `src/ffi/`
- **現状**: GC（コンパクティング・FR-226）がオブジェクトを移動可能。C関数に渡したLispオブジェクトのアドレスが無効化されるリスク
- **内容**: `(cl-cc:with-pinned-object (obj) (foreign-call "cfn" (object-address obj)))` でC関数呼び出し中のGC移動を禁止。pinされたオブジェクトは `pinned-objects` セットに登録しGCのcompaction対象から除外。C関数からコールバック（FR-884）で再入可能: pin は呼び出しスタック全体に継承。`(cl-cc:pin obj)` / `(cl-cc:unpin obj)` で手動制御。FR-550（Stack Maps）と統合しGCがpinnedを把握。Java JNI `GetPrimitiveArrayCritical` / .NET `fixed` 相当
- **根拠**: GCコンパクション（FR-226）とFFI（FR-632 Bindgen）の共存に不可欠。ピン忘れはサイレントなメモリ破壊を起こす最悪のバグ
- **難易度**: Medium

#### FR-857: GC External Roots / Foreign GC Roots (GC外部ルート)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: GCルートはVMレジスタとスタックのみ。C側に保持されたLispオブジェクト参照がGCに見えない
- **内容**: `(cl-cc:register-gc-root ptr-to-lisp-obj)` でCデータ構造内のLispオブジェクト参照をGCルートとして登録。GCマーク相（FR-552 Concurrent GC）でexternal rootsを走査対象に追加。`(cl-cc:unregister-gc-root ptr)` で登録解除。C++コンテナ内のLispオブジェクト群を一括登録: `(cl-cc:register-gc-root-range start count)`。スレッドセーフ: `roots-lock` で保護。JVM `AddVMInitArgs` roots / V8 `Persistent<T>` / Mono `mono_gchandle_new` と同等設計
- **根拠**: C++ゲームエンジン・Pythonモジュール・GTKウィジェット内にLispオブジェクトを埋め込む用途で必須。登録忘れはGCによる即死バグ
- **難易度**: Medium

---

### Phase 153 — 最適化パス管理

#### FR-860: Pass Pipeline Configuration (パスパイプライン設定)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `cli/src/main.lisp`
- **現状**: 最適化パスの実行順序はハードコード。特定パスの無効化・順序変更・パラメータ調整が困難
- **内容**: `--pass-pipeline "licm,gvn,sccp,cse,dce"` でパス実行順を文字列指定。`--disable-pass licm` で特定パスを無効化（デバッグ用）。`--pass-param inlining:threshold=50` でパスパラメータを CLI から設定。`(cl-cc:define-pass-pipeline :O2 (licm gvn sccp cse dce inlining loop-unroll))` でプリセット定義。パスの実行ログ（`--print-passes`）で各パスの入出力を表示。LLVM `opt -passes="..."` / GCC `-fdump-tree-` と同等
- **根拠**: 最適化バグの二分探索（FR-809 Bisection）でパス単位の有効/無効切り替えが必須。新パス追加時の順序依存性のデバッグに不可欠
- **難易度**: Medium

#### FR-861: Analysis Preservation Tracking (解析保存追跡)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`
- **現状**: 各最適化パスが実行されると全ての解析結果（エイリアス解析・支配木・live range）が無効化され再計算。不要な再計算が多い
- **内容**: 各パスが「保存する解析」を宣言: `(define-opt-pass licm :preserves (alias-analysis dominance-tree))`. パスマネージャが宣言を追跡し、保存されている解析を再計算なしに後続パスへ渡す。「解析の依存グラフ」で何が何を必要とするかを自動管理。`--print-preserved-analyses` でどの解析が有効かをダンプ。LLVM `PreservedAnalyses` / GCC `PROP_cfg` と同等
- **根拠**: 大規模プログラムのコンパイルでエイリアス解析・支配木の再計算コストが無視できない。保存追跡でコンパイル時間20〜40%削減の実測値あり
- **難易度**: Medium

#### FR-862: Value Profiling (値プロファイリング)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `cli/src/main.lisp`
- **現状**: PGO（FR-508 AutoFDO）は関数呼び出し頻度・分岐頻度のみ計測。実際の引数値の分布不明
- **内容**: `./cl-cc run --value-profile foo.lisp` で関数引数・分岐条件・インダイレクト呼び出し先の実際の値をサンプリング。「引数 x は90%の確率で 0」「間接呼び出しは85%が `foo`」などの値プロファイルを収集。FR-842（Function Versioning）: 高頻度値でのクローン生成の根拠として使用。FR-560（Speculative Inlining）: 高頻度呼び出し先への投機的インライン化の確率根拠。LLVM `InstrProfValueData` / JVM Value Profiling と同等
- **根拠**: 型フィードバック（FR-559）の拡張。「この関数の引数は常に小さな整数」という情報を最適化の根拠にする
- **難易度**: Medium

#### FR-863: Loop Versioning with Runtime Alias Checks (ランタイムエイリアスチェック付きループバージョニング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: SIMD自動ベクトル化（FR-035）は静的エイリアス解析（FR-234）でセーフと確認できたループのみ対象。解析が不確定なループはベクトル化不可
- **内容**: エイリアス解析が「不確定」なループで **ランタイムチェック** を挿入。`if (|src - dst| >= vector_width) { SIMD版 } else { スカラー版 }` の分岐生成。チェックコスト < ベクトル化利益 の場合にのみ適用（ループ回数推定が十分大きい場合）。複数のエイリアスペアのチェックを1条件に結合。LLVM `LoopVersioning` / GCC polyvect runtime check と同等
- **根拠**: エイリアス不確定な配列操作ループ（外部から渡された配列ペア）の70〜80%がランタイムで非エイリアスと確認されSIMD化可能
- **難易度**: Hard

---

### Phase 154 — 型システム VII

#### FR-866: Recursive Types / μ-Types (再帰型・μ型)

- **対象**: `packages/type/type/src/`, `packages/frontend/expand/src/expander.lisp`
- **現状**: 型定義に相互再帰（`list = nil | (cons value list)`）はCLOSで表現可能だが型推論システムの対象外
- **内容**: `(deftype-rec (list a) (or nil (cons a (list a))))` で再帰型を宣言。型検査はμ型の展開（unrolling）で等価性判定。`μX.F[X]` 表現で型システム内部に保持。無限型（equi-recursive）と有限展開（iso-recursive）の両モード。`(cl-cc:fold val)` / `(cl-cc:unfold val)` でiso-recursive型の明示的コンバージョン。Amadio & Cardelli（1993）の再帰型等価アルゴリズム実装
- **根拠**: 型推論が再帰型（リスト・ツリー・グラフ）に対して正確な型を導出できる。現在の「any」フォールバックを解消
- **難易度**: Hard

#### FR-867: Codata / Coinductive Types (余データ・余帰納的型)

- **対象**: `packages/type/type/src/`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 帰納的型（有限データ構造）のみ型システムで表現。無限データ構造（無限ストリーム）の型安全な表現なし
- **内容**: `(defcodata stream (head (-> stream a)) (tail (-> stream a (stream a))))` で余帰納的型を定義。コンストラクタは遅延（thunk化）。`(cl-cc:coiterate f seed)` で無限ストリームを生成。型チェックは **余帰納的等価** （bisimulation）で判定。停止性チェック（FR-713）の余データ版: **生産性チェック** で corecursive 関数が無限に要素を生産することを保証。Haskell lazy lists / Agda コパターンマッチ / Coq CoInductive と同等
- **根拠**: 無限ストリーム・リアクティブシグナル（FR-601 FRP）・I/O チャネルの型安全な表現基盤
- **難易度**: Very Hard

#### FR-868: Heterogeneous Lists / HList (異種リスト)

- **対象**: `packages/type/type/src/`, `packages/frontend/expand/src/expander.lisp`
- **現状**: `list` 型は全要素が同一型。異なる型の要素を持つ固定長リスト（タプル的）の型レベル表現なし
- **内容**: `(deftype hlist () nil)` `(deftype (hlist a . rest) () (cons a (hlist . rest)))` で型レベルリストとして HList を定義。`(cl-cc:hnil)` / `(cl-cc:hcons x xs)` でコンストラクタ。`(cl-cc:hhead xs)` / `(cl-cc:htail xs)` で型安全アクセス（型が自動的に `integer` / `string` に特定）。型レベルプログラミング（FR-595）との統合: `(cl-cc:length-type hlist)` で HList の長さを型レベル自然数として取得。Haskell `HList` library / GHC `DataKinds` TypeLists と同等
- **根拠**: 可変引数関数の型安全な実装基盤。`(cl-cc:zip hl1 hl2)` が要素ごとの型ペアを正確に推論できる
- **難易度**: Hard

#### FR-869: Opaque Types / Abstract Type Boundaries (不透明型・抽象型境界)

- **対象**: `packages/type/type/src/`, `src/package.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 型定義はパッケージ外から構造が見える。内部表現を隠蔽した「抽象型」の型システムレベル強制なし
- **内容**: `(defopaque-type password string)` で`password`型を宣言。`password`の内部が`string`であることはモジュール外から不可視。型チェッカーが`(the string (the password x))`を型エラーにする（`password`と`string`は区別される）。`(cl-cc:coerce-opaque x password)` でモジュール内限定の変換。ML `abstype` / Haskell `newtype` + `module` export control / Java `private` の型システムレベル版。FR-710（Phantom Types）の強化: Phantomは実行時型消去、Opaqueは型システムレベル分離
- **根拠**: APIの表現型（raw string）が意味型（password）と混同されない。型安全な設計によるセキュリティバグの防止
- **難易度**: Medium

---

### Phase 155 — 言語構文拡張

#### FR-872: Do-Notation / Monadic Syntax (do記法・モナド構文)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: モナド的なコードは `(bind m (lambda (x) ...))` の入れ子で記述。可読性が低い
- **内容**: `(cl-cc:do (x <- (read-int)) (y <- (read-int)) (return (+ x y)))` でHaskell-style do記法を提供。マクロが `bind`/`then` 呼び出しへ脱糖。`(cl-cc:do-maybe ...)` / `(cl-cc:do-result ...)` / `(cl-cc:do-io ...)` でモナド特化構文。代数的エフェクト（FR-680）との統合: `(cl-cc:do-effect (IO ...) ...)` でエフェクトハンドラ付きdo記法。Scheme `do` とは別物（こちらはループ記法）。`<-` は Lisp リーダーで `←` 相当のシンボルとして扱う
- **根拠**: エフェクト（IO・エラー・状態）の合成コードが`bind`の深いネストから平坦な逐次記述に変換。可読性を保ちつつFR-680代数的エフェクトの実用性向上
- **難易度**: Medium

#### FR-873: List Comprehensions (リスト内包表記)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: `(loop for x in xs when (pred x) collect (f x))` が最短表記。Python/Haskellスタイルの内包表記なし
- **内容**: `[f x | x <- xs, (pred x)]` を`(cl-cc:list-of (f x) (x <- xs) (pred x))` のマクロ呼び出しへ展開。ネスト: `(cl-cc:list-of (* x y) (x <- xs) (y <- ys))` → デカルト積。ジェネレータ列の遅延評価バリアント: `(cl-cc:lazy-of ...)` で無限リスト生成。集合内包表記: `(cl-cc:set-of ...)` でハッシュセット生成。FR-608（Deforestation）と統合して中間リスト生成を排除（`map compose filter`に変換）
- **根拠**: `loop`マクロより意図が明確で数学的記法に近い。コンパイラ内部のフィルタリング処理の可読性向上
- **難易度**: Easy

#### FR-874: Generator / yield Syntax (ジェネレータ・yield構文)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: FR-653（Lazy Eval）でthunkベースの遅延シーケンス。Python的な`yield`構文なし
- **内容**: `(cl-cc:defgenerator fibonacci () (let ((a 0) (b 1)) (loop (cl-cc:yield a) (psetq a b b (+ a b)))))` でジェネレータ関数を定義。`(cl-cc:next gen)` で次の値を取得（`nil` で終了）。コンパイラがジェネレータをコルーチン（FR-166）に変換: `yield`が`(coroutine-yield val)` に展開。`(cl-cc:for-each (x gen) body)` でジェネレータのイテレーション。FR-867（Codata）との統合: ジェネレータは余帰納的型として型付け可能
- **根拠**: 大規模データの遅延処理・無限シーケンスの自然な記述。Python generators / JavaScript generators と同等の開発体験
- **難易度**: Medium

#### FR-875: View Patterns / As-Patterns (ビューパターン・Asパターン)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: パターンマッチは構造的分解のみ。関数適用結果でのマッチ・元の値を別名で参照する構文なし
- **内容**: **Asパターン** (`@`): `(match x ((@ whole (cons h t)) ...))` で `whole` に元のリスト、`h`/`t` に分解した結果を同時バインド。再割り当てなし: `whole` は `x` と同一オブジェクト（コピーなし）。**ビューパターン**: `(match x (((view abs-value) v) ...))` で `(abs-value x)` の結果 `v` にマッチ。Haskell `@` patterns / GHC `ViewPatterns` / Rust `binding @ pattern` と同等。FR-728（Decision Tree）での最適化: Asパターンは分岐コスト増加なし
- **根拠**: データ構造の一部を分解しつつ全体も参照するパターンが多い（リストの先頭確認+全体渡し）。コードの明確化とコピー排除を同時実現
- **難易度**: Medium

---

### Phase 156 — REPL・インタラクティブ機能

#### FR-878: REPL Tab Completion (REPLタブ補完)

- **対象**: `cli/src/main.lisp`, `packages/engine/vm/src/symbols.lisp`
- **現状**: REPL（FR-098）は基本的な読み込み・評価・印刷のループ。タブキーでの補完なし
- **内容**: GNU Readline / libedit 統合でタブ補完を実装。補完対象: 現在の`*package*`の全シンボル・グローバル変数・マクロ名・CLOSクラス名・スロット名（`(slot-value obj <TAB>`→スロット名一覧）。S式文脈解析: `(defun <TAB>` → 関数名補完、`(make-instance '<TAB>` → クラス名補完。Fuzzy matching: `fcl` → `funcall`。補完候補はFR-725（Perfect Hash Symbol Table）から高速取得。LSP補完（FR-070）と同一ロジックを共有
- **根拠**: 開発者がシンボル名・スロット名を記憶せずにインタラクティブ開発可能。SLIME/SLY の最重要機能の独立実装
- **難易度**: Medium

#### FR-879: REPL Multiline Input Detection (REPL複数行入力検出)

- **対象**: `cli/src/main.lisp`, `packages/frontend/parse/src/cl/parser.lisp`
- **現状**: REPLは1行ずつ読み取り。`(defun foo (x)\n  (+ x 1))` のような複数行入力が改行で即評価される
- **内容**: パーサが**括弧の均衡**を追跡し、未閉じの括弧がある場合は継続プロンプト (`..>`) を表示して次行を読み込み。`(read-balanced-form)` が EOF / 完全なS式 / 不完全なS式を区別して返す。インデントヒント: 継続行の自動インデント（開き括弧の次列に揃える）。マルチライン入力のヒストリーは1エントリとして記録（FR-880）。Emacs SLIME / sbcl --interactive の動作を独立実装
- **根拠**: 現状のREPLで複数行defunを入力するには特別な工夫が必要。基本的なインタラクティブ使用体験の向上
- **難易度**: Easy

#### FR-880: REPL Syntax Highlighting (REPLシンタックスハイライト)

- **対象**: `cli/src/main.lisp`, `packages/frontend/parse/src/lexer.lisp`
- **現状**: REPL出力は全て同一色。型エラー・シンボル・数値・文字列が視覚的に区別できない
- **内容**: ANSI エスケープコード（`\x1b[...m`）でターミナル上のシンタックスハイライト。色スキーム: キーワード（青）・特殊フォーム（紫）・文字列（緑）・数値（シアン）・コメント（グレー）・エラー（赤太字）。Readline callbacks で**入力中**のハイライトもリアルタイム更新。`--no-color` / `NO_COLOR` 環境変数でオフ。REPL出力のプリティプリント（FR-657 Reflection APIと統合）でCLOSオブジェクトを構造的に表示
- **根拠**: シンタックスハイライトは現代の開発ツールの最低限の要件。特にエラーメッセージの赤色強調で即座に問題箇所を認識
- **難易度**: Easy

#### FR-881: REPL Undo / Side-Effect Journal (REPLアンドゥ・副作用ジャーナル)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `cli/src/main.lisp`
- **現状**: REPLでの`defun`/`defvar`/`setf`は不可逆。誤って実行した定義を取り消す手段なし
- **内容**: REPLセッション中の全**グローバル状態変化**をジャーナルに記録: `(defun foo ...)` → 旧定義を保存、`(defvar x 10)` → 旧値を保存、`(setf (gethash :k ht) v)` → 旧エントリを保存。`(cl-cc:undo)` で直前の状態変化を逆適用。`(cl-cc:undo-to 5)` でREPL入力5番目の状態まで巻き戻し。ファイルI/O・外部プロセス呼び出しはジャーナル外（不可逆とマーク）。FR-507（Time-Travel Debugging）のREPL特化版
- **根拠**: `(defun+` の誤タイプで関数を壊した後に `Ctrl-Z` で戻れる体験。REPLベースの探索的開発での試行錯誤コストを劇的に削減
- **難易度**: Hard

---

### Phase 157 — FFI高度化

#### FR-884: C Callbacks from Lisp (LispからCへのコールバック)

- **対象**: `src/ffi/`, `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: LispからC関数を呼び出し可能（FR-707 dlopen）。C関数がLisp関数をコールバックとして呼ぶ逆方向のFFIなし
- **内容**: `(cl-cc:make-callback #'my-handler :ctype (-> int int))` でC互換な関数ポインタを生成。コンパイラがLisp関数をC ABI（System V AMD64 / Win64 ABI）でラップするtrampoline コードを生成。コールバック中のGCは一時停止（FR-551 Safepoint）。コールバックからLispシグナルが投げられた場合の処理（`longjmp` ベース脱出）。CFFI `defcallback` / JNI `CallbackObjectV` / Python `ctypes.CFUNCTYPE` と同等
- **根拠**: GUI ツールキット（GTK/Qt）・イベントループ・ソートアルゴリズム（qsort の比較関数）・libusb等のasync APIがコールバック必須
- **難易度**: Hard

#### FR-885: Variadic C Function Support (可変長引数C関数サポート)

- **対象**: `src/ffi/`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: FFI（FR-632）は固定引数の関数のみ。`printf`・`open`等の可変長引数C関数の呼び出し不可
- **内容**: `(cl-cc:foreign-call-varargs "printf" :int (:string "%d %s\n") (list 42 "hello"))` で可変長引数呼び出し。System V AMD64 ABI: `%al` = 使用するSSEレジスタ数を設定してから `call`。Win64 ABI: シャドウスペース + スタック渡し。型マッピング: Lisp integer → C `int`/`long`、Lisp float → C `double`（デフォルト昇格）。Lispで可変長引数関数を**定義**する場合（Cから呼ばれる）: `(cl-cc:define-varargs-callback f (fixed-arg &rest args) ...)` で `va_list` をLispシーケンスとして受け取り
- **根拠**: POSIX API (`open(2)` の O_CREAT時3引数)・フォーマット系関数（`printf` / `sprintf` / `syslog`）に必須
- **難易度**: Hard

#### FR-886: Platform-Specific API Integration (プラットフォーム固有API統合)

- **対象**: `packages/engine/vm/src/io.lisp`, `cli/src/main.lisp`, 新規 `src/platform/`
- **現状**: POSIX API のみ。macOS GCD・Windows IOCP・Linux BPF などのプラットフォーム固有の高性能APIへのアクセスなし
- **内容**: **macOS**: `dispatch_queue_create` / `dispatch_async` (Grand Central Dispatch) のLispラッパー。`kern_return_t`型・Mach メッセージ。**Linux**: `io_uring`（FR-600 統合拡張）・`eBPF` プログラム作成/ロード・`netlink` ソケット。**Windows**: `CreateIoCompletionPort` / `GetQueuedCompletionStatusEx`（IOCP）。全プラットフォームで `(cl-cc:with-platform (:macos gcd-impl :linux epoll-impl :windows iocp-impl) ...)` で統一API
- **根拠**: 高性能ネットワークサーバーや並行I/Oシステムでプラットフォーム固有APIへのアクセスが不可欠
- **難易度**: Hard

#### FR-887: ABI-Stable C Public Interface Generation (ABI安定C公開インターフェース生成)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/binary/src/macho.lisp`, `cli/src/main.lisp`
- **現状**: cl-ccはライブラリとして使えるが、公開するC APIのヘッダファイル・ABI保証なし
- **内容**: `(cl-cc:export-c-api foo :sig (-> integer integer))` でC互換の公開関数を宣言。`./cl-cc build --emit-header foo.h` でC/C++ヘッダーを自動生成（型マッピング・`extern "C"` ラッパー・Doxygen コメント付き）。`--abi-version 2` でAPIバージョン管理（FR-777 ABI Stability Manifest統合）。FR-635（COMDAT）/ シンボルバージョニングとの統合で `.so` の `foo@@V2` 形式シンボルを生成。Rust `#[no_mangle] pub extern "C" fn` / Swift `@_cdecl` と同等
- **根拠**: cl-ccを他言語（C/Python/Ruby）から動的ライブラリとして使用できるエコシステム基盤。selfhostingしつつCライブラリも提供
- **難易度**: Medium

---

### Phase 158 — 文字列・テキスト処理

#### FR-890: Unicode Normalization (Unicode正規化)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/engine/vm/src/symbols.lisp`
- **現状**: 文字列はUTF-8バイト列として保持。NFC/NFD/NFKC/NFKD正規化なし。同一文字を表す異なるコードポイント列が`equal`でfalseになる
- **内容**: `(cl-cc:unicode-normalize s :nfc)` で NFC（合成済み正規形）に変換。`(cl-cc:unicode-normalize s :nfd)` で NFD（分解正規形）。`(cl-cc:unicode-normalize s :nfkc)` / `:nfkd` で互換正規化。Unicode Derivation Tables（CompositionExclusions.txt / UnicodeData.txt）を内蔵ルックアップテーブルとして コンパイル時生成（FR-670 Constexpr活用）。シンボルインターン（FR-722）時に自動NFC正規化を選択可能。ICU / Java `Normalizer2` / Python `unicodedata.normalize` と同等
- **根拠**: 国際化テキスト処理でNFC/NFD混在による比較バグを防止。Lisp識別子のUnicode対応（NFC正規化）
- **難易度**: Medium

#### FR-891: SIMD String Searching (SIMD文字列探索)

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: `(search needle haystack)` はバイト単位の線形探索。大きな文字列での検索が遅い
- **内容**: **SIMD加速 Boyer-Moore-Horspool**: `VPCMPEQB` で16/32バイト同時比較、ミスマッチを `VPMOVMSKB` でビットマスク取得。**SWAR（FR-825）版**: 64ビット整数に4バイトパターンを詰めてXOR+ANDで検索。`PCMPESTRI`（x86 SSE4.2）: 文字クラス検索・部分文字列検索を専用命令で高速化。`(cl-cc:string-contains-simd haystack needle)` でSIMD版を明示的に使用。`(cl-cc:find-char-class s #'digit-char-p)` でSIMD文字クラス検索。glibc `memmem` / Hyperscan（Intel）の手法
- **根拠**: テキストパーサ（selfhostingパーサ・LSPの全文検索）での文字列探索がホットスポット。SIMD版でスカラー比10〜50倍高速
- **難易度**: Hard

#### FR-892: Cryptographically Secure PRNG / CSPRNG (暗号学的擬似乱数生成)

- **対象**: 新規 `src/crypto/random.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: 乱数生成は`(random n)`（SBCL Mersenne Twister）のみ。暗号学的安全性なし・状態予測可能
- **内容**: `(cl-cc:crypto-random n)` でCSPRNG（暗号学的安全な疑似乱数）を提供。実装: OS乱数（`getrandom(2)` Linux / `arc4random(3)` macOS / `BCryptGenRandom` Windows）をシードとしたChaCha20-based PRNG（RFC 7539）。`(cl-cc:random-bytes n)` でnバイトのランダムバイト列。`(cl-cc:random-uuid)` でUUID v4生成。FR-849（AES-NI）との統合でAES-CTR-based CSPRNG も選択可能。`with-random-seed`でテスト用に固定シード設定可能
- **根拠**: セキュリティトークン生成・暗号キー導出・セキュアなセッションID生成にCSPRNG必須。Mersenne Twisterは暗号用途に使用不可
- **難易度**: Medium

#### FR-893: Hashing Library / Universal Hash Functions (ハッシュライブラリ・汎用ハッシュ関数)

- **対象**: `packages/engine/vm/src/hash.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: ハッシュテーブル（`gethash`）のキーハッシュは`sxhash`（CL標準）のみ。高速ハッシュ・衝突耐性ハッシュなし
- **内容**: 複数のハッシュアルゴリズムを提供: **FNV-1a**: 高速、小さな値向け。**xxHash64**: 大バッファ向け最速（SIMD対応）。**SipHash-2-4**: HashDoS（意図的衝突攻撃）耐性・デフォルト採用（Python3・Rust の標準）。**MurmurHash3**: 分散処理向け良好な分布。`(cl-cc:hash-with :siphash key seed)` でアルゴリズム選択。ハッシュテーブルのresize時に自動的にseedをランダム化（HashDoS対策）。`--hash-seed random`（デフォルト）/ `--hash-seed 0`（デバッグ用固定）
- **根拠**: 現状のsxhashは遅く分布も均一でない。SipHashでHashDoS攻撃を防止しつつxxHashで大バッファを高速処理
- **難易度**: Medium

---

### Phase 159 — ライブラリ・配布形式

#### FR-896: Shared Library Compilation (.so / .dylib 生成)

- **対象**: `packages/backend/binary/src/macho.lisp`, `cli/src/main.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: `./cl-cc compile` は実行可能バイナリのみ生成。動的ライブラリ（`.so` / `.dylib`）形式での出力なし
- **内容**: `./cl-cc compile --shared -o libfoo.so foo.lisp` で共有ライブラリを生成。ELF `.so`: PIC コード（FR-051 PIC）+ `.dynsym` / `.dynstr` / `DT_NEEDED` セクション。Mach-O `.dylib`: `LC_ID_DYLIB` / `LC_DYLD_INFO_ONLY` / `__DATA_CONST.__got` 生成。FR-887（ABI-Stable C API）と統合して公開シンボルのみexport。`--soname` / `--compatibility-version` オプション。`install_name_tool` 相当の rpath 操作機能
- **根拠**: cl-ccコードをPython・Ruby・Node.jsから`ctypes`/`ffi`でロード可能に。組み込み・プラグイン配布の標準形式
- **難易度**: Hard

#### FR-897: Static Library Compilation (.a 生成)

- **対象**: `packages/backend/binary/src/macho.lisp`, `cli/src/main.lisp`
- **現状**: バイナリのみ生成。スタティックリンクのためのオブジェクトアーカイブなし
- **内容**: `./cl-cc compile --static -o libfoo.a foo.lisp` で `.a` アーカイブを生成。各コンパイル単位を `.o`（ELF relocatable）として出力し `ar cr libfoo.a *.o` でまとめる。`--whole-archive` フラグ対応でLTO（FR-040）との統合。`./cl-cc ar` コマンドで `ar` 相当の操作（create/add/list/extract）。**Thin Archive**: `.a` に実オブジェクトを格納せずパスのみ記録（Xcode thin archive 相当）でビルド速度向上
- **根拠**: 組み込み（FR-605 Bare Metal）・カーネル（FR-898）への静的リンクに必須。C/C++プロジェクトへのcl-ccコードの組み込み
- **難易度**: Medium

#### FR-898: Kernel Module Support (カーネルモジュールサポート)

- **対象**: `cli/src/main.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: FR-605（Bare Metal）はOS不使用環境向け。Linuxカーネル空間でのコード実行未対応
- **内容**: `./cl-cc compile --target linux-kernel-module foo.lisp` でLKM（Loadable Kernel Module）を生成。制約モード: GCなし（FR-719 Epsilon GC）・例外なし（代わりに`IS_ERR`スタイル）・動的割り当て制限（`kmalloc` ラッパーのみ）。`module_init` / `module_exit` エントリポイントを自動生成。`(cl-cc:kernel-func ...)` / `(cl-cc:kernel-printk ...)` のカーネルAPI高水準ラッパー。eBPF（FR-886の拡張）との連携でuserspace↔kernel通信
- **根拠**: デバイスドライバ・ファイルシステム・ネットワークフィルタをLispで記述するニッチだが重要な用途
- **難易度**: Very Hard

#### FR-899: Unikernel / Library OS Target (ユニカーネル・ライブラリOSターゲット)

- **対象**: `cli/src/main.lisp`, `packages/backend/emit/src/`, `packages/engine/vm/src/io.lisp`
- **現状**: Linux/macOS のOSサービスに依存した実行のみ。カーネルなし単独実行形式なし
- **内容**: `./cl-cc compile --target unikernel-xen foo.lisp` でXen PVMハイパーバイザー上で直接起動するイメージを生成。MirageOS / Unikraft / HermiCore の設計に倣い OS サービス（ネットワーク / ファイルシステム / タイマー）をライブラリとして静的リンク。ネットワークスタック: `lwip` または minimalistic TCPスタック の静的リンク。不要なOSコード（プロセス管理・ユーザー権限）をゼロにし攻撃面を最小化。FR-605（Bare Metal）の拡張で仮想マシンサポートを追加
- **根拠**: クラウドFaaS（Function as a Service）環境でコールドスタート時間が100ms→数ms に改善。セキュリティ分離をハイパーバイザーに委譲しTCBを最小化
- **難易度**: Very Hard

---

### Phase 160 — ドキュメント・品質ツール

#### FR-902: Docstring Extraction / API Doc Generation (ドキュメント文字列抽出・API文書生成)

- **対象**: `cli/src/main.lisp`, 新規 `src/docs/extractor.lisp`
- **現状**: `defun`のdocstringは`documentation`関数で取得可能。APIドキュメントの自動生成ツールなし
- **内容**: `./cl-cc doc --format html src/` で全ソースからdocstringを抽出しHTML/Markdown APIドキュメントを生成。抽出対象: `defun` / `defmacro` / `defclass` / `defgeneric` / `defmethod` / `defconstant` の docstring。型アノテーション（FR-type系）・パラメータ説明・戻り値型を自動抽出。`(cl-cc:doc-example ...)` でインラインコード例（FR-903 Doctest用）。生成ドキュメントに FR-657（Reflection API）で取得したメソッド一覧・スロット情報を自動追記。Rustdoc / Haddock / JavaDoc と同等
- **根拠**: cl-cc自身の~600エクスポートシンボルのAPIドキュメントが現在存在しない。セルフホスティング後の公開ライブラリとしての利用性向上
- **難易度**: Medium

#### FR-903: Example Code Testing / Doctest (ドキュメントコード例のテスト)

- **対象**: `cli/src/main.lisp`, `src/testing/`
- **現状**: docstringのコード例が正しいか検証する手段なし。ドキュメントとコードの乖離が発生する
- **内容**: `./cl-cc doctest src/` でdocstring内の`(cl-cc:doc-example ...)`ブロックを抽出して実行・検証。書式: `(cl-cc:doc-example (fib 10) => 55)` → `(assert (equal (fib 10) 55))`に変換して実行。エラー時のコード例: `(cl-cc:doc-example (/ 1 0) => (cl-cc:signals division-by-zero))`。CIパイプラインでの`--doctest-fail-fast`でドキュメントバグを即座に検出。Python doctest / Haskell doctest / Rust `///` tests と同等
- **根拠**: ドキュメントのコード例が古くなってバグを含むケースを自動検出。ドキュメントをテストとして扱い常に最新状態を保証
- **難易度**: Medium

#### FR-904: Type Signature Documentation (型シグネチャ文書化)

- **対象**: `cli/src/main.lisp`, `packages/type/type/src/`, `src/docs/extractor.lisp`
- **現状**: 型推論（FR-type系）の結果はコンパイル時のみ利用。推論された型シグネチャの外部文書化・表示なし
- **内容**: `./cl-cc show-types packages/engine/compile/src/codegen.lisp` で推論済み型シグネチャを一覧表示。`./cl-cc check-types src/` で型エラーのみを報告（コンパイルせず）。型シグネチャをFR-902（docstring抽出）に自動付加。`(cl-cc:print-inferred-type fn)` でREPLから型を確認。Haskell `:type` / TypeScript `--declaration` / mypy `--show-column-numbers` と同等
- **根拠**: Lispの動的型付けで失われがちな型情報を文書化。API利用者が型シグネチャなしに関数を使う困難を解消
- **難易度**: Medium

#### FR-905: Assertion Density Analysis (アサーション密度解析)

- **対象**: `cli/src/main.lisp`, 新規 `src/analysis/assertion-density.lisp`
- **現状**: テスト品質評価はFR-694（Mutation Testing）のみ。ソースコード中のアサーション・契約の密度を測定しない
- **内容**: `./cl-cc assert-density src/` でコードのアサーション密度を計測。対象: `assert`・`check-type`・`(cl-cc:require ...)`（FR-656 Contract Programming）・型アノテーション。密度指標: アサーション/関数数・アサーション/LOC・境界チェック/配列アクセス比。「アサーション密度が低いモジュール」を警告（テスト補強候補）。FR-692（Coverage）と組み合わせ: カバレッジ高×アサーション密度低 → 「テストは通るが検証が弱い」を発見。NASAの Assertion Density Metric（JPL コーディング標準）に準拠
- **根拠**: カバレッジ100%でも各テストが何を検証しているかを定量化できない問題を解消。コードの「防御度」を数値化
- **難易度**: Medium

---
