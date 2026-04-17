# Tooling: Advanced Compilation II

Runtime infrastructure, JIT, object layout optimization, ecosystem/diagnostics, binary/linker, type system extensions, concurrency models, embedded targets, polyhedral/auto-parallelization, multi-level IR, FFI, build reliability, memory representation, function transforms, test instrumentation, build scalability, low-level control.

---

### Phase 104 — ランタイムインフラ基盤

#### FR-550: Stack Map Generation (スタックマップ生成)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/runtime/src/gc.lisp`, FR-421（Moving GC）前提
- **現状**: GCはVM命令レベルのスタック（`vm-call-stack`）のみスキャン。JITコンパイル後の機械語スタックフレーム内ポインタを識別不可
- **内容**: 各JIT関数のコンパイル時に**スタックマップテーブル**を生成：`(pc-offset → {live-registers: {R1,R3}, live-stack-slots: {sp+8, sp+24}})` の対応表。GC時にこのテーブルでスタックフレームを走査してすべてのヒープ参照を発見。LLVM StatepointGC / JVM GC map / V8 StackMaps相当。Moving GC（FR-421）・Concurrent GC（FR-420）の必須前提インフラ
- **根拠**: スタックマップなしでMoving GCは実装不可能。JITコンパイラとGCの連携の要
- **難易度**: Hard

#### FR-551: Safepoints (セーフポイント)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, FR-420（Concurrent GC）前提
- **現状**: GCはVM命令間の暗黙の停止点のみ。JITコード実行中の安全な停止点がない
- **内容**: **ポーリングベース**: JITコードのバックエッジ・関数エントリ・一定命令間隔にポーリング命令（`test [safepoint_flag], 0`）を挿入。GCがflagをセットすると次のポーリングで停止。**スタックウォーキング**: 停止時に全スレッドのフレームをスタックマップ（FR-550）で走査。**Stop-the-World実装**: Concurrent GC（FR-420）のSTWフェーズを実現。JVM Safepoint / HotSpot polling page / V8 safepoints
- **根拠**: Concurrent GCがスタックスキャンを行う唯一安全な方法。ランタイムの根幹インフラ
- **難易度**: Hard

#### FR-552: Write Barrier Optimization (書き込みバリア最適化)

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: 2世代GC（`gc.lisp`）の世代間参照追跡（remembered set）にwrite barrierが必要だが最適化なし
- **内容**: **Card Table barrier**: ヒープを512バイト「カード」に分割し、スロット書き込み時に対応カードをダーティマーク（1バイト書き込みのみ）。**Remembered set barrier**: Old→Youngの参照のみ記録（Young→Old不要）。**Barrier elision**: コンパイル時にYoung世代オブジェクトへの書き込みと証明された場合はバリア省略（エスケープ解析FR-341連携）。**Snapshot-at-the-beginning (SATB)**: Concurrent GC用のバリア実装
- **根拠**: write barrierは頻繁に実行されるコード（スロット書き込み毎）。最適化なしでは5〜10%オーバーヘッドが常に発生
- **難易度**: Hard

#### FR-553: Lazy JIT Compilation / Call Stubs (遅延JITコンパイル)

- **対象**: `src/jit/baseline.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: JIT（FR-330〜331）は呼び出しカウント到達時にコンパイル開始。初回呼び出し時の遅延コンパイルなし
- **内容**: **コールスタブ**: 未コンパイル関数エントリに「コンパイルしてから再呼び出し」する5バイトのtrampoline stub（`call compile_stub`）を配置。初回呼び出し時のみJITコンパイルを起動し、完了後にstubを実際のコードへの`jmp`で上書き（atomic patch）。**バックグラウンドコンパイル**: コンパイルをバックグラウンドスレッドで実行し、インタープリタで実行継続。V8 Ignition stubs / HotSpot interpreter stubs
- **根拠**: 全関数を起動時にコンパイルすると初回レイテンシが爆発。Lazy compilationで使われた関数のみをコンパイルする省エネ戦略
- **難易度**: Hard

#### FR-554: JIT Code Cache Persistence (JITコードキャッシュ永続化)

- **対象**: `src/jit/`, `packages/cli/src/main.lisp`
- **現状**: JITコンパイル結果はプロセス終了時に消失。次回起動で再コンパイルが必要
- **内容**: JITコンパイル済み機械語をファイルにシリアライズ（`~/.cache/cl-cc/jit/<hash>.jit`）。コンテンツハッシュ（ソースハッシュ + CPUフィーチャー + 最適化レベル）でキャッシュキー決定。次回起動時にキャッシュヒットすればJITコンパイルをスキップ。セキュリティ: キャッシュファイルに署名（FR-378）。Chromium V8 Code Cache / Android ART AOT compiler相当
- **根拠**: JITウォームアップ時間の削減。セルフホスティングコンパイラのJIT起動を2回目以降でほぼゼロに
- **難易度**: Hard

#### FR-555: Class Hierarchy Analysis / CHA (クラス階層解析)

- **対象**: 新規`src/analyze/cha.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: devirt（FR-337）はPIC（FR-334）の観測データに依存。クラス階層情報を使った静的解析なし
- **内容**: `*class-registry*` からCLOSクラスの継承木を構築（`class → [subclasses]`）。あるgeneric functionのメソッドが「特定クラスのサブクラスが存在しない」場合は**単相と証明**しdevirt。`(defgeneric area (shape))` で`shape`のサブクラスが`circle`のみなら`area`呼び出しを`area-circle`への直接呼び出しに変換。CHA + PIC = 最強のdevirt。Java HotSpot CHA / .NET CLR devirt
- **根拠**: PICは実行時観測に依存するため静的証明できない。CHAは「このクラスにはサブクラスがない」という不変条件から永続的な最適化が可能
- **難易度**: Medium

---

### Phase 105 — JIT高度化

#### FR-558: Trace JIT / Hot Trace Recording (トレースJIT)

- **対象**: `src/jit/`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: JIT（FR-330〜334）はメソッド（関数）単位のコンパイル。ホットトレース（パス）単位のコンパイルなし
- **内容**: **トレース記録**: ループバックエッジのホットカウント到達時に実行パスを**線形命令列（トレース）**として記録。呼び出し先関数も含めてインライン化した超長直線コードを生成。**ガード命令**: トレースが想定した型・分岐方向と異なる場合に脱出（side exit）するガード挿入。**トレースツリー**: 複数のsideexitから派生するトレースをツリー構造で管理。LuaJIT / Mozilla TraceMonkey (Firefox 3.5〜) / PyPy
- **根拠**: LuaJITがトレースJITで他のLuaより10〜50x高速化を実現。メソッドJITより呼び出しオーバーヘッドがなく、ループ集中型ワークロードに特に有効
- **難易度**: Very Hard

#### FR-559: Type Feedback Collection (型フィードバック収集)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `src/jit/baseline.lisp`
- **現状**: PIC（FR-334）はメソッドキャッシュのみ。汎用な型フィードバック（引数型・戻り値型の実行時観測）なし
- **内容**: **ICベース型プロファイリング**: 各呼び出しサイト・各演算に**型プロファイルスロット**を付与。Tier-0/Tier-1実行時に型観測データを蓄積（`fixnum/float/string/cons`の出現率）。**フィードバックベクタ**: 関数ごとのICデータを構造化して保持。Tier-2コンパイル時にフィードバックデータを入力としてspecialized codeを生成。V8 Maglev / HotSpot type profiling
- **根拠**: PICだけではカバーできない「変数の型」の観測。型フィードバックはJIT投機的最適化の全ての基盤
- **難易度**: Hard

#### FR-560: Speculative Inlining with Guards (ガード付き投機的インライン化)

- **対象**: `src/jit/`, FR-559（型フィードバック）前提
- **現状**: インライン化（ML-guided FR-372）は静的サイズ閾値。型フィードバックに基づく動的インライン化なし
- **内容**: 型フィードバック（FR-559）で「この呼び出しサイトで受信したオブジェクトの98%がclass Xである」と判明した場合、**型ガード + インライン済みコード + deoptスローパス**を生成。型ガード（`typep obj 'X`）が失敗した場合のみdeopt（FR-333）。ホットパスは型チェック1命令 + インライン済みメソッド本体。V8 TurboFan / HotSpot speculative inlining
- **根拠**: 一般的なCLOSコードで呼び出しの95%以上が同一型。投機的インライン化でgeneric dispatch overhead完全除去
- **難易度**: Hard

#### FR-561: Megamorphic IC Handling (メガモーフィックICハンドリング)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `src/jit/baseline.lisp`, FR-334（PIC）の拡張
- **現状**: PIC（FR-334）は最大4エントリでオーバーフロー後はグローバルキャッシュにフォールバック
- **内容**: PICが4エントリを超えた**メガモーフィック**状態での専用最適化: **グローバル型フィードバックキャッシュ**: 全コールサイト横断でのメソッドキャッシュ共有。**Inlining caches for megamorphic**: ハッシュベースのO(1)ディスパッチを維持しつつ予測不能な分岐を除去。**型分布に基づく確率的speculative inlining**: 最頻出1〜2型のみガード付きインライン化
- **根拠**: GCキャラクタリゼーション的コード（多相コレクション操作）では避けられないメガモーフィック状態。専用ハンドリングで未最適化フォールバックを回避
- **難易度**: Hard

#### FR-562: JIT Warmup / AOT Pre-warming (JITウォームアップ最適化)

- **対象**: `src/jit/`, `packages/cli/src/main.lisp`
- **現状**: JIT（FR-330〜331）はcall countが閾値到達後に初めてコンパイル。最初の数千回呼び出しはインタープリタ
- **内容**: **プロファイル付きAOTコンパイル**: 事前プロファイル実行（`./cl-cc run --warmup-profile app.lisp`）でホット関数を特定→`--aot-warm`でそれらをプロセス起動前にコンパイル済み状態にする。**Lazy compilation ordering**: 依存関係順にコンパイルして初期化シーケンスを最適化。JIT閾値を0に設定して即時Tier-2へのオプション（`--jit-threshold=0`）
- **根拠**: サーバープロセスの「ウォームアップ期間」でのスループット低下問題（数分間の低速フェーズ）を解消。Javaの-server flagとAOTコンパイルの折衷案
- **難易度**: Medium

---

### Phase 106 — オブジェクト・レイアウト・演算最適化

#### FR-565: Type-Based Alias Analysis / TBAA (型ベースエイリアス解析)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, 新規`packages/engine/optimize/src/tbaa.lisp`, FR-340（エイリアス解析）の拡張
- **現状**: 汎用エイリアス解析（FR-340）はポインタ解析ベース。型情報を使ったエイリアス証明なし
- **内容**: **CLOS型に基づくno-alias証明**: 異なるクラスのインスタンスはスロットが重なりえない（`circle`の`:radius`と`rect`の`:width`はno-alias）。**スロット型計算**: `(slot-value c1 :x)` と `(slot-value r1 :x)` がno-aliasと証明されれば命令リオーダーが可能。LLVM TBAA metadata（`!tbaa`）/ C `restrict` の型システム版。汎用エイリアス解析より低コストで高精度
- **根拠**: CLOSコードのほとんどのスロットアクセスはTBAAで解決できる（クラスが明確な場合）。MemSSA（FR-409）+ TBAAでloop-carried依存の大半を除去
- **難易度**: Medium

#### FR-566: Object Layout Optimization (オブジェクトレイアウト最適化)

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: CLOSオブジェクトはスロット名→値の汎用ハッシュテーブル。フィールドオフセットが実行時計算
- **内容**: **固定オフセット化**: `(defclass point (x y))` → `x`をoffset 0、`y`をoffset 8の**C構造体スタイル**に変換。スロットアクセスが`[base + offset]`の直接メモリ参照に。**ホット/コールドフィールド分離**: 頻繁アクセススロットを先頭に配置（プロファイル駆動）。**ネイティブ構造体としてのコンパイル**: `defstruct`/`defclass`の宣言でハッシュテーブルなし構造体生成。**CLOS互換維持**: MOP経由のダイナミックアクセスにはフォールバック
- **根拠**: 現在のハッシュテーブルベースCLOSオブジェクトに対してslot-value一回が~30命令。固定オフセット化で2命令に
- **難易度**: Hard

#### FR-567: Dead Slot Elimination (デッドスロット除去)

- **対象**: `packages/engine/compile/src/codegen.lisp`, FR-566（Object layout）と連携
- **現状**: `defclass`で定義した全スロットがオブジェクトに存在。読まれないスロットも割り当てられる
- **内容**: **全プログラム解析**: コールグラフ全体を走査し、特定クラスのスロットが一度も`slot-value`で読まれない場合に「デッドスロット」と判定。LTO（FR-335）モードでの全プログラム解析。`--strip-dead-slots`フラグ。デッドスロットは構造体から除去（メモリ節約 + キャッシュ改善）。`(declare (cl-cc:keep-slot x))` でエクスポートスロットを明示保護
- **根拠**: デバッグ用・将来用スロットが残留するのはよくあるパターン。Large objectsでのメモリ削減効果大
- **難易度**: Medium

#### FR-568: Copy Elision / NRVO (戻り値最適化・コピー省略)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: 関数の戻り値は常にスタック/レジスタ経由でコピー。大きなオブジェクト返却のコピーが常に発生
- **内容**: **Named Return Value Optimization (NRVO)**: 関数内で作成して返す単一オブジェクトを呼び出し元の変数に**直接構築**（コピーなし）。**Return Value Optimization (RVO)**: 匿名一時オブジェクトのin-place構築。`(let ((result (make-array n))) ... result)` パターンの検出と変換。C++ RVO/NRVO / Rust move semantics + `return` optimization
- **根拠**: 大きな配列・多スロットCLOSオブジェクトの返却コストをゼロに。数値計算での中間結果コピー除去
- **難易度**: Hard

#### FR-569: Multiple Return Values via Registers (多値レジスタ返却)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/calling-convention.lisp`
- **現状**: `(values a b c)` は複数値オブジェクト（ヒープ割り当て）またはスタック経由で実装
- **内容**: 最大2〜4個の多値を**複数レジスタ**（x86-64: RAX+RDX+RCX+R8）で直接返却。`(multiple-value-bind (a b) (floor x y) ...)` でデスチネーションが確定している場合はヒープ割り当てゼロ。呼び出し側でのmv-call展開時にも複数レジスタを直接受け取るABIを使用。SBCL already does this for 2 values / LuaJIT multiple returns via fixed slots
- **根拠**: `(floor x y)` / `(truncate x y)` / `(round x y)` は数値コードの頻出多値関数。毎回ヒープ割り当てが発生するのは過剰
- **難易度**: Medium

#### FR-570: Branch-Free Arithmetic (分岐フリー算術)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: `(abs x)` / `(min a b)` / `(max a b)` / `(signum x)` が条件分岐にコンパイルされる可能性
- **内容**: **整数abs**: `(abs x)` → `sar reg, 63; xor rax, reg; sub rax, reg`（分岐ゼロ）。**整数min/max**: CMOV命令（`cmovl`/`cmovg`）使用。**float abs/min/max**: `andps`/`andnps`/`maxss`/`minss` SIMD命令。**signum**: ゼロ比較 + 符号ビット抽出の組み合わせ。FR-410（If-Conversion）の特殊ケースとして実装可能
- **根拠**: 分岐予測が難しい数値コード（データ依存のmin/max）でのパフォーマンス。コンパイラが自動変換すれば手書き最適化不要
- **難易度**: Easy

#### FR-571: Interprocedural Register Allocation (手続き間レジスタ割り当て)

- **対象**: `packages/engine/compile/src/regalloc.lisp`, FR-414（Custom Calling Convention）連携
- **現状**: レジスタ割り当ては関数ごと独立。関数境界でのcallee-save保存/復元が常に発生
- **内容**: **呼び出しグラフベースのRA**: リーフ関数（再帰なし・内部呼び出しのみ）について呼び出し側と被呼び出し側が**同一レジスタ集合を共用**するRA。caller-saved拡張（calling convention merging）。非リーフ関数には適用せずABI互換を維持。IPRAアノテーション（`(declare (cl-cc:ipra))`）で明示指定
- **根拠**: GCC IPRA (`-fipa-ra`). 内部呼び出しの多い関数ファミリ（相互再帰）でcallee-save保存を削減
- **難易度**: Hard

---

### Phase 107 — エコシステム・診断追加

#### FR-574: Compilation Database / compile_commands.json (コンパイルデータベース)

- **対象**: `packages/cli/src/main.lisp`, `cl-cc.asd`
- **現状**: 各ファイルのコンパイルオプション・インクルードパスの構造化記録なし
- **内容**: `./cl-cc compile --emit-compile-commands *.lisp` で **`compile_commands.json`** を生成（Clang tooling標準形式）。`[{"file": "packages/engine/vm/src/vm.lisp", "command": "cl-cc compile ...", "directory": "/..."}]` 形式。clangd / clang-tidy の代替ツール（lsp-mode / eglot）が参照可能。`ASDF:compile-system`フックで自動生成
- **根拠**: IDE・静的解析ツール・フォーマッタが「どのオプションでコンパイルするか」を知るための標準インターフェース。2026年でClang tooling chainが標準化
- **難易度**: Easy

#### FR-575: Core Dump / Crash Report Analysis (コアダンプ・クラッシュレポート解析)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/cli/src/main.lisp`
- **現状**: VM例外は`handler-case`でキャッチ。未捕捉例外でのクラッシュ情報が不十分
- **内容**: **構造化クラッシュレポート**: 未捕捉conditionでのVM状態（レジスタ・スタックトレース・ヒープ統計）を`crash-TIMESTAMP.cl-cc-dump`に自動保存。`./cl-cc analyze-crash crash.cl-cc-dump` でインタラクティブなpost-mortem調査（FR-310デバッガの読み込みモード）。**シグナルハンドラ**: SIGSEGV/SIGABRTでCクラッシュをキャッチしてbacktrace収集。macOS CrashReporter / Sentry crash reporting形式での出力
- **根拠**: 本番環境でのVMクラッシュは再現が困難。クラッシュ時の完全状態を保存することでデバッグを可能にする
- **難易度**: Medium

#### FR-576: Work-Stealing Scheduler (ワークスティーリングスケジューラ)

- **対象**: `src/concurrent/`, FR-387（グリーンスレッド）、FR-524（構造化並行性）前提
- **現状**: グリーンスレッドのスケジューラは未実装（FR-387で計画のみ）
- **内容**: **Cilk-like Work Stealing**: 各OSスレッドが**deque**（両端キュー）でタスクを管理。スレッドが暇になると他スレッドのdeque末尾からタスクを**スチール**。`with-nursery`（FR-524）のspawn-inはローカルdequeの先頭に積む。**Continuation Stealing**: 継続を盗む版（call/cc FR-528との統合）。**Randomized victim selection**でデッドロック回避。Intel TBB / Cilk++ / Rust Rayon
- **根拠**: 構造化並行性（FR-524）のスケジューラとして最適。理論的にwork-stealing はO(P × T∞ + T₁/P)の最適実行時間を保証（P: プロセッサ数、T₁: 逐次時間、T∞: 並列深度）
- **難易度**: Hard

#### FR-577: Continuation Marks (継続マーク)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/compile/src/cps.lisp`
- **現状**: デバッガ・プロファイラがスタック情報を得るためにfull call stack（`vm-call-stack`）を走査。実行中のフレームに任意メタデータを添付する手段なし
- **内容**: `(cl-cc:with-continuation-mark key value body)` フォーム：現在のテール位置に**キー・値ペア**をマーク。`(cl-cc:current-continuation-marks key)` で最新マークを取得。テール呼び出し時はマークを置き換える（スタックスペース O(1)）。**用途**: プロファイラ（関数名マーク）・デバッガ（ブレークポイント条件）・動的変数（FLUIDバインディング）。Racket `with-continuation-mark` / SRFI-157
- **根拠**: full call/cc（FR-528）なしでデバッガ・プロファイラが必要な情報を効率的に取得できる。スタックを汚染しないメタデータ伝達機構
- **難易度**: Medium

#### FR-578: Compile-Time SQL DSL (コンパイル時SQLドメイン特化言語)

- **対象**: 新規`packages/engine/compile/src/sql-dsl.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: SQLクエリは実行時文字列結合。型安全性・インジェクション防止なし
- **内容**: `(cl-cc:sql select (user.name user.email) from user where (= user.age (param age fixnum)))` マクロ: **コンパイル時SQL検証**（構文・テーブル名・カラム名）。`param`でバインドパラメータ（SQLインジェクション防止）。スキーマ定義（`cl-cc:defsql-schema`）からの型チェック。プリペアドステートメントへの自動変換。Haskell Persistent / TypeScript Prisma / Rust SQLx の型安全クエリに相当するCL実装
- **根拠**: CL製Webアプリでのプリペアドステートメントの一貫した使用を強制。コンパイル時エラーでSQLインジェクション脆弱性を排除
- **難易度**: Hard

#### FR-579: REPL Session Recording & Replay (REPLセッション記録・再生)

- **対象**: `packages/cli/src/main.lisp`, FR-312（REPL拡張）の拡張
- **現状**: REPLの入力/出力は揮発性。セッションの再現不可
- **内容**: `./cl-cc repl --record session.cl-cc-repl` でREPL入力・出力・VM状態差分を構造化JSON/Lispデータとして記録。`./cl-cc repl --replay session.cl-cc-repl` で再生（テスト化）。`(cl-cc:save-session "file.cl-cc-repl")` REPLコマンド。**REPLセッション→テストスクリプト変換**: 記録したセッションをFiveAMテストに自動変換（`--export-as-tests`）
- **根拠**: SBCL REPLでの探索的開発の結果をテストに変換するワークフロー。バグ再現セッションの共有にも有効
- **難易度**: Medium

#### FR-580: GC Safepoint-Free Regions (GCセーフポイントフリー領域)

- **対象**: `packages/backend/runtime/src/gc.lisp`, FR-551（Safepoints）の拡張
- **現状**: 全コードパスにセーフポイントポーリングが挿入される（FR-551）
- **内容**: `(cl-cc:without-gc-preemption ...)` フォームでGCセーフポイントチェックを一時無効化。**リアルタイムコード**: オーディオコールバック・割り込みハンドラ等のGCポーズ許容不可なパスに使用。無効化中のアロケーションは禁止（コンパイル時検証、FR-357 Allocation-free verificationと連携）。Azul Zing / OpenJDK `@Restricted` annotation相当
- **根拠**: ソフトリアルタイム要件（レイテンシ保証）のコードでGCセーフポイントの確定的除去が必要
- **難易度**: Medium

---

### Phase 108 — バイナリ・リンカ高度化 II

#### FR-582: Identical Code Folding / ICF (同一コード折り畳み)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/backend/binary/src/elf.lisp`
- **現状**: 内容が同一の関数が異なるシンボル名で複数コンパイル済みバイナリに存在しうる
- **内容**: リンク時に全関数のバイト列ハッシュを計算し、**内容が同一の関数を1つに統合**（別名シンボルを同一アドレスに向ける）。セーフICF（`--icf=safe`）: 呼び出し可能なアドレス比較を行うコードがある場合は除外。アグレッシブICF（`--icf=all`）: 関数ポインタ比較も無視して統合。Mach-O `.subsections_via_symbols` + LD64 `-object_path_lto`、ELF LLD `--icf=safe`
- **根拠**: Chromeでの計測で3〜6%のバイナリサイズ削減。cl-ccが生成するCLOS accessor stub・ジェネリック dispatch wrapper に多数の同一コードが存在
- **難易度**: Medium

#### FR-583: Split Debug Info / .dwo Files (分離デバッグ情報)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/backend/binary/src/elf.lisp`, FR-195（DWARF）拡張
- **現状**: DWARF情報は本体バイナリに直接埋め込み（計画段階）
- **内容**: **DWARF Skeleton CU + .dwo**: バイナリ本体には「スケルトン」（アドレス→ファイル名+行番号のマッピングのみ）を残し、詳細デバッグ情報（型情報・変数名）を別ファイル`foo.dwo`に分離。`dwp`（DWP packager）で複数.dwoを`foo.dwp`に集約。本番バイナリの肥大化なし + デバッガは.dwpを参照。**DWARF zstd圧縮**（DWARF 5 `DW_FORM_strp_sup` + ZSTD）: デバッグ情報を1/5〜1/10に圧縮
- **根拠**: 実用的なLispバイナリでDWARFが本体の数倍のサイズになることも。Clang `-gsplit-dwarf` / GCC `-gsplit-dwarf` は本番デプロイの標準
- **難易度**: Medium

#### FR-584: Patchable Function Entry Points (パッチ可能関数エントリ)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64-codegen.lisp`
- **現状**: 関数エントリは最小命令。実行中の差し替えのための予約バイトなし
- **内容**: `--patchable-function-entry=N` フラグで各関数エントリ前にN個の**NOP命令**を挿入（x86-64: `nop` / AArch64: `nop`）。実行時にNOP列を`jmp patch_handler`で上書き可能（dtrace / SystemTap / eBPF uprobe方式）。**Hot patching**: ライブラリの関数を再起動なしに差し替え（`-fpatchable-function-entry=8,4`でエントリ前4バイト + 後4バイト）。GCC/Clang `-fpatchable-function-entry`
- **根拠**: 本番環境での動的計装（profiling・hot fix）に必要。FR-576（ワークスティーリング）のスケジューラフックにも使用
- **難易度**: Medium

#### FR-585: Cache Line Alignment Annotations (キャッシュライン整列アノテーション)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: データ構造のキャッシュライン境界へのアライメント指定なし
- **内容**: `(declare (cl-cc:align 64))` でCLOSオブジェクト・配列を64バイトキャッシュライン境界に配置。`(cl-cc:define-aligned-struct name 64 ...)` でアライメント付き構造体定義。`(cl-cc:cache-line-size)` → 実行時キャッシュライン検出（`cpuid`）。SIMD命令の`vmovaps`（aligned）使用でペナルティゼロ。ヒープアロケータ（FR-423アリーナ）のデフォルトアライメント設定
- **根拠**: SSE/AVX aligned load/storeは非アライメントより最大10%高速。ホットデータ構造の意図的アライメントがL1キャッシュ効率を大幅改善
- **難易度**: Easy

#### FR-586: False Sharing Elimination (偽共有除去)

- **対象**: 新規`src/concurrent/padded.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 並列スレッドが同一キャッシュライン内の異なる変数を更新した場合の偽共有問題への対策なし
- **内容**: `(cl-cc:defpadded-var *counter* 0)` で隣接変数と別キャッシュラインに強制配置（64バイトパディング）。スレッドローカルストレージ（TLS）との連携: `(cl-cc:thread-local *local-count* 0)` で変数をスレッドローカルに。**False sharing detector**: Thread Sanitizer（FR-399）拡張で同一キャッシュラインへの競合アクセスを警告。Linux `perf c2c`（Cache to Cache）相当の分析
- **根拠**: マルチコア並列コードの主要なパフォーマンス罠。並列カウンタ・並列アロケータで実測5〜30x減速事例あり
- **難易度**: Medium

#### FR-587: GOT/PLT Lazy Binding Optimization (GOT/PLT遅延バインディング最適化)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/backend/binary/src/elf.lisp`, FR-197（PIC）拡張
- **現状**: PIC（FR-197）でGOT/PLTを生成するが遅延バインディング最適化なし
- **内容**: **PLT IBTPLT**: 関数が同一バイナリ内で定義されている場合はPLTをバイパスして直接RIP-relative呼び出し。**GOTPCRELX relocation**: `mov rax, [rip+got@gotpcrelx]` → `lea rax, [rip+symbol@plt]` への最適化（linker relaxation）。**Eager binding** (`RTLD_NOW`): 起動時に全シンボル解決（セキュリティ向上）。ELF `-z now` / Mach-O `-bind_at_load`
- **根拠**: PLT indirectionは関数呼び出し毎に余分なジャンプ1回。ライブラリ呼び出し頻度が高い場合に顕著
- **難易度**: Hard

---

### Phase 109 — 型システム拡張 III

#### FR-590: Algebraic Data Types / ADTs (代数的データ型)

- **対象**: `packages/frontend/expand/src/macros-basic.lisp`, `packages/foundation/type/src/types.lisp`
- **現状**: CLの型システムはCLOSクラス階層（積型）のみ。和型（tagged union / sum types）の組み込みサポートなし
- **内容**: `(cl-cc:defdata shape (circle :radius float) (rect :width float :height float) (triangle :base float :height float))` マクロ。**タグ付きユニオン**として効率的に表現（最大バリアントサイズ + 1タグワード）。`(cl-cc:match)` との統合（FR-530 Pattern matchingとの連携）で**コンパイル時網羅性保証**。型推論で各バリアントを識別。Haskell ADT / Rust enum / OCaml variant
- **根拠**: CLのdefclassはopen（後付けサブクラス可能）で網羅性保証が難しい。ADTはclosed（定義時に全バリアント固定）で網羅性をコンパイル時に検証できる
- **難易度**: Hard

#### FR-591: Newtype / Zero-Cost Wrappers (ニュータイプ・ゼロコストラッパ)

- **対象**: `packages/foundation/type/src/types.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 型エイリアス（`(deftype positive-integer () ...)`)はコンパイル時に消去され名目型（nominal type）が使えない
- **内容**: `(cl-cc:defnewtype user-id fixnum)` でfixnumと同一実装だが**型検査は別物**の新型定義。`(user-id 42)` が`fixnum`を取る関数に誤渡しされるとコンパイルエラー。**ゼロコスト**: ランタイム表現はラップ元と同一（ボックス化なし）。`(cl-cc:unwrap x)` で明示的にアンラップ。Haskell `newtype` / Rust newtype pattern / Kotlin value class
- **根拠**: `user-id`と`product-id`が両方fixnumの場合に混同をコンパイル時に検出。API設計の安全性向上
- **難易度**: Medium

#### FR-592: Higher-Kinded Types / HKT (高カインド型)

- **対象**: `packages/foundation/type/src/inference.lisp`, `packages/foundation/type/src/types.lisp`
- **現状**: 型パラメータは具体型のみ（`(list fixnum)` 等）。型コンストラクタを型引数にできない
- **内容**: `(cl-cc:definterface Functor (F) (fmap (fn (a) b) (F a) → (F b)))` のような**型コンストラクタ上の抽象**。`*` / `* → *` / `(* → *) → *` 等のカインドアノテーション。型推論にカインドチェックを統合。**型クラス/プロトコル**: 複数の型にわたる共通インターフェースをHKTで表現（FunctorをListにもMaybeにも適用）。Haskell type classes / Scala implicits / Rust traits の型システム基盤
- **根拠**: `(map f (list 1 2 3))` と `(map f (maybe 5))` を同一プロトコルで書けるようになる。CLのCLOS generic functionとは別の静的なポリモーフィズム
- **難易度**: Very Hard

#### FR-593: Bidirectional Type Checking (双方向型検査)

- **対象**: `packages/foundation/type/src/inference.lisp`
- **現状**: MEMORY.mdに「Bidirectional type inference (check mode vs synth mode)」が「Remaining Work」として記載。未実装
- **内容**: **Check mode（下向き）**: 期待型が既知の文脈（`(the fixnum expr)` / `(declare (type fixnum x))` / 関数引数位置）でexpression typeを型に対してcheckする。**Synth mode（上向き）**: 型が不明な文脈でexpressionから型を合成する。現在の単方向HMより多くの型を推論できる（特に高階関数・ADT）。Dunfield-Krishnaswami ICFP 2013 Bidiretional Typing
- **根拠**: MEMORY.mdに明記されたRemainingWork。双方向化によりアノテーション数が大幅に削減され、複雑なHOF・ADTパターンの型推論が可能に
- **難易度**: Hard

#### FR-594: Gradual Typing Improvements (段階的型付け改善)

- **対象**: `packages/foundation/type/src/inference.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 型付きコードと非型付きコードの境界での型変換（キャスト）の最適化なし
- **内容**: **Cast insertion optimization**: 型境界でのランタイムチェック(`typep`)挿入を最小化（隣接するキャストのキャンセル検出）。**Blame tracking**: 型違反が発生した場合の責任帰属（型付きコードが悪いか、非型付きコードが悪いか）を追跡してエラーメッセージに表示。**Concrete type propagation**: 型付き関数が非型付き関数を呼ぶ場合の型情報伝播の改善。Typed Racket / Reticulated Python / Transient semantics
- **根拠**: CLの段階的型付けは`:type`宣言と`check-type`の組み合わせに依存。型境界の最適化なしではパフォーマンスペナルティが大きい
- **難易度**: Hard

#### FR-595: Type-Level Computation (型レベル計算)

- **対象**: `packages/foundation/type/src/inference.lisp`
- **現状**: 型は静的な集合論的記述のみ。型レベルの計算（条件・算術）なし
- **内容**: **Type families**: `(cl-cc:deftype-family ElementType (T) ...)` で型パラメータから別の型を計算。`(ElementType (list fixnum))` → `fixnum`。**条件型**: `(cl-cc:if-type (subtype-p T fixnum) T float)` で型レベルのif式。**型レベル自然数**: `(array-length-type 5)` → `5-element-fixnum-array`の型。Haskell TypeFamilies / TypeLits / C++ `std::conditional_t`
- **根拠**: ADT（FR-590）・HKT（FR-592）の実用的活用に型レベル計算が必要。配列長を型に埋め込んで境界チェックをゼロコスト化できる
- **難易度**: Very Hard

---

### Phase 110 — 並行モデル拡張

#### FR-598: Actor Model (アクターモデル)

- **対象**: `src/concurrent/`, FR-387（グリーンスレッド）+ FR-524（構造化並行性）前提
- **現状**: グリーンスレッドは共有メモリ型。メッセージパッシング抽象なし
- **内容**: `(cl-cc:defactor my-actor (state) (on :increment (delta) (+ state delta)) (on :get () state))` マクロ。**軽量アクタープロセス**（Erlang-style）: メールボックス付きgreem thread。`(cl-cc:send actor :increment 5)` でメッセージ送信。`(cl-cc:receive &key timeout)` で受信。**Supervision tree**: 子アクタのクラッシュを親が監視・再起動。**Link/monitor**: `(cl-cc:link actor)` でアクタ間の死活監視。Erlang/OTP / Akka / Elixir GenServer
- **根拠**: 共有メモリ並行性（STM/lock-free）より推論が容易な並行モデル。CLのイメージベース開発（FR-364）とアクタのhot upgrade（FR-363）の統合が自然
- **難易度**: Hard

#### FR-599: Channel-Based Concurrency (チャネルベース並行性)

- **対象**: `src/concurrent/`, FR-387（グリーンスレッド）前提
- **現状**: グリーンスレッド間の通信抽象なし
- **内容**: `(cl-cc:make-channel :type fixnum :buffer 10)` でバッファ付きチャネル生成。`(cl-cc:send ch value)` / `(cl-cc:receive ch)` でブロッキング送受信。`(cl-cc:select (ch1 v1 ...) (ch2 v2 ...) (:default ...))` でノンブロッキング多重待機。**方向付きチャネル型**: `(send-channel fixnum)` / `(recv-channel fixnum)`。Go channels / Kotlin Channel / Rust mpsc + sync_channel / CSP（Hoare 1978）
- **根拠**: 共有状態なしの純粋メッセージパッシング。「メモリを共有するな。通信で共有せよ」のGoスタイル。STM（FR-523）との使い分けが明確
- **難易度**: Hard

#### FR-600: io_uring / kqueue Integration (非同期I/O統合)

- **対象**: `packages/engine/vm/src/io.lisp`, `src/concurrent/`, `packages/cli/src/main.lisp`
- **現状**: `packages/engine/vm/src/io.lisp` はSBCLのブロッキングI/O呼び出し。イベント駆動I/Oなし
- **内容**: **Linux io_uring**: `io_uring_setup` / `io_uring_enter` syscallをFFI（FR-194）経由で呼び出し。`(cl-cc:async-read fd buffer callback)` → io_uring SQE投入。**macOS kqueue / kevent**: `kqueue()`+ `kevent()`でI/O多重化。`(cl-cc:async-accept socket callback)` でノンブロッキングacceptループ。async/await（FR-388）のバックエンドとしてio_uringを使用。**ゼロコピー**: `io_uring_prep_read_fixed` + registered buffers
- **根拠**: Linux io_uring（Jens Axboe, 2019〜）は2026年でシステムコールオーバーヘッドを1/10に削減。Node.js libuv / Tokio（Rust）が採用。ネットワークサーバー性能の決定要因
- **難易度**: Hard

#### FR-601: Reactive Streams / FRP (リアクティブストリーム)

- **対象**: 新規`src/reactive/`, FR-387（グリーンスレッド）+ FR-599（チャネル）前提
- **現状**: データパイプラインの組み合わせ抽象なし
- **内容**: `(cl-cc:observable (from-list '(1 2 3)) (map #'*2) (filter #'evenp) (reduce #'+ 0))` パイプライン。**バックプレッシャー**: 下流の処理速度に合わせて上流の生成を制御（`(cl-cc:throttle n)` / `(cl-cc:buffer n)`）。**冷たい/熱いObservable**: 遅延評価ストリームvs即時ブロードキャスト。**エラー伝播**: ストリーム内エラーのハンドリング（`(cl-cc:catch-error handler)`）。RxJava / Kotlin Flow / Reactor / Akka Streams
- **根拠**: コンパイラパイプライン自体がデータ変換ストリームであり（パース→展開→CPS→最適化）、Reactive Streamsで表現可能。バックプレッシャー制御でメモリ効率が良い
- **難易度**: Hard

#### FR-602: Structured Logging Macros (構造化ロギングマクロ)

- **対象**: 新規`packages/engine/compile/src/logging.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: `format`ベースのフリーテキストログのみ。構造化ログ（JSON/ECS）の生成なし
- **内容**: `(cl-cc:log :info "request processed" :user-id user-id :duration-ms elapsed)` マクロ。コンパイル時に**キー名の重複・型チェック**（FR-533フォーマット検証との類比）。`--log-level=debug/info/warn/error`での動的フィルタ。無効ログレベルのログ呼び出しをコンパイル時に**ゼロコスト**に（dead-code elimination）。ECS（Elastic Common Schema）/ OpenTelemetry logs形式のJSON出力。Slog（Go 1.21）/ tracing（Rust）相当
- **根拠**: フリーテキストログは検索・集計が困難。構造化ログのコンパイル時型検査でキー名タイポを排除
- **難易度**: Medium

---

### Phase 111 — 組み込み・特殊ターゲット

#### FR-605: Bare Metal / No-OS Support (ベアメタル・OS不使用サポート)

- **対象**: `packages/backend/runtime/src/`, `packages/cli/src/main.lisp`, `cl-cc.asd`
- **現状**: ランタイムはSBCLのOSサービス（mmap/mprotect等）に依存
- **内容**: `--target=x86-64-baremetal` / `--target=aarch64-none-elf` ターゲット追加。**POSIX依存ゼロ**: `write()`/`mmap()`/`pthread()`を使わない自己完結ランタイム。**ブートストラップコード**: x86-64向けMultiboot2ヘッダ + GDT/IDT初期化。**ページアロケータ**: 物理メモリマップからのヒープ初期化（E820/UEFI memory map）。`(declare (cl-cc:no-runtime-services))` で全OS依存をコンパイルエラー化
- **根拠**: Lisp Machine的な「OS上ではなくLisp上で動く」環境の構築。IoT/組み込みデバイスへのCL展開
- **難易度**: Very Hard

#### FR-606: No-Allocator Mode (割り当てゼロモード)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 全オブジェクトがGC管理ヒープに割り当てられる
- **内容**: `--no-allocator` フラグ: ヒープ割り当てを**コンパイルエラー**にする（FR-357 Allocation-free verificationの極端版）。スタック割り当て（FR-341エスケープ解析）・静的割り当て（グローバル変数）のみ許可。配列は`(make-static-array n)` で静的サイズのみ。固定サイズメモリプール（`(cl-cc:defpool point-pool point :size 1024)`）。RT-Linux / Zephyr RTOS / AUTOSAR向けコード生成
- **根拠**: 安全規格（ISO 26262 ASIL-D / IEC 61508 SIL4）でのGC使用禁止要件。自動運転・医療機器での使用
- **難易度**: Hard

#### FR-607: Partial Evaluation / Futamura Projections (部分評価・フタムラ投影)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: 段階的コンパイル（FR-431）はユーザー記述のmulti-stageプログラム。自動部分評価なし
- **内容**: `(cl-cc:specialize interpreter program)` でインタープリタをプログラムで**自動部分評価**し特化版を生成（Futamura第一投影）。静的値（コンパイル時定数）に関する全分岐・ループを展開。**混合計算解析（Binding-Time Analysis）**: 各変数が静的（コンパイル時既知）か動的かを自動分類。混合計算モナド（BTA）による変換。Jones-Gomard-Sestoft自動部分評価器の実装
- **根拠**: cl-ccのVM（インタープリタ）+ CLプログラムでFutamura第一投影を実証できる。「cl-ccで書いたLISPをcl-ccがネイティブコードにコンパイルする」という再帰的な美しさ
- **難易度**: Very Hard

#### FR-608: Deforestation / Stream Fusion (中間データ構造除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/frontend/expand/src/macros-sequence.lisp`
- **現状**: `(mapcar f (mapcar g lst))` が中間リストを生成してGCプレッシャー
- **内容**: **Deforestation**: `(mapcar f (mapcar g lst))` → `(mapcar (compose f g) lst)` への自動変換（中間リスト生成ゼロ）。**Stream fusion**: `(loop for x in list collect (f x))` の内部表現を`unfold/fold`型に変換してfusion可能化。`(reduce f (map g (filter p lst)))` の完全fusion。GHC Stream Fusion / Haskell rewrite rules / OCaml sequence fusion
- **根拠**: Lispの高階リスト操作は中間データ構造を大量生成。deforestationで`n`回の`mapcar`チェーンが1パスに統合でき、アロケーション数をn倍削減
- **難易度**: Hard

#### FR-609: Compiler-as-a-Library API (コンパイラAPIライブラリ)

- **対象**: `packages/umbrella/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **現状**: cl-ccはCLIのみ。プログラムから呼び出せるAPIなし（`compile-expression`は内部用）
- **内容**: **Public Compiler API**: `(cl-cc:compile-form form &key optimize-level target)` → `vm-program`。`(cl-cc:compile-file path &key ...)` → binary。`(cl-cc:parse-form str)` → `ast-node`。`(cl-cc:expand-macros form)` → expanded-form。**IR construction API**: `(cl-cc:make-ir-function name params body)` でプログラマティックなIR構築。LSP（FR-319）・IDEプラグイン（FR-398）・REPL（FR-312）のバックエンドとして統合。Clang libclang / LLVM C API / Roslyn API
- **根拠**: 外部ツールがcl-ccの解析・変換能力を利用できるようにする。静的解析ツール・フォーマッタ・リンタのすべてがこのAPIを使える
- **難易度**: Medium

#### FR-610: Adaptive Recompilation (適応的再コンパイル)

- **対象**: `src/jit/`, `packages/umbrella/pipeline/pipeline.lisp`
- **現状**: 各関数は一度だけTier-1→Tier-2にコンパイルされ、以後変化なし
- **内容**: **実行プロファイルの継続収集**: Tier-2でも型フィードバック（FR-559）を継続収集。型分布の**ドリフト検出**（初期プロファイルと現在の差異が閾値超過）。ドリフト検出時に新しいプロファイルに基づいてTier-2コードを**再コンパイル**（古いコードへのデオプトを経由）。long-running server processでのJVM C2最適化同様の動作。Graal/GraalVM Adaptive Compilation / JVM Adaptive Optimization
- **根拠**: 長時間実行プロセスでは実行パターンが変化する（スタートアップvs安定稼働）。初期プロファイルに基づくJITコードが後に非最適になる問題を解決
- **難易度**: Hard

#### FR-611: Persistent Data Structures (永続データ構造)

- **対象**: 新規`src/data/persistent.lisp`
- **現状**: CLの標準データ構造は全てmutable。不変データ構造の共有コピーなし
- **内容**: **Persistent Vector** (RRB-Tree: Bagwell 2012): `(cl-cc:pvec-assoc vec idx val)` がO(log₃₂n)でコピーオンライト。**Persistent Hash Map** (HAMT: Hash Array Mapped Trie): `(cl-cc:pmap-assoc map key val)`。**Structural sharing**: 変更されたパスのみ新ノードを作成し残りを共有。STM（FR-523）のトランザクション内でpersistent DSを使うと競合なしに並行更新可能。Clojure persistent collections / Scala Vector / Haskell Map
- **根拠**: 純粋関数型スタイル・STM・並行プログラミングの基盤。コピーオンライトより効率的（O(log n) vs O(n)）
- **難易度**: Hard

#### FR-612: Hash Consing (ハッシュコンシング)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: `(cons a b)` は常に新規ヒープオブジェクト。同一内容のconsが複数存在
- **内容**: **正準化テーブル**: `cons`/`list`/CLOSオブジェクト生成時に内容ハッシュで既存オブジェクトを検索。同一内容が存在すれば新規割り当てなしで既存を返す。`(eq (list 1 2 3) (list 1 2 3))` → `t`（通常は`nil`）。`(cl-cc:with-hash-consing ...)` スコープで有効化。`(declare (cl-cc:hashcons))` で個別構造体に適用。Lisp-1.5の歴史的機能 / Guile hash-consing / BDD実装
- **根拠**: コンパイラ内部の型表現・AST共有で大幅なメモリ削減効果。同一型シグネチャが何千も重複生成される最適化パスに有効
- **難易度**: Medium

---

### Phase 95 — 高度デバッグ・バイナリ最適化

#### FR-507: Time-Travel Debugging / Record-Replay (タイムトラベルデバッグ)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/cli/src/main.lisp`
- **現状**: VM 実行は前向きのみ。デバッガ（FR-319 LSP DAP）はステップ実行可能だが後退実行不可。セルフホスティング中のハイゼンバグを「発生直前まで巻き戻す」手段がない
- **内容**: VM の全命令実行を **execution log** に記録（命令インデックス・レジスタ差分・ヒープ書き込みアドレス）。`./cl-cc run --record foo.lisp` でログを `foo.clcc-trace` に保存。デバッガで `step-back` / `reverse-continue` コマンドでログを逆走させて過去の VM 状態を再現。Debug Adapter Protocol（DAP）の `reverseContinue` / `stepBack` リクエストに対応することで VSCode 等から透過的に利用可能
- **根拠**: Mozilla rr（Linux ptrace ベース）/ WinDbg TTD / UDB（Undo Software）。cl-cc 自身のセルフホスティングデバッグでの実用価値が高い。ハイゼンバグ解析の唯一の確実な手段
- **難易度**: Hard

#### FR-508: Post-Link Binary Layout Optimization (プロファイル駆動バイナリ再配置)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/cli/src/main.lisp`
- **現状**: FR-036（Hot/Cold レイアウト）と FR-186（関数並べ替え）は静的ヒューリスティック。実行プロファイルに基づくバイナリレイアウト最適化なし
- **内容**: `./cl-cc compile --instrument` で関数・基本ブロック単位の実行カウンタを埋め込んだバイナリ生成。`./cl-cc run foo` でプロファイルデータ（`foo.clcc-profile`）を収集。`./cl-cc optimize foo.clcc-profile foo` でホット関数を `.text.hot` 先頭に集約・コールドコードを `.text.cold` に分離してリンク。TLB / I-cache スラッシングを削減。BOLT（Meta 2018）/ Google Propeller と同等の手法
- **根拠**: BOLT paper: HHVM に適用して 7〜20% スループット向上。`./cl-cc selfhost` が現実的なプロファイルワークロードとなる
- **難易度**: Very Hard

---

### Phase 112 — コードサイズ・電力最適化

#### FR-615: Code Size Optimization Mode / -Os / -Oz (コードサイズ最適化モード)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/cli/src/main.lisp`
- **現状**: 最適化は速度優先。`-O2` 相当の最適化が唯一の選択肢。コードサイズを犠牲にする展開（ループアンローリング・インライン化・アウトライン抑制）の制御不可
- **内容**: `--optimize-for size` フラグを追加。`-Os`モード: インライン化コスト閾値を大幅に引き下げ（≤8命令のみ）、ループアンローリングを無効化、重複コードをアウトライン化（FR-042逆適用）。`-Oz`モード: さらに圧縮命令優先（x86-64の短いエンコーディング: `xor rax, rax` vs `mov rax, 0`）、ホットパス外の定数畳み込みを抑制。`(declare (cl-cc:optimize-for :size))` で関数単位適用
- **根拠**: 組み込みターゲット（FR-605 Bare Metal）や WASM バイナリサイズ削減に直結。LLVM -Os/-Oz / GCC -Os の標準的実践
- **難易度**: Medium

#### FR-616: Function Multi-Versioning / CPU Feature Dispatching (CPU機能ディスパッチ)

- **対象**: `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: コンパイル時に固定の命令セットを選択。実行時 CPU に依存した命令（AVX-512 / AVX2 / SSE4.2）の動的選択不可
- **内容**: `(defun foo (x) (declare (cl-cc:target-versions (:avx512 :avx2 :baseline))) ...)` でマルチバージョン関数を生成。コンパイラが3つのコード片を出力し、起動時 `cpuid` 結果でポインタを書き換える IFUNCリゾルバ（ELF `STT_GNU_IFUNC` / Mach-O `__TEXT.__stubs` resolver）を挿入。GNU `__attribute__((target_clones(...)))` / LLVM `@llvm.x86.cpuid` と同等
- **根拠**: FFT / BLAS カーネルで最大3倍の性能差。実行バイナリが対象 CPU のフル能力を引き出せる
- **難易度**: Hard

#### FR-617: Energy-Aware Compilation (エネルギー認識コンパイル)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/cli/src/main.lisp`
- **現状**: 最適化目標は実行速度のみ。エネルギー消費・電力効率の観点なし
- **内容**: `--optimize-for energy` モード追加。電力コストモデル（各命令の average power consumption 表を内蔵: 除算≫乗算≫加算）を参照した命令選択。高電力命令（64-bit IDIV, FP sqrt）の代替列への置換。P-coreとE-coreで異なるコードパスを生成するhybrid-CPU awareness。`(declare (cl-cc:power-budget 5))` で最大5ワット制約を宣言的に記述。ARM Neoverse N1 / Intel Hybrid architecture 対応
- **根拠**: データセンター電力コスト削減（10%省電力 ≈ 数億円/年）。モバイル・IoT バッテリー寿命最大化。Green Software Foundation の2026年標準要件
- **難易度**: Hard

#### FR-618: Dead Argument Elimination / DAE (デッド引数除去)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 関数引数は宣言された通りに全て渡す。呼び出し側で定数が渡される引数・内部で未使用の引数もレジスタ/スタック占有
- **内容**: LTO スコープ内（FR-040）で全呼び出しサイトを解析。引数 `x` が（a）全呼び出しサイトで同一定数 → クローン関数に定数埋め込み、（b）全サイトで未使用 → シグネチャから削除。`make-register-frame` のレジスタ保存コスト削減。LLVM `DeadArgumentElimination` パスと同等。スペシャライズ版関数は元関数のラッパーとしてABI維持
- **根拠**: LTOとIPCPの補完。コール規約のオーバーヘッド削減。selfhostingコードでの`(defun %compile-ast (ast env ... unused-debug-ctx))` のような内部関数に効果
- **難易度**: Medium

---

### Phase 113 — ポリヘドラル・自動並列化

#### FR-620: Polyhedral Model Optimization (多面体モデル最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: ループ最適化はループアンローリング・LICM（FR-031）・ループ交換（FR-360）の個別パスのみ。ネストしたループの連携最適化なし
- **内容**: `do`/`loop`マクロが生成するネストループをStaticControl Part (SCoP)として検出。整数線形計画法ベースの依存解析（ISL: Integer Set Library相当を純CLで実装）でデータ依存グラフを構築。Plutoアルゴリズムでループタイリング・ループ融合・ループ分散・ループスキューイングを自動適用。生成コードはFR-622 SLPと連携してSIMD化
- **根拠**: 行列乗算・画像処理・数値シミュレーションで10〜100倍の性能向上。LLVM Polly / GCC Graphite / Pluto の手法。FFT実装の自動最適化に直結
- **難易度**: Very Hard

#### FR-621: Auto-Parallelization (自動並列化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: `cl-cc`のループは全てシリアル実行。FR-576（Work-Stealing Scheduler）は並行タスクAPIを提供するが自動適用なし
- **内容**: SCoP解析（FR-620）の依存グラフが並列化可能な外側ループを特定。ループ本体をFR-576のワークスティーリングタスクに自動変換。`(declare (cl-cc:parallel))` アノテーションで手動ヒント。依存違反時はコンパイル時警告。ループ繰り返し数が閾値未満（<1024）の場合はシリアル実行にフォールバック。OpenMP `#pragma omp parallel for` の意味論と同等
- **根拠**: マルチコア CPU を自動活用。並列化対応のループコードをユーザーが明示的に書く必要なし
- **難易度**: Very Hard

#### FR-622: Superword-Level Parallelism / SLP (スーパーワードレベル並列性)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: SIMD自動ベクトル化（FR-035）はループ全体を対象。ループ外の隣接スカラー演算のパック化なし
- **内容**: 基本ブロック内の独立スカラー演算列（`(+ a0 b0)` `(+ a1 b1)` `(+ a2 b2)` `(+ a3 b3)` → `vaddps xmm0, ...`）を検出し128/256bit SIMD命令にパック。データ型整合・アライメント確認・レジスタ割り当て統合。LLVM SLPVectorizer / GCC SLP と同等。FR-035 LoopVecで対応できない非ループコード（構造体フィールド初期化など）に効果
- **根拠**: 3D座標演算・カラー処理・CLMS行列要素初期化など、展開後の隣接演算に広く適用可能
- **難易度**: Hard

#### FR-623: Loop Fusion (ループ融合)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: 複数の独立したループが同一配列を順次処理する場合、それぞれ別個にコンパイル。キャッシュ再利用機会を逃す
- **内容**: 連続する2つのループが同一イテレーション空間かつデータ依存がない場合、ループ本体を統合（`(dotimes (i n) (setf (aref a i) ...)) (dotimes (i n) (setf (aref b i) ...))` → 1ループ）。FR-620 SCoP依存グラフで合法性確認。融合後の中間配列が消滅する場合はFR-608 Deforestationと協調。`map`/`mapcar`連鎖の融合（stream fusion）も対象
- **根拠**: キャッシュラインの再利用でL1ヒット率向上。ループオーバーヘッド（カウンタ更新・分岐）削減。GHC deforestation / Haskell stream-fusion の Lisp版
- **難易度**: Medium

---

### Phase 114 — マルチレベルIR・形式検証

#### FR-626: Multi-Level IR / MLIR-Style Dialect Lowering (多段階IR・段階的降下)

- **対象**: `packages/foundation/mir/src/mir.lisp`, `packages/foundation/mir/src/target.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: コンパイルパイプラインはAST → VM instructions → x86-64の2段階変換。中間IRが固定的で新ターゲット追加時の再利用困難
- **内容**: MIR層を拡張して「Dialect」概念を導入。`cl-cc.affine` dialect（ループ/アフィン演算）→ `cl-cc.arith` dialect（スカラー演算）→ `cl-cc.llvm` / `cl-cc.x86` dialectの段階的降下パイプライン。各dialect間の変換パターンを`define-dialect-conversion`マクロで宣言。MLIR（LLVM 2020）のConversionPatternRewriter と同等の設計。WASM・GPU・AArch64ターゲットが同一フロントエンドから派生可能
- **根拠**: MLIR paper（PLDI 2020）: 言語固有最適化を適切な抽象レベルで適用可能。ターゲット追加コストを指数→線形に削減
- **難易度**: Very Hard

#### FR-627: Formal Verification Integration / Coq-Lean Extraction (形式検証統合)

- **対象**: `packages/foundation/type/src/`, `packages/engine/compile/src/codegen.lisp`, `packages/cli/src/main.lisp`
- **現状**: 型システム（FR-type系）は型安全性を保証するが、プログラムの機能的正しさは検証不可
- **内容**: `(defun foo (x) (declare (cl-cc:spec (-> (and integer (>= 0)) integer))) ...)` で前後条件を宣言。コンパイラが仕様をSMTソルバー（FR-246 Z3統合）に送出して検証。検証失敗時はコンパイルエラー。`--emit-lean` フラグでLean 4証明ターゲットに変換（F\*スタイル）。副作用追跡（FR-244 Effect System）と統合してIOを型レベルで分離
- **根拠**: CompCert（INRIA）: 検証済みCコンパイラ。セキュリティクリティカルコード（暗号・OS kernel）の信頼性向上。cl-ccコンパイラ自身の健全性検証
- **難易度**: Very Hard

#### FR-628: Proof-Carrying Code / PCC (証明付きコード)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/engine/vm/src/vm-execute.lisp`, `packages/foundation/type/src/`
- **現状**: 生成バイナリの安全性は動的チェック（境界検査・型タグ）に依存。ロード時の型安全性証明なし
- **内容**: バイナリに型証明（proof term）を埋め込む。VM実行前に証明検証器（proof checker）が証明を検査し、メモリ安全性・型安全性・制御フロー整合性を静的に確認。`--emit-pcc` フラグでPCC付きバイナリ生成。Foundational PCC（Appel & Felten 1999）/ TAL（Typed Assembly Language）の手法。信頼ベースを最小化（証明検証器のみ信頼）
- **根拠**: 動的チェックオーバーヘッドをゼロに近づけながら安全性を維持。プラグインシステムで未検証コードのロードを拒否するセキュリティモデル
- **難易度**: Very Hard

#### FR-629: Certified Compilation / Bisimulation Proofs (認証済みコンパイル)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/compile/src/cps.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: コンパイル変換の正しさはテストで担保（4322テスト）。変換の意味論的等価性の形式的証明なし
- **内容**: 主要変換（CPS変換・クロージャ変換・レジスタ割り当て）に対してシミュレーション関係を定義。`--verify-transform` モードで変換前後の意味論をランダムプログラムで差分テスト（FR-453 Differential Testing の強化版）。CompCert の verification methodology に準拠。将来的にCoq/Leanで変換正当性証明を形式化
- **根拠**: コンパイラのバグが最も発見困難なバグ（最適化による意味変化）。認証済みバックエンドはセキュリティコンパイル保証の基盤
- **難易度**: Very Hard

---

### Phase 115 — FFI・クロスコンパイル

#### FR-632: FFI Binding Generation / Bindgen (FFIバインディング自動生成)

- **対象**: `packages/cli/src/main.lisp`, 新規 `src/ffi/bindgen.lisp`
- **現状**: C関数呼び出しは手動で`(cl-cc:foreign-call "printf" :int :string)` と記述。Cヘッダーからの自動生成なし
- **内容**: `./cl-cc bindgen foo.h` でCヘッダーを解析し、cl-cc外部関数宣言を自動生成。C型→cl-cc型マッピング（`int*` → `(cl-cc:ptr cl-cc:int32)`）。`struct`定義 → `defstruct`生成。`enum` → `defconstant`群生成。libclang / tree-sitter-cで構文解析。`--with-header /usr/include/stdio.h` でシステムヘッダー対応。Rust bindgen / CFFI (Common Lisp) と同等
- **根拠**: POSIX API・OpenSSL・GTK等のライブラリを手動FFI記述なしに利用可能。エコシステム拡張の加速
- **難易度**: Hard

#### FR-633: Cross-Compilation Toolchain (クロスコンパイルツールチェーン)

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/`, `packages/backend/binary/src/`
- **現状**: ホスト環境（macOS/Linux x86-64）向けにのみコンパイル可能。`--target` フラグはアーキテクチャ切り替えのみ
- **内容**: `--target triple` 形式（`aarch64-linux-musl`, `x86_64-windows-gnu`, `riscv64-linux-gnu`）で完全なクロスコンパイルを実現。Sysroot管理（`--sysroot /path/to/arm-sysroot`）。クロス用リンカスクリプト（ELF/PE/Mach-O）の自動選択。標準ライブラリのターゲット向けビルド済みアーカイブ配布。Nix flake との統合（`devenv.nix` の `cross.aarch64-linux`）
- **根拠**: Raspberry Pi・組み込み Linux・Windows バイナリを開発機から直接生成。CI/CD での多ターゲットリリース自動化
- **難易度**: Hard

#### FR-634: Custom Calling Conventions (カスタム呼び出し規約)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: 全関数が統一的なvm-call規約（レジスタ保存・引数スタック）を使用。呼び出しオーバーヘッド削減の余地あり
- **内容**: `(defun hot-fn (a b) (declare (cl-cc:calling-convention :register-only)) ...)` でカスタム規約指定。`:register-only`: 全引数をレジスタ渡し（R0-R7）、呼び出し側保存なし（leaf関数前提）。`:tailcall-optimized`: TCO専用規約（継続渡しスタイル）。`:interrupt-handler`: 全レジスタ保存（ISRルーティン用）。LLVM `cc` attribute / GCC `__attribute__((regparm))` 相当。FR-618 DAEと協調
- **根拠**: JITコンパイルされたホットパスで呼び出しオーバーヘッドを90%削減可能。コンパイラ内部の再帰関数（CPS変換・コード生成）に即座に適用可能
- **難易度**: Hard

#### FR-635: COMDAT Deduplication (COMDATセクション重複除去)

- **対象**: `packages/backend/binary/src/macho.lisp`, `packages/foundation/mir/src/target.lisp`
- **現状**: テンプレート/generic関数の複数インスタンスが別々のコンパイル単位に重複して存在する場合、リンク時に全コピーを保持
- **内容**: 関数・データを COMDAT グループ（ELF `SHF_GROUP` / Mach-O `S_ATTR_NO_DEAD_STRIP` + `.weak_definition`）に配置。リンカが同名 COMDAT グループを1コピーに統合。`(defun foo (x) (declare (cl-cc:comdat :any)) ...)` で明示指定。ジェネリック関数のspecializer組み合わせ爆発を防止。FR-582 ICFの補完（ICFは内容一致、COMDATは名前一致）
- **根拠**: C++テンプレートと同様に、cl-ccのgeneric function specializer重複によるバイナリ肥大化を防止
- **難易度**: Medium

---

### Phase 116 — 自動微分・ハードウェア並行

#### FR-638: Automatic Differentiation / AD (自動微分)

- **対象**: 新規 `src/ad/forward.lisp`, `src/ad/reverse.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 数値微分は手動実装のみ。機械学習・最適化アルゴリズムへの対応なし
- **内容**: `(cl-cc:grad f)` でスカラー関数`f`の微分関数を返す。**前向きモード（ForwardAD）**: 双数（dual numbers `(value . derivative)`）を使ったオペレータオーバーロード。**逆向きモード（ReverseAD）**: 計算グラフをテープに記録しバックプロパゲーション。`(cl-cc:jacobian f)` でヤコビアン行列計算。`(declare (cl-cc:differentiable))` でAD対応関数を宣言。JAX-style transformations: `grad`, `jvp`, `vjp`, `hessian`
- **根拠**: 機械学習ライブラリ（FR-597 ML-Guided Inlining）の内部実装基盤。科学計算・最適化ソルバーへの応用。Enzyme（LLVM AD）/ JAX / Zygote（Julia）の手法
- **難易度**: Hard

#### FR-639: Hardware Transactional Memory / HTM (ハードウェアトランザクショナルメモリ)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/vm/src/conditions.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: FR-165（STM Software Transactional Memory）は純ソフトウェア実装。Intel TSX / ARM TME ハードウェア命令未活用
- **内容**: `(cl-cc:with-htm ...)` マクロがhardware transactionを試行。Intel `XBEGIN`/`XEND`/`XABORT` (RTM) または ARM `TSTART`/`TCOMMIT`/`TCANCEL` を発行。中断時はFR-165 STMソフトウェア実装にフォールバック。競合率が高い場合は自動的にロック実装に降格。トランザクション中のIO・syscall・例外はコンパイル時に禁止
- **根拠**: ロックフリーデータ構造（FR-163）より高いスループット。Intel TSX搭載CPU（Haswell以降）で投機的並行性を直接活用
- **難易度**: Hard

#### FR-640: NUMA-Aware Memory Allocation (NUMA対応メモリ割り当て)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/runtime/src/gc.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: ヒープ割り当ては単一アリーナ（FR-228 arena allocator）。NUMA topologyを無視した割り当てでリモートメモリアクセスが発生
- **内容**: 起動時に `/sys/devices/system/node/` (Linux) / IOKit NUMA API (macOS) でNUMAトポロジーを取得。スレッド（FR-576 work-stealing）がローカルNUMAノードのアリーナから優先的に割り当て。GCスキャンもNUMAノード単位で並列化。`numactl` / `mbind` / `set_mempolicy` syscall を活用。`(declare (cl-cc:numa-local))` で関数内割り当てをローカルノードに固定
- **根拠**: 64コア以上のサーバーでNUMA効果が顕著（リモートアクセス: 2〜4倍遅延）。大規模コンパイルジョブでのメモリスループット改善
- **難易度**: Hard

#### FR-641: Transparent Huge Pages / THP (透明な大ページ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: 標準 4KB ページでメモリ割り当て。JITコードおよびヒープが多数のTLBエントリを消費
- **内容**: JITコードバッファ（FR-060）と大規模ヒープアリーナ（>2MB）を `mmap(MAP_HUGETLB)` / `madvise(MADV_HUGEPAGE)` で 2MB huge page に配置。macOS では `MAP_JIT | MAP_SHARED` + `vm_map` の適切なアライメント。TLBミス率をperfカウンタで計測し、big-enough ヒープのみに適用（小アリーナはオーバーヘッドが逆効果）。`--huge-pages` フラグで制御
- **根拠**: JITコードとヒープで TLB miss が最も多い。MongoDB/JVM で THP 有効化により 10-15% スループット向上の実績
- **難易度**: Medium

---

### Phase 117 — 新興ターゲット

#### FR-644: WASM GC Proposal Support (WASMガベージコレクション提案対応)

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/foundation/mir/src/target.lisp`
- **現状**: FR-049（WASM backend）は線形メモリモデルのみ。WASM GC提案（struct/array型）未対応
- **内容**: WASM GC提案（W3C 2024勧告）の `struct.new`, `array.new`, `ref.cast`, `extern.convert_any` 命令を活用。cl-ccのCLOSオブジェクトをWASM `struct` 型にマッピング。リスト/配列を WASM `array` 型に変換。ブラウザ内蔵GCに管理を委譲（cl-ccのカスタムGCが不要）。V8/SpiderMonkey/JavaScriptCore のWASM GC実装と互換。`--target wasm-gc` フラグで有効化
- **根拠**: WASM GCにより生成バイナリサイズが30-50%削減（手製メモリ管理コード不要）。ブラウザでの cl-cc プログラム実行の現実化
- **難易度**: Hard

#### FR-645: GPU Compute Kernel Compilation (GPUコンピュートカーネルコンパイル)

- **対象**: 新規 `packages/backend/emit/src/gpu-kernel.lisp`, `packages/foundation/mir/src/target.lisp`
- **現状**: FR-053（SPIR-V backend）は基本的な shader 生成。CUDA/ROCm/Metal Compute の高水準抽象なし
- **内容**: `(cl-cc:defkernel matrix-mul ((a :buffer) (b :buffer) (c :buffer :write)) ...)` でGPUカーネルを定義。コンパイラがSPIR-V（Vulkan Compute）/ PTX（NVIDIA CUDA）/ AIR（Apple Metal）/ ROCm LLVM IRへのコード生成。自動的なthread block / workgroup サイズ選択。スカラー演算のwarp-levelベクトル化。`cl-cc:launch-kernel` でホストからディスパッチ。oneAPI Level Zero / Vulkan Compute API との統合
- **根拠**: AI推論・科学計算・コンパイラ内部の並列最適化パスをGPUオフロード。M1/M2 Mac のunified memory architectureでとくに効果的
- **難易度**: Very Hard

#### FR-646: FPGA High-Level Synthesis / HLS (FPGA高水準合成)

- **対象**: 新規 `packages/backend/emit/src/fpga-hls.lisp`, `packages/foundation/mir/src/target.lisp`
- **現状**: ターゲットはCPU/GPU/WASM。FPGA向け合成パスなし
- **内容**: `(defun foo (a b) (declare (cl-cc:target :fpga-hls)) ...)` でFPGA合成対象を指定。コンパイラがCIRCT（Circuit IR Compiler Tools）互換のHLS IRを出力。パイプライン化・II（Initiation Interval）最適化・リソース共有の制御。制御フロー（if/case）をMux/FSMに変換。`(declare (cl-cc:ii 1))` でII=1（毎サイクル新入力受付）を指定。Vivado HLS / Intel HLS Compiler / Bambu と同等
- **根拠**: FPGA上での高性能DSP・ネットワーク処理・暗号化アクセラレータを高水準言語で記述可能
- **難易度**: Very Hard

#### FR-647: NPU / ML Accelerator Code Generation (NPU/MLアクセラレータコード生成)

- **対象**: 新規 `packages/backend/emit/src/npu-codegen.lisp`, `packages/foundation/mir/src/target.lisp`
- **現状**: FR-645でGPUカーネル対応。専用ML推論アクセラレータ（Apple Neural Engine / Google TPU / Qualcomm HTP）未対応
- **内容**: `(cl-cc:defnn-op conv2d ...)` でニューラルネットワーク演算を宣言的に定義。コンパイラがMLIRの`linalg`/`tosa`/`nnef` dialectを経由してTFLite FlatBuffer / CoreML Model / QNN SDK 形式に降下。フュージョン（Conv+BN+ReLUを1カーネルに）・量子化（fp32→int8）・テンソルレイアウト変換（NCHW↔NHWC）を自動適用。Apple ANE（Core ML）/ Google Edge TPU / Hexagon DSP 対応
- **根拠**: エッジAI推論（FR-597 ML-Guided Inlining の実行デバイス）の電力効率が GPU比10〜100倍。2026年以降の主要デプロイ先
- **難易度**: Very Hard

---

### Phase 118 — パーサ高度化・多言語

#### FR-650: Incremental Parsing / Error-Resilient Parser (インクリメンタルパーシング)

- **対象**: `packages/frontend/parse/src/cl/parser.lisp`, `packages/frontend/parse/src/lexer.lisp`
- **現状**: ソース全体を再解析。1文字変更でもAST全再構築。LSP補完（FR-070）でのレスポンス遅延の原因
- **内容**: Tree-sitter スタイルのインクリメンタルパーシング。変更されたソース範囲のみを再解析し既存ASTと差分マージ。`edit-tree(old-tree, start-byte, old-end-byte, new-end-byte)` APIを提供。変更範囲を `(changed-ranges old new)` で取得し、LSPサーバーがインクリメンタルにセマンティクス更新。タイピング中の未完成コードでも部分ASTを生成（エラーノード`ast-error`で表現）
- **根拠**: Tree-sitter（GitHub 2018）: エディタ組み込みパーシングの標準。LSPレスポンスを O(ファイルサイズ) から O(変更サイズ) に削減
- **難易度**: Hard

#### FR-651: Error Recovery in Parsing (パーシングエラー回復)

- **対象**: `packages/frontend/parse/src/cl/parser.lisp`, `packages/frontend/parse/src/lexer.lisp`
- **現状**: パース中の構文エラーで解析中断。以降のエラーが全て報告されない（最初のエラーで止まる）
- **内容**: **パニックモード回復**: エラー発生後、同期トークン（`(`, `)`, `defun`, `defvar`）まで入力を読み飛ばして解析継続。**エラープロダクション**: `(if cond then)` のような不完全フォームをエラーASTノードとして保持し後続解析継続。`(cl-cc:parse-resilient "...")` がエラーリストと部分ASTの両方を返す。LSP（FR-070）との統合でエディタが複数エラーを一括表示
- **根拠**: `gcc`/`clang` は10個以上のエラーを同時報告。cl-ccの現在の「最初のエラーで停止」動作は開発体験を著しく損なう
- **難易度**: Medium

#### FR-652: Polyglot Compilation / Multi-Language Interop (多言語コンパイル)

- **対象**: `packages/cli/src/main.lisp`, `src/ffi/`, 新規 `src/polyglot/`
- **現状**: cl-ccソースのみ処理。他言語との相互運用はCFFI手動記述のみ
- **内容**: `./cl-cc build --polyglot project.toml` で混合言語プロジェクトのビルド。サポート言語: C（clang経由）、Python（CPython C-API）、Rust（cargo build → `.a` リンク）。各言語の型 → cl-cc型の自動変換スキーム。GraalVM polyglot / Nix mkDerivation との統合。`(cl-cc:import-from :python "numpy" :as numpy)` のような高水準インポート構文
- **根拠**: 実世界のシステムは単一言語で構成されない。科学計算（Python/NumPy）・システムライブラリ（C/Rust）との協調が必須
- **難易度**: Hard

#### FR-653: Lazy Evaluation / Call-by-Need (遅延評価・必要時呼び出し)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/frontend/expand/src/expander.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: 全式が正格評価（call-by-value）。無限リスト・遅延シーケンスの表現不可
- **内容**: `(cl-cc:lazy expr)` でサンク（thunk）生成。初回 `(cl-cc:force thunk)` でeval・結果をメモ化（以後同値返却）。`(cl-cc:delay-seq ...)` で無限遅延シーケンス生成。`define-lazy-syntax` マクロで`and`/`or`/`if`の短絡評価をlazy semanticsで再定義可能。コンパイラがサンクを値タグで表現しforceで自動透過。Haskell thunks / Scheme `delay`/`force` / SRFI-41 と同等
- **根拠**: 無限リスト（素数の無限列・フィボナッチ数列）の自然な表現。短絡評価マクロの透明な実装基盤
- **難易度**: Medium

---

### Phase 119 — 契約・リフレクション・合成

#### FR-656: Contract Programming / Design by Contract (契約プログラミング)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/foundation/type/src/`
- **現状**: `assert`マクロは実行時検査のみ。前提条件・事後条件・不変条件の宣言的記述と静的検証なし
- **内容**: `(defun divide (x y) (require (not (zerop y)) "y≠0") (ensure (numberp result) "result is number") ...)` でEiffel/Racket-styleの契約を記述。**静的検証**: コンパイル時にSMTソルバー（FR-246）で検証試行。**動的検証**: `--contracts :check` で実行時検査有効化、`--contracts :none` で本番無効化。クラス不変条件: `(definvariant stack-class (>= (length items) 0))`。Racket Contracts / CLIM preconditions / Cofoja（Java）と同等
- **根拠**: cl-ccコンパイラ内部の不変条件（CPS変換後の継続ノード構造など）を宣言的に検証可能。バグの早期発見
- **難易度**: Medium

#### FR-657: Runtime Reflection API (実行時リフレクションAPI)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/vm/src/vm-clos.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `class-of` / `type-of` は基本的な型情報のみ返す。メソッド一覧・スロット名・ソース位置の実行時取得なし
- **内容**: `(cl-cc:reflect:methods obj)` でオブジェクトの全適用可能メソッドを返す。`(cl-cc:reflect:slots class)` でスロット名/型/初期値のリスト。`(cl-cc:reflect:source-location fn)` でソースファイル・行番号。`(cl-cc:reflect:disassemble fn)` でVM命令列を返す（デバッグ用）。`(cl-cc:reflect:type fn)` で推論済み型シグネチャ取得。コンパイラが`--emit-reflection-metadata`フラグでメタデータセクションをバイナリに埋め込み
- **根拠**: REPL・デバッガ・テストフレームワーク・シリアライザが内省APIに依存。現在のFiveAMテストがメソッドを動的発見できない制約の解消
- **難易度**: Medium

#### FR-658: Program Synthesis / CEGIS (プログラム合成・反例誘導合成)

- **対象**: `packages/cli/src/main.lisp`, 新規 `src/synthesis/cegis.lisp`
- **現状**: プログラムは手動記述のみ。仕様から実装を自動導出する機能なし
- **内容**: `(cl-cc:synthesize (fn (integer) integer) :spec (lambda (n result) (= result (* n n))))` で仕様から関数を合成。**CEGIS**: 候補プログラムを生成 → SMTで反例確認 → 反例を制約追加 → 再合成のループ。スケッチ（`cl-cc:??` でホール）: `(+ x (cl-cc:?? integer))` の `??` を合成。Sketching（Armando Solar-Lezama 2006）/ Rosette（Racket）/ SyGuS 競技形式と同等。合成スコープは純関数（副作用なし）に限定
- **根拠**: テストケース（入出力例）から関数実装を自動生成。小規模ユーティリティ関数の自動記述
- **難易度**: Very Hard

#### FR-659: Copy-on-Write Semantics (コピーオンライトセマンティクス)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/vm/src/list.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: `copy-list` / `copy-seq` は常に即時コピー。大規模データ構造の不要なコピーでメモリ使用量増大
- **内容**: `(cl-cc:cow-copy obj)` でCOW参照を作成（実際のコピーは書き込み時まで遅延）。内部的に参照カウント + `is-shared` フラグで管理。配列/文字列への書き込み時に `copy-on-write-check` を自動挿入（コンパイラが検出）。`(declare (cl-cc:cow))` スコープで暗黙的COW有効化。FR-611（Persistent Data Structures）との相違: COWはミュータブル操作を透過的に最適化、PDSは不変性を保証
- **根拠**: 文字列処理・大規模リストのスライシング・フォークベースの並行処理でメモリ効率が大幅改善。Unix fork の `MAP_PRIVATE` と同等のセマンティクス
- **難易度**: Medium

---

### Phase 120 — ビルド信頼性・診断

#### FR-662: Reproducible Builds (再現可能ビルド)

- **対象**: `packages/cli/src/main.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: 生成バイナリにビルド時刻・ホスト情報が埋め込まれる。同一ソースから異なるバイナリが生成されサプライチェーン検証不可
- **内容**: `--reproducible` フラグで完全決定論的ビルドを保証。タイムスタンプを固定値（`SOURCE_DATE_EPOCH`環境変数）に置換。ハッシュマップ・セットの反復順序を決定論的に固定（ソートキーを使用）。デバッグ情報のパスを `--remap-path-prefix` で正規化。コンパイル並列処理の順序をトポロジカルソートで固定。Reproducible Builds project（Debian/NixOS）との互換性。`./cl-cc build --check-reproducible` で2回ビルドしてSHA256比較
- **根拠**: NixOS/Guix のコンテンツアドレス型ビルドシステムとの統合必須。サプライチェーン攻撃対策（SLSA Level 3要件）
- **難易度**: Medium

#### FR-663: Build System Integration (ビルドシステム統合)

- **対象**: `packages/cli/src/main.lisp`, 新規 `src/build/integration.lisp`
- **現状**: `cl-cc.asd`（ASDF）のみ。CMake/Meson/Bazel/Nix との統合なし
- **内容**: **CMake**: `FindCL-CC.cmake` モジュール提供、`add_cl_cc_library(name SOURCES ...)` CMake関数。**Meson**: `cl_cc.dependency()` wrap。**Bazel**: `cl_cc_binary` / `cl_cc_library` Starlark ルール。**Nix**: `buildCLCCPackage` nixpkgsヘルパー（devenv.nix統合）。**Buck2**: `cl_cc_library()` target。全ビルドシステムが`compile_commands.json`（FR-574）を出力し、LSP/clangd-compatible
- **根拠**: C/C++プロジェクトへのcl-ccコンポーネント組み込み。企業規模のモノレポ（Bazel/Buck2）でのcl-cc利用
- **難易度**: Hard

#### FR-664: Compiler Directives / Pragma System (コンパイラディレクティブシステム)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/frontend/parse/src/cl/parser.lisp`
- **現状**: `(declare ...)` フォームは型宣言と最適化ヒントに限定。汎用コンパイラ制御ディレクティブなし
- **内容**: `(cl-cc:pragma ...)` マクロで汎用ディレクティブシステム実装。`(cl-cc:pragma :assume (> x 0))` で前提条件をコンパイラに伝達（`__builtin_assume` 相当）。`(cl-cc:pragma :no-inline fn)` で特定関数のインライン化抑制。`(cl-cc:pragma :restrict ptr)` でポインタエイリアスなしを宣言（C `restrict` 相当）。`(cl-cc:pragma :loop-count 1000)` でループ回数ヒント。`(cl-cc:pragma :cold)` でコールドコードマーク。未知プラグマは警告のみで無視（前方互換性）
- **根拠**: `__builtin_expect` / `__builtin_unreachable` / `__builtin_assume_aligned` の宣言的Lisp版。最適化ヒントをソースコードに埋め込む標準的手段
- **難易度**: Easy

#### FR-665: Inlining Cost Model Tuning (インライン化コストモデルチューニング)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: インライン化決定はヒューリスティック（命令数閾値）のみ。呼び出し頻度・コードサイズ増加・レジスタ圧力の総合評価なし
- **内容**: 多因子インライン化コストモデル: `score = benefit(call_savings + specialization_gain + loop_invariant_removal) - cost(code_size_increase + register_pressure + compilation_time)`。JITプロファイル（FR-559 Type Feedback）による呼び出し頻度重み付け。再帰関数のインライン展開回数上限（デフォルト2）。`--inline-budget N` でコードサイズ予算制約（GCC `-finline-limit` 相当）。インライン化決定ログ（`--report-inlining`）でチューニング支援。FR-597 ML-Guided Inliningとの統合
- **根拠**: 過剰インライン化はI-cacheスラッシング・コンパイル時間増大を招く。適切なコストモデルがコンパイラ性能の鍵
- **難易度**: Medium

---

### Phase 121 — スカラー置換・例外・コンパイル時評価

#### FR-668: Scalar Replacement of Aggregates / SROA (集合体のスカラー置換)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 構造体・CLOS インスタンスはヒープオブジェクトとして生成。エスケープしない局所オブジェクトのスタック/レジスタ分解なし
- **内容**: FR-231（Escape Analysis）でスタックエスケープしないオブジェクトを特定後、SROAを適用。`(let ((p (make-point :x 1 :y 2))) (+ (point-x p) (point-y p)))` → `(let ((p.x 1) (p.y 2)) (+ p.x p.y))`。構造体全体がレジスタ変数群に分解され、ヒープ割り当てゼロ。CLOSインスタンスのスロット配列もSROA対象（ただしメソッドディスパッチが静的に解決できる場合のみ）。LLVM `ScalarReplAggregates` / GCC `SRA` パスと同等
- **根拠**: 内部的にStructを多用するコンパイラパス（AST ノード、CPS フレーム）でGCプレッシャーを大幅削減。ホットパスのアロケーションを0に近づける最重要最適化の一つ
- **難易度**: Hard

#### FR-669: Zero-Cost Exceptions / EH Table Optimization (ゼロコスト例外処理)

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/binary/src/macho.lisp`
- **現状**: `handler-case` / `handler-bind` は実行時セットアップコスト（スタックフレーム登録）あり。例外がスローされない正常パスでも毎回コストを支払う
- **内容**: **ゼロコスト例外テーブル方式**: 正常パスのコードからハンドラ登録コードを除去し、EH（Exception Handling）テーブルをバイナリ別セクション（`.eh_frame` / Mach-O `__DATA.__eh_frame`）に配置。例外スロー時のみDWARF unwind tableをウォークしてハンドラを発見。正常パスのコストはゼロ（メモリアクセスなし）。`_Unwind_RaiseException` / libunwind との統合。C++ ABI互換のLSDA（Language Specific Data Area）フォーマット
- **根拠**: Itanium C++ ABI EH: 正常パスに実行コストなし（ただしコードサイズは増加）。cl-ccのcondition systemのオーバーヘッドを実測で確認・排除
- **難易度**: Very Hard

#### FR-670: Compile-Time Evaluation / Constexpr (コンパイル時評価)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 定数畳み込み（FR-002）は基本算術演算のみ。任意の純粋関数のコンパイル時実行なし
- **内容**: `(cl-cc:eval-at-compile-time expr)` で純粋式をコンパイル時に完全評価し結果をリテラルに置換。評価可能条件: 副作用なし・全引数がコンパイル時定数・IOなし・実行時間上限あり（デフォルト1秒）。`(defconstant +fib-10+ (cl-cc:eval-at-compile-time (fib 10)))` → `55`。C++17 `constexpr` / Zig `comptime` / D `enum` の手法。コンパイラが内部で`our-eval`をサンドボックス実行。再帰深さ・アロケーション量に上限
- **根拠**: ルックアップテーブルの事前計算・フォーマット文字列解析・暗号定数の生成をコンパイル時に完了。実行時の計算コストゼロ
- **難易度**: Medium

#### FR-671: Superoptimization / Peephole Superoptimizer (スーパー最適化)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: ピープホール最適化はプリセットパターンの照合。探索ベースの最適命令列発見なし
- **内容**: 小さな命令ウィンドウ（3〜6命令）に対して等価な短い命令列を **完全列挙+SMT検証** で探索。`(vm-move :r0 :r1) (vm-add :r0 5)` より短い等価列があればSMTで確認して置換。Souper（LLVM IR superoptimizer）/ Stochastic Superoptimization / STOKE の手法。探索はオフラインで行い結果をルールテーブルにコンパイル済みとして内蔵。`--superoptimize-window 5` で探索ウィンドウ幅を指定
- **根拠**: 人間が思いつかない命令列の短縮（`x * 15 = (x << 4) - x` など）。ホット関数に集中適用で実測性能向上
- **難易度**: Hard

---

### Phase 122 — メモリ表現最適化

#### FR-674: NaN Boxing / Pointer Tagging (NaNボクシング・ポインタタグ付け)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/vm-execute.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 値は個別の型タグ付きstructとして表現。`vm-integer` / `vm-float` / `vm-cons` それぞれがヒープオブジェクト
- **内容**: **NaN Boxing**: IEEE 754 倍精度浮動小数点の NaN ビットパターン（指数部全1）を利用し、64ビット値に型タグ+ペイロードを圧縮。`0x0000...` = fixnum（48bit符号付き整数）、`0x7FFF0001...` = cons pointer、`0x7FFF0002...` = symbol pointer、NaN以外の全bitパターン = double。JavaScriptエンジン（V8 / SpiderMonkey / JavaScriptCore）標準手法。OR **Pointer Tagging**: 最下位2-3ビットを型タグに使用（アライメント保証のため常に0）。fixnum=0b01、char=0b10、heap-ptr=0b00
- **根拠**: Boxed値のメモリ使用量を50-75%削減。型チェックがビットマスク1命令。V8のNaN-boxing実装でJSベンチマーク30%高速化の実績
- **難易度**: Very Hard

#### FR-675: Pointer Compression (ポインタ圧縮)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: ヒープポインタは64ビット絶対アドレス。8GBヒープのポインタが全て64ビットを消費
- **内容**: ヒープを4GB以下の連続領域に固定（`mmap` の `MAP_FIXED_NOREPLACE`）。ヒープ内ポインタを32ビットオフセット（base relative）で表現。ポインタderef時に `base + compressed_ptr` で復元（1命令）。V8 Pointer Compression（2020）: V8ヒープのメモリ使用量40%削減を実現。`--max-heap 4g` 制約下で有効化。GC中の移動時はオフセット更新のみ
- **根拠**: ポインタサイズ半減によりキャッシュライン当たりの参照数2倍。ヒープスキャン速度向上。大規模Lispプログラムのメモリ効率改善
- **難易度**: Hard

#### FR-676: Thread-Local Allocation Buffers / TLAB (スレッドローカル割り当てバッファ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: ヒープ割り当てはグローバルアリーナへのatomic CAS。高並行時にキャッシュライン競合が発生
- **内容**: 各ワーカースレッド（FR-576 Work-Stealing）が専用のアロケーションバッファ（TLAB: 64-256KB）を保持。バッファ内割り当ては bump-pointer のみ（同期不要、2命令）。バッファ満杯時のみグローバルGCアリーナからrefill（ロック取得）。JVM TLAB / .NET SOH thread-local allocation の手法。`--tlab-size` で調整可能
- **根拠**: マルチスレッド割り当てのスケーラビリティをコア数に比例させる。JVM の実測: スレッド数増加時の割り当てスループット10倍向上
- **難易度**: Medium

#### FR-677: Object Pooling / Free-List Allocator (オブジェクトプーリング・フリーリストアロケータ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: オブジェクト生成は常に新規割り当て。コンパイラ内部で多用する短命AST/IR オブジェクトが毎サイクルGCプレッシャーを生む
- **内容**: `(cl-cc:with-object-pool (ast-call ast-var ast-const) ...)` スコープ内でプールからオブジェクトを再利用。スコープ脱出時に一括解放（GC圧力なし）。固定サイズオブジェクトをフリーリストで管理（`car` フィールドをnextポインタとして再利用）。コンパイラのhotパス（コード生成ループ）に`(declare (cl-cc:use-pool))`で自動適用。FR-668 SROAと協調してスタック割り当て可能オブジェクトをプールから除外
- **根拠**: コンパイラ自体のコンパイル速度向上（selfhosting高速化）。短命オブジェクトのGCパウズを排除
- **難易度**: Medium

---

### Phase 123 — 高度型システム IV

#### FR-680: Algebraic Effects and Handlers (代数的エフェクト・ハンドラ)

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/foundation/type/src/`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: 副作用はCLOSのdynamic-wind/handler-bind で管理。エフェクトの型安全な合成・分離なし
- **内容**: `(defeffect IO (read-char () char) (write-char (char) unit))` でエフェクト型を定義。`(with-handler (IO ...) body)` でハンドラを提供。コンパイラがエフェクトをFR-244（Effect System）の row polymorphism で型付け: `(-> unit char ! IO)`。`resume` でハンドラから継続を呼び出し（one-shot/multi-shot制御）。Koka（Microsoft Research）/ Eff / Frank / OCaml 5 effects と同等
- **根拠**: モナドトランスフォーマーより合成しやすい副作用管理。IOとエラーと状態を直交的に型付け可能。cl-ccのVM実行エフェクト（I/O、例外、状態）の型安全な抽象
- **難易度**: Very Hard

#### FR-681: GADTs / Generalized Algebraic Data Types (一般化代数的データ型)

- **対象**: `packages/foundation/type/src/`, `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: ADTs（FR-590）は全コンストラクタが同一型を返す。型インデックスによる精緻化なし
- **内容**: `(defgadt expr (int) (lit-int () (expr integer)) (add () (expr integer) (expr integer) (expr integer)) (lit-bool () (expr boolean)) (if-expr () (expr boolean) (expr a) (expr a) (expr a)))` で型インデックス付きADTを定義。パターンマッチで型インデックスが精緻化され、`add`ブランチ内では`x`が`(expr integer)`と確定。HM型推論（FR-type系）のWAMへの拡張（型等式制約）。Haskell GHC GADTs / OCaml GADTs / Coq inductive types と同等
- **根拠**: 型安全なASTを定義でき、型チェックの不変条件をコンパイル時に保証。Well-typed interpretersパターンのcl-cc実装
- **難易度**: Very Hard

#### FR-682: Rank-N Polymorphism / Higher-Rank Types (高階ランク多相)

- **対象**: `packages/foundation/type/src/`, `packages/frontend/expand/src/expander.lisp`
- **現状**: HM型推論はRank-1多相（全称量化子はトップレベルのみ）。`(forall a. a -> a) -> int` のような型は表現不可
- **内容**: System F スタイルのRank-N型を型アノテーションで受け付け。`(: run-st (forall s. (forall a. (st s a)) -> a))` のような型宣言。型推論はRank-1のままで、Rank-N箇所は明示アノテーション必須（GHC `RankNTypes` と同等の設計）。型チェックに双方向型検査（FR-593）を活用。`(declare (cl-cc:type (forall a. (-> (-> a a) a a)) twice))` で宣言
- **根拠**: STモナド（可変状態の型安全なカプセル化）・CPS変換後の継続型・高階コールバックに必要
- **難易度**: Very Hard

#### FR-683: Dependent Types / Pi Types (依存型・Pi型)

- **対象**: `packages/foundation/type/src/`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 型は値に依存しない。`(vector n)` の長さ`n`は型に反映されない
- **内容**: `(: make-vector (Pi (n : Nat) -> (vector n)))` で型が値に依存する関数を宣言。`(: vzip (Pi (n : Nat) -> (vector n) -> (vector n) -> (vector n)))` で長さ整合性をコンパイル時保証。型検査はSMTソルバー（FR-246）に長さ制約を送出して解決。完全な依存型（CIC）ではなく **液体型（Liquid Types）** スタイルの部分的依存型（整数・長さ・非零に限定）。Agda / Idris2 / Coq / Liquid Haskell と比較してスコープ限定
- **根拠**: 配列境界違反・整数オーバーフロー・ゼロ除算をコンパイル時に排除。証明支援システムへの橋渡し
- **難易度**: Very Hard

---

### Phase 124 — 関数変換技法

#### FR-686: Worker-Wrapper Transformation (ワーカー・ラッパー変換)

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 再帰関数は外部インターフェースと内部ループが一体化。boxed引数/戻り値の変換が毎再帰で発生
- **内容**: 再帰関数を **wrapper**（ユーザー向けインターフェース、型変換）と **worker**（unboxed引数でタイトループ）の2関数に分割。`(defun sum (xs) (labels ((worker (xs acc) ...)) (worker xs 0)))` の構造を自動導出。workerはunboxed整数引数（FR-type 系の unboxed representation 活用）。GHC の W/W split（最重要最適化の一つ）と同等。FR-618 DAE の補完
- **根拠**: 再帰関数のhotループからboxing/unboxingを排除。GHC実績: 多くの数値関数で2〜5倍高速化
- **難易度**: Hard

#### FR-687: Administrative Normal Form / ANF Transformation (行政正規形変換)

- **対象**: `packages/engine/compile/src/cps.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: CPS変換（FR-CPS系）が中間表現。ANFはCPSと表現力等価だがより直接的なSSA的性質を持つ
- **内容**: CPSパイプラインと並列にANF変換器を実装。`(+ (* a b) (* c d))` → `(let ((t1 (* a b)) (t2 (* c d))) (+ t1 t2))`。全部分式が名前付き一時変数に展開され、評価順序が明示的。ANF のプロパティ: SSAと同型（Kelsey 1995）、最適化パス（GVN・CSE）の実装が簡単化。`--emit-anf` フラグでANFダンプ。将来的にCPSとANFのどちらかに統一可能な設計
- **根拠**: SSAとCPS/ANFの等価性（Kelsey 1995 / Appel 1998）は型理論上重要。MLIR lower passへの入力として有用
- **難易度**: Medium

#### FR-688: Defunctionalization (脱関数化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: 高階関数はクロージャとして表現。特定パターン（CPS継続の固定セット）で関数オブジェクトをデータに変換可能な場合も常にクロージャ割り当て
- **内容**: Reynolds（1972）の脱関数化: 関数値をsumタイプ（`cont-add`/`cont-mul`/`cont-done`など）のデータに変換し`apply-cont`ディスパッチャで解釈。CLOSのgeneric functionとして実装（specializerがdefunction tag）。静的解析でclose-worldの関数セットが確定できる場合にのみ適用。`(declare (cl-cc:defunctionalize))` で手動指定
- **根拠**: クロージャ割り当てゼロの一階CPS実行。インタプリタのメイン継続ループを最適化。Reynolds defunctionalization のCPS + tail callとの組み合わせが非常に強力
- **難易度**: Hard

#### FR-689: Lambda Lifting (ラムダリフティング)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/frontend/expand/src/expander.lisp`
- **現状**: ネストした `flet`/`labels` 定義はクロージャとしてコンパイル。自由変数がキャプチャされ毎呼び出しでクロージャ生成
- **内容**: 自由変数を明示的な引数として追加してローカル関数をトップレベルに持ち上げる。`(labels ((add (x) (+ x n))) (add 5))` → `(defun lifted-add (n x) (+ x n))` `(lifted-add n 5)`。クロージャ変換との使い分け: キャプチャ変数が少なく呼び出し頻度が高い場合はリフティングが優位（クロージャ割り当てなし）。FR-031 LICMと協調（ループ不変クロージャをリフト）
- **根拠**: ホットループ内のlocal function呼び出しからクロージャ割り当てを除去。selfhostingコードの内部ヘルパー関数に多数適用可能
- **難易度**: Medium

---

### Phase 125 — テスト計装・品質

#### FR-692: Code Coverage Instrumentation (コードカバレッジ計装)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/cli/src/main.lisp`
- **現状**: テスト実行時のカバレッジ計測なし。どのコードパスがテストされているか不明
- **内容**: `--coverage` フラグでライン/ブランチ/MC/DCカバレッジ計装を追加。各基本ブロックの先頭にカウンタインクリメント命令を挿入。実行後 `foo.clcc-cov` ファイルに集計データを出力。`./cl-cc coverage report` でHTML/LCOVレポート生成。LLVM `InstrProfiling` / gcov (GCC) / Istanbul (JS) と同等形式。`(declare (cl-cc:no-coverage))` で特定関数を除外。FiveAMとの統合でテスト実行時に自動収集
- **根拠**: 4322テストのカバレッジ把握が困難。死んだコードの発見・テスト不足領域の特定に不可欠
- **難易度**: Medium

#### FR-693: Heap Profiler (ヒーププロファイラ)

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/engine/vm/src/vm-execute.lisp`, `packages/cli/src/main.lisp`
- **現状**: `--verbose-gc` でGC統計のみ。どの関数・コードパスが最もメモリを消費するか不明
- **内容**: `./cl-cc run --heap-profile foo.lisp` でサンプリングベースヒーププロファイリング。割り当て時にPC（プログラムカウンタ）をサンプリングしてコールスタック記録。`./cl-cc heapprof report foo.clcc-heap` でフレームグラフ（Brendan Gregg形式）/ allocation breakdown ツリー生成。型別割り当て量の集計。`(cl-cc:with-heap-profile ...)` スコープで特定区間をプロファイル。Heaptrack / jemalloc heap profiler / Go pprof と同等
- **根拠**: selfhostingコンパイルの最大メモリ消費箇所の特定。GCチューニングの根拠データ収集
- **難易度**: Medium

#### FR-694: Mutation Testing (ミューテーションテスト)

- **対象**: `packages/cli/src/main.lisp`, 新規 `src/testing/mutation.lisp`
- **現状**: テストスイート（4322テスト）の品質はカバレッジ指標なし。テストが「通る」だけでなく「変更を検出できるか」不明
- **内容**: `./cl-cc muttest packages/engine/compile/src/cps.lisp` でソースにミューテーション（`+`→`-`、`<`→`<=`、定数変更、ブランチ反転、関数呼び出し削除）を自動適用しテスト実行。**生き残ったミュータント**（テストが通るが実際は壊れたコード）を報告。ミューテーションスコア = 殺されたミュータント / 全ミュータント数。`--timeout 10s` で各ミュータントのテスト実行タイムアウト。PiTest（Java）/ Mutant（Ruby）/ cosmic-ray（Python）と同等
- **根拠**: 高カバレッジでも検出力が低いテストを発見。テストスイートの実質的な強さを定量化
- **難易度**: Hard

#### FR-695: Benchmarking Framework (ベンチマークフレームワーク)

- **対象**: 新規 `src/testing/benchmark.lisp`, `tests/bench/`
- **現状**: パフォーマンス回帰の検出手段なし。コード変更がコンパイラ速度・生成コード速度に与える影響を定量化できない
- **内容**: `(defbench matrix-mul-3x3 (b) (declare (iterations 10000)) (run-once b (matrix-mul a b)))` でベンチマーク定義。**統計的に頑健な計測**: warm-up・外れ値除去・Mann-Whitney U検定で回帰検出。`./cl-cc bench --compare old.json` でベースラインとの差分表示。`--emit-flamegraph` でCPUプロファイルをGnuplotフレームグラフ出力。Criterion（Rust）/ Google Benchmark（C++）/ Hyperfine と同等の統計的精度
- **根拠**: 最適化パスの追加・変更時にパフォーマンス回帰を自動検出。selfhostingコンパイル時間のベースライン管理
- **難易度**: Medium

---

### Phase 126 — ビルドスケーラビリティ

#### FR-698: Parallel Compilation (並列コンパイル)

- **対象**: `packages/cli/src/main.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: ファイルのコンパイルはシリアル。86ソースファイルの selfhosting が全て順次処理
- **内容**: ASDF依存グラフを解析してトポロジカルソート後、独立コンパイル単位を並列実行。`./cl-cc build --jobs 8` で最大8並列コンパイル。ファイル間依存（パッケージ定義・マクロ）の依存追跡と並列安全性確認。共有マクロ環境の読み取りロック・書き込みロック（SBCL `bt:make-lock`）。`make -j` / Bazel 並列ビルド / Cargo `--jobs` と同等
- **根拠**: selfhostingコンパイル時間の削減。8コアマシンで最大4〜6倍のビルド高速化（I/O待ちで線形にはならない）
- **難易度**: Hard

#### FR-699: Distributed Build Cache (分散ビルドキャッシュ)

- **対象**: `packages/cli/src/main.lisp`, 新規 `src/build/cache.lisp`
- **現状**: FR-452（コンパイルキャッシュ）はローカルディスクキャッシュのみ。CI/CDの並列ジョブ間でキャッシュ共有不可
- **内容**: コンテンツアドレス型リモートキャッシュ（入力ハッシュ→コンパイル済み成果物）。バックエンド: HTTP(S) RESTful API / Redis / S3 互換ストレージ。`--remote-cache https://cache.example.com` で有効化。アップロード/ダウンロードの並列化。FR-662（Reproducible Builds）前提（非決定論的出力はキャッシュ不可）。Bazel Remote Cache API（REAPI）互換プロトコル。GitHub Actions / GitLab CI との統合例を文書化
- **根拠**: CI/CDでPRごとに全ファイル再コンパイルを回避。大規模チームでのビルド時間を数十分→数秒に短縮
- **難易度**: Hard

#### FR-700: Compiler Plugin API (コンパイラプラグインAPI)

- **対象**: `packages/cli/src/main.lisp`, `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: コンパイラのパスは全て内部実装。外部からIR変換・最適化・診断を注入する拡張点なし
- **内容**: `(cl-cc:define-compiler-plugin my-plugin (:after :cps-transform) (fn ir) ...)` でコンパイルパイプラインへのフックを定義。`--load-plugin my-plugin.lisp` でロード。プラグインがアクセスできるAPI: IR読み取り・変換・診断発行・新命令登録。プラグインのサンドボックス化（危険操作は別パーミッション）。GCC plugin API / LLVM pass plugin / rustc `proc-macro` の設計を参考
- **根拠**: ドメイン固有最適化（DSPコード用ベクトル化ヒント、セキュリティポリシー強制）を外部から追加可能。cl-ccの拡張性の核心
- **難易度**: Hard

#### FR-701: Live Code Update / Hot Patching (ライブコード更新・ホットパッチ)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/cli/src/main.lisp`, `packages/engine/vm/src/vm-run.lisp`
- **現状**: コード変更にはプロセス再起動が必要。REPL（FR-098）はトップレベル式の評価のみ
- **内容**: 実行中VMへの関数定義の動的差し替え。`./cl-cc live-update --pid 1234 new-fn.lisp` で実行中プロセスの関数テーブルを更新。`*function-table*` ハッシュの原子的更新（実行中フレームは旧版を完了、新呼び出しから新版使用）。インライン化済みJITコードの無効化（FR-060 JITキャッシュ連携）。Erlang Hot Code Loading / Clojure nREPL / Common Lisp image hot patch と同等
- **根拠**: Webサービス・長時間デーモンのゼロダウンタイム更新。cl-cc自身のセルフホスティング開発サイクルの高速化（再起動なし）
- **難易度**: Very Hard

---

### Phase 127 — 低レベル制御

#### FR-704: Inline Assembly (インラインアセンブリ)

- **対象**: `packages/frontend/expand/src/expander.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: x86-64バックエンドは内部命令セット（FR-x86-64系）のみ。ユーザーが任意のアセンブリを挿入できない
- **内容**: `(cl-cc:asm "movq %rax, %rbx" :outputs ((dst :r)) :inputs ((src :r)) :clobbers (:r0))` でGCC extended assembly構文相当のインラインasmを提供。制約文字列（`"r"`, `"m"`, `"=r"`）でレジスタ/メモリ指定。レジスタアロケータとの統合（clobber宣言で使用レジスタを保護）。`(cl-cc:asm :volatile "cpuid" ...)` でmemory barrier / serialize として使用。SBCL `%primitive` / C `__asm__` 相当
- **根拠**: 暗号アルゴリズムの定数時間実装（FR-453 Constant-Time）・OS kernel サポート・カスタムABI呼び出しでインラインasmが不可欠
- **難易度**: Hard

#### FR-705: Bit Manipulation Intrinsics (ビット操作組み込み関数)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/vm-bitwise.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **現状**: bitwise演算（FR-304系）は汎用。`popcount`/`clz`/`ctz`/`bswap` などの1命令CPU操作をエミュレート
- **内容**: `(cl-cc:popcount x)` → `POPCNT` x86命令（1サイクル）。`(cl-cc:clz x)` → `BSR`/`LZCNT`（先頭ゼロビット数）。`(cl-cc:ctz x)` → `BSF`/`TZCNT`（末尾ゼロビット数）。`(cl-cc:bswap x)` → `BSWAP`（バイト順反転）。`(cl-cc:pdep mask val)` → `PDEP`（ビット散布）。`(cl-cc:pext mask val)` → `PEXT`（ビット収集）。ARMでは `RBIT`/`CLZ`/`REV` にマップ。コンパイラが使用箇所を自動認識してintrinsicに置換する パターンマッチも実装
- **根拠**: ハッシュテーブル（popcount）・圧縮データ構造（pdep/pext）・暗号実装（bswap）で手書きより100倍速い単一命令実行
- **難易度**: Medium

#### FR-706: Memory Model Specification (メモリモデル仕様)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`, `packages/backend/emit/src/aarch64.lisp`
- **現状**: マルチスレッドのメモリ可視性（FR-160系）はhardwareに依存。形式的なメモリモデルの宣言なし
- **内容**: C11/C++11/Java Memory Model 準拠のメモリ順序を宣言的に扱う。`(cl-cc:load :acquire ptr)` / `(cl-cc:store :release ptr val)` / `(cl-cc:fence :seq-cst)` の原子操作APIにセマンティクスを付与。x86-64は TSO（Total Store Order）なので `acquire`/`release` はコード生成上のバリアなし、AArch64では `LDAR`/`STLR` 命令を発行。`(declare (cl-cc:memory-model :tso))` でターゲットのメモリモデルを宣言し最適化の適法性チェック
- **根拠**: ロックフリーデータ構造（FR-163）のバグの最大原因がメモリモデル誤解。形式的仕様による安全性保証
- **難易度**: Hard

#### FR-707: Dynamic Loading / dlopen (動的ローディング)

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/cli/src/main.lisp`, `src/ffi/`
- **現状**: バイナリは静的リンクのみ。実行時の共有ライブラリロード・プラグイン機能なし
- **内容**: `(cl-cc:load-library "libfoo.so")` → `dlopen`（Linux/macOS）/ `LoadLibraryA`（Windows）呼び出し。`(cl-cc:foreign-symbol lib "foo_init")` → `dlsym` でシンボル解決。アンロード: `(cl-cc:unload-library lib)` → `dlclose`。ライブラリのエラー処理（`dlerror`）。型安全なwrapper: `(cl-cc:define-foreign-callable lib "process" (-> integer integer))` でFFI型注釈付きロード。FR-632（Bindgen）との統合で型付きwrapperを自動生成
- **根拠**: プラグインシステム・ゲームMOD・ライブラリの遅延ロードに必須。cl-ccのREPL（FR-098）でのライブラリ動的ロードに直結
- **難易度**: Medium

---

### Phase 128 — 型システム V

#### FR-710: Phantom Types (ファントム型)

- **対象**: `packages/foundation/type/src/`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: 型パラメータは全て実行時値を持つ。コンパイル時専用の型マーカーなし
- **内容**: `(deftype tagged (a) ...)` で型パラメータ`a`が実行時には消去される「ファントム型」を定義。`(: make-safe-ptr (-> raw-ptr (tagged :validated raw-ptr)))` で検証済みポインタを型レベルでマーク。`(: use-safe-ptr (-> (tagged :validated raw-ptr) result))` で検証済みのみ受け付け。状態機械をファントム型でエンコード: `(tagged :open file-handle)` / `(tagged :closed file-handle)`。Haskell phantom types / Rust zero-sized type markers と同等
- **根拠**: SQLインジェクション防止（`(tagged :sanitized string)` のみSQL関数に渡せる）・未初期化データ保護・プロトコル状態の静的検証
- **難易度**: Medium

#### FR-711: Type Classes / Coherent Overloading (型クラス・コヒーレントオーバーロード)

- **対象**: `packages/foundation/type/src/`, `packages/frontend/expand/src/expander.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: CLOSのgeneric functionがアドホック多相を提供するが型推論との統合なし。型クラス制約の推論不可
- **内容**: `(define-class (Eq a) (= (-> a a boolean)))` でHaskellスタイル型クラスを定義。`(define-instance (Eq integer) (= (lambda (a b) (cl:= a b))))` でインスタンス実装。型推論が型クラス制約を自動推論: `(defun member (x xs) ...)` → 型 `(forall a. (Eq a) => a -> (list a) -> boolean)` を導出。**コヒーレンス保証**: 同一型に複数の矛盾するインスタンスが存在しないことをコンパイル時検証。辞書渡し（dictionary passing）でコンパイル
- **根拠**: CLOSよりも型安全なオーバーロード。型推論システムとの統合でアドホック多相の型エラーを早期検出
- **難易度**: Very Hard

#### FR-712: Monomorphization (単態化)

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: generic functionはディスパッチテーブル経由の動的ディスパッチ。型が静的に確定している場合も動的コスト発生
- **内容**: 型推論（FR-type系）で型引数が完全に確定した generic function 呼び出しを特定。`(foo 1 2)` の `foo` が `(-> integer integer integer)` と確定したら、整数特化版 `foo/integer/integer` を生成しダイレクト呼び出しに置換。Rust のゼロコスト抽象 / C++ テンプレート特殊化 / MLton の whole-program monomorphization と同等。コードサイズとランタイム性能のトレードオフを `--mono-threshold` で制御
- **根拠**: 動的ディスパッチを直接呼び出しに変換し間接参照コストを排除。JITなしで数値演算を最大5倍高速化可能
- **難易度**: Hard

#### FR-713: Termination Checking (停止性検査)

- **対象**: `packages/foundation/type/src/`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 全再帰関数は停止するかどうか不明。無限ループと意図的な非停止が区別不可
- **内容**: **構造的再帰チェック**: 再帰呼び出しの引数が元の引数の「構造的に小さい」部分項であることを確認（`cdr`・`rest`・整数デクリメント）。`(declare (cl-cc:terminates))` で停止性を宣言しコンパイラが検証。停止性不明時は警告。**サイズ変化分析**: 整数引数の減少を追跡（`(> n 0)` ガード + `(- n 1)` 再帰）。Agda / Coq / Idris の termination checker と同等（簡易版）。`(declare (cl-cc:non-terminating))` でジェネレータ・サーバーループを明示
- **根拠**: 型理論の健全性には停止性が必要。証明支援システム統合（FR-627）の前提条件
- **難易度**: Hard

---
