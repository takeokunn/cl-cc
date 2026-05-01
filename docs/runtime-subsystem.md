# Runtime: Subsystems & Infrastructure

Inline caches, safepoints, numeric tower, FFI, debugging/profiling, concurrent runtime, dynamic code management, runtime self-hosting, advanced optimization, Unicode, pathname, streams, pretty printer.

---

### Phase 90 — インラインキャッシュ・動的ディスパッチ最適化

2026年のモダンJIT（V8 Maglev/Turbofan、GraalVM、HotSpot C2）が実装する動的ディスパッチの最適化インフラ。

#### FR-500: Monomorphic Inline Cache (MIC) for Generic Functions

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: `vm-generic-call`が毎回ディスパッチテーブルをフルスキャン（`vm-clos.lisp:execute-instruction vm-generic-call`）
- **内容**:
  - 各call siteに「最後に見た型→メソッドクロージャ」のインラインキャッシュを添付
  - 同じ型で呼ばれた場合はディスパッチテーブル検索をスキップ
  - キャッシュミス時のみフォールバック（フルスキャン）
  - 型変化を検出したら Polymorphic IC (FR-501) にエスカレート
- **根拠**: V8 IC / HotSpot inline cache。ジェネリック関数呼び出しコストを1〜2命令に削減
- **難易度**: Hard

#### FR-501: Polymorphic Inline Cache (PIC) for Generic Functions

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/compile/src/codegen.lisp`
- **依存**: FR-500
- **現状**: MIC実装後、複数型から呼ばれるcall siteで毎回キャッシュミスが発生する
- **内容**:
  - キャッシュを最大N件（デフォルト4）の`(type . method)`ペア配列に拡張
  - 線形スキャンでO(N)の型マッチ、N超過でメガモーフィック状態に移行
  - PIC配列をVM命令のスロット（`:sexp-slots`）に格納してGC管理
- **根拠**: SpiderMonkey PIC / V8 polymorphic IC。2〜4型のジェネリック関数呼び出しを高速化
- **難易度**: Hard

#### FR-502: Megamorphic Call Site Handling

- **対象**: `packages/vm/src/vm-clos.lisp`
- **依存**: FR-501
- **内容**:
  - PICがN型を超えた場合、グローバルメガモーフィックキャッシュ（全call site共有のハッシュテーブル）に移行
  - メガモーフィック状態は「最適化を諦めて安定した低コスト実装に落とす」戦略
  - インライン化候補から除外（FR-158への入力として利用）
- **根拠**: V8 megamorphic stub / HotSpot MegamorphicCache。多態性の高いcall siteを安定処理
- **難易度**: Medium

#### FR-503: Type Feedback Vector (TFV)

- **対象**: `packages/compile/src/codegen.lisp`, `packages/vm/src/vm-run.lisp`
- **内容**:
  - 各関数に対してcall site数分のフィードバックスロットを持つベクタを割り当て
  - 実行時に型情報（引数型、分岐方向、ループカウント）をスロットに記録
  - Tier-1コンパイル時（FR-154）にTFVを参照して特化コード生成
- **根拠**: V8 FeedbackVector / HotSpot MethodData。型プロファイリングの中核データ構造
- **難易度**: Hard

#### FR-504: Call Site Speculation (呼び出し先特化)

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
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

- **対象**: `packages/vm/src/vm-run.lisp`, `packages/runtime/src/gc.lisp`
- **現状**: GCは任意のタイミングで`rt-gc-collect`を呼び出せる前提。スタック上のVM値がGCルートとして登録されていない
- **内容**:
  - セーフポイント: 関数呼び出し・ループバックエッジ・メモリ割り当て直前を「安全点」とする
  - セーフポイント到達時のみGCが起動（コオペラティブ方式）
  - `*gc-pending*`フラグをセーフポイントチェック命令でポーリング
- **根拠**: HotSpot Safepoint / V8 GC safepoints。正確なGCの必須インフラ
- **難易度**: Hard

#### FR-511: Precise Stack Map (精確スタックマップ)

- **対象**: `packages/compile/src/codegen.lisp`, `packages/runtime/src/gc.lisp`
- **依存**: FR-510
- **現状**: GCはスタックフレームのどのスロットがLispオブジェクトかを知らない（保守的スキャン）
- **内容**:
  - 各セーフポイントでレジスタ・スタックスロット中のオブジェクト参照の位置情報を記録
  - `stackmap: ((frame-offset . :object) ...)` テーブルをコンパイル時に生成
  - GCがスタックマップを参照して精確にオブジェクトを追跡
- **根拠**: JVM StackMapTable / GHC info tables。保守的GCによる偽参照保持を排除
- **難易度**: Very Hard

#### FR-512: Return Address Poisoning (リターンアドレス保護)

- **対象**: `packages/vm/src/vm-execute.lisp`, `packages/runtime/src/runtime.lisp`
- **内容**:
  - GCがフレームをスキャンする際、リターンアドレスとLispオブジェクトポインタを区別
  - リターンアドレスにタグビットを付与（または別フィールドに移動）してGCスキャンから除外
  - セキュリティ上も有益: Shadow Stack（FR-513）との連携
- **根拠**: V8 / HotSpot frame layout。スタックスキャンの正確性向上
- **難易度**: Medium

#### FR-513: Shadow Stack for CLOS Dispatch (CLOSディスパッチ用シャドウスタック)

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/vm/src/vm-execute.lisp`
- **内容**:
  - CLOS `call-next-method` の連鎖をスタックとは別の「シャドウスタック」で管理
  - メソッドコンビネーション（:before/:after/:around）の呼び出し順を明示的なデータ構造で表現
  - 現状の暗黙的な再帰呼び出しを排除し、デバッグ・プロファイリングを容易にする
- **根拠**: CLOS next-method chain visualization。SBCL PCL実装の参照実装
- **難易度**: Medium

---

### Phase 92 — 数値タワー・Unboxed演算

#### FR-520: Fixnum Unboxed Arithmetic Path

- **対象**: `packages/vm/src/primitives.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: 全算術演算が`vm-value-fixnum-p`チェック後にunboxして計算、再boxして返す
- **内容**:
  - 型推論（FR-004）でfixnumと確定した変数に対してunboxedレジスタを割り当て
  - `vm-add-fixnum`/`vm-mul-fixnum`等の型特化命令を追加
  - box/unboxコストを完全除去。内側ループでの数値演算が2-5倍高速化
- **根拠**: SBCL `(declare (type fixnum n))` unboxed / HotSpot int intrinsics
- **難易度**: Hard

#### FR-521: Float Unboxed Arithmetic (浮動小数点Unboxed演算)

- **対象**: `packages/vm/src/primitives.lisp`, `packages/compile/src/codegen.lisp`
- **依存**: FR-520
- **内容**:
  - NaN-boxingの64-bit doubleペイロードを直接XMMレジスタに保持
  - `vm-fadd`/`vm-fmul`/`vm-fsqrt`等のfloat専用命令
  - float配列に対するループでのスカラー展開
- **根拠**: SBCL `(declare (type double-float x))` / LuaJIT float unboxing
- **難易度**: Hard

#### FR-522: Bignum Fast Path (Bignum高速パス)

- **対象**: `packages/vm/src/primitives.lisp`
- **現状**: bignum演算はホストCL `+`/`*`等に完全委譲
- **内容**:
  - 小bignum（2ワード以内）をfixnum隣接の特化パスで処理
  - Karatsuba乗算（FR-523の基盤）
  - コンパイル時bignum畳み込みをFR-181の定数プールに統合
- **根拠**: GMP / Java BigInteger fast path。数値演算ベンチマークの底上げ
- **難易度**: Hard

#### FR-523: SIMD Intrinsics (SIMDイントリンシクス)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/vm/src/vm.lisp`
- **内容**:
  - `(simd:+ vec1 vec2)` / `(simd:dot vec1 vec2)` 等のSIMD組み込み関数
  - SSE2/AVX2命令への直接マッピング (`vmovdqu`, `vaddps`, `vmulps`)
  - `make-array :element-type 'single-float` の配列を SIMD バッファとして扱う
  - 自動ベクタライザ（FR-524）の低レベル基盤
- **根拠**: LLVM vector intrinsics / LuaJIT FFI SIMD。数値計算の10-20倍高速化
- **難易度**: Very Hard

#### FR-524: Auto-Vectorization (自動ベクタライズ)

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **依存**: FR-521, FR-523
- **内容**:
  - float配列に対する単純ループ（`(dotimes (i n) (aset r i (f+ (aref a i) (aref b i))))`）をSIMDループに変換
  - ループ長が4の倍数でない場合のスカラーエピローグ自動生成
  - エイリアス解析（FR-017）でa/b/rが重ならないことを確認してからベクタライズ
- **根拠**: GCC/LLVM auto-vectorization (-O3 -march=native)
- **難易度**: Very Hard

#### FR-525: Number Tower Rationalization (数値タワー整理)

- **対象**: `packages/vm/src/primitives.lisp`, `packages/expand/src/macros-stdlib.lisp`
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

- **対象**: `packages/runtime/src/runtime.lisp`, `packages/vm/src/vm.lisp` (新ファイル `src/ffi/cffi.lisp`)
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

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **内容**:
  - `(asm :mov :rax 42)` — VM命令シーケンス中にアセンブリ命令を直接埋め込む
  - システムコール発行(`syscall`命令)のためのインターフェース
  - CPUID/RDTSC等の特殊命令へのアクセス
- **根拠**: GCC `__asm__` / SBCL `%primitive`。低レベルランタイム操作の基盤
- **難易度**: Medium

---

### Phase 94 — デバッグ・プロファイリングインフラ

#### FR-540: DWARF Debug Information Generation

- **対象**: `packages/binary/src/macho.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: Mach-Oバイナリ生成時にデバッグ情報なし。`lldb`/`gdb`でソースマップ不可
- **内容**:
  - DWARF 5形式でデバッグ情報を生成（`.debug_info`, `.debug_line`, `.debug_abbrev`セクション）
  - VM命令→元のLispフォームのソース位置マッピング
  - 変数名・型情報の埋め込み（デバッガからスロットが見える）
  - `--debug`ビルドフラグで有効化
- **根拠**: DWARF standard / LLVM debug info。プロダクションデバッグの必須条件
- **難易度**: Very Hard

#### FR-541: Sampling Profiler (サンプリングプロファイラ)

- **対象**: `packages/vm/src/vm-run.lisp`, `packages/cli/src/main.lisp`
- **現状**: プロファイリング機能なし。ホットスポット特定が不可能
- **内容**:
  - `SIGPROF`シグナルハンドラでスタックトレースをサンプリング（macOS: `setitimer`）
  - 関数名→カウントのヒストグラムを収集
  - `./cl-cc profile myscript.lisp` でフレームグラフ出力
  - FR-503（TFV）のプロファイルデータと統合
- **根拠**: Linux perf / py-spy / rbspy。実行時ホットスポット特定の標準ツール
- **難易度**: Hard

#### FR-542: Allocation Profiler (アロケーションプロファイラ)

- **対象**: `packages/runtime/src/gc.lisp`, `packages/vm/src/vm-run.lisp`
- **内容**:
  - `rt-gc-alloc`呼び出し時に呼び出し元情報（関数名・行番号）を記録
  - 型別・サイズ別の割り当て統計（`cons`が全割り当ての何%か等）
  - GCプレッシャーの高い関数を特定してTRMC/スタック割り当て変換の対象を絞る
- **根拠**: JVM allocation profiling (-agentlib:hprof) / Heaptrack
- **難易度**: Medium

#### FR-543: VM Instruction Tracer (VM命令トレーサ)

- **対象**: `packages/vm/src/vm-run.lisp`
- **内容**:
  - `*vm-trace-mode*` フラグで実行命令をリアルタイム出力
  - 条件付きブレークポイント: `(vm-break-on 'vm-generic-call)` で特定命令型で停止
  - デバッガREPLへの接続（`slynk`/`swank`プロトコル拡張）
- **根拠**: SBCL `(trace)` / ChakraCore instruction tracing
- **難易度**: Medium

#### FR-544: Coverage Instrumentation (カバレッジ計測)

- **対象**: `packages/compile/src/codegen.lisp`, `packages/vm/src/vm-run.lisp`
- **内容**:
  - `--coverage`フラグで各フォームの実行カウンタを埋め込み
  - `lcov`互換カバレッジレポートをHTMLで出力
  - テストスイートの未カバーパス特定（FiveAMとの統合）
- **根拠**: SBCL sb-cover / Istanbul / gcov
- **難易度**: Medium

---

### Phase 95 — コンカレントランタイム基盤

#### FR-550: Thread-Local Allocation Buffer (TLAB)

- **対象**: `packages/runtime/src/heap.lisp`, `packages/runtime/src/gc.lisp`
- **現状**: 単一バンプポインタ（`young-free`）へのアクセスがシリアライズされる
- **内容**:
  - 各スレッドにナーサリの一部（デフォルト256KB）を専有させる
  - スレッドローカルなバンプポインタでアロケーション
  - TLABが枯渇したらGCロックを取ってリフィル
- **根拠**: HotSpot TLAB / GraalVM TLAB。マルチスレッドアロケーションのスケールアップ
- **難易度**: Hard

#### FR-551: Atomic Value Operations (アトミック値操作)

- **対象**: `packages/vm/src/vm.lisp`, `packages/vm/src/primitives.lisp`
- **内容**:
  - `(atomic-swap! place new-val)` — CAS (Compare-And-Swap)
  - `(atomic-incf! place delta)` — アトミックインクリメント
  - `(memory-barrier)` — フェンス命令
  - x86-64: `lock cmpxchg` / AArch64: `ldxr`/`stxr`
- **根拠**: SBCL `sb-ext:atomic-push` / Java `AtomicInteger`。ロックフリーデータ構造の基盤
- **難易度**: Hard

#### FR-552: Green Thread / Continuation (グリーンスレッド/継続)

- **対象**: `packages/vm/src/vm-execute.lisp`, `packages/vm/src/vm-run.lisp`
- **内容**:
  - `(spawn fn args)` — VMレジスタファイルをスタックに保存して新コルーチンを作成
  - `(yield)` — 現スレッドをサスペンドしてスケジューラに制御を返す
  - `(resume co val)` — 保存されたレジスタファイルを復元して実行再開
  - M:Nスレッドスケジューラ（Mグリーンスレッド : NホストOSスレッド）
- **根拠**: Go goroutine / Erlang process / Ruby Fiber。Lispのcall/ccを軽量実装する基盤
- **難易度**: Very Hard

#### FR-553: Delimited Continuations (限定継続)

- **対象**: `packages/vm/src/vm-execute.lisp`, `packages/compile/src/cps.lisp`
- **依存**: FR-552
- **内容**:
  - `(reset thunk)` / `(shift k body)` — Filinski演算子
  - CPS変換済みコードとの統合（現在のCPS変換をshift/resetのコンパイルターゲットとして活用）
  - `call/cc` のより効率的な代替実装
- **根拠**: Racket delimited continuations / Koka effect handlers
- **難易度**: Very Hard

#### FR-554: Lock-Free Symbol Table (ロックフリーシンボルテーブル)

- **対象**: `packages/vm/src/vm.lisp` (`vm-intern-symbol`)
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

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **現状**: コンパイル済みVM命令列はメモリ上に保持されるが容量上限なし。再コンパイル時の古いコード回収なし
- **内容**:
  - コードキャッシュサイズ上限（デフォルト256MB）を設定
  - LRU / 呼び出し頻度ベースの退避ポリシー
  - 退避されたコードを再JIT可能な状態で保持（ソースは保持）
- **根拠**: HotSpot code cache / V8 code flushing。長時間動作プロセスのメモリ安定化
- **難易度**: Hard

#### FR-561: Lazy Module Loading (遅延モジュールロード)

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **内容**:
  - `(require "module")` を遅延実行：参照時点でコンパイル・ロード
  - モジュール依存グラフの構築と循環依存検出
  - コンパイル済みFASLキャッシュとの統合（タイムスタンプで再コンパイル判定）
- **根拠**: Python `importlib` lazy loading / Node.js lazy require
- **難易度**: Medium

#### FR-562: Hot Code Reload (ホットコードリロード)

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/vm/src/vm.lisp`
- **内容**:
  - `(reload-function 'foo)` — 実行中プロセスの関数定義を差し替え
  - 実行中フレームの古い定義への参照を新定義にリダイレクト（Erlang hot_code_replace相当）
  - REPL・開発環境での高速イテレーション
- **根拠**: Erlang hot code reload / SBCL `(compile 'foo)`。Lisp開発体験の中核
- **難易度**: Hard

#### FR-563: Image Snapshot / Heap Dump (イメージスナップショット)

- **対象**: `packages/runtime/src/heap.lisp`, `packages/cli/src/main.lisp`
- **内容**:
  - `(save-image "myapp.img")` — 現在のヒープ状態をバイナリ化してディスクに保存
  - `./cl-cc --image myapp.img` — イメージをロードして実行再開
  - すべてのグローバル変数・定義済み関数・インターン済みシンボルを保存
- **根拠**: SBCL `save-lisp-and-die` / Smalltalk image。起動コスト0化の必殺技
- **難易度**: Very Hard

#### FR-564: Incremental Compilation Cache (増分コンパイルキャッシュ)

- **対象**: `packages/pipeline/pipeline.lisp`
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

- **対象**: `packages/runtime/src/runtime.lisp` (新ファイル `packages/runtime/src/os.lisp`)
- **内容**:
  - ファイルIO: `rt-open`/`rt-read`/`rt-write`/`rt-close` — `read`/`write` syscallの薄いラッパ
  - プロセス: `rt-fork`/`rt-exec`/`rt-waitpid`
  - タイマー: `rt-gettime`（`clock_gettime`）/ `rt-sleep`（`nanosleep`）
  - macOS/Linux共通インターフェース（syscall番号をプラットフォーム別定数で分岐）
- **根拠**: musl libc相当の最小OSインターフェース。ホストCL依存を段階的に解消する基盤
- **難易度**: Hard

#### FR-571: Standalone Binary Bootstrap

- **対象**: `packages/cli/src/main.lisp`, `packages/binary/src/macho.lisp`, `flake.nix`
- **依存**: FR-570
- **内容**:
  - `./cl-cc compile --standalone foo.lisp -o foo` で依存ゼロのネイティブバイナリを生成
  - ランタイム（GC + VM interpreter + 標準ライブラリ）をバイナリに静的リンク
  - エントリポイント: `_start` → ランタイム初期化 → `main`関数呼び出し
- **根拠**: Go `go build` の静的バイナリ / Zig `zig build-exe`
- **難易度**: Very Hard

#### FR-572: Signal Handling (シグナルハンドリング)

- **対象**: `packages/runtime/src/os.lisp` (FR-570)
- **内容**:
  - `(signal-handler SIGINT (lambda () ...))` — OSシグナルをLispハンドラに変換
  - `SIGFPE` → `arithmetic-error` condition
  - `SIGSEGV` → `storage-condition` condition（スタックオーバーフロー検出）
  - セーフポイント（FR-510）との統合: シグナルはセーフポイントでのみ配送
- **根拠**: SBCL `sb-sys:enable-interrupt` / Erlang signal handling
- **難易度**: Hard

#### FR-573: Process/Environment Interface (プロセス・環境インターフェース)

- **対象**: `packages/cli/src/main.lisp`, `packages/runtime/src/runtime.lisp`
- **現状**: `(uiop:getenv "PATH")` 等のホストCL経由。ネイティブバイナリでは使用不可
- **内容**:
  - `(getenv "PATH")` — `environ`配列からの環境変数取得
  - `(argv)` — コマンドライン引数リスト
  - `(exit code)` — `_exit` syscall
  - `(getcwd)` / `(chdir path)`
- **根拠**: POSIX.1 process model。完全自立バイナリの必須インターフェース
- **難易度**: Easy

#### FR-574: Socket/Network Primitives (ソケット・ネットワーク原語)

- **対象**: 新ファイル `packages/runtime/src/net.lisp`
- **内容**:
  - `(socket :tcp)` / `(bind s addr port)` / `(connect s addr port)` / `(listen s)` / `(accept s)`
  - ノンブロッキングIO: `(set-nonblocking s)` + `(select fds timeout)`
  - `epoll`（Linux）/ `kqueue`（macOS）イベントループ統合
- **根拠**: SBCL `sb-bsd-sockets` / Erlang gen_tcp。Webサーバ・REPLサーバの基盤
- **難易度**: Hard

---

### Phase 98 — 高度なランタイム最適化 (2026年最先端)

#### FR-580: Shape-Based Object Layout (シェイプ最適化)

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: CLOSインスタンスはハッシュテーブル（スロット名→値）。`slot-value`が毎回ハッシュ参照
- **内容**:
  - Hidden Class / Object Shape: 同じスロット集合を持つインスタンスは同一「シェイプ」を共有
  - シェイプIDをオブジェクトヘッダ（FR-266）に格納
  - `slot-value` → 固定オフセット配列アクセスに変換（ハッシュ参照不要）
  - `(slot-value obj 'x)` → `(aref (object-slots obj) shape-offset-of-x)`
- **根拠**: V8 Hidden Classes / PyPy map cache / SBCL structure slots
- **難易度**: Very Hard

#### FR-581: Speculative Slot Access Optimization

- **対象**: `packages/vm/src/vm-clos.lisp`, `packages/compile/src/codegen.lisp`
- **依存**: FR-580, FR-503
- **内容**:
  - TFVから「このオブジェクトは常にシェイプSを持つ」と判明した場合、固定オフセットアクセスにコンパイル
  - シェイプチェックガード（1命令）＋固定オフセットロード
  - シェイプ変化時はdeopt（FR-155）
- **根拠**: V8 Maglev property access / SpiderMonkey JIT slot load
- **難易度**: Very Hard

#### FR-582: Escape Analysis → Stack Allocation

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/compile/src/codegen.lisp`
- **依存**: FR-007（エスケープ解析）
- **内容**:
  - エスケープしないと証明されたCLOSインスタンス・クロージャをVMスタックフレームに直接展開
  - `(let ((p (make-instance 'point :x 1 :y 2))) (+ (slot-value p 'x) (slot-value p 'y)))` → スタック上の2変数に展開
  - GCヒープ割り当てを完全回避
- **根拠**: HotSpot Escape Analysis + Scalar Replacement / GraalVM PE
- **難易度**: Very Hard

#### FR-583: Partial Evaluation / Supercompilation

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/compile/src/codegen.lisp`
- **内容**:
  - `(defun make-adder (n) (lambda (x) (+ x n)))` → `(make-adder 5)` → `(lambda (x) (+ x 5))` に部分評価
  - 定数引数が既知の関数呼び出しを特化クロージャとしてコンパイル
  - メモ化付き特化（同じ定数引数の組み合わせは1回だけコンパイル）
- **根拠**: Futamura projections / GraalVM partial evaluation / PyPy tracing JIT
- **難易度**: Very Hard

#### FR-584: Profile-Guided Optimization (PGO)

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/optimize/src/optimizer.lisp`
- **依存**: FR-541（サンプリングプロファイラ）, FR-503（TFV）
- **内容**:
  - Tier-1コンパイル時にTFVのプロファイルデータを参照して最適化判断
  - ホットブランチ予測情報をコード配置（hot/cold分離）に反映
  - `./cl-cc pgo-train myscript.lisp` → プロファイルデータ生成 → `./cl-cc compile --pgo`
- **根拠**: LLVM PGO / GCC -fprofile-use / HotSpot tiered profiling
- **難易度**: Very Hard

#### FR-585: Concurrent Compilation (並列コンパイル)

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **現状**: selfhostが84ファイルをシリアルにコンパイル
- **内容**:
  - ASDF依存グラフのトポロジカルソートから並列化可能なファイルセットを抽出
  - `lparallel`/OSスレッドプールで独立ファイルを並列コンパイル
  - コンパイル中のマクロ定義が後続ファイルに伝播するセマンティクスを保持
- **根拠**: `make -jN` / Buck2 parallel actions / Gradle parallel tasks
- **難易度**: Hard

#### FR-586: Compressed Instructions (命令圧縮)

- **対象**: `packages/vm/src/vm.lisp`, `packages/vm/src/vm-run.lisp`
- **現状**: 全VM命令がCLOSオブジェクト（デフストラクト）としてリスト/ベクタに格納
- **内容**:
  - 高頻度命令（`vm-move`, `vm-const`, `vm-add`, `vm-call`）をfixed-width バイト列にエンコード
  - 命令デコーダ（`decode-instruction`）をdispatch-tableで実装
  - メモリフットプリントを最大70%削減、icache効率向上
- **根拠**: JVM bytecode / WASM binary format / RISC-V C拡張（16-bit圧縮命令）
- **難易度**: Hard

#### FR-587: Tracing JIT Prototype (トレースJITプロトタイプ)

- **対象**: `packages/vm/src/vm-run.lisp`, `packages/emit/src/x86-64-codegen.lisp`
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

- **対象**: `packages/vm/src/strings.lisp`, `packages/vm/src/vm.lisp` (新ファイル `packages/vm/src/unicode.lisp`)
- **現状**: 文字述語（`alpha-char-p`等）がASCII範囲のみ動作。Unicode 15+の多バイト文字で誤動作
- **内容**:
  - Unicode Character Database (UCD) を静的テーブルとして埋め込み（約200KB）
  - `char-upcase`/`char-downcase` — Unicode case folding (Turkish dotless-i等の特殊ケース含む)
  - `alpha-char-p`/`digit-char-p`/`alphanumericp`/`upper-case-p`/`lower-case-p` — UCD General Category準拠
  - `char-code` — Unicode code point (U+0000〜U+10FFFF)、`code-char` — code point → character
- **根拠**: ANSI CL 13章 / Unicode 15.0 standard。多言語テキスト処理の必須基盤
- **難易度**: Hard

#### FR-591: Unicode String Normalization (Unicode正規化)

- **対象**: `packages/vm/src/strings.lisp`
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

- **対象**: `packages/vm/src/strings.lisp`, `packages/runtime/src/value.lisp`
- **現状**: 文字列がホストCLの内部表現に依存（SBCLはUTF-32等）
- **内容**:
  - VM内部文字列をUTF-8バイト列として格納（最もメモリ効率が高い）
  - `vm-char`命令 → コードポイント単位のインデックス（O(n)だが一般的）
  - `vm-string-byte-length` vs `vm-string-char-length` の区別
  - FR-265（SSO）との統合：7バイト以下のUTF-8シーケンスをインライン格納
- **根拠**: Go `string` / Rust `str` / Python 3.12 PEP 623 compact encoding
- **難易度**: Hard

#### FR-593: Character Syntax Classes (文字構文クラス)

- **対象**: `packages/parse/src/cl/lexer.lisp`
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

- **対象**: `packages/vm/src/io.lisp` (新ファイル `packages/vm/src/pathname.lisp`)
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

- **対象**: `packages/vm/src/pathname.lisp` (FR-595)
- **依存**: FR-595
- **内容**:
  - `logical-pathname` 型: `"SRC:COMPILE;CODEGEN.LISP"` 形式
  - `logical-pathname-translations` — 論理→物理パスのマッピングテーブル
  - `translate-logical-pathname` — 変換の実行
  - ASDF統合: `:cl-cc` ホストを自動登録
- **根拠**: ANSI CL 19.3 Logical Pathnames。ポータブルなソース参照に必要
- **難易度**: Medium

#### FR-597: Filesystem Operations (ファイルシステム操作)

- **対象**: `packages/vm/src/io.lisp`
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

- **対象**: `packages/vm/src/io.lisp` (新ファイル `packages/vm/src/stream.lisp`)
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

- **対象**: `packages/vm/src/stream.lisp` (FR-600)
- **依存**: FR-600
- **内容**:
  - `fundamental-stream` / `fundamental-character-input-stream` / `fundamental-character-output-stream`
  - ユーザーが `stream-read-char` / `stream-write-char` 等のジェネリック関数をオーバーライドしてカスタムストリームを作成
  - `with-input-from-string` / `with-output-to-string` の内部実装として活用
- **根拠**: Gray Streams proposal (STREAM-DEFINITION-BY-USER) / SBCL Gray streams
- **難易度**: Medium

#### FR-602: Bivalent Streams (バイバレントストリーム)

- **対象**: `packages/vm/src/stream.lisp`
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

- **対象**: `packages/vm/src/io.lisp` (新ファイル `packages/vm/src/pprint.lisp`)
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

- **対象**: `packages/vm/src/io.lisp`, `packages/vm/src/vm.lisp`
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

- **対象**: `packages/parse/src/cl/lexer.lisp`, `packages/vm/src/vm.lisp`
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

- **対象**: `packages/vm/src/vm.lisp` (新ファイル `packages/vm/src/time.lisp`)
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

- **対象**: `packages/vm/src/primitives.lisp` (新ファイル `packages/vm/src/random.lisp`)
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

- **対象**: `packages/vm/src/vm.lisp`, `packages/cli/src/main.lisp`
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

- **対象**: `packages/compile/src/cps.lisp`, `packages/compile/src/codegen.lisp`
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

- **対象**: `packages/vm/src/vm-run.lisp`, `packages/runtime/src/runtime.lisp`
- **現状**: 深い再帰でホストCLのスタックオーバーフローが発生し、Lispレベルのエラーでなくクラッシュ
- **内容**:
  - `*max-call-stack-depth*` — 最大呼び出し深さ（デフォルト10000）
  - 各 `vm-call` でデプスカウンタをインクリメント、閾値で `stack-overflow` conditionを signal
  - ネイティブバックエンド（FR-571）ではガードページ（`mmap`で無効ページ配置）
  - `sb-kernel:*max-trace-depth*` 相当のデバッガ制御
- **根拠**: SBCL `sb-kernel:stack-overflow` / JVM `-Xss` スタックサイズ指定
- **難易度**: Medium

#### FR-617: OOM Condition / GC Pressure Threshold (メモリ不足ハンドリング)

- **対象**: `packages/runtime/src/gc.lisp`, `packages/runtime/src/heap.lisp`
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

- **対象**: `packages/vm/src/vm.lisp`
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

- **対象**: `packages/vm/src/vm.lisp` (`vm-intern-symbol`, `vm-find-package`)
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

- **対象**: `packages/vm/src/vm.lisp`
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

- **対象**: `packages/parse/src/cl/lexer.lisp`
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

- **対象**: `packages/vm/src/format.lisp` (または `packages/vm/src/io.lisp`)
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

- **対象**: `packages/cli/src/main.lisp`, `packages/vm/src/vm.lisp`
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

- **対象**: `packages/vm/src/array.lisp`, `packages/vm/src/list.lisp`
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

- **対象**: `packages/vm/src/array.lisp`
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

- **対象**: `packages/vm/src/array.lisp`
- **内容**:
  - `(make-array n :displaced-to base-array :displaced-index-offset k)` — 別配列の一部を共有
  - `aref`/`aset` が透過的にベース配列のオフセット位置にアクセス
  - `array-displacement` — ベース配列とオフセットの取得
  - 多次元配列のスライス操作に利用（行列の行ベクタ抽出等）
- **根拠**: ANSI CL 15.1.2。数値計算・行列演算の基盤
- **難易度**: Hard

#### FR-633: Bit Arrays / Bit Vectors (ビット配列)

- **対象**: `packages/vm/src/array.lisp`
- **現状**: `bit-vector-p` は部分実装。ビット演算命令なし
- **内容**:
  - `(make-array n :element-type 'bit)` — ビットベクタ（64bits/ワードで圧縮格納）
  - `bit-and`/`bit-or`/`bit-xor`/`bit-eqv`/`bit-nand`/`bit-nor`/`bit-andc1`/`bit-andc2`/`bit-orc1`/`bit-orc2`/`bit-not`
  - `bit`/`sbit` — ビットベクタのインデックスアクセス
  - ビット演算は`unsigned-long`単位で実装し、要素ループを最大64倍高速化
- **根拠**: ANSI CL 15.3。集合演算・フィルタマスク・GCビットマップに活用
- **難易度**: Medium

#### FR-634: Array Dimension Queries (配列次元問い合わせ)

- **対象**: `packages/vm/src/array.lisp`
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

- **対象**: `packages/vm/src/io.lisp`, `packages/vm/src/format.lisp`
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

- **対象**: `packages/vm/src/io.lisp`, `packages/vm/src/format.lisp`
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

- **対象**: `packages/vm/src/io.lisp`
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

- **対象**: `packages/vm/src/conditions.lisp`
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

- **対象**: `packages/vm/src/conditions.lisp`
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

- **対象**: `packages/vm/src/conditions.lisp`, `packages/cli/src/main.lisp`
- **内容**:
  - `*debugger-hook*` — デバッガ起動時に呼ばれるフック関数 `(lambda (condition hook) ...)`
  - `invoke-debugger condition` — デバッガを明示的に起動
  - `break &optional format-string &rest args` — デバッガに入る（`continue`リスタート付き）
  - `*break-on-signals*` — 特定conditionでbreak
  - デバッガレベル: `*debugger-level*` のネスト管理
- **根拠**: ANSI CL 9.2 / SBCL debugger。本番バグの対話的修復
- **難易度**: Hard

#### FR-646: Condition Report / Print-Object Integration

- **対象**: `packages/vm/src/conditions.lisp`, `packages/vm/src/io.lisp`
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

- **対象**: `packages/vm/src/strings.lisp`
- **現状**: `char=`/`char/=`のみ実装。大小比較・ケース非依存比較なし
- **内容**:
  - ケース依存: `char<`/`char>`/`char<=`/`char>=`
  - ケース非依存: `char-equal`/`char-not-equal`/`char-lessp`/`char-greaterp`/`char-not-greaterp`/`char-not-lessp`
  - 全て可変引数対応: `(char< a b c)` → `(and (char< a b) (char< b c))`
  - Unicode照合順序（FR-591 UCA）との統合
- **根拠**: ANSI CL 13.2 Character Comparisons
- **難易度**: Easy

#### FR-651: Character Name Functions (文字名関数)

- **対象**: `packages/vm/src/strings.lisp`, `packages/parse/src/cl/lexer.lisp`
- **現状**: `#\Space`/`#\Newline` 等のリードは実装済みだが、`char-name`/`name-char`なし
- **内容**:
  - `char-name character` → 名前文字列 (`#\Space` → `"Space"`, `#\A` → `nil`)
  - `name-char name` → 文字 (`"Space"` → `#\Space`, `"Newline"` → `#\Newline`)
  - 標準名前: `Space`, `Newline`, `Tab`, `Return`, `Rubout`, `Backspace`, `Page`, `Null`, `Altmode`, `Delete`, `Escape`
  - Unicode拡張名: U+XXXX形式の名前もサポート
- **根拠**: ANSI CL 13.2
- **難易度**: Easy

#### FR-652: String Trim Functions (文字列トリム関数)

- **対象**: `packages/vm/src/strings.lisp`
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

- **対象**: `packages/vm/src/strings.lisp`
- **現状**: `string-upcase`/`string-downcase`/`string-capitalize`は非破壊版のみ
- **内容**:
  - `nstring-upcase`/`nstring-downcase`/`nstring-capitalize` — in-place変換
  - `:start`/`:end`キーワードによる部分範囲操作
  - `string-upcase`/`string-downcase`/`string-capitalize` にも`:start`/`:end`を追加
- **根拠**: ANSI CL 16.2
- **難易度**: Easy

#### FR-654: String Comparison Functions (文字列比較関数)

- **対象**: `packages/vm/src/strings.lisp`
- **現状**: `string=`/`string/=`のみ
- **内容**:
  - ケース依存: `string<`/`string>`/`string<=`/`string>=`
  - ケース非依存: `string-equal`/`string-not-equal`/`string-lessp`/`string-greaterp`/`string-not-greaterp`/`string-not-lessp`
  - 全て `:start1`/`:end1`/`:start2`/`:end2` キーワード対応
  - 不一致位置のインデックスを返す（`string<` は最初の相違点の位置）
- **根拠**: ANSI CL 16.2 String Comparisons
- **難易度**: Easy

---

### Phase 35 — 並行性・スレッド基盤

#### FR-190: Concurrent / Incremental GC

- **対象**: `packages/runtime/src/gc.lisp`
- **現状**: Stop-the-world GC（minor: `gc.lisp:200-263`、major: `gc.lisp:331-392`）。SATBバリアは実装済みだが並行マーキング未対応
- **内容**: major GCのマーキングフェーズを並行化。SATB事前書き込みバリア（既存`gc.lisp:282-289`）を活用してミューテータと並行してマーク。スイープも並列化可能
- **根拠**: Go/JVM/ZGC はすべて並行GC。STWポーズ時間がヒープサイズに比例する現状は大規模プログラムで致命的
- **難易度**: Very Hard

#### FR-191: Atomic Operations (CAS, Memory Fences)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`, `packages/vm/src/vm.lisp`
- **現状**: アトミック命令ゼロ。x86-64にmfence/lfence/sfence、AArch64にdmb/dsb/isbの生成なし
- **内容**: `vm-cas`（compare-and-swap）、`vm-atomic-add`命令を追加。ネイティブバックエンドで`LOCK CMPXCHG`（x86-64）、`LDXR/STXR`（AArch64）を生成。メモリフェンス命令も追加
- **根拠**: ロックフリーデータ構造・並行GC（FR-190）の前提インフラ
- **難易度**: Hard

#### FR-192: Thread-Local Storage (TLS)

- **対象**: `packages/vm/src/vm.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: `vm-state`はシングルインスタンス前提（`vm.lisp:342-372`）。全レジスタ・ヒープ・グローバル変数が共有
- **内容**: per-thread `vm-state`分離: 各スレッドが独自のレジスタファイル・コールスタック・GCルートを持つ。special変数のper-thread binding（CLの動的束縛セマンティクス保持）
- **根拠**: SBCL/CCLはすべてper-thread binding stack。マルチスレッドLispの基本要件
- **難易度**: Very Hard

#### FR-193: Thread-Safe Global Variable Access

- **対象**: `packages/vm/src/vm.lisp`
- **現状**: `vm-set-global`（`vm.lisp:991-996`）と`vm-get-global`（`vm.lisp:998-1005`）がロックなしでhash-tableを直接操作
- **内容**: グローバル変数ストアにread-write lockまたはCAS-based concurrent hash tableを適用。`function-registry`（line 360）、`class-registry`（line 353）も同様に保護
- **根拠**: 複数スレッドからの`defvar`/`setf`がデータ競合を引き起こす
- **難易度**: Medium

---

### Phase 56 — 軽量並行性・スケジューラ

#### FR-257: Green Threads / M:N Threading (軽量スレッド)

- **対象**: `packages/vm/src/vm.lisp`, `packages/runtime/src/` (新規 scheduler.lisp)
- **現状**: `vm.lisp:342-372` — `vm-state`はシングルトン。OSスレッドのみ、ユーザースペースタスクスケジューラなし。FR-192（TLS）は定義済みだが軽量スレッド基盤なし
- **内容**: M:N スレッドモデル（M個のOSスレッド上にN個の軽量タスクをマルチプレクス）。協調プリエンプション（safepoint FR-233でyield）。FR-221（限定継続）上にコンテキストスイッチを構築。work-stealing対応のランキュー
- **根拠**: Go goroutines / Erlang processes / Tokio tasks。高並行性サーバーワークロードに必須
- **難易度**: Very Hard

#### FR-258: Work-Stealing Scheduler for Parallel GC (GC用ワークスティーリング)

- **対象**: `packages/runtime/src/gc.lisp`
- **現状**: FR-190（並行GC）は並列マーキングを定義しているがタスク分配戦略なし。マークスタックの負荷分散メカニズムなし
- **内容**: 各GCスレッドにwork-stealing deque。マークスタックが空になったスレッドは他スレッドのdequeから盗む。Chase-Lev dequeアルゴリズム。アイドルスレッドの再分配
- **根拠**: Java G1 GC / OpenJDK CMS / Go GC。並列マーキング効率化に不可欠
- **難易度**: Hard

#### FR-259: GC Pause Time Budget / Incremental Scheduling (GCポーズ予算)

- **対象**: `packages/runtime/src/gc.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: STW GC（`gc.lisp:200-263,331-392`）にポーズ時間制御なし。ヒープサイズに比例してポーズ時間が線形増加
- **内容**: `*max-gc-pause-ms*`パラメータ（デフォルト10ms）。マーキング/スイープを複数インクリメンタルフェーズに分割し、各フェーズで予算超過検査。アロケーション速度に基づくGCペーシング（Go GOGC方式）
- **根拠**: Go runtime GOGC / HotSpot -XX:MaxGCPauseMillis / ZGC。大規模プログラムのポーズスケーリング対策
- **難易度**: Hard

#### FR-260: Lock Elision via Intel RTM (ロック省略)

- **対象**: `packages/emit/src/x86-64-codegen.lisp`
- **現状**: FR-191でアトミック操作（CAS, メモリフェンス）のみ定義。トランザクショナルメモリなし
- **内容**: ロック取得前に`XBEGIN`でトランザクション開始を試行。成功時はロック取得をスキップしクリティカルセクションを投機実行。`XABORT`時はフォールバックで従来ロックを使用。非競合パスで25-50%高速化
- **根拠**: Intel Haswell以降のRTM / AMD Ryzen TSX。非競合ロックのファストパス最適化
- **難易度**: Medium

---

### Phase 60 — 構造化並行性・タスクモデル

#### FR-280: Structured Concurrency（構造化並行性）

- **対象**: 新規`packages/runtime/src/task.lisp`, `packages/vm/src/vm.lisp`
- **現状**: FR-257（Green Threads）でM:Nスレッドは定義済みだが、タスクのライフタイムとスコープが未管理。タスクリークやエラー伝播の仕組みなし
- **内容**: タスクグループ（nursery）API: `with-task-group`スコープ内でサブタスクを生成し、グループが終了する前に全サブタスクの完了を保証。サブタスクのパニック/エラーを親タスクへ伝播。キャンセルトークンによる協調的キャンセル
- **根拠**: Swift Structured Concurrency (SE-0304) / Python anyio / Kotlin structured concurrency。タスクリークをコンパイル時に防止できる唯一のモデル
- **難易度**: Hard

#### FR-281: Async/Await（非同期構文）

- **対象**: `packages/expand/src/macros-stdlib.lisp`, `packages/vm/src/vm.lisp`, 新規`packages/runtime/src/async.lisp`
- **現状**: 同期実行のみ。CPS変換（`packages/compile/src/cps.lisp`）はあるが非同期スケジューリングと未接続
- **内容**: `async`/`await`マクロ。`async`はFuture/Promiseオブジェクトを返すタスクとしてコンパイル。`await`は現在のタスクをサスペンドしスケジューラに制御を返す。CLのCPS変換基盤を流用してコールバック地獄を回避
- **根拠**: Rust async/await / JavaScript ES2017 / Python asyncio。I/Oバウンドな並行性の標準パターン
- **難易度**: Very Hard

#### FR-282: Channels（チャネル / CSP）

- **対象**: 新規`packages/runtime/src/channel.lisp`, `packages/vm/src/vm.lisp`
- **現状**: スレッド間通信なし。共有メモリのみ
- **内容**: 型付きチャネル（バッファあり/なし両対応）。`chan-send`/`chan-recv`プリミティブ。`select`文（複数チャネルの多重待ち）。クローズセマンティクス（送信側クローズ→受信側が残余メッセージを消費後にnil受け取り）
- **根拠**: Go channels / Rust std::sync::mpsc / Clojure core.async。共有メモリより安全なメッセージパッシング
- **難易度**: Hard

#### FR-283: Future/Promise（先物/約束）

- **対象**: 新規`packages/runtime/src/future.lisp`
- **現状**: 非同期計算の結果を表現するオブジェクトなし
- **内容**: `make-future`/`future-resolve`/`future-reject`/`future-await`/`future-then`（コールバックチェーン）。`all-futures`（全解決待ち）、`any-future`（最初の解決待ち）。FR-281（async/await）のコア実装として利用
- **根拠**: JavaScript Promise / Scala Future / C++ std::future。非同期値の第一級オブジェクト化
- **難易度**: Medium

---

### Phase 61 — アクターモデル・メッセージパッシング

#### FR-290: Actor Model（アクターモデル）

- **対象**: 新規`packages/runtime/src/actor.lisp`, `packages/vm/src/vm.lisp`
- **現状**: 状態共有はグローバルハッシュテーブルのみ。アクターの概念なし
- **内容**: `define-actor`マクロ: per-actor メールボックス + 状態 + 動作定義。`actor-send`/`actor-receive`プリミティブ。supervisorツリー（Erlang OTP風）: `one-for-one`/`all-for-one`再起動戦略。`monitor`/`link`によるクラッシュ通知
- **根拠**: Erlang/OTP / Akka / Pony。共有状態ゼロでの並行性。フォールトトレランスの標準パターン
- **難易度**: Very Hard

#### FR-291: Mailbox Scheduler with Priority（優先度付きメールボックス）

- **対象**: 新規`packages/runtime/src/scheduler.lisp`（FR-257 scheduler.lisに統合可能）
- **現状**: FR-257のwork-stealing runqueueは優先度なし
- **内容**: アクターへのメッセージキューに優先度レベル（`:high`/`:normal`/`:low`）。優先度付きwork-stealing heap。タイムアウト付き`receive`（`after`節）
- **根拠**: Erlang message passing priority / Tokio task priorities。レイテンシクリティカルなメッセージの遅延抑制
- **難易度**: Hard

---

### Phase 62 — ソフトウェアトランザクショナルメモリ

#### FR-300: Software Transactional Memory (STM)

- **対象**: 新規`packages/runtime/src/stm.lisp`, `packages/vm/src/vm.lisp`
- **現状**: ロック/CASのみ。コンポーザブルなアトミックセクションなし
- **内容**: `atomically`マクロ（トランザクションスコープ）。`tvar`（transactional variable）型。楽観的並行制御: トランザクション内の読み書きをlogに記録し、コミット時に競合検出→再試行。`retry`/`orElse`（ブロッキング/代替トランザクション）
- **根拠**: Haskell STM (`Control.Concurrent.STM`) / Clojure refs/dosync。ロックなしで合成可能なアトミック操作
- **難易度**: Very Hard

#### FR-301: STM Contention Management（STM競合管理）

- **対象**: `packages/runtime/src/stm.lisp`（FR-300依存）
- **現状**: FR-300の再試行戦略未定義
- **内容**: Polka/Greedy/Karma等の競合管理ポリシー。適応的バックオフ（競合率に応じてspin→sleep）。`*stm-max-retries*`パラメータ。競合統計収集（プロファイリング用）
- **根拠**: GCC TM / Intel STM Compiler。高競合時のスループット維持
- **難易度**: Hard

---

### Phase 63 — 非同期I/O・イベントループ

#### FR-310: io_uring Integration（Linux非同期I/O）

- **対象**: 新規`packages/runtime/src/io-uring.lisp`, `packages/runtime/src/runtime.lisp`
- **現状**: `rt-open-file`/`rt-read-file`はブロッキング`(open ...)`/`(read-sequence ...)`（`runtime.lisp:549-594`）
- **内容**: `io_uring_setup`/`io_uring_enter`syscallラッパー（FFI）。SQE（Submission Queue Entry）への`read`/`write`/`fsync`/`accept`/`connect`/`sendmsg`登録。CQE（Completion Queue Entry）ポーリング。FR-281（async/await）との統合: I/O完了でFutureを解決
- **根拠**: Linux 5.1+ io_uring。システムコールオーバーヘッドをゼロに近づける。Node.js libuv / Rust tokio-uring が採用
- **難易度**: Very Hard

#### FR-311: kqueue / IOCP Abstraction（macOS/Windows非同期I/O）

- **対象**: 新規`packages/runtime/src/event-loop.lisp`
- **現状**: 非同期I/Oバックエンド抽象化なし
- **内容**: イベントループ抽象レイヤ: macOS/BSDs→`kqueue`、Linux→`epoll`/`io_uring`、Windows→IOCP。`register-fd`/`deregister-fd`/`wait-events`統一API。FR-310（io_uring）のLinux固有実装を吸収
- **根拠**: libuv event loop abstraction / Rust mio。クロスプラットフォーム非同期I/Oの標準手法
- **難易度**: Hard

#### FR-312: Async DNS / Timer（非同期DNS解決・タイマー）

- **対象**: `packages/runtime/src/event-loop.lisp`（FR-311依存）
- **現状**: DNSなし。タイマーはスリープのみ
- **内容**: 非ブロッキングDNS解決（`getaddrinfo_a`またはスレッドプール方式）。高精度タイマー（`timerfd_create`/`kqueue EVFILT_TIMER`）。`sleep-async`/`timeout-after`プリミティブ。FR-281（async/await）と統合
- **根拠**: Tokio `time::sleep` / Node.js `setTimeout`。非同期ランタイムの基本コンポーネント
- **難易度**: Medium

---

### Phase 64 — ロックフリー・メモリ安全並行性

#### FR-320: Epoch-Based Reclamation (EBR)（エポックベースメモリ回収）

- **対象**: 新規`packages/runtime/src/ebr.lisp`, `packages/runtime/src/gc.lisp`
- **現状**: GCはSTW（FR-190）。ロックフリーデータ構造の安全な解放機構なし
- **内容**: グローバルエポックカウンタ（0/1/2循環）。スレッドがクリティカルセクションに入る前に現在エポックを記録。全スレッドがエポックEを経験した後、エポックE-2のオブジェクトを安全に解放。`with-epoch`スコープマクロ
- **根拠**: crossbeam-epoch (Rust) / Folly hazard pointers。GCなしでロックフリー構造を安全に解放する最も軽量な手法
- **難易度**: Hard

#### FR-321: Hazard Pointers（ハザードポインタ）

- **対象**: 新規`packages/runtime/src/hazard.lisp`
- **現状**: FR-320（EBR）と相補的。読み取り中オブジェクトの保護機構なし
- **内容**: per-thread ハザードポインタ配列。読み取り前にポインタをハザードスロットに登録、解放前に全スレッドのハザードポインタをスキャン。`hp-protect`/`hp-retire`API。EBRより正確だがオーバーヘッドがやや大きい
- **根拠**: M. Michael (IBM) / Rust `haphazard` crate。超高競合下でのEBR代替
- **難易度**: Hard

#### FR-322: Lock-Free Data Structures（ロックフリーデータ構造）

- **対象**: 新規`packages/runtime/src/lockfree.lisp`（FR-320/321依存）
- **現状**: 全データ構造（ハッシュテーブル、リスト等）がシングルスレッド前提
- **内容**: Michael-Scott非ブロッキングキュー（MPMC）。ロックフリーハッシュマップ（split-ordered lists または Cliff Click方式）。ロックフリースキップリスト。FR-345（Atomics）のCAS/FAA命令で実装
- **根拠**: Java ConcurrentLinkedQueue / Rust DashMap / Go sync.Map。共有データ構造のスケーラビリティ
- **難易度**: Very Hard

---

### Phase 65 — SIMD並列性・ベクトル化

#### FR-330: Auto-Vectorization（自動ベクトル化）

- **対象**: `packages/optimize/src/optimizer.lisp`, `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: ループは逐次実行。SIMDレジスタへのアクセスなし
- **内容**: ループベクトル化パス: 依存解析→SIMDレーン数（x86-64: AVX2=256bit/AVX-512=512bit、AArch64: SVE2=可変長）計算→`VMOVDQU`/`VPADDD`等生成。スカラーエピローグ。`declare (type (simple-array single-float (*)) v)`アノテーション活用
- **根拠**: LLVM LoopVectorizePass / GCC Tree-loop-vectorize。数値計算ループの2-16x高速化
- **難易度**: Very Hard

#### FR-331: SIMD Intrinsics API（SIMDイントリンシクスAPI）

- **対象**: 新規`packages/vm/src/vm-simd.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **現状**: SIMDレジスタ型・命令なし
- **内容**: `vm-simd-load`/`vm-simd-store`/`vm-simd-add`/`vm-simd-mul`/`vm-simd-fma`命令。x86-64 SSE4.2/AVX2/AVX-512マッピング。AArch64 NEON/SVE2マッピング。`simd-float32x8`等の型
- **根拠**: C `<immintrin.h>` / Rust std::arch。手動最適化が必要な暗号・ML・信号処理ルーティン向け
- **難易度**: Hard

#### FR-332: Parallel Array Operations（並列配列操作）

- **対象**: `packages/expand/src/macros-stdlib.lisp`, 新規`packages/runtime/src/parallel.lisp`
- **現状**: `map`/`reduce`/`dotimes`は逐次
- **内容**: `pmap`/`preduce`（自動並列化版）。`parallel-for`マクロ（ループ本体がデータ並列）。チャンク分割→ワーカースレッド投入→結果統合。閾値以下は逐次フォールバック。FR-258（work-stealing scheduler）活用
- **根拠**: Java parallel streams / Rust rayon / Haskell `par`/`pseq`。関数型並列性の標準インターフェース
- **難易度**: Hard

---

### Phase 66 — コルーチン・ファイバー

#### FR-340: Stackful Coroutines / Fibers（スタックフルコルーチン）

- **対象**: 新規`packages/runtime/src/fiber.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **現状**: FR-221（限定継続）がコンテキストスイッチの基盤として定義済みだが専用スタック管理なし
- **内容**: per-fiber 独立スタック（デフォルト8KB、オンデマンド拡張）。`fiber-create`/`fiber-resume`/`fiber-yield`。x86-64: `jmp`ベースのスタックスイッチ（`%rsp`/`%rbp`入れ替え）。AArch64: `sp`/`lr`スイッチ。FR-257（Green Threads）のM:Nモデルで実際のファイバー実装として使用
- **根拠**: Boost.Context / Rust async fn / ucontext_t。軽量スイッチング（~10ns vs スレッド~1μs）
- **難易度**: Very Hard

#### FR-341: Stackless Coroutines（スタックレスコルーチン）

- **対象**: `packages/compile/src/cps.lisp`, `packages/expand/src/macros-stdlib.lisp`
- **現状**: CPS変換基盤あり。`yield`を生成する構文なし
- **内容**: `generator`マクロ: `yield`を含む関数を状態機械にCPS変換。状態は固定サイズのクロージャ（スタック不要）。`next`で再開。FR-281（async/await）の`await`ポイントもスタックレスコルーチンとしてコンパイル可能
- **根拠**: Python `yield` / C++20 coroutines / Kotlin suspend fun。スタックレスでメモリ効率が高い
- **難易度**: Hard

---

### Phase 67 — NUMA・メモリトポロジー最適化

#### FR-355: NUMA-Aware Memory Allocation（NUMA対応メモリアロケーション）

- **対象**: `packages/runtime/src/heap.lisp`, `packages/runtime/src/gc.lisp`
- **現状**: ヒープは単一`rt-heap`構造体（`heap.lisp`）。NUMAノード分離なし
- **内容**: NUMAノードごとのヒープアリーナ。`numa_alloc_local`/`numa_alloc_onnode`（libnuma FFI）。GCスレッドをNUMAノードにピン留め。リモートアクセス率の統計収集
- **根拠**: Linux libnuma / JVM `-XX:+UseNUMA`。多ソケットサーバーでメモリ帯域幅を最大2倍活用
- **難易度**: Hard

#### FR-356: False Sharing Elimination（偽共有排除）

- **対象**: `packages/vm/src/vm.lisp`, `packages/runtime/src/heap.lisp`
- **現状**: per-threadデータのキャッシュライン整列なし
- **内容**: `vm-state`のカウンタフィールドを64バイト境界にパディング。ロックフリーキュー（FR-322）のhead/tailを別キャッシュラインに配置。`alignas(64)`相当のアロケーター指定
- **根拠**: Linux kernel `____cacheline_aligned` / Java `@Contended`。コア間の帯域幅浪費を防止（最大40%スループット改善事例あり）
- **難易度**: Medium

---

### Phase 68 — デバッグ・可観測性・安全性

#### FR-360: Thread Sanitizer Integration（スレッドサニタイザー統合）

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/compile/src/codegen.lisp`
- **現状**: データ競合の検出機構ゼロ
- **内容**: TSanシャドウメモリ instrumentation: 全メモリ読み書きの前後に`__tsan_read`/`__tsan_write`呼び出しを挿入。Lock acquire/releaseの`__tsan_mutex_lock`通知。`--sanitize=thread`コンパイルフラグで有効化。報告フォーマット: ファイル名・行番号・スタックトレース
- **根拠**: Clang/GCC TSan / Go race detector。データ競合を実行時に確実検出
- **難易度**: Hard

#### FR-361: Deadlock Detector（デッドロック検出器）

- **対象**: 新規`packages/runtime/src/deadlock.lisp`
- **現状**: ロック順序検証なし
- **内容**: ロック獲得グラフの動的構築（wait-for graph）。サイクル検出（DFS）でデッドロックを実行時報告。`--detect-deadlock`フラグで有効化。検出時はスタックトレース付きエラーレポート
- **根拠**: Valgrind helgrind / Go deadlock detector（goroutine全停止検出）。本番前のデッドロック発見
- **難易度**: Medium

#### FR-362: Async Profiler Integration（非同期プロファイラー統合）

- **対象**: `packages/runtime/src/`, `packages/cli/src/main.lisp`
- **現状**: プロファイリング機構なし（FR-235 profiling定義済みだがCPU専用）
- **内容**: 非同期タスクのウォールクロック時間計測。タスク生成/終了/ブロッキングイベントの記録。FlameGraph出力（`perf`互換）。チャネル待ち時間・ロック競合のヒートマップ。`cl-cc profile <file>`サブコマンド
- **根拠**: async-profiler (Java) / Tokio Console / Go pprof。非同期コードのボトルネック特定
- **難易度**: Hard

#### FR-363: Cooperative Preemption Safepoints（協調的プリエンプションセーフポイント）

- **対象**: `packages/emit/src/x86-64-codegen.lisp`, `packages/runtime/src/scheduler.lisp`
- **現状**: FR-233（safepoint）は定義済みだがスケジューラとの接続なし
- **内容**: ループバックエッジ・関数エントリに`safepoint_check`命令挿入（カウンタデクリメント→ゼロでyield）。GCセーフポイントと兼用。スレッド間で公平なスケジューリング実現。コンパイラフラグ`--safepoint-interval=N`（デフォルト1000命令）
- **根拠**: Go runtime preemption (go1.14 asynchronous preemption) / JVM safepoint。ロングランニングループの starvation防止
- **難易度**: Hard

---

### Phase 72 — 並行性・OS統合

#### FR-345: Atomic Operations (アトミック操作)

- **対象**: `packages/vm/src/vm.lisp`, 新規`packages/vm/src/atomics.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **現状**: 全VM状態アクセスがシングルスレッドのハッシュテーブル操作（`vm-state-registers`/`vm-global-vars`/`vm-function-registry`、`vm.lisp:343-371`で`make-hash-table`）。`vm-heap-alloc`（`vm.lisp:408-412`）は非アトミック`incf`
- **内容**: `vm-cas`/`vm-atomic-add`/`vm-atomic-load`/`vm-atomic-store`命令。ランタイム`rt-cas`/`rt-atomic-incf`。x86-64 `LOCK CMPXCHG`/`LOCK XADD`エミッション。FR-191（Lock Elision）の前提
- **根拠**: Java `java.util.concurrent.atomic` / C++ `<atomic>`。並行プログラミングの基盤
- **難易度**: Medium

#### FR-346: Memory Ordering Fences (メモリオーダリングフェンス)

- **対象**: `packages/mir/src/target.lisp`, `packages/emit/src/x86-64-codegen.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: フェンス/バリア命令なし。`target-desc`の`features`（`target.lisp:38-41`）にメモリモデル記述なし
- **内容**: `vm-fence`命令（`:acquire`/`:release`/`:seq-cst`セマンティクス）。x86-64 `MFENCE`/`LFENCE`/`SFENCE`。AArch64 `DMB`/`DSB`/`ISB`。FR-345（アトミック操作）と相補的
- **根拠**: C++11 memory model / Java Memory Model。弱順序アーキテクチャ（AArch64）での正確性保証
- **難易度**: Hard

#### FR-347: Thread-Safe VM Registries (スレッドセーフVMレジストリ)

- **対象**: `packages/vm/src/vm.lisp`, `packages/runtime/src/runtime.lisp`
- **現状**: `vm-function-registry`（`vm.lisp:361`）、`vm-global-vars`（`vm.lisp:364`）、`vm-class-registry`（`vm.lisp:353`）が全て非同期ハッシュテーブル。`rt-intern`（`runtime.lisp:398`）もロックなし
- **内容**: 読み書きロックまたはロックフリー並行ハッシュテーブル。`intern`/`find-symbol`の同期化。FR-192（TLS）・FR-345（Atomics）に依存
- **根拠**: JVM ConcurrentHashMap / SBCL package lock。マルチスレッド環境での正確性
- **難易度**: Hard

#### FR-348: OS Signal to CL Condition Mapping (OSシグナル→CLコンディション変換)

- **対象**: `packages/runtime/src/runtime.lisp`, 新規`packages/runtime/src/signals.lisp`
- **現状**: `runtime.lisp:469-498`にCLレベルcondition関数のみ。OSレベルシグナルハンドラなし。SIGINT→break、SIGSEGV→storage-condition、SIGFPE→arithmetic-error等の変換なし。CLIのREPLはread-line EOF依存（`main.lisp:274`）
- **内容**: POSIX `sigaction`ベースのハンドラ登録（FFI経由）。SIGFPE→`arithmetic-error`、SIGSEGV→`storage-condition`、SIGINT→`break`/デバッガ。ネイティブコード用ガードページによるスタックオーバーフロー検出
- **根拠**: SBCL signal handling / CCL `%install-signal-handler`。ハードウェア例外のCL統合
- **難易度**: Hard

#### FR-349: Memory-Mapped I/O (メモリマップドI/O)

- **対象**: `packages/runtime/src/runtime.lisp`, 新規`packages/runtime/src/mmap.lisp`
- **現状**: ファイルI/OはCLストリームのみ（`runtime.lisp:549-594`の`rt-open-file`は`(open path ...)`）。`mmap`/`munmap`/`mprotect`なし
- **内容**: `rt-mmap`/`rt-munmap`/`rt-mprotect`（FFIまたはsyscallエミッション経由）。大規模データのメモリマップドアクセス。ネイティブランタイムのヒープバッキングにmmap使用
- **根拠**: SBCL `sb-posix:mmap` / GC heap backing via mmap。大規模ファイル処理の標準手法
- **難易度**: Medium

#### FR-350: Core Image Save/Restore (コアイメージ保存/復元)

- **対象**: 新規`packages/runtime/src/image.lisp`, `packages/vm/src/vm.lisp`, `packages/cli/src/main.lisp`
- **現状**: VM状態（`vm.lisp:342-373`）にハッシュテーブル・CLOSオブジェクト・ラベル付きクロージャが含まれるが全て非シリアライザブル。SBCL `save-lisp-and-die`相当の機能なし
- **内容**: VM状態シリアライザ（レジスタ・ヒープ・関数レジストリ・グローバル変数・クラスレジストリ）。イメージファイルフォーマット定義。`cl-cc save-image`/`cl-cc load-image` CLIコマンド。起動高速化
- **根拠**: SBCL `save-lisp-and-die` / ECL `ext:save-lisp-and-die`。デプロイ・配布の標準手法
- **難易度**: Hard

#### FR-351: Sandboxing / Resource Limits (サンドボックス/リソース制限)

- **対象**: `packages/vm/src/vm.lisp`, `packages/cli/src/main.lisp`
- **現状**: VM実行に命令数制限・メモリ上限・タイムアウトなし。`vm-heap-alloc`（`vm.lisp:408`）は枯渇時のみエラー。ソフトリミットなし
- **内容**: 命令カウント制限、ウォールクロックタイムアウト、ヒープサイズ上限（設定可能）、ファイルI/Oホワイトリスト。ネイティブコード向け`setrlimit`
- **根拠**: Cloudflare Workers / Deno / Lua sandbox。信頼されないコードの安全な実行
- **難易度**: Medium

#### FR-352: Hot Code Reloading (ホットコードリロード)

- **対象**: `packages/vm/src/vm.lisp`, `packages/pipeline/pipeline.lisp`
- **現状**: `vm-register-function`（`vm.lisp:988`）はベアな`(setf (gethash ...))`で、バージョニング・ロールバック・アトミックスワップなし
- **内容**: アトミック関数置換（旧バージョン保持）。実行中呼び出しの完了待ち後のスワップ。モジュールレベルのホットリロード（依存追跡付き）
- **根拠**: Erlang hot code loading / SBCL interactive redefinition。ライブシステムの更新
- **難易度**: Medium

#### FR-353: Foreign Thread Callbacks (外部スレッドコールバック)

- **対象**: `packages/runtime/src/runtime.lisp`, `packages/emit/src/x86-64-codegen.lisp`
- **現状**: FFI機構なし。`macho.lisp:44`は`+mh-noundefs+`（動的リンクなし）。ELFにも`.dynsym`/`.dynamic`セクションなし。コールバックトランポリン生成なし
- **内容**: C呼び出し可能な関数ポインタ（CLクロージャをラップするトランポリン）生成。外部スレッド用スレッドローカルVM状態初期化。FR-194（FFI基盤）・FR-196（動的リンク）に依存
- **根拠**: CFFI `defcallback` / JNA `Callback`。C/C++ライブラリとの双方向相互運用
- **難易度**: Very Hard

---

### Phase 69 — 基本同期プリミティブ

#### FR-370: Mutex / RWLock / Semaphore（基本排他制御）

- **対象**: 新規`packages/runtime/src/sync.lisp`, `packages/vm/src/vm.lisp`
- **現状**: FR-191/FR-345でCAS・アトミック操作は定義済みだが、高レベルの排他プリミティブなし。`vm-state`のレジスタアクセスに排他制御を掛ける手段が言語レベルで未提供
- **内容**: `make-mutex`/`mutex-lock`/`mutex-unlock`/`with-mutex`マクロ。`make-rwlock`/`rwlock-read-lock`/`rwlock-write-lock`（複数読み取り/単一書き込み）。`make-semaphore`/`semaphore-wait`/`semaphore-signal`（カウンティングセマフォ）。再入可能ミューテックス（`make-recursive-mutex`）。タイムアウト付きtry-lock変種。futexベース実装（Linux）/`os_unfair_lock`（macOS）
- **根拠**: POSIX `pthread_mutex_t` / Java `synchronized` / C++ `std::mutex`。高レベル同期の最基本単位。FR-322（ロックフリー構造）の代替として必須
- **難易度**: Medium

#### FR-371: Condition Variables（条件変数）

- **対象**: `packages/runtime/src/sync.lisp`（FR-370依存）
- **現状**: スレッド間のシグナリング機構なし。FR-282（チャネル）はメッセージパッシング専用で汎用条件待ちを提供しない
- **内容**: `make-condition-variable`/`condition-wait`/`condition-notify`/`condition-notify-all`。wait時にミューテックスをアトミックに解放して待機（spurious wakeup対応のloopラッパー）。`with-condition-wait`マクロ（述語ループを自動生成）。Linux `futex(FUTEX_WAIT)`/macOS `pthread_cond_t`マッピング
- **根拠**: POSIX `pthread_cond_t` / Java `Object.wait`/`notify`。生産者・消費者パターン・バウンドキューの標準実装基盤
- **難易度**: Medium

#### FR-372: Barriers / Latch / CyclicBarrier（集合同期）

- **対象**: `packages/runtime/src/sync.lisp`（FR-370/371依存）
- **現状**: 複数スレッドの集合待ち機構なし。並列GC（FR-258）やParallel Array（FR-332）でワーカー完了待ちに必要
- **内容**: `make-barrier n`（N個のスレッドが`barrier-wait`を呼ぶまで全員ブロック）。`make-countdown-latch n`（カウントゼロで全待機スレッド解放、再利用不可）。`make-cyclic-barrier n`（フェーズ繰り返し可能なバリア、各フェーズ完了時にアクション実行可）。アトミックカウンタ＋条件変数で実装
- **根拠**: Java `CountDownLatch`/`CyclicBarrier`/`Phaser`。フォーク・ジョイン並列アルゴリズムの終了同期
- **難易度**: Medium

#### FR-373: Once / Lazy Initialization（一度だけの初期化）

- **対象**: `packages/expand/src/macros-stdlib.lisp`, `packages/runtime/src/sync.lisp`
- **現状**: グローバル変数の遅延初期化はシングルスレッド前提（`defvar`の初期化フォームは非保護）
- **内容**: `make-once`/`once-call fn`（fnを最大一度だけ実行、他スレッドは完了まで待機）。`define-lazy var init-form`マクロ（初アクセス時にスレッドセーフ初期化）。ダブルチェックロッキングの安全実装（FR-346メモリフェンスで保証）。`call_once`相当のFAST PATH: アトミックフラグでロックを回避
- **根拠**: C++ `std::call_once` / Go `sync.Once` / Java `volatile` double-checked locking。シングルトン・キャッシュの安全な遅延初期化
- **難易度**: Medium

---

### Phase 70 — Advanced Locking

#### FR-380: RCU（Read-Copy-Update）

- **対象**: 新規`packages/runtime/src/rcu.lisp`（FR-320 EBRと相補的）
- **現状**: FR-322のロックフリー構造は読み取り側にもCAS/アトミック操作を要求する。読み取りゼロオーバーヘッドの達成手段なし
- **内容**: `rcu-read-lock`/`rcu-read-unlock`（読み取り側: ゼロオーバーヘッド、単なるコンパイラバリア）。`rcu-assign-pointer`/`rcu-dereference`（書き込み側: 古いコピーを保持しつつ新コピーをアトミック公開）。`synchronize-rcu`（全読み取り側の猶予期間完了を待つ）。クォーセントステート検出方式（per-CPU カウンタベース）
- **根拠**: Linux kernel RCU / userspace-rcu (liburcu)。読み取り圧倒的多数のルーティングテーブル・設定レジストリに最適。読み取りコストが文字通りゼロ
- **難易度**: Very Hard

#### FR-381: MCS / CLH Lock（スケーラブルキューロック）

- **対象**: `packages/runtime/src/sync.lisp`（FR-370の内部実装として使用可能）
- **現状**: FR-370のMutexはfutexベースだが多コアでの公平性・スケーラビリティが未考慮
- **内容**: MCS Lock: per-thread ノードを連結リストでキューイング。CAS一発でテール追加、スピンは自ノードのフラグのみ（キャッシュライン競合なし）。CLH Lock: MCSの仮想版（ポインタ操作なし、前ノードのフラグをスピン）。NUMA環境での局所スピンによるキャッシュトラフィック削減
- **根拠**: J. Mellor-Crummey & M. Scott (1991) / Linux `qspinlock`（MCS変種）。コア数に対してO(1)スケールする唯一のスピンロック
- **難易度**: Hard

#### FR-382: Seqlock（シーケンスロック）

- **対象**: 新規`packages/runtime/src/sync.lisp`
- **現状**: RWLockは書き込み待ちが読み取りをブロック。書き込み優先の軽量同期なし
- **内容**: `make-seqlock`/`seqlock-read`/`seqlock-write`。書き込みは奇数シーケンスナンバーでフェンスを挟んでデータ更新後に偶数へインクリメント。読み取りは読前後のシーケンスが同じ偶数なら成功（不一致ならリトライ）。読み取りはロックなし・ライタもブロックなし
- **根拠**: Linux kernel `seqlock_t`（`jiffies`や`xtime`の更新に使用）。タイムスタンプ・統計カウンタ等の小データで最高効率
- **難易度**: Medium

#### FR-383: Priority-Inheritance Mutex（優先度継承ミューテックス）

- **対象**: `packages/runtime/src/sync.lisp`, `packages/runtime/src/scheduler.lisp`（FR-291 priority scheduler依存）
- **現状**: FR-370のMutexに優先度管理なし。優先度逆転（低優先度がロック保持中に高優先度タスクがブロック）の防止機構なし
- **内容**: ロック待ちスレッドの優先度をロック保持者に一時的に継承。解放時に元の優先度に復元。PI待ちグラフの構築（連鎖継承: A→B→C）。`PTHREAD_PRIO_INHERIT`相当のセマンティクス
- **根拠**: POSIX `PTHREAD_PRIO_INHERIT` / VxWorks PI mutex。リアルタイムシステム・組み込みのMars Pathfinder事例。優先度逆転がシステムクラッシュを引き起こした歴史的教訓
- **難易度**: Hard

---

### Phase 71 — 型システムによる並行性安全

#### FR-390: Ownership-Based Concurrency Safety（所有権ベース並行安全性）

- **対象**: `packages/type/src/`, `packages/compile/src/codegen.lisp`, `packages/expand/src/expander.lisp`
- **現状**: 型システム（`packages/type/src/`）はHindley-Milner型推論だがSend/Sync相当の並行性マーカーなし。スレッド境界を跨ぐ値の型チェックなし
- **内容**: `Send`トレイト相当: 「別スレッドへ転送可能な型」マーカー（デフォルトは推論、ミュータブル共有参照は非Send）。`Sync`トレイト相当: 「複数スレッドから安全に共有参照可能な型」マーカー。`spawn`/`chan-send`のシグネチャに`Send`境界を要求。型エラーメッセージに「スレッド安全でない型をスレッド境界を跨いで使用」の診断
- **根拠**: Rust `Send`/`Sync` / Pony reference capabilities。コンパイル時にデータ競合を証明可能に排除
- **難易度**: Very Hard

#### FR-391: Session Types for Channels（チャネルセッション型）

- **対象**: `packages/type/src/`, `packages/vm/src/vm.lisp`（FR-282チャネル依存）
- **現状**: FR-282のチャネルは型なし。通信プロトコルの順序保証がない
- **内容**: セッション型: `!Int . ?String . End`（IntをSendしてStringをReceiveして終了）の型でチャネルを型付け。デュアル型: 送信端の型が受信端の型の双対になることを型チェッカーが検証。`session-new`が送受信ペアを生成。プロトコル違反はコンパイルエラー。型レベルでデッドロックフリーを部分的に保証
- **根拠**: Frank Pfenning/Luís Caires Session Types / Rust `async-session`. プロトコル違反によるデッドロックをコンパイル時に防止
- **難易度**: Very Hard

#### FR-392: Algebraic Effects / Effect Handlers（代数的エフェクト）

- **対象**: `packages/compile/src/cps.lisp`, `packages/expand/src/macros-stdlib.lisp`, `packages/vm/src/vm.lisp`
- **現状**: CPS変換基盤あり。限定継続（FR-221）で再開可能な継続が実装済み。しかしエフェクトシステムとしての統合なし
- **内容**: `define-effect`マクロ（エフェクト宣言: `(define-effect Async (await promise))`）。`perform`（エフェクト発動: サスペンドして継続をハンドラへ渡す）。`with-handler`（エフェクトに対応する解釈器を定義: 再開・中断・変換）。async/awaitをエフェクトで再実装可能（FR-281との統合）。例外・状態・非決定性もエフェクトで表現
- **根拠**: OCaml 5 multicore effects / Koka / Eff言語。単一の機構でasync/例外/コルーチン/STMを統一的に表現できる
- **難易度**: Very Hard

---

### Phase 73 — 異種並列計算

#### FR-400: GPU Compute Offloading（GPU計算オフロード）

- **対象**: 新規`packages/emit/src/gpu-codegen.lisp`, 新規`packages/runtime/src/gpu.lisp`
- **現状**: バックエンドはx86-64/AArch64/WASMのみ。GPU実行モデルなし
- **内容**: `gpu-kernel`マクロ: 本体をGPUカーネルとしてコンパイル（グリッド/ブロック/スレッド次元指定）。Metal Shading Language（macOS）/ SPIRV（Vulkan/WebGPU）への変換バックエンド。`gpu-buffer-alloc`/`gpu-buffer-copy`/`gpu-launch-kernel`ランタイムAPI。FR-281（async/await）との統合: GPU完了イベントでFutureを解決
- **根拠**: Apple Metal / WebGPU (W3C) / CUDA / ROCm。ML推論・物理シミュレーション・暗号処理で CPU の 100x 以上の演算密度
- **難易度**: Very Hard

#### FR-401: WASM Threads + SharedArrayBuffer（WASM並行性）

- **対象**: `packages/emit/src/wasm-codegen.lisp`（`docs/wasm.md`参照）, 新規`packages/emit/src/wasm-threads.lisp`
- **現状**: WASMバックエンドはシングルスレッド前提。WASM Threads proposal（`shared`メモリ・`wait`/`notify`命令）未対応
- **内容**: `memory (shared)`フラグ付きWASMメモリ生成。`memory.atomic.wait`/`memory.atomic.notify`命令エミッション（WASM Threads proposal）。`i32.atomic.rmw.cmpxchg`等のアトミックRMW命令。FR-370（Mutex）のWASMターゲット実装（SAB + Atomics.wait）。Web Worker間での`SharedArrayBuffer`共有セマンティクス
- **根拠**: WebAssembly Threads Proposal (Phase 4) / Emscripten pthreads。ブラウザ上でのマルチスレッド計算（Figma・AutoCAD Web等が採用）
- **難易度**: Hard

---

### Phase 74 — リアクティブ・データフロー

#### FR-410: Reactive Streams / Backpressure（リアクティブストリーム・背圧）

- **対象**: 新規`packages/runtime/src/stream.lisp`（FR-282チャネル・FR-281 async依存）
- **現状**: FR-282のチャネルはバッファあり/なしのシンプルなキュー。プロデューサー速度制御（背圧）なし
- **内容**: `Publisher`/`Subscriber`/`Subscription`/`Processor`インターフェース（Reactive Streams仕様準拠）。`request(n)`セマンティクス: サブスクライバーが処理可能な要素数をパブリッシャーに通知（プル型背圧）。`map`/`filter`/`flatMap`/`merge`/`zip`オペレーター。エラー・完了シグナルの伝播。FR-281（async/await）との透過的統合
- **根拠**: Reactive Streams Specification / Project Reactor / RxJava 3 / Akka Streams。パイプライン全体の流量制御でOOMを防止
- **難易度**: Hard

#### FR-411: Async Generators / Async Iterators（非同期ジェネレーター）

- **対象**: `packages/expand/src/macros-stdlib.lisp`, `packages/compile/src/cps.lisp`（FR-341スタックレスコルーチン・FR-281 async依存）
- **現状**: FR-341の`generator`マクロは同期`yield`のみ。非同期ソース（I/O、タイマー）からの逐次生産なし
- **内容**: `async-generator`マクロ: 本体で`yield`と`await`を混在可能。`async-for`マクロ: 非同期イテレータを`await`しながら逐次消費。`aiter`プロトコル（`next`がFutureを返す）。変換オペレーター: `async-map`/`async-filter`/`async-take`/`async-collect`。FR-310（io_uring）との統合: ファイル行を非同期にストリーム
- **根拠**: Python `async for` / JavaScript `AsyncIterator` / Rust `Stream`。I/Oバウンドなデータパイプラインのコルーチン的記述
- **難易度**: Hard

#### FR-412: Context Propagation（コンテキスト伝播）

- **対象**: 新規`packages/runtime/src/context.lisp`, `packages/runtime/src/scheduler.lisp`
- **現状**: キャンセル・タイムアウト・トレースIDをタスクツリー全体に伝播する機構なし。FR-280（Structured Concurrency）のキャンセルトークンは独立したキャンセルのみ
- **内容**: `*current-context*` スレッドローカル変数。`with-context (ctx) body`: サブタスク生成時にコンテキストを自動継承。`context-deadline`（絶対時刻）・`context-cancelled-p`・`context-value key`（任意KV伝播）。タスク生成/チャネル送受信で自動伝播。`cancel-context`で木全体に協調的キャンセル伝播
- **根拠**: Go `context.Context` / OpenTelemetry Context propagation。分散トレーシング・タイムアウト制御・グレースフルシャットダウンの統一基盤
- **難易度**: Medium

---

### Phase 75 — コンパイラレベル並行最適化

#### FR-420: Parallel Compilation（並列コンパイル）

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/cli/src/main.lisp`
- **現状**: `our-load`によるファイルのシリアル処理（`pipeline.lisp`）。依存関係解析なし
- **内容**: ASCFビルドグラフ解析でファイル間依存関係を抽出。依存のないファイルをワーカースレッドプールで並列コンパイル。`*compilation-mutex*`でグローバルレジストリへの書き込みを保護。`make -jN`相当: `--parallel N`フラグ。インクリメンタルビルドキャッシュ（タイムスタンプ+コンテンツハッシュ）
- **根拠**: Rust cargo parallel codegen / GCC `make -j` / Bazel。中規模プロジェクトで壁時計時間を1/Nに短縮
- **難易度**: Hard

#### FR-421: Escape Analysis for Concurrency（並行性向けエスケープ解析）

- **対象**: `packages/compile/src/codegen.lisp`, `packages/optimize/src/optimizer.lisp`
- **現状**: スタック割り当て最適化（FR-optimize系）はあるが並行性文脈での解析なし
- **内容**: スレッドローカル証明: オブジェクトが生成スレッドの外に「エスケープ」しないことを証明→ロック不要。`chan-send`・`actor-send`に渡す値は「エスケープ」と見なす。エスケープしないオブジェクトへの可変アクセスにロック生成をスキップ。FR-390（Send/Sync型）との協調: 型システムの証明をエスケープ解析で補強
- **根拠**: JVM JIT escape analysis（`-XX:+DoEscapeAnalysis`）/ GraalVM。並行プログラムのロックオーバーヘッドをコンパイル時に排除
- **難易度**: Very Hard

#### FR-422: Lock Coarsening / Lock Elimination（ロック粗化・消去）

- **対象**: `packages/optimize/src/optimizer.lisp`
- **現状**: FR-370のロック操作は挿入された通りに生成される。冗長なロック取得/解放の除去最適化なし
- **内容**: **Lock Elimination**: エスケープ解析（FR-421）でスレッドローカルと証明されたオブジェクトへのロック呼び出しを削除。**Lock Coarsening**: 隣接するロック/アンロックペアを一つの大きなクリティカルセクションに合併（`lock A; work1; unlock A; lock A; work2; unlock A` → `lock A; work1; work2; unlock A`）。**Biased Locking**: 常に同一スレッドが取得するロックにはスレッドIDを刻印してCAS不要化（Java biased locking）
- **根拠**: JVM JIT lock optimization / GraalVM Partial Escape。synchronized/with-mutexの多用でもゼロオーバーヘッドに近づける
- **難易度**: Hard

---

### Phase 76 — 分散並行性

#### FR-430: Distributed Actors / Node Communication（分散アクター・ノード通信）

- **対象**: 新規`packages/runtime/src/cluster.lisp`（FR-290アクターモデル依存）
- **現状**: FR-290のアクターはプロセス内のみ。ノード間メッセージパッシングなし
- **内容**: ノードアドレス（`host:port:actor-id`）によるリモートアクター参照。TCP/UDP上のメッセージシリアライズ（MessagePackまたは独自バイナリフォーマット）。`remote-send`（ノード境界を透過的に越えるメッセージ送信）。ノード死活監視（`monitor`のリモート版: `:nodedown`シグナル）。ネットワーク分断時の再接続とメッセージ再送
- **根拠**: Erlang distribution protocol / Akka remoting / Ray.io。マルチマシンスケールアウトの基盤
- **難易度**: Very Hard

#### FR-431: CRDTs（無衝突複製データ型）

- **対象**: 新規`packages/runtime/src/crdt.lisp`（FR-430分散アクター依存）
- **現状**: 分散状態管理なし。競合解決機構なし
- **内容**: State-based CRDT: `g-counter`（加算のみカウンタ）、`pn-counter`（増減カウンタ）、`or-set`（観測削除セット）、`lww-register`（最終書き込み勝ちレジスタ）。`merge`操作（最小上界=join、可換・結合・冪等）。Operation-based CRDT（op-based）: 操作のブロードキャストと到着順不変性。`define-crdt`マクロで新型CRDT定義
- **根拠**: Riak / Redis CRDTs / Automerge。ネットワーク分断下でも結果整合性を保証する唯一のデータ構造クラス
- **難易度**: Very Hard

#### FR-432: Consensus Protocol（合意プロトコル）

- **対象**: 新規`packages/runtime/src/consensus.lisp`（FR-430分散アクター・FR-283 Future依存）
- **現状**: 分散状態機械・リーダー選出なし
- **内容**: **Raft実装**: リーダー選出（ランダムタイムアウト投票）、ログ複製（AppendEntries RPC）、コミット（過半数確認）、スナップショット（InstallSnapshot）。`raft-node`アクター型。`raft-propose value`（値の合意をFutureで返す）。`raft-read`（線形化読み取り）。クラスタ設定変更（メンバー追加/削除のjoint consensus）
- **根拠**: Raft論文 (Ongaro & Ousterhout 2014) / etcd / CockroachDB。分散システムで厳密な一貫性を提供する標準アルゴリズム
- **難易度**: Very Hard

---

### Phase 77 — ランタイムアロケーション・トポロジー

#### FR-440: Lock-Free Memory Allocator（ロックフリーメモリアロケーター）

- **対象**: `packages/runtime/src/heap.lisp`, 新規`packages/runtime/src/allocator.lisp`
- **現状**: `vm-heap-alloc`（`vm.lisp:408-412`）は単一カウンタへの非アトミック`incf`。複数スレッドが同時にアロケートするとデータ競合
- **内容**: per-thread スラブキャッシュ（サイズクラス別: 8B/16B/32B/64B/128B/256B/512B/1KB/4KB）。スラブが空になったときのみグローバルヒープからバルク取得（1度のロックで多数取得=ロック頻度を1/Nに低減）。解放はper-thread フリーリストに戻す。スレッド終了時に未使用スラブをグローバルプールに返却。Large object（>32KB）は直接`mmap`
- **根拠**: tcmalloc (Google) / jemalloc (Facebook) / mimalloc (Microsoft)。並行アロケーションのスケーラビリティをコア数に対してほぼ線形に保つ。malloc競合がマルチスレッドプログラムのボトルネック1位になる頻度が高い
- **難易度**: Hard

#### FR-441: CPU Affinity / Core Pinning（CPUアフィニティ・コアピン留め）

- **対象**: 新規`packages/runtime/src/topology.lisp`, `packages/runtime/src/scheduler.lisp`
- **現状**: FR-355（NUMA）はノード単位のメモリ配置を定義しているが、スレッドを特定コアに固定する機構なし
- **内容**: `thread-set-affinity thread cpu-set`（Linux `sched_setaffinity` / macOS `pthread_setaffinity_np` / Windows `SetThreadAffinityMask` FFI）。`cpu-topology`構造体: ソケット/NUMAノード/コア/SMT情報を取得（`/sys/devices/system/cpu` または `sysctl hw.*` パース）。スケジューラのワーカースレッドをNUMAローカルコアに自動ピン。`with-thread-pinned-to core body`マクロ
- **根拠**: Linux `taskset` / Intel VTune CPU affinity。L3キャッシュのウォームアップを維持し、コンテキストスイッチによるキャッシュ無効化を防止
- **難易度**: Medium

#### FR-442: Fiber-Local Storage（ファイバーローカルストレージ）

- **対象**: `packages/runtime/src/fiber.lisp`（FR-340依存）, `packages/runtime/src/scheduler.lisp`
- **現状**: FR-192のTLSはOSスレッド単位。FR-340のFiberはスタックを持つが独立したローカルストレージなし。Fiberが別のOSスレッドに移送されるとTLSが無効になる
- **内容**: per-fiber `fls-table`（`fiber-state`構造体に追加）。`fls-alloc`（FLSキーの動的確保）/`fls-set`/`fls-get`。Fiber生成時に親のFLSを選択的に継承（`inherit: :none/:copy/:shared`）。コンテキストスイッチ時にFLSポインタを自動切り替え（TLSとの合成が必要）。FR-412（Context Propagation）の実装基盤として使用
- **根拠**: Windows Fiber Local Storage / Boost.Fiber `fiber_specific_ptr`。Green Thread移送後もローカル状態を正しく参照できる
- **難易度**: Hard

---

### Phase 78 — 並行正確性・バージョニング

#### FR-450: MVCC（マルチバージョン並行制御）

- **対象**: 新規`packages/runtime/src/mvcc.lisp`（FR-345アトミック・FR-320 EBR依存）
- **現状**: FR-300（STM）は楽観的制御を定義しているが、バージョン付きデータ管理が未定義。読み取りが書き込みをブロックするケースが残る
- **内容**: `mvcc-var`型: 値のバージョンチェーン（linked list: `(value . timestamp) → old-version → ...`）。読み取りは自分のトランザクション開始タイムスタンプ以前の最新バージョンを参照（ロックなし）。書き込みは新バージョンを先頭に追加しアトミックにスワップ。古いバージョンをEBR（FR-320）で安全に回収。`with-snapshot`マクロ（スナップショット分離レベル）
- **根拠**: PostgreSQL MVCC / MySQL InnoDB / FoundationDB。読み取りと書き込みが互いをブロックしない唯一の方式
- **難易度**: Hard

#### FR-451: Async Exception Propagation（非同期例外伝播）

- **対象**: `packages/runtime/src/task.lisp`（FR-280依存）, `packages/vm/src/conditions.lisp`
- **現状**: FR-280（Structured Concurrency）はサブタスクのエラーを「親へ伝播」と記述しているが伝播モデルが未定義。CLのconditionシステムとasyncタスクの統合なし
- **内容**: タスク完了時に未捕捉conditionをラップして親タスクに再送出。`task-group`内の複数サブタスク失敗を`aggregate-error`として収集。`with-task-error-handler`（タスクツリー内のconditionに対するrestart定義）。`cancel-sibling-tasks-on-error`ポリシー（Swift TaskGroup cancelRemainingOnError相当）。CLのrestart/invoke-restart機構をasync境界を越えて伝播
- **根拠**: Swift `withThrowingTaskGroup` / Python `ExceptionGroup` (PEP 654) / Java `CompletableFuture.exceptionally`。非同期タスクの例外が握りつぶされるバグを防止
- **難易度**: Hard

#### FR-452: QSBR（Quiescent State Based Reclamation）

- **対象**: 新規`packages/runtime/src/qsbr.lisp`（FR-380 RCUと選択的補完）
- **現状**: FR-380（RCU）は内部でクォーセントステートを検出するが汎用APIとして露出していない。FR-320（EBR）はエポックカウンタが必要
- **内容**: `qsbr-register-thread`/`qsbr-quiescent`（スレッドが「安全なポイント」にいることを宣言）/`qsbr-synchronize`（全スレッドがquiescentを経るまで待機）。スケジューラのアイドルループや関数コールバック境界で自動的に`qsbr-quiescent`を呼び出す。RCUよりAPIが単純: readロック不要、単なる「定期的な宣言」のみ
- **根拠**: userspace-rcu QSBR flavor / FreeBSD SMR。EBRの変形として実装コストが最も低く、スループットが最も高い
- **難易度**: Medium

---

### Phase 79 — 高性能I/O・ネットワーク

#### FR-460: Zero-Copy I/O（ゼロコピーI/O）

- **対象**: `packages/runtime/src/io-uring.lisp`（FR-310依存）, 新規`packages/runtime/src/zerocopy.lisp`
- **現状**: FR-310（io_uring）はSQE経由のI/Oを定義しているが通常のバッファコピーを使用。カーネル↔ユーザー空間間のデータコピーが発生
- **内容**: **io_uring fixed buffers**: `io_uring_register_buffers`でバッファを事前登録、`IORING_OP_READ_FIXED`/`IORING_OP_WRITE_FIXED`でカーネルに直接アクセス許可。**`sendfile`**: ファイル→ソケット転送でユーザー空間バッファ不要（HTTP静的ファイル配信）。**`splice`**: パイプ経由でfd間データ転送。**`MSG_ZEROCOPY`**: sendの送信バッファをカーネルが完了通知まで保持
- **根拠**: Linux `sendfile(2)` / `splice(2)` / io_uring registered buffers。ネットワークI/Oのスループットを最大2xに改善（コピーコストがゼロになるため）
- **難易度**: Hard

#### FR-461: Rate Limiting / Token Bucket（レート制限・トークンバケット）

- **対象**: 新規`packages/runtime/src/ratelimit.lisp`（FR-282チャネル・FR-312タイマー依存）
- **現状**: チャネル送受信やスケジューラに流量制御なし。FR-410（Reactive Streams）の背圧は受信側主導だが時間ベース制御なし
- **内容**: `make-token-bucket rate burst`（レート: トークン/秒、burst: 最大バースト量）。`token-bucket-acquire`（トークン消費: 不足時はasync sleep）。`make-leaky-bucket rate`（固定レートで漏れる: 均一送出）。`make-sliding-window-limiter n duration`（N回/duration 制限）。`rate-limit-channel chan limiter`（チャネルに透過的にレート制限を適用）
- **根拠**: Nginx `limit_req` / Envoy rate limiting / Go `golang.org/x/time/rate`。APIレート制限・DDoS対策・スロットリングの基本アルゴリズム
- **難易度**: Medium

#### FR-462: SPSC Ring Buffer（単一生産者・単一消費者リングバッファ）

- **対象**: 新規`packages/runtime/src/spsc.lisp`（FR-345アトミック依存）
- **現状**: FR-322（ロックフリーデータ構造）でMPMCキューを定義しているが、SPSC特化の最適化なし。FR-362（Async Profiler）のデータ転送など1:1パターンが多数存在
- **内容**: power-of-2サイズのリングバッファ（インデックスのマスク演算で剰余を排除）。headはproducerのみ書き込み（キャッシュラインを分離）、tailはconsumerのみ書き込み。`store/load`メモリオーダリングのみ（CAS不要）。`spsc-push`/`spsc-pop`/`spsc-try-push`/`spsc-try-pop`。FR-258（work-stealing scheduler）のローカルキュー実装として使用可能
- **根拠**: Disruptor (LMAX) / Rust `rtrb` / C++ `boost::lockfree::spsc_queue`。ロックフリー構造の中で最も高速（CASゼロ）。プロファイラ・ログ収集・オーディオ処理の標準実装
- **難易度**: Medium

---

### Phase 80 — 並列アルゴリズム標準ライブラリ

#### FR-470: Parallel Sort（並列ソート）

- **対象**: 新規`packages/runtime/src/parallel-algo.lisp`（FR-332並列配列操作・FR-258 work-stealing依存）
- **現状**: `sort`/`stable-sort`はCLの逐次実装。FR-332の`pmap`/`preduce`はあるが、ソートのような非均一作業分割が必要なアルゴリズムには不十分
- **内容**: **並列マージソート**: 再帰分割の各レベルをwork-stealing scheduler（FR-258）に投入。閾値以下は逐次ソートにフォールバック。**並列サンプルソート**: ピボットサンプリング→バケット分割→並列バケットソート。`psort`/`pstable-sort`関数。`declare (parallel-sort)`最適化宣言
- **根拠**: Intel TBB `parallel_sort` / C++17 `std::sort(std::execution::par)` / Java `Arrays.parallelSort`。ソートはほぼ全アプリケーションで登場する最重要アルゴリズム
- **難易度**: Hard

#### FR-471: Parallel Prefix Scan（並列プレフィックススキャン）

- **対象**: `packages/runtime/src/parallel-algo.lisp`（FR-470と同一ファイル）
- **現状**: `reduce`は逐次左畳み込み。並列プレフィックス計算なし
- **内容**: **Up-sweep/Down-sweep（Blelloch scan）**: O(N)ワーク、O(log N)深度。`parallel-scan seq fn init`（inclusive/exclusive両対応）。`parallel-prefix-sum`（加算scan特化）。GPUバックエンド（FR-400）への橋渡し: WGSLのWorkgroup scan命令へマッピング。`parallel-histogram`（頻度集計の並列化）
- **根拠**: CUDA `thrust::inclusive_scan` / Rust rayon `scan`。Stream compaction・Radix sort・BFS等の基本ビルディングブロック
- **難易度**: Hard

#### FR-472: Fork-Join Framework（フォーク・ジョイン並列フレームワーク）

- **対象**: `packages/runtime/src/parallel-algo.lisp`, `packages/runtime/src/scheduler.lisp`（FR-258 work-stealing依存）
- **現状**: FR-332の`parallel-for`は単純なチャンク分割のみ。再帰的なデータ分割（分割統治）を自然に表現する構文なし
- **内容**: `fork-join`マクロ: 2つの独立したサブ計算をフォーク→両方完了でジョイン。`parallel-reduce`（再帰的二分割）。`spawn`/`sync`（Cilkスタイルの軽量fork-join）。タスク盗取はFR-258のwork-stealing上に実装。スタックオーバーフロー防止のための逐次フォールバック閾値（grain size）
- **根拠**: Cilk / Intel TBB / Java ForkJoinPool / Rust rayon join。分割統治アルゴリズム（クイックソート・FFT・行列乗算・ツリー探索）の自然な並列化
- **難易度**: Hard

---

### Phase 81 — ハードウェアセキュリティ拡張

#### FR-480: ARM MTE（メモリタギング拡張）

- **対象**: `packages/runtime/src/heap.lisp`, `packages/emit/src/aarch64-codegen.lisp`
- **現状**: AArch64バックエンドあり（`packages/emit/src/aarch64.lisp`）。MTE命令（`IRG`/`STG`/`LDG`/`ADDG`）の生成なし。並行UAF（Use-After-Free）検出なし
- **内容**: ヒープアロケーション時に`IRG`（Insert Random Tag）でポインタ上位4ビットにランダムタグ付与。メモリへの`STG`（Store Allocation Tag）でタグを記録。解放時にタグを変更。メモリアクセス時にハードウェアがタグ照合、不一致で`SIGSEGV`（`SEGV_MTEAERR`）。FR-362（Async Profiler）との統合でUAFの発生スタックを記録
- **根拠**: ARMv8.5-A MTE / Android 11+ MTE support / LLVM MemTagSanitizer。TSanより15-40%オーバーヘッドが低く、本番環境での常時有効が現実的
- **難易度**: Hard

#### FR-481: Hardware Performance Counters（ハードウェアパフォーマンスカウンター）

- **対象**: 新規`packages/runtime/src/perf.lisp`, `packages/cli/src/main.lisp`
- **現状**: FR-362（Async Profiler）はウォールクロック時間のみ。CPUサイクル・キャッシュミス・分岐予測ミス等のハードウェアイベント収集なし
- **内容**: Linux `perf_event_open` syscall（FFI）。x86-64 `RDPMC`命令。カウンタ: サイクル、命令数、L1/L2/L3キャッシュミス、分岐予測ミス、TLBミス、メモリ帯域幅。`with-perf-counters counters body`マクロ（スコープ内の集計）。並行性固有カウンタ: CAS失敗回数・スピン待ち時間・コンテキストスイッチ回数
- **根拠**: Linux perf / Intel VTune / Apple Instruments。並行プログラムのキャッシュ競合・False Sharing（FR-356）の定量的検出
- **難易度**: Hard

---

### Phase 82 — 可観測性・分散トレーシング

#### FR-490: OpenTelemetry Integration（分散トレーシング統合）

- **対象**: 新規`packages/runtime/src/otel.lisp`（FR-412 Context Propagation依存）
- **現状**: FR-412のContext PropagationはトレースIDの伝播を定義しているが、OpenTelemetry準拠のスパン生成・エクスポートなし
- **内容**: `with-span name body`マクロ: スパン開始/終了を自動記録。FR-412の`*current-context*`にトレース・スパンIDを格納して自動伝播。`span-set-attribute key value`/`span-add-event name attrs`。OTLPプロトコル（gRPC/HTTP）でJaeger・Tempo・Honeycombへエクスポート。非同期タスク（FR-280）のスパンを親タスクのスパンの子として自動接続。サンプリング（Head-based: 確率的、Tail-based: エラー時のみ）
- **根拠**: OpenTelemetry specification (CNCF) / Jaeger / Zipkin。マイクロサービス・分散アクター（FR-430）のレイテンシ追跡に不可欠
- **難易度**: Hard

#### FR-491: Vector Clocks / Logical Clocks（ベクタークロック・論理時計）

- **対象**: 新規`packages/runtime/src/clock.lisp`（FR-430分散アクター依存）
- **現状**: 分散システムでのイベント順序付け機構なし。物理時計は`get-internal-real-time`のみ
- **内容**: **Lamport Clock**: メッセージ送受信のたびにインクリメント（因果関係の部分順序）。**Vector Clock**: per-nodeカウンタベクター（`[node-id → counter]`）、同時発生イベントの検出が可能。**Hybrid Logical Clock (HLC)**: 物理時計と論理時計の合成（単調増加保証 + 物理時刻近似）。`hlc-now`/`hlc-update`/`hlc-compare`。FR-432（Raft）のログエントリタイムスタンプとして使用
- **根拠**: Leslie Lamport (1978) / Amazon DynamoDB Vector Clocks / CockroachDB HLC。「どちらが先か」を正確に判定できない物理時計の代替
- **難易度**: Hard

#### FR-492: Structured Logging for Concurrency（並行性構造化ロギング）

- **対象**: 新規`packages/runtime/src/log.lisp`（FR-462 SPSC Ring Buffer・FR-412 Context依存）
- **現状**: ロギング機構なし（`format t`のみ）。マルチスレッド環境でのインターリーブ出力問題が発生
- **内容**: per-thread ログバッファ（FR-462 SPSC Ring Buffer使用）→バックグラウンドI/Oスレッドが非同期フラッシュ（ロギングスレッドをブロックしない）。構造化フォーマット（JSON Lines）: タイムスタンプ・スレッドID・ファイバーID・トレースID・ログレベル・メッセージ・任意KV。`log-info`/`log-warn`/`log-error`マクロ（FR-412コンテキストからトレースIDを自動取得）。サンプリングロガー（高頻度ループ内でのログ出力を間引く）
- **根拠**: Tokio tracing / Rust `tracing` crate / Go `slog` (1.21)。並行プログラムのデバッグで最も実用的なツール
- **難易度**: Medium

---
