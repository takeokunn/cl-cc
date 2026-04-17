# Runtime: Standard Library — Extended Features (Phase 138-175, 71 FRs)

Algebraic effects, string builder/rope, structured logging, LSP/DAP, continuations, hygienic macros, script mode, C embedding API, memory pools, circular structure printing, transient collections, security hardening, runtime configuration, sequence protocol, numeric stability, thread synchronization, networking, lazy evaluation, numeric dispatch optimization, multiple values ABI, random access I/O, copy-on-write, green threads, custom hash tables, floor/truncate complete, CLOS fast path, load-time-value, symbol table, FASL paging, PGO persistence, tail call/exception interaction, backquote optimization, RISC-V backend, delimited continuations, reproducible builds, forward references, I/O buffering, pathname system.

---

### Phase 138 — 文字列ビルダ・Rope

#### FR-787: String Builder (文字列ビルダ)

- **対象**: 新ファイル `packages/engine/vm/src/string-builder.lisp`
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

- **対象**: `packages/engine/vm/src/string-builder.lisp`
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

- **対象**: 新ファイル `packages/backend/runtime/src/logging.lisp`
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

- **対象**: 新ファイル `packages/backend/runtime/src/metrics.lisp`
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

- **対象**: `packages/backend/runtime/src/metrics.lisp`
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

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/compile/src/cps.lisp`
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

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
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

- **対象**: `packages/frontend/expand/src/macro.lisp` (新ファイル `packages/frontend/expand/src/syntax-rules.lisp`)
- **内容**:
  - `(define-syntax name (syntax-rules (keywords...) (pattern template) ...))` — Scheme R7RS互換
  - パターン変数の衛生性: マクロ展開で導入される変数が呼び出し側の変数と衝突しない
  - `...` エリプシス: ゼロ個以上の要素のパターンマッチ
  - `syntax-case` (R6RS) の部分サポート: 複雑なパターンと条件式
  - 既存の `defmacro` との共存（`defmacro` はデフォルト、衛生性が必要な場合に `define-syntax`）
- **根拠**: Scheme R7RS §4.3 / Kohlbecker et al. 1986 hygienic macros。マクロのコード汚染を排除
- **難易度**: Hard

#### FR-805: Gensym-based Hygiene (gensymベースの衛生性)

- **対象**: `packages/frontend/expand/src/macro.lisp`
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

- **対象**: `cli/src/main.lisp`, `packages/frontend/parse/src/cl/lexer.lisp`
- **内容**:
  - `#!/usr/bin/env cl-cc` を含むファイルを `./script.lisp` として実行可能にする
  - レクサが行頭の `#!` を行コメントとして無視（ANSI CL外の拡張）
  - `(cl-cc:main args)` — `main`関数をエントリポイントとして自動呼び出し
  - `--script` フラグ: デバッグ出力を抑制して純粋な stdout のみ
  - パイプライン: `echo "input" | cl-cc --script process.lisp`
- **根拠**: SBCL `--script` / Chicken Scheme `csi -s` / Guile shebang
- **難易度**: Easy

#### FR-809: Command-Line Arguments API (コマンドライン引数API)

- **対象**: `cli/src/main.lisp`
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

- **対象**: `packages/engine/vm/src/vm.lisp`, `src/ffi/embedding.lisp`
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

- **対象**: `packages/backend/runtime/src/heap.lisp`
- **内容**:
  - `(make-arena &optional size-hint)` — GC管理外の専用アリーナ確保
  - `(arena-alloc arena size)` — アリーナからのバンプポインタ割り当て O(1)
  - `(with-arena (a) &body)` — スコープ終了時にアリーナを一括解放（GC停止なし）
  - 用途: パーサ中間ノード、JSON パース、一時バッファの高速割り当て・一括解放
  - `arena-reset arena` — アリーナを先頭に巻き戻す（メモリ返却なし）
- **根拠**: Apache APR pools / Zig ArenaAllocator / Rust Bump。GCプレッシャーゼロの一時割り当て
- **難易度**: Medium

#### FR-817: Object Pool (オブジェクトプール)

- **対象**: `packages/backend/runtime/src/heap.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`
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

- **対象**: `packages/frontend/expand/src/expander-defstruct.lisp`, `packages/engine/vm/src/vm-clos.lisp`
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

- **対象**: `packages/engine/vm/src/persistent.lisp`
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

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/engine/vm/src/array.lisp`
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

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/backend/runtime/src/runtime.lisp`
- **内容**:
  - スタックフレーム確立時にカナリア値（ランダム整数）を配置
  - 関数リターン時にカナリア値の一致を確認
  - 不一致検出時に `memory-fault` condition をシグナル
  - `--security-canaries` フラグで有効化（パフォーマンスオーバーヘッド約5%）
  - `SIGILL`/`SIGABRT` でのクラッシュをLispレベルでキャッチ
- **根拠**: GCC/Clang `-fstack-protector-strong`。バッファオーバーフロー検出の基本防御
- **難易度**: Medium

#### FR-829: Integer Overflow Detection (整数オーバーフロー検出)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/x86-64-codegen.lisp`
- **内容**:
  - fixnum算術のオーバーフローを x86-64 `jo`（overflow flag）命令で検出
  - オーバーフロー時: fixnum → bignum への自動昇格（`arithmetic-error`を投げない）
  - `(declare (optimize (safety 0)))` で昇格チェックを省略（C言語の `int` 動作）
  - AArch64: `adds`/`subs`命令の overflow flag 利用
- **根拠**: SBCL fixnum overflow → bignum promotion / GCC `-ftrapv`
- **難易度**: Hard

#### FR-830: Taint Tracking (テイントトラッキング)

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/strings.lisp`
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

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/runtime/src/heap.lisp`
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

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `pipeline/src/pipeline.lisp`
- **内容**:
  - `*jit-tier1-threshold*` — Tier-0 → Tier-1 エスカレートの呼び出し回数（デフォルト100）
  - `*jit-trace-threshold*` — トレース記録開始の閾値（デフォルト100ループ反復）
  - `*jit-inline-size-limit*` — インライン展開する関数の最大命令数（デフォルト30）
  - `*jit-code-cache-size*` — コードキャッシュサイズ（デフォルト256MB）
  - プロファイル誘導自動調整: `--adaptive-jit` フラグで閾値を実行時に最適化
- **根拠**: V8 `--trace-opt` / HotSpot `-XX:CompileThreshold`
- **難易度**: Easy

#### FR-835: Adaptive Runtime Optimization (適応的ランタイム最適化)

- **対象**: `packages/engine/vm/src/vm-run.lisp`, `packages/engine/optimize/src/optimizer.lisp`
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

- **対象**: `packages/engine/vm/src/vm-clos.lisp`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **内容**:
  - ユーザーが独自のシーケンス型を定義し、`length`/`elt`/`(setf elt)`/`subseq`/`make-sequence-like` を実装することで全シーケンス関数が動作する
  - `sequence` 抽象クラスを `standard-class` の上位に定義
  - `(defclass my-deque (sequence) ...)` → `mapcar`/`find`/`sort`等が自動で動作
  - `elt` / `length` / `make-sequence-like` / `sequence-protocol-p` の基本ジェネリック関数定義
- **根拠**: SBCL extensible sequences (Rhodes 2007)。リングバッファ・双方向リスト等をシームレスに使用
- **難易度**: Hard

#### FR-839: Iteration Protocol (反復プロトコル)

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`
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

- **対象**: `packages/engine/vm/src/primitives.lisp`
- **内容**:
  - `(kahan-sum sequence)` — 誤差補正付き浮動小数点加算
  - `make-kahan-accumulator` / `kahan-add! acc val` / `kahan-result acc`
  - `(loop :sum :kahan x :into acc)` — loop マクロの `:kahan` 蓄積クォーリファイア
  - `(pairwise-sum seq)` — 分割統治による数値安定加算 O(n) but O(log n) 誤差増大
- **根拠**: Kahan 1965 / Higham "Accuracy and Stability"。数値計算の基本精度保証
- **難易度**: Easy

#### FR-843: Floating-Point Exception Control (浮動小数点例外制御)

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/runtime/src/runtime.lisp`
- **内容**:
  - `(with-float-traps-masked (:divide-by-zero :overflow :underflow :inexact) body)` — FPU例外マスク
  - `(get-float-traps)` — 現在の例外マスク状態を取得
  - x86-64 MXCSR / x87 FCW レジスタへのアクセス
  - `float-features:with-float-features` 互換API
  - `*floating-point-modes*` — デフォルト丸めモード (:round-to-nearest/:round-to-zero/etc.)
- **根拠**: IEEE 754-2019 §9 / SBCL `sb-int:with-float-traps-masked`
- **難易度**: Medium

#### FR-844: Extended Precision Arithmetic (拡張精度演算)

- **対象**: `packages/engine/vm/src/primitives.lisp`
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

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/backend/runtime/src/runtime.lisp`
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

- **対象**: `packages/engine/vm/src/conditions.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/backend/runtime/src/runtime.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`
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

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/engine/vm/src/vm.lisp`
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

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`
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

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/engine/compile/src/codegen.lisp`
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

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/primitives.lisp`
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

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/engine/vm/src/vm-execute.lisp`
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

- **対象**: `packages/engine/vm/src/vm-execute.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`, `packages/backend/runtime/src/runtime.lisp`
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

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/engine/vm/src/vm.lisp`
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

- **対象**: `packages/engine/vm/src/vm.lisp`
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

- **対象**: `packages/backend/runtime/src/runtime.lisp`, `packages/engine/vm/src/vm.lisp`
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

- **対象**: `packages/backend/runtime/src/runtime.lisp`
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

- **対象**: `packages/engine/vm/src/hash.lisp`
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

- **対象**: `packages/engine/vm/src/hash.lisp`
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

- **対象**: `packages/engine/vm/src/primitives.lisp`
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

- **対象**: `packages/engine/vm/src/primitives.lisp`
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

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
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

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
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

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
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

- **対象**: `packages/engine/vm/src/vm.lisp`
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

- **対象**: `packages/engine/vm/src/vm.lisp`, `packages/frontend/expand/src/expander.lisp`
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

- **対象**: `packages/engine/vm/src/vm.lisp`, `cli/src/main.lisp`
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

- **対象**: `packages/engine/vm/src/vm.lisp`, `cli/src/main.lisp`
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

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm-execute.lisp`
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

- **対象**: `packages/frontend/expand/src/macros-stdlib.lisp`, `packages/engine/compile/src/codegen.lisp`
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

- **対象**: `packages/backend/emit/src/riscv64.lisp` (新規), `packages/foundation/mir/src/target.lisp`
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

- **対象**: `packages/engine/vm/src/vm-execute.lisp`, `packages/frontend/expand/src/macros-stdlib.lisp`
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

- **対象**: `cli/src/main.lisp`, `cl-cc.asd`
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

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/engine/vm/src/vm.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`
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

- **対象**: `packages/engine/vm/src/io.lisp`
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

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
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

- **対象**: `packages/engine/vm/src/vm-clos.lisp`
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
