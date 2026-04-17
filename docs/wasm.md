# WebAssembly Backend

WebAssembly backend optimization and advanced features.

Modern Wasm compiler features (2026 coverage):

- **Phase 23**: Backend micro-optimizations (ref.cast, return_call, typed env, fixnum range)
- **Phase 38**: SIMD128, Threads/Atomics, Exception Handling codegen
- **Phase 39**: Stack Switching, Component Model, WASI 0.2, Multiple Memories
- **Phase 40**: Wasm GC / i31ref integration, Typed Function References
- **Phase 41**: Memory64, Relaxed SIMD, Extended Const Expressions
- **Phase 42**: JS Promise Integration, String Builtins, AOT/PGO
- **Phase 43**: Debug Info (DWARF/source maps), Shared Everything Threads

---

### Phase 23 — Wasmバックエンド最適化

#### FR-142: Wasm `ref.cast` Elimination

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`
- **内容**: 直前に`struct.new $closure_t`で割り当てられたレジスタへの`ref.cast`を除去（型が静的に確定済み）
- **根拠**: `vm-call`のたびに型が判明しているクロージャへも`ref.cast`を発行
- **難易度**: Medium

#### FR-143: Wasm `return_call` / `return_call_indirect`

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`
- **内容**: selfループ+`br_table`によるPC-dispatchをWasm tail-call proposal の`return_call_indirect`に置換
- **根拠**: tail callをPC更新+ループとして扱う現状の非効率を解消
- **難易度**: Medium

#### FR-144: Wasm Typed Closure Environment Array

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`
- **内容**: クロージャ環境を`$env_t`参照でなくWasm GC `array.new_fixed eqref`として具体化、`array.get`で直接インデックスアクセス
- **根拠**: 現状`(ref.null $env_t)` のTODO状態
- **難易度**: Hard

#### FR-145: Wasm Integer Range Annotation

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/compile/src/codegen.lisp`
- **内容**: fixnum範囲と証明された`vm-const`にアノテーション付与、Wasmバックエンドの`wasm-fixnum-unbox`を省略
- **根拠**: `wasm-fixnum-unbox`が全整数演算で無条件発行されている
- **難易度**: Medium

---

### Phase 38 — Wasm高度機能

#### FR-202: Wasm SIMD Support

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: Wasm SIMD命令なし。ベクトル型（v128）未定義
- **内容**: Wasm SIMD128拡張（v128.load, i32x4.add, f64x2.mul等）。数値配列操作のベクトル化コード生成。自動ベクトル化との連携
- **根拠**: Wasm SIMD Proposalは全主要ブラウザで出荷済み（Chrome/Firefox/Safari/Edge）
- **難易度**: Hard

#### FR-203: Wasm Threads / Shared Memory

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: shared memoryなし。atomic命令なし。Worker間データ共有不可
- **内容**: `shared` memory宣言、`memory.atomic.wait`/`memory.atomic.notify`、`i32.atomic.rmw.cmpxchg`等のatomic操作。SharedArrayBuffer経由のWorker間通信
- **根拠**: Wasm Threads Proposalは主要ブラウザで出荷済み。並行Webアプリケーションに必須
- **難易度**: Very Hard

#### FR-204: Wasm Exception Handling Codegen

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/backend/emit/src/wasm-types.lisp`
- **現状**: `+wasm-try+`/`+wasm-catch+`/`+wasm-throw+`定数は定義済み（`wasm-types.lisp:89-91`）だがコード生成なし
- **内容**: `handler-case`/`handler-bind`をWasmの`try`/`catch`/`throw`に変換。現状のPC-dispatchベースの例外処理をネイティブWasm例外に置換
- **根拠**: 定数は実装済みだがemitルールが未接続。ネイティブ例外で性能10-100倍改善
- **難易度**: Medium

---

### Phase 40 — Wasm GC / i31ref 統合

#### FR-209: Wasm GC `i31ref` — Fixnum ネイティブボクシング

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/backend/emit/src/wasm-types.lisp`
- **現状**: fixnum は `(struct.new $fixnum_t (i64.const N))` としてヒープ上にボクシング。GCプレッシャー大
- **内容**: Wasm GC Proposal（Chrome 119+ 標準化済み）の `i31ref` 型を利用し、31bit fixnum をポインタ幅のスロットに即値格納。`ref.i31`/`i31.get_s`/`i31.get_u` でボクシングなし整数演算。CL の fixnum タグビット戦略と直接対応
- **根拠**: `i31ref` は LISP-family 言語のタグ付き整数のために設計された命令。Wasm GC 仕様 § 3.2.2 "Scalar reference types"
- **難易度**: Medium

#### FR-210: Wasm GC 構造体型 — CL クラスの静的型レイアウト

- **対象**: `packages/backend/emit/src/wasm-types.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: CLOS インスタンスは `:__class__` キーを持つハッシュテーブル。全スロットアクセスが `(gethash :slot inst)` 経由
- **内容**: `defclass` のスロット定義から Wasm GC `struct` 型 (`struct.new_canon $ClassName`) を生成。スロットアクセスを `struct.get $ClassName $slot_idx` に変換。構造体の継承チェーンを `sub` 型階層で表現（Wasm GC § 4.2 "Structural subtyping"）
- **根拠**: JVM / V8 の hidden class と同等の型特化。スロット読み書き O(1) を GC-root 配置で実現
- **難易度**: Very Hard

#### FR-211: Wasm GC 配列型 — `array.new_fixed` / `array.get`

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/list.lisp`
- **現状**: CL ベクタ・クロージャ環境は `eqref` 配列として TODO 状態（FR-144 から継続）
- **内容**: `(array.new_fixed eqref N)` でクロージャ環境・ベクタをWasm配列として具体化。`array.get`/`array.set`/`array.len` で直接インデックスアクセス。`array.copy` で `replace`/`subseq` を1命令化
- **根拠**: FR-144 の実装基盤。Wasm GC `array` 型は Chrome 119 / Firefox 120 / Safari 17.4 出荷済み
- **難易度**: Hard

#### FR-212: Typed Function References — `call_ref` / `ref.func`

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: 関数ポインタは `funcref`（untyped）として `call_indirect` 経由で呼び出し。型チェックは実行時テーブル検索
- **内容**: Wasm Typed Function References Proposal（標準化済み）の `(ref $func_type)` を利用。`ref.func $fn` で型付き関数参照を生成し `call_ref (type $fn_sig)` で直接呼び出し。`#'fn` のWasm表現を `funcref` → `(ref $closure_sig)` に昇格
- **根拠**: `call_indirect` のテーブル検索オーバーヘッドを除去。V8 では `call_ref` が `call_indirect` より 15〜30% 高速（Wasm GC ベンチマーク 2023）
- **難易度**: Medium

---

### Phase 41 — Memory64 / Relaxed SIMD / Extended Const

#### FR-213: Memory64 — 64bit アドレス空間

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: Wasm memory は `memory (import "" "mem") (i32)` 宣言（4GB上限）。大規模データ構造でアドレス枯渇リスク
- **内容**: `memory64` フラグ（`(memory i64 ...)` 宣言）を付与し、`i64.load`/`i64.store`/`memory.grow` の型を `i64` に昇格。`--target wasm64-unknown-unknown` ビルドターゲット追加。GCヒープのアドレス空間を事実上無制限化
- **根拠**: Wasm Memory64 Proposal は Chrome 119 / Firefox 120 出荷済み。大規模CL処理系（Quicklisp全体のロードなど）に必要
- **難易度**: Medium

#### FR-214: Relaxed SIMD — NaN-propagation / fused multiply-add

- **対象**: `packages/backend/emit/src/wasm.lisp`, 数値演算エミッタ
- **現状**: SIMD128（FR-202）は strict NaN セマンティクスの `f64x2.add` 等のみ
- **内容**: Wasm Relaxed SIMD Proposal（Chrome 114+ 出荷済み）の以下命令を追加：`f32x4.relaxed_madd`（fused multiply-add）、`i32x4.relaxed_laneselect`（VPBLENDVB相当）、`f64x2.relaxed_min/max`（platform-native NaN処理）。浮動小数点集約演算で 5〜15% の追加高速化
- **根拠**: Wasm Relaxed SIMD は2022年Phase 4到達。IEEE 754 strict 準拠不要な数値計算用途向け
- **難易度**: Medium

#### FR-215: Extended Constant Expressions — グローバル初期化の拡張

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: グローバル初期化式は `i32.const`/`i64.const`/`f64.const` のみ許可。複合定数（`*read-table*` テーブル等）は実行時初期化
- **内容**: Wasm Extended Constant Expressions Proposal（Chrome 113+ 出荷済み）を使い、`global.get`/`i32.add`/`i32.sub`/`i32.mul` 等をグローバル初期化式内で使用可能に。`defconstant` 束縛の Wasm グローバルとしての静的宣言が可能になる
- **根拠**: グローバルテーブルのゼロ初期化コスト除去。スタートアップ時間の短縮
- **難易度**: Easy

#### FR-216: Branch Hinting — 予測ヒント付与

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, 型述語エミッタ
- **現状**: `br_if`/`if` に予測ヒントなし。型チェック分岐でエンジン側の分岐予測が外れる
- **内容**: Wasm Branch Hinting Proposal の `@metadata.code.branch_hint` カスタムセクションを生成。型述語（`consp`/`numberp`/`symbolp`）の fast-path を `likely=1` でアノテート。エンジン（V8/SpiderMonkey）の投機実行精度向上
- **根拠**: Google/Mozilla が仕様策定中（2024年Phase 2）。型述語が多発するCL dispatch では効果が大きい
- **難易度**: Easy

---

### Phase 42 — JS 統合 / 文字列 / AOT・PGO

#### FR-217: Wasm JS Promise Integration — CL `async`/`await`

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/frontend/expand/src/macros-stdlib.lisp`
- **現状**: Wasmから非同期JSコール（fetch/setTimeout等）を行うにはコールバック地獄か手動コルーチン
- **内容**: Wasm JS Promise Integration（Chrome 123+ 出荷済み）の `WebAssembly.promising` / `WebAssembly.suspending` を利用。CL の `cl:async-handler` / `cl:await` マクロをWasm Stack Switching で実装し、JSの `Promise` を `await` で待機可能に。`(await (js-fetch url))` → `(resume (cont.new $await_cont))` に変換
- **根拠**: Wasm Stack Switching 本体（FR-205）より先に実用化可能な非同期パス。ブラウザCL REPLの実装に必要
- **難易度**: Hard

#### FR-218: Wasm String Builtins — WTF-16 ネイティブ文字列

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: CL文字列はUTF-8バイト配列として線形メモリに格納。JSとの文字列受け渡しにエンコード変換が必要
- **内容**: Wasm String Builtins Proposal（Chrome 117+ 出荷済み）の `string.new_utf8_array`/`string.encode_wtf16_array`/`string.measure_wtf16` を使用。`simple-string` をWasm managed string として直接表現し、JS `String` とゼロコピー変換。`string-upcase`/`string-downcase` を組み込み命令にマップ
- **根拠**: テキスト処理重視のWebアプリでエンコード変換コスト撤廃。JS ↔ Wasm 文字列境界のゼロコピー化
- **難易度**: Medium

#### FR-219: AOT モード — 静的 Wasm バイナリ生成

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: `./cl-cc compile` はVM命令列を出力後、実行時JITでWasmに変換
- **内容**: `./cl-cc compile --target wasm32 --aot` フラグで完全静的な `.wasm` バイナリを1パスで生成。`wasm-opt`（Binaryen）を自動起動してサイズ最適化（`-O3 --strip-debug`）。`wasm2wat` でテキスト形式を生成しデバッグ支援。dead export elimination でバイナリサイズを 20〜40% 削減
- **根拠**: CDN配信・エッジ実行（Cloudflare Workers / Fastly Compute）向けの必須ビルドモード
- **難易度**: Hard

#### FR-220: Profile-Guided Optimization (PGO) for Wasm

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: 最適化パスは静的ヒューリスティクスのみ（定数畳み込み・CSE・デッドコード除去）
- **内容**: Wasm実行中のプロファイルデータ（V8の `--wasm-pgo` フラグ、または instrumented ビルド）をフィードバックし、インライン展開判定・分岐重み・型特化に反映。2段階ビルド：(1) instrumented `.wasm` でプロファイル収集 → (2) PGO `.wasm` でホットパス特化。`defgeneric` の最頻ディスパッチパスを monomorphic stub にインライン化
- **根拠**: V8 の Wasm PGO は2023年実装。ディスパッチ多発の CLOS コードで 30〜50% 高速化が報告されている
- **難易度**: Very Hard

#### FR-221: Wasm Import/Export リンク最適化 — Dead Import Elimination

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: 全 VM ホストブリッジ関数（`*vm-host-bridge-functions*` の全エントリ）が無条件にWasm import として宣言される
- **内容**: 依存グラフ解析で実際に呼び出される import のみを宣言。`--gc-sections` 相当のリンク時デッドコード除去をWasm レベルで実施。Binaryen `--remove-unused-module-elements` パスを AOT パイプラインに統合
- **根拠**: 未使用 import はバイナリサイズ・JIT コンパイル時間両方に影響。最小 `.wasm` は CDN キャッシュヒット率に直結
- **難易度**: Medium

---

### Phase 43 — デバッグ情報 / Shared Everything Threads

#### FR-222: DWARF デバッグ情報 — Wasm カスタムセクション

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: デバッグ情報なし。Chrome DevTools でのステップ実行・変数インスペクト不可
- **内容**: DWARF 5 形式のデバッグ情報を Wasm カスタムセクション（`.debug_info`/`.debug_line`/`.debug_abbrev`）に埋め込み。CL ソース行 → Wasm バイトオフセットのマッピングを生成。`--emit-debug-info` フラグで有効化。Chrome DevTools / LLDB の Wasm デバッガが CL ソースを直接表示
- **根拠**: DWARF-in-Wasm は Chrome 90+（2021年）でサポート。Emscripten・Rust の実装が先行。開発体験の大幅改善
- **難易度**: Hard

#### FR-223: Source Maps — ブラウザコンソール CL ソース表示

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: エラースタックトレースが Wasm バイトオフセット表記（`wasm-function[42]:0x1a3f`）
- **内容**: Source Map v3 形式 (`.wasm.map`) を生成し、`sourceMappingURL` カスタムセクションで参照。CL ソースファイル・行番号・列番号を Chrome/Firefox DevTools に露出。`--source-map` フラグで有効化。本番環境では Source Map を別ホストに配置してバイナリサイズを節約
- **根拠**: ブラウザ上でのCL REPL・デバッガ実装の前提条件。DWARF より軽量なデバッグパス
- **難易度**: Medium

#### FR-224: Wasm Shared Everything Threads

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: Wasm Threads（FR-203）は SharedArrayBuffer + Atomics のみ。Wasm オブジェクト（struct/array）は Worker 間で共有不可
- **内容**: Wasm Shared Everything Threads Proposal（2024年Phase 2、Firefox Nightly 実装中）を利用。`(shared (struct ...))` / `(shared (array ...))` 型宣言で GC マネージドオブジェクトを Worker 間で共有可能に。CL の `bt:make-thread` → Wasm shared thread に変換。CL `bordeaux-threads` API の Wasm ネイティブバックエンドを実装
- **根拠**: 従来の Wasm Threads では構造体・クロージャをスレッド間で共有するにはシリアライズが必要だった。Shared Everything はこれを解消し、CL のマルチスレッドモデルに直接対応
- **難易度**: Very Hard

#### FR-225: Custom Annotations — コンパイラヒントカスタムセクション

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: コンパイラヒント（型特化・インライン展開済みマーク・モジュール境界）をWasmバイナリに伝達する手段なし
- **内容**: Wasm Custom Annotations Proposal を利用し、独自カスタムセクション `cl-cc.annotations` を定義。アノテーション内容：(1) 型特化済み関数マーク（エンジンの再最適化を抑制）、(2) インライン展開ヒント (`@noinline`/`@always_inline`)、(3) コールグラフエッジ頻度（PGO フィードバックの伝達形式）。`wasmtime` の custom annotation API と連携
- **根拠**: LTO（Link-Time Optimization）相当の情報をWasmレイヤーで伝達する標準化パス。2024年提案中
- **難易度**: Medium

---

### Phase 44 — JS FFI / Dynamic Linking / Bulk Memory

#### FR-226: `externref` — 不透過 JS オブジェクト参照

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: JS オブジェクトの受け渡しには線形メモリへのシリアライズが必要。DOM ノード・Promise・Map 等の JS オブジェクトを CL 側で保持できない
- **内容**: Wasm Reference Types Proposal（Chrome 91+、標準化済み）の `externref` 型を利用。JS → Wasm import 関数の引数・戻り値に `externref` を宣言し、JS オブジェクトを不透明なポインタとして CL ヒープに格納。`(js-ref-p x)` / `(js-ref-get x :property)` 等の CL バインディングを生成。`*vm-host-bridge-functions*` ホワイトリストの自然な拡張先
- **根拠**: DOM 操作・WebGL・Fetch API 等を CL から呼び出す際の基盤。`externref` は Wasm GC の `eqref` 型階層の外部入り口でもある
- **難易度**: Medium

#### FR-227: Wasm Dynamic Linking — CL `load`/`require` のWasm実装

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: コンパイル出力は単一の monolithic `.wasm` バイナリ。CL の `(load "my-lib")` / `(require :my-system)` に相当するランタイム動的ロードなし
- **内容**: Wasm Dynamic Linking（Emscripten の `SIDE_MODULE`/`MAIN_MODULE` 方式 + Wasmtime `component link` の2方式）に対応。メインモジュールが `__indirect_function_table` を export し、サイドモジュールが `__stack_pointer`/`__memory_base`/`__table_base` を import する規約に準拠。`./cl-cc compile --side-module my-lib.lisp` でサイドモジュール生成。CL の `(load "my-lib.wasm")` を `WebAssembly.instantiate` + テーブル更新にコード生成
- **根拠**: Quicklisp スタイルの遅延ロード・ライブラリ分割ビルドに必須。ブラウザの code splitting（初期ロードサイズ削減）にも対応
- **難易度**: Very Hard

#### FR-228: Bulk Memory Operations — `memory.copy` / `memory.fill`

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/list.lisp`, `packages/engine/vm/src/strings.lisp`
- **現状**: 配列コピー・文字列コピーは `i32.load`/`i32.store` ループで実装（Wasm MVP 相当）
- **内容**: Wasm Bulk Memory Operations（MVP v1.1、全主要ブラウザ標準化済み）の `memory.copy`/`memory.fill`/`memory.init`/`data.drop` を使用。`copy-seq`/`replace`/`fill` を `memory.copy`/`memory.fill` 1命令に変換。文字列コンカテネーション・サブシーケンス操作でメモリ帯域効率を改善。`data.drop` でイニシャライザ完了後の定数プールを解放
- **根拠**: Bulk Memory は2019年標準化済み（long-standing 未実装）。`memmove` 相当を Wasm エンジンのネイティブ SIMD 実装に委譲できる
- **難易度**: Easy

#### FR-229: `table64` — 64bit 関数テーブル

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: `call_indirect` の関数テーブルは 32bit インデックス（4G エントリ上限）。Memory64（FR-213）と非対称
- **内容**: Wasm `table64` Proposal（Memory64 の対応テーブル拡張）で関数テーブルインデックスを `i64` に昇格。`table.get`/`table.set`/`table.grow`/`table.size` の型を `i64` に統一。`--target wasm64` ビルドで Memory64 と table64 を同時有効化
- **根拠**: 64bit アドレス空間で 4G を超える関数エントリが必要な巨大 CL システム（Quicklisp 全体のロード等）への備え。Memory64 との一貫性
- **難易度**: Easy

#### FR-230: WebAssembly ESM Integration — `import` ネイティブ対応

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: ブラウザでの Wasm ロードは `fetch` + `WebAssembly.instantiate` の手動ステップが必要
- **内容**: Wasm ESM Integration Proposal（Chrome 89+、Safari 15+）に準拠した `.wasm` export を生成。JS から `import { myFn } from './module.wasm'` で直接インポート可能な形式。Top-level await との統合。`<script type="module">` での Wasm モジュール直接参照。`./cl-cc compile --esm` フラグで ESM 準拠バイナリ生成
- **根拠**: CDN 経由の Wasm ライブラリ配布・ブラウザ CL REPL のバンドレス配信に必要
- **難易度**: Medium

#### FR-231: Wasm GC 型階層の完全活用 — `anyref` / `eqref`

- **対象**: `packages/backend/emit/src/wasm-types.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: Wasm GC 型（FR-209〜211）を個別に使用しているが、型階層（`anyref > eqref > (i31ref | (ref $struct) | (ref $array))`）を活用したダウンキャスト最適化なし
- **内容**: CL の `(typep x 'fixnum)` → `ref.test i31` へのコンパイル。`(typep x 'cons)` → `ref.test (ref $cons_t)`。`case`/`typecase` のディスパッチを `ref.cast` チェーンから `br_on_cast`/`br_on_cast_fail` に変換（Wasm GC § 5.4.3 "Reference instructions"）。`eqref` を CL `eql` の実装基盤として使用
- **根拠**: `br_on_cast` は分岐と型チェックを1命令に融合し、V8 で `ref.cast` + `br_if` の組み合わせより 10〜20% 高速
- **難易度**: Medium

#### FR-232: Wasm Streaming Compilation / Module Caching

- **対象**: `packages/cli/src/main.lisp`
- **現状**: `WebAssembly.instantiate(buffer)` で全バイト受信後にコンパイル開始
- **内容**: `WebAssembly.instantiateStreaming(fetch(url))` でダウンロードと並行コンパイルを有効化。`WebAssembly.compileStreaming` + IndexedDB でコンパイル済みモジュールを永続キャッシュ。`--streaming` フラグで生成コードを streaming API 使用に切り替え。Content-Hash ベースのキャッシュキー生成（バイナリ変更時のみ再コンパイル）
- **根拠**: 大規模 CL システム（Quicklisp 全体）の初回ロード時間を最大 60% 削減。Progressive Web Apps での必須パターン
- **難易度**: Easy

---

### Phase 45 — Wasm Spec v2.0 / v3.0 標準済み・未実装

#### FR-233: Non-trapping Float-to-int — CL `floor`/`truncate` の安全な変換

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: Wasm MVP の `i32.trunc_f32_s` は NaN・±∞・オーバーフロー時にトラップ。CL の `floor`/`ceiling`/`truncate`/`round` は NaN を渡すと実行時クラッシュ
- **内容**: Wasm Non-trapping Float-to-int Conversions（MVP v1.1、全主要ブラウザ標準化済み）の `i32.trunc_sat_f32_s`/`i32.trunc_sat_f64_u` 等を使用。NaN → 0、±∞ → INT_MAX/INT_MIN の飽和変換をエミット。CL の `(floor 1.5)` → `(i32.trunc_sat_f64_s (f64.const 1.5))` に変換
- **根拠**: 現状のコードは NaN・無限大を渡すと `WebAssembly.RuntimeError: integer overflow` でクラッシュ。CL 仕様準拠に必須
- **難易度**: Easy

#### FR-234: Sign-extension Operators — Fixnum 符号拡張の1命令化

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/primitives.lisp`
- **現状**: 8/16/32bit 符号拡張を `i32.shl` + `i32.shr_s` の2命令シフトペアで実装
- **内容**: Wasm Sign-extension Ops（MVP v1.1、全主要ブラウザ標準化済み）の `i32.extend8_s`/`i32.extend16_s`/`i64.extend8_s`/`i64.extend16_s`/`i64.extend32_s` を使用。fixnum タグビットのマスク+符号拡張を1命令に置換。CL の `(ash x -1)` で生じる符号ビット処理を最適化
- **根拠**: MVP v1.1 で標準化済みにもかかわらず未実装。fixnum boxing/unboxing ループ内での連続発行で効果が大きい
- **難易度**: Easy

#### FR-235: Multi-value Returns — CL `values` のネイティブ表現

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: `(values a b c)` の多値返却はヒープ上のベクタ経由（`vm-values` 命令 + `vm-multiple-value-bind`）。全多値呼び出しでGCプレッシャー発生
- **内容**: Wasm Multi-value（MVP v1.1 / 全主要ブラウザ標準化済み）を利用。`(values a b c)` → Wasm `(tuple a b c)` としてスタック上に直接返却。`(multiple-value-bind (x y) (floor 3.7))` → Wasm ブロック型 `(block (result f64 i32) ...)` に変換。`(values)` はゼロ返却ブロックへ
- **根拠**: 現状は多値ごとにヒープ確保。スタック直渡しへの変換で `floor`/`truncate`/`values`-heavy コードが大幅に高速化
- **難易度**: Hard

#### FR-236: JavaScript BigInt ↔ i64 Integration

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: JS → Wasm の境界で `i64` 値を渡すと `TypeError: Cannot convert BigInt to a Number`。CL の fixnum (i64) を JS に渡す手段がない
- **内容**: Wasm JS BigInt Integration（Wasm Spec v2.0、Chrome 85+ / Firefox 78+）を有効化。`i64` 引数・戻り値を JS `BigInt` と相互変換。CL の `(js-call "fetch" url timeout-ms)` で i64 パラメータが正しく伝達される。`--bigint` フラグで有効化（デフォルト: off、後方互換性のため）
- **根拠**: 現状 i64 を持つ全 import/export 関数が JS から呼び出し不能。Wasm Spec v2.0 で標準化済みにもかかわらず未対応
- **難易度**: Easy

#### FR-237: Bulk Table Operations — `table.init` / `elem.drop`

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: 関数テーブルの初期化は `table.set` ループ。データセグメントの解放手段なし
- **内容**: Wasm Bulk Memory Operations の対テーブル版（MVP v1.1 同梱）`table.init`/`table.copy`/`table.fill`/`elem.drop` を使用。起動時の `__indirect_function_table` 初期化を `table.init` 1命令で実施。動的リンク（FR-227）でサイドモジュールのテーブルを `table.copy` でマージ。`elem.drop` でリロード不要な element セグメントを解放
- **根拠**: FR-228（Bulk Memory）と対称の操作。Dynamic Linking（FR-227）の基盤として必須
- **難易度**: Easy

---

### Phase 46 — Phase 3 提案（実装フェーズ）

#### FR-238: Wide Arithmetic — 128bit 整数演算

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: CL の bignum 中間演算は `i64` 2本による手動128bit分割（`mulhu`/`mulhsu`等で6〜10命令）
- **内容**: Wasm Wide Arithmetic Proposal（Phase 3、2024年）の `i64.add128`/`i64.sub128`/`i64.mul_wide_s`/`i64.mul_wide_u` を使用。128bit 積をネイティブ2-result命令で取得。bignum キャリー伝播を `i64.add_carry`/`i64.sub_borrow` で実装
- **根拠**: Bignum 演算はSBCL・CCL等の CL 処理系の数値性能のボトルネック。128bit命令でシングルリムの乗算が 10命令→1命令
- **難易度**: Medium

#### FR-239: Custom Page Sizes — GC ヒープのページサイズ最適化

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: Wasm のメモリページサイズは固定 64KB。CL の小オブジェクト主体のヒープには粒度が粗すぎる
- **内容**: Wasm Custom Page Sizes Proposal（Phase 3、2024年）でページサイズを `4096`（4KB）または `1`（1B）に設定可能。GC ヒープを 4KB ページで管理し、`memory.grow` の最小単位を OS の mmap と揃える。世代別GC（`packages/backend/runtime/src/gc.lisp`）のページビットマップが 16倍細粒度化
- **根拠**: 64KB ページは大容量連続確保時に有利だが、小オブジェクト多数の CL ヒープでは内部断片化が大きい
- **難易度**: Medium

#### FR-240: Compact Import Section — バイナリサイズ削減

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: import セクションはモジュール名・フィールド名を UTF-8 文字列で列挙。大量インポートでバイナリヘッダが肥大化
- **内容**: Wasm Compact Import Section Proposal（Phase 3）で共通プレフィックス・インデックスによる import 名圧縮を使用。Dynamic Linking（FR-227）でサイドモジュールが数百の import を持つ場合のバイナリサイズを削減。Dead Import Elimination（FR-221）と組み合わせてヘッダ圧縮率を最大化
- **根拠**: CL システムは関数・グローバル・メモリの import 数が多い。import セクション圧縮で先頭ロード時間短縮
- **難易度**: Easy

#### FR-241: Custom Descriptors — `externref` 型付き JS バインディング

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: FR-226 の `externref` は不透明参照のみ。プロパティアクセス・メソッド呼び出しには JS ラッパー関数が必要
- **内容**: Wasm Custom Descriptors Proposal（Phase 3、2024年）で `externref` に型記述子（デスクリプタオブジェクト）を付与。`(js-get ref :property)` → Wasm デスクリプタ経由の型安全アクセス。CL のコンパイラが JS クラス定義から WIT 風の CL バインディングを自動生成。`WebAssembly.Descriptor` API との連携
- **根拠**: `externref`（FR-226）単体では JS オブジェクトの構造が不明。デスクリプタにより型チェックと最適化アクセスが可能になる
- **難易度**: Hard

---

### Phase 47 — Phase 1〜2 提案（将来対応）

#### FR-242: Extended Name Section — 人間可読デバッグ名

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: Name Section（関数名のみ）は未実装。Chrome DevTools でローカル変数・パラメータが `$var0`/`$var1` と表示される
- **内容**: Wasm Extended Name Section Proposal（Phase 2）でローカル変数名・パラメータ名・ラベル名・型名・フィールド名を Wasm バイナリに埋め込み。CL 変数名 (`:R0` → `temp-result` 等) を保持。DWARF（FR-222）より軽量なデバッグ支援。`--emit-names` フラグで有効化
- **根拠**: DWARF なしでも DevTools で CL 変数名が見える。デバッグ用途でのバイナリサイズ増加は数KB 程度
- **難易度**: Easy

#### FR-243: Memory Control — `memory.discard` によるページ返却

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: GC 後の空きページはメモリ確保済みのまま OS に返却できない。長時間実行で Wasm ヒープが肥大化し続ける
- **内容**: Wasm Memory Control Proposal（Phase 1）の `memory.discard offset len` でGC後の空きスパンを OS に返却。2世代GC（`gc.lisp`）のメジャーGC後にサバイバー領域以外を `memory.discard`。ブラウザタブの長時間実行 CL プログラムのメモリフットプリント改善
- **根拠**: WASM の `memory.grow` はページ確保のみで解放なし。`memory.discard` により初めてランタイムがメモリを OS に戻せる
- **難易度**: Medium

#### FR-244: Type Imports — モジュール間型安全 Dynamic Linking

- **対象**: `packages/backend/emit/src/wasm.lisp`, FR-227 の動的リンク基盤
- **現状**: Dynamic Linking（FR-227）でサイドモジュールをロードする際、型情報は実行時にのみ検証される
- **内容**: Wasm Type Imports Proposal（Phase 1）でホスト環境・他モジュールから型を import 可能に。`(import "cl-core" "$cons_t" (type))` で CL コアの `$cons_t` struct 型をサイドモジュールが参照。`call_ref`（FR-212）と組み合わせてモジュール境界でも型安全なディスパッチを実現
- **根拠**: 動的リンク（FR-227）の型安全版。Typed Function References（FR-212）+ Component Model（FR-206）の架け橋
- **難易度**: Hard

#### FR-245: JIT Interface — エンジンフィードバックの取り込み

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: PGO（FR-220）はinstrumented ビルドによる静的プロファイルのみ。V8/SpiderMonkey のランタイム型フィードバック（inline caches）を取得する手段なし
- **内容**: Wasm JIT Interface Proposal（Phase 1）で Wasm モジュールがエンジンの JIT フィードバックを取得可能に。V8 の IC フィードバック（most-frequent type per call site）を取得し、`defgeneric` のディスパッチを monomorphic stub にインライン化。CL の「warm-up 後に再コンパイル」パターンをWasmレベルで実現
- **根拠**: PGO（FR-220）の動的版。JIT Interface により「実行中に最適化が進む」CL 処理系の挙動がWasmで再現できる
- **難易度**: Very Hard

#### FR-246: Flexible Vectors — 可変幅 SIMD

- **対象**: `packages/backend/emit/src/wasm.lisp`, SIMD エミッタ
- **現状**: SIMD128（FR-202）は 128bit 固定幅。AVX2（256bit）・AVX-512（512bit）相当の演算は手動ループで実装
- **内容**: Wasm Flexible Vectors Proposal（Phase 1）で実行環境の最大 SIMD 幅（256/512bit）を自動検出し、ベクタ型を動的に選択。`(vec-add xs ys)` → 環境に応じて `v128x2.add` や `v512.add` を発行。CL の `map`/`reduce` の自動ベクトル化ループ生成
- **根拠**: SIMD128（FR-202）の自然な拡張。数値計算ライブラリ（CL の `linear-algebra` 相当）で 2〜4倍の追加高速化
- **難易度**: Very Hard

#### FR-247: Frozen Values — 不変定数テーブルの最適化

- **対象**: `packages/engine/compile/src/codegen.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: `defconstant` や読み取り専用ディスパッチテーブルも通常のmutable GC struct として確保される。エンジン側の immutability 最適化が適用されない
- **内容**: Wasm Frozen Values Proposal（Phase 1）の `struct.new_immutable` / `array.new_immutable` を使用。`defconstant` 束縛・マクロ展開テーブル（`*expander-head-table*`）・ディスパッチテーブルを frozen struct として生成。エンジンが write-barrier スキップ・定数畳み込みを適用可能に
- **根拠**: CL の定数が多い処理系でGCのwrite-barrierコストを削減。`*expander-head-table*` 等の大型定数ハッシュテーブルへの効果が大きい
- **難易度**: Medium

#### FR-248: Half Precision (`f16`) — 機械学習ワークロード向け

- **対象**: `packages/backend/emit/src/wasm.lisp`, 浮動小数点エミッタ
- **現状**: CL の浮動小数点型は `single-float`（f32）と `double-float`（f64）のみ対応。`f16` 型なし
- **内容**: Wasm Half Precision Proposal（Phase 1）の `f16` 型と `f16.load`/`f16.store`/`f16.add`/`f16.mul` 等を追加。CL の `short-float` を `f16` にマップ。SIMD128 の `f16x8` pack 型と連携して機械学習推論ワークロードの帯域幅を半減
- **根拠**: LLM inference・行列演算では f16 が事実上の標準。WebGPU との相互運用でも f16 が共通フォーマット
- **難易度**: Medium

#### FR-249: More Array Constructors — GC 配列の高速初期化

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/array.lisp`
- **現状**: `(make-array n :initial-element 0)` は `array.new_fixed` に `i32.const 0` を N 回プッシュ。大きな配列の初期化がO(N)命令列を生成
- **内容**: Wasm More Array Constructors Proposal（Phase 1）の `array.new_default`（全要素ゼロ初期化）/ `array.new_data`（data segment からコピー初期化）/ `array.new_fill`（指定値で初期化）を使用。`(make-array n)` → `array.new_default` 1命令。リテラル配列 `#(1 2 3)` → `array.new_data $data_seg` で静的セグメントから直接初期化
- **根拠**: `#()` リテラルや `(make-array n :initial-element x)` は CL で頻出パターン。コード生成サイズと実行時コストを両方削減
- **難易度**: Easy

#### FR-250: Multibyte Array Access — `array.load2_u` / `array.load4_u`

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/engine/vm/src/array.lisp`
- **現状**: Wasm GC 配列の要素アクセスは `array.get` で1要素ずつ。文字列スキャン・シリアライザは ループで複数要素を個別取得
- **内容**: Wasm Multibyte Array Access Proposal（Phase 1）の `array.load2_u`（2要素を i32 にパック）/ `array.load4_u`（4要素を i32 に）/ `array.store2`/`array.store4` を使用。UTF-16 文字列操作・バイナリシリアライゼーション（Mach-O builder `packages/backend/binary/src/macho.lisp` 等）でアクセス命令数を 1/2〜1/4 に削減
- **根拠**: 文字列処理は CL REPL・パーサーの内部ループで支配的。マルチバイトアクセスで String Builtins（FR-218）の補完として効果的
- **難易度**: Medium

#### FR-251: Reference-Typed Strings — Wasm ネイティブ文字列型

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: String Builtins（FR-218）は JS の WTF-16 文字列を Wasm 内部で不透明な `externref` として扱う。CL 文字列との統合が表層的
- **内容**: Wasm Reference-Typed Strings Proposal（Phase 1）で `stringref` 型を導入。CL の `simple-string` を `stringref` として直接表現。`string.length`/`string.get_codeunit`/`string.concat`/`string.compare` をネイティブ命令で発行。`stringref` は Wasm GC 型階層に統合され `eqref` のサブタイプとして扱われる
- **根拠**: String Builtins（FR-218）が `externref` 経由なのに対し、`stringref` は GC struct/array と同等の型安全性を持つ。CL の文字列最適化の最終形
- **難易度**: Hard

---

### Phase 48 — 実行モデル完全化

#### FR-252: Exception Handling v2 — `try_table` / `throw_ref` / `resume_throw`

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: FR-204 は MVP `try`/`catch`/`throw` のみ対応。例外値の再投げ（`rethrow`）は static depth インデックスが必要で動的再送不可
- **内容**: Wasm Exception Handling v2（Phase 4、Chrome 123+）の `try_table` 命令を使用。`try_table` はキャッチハンドラをブランチテーブルとして宣言し、動的なハンドラ選択が可能。`throw_ref` で例外値を参照として保持し任意のスコープから再投げ。`resume_throw` で Stack Switching（FR-205）との例外統合。CL の `handler-bind` のネスト構造を `try_table` のフラットテーブルに変換
- **根拠**: `rethrow` は静的スコープ制限があり CL の動的ハンドラチェーンに不向き。`try_table` + `throw_ref` により `handler-bind` の動的 unwinding が正確に表現できる
- **難易度**: Medium

#### FR-253: `return_call_ref` — 型付きクロージャ末尾呼び出し

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/compile/src/cps.lisp`
- **現状**: FR-143 は `return_call_indirect`（型なし関数テーブル経由）のみ。Typed Function References（FR-212）と組み合わせた型付きクロージャへの末尾呼び出しなし
- **内容**: Wasm Tail Calls Proposal（Phase 4）に含まれる `return_call_ref (type $sig)` を使用。`(call/cc fn)` や CL CPS 変換後の継続クロージャへの末尾呼び出しを `return_call_ref` でコード生成。`call_ref`（FR-212）の末尾呼び出し版として使用。Wasm GC struct を環境として持つクロージャへの末尾呼び出しでスタックオーバーフロー防止
- **根拠**: CPS 変換（`packages/engine/compile/src/cps.lisp`）後のコードは継続への末尾呼び出しが支配的。`return_call_ref` なしでは深い再帰がスタック枯渇する
- **難易度**: Medium

#### FR-254: Wasm GC Recursive Types — `rec` 型グループ

- **対象**: `packages/backend/emit/src/wasm-types.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: FR-210 の `$cons_t` struct は `(field $car eqref) (field $cdr eqref)` として定義。`$cdr` が `(ref $cons_t)` を直接参照できず `eqref` を経由するため型安全性が落ちる
- **内容**: Wasm GC `rec` 型グループ（GC Spec § 2.3.8）を使用し相互再帰する struct 型を定義。`rec ($cons_t (struct (field $car eqref) (field $cdr (ref null $cons_t))))` として `$cdr` を nullable cons ref に型付け。doubly-linked list・二分木・循環リストなどの CL 再帰データ構造を型安全に表現。`$cdr` の `ref.test $cons_t` で `endp`/`null` チェックをO(1)で実施
- **根拠**: `rec` なしでは cons の cdr を静的型で表現できない。`eqref` 経由は型情報の損失と runtime `ref.cast` のオーバーヘッドを生む
- **難易度**: Medium

#### FR-255: Wasm GC Finalization — `WeakRef` / ファイナライザ

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: FR-209〜211 の GC struct はファイナライザなし。CL の `finalize`（Trivial-GC API）・ファイルストリームの自動クローズ・外部リソースの解放手段なし
- **内容**: Wasm GC Finalization Proposal（Phase 1、検討中）の `WeakRef`/`FinalizationRegistry` 相当を利用。JS の `FinalizationRegistry` をWasm GC struct の lifetime フック として使用。`(make-instance 'file-stream ...)` でファイナライザ登録。GC 回収時に `fd_close` 相当の WASI コールを実行。Wasm ネイティブのファイナライザが標準化されるまで JS `FinalizationRegistry` をブリッジ経由で使用
- **根拠**: CL の CLOS インスタンスはリソース所有が一般的。ファイナライザなしではファイル・ソケット・FFIメモリのリークが防げない
- **難易度**: Hard

#### FR-256: `atomic.fence` — メモリバリア命令

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: FR-203（Threads）は `i32.atomic.rmw.cmpxchg` 等の RMW 命令を持つが、メモリ順序を強制するスタンドアロンの fence 命令なし
- **内容**: Wasm Threads Proposal に含まれる `atomic.fence` 命令（SeqCst バリア）を CL のスレッド同期プリミティブに使用。`bt:make-lock`/`bt:acquire-lock` の実装にフルバリアを付与。Shared Everything Threads（FR-224）の `(shared struct)` アクセス時の順序保証に使用。SBCL の `sb-thread:barrier` 相当を Wasm に移植
- **根拠**: `atomic.fence` は Wasm Threads 仕様に含まれるが codegen への接続が未実装。マルチスレッドCLコードの correctness に必須
- **難易度**: Easy

#### FR-267: Wasm GC Precise Roots / Write Barriers

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: Wasm エンジン（V8/SpiderMonkey）は GC struct の root 追跡を自動で行うが、cl-cc の2世代GC（`gc.lisp`）は独自ヒープを持ち write barrier を一切発行しない。インタージェネレーショナル参照が未追跡
- **内容**: Wasm GC struct を使用したオブジェクト（FR-210）への書き込み時に write barrier を発行。エンジンの GC と cl-cc 独自GCの協調モードを実装。Remembered set（old→young 参照集合）を Wasm `array` で管理。`struct.set` 後に `call $write_barrier` を自動挿入するコード生成パス
- **根拠**: Wasm エンジンの GC は cl-cc の独自ヒープを認識しない。Write barrier なしでは世代別GCが old→young 参照を見逃し、young オブジェクトが早期回収される
- **難易度**: Hard

---

### Phase 49 — ビルドパイプライン / セキュリティ / ツールチェーン

#### FR-257: WASI 0.3 — Async-first システムインターフェース

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/io.lisp`
- **現状**: FR-207 は WASI 0.2（同期 capability-based I/O）対応。WASI 0.3 は async-first 設計で全 I/O が非同期
- **内容**: WASI 0.3（2025年提案中）の async-first インターフェースに対応。`wasi:io/streams` の非同期 read/write を Stack Switching（FR-205）の `suspend`/`resume` でラップ。`(with-open-file (f path) ...)` が async I/O として透過的に動作。CL の `read-char`/`write-char` が WASI 0.3 非同期ストリームにマップ
- **根拠**: WASI 0.3 はWASI 0.2 の後継。サーバーサイド CL の高並行性（N:M スレッドモデル）に対応するために必要
- **難易度**: Very Hard

#### FR-258: Wasm Profiles — 機能プロファイル宣言

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: コンパイルターゲットに必要な Wasm 機能セットを宣言する手段なし。実行環境が必要な機能をサポートするか実行時まで不明
- **内容**: Wasm Profiles Proposal（2024年提案）でモジュールが依存する機能プロファイルを宣言。`./cl-cc compile --profile gc,threads,tail-call` でプロファイルセクション生成。`wasmtime`/ブラウザが起動前に互換性チェック。`--profile minimal`（MVP のみ）から `--profile full`（全提案）まで段階的ターゲット指定
- **根拠**: 機能検出の実行時コストを排除。デプロイ先環境の事前互換性検証で `WebAssembly.RuntimeError` を本番環境で踏むリスクを排除
- **難易度**: Easy

#### FR-259: Tiered Compilation 最適化ヒント — Liftoff / TurboFan 対応

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/optimize/src/optimizer.lisp`
- **現状**: V8 は全関数を Liftoff（ベースラインJIT）でコンパイル後、ホット関数のみ TurboFan で再コンパイル。cl-cc のコード生成はこの2段階を意識していない
- **内容**: Liftoff フレンドリーなコード生成（単純なレジスタ配置・深いネストを避ける）とTurboFan 最適化を促す関数マーク（`--optimize-hot` フラグ）を分離。`@optimize(2)` カスタムアノテーション（FR-225）でホット関数を TurboFan 優先コンパイルに誘導。Cold path（エラーハンドラ・型チェック失敗）を別関数に分離してTurboFan コンパイルコストを削減。PGO（FR-220）のデータをティア昇格ヒントとして活用
- **根拠**: V8 の Liftoff→TurboFan 遷移は関数単位。大きな関数は TurboFan コンパイルが遅延する。適切な関数分割でウォームアップ時間を短縮
- **難易度**: Medium

#### FR-260: LTO — リンク時クロスモジュールインライン展開

- **対象**: `packages/engine/optimize/src/optimizer.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: FR-221 はデッドインポートを除去するのみ。モジュール境界をまたいだ関数インライン展開・定数伝播なし
- **内容**: Binaryen の `--lto` フラグ（`wasm-merge` ツール）を AOT パイプライン（FR-219）に統合。Component Model（FR-206）でリンクされたサイドモジュールを LTO でマージ。`(declare (inline f))` アノテーションをLTOパスへのヒントとして伝達（FR-225 Custom Annotations 経由）。クロスモジュール定数折り畳み・デッドコード除去をリンク後に適用
- **根拠**: CL の `inline` 宣言は現状モジュール内のみ有効。LTO により Quicklisp ライブラリ境界での `inline` が機能する
- **難易度**: Hard

#### FR-261: Wasm セキュリティ — CFI / CSP / Constant-Time

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: セキュリティ関連の考慮なし。CL の暗号ライブラリ（Ironclad 等）がタイミング攻撃に脆弱な可能性
- **内容**: (1) **CFI**: `call_ref`（FR-212）に型シグネチャチェックを追加し間接呼び出しの型安全性を強化。(2) **CSP**: `'wasm-unsafe-eval'` CSP ディレクティブ不要なコード生成（動的コード生成を避けた静的 AOT バイナリ）。(3) **Constant-Time**: 暗号プリミティブの分岐を `select` 命令に変換してタイミングチャネルを排除。`--constant-time` フラグで有効化
- **根拠**: ブラウザ上の CL は同一オリジンポリシー下で動作。CSP 準拠・CFI はセキュリティ審査の必須項目
- **難易度**: Hard

#### FR-262: `WebAssembly.Exception` JS API — CL ↔ JS 例外ブリッジ

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: FR-204 は Wasm 内部の例外処理のみ。JS 側から Wasm 例外をキャッチ・検査・再送する API なし
- **内容**: `WebAssembly.Tag` / `WebAssembly.Exception` JS API（Chrome 95+）を使用。CL の `condition` オブジェクトを `WebAssembly.Tag` に紐付け。JS の `try { wasmFn() } catch(e) { if (e instanceof WebAssembly.Exception) ... }` で CL 例外を JS から処理。逆に JS の `throw` を CL の `handler-case` でキャッチする双方向ブリッジを実装
- **根拠**: FR-204 で Wasm 例外は生成できるが JS との境界で消失する。ブラウザ CL REPL のエラー表示に必須
- **難易度**: Medium

#### FR-263: Wasm Type Reflection JS API — 実行時型検査

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: Wasm GC struct 型（FR-210）の情報は Wasm バイナリ内部にのみ存在。JS デバッガ・REPL ツールが Wasm オブジェクトの型を検査できない
- **内容**: `WebAssembly.Function` の `type` プロパティ（Chrome 78+）を拡張し、GC struct/array 型のフィールド名・型情報を JS から取得可能に。CL REPL が `(describe obj)` で Wasm struct のフィールドを動的に列挙。`WebAssembly.Global` の型情報を DevTools に露出。Name Section（FR-242）と組み合わせてフィールド名を人間可読形式で提供
- **根拠**: Wasm GC（FR-210）で struct を使用すると Chrome DevTools の "Memory" パネルでフィールド名が失われる。Type Reflection により CL デバッガが Wasm オブジェクトを正しく表示できる
- **難易度**: Medium

#### FR-264: Lazy Function Bodies — 遅延関数コンパイル

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: AOT バイナリ（FR-219）は全関数のコード本体を含む。Quicklisp システム全体をロードすると数千関数のコードが一括コンパイルされ初回ロードが遅い
- **内容**: コード本体をスタブ関数（`unreachable` 1命令）に置き換え、実際の実装を別の `.wasm` セクションまたは外部リソースとして遅延ロード。`WebAssembly.instantiate` の `importObject` 経由で動的パッチ。使用頻度に基づく関数本体の分割戦略（FR-220 PGO データを活用）。Dynamic Linking（FR-227）と組み合わせてホット関数のみ初回バイナリに含める
- **根拠**: Quicklisp 全体（~1000パッケージ）をブラウザで動かす際の初回ロード時間削減。V8 の lazy compilation と同等のユーザー体験
- **難易度**: Hard

#### FR-265: 決定論的ビルド — 再現可能バイナリ

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: コンパイル出力にタイムスタンプ・非決定論的なハッシュテーブル順序が混入する可能性がある。同じソースから異なるバイナリが生成され CDN キャッシュが無効化される
- **内容**: `--deterministic` フラグで決定論的出力を保証。シンボルテーブル・型定義・エクスポートリストをソート済みで出力。タイムスタンプ・ファイルパス絶対表記をバイナリから除去。SHA-256 コンテントハッシュをカスタムセクションに埋め込み。Streaming Compilation（FR-232）の IndexedDB キャッシュキーとして使用
- **根拠**: CDN キャッシュ・供給チェーンセキュリティ・`diff` によるリリース差分検査に必須。Rust/Zig のコンパイラが先行実装済み
- **難易度**: Easy

#### FR-266: `global.atomic` — Shared Everything スレッド間グローバル共有

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: FR-203（Threads）の atomic 命令はメモリ（線形メモリ上のアドレス）にのみ作用。Wasm グローバル変数への atomic アクセスなし。Shared Everything Threads（FR-224）での共有グローバル状態に未対応
- **内容**: Wasm Shared Everything Threads に含まれる `global.atomic.get`/`global.atomic.set`/`global.atomic.rmw.*` を使用。CL の動的変数（`defvar`）をスレッド安全なグローバルとして実装。`*package*`/`*readtable*` 等の CL 標準動的変数をスレッドローカルストレージ（TLS）から shared atomic グローバルに昇格するオプション
- **根拠**: `atomic.fence`（FR-256）と対で必要。Shared Everything（FR-224）でスレッド間で動的変数を共有する際のメモリ安全性を確保
- **難易度**: Medium

#### FR-268: Wasm Decimal — IEEE 754 十進浮動小数点

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: CL の数値型は `single-float`（f32）・`double-float`（f64）のみ Wasm にマップ。金融計算で必要な十進小数（`ratio` 近似）は binary float で丸め誤差が生じる
- **内容**: Wasm Decimal Proposal（Phase 0〜1、初期段階）の `decimal32`/`decimal64`/`decimal128` 型をサポート。CL の `(/ 1 10)` → `(decimal64.const 0.1d0)` で十進精度を保持。`decimal64.add`/`decimal64.mul` 等の算術命令。CL `ratio` 型の近似実装として `decimal128` を使用
- **根拠**: WebGPU の f16 サポートと並行して Decimal も金融・科学計算分野で要求が高まっている。CL の数値タワーの完全性に寄与
- **難易度**: Hard

#### FR-269: Wasm `call_stack` Inspection — スタックイントロスペクション

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/cli/src/main.lisp`
- **現状**: CL の `(backtrace)` / `(sb-debug:print-backtrace)` 相当なし。例外発生時のスタックトレースが Wasm バイトオフセット表記のみ
- **内容**: Wasm Stack Introspection Proposal（Phase 0、2024年提案開始）が標準化されるまでの橋渡しとして `Error.captureStackTrace` + DWARF（FR-222）/ Source Maps（FR-223）を組み合わせてCLスタックフレームを再構成。将来の `WebAssembly.StackTrace` API に対応した抽象レイヤーを実装。CL の `(print-backtrace)` が CL 関数名・引数・ファイル行番号を出力。REPL のデバッガコマンド `:backtrace`/`:frame` の Wasm 実装
- **根拠**: デバッグ体験の基本。DWARF（FR-222）と Source Maps（FR-223）を実装しても `backtrace` がなければ CL プログラマは使い物にならないと感じる
- **難易度**: Hard

---

### Phase 50 — GC Null Safety / Effect Handlers / ABI / SIMD完全化

#### FR-270: Wasm GC Null Safety — `br_on_null` / `ref.as_non_null`

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/list.lisp`
- **現状**: CL の `null` チェック（`(null x)`/`(endp x)`）は `ref.test i31` + `i31.get_s` == 0 の2命令。`ref.cast` 失敗時のヌルポインタ例外が実行時まで検出されない
- **内容**: Wasm GC Null Safety 命令群（Phase 4 標準）を使用。`br_on_null $label` で null なら分岐・非 null なら non-null ref として継続。`br_on_non_null $label` はその逆。`ref.as_non_null` で nullable ref を non-nullable に昇格（失敗時 trap）。`ref.is_null` で boolean 判定。CL の `(null x)` → `br_on_null`、`(car x)` の型ガード → `ref.as_non_null $cons_t` に変換
- **根拠**: `br_on_null` は分岐と null チェックを1命令に融合し、CLOS スロットアクセス・cons 操作のガード命令数を半減。Wasm GC 仕様 § 5.4.3 に含まれ Chrome 119+ で出荷済み
- **難易度**: Easy

#### FR-271: `exnref` — 例外参照型

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: FR-252 の `throw_ref` は例外値を再送するが、その型（`exnref`）を変数として保持・検査するコード生成なし
- **内容**: Wasm Exception Handling v2 の `exnref` 型（Chrome 123+）を使用。`try_table (catch $tag $label)` でキャッチした例外を `exnref` としてスタックに積み変数に格納。`exception.get_tag`/`exception.get_payload` で例外タグ・ペイロードを検査。CL の `handler-case` の条件型マッチを `exnref` の tag 比較で実装。格納した `exnref` を後で `throw_ref` で再投げ
- **根拠**: `exnref` なしでは `(handler-case ... (condition (e) (when (typep e 'file-error) (throw_ref e))))` の「検査して再送」パターンが実装できない。FR-252 の完全実装に必須
- **難易度**: Medium

#### FR-272: Algebraic Effect Handlers — CL 条件システムの自然な表現

- **対象**: `packages/engine/vm/src/conditions.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: FR-205 の Stack Switching は第一級継続（`call/cc`）を対象。CL の条件システム（`handler-bind`/`restart-case`/`invoke-restart`）はリセット可能な非局所脱出であり、スタックを破壊しない「再開可能例外」
- **内容**: Wasm Algebraic Effect Handlers（Stack Switching の上位抽象、研究段階）を利用。`effect $restart_handler` で再開ポイントを宣言。`perform $restart_handler payload` でハンドラを呼び出し。ハンドラが `resume value` で処理を継続するか `abort` で脱出。CL の `(invoke-restart 'use-value x)` → `perform $use_value_restart x`。`restart-case` のクローズ→`effect` 宣言に変換
- **根拠**: Stack Switching（FR-205）は継続全体を捕捉するが、CL の再開可能条件は「ハンドラが呼び出し側のスタックを保持したまま戻る」。Effect Handlers はこのセマンティクスに直接対応し、Stack Switching より低コスト
- **難易度**: Very Hard

#### FR-273: `struct.atomic` / `array.atomic` — GC オブジェクトの原子フィールド操作

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/vm-clos.lisp`
- **現状**: FR-224 の Shared Everything Threads で GC struct を Worker 間共有できるが、フィールドへの atomic read/write/CAS 命令なし。mutex なしのロックフリーアルゴリズム（CAS ループ）が実装できない
- **内容**: Wasm Shared Everything Threads Proposal に含まれる `struct.atomic.get $T $field`/`struct.atomic.set $T $field`/`struct.atomic.rmw.cmpxchg $T $field` と `array.atomic.get`/`array.atomic.set` を使用。CLOS スロットへの atomic 書き込みで `bt:compare-and-swap` を実装。`array.atomic.rmw.add` でロックフリーカウンタ。CL の `(sb-ext:atomic-incf (slot-value obj 'count))` → `struct.atomic.rmw.add`
- **根拠**: ロックベースの同期は Wasm で `memory.atomic.wait` + mutex が必要で高コスト。struct/array への CAS 命令で CLOS スロットのロックフリー更新が実現
- **難易度**: Hard

#### FR-274: WASI Worlds 完全対応 — `wasi:nn` / `wasi:http` / `wasi:cli`

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/io.lisp`, `packages/cli/src/main.lisp`
- **現状**: FR-207 は `wasi:filesystem`/`wasi:sockets`/`wasi:clocks` を列挙。`wasi:nn`（Neural Network）・`wasi:http`（HTTP クライアント）・`wasi:cli`（標準 CLI world）が未対応
- **内容**: (1) **`wasi:nn`**: WASI Neural Network Interface（wasi-nn 0.2）の `load`/`init_execution_context`/`set_input`/`compute`/`get_output` を CL バインディングとして公開。`(wasi:nn-infer model inputs)` API。(2) **`wasi:http`**: HTTP/1.1・HTTP/2 クライアント（`wasi:http/outgoing-handler`）を `(http-request :get url)` として実装。(3) **`wasi:cli`**: argv・環境変数・stdin/stdout/stderr の標準 CLI world。`./cl-cc compile --world cli` で CLI アプリ生成
- **根拠**: `wasi:nn` は ONNX モデルの CL 推論に必要。`wasi:http` は Quicklisp のパッケージダウンロードを WASI 環境で実現。`wasi:cli` は `wasmtime run` での標準実行形式
- **難易度**: Hard

#### FR-275: `WebAssembly.Module` postMessage — Worker 間モジュール共有

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: FR-227 の Dynamic Linking は `WebAssembly.instantiate` でモジュールをロード。複数 Worker が同一モジュールを使う場合、各 Worker が独立してコンパイルするため N 倍のコンパイル時間
- **内容**: `WebAssembly.Module` は `postMessage` で `Transferable` として Worker に転送可能（Chrome 全バージョン対応）。コンパイル済みモジュールを `postMessage(module, [module])` で Worker に配布し `WebAssembly.instantiate(module, imports)` で即時インスタンス化。Streaming Compilation（FR-232）と組み合わせてメインスレッドで1回コンパイル→全 Worker に転送。CL の `bt:make-thread` がバックグラウンドで Worker を起動する際にモジュールを転送
- **根拠**: 4 Worker でコンパイル4回→1回に削減。Shared Everything Threads（FR-224）の前提として Worker 間でモジュールを共有する必要がある
- **難易度**: Easy

#### FR-276: Wasm Import Maps — ブラウザモジュール名前解決

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: Wasm モジュールの import は文字列リテラル（`"wasi:filesystem/types"`）で固定。開発・本番・テスト環境でモジュール URL を切り替える手段がコード変更のみ
- **内容**: HTML `<script type="importmap">` の Wasm 版（Wasm Import Maps 提案）で モジュール名 → URL のマッピングを外部設定として分離。`wasi:filesystem` → `https://cdn.example.com/wasi-fs-0.2.wasm` のような環境別マッピング。`./cl-cc compile --import-map prod.json` で本番用 URL を生成バイナリに埋め込まず外部設定に委譲。Dynamic Linking（FR-227）の URL 解決層として統合
- **根拠**: Quicklisp パッケージの CDN 配布で開発版・本番版を切り替える標準パターン。Deploy パイプラインでコード変更なしにモジュール URL を差し替え可能
- **難易度**: Medium

#### FR-277: CL ABI / シンボル名マングリング — クロス言語 FFI

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: CL 関数の Wasm export 名はシンボル名そのまま（例: `FOO:BAR-BAZ`）。C/Rust/JS からの呼び出しで特殊文字が問題になる。`defpackage`・`defun` の名前空間情報が export 名から失われる
- **内容**: CL ABI マングリング規約を定義。`(export #'foo:bar-baz)` → `_ZN3FOO7BAR_BAZE`（C++ like）または `foo__bar_baz`（snake_case）。`--abi c` / `--abi rust` / `--abi js` フラグで対象言語別マングリングを選択。Component Model（FR-206）の WIT バインディングとの名前対応表を自動生成。`extern "cl" { fn foo_bar_baz(x: i64) -> i64; }` 形式の Rust バインディングを出力
- **根拠**: ESM Integration（FR-230）・Dynamic Linking（FR-227）で他言語から CL 関数を呼ぶ際の名前衝突・特殊文字エスケープの標準化。C ABI 互換なしでは既存ライブラリとのリンクが困難
- **難易度**: Medium

#### FR-278: SIMD 命令完全化 — `i8x16.shuffle` / dot product / 型変換

- **対象**: `packages/backend/emit/src/wasm.lisp`, SIMD エミッタ
- **現状**: FR-202 は `v128.load`/`i32x4.add`/`f64x2.mul` 等の基本命令のみ列挙。`i8x16.shuffle`（任意置換）・`i32x4.dot_i16x8_s`（ドット積）・SIMD 型変換命令群が未記載
- **内容**: 以下の命令を SIMD エミッタに追加。(1) **Shuffle/Permute**: `i8x16.shuffle` で任意レーン置換（行列転置・AES 操作）。(2) **Dot Product**: `i32x4.dot_i16x8_s` で INT16 4要素ドット積→INT32（NN の conv layer）。(3) **型変換**: `i32x4.trunc_sat_f32x4_s`（f32x4→i32x4 飽和変換）、`f32x4.convert_i32x4_s`（i32x4→f32x4）、`f64x2.promote_low_f32x4`（f32x4 下半→f64x2）。(4) **Narrow/Widen**: `i16x8.narrow_i32x4_s`（幅縮小）、`i32x4.widen_low_i16x8_s`（幅拡大）
- **根拠**: `i8x16.shuffle` は AES 実装・LZ77 辞書探索で必須。`dot_i16x8_s` は INT8 量子化モデルの conv layer 演算コアで WASI-NN（FR-274）と連携
- **難易度**: Medium

#### FR-279: Typed `select` — 参照型の条件選択

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `(if cond x y)` で `x`/`y` が GC struct ref の場合、`if`/`else`/`end` ブロックで実装。`select` 命令はスカラー型のみ対応でリファレンス型に使用不可
- **内容**: Wasm Typed `select`（GC Proposal 標準、`select (type (ref null $t))`）を使用。`(if cond a b)` で `a`/`b` が同型の GC ref → `select (type (ref null $cons_t))` 1命令に変換。`(or x y)` のショートサーキット最適化で非 null ref を `select` で選択。`(if (null x) default x)` → `br_on_null`（FR-270）+ `select` のペアで実装
- **根拠**: `if`/`else`/`end` ブロックは3命令+ネストレベル増加。GC ref の多発する CL コード（alist 操作・ツリー探索）で `select` 使用によりバイナリサイズ削減
- **難易度**: Easy

#### FR-280: Wasm 初期化順序 — `__wasm_call_ctors` / toplevel 実行

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: CL の `eval-when (:load-toplevel)` フォーム・`defvar`・`defparameter` の実行順序がWasm モジュール instantiation 後に保証されない。複数モジュールのコンストラクタ実行順序が未定義
- **内容**: `__wasm_call_ctors` 関数（Emscripten/wasm-ld 慣習）を生成。モジュール instantiation 直後に embedder が `__wasm_call_ctors()` を呼び出す規約に準拠。ASDF の `component-depends-on` 順序を `__wasm_call_ctors` の呼び出しシーケンスに反映。Dynamic Linking（FR-227）でサイドモジュールのコンストラクタをメインモジュールの `__wasm_call_ctors` から順次呼び出し。`eval-when (:load-toplevel)` の逆順ファイナライザを `__wasm_call_dtors` として登録
- **根拠**: ASDF の依存グラフに基づく初期化順序を Wasm で再現。`defvar *db* (make-hash-table)` の後に `(setf (gethash :key *db*) val)` が実行されることを保証
- **難易度**: Medium

#### FR-281: `array.new_elem` — Element Segment からの配列初期化

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: FR-249（More Array Constructors）は `array.new_data`（data segment）を記載。`array.new_elem`（element segment、関数参照・GC ref の配列）が未記載
- **内容**: `array.new_elem $array_type $elem_seg_idx` で element segment（関数参照テーブル・GC object テーブル）から GC 配列を直接初期化。CL の `#(#'foo #'bar #'baz)` 関数参照ベクタ → element segment + `array.new_elem` で静的初期化。ディスパッチテーブル（`*expander-head-table*` のメソッドクロージャ列）をセグメント初期化で起動時コスト削減
- **根拠**: 関数参照ベクタ（CLOS メソッドテーブル等）の初期化を `array.new_elem` 1命令に置換。起動時の `table.set` ループを排除
- **難易度**: Easy

#### FR-282: Wasm Abstract Types — 不透明ホストオブジェクト

- **対象**: `packages/backend/emit/src/wasm-types.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: FR-226 の `externref` は JS オブジェクトを不透明に保持するが型情報なし。Wasm GC struct への不正キャスト（`ref.cast $cons_t` を JS DOM ノードに適用等）を型システムで防げない
- **内容**: Wasm Abstract Types Proposal（GC 型システムの拡張、検討中）で定義を持たない import 型を宣言。`(import "env" "$DomNode" (type (sub $opaque)))` でモジュールが構造を知らない型を import。CL の `(make-instance 'js-dom-node :ref ref)` の `:ref` スロット型を `(ref $DomNode)` として宣言。`ref.cast $DomNode` は同じ abstract type のインスタンスにのみ成功し、GC struct への誤キャストを型エラーに
- **根拠**: `externref`（FR-226）は型なし・Abstract Types は型あり不透明参照。セキュリティサンドボックス（FR-261）の型安全基盤。WASI capability オブジェクト（ファイルハンドル等）を abstract type として扱い不正操作を防止
- **難易度**: Hard

---

### Phase 51 — GC 型完全化 / ABI 補完 / WASI 拡張

#### FR-283: Wasm GC Packed Fields — `i8`/`i16` 圧縮フィールド

- **対象**: `packages/backend/emit/src/wasm-types.lisp`, `packages/engine/vm/src/strings.lisp`
- **現状**: GC struct フィールドは最小 `i32`。`simple-base-string`（8bit 文字）や bit-vector の要素が 32bit にパディングされメモリ使用量が 4倍
- **内容**: Wasm GC Packed Field Types（GC Spec § 2.3.7）の `i8`/`i16` フィールド宣言を使用。`(field $char i8)` で ASCII 文字を 1byte で格納。`struct.get_s $t $f`/`struct.get_u $t $f` で符号あり/なし拡張読み出し。`array (mut i8)` で `simple-base-string` を実装。bit-vector を `array (mut i8)` のビットマスクで表現
- **根拠**: `simple-base-string` は CL の最も一般的な文字列型。`i8` packed field なしでは文字列メモリが 4倍に膨らむ。String Builtins（FR-218）・Multibyte Array Access（FR-250）の前提
- **難易度**: Medium

#### FR-284: Wasm GC Bulk Array Ops — `array.init_data` / `array.init_elem` / `array.fill`

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/array.lisp`
- **現状**: FR-211 は `array.copy` のみ記載。`array.fill`（一括値書き込み）・`array.init_data`（data segment からコピー）・`array.init_elem`（element segment からコピー）が未記載
- **内容**: Wasm GC Bulk Array Operations（GC Spec 標準）を追加。`array.fill $t val n` で配列の n 要素を val で一括初期化（`(fill vec 0 :start 0 :end n)` → 1命令）。`array.init_data $t $seg offset n` で data segment から直接コピー（文字列リテラルの初期化を静的データから展開）。`array.init_elem $t $seg offset n` で element segment から関数参照配列を初期化（FR-281 と統合）
- **根拠**: FR-228（Bulk Memory）の GC 配列版。`(fill vector 0)` が `array.fill` 1命令になり、文字列リテラル `"hello"` が `array.init_data` で data segment から直接展開される
- **難易度**: Easy

#### FR-285: `ref.eq` — `eqref` 同一性比較

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/vm-execute.lisp`
- **現状**: CL の `(eq x y)` は `vm-eq` 命令で実装。GC struct 参照の同一性チェックに `ref.test`＋アドレス比較を使用（正確ではない）
- **内容**: Wasm GC `ref.eq` 命令（`eqref` の同一性比較、GC Spec 標準）を使用。`(eq x y)` → `ref.eq` 1命令。`i31ref`・struct ref・array ref・null の全 `eqref` サブタイプ間で pointer equality を判定。`(eql x y)` の数値以外パスも `ref.eq` に統合。`assoc`/`member` の `eq` ベースのリスト操作がホットパスで1命令になる
- **根拠**: `eq` は CL で最も呼ばれる比較演算の一つ。現状の複数命令実装を `ref.eq` 1命令に置換することで `assoc`/`rassoc`/`member` のループが高速化
- **難易度**: Easy

#### FR-286: `any.convert_extern` / `extern.convert_any` — JS ↔ GC 型ブリッジ

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: FR-226（`externref`）と FR-209（`i31ref`）・FR-210（struct ref）は別の型階層。JS から受け取った `externref` を Wasm GC 型として扱う変換命令なし
- **内容**: Wasm GC 型変換命令（GC Spec 標準）`any.convert_extern`（`externref` → `anyref`）/ `extern.convert_any`（`anyref` → `externref`）を使用。JS の `Array`/`Map` を `any.convert_extern` で `anyref` に変換後 `ref.cast` で GC struct に昇格。CL の `(js->cl js-array)` が変換パスを自動選択。`externref`（JS 不透明）と `anyref`（Wasm GC 管理）の統一インターフェース
- **根拠**: `externref`（FR-226）は Wasm GC の外側にあり `ref.cast` できない。`any.convert_extern` により JS オブジェクトを Wasm GC 型システムに取り込む唯一のパス
- **難易度**: Medium

#### FR-287: Wasm Startup Snapshots — V8 ヒープスナップショット

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: 毎回の起動時に全 `defvar`・`defparameter`・グローバル初期化（FR-280 `__wasm_call_ctors`）が実行される。Quicklisp システム全体のロードで数秒の起動コスト
- **内容**: V8 の `--wasm-lazy-validation` + Wasm Snapshot API（Node.js `v8.serialize`/`v8.deserialize` のWasm版）を活用。初期化済みの Wasm GC ヒープ状態をシリアライズしてスナップショット `.wasm.snap` ファイルに保存。次回起動時はスナップショットから直接 resume。`./cl-cc compile --snapshot` で snapshot 付きバイナリを生成。Chrome の `WebAssembly.compileStreaming` + IndexedDB（FR-232）と連携してブラウザ側でも永続化
- **根拠**: Node.js の startup snapshot（`node --build-snapshot`）と同等の機能。SBCL の `save-lisp-and-die` のWasm版。起動時間を数秒→数十ミリ秒に削減
- **難易度**: Very Hard

#### FR-288: Wasm インクリメンタル REPL コンパイル

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `./cl-cc repl` はVM上でインタープリタ実行。Wasm バックエンドへのコンパイルはモジュール単位で全体再コンパイルが必要
- **内容**: REPL で入力された S 式を単一関数の Wasm モジュールとしてその場でコンパイル→`WebAssembly.instantiate` → Dynamic Linking（FR-227）で既存モジュールの関数テーブルにパッチ。`(defun f (x) ...)` REPL 入力 → 新関数を既存モジュールにリンク。Lazy Function Bodies（FR-264）と組み合わせてインクリメンタルビルドを実現。`*repl-pool-instructions*`（既存の REPL プール）をWasmバックエンドで置換
- **根拠**: CL の REPL 体験の核心。現状の VM インタープリタ実行よりWasm JIT実行の方が高速。ブラウザ上での CL REPL に必須
- **難易度**: Hard

#### FR-289: GC Struct as Exception Payload — 構造化例外値

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: FR-252（EH v2）・FR-271（`exnref`）は例外の再送・型検査を記載。CL の condition オブジェクト（CLOS instance = GC struct）を直接例外ペイロードとして格納する方法が未記載
- **内容**: Wasm `throw $tag (ref $condition_t)` で GC struct（`$condition_t`）を直接例外値として投げる。`try_table (catch $condition_tag $label)` でキャッチ時にスタックに `(ref $condition_t)` が積まれる。`handler-case` の条件型マッチを `(ref.cast $file_error_t)`（FR-231 `br_on_cast`）で実装。`exnref`（FR-271）を介さずに直接 GC struct としてハンドラに渡す高速パス
- **根拠**: 現状は condition オブジェクトをヒープに確保後ポインタを整数エンコードして `throw`。GC struct ペイロードにより型安全かつゼロコピーで condition を伝達
- **難易度**: Medium

#### FR-290: `func.bind` — Wasm レベル部分適用

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: CL の `(lambda (x) (f captured x))` は Wasm GC closure struct（FR-144）として実装。クロージャ生成のたびに struct 確保＋`array.new_fixed` が発生
- **内容**: Wasm Stack Switching 関連の `func.bind` 命令（提案段階）で既存関数の先頭引数を束縛した新しい `funcref` を生成。`(curry #'cons :atom)` → `func.bind $cons_sig :atom` で struct 確保なしに部分適用。Typed Function References（FR-212）と組み合わせて束縛済み関数を `call_ref` で呼び出し。`mapcar`/`funcall` の高頻度クロージャ生成パスに適用
- **根拠**: クロージャの大半は単一引数を束縛するだけ。`func.bind` により struct 確保を省き GC プレッシャーを削減
- **難易度**: Hard

#### FR-291: Extended Const `ref.func` — 関数参照のグローバル定数

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: FR-215（Extended Const Expressions）は `global.get`/`i32.add` 等の算術のみ対応。`ref.func` を global 初期化式に使えないため、関数参照グローバルが実行時初期化になる
- **内容**: Wasm Extended Const Expressions の `ref.func` サポート（仕様拡張、提案中）を利用。`(defparameter *callback* #'my-fn)` → `(global $callback (ref func) (ref.func $my_fn))` として静的宣言。`defconstant` な関数参照テーブルを `global` 宣言に昇格。起動時の `__wasm_call_ctors`（FR-280）での関数参照初期化コストを排除
- **根拠**: 関数参照グローバルは `(global.get $callback)` 1命令でアクセスできるが、現状は実行時初期化が必要。`ref.func` in const expr により静的リンク時に解決
- **難易度**: Easy

#### FR-292: `call_indirect` × `table64` 統合

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: FR-229（`table64`）と既存の `call_indirect` のインデックス型（`i32`）の相互作用が未記載。`table64` 有効時に `call_indirect` のインデックスを `i32` から `i64` に昇格する必要がある
- **内容**: `table64` 有効時の `call_indirect (type $sig) (table $idx i64)` でテーブルインデックスを `i64` で指定。`--target wasm64` ビルドで `call_indirect` のインデックス計算を全て `i64` に統一。ディスパッチテーブル（CLOS generic function table）の `i64` インデックス計算パスを codegen に追加。Memory64（FR-213）+ `table64`（FR-229）+ `call_indirect` の3方向統合テストを追加
- **根拠**: Memory64 ビルドで `call_indirect` だけが `i32` インデックスのままだと型ミスマッチ。`table64` との統合を明示的に設計しないとコーナーケースでトラップ
- **難易度**: Medium

#### FR-293: Multi-memory Atomics — 複数 Shared Memory 間のアトミック操作

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: FR-203（Threads）の `i32.atomic.rmw.*` は `memory 0` にのみ作用。FR-208（Multiple Memories）でヒープを別メモリに分離した場合、GC ヒープ（`memory 1`）への atomic 操作が未定義
- **内容**: Multi-memory + Threads の組み合わせ（`i32.atomic.load (memory 1) offset`）を使用。GC ヒープ専用の shared memory（`memory 1`）に対するアトミック read/write/CAS を生成。スタック（`memory 0`）とヒープ（`memory 1`）の分離を保ちつつスレッド安全な GC 操作を実現。`memory.atomic.wait`/`notify` を `memory 1` ベースで実装
- **根拠**: Multiple Memories（FR-208）とThreads（FR-203）を同時使用するとメモリインデックス指定の atomic 命令が必要。現状の codegen は `memory 0` 固定
- **難易度**: Medium

#### FR-294: Passive Element / Data Segments — 遅延セグメントロード

- **対象**: `packages/backend/emit/src/wasm.lisp`
- **現状**: data segment と element segment は instantiation 時に自動適用（active segment）。全定数データが起動時にメモリにコピーされ初期化時間に影響
- **内容**: Wasm Bulk Memory（FR-228）に含まれる passive segment を使用。`(data passive "...")` で宣言した文字列定数・定数テーブルを instantiation 時にコピーせず保持。`memory.init $seg offset len` で必要時に遅延ロード。`data.drop $seg` で使用済みセグメントを解放。Lazy Function Bodies（FR-264）と組み合わせてホットパスのデータのみを初期化時にアクティブロード
- **根拠**: 巨大な定数プール（読み取り専用文字列テーブル・数値定数テーブル）の起動時コピーを遅延化。初期ロード時間の削減に直結
- **難易度**: Easy

#### FR-295: String Transcoding 完全実装 — `string.decode` / `string.encode` 全バリアント

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: FR-218 は `string.new_utf8_array`/`string.encode_wtf16_array`/`string.measure_wtf16` を記載。UTF-8 decode（`string.new_wtf8_array`/`string.new_lossy_utf8_array`）・Latin-1 encode/decode・WTF-8 round-trip が未記載
- **内容**: Wasm String Builtins（FR-218）の全エンコーディングバリアントを追加実装。`string.new_wtf8_array`（WTF-8 decode）・`string.new_lossy_utf8_array`（lossy UTF-8）・`string.from_code_point`（コードポイント→文字列）・`string.encode_utf8_array`（UTF-8 encode）・`string.encode_wtf8_array`（WTF-8 encode）・`string.measure_utf8`/`string.measure_wtf8`。CL の `babel:encode-string-to-octets`/`babel:decode-string-from-octets` の各エンコーディングを上記命令にマップ
- **根拠**: CL は UTF-8/UTF-16/Latin-1 の相互変換が日常的。全バリアントの命令化でエンコーディング変換ループが消える
- **難易度**: Medium

#### FR-296: WASI Extended Worlds — `wasi:keyvalue` / `wasi:messaging` / `wasi:sql`

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/io.lisp`
- **現状**: FR-274 は `wasi:nn`/`wasi:http`/`wasi:cli` を記載。Key-Value ストア・メッセージング・SQL インターフェースが未対応
- **内容**: (1) **`wasi:keyvalue`**（WASI 0.2 標準化済み）: `get`/`set`/`delete`/`exists` を `(kv-get store key)` として CL バインディング。Redis/Cloudflare KV/NATS KV に対応。(2) **`wasi:messaging`**（提案中）: パブサブメッセージング。`(messaging-publish topic payload)` で Kafka/NATS へ送信。(3) **`wasi:sql`**（提案中）: SQL クエリインターフェース。`(sql-query conn "SELECT ...")` で CL から直接 DB アクセス。`wasi:http` + `wasi:keyvalue` でサーバーレス CL Web アプリを構築
- **根拠**: Cloudflare Workers・Fastly Compute 等のエッジ環境では `wasi:keyvalue` が標準 KV アクセス手段。CL の `cl-redis`/`cl-dbi` の Wasm バックエンドとして活用
- **難易度**: Hard

---

### Phase 52 — ブラウザ統合 / SIMD 補完 / セキュリティ基盤

#### FR-297: COOP / COEP — SharedArrayBuffer の前提条件

- **対象**: `packages/cli/src/main.lisp`, ドキュメント
- **現状**: FR-203（Threads）は SharedArrayBuffer を前提とするが、Chrome 92+ でクロスオリジン分離なしに SharedArrayBuffer を使用すると `TypeError: SharedArrayBuffer is not defined` が発生
- **内容**: `Cross-Origin-Opener-Policy: same-origin` と `Cross-Origin-Embedder-Policy: require-corp`（または `credentialless`）ヘッダを Wasm ホストサーバーに設定する要件をビルドパイプラインに統合。`./cl-cc serve --threads` で開発サーバーが自動的に COOP/COEP ヘッダを付与。デプロイガイドに Cloudflare Workers / Nginx / Apache の設定例を記載。`SharedArrayBuffer` 非対応環境では自動的に単一スレッドフォールバック（FR-258 Profiles と連携）
- **根拠**: Threads（FR-203）・Shared Everything（FR-224）・Worker postMessage（FR-275）はすべて SharedArrayBuffer に依存するが、COOP/COEP なしでは Safari/Chrome で無効化される。最も見落とされやすいデプロイ時の落とし穴
- **難易度**: Easy

#### FR-298: `i8x16.swizzle` — 可変インデックス SIMD レーン置換

- **対象**: `packages/backend/emit/src/wasm.lisp`, SIMD エミッタ
- **現状**: FR-278 の `i8x16.shuffle` は静的即値インデックスによる固定置換のみ。実行時に計算されるインデックスでのレーン置換が不可
- **内容**: Wasm SIMD128 の `i8x16.swizzle` 命令を追加。可変インデックスベクタ（別の `v128` レジスタ）でレーン置換。AES SubBytes ステップ（256 エントリ S-box の 16 並列ルックアップ）・LZ77 辞書探索・Base64 エンコードテーブルルックアップに使用。CL の `(svref lookup-table idx)` のベクトル化版として `i8x16.swizzle` をエミット
- **根拠**: `shuffle`（静的）と `swizzle`（動的）は用途が異なる。暗号・圧縮アルゴリズムでは動的置換が支配的。SIMD128 標準命令として全主要ブラウザ出荷済み
- **難易度**: Easy

#### FR-299: WASI `wasi:random` / `wasi:crypto` — セキュア乱数・暗号

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: CL の `(cl:random n)` は内部 PRNG（メルセンヌツイスタ等）で実装。暗号学的に安全な乱数源・ハードウェア AES 命令への標準アクセスなし
- **内容**: (1) **`wasi:random`**（WASI 0.2 標準）: `get-random-bytes` / `get-random-u64` で OS 乱数源（`/dev/urandom` 相当）を取得。`(cl:random n)` のシード・`(secure-random n)` の実装に使用。(2) **`wasi:crypto`**（WASI 提案中）: AES-GCM・ChaCha20-Poly1305・SHA-256/512・HMAC の標準インターフェース。CL の Ironclad ライブラリが `wasi:crypto` バックエンドを選択可能に。ブラウザ環境では `crypto.getRandomValues` にフォールバック
- **根拠**: `wasi:random` は WASI 0.2 に含まれ wasmtime / wasmer で標準対応済み。セキュア乱数なしでは UUID 生成・セッション管理が危険
- **難易度**: Medium

#### FR-300: Multi-memory Bulk Copy — メモリ間 `memory.copy`

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: FR-228（Bulk Memory）の `memory.copy` は同一メモリ内のコピーのみ。FR-208（Multiple Memories）でスタック・ヒープ・文字列インターン領域を分離した場合、メモリ間コピーが `i32.load`/`i32.store` ループに後退
- **内容**: Wasm Multi-memory + Bulk Memory の組み合わせによる `memory.copy (memory $dst) $dst_off (memory $src) $src_off len` を使用。GC ヒープ（`memory 1`）からスタック（`memory 0`）への大量データコピーを1命令化。`marshal`/`unmarshal`（FFI 境界のデータコピー）・`string-to-octets` の跨ぎコピーに適用
- **根拠**: Multiple Memories（FR-208）を有効化した瞬間にメモリ間コピーが性能ボトルネックになる。Multi-memory bulk copy なしでは FR-208 の分離メリットが帳消しになる
- **難易度**: Easy

#### FR-301: `cont.throw` — 中断継続への例外伝播

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: FR-205（Stack Switching）は `cont.new`/`cont.bind`/`resume`/`suspend` を記載。中断（suspend）された継続に対して例外を投げ込む `cont.throw` が未記載
- **内容**: Wasm Stack Switching Proposal の `cont.throw $tag $cont` 命令を使用。`(signal condition)` を処理中の継続が `suspend` している場合、外部から `cont.throw` で condition を注入。SBCL の `sb-thread:interrupt-thread` 相当をWasmで実現。非同期シグナル（タイムアウト・Ctrl-C）を中断コルーチンに `cont.throw` で伝達。FR-272（Algebraic Effect Handlers）の `abort` セマンティクスの実装基盤
- **根拠**: `resume` だけでは「継続を正常再開する」しかできない。`cont.throw` により非同期例外・強制中断・タイムアウトを継続ベースのコードに伝達できる
- **難易度**: Hard

#### FR-302: ServiceWorker + Wasm — オフライン CL Web アプリ

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: Wasm モジュールのキャッシュは IndexedDB（FR-232）のみ。ネットワーク断絶時に `.wasm` の再フェッチが発生しアプリが動作しない
- **内容**: Service Worker（`sw.js`）で `.wasm` バイナリを Cache Storage にキャッシュ。`./cl-cc compile --pwa` で Service Worker 登録コード + `manifest.json` を自動生成。オフライン時は Cache Storage から `.wasm` を配信。Streaming Compilation（FR-232）と組み合わせてキャッシュからのインスタンス化を高速化。WASI CLI world（FR-274）を利用した CL スクリプトをブラウザオフラインで実行
- **根拠**: Progressive Web Apps の標準パターン。CL の REPL をオフライン対応にするには ServiceWorker が必須。CDN キャッシュとは独立して動作する
- **難易度**: Medium

#### FR-303: SIMD Lane Load / Store — `v128.load_lane` / `v128.load_splat`

- **対象**: `packages/backend/emit/src/wasm.lisp`, SIMD エミッタ
- **現状**: FR-202 は `v128.load`（16byte 一括ロード）のみ記載。個別レーンのロード・スプラット（1スカラー→全レーン複製）・レーン単位ストアが未記載
- **内容**: Wasm SIMD128 の以下命令を追加。(1) **Load Splat**: `v128.load8_splat`/`load16_splat`/`load32_splat`/`load64_splat` — スカラーを全レーンに複製（スカラー×ベクタ演算の定数ブロードキャスト）。(2) **Load Lane**: `v128.load8_lane`/`load16_lane`/`load32_lane`/`load64_lane $lane` — 既存ベクタの1レーンのみ更新。(3) **Store Lane**: `v128.store8_lane`/`store16_lane`/`store32_lane`/`store64_lane $lane` — ベクタの1レーンのみをメモリに書き出し。CL の `(aref vec i)` の SIMD スカラー操作をレーン命令にマップ
- **根拠**: `v128.load_splat` なしではスカラー×ベクタの乗算が「スカラーを16回ロードしてベクタ構築」になる。SIMD 数値演算ループの大部分がこのパターンに依存
- **難易度**: Medium

#### FR-304: `v128.any_true` / SIMD Boolean Reduction

- **対象**: `packages/backend/emit/src/wasm.lisp`, SIMD エミッタ
- **現状**: SIMD 比較（FR-278 の型変換に含意）は `v128` を返す。その `v128` が「どれか1つでも真か」「全て真か」を1命令で判定する手段が未記載
- **内容**: Wasm SIMD128 の以下 boolean reduction 命令を追加。(1) `v128.any_true` — いずれかのレーンが非ゼロなら 1 を返す（`(some #'identity vec)` のベクトル化）。(2) `i8x16.all_true`/`i16x8.all_true`/`i32x4.all_true`/`i64x2.all_true` — 全レーンが非ゼロなら 1 を返す（`(every #'identity vec)` のベクトル化）。(3) `i8x16.bitmask`/`i16x8.bitmask`/`i32x4.bitmask`/`i64x2.bitmask` — 各レーンの最上位ビットを1整数にパック（`find-if` の SIMD 高速実装）。CL の `(find-if #'zerop vec)` → `i32x4.bitmask` + `ctz`（count trailing zeros）
- **根拠**: SIMD 比較後の分岐判定に必須。`any_true`/`all_true` なしでは SIMD ループの早期終了・境界チェックが実装できない
- **難易度**: Easy

#### FR-305: `WebAssembly.validate()` — 静的検証 API

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: Wasm バイナリの正当性は `WebAssembly.instantiate` 実行時に初めて検証。無効なバイナリが本番環境で `CompileError` を起こすまで検出できない
- **内容**: `WebAssembly.validate(buffer)` JS API（全主要ブラウザ対応）をビルドパイプラインに統合。AOT ビルド（FR-219）後に `WebAssembly.validate` を自動実行して不正バイナリを CI で検出。`wasmtime validate` コマンドとのダブルチェック体制。`--validate` フラグで生成直後に検証を実行。型エラー・スタック不整合・GC 型循環を本番前に検出
- **根拠**: Wasm バリデーションは線形時間で完了。CI パイプラインでの静的検証によりデプロイ後の `CompileError` を排除。Security（FR-261）の型安全検証の一環
- **難易度**: Easy

#### FR-306: `memory.atomic.wait` タイムアウト — 有限時間ブロック待機

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: FR-203（Threads）の `memory.atomic.wait32`/`wait64` は無限時間ブロックまたは即時リターンのみ言及。タイムアウト付き有限時間待機が未記載
- **内容**: `memory.atomic.wait32 addr expected timeout_ns` のタイムアウト引数（ナノ秒、`i64`、`-1` = 無限）を明示的に使用。CL の `(bt:with-timeout (seconds) ...)` → `memory.atomic.wait32` + タイムアウト値（`(* seconds 1_000_000_000)`）。デッドロック検出・看視犬タイマーの実装。タイムアウト時の戻り値（`2` = timed-out）を CL の `timeout` condition にマップ
- **根拠**: 無限待機は本番 CL サーバーでデッドロック時に永久停止する。タイムアウトは `bt:with-timeout` の正確な Wasm 実装に必須であり、FR-203 の記述では明示されていなかった
- **難易度**: Easy

#### FR-307: Subresource Integrity for Wasm — `.wasm` ファイルの完全性検証

- **対象**: `packages/cli/src/main.lisp`
- **現状**: `<script src="main.wasm">` や `fetch("main.wasm")` で読み込む `.wasm` ファイルにコンテンツ完全性チェックなし。CDN 改ざんや中間者攻撃で悪意ある Wasm が実行される可能性
- **内容**: Wasm ファイルに Subresource Integrity（SRI）ハッシュを付与。`./cl-cc compile` が `main.wasm` の SHA-256/SHA-384 ハッシュを計算し `integrity="sha256-..."` 属性値を出力。`<link rel="preload" href="main.wasm" as="fetch" integrity="sha384-..." crossorigin>` を生成 HTML に自動挿入。`fetch` ベースのロードコードにも `{ integrity: "sha384-..." }` を付与。決定論的ビルド（FR-265）と組み合わせて再現可能なハッシュを保証
- **根拠**: 供給チェーンセキュリティ（FR-261）の具体的実装。CDN キャッシュポイズニングや JS bundle 改ざんと同等の攻撃ベクタを防御。npm パッケージの `integrity` フィールドと同じ仕組み
- **難易度**: Easy

---

### Phase 53 — 仕様補完 / 運用 / エコシステム

#### FR-308: Relaxed Dead Code Validation — デッドコードの型チェック緩和

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/compile/src/codegen.lisp`
- **現状**: `(error "unreachable")` や `(return-from block)` 後の CL コードはWasm的には dead code だが、Wasm バリデーターは dead code にも型整合性を要求する。現状の codegen は dead path に型を合わせるダミー命令（`unreachable` + 型強制）を挿入
- **内容**: Wasm Relaxed Dead Code Validation Proposal（Phase 2）で `unreachable`/`br`/`return` 後のコードに任意の型スタックを許容。CL の `(progn (error "msg") x)` 後の `x` 評価がポリモーフィック unreachable として型チェックをスキップ。dead code パスへのダミー命令生成ロジックを削除。コード生成が単純化され、特に `tagbody`/`go`/`return-from` の多い CL コードでバイナリサイズが削減
- **根拠**: 現状の codegen は `(vm-halt)` 後に型スタックを空にする命令を挿入しているが、Relaxed Dead Code Validation でこの処理が不要になる。コンパイラの単純化に直結
- **難易度**: Easy

#### FR-309: Rounding Variants — `f64.nearest_int` / Banker's Rounding

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: Wasm MVP の `f64.nearest` は "round half to even"（Banker's rounding）だが、cl-cc の `(round 2.5)` → `2` の CL 標準動作が正しくコード生成されているか未検証。SBCL と同一の丸め挙動を保証する命令マッピングが未記載
- **内容**: Wasm Rounding Variants Proposal（Phase 2）の `f32.nearest_int`/`f64.nearest_int` を使用。CL の `(round x)` → `f64.nearest_int`（IEEE 754 round-half-to-even）。`(fround x)` → `f32.nearest_int`。`(truncate x)` → `f64.trunc`。`(floor x)` → `f64.floor`。`(ceiling x)` → `f64.ceil`。各関数の Wasm 命令マッピングと端数処理の仕様を codegen に明示。`(round 0.5)` → `0`、`(round 1.5)` → `2` の正確な挙動テストを追加
- **根拠**: Banker's rounding は CL 標準（CLHS §12.1.4.3）。現状の `f64.nearest` がこれを満たすか明示的に文書化・テストされていない。金融計算での誤差蓄積を防止
- **難易度**: Easy

#### FR-310: Exception Tag Import / Export — モジュール間 Condition 型共有

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: FR-252（EH v2）・FR-289（GC struct ペイロード）は単一モジュール内の例外処理のみ記載。Dynamic Linking（FR-227）でサイドモジュールがメインモジュールの condition 型（`$file-error`・`$type-error`）を `throw`/`catch` する際のタグ共有方法が未定義
- **内容**: Wasm Exception Handling v2 の exception tag を `export`/`import` として宣言。メインモジュールが `(export (tag $cl-condition-tag))` でタグを公開。サイドモジュールが `(import "cl-core" "condition-tag" (tag $cl-condition-tag))` で同一タグを import し `throw`/`catch` を共有。Component Model（FR-206）の WIT 型として condition 階層を宣言。Cross-module `handler-case` が正しく動作することをテスト
- **根拠**: CL の condition システムはパッケージ（モジュール）を跨いで動作する。Dynamic Linking（FR-227）環境でサイドライブラリが `signal` した条件をメインが `handler-case` で捕捉するにはタグ共有が必須
- **難易度**: Medium

#### FR-311: `wasm-c-api` — ネイティブ C 埋め込み API

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: cl-cc の Wasm バイナリは JS/ブラウザまたは WASI ランタイム（wasmtime/wasmer）経由でのみ実行可能。ネイティブ C/C++ アプリケーションへの直接埋め込みパスなし
- **内容**: `wasm-c-api`（W3C 標準 C API、`wasm.h`）を使用したホスト埋め込みコードを生成。`./cl-cc compile --emit-c-header` で `cl_module.h`（`wasm_func_t`/`wasm_instance_t` 型のバインディング）を出力。C から `cl_call_function(instance, "my-fn", args)` で CL 関数を呼び出し。FFI 境界の型変換（CL → `wasm_val_t`）を自動生成。iOS/Android への CL ライブラリ埋め込み・ゲームエンジン（Unity/Unreal）への CL スクリプト統合に対応
- **根拠**: WASI なしの組み込み環境（マイコン・ゲーム・モバイル）では `wasm-c-api` が唯一の標準埋め込みパス。CL をネイティブアプリのスクリプト言語として使用するユースケース
- **難易度**: Hard

#### FR-312: Wasm 実行時フィーチャー検出

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: FR-258（Profiles）は静的なフィーチャー宣言のみ。デプロイ先ブラウザ/ランタイムが実際にどの Proposal を実装しているかを実行時に確認する手段が未記載
- **内容**: `WebAssembly.featureDetect`（Chrome提案）または 小さな Wasm モジュールを `WebAssembly.validate` でテストするフィーチャー検出スニペットを生成。`(if (wasm-supports-p :gc :threads :tail-calls) ...)` で実行時に最適ビルドを選択。FR-258（Profiles）との連携：プロファイル別ビルドを用意し実行時に最適版を選択してロード。`./cl-cc compile --feature-detect` でフィーチャー検出コード付きローダーを生成
- **根拠**: 2026年でも古いブラウザ（Safari 16以下）や組み込みランタイムでは GC/Threads が未実装の場合がある。静的プロファイル（FR-258）だけでは実行時の互換性エラーを防げない
- **難易度**: Medium

#### FR-313: `v128.load32_zero` / `v128.load64_zero` — ゼロ拡張 SIMD ロード

- **対象**: `packages/backend/emit/src/wasm.lisp`, SIMD エミッタ
- **現状**: SIMD ベクタに1スカラー値をロードするには `v128.load32_splat`（全レーンに複製）または `v128.load` + mask の2命令が必要。ゼロ拡張（下位レーンのみ設定・残りゼロ）の1命令ロードなし
- **内容**: Wasm SIMD128 の `v128.load32_zero`/`v128.load64_zero` 命令を追加。32bit スカラーを v128 下位 32bit に格納・残り 96bit をゼロ初期化。64bit スカラーを下位 64bit に格納・残り 64bit をゼロ初期化。部分的な SIMD ベクタ初期化・行列演算の境界処理・scalar-to-vector 昇格パスに使用。`(coerce x 'single-float)` を v128 に変換する際の初期化命令として使用
- **根拠**: SIMD ループの prologue/epilogue で scalar→vector 変換が頻出。`v128.load32_zero` 1命令で2命令シーケンスを置換
- **難易度**: Easy

#### FR-314: Relaxed SIMD 整数演算拡張 — Q15 / INT8 ドット積 / BF16

- **対象**: `packages/backend/emit/src/wasm.lisp`, SIMD エミッタ
- **現状**: FR-214（Relaxed SIMD）は `f32x4.relaxed_madd`・`f64x2.relaxed_min/max`・`i32x4.relaxed_laneselect` を記載。整数系拡張命令（Q15 固定小数点・INT8 量子化ドット積・BF16）が未記載
- **内容**: Wasm Relaxed SIMD Phase 4 の以下命令を追加。(1) `i16x8.relaxed_q15mulr_s` — Q15 固定小数点乗算（DSP・音声処理）。(2) `i32x4.relaxed_dot_i8x16_i7x16_s` — INT8×INT7 量子化ドット積（LLM inference の INT8 gemm）。(3) `f32x4.relaxed_dot_bf16x8_add_f32x4` — BF16 ドット積 + f32 accumulate（Transformer の attention 計算）。CL の NN 推論ライブラリが WASI-NN（FR-299）に加えてネイティブ Wasm SIMD でも推論可能に
- **根拠**: LLM 推論の INT8 量子化が標準化。BF16 は TensorFlow/PyTorch の標準浮動小数点型。WASI-NN（FR-299）が未対応の環境でも Wasm SIMD で推論を実行するためのフォールバック
- **難易度**: Medium

#### FR-315: `catch_all_ref` — 全例外を `exnref` として捕捉

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/conditions.lisp`
- **現状**: FR-252（EH v2）は `try_table (catch $tag $label)` で特定タグの例外のみキャッチ。FR-271（`exnref`）は型検査・再送を記載。タグを問わず全例外を `exnref` として捕捉する `catch_all_ref` が未記載
- **内容**: Wasm Exception Handling v2 の `catch_all_ref $label` 句を `try_table` に追加。全タグの例外を `exnref` としてスタックに積んで `$label` に分岐。CL の `(handler-case expr (t (e) ...))` → `catch_all_ref` に変換。`exnref` を `exception.get_tag` で検査し動的 condition 型マッチを実装。`ignore-errors` / `handler-case (t ...)` の最も一般的なパターンに対応
- **根拠**: `handler-case (t (e) ...)` は CL で最頻出のエラーハンドリングパターン。`catch_all_ref` なしでは全タグを列挙する必要があり、動的ロード（FR-227）で追加されたサイドモジュールの condition を捕捉できない
- **難易度**: Easy

#### FR-316: JS Primitive Builtins — 数値型変換の組み込み最適化

- **対象**: `packages/backend/emit/src/wasm-trampoline.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: FR-218（String Builtins）は文字列操作を組み込み化。JS ↔ Wasm 境界での数値型変換（`Number()` / `parseInt()` / `parseFloat()` / プロパティアクセス）は手動 JS ラッパー関数経由
- **内容**: Wasm JS Primitive Builtins Proposal（Phase 2、String Builtins の拡張）の組み込み数値変換命令を使用。`js:number->i32`（JS `Number` → Wasm `i32`）・`js:i32->number`（`i32` → JS `Number`）・`js:string->i32`（`parseInt` 相当）・`js:string->f64`（`parseFloat` 相当）を組み込み命令としてエミット。CL の `(js-get obj :length)` → `js:number->i32 (js:get_property obj "length")` に変換。手動変換ラッパー関数を削除
- **根拠**: DOM API の数値プロパティ（`.length`・`.width`・`.offsetTop`）の取得が全て JS ラッパー呼び出し経由。組み込み化で関数呼び出しオーバーヘッドを除去
- **難易度**: Medium

#### FR-317: Hot Code Reloading — 実行中モジュールの関数パッチ

- **対象**: `packages/cli/src/main.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: FR-288（REPL インクリメンタルコンパイル）は新関数を Dynamic Linking で追加する。既存関数の実装を実行中に差し替える（hot swap）手段なし
- **内容**: Dynamic Linking（FR-227）の `__indirect_function_table` への `table.set` を使用してホット関数パッチを実装。`(defun f (x) ...)` の再定義 → 新しいWasm関数コードを含むサイドモジュールをインスタンス化 → テーブルエントリを `table.set` で上書き。実行中の CL サーバー（`hunchentoot`/`clack`）のリクエストハンドラを再起動なしで更新。CLOS の `add-method` / `remove-method` を hot swap で実現
- **根拠**: SBCL の `compile` + `load` によるホットリロードはCL開発の核心。Wasm 環境でもゼロダウンタイム更新・インタラクティブ開発を実現するために必須
- **難易度**: Hard

#### FR-318: Wasm メモリプロファイラ / ヒープインスペクタ

- **対象**: `packages/backend/runtime/src/gc.lisp`, `packages/cli/src/main.lisp`
- **現状**: FR-222（DWARF）・FR-223（Source Maps）はソースレベルデバッグを提供。GC ヒープの使用量・オブジェクト分布・リーク検出の可視化手段なし
- **内容**: Chrome DevTools の "Memory" パネルと連携するヒープスナップショット API を実装。`./cl-cc run --heap-profile` で GC ヒープ状態を定期スナップショット。Wasm GC struct/array の型別サイズ分布・alive オブジェクト数を集計。`(heap-dump "/tmp/heap.json")` でヒープ状態を JSON 出力（Chrome DevTools ヒープスナップショット形式）。GC ルートからの参照グラフを DOT 形式で出力。メモリリーク（循環参照・キャッシュ肥大化）を検出する `(find-leaks)` ツール
- **根拠**: Quicklisp 全体をロードすると GC ヒープが数百MBになる場合がある。ヒープインスペクタなしでは どのデータ構造がメモリを消費しているか分からない
- **難易度**: Hard

#### FR-319: Component Model テスト基盤 — WIT インターフェース検証

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: FR-206（Component Model）は WIT バインディング生成を記載。生成した `.wasm` コンポーネントが WIT インターフェース仕様に準拠しているか検証するテストハーネスなし
- **内容**: `wit-bindgen test` 相当のインターフェース適合テストを統合。`./cl-cc test --wit my-component.wit` で WIT 定義に対する自動テスト生成。型マッピング（CL `string` ↔ WIT `string`・CL `list` ↔ WIT `list<T>`）の正確性をラウンドトリップテストで検証。Rust / Python / JS の `wit-bindgen` 生成コードとの相互運用テスト。`wasmtime serve` で HTTP handler world（FR-274）の E2E テスト実行
- **根拠**: Component Model（FR-206）は他言語との相互運用が目的だが、WIT 型マッピングのバグは実行時型エラーとして現れる。静的インターフェース検証により「CL と Rust が同じ WIT を読んでいる」ことを CI で保証
- **難易度**: Medium

---

### Phase 54 — MVP 完全性 / ツールチェーン補完 / 数値精度

#### FR-320: String Iterator Views — `stringview_wtf16` / `stringview_iter`

- **対象**: `packages/engine/vm/src/strings.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: String Builtins（FR-218・FR-295）は文字列全体の encode/decode を提供。文字単位のイテレーション（`(loop for c across str)`）は `string.encode_wtf16_array` で一度 GC 配列にコピーしてから `array.get` でアクセス
- **内容**: Wasm String Builtins の String Iterator Views を使用。`string.as_wtf16` で文字列を WTF-16 ビューに変換（コピーなし）。`stringview_wtf16.get_codeunit $idx` で UTF-16 コードユニットを直接アクセス。`string.as_iter` でイテレータビューを生成し `stringview_iter.next` でコードポイントを1つずつ取得。`(char str i)` → `stringview_wtf16.get_codeunit i`。`(loop for c across str)` → `string.as_iter` + `stringview_iter.next` ループ
- **根拠**: 現状の `(loop for c across str)` は文字列をWasm配列にコピーしてからイテレートするため O(N) の追加メモリ。Iterator View はゼロコピーで文字アクセスを提供し、文字列スキャンが支配的な CL パーサーに大きく効く
- **難易度**: Medium

#### FR-321: WASI Preview 1 互換シム — 後方互換性レイヤー

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: FR-207（WASI 0.2）は WASI 0.1 との後方互換なしと明記。既存の `wasm32-wasi` ターゲット向けビルド（Quicklisp の一部パッケージ等）が WASI 0.2 環境で動作しない
- **内容**: `wasm32-wasi-preview1` 互換シム（`wasip1-compat`）を提供。`fd_write`/`fd_read`/`proc_exit` 等の WASI 0.1 関数シグネチャを WASI 0.2 の `wasi:filesystem`/`wasi:cli` にマップするアダプタ層を生成。`./cl-cc compile --target wasm32-wasi` が自動的に互換シムを注入。Component Model（FR-206）の `wasm-tools component` が生成するアダプタと統合。WASI 0.1 バイナリを WASI 0.2 ランタイム上で実行可能に
- **根拠**: WASI 0.2 移行期（2024〜2026年）に WASI 0.1 向けに書かれた CL ライブラリが多数存在。互換シムなしでは移行コストが高く Quicklisp エコシステムの利用が困難
- **難易度**: Medium

#### FR-322: Wasm バイナリツール統合 — `wat2wasm` / `wasm-objdump`

- **対象**: `packages/cli/src/main.lisp`, `flake.nix`
- **現状**: FR-219 は `wasm2wat`（逆アセンブル）と `wasm-opt`（Binaryen 最適化）に言及。`wat2wasm`（テキスト→バイナリ）・`wasm-objdump`（セクション解析）・`wasm-decompile`（高水準逆コンパイル）がビルドパイプラインに未統合
- **内容**: WABT（WebAssembly Binary Toolkit）の各ツールをビルドパイプラインに統合。`./cl-cc disasm --wat module.wasm` → `wasm2wat` 経由のテキスト出力。`./cl-cc inspect module.wasm` → `wasm-objdump -x -d` でセクション・型・インポート・エクスポート・コード解析。`./cl-cc decompile module.wasm` → `wasm-decompile` で擬似コード出力（デバッグ用）。`nix run nixpkgs#wabt` でツールが存在しない場合に自動取得
- **根拠**: `wasm-opt`（FR-219）は最適化専用。コード解析・デバッグには `wasm-objdump` が不可欠。FR-265（決定論的ビルド）の検証・FR-222（DWARF）のセクション確認に使用
- **難易度**: Easy

#### FR-323: Wasm MVP Bit Operations — `clz` / `ctz` / `popcnt`

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: CL の `(integer-length n)` は `(- 64 (clz n))` で実装可能だが、現状は ループまたは SBCL ホスト関数への委譲で実装。`(logcount n)`（set bits の数）も同様
- **内容**: Wasm MVP のビット操作命令を CL 算術関数に直接マップ。`i32.clz`/`i64.clz`（先頭ゼロビット数）→ `(integer-length n)` = `(- 64 (i64.clz n))`。`i32.ctz`/`i64.ctz`（末尾ゼロビット数）→ `(1+ (i64.ctz n))` で最下位セットビット位置。`i32.popcnt`/`i64.popcnt`（セットビット数）→ `(logcount n)`。CL の `(ash n (- (integer-length n) 1))` パターンを `i64.clz` 1命令に最適化
- **根拠**: `(integer-length n)` と `(logcount n)` は暗号・ビット操作ライブラリ（Ironclad 等）で頻出。現状の多命令実装をハードウェア1命令に置換
- **難易度**: Easy

#### FR-324: `f32.copysign` / `f64.copysign` — 符号ビットコピー

- **対象**: `packages/engine/vm/src/primitives.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: CL の `(float-sign x y)` は `(* (abs y) (if (minusp x) -1.0 1.0))` として展開。条件分岐＋絶対値計算の2〜3命令
- **内容**: Wasm MVP の `f32.copysign`/`f64.copysign`（IEEE 754 `copySign` 操作）を使用。`(float-sign x y)` → `f64.copysign y x`（`y` の大きさに `x` の符号を付与）1命令。`(float-sign x)` → `f64.copysign 1.0 x` で符号のみ抽出。数値計算ライブラリ（`cl-num-utils`・Ironclad の符号付き演算）のホットパスに適用
- **根拠**: `copysign` は IEEE 754 の基本演算。CL の `float-sign` の正確な実装として分岐なし・1命令が保証される
- **難易度**: Easy

#### FR-325: Wasm SIMD NaN 非正規化 — ベクトル演算の NaN 意味論

- **対象**: `packages/backend/emit/src/wasm.lisp`, SIMD エミッタ, `tests/unit/vm/primitives-tests.lisp`
- **現状**: FR-202（SIMD128）・FR-214（Relaxed SIMD）は命令の列挙のみ。Wasm SIMD は IEEE 754 の **NaN 正規化を保証しない**（`f32x4.add` の結果 NaN は non-canonical NaN の可能性がある）。CL の `(= nan nan)` は常に `nil` だが SIMD レーン内では実装依存
- **内容**: Wasm SIMD の NaN 意味論を明示的にドキュメント化し、コード生成に反映。SIMD レーンの NaN 比較（`f32x4.eq`）は V8/SpiderMonkey で non-canonical NaN を返す可能性があることを warn。CL の `(every #'= vec1 vec2)` の SIMD 実装では NaN レーンを `v128.any_true`（FR-304）で別途処理。Relaxed SIMD（FR-214）の `f64x2.relaxed_min/max` は NaN 伝播が実装依存であることをコメントに明記。`--strict-nan` フラグで SIMD NaN 正規化を強制（Relaxed SIMD 使用を無効化）
- **根拠**: CL の浮動小数点意味論は IEEE 754 準拠。SIMD ベクトル化によって `(= x x)` が `nil` を返す NaN が non-canonical NaN に変化する可能性があり、数値ライブラリの正確性に影響する
- **難易度**: Medium

#### FR-326: `memory.grow` OOM 検出 — メモリ確保失敗の堅牢な処理

- **対象**: `packages/backend/runtime/src/heap.lisp`, `packages/backend/emit/src/wasm.lisp`
- **現状**: `memory.grow n` は失敗時に `-1`（`0xFFFFFFFF` as i32）を返すが、cl-cc のヒープ拡張コードがこの戻り値を検査しているか未検証。OOM 時に無効なメモリアドレスへのアクセスが発生する可能性
- **内容**: `memory.grow` の戻り値を必ずチェックし `-1` 返却時に CL の `storage-condition` を発生。`(gc.lisp の expand-heap)` → `memory.grow n` → `(= result -1)` なら `(error 'storage-condition "Wasm memory exhausted")`。Memory64（FR-213）では `i64` 戻り値の `-1_i64` をチェック。`memory.size` で現在のページ数・利用可能上限を取得し GC ポリシーに反映。ブラウザタブの 4GB 制限・モバイルデバイスの厳しいメモリ制約に対応
- **根拠**: SBCL は OOM を `storage-condition` として正しく処理する。Wasm でも同等の堅牢性が必要。現状の実装では OOM が `WebAssembly.RuntimeError: memory access out of bounds` として現れる可能性がある
- **難易度**: Easy

#### FR-327: Sub-word Atomic Operations — 8/16bit CAS

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/vm.lisp`
- **現状**: FR-203（Threads）・FR-273（struct.atomic）は 32/64bit のアトミック操作のみ言及。8/16bit サブワードのアトミック CAS・RMW 操作が未記載
- **内容**: Wasm Threads の sub-word atomic 命令群を使用。`i32.atomic.rmw8.cmpxchg_u`/`i32.atomic.rmw16.cmpxchg_u` で 1/2 バイト単位の CAS。`i32.atomic.rmw8.add_u`/`i32.atomic.rmw8.xchg_u` 等の 8bit RMW。CL の packed 構造体（FR-283 の `i8`/`i16` フィールド）への atomic アクセス。ロックフリーなフラグビットマップ（GC mark bits・スレッドスケジューラのステートマシン）の実装
- **根拠**: GC の mark bits は 1byte 単位で管理される場合が多い。32bit アトミックで 1bit を更新すると不必要な競合が発生。8bit CAS により GC マークフェーズのスレッド競合を 1/4 に削減

- **難易度**: Medium

---

### Wasm 機能カバレッジ マトリクス（2026年）

**Phase 4 標準（MVP v1.1 / 全主要ブラウザ出荷済み）**

| Proposal                                                                                               | Status                    | FR                                                  | 難易度        |
| ------------------------------------------------------------------------------------------------------ | ------------------------- | --------------------------------------------------- | ------------- |
| Non-trapping Float-to-int                                                                              | MVP v1.1 / 全主要ブラウザ | FR-233                                              | Easy          |
| Sign-extension Operators                                                                               | MVP v1.1 / 全主要ブラウザ | FR-234                                              | Easy          |
| Bulk Memory + Bulk Tables                                                                              | MVP v1.1 / 全主要ブラウザ | FR-228, FR-237                                      | Easy          |
| Multi-value Returns                                                                                    | MVP v1.1 / 全主要ブラウザ | FR-235                                              | Hard          |
| JS BigInt ↔ i64                                                                                       | Chrome 85+ / Firefox 78+  | FR-236                                              | Easy          |
| Reference Types (`externref` + Abstract Types)                                                         | Chrome 91+                | FR-226, FR-282                                      | Medium / Hard |
| SIMD128 完全版（shuffle / swizzle / dot / 型変換 / lane ops / reduction / load_zero）                  | 全主要ブラウザ            | FR-202, FR-278, FR-298, FR-303, FR-304, FR-313      | Easy〜Hard    |
| Relaxed SIMD 拡張（Q15 / INT8 dot / BF16）                                                             | Chrome 114+               | FR-214, FR-314                                      | Medium        |
| Threads / Atomics + `atomic.fence` + `wait` timeout + sub-word CAS                                     | 全主要ブラウザ            | FR-203, FR-256, FR-306, FR-327                      | Very Hard     |
| Exception Handling v1 + v2 (`try_table`/`throw_ref`/`exnref`/`catch_all_ref`) + tag import/export      | Chrome 95+ / 123+         | FR-204, FR-252, FR-271, FR-310, FR-315              | Medium        |
| `WebAssembly.Exception` JS API                                                                         | Chrome 95+                | FR-262                                              | Medium        |
| Multiple Memories                                                                                      | Chrome 92+                | FR-208                                              | Hard          |
| Typed Function References + `return_call_ref`                                                          | Chrome 113+               | FR-212, FR-253                                      | Medium        |
| Extended Const Expressions                                                                             | Chrome 113+               | FR-215                                              | Easy          |
| Tail Calls                                                                                             | Chrome 112+               | FR-143                                              | Medium        |
| Relaxed SIMD                                                                                           | Chrome 114+               | FR-214                                              | Medium        |
| Memory64                                                                                               | Chrome 119+               | FR-213                                              | Medium        |
| Wasm GC 完全版（i31ref / struct / array / rec / null safety / packed fields）                          | Chrome 119+               | FR-209〜211, FR-231, FR-254, FR-270, FR-279, FR-283 | Easy〜Hard    |
| `ref.eq` — eqref 同一性比較                                                                            | Chrome 119+               | FR-285                                              | Easy          |
| `any.convert_extern` / `extern.convert_any`                                                            | Chrome 119+               | FR-286                                              | Medium        |
| GC Bulk Array Ops（array.fill / init_data / init_elem）                                                | Chrome 119+               | FR-284                                              | Easy          |
| `array.new_elem` + More Array Constructors                                                             | Chrome 119+               | FR-249, FR-281                                      | Easy          |
| Typed `select` for refs                                                                                | Chrome 119+               | FR-279                                              | Easy          |
| GC Struct as Exception Payload                                                                         | Chrome 123+               | FR-289                                              | Medium        |
| Passive Element / Data Segments                                                                        | MVP v1.1 / 全主要ブラウザ | FR-294                                              | Easy          |
| JS Promise Integration                                                                                 | Chrome 123+               | FR-217                                              | Hard          |
| String Builtins + Iterator Views                                                                       | Chrome 117+               | FR-218, FR-295, FR-320                              | Medium        |
| ESM Integration                                                                                        | Chrome 89+ / Safari 15+   | FR-230                                              | Medium        |
| Component Model / WIT                                                                                  | WASI 0.2 / W3C 2024       | FR-206                                              | Very Hard     |
| WASI 0.2 + Preview 1 シム + WASI Worlds (`wasi:nn`/`wasi:http`/`wasi:cli`/`wasi:random`/`wasi:crypto`) | wasmtime / wasmer         | FR-207, FR-274, FR-299, FR-321                      | Hard          |

**Phase 3（実装フェーズ）**

| Proposal                                  | FR     | 難易度 |
| ----------------------------------------- | ------ | ------ |
| Wide Arithmetic (128bit)                  | FR-238 | Medium |
| Custom Page Sizes                         | FR-239 | Medium |
| Compact Import Section                    | FR-240 | Easy   |
| Custom Descriptors (externref 型付き拡張) | FR-241 | Hard   |

**Phase 2（提案テキストあり）**

| Proposal                                                                       | FR                     | 難易度    |
| ------------------------------------------------------------------------------ | ---------------------- | --------- |
| Branch Hinting                                                                 | FR-216                 | Easy      |
| Stack Switching + `cont.throw` + Algebraic Effect Handlers                     | FR-205, FR-272, FR-301 | Very Hard |
| Shared Everything Threads + `struct.atomic` / `array.atomic` / `global.atomic` | FR-224, FR-266, FR-273 | Very Hard |
| `table64`                                                                      | FR-229                 | Easy      |
| Extended Name Section                                                          | FR-242                 | Easy      |
| Relaxed Dead Code Validation                                                   | FR-308                 | Easy      |
| Rounding Variants (`f64.nearest_int`)                                          | FR-309                 | Easy      |
| JS Primitive Builtins                                                          | FR-316                 | Medium    |

**Phase 1（フィーチャー提案）**

| Proposal                                             | FR     | 難易度    |
| ---------------------------------------------------- | ------ | --------- |
| Memory Control (`memory.discard`)                    | FR-243 | Medium    |
| GC Finalization / WeakRef                            | FR-255 | Hard      |
| GC Precise Roots / Write Barriers                    | FR-267 | Hard      |
| Type Imports                                         | FR-244 | Hard      |
| JIT Interface                                        | FR-245 | Very Hard |
| Flexible Vectors                                     | FR-246 | Very Hard |
| Frozen Values                                        | FR-247 | Medium    |
| Half Precision (`f16`)                               | FR-248 | Medium    |
| Multibyte Array Access                               | FR-250 | Medium    |
| Reference-Typed Strings                              | FR-251 | Hard      |
| Custom Annotations                                   | FR-225 | Medium    |
| Wasm Profiles                                        | FR-258 | Easy      |
| Wasm Decimal (decimal64/128)                         | FR-268 | Hard      |
| `func.bind` — Wasm 部分適用                          | FR-290 | Hard      |
| Extended Const `ref.func`                            | FR-291 | Easy      |
| WASI `wasi:keyvalue` / `wasi:messaging` / `wasi:sql` | FR-296 | Hard      |

**Phase 0（初期探索）**

| Proposal                | FR     | 難易度    |
| ----------------------- | ------ | --------- |
| `call_stack` Inspection | FR-269 | Hard      |
| Wasm Startup Snapshots  | FR-287 | Very Hard |

**コンパイラ実装（Wasm仕様外）**

| 機能                                                                | FR             | 難易度    |
| ------------------------------------------------------------------- | -------------- | --------- |
| ref.cast 除去 / `br_on_cast` 最適化                                 | FR-142, FR-231 | Medium    |
| Integer Range Annotation                                            | FR-145         | Medium    |
| MVP Bit Ops (`clz`/`ctz`/`popcnt`) → `integer-length`/`logcount`    | FR-323         | Easy      |
| `copysign` → `float-sign`                                           | FR-324         | Easy      |
| SIMD NaN 意味論 / `--strict-nan` フラグ                             | FR-325         | Medium    |
| `memory.grow` OOM 検出 → `storage-condition`                        | FR-326         | Easy      |
| Typed Closure Environment Array                                     | FR-144         | Hard      |
| AOT バイナリ生成                                                    | FR-219         | Hard      |
| Profile-Guided Optimization                                         | FR-220         | Very Hard |
| Tiered Compilation ヒント (Liftoff / TurboFan)                      | FR-259         | Medium    |
| LTO / クロスモジュールインライン                                    | FR-260         | Hard      |
| Dead Import Elimination                                             | FR-221         | Medium    |
| Dynamic Linking (`dlopen` 相当)                                     | FR-227         | Very Hard |
| Lazy Function Bodies                                                | FR-264         | Hard      |
| Streaming Compilation / Module Cache                                | FR-232         | Easy      |
| Worker 間 Module postMessage 転送                                   | FR-275         | Easy      |
| Wasm Import Maps                                                    | FR-276         | Medium    |
| CL ABI / シンボル名マングリング                                     | FR-277         | Medium    |
| 初期化順序 (`__wasm_call_ctors`)                                    | FR-280         | Medium    |
| `call_indirect` × `table64` 統合                                    | FR-292         | Medium    |
| Multi-memory Atomics + Multi-memory Bulk Copy                       | FR-293, FR-300 | Medium    |
| Wasm REPL インクリメンタルコンパイル + Hot Code Reloading           | FR-288, FR-317 | Hard      |
| String Transcoding 全バリアント                                     | FR-295         | Medium    |
| 決定論的ビルド + SRI ハッシュ                                       | FR-265, FR-307 | Easy      |
| Security (CFI / CSP / Constant-Time)                                | FR-261         | Hard      |
| COOP / COEP ヘッダ（Threads 前提条件）                              | FR-297         | Easy      |
| `WebAssembly.validate()` 静的検証                                   | FR-305         | Easy      |
| Wasm バイナリツール統合（wat2wasm / wasm-objdump / wasm-decompile） | FR-322         | Easy      |
| ServiceWorker + PWA Wasm キャッシュ                                 | FR-302         | Medium    |
| DWARF デバッグ情報                                                  | FR-222         | Hard      |
| Source Maps                                                         | FR-223         | Medium    |
| Type Reflection JS API                                              | FR-263         | Medium    |
| Wasm メモリプロファイラ / ヒープインスペクタ                        | FR-318         | Hard      |
| Component Model テスト基盤 (WIT 検証)                               | FR-319         | Medium    |
| `wasm-c-api` ネイティブ埋め込み                                     | FR-311         | Hard      |
| 実行時フィーチャー検出                                              | FR-312         | Medium    |
| WASI 0.3 (Async-first)                                              | FR-257         | Very Hard |

---

#### FR-205: Wasm Stack Switching (スタックスイッチング)

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/backend/emit/src/wasm-trampoline.lisp`
- **現状**: コルーチン・ファイバー・継続はPC-dispatchループでエミュレート。Wasmネイティブのスタック操作なし
- **内容**: Wasm Stack Switching Proposal（`cont.new`/`cont.bind`/`resume`/`suspend`）を使用してコルーチン生成・yield・resumeをネイティブWasm命令にマップ。CL の `(call/cc ...)` と継続オブジェクト（FR-537）のWasmネイティブ実装。PC-dispatchよりもエンジン側での最適化が期待できる
- **根拠**: Wasm Stack Switching Proposal（2024年Chrome/Firefoxに実装中）。OCaml 5 effect handlers の Wasm コンパイル先として設計
- **難易度**: Very Hard

#### FR-206: Wasm Component Model (コンポーネントモデル)

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/cli/src/main.lisp`
- **現状**: 出力は単一の `.wasm` バイナリ（core module）。他言語コンポーネントとの型安全な相互運用インターフェースなし
- **内容**: Wasm Component Model（W3C 2024 標準）対応の `.wasm` コンポーネント生成。WIT（WebAssembly Interface Types）ファイルから CL 型への自動マッピング（string/list/record → Wasm canonical types）。`wit-bindgen` 相当のバインディング自動生成
- **根拠**: Wasm Component Model は 2024 年に W3C 勧告。WASI 0.2 の前提。他言語との相互運用の標準パス
- **難易度**: Very Hard

#### FR-207: WASI 0.2 / WASIp2 (次世代システムインターフェース)

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/engine/vm/src/io.lisp`
- **現状**: WASI 0.1（preview 1）相当の `fd_write`/`fd_read` のみ想定。capability-based セキュリティモデル未対応
- **内容**: WASI 0.2 (Preview 2) の capability-based インターフェース対応。`wasi:filesystem`/`wasi:sockets`/`wasi:clocks`/`wasi:http` の WIT インターフェース経由でのシステムコール。`./cl-cc compile --target wasm32-wasi` で WASI 0.2 準拠バイナリ生成
- **根拠**: WASI 0.2 は 2024 年 1 月リリース。`wasmtime`/`wasmer` の標準実行環境。WASI 0.1 との後方互換なし
- **難易度**: Hard

#### FR-208: Wasm Multiple Memories (複数メモリ)

- **対象**: `packages/backend/emit/src/wasm.lisp`, `packages/backend/runtime/src/heap.lisp`
- **現状**: 単一の線形メモリ（memory 0）のみ。マネージドヒープとスタックが同一メモリ空間に混在
- **内容**: Wasm Multiple Memories 提案（Chrome/Firefox で出荷済み）を活用し、メモリ 0 をスタック・メモリ 1 をマネージドヒープ・メモリ 2 を文字列インターン領域として分離。GC スキャン範囲の限定（ヒープメモリのみスキャン）で GC 速度向上
- **根拠**: Wasm Multiple Memories は 2022 年全主要ブラウザ対応。セグメント分離によるセキュリティ強化にも寄与
- **難易度**: Hard

---
