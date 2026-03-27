# CPS, SSA, IR & VM Architecture

CPS transformation, SSA/IR infrastructure, and VM architecture improvements.

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

### Phase 90 — Sea of Nodes IR & スケジュール自由IR

#### FR-530: Sea of Nodes IR (スケジュール自由IR)

- **対象**: `src/emit/mir.lisp`, `src/compile/codegen.lisp`
- **現状**: MIR層はCFGベースのSSA（Braunアルゴリズムでφ挿入）。制御フローとデータフローが分離されていない従来型表現
- **内容**: Cliff Click (1995) の Sea of Nodes IR。制御ノードとデータノードを統一グラフに混在させ、命令順序を遅延してコード生成時に決定。φノードを Region/Phi ノードペアに置換。Loop/Start/End ノードで自然ループを表現。スケジューリングの自由度が高く、GVN・型推論・インライン化を単一パスで実行可能
- **根拠**: V8 TurboFan / GraalVM Graal compiler。HotSpot C2 も同系統。スケジュール独立性により GVN 精度が大幅向上
- **難易度**: Very Hard

#### FR-531: Schedule-Early / Schedule-Late (命令スケジューリング戦略)

- **対象**: `src/emit/mir.lisp`
- **現状**: MIRに固定の命令順序
- **内容**: Sea of Nodes (FR-530) 上の2段階スケジューリング: (1) Schedule-Early — dominance最小ノード（支配木上で最も浅い位置）に命令を置く。(2) Schedule-Late — 使用箇所に最も近い位置に命令を置く。実際のスケジューリングは Early と Late の間の「最適位置」（最も外側のループかつ liveness 考慮）を選択
- **根拠**: Cliff Click (1995) "Global Code Motion / Global Value Numbering"。SSAベース GVN とコードモーションの統一手法
- **難易度**: Hard

---

### Phase 91 — SSA形式変換・破壊

#### FR-532: Out-of-SSA / Phi Node Coalescing (SSA破壊・Phi合体)

- **対象**: `src/optimize/ssa.lisp`, `src/emit/regalloc.lisp`
- **現状**: SSA構築（`ssa.lisp`）は完備。SSA破壊（Phi→コピー命令への変換）なし。レジスタ割り当て（`regalloc.lisp`）との接続なし
- **内容**: SSA破壊3ステップ: (1) 各 Phi をその引数の数だけのコピー命令に展開。(2) Phi-coalescing — 干渉しないコピーを同一物理変数に統合してコピー命令を消去。(3) Parallel copy sequentialization — 同一基本ブロック内の並行コピー群を Swap-based/一時変数方式でシーケンシャル化。Sreedhar et al. (1999) または Boissinot et al. (2009) のアルゴリズム
- **根拠**: SSAベース RA の標準最終ステップ。cl-cc では SSA→RA の橋渡しが欠如
- **難易度**: Hard

#### FR-533: Parallel Copy Sequentialization (並行コピー逐次化)

- **対象**: `src/emit/regalloc.lisp`
- **現状**: SSA破壊後の並行コピー（`{r1←r2, r2←r1}` のような循環依存）を正しく逐次化する機構なし
- **内容**: 依存グラフ（DAG）を構築。非循環部分はトポロジカル順にコピー生成。循環部分は XOR-swap または一時レジスタで解消。`vm-move` 命令列への展開
- **根拠**: Boissinot et al. (2009) / GCC out-of-SSA。正当性に関わる基礎コンポーネント
- **難易度**: Medium

#### FR-534: Interprocedural SSA / Function Summaries (手続き間SSA・関数サマリー)

- **対象**: `src/optimize/optimizer.lisp`, `src/compile/pipeline.lisp`
- **現状**: SSAは関数内のみ。関数境界を越えたSSA値の追跡なし
- **内容**: 関数ごとに「入力条件 → 出力特性」のサマリー（副作用集合・戻り値型・純粋性フラグ）を構築。サマリーを用いた手続き間定数伝播・エイリアス解析。IPSCCP（FR-050）の基盤
- **根拠**: LLVM GlobalsModRef / Inliner function attrs。手続き間最適化の標準インフラ
- **難易度**: Hard

#### FR-535: Gated SSA / E-SSA (述語付きSSA)

- **対象**: `src/optimize/ssa.lisp`, `src/type/inference.lisp`
- **現状**: SSAの Phi ノードに値条件の述語情報なし。分岐条件と Phi の関係が失われる
- **内容**: Gated SSA (Tu & Padua 1995): `γ-node`（条件付き Phi）と `μ-node`（ループ帰納 Phi）を区別。E-SSA: Phi 引数に述語コンテキスト（`x > 0` のような条件）を付与。Flow-sensitive type narrowing（FR-116）の精度向上。配列範囲解析（FR-039）の基盤
- **根拠**: LLVM LazyValueInfo / GCC VRP の述語追跡。Bourdoncle widening operator
- **難易度**: Hard

---

### Phase 92 — 代数的エフェクト・限定継続

#### FR-536: Algebraic Effects / Effect Handlers (代数的エフェクト)

- **対象**: `src/compile/cps.lisp`, `src/vm/vm.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: CL `handler-case`/`restart-case` は `vm-conditions.lisp` で実装。代数的エフェクト（再開可能なエラー処理より一般的な機構）なし
- **内容**: `(perform eff args)` / `(handle body (eff (args k) handler-body))` 形式の代数的エフェクトシステム。継続 `k` は再開可能。CPS変換でエフェクトの穿孔（effect tunneling）を実現。one-shot継続の場合はスタック複製不要。非決定性・状態・例外・I/Oを統一的に扱う
- **根拠**: OCaml 5 multicore effects / Koka / Eff。2024-2026年のモダン言語ランタイムの主要機能。CPS自体がエフェクトエンコーディングと等価なため CPS 層との統合が自然
- **難易度**: Very Hard

#### FR-537: Delimited Continuations (限定継続 — shift/reset)

- **対象**: `src/compile/cps.lisp`, `src/vm/vm.lisp`
- **現状**: `block`/`return-from` による非限定脱出のみ。`call/cc` なし
- **内容**: `(reset body)` / `(shift k body)` または `(call-with-prompt tag body handler)` / `(abort-to-prompt tag val)` の限定継続プリミティブ実装。CPS変換での表現: プロンプトを継続スタックのセグメント境界として表現。`dynamic-wind` との統合。コルーチン・非同期I/O・バックトラッキングの基盤
- **根拠**: Racket `call-with-continuation-prompt` / Guile delimited continuations / R6RS `(rnrs control)`。Common Lispの `call/cc` 代替として機能
- **難易度**: Very Hard

#### FR-538: Continuation Marks (継続マーク — SRFI-157)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **現状**: コールスタックにユーザ定義メタデータを付与する機構なし
- **内容**: `(with-continuation-mark key value body)` / `(current-continuation-marks)` プリミティブ。各コールフレームに key-value マップを付与。現在の継続チェーンのマーク集合を O(stack-depth) で取得。デバッガ・プロファイラ・動的スコーピング・スタック検査に活用
- **根拠**: SRFI-157 / Racket continuation marks / Java StackWalker API。軽量なスタック注釈機構
- **難易度**: Medium

---

### Phase 93 — VMアーキテクチャ高度化

#### FR-539: On-Stack Replacement (OSR — スタック上での差し替え)

- **対象**: `src/vm/vm.lisp`, `src/compile/pipeline.lisp`
- **現状**: 関数の再コンパイルは次の呼び出しから有効。実行中の関数を最適化版に切り替える OSR なし
- **内容**: VMインタプリタのループバックエッジに OSR チェックポイントを挿入。ホットループ検出時に現在の実行状態（レジスタ・スタック・PC）を最適化コンテキストにマッピングしてネイティブコードに飛び込む「OSR-in」。最適化コードが投機的仮定を破った場合に VM に戻る「OSR-out」（deoptimization）
- **根拠**: HotSpot C1/C2 OSR / V8 OSR。長時間実行ループの最適化に必須。FR-244（Trace JIT）と相補的
- **難易度**: Very Hard

#### FR-540: Direct Threaded / Computed-Goto Dispatch (直接スレッドディスパッチ)

- **対象**: `src/vm/vm-run.lisp`
- **現状**: `run-compiled` が `execute-instruction` CLOS メソッドディスパッチ（FR-454で改善予定）。`run-vm` が defopcode ベースの `case` ディスパッチ
- **内容**: GNU C 拡張の `&&label`（computed goto）またはダイレクトスレッディング方式。各命令ハンドラの末尾に次命令のアドレスへの直接ジャンプを埋め込む。CPython 3.11+ / LuaJIT / Dalvik が採用。Common Lisp 実装では CFFI + callback 経由またはホスト SBCL の jump-table 生成を利用可能
- **根拠**: CPython 3.11 specializing adaptive interpreter。switch ディスパッチ比 20-30% 高速化。FR-111（Dispatch Loop Specialization）の実装基盤
- **難易度**: Hard

#### FR-541: Precise Deoptimization Frame Reconstruction (精密デオプト・フレーム復元)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **現状**: ガード失敗時（FR-232 Uncommon Trap）の正確な VM 状態復元機構なし
- **内容**: 各最適化コンパイルポイントに「逆マッピングテーブル」（物理レジスタ→仮想レジスタ対応）を付与。ガード失敗時に CPU レジスタから VM 状態を復元し、対応する VM PC から解釈実行を再開。インライン展開された関数のフレームも個別復元（inlined frame materialization）
- **根拠**: HotSpot deopt / V8 deopt / GraalVM deopt。投機的最適化の安全ネット。FR-231（Stack Map）の上位機能
- **難易度**: Very Hard

#### FR-542: Coroutine / Fiber VM Support (コルーチン/ファイバーVMサポート)

- **対象**: `src/vm/vm.lisp`, `src/compile/codegen.lisp`
- **現状**: VM は単一実行スレッド。コルーチン・非同期I/O・generator のプリミティブなし
- **内容**: VM レベルの `vm-yield` / `vm-resume` 命令追加。各コルーチンは独立した VM 状態（レジスタ・スタック）を保持。M:N スケジューリング（多数のファイバーを OS スレッドにマッピング）。`cl:generators`/`cl:lazy-sequences` の実装基盤。FR-537（限定継続）がある場合はそれで実装可能
- **根拠**: Python generators / Lua coroutines / Go goroutines / OCaml 5 Domain+Effect。非同期I/O とストリーム処理の基盤
- **難易度**: Hard

#### FR-543: Polymorphic Inline Cache Patching (PICパッチング — 自己書き換えIC)

- **対象**: `src/vm/vm.lisp`, `src/vm/vm-clos.lisp`
- **現状**: IC（FR-009 Monomorphic）はハッシュテーブルベース。コンパイル済みコードの命令列を書き換える self-modifying 型 IC なし
- **内容**: 生成済みネイティブコードの呼び出しサイトに IC stub をパッチ。初回呼び出し時は「IC miss stub」→ タイプチェック+分岐を挿入してコードを書き換え。V8 の Inline Cache は CodeStub Arch を使用。ネイティブバックエンドに適用可能
- **根拠**: V8 IC patching / HotSpot inline caches。IC ルックアップコストをゼロに近づける
- **難易度**: Very Hard

---

### Phase 94 — CPS変換・継続表現高度化

#### FR-544: CPS to Direct Style Transformation (CPS→直接スタイル逆変換)

- **対象**: `src/compile/cps.lisp`
- **現状**: CPS変換は一方向のみ。CPS形式で最適化後に Direct Style に戻す変換なし
- **内容**: CPS形式の検査で「継続が単純な関数呼び出し継続でありエスケープしない」場合を検出し、直接スタイルに逆変換。`(let ((k (lambda (v) (f v)))) ...)` → `(f ...)` 形式への縮退。管理ラムダ（administrative lambda）の大部分を除去。Kennedy (2007) selective CPS変換の後処理
- **根拠**: MLton / Chez Scheme。CPS変換→最適化→直接スタイル変換のパイプラインで最終コードの継続ラムダを除去
- **難易度**: Hard

#### FR-545: Trampoline-Based TCO (トランポリンTCO)

- **対象**: `src/compile/cps.lisp`, `src/vm/vm.lisp`
- **現状**: 末尾呼び出し最適化は `vm-tail-call` 命令（`codegen-functions.lisp`）で対応。しかし CPS 変換後の継続ラムダが末尾位置にある場合のトランポリン変換なし
- **内容**: CPS変換後に末尾呼び出し以外の位置での継続呼び出しをトランポリン形式に変換。`(lambda () (k v))` サンクを返し、外部ループで強制実行。スタックオーバーフローを根絶。Chicken Scheme の CPS+ヒープスタック方式とは別アプローチ
- **根拠**: Clojure `recur` / Python trampoline pattern。CPS末尾呼び出しの完全なスタック使用量制御
- **難易度**: Medium

#### FR-546: Chicken-Style Stack Copying Continuations (スタックコピー継続)

- **対象**: `src/runtime/gc.lisp`, `src/vm/vm.lisp`
- **現状**: 継続はCPS変換でクロージャとしてヒープ割り当て。スタック全体のコピーによる第一級継続未実装
- **内容**: Chicken Scheme 方式: 実行スタックを VM 専用スタックに配置。スタックが溢れたら生きているデータをヒープにコピーして GC（"Baker's Cheney on the MTA"）。`call/cc` は現在のスタックフレームを継続オブジェクトとしてヒープにコピー。一貫した CPS + GC ベース設計
- **根拠**: Chicken Scheme "A practical use of 'Cheney on the MTA'" / Gambit-C。第一級継続の効率的実装
- **難易度**: Very Hard

---

### Phase 95 — 型付きIR・エフェクトシステム

#### FR-547: Effect System in IR (IRエフェクトシステム)

- **対象**: `src/optimize/effects.lisp`, `src/emit/mir.lisp`, `src/compile/codegen.lisp`
- **現状**: `effects.lisp` に命令別副作用分類（`inst-effects`）が存在。しかし MIR/CPS IR ノードへのエフェクト注釈なし
- **内容**: 各 IR 命令に `{read-heap, write-heap, alloc, io, raise}` のエフェクトセットを付与。エフェクトベースのコードモーション判定（純粋命令のみ hoist/sink）。エフェクト推論（関数シグネチャにエフェクトを伝播）。Koka / OCaml 5 の effect inference に相当
- **根拠**: エフェクト情報で最適化の保守性を排除。現行 `opt-inst-pure-p` が未完全な純粋性チェックをしているが、エフェクトシステムで完全化
- **難易度**: Hard

#### FR-548: Typed SSA / Bidirectional Type Checking in IR (型付きSSA・双方向型検査)

- **対象**: `src/emit/mir.lisp`, `src/type/checker.lisp`
- **現状**: MIR 命令に型注釈なし。型チェック（`checker.lisp`）は AST レベルで実行され IR に伝播しない
- **内容**: MIR 命令の各引数と結果に ML 風型を付与。Phi 命令には合流型（join type）を付与。型伝播で SSA use-def 鎖に沿って型情報を伝播。双方向型検査（synthesis mode + checking mode）を `check/synth` モードで実装。不要な型チェック命令（`vm-integer-p` 等）を型証明で消去
- **根拠**: LLVM typed IR / Typed Assembly Language (TAL)。型情報を IR に保持することで型ベース最適化の精度向上
- **難易度**: Hard

#### FR-549: Multi-Level IR / Progressive Lowering (マルチレベルIR・段階的降下)

- **対象**: `src/compile/pipeline.lisp`, `src/emit/mir.lisp`
- **現状**: コンパイルパイプラインは `parse → expand → CPS → codegen → VM命令 → [MIR] → native` の不連続なステージ。各ステージ間の変換が大きすぎてデバッグが困難
- **内容**: MLIR スタイルの段階的降下: High-level IR（defun/let/if 等） → Mid-level IR（関数呼び出し・クロージャ展開済み） → Low-level IR（レジスタ・メモリ明示） → Machine IR（ターゲット命令）。各レベルで独立した検証・最適化が可能
- **根拠**: MLIR (Lattner et al. 2020) / LLVM-IR → SelectionDAG → MachineIR。コンパイラの保守性・拡張性を大幅改善
- **難易度**: Very Hard

---

### Phase 96 — 高度VM・JIT最適化

#### FR-550: Object Shape / Hidden Class Tracking (オブジェクトシェイプ追跡)

- **対象**: `src/vm/vm-clos.lisp`, `src/vm/vm.lisp`
- **現状**: CLOS インスタンスはスロット名→値のハッシュテーブル（`vm-clos.lisp`）。シェイプ（スロット順序）固定の最適化なし
- **内容**: V8 の Hidden Class / SpiderMonkey の Shape に相当する機構。CLOS インスタンスのスロット集合を「シェイプID」で識別。同シェイプのインスタンスはスロットオフセットを固定配列アクセスに最適化（ハッシュ不要）。メソッド追加/削除時のシェイプ遷移グラフ管理。IC（FR-009/FR-023）との連携でメソッド探索をシェイプチェック+直接オフセットに変換
- **根拠**: V8 hidden classes / SpiderMonkey shape tree / LuaJIT table shapes。動的言語 VM 最適化の核心
- **難易度**: Hard

#### FR-551: Tiered VM Interpreter (段階的VMインタプリタ)

- **対象**: `src/vm/vm-run.lisp`, `src/compile/pipeline.lisp`
- **現状**: 単一の VM インタプリタ（CLOS ベース `run-compiled` または defopcode `run-vm`）。バイトコード V2（FR-456）も独立実装。段階的コンパイルなし
- **内容**: 3段階実行: (1) Tier-0 — CLOS インタプリタ（現行、起動コスト最小）。(2) Tier-1 — バイトコード JIT（FR-456 完成後、速度改善）。(3) Tier-2 — 型フィードバック付き最適化 JIT（FR-058/FR-244、最大性能）。呼び出し回数カウンタで昇格トリガー。V8 の Ignition→Maglev→TurboFan、HotSpot の C1→C2 に相当
- **根拠**: V8 tiered compilation / HotSpot tiered compilation。起動時間とピーク性能の両立
- **難易度**: Very Hard

#### FR-552: Speculative Monomorphization (投機的モノモーフィゼーション)

- **対象**: `src/compile/codegen.lisp`, `src/optimize/optimizer.lisp`
- **現状**: ジェネリック関数は常に多態ディスパッチ（`vm-generic-call`）。型フィードバック情報があっても特化コードを生成しない
- **内容**: IC 型プロファイル（FR-058）から最頻出型を取得し、その型に特化したモノモーフィックな関数クローンを生成。実行時型ガード（FR-232）を先頭に置き、ガード成功時は特化版へ、失敗時は汎用版にフォールバック。`(defgeneric +)` の `(fixnum, fixnum) → fixnum` 特化が代表例
- **根拠**: GraalVM SpeculativeMonomorphization / V8 Maglev モノモーフィック IC 最適化。CLOS パフォーマンスの主要ボトルネック解消
- **難易度**: Hard

#### FR-553: Bytecode AOT Serialization (バイトコードAOTシリアライズ)

- **対象**: `src/bytecode/encode.lisp`, `src/compile/pipeline.lisp`
- **現状**: `encode.lisp`（50オペコード ISA 定義・エンコーダ・デコーダ）が実装済みだが、コンパイル済みバイトコードの永続化なし。毎起動時に全ソースを再コンパイル
- **内容**: コンパイル済み VM 命令列をバイトコードフォーマット（encode.lisp の ISA を使用）にシリアライズして `.clcc` ファイルに保存。次回起動時はデシリアライズのみでソースコンパイルをスキップ。ソースハッシュ検証付き（変更時に再コンパイル）。`./cl-cc compile foo.lisp → foo.clcc`、`./cl-cc run foo.clcc`
- **根拠**: Python `.pyc` / Java `.class` / Ruby `.rbc`。起動時間をソースコンパイルから分離。FR-405（Startup Optimization）と連携
- **難易度**: Medium

---

### Phase 97 — SSA高度化・末尾最適化補完

#### FR-554: Tail Modulo Cons (TMC — 末尾Cons変換)

- **対象**: `src/compile/cps.lisp`, `src/optimize/optimizer.lisp`
- **現状**: TCO（`vm-tail-call`）は末尾位置の関数呼び出しのみ対象。`(cons x (f rest))` のような「末尾Consセル構築」はスタックを消費し続ける
- **内容**: `(cons head (f tail))` パターンを検出し、アキュムレータセルへの「後置き」に変換。具体的には事前にConsセルを確保してCDRスロットをホールとして残し、再帰呼び出しをそのスロットへの書き込み先として渡す。末尾再帰版 `map`/`filter`/`append` が定数スタックで動作するようになる
- **根拠**: OCaml 4.14 `[@tail_mod_cons]` annotation / GHC `-O2` の `build/foldr` fusion。Lisp のリスト処理関数群を完全末尾再帰化する唯一の手段
- **難易度**: Hard

#### FR-555: Demand-Driven SSA Construction (需要駆動SSA構築)

- **対象**: `src/compile/ssa.lisp`
- **現状**: SSA構築は `ssa-insert-phi-nodes` でCFG全体にφ関数を挿入（Cytron et al. 全量挿入法）。利用されないφ関数も生成される
- **内容**: 最適化パスが特定変数のSSAフォームを要求したときだけφ関数を挿入する遅延構築。Cooper & Harvey "A Simple, Fast Dominance Algorithm"の支配辺境界を利用しつつ、実際に参照される変数のみ処理。不要φ関数の初期生成ゼロでコンパイル時間短縮
- **根拠**: LLVM `mem2reg` の需要駆動アルゴリズム / GCC の LazySSA。小関数多数の Common Lisp スタイルに特に有効
- **難易度**: Medium

#### FR-556: Incremental SSA Maintenance (増分SSA更新)

- **対象**: `src/compile/ssa.lisp`, `src/optimize/optimizer.lisp`
- **現状**: 最適化パス（インライン展開、定数畳み込み等）がIRを変更するたびに `ssa-transform` を再実行して完全再構築。変更点が局所的でも全関数を再処理
- **内容**: φ関数の追加・削除・書き換えを追跡する差分ログを維持。各最適化パスが「変更されたCFGエッジ」を報告し、影響を受けるφ関数のみを再計算。Braun et al. (2013) の増分アルゴリズム（本プロジェクトのSSA基盤と同系統）を拡張して差分更新対応
- **根拠**: LLVM `SSAUpdater` / GCC `update_ssa()`。大規模インライン展開後の再SSA化コストを O(変更量) に抑制
- **難易度**: Hard

---
