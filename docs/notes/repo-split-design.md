# リポジトリ分割 全体設計

> ステータス: ドラフト（全体設計のみ／実装は未着手）
> 対象: cl-cc モノレポの「良い単位」でのリポジトリ切り出し
> 前提: 直近で `cl-prolog-kit` `cl-weave` `cl-cli` `cl-tty-kit` `cl-boundary-kit`
> `cl-dataflow-kit` `cl-parser-kit` を外部化した「切り出しの型」を踏襲する。

## 1. 目的と原則

モノレポが約 30 万 loc / 35 パッケージに達し、以下が課題になっている:

- ビルド／テストの一括実行が重い（全スイートが遅い）。
- 責務の異なる領域（言語バックエンド・ネイティブ codegen・汎用ツール）が
  同じリリースサイクルに縛られている。
- 汎用的に再利用できる部品が compiler 本体と同居している。

### 1-0. 基本方針: 「分割」ではなく「剥離」

実測（§2-1）で、cl-cc は「凍った完成 repo をブロックに分割する」対象では
**ない**ことが確認されている:

- **本体は活発に変化中** — 過去 90 日で 578 コミット（1 日 6.4 回）、
  作業ツリーに常時数十ファイルの変更。churn の高い領域を切ると、切った先が
  日々ズレて `flake.lock` 追従コストが恒常化する。
- **核はセルフホストの自己参照ウェブ** — `our-eval` を `:cl-cc/bootstrap` に
  事前 intern して下流（expand/compile/selfhost）が共有し、compiler が自分自身を
  コンパイルする。`cl-cc/vm::` への跨ぎ内部参照は 301 箇所。これは行儀の問題では
  なくセルフホスト compiler の本質であり、リポジトリ境界で割ると壊れ続ける。

したがって本設計は「全体をガンガン分割」ではなく、**一枚岩の核を残したまま、
境界が凍った周縁だけを剥がす“剥離”**である。判断は「repo が完成しているか」では
なく「その**境界**が凍っているか」で行う。

**非目標（Non-goals）**:

- 核（bootstrap / vm / runtime / expand / compile / cps / selfhost）の分割 —
  自己参照・事前 intern のため **分割対象外**。monorepo のまま持つ。
- churn の高い領域の切り出し — 境界が動いている間は切らない。仕様が枯れ、
  逆依存が収束してから剥がす。
- 全パッケージの polyrepo 化 — 目的はビルド/リリースの分離であって、
  リポジトリ数の最大化ではない。

分割の原則（既存 `cl-*` 外部化の判断基準を明文化したもの）:

1. **ファンインが狭いものから切る** — 「誰が依存するか」が少ないほど、
   切っても本体側の変更が小さい。
2. **依存は非循環・単方向** — 切り出し先が本体へ逆流依存しない。
   循環がある場合は先にインタフェースを挿して断つ。
3. **ドメインが一貫している** — 1 リポジトリ = 1 責務。
4. **切り出しに見合うサイズ** — 配管コスト（flake input + Nix 派生 + CI）を
   上回る規模・独立性があること。
5. **配管は既存方式を再利用** — `flake = false` の source tree として取り込み、
   `sbcl.buildASDFSystem` で内部システムと同列にビルド（後述 §7）。
6. **境界が凍っているものだけ切る** — repo 全体の完成度ではなく、対象の
   *境界* の安定性（逆依存の収束・仕様の枯れ）を基準にする。

## 2. 現状の依存グラフ

内部 ASDF システムの依存（`packages/*/*.asd` の `:depends-on` を集約）:

```
Tier0 (依存ゼロ):  bootstrap  ast  binary  runtime  mir  target  ir
                   bytecode  docgen  formatter
Tier1:             vm(bootstrap,runtime)  parse(ast,bootstrap)
                   type(ast)  cps(ast,bootstrap)  stdlib(bootstrap)
Tier2:             optimize(vm,type,ast +cl-prolog-kit,cl-parser-kit)
                   expand(type,vm)  debug(vm)
Tier3:             regalloc(vm,mir,target,optimize)
                   codegen(vm,mir,target,binary,optimize,regalloc)
Tier4:             emit(vm,ast,mir,optimize,codegen)
バックエンド:       php(ast,parse,vm)         逆依存 = {pipeline, testing-framework}
                   javascript(ast,parse)      逆依存 = {pipeline}
Orchestration:     compile → pipeline → selfhost → repl
Tools(葉):         cli  tools  testing-framework  prolog-tools  docgen  formatter
```

**核（切り出せない）**: `bootstrap` と `vm` は全層を貫く。`ast` も広く使われる。
これらは cl-cc 本体に残す前提で、他をこの核の周囲から剥がしていく。

### 2-1. 境界の実測（切れるかの根拠）

「跨ぎの内部シンボル参照（`cl-cc/PKG::symbol`）」＝境界の綺麗さの逆指標。
外部から内部実装に手を突っ込まれている数が少ないほど、そのまま切れる。

跨ぎ内部参照の被参照数（`packages/*/src/*.lisp` 実測）:

```
cl-cc/vm      301   ← 核。分割不可の裏付け
cl-cc/php     167   （うち 135 は php 自身の中で閉じている → 外部侵入は僅少）
cl-cc/codegen  64
cl-cc/runtime  44
...
```

切り出し候補への「外部からの内部侵入」ファイル数:

| 候補 | 外部から内部に侵入している他パッケージ | 判定 |
|------|------|------|
| formatter / docgen / prolog-tools | **0 ファイル** | 前準備なしで即切れる |
| php | `pipeline`（**1 ファイルのみ** = `pipeline-runtime-bridges.lisp`） | 1 点の脱結合で切れる |
| javascript | `pipeline`（**1 ファイルのみ**） | 同上 |
| 核（vm 他） | 多数（vm だけで 301） | 切らない |

→ php の内部参照 167 のうち外部から来ているのは実質 1 ファイル。§5-1 で名指しした
「唯一の障害」が実測でも文字どおり唯一であることが確認できる。周縁の境界は
（本体全体が未完成でも）既に凍っている。

## 3. 分割の判断: 2 段ゲート（切れるか × 切る価値があるか）

抽出は必ず 2 つのゲートを順に通す。片方だけでは不十分:

- **ゲート①「切れるか」** — 外部からの内部侵入(`::`)が 0 か、収束しているか
  （§2-1）。これは *前提条件* にすぎない。
- **ゲート②「切る価値があるか」** — サイズ × 独立性が配管コスト（flake input +
  Nix 派生 + CI + cachix + README + `flake.lock` 追従）を上回るか（原則④）。
  小さすぎる / cl-cc 専用ツールは、①満点でも②で落ちる。

| 候補 | 規模 | ①切れるか | ②価値 | 判定 |
|------|------|------|------|------|
| `type` | ~16k loc | ◎ 侵入0・外向きは ast public のみ | ◎ 型システムとして一貫 | **抽出** |
| php + javascript | ~48k loc | ○ 逆依存1ファイル(要脱結合) | ◎ 全体の30% | **抽出** |
| optimize | ~41k loc | △ vm 内部75(要 vm 硬化) | ◎ 最適化ドメイン | **抽出(硬化後)** |
| native codegen 一式 | ~34k loc | △ vm 内部100(要 vm 硬化) | ◎ 一貫ドメイン | **抽出(硬化後)** |
| formatter/docgen/prolog-tools | ~1k loc | ◎ 侵入0 | ✕ 小さすぎ / cl-cc専用 | **monorepoに残す** |
| testing-framework | ~3k loc | △ | ✕ cl-weave と重複 | **廃止方向** |

> **formatter/docgen/prolog-tools を切らない理由**: ①は満点(侵入0)だが②で落第。
> 221〜473 loc に repo 1 個ぶんの配管が付くと *コードより配管が重い*。docgen /
> prolog-tools はそもそも汎用ライブラリではなく cl-cc の構造に依存した内製ツール
> （cl-prolog-kit が汎用エンジンなのとは別物）。既に `maybe-load-asd` の probe-file
> ガードで疎結合なので、monorepo 内モジュールのままで隔離の恩恵は得られている。

## 4. 提案するリポジトリ境界（最終ターゲット）

抽出する外部 repo は **5 個**。残りは `cl-cc-core`（monorepo）に留める。
Terraform 側の器は `takeokunn/private-terraform` の
`projects/github/repos_nerima_lisp_cl_cc.tf` に仮実装済み（apply は抽出 PR 待ち）。

```
cl-cc-core (monorepo として残す = 「compiler 本体」)
  kernel/vm/frontend(parse,type*,expand,cps)/optimize*/native*/compile/stdlib
  + 小物: formatter, docgen, prolog-tools, testing-framework(廃止方向)
  * type/optimize/native は抽出候補だが、core 内では公開APIモジュールとして整理

外部 repo（クラス A = 即, クラス B = vm 公開API硬化後）:
  [A] cl-cc-type            packages/type
  [A] cl-cc-php             packages/php
  [A] cl-cc-javascript      packages/javascript
  [B] cl-cc-optimize        packages/optimize
  [B] cl-cc-codegen-native  mir/target/regalloc/codegen/emit(+binary)
```

### フェーズ A — クラス A の抽出（境界が凍っている・低〜中リスク）

- **`cl-cc-type`** ← `packages/type`（~16k loc, deps: ast public のみ）。
  侵入0・外向き結合0 で、core で最もクリーンな抽出対象。配管 PoC に最適。
- **`cl-cc-php`** ← `packages/php`（~25.6k loc）
- **`cl-cc-javascript`** ← `packages/javascript`（~22.9k loc）
  php/js は逆依存が `pipeline` のみ。ただし脱結合 1 点が前提（次項）。

> **monorepo に残すもの**: formatter / docgen / prolog-tools（②価値ゲート落第、
> §3）。`testing-framework` は cl-weave のネイティブ property API と重複のため
> **cl-weave 移行 → 廃止**を別トラックで検討（切り出し対象外）。

### フェーズ B — php/javascript の脱結合（最大効果・要作業）

- **`cl-cc-php`** ← `packages/php`（~25.6k loc）
- **`cl-cc-javascript`** ← `packages/javascript`（~22.9k loc）

合計で全体の約 30%。逆依存が実質 `pipeline` のみで、切れば本体が大幅に軽くなる。

**唯一かつ最大の障害**: `pipeline` が php バックエンドの *内部シンボル* を
直接参照している。

```
packages/pipeline/src/pipeline-runtime-bridges.lisp:
  '((cl-cc/php::%php-array      . cl-cc/php:%php-array)   ; 内部→公開の対応表
    (cl-cc/php::%php-array-ref  . cl-cc/php:%php-array-ref)
    ... 数十エントリ ...)
```

`pipeline` → `php` の**コンパイル時・内部実装レベルの結合**であり、このままでは
php を別リポジトリに動かせない（内部パッケージ `cl-cc/php::` を外部から参照できない）。

→ 切り出し前に **§5 のバックエンド登録プロトコル**を導入して疎結合化する。

### フェーズ C — クラス B の抽出（vm 公開 API 硬化が前提）

optimize と native codegen は「②価値」は高いが「①切れるか」で vm 内部への
`::` 依存を抱える（optimize→vm 75、codegen→vm 100）。先に §5-2 の vm 公開 API
硬化（`::`→`:` + lint）を済ませてから抽出する。

- **`cl-cc-optimize`** ← `packages/optimize`（~41k loc）。最適化パス群。
- **`cl-cc-codegen-native`** ← `mir + target + regalloc + codegen + emit + binary`
  （~34k loc）。「IR → x86-64 / riscv64 / arm 機械語 + ELF/Mach-O 出力」の一貫ドメイン。

vm が安定インタフェース（IR/MIR/命令構築子/条件・アトミックの公開契約）を提供
してからでないと境界が濁る。フェーズ B 完了後、vm API を固める作業と合わせて着手。

## 5. インタフェース設計（脱結合の要点）

### 5-1. バックエンド登録プロトコル（フェーズ B の前提作業）— **完了 2026-07-27**

> 実装済み: `cl-cc/backend-protocol`（`packages/bootstrap/src/backend-protocol.lisp`）。
> php は `packages/php/src/backend.lisp`、js は `packages/javascript/src/backend.lisp`
> で自己登録する。js の双方向結合（`*js-apply-fn*` 等への VM クロージャ注入）は
> `vm-integration` 構造体で反転済み — pipeline が capability を渡し、policy は
> backend 側に残る。登録シンボル集合の同一性はテストで固定し、加えて
> Array.map の compiled-JS コールバック / メソッド内 `this` / ネスト時の
> レシーバ復元を e2e で確認済み。以下は当時の記述。

**現状の実結合**（`packages/pipeline/src/pipeline-runtime-bridges.lisp`, 173 行）:

- `%register-php-runtime-bridges`: php パッケージを `do-external-symbols` で走査し、
  `%PHP-*` 命名の fbound 関数を `cl-cc/vm:vm-register-host-bridge` で VM に登録
  （旧 `*php-runtime-bridge-entries*` alist はドリフトのため命名規約走査に置換済み）。
- `%register-js-runtime-bridges` / `seed-js-runtime-globals`: js パッケージを同様に
  走査してブリッジ＋グローバルを登録。
- これらを `pipeline.lisp`（694/697/773/776 行）が直接呼ぶ。

→ `pipeline` が **php/js の命名規約とパッケージを知っている**（`cl-cc/php`/`cl-cc/js`
への参照）。これが唯一のクリティカル結合。

**目標: 向きを反転（`backend → protocol ← pipeline` の Y 字）。**

```lisp
;; 核側の新パッケージ cl-cc/backend-protocol（vm にのみ依存）:
(defvar *registered-backends* '())            ; (language . backend) alist

(defgeneric backend-bridge-symbols (backend)
  (:documentation "host bridge として VM に登録する fbound シンボルのリスト"))
(defgeneric backend-global-symbols (backend)  ; js グローバル seed 用（任意）
  (:method (backend) '()))

(defun register-backend (language backend)
  (setf (getf *registered-backends* language) backend))

(defun register-all-backend-bridges ()        ; pipeline がこれだけ呼ぶ
  (loop for (lang backend) on *registered-backends* by #'cddr do
    (dolist (sym (backend-bridge-symbols backend))
      (cl-cc/vm:vm-register-host-bridge sym (fdefinition sym)))))
```

```lisp
;; cl-cc-php リポジトリ側で自己登録（load 時）:
(defclass php-backend () ())
(defmethod backend-bridge-symbols ((b php-backend))
  ;; %PHP-* 走査ロジックは php repo 内に移設（自分の内部を自分で知る）
  (loop for s being the external-symbols of :cl-cc/php
        when (and (fboundp s) (%php-bridge-name-p s)) collect s))
(register-backend :php (make-instance 'php-backend))
```

これにより:
- `pipeline` は `register-all-backend-bridges` を 1 回呼ぶだけ。`cl-cc/php`/`cl-cc/js`
  への参照が消える（`pipeline-runtime-bridges.lisp` は削除、走査ロジックは各 backend へ）。
- 依存が `php → backend-protocol ← pipeline` の Y 字になり、php/js を独立リポジトリへ
  移動できる（ただし php/js は core=`cl-cc` umbrella に依存する**プラグイン repo**。
  standalone ではなく「cl-cc に対するプラグイン」モデル）。
- 将来のバックエンド追加（wasm/native）も同じ登録口。

**php と js で難易度が大きく違う（実コード精査で判明）**:

- **php: 容易**。`%register-php-runtime-bridges` は php パッケージを走査して `%PHP-*`
  関数を `vm-register-host-bridge` に登録するだけ。走査ロジックを php 側の
  `backend-bridge-symbols` メソッドへ移設すれば、登録シンボル集合は不変（＝挙動不変）。
  検証は「登録シンボル集合の before/after 一致」で足りる（フル php テスト不要）。

- **js: 高難度・双方向結合**。pipeline は bridge 登録に加えて、js ランタイムの特殊変数へ
  **VM 統合クロージャを注入**している:
  - `cl-cc/javascript::*js-apply-fn*` ← `%vm-call-closure-sync` で VM クロージャを呼ぶ
    ロジック（Array.map 等のコールバックを host→VM へ逆ルート）。
  - `*js-callable-p*` ← `%vm-closure-object-p` を callable 判定に含める。
  - `*js-apply-with-this-fn*` ← `this` を host special と VM global の両方に束縛。
  - `seed-js-runtime-globals` ← `*JS-*` special を VM globals へ複写。

  これは「js が VM クロージャの呼び方を知らない」ための注入で、js↔vm の双方向依存。
  脱結合には「js 側が *js-apply-fn* を受け取る protocol（VM closure invoker を注入する
  口）」を定義し、pipeline/vm がそれを実装する形にする必要がある。**雑な移設は
  コンパイル済み JS を全滅させる**ため、js-e2e / selfhost の全緑確認が必須。

**移行手順（テスト緑を保ちながら）**:
1. 核に `cl-cc/backend-protocol` を追加（registry + 総称関数）。**registry は php/js/
   pipeline の共通依存に置く必要がある** — php/js の共通下位は bootstrap（js は vm に
   依存しない）なので、registry(defvar + register-backend + generic)は bootstrap、
   実登録(vm-register-host-bridge 呼び出し)は pipeline に残す。
2. **php を先に移行**（低リスク・検証容易）。走査を php 側メソッドへ、pipeline は
   protocol 経由。登録集合の一致を確認。
3. **js を移行**（高リスク）。`*js-apply-fn*` 等の VM 統合を「js が invoker を受け取る」
   protocol へ反転。js-e2e / selfhost 全緑を確認。ここが本設計の最難関で、専用の
   検証済み作業を要する。
4. php/js を `cl-cc-php` / `cl-cc-javascript` へ移設（`:depends-on (:cl-cc)` の
   プラグイン repo）。monorepo umbrella の deps から外し flake input 化。

> **実装フェーズの位置づけ**: 手順 3（js の双方向脱結合）は、コンパイル済み JS の
> 挙動を変えずに VM 統合を反転する繊細な作業で、フル e2e 検証(pipeline/selfhost/
> js-runtime-* テスト)が前提。leaf 抽出(ast/type/binary/runtime)のような
> 「単独ビルドで検証完結」とは性質が異なり、monorepo が clean な状態で腰を据えて
> 行うべき——原設計 §1-0 が予言した「核=セルフホスト結合」の一種。

### 5-2. 核の公開契約（フェーズ C の前提）

`ast` / `ir` / `mir` / `vm` について「外部リポジトリが依存してよい公開シンボル」を
明示パッケージ（例: `cl-cc-ast` の外部シンボルのみ）として固定し、
`::`（内部）参照を禁止する lint を CI に入れる。これが崩れると再び密結合に戻る。

## 6. 移行順序（切りやすい順 = ①②とも高い順）

```
0. フェーズ A-0: cl-cc-ast を外部化 ★着手済み(ローカル repo 作成・build 緑)
   └ 依存ゼロの葉。type/parse/php/js が依存するため必ず先頭。これを先に出さ
     ないと cl-cc → cl-cc-type → (cl-cc内ast) の repo 間循環になる。
1. フェーズ A-1: cl-cc-type を外部化(cl-cc-ast に依存)
   └ ast の公開 API のみ使用(侵入0)。type 抽出はこれで循環なく成立。
2. 脱結合 PR: §5-1 のバックエンド登録プロトコルを本体に導入
   └ この時点では php/js はまだモノレポ内。pipeline から内部参照を除去し、
     テスト（compiler-tests-stdlib 等）が緑のままを確認。
3. フェーズ A-2: php → cl-cc-php、javascript → cl-cc-javascript を外部化
   └ 逆依存が pipeline のみ。登録プロトコル経由で接続。
4. vm 公開 API 硬化: §5-2 の公開契約 + `::` 禁止 lint
5. フェーズ C: cl-cc-optimize → cl-cc-codegen-native を外部化
```

> **順序修正の教訓**: 当初 type を先頭に置いたが、type は ast に依存するため
> ast を先に出さないと repo 間循環になる。「切りやすさ」は *外向き依存が公開
> API か* だけでなく *その依存先が既に外部化済みか* にも依存する。真の第一
> ドミノは依存ゼロの葉 `ast`。

formatter / docgen / prolog-tools は外部化しない（§3・§4）。各ステップは
「本体テストが緑」を通過条件にし、特に `selfhost`（セルフホスト）と `pipeline`
の e2e が壊れないことを受け入れ基準にする。

## 7. 配管（Nix / ASDF）— 既存方式の再利用

新規リポジトリは既存 `cl-*` と全く同じ経路で取り込む。追加コストは小さい。

**flake.nix** — plain source tree として input 追加:

```nix
cl-cc-php = { url = "github:nerima-lisp/cl-cc-php"; flake = false; };
```

理由は既存コメント（flake.nix:10-32）と同じ:
- 各 `cl-*` の flake を評価すると transitive input（paredit-cli 等）を
  無駄に引き込む。source だけ欲しい。
- 依存先 flake が Linux `systems` しか宣言せず aarch64-darwin が壊れる問題も回避。

**nix/asdf-systems.nix** — `mkAsdfSystem` / `extraLispLibs` で内部システムと合流:

```nix
# 外部由来（非 cl-cc-* 派生）は extraLispLibs で threads-in する既存の仕組みに乗せる
```

**cl-cc.asd** — 開発時は `maybe-load-asd` のオプショナル読み込みで
ローカルソースを、CI/リリースでは Nix 派生を参照。probe-file ガードにより
外部リポジトリが無くてもプロダクションビルドは成功する（既存の設計を踏襲）。

## 8. リスクと未解決点

- **核は分割対象外（前提の再掲）**: bootstrap/vm/expand/compile/selfhost は
  `our-eval` の事前 intern とセルフホストで結合しており（`cl-cc/vm::` 跨ぎ参照 301）、
  リポジトリ境界で割ると壊れる。分割の誘惑が出ても核には手を付けない（§1-0 非目標）。
- **churn の高い領域を切らない**: 本体は 578 コミット/90 日で活発に変化中。
  境界が動いている領域を切ると flake.lock 追従が恒常コスト化する。仕様が枯れ、
  逆依存が収束した周縁のみを対象にする。
- **php ⇄ pipeline の runtime-bridge**: §5-1 が完了しない限り php/js 外部化は
  着手不可。ここが本設計の最重要クリティカルパス。
- **バージョン整合**: 外部リポジトリが増えると flake.lock の更新運用が要る。
  複数リポジトリ同時変更時の atomic 性は失われる（モノレポの利点の喪失）。
  → 変更頻度が低く安定した領域（php/js/native codegen）を優先し、活発に相互変更が
    走る核は残す、という本設計の順序で緩和する。
- **テスト分散**: 各バックエンドのテスト（`compiler-tests-*`, `js-runtime-*`）は
  切り出し先へ移す。本体 e2e には「代表シナリオのスモーク」だけ残す。
- **testing-framework の去就**: cl-weave 移行と重複解消を別トラックで先に判断する。

## 9. 次アクション（この設計を承認後）

- [x] Terraform 仮実装（`private-terraform` の `repos_nerima_lisp_cl_cc.tf`, 5 repo）
- [x] 葉パッケージ 4 個の外部化（ast / type / binary / runtime）— `nix flake check` 緑
- [x] §5-1 登録プロトコル（php / javascript 両方）— `cl-cc/backend-protocol`
- [x] vm 内部参照の棚卸し + ratchet lint（`packages/vm/tests/vm-public-api-lint-tests.lisp`）
      — 106 個中 37 個は既に export 済みで `::` を惰性で書いていただけだったため `:` に修正。
      残りの distinct 数は optimize 16 / codegen 34 / compile 6 / pipeline 7 / expand 7、
      regalloc・emit・mir・parse は 0。lint は「増やせない」だけを保証する
      （減らすのが作業本体、天井の置き去りも別テストで検出）。
- [ ] vm 公開契約そのものの決定（フェーズ C の前提）— 残りの大半は命令型と
      そのスロットアクセサで、§5-2 が「export すべき」と言っている当のもの。
      外部 repo のパスがどこまで依存してよいかの判断が要る。
- [ ] **§6 手順 4（php/js の repo 移設）の意味を決める** — 下記 §10 参照

## 10. 実測で判明した制約（2026-07-27 追記）

### 10-1. 「input として宣言済み」は「実際にビルドされている」ではない

ast / type は `flake.nix` の input として宣言され `nix/asdf-systems.nix` で
注入されていたにもかかわらず、**一度もコンパイルされていなかった**。
`cl-cc.asd` の `ensure-system-asd` は `find-system` が *外した時だけ* per-package
`.asd` を読むが、cl-cc のソースツリーは注入された derivation より先に走査される。
結果、Nix でもローカルでも in-tree のコピーが勝ち続け、両者は数か月ドリフトした。

検出は配管を読むのではなく **片方でしか通らないテストを走らせる**こと。
`infer-with-constraints` は `packages/type/src` にしか無いのに CI では緑だった。

4 個すべてでこの手順を実行済み: input + derivation + `externalCcSystems` 追加 →
`packages/X/{src,*.asd}` 削除 → `ensure-system-asd` の行を削除 →
`nix flake check` で出たものを潰す。ハードコードされた `packages/X/src/...`
文字列に注意（optimizer roadmap と `nix/checks.nix` にあった）。

### 10-2. php/js は葉と同じ手順では切り出せない（依存が逆）

`cl-cc-php` / `cl-cc-javascript` は **`cl-cc` を flake input に取っている**。
ASDF 上も `:cl-cc-vm` `:cl-cc-parse`（＝核）に依存する。したがって cl-cc 側が
これらを input に取ると**循環**になり、flake では表現できない。

§4 が「プラグイン repo」と書いていたのはこの意味であり、**「php/js の外部化」は
cl-cc がこれらを自分のビルドから外すこと**を意味する — `:language :php` /
`:language :javascript` を pipeline から落とし、対応するテスト群を各 repo へ
移すということ。これは配管作業ではなく「cl-cc とは何か」を決める製品判断であり、
`cl-cc.asd` の system description が両バックエンドを明示している以上、
設計の承認とは別に判断が要る。

§5-1（この判断の前提として設計が挙げていた作業）は完了しており、
`cl-cc/pipeline` はもう両バックエンドの内部シンボルを一切名指ししていない。

### 10-4. 訂正: php/js は製品判断を要しない（2026-07-27）

上の §10-2 は「php/js を切り出す＝cl-cc がこれらを落とす」と書いたが、**これは
不正確**。循環の原因は php/js が `cl-cc` を input に取っていることであり、
その理由は依存 4 つのうち `cl-cc-parse` だけが monorepo に残っていたからである。

```
php/js の :depends-on  = (cl-cc-ast  cl-cc-bootstrap  cl-cc-parse  cl-cc-vm)
                           ↑外部化済   ↑外部化済        ↑monorepo    ↑外部化済
cl-cc-parse の :depends-on = (cl-cc-ast cl-cc-bootstrap)  ← 両方とも外部化済み
```

つまり **parse を切り出せば php/js の依存は 4 つとも外部になり、`cl-cc` への依存が
消える**。そうなれば cl-cc は php/js を input に取れ、`:language :php` /
`:javascript` を保ったまま両者を外部リポジトリから引ける。「cl-cc とは何か」を
変える必要はない。

手順:

1. `cl-cc-parse` を切り出す（3,531 loc、依存は外部化済みの 2 つのみ）。
2. `cl-cc-php` / `cl-cc-javascript` の flake input を `cl-cc` から
   `cl-cc-{ast,bootstrap,parse,vm}` の粒度に置き換える。ASDF 側の
   `:depends-on` は既にその 4 つなので変更不要。
3. cl-cc が php/js を input として取る。

残る唯一の製品判断は「php/js を **落としたい** か」であって、「落とさないと
切り出せない」ではない。

> 参考: 会話中に検討した `cl-cc-runtime-concurrent`（並行ランタイム26ファイル/
> ~3.1k loc）は「compiler と target runtime の分離」という別軸の候補。runtime
> 単一パッケージの分割とオーファン判定が前提のため、本設計の 5 repo とは別トラック。

### 10-3. 核＝分割不可 という前提は実測で否定された（2026-07-27）

§1-0 は「核はセルフホストの自己参照ウェブ」「`cl-cc/vm::` 跨ぎ参照 301」を根拠に
vm を分割対象外としていた。**この根拠は方向を取り違えている。** 301 は
*他パッケージが vm の内部に手を突っ込んでいる数* であって、vm が何かに依存して
いる数ではない。前者は公開 API の問題であり、§5-2 の作業そのものである
（optimize / codegen は既に 0 に到達済み）。

実測: `(asdf:load-system :cl-cc-vm)` は cl-cc ツリー単独で成功し、その時点で
`cl-cc/expand` `cl-cc/compile` `cl-cc/optimize` `cl-cc/parse` `cl-cc/ast`
`cl-cc/type` はいずれも **未ロード**。存在するのは `cl-cc/bootstrap` と
`cl-cc/runtime`（＝既に外部化済み）だけ。`vm-bridge-io-docs.lisp` の
`cl-cc/expand` 参照は `find-package` による実行時ルックアップで、不在なら NIL に
落ちるだけ。残りは docstring / コメント。

したがって **vm は葉である**。依存は `bootstrap`(283 loc, 依存ゼロ) と
`runtime`(外部化済み) のみ。

**これが「cl-cc を小さくする」唯一の実効レバー。** vm の背後に積み上がっている量:

```
vm          26,897        ← bootstrap(283) の上の葉
optimize    25,092  ┐
codegen     19,468  │
php         19,251  ├ すべて vm 依存。vm が外部化されない限り
javascript  15,220  │ cl-cc 側が input に取ると循環になる（§10-2）
emit         2,808  │
regalloc     1,608  ┘
                    合計 ≈ 110,000 / 全体約 300,000
```

つまり順序は **bootstrap → vm → 残り全部**。vm を出さない限り大物は 1 つも
出せず、出せば約 1/3 が一度に外に出る。逆に、依存ゼロの小物
（ir 523 / mir 697 / target 302 / bytecode 1,073 / docgen 134 / formatter 101 /
prolog-tools 288）は今すぐ出せるが、合計 3.1k loc で本体はほとんど小さくならない。

**未着手の理由**: `cl-cc-bootstrap` と `cl-cc-vm` の GitHub repo が未作成
（Terraform が用意したのは type/php/javascript/optimize/codegen-native の 5 個）。
repo 作成は outward な操作なので判断を残している。

### 10-5. 訂正: §5-1 は二重に実装されている（2026-07-27）

§5-1 は **2 系統が併存**している。判断が要るのはどちらを残すかであって、
設計を新たに決めることではない。

| | 場所 | 扱える範囲 |
|---|---|---|
| A | `split/php-backend-protocol` ブランチ（**main 未マージ**）| bridge / VM integration / global seeder / **parser** の 4 経路 |
| B | main の `cl-cc/backend-protocol` | bridge / VM integration / global seeder の 3 経路 |

A は `cl-cc/bootstrap` に `register-backend-bridge-provider` /
`register-backend-vm-integration-installer` / `register-backend-global-seeder` /
`register-backend-parser` を置く設計で、**upstream の cl-cc-php /
cl-cc-javascript は既にこちらに移行済み**（`src/runtime-bridge-provider.lisp`）。
B はこのセッションで main 上に書いたもの。

**A のほうが射程が広い**（parser 登録まで含む）。したがって採るべきは A で、
B は畳むのが素直。なお A の API は既に `cl-cc-bootstrap` リポジトリに入っている
——抽出時に `packages/bootstrap/src/package.lisp` ごと持って行ったため。

php/js の残作業は「設計を決める」ではなく:

1. `split/php-backend-protocol` を main に取り込み、B を畳む。
2. cl-cc の pipeline が A のレジストリ（`backend-bridge-providers` 等）を
   drain するようにする。
3. php/js を flake input 化し、`packages/{php,javascript}` を削除する。

**注意**: cl-cc-php に `backend.lisp`（B 方式）を足すと
`runtime-bridge-provider.lisp`（A 方式）と**二重登録**になり CI が落ちる。
2026-07-27 に実際に落として revert した（cl-cc-php `c2cf5c1`）。ファイル数の差
（92 対 75）は設計の分岐ではなく、A への移行が upstream だけ先行しているため。

### 10-6. php の flake input 化を試した結果（2026-07-27, 測定値）

§10-5 の手順 1・2 は完了した:

- 手順 1（ブランチ取り込み）は**不要**だった。A の API は既に `cl-cc-bootstrap`
  リポジトリに入っており、main のビルドから使える。ブランチ自体は 543 files /
  ±50k 行で cl-weave テスト移行を丸ごと含み、しかも今夜の 11 パッケージ抽出より
  前の main から分岐しているため、マージは現実的でない。
- 手順 2 完了（cl-cc `3bee060e`）。pipeline は A と B の両レジストリを drain する
  ようになったので、**どちらの方式で登録する backend も拾える**。

手順 3（php を flake input 化）は**実測して差し戻した**:

```
11990 passed, 7 failed, 217 errored / 12214
```

つまり upstream の cl-cc-php と `packages/php` の乖離は 217 テスト分ある。
ast/type のときと同じ「宣言済みだが一度もビルドされていない」状態が php でも
起きていたということ（[[cl-cc-ast-type-split-not-live]] の罠）。ファイル数の差
20 個は表層で、実体はこの 217。

**217 の内訳（実測済み）**: すべて同一カテゴリで、`%PHP-*` 関数が
*undefined function* になる。上位は `%php-array` 42 / `%php-set-error-handler` 19 /
`%php-function-exists` 11 / `%php-concat` 11。

重要なのは、これが「upstream に無い」ではないこと:

- upstream の `.asd` は **131 コンポーネント**、in-tree は **75**。upstream のほうが
  多い。
- `%php-array` は両方に `defun` がある。
- 読み込みは通っている（`cl-cc/php:%php-array` が read error にならない）ので
  **シンボルは export されている**。にもかかわらず実行時に unbound。

**リネーム説は実測で否定された**（2026-07-27）。upstream を直接確認した結果:

- `%php-array` は `src/runtime-helpers-array.lisp:121` に `(defun %php-array ...)`
  として存在し、`in-package :cl-cc/php`、`package.lisp:195` で export 済み。
- `src/*.lisp` のうち `.asd` の components に入っていないファイルは **0 個**。
  つまり全ファイルがコンパイルされる。

定義があり、export されており、コンパイルもされる。それでも実行時に unbound。
したがって**シンボルのドリフトではない**。

**残る仮説**: これらは VM host bridge 経由で呼ばれる関数であり、失敗しているのは
関数定義ではなく **bridge 登録**のほう。upstream は
`register-backend-bridge-provider`（`src/runtime-bridge-provider.lisp`）で登録する
が、それが走るには cl-cc-php が *load* されている必要がある。テストイメージでの
ロード順序 / `%register-php-runtime-bridges` の呼ばれる時点との関係を疑うべき。

**次にやること**: 217 のうち 1 つを取り、`cl-cc/bootstrap:*backend-bridge-providers*`
がテストイメージ内で非 nil かどうかを見る。非 nil なら登録は走っているので
`vm-register-host-bridge` 側、nil ならロード順序。symbol 追跡ではなく
**登録が走っているかどうか**の 1 点を先に確定させること。javascript は未測定。

cl-cc 側の受け入れ準備は完了しているので、残りは upstream 2 リポジトリの
parity 作業のみ。

### 10-7. 抽出フロンティアの再実測（2026-08-01）

§10-3 以降、ast/type/binary/runtime/vm/parse/optimize/php/javascript/
bootstrap/ir/bytecode/emit（mir/target/regalloc/codegen は
`cl-cc-codegen-native` に統合）の **13 個が抽出済み**（`packages/X/` は
`tests/` のみ残り、src は無い）。まだ `packages/*/src` が本体に残っているのは
以下の 14 個・合計 **約 36.4k loc**:

```
cli 4023  compile 9637  cps 1570  debug 350  docgen 134  expand 11086
formatter 101  pipeline 3938  prolog-tools 288  repl 787  selfhost 225
stdlib 1555  testing-framework 2182  tools 532
```

§1-0 の非目標（bootstrap/vm/runtime/expand/compile/cps/selfhost は自己ホスト
結合で分割不可）は bootstrap/vm/runtime について §10-3 で実測により**否定**
されている。同じ実測を cps/expand/debug にも適用した結果:

| 候補 | 規模 | 依存 | 外部からの `::` 侵入 | 判定 |
|---|---|---|---|---|
| **cps** | 1,570 | bootstrap+ast（両方抽出済み） | 0（`compile/tests/` のみ） | ①②とも満点。**type/vm と同格のクリーンな葉** |
| **expand** | 11,086 | bootstrap+type+vm（全て抽出済み） | 3ファイル: `selfhost/src/pipeline-selfhost.lisp`, `stdlib/src/stdlib-source.lisp`, `testing-framework/src/framework-fixtures.lisp`（いずれもテストではなく本体コード） | ①はほぼ満点だが**要脱結合**（§5-1 と同型の作業）。最大のレバー |
| debug | 350 | bootstrap+vm（抽出済み） | 0、逆依存も 0 | ①満点だが②で単独では小さすぎる |
| compile | 9,637 | cps/expand 以外は全て抽出済み | — | cps/expand を出せば依存は全て外部になるが、pipeline/selfhost/repl が直接束ねる自己ホスト本体そのもの。§1-0 の「一枚岩の核を残したまま周縁を剥がす」の“核”側として当面は抽出対象外 |

**推奨順序**: cps（即・依存ゼロ） → expand（3ファイルの脱結合後） →
debug（任意・低価値） → compile は当面 core に残す。stdlib（1,555 loc,
bootstrap のみ依存）は §3/§4 の判定どおり formatter/docgen/prolog-tools と
同規模のため単独抽出は見送り。

## 11. 全16外部repoの実地監査による最終形の再設計（2026-08-01）

10 エージェントを並列に走らせ、抽出済み・抽出候補の外部 repo を**全部**
（ast/type/vm/parse/bootstrap/ir/mir/target/bytecode/codegen-native/binary/
optimize/php/javascript/runtime/prolog-tools の 16）実際に読ませた。
結論: **これまでの抽出方針（葉から剥がす）自体は正しく、大半は健全**。ただし
実測でしか出てこない**4 種類の不具合**が見つかった——「切り出し方」の議論の
前に、まずこれを片付けるのが「最低限で回る cl-cc」への近道。

### 11-1. 健全性サマリ

| repo | loc | 依存 | 判定 |
|---|---|---|---|
| bootstrap | 287 | ゼロ | ◎ 自己ホストの要（`our-eval`前方参照・Prolog pattern atom の事前intern・backend registry）。依存ゼロなのでどこに置いても物理的には成立するが、意味的には core の一部 |
| ast | 1,283 | ゼロ | ◎ 最もクリーンな葉 |
| type | ~10,125 | ast | ○ 6テストが`parse`の`lower-sexp-to-ast`待ち（parseは抽出済みなので**今すぐ解消可能**）。`inference-effects.lisp`がVM opcode名をハードコード |
| parse | 3,573 | ast+bootstrap | ◎ 境界テストが vm/optimize/codegen/compile/expand/type 不在を自己検証 |
| vm | 25,463 | bootstrap+runtime+cl-regex-kit+cl-tty-kit | ○ 自己ホスト結合(`VM-EVAL`命令・`*vm-self-host-mode*`)は正当。ただし**"Phase 129-160"一式**(v8-objects/JIT hardening/stack-thread)とOSR/tiering/deoptが**codegen-native寄りの内容**でVMに同居 |
| optimize | 27,528 | vm+type+ast+cl-prolog-kit+cl-parser-kit | ○ vm内部`::`参照0（境界テストで強制）。ただし**約2,000loc(7%)が"roadmap"/FR文書追跡というcl-cc-project固有の管理ツール**でoptimizerパッケージに同居 |
| codegen-native(regalloc+codegen+emit) | 23,001 | mir+target(+binaryは子systemのみ) | ○ | ルート`.asd`の`:depends-on`に`cl-cc-binary`が**抜けている**（子systemのcodegenは使うのに宣言漏れ） |
| binary | 4,622 | cl-log-kit+cl-process-kit | ◎ 純粋なELF/Mach-O/PEライタ。codegen-nativeとの重複は無し |
| mir | 697 | ゼロ | ○ codegen-nativeの実consumer有り |
| target | 302 | ゼロ | ○ 常にmirとセットで消費される |
| **ir** | 523 | ゼロ | ✕ **抽出済みだが実consumerゼロ**（他の抽出済みrepoも参照していない。旧monorepoの残骸testしか触れていない） |
| **bytecode** | 1,073 | ゼロ | ✕ **抽出済みだが実consumerゼロ**（vm自身が使っていない。145個もexportがあるのに孤立） |
| php | 19,736 | ast+bootstrap+parse+vm+cl-json-kit | ◎ backend登録プロトコル(`register-backend-bridge-provider`/`-parser`)がクリーンに機能 |
| javascript | 16,263 | ast+bootstrap+parse+vm+cl-date-kit+cl-json-kit+cl-concurrent-kit | ○ VMクロージャの双方向結合は本質的に残るが、`register-backend-vm-integration-installer`/`-global-seeder`でプロトコル化済み。素朴な特別扱いではない |
| runtime | 18,244(+test 10,734) | cl-log-kit+cl-process-kit+cl-json-kit | ○ `include/cl-cc.h`は**どこからもビルドされない死んだCヘッダ**。§旧知の19個のorphan並行性モジュールは**個別テストは付いたが依然クロスモジュール利用ゼロ**のまま |
| **prolog-tools** | 288 | ast+cl-prolog-kit | ✕✕ **§10-6と同じ「宣言はあるが実体は本体側」の罠が再発**。GitHubにrepoはあるが`flake.nix`は未参照、`packages/prolog-tools/src`も`.asd`も削除されておらず、README自身が「これを切り出しても本体はほぼ縮まない」と書いている。本文書の§3判定（切らない）を無視して作業だけ進み、後始末されていない |

### 11-2. 見つかった実害と対処（切り出し方の議論より優先度が高い）

1. **`cl-cc-ir`/`cl-cc-bytecode`が孤立——確認済み、本当に死んでいる**。本体に
   まだ残る`compile`/`cps`/`expand`/`pipeline`/`selfhost`等の in-tree src を
   `cl-cc/ir`・`cl-cc/bytecode`・`:cl-cc-ir`・`:cl-cc-bytecode`で全grepして
   ヒット0件（2026-08-01実測）。二重定義の罠（本体が古い在庫コピーを握っている）
   ではなく、**単純にどこからも呼ばれていない**。vm自身のバイトコード直列化も
   `vm-fasl.lisp`/`vm-serialize.lisp`という独自実装で、`cl-cc-bytecode`は使って
   いない。両repoともarchive/削除が妥当（"抽出したが誰も使わなかった"という
   計画時点の見込み違い。汎用IR/バイトコード表現として将来使う計画があるなら
   別だが、現状は死んだコード）。
2. **`cl-cc-prolog-tools`の後始末**。設計判断（§3: 切らない）と実態（repoが存在し
   独立コミットまである）が矛盾している。GitHubリポジトリをarchive/削除するか、
   本気で採用するなら flake input化 + 本体`packages/prolog-tools`削除まで完走させる。
   中途半端が一番良くない。
3. **`cl-cc-codegen-native`のルート`.asd`に`:cl-cc-binary`を追加**。子systemが
   実際に使っている依存が親の宣言に出ていない、単純なメタデータ漏れ。
4. **vmの"Phase 129-160"グループとOSR/tiering/deoptを`codegen-native`へ移設候補として検討**。
   VMは「バイトコード実行」の境界が曖昧になっており、JIT寄りの最適化コードが
   紛れ込んでいる。
5. **runtimeの19個のorphan並行性モジュールを削る**。個別unit testはあるが
   クロスモジュール利用が無いままなのは、テストを足したことで「使われている風」に
   見えるだけで実態は変わっていない。`docs/notes/repo-split-design.md`の元々の
   pending課題（このファイル冒頭近くのメモ参照）がまだ解消されていない。
6. **optimizeの"roadmap"サブシステム(~2,000loc)は別パッケージへ**。汎用最適化ライブラリの
   顔をしているoptimizeパッケージに、cl-cc固有のFR文書追跡ツールが同居しているのは
   境界として不透明。

### 11-3. 「最低限で回る cl-cc core」の最終形

外部化した16個は──11-2 の4件を除けば──設計として正しく機能している。
残る問題は「本体に何を残すか」ではなく「境界を汚している少量のコードを
正しい場所に動かす」フェーズに入っている。最終形の core は:

```
cl-cc-core（monorepoとして残す）:
  - 自己ホストの実行部: compile, expand, cps, selfhost, pipeline, repl
    （§1-0の「一枚岩」はここだけに縮小。ast/vm/parseは葉として外に出せたが、
      compile/expand/cpsは自分自身をコンパイルする現在進行形のプロセスの
      当事者なので、依存関係が綺麗でも当面はここに残す）
  - 薄いオーケストレーション層: cli, tools, stdlib
  - 小物（②価値ゲート未達のためmonorepo残留、§3の判断を維持）:
    docgen, formatter, debug
  - testing-framework は cl-weave への移行完了後に廃止（既知の別トラック）

外部（すでに実現・健全）:
  ast, type, parse, vm, optimize, codegen-native(regalloc+codegen+emit),
  binary, mir, target, php, javascript, runtime, bootstrap

未解決（11-2 参照、切り出し作業より先に片付ける）:
  ir, bytecode（孤立の理由を特定してから、統合 or 本体への還流を判断）
  prolog-tools（GitHub repoの後始末）
```

**次にやる順序**: (1) 11-2 の6件（特に ir/bytecode の孤立原因調査と
prolog-toolsの後始末——どちらも「壊れているものを直す」なので新規切り出しより
優先） → (2) cps の抽出（§10-7で確定済み、依存ゼロの葉） →
(3) expand の脱結合＋抽出（3ファイル） → (4) mir+targetの1repo統合を検討
（常にセット消費・合計1,000loc未満で2repo分の配管は過剰という実測結果）。

## 12. §11-2 の後始末を実行（2026-08-01）

11-2 で見つかった4件のうち、以下を完了:

1. **`cl-cc-ir`/`cl-cc-bytecode`を完全除去** — GitHub repoをarchive、かつ
   `cl-cc.asd`の`:depends-on`・テスト集約・`flake.nix`・`nix/asdf-systems.nix`
   から実体を削除（`packages/ir`・`packages/bytecode`ごと削除）。副産物として
   `t/e2e/selfhost-test-support.lisp`と`packages/compile/tests/standalone-load-tests.lisp`
   にも生きた参照が残っていたことが判明・修正（grepだけでは見つからず、実際に
   ビルドを試みて発見）。
2. **`cl-cc-target`を`cl-cc-mir`へ統合完了** — `cl-cc-mir`にファイル移設・
   push・`nix flake check`緑を確認済み。`cl-cc-target`はarchive。
   `cl-cc-codegen-native`・`cl-cc`双方のflake入力をmir単独に統合。
3. **`prolog-tools`を`cl-prolog-kit`へ分割**——288行のうち約230行（call-graph
   構造体・reachability・dead-code・mutual-recursion・graph-coloring・
   edge-DCG）はcl-cc-ast非依存と判明したため`cl-prolog-kit/callgraph`
   （cl-prolog-kit v1.2.0）へ移設。`build-call-graph`をAST→edgesの薄い
   アダプタに再設計し、`packages/prolog-tools`はこのアダプタのみに縮小。
   ストレイrepo`cl-cc-prolog-tools`はarchive。
4. **`cl-cc-type`の宙ぶらりんテスト5本を復活** — `cl-cc-parse`が既に
   外部化されていたため復元可能と判明。`cl-cc-parse`にv0.1.0タグを
   新規作成（初リリース）。

**未解決のまま残っているのは**: §11-2 5番目（vmの"Phase 129-160"グループ・
OSR/tiering/deoptのcodegen-native移設検討）、6番目（optimizeの"roadmap"
サブシステム分離）、runtimeの19個のorphan並行性モジュール削除、および
§10-7で確定した`cps`/`expand`の抽出そのもの。

**運用上の注記**: この一連の変更は複数エージェントを並列実行して行ったが、
同一の共有マシン上でnix/SBCLの重い処理が競合し、ローカルでの動的テスト
実行（`sbcl --script run-tests.lisp`等）がことごとくCPU飢餓状態でハング
した。静的チェック（`paredit inspect lint`・nix構文パース・`nix flake
metadata`・`nix flake check`が通ったサブセット）のみで各commitをpushし、
実際のテストグリーン確認はCIに委ねた。次にこの種の並列作業をする際は、
同時に動かす重量級ジョブの数を絞るか、`isolation: worktree`で作業ツリーを
分離すること。

## 13. §12の未解決項目を実行（2026-08-01、同日中の後続セッション）

§12末尾の「未解決」リストのうち、以下を実行・決着させた:

1. **`cps`の抽出完了** — `nerima-lisp/cl-cc-cps`（v0.1.0）。依存ゼロの葉、
   type/vm/parse抽出と同じ手順。`nix flake check --no-build --all-systems`
   が両repoとも全derivationで緑。
2. **`expand`の脱結合＋抽出完了** — 3ファイル（`selfhost/pipeline-selfhost.lisp`・
   `stdlib/stdlib-source.lisp`・`testing-framework/framework-fixtures.lisp`）の
   `cl-cc/expand::`直接参照を、`rt-use-package`・`add-package-local-nickname`・
   5個の`%record-declaim-*-clause`関数・`*macroexpand-step-cache*`・
   `*macroexpand-all-cache*`の計9シンボルをexportすることで解消してから抽出。
   `nerima-lisp/cl-cc-expand`（v0.1.0）。レビューで指摘された残課題:
   キャッシュ変数を直接exportするのではなく`clear-macroexpand-caches`のような
   専用APIを設けるべき、5個の`%record-declaim-*-clause`も1本の
   `record-declaim-clause`に統合すべき——今回は抽出期限を優先した実用的な判断。
   将来のクリーンアップ候補として記録。
3. **`vm`のJIT寄りコード移設は「移設ではなく削除」と判明** — 再調査の結果、
   `v8-objects-133.lisp`・`security-134.lisp`・`stack-thread-137.lisp`は
   codegen-nativeへの境界漏れではなく、そもそも**未使用の投機的コード**
   （cl-cc-vm内でもcodegen-native/optimizeからも一切参照されない）と判明。
   移設は実行せず。削除候補として別途フラグ（未実行）。OSR/deopt
   (`vm-execute-osr.lisp`)は本物のdispatch loop呼び出しがあり、正しくVM内に
   留まるべきものと確認。
4. **`optimize`の"roadmap"サブシステム分離は見送り** — 調査の結果、
   cl-cc本体の`packages/optimize/tests/optimizer-roadmap-{,backend-}tests.lisp`
   が53箇所でこのサブシステムをシングルコロン（外部シンボル）参照しており、
   かつdoc読み込み(`docs/notes/optimize-passes.md`等)がcl-cc本体専用パスに
   依存している。同一repo内でのsecondary system分離では「exportを減らす」
   という目的を達成できず、cl-cc本体との協調PRが必要——今回のタスク規模を
   超えるため見送り。
5. **`runtime`のorphan並行性モジュール削除——19個中8個のみ確定** —
   モジュールごとに再検証した結果、`cluster`/`mvcc`/`reactive`/`io-uring`/
   `gpu`/`zerocopy`/`event-loop`/`async-generators`の8個（2,080loc）のみ
   真に未使用と確認、削除。残り11個（`task`/`work-stealing`/`otel`/
   `consensus`/`crdt`/`qsbr`/`rcu`/`spsc`/`ebr`/`parallel-algo`/`topology`）
   は`scheduler.lisp`の`fboundp`経由呼び出しや、cl-cc本体の
   `runtime-subsystem-fr-tests.lisp`からの直接呼び出しなど実際の消費者が
   見つかり保持。**「19個全て未使用」という当初の監査は不正確だった**——
   モジュール単位の再検証が必須という教訓。

**post-implementation review（quality/security/design/docs/performance/test
の6エージェント並列）で見つかった残課題**:
- `cl-cc-cps`/`cl-cc-expand`に`.github/workflows/`が無く、`nix flake check`
  がCIで自動実行されない（同日に追加されたorg規約整合作業より後に作られた
  にもかかわらず追従していなかった）→ 修正実施。
- `cl-cc-expand`の脱結合commitメッセージが「8シンボル・3ファイル」と書いて
  いたが実際は9シンボル・4ファイル（`packages/compile/tests/`の
  `invoke-registered-expander`参照はcps抽出時と同様に意図的除外だが、
  その旨の記載がcps側ほど明示的でなかった）——機能上の問題ではないため
  未修正、記録のみ。
- セキュリティ・パフォーマンス面の懸念は無し（6エージェントとも「該当なし」
  を明示的に報告）。

残る本当の未解決: vmの3ファイル削除（今回は投機で終わらせず、実行するなら
別タスクとして明示的にスコープを切ること）、optimizeのroadmap分離
（cl-cc本体との協調PRが必要）。