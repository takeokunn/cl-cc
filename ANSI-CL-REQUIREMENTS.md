# ANSI Common Lisp 完全準拠 要件定義

## サマリー

cl-cc コンパイラの入力言語・実行セマンティクス・標準ライブラリを ANSI X3.226-1994 (CL HyperSpec) に完全準拠させ、ANSI-TEST (~21,000 テスト) をすべてパスする状態にする。

---

## 現状分析

### 現在のサポート状況

| カテゴリ | 現状 | 備考 |
|---|---|---|
| 特殊演算子 (25) | 22/25 実装済み | `locally`, `progv`, `load-time-value` が欠如 |
| 標準マクロ (~90) | ~40/90 実装済み | `handler-bind`, `restart-case`, `shiftf`, `psetf`, `assert`, `define-condition` 等が欠如 |
| 数値 | 基本算術のみ | `round`, `ash`, `logand`, bignum, float, rational, complex が欠如 |
| 文字列/文字 | 最小限 | `subseq`, `search`, `string-trim`, `char=`/`char<` 等が欠如 |
| シーケンス | リストのみ | 配列・ベクタ対応がほぼなし |
| I/O | print/format 部分対応 | read 系, stream 系が欠如 |
| コンディション | handler-case のみ | handler-bind, restart system が欠如 |
| パッケージ | no-op | 完全未実装 |
| CLOS高度機能 | 基本のみ | method-combination, change-class 等が欠如 |

### テスト基準

- テストスイート: **ANSI-TEST** (Peter Van Eynde, ~21,000 テスト)
  - 入手先: `https://gitlab.common-lisp.net/ansi-test/ansi-test`
- 既存テスト: FiveAM → 独自フレームワーク移行後も全パスを維持 (EXECUTION.md 参照)

---

## 機能要件

### FR-100: 特殊演算子の完全実装

| ID | 要件 | 対象形式 | 優先度 |
|---|---|---|---|
| FR-101 | `locally` の実装 | `(locally declarations* form*)` | Mandatory |
| FR-102 | `progv` の実装 | `(progv symbols values form*)` | Mandatory |
| FR-103 | `load-time-value` の実装 | `(load-time-value form &optional read-only-p)` | Mandatory |

**技術制約**: `progv` は VM に動的変数束縛スタックの追加が必要。

---

### FR-200: 標準マクロの完全実装

| ID | 要件 | 対象 | 優先度 |
|---|---|---|---|
| FR-201 | `handler-bind` | コンディション捕捉(スタック巻き戻しなし) | Mandatory |
| FR-202 | `restart-case` / `restart-bind` | リスタートシステム | Mandatory |
| FR-203 | `assert` (リスタート付き) | `(assert test (place*) datum arg*)` | Mandatory |
| FR-204 | `define-condition` | コンディション型定義 | Mandatory |
| FR-205 | `with-accessors` | スロットアクセサ束縛 | Mandatory |
| FR-206 | `shiftf` | `(shiftf place* newvalue)` | Mandatory |
| FR-207 | `psetf` | 並列 setf | Mandatory |
| FR-208 | `define-modify-macro` | ユーザー定義 modify マクロ | Mandatory |
| FR-209 | `with-input-from-string` | 文字列から入力ストリーム | Mandatory |
| FR-210 | `with-standard-io-syntax` | 標準 I/O 設定 | Mandatory |
| FR-211 | `with-package-iterator` | パッケージシンボル反復 | Mandatory |
| FR-212 | `define-compiler-macro` | コンパイラマクロ定義 | Mandatory |

---

### FR-300: 数値塔 (Numeric Tower)

| ID | 要件 | 関数/型 | 優先度 |
|---|---|---|---|
| FR-301 | 丸め関数 | `round`, `ffloor`, `fceiling`, `ftruncate`, `fround` | Mandatory |
| FR-302 | 数値変換 | `parse-integer`, `parse-float`, `number-to-string` (`write-to-string`) | Mandatory |
| FR-303 | ビット演算 | `ash`, `logand`, `logior`, `logxor`, `lognot`, `logeqv`, `logtest`, `logcount`, `integer-length` | Mandatory |
| FR-304 | 超越関数 | `expt`, `sqrt`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh` | Mandatory |
| FR-305 | Float 操作 | `float`, `float-precision`, `float-radix`, `float-sign`, `float-digits`, `decode-float`, `scale-float`, `integer-decode-float` | Mandatory |
| FR-306 | 有理数 | `rational`, `rationalize`, `numerator`, `denominator`, `gcd`, `lcm` | Mandatory |
| FR-307 | 複素数 | `complex`, `realpart`, `imagpart`, `conjugate`, `phase`, `abs` (complex版) | Mandatory |
| FR-308 | 多倍長整数 (Bignum) | 任意精度整数演算、VM の整数型拡張 | Mandatory |

**技術制約**: FR-308 (Bignum) は VM の integer 表現を `fixnum` 単一から tagged union に変更する必要があり、全 VM 命令の型処理に影響する最大規模の変更。

---

### FR-400: 文字列・文字操作

| ID | 要件 | 関数 | 優先度 |
|---|---|---|---|
| FR-401 | 部分文字列 | `subseq` (文字列版) | Mandatory |
| FR-402 | 検索 | `search` (文字列・シーケンス版) | Mandatory |
| FR-403 | トリム | `string-trim`, `string-left-trim`, `string-right-trim` | Mandatory |
| FR-404 | 文字列変換 | `string` (symbol/char → string) | Mandatory |
| FR-405 | 文字列生成 | `make-string` | Mandatory |
| FR-406 | 文字比較 | `char=`, `char/=`, `char<`, `char>`, `char<=`, `char>=` | Mandatory |
| FR-407 | 大文字小文字非依存比較 | `char-equal`, `char-not-equal`, `char-lessp`, `char-greaterp` | Mandatory |
| FR-408 | 文字変換 | `char-upcase`, `char-downcase` | Mandatory |
| FR-409 | 文字述語 | `upper-case-p`, `lower-case-p`, `both-case-p`, `digit-char-p`, `alpha-char-p`, `graphic-char-p`, `standard-char-p` | Mandatory |
| FR-410 | 文字変換その他 | `digit-char`, `char-int`, `char-name`, `name-char` | Mandatory |
| FR-411 | 文字列比較拡張 | `string/=`, `string>=`, `string<=`, `string-not-equal`, `string-lessp`, `string-greaterp` | Mandatory |

---

### FR-500: シーケンス操作

| ID | 要件 | 関数 | 優先度 |
|---|---|---|---|
| FR-501 | 汎用 subseq | `subseq` (配列・ベクタ対応) | Mandatory |
| FR-502 | fill/replace | `fill`, `replace` | Mandatory |
| FR-503 | 多シーケンス map | `map` (複数シーケンス版), `map-into` | Mandatory |
| FR-504 | merge/delete | `merge`, `delete`, `delete-if`, `delete-if-not`, `delete-duplicates` | Mandatory |
| FR-505 | substitute | `substitute`, `substitute-if`, `substitute-if-not`, `nsubstitute`, `nsubstitute-if`, `nsubstitute-if-not` | Mandatory |
| FR-506 | mismatch | `mismatch` | Mandatory |
| FR-507 | copy-seq | `copy-seq` | Mandatory |

---

### FR-600: 配列・ベクタ

| ID | 要件 | 対象 | 優先度 |
|---|---|---|---|
| FR-601 | 多次元配列 | `array-rank`, `array-dimension`, `array-dimensions`, `array-total-size` | Mandatory |
| FR-602 | row-major アクセス | `array-row-major-index`, `row-major-aref` | Mandatory |
| FR-603 | svref | `svref` (simple-vector high-speed access) | Mandatory |
| FR-604 | fill-pointer | `vector-push`, `vector-pop`, `vector-push-extend`, `fill-pointer`, `array-has-fill-pointer-p` | Mandatory |
| FR-605 | 配列調整 | `adjust-array`, `array-adjustable-p`, `array-displacement` | Mandatory |
| FR-606 | bit 配列 | `bit`, `sbit`, `bit-and`, `bit-or`, `bit-xor`, `bit-not` | Mandatory |

---

### FR-700: I/O システム

| ID | 要件 | 対象 | 優先度 |
|---|---|---|---|
| FR-701 | Read 系 | `read`, `read-preserving-whitespace`, `read-from-string`, `read-delimited-list` | Mandatory |
| FR-702 | Read-char 系 | `read-char`, `unread-char`, `peek-char`, `read-line`, `read-sequence`, `read-byte` | Mandatory |
| FR-703 | Write 系 | `write`, `write-char`, `write-string`, `write-line`, `write-sequence`, `write-byte` | Mandatory |
| FR-704 | 出力制御 | `fresh-line`, `terpri`, `finish-output`, `force-output`, `clear-output` | Mandatory |
| FR-705 | ストリーム生成 | `make-string-input-stream`, `make-string-output-stream`, `make-broadcast-stream`, `make-two-way-stream`, `make-echo-stream`, `make-concatenated-stream` | Mandatory |
| FR-706 | ストリーム述語 | `input-stream-p`, `output-stream-p`, `interactive-stream-p`, `open-stream-p`, `stream-element-type` | Mandatory |
| FR-707 | format 完全実装 | 全 format ディレクティブ (`~A`, `~S`, `~D`, `~B`, `~O`, `~X`, `~F`, `~E`, `~G`, `~$`, `~R`, `~/`, `~*`, `~[`, `~<`, `~>`, `~^`, `~{`, `~}` 等) | Mandatory |
| FR-708 | Pretty-printer | `pprint`, `pprint-*` 関数群, `*print-pretty*` | Mandatory |

---

### FR-800: コンディション・リスタートシステム

| ID | 要件 | 対象 | 優先度 |
|---|---|---|---|
| FR-801 | 標準コンディション型 | `simple-error`, `simple-warning`, `type-error`, `program-error`, `control-error`, `stream-error`, `file-error`, `cell-error`, `unbound-variable`, `undefined-function` 等 | Mandatory |
| FR-802 | コンディション生成 | `make-condition`, `signal`, `error` (高度版), `warn` (高度版), `cerror` | Mandatory |
| FR-803 | handler-bind | ハンドラ束縛(巻き戻しなし) | Mandatory |
| FR-804 | リスタート | `restart-case`, `restart-bind`, `invoke-restart`, `find-restart`, `compute-restarts`, `restart-name` | Mandatory |
| FR-805 | 組み込みリスタート | `abort`, `continue`, `muffle-warning`, `use-value`, `store-value` | Mandatory |
| FR-806 | 条件エラーアクセス | `condition-report-string`, コンディションスロットアクセサ | Mandatory |

**技術制約**: FR-803/FR-804 は VM に動的ハンドラスタックと非局所遷移機構の追加が必要。

---

### FR-900: パッケージシステム

| ID | 要件 | 対象 | 優先度 |
|---|---|---|---|
| FR-901 | パッケージ生成 | `make-package`, `find-package`, `delete-package`, `rename-package` | Mandatory |
| FR-902 | シンボル操作 | `find-symbol`, `intern`, `export`, `unexport`, `import`, `shadow`, `shadowing-import` | Mandatory |
| FR-903 | パッケージ使用 | `use-package`, `unuse-package`, `package-use-list`, `package-used-by-list` | Mandatory |
| FR-904 | シンボル列挙 | `do-symbols`, `do-external-symbols`, `do-all-symbols`, `with-package-iterator` | Mandatory |
| FR-905 | パッケージ情報 | `package-name`, `package-nicknames`, `list-all-packages`, `package-shadowing-symbols` | Mandatory |

**技術制約**: 現在 `in-package`/`defpackage` は no-op。フルパッケージシステムの実装は VM のシンボル表現全体の変更を伴う。

---

### FR-1000: CLOS 高度機能

| ID | 要件 | 対象 | 優先度 |
|---|---|---|---|
| FR-1001 | method-combination | `:before`, `:after`, `:around`, `call-next-method`, `next-method-p` | Mandatory |
| FR-1002 | MOP 基本 | `class-of`, `find-class`, `slot-exists-p`, `slot-boundp`, `slot-makunbound` | Mandatory |
| FR-1003 | インスタンス変更 | `change-class`, `update-instance-for-changed-class`, `update-instance-for-different-class` | Mandatory |
| FR-1004 | print-object | `print-object`, `print-unreadable-object`, `describe-object` | Mandatory |
| FR-1005 | クラス再定義 | `ensure-class`, `reinitialize-instance`, `shared-initialize` | Mandatory |

---

### FR-1100: ファイルシステム・パス名

| ID | 要件 | 対象 | 優先度 |
|---|---|---|---|
| FR-1101 | パス名 | `make-pathname`, `pathname-*`, `merge-pathnames`, `enough-namestring`, `namestring` | Mandatory |
| FR-1102 | ファイル操作 | `probe-file`, `truename`, `rename-file`, `delete-file`, `directory`, `file-length`, `file-position` | Mandatory |
| FR-1103 | ファイル I/O | `open` (full options), `close`, `with-open-file` (full options) | Mandatory |

---

### FR-1200: 環境・その他

| ID | 要件 | 対象 | 優先度 |
|---|---|---|---|
| FR-1201 | シンボルプロパティ | `get`, `setf get`, `remprop`, `symbol-plist`, `setf symbol-plist` | Mandatory |
| FR-1202 | シンボル述語 | `boundp`, `fboundp`, `makunbound`, `fmakunbound` | Mandatory |
| FR-1203 | 評価・ロード | `eval`, `compile`, `load`, `compile-file`, `load-compiled-file` | Mandatory |
| FR-1204 | 時刻 | `get-internal-real-time`, `get-internal-run-time`, `get-universal-time`, `decode-universal-time` | Mandatory |
| FR-1205 | 乱数 | `random`, `make-random-state`, `*random-state*` | Mandatory |
| FR-1206 | 環境変数 | `*features*`, `*modules*`, `provide`, `require` | Mandatory |

---

## 非機能要件

| ID | 要件 |
|---|---|
| NFR-001 | ANSI-TEST (~21,000 テスト) を全パス |
| NFR-002 | 既存 1149 テストが引き続き全パス |
| NFR-003 | EXECUTION.md の独自テストフレームワーク移行と並行実装可能 |
| NFR-004 | VM の後方互換性を保ちながら数値塔・パッケージシステムを拡張 |

---

## 技術仕様・設計方針

### アーキテクチャ影響スコープ

```
VM 層の変更が必要な FR:
┌─ FR-308 (Bignum): integer 型表現 → tagged integer/bignum union
├─ FR-102 (progv): 動的変数束縛スタック追加 → vm-state に dynamic-binding-stack
├─ FR-803/804 (handler-bind/restart): dynamic handler stack → vm-state 拡張
└─ FR-900 (Package): シンボル = (package-name . symbol-name) の構造変更

コンパイラ層の変更が必要な FR:
├─ FR-101/103 (locally, load-time-value): compiler-macroexpand-all 拡張
├─ FR-200 系: macro.lisp への our-defmacro 追加
└─ FR-1000 (CLOS): compile-ast (ast-defmethod) の method-combination 対応

VM primitives 層の追加が必要な FR:
├─ FR-300 系: vm-primitives.lisp への新命令追加
├─ FR-400-600 系: 文字列・配列・シーケンス命令追加
└─ FR-700 系: I/O 命令追加
```

---

## 優先実装フェーズ

### Phase 1: 言語コア完成 (特殊演算子 + 高優先マクロ)

対象: FR-101/102/103, FR-201/202/203/204/206/207

- `locally` → `compiler-macroexpand-all` に declare 除去 + progn 変換を追加
- `progv` → VM state に `dynamic-binding-stack` フィールド追加、push/pop 命令追加
- `load-time-value` → `eval-when :compile-toplevel` と同等の事前評価へ変換
- `shiftf`, `psetf` → macro.lisp に `our-defmacro` として追加

リスク: `progv` の VM 変更が中程度

### Phase 2: 数値塔基礎 (Bignum 除く)

対象: FR-301/302/303/304/305

- 各関数を `vm-primitives.lisp` の VM 命令として追加
- `compiler-macroexpand-all` の builtin リストへの追加

リスク: 低〜中 (`ash`/`logand` は VM命令追加のみ)

### Phase 3: 文字列・文字・シーケンス

対象: FR-400, FR-500

- `vm-primitives.lisp` への命令追加が中心

リスク: 低〜中

### Phase 4: コンディション・リスタートシステム

対象: FR-800

- `vm-state` に `handler-stack` / `restart-stack` フィールド追加
- `handler-bind` のコンパイルは `unwind-protect` を内部的に利用

リスク: 高 (VM 根本的例外処理機構の変更)

### Phase 5: Bignum・有理数・複素数

対象: FR-306/307/308

- VM の integer 表現全体の変更。既存 fixnum 最適化パスを保ちながら bignum フォールバックを追加

リスク: 最高

### Phase 6: I/O システム

対象: FR-700

- `*standard-input*`/`*standard-output*` をグローバル変数として VM に追加
- `vm-primitives.lisp` に read/write 命令追加

リスク: 中〜高 (OS syscall との連携)

### Phase 7: 配列・ベクタ完全実装

対象: FR-600

- VM ヒープへの多次元配列表現追加

リスク: 中

### Phase 8: パッケージシステム

対象: FR-900

- シンボル表現の根本変更 (`(pkg . name)` 構造)
- 新規 `src/package-system.lisp` の作成

リスク: 最高 (全コードに影響)

### Phase 9: CLOS 高度機能

対象: FR-1000

- `compile-ast` (ast-defmethod) への method-combination 対応追加

リスク: 中

### Phase 10: ファイルシステム・環境

対象: FR-1100, FR-1200

リスク: 中 (OS syscall バインディング)

---

## タスクブレークダウン

| タスク | 対象 FR | 変更ファイル | リスク |
|---|---|---|---|
| T1: locally / progv / load-time-value | FR-101〜103 | `src/compiler.lisp`, `src/vm.lisp` | 中 |
| T2: 標準マクロ追加 (shiftf, psetf, with-accessors 等) | FR-205〜212 | `src/macro.lisp` | 低 |
| T3: 数値演算拡張 (round, ash, logand, expt 等) | FR-301〜305 | `src/vm-primitives.lisp`, `src/compiler.lisp` | 低〜中 |
| T4: 文字列・文字・シーケンス関数 | FR-401〜507 | `src/vm-primitives.lisp` | 低〜中 |
| T5: コンディションシステム拡張 (handler-bind, restart-case) | FR-801〜806 | `src/vm.lisp`, `src/compiler.lisp` | 高 |
| T6: I/O システム | FR-701〜708 | `src/vm-primitives.lisp`, `src/vm.lisp` | 中〜高 |
| T7: 配列・ベクタ完全実装 | FR-601〜606 | `src/vm-primitives.lisp` | 中 |
| T8: Bignum 実装 | FR-306〜308 | `src/vm.lisp`, `src/vm-primitives.lisp` | 最高 |
| T9: パッケージシステム | FR-901〜905 | 新規 `src/package-system.lisp`, `src/vm.lisp` | 最高 |
| T10: CLOS 高度機能 | FR-1001〜1005 | `src/compiler.lisp` | 中 |
| T11: ファイルシステム・環境 | FR-1101〜1206 | `src/vm-primitives.lisp` | 中 |
| T12: ANSI-TEST 統合 | NFR-001 | `tests/` | 低 |

---

## 設計決定 (解決済み)

| # | 決定事項 | 選択 | 根拠 |
|---|---|---|---|
| D-001 | ANSI-TEST の管理方法 | **コンパイラ内に埋め込み** | zero-dependency 方針と一致、外部ネットワーク不要 |
| D-002 | Bignum の実装方針 | **純粋 VM 実装** | self-hosting 目標に直結; Host 依存を排除 |
| D-003 | ファイル I/O の実装方針 | **Host CL (SBCL) I/O に委譲** | OS syscall の直接実装より安全・移植性が高い |

### D-001 詳細: ANSI-TEST 埋め込み方針

`tests/ansi-test/` ディレクトリを作成し、ANSI-TEST のソースを git submodule または直接コピーで管理する。
実行は **EXECUTION.md の独自フレームワークから呼び出す**。具体的には以下の統合方針を採用する:

```lisp
;; tests/ansi-test-integration.lisp
;; ANSI-TEST の各テストを独自フレームワークの deftest として wrap する

(defsuite ansi-test-suite
  :description "ANSI Common Lisp conformance tests (ANSI-TEST)"
  :parent cl-cc-suite)

(in-suite ansi-test-suite)

;; ANSI-TEST の run-all-tests を独自フレームワーク経由で実行
(deftest ansi-test-full-suite
  "Run ANSI-TEST conformance suite against cl-cc VM"
  (let ((results (ansi-test:run-all-tests :evaluator #'cl-cc:run-string)))
    (assert-true (ansi-test:all-passed-p results))))
```

`make test` で既存テストと同時実行、`make ansi-test` で ANSI-TEST のみ実行できるよう Makefile にターゲットを追加する。

### D-002 詳細: 純粋 VM Bignum 実装方針

```
VM integer 表現の変更:
  Before: fixnum (CL integer, fixed precision)
  After:  tagged union
            :fixnum  → immediate integer (fits in machine word)
            :bignum  → heap-allocated limb array (base 2^32 or 2^64)

変更が必要な VM 命令:
  vm-add, vm-sub, vm-mul, vm-div → bignum overflow 検出 + fallback
  vm-const → bignum literal をヒープに確保
  vm-equal, vm-lt, vm-gt 等 → bignum 比較ロジック追加
```

### D-003 詳細: Host CL I/O 委譲方針

```
VM の I/O 命令は Host CL の対応関数を呼び出す:
  vm-read-char   → (cl:read-char *standard-input*)
  vm-write-char  → (cl:write-char ch *standard-output*)
  vm-open-file   → (cl:open path ...)
  vm-close       → (cl:close stream)

*standard-input* / *standard-output* は VM のグローバル変数として管理し、
with-open-file 等でスタック的に差し替え可能にする。
```

## 未解決事項

なし。すべての設計決定が解決済み。

---

## メトリクス

| 指標 | スコア |
|---|---|
| 要件明確度 | 95 (全設計決定が解決済み) |
| 技術的実現可能性 | 80 (全項目は既存 CL 実装が達成済みの実績あり) |
| ステークホルダー合意 | 100 (全未解決事項が解決済み) |
| **総合スコア** | **92** (success threshold 80 以上) |
