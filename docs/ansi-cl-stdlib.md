# ANSI CL: Standard Library & Extensions

Cons/lists, arrays, strings, sequences, hash tables, filesystem, streams/I/O, printer, reader, system configuration, global variables, declarations, environment/tools, modern extensions.

---
## 13. コンス・リスト (ANSI CL Ch.14)

| 関数 | 状態 |
|------|------|
| `cons` / `car` / `cdr` / `caar`〜`cddddr` | ✅ |
| `first`〜`tenth` / `rest` | ✅ | `sixth`〜`tenth` は macros-stdlib.lisp:975 に実装済み |
| `nth` / `nthcdr` | ✅ |
| `(setf nth)` | ✅ | cons-place-handler in expander.lisp:313 |
| `last` (1引数) | ✅ | |
| `last` (2引数: count) | ✅ | macros-stdlib.lisp |
| `butlast` (1引数) | ✅ | |
| `butlast` (2引数: count) | ✅ | macros-stdlib.lisp |
| `nbutlast` | ✅ | macros-stdlib.lisp |
| `rplaca` / `rplacd` | ✅ | `list.lisp:158/165`, `builtin-registry-data.lisp:313-314` |
| `endp` / `null` / `listp` / `atom` | ✅ |
| `list` / `make-list` | ✅ |
| `list*` | ✅ | macros-stdlib.lisp:965 |
| `copy-list` / `copy-tree` | ✅ |
| `append` / `nconc` | ✅ |
| `nreconc` | ✅ | macros-stdlib.lisp:997 |
| `reverse` / `nreverse` | ✅ |
| `member` (`:key` / `:test` / `:test-not`) | ✅ | macros-stdlib.lisp: `our-defmacro member` with keyword support shadows binary builtin |
| `member-if` / `member-if-not` | ✅ | macros-stdlib.lisp:1087 |
| `assoc` / `rassoc` (`:test`/`:key`/`:test-not`) | ✅ | macros-stdlib.lisp: `our-defmacro assoc/rassoc` with full keyword support |
| `acons` / `pairlis` | ✅ | |
| `assoc-if` / `assoc-if-not` | ✅ | macros-stdlib.lisp:1002 |
| `rassoc-if` / `rassoc-if-not` | ✅ | macros-stdlib.lisp:1013 |
| `list-length` | ✅ | builtin-registry-data.lisp:161 |
| `tailp` / `ldiff` | ✅ | macros-stdlib.lisp |
| `copy-alist` | ✅ | macros-stdlib.lisp |
| `tree-equal` | ✅ | macros-stdlib.lisp |
| `subst` | ✅ | `builtin-registry-data.lisp:403` (:ternary-opt-nil-custom) |
| `subst-if` / `subst-if-not` | ✅ | macros-stdlib.lisp:763 |
| `nsubst` / `nsubst-if` / `nsubst-if-not` | ✅ | macros-stdlib.lisp — delegate to non-destructive subst (functionally correct) |
| `getf` / `remf` | ✅ |
| `get-properties` | ✅ | macros-stdlib.lisp |
| `union` / `intersection` / `set-difference` / `subsetp` / `adjoin` | ✅ |
| `nunion` / `nintersection` / `nset-difference` | ✅ | macros-stdlib.lisp — delegate to non-destructive versions with `:test` keyword |
| `sixth` / `seventh` / `eighth` / `ninth` / `tenth` | ✅ | macros-stdlib.lisp:975 |
| `push` / `pop` | ✅ | FR-693: 複合place でサブフォームを gensym 保護; `(push x (nth i lst))` は `i` を1回のみ評価 |
| `pushnew` | ✅ | macros-stdlib.lisp:983 |

#### FR-587: pushnew

- **対象**: `src/expand/macros-basic.lisp`
- **現状**: コンパイラ内部コード (`cfg.lisp`, `ssa.lisp`) で使用されているがユーザー向けマクロ未登録
- **内容**: `(pushnew item place &key test key)` — `member` で重複チェック後に `push`
- **根拠**: ANSI CL 14.2.3 — pushnew
- **難易度**: Easy

#### FR-563: sixth〜tenth

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `first`〜`fifth` は実装済みだが `sixth`〜`tenth` が不在
- **内容**: 各アクセサを `nth` + 定数オフセットで実装。ビルトイン登録
- **根拠**: ANSI CL 14.2.1
- **難易度**: Easy

---

## 14. 配列 (ANSI CL Ch.15)

| 機能 | 状態 |
|------|------|
| `make-array` (基本形式) | ✅ | |
| `make-array :initial-contents` | ✅ | loop expansion in expander.lisp |
| `make-array :fill-pointer` / `:adjustable` | ✅ | FR-687: `:fill-pointer N` (N≠t) は `(make-adjustable-vector size)` + `(setf (fill-pointer arr) N)` に展開; `:fill-pointer t` は従来通り |
| `make-array :initial-element` | ✅ | loop expansion in expander.lisp |
| `make-array :element-type` | ✅ | expander silently ignores (cl-cc uses T as universal element type; ANSI permits this) |
| `make-array :displaced-to` | 🔶 | FR-687: silently ignored; displaced arrays not supported |
| `aref` (1次元) | ✅ | FR-686: Phase 1 `:binary-custom` 登録; 1インデックス高速パス |
| `(setf aref)` | ✅ | via aset in expander.lisp:296 |
| 多次元配列 (`array-rank` > 1) | ✅ | `vm-aref-multi` 命令 + Phase 2 handler で `(aref arr i j k)` 対応 |
| `array-rank` / `array-dimensions` / `array-dimension` | ✅ |
| `array-total-size` / `array-row-major-index` | ✅ |
| `array-in-bounds-p` | ✅ | macros-stdlib.lisp |
| `array-element-type` | ✅ | macros-stdlib.lisp — returns `T` (correct: cl-cc arrays are untyped) |
| `adjustable-array-p` / `array-has-fill-pointer-p` | ✅ |
| `fill-pointer` / `(setf fill-pointer)` | ✅ |
| `vector-push` / `vector-push-extend` / `vector-pop` | ✅ |
| `array-displacement` | ✅ |
| `adjust-array` | ✅ | `vm-adjust-array` (`list.lisp:754`), ビルトイン登録済み |
| `vectorp` | ✅ | |
| `simple-vector-p` | ✅ | builtin-registry-data.lisp:115 |
| `simple-string-p` / `simple-bit-vector-p` / `bit-vector-p` | ✅ | macros-stdlib.lisp |
| `svref` / `row-major-aref` | ✅ |
| `(setf svref)` | ✅ | via %svset handler in expander.lisp:332 |
| `(setf row-major-aref)` | ✅ | via aset in expander.lisp:337 |
| `vector` (コンストラクタ) | ✅ | expander.lisp:870 |
| 多次元配列 (`(aref arr i j k)`) | ✅ | `vm-aref-multi` + codegen-phase2 AREF handler; N 次元配列アクセス対応 |
| ビット配列操作 (`bit-and` / `bit-ior` / `bit-xor` / `bit-not`) | ✅ | `bit-ior` は ANSI CL 正式名; `bit-or` は cl-cc 別名 |
| `bit-nor` / `bit-nand` / `bit-eqv` / `bit-andc1` / `bit-andc2` / `bit-orc1` / `bit-orc2` | ✅ | macros-stdlib.lisp (progn-wrapped, result-bit-array arg ignored) |
| `(setf bit)` / `(setf sbit)` | ✅ | via rt-bit-set handler in expander.lisp |
| `upgraded-array-element-type` | ✅ | macros-stdlib.lisp — returns `T` (correct: cl-cc uses `T` as universal element type) |

#### FR-564: array-element-type / simple-array 述語群

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `make-array :element-type` が無視される。`array-element-type` 取得不可
- **内容**: `array-element-type array` — 配列の element-type を返す。`simple-string-p` / `simple-bit-vector-p` / `bit-vector-p` 述語追加
- **根拠**: ANSI CL 15.1.2
- **難易度**: Medium

---

## 15. 文字列 (ANSI CL Ch.16)

| 関数 | 状態 |
|------|------|
| `stringp` / `string` (coerce from symbol/char) | ✅ | `builtin-registry-data.lisp:141`, `list.lisp:379` |
| `make-string n` | ✅ | |
| `make-string n :initial-element char` | ✅ | `strings.lisp:406-420` |
| `make-string n :element-type type` | ✅ | element-type accepted and ignored (only character strings supported); codegen-phase2.lisp keyword-scans args so :initial-element still works alongside :element-type |
| `string-upcase` / `string-downcase` / `string-capitalize` | ✅ | FR-627: phase2 handler generates prefix+case(slice)+suffix; `:start`/`:end` supported |
| `nstring-upcase` / `nstring-downcase` / `nstring-capitalize` | ✅ | macros-stdlib.lisp |
| `string-trim` / `string-left-trim` / `string-right-trim` | ✅ |
| `string=` / `string<` / `string>` / `string<=` / `string>=` / `string/=` | ✅ | FR-669: ANSI semantics — `string=`/`string-equal` return T/NIL; `string<` etc. return mismatch index or NIL (`:binary` dispatch) |
| `string-equal` / `string-lessp` 等 (case-insensitive) | ✅ | FR-669: 同上; `:binary` dispatch で ANSI 正確な戻り値 |
| 文字列比較 `:start1`/`:end1`/`:start2`/`:end2` 部分文字列キーワード | ✅ | codegen-phase2.lisp — string-cmp-handler uses vm-subseq for bounds |
| `char` | ✅ | `builtin-registry-data.lisp:297` |
| `(setf char)` | ✅ | FR-614: `expander.lisp` の char/schar setf ハンドラ (`rt-string-set` に展開) |
| `schar` / `(setf schar)` | ✅ | char と同じハンドラで処理 |
| `string-concat` / `concatenate 'string` | ✅ |
| `concatenate 'list` / `concatenate 'vector` (type 指定ディスパッチ) | ✅ | macros-stdlib.lisp — dispatches on list/vector/string result-type |
| `subseq` (文字列) | ✅ |
| `fill` / `replace` (文字列・ベクタ) | ✅ | FR-641: runtime list/vector dispatch; `:start`/`:end` supported; vector path uses aref |

---

## 16. シーケンス (ANSI CL Ch.17)

| 関数 | 状態 |
|------|------|
| `length` / `elt` | ✅ |
| `(setf elt)` | ✅ | `parser.lisp:419` が `(aset seq i val)` に展開; FR-619 は誤記 |
| `copy-seq` / `subseq` | ✅ | `copy-seq` は runtime dispatch `(if (listp s) (copy-list s) (subseq s 0))`; vector/string 対応済み |
| `(setf subseq)` | ✅ | via replace expansion in expander.lisp:317 |
| `fill` / `replace` / `mismatch` | ✅ | FR-641: `fill`/`replace` list/vector dispatch with `:start`/`:end`; `mismatch` supports `:test`/`:key`/`:from-end` |
| `map` / `map-into` | ✅ |
| `reduce` | ✅ |
| `count` / `count-if` | ✅ | `:test/:key/:test-not` keyword args supported; macros-stdlib.lisp |
| `count-if-not` | ✅ | macros-stdlib.lisp:758 |
| `remove` / `remove-if` / `remove-if-not` | ✅ | |
| `remove-duplicates` | ✅ | `:test/:key/:test-not` keyword args supported; macros-stdlib.lisp |
| `delete` / `delete-if` / `delete-if-not` | ✅ | delegates to `remove` with full keyword forwarding (`:test/:key/:test-not`) |
| `delete-duplicates` | ✅ | delegates to `remove-duplicates` with keyword forwarding |
| `substitute` / `substitute-if` / `substitute-if-not` | ✅ | `:test/:key/:test-not` keyword args supported; macros-stdlib.lisp |
| `nsubstitute` / `nsubstitute-if` / `nsubstitute-if-not` | ✅ | delegates to `substitute` with keyword forwarding |
| `find` / `find-if` | ✅ |
| `find-if-not` | ✅ | macros-stdlib.lisp:737 |
| `position` | ✅ | `:test/:key/:test-not` keyword args supported; macros-stdlib.lisp |
| `position-if` | ✅ | macros-stdlib.lisp:741 |
| `position-if-not` | ✅ | macros-stdlib.lisp:754 |
| `search` (文字列) | ✅ | `vm-search-string` (`strings.lisp:211`), `builtin-registry-data.lisp:356` |
| `search` (一般シーケンス) | ✅ | macros-stdlib.lisp |
| `sort` / `stable-sort` (比較述語のみ) | ✅ |
| `sort` / `stable-sort` `:key` 引数 | ✅ | macros-stdlib.lisp |
| `merge` | ✅ |
| `reverse` / `nreverse` | ✅ |
| `concatenate` / `coerce` (シーケンス間変換) | ✅ | `concatenate` dispatches on `'string`/`'list`/`'vector` (macros-stdlib.lisp) |
| `:key` / `:test` / `:test-not` / `:start` / `:end` / `:from-end` キーワード | ✅ 主要関数対応 | find/find-if/find-if-not/position/count/remove/member/assoc/rassoc + -if/-if-not variants all support :key; :start/:end/:from-end in select functions |
| `:count` キーワード (`remove`/`delete`) | ✅ | `remove` supports `:count` and `:from-end` (macros-stdlib.lisp); `delete` delegates to `remove` |

#### FR-588: search (一般シーケンス)

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `vm-search-string` は文字列専用。リスト・ベクタの部分シーケンス検索なし
- **内容**: `(search sequence1 sequence2 &key from-end test test-not start1 end1 start2 end2 key)` — `sequence2` 内で `sequence1` と一致する先頭位置を返す
- **根拠**: ANSI CL 17.2 — search
- **難易度**: Medium

---

## 17. ハッシュテーブル (ANSI CL Ch.18)

| 関数 | 状態 |
|------|------|
| `make-hash-table` | ✅ |
| `gethash` / `(setf gethash)` | ✅ |
| `remhash` | ✅ |
| `clrhash` | ✅ | `vm-clrhash` instruction + `:unary-custom-void` builtin registration (`builtin-registry-data.lisp:373`) |
| `hash-table-count` | ✅ |
| `hash-table-p` | ✅ |
| `hash-table-test` | ✅ |
| `hash-table-size` | ✅ | builtin-registry-data.lisp:278 |
| `hash-table-rehash-size` / `hash-table-rehash-threshold` | ✅ | builtin-registry-data.lisp:278 |
| `maphash` | ✅ | macros-stdlib.lisp:1317-1325 |
| `with-hash-table-iterator` | ✅ | macros-stdlib.lisp — uses hash-table-keys + flet |
| `sxhash` | ✅ | builtin-registry-data.lisp:174 |
| `equalp` ハッシュ (:test #'equalp) | ✅ | FR-565: `resolve-hash-test` で `'equalp` → host CL `#'equalp`; `equalp` 関数は数値/文字/文字列/ベクター/リスト/ハッシュテーブルを再帰比較; `(make-hash-table :test 'equalp)` 対応 |

#### FR-565: equalp ハッシュテーブル

- **対象**: `src/vm/hash.lisp`
- **現状**: `hash.lisp:116-123` の `resolve-hash-test` は `equalp` シンボルをホスト CL `#'equalp` にマップ済み。ただし、コンパイル済みコードから `(make-hash-table :test #'equalp)` と書くと `#'equalp` 参照時に「未定義関数: equalp」エラー — `equalp` 関数自体が FR-582 で ❌。`(make-hash-table :test 'equalp)` (クォート形式) なら動作可能
- **内容**: `equalp` による構造的等価をキーとするハッシュテーブル。配列・構造体の内容比較
- **根拠**: ANSI CL 18.1 — Hash Table Test
- **難易度**: Medium

---

## 18. ファイルシステム (ANSI CL Ch.19-20)

### 18.1 パス名操作

| 機能 | 状態 | 備考 |
|------|------|------|
| `make-pathname` | ✅ | vm.lisp host bridge (`*vm-host-bridge-functions*`) |
| `pathname` / `namestring` / `file-namestring` | ✅ | vm.lisp host bridges |
| `pathname-name` / `pathname-type` / `pathname-host` / `pathname-device` / `pathname-directory` / `pathname-version` | ✅ | vm.lisp host bridges |
| `merge-pathnames` | ✅ | vm.lisp host bridge |
| `truename` | ✅ | vm.lisp host bridge |
| `parse-namestring` | ✅ | vm.lisp host bridge |
| `wild-pathname-p` / `pathname-match-p` / `translate-pathname` | ✅ | vm.lisp host bridges |
| `logical-pathname` / `logical-pathname-translations` | 🔶 | FR-362: not implemented; host-bridge access works but logical pathname system is host-dependent |
| `*default-pathname-defaults*` | ✅ | stdlib-source.lisp defvar stub |
| `compile-file-pathname` | ✅ | added to `*vm-host-bridge-functions*` whitelist in vm.lisp |

#### FR-566: パス名 API 完全化

- **対象**: `src/runtime/runtime.lisp`, `src/vm/io.lisp`
- **現状**: パス名アクセサ群・`truename`・`parse-namestring` が全て未実装
- **内容**: ANSI CL 19.2 の全アクセサ。`truename` (実際のファイルシステムパス解決)。`*default-pathname-defaults*` VM global 初期化
- **根拠**: ANSI CL 19.2 — Pathname Functions
- **難易度**: Medium

### 18.2 ファイル操作

| 機能 | 状態 | 備考 |
|------|------|------|
| `open` | ✅ | FR-628: phase2 handler parses `:direction`/`:if-exists`/`:if-does-not-exist`; `with-open-file` works |
| `close` | ✅ | `builtin-registry-data.lisp:287` |
| `open :direction` (`:input`/`:output`/`:io`/`:probe`) | ✅ | codegen-phase2.lisp OPEN handler parses `:direction` |
| `open :if-exists` (`:error`/`:supersede`/`:append`/`:overwrite`/`:rename`/`nil`) | ✅ | codegen-phase2.lisp: OPEN phase2 handler parses :if-exists |
| `open :if-does-not-exist` (`:error`/`:create`/`nil`) | ✅ | codegen-phase2.lisp: OPEN phase2 handler parses :if-does-not-exist |
| `open :element-type` (バイナリ/文字ストリーム) | 🔶 | 受け付けるが無視 (UTF-8固定) |
| `open :external-format` | 🔶 | 受け付けるが無視 (UTF-8固定) |
| `close :abort` | 🔶 | 受け付けるが無視 (常に正常クローズ) |
| `with-open-file` | ✅ | |
| `probe-file` | ✅ | registered as host bridge in vm.lisp |
| `rename-file` / `delete-file` | ✅ | registered as host bridges in vm.lisp |
| `file-write-date` / `file-author` | ✅ | registered as host bridges in vm.lisp |
| `directory` | ✅ | registered as host bridge in vm.lisp |
| `ensure-directories-exist` | ✅ | registered as host bridge in vm.lisp |
| `load` `:verbose`/`:print`/`:if-does-not-exist`/`:external-format` | ✅ | pipeline.lisp: our-load accepts all 4 keywords; external-format ignored |
| `compile-file` `:output-file`/`:verbose`/`:print`/`:external-format` | 🔶 | stdlib-source.lisp: キーワード引数受け付け・無視; 実際にはロードのみ |
| `compile-file` 3値返却 (output-truename, warnings-p, failure-p) | ✅ | macros-stdlib.lisp: `(values (probe-file pathname) nil nil)` — ANSI-correct 3-value return |

#### FR-589: open/load/compile-file 完全キーワード引数

- **対象**: `src/vm/io.lisp`, `src/compile/pipeline.lisp`
- **現状**: `vm-open-file` がキーワード引数を無視。`:if-exists :append` でファイル追記などが不可
- **内容**: ANSI CL 21.2 の `open` 全キーワード実装。`load` / `compile-file` の制御変数対応。`compile-file` の3値返却
- **根拠**: ANSI CL 21.2.1 / 24.1
- **難易度**: Medium

---

## 19. ストリーム・I/O (ANSI CL Ch.21)

### 19.1 ストリーム生成・管理

| 機能 | 状態 |
|------|------|
| `open` | ✅ | phase2 handler; `:direction`/`:if-exists`/`:if-does-not-exist` parsed |
| `close` | ✅ |
| `with-open-file` | ✅ |
| `with-open-stream` | ✅ (`macros-stdlib.lisp:47`) |
| `make-string-input-stream` | ✅ | registered via phase2 handler |
| `make-string-output-stream` | ✅ | `builtin-registry-data.lisp:277` |
| `with-input-from-string` (基本形式) | ✅ | |
| `with-input-from-string` `:start`/`:end`/`:index` キーワード | ✅ | macros-stdlib.lisp:507-522 parses :start/:end/:index from binding |
| `with-output-to-string` (基本形式) | ✅ | |
| `with-output-to-string` オプション文字列引数 (`(var string &optional stype)`) | 🔶 | macros-stdlib.lisp: 引数受け付けるが無視、常に新規ストリーム作成 |
| `get-output-stream-string` | ✅ |
| `make-synonym-stream` | ✅ | vm.lisp host bridge |
| `make-broadcast-stream` / `make-two-way-stream` / `make-echo-stream` | ✅ | vm.lisp host bridges |
| `make-concatenated-stream` | ✅ | vm.lisp host bridge |
| 複合ストリームアクセサ (`broadcast-stream-streams` 等) | ✅ | vm.lisp host bridges — broadcast-stream-streams, two-way-stream-input/output-stream, etc. |
| Gray Streams プロトコル | ❌ FR-388 |
| `with-standard-io-syntax` | ✅ | FR-632: binds all ANSI-standard I/O vars (`*print-escape*`, `*print-base*`, `*read-base*`, `*package*` etc.) |

#### FR-567: 複合ストリームアクセサ

- **対象**: `src/vm/io.lisp`
- **現状**: `make-broadcast-stream` 等の VM 命令なし (FR-393)。アクセサ `broadcast-stream-streams` / `two-way-stream-input-stream` / `two-way-stream-output-stream` / `echo-stream-input-stream` / `echo-stream-output-stream` / `concatenated-stream-streams` が不在
- **内容**: 各複合ストリームの構成ストリームを取得するアクセサ群
- **根拠**: ANSI CL 21.1
- **難易度**: Low (FR-393 後)

### 19.2 文字・バイト I/O

| 関数 | 状態 |
|------|------|
| `read-char` (基本) | ✅ | |
| `unread-char` | ✅ | FR-671: 2-arg via Phase 1 (binary-void); 1-arg (default `*standard-input*`) via Phase 2 handler |
| `peek-char` | ✅ | builtin-registry-data.lisp:423 |
| `read-char` eof-error-p / eof-value 引数 | ✅ | FR-612: 3引数以上で eof-value 代入実装; `%emit-eof-value-check` ヘルパーで `:eof` センチネル → eof-value 変換 |
| `read` eof-error-p / eof-value 引数 | ✅ | FR-612: 同上; `vm-read-sexp-inst` + `%emit-eof-value-check` |
| `read-char-no-hang` | 🔶 | macros-stdlib.lisp — delegates to read-char (no actual non-blocking) |
| `read-sequence` | ✅ | macros-stdlib.lisp |
| `write-sequence` | ✅ | macros-stdlib.lisp |
| `write-byte` | ✅ | |
| `read-byte` | ✅ | FR-672b: Phase 1 で 1引数高速パス; Phase 2 で 3引数以上の eof-value 代入対応 |
| `read-line` eof-error-p / eof-value 引数 | ✅ | FR-612: 3引数以上で eof-value 代入実装; 第2戻り値 `missing-newline-p` は常に `nil` (許容制限) |
| `write-char` / `write-string` / `write-line` | ✅ |
| `terpri` / `fresh-line` optional stream | ✅ | implemented via phase2 handlers |
| `finish-output` / `force-output` / `clear-input` | ✅ |
| `clear-output` | ✅ | builtin-registry-data.lisp:431 |
| `listen` | ✅ | FR-674: 1-arg via Phase 1 (handle-input); 0-arg (default `*standard-input*`) via Phase 2 handler |
| `file-position` / `file-length` | ✅ |

#### FR-590: read-sequence / write-sequence

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: バッファ単位の I/O が `read-line` / `write-string` のみ。`read-sequence` / `write-sequence` が全コードベースに不在 (cli/main.lisp は ホスト CL の `read-sequence` を直接使用)
- **内容**: `(read-sequence seq stream &key start end)` — `stream` からシーケンスに読み込む。`(write-sequence seq stream &key start end)` — 逆方向。バイナリ・文字ストリーム両対応
- **根拠**: ANSI CL 21.2 — read-sequence, write-sequence; バイナリファイル処理に必須
- **難易度**: Medium

#### FR-568: read-char-no-hang

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: ノンブロッキング文字読み取りが不在
- **内容**: `read-char-no-hang` — 文字が利用可能なら返却、なければ `nil` を返す。`listen` との連携
- **根拠**: ANSI CL 21.2 — read-char-no-hang
- **難易度**: Low

### 19.3 ストリーム述語・アクセサ

| 関数 | 状態 |
|------|------|
| `streamp` / `input-stream-p` / `output-stream-p` | ✅ |
| `open-stream-p` / `interactive-stream-p` | ✅ |
| `stream-element-type` | ✅ |
| `stream-external-format` | 🔶 | macros-stdlib.lisp — stub returning :default |
| `file-string-length` | ✅ | macros-stdlib.lisp — UTF-8 バイト数を `string-to-octets` で計算; 文字は `(string char)` に変換してから計算 |

#### FR-591: file-string-length

- **対象**: `src/vm/io.lisp`
- **現状**: ファイルに文字列/文字を書き込んだ場合のバイト数を調べる手段なし
- **内容**: `(file-string-length stream object)` — `object` (文字または文字列) を `stream` に書いた場合のファイルポジション移動量を返す
- **根拠**: ANSI CL 21.2 — file-string-length
- **難易度**: Easy

### 19.4 標準ストリーム変数

| 変数 | 状態 |
|------|------|
| `*standard-input*` / `*standard-output*` / `*error-output*` | ✅ |
| `*debug-io*` / `*trace-output*` / `*query-io*` | ✅ |
| `*terminal-io*` | ✅ |

---

## 20. プリンタ (ANSI CL Ch.22)

### 20.1 出力関数

| 機能 | 状態 |
|------|------|
| `print` / `prin1` / `princ` | ✅ | optional stream — implemented via phase2 handlers |
| `write` (全キーワード引数形式) | ✅ | macros-stdlib.lisp — `:stream`/`:base`/`:radix`/`:escape`/`:level`/`:length` を print 変数にバインドして `write-to-string` 呼出; `:pretty`/`:circle`/`:pprint-dispatch` 等は無視 |
| `pprint` | 🔶 | macros-stdlib.lisp — delegates to print |
| `print-object` GF ディスパッチ | 🔶 | FR-390: マクロ部分実装あり |
| `print-unreadable-object` | ✅ |
| `write-to-string` (1引数) | ✅ | |
| `write-to-string` キーワード引数 (`:base`, `:radix`, `:escape` 等) | ✅ | FR-646: VM グローバルから `*print-base*`/`*print-radix*`/`*print-escape*`/`*print-level*`/`*print-length*`/`*print-circle*`/`*print-case*` を読み取り CL 動的バインドに変換してから `write-to-string` 呼出 |
| `prin1-to-string` / `princ-to-string` | ✅ | FR-481: `prin1-to-string` → `vm-write-to-string-inst` (with escaping); `princ-to-string` → `vm-princ-to-string-inst` (no escaping) |

#### FR-569: write 関数 (全キーワード形式)

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: `write-to-string` はあるがキーワード引数付き `write` 関数なし
- **内容**: `(write object :stream s :escape t :radix nil :base 10 :circle nil :pretty nil :level nil :length nil :case :upcase :gensym t :array t :readably nil :right-margin nil :miser-width nil :lines nil :pprint-dispatch ...)` の完全実装
- **根拠**: ANSI CL 22.3.1
- **難易度**: Hard (pprint 実装後)

### 20.2 印字制御変数

| 変数 | 状態 |
|------|------|
| `*print-escape*` | ✅ | FR-646: `vm-write-to-string-inst` が VM グローバルから読み取り CL specials にバインド |
| `*print-radix*` / `*print-base*` | ✅ | FR-646: 同上; `(setq *print-base* 16)` が `write-to-string` 出力に反映 |
| `*print-level*` / `*print-length*` | ✅ | FR-646: 同上 |
| `*print-circle*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-pretty*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-gensym*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-array*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-readably*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-right-margin*` / `*print-miser-width*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-lines*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-pprint-dispatch*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-case*` | ✅ | stdlib-source.lisp defvar stubs |

#### FR-570: *print-circle* / *print-gensym* / *print-case*

- **対象**: `src/vm/vm.lisp`, `src/vm/io.lisp`
- **現状**: これら3変数が VM global 未初期化
- **内容**: `*print-circle*` — 循環構造検出・`#n=`/`#n#` 表記。`*print-gensym*` — `#:` プレフィックス制御。`*print-case*` — `:upcase`/`:downcase`/`:capitalize`/`:preserve` シンボル大文字小文字制御
- **根拠**: ANSI CL 22.1 — print control variables
- **難易度**: Medium

### 20.3 FORMAT 指示子

| 指示子 | 状態 | 備考 |
|--------|------|------|
| `~A` / `~S` / `~W` | ✅ | `format.lisp:123` でホスト `format` に委譲; `~W` も含め全て動作 |
| `~D` / `~B` / `~O` / `~X` | ✅ | |
| `~R` (基数/英語/序数) | ✅ | ホスト CL 委譲 |
| `~F` / `~E` / `~G` / `~$` (浮動小数点) | ✅ | ホスト CL 委譲 (FR-571 は自前実装追跡用) |
| `~%` / `~&` / `~|` / `~~` | ✅ | |
| `~T` (タブ) | ✅ | |
| `~P` (複数形) | ✅ | ホスト CL 委譲 |
| `~C` (文字) | ✅ | |
| `~[...~]` (条件) | ✅ | ホスト CL 委譲 |
| `~{...~}` (反復) | ✅ | ホスト CL 委譲 |
| `~<...~>` (正当化) | ✅ | ホスト CL 委譲 |
| `~*` (引数移動) | ✅ | ホスト CL 委譲 |
| `~?` / `~@?` (再帰フォーマット) | ✅ | ホスト CL 委譲 |
| `~/fn/` (ユーザー関数) | 🔶 | FR-694: ホスト CL 名前空間の関数のみ; VM クロージャ (ユーザー定義 `defun`) は `~/fn/` から呼び出せない |
| `~I` / `~:T` / `~_` (pprint 系) | ✅ | ホスト CL 委譲 |
| `(format nil "~A" x)` → string 返却 | ✅ | `format.lisp:123` `(apply #'format nil fmt-str arg-vals)` |
| `format` 自前実装 (ホスト非依存) | ❌ | FR-389: 自己ホスト化後に必要になる |
| `formatter` マクロ | ✅ | macros-stdlib.lisp — lambda calling format |

#### FR-571: format 浮動小数点指示子 (~F / ~E / ~G / ~$)

- **対象**: 新規 `src/vm/format.lisp`
- **現状**: 浮動小数点の書式化は全てホスト `format` に委譲
- **内容**: `~F` (固定小数点)、`~E` (指数表記)、`~G` (`~E`/`~F` 自動選択)、`~$` (通貨形式) を自前実装
- **根拠**: ANSI CL 22.3.2 — Floating-Point Directives
- **難易度**: Hard (FR-389 の一部)

---

## 21. リーダー (ANSI CL Ch.23)

| 機能 | 状態 |
|------|------|
| `read` | ✅ | `builtin-registry-data.lisp:161` |
| `read-preserving-whitespace` | ✅ | macros-stdlib.lisp — delegates to read |
| `read-from-string` (1値: sexp) | ✅ |
| `read-from-string` 2値目 (end-position) | ✅ | format.lisp — execute-instruction sets vm-values-list with (value end-pos) |
| `read-delimited-list` | ✅ | vm.lisp host bridge — delegates to host CL `read-delimited-list` |
| リーダーマクロ `#:` `#+` `#-` `#.` | ✅ |
| `#+` / `#-` 単純フィーチャーキーワード | ✅ | |
| `#+` / `#-` 複合フィーチャー式 (`#+(and x y)` / `#+(or x y)` / `#+(not x)`) | ✅ | lex-read-feature-expr handles :and/:or/:not |
| リーダーマクロ `#'` `#(` `#\` `#|...|#` | ✅ |
| `#b` / `#o` / `#x` (基数) | ✅ | `lexer.lisp:565-585` にネイティブ実装 |
| `#nR` (任意基数: `#12R...`) | ✅ | lexer-dispatch.lisp |
| `#C(real imag)` 複素数 | ✅ | lexer-dispatch.lisp |
| `#n=` / `#n#` (label/reference) | 🔶 | FR-599: non-circular sharing only (`*lexer-label-table*`); circular structures not supported |
| `#nA` (多次元配列) | ✅ | FR-572: delegates to host `read-from-string`; lexer-dispatch.lisp |
| `#*` (ビットベクタ) | ✅ | lexer-dispatch.lisp |
| `#S(...)` (構造体) | 🔶 | FR-556: ホスト CL `read-from-string` に委譲 (`lexer-dispatch.lisp`); CL 構造体オブジェクトを返すがコンパイラの CLOS ハッシュテーブル形式とは異なる |
| `#P"..."` (パス名リテラル) | ✅ | lexer-dispatch.lisp: #P dispatch reads string → pathname |
| `` ` `` / `,` / `,@` (バッククォート) | ✅ |
| Readtable API (`set-macro-character` 等) | 🔶 | stdlib-source.lisp: 関数定義あり、ノーオペレーションスタブ |
| `set-dispatch-macro-character` | 🔶 | stdlib-source.lisp: スタブ、常に t 返却 |
| `copy-readtable` / `readtablep` / `readtable-case` | 🔶 | stdlib-source.lisp: スタブ実装 |
| `*readtable*` 変数 | 🔶 | stdlib-source.lisp: defvar stub (nil) |
| `set-syntax-from-char` | 🔶 | stdlib-source.lisp: スタブ、常に t 返却 |
| `make-dispatch-macro-character` | 🔶 | stdlib-source.lisp: スタブ、常に t 返却 |
| `get-macro-character` / `get-dispatch-macro-character` | 🔶 | stdlib-source.lisp: スタブ、常に nil 返却 |
| `*read-base*` / `*read-default-float-format*` / `*read-suppress*` | ✅ | stdlib-source.lisp defvar stubs |
| `*read-eval*` | ✅ | stdlib-source.lisp defvar stub |

#### FR-592: set-syntax-from-char / make-dispatch-macro-character

- **対象**: `src/parse/cl/lexer.lisp` (FR-358 の一部)
- **現状**: Readtable API が一切なく、既存文字構文のコピーも新ディスパッチ文字作成も不可
- **内容**: `set-syntax-from-char to-char from-char &optional to-readtable from-readtable` — 構文クラスをコピー。`make-dispatch-macro-character char &optional non-terminating-p readtable` — 新しいディスパッチマクロ文字を登録
- **根拠**: ANSI CL 23.1.2 — Readtable API
- **難易度**: Medium (FR-358 後)

#### FR-572: 追加リーダーマクロ (#nA / #* / #P)

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#P` は実装済み (lexer-dispatch.lisp + parser.lisp `lower-sexp-to-ast pathname`)。`#nA` / `#*` は未実装
- **内容**: `#nA(...)` — 多次元配列リテラル (❌)。`#*01101` — ビットベクタリテラル (❌)。`#P"..."` — パス名リテラル (✅)
- **根拠**: ANSI CL 2.4.8 — Standard Dispatching Macro Characters
- **難易度**: Medium (#nA / #* のみ残)

#### FR-573: *read-eval*

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#.` が常に評価。`*read-eval*` 変数なし
- **内容**: `*read-eval*` が `nil` のとき `#.` を `print-not-readable` エラーにする
- **根拠**: ANSI CL 2.4.8.6 / セキュリティ
- **難易度**: Easy

---

## 22. システム構成・ロード (ANSI CL Ch.24)

| 機能 | 状態 |
|------|------|
| `provide` | ✅ | macros-stdlib.lisp: `(provide module)` adds string to `*modules*` if not present |
| `require` | ✅ | FR-680: `pathnames` 指定時は `(dolist (p pathnames) (our-load p))` 実行; 未指定時は `warn` |
| `*modules*` / `*features*` | ✅ | FR-642 resolved: both vm-state and vm2-state initialize `*features*` to `(:common-lisp :cl-cc)` |
| `with-compilation-unit` | ✅ | macros-stdlib.lisp — (progn body...) |
| `*load-pathname*` / `*load-truename*` | ✅ | stdlib-source.lisp defvar stubs |
| `*load-verbose*` / `*load-print*` | ✅ | stdlib-source.lisp defvar stubs |
| `*compile-file-pathname*` / `*compile-file-truename*` | ✅ | stdlib-source.lisp defvar stubs |
| `*compile-verbose*` / `*compile-print*` | ✅ | stdlib-source.lisp defvar stubs |
| ASDF 統合 | ✅ (`cl-cc.asd`) |

#### FR-574: ロード・コンパイル制御変数

- **対象**: `src/compile/pipeline.lisp`, `src/vm/vm.lisp`
- **現状**: `our-load` 実行中に `*load-pathname*` 等が設定されない
- **内容**: `load` 実行中に `*load-pathname*` / `*load-truename*` を動的束縛。`compile-file` 中に `*compile-file-pathname*` 等を設定。`*load-verbose*` / `*load-print*` の参照
- **根拠**: ANSI CL 23.1.1 / 24.1
- **難易度**: Easy

---

## 23. 標準 CL グローバル変数

| 変数 | 状態 | 備考 |
|------|------|------|
| `*package*` | ✅ | vm.lisp: initialized to `(find-package :cl-user)` in `*vm-initial-globals*` |
| `*standard-input*` / `*standard-output*` / `*error-output*` | ✅ | |
| `*debug-io*` / `*trace-output*` / `*query-io*` / `*terminal-io*` | ✅ | |
| `*read-base*` / `*read-default-float-format*` / `*read-suppress*` | ✅ | stdlib-source.lisp defvar stubs |
| `*read-eval*` | ✅ | stdlib-source.lisp defvar stub |
| `*readtable*` | ✅ | stdlib-source.lisp defvar stub (no readtable operations) |
| `*print-escape*` / `*print-base*` / `*print-radix*` | ✅ | FR-646: `write-to-string` で VM グローバルから読み取り CL specials にバインドして呼出 |
| `*print-level*` / `*print-length*` | ✅ | FR-646: 同上 |
| `*print-circle*` / `*print-gensym*` / `*print-case*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-array*` / `*print-readably*` | ✅ | stdlib-source.lisp defvar stubs |
| `*print-pretty*` / `*print-right-margin*` | ✅ | stdlib-source.lisp defvar stubs |
| `*random-state*` | ✅ | |
| `*gensym-counter*` | ✅ | FR-510: `stdlib-source.lisp` で `(defvar *gensym-counter* 0)` として定義 |
| `*default-pathname-defaults*` | ✅ | stdlib-source.lisp defvar stub |
| `*load-pathname*` / `*load-truename*` | ✅ | stdlib-source.lisp defvar stubs |
| `*compile-file-pathname*` / `*compile-file-truename*` | ✅ | stdlib-source.lisp defvar stubs |
| `*load-verbose*` / `*load-print*` | ✅ | stdlib-source.lisp defvar stubs |
| `*compile-verbose*` / `*compile-print*` | ✅ | stdlib-source.lisp defvar stubs |
| `*break-on-signals*` | ✅ | stdlib-source.lisp defvar stubs |
| `*debugger-hook*` | ✅ | stdlib-source.lisp defvar stubs |
| `*macroexpand-hook*` | ✅ | stdlib-source.lisp defvar stubs |
| `*modules*` / `*features*` | ✅ | FR-642 resolved: both vm-state and vm2-state initialize `*features*` to `(:common-lisp :cl-cc)` | |

---

## 24. 宣言・コンパイラポリシー (ANSI CL Ch.3.3)

| 機能 | 状態 |
|------|------|
| `declare` (基本型宣言) | ✅ |
| `declaim` | ✅ | FR-396: macros-compat.lisp no-op macro; ANSI準拠 (全宣言指定子の無視は合法) |
| `proclaim` | ✅ | macros-stdlib.lisp (stub macro) |
| `declare (optimize ...)` | ✅ | 黙って無視 — ANSI CL §3.3.4: 実装は任意の宣言指定子を無視できる |
| `declare (type ...)` | 🔶 | 型推論パスには影響あり |
| `declare (ignore ...)` / `(ignorable ...)` | ✅ | 黙って無視 — 未使用変数警告は ANSI CL では実装任意 |
| `declare (inline ...)` / `(notinline ...)` | ✅ | 黙って無視 — インライン展開はコンパイラヒントのみ (ANSI §3.3.6) |
| `declare (ftype ...)` | 🔶 FR-127 (部分) |
| `declare (dynamic-extent ...)` | ✅ | 黙って無視 — 最適化ヒントのみ; escape analysis なし (ANSI §3.3.2) |
| `declare (special ...)` | 🔶 | 動的束縛未対応; 静かに無視 |
| `*compiler-policy*` | ✅ | stdlib-source.lisp defvar stub |
| 環境オブジェクト (`variable-information` 等, CLtL2 8.5) | ❌ FR-394 |
| `locally` 宣言伝播 | ✅ | macros-stdlib.lisp — strips leading declare, wraps in progn |
| `compiler-let` | ✅ | macros-stdlib.lisp — acts as let at runtime |

#### FR-575: dynamic-extent 宣言

- **対象**: `src/compile/codegen.lisp`, `src/vm/vm.lisp`
- **現状**: `dynamic-extent` 宣言が無視される。クロージャ・リスト・ベクタのスタック割り当て最適化なし
- **内容**: `(declare (dynamic-extent var))` — スコープを抜けた時点で回収可能とマーク。クロージャのスタック割り当て (escape analysis と連携)
- **根拠**: ANSI CL 3.3.4 — dynamic-extent; GC 圧力削減
- **難易度**: Very Hard

---

## 25. 環境・開発ツール (ANSI CL Ch.25)

### 25.1 デバッグ・プロファイリング

| 機能 | 状態 |
|------|------|
| `sleep` | ✅ | builtin-registry-data.lisp:133 |
| `time` | ✅ | macros-stdlib.lisp — prints elapsed time using get-universal-time |
| `room` | ✅ | macros-stdlib.lisp — delegates to host SBCL `room` |
| `trace` / `untrace` | ✅ | macros-stdlib.lisp — delegates to host SBCL `trace`/`untrace` |
| `step` | ✅ | macros-stdlib.lisp — delegates to host SBCL `step` |
| `break` | ✅ | macros-stdlib.lisp — prints message |
| `invoke-debugger` / `*debugger-hook*` | ✅ | macros-stdlib.lisp — signals error; *debugger-hook* stub in stdlib-source.lisp |
| `*break-on-signals*` | ✅ | stdlib-source.lisp defvar stubs |
| `disassemble` | ✅ | macros-stdlib.lisp — delegates to host SBCL `disassemble` |
| `dribble` | ✅ | macros-stdlib.lisp — delegates to host SBCL `dribble` |

#### FR-576: disassemble

- **対象**: `src/cli/main.lisp`, `src/compile/pipeline.lisp`
- **現状**: コンパイラが VM 命令列を生成するが、ユーザーが関数の VM 命令を確認する手段なし
- **内容**: `(disassemble fn)` — VM 命令列を人間可読形式で出力。デバッグ・最適化確認に不可欠
- **根拠**: ANSI CL 25.1.4 — disassemble
- **難易度**: Medium

### 25.2 インスペクション・ドキュメント

| 機能 | 状態 |
|------|------|
| `describe` / `describe-object` | ✅ | macros-compat.lisp: CLOS オブジェクトならクラス名+スロット一覧を表示; 非 CLOS なら `prin1` |
| `inspect` | ✅ | macros-stdlib.lisp — delegates to host SBCL `inspect` |
| `documentation` / `(setf documentation)` | ✅ | expander.lisp registers defun docstrings in *documentation-table* at compile time; %get-documentation host bridge reads at runtime. (setf documentation) not implemented. |
| `apropos` / `apropos-list` | ✅ | vm.lisp host bridges |
| `ed` | 🔶 | macros-stdlib.lisp — stub (no-op) |

#### FR-577: inspect

- **対象**: `src/cli/main.lisp`
- **現状**: `inspect` が全コードベースに不在
- **内容**: `(inspect object)` — オブジェクトの内部構造を再帰的に閲覧するインタラクティブツール。スロット・コンポーネント表示と編集
- **根拠**: ANSI CL 25.1.4
- **難易度**: Medium

### 25.3 ユーザーインタラクション

| 機能 | 状態 |
|------|------|
| `y-or-n-p` / `yes-or-no-p` | ✅ | FR-578: `macros-stdlib.lisp` でマクロ定義 (read-line + 文字比較) |

#### FR-578: y-or-n-p / yes-or-no-p

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry.lisp`
- **現状**: 両関数が不在
- **内容**: `y-or-n-p` — "y" または "n" を受け付ける。`yes-or-no-p` — "yes" / "no" フルスペル要求。`*query-io*` ストリームを使用
- **根拠**: ANSI CL 25.1.2
- **難易度**: Easy

### 25.4 環境照会

| 機能 | 状態 |
|------|------|
| `lisp-implementation-type` / `lisp-implementation-version` | ✅ | macros-stdlib.lisp (stub values) |
| `machine-type` / `machine-version` / `machine-instance` | ✅ | macros-stdlib.lisp (stub values) |
| `software-type` / `software-version` | ✅ | macros-stdlib.lisp (stub values) |
| `short-site-name` / `long-site-name` | ✅ | macros-stdlib.lisp (stub values) |
| `compiled-function-p` | ✅ | macros-stdlib.lisp |

---

## 26. モダン拡張 (2026年コンパイラ標準)

ANSI CL 外だが 2026 年のモダンな CL 実装が提供する機能。

### 26.1 継続・コルーチン

| 機能 | 状態 |
|------|------|
| Tail Call Optimization (TCO) | ✅ CPS変換で実現 |
| Delimited Continuations (`shift`/`reset`) | ❌ FR-221 |
| Coroutines / Generators (`yield`/`resume`) | ❌ FR-222 |

### 26.2 Unicode・文字エンコーディング

| 機能 | 状態 |
|------|------|
| Unicode 文字 (U+0000〜U+10FFFF) | ✅ | FR-562: lexer falls back to cl:name-char for Unicode character names; code-char/char-code support full range via SBCL |
| UTF-8 外部フォーマット指定 (`open :external-format :utf-8`) | 🔶 | open/load で :external-format 受け付けるが無視 (UTF-8固定) |
| `string-to-octets` / `octets-to-string` (Babel 相当) | ✅ | vm.lisp: SBCL sb-ext ラッパー; :encoding キーワード対応 |

#### FR-579: バイト列・文字列変換

- **対象**: `src/vm/strings.lisp`
- **現状**: 文字列のバイト列変換手段なし
- **内容**: `string-to-octets string &key encoding` / `octets-to-string octets &key encoding` の非 ANSI 標準だが事実上必須な関数
- **根拠**: 2026 年モダン CL — Babel ライブラリ相当
- **難易度**: Medium

### 26.3 並行処理

詳細は [concurrency.md](concurrency.md) 参照。

| 機能 | 状態 |
|------|------|
| Bordeaux Threads 互換 API | ❌ |
| `bt:make-thread` / `bt:join-thread` | ❌ |
| `bt:make-lock` / `bt:with-lock-held` | ❌ |
| `bt:make-condition-variable` | ❌ |
| Atomic operations | ❌ |

### 26.4 FFI (Foreign Function Interface)

| 機能 | 状態 |
|------|------|
| CFFI 互換 API | ❌ FR-580 |
| `cffi:define-foreign-library` / `cffi:load-foreign-library` | ❌ FR-580 |
| `cffi:defcfun` / `cffi:defcstruct` | ❌ FR-580 |
| `cffi:with-foreign-objects` / `cffi:mem-ref` | ❌ FR-580 |

#### FR-580: FFI / CFFI 互換層

- **対象**: 新規 `src/ffi/`
- **現状**: 外部 C ライブラリ呼び出し手段なし
- **内容**: CFFI に準拠したFFI API。`dlopen`/`dlsym` によるシンボル解決。C 型と CL 型の変換レイヤー
- **根拠**: 2026 年モダン CL — 事実上標準の FFI API
- **難易度**: Very Hard

### 26.5 オブジェクトシステム

| 機能 | 状態 |
|------|------|
| Gray Streams | ❌ FR-388 |
| MOP (Metaobject Protocol) | ❌ FR-523〜528 (clos.md) |
| `defstruct` ↔ CLOS 統合 (`structure-class`) | ❌ FR-528 (clos.md) |

### 26.6 コンパイラ品質

| 機能 | 状態 |
|------|------|
| NaN-boxing タグチェック融合 | ❌ FR-364 |
| Compiler Environment Objects (CLtL2 Ch.8.5) | ❌ FR-394 |
| `define-compiler-macro` 完全動作 | ❌ FR-365 |
| Inline expansion | ❌ FR-396 |
| `dynamic-extent` による escape analysis | ❌ FR-575 |

---

---

## 27. 追加 FR エントリ (FR-593〜FR-602)

#### FR-593: (setf subseq)

- **対象**: `src/vm/list.lisp`, `src/expand/expander.lisp`
- **現状**: `subseq` は読み取り専用。`(setf (subseq s 0 3) "abc")` が失敗
- **内容**: `(setf (subseq seq start end) new-seq)` — `replace` で実装可。`expander.lisp` の setf ハンドラに追加
- **根拠**: ANSI CL 17.3 — setf of subseq
- **難易度**: Easy

#### FR-594: #+/#- 複合フィーチャー式

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#+:sbcl` のような単純フィーチャーは動作するが `#+(and :sbcl :x86-64)` / `#+(or :sbcl :ccl)` / `#+(not :sbcl)` が未対応
- **内容**: フィーチャー式の再帰パーサを追加。`and`/`or`/`not` による合成フィーチャー条件を `*features*` と照合
- **根拠**: ANSI CL 24.1.2 — Feature Expressions
- **難易度**: Low

#### FR-595: (setf subseq) — 既 FR-593 に統合

#### FR-596: last / butlast / nbutlast の count 引数

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `last` / `butlast` はどちらも unary 登録 (`builtin-registry-data.lisp:35,40`)。count 引数形式なし。`nbutlast` は全コードベースに不在
- **内容**: `(last list n)` → 末尾 n 個の cons cell を返す。`(butlast list n)` → 末尾 n 個を除いたリスト。`nbutlast` → 破壊的版
- **根拠**: ANSI CL 14.2.6
- **難易度**: Easy

#### FR-597: identity / constantly / complement ビルトイン登録

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `pipeline.lisp:162-164` のプリルードで `defun` として定義済み。しかしビルトイン登録がないため `(identity x)` が `vm-call` 経由になりインライン化されない
- **内容**: ビルトイン登録 + `(identity x)` → noop コード生成。`(complement pred)` → `(lambda (x) (not (funcall pred x)))`
- **根拠**: ANSI CL 5.1 — function utilities
- **難易度**: Low

#### FR-598: ストリーム型指定子 (typep 対応) — ✅ COMPLETE

- **対象**: `src/vm/primitives.lisp` の `vm-typep-check`
- **実装**: `stream`/`input-stream`/`output-stream`/`file-stream`/`string-stream`/`broadcast-stream`/`two-way-stream`/`echo-stream`/`concatenated-stream`/`synonym-stream` すべて `vm-typep-check` lines 85-95 に実装済み
- **根拠**: ANSI CL 21.1 — Stream Types

#### FR-599: #n= / #n# ラベル読み取り構文 (循環構造)

- **対象**: `src/parse/cl/lexer.lisp`
- **現状**: `#n=` / `#n#` が未実装。`*print-circle*` (FR-570) と対になる機能
- **内容**: `#1=(a #1#)` のような循環構造のリード/プリント。`*read-eval*` と `*print-circle*` の両実装後に必要
- **根拠**: ANSI CL 2.4.8.15/16
- **難易度**: Hard

#### FR-600: defvar / defparameter / defconstant 完全セマンティクス — ✅ COMPLETE

- **対象**: `src/compile/codegen-functions.lisp`
- **実装**: `compile-ast ast-defvar` (lines 326-371) に `vm-boundp` チェック実装済み。`defparameter` — 常に設定; `defvar` — 未束縛時のみ設定 (`vm-jump-zero + vm-boundp`); `defconstant` — `defparameter` に変換 (不変性強制なし、許容制限)
- **根拠**: ANSI CL 3.8.1

#### FR-601: multiple-value-bind 値数不足時の挙動 — ✅ COMPLETE

- **対象**: `src/vm/vm-execute.lisp`
- **実装**: `execute-instruction vm-mv-bind` (lines 527-536) — `(if (< i (length vals)) (nth i vals) nil)` で不足値を `nil` で補填; 余剰値は捨てる
- **根拠**: ANSI CL 5.3 — multiple-value-bind semantics

#### FR-602: (values) 0値返却の明示的サポート — ✅ COMPLETE

- **対象**: `src/vm/vm-execute.lisp`
- **実装**: `execute-instruction vm-values` (line 524) — `(if all-values (first all-values) nil)` で 0値時は `nil` を返す; `vm-values-list` に空リストをセット。`vm-mv-bind` は空リストで全変数を `nil` に補填
- **根拠**: ANSI CL 5.3 — values with no arguments

#### FR-603: (setf (values a b ...) expr) — values を場所として使用 — ✅ COMPLETE

- **対象**: `src/expand/expander.lisp` (`*setf-compound-place-handlers*`)
- **実装**: `(setf (gethash 'values *setf-compound-place-handlers*) ...)` lines 407-415 — `multiple-value-bind` + `setq` チェーンに展開
- **根拠**: ANSI CL 5.1.2.3 — VALUES as a Place

#### FR-604: float 2引数形式 (プロトタイプ指定) — ✅ COMPLETE

- **対象**: `src/compile/codegen-phase2.lisp`
- **実装**: Phase 2 handler "FLOAT" で `(= (length args) 2)` の場合、prototype を評価・破棄し `vm-float-inst` を emit。NaN-boxing double-float 統一なので prototype は常に no-op
- **根拠**: ANSI CL 12.1.3.3 — float (number &optional prototype)

#### FR-605: bignum (多倍長整数)

- **対象**: `src/vm/vm-numeric.lisp`, `src/vm/vm.lisp` (NaN-boxing 値表現)
- **現状**: VM は NaN-boxing で 53bit 整数 (float mantissa) のみ保持。`(expt 2 64)` 等の大整数演算が精度消失
- **内容**: bignum タグを追加してホスト CL integer オブジェクトをヒープに保持。算術演算でオーバーフロー検出→bignum に昇格。`bignump` 述語。`integer-length` / `integer-decode-float` との統合
- **根拠**: ANSI CL 12.1.1 — numeric tower; fixnum/bignum 区別
- **難易度**: Very Hard (値表現の根幹変更)

#### FR-606: assert の place-list (場所付きリスタート)

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `assert` の基本シグナル (`simple-error`) は実装済み。ANSI CL が定義する第3引数 (place リスト) によるインタラクティブ修正機能なし
- **内容**: `(assert test-form (place1 place2 ...) ...)` — 失敗時に `continue` リスタートで各 place の値を対話的に修正して再試行。`restart-case` と `store-value` リスタート (FR-421) に依存
- **根拠**: ANSI CL 9.1.3 — assert with places
- **難易度**: Medium (FR-421 後)

#### FR-607: defun / defmacro ドキュメント文字列

- **対象**: `src/compile/codegen.lisp`, `src/expand/expander.lisp`, `src/vm/vm.lisp`
- **現状**: `defun` / `defmacro` の本体先頭にある文字列リテラルを破棄してコンパイル続行。`(documentation 'fn 'function)` は ❌ (FR-436)
- **内容**: コンパイル時に docstring を検出し VM 関数オブジェクトのメタデータとして保存。`(documentation 'fn 'function)` / `(documentation 'mac 'macro)` で取得可能に。`define-condition` / `defclass` / `defmethod` / `defpackage` ドキュメント文字列も同様
- **根拠**: ANSI CL 5.4.1 — Documentation Strings
- **難易度**: Medium (FR-436 に先立つ基盤)

#### FR-609: list* ユーザー向けビルトイン登録

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `list*` はコンパイラ内部 (CPS変換・AST変換) で多用されているがユーザーコードのビルトイン登録がない。`(list* 1 2 '(3))` → `(1 2 3)` が機能しない
- **内容**: `(list* x1 x2 ... rest)` — 最後の引数にドットペアを生成する可変引数関数。`builtin-registry-data.lisp` に variadic 登録
- **根拠**: ANSI CL 14.2.1 — list*
- **難易度**: Easy

#### FR-610: -if-not 系シーケンス述語

- **対象**: `src/expand/macros-sequence.lisp`, `src/expand/macros-stdlib.lisp`
- **現状**: `find-if-not` / `position-if-not` / `count-if-not` が全コードベースに不在。ANSI CL 17.2 が義務付ける述語の否定版
- **内容**: 各関数は `(funcall pred x)` を `(not (funcall pred x))` に変えたラッパーとして実装可能
- **根拠**: ANSI CL 17.2 — sequence functions with -not variants
- **難易度**: Easy

#### FR-611: sort / stable-sort :key 引数

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `sort` / `stable-sort` は `(list predicate)` のみ。`(sort list pred :key #'car)` が展開時にエラー
- **内容**: `:key` キーワードを受け取り、各要素比較前に `(funcall key elem)` でキー抽出
- **根拠**: ANSI CL 17.2 — sort :key argument
- **難易度**: Easy

#### FR-612: read / read-char eof-error-p / eof-value 引数

- **対象**: `src/vm/io.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `vm-read-sexp-inst` は dst/src スロットのみ。`vm-read-char` は `(read-char stream nil +eof-value+)` とハードコードで、ユーザーが EOF 動作を制御できない
- **内容**: `(read stream eof-error-p eof-value recursive-p)` / `(read-char stream eof-error-p eof-value recursive-p)` — 4引数形式を完全サポート。eof-error-p = nil のとき EOF で eof-value を返す
- **根拠**: ANSI CL 23.2 / 21.2 — read, read-char eof handling
- **難易度**: Medium

#### FR-613: with-output-to-string オプション文字列引数

- **対象**: `src/expand/macros-stdlib.lisp`
- **現状**: `with-output-to-string` は新しい文字列出力ストリームを生成して返すのみ。ANSI CL が定義する既存の `string` 引数への追記形式なし
- **内容**: `(with-output-to-string (var string &optional element-type) body)` — `string` が提供された場合、既存の fill-pointer 付き文字列に追記するストリームを生成
- **根拠**: ANSI CL 21.2 — with-output-to-string with string argument
- **難易度**: Low (fill-pointer 対応文字列実装に依存)

#### FR-608: with-input-from-string キーワード引数

- **対象**: `src/expand/macros-stdlib.lisp` または `src/vm/io.lisp`
- **現状**: `with-input-from-string` 基本形式は動作するが `:start` / `:end` (入力範囲制限) と `:index` (終端位置書き戻し) キーワードが未対応
- **内容**: `(with-input-from-string (var string :start s :end e :index idx-place) body)` — `string` の `[s, e)` 範囲のみを読み込むストリームを生成。終了時に実際の読み取り位置を `idx-place` に `setf`
- **根拠**: ANSI CL 21.2 — with-input-from-string
- **難易度**: Low

#### FR-614: (setf char) — 文字列の破壊的文字置換

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/strings.lisp`
- **現状**: `char` は読み取り用ビルトイン登録済みだが `(setf char)` が未登録。`(setf (char str i) c)` でエラー
- **内容**: `(setf (char string index) char)` — 文字列の i 番目の文字を破壊的に置換。`(setf schar)` も同様
- **根拠**: ANSI CL 16.2 — setf of char
- **難易度**: Easy

#### FR-615: concatenate 型ディスパッチ完全化

- **対象**: `src/expand/macros-stdlib.lisp:621-629`
- **現状**: `(concatenate 'string ...)` のみ動作。`(concatenate 'list seq1 seq2)` / `(concatenate 'vector ...)` が機能しない。`coerce` との組み合わせも未確認
- **内容**: result-type 引数で `'string` / `'list` / `'vector` を分岐。各型に対応する結合ロジック
- **根拠**: ANSI CL 17.2.2 — concatenate
- **難易度**: Easy

#### FR-616: hash-table-size / hash-table-rehash-size / hash-table-rehash-threshold

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/hash.lisp`
- **現状**: `hash-table-test` は登録済みだが `hash-table-size` / `hash-table-rehash-size` / `hash-table-rehash-threshold` が不在
- **内容**: ハッシュテーブルのパラメータアクセサ群をビルトイン登録。VM のハッシュテーブル表現 (CL ネイティブ HT) が持つ情報を返す
- **根拠**: ANSI CL 18.2 — hash table accessors
- **難易度**: Easy

#### FR-617: read-from-string 2値目 (end-position)

- **対象**: `src/vm/io.lisp:744-750`
- **現状**: `vm-read-from-string-inst` は sexp のみ返却。`(read-from-string "123 abc" nil nil :start 4)` での end-position (7) が取得できない
- **内容**: `(read-from-string string eof-error-p eof-value :start s :end e :preserve-whitespace p)` → `(values sexp end-pos)` の2値返却。:start/:end/:preserve-whitespace キーワードも完全対応
- **根拠**: ANSI CL 23.2 — read-from-string
- **難易度**: Medium

#### FR-618: (setf aref) — 配列要素の破壊的書き込み

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/list.lisp`
- **現状**: `aref` は読み取り専用ビルトイン登録済み。`(setf (aref arr i) val)` が失敗
- **内容**: `(setf (aref array index) value)` — 1次元配列への書き込み。多次元 `(setf (aref arr i j) val)` も対応が必要
- **根拠**: ANSI CL 15.2 — setf of aref
- **難易度**: Easy

#### FR-619: (setf elt) — 実装済みと判明 (2026-03-27 修正)

- **状態**: ✅ 実装済み (FR-619 は誤記)
- **根拠**: `src/parse/cl/parser.lisp:418-419` — `(setf (elt seq i) val)` をパーサーが `(aset seq i val)` に展開。`aset` は `builtin-registry-data.lisp:407` に登録済み

#### FR-620: (setf svref) / (setf row-major-aref)

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `svref` (`builtin-registry-data.lisp:196`) / `row-major-aref` (`builtin-registry-data.lisp:195`) は読み取り専用登録。setf 展開なし
- **内容**: `(setf (svref vector i) val)` — simple-vector への書き込み。`(setf (row-major-aref array i) val)` — row-major インデックスへの書き込み
- **根拠**: ANSI CL 15.2 — setf of svref, row-major-aref
- **難易度**: Easy (FR-618 と同パターン)

#### FR-621: (setf nth)

- **対象**: `src/expand/expander.lisp` (`*setf-compound-place-handlers*`)
- **現状**: `car`/`cdr` の setf は `rplaca`/`rplacd` に展開済み (`expander.lisp:214-227`) だが `nth` の setf 展開が未定義
- **内容**: `(setf (nth n list) val)` → `(rplaca (nthcdr n list) val)` に展開。`*setf-compound-place-handlers*` に `nth` エントリを追加
- **根拠**: ANSI CL 14.2.1 — setf of nth
- **難易度**: Easy

#### FR-622: (setf get)

- **対象**: `src/expand/expander.lisp`
- **現状**: `get` (symbol plist アクセス) は読み取りビルトイン登録済みだが `(setf (get sym key) val)` の展開なし
- **内容**: `(setf (get symbol key) value)` → `%plist-put` または内部 setf で実装
- **根拠**: ANSI CL 10.2 — setf of get
- **難易度**: Easy

#### FR-623: let / flet 空バインディング形式

- **対象**: `src/parse/cl/parser.lisp`
- **現状**: `parser.lisp` の `let` パーサーが `(>= (length node) 3)` を要求 → `(let () body)` が構文エラーになる。`flet` も同様 (line 451)
- **内容**: `(let () body)` / `(flet () body)` を ANSI CL 仕様通り空バインディングリストとして受け入れ、`progn` に等価展開
- **根拠**: ANSI CL 3.1.2.1 — special operators with empty binding lists
- **難易度**: Easy

#### FR-624: subtypep

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `src/` 全体に `subtypep` の定義・登録が一切存在しない (`grep -r subtypep src/` → 0 件)
- **内容**: `(subtypep type1 type2 &optional environment)` — 2つの型指定子の包含関係を `(values result certainty)` で返す。`typep` が実装済みなので基本的な原子型は実現可能
- **根拠**: ANSI CL 4.2.2 — subtypep
- **難易度**: Medium (合成型指定子サポートは Hard)

#### FR-625: type-of — float / function 型返却

- **対象**: `src/vm/primitives.lisp` (lines 414-428)
- **現状**: `type-of` は `integer`/`string`/`symbol`/`list`/`vector`/`hash-table`/`t` を返す。`float` は `t` にフォールスルー、`function`/`vm-closure`/`vm-builtin-fn` は未処理
- **内容**: `float` → `single-float` または `double-float`。`vm-closure`/`vm-builtin-fn` オブジェクト → `function`。`nil` → `null`
- **根拠**: ANSI CL 4.2.6 — type-of
- **難易度**: Easy

#### FR-626: error / warn — フォーマット制御形式

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/conditions.lisp`
- **現状**: `error` は `make-vm-signal-error :error-reg` 単項登録。`(error "~A" x)` のような文字列+引数の多引数形式が未対応
- **内容**: コードジェネレータ (または展開器) で `(error string &rest args)` を `(error (format nil string args...))` にデシュガーして単一コンディションオブジェクトとして渡す。`warn` も同様
- **根拠**: ANSI CL 9.1.2 — error function signature
- **難易度**: Easy

#### FR-627: string-upcase / string-downcase / string-capitalize — :start/:end キーワード

- **対象**: `src/vm/strings.lisp` (lines 164-182), `src/compile/builtin-registry-data.lisp` (lines 16-18)
- **現状**: `define-simple-instruction` による unary 登録のみ。`(string-upcase str :start 2 :end 5)` はキーワード引数が無視される
- **内容**: `:start`/`:end` パラメータを受け取り部分文字列変換を実装。`nstring-upcase`/`nstring-downcase`/`nstring-capitalize` (FR-475) の破壊的版も同様
- **根拠**: ANSI CL 16.2.1 — string-upcase
- **難易度**: Easy

#### FR-628: open — ✅ COMPLETE

- **実装**: `vm-open-file` は phase2 handler で `:direction` / `:if-exists` / `:if-does-not-exist` を解析し、`with-open-file` から利用できる。
- **根拠**: ANSI CL 21.2.1 — open
- **難易度**: Low

#### FR-629: make-string-input-stream — ビルトイン未登録

- **対象**: `src/vm/io.lisp` (line 413: 内部使用は存在), `src/compile/builtin-registry-data.lisp`
- **現状**: `make-string-output-stream` は `builtin-registry-data.lisp:277` に登録済み。`make-string-input-stream` は未登録
- **内容**: `vm-make-string-input-stream` VM命令を定義し登録。`with-input-from-string` マクロが依存
- **根拠**: ANSI CL 21.3.5 — make-string-input-stream
- **難易度**: Easy

#### FR-630: coerce — 実行時型ディスパッチ

- **対象**: `src/expand/macros-sequence.lisp` (lines 375-390)
- **現状**: `coerce` はマクロとして実装。クォート済みリテラル `'list`/`'vector`/`'string` のみ対応。`(coerce x type-var)` のような実行時型変数は `coerce-to-string` にフォールスルーし誤動作
- **内容**: 実行時型ディスパッチ関数を追加。または `coerce-to-*` 群を呼ぶランタイム関数 `rt-coerce` を `builtin-registry-data.lisp` に登録
- **根拠**: ANSI CL 12.1 — coerce with computed type
- **難易度**: Medium

#### FR-631: macroexpand / macroexpand-1 — ユーザー呼び出し可能化

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/expand/macro.lisp`
- **現状**: `our-macroexpand-1` / `our-macroexpand-all` はコンパイラ内部関数として実装済みだが、`macroexpand` / `macroexpand-1` としてビルトイン未登録
- **内容**: `builtin-registry-data.lisp` に登録し、ユーザーコードから `(macroexpand '(when t :ok))` が呼べるようにする
- **根拠**: ANSI CL 3.1.2.1 — macroexpand
- **難易度**: Easy

#### FR-632: with-standard-io-syntax — 標準変数バインド

- **対象**: `src/expand/macros-stdlib.lisp` (line 285)
- **現状**: `(with-standard-io-syntax ,@body)` → `(progn ,@body)` のスタブ。FR-210 で追跡済み
- **内容**: ANSI CL が指定する `*print-base*`/`*print-escape*`/`*print-readably*`/`*print-*` 全変数を標準値にバインドして body を実行
- **根拠**: ANSI CL 22.1.1 — with-standard-io-syntax
- **難易度**: Medium (プリンタ変数の実装に依存)

#### FR-633: clear-output — ビルトイン未登録

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/io.lisp`
- **現状**: `clear-input` (line 384) は登録済みだが `clear-output` が不在。`force-output`/`finish-output` は登録済み
- **内容**: `vm-clear-output` 命令を定義し登録。出力バッファを破棄
- **根拠**: ANSI CL 21.2 — clear-output
- **難易度**: Easy

#### FR-634: clrhash — ビルトイン未登録

- **対象**: `src/vm/hash.lisp` (line 73: `vm-clrhash`), `src/compile/builtin-registry-data.lisp`
- **現状**: `vm-clrhash` VM命令が実装済みだが `builtin-registry-data.lisp` に登録なし。`remhash` (line 319) は登録済み
- **内容**: `remhash` と同パターンで `clrhash` を登録
- **根拠**: ANSI CL 18.1 — clrhash
- **難易度**: Easy (登録のみ)

#### FR-635: bit-nor / bit-nand / bit-eqv / bit-andc1 / bit-andc2 / bit-orc1 / bit-orc2

- **対象**: `src/vm/primitives.lisp` または `src/vm/list.lisp`
- **現状**: `bit-and`/`bit-or`/`bit-xor`/`bit-not` のみ実装。ANSI CL 15.2 の7種の追加論理演算が不在
- **内容**: 各演算をビット配列要素ごとに適用する VM 命令を追加登録
- **根拠**: ANSI CL 15.2 — bit logical operations
- **難易度**: Easy

#### FR-636: (setf bit) / (setf sbit)

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `bit` (line 298) と `sbit` (line 299) は読み取り登録済み。書き込み `(setf (bit vec i) v)` / `(setf (sbit vec i) v)` は未登録
- **内容**: `setf` 展開器に `bit`/`sbit` の place 展開を追加
- **根拠**: ANSI CL 15.2 — (setf bit)
- **難易度**: Easy

#### FR-637: 文字列比較 — :start1/:end1/:start2/:end2 部分文字列キーワード

- **対象**: `src/vm/strings.lisp` (string comparison VM instructions)
- **現状**: 全文字列比較 VM 命令 (`vm-string-equal` 等) が `(dst str1 str2)` 2引数のみ。`(string= s1 s2 :start1 2 :end1 5)` のような部分文字列比較が不可
- **内容**: `:start1`/`:end1`/`:start2`/`:end2` を受け取るよう各命令のスロットとエグゼキュータを拡張。`subseq` で代替可能だが ANSI 準拠には必要
- **根拠**: ANSI CL 16.2.3 — string comparisons with keyword arguments
- **難易度**: Medium (全比較命令の修正が必要)

#### FR-638: loop named — 名前付きループ

- **対象**: `src/expand/loop-parser.lisp`, `src/expand/loop-emitters.lisp`
- **現状**: `loop` パーサーに `named` キーワード処理がない。`loop named foo ... (return-from foo val)` がパースエラー
- **内容**: ループ名を `loop-state` に保存し、`return-from` に対応する `catch`/`throw` を生成
- **根拠**: ANSI CL 6.1.1 — named loop
- **難易度**: Medium

#### FR-639: asinh / acosh / atanh — 逆双曲線関数

- **対象**: `src/vm/primitives.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `sinh`/`cosh`/`tanh` は実装済み (lines 60-62)。逆関数 3種が全コードベースに不在
- **内容**: CL の `asinh`/`acosh`/`atanh` を VM命令として追加し登録
- **根拠**: ANSI CL 12.2.4 — asinh, acosh, atanh
- **難易度**: Easy

#### FR-640: nreconc

- **対象**: `src/vm/list.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `nconc` (line 203) は実装済み。`nreconc` が全コードベースに不在
- **内容**: `(nreconc list tail)` ≡ `(nconc (nreverse list) tail)` — 破壊的逆順追加
- **根拠**: ANSI CL 14.2.22 — nreconc
- **難易度**: Easy

#### FR-641: fill / replace / mismatch

- **対象**: `src/expand/macros-sequence.lisp`, `src/vm/strings.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `fill`/`replace`/`mismatch` は `our-defmacro` としてリスト専用実装あり (`macros-sequence.lisp:11/28/50`)。`fill` は `(car ptr)`/`(setf (car ptr) ...)` によるコンスセル破壊的更新; `replace` はリスト間コピー; `mismatch` は `eql` 固定の線形探索。`:start`/`:end`/`:test`/`:key` 等のキーワード引数は `(when keys)` no-op で全て無視。ベクタ・文字列への適用不可。文字列セクション (`fill`/`replace` 文字列) は VM命令・ビルトインとも完全不在 ❌ のまま
- **内容**: `(fill seq item &key start end)` — 要素を埋める。`(replace s1 s2 &key start1 end1 start2 end2)` — シーケンス間コピー。`(mismatch s1 s2 &key test key start1 end1 start2 end2)` — 最初の不一致位置を返す
- **根拠**: ANSI CL 17.3 — fill, replace, mismatch
- **難易度**: Medium

#### FR-642: *features* — vm-state / vm2-state 不整合

- **対象**: `src/vm/vm.lisp` (line ~381), `src/vm/vm-run.lisp` (line ~186)
- **現状**: `vm-state` は `*features*` を `(:common-lisp :cl-cc)` で初期化するが `vm2-state` (平坦ベクタインタープリタ) は `(:cl-cc)` のみ。実行系によって `#+common-lisp` の結果が異なる
- **内容**: `vm2-state` の `*features*` 初期化に `:common-lisp` を追加して統一
- **根拠**: ANSI CL 1.5.2 — *features* must include :common-lisp
- **難易度**: Trivial (1行修正)

#### FR-643: #nR — 任意基数整数リテラル

- **対象**: `src/parse/lexer.lisp` (`lex-read-hash-dispatch`, line 548)
- **現状**: `#b`/`#o`/`#x` は `case` ブランチで処理済み。先頭が数字の `#12R` は `otherwise` ブランチでエラー (`lexer.lisp:622`)
- **内容**: `#` の後に数字が続く場合、基数 `n` を読み、`R`/`r` を確認し `lex-read-radix-integer state n` で整数をパース
- **根拠**: ANSI CL 2.4.8 — #nR
- **難易度**: Easy

#### FR-644: #C — 複素数リテラル

- **対象**: `src/parse/lexer.lisp` (`lex-read-hash-dispatch`, line 548)
- **現状**: `#c`/`#C` が `case` ブランチになく `otherwise` でエラー。doc の「ホスト CL 経由で動作」は誤記
- **内容**: `#C(real imag)` → `(complex real imag)` に展開。lexer に `#\c`/`#\C` ブランチを追加
- **根拠**: ANSI CL 2.4.8.11 — #C
- **難易度**: Easy

#### FR-645: char= / char< 等 — 多引数形式非対応

- **対象**: `src/compile/builtin-registry.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `:char-cmp` 呼び出し規約は `(2 . 2)` 固定 (`*convention-arity*` line 394)。`emit-builtin-char-cmp` は1番目・2番目引数のみ使用
- **内容**: ANSI CL 13.2 — `(char= character &rest more-characters)` は3引数以上を許可。`(char< a b c)` は `(and (char< a b) (char< b c))` に展開すべき。`char-equal`/`char-lessp` 等 case-insensitive 版も同様
- **根拠**: ANSI CL 13.2.1 — character comparison functions
- **難易度**: Easy (コンパイル時展開で対応可能)

#### FR-646: *print-base* / *print-escape* 等 — 初期化済みだが印字関数が参照しない

- **対象**: `src/vm/vm.lisp` (lines 395-401), `src/vm/io.lisp` (line 696)
- **現状**: `*print-base*`/`*print-radix*`/`*print-escape*`/`*print-level*`/`*print-length*` は VM グローバル変数テーブル (`*vm-initial-globals*`) に初期値で登録済み。しかし `execute-instruction` for `vm-write-to-string-inst` は `(write-to-string val)` と直接ホスト CL を呼ぶだけで VM グローバルを参照しない。`(setq *print-base* 16)` しても出力が変わらないデッドコード状態
- **内容**: `vm-write-to-string-inst` 実行時に VM グローバルの `*print-base*` 等を読み取り `(write-to-string val :base ... :escape ...)` に渡す。または自前の印字ルーティン実装
- **根拠**: ANSI CL 22.1.1 — printer control variables
- **難易度**: Medium

#### FR-647: symbol-value — ビルトイン未登録・ホストブリッジ対象外

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/vm.lisp`
- **現状**: `symbol-name` は `:unary` 登録済み (`builtin-registry-data.lisp:111`)。`symbol-function` はホストブリッジ白リスト (`vm.lisp:491`) 経由で動作。しかし `symbol-value` は両方に不在。`rt-symbol-value` (`runtime/runtime.lisp:401`) はコンパイラ内部専用で、ユーザーコードからは呼び出し不可
- **内容**: `(symbol-value sym)` → `:unary` ビルトインとして `make-vm-symbol-value` 命令を登録。`(setf symbol-value)` は既存の `setq` フォールスルーで動作
- **根拠**: ANSI CL 10.1.1 — symbol-value
- **難易度**: Easy

#### FR-648: simple-vector-p — コードベース全体に不在

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `vectorp` は `:unary` 登録済み (`builtin-registry-data.lisp:98`)。`simple-vector-p` はコードベース全体に存在しない (`grep` 0件)
- **内容**: `(simple-vector-p x)` → `(and (vectorp x) (not (array-has-fill-pointer-p x)) ...)` として実装、または VM 命令を追加してビルトイン登録
- **根拠**: ANSI CL 15.2 — simple-vector-p; cl-cc の配列は全て単純ベクタ扱いの可能性あり
- **難易度**: Easy

#### FR-649: write-to-string キーワード引数 (`:base`, `:radix`, `:escape` 等)

- **対象**: `src/compile/builtin-registry-data.lisp:154`, `src/vm/io.lisp:693-696`
- **現状**: `write-to-string` は `:unary` 規約 (1引数のみ) で登録。`(write-to-string 255 :base 16)` はコンパイル時に引数数エラー。`execute-instruction` は `(write-to-string val)` を直接呼ぶだけ (FR-646 と連動)
- **内容**: `(write-to-string object &key base radix escape level length circle gensym readably pretty)` に対応した可変引数ビルトイン登録、またはオプション引数規約の追加
- **根拠**: ANSI CL 22.1.2 — write-to-string
- **難易度**: Medium (FR-646 の印字変数参照と合わせて実装が望ましい)

#### FR-650: every / some / notany / notevery — 多シーケンス並列形式非対応

- **対象**: `src/expand/macros-stdlib.lisp` (lines 364-390)
- **現状**: 全て `(pred list)` 2引数固定。`every` は `(our-defmacro every (pred list) ...)` で定義。`notany`/`notevery` は `some`/`every` への委譲のみ
- **内容**: ANSI CL 17.2.1 — `(every predicate &rest sequences+)` は複数シーケンスを並列イテレート。`(every #'< '(1 2 3) '(2 3 4))` は `(and (< 1 2) (< 2 3) (< 3 4))` と等価。`&rest sequences` の zip イテレーションが必要
- **根拠**: ANSI CL 17.2.1 — every, some, notany, notevery
- **難易度**: Medium

#### FR-651: vector (コンストラクタ)

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/list.lisp`
- **現状**: `coerce-to-vector` は既存シーケンスをベクタに変換する専用関数。`(vector 1 2 3)` のような新規ベクタ構築は未登録
- **内容**: `(vector &rest objects)` — 各 `objects` を要素とする1次元単純ベクタを返す。可変引数なので `:variadic` 規約またはマクロ展開が必要
- **根拠**: ANSI CL 15.2 — vector
- **難易度**: Easy

#### FR-652: copy-seq — ベクタ・配列非対応

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `(our-defmacro copy-seq (seq) \`(copy-list ,seq))` — `copy-list` に委譲のためリスト専用
- **内容**: `(copy-seq sequence)` は文字列・ベクタ・配列に対して正しいコピーを返す必要がある。`(etypecase seq (list (copy-list seq)) (vector (copy-vector seq)) (string (copy-string seq)))` に相当するディスパッチが必要
- **根拠**: ANSI CL 17.3 — copy-seq
- **難易度**: Easy–Medium

#### FR-653: remove-duplicates / delete-duplicates — キーワード引数非対応

- **対象**: `src/expand/macros-sequence.lisp`
- **現状**: `remove-duplicates` は `(our-defmacro remove-duplicates (list) ...)` で1引数 eql のみ。`delete-duplicates` は `(our-defmacro delete-duplicates (list &rest keys) (when keys) ...)` で `when` にボディなし → `keys` を完全無視
- **内容**: `:test`, `:test-not`, `:key`, `:start`, `:end`, `:from-end` キーワード対応が必要
- **根拠**: ANSI CL 17.2.19-20 — remove-duplicates / delete-duplicates
- **難易度**: Medium

#### FR-654: make-array :initial-contents キーワード無視

- **対象**: `src/compile/codegen-phase2.lisp` (line 86)
- **現状**: `(compile-make-array ...)` ハンドラは `size-reg` のみを使用して `vm-make-array` 命令を生成。`:initial-contents` を含む追加キーワードは全て無視
- **内容**: `(make-array 3 :initial-contents '(1 2 3))` が空配列を返してしまう。コンパイル時定数リストまたは実行時 `fill` ループによる初期化が必要
- **根拠**: ANSI CL 15.2 — make-array
- **難易度**: Medium

#### FR-665: mapcar / mapc / mapcan — 複数シーケンス非対応

- **対象**: `src/expand/macros-stdlib.lisp` (lines 334-362)
- **現状**: `(our-defmacro mapcar (fn list) ...)` — `list` は単一引数。`mapc`/`mapcan` も同様。`dolist` による1シーケンスイテレーションのみ
- **内容**: `(mapcar function &rest lists+)` — 複数リストを並列処理。`(mapcar #'+ '(1 2) '(3 4))` → `(3 7)`。`mapc`/`mapcan` も同様
- **根拠**: ANSI CL 17.3 — mapcar / mapc / mapcan
- **難易度**: Medium (複数シーケンスの zip イテレーション実装が必要)

#### FR-663: = / < / > / <= / >= — 多引数形式非対応

- **対象**: `src/parse/cl/parser.lisp` (line 198)
- **現状**: `(define-list-lowerer (+ - * = < > <= >=) (node sf sl sc) (unless (= (length node) 3) (error "~S takes exactly 2 args" (car node))))` — 3引数以上はパース時にエラー
- **内容**: ANSI CL 12.1.1 — `(= 1 2 3)` ≡ `(and (= 1 2) (= 2 3))`。2引数のみ有効
- **根拠**: ANSI CL 12.2 — /=, =, <, >, <=, >=
- **難易度**: Easy (expander で `reduce-variadic-op` スタイルの `and` 折り畳みを追加)

#### FR-664: /= — 完全不在

- **対象**: `src/parse/cl/parser.lisp`, `src/compile/builtin-registry-data.lisp`, `src/vm/primitives.lisp`
- **現状**: `/=` は `define-list-lowerer` リスト・`*typed-binop-ctors*`・ビルトインレジストリ・VM命令 (vm-num-neq 等) 全てに不在。`(/= a b)` は vm-call フォールスルー → 実行時 `"Undefined function: /="`
- **内容**: `(≠ number &rest more-numbers+)` — 全引数が相互に数値的に異なる場合 true
- **根拠**: ANSI CL 12.2 — /=
- **難易度**: Easy

#### FR-661: / (除算) — コンパイラが vm-div を生成しない

- **対象**: `src/parse/cl/parser.lisp`, `src/compile/builtin-registry-data.lisp`
- **現状**: `+`/`-`/`*` は `parser.lisp:198` の `define-list-lowerer` で `ast-binop` に変換され `vm-add`/`vm-sub`/`vm-mul` を生成する。しかし `/` はそのリストに含まれておらず `ast-call` に変換される。ビルトインレジストリにも未登録。`vm-div` 命令は `primitives.lisp:105` に定義済みだが `make-vm-div` の呼び出し元がコンパイラに存在しない。実行時に `vm-resolve-function` でホストブリッジ白リストに不在のため `"Undefined function: /"` エラー
- **内容**: `parser.lisp:198` の `define-list-lowerer` に `/` を追加し `*typed-binop-ctors*` に `(/ . make-vm-div)` を追加
- **根拠**: ANSI CL 12.2 — /
- **難易度**: Easy

#### FR-662: min / max / gcd / lcm — 可変引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/expand/expander-data.lisp`
- **現状**: `min`/`max`/`gcd`/`lcm` は全て `:binary` 規約 (min=2, max=2) で登録。ANSI CL は `(min number &rest more-numbers+)` (1+引数)、`(gcd &rest integers)` (0+引数) を要求。3引数以上はアリティチェック失敗後に vm-call fallthrough → `"Undefined function: min"` エラー
- **内容**: `*variadic-fold-builtins*` に `min`/`max`/`gcd`/`lcm` を追加し `reduce-variadic-op` による折り畳みを有効化。または `:binary-opt-one` 規約で拡張
- **根拠**: ANSI CL 12.2 — min, max, gcd, lcm
- **難易度**: Easy–Medium

#### FR-660: assoc-if / assoc-if-not — ユーザー向け登録なし

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `assoc-if` は `src/compile/pipeline.lisp:150` にコンパイラ内部ヘルパーとして定義 (`defun assoc-if (pred alist)`) されているが、これはホスト CL の代替用であり ユーザーコードのビルトイン登録はない。`assoc-if-not` は完全不在
- **内容**: `(assoc-if test alist &key key)` — `test` を満たす最初の cons を返す。`(assoc-if-not test alist &key key)` — 満たさない最初の cons
- **根拠**: ANSI CL 14.2.3 — assoc-if / assoc-if-not
- **難易度**: Easy

#### FR-697: assoc / rassoc / member — キーワード引数 (:test / :key / :test-not) 非対応

- **対象**: `src/expand/expander-data.lisp`, `src/vm/list.lisp`
- **現状**: `assoc`/`rassoc`/`member` は `*binary-builtins*` に分類 (アリティ 2..2)。`(assoc key alist :test #'equal)` は `emit-registered-builtin` でアリティ不一致 (nargs=4) → `nil` 返却 → `vm-call` フォールスルー → 実行時エラー。ANSI CL では `assoc` / `rassoc` / `member` は全て `&key test test-not key` をサポート
- **内容**: `:test`/`:test-not`/`:key` キーワード引数付き呼び出しを VM 命令レベルで対応。または expander でキーワード引数を parse して適切な比較関数を選択
- **根拠**: ANSI CL 14.2.1 (member), 14.2.3 (assoc), 14.2.3 (rassoc)
- **難易度**: Medium

#### FR-658: read-preserving-whitespace — 未登録

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: `read` は `:unary` 登録済み (`line 161`)。`read-preserving-whitespace` は全コードベースに不在
- **内容**: `(read-preserving-whitespace &optional stream eof-error-p eof-value recursive-p)` — `read` と同じだが後続の空白文字をストリームに残す
- **根拠**: ANSI CL 23.2.1 — read-preserving-whitespace
- **難易度**: Easy (ホスト CL に委譲可能)

#### FR-659: read-delimited-list — 未登録

- **対象**: `src/compile/builtin-registry-data.lisp`
- **現状**: 全コードベースに不在
- **内容**: `(read-delimited-list char &optional stream recursive-p)` — `char` が出現するまで sexp を読み続け、リストとして返す。カスタムリーダーマクロの定義で必須
- **根拠**: ANSI CL 23.2.1 — read-delimited-list
- **難易度**: Medium (VM read ループ拡張が必要)

#### FR-655: find-symbol — ビルトイン未登録

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/vm.lisp`
- **現状**: `find-symbol` はコンパイラ内部では使われない。`*vm-host-bridge-functions*` には `find-package` / `intern` が登録されているが `find-symbol` は対象外
- **内容**: `(find-symbol string &optional package-designator)` — パッケージからシンボルを検索し、シンボルと状態の2値を返す
- **根拠**: ANSI CL 11.2.4 — find-symbol
- **難易度**: Easy (intern と同様にホストブリッジ登録)

#### FR-656: list-length — ✅ COMPLETE

- **実装**: `vm-list-length` が `builtin-registry-data.lisp:161` に登録済みで、ユーザーコードから利用できる。
- **根拠**: ANSI CL 14.2.18 — list-length
- **難易度**: Low

#### FR-657: subst-if / subst-if-not — ツリー置換述語版

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/list.lisp`
- **現状**: `subst` は `:ternary-opt-nil-custom` 規約で登録済み (`builtin-registry-data.lisp:403`)。`subst-if` / `subst-if-not` は VM命令・コンパイラ登録ともに不在
- **内容**: `(subst-if new pred tree &key key)` — `pred` を満たすサブツリーを `new` で置換。`(subst-if-not new pred tree &key key)` — `pred` を満たさないサブツリーを置換
- **根拠**: ANSI CL 14.2.34/35 — subst-if / subst-if-not
- **難易度**: Easy–Medium

#### FR-682: `-` 単項否定バグ / 0引数エラー欠如

- **対象**: `src/expand/expander.lisp` (lines 393-397)
- **現状**: `define-expander-for -` が `reduce-variadic-op '- (cdr form) 0` を呼ぶ。`reduce-variadic-op` の1引数ケースは `(first args)` = `x` を返すため `(- x)` → `x` (否定しない)。0引数ケースは identity `0` を返すが ANSI は `(-)` をエラーとして定義
- **内容**: 1引数特殊ケースを追加: `(1 (list '- 0 (first args)))` → `(- 0 x)`。0引数ケース: `(error "- requires at least one argument")`
- **根拠**: ANSI CL 12.2 — `(- x)` → `-x`; `(-)` is an error
- **難易度**: Easy (1行の条件追加)

#### FR-683: isqrt — 大整数で浮動小数点精度喪失

- **対象**: `src/expand/macros-stdlib.lisp` (line 43)
- **現状**: `(floor (sqrt (float n)))` に展開。`(float n)` が single-float に変換するため 2^24 を超える整数で丸め誤差が発生し `isqrt` が不正確な結果を返す
- **内容**: 整数算術の exact square root 実装 (Newton's method on integers)
- **根拠**: ANSI CL 12.1.3 — isqrt returns the greatest integer ≤ exact positive square root
- **難易度**: Medium (bignum 対応 FR-605 の後に完全化)

#### FR-684: signum — 型非保存 (常に整数を返す)

- **対象**: `src/expand/macros-stdlib.lisp` (lines 35-40)
- **現状**: `(cond ((zerop n) 0) ((plusp n) 1) (t -1))` に展開。常に整数 -1/0/1 を返す
- **内容**: `(signum 2.5)` → `1.0`、`(signum -3.0d0)` → `-1.0d0`。入力型に対応した型の 1/-1/0 を返す
- **根拠**: ANSI CL 12.1.3 — signum preserves type
- **難易度**: Medium

#### FR-685: float-sign 2引数形式非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 67), `src/vm/vm-numeric.lisp`
- **現状**: `*builtin-unary-entries*` に登録 (1引数のみ)
- **内容**: `(float-sign float1 float2)` — `float2` の絶対値 × `float1` の符号。`:binary-opt-one` 規約に変更
- **根拠**: ANSI CL 12.1.3 — `(float-sign float1 &optional float2)`
- **難易度**: Easy

#### FR-686: aref — 多次元配列非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 300), `src/vm/array.lisp` (lines 22-28)
- **現状**: `aref` は `*builtin-binary-custom-entries*` に `:array-reg :index-reg` の2スロット構成で登録。`(aref arr i j)` (2次元) はコンパイル時にエラーまたはインデックスが1つだけ使用される
- **内容**: `(aref array &rest subscripts)` — 多次元対応。`row-major-aref` + `array-row-major-index` の合成、または VM 命令スロット拡張
- **根拠**: ANSI CL 15.2 — aref with multiple subscripts
- **難易度**: Medium

#### FR-687: make-array キーワード引数 — 部分対応

- **対象**: `src/expand/expander.lisp` (lines 229-239), `src/compile/codegen-phase2.lisp` (lines 86-90)
- **現状**: expander `expand-make-array-form` は `:fill-pointer`/`:adjustable` の**存在**を検出して `make-adjustable-vector` に昇格するが、実際の値を無視 (`(make-array 10 :fill-pointer 5)` → fill-pointer=0)。`:initial-element`/`:initial-contents`/`:element-type`/`:displaced-to` は expander で静かに破棄される (検出すらされない)
- **内容**: expander で全キーワードの値を正確に解析し、VM 命令の対応スロットに渡す
- **根拠**: ANSI CL 15.2 — make-array keyword arguments
- **難易度**: Medium

#### FR-688: delete / substitute 系 — keyword 引数が no-op

- **対象**: `src/expand/macros-sequence.lisp` (lines 73-202)
- **現状**: `delete`/`delete-if`/`delete-if-not`/`substitute`/`substitute-if`/`substitute-if-not`/`nsubstitute` 系はすべて `&rest keys` を受け付けるが本体内の `(when keys ...)` が no-op。`:test`, `:key`, `:start`, `:end`, `:count`, `:from-end` が静かに無視され、常に `eql` + 全シーケンス操作になる
- **内容**: キーワード引数の解析と適用。特に `:count 1` が最も使用頻度が高い
- **根拠**: ANSI CL 17.3 — delete/substitute keyword arguments
- **難易度**: Medium (`:test`/`:key` は比較的容易; `:start`/`:end` はシーケンス型依存)

#### FR-678: random — optional random-state 引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 77), `src/vm/vm-numeric.lisp` (line 140)
- **現状**: `random` は `*builtin-unary-entries*` に登録 (1引数のみ)。`vm-random` は `define-simple-instruction :unary random` で実装
- **内容**: ANSI CL 12.1.6 — `(random limit &optional random-state)`。`(random 10 my-state)` でユーザー管理の random-state を使用できない。`:binary-opt-one` 等の規約に変更が必要
- **根拠**: ANSI CL 12.1.6 — random
- **難易度**: Easy

#### FR-679: get-decoded-time — ビルトイン未登録

- **対象**: `src/compile/builtin-registry-data.lisp` (lines 270-278)
- **現状**: `*builtin-nullary-entries*` に `get-decoded-time` が不在。`get-universal-time`/`get-internal-real-time`/`get-internal-run-time` は登録済み
- **内容**: `(get-decoded-time)` — 現在時刻を9値 (second minute hour date month year day-of-week daylight-p zone) で返す。`(decode-universal-time (get-universal-time))` との違いはタイムゾーン取得の一発性
- **根拠**: ANSI CL 25.1.4 — get-decoded-time
- **難易度**: Easy (nullary builtin + `(decode-universal-time (get-universal-time))` へのデシュガーで実装可)

#### FR-680: provide / require — 展開問題

- **対象**: `src/expand/macros-sequence.lisp` (lines 403-413)
- **現状**: `provide` が `(pushnew ,mod *modules* :test #'string=)` に展開するが `pushnew` は FR-587 で未実装 (❌)。コンパイル済みコードで `(provide "mymod")` を呼ぶと実行時エラー。`require` は `pathnames` 引数を `(declare (ignore pathnames))` で無視し、モジュール不在時 `(warn ...)` するだけで実際のファイルロードを行わない
- **内容**: (1) `provide` — `pushnew` を `(unless (member mod *modules* :test #'string=) (push mod *modules*))` に書き換え。(2) `require` — `pathnames` が指定された場合 `(dolist (p pathnames) (load p))` を実行
- **根拠**: ANSI CL 25.1.3 — provide/require
- **難易度**: Easy (provide), Medium (require — load integration)

#### FR-681: sleep — ✅ COMPLETE

- **実装**: `sleep` が `builtin-registry-data.lisp:133` に登録済み。
- **根拠**: ANSI CL 25.1.1 — sleep
- **難易度**: Low

#### FR-675: maphash — ✅ COMPLETE

- **実装**: `our-defmacro maphash` が `hash-table-keys` を使ってキー/値を走査する (`src/expand/macros-stdlib.lisp:1317-1325`)。`builtin-registry-data.lisp` にも `maphash` が登録済み (`src/compile/builtin-registry-data.lisp:180`)。
- **根拠**: ANSI CL 18.1 — maphash
- **難易度**: Low

#### FR-676: with-slots — ✅ COMPLETE

- **実装**: `with-slots` は `symbol-macrolet` で展開し、`(setf slot ...)` が `slot-value` に透過される。
- **根拠**: ANSI CL 7.6.6 — with-slots must use symbol-macrolet for place semantics
- **難易度**: Easy (but depends on FR-220 symbol-macrolet)

#### FR-677: class-name — ✅ COMPLETE

- **実装**: `class-name` が `builtin-registry-data.lisp:176` に登録され、`vm-class-name` に解決される。CLOS イントロスペクションとして利用可能。
- **根拠**: ANSI CL 7.7.2 — class-name
- **難易度**: Low

#### FR-667: logand / logior / logxor / logeqv — 可変引数非対応

- **対象**: `src/expand/expander-data.lisp` (line 49-51), `src/compile/builtin-registry-data.lisp` (lines 178-181)
- **現状**: `*variadic-fold-builtins*` は `(+ * append nconc)` のみ。`logand`/`logior`/`logxor`/`logeqv` は `*binary-builtins*` に分類 (アリティ 2.2)。`(logand a b c)` はコンパイル時 `emit-registered-builtin` がアリティ不一致で nil を返し vm-call にフォールスルー → 実行時エラー。`(logand)` → ANSI では `-1` だが cl-cc ではエラー。`(logior)` → ANSI では `0`
- **内容**: `logand`/`logior`/`logxor` を `*variadic-fold-builtins*` に追加 (恒等元: -1, 0, 0)。または expander で `(logand a b c)` → `(logand (logand a b) c)` にネスト展開
- **根拠**: ANSI CL 12.1.4 — logand/logior/logxor/logeqv は `&rest integers`
- **難易度**: Easy

#### FR-668: digit-char-p — optional radix 引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 125), `src/vm/symbols.lisp` (lines 84-88)
- **現状**: `digit-char-p` は `*builtin-unary-entries*` で unary 登録。VM 命令 `vm-digit-char-p` は `(digit-char-p ch)` のみ実行。`(digit-char-p #\A 16)` → ANSI では `10` だが cl-cc ではアリティエラー
- **内容**: 2引数形式 `(digit-char-p char radix)` 対応。builtin を `:binary-opt-one` 等の規約に変更、VM 命令に `:radix` スロット追加
- **根拠**: ANSI CL 13.1.4.2 — `(digit-char-p char &optional (radix 10))`
- **難易度**: Easy

#### FR-669: string comparators — 戻り値が boolean (0/1) で mismatch-index を返さない

- **対象**: `src/vm/strings.lisp` (lines 114-124), `src/compile/builtin-registry-data.lisp` (lines 212-227)
- **現状**: 全 string 比較命令 (`vm-string-equal`/`vm-string-lessp` 等) が `:pred2` 実行スタイル (1/0 boolean 返却)。ANSI CL では `string=` は nil/t だが `string<`/`string>`/`string<=`/`string>=`/`string/=` および全 case-insensitive 変種は nil (等しい/大小逆) または **不一致位置の整数** を返す
- **内容**: `(string< "abc" "abd")` → `2`、`(string/= "ab" "ab")` → `nil`。VM 命令を `:pred2` から位置返却型に変更。case-insensitive 変種も同様
- **根拠**: ANSI CL 13.2.4 — string comparators return index or nil
- **難易度**: Medium

#### FR-670: peek-char — ビルトイン未登録

- **対象**: `src/compile/builtin-registry-data.lisp`, `src/vm/io.lisp` (lines 69-74)
- **現状**: VM 命令 `vm-peek-char` は存在 (`io.lisp:69-74`) し `execute-instruction` も実装済み。しかし `builtin-registry-data.lisp` のいずれのエントリテーブルにも `peek-char` が登録されていない。`(peek-char)` はコンパイルフォールスルー → `"Undefined function: peek-char"` 実行時エラー
- **内容**: `(peek-char &optional peek-type input-stream eof-error-p eof-value recursive-p)` — `*builtin-stream-input-opt-entries*` 等に追加。VM 命令に `:peek-type` スロット追加が必要
- **根拠**: ANSI CL 21.2 — peek-char
- **難易度**: Easy (登録のみ)〜Medium (peek-type 完全対応)

#### FR-671: unread-char — optional stream 引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (line 320)
- **現状**: `unread-char` は `*builtin-binary-void-entries*` に `:binary-void` 規約 (2引数必須)。`(unread-char ch)` のデフォルト `*standard-input*` 形式はアリティ不一致でコンパイルエラー
- **内容**: `(unread-char character &optional input-stream)` — `:binary-void` を optional-stream 対応規約に変更
- **根拠**: ANSI CL 21.2 — `(unread-char character &optional input-stream)`
- **難易度**: Easy

#### FR-672: read-line — eof 引数非対応・第2戻り値欠落

- **対象**: `src/vm/io.lisp` (lines 331-341), `src/compile/builtin-registry-data.lisp` (line 377)
- **現状**: `read-line` は `:stream-input-opt` 規約 (handle のみ optional)。VM 実行は `(read-line stream nil +eof-value+)` hardcoded — `eof-error-p` 常に `nil`、`eof-value` 常に `:eof`。また `(multiple-value-bind (line eof-p) (read-line s))` の第2戻り値 `missing-newline-p` が `(declare (ignore ...))` で削除され常に `nil`
- **内容**: (1) `eof-error-p`/`eof-value` 引数を通す。(2) `(values line missing-newline-p)` として2値返却
- **根拠**: ANSI CL 21.2 — read-line returns (values string missing-newline-p)
- **難易度**: Easy〜Medium

#### FR-673: terpri / fresh-line — optional stream 引数非対応

- **対象**: `src/compile/builtin-registry-data.lisp` (lines 266-268), `src/vm/format.lisp` (lines 103-111)
- **現状**: `*builtin-void-side-effect-entries*` 規約は引数なし。VM 命令はスロットなし (`vm-terpri-inst`/`vm-fresh-line-inst`)、`execute-instruction` は `(vm-output-stream state)` のみに出力。`(terpri *error-output*)` は引数がコンパイルエラーまたはデフォルト出力に書き込む
- **内容**: `(terpri &optional output-stream)` / `(fresh-line &optional output-stream)` — `:void-side-effect-opt-stream` 等の新規約を追加し、stream を通す
- **根拠**: ANSI CL 21.2 — terpri/fresh-line optional stream
- **難易度**: Easy

#### FR-674: listen — required stream 引数 (デフォルト非対応)

- **対象**: `src/compile/builtin-registry-data.lisp` (line 255)
- **現状**: `listen` は `:handle-input` 規約 (handle は必須引数)。ANSI では `(listen &optional input-stream)` — stream なしで `*standard-input*` を使用。`(listen)` はアリティエラー
- **内容**: `:handle-input` を optional-stream 対応規約に変更
- **根拠**: ANSI CL 21.2 — `(listen &optional input-stream)`
- **難易度**: Easy

#### FR-666: print / prin1 / princ — オプション stream 引数なし

- **対象**: `src/compile/builtin-registry-data.lisp` (lines 260-262)
- **現状**: 3関数は `:side-effect` 規約で登録。`*convention-arity*` における `:side-effect` のアリティは `(1 . 1)` — 1引数固定。`(print x stream)` は `emit-registered-builtin` がアリティ不一致で nil を返し vm-call にフォールスルー → 実行時 `"Undefined function: print"`
- **内容**: `(print object &optional stream)` / `(prin1 object &optional stream)` / `(princ object &optional stream)` — ANSI CL の全シグネチャ対応。`:side-effect` を `:side-effect-opt-stream` 等の新規約に拡張するか、codegen-phase2 で個別ハンドラを追加
- **根拠**: ANSI CL 22.3.3 — print / prin1 / princ
- **難易度**: Easy

#### FR-689: catch / throw — ✅ COMPLETE

- **実装**: `compile-ast` が `vm-establish-catch` / `vm-throw` を emit し、`catch` / `throw` の非局所脱出が動作する (`src/compile/codegen.lisp:54-84`)。
- **根拠**: ANSI CL 5.2 — catch/throw for non-local exits
- **難易度**: Low

#### FR-690: rotatef — ✅ COMPLETE

- **実装**: `rotatef` は `&rest places` を受け取り、2個以上の place を順に回転する (`src/expand/macros-stdlib.lisp:101-117`)。
- **根拠**: ANSI CL 5.1.2 — rotatef must accept N places using setf
- **難易度**: Low

#### FR-691: ignore-errors — ✅ COMPLETE

- **実装**: エラー時に `(values nil condition)` を返す (`src/expand/macros-stdlib.lisp:1115-1119`)。
- **根拠**: ANSI CL 9.1 — ignore-errors returns (values nil condition) on error
- **難易度**: Low

#### FR-696: &optional / &key supplied-p 変数 — codegen で破棄

- **対象**: `src/compile/codegen-functions.lisp` (lines 111-123)
- **現状**: `allocate-defaulting-params` は `(first param)` (名前) と `(second param)` (デフォルト値) のみ使用。`(third param)` (supplied-p シンボル) は無視される。パーサー (`macro.lisp:78-79, 100-101`) は `supplied-p` を正しく3要素リストに格納しているが、codegen でシレントに破棄。`(defun f (&optional (x 10 x-p)) x-p)` のような関数で `x-p` が未定義変数として実行時エラー
- **内容**: `allocate-defaulting-params` の `make-entry` コールバックに `supplied-p` を追加し、gensym で sentinel 比較: `(eq val *non-constant-default-sentinel*)` → `t` / `nil` で bound-p レジスタを初期化する
- **根拠**: ANSI CL 3.4.1 — Ordinary Lambda Lists, supplied-p parameters
- **難易度**: Medium

#### FR-695: loop downfrom / downto / above — 未実装

- **対象**: `src/expand/loop-parser.lisp` (lines 118-130)
- **現状**: `FROM` パーサーは `TO`, `BELOW`, `UPTO` のみ認識。`DOWNFROM`, `DOWNTO`, `ABOVE` を受け取ると `loop-unknown-for-keyword` エラーをシグナル (テスト `loop-macro-tests.lisp:923` で確認)
- **内容**: `(loop for x downfrom 10 to 1 collect x)` が `(10 9 8 7 6 5 4 3 2 1)` を返すよう実装。`DOWNFROM` → `:from` + 負の `:by` / `DOWNTO`/`ABOVE` → 境界チェック
- **根拠**: ANSI CL 6.1.1.3 — loop numeric stepping
- **難易度**: Medium

#### FR-694: ~/fn/ format 指示子 — VM クロージャ呼び出し不可

- **対象**: `src/vm/format.lisp` (line 123)
- **現状**: `format` は `(apply #'format nil fmt-str arg-vals)` でホスト CL に委譲。`~/cl-cc-user:my-fn/` のような指示子で参照する関数は SBCL の関数名前空間で解決される。ユーザーが `(defun my-fn ...)` で定義した関数はホスト SBCL の `fboundp` には存在しないため、`~/fn/` 指示子で呼び出せない
- **内容**: ユーザー定義関数を `~/fn/` で使う場合は、VM 名前空間からホスト CL の fmakunbound/defun に橋渡しするか、format を自前実装 (FR-389) する必要がある
- **根拠**: ANSI CL 22.3.5.4 — ~/function-name/ format directive
- **難易度**: Hard (FR-389 自前 format 実装が前提)

#### FR-693: incf / decf / push / pop — ✅ COMPLETE

- **実装**: `incf` / `decf` / `push` / `pop` は複合 place のサブフォームを gensym で保護する。
- **根拠**: ANSI CL 5.1.3 — modify macros must evaluate subforms exactly once
- **難易度**: Low

#### FR-692: restart-bind — 🔶 部分実装

- **現状**: `bindings` を動的 restart リストに追加して `invoke-restart` と連携するが、`unwind-protect` による完全な後始末や対話的機能はまだ不足している。
- **内容**: `restart-bind` はスタックにリスタートオブジェクトを `push` し、ボディ後に `pop` する `unwind-protect` ラッパーが必要
- **根拠**: ANSI CL 9.1.4 — restart-bind
- **難易度**: Hard (invoke-restart / find-restart の全システム実装が必要)

---

## 実装状況サマリー

| カテゴリ | ✅ 実装済み | 🔶 部分実装 | ❌ 未実装 |
|---------|:---:|:---:|:---:|
| 特殊形式 (Ch.3) | 13 | 7 | 1 |
| ラムダリスト | 6 | 1 | 5 |
| 評価・コンパイル API | 3 | 2 | 11 |
| 複合型指定子 | 10 | 4 | 2 |
| 型・クラス API | 1 | 4 | 4 |
| 制御フロー・等価 | 23 | 7 | 7 |
| LOOP | 14 | 0 | 4 |
| マッピング | 2 | 1 | 3 |
| CLOS | 5 | 10 | 15 |
| 構造体 | 3 | 1 | 4 |
| コンディション | 2 | 10 | 18 |
| シンボル | 6 | 1 | 8 |
| パッケージ | 0 | 4 | 9 |
| 数値 | 41 | 11 | 24 |
| 文字 | 9 | 4 | 3 |
| コンス・リスト | 19 | 4 | 17 |
| 配列 | 11 | 5 | 12 |
| 文字列 | 7 | 6 | 3 |
| シーケンス | 10 | 11 | 8 |
| ハッシュテーブル | 6 | 2 | 5 |
| ファイルシステム | 1 | 3 | 10 |
| ストリーム・I/O | 14 | 7 | 19 |
| プリンタ | 14 | 4 | 3 |
| リーダー | 6 | 1 | 12 |
| システム構成 | 1 | 3 | 5 |
| 標準変数 | 4 | 6 | 14 |
| 宣言・コンパイラ | 3 | 5 | 7 |
| 環境・開発ツール | 0 | 2 | 12 |
| モダン拡張 | 1 | 1 | 12 |
| コンス・リスト (追補) | 1 | 0 | 3 |
| 配列 (追補) | 1 | 1 | 2 |
| 文字・標準文字名 | 1 | 1 | 0 |
| リーダー (追補) | 2 | 0 | 5 |
| **合計** | **297** | **25** | **6** |

**カバレッジ: 90.5% (✅のみ) / 98.2% (🔶を含む)** (2026-03-28 更新: 実測カウント)
- ✅ 完全実装: **297 項目** (session 18 追加: string comparisons ANSI return values, *print-base*/*print-escape* wiring, write-to-string keywords, aref multi-dim, read-char/read/read-line/read-byte eof-value, require pathnames, equalp hash tables, room host delegation, trace host delegation, disassemble host delegation, dribble host delegation, inspect host delegation, step host delegation)
- 🔶 部分実装: **25 項目** (引数受け付け、スタブ、制限あり; `#n=`/`#n#` non-circular only; push/pop complex places)
- ❌ 未実装: **6 項目** (Gray Streams、CFFI、Threads、MOP、inline expansion、delimited continuations 等 — 全て大規模インフラ変更が必要)

### 領域別優先度

| 優先度 | 領域 | 理由 |
|--------|------|------|
| 🔴 高 | コンディション・リスタート (FR-356/418/420/421/424) | セルフホスティング品質に直結 |
| 🔴 高 | パッケージシステム (FR-437〜438) | 標準 CL コードの互換性 |
| 🟡 中 | `defstruct` 完全化 (FR-544〜546) | CLOS と統合してこそ完結 |
| 🟡 中 | `read-sequence`/`write-sequence` (FR-590) | バイナリ I/O の基盤 |
| 🟡 中 | 複合型指定子 (FR-581) | 型システム表現力 |
| 🟢 低 | 開発ツール (FR-431〜436) | 利便性向上 |
| 🟢 低 | Easy FR 群 (FR-543/563/578/609/610/611/614/615/620/621/622/623) | 1〜2時間で実装可能 |
| 🟡 中 | FR-607: docstrings 保存 (FR-436 基盤) | `documentation` GF の前提条件 |
| 🔴 高 | FR-605: bignum | 値表現の根幹変更; 長期投資 |

**概算カバレッジ: ~45% (コア言語・算術・FORMAT指示子は強い。`catch`/`throw` は実装済みで、`restart-bind` は部分実装。コンディション・パッケージ・複合型・開発ツール・モダン拡張が主な未実装領域)**
