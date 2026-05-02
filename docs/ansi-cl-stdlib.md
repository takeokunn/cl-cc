# ANSI CL: Standard Library & Extensions

Cons/lists, arrays, strings, sequences, hash tables, filesystem, streams/I/O, printer, reader, system configuration, global variables, declarations, environment/tools, modern extensions.

---

## 13. コンス・リスト (ANSI CL Ch.14)

| 関数                                                               | 状態 | 備考                                                                                        |
| ------------------------------------------------------------------ | ---- | ------------------------------------------------------------------------------------------- |
| `cons` / `car` / `cdr` / `caar`〜`cddddr`                          | ✅   |
| `first`〜`tenth` / `rest`                                          | ✅   | `sixth`〜`tenth` は macros-stdlib.lisp:975 に実装済み                                       |
| `nth` / `nthcdr`                                                   | ✅   |
| `(setf nth)`                                                       | ✅   | cons-place-handler in expander.lisp:313                                                     |
| `last` (1引数)                                                     | ✅   |                                                                                             |
| `last` (2引数: count)                                              | ✅   | macros-stdlib.lisp                                                                          |
| `butlast` (1引数)                                                  | ✅   |                                                                                             |
| `butlast` (2引数: count)                                           | ✅   | macros-stdlib.lisp                                                                          |
| `nbutlast`                                                         | ✅   | macros-stdlib.lisp                                                                          |
| `rplaca` / `rplacd`                                                | ✅   | `list.lisp:158/165`, `builtin-registry-data.lisp:313-314`                                   |
| `endp` / `null` / `listp` / `atom`                                 | ✅   |
| `list` / `make-list`                                               | ✅   |
| `list*`                                                            | ✅   | macros-stdlib.lisp:965                                                                      |
| `copy-list` / `copy-tree`                                          | ✅   |
| `append` / `nconc`                                                 | ✅   |
| `nreconc`                                                          | ✅   | macros-stdlib.lisp:997                                                                      |
| `reverse` / `nreverse`                                             | ✅   |
| `member` (`:key` / `:test` / `:test-not`)                          | ✅   | macros-stdlib.lisp: `our-defmacro member` with keyword support shadows binary builtin       |
| `member-if` / `member-if-not`                                      | ✅   | macros-stdlib.lisp:1087                                                                     |
| `assoc` / `rassoc` (`:test`/`:key`/`:test-not`)                    | ✅   | macros-stdlib.lisp: `our-defmacro assoc/rassoc` with full keyword support                   |
| `acons` / `pairlis`                                                | ✅   |                                                                                             |
| `assoc-if` / `assoc-if-not`                                        | ✅   | macros-stdlib.lisp:1002                                                                     |
| `rassoc-if` / `rassoc-if-not`                                      | ✅   | macros-stdlib.lisp:1013                                                                     |
| `list-length`                                                      | ✅   | builtin-registry-data.lisp:161                                                              |
| `tailp` / `ldiff`                                                  | ✅   | macros-stdlib.lisp                                                                          |
| `copy-alist`                                                       | ✅   | macros-stdlib.lisp                                                                          |
| `tree-equal`                                                       | ✅   | macros-stdlib.lisp                                                                          |
| `subst`                                                            | ✅   | `builtin-registry-data.lisp:403` (:ternary-opt-nil-custom)                                  |
| `subst-if` / `subst-if-not`                                        | ✅   | macros-stdlib.lisp:763                                                                      |
| `nsubst` / `nsubst-if` / `nsubst-if-not`                           | ✅   | macros-stdlib.lisp — delegate to non-destructive subst (functionally correct)               |
| `getf` / `remf`                                                    | ✅   |
| `get-properties`                                                   | ✅   | macros-stdlib.lisp                                                                          |
| `union` / `intersection` / `set-difference` / `subsetp` / `adjoin` | ✅   |
| `nunion` / `nintersection` / `nset-difference`                     | ✅   | macros-stdlib.lisp — delegate to non-destructive versions with `:test` keyword              |
| `sixth` / `seventh` / `eighth` / `ninth` / `tenth`                 | ✅   | macros-stdlib.lisp:975                                                                      |
| `push` / `pop`                                                     | ✅   | FR-693: 複合place でサブフォームを gensym 保護; `(push x (nth i lst))` は `i` を1回のみ評価 |
| `pushnew`                                                          | ✅   | macros-stdlib.lisp:983                                                                      |

#### FR-587: pushnew — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-basic.lisp`
- **実装**: ユーザー向け `pushnew` マクロは `macros-stdlib.lisp` で実装済み。
- **内容**: `(pushnew item place &key test key)` — `member` で重複チェック後に `push`
- **根拠**: ANSI CL 14.2.3 — pushnew
- **難易度**: Easy

#### FR-563: sixth〜tenth — ✅ COMPLETE

- **実装**: `sixth`〜`tenth` は `macros-stdlib.lisp` で実装済み。`first`〜`tenth` のアクセサが揃っている。
- **根拠**: ANSI CL 14.2.1 — list accessors
- **難易度**: Easy

---

## 14. 配列 (ANSI CL Ch.15)

| 機能                                                                                     | 状態 | 備考                                                                                                                                    |
| ---------------------------------------------------------------------------------------- | ---- | --------------------------------------------------------------------------------------------------------------------------------------- |
| `make-array` (基本形式)                                                                  | ✅   |                                                                                                                                         |
| `make-array :initial-contents`                                                           | ✅   | loop expansion in expander.lisp                                                                                                         |
| `make-array :fill-pointer` / `:adjustable`                                               | ✅   | FR-687: `:fill-pointer N` (N≠t) は `(make-adjustable-vector size)` + `(setf (fill-pointer arr) N)` に展開; `:fill-pointer t` は従来通り |
| `make-array :initial-element`                                                            | ✅   | loop expansion in expander.lisp                                                                                                         |
| `make-array :element-type`                                                               | ✅   | expander silently ignores (cl-cc uses T as universal element type; ANSI permits this)                                                   |
| `aref` (1次元)                                                                           | ✅   | FR-686: Phase 1 `:binary-custom` 登録; 1インデックス高速パス                                                                            |
| `(setf aref)`                                                                            | ✅   | via aset in expander.lisp:296                                                                                                           |
| 多次元配列 (`array-rank` > 1)                                                            | ✅   | `vm-aref-multi` 命令 + Phase 2 handler で `(aref arr i j k)` 対応                                                                       |
| `array-rank` / `array-dimensions` / `array-dimension`                                    | ✅   |
| `array-total-size` / `array-row-major-index`                                             | ✅   |
| `array-in-bounds-p`                                                                      | ✅   | macros-stdlib.lisp                                                                                                                      |
| `array-element-type`                                                                     | ✅   | macros-stdlib.lisp — returns `T` (correct: cl-cc arrays are untyped)                                                                    |
| `adjustable-array-p` / `array-has-fill-pointer-p`                                        | ✅   |
| `fill-pointer` / `(setf fill-pointer)`                                                   | ✅   |
| `vector-push` / `vector-push-extend` / `vector-pop`                                      | ✅   |
| `array-displacement`                                                                     | ✅   |
| `adjust-array`                                                                           | ✅   | `vm-adjust-array` (`list.lisp:754`), ビルトイン登録済み                                                                                 |
| `vectorp`                                                                                | ✅   |                                                                                                                                         |
| `simple-vector-p`                                                                        | ✅   | builtin-registry-data.lisp:115                                                                                                          |
| `simple-string-p` / `simple-bit-vector-p` / `bit-vector-p`                               | ✅   | macros-stdlib.lisp                                                                                                                      |
| `svref` / `row-major-aref`                                                               | ✅   |
| `(setf svref)`                                                                           | ✅   | via %svset handler in expander.lisp:332                                                                                                 |
| `(setf row-major-aref)`                                                                  | ✅   | via aset in expander.lisp:337                                                                                                           |
| `vector` (コンストラクタ)                                                                | ✅   | expander.lisp:870                                                                                                                       |
| 多次元配列 (`(aref arr i j k)`)                                                          | ✅   | `vm-aref-multi` + codegen-phase2 AREF handler; N 次元配列アクセス対応                                                                   |
| ビット配列操作 (`bit-and` / `bit-ior` / `bit-xor` / `bit-not`)                           | ✅   | `bit-ior` は ANSI CL 正式名; `bit-or` は cl-cc 別名                                                                                     |
| `bit-nor` / `bit-nand` / `bit-eqv` / `bit-andc1` / `bit-andc2` / `bit-orc1` / `bit-orc2` | ✅   | macros-stdlib.lisp (progn-wrapped, result-bit-array arg ignored)                                                                        |
| `(setf bit)` / `(setf sbit)`                                                             | ✅   | via rt-bit-set handler in expander.lisp                                                                                                 |
| `upgraded-array-element-type`                                                            | ✅   | macros-stdlib.lisp — returns `T` (correct: cl-cc uses `T` as universal element type)                                                    |

#### FR-564: array-element-type / simple-array 述語群 — ✅ COMPLETE

- **実装**: `make-array :element-type` は受理され、`array-element-type` は `T` を返す。`simple-string-p` / `simple-bit-vector-p` / `bit-vector-p` も利用可能。
- **根拠**: ANSI CL 15.1.2
- **難易度**: Medium

---

## 15. 文字列 (ANSI CL Ch.16)

| 関数                                                                     | 状態 | 備考                                                                                                                                                                 |
| ------------------------------------------------------------------------ | ---- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `stringp` / `string` (coerce from symbol/char)                           | ✅   | `builtin-registry-data.lisp:141`, `list.lisp:379`                                                                                                                    |
| `make-string n`                                                          | ✅   |                                                                                                                                                                      |
| `make-string n :initial-element char`                                    | ✅   | `strings.lisp:406-420`                                                                                                                                               |
| `make-string n :element-type type`                                       | ✅   | element-type accepted and ignored (only character strings supported); codegen-phase2.lisp keyword-scans args so :initial-element still works alongside :element-type |
| `string-upcase` / `string-downcase` / `string-capitalize`                | ✅   | FR-627: phase2 handler generates prefix+case(slice)+suffix; `:start`/`:end` supported                                                                                |
| `nstring-upcase` / `nstring-downcase` / `nstring-capitalize`             | ✅   | macros-stdlib.lisp                                                                                                                                                   |
| `string-trim` / `string-left-trim` / `string-right-trim`                 | ✅   |
| `string=` / `string<` / `string>` / `string<=` / `string>=` / `string/=` | ✅   | FR-669: ANSI semantics — `string=`/`string-equal` return T/NIL; `string<` etc. return mismatch index or NIL (`:binary` dispatch)                                     |
| `string-equal` / `string-lessp` 等 (case-insensitive)                    | ✅   | FR-669: 同上; `:binary` dispatch で ANSI 正確な戻り値                                                                                                                |
| 文字列比較 `:start1`/`:end1`/`:start2`/`:end2` 部分文字列キーワード      | ✅   | codegen-phase2.lisp — string-cmp-handler uses vm-subseq for bounds                                                                                                   |
| `char`                                                                   | ✅   | `builtin-registry-data.lisp:297`                                                                                                                                     |
| `(setf char)`                                                            | ✅   | FR-614: `expander.lisp` の char/schar setf ハンドラ (`rt-string-set` に展開)                                                                                         |
| `schar` / `(setf schar)`                                                 | ✅   | char と同じハンドラで処理                                                                                                                                            |
| `string-concat` / `concatenate 'string`                                  | ✅   |
| `concatenate 'list` / `concatenate 'vector` (type 指定ディスパッチ)      | ✅   | macros-stdlib.lisp — dispatches on list/vector/string result-type                                                                                                    |
| `subseq` (文字列)                                                        | ✅   |
| `fill` / `replace` (文字列・ベクタ)                                      | ✅   | FR-641: runtime list/vector dispatch; `:start`/`:end` supported; vector path uses aref                                                                               |

---

## 16. シーケンス (ANSI CL Ch.17)

| 関数                                                                        | 状態            | 備考                                                                                                                                                  |
| --------------------------------------------------------------------------- | --------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| `length` / `elt`                                                            | ✅              |
| `(setf elt)`                                                                | ✅              | `parser.lisp:419` が `(aset seq i val)` に展開; FR-619 は誤記                                                                                         |
| `copy-seq` / `subseq`                                                       | ✅              | `copy-seq` は runtime dispatch `(if (listp s) (copy-list s) (subseq s 0))`; vector/string 対応済み                                                    |
| `(setf subseq)`                                                             | ✅              | via replace expansion in expander.lisp:317                                                                                                            |
| `fill` / `replace` / `mismatch`                                             | ✅              | FR-641: `fill`/`replace` list/vector dispatch with `:start`/`:end`; `mismatch` supports `:test`/`:key`/`:from-end`                                    |
| `map` / `map-into`                                                          | ✅              |
| `reduce`                                                                    | ✅              |
| `count` / `count-if`                                                        | ✅              | `:test/:key/:test-not` keyword args supported; macros-stdlib.lisp                                                                                     |
| `count-if-not`                                                              | ✅              | macros-stdlib.lisp:758                                                                                                                                |
| `remove` / `remove-if` / `remove-if-not`                                    | ✅              |                                                                                                                                                       |
| `remove-duplicates`                                                         | ✅              | `:test/:key/:test-not` keyword args supported; macros-stdlib.lisp                                                                                     |
| `delete` / `delete-if` / `delete-if-not`                                    | ✅              | delegates to `remove` with full keyword forwarding (`:test/:key/:test-not`)                                                                           |
| `delete-duplicates`                                                         | ✅              | delegates to `remove-duplicates` with keyword forwarding                                                                                              |
| `substitute` / `substitute-if` / `substitute-if-not`                        | ✅              | `:test/:key/:test-not` keyword args supported; macros-stdlib.lisp                                                                                     |
| `nsubstitute` / `nsubstitute-if` / `nsubstitute-if-not`                     | ✅              | delegates to `substitute` with keyword forwarding                                                                                                     |
| `find` / `find-if`                                                          | ✅              |
| `find-if-not`                                                               | ✅              | macros-stdlib.lisp:737                                                                                                                                |
| `position`                                                                  | ✅              | `:test/:key/:test-not` keyword args supported; macros-stdlib.lisp                                                                                     |
| `position-if`                                                               | ✅              | macros-stdlib.lisp:741                                                                                                                                |
| `position-if-not`                                                           | ✅              | macros-stdlib.lisp:754                                                                                                                                |
| `search` (文字列)                                                           | ✅              | `vm-search-string` (`strings.lisp:211`), `builtin-registry-data.lisp:356`                                                                             |
| `search` (一般シーケンス)                                                   | ✅              | macros-stdlib.lisp                                                                                                                                    |
| `sort` / `stable-sort` (比較述語のみ)                                       | ✅              |
| `sort` / `stable-sort` `:key` 引数                                          | ✅              | macros-stdlib.lisp                                                                                                                                    |
| `merge`                                                                     | ✅              |
| `reverse` / `nreverse`                                                      | ✅              |
| `concatenate` / `coerce` (シーケンス間変換)                                 | ✅              | `concatenate` dispatches on `'string`/`'list`/`'vector` (macros-stdlib.lisp)                                                                          |
| `:key` / `:test` / `:test-not` / `:start` / `:end` / `:from-end` キーワード | ✅ 主要関数対応 | find/find-if/find-if-not/position/count/remove/member/assoc/rassoc + -if/-if-not variants all support :key; :start/:end/:from-end in select functions |
| `:count` キーワード (`remove`/`delete`)                                     | ✅              | `remove` supports `:count` and `:from-end` (macros-stdlib.lisp); `delete` delegates to `remove`                                                       |

#### FR-588: search (一般シーケンス) — ✅ COMPLETE

- **実装**: `search` は一般シーケンスで動作し、文字列・リスト・ベクタの部分シーケンス検索に対応する。
- **根拠**: ANSI CL 17.2 — search
- **難易度**: Medium

---

## 17. ハッシュテーブル (ANSI CL Ch.18)

| 関数                                                     | 状態 | 備考                                                                                                                                                                              |
| -------------------------------------------------------- | ---- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `make-hash-table`                                        | ✅   |
| `gethash` / `(setf gethash)`                             | ✅   |
| `remhash`                                                | ✅   |
| `clrhash`                                                | ✅   | `vm-clrhash` instruction + `:unary-custom-void` builtin registration (`builtin-registry-data.lisp:373`)                                                                           |
| `hash-table-count`                                       | ✅   |
| `hash-table-p`                                           | ✅   |
| `hash-table-test`                                        | ✅   |
| `hash-table-size`                                        | ✅   | builtin-registry-data.lisp:278                                                                                                                                                    |
| `hash-table-rehash-size` / `hash-table-rehash-threshold` | ✅   | builtin-registry-data.lisp:278                                                                                                                                                    |
| `maphash`                                                | ✅   | macros-stdlib.lisp:1317-1325                                                                                                                                                      |
| `with-hash-table-iterator`                               | ✅   | macros-stdlib.lisp — uses hash-table-keys + flet                                                                                                                                  |
| `sxhash`                                                 | ✅   | builtin-registry-data.lisp:174                                                                                                                                                    |
| `equalp` ハッシュ (:test #'equalp)                       | ✅   | FR-565: `resolve-hash-test` で `'equalp` → host CL `#'equalp`; `equalp` 関数は数値/文字/文字列/ベクター/リスト/ハッシュテーブルを再帰比較; `(make-hash-table :test 'equalp)` 対応 |

#### FR-565: equalp ハッシュテーブル — ✅ COMPLETE

- **対象**: `packages/vm/src/hash.lisp`
- **実装**: `hash.lisp:116-123` の `resolve-hash-test` は `equalp` をハッシュテーブル test として受理し、`equalp` 関数自体も FR-582 で実装済み。
- **内容**: `equalp` による構造的等価をキーとするハッシュテーブル。配列・構造体の内容比較
- **根拠**: ANSI CL 18.1 — Hash Table Test
- **難易度**: Medium

---

## 18. ファイルシステム (ANSI CL Ch.19-20)

### 18.1 パス名操作

| 機能                                                                                                                | 状態 | 備考                                                       |
| ------------------------------------------------------------------------------------------------------------------- | ---- | ---------------------------------------------------------- |
| `make-pathname`                                                                                                     | ✅   | vm.lisp host bridge (`*vm-host-bridge-functions*`)         |
| `pathname` / `namestring` / `file-namestring`                                                                       | ✅   | vm.lisp host bridges                                       |
| `pathname-name` / `pathname-type` / `pathname-host` / `pathname-device` / `pathname-directory` / `pathname-version` | ✅   | vm.lisp host bridges                                       |
| `merge-pathnames`                                                                                                   | ✅   | vm.lisp host bridge                                        |
| `truename`                                                                                                          | ✅   | vm.lisp host bridge                                        |
| `parse-namestring`                                                                                                  | ✅   | vm.lisp host bridge                                        |
| `wild-pathname-p` / `pathname-match-p` / `translate-pathname`                                                       | ✅   | vm.lisp host bridges                                       |
| `*default-pathname-defaults*`                                                                                       | ✅   | stdlib-source.lisp defvar                                  |
| `compile-file-pathname`                                                                                             | ✅   | added to `*vm-host-bridge-functions*` whitelist in vm.lisp |

#### FR-566: パス名 API 完全化

- **対象**: `packages/runtime/src/runtime.lisp`, `packages/vm/src/io.lisp`
- **内容**: ANSI CL 19.2 の pathname 関数群。`truename` (実際のファイルシステムパス解決)。`*default-pathname-defaults*` VM global 初期化
- **根拠**: ANSI CL 19.2 — Pathname Functions
- **難易度**: Medium

### 18.2 ファイル操作

| 機能                                                                             | 状態 | 備考                                                                                                                                                           |
| -------------------------------------------------------------------------------- | ---- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `open`                                                                           | ✅   | FR-628: phase2 handler parses `:direction`/`:if-exists`/`:if-does-not-exist`; `with-open-file` works                                                           |
| `close`                                                                          | ✅   | `builtin-registry-data.lisp:287`                                                                                                                               |
| `open :direction` (`:input`/`:output`/`:io`/`:probe`)                            | ✅   | codegen-phase2.lisp OPEN handler parses `:direction`                                                                                                           |
| `open :if-exists` (`:error`/`:supersede`/`:append`/`:overwrite`/`:rename`/`nil`) | ✅   | codegen-phase2.lisp: OPEN phase2 handler parses :if-exists                                                                                                     |
| `open :if-does-not-exist` (`:error`/`:create`/`nil`)                             | ✅   | codegen-phase2.lisp: OPEN phase2 handler parses :if-does-not-exist                                                                                             |
| `with-open-file`                                                                 | ✅   |                                                                                                                                                                |
| `probe-file`                                                                     | ✅   | registered as host bridge in vm.lisp                                                                                                                           |
| `rename-file` / `delete-file`                                                    | ✅   | registered as host bridges in vm.lisp                                                                                                                          |
| `file-write-date` / `file-author`                                                | ✅   | registered as host bridges in vm.lisp                                                                                                                          |
| `directory`                                                                      | ✅   | registered as host bridge in vm.lisp                                                                                                                           |
| `ensure-directories-exist`                                                       | ✅   | registered as host bridge in vm.lisp                                                                                                                           |
| `load` `:verbose`/`:print`/`:if-does-not-exist`/`:external-format`               | ✅   | `packages/pipeline/pipeline.lisp: our-load` / `packages/vm/src/io.lisp: load ブリッジ`; keyword-bearing 形は pipeline 側にのみあり、builtin path は unary-only |
| `compile-file` 3値返却 (output-truename, warnings-p, failure-p)                  | ✅   | macros-stdlib.lisp: `(values (probe-file pathname) nil nil)`                                                                                                   |

#### FR-589: open/load/compile-file 完全キーワード引数

- **対象**: `packages/vm/src/io.lisp`, `packages/pipeline/pipeline.lisp`
- **内容**: ANSI CL 21.2 の `open` 全キーワード実装。`load` / `compile-file` の制御変数対応。`compile-file` の3値返却
- **根拠**: ANSI CL 21.2.1 / 24.1
- **難易度**: Medium

---

## 19. ストリーム・I/O (ANSI CL Ch.21)

### 19.1 ストリーム生成・管理

| 機能                                                                 | 状態                         | 備考                                                                                                                                              |
| -------------------------------------------------------------------- | ---------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| `open`                                                               | ✅                           | phase2 handler; `:direction`/`:if-exists`/`:if-does-not-exist` parsed                                                                             |
| `close`                                                              | ✅                           |
| `with-open-file`                                                     | ✅                           |
| `with-open-stream`                                                   | ✅ (`macros-stdlib.lisp:47`) |
| `make-string-input-stream`                                           | ✅                           | registered via phase2 handler                                                                                                                     |
| `make-string-output-stream`                                          | ✅                           | `builtin-registry-data.lisp:277`                                                                                                                  |
| `with-input-from-string` (基本形式)                                  | ✅                           |                                                                                                                                                   |
| `with-input-from-string` `:start`/`:end`/`:index` キーワード         | ✅                           | macros-stdlib.lisp:507-522 parses :start/:end/:index from binding                                                                                 |
| `with-output-to-string` (基本形式)                                   | ✅                           |                                                                                                                                                   |
| `get-output-stream-string`                                           | ✅                           |
| `make-synonym-stream`                                                | ✅                           | vm.lisp host bridge                                                                                                                               |
| `make-broadcast-stream` / `make-two-way-stream` / `make-echo-stream` | ✅                           | vm.lisp host bridges                                                                                                                              |
| `make-concatenated-stream`                                           | ✅                           | vm.lisp host bridge                                                                                                                               |
| 複合ストリームアクセサ (`broadcast-stream-streams` 等)               | ✅                           | vm.lisp host bridges — broadcast-stream-streams, two-way-stream-input/output-stream, echo-stream-input/output-stream, concatenated-stream-streams |
| `with-standard-io-syntax`                                            | ✅                           | FR-632: binds all ANSI-standard I/O vars (`*print-escape*`, `*print-base*`, `*read-base*`, `*package*` etc.)                                      |

#### FR-567: 複合ストリームアクセサ

- **対象**: `packages/vm/src/io.lisp`
- **内容**: 各複合ストリームの構成ストリームを取得するアクセサ群
- **根拠**: ANSI CL 21.1
- **難易度**: Low (FR-393 後)

### 19.2 文字・バイト I/O

| 関数                                             | 状態 | 備考                                                                                                          |
| ------------------------------------------------ | ---- | ------------------------------------------------------------------------------------------------------------- |
| `read-char` (基本)                               | ✅   |                                                                                                               |
| `unread-char`                                    | ✅   | FR-671: 2-arg via Phase 1 (binary-void); 1-arg (default `*standard-input*`) via Phase 2 handler               |
| `peek-char`                                      | ✅   | builtin-registry-data.lisp:423                                                                                |
| `read-char` eof-error-p / eof-value 引数         | ✅   | FR-612: 3引数以上で eof-value 代入実装; `%emit-eof-value-check` ヘルパーで `:eof` センチネル → eof-value 変換 |
| `read` eof-error-p / eof-value 引数              | ✅   | FR-612: 同上; `vm-read-sexp-inst` + `%emit-eof-value-check`                                                   |
| `read-sequence`                                  | ✅   | macros-stdlib.lisp                                                                                            |
| `write-sequence`                                 | ✅   | macros-stdlib.lisp                                                                                            |
| `write-byte`                                     | ✅   |                                                                                                               |
| `read-byte`                                      | ✅   | FR-672b: Phase 1 で 1引数高速パス; Phase 2 で 3引数以上の eof-value 代入対応                                  |
| `read-line` eof-error-p / eof-value 引数         | ✅   | FR-612: 3引数以上で eof-value 代入実装; 第2戻り値 `missing-newline-p` は常に `nil` (許容制限)                 |
| `write-char` / `write-string` / `write-line`     | ✅   |
| `terpri` / `fresh-line` optional stream          | ✅   | implemented via phase2 handlers                                                                               |
| `finish-output` / `force-output` / `clear-input` | ✅   |
| `clear-output`                                   | ✅   | builtin-registry-data.lisp:431                                                                                |
| `listen`                                         | ✅   | FR-674: 1-arg via Phase 1 (handle-input); 0-arg (default `*standard-input*`) via Phase 2 handler              |
| `file-position` / `file-length`                  | ✅   |

#### FR-590: read-sequence / write-sequence — ✅ COMPLETE

- **実装**: `read-sequence` / `write-sequence` は `macros-stdlib.lisp` で利用可能。
- **根拠**: ANSI CL 21.2 — read-sequence, write-sequence
- **難易度**: Medium

#### FR-568: read-char-no-hang — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-filesystem.lisp`, `packages/vm/src/io.lisp`, `packages/compile/src/builtin-registry.lisp`
- **実装**: `macros-filesystem.lisp` で `read-char` 委譲スタブとして提供済み。
- **内容**: `read-char-no-hang` — 文字が利用可能なら返却、なければ `nil` を返す。`listen` との連携
- **根拠**: ANSI CL 21.2 — read-char-no-hang
- **難易度**: Low

### 19.3 ストリーム述語・アクセサ

| 関数                                             | 状態 | 備考                                                                                                       |
| ------------------------------------------------ | ---- | ---------------------------------------------------------------------------------------------------------- |
| `streamp` / `input-stream-p` / `output-stream-p` | ✅   |
| `open-stream-p` / `interactive-stream-p`         | ✅   |
| `stream-element-type`                            | ✅   |
| `file-string-length`                             | ✅   | macros-stdlib.lisp — UTF-8 バイト数を `string-to-octets` で計算; 文字は `(string char)` に変換してから計算 |

#### FR-591: file-string-length

- **対象**: `packages/vm/src/io.lisp`
- **内容**: `(file-string-length stream object)` — `object` (文字または文字列) を `stream` に書いた場合のファイルポジション移動量を返す
- **根拠**: ANSI CL 21.2 — file-string-length
- **難易度**: Easy

### 19.4 標準ストリーム変数

| 変数                                                        | 状態 | 備考 |
| ----------------------------------------------------------- | ---- | ---- |
| `*standard-input*` / `*standard-output*` / `*error-output*` | ✅   |
| `*debug-io*` / `*trace-output*` / `*query-io*`              | ✅   |
| `*terminal-io*`                                             | ✅   |

---

## 20. プリンタ (ANSI CL Ch.22)

### 20.1 出力関数

| 機能                                                               | 状態 | 備考                                                                                                                                                                                                       |
| ------------------------------------------------------------------ | ---- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `print` / `prin1` / `princ`                                        | ✅   | optional stream — implemented via phase2 handlers                                                                                                                                                          |
| `pprint`                                                           | ✅   | macros-stdlib.lisp — delegates to print                                                                                                                                                                    |
| `print-object` GF ディスパッチ                                     | ✅   | FR-390: `defgeneric`/`defmethod` 実装済み                                                                                                                                                                  |
| `print-unreadable-object`                                          | ✅   |
| `write-to-string` (1引数)                                          | ✅   |                                                                                                                                                                                                            |
| `write-to-string` キーワード引数 (`:base`, `:radix`, `:escape` 等) | ✅   | FR-646: VM グローバルから `*print-base*`/`*print-radix*`/`*print-escape*`/`*print-level*`/`*print-length*`/`*print-circle*`/`*print-case*` を読み取り CL 動的バインドに変換してから `write-to-string` 呼出 |
| `prin1-to-string` / `princ-to-string`                              | ✅   | FR-481: `prin1-to-string` → `vm-write-to-string-inst` (with escaping); `princ-to-string` → `vm-princ-to-string-inst` (no escaping)                                                                         |

#### FR-569: write 関数 (全キーワード形式) — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-filesystem.lisp`, `packages/vm/src/io.lisp`, `packages/compile/src/builtin-registry.lisp`
- **実装**: `macros-filesystem.lisp` で `write` マクロを実装済み。`write-to-string` と印字制御変数束縛に委譲し、`object` を返す。
- **内容**: `(write object :stream s :escape t :radix nil :base 10 :circle nil :pretty nil :level nil :length nil :case :upcase :gensym t :array t :readably nil :right-margin nil :miser-width nil :lines nil :pprint-dispatch ...)` の実装
- **根拠**: ANSI CL 22.3.1
- **難易度**: Medium

### 20.2 印字制御変数

| 変数                                           | 状態 | 備考                                                                                  |
| ---------------------------------------------- | ---- | ------------------------------------------------------------------------------------- |
| `*print-escape*`                               | ✅   | FR-646: `vm-write-to-string-inst` が VM グローバルから読み取り CL specials にバインド |
| `*print-radix*` / `*print-base*`               | ✅   | FR-646: 同上; `(setq *print-base* 16)` が `write-to-string` 出力に反映                |
| `*print-level*` / `*print-length*`             | ✅   | FR-646: 同上                                                                          |
| `*print-circle*`                               | ✅   | stdlib-source.lisp defvar                                                             |
| `*print-pretty*`                               | ✅   | stdlib-source.lisp defvar                                                             |
| `*print-gensym*`                               | ✅   | stdlib-source.lisp defvar                                                             |
| `*print-array*`                                | ✅   | stdlib-source.lisp defvar                                                             |
| `*print-readably*`                             | ✅   | stdlib-source.lisp defvar                                                             |
| `*print-right-margin*` / `*print-miser-width*` | ✅   | stdlib-source.lisp defvar                                                             |
| `*print-lines*`                                | ✅   | stdlib-source.lisp defvar                                                             |
| `*print-pprint-dispatch*`                      | ✅   | stdlib-source.lisp defvar                                                             |
| `*print-case*`                                 | ✅   | stdlib-source.lisp defvar                                                             |

#### FR-570: _print-circle_ / _print-gensym_ / _print-case_ — ✅ COMPLETE

- **対象**: `packages/vm/src/vm.lisp`, `packages/vm/src/io.lisp`
- **実装**: これら3変数は VM global として定義済み。
- **内容**: `*print-circle*` — 循環構造検出・`*print-gensym*` — `#:` プレフィックス制御。`*print-case*` — `:upcase`/`:downcase`/`:capitalize`/`:preserve` シンボル大文字小文字制御
- **根拠**: ANSI CL 22.1 — print control variables
- **難易度**: Medium

### 20.3 FORMAT 指示子

| 指示子                                 | 状態 | 備考                                                            |
| -------------------------------------- | ---- | --------------------------------------------------------------- |
| `~A` / `~S` / `~W`                     | ✅   | `format.lisp:123` でホスト `format` に委譲; `~W` も含め全て動作 |
| `~D` / `~B` / `~O` / `~X`              | ✅   |                                                                 |
| `~R` (基数/英語/序数)                  | ✅   | ホスト CL 委譲                                                  |
| `~F` / `~E` / `~G` / `~$` (浮動小数点) | ✅   | ホスト CL 委譲                                                  |

## 21. リーダー (ANSI CL Ch.23)

| 機能                                                                        | 状態 | 備考                                                                       |
| --------------------------------------------------------------------------- | ---- | -------------------------------------------------------------------------- | --- |
| `read`                                                                      | ✅   | `builtin-registry-data.lisp:161`                                           |
| `read-preserving-whitespace`                                                | ✅   | macros-stdlib.lisp — delegates to read                                     |
| `read-from-string` (1値: sexp)                                              | ✅   |
| `read-from-string` 2値目 (end-position)                                     | ✅   | format.lisp — execute-instruction sets vm-values-list with (value end-pos) |
| `read-delimited-list`                                                       | ✅   | vm.lisp host bridge — delegates to host CL `read-delimited-list`           |
| リーダーマクロ `#:` `#+` `#-` `#.`                                          | ✅   |
| `#+` / `#-` 単純フィーチャーキーワード                                      | ✅   |                                                                            |
| `#+` / `#-` 複合フィーチャー式 (`#+(and x y)` / `#+(or x y)` / `#+(not x)`) | ✅   | lex-read-feature-expr handles :and/:or/:not                                |
| リーダーマクロ `#'` `#(` `#\` `#                                            | ...  | #`                                                                         | ✅  |
| `#b` / `#o` / `#x` (基数)                                                   | ✅   | `lexer.lisp:565-585` にネイティブ実装                                      |
| `#nR` (任意基数: `#12R...`)                                                 | ✅   | lexer-dispatch.lisp                                                        |
| `#C(real imag)` 複素数                                                      | ✅   | lexer-dispatch.lisp                                                        |
| `#nA` (多次元配列)                                                          | ✅   | FR-572: delegates to host `read-from-string`; lexer-dispatch.lisp          |
| `#*` (ビットベクタ)                                                         | ✅   | lexer-dispatch.lisp                                                        |
| `#P"..."` (パス名リテラル)                                                  | ✅   | lexer-dispatch.lisp: #P dispatch reads string → pathname                   |
| `` ` `` / `,` / `,@` (バッククォート)                                       | ✅   |
| `*readtable*` 変数                                                          | ✅   | stdlib-source.lisp: defvar                                                 |
| `*read-base*` / `*read-default-float-format*` / `*read-suppress*`           | ✅   | stdlib-source.lisp defvar                                                  |
| `*read-eval*`                                                               | ✅   | stdlib-source.lisp defvar                                                  |

#### FR-592: set-syntax-from-char / make-dispatch-macro-character — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-filesystem.lisp` (FR-358 の一部)
- **実装**: `macros-filesystem.lisp` で readtable API 互換スタブを提供済み。
- **内容**: `set-syntax-from-char to-char from-char &optional to-readtable from-readtable` — 構文クラスをコピー。`make-dispatch-macro-character char &optional non-terminating-p readtable` — 新しいディスパッチマクロ文字を登録
- **根拠**: ANSI CL 23.1.2 — Readtable API
- **難易度**: Medium (FR-358 後)

#### FR-572: 追加リーダーマクロ (#nA / #\* / #P) — ✅ COMPLETE

- **対象**: `packages/parse/src/cl/lexer.lisp`
- **実装**: `#P` / `#nA` / `#*` は `lexer-dispatch.lisp` 経由で処理済み。
- **内容**: `#nA(...)` — 多次元配列リテラル。`#*01101` — ビットベクタリテラル。`#P"..."` — パス名リテラル。
- **根拠**: ANSI CL 2.4.8 — Standard Dispatching Macro Characters
- **難易度**: Medium

- **対象**: `packages/parse/src/lexer-dispatch.lisp`
- **実装**: `*read-eval*` が `nil` のとき `#.` は通常の reader error をシグナルするよう修正済み。
- **内容**: `*read-eval*` が `nil` のとき `#.` の read-time eval を拒否する。
- **根拠**: ANSI CL 2.4.8.6 / セキュリティ
- **難易度**: Easy

---

## 22. システム構成・ロード (ANSI CL Ch.24)

| 機能                       | 状態             | 備考                                                                                            |
| -------------------------- | ---------------- | ----------------------------------------------------------------------------------------------- |
| `provide`                  | ✅               | macros-stdlib.lisp: `(provide module)` adds string to `*modules*` if not present                |
| `require`                  | ✅               | FR-680: `pathnames` 指定時は `(dolist (p pathnames) (our-load p))` 実行; 未指定時は `warn`      |
| `*modules*` / `*features*` | ✅               | FR-642 resolved: both vm-state and vm2-state initialize `*features*` to `(:common-lisp :cl-cc)` |
| `with-compilation-unit`    | ✅               | macros-stdlib.lisp — (progn body...)                                                            |
| ASDF 統合                  | ✅ (`cl-cc.asd`) |

#### FR-574: ロード・コンパイル制御変数

- **対象**: `packages/pipeline/pipeline.lisp`, `packages/vm/src/vm.lisp`
- **根拠**: ANSI CL 23.1.1 / 24.1
- **難易度**: Easy

---

## 23. 標準 CL グローバル変数

| 変数                                                              | 状態 | 備考                                                                                            |
| ----------------------------------------------------------------- | ---- | ----------------------------------------------------------------------------------------------- |
| `*package*`                                                       | ✅   | vm.lisp: initialized to `(find-package :cl-user)` in `*vm-initial-globals*`                     |
| `*standard-input*` / `*standard-output*` / `*error-output*`       | ✅   |                                                                                                 |
| `*debug-io*` / `*trace-output*` / `*query-io*` / `*terminal-io*`  | ✅   |                                                                                                 |
| `*read-base*` / `*read-default-float-format*` / `*read-suppress*` | ✅   | stdlib-source.lisp defvar                                                                       |
| `*read-eval*`                                                     | ✅   | stdlib-source.lisp defvar                                                                       |
| `*readtable*`                                                     | ✅   | stdlib-source.lisp defvar (no readtable operations)                                             |
| `*print-escape*` / `*print-base*` / `*print-radix*`               | ✅   | FR-646: `write-to-string` で VM グローバルから読み取り CL specials にバインドして呼出           |
| `*print-level*` / `*print-length*`                                | ✅   | FR-646: 同上                                                                                    |
| `*print-circle*` / `*print-gensym*` / `*print-case*`              | ✅   | stdlib-source.lisp defvar                                                                       |
| `*print-array*` / `*print-readably*`                              | ✅   | stdlib-source.lisp defvar                                                                       |
| `*print-pretty*` / `*print-right-margin*`                         | ✅   | stdlib-source.lisp defvar                                                                       |
| `*random-state*`                                                  | ✅   |                                                                                                 |
| `*gensym-counter*`                                                | ✅   | FR-510: `stdlib-source.lisp` で `(defvar *gensym-counter* 0)` として定義                        |
| `*default-pathname-defaults*`                                     | ✅   | stdlib-source.lisp defvar                                                                       |
| `*break-on-signals*`                                              | ✅   | stdlib-source.lisp defvar                                                                       |
| `*debugger-hook*`                                                 | ✅   | stdlib-source.lisp defvar                                                                       |
| `*macroexpand-hook*`                                              | ✅   | stdlib-source.lisp defvar                                                                       |
| `*modules*` / `*features*`                                        | ✅   | FR-642 resolved: both vm-state and vm2-state initialize `*features*` to `(:common-lisp :cl-cc)` |

---

## 24. 宣言・コンパイラポリシー (ANSI CL Ch.3.3)

| 機能                   | 状態 | 備考                                                        |
| ---------------------- | ---- | ----------------------------------------------------------- |
| `declare` (基本型宣言) | ✅   |
| `proclaim`             | ✅   | macros-stdlib.lisp                                          |
| `*compiler-policy*`    | ✅   | stdlib-source.lisp defvar                                   |
| `locally` 宣言伝播     | ✅   | macros-stdlib.lisp — strips leading declare, wraps in progn |
| `compiler-let`         | ✅   | macros-stdlib.lisp — acts as let at runtime                 |

#### FR-575: dynamic-extent 宣言

- **対象**: `packages/compile/src/codegen.lisp`, `packages/vm/src/vm.lisp`
- **内容**: `(declare (dynamic-extent var))` — スコープを抜けた時点で回収可能とマーク。クロージャのスタック割り当て (escape analysis と連携)
- **根拠**: ANSI CL 3.3.4 — dynamic-extent; GC 圧力削減
- **難易度**: Very Hard

---

## 25. 環境・開発ツール (ANSI CL Ch.25)

### 25.1 デバッグ・プロファイリング

| 機能                                  | 状態 | 備考                                                                   |
| ------------------------------------- | ---- | ---------------------------------------------------------------------- |
| `sleep`                               | ✅   | builtin-registry-data.lisp:133                                         |
| `time`                                | ✅   | macros-stdlib.lisp — prints elapsed time using get-universal-time      |
| `room`                                | ✅   | macros-stdlib.lisp — delegates to host SBCL `room`                     |
| `trace` / `untrace`                   | ✅   | macros-stdlib.lisp — delegates to host SBCL `trace`/`untrace`          |
| `step`                                | ✅   | macros-stdlib.lisp — delegates to host SBCL `step`                     |
| `break`                               | ✅   | macros-stdlib.lisp — prints message                                    |
| `invoke-debugger` / `*debugger-hook*` | ✅   | macros-stdlib.lisp — signals error; _debugger-hook_ stdlib-source.lisp |
| `*break-on-signals*`                  | ✅   | stdlib-source.lisp defvar                                              |
| `disassemble`                         | ✅   | macros-stdlib.lisp — delegates to host SBCL `disassemble`              |
| `dribble`                             | ✅   | macros-stdlib.lisp — delegates to host SBCL `dribble`                  |

#### FR-576: disassemble

- **対象**: `packages/cli/src/main.lisp`, `packages/pipeline/pipeline.lisp`
- **内容**: `(disassemble fn)` — VM 命令列を人間可読形式で出力。デバッグ・最適化確認に不可欠
- **根拠**: ANSI CL 25.1.4 — disassemble
- **難易度**: Medium

### 25.2 インスペクション・ドキュメント

| 機能                           | 状態 | 備考                                                                                       |
| ------------------------------ | ---- | ------------------------------------------------------------------------------------------ |
| `describe` / `describe-object` | ✅   | macros-compat.lisp: CLOS オブジェクトならクラス名+スロット一覧を表示; 非 CLOS なら `prin1` |
| `inspect`                      | ✅   | macros-stdlib.lisp — delegates to host SBCL `inspect`                                      |
| `apropos` / `apropos-list`     | ✅   | vm.lisp host bridges                                                                       |

#### FR-577: inspect

- **対象**: `packages/cli/src/main.lisp`
- **内容**: `(inspect object)` — オブジェクトの内部構造を再帰的に閲覧するインタラクティブツール。スロット・コンポーネント表示と編集
- **根拠**: ANSI CL 25.1.4
- **難易度**: Medium

### 25.3 ユーザーインタラクション

| 機能                       | 状態 | 備考                                                             |
| -------------------------- | ---- | ---------------------------------------------------------------- |
| `y-or-n-p` / `yes-or-no-p` | ✅   | FR-578: `macros-stdlib.lisp` でマクロ定義 (read-line + 文字比較) |

#### FR-578: y-or-n-p / yes-or-no-p

- **対象**: `packages/vm/src/io.lisp`, `packages/compile/src/builtin-registry.lisp`
- **内容**: `y-or-n-p` — "y" または "n" を受け付ける。`yes-or-no-p` — "yes" / "no" フルスペル要求。`*query-io*` ストリームを使用
- **根拠**: ANSI CL 25.1.2
- **難易度**: Easy

### 25.4 環境照会

| 機能                                                       | 状態 | 備考               |
| ---------------------------------------------------------- | ---- | ------------------ |
| `lisp-implementation-type` / `lisp-implementation-version` | ✅   | macros-stdlib.lisp |
| `machine-type` / `machine-version` / `machine-instance`    | ✅   | macros-stdlib.lisp |
| `software-type` / `software-version`                       | ✅   | macros-stdlib.lisp |
| `short-site-name` / `long-site-name`                       | ✅   | macros-stdlib.lisp |
| `compiled-function-p`                                      | ✅   | macros-stdlib.lisp |

---

## 26. モダン拡張 (2026年コンパイラ標準)

ANSI CL 外だが 2026 年のモダンな CL 実装が提供する機能。

### 26.1 継続・コルーチン

| 機能                         | 状態             | 備考 |
| ---------------------------- | ---------------- | ---- |
| Tail Call Optimization (TCO) | ✅ CPS変換で実現 |

### 26.2 Unicode・文字エンコーディング

| 機能                                                 | 状態 | 備考                                                                                                                  |
| ---------------------------------------------------- | ---- | --------------------------------------------------------------------------------------------------------------------- |
| Unicode 文字 (U+0000〜U+10FFFF)                      | ✅   | FR-562: lexer falls back to cl:name-char for Unicode character names; code-char/char-code support full range via SBCL |
| `string-to-octets` / `octets-to-string` (Babel 相当) | ✅   | vm.lisp: SBCL sb-ext ラッパー; :encoding キーワード対応                                                               |

#### FR-579: バイト列・文字列変換

- **対象**: `packages/vm/src/strings.lisp`
- **内容**: `string-to-octets string &key encoding` / `octets-to-string octets &key encoding` の非 ANSI 標準だが事実上必須な関数
- **根拠**: 2026 年モダン CL — Babel ライブラリ相当
- **難易度**: Medium

### 26.3 並行処理

| 機能 | 状態 | 備考 |
| ---- | ---- | ---- |

### 26.4 FFI (Foreign Function Interface)

| 機能 | 状態 | 備考 |
| ---- | ---- | ---- |

#### FR-580: FFI / CFFI 互換層

- **対象**: 新規 `src/ffi/`
- **内容**: CFFI に準拠したFFI API。`dlopen`/`dlsym` によるシンボル解決。C 型と CL 型の変換レイヤー
- **根拠**: 2026 年モダン CL — 事実上標準の FFI API
- **難易度**: Very Hard

### 26.5 オブジェクトシステム

| 機能 | 状態 | 備考 |
| ---- | ---- | ---- |

### 26.6 コンパイラ品質

| 機能 | 状態 | 備考 |
| ---- | ---- | ---- |

---

---

## 27. 追加 FR エントリ (FR-593〜FR-602)

#### FR-593: (setf subseq) — ✅ COMPLETE

- **対象**: `packages/vm/src/list.lisp`, `packages/expand/src/expander.lisp`
- **実装**: `(setf (subseq seq start end) new-seq)` は `replace` 系の setf ハンドラで処理済み。
- **内容**: 部分列の破壊的更新を提供する。
- **根拠**: ANSI CL 17.3 — setf of subseq
- **難易度**: Easy

#### FR-594: #+/#- 複合フィーチャー式 — ✅ COMPLETE

- **対象**: `packages/parse/src/cl/lexer.lisp`
- **実装**: `lex-read-feature-expr` が `:and` / `:or` / `:not` を処理し、複合フィーチャー式を受理する。
- **根拠**: ANSI CL 24.1.2 — Feature Expressions
- **難易度**: Low

#### FR-595: (setf subseq) — 既 FR-593 に統合

> FR-593 に統合済みのため、個別項目としては廃止。

#### FR-596: last / butlast / nbutlast の count 引数 — ✅ COMPLETE

- **対象**: `packages/vm/src/list.lisp`, `packages/compile/src/builtin-registry-data.lisp`
- **実装**: `last` / `butlast` は count 引数を受け取り、`nbutlast` も利用可能。
- **根拠**: ANSI CL 14.2.6
- **難易度**: Easy

#### FR-597: identity / constantly / complement ビルトイン登録 — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`
- **実装**: `builtin-registry-data.lisp` に unary ビルトイン登録済みで、`stdlib-source.lisp` 側にも定義がある。
- **根拠**: ANSI CL 5.1 — function utilities
- **難易度**: Low

#### FR-598: ストリーム型指定子 (typep 対応) — ✅ COMPLETE

- **対象**: `packages/vm/src/primitives.lisp` の `vm-typep-check`
- **実装**: `stream`/`input-stream`/`output-stream`/`file-stream`/`string-stream`/`broadcast-stream`/`two-way-stream`/`echo-stream`/`concatenated-stream`/`synonym-stream` すべて `vm-typep-check` lines 85-95 に実装済み
- **根拠**: ANSI CL 21.1 — Stream Types

#### FR-600: defvar / defparameter / defconstant 完全セマンティクス — ✅ COMPLETE

- **対象**: `packages/compile/src/codegen-functions.lisp`
- **実装**: `compile-ast ast-defvar` (lines 326-371) に `vm-boundp` チェック実装済み。`defparameter` — 常に設定; `defvar` — 未束縛時のみ設定 (`vm-jump-zero + vm-boundp`); `defconstant` — `defparameter` に変換 (不変性強制なし、許容制限)
- **根拠**: ANSI CL 3.8.1

#### FR-601: multiple-value-bind 値数不足時の挙動 — ✅ COMPLETE

- **対象**: `packages/vm/src/vm-execute.lisp`
- **実装**: `execute-instruction vm-mv-bind` (lines 527-536) — `(if (< i (length vals)) (nth i vals) nil)` で不足値を `nil` で補填; 余剰値は捨てる
- **根拠**: ANSI CL 5.3 — multiple-value-bind semantics

#### FR-602: (values) 0値返却の明示的サポート — ✅ COMPLETE

- **対象**: `packages/vm/src/vm-execute.lisp`
- **実装**: `execute-instruction vm-values` (line 524) — `(if all-values (first all-values) nil)` で 0値時は `nil` を返す; `vm-values-list` に空リストをセット。`vm-mv-bind` は空リストで全変数を `nil` に補填
- **根拠**: ANSI CL 5.3 — values with no arguments

#### FR-603: (setf (values a b ...) expr) — values を場所として使用 — ✅ COMPLETE

- **対象**: `packages/expand/src/expander.lisp` (`*setf-compound-place-handlers*`)
- **実装**: `(setf (gethash 'values *setf-compound-place-handlers*) ...)` lines 407-415 — `multiple-value-bind` + `setq` チェーンに展開
- **根拠**: ANSI CL 5.1.2.3 — VALUES as a Place

#### FR-604: float 2引数形式 (プロトタイプ指定) — ✅ COMPLETE

- **対象**: `packages/compile/src/codegen-phase2.lisp`
- **実装**: Phase 2 handler "FLOAT" で `(= (length args) 2)` の場合、prototype を評価・破棄し `vm-float-inst` を emit。NaN-boxing double-float 統一なので prototype は常に no-op
- **根拠**: ANSI CL 12.1.3.3 — float (number &optional prototype)

#### FR-605: bignum (多倍長整数)

- **対象**: `packages/vm/src/vm-numeric.lisp`, `packages/vm/src/vm.lisp` (NaN-boxing 値表現)
- **内容**: bignum タグを追加してホスト CL integer オブジェクトをヒープに保持。算術演算でオーバーフロー検出→bignum に昇格。`bignump` 述語。`integer-length` / `integer-decode-float` との統合
- **根拠**: ANSI CL 12.1.1 — numeric tower; fixnum/bignum 区別
- **難易度**: Very Hard (値表現の根幹変更)

#### FR-606: assert の place-list (場所付きリスタート)

- **対象**: `packages/expand/src/macros-stdlib.lisp`
- **内容**: `(assert test-form (place1 place2 ...) ...)` — 失敗時に `continue` リスタートで各 place の値を対話的に修正して再試行。`restart-case` と `store-value` リスタート (FR-421) に依存
- **根拠**: ANSI CL 9.1.3 — assert with places
- **難易度**: Medium (FR-421 後)

#### FR-607: defun / defmacro ドキュメント文字列

- **対象**: `packages/compile/src/codegen.lisp`, `packages/expand/src/expander.lisp`, `packages/vm/src/vm.lisp`
- **内容**: コンパイル時に docstring を検出し VM 関数オブジェクトのメタデータとして保存。`(documentation 'fn 'function)` / `(documentation 'mac 'macro)` で取得可能に。`define-condition` / `defclass` / `defmethod` / `defpackage` ドキュメント文字列も同様
- **根拠**: ANSI CL 5.4.1 — Documentation Strings
- **難易度**: Medium (FR-436 に先立つ基盤)

#### FR-609: list\* ユーザー向けビルトイン登録 — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`
- **実装**: `list*` はユーザーコード向けにも登録済み。
- **内容**: `(list* x1 x2 ... rest)` — 最後の引数にドットペアを生成する可変引数関数。
- **根拠**: ANSI CL 14.2.1 — list\*
- **難易度**: Easy

#### FR-610: -if-not 系シーケンス述語 — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-sequence.lisp`, `packages/expand/src/macros-stdlib.lisp`
- **実装**: `find-if-not` / `position-if-not` / `count-if-not` は実装済み。
- **内容**: 各関数は述語の否定版として動作する。
- **根拠**: ANSI CL 17.2 — sequence functions with -not variants
- **難易度**: Easy

#### FR-611: sort / stable-sort :key 引数 — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-stdlib.lisp`
- **実装**: `sort` / `stable-sort` は `:key` を受け取る。
- **内容**: 各要素比較前に `(funcall key elem)` でキー抽出する。
- **根拠**: ANSI CL 17.2 — sort :key argument
- **難易度**: Easy

#### FR-612: read / read-char eof-error-p / eof-value 引数

- **対象**: `packages/vm/src/io.lisp`, `packages/compile/src/builtin-registry-data.lisp`
- **内容**: `(read stream eof-error-p eof-value recursive-p)` / `(read-char stream eof-error-p eof-value recursive-p)` — 4引数形式を完全サポート。eof-error-p = nil のとき EOF で eof-value を返す
- **根拠**: ANSI CL 23.2 / 21.2 — read, read-char eof handling
- **難易度**: Medium

#### FR-613: with-output-to-string オプション文字列引数 — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-stdlib.lisp`
- **実装**: `(with-output-to-string (var string) ...)` は fresh stream に `string` を先に書き込み、その後の出力を連結して返す。ANSI の supplied-string backing semantics まではモデル化していない。
- **内容**: `string` が提供された場合、その内容を先頭プレフィックスとして保持する cl-cc 互換形を提供する。
- **根拠**: ANSI CL 21.2 — with-output-to-string with string argument
- **難易度**: Low (fill-pointer 対応文字列実装に依存)

#### FR-608: with-input-from-string キーワード引数 — ✅ COMPLETE

- **実装**: `with-input-from-string` は `:start` / `:end` / `:index` を受理し、入力範囲制限と終端位置の書き戻しに対応する。
- **根拠**: ANSI CL 21.2 — with-input-from-string
- **難易度**: Low

#### FR-614: (setf char) — 文字列の破壊的文字置換 — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`, `packages/vm/src/strings.lisp`
- **実装**: `(setf char)` / `(setf schar)` は登録済み。
- **内容**: 文字列の i 番目の文字を破壊的に置換する。
- **根拠**: ANSI CL 16.2 — setf of char
- **難易度**: Easy

#### FR-615: concatenate 型ディスパッチ完全化 — ✅ COMPLETE

- **実装**: `concatenate` は `'string` / `'list` / `'vector` を分岐して結合する。
- **根拠**: ANSI CL 17.2.2 — concatenate
- **難易度**: Easy

#### FR-616: hash-table-size / hash-table-rehash-size / hash-table-rehash-threshold — ✅ COMPLETE

- **実装**: `builtin-registry-data.lisp:278` でハッシュテーブルアクセサが登録済み。
- **根拠**: ANSI CL 18.2 — hash table accessors
- **難易度**: Easy

#### FR-617: read-from-string 2値目 (end-position) — ✅ COMPLETE

- **実装**: `read-from-string` は 2値目 `end-position` を返し、`format.lisp` 側で `vm-values-list` に反映する。
- **根拠**: ANSI CL 23.2 — read-from-string
- **難易度**: Medium

#### FR-618: (setf aref) — 配列要素の破壊的書き込み — ✅ COMPLETE

- **実装**: `(setf aref)` は `aset` への展開で処理される。
- **根拠**: ANSI CL 15.2 — setf of aref
- **難易度**: Easy

#### FR-619: (setf elt) — 実装済みと判明 (2026-03-27 修正)

- **状態**: ✅ 実装済み (FR-619 は誤記)
- **根拠**: `packages/parse/src/cl/parser.lisp:418-419` — `(setf (elt seq i) val)` をパーサーが `(aset seq i val)` に展開。`aset` は `builtin-registry-data.lisp:407` に登録済み

#### FR-620: (setf svref) / (setf row-major-aref) — ✅ COMPLETE

- **対象**: `packages/expand/src/expander-setf-places.lisp`
- **実装**: `*setf-compound-place-handlers*` に `svref` / `row-major-aref` ハンドラを登録済み。
- **内容**: `(setf (svref vector i) val)` — simple-vector への書き込み。`(setf (row-major-aref array i) val)` — row-major インデックスへの書き込み
- **根拠**: ANSI CL 15.2 — setf of svref, row-major-aref
- **難易度**: Easy (FR-618 と同パターン)

#### FR-621: (setf nth) — ✅ COMPLETE

- **対象**: `packages/expand/src/expander.lisp` (`*setf-compound-place-handlers*`)
- **実装**: `expand-setf-cons-place` 経由で `(rplaca (nthcdr n list) val)` に展開済み。
- **内容**: `(setf (nth n list) val)` → `(rplaca (nthcdr n list) val)` に展開
- **根拠**: ANSI CL 14.2.1 — setf of nth
- **難易度**: Easy

#### FR-622: (setf get) — ✅ COMPLETE

- **対象**: `packages/expand/src/expander-setf-places.lisp`
- **実装**: `*setf-compound-place-handlers*` に `get` ハンドラを追加し、`symbol-plist` + `rt-plist-put` + `%set-symbol-plist` で更新する。
- **内容**: `(setf (get symbol key) value)` — シンボル property list を更新して格納値を返す
- **根拠**: ANSI CL 10.2 — setf of get
- **難易度**: Easy

#### FR-623: let / flet 空バインディング形式

- **対象**: `packages/parse/src/cl/parser.lisp`
- **内容**: `(let () body)` / `(flet () body)` を ANSI CL 仕様通り空バインディングリストとして受け入れ、`progn` に等価展開
- **根拠**: ANSI CL 3.1.2.1 — special operators with empty binding lists
- **難易度**: Easy

#### FR-624: subtypep

- **対象**: `packages/vm/src/primitives.lisp`, `packages/compile/src/builtin-registry-data.lisp`
- **内容**: `(subtypep type1 type2 &optional environment)` — 2つの型指定子の包含関係を `(values result certainty)` で返す
- **根拠**: ANSI CL 4.2.2 — subtypep
- **難易度**: Medium (合成型指定子サポートは Hard)

#### FR-625: type-of — float / function 型返却

- **対象**: `packages/vm/src/primitives.lisp` (lines 414-428)
- **内容**: `float` → `single-float` または `double-float`。`vm-closure`/`vm-builtin-fn` オブジェクト → `function`。`nil` → `null`
- **根拠**: ANSI CL 4.2.6 — type-of
- **難易度**: Easy

#### FR-626: error / warn — フォーマット制御形式

- **対象**: `packages/compile/src/builtin-registry-data.lisp`, `packages/vm/src/conditions.lisp`
- **内容**: コードジェネレータ (または展開器) で `(error string &rest args)` を `(error (format nil string args...))` にデシュガーして単一コンディションオブジェクトとして渡す。`warn` も同様
- **根拠**: ANSI CL 9.1.2 — error function signature
- **難易度**: Easy

#### FR-627: string-upcase / string-downcase / string-capitalize — :start/:end キーワード

- **対象**: `packages/vm/src/strings.lisp` (lines 164-182), `packages/compile/src/builtin-registry-data.lisp` (lines 16-18)
- **内容**: `:start`/`:end` パラメータを受け取り部分文字列変換を実装。`nstring-upcase`/`nstring-downcase`/`nstring-capitalize` (FR-475) の破壊的版も同様
- **根拠**: ANSI CL 16.2.1 — string-upcase
- **難易度**: Easy

#### FR-628: open — ✅ COMPLETE

- **実装**: `vm-open-file` は phase2 handler で `:direction` / `:if-exists` / `:if-does-not-exist` を解析し、`with-open-file` から利用できる。
- **根拠**: ANSI CL 21.2.1 — open
- **難易度**: Low

#### FR-629: make-string-input-stream — ✅ COMPLETE

- **実装**: `make-string-input-stream` は phase2 handler 経由で登録済み。
- **根拠**: ANSI CL 21.3.5 — make-string-input-stream
- **難易度**: Easy

#### FR-630: coerce — 実行時型ディスパッチ

- **対象**: `packages/expand/src/macros-sequence.lisp` (lines 375-390)
- **内容**: 実行時型ディスパッチ関数を追加。または `coerce-to-*` 群を呼ぶランタイム関数 `rt-coerce` を `builtin-registry-data.lisp` に登録
- **根拠**: ANSI CL 12.1 — coerce with computed type
- **難易度**: Medium

#### FR-631: macroexpand / macroexpand-1 — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`, `packages/expand/src/macro.lisp`
- **実装**: `macroexpand` / `macroexpand-1` はビルトイン登録済みで、`our-macroexpand-1` / `our-macroexpand-all` に接続されている
- **内容**: ユーザーコードから `(macroexpand '(when t :ok))` が呼べる
- **根拠**: ANSI CL 3.1.2.1 — macroexpand
- **難易度**: Easy

#### FR-632: with-standard-io-syntax — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-stdlib.lisp` (line 285)
- **内容**: ANSI CL が指定する `*print-base*`/`*print-escape*`/`*print-readably*`/`*print-*` 全変数を標準値にバインドして body を実行
- **根拠**: ANSI CL 22.1.1 — with-standard-io-syntax
- **難易度**: Medium (プリンタ変数の実装に依存)

#### FR-633: clear-output — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`, `packages/vm/src/io.lisp`
- **実装**: `vm-clear-output` 命令と `clear-output` ビルトイン登録が実装済み
- **内容**: 出力バッファを破棄する
- **根拠**: ANSI CL 21.2 — clear-output
- **難易度**: Easy

#### FR-634: clrhash — ✅ COMPLETE

- **対象**: `packages/vm/src/hash.lisp` (line 73: `vm-clrhash`), `packages/compile/src/builtin-registry-data.lisp`
- **内容**: `remhash` と同パターンで `clrhash` を実行
- **根拠**: ANSI CL 18.1 — clrhash
- **難易度**: Easy (登録のみ)

#### FR-635: bit-nor / bit-nand / bit-eqv / bit-andc1 / bit-andc2 / bit-orc1 / bit-orc2 — ✅ COMPLETE

- **対象**: `packages/vm/src/primitives.lisp` または `packages/vm/src/list.lisp`
- **実装**: `bit-nor` / `bit-nand` / `bit-eqv` / `bit-andc1` / `bit-andc2` / `bit-orc1` / `bit-orc2` は `macros-stdlib.lisp` で実装済み
- **内容**: 各演算をビット配列要素ごとに適用する
- **根拠**: ANSI CL 15.2 — bit logical operations
- **難易度**: Easy

#### FR-636: (setf bit) / (setf sbit) — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`
- **実装**: `(setf (bit vec i) v)` / `(setf (sbit vec i) v)` は登録済み
- **内容**: `bit`/`sbit` の place 展開を提供する
- **根拠**: ANSI CL 15.2 — (setf bit)
- **難易度**: Easy

#### FR-637: 文字列比較 — :start1/:end1/:start2/:end2 部分文字列キーワード — ✅ COMPLETE

- **対象**: `packages/vm/src/strings.lisp` (string comparison VM instructions)
- **実装**: 文字列比較は `:start1`/`:end1`/`:start2`/`:end2` を受け取る実装に接続済み
- **内容**: 部分文字列比較をサポートする
- **根拠**: ANSI CL 16.2.3 — string comparisons with keyword arguments
- **難易度**: Medium (全比較命令の修正が必要)

#### FR-638: loop named — ✅ COMPLETE

- **対象**: `packages/expand/src/loop-parser.lisp`, `packages/expand/src/loop-emitters.lisp`
- **内容**: ループ名を `loop-state` に保存し、`return-from` に対応する `catch`/`throw` を生成
- **根拠**: ANSI CL 6.1.1 — named loop
- **難易度**: Medium

#### FR-639: asinh / acosh / atanh — ✅ COMPLETE

- **対象**: `packages/vm/src/vm-transcendental.lisp`, `packages/compile/src/builtin-registry-data.lisp`
- **実装**: `vm-asinh-inst` / `vm-acosh-inst` / `vm-atanh-inst` は `define-vm-unary-instruction` + `define-simple-instruction` で定義済み。`builtin-registry-data.lisp:77-79` に登録済み。
- **根拠**: ANSI CL 12.2.4 — asinh, acosh, atanh
- **難易度**: Easy

#### FR-640: nreconc — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-stdlib.lisp:997`
- **実装**: `(nreconc list tail)` は `macros-stdlib.lisp` で実装済み。`(nconc (nreverse list) tail)` に展開。
- **根拠**: ANSI CL 14.2.22 — nreconc
- **難易度**: Easy

#### FR-641: fill / replace / mismatch — ✅ COMPLETE

- **実装**: `fill` / `replace` / `mismatch` は list / vector / string で動作し、`:start` / `:end` / `:test` / `:key` / `:from-end` も対応している。
- **根拠**: ANSI CL 17.3 — fill, replace, mismatch
- **難易度**: Medium

#### FR-642: _features_ — ✅ COMPLETE

- **対象**: `packages/vm/src/vm.lisp` (line ~381), `packages/vm/src/vm-run.lisp` (line ~186)
- **実装**: `vm-state` / `vm2-state` の両方で `*features*` は `(:common-lisp :cl-cc)` に統一済み
- **内容**: 実行系差をなくす
- **根拠**: ANSI CL 1.5.2 — _features_ must include :common-lisp
- **難易度**: Trivial (1行修正)

#### FR-643: #nR — 任意基数整数リテラル — ✅ COMPLETE

- **対象**: `packages/parse/src/lexer-dispatch.lisp` (`lex-read-hash-dispatch`)
- **実装**: `otherwise` ブランチで数字を読み、`R`/`r` を確認 → `(lex-read-radix-integer state radix)` で整数をパース。`lexer-dispatch.lisp:285-290`。radix 2-36 の範囲チェック付き。
- **根拠**: ANSI CL 2.4.8 — #nR
- **難易度**: Easy

#### FR-644: #C — 複素数リテラル — ✅ COMPLETE

- **実装**: `#C(real imag)` は `lexer-dispatch.lisp` で読み取れる。
- **根拠**: ANSI CL 2.4.8.11 — #C
- **難易度**: Easy

#### FR-647: symbol-value — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`, `packages/vm/src/vm.lisp`
- **実装**: `symbol-value` はホストブリッジ登録済みで、ユーザーコードから利用できる
- **内容**: `(symbol-value sym)` を標準 API として提供する
- **根拠**: ANSI CL 10.1.1 — symbol-value
- **難易度**: Easy

#### FR-648: simple-vector-p — ✅ COMPLETE

- **実装**: `simple-vector-p` はビルトインとして利用可能。`vectorp` と組み合わせた単純ベクタ判定が動作する。
- **根拠**: ANSI CL 15.2 — simple-vector-p
- **難易度**: Easy

#### FR-649: write-to-string キーワード引数 (`:base`, `:radix`, `:escape` 等) — ✅ COMPLETE

- **実装**: `write-to-string` は `:base` / `:radix` / `:escape` / `:level` / `:length` / `:circle` / `:gensym` / `:readably` / `:pretty` を受理する。
- **根拠**: ANSI CL 22.1.2 — write-to-string
- **難易度**: Medium

#### FR-650: every / some / notany / notevery — ✅ COMPLETE

- **実装**: `every` / `some` / `notany` / `notevery` は複数シーケンスを並列処理できる。
- **根拠**: ANSI CL 17.2.1 — every, some, notany, notevery
- **難易度**: Medium

#### FR-651: vector (コンストラクタ) — ✅ COMPLETE

- **実装**: `(vector &rest objects)` は新規ベクタ構築に対応する。
- **根拠**: ANSI CL 15.2 — vector
- **難易度**: Easy

#### FR-652: copy-seq — ✅ COMPLETE

- **実装**: `copy-seq` は list / vector / string に対して正しいコピーを返す。
- **根拠**: ANSI CL 17.3 — copy-seq
- **難易度**: Easy–Medium

#### FR-653: remove-duplicates / delete-duplicates — ✅ COMPLETE

- **実装**: `remove-duplicates` / `delete-duplicates` は `:test` / `:test-not` / `:key` / `:start` / `:end` / `:from-end` を受け付ける。
- **根拠**: ANSI CL 17.2.19-20 — remove-duplicates / delete-duplicates
- **難易度**: Medium

#### FR-654: make-array :initial-contents — ✅ COMPLETE

- **実装**: `compile-make-array` は `:initial-contents` を処理し、初期内容付きの配列を生成する。
- **根拠**: ANSI CL 15.2 — make-array
- **難易度**: Medium

#### FR-665: mapcar / mapc / mapcan — ✅ COMPLETE

- **実装**: `mapcar` / `mapc` / `mapcan` は複数シーケンスに対応する。`(mapcar #'+ '(1 2) '(3 4))` のような並列処理が動作する。
- **根拠**: ANSI CL 17.3 — mapcar / mapc / mapcan
- **難易度**: Medium

#### FR-661: / (除算) — ✅ COMPLETE

- **対象**: `packages/parse/src/cl/parser.lisp`, `packages/compile/src/builtin-registry-data.lisp`
- **内容**: ANSI CL の `/` を VM 除算命令へ接続
- **根拠**: ANSI CL 12.2 — /
- **難易度**: Easy

#### FR-660: assoc-if / assoc-if-not — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`
- **実装**: `assoc-if` / `assoc-if-not` は `macros-stdlib.lisp` にユーザー向けマクロとして実装済み。
- **根拠**: ANSI CL 14.2.3 — assoc-if / assoc-if-not
- **難易度**: Easy

#### FR-697: assoc / rassoc / member — ✅ COMPLETE

- **対象**: `packages/expand/src/expander-data.lisp`, `packages/vm/src/list.lisp`
- **実装**: `assoc` / `rassoc` / `member` は `:test` / `:test-not` / `:key` を受け付ける。
- **根拠**: ANSI CL 14.2.1 (member), 14.2.3 (assoc), 14.2.3 (rassoc)
- **難易度**: Medium

#### FR-658: read-preserving-whitespace — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`
- **実装**: `read-preserving-whitespace` は `read` に委譲して利用できる。
- **根拠**: ANSI CL 23.2.1 — read-preserving-whitespace
- **難易度**: Easy (ホスト CL に委譲可能)

#### FR-659: read-delimited-list — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`
- **実装**: `read-delimited-list` は host bridge 経由で利用できる。
- **根拠**: ANSI CL 23.2.1 — read-delimited-list
- **難易度**: Medium (VM read ループ拡張が必要)

#### FR-655: find-symbol — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`, `packages/vm/src/vm.lisp`
- **実装**: `find-symbol` はホストブリッジ経由で利用可能
- **内容**: パッケージからシンボルを検索し、シンボルと状態の2値を返す
- **根拠**: ANSI CL 11.2.4 — find-symbol
- **難易度**: Easy (intern と同様にホストブリッジ登録)

#### FR-656: list-length — ✅ COMPLETE

- **実装**: `vm-list-length` が `builtin-registry-data.lisp:161` に登録済みで、ユーザーコードから利用できる。
- **根拠**: ANSI CL 14.2.18 — list-length
- **難易度**: Low

#### FR-657: subst-if / subst-if-not — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp`, `packages/vm/src/list.lisp`
- **実装**: `subst-if` / `subst-if-not` は `macros-stdlib.lisp` に実装済み。
- **根拠**: ANSI CL 14.2.34/35 — subst-if / subst-if-not
- **難易度**: Easy–Medium

#### FR-682: `-` 単項否定バグ / 0引数エラー欠如 — ✅ COMPLETE

- **対象**: `packages/expand/src/expander.lisp`
- **実装**: `expander.lisp:430-431` で0引数→エラー、1引数→ `(- 0 x)` に展開。`(- x)` → `-x`、`(-)` → `error` が動作。
- **根拠**: ANSI CL 12.2 — `(- x)` → `-x`; `(-)` is an error
- **難易度**: Easy

#### FR-683: isqrt — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-stdlib.lisp`
- **実装**: Newton 法による整数平方根。`(floor (sqrt (float n)))` + Newton 修正ループで浮動小数点精度喪失を回避。
- **根拠**: ANSI CL 12.1.3 — isqrt returns the greatest integer ≤ exact positive square root
- **難易度**: Medium

#### FR-684: signum — ✅ COMPLETE

- **対象**: `packages/expand/src/macros-stdlib.lisp`
- **実装**: `macros-stdlib.lisp` で型保存実装。`(signum 2.5)` → `1.0`、`(signum -3)` → `-1`。入力型を保存した戻り値。
- **根拠**: ANSI CL 12.1.3 — signum preserves type
- **難易度**: Medium

#### FR-686: aref — ✅ COMPLETE

- **実装**: `aref` は多次元配列に対応し、複数 subscript を受け付ける。
- **根拠**: ANSI CL 15.2 — aref with multiple subscripts
- **難易度**: Medium

#### FR-688: delete / substitute 系 — keyword 引数 — ✅ COMPLETE

- **実装**: `delete` / `delete-if` / `delete-if-not` / `substitute` / `substitute-if` / `substitute-if-not` / `nsubstitute` 系は `:test` / `:key` / `:start` / `:end` / `:count` / `:from-end` を正しく受け付ける。
- **根拠**: ANSI CL 17.2, 17.3 — delete/substitute keyword arguments
- **難易度**: Medium (`:test`/`:key` は比較的容易; `:start`/`:end` はシーケンス型依存)

#### FR-679: get-decoded-time — ✅ COMPLETE

- **対象**: `packages/pipeline/stdlib-source.lisp`
- **実装**: `stdlib-source.lisp:358-359` で `(defun get-decoded-time () (decode-universal-time (get-universal-time)))` として定義済み。
- **根拠**: ANSI CL 25.1.4 — get-decoded-time
- **難易度**: Easy

#### FR-680: provide / require — ✅ COMPLETE

- **実装**: `provide` はモジュールを重複なしで登録し、`require` は `pathnames` 指定時に順次ロードする。
- **根拠**: ANSI CL 25.1.3 — provide/require
- **難易度**: Easy (provide), Medium (require — load integration)

#### FR-681: sleep — ✅ COMPLETE

- **実装**: `sleep` が `builtin-registry-data.lisp:133` に登録済み。
- **根拠**: ANSI CL 25.1.1 — sleep
- **難易度**: Low

#### FR-675: maphash — ✅ COMPLETE

- **実装**: `our-defmacro maphash` が `hash-table-keys` を使ってキー/値を走査する (`packages/expand/src/macros-stdlib.lisp:1317-1325`)。`builtin-registry-data.lisp` にも `maphash` が登録済み (`packages/compile/src/builtin-registry-data.lisp:180`)。
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

#### FR-669: string comparators — 戻り値が boolean (0/1) で mismatch-index を返さない — ✅ COMPLETE

- **対象**: `packages/vm/src/strings.lisp` (lines 114-124), `packages/compile/src/builtin-registry-data.lisp` (lines 212-227)
- **実装**: 文字列比較は ANSI 仕様どおり mismatch-index / nil を返す。
- **根拠**: ANSI CL 13.2.4 — string comparators return index or nil
- **難易度**: Medium

#### FR-670: peek-char — ✅ COMPLETE

- **実装**: `peek-char` は `builtin-registry-data.lisp:423` に登録済み。
- **根拠**: ANSI CL 21.2 — peek-char
- **難易度**: Easy (登録のみ)〜Medium (peek-type 完全対応)

#### FR-671: unread-char — ✅ COMPLETE

- **実装**: 2引数版は Phase 1、1引数のデフォルト `*standard-input*` 版は Phase 2 handler で対応済み。
- **根拠**: ANSI CL 21.2 — `(unread-char character &optional input-stream)`
- **難易度**: Easy

#### FR-672: read-line — ✅ COMPLETE

- **実装**: `eof-error-p` / `eof-value` は通り、`missing-newline-p` は 2値目として返る。
- **根拠**: ANSI CL 21.2 — read-line returns (values string missing-newline-p)
- **難易度**: Easy〜Medium

#### FR-673: terpri / fresh-line — ✅ COMPLETE

- **実装**: optional stream 引数は Phase 2 handlers で利用できる。
- **根拠**: ANSI CL 21.2 — terpri/fresh-line optional stream
- **難易度**: Easy

#### FR-674: listen — ✅ COMPLETE

- **実装**: 1引数版は Phase 1、0引数のデフォルト `*standard-input*` 版は Phase 2 handler で対応済み。
- **根拠**: ANSI CL 21.2 — `(listen &optional input-stream)`
- **難易度**: Easy

#### FR-666: print / prin1 / princ — オプション stream 引数なし — ✅ COMPLETE

- **対象**: `packages/compile/src/builtin-registry-data.lisp` (lines 260-262)
- **実装**: `print` / `prin1` / `princ` は optional stream 付きで利用できる。
- **根拠**: ANSI CL 22.3.3 — print / prin1 / princ
- **難易度**: Easy

#### FR-689: catch / throw — ✅ COMPLETE

- **実装**: `compile-ast` が `vm-establish-catch` / `vm-throw` を emit し、`catch` / `throw` の非局所脱出が動作する (`packages/compile/src/codegen.lisp:54-84`)。
- **根拠**: ANSI CL 5.2 — catch/throw for non-local exits
- **難易度**: Low

#### FR-690: rotatef — ✅ COMPLETE

- **実装**: `rotatef` は `&rest places` を受け取り、2個以上の place を順に回転する (`packages/expand/src/macros-stdlib.lisp:101-117`)。
- **根拠**: ANSI CL 5.1.2 — rotatef must accept N places using setf
- **難易度**: Low

#### FR-691: ignore-errors — ✅ COMPLETE

- **実装**: エラー時に `(values nil condition)` を返す (`packages/expand/src/macros-stdlib.lisp:1115-1119`)。
- **根拠**: ANSI CL 9.1 — ignore-errors returns (values nil condition) on error
- **難易度**: Low

#### FR-696: &optional / &key supplied-p 変数 — codegen で破棄 — ✅ COMPLETE

- **実装**: `supplied-p` 変数は codegen で保持され、`&optional` / `&key` の supplied-p 判定が動作する。
- **根拠**: ANSI CL 3.4.1 — Ordinary Lambda Lists, supplied-p parameters
- **難易度**: Medium

#### FR-695: loop downfrom / downto / above — ✅ COMPLETE

- **対象**: `packages/expand/src/loop-parser.lisp` (lines 118-130)
- **内容**: `(loop for x downfrom 10 to 1 collect x)` が `(10 9 8 7 6 5 4 3 2 1)` を返す
- **根拠**: ANSI CL 6.1.1.3 — loop numeric stepping
- **難易度**: Medium
  s

### incf / decf / push / pop — ✅ COMPLETE

- **実装**: `incf` / `decf` / `push` / `pop` は複合 place のサブフォームを gensym で保護する。
- **根拠**: ANSI CL 5.1.3 — modify macros must evaluate subforms exactly once
- **難易度**: Low
