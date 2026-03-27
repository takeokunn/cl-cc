# CLOS & MOP — 2026年モダンコンパイラ機能リファレンス

2026年現在、SBCL 2.6 / CCL 1.13 / ECL 24.x が実装している CLOS・MOP 機能の完全リファレンス。
cl-cc の実装状況を `✅ 実装済み` / `🔧 部分実装` / `📋 未実装` で示す。

---

## 1. コアオブジェクトモデル (ANSI CL Chapter 4, 7)

### 1.1 クラス定義 — `defclass`

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| 基本スロット定義 | ✅ | ✅ | `:reader` `:writer` `:accessor` `:initarg` `:initform` |
| `:allocation :instance` | ✅ | ✅ | デフォルト — インスタンスごとのスロット |
| `:allocation :class` | 📋 FR-374 | ✅ | クラス共有スロット (`*accessor-slot-map*` 拡張が必要) |
| `:type` スロットオプション | 📋 | ✅ | コンパイル時型チェック / 最適化ヒント |
| `:documentation` スロットオプション | 📋 | ✅ | `slot-definition-documentation` で参照可能 |
| クラスオプション `:default-initargs` | 📋 FR-375 | ✅ | `shared-initialize` で処理 |
| クラスオプション `:metaclass` | 📋 FR-375 | ✅ | MOP の中核。`standard-class` / `funcallable-standard-class` 等 |
| クラスオプション `:documentation` | 📋 FR-375 | ✅ | `documentation` GF で参照 |
| 多重継承 | ✅ | ✅ | 複数スーパークラス指定可能 |
| 自動 `standard-object` 継承 | 📋 FR-528 | ✅ | スーパークラス省略時の暗黙継承 |

**現在の cl-cc 内部表現:**
```
クラスHT = { :__name__ → symbol
             :__superclasses__ → list
             :__slots__ → list
             :__initargs__ → alist (initarg → slot-name)
             :__initforms__ → alist (slot-name → value)
             :__methods__ → HT (dispatch-key → closure)
             :__cpl__ → list }
インスタンスHT = { :__class__ → クラスHT, slot-name → value, ... }
```

### 1.2 クラス優先順位リスト (CPL / MRO)

| アルゴリズム | cl-cc | SBCL | 備考 |
|------------|-------|------|------|
| 深さ優先左右探索 (DFLS) | ✅ | — | 現行実装 (`compute-class-precedence-list` in `vm-clos.lisp:119`) |
| **C3 線形化** | 📋 FR-378 | ✅ | Barrett et al. 1996。ANSI CL 4.3.5 準拠の正式アルゴリズム |
| ダイアモンド継承の正確なCPL | 📋 | ✅ | C3 なしでは `(A B C B)` のような重複が生じる |
| CPL 矛盾検出 (エラー) | 📋 | ✅ | `(defclass C (A B))` vs `(defclass D (B A))` → `error` |

```lisp
;; C3 の例
(defclass A () ())
(defclass B (A) ())
(defclass C (A) ())
(defclass D (B C) ())
;; SBCL CPL: (D B C A standard-object t)  ← C3
;; 現cl-cc:  (D B A C A ...)              ← DFS (重複あり・不正)
```

---

## 2. ジェネリック関数 (ANSI CL 7.6)

### 2.1 `defgeneric`

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| 基本定義 | ✅ | ✅ | ディスパッチHTを生成 |
| `:method-combination` オプション | 📋 FR-382 | ✅ | `standard` / `+` / `append` 等 |
| `:argument-precedence-order` | 📋 FR-382 | ✅ | デフォルト左→右 |
| `:generic-function-class` | 📋 | ✅ | MOP: カスタムGFクラス |
| `:documentation` | 📋 | ✅ | GF のドキュメント文字列 |
| インラインメソッド `(:method ...)` | 📋 FR-382 | ✅ | `defgeneric` 内に直接 `defmethod` |
| `ensure-generic-function` (公開) | 🔧 FR-525 | ✅ | `%ensure-generic-function` は内部のみ |
| GF 再定義セマンティクス | 🔧 | ✅ | 既存GFに `defgeneric` を再実行すると既存メソッドは保持。lambda-list 不一致なら `error` |
| `generic-function-name` | 📋 FR-526 | ✅ | MOP アクセサ |
| `generic-function-methods` | 📋 FR-526 | ✅ | MOP アクセサ |
| `generic-function-lambda-list` | 📋 | ✅ | MOP アクセサ |
| `generic-function-method-class` | 📋 | ✅ | MOP アクセサ |
| `generic-function-method-combination` | 📋 | ✅ | MOP アクセサ |

### 2.2 `defmethod` と特化子 (Specializers)

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| クラス特化子 `(param ClassName)` | ✅ | ✅ | 基本ディスパッチ |
| T (全型) 特化子 | ✅ | ✅ | フォールバックメソッド |
| **EQL 特化子** `(param (eql value))` | 📋 FR-124 | ✅ | `(eql :keyword)` や `(eql 42)` 等 |
| 多重ディスパッチ | ✅ | ✅ | 複合キー `(list class1 class2 ...)` |
| 継承ベースのフォールバック | ✅ | ✅ | CPL を辿って最も特化したメソッドを選択 |
| `method-qualifiers` アクセサ | 📋 FR-526 | ✅ | MOP |
| `method-specializers` アクセサ | 📋 FR-526 | ✅ | MOP |
| `find-method` | 📋 FR-377 | ✅ | `(find-method gf qualifiers specializers)` |
| `add-method` / `remove-method` | 📋 FR-377 | ✅ | 実行時メソッド追加・削除 |
| `compute-applicable-methods` | 📋 FR-377 | ✅ | ユーザーから呼び出し可能 |
| `compute-applicable-methods-using-classes` | 📋 | ✅ | MOP: クラスリストで候補取得 |
| `no-applicable-method` | 📋 FR-377 | ✅ | GF として定義。デフォルトは `error` |
| `no-next-method` | 📋 | ✅ | `call-next-method` で次がない場合 |
| `function-keywords` | 📋 | ✅ | `(function-keywords method)` → `(values keyword-list allow-other-keys-p)` |
| メソッド再定義セマンティクス (ANSI CL 7.6.3) | ✅ | ✅ | 同一 name/qualifier/specializer の `defmethod` は既存メソッドを置換 |

**`function-keywords` の使い方:**
```lisp
(defmethod f ((x integer) &key verbose debug) ...)
(function-keywords (find-method #'f '() (list (find-class 'integer))))
;; → (values '(:verbose :debug) nil)   ; nil = &allow-other-keys なし
```

### 2.3 メソッド修飾子 (Method Qualifiers)

| 修飾子 | cl-cc | SBCL | 備考 |
|--------|-------|------|------|
| `primary` (修飾子なし) | ✅ | ✅ | 現在のディスパッチで動作 |
| `:before` | 📋 FR-215 | ✅ | primary の前に全 `:before` を実行 |
| `:after` | 📋 FR-215 | ✅ | primary の後に全 `:after` を逆順実行 |
| `:around` | 📋 FR-215 | ✅ | 全体をラップ。`call-next-method` で連鎖 |
| AST でのqualifier保持 | 📋 FR-383 | — | `parser.lisp:548` で現在 `(declare (ignore qualifier))` |

**standard method combination 実行順序 (SBCL準拠):**
```
most-specific-around
  → next-around
    → all :before (most-specific first)
      → most-specific primary
        → call-next-method chain
    → all :after (least-specific first)
```

### 2.4 `call-next-method` と `next-method-p`

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `call-next-method` 引数なし (現引数を引き継ぐ) | ✅ | ✅ | `vm-method-call-stack` で追跡。CPL 順に次メソッドへ |
| `call-next-method new-arg1 new-arg2` (引数変更) | 📋 | ✅ | 新しい引数で次メソッドを呼び出す。再ディスパッチはしない |
| `call-next-method` with `:around` qualifier | 📋 FR-215 | ✅ | around の連鎖。primary chain とは別のスタック |
| `next-method-p` | 📋 | ✅ | 次のメソッドが存在するか確認。`nil` なら `call-next-method` はエラー |
| `no-next-method` GF | 📋 | ✅ | 次メソッドがない状態で `call-next-method` → この GF を呼ぶ |
| メソッド外での `call-next-method` | — | ✅ エラー | `call-next-method` はメソッドボディ内でのみ有効 |

**意味論の詳細 (ANSI CL 7.6.6.2):**
```lisp
;; 引数なし: 元の引数をそのまま次メソッドに渡す
(defmethod area :around ((s square))
  (* 2 (call-next-method)))  ; square の primary を元の引数で呼ぶ

;; 引数あり: 新しい引数で次メソッドを呼ぶ (再ディスパッチなし)
(defmethod normalize ((v vector))
  (call-next-method (normalize-impl v)))  ; normalize の次メソッドを new-v で呼ぶ

;; next-method-p でガード
(defmethod process ((x t))
  (when (next-method-p)
    (call-next-method)))
```

**cl-cc の現実装 (`vm-method-call-stack`):**
```
vm-method-call-stack = ((gf-ht [m2 m3] [arg1 arg2])  ; top: 現メソッド
                         (gf-ht [m3]    [arg1 arg2])   ; next
                         nil                            ; 通常 call フレーム
                         ...)
```

### 2.5 ラムダリスト適合規則 (Lambda List Congruence)

ANSI CL 7.6.4 — `defmethod` のラムダリストは `defgeneric` と適合していなければならない。

| ルール | cl-cc | SBCL | 説明 |
|--------|-------|------|------|
| 必須引数の数が同じ | 📋 | ✅ エラー | `(defgeneric f (a b))` → `defmethod` も2引数必須 |
| `&optional` の有無が一致 | 📋 | ✅ エラー | GF に `&optional` があれば全メソッドに必要 |
| `&rest` の有無が一致 | 📋 | ✅ エラー | |
| `&key` 使用の一致 | 📋 | ✅ エラー | GF か任意のメソッドが `&key` を使えば全員 `&key` 対応が必要 |
| 特化子は必須引数のみ | ✅ | ✅ | `&optional`/`&rest`/`&key` 引数への特化子は不可 |
| `&key` キー名の一致 | — | ✅ 警告 | SBCL は不整合 key を警告。ANSI は `&allow-other-keys` で回避 |
| GF なしでの `defmethod` | ✅ | ✅ | GF を自動生成 (cl-cc は `%ensure-generic-function`) |

```lisp
;; 適合エラーの例
(defgeneric f (a b &optional c))
(defmethod f ((a integer) b) ...)  ; エラー: &optional が欠落
(defmethod f ((a integer) b &optional c &key d) ...)  ; &key は追加可 (ANSI では許容)
```

---

## 3. メソッドコンビネーション (ANSI CL 7.6.6)

### 3.0 メソッドコンビネーション内のエラーシグナル

`define-method-combination` の長形式ボディ内でのみ使える特殊なエラー関数。

| 関数 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `invalid-method-error method format-string &rest args` | 📋 | ✅ | 不正な修飾子を持つメソッドを報告。`method-combination` のチェック時に使用 |
| `method-combination-error format-string &rest args` | 📋 | ✅ | メソッドコンビネーション自体のエラーを報告 (メソッド特定なし) |

```lisp
;; 使用例: my-around qualifier しか許可しないコンビネーション
(define-method-combination my-combo ()
  ((primary () :required t)
   (around  (:my-around)))
  ;; 不正な qualifier を持つメソッドを検出してエラー
  (dolist (m (generic-function-methods (generic-function-argument-precedence-order ...)))
    (unless (or (null (method-qualifiers m))
                (equal '(:my-around) (method-qualifiers m)))
      (invalid-method-error m "Method ~S has invalid qualifier ~S" m (method-qualifiers m))))
  ...)
```

### 3.1 組み込みコンビネーション

| 種類 | cl-cc | SBCL | 説明 |
|------|-------|------|------|
| `standard` | 🔧 | ✅ | `:before`/:after`/`:around` + primary |
| `+` | 📋 FR-376 | ✅ | 全メソッドの戻り値を `+` で集約 |
| `append` | 📋 FR-376 | ✅ | リスト結果を `append` で結合 |
| `list` | 📋 FR-376 | ✅ | 結果をリストに収集 |
| `nconc` | 📋 FR-376 | ✅ | `nconc` で結合 |
| `progn` | 📋 FR-376 | ✅ | 最後の値を返す |
| `and` | 📋 FR-376 | ✅ | 全て真なら最後の値 |
| `or` | 📋 FR-376 | ✅ | 最初の真の値 |
| `max` / `min` | 📋 FR-376 | ✅ | 数値結果の最大・最小 |

**`:around` と非標準コンビネーションの相互作用 (ANSI CL 7.6.6.4):**

非標準コンビネーション (`+`, `append` 等) であっても、**`:around` メソッドは常に有効**。`:around` が全体を包み、`call-next-method` が非標準コンビネーションの結果を呼ぶ。

```lisp
(defgeneric total (x) (:method-combination +))
(defmethod total + ((x number)) x)
(defmethod total + ((x integer)) (* x 10))
(defmethod total :around ((x t))
  (format t "Computing...~%")
  (call-next-method))  ; ← + combination の結果 (x + x*10) を返す
```

これは `standard` コンビネーションの `:around` と同じ仕組み。非標準コンビネーションでも短形式の `define-method-combination` は `:around` を自動サポートする。

### 3.3 `method-combination` MOP クラス

`define-method-combination` が定義したコンビネーション型は `method-combination` クラスのインスタンスとして表現される。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `method-combination` クラス (抽象) | 📋 | ✅ | MOP: メソッドコンビネーション型のオブジェクト表現 |
| `short-method-combination` | 📋 | ✅ | 短形式で定義されたコンビネーション |
| `long-method-combination` | 📋 | ✅ | 長形式で定義されたコンビネーション |
| `generic-function-method-combination gf` | 📋 | ✅ | GF が使うコンビネーションオブジェクトを返す |
| `find-method-combination gf type options` | 📋 | ✅ SBCL | `sb-pcl` 経由でコンビネーション型を検索 |

```lisp
;; SBCL での確認
(generic-function-method-combination #'print-object)
;; → #<SB-PCL::STANDARD-METHOD-COMBINATION STANDARD {}>
(class-of *)
;; → #<SB-PCL::STANDARD-METHOD-COMBINATION>
```

### 3.4 (旧 3.2) `define-method-combination`

#### 短形式

```lisp
;; (define-method-combination name :operator op :identity-with-one-argument bool)
(define-method-combination +   :operator +   :identity-with-one-argument t)
(define-method-combination max :operator max :identity-with-one-argument t)
```

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `:operator` — 結合演算子 | 📋 FR-376 | ✅ | 全 primary メソッドの結果をこれで fold |
| `:identity-with-one-argument` | 📋 FR-376 | ✅ | `t` のとき単一メソッドで演算子をスキップ |
| `:documentation` | 📋 | ✅ | |

#### 長形式

```lisp
(define-method-combination progn ()
  ((methods ()))   ; method-group: qualifiers = ()
  `(progn ,@(mapcar #'(lambda (m) `(call-method ,m)) methods)))
```

| 要素 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `(method-group qualifier-pattern ...)` | 📋 FR-376 | ✅ | メソッドをqualifierでグループ分け |
| `qualifier-pattern`: `()` / `(:before)` / `*` | 📋 | ✅ | `*` = 任意のqualifier |
| `:required t/nil` | 📋 | ✅ | グループが空のときエラーにするか |
| `:order :most-specific-first` / `:most-specific-last` | 📋 | ✅ | グループ内の順序 |
| `:description` 文字列 | 📋 | ✅ | エラーメッセージ用 |
| `:arguments &rest args` | 📋 | ✅ | effective method に引数リストを渡す |
| `:generic-function gf-var` | 📋 | ✅ | effective method に GF オブジェクトを渡す |
| `call-method m next-methods` | 📋 | ✅ | effective method 内でメソッドを呼ぶ疑似形式 |
| `make-method form` | 📋 | ✅ | 匿名メソッドを effective method 内に埋め込む |

**長形式の完全例:**
```lisp
;; :around → :before → primary (最特化) → :after の標準組み合わせ相当
(define-method-combination standard ()
  ((around   (:around))
   (before   (:before))
   (primary  ()         :required t)
   (after    (:after)))
  (flet ((call-methods (methods)
           (mapcar (lambda (m) `(call-method ,m)) methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                       (progn ,@(call-methods before)
                              (call-method ,(first primary) ,(rest primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary) ,(rest primary)))))
      (if around
          `(call-method ,(first around) (,@(rest around) (make-method ,form)))
          form))))
```

---

## 4. インスタンス管理 (ANSI CL 7.1, 7.2)

### 4.1 オブジェクト生成

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `make-instance` | ✅ | ✅ | 静的クラス名 + 動的クラス変数 両対応 |
| `allocate-instance` GF | 📋 FR-524 | ✅ | メモリ割り当てのカスタマイズポイント |
| `initialize-instance` GF | 📋 FR-379 | ✅ | `:after` でカスタム初期化が可能に |
| `shared-initialize` GF | 📋 | ✅ | `initialize-instance` / `reinitialize-instance` の共通ロジック |
| `reinitialize-instance` | 📋 | ✅ | 既存インスタンスのスロット再初期化 |
| `:default-initargs` 処理 | 📋 FR-375 | ✅ | クラスオプションの `:default-initargs` を `shared-initialize` で適用 |

### 4.2 スロットアクセス

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `slot-value` / `(setf slot-value)` | ✅ | ✅ | `vm-slot-read` / `vm-slot-write` 命令 |
| `slot-boundp` | ✅ | ✅ | `vm-slot-boundp` 命令 |
| `slot-makunbound` | ✅ | ✅ | `vm-slot-makunbound` 命令 |
| `slot-exists-p` | ✅ | ✅ | `vm-slot-exists-p` 命令 |
| `slot-unbound` GF | 📋 FR-384 | ✅ | 未束縛アクセス時のカスタマイズ。現在は直接 `error` |
| `slot-missing` GF | 📋 | ✅ | **スロット名自体が存在しない**場合。`slot-unbound` とは別 (operation引数: `:slot-value`/`:setf`/`:slot-boundp`/`:slot-makunbound`) |
| `:reader` / `:writer` / `:accessor` | ✅ | ✅ | `compile-slot-accessor` で closure 生成 |
| accessor read inlining | 📋 FR-120 | ✅ | `*accessor-slot-map*` あり。read 方向は未展開 |
| `with-slots` マクロ | ✅ | ✅ | `macros-stdlib.lisp` 実装済み |
| `with-accessors` マクロ | ✅ | ✅ | `macros-stdlib.lisp` 実装済み |

### 4.3 クラス変更プロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `change-class` | 🔧 FR-380 | ✅ | `macros-sequence.lisp:527` — `:__class__` 差し替えのみ。スロット移行・`update-instance-for-different-class` 呼び出しなし |
| `update-instance-for-different-class` GF | 📋 FR-380 | ✅ | `change-class` プロトコルの一部 |
| `make-instances-obsolete` | 📋 FR-523 | ✅ | クラス再定義時の既存インスタンス無効化 |
| `update-instance-for-redefined-class` GF | 📋 FR-523 | ✅ | 遅延マイグレーション。スロットアクセス時に自動起動 |

---

## 5. 型システムとディスパッチ (ANSI CL 4.3, 7.6)

### 5.1 型分類 (`vm-classify-arg`)

現在 `vm-execute.lisp:383` の実装:

```lisp
;; 現行: 4型のみ
(typecase arg
  (integer 'integer)
  (string  'string)
  (symbol  'symbol)
  (t       t))       ; CLOS インスタンスは :__class__ で分岐
```

**SBCL / ANSI CL が要求する完全な型カバレッジ (FR-381):**

| 型 | cl-cc | 備考 |
|----|-------|------|
| `integer` (fixnum / bignum) | ✅ | 現行 |
| `string` | ✅ | 現行 |
| `symbol` (null を含む) | ✅ | 現行 |
| CLOS インスタンス | ✅ | `:__class__` で判別 |
| `float` (single / double) | 📋 FR-381 | |
| `ratio` | 📋 FR-381 | |
| `complex` | 📋 FR-381 | |
| `character` | 📋 FR-381 | |
| `cons` / `list` | 📋 FR-381 | |
| `array` / `vector` / `bit-vector` | 📋 FR-381 | |
| `function` / `closure` | 📋 FR-381 | |
| `hash-table` | 📋 FR-381 | CLOS HTと区別が必要 |
| `pathname` | 📋 | |
| `stream` | 📋 | |
| `null` | 📋 FR-381 | `symbol` とは別クラス |
| `structure-object` | 📋 | `defstruct` 由来 |

### 5.2 `class-of` / `type-of` / `typep`

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `class-of` | 📋 FR-385 | ✅ | VM命令なし。`:__class__` から取得する命令が必要 |
| `type-of` | 📋 FR-385 | ✅ | 標準型 + CLOSクラス両方返却 |
| `typep` (クラス名) | 📋 FR-385 | ✅ | `(typep obj 'my-class)` — CPL トラバーサルが必要 |
| **`typep` (クラスオブジェクト)** | 📋 | ✅ | `(typep obj (find-class 'my-class))` — ANSI CL 4.3.7。クラスオブジェクトを直接渡せる |
| `subtypep` (クラス間) | 📋 | ✅ | `(subtypep 'child 'parent)` → T。CPL を使って判定 |
| クラス型としての型指定子 | 📋 | ✅ | クラス名はそのまま型指定子として使える。`(declare (type my-class x))` 等 |

### 5.3 組み込みクラス階層

**SBCL の標準クラス階層 (cl-cc での実装状況):**

```
t
└── standard-object         📋 FR-528 (VMレジストリに未登録)
    └── (user classes)      ✅
└── function                📋 FR-528
└── stream                  📋 FR-528
└── structure-object        📋
└── condition               🔧 (conditions.lisp で独自実装)
    └── serious-condition
        └── error
    └── warning
└── number
    ├── integer             ✅ (vm-classify-arg)
    ├── float               📋 FR-381
    └── ratio               📋 FR-381
└── sequence
    ├── list                📋 FR-381
    └── vector              📋 FR-381
└── string                  ✅ (vm-classify-arg)
└── symbol                  ✅ (vm-classify-arg)
└── character               📋 FR-381
└── hash-table              📋 FR-381
```

---

## 6. MOP — メタオブジェクトプロトコル (AMOP)

### 6.1 クラスイントロスペクション

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `class-name` | 📋 FR-527 | ✅ | `:__name__` は内部アクセスのみ |
| `class-direct-superclasses` | 📋 FR-527 | ✅ | `:__superclasses__` の公開 |
| `class-precedence-list` | 📋 FR-527 | ✅ | `:__cpl__` の公開 |
| `class-direct-slots` | 📋 FR-527 | ✅ | 直接定義スロットのみ |
| `class-slots` | 📋 | ✅ | 継承含む全スロット |
| `class-direct-subclasses` | 📋 | ✅ | 逆参照テーブルが必要 |
| `class-direct-methods` | 📋 | ✅ | このクラスを直接特化子として持つメソッドのリスト |
| `class-direct-default-initargs` | 📋 | ✅ | |
| `class-default-initargs` | 📋 | ✅ | 継承含む |
| `class-prototype` | 📋 | ✅ | クラスのプロトタイプインスタンス |
| `class-finalized-p` | 📋 | ✅ | クラス確定状態 |
| `finalize-inheritance` | 📋 | ✅ | CPL・スロット解決を確定 |

### 6.2 スロット定義イントロスペクション

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `slot-definition-name` | 📋 | ✅ | |
| `slot-definition-initargs` | 📋 | ✅ | |
| `slot-definition-initform` | 📋 | ✅ | |
| `slot-definition-initfunction` | 📋 | ✅ | |
| `slot-definition-type` | 📋 | ✅ | |
| `slot-definition-allocation` | 📋 | ✅ | `:instance` / `:class` |
| `slot-definition-readers` | 📋 | ✅ | |
| `slot-definition-writers` | 📋 | ✅ | |
| `slot-definition-location` | 📋 | ✅ SBCL | スロットのインスタンスベクタ内インデックス (slot vector 実装時に重要) |

### 6.2b Effective Slot Definition マージ規則 (ANSI CL 7.5.3)

多重継承で複数のスーパークラスが同名スロットを定義した場合、`compute-effective-slot-definition` が結合する。

| 項目 | マージ規則 | cl-cc | SBCL |
|------|-----------|-------|------|
| スロット名の衝突 | CPL 最上位の定義が「勝つ」 | 📋 | ✅ |
| `:initform` | CPL で最初に見つかった定義を使用 | 📋 | ✅ |
| `:initargs` | **全スーパークラスの `:initarg` を合わせた和集合** | 📋 | ✅ |
| `:type` | `and` で合成 (例: `(and integer positive)`) | 📋 | ✅ |
| `:allocation` | CPL 最上位 (`:class` が `:instance` より優先されない) | 📋 | ✅ |
| `:readers` / `:writers` | 和集合 | 📋 | ✅ |

```lisp
;; 例: :initargs の和集合
(defclass A () ((x :initarg :a-x)))
(defclass B () ((x :initarg :b-x)))
(defclass C (A B) ())
;; C のスロット x には :a-x と :b-x 両方が有効
(make-instance 'C :a-x 1)  ; OK
(make-instance 'C :b-x 1)  ; OK (SBCL)
;; cl-cc は :initargs を和集合にする処理が未実装
```

### 6.3 メタクラスプロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `standard-class` (デフォルトメタクラス) | 📋 FR-528 | ✅ | |
| `funcallable-standard-class` | 📋 | ✅ | callable インスタンス (CLOS GF の実装基盤) |
| `built-in-class` | 📋 | ✅ | `integer` 等の組み込み型クラス |
| `structure-class` | 📋 | ✅ | `defstruct` 由来 |
| カスタムメタクラス | 📋 | ✅ | `(:metaclass my-meta)` |
| `validate-superclass` | 📋 | ✅ | MOP: メタクラス互換性検査 |
| `compute-class-precedence-list` (MOP GF) | 📋 | ✅ | C3 のカスタマイズポイント |
| `compute-slots` (MOP GF) | 📋 | ✅ | スロットレイアウト決定 |
| `compute-effective-slot-definition` | 📋 | ✅ | スロット定義マージ |
| `direct-slot-definition-class` | 📋 | ✅ | |
| `effective-slot-definition-class` | 📋 | ✅ | |

### 6.4 ジェネリック関数 MOP

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `compute-discriminating-function` | 📋 | ✅ | ディスパッチ関数のカスタマイズ |
| `compute-effective-method` | 📋 FR-122 | ✅ | effective method 計算。キャッシュ基盤 |
| `make-method-lambda` | 📋 | ✅ | `defmethod` のコンパイル方法変更 |
| `method-function` | 📋 | ✅ | メソッド closure の取得 |
| `make-instance` (メタクラス経由) | 📋 | ✅ | MOP: `make-instance` 自体が GF |
| `compute-default-initargs` | 📋 | ✅ | MOP GF: クラスの effective default initargs を計算 |

---

## 7. 最適化 (Optimization)

### 7.1 ディスパッチキャッシュ

| 最適化 | cl-cc | SBCL | 備考 |
|--------|-------|------|------|
| Multiple dispatch memoization | 📋 FR-119 | ✅ | GF ごとに `(arg-class-tuple → method)` HT |
| Effective method caching | 📋 FR-122 | ✅ | `(gf . class-tuple) → effective-method` |
| Inline cache (IC / PIC) | 📋 | ✅ | 呼び出しサイトに1〜8エントリのキャッシュ |
| Megamorphic cache | 📋 | ✅ | 多型サイト向けグローバルキャッシュ |
| `no-applicable-method` のキャッシュ除外 | 📋 | ✅ | |

### 7.2 インスタンス表現

| 最適化 | cl-cc | SBCL | 備考 |
|--------|-------|------|------|
| **現行: ハッシュテーブル** | ✅ | — | `gethash` O(1) average, memory heavy |
| **Slot vector (FR-121)** | 📋 | ✅ | `svref` でスロットアクセス。10× 高速化見込み |
| **Hidden class / Shape (FR-214)** | 📋 | V8 style | 同一クラスインスタンスはfixed-vector共有 |
| SBCL の `standard-instance` | — | ✅ | `(instance header slots)` の3ワードレイアウト |
| Slot index (compile-time定数) | 📋 FR-121 | ✅ | スロット名→整数インデックスの静的解決 |

### 7.3 静的最適化

| 最適化 | cl-cc | SBCL | 備考 |
|--------|-------|------|------|
| 型指定スロットアクセス除去 (FR-024) | 📋 | ✅ | 静的型証明でディスパッチ除去。VOPに相当 |
| Accessor read inlining (FR-120) | 📋 | ✅ | `*accessor-slot-map*` で `(foo obj)` → `vm-slot-read` |
| `make-instance` の静的特化 (FR-123) | 📋 | ✅ | 静的クラス+固定initargsで定数スロット書き込み生成 |
| EQL 特化子の switch 生成 (FR-124) | 📋 | ✅ | クラスディスパッチ前に `cond` → `switch` |
| Dead argument elimination (FR-025) | 📋 | ✅ | 使われない引数の除去 |
| SBCL `deftransform` (メソッド特化変換) | — | ✅ | コンパイラマクロ的なメソッドインライン化 |

---

## 8. `defstruct` との統合

cl-cc は `defstruct` を `defclass` への変換として実装 (`src/expand/expander-defstruct.lisp`)。
SBCL は別の `structure-class` メタクラスを使う点で異なる。

**cl-cc の変換方式:**
```lisp
;; 入力
(defstruct point (x 0) (y 0))

;; 展開結果 (expand-defstruct)
(progn
  (defclass point () ((x :initarg :x :initform 0 :accessor point-x)
                      (y :initarg :y :initform 0 :accessor point-y)))
  (defun make-point (&key (x 0) (y 0)) (make-instance 'point :x x :y y))
  (defun point-p (obj) (typep obj 'point))
  'point)
```

### 8.1 `defstruct` オプション対応状況

| オプション | cl-cc | SBCL | 備考 |
|-----------|-------|------|------|
| `:conc-name` | ✅ | ✅ | アクセサ名のプレフィックス。`nil` で無プレフィックス |
| `:constructor name` | ✅ | ✅ | コンストラクタ名の変更 |
| `:constructor name boa-list` | ✅ | ✅ | BOA (By-Order-of-Arguments) ラムダリスト |
| `:include parent` | ✅ | ✅ | 親構造体の継承。`*defstruct-slot-registry*` で追跡 |
| `:predicate name` | 📋 | ✅ | 述語関数名の変更または `nil` で抑制 |
| `:copier name` | 📋 | ✅ | `copy-NAME` 関数の生成 |
| `:print-function` / `:print-object` | 📋 | ✅ | `print-object` メソッドの自動生成 |
| `:type vector` / `:type list` | 📋 | ✅ | CLOS インスタンスでなくベクタ/リストで格納 |
| `:named` | 📋 | ✅ | `:type` 構造体に型タグを付加 |
| 複数 `:constructor` | 📋 | ✅ | 複数コンストラクタ同時定義 |

### 8.2 SBCL `structure-class` との差異

| 項目 | cl-cc | SBCL |
|------|-------|------|
| 内部表現 | `defclass` (ハッシュテーブル) | `sb-kernel:structure-object` (固定サイズベクタ) |
| `structure-class` メタクラス | 📋 | ✅ (別クラス階層) |
| `vm-classify-arg` での構造体認識 | 📋 | ✅ |
| `copy-structure` | 📋 | ✅ |
| `structure-object` 組み込みクラス | 📋 FR-528 | ✅ |
| CLOS メソッド定義 (構造体への `defmethod`) | 🔧 | ✅ (CPL に `structure-object` が入る) |
| `defstruct` の暗黙的 sealed | 📋 | ✅ (SBCL 2.3+) |

---

## 9. インスタンス初期化プロトコルチェーン (ANSI CL 7.1)

`make-instance` の呼び出しは以下の GF チェーンを経由する。各 GF がカスタマイズポイント。

```
make-instance class &rest initargs
  │
  ├─ apply-default-initargs  (defgeneric: initargs に :default-initargs を適用)
  │
  └─ allocate-instance class &rest initargs
       │  (メモリ割り当て: cl-cc は vm-make-obj がインライン実行)
       │
       └─ initialize-instance instance &rest initargs
            │  (ユーザーが :after でカスタム初期化を行う主なポイント)
            │
            └─ shared-initialize instance slot-names &rest initargs
                 (initarg からスロットに値をセット。:before/:after でさらにカスタマイズ)
```

| GF | cl-cc | SBCL | 備考 |
|----|-------|------|------|
| `make-instance` | ✅ (macro展開) | ✅ GF | cl-cc は `vm-make-obj` 命令。GF ではない |
| `allocate-instance` | 📋 FR-524 | ✅ GF | メモリ割り当てのカスタマイズ。cl-cc は `vm-make-obj` 内にインライン |
| `initialize-instance` | 📋 FR-379 | ✅ GF | ユーザーが `:after` で初期化追加できる。cl-cc は未実装 |
| `shared-initialize` | 📋 | ✅ GF | `initialize-instance` と `reinitialize-instance` の共通ロジック |
| `reinitialize-instance` | 🔧 | ✅ GF | `macros-sequence.lisp:534` にマクロ stub あり。スロットへの initarg 適用のみ |
| `apply-default-initargs` (内部) | 📋 | ✅ | クラスの `:default-initargs` を initarg リストにマージ |
| `update-instance-for-different-class` | 🔧 FR-380 | ✅ GF | `macros-sequence.lisp:490` に stub (実質 `reinitialize-instance` に委譲) |

**`shared-initialize` の役割 (ANSI CL 7.1.4):**
```lisp
;; SLOT-NAMES = t なら全スロット、リストなら指定スロットのみ初期化
(defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
  (dolist (slot (class-slots (class-of instance)))
    (let* ((slot-name (slot-definition-name slot))
           (initarg   (find-initarg-in-slot slot initargs)))
      (cond
        ;; initarg 指定あり → スロットに設定
        (initarg (setf (slot-value instance slot-name) (second initarg)))
        ;; slot-names 対象 + initform あり → initform を評価して設定
        ((and (or (eq slot-names t) (member slot-name slot-names))
              (slot-definition-initfunction slot))
         (setf (slot-value instance slot-name)
               (funcall (slot-definition-initfunction slot))))))))
```

---

## 10. `ensure-class` / `ensure-class-using-class` (MOP)

`defclass` は内部的に `ensure-class` を呼ぶ。直接呼ぶことでプログラム的にクラスを作成・更新できる。

**cl-cc の現実装 (`macros-sequence.lisp:504`):**
```lisp
;; stub — defclass に委譲するだけ
(our-defmacro ensure-class (name &rest options)
  (let ((direct-superclasses (or (getf options :direct-superclasses) '()))
        (direct-slots (or (getf options :direct-slots) '())))
    `(defclass ,name ,direct-superclasses ,direct-slots)))
```

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `ensure-class name &key ...` | 🔧 stub | ✅ | キーワード: `:direct-superclasses` `:direct-slots` `:metaclass` `:name` 等 |
| `ensure-class-using-class existing-class name &key ...` | 📋 | ✅ MOP GF | クラスが既存かどうかで分岐するカスタマイズポイント |
| `ensure-class-using-class nil name &key ...` | 📋 | ✅ | 新規作成の場合 |
| `defclass` が `ensure-class` を呼ぶ | 📋 | ✅ | ANSI CL では `defclass` の展開が `ensure-class` に委譲 |

**SBCL での `defclass` → `ensure-class` の流れ:**
```lisp
;; defclass マクロが展開すると:
(ensure-class 'my-class
  :direct-superclasses (list (find-class 'base))
  :direct-slots '((:name x :initarg :x :readers (my-x) ...))
  :metaclass (find-class 'standard-class))
;; → ensure-class は find-class で既存クラスを検索
;; → 存在すれば ensure-class-using-class existing-class ...
;; → なければ  ensure-class-using-class nil ...
```

---

## 11. コンディションシステムとの統合

### 11.1 `define-condition` と `:report` プロトコル

`define-condition` は `defclass` のサブセットで、コンディション固有のオプションを追加する。

**cl-cc の実装 (`conditions.lisp`) — 実際に `:report` を使用中:**
```lisp
(define-condition vm-type-error (vm-error)
  ((expected-type :initarg :expected-type :reader vm-expected-type)
   (datum         :initarg :datum         :reader vm-datum))
  (:report (lambda (condition stream)
             (format stream "VM Type Error: expected ~A, got ~S"
                     (vm-expected-type condition)
                     (vm-datum condition)))))
```

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `define-condition` 基本定義 | ✅ | ✅ | `conditions.lisp` で実用中 |
| `:report lambda` | ✅ | ✅ | `(lambda (condition stream) ...)` 形式 |
| `:report string` | 📋 | ✅ | 文字列定数を直接使う略記 |
| `:report function-name` | 📋 | ✅ | シンボルで関数参照 |
| `condition` の CLOS CPL への統合 | 📋 | ✅ | cl-cc では条件とCLOS階層が未統合 |
| `make-condition` | 🔧 | ✅ | `make-instance` に委譲する stub |
| コンディションへの `slot-value` | 📋 | ✅ | |
| `condition-report` (内部) | 📋 | ✅ | `format`/`print` が `print-object` 経由で呼ぶ |
| `:default-initargs` on conditions | 📋 | ✅ | |

### 11.2 組み込みコンディションクラス階層

SBCL の標準コンディション階層 (cl-cc での実装状況):

```
condition                               📋 (未統合)
├── serious-condition                   📋
│   ├── error                           ✅ (host CL の error)
│   │   ├── simple-error                📋
│   │   ├── type-error                  🔧 vm-type-error
│   │   ├── unbound-variable            🔧 vm-unbound-variable
│   │   ├── undefined-function          🔧 vm-undefined-function
│   │   ├── unbound-slot               🔧 (slot-unbound GF が signal。FR-384)
│   │   ├── division-by-zero            🔧 vm-division-by-zero
│   │   ├── arithmetic-error            📋
│   │   ├── cell-error                  📋
│   │   ├── control-error               📋
│   │   ├── file-error                  📋
│   │   ├── package-error               📋
│   │   ├── parse-error                 📋
│   │   ├── print-not-readable          📋
│   │   ├── program-error               📋
│   │   ├── reader-error                📋
│   │   └── stream-error                📋
│   └── storage-condition              📋 (SBCL拡張)
├── warning                             ✅ (host)
│   ├── simple-warning                  📋
│   └── style-warning                   📋
└── simple-condition                    📋
```

### 11.3 コンディションシステムのプロトコル関数

| 関数 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `signal` / `error` / `warn` / `cerror` | ✅ | ✅ | VM 命令として実装 |
| `handler-bind` / `handler-case` | ✅ | ✅ | VM 命令として実装 |
| `restart-case` / `invoke-restart` | ✅ | ✅ | VM 命令として実装 |
| `compute-restarts` | 📋 | ✅ | 現在の再起動リストを返す |
| `find-restart` | 📋 | ✅ | 名前で再起動を検索 |
| `condition-report` (内部 GF) | 📋 | ✅ | `:report` を呼ぶ |
| `print-object` for conditions | 📋 | ✅ | `#<TYPE-ERROR ...>` 形式 |

---

## 12. `print-object` と出力プロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `print-object` GF | 📋 | ✅ | オブジェクトの表示カスタマイズ |
| `print-unreadable-object` マクロ | 📋 | ✅ | `#<...>` 形式の出力 |
| デフォルト `print-object` for `standard-object` | 📋 | ✅ | `#<ClassName {addr}>` |
| `*print-readably*` との連携 | 📋 | ✅ | `t` のとき readable 出力か `print-not-readable` を signal |
| `~A` / `~S` と `print-object` | 📋 | ✅ | `format` の `~A`/`~S` は内部で `write` → `print-object` を呼ぶ |
| `with-standard-io-syntax` 下での挙動 | 📋 | ✅ | print 制御変数をリセット。`print-object` は影響を受ける |

**`*print-readably*` への対応パターン:**
```lisp
(defmethod print-object ((p point) stream)
  (if *print-readably*
      ;; readable: (make-instance 'point :x 1 :y 2) として出力
      (format stream "#.(make-instance '~S :x ~S :y ~S)"
              (class-name (class-of p)) (point-x p) (point-y p))
      ;; unreadable: #<POINT (1, 2)>
      (print-unreadable-object (p stream :type t)
        (format stream "(~S, ~S)" (point-x p) (point-y p)))))
```

---

## 12b. `defclass` 前方参照と確定 (Finalization)

| 挙動 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| スーパークラス未定義での `defclass` | 🔧 | ✅ | SBCL は前方参照を許容し `finalize-inheritance` を遅延 |
| `class-finalized-p` が `nil` の状態 | 📋 | ✅ | スーパークラスが未確定のクラスは `finalized-p = nil` |
| `finalize-inheritance` の自動呼び出し | 📋 | ✅ | `make-instance` 前に自動フィナライズ |
| 循環継承 | — | ✅ エラー | `(defclass A (B)) (defclass B (A))` → CPL 矛盾 |
| `make-instance` on unfinalized class | 📋 | ✅ エラー | SBCL は自動フィナライズを試みるがスーパーが未定義ならエラー |

---

## 12c. initarg 検証プロトコル (ANSI CL 7.1.2)

`make-instance` に渡す initarg が有効か検証される。無効な場合 `error`。

**有効な initarg の条件 (いずれか):**
1. クラスのスロット定義 (継承含む) の `:initarg` に含まれる
2. `initialize-instance`・`reinitialize-instance`・`shared-initialize` の applicable method の `&key` 引数に含まれる
3. `&allow-other-keys` が有効

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| 無効 initarg の検出と `error` | 📋 | ✅ | cl-cc は `vm-make-obj` で検証なし。未知 initarg は黙って無視 |
| `&allow-other-keys` で検証スキップ | 📋 | ✅ | `make-instance` の引数リストに `:allow-other-keys t` |
| `initialize-instance` の `&key` も有効 initarg | 📋 | ✅ | GF のメソッドを走査する必要がある |

```lisp
;; SBCL での挙動
(defclass foo () ((x :initarg :x)))
(make-instance 'foo :y 1)  ; ERROR: invalid initarg :y
(make-instance 'foo :y 1 :allow-other-keys t)  ; OK: 検証スキップ
```

---

## 実装優先度マトリクス

| 優先度 | FR番号 | 機能 | 効果 | 難易度 |
|--------|--------|------|------|--------|
| **P0** | FR-378 | C3 線形化 CPL | 正確性 (ダイアモンド継承) | Medium |
| **P0** | FR-383 | qualifier AST 保持 | FR-215 の前提条件 | Easy |
| **P0** | FR-381 | `vm-classify-arg` 全型対応 | 型安全なディスパッチ | Easy |
| **P1** | FR-215 | `:before`/`:after`/`:around` | 標準メソッドコンビネーション | Hard |
| **P1** | FR-119 | Multiple dispatch memoization | 最高ROI の性能改善 | Easy |
| **P1** | FR-120 | Accessor read inlining | `*accessor-slot-map*` 利用 | Easy |
| **P1** | FR-379 | `initialize-instance` GF | `:after` 初期化が使える | Hard |
| **P2** | FR-121 | Slot vector 表現 | スロットアクセス 10× | Hard |
| **P2** | FR-374 | `:allocation :class` | 共有スロット | Medium |
| **P2** | FR-375 | `defclass` クラスオプション | `:default-initargs` 等 | Medium |
| **P2** | FR-124 | EQL 特化子 | ディスパッチ完全性 | Medium |
| **P2** | FR-377 | `compute-applicable-methods` 公開 | MOP 基盤 | Medium |
| **P3** | FR-376 | `define-method-combination` | 高度なメソッドコンビネーション | Hard |
| **P3** | FR-523 | `make-instances-obsolete` | クラス再定義プロトコル | Very Hard |
| **P3** | FR-214 | Hidden class / Shape | V8スタイル最適化 | Very Hard |

---

---

## 13. クラスレジストリ (`find-class`)

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `find-class name` | 📋 | ✅ | グローバルクラスレジストリ検索。cl-cc は `vm-class-registry` HT を持つが公開 GF なし |
| `(setf find-class)` | 📋 | ✅ | クラスの登録・置換。`defclass` の内部実装がこれを呼ぶ |
| `find-class name nil` (エラーなし) | 📋 | ✅ | 第2引数 `nil` でクラス不在時に `nil` 返却 |
| `(setf class-name)` | 📋 | ✅ | クラスのリネーム。レジストリを更新 |
| クラス再定義セマンティクス | 📋 | ✅ | 後述 §14 参照 |

**cl-cc の内部レジストリ:**
```lisp
;; vm-state 内の vm-class-registry は HT (symbol → class-HT)
;; defclass コンパイル時に codegen-clos.lisp:39 で登録
(setf (gethash name (ctx-global-classes ctx)) dst)
;; ユーザーからは find-class で参照不可 — 公開 GF 未実装
```

---

## 14. クラス再定義プロトコル (Class Redefinition)

既存クラスを `defclass` で再定義した場合の動作。SBCL は遅延マイグレーション (lazy migration) を採用。

| 段階 | cl-cc | SBCL | 説明 |
|------|-------|------|------|
| 新クラス定義で旧クラスを置換 | 🔧 | ✅ | cl-cc は HT を上書き。古いインスタンスへの参照が断ち切れる |
| `make-instances-obsolete` 自動呼び出し | 📋 FR-523 | ✅ | 既存インスタンスを「陳腐化」マーク |
| 遅延スロットマイグレーション | 📋 FR-523 | ✅ | 次回スロットアクセス時に自動的に `update-instance-for-redefined-class` を呼ぶ |
| `update-instance-for-redefined-class` GF | 📋 FR-523 | ✅ | 追加スロット初期化・削除スロット後処理 |
| スロット追加時のデフォルト初期化 | 📋 | ✅ | 新スロットに initform を適用 |
| スロット削除時の廃棄済みスロット値 | 📋 | ✅ | `discarded-slots` 引数でアクセス可能 (移行中のみ) |

**SBCL の実装方式:**
```
インスタンスの先頭ワード = "wrapper" ポインタ
wrapper が無効化 (invalidated) されたら → 次のスロットアクセス時に trap → migrate
```

---

## 15. Funcallable インスタンス

CLOS の汎関数自体が CLOS オブジェクト (GF は `funcallable-standard-class` のインスタンス) である。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `funcallable-standard-class` メタクラス | 📋 | ✅ | `(:metaclass funcallable-standard-class)` で定義 |
| `funcallable-standard-object` | 📋 | ✅ | callable なCLOSインスタンスのスーパークラス |
| `set-funcallable-instance-function` | 📋 | ✅ | MOP: インスタンスの `funcall` 動作を設定 |
| GF 自体が CLOS インスタンス | 📋 | ✅ | `(class-of #'print)` → `#<STANDARD-GENERIC-FUNCTION>` |
| `funcall` で CLOS インスタンスを呼べる | 📋 | ✅ | `(funcall (make-instance 'my-functor) arg)` |

**用途の例:**
```lisp
;; カスタムディスパッチャを持つ呼び出し可能オブジェクト
(defclass my-callable ()
  ((fn :initarg :fn))
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((obj my-callable) &key)
  (set-funcallable-instance-function
   obj (lambda (&rest args) (apply (slot-value obj 'fn) args))))
```

---

## 16. MOP — Specializer プロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `specializer` クラス (抽象基底) | 📋 | ✅ | `class` と `eql-specializer` の共通スーパー |
| `class-eq-specializer` | 📋 | ✅ | SBCL 内部。厳密なクラス一致 (継承なし) |
| `eql-specializer` クラス | 📋 | ✅ | MOP オブジェクトとしての EQL 特化子 |
| `intern-eql-specializer value` | 📋 | ✅ | EQL 特化子オブジェクトのインターン (canonical化) |
| `eql-specializer-object` | 📋 | ✅ | EQL 特化子が保持する値を取得 |
| `specializer-direct-methods` | 📋 | ✅ | この特化子を使うメソッドのリスト |
| `specializer-direct-generic-functions` | 📋 | ✅ | この特化子を持つGFのリスト |
| `make-method-specializers-form` | 📋 | ✅ | コンパイラが特化子リストを正規化 |

---

## 17. MOP — `slot-value-using-class` プロトコル

最も強力なMOPフック。スロットアクセスをメタクラスでインターセプトできる。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `slot-value-using-class class obj slot-def` | 📋 | ✅ | `slot-value` の MOP フック |
| `(setf slot-value-using-class)` | 📋 | ✅ | `(setf slot-value)` の MOP フック |
| `slot-boundp-using-class class obj slot-def` | 📋 | ✅ | `slot-boundp` の MOP フック |
| `slot-makunbound-using-class class obj slot-def` | 📋 | ✅ | `slot-makunbound` の MOP フック |

**用途:** 永続化・遅延ロード・代理 (proxy) パターン:
```lisp
;; スロット読み取り時にDBから自動ロードするメタクラス
(defmethod slot-value-using-class ((class persistent-class) obj slot-def)
  (or (call-next-method)
      (load-from-db (slot-definition-name slot-def) (object-id obj))))
```

---

## 18. MOP — Method クラスプロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `standard-method` クラス | 📋 | ✅ | 通常の `defmethod` が生成するメソッドオブジェクト |
| `standard-accessor-method` | 📋 | ✅ | `:reader`/`:writer`/`:accessor` 由来のメソッド |
| `standard-reader-method` | 📋 | ✅ | `standard-accessor-method` のサブクラス |
| `standard-writer-method` | 📋 | ✅ | `standard-accessor-method` のサブクラス |
| `reader-method-class` | 📋 | ✅ | MOP GF: reader メソッドのクラスを返す |
| `writer-method-class` | 📋 | ✅ | MOP GF: writer メソッドのクラスを返す |
| `accessor-method-slot-definition` | 📋 | ✅ | accessor メソッドが対応するスロット定義を返す |
| `method-function` | 📋 FR-526 | ✅ | メソッドが保持する closure を取得 |
| `method-generic-function` | 📋 | ✅ | メソッドが属するGFを返す |
| `method-lambda-list` | 📋 | ✅ | メソッドのラムダリスト |
| `make-method-lambda` | 📋 | ✅ | メソッドbodyのコンパイル方法をカスタマイズ |
| `extract-lambda-list` | 📋 | ✅ | specialized lambda-list から通常 lambda-list を抽出 |
| `extract-specializer-names` | 📋 | ✅ | specialized lambda-list から特化子名を抽出 |

---

## 19. `make-load-form` — FASL シリアライズ

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `make-load-form object` GF | 📋 | ✅ | FASL にオブジェクトを埋め込むための式を返す |
| `make-load-form-saving-slots` | 📋 | ✅ | 指定スロットを保存する汎用実装 |
| `(make-load-form obj env)` 第2引数 | 📋 | ✅ | コンパイル環境。循環参照検出に使用 |
| 構造体への自動適用 | 📋 | ✅ | `defstruct` は自動的に `make-load-form` を定義 |

**使用例:**
```lisp
(defclass point ()
  ((x :initarg :x) (y :initarg :y)))

(defmethod make-load-form ((p point) &optional env)
  (make-load-form-saving-slots p :slot-names '(x y) :environment env))
;; → コンパイル時定数として point インスタンスを FASL に埋め込める
```

---

## 20. `describe-object` / `documentation` プロトコル

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `describe-object object stream` GF | 📋 | ✅ | `describe` の内部実装。デフォルトはスロット一覧表示 |
| `documentation object doc-type` GF | 📋 | ✅ | クラス・GF・メソッド・スロットのドキュメント取得 |
| `(setf documentation)` | 📋 | ✅ | ドキュメント文字列の設定 |
| `doc-type` の種類 | — | — | `t` / `'function` / `'variable` / `'type` / `'structure` |

---

## 21. SBCL 固有拡張 (2026年現在)

### 21.1 `sb-mop` パッケージ

SBCL は ANSI CL にない MOP 機能を `sb-mop` で提供 (`closer-mop` 経由が推奨)。

| 機能 | SBCL | 備考 |
|------|------|------|
| `sb-mop:class-direct-slots` | ✅ | direct slot definitions のリスト |
| `sb-mop:class-slots` | ✅ | effective slot definitions のリスト |
| `sb-mop:slot-definition-location` | ✅ | スロットのメモリ位置 (インデックス) |
| `sb-mop:funcallable-standard-class` | ✅ | |
| `sb-mop:set-funcallable-instance-function` | ✅ | |

### 21.2 Sealed Classes (SBCL 2.3+)

```lisp
;; クラスの継承・メソッド追加を封じて静的最適化を許可
(sb-ext:define-sealed-type point)
;; → コンパイラはディスパッチを静的解決できる
```

| 機能 | SBCL | cl-cc | 備考 |
|------|------|-------|------|
| `define-sealed-type` | ✅ 2.3+ | 📋 | 封印クラス。追加サブクラス・メソッドを禁止 |
| Sealed クラスの静的ディスパッチ | ✅ | 📋 | `call` → `direct-call` への変換 |
| `defstruct` の暗黙的 sealing | ✅ | 📋 | 構造体は常に sealed 相当 |

### 21.3 Satiated Generic Functions (SBCL)

```lisp
;; 全ディスパッチパスを事前展開してキャッシュ
(sb-pcl::satiating-gfs-p *my-gf*)
```

| 機能 | SBCL | cl-cc | 備考 |
|------|------|-------|------|
| Satiated GF (全ディスパッチ事前展開) | ✅ | 📋 | GF の全メソッドが確定したとき effective method を全て事前計算 |
| `compile-time-value` GF | ✅ | 📋 | コンパイル時定数折り畳みとCLOSの統合 |

### 21.4 `closer-mop` (ポータビリティ層)

SBCL/CCL/ABCL/ECL の MOP 差異を吸収するライブラリ。

| 機能 | 備考 |
|------|------|
| `closer-mop:class-direct-slots` | 実装間の API 統一 |
| `closer-mop:ensure-finalized` | `finalize-inheritance` の portable ラッパー |
| `closer-mop:slot-definition-*` | 全実装で同一 API |

---

## 26. 実装優先度マトリクス

| 優先度 | FR番号 | 機能 | 効果 | 難易度 |
|--------|--------|------|------|--------|
| **P0** | FR-378 | C3 線形化 CPL | 正確性 (ダイアモンド継承) | Medium |
| **P0** | FR-383 | qualifier AST 保持 | FR-215 の前提条件 | Easy |
| **P0** | FR-381 | `vm-classify-arg` 全型対応 | 型安全なディスパッチ | Easy |
| **P0** | — | `find-class` 公開 | MOP 全機能の前提 | Easy |
| **P1** | FR-215 | `:before`/`:after`/`:around` | 標準メソッドコンビネーション | Hard |
| **P1** | FR-119 | Multiple dispatch memoization | 最高ROI の性能改善 | Easy |
| **P1** | FR-120 | Accessor read inlining | `*accessor-slot-map*` 利用 | Easy |
| **P1** | FR-379 | `initialize-instance` GF | `:after` 初期化が使える | Hard |
| **P1** | FR-384 | `slot-unbound` GF | 標準エラープロトコル | Medium |
| **P2** | FR-121 | Slot vector 表現 | スロットアクセス 10× | Hard |
| **P2** | FR-374 | `:allocation :class` | 共有スロット | Medium |
| **P2** | FR-375 | `defclass` クラスオプション | `:default-initargs` 等 | Medium |
| **P2** | FR-124 | EQL 特化子 | ディスパッチ完全性 | Medium |
| **P2** | FR-377 | `compute-applicable-methods` 公開 | MOP 基盤 | Medium |
| **P2** | — | `slot-value-using-class` | MOP スロットフック | Hard |
| **P2** | — | `make-load-form` | FASL 埋め込み | Medium |
| **P3** | FR-376 | `define-method-combination` | 高度なメソッドコンビネーション | Hard |
| **P3** | FR-523 | `make-instances-obsolete` | クラス再定義プロトコル | Very Hard |
| **P3** | — | `funcallable-standard-class` | callable CLOS インスタンス | Very Hard |
| **P3** | FR-214 | Hidden class / Shape | V8スタイル最適化 | Very Hard |

---

---

## 22. `&key` とジェネリック関数 (ANSI CL 7.6.5)

`&key` を使うメソッドが混在する場合の規則は通常の関数より複雑。

### 22.1 `&key` の合法性規則

| ルール | cl-cc | SBCL | 説明 |
|--------|-------|------|------|
| GF が `&key` を持つ → 全メソッドも `&key` 受け入れ必須 | 📋 | ✅ エラー | ラムダリスト適合規則 (§2.5) の延長 |
| 任意メソッドが `&key` を持つ → GF も `&key` 対応が必要 | 📋 | ✅ | |
| **全メソッドは暗黙的に `&allow-other-keys`** | 📋 | ✅ | GF 呼び出し時、有効メソッドのキーワードの和集合が許可される |
| メソッドの `&key` キー名の非一致 | — | ✅ 許容 | 各メソッドが知らない `&key` は `&allow-other-keys` で無視 |
| `&allow-other-keys` の明示 | 📋 | ✅ | 明示すれば任意のキーを受け入れる |

**重要な特別規則 (ANSI CL 7.6.5):**
```lisp
(defgeneric f (a &key))
(defmethod f ((a integer) &key x) ...)   ; :x を知る
(defmethod f ((a string)  &key y) ...)   ; :y を知る

;; 呼び出し側: (f 42 :x 1 :y 2) — 両方合法
;; effective method に含まれる全メソッドのキー名の和集合が許容される
;; → integer メソッドだけが適用されても :y は合法 (暗黙の &allow-other-keys)
```

### 22.2 `&rest` + `&key` の組み合わせ

```lisp
;; &rest と &key を組み合わせる慣用形
(defmethod process ((x t) &rest args &key verbose &allow-other-keys)
  (when verbose (format t "Processing ~S~%" x))
  (apply #'call-next-method x args))  ; 全キーワードを次メソッドに転送
```

| パターン | cl-cc | SBCL | 備考 |
|---------|-------|------|------|
| `&rest args &key k1 k2` | 📋 | ✅ | `args` に全キーワードが入り、`k1`/`k2` が個別バインド |
| `apply #'call-next-method` | 📋 | ✅ | `&rest` を次メソッドへ転送する慣用形 |

---

## 23. ジェネリック関数の First-Class 利用

GF は通常の関数と同じく first-class 値として使える。

**cl-cc での判定 (`vm.lisp:443`):**
```lisp
(defun vm-generic-function-p (value)
  ;; :__methods__ キーを持つ HT = GF と判定
  (and (hash-table-p value) (gethash :__methods__ value) t))
;; %vm-dispatch-call (vm-execute.lisp:145) が呼び出し前にこれで分岐
```

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `#'my-generic-fn` で GF を値として取得 | ✅ | ✅ | `vm-generic-function-p` で識別 |
| `(funcall #'my-gf arg)` | ✅ | ✅ | `%vm-dispatch-call` が GF ディスパッチに分岐 |
| `(apply #'my-gf list)` | ✅ | ✅ | `vm-execute.lisp:505` で対応 |
| `(fdefinition 'my-gf)` | 📋 | ✅ | GF オブジェクト (SBCL では `standard-generic-function` インスタンス) を返す |
| `(symbol-function 'my-gf)` | 📋 | ✅ | `fdefinition` と等価 |
| `(typep #'my-gf 'generic-function)` | 📋 | ✅ | cl-cc は HT なので `generic-function` 型なし |
| `(class-of #'my-gf)` | 📋 | ✅ SBCL: `standard-generic-function` | cl-cc では HT なのでクラス情報がない |

**`standard-generic-function` クラス:**
```
funcallable-standard-object  (MOP)
└── generic-function         (ANSI CL abstract)
    └── standard-generic-function  (ANSI CL concrete)
        ← defgeneric が生成するオブジェクトのクラス (SBCL)
        ← cl-cc では単なる HT (クラス階層未実装)
```

| MOP アクセサ | cl-cc | SBCL | 備考 |
|-------------|-------|------|------|
| `generic-function-name` | 📋 FR-526 | ✅ | |
| `generic-function-methods` | 📋 FR-526 | ✅ | |
| `generic-function-lambda-list` | 📋 | ✅ | |
| `generic-function-argument-precedence-order` | 📋 | ✅ | デフォルト: 引数の左→右順 |
| `generic-function-method-class` | 📋 | ✅ | |
| `generic-function-method-combination` | 📋 | ✅ | |
| `generic-function-declarations` | 📋 | ✅ | `defgeneric` の `declare` オプション |

---

## 24. CLOS と多値・型宣言・その他の連携

### 24.1 多値 (`values`) とメソッド

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| メソッドから `(values a b)` を返す | ✅ | ✅ | 通常の `vm-ret` が多値を伝播 |
| `:before` / `:after` の戻り値は無視 | 📋 FR-215 | ✅ | ANSI CL 7.6.6.2: qualifier 実装後に保証 |
| `(nth-value 0 (my-gf x))` | ✅ | ✅ | |
| `(multiple-value-bind (a b) (my-gf x) ...)` | ✅ | ✅ | |

### 24.2 型宣言とメソッド

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `(declare (type integer x))` in method body | 📋 | ✅ | コンパイラヒント。SBCL は最適化に利用 |
| `(the my-class obj)` とディスパッチ | 📋 | ✅ | SBCL は `the` で静的ディスパッチを試みる |
| `:type` スロットオプションの実行時チェック | 📋 | ✅ (safe policy) | `(safety 3)` で `slot-value` に型チェックを挿入 |
| `(declare (optimize (speed 3)))` で静的展開 | 📋 | ✅ | SBCL は適用可能メソッドを compile-time に解決 |

### 24.3 オブジェクト同一性・等価性

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `eq` — 同一インスタンス判定 | ✅ | ✅ | HT の `eq` 比較 |
| `eql` — CLOS インスタンスでは `eq` と同じ | ✅ | ✅ | CLOS インスタンスは数値でないので `eql` = `eq` |
| `equal` / `equalp` — デフォルトは `eq` | ✅ | ✅ | ユーザーが `defmethod` で override しない限り同一性 |
| ユーザー定義 `equal` via `defmethod` | 📋 | ✅ | SBCL は `equal` がGFでないので不可。`equalp` も同様 |
| EQL 特化子と `eql` の関係 | 📋 FR-124 | ✅ | `(eql value)` 特化子は `(eql arg value)` が真のときマッチ |

### 24.4 CLOS と `copy`

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `copy-structure` (for `defstruct`) | 📋 | ✅ | `defstruct` が自動生成 |
| CLOS インスタンスの `copy` (非標準) | — | — | ANSI CL に `copy-instance` はない |
| `(setf (slot-value copy 'x) ...)` 手動コピー | ✅ | ✅ | 標準的な方法 |
| `make-instance` + `initialize-instance :after` でのコピー | 📋 | ✅ | `:after` でコピー元スロットを移植するパターン |

---

## 25. CLOS と `setf` の統合

### 25.1 自動生成される `setf` 展開

`defclass` の `:writer` / `:accessor` は `setf` 展開を自動定義する。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `(setf (slot-value obj 'x) v)` | ✅ | ✅ | `vm-slot-write` 命令。expander.lisp で展開 |
| `(setf (accessor obj) v)` — `:accessor` 由来 | ✅ | ✅ | `expand-setf-accessor` で展開済み |
| `(setf (reader obj) v)` — `:reader` 単独 | 📋 | ✅ エラー | SBCL はコンパイルエラー。cl-cc は未チェック |
| `(writer v obj)` — `:writer` 形式 | ✅ | ✅ | writer は `(setf reader)` ではなく専用関数 |
| `defsetf` でのカスタム `setf` 定義 | 📋 | ✅ | |
| `define-setf-expander` | 📋 | ✅ | `get-setf-expansion` が返す5値を制御 |
| `get-setf-expansion` | 📋 | ✅ | 任意の場所の setf 展開を返す |

**cl-cc の現実装 (`expander.lisp`):**
```lisp
;; *accessor-slot-map* = HT (accessor-sym → (class-name . slot-name))
;; (setf (point-x p) 42) → expand-setf-accessor → vm-slot-write
```

### 25.2 `with-slots` / `with-accessors` の展開

`with-slots` は `symbol-macrolet` を使ってスロットアクセスをローカルシンボルにバインドする。

| 機能 | cl-cc | SBCL | 備考 |
|------|-------|------|------|
| `with-slots (slot ...) obj body` | ✅ | ✅ | `macros-stdlib.lisp` 実装済み |
| `with-slots` の `symbol-macrolet` 展開 | ✅ | ✅ | `(slot-value obj 'slot)` にシンボルマクロ展開 |
| `(with-slots (x y) obj (setf x 1))` | ✅ | ✅ | `setf` も `(setf (slot-value obj 'x) 1)` に展開 |
| `with-accessors (var accessor ...) obj body` | ✅ | ✅ | `(accessor obj)` にシンボルマクロ展開 |
| `with-slots` でのネスト | ✅ | ✅ | 複数オブジェクトを同時バインド不可 (単一形式) |

**展開例:**
```lisp
(with-slots (x y) point-obj
  (setf x 10 y 20))
;; → (symbol-macrolet ((x (slot-value point-obj 'x))
;;                      (y (slot-value point-obj 'y)))
;;     (setf x 10 y 20))
;; → (setf (slot-value point-obj 'x) 10
;;         (slot-value point-obj 'y) 20)
```

### 25.3 `setf` の5値プロトコル (`get-setf-expansion`)

```lisp
;; (get-setf-expansion place env) → (temps vals newvals setter getter)
(get-setf-expansion '(slot-value obj 'x) env)
;; temps:   (#:G001)          ; 一時変数
;; vals:    (obj)             ; 一時変数に束縛する式
;; newvals: (#:NEW001)        ; 新しい値を受け取る変数
;; setter:  (setf (slot-value #:G001 'x) #:NEW001)
;; getter:  (slot-value #:G001 'x)
```

---

## 参照

- **ANSI Common Lisp**: ANSI INCITS 226-1994 — Chapter 4 (Types), Chapter 7 (Objects)
- **AMOP**: *The Art of the Metaobject Protocol* (Kiczales et al., 1991)
- **SBCL Internals**: `src/pcl/` — PCL (Portable Common Loops) をベースに SBCL が拡張
- **CCL CLOS**: `level-1/l1-clos-*.lisp`
- **C3 線形化**: Barrett et al., "A Monotonic Superclass Linearization for Dylan" (OOPSLA 1996)
- **closer-mop**: https://github.com/pcostanza/closer-mop — MOP ポータビリティ層
- **cl-cc 実装**: `src/vm/vm-clos.lisp`, `src/compile/codegen-clos.lisp`, `src/vm/vm-execute.lisp:383-457`
