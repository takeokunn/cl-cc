# CL-CC 開発ガイドライン

## 理念

CL-CCの開発は、コンパイラコレクションの実現に向けて、以下の理念に基づいて行われます：

1. **Excellence over Expediency** - 妥協なき品質の追求
2. **Simplicity through Sophistication** - 洗練による単純性
3. **Test-Driven Everything** - すべてをテスト駆動で
4. **Documentation as Code** - ドキュメントもコード
5. **Community through Quality** - 品質によるコミュニティ形成

## コーディング規約

### Common Lispスタイルガイド

#### 命名規則

```lisp
;; パッケージ名：小文字、ハイフン区切り
(defpackage :cl-cc.frontend.parser)

;; クラス名：小文字、ハイフン区切り
(defclass ast-node ())

;; 関数名：動詞から始まる、述語は-pで終わる
(defun compile-source (source))
(defun valid-token-p (token))

;; 定数：+で囲む
(defconstant +max-optimization-level+ 3)

;; 特殊変数：*で囲む
(defparameter *current-compiler* nil)
(defvar *debug-mode* nil)

;; マクロ：動作を明確に表す名前
(defmacro with-compilation-context (&body body))
```

#### インデント規則

```lisp
;; 標準インデント：2スペース
(defun example-function (arg1 arg2)
  (let ((local-var (process arg1)))
    (when (valid-p local-var)
      (handle arg2))))

;; 長い引数リスト：縦に整列
(defmethod complex-method ((instance my-class)
                          (parameter1 type1)
                          (parameter2 type2)
                          &key option1
                               option2
                               (option3 default-value))
  (implementation))

;; マクロのbody：2スペース
(defmacro with-resource (resource &body body)
  `(let ((,resource (acquire-resource)))
     (unwind-protect
          (progn ,@body)
       (release-resource ,resource))))
```

### CLOSベストプラクティス

#### クラス設計

```lisp
;; 明確な継承階層
(defclass compiler-component ()
  ((name :initarg :name
         :reader component-name
         :documentation "コンポーネント名")
   (version :initarg :version
            :reader component-version
            :documentation "バージョン"))
  (:documentation "すべてのコンパイラコンポーネントの基底クラス"))

;; ミックスインの活用
(defclass optimizable-mixin ()
  ()
  (:documentation "最適化可能なコンポーネント"))

;; 多重継承の適切な使用
(defclass optimizing-compiler (compiler compiler-component optimizable-mixin)
  ())
```

#### ジェネリック関数

```lisp
;; 明確なプロトコル定義
(defgeneric compile-node (node context)
  (:documentation
   "ASTノードをコンパイル

   引数:
   - node: コンパイル対象のASTノード
   - context: コンパイルコンテキスト

   返り値:
   - コンパイル結果のIRノード"))

;; メソッドコンビネーション
(defgeneric validate-node (node)
  (:method-combination progn)
  (:documentation "ノードの検証（すべてのメソッドを実行）"))
```

### マクロ設計原則

#### 衛生的マクロ

```lisp
;; gensymによる変数衝突の回避
(defmacro with-temp-var ((&rest bindings) &body body)
  (let ((vars (mapcar (lambda (b) (gensym (symbol-name (first b))))
                      bindings)))
    `(let ,(mapcar (lambda (var binding)
                     (list var (second binding)))
                   vars bindings)
       (symbol-macrolet ,(mapcar (lambda (binding var)
                                   (list (first binding) var))
                                 bindings vars)
         ,@body))))

;; once-onlyパターン
(defmacro safe-increment (place &optional (delta 1))
  (let ((g-place (gensym))
        (g-delta (gensym)))
    `(let ((,g-place ,place)
           (,g-delta ,delta))
       (setf ,place (+ ,g-place ,g-delta)))))
```

## テスト戦略

### TDD実践

#### Red-Green-Refactorサイクル

```lisp
;; 1. RED: 失敗するテストを書く
(deftest test-constant-folding
  (let ((ast (make-binary-op :+ (make-literal 2) (make-literal 3))))
    (is (equal (optimize-ast ast)
               (make-literal 5)))))

;; 2. GREEN: テストを通す最小限の実装
(defmethod optimize-ast ((node binary-op))
  (if (and (literal-p (left node))
           (literal-p (right node)))
      (make-literal (funcall (operator node)
                            (value (left node))
                            (value (right node))))
      node))

;; 3. REFACTOR: コードを改善
(defmethod optimize-ast ((node binary-op))
  (match node
    ((binary-op :operator op
                :left (literal :value left-val)
                :right (literal :value right-val))
     (make-literal (apply-operator op left-val right-val)))
    (_ node)))
```

### Property-Based Testing実装

```lisp
;; プロパティ定義
(defproperty prop-parse-unparse-identity
  "パースしてアンパースすると元に戻る"
  (for-all ((source (gen-valid-source)))
    (let* ((ast (parse source))
           (unparsed (unparse ast))
           (reparsed (parse unparsed)))
      (ast-equal-p ast reparsed))))

;; ジェネレータの定義
(defgenerator gen-valid-source ()
  "有効なソースコードを生成"
  (one-of
    (gen-literal)
    (gen-identifier)
    (gen-binary-expression)
    (gen-function-definition)))

;; シュリンカーの実装
(defshrinker shrink-ast (ast)
  "失敗したASTを最小化"
  (match ast
    ((binary-op _ left right)
     (list left right
           (make-binary-op :+ left (make-literal 0))
           (make-binary-op :+ (make-literal 0) right)))
    (_ nil)))
```

## S式Prolog活用

### 型推論ルール記述

```lisp
;; 型推論ルールの宣言的記述
(define-prolog-rules type-inference
  ;; 基本型
  ((type ?x integer) :- (literal ?x ?v) (integerp ?v))
  ((type ?x float) :- (literal ?x ?v) (floatp ?v))
  ((type ?x string) :- (literal ?x ?v) (stringp ?v))

  ;; 複合型
  ((type ?x (function ?arg-type ?return-type))
   :- (lambda ?x ?param ?body)
      (type ?param ?arg-type)
      (type ?body ?return-type))

  ;; 型の統一
  ((unify ?t1 ?t2 ?result)
   :- (subtype ?t1 ?common)
      (subtype ?t2 ?common)
      (= ?result ?common)))

;; クエリの実行
(defun infer-type (ast)
  (query type-inference `(type ,ast ?type)))
```

## パフォーマンス最適化

### プロファイリング駆動開発

```lisp
;; プロファイリングマクロ
(defmacro with-profiling ((&key (time t) (space t) (calls t)) &body body)
  `(let ((start-time (get-internal-real-time))
         (start-consing (get-consing)))
     (trace-calls
       (unwind-protect
            (progn ,@body)
         (report-profile
          :time ,(when time '(- (get-internal-real-time) start-time))
          :space ,(when space '(- (get-consing) start-consing))
          :calls ,(when calls '(get-call-counts)))))))

;; 最適化アノテーション
(defun performance-critical-function (data)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array fixnum (*)) data))
  (loop for i fixnum from 0 below (length data)
        sum (aref data i) fixnum))
```

### メモリ管理

```lisp
;; オブジェクトプール
(defclass object-pool ()
  ((available :initform nil)
   (in-use :initform nil)
   (factory :initarg :factory)))

(defmethod acquire ((pool object-pool))
  (or (pop (slot-value pool 'available))
      (funcall (slot-value pool 'factory))))

(defmethod release ((pool object-pool) object)
  (push object (slot-value pool 'available)))

;; 弱参照の活用
(defclass cache ()
  ((table :initform (make-weak-hash-table :test 'equal))))
```

## デバッグ技法

### 専門的なデバッグ

```lisp
;; カスタム条件型
(define-condition compiler-error (error)
  ((phase :initarg :phase :reader error-phase)
   (node :initarg :node :reader error-node)
   (context :initarg :context :reader error-context))
  (:report (lambda (condition stream)
             (format stream "Compiler error in ~A phase:~%  Node: ~A~%  Context: ~A"
                     (error-phase condition)
                     (error-node condition)
                     (error-context condition)))))

;; トレースマクロ
(defmacro with-tracing ((&rest functions) &body body)
  `(let ((original-defs (list ,@(mapcar (lambda (f) `(symbol-function ',f))
                                        functions))))
     (unwind-protect
          (progn
            ,@(mapcar (lambda (f)
                        `(trace-function ',f))
                      functions)
            ,@body)
       (mapcar #'fmakunbound ',functions)
       (mapcar #'(lambda (f def) (setf (symbol-function f) def))
               ',functions original-defs))))
```

## CI/CD設定

### GitHub Actions

```yaml
name: CL-CC CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        lisp: [sbcl, ccl, ecl]

    steps:
    - uses: actions/checkout@v2

    - name: Setup Lisp
      uses: 40ants/setup-lisp@v1
      with:
        implementation: ${{ matrix.lisp }}

    - name: Run tests
      run: |
        ros run -- --load cl-cc.asd \
                  --eval '(asdf:test-system :cl-cc)'

    - name: Run property tests
      run: |
        ros run -- --load cl-cc.asd \
                  --eval '(cl-cc.test:run-property-tests)'

    - name: Performance benchmarks
      run: |
        ros run -- --load cl-cc.asd \
                  --eval '(cl-cc.bench:run-benchmarks)'
```

## ドキュメント作成

### 自動ドキュメント生成

```lisp
(defmacro defdocumented (type name &body body)
  "ドキュメント付き定義"
  (let ((doc-string (extract-docstring body))
        (examples (extract-examples body))
        (see-also (extract-see-also body)))
    `(progn
       (,type ,name ,@body)

       (register-documentation
        ',name ',type
        :description ,doc-string
        :examples ',examples
        :see-also ',see-also)

       (generate-markdown-doc ',name))))
```

## コントリビューション

### プルリクエストのテンプレート

```markdown
## 概要
変更の簡潔な説明

## 変更の種類
- [ ] バグ修正
- [ ] 新機能
- [ ] パフォーマンス改善
- [ ] リファクタリング
- [ ] ドキュメント

## チェックリスト
- [ ] テストを追加/更新した
- [ ] Property-Based Testを追加した
- [ ] ドキュメントを更新した
- [ ] パフォーマンスへの影響を測定した
- [ ] CLOSベストプラクティスに従った

## テスト結果
```lisp
(test-results)
```

## パフォーマンス影響
前: X ms
後: Y ms
改善: Z%
```

## レビュープロセス

### コードレビュー基準

1. **正しさ** - アルゴリズムは正しいか
2. **テスト** - 十分なテストカバレッジか
3. **可読性** - コードは理解しやすいか
4. **パフォーマンス** - 効率的な実装か
5. **拡張性** - 将来の変更に対応できるか

## セキュリティ

### セキュアコーディング

```lisp
;; 入力検証
(defun validate-input (input)
  (assert (and (stringp input)
               (< (length input) +max-input-length+)
               (not (contains-injection-p input)))
          (input)
          "Invalid input: ~A" input))

;; リソース制限
(defmacro with-resource-limits ((&key time memory) &body body)
  `(with-timeout ,time
     (with-memory-limit ,memory
       ,@body)))
```

## まとめ

これらのガイドラインに従うことで、CL-CCはコンパイラコレクションとしての品質を維持し、継続的に改善されていきます。すべての開発者は、これらの原則を理解し、実践することが求められます。

## 関連ドキュメント

- [📐 ARCHITECTURE](ARCHITECTURE.md) - アーキテクチャ設計
- [📚 MASTER-SSOT](MASTER-SSOT.md) - Single Source of Truth
- [→ Tutorials](tutorials/) - チュートリアル
- [📖 Reference](reference/) - APIリファレンス