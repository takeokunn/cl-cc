# チュートリアル3: CLOSアーキテクチャの極限

## 学習目標

このチュートリアルでは、CLOSの真の力を解き放ち、コンパイラアーキテクチャを構築します：

1. メタオブジェクトプロトコル（MOP）の完全理解
2. 多重ディスパッチによる柔軟な拡張性
3. ミックスインとアスペクト指向プログラミング
4. 動的コンパイラ最適化の実現

## 前提条件

- [チュートリアル1: 最初のコンパイラを作る](01-first-compiler.md)の完了
- [チュートリアル2: マクロシステムの力](02-macro-power.md)の完了
- CLOSの基本概念の理解

## ステップ1: メタオブジェクトプロトコルの基礎

### コンパイラメタクラスの定義

```lisp
;; src/metaclass.lisp
(in-package :cl-cc.core)

;;; コンパイラコンポーネント用のメタクラス
(defclass compiler-component-class (standard-class)
  ((optimization-strategies :initform (make-hash-table :test 'equal)
                           :accessor class-optimization-strategies
                           :documentation "クラスレベルの最適化戦略")
   (compilation-hooks :initform nil
                      :accessor class-compilation-hooks
                      :documentation "コンパイル時フック")
   (performance-hints :initform nil
                      :accessor class-performance-hints
                      :documentation "パフォーマンスヒント"))
  (:documentation "コンパイラコンポーネントのメタクラス"))

;;; メタクラスの検証
(defmethod validate-superclass ((class compiler-component-class)
                               (superclass standard-class))
  t)

(defmethod validate-superclass ((class standard-class)
                               (superclass compiler-component-class))
  nil)

;;; スロット定義の拡張
(defclass compiler-slot-definition (standard-slot-definition)
  ((compile-time-type :initarg :compile-time-type
                      :initform t
                      :accessor slot-compile-time-type
                      :documentation "コンパイル時の型情報")
   (optimization-level :initarg :optimization-level
                       :initform 0
                       :accessor slot-optimization-level
                       :documentation "スロット固有の最適化レベル")))
```

### メタクラスを使ったコンポーネント定義

```lisp
;;; 基底コンポーネントクラス
(defclass compiler-component ()
  ((id :initarg :id
       :reader component-id
       :compile-time-type 'symbol
       :optimization-level 3
       :documentation "コンポーネント識別子")
   (metadata :initform (make-hash-table)
            :accessor component-metadata
            :documentation "メタデータストレージ"))
  (:metaclass compiler-component-class)
  (:documentation "すべてのコンパイラコンポーネントの基底クラス"))

;;; メタレベルでの最適化登録
(defmethod initialize-instance :after ((class compiler-component-class) &key)
  "クラス初期化時に最適化戦略を自動登録"
  (register-optimization-strategies class))

(defun register-optimization-strategies (class)
  "クラスに最適化戦略を登録"
  (setf (gethash 'inline (class-optimization-strategies class))
        #'inline-optimization-strategy)
  (setf (gethash 'constant-propagation (class-optimization-strategies class))
        #'constant-propagation-strategy))
```

## ステップ2: 多重ディスパッチの活用

### 型システムの実装

```lisp
;; src/type-system.lisp
(in-package :cl-cc.types)

;;; 型階層の定義
(defclass cl-type ()
  ((name :initarg :name :reader type-name))
  (:metaclass compiler-component-class))

(defclass primitive-type (cl-type)
  ((size :initarg :size :reader type-size)))

(defclass composite-type (cl-type)
  ((components :initarg :components :reader type-components)))

(defclass function-type (cl-type)
  ((arg-types :initarg :arg-types :reader function-arg-types)
   (return-type :initarg :return-type :reader function-return-type)))

;;; 型推論の多重ディスパッチ
(defgeneric infer-type (ast-node context)
  (:documentation "ASTノードの型を推論"))

(defmethod infer-type ((node literal) (context compilation-context))
  (make-instance 'primitive-type
                 :name (type-of (literal-value node))
                 :size (size-of (literal-value node))))

(defmethod infer-type ((node binary-operation) (context compilation-context))
  (let ((left-type (infer-type (binary-left node) context))
        (right-type (infer-type (binary-right node) context)))
    (infer-binary-result-type (binary-operator node) left-type right-type)))

;;; 型の統一（多重ディスパッチ）
(defgeneric unify-types (type1 type2 context)
  (:documentation "2つの型を統一"))

(defmethod unify-types ((t1 primitive-type) (t2 primitive-type) context)
  (if (eq (type-name t1) (type-name t2))
      t1
      (find-common-supertype t1 t2)))

(defmethod unify-types ((t1 function-type) (t2 function-type) context)
  (make-instance 'function-type
                 :arg-types (mapcar (lambda (a1 a2)
                                      (unify-types a1 a2 context))
                                    (function-arg-types t1)
                                    (function-arg-types t2))
                 :return-type (unify-types (function-return-type t1)
                                          (function-return-type t2)
                                          context)))
```

## ステップ3: ミックスインによる機能合成

### コンパイラミックスインの設計

```lisp
;; src/mixins.lisp
(in-package :cl-cc.mixins)

;;; 最適化可能ミックスイン
(defclass optimizable-mixin ()
  ((optimization-level :initarg :optimization-level
                       :initform 0
                       :accessor optimization-level)
   (optimization-passes :initform nil
                        :accessor optimization-passes))
  (:documentation "最適化機能を提供するミックスイン"))

(defmethod optimize-component ((component optimizable-mixin))
  "コンポーネントを最適化"
  (dolist (pass (optimization-passes component))
    (setf component (apply-optimization-pass pass component)))
  component)

;;; プロファイル可能ミックスイン
(defclass profilable-mixin ()
  ((profile-data :initform (make-hash-table)
                 :accessor profile-data)
   (profiling-enabled-p :initform nil
                        :accessor profiling-enabled-p))
  (:documentation "プロファイリング機能を提供"))

(defmethod with-profiling ((component profilable-mixin) thunk)
  "プロファイリング付きで実行"
  (if (profiling-enabled-p component)
      (let ((start-time (get-internal-real-time)))
        (multiple-value-prog1 (funcall thunk)
          (record-profile-data component
                              :time (- (get-internal-real-time) start-time))))
      (funcall thunk)))

;;; デバッグ可能ミックスイン
(defclass debuggable-mixin ()
  ((debug-info :initform nil
               :accessor debug-info)
   (breakpoints :initform nil
                :accessor breakpoints))
  (:documentation "デバッグ機能を提供"))

(defmethod add-debug-info ((component debuggable-mixin) info)
  "デバッグ情報を追加"
  (push info (debug-info component)))
```

### 多機能コンパイラクラスの構築

```lisp
;; src/advanced-compiler.lisp
(in-package :cl-cc.compiler)

;;; 専門的なコンパイラクラス
(defclass advanced-compiler (compiler-component
                             optimizable-mixin
                             profilable-mixin
                             debuggable-mixin)
  ((frontend :initarg :frontend
             :accessor compiler-frontend)
   (middle-end :initarg :middle-end
               :accessor compiler-middle-end)
   (backend :initarg :backend
            :accessor compiler-backend))
  (:metaclass compiler-component-class)
  (:documentation "すべての機能を持つ専門的なコンパイラ"))

;;; メソッドコンビネーション
(defgeneric compile-with-features (compiler source &key)
  (:method-combination progn))

(defmethod compile-with-features progn ((compiler advanced-compiler) source &key)
  "プロファイリング開始"
  (when (profiling-enabled-p compiler)
    (start-profiling compiler)))

(defmethod compile-with-features progn ((compiler advanced-compiler) source &key)
  "デバッグ情報の収集"
  (when (debug-info compiler)
    (collect-debug-info compiler source)))

(defmethod compile-with-features progn ((compiler advanced-compiler) source &key)
  "最適化の適用"
  (when (> (optimization-level compiler) 0)
    (apply-optimizations compiler)))
```

## ステップ4: 動的プロトコル拡張

### プロトコルの動的定義

```lisp
;; src/dynamic-protocol.lisp
(in-package :cl-cc.protocol)

;;; 動的プロトコル定義マクロ
(defmacro define-compiler-protocol (name &body specifications)
  "コンパイラプロトコルを動的に定義"
  `(progn
     ;; プロトコルクラスの生成
     (defclass ,name ()
       ,(extract-protocol-slots specifications)
       (:metaclass compiler-component-class))

     ;; ジェネリック関数の生成
     ,@(generate-protocol-methods name specifications)

     ;; 検証関数の生成
     (defmethod validate-protocol ((instance ,name))
       ,(generate-validation-code specifications))

     ;; 登録
     (register-protocol ',name)))

;;; 実際のプロトコル定義
(define-compiler-protocol ast-visitor-protocol
  (:method visit-node (visitor node)
   :documentation "ノードを訪問")

  (:method enter-node (visitor node)
   :documentation "ノードに入る"
   :before-methods ((record-entry visitor node)))

  (:method leave-node (visitor node)
   :documentation "ノードを出る"
   :after-methods ((record-exit visitor node)))

  (:invariant (lambda (visitor)
                (balanced-visits-p visitor))
   :documentation "enter/leaveが対応していること"))
```

## ステップ5: アスペクト指向プログラミング

### クロスカッティング関心事の実装

```lisp
;; src/aspects.lisp
(in-package :cl-cc.aspects)

;;; アスペクトの定義
(defclass compilation-aspect ()
  ((pointcuts :initform nil
              :accessor aspect-pointcuts)
   (advices :initform nil
            :accessor aspect-advices))
  (:metaclass compiler-component-class))

;;; ロギングアスペクト
(defclass logging-aspect (compilation-aspect)
  ((log-level :initarg :log-level
              :initform :info
              :accessor log-level)))

(defmethod weave-aspect ((aspect logging-aspect) (compiler advanced-compiler))
  "ロギングアスペクトを織り込む"
  (dolist (method (find-compiler-methods compiler))
    (add-method-advice method
                       :before (lambda (&rest args)
                                (log-method-entry method args))
                       :after (lambda (result &rest args)
                               (log-method-exit method result args)))))

;;; キャッシングアスペクト
(defclass caching-aspect (compilation-aspect)
  ((cache :initform (make-hash-table :test 'equal)
          :accessor aspect-cache)))

(defmethod weave-aspect ((aspect caching-aspect) (compiler advanced-compiler))
  "キャッシングアスペクトを織り込む"
  (dolist (method (find-pure-methods compiler))
    (add-method-advice method
                       :around (lambda (next-method &rest args)
                                (or (gethash args (aspect-cache aspect))
                                    (setf (gethash args (aspect-cache aspect))
                                          (apply next-method args)))))))
```

## ステップ6: メタレベル最適化

### コンパイル時最適化の実装

```lisp
;; src/meta-optimization.lisp
(in-package :cl-cc.meta)

;;; メタレベル最適化器
(defclass meta-optimizer ()
  ((rules :initform nil
          :accessor optimizer-rules)
   (statistics :initform (make-hash-table)
               :accessor optimizer-statistics))
  (:metaclass compiler-component-class))

;;; 最適化ルールの動的生成
(defmethod generate-optimization-rule ((optimizer meta-optimizer) pattern)
  "パターンから最適化ルールを生成"
  (compile nil
           `(lambda (node)
              (match node
                (,pattern
                 ,(generate-optimized-form pattern))
                (_ node)))))

;;; 自己最適化コンパイラ
(defclass self-optimizing-compiler (advanced-compiler)
  ((meta-optimizer :initform (make-instance 'meta-optimizer)
                   :reader compiler-meta-optimizer)
   (learning-enabled-p :initform t
                       :accessor learning-enabled-p))
  (:metaclass compiler-component-class))

(defmethod compile-and-learn ((compiler self-optimizing-compiler) source)
  "コンパイルしながら最適化パターンを学習"
  (let ((ast (parse source))
        (patterns (extract-patterns ast)))
    (when (learning-enabled-p compiler)
      (dolist (pattern patterns)
        (let ((rule (generate-optimization-rule
                     (compiler-meta-optimizer compiler)
                     pattern)))
          (register-optimization-rule compiler rule))))
    (compile-ast compiler ast)))
```

## ステップ7: 実践例 - 完全なコンパイラ構築

### 統合コンパイラの実装

```lisp
;; src/advanced-compiler.lisp
(in-package :cl-cc.advanced)

;;; 高いコンパイラクラス
(defclass advanced-compiler (self-optimizing-compiler)
  ((name :initform "CL-CC Advanced Compiler"
         :reader compiler-name)
   (version :initform "1.0.0"
            :reader compiler-version)
   (supported-languages :initform '(:lisp :scheme :ml :python)
                        :reader supported-languages))
  (:metaclass compiler-component-class)
  (:documentation "コンパイラ実装"))

;;; 初期化メソッド
(defmethod initialize-instance :after ((compiler advanced-compiler) &key)
  ;; アスペクトの織り込み
  (weave-aspect (make-instance 'logging-aspect) compiler)
  (weave-aspect (make-instance 'caching-aspect) compiler)

  ;; 最適化パスの登録
  (register-optimization-pass compiler 'constant-folding)
  (register-optimization-pass compiler 'dead-code-elimination)
  (register-optimization-pass compiler 'inline-expansion)

  ;; プロファイリング有効化
  (setf (profiling-enabled-p compiler) t))

;;; 統合コンパイルメソッド
(defmethod compile-advanced ((compiler advanced-compiler) source &key
                             (language :lisp)
                             (target :native)
                             (optimization 2))
  "高いコンパイル処理"
  (with-compilation-context (compiler)
    (with-profiling (compiler)
      (let* ((ast (parse-language source language))
             (typed-ast (infer-types ast compiler))
             (optimized-ast (optimize-ast typed-ast
                                          :level optimization
                                          :compiler compiler))
             (ir (generate-ir optimized-ast))
             (optimized-ir (optimize-ir ir compiler))
             (code (generate-code optimized-ir target)))

        ;; 学習
        (when (learning-enabled-p compiler)
          (learn-from-compilation compiler ast optimized-ast))

        ;; 結果を返す
        (make-compilation-result
         :code code
         :metadata (collect-compilation-metadata compiler)
         :profile (when (profiling-enabled-p compiler)
                   (get-profile-data compiler)))))))
```

## ステップ8: テストとベンチマーク

### CLOSアーキテクチャのテスト

```lisp
;; tests/clos-tests.lisp
(in-package :cl-cc.test)

;;; メタクラステスト
(deftest test-compiler-metaclass
  (let ((class (find-class 'advanced-compiler)))
    ;; メタクラスの確認
    (is (typep class 'compiler-component-class))

    ;; 最適化戦略の確認
    (is (hash-table-p (class-optimization-strategies class)))

    ;; スロット定義の確認
    (let ((slot (find-slot-definition class 'id)))
      (is (typep slot 'compiler-slot-definition))
      (is (eq (slot-compile-time-type slot) 'symbol)))))

;;; 多重ディスパッチテスト
(deftest test-multiple-dispatch
  (let ((int-type (make-instance 'primitive-type :name 'integer))
        (float-type (make-instance 'primitive-type :name 'float))
        (context (make-instance 'compilation-context)))

    ;; 同じ型の統一
    (is (eq (type-name (unify-types int-type int-type context))
            'integer))

    ;; 異なる型の統一
    (is (eq (type-name (unify-types int-type float-type context))
            'number))))

;;; アスペクトテスト
(deftest test-aspects
  (let ((compiler (make-instance 'advanced-compiler))
        (aspect (make-instance 'logging-aspect)))

    ;; アスペクトの織り込み
    (weave-aspect aspect compiler)

    ;; ログが記録されることを確認
    (with-output-to-string (*standard-output*)
      (compile-advanced compiler "(+ 1 2)"))

    (is (> (length (get-output-stream-string *standard-output*)) 0))))

;;; パフォーマンステスト
(defbenchmark benchmark-clos-dispatch
  "CLOSディスパッチのベンチマーク"
  (let ((compiler (make-instance 'advanced-compiler))
        (ast (make-test-ast :size 1000)))
    (time-it
      (dotimes (i 10000)
        (infer-type ast (make-instance 'compilation-context))))))
```

## まとめと次のステップ

### 学んだこと

1. **メタオブジェクトプロトコル**: コンパイラのメタレベル制御
2. **多重ディスパッチ**: 柔軟な型システムの実装
3. **ミックスイン**: 機能の動的合成
4. **アスペクト指向**: クロスカッティング関心事の分離
5. **動的プロトコル**: 実行時の拡張性
6. **自己最適化**: 学習するコンパイラ

### 発展課題

1. **メタ循環コンパイラ**: 自分自身をコンパイルする
2. **並列コンパイル**: CLOSベースの並列化
3. **分散コンパイル**: ネットワーク透過的なCLOS
4. **リフレクティブ最適化**: 実行時の自己変更
5. **プラグインシステム**: 動的ロード可能な拡張

### 次のチュートリアル

[→ Tutorial: S式Prolog統合](04-prolog-integration.md) - 論理プログラミングによる宣言的コンパイラ実装を学びます。

## リソース

- [📖 Reference: CLOSクラス階層](../reference/clos-hierarchy.md)
- [💡 Explanation: CLOSの活用哲学](../explanation/clos-utilization.md)
- [⚙ How-to: 動的プロトコル拡張](../how-to/extend-protocols.md)