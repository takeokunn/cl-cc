# チュートリアル2: マクロシステムの力

## 学習目標

このチュートリアルでは以下を学習します：

1. Common Lispマクロによるコンパイラ拡張
2. DSL（Domain-Specific Language）の設計と実装
3. コンパイル時メタプログラミング
4. マクロを活用した最適化技術

## 前提条件

- [チュートリアル1: 最初のコンパイラを作る](01-first-compiler.md)の完了
- Common Lispマクロの基礎知識
- メタプログラミングへの興味

## ステップ1: マクロによる言語拡張の基礎

### パターンマッチングマクロの実装

```lisp
;; src/pattern-matching.lisp
(in-package :cl-cc-tutorial)

;;; シンプルなパターンマッチングマクロ
(defmacro match (expr &body clauses)
  "パターンマッチングDSL"
  (let ((value (gensym "VALUE")))
    `(let ((,value ,expr))
       (cond
         ,@(mapcar (lambda (clause)
                     (destructuring-bind (pattern . body) clause
                       (generate-match-clause value pattern body)))
                   clauses)))))

(defun generate-match-clause (value pattern body)
  "パターンに対応する条件節を生成"
  (cond
    ;; ワイルドカード
    ((eq pattern '_)
     `(t ,@body))

    ;; 変数バインディング
    ((symbolp pattern)
     `(t (let ((,pattern ,value))
           ,@body)))

    ;; リテラル値
    ((atom pattern)
     `((equal ,value ',pattern) ,@body))

    ;; リスト構造のパターン
    ((listp pattern)
     (case (first pattern)
       ;; 型パターン
       (:type
        (let ((type (second pattern))
              (var (third pattern)))
          `((typep ,value ',type)
            (let ((,var ,value))
              ,@body))))

       ;; コンストラクタパターン
       (:cons
        (let ((head-pattern (second pattern))
              (tail-pattern (third pattern)))
          `((consp ,value)
            (match (car ,value)
              (,head-pattern
               (match (cdr ,value)
                 (,tail-pattern ,@body)))))))

       ;; クラスパターン
       (:class
        (let ((class-name (second pattern))
              (slot-patterns (cddr pattern)))
          (generate-class-pattern value class-name slot-patterns body)))

       ;; デフォルト：リストパターン
       (t `((and (listp ,value)
                 (= (length ,value) ,(length pattern)))
            (destructuring-bind ,pattern ,value
              ,@body)))))))

(defun generate-class-pattern (value class-name slot-patterns body)
  "CLOSクラスパターンの生成"
  (let ((bindings (loop for (slot-name pattern) in slot-patterns
                        collect (list pattern
                                      `(slot-value ,value ',slot-name)))))
    `((typep ,value ',class-name)
      (let ,bindings
        ,@body))))
```

## ステップ2: ASTパターンマッチングの活用

### パターンマッチングを使ったコンパイラの再実装

```lisp
;; src/pattern-compiler.lisp
(in-package :cl-cc-tutorial)

;;; パターンマッチングを使った美しいコンパイラ
(defgeneric compile-with-patterns (node)
  (:documentation "パターンマッチングを使ったコンパイル"))

(defmethod compile-with-patterns (node)
  (match node
    ;; リテラル
    ((:class literal value)
     `((push ,value)))

    ;; 二項演算
    ((:class binary-operation op left right)
     (append (compile-with-patterns left)
             (compile-with-patterns right)
             (list (operator->instruction op))))

    ;; 単項演算
    ((:class unary-operation op operand)
     (append (compile-with-patterns operand)
             (list (operator->instruction op))))

    ;; デフォルト
    (_ (error "Unknown node type: ~A" node))))

(defun operator->instruction (op)
  "演算子を命令に変換"
  (match op
    (:plus 'add)
    (:minus 'sub)
    (:multiply 'mul)
    (:divide 'div)
    (:negate 'neg)
    (_ (error "Unknown operator: ~A" op))))
```

## ステップ3: DSL構築マクロ

### コンパイラルール記述DSL

```lisp
;; src/compiler-dsl.lisp
(in-package :cl-cc-tutorial)

;;; コンパイラルールを宣言的に記述するDSL
(defmacro define-compiler-rules (name &body rules)
  "コンパイラルールDSL"
  `(progn
     (defparameter ,name
       (make-hash-table :test 'equal))

     ,@(mapcar (lambda (rule)
                 (destructuring-bind (pattern arrow &rest body) rule
                   (unless (eq arrow '=>)
                     (error "Expected => in rule"))
                   `(setf (gethash ',pattern ,name)
                          (lambda (node)
                            (match node
                              (,pattern ,@body))))))
               rules)

     (defun ,(intern (format nil "COMPILE-WITH-~A" name)) (node)
       (loop for pattern being the hash-keys of ,name
             for handler being the hash-values of ,name
             when (pattern-matches-p node pattern)
             return (funcall handler node)
             finally (error "No matching rule for ~A" node)))))

;;; 最適化ルールの定義
(define-compiler-rules *optimization-rules*
  ;; 定数畳み込み
  ((:class binary-operation :operator :plus
    :left (:class literal :value a)
    :right (:class literal :value b))
   => (make-instance 'literal :value (+ a b)))

  ((:class binary-operation :operator :multiply
    :left (:class literal :value a)
    :right (:class literal :value b))
   => (make-instance 'literal :value (* a b)))

  ;; 単位元の除去
  ((:class binary-operation :operator :plus
    :left x
    :right (:class literal :value 0))
   => x)

  ((:class binary-operation :operator :multiply
    :left x
    :right (:class literal :value 1))
   => x)

  ;; ゼロ乗算の最適化
  ((:class binary-operation :operator :multiply
    :left _
    :right (:class literal :value 0))
   => (make-instance 'literal :value 0))

  ;; 二重否定の除去
  ((:class unary-operation :operator :negate
    :operand (:class unary-operation :operator :negate :operand x))
   => x))
```

## ステップ4: 型推論マクロ

### コンパイル時型推論システム

```lisp
;; src/type-inference-macro.lisp
(in-package :cl-cc-tutorial)

;;; 型推論マクロ
(defmacro with-type-inference ((&key (strict nil)) &body body)
  "型推論を有効にするマクロ"
  `(let ((*type-inference-enabled* t)
         (*strict-typing* ,strict))
     (macrolet ((typed-let (bindings &body body)
                  `(let ,(mapcar #'process-typed-binding bindings)
                     (locally (declare (optimize (safety 3)))
                       ,@body))))
       ,@body)))

(defun process-typed-binding (binding)
  "型付きバインディングを処理"
  (destructuring-bind (var type-spec value) binding
    `(,var (the ,type-spec ,value))))

;;; 型推論器の実装
(defgeneric infer-type (node)
  (:documentation "ASTノードの型を推論"))

(defmethod infer-type ((node literal))
  (type-of (literal-value node)))

(defmethod infer-type ((node binary-operation))
  (let ((left-type (infer-type (binary-left node)))
        (right-type (infer-type (binary-right node)))
        (op (binary-operator node)))
    (match (list op left-type right-type)
      ((:plus integer integer) 'integer)
      ((:plus number number) 'number)
      ((:multiply integer integer) 'integer)
      ((:multiply number number) 'number)
      ((:divide integer integer) 'rational)
      ((:divide number number) 'number)
      (_ 't))))

;;; 型チェックマクロ
(defmacro deftyped-function (name params return-type &body body)
  "型付き関数定義マクロ"
  (let ((typed-params (mapcar (lambda (param)
                                 (if (listp param)
                                     param
                                     (list param 't)))
                               params)))
    `(defun ,name ,(mapcar #'first typed-params)
       (declare ,@(mapcar (lambda (param)
                            `(type ,(second param) ,(first param)))
                          typed-params)
                (values ,return-type))
       ,@body)))
```

## ステップ5: コンパイル時最適化マクロ

### 専門的な最適化技術

```lisp
;; src/optimization-macros.lisp
(in-package :cl-cc-tutorial)

;;; インライン展開マクロ
(defmacro define-inlinable (name params &body body)
  "インライン展開可能な関数を定義"
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)
     (define-compiler-macro ,name ,params
       ,@body)))

;;; 部分評価マクロ
(defmacro with-partial-evaluation (&body body)
  "部分評価を有効にする"
  `(macrolet ((partial-eval (expr &rest known-values)
                (partial-evaluate expr known-values)))
     ,@body))

(defun partial-evaluate (expr known-values)
  "式を部分的に評価"
  (match expr
    ;; 既知の値
    ((guard var (assoc var known-values))
     (cdr (assoc var known-values)))

    ;; 関数呼び出し
    ((list* op args)
     (let ((evaluated-args (mapcar (lambda (arg)
                                      (partial-evaluate arg known-values))
                                    args)))
       (if (every #'constantp evaluated-args)
           (apply op (mapcar #'eval evaluated-args))
           `(,op ,@evaluated-args))))

    ;; その他
    (_ expr)))

;;; ループ最適化マクロ
(defmacro optimized-loop ((&key (unroll-factor 4)) &body body)
  "ループ展開を含む最適化ループ"
  `(progn
     ,@(loop repeat unroll-factor
             collect `(progn ,@body))))

;;; SIMD風ベクトル演算マクロ
(defmacro vector-op (op vec1 vec2)
  "ベクトル演算の最適化"
  (let ((len (gensym))
        (result (gensym))
        (i (gensym)))
    `(let* ((,len (length ,vec1))
            (,result (make-array ,len)))
       (declare (type simple-vector ,result)
                (optimize (speed 3) (safety 0)))
       (loop for ,i from 0 below ,len
             do (setf (svref ,result ,i)
                      (,op (svref ,vec1 ,i)
                           (svref ,vec2 ,i))))
       ,result)))
```

## ステップ6: S式Prolog統合マクロ

### 論理プログラミングとの統合

```lisp
;; src/prolog-integration.lisp
(in-package :cl-cc-tutorial)

;;; S式Prologルール定義マクロ
(defmacro define-prolog-rules (name &body rules)
  "Prologスタイルのルール定義"
  `(progn
     (defparameter ,name '())

     ,@(mapcar (lambda (rule)
                 `(push ',rule ,name))
               rules)

     (defun ,(intern (format nil "QUERY-~A" name)) (goal)
       (prolog-query ,name goal))))

;;; 型推論ルール
(define-prolog-rules *type-inference-rules*
  ;; 基本型
  ((type ?x integer) :- (literal ?x ?v) (integer-p ?v))
  ((type ?x float) :- (literal ?x ?v) (float-p ?v))

  ;; 演算結果の型
  ((type ?expr integer)
   :- (binary-op ?expr + ?left ?right)
      (type ?left integer)
      (type ?right integer))

  ((type ?expr float)
   :- (binary-op ?expr + ?left ?right)
      (or (type ?left float)
          (type ?right float)))

  ;; 型変換
  ((can-convert ?from ?to)
   :- (subtype ?from ?to))

  ((subtype integer number))
  ((subtype float number))
  ((subtype ratio number)))

;;; Prolog統合マクロ
(defmacro with-prolog-inference (&body body)
  "Prolog推論を有効にする"
  `(let ((*prolog-enabled* t))
     (macrolet ((infer (goal)
                  `(prolog-query *type-inference-rules* ',goal))
                (assert-fact (fact)
                  `(push ',fact *type-inference-rules*)))
       ,@body)))
```

## ステップ7: テスト生成マクロ

### 自動テストケース生成

```lisp
;; src/test-generation-macros.lisp
(in-package :cl-cc-tutorial)

;;; プロパティベーステスト生成マクロ
(defmacro define-property-test (name params &body properties)
  "プロパティベーステストの定義"
  `(defun ,name ()
     (dotimes (iteration *test-iterations*)
       (let ,params
         ,@(mapcar (lambda (prop)
                     `(assert ,prop
                              nil
                              "Property failed: ~A with ~A"
                              ',prop
                              (list ,@(mapcar #'first params))))
                   properties)))))

;;; ファジングテスト生成
(defmacro define-fuzz-test (name target-function &key
                             (input-generator 'random-input)
                             (oracle nil))
  "ファジングテストの定義"
  `(defun ,name (&optional (iterations 1000))
     (loop repeat iterations
           for input = (funcall ,input-generator)
           for result = (handler-case
                            (,target-function input)
                          (error (e) (list :error e)))
           when ,(if oracle
                     `(not (funcall ,oracle input result))
                     `(eq (first result) :error))
           collect (list input result))))

;;; 生成的テスト
(defmacro generate-test-cases ((&key (count 10)) &body generators)
  "テストケースを生成"
  `(loop repeat ,count
         collect (list ,@generators)))

;;; QuickCheckスタイルのテスト
(defmacro quickcheck (property &key (trials 100) (size 10))
  "QuickCheckスタイルのプロパティテスト"
  (let ((trial (gensym))
        (result (gensym)))
    `(loop for ,trial from 1 to ,trials
           for ,result = (funcall ,property (random-input ,size))
           unless ,result
           return (format nil "Failed at trial ~D" ,trial)
           finally (return t))))
```

## ステップ8: 実践例 - 完全な最適化コンパイラ

### すべてを統合した実装

```lisp
;; src/advanced-compiler.lisp
(in-package :cl-cc-tutorial)

;;; 専門的なコンパイラの定義
(defmacro define-advanced-compiler (name &body specs)
  "完全なコンパイラを宣言的に定義"
  `(progn
     ;; パーサールール
     ,(generate-parser-from-spec
       (find-spec 'parser specs))

     ;; 型推論ルール
     ,(generate-type-rules
       (find-spec 'types specs))

     ;; 最適化ルール
     ,(generate-optimization-rules
       (find-spec 'optimizations specs))

     ;; コード生成ルール
     ,(generate-codegen-rules
       (find-spec 'codegen specs))

     ;; 統合コンパイル関数
     (defun ,name (source)
       (let* ((ast (parse-source source))
              (typed-ast (infer-types ast))
              (optimized-ast (optimize-ast typed-ast))
              (code (generate-code optimized-ast)))
         code))))

;;; 実際のコンパイラ定義
(define-advanced-compiler compile-optimized
  (:parser
   (expression := term ((+ | -) term)*)
   (term := factor ((* | /) factor)*)
   (factor := number | "(" expression ")" | "-" factor))

  (:types
   (number => integer | float | rational)
   (+ : (number number) -> number)
   (* : (number number) -> number)
   (/ : (number number) -> rational))

  (:optimizations
   ;; 定数畳み込み
   ((+ (const a) (const b)) => (const (+ a b)))
   ((* (const a) (const b)) => (const (* a b)))

   ;; 強度削減
   ((* x 2) => (<< x 1))  ; ビットシフトへの変換
   ((/ x 2) => (>> x 1))

   ;; 代数的簡約
   ((+ x 0) => x)
   ((* x 1) => x)
   ((* x 0) => 0))

  (:codegen
   (const => (push value))
   (+ => (add))
   (* => (mul))
   (- => (neg))))
```

## ステップ9: マクロデバッグツール

### マクロ展開の可視化

```lisp
;; src/macro-debugging.lisp
(in-package :cl-cc-tutorial)

;;; マクロ展開トレーサー
(defmacro trace-macro-expansion (form)
  "マクロ展開を可視化"
  (let ((expansion (macroexpand-1 form)))
    `(progn
       (format t "~%Original: ~S" ',form)
       (format t "~%Expanded: ~S" ',expansion)
       (format t "~%Result: ")
       ,expansion)))

;;; ステップ実行マクロ
(defmacro step-through (&body forms)
  "各フォームをステップ実行"
  `(progn
     ,@(mapcar (lambda (form)
                 `(progn
                    (format t "~%Executing: ~S" ',form)
                    (let ((result ,form))
                      (format t "~%Result: ~S" result)
                      result)))
               forms)))

;;; プロファイリングマクロ
(defmacro with-profiling ((&key (time t) (space nil)) &body body)
  "実行時プロファイリング"
  `(let ((start-time (get-internal-real-time))
         (start-space ,(when space '(room))))
     (unwind-protect
          (progn ,@body)
       ,(when time
          `(format t "~%Time: ~F seconds"
                   (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)))
       ,(when space
          `(progn
             (room)
             (format t "~%Space used"))))))
```

## まとめ

### 学んだ内容

1. **パターンマッチング**: 宣言的なコード記述
2. **DSL構築**: ドメイン特化言語の設計
3. **コンパイル時計算**: マクロによる最適化
4. **型推論**: 静的型付けの実装
5. **Prolog統合**: 論理プログラミングの活用
6. **テスト生成**: 自動テストケース生成
7. **デバッグツール**: マクロ開発の支援

### 実践演習

1. 独自のパターンマッチングDSLを拡張
2. 新しい最適化ルールを追加
3. 型推論を拡張して型エラーを検出
4. Prologルールで制約ソルバーを実装
5. プロパティテストを追加

### 次のステップ

[→ Tutorial: CLOSアーキテクチャ](03-clos-architecture.md) - オブジェクト指向コンパイラ設計を学びます。

## リソース

- [📖 Reference: Macro API](../reference/macro-api.md)
- [💡 Explanation: メタプログラミング理論](../explanation/metaprogramming-theory.md)
- [⚙ How-to: DSL構築ガイド](../how-to/build-dsl.md)