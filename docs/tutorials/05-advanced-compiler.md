# チュートリアル: 発展編 - S式Prologによる型推論とProperty-Based Testing

## 🎯 このチュートリアルで学ぶこと

コンパイラには、堅牢な型システムと徹底的なテストが不可欠です。このチュートリアルでは：

1. **S式Prolog統合** - 論理型プログラミングによる型推論
2. **制約解決システム** - 複雑な型制約の解決
3. **Property-Based Testing** - 数学的性質による品質保証
4. **専門的な最適化** - 型情報を活用した最適化

## 📦 S式Prologエンジンの実装

### ステップ1: 単一化（Unification）の実装

```lisp
(defpackage :cl-cc.prolog
  (:use :cl)
  (:export #:unify #:*bindings* #:variable-p #:lookup #:extend-bindings))

(in-package :cl-cc.prolog)

;; 変数は?で始まるシンボル
(defun variable-p (x)
  "Prologスタイルの変数判定"
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\?)))

;; 束縛環境
(defparameter *bindings* nil)

(defstruct binding
  var
  value)

;; 変数の値を検索
(defun lookup (var bindings)
  "変数の束縛を検索"
  (let ((binding (find var bindings :key #'binding-var)))
    (if binding
        (let ((value (binding-value binding)))
          (if (variable-p value)
              (lookup value bindings)
              value))
        var)))

;; 束縛を拡張
(defun extend-bindings (var val bindings)
  "新しい束縛を追加"
  (cons (make-binding :var var :value val) bindings))

;; 単一化アルゴリズム
(defun unify (x y &optional (bindings *bindings*))
  "2つの項を単一化"
  (cond
    ;; 同一なら成功
    ((eql x y) bindings)

    ;; xが変数なら束縛
    ((variable-p x)
     (unify-variable x y bindings))

    ;; yが変数なら束縛
    ((variable-p y)
     (unify-variable y x bindings))

    ;; 両方リストなら再帰的に単一化
    ((and (consp x) (consp y))
     (let ((bindings (unify (first x) (first y) bindings)))
       (and bindings
            (unify (rest x) (rest y) bindings))))

    ;; それ以外は失敗
    (t nil)))

(defun unify-variable (var val bindings)
  "変数を値に単一化"
  (let ((var-binding (lookup var bindings))
        (val-binding (lookup val bindings)))
    (cond
      ;; 既に束縛されている
      ((not (eq var-binding var))
       (unify var-binding val bindings))

      ;; valも束縛されている
      ((not (eq val-binding val))
       (unify var val-binding bindings))

      ;; 出現検査（occurs check）
      ((occurs-check var val bindings)
       nil)

      ;; 新しい束縛を作成
      (t (extend-bindings var val bindings)))))

(defun occurs-check (var expr bindings)
  "変数が式に出現するかチェック（無限構造を防ぐ）"
  (cond
    ((eq var expr) t)
    ((variable-p expr)
     (let ((binding (lookup expr bindings)))
       (unless (eq binding expr)
         (occurs-check var binding bindings))))
    ((consp expr)
     (or (occurs-check var (first expr) bindings)
         (occurs-check var (rest expr) bindings)))
    (t nil)))
```

### ステップ2: 推論エンジンの実装

```lisp
(defpackage :cl-cc.prolog.inference
  (:use :cl :cl-cc.prolog)
  (:export #:defrel #:query #:with-inference))

(in-package :cl-cc.prolog.inference)

;; ルールデータベース
(defparameter *rules* (make-hash-table :test 'equal))

;; 関係（述語）の定義
(defmacro defrel (name &body clauses)
  "Prolog風の関係定義"
  `(progn
     (setf (gethash ',name *rules*)
           ',(mapcar (lambda (clause)
                       (if (consp (first clause))
                           clause  ; ルール
                           (list clause)))  ; ファクト
                     clauses))
     ',name))

;; ゴールの証明
(defun prove (goals bindings)
  "ゴールリストを証明"
  (cond
    ;; すべて証明済み
    ((null goals) (list bindings))

    ;; 最初のゴールを証明
    (t (mapcan (lambda (rule-bindings)
                 (prove (rest goals) rule-bindings))
               (prove-goal (first goals) bindings)))))

(defun prove-goal (goal bindings)
  "単一ゴールを証明"
  (let* ((predicate (first goal))
         (rules (gethash predicate *rules*)))
    (mapcan (lambda (rule)
              (let ((renamed-rule (rename-variables rule)))
                (prove-rule goal renamed-rule bindings)))
            rules)))

(defun prove-rule (goal rule bindings)
  "ルールを使ってゴールを証明"
  (let ((head (first rule))
        (body (rest rule)))
    (let ((new-bindings (unify goal head bindings)))
      (when new-bindings
        (if body
            (prove (substitute-bindings body new-bindings) new-bindings)
            (list new-bindings))))))

;; 変数のリネーミング（名前の衝突を避ける）
(defparameter *var-counter* 0)

(defun rename-variables (expr)
  "変数を新しい名前にリネーム"
  (let ((renaming (make-hash-table)))
    (labels ((rename (x)
               (cond
                 ((variable-p x)
                  (or (gethash x renaming)
                      (setf (gethash x renaming)
                            (intern (format nil "?~A_~D"
                                            (symbol-name x)
                                            (incf *var-counter*))))))
                 ((consp x)
                  (cons (rename (first x))
                        (rename (rest x))))
                 (t x))))
      (rename expr))))

;; 束縛の置換
(defun substitute-bindings (expr bindings)
  "式中の変数を束縛値で置換"
  (cond
    ((variable-p expr)
     (let ((value (lookup expr bindings)))
       (if (eq value expr)
           expr
           (substitute-bindings value bindings))))
    ((consp expr)
     (cons (substitute-bindings (first expr) bindings)
           (substitute-bindings (rest expr) bindings)))
    (t expr)))

;; クエリインターフェース
(defmacro query (&body goals)
  "Prologスタイルのクエリ"
  `(prove ',goals nil))
```

## 🔬 型推論システムの実装

### ステップ3: 型システムの定義

```lisp
(defpackage :cl-cc.types
  (:use :cl :cl-cc.prolog :cl-cc.prolog.inference)
  (:export #:infer-type #:type-check))

(in-package :cl-cc.types)

;; 型の定義
(defrel type-of
  ;; 基本型
  ((type-of ?n int) :- (integer-p ?n))
  ((type-of ?s string) :- (string-p ?s))
  ((type-of ?b bool) :- (boolean-p ?b))

  ;; 算術演算の型
  ((type-of (+ ?x ?y) int) :-
   (type-of ?x int)
   (type-of ?y int))

  ((type-of (- ?x ?y) int) :-
   (type-of ?x int)
   (type-of ?y int))

  ((type-of (* ?x ?y) int) :-
   (type-of ?x int)
   (type-of ?y int))

  ((type-of (/ ?x ?y) float) :-
   (type-of ?x numeric)
   (type-of ?y numeric))

  ;; 比較演算
  ((type-of (< ?x ?y) bool) :-
   (type-of ?x numeric)
   (type-of ?y numeric))

  ;; 条件分岐
  ((type-of (if ?cond ?then ?else) ?type) :-
   (type-of ?cond bool)
   (type-of ?then ?type)
   (type-of ?else ?type))

  ;; 関数型
  ((type-of (lambda ?params ?body) (-> ?param-types ?return-type)) :-
   (extract-param-types ?params ?param-types)
   (type-of ?body ?return-type))

  ;; 関数適用
  ((type-of (?f . ?args) ?return-type) :-
   (type-of ?f (-> ?param-types ?return-type))
   (check-arg-types ?args ?param-types)))

;; 補助述語
(defrel numeric
  ((numeric int))
  ((numeric float)))

(defrel integer-p
  ((integer-p ?x) :- (numberp ?x) (integerp ?x)))

(defrel string-p
  ((string-p ?x) :- (stringp ?x)))

(defrel boolean-p
  ((boolean-p t))
  ((boolean-p nil)))

;; 型推論関数
(defun infer-type (expr)
  "式の型を推論"
  (let ((type-var (intern "?TYPE")))
    (let ((results (query (type-of ,expr ,type-var))))
      (when results
        (substitute-bindings type-var (first results))))))

;; 型チェック
(defun type-check (expr expected-type)
  "式が期待される型を持つかチェック"
  (let ((inferred (infer-type expr)))
    (unify inferred expected-type)))
```

## 🧪 Property-Based Testingの実装

### ステップ4: ジェネレータとプロパティ

```lisp
(defpackage :cl-cc.pbt
  (:use :cl)
  (:export #:defgenerator #:defproperty #:quickcheck #:for-all))

(in-package :cl-cc.pbt)

;; ランダムデータジェネレータ
(defclass generator ()
  ((generate-fn :initarg :generate-fn
                :accessor generator-fn)
   (shrink-fn :initarg :shrink-fn
              :initform #'identity
              :accessor shrink-fn)))

(defmethod generate ((gen generator) &optional (size 100))
  "ランダムデータを生成"
  (funcall (generator-fn gen) size))

(defmethod shrink ((gen generator) value)
  "失敗時に値を縮小"
  (funcall (shrink-fn gen) value))

;; 基本的なジェネレータ
(defparameter *gen-int*
  (make-instance 'generator
    :generate-fn (lambda (size)
                   (- (random (* 2 size)) size))
    :shrink-fn (lambda (n)
                 (cond
                   ((zerop n) nil)
                   ((plusp n) (list (1- n) (floor n 2)))
                   (t (list (1+ n) (- (floor n 2))))))))

(defparameter *gen-bool*
  (make-instance 'generator
    :generate-fn (lambda (_) (zerop (random 2)))
    :shrink-fn (lambda (_) nil)))

(defparameter *gen-list*
  (make-instance 'generator
    :generate-fn (lambda (size)
                   (loop repeat (random size)
                         collect (generate *gen-int* size)))
    :shrink-fn (lambda (lst)
                 (when (> (length lst) 0)
                   (list (rest lst)
                         (butlast lst))))))

;; ジェネレータコンビネータ
(defun gen-tuple (&rest generators)
  "タプルジェネレータ"
  (make-instance 'generator
    :generate-fn (lambda (size)
                   (mapcar (lambda (g) (generate g size))
                           generators))))

(defun gen-one-of (&rest generators)
  "選択ジェネレータ"
  (make-instance 'generator
    :generate-fn (lambda (size)
                   (generate (nth (random (length generators))
                                  generators)
                             size))))

(defun gen-such-that (gen pred)
  "条件付きジェネレータ"
  (make-instance 'generator
    :generate-fn (lambda (size)
                   (loop for value = (generate gen size)
                         when (funcall pred value)
                         return value))))

;; プロパティの定義
(defstruct property
  name
  generators
  predicate)

(defmacro defproperty (name generators &body body)
  "プロパティを定義"
  `(defparameter ,name
     (make-property
       :name ',name
       :generators ,generators
       :predicate (lambda ,(mapcar #'first generators)
                    ,@body))))

;; プロパティテスト実行
(defun quickcheck (property &key (trials 100) (max-shrinks 100))
  "プロパティをテスト"
  (loop repeat trials
        for test-case = (mapcar (lambda (gen-spec)
                                   (generate (second gen-spec)))
                                 (property-generators property))
        do (handler-case
               (unless (apply (property-predicate property) test-case)
                 (let ((shrunk (shrink-failure test-case
                                                property
                                                max-shrinks)))
                   (return (values nil shrunk))))
             (error (e)
               (return (values nil test-case e)))))
  (values t nil))

(defun shrink-failure (failing-case property max-shrinks)
  "失敗ケースを最小化"
  (loop with current = failing-case
        repeat max-shrinks
        for shrunk-cases = (shrink-candidates current property)
        for smaller = (find-if (lambda (c)
                                  (not (apply (property-predicate property) c)))
                                shrunk-cases)
        while smaller
        do (setf current smaller)
        finally (return current)))

;; マクロによる簡潔な記述
(defmacro for-all (bindings &body body)
  "Property-Based Testingのための簡潔な記法"
  `(quickcheck
     (make-property
       :generators (list ,@(mapcar (lambda (b)
                                      `(list ',(first b) ,(second b)))
                                    bindings))
       :predicate (lambda ,(mapcar #'first bindings)
                    ,@body))))
```

### ステップ5: コンパイラのプロパティテスト

```lisp
(defpackage :cl-cc.compiler-tests
  (:use :cl :cl-cc.pbt :cl-cc.compiler))

(in-package :cl-cc.compiler-tests)

;; ASTジェネレータ
(defparameter *gen-ast*
  (make-instance 'generator
    :generate-fn
    (lambda (size)
      (if (< size 2)
          (generate *gen-int* size)
          (case (random 4)
            (0 (generate *gen-int* size))
            (1 `(+ ,(generate *gen-ast* (floor size 2))
                   ,(generate *gen-ast* (floor size 2))))
            (2 `(- ,(generate *gen-ast* (floor size 2))
                   ,(generate *gen-ast* (floor size 2))))
            (3 `(if ,(generate *gen-bool* 1)
                    ,(generate *gen-ast* (floor size 2))
                    ,(generate *gen-ast* (floor size 2)))))))))

;; プロパティ: コンパイルと実行の意味保存
(defproperty compiler-preserves-semantics
  ((ast *gen-ast*))
  (let* ((interpreted-result (interpret ast))
         (compiled-result (execute (compile-expr ast))))
    (equal interpreted-result compiled-result)))

;; プロパティ: 型安全性
(defproperty type-safety
  ((ast *gen-ast*))
  (let ((inferred-type (infer-type ast)))
    (when inferred-type
      (typep (interpret ast) inferred-type))))

;; プロパティ: 最適化の正当性
(defproperty optimization-correctness
  ((ast *gen-ast*))
  (let* ((unoptimized (compile-expr ast :optimize nil))
         (optimized (compile-expr ast :optimize t))
         (result1 (execute unoptimized))
         (result2 (execute optimized)))
    (equal result1 result2)))

;; プロパティ: 定数畳み込みの効果
(defproperty constant-folding-reduces-instructions
  ((ast *gen-const-expr*))
  (<= (count-instructions (compile-expr ast :optimize t))
      (count-instructions (compile-expr ast :optimize nil))))

;; プロパティ: SSA形式の単一代入
(defproperty ssa-single-assignment
  ((ast *gen-ast*))
  (let ((ssa-ir (to-ssa (compile-to-ir ast))))
    (no-duplicate-assignments-p ssa-ir)))

;; テスト実行
(defun run-all-property-tests ()
  "すべてのプロパティテストを実行"
  (format t "~%===== Property-Based Testing Results =====~%")

  (dolist (prop '(compiler-preserves-semantics
                  type-safety
                  optimization-correctness
                  constant-folding-reduces-instructions
                  ssa-single-assignment))
    (format t "~%Testing ~A..." prop)
    (multiple-value-bind (success failure)
        (quickcheck (symbol-value prop) :trials 1000)
      (if success
          (format t " ✓ PASSED (1000 trials)")
          (format t " ✗ FAILED~%  Counterexample: ~S" failure))))

  (format t "~%~%===== Testing Complete =====~%"))
```

## 🚀 統合例: 型推論を活用した最適化

```lisp
(defpackage :cl-cc.type-directed-optimization
  (:use :cl :cl-cc.types :cl-cc.prolog.inference))

(in-package :cl-cc.type-directed-optimization)

;; 型情報を使った最適化ルール
(defrel optimize-with-type
  ;; 整数除算の最適化
  ((optimize (/ ?x ?y) (div ?x ?y)) :-
   (type-of ?x int)
   (type-of ?y int))

  ;; 定数畳み込み
  ((optimize (+ ?x ?y) ?result) :-
   (number-p ?x)
   (number-p ?y)
   (is ?result (+ ?x ?y)))

  ;; 強度削減
  ((optimize (* ?x 2) (shift-left ?x 1)) :-
   (type-of ?x int))

  ((optimize (* ?x 0) 0) :-
   (type-of ?x numeric))

  ((optimize (+ ?x 0) ?x) :-
   (type-of ?x numeric))

  ;; 条件式の最適化
  ((optimize (if t ?then ?else) ?then))
  ((optimize (if nil ?then ?else) ?else))

  ;; 型による特殊化
  ((optimize (generic-add ?x ?y) (int-add ?x ?y)) :-
   (type-of ?x int)
   (type-of ?y int))

  ((optimize (generic-add ?x ?y) (float-add ?x ?y)) :-
   (type-of ?x float)
   (type-of ?y float)))

;; 最適化パイプライン
(defun optimize-with-types (ast)
  "型推論結果を使った最適化"
  (let* ((typed-ast (annotate-types ast))
         (optimized (apply-optimization-rules typed-ast)))
    optimized))

(defun annotate-types (ast)
  "ASTに型注釈を追加"
  (cond
    ((atom ast) ast)
    (t (let ((type (infer-type ast)))
         (cons (first ast)
               (mapcar #'annotate-types (rest ast)))))))

(defun apply-optimization-rules (ast)
  "最適化ルールを適用"
  (let ((results (query (optimize ,ast ?result))))
    (if results
        (substitute-bindings '?result (first results))
        ast)))
```

## 📊 ベンチマークと性能測定

```lisp
(defpackage :cl-cc.benchmarks
  (:use :cl :cl-cc.compiler))

(in-package :cl-cc.benchmarks)

(defmacro benchmark (name &body body)
  "ベンチマークマクロ"
  `(progn
     (format t "~%Benchmark: ~A~%" ',name)
     (let ((start (get-internal-real-time)))
       (prog1 (progn ,@body)
         (let ((elapsed (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)))
           (format t "  Time: ~,3F seconds~%" elapsed))))))

;; コンパイラベンチマーク
(defun run-compiler-benchmarks ()
  "コンパイラの性能測定"
  (let ((test-programs
          '((fibonacci 20)
            (ackermann 3 10)
            (matrix-multiply 100)
            (type-inference-stress 1000))))

    (dolist (program test-programs)
      (benchmark (first program)
        (compile-and-execute program))

      ;; 最適化の効果を測定
      (let ((unopt-time 0)
            (opt-time 0))
        (benchmark "Without optimization"
          (setf unopt-time
                (measure-time
                  (compile-expr (generate-test-program program)
                                :optimize nil))))
        (benchmark "With optimization"
          (setf opt-time
                (measure-time
                  (compile-expr (generate-test-program program)
                                :optimize t))))
        (format t "  Speedup: ~,2Fx~%"
                (/ unopt-time opt-time))))))
```

## 🎯 まとめ

このチュートリアルで学んだ内容：

### 技術の統合
1. **S式Prolog** - 宣言的な型推論と制約解決
2. **Property-Based Testing** - 数学的性質による品質保証
3. **型駆動最適化** - 型情報を活用した専門的な最適化

### 実践的スキル
- 単一化アルゴリズムの実装
- 推論エンジンの構築
- ランダムテストデータ生成
- 性能測定とベンチマーク

## 📚 次のステップ

1. [並列コンパイル](../how-to/parallel-compilation.md)
2. [分散型コンパイラ](../how-to/distributed-compiler.md)
3. [JITコンパイル](../how-to/jit-compilation.md)
4. [メタサーキュラ評価器](../explanation/metacircular-evaluator.md)

## 🔥 チャレンジ課題

### 上級課題1: 依存型の実装
依存型システムを実装し、より型レベル計算を可能にする。

### 上級課題2: 段階的型付け
動的型付けと静的型付けを統合した段階的型システムを構築。

### 上級課題3: 効果システム
副作用を型レベルで追跡する効果システムを実装。

---

> "The best compiler is not just correct, but provably correct." - CL-CC Philosophy