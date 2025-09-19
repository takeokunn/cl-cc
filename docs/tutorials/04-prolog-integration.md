# チュートリアル: S式Prologの統合 - 論理型パラダイムによる推論エンジン

## 🎯 このチュートリアルで学ぶこと

S式ベースのPrologインタープリタをコンパイラに統合し、論理型プログラミングの力を活用します：

1. **ユニフィケーション**: パターンマッチングと変数束縛
2. **バックトラッキング**: 探索空間の効率的な探索
3. **制約充足**: 最適化問題の宣言的解決
4. **メタ推論**: コンパイラ最適化の自動導出

## 📋 前提条件

- [CLOSでの拡張](03-clos-extension.md)を完了していること
- Prologの基本概念（事実、規則、質問）を理解していること

## 🔍 ステップ1: S式Prologの基礎実装

### 1.1 データ構造の定義

```lisp
;;;; prolog-core.lisp
(in-package :cl-cc)

;; Prolog項の表現
(defclass prolog-term () ()
  (:documentation "Prolog項の基底クラス"))

(defclass variable-term (prolog-term)
  ((name :initarg :name
         :accessor var-name
         :documentation "変数名"))
  (:documentation "論理変数"))

(defclass atom-term (prolog-term)
  ((value :initarg :value
          :accessor atom-value
          :documentation "アトムの値"))
  (:documentation "アトム（定数）"))

(defclass compound-term (prolog-term)
  ((functor :initarg :functor
            :accessor term-functor
            :documentation "関手")
   (args :initarg :args
         :accessor term-args
         :documentation "引数リスト"))
  (:documentation "複合項"))

;; S式からProlog項への変換
(defun parse-term (sexp)
  "S式をProlog項に変換"
  (cond
    ;; 変数（?で始まるシンボル）
    ((and (symbolp sexp)
          (char= (char (symbol-name sexp) 0) #\?))
     (make-instance 'variable-term :name sexp))

    ;; アトム（その他のシンボルや数値）
    ((or (symbolp sexp) (numberp sexp) (stringp sexp))
     (make-instance 'atom-term :value sexp))

    ;; 複合項（リスト）
    ((listp sexp)
     (make-instance 'compound-term
                    :functor (first sexp)
                    :args (mapcar #'parse-term (rest sexp))))

    ;; エラー
    (t (error "Invalid Prolog term: ~S" sexp))))

;; 環境（変数束縛）
(defclass binding-environment ()
  ((bindings :initform (make-hash-table :test #'eq)
             :accessor env-bindings
             :documentation "変数束縛のハッシュテーブル"))
  (:documentation "変数束縛環境"))

(defmethod lookup ((env binding-environment) var)
  "変数の値を検索"
  (gethash (var-name var) (env-bindings env)))

(defmethod bind ((env binding-environment) var value)
  "変数を値に束縛"
  (setf (gethash (var-name var) (env-bindings env)) value))

(defmethod copy-environment ((env binding-environment))
  "環境のコピーを作成"
  (let ((new-env (make-instance 'binding-environment)))
    (loop for key being the hash-keys of (env-bindings env)
          using (hash-value value)
          do (setf (gethash key (env-bindings new-env)) value))
    new-env))
```

### 1.2 ユニフィケーションエンジン

```lisp
;;;; unification.lisp
(in-package :cl-cc)

(defgeneric unify (term1 term2 env)
  (:documentation "2つの項をユニファイ"))

;; 変数のユニフィケーション
(defmethod unify ((var1 variable-term) term2 env)
  (let ((binding (lookup env var1)))
    (if binding
        ;; 既に束縛されている場合
        (unify binding term2 env)
        ;; 未束縛の場合
        (if (occurs-check var1 term2 env)
            nil  ; 循環参照
            (progn
              (bind env var1 term2)
              env)))))

(defmethod unify (term1 (var2 variable-term) env)
  (unify var2 term1 env))

;; アトムのユニフィケーション
(defmethod unify ((atom1 atom-term) (atom2 atom-term) env)
  (when (equal (atom-value atom1) (atom-value atom2))
    env))

;; 複合項のユニフィケーション
(defmethod unify ((comp1 compound-term) (comp2 compound-term) env)
  (when (and (eq (term-functor comp1) (term-functor comp2))
             (= (length (term-args comp1)) (length (term-args comp2))))
    (loop for arg1 in (term-args comp1)
          for arg2 in (term-args comp2)
          do (setf env (unify arg1 arg2 env))
          unless env return nil
          finally (return env))))

;; デフォルト（失敗）
(defmethod unify (term1 term2 env)
  (declare (ignore term1 term2 env))
  nil)

;; 出現検査（occur check）
(defun occurs-check (var term env)
  "変数が項に出現するかチェック（無限ループ防止）"
  (labels ((occurs-in (t1)
             (typecase t1
               (variable-term
                (or (eq (var-name var) (var-name t1))
                    (let ((binding (lookup env t1)))
                      (and binding (occurs-in binding)))))
               (compound-term
                (some #'occurs-in (term-args t1)))
               (t nil))))
    (occurs-in term)))

;; 高度なユニフィケーション機能
(defclass unification-context ()
  ((environment :initarg :env
                :initform (make-instance 'binding-environment)
                :accessor context-env)
   (trail :initform nil
          :accessor context-trail
          :documentation "バックトラック用のトレイル")
   (choice-points :initform nil
                  :accessor context-choices
                  :documentation "選択点スタック"))
  (:documentation "ユニフィケーションコンテキスト"))

(defmethod save-choice-point ((ctx unification-context))
  "選択点を保存"
  (push (list (copy-environment (context-env ctx))
              (copy-list (context-trail ctx)))
        (context-choices ctx)))

(defmethod restore-choice-point ((ctx unification-context))
  "選択点を復元"
  (when (context-choices ctx)
    (destructuring-bind (env trail) (pop (context-choices ctx))
      (setf (context-env ctx) env
            (context-trail ctx) trail)
      t)))
```

## 🎯 ステップ2: 推論エンジンの実装

### 2.1 知識ベースと推論

```lisp
;;;; inference-engine.lisp
(in-package :cl-cc)

;; 節（clause）の表現
(defclass prolog-clause ()
  ((head :initarg :head
         :accessor clause-head
         :documentation "節の頭部")
   (body :initarg :body
         :initform nil
         :accessor clause-body
         :documentation "節の本体（ゴールのリスト）"))
  (:documentation "Prolog節"))

(defun make-clause (head &rest body)
  "節を作成"
  (make-instance 'prolog-clause
                 :head (parse-term head)
                 :body (mapcar #'parse-term body)))

;; 知識ベース
(defclass knowledge-base ()
  ((clauses :initform (make-hash-table :test #'equal)
            :accessor kb-clauses
            :documentation "述語名から節リストへのマップ")
   (indexes :initform (make-hash-table :test #'equal)
            :accessor kb-indexes
            :documentation "インデックス構造"))
  (:documentation "Prolog知識ベース"))

(defmethod assert-clause ((kb knowledge-base) clause)
  "節を知識ベースに追加"
  (let ((functor (term-functor (clause-head clause))))
    (push clause (gethash functor (kb-clauses kb) nil))
    ;; インデックスの更新
    (update-index kb clause)))

(defmethod retract-clause ((kb knowledge-base) pattern)
  "パターンにマッチする節を削除"
  (let ((functor (term-functor pattern)))
    (setf (gethash functor (kb-clauses kb))
          (remove-if (lambda (clause)
                       (unify (clause-head clause) pattern
                              (make-instance 'binding-environment)))
                     (gethash functor (kb-clauses kb))))))

;; SLD解消による推論
(defclass inference-engine ()
  ((knowledge-base :initarg :kb
                   :initform (make-instance 'knowledge-base)
                   :accessor engine-kb)
   (trace-mode :initform nil
               :accessor engine-trace
               :documentation "トレースモード")
   (depth-limit :initform 1000
                :accessor engine-depth-limit
                :documentation "推論の深さ制限"))
  (:documentation "推論エンジン"))

(defmethod prove ((engine inference-engine) goals &optional (env (make-instance 'binding-environment)))
  "ゴールリストを証明"
  (when (engine-trace engine)
    (format t "~&Proving: ~S~%" goals))

  (cond
    ;; すべてのゴールが証明された
    ((null goals)
     (list env))

    ;; 深さ制限チェック
    ((zerop (engine-depth-limit engine))
     nil)

    ;; 最初のゴールを解決
    (t
     (let ((goal (first goals))
           (remaining (rest goals))
           (solutions nil))

       ;; マッチする節を探す
       (dolist (clause (get-matching-clauses (engine-kb engine) goal))
         (let ((renamed-clause (rename-variables clause))
               (new-env (copy-environment env)))

           ;; 頭部とのユニフィケーション
           (when-let ((unified-env (unify goal
                                          (clause-head renamed-clause)
                                          new-env)))
             ;; 本体のゴールを追加して再帰的に証明
             (let ((new-goals (append (clause-body renamed-clause) remaining)))
               (with-depth-limit (engine)
                 (dolist (solution (prove engine new-goals unified-env))
                   (push solution solutions)))))))

       solutions))))

(defmacro with-depth-limit ((engine) &body body)
  "深さ制限付きで実行"
  `(let ((old-limit (engine-depth-limit ,engine)))
     (unwind-protect
         (progn
           (decf (engine-depth-limit ,engine))
           ,@body)
       (setf (engine-depth-limit ,engine) old-limit))))

(defmethod get-matching-clauses ((kb knowledge-base) goal)
  "ゴールにマッチする可能性のある節を取得"
  (gethash (term-functor goal) (kb-clauses kb) nil))

(defun rename-variables (clause)
  "節内の変数を一意にリネーム"
  (let ((renaming (make-hash-table :test #'eq))
        (counter 0))
    (labels ((rename-term (term)
               (typecase term
                 (variable-term
                  (or (gethash (var-name term) renaming)
                      (setf (gethash (var-name term) renaming)
                            (make-instance 'variable-term
                                           :name (intern (format nil "?G~D" (incf counter)))))))
                 (compound-term
                  (make-instance 'compound-term
                                 :functor (term-functor term)
                                 :args (mapcar #'rename-term (term-args term))))
                 (t term))))
      (make-instance 'prolog-clause
                     :head (rename-term (clause-head clause))
                     :body (mapcar #'rename-term (clause-body clause))))))
```

### 2.2 組み込み述語

```lisp
;;;; built-in-predicates.lisp
(in-package :cl-cc)

;; 組み込み述語の基底クラス
(defclass built-in-predicate ()
  ((name :initarg :name
         :accessor predicate-name))
  (:documentation "組み込み述語"))

(defgeneric evaluate-builtin (predicate args env)
  (:documentation "組み込み述語を評価"))

;; カット（!）
(defclass cut-predicate (built-in-predicate)
  ()
  (:default-initargs :name '!))

(defmethod evaluate-builtin ((pred cut-predicate) args env)
  "バックトラックを防ぐカット"
  (declare (ignore args))
  ;; カットフラグをセット
  (list (cons env :cut)))

;; 算術演算
(defclass is-predicate (built-in-predicate)
  ()
  (:default-initargs :name 'is))

(defmethod evaluate-builtin ((pred is-predicate) args env)
  "算術式の評価"
  (destructuring-bind (result expr) args
    (let ((value (evaluate-arithmetic expr env)))
      (when value
        (let ((new-env (unify result
                              (make-instance 'atom-term :value value)
                              env)))
          (when new-env (list new-env)))))))

(defun evaluate-arithmetic (expr env)
  "算術式を評価"
  (typecase expr
    (atom-term (atom-value expr))
    (variable-term
     (let ((binding (lookup env expr)))
       (when binding (evaluate-arithmetic binding env))))
    (compound-term
     (let ((op (term-functor expr))
           (args (mapcar (lambda (arg) (evaluate-arithmetic arg env))
                        (term-args expr))))
       (when (every #'numberp args)
         (case op
           (+ (apply #'+ args))
           (- (apply #'- args))
           (* (apply #'* args))
           (/ (apply #'/ args))
           (mod (apply #'mod args))
           (otherwise nil)))))))

;; リスト操作
(defclass append-predicate (built-in-predicate)
  ()
  (:default-initargs :name 'append))

(defmethod evaluate-builtin ((pred append-predicate) args env)
  "リストの連結"
  (destructuring-bind (list1 list2 result) args
    (cond
      ;; append([], L, L).
      ((and (atom-term-p list1)
            (null (atom-value list1)))
       (let ((new-env (unify list2 result env)))
         (when new-env (list new-env))))

      ;; append([H|T], L, [H|R]) :- append(T, L, R).
      ((compound-term-p list1)
       (let ((head (first (term-args list1)))
             (tail (second (term-args list1))))
         ;; 再帰的な処理
         (when (and head tail)
           ;; ... 実装省略
           ))))))

;; メタ述語
(defclass findall-predicate (built-in-predicate)
  ()
  (:default-initargs :name 'findall))

(defmethod evaluate-builtin ((pred findall-predicate) args env)
  "すべての解を収集"
  (destructuring-bind (template goal result) args
    (let ((solutions nil))
      ;; goalを証明してtemplateのインスタンスを収集
      (dolist (solution (prove *current-engine* (list goal) env))
        (push (instantiate-term template solution) solutions))
      ;; 結果をリストとして返す
      (let ((result-list (make-list-term (reverse solutions))))
        (let ((new-env (unify result result-list env)))
          (when new-env (list new-env)))))))
```

## 🔮 ステップ3: コンパイラ最適化への応用

### 3.1 最適化ルールの宣言的記述

```lisp
;;;; optimization-rules.lisp
(in-package :cl-cc)

(defclass optimization-rule-base (knowledge-base)
  ((rule-priorities :initform (make-hash-table :test #'equal)
                    :accessor rule-priorities))
  (:documentation "最適化ルールの知識ベース"))

;; 最適化ルールをPrologで定義
(defmacro define-optimization-rule (name &body clauses)
  "最適化ルールを宣言的に定義"
  `(progn
     ,@(mapcar (lambda (clause)
                 (destructuring-bind (head &rest body) clause
                   `(assert-clause *optimization-rules*
                                   (make-clause ',head ,@(mapcar (lambda (goal)
                                                                    `',goal)
                                                                  body)))))
               clauses)))

;; 定数畳み込みルール
(define-optimization-rule constant-folding
  ;; 加算の定数畳み込み
  ((optimize (+ ?const1 ?const2) ?result)
   (constant ?const1)
   (constant ?const2)
   (is ?result (+ ?const1 ?const2)))

  ;; 乗算の定数畳み込み
  ((optimize (* ?const1 ?const2) ?result)
   (constant ?const1)
   (constant ?const2)
   (is ?result (* ?const1 ?const2)))

  ;; 恒等元の除去
  ((optimize (+ ?x 0) ?x))
  ((optimize (* ?x 1) ?x))
  ((optimize (- ?x 0) ?x))

  ;; 強度削減
  ((optimize (* ?x 2) (shift-left ?x 1)))
  ((optimize (/ ?x 2) (shift-right ?x 1))
   (integer ?x)))

;; デッドコード除去ルール
(define-optimization-rule dead-code-elimination
  ;; 未使用変数の除去
  ((eliminate-dead (let ?var ?value ?body) ?body)
   (not (uses ?body ?var)))

  ;; 到達不可能コードの除去
  ((eliminate-dead (if true ?then ?else) ?then))
  ((eliminate-dead (if false ?then ?else) ?else))

  ;; 副作用のない式の除去
  ((eliminate-dead ?expr nil)
   (pure-expression ?expr)
   (not (used-value ?expr))))

;; ループ最適化ルール
(define-optimization-rule loop-optimization
  ;; ループ不変式の移動
  ((optimize-loop (loop ?var ?init ?test ?body)
                  (let ?invariant ?value
                    (loop ?var ?init ?test ?optimized-body)))
   (loop-invariant ?body ?invariant ?value)
   (substitute ?body ?invariant ?temp ?optimized-body))

  ;; ループ展開
  ((optimize-loop (loop ?var 0 ?n ?body)
                  (unrolled-loop ?var 0 ?n ?body 4))
   (constant ?n)
   (< ?n 100)
   (mod ?n 4 0))

  ;; ループ融合
  ((fuse-loops (seq (loop ?var ?init ?test1 ?body1)
                    (loop ?var ?init ?test2 ?body2))
               (loop ?var ?init (and ?test1 ?test2)
                     (seq ?body1 ?body2)))
   (compatible-loops ?body1 ?body2)))
```

### 3.2 推論ベースの最適化エンジン

```lisp
;;;; inference-optimizer.lisp
(in-package :cl-cc)

(defclass inference-optimizer (compiler-phase)
  ((rule-engine :initform (make-instance 'inference-engine)
                :accessor optimizer-engine)
   (statistics :initform (make-hash-table :test #'equal)
               :accessor optimizer-stats))
  (:documentation "推論ベースの最適化器"))

(defmethod initialize-instance :after ((optimizer inference-optimizer) &key)
  "最適化ルールをロード"
  (load-optimization-rules (optimizer-engine optimizer)))

(defmethod optimize-with-inference ((optimizer inference-optimizer) ast)
  "推論エンジンを使用してASTを最適化"
  (let ((optimized (apply-optimization-rules ast (optimizer-engine optimizer))))
    (record-statistics optimizer ast optimized)
    optimized))

(defun apply-optimization-rules (ast engine)
  "ASTに最適化ルールを適用"
  (let ((query (make-optimization-query ast)))
    (let ((solutions (prove engine query)))
      (if solutions
          (extract-optimized-ast (first solutions))
          ast))))

(defun make-optimization-query (ast)
  "AST用の最適化クエリを生成"
  (list (parse-term `(optimize ,(ast-to-term ast) ?result))))

(defun ast-to-term (ast)
  "ASTをProlog項に変換"
  (typecase ast
    (literal-node
     (literal-value ast))
    (binary-op-node
     `(,(binary-operator ast)
       ,(ast-to-term (binary-left ast))
       ,(ast-to-term (binary-right ast))))
    (if-expr-node
     `(if ,(ast-to-term (if-condition ast))
          ,(ast-to-term (if-then ast))
          ,(when (if-else ast)
             (ast-to-term (if-else ast)))))
    (t ast)))

;; 制約ベースの最適化
(defclass constraint-optimizer (inference-optimizer)
  ((constraints :initform nil
                :accessor optimizer-constraints))
  (:documentation "制約充足による最適化"))

(defmethod add-constraint ((optimizer constraint-optimizer) constraint)
  "制約を追加"
  (push constraint (optimizer-constraints optimizer)))

(defmethod solve-constraints ((optimizer constraint-optimizer))
  "制約充足問題を解く"
  (let ((query (build-constraint-query (optimizer-constraints optimizer))))
    (prove (optimizer-engine optimizer) query)))

;; パターンベースの変換
(defmacro define-transformation (name pattern replacement &key condition)
  "パターンベースの変換ルール"
  `(assert-clause *transformation-rules*
                  (make-clause '(transform ,pattern ,replacement)
                               ,@(when condition
                                   (list condition)))))

(define-transformation distribute-multiplication
  (* ?x (+ ?y ?z))
  (+ (* ?x ?y) (* ?x ?z)))

(define-transformation factor-common-term
  (+ (* ?x ?y) (* ?x ?z))
  (* ?x (+ ?y ?z)))

(define-transformation tail-recursion
  (function ?name ?params
            (if ?test
                ?base
                (call ?name ?recursive-args)))
  (function ?name ?params
            (tail-recursive-loop ?test ?base ?recursive-args))
  :condition (last-call-p ?name ?recursive-args))
```

## 🧮 ステップ4: 高度な推論機能

### 4.1 メタ推論とリフレクション

```lisp
;;;; meta-reasoning.lisp
(in-package :cl-cc)

(defclass meta-reasoner ()
  ((meta-rules :initform nil
               :accessor meta-rules
               :documentation "メタレベルの推論ルール")
   (performance-model :initform (make-instance 'performance-predictor)
                      :accessor reasoner-performance))
  (:documentation "メタ推論エンジン"))

(defmethod reason-about-optimization ((reasoner meta-reasoner) optimization)
  "最適化の効果を推論"
  (let ((predicted-improvement (predict-improvement
                                (reasoner-performance reasoner)
                                optimization)))
    (when (> predicted-improvement *optimization-threshold*)
      optimization)))

;; 学習機能付き推論
(defclass learning-reasoner (meta-reasoner)
  ((experience-base :initform (make-hash-table :test #'equal)
                    :accessor reasoner-experience)
   (success-rate :initform (make-hash-table :test #'equal)
                 :accessor reasoner-success-rate))
  (:documentation "経験から学習する推論エンジン"))

(defmethod learn-from-result ((reasoner learning-reasoner)
                              optimization
                              actual-improvement)
  "最適化結果から学習"
  (let ((pattern (extract-pattern optimization)))
    (push (cons pattern actual-improvement)
          (gethash pattern (reasoner-experience reasoner) nil))
    (update-success-rate reasoner pattern actual-improvement)))

;; 説明生成
(defclass explanation-generator ()
  ((trace-depth :initform 5
                :accessor explanation-depth))
  (:documentation "推論過程の説明生成"))

(defmethod generate-explanation ((generator explanation-generator)
                                 proof-tree)
  "証明木から人間が理解可能な説明を生成"
  (format nil "~{~A~%~}"
          (collect-explanation-steps proof-tree 0)))

(defun collect-explanation-steps (tree depth)
  "説明ステップを収集"
  (when (< depth *max-explanation-depth*)
    (cons (format nil "~VT- ~A"
                  (* depth 2)
                  (describe-inference-step tree))
          (mapcan (lambda (child)
                    (collect-explanation-steps child (1+ depth)))
                  (inference-children tree)))))
```

### 4.2 並列推論と分散処理

```lisp
;;;; parallel-inference.lisp
(in-package :cl-cc)

(defclass parallel-prolog-engine (inference-engine)
  ((worker-pool :initform nil
                :accessor engine-workers)
   (task-queue :initform (make-instance 'concurrent-queue)
               :accessor engine-queue)
   (result-combiner :initform #'append
                    :accessor engine-combiner))
  (:documentation "並列推論エンジン"))

(defmethod prove-parallel ((engine parallel-prolog-engine) goals env)
  "ゴールを並列に証明"
  (let ((tasks (partition-goals goals))
        (results nil))

    ;; タスクを並列実行
    (dolist (task tasks)
      (submit-task (engine-queue engine) task env))

    ;; 結果を収集
    (dotimes (i (length tasks))
      (push (receive-result engine) results))

    ;; 結果を結合
    (funcall (engine-combiner engine) results)))

(defun partition-goals (goals)
  "ゴールを並列実行可能な部分に分割"
  (let ((independent-groups nil)
        (current-group nil))
    (dolist (goal goals)
      (if (independent-from-p goal current-group)
          (push goal current-group)
          (progn
            (when current-group
              (push (reverse current-group) independent-groups))
            (setf current-group (list goal)))))
    (when current-group
      (push (reverse current-group) independent-groups))
    (reverse independent-groups)))

;; 分散推論
(defclass distributed-reasoner ()
  ((nodes :initform nil
          :accessor reasoner-nodes)
   (coordinator :initform nil
                :accessor reasoner-coordinator))
  (:documentation "分散推論システム"))

(defmethod distribute-reasoning ((reasoner distributed-reasoner) problem)
  "推論を複数ノードに分散"
  (let ((subproblems (decompose-problem problem)))
    (mapcar (lambda (node subproblem)
              (send-to-node node subproblem))
            (reasoner-nodes reasoner)
            subproblems)))
```

## 🧪 ステップ5: 実践例とテスト

### 5.1 コンパイラ最適化の実例

```lisp
;;;; optimization-examples.lisp
(in-package :cl-cc)

;; 実例：インライン展開の推論
(define-optimization-rule inline-expansion
  ;; 小さな関数はインライン展開
  ((should-inline ?func ?size true)
   (function-size ?func ?size)
   (< ?size 10))

  ;; 頻繁に呼ばれる関数もインライン展開
  ((should-inline ?func ?call-count true)
   (call-frequency ?func ?call-count)
   (> ?call-count 100))

  ;; 再帰関数は展開しない
  ((should-inline ?func ?any false)
   (recursive-function ?func)))

;; 使用例
(defun test-optimization ()
  "最適化ルールのテスト"
  (let ((engine (make-instance 'inference-engine))
        (kb (make-instance 'optimization-rule-base)))

    ;; ルールをロード
    (load-optimization-rules kb)

    ;; 事実を追加
    (assert-clause kb (make-clause '(function-size foo 5)))
    (assert-clause kb (make-clause '(call-frequency foo 150)))

    ;; 推論実行
    (let ((result (prove engine
                        '((should-inline foo ?decision))
                        (make-instance 'binding-environment))))
      (format t "Decision: ~A~%" result))))

;; 制約充足による最適化
(defun test-constraint-optimization ()
  "制約ベース最適化のテスト"
  (let ((optimizer (make-instance 'constraint-optimizer)))

    ;; 制約を追加
    (add-constraint optimizer '(register-count ?r))
    (add-constraint optimizer '(< ?r 16))
    (add-constraint optimizer '(memory-access ?m))
    (add-constraint optimizer '(minimize (+ ?r ?m)))

    ;; 最適解を探索
    (let ((solution (solve-constraints optimizer)))
      (format t "Optimal allocation: ~A~%" solution))))
```

### 5.2 プロパティベーステスト

```lisp
;;;; prolog-property-tests.lisp
(in-package :cl-cc)

(defclass prolog-property-test ()
  ((properties :initform nil
               :accessor test-properties))
  (:documentation "Prologベースのプロパティテスト"))

(defmethod define-property ((test prolog-property-test) name rule)
  "プロパティを定義"
  (push (cons name rule) (test-properties test)))

;; ユニフィケーションのプロパティ
(define-property unification-reflexivity
  '((unify ?x ?x ?env ?env)))

(define-property unification-symmetry
  '((unify ?x ?y ?env1 ?env2)
    (unify ?y ?x ?env1 ?env2)))

(define-property unification-transitivity
  '((unify ?x ?y ?env1 ?env2)
    (unify ?y ?z ?env2 ?env3)
    (unify ?x ?z ?env1 ?env3)))

;; 最適化の健全性
(define-property optimization-soundness
  '((optimize ?expr1 ?expr2)
    (semantically-equivalent ?expr1 ?expr2)))

(define-property optimization-improvement
  '((optimize ?expr1 ?expr2)
    (performance ?expr1 ?perf1)
    (performance ?expr2 ?perf2)
    (<= ?perf2 ?perf1)))
```

## 🎯 実践課題

### 課題1: タブロー法の実装
解析的タブロー法による定理証明器を実装。

### 課題2: 制約論理プログラミング
CLP(FD)風の有限領域制約ソルバーを追加。

### 課題3: 確率的推論
確率的Prologによるベイジアン推論を実装。

## 📚 次のステップ

- [新しい言語フロントエンドの追加](../how-to/add-language-frontend.md)
- [S式Prolog述語リファレンス](../reference/prolog-predicates.md)
- [S式Prologの統合理由](../explanation/prolog-integration-rationale.md)

## 💡 重要なポイント

1. **ユニフィケーション**: パターンマッチングの基礎
2. **バックトラッキング**: 探索空間の効率的な探索
3. **制約充足**: 宣言的な問題記述と解法
4. **メタ推論**: 推論プロセス自体の最適化

## 🔗 参考資料

- [The Art of Prolog](https://mitpress.mit.edu/9780262691635/)
- [Logic Programming with Prolog](https://www.springer.com/gp/book/9781447154877)
- [Warren's Abstract Machine](https://www.aitplanet.com/wam_tutorial.pdf)

---

*論理は美しい。S式でそれを表現することで、さらに美しくなる。*