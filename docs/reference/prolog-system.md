# S式Prolog システム仕様

## 🎯 概要

CL-CC統合のS式Prologシステムの完全な技術仕様。コンパイラの型推論、制約解決、最適化推論に使用される論理プログラミング基盤を詳述します。

## 📋 基本仕様

### システム構成

```lisp
;; Prologシステムのメインクラス
(defclass s-expr-prolog-system ()
  ((knowledge-base :initform (make-instance 'indexed-knowledge-base)
                   :reader prolog-kb
                   :documentation "事実とルールの知識ベース")

   (inference-engine :initform (make-instance 'sld-inference-engine)
                     :reader inference-engine
                     :documentation "推論エンジン")

   (constraint-solver :initform (make-instance 'constraint-solver)
                      :reader constraint-solver
                      :documentation "制約解決器")

   (query-cache :initform (make-hash-table :test 'equal)
                :reader query-cache
                :documentation "クエリ結果のキャッシュ")

   (statistics :initform (make-instance 'prolog-statistics)
               :reader prolog-stats
               :documentation "実行統計")))

;; システム初期化
(defmethod initialize-instance :after ((system s-expr-prolog-system) &key)
  (setup-built-in-predicates system)
  (configure-constraint-domains system)
  (enable-query-optimization system))
```

### 基本データ型

```lisp
;; 項（Term）の定義
(defclass prolog-term ()
  ((term-type :initarg :type :reader term-type)))

;; 変数
(defclass prolog-variable (prolog-term)
  ((name :initarg :name :reader variable-name)
   (binding :initform :unbound :accessor variable-binding))
  (:default-initargs :type :variable))

;; 定数
(defclass prolog-constant (prolog-term)
  ((value :initarg :value :reader constant-value))
  (:default-initargs :type :constant))

;; 複合項
(defclass prolog-compound (prolog-term)
  ((functor :initarg :functor :reader compound-functor)
   (arity :initarg :arity :reader compound-arity)
   (arguments :initarg :args :reader compound-arguments))
  (:default-initargs :type :compound))

;; リスト
(defclass prolog-list (prolog-term)
  ((elements :initarg :elements :reader list-elements)
   (tail :initarg :tail :initform nil :reader list-tail))
  (:default-initargs :type :list))
```

## 🏗️ 知識ベース構造

### インデックス化知識ベース

```lisp
(defclass indexed-knowledge-base ()
  ((facts :initform (make-hash-table :test 'equal)
          :accessor kb-facts
          :documentation "事実の格納")

   (rules :initform (make-hash-table :test 'equal)
          :accessor kb-rules
          :documentation "ルールの格納")

   (predicate-index :initform (make-hash-table :test 'eq)
                    :reader predicate-index
                    :documentation "述語名によるインデックス")

   (arity-index :initform (make-hash-table :test 'eql)
                :reader arity-index
                :documentation "アリティによるインデックス")

   (first-arg-index :initform (make-hash-table :test 'equal)
                    :reader first-arg-index
                    :documentation "第一引数によるインデックス")))

;; 事実の追加
(defmethod add-fact ((kb indexed-knowledge-base) fact)
  "事実の知識ベースへの追加"
  (let* ((predicate (term-functor fact))
         (arity (term-arity fact))
         (first-arg (first (term-arguments fact)))
         (fact-id (generate-fact-id fact)))

    ;; 事実の格納
    (setf (gethash fact-id (kb-facts kb)) fact)

    ;; インデックスの更新
    (push fact-id (gethash predicate (predicate-index kb)))
    (push fact-id (gethash arity (arity-index kb)))
    (when (constantp first-arg)
      (push fact-id (gethash first-arg (first-arg-index kb))))

    fact-id))

;; ルールの追加
(defmethod add-rule ((kb indexed-knowledge-base) rule)
  "ルールの知識ベースへの追加"
  (let* ((head (rule-head rule))
         (body (rule-body rule))
         (predicate (term-functor head))
         (rule-id (generate-rule-id rule)))

    ;; ルールの格納
    (setf (gethash rule-id (kb-rules kb)) rule)

    ;; インデックスの更新（ヘッド述語で）
    (push rule-id (gethash predicate (predicate-index kb)))

    rule-id))
```

### クエリ最適化

```lisp
;; クエリ最適化器
(defclass query-optimizer ()
  ((optimization-rules :initform nil :accessor optimization-rules)
   (cost-model :initform (make-instance 'prolog-cost-model)
               :reader cost-model)))

(defmethod optimize-query ((optimizer query-optimizer) query)
  "クエリの最適化"
  (-> query
      (reorder-goals)
      (apply-cut-optimization)
      (inline-simple-predicates)
      (eliminate-redundant-unifications)))

;; ゴールの並び替え
(defmethod reorder-goals ((optimizer query-optimizer) goals)
  "効率的な実行順序への並び替え"
  (sort goals #'< :key (lambda (goal)
                         (estimate-goal-cost (cost-model optimizer) goal))))

;; カット最適化
(defmethod apply-cut-optimization ((optimizer query-optimizer) query)
  "カットの最適な配置"
  (insert-cuts-at-deterministic-points query))
```

## 🔧 推論エンジン

### SLD解決

```lisp
;; SLD推論エンジン
(defclass sld-inference-engine ()
  ((search-strategy :initarg :strategy
                    :initform :depth-first
                    :reader search-strategy)

   (backtrack-stack :initform nil
                    :accessor backtrack-stack)

   (choice-points :initform nil
                  :accessor choice-points)

   (cut-level :initform 0
              :accessor cut-level)))

(defmethod solve-query ((engine sld-inference-engine) query kb)
  "SLD解決によるクエリ実行"
  (case (search-strategy engine)
    (:depth-first (dfs-solve engine query kb))
    (:breadth-first (bfs-solve engine query kb))
    (:iterative-deepening (id-solve engine query kb))
    (:best-first (best-first-solve engine query kb))))

;; 深さ優先探索
(defmethod dfs-solve ((engine sld-inference-engine) goals kb &optional bindings)
  "深さ優先でのゴール解決"
  (cond
    ((null goals)
     ;; すべてのゴールが解決済み
     (list bindings))

    ((cut-goal-p (first goals))
     ;; カットの処理
     (setf (cut-level engine) (length (choice-points engine)))
     (dfs-solve engine (rest goals) kb bindings))

    (t
     ;; 通常のゴール解決
     (let ((goal (first goals))
           (remaining-goals (rest goals))
           (solutions nil))

       (dolist (clause (find-matching-clauses goal kb))
         (multiple-value-bind (unified-head unified-body new-bindings)
             (unify-with-clause goal clause bindings)

           (when unified-head
             (let ((new-goals (append unified-body remaining-goals)))
               ;; チョイスポイントの記録
               (push (make-choice-point goal clause bindings)
                     (choice-points engine))

               ;; 再帰的解決
               (let ((sub-solutions (dfs-solve engine new-goals kb new-bindings)))
                 (setf solutions (append solutions sub-solutions)))

               ;; バックトラック
               (pop (choice-points engine))))))

       solutions))))
```

### 統一化アルゴリズム

```lisp
;; 統一化システム
(defclass unification-system ()
  ((occurs-check :initarg :occurs-check
                 :initform t
                 :reader occurs-check-p)

   (unification-cache :initform (make-hash-table :test 'equal)
                      :reader unification-cache)))

(defmethod unify-terms ((system unification-system) term1 term2 &optional bindings)
  "項の統一化"
  (let ((t1 (dereference term1 bindings))
        (t2 (dereference term2 bindings)))

    (cond
      ;; 同一項
      ((term-equal-p t1 t2) bindings)

      ;; 変数の統一化
      ((variable-p t1) (bind-variable t1 t2 bindings system))
      ((variable-p t2) (bind-variable t2 t1 bindings system))

      ;; 複合項の統一化
      ((and (compound-p t1) (compound-p t2))
       (unify-compounds t1 t2 bindings system))

      ;; リストの統一化
      ((and (list-p t1) (list-p t2))
       (unify-lists t1 t2 bindings system))

      ;; 統一化失敗
      (t nil))))

;; 変数束縛
(defmethod bind-variable ((var prolog-variable) term bindings system)
  "変数の束縛"
  (cond
    ;; occurs check
    ((and (occurs-check-p system) (occurs-in-p var term bindings))
     nil)

    ;; 正常な束縛
    (t (extend-bindings var term bindings))))

;; occurs check
(defmethod occurs-in-p ((var prolog-variable) term bindings)
  "変数がterm内に出現するかチェック"
  (let ((dereferenced (dereference term bindings)))
    (cond
      ((variable-p dereferenced) (eq var dereferenced))
      ((compound-p dereferenced)
       (some (lambda (arg) (occurs-in-p var arg bindings))
             (compound-arguments dereferenced)))
      ((list-p dereferenced)
       (or (some (lambda (elem) (occurs-in-p var elem bindings))
                 (list-elements dereferenced))
           (and (list-tail dereferenced)
                (occurs-in-p var (list-tail dereferenced) bindings))))
      (t nil))))
```

## 🎨 制約論理プログラミング

### 制約ドメイン

```lisp
;; 制約ドメインの基底クラス
(defclass constraint-domain ()
  ((domain-name :initarg :name :reader domain-name)
   (domain-type :initarg :type :reader domain-type)
   (variables :initform (make-hash-table) :accessor domain-variables)
   (constraints :initform nil :accessor domain-constraints)))

;; 有限ドメイン制約
(defclass finite-domain (constraint-domain)
  ((domain-size :initarg :size :reader domain-size)
   (value-set :initarg :values :reader domain-values))
  (:default-initargs
   :name :finite-domain
   :type :discrete))

;; 実数ドメイン制約
(defclass real-domain (constraint-domain)
  ((lower-bound :initarg :min :initform most-negative-single-float)
   (upper-bound :initarg :max :initform most-positive-single-float))
  (:default-initargs
   :name :real-domain
   :type :continuous))

;; 制約の定義
(defclass constraint ()
  ((constraint-type :initarg :type :reader constraint-type)
   (variables :initarg :vars :reader constraint-variables)
   (relation :initarg :relation :reader constraint-relation)
   (satisfaction-level :initform :unknown :accessor satisfaction-level)))

;; 等式制約
(defclass equality-constraint (constraint)
  ()
  (:default-initargs :type :equality :relation '=))

;; 不等式制約
(defclass inequality-constraint (constraint)
  ((operator :initarg :op :reader inequality-operator))
  (:default-initargs :type :inequality))
```

### 制約解決

```lisp
;; 制約解決器
(defclass constraint-solver ()
  ((propagation-algorithm :initarg :propagation
                          :initform :arc-consistency
                          :reader propagation-algorithm)

   (search-strategy :initarg :search
                    :initform :backtrack-search
                    :reader search-strategy)

   (heuristics :initarg :heuristics
               :initform (list :most-constrained-variable
                              :least-constraining-value)
               :reader solver-heuristics)))

(defmethod solve-constraints ((solver constraint-solver) constraints variables)
  "制約の解決"
  (let ((csp (make-instance 'constraint-satisfaction-problem
                           :variables variables
                           :constraints constraints)))

    ;; 制約伝播
    (propagate-constraints solver csp)

    ;; 探索による解決
    (search-solution solver csp)))

;; アーク整合性
(defmethod arc-consistency ((solver constraint-solver) csp)
  "アーク整合性の維持"
  (let ((queue (make-constraint-queue (csp-constraints csp))))
    (loop while (not (empty-p queue))
          do (let ((constraint (dequeue queue)))
               (when (revise-constraint constraint csp)
                 (enqueue-dependent-constraints constraint queue))))))

;; バックトラック探索
(defmethod backtrack-search ((solver constraint-solver) csp &optional assignment)
  "バックトラック探索"
  (cond
    ((complete-assignment-p assignment csp)
     assignment)

    (t
     (let ((var (select-unassigned-variable solver csp assignment)))
       (dolist (value (order-domain-values solver var csp assignment))
         (when (consistent-p var value assignment csp)
           (let ((new-assignment (assign var value assignment)))
             (let ((result (backtrack-search solver csp new-assignment)))
               (when result
                 (return result))))))))))
```

## 🚀 組み込み述語

### 基本述語

```lisp
;; 算術述語
(defrel arithmetic-predicates
  ;; 加算
  ((is ?result (+ ?x ?y)) :-
   (number ?x) (number ?y)
   (setq ?result (+ ?x ?y)))

  ;; 減算
  ((is ?result (- ?x ?y)) :-
   (number ?x) (number ?y)
   (setq ?result (- ?x ?y)))

  ;; 乗算
  ((is ?result (* ?x ?y)) :-
   (number ?x) (number ?y)
   (setq ?result (* ?x ?y)))

  ;; 除算
  ((is ?result (/ ?x ?y)) :-
   (number ?x) (number ?y) (not (= ?y 0))
   (setq ?result (/ ?x ?y)))

  ;; 比較
  ((< ?x ?y) :-
   (number ?x) (number ?y)
   (lisp-call < ?x ?y))

  ((> ?x ?y) :-
   (number ?x) (number ?y)
   (lisp-call > ?x ?y))

  ((= ?x ?y) :-
   (unify ?x ?y)))

;; 型チェック述語
(defrel type-predicates
  ;; 数値型
  ((number ?x) :- (lisp-call numberp ?x))
  ((integer ?x) :- (lisp-call integerp ?x))
  ((float ?x) :- (lisp-call floatp ?x))

  ;; シンボル型
  ((symbol ?x) :- (lisp-call symbolp ?x))
  ((atom ?x) :- (lisp-call atom ?x))

  ;; リスト型
  ((list ?x) :- (lisp-call listp ?x))
  ((cons ?x) :- (lisp-call consp ?x))

  ;; 変数型
  ((var ?x) :- (prolog-variable-p ?x))
  ((nonvar ?x) :- (not (prolog-variable-p ?x))))

;; リスト操作述語
(defrel list-predicates
  ;; リスト構成
  ((append [] ?list ?list) :-)
  ((append [?head | ?tail] ?list [?head | ?result]) :-
   (append ?tail ?list ?result))

  ;; リスト分解
  ((member ?elem [?elem | ?tail]) :-)
  ((member ?elem [?head | ?tail]) :-
   (member ?elem ?tail))

  ;; リスト長
  ((length [] 0) :-)
  ((length [?head | ?tail] ?len) :-
   (length ?tail ?tail-len)
   (is ?len (+ ?tail-len 1)))

  ;; リスト逆転
  ((reverse ?list ?reversed) :-
   (reverse-acc ?list [] ?reversed))

  ((reverse-acc [] ?acc ?acc) :-)
  ((reverse-acc [?head | ?tail] ?acc ?result) :-
   (reverse-acc ?tail [?head | ?acc] ?result)))
```

### 高階述語

```lisp
;; メタ述語
(defrel meta-predicates
  ;; call/1
  ((call ?goal) :-
   (prolog-call ?goal))

  ;; findall/3
  ((findall ?template ?goal ?list) :-
   (collect-solutions ?template ?goal ?list))

  ;; bagof/3
  ((bagof ?template ?goal ?list) :-
   (collect-grouped-solutions ?template ?goal ?list))

  ;; setof/3
  ((setof ?template ?goal ?sorted-list) :-
   (bagof ?template ?goal ?list)
   (sort ?list ?sorted-list))

  ;; forall/2
  ((forall ?condition ?action) :-
   (not (call ?condition) (not (call ?action))))

  ;; once/1
  ((once ?goal) :-
   (call ?goal) !))

;; 制御述語
(defrel control-predicates
  ;; カット
  ((!) :- (prolog-cut))

  ;; 失敗
  ((fail) :- (prolog-fail))

  ;; 真
  ((true) :-)

  ;; 条件分岐
  ((if-then-else ?condition ?then ?else) :-
   (call ?condition) ! (call ?then))
  ((if-then-else ?condition ?then ?else) :-
   (call ?else))

  ;; 否定
  ((not ?goal) :-
   (call ?goal) ! fail)
  ((not ?goal) :-)

  ;; 論理和
  ((; ?goal1 ?goal2) :-
   (call ?goal1))
  ((; ?goal1 ?goal2) :-
   (call ?goal2)))
```

## 📊 性能最適化

### インデックス戦略

```lisp
;; 高度なインデックス
(defclass advanced-indexing ()
  ((hash-index :initform (make-hash-table :test 'equal))
   (trie-index :initform (make-instance 'prolog-trie))
   (bitmap-index :initform (make-instance 'bitmap-index))))

;; トライインデックス
(defclass prolog-trie ()
  ((root :initform (make-instance 'trie-node))
   (compression :initform t :reader compression-enabled-p)))

(defmethod index-term ((trie prolog-trie) term clause-id)
  "項のトライインデックス化"
  (let ((path (term-to-path term)))
    (insert-path (trie-root trie) path clause-id)))

;; ビットマップインデックス
(defclass bitmap-index ()
  ((predicate-bitmaps :initform (make-hash-table))
   (arity-bitmaps :initform (make-hash-table))
   (type-bitmaps :initform (make-hash-table))))

(defmethod create-bitmap-index ((index bitmap-index) clauses)
  "ビットマップインデックスの構築"
  (loop for clause in clauses
        for i from 0
        do (update-bitmaps index clause i)))
```

### クエリコンパイル

```lisp
;; クエリコンパイラ
(defclass query-compiler ()
  ((compilation-cache :initform (make-hash-table :test 'equal))
   (optimization-level :initform 2 :accessor optimization-level)))

(defmethod compile-query ((compiler query-compiler) query)
  "クエリのネイティブコードコンパイル"
  (or (gethash query (compilation-cache compiler))
      (setf (gethash query (compilation-cache compiler))
            (-> query
                (analyze-query)
                (optimize-query-plan)
                (generate-native-code)))))

;; WAM（Warren Abstract Machine）エミュレータ
(defclass wam-emulator ()
  ((heap :initform (make-array 10000 :fill-pointer 0))
   (stack :initform (make-array 1000 :fill-pointer 0))
   (trail :initform (make-array 1000 :fill-pointer 0))
   (registers :initform (make-array 256 :initial-element nil))))

(defmethod wam-execute ((wam wam-emulator) instructions)
  "WAM命令の実行"
  (loop for instruction in instructions
        do (execute-wam-instruction wam instruction)))
```

## 🔧 デバッグ支援

### トレース機能

```lisp
;; トレースシステム
(defclass prolog-tracer ()
  ((trace-level :initform 0 :accessor trace-level)
   (trace-predicates :initform nil :accessor traced-predicates)
   (trace-output :initform *standard-output* :accessor trace-output)))

(defmethod trace-call ((tracer prolog-tracer) goal depth)
  "ゴール呼び出しのトレース"
  (when (should-trace-p tracer goal)
    (format (trace-output tracer) "~VTCall: ~A~%" depth goal)))

(defmethod trace-exit ((tracer prolog-tracer) goal result depth)
  "ゴール終了のトレース"
  (when (should-trace-p tracer goal)
    (format (trace-output tracer) "~VTExit: ~A -> ~A~%" depth goal result)))

(defmethod trace-redo ((tracer prolog-tracer) goal depth)
  "再実行のトレース"
  (when (should-trace-p tracer goal)
    (format (trace-output tracer) "~VTRedo: ~A~%" depth goal)))

(defmethod trace-fail ((tracer prolog-tracer) goal depth)
  "失敗のトレース"
  (when (should-trace-p tracer goal)
    (format (trace-output tracer) "~VTFail: ~A~%" depth goal)))
```

### プロファイリング

```lisp
;; プロファイラ
(defclass prolog-profiler ()
  ((call-counts :initform (make-hash-table :test 'equal))
   (execution-times :initform (make-hash-table :test 'equal))
   (memory-usage :initform (make-hash-table :test 'equal))
   (backtrack-counts :initform (make-hash-table :test 'equal))))

(defmethod profile-predicate ((profiler prolog-profiler) predicate)
  "述語のプロファイリング開始"
  (setf (gethash predicate (call-counts profiler)) 0)
  (setf (gethash predicate (execution-times profiler)) 0)
  (setf (gethash predicate (backtrack-counts profiler)) 0))

(defmethod record-call ((profiler prolog-profiler) predicate start-time)
  "呼び出し記録"
  (incf (gethash predicate (call-counts profiler) 0))
  (let ((execution-time (- (get-internal-real-time) start-time)))
    (incf (gethash predicate (execution-times profiler) 0) execution-time)))
```

## 📈 統計情報

### 実行統計

```lisp
;; 実行統計クラス
(defclass prolog-statistics ()
  ((total-queries :initform 0 :accessor total-queries)
   (successful-queries :initform 0 :accessor successful-queries)
   (total-unifications :initform 0 :accessor total-unifications)
   (successful-unifications :initform 0 :accessor successful-unifications)
   (backtrack-count :initform 0 :accessor backtrack-count)
   (memory-allocated :initform 0 :accessor memory-allocated)
   (gc-count :initform 0 :accessor gc-count)))

(defmethod collect-statistics ((stats prolog-statistics))
  "統計情報の収集"
  `((queries . ((total . ,(total-queries stats))
                (successful . ,(successful-queries stats))
                (success-rate . ,(/ (successful-queries stats)
                                   (max 1 (total-queries stats))))))
    (unifications . ((total . ,(total-unifications stats))
                     (successful . ,(successful-unifications stats))
                     (success-rate . ,(/ (successful-unifications stats)
                                        (max 1 (total-unifications stats))))))
    (backtracking . ,(backtrack-count stats))
    (memory . ((allocated . ,(memory-allocated stats))
               (gc-count . ,(gc-count stats))))))
```

## 🎯 API 仕様

### 基本API

```lisp
;; システム操作
(defgeneric prolog-assert (system fact-or-rule)
  (:documentation "事実またはルールの追加"))

(defgeneric prolog-retract (system pattern)
  (:documentation "パターンにマッチする事実/ルールの削除"))

(defgeneric prolog-query (system query)
  (:documentation "クエリの実行"))

(defgeneric prolog-query-all (system query)
  (:documentation "すべての解の取得"))

;; 知識ベース操作
(defgeneric kb-size (knowledge-base)
  (:documentation "知識ベースのサイズ"))

(defgeneric kb-predicates (knowledge-base)
  (:documentation "定義済み述語の一覧"))

(defgeneric kb-clear (knowledge-base)
  (:documentation "知識ベースのクリア"))

;; 統一化操作
(defgeneric unify (term1 term2 &optional bindings)
  (:documentation "項の統一化"))

(defgeneric substitute (bindings term)
  (:documentation "束縛の適用"))

(defgeneric rename-variables (term &optional mapping)
  (:documentation "変数名の変更"))
```

### 拡張API

```lisp
;; 制約プログラミング
(defgeneric add-constraint (system constraint)
  (:documentation "制約の追加"))

(defgeneric solve-constraints (system variables constraints)
  (:documentation "制約の解決"))

;; 高度な機能
(defgeneric compile-predicate (system predicate)
  (:documentation "述語のコンパイル"))

(defgeneric optimize-knowledge-base (system)
  (:documentation "知識ベースの最適化"))

(defgeneric export-knowledge-base (system format destination)
  (:documentation "知識ベースのエクスポート"))
```

## 🔧 設定オプション

### システム設定

```lisp
;; 設定パラメータ
(defparameter *prolog-config*
  '(;; 基本設定
    (:occurs-check . t)
    (:cut-semantics . :standard)
    (:arithmetic-mode . :iso)

    ;; 最適化設定
    (:query-optimization . t)
    (:indexing-strategy . :advanced)
    (:compilation-level . 2)

    ;; メモリ設定
    (:initial-heap-size . 1000000)
    (:max-heap-size . 100000000)
    (:gc-threshold . 0.8)

    ;; デバッグ設定
    (:trace-level . 0)
    (:debug-mode . nil)
    (:profiling . nil)

    ;; 制約設定
    (:constraint-domains . (:finite-domain :real-domain))
    (:constraint-propagation . :arc-consistency)
    (:constraint-search . :backtrack-search)))

(defun configure-prolog-system (config-alist)
  "Prologシステムの設定"
  (dolist (config config-alist)
    (set-prolog-option (car config) (cdr config))))
```

## 📚 使用例

### 基本的な使用

```lisp
;; システムの初期化
(defparameter *prolog* (make-instance 's-expr-prolog-system))

;; 事実の追加
(prolog-assert *prolog* '(parent tom bob))
(prolog-assert *prolog* '(parent bob pat))
(prolog-assert *prolog* '(parent bob liz))

;; ルールの追加
(prolog-assert *prolog* '((grandparent ?x ?z) :-
                          (parent ?x ?y)
                          (parent ?y ?z)))

;; クエリの実行
(prolog-query *prolog* '(grandparent tom ?who))
;; => ((?who . pat) (?who . liz))

;; すべての解の取得
(prolog-query-all *prolog* '(parent ?x ?y))
;; => (((?x . tom) (?y . bob))
;;     ((?x . bob) (?y . pat))
;;     ((?x . bob) (?y . liz)))
```

### コンパイラ統合例

```lisp
;; 型推論ルールの設定
(prolog-assert *prolog* '((type-of (const ?n) int) :- (integerp ?n)))
(prolog-assert *prolog* '((type-of (+ ?x ?y) int) :-
                          (type-of ?x int)
                          (type-of ?y int)))

;; 最適化ルールの設定
(prolog-assert *prolog* '((optimize (+ ?x 0) ?x) :-))
(prolog-assert *prolog* '((optimize (* ?x 1) ?x) :-))

;; コンパイラでの使用
(defun infer-expression-type (expr)
  (prolog-query *prolog* `(type-of ,expr ?type)))

(defun apply-optimization (expr)
  (or (prolog-query *prolog* `(optimize ,expr ?optimized))
      expr))
```

## 🎯 まとめ

このS式Prologシステムは以下の特徴を提供：

1. **完全なISO Prolog互換性**: 標準Prolog機能の完全実装
2. **高性能インデックス**: 複数のインデックス戦略による高速検索
3. **制約論理プログラミング**: 有限ドメインと実数ドメインの制約解決
4. **コンパイル最適化**: WAMベースの高速実行
5. **デバッグ支援**: 包括的なトレースとプロファイリング
6. **コンパイラ統合**: CL-CCとのシームレスな統合

これにより、CL-CCに強力な論理推論能力が統合され、世界最高峰のコンパイラシステムが実現されます。

---

*この仕様書は、CL-CC S式Prologシステムの完全な技術文書です。*