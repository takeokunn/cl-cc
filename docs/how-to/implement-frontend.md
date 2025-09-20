# How-to: 言語フロントエンドの実装

## 目的

新しいプログラミング言語のフロントエンドをCL-CCフレームワークに統合する方法を説明します。

## 前提条件

- CL-CCコアフレームワークの理解
- 対象言語の文法仕様
- CLOSの基本知識

## 実装手順

### ステップ1: 言語仕様の分析

#### 文法定義の整理

```lisp
;; 言語仕様をEBNF形式で定義
(defparameter *language-grammar*
  '((program     ::= (statement*))
    (statement   ::= (declaration | expression | control-flow))
    (declaration ::= (var-decl | func-decl | class-decl))
    (expression  ::= (primary | binary | unary | call))
    (primary     ::= (literal | identifier | grouped))))
```

#### トークン種別の定義

```lisp
(defclass token-type ()
  ((name :initarg :name :reader token-name)
   (pattern :initarg :pattern :reader token-pattern)
   (priority :initarg :priority :initform 0 :reader token-priority))
  (:documentation "トークン種別の定義"))

(defparameter *token-types*
  (list
   ;; キーワード
   (make-instance 'token-type :name :if :pattern "if" :priority 10)
   (make-instance 'token-type :name :else :pattern "else" :priority 10)
   (make-instance 'token-type :name :while :pattern "while" :priority 10)
   (make-instance 'token-type :name :function :pattern "function" :priority 10)
   (make-instance 'token-type :name :return :pattern "return" :priority 10)

   ;; リテラル
   (make-instance 'token-type :name :number :pattern "[0-9]+(\\.[0-9]+)?" :priority 5)
   (make-instance 'token-type :name :string :pattern "\"([^\\\"]|\\\\.)*\"" :priority 5)
   (make-instance 'token-type :name :identifier :pattern "[a-zA-Z_][a-zA-Z0-9_]*" :priority 1)

   ;; 演算子
   (make-instance 'token-type :name :plus :pattern "\\+" :priority 7)
   (make-instance 'token-type :name :minus :pattern "-" :priority 7)
   (make-instance 'token-type :name :star :pattern "\\*" :priority 7)
   (make-instance 'token-type :name :slash :pattern "/" :priority 7)
   (make-instance 'token-type :name :equal :pattern "==" :priority 8)
   (make-instance 'token-type :name :assign :pattern "=" :priority 6)))
```

### ステップ2: レキサーの実装

#### 効率的なトークナイザー

```lisp
(defclass advanced-lexer ()
  ((source :initarg :source :reader lexer-source)
   (position :initform 0 :accessor lexer-position)
   (line :initform 1 :accessor lexer-line)
   (column :initform 1 :accessor lexer-column)
   (tokens :initform nil :accessor lexer-tokens)
   (error-handler :initarg :error-handler
                  :initform #'default-error-handler
                  :accessor lexer-error-handler))
  (:documentation "専門的な字句解析器"))

(defmethod tokenize-advanced ((lexer advanced-lexer))
  "専門的なトークン化処理"
  (with-slots (source position line column tokens) lexer
    (loop while (< position (length source))
          do (skip-whitespace-and-comments lexer)
          when (< position (length source))
          do (push (next-token lexer) tokens))
    (nreverse tokens)))

(defmethod next-token ((lexer advanced-lexer))
  "次のトークンを読み取る"
  (let ((best-match nil)
        (best-length 0))
    ;; すべてのトークンタイプを試す
    (dolist (token-type *token-types*)
      (let ((match-length (try-match-token lexer token-type)))
        (when (and match-length
                   (or (> match-length best-length)
                       (and (= match-length best-length)
                            (> (token-priority token-type)
                               (token-priority best-match)))))
          (setf best-match token-type
                best-length match-length))))

    (if best-match
        (create-token lexer best-match best-length)
        (handle-unknown-character lexer))))

(defmethod skip-whitespace-and-comments ((lexer advanced-lexer))
  "空白とコメントをスキップ"
  (with-slots (source position line column) lexer
    (loop while (and (< position (length source))
                     (or (whitespace-p (char source position))
                         (comment-start-p source position)))
          do (cond
               ;; 改行
               ((char= (char source position) #\Newline)
                (incf line)
                (setf column 1)
                (incf position))

               ;; 単一行コメント
               ((single-line-comment-p source position)
                (skip-to-end-of-line lexer))

               ;; 複数行コメント
               ((multi-line-comment-p source position)
                (skip-multi-line-comment lexer))

               ;; 通常の空白
               (t
                (incf column)
                (incf position))))))
```

### ステップ3: ASTノードの設計

#### 言語固有のAST構造

```lisp
(defclass language-ast-node (ast-node)
  ((metadata :initform (make-hash-table)
             :accessor node-metadata
             :documentation "追加のメタデータ"))
  (:documentation "言語固有のAST基底クラス"))

;;; 宣言ノード
(defclass variable-declaration (language-ast-node)
  ((name :initarg :name :reader var-name)
   (type-annotation :initarg :type :initform nil :reader var-type)
   (initializer :initarg :init :initform nil :reader var-init)
   (mutable :initarg :mutable :initform t :reader var-mutable-p))
  (:documentation "変数宣言"))

(defclass function-declaration (language-ast-node)
  ((name :initarg :name :reader func-name)
   (parameters :initarg :params :reader func-params)
   (return-type :initarg :return-type :initform nil :reader func-return-type)
   (body :initarg :body :reader func-body)
   (attributes :initarg :attributes :initform nil :reader func-attributes))
  (:documentation "関数宣言"))

(defclass class-declaration (language-ast-node)
  ((name :initarg :name :reader class-name)
   (superclass :initarg :superclass :initform nil :reader class-superclass)
   (interfaces :initarg :interfaces :initform nil :reader class-interfaces)
   (members :initarg :members :reader class-members)
   (methods :initarg :methods :reader class-methods))
  (:documentation "クラス宣言"))

;;; 制御フロー
(defclass if-statement (language-ast-node)
  ((condition :initarg :condition :reader if-condition)
   (then-branch :initarg :then :reader if-then)
   (else-branch :initarg :else :initform nil :reader if-else))
  (:documentation "if文"))

(defclass while-statement (language-ast-node)
  ((condition :initarg :condition :reader while-condition)
   (body :initarg :body :reader while-body))
  (:documentation "while文"))

(defclass for-statement (language-ast-node)
  ((initializer :initarg :init :reader for-init)
   (condition :initarg :condition :reader for-condition)
   (increment :initarg :increment :reader for-increment)
   (body :initarg :body :reader for-body))
  (:documentation "for文"))
```

### ステップ4: パーサーの実装

#### Prattパーサー（演算子優先順位パーサー）

```lisp
(defclass pratt-parser ()
  ((tokens :initarg :tokens :accessor parser-tokens)
   (current :initform 0 :accessor parser-current)
   (precedence-table :initform (make-hash-table) :reader parser-precedences)
   (prefix-parsers :initform (make-hash-table) :reader parser-prefix)
   (infix-parsers :initform (make-hash-table) :reader parser-infix))
  (:documentation "Pratt式パーサー"))

(defmethod initialize-instance :after ((parser pratt-parser) &key)
  "パーサーの初期化"
  ;; 優先順位の設定
  (set-precedence parser :assign 1)
  (set-precedence parser :or 2)
  (set-precedence parser :and 3)
  (set-precedence parser :equal 4)
  (set-precedence parser :not-equal 4)
  (set-precedence parser :less 5)
  (set-precedence parser :greater 5)
  (set-precedence parser :plus 6)
  (set-precedence parser :minus 6)
  (set-precedence parser :star 7)
  (set-precedence parser :slash 7)
  (set-precedence parser :call 8)

  ;; prefix parserの登録
  (register-prefix parser :number #'parse-number-literal)
  (register-prefix parser :string #'parse-string-literal)
  (register-prefix parser :identifier #'parse-identifier)
  (register-prefix parser :minus #'parse-unary-minus)
  (register-prefix parser :lparen #'parse-grouped)

  ;; infix parserの登録
  (register-infix parser :plus #'parse-binary-op)
  (register-infix parser :minus #'parse-binary-op)
  (register-infix parser :star #'parse-binary-op)
  (register-infix parser :slash #'parse-binary-op)
  (register-infix parser :equal #'parse-binary-op)
  (register-infix parser :assign #'parse-assignment)
  (register-infix parser :lparen #'parse-call))

(defmethod parse-expression ((parser pratt-parser) &optional (min-precedence 0))
  "式をパース（Prattアルゴリズム）"
  (let* ((token (current-token parser))
         (prefix (get-prefix-parser parser (token-type token))))
    (unless prefix
      (error "Unexpected token: ~A" token))

    (advance parser)
    (let ((left (funcall prefix parser token)))
      (loop while (and (not (at-end-p parser))
                       (>= (get-precedence parser (current-token-type parser))
                           min-precedence))
            for token = (current-token parser)
            for infix = (get-infix-parser parser (token-type token))
            do (advance parser)
            do (setf left (funcall infix parser left token)))
      left)))
```

### ステップ5: エラー処理とリカバリー

#### パニックモードリカバリー

```lisp
(defclass error-recovery-parser (pratt-parser)
  ((errors :initform nil :accessor parser-errors)
   (sync-tokens :initform '(:semicolon :rbrace :eof)
                :accessor parser-sync-tokens))
  (:documentation "エラー回復機能付きパーサー"))

(defmethod parse-with-recovery ((parser error-recovery-parser))
  "エラー回復を伴うパース"
  (let ((statements nil))
    (loop until (at-end-p parser)
          do (handler-case
                 (push (parse-statement parser) statements)
               (parse-error (e)
                 (record-error parser e)
                 (synchronize parser))))
    (nreverse statements)))

(defmethod synchronize ((parser error-recovery-parser))
  "同期ポイントまでスキップ"
  (advance parser)
  (loop until (or (at-end-p parser)
                  (member (previous-token-type parser)
                          (parser-sync-tokens parser)))
        when (eq (current-token-type parser) :if) return nil
        when (eq (current-token-type parser) :for) return nil
        when (eq (current-token-type parser) :while) return nil
        when (eq (current-token-type parser) :return) return nil
        do (advance parser)))

(defmethod record-error ((parser error-recovery-parser) error)
  "エラーを記録"
  (push (make-error-info
         :message (error-message error)
         :token (current-token parser)
         :location (token-location (current-token parser)))
        (parser-errors parser)))
```

### ステップ6: セマンティック解析

#### 型チェックと名前解決

```lisp
(defclass semantic-analyzer ()
  ((symbol-table :initform (make-symbol-table)
                 :accessor analyzer-symbols)
   (type-checker :initform (make-type-checker)
                 :accessor analyzer-types)
   (errors :initform nil :accessor analyzer-errors)
   (warnings :initform nil :accessor analyzer-warnings))
  (:documentation "セマンティック解析器"))

(defmethod analyze-ast ((analyzer semantic-analyzer) ast)
  "ASTのセマンティック解析"
  (enter-scope (analyzer-symbols analyzer))
  (analyze-node analyzer ast)
  (exit-scope (analyzer-symbols analyzer))

  ;; 解析結果を返す
  (make-analysis-result
   :ast ast
   :errors (analyzer-errors analyzer)
   :warnings (analyzer-warnings analyzer)
   :symbol-table (analyzer-symbols analyzer)))

(defgeneric analyze-node (analyzer node)
  (:documentation "ノードのセマンティック解析"))

(defmethod analyze-node ((analyzer semantic-analyzer)
                         (node variable-declaration))
  "変数宣言の解析"
  ;; 重複チェック
  (when (lookup-local (analyzer-symbols analyzer) (var-name node))
    (add-error analyzer
               (format nil "Variable '~A' already declared"
                       (var-name node))))

  ;; 型推論または型チェック
  (let ((declared-type (var-type node))
        (inferred-type (when (var-init node)
                        (infer-type analyzer (var-init node)))))
    (when (and declared-type inferred-type)
      (unless (types-compatible-p declared-type inferred-type)
        (add-error analyzer
                   (format nil "Type mismatch: ~A vs ~A"
                           declared-type inferred-type))))

    ;; シンボルテーブルに登録
    (register-symbol (analyzer-symbols analyzer)
                     (var-name node)
                     (or declared-type inferred-type 't))))

(defmethod analyze-node ((analyzer semantic-analyzer)
                         (node function-declaration))
  "関数宣言の解析"
  ;; 関数をシンボルテーブルに登録
  (register-function (analyzer-symbols analyzer)
                     (func-name node)
                     (mapcar #'param-type (func-params node))
                     (func-return-type node))

  ;; 新しいスコープで本体を解析
  (enter-scope (analyzer-symbols analyzer))

  ;; パラメータを登録
  (dolist (param (func-params node))
    (register-symbol (analyzer-symbols analyzer)
                     (param-name param)
                     (param-type param)))

  ;; 本体を解析
  (analyze-node analyzer (func-body node))

  ;; リターン型の検証
  (validate-return-paths analyzer node)

  (exit-scope (analyzer-symbols analyzer)))
```

### ステップ7: 最適化とコード生成への準備

#### AST変換パイプライン

```lisp
(defclass transformation-pipeline ()
  ((passes :initform nil :accessor pipeline-passes))
  (:documentation "AST変換パイプライン"))

(defmethod add-pass ((pipeline transformation-pipeline) pass)
  "変換パスを追加"
  (push pass (pipeline-passes pipeline)))

(defmethod run-pipeline ((pipeline transformation-pipeline) ast)
  "パイプラインを実行"
  (reduce (lambda (current-ast pass)
            (funcall pass current-ast))
          (reverse (pipeline-passes pipeline))
          :initial-value ast))

;;; 標準的な変換パス
(defun constant-folding-pass (ast)
  "定数畳み込み"
  (transform-ast ast
    ((binary-op :operator '+
                :left (literal :value ?a)
                :right (literal :value ?b))
     => (literal :value (+ ?a ?b)))

    ((binary-op :operator '*
                :left (literal :value ?a)
                :right (literal :value ?b))
     => (literal :value (* ?a ?b)))))

(defun dead-code-elimination-pass (ast)
  "デッドコード除去"
  (transform-ast ast
    ((if-statement :condition (literal :value nil)
                   :then ?then
                   :else ?else)
     => ?else)

    ((if-statement :condition (literal :value ?v)
                   :then ?then
                   :else ?else)
     => ?then)))

(defun inline-expansion-pass (ast)
  "インライン展開"
  (let ((inline-candidates (find-inline-functions ast)))
    (transform-ast ast
      ((call :function ?f :args ?args)
       => (if (member ?f inline-candidates)
              (inline-function ?f ?args)
              (call :function ?f :args ?args))))))
```

## トラブルシューティング

### 問題: レキサーのパフォーマンス

**解決策**: 正規表現のプリコンパイル

```lisp
(defmethod compile-patterns ((lexer advanced-lexer))
  "パターンをプリコンパイル"
  (loop for token-type in *token-types*
        do (setf (token-pattern token-type)
                 (create-compiled-regex (token-pattern token-type)))))
```

### 問題: 左再帰の処理

**解決策**: 左再帰の除去または特別な処理

```lisp
(defmethod parse-left-recursive ((parser pratt-parser) rule)
  "左再帰の処理"
  ;; Prattパーサーは自然に左再帰を扱える
  (parse-expression parser (get-precedence parser rule)))
```

### 問題: エラーメッセージの改善

**解決策**: 詳細なエラー情報の収集

```lisp
(defclass detailed-error ()
  ((message :initarg :message)
   (location :initarg :location)
   (source-line :initarg :source-line)
   (suggestions :initarg :suggestions :initform nil))
  (:documentation "詳細なエラー情報"))

(defmethod format-error ((error detailed-error))
  "エラーを整形して表示"
  (format nil "Error at ~A:~%  ~A~%  ~A~%~{  Suggestion: ~A~%~}"
          (error-location error)
          (error-source-line error)
          (error-message error)
          (error-suggestions error)))
```

## 関連リソース

- [→ Tutorial: 最初のコンパイラを作る](../tutorials/01-first-compiler.md)
- [📖 Reference: Frontend API](../reference/frontend-api.md)
- [💡 Explanation: パーサー理論](../explanation/parser-theory.md)
- [⚙ How-to: 最適化パスの実装](implement-optimization-pass.md)