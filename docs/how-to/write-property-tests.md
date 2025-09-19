# Property-Based Testing 完全実装ガイド

## 🎯 目標

このガイドでは、CL-CCにおけるProperty-Based Testing（PBT）の完全な実装方法を学びます。手動でテストケースを書く代わりに、数学的性質を定義し、自動生成された無数のテストケースで検証する革新的なアプローチを習得します。

## 📋 前提条件

- Common Lispの基礎知識
- CL-CCのアーキテクチャ理解
- テスト理論の基本概念

## 🛠️ 実装手順

### ステップ1: 環境構築

```lisp
;;;; test-setup.lisp
;; 必要なライブラリをロード
(ql:quickload '(:check-it        ; Property-based testing
                :prove           ; Unit testing framework
                :alexandria      ; Utilities
                :trivial-types   ; Type utilities
                :cl-cc))         ; メインシステム

(defpackage :cl-cc-property-tests
  (:use :cl :check-it :prove :cl-cc)
  (:export #:run-all-property-tests
           #:define-compiler-property
           #:gen-program
           #:gen-expr))

(in-package :cl-cc-property-tests)
```

### ステップ2: カスタムジェネレータの構築

#### 基本型ジェネレータ
```lisp
;;;; generators.lisp
(defparameter *max-depth* 5
  "生成される式の最大深度")

(defparameter *identifier-chars* "abcdefghijklmnopqrstuvwxyz_"
  "識別子に使用可能な文字")

;; 基本的なリテラル生成
(defgenerator gen-integer (&optional (min -1000) (max 1000))
  "指定範囲の整数を生成"
  (generator (lambda (size)
             (declare (ignore size))
             (+ min (random (- max min))))))

(defgenerator gen-float (&optional (min -100.0) (max 100.0))
  "指定範囲の浮動小数点数を生成"
  (generator (lambda (size)
             (declare (ignore size))
             (+ min (* (random 1.0) (- max min))))))

(defgenerator gen-boolean ()
  "ブール値を生成"
  (gen-one-of '(t nil)))

(defgenerator gen-identifier ()
  "有効な識別子を生成"
  (generator (lambda (size)
             (let ((length (max 1 (min size 10))))
               (intern
                (map 'string
                     (lambda (_)
                       (declare (ignore _))
                       (char *identifier-chars*
                             (random (length *identifier-chars*))))
                     (make-array length :initial-element nil)))))))

;; リテラルノード生成
(defgenerator gen-literal ()
  "リテラルASTノードを生成"
  (gen-one-of (list
               (gen-map 'literal-node
                        (gen-integer))
               (gen-map 'literal-node
                        (gen-float))
               (gen-map 'literal-node
                        (gen-boolean)))))
```

#### 複合構造ジェネレータ
```lisp
;; 変数参照生成
(defgenerator gen-variable ()
  "変数参照ASTノードを生成"
  (gen-map 'variable-node
           (gen-identifier)))

;; 二項演算生成
(defgenerator gen-binary-op (&optional (depth 0))
  "二項演算ASTノードを生成"
  (when (< depth *max-depth*)
    (gen-map 'binary-op-node
             (gen-one-of '(+ - * / < > <= >= = /=))
             (gen-expr (1+ depth))
             (gen-expr (1+ depth)))))

;; 条件式生成
(defgenerator gen-if-expr (&optional (depth 0))
  "条件式ASTノードを生成"
  (when (< depth *max-depth*)
    (gen-map 'if-expr-node
             (gen-expr (1+ depth))
             (gen-expr (1+ depth))
             (gen-maybe (gen-expr (1+ depth))))))

;; 関数呼び出し生成
(defgenerator gen-function-call (&optional (depth 0))
  "関数呼び出しASTノードを生成"
  (when (< depth *max-depth*)
    (gen-map 'function-call-node
             (gen-identifier)
             (gen-list (gen-expr (1+ depth)) :min-length 0 :max-length 5))))

;; 式全体の生成
(defgenerator gen-expr (&optional (depth 0))
  "任意の式ASTノードを生成"
  (if (>= depth *max-depth*)
      (gen-one-of (list (gen-literal)
                        (gen-variable)))
      (gen-one-of (list (gen-literal)
                        (gen-variable)
                        (gen-binary-op depth)
                        (gen-if-expr depth)
                        (gen-function-call depth)))))

;; プログラム全体の生成
(defgenerator gen-program ()
  "完全なプログラムを生成"
  (gen-tuple 'program
             (gen-list (gen-function-definition))
             (gen-expr)))

(defgenerator gen-function-definition ()
  "関数定義を生成"
  (gen-tuple 'defun
             (gen-identifier)
             (gen-list (gen-identifier) :max-length 5)
             (gen-expr)))
```

### ステップ3: プロパティの定義

#### 基本的なプロパティ
```lisp
;;;; basic-properties.lisp

;; パーサーの可逆性
(define-property parser-roundtrip
  "パーサーの往復変換で情報が保持される"
  (for-all ((expr (gen-expr)))
    (let* ((s-expr (ast-to-s-expr expr))
           (reparsed (parse-s-expr s-expr)))
      (ast-structurally-equal expr reparsed))))

;; コンパイラの決定性
(define-property compilation-deterministic
  "同じ入力に対して同じ出力を生成する"
  (for-all ((program (gen-program)))
    (let ((result1 (compile-program program))
          (result2 (compile-program program)))
      (compilation-results-equal result1 result2))))

;; 型推論の健全性
(define-property type-inference-soundness
  "推論された型が実際の実行時型と一致する"
  (for-all ((expr (gen-well-typed-expr)))
    (let* ((inferred-type (infer-type expr))
           (runtime-type (eval-and-get-type expr)))
      (type-compatible inferred-type runtime-type))))
```

#### 最適化のプロパティ
```lisp
;; 最適化の意味論保持
(define-property optimization-preserves-semantics
  "最適化前後で実行結果が同じ"
  (for-all ((program (gen-program))
            (optimization-level (gen-integer 0 3)))
    (let* ((unoptimized (compile-program program :optimization 0))
           (optimized (compile-program program :optimization optimization-level))
           (unopt-result (execute-program unoptimized))
           (opt-result (execute-program optimized)))
      (results-equivalent unopt-result opt-result))))

;; 定数畳み込みの正確性
(define-property constant-folding-correctness
  "定数畳み込みが数学的に正確"
  (for-all ((op (gen-one-of '(+ - * /)))
            (a (gen-integer))
            (b (gen-integer)))
    (when (not (and (eq op '/) (zerop b))) ; ゼロ除算を避ける
      (let* ((expr `(,op ,a ,b))
             (folded (constant-fold expr))
             (expected (eval expr)))
        (= folded expected)))))

;; デッドコード除去の正確性
(define-property dead-code-elimination-correctness
  "デッドコード除去が結果に影響しない"
  (for-all ((program (gen-program-with-dead-code)))
    (let* ((original (compile-program program))
           (optimized (eliminate-dead-code original))
           (original-result (execute-program original))
           (optimized-result (execute-program optimized)))
      (and (results-equivalent original-result optimized-result)
           (<= (program-size optimized) (program-size original))))))
```

#### メタモルフィックプロパティ
```lisp
;; 変数名変更の不変性
(define-property alpha-equivalence
  "変数名を変更しても意味が変わらない"
  (for-all ((program (gen-program)))
    (let* ((renamed (alpha-rename program))
           (original-result (execute-program program))
           (renamed-result (execute-program renamed)))
      (results-equivalent original-result renamed-result))))

;; 最適化の可換性
(define-property optimization-commutativity
  "異なる順序での最適化が同じ結果を生む"
  (for-all ((program (gen-program)))
    (let* ((opt1-then-2 (-> program
                           constant-fold
                           eliminate-dead-code))
           (opt2-then-1 (-> program
                           eliminate-dead-code
                           constant-fold)))
      (programs-equivalent opt1-then-2 opt2-then-1))))

;; バックエンドの等価性
(define-property backend-equivalence
  "異なるバックエンドが同じ結果を生成"
  (for-all ((program (gen-program)))
    (let* ((x86-result (compile-and-run program :target :x86-64))
           (arm-result (compile-and-run program :target :arm64)))
      (results-equivalent x86-result arm-result))))
```

### ステップ4: 高度なテスト戦略

#### シュリンキング（最小化）
```lisp
;;;; shrinking.lisp

(defun shrink-ast (ast failing-property)
  "失敗するASTを最小の例に縮小"
  (let ((candidates (generate-shrink-candidates ast)))
    (loop for candidate in candidates
          when (not (funcall failing-property candidate))
          return (shrink-ast candidate failing-property)
          finally (return ast))))

(defun generate-shrink-candidates (ast)
  "ASTの縮小候補を生成"
  (append
   (shrink-by-simplification ast)
   (shrink-by-reduction ast)
   (shrink-by-substitution ast)))

(defun shrink-by-simplification (ast)
  "式を簡単な形に置き換えて縮小"
  (typecase ast
    (binary-op-node
     (list (binary-left ast)
           (binary-right ast)
           (make-literal 0)))
    (if-expr-node
     (list (if-then ast)
           (if-else ast)
           (make-literal t)))
    (function-call-node
     (function-args ast))
    (t '())))
```

#### カバレッジ誘導生成
```lisp
;;;; coverage-guided.lisp

(defparameter *coverage-database* (make-hash-table :test #'equal)
  "コードカバレッジデータベース")

(defun guided-generation (target-coverage)
  "指定されたカバレッジを達成するための生成"
  (loop with attempts = 0
        with current-coverage = 0
        while (and (< current-coverage target-coverage)
                   (< attempts 10000))
        do (let* ((program (generate (gen-program)))
                  (coverage (measure-coverage program)))
             (when (> coverage current-coverage)
               (setf current-coverage coverage)
               (record-interesting-program program coverage))
             (incf attempts))
        finally (return current-coverage)))

(defun measure-coverage (program)
  "プログラムのコードカバレッジを測定"
  (let ((instrumented (instrument-for-coverage program)))
    (handler-case
        (compile-and-run instrumented)
      (error () 0))
    (calculate-coverage-percentage)))
```

### ステップ5: 実行とレポート

#### テスト実行フレームワーク
```lisp
;;;; test-runner.lisp

(defun run-all-property-tests (&key (iterations 1000) (timeout 300))
  "全プロパティテストを実行"
  (let ((results '())
        (start-time (get-internal-real-time)))

    (dolist (property *all-properties*)
      (format t "~&Testing property: ~A~%" (property-name property))

      (let ((result (with-timeout (timeout)
                      (check-property property :max-examples iterations))))

        (push (list property result) results)

        (case (result-status result)
          (:success
           (format t "~&  ✓ PASSED (~D examples)~%"
                   (result-examples-count result)))
          (:failure
           (format t "~&  ✗ FAILED after ~D examples~%"
                   (result-examples-count result))
           (format t "~&    Counter-example: ~S~%"
                   (result-counter-example result))
           (format t "~&    Shrunk to: ~S~%"
                   (result-shrunk-example result)))
          (:error
           (format t "~&  ⚠ ERROR: ~A~%"
                   (result-error-message result))))))

    (let ((end-time (get-internal-real-time)))
      (format t "~&~%Total time: ~,2Fs~%"
              (/ (- end-time start-time) internal-time-units-per-second)))

    (generate-html-report results)
    results))

(defun generate-html-report (results)
  "HTMLレポートを生成"
  (with-open-file (out "property-test-report.html"
                       :direction :output
                       :if-exists :supersede)
    (format out "<!DOCTYPE html>~%<html><head>~%")
    (format out "<title>Property Test Report</title>~%")
    (format out "<style>~%")
    (format out ".passed { color: green; }~%")
    (format out ".failed { color: red; }~%")
    (format out ".error { color: orange; }~%")
    (format out "</style>~%</head><body>~%")

    (format out "<h1>Property Test Report</h1>~%")
    (format out "<table border='1'>~%")
    (format out "<tr><th>Property</th><th>Status</th><th>Examples</th><th>Details</th></tr>~%")

    (dolist (result-pair results)
      (destructuring-bind (property result) result-pair
        (let ((status (result-status result))
              (count (result-examples-count result)))
          (format out "<tr class='~A'>~%" status)
          (format out "<td>~A</td>~%" (property-name property))
          (format out "<td>~A</td>~%" status)
          (format out "<td>~A</td>~%" count)
          (format out "<td>")
          (when (eq status :failure)
            (format out "Counter-example: ~S<br/>~%"
                    (result-counter-example result))
            (format out "Shrunk: ~S"
                    (result-shrunk-example result)))
          (format out "</td></tr>~%"))))

    (format out "</table>~%</body></html>~%")))
```

### ステップ6: 統合とCI対応

#### 継続的インテグレーション
```lisp
;;;; ci-integration.lisp

(defun run-ci-property-tests ()
  "CI環境でのプロパティテスト実行"
  (let ((results (run-all-property-tests :iterations 10000 :timeout 600)))
    (if (every (lambda (result-pair)
                 (eq (result-status (second result-pair)) :success))
               results)
        (progn
          (format t "~&All property tests passed!~%")
          (sb-ext:exit :code 0))
        (progn
          (format t "~&Some property tests failed!~%")
          (sb-ext:exit :code 1)))))

(defun property-test-watchdog ()
  "プロパティテストの監視と自動実行"
  (loop
    (when (source-files-modified-p)
      (format t "~&Source files modified, running property tests...~%")
      (run-all-property-tests :iterations 100))
    (sleep 5)))
```

## 🎯 ベストプラクティス

### 1. プロパティ設計の原則
- **普遍的**: 全ての有効な入力に対して成り立つ
- **簡潔**: 理解しやすく、実装しやすい
- **強力**: 重要なバグを発見できる

### 2. ジェネレータ設計のコツ
- **多様性**: 様々な種類の入力を生成
- **現実性**: 実際に使われそうな入力を優先
- **極端値**: エッジケースも含める

### 3. パフォーマンス最適化
- **並列実行**: 複数のプロパティを並行してテスト
- **早期終了**: 失敗が見つかったら即座に停止
- **キャッシュ**: 結果をキャッシュして高速化

## 🔧 トラブルシューティング

### よくある問題と解決法

#### 問題1: テストが遅すぎる
```lisp
;; 解決法: 生成サイズを制限
(defparameter *max-examples* 100)
(defparameter *max-shrink-attempts* 50)
```

#### 問題2: 意味のない入力が生成される
```lisp
;; 解決法: より制約の強いジェネレータを使用
(defgenerator gen-valid-program ()
  (gen-such-that (gen-program)
                 #'program-is-well-formed))
```

#### 問題3: シュリンキングが進まない
```lisp
;; 解決法: カスタムシュリンク関数を定義
(defmethod shrink ((ast binary-op-node))
  (list (binary-left ast)
        (binary-right ast)))
```

## 📊 成果測定

### メトリクス
- **バグ発見率**: 見つかったバグの数
- **カバレッジ**: テストされたコードの割合
- **実行時間**: テスト完了までの時間
- **信頼度**: テストの信頼性レベル

### 改善指標
```lisp
(defun calculate-test-effectiveness ()
  "テスト効果の計算"
  (let* ((bugs-found (count-bugs-found))
         (total-examples (count-total-examples))
         (coverage (calculate-code-coverage)))
    (list :bugs-per-example (/ bugs-found total-examples)
          :coverage-percentage coverage
          :effectiveness-score (* (/ bugs-found 100) coverage))))
```

## 🔗 関連リソース

- [チュートリアル: TDD](../tutorials/06-test-driven-development.md)
- [リファレンス: テストAPI](../reference/testing-api.md)
- [説明: 品質保証理論](../explanation/quality-assurance.md)

---

*Property-Based Testingは、従来のテスト手法を超えた数学的厳密性を提供します。このガイドに従って実装することで、CL-CCの品質を飛躍的に向上させることができます。*