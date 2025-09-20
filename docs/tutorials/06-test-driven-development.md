# チュートリアル: Property-Based Testing とTDD - 数学的証明に基づく品質保証

## 🎯 このチュートリアルで学ぶこと

コンパイラの品質保証は単なるテストを超えた数学的厳密性が要求されます。このチュートリアルでは、CL-CCでProperty-Based Testing（PBT）とTest-Driven Development（TDD）を実践し、証明可能な品質を実現する方法を学びます。

### コア技術
1. **Property-Based Testing** - 柔軟なテストケース生成
2. **Generative Testing** - 自動的な反例発見
3. **Shrinking** - 最小の失敗例を発見
4. **Test-Driven Development** - 仕様駆動の開発プロセス
5. **Formal Verification** - 数学的証明による保証
6. **Metamorphic Testing** - コンパイラ特有のテスト手法

## 📋 前提条件

### 必須ライブラリ
```lisp
;; cl-cc-testing.asd
(defsystem :cl-cc-testing
  :depends-on (:cl-cc
               :check-it      ; Property-based testing
               :prove         ; Unit testing framework
               :alexandria    ; Utilities
               :trivial-types ; Type utilities
               :closer-mop)   ; Metaclass support
  :components ((:module "tests"
                :components
                ((:file "generators")
                 (:file "properties")
                 (:file "metamorphic")
                 (:file "unit-tests")
                 (:file "integration-tests")))))
```

### 環境設定
```lisp
(ql:quickload :cl-cc-testing)
(in-package :cl-cc-testing)
```

## 🔬 ステップ1: テストジェネレータの実装

まず、様々な入力を自動生成するジェネレータを作成します。

```lisp
;;;; generators.lisp
(in-package :cl-cc-testing)

;; 基本型のジェネレータ
(defgenerator gen-integer ()
  "整数の生成"
  (generator (lambda (size)
             (- (random (* 2 size)) size))))

(defgenerator gen-float ()
  "浮動小数点数の生成"
  (generator (lambda (size)
             (* (- (random 2.0) 1.0) size))))

(defgenerator gen-symbol ()
  "シンボルの生成"
  (let ((chars "abcdefghijklmnopqrstuvwxyz"))
    (generator (lambda (size)
               (intern
                (map 'string (lambda (_)
                              (declare (ignore _))
                              (char chars (random (length chars))))
                     (make-array (max 1 (random size))
                                :initial-element nil)))))))

;; 式のジェネレータ
(defgenerator gen-literal (type)
  "リテラル式の生成"
  (case type
    (:integer (gen-integer))
    (:float (gen-float))
    (:string (gen-string))
    (:boolean (gen-boolean))))

(defgenerator gen-binary-expr ()
  "二項演算式の生成"
  (gen-tuple (gen-one-of '(+ - * / < > <= >= = /=))
             (gen-expr)
             (gen-expr)))

(defgenerator gen-unary-expr ()
  "単項演算式の生成"
  (gen-tuple (gen-one-of '(not - abs))
             (gen-expr)))

(defgenerator gen-if-expr ()
  "条件式の生成"
  (gen-tuple 'if
             (gen-expr)
             (gen-expr)
             (gen-maybe (gen-expr))))

(defgenerator gen-let-expr ()
  "let式の生成"
  (gen-tuple 'let
             (gen-list (gen-tuple (gen-symbol) (gen-expr)))
             (gen-expr)))

(defgenerator gen-expr (&optional (max-depth 5))
  "任意の式の生成（再帰構造対応）"
  (if (zerop max-depth)
      (gen-one-of (list (gen-integer)
                        (gen-float)
                        (gen-symbol)))
      (gen-one-of (list (gen-integer)
                        (gen-float)
                        (gen-symbol)
                        (gen-binary-expr)
                        (gen-unary-expr)
                        (gen-if-expr)
                        (gen-let-expr)))))

;; プログラムジェネレータ
(defgenerator gen-function-def ()
  "関数定義の生成"
  (gen-tuple 'defun
             (gen-symbol)
             (gen-list (gen-symbol))
             (gen-expr)))

(defgenerator gen-program ()
  "プログラム全体の生成"
  (gen-list (gen-one-of (list (gen-function-def)
                              (gen-expr)))))

;; コンパイラ設定のジェネレータ
(defgenerator gen-optimization-level ()
  "最適化レベルの生成"
  (gen-one-of '(0 1 2 3)))

(defgenerator gen-target-platform ()
  "ターゲットプラットフォームの生成"
  (gen-one-of '(:x86-64 :arm64 :riscv :wasm)))

(defgenerator gen-compiler-options ()
  "コンパイラオプションの生成"
  (gen-hash-table
   :key-gen (gen-one-of '(:optimization-level :target :debug-info :warnings))
   :value-gen (gen-one-of (list (gen-optimization-level)
                                (gen-target-platform)
                                (gen-boolean)
                                (gen-boolean)))))
```

## 🧮 ステップ2: プロパティの定義

コンパイラが満たすべき数学的性質を定義します。

```lisp
;;;; properties.lisp
(in-package :cl-cc-testing)

;; パーサーのプロパティ
(defproperty parser-roundtrip
  "パーサーの可逆性: parse(print(ast)) = ast"
  (for-all ((expr (gen-expr)))
    (let* ((ast (parse-expr expr))
           (printed (ast-to-s-expr ast))
           (reparsed (parse-expr printed)))
      (ast-equal ast reparsed))))

(defproperty parser-preserves-structure
  "パーサーが構造を保持する"
  (for-all ((expr (gen-expr)))
    (let ((ast (parse-expr expr)))
      (implies (listp expr)
               (= (count-nodes ast)
                  (count-s-expr-nodes expr))))))

;; 型推論のプロパティ
(defproperty type-inference-soundness
  "型推論の健全性: 推論された型が実際の型と一致"
  (for-all ((expr (gen-well-typed-expr)))
    (let* ((ast (parse-expr expr))
           (inferred-type (infer-type ast))
           (actual-type (eval-type expr)))
      (type-equal inferred-type actual-type))))

(defproperty type-inference-completeness
  "型推論の完全性: 型付け可能な式には型を付けられる"
  (for-all ((expr (gen-typeable-expr)))
    (let ((ast (parse-expr expr)))
      (not (null (infer-type ast))))))

;; 最適化のプロパティ
(defproperty optimization-preserves-semantics
  "最適化が意味論を保持する"
  (for-all ((expr (gen-expr))
            (options (gen-compiler-options)))
    (let* ((original-ir (compile-to-ir expr))
           (optimized-ir (optimize-ir original-ir options))
           (original-result (evaluate-ir original-ir))
           (optimized-result (evaluate-ir optimized-ir)))
      (result-equal original-result optimized-result))))

(defproperty constant-folding-correctness
  "定数畳み込みの正確性"
  (for-all ((a (gen-integer))
            (b (gen-integer))
            (op (gen-one-of '(+ - * /))))
    (let* ((expr `(,op ,a ,b))
           (folded (constant-fold expr))
           (expected (eval expr)))
      (= folded expected))))

(defproperty dead-code-elimination-correctness
  "デッドコード除去の正確性"
  (for-all ((program (gen-program-with-dead-code)))
    (let* ((original (compile-program program))
           (optimized (eliminate-dead-code original))
           (original-result (execute-program original))
           (optimized-result (execute-program optimized)))
      (and (result-equal original-result optimized-result)
           (<= (code-size optimized) (code-size original))))))

;; コード生成のプロパティ
(defproperty codegen-correctness
  "コード生成の正確性"
  (for-all ((expr (gen-expr))
            (target (gen-target-platform)))
    (let* ((ir (compile-to-ir expr))
           (code (generate-code ir target))
           (ir-result (evaluate-ir ir))
           (code-result (execute-native-code code)))
      (result-equal ir-result code-result))))

(defproperty register-allocation-validity
  "レジスタ割り当ての妥当性"
  (for-all ((ir (gen-ir-program)))
    (let ((allocated (allocate-registers ir)))
      (and (no-register-conflicts allocated)
           (preserves-data-flow ir allocated)))))

;; メタモルフィックテスト
(defproperty compilation-idempotence
  "コンパイルの冪等性"
  (for-all ((expr (gen-expr)))
    (let* ((code1 (compile-expr expr))
           (decompiled (decompile code1))
           (code2 (compile-expr decompiled)))
      (code-equivalent code1 code2))))

(defproperty optimization-monotonicity
  "最適化の単調性: より高い最適化レベルは性能を改善する"
  (for-all ((expr (gen-expr)))
    (let* ((unoptimized (compile-expr expr :optimization 0))
           (optimized (compile-expr expr :optimization 3)))
      (implies (compilation-succeeded unoptimized optimized)
               (<= (execution-time optimized)
                   (execution-time unoptimized))))))
```

## 🔄 ステップ3: メタモルフィックテスト

コンパイラ特有のテスト手法を実装します。

```lisp
;;;; metamorphic.lisp
(in-package :cl-cc-testing)

;; 等価性を保持する変換
(defproperty alpha-conversion-preservation
  "α変換が意味論を保持する"
  (for-all ((program (gen-program)))
    (let* ((renamed (alpha-convert program))
           (original-result (compile-and-run program))
           (renamed-result (compile-and-run renamed)))
      (result-equal original-result renamed-result))))

(defproperty eta-conversion-preservation
  "η変換が意味論を保持する"
  (for-all ((function-expr (gen-function-expr)))
    (let* ((eta-expanded (eta-expand function-expr))
           (original-result (compile-and-run function-expr))
           (expanded-result (compile-and-run eta-expanded)))
      (function-equal original-result expanded-result))))

;; 最適化の関係性
(defproperty optimization-commutativity
  "最適化パスの可換性"
  (for-all ((program (gen-program)))
    (let* ((const-then-dead (-> program constant-fold eliminate-dead-code))
           (dead-then-const (-> program eliminate-dead-code constant-fold)))
      (ir-equivalent const-then-dead dead-then-const))))

(defproperty partial-evaluation-consistency
  "部分評価の一貫性"
  (for-all ((program (gen-program))
            (input-subset (gen-partial-input)))
    (let* ((specialized (partial-evaluate program input-subset))
           (full-input (complete-input input-subset))
           (original-result (run-program program full-input))
           (specialized-result (run-program specialized
                                          (remaining-input full-input input-subset))))
      (result-equal original-result specialized-result))))

;; コンパイラバックエンドの関係性
(defproperty backend-equivalence
  "異なるバックエンドの等価性"
  (for-all ((program (gen-program)))
    (let* ((x86-result (compile-and-run program :target :x86-64))
           (arm-result (compile-and-run program :target :arm64))
           (wasm-result (compile-and-run program :target :wasm)))
      (and (result-equal x86-result arm-result)
           (result-equal arm-result wasm-result)))))

;; 型システムの関係性
(defproperty type-annotation-preservation
  "型注釈が推論に影響しない"
  (for-all ((program (gen-program)))
    (let* ((annotated (add-type-annotations program))
           (original-types (infer-all-types program))
           (annotated-types (infer-all-types annotated)))
      (type-map-equal original-types annotated-types))))
```

## 🧪 ステップ4: TDDサイクルの実装

```lisp
;;;; unit-tests.lisp
(in-package :cl-cc-testing)

;; 赤: 失敗するテストを書く
(deftest test-lexer-basic-tokens
  "字句解析器の基本トークン認識"
  (testing "数値リテラル"
    (is (equal (tokenize "42")
               '((:integer . 42))))

    (is (equal (tokenize "3.14")
               '((:float . 3.14)))))

  (testing "識別子"
    (is (equal (tokenize "hello")
               '((:identifier . "hello"))))

    (is (equal (tokenize "var_123")
               '((:identifier . "var_123")))))

  (testing "演算子"
    (is (equal (tokenize "+ - * /")
               '((:operator . "+")
                 (:operator . "-")
                 (:operator . "*")
                 (:operator . "/"))))))

;; 緑: テストをパスさせる最小限のコード
(defun tokenize (input)
  "最小限の字句解析器"
  (let ((tokens '())
        (pos 0))
    (loop while (< pos (length input))
          do (multiple-value-bind (token new-pos)
                 (read-next-token input pos)
               (when token (push token tokens))
               (setf pos new-pos)))
    (nreverse tokens)))

(defun read-next-token (input pos)
  "次のトークンを読み取り"
  (skip-whitespace input pos)
  (cond
    ((>= pos (length input)) (values nil pos))
    ((digit-char-p (char input pos)) (read-number input pos))
    ((alpha-char-p (char input pos)) (read-identifier input pos))
    ((member (char input pos) '(#\+ #\- #\* #\/)) (read-operator input pos))
    (t (error "Unexpected character: ~A" (char input pos)))))

;; リファクタリング: コードを改善
(defclass lexer ()
  ((input :initarg :input :accessor lexer-input)
   (position :initform 0 :accessor lexer-position)
   (tokens :initform '() :accessor lexer-tokens)))

(defgeneric tokenize-next (lexer)
  (:documentation "次のトークンを処理"))

(defmethod tokenize-next ((lexer lexer))
  (with-accessors ((input lexer-input)
                   (pos lexer-position)
                   (tokens lexer-tokens)) lexer
    (when (< pos (length input))
      (let ((token (read-token-at lexer pos)))
        (when token
          (push token tokens)
          (incf pos (token-length token)))))))

;; プロパティベーステストとの統合
(deftest test-lexer-properties
  "字句解析器のプロパティテスト"
  (testing "トークン化の可逆性"
    (check-it (gen-string :elements "abcdefg0123456789 +-*/")
              (lambda (input)
                (let* ((tokens (tokenize input))
                       (reconstructed (tokens-to-string tokens)))
                  (string-equal input reconstructed)))))

  (testing "位置情報の正確性"
    (check-it (gen-string)
              (lambda (input)
                (let ((tokens (tokenize-with-positions input)))
                  (every (lambda (token)
                           (valid-position-p token input))
                         tokens))))))
```

## 📊 ステップ5: 継続的テスト環境

```lisp
;;;; test-runner.lisp
(in-package :cl-cc-testing)

(defun run-all-tests (&key (verbose t) (report-format :text))
  "全テストを実行"
  (let ((results '()))

    ;; ユニットテスト
    (format t "~&=== Unit Tests ===~%")
    (let ((unit-results (prove:run-test-system :cl-cc-testing)))
      (push (list :unit unit-results) results))

    ;; プロパティテスト
    (format t "~&=== Property Tests ===~%")
    (let ((property-results (run-property-tests)))
      (push (list :property property-results) results))

    ;; メタモルフィックテスト
    (format t "~&=== Metamorphic Tests ===~%")
    (let ((metamorphic-results (run-metamorphic-tests)))
      (push (list :metamorphic metamorphic-results) results))

    ;; 統合テスト
    (format t "~&=== Integration Tests ===~%")
    (let ((integration-results (run-integration-tests)))
      (push (list :integration integration-results) results))

    ;; レポート生成
    (generate-test-report results :format report-format)

    results))

(defun run-property-tests (&key (iterations 1000))
  "プロパティテストを実行"
  (let ((results '()))
    (dolist (property *all-properties*)
      (format t "~&  Testing property: ~A~%" (property-name property))
      (let ((result (check-property property :iterations iterations)))
        (push (list property result) results)
        (unless (property-passed-p result)
          (format t "~&    FAILED: ~A~%" (property-failure-reason result))
          (format t "~&    Counter-example: ~A~%" (property-counter-example result)))))
    results))

(defun run-metamorphic-tests ()
  "メタモルフィックテストを実行"
  (let ((results '()))
    (dolist (test *metamorphic-tests*)
      (format t "~&  Testing metamorphic property: ~A~%" (test-name test))
      (let ((result (run-metamorphic-test test)))
        (push (list test result) results)))
    results))

;; CIとの統合
(defun run-ci-tests ()
  "CI環境でのテスト実行"
  (handler-case
      (let ((results (run-all-tests :verbose nil :report-format :junit)))
        (if (all-tests-passed-p results)
            (progn
              (format t "~&All tests passed!~%")
              (sb-ext:exit :code 0))
            (progn
              (format t "~&Some tests failed!~%")
              (sb-ext:exit :code 1))))
    (error (e)
      (format t "~&Test execution failed: ~A~%" e)
      (sb-ext:exit :code 2))))

;; パフォーマンステスト
(defun run-performance-tests ()
  "パフォーマンステストを実行"
  (format t "~&=== Performance Tests ===~%")

  (testing "コンパイル時間の回帰テスト"
    (let ((baseline (load-performance-baseline))
          (current (measure-compilation-performance)))
      (is (< current (* baseline 1.1)) ; 10%以内の劣化を許容
          "Compilation performance regression detected")))

  (testing "実行時間の最適化効果"
    (dolist (optimization-level '(0 1 2 3))
      (let ((time (measure-execution-time :optimization optimization-level)))
        (record-performance-metric :execution-time optimization-level time))))

  (testing "メモリ使用量"
    (let ((memory-usage (measure-memory-usage)))
      (is (< memory-usage *max-allowed-memory*)
          "Memory usage exceeds limit"))))

;; テストデータの生成と管理
(defun generate-test-corpus ()
  "テスト用のプログラムコーパスを生成"
  (dotimes (i 10000)
    (let* ((program (generate (gen-program)))
           (filename (format nil "test-corpus/program-~5,'0D.lisp" i)))
      (with-open-file (out filename :direction :output :if-exists :supersede)
        (write program :stream out :pretty t)))))

(defun minimize-failing-test-case (property counter-example)
  "失敗したテストケースを最小化"
  (let ((minimizer (make-instance 'test-case-minimizer
                                  :property property
                                  :initial-case counter-example)))
    (minimize minimizer)))
```

## 🎯 実践的なテスト戦略

### テストピラミッド
```lisp
;; 1. 単体テスト (70%)
(deftest test-parser-individual-rules ...)
(deftest test-lexer-edge-cases ...)
(deftest test-ast-node-creation ...)

;; 2. 統合テスト (20%)
(deftest test-frontend-pipeline ...)
(deftest test-optimization-pipeline ...)
(deftest test-backend-integration ...)

;; 3. E2Eテスト (10%)
(deftest test-complete-compilation ...)
(deftest test-cross-platform-compatibility ...)
```

### カバレッジ分析
```lisp
(defun analyze-test-coverage ()
  "テストカバレッジを分析"
  (let ((coverage (measure-code-coverage #'run-all-tests)))
    (format t "~&Line coverage: ~,1F%~%" (coverage-line-percentage coverage))
    (format t "~&Branch coverage: ~,1F%~%" (coverage-branch-percentage coverage))
    (format t "~&Function coverage: ~,1F%~%" (coverage-function-percentage coverage))

    ;; カバレッジが低い部分を特定
    (let ((uncovered (find-uncovered-code coverage)))
      (format t "~&Uncovered code:~%")
      (dolist (location uncovered)
        (format t "~&  ~A:~D~%" (location-file location) (location-line location))))))
```

## 🔬 専門的なテスト技法

### ファズテスト
```lisp
(defun fuzz-test-compiler (&key (iterations 100000))
  "コンパイラのファズテスト"
  (dotimes (i iterations)
    (let ((random-input (generate-random-program)))
      (handler-case
          (compile-program random-input)
        (compiler-error (e)
          ;; 期待されるエラー
          (log-expected-error e random-input))
        (error (e)
          ;; 予期しないクラッシュ
          (log-crash e random-input)
          (save-crash-case random-input))))))

;; 差分テスト
(defun differential-testing (program)
  "他のコンパイラとの差分テスト"
  (let ((our-result (compile-with-cl-cc program))
        (sbcl-result (compile-with-sbcl program))
        (ccl-result (compile-with-ccl program)))
    (unless (and (result-equivalent our-result sbcl-result)
                 (result-equivalent our-result ccl-result))
      (report-differential-bug program our-result sbcl-result ccl-result))))
```

## 📈 継続的改善

### テストメトリクス
```lisp
(defun collect-test-metrics ()
  "テストメトリクスを収集"
  (list
   :test-count (count-all-tests)
   :test-execution-time (measure-test-execution-time)
   :code-coverage (measure-code-coverage)
   :mutation-score (calculate-mutation-score)
   :property-test-iterations (get-property-test-iterations)
   :false-positive-rate (calculate-false-positive-rate)
   :bug-detection-rate (calculate-bug-detection-rate)))

(defun generate-test-report (metrics)
  "テストレポートを生成"
  (with-open-file (out "test-report.html" :direction :output :if-exists :supersede)
    (format out "<!DOCTYPE html>~%")
    (format out "<html><head><title>CL-CC Test Report</title></head><body>~%")
    (format out "<h1>Test Execution Report</h1>~%")
    (format out "<h2>Metrics</h2>~%")
    (format out "<ul>~%")
    (loop for (key value) on metrics by #'cddr
          do (format out "<li>~A: ~A</li>~%" key value))
    (format out "</ul>~%")
    (format out "</body></html>~%")))
```

## 💡 ベストプラクティス

### 1. テストの構造化
- **AAA Pattern**: Arrange, Act, Assert
- **Given-When-Then**: BDD形式の明確性
- **Single Responsibility**: 一つのテストは一つの概念をテスト

### 2. プロパティ設計のガイドライン
- **不変性**: 変換前後で保持されるべき性質
- **関係性**: 異なる入力間の関係
- **境界値**: エッジケースでの動作

### 3. テストデータ管理
- **生成的**: ランダム生成で多様性を確保
- **決定的**: シードを使った再現可能性
- **最小化**: 失敗ケースの自動縮小

## 🔗 次のステップ

- [デバッグとプロファイリング](../how-to/debugging-profiling.md) - テスト失敗時の分析手法
- [形式的検証](../explanation/formal-verification.md) - 数学的証明による品質保証
- [パフォーマンステスト](../how-to/performance-testing.md) - 性能の定量的評価

## 📚 参考資料

- [QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs](https://dl.acm.org/doi/10.1145/351240.351266)
- [Property-Based Testing for Better Code](https://hypothesis.works/articles/what-is-property-based-testing/)
- [The Art of Software Testing](https://www.wiley.com/en-us/The+Art+of+Software+Testing%2C+3rd+Edition-p-9781118031964)

---

*このチュートリアルで、数学的厳密性に基づく品質保証の実践方法を身につけました。次のチュートリアルでは、これらのテスト技法を活用しながら実際のコンパイラ機能を実装していきます。*