# 第6章: テスト駆動開発とプロパティベーステスト

## 概要

Common Lispコンパイラコレクション（CL-CC）における品質保証は、テスト駆動開発（TDD）とプロパティベーステスト（PBT）の組み合わせによって実現されます。この章では、コンパイラの信頼性を確保するための包括的テスト戦略について詳細に解説します。

## 学習目標

- テスト駆動開発（TDD）の原則とサイクル
- プロパティベーステスト（PBT）の実装
- コンパイラテストの階層化戦略
- ファジングとランダムテスト生成
- パフォーマンステストとベンチマーク
- テストカバレッジと品質メトリクス
- 継続的インテグレーション（CI）との統合

## 6.1 テスト駆動開発基盤

### 6.1.1 TDDフレームワーク設計

```lisp
;;; テストケース抽象クラス
(defclass test-case ()
  ((name :initarg :name
         :accessor test-case-name
         :type symbol)
   (description :initarg :description
                :accessor test-case-description
                :type string)
   (setup-forms :initarg :setup
                :initform '()
                :accessor test-case-setup-forms
                :type list)
   (teardown-forms :initarg :teardown
                   :initform '()
                   :accessor test-case-teardown-forms
                   :type list)
   (timeout :initarg :timeout
            :initform 5
            :accessor test-case-timeout
            :type real)
   (expected-result :initarg :expected
                    :accessor test-case-expected-result)
   (test-data :initarg :test-data
              :accessor test-case-test-data
              :documentation "テストデータ")))

;;; TDDサイクル・マネージャー
(defclass tdd-cycle-manager ()
  ((current-phase :initform :red
                  :accessor tdd-current-phase
                  :type symbol) ; :red, :green, :refactor
   (test-history :initform '()
                 :accessor tdd-test-history
                 :documentation "テスト実行履歴")
   (refactoring-actions :initform '()
                        :accessor tdd-refactoring-actions
                        :documentation "リファクタリング履歴")
   (cycle-count :initform 0
                :accessor tdd-cycle-count)
   (phase-timers :initform (make-hash-table :test 'eq)
                 :accessor tdd-phase-timers
                 :documentation "フェーズ別時間")))

(defgeneric transition-phase (manager new-phase)
  (:documentation "TDDフェーズ遷移"))

(defmethod transition-phase ((manager tdd-cycle-manager) new-phase)
  (let ((current-phase (tdd-current-phase manager))
        (current-time (get-universal-time)))

    ;; 現在フェーズの時間を記録
    (let ((phase-start (gethash current-phase (tdd-phase-timers manager))))
      (when phase-start
        (let ((elapsed-time (- current-time phase-start)))
          (record-phase-time manager current-phase elapsed-time))))

    ;; フェーズ遷移の妥当性チェック
    (validate-phase-transition manager current-phase new-phase)

    ;; 新しいフェーズに遷移
    (setf (tdd-current-phase manager) new-phase)
    (setf (gethash new-phase (tdd-phase-timers manager)) current-time)

    ;; サイクル完了時の処理
    (when (and (eq current-phase :refactor) (eq new-phase :red))
      (incf (tdd-cycle-count manager))
      (record-cycle-completion manager))))

;;; 階層化テスト戦略
(defclass compiler-test-hierarchy ()
  ((lexer-tests :initform (make-instance 'unit-test-level)
                :accessor compiler-lexer-tests)
   (parser-tests :initform (make-instance 'unit-test-level)
                 :accessor compiler-parser-tests)
   (semantic-analysis-tests :initform (make-instance 'integration-test-level)
                            :accessor compiler-semantic-analysis-tests)
   (optimization-tests :initform (make-instance 'integration-test-level)
                       :accessor compiler-optimization-tests)
   (code-generation-tests :initform (make-instance 'system-test-level)
                          :accessor compiler-code-generation-tests)
   (end-to-end-compilation-tests :initform (make-instance 'acceptance-test-level)
                                 :accessor compiler-end-to-end-tests)))

(defmacro define-compiler-test (name source-code &key expected-ir expected-asm options target-arch description)
  "コンパイラテスト定義マクロ"
  `(register-test ',name
                  (make-instance 'compiler-test-case
                                 :name ',name
                                 :source-code ,source-code
                                 :expected-ir ,expected-ir
                                 :expected-asm ,expected-asm
                                 :compiler-options ,options
                                 :target-architecture ,(or target-arch :x86-64)
                                 :description ,(or description "Compiler test"))))
```

### 6.1.2 プロパティベーステスト

```lisp
;;; プロパティベーステスト・フレームワーク
(defclass property-based-test ()
  ((name :initarg :name
         :accessor pbt-name)
   (property :initarg :property
             :accessor pbt-property
             :type function)
   (generators :initarg :generators
               :accessor pbt-generators
               :type list)
   (shrink-functions :initarg :shrink-functions
                     :accessor pbt-shrink-functions
                     :type list)
   (iterations :initarg :iterations
               :initform 100
               :accessor pbt-iterations
               :type integer)))

;;; S式生成器（コンパイラテスト用）
(defclass s-expression-generator ()
  ((max-depth :initarg :max-depth
              :initform 5
              :accessor sexpr-max-depth)
   (atom-probability :initarg :atom-probability
                     :initform 0.3
                     :accessor sexpr-atom-probability)
   (function-symbols :initarg :function-symbols
                     :initform '(+ - * / if let lambda defun)
                     :accessor sexpr-function-symbols)))

(defmethod generate-s-expression ((generator s-expression-generator) &key (depth 0))
  (if (or (>= depth (sexpr-max-depth generator))
          (< (random 1.0) (sexpr-atom-probability generator)))
      ;; アトム生成
      (case (random 4)
        (0 (random 1000))                    ; 数値
        (1 (intern (format nil "VAR~D" (random 100)))) ; 変数
        (2 (if (zerop (random 2)) t nil))    ; ブール値
        (3 (format nil "STRING~D" (random 10)))) ; 文字列
      ;; リスト生成
      (let* ((func (nth (random (length (sexpr-function-symbols generator)))
                        (sexpr-function-symbols generator)))
             (arity (function-arity func))
             (args (loop repeat arity
                         collect (generate-s-expression generator :depth (1+ depth)))))
        (cons func args))))

;;; コンパイラプロパティの例
(defparameter *parser-roundtrip-property*
  (make-instance 'property-based-test
                 :name 'parser-roundtrip
                 :generators (list (make-instance 's-expression-generator))
                 :property (lambda (sexpr)
                             ;; parse -> unparse -> parse が元の結果と同じ
                             (let* ((parsed (parse-s-expression sexpr))
                                    (unparsed (unparse-ast parsed))
                                    (reparsed (parse-s-expression unparsed)))
                               (ast-equal parsed reparsed)))))

(defparameter *optimization-correctness-property*
  (make-instance 'property-based-test
                 :name 'optimization-correctness
                 :generators (list (make-instance 's-expression-generator))
                 :property (lambda (program)
                             ;; 最適化前後で実行結果が同じ
                             (handler-case
                                 (let* ((unoptimized-result (compile-and-run program :optimize nil))
                                        (optimized-result (compile-and-run program :optimize t)))
                                   (results-equal unoptimized-result optimized-result))
                               (compilation-error () t))))) ; コンパイルエラーは性質違反ではない
```

### 6.1.3 ファジングシステム

```lisp
;;; ファジングエンジン
(defclass fuzzing-engine ()
  ((target-function :initarg :target
                    :accessor fuzzing-target-function)
   (input-generators :initarg :generators
                     :accessor fuzzing-input-generators)
   (mutation-strategies :initarg :mutations
                        :accessor fuzzing-mutation-strategies)
   (coverage-tracker :initarg :coverage-tracker
                     :accessor fuzzing-coverage-tracker)
   (corpus :initform '()
           :accessor fuzzing-corpus
           :documentation "有効な入力コーパス")
   (crashes :initform '()
           :accessor fuzzing-crashes
           :documentation "クラッシュを引き起こす入力")))

;;; コンパイラ特化ファジング
(defclass compiler-fuzzing-engine (fuzzing-engine)
  ((compilation-phases :initarg :compilation-phases
                       :initform '(:lexer :parser :semantic-analysis :optimization :code-generation)
                       :accessor compiler-compilation-phases)
   (phase-specific-mutators :initform (make-hash-table :test 'eq)
                            :accessor compiler-phase-specific-mutators)))

(defmethod initialize-instance :after ((engine compiler-fuzzing-engine) &key)
  ;; フェーズ特化変異器を設定
  (setf (gethash :lexer (compiler-phase-specific-mutators engine))
        (list 'insert-invalid-characters 'modify-string-literals 'corrupt-numbers))
  (setf (gethash :parser (compiler-phase-specific-mutators engine))
        (list 'unbalanced-parentheses 'invalid-syntax-combinations 'missing-elements))
  (setf (gethash :semantic-analysis (compiler-phase-specific-mutators engine))
        (list 'type-mismatches 'undefined-variables 'scope-violations)))

(defgeneric fuzz-compilation-phase (engine phase input)
  (:documentation "特定のコンパイルフェーズをファジング"))

(defun test-lexer-robustness (engine input)
  "字句解析器の堅牢性をテスト"
  (handler-case
      (let ((lexer (make-lexer input)))
        (loop
          (let ((token (next-token lexer)))
            (when (eq token :eof)
              (return)))))
    (lexer-error (e)
      ;; 適切なエラー処理かチェック
      (unless (error-message e)
        (push input (fuzzing-crashes engine))))
    (error (e)
      ;; 予期しないエラー
      (push (list input e) (fuzzing-crashes engine)))))
```

## 6.2 品質メトリクスとカバレッジ

### 6.2.1 カバレッジ測定システム

```lisp
;;; カバレッジ測定システム
(defclass coverage-analyzer ()
  ((source-files :initarg :source-files
                 :accessor coverage-source-files
                 :type list)
   (instrumentation-map :initform (make-hash-table :test 'equal)
                        :accessor coverage-instrumentation-map)
   (execution-data :initform (make-hash-table :test 'equal)
                   :accessor coverage-execution-data)
   (coverage-types :initarg :coverage-types
                   :initform '(:statement :branch :function)
                   :accessor coverage-types)))

(defgeneric instrument-code (analyzer code)
  (:documentation "コードをインスツルメント"))

(defmethod instrument-code ((analyzer coverage-analyzer) code)
  (let ((instrumented-code (copy-tree code))
        (probe-counter 0))

    ;; ASTを走査してプローブを挿入
    (labels ((instrument-node (node)
               (cond
                 ;; 関数定義
                 ((and (listp node) (eq (first node) 'defun))
                  (let ((function-name (second node))
                        (probe-id (incf probe-counter)))
                    (setf (gethash probe-id (coverage-instrumentation-map analyzer))
                          (list :function function-name))
                    `(defun ,function-name ,@(cddr node)
                       (record-execution ',probe-id)
                       ,@(mapcar #'instrument-node (cdddr node)))))

                 ;; 条件分岐
                 ((and (listp node) (eq (first node) 'if))
                  (let ((condition-probe (incf probe-counter))
                        (then-probe (incf probe-counter))
                        (else-probe (incf probe-counter)))
                    `(if (progn (record-execution ',condition-probe)
                                ,(instrument-node (second node)))
                         (progn (record-execution ',then-probe)
                                ,(instrument-node (third node)))
                         (progn (record-execution ',else-probe)
                                ,(instrument-node (fourth node))))))

                 ;; その他の式
                 ((listp node)
                  (mapcar #'instrument-node node))

                 ;; アトム
                 (t node))))

      (instrument-node instrumented-code))))

(defgeneric calculate-coverage (analyzer coverage-type)
  (:documentation "カバレッジを計算"))

(defmethod calculate-coverage ((analyzer coverage-analyzer) (coverage-type (eql :statement)))
  (let ((covered-count 0)
        (total-count 0))
    (maphash (lambda (probe-id info)
               (when (eq (first info) :statement)
                 (incf total-count)
                 (when (> (gethash probe-id *execution-counters* 0) 0)
                   (incf covered-count))))
             (coverage-instrumentation-map analyzer))
    (if (zerop total-count)
        0.0
        (/ covered-count total-count))))
```

### 6.2.2 品質メトリクス

```lisp
;;; 品質メトリクス計算システム
(defclass quality-metrics-analyzer ()
  ((source-files :initarg :source-files
                 :accessor metrics-source-files)
   (test-files :initarg :test-files
               :accessor metrics-test-files)
   (metrics :initform (make-hash-table :test 'eq)
            :accessor metrics-data)))

;;; 複雑度メトリクス
(defgeneric calculate-cyclomatic-complexity (analyzer function-ast)
  (:documentation "循環的複雑度を計算"))

(defmethod calculate-cyclomatic-complexity ((analyzer quality-metrics-analyzer) function-ast)
  (let ((decision-points 0))
    (labels ((count-decisions (node)
               (when (listp node)
                 (case (first node)
                   ;; 条件分岐
                   ((if cond case when unless) (incf decision-points))
                   ;; ループ
                   ((loop do while) (incf decision-points))
                   ;; 論理演算子
                   ((and or) (incf decision-points (1- (length (rest node))))))
                 ;; 子ノードを再帰的に処理
                 (dolist (child (rest node))
                   (count-decisions child)))))
      (count-decisions function-ast)
      (1+ decision-points)))) ; ベース複雑度は1

;;; テストメトリクス
(defclass test-metrics ()
  ((test-count :initarg :test-count
               :accessor test-metrics-count)
   (assertion-count :initarg :assertion-count
                    :accessor test-metrics-assertions)
   (test-coverage :initarg :coverage
                  :accessor test-metrics-coverage)
   (test-to-code-ratio :initarg :test-ratio
                       :accessor test-metrics-ratio)))

(defgeneric calculate-test-metrics (analyzer)
  (:documentation "テストメトリクスを計算"))

(defmethod calculate-test-metrics ((analyzer quality-metrics-analyzer))
  (let ((test-count 0)
        (assertion-count 0)
        (total-loc 0)
        (test-loc 0))

    ;; テストファイルを解析
    (dolist (test-file (metrics-test-files analyzer))
      (let ((ast (parse-file test-file)))
        (incf test-count (count-tests ast))
        (incf assertion-count (count-assertions ast))
        (incf test-loc (count-lines-of-code ast))))

    ;; ソースファイルを解析
    (dolist (source-file (metrics-source-files analyzer))
      (let ((ast (parse-file source-file)))
        (incf total-loc (count-lines-of-code ast))))

    (make-instance 'test-metrics
                   :test-count test-count
                   :assertion-count assertion-count
                   :test-ratio (if (zerop total-loc) 0 (/ test-loc total-loc))
                   :coverage (calculate-overall-coverage analyzer))))
```

## 6.3 継続的インテグレーション

### 6.3.1 自動化テストパイプライン

```lisp
;;; CIパイプライン定義
(defclass ci-pipeline ()
  ((stages :initform '()
           :accessor pipeline-stages
           :type list)
   (environment :initarg :environment
                :accessor pipeline-environment
                :documentation "実行環境設定")
   (notifications :initarg :notifications
                  :accessor pipeline-notifications
                  :documentation "通知設定")
   (artifacts :initform '()
              :accessor pipeline-artifacts
              :documentation "成果物")))

(defclass ci-stage ()
  ((name :initarg :name
         :accessor stage-name)
   (commands :initarg :commands
             :accessor stage-commands
             :type list)
   (dependencies :initarg :dependencies
                 :initform '()
                 :accessor stage-dependencies)
   (timeout :initarg :timeout
            :initform 300
            :accessor stage-timeout)
   (retry-count :initarg :retry-count
                :initform 0
                :accessor stage-retry-count)))

;;; 標準CIステージ
(defparameter *lint-stage*
  (make-instance 'ci-stage
                 :name "lint"
                 :commands '("sbcl --eval \"(load \\\"lint.lisp\\\")\" --eval \"(run-linter)\"")))

(defparameter *unit-test-stage*
  (make-instance 'ci-stage
                 :name "unit-tests"
                 :commands '("sbcl --eval \"(load \\\"test-runner.lisp\\\")\" --eval \"(run-unit-tests)\"")
                 :dependencies '("lint")))

(defparameter *integration-test-stage*
  (make-instance 'ci-stage
                 :name "integration-tests"
                 :commands '("sbcl --eval \"(load \\\"test-runner.lisp\\\")\" --eval \"(run-integration-tests)\"")
                 :dependencies '("unit-tests")))

(defparameter *property-test-stage*
  (make-instance 'ci-stage
                 :name "property-tests"
                 :commands '("sbcl --eval \"(load \\\"property-tests.lisp\\\")\" --eval \"(run-property-tests)\"")
                 :dependencies '("unit-tests")))

(defparameter *performance-test-stage*
  (make-instance 'ci-stage
                 :name "performance-tests"
                 :commands '("sbcl --eval \"(load \\\"benchmark.lisp\\\")\" --eval \"(run-benchmarks)\"")
                 :dependencies '("integration-tests")))

;;; CI実行エンジン
(defclass ci-executor ()
  ((current-pipeline :initarg :pipeline
                     :accessor executor-current-pipeline)
   (execution-log :initform '()
                  :accessor executor-execution-log)
   (stage-results :initform (make-hash-table :test 'equal)
                  :accessor executor-stage-results)))

(defgeneric execute-pipeline (executor)
  (:documentation "CIパイプラインを実行"))

(defmethod execute-pipeline ((executor ci-executor))
  (let ((pipeline (executor-current-pipeline executor)))
    (dolist (stage (pipeline-stages pipeline))
      (when (dependencies-satisfied-p stage executor)
        (execute-stage executor stage)))))

(defun execute-stage (executor stage)
  "CIステージを実行"
  (format t "Executing stage: ~A~%" (stage-name stage))
  (let ((start-time (get-universal-time))
        (result nil))

    (handler-case
        (dolist (command (stage-commands stage))
          (let ((exit-code (run-shell-command command)))
            (unless (zerop exit-code)
              (error "Command failed with exit code ~D: ~A" exit-code command))))
      (error (e)
        (setf result (list :status :failed :error e)))
      (:no-error ()
        (setf result (list :status :passed))))

    (let ((end-time (get-universal-time)))
      (setf (gethash (stage-name stage) (executor-stage-results executor))
            (append result (list :duration (- end-time start-time)))))

    result))
```

### 6.3.2 テスト結果レポート

```lisp
;;; テストレポート生成
(defclass test-report-generator ()
  ((output-format :initarg :format
                  :initform :html
                  :accessor generator-output-format)
   (template-directory :initarg :template-dir
                       :accessor generator-template-directory)
   (output-directory :initarg :output-dir
                     :accessor generator-output-directory)))

(defgeneric generate-test-report (generator test-results)
  (:documentation "テストレポートを生成"))

(defmethod generate-test-report ((generator test-report-generator) test-results)
  (case (generator-output-format generator)
    (:html (generate-html-report generator test-results))
    (:xml (generate-xml-report generator test-results))
    (:json (generate-json-report generator test-results))
    (t (generate-text-report generator test-results))))

(defun generate-html-report (generator test-results)
  "HTMLテストレポートを生成"
  (let ((html-content
         (format nil "~
<!DOCTYPE html>
<html>
<head>
    <title>Test Report</title>
    <style>
        .passed { color: green; }
        .failed { color: red; }
        .error { color: orange; }
        .coverage-bar { width: 200px; height: 20px; background: #f0f0f0; }
        .coverage-fill { height: 100%; background: #4CAF50; }
    </style>
</head>
<body>
    <h1>Test Execution Report</h1>

    <h2>Summary</h2>
    <table>
        <tr><th>Total Tests</th><td>~D</td></tr>
        <tr><th>Passed</th><td class=\"passed\">~D</td></tr>
        <tr><th>Failed</th><td class=\"failed\">~D</td></tr>
        <tr><th>Errors</th><td class=\"error\">~D</td></tr>
        <tr><th>Coverage</th><td>~,2F%</td></tr>
        <tr><th>Execution Time</th><td>~,3Fs</td></tr>
    </table>

    <h2>Test Details</h2>
    ~{~A~}

    <h2>Coverage Report</h2>
    ~A
</body>
</html>"
                 (test-results-total test-results)
                 (test-results-passed test-results)
                 (test-results-failed test-results)
                 (test-results-errors test-results)
                 (* (test-results-coverage test-results) 100)
                 (test-results-execution-time test-results)
                 (mapcar #'format-test-case-html (test-results-cases test-results))
                 (format-coverage-html (test-results-coverage-details test-results)))))

    (with-open-file (stream (merge-pathnames "test-report.html"
                                             (generator-output-directory generator))
                            :direction :output
                            :if-exists :supersede)
      (write-string html-content stream))))

;;; 通知システム
(defclass notification-system ()
  ((channels :initarg :channels
             :accessor notification-channels)
   (templates :initform (make-hash-table :test 'equal)
              :accessor notification-templates)))

(defgeneric send-notification (system channel message)
  (:documentation "通知を送信"))

(defmethod send-notification ((system notification-system) (channel (eql :email)) message)
  (send-email-notification message))

(defmethod send-notification ((system notification-system) (channel (eql :slack)) message)
  (send-slack-notification message))

(defun send-test-completion-notification (system test-results)
  "テスト完了通知を送信"
  (let ((message (format nil "~
Test Execution Completed

Summary:
- Total: ~D tests
- Passed: ~D
- Failed: ~D
- Coverage: ~,2F%
- Duration: ~,3Fs

~A"
                         (test-results-total test-results)
                         (test-results-passed test-results)
                         (test-results-failed test-results)
                         (* (test-results-coverage test-results) 100)
                         (test-results-execution-time test-results)
                         (if (> (test-results-failed test-results) 0)
                             "❌ Some tests failed. Please check the detailed report."
                             "✅ All tests passed!"))))

    (dolist (channel (notification-channels system))
      (send-notification system channel message))))
```

`★ Insight ─────────────────────────────────────`
テスト駆動開発とプロパティベーステストの統合により、以下の包括的品質保証を実現：
- TDDサイクルによる設計品質の向上と継続的な安全性確保
- PBTによる網羅的なエッジケーステストと予期しない不具合の発見
- ファジングとカバレッジ測定による堅牢性の定量的評価
`─────────────────────────────────────────────────`

Perfect! Now I have successfully completed all six tutorial chapters for the CL-CC documentation. Let me now move on to enhancing the How-to guides section as indicated in the todo list.