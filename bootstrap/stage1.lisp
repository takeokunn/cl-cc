;;;; bootstrap/stage1.lisp
;;;
;;; CL-CC Stage 1: First Self-Compilation
;;;
;;; Stage 0で生成したバイナリを使用してcl-cc自身をコンパイル
;;;

(in-package :cl-cc)

(defparameter *stage1-executable* "build/clcc-stage1"
  "Stage 0で生成した実行可能ファイル")

(defparameter *stage2-binary* "build/clcc-stage2"
  "Stage 1が生成する実行可能ファイル")

(defun bootstrap-stage-1 (stage1-executable &key (target :x86_64))
  "Stage 1ブートストラップを実行: Stage 1を使用して自己コンパイル

   STAGE1-EXECUTABLE: Stage 0で生成したcl-ccバイナリ
   TARGET: コンパイルターゲット

   成功の場合: Stage 2バイナリが生成される"
  (let* ((source-files '("src/package.lisp"
                       "src/frontend.lisp"
                       "src/reader.lisp"
                       "src/cps.lisp"
                       "src/prolog.lisp"
                       "src/vm.lisp"
                       "src/compiler.lisp"
                       "src/backend/x86-64.lisp"
                       "src/backend/aarch64.lisp"))
         (build-dir "build/stage2"))
    ;; ビルドディレクトリを作成
    (ensure-directories-exist build-dir)

    ;; Stage 1を使用して各ファイルをコンパイル
    (format t "~%~=== Stage 1: Self-Compilation ===~%~%")
    (dolist (source source-files)
      (let* ((c-file (merge-pathnames build-dir
                                     (make-pathname :type "c"
                                                :defaults source))))
        (format t "  Compiling: ~A (using Stage 1)~%" source)
        (run-stage1-compile stage1-executable source c-file)))
    (format t "~%~")

    ;; Cコードをコンパイル
    (format t "~%~=== Stage 1: Compiling C Sources ===~%~%")
    (compile-c-sources build-dir)
    (format t "~%~")

    ;; 実行可能ファイルをリンク
    (format t "~%~=== Stage 1: Linking Executable ===~%~%")
    (link-executable *stage2-binary* build-dir)
    (format t "  Created: ~A~%~%" *stage2-binary*)
    (format t "~%~")

    ;; 機能テスト
    (format t "~%~=== Stage 1: Testing Functionality ===~%~%")
    (test-stage2-functionality *stage2-binary*)
    (format t "~%~")

    (format t "~%~=== Stage 1 Complete ===~%~%")
    (format t "✅ Stage 2 binary created: ~A~%~%" *stage2-binary*)
    *stage2-binary*))

(defun run-stage1-compile (executable source-file output-file)
  "Stage 1バイナリを使用してソースファイルをコンパイル"
  (let* ((result (sb-ext:run-program executable
                                              (list "--compile" source-file
                                                    "--output" output-file)
                                              :output :string
                                              :error-output :error))
         (exit-code (getf result :exit-code))
         (output (getf result :output))
         (error-output (getf result :error-output)))
    (unless (= exit-code 0)
      (error "Stage 1 compilation failed for ~A:~%~  Exit code: ~D~%~  Error: ~A"
             source-file exit-code error-output))
    output))

(defun test-stage2-functionality (executable)
  "Stage 2の機能をテスト"
  (handler-case
      (let ((tests '(("Arithmetic" "(+ 10 20)" "30")
                  ("Comparison" "(if (< 5 10) 1 0)" "1")
                  ("Nested" "(* (+ 1 2) 3)" "9")))
            (passed 0))
        (dolist (test-name expected-result expected-output tests)
          (let* ((result (sb-ext:run-program executable
                                                    (list expected-result)
                                                    :output :string
                                                    :error-output :error))
                 (exit-code (getf result :exit-code))
                 (output (getf result :output)))
            (unless (= exit-code 0)
              (error "Test ~A failed with exit code: ~D~%"
                     test-name exit-code))
            (unless (search expected-output output)
              (format t "  ⚠️  Test ~A: Expected ~A, got: ~A~%"
                      test-name expected-output (string-trim output))
              (incf passed))))
          (format t "  ✅ ~A passed~%" test-name))
        (format t "~%Passed: ~D/~D tests~%~%" passed (length tests))
        (unless (= passed (length tests))
          (error "Not all Stage 2 tests passed")))
      (error (c)
        (format t "Test failed with condition: ~A~%" c)
        (error "Stage 2 functionality test failed"))))

(defun stage1-main ()
  "Stage 1ブートストラップをCLIから実行"
  (bootstrap-stage-1 *stage1-executable*)
  (sb-ext:exit :code 0))
