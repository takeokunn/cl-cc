;;;; bootstrap/stage2.lisp
;;;
;;; CL-CC Stage 2: Fixed Point Verification
;;;
;;; Stage 1とStage 2がバイト単位で一致することを検証し、自己ホスティングを達成
;;;

(in-package :cl-cc)

(defparameter *stage2-binary* "build/clcc-stage2"
  "Stage 2バイナリ")

(defparameter *stage1-binary* "build/clcc-stage1"
  "Stage 1バイナリ")

(defun bootstrap-stage-2 (stage2-binary)
  "Stage 2ブートストラップを実行: Stage 1とStage 2のバイナリを比較

   STAGE2-BINARY: 検証するStage 2バイナリ

   成功の場合:
   1. Stage 1とStage 2がバイト単位で一致
   2. 全てのテストが通過

   返り値: t（成功）または error（失敗）"
  (let* ((source-files '("src/package.lisp"
                       "src/frontend.lisp"
                       "src/reader.lisp"
                       "src/cps.lisp"
                       "src/prolog.lisp"
                       "src/vm.lisp"
                       "src/compiler.lisp"
                       "src/backend/x86-64.lisp"
                       "src/backend/aarch64.lisp"))
         (build-dir "build/stage2")
         (stage2-outputs (mapcar (lambda (src)
                                          (merge-pathnames build-dir
                                                         (make-pathname :type "c"
                                                                            :defaults src)))
                                        source-files)))
    (format t "~%~=== Stage 2: Fixed Point Verification ===~%~%")

    ;; バイナリファイルが存在することを確認
    (unless (probe-file stage2-binary)
      (error "Stage 2 binary not found: ~A~%" stage2-binary))

    (unless (probe-file *stage1-binary*)
      (error "Stage 1 binary not found: ~A~%" *stage1-binary*))

    ;; バイナリ差分を確認
    (format t "~%~Checking binary identity...~%~%")
    (let ((identical-p (verify-binary-identity *stage1-binary* stage2-binary)))
      (if identical-p
          (progn
            (format t "✅ Stage 1 and Stage 2 are byte-identical!~%~%")
            (format t "✅ Fixed point achieved!~%~%"))
          (error "Binary verification failed: Stage 1 and Stage 2 differ~%~%")))
    (format t "~%~")

    ;; Cソースコードを比較
    (format t "~%~Verifying C source files...~%~%")
    (let* ((stage1-c-files (mapcar (lambda (src)
                                          (merge-pathnames "build/stage1"
                                                         (make-pathname :type "c"
                                                                            :defaults src)))
                                        source-files))
           (stage1-c-files-sorted (sort stage1-c-files #'string<)))
      (dolist (c-file stage1-c-files-sorted)
        (let* ((stage1-file c-file)
               (stage2-file (merge-pathnames build-dir c-file)))
          (unless (probe-file stage2-file)
            (warn "Stage 2 C file not found: ~A~%" stage2-file))
            (continue))
          (let ((identical-c-p (verify-c-file-identity stage1-file stage2-file)))
            (if identical-c-p
                (format t "  ✅ ~A: identical~%" (pathname-name c-file))
                (warn "  ⚠️  ~A: differs~%" (pathname-name c-file))))))

    ;; 全テストを実行
    (format t "~%~=== Running Full Test Suite ===~%~%")
    (run-test-suite stage2-binary)
    (format t "~%~")

    ;; 結果を報告
    (format t "~%~=== Stage 2 Complete ===~%~%")
    (format t "✅ Fixed point verified~%~%")
    (format t "✅ All tests passed~%~%")
    (format t "✅ Self-hosting achieved!~%~%")
    t))

(defun verify-binary-identity (file1 file2)
  "バイナリファイルがバイト単位で一致することを検証

   FILE1: Stage 1バイナリ
   FILE2: Stage 2バイナリ

   返り値: t（完全に一致）、nil（異なる）"
  (handler-case
      (let* ((stat1 (sb-unix:unix-stat file1))
             (stat2 (sb-unix:unix-stat file2))
             (size1 (sb-unix:stat-size stat1))
             (size2 (sb-unix:stat-size stat2)))
        (unless (= size1 size2)
          (warn "Binary sizes differ: ~A=~D bytes, ~A=~D bytes~%"
                (pathname-name file1) size1
                (pathname-name file2) size2)))
        ;; バイト単位で比較
        (with-open-file (stream1 file1 :element-type '(unsigned-byte 8))
          (with-open-file (stream2 file2 :element-type '(unsigned-byte 8))
            (loop
              (for byte1 = (read-byte stream1 nil)
               for byte2 = (read-byte stream2 nil)
               while byte1)
               when (/= byte1 byte2)
               do (return nil)
               finally (return t)))))
    (error (c)
      (error "Binary comparison failed: ~A~%" c))))

(defun verify-c-file-identity (file1 file2)
  "Cソースファイルが一致することを検証

   FILE1: Stage 1 Cファイル
   FILE2: Stage 2 Cファイル

   返り値: t（完全に一致）、nil（異なる）"
  (handler-case
      (with-open-file (stream1 file1 :direction :input)
        (with-open-file (stream2 file2 :direction :input)
          (loop
            (for line1 = (read-line stream1 nil)
             for line2 = (read-line stream2 nil)
             while line1
             when (string/= line1 line2)
             do (return nil)
             finally (return t))))
      (error (c)
        (error "C file comparison failed: ~A~%" c))))

(defun run-test-suite (executable)
  "全テストスイートを実行

   EXECUTABLE: テストを実行するバイナリ

   成功の場合: 全てのテストが通過"
  (let* ((test-commands '(("Arithmetic" "(+ 1 2)" "3")
                         ("Subtraction" "(- 10 3)" "7")
                         ("Multiplication" "(* 4 5)" "20")
                         ("Division" "(/ 20 4)" "5")
                         ("If" "(if (> 10 5) 1 0)" "1")
                         ("Let" "(let ((x 10)) (+ x 5))" "15")
                         ("Progn" "(progn 1 2 3)" "3")
                         ("Comparison" "(= 5 5)" "nil"))))
         (passed 0))
    (dolist (test-name expected-result expected-output test-commands)
      (let* ((expression (second test-commands))
             (result (sb-ext:run-program executable
                                            (list expression)
                                            :output :string
                                            :error-output :error))
             (exit-code (getf result :exit-code))
             (output (getf result :output)))
        (format t "  Testing: ~A... " test-name)
        (unless (= exit-code 0)
          (error "Test ~A failed with exit code: ~D~%" test-name exit-code))
        (string-trim output)
        (unless (search expected-output output)
          (error "Test ~A failed: expected '~A', got '~A'~%"
                 test-name expected-output output))
        (format t "  ✅ ~A passed~%~%" test-name)
        (incf passed)))
    (format t "~%Tests passed: ~D/~D~%~%" passed (length test-commands))
    (unless (= passed (length test-commands))
      (error "Not all tests passed"))))

(defun stage2-main ()
  "Stage 2ブートストラップをCLIから実行"
  (bootstrap-stage-2 *stage2-binary*)
  (sb-ext:exit :code 0))
