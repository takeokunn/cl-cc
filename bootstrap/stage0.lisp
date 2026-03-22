;;;; bootstrap/stage0.lisp
;;;
;;; CL-CC Stage 0: Host Common Lisp Bootstrap
;;;
;;; ホストのCommon Lisp（SBCL/CCL）を使用してcl-ccを初期コンパイル
;;;

(in-package :cl-cc)

(defparameter *stage0-target* :x86_64
  "Stage 0で使用するターゲットアーキテクチャ")

(defun bootstrap-stage-0 (&key (target :x86_64))
  "Stage 0ブートストラップを実行: ホストCLを使用してCコードを生成

   TARGET: コンパイルターゲット（:x86_64 または :aarch64）

   成功の場合: Stage 1バイナリが生成される"
  (let* ((*stage0-target* target)
         (source-files '("src/package.lisp"
                       "src/frontend.lisp"
                       "src/reader.lisp"
                       "src/cps.lisp"
                       "src/prolog.lisp"
                       "src/vm.lisp"
                       "src/vm-primitives.lisp"
                       "src/vm-io.lisp"
                       "src/vm-conditions.lisp"
                       "src/vm-list.lisp"
                       "src/vm-strings.lisp"
                       "src/vm-hash.lisp"
                       "src/compiler.lisp"
                       "src/backend/x86-64.lisp"
                       "src/backend/aarch64.lisp"))
         (stage1-binary "build/clcc-stage1")
         (build-dir "build/stage1"))
    ;; ビルドディレクトリを作成
    (ensure-directories-exist build-dir)

    ;; 各ソースファイルをCコードにトランスパイル
    (format t "~%~=== Stage 0: Compiling CL-CC to C ===~%~%")
    (dolist (source source-files)
      (let* ((ast (read-source source))
             (compiled (cl-cc:compile-expression ast :target target))
             (c-code (getf compiled :c-source))
             (c-file (merge-pathnames build-dir
                                     (make-pathname :type "c"
                                                :defaults source))))
        (format t "  Compiling: ~A~%" source)
        (write-c-source c-file c-code)))
    (format t "~%~")

    ;; Cコードをコンパイル
    (format t "~%~=== Stage 0: Compiling C Sources ===~%~%")
    (compile-c-sources build-dir)
    (format t "~%~")

    ;; 実行可能ファイルをリンク
    (format t "~%~=== Stage 0: Linking Executable ===~%~%")
    (link-executable stage1-binary build-dir)
    (format t "  Created: ~A~%~%" stage1-binary)
    (format t "~%~")

    ;; 基本機能テスト
    (format t "~%~=== Stage 0: Testing Basic Functionality ===~%~%")
    (test-basic-functionality stage1-binary)
    (format t "~%~")

    (format t "~%~=== Stage 0 Complete ===~%~%")
    (format t "✅ Stage 1 binary created: ~A~%~%" stage1-binary)
    (format t "✅ Basic tests passed~%~%")
    stage1-binary))

(defun ensure-directories-exist (path)
  "ディレクトリが存在することを確認し、なければ作成"
  (unless (probe-file path)
    (sb-ext:ensure-directories-exist path)))

(defun read-source (source-file)
  "ソースファイルを読み込み、ASTに変換"
  (with-open-file (stream source-file :direction :input)
    (let* ((source (read stream))
           (ast (lower-sexp-to-ast source)))
      ast)))

(defun write-c-source (file content)
  "Cソースコードをファイルに書き込み"
  (with-open-file (stream file :direction :output
                            :if-exists :supersede)
    (format stream "~A~%" content)))

(defun compile-c-sources (build-dir)
  "Cソースファイルをコンパイル"
  (let ((c-files (directory build-dir)))
    (dolist (c-file c-files)
      (when (string= (pathname-type c-file) "c")
        (let* ((obj-file (make-pathname :type "o"
                                            :defaults c-file))
               (cmd (format nil "gcc -c -O2 -Wall -Werror -I src/runtime -o ~A ~A"
                           obj-file
                           c-file)))
          (format t "  Compiling C: ~A~%" c-file)
          (sb-ext:run-program "/bin/sh" (list "-c" cmd)))))))

(defun link-executable (executable build-dir)
  "実行可能ファイルをリンク"
  (let* ((o-files (remove-if-not
                     (lambda (f) (string= (pathname-type f) "o"))
                     (directory build-dir)))
         (cmd (format nil "gcc ~{~A~} -o ~A"
                       (format nil "~{~A~} "
                               (mapcar #'namestring o-files))
                       executable)))
    (format t "  Linking: ~A~%" executable)
    (sb-ext:run-program "/bin/sh" (list "-c" cmd))
    (sb-ext:run-program "chmod" (list "+x" (namestring executable)))))

(defun test-basic-functionality (executable)
  "基本的な機能をテスト"
  (handler-case
      (let ((result (sb-ext:run-program executable
                                              '("+ 1 2")
                                              :output :string
                                              :error-output :error)))
        (let* ((output (getf result :output))
               (exit-code (getf result :exit-code)))
          (unless (= exit-code 0)
            (error "Test failed with exit code: ~D~%" exit-code))
          (unless (search "3" output)
            (error "Test failed: expected output '3', got: ~A" output)))))
    (error (c)
      (format t "Test failed with condition: ~A~%" c)
      (error "Basic functionality test failed"))))

;; Stage 0をCLIから実行するための関数
(defun stage0-main (&key (target :x86_64))
  "Stage 0ブートストラップをCLIから実行"
  (let ((result (bootstrap-stage-0 :target target)))
    (sb-ext:exit :code 0)))
