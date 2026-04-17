;;;; tests/unit/cli/main-dump-tests.lisp — dump/compile-option helper tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest cli-string-suffix-p-basic-cases
  (assert-true (cl-cc/cli::%string-suffix-p ".php" "hello.php"))
  (assert-true (cl-cc/cli::%string-suffix-p "out" "a.out"))
  (assert-false (cl-cc/cli::%string-suffix-p ".lisp" "hello.php"))
  (assert-false (cl-cc/cli::%string-suffix-p "longer" "short")))

(deftest cli-arch-keyword-parses-supported-architectures
  (assert-eq :x86-64 (cl-cc/cli::%arch-keyword "x86-64"))
  (assert-eq :x86-64 (cl-cc/cli::%arch-keyword "x86_64"))
  (assert-eq :arm64  (cl-cc/cli::%arch-keyword "arm64"))
  (assert-eq :arm64  (cl-cc/cli::%arch-keyword "aarch64")))

(deftest cli-arch-keyword-invalid-exits-2
  (let ((stderr (make-string-output-stream)))
    (with-fake-quit
      (handler-case
          (let ((*error-output* stderr))
            (cl-cc/cli::%arch-keyword "mips")
            (assert-false t))
        (fake-quit (q)
          (assert-= 2 (fake-quit-code q)))))
    (assert-true (search "Unknown architecture" (get-output-stream-string stderr)))))

(deftest cli-compile-target-keyword-parses-supported-architectures
  (assert-eq :x86_64 (cl-cc/cli::%compile-target-keyword "x86-64"))
  (assert-eq :x86_64 (cl-cc/cli::%compile-target-keyword "x86_64"))
  (assert-eq :aarch64 (cl-cc/cli::%compile-target-keyword "arm64"))
  (assert-eq :aarch64 (cl-cc/cli::%compile-target-keyword "aarch64")))

(deftest cli-compile-target-keyword-invalid-signals-error
  (handler-case
      (progn
        (cl-cc/cli::%compile-target-keyword "weird")
        (assert-false t))
    (error (e)
      (assert-true (search "Unknown architecture for compilation"
                           (princ-to-string e))))))

(deftest cli-parse-opt-remarks-mode-cases
  (assert-null (cl-cc/cli::%parse-opt-remarks-mode nil))
  (assert-null (cl-cc/cli::%parse-opt-remarks-mode ""))
  (assert-eq :all (cl-cc/cli::%parse-opt-remarks-mode "all"))
  (assert-eq :changed (cl-cc/cli::%parse-opt-remarks-mode "changed"))
  (assert-eq :missed (cl-cc/cli::%parse-opt-remarks-mode "missed")))

(deftest cli-parse-opt-remarks-mode-invalid-exits-2
  (let ((stderr (make-string-output-stream)))
    (with-fake-quit
      (handler-case
          (let ((*error-output* stderr))
            (cl-cc/cli::%parse-opt-remarks-mode "bogus")
            (assert-false t))
        (fake-quit (q)
          (assert-= 2 (fake-quit-code q)))))
    (assert-true (search "Unknown opt-remarks mode" (get-output-stream-string stderr)))))

(deftest cli-parse-compile-opts-reads-shared-flags
  (let* ((parsed (make-cli-parsed
                  :command "compile"
                  :flags '(("--pass-pipeline" . t)
                           ("--time-passes" . t)
                           ("--trace-json" . "trace.json")
                           ("--flamegraph" . "fg.svg")
                           ("--stats" . t)
                           ("--trace-emit" . t)
                           ("--opt-remarks" . "changed"))))
         (opts (cl-cc/cli::%parse-compile-opts parsed)))
    (assert-true (cl-cc/cli::compile-opts-pass-pipeline opts))
    (assert-true (cl-cc/cli::compile-opts-print-pass-timings opts))
    (assert-string= "trace.json" (cl-cc/cli::compile-opts-trace-json-path opts))
    (assert-string= "fg.svg" (cl-cc/cli::compile-opts-flamegraph-path opts))
    (assert-true (cl-cc/cli::compile-opts-print-pass-stats opts))
    (assert-true (cl-cc/cli::compile-opts-trace-emit opts))
    (assert-eq :changed (cl-cc/cli::compile-opts-opt-remarks-mode opts))))

(deftest cli-compile-opts-kwargs-expands-struct
  (let* ((opts (cl-cc/cli::make-compile-opts
                :pass-pipeline t
                :print-pass-timings t
                :trace-json-path "trace.json"
                :print-pass-stats t
                :trace-emit t
                :opt-remarks-mode :missed))
         (kwargs (cl-cc/cli::%compile-opts-kwargs opts :trace-stream)))
    (assert-equal '(:trace-json-stream :trace-stream
                    :print-pass-stats t
                    :pass-pipeline t
                    :print-pass-timings t
                    :print-opt-remarks t
                    :opt-remarks-mode :missed)
                  kwargs)))

(deftest cli-dump-ir-phase-invalid-signals-error
  (handler-case
      (progn
        (cl-cc/cli::%dump-ir-phase :bogus nil *standard-output* nil)
        (assert-false t))
    (error (e)
      (assert-true (search "Unknown IR phase" (princ-to-string e))))))
