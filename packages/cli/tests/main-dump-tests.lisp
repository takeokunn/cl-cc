;;;; tests/unit/cli/main-dump-tests.lisp — dump/compile-option helper tests

(in-package :cl-cc/test)

(in-suite cl-cc-cli-serial-suite)

(deftest-each cli-string-suffix-p-basic-cases
  "string-suffix-p returns t when the suffix matches, nil otherwise."
  :cases (("php-match"     t   ".php"   "hello.php")
          ("out-match"     t   "out"    "a.out")
          ("lisp-mismatch" nil ".lisp"  "hello.php")
          ("too-long"      nil "longer" "short"))
  (expected suffix str)
  (assert-eq expected (if (cl-cc/cli::%string-suffix-p suffix str) t nil)))

(deftest-each cli-arch-keyword-parses-supported-architectures
  "Each recognized architecture string maps to its canonical keyword."
  :cases (("x86-64"  :x86-64 "x86-64")
          ("x86_64"  :x86-64 "x86_64")
          ("arm64"   :arm64  "arm64")
          ("aarch64" :arm64  "aarch64"))
  (expected input)
  (assert-eq expected (cl-cc/cli::%arch-keyword input)))

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

(deftest-each cli-compile-target-keyword-parses-supported-architectures
  "Each recognized architecture string maps to its compilation target keyword."
  :cases (("x86-64"  :x86_64  "x86-64")
          ("x86_64"  :x86_64  "x86_64")
          ("arm64"   :aarch64 "arm64")
          ("aarch64" :aarch64 "aarch64"))
  (expected input)
  (assert-eq expected (cl-cc/cli::%compile-target-keyword input)))

(deftest cli-compile-target-keyword-invalid-signals-error
  (handler-case
      (progn
        (cl-cc/cli::%compile-target-keyword "weird")
        (assert-false t))
    (error (e)
      (assert-true (search "Unknown architecture for compilation"
                           (princ-to-string e))))))

(deftest-each cli-parse-opt-remarks-mode-cases
  "Each recognized mode string maps to its keyword; nil/empty returns nil."
  :cases (("nil-input" nil      nil)
          ("empty-str" nil      "")
          ("all"       :all     "all")
          ("changed"   :changed "changed")
          ("missed"    :missed  "missed"))
  (expected input)
  (assert-eq expected (cl-cc/cli::%parse-opt-remarks-mode input)))

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
