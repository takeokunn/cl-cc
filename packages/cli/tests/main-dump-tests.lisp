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

(deftest cli-parse-opt-remarks-mode-invalid-shows-did-you-mean
  (let ((stderr (make-string-output-stream)))
    (with-fake-quit
      (handler-case
          (let ((*error-output* stderr))
            (cl-cc/cli::%parse-opt-remarks-mode "chagned")
            (assert-false t))
        (fake-quit (q)
          (assert-= 2 (fake-quit-code q)))))
    (let ((out (get-output-stream-string stderr)))
      (assert-true (search "did you mean" out))
      (assert-true (search "changed" out)))))

(deftest cli-parse-compile-opts-reads-shared-flags
  (let* ((parsed (make-cli-parsed
                  :command "compile"
                 :flags '(("--pass-pipeline" . t)
                          ("--time-passes" . t)
                          ("--trace-json" . "trace.json")
                          ("--flamegraph" . "fg.svg")
                          ("--stats" . t)
                          ("--trace-emit" . t)
                          ("--shadow-stack" . t)
                          ("--opt-remarks" . "changed"))))
         (opts (cl-cc/cli::%parse-compile-opts parsed)))
    (assert-true (cl-cc/cli::compile-opts-pass-pipeline opts))
    (assert-true (cl-cc/cli::compile-opts-print-pass-timings opts))
    (assert-string= "trace.json" (cl-cc/cli::compile-opts-trace-json-path opts))
    (assert-string= "fg.svg" (cl-cc/cli::compile-opts-flamegraph-path opts))
    (assert-true (cl-cc/cli::compile-opts-print-pass-stats opts))
    (assert-true (cl-cc/cli::compile-opts-trace-emit opts))
    (assert-true (cl-cc/cli::compile-opts-shadow-stack opts))
    (assert-eq :changed (cl-cc/cli::compile-opts-opt-remarks-mode opts))))

(deftest cli-compile-opts-kwargs-expands-struct
  (let* ((opts (cl-cc/cli::make-compile-opts
                :pass-pipeline t
                :print-pass-timings t
                :trace-json-path "trace.json"
                :print-pass-stats t
                :trace-emit t
                :shadow-stack t
                :opt-remarks-mode :missed))
         (kwargs (cl-cc/cli::%compile-opts-kwargs opts :trace-stream)))
    (assert-equal '(:trace-json-stream :trace-stream
                    :print-pass-stats t
                    :pass-pipeline t
                    :inline-threshold-scale 1
                    :shadow-stack t
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

;;; FR-463 regression: %dump-ir-phase dispatches all 6 recognized IR phases
;;; and produces non-empty output for a minimal compilation-result.

(defun %make-minimal-compilation-result (&key (source-location-p t))
  "Build a compilation-result with enough fields populated that every IR phase
dump function can write at least one line without erroring."
  (let ((ast (cl-cc:make-ast-int :value 42
                                 :source-file (and source-location-p "test.lisp")
                                 :source-line (and source-location-p 1)
                                 :source-column (and source-location-p 0))))
    (cl-cc:make-compilation-result
     :program (cl-cc:make-vm-program
               :instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)))
     :assembly "mov rax, 42"
     :globals (make-hash-table :test #'equal)
     :type nil
     :type-env nil
     :cps '(lambda (k) (funcall k 42))
     :ast ast
     :vm-instructions (list (cl-cc:make-vm-const :dst :r0 :value 42))
     :optimized-instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)))))

(deftest-each cli-dump-ir-phase-dispatches-all-phases
  "Each recognized IR phase keyword produces non-empty output via %dump-ir-phase."
  :cases (("ast" :ast)
          ("cps" :cps)
          ("ssa" :ssa)
          ("vm"  :vm)
          ("opt" :opt)
          ("asm" :asm))
  (phase)
  (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream nil)
    (let ((output (get-output-stream-string stream)))
      (assert-true (> (length output) 0)))))

(deftest cli-dump-ir-phase-annotate-source-writes-comment-for-ast
  "With annotate-source T, the AST phase emits a source location comment line."
  (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase :ast result stream t)
    (let ((output (get-output-stream-string stream)))
      (assert-true (search "; source:" output)))))

(deftest-each cli-dump-ir-phase-annotate-source-writes-comment-for-vm-and-opt
  "With annotate-source T, VM and OPT phases emit source location comment lines."
  :cases (("vm" :vm)
          ("opt" :opt))
  (phase)
  (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream t)
    (let ((output (get-output-stream-string stream)))
      (assert-true (search "; source:" output)))))

(deftest cli-dump-ir-phase-asm-output-is-ansi-colored
  "ASM phase wraps assembly output in ANSI opcode color and reset sequences."
  (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase :asm result stream nil)
    (let ((output (get-output-stream-string stream)))
      (assert-true (search cl-cc/cli::+ansi-opcode+ output))
      (assert-true (search cl-cc/cli::+ansi-reset+ output)))))

(deftest-each cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location
  "annotate-source T emits no source comment when the AST has no source location."
  :cases (("ast" :ast)
          ("vm"  :vm)
          ("opt" :opt))
  (phase)
  (let* ((result (%make-minimal-compilation-result :source-location-p nil))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream t)
    (let ((output (get-output-stream-string stream)))
      (assert-true (> (length output) 0))
      (assert-false (search "; source:" output)))))

(deftest cli-dump-ir-phase-phase-table-covers-all-recognized-phases
  "*ir-phase-dump-fns* covers every phase in *ir-phases*."
  (dolist (phase cl-cc/cli::*ir-phases*)
    (assert-true (cdr (assoc phase cl-cc/cli::*ir-phase-dump-fns*)))))
