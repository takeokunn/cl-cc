;;;; tests/unit/emit/mir-target-tests.lisp — MIR printer smoke tests and target-desc tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;;; ─── printer smoke tests ───────────────────────────────────────────────

(deftest mir-format-value-shows-id-and-name
  "mir-format-value formats values with %N prefix and name; consts show their value."
  (let* ((fn (mir-make-function :f))
         (v  (mir-new-value fn :name :x))
         (c  (make-mir-const :value 42)))
    (let ((sv (mir-format-value v))
          (sc (mir-format-value c)))
      (assert-true (search "%0" sv))
      (assert-true (search "X"  sv))
      (assert-true (search "42" sc)))))

(deftest mir-print-function-shows-name-and-entry
  "mir-print-function outputs the function name and entry label."
  (let* ((fn  (mir-make-function :smoke))
         (blk (mirf-entry fn))
         (dst (mir-new-value fn :name :r))
         (c   (make-mir-const :value 7 :type :integer)))
    (mir-emit blk :const :dst dst :srcs (list c))
    (mir-emit blk :ret   :srcs (list dst))
    (let ((out (with-output-to-string (s) (mir-print-function fn s))))
      (assert-true (search "SMOKE" out))
      (assert-true (search "ENTRY" out)))))

;;;; ─── target-desc ──────────────────────────────────────────────────────

(deftest target-x86-64-description
  "x86-64 target: name/word-size/endianness, arg registers, return rax, callee-saved."
  (assert-eq :x86-64  (target-name *x86-64-target*))
  (assert-=  8        (target-word-size *x86-64-target*))
  (assert-eq :little  (target-endianness *x86-64-target*))
  (assert-=  16       (target-stack-alignment *x86-64-target*))
  (assert-eq :rax     (target-ret-reg *x86-64-target*))
  (assert-eq :rdi     (first (target-arg-regs *x86-64-target*)))
  (assert-=  6        (length (target-arg-regs *x86-64-target*)))
  (assert-true (member :rbx (target-callee-saved *x86-64-target*)))
  (assert-true (member :r12 (target-callee-saved *x86-64-target*))))

(deftest-each target-non-x86-basic
  "Non-x86 targets: name, word-size/gpr-count, return register, and arg-register count."
  :cases (("aarch64" *aarch64-target* :aarch64 8 :x0  8 nil)
          ("riscv64" *riscv64-target* :riscv64 8 :a0  8 32)
          ("wasm32"  *wasm32-target*  :wasm32  4 nil  0  0))
  (target expected-name expected-word expected-ret expected-args expected-gprs)
  (assert-eq expected-name (target-name target))
  (assert-= expected-word (target-word-size target))
  (when expected-ret
    (assert-eq expected-ret (target-ret-reg target)))
  (assert-= expected-args (length (target-arg-regs target)))
  (when expected-gprs
    (assert-= expected-gprs (target-gpr-count target))))

(deftest-each target-registry-lookup
  "find-target returns the correct target-desc for each registered name."
  :cases (("x86-64"      :x86-64
           (lambda (target)
             (assert-eq *x86-64-target* target)))
          ("aarch64"     :aarch64
           (lambda (target)
             (assert-eq *aarch64-target* target)))
          ("riscv64"     :riscv64
           (lambda (target)
             (assert-eq *riscv64-target* target)))
          ("wasm32"      :wasm32
           (lambda (target)
             (assert-eq *wasm32-target* target)))
          ("nonexistent" :nonexistent
           (lambda (target)
             (assert-null target))))
  (name verify)
  (funcall verify (find-target name)))

(deftest-each target-64-bit-predicate
  "target-64-bit-p returns true for 64-bit targets, false for 32-bit."
  :cases (("x86-64"  *x86-64-target*
           (lambda (target)
             (assert-true (target-64-bit-p target))))
          ("aarch64" *aarch64-target*
           (lambda (target)
             (assert-true (target-64-bit-p target))))
          ("riscv64" *riscv64-target*
           (lambda (target)
             (assert-true (target-64-bit-p target))))
          ("wasm32"  *wasm32-target*
           (lambda (target)
             (assert-false (target-64-bit-p target)))))
  (target verify)
  (funcall verify target))

(deftest-each target-feature-predicate
  "target-has-feature-p correctly detects presence and absence of features."
  :cases (("x86-64-fused-cmp"   *x86-64-target*  :has-fused-cmp-branch
           (lambda (target feature)
             (assert-true (target-has-feature-p target feature))))
          ("aarch64-tail-call"  *aarch64-target* :has-native-tail-call
           (lambda (target feature)
             (assert-true (target-has-feature-p target feature))))
          ("riscv64-psabi"      *riscv64-target* :riscv-elf-psabi
           (lambda (target feature)
             (assert-true (target-has-feature-p target feature))))
          ("wasm32-wasi"        *wasm32-target*  :wasi-0.2
           (lambda (target feature)
             (assert-true (target-has-feature-p target feature))))
          ("x86-64-no-wasi"     *x86-64-target*  :wasi-0.2
           (lambda (target feature)
             (assert-false (target-has-feature-p target feature))))
          ("wasm32-no-sysv"     *wasm32-target*  :sysv-abi
           (lambda (target feature)
             (assert-false (target-has-feature-p target feature)))))
  (target feature verify)
  (funcall verify target feature))

(deftest target-scratch-regs-excluded-from-allocatable
  "Scratch registers must not appear in the allocatable register set."
  (dolist (target (list *x86-64-target* *aarch64-target* *riscv64-target*))
    (let ((alloc   (target-allocatable-regs target))
          (scratch (target-scratch-regs target)))
      (dolist (sr scratch)
        (assert-false (member sr alloc))))))

(deftest target-caller-saved-subset-of-allocatable-and-disjoint-from-callee
  "Caller-saved registers are a subset of allocatable and disjoint from callee-saved."
  (let* ((alloc  (target-allocatable-regs *x86-64-target*))
         (callee (target-callee-saved *x86-64-target*))
         (caller (target-caller-saved *x86-64-target*)))
    (dolist (r caller)
      (assert-true  (member r alloc)))
    (dolist (r caller)
      (assert-false (member r callee)))))

(deftest target-register-and-find-roundtrip
  "register-target followed by find-target returns the same target-desc."
  (let* ((custom (make-target-desc
                  :name      :test-custom
                  :word-size 8
                  :endianness :little
                  :gpr-count 4
                  :gpr-names #(:r0 :r1 :r2 :r3)
                  :arg-regs  '(:r0 :r1)
                  :ret-reg   :r0
                  :stack-alignment 16))
         (got (progn
                (register-target custom)
                (find-target :test-custom))))
    (assert-eq custom got)
    (assert-eq :test-custom (target-name got))
    (remhash :test-custom *target-registry*)))
