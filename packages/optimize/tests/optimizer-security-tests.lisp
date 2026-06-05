;;;; tests/unit/optimize/optimizer-security-tests.lisp
;;;; Unit tests for security hardening helpers in the optimizer:
;;;; CFI (control-flow integrity), retpoline, stack canary, shadow stack.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── CFI ─────────────────────────────────────────────────────────────────────

(deftest optimize-build-cfi-plan-selects-target-specific-guards
  "CFI helper selects ENDBR64 for x86-64 and BTI for AArch64 when indirect calls exist."
  (let ((x86 (cl-cc/optimize::opt-build-cfi-plan :target :x86-64 :has-indirect-calls-p t))
        (arm (cl-cc/optimize::opt-build-cfi-plan :target :aarch64 :has-indirect-calls-p t)))
    (assert-true (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p x86))
    (assert-false (cl-cc/optimize::opt-cfi-plan-insert-bti-p x86))
    (assert-true (cl-cc/optimize::opt-cfi-plan-insert-bti-p arm))
    (assert-false (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p arm))))

(deftest-each cfi-entry-opcode-cases
  "CFI entry opcode helper exposes the target marker selected by the CFI plan."
  :cases (("x86-64-indirect"    :x86-64  t   :endbr64)
          ("aarch64-indirect"   :aarch64 t   :bti-c)
          ("x86-64-no-indirect" :x86-64  nil :none))
  (target indirect-p expected)
  (assert-eq expected
             (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p indirect-p))))

;;; ─── Retpoline ───────────────────────────────────────────────────────────────

(deftest-each retpoline-cases
  "Retpoline helper enables mitigation only for x86-64 indirect branches without IBRS."
  :cases (("x86-indirect-no-ibrs" :x86-64  t nil t)
          ("x86-indirect-ibrs"    :x86-64  t t   nil)
          ("aarch64"              :aarch64 t nil nil))
  (target indirect-p ibrs-p expected)
  (if expected
      (assert-true (cl-cc/optimize::opt-should-use-retpoline-p
                    :target target :has-indirect-branch-p indirect-p :supports-ibrs-p ibrs-p))
      (assert-false (cl-cc/optimize::opt-should-use-retpoline-p
                     :target target :has-indirect-branch-p indirect-p :supports-ibrs-p ibrs-p))))

(deftest optimize-retpoline-thunk-name-is-target-register-specific
  "Retpoline thunk names are deterministic and register-specific."
  (assert-equal "__clcc_retpoline_r11"
                (cl-cc/optimize:opt-retpoline-thunk-name :r11))
  (assert-equal "__clcc_retpoline_rax"
                (cl-cc/optimize:opt-retpoline-thunk-name :rax)))

;;; ─── Stack canary ────────────────────────────────────────────────────────────

(deftest optimize-needs-stack-canary-p-follows-stack-buffer-presence
  "Stack canary helper is enabled when stack buffer is present."
  (assert-true (cl-cc/optimize::opt-needs-stack-canary-p :has-stack-buffer-p t))
  (assert-false (cl-cc/optimize::opt-needs-stack-canary-p :has-stack-buffer-p nil)))

(deftest optimize-stack-canary-emit-plan-describes-prologue-and-epilogue
  "Stack canary emission plan records guard slot and failure target only when enabled."
  (let ((enabled (cl-cc/optimize:opt-stack-canary-emit-plan
                  :has-stack-buffer-p t
                  :guard-slot -8
                  :failure-target 'panic))
        (disabled (cl-cc/optimize:opt-stack-canary-emit-plan
                   :has-stack-buffer-p nil)))
    (assert-true (getf enabled :enabled-p))
    (assert-= -8 (getf enabled :guard-slot))
    (assert-eq :tls-canary (getf enabled :load-source))
    (assert-eq 'panic (getf enabled :failure-target))
    (assert-false (getf disabled :enabled-p))
    (assert-null (getf disabled :guard-slot))))

(deftest optimize-stack-canary-sequences-describe-prologue-and-epilogue-ops
  "Stack canary sequence helpers expose backend-neutral prologue/epilogue ops."
  (let ((plan (cl-cc/optimize:opt-stack-canary-emit-plan
               :has-stack-buffer-p t
               :guard-slot -8
               :failure-target 'panic)))
    (assert-equal '((:op :load-canary :source :tls-canary :dst :tmp)
                    (:op :store-canary :src :tmp :slot -8))
                  (cl-cc/optimize:opt-stack-canary-prologue-seq
                   plan :temp-reg :tmp))
    (assert-equal '((:op :load-canary :source -8 :dst :tmp)
                    (:op :compare-canary :left :tmp :right :tls-canary)
                    (:op :branch-if-canary-mismatch :target panic))
                  (cl-cc/optimize:opt-stack-canary-epilogue-seq
                   plan :temp-reg :tmp))))

(deftest optimize-stack-canary-sequences-are-empty-when-disabled
  "Stack canary sequence helpers return NIL for disabled plans."
  (let ((plan (cl-cc/optimize:opt-stack-canary-emit-plan
               :has-stack-buffer-p nil)))
    (assert-null (cl-cc/optimize:opt-stack-canary-prologue-seq plan))
    (assert-null (cl-cc/optimize:opt-stack-canary-epilogue-seq plan))))

;;; ─── Shadow stack ────────────────────────────────────────────────────────────

(deftest optimize-build-shadow-stack-plan-enables-only-for-x86-64-with-cet
  "Shadow stack helper enables only for x86-64 targets with CET SS support."
  (let ((enabled (cl-cc/optimize:opt-build-shadow-stack-plan
                  :target :x86-64
                  :supports-cet-ss-p t))
        (no-cet (cl-cc/optimize:opt-build-shadow-stack-plan
                 :target :x86-64
                 :supports-cet-ss-p nil))
        (wrong-arch (cl-cc/optimize:opt-build-shadow-stack-plan
                     :target :aarch64
                     :supports-cet-ss-p t)))
    (assert-true (cl-cc/optimize:opt-shadow-stack-plan-enabled-p enabled))
    (assert-eq :x86-64 (cl-cc/optimize:opt-shadow-stack-plan-target enabled))
    (assert-true (cl-cc/optimize:opt-shadow-stack-plan-needs-incsssp-p enabled))
    (assert-false (cl-cc/optimize:opt-shadow-stack-plan-needs-save-restore-p enabled))
    (assert-false (cl-cc/optimize:opt-shadow-stack-plan-enabled-p no-cet))
    (assert-false (cl-cc/optimize:opt-shadow-stack-plan-enabled-p wrong-arch))))

(deftest optimize-shadow-stack-plan-requires-save-restore-for-nonlocal-control
  "Shadow stack helper flags save/restore when continuations or setjmp-like flow exist."
  (let ((plan (cl-cc/optimize:opt-build-shadow-stack-plan
               :target :x86-64
               :supports-cet-ss-p t
               :has-nonlocal-control-p t)))
    (assert-true (cl-cc/optimize:opt-shadow-stack-plan-needs-save-restore-p plan))))
