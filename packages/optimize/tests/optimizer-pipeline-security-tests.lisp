;;;; tests/unit/optimize/optimizer-pipeline-security-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — security hardening passes
;;;;
;;;; Covers: CFI, retpoline, stack canary, shadow stack, HTM.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Control Flow Integrity (CFI) ───────────────────────────────────────────

(deftest optimize-build-cfi-plan-selects-target-specific-guards
  "CFI helper selects ENDBR64 for x86-64 and BTI for AArch64 when indirect calls exist."
  (let ((x86 (cl-cc/optimize::opt-build-cfi-plan :target :x86-64 :has-indirect-calls-p t))
        (arm (cl-cc/optimize::opt-build-cfi-plan :target :aarch64 :has-indirect-calls-p t)))
    (assert-true (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p x86))
    (assert-false (cl-cc/optimize::opt-cfi-plan-insert-bti-p x86))
    (assert-true (cl-cc/optimize::opt-cfi-plan-insert-bti-p arm))
    (assert-false (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p arm))))

(deftest-each optimize-cfi-entry-opcode-by-target-and-calls
  "CFI entry opcode reflects target architecture and indirect-call presence."
  :cases (("x86-with-calls"  :x86-64  t   :endbr64)
          ("arm-with-calls"  :aarch64 t   :bti-c)
          ("x86-no-calls"    :x86-64  nil :none))
  (target has-indirect-calls-p expected)
  (assert-eq expected
             (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p has-indirect-calls-p))))

;;; ─── Retpoline ───────────────────────────────────────────────────────────────

(deftest-each optimize-should-use-retpoline-p-cases
  "opt-should-use-retpoline-p enables mitigation only for x86-64 with indirect branches and no IBRS."
  :cases (("x86-no-ibrs"   :x86-64  nil t)
          ("x86-with-ibrs" :x86-64  t   nil)
          ("aarch64"       :aarch64 nil nil))
  (target supports-ibrs-p expected)
  (assert-equal expected
                (cl-cc/optimize::opt-should-use-retpoline-p
                 :target target :has-indirect-branch-p t :supports-ibrs-p supports-ibrs-p)))

(deftest-each optimize-retpoline-thunk-names-are-register-specific
  "Retpoline thunk names are deterministic and register-specific."
  :cases (("r11" :r11 "__clcc_retpoline_r11")
          ("rax" :rax "__clcc_retpoline_rax"))
  (register expected)
  (assert-equal expected (cl-cc/optimize:opt-retpoline-thunk-name register)))

;;; ─── Stack canary ────────────────────────────────────────────────────────────

(deftest-each optimize-needs-stack-canary-p-cases
  "opt-needs-stack-canary-p is enabled iff :has-stack-buffer-p is true."
  :cases (("with-buffer"    t   t)
          ("without-buffer" nil nil))
  (has-stack-buffer-p expected)
  (assert-equal expected
                (cl-cc/optimize::opt-needs-stack-canary-p
                 :has-stack-buffer-p has-stack-buffer-p)))

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

;;; ─── Shadow stack (CET) ──────────────────────────────────────────────────────

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

;;; ─── Hardware Transactional Memory (HTM) ─────────────────────────────────────

(deftest optimize-build-htm-plan-enables-lock-elision-only-when-supported-and-low-contention
  "HTM helper enables lock elision only under support + low-contention preconditions."
  (let ((enabled (cl-cc/optimize::opt-build-htm-plan
                  :target :x86-64
                  :supports-htm-p t
                  :low-contention-p t))
        (disabled (cl-cc/optimize::opt-build-htm-plan
                   :target :x86-64
                   :supports-htm-p t
                   :low-contention-p nil)))
    (assert-true (cl-cc/optimize::opt-htm-plan-uses-htm-p enabled))
    (assert-eq :xbegin (cl-cc/optimize::opt-htm-plan-begin-opcode enabled))
    (assert-eq :xend (cl-cc/optimize::opt-htm-plan-end-opcode enabled))
    (assert-eq :xabort (cl-cc/optimize::opt-htm-plan-abort-opcode enabled))
    (assert-true (cl-cc/optimize::opt-htm-plan-fallback-lock-p enabled))
    (assert-false (cl-cc/optimize::opt-htm-plan-uses-htm-p disabled))))
