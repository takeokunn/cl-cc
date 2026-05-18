(in-package :cl-cc/optimize)

(defstruct (opt-cfi-plan (:conc-name opt-cfi-plan-))
  "Control-Flow Integrity planning record."
  (insert-endbr64-p nil :type boolean)
  (insert-bti-p nil :type boolean)
  (protect-indirect-calls-p t :type boolean))

(defun opt-build-cfi-plan (&key target has-indirect-calls-p)
  "Build a conservative CFI insertion plan for TARGET (:x86-64 or :aarch64)."
  (make-opt-cfi-plan
   :insert-endbr64-p (and has-indirect-calls-p (eq target :x86-64))
   :insert-bti-p (and has-indirect-calls-p (eq target :aarch64))
   :protect-indirect-calls-p has-indirect-calls-p))

(defun opt-cfi-entry-opcode (plan)
  "Return the function-entry CFI marker opcode selected by PLAN."
  (cond
    ((opt-cfi-plan-insert-endbr64-p plan) :endbr64)
    ((opt-cfi-plan-insert-bti-p plan) :bti-c)
    (t :none)))

(defun opt-should-use-retpoline-p (&key target has-indirect-branch-p supports-ibrs-p)
  "Return T when retpoline mitigation should be enabled.

Retpoline is x86-64 specific and unnecessary when IBRS/eIBRS is available."
  (and (eq target :x86-64)
       has-indirect-branch-p
       (not supports-ibrs-p)))

(defun opt-retpoline-thunk-name (target-reg)
  "Return the module-local retpoline thunk name for TARGET-REG."
  (format nil "__clcc_retpoline_~(~a~)" target-reg))

(defun opt-needs-stack-canary-p (&key has-stack-buffer-p)
  "Return T when stack protector instrumentation should be inserted."
  (not (null has-stack-buffer-p)))

(defun opt-stack-canary-emit-plan (&key has-stack-buffer-p guard-slot failure-target)
  "Return a backend-neutral stack canary prologue/epilogue emission plan."
  (let ((enabled-p (opt-needs-stack-canary-p :has-stack-buffer-p has-stack-buffer-p)))
    (list :enabled-p enabled-p
          :guard-slot (and enabled-p (or guard-slot :stack-canary))
          :load-source (and enabled-p :tls-canary)
          :failure-target (and enabled-p (or failure-target '__stack_chk_fail)))))

(defun opt-stack-canary-prologue-seq (plan &key (temp-reg :stack-canary-temp))
  "Return abstract prologue operations for PLAN's stack canary setup."
  (when (getf plan :enabled-p)
    `((:op :load-canary
       :source ,(getf plan :load-source)
       :dst ,temp-reg)
      (:op :store-canary
       :src ,temp-reg
       :slot ,(getf plan :guard-slot)))))

(defun opt-stack-canary-epilogue-seq (plan &key (temp-reg :stack-canary-temp))
  "Return abstract epilogue operations for PLAN's stack canary verification."
  (when (getf plan :enabled-p)
    `((:op :load-canary
       :source ,(getf plan :guard-slot)
       :dst ,temp-reg)
      (:op :compare-canary
       :left ,temp-reg
       :right ,(getf plan :load-source))
      (:op :branch-if-canary-mismatch
       :target ,(getf plan :failure-target)))))

(defstruct (opt-shadow-stack-plan (:conc-name opt-shadow-stack-plan-))
  "Shadow Stack (CET SS) planning record for return-address verification."
  (enabled-p nil :type boolean)
  target
  (needs-incsssp-p nil :type boolean)
  (needs-save-restore-p nil :type boolean))

(defun opt-build-shadow-stack-plan (&key target supports-cet-ss-p
                                      has-nonlocal-control-p
                                      has-setjmp-longjmp-p)
  "Build a conservative Shadow Stack plan for TARGET.

CET Shadow Stack is x86-64 specific. Non-local control transfers require
explicit save/restore planning before backend instruction emission is safe."
  (let* ((enabled-p (and (eq target :x86-64) supports-cet-ss-p))
         (save-restore-p (and enabled-p
                              (or has-nonlocal-control-p
                                  has-setjmp-longjmp-p))))
    (make-opt-shadow-stack-plan
     :enabled-p enabled-p
     :target (and enabled-p target)
     :needs-incsssp-p enabled-p
     :needs-save-restore-p save-restore-p)))

