(in-package :cl-cc/codegen)

(defun emit-x86-64-stack-probes (stream probe-count)
  "Emit one non-mutating page touch per PROBE-COUNT below RSP."
  (loop for page from 1 to probe-count
        do (emit-or-mem-rsp-disp32-imm8 (- (* page +stack-probe-page-size+)) 0 stream)))

(defun emit-x86-64-cfi-entry (stream cfi-plan)
  "Emit x86-64 CFI entry marker (ENDBR64) when enabled in CFI-PLAN."
  (when (eq (getf cfi-plan :entry-opcode) :endbr64)
    ;; ENDBR64 = F3 0F 1E FA
    (emit-byte #xF3 stream)
    (emit-byte #x0F stream)
    (emit-byte #x1E stream)
    (emit-byte #xFA stream)))

(defun emit-x86-64-stack-canary-prologue (stream canary-plan frame-pointer-p)
  "Emit stack canary prologue sequence when enabled."
  (when (and frame-pointer-p (getf canary-plan :enabled-p))
    (let ((slot (getf canary-plan :guard-slot)))
      (emit-mov-rm64-fs-disp32 +rax+ +x86-64-tls-canary-disp32+ stream)
      (emit-mov-mr64 +rbp+ slot +rax+ stream))))

(defun emit-x86-64-stack-canary-epilogue (stream canary-plan frame-pointer-p)
  "Emit stack canary epilogue check when enabled.

On mismatch, trap with UD2."
  (when (and frame-pointer-p (getf canary-plan :enabled-p))
    (let ((slot (getf canary-plan :guard-slot)))
      (emit-mov-rm64 +rax+ +rbp+ slot stream)
      (emit-cmp-rm64-fs-disp32 +rax+ +x86-64-tls-canary-disp32+ stream)
      ;; JE +2 (equal => skip trap, mismatch => fallthrough into UD2)
      (emit-byte #x0F stream)
      (emit-byte #x84 stream)
      (emit-dword 2 stream)
      (emit-byte #x0F stream)
      (emit-byte #x0B stream))))

;; Per-instruction emitters (emit-vm-halt-inst through emit-vm-spill-load-inst),
;; *x86-64-emitter-entries*, *x86-64-emitter-table*, and
;; emit-vm-instruction-with-labels are in x86-64-codegen-dispatch.lisp (loaded next).

(defun emit-vm-program (program stream)
  "Emit machine code for entire VM program.
    Uses two-pass approach: first pass builds label offset table,
    second pass emits code with resolved jump targets."
  ;; FR-370: Stack Map Generation — per-safepoint stack frame layout recorded for precise GC root scanning
  ;; FR-371: GC Safepoints — signal-based vs polling safepoint mechanism for Stop-The-World coordination
  (let* ((instructions (vm-program-instructions program))
         (cfg (cfg-build instructions))
         (has-indirect-calls-p
           (some (lambda (inst)
                   (typep inst '(or vm-call vm-tail-call vm-generic-call)))
                 instructions))
         (cfi-plan (x86-64-cfi-plan :has-indirect-calls-p has-indirect-calls-p))
         (cfi-entry-size (if (eq (getf cfi-plan :entry-opcode) :endbr64) 4 0))
         (leaf-p (vm-program-leaf-p program))
          (spill-count (regalloc-spill-count *current-regalloc*))
          (red-zone-spill-p (x86-64-red-zone-spill-p leaf-p spill-count))
          (frame-pointer-p (and (not *x86-64-omit-frame-pointer*)
                                (not red-zone-spill-p)
                                (or (not leaf-p)
                                    (plusp spill-count))))
          (callee-saved (x86-64-used-callee-saved-regs *current-regalloc*
                                                        (x86-64-codegen-target)))
          (save-regs (if frame-pointer-p
                         (cons +rbp+ callee-saved)
                         callee-saved))
          (spill-frame-size (if (and (not frame-pointer-p)
                                     (not red-zone-spill-p)
                                     (plusp spill-count))
                                (* 8 spill-count)
                                0))
          (*current-spill-base-reg* (if frame-pointer-p +rbp+ +rsp+))
          (*current-spill-offset-bias* (if frame-pointer-p 0 spill-frame-size))
          (supports-ibrs-p (x86-64-supports-ibrs-p))
          (use-retpoline-p
            (opt-should-use-retpoline-p
             :target :x86-64
             :has-indirect-branch-p has-indirect-calls-p
             :supports-ibrs-p supports-ibrs-p))
          (shadow-stack-plan
            (opt-build-shadow-stack-plan
             :target :x86-64
             :supports-cet-ss-p (x86-64-supports-cet-ss-p)
             :has-nonlocal-control-p (x86-64-program-has-nonlocal-control-p instructions)
             :has-setjmp-longjmp-p nil))
          (has-stack-buffer-p (and *x86-64-stack-protector-enabled*
                                   (x86-64-program-has-stack-buffer-p instructions)))
          (canary-plan (x86-64-stack-canary-plan
                        :has-stack-buffer-p has-stack-buffer-p))
          (probe-count (stack-probe-count
                          (x86-64-stack-frame-size save-regs spill-frame-size)))
          (probe-size (* probe-count +x86-64-stack-probe-size+))
           (canary-prologue-size (if (and frame-pointer-p (getf canary-plan :enabled-p)) 13 0))
           (frame-pointer-establish-size (if frame-pointer-p 3 0))
           (spill-frame-adjust-size (if (plusp spill-frame-size) 7 0))
          ;; Push sizes: 1 byte for RAX-RDI, 2 bytes for R8-R15 (REX.B prefix).
           (prologue-size (+ cfi-entry-size
                              probe-size
                              (reduce #'+ save-regs :key #'push-r64-byte-size :initial-value 0)
                              frame-pointer-establish-size
                              canary-prologue-size
                              spill-frame-adjust-size))
             (ordered-instructions (if (cfg-entry cfg)
                                       (progn
                                        (cfg-compute-dominators cfg)
                                       (cfg-compute-loop-depths cfg)
                                      (cfg-flatten-hot-cold cfg))
                                    instructions))
           ;; First pass: build label offset table
           (label-offsets
             (let ((*x86-64-use-retpoline* (or *x86-64-use-retpoline* use-retpoline-p))
                   (*x86-64-cfi-enabled* (or *x86-64-cfi-enabled* (getf cfi-plan :enabled-p))))
               (build-label-offsets ordered-instructions prologue-size))))

    ;; CFI entry marker (FR-315): must be at function entry.
    (emit-x86-64-cfi-entry stream cfi-plan)

    ;; Stack probing: touch each guard page before the large frame is used.
    (emit-x86-64-stack-probes stream probe-count)

    ;; Prologue: save only the callee-saved registers actually used.
    ;; When frame pointer is enabled, establish RBP after saving old RBP.
    (if frame-pointer-p
        (progn
          (emit-push-r64 +rbp+ stream)
          (emit-mov-rr64 +rbp+ +rsp+ stream)
          (dolist (reg (cdr save-regs))
            (emit-push-r64 reg stream)))
        (dolist (reg save-regs)
          (emit-push-r64 reg stream)))

    (when (plusp spill-frame-size)
      (emit-sub-ri32 +rsp+ spill-frame-size stream))

    ;; Stack protector prologue (FR-317)
    (emit-x86-64-stack-canary-prologue stream canary-plan frame-pointer-p)

    ;; Second pass: emit instructions with resolved jumps
    (let ((pos prologue-size)
          (*x86-64-use-retpoline* (or *x86-64-use-retpoline* use-retpoline-p))
          (*x86-64-cfi-enabled* (or *x86-64-cfi-enabled* (getf cfi-plan :enabled-p)))
          (*x86-64-shadow-stack-enabled*
            (or *x86-64-shadow-stack-enabled*
                (opt-shadow-stack-plan-enabled-p shadow-stack-plan))))
      (dolist (inst ordered-instructions)
        (emit-vm-instruction-with-labels inst stream pos label-offsets)
        (incf pos (instruction-size inst))))

    (when (plusp spill-frame-size)
      (emit-add-ri32 +rsp+ spill-frame-size stream))

    ;; Stack protector epilogue (FR-317)
    (emit-x86-64-stack-canary-epilogue stream canary-plan frame-pointer-p)

    ;; Epilogue: restore callee-saved registers in reverse order
    (dolist (reg (reverse save-regs))
      (emit-pop-r64 reg stream))

    ;; Return
    (emit-ret stream)))

;;; Public API

(defun compile-to-x86-64-bytes (program &key retpoline stack-protector shadow-stack
                                          asan msan tsan ubsan hwasan)
  "Compile VM program to x86-64 machine code bytes.

   Returns: (simple-array (unsigned-byte 8) (*))"
  ;; Run register allocation before emitting machine code
  (declare (ignore shadow-stack))
  (let ((sanitizer-enabled (or asan msan tsan ubsan hwasan)))
  (let* ((instructions (vm-program-instructions program))
         (float-vregs (x86-64-compute-float-vregs instructions))
         (target (x86-64-codegen-target))
         (ra (allocate-registers instructions target float-vregs))
         (allocated-program (make-vm-program
                               :instructions (regalloc-instructions ra)
                               :result-register (vm-program-result-register program)
                               :leaf-p (vm-program-leaf-p program))))
    ;; Store the regalloc result for use during code generation
    (let ((*current-regalloc* ra)
          (*current-float-vregs* float-vregs)
          (*x86-64-stack-protector-enabled*
            (or stack-protector sanitizer-enabled *x86-64-stack-protector-enabled*))
          (*x86-64-omit-frame-pointer*
            (if (or stack-protector sanitizer-enabled *x86-64-stack-protector-enabled*)
                nil
                *x86-64-omit-frame-pointer*))
          (*x86-64-use-retpoline* (or retpoline *x86-64-use-retpoline*)))
      (with-output-to-vector (stream)
        (emit-vm-program allocated-program stream))))))
