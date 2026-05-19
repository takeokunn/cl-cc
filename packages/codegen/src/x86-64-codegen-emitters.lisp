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

(defun emit-x86-64-unlikely-branch-prefix (inst stream)
  "Emit the legacy DS segment override branch-hint prefix for unlikely branches.

Modern x86-64 processors mostly ignore static branch-hint prefixes, so this is a
safe metadata-preserving hint.  Hot/cold CFG layout remains the primary layout
signal when the target CPU does not use the prefix."
  (when (x86-64-unlikely-branch-prefix-p inst)
    ;; 2E = legacy not-taken branch hint prefix on CPUs that honor it.
    (emit-byte #x2E stream)))

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

(defun emit-x86-64-function-prologue (stream frame-pointer-p save-regs)
  "Emit the standard x86-64 function prologue for SAVE-REGS.

When FRAME-POINTER-P is true, SAVE-REGS starts with RBP and the emitted prefix is
the canonical `push rbp; mov rbp, rsp`.  Leaf/frame-pointer-omitted functions
skip the RBP setup and save only the required callee-saved registers."
  (if frame-pointer-p
      (progn
        (emit-push-r64 +rbp+ stream)
        (emit-mov-rr64 +rbp+ +rsp+ stream)
        (dolist (reg (cdr save-regs))
          (emit-push-r64 reg stream)))
      (dolist (reg save-regs)
        (emit-push-r64 reg stream))))

(defun emit-x86-64-function-epilogue (stream frame-pointer-p save-regs)
  "Emit the standard x86-64 function epilogue for SAVE-REGS.

Frame-pointer functions restore non-RBP callee-saved registers, then use
`leave; ret`.  Frame-pointer-omitted functions use `pop ...; ret`."
  (if frame-pointer-p
      (progn
        (dolist (reg (reverse (cdr save-regs)))
          (emit-pop-r64 reg stream))
        (emit-leave stream)
        (emit-ret stream))
      (progn
        (dolist (reg (reverse save-regs))
          (emit-pop-r64 reg stream))
        (emit-ret stream))))

;; Per-instruction emitters (emit-vm-halt-inst through emit-vm-spill-load-inst),
;; *x86-64-emitter-entries*, *x86-64-emitter-table*, and
;; emit-vm-instruction-with-labels are in x86-64-codegen-dispatch.lisp (loaded next).

(defun codegen-hot-cold-ordered-instructions (instructions)
  "Return INSTRUCTIONS in the CFG hot/cold order used for native emission.

FR-036: codegen must emit the flat instruction stream produced by the optimizer's
CFG layout logic, not the original basic-block order.  CFG-FLATTEN-HOT-COLD also
patches broken implicit fall-through edges, so both label-offset computation and
the second emission pass must consume this exact list."
  (let ((cfg (cfg-build instructions)))
    (if (cfg-entry cfg)
        (progn
          (cfg-compute-dominators cfg)
          (cfg-compute-loop-depths cfg)
          (cfg-flatten-hot-cold cfg))
        instructions)))

(defun x86-64-lea-valid-index-p (reg)
  "Return true when physical REG is valid as an x86-64 SIB index."
  (and reg (/= (logand reg #x7) 4)))

(defun x86-64-lea-scale-p (scale)
  "Return true when SCALE is encodable by x86-64 LEA."
  (member scale '(1 2 4 8) :test #'=))

(defun x86-64-power-of-two-scale-from-shift (shift)
  "Return LEA scale for non-negative SHIFT values 0..3, otherwise NIL."
  (case shift
    (0 1)
    (1 2)
    (2 4)
    (3 8)
    (otherwise nil)))

(defun x86-64-vm-int-const-p (inst)
  "Return true when INST is an integer VM constant."
  (and (typep inst 'vm-const)
       (let ((value (vm-value inst)))
         (or (integerp value)
             (null value)
             (eq value t)))))

(defun x86-64-vm-const-reg-value (inst)
  "Return two values: destination register and integer value for VM-CONST INST."
  (values (vm-dst inst) (vm-const-to-integer (vm-value inst))))

(defun x86-64-count-register-uses (instructions)
  "Return a hash table mapping VM registers to use counts in INSTRUCTIONS."
  (let ((uses (make-hash-table :test #'eq)))
    (labels ((use (reg)
               (when reg
                 (incf (gethash reg uses 0)))))
      (dolist (inst instructions uses)
        (dolist (reg (instruction-uses inst))
          (use reg))))))

(defun x86-64-single-use-const-p (inst uses)
  "Return true when INST is an integer constant used exactly once."
  (and (x86-64-vm-int-const-p inst)
       (= (gethash (vm-dst inst) uses 0) 1)))

(defun x86-64-commutative-add-with-reg-p (inst dst reg)
  "Return the other addend when INST computes DST = REG + other, else NIL."
  (when (and (typep inst '(or vm-add vm-integer-add))
             (eq (vm-dst inst) dst))
    (cond
      ((eq (vm-lhs inst) reg) (vm-rhs inst))
      ((eq (vm-rhs inst) reg) (vm-lhs inst))
      (t nil))))

(defun x86-64-lea-internal (dst-reg base-reg index-reg scale displacement)
  "Build an internal LEA instruction from VM registers when encodable."
  (let ((dst (vm-reg-to-x86 dst-reg))
        (base (vm-reg-to-x86 base-reg))
        (index (and index-reg (vm-reg-to-x86 index-reg))))
    (when (and (typep displacement '(signed-byte 32))
               (or (null index)
                   (and (x86-64-lea-valid-index-p index)
                        (x86-64-lea-scale-p scale))))
      (make-x86-64-lea-address :dst dst
                                :base base
                                :index index
                                :scale scale
                                :displacement displacement))))

(defun x86-64-match-lea-pattern (instructions uses)
  "Try to consume a LEA-able prefix of INSTRUCTIONS.

Returns two values: the replacement instruction and the number of source
instructions consumed."
  (destructuring-bind (i0 &optional i1 i2 i3 i4 &rest rest) instructions
    (declare (ignore rest))
    ;; const shift, ash r,r,shift, add r,base, [const disp, add r,disp]
    (when (and i1 i2
               (x86-64-single-use-const-p i0 uses)
               (typep i1 'vm-ash))
      (multiple-value-bind (shift-reg shift) (x86-64-vm-const-reg-value i0)
        (let* ((accum-reg (vm-dst i1))
               (index-reg (vm-lhs i1))
               (scale (x86-64-power-of-two-scale-from-shift shift))
               (base-reg (and scale
                              (eq (vm-rhs i1) shift-reg)
                              (x86-64-commutative-add-with-reg-p i2 accum-reg accum-reg))))
          (when base-reg
            (cond
              ((and i3 i4
                    (x86-64-single-use-const-p i3 uses)
                    (x86-64-commutative-add-with-reg-p i4 accum-reg accum-reg))
               (multiple-value-bind (disp-reg disp) (x86-64-vm-const-reg-value i3)
                 (when (eq (x86-64-commutative-add-with-reg-p i4 accum-reg accum-reg) disp-reg)
                   (let ((lea (x86-64-lea-internal accum-reg base-reg index-reg scale disp)))
                     (when lea (return-from x86-64-match-lea-pattern (values lea 5)))))))
              (t
               (let ((lea (x86-64-lea-internal accum-reg base-reg index-reg scale 0)))
                 (when lea (return-from x86-64-match-lea-pattern (values lea 3))))))))))
    ;; const scale, mul r,r,scale, add r,base, [const disp, add r,disp]
    (when (and i1 i2
               (x86-64-single-use-const-p i0 uses)
               (typep i1 '(or vm-mul vm-integer-mul)))
      (multiple-value-bind (scale-reg scale) (x86-64-vm-const-reg-value i0)
        (let* ((accum-reg (vm-dst i1))
               (index-reg (vm-lhs i1))
               (base-reg (and (x86-64-lea-scale-p scale)
                              (eq (vm-rhs i1) scale-reg)
                              (x86-64-commutative-add-with-reg-p i2 accum-reg accum-reg))))
          (when base-reg
            (cond
              ((and i3 i4
                    (x86-64-single-use-const-p i3 uses)
                    (x86-64-commutative-add-with-reg-p i4 accum-reg accum-reg))
               (multiple-value-bind (disp-reg disp) (x86-64-vm-const-reg-value i3)
                 (when (eq (x86-64-commutative-add-with-reg-p i4 accum-reg accum-reg) disp-reg)
                   (let ((lea (x86-64-lea-internal accum-reg base-reg index-reg scale disp)))
                     (when lea (return-from x86-64-match-lea-pattern (values lea 5)))))))
              (t
               (let ((lea (x86-64-lea-internal accum-reg base-reg index-reg scale 0)))
                 (when lea (return-from x86-64-match-lea-pattern (values lea 3))))))))))
    ;; add r,base, const disp, add r,disp => lea r,[base + r + disp]
    (when (and i1 i2
               (typep i0 '(or vm-add vm-integer-add))
               (x86-64-single-use-const-p i1 uses)
               (typep i2 '(or vm-add vm-integer-add)))
      (let* ((dst-reg (vm-dst i0))
             (index-reg (vm-lhs i0))
             (base-reg (vm-rhs i0)))
        (unless (eq (vm-dst i0) (vm-lhs i0))
          (when (eq (vm-dst i0) (vm-rhs i0))
            (rotatef index-reg base-reg)))
        (when (and base-reg (eq (vm-dst i2) dst-reg))
          (multiple-value-bind (disp-reg disp) (x86-64-vm-const-reg-value i1)
            (when (eq (x86-64-commutative-add-with-reg-p i2 dst-reg dst-reg) disp-reg)
              (let ((lea (x86-64-lea-internal dst-reg base-reg index-reg 1 disp)))
                (when lea (return-from x86-64-match-lea-pattern (values lea 3)))))))))
    (values nil 0)))

(defun x86-64-contiguous-low-mask-width (mask)
  "Return WIDTH when MASK is 2^WIDTH-1, otherwise NIL."
  (when (and (integerp mask) (>= mask 0))
    (let ((width (integer-length mask)))
      (when (= mask (1- (ash 1 width)))
        width))))

(defun x86-64-match-bextr-pattern (instructions uses)
  "Try to consume (ash SRC -START) followed by a low-bit mask as BEXTR."
  (destructuring-bind (i0 &optional i1 i2 i3 &rest rest) instructions
    (declare (ignore rest))
    (when (and i1 i2 i3
               (x86-64-single-use-const-p i0 uses)
               (typep i1 'vm-ash)
               (x86-64-single-use-const-p i2 uses)
               (typep i3 'vm-logand))
      (multiple-value-bind (shift-reg shift) (x86-64-vm-const-reg-value i0)
        (multiple-value-bind (mask-reg mask) (x86-64-vm-const-reg-value i2)
          (let* ((shifted-reg (vm-dst i1))
                 (src-reg (vm-lhs i1))
                 (start (and (integerp shift) (minusp shift) (- shift)))
                 (width (x86-64-contiguous-low-mask-width mask)))
            (when (and start width
                       (< 0 width 64)
                       (< start 64)
                       (<= (+ start width) 64)
                       (eq (vm-rhs i1) shift-reg)
                       (or (and (eq (vm-lhs i3) shifted-reg)
                                (eq (vm-rhs i3) mask-reg))
                           (and (eq (vm-rhs i3) shifted-reg)
                                (eq (vm-lhs i3) mask-reg))))
              (return-from x86-64-match-bextr-pattern
                (values (make-x86-64-bextr-field
                         :dst (vm-reg-to-x86 (vm-dst i3))
                          :src (vm-reg-to-x86 src-reg)
                          :start start
                          :width width)
                         4)))))))
    (values nil 0)))

(defun emit-x86-64-bextr-field-inst (inst stream)
  "Emit internal BEXTR using R11 as the BMI control register."
  (emit-mov-ri64 +r11+
                 (logior (x86-64-bextr-field-start inst)
                         (ash (x86-64-bextr-field-width inst) 8))
                 stream)
  (emit-bextr-rrr64 (x86-64-bextr-field-dst inst)
                    (x86-64-bextr-field-src inst)
                    +r11+
                    stream))

(defun x86-64-peephole-lea-addresses (instructions)
  "Replace LEA-able arithmetic/address and BMI bit-field runs with internal ops."
  (let ((uses (x86-64-count-register-uses instructions))
        (result '())
        (remaining instructions))
    (loop while remaining
          do (multiple-value-bind (replacement consumed)
                  (multiple-value-bind (bextr bextr-consumed)
                      (x86-64-match-bextr-pattern remaining uses)
                    (if bextr
                        (values bextr bextr-consumed)
                        (x86-64-match-lea-pattern remaining uses)))
                (if replacement
                    (progn
                      (push replacement result)
                     (setf remaining (nthcdr consumed remaining)))
                    (progn
                      (push (first remaining) result)
                      (setf remaining (rest remaining))))))
    (nreverse result)))

(defun x86-64-relaxable-branch-p (inst)
  "Return T when INST participates in x86-64 rel8/rel32 branch relaxation."
  (typep inst '(or vm-jump vm-jump-zero)))

(defun x86-64-branch-target-offset (inst label-offsets)
  "Return byte offset of INST's branch target from LABEL-OFFSETS."
  (let* ((target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets)))
    (unless target-pos
      (error "Unknown x86-64 branch target label: ~A" target-label))
    target-pos))

(defun x86-64-branch-displacement (inst current-pos label-offsets)
  "Return INST's branch displacement under the current relaxation state."
  (- (x86-64-branch-target-offset inst label-offsets)
     (+ current-pos (instruction-size inst))))

(defun x86-64-relax-branch-encodings (instructions prologue-size)
  "Compute label offsets and branch encodings using iterative rel8→rel32 relaxation.

Pass 1 starts with every VM branch as a short rel8 branch.  Each iteration builds
label offsets with the current branch-size choices, marks any out-of-range short
branch as :NEAR, and repeats because expanding one branch can move later labels
and make additional short branches overflow.  Near branches are never shrunk,
which makes the process monotonic and bounded by the number of branches."
  (let ((encodings (make-hash-table :test #'eq))
        (final-offsets nil))
    (loop
      for iteration from 0
      for changed-p = nil
      do (let* ((*x86-64-branch-encodings* encodings)
                (label-offsets (build-label-offsets instructions prologue-size))
                (pos prologue-size))
           (setf final-offsets label-offsets)
           (dolist (inst instructions)
             (when (and (x86-64-relaxable-branch-p inst)
                        (eq (x86-64-branch-encoding inst) :short)
                        (not (fits-in-rel8-p
                              (x86-64-branch-displacement inst pos label-offsets))))
               (setf (gethash inst encodings) :near
                     changed-p t))
             (incf pos (instruction-size inst))))
      when (not changed-p)
        return (values final-offsets encodings)
      when (> iteration (length instructions))
        do (error "x86-64 branch relaxation did not converge"))))

(defun emit-vm-program (program stream)
  "Emit machine code for entire VM program.
    Uses two-pass approach: first pass builds label offset table,
    second pass emits code with resolved jump targets."
  ;; FR-370: Stack Map Generation — per-safepoint stack frame layout recorded for precise GC root scanning
  ;; FR-371: GC Safepoints — signal-based vs polling safepoint mechanism for Stop-The-World coordination
  (let* ((instructions (vm-program-instructions program))
         (calling-convention (vm-program-calling-convention-object program))
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
                                (not (calling-convention-omit-frame-pointer-p calling-convention))
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
            (ordered-instructions
                 (x86-64-peephole-lea-addresses
                  (codegen-hot-cold-ordered-instructions instructions)))
            (branch-encodings nil)
           ;; First pass: relax branches and build label offset table
            (label-offsets
              (let ((*x86-64-use-retpoline* (or *x86-64-use-retpoline* use-retpoline-p))
                    (*x86-64-cfi-enabled* (or *x86-64-cfi-enabled* (getf cfi-plan :enabled-p)))
                    (*x86-64-shadow-stack-enabled*
                      (or *x86-64-shadow-stack-enabled*
                          (opt-shadow-stack-plan-enabled-p shadow-stack-plan))))
                (multiple-value-bind (offsets encodings)
                    (x86-64-relax-branch-encodings ordered-instructions prologue-size)
                  (setf branch-encodings encodings)
                  offsets))))

    ;; CFI entry marker (FR-315): must be at function entry.
    (emit-x86-64-cfi-entry stream cfi-plan)

    ;; Stack probing: touch each guard page before the large frame is used.
    (emit-x86-64-stack-probes stream probe-count)

    ;; Prologue: save only the callee-saved registers actually used.
    (emit-x86-64-function-prologue stream frame-pointer-p save-regs)

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
                (opt-shadow-stack-plan-enabled-p shadow-stack-plan)))
          (*x86-64-branch-encodings* branch-encodings))
      (dolist (inst ordered-instructions)
        (emit-vm-instruction-with-labels inst stream pos label-offsets)
        (incf pos (instruction-size inst))))

    (when (plusp spill-frame-size)
      (emit-add-ri32 +rsp+ spill-frame-size stream))

    ;; Stack protector epilogue (FR-317)
    (emit-x86-64-stack-canary-epilogue stream canary-plan frame-pointer-p)

    ;; Epilogue: restore callee-saved registers and return.
    (emit-x86-64-function-epilogue stream frame-pointer-p save-regs)))

;;; Public API

(defun compile-to-x86-64-bytes (program &key retpoline spectre-mitigations
                                           stack-protector shadow-stack
                                           asan msan tsan ubsan hwasan)
  "Compile VM program to x86-64 machine code bytes.

   Returns: (simple-array (unsigned-byte 8) (*))"
  ;; Run register allocation before emitting machine code
  (declare (ignore shadow-stack))
  (let ((sanitizer-enabled (or asan msan tsan ubsan hwasan))
        (*current-calling-convention* (vm-program-calling-convention-object program)))
  (let* ((instructions (schedule-pre-ra (vm-program-instructions program)))
         (float-vregs (x86-64-compute-float-vregs instructions))
         (target (x86-64-codegen-target))
           (ra (allocate-registers instructions target float-vregs))
           (allocated-program (make-vm-program
                                 :instructions (opt-analyze-branch-weights
                                                (regalloc-instructions ra))
                                 :result-register (vm-program-result-register program)
                                 :leaf-p (vm-program-leaf-p program)
                                 :calling-convention (vm-program-calling-convention program)
                                 :function-conventions (vm-program-function-conventions program))))
    ;; Store the regalloc result for use during code generation
    (let ((*current-regalloc* ra)
          (*current-float-vregs* float-vregs)
          (*x86-64-stack-protector-enabled*
            (or stack-protector sanitizer-enabled *x86-64-stack-protector-enabled*))
          (*x86-64-omit-frame-pointer*
            (if (or stack-protector sanitizer-enabled *x86-64-stack-protector-enabled*)
                nil
                *x86-64-omit-frame-pointer*))
          (*x86-64-use-retpoline* (or retpoline spectre-mitigations *x86-64-use-retpoline*))
          (*x86-64-spectre-mitigations-enabled*
            (or spectre-mitigations *x86-64-spectre-mitigations-enabled*)))
      (x86-64-peephole-optimize
       (with-output-to-vector (stream)
         (emit-vm-program allocated-program stream)))))))
