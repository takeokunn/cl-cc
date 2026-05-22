(in-package :cl-cc/codegen)

(defun emit-x86-64-stack-probes (stream probe-count)
  "Emit one non-mutating page touch per PROBE-COUNT below RSP."
  (loop for page from 1 to probe-count
        do (emit-or-mem-rsp-disp32-imm8 (- (* page +stack-probe-page-size+)) 0 stream)))

(defun emit-x86-64-speculation-barrier (stream)
  "Emit an LFENCE speculation barrier for FR-534 speculative execution mitigation."
  (emit-byte #x0F stream)
  (emit-byte #xAE stream)
  (emit-byte #xE8 stream))

(defun x86-64-speculative-execution-mitigation-enabled-p ()
  "Return true when x86-64 speculative execution mitigation is enabled."
  (or *x86-64-spectre-mitigations-enabled*
      *x86-64-use-retpoline*))

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

(defun emit-x86-64-shrink-pseudo (inst stream)
  "Emit x86-64 shrink-wrap pseudo-instruction INST."
  (typecase inst
    (x86-64-shrink-save
     (emit-push-r64 (x86-64-shrink-save-reg inst) stream))
    (x86-64-shrink-restore
     (emit-pop-r64 (x86-64-shrink-restore-reg inst) stream))))

(defun x86-64-phys-code->keyword (code)
  "Return the physical register keyword for x86-64 register CODE."
  (car (rassoc code *phys-reg-to-x86-code* :test #'=)))

(defun %native-inst-touches-phys-p (inst phys-key assignment)
  "Return true when INST reads or writes a virtual register assigned to PHYS-KEY."
  (or (some (lambda (reg) (eq (gethash reg assignment) phys-key))
            (instruction-defs inst))
      (some (lambda (reg) (eq (gethash reg assignment) phys-key))
            (instruction-uses inst))))

(defun %native-inst-reads-phys-p (inst phys-key assignment)
  "Return true when INST reads a virtual register assigned to PHYS-KEY."
  (some (lambda (reg) (eq (gethash reg assignment) phys-key))
        (instruction-uses inst)))

(defun %native-common-dominator (blocks)
  "Return the nearest common dominator for BLOCKS."
  (labels ((depth (b)
             (loop with n = 0
                   for cur = b then (bb-idom cur)
                   while (and cur (not (eq cur (bb-idom cur))))
                   do (incf n)
                   finally (return n)))
           (raise (b n)
             (loop repeat n do (setf b (bb-idom b)) finally (return b)))
           (join2 (a b)
             (let ((da (depth a))
                   (db (depth b)))
               (when (> da db) (setf a (raise a (- da db))))
               (when (> db da) (setf b (raise b (- db da))))
               (loop until (eq a b)
                     do (setf a (bb-idom a)
                              b (bb-idom b)))
               a)))
    (reduce #'join2 (rest blocks) :initial-value (first blocks))))

(defun %native-block-terminator (block)
  "Return BLOCK's final control-transfer instruction, if present."
  (let ((last (car (last (bb-instructions block)))))
    (and (typep last '(or vm-jump vm-jump-zero vm-ret vm-halt)) last)))

(defun %native-unsafe-restore-before-terminator-p (block phys-key assignment)
  "True when restoring before BLOCK's terminator would clobber its condition/value."
  (let ((term (%native-block-terminator block)))
    (and term (%native-inst-reads-phys-p term phys-key assignment))))

(defun %native-shrink-wrap-plan (instructions reg-codes assignment make-save make-restore)
  "Return three values: annotated instructions, entry-save regs, final-restore regs.

The planner is intentionally conservative: a save is placed at the nearest common
dominator of all blocks touching the callee-saved register, and restores are
placed at every edge leaving that dominated region (before the block terminator).
If an edge restore would clobber a terminator that reads the same register, the
register falls back to the monolithic entry/final epilogue path."
  (let ((cfg (cfg-build instructions))
        (entry-regs nil)
        (final-regs nil))
    (unless (cfg-entry cfg)
      (return-from %native-shrink-wrap-plan (values instructions reg-codes reg-codes)))
    (cfg-compute-dominators cfg)
    (let ((save-map (make-hash-table :test #'eq))
          (restore-map (make-hash-table :test #'eq))
          (fallback nil))
      (dolist (reg-code reg-codes)
        (let* ((phys-key (if (numberp reg-code)
                             (x86-64-phys-code->keyword reg-code)
                             reg-code))
               (touch-blocks
                 (loop for block across (cfg-blocks cfg)
                       when (some (lambda (inst)
                                    (%native-inst-touches-phys-p inst phys-key assignment))
                                  (bb-instructions block))
                         collect block)))
          (cond
            ((null touch-blocks) nil)
            (t
             (let* ((save-block (%native-common-dominator touch-blocks))
                    (region (loop for block across (cfg-blocks cfg)
                                  when (cfg-dominates-p save-block block)
                                    collect block))
                    (boundary-blocks
                      (remove-duplicates
                       (loop for block in region
                             when (or (null (bb-successors block))
                                      (some (lambda (succ)
                                              (not (cfg-dominates-p save-block succ)))
                                            (bb-successors block)))
                               collect block)
                       :test #'eq)))
               (if (or (eq save-block (cfg-entry cfg))
                       (some (lambda (block)
                               (%native-unsafe-restore-before-terminator-p block phys-key assignment))
                             boundary-blocks))
                   (push reg-code fallback)
                   (progn
                     (push (funcall make-save reg-code) (gethash save-block save-map))
                     (dolist (block boundary-blocks)
                       (push (funcall make-restore reg-code) (gethash block restore-map))))))))))
      (setf entry-regs (nreverse fallback)
            final-regs (nreverse fallback))
      (let ((result nil))
        (loop for block across (cfg-blocks cfg) do
          (when (bb-label block)
            (push (bb-label block) result))
          (dolist (pseudo (reverse (gethash block save-map)))
            (push pseudo result))
          (let* ((insts (bb-instructions block))
                 (term (%native-block-terminator block))
                 (body (if term (butlast insts) insts)))
            (dolist (inst body)
              (push inst result))
            (dolist (pseudo (reverse (gethash block restore-map)))
              (push pseudo result))
            (when term
              (push term result))))
        (values (nreverse result) entry-regs final-regs)))))

(defun x86-64-shrink-wrap-instructions (instructions save-regs assignment)
  "Annotate INSTRUCTIONS with x86-64 callee-save shrink-wrap pseudo-ops."
  (%native-shrink-wrap-plan instructions save-regs assignment
                            (lambda (reg) (make-x86-64-shrink-save :reg reg))
                            (lambda (reg) (make-x86-64-shrink-restore :reg reg))))

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
                                   (or *x86-64-stack-protector-enabled*
                                       (x86-64-program-has-stack-buffer-p instructions))))
          (canary-plan (x86-64-stack-canary-plan
                        :has-stack-buffer-p has-stack-buffer-p))
           (shrink-wrap-p (and *shrink-wrap-enabled*
                               (zerop spill-count)
                               (not has-indirect-calls-p)))
           (ordered-body
             (x86-64-peephole-lea-addresses
              (codegen-hot-cold-ordered-instructions instructions)))
           (shrink-entry-regs nil)
           (shrink-final-regs nil)
           (ordered-instructions
             (if shrink-wrap-p
                 (multiple-value-bind (annotated entry-regs final-regs)
                     (x86-64-shrink-wrap-instructions ordered-body callee-saved
                                                       (regalloc-assignment *current-regalloc*))
                   (setf shrink-entry-regs entry-regs
                         shrink-final-regs final-regs)
                   annotated)
                 (progn
                   (setf shrink-entry-regs callee-saved
                         shrink-final-regs callee-saved)
                   ordered-body)))
           (entry-save-regs (if frame-pointer-p
                                (cons +rbp+ shrink-entry-regs)
                                shrink-entry-regs))
           (final-save-regs (if frame-pointer-p
                                (cons +rbp+ shrink-final-regs)
                                shrink-final-regs))
           (probe-count (stack-probe-count
                         (x86-64-stack-frame-size save-regs spill-frame-size)))
          (probe-size (* probe-count +x86-64-stack-probe-size+))
            (canary-prologue-size (if (and frame-pointer-p (getf canary-plan :enabled-p)) 13 0))
            (safe-stack-prologue-size (x86-64-safe-stack-prologue-size))
            (frame-pointer-establish-size (if frame-pointer-p 3 0))
            (spill-frame-adjust-size (if (plusp spill-frame-size) 7 0))
          ;; Push sizes: 1 byte for RAX-RDI, 2 bytes for R8-R15 (REX.B prefix).
           (prologue-size (+ cfi-entry-size
                              probe-size
                                (reduce #'+ entry-save-regs :key #'push-r64-byte-size :initial-value 0)
                                frame-pointer-establish-size
                                canary-prologue-size
                                safe-stack-prologue-size
                                spill-frame-adjust-size))
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
                  offsets)))
            (landing-pads
              (x86-64-build-landing-pad-table ordered-instructions label-offsets prologue-size)))

    ;; CFI entry marker (FR-315): must be at function entry.
    (emit-x86-64-cfi-entry stream cfi-plan)

    ;; Stack probing: touch each guard page before the large frame is used.
    (emit-x86-64-stack-probes stream probe-count)

    ;; Prologue: save only the callee-saved registers actually used.
    (emit-x86-64-function-prologue stream frame-pointer-p entry-save-regs)

    (when (plusp spill-frame-size)
      (emit-sub-ri32 +rsp+ spill-frame-size stream))

    ;; Stack protector prologue (FR-317)
    (emit-x86-64-stack-canary-prologue stream canary-plan frame-pointer-p)

    ;; SafeStack dual-stack layout (FR-771): materialize the unsafe-stack pointer
    ;; from TLS before stack-memory operations can be lowered to it.  The minimal
    ;; pure-CL backend keeps ordinary spills on the architectural stack for
    ;; compatibility, but emits the ABI-visible load/store hooks so native frames
    ;; have a distinct unsafe-stack root when the feature is enabled.
    (emit-x86-64-safe-stack-load-pointer +r11+ stream)

    ;; Second pass: emit instructions with resolved jumps
    (let ((pos prologue-size)
          (*x86-64-use-retpoline* (or *x86-64-use-retpoline* use-retpoline-p))
          (*x86-64-cfi-enabled* (or *x86-64-cfi-enabled* (getf cfi-plan :enabled-p)))
          (*x86-64-shadow-stack-enabled*
            (or *x86-64-shadow-stack-enabled*
                (opt-shadow-stack-plan-enabled-p shadow-stack-plan)))
          (*x86-64-branch-encodings* branch-encodings))
      (dolist (inst ordered-instructions)
        (if (typep inst '(or x86-64-shrink-save x86-64-shrink-restore))
            (emit-x86-64-shrink-pseudo inst stream)
            (emit-sanitized-vm-instruction-with-labels inst stream pos label-offsets))
        (incf pos (instruction-size inst))))

    (when (plusp spill-frame-size)
      (emit-add-ri32 +rsp+ spill-frame-size stream))

    ;; Stack protector epilogue (FR-317)
    (emit-x86-64-stack-canary-epilogue stream canary-plan frame-pointer-p)

    ;; Preserve the current unsafe-stack pointer back to TLS on return.
    (emit-x86-64-safe-stack-store-pointer +r11+ stream)

    ;; Epilogue: restore callee-saved registers and return.
    (emit-x86-64-function-epilogue stream frame-pointer-p final-save-regs)

    ;; Opt-in zero-cost EH landing-pad code.  Appended after the normal epilogue
    ;; so existing binary text layout stays compatible when the flag is disabled.
    (emit-x86-64-landing-pads landing-pads stream)))

;;; Public API

(defun compile-to-x86-64-bytes (program &key retpoline spectre-mitigations
                                            stack-protector shadow-stack
                                            asan msan tsan ubsan hwasan
                                            eh-model)
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
            (post-ra-instructions (schedule-post-ra (regalloc-instructions ra) ra))
            (allocated-program (make-vm-program
                                  :instructions (opt-analyze-branch-weights
                                                 post-ra-instructions)
                                  :result-register (vm-program-result-register program)
                                 :leaf-p (vm-program-leaf-p program)
                                 :calling-convention (vm-program-calling-convention program)
                                 :function-conventions (vm-program-function-conventions program))))
    ;; Store the regalloc result for use during code generation
    (let ((*current-regalloc* ra)
          (*current-float-vregs* float-vregs)
          (*asan-instrumentation-enabled* (or asan hwasan *asan-instrumentation-enabled*))
          (*ubsan-instrumentation-enabled* (or ubsan msan tsan *ubsan-instrumentation-enabled*))
          (*x86-64-stack-protector-enabled*
            (or stack-protector sanitizer-enabled *x86-64-stack-protector-enabled*))
          (*x86-64-omit-frame-pointer*
            (if (or stack-protector sanitizer-enabled *x86-64-stack-protector-enabled*)
                nil
                *x86-64-omit-frame-pointer*))
           (*x86-64-use-retpoline* (or retpoline spectre-mitigations *x86-64-use-retpoline*))
           (*x86-64-spectre-mitigations-enabled*
            (or spectre-mitigations *x86-64-spectre-mitigations-enabled*))
           (*eh-model* (normalize-x86-64-eh-model (or eh-model *eh-model*)))
           (*zero-cost-eh-enabled* (or *zero-cost-eh-enabled*
                                       (eq (normalize-x86-64-eh-model (or eh-model *eh-model*))
                                           :table))))
      (x86-64-peephole-optimize
       (with-output-to-vector (stream)
         (emit-vm-program allocated-program stream)))))))
