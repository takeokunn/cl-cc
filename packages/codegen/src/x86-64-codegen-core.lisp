(in-package :cl-cc/codegen)

(defparameter *x86-64-host-probe-timeout-seconds* 2
  "Timeout in seconds for external host feature probe commands.")

(defun x86-64-unlikely-branch-prefix-p (inst)
  "Return T when INST should carry the legacy x86 unlikely-branch hint prefix."
  (and (typep inst 'vm-jump-zero)
       (eq (opt-branch-weight inst) :unlikely)))

(defvar *x86-64-branch-encodings* nil
  "Optional EQ hash table mapping branch instruction objects to :SHORT or :NEAR.

When NIL, branch size accounting assumes the first relaxation pass where every
branch is encoded as rel8.")

(defun x86-64-branch-encoding (inst)
  "Return selected x86-64 branch encoding for INST (:SHORT or :NEAR)."
  (or (and *x86-64-branch-encodings*
           (gethash inst *x86-64-branch-encodings*))
      :short))

(defun x86-64-branch-byte-size (inst conditional-p)
  "Return byte size of INST's control-transfer opcode and displacement."
  (ecase (x86-64-branch-encoding inst)
    (:short 2)
    (:near (if conditional-p 6 5))))

(defun x86-64-conditional-branch-size (inst)
  "Return TEST+Jcc size for conditional VM branches, including optional hint prefix."
  (+ 3
     (if (x86-64-unlikely-branch-prefix-p inst) 1 0)
     (x86-64-branch-byte-size inst t)))

(defun x86-64-dynamic-instruction-size (inst)
  "Return INST size, including dynamically relaxed x86-64 branch sizes."
  (case (type-of inst)
    (vm-jump (x86-64-branch-byte-size inst nil))
    (vm-jump-zero (x86-64-conditional-branch-size inst))
    (otherwise (gethash (type-of inst) *x86-64-instruction-sizes*))))

(defun instruction-size (inst)
  "Estimate the size in bytes of the x86-64 encoding for a VM instruction.
   Used in first pass to build label offset table."
  (typecase inst
    (x86-64-lea-address
      (x86-64-lea-address-byte-size inst))
    (x86-64-bextr-field
     16)
    (x86-64-shrink-save
     (push-r64-byte-size (x86-64-shrink-save-reg inst)))
    (x86-64-shrink-restore
     (pop-r64-byte-size (x86-64-shrink-restore-reg inst)))
    (vm-const
     (if (floatp (vm-value inst)) 15 10))
    (vm-move
     (if (or (x86-64-float-vreg-p (vm-dst inst))
             (x86-64-float-vreg-p (vm-src inst)))
         (let ((dst (vm-reg-to-xmm (vm-dst inst)))
               (src (vm-reg-to-xmm (vm-src inst))))
           (if (= dst src) 0 4))
         (let ((dst (vm-reg-to-x86 (vm-dst inst)))
               (src (vm-reg-to-x86 (vm-src inst))))
           (if (= dst src) 0 3))))
    (vm-halt
      (if (x86-64-float-vreg-p (vm-reg inst))
         (let ((result-reg (vm-reg-to-xmm (vm-reg inst))))
           (if (= result-reg +xmm0+) 0 4))
          (let ((result-reg (vm-reg-to-x86 (vm-reg inst))))
            (if (= result-reg +rax+) 0 3))))
    (vm-call
      (+ (if *x86-64-use-retpoline* 44 6)
         (if (and (not *x86-64-use-retpoline*)
                  (x86-64-float-vreg-p (vm-dst inst)))
             1
             0)
         (if *x86-64-cfi-enabled* 28 0)))
    (vm-tail-call
     (+ (if *x86-64-use-retpoline* 20 3)
     (if *x86-64-cfi-enabled* 28 0)))
     (vm-jump-zero
      (x86-64-conditional-branch-size inst))
      ((or vm-spill-store vm-spill-load)
       (let* ((phys (if (typep inst 'vm-spill-store)
                        (vm-spill-src inst)
                        (vm-spill-dst inst)))
              (fp-entry (assoc phys *phys-fp-reg-to-x86-code*))
              (fp-code (cdr fp-entry))
              (offset (x86-64-spill-slot-offset (vm-spill-slot inst)))
             (mod (cond
                   ((and (zerop offset)
                         (/= (logand *current-spill-base-reg* #x7) 5))
                    0)
                   ((typep offset '(signed-byte 8))
                    1)
                   (t
                    2)))
            (sib-p (= (logand *current-spill-base-reg* #x7) 4))
            (addr-size (+ 1
                          (if sib-p 1 0)
                           (ecase mod
                             (0 0)
                             (1 1)
                             (2 4)))))
         (+ (if fp-entry 3 2)
            (if (and fp-entry
                     (or (>= fp-code 8)
                         (>= *current-spill-base-reg* 8)))
                1
                0)
            addr-size)))
      ((or vm-aref vm-aset)
       (let ((index (vm-reg-to-x86 (vm-index-reg inst))))
         (+ (if (= (logand index #x7) 4) 3 0)
            (gethash (type-of inst) *x86-64-instruction-sizes*))))
      (vm-prefetch
       (let* ((base (vm-reg-to-x86 (vm-prefetch-base-reg inst)))
              (index-reg (vm-prefetch-index-reg inst))
              (raw-index (and index-reg (vm-reg-to-x86 index-reg)))
              (index (and raw-index
                          (if (= (logand raw-index #x7) 4) +r11+ raw-index)))
              (offset (vm-prefetch-offset inst))
              (index-fixup-size (if (and raw-index (= (logand raw-index #x7) 4)) 3 0))
              (rex-size (if (or (>= base 8) (and index (>= index 8))) 1 0)))
          (+ index-fixup-size rex-size 2
             (x86-64-memory-address-byte-size base offset :index index))))
      (vm-simd-vector-op
       (x86-64-validate-simd-vector-op inst)
       (let* ((raw-index (vm-reg-to-x86 (vm-simd-vector-op-index-reg inst)))
              (index-fixup-size (if (= (logand raw-index #x7) 4) 3 0))
              (index (if (= (logand raw-index #x7) 4) +r11+ raw-index))
              (element-type (vm-simd-vector-op-element-type inst))
              (addr-size (lambda (base)
                           (x86-64-lea-address-byte-size
                            (make-x86-64-lea-address
                             :dst +r11+ :base base :index index
                             :scale (x86-64-simd-element-scale element-type)
                             :displacement +x86-64-array-data-offset+))))
              (lhs (vm-reg-to-x86 (vm-simd-vector-op-lhs-array inst)))
              (rhs (vm-reg-to-x86 (vm-simd-vector-op-rhs-array inst)))
              (dst (vm-reg-to-x86 (vm-simd-vector-op-dst-array inst)))
              (mov-size 5)
              (op-size (if (= (vm-simd-vector-op-lanes inst) 4)
                           (if (eq (vm-simd-vector-op-op inst) :mul) 5 4)
                           4)))
         (+ index-fixup-size
            (funcall addr-size lhs) mov-size
            (funcall addr-size rhs) mov-size
            op-size
            (funcall addr-size dst) mov-size)))
      (t
     (let ((tp (string-downcase (symbol-name (type-of inst)))))
       (if (member tp
                   '("vm-push-handler"
                     "vm-pop-handler"
                     "vm-bind-restart"
                     "vm-invoke-restart"
                     "vm-signal"
                     "vm-error-instruction"
                     "vm-cerror"
                     "vm-warn"
                     "vm-establish-handler"
                     "vm-remove-handler"
                     "vm-sync-handler-regs"
                     "vm-signal-error"
                     "vm-establish-catch"
                     "vm-throw")
                    :test #'string=)
            (if *x86-64-shadow-stack-enabled* 6 2)
             (or (x86-64-dynamic-instruction-size inst) 0))))))

(defun x86-64-lea-address-byte-size (inst)
  "Return the exact byte size of internal LEA instruction INST."
  (let* ((base (x86-64-lea-address-base inst))
         (index (x86-64-lea-address-index inst))
         (disp (x86-64-lea-address-displacement inst))
         (mod (x86-64-memory-mod base disp))
         (sib-p (or index (= (logand base #x7) 4))))
    (+ 2                         ; REX.W + opcode 8D
       1                         ; ModR/M
       (if sib-p 1 0)
       (ecase mod
         (0 0)
         (1 1)
         (2 4)))))

(defun build-label-offsets (instructions prologue-size)
  "Build a hash table mapping label names to byte offsets.
   First pass: walk instructions, accumulate sizes."
  (let ((offsets (make-hash-table :test #'equal))
        (pos prologue-size))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) offsets) pos))
      (incf pos (instruction-size inst)))
    offsets))

(defun stack-probe-count (frame-size)
  "Return how many guard-page probes are needed for FRAME-SIZE bytes."
  (if (>= frame-size +stack-probe-page-size+)
      (floor frame-size +stack-probe-page-size+)
      0))

(defun x86-64-stack-frame-size (save-regs spill-frame-size)
  "Return conservative stack-frame bytes represented by saves plus allocated spill space."
  (+ (* 8 (length save-regs))
     spill-frame-size))

(defun x86-64-tls-base-register ()
  "Return the selected x86-64 TLS base register from optimizer planning."
  (let ((plan (opt-build-tls-plan :target :x86-64 :hot-access-p t)))
    (opt-tls-plan-base-register plan)))

(defun x86-64-atomic-lowering-plan (operation memory-order)
  "Return x86-64 atomic lowering metadata for OPERATION and MEMORY-ORDER.

Result plist keys:
- :opcode          selected representative opcode keyword
- :pre-fence       list of fence opcodes before atomic op
- :post-fence      list of fence opcodes after atomic op"
  (let* ((plan (opt-build-atomic-plan
                :target :x86-64
                :operation operation
                :memory-order memory-order))
         (pre-fence (case memory-order
                      (:seq-cst '(:mfence))
                      (otherwise nil)))
         (post-fence (case memory-order
                       (:seq-cst '(:mfence))
                       (otherwise nil))))
    (list :opcode (opt-atomic-plan-opcode plan)
          :pre-fence pre-fence
          :post-fence post-fence)))

(defun x86-64-env-true-p (value)
  "Return T when VALUE represents an enabled boolean environment flag." 
  (and value
       (member (string-downcase value)
               '("1" "true" "yes" "on" "enabled")
               :test #'string=)))

(defun x86-64-ibrs-token-present-p (text)
  "Return T when TEXT includes an IBRS/eIBRS capability token.

The detector is intentionally permissive to support varying host feature
formats such as:\n
- Linux `/proc/cpuinfo` flags: `... ibrs ...`\n
- Darwin sysctl strings: `... IBRS ...` or `... eIBRS ...`."
  (and (stringp text)
       (let* ((lower (string-downcase text))
              (len (length lower)))
         (labels ((token-char-p (ch)
                    (or (alpha-char-p ch)
                        (digit-char-p ch)
                        (char= ch #\_)
                        (char= ch #\-)
                        (char= ch #\.)))
                  (matches-token-at-p (i token)
                    (let* ((tlen (length token))
                           (end (+ i tlen)))
                      (and (<= end len)
                           (string= lower token :start1 i :end1 end)
                           (or (= i 0)
                               (not (token-char-p (char lower (1- i)))))
                           (or (= end len)
                               (not (token-char-p (char lower end))))))))
           (loop for i from 0 below len
                  thereis (or (matches-token-at-p i "ibrs")
                              (matches-token-at-p i "eibrs")))))))

(defun x86-64-cpu-token-present-p (text token)
  "Return T when TEXT includes TOKEN as a CPU-feature token."
  (and (stringp text)
       (stringp token)
       (let* ((lower (string-downcase text))
              (needle (string-downcase token))
              (len (length lower)))
         (labels ((token-char-p (ch)
                    (or (alpha-char-p ch)
                        (digit-char-p ch)
                        (char= ch #\_)
                        (char= ch #\-)
                        (char= ch #\.)))
                  (matches-at-p (i)
                    (let ((end (+ i (length needle))))
                      (and (<= end len)
                           (string= lower needle :start1 i :end1 end)
                           (or (= i 0)
                               (not (token-char-p (char lower (1- i)))))
                           (or (= end len)
                               (not (token-char-p (char lower end))))))))
            (loop for i from 0 below len thereis (matches-at-p i))))))

(defun x86-64-host-x86-p ()
  "Return T when the host machine is itself x86/x86-64."
  (let ((machine (string-upcase (or (machine-type) ""))))
    (or (search "X86" machine)
        (search "AMD64" machine)
        (search "X86-64" machine)
        (search "X86_64" machine))))

(defun x86-64-host-cpu-feature-text ()
  "Return best-effort host CPU feature text, or NIL when probing fails."
  (when (x86-64-host-x86-p)
    (or (ignore-errors
          (when (and (find-package :uiop)
                     (fboundp 'uiop:run-program))
            (sb-ext:with-timeout *x86-64-host-probe-timeout-seconds*
              (uiop:run-program
               '("sysctl" "-a")
               :output :string
               :ignore-error-status t))))
        (ignore-errors
          (with-open-file (in "/proc/cpuinfo" :direction :input)
            (let ((buf (make-string-output-stream)))
              (loop for line = (read-line in nil nil)
                    while line
                    do (progn (write-string line buf)
                              (write-char #\Newline buf)))
              (get-output-stream-string buf)))))))

(defun x86-64-host-supports-cpu-feature-p (token)
  "Return T when host CPU feature TOKEN is visible in sysctl/procfs output."
  (and (x86-64-cpu-token-present-p (x86-64-host-cpu-feature-text) token) t))

(defun x86-64-supports-popcnt-p ()
  "Return T when POPCNT support is enabled or detected."
  (or (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_POPCNT")))
      (x86-64-host-supports-cpu-feature-p "popcnt")))

(defun x86-64-supports-bmi1-p ()
  "Return T when BMI1/LZCNT support is enabled or detected."
  (or (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_BMI1")))
      (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_LZCNT")))
      (x86-64-host-supports-cpu-feature-p "bmi1")
      (x86-64-host-supports-cpu-feature-p "lzcnt")))

(defun x86-64-supports-bmi2-p ()
  "Return T when BMI2 support is enabled or detected."
  (or (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_BMI2")))
      (x86-64-host-supports-cpu-feature-p "bmi2")))

(defun x86-64-host-supports-ibrs-p ()
  "Best-effort host capability probe for IBRS/eIBRS.

Priority:
1) Darwin: `sysctl -a` output token scan
2) Linux: `/proc/cpuinfo` token scan

All probe failures are treated as "unknown" => NIL.
Environment variables remain the primary override path."
   (and (x86-64-host-x86-p)
    (or
     ;; macOS / Darwin path
     (ignore-errors
      (when (and (find-package :uiop)
                 (fboundp 'uiop:run-program))
        (let ((out (sb-ext:with-timeout *x86-64-host-probe-timeout-seconds*
                      (uiop:run-program
                      '("sysctl" "-a")
                      :output :string
                      :ignore-error-status t))))
          (and (x86-64-ibrs-token-present-p out) t))))
   ;; Linux path
   (ignore-errors
     (with-open-file (in "/proc/cpuinfo" :direction :input)
       (let ((buf (make-string-output-stream)))
         (loop for line = (read-line in nil nil)
               while line
               do (progn (write-string line buf)
                         (write-char #\Newline buf)))
          (and (x86-64-ibrs-token-present-p (get-output-stream-string buf)) t)))))))

(defun x86-64-supports-ibrs-p ()
  "Return T when runtime feature flags indicate IBRS/eIBRS support.

Environment overrides:
- CLCC_IBRS=1
- CLCC_EIBRS=1"
  (or (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_IBRS")))
      (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_EIBRS")))
      (x86-64-host-supports-ibrs-p)))

(defun x86-64-supports-cet-ss-p ()
  "Return T when runtime feature flags indicate CET Shadow Stack support.

Environment override:
- CLCC_CET_SS=1"
  (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_CET_SS"))))

(defun x86-64-program-has-nonlocal-control-p (instructions)
  "Return T when INSTRUCTIONS include non-local control-flow operations.

This is a conservative detector used for shadow-stack planning."
  (some (lambda (inst)
          (let ((tp (string-downcase (symbol-name (type-of inst)))))
            (member tp
                    '("vm-push-handler"
                      "vm-pop-handler"
                      "vm-bind-restart"
                      "vm-invoke-restart"
                      "vm-signal"
                      "vm-error-instruction"
                      "vm-cerror"
                      "vm-warn"
                      "vm-establish-handler"
                      "vm-remove-handler"
                      "vm-sync-handler-regs"
                      "vm-signal-error"
                      "vm-establish-catch"
                      "vm-throw")
                    :test #'string=)))
        instructions))

(defun x86-64-stack-canary-plan (&key has-stack-buffer-p
                                      (guard-slot -8)
                                      (failure-target 'clcc_stack_chk_fail))
  "Return x86-64 stack-canary emission metadata derived from optimizer planning.

Result plist keys:
- :enabled-p      whether stack protector should be emitted
- :guard-slot     stack slot used for canary shadow copy
- :failure-target branch target used on canary mismatch
- :prologue       backend-neutral prologue sequence
- :epilogue       backend-neutral epilogue sequence"
  (let* ((plan (opt-stack-canary-emit-plan
                :has-stack-buffer-p has-stack-buffer-p
                :guard-slot guard-slot
                :failure-target failure-target))
         (prologue (opt-stack-canary-prologue-seq plan :temp-reg :rax))
         (epilogue (opt-stack-canary-epilogue-seq plan :temp-reg :rax)))
    (list :enabled-p (getf plan :enabled-p)
          :guard-slot (getf plan :guard-slot)
          :failure-target (getf plan :failure-target)
          :prologue prologue
          :epilogue epilogue)))

(defun x86-64-stack-buffer-inst-p (inst)
  "Conservative predicate for VM instructions that imply stack-buffer risk."
  (let ((tp (string-downcase (symbol-name (type-of inst)))))
    (member tp
            '("vm-make-array"
              "vm-aset"
              "vm-aref-multi"
              "vm-adjust-array"
              "vm-vector-push"
              "vm-vector-push-extend"
              "vm-set-fill-pointer"
              "vm-fill"
              "vm-row-major-aref"
              "vm-array-row-major-index"
              "vm-svset"
              "vm-svref")
            :test #'string=)))

(defun x86-64-program-has-stack-buffer-p (instructions)
  "Return T when INSTRUCTIONS include buffer-like array/vector operations."
  (some #'x86-64-stack-buffer-inst-p instructions))

(defun x86-64-cfi-plan (&key has-indirect-calls-p)
  "Return x86-64 CFI entry planning metadata.

Result plist keys:
- :enabled-p whether CFI entry marker is enabled
- :entry-opcode marker opcode keyword (`:endbr64` or `:none`)"
  (let* ((plan (opt-build-cfi-plan
                :target :x86-64
                :has-indirect-calls-p has-indirect-calls-p))
         (entry-opcode (opt-cfi-entry-opcode plan)))
    (list :enabled-p (eq entry-opcode :endbr64)
          :entry-opcode entry-opcode)))

(defun push-r64-byte-size (reg)
  "Return byte count for PUSH reg: 2 for R8-R15, 1 for RAX-RDI."
  (if (>= reg 8) 2 1))

(defun pop-r64-byte-size (reg)
  "Return byte count for POP reg: 2 for R8-R15, 1 for RAX-RDI."
  (if (>= reg 8) 2 1))
