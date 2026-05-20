;;;; packages/emit/src/aarch64-emitters.lisp - AArch64 VM Instruction Emitters
;;;;
;;;; Per-VM-instruction emit-a64-vm-* functions that translate individual
;;;; VM IR instructions into AArch64 machine code via the encode-* layer.
;;;; Includes: define-a64-binary-emitter / define-a64-csel-emitter macros,
;;;; emit-a64-vm-{const,move,select,bswap,rotate,halt,jump,jump-zero,
;;;; spill-store,spill-load,ret} and all arithmetic/comparison/logic emitters.
;;;;
;;;; Register mapping, instruction encoders, and the label-offset builder are
;;;; in aarch64-codegen.lisp (loads before this file).
;;;;
;;;; Load order: after aarch64-codegen.lisp, before aarch64-program.lisp.

(in-package :cl-cc/codegen)

(defmacro define-a64-binary-emitter (fn-name encode-fn)
  "Define an AArch64 binary VM instruction emitter using ENCODE-FN(rd rn rm)."
  `(defun ,fn-name (inst stream)
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-lhs inst)))
           (rm (a64-reg (vm-rhs inst))))
       (emit-a64-instr (,encode-fn rd rn rm) stream))))

(defmacro define-a64-csel-emitter (fn-name description cond-code)
  "Define an AArch64 CSEL-based min/max emitter with COND-CODE."
  `(defun ,fn-name (inst stream)
     ,description
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-lhs inst)))
           (rm (a64-reg (vm-rhs inst))))
        (emit-a64-instr (encode-cmp rn rm) stream)
         (emit-a64-instr (encode-csel rd rn rm ,cond-code) stream))))

(defparameter *current-a64-literal-pool* nil
  "Literal-pool plan active during AArch64 machine-code emission.")

(defmacro define-a64-unary-emitter (fn-name encode-fn)
  "Define an AArch64 unary VM instruction emitter using ENCODE-FN(rd rn)."
  `(defun ,fn-name (inst stream)
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-src inst))))
       (emit-a64-instr (,encode-fn rd rn) stream))))

(defun emit-a64-vm-const (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (value (vm-value inst)))
    ;; Handle negative values via logical masking to 64-bit
    (emit-a64-mov-imm64 rd (logand value #xFFFFFFFFFFFFFFFF) stream)))

(defun emit-a64-vm-const-at (inst stream current-pos)
  "Emit VM-CONST at CURRENT-POS, using the active literal pool when profitable."
  (let* ((rd (a64-reg (vm-dst inst)))
         (value (logand (vm-value inst) #xFFFFFFFFFFFFFFFF)))
    (if (and *current-a64-literal-pool* (a64-literal-pool-value-p value))
        (aarch64-emit-literal-reference rd value current-pos stream
                                        *current-a64-literal-pool*)
        (emit-a64-mov-imm64 rd value stream))))

(defun emit-a64-vm-move (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-src inst))))
    (unless (= rd rn)
      (emit-a64-instr (encode-mov-rr rd rn) stream))))

(define-a64-binary-emitter emit-a64-vm-add encode-add)
(define-a64-binary-emitter emit-a64-vm-sub encode-sub)
(define-a64-binary-emitter emit-a64-vm-mul encode-mul)
(define-a64-binary-emitter emit-a64-vm-truncate encode-sdiv)
(define-a64-binary-emitter emit-a64-vm-integer-mul-high-u encode-umulh)
(define-a64-binary-emitter emit-a64-vm-integer-mul-high-s encode-smulh)
(define-a64-binary-emitter emit-a64-vm-float-add encode-fadd)
(define-a64-binary-emitter emit-a64-vm-float-sub encode-fsub)
(define-a64-binary-emitter emit-a64-vm-float-mul encode-fmul)
(define-a64-binary-emitter emit-a64-vm-float-div encode-fdiv)

(defun emit-a64-vm-fma (inst stream)
  "Emit FMADD Dd, Dn, Dm, Da for vm-fma."
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-a inst)))
        (rm (a64-reg (vm-b inst)))
        (ra (a64-reg (vm-c inst))))
    (emit-a64-instr (encode-fmadd rd rn rm ra) stream)))

;;; Checked arithmetic emitters (FR-303 overflow detection)
;;;
;;; AArch64 pattern: ADDS/SUBS rd,rn,rm + B.cond VC(cond=7) #2 + BRK #1
;;; ADDS/SUBS set the V (overflow) flag in NZCV.
;;; B.cond VC (overflow clear, cond=7) branches past BRK when no overflow.
;;; BRK #1 traps on overflow.
;;; Total: 3 instructions = 12 bytes for add-checked and sub-checked.

(defun emit-a64-vm-add-checked (inst stream)
  "vm-add-checked: dst = lhs + rhs with hardware overflow trap (FR-303).
   ADDS sets V on overflow; B.cond VC skips BRK; BRK traps."
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-adds rd rn rm) stream)
    (emit-a64-instr (encode-b-cond 2 7) stream)     ; B.VC #2 — skip BRK if no overflow
    (emit-a64-instr (encode-brk 1) stream)))          ; BRK #1 — overflow trap

(defun emit-a64-vm-sub-checked (inst stream)
  "vm-sub-checked: dst = lhs - rhs with hardware overflow trap (FR-303).
   SUBS sets V on overflow; B.cond VC skips BRK; BRK traps."
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-subs rd rn rm) stream)
    (emit-a64-instr (encode-b-cond 2 7) stream)     ; B.VC #2 — skip BRK if no overflow
    (emit-a64-instr (encode-brk 1) stream)))          ; BRK #1 — overflow trap

(defun emit-a64-vm-mul-checked (inst stream)
  "vm-mul-checked: dst = lhs * rhs with overflow trap (FR-303).
   MUL does not set flags on AArch64. Uses SMULH to detect overflow:
   MUL rd + SMULH X16 + ASR X17,rd,#63 + CMP X16,X17 + B.EQ + BRK.
   Uses X16 (IP0) and X17 (IP1) as temporary registers.
   6 instructions = 24 bytes."
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst)))
        (tmp 16)                                       ; X16 = IP0 scratch
        (sext 17))                                      ; X17 = IP1 scratch
    (emit-a64-instr (encode-mul rd rn rm) stream)       ; rd = low 64 bits
    (emit-a64-instr (encode-smulh tmp rn rm) stream)    ; tmp = high 64 bits
    (emit-a64-instr (encode-asr sext rd 63) stream)     ; sext = sign_extend(rd) = 0 or -1
    (emit-a64-instr (encode-cmp tmp sext) stream)       ; CMP high, expected
    (emit-a64-instr (encode-b-cond 1 0) stream)         ; B.EQ #1 — skip if no overflow
    (emit-a64-instr (encode-brk 1) stream)))             ; BRK #1 — overflow trap
(define-a64-unary-emitter emit-a64-vm-sqrt encode-fsqrt)

;;; Libm call emitters for sin/cos/exp/log (FR-286)
;;;
;;; AArch64 libm call sequence (worst-case 36 bytes = 9 instructions):
;;;   FMOV D0, Dsrc          — move float arg to AAPCS64 D0
;;;   STP X30, X31, [SP,#-16]! — save LR (clobbered by BLR)
;;;   MOVZ/MOVK X16, #addr   — load 64-bit libm address (up to 4 instrs)
;;;   BLR X16                — indirect call
;;;   LDP X30, X31, [SP], #16 — restore LR
;;;   FMOV Ddst, D0          — move float result from D0

(defun a64-libm-address (libm-fn)
  "Return the process address of LIBM-FN as an unsigned 64-bit value."
  (cond
    ((string= libm-fn "sin")
     (load-time-value (logand (sb-sys:sap-int (sb-alien:alien-sap
                                               (sb-alien:extern-alien "sin" (function double-float double-float))))
                              #xFFFFFFFFFFFFFFFF)))
    ((string= libm-fn "cos")
     (load-time-value (logand (sb-sys:sap-int (sb-alien:alien-sap
                                               (sb-alien:extern-alien "cos" (function double-float double-float))))
                              #xFFFFFFFFFFFFFFFF)))
    ((string= libm-fn "exp")
     (load-time-value (logand (sb-sys:sap-int (sb-alien:alien-sap
                                               (sb-alien:extern-alien "exp" (function double-float double-float))))
                              #xFFFFFFFFFFFFFFFF)))
    ((string= libm-fn "log")
     (load-time-value (logand (sb-sys:sap-int (sb-alien:alien-sap
                                               (sb-alien:extern-alien "log" (function double-float double-float))))
                              #xFFFFFFFFFFFFFFFF)))
    ((string= libm-fn "tan")
     (load-time-value (logand (sb-sys:sap-int (sb-alien:alien-sap
                                               (sb-alien:extern-alien "tan" (function double-float double-float))))
                              #xFFFFFFFFFFFFFFFF)))
    ((string= libm-fn "asin")
     (load-time-value (logand (sb-sys:sap-int (sb-alien:alien-sap
                                               (sb-alien:extern-alien "asin" (function double-float double-float))))
                              #xFFFFFFFFFFFFFFFF)))
    ((string= libm-fn "acos")
     (load-time-value (logand (sb-sys:sap-int (sb-alien:alien-sap
                                               (sb-alien:extern-alien "acos" (function double-float double-float))))
                              #xFFFFFFFFFFFFFFFF)))
    ((string= libm-fn "atan")
     (load-time-value (logand (sb-sys:sap-int (sb-alien:alien-sap
                                               (sb-alien:extern-alien "atan" (function double-float double-float))))
                              #xFFFFFFFFFFFFFFFF)))
    (t (error "Unsupported AArch64 libm function: ~A" libm-fn))))

(defmacro define-a64-float-libm-unary-emitter (fn-name libm-fn)
  "Define an AArch64 emitter that calls libm function LIBM-FN via BLR for a VM unary float instruction."
  `(defun ,fn-name (inst stream &optional current-pos)
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-src inst))))
       ;; FMOV D0, Dsrc — move arg to AAPCS64 float arg register
       (emit-a64-instr (encode-fmov-dd 0 rn) stream)
       ;; STP X30, X31(ZR), [SP, #-16]! — save LR on stack
       (emit-a64-instr (encode-stp-pre +a64-lr+ +a64-zr+ +a64-sp+ -2) stream)
       ;; ADRP/LDR X16, literal — load libm function address through the pool
       ;; when program-level emission has planned one; keep MOV fallback for
       ;; direct unit use of this emitter.
       (let ((addr (a64-libm-address ,libm-fn)))
         (if (and *current-a64-literal-pool* current-pos)
             (aarch64-emit-literal-reference 16 addr current-pos stream
                                             *current-a64-literal-pool*)
             (emit-a64-mov-imm64 16 addr stream)))
       ;; BLR X16 — call libm
       (emit-a64-instr (encode-blr 16) stream)
       ;; LDP X30, X31(ZR), [SP], #16 — restore LR
       (emit-a64-instr (encode-ldp-post +a64-lr+ +a64-zr+ +a64-sp+ 2) stream)
       ;; FMOV Ddst, D0 — move result from AAPCS64 return register
       (emit-a64-instr (encode-fmov-dd rd 0) stream))))

(define-a64-float-libm-unary-emitter emit-a64-vm-sin "sin")
(define-a64-float-libm-unary-emitter emit-a64-vm-cos "cos")
(define-a64-float-libm-unary-emitter emit-a64-vm-exp "exp")
(define-a64-float-libm-unary-emitter emit-a64-vm-log "log")
(define-a64-float-libm-unary-emitter emit-a64-vm-tan "tan")
(define-a64-float-libm-unary-emitter emit-a64-vm-asin "asin")
(define-a64-float-libm-unary-emitter emit-a64-vm-acos "acos")
(define-a64-float-libm-unary-emitter emit-a64-vm-atan "atan")
(define-a64-csel-emitter emit-a64-vm-min "vm-min: dst = min(lhs, rhs) — signed, branchless via CSEL LT." 11)
(define-a64-csel-emitter emit-a64-vm-max "vm-max: dst = max(lhs, rhs) — signed, branchless via CSEL GT." 12)

(defun emit-a64-vm-select (inst stream)
  "vm-select: dst = cond ? then : else  (branchless via CSEL NE)."
  (let ((rd (a64-reg (vm-dst inst)))
        (cond (a64-reg (vm-select-cond-reg inst)))
        (then (a64-reg (vm-select-then-reg inst)))
        (else (a64-reg (vm-select-else-reg inst))))
    (emit-a64-instr (encode-mov-rr rd else) stream)
    (emit-a64-instr (encode-cmp cond 31) stream)
    (emit-a64-instr (encode-csel rd then rd 1) stream)))

(defun emit-a64-vm-bswap (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-src inst))))
    (emit-a64-instr (encode-rev32 rd rn) stream)))

(defun emit-a64-vm-rotate (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-mov-rr rd rn) stream)
    (emit-a64-instr (encode-rorv rd rd rm) stream)))

(defun emit-a64-vm-halt (inst stream)
  "Move result to X0 (return register). Always emits exactly one MOV instruction."
  (let ((result-reg (a64-reg (vm-reg inst))))
    (emit-a64-instr (encode-mov-rr 0 result-reg) stream)))

(defun emit-a64-vm-jump (inst stream current-pos label-offsets)
  "Emit B #imm26 for unconditional jump."
  (let* ((target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         ;; offset in instruction units: (target_byte_offset - current_byte_offset) / 4
         (byte-offset (- target-pos current-pos))
         (imm26 (ash byte-offset -2)))
    (emit-a64-instr (encode-b imm26) stream)))

(defun emit-a64-vm-jump-zero (inst stream current-pos label-offsets)
  "Emit CBZ Xn, #imm19 for conditional branch on zero."
  (let* ((rn (a64-reg (vm-reg inst)))
         (target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         (byte-offset (- target-pos current-pos))
         (imm19 (ash byte-offset -2)))
    (emit-a64-instr (encode-cbz rn imm19) stream)))

(defun emit-a64-vm-spill-store (inst stream)
  "STUR Xs, [BASE, OFFSET] using the active spill base register and slot bias."
  (let* ((src-phys (vm-spill-src inst))
         (src-num (cdr (assoc src-phys *aarch64-reg-number*)))
         (offset (a64-spill-slot-offset (vm-spill-slot inst))))
    (emit-a64-instr (encode-stur src-num *current-a64-spill-base-reg* offset) stream)))

(defun emit-a64-vm-spill-load (inst stream)
  "LDUR Xd, [BASE, OFFSET] using the active spill base register and slot bias."
  (let* ((dst-phys (vm-spill-dst inst))
         (dst-num (cdr (assoc dst-phys *aarch64-reg-number*)))
            (offset (a64-spill-slot-offset (vm-spill-slot inst))))
    (emit-a64-instr (encode-ldur dst-num *current-a64-spill-base-reg* offset) stream)))

(defun emit-a64-vm-prefetch (inst stream)
  "Emit PRFM PLDL1KEEP/PLDL1STRM, [Xn,#offset] for VM-PREFETCH."
  (let* ((rn (a64-reg (vm-prefetch-base-reg inst)))
         (index-reg (vm-prefetch-index-reg inst))
         (offset (vm-prefetch-offset inst))
         (rt (ecase (vm-prefetch-locality inst)
               ((:t0 :keep) 0)   ; PLDL1KEEP
               ((:nta :strm) 1)))) ; PLDL1STRM
    (when index-reg
      (unless (= (vm-prefetch-scale inst) 8)
        (error "AArch64 indexed VM-PREFETCH only supports scale 8, got ~D" (vm-prefetch-scale inst)))
      ;; Materialize BASE + INDEX*8 into X16 (IP0), then prefetch [X16,#offset].
      (emit-a64-instr (encode-add-shift +a64-stack-probe-scratch+ rn (a64-reg index-reg) 0 3) stream)
      (setf rn +a64-stack-probe-scratch+))
    (unless (and (not (minusp offset)) (zerop (mod offset 8)) (<= offset (* #xFFF 8)))
      (error "AArch64 PRFM offset must be an unsigned multiple of 8 within imm12 range: ~D" offset))
    (emit-a64-instr (encode-prfm rt rn (ash offset -3)) stream)))

(defun emit-a64-vm-ret (inst stream)
  "Emit inline epilogue (restore all callee-saved registers) then RET.
    Must stay in sync with emit-a64-prologue and a64-instruction-size for vm-ret."
  (declare (ignore inst))
  (dolist (pair (reverse (or *current-a64-epilogue-save-pairs*
                            (a64-used-callee-saved-pairs *current-a64-regalloc*))))
    (destructuring-bind (rn rm) pair
      (emit-a64-instr (encode-ldp-post rn rm +a64-sp+ 2) stream)))
  (emit-a64-instr +a64-ret+ stream))

;;; FR-268: AArch64 Constant Islands / Literal Pools
;;;
;;; ADR/ADRP + LDR for PC-relative constant pool loading.  Large immediates
;;; (beyond MOVZ/MOVK 16-bit range) are loaded from a literal pool placed
;;; at the end of the function.

(defun encode-adr (rd offset)
  "ADR Rd, #offset  — PC-relative address (1 MiB range, page-relative).
   Encoding: 0xx10000 iiiii iiiiii iiiii iiiii xxxxx"
  (let ((imm (logand offset #x1FFFFF)))
    (logior (ash (ldb (byte 2 0) imm) 29) ; immlo[1:0]
          #x10000000
            (ash (ldb (byte 19 2) imm) 5)  ; immhi[20:2]
            (ash rd 0))))

(defun encode-adrp (rd page-delta)
  "ADRP Rd, #PAGE-DELTA — PC-relative page address in 4 KiB pages."
  (let ((imm (logand page-delta #x1FFFFF)))
    (logior #x90000000
            (ash (ldb (byte 2 0) imm) 29)
            (ash (ldb (byte 19 2) imm) 5)
            rd)))

(defun encode-ldr-unsigned64 (rt rn byte-offset)
  "LDR Xt, [Xn, #BYTE-OFFSET] using unsigned scaled imm12 encoding."
  (unless (and (not (minusp byte-offset))
               (zerop (mod byte-offset 8))
               (<= byte-offset (* #xFFF 8)))
    (error "AArch64 LDR unsigned offset must be an unsigned multiple of 8 within imm12 range: ~D"
           byte-offset))
  (logior #xF9400000
          (ash (ash byte-offset -3) 10)
          (ash rn 5)
          rt))

(defun encode-ldr-literal (rt offset)
  "LDR Rt, [PC, #offset]  — load from literal pool (word, scaled by 4).
   Encoding: 0xx11000 iiiii iiiiii iiiii iiiii xxxxx"
  (declare (type fixnum rt offset))
  (let ((imm19 (ash offset -2)))  ; 19-bit signed immediate / 4
    (logior #x18000000
            (ash (ldb (byte 19 0) (logand imm19 #x7FFFF)) 5)
            (ash rt 0))))

;;; FR-268: Literal pool builder
;;;
;;; Accumulates 64-bit constants and emits them at function end.  The pool is
;;; placed after the RET instruction, aligned to 8 bytes.

(defconstant +a64-literal-island-threshold+ (* 28 1024)
  "Preferred maximum code bytes between AArch64 literal references and islands.")

(defstruct a64-literal-entry
  "One 64-bit literal-pool entry."
  label
  value
  (offset 0 :type integer))

(defstruct a64-literal-pool
  "Planned 64-bit constant island/literal-pool entries for one function."
  (entries nil :type list)
  (value-labels (make-hash-table :test #'eql))
  (base-offset 0 :type integer)
  (islands nil :type list))

(defun a64-literal-pool-value-p (value)
  "Return true when VALUE is better loaded via literal pool than MOVZ/MOVK."
  (> (a64-imm64-size value) 1))

(defun a64-pool-align-offset (offset)
  "Return OFFSET rounded up to the next 8-byte boundary."
  (+ offset (mod (- 8 (mod offset 8)) 8)))

(defun a64-pool-add (pool value)
  "Add VALUE to POOL if absent and return its literal label."
  (let* ((u64 (logand value #xFFFFFFFFFFFFFFFF))
         (labels (a64-literal-pool-value-labels pool))
         (existing (gethash u64 labels)))
    (or existing
        (let ((label (gensym "A64-LIT-")))
          (push (make-a64-literal-entry :label label :value u64)
                (a64-literal-pool-entries pool))
          (setf (gethash u64 labels) label)
          label))))

(defun a64-pool-entry-for-value (pool value)
  "Return the pool entry for VALUE, or signal if it was not planned."
  (let ((label (gethash (logand value #xFFFFFFFFFFFFFFFF)
                        (a64-literal-pool-value-labels pool))))
    (or (find label (a64-literal-pool-entries pool)
              :key #'a64-literal-entry-label)
        (error "AArch64 literal value was not planned: ~X" value))))

(defun a64-finalize-literal-pool (pool base-offset)
  "Assign final byte offsets to POOL entries starting at aligned BASE-OFFSET."
  (let ((pos (a64-pool-align-offset base-offset)))
    (setf (a64-literal-pool-base-offset pool) pos)
    (dolist (entry (reverse (a64-literal-pool-entries pool)) pool)
      (setf (a64-literal-entry-offset entry) pos)
      (incf pos 8))))

(defun a64-finalize-literal-pool-islands (pool base-offset island-offsets)
  "Assign final pool and intermediate constant-island byte offsets."
  (a64-finalize-literal-pool pool base-offset)
  (setf (a64-literal-pool-islands pool) island-offsets)
  pool)

(defun a64-literal-entry-index (pool entry)
  "Return ENTRY's stable index within POOL emission order."
  (position entry (reverse (a64-literal-pool-entries pool)) :test #'eq))

(defun a64-literal-offset-for-reference (pool entry current-pos)
  "Return nearest literal byte offset for ENTRY relative to CURRENT-POS."
  (let* ((index (a64-literal-entry-index pool entry))
         (candidate-bases (cons (a64-literal-pool-base-offset pool)
                                (a64-literal-pool-islands pool)))
         (candidates (mapcar (lambda (base) (+ base (* index 8))) candidate-bases)))
    (or (first (sort candidates #'< :key (lambda (offset) (abs (- offset current-pos)))))
        (a64-literal-entry-offset entry))))

(defun a64-page (offset)
  "Return the 4 KiB page number containing byte OFFSET."
  (floor offset 4096))

(defun a64-page-offset (offset)
  "Return the byte offset within OFFSET's 4 KiB page."
  (mod offset 4096))

(defun aarch64-emit-literal-reference (rd value current-pos stream pool)
  "Emit ADRP+LDR that loads VALUE from POOL into RD at CURRENT-POS."
  (let* ((entry (a64-pool-entry-for-value pool value))
         (literal-pos (a64-literal-offset-for-reference pool entry current-pos))
         (page-delta (- (a64-page literal-pos) (a64-page current-pos)))
         (pageoff (a64-page-offset literal-pos)))
    (unless (<= (- (ash 1 20)) page-delta (1- (ash 1 20)))
      (error "AArch64 literal pool entry out of ADRP range: ~D pages" page-delta))
    (emit-a64-instr (encode-adrp rd page-delta) stream)
    (emit-a64-instr (encode-ldr-unsigned64 rd rd pageoff) stream)))

(defun aarch64-emit-literal-pool (entries stream)
  "Emit aligned 8-byte literal-pool ENTRIES as (LABEL VALUE) or entry structs."
  (loop repeat (/ (mod (- 8 (mod (a64-literal-pool-base-offset
                                  *current-a64-literal-pool*)
                                 8))
                     8)
                  4)
        do (emit-a64-instr 0 stream))
  (dolist (entry entries)
    (let ((value (etypecase entry
                   (a64-literal-entry (a64-literal-entry-value entry))
                   (cons (second entry)))))
      (emit-a64-instr (logand value #xFFFFFFFF) stream)
      (emit-a64-instr (logand (ash value -32) #xFFFFFFFF) stream))))

(defun a64-pool-emit (pool stream)
  "Emit all planned POOL entries as an aligned 8-byte literal pool."
  (when (a64-literal-pool-entries pool)
    (aarch64-emit-literal-pool (reverse (a64-literal-pool-entries pool)) stream)))

(defun a64-emit-constant-island (pool stream current-pos)
  "Emit a branch-around constant island for POOL at CURRENT-POS."
  (let* ((data-pos (a64-pool-align-offset (+ current-pos 4)))
         (pad (- data-pos (+ current-pos 4)))
         (data-size (* 8 (length (a64-literal-pool-entries pool))))
         (total-size (+ 4 pad data-size)))
    (emit-a64-instr (encode-b (ash total-size -2)) stream)
    (loop repeat (/ pad 4) do (emit-a64-instr 0 stream))
    (aarch64-emit-literal-pool (reverse (a64-literal-pool-entries pool)) stream)
    total-size))
