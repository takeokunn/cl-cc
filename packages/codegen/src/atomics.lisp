;;;; packages/codegen/src/atomics.lisp — Native atomic lowering helpers (FR-631)

(in-package :cl-cc/codegen)

(defun codegen-memory-order (order)
  "Normalize C11 memory order keywords for backend lowering."
  (case order
    ((nil :seq-cst) :seq-cst)
    ((:acquire :release :relaxed) order)
    ((:acq-rel) :seq-cst)
    (otherwise (error "Invalid atomic memory order: ~S" order))))

(defun vm-atomic-memory-order* (inst)
  "Return INST's memory order when present; VM atomics default to :SEQ-CST."
  (declare (ignore inst))
  :seq-cst)

(defun x86-64-atomic-fence-byte-size (order)
  "Return x86-64 fence byte count for ORDER under TSO."
  (case (codegen-memory-order order)
    (:seq-cst 6)      ; MFENCE before and after explicit barriers/ops where used.
    (otherwise 0)))

(defun x86-64-atomic-op-byte-size (inst op-size &key result-move-p expected-move-p)
  "Return byte size for an x86-64 locked atomic instruction and surrounding fences."
  (+ (case (codegen-memory-order (vm-atomic-memory-order* inst))
       (:seq-cst 6)
       (otherwise 0))
     (if expected-move-p 3 0)
     op-size
     (if result-move-p 3 0)))

(defun emit-x86-64-mfence (stream)
  "Emit MFENCE."
  (emit-byte #x0F stream) (emit-byte #xAE stream) (emit-byte #xF0 stream))

(defun emit-x86-64-sfence (stream)
  "Emit SFENCE."
  (emit-byte #x0F stream) (emit-byte #xAE stream) (emit-byte #xF8 stream))

(defun emit-x86-64-fence-for-order (order stream &key (position :both))
  "Emit x86-64 fences for C11 ORDER. Acquire/release are no-ops on TSO."
  (declare (ignore position))
  (when (eq (codegen-memory-order order) :seq-cst)
    (emit-x86-64-mfence stream)))

(defun emit-x86-64-lock-cmpxchg-m64-r64 (addr-reg src-reg stream)
  "Emit LOCK CMPXCHG [ADDR-REG], SRC-REG. Expected value is in RAX."
  (emit-byte #xF0 stream)
  (emit-byte (rex-prefix :w 1 :r (ash src-reg -3) :b (ash addr-reg -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xB1 stream)
  (%emit-modrm-address (x86-64-memory-mod addr-reg 0) src-reg addr-reg 0 stream))

(defun emit-x86-64-xchg-m64-r64 (addr-reg src-reg stream)
  "Emit XCHG [ADDR-REG], SRC-REG. Memory XCHG is implicitly locked on x86-64."
  (emit-byte (rex-prefix :w 1 :r (ash src-reg -3) :b (ash addr-reg -3)) stream)
  (emit-byte #x87 stream)
  (%emit-modrm-address (x86-64-memory-mod addr-reg 0) src-reg addr-reg 0 stream))

(defun emit-x86-64-lock-xadd-m64-r64 (addr-reg src-reg stream)
  "Emit LOCK XADD [ADDR-REG], SRC-REG. SRC-REG receives the old memory value."
  (emit-byte #xF0 stream)
  (emit-byte (rex-prefix :w 1 :r (ash src-reg -3) :b (ash addr-reg -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xC1 stream)
  (%emit-modrm-address (x86-64-memory-mod addr-reg 0) src-reg addr-reg 0 stream))

(defun emit-x86-64-atomic-cas (inst stream)
  "Lower VM atomic CAS to LOCK CMPXCHG."
  (let ((addr (vm-reg-to-x86 (vm-acas-addr inst)))
        (expected (vm-reg-to-x86 (vm-acas-expected inst)))
        (newval (vm-reg-to-x86 (vm-acas-newval inst)))
        (dst (vm-reg-to-x86 (vm-dst inst)))
        (order (vm-atomic-memory-order* inst)))
    (emit-x86-64-fence-for-order order stream :position :before)
    (emit-mov-rr64 +rax+ expected stream)
    (emit-x86-64-lock-cmpxchg-m64-r64 addr newval stream)
    (emit-mov-rr64 dst +rax+ stream)
    (emit-x86-64-fence-for-order order stream :position :after)))

(defun emit-x86-64-atomic-swap (inst stream)
  "Lower VM atomic swap to locked XCHG."
  (let ((addr (vm-reg-to-x86 (vm-aswap-addr inst)))
        (newval (vm-reg-to-x86 (vm-aswap-newval inst)))
        (dst (vm-reg-to-x86 (vm-dst inst)))
        (order (vm-atomic-memory-order* inst)))
    (emit-x86-64-fence-for-order order stream :position :before)
    (emit-mov-rr64 dst newval stream)
    (emit-x86-64-xchg-m64-r64 addr dst stream)
    (emit-x86-64-fence-for-order order stream :position :after)))

(defun emit-x86-64-atomic-fetch-add (inst stream)
  "Lower VM atomic fetch-add to LOCK XADD."
  (let ((addr (vm-reg-to-x86 (vm-aincf-addr inst)))
        (delta (vm-reg-to-x86 (vm-aincf-delta inst)))
        (dst (vm-reg-to-x86 (vm-dst inst)))
        (order (vm-atomic-memory-order* inst)))
    (emit-x86-64-fence-for-order order stream :position :before)
    (emit-mov-rr64 dst delta stream)
    (emit-x86-64-lock-xadd-m64-r64 addr dst stream)
    (emit-x86-64-fence-for-order order stream :position :after)))

(defun emit-x86-64-memory-barrier (inst stream)
  "Lower VM memory barrier to x86-64 LFENCE/SFENCE/MFENCE as appropriate."
  (case (codegen-memory-order (vm-atomic-memory-order* inst))
    (:seq-cst (emit-x86-64-mfence stream))
    (:acquire (emit-x86-64-lfence stream))
    (:release (emit-x86-64-sfence stream))
    (:relaxed nil)))

(defun encode-a64-dmb-ish () #xD5033BBF)
(defun encode-a64-ldxr (rt rn) (logior #xC85F7C00 (ash (logand rn #x1F) 5) (logand rt #x1F)))
(defun encode-a64-stxr (rs rt rn) (logior #xC8007C00 (logand rs #x1F) (ash (logand rn #x1F) 5) (ash (logand rt #x1F) 16)))
(defun encode-a64-cbnz (rn imm19) (logior #xB5000000 (ash (logand imm19 #x7FFFF) 5) (logand rn #x1F)))
(defun encode-a64-casal (rs rt rn) (logior #xC8E0FC00 (ash (logand rs #x1F) 16) (ash (logand rn #x1F) 5) (logand rt #x1F)))

(defun emit-a64-dmb-for-order (order stream)
  "Emit AArch64 DMB ISH for non-relaxed ordering."
  (unless (eq (codegen-memory-order order) :relaxed)
    (emit-a64-instr (encode-a64-dmb-ish) stream)))

(defun emit-a64-atomic-cas (inst stream)
  "Lower VM CAS to ARMv8.1 CASAL."
  (let ((addr (a64-reg (vm-acas-addr inst)))
        (expected (a64-reg (vm-acas-expected inst)))
        (newval (a64-reg (vm-acas-newval inst)))
        (dst (a64-reg (vm-dst inst)))
        (order (vm-atomic-memory-order* inst)))
    (emit-a64-dmb-for-order order stream)
    (emit-a64-instr (encode-mov-rr dst expected) stream)
    (emit-a64-instr (encode-a64-casal newval dst addr) stream)
    (emit-a64-dmb-for-order order stream)))

(defun emit-a64-atomic-swap (inst stream)
  "Lower VM atomic swap to an LDXR/STXR retry loop."
  (let ((addr (a64-reg (vm-aswap-addr inst)))
        (newval (a64-reg (vm-aswap-newval inst)))
        (dst (a64-reg (vm-dst inst)))
        (order (vm-atomic-memory-order* inst)))
    (emit-a64-dmb-for-order order stream)
    (emit-a64-instr (encode-a64-ldxr dst addr) stream)
    (emit-a64-instr (encode-a64-stxr +a64-stack-probe-scratch+ newval addr) stream)
    (emit-a64-instr (encode-a64-cbnz +a64-stack-probe-scratch+ -2) stream)
    (emit-a64-dmb-for-order order stream)))

(defun emit-a64-atomic-fetch-add (inst stream)
  "Lower VM fetch-add to an LDXR/STXR retry loop."
  (let ((addr (a64-reg (vm-aincf-addr inst)))
        (delta (a64-reg (vm-aincf-delta inst)))
        (dst (a64-reg (vm-dst inst)))
        (order (vm-atomic-memory-order* inst)))
    (emit-a64-dmb-for-order order stream)
    (emit-a64-instr (encode-a64-ldxr dst addr) stream)
    (emit-a64-instr (encode-add +a64-scs-tmp+ dst delta) stream)
    (emit-a64-instr (encode-a64-stxr +a64-stack-probe-scratch+ +a64-scs-tmp+ addr) stream)
    (emit-a64-instr (encode-a64-cbnz +a64-stack-probe-scratch+ -3) stream)
    (emit-a64-dmb-for-order order stream)))

(defun emit-a64-memory-barrier (inst stream)
  "Lower VM memory barrier to DMB ISH except for relaxed barriers."
  (emit-a64-dmb-for-order (vm-atomic-memory-order* inst) stream))

(defun emit-x86-64-atomic-load (inst stream)
  "Lower VM atomic load to MOV plus an acquire/seq-cst fence."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (addr (vm-reg-to-x86 (vm-aload-addr inst))))
    (emit-mov-rm64 dst addr 0 stream)
    (emit-x86-64-fence-for-order (vm-atomic-memory-order* inst) stream)))

(defun emit-x86-64-atomic-store (inst stream)
  "Lower VM atomic store to an ordered MOV."
  (let ((addr (vm-reg-to-x86 (vm-astore-addr inst)))
        (val (vm-reg-to-x86 (vm-astore-val inst))))
    (emit-x86-64-fence-for-order (vm-atomic-memory-order* inst) stream)
    (emit-mov-mr64 addr 0 val stream)))

(defun emit-a64-atomic-load (inst stream)
  "Lower VM atomic load to LDXR plus DMB ordering."
  (emit-a64-dmb-for-order (vm-atomic-memory-order* inst) stream)
  (emit-a64-instr (encode-a64-ldxr (a64-reg (vm-dst inst))
                                   (a64-reg (vm-aload-addr inst))) stream)
  (emit-a64-dmb-for-order (vm-atomic-memory-order* inst) stream))

(defun emit-a64-atomic-store (inst stream)
  "Lower VM atomic store to STXR retry loop plus DMB ordering."
  (let ((addr (a64-reg (vm-astore-addr inst)))
        (val (a64-reg (vm-astore-val inst))))
    (emit-a64-dmb-for-order (vm-atomic-memory-order* inst) stream)
    (emit-a64-instr (encode-a64-stxr +a64-stack-probe-scratch+ val addr) stream)
    (emit-a64-instr (encode-a64-cbnz +a64-stack-probe-scratch+ -1) stream)
    (emit-a64-dmb-for-order (vm-atomic-memory-order* inst) stream)))
