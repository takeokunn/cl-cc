;;;; packages/codegen/src/sanitizer.lisp — FR-554 sanitizer instrumentation

(in-package :cl-cc/codegen)

(defconstant +asan-shadow-base-x86-64+ #x7fff8000
  "Linux x86-64 ASan shadow base.  One shadow byte covers 8 app bytes.")

(defparameter *asan-instrumentation-enabled* nil
  "When true, emit x86-64 ASan shadow checks before memory loads/stores.")

(defparameter *ubsan-instrumentation-enabled* nil
  "When true, emit x86-64 UBSan overflow/null checks around unsafe operations.")

(defparameter *asan-shadow-base* +asan-shadow-base-x86-64+
  "Current ASan shadow-memory base used by native x86-64 instrumentation.")

(defstruct sanitizer-options
  "Sanitizer options carried by the code generator.  Disabled options emit no code."
  (address nil :type boolean)
  (undefined nil :type boolean)
  (shadow-base +asan-shadow-base-x86-64+ :type integer))

(defun sanitizer-enabled-p (&optional (options nil options-supplied-p))
  "Return true when any sanitizer instrumentation is enabled."
  (if options-supplied-p
      (and options
           (or (sanitizer-options-address options)
               (sanitizer-options-undefined options)))
      (or *asan-instrumentation-enabled*
          *ubsan-instrumentation-enabled*)))

(defun %emit-shr-ri8 (reg imm stream)
  "Emit SHR r64, imm8."
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xC1 stream)
  (emit-byte (modrm 3 5 reg) stream)
  (emit-byte (logand imm #xff) stream))

(defun %emit-ubsan-trap-if-zero (reg stream)
  "Emit TEST reg,reg; JNE ok; INT3."
  (emit-test-rr64 reg reg stream)
  (emit-jne-rel8 1 stream)
  (emit-byte #xCC stream))

(defun sanitize-null-pointer-before-access (reg stream)
  "Emit a UBSan null-pointer check for REG when UBSan is enabled."
  (when *ubsan-instrumentation-enabled*
    (%emit-ubsan-trap-if-zero reg stream)))

(defun sanitize-memory-access-before-load-store (base index scale offset stream)
  "Emit ASan shadow-memory check for [BASE + INDEX*SCALE + OFFSET].

Sequence:
  LEA r11, [addr]
  MOV r10, r11
  SHR r10, 3
  ADD r10, shadow_base
  MOV r10, [r10]
  CMP r10, 0
  JNE report_error
  ... original load/store ...

The error path is a one-byte INT3 trap.  The entire sequence is gated by
*ASAN-INSTRUMENTATION-ENABLED*, so disabled builds generate no extra bytes."
  (when *asan-instrumentation-enabled*
    (emit-lea +r11+ base index scale offset stream)
    (emit-mov-rr64 +r10+ +r11+ stream)
    (%emit-shr-ri8 +r10+ 3 stream)
    (emit-add-ri32 +r10+ *asan-shadow-base* stream)
    (emit-mov-rm64 +r10+ +r10+ 0 stream)
    (emit-cmp-ri64 +r10+ 0 stream)
    (emit-jne-rel8 1 stream)
    (emit-byte #xCC stream)))

(defun sanitize-integer-overflow-after-op (stream)
  "Emit a UBSan signed-overflow trap after an arithmetic op that sets OF."
  (when *ubsan-instrumentation-enabled*
    ;; JO +1; INT3.  The non-overflow path falls through over the trap.
    (emit-byte #x70 stream)
    (emit-byte #x01 stream)
    (emit-byte #xCC stream)))

(defun %sanitizer-memory-access-p (inst)
  (typep inst '(or vm-aref vm-aset)))

(defun %sanitizer-overflow-op-p (inst)
  (member (type-of inst)
          '(vm-integer-add vm-integer-sub vm-integer-mul)
          :test #'eq))

(defun sanitizer-instrumentation-size (inst)
  "Return the extra encoded byte count added for INST by enabled sanitizers."
  (let ((size 0))
    (when (and *ubsan-instrumentation-enabled*
               (%sanitizer-overflow-op-p inst))
      (incf size 3))
    (when (%sanitizer-memory-access-p inst)
      (when *ubsan-instrumentation-enabled*
        (incf size 6))
      (when *asan-instrumentation-enabled*
        (let* ((base (vm-reg-to-x86 (if (typep inst 'vm-aref)
                                        (vm-array-reg inst)
                                        (vm-array-reg inst))))
               (raw-index (vm-reg-to-x86 (vm-index-reg inst)))
               (index (if (= (logand raw-index #x7) 4) +r11+ raw-index))
               (index-fixup (if (= (logand raw-index #x7) 4) 3 0))
               (lea-size (x86-64-lea-address-byte-size
                          (make-x86-64-lea-address
                           :dst +r11+ :base base :index index
                           :scale (x86-64-array-element-scale inst)
                           :displacement +x86-64-array-data-offset+))))
          (incf size (+ index-fixup lea-size 27)))))
    size))

(defun emit-sanitized-vm-instruction-with-labels (inst stream pos label-offsets)
  "Emit INST, adding ASan/UBSan checks only when their flags are enabled."
  (cond
    ((not (sanitizer-enabled-p))
     (emit-vm-instruction-with-labels inst stream pos label-offsets))
    ((%sanitizer-memory-access-p inst)
     (let* ((base (vm-reg-to-x86 (vm-array-reg inst)))
            (raw-index (vm-reg-to-x86 (vm-index-reg inst)))
            (index (x86-64-sib-index-register raw-index stream)))
       (sanitize-null-pointer-before-access base stream)
       (sanitize-memory-access-before-load-store
        base index (x86-64-array-element-scale inst) +x86-64-array-data-offset+ stream)
       (emit-vm-instruction-with-labels inst stream pos label-offsets)))
    ((%sanitizer-overflow-op-p inst)
     (emit-vm-instruction-with-labels inst stream pos label-offsets)
     (sanitize-integer-overflow-after-op stream))
    (t
     (emit-vm-instruction-with-labels inst stream pos label-offsets))))
