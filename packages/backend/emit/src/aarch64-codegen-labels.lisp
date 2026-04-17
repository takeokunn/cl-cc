;;;; packages/backend/emit/src/aarch64-codegen-labels.lisp — AArch64 64-bit immediate + size estimation + label offsets
;;;
;;; Two-pass label resolution infrastructure for AArch64 code generation:
;;;   emit-a64-mov-imm64, a64-imm64-size — MOVZ/MOVK sequence helpers
;;;   *a64-instruction-sizes*, a64-instruction-size — size table + estimator
;;;   build-a64-label-offsets — label offset map builder
;;;
;;; Depends on aarch64-codegen.lisp (encode-movz, encode-movk, a64-reg).
;;; Load order: immediately after aarch64-codegen.lisp.

(in-package :cl-cc/emit)

;;; 64-bit Immediate Loading

(defun emit-a64-mov-imm64 (rd value stream)
  "Emit MOVZ/MOVK sequence to load a 64-bit integer VALUE into register RD."
  (let ((chunk0 (logand value #xFFFF))
        (chunk1 (logand (ash value -16) #xFFFF))
        (chunk2 (logand (ash value -32) #xFFFF))
        (chunk3 (logand (ash value -48) #xFFFF)))
    ;; Always emit MOVZ for chunk0
    (emit-a64-instr (encode-movz rd chunk0 0) stream)
    ;; Emit MOVK for non-zero upper chunks
    (when (or (not (zerop chunk1)) (not (zerop chunk2)) (not (zerop chunk3)))
      (emit-a64-instr (encode-movk rd chunk1 1) stream))
    (when (or (not (zerop chunk2)) (not (zerop chunk3)))
      (emit-a64-instr (encode-movk rd chunk2 2) stream))
    (when (not (zerop chunk3))
      (emit-a64-instr (encode-movk rd chunk3 3) stream))))

(defun a64-imm64-size (value)
  "Return the number of 4-byte instructions needed to load VALUE."
  (let ((chunk1 (logand (ash value -16) #xFFFF))
        (chunk2 (logand (ash value -32) #xFFFF))
        (chunk3 (logand (ash value -48) #xFFFF)))
    (cond ((not (zerop chunk3)) 4)
          ((not (zerop chunk2)) 3)
          ((not (zerop chunk1)) 2)
          (t 1))))

;;; Instruction Size Estimation (for two-pass label resolution)

(defparameter *a64-instruction-sizes*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash 'vm-move ht) 4)          ; encode-mov-rr
    (setf (gethash 'vm-add ht) 4)           ; encode-add
    (setf (gethash 'vm-integer-add ht) 4)   ; encode-add
    (setf (gethash 'vm-sub ht) 4)           ; encode-sub
    (setf (gethash 'vm-integer-sub ht) 4)   ; encode-sub
    (setf (gethash 'vm-mul ht) 4)           ; encode-mul
    (setf (gethash 'vm-integer-mul ht) 4)   ; encode-mul
    (setf (gethash 'vm-min ht) 8)           ; encode-cmp + encode-csel
    (setf (gethash 'vm-max ht) 8)           ; encode-cmp + encode-csel
    (setf (gethash 'vm-select ht) 8)        ; encode-cmp + encode-csel
    (setf (gethash 'vm-bswap ht) 4)         ; encode-rev32
    (setf (gethash 'vm-rotate ht) 8)        ; MOV + RORV
    (setf (gethash 'vm-label ht) 0)         ; no code
    (setf (gethash 'vm-jump ht) 4)          ; encode-b
    (setf (gethash 'vm-jump-zero ht) 4)     ; encode-cbz
    (setf (gethash 'vm-halt ht) 4)          ; encode-mov-rr
    (setf (gethash 'vm-ret ht) 28)          ; 6x LDP + RET = 7 instructions
    (setf (gethash 'vm-call ht) 4)          ; encode-blr
    (setf (gethash 'vm-tail-call ht) 4)     ; encode-br
    (setf (gethash 'vm-spill-store ht) 4)   ; STUR
    (setf (gethash 'vm-spill-load ht) 4)    ; LDUR
    (setf (gethash 'vm-print ht) 0)
    ht)
  "Maps VM instruction type symbols to their AArch64 encoded byte sizes.")

(defun a64-instruction-size (inst)
  "Return size in bytes (multiple of 4) for an AArch64-encoded VM instruction."
  (let ((tp (type-of inst)))
    (cond
      ((eq tp 'vm-const)
       (* 4 (a64-imm64-size (logand (vm-value inst) #xFFFFFFFFFFFFFFFF))))
      ((eq tp 'vm-move)
       (let ((rd (a64-reg (vm-dst inst)))
             (rn (a64-reg (vm-src inst))))
         (if (= rd rn) 0 4)))
      (t
       (or (gethash tp *a64-instruction-sizes*) 0)))))

;;; Label offset table builder

(defun build-a64-label-offsets (instructions prologue-size)
  "Build hash table mapping label names to byte offsets from function start."
  (let ((offsets (make-hash-table :test #'equal))
        (pos prologue-size))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) offsets) pos))
      (incf pos (a64-instruction-size inst)))
    offsets))
