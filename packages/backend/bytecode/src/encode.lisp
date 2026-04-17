;;;; packages/backend/bytecode/src/encode.lisp - CL-CC Bytecode ISA v2 Encoder
;;;;
;;;; 32-bit instruction word encoding.
;;;;
;;;; Instruction formats:
;;;;   [opcode:8][dst:8][src1:8][src2:8]   ; 3-operand (arithmetic, compare)
;;;;   [opcode:8][dst:8][imm16:16]          ; immediate load
;;;;   [opcode:8][offset:24]                ; branch (signed 24-bit offset)
;;;;   [opcode:8][dst:8][src:8][pad:8]      ; 2-operand (unary, move)
;;;;
;;;; Wide operands (>256) use extension prefix 0xFE followed by a full 32-bit word.

(in-package :cl-cc/bytecode)

;;; ------------------------------------------------------------
;;; Bytecode Word Type
;;; ------------------------------------------------------------

(deftype bytecode-word () '(unsigned-byte 32))

;;; ------------------------------------------------------------
;;; Opcode Constants
;;; ------------------------------------------------------------

;; Misc
(defconstant +op-nop+         #x00)

;; Load / Move
(defconstant +op-load-const+  #x01)
(defconstant +op-move+        #x02)
(defconstant +op-load-nil+    #x03)
(defconstant +op-load-true+   #x04)
(defconstant +op-load-fixnum+ #x05)

;; Arithmetic (NaN-boxed fast path + slow path)
(defconstant +op-add+  #x10)
(defconstant +op-sub+  #x11)
(defconstant +op-mul+  #x12)
(defconstant +op-div+  #x13)
(defconstant +op-mod+  #x14)
(defconstant +op-neg+  #x15)
(defconstant +op-inc+  #x16)
(defconstant +op-dec+  #x17)

;; Comparison
(defconstant +op-eq+     #x20)
(defconstant +op-eql+    #x21)
(defconstant +op-equal+  #x22)
(defconstant +op-num-lt+ #x23)
(defconstant +op-num-gt+ #x24)
(defconstant +op-num-le+ #x25)
(defconstant +op-num-ge+ #x26)
(defconstant +op-num-eq+ #x27)

;; Control Flow
(defconstant +op-jump+         #x30)
(defconstant +op-jump-if-nil+  #x31)
(defconstant +op-jump-if-true+ #x32)
(defconstant +op-call+         #x33)
(defconstant +op-tail-call+    #x34)
(defconstant +op-return+       #x35)
(defconstant +op-return-nil+   #x36)

;; Closure & Upvalue
(defconstant +op-make-closure+  #x40)
(defconstant +op-get-upvalue+   #x41)
(defconstant +op-set-upvalue+   #x42)
(defconstant +op-close-upvalue+ #x43)

;; Object Access (with inline cache)
(defconstant +op-get-slot+     #x50)
(defconstant +op-set-slot+     #x51)
(defconstant +op-get-global+   #x52)
(defconstant +op-set-global+   #x53)
(defconstant +op-make-instance+ #x54)

;; Collections
(defconstant +op-cons+        #x60)
(defconstant +op-car+         #x61)
(defconstant +op-cdr+         #x62)
(defconstant +op-make-vector+ #x63)
(defconstant +op-vector-ref+  #x64)
(defconstant +op-vector-set+  #x65)
(defconstant +op-make-hash+   #x66)
(defconstant +op-hash-ref+    #x67)
(defconstant +op-hash-set+    #x68)

;; Type Check (fast path)
(defconstant +op-type-check+ #x70)
(defconstant +op-fixnump+    #x71)
(defconstant +op-consp+      #x72)
(defconstant +op-symbolp+    #x73)
(defconstant +op-functionp+  #x74)
(defconstant +op-stringp+    #x75)

;; Multiple Values
(defconstant +op-values+      #x80)
(defconstant +op-recv-values+ #x81)

;; Exception Handling
(defconstant +op-push-handler+ #x90)
(defconstant +op-pop-handler+  #x91)
(defconstant +op-signal+       #x92)
(defconstant +op-push-unwind+  #x93)
(defconstant +op-pop-unwind+   #x94)

;; Wide prefix for operands > 255
(defconstant +op-wide+ #xfe)

;;; ------------------------------------------------------------
;;; Inline Field Extraction Helpers (for encode use)
;;; ------------------------------------------------------------

(declaim (inline %pack-byte %sign-extend-16 %sign-extend-24))

(defun %pack-byte (x)
  "Clamp x to 8 bits (unsigned)."
  (declare (type (unsigned-byte 8) x))
  (logand x #xff))

(defun %sign-extend-16 (n)
  "Interpret signed 16-bit value n as unsigned 16-bit for encoding.
   Input n must be in [-32768, 32767]."
  (declare (type (integer -32768 32767) n))
  (logand n #xffff))

(defun %sign-extend-24 (n)
  "Interpret signed 24-bit value n as unsigned 24-bit for encoding.
   Input n must be in [-8388608, 8388607]."
  (declare (type (integer -8388608 8388607) n))
  (logand n #xffffff))

;;; ------------------------------------------------------------
;;; Core Encoding Functions
;;; ------------------------------------------------------------

