;;;; packages/codegen/src/ppc64-codegen.lisp - POWER10 backend support (FR-695)

(in-package :cl-cc/codegen)

(defparameter *ppc64-enabled* nil
  "When true, experimental POWER10/PPC64 code generation may be selected.")

(defparameter r0 0 "POWER general-purpose register 0.")
(defparameter r1 1 "POWER general-purpose register 1, stack pointer in ELFv2.")
(defparameter r2 2 "POWER general-purpose register 2, TOC pointer in ELFv2.")
(defparameter r3 3 "POWER general-purpose register 3, first argument/result in ELFv2.")
(defparameter r4 4 "POWER general-purpose register 4.")
(defparameter r5 5 "POWER general-purpose register 5.")
(defparameter r6 6 "POWER general-purpose register 6.")
(defparameter r7 7 "POWER general-purpose register 7.")
(defparameter r8 8 "POWER general-purpose register 8.")
(defparameter r9 9 "POWER general-purpose register 9.")
(defparameter r10 10 "POWER general-purpose register 10.")
(defparameter r11 11 "POWER general-purpose register 11.")
(defparameter r12 12 "POWER general-purpose register 12.")
(defparameter r13 13 "POWER general-purpose register 13, TLS pointer in ELFv2.")
(defparameter r14 14 "POWER general-purpose register 14.")
(defparameter r15 15 "POWER general-purpose register 15.")
(defparameter r16 16 "POWER general-purpose register 16.")
(defparameter r17 17 "POWER general-purpose register 17.")
(defparameter r18 18 "POWER general-purpose register 18.")
(defparameter r19 19 "POWER general-purpose register 19.")
(defparameter r20 20 "POWER general-purpose register 20.")
(defparameter r21 21 "POWER general-purpose register 21.")
(defparameter r22 22 "POWER general-purpose register 22.")
(defparameter r23 23 "POWER general-purpose register 23.")
(defparameter r24 24 "POWER general-purpose register 24.")
(defparameter r25 25 "POWER general-purpose register 25.")
(defparameter r26 26 "POWER general-purpose register 26.")
(defparameter r27 27 "POWER general-purpose register 27.")
(defparameter r28 28 "POWER general-purpose register 28.")
(defparameter r29 29 "POWER general-purpose register 29.")
(defparameter r30 30 "POWER general-purpose register 30.")
(defparameter r31 31 "POWER general-purpose register 31.")

(defparameter lr 8 "POWER Link Register SPR number.")
(defparameter ctr 9 "POWER Count Register SPR number.")

(defconstant +ppc64-opcode-nop+ #x60000000
  "POWER NOP instruction, encoded as ORI R0,R0,0.")

(defun emit-ppc64-instr (word32 stream)
  "Emit one 32-bit POWER instruction word in big-endian byte order.

The backend remains default-disabled by *PPC64-ENABLED*, but the encoder emits
real POWER instruction words for callers that explicitly select it."
  (funcall stream (logand (ash word32 -24) #xFF))
  (funcall stream (logand (ash word32 -16) #xFF))
  (funcall stream (logand (ash word32 -8) #xFF))
  (funcall stream (logand word32 #xFF)))

(defun encode-ppc64-nop ()
  "Encode POWER NOP (ORI R0,R0,0)."
  +ppc64-opcode-nop+)

(defun %ppc64-u5 (value)
  (logand value #x1F))

(defun %ppc64-s16 (value)
  (unless (<= -32768 value 32767)
    (error "POWER D-form immediate out of signed 16-bit range: ~D" value))
  (logand value #xFFFF))

(defun %ppc64-branch-disp (value bits alignment)
  (unless (zerop (mod value alignment))
    (error "POWER branch displacement must be ~D-byte aligned: ~D" alignment value))
  (let ((min (- (ash 1 (1- bits))))
        (max (1- (ash 1 (1- bits)))))
    (unless (<= min value max)
      (error "POWER branch displacement out of range: ~D" value)))
  value)

(defun %ppc64-spr-field (spr)
  "Return the split SPR field encoding used by mfspr/mtspr."
  (logior (ash (logand spr #x1F) 16)
          (ash (logand (ash spr -5) #x1F) 11)))

(defun encode-power10-addi (rt ra simm)
  "Encode ADDI RT,RA,SIMM."
  (logior (ash 14 26)
          (ash (%ppc64-u5 rt) 21)
          (ash (%ppc64-u5 ra) 16)
          (%ppc64-s16 simm)))

(defun encode-power10-add (rt ra rb)
  "Encode ADD RT,RA,RB using the POWER fixed-point XO form."
  (logior #x7C000214
          (ash (logand rt #x1F) 21)
          (ash (logand ra #x1F) 16)
          (ash (logand rb #x1F) 11)))

(defun encode-power10-ld (rt ra ds)
  "Encode LD RT,DS(RA).  DS is the byte displacement and must be 4-byte aligned."
  (unless (zerop (logand ds #x3))
    (error "POWER10 LD displacement must be 4-byte aligned: ~D" ds))
  (logior #xE8000000
           (ash (logand rt #x1F) 21)
           (ash (logand ra #x1F) 16)
           (logand ds #xFFFC)))

(defun encode-power10-std (rs ra ds)
  "Encode STD RS,DS(RA).  DS is the byte displacement and must be 4-byte aligned."
  (unless (zerop (logand ds #x3))
    (error "POWER10 STD displacement must be 4-byte aligned: ~D" ds))
  (logior #xF8000000
           (ash (logand rs #x1F) 21)
           (ash (logand ra #x1F) 16)
           (logand ds #xFFFC)))

(defun encode-power10-cmp (crfd ra rb &key (doubleword t))
  "Encode CMP CRFD,RA,RB.  DOUBLEWORD selects the 64-bit L bit."
  (logior (ash 31 26)
          (ash (logand crfd #x7) 23)
          (if doubleword (ash 1 21) 0)
          (ash (%ppc64-u5 ra) 16)
          (ash (%ppc64-u5 rb) 11)))

(defun encode-power10-bc (bo bi bd &key aa lk)
  "Encode conditional branch BC BO,BI,BD.  BD is a signed byte displacement."
  (let ((disp (%ppc64-branch-disp bd 16 4)))
    (logior (ash 16 26)
            (ash (%ppc64-u5 bo) 21)
            (ash (%ppc64-u5 bi) 16)
            (logand disp #xFFFC)
            (if aa #x2 0)
            (if lk #x1 0))))

(defun encode-power10-b (li &key aa lk)
  "Encode branch B LI.  LI is a signed byte displacement."
  (let ((disp (%ppc64-branch-disp li 26 4)))
    (logior (ash 18 26)
            (logand disp #x03FFFFFC)
            (if aa #x2 0)
            (if lk #x1 0))))

(defun encode-power10-bl (li &key aa)
  "Encode branch-and-link BL LI."
  (encode-power10-b li :aa aa :lk t))

(defun encode-power10-mflr (rt)
  "Encode MFLR RT as MFSPR RT,LR."
  (logior (ash 31 26)
          (ash (%ppc64-u5 rt) 21)
          (%ppc64-spr-field lr)
          (ash 339 1)))

(defun encode-power10-mtlr (rs)
  "Encode MTLR RS as MTSPR LR,RS."
  (logior (ash 31 26)
          (ash (%ppc64-u5 rs) 21)
          (%ppc64-spr-field lr)
          (ash 467 1)))

(defun ppc64-elfv2-prologue (&key (frame-size 32) (linkage-slot 16))
  "Return a basic ELFv2 prologue instruction list.

The sequence saves LR via R0 and reserves FRAME-SIZE bytes with ADDI R1,R1,-N.
Callers that require a back-chain store can add target-specific storage around
this conservative sequence."
  (unless (and (integerp frame-size) (plusp frame-size) (zerop (mod frame-size 16)))
    (error "ELFv2 frame size must be a positive 16-byte multiple: ~S" frame-size))
  (list (encode-power10-mflr r0)
        (encode-power10-std r0 r1 linkage-slot)
        (encode-power10-addi r1 r1 (- frame-size))))

(defun ppc64-elfv2-epilogue (&key (frame-size 32) (linkage-slot 16))
  "Return a basic ELFv2 epilogue instruction list restoring SP and LR."
  (unless (and (integerp frame-size) (plusp frame-size) (zerop (mod frame-size 16)))
    (error "ELFv2 frame size must be a positive 16-byte multiple: ~S" frame-size))
  (list (encode-power10-addi r1 r1 frame-size)
        (encode-power10-ld r0 r1 linkage-slot)
        (encode-power10-mtlr r0)))

(defun ppc64-backend-available-p ()
  "Return true only when the experimental POWER10 backend flag is enabled."
  (and *ppc64-enabled* t))
