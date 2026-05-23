;;;; src/jit/write-barrier.lisp — FR-552 Write Barrier Optimization
;;;; Card table + remembered set + SATB barrier implementation.
;;;; Generational GC write barrier optimization.

(in-package :cl-cc/jit)

;;; ──── Card Table ────
;; Heap divided into 512-byte cards. Writing to a card dirties it.
;; GC only scans dirty cards during minor collections.
(defconstant +card-size+ 512
  "Size of each card in the card table (bytes).")

(defvar *card-table* nil
  "Byte vector: card-table[i] ≠ 0 means card i is dirty.")

(defvar *heap-base* nil
  "Base address of the managed heap.")

(defun card-index (address)
  "Compute card table index for heap ADDRESS."
  (floor (- address *heap-base*) +card-size+))

(defun card-table-mark (address)
  "Mark the card containing ADDRESS as dirty (one byte write)."
  (when *card-table*
    (setf (aref *card-table* (card-index address)) 1)))

(defun card-table-clear (address)
  "Clear the dirty mark for the card containing ADDRESS."
  (when *card-table*
    (setf (aref *card-table* (card-index address)) 0)))

(defun card-table-clear-all ()
  "Clear the entire card table (after major GC sweep)."
  (when *card-table*
    (fill *card-table* 0)))

(defun card-table-init (heap-size)
  "Initialize the card table for a heap of HEAP-SIZE bytes."
  (setf *card-table*
        (make-array (ceiling heap-size +card-size+)
                    :element-type '(unsigned-byte 8)
                    :initial-element 0)))

;;; ──── Write Barrier Emission ────
;; Emitted before every slot write (setf slot-value / setf aref).
;; Optimized: card table mark only when slot is in old generation
;; and value being stored is a young-generation reference.

(defvar *barrier-elision-enabled* t
  "When T, skip write barrier when compiler proves it unnecessary.")

(defun emit-write-barrier (stream slot-address-reg)
  "Emit write barrier code for a slot write at SLOT-ADDRESS-REG.
x86-64: MOV [card_table + idx], 1 (minimal: ~6 bytes).
Returns the number of bytes emitted."
  ;; Simplified: emit card table mark
  ;; In production: check young→old direction, SATB logging
  (let ((bytes-emitted 0))
    ;; MOV byte [card_table_addr + card_index], 1
    ;; card_index = (slot_addr - heap_base) / 512
    ;; Simplified for now: use immediate store
    (when *card-table*
      #+x86-64
      (progn
        (write-byte #xC6 stream)        ; MOV r/m8, imm8
        (write-byte #x05 stream)        ; ModRM: [RIP+disp32]
        (write-sequence (encode-int32 (card-index 0)) stream)
        (write-byte 1 stream)
        (incf bytes-emitted 7)))
    bytes-emitted))

;;; ──── Barrier Elision ────
(defun barrier-elision-p (slot-address value-type)
  "Return T if the write barrier can be safely elided.
Conditions for elision:
- Compiling for young-generation allocation (no old→young references possible)
- Value being stored is definitely not a heap reference (fixnum, char, etc.)
- Slot is in a newly allocated object (still in nursery)"
  (and *barrier-elision-enabled*
       (or (member value-type '(fixnum character float))
           ;; Add more type-based elision rules
           nil)))

;;; ──── SATB (Snapshot-At-The-Beginning) Barrier ────
;; Used for concurrent GC: log old values before overwriting.

(defvar *satb-queue* nil
  "SATB marking queue for concurrent GC.")

(defvar *satb-enabled* nil
  "T when SATB concurrent marking is active.")

(defmacro with-satb-barrier (&body body)
  "Execute BODY with SATB concurrent marking barrier enabled."
  `(let ((*satb-enabled* t))
     ,@body))

(defun satb-enqueue (value)
  "Enqueue VALUE into the SATB marking queue (before overwriting)."
  (when (and *satb-enabled* *satb-queue*)
    (vector-push-extend value *satb-queue*)))

;;; ──── Remembered Set Barrier ────
;; Only track Old→Young references (Young→Old doesn't need tracking).

(defun old-to-young-reference-p (slot-address value)
  "Return T if writing VALUE to SLOT-ADDRESS creates an Old→Young ref."
  (and (in-old-generation-p slot-address)
       (in-young-generation-p value)))

(defun in-old-generation-p (address)
  "Return T if ADDRESS is in the old generation."
  (declare (ignore address))
  nil) ; Placeholder — use real heap bounds

(defun in-young-generation-p (address)
  "Return T if ADDRESS is in the young generation (nursery)."
  (declare (ignore address))
  nil) ; Placeholder

;;; ──── Helper ────
(defun encode-int32 (value)
  "Encode a 32-bit signed integer as 4 bytes (little-endian)."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref buf 0) (logand value #xFF)
          (aref buf 1) (logand (ash value -8) #xFF)
          (aref buf 2) (logand (ash value -16) #xFF)
          (aref buf 3) (logand (ash value -24) #xFF))
    buf))
