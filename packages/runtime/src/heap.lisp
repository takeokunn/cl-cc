;;;; packages/runtime/src/heap.lisp - CL-CC Managed Heap for Generational GC
;;;
;;; This file provides the managed heap data structure for a 2-generation
;;; Generational GC:
;;;   - Young (nursery) split into 2 semi-spaces (from-space + to-space)
;;;     for Cheney copying collection.
;;;   - Old generation for tenured objects with a card table for tracking
;;;     old-to-young pointer references.
;;;
;;; Memory layout (flat simple-vector):
;;;   [0 .. semi-size-1]                  = from-space (young)
;;;   [semi-size .. 2*semi-size-1]         = to-space (young)
;;;   [2*semi-size .. 2*semi-size+old-1]   = old space
;;;
;;; Object header word bit layout (non-negative integer):
;;;   bit 63..32 = size in words (including header)
;;;   bit 31..24 = type tag (0..7, matching +tag-* constants in runtime.lisp)
;;;   bit 23..20 = age (0-15, minor GC survival cycles)
;;;   bit 19     = mark bit (set during major GC marking)
;;;   bit 18     = gray bit (SATB marking queue membership)
;;;   bit 17     = forwarding flag (Cheney copy; when set, header encodes dest addr)
;;;   bit 16..0  = reserved / forwarding address (when forwarding flag is set)

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Configuration Parameters
;;; ------------------------------------------------------------

(defparameter *gc-young-size-words* (* 128 1024)
  "Young space in 64-bit words (1MB total, 512KB per semi-space).")

(defparameter *gc-old-size-words* (* 512 1024)
  "Old space in 64-bit words (4MB).")

(defparameter *gc-tenuring-threshold* 3
  "Minor GC survival cycles before promotion to old generation.")

(defconstant +gc-card-size-words+ 64
  "Card size in words (512 bytes with 8-byte words).")

;;; ------------------------------------------------------------
;;; Object Header Bit Layout Constants
;;; ------------------------------------------------------------

;;; Size field: bits 63..32
(defconstant +header-size-shift+    32)
(defconstant +header-size-mask+     #xFFFFFFFF00000000)

;;; Type tag field: bits 31..24
(defconstant +header-tag-shift+     24)
(defconstant +header-tag-mask+      #x00000000FF000000)

;;; Age field: bits 23..20
(defconstant +header-age-shift+     20)
(defconstant +header-age-mask+      #x0000000000F00000)

;;; Flag bits (bits 19 and 18 for mark/gray; used only on integer headers)
(defconstant +header-mark-bit+      #x0000000000080000)  ; bit 19
(defconstant +header-gray-bit+      #x0000000000040000)  ; bit 18

;;; Forwarding pointers are represented as (cons :forwarded dest-addr)
;;; rather than bit-packed integers.  This avoids address-size limitations
;;; and is idiomatic for a Pure CL heap simulation.

;;; ------------------------------------------------------------
;;; Header Construction and Accessors
;;; ------------------------------------------------------------

(defun make-header (size type-tag &optional (age 0))
  "Construct a header word encoding SIZE, TYPE-TAG, and AGE.
   SIZE is the object size in words (including header).
   TYPE-TAG is 0-7 matching +tag-* constants.
   AGE is 0-15 (minor GC survival count)."
  (logior (ash size +header-size-shift+)
          (ash (logand type-tag #xFF) +header-tag-shift+)
          (ash (logand age #xF) +header-age-shift+)))

(defun header-size (h)
  "Extract the object size in words from header word H."
  (ash h (- +header-size-shift+)))

(defun header-tag (h)
  "Extract the type tag (0-7) from header word H."
  (logand (ash h (- +header-tag-shift+)) #xFF))

(defun header-age (h)
  "Extract the age (0-15) from header word H."
  (logand (ash h (- +header-age-shift+)) #xF))

(defun header-marked-p (h)
  "Return true if the mark bit is set in header word H."
  (not (zerop (logand h +header-mark-bit+))))

(defun header-gray-p (h)
  "Return true if the gray bit is set in header word H."
  (not (zerop (logand h +header-gray-bit+))))

(defun header-forwarding-p (h)
  "Return true if H represents a forwarding pointer (i.e. is a cons :forwarded)."
  (and (consp h) (eq (car h) :forwarded)))

(defun header-set-mark (h)
  "Return a new header with the mark bit set."
  (logior h +header-mark-bit+))

(defun header-clear-mark (h)
  "Return a new header with the mark bit cleared."
  (logand h (lognot +header-mark-bit+)))

(defun header-set-gray (h)
  "Return a new header with the gray bit set."
  (logior h +header-gray-bit+))

(defun header-clear-gray (h)
  "Return a new header with the gray bit cleared."
  (logand h (lognot +header-gray-bit+)))

(defun header-make-forwarding-ptr (dest-addr)
  "Create a forwarding pointer value for DEST-ADDR.
   This value is stored in slot 0 of the from-space object.
   Use header-forwarding-p to detect it and header-forwarding-ptr to extract."
  (cons :forwarded dest-addr))

(defun header-forwarding-ptr (h)
  "Extract the forwarding destination address from a forwarding pointer H.
   Only valid when (header-forwarding-p h) is true."
  (cdr h))

(defun header-increment-age (h)
  "Return a new header with age incremented by 1, capped at 15."
  (let* ((current-age (header-age h))
         (new-age (min 15 (1+ current-age))))
    (logior (logand h (lognot +header-age-mask+))
            (ash new-age +header-age-shift+))))

;;; ------------------------------------------------------------
;;; rt-heap Structure
;;; ------------------------------------------------------------

(defstruct (rt-heap (:constructor %make-rt-heap)
                    (:conc-name rt-heap-))
  (words           nil  :type simple-vector)
  (young-from-base 0    :type fixnum)
  (young-to-base   0    :type fixnum)
  (young-semi-size 0    :type fixnum)
  (young-free      0    :type fixnum)
  (old-base        0    :type fixnum)
  (old-size        0    :type fixnum)
  (old-free        0    :type fixnum)
  (minor-gc-count  0    :type fixnum)
  (major-gc-count  0    :type fixnum)
  (words-collected 0    :type fixnum)
  (words-promoted  0    :type fixnum)
  (card-table      nil  :type (simple-array (unsigned-byte 8) (*)))
  (roots           nil  :type list)
  (satb-queue      nil  :type list)
  (free-list       nil  :type list)
  (gc-state        :normal :type keyword))

;;; ------------------------------------------------------------
;;; make-rt-heap Factory
;;; ------------------------------------------------------------

(defun make-rt-heap (&key (young-size *gc-young-size-words*)
                          (old-size   *gc-old-size-words*))
  "Allocate and initialize a fresh managed heap.

   YOUNG-SIZE is the total young generation size in words; it is split
   evenly into two semi-spaces of YOUNG-SIZE/2 words each.
   OLD-SIZE is the old generation size in words.

   Layout of the flat word vector:
     [0 .. semi-size-1]                from-space
     [semi-size .. 2*semi-size-1]      to-space
     [2*semi-size .. 2*semi-size+old-1] old space"
  (let* ((semi-size  (floor young-size 2))
         (total-size (+ (* 2 semi-size) old-size))
         (words      (make-array total-size :initial-element 0))
         (old-base   (* 2 semi-size))
         (num-cards  (ceiling old-size +gc-card-size-words+))
         (card-table (make-array num-cards
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    (%make-rt-heap
     :words           words
     :young-from-base 0
     :young-to-base   semi-size
     :young-semi-size semi-size
     :young-free      0
     :old-base        old-base
     :old-size        old-size
     :old-free        old-base
     :minor-gc-count  0
     :major-gc-count  0
     :words-collected 0
     :words-promoted  0
     :card-table      card-table
     :roots           nil
     :satb-queue      nil
     :free-list       nil
     :gc-state        :normal)))

;;; ------------------------------------------------------------
;;; Heap Word Access
;;; ------------------------------------------------------------

(defun rt-heap-ref (heap index)
  "Read word at absolute INDEX from HEAP."
  (svref (rt-heap-words heap) index))

(defun rt-heap-set (heap index value)
  "Write VALUE at absolute INDEX in HEAP."
  (setf (svref (rt-heap-words heap) index) value))

(defun rt-heap-object-header (heap addr)
  "Read the header word of the object at absolute word address ADDR."
  (rt-heap-ref heap addr))

(defun rt-heap-set-header (heap addr new-header)
  "Write NEW-HEADER as the header of the object at absolute word address ADDR."
  (rt-heap-set heap addr new-header))

(defun rt-heap-object-size (heap addr)
  "Return the size in words of the object at absolute word address ADDR."
  (header-size (rt-heap-object-header heap addr)))

;;; Card table helpers, address predicates, and rt-object-pointer-slots
;;; are in heap-trace.lisp (loads next).
