;;;; packages/runtime/src/heap-trace.lisp — Card Table + Pointer Tracing Helpers
;;;
;;; Extracted from heap.lisp.
;;; Contains:
;;;   - Card table helpers: rt-card-index, rt-card-dirty-p,
;;;     rt-card-mark-dirty, rt-card-clear, rt-card-clear-all
;;;   - Address predicate helpers: rt-young-addr-p, rt-old-addr-p,
;;;     rt-heap-addr-p
;;;   - Pointer slot resolver: rt-object-pointer-slots (used by GC tracing)
;;;
;;; The rt-heap struct and basic allocators remain in heap.lisp (loads before).
;;;
;;; Load order: after heap.lisp, before gc.lisp.
(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Card Table Helpers
;;; ------------------------------------------------------------

(defun rt-card-index (heap old-addr)
  "Return the card table index for OLD-ADDR (an old-space word address)."
  (floor (- old-addr (rt-heap-old-base heap)) +gc-card-size-words+))

(defun rt-card-dirty-p (heap old-addr)
  "Return true if the card containing OLD-ADDR is marked dirty."
  (not (zerop (aref (rt-heap-card-table heap) (rt-card-index heap old-addr)))))

(defun rt-card-mark-dirty (heap old-addr)
  "Mark the card containing OLD-ADDR as dirty (contains old->young pointer)."
  (let ((card-idx (rt-card-index heap old-addr)))
    (setf (aref (rt-heap-card-table heap) card-idx) 1)
    (rt-card-summary-update heap card-idx)))

(defun rt-card-clear (heap old-addr)
  "Clear the dirty flag for the card containing OLD-ADDR."
  (let ((card-idx (rt-card-index heap old-addr)))
    (setf (aref (rt-heap-card-table heap) card-idx) 0)
    (rt-card-summary-update heap card-idx)))

(defun rt-card-clear-all (heap)
  "Clear all dirty flags in the card table."
  (fill (rt-heap-card-table heap) 0)
  (fill (rt-heap-card-summary heap) nil))

(defun rt-card-summary-update (heap card-idx)
  "Update the summary entry that covers CARD-IDX.

Each summary entry is true when any card in its 64-card block is dirty."
  (let* ((summary (rt-heap-card-summary heap))
         (card-table (rt-heap-card-table heap))
         (summary-idx (floor card-idx +gc-card-summary-block-size+)))
    (when (< summary-idx (length summary))
      (let* ((start (* summary-idx +gc-card-summary-block-size+))
             (end (min (length card-table)
                       (+ start +gc-card-summary-block-size+))))
        (setf (svref summary summary-idx)
              (loop for i from start below end
                    thereis (not (zerop (aref card-table i)))))))))

(defun rt-card-summary-clean-block-p (heap block-idx)
  "Return true when summary BLOCK-IDX contains no dirty cards."
  (let ((summary (rt-heap-card-summary heap)))
    (or (>= block-idx (length summary))
        (not (svref summary block-idx)))))

;;; ------------------------------------------------------------
;;; Object Pointer Predicate Helpers
;;; ------------------------------------------------------------

(defun rt-young-addr-p (heap addr)
  "Return true if ADDR is within the young from-space."
  (and (>= addr (rt-heap-young-from-base heap))
       (< addr (+ (rt-heap-young-from-base heap)
                  (rt-heap-young-semi-size heap)))))

(defun rt-old-addr-p (heap addr)
  "Return true if ADDR is within the old space."
  (and (>= addr (rt-heap-old-base heap))
       (< addr (+ (rt-heap-old-base heap)
                   (rt-heap-old-size heap)))))

(defun rt-large-obj-addr-p (heap addr)
  "Return true if ADDR is within the large object space."
  (and (>= addr (rt-heap-large-obj-base heap))
       (< addr (+ (rt-heap-large-obj-base heap)
                  (rt-heap-large-obj-size heap)))))

(defun rt-heap-addr-p (heap addr)
  "Return true if ADDR is within any live region of HEAP."
  (or (rt-young-addr-p heap addr)
      (rt-old-addr-p heap addr)
      (rt-large-obj-addr-p heap addr)))

;;; ------------------------------------------------------------
;;; Tracing: Pointer Slots per Object Type Tag
;;; ------------------------------------------------------------

(defun %rt-array-element-type-pointer-free-p (element-type)
  "Return true when ELEMENT-TYPE is stored unboxed and has no pointer slots."
  (member element-type '(:fixnum fixnum :integer integer
                         :double-float double-float :double
                         :character character :char
                         :bit bit)
          :test #'eq))

(defun %rt-array-metadata-element-type (metadata)
  "Extract array element type from runtime array METADATA.

Current heap arrays use slot 1 as rank/dimension metadata.  Specialized arrays
may instead store a plist header such as:
  (:type-tag :specialized-array :element-type :fixnum :gc-skip-p t)
or a compact pair like (:element-type . :fixnum)."
  (cond
    ((and (consp metadata) (keywordp (first metadata)))
     (or (getf metadata :element-type)
         (getf metadata :array-element-type)))
    ((and (consp metadata) (eq (car metadata) :element-type))
     (cdr metadata))
    (t nil)))

(defun %rt-array-metadata-gc-skip-p (metadata)
  "Return true when METADATA marks array payload as pointer-free."
  (or (and (consp metadata)
           (keywordp (first metadata))
           (getf metadata :gc-skip-p))
      (%rt-array-element-type-pointer-free-p
       (%rt-array-metadata-element-type metadata))))

(defun rt-object-pointer-slots (heap addr)
  "Return a list of slot indices (relative to ADDR) that contain pointer values.
   Returns NIL for leaf objects (strings, fixnums, characters).
   Used by the GC to find all outgoing references from an object at ADDR.

   Slot 0 is always the header word (never a pointer).
   Tag assignments match +tag-* constants in runtime.lisp:
     0 = fixnum (immediate, never heap-allocated with this header)
     1 = cons:    slot 1 = car, slot 2 = cdr
     2 = symbol:  slot 1 = name (string), slot 2 = pkg, slot 3 = plist
     3 = closure: slot 1 = fn-index (NOT a pointer), slots 2..size-1 = env
     4 = character (immediate)
     5 = array:   slots 2..size-1 are elements (all potential pointers)
     6 = string:  no pointer slots (packed character data)
     7 = other:   slots 1..size-1 are all pointers"
  (let ((tag  (rt-header-type-tag  (rt-heap-object-header heap addr)))
        (size (rt-heap-object-size heap addr)))
    (case tag
      (1  ; cons: car=slot1, cdr=slot2
       '(1 2))
      (2  ; symbol: name=slot1, pkg=slot2, plist=slot3
       '(1 2 3))
      (3  ; closure: fn-index=slot1 (raw integer, not a pointer), env=slots 2..size-1
       (loop for i from 2 below size collect i))
      (5  ; array: slot 1 holds rank/dimensions or specialized element metadata
       (let ((metadata (rt-heap-ref heap (1+ addr))))
         (if (%rt-array-metadata-gc-skip-p metadata)
             nil
             (loop for i from 2 below size collect i))))
      (6  ; string: character data packed in words — no pointer slots
       nil)
      (7  ; other heap object: all non-header slots are pointers
       (loop for i from 1 below size collect i))
      (otherwise
       nil))))
