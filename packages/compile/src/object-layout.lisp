;;;; packages/compile/src/object-layout.lisp — FR-566 Object Layout Optimization
;;;; CLOS objects → C-struct style fixed-offset fields.
;;;; Hash-table-based slot access → direct [base+offset] memory reference.

(in-package :cl-cc/compile)

;;; ──── Slot offset mapping ────
(defvar *class-layout-table* (make-hash-table :test #'eq)
  "Class name → (slot-name . byte-offset) alist with fixed offsets.")

(defvar *layout-optimization-enabled* t
  "When T, compile slot-value/slot-setf to direct [base+offset] access.")

(defstruct (class-layout (:conc-name cl-))
  "Fixed-offset layout for a CLOS class."
  (class-name nil :type symbol)
  (slot-offsets nil :type list)     ; ((slot-name . offset) ...)
  (hot-slots nil :type list)        ; slots ordered by access frequency
  (cold-slots nil :type list)       ; rarely accessed slots
  (total-size 0 :type fixnum)       ; total object size in bytes
  (is-struct-p nil))                ; T if struct-style (defstruct)

;;; ──── Layout computation ────
(defun compute-class-layout (class-name slot-names &key (hot-slots nil))
  "Compute a fixed-offset layout for CLASS-NAME with SLOT-NAMES.
HOT-SLOTS are placed first for cache efficiency.
Returns a class-layout struct."
  (let* ((ordered-slots (append hot-slots
                               (set-difference slot-names hot-slots :test #'eq)))
         (offset 8) ; skip object header (8 bytes)
         (slot-offsets nil))
    (dolist (slot ordered-slots)
      (push (cons slot offset) slot-offsets)
      (incf offset 8)) ; each slot is 8 bytes (Lisp word)
    (let ((layout (make-class-layout
                   :class-name class-name
                   :slot-offsets (nreverse slot-offsets)
                   :hot-slots hot-slots
                   :cold-slots (set-difference slot-names hot-slots :test #'eq)
                   :total-size offset)))
      (setf (gethash class-name *class-layout-table*) layout)
      layout)))

(defun slot-offset (class-name slot-name)
  "Return the byte offset of SLOT-NAME in CLASS-NAME, or NIL if not computed."
  (let* ((layout (gethash class-name *class-layout-table*))
         (entry (and layout (assoc slot-name (cl-slot-offsets layout) :test #'eq))))
    (when entry
      (cdr entry))))

;;; ──── Codegen: slot-value → [base + offset] ────
(defun emit-direct-slot-read (stream base-reg class-name slot-name)
  "Emit code for direct slot read: MOV dst, [base + offset].
x86-64: 3-4 bytes, vs ~30 for hash-table lookup."
  (let ((offset (slot-offset class-name slot-name)))
    (if offset
        (progn
          ;; MOV reg, [base + offset]
          ;; 48 8B XX XX — REX.W + MOV r64, [base+disp8/32]
          #+x86-64
          (progn
            (write-byte #x48 stream)          ; REX.W
            (write-byte #x8B stream)          ; MOV r64, r/m64
            ;; ModRM: [base + disp8] if offset < 128 else [base + disp32]
            (if (< offset 128)
                (progn
                  (write-byte (logior (ash base-reg 3) #x40) stream) ; ModRM: [base+disp8]
                  (write-byte offset stream))
                (progn
                  (write-byte (logior (ash base-reg 3) #x80) stream) ; ModRM: [base+disp32]
                  (write-sequence (encode-int32 offset) stream))))
          3) ; estimated bytes
        ;; Fallback: use generic slot-value (hash-table lookup)
        (values -1 nil)))))

(defun emit-direct-slot-write (stream base-reg class-name slot-name value-reg)
  "Emit code for direct slot write: MOV [base + offset], src."
  (let ((offset (slot-offset class-name slot-name)))
    (when offset
      #+x86-64
      (progn
        (write-byte #x48 stream)
        (write-byte #x89 stream)         ; MOV r/m64, r64
        (if (< offset 128)
            (progn
              (write-byte (logior (ash base-reg 3) #x40) stream)
              (write-byte offset stream))
            (progn
              (write-byte (logior (ash base-reg 3) #x80) stream)
              (write-sequence (encode-int32 offset) stream))))
      t)))

;;; ──── Integration with defclass ────
(defmacro define-class-layout (class-name &rest slot-names)
  "Define the fixed-offset layout for CLASS-NAME.
Must be called after defclass to enable direct slot access."
  `(compute-class-layout ',class-name ',slot-names))

;;; ──── MOP fallback ────
;; When slot-value is used dynamically (via MOP), fall back to hash-table.
;; The compiler checks at compile time whether the class layout is known.
(defun slot-access-has-layout-p (class-name)
  "Return T if CLASS-NAME has a computed fixed-offset layout."
  (gethash class-name *class-layout-table*))

;;; ──── Helper ────
(defun encode-int32 (v)
  (let ((b (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref b 0) (logand v #xFF)
          (aref b 1) (logand (ash v -8) #xFF)
          (aref b 2) (logand (ash v -16) #xFF)
          (aref b 3) (logand (ash v -24) #xFF))
    b))
