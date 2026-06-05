;;;; packages/runtime/src/value.lisp - NaN-Boxing Value Representation
;;;
;;; Simulates 64-bit NaN-boxing on top of SBCL using (unsigned-byte 64) integers.
;;; All VM instructions input and output boxed values (type VAL = (unsigned-byte 64)).
;;;
;;; Bit layout:
;;;
;;;   Fixnum:    bits[63:13] sign-extended integer, bits[12:0] = 0 (tag = 0000)
;;;              Encoding: (ash integer 13)  — 51-bit signed integers
;;;
;;;   Double:    Any bit pattern that is NOT a quiet NaN with our tag flag set.
;;;              Decoded by reinterpreting bits as IEEE 754 double via LDB.
;;;
;;;   Pointer:   Quiet NaN base (bits[62:51] = #x7FF, bit[50] = 1) + 3-bit tag in bits[50:48]
;;;              + 48-bit address in bits[47:0].  When pointer compression is
;;;              enabled, bit[47] marks a compressed payload and bits[31:0]
;;;              carry an offset from *HEAP-BASE-ADDRESS*.
;;;              Base mask: #x7FF8000000000000
;;;              Tag in bits [50:48]:
;;;                001 = Object   (general heap object)
;;;                010 = Cons     (cons cell)
;;;                011 = Symbol
;;;                100 = Function (closure)
;;;                101 = String
;;;
;;;   Character: Quiet NaN base #x7FFE000000000000 + 21-bit codepoint in bits[20:0]
;;;
;;;   Nil:       #x7FFF000000000000
;;;   T:         #x7FFF000000000001
;;;   Unbound:   #x7FFF000000000002
;;;
;;;   Immediate symbols:
;;;              #x7FFF000000000100 + small index in bits[7:0]
;;;              NIL and T keep their dedicated singleton encodings above.
;;;
;;;   SSO string: bits[63:59]=#b01110, bits[58:3] store up to 7 character
;;;               bytes little-endian by index, bits[2:0] store byte length.

(in-package :cl-cc/runtime)

(declaim (optimize (speed 3) (safety 1)))

(defconstant +max-u64+ #xFFFFFFFFFFFFFFFF
  "All-ones 64-bit mask.  Used to force u64 representation from integer
   operations that SBCL may otherwise represent as bignums.")

;;; ------------------------------------------------------------
;;; Core bit-pattern constants
;;; ------------------------------------------------------------

;;; Quiet NaN base: exponent all-ones (bits[62:52]=7FF) + quiet bit (bit[51]=1).
;;; We use bit[50] as the "tagged pointer" discriminator — when set, this is a
;;; pointer value; when clear (but still NaN), it is a character or special.
(defconstant +nan-boxing-quiet-nan+  #x7FF8000000000000
  "IEEE 754 quiet NaN canonical bit pattern (positive, no payload).")

;;; Pointer values use +nan-boxing-quiet-nan+ as the base, with bits[50:48] = 000.
;;; OR-ing one of the 3-bit sub-tags (001..101) into bits[50:48] gives each kind
;;; a distinct upper-16-bit pattern:
;;;   object=001  → upper16=#x7FF9
;;;   cons=010    → upper16=#x7FFA
;;;   symbol=011  → upper16=#x7FFB
;;;   function=100→ upper16=#x7FFC
;;;   string=101  → upper16=#x7FFD
;;; Character (#x7FFE) and nil/t/unbound (#x7FFF) therefore cannot collide.
(defconstant +ptr-base+     #x7FF8000000000000
  "Base bit pattern for pointer-tagged values (= quiet NaN, bits[50:48]=000).")

(defconstant +ptr-mask+     #x7FFF000000000000
  "Mask covering the NaN prefix + all tag bits (bits[63:48]).")

(defconstant +tag-mask+     #x0007000000000000
  "Isolates the 3-bit sub-tag from a pointer value (bits[50:48]).")

(defconstant +addr-mask+    #x0000FFFFFFFFFFFF
  "Isolates the 48-bit address payload from a pointer value.")

(defconstant +compressed-pointer-flag+ #x0000800000000000
  "Payload bit marking a pointer value whose low 32 bits are heap-relative.")

(defconstant +compressed-pointer-offset-mask+ #x00000000FFFFFFFF
  "Mask selecting the 32-bit compressed pointer offset payload.")

(defconstant +compressed-heap-region-bytes+ #x100000000
  "Maximum byte size of a heap region addressable by compressed pointers.")

(defconstant +compressed-heap-region-words+ #x20000000
  "Maximum 8-byte words in the 4GB compressed pointer heap region.")

(defparameter *compressed-pointers-enabled* nil
  "When true, pointer NaN-box payloads store 32-bit offsets from *HEAP-BASE-ADDRESS*.")

(defparameter *heap-base-address* 0
  "Logical start address of the managed heap region used for pointer compression.")

(export '(+compressed-pointer-flag+ +compressed-pointer-offset-mask+
          +compressed-heap-region-bytes+ +compressed-heap-region-words+
          *compressed-pointers-enabled* *heap-base-address*
          val-compressed-pointer-p compressed-pointer-enabled-p
          encode-compressed-pointer decode-compressed-pointer
          val-encode-pointer val-decode-pointer)
        :cl-cc/runtime)

;;; 3-bit pointer sub-tags (shifted to bits[50:48])
(defconstant +tag-object+   #x0001000000000000  "Heap object sub-tag.")
(defconstant +tag-cons+     #x0002000000000000  "Cons cell sub-tag.")
(defconstant +tag-symbol+   #x0003000000000000  "Symbol sub-tag.")
(defconstant +tag-function+ #x0004000000000000  "Function/closure sub-tag.")
(defconstant +tag-string+   #x0005000000000000  "String sub-tag.")

;;; Character: quiet NaN with bit[50] clear but bit[49] set.
;;; Base = #x7FFE_... gives bit[49]=1, bit[50]=0 → distinct from pointers.
(defconstant +tag-char+     #x7FFE000000000000
  "Base bit pattern for character immediate values.")

;;; Special singleton values (quiet NaN space, bits[50:49]=11).
(defconstant +val-nil+      #x7FFF000000000000  "Boxed NIL.")
(defconstant +val-t+        #x7FFF000000000001  "Boxed T.")
(defconstant +val-unbound+  #x7FFF000000000002  "Unbound-slot sentinel.")

;;; A compact immediate-symbol subspace inside the already-reserved #x7FFF
;;; singleton range.  The low byte is the symbol table index; the next byte is
;;; fixed to #x01 so these values cannot collide with NIL/T/UNBOUND.
(defconstant +immediate-symbol-base+ #x7FFF000000000100
  "Base bit pattern for frequent symbol immediate values.")

(defconstant +immediate-symbol-mask+ #xFFFFFFFFFFFFFF00
  "Mask selecting the fixed immediate-symbol prefix.")

(defconstant +immediate-symbol-index-mask+ #xFF
  "Mask selecting the 8-bit immediate-symbol table index.")

(defconstant +sso-string-base+ #x7000000000000000
  "Base marker for small-string immediate values.")

(defconstant +sso-string-mask+ #xF800000000000000
  "Mask selecting the fixed SSO marker bits in the top byte.")

(defconstant +sso-string-length-mask+ #x7
  "Mask selecting the 3-bit SSO length field.")

(defconstant +sso-string-max-length+ 7
  "Maximum number of inline bytes in an SSO string.")

(defparameter *immediate-symbol-table*
  #(:key :value :test :test-not :start :end :from-end :count :initial-value
    :element-type :initial-element :allow-other-keys :adjustable :fill-pointer
    quote lambda function declare setq setf if progn let let* block return-from
    tagbody go catch throw unwind-protect flet labels macrolet symbol-macrolet
    the values multiple-value-bind multiple-value-call eval-when locally and or
    cond case typecase ecase ccase loop do do* dolist dotimes defun defmacro
    defvar defparameter defconstant defclass defmethod defgeneric car cdr cons
    list append apply funcall)
  "Small canonical table of frequent symbols encoded without heap allocation.
NIL and T intentionally remain +VAL-NIL+ and +VAL-T+, not entries here.")

(defparameter *immediate-symbol-indexes*
  (let ((table (make-hash-table :test #'eq)))
    (loop for sym across *immediate-symbol-table*
          for i from 0
          do (setf (gethash sym table) i))
    table)
  "Host symbol -> immediate-symbol index mapping.")

;;; Fixnum: tag = 0 (bits[12:0] = 0), integer in bits[63:13].
;;; A valid boxed fixnum has its low 13 bits all zero.
(defconstant +fixnum-tag+   0)
(defconstant +fixnum-mask+  #x1FFF  "Low 13 bits; zero means fixnum.")
(defconstant +fixnum-shift+ 13      "Bits to shift integer for fixnum encoding.")

;;; Double detection: a double is any (unsigned-byte 64) that is NOT in our
;;; tagged NaN space.  Our tagged NaN space starts at #x7FF8000000000000
;;; with bit[50] or bit[49] set.  The simplest discriminant: if
;;; bits[62:49] = #x3FFE (i.e., upper 15 bits after bit63 = 0111_1111_1111_110x)
;;; then it is a pointer or special.  We detect doubles as "not NaN-tagged".
(defconstant +nan-tag-base+ #x7FF8000000000000
  "Minimum value whose upper bits indicate our NaN-tagged space.")

;;; ------------------------------------------------------------
;;; Type predicates  (inline for performance)
;;; ------------------------------------------------------------

(declaim (ftype (function ((unsigned-byte 64)) boolean)
                val-fixnum-p val-double-p val-pointer-p
                val-compressed-pointer-p
                val-nil-p val-t-p val-char-p val-unbound-p
                val-object-p val-cons-p val-symbol-p val-function-p val-string-p
                val-sso-string-p val-immediate-symbol-p))

(declaim (inline val-fixnum-p val-double-p val-pointer-p
                 val-compressed-pointer-p
                 val-nil-p val-t-p val-char-p val-unbound-p
                 val-object-p val-cons-p val-symbol-p val-function-p val-string-p
                 val-sso-string-p val-immediate-symbol-p encode-immediate-symbol-index
                 immediate-symbol-index decode-immediate-symbol))

(defun val-sso-string-p (v)
  "True if V is an inline small-string immediate value."
  (declare (type (unsigned-byte 64) v))
  (= (logand v +sso-string-mask+) +sso-string-base+))

(defun val-fixnum-p (v)
  "True if V is a boxed fixnum: low 13 bits all zero AND not in NaN-tagged space.

   The NaN-tagged space occupies [+nan-tag-base+, #x8000000000000000) — a narrow
   positive range with exponent bits all-ones.  Negative fixnums have bit 63 = 1
   (value >= #x8000000000000000) and therefore fall ABOVE the NaN-tagged range;
   they must not be rejected by the +nan-tag-base+ guard."
  (declare (type (unsigned-byte 64) v))
  (and (zerop (logand v +fixnum-mask+))
       (not (val-sso-string-p v))
       ;; Exclude only the NaN-tagged range.  It is entirely positive (bit 63 = 0),
       ;; so values with bit 63 = 1 (negative fixnums) are never in NaN space.
       (not (and (zerop (ldb (byte 1 63) v))    ; positive (bit 63 = 0)
                 (>= v +nan-tag-base+)))))

(defun val-double-p (v)
  "True if V encodes an IEEE 754 double (not in our NaN-tagged space)."
  (declare (type (unsigned-byte 64) v))
  ;; A value is a double if it is NOT in our special NaN space.
  ;; Our NaN space: bits[62:51] = #x7FF and bit[50..49] != 00
  ;; (fixnums have low bits clear so they escape this).
  ;; Practical check: v >= +nan-tag-base+ and NOT fixnum and NOT char/ptr/special.
  (and (not (val-fixnum-p v))
       (not (val-sso-string-p v))
       (< v +nan-tag-base+)))

(defun val-pointer-p (v)
  "True if V is any pointer-tagged value (object/cons/symbol/function/string).
   Pointer upper-16-bits range: #x7FF9 (object/tag=001) to #x7FFD (string/tag=101)."
  ;; Extract upper 16 bits and check they fall in the pointer sub-tag range.
  (and (typep v '(unsigned-byte 64))
       (let ((h (ash v -48)))
         (declare (type (unsigned-byte 16) h))
         (and (>= h #x7FF9) (<= h #x7FFD)))))

(defun val-compressed-pointer-p (v)
  "True if V is a pointer-tagged value carrying a 32-bit heap-relative offset."
  (declare (type (unsigned-byte 64) v))
  (and (val-pointer-p v)
       (not (zerop (logand v +compressed-pointer-flag+)))))

(declaim (inline %val-ptr-tag))
(defun %val-ptr-tag (v)
  "Extract the 3-bit sub-tag from a pointer value."
  (declare (type (unsigned-byte 64) v))
  (logand v +tag-mask+))

(defun val-object-p (v)
  (declare (type (unsigned-byte 64) v))
  (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-object+)))

(defun val-cons-p (v)
  (declare (type (unsigned-byte 64) v))
  (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-cons+)))

(defun val-symbol-p (v)
  (declare (type (unsigned-byte 64) v))
  (or (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-symbol+))
      (val-immediate-symbol-p v)))

(defun val-immediate-symbol-p (v)
  "True if V is one of the frequent-symbol immediate values."
  (declare (type (unsigned-byte 64) v))
  (= (logand v +immediate-symbol-mask+) +immediate-symbol-base+))

(defun encode-immediate-symbol-index (index)
  "Encode immediate symbol table INDEX as a NaN-boxed immediate value."
  (declare (type (unsigned-byte 8) index))
  (logand (logior +immediate-symbol-base+ index) +max-u64+))

(defun immediate-symbol-index (v)
  "Return the immediate symbol table index carried by V."
  (declare (type (unsigned-byte 64) v))
  (logand v +immediate-symbol-index-mask+))

(defun decode-immediate-symbol (v)
  "Decode an immediate symbol value to its host CL symbol."
  (declare (type (unsigned-byte 64) v))
  (unless (val-immediate-symbol-p v)
    (error "decode-immediate-symbol: not an immediate symbol #x~16,'0X" v))
  (svref *immediate-symbol-table* (immediate-symbol-index v)))

(defun immediate-symbol-value (symbol)
  "Return SYMBOL's immediate encoding, or NIL when SYMBOL is heap-backed."
  (let ((index (and (symbolp symbol) (gethash symbol *immediate-symbol-indexes*))))
    (and index (encode-immediate-symbol-index index))))

(defun val-function-p (v)
  (declare (type (unsigned-byte 64) v))
  (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-function+)))

(defun val-string-p (v)
  (declare (type (unsigned-byte 64) v))
  (or (val-sso-string-p v)
      (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-string+))))

(defun %sso-string-byte-p (char)
  "True if CHAR can be stored as one inline byte."
  (declare (type character char))
  (<= (char-code char) #xFF))

(defun encode-sso-string (string)
  "Encode STRING as an inline small-string immediate.

STRING must contain at most seven one-byte characters.  Longer strings or
characters with codes above #xFF must use the heap string representation."
  (check-type string string)
  (let ((length (length string)))
    (unless (<= length +sso-string-max-length+)
      (error "encode-sso-string: string length ~D exceeds SSO limit ~D"
             length +sso-string-max-length+))
    (let ((payload 0))
      (loop for i from 0 below length
            for char = (char string i)
            for code = (char-code char)
            do (unless (%sso-string-byte-p char)
                 (error "encode-sso-string: non-byte character ~S at index ~D"
                        char i))
               (setf payload (logand (logior payload (ash code (+ 3 (* 8 i))))
                                     +max-u64+)))
      (logand (logior +sso-string-base+
                      length
                      payload)
              +max-u64+))))

(defun decode-sso-string (value)
  "Decode an inline small-string immediate VALUE into a host CL string."
  (declare (type (unsigned-byte 64) value))
  (unless (val-sso-string-p value)
    (error "decode-sso-string: not an SSO string #x~16,'0X" value))
  (let* ((length (logand value +sso-string-length-mask+))
         (string (make-string length)))
    (loop for i from 0 below length
          do (setf (char string i)
                   (code-char (ldb (byte 8 (+ 3 (* 8 i))) value))))
    string))

(export '(+sso-string-base+ +sso-string-mask+ +sso-string-max-length+
          val-sso-string-p encode-sso-string decode-sso-string)
        :cl-cc/runtime)

(defun val-char-p (v)
  "True if V is a boxed character."
  (declare (type (unsigned-byte 64) v))
  (= (logand v #xFFFF000000000000) +tag-char+))

(defun val-nil-p (v)
  (declare (type (unsigned-byte 64) v))
  (= v +val-nil+))

(defun val-t-p (v)
  (declare (type (unsigned-byte 64) v))
  (= v +val-t+))

(defun val-unbound-p (v)
  (declare (type (unsigned-byte 64) v))
  (= v +val-unbound+))

;;; Encode/decode codecs (encode-fixnum, decode-fixnum, encode-double,
;;; decode-double, encode-pointer, decode-pointer, pointer-tag,
;;; encode-char, decode-char, encode-bool, cl-value->val, val->cl-value)
;;; are in value-codec.lisp (loaded next).

;;; ── Pinned arrays for FFI (FR-417) ───────────────────────────────────────

(defstruct (rt-pinned-unboxed-array-buffer (:constructor %make-pinned-buffer))
  (array nil :type (simple-array (unsigned-byte 8) (*)))
  (length 0 :type fixnum)
  (data-pointer nil)
  (released-p nil))

(defun rt-pin-unboxed-array (array)
  "Pin ARRAY (a SIMPLE-ARRAY of (UNSIGNED-BYTE 8)) for FFI access.
Returns an RT-PINNED-UNBOXED-ARRAY-BUFFER holding a stable data pointer."
  (check-type array (simple-array (unsigned-byte 8) (*)))
  (let ((buf (%make-pinned-buffer :array array
                                   :length (length array)
                                   :data-pointer (sb-sys:vector-sap array)
                                   :released-p nil)))
    buf))

(defun rt-release-pinned-array (buffer)
  "Release the pinned array BUFFER. After release, data pointer is no longer valid."
  (check-type buffer rt-pinned-unboxed-array-buffer)
  (setf (rt-pinned-unboxed-array-buffer-released-p buffer) t
        (rt-pinned-unboxed-array-buffer-data-pointer buffer) nil)
  (values))

(defun rt-pinned-array-data-pointer (buffer)
  "Return the stable data pointer of BUFFER while it is pinned.
Signals an error if the buffer has been released."
  (check-type buffer rt-pinned-unboxed-array-buffer)
  (when (rt-pinned-unboxed-array-buffer-released-p buffer)
    (error "Pinned array buffer has already been released."))
  (or (rt-pinned-unboxed-array-buffer-data-pointer buffer)
      (error "Pinned array data pointer is unavailable on this implementation.")))
