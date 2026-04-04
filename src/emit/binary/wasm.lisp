;;;; src/binary/wasm.lisp - WASM Binary Format Encoder
;;;
;;; Implements the WebAssembly binary format encoder for cl-cc.
;;; Supports the GC proposal (struct/array types), LEB128 encoding,
;;; and incremental section building via wasm-binary-builder.
;;;
;;; Reference: https://webassembly.github.io/spec/core/binary/index.html

(in-package :cl-cc/binary)

;;; ------------------------------------------------------------
;;; Section 1: LEB128 Encoding
;;; ------------------------------------------------------------

(defun encode-uleb128 (value)
  "Encode VALUE as an unsigned LEB128 byte sequence. Returns a list of bytes."
  (let ((bytes nil))
    (loop
      (let ((byte (logand value #x7f)))
        (setf value (ash value -7))
        (if (zerop value)
            (progn (push byte bytes) (return))
            (push (logior byte #x80) bytes))))
    (nreverse bytes)))

(defun encode-sleb128 (value)
  "Encode VALUE as a signed LEB128 byte sequence. Returns a list of bytes."
  (let ((bytes nil)
        (more t))
    (loop while more do
      (let ((byte (logand value #x7f)))
        (setf value (ash value -7))
        (if (or (and (zerop value) (zerop (logand byte #x40)))
                (and (= value -1) (not (zerop (logand byte #x40)))))
            (setf more nil)
            (setf byte (logior byte #x80)))
        (push byte bytes)))
    (nreverse bytes)))

;;; ------------------------------------------------------------
;;; Section 2: Byte Buffer Helpers
;;; ------------------------------------------------------------

(defun make-wasm-buffer ()
  "Create a fresh WASM byte buffer (adjustable byte array with fill pointer)."
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun wasm-buf-write-byte (buf byte)
  "Append a single byte to BUF (an adjustable byte array with fill-pointer)."
  (vector-push-extend byte buf))

(defun wasm-buf-write-bytes (buf bytes)
  "Append a sequence of bytes (list or vector) to BUF."
  (etypecase bytes
    (list (dolist (b bytes) (vector-push-extend b buf)))
    (vector (loop for b across bytes do (vector-push-extend b buf)))))

(defun wasm-buf-write-uleb128 (buf value)
  "Write VALUE as unsigned LEB128 into BUF."
  (wasm-buf-write-bytes buf (encode-uleb128 value)))

(defun wasm-buf-write-sleb128 (buf value)
  "Write VALUE as signed LEB128 into BUF."
  (wasm-buf-write-bytes buf (encode-sleb128 value)))

(defun wasm-buf-write-string-utf8 (buf str)
  "Write STR as a UTF-8 length-prefixed string (LEB128 length + bytes)."
  (let ((bytes (map 'list #'char-code str)))
    (wasm-buf-write-uleb128 buf (length bytes))
    (wasm-buf-write-bytes buf bytes)))

(defun portable-double-float-bits (value)
  "Return VALUE as an IEEE754 double bit pattern using portable CL operations."
  (let* ((x (float value 1.0d0))
         (sign-bit (if (minusp (float-sign x 1.0d0)) 1 0)))
    (cond
      ((zerop x)
       (ash sign-bit 63))
      ((not (= x x))
       ;; Quiet NaN with a minimal payload.
       (logior (ash sign-bit 63)
               (ash #x7ff 52)
               #x0008000000000000))
      ((and (= x (* x 2.0d0)) (not (zerop x)))
       ;; Infinity.
       (logior (ash sign-bit 63)
               (ash #x7ff 52)))
      (t
       (multiple-value-bind (significand exponent sign) (integer-decode-float x)
         (declare (ignore sign))
         (let* ((abs-significand (abs significand))
                (unbiased-exp (+ exponent 52))
                (fraction
                  (if (>= unbiased-exp -1022)
                      (- abs-significand (ash 1 52))
                      (ash abs-significand (+ exponent 1074))))
                (exp-field
                  (if (>= unbiased-exp -1022)
                      (+ unbiased-exp 1023)
                      0)))
           (logior (ash sign-bit 63)
                   (ash exp-field 52)
                   (logand fraction #x000fffffffffffff))))))))

(defun wasm-buf-write-f64 (buf value)
  "Write a 64-bit IEEE754 float VALUE into BUF (little-endian)."
  (let* ((bits (portable-double-float-bits value))
         (lo (logand bits #xffffffff))
         (hi (ash bits -32)))
    (loop for shift from 0 to 24 by 8
          do (wasm-buf-write-byte buf (logand (ash lo (- shift)) #xff)))
    (loop for shift from 0 to 24 by 8
          do (wasm-buf-write-byte buf (logand (ash hi (- shift)) #xff)))))

(defun wasm-buffer-to-array (buf)
  "Convert a WASM byte buffer to a simple (unsigned-byte 8) array."
  (make-array (length buf) :element-type '(unsigned-byte 8)
              :initial-contents buf))

;;; ------------------------------------------------------------
;;; Section 3: WASM Binary Format Constants
;;; ------------------------------------------------------------

;;; SBCL: defvar for list-valued "constants" (lists are not eql-comparable)
(defvar +wasm-magic-bytes+ '(0 #x61 #x73 #x6d))    ; "\0asm"
(defvar +wasm-version-bytes+ '(1 0 0 0))

;;; ------------------------------------------------------------
;;; Section 4: WASM Module Binary Builder
;;; ------------------------------------------------------------

(defstruct (wasm-binary-builder (:conc-name wasm-bin-))
  "Accumulates WASM module sections for binary serialization."
  ;; Each section is an adjustable byte buffer; nil means not yet started
  (type-buf nil)      ; type section payload
  (import-buf nil)    ; import section payload
  (func-buf nil)      ; function section payload (type indices)
  (table-buf nil)     ; table section payload
  (memory-buf nil)    ; memory section payload
  (global-buf nil)    ; global section payload
  (export-buf nil)    ; export section payload
  (element-buf nil)   ; element section payload
  (code-buf nil)      ; code section payload (function bodies)
  (data-buf nil)      ; data section payload
  ;; counts for vector headers
  (type-count 0 :type integer)
  (import-count 0 :type integer)
  (func-count 0 :type integer)
  (table-count 0 :type integer)
  (global-count 0 :type integer)
  (export-count 0 :type integer)
  (element-count 0 :type integer)
  (code-count 0 :type integer)
  (data-count 0 :type integer))

;;; ------------------------------------------------------------
;;; Section 5: Section Serialization Helper
;;; ------------------------------------------------------------

(defun wasm-write-section (out section-id payload-buf)
  "Write a WASM section with SECTION-ID and PAYLOAD-BUF content to OUT."
  (when (plusp (length payload-buf))
    (wasm-buf-write-byte out section-id)
    (wasm-buf-write-uleb128 out (length payload-buf))
    (wasm-buf-write-bytes out payload-buf)))

;;; ------------------------------------------------------------
;;; Section 6: Value Type Encoder
;;; ------------------------------------------------------------

(defun encode-valtype (type-spec buf)
  "Write the binary encoding of a WASM value type into BUF.
   TYPE-SPEC can be: :i32 :i64 :f32 :f64 :funcref :externref :anyref :eqref
   :i31ref :structref :arrayref, or (:ref N) or (:ref-null N) where N is a
   heap type index (signed LEB128)."
  (etypecase type-spec
    (keyword
     (case type-spec
       (:i32       (wasm-buf-write-byte buf #x7f))
       (:i64       (wasm-buf-write-byte buf #x7e))
       (:f32       (wasm-buf-write-byte buf #x7d))
       (:f64       (wasm-buf-write-byte buf #x7c))
       (:funcref   (wasm-buf-write-byte buf #x70))
       (:externref (wasm-buf-write-byte buf #x6f))
       (:anyref    (wasm-buf-write-byte buf #x6e))
       (:eqref     (wasm-buf-write-byte buf #x6d))
       (:i31ref    (wasm-buf-write-byte buf #x6c))
       (:structref (wasm-buf-write-byte buf #x6b))
       (:arrayref  (wasm-buf-write-byte buf #x6a))
       (otherwise  (error "Unknown WASM value type keyword: ~S" type-spec))))
    (list
     (case (first type-spec)
       (:ref
        ;; Non-nullable ref: 0x64 <heap-type> (GC proposal)
        (wasm-buf-write-byte buf #x64)
        (wasm-buf-write-sleb128 buf (second type-spec)))
       (:ref-null
        ;; Nullable ref: 0x63 <heap-type>
        (wasm-buf-write-byte buf #x63)
        (wasm-buf-write-sleb128 buf (second type-spec)))
       (otherwise (error "Unknown WASM type spec: ~S" type-spec))))))

;;; ------------------------------------------------------------
;;; Section 7: Type Section Encoding
;;; ------------------------------------------------------------

(defun encode-func-type (params results buf)
  "Encode a function type signature (0x60 params results) into BUF.
   PARAMS and RESULTS are lists of value type specs."
  (wasm-buf-write-byte buf #x60)
  (wasm-buf-write-uleb128 buf (length params))
  (dolist (p params) (encode-valtype p buf))
  (wasm-buf-write-uleb128 buf (length results))
  (dolist (r results) (encode-valtype r buf)))

(defun encode-struct-type (fields buf)
  "Encode a struct type (0x5f [fields]) into BUF.
   FIELDS is a list of (type-spec mutability) pairs where mutability is
   :immutable or :mutable."
  (wasm-buf-write-byte buf #x5f)
  (wasm-buf-write-uleb128 buf (length fields))
  (dolist (field fields)
    (destructuring-bind (type-spec mutability) field
      (encode-valtype type-spec buf)
      (wasm-buf-write-byte buf (if (eq mutability :immutable) 0 1)))))

(defun encode-array-type (element-type mutability buf)
  "Encode an array type (0x5e element-type mutability) into BUF."
  (wasm-buf-write-byte buf #x5e)
  (encode-valtype element-type buf)
  (wasm-buf-write-byte buf (if (eq mutability :immutable) 0 1)))

;;; ------------------------------------------------------------
;;; Section 8: Code Section - Local Variable Encoding
;;; ------------------------------------------------------------

(defun encode-locals (local-groups buf)
  "Encode function locals as compressed groups into BUF.
   LOCAL-GROUPS is a list of (count type-spec) pairs, e.g. ((3 :i32) (1 :f64))."
  (wasm-buf-write-uleb128 buf (length local-groups))
  (dolist (group local-groups)
    (destructuring-bind (count type-spec) group
      (wasm-buf-write-uleb128 buf count)
      (encode-valtype type-spec buf))))

;;; ------------------------------------------------------------
;;; Section 9: Module Finalization
;;; ------------------------------------------------------------

(defun wasm-finalize-module (builder)
  "Assemble the complete WASM binary module from BUILDER sections.
   Returns a (simple-array (unsigned-byte 8) (*)) suitable for writing to disk."
  (let ((out (make-wasm-buffer)))
    ;; Magic bytes + version
    (wasm-buf-write-bytes out +wasm-magic-bytes+)
    (wasm-buf-write-bytes out +wasm-version-bytes+)
    ;; Section 1: Type section
    (when (plusp (wasm-bin-type-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-type-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-type-buf builder))
        (wasm-write-section out 1 payload)))
    ;; Section 2: Import section
    (when (plusp (wasm-bin-import-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-import-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-import-buf builder))
        (wasm-write-section out 2 payload)))
    ;; Section 3: Function section (type indices)
    (when (plusp (wasm-bin-func-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-func-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-func-buf builder))
        (wasm-write-section out 3 payload)))
    ;; Section 4: Table section
    (when (plusp (wasm-bin-table-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-table-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-table-buf builder))
        (wasm-write-section out 4 payload)))
    ;; Section 5: Memory section (no count field in builder; emit raw buf if non-empty)
    (when (and (wasm-bin-memory-buf builder)
               (plusp (length (wasm-bin-memory-buf builder))))
      (wasm-write-section out 5 (wasm-bin-memory-buf builder)))
    ;; Section 6: Global section
    (when (plusp (wasm-bin-global-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-global-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-global-buf builder))
        (wasm-write-section out 6 payload)))
    ;; Section 7: Export section
    (when (plusp (wasm-bin-export-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-export-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-export-buf builder))
        (wasm-write-section out 7 payload)))
    ;; Section 9: Element section
    (when (plusp (wasm-bin-element-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-element-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-element-buf builder))
        (wasm-write-section out 9 payload)))
    ;; Section 10: Code section
    (when (plusp (wasm-bin-code-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-code-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-code-buf builder))
        (wasm-write-section out 10 payload)))
    ;; Section 11: Data section
    (when (plusp (wasm-bin-data-count builder))
      (let ((payload (make-wasm-buffer)))
        (wasm-buf-write-uleb128 payload (wasm-bin-data-count builder))
        (wasm-buf-write-bytes payload (wasm-bin-data-buf builder))
        (wasm-write-section out 11 payload)))
    (wasm-buffer-to-array out)))

(defun write-wasm-file (filename bytes)
  "Write BYTES (a (simple-array (unsigned-byte 8) (*))) to FILENAME."
  (with-open-file (out filename
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (write-sequence bytes out)))
