(in-package :cl-cc/codegen)

;;;; packages/emit/src/wasm-types.lisp - WASM type encoding helpers

(defun wasm-encode-simd-op (simd-opcode)
  "Encode a prefixed SIMD/relaxed-SIMD opcode."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (vector +wasm-simd-prefix+)
               (wasm-encode-u32-leb128 simd-opcode)))

(defun wasm-encode-u32-leb128 (value)
  "Return VALUE encoded as unsigned LEB128 bytes."
  (check-type value (integer 0 *))
  (let ((bytes nil)
        (n value))
    (loop
      (let ((byte (logand n #x7f)))
        (setf n (ash n -7))
        (when (plusp n)
          (setf byte (logior byte #x80)))
        (push byte bytes)
        (unless (plusp n)
          (return (coerce (nreverse bytes)
                          '(simple-array (unsigned-byte 8) (*)))))))))

(defun wasm-encode-op-u32 (opcode index)
  "Return OPCODE followed by unsigned LEB128 INDEX."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (vector opcode)
               (wasm-encode-u32-leb128 index)))

(defun wasm-encode-global-get (globalidx)
  "Encode global.get GLOBALIDX: 0x23 + LEB128(globalidx)."
  (wasm-encode-op-u32 +wasm-global-get+ globalidx))

(defun wasm-encode-global-set (globalidx)
  "Encode global.set GLOBALIDX: 0x24 + LEB128(globalidx)."
  (wasm-encode-op-u32 +wasm-global-set+ globalidx))

(defun wasm-encode-call (funcidx)
  "Encode call FUNCIDX: 0x10 + LEB128(funcidx)."
  (wasm-encode-op-u32 +wasm-call+ funcidx))

(defun wasm-encode-call-indirect (typeidx tableidx)
  "Encode call_indirect TYPEIDX TABLEIDX."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (vector +wasm-call-indirect+)
               (wasm-encode-u32-leb128 typeidx)
               (wasm-encode-u32-leb128 tableidx)))

(defun wasm-encode-gc-op-u32 (gc-opcode typeidx)
  "Return GC prefix, GC-OPCODE, and unsigned LEB128 TYPEIDX."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (vector +wasm-gc-prefix+ gc-opcode)
               (wasm-encode-u32-leb128 typeidx)))

(defun wasm-encode-struct-new (typeidx)
  "Encode struct.new TYPEIDX: 0xfb 0x00 + LEB128(typeidx)."
  (wasm-encode-gc-op-u32 +wasm-gc-struct-new+ typeidx))

(defun wasm-encode-array-new (typeidx)
  "Encode array.new TYPEIDX: 0xfb 0x06 + LEB128(typeidx)."
  (wasm-encode-gc-op-u32 +wasm-gc-array-new+ typeidx))

(defun wasm-encode-try (tag-type)
  "Encode try TAG-TYPE: 0x06 + block/tag type byte."
  (vector +wasm-try+ tag-type))

(defun wasm-encode-catch (tagidx)
  "Encode catch TAGIDX: 0x07 + LEB128(tagidx)."
  (wasm-encode-op-u32 +wasm-catch+ tagidx))

(defun wasm-encode-throw (tagidx)
  "Encode throw TAGIDX: 0x08 + LEB128(tagidx)."
  (wasm-encode-op-u32 +wasm-throw+ tagidx))
