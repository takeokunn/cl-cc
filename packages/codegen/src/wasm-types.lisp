;;;; packages/emit/src/wasm-types.lisp - WASM type code constants and GC type definitions
;;;
;;; Defines WASM binary encoding constants and the GC proposal type hierarchy
;;; for representing Lisp values as WASM GC structs.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 1: WASM Binary Format Constants
;;; ─────────────────────────────────────────────────────────────────────────────

;;; WASM section IDs
(defconstant +wasm-section-custom+     0)
(defconstant +wasm-section-type+       1)
(defconstant +wasm-section-import+     2)
(defconstant +wasm-section-function+   3)
(defconstant +wasm-section-table+      4)
(defconstant +wasm-section-memory+     5)
(defconstant +wasm-section-global+     6)
(defconstant +wasm-section-export+     7)
(defconstant +wasm-section-start+      8)
(defconstant +wasm-section-element+    9)
(defconstant +wasm-section-code+      10)
(defconstant +wasm-section-data+      11)
(defconstant +wasm-section-data-count+ 12)
;;; Exception Handling proposal tag section.
(defconstant +wasm-section-tag+      13)

;;; WASM primitive value type encodings
(defconstant +wasm-i32+     #x7f)
(defconstant +wasm-i64+     #x7e)
(defconstant +wasm-f32+     #x7d)
(defconstant +wasm-f64+     #x7c)
(defconstant +wasm-funcref+ #x70)
(defconstant +wasm-externref+ #x6f)
;;; GC proposal reference types
(defconstant +wasm-anyref+    #x6e)
(defconstant +wasm-eqref+     #x6d)
(defconstant +wasm-i31ref+    #x6c)
(defconstant +wasm-structref+ #x6b)
(defconstant +wasm-arrayref+  #x6a)
(defconstant +wasm-nullref+   #x69)
(defconstant +wasm-nullfuncref+ #x73)
(defconstant +wasm-nullexternref+ #x72)

;;; Heap type encodings (for ref types in GC proposal)
(defconstant +heap-func+    #x70)
(defconstant +heap-extern+  #x6f)
(defconstant +heap-any+     #x6e)
(defconstant +heap-eq+      #x6d)
(defconstant +heap-i31+     #x6c)
(defconstant +heap-struct+  #x6b)
(defconstant +heap-array+   #x6a)
(defconstant +heap-none+    #x69)
(defconstant +heap-nofunc+  #x73)
(defconstant +heap-noextern+ #x72)

;;; GC type definition encodings (in type section)
(defconstant +wasm-type-func+      #x60)  ; function type
(defconstant +wasm-type-struct+    #x5f)  ; struct type (GC proposal)
(defconstant +wasm-type-array+     #x5e)  ; array type (GC proposal)
(defconstant +wasm-type-sub+       #x50)  ; sub type
(defconstant +wasm-type-sub-final+ #x4f)  ; final sub type
(defconstant +wasm-type-rec+       #x4e)  ; recursive type group

;;; Field packed type encodings
(defconstant +wasm-field-i8+  #x78)
(defconstant +wasm-field-i16+ #x77)

;;; Mutability
(defconstant +wasm-immutable+ 0)
(defconstant +wasm-mutable+   1)

;;; Export/import descriptor types
(defconstant +wasm-export-func+   0)
(defconstant +wasm-export-table+  1)
(defconstant +wasm-export-memory+ 2)
(defconstant +wasm-export-global+ 3)
(defconstant +wasm-import-func+   0)
(defconstant +wasm-import-table+  1)
(defconstant +wasm-import-memory+ 2)
(defconstant +wasm-import-global+ 3)

;;; Core opcodes
(defconstant +wasm-unreachable+   #x00)
(defconstant +wasm-nop+           #x01)
(defconstant +wasm-block+         #x02)
(defconstant +wasm-loop+          #x03)
(defconstant +wasm-if+            #x04)
(defconstant +wasm-else+          #x05)
(defconstant +wasm-try+           #x06)  ; exceptions proposal
(defconstant +wasm-catch+         #x07)  ; exceptions proposal
(defconstant +wasm-throw+         #x08)  ; exceptions proposal
(defconstant +wasm-end+           #x0b)
(defconstant +wasm-br+            #x0c)
(defconstant +wasm-br-if+         #x0d)
(defconstant +wasm-br-table+      #x0e)
(defconstant +wasm-return+        #x0f)
(defconstant +wasm-call+          #x10)
(defconstant +wasm-call-indirect+ #x11)
;;; GC typed function-reference call: opcode 0x14 followed by typeidx LEB128.
;;; Unlike call_indirect, this consumes a typed funcref from the stack and does
;;; not take a table index.
(defconstant +wasm-call-ref+      #x14)
(defconstant +wasm-drop+          #x1a)
(defconstant +wasm-select+        #x1b)
(defconstant +wasm-local-get+     #x20)
(defconstant +wasm-local-set+     #x21)
(defconstant +wasm-local-tee+     #x22)
(defconstant +wasm-global-get+    #x23)
(defconstant +wasm-global-set+    #x24)
(defconstant +wasm-i32-const+     #x41)
(defconstant +wasm-i64-const+     #x42)
(defconstant +wasm-f64-const+     #x44)
;;; i32 comparison opcodes
(defconstant +wasm-i32-eqz+  #x45)
(defconstant +wasm-i32-eq+   #x46)
(defconstant +wasm-i32-ne+   #x47)
(defconstant +wasm-i32-lt-s+ #x48)
(defconstant +wasm-i32-gt-s+ #x4a)
(defconstant +wasm-i32-le-s+ #x4c)
(defconstant +wasm-i32-ge-s+ #x4e)
;;; i64 comparison opcodes
(defconstant +wasm-i64-eqz+  #x50)
(defconstant +wasm-i64-eq+   #x51)
(defconstant +wasm-i64-ne+   #x52)
(defconstant +wasm-i64-lt-s+ #x53)
(defconstant +wasm-i64-gt-s+ #x55)
(defconstant +wasm-i64-le-s+ #x57)
(defconstant +wasm-i64-ge-s+ #x59)
;;; i64 arithmetic opcodes
(defconstant +wasm-i64-clz+   #x79)
(defconstant +wasm-i64-ctz+   #x7a)
(defconstant +wasm-i64-popcnt+ #x7b)
(defconstant +wasm-i64-add+   #x7c)
(defconstant +wasm-i64-sub+   #x7d)
(defconstant +wasm-i64-mul+   #x7e)
(defconstant +wasm-i64-div-s+ #x7f)
(defconstant +wasm-i64-div-u+ #x80)
(defconstant +wasm-i64-rem-s+ #x81)
(defconstant +wasm-i64-rem-u+ #x82)
(defconstant +wasm-i64-and+   #x83)
(defconstant +wasm-i64-or+    #x84)
(defconstant +wasm-i64-xor+   #x85)
(defconstant +wasm-i64-shl+   #x86)
(defconstant +wasm-i64-shr-s+ #x87)
(defconstant +wasm-i64-shr-u+ #x88)
(defconstant +wasm-i64-rotl+  #x89)
(defconstant +wasm-i64-rotr+  #x8a)
;;; f64 opcodes
(defconstant +wasm-f64-abs+   #x99)
(defconstant +wasm-f64-neg+   #x9a)
(defconstant +wasm-f64-ceil+  #x9b)
(defconstant +wasm-f64-floor+ #x9c)
(defconstant +wasm-f64-trunc+ #x9d)
(defconstant +wasm-f64-nearest+ #x9e)
(defconstant +wasm-f64-sqrt+  #x9f)
(defconstant +wasm-f64-add+   #xa0)
(defconstant +wasm-f64-sub+   #xa1)
(defconstant +wasm-f64-mul+   #xa2)
(defconstant +wasm-f64-div+   #xa3)
(defconstant +wasm-f64-min+   #xa4)
(defconstant +wasm-f64-max+   #xa5)
(defconstant +wasm-f64-copysign+ #xa6)  ; FR-324 copysign
;;; f32 opcodes (FR-324)
(defconstant +wasm-f32-abs+   #x8b)
(defconstant +wasm-f32-neg+   #x8c)
(defconstant +wasm-f32-ceil+  #x8d)
(defconstant +wasm-f32-floor+ #x8e)
(defconstant +wasm-f32-trunc+ #x8f)
(defconstant +wasm-f32-nearest+ #x90)
(defconstant +wasm-f32-sqrt+  #x91)
(defconstant +wasm-f32-add+   #x92)
(defconstant +wasm-f32-sub+   #x93)
(defconstant +wasm-f32-mul+   #x94)
(defconstant +wasm-f32-div+   #x95)
(defconstant +wasm-f32-min+   #x96)
(defconstant +wasm-f32-max+   #x97)
(defconstant +wasm-f32-copysign+ #x98)  ; FR-324
;;; i32 bit count ops (FR-323)
(defconstant +wasm-i32-clz+   #x67)
(defconstant +wasm-i32-ctz+   #x68)
(defconstant +wasm-i32-popcnt+ #x69)
;;; Conversion opcodes
(defconstant +wasm-i32-wrap-i64+       #xa7)
(defconstant +wasm-i64-extend-i32-s+   #xac)
(defconstant +wasm-f64-convert-i64-s+  #xb9)
(defconstant +wasm-i64-trunc-f64-s+    #xb0)
;;; Misc opcode prefix (0xfc — saturating trunc, bulk-memory, strings, wide-arith, f16)
(defconstant +wasm-misc-prefix+ #xfc)
;;; Non-trapping float-to-int conversions (MVP v1.1, FR-233) — secondary bytes after +wasm-misc-prefix+
(defconstant +wasm-i32-trunc-sat-f32-s+ 0)
(defconstant +wasm-i32-trunc-sat-f32-u+ 1)
(defconstant +wasm-i32-trunc-sat-f64-s+ 2)
(defconstant +wasm-i32-trunc-sat-f64-u+ 3)
(defconstant +wasm-i64-trunc-sat-f32-s+ 4)
(defconstant +wasm-i64-trunc-sat-f32-u+ 5)
(defconstant +wasm-i64-trunc-sat-f64-s+ 6)
(defconstant +wasm-i64-trunc-sat-f64-u+ 7)
;;; Sign-extension operators (MVP v1.1, FR-234)
(defconstant +wasm-i32-extend8-s+  #xc0)
(defconstant +wasm-i32-extend16-s+ #xc1)
(defconstant +wasm-i64-extend8-s+  #xc2)
(defconstant +wasm-i64-extend16-s+ #xc3)
(defconstant +wasm-i64-extend32-s+ #xc4)
;;; Reference opcodes
(defconstant +wasm-ref-null+    #xd0)
(defconstant +wasm-ref-is-null+ #xd1)
(defconstant +wasm-ref-func+    #xd2)
(defconstant +wasm-ref-eq+      #xd3)  ; GC proposal
(defconstant +wasm-ref-as-non-null+ #xd4)  ; GC proposal null safety
(defconstant +wasm-br-on-null+  #xd5)  ; GC proposal null safety
(defconstant +wasm-br-on-non-null+ #xd6) ; GC proposal null safety
;;; Tail-call proposal (FR-143)
(defconstant +wasm-return-call+          #x12)
(defconstant +wasm-return-call-indirect+ #x13)
;;; Bulk memory operations (MVP v1.1, FR-228) — secondary bytes after +wasm-misc-prefix+
(defconstant +wasm-memory-copy+  10)
(defconstant +wasm-memory-fill+  11)
(defconstant +wasm-memory-init+  8)
(defconstant +wasm-data-drop+    9)
;;; Bulk table operations (MVP v1.1, FR-237) — secondary bytes after +wasm-misc-prefix+
(defconstant +wasm-table-init+  12)
(defconstant +wasm-table-copy+  15)
(defconstant +wasm-table-fill+  17)
(defconstant +wasm-elem-drop+   13)

;;; GC opcode prefix
(defconstant +wasm-gc-prefix+ #xfb)

;;; SIMD128 opcode prefix and representative SIMD opcodes (0xFD prefix space).
(defconstant +wasm-simd-prefix+ #xfd)
(defconstant +wasm-v128-load+ 0)
(defconstant +wasm-v128-store+ 11)
(defconstant +wasm-v128-const+ 12)
(defconstant +wasm-i8x16-add+ 110)
(defconstant +wasm-i16x8-add+ 128)
(defconstant +wasm-i32x4-add+ 174)
(defconstant +wasm-i64x2-add+ 192)
(defconstant +wasm-v128-and+ 78)
(defconstant +wasm-v128-or+ 80)
(defconstant +wasm-v128-xor+ 81)

;;; Relaxed SIMD opcodes in the 0xFD prefixed relaxed-SIMD range.
(defconstant +wasm-i8x16-relaxed-swizzle+ 128)
(defconstant +wasm-i32x4-relaxed-trunc-f32x4-s+ 129)
(defconstant +wasm-i32x4-relaxed-trunc-f32x4-u+ 130)
(defconstant +wasm-i32x4-relaxed-trunc-f64x2-s-zero+ 131)
(defconstant +wasm-i32x4-relaxed-trunc-f64x2-u-zero+ 132)
(defconstant +wasm-f32x4-relaxed-madd+ 133)
(defconstant +wasm-f32x4-relaxed-nmadd+ 134)
(defconstant +wasm-f64x2-relaxed-madd+ 135)
(defconstant +wasm-f64x2-relaxed-nmadd+ 136)
(defconstant +wasm-i8x16-relaxed-lane-select+ 137)
(defconstant +wasm-i16x8-relaxed-lane-select+ 138)
(defconstant +wasm-i32x4-relaxed-lane-select+ 139)
(defconstant +wasm-i64x2-relaxed-lane-select+ 140)
(defconstant +wasm-f32x4-relaxed-min+ 141)
(defconstant +wasm-f32x4-relaxed-max+ 142)
(defconstant +wasm-f64x2-relaxed-min+ 143)
(defconstant +wasm-f64x2-relaxed-max+ 144)

(defun wasm-encode-simd-op (simd-opcode)
  "Encode a prefixed SIMD/relaxed-SIMD opcode."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (vector +wasm-simd-prefix+)
               (wasm-encode-u32-leb128 simd-opcode)))

;;; GC opcodes (following +wasm-gc-prefix+)
(defconstant +wasm-gc-struct-new+         0)
(defconstant +wasm-gc-struct-new-default+ 1)
(defconstant +wasm-gc-struct-get+         2)
(defconstant +wasm-gc-struct-get-s+       3)
(defconstant +wasm-gc-struct-get-u+       4)
(defconstant +wasm-gc-struct-set+         5)
;;; FR-297 targets the GC proposal encoding used by cl-cc's backend plan:
;;; array.new is emitted as 0xfb 0x1b followed by typeidx.
(defconstant +wasm-gc-array-new+          6)
(defconstant +wasm-gc-array-new-default+  7)
(defconstant +wasm-gc-array-new-fixed+    8)
(defconstant +wasm-gc-array-get+         11)
(defconstant +wasm-gc-array-get-s+       12)
(defconstant +wasm-gc-array-get-u+       13)
(defconstant +wasm-gc-array-set+         14)
(defconstant +wasm-gc-array-len+         15)
(defconstant +wasm-gc-ref-test+          20)
(defconstant +wasm-gc-ref-cast+          22)
(defconstant +wasm-gc-ref-i31+           28)
(defconstant +wasm-gc-i31-get-s+         29)
(defconstant +wasm-gc-i31-get-u+         30)
(defconstant +wasm-gc-br-on-cast+        31)
(defconstant +wasm-gc-br-on-cast-fail+   32)
(defconstant +wasm-gc-any-convert-extern+ 33)
(defconstant +wasm-gc-extern-convert-any+ 34)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 2: Named GC Type Indices
;;; These indices match the ORDER of type definitions emitted in the WASM type section.
;;; Every module emitted by cl-cc starts with these predefined types.
;;; FR-209: fixnums use the built-in Wasm GC i31ref immediate type.  The logical
;;; name $fixnum_t is documented in the WAT type section as an alias to i31ref;
;;; it has no numeric type-index because it is not heap allocated.
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Type 0: main function type () -> (eqref)
(defconstant +type-idx-main-func+    0)
;;; Type 1: bytes array type (array i8, for strings/symbols)
(defconstant +type-idx-bytes-array+  1)
;;; Type 2: $string struct { chars: (ref +type-idx-bytes-array+) }
(defconstant +type-idx-string+       2)
;;; Type 3: $symbol struct { name: (ref +type-idx-string+), plist: eqref }
(defconstant +type-idx-symbol+       3)
;;; Type 4: $cons struct { car: eqref, cdr: eqref }
(defconstant +type-idx-cons+         4)
;;; Type 5: $eqref-array array (eqref) — used for slot arrays, env vars, mv-buffer, etc.
(defconstant +type-idx-eqref-array+  5)
;;; Type 6: $env struct { vars: (ref +type-idx-eqref-array+), parent: (ref null $env) }
;;; Note: recursive type — parent refs env itself
(defconstant +type-idx-env+          6)
;;; Type 7: $closure struct { entry: i32, env: (ref null $env) }
(defconstant +type-idx-closure+      7)
;;; Type 8: $class-meta struct { name: (ref $symbol), slot-names: (ref $eqref-array) }
(defconstant +type-idx-class-meta+   8)
;;; Type 9: $instance struct { class: (ref $class-meta), slots: (ref $eqref-array) }
(defconstant +type-idx-instance+     9)
;;; Type 10: $htable struct { keys: (ref $eqref-array), vals: (ref $eqref-array), count: i32 }
(defconstant +type-idx-htable+      10)
;;; Type 11: $float struct { val: f64 } — boxed floating-point value
(defconstant +type-idx-float+       11)
;;; Type 12: $char struct { code: i32 } — character
(defconstant +type-idx-char+        12)
;;; Type 13: $fixnum_array_t array (mut i64) — specialized unboxed fixnum vector
(defconstant +type-idx-fixnum-array+ 13)
;;; Type 14: $float_array_t array (mut f64) — specialized unboxed float vector
(defconstant +type-idx-float-array+ 14)
;;; Type 15: $char_array_t array (mut i32) — specialized character-code vector
(defconstant +type-idx-char-array+ 15)
;;; Number of historical predefined core types.  FR-211 specialized array type
;;; indices are fixed above but kept outside this legacy count for compatibility
;;; with tests and binary metadata that treat 0..12 as the stable core prefix.
(defconstant +num-predefined-types+ 13)

;;; Exception tag indices emitted by the WAT backend.
(defconstant +tag-idx-cl-condition+ 0)

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

;;; Sentinel value for "no type" / void
(defconstant +wasm-void+ #x40)  ; used as block type for empty results

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-213: Memory64 opcodes
;;; ─────────────────────────────────────────────────────────────────────────────
(defconstant +wasm-memory-size64+  30)
(defconstant +wasm-memory-grow64+  31)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-247: Frozen values (immutable GC types)
;;; ─────────────────────────────────────────────────────────────────────────────
(defconstant +wasm-gc-struct-new-immutable+  16)  ; 0xfb 0x10
(defconstant +wasm-gc-array-new-immutable+    9)  ; 0xfb 0x09
;;; FR-249: More Array Constructors
(defconstant +wasm-gc-array-new-data+       10)  ; 0xfb 0x0a
(defconstant +wasm-gc-array-new-elem+       17)  ; 0xfb 0x11
(defconstant +wasm-gc-array-fill+           18)  ; 0xfb 0x12
;;; FR-250: Multibyte Array Access
(defconstant +wasm-gc-array-load2-u+        19)  ; 0xfb 0x13
(defconstant +wasm-gc-array-load4-u+        20)  ; 0xfb 0x14
(defconstant +wasm-gc-array-store2+         21)  ; 0xfb 0x15
(defconstant +wasm-gc-array-store4+         22)  ; 0xfb 0x16
;;; FR-283: GC packed fields
(defconstant +wasm-gc-struct-get-s-packed+  23)  ; 0xfb 0x17
(defconstant +wasm-gc-struct-get-u-packed+  24)  ; 0xfb 0x18
(defconstant +wasm-gc-struct-set-packed+    25)  ; 0xfb 0x19
;;; FR-279: Typed select for refs
(defconstant +wasm-select-typed+            #x1c)
;;; FR-275/FR-290: func.bind
(defconstant +wasm-func-bind+           33)
;;; FR-310: Exception tag import/export (EH v2)
(defconstant +wasm-try-table+           #x1f)
(defconstant +wasm-throw-ref+           #x0a)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-205: Stack Switching opcodes
;;; ─────────────────────────────────────────────────────────────────────────────
(defconstant +wasm-cont-new+            #xe0)
(defconstant +wasm-cont-bind+           #xe1)
(defconstant +wasm-suspend+             #xe2)
(defconstant +wasm-resume+              #xe3)
(defconstant +wasm-resume-throw+        #xe4)
(defconstant +wasm-cont-throw+          #xe5)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-218, FR-295: String Builtins opcodes
;;; ─────────────────────────────────────────────────────────────────────────────
(defconstant +wasm-string-new-utf8+     80)
(defconstant +wasm-string-new-wtf8+     81)
(defconstant +wasm-string-new-lossy-utf8+ 82)
(defconstant +wasm-string-encode-wtf16+ 83)
(defconstant +wasm-string-encode-utf8+  84)
(defconstant +wasm-string-encode-wtf8+  85)
(defconstant +wasm-string-measure-wtf16+ 86)
(defconstant +wasm-string-measure-utf8+  87)
(defconstant +wasm-string-measure-wtf8+  88)
(defconstant +wasm-string-concat+        89)
(defconstant +wasm-string-compare+       90)
(defconstant +wasm-string-from-code-point+ 91)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-238: Wide Arithmetic (128-bit) opcodes
;;; ─────────────────────────────────────────────────────────────────────────────
(defconstant +wasm-i64-add128+          19)
(defconstant +wasm-i64-sub128+          20)
(defconstant +wasm-i64-mul-wide-s+      21)
(defconstant +wasm-i64-mul-wide-u+      22)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-248: Half Precision (f16) opcodes
;;; ─────────────────────────────────────────────────────────────────────────────
(defconstant +wasm-f16-load+            50)
(defconstant +wasm-f16-store+           51)
(defconstant +wasm-f16-add+             52)
(defconstant +wasm-f16-sub+             53)
(defconstant +wasm-f16-mul+             54)
(defconstant +wasm-f16-div+             55)
(defconstant +wasm-f16-min+             56)
(defconstant +wasm-f16-max+             57)

;;; Fixnum tag convention: fixnums are stored as i31ref
;;; i31ref can hold 31-bit signed integers directly without heap allocation
;;; NIL is represented as ref.null (eqref)
;;; T is represented as i31ref(1)
;;; Booleans generally: NIL = ref.null, non-NIL = any non-null ref or i31ref

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR coverage: remaining FRs now have feature flags + opcode references
;;; See wasm-features.lisp for all *wasm-*-enabled* flags
;;;
;;; FR-264: Lazy function bodies — deferred compilation (feature flag)
;;; FR-268: Wasm Decimal — IEEE 754 decimal floating point (feature flag)
;;; FR-269: call_stack inspection — stack introspection (feature flag)
;;; FR-272: Algebraic Effect Handlers — CL condition system (feature flag)
;;; FR-274: WASI Worlds — wasi:nn/http/cli (feature flag)
;;; FR-277: CL ABI / symbol mangling — cross-language FFI (feature flag)
;;; FR-280: __wasm_call_ctors init ordering (feature flag)
;;; FR-282: Abstract Types — opaque host objects (feature flag)
;;; FR-292: call_indirect × table64 integration (feature flag)
;;; FR-296: WASI Extended Worlds — keyvalue/messaging/sql (feature flag)
;;; FR-299: WASI random/crypto — secure RNG (feature flag)
;;; FR-300: Multi-memory Bulk Copy (feature flag)
;;; FR-305: WebAssembly.validate() static API (feature flag)
;;; FR-308: Relaxed Dead Code Validation (feature flag)
;;; FR-309: Rounding Variants — f64.nearest_int (feature flag, f64.nearest opcode at #x9e)
;;; FR-311: wasm-c-api — native C embedding (feature flag)
;;; FR-312: Runtime feature detection (feature flag)
;;; FR-313: v128.load32/load64_zero — SIMD zero-extending loads (feature flag)
;;; FR-314: Relaxed SIMD integer ops — Q15/INT8 dot/BF16 (feature flag, relaxed SIMD range)
;;; FR-315: catch_all_ref — catch all exceptions as exnref (feature flag, +wasm-try-table+)
;;; FR-316: JS Primitive Builtins — numeric conversion (feature flag)
;;; FR-319: Component Model tests — WIT interface (feature flag)
;;; FR-322: Binary tool integration — wat2wasm/wasm-objdump (feature flag)
;;; FR-325: SIMD NaN semantics documented (feature flag)
;;; FR-327: Sub-word Atomics — 8/16bit CAS/rmw (feature flag)
