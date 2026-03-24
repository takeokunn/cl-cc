;;;; src/backend/wasm-types.lisp - WASM type code constants and GC type definitions
;;;
;;; Defines WASM binary encoding constants and the GC proposal type hierarchy
;;; for representing Lisp values as WASM GC structs.

(in-package :cl-cc)

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
;;; Conversion opcodes
(defconstant +wasm-i32-wrap-i64+       #xa7)
(defconstant +wasm-i64-extend-i32-s+   #xac)
(defconstant +wasm-f64-convert-i64-s+  #xb9)
(defconstant +wasm-i64-trunc-f64-s+    #xb0)
;;; Reference opcodes
(defconstant +wasm-ref-null+    #xd0)
(defconstant +wasm-ref-is-null+ #xd1)
(defconstant +wasm-ref-func+    #xd2)
(defconstant +wasm-ref-eq+      #xd3)  ; GC proposal

;;; GC opcode prefix
(defconstant +wasm-gc-prefix+ #xfb)

;;; GC opcodes (following +wasm-gc-prefix+)
(defconstant +wasm-gc-struct-new+         0)
(defconstant +wasm-gc-struct-new-default+ 1)
(defconstant +wasm-gc-struct-get+         2)
(defconstant +wasm-gc-struct-get-s+       3)
(defconstant +wasm-gc-struct-get-u+       4)
(defconstant +wasm-gc-struct-set+         5)
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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 2: Named GC Type Indices
;;; These indices match the ORDER of type definitions emitted in the WASM type section.
;;; Every module emitted by cl-cc starts with these predefined types.
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
;;; Number of predefined types
(defconstant +num-predefined-types+ 13)

;;; Sentinel value for "no type" / void
(defconstant +wasm-void+ #x40)  ; used as block type for empty results

;;; Fixnum tag convention: fixnums are stored as i31ref
;;; i31ref can hold 31-bit signed integers directly without heap allocation
;;; NIL is represented as ref.null (eqref)
;;; T is represented as i31ref(1)
;;; Booleans generally: NIL = ref.null, non-NIL = any non-null ref or i31ref
