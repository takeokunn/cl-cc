;;;; tests/unit/emit/wasm-types-tests.lisp — WASM Type Constants Tests
;;;;
;;;; Tests for src/emit/wasm-types.lisp:
;;;; Section IDs, value type encodings, GC proposal types, heap types,
;;;; type definition encodings, opcodes, named type indices, and
;;;; uniqueness invariants.

(in-package :cl-cc/test)

(defsuite wasm-types-suite :description "WASM type constants and GC type index tests"
  :parent cl-cc-unit-suite)


(in-suite wasm-types-suite)
;;; ─── Section IDs ───────────────────────────────────────────────────────────

(deftest-each wasm-section-ids-range
  "Section IDs span 0-12 contiguously."
  :cases (("custom"     cl-cc/codegen::+wasm-section-custom+     0)
          ("type"       cl-cc/codegen::+wasm-section-type+       1)
          ("import"     cl-cc/codegen::+wasm-section-import+     2)
          ("function"   cl-cc/codegen::+wasm-section-function+   3)
          ("table"      cl-cc/codegen::+wasm-section-table+      4)
          ("memory"     cl-cc/codegen::+wasm-section-memory+     5)
          ("global"     cl-cc/codegen::+wasm-section-global+     6)
          ("export"     cl-cc/codegen::+wasm-section-export+     7)
          ("start"      cl-cc/codegen::+wasm-section-start+      8)
          ("element"    cl-cc/codegen::+wasm-section-element+    9)
          ("code"       cl-cc/codegen::+wasm-section-code+       10)
          ("data"       cl-cc/codegen::+wasm-section-data+       11)
          ("data-count" cl-cc/codegen::+wasm-section-data-count+ 12))
  (const expected)
  (assert-equal expected const))

;;; ─── Primitive value types ─────────────────────────────────────────────────

(deftest-each wasm-primitive-value-types
  "WASM primitive types match spec encodings."
  :cases (("i32"       cl-cc/codegen::+wasm-i32+       #x7f)
          ("i64"       cl-cc/codegen::+wasm-i64+       #x7e)
          ("f32"       cl-cc/codegen::+wasm-f32+       #x7d)
          ("f64"       cl-cc/codegen::+wasm-f64+       #x7c)
          ("funcref"   cl-cc/codegen::+wasm-funcref+   #x70)
          ("externref" cl-cc/codegen::+wasm-externref+ #x6f))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-gc-reference-types
  "GC proposal reference types match spec encodings."
  :cases (("anyref"    cl-cc/codegen::+wasm-anyref+    #x6e)
          ("eqref"     cl-cc/codegen::+wasm-eqref+     #x6d)
          ("i31ref"    cl-cc/codegen::+wasm-i31ref+    #x6c)
          ("structref" cl-cc/codegen::+wasm-structref+ #x6b)
          ("arrayref"  cl-cc/codegen::+wasm-arrayref+  #x6a)
          ("nullref"   cl-cc/codegen::+wasm-nullref+   #x69))
  (const expected)
  (assert-equal expected const))

;;; ─── Heap types mirror reference types ─────────────────────────────────────

(deftest-each wasm-heap-types-mirror-ref-types
  "Heap type encodings match their reference type counterparts."
  :cases (("func"   cl-cc/codegen::+wasm-funcref+   cl-cc/codegen::+heap-func+)
          ("extern" cl-cc/codegen::+wasm-externref+ cl-cc/codegen::+heap-extern+)
          ("any"    cl-cc/codegen::+wasm-anyref+    cl-cc/codegen::+heap-any+)
          ("eq"     cl-cc/codegen::+wasm-eqref+     cl-cc/codegen::+heap-eq+)
          ("i31"    cl-cc/codegen::+wasm-i31ref+    cl-cc/codegen::+heap-i31+)
          ("struct" cl-cc/codegen::+wasm-structref+ cl-cc/codegen::+heap-struct+)
          ("array"  cl-cc/codegen::+wasm-arrayref+  cl-cc/codegen::+heap-array+)
          ("none"   cl-cc/codegen::+wasm-nullref+   cl-cc/codegen::+heap-none+))
  (ref-type heap-type)
  (assert-equal ref-type heap-type))

;;; ─── GC type definition encodings ──────────────────────────────────────────

(deftest-each wasm-type-definition-encodings
  "Type section definition opcodes match spec."
  :cases (("func"      cl-cc/codegen::+wasm-type-func+      #x60)
          ("struct"    cl-cc/codegen::+wasm-type-struct+    #x5f)
          ("array"     cl-cc/codegen::+wasm-type-array+     #x5e)
          ("sub"       cl-cc/codegen::+wasm-type-sub+       #x50)
          ("sub-final" cl-cc/codegen::+wasm-type-sub-final+ #x4f)
          ("rec"       cl-cc/codegen::+wasm-type-rec+       #x4e))
  (const expected)
  (assert-equal expected const))

;;; ─── Mutability ────────────────────────────────────────────────────────────

(deftest-each wasm-mutability-values
  "Mutability flags: 0=immutable, 1=mutable."
  :cases (("immutable" cl-cc/codegen::+wasm-immutable+ 0)
          ("mutable"   cl-cc/codegen::+wasm-mutable+   1))
  (const expected)
  (assert-equal expected const))

;;; ─── Export/import descriptors ─────────────────────────────────────────────

(deftest-each wasm-export-descriptor-values
  "Export descriptors span 0-3."
  :cases (("func"   cl-cc/codegen::+wasm-export-func+   0)
          ("table"  cl-cc/codegen::+wasm-export-table+  1)
          ("memory" cl-cc/codegen::+wasm-export-memory+ 2)
          ("global" cl-cc/codegen::+wasm-export-global+ 3))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-import-export-descriptors-equal
  "Import descriptor encodings are identical to export descriptor encodings."
  :cases (("func"   cl-cc/codegen::+wasm-export-func+   cl-cc/codegen::+wasm-import-func+)
          ("table"  cl-cc/codegen::+wasm-export-table+  cl-cc/codegen::+wasm-import-table+)
          ("memory" cl-cc/codegen::+wasm-export-memory+ cl-cc/codegen::+wasm-import-memory+)
          ("global" cl-cc/codegen::+wasm-export-global+ cl-cc/codegen::+wasm-import-global+))
  (export-val import-val)
  (assert-equal export-val import-val))

;;; ─── Core opcodes ──────────────────────────────────────────────────────────

(deftest-each wasm-control-flow-opcodes
  "Control, branch, and call opcodes."
  :cases (("unreachable"   cl-cc/codegen::+wasm-unreachable+   #x00)
          ("nop"           cl-cc/codegen::+wasm-nop+           #x01)
          ("block"         cl-cc/codegen::+wasm-block+         #x02)
          ("loop"          cl-cc/codegen::+wasm-loop+          #x03)
          ("if"            cl-cc/codegen::+wasm-if+            #x04)
          ("else"          cl-cc/codegen::+wasm-else+          #x05)
          ("end"           cl-cc/codegen::+wasm-end+           #x0b)
          ("return"        cl-cc/codegen::+wasm-return+        #x0f)
          ("br"            cl-cc/codegen::+wasm-br+            #x0c)
          ("br-if"         cl-cc/codegen::+wasm-br-if+        #x0d)
          ("br-table"      cl-cc/codegen::+wasm-br-table+     #x0e)
          ("call"          cl-cc/codegen::+wasm-call+          #x10)
          ("call-indirect" cl-cc/codegen::+wasm-call-indirect+ #x11))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-local-global-opcodes
  "Variable access opcodes."
  :cases (("local-get"  cl-cc/codegen::+wasm-local-get+  #x20)
          ("local-set"  cl-cc/codegen::+wasm-local-set+  #x21)
          ("local-tee"  cl-cc/codegen::+wasm-local-tee+  #x22)
          ("global-get" cl-cc/codegen::+wasm-global-get+ #x23)
          ("global-set" cl-cc/codegen::+wasm-global-set+ #x24))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-const-opcodes
  "Constant push opcodes."
  :cases (("i32-const" cl-cc/codegen::+wasm-i32-const+ #x41)
          ("i64-const" cl-cc/codegen::+wasm-i64-const+ #x42)
          ("f64-const" cl-cc/codegen::+wasm-f64-const+ #x44))
  (const expected)
  (assert-equal expected const))

