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
  :cases (("custom"     cl-cc/emit::+wasm-section-custom+     0)
          ("type"       cl-cc/emit::+wasm-section-type+       1)
          ("import"     cl-cc/emit::+wasm-section-import+     2)
          ("function"   cl-cc/emit::+wasm-section-function+   3)
          ("table"      cl-cc/emit::+wasm-section-table+      4)
          ("memory"     cl-cc/emit::+wasm-section-memory+     5)
          ("global"     cl-cc/emit::+wasm-section-global+     6)
          ("export"     cl-cc/emit::+wasm-section-export+     7)
          ("start"      cl-cc/emit::+wasm-section-start+      8)
          ("element"    cl-cc/emit::+wasm-section-element+    9)
          ("code"       cl-cc/emit::+wasm-section-code+       10)
          ("data"       cl-cc/emit::+wasm-section-data+       11)
          ("data-count" cl-cc/emit::+wasm-section-data-count+ 12))
  (const expected)
  (assert-equal expected const))

;;; ─── Primitive value types ─────────────────────────────────────────────────

(deftest-each wasm-primitive-value-types
  "WASM primitive types match spec encodings."
  :cases (("i32"       cl-cc/emit::+wasm-i32+       #x7f)
          ("i64"       cl-cc/emit::+wasm-i64+       #x7e)
          ("f32"       cl-cc/emit::+wasm-f32+       #x7d)
          ("f64"       cl-cc/emit::+wasm-f64+       #x7c)
          ("funcref"   cl-cc/emit::+wasm-funcref+   #x70)
          ("externref" cl-cc/emit::+wasm-externref+ #x6f))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-gc-reference-types
  "GC proposal reference types match spec encodings."
  :cases (("anyref"    cl-cc/emit::+wasm-anyref+    #x6e)
          ("eqref"     cl-cc/emit::+wasm-eqref+     #x6d)
          ("i31ref"    cl-cc/emit::+wasm-i31ref+    #x6c)
          ("structref" cl-cc/emit::+wasm-structref+ #x6b)
          ("arrayref"  cl-cc/emit::+wasm-arrayref+  #x6a)
          ("nullref"   cl-cc/emit::+wasm-nullref+   #x69))
  (const expected)
  (assert-equal expected const))

;;; ─── Heap types mirror reference types ─────────────────────────────────────

(deftest-each wasm-heap-types-mirror-ref-types
  "Heap type encodings match their reference type counterparts."
  :cases (("func"   cl-cc/emit::+wasm-funcref+   cl-cc/emit::+heap-func+)
          ("extern" cl-cc/emit::+wasm-externref+ cl-cc/emit::+heap-extern+)
          ("any"    cl-cc/emit::+wasm-anyref+    cl-cc/emit::+heap-any+)
          ("eq"     cl-cc/emit::+wasm-eqref+     cl-cc/emit::+heap-eq+)
          ("i31"    cl-cc/emit::+wasm-i31ref+    cl-cc/emit::+heap-i31+)
          ("struct" cl-cc/emit::+wasm-structref+ cl-cc/emit::+heap-struct+)
          ("array"  cl-cc/emit::+wasm-arrayref+  cl-cc/emit::+heap-array+)
          ("none"   cl-cc/emit::+wasm-nullref+   cl-cc/emit::+heap-none+))
  (ref-type heap-type)
  (assert-equal ref-type heap-type))

;;; ─── GC type definition encodings ──────────────────────────────────────────

(deftest-each wasm-type-definition-encodings
  "Type section definition opcodes match spec."
  :cases (("func"      cl-cc/emit::+wasm-type-func+      #x60)
          ("struct"    cl-cc/emit::+wasm-type-struct+    #x5f)
          ("array"     cl-cc/emit::+wasm-type-array+     #x5e)
          ("sub"       cl-cc/emit::+wasm-type-sub+       #x50)
          ("sub-final" cl-cc/emit::+wasm-type-sub-final+ #x4f)
          ("rec"       cl-cc/emit::+wasm-type-rec+       #x4e))
  (const expected)
  (assert-equal expected const))

;;; ─── Mutability ────────────────────────────────────────────────────────────

(deftest-each wasm-mutability-values
  "Mutability flags: 0=immutable, 1=mutable."
  :cases (("immutable" cl-cc/emit::+wasm-immutable+ 0)
          ("mutable"   cl-cc/emit::+wasm-mutable+   1))
  (const expected)
  (assert-equal expected const))

;;; ─── Export/import descriptors ─────────────────────────────────────────────

(deftest-each wasm-export-descriptor-values
  "Export descriptors span 0-3."
  :cases (("func"   cl-cc/emit::+wasm-export-func+   0)
          ("table"  cl-cc/emit::+wasm-export-table+  1)
          ("memory" cl-cc/emit::+wasm-export-memory+ 2)
          ("global" cl-cc/emit::+wasm-export-global+ 3))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-import-export-descriptors-equal
  "Import descriptor encodings are identical to export descriptor encodings."
  :cases (("func"   cl-cc/emit::+wasm-export-func+   cl-cc/emit::+wasm-import-func+)
          ("table"  cl-cc/emit::+wasm-export-table+  cl-cc/emit::+wasm-import-table+)
          ("memory" cl-cc/emit::+wasm-export-memory+ cl-cc/emit::+wasm-import-memory+)
          ("global" cl-cc/emit::+wasm-export-global+ cl-cc/emit::+wasm-import-global+))
  (export-val import-val)
  (assert-equal export-val import-val))

;;; ─── Core opcodes ──────────────────────────────────────────────────────────

(deftest-each wasm-control-flow-opcodes
  "Control, branch, and call opcodes."
  :cases (("unreachable"   cl-cc/emit::+wasm-unreachable+   #x00)
          ("nop"           cl-cc/emit::+wasm-nop+           #x01)
          ("block"         cl-cc/emit::+wasm-block+         #x02)
          ("loop"          cl-cc/emit::+wasm-loop+          #x03)
          ("if"            cl-cc/emit::+wasm-if+            #x04)
          ("else"          cl-cc/emit::+wasm-else+          #x05)
          ("end"           cl-cc/emit::+wasm-end+           #x0b)
          ("return"        cl-cc/emit::+wasm-return+        #x0f)
          ("br"            cl-cc/emit::+wasm-br+            #x0c)
          ("br-if"         cl-cc/emit::+wasm-br-if+        #x0d)
          ("br-table"      cl-cc/emit::+wasm-br-table+     #x0e)
          ("call"          cl-cc/emit::+wasm-call+          #x10)
          ("call-indirect" cl-cc/emit::+wasm-call-indirect+ #x11))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-local-global-opcodes
  "Variable access opcodes."
  :cases (("local-get"  cl-cc/emit::+wasm-local-get+  #x20)
          ("local-set"  cl-cc/emit::+wasm-local-set+  #x21)
          ("local-tee"  cl-cc/emit::+wasm-local-tee+  #x22)
          ("global-get" cl-cc/emit::+wasm-global-get+ #x23)
          ("global-set" cl-cc/emit::+wasm-global-set+ #x24))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-const-opcodes
  "Constant push opcodes."
  :cases (("i32-const" cl-cc/emit::+wasm-i32-const+ #x41)
          ("i64-const" cl-cc/emit::+wasm-i64-const+ #x42)
          ("f64-const" cl-cc/emit::+wasm-f64-const+ #x44))
  (const expected)
  (assert-equal expected const))

