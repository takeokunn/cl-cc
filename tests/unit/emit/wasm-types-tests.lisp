;;;; tests/unit/emit/wasm-types-tests.lisp — WASM Type Constants Tests
;;;;
;;;; Tests for src/emit/wasm-types.lisp:
;;;; Section IDs, value type encodings, GC proposal types, heap types,
;;;; type definition encodings, opcodes, named type indices, and
;;;; uniqueness invariants.

(in-package :cl-cc/test)

(defsuite wasm-types-suite :description "WASM type constants and GC type index tests"
  :parent cl-cc-suite)


(in-suite wasm-types-suite)
;;; ─── Section IDs ───────────────────────────────────────────────────────────

(deftest-each wasm-section-ids-range
  "Section IDs span 0-12 contiguously."
  :cases (("custom"     cl-cc::+wasm-section-custom+     0)
          ("type"       cl-cc::+wasm-section-type+       1)
          ("import"     cl-cc::+wasm-section-import+     2)
          ("function"   cl-cc::+wasm-section-function+   3)
          ("table"      cl-cc::+wasm-section-table+      4)
          ("memory"     cl-cc::+wasm-section-memory+     5)
          ("global"     cl-cc::+wasm-section-global+     6)
          ("export"     cl-cc::+wasm-section-export+     7)
          ("start"      cl-cc::+wasm-section-start+      8)
          ("element"    cl-cc::+wasm-section-element+    9)
          ("code"       cl-cc::+wasm-section-code+       10)
          ("data"       cl-cc::+wasm-section-data+       11)
          ("data-count" cl-cc::+wasm-section-data-count+ 12))
  (const expected)
  (assert-equal expected const))

;;; ─── Primitive value types ─────────────────────────────────────────────────

(deftest-each wasm-primitive-value-types
  "WASM primitive types match spec encodings."
  :cases (("i32"       cl-cc::+wasm-i32+       #x7f)
          ("i64"       cl-cc::+wasm-i64+       #x7e)
          ("f32"       cl-cc::+wasm-f32+       #x7d)
          ("f64"       cl-cc::+wasm-f64+       #x7c)
          ("funcref"   cl-cc::+wasm-funcref+   #x70)
          ("externref" cl-cc::+wasm-externref+ #x6f))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-gc-reference-types
  "GC proposal reference types match spec encodings."
  :cases (("anyref"    cl-cc::+wasm-anyref+    #x6e)
          ("eqref"     cl-cc::+wasm-eqref+     #x6d)
          ("i31ref"    cl-cc::+wasm-i31ref+    #x6c)
          ("structref" cl-cc::+wasm-structref+ #x6b)
          ("arrayref"  cl-cc::+wasm-arrayref+  #x6a)
          ("nullref"   cl-cc::+wasm-nullref+   #x69))
  (const expected)
  (assert-equal expected const))

;;; ─── Heap types mirror reference types ─────────────────────────────────────

(deftest-each wasm-heap-types-mirror-ref-types
  "Heap type encodings match their reference type counterparts."
  :cases (("func"   cl-cc::+wasm-funcref+   cl-cc::+heap-func+)
          ("extern" cl-cc::+wasm-externref+ cl-cc::+heap-extern+)
          ("any"    cl-cc::+wasm-anyref+    cl-cc::+heap-any+)
          ("eq"     cl-cc::+wasm-eqref+     cl-cc::+heap-eq+)
          ("i31"    cl-cc::+wasm-i31ref+    cl-cc::+heap-i31+)
          ("struct" cl-cc::+wasm-structref+ cl-cc::+heap-struct+)
          ("array"  cl-cc::+wasm-arrayref+  cl-cc::+heap-array+)
          ("none"   cl-cc::+wasm-nullref+   cl-cc::+heap-none+))
  (ref-type heap-type)
  (assert-equal ref-type heap-type))

;;; ─── GC type definition encodings ──────────────────────────────────────────

(deftest-each wasm-type-definition-encodings
  "Type section definition opcodes match spec."
  :cases (("func"      cl-cc::+wasm-type-func+      #x60)
          ("struct"    cl-cc::+wasm-type-struct+    #x5f)
          ("array"     cl-cc::+wasm-type-array+     #x5e)
          ("sub"       cl-cc::+wasm-type-sub+       #x50)
          ("sub-final" cl-cc::+wasm-type-sub-final+ #x4f)
          ("rec"       cl-cc::+wasm-type-rec+       #x4e))
  (const expected)
  (assert-equal expected const))

;;; ─── Mutability ────────────────────────────────────────────────────────────

(deftest-each wasm-mutability-values
  "Mutability flags: 0=immutable, 1=mutable."
  :cases (("immutable" cl-cc::+wasm-immutable+ 0)
          ("mutable"   cl-cc::+wasm-mutable+   1))
  (const expected)
  (assert-equal expected const))

;;; ─── Export/import descriptors ─────────────────────────────────────────────

(deftest-each wasm-export-descriptor-values
  "Export descriptors span 0-3."
  :cases (("func"   cl-cc::+wasm-export-func+   0)
          ("table"  cl-cc::+wasm-export-table+  1)
          ("memory" cl-cc::+wasm-export-memory+ 2)
          ("global" cl-cc::+wasm-export-global+ 3))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-import-export-descriptors-equal
  "Import descriptor encodings are identical to export descriptor encodings."
  :cases (("func"   cl-cc::+wasm-export-func+   cl-cc::+wasm-import-func+)
          ("table"  cl-cc::+wasm-export-table+  cl-cc::+wasm-import-table+)
          ("memory" cl-cc::+wasm-export-memory+ cl-cc::+wasm-import-memory+)
          ("global" cl-cc::+wasm-export-global+ cl-cc::+wasm-import-global+))
  (export-val import-val)
  (assert-equal export-val import-val))

;;; ─── Core opcodes ──────────────────────────────────────────────────────────

(deftest-each wasm-control-flow-opcodes
  "Control, branch, and call opcodes."
  :cases (("unreachable"   cl-cc::+wasm-unreachable+   #x00)
          ("nop"           cl-cc::+wasm-nop+           #x01)
          ("block"         cl-cc::+wasm-block+         #x02)
          ("loop"          cl-cc::+wasm-loop+          #x03)
          ("if"            cl-cc::+wasm-if+            #x04)
          ("else"          cl-cc::+wasm-else+          #x05)
          ("end"           cl-cc::+wasm-end+           #x0b)
          ("return"        cl-cc::+wasm-return+        #x0f)
          ("br"            cl-cc::+wasm-br+            #x0c)
          ("br-if"         cl-cc::+wasm-br-if+        #x0d)
          ("br-table"      cl-cc::+wasm-br-table+     #x0e)
          ("call"          cl-cc::+wasm-call+          #x10)
          ("call-indirect" cl-cc::+wasm-call-indirect+ #x11))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-local-global-opcodes
  "Variable access opcodes."
  :cases (("local-get"  cl-cc::+wasm-local-get+  #x20)
          ("local-set"  cl-cc::+wasm-local-set+  #x21)
          ("local-tee"  cl-cc::+wasm-local-tee+  #x22)
          ("global-get" cl-cc::+wasm-global-get+ #x23)
          ("global-set" cl-cc::+wasm-global-set+ #x24))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-const-opcodes
  "Constant push opcodes."
  :cases (("i32-const" cl-cc::+wasm-i32-const+ #x41)
          ("i64-const" cl-cc::+wasm-i64-const+ #x42)
          ("f64-const" cl-cc::+wasm-f64-const+ #x44))
  (const expected)
  (assert-equal expected const))

;;; ─── i32 comparison opcodes ────────────────────────────────────────────────

(deftest-each wasm-i32-comparison-opcodes
  "i32 comparison opcodes."
  :cases (("eqz"  cl-cc::+wasm-i32-eqz+  #x45)
          ("eq"   cl-cc::+wasm-i32-eq+   #x46)
          ("ne"   cl-cc::+wasm-i32-ne+   #x47)
          ("lt-s" cl-cc::+wasm-i32-lt-s+ #x48)
          ("gt-s" cl-cc::+wasm-i32-gt-s+ #x4a)
          ("le-s" cl-cc::+wasm-i32-le-s+ #x4c)
          ("ge-s" cl-cc::+wasm-i32-ge-s+ #x4e))
  (const expected)
  (assert-equal expected const))

;;; ─── i64 opcodes ───────────────────────────────────────────────────────────

(deftest-each wasm-i64-comparison-opcodes
  "i64 comparison opcodes."
  :cases (("eqz"  cl-cc::+wasm-i64-eqz+  #x50)
          ("eq"   cl-cc::+wasm-i64-eq+   #x51)
          ("ne"   cl-cc::+wasm-i64-ne+   #x52)
          ("lt-s" cl-cc::+wasm-i64-lt-s+ #x53)
          ("gt-s" cl-cc::+wasm-i64-gt-s+ #x55)
          ("le-s" cl-cc::+wasm-i64-le-s+ #x57)
          ("ge-s" cl-cc::+wasm-i64-ge-s+ #x59))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-i64-arithmetic-opcodes
  "i64 arithmetic opcodes."
  :cases (("add"   cl-cc::+wasm-i64-add+   #x7c)
          ("sub"   cl-cc::+wasm-i64-sub+   #x7d)
          ("mul"   cl-cc::+wasm-i64-mul+   #x7e)
          ("div-s" cl-cc::+wasm-i64-div-s+ #x7f)
          ("rem-s" cl-cc::+wasm-i64-rem-s+ #x81)
          ("and"   cl-cc::+wasm-i64-and+   #x83)
          ("or"    cl-cc::+wasm-i64-or+    #x84)
          ("xor"   cl-cc::+wasm-i64-xor+   #x85)
          ("shl"   cl-cc::+wasm-i64-shl+   #x86)
          ("shr-s" cl-cc::+wasm-i64-shr-s+ #x87))
  (const expected)
  (assert-equal expected const))

;;; ─── f64 opcodes ───────────────────────────────────────────────────────────

(deftest-each wasm-f64-opcodes
  "f64 arithmetic and math opcodes."
  :cases (("add"  cl-cc::+wasm-f64-add+  #xa0)
          ("sub"  cl-cc::+wasm-f64-sub+  #xa1)
          ("mul"  cl-cc::+wasm-f64-mul+  #xa2)
          ("div"  cl-cc::+wasm-f64-div+  #xa3)
          ("neg"  cl-cc::+wasm-f64-neg+  #x9a)
          ("abs"  cl-cc::+wasm-f64-abs+  #x99)
          ("sqrt" cl-cc::+wasm-f64-sqrt+ #x9f))
  (const expected)
  (assert-equal expected const))

;;; ─── Conversion opcodes ────────────────────────────────────────────────────

(deftest-each wasm-conversion-opcodes
  "Integer/float conversion opcodes."
  :cases (("i32-wrap-i64"       cl-cc::+wasm-i32-wrap-i64+       #xa7)
          ("i64-extend-i32-s"   cl-cc::+wasm-i64-extend-i32-s+   #xac)
          ("f64-convert-i64-s"  cl-cc::+wasm-f64-convert-i64-s+  #xb9)
          ("i64-trunc-f64-s"    cl-cc::+wasm-i64-trunc-f64-s+    #xb0))
  (const expected)
  (assert-equal expected const))

;;; ─── Reference opcodes ─────────────────────────────────────────────────────

(deftest-each wasm-reference-opcodes
  "Reference manipulation opcodes."
  :cases (("ref-null"    cl-cc::+wasm-ref-null+    #xd0)
          ("ref-is-null" cl-cc::+wasm-ref-is-null+ #xd1)
          ("ref-func"    cl-cc::+wasm-ref-func+    #xd2)
          ("ref-eq"      cl-cc::+wasm-ref-eq+      #xd3))
  (const expected)
  (assert-equal expected const))

;;; ─── GC opcodes ────────────────────────────────────────────────────────────

(deftest wasm-gc-prefix
  "GC opcode prefix byte."
  (assert-equal #xfb cl-cc::+wasm-gc-prefix+))

(deftest-each wasm-gc-struct-opcodes
  "GC struct manipulation opcodes."
  :cases (("new"         cl-cc::+wasm-gc-struct-new+         0)
          ("new-default" cl-cc::+wasm-gc-struct-new-default+ 1)
          ("get"         cl-cc::+wasm-gc-struct-get+         2)
          ("get-s"       cl-cc::+wasm-gc-struct-get-s+       3)
          ("get-u"       cl-cc::+wasm-gc-struct-get-u+       4)
          ("set"         cl-cc::+wasm-gc-struct-set+         5))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-gc-array-opcodes
  "GC array manipulation opcodes."
  :cases (("new"         cl-cc::+wasm-gc-array-new+         6)
          ("new-default" cl-cc::+wasm-gc-array-new-default+ 7)
          ("new-fixed"   cl-cc::+wasm-gc-array-new-fixed+   8)
          ("get"         cl-cc::+wasm-gc-array-get+         11)
          ("set"         cl-cc::+wasm-gc-array-set+         14)
          ("len"         cl-cc::+wasm-gc-array-len+         15))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-gc-ref-opcodes
  "GC reference test/cast opcodes."
  :cases (("ref-test"   cl-cc::+wasm-gc-ref-test+   20)
          ("ref-cast"   cl-cc::+wasm-gc-ref-cast+   22)
          ("ref-i31"    cl-cc::+wasm-gc-ref-i31+    28)
          ("i31-get-s"  cl-cc::+wasm-gc-i31-get-s+  29)
          ("i31-get-u"  cl-cc::+wasm-gc-i31-get-u+  30))
  (const expected)
  (assert-equal expected const))

;;; ─── Named type indices ────────────────────────────────────────────────────

(deftest-each wasm-predefined-type-indices
  "Named type indices are contiguous starting from 0."
  :cases (("main-func"    cl-cc::+type-idx-main-func+    0)
          ("bytes-array"  cl-cc::+type-idx-bytes-array+  1)
          ("string"       cl-cc::+type-idx-string+       2)
          ("symbol"       cl-cc::+type-idx-symbol+       3)
          ("cons"         cl-cc::+type-idx-cons+         4)
          ("eqref-array"  cl-cc::+type-idx-eqref-array+  5)
          ("env"          cl-cc::+type-idx-env+          6)
          ("closure"      cl-cc::+type-idx-closure+      7)
          ("class-meta"   cl-cc::+type-idx-class-meta+   8)
          ("instance"     cl-cc::+type-idx-instance+     9)
          ("htable"       cl-cc::+type-idx-htable+       10)
          ("float"        cl-cc::+type-idx-float+        11)
          ("char"         cl-cc::+type-idx-char+         12))
  (const expected)
  (assert-equal expected const))

(deftest wasm-num-predefined-types
  "Predefined type count matches last index + 1."
  (assert-equal 13 cl-cc::+num-predefined-types+)
  (assert-equal (1+ cl-cc::+type-idx-char+) cl-cc::+num-predefined-types+))

;;; ─── Void sentinel ─────────────────────────────────────────────────────────

(deftest wasm-void-sentinel
  "Void block type is #x40."
  (assert-equal #x40 cl-cc::+wasm-void+))

;;; ─── Uniqueness invariants ─────────────────────────────────────────────────

(deftest wasm-uniqueness-invariants
  "Section IDs, predefined type indices, and GC struct opcodes are all unique/contiguous."
  (let ((ids (list cl-cc::+wasm-section-custom+ cl-cc::+wasm-section-type+
                   cl-cc::+wasm-section-import+ cl-cc::+wasm-section-function+
                   cl-cc::+wasm-section-table+ cl-cc::+wasm-section-memory+
                   cl-cc::+wasm-section-global+ cl-cc::+wasm-section-export+
                   cl-cc::+wasm-section-start+ cl-cc::+wasm-section-element+
                   cl-cc::+wasm-section-code+ cl-cc::+wasm-section-data+
                   cl-cc::+wasm-section-data-count+)))
    (assert-equal (length ids) (length (remove-duplicates ids))))
  (let ((indices (list cl-cc::+type-idx-main-func+ cl-cc::+type-idx-bytes-array+
                       cl-cc::+type-idx-string+ cl-cc::+type-idx-symbol+
                       cl-cc::+type-idx-cons+ cl-cc::+type-idx-eqref-array+
                       cl-cc::+type-idx-env+ cl-cc::+type-idx-closure+
                       cl-cc::+type-idx-class-meta+ cl-cc::+type-idx-instance+
                       cl-cc::+type-idx-htable+ cl-cc::+type-idx-float+
                       cl-cc::+type-idx-char+)))
    (assert-equal (length indices) (length (remove-duplicates indices))))
  (let ((ops (list cl-cc::+wasm-gc-struct-new+ cl-cc::+wasm-gc-struct-new-default+
                   cl-cc::+wasm-gc-struct-get+ cl-cc::+wasm-gc-struct-get-s+
                   cl-cc::+wasm-gc-struct-get-u+ cl-cc::+wasm-gc-struct-set+)))
    (assert-equal 6 (length (remove-duplicates ops)))
    (assert-equal 0 (first ops))
    (assert-equal 5 (car (last ops)))))
