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

;;; ─── i32 comparison opcodes ────────────────────────────────────────────────

(deftest-each wasm-i32-comparison-opcodes
  "i32 comparison opcodes."
  :cases (("eqz"  cl-cc/emit::+wasm-i32-eqz+  #x45)
          ("eq"   cl-cc/emit::+wasm-i32-eq+   #x46)
          ("ne"   cl-cc/emit::+wasm-i32-ne+   #x47)
          ("lt-s" cl-cc/emit::+wasm-i32-lt-s+ #x48)
          ("gt-s" cl-cc/emit::+wasm-i32-gt-s+ #x4a)
          ("le-s" cl-cc/emit::+wasm-i32-le-s+ #x4c)
          ("ge-s" cl-cc/emit::+wasm-i32-ge-s+ #x4e))
  (const expected)
  (assert-equal expected const))

;;; ─── i64 opcodes ───────────────────────────────────────────────────────────

(deftest-each wasm-i64-comparison-opcodes
  "i64 comparison opcodes."
  :cases (("eqz"  cl-cc/emit::+wasm-i64-eqz+  #x50)
          ("eq"   cl-cc/emit::+wasm-i64-eq+   #x51)
          ("ne"   cl-cc/emit::+wasm-i64-ne+   #x52)
          ("lt-s" cl-cc/emit::+wasm-i64-lt-s+ #x53)
          ("gt-s" cl-cc/emit::+wasm-i64-gt-s+ #x55)
          ("le-s" cl-cc/emit::+wasm-i64-le-s+ #x57)
          ("ge-s" cl-cc/emit::+wasm-i64-ge-s+ #x59))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-i64-arithmetic-opcodes
  "i64 arithmetic opcodes."
  :cases (("add"   cl-cc/emit::+wasm-i64-add+   #x7c)
          ("sub"   cl-cc/emit::+wasm-i64-sub+   #x7d)
          ("mul"   cl-cc/emit::+wasm-i64-mul+   #x7e)
          ("div-s" cl-cc/emit::+wasm-i64-div-s+ #x7f)
          ("rem-s" cl-cc/emit::+wasm-i64-rem-s+ #x81)
          ("and"   cl-cc/emit::+wasm-i64-and+   #x83)
          ("or"    cl-cc/emit::+wasm-i64-or+    #x84)
          ("xor"   cl-cc/emit::+wasm-i64-xor+   #x85)
          ("shl"   cl-cc/emit::+wasm-i64-shl+   #x86)
          ("shr-s" cl-cc/emit::+wasm-i64-shr-s+ #x87))
  (const expected)
  (assert-equal expected const))

;;; ─── f64 opcodes ───────────────────────────────────────────────────────────

(deftest-each wasm-f64-opcodes
  "f64 arithmetic and math opcodes."
  :cases (("add"  cl-cc/emit::+wasm-f64-add+  #xa0)
          ("sub"  cl-cc/emit::+wasm-f64-sub+  #xa1)
          ("mul"  cl-cc/emit::+wasm-f64-mul+  #xa2)
          ("div"  cl-cc/emit::+wasm-f64-div+  #xa3)
          ("neg"  cl-cc/emit::+wasm-f64-neg+  #x9a)
          ("abs"  cl-cc/emit::+wasm-f64-abs+  #x99)
          ("sqrt" cl-cc/emit::+wasm-f64-sqrt+ #x9f))
  (const expected)
  (assert-equal expected const))

;;; ─── Conversion opcodes ────────────────────────────────────────────────────

(deftest-each wasm-conversion-opcodes
  "Integer/float conversion opcodes."
  :cases (("i32-wrap-i64"       cl-cc/emit::+wasm-i32-wrap-i64+       #xa7)
          ("i64-extend-i32-s"   cl-cc/emit::+wasm-i64-extend-i32-s+   #xac)
          ("f64-convert-i64-s"  cl-cc/emit::+wasm-f64-convert-i64-s+  #xb9)
          ("i64-trunc-f64-s"    cl-cc/emit::+wasm-i64-trunc-f64-s+    #xb0))
  (const expected)
  (assert-equal expected const))

;;; ─── Reference opcodes ─────────────────────────────────────────────────────

(deftest-each wasm-reference-opcodes
  "Reference manipulation opcodes."
  :cases (("ref-null"    cl-cc/emit::+wasm-ref-null+    #xd0)
          ("ref-is-null" cl-cc/emit::+wasm-ref-is-null+ #xd1)
          ("ref-func"    cl-cc/emit::+wasm-ref-func+    #xd2)
          ("ref-eq"      cl-cc/emit::+wasm-ref-eq+      #xd3))
  (const expected)
  (assert-equal expected const))

;;; ─── GC opcodes ────────────────────────────────────────────────────────────

(deftest wasm-gc-prefix
  "GC opcode prefix byte."
  (assert-equal #xfb cl-cc/emit::+wasm-gc-prefix+))

(deftest-each wasm-gc-struct-opcodes
  "GC struct manipulation opcodes."
  :cases (("new"         cl-cc/emit::+wasm-gc-struct-new+         0)
          ("new-default" cl-cc/emit::+wasm-gc-struct-new-default+ 1)
          ("get"         cl-cc/emit::+wasm-gc-struct-get+         2)
          ("get-s"       cl-cc/emit::+wasm-gc-struct-get-s+       3)
          ("get-u"       cl-cc/emit::+wasm-gc-struct-get-u+       4)
          ("set"         cl-cc/emit::+wasm-gc-struct-set+         5))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-gc-array-opcodes
  "GC array manipulation opcodes."
  :cases (("new"         cl-cc/emit::+wasm-gc-array-new+         6)
          ("new-default" cl-cc/emit::+wasm-gc-array-new-default+ 7)
          ("new-fixed"   cl-cc/emit::+wasm-gc-array-new-fixed+   8)
          ("get"         cl-cc/emit::+wasm-gc-array-get+         11)
          ("set"         cl-cc/emit::+wasm-gc-array-set+         14)
          ("len"         cl-cc/emit::+wasm-gc-array-len+         15))
  (const expected)
  (assert-equal expected const))

(deftest-each wasm-gc-ref-opcodes
  "GC reference test/cast opcodes."
  :cases (("ref-test"   cl-cc/emit::+wasm-gc-ref-test+   20)
          ("ref-cast"   cl-cc/emit::+wasm-gc-ref-cast+   22)
          ("ref-i31"    cl-cc/emit::+wasm-gc-ref-i31+    28)
          ("i31-get-s"  cl-cc/emit::+wasm-gc-i31-get-s+  29)
          ("i31-get-u"  cl-cc/emit::+wasm-gc-i31-get-u+  30))
  (const expected)
  (assert-equal expected const))

;;; ─── Named type indices ────────────────────────────────────────────────────

(deftest-each wasm-predefined-type-indices
  "Named type indices are contiguous starting from 0."
  :cases (("main-func"    cl-cc/emit::+type-idx-main-func+    0)
          ("bytes-array"  cl-cc/emit::+type-idx-bytes-array+  1)
          ("string"       cl-cc/emit::+type-idx-string+       2)
          ("symbol"       cl-cc/emit::+type-idx-symbol+       3)
          ("cons"         cl-cc/emit::+type-idx-cons+         4)
          ("eqref-array"  cl-cc/emit::+type-idx-eqref-array+  5)
          ("env"          cl-cc/emit::+type-idx-env+          6)
          ("closure"      cl-cc/emit::+type-idx-closure+      7)
          ("class-meta"   cl-cc/emit::+type-idx-class-meta+   8)
          ("instance"     cl-cc/emit::+type-idx-instance+     9)
          ("htable"       cl-cc/emit::+type-idx-htable+       10)
          ("float"        cl-cc/emit::+type-idx-float+        11)
          ("char"         cl-cc/emit::+type-idx-char+         12))
  (const expected)
  (assert-equal expected const))

(deftest wasm-num-predefined-types
  "Predefined type count matches last index + 1."
  (assert-equal 13 cl-cc/emit::+num-predefined-types+)
  (assert-equal (1+ cl-cc/emit::+type-idx-char+) cl-cc/emit::+num-predefined-types+))

;;; ─── Void sentinel ─────────────────────────────────────────────────────────

(deftest wasm-void-sentinel
  "Void block type is #x40."
  (assert-equal #x40 cl-cc/emit::+wasm-void+))

;;; ─── Uniqueness invariants ─────────────────────────────────────────────────

(deftest wasm-uniqueness-invariants
  "Section IDs, predefined type indices, and GC struct opcodes are all unique/contiguous."
  (let ((ids (list cl-cc/emit::+wasm-section-custom+ cl-cc/emit::+wasm-section-type+
                   cl-cc/emit::+wasm-section-import+ cl-cc/emit::+wasm-section-function+
                   cl-cc/emit::+wasm-section-table+ cl-cc/emit::+wasm-section-memory+
                   cl-cc/emit::+wasm-section-global+ cl-cc/emit::+wasm-section-export+
                   cl-cc/emit::+wasm-section-start+ cl-cc/emit::+wasm-section-element+
                   cl-cc/emit::+wasm-section-code+ cl-cc/emit::+wasm-section-data+
                   cl-cc/emit::+wasm-section-data-count+)))
    (assert-equal (length ids) (length (remove-duplicates ids))))
  (let ((indices (list cl-cc/emit::+type-idx-main-func+ cl-cc/emit::+type-idx-bytes-array+
                       cl-cc/emit::+type-idx-string+ cl-cc/emit::+type-idx-symbol+
                       cl-cc/emit::+type-idx-cons+ cl-cc/emit::+type-idx-eqref-array+
                       cl-cc/emit::+type-idx-env+ cl-cc/emit::+type-idx-closure+
                       cl-cc/emit::+type-idx-class-meta+ cl-cc/emit::+type-idx-instance+
                       cl-cc/emit::+type-idx-htable+ cl-cc/emit::+type-idx-float+
                       cl-cc/emit::+type-idx-char+)))
    (assert-equal (length indices) (length (remove-duplicates indices))))
  (let ((ops (list cl-cc/emit::+wasm-gc-struct-new+ cl-cc/emit::+wasm-gc-struct-new-default+
                   cl-cc/emit::+wasm-gc-struct-get+ cl-cc/emit::+wasm-gc-struct-get-s+
                   cl-cc/emit::+wasm-gc-struct-get-u+ cl-cc/emit::+wasm-gc-struct-set+)))
    (assert-equal 6 (length (remove-duplicates ops)))
    (assert-equal 0 (first ops))
    (assert-equal 5 (car (last ops)))))
