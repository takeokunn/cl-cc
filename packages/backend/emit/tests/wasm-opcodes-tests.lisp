;;;; tests/unit/emit/wasm-opcodes-tests.lisp — WASM Opcode Tests
;;;;
;;;; Tests for src/emit/wasm-types.lisp:
;;;; i32/i64/f64 arithmetic and comparison opcodes, conversion opcodes,
;;;; reference opcodes, GC opcodes, named type indices, void sentinel,
;;;; and uniqueness invariants.

(in-package :cl-cc/test)

(in-suite wasm-types-suite)

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
