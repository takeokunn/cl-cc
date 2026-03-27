;;;; tests/unit/emit/wasm-types-tests.lisp — WASM Type Constants Tests
;;;;
;;;; Tests for src/emit/wasm-types.lisp:
;;;; Section IDs, value type encodings, GC proposal types, heap types,
;;;; type definition encodings, opcodes, named type indices, and
;;;; uniqueness invariants.

(in-package :cl-cc/test)

(defsuite wasm-types-suite :description "WASM type constants and GC type index tests")

;;; ─── Section IDs ───────────────────────────────────────────────────────────

(deftest wasm-section-ids-range
  "Section IDs span 0-12 contiguously."
  (assert-equal 0  cl-cc::+wasm-section-custom+)
  (assert-equal 1  cl-cc::+wasm-section-type+)
  (assert-equal 2  cl-cc::+wasm-section-import+)
  (assert-equal 3  cl-cc::+wasm-section-function+)
  (assert-equal 4  cl-cc::+wasm-section-table+)
  (assert-equal 5  cl-cc::+wasm-section-memory+)
  (assert-equal 6  cl-cc::+wasm-section-global+)
  (assert-equal 7  cl-cc::+wasm-section-export+)
  (assert-equal 8  cl-cc::+wasm-section-start+)
  (assert-equal 9  cl-cc::+wasm-section-element+)
  (assert-equal 10 cl-cc::+wasm-section-code+)
  (assert-equal 11 cl-cc::+wasm-section-data+)
  (assert-equal 12 cl-cc::+wasm-section-data-count+))

;;; ─── Primitive value types ─────────────────────────────────────────────────

(deftest wasm-primitive-value-types
  "WASM primitive types match spec encodings."
  (assert-equal #x7f cl-cc::+wasm-i32+)
  (assert-equal #x7e cl-cc::+wasm-i64+)
  (assert-equal #x7d cl-cc::+wasm-f32+)
  (assert-equal #x7c cl-cc::+wasm-f64+)
  (assert-equal #x70 cl-cc::+wasm-funcref+)
  (assert-equal #x6f cl-cc::+wasm-externref+))

(deftest wasm-gc-reference-types
  "GC proposal reference types match spec encodings."
  (assert-equal #x6e cl-cc::+wasm-anyref+)
  (assert-equal #x6d cl-cc::+wasm-eqref+)
  (assert-equal #x6c cl-cc::+wasm-i31ref+)
  (assert-equal #x6b cl-cc::+wasm-structref+)
  (assert-equal #x6a cl-cc::+wasm-arrayref+)
  (assert-equal #x69 cl-cc::+wasm-nullref+))

;;; ─── Heap types mirror reference types ─────────────────────────────────────

(deftest wasm-heap-types-mirror-ref-types
  "Heap type encodings match their reference type counterparts."
  (assert-equal cl-cc::+wasm-funcref+   cl-cc::+heap-func+)
  (assert-equal cl-cc::+wasm-externref+ cl-cc::+heap-extern+)
  (assert-equal cl-cc::+wasm-anyref+    cl-cc::+heap-any+)
  (assert-equal cl-cc::+wasm-eqref+     cl-cc::+heap-eq+)
  (assert-equal cl-cc::+wasm-i31ref+    cl-cc::+heap-i31+)
  (assert-equal cl-cc::+wasm-structref+ cl-cc::+heap-struct+)
  (assert-equal cl-cc::+wasm-arrayref+  cl-cc::+heap-array+)
  (assert-equal cl-cc::+wasm-nullref+   cl-cc::+heap-none+))

;;; ─── GC type definition encodings ──────────────────────────────────────────

(deftest wasm-type-definition-encodings
  "Type section definition opcodes match spec."
  (assert-equal #x60 cl-cc::+wasm-type-func+)
  (assert-equal #x5f cl-cc::+wasm-type-struct+)
  (assert-equal #x5e cl-cc::+wasm-type-array+)
  (assert-equal #x50 cl-cc::+wasm-type-sub+)
  (assert-equal #x4f cl-cc::+wasm-type-sub-final+)
  (assert-equal #x4e cl-cc::+wasm-type-rec+))

;;; ─── Mutability ────────────────────────────────────────────────────────────

(deftest wasm-mutability-values
  "Mutability flags: 0=immutable, 1=mutable."
  (assert-equal 0 cl-cc::+wasm-immutable+)
  (assert-equal 1 cl-cc::+wasm-mutable+))

;;; ─── Export/import descriptors ─────────────────────────────────────────────

(deftest wasm-export-import-descriptors
  "Export/import descriptors span 0-3 and export/import encodings are identical."
  (assert-equal 0 cl-cc::+wasm-export-func+)
  (assert-equal 1 cl-cc::+wasm-export-table+)
  (assert-equal 2 cl-cc::+wasm-export-memory+)
  (assert-equal 3 cl-cc::+wasm-export-global+)
  (assert-equal cl-cc::+wasm-export-func+   cl-cc::+wasm-import-func+)
  (assert-equal cl-cc::+wasm-export-table+  cl-cc::+wasm-import-table+)
  (assert-equal cl-cc::+wasm-export-memory+ cl-cc::+wasm-import-memory+)
  (assert-equal cl-cc::+wasm-export-global+ cl-cc::+wasm-import-global+))

;;; ─── Core opcodes ──────────────────────────────────────────────────────────

(deftest wasm-control-flow-opcodes
  "Control, branch, and call opcodes."
  (assert-equal #x00 cl-cc::+wasm-unreachable+)
  (assert-equal #x01 cl-cc::+wasm-nop+)
  (assert-equal #x02 cl-cc::+wasm-block+)
  (assert-equal #x03 cl-cc::+wasm-loop+)
  (assert-equal #x04 cl-cc::+wasm-if+)
  (assert-equal #x05 cl-cc::+wasm-else+)
  (assert-equal #x0b cl-cc::+wasm-end+)
  (assert-equal #x0f cl-cc::+wasm-return+)
  (assert-equal #x0c cl-cc::+wasm-br+)
  (assert-equal #x0d cl-cc::+wasm-br-if+)
  (assert-equal #x0e cl-cc::+wasm-br-table+)
  (assert-equal #x10 cl-cc::+wasm-call+)
  (assert-equal #x11 cl-cc::+wasm-call-indirect+))

(deftest wasm-local-global-opcodes
  "Variable access opcodes."
  (assert-equal #x20 cl-cc::+wasm-local-get+)
  (assert-equal #x21 cl-cc::+wasm-local-set+)
  (assert-equal #x22 cl-cc::+wasm-local-tee+)
  (assert-equal #x23 cl-cc::+wasm-global-get+)
  (assert-equal #x24 cl-cc::+wasm-global-set+))

(deftest wasm-const-opcodes
  "Constant push opcodes."
  (assert-equal #x41 cl-cc::+wasm-i32-const+)
  (assert-equal #x42 cl-cc::+wasm-i64-const+)
  (assert-equal #x44 cl-cc::+wasm-f64-const+))

;;; ─── i32 comparison opcodes ────────────────────────────────────────────────

(deftest wasm-i32-comparison-opcodes
  "i32 comparison opcodes."
  (assert-equal #x45 cl-cc::+wasm-i32-eqz+)
  (assert-equal #x46 cl-cc::+wasm-i32-eq+)
  (assert-equal #x47 cl-cc::+wasm-i32-ne+)
  (assert-equal #x48 cl-cc::+wasm-i32-lt-s+)
  (assert-equal #x4a cl-cc::+wasm-i32-gt-s+)
  (assert-equal #x4c cl-cc::+wasm-i32-le-s+)
  (assert-equal #x4e cl-cc::+wasm-i32-ge-s+))

;;; ─── i64 opcodes ───────────────────────────────────────────────────────────

(deftest wasm-i64-comparison-opcodes
  "i64 comparison opcodes."
  (assert-equal #x50 cl-cc::+wasm-i64-eqz+)
  (assert-equal #x51 cl-cc::+wasm-i64-eq+)
  (assert-equal #x52 cl-cc::+wasm-i64-ne+)
  (assert-equal #x53 cl-cc::+wasm-i64-lt-s+)
  (assert-equal #x55 cl-cc::+wasm-i64-gt-s+)
  (assert-equal #x57 cl-cc::+wasm-i64-le-s+)
  (assert-equal #x59 cl-cc::+wasm-i64-ge-s+))

(deftest wasm-i64-arithmetic-opcodes
  "i64 arithmetic opcodes."
  (assert-equal #x7c cl-cc::+wasm-i64-add+)
  (assert-equal #x7d cl-cc::+wasm-i64-sub+)
  (assert-equal #x7e cl-cc::+wasm-i64-mul+)
  (assert-equal #x7f cl-cc::+wasm-i64-div-s+)
  (assert-equal #x81 cl-cc::+wasm-i64-rem-s+)
  (assert-equal #x83 cl-cc::+wasm-i64-and+)
  (assert-equal #x84 cl-cc::+wasm-i64-or+)
  (assert-equal #x85 cl-cc::+wasm-i64-xor+)
  (assert-equal #x86 cl-cc::+wasm-i64-shl+)
  (assert-equal #x87 cl-cc::+wasm-i64-shr-s+))

;;; ─── f64 opcodes ───────────────────────────────────────────────────────────

(deftest wasm-f64-opcodes
  "f64 arithmetic and math opcodes."
  (assert-equal #xa0 cl-cc::+wasm-f64-add+)
  (assert-equal #xa1 cl-cc::+wasm-f64-sub+)
  (assert-equal #xa2 cl-cc::+wasm-f64-mul+)
  (assert-equal #xa3 cl-cc::+wasm-f64-div+)
  (assert-equal #x9a cl-cc::+wasm-f64-neg+)
  (assert-equal #x99 cl-cc::+wasm-f64-abs+)
  (assert-equal #x9f cl-cc::+wasm-f64-sqrt+))

;;; ─── Conversion opcodes ────────────────────────────────────────────────────

(deftest wasm-conversion-opcodes
  "Integer/float conversion opcodes."
  (assert-equal #xa7 cl-cc::+wasm-i32-wrap-i64+)
  (assert-equal #xac cl-cc::+wasm-i64-extend-i32-s+)
  (assert-equal #xb9 cl-cc::+wasm-f64-convert-i64-s+)
  (assert-equal #xb0 cl-cc::+wasm-i64-trunc-f64-s+))

;;; ─── Reference opcodes ─────────────────────────────────────────────────────

(deftest wasm-reference-opcodes
  "Reference manipulation opcodes."
  (assert-equal #xd0 cl-cc::+wasm-ref-null+)
  (assert-equal #xd1 cl-cc::+wasm-ref-is-null+)
  (assert-equal #xd2 cl-cc::+wasm-ref-func+)
  (assert-equal #xd3 cl-cc::+wasm-ref-eq+))

;;; ─── GC opcodes ────────────────────────────────────────────────────────────

(deftest wasm-gc-prefix
  "GC opcode prefix byte."
  (assert-equal #xfb cl-cc::+wasm-gc-prefix+))

(deftest wasm-gc-struct-opcodes
  "GC struct manipulation opcodes."
  (assert-equal 0 cl-cc::+wasm-gc-struct-new+)
  (assert-equal 1 cl-cc::+wasm-gc-struct-new-default+)
  (assert-equal 2 cl-cc::+wasm-gc-struct-get+)
  (assert-equal 3 cl-cc::+wasm-gc-struct-get-s+)
  (assert-equal 4 cl-cc::+wasm-gc-struct-get-u+)
  (assert-equal 5 cl-cc::+wasm-gc-struct-set+))

(deftest wasm-gc-array-opcodes
  "GC array manipulation opcodes."
  (assert-equal 6  cl-cc::+wasm-gc-array-new+)
  (assert-equal 7  cl-cc::+wasm-gc-array-new-default+)
  (assert-equal 8  cl-cc::+wasm-gc-array-new-fixed+)
  (assert-equal 11 cl-cc::+wasm-gc-array-get+)
  (assert-equal 14 cl-cc::+wasm-gc-array-set+)
  (assert-equal 15 cl-cc::+wasm-gc-array-len+))

(deftest wasm-gc-ref-opcodes
  "GC reference test/cast opcodes."
  (assert-equal 20 cl-cc::+wasm-gc-ref-test+)
  (assert-equal 22 cl-cc::+wasm-gc-ref-cast+)
  (assert-equal 28 cl-cc::+wasm-gc-ref-i31+)
  (assert-equal 29 cl-cc::+wasm-gc-i31-get-s+)
  (assert-equal 30 cl-cc::+wasm-gc-i31-get-u+))

;;; ─── Named type indices ────────────────────────────────────────────────────

(deftest wasm-predefined-type-indices
  "Named type indices are contiguous starting from 0."
  (assert-equal 0  cl-cc::+type-idx-main-func+)
  (assert-equal 1  cl-cc::+type-idx-bytes-array+)
  (assert-equal 2  cl-cc::+type-idx-string+)
  (assert-equal 3  cl-cc::+type-idx-symbol+)
  (assert-equal 4  cl-cc::+type-idx-cons+)
  (assert-equal 5  cl-cc::+type-idx-eqref-array+)
  (assert-equal 6  cl-cc::+type-idx-env+)
  (assert-equal 7  cl-cc::+type-idx-closure+)
  (assert-equal 8  cl-cc::+type-idx-class-meta+)
  (assert-equal 9  cl-cc::+type-idx-instance+)
  (assert-equal 10 cl-cc::+type-idx-htable+)
  (assert-equal 11 cl-cc::+type-idx-float+)
  (assert-equal 12 cl-cc::+type-idx-char+))

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
