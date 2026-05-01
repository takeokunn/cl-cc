;;;; tests/unit/emit/wasm-trampoline-tests.lisp — WASM Trampoline Builder Tests
;;;;
;;;; Tests for src/emit/wasm-trampoline.lisp:
;;;; group-into-basic-blocks, build-label-pc-map, wasm-basic-block struct

(in-package :cl-cc/test)

(defsuite wasm-trampoline-suite :description "WASM trampoline builder tests"
  :parent cl-cc-unit-suite)


(in-suite wasm-trampoline-suite)
;;; ─── group-into-basic-blocks ──────────────────────────────────────────────────

(deftest trampoline-bb-empty
  "group-into-basic-blocks on empty list returns empty."
  (assert-null (cl-cc/emit::group-into-basic-blocks nil)))

(deftest trampoline-bb-no-labels
  "Instructions with no labels form a single basic block with nil label."
  (let* ((instrs (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs)))
    (assert-equal 1 (length bbs))
    (assert-null (cl-cc/emit::wasm-bb-label (first bbs)))
    (assert-equal 0 (cl-cc/emit::wasm-bb-pc-index (first bbs)))
    (assert-equal 2 (length (cl-cc/emit::wasm-bb-instructions (first bbs))))))

(deftest trampoline-bb-label-grouping
  "group-into-basic-blocks correctly groups instructions across label boundary cases."
  ;; single label followed by instructions → one block
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 42)
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs)))
    (assert-equal 1 (length bbs))
    (assert-equal "entry" (cl-cc/emit::wasm-bb-label (first bbs)))
    (assert-equal 0 (cl-cc/emit::wasm-bb-pc-index (first bbs)))
    (assert-equal 2 (length (cl-cc/emit::wasm-bb-instructions (first bbs)))))
  ;; two labels produce two blocks with ascending pc-index
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-label :name "b")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs)))
    (assert-equal 2 (length bbs))
    (assert-equal "a" (cl-cc/emit::wasm-bb-label (first bbs)))
    (assert-equal 0 (cl-cc/emit::wasm-bb-pc-index (first bbs)))
    (assert-equal "b" (cl-cc/emit::wasm-bb-label (second bbs)))
    (assert-equal 1 (cl-cc/emit::wasm-bb-pc-index (second bbs))))
  ;; instructions before first label form a nil-label entry block
  (let* ((instrs (list (make-vm-const :dst :r0 :value 0)
                       (make-vm-label :name "loop")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs)))
    (assert-equal 2 (length bbs))
    (assert-null (cl-cc/emit::wasm-bb-label (first bbs)))
    (assert-equal 1 (length (cl-cc/emit::wasm-bb-instructions (first bbs))))
    (assert-equal "loop" (cl-cc/emit::wasm-bb-label (second bbs))))
  ;; two consecutive labels with no instructions between produce two blocks
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-label :name "b")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs)))
    (assert-equal 2 (length bbs))
    (assert-equal "a" (cl-cc/emit::wasm-bb-label (first bbs)))
    (assert-equal 0 (length (cl-cc/emit::wasm-bb-instructions (first bbs))))
    (assert-equal "b" (cl-cc/emit::wasm-bb-label (second bbs)))))

(deftest trampoline-bb-struct-predicate
  "wasm-basic-block-p recognizes wasm-basic-block structs."
  (let* ((instrs (list (make-vm-label :name "x")))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs)))
    (assert-true (cl-cc/emit::wasm-basic-block-p (first bbs)))))

;;; ─── build-label-pc-map ───────────────────────────────────────────────────────

(deftest trampoline-pc-map-zero-entries
  "build-label-pc-map returns a 0-count map for empty input or instructions with no labels."
  (assert-true (hash-table-p (cl-cc/emit::build-label-pc-map nil)))
  (assert-equal 0 (hash-table-count (cl-cc/emit::build-label-pc-map nil)))
  (let* ((instrs (list (make-vm-const :dst :r0 :value 1)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs))
         (m (cl-cc/emit::build-label-pc-map bbs)))
    (assert-equal 0 (hash-table-count m))))

(deftest trampoline-pc-map-labels
  "build-label-pc-map maps labels to correct pc-indices for single and multiple labels."
  ;; single label maps to pc-index 0
  (let* ((instrs (list (make-vm-label :name "main")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs))
         (m (cl-cc/emit::build-label-pc-map bbs)))
    (assert-equal 1 (hash-table-count m))
    (assert-equal 0 (gethash "main" m)))
  ;; three labels each map to their correct ascending pc-index
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-label :name "b")
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-label :name "c")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs))
         (m (cl-cc/emit::build-label-pc-map bbs)))
    (assert-equal 3 (hash-table-count m))
    (assert-equal 0 (gethash "a" m))
    (assert-equal 1 (gethash "b" m))
    (assert-equal 2 (gethash "c" m))))

(deftest trampoline-pc-map-with-implicit-entry
  "build-label-pc-map correctly handles implicit entry block + labeled blocks."
  (let* ((instrs (list (make-vm-const :dst :r0 :value 0)
                       (make-vm-label :name "loop")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/emit::group-into-basic-blocks instrs))
         (m (cl-cc/emit::build-label-pc-map bbs)))
    ;; Only the labeled block appears in the map
    (assert-equal 1 (hash-table-count m))
    (assert-equal 1 (gethash "loop" m))))

;;; ─── %make-eq-hash-table ────────────────────────────────────────────────────

(deftest-each trampoline-make-eq-hash-table
  "%make-eq-hash-table builds a correct EQ hash-table from an alist."
  :cases (("dotted-pair"  '((a . 1) (b . 2))  'a  1)
          ("list-style"   '((x 10))            'x  10)
          ("two-keys"     '((m . "v1") (n . "v2")) 'n "v2"))
  (alist key expected)
  (let ((ht (cl-cc/emit::%make-eq-hash-table alist)))
    (assert-equal expected (gethash key ht))))

(deftest trampoline-make-eq-hash-table-size
  "%make-eq-hash-table creates a table with the correct number of entries."
  (let ((ht (cl-cc/emit::%make-eq-hash-table '((a . 1) (b . 2) (c . 3)))))
    (assert-equal 3 (hash-table-count ht))))

;;; ─── WASM dispatch tables ───────────────────────────────────────────────────

(deftest-each trampoline-binop-table-lookups
  "*wasm-i64-binop-table* maps VM binary instruction types to i64 opcode strings."
  :cases (("vm-add"    'vm-add         "i64.add")
          ("vm-sub"    'vm-sub         "i64.sub")
          ("vm-mul"    'vm-mul         "i64.mul")
          ("vm-logand" 'vm-logand      "i64.and")
          ("vm-logior" 'vm-logior      "i64.or")
          ("vm-logxor" 'vm-logxor      "i64.xor"))
  (type-sym expected-opcode)
  (assert-equal expected-opcode (gethash type-sym cl-cc/emit::*wasm-i64-binop-table*)))

(deftest-each trampoline-cmp-table-lookups
  "*wasm-i64-cmp-table* maps VM comparison instruction types to i64 opcode strings."
  :cases (("vm-lt"     'vm-lt    "i64.lt_s")
          ("vm-gt"     'vm-gt    "i64.gt_s")
          ("vm-le"     'vm-le    "i64.le_s")
          ("vm-ge"     'vm-ge    "i64.ge_s")
          ("vm-eq"     'vm-eq    "i64.eq"))
  (type-sym expected-opcode)
  (assert-equal expected-opcode (gethash type-sym cl-cc/emit::*wasm-i64-cmp-table*)))

(deftest-each trampoline-unary-table-lookups
  "*wasm-unary-fixnum-table* maps VM unary instruction types to i64 format strings."
  :cases (("vm-inc"      'vm-inc      "(i64.add ~A (i64.const 1))")
          ("vm-dec"      'vm-dec      "(i64.sub ~A (i64.const 1))")
          ("vm-neg"      'vm-neg      "(i64.sub (i64.const 0) ~A)")
          ("vm-lognot"   'vm-lognot   "(i64.xor ~A (i64.const -1))")
          ("vm-logcount" 'vm-logcount "(i64.popcnt ~A)"))
  (type-sym expected-fmt)
  (assert-equal expected-fmt (gethash type-sym cl-cc/emit::*wasm-unary-fixnum-table*)))

(deftest-each trampoline-minmax-table-lookups
  "*wasm-minmax-table* maps min/max instruction types to WASM comparison opcodes."
  :cases (("vm-min" 'vm-min "i64.le_s")
          ("vm-max" 'vm-max "i64.ge_s"))
  (type-sym expected-opcode)
  (assert-equal expected-opcode (gethash type-sym cl-cc/emit::*wasm-minmax-table*)))

(deftest trampoline-struct-get-table-entries
  "*wasm-struct-get-table* covers vm-car and vm-cdr with struct.get format strings."
  (assert-true (search "struct.get $cons_t 0"
                       (gethash 'vm-car cl-cc/emit::*wasm-struct-get-table*)))
  (assert-true (search "struct.get $cons_t 1"
                       (gethash 'vm-cdr cl-cc/emit::*wasm-struct-get-table*))))

;;; ─── %wasm-const-value-to-wat ─────────────────────────────────────────────

(deftest-each wasm-const-value-to-wat-cases
  "%wasm-const-value-to-wat maps CL constant values to WASM WAT strings."
  :cases (("integer-42"  42    "(ref.i31")
          ("nil"         nil   "(ref.null eq)")
          ("true"        t     "(ref.i31 (i32.const 1))"))
  (val expected-prefix)
  (let ((result (cl-cc/emit::%wasm-const-value-to-wat val)))
    (assert-true (stringp result))
    (assert-true (or (string= result expected-prefix)
                     (and (>= (length result) (length expected-prefix))
                          (string= expected-prefix result :end2 (length expected-prefix)))))))

(deftest wasm-const-value-to-wat-string-signals-error
  "%wasm-const-value-to-wat signals error for unsupported string constants."
  (assert-signals error (cl-cc/emit::%wasm-const-value-to-wat "unsupported")))

;;; ─── %wasm-if-eqref ────────────────────────────────────────────────────────

(deftest-each wasm-if-eqref-structure
  "%wasm-if-eqref produces (if (result eqref) ...) WAT strings."
  :cases (("basic" "(i64.ge_s x (i64.const 0))" "then-wat" "else-wat"))
  (cond-wat then-wat else-wat)
  (let ((result (cl-cc/emit::%wasm-if-eqref cond-wat then-wat else-wat)))
    (assert-true (search "(if (result eqref)" result))
    (assert-true (search cond-wat result))
    (assert-true (search "(then" result))
    (assert-true (search "(else" result))))
