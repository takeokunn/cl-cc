;;;; tests/unit/emit/wasm-trampoline-tests.lisp — WASM Trampoline Builder Tests
;;;;
;;;; Tests for src/emit/wasm-trampoline.lisp:
;;;; group-into-basic-blocks, build-label-pc-map, wasm-basic-block struct

(in-package :cl-cc/test)

(defsuite wasm-trampoline-suite :description "WASM trampoline builder tests")

;;; ─── group-into-basic-blocks ──────────────────────────────────────────────────

(deftest trampoline-bb-empty
  "group-into-basic-blocks on empty list returns empty."
  (assert-null (cl-cc::group-into-basic-blocks nil)))

(deftest trampoline-bb-no-labels
  "Instructions with no labels form a single basic block with nil label."
  (let* ((instrs (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc::group-into-basic-blocks instrs)))
    (assert-equal 1 (length bbs))
    (assert-null (cl-cc::wasm-bb-label (first bbs)))
    (assert-equal 0 (cl-cc::wasm-bb-pc-index (first bbs)))
    (assert-equal 2 (length (cl-cc::wasm-bb-instructions (first bbs))))))

(deftest trampoline-bb-single-label
  "A label followed by instructions produces one basic block."
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 42)
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc::group-into-basic-blocks instrs)))
    (assert-equal 1 (length bbs))
    (assert-equal "entry" (cl-cc::wasm-bb-label (first bbs)))
    (assert-equal 0 (cl-cc::wasm-bb-pc-index (first bbs)))
    (assert-equal 2 (length (cl-cc::wasm-bb-instructions (first bbs))))))

(deftest trampoline-bb-two-labels
  "Two labels produce two basic blocks with ascending pc-index."
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-label :name "b")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc::group-into-basic-blocks instrs)))
    (assert-equal 2 (length bbs))
    (assert-equal "a" (cl-cc::wasm-bb-label (first bbs)))
    (assert-equal 0 (cl-cc::wasm-bb-pc-index (first bbs)))
    (assert-equal "b" (cl-cc::wasm-bb-label (second bbs)))
    (assert-equal 1 (cl-cc::wasm-bb-pc-index (second bbs)))))

(deftest trampoline-bb-instructions-before-first-label
  "Instructions before the first label form a nil-label entry block."
  (let* ((instrs (list (make-vm-const :dst :r0 :value 0)
                       (make-vm-label :name "loop")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc::group-into-basic-blocks instrs)))
    (assert-equal 2 (length bbs))
    (assert-null (cl-cc::wasm-bb-label (first bbs)))
    (assert-equal 1 (length (cl-cc::wasm-bb-instructions (first bbs))))
    (assert-equal "loop" (cl-cc::wasm-bb-label (second bbs)))))

(deftest trampoline-bb-consecutive-labels
  "Two consecutive labels with no instructions between produce two blocks."
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-label :name "b")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc::group-into-basic-blocks instrs)))
    (assert-equal 2 (length bbs))
    (assert-equal "a" (cl-cc::wasm-bb-label (first bbs)))
    (assert-equal 0 (length (cl-cc::wasm-bb-instructions (first bbs))))
    (assert-equal "b" (cl-cc::wasm-bb-label (second bbs)))))

(deftest trampoline-bb-struct-predicate
  "wasm-basic-block-p recognizes wasm-basic-block structs."
  (let* ((instrs (list (make-vm-label :name "x")))
         (bbs (cl-cc::group-into-basic-blocks instrs)))
    (assert-true (cl-cc::wasm-basic-block-p (first bbs)))))

;;; ─── build-label-pc-map ───────────────────────────────────────────────────────

(deftest trampoline-pc-map-empty
  "build-label-pc-map on empty list returns empty hash table."
  (let ((m (cl-cc::build-label-pc-map nil)))
    (assert-true (hash-table-p m))
    (assert-equal 0 (hash-table-count m))))

(deftest trampoline-pc-map-nil-label-excluded
  "build-label-pc-map skips blocks with nil label."
  (let* ((instrs (list (make-vm-const :dst :r0 :value 1)))
         (bbs (cl-cc::group-into-basic-blocks instrs))
         (m (cl-cc::build-label-pc-map bbs)))
    (assert-equal 0 (hash-table-count m))))

(deftest trampoline-pc-map-single
  "build-label-pc-map maps a single label to its pc-index."
  (let* ((instrs (list (make-vm-label :name "main")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc::group-into-basic-blocks instrs))
         (m (cl-cc::build-label-pc-map bbs)))
    (assert-equal 1 (hash-table-count m))
    (assert-equal 0 (gethash "main" m))))

(deftest trampoline-pc-map-multiple
  "build-label-pc-map maps each label to its correct pc-index."
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-label :name "b")
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-label :name "c")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc::group-into-basic-blocks instrs))
         (m (cl-cc::build-label-pc-map bbs)))
    (assert-equal 3 (hash-table-count m))
    (assert-equal 0 (gethash "a" m))
    (assert-equal 1 (gethash "b" m))
    (assert-equal 2 (gethash "c" m))))

(deftest trampoline-pc-map-with-implicit-entry
  "build-label-pc-map correctly handles implicit entry block + labeled blocks."
  (let* ((instrs (list (make-vm-const :dst :r0 :value 0)
                       (make-vm-label :name "loop")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc::group-into-basic-blocks instrs))
         (m (cl-cc::build-label-pc-map bbs)))
    ;; Only the labeled block appears in the map
    (assert-equal 1 (hash-table-count m))
    (assert-equal 1 (gethash "loop" m))))
