;;;; tests/unit/parse/incremental-tests.lisp — Incremental Parser Tests
;;;;
;;;; Tests for tree-sitter style incremental parsing: edit operations,
;;;; geometric predicates, byte shifting, minimal reparse detection,
;;;; tree rebuilding, parse cache, and CST equality.

(in-package :cl-cc/test)

(defsuite incremental-suite :description "Incremental parser unit tests")

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-test-token (kind value start end)
  "Create a CST token with given byte range."
  (cl-cc::make-cst-token :kind kind :value value
                          :start-byte start :end-byte end))

(defun make-test-interior (kind start end children)
  "Create a CST interior node with given byte range and children."
  (cl-cc::make-cst-interior :kind kind :start-byte start :end-byte end
                             :children children))

(defun make-test-edit (start old-end new-end)
  "Create an edit operation."
  (cl-cc::make-edit-operation :start-byte start
                               :old-end-byte old-end
                               :new-end-byte new-end))

;;; ─── edit-operation struct ──────────────────────────────────────────────────

(deftest incr-edit-operation-accessors
  "edit-operation struct has correct fields."
  (let ((e (make-test-edit 10 20 25)))
    (assert-equal 10 (cl-cc::edit-operation-start-byte e))
    (assert-equal 20 (cl-cc::edit-operation-old-end-byte e))
    (assert-equal 25 (cl-cc::edit-operation-new-end-byte e))))

;;; ─── Geometric Predicates ──────────────────────────────────────────────────

(deftest incr-overlaps-edit-true
  "cst-overlaps-edit-p returns true when node spans edit region."
  (let ((node (make-test-token :T-INT 1 5 15))
        (edit (make-test-edit 10 20 25)))
    (assert-true (cl-cc::cst-overlaps-edit-p node edit))))

(deftest incr-overlaps-edit-false-before
  "cst-overlaps-edit-p returns false when node is entirely before edit."
  (let ((node (make-test-token :T-INT 1 0 5))
        (edit (make-test-edit 10 20 25)))
    (assert-false (cl-cc::cst-overlaps-edit-p node edit))))

(deftest incr-overlaps-edit-false-after
  "cst-overlaps-edit-p returns false when node is entirely after edit."
  (let ((node (make-test-token :T-INT 1 25 30))
        (edit (make-test-edit 10 20 25)))
    (assert-false (cl-cc::cst-overlaps-edit-p node edit))))

(deftest incr-before-edit-true
  "cst-before-edit-p returns true when node ends at or before edit start."
  (let ((node (make-test-token :T-INT 1 0 10))
        (edit (make-test-edit 10 20 25)))
    (assert-true (cl-cc::cst-before-edit-p node edit))))

(deftest incr-before-edit-false
  "cst-before-edit-p returns false when node extends past edit start."
  (let ((node (make-test-token :T-INT 1 0 15))
        (edit (make-test-edit 10 20 25)))
    (assert-false (cl-cc::cst-before-edit-p node edit))))

(deftest incr-after-edit-true
  "cst-after-edit-p returns true when node starts at or after edit old-end."
  (let ((node (make-test-token :T-INT 1 20 30))
        (edit (make-test-edit 10 20 25)))
    (assert-true (cl-cc::cst-after-edit-p node edit))))

(deftest incr-after-edit-false
  "cst-after-edit-p returns false when node starts before edit old-end."
  (let ((node (make-test-token :T-INT 1 15 25))
        (edit (make-test-edit 10 20 25)))
    (assert-false (cl-cc::cst-after-edit-p node edit))))

(deftest incr-reuse-p-before
  "cst-reuse-p returns true for node before edit."
  (let ((node (make-test-token :T-INT 1 0 5))
        (edit (make-test-edit 10 20 25)))
    (assert-true (cl-cc::cst-reuse-p node edit))))

(deftest incr-reuse-p-overlapping
  "cst-reuse-p returns false for overlapping node."
  (let ((node (make-test-token :T-INT 1 5 15))
        (edit (make-test-edit 10 20 25)))
    (assert-false (cl-cc::cst-reuse-p node edit))))

;;; ─── Byte Shifting ──────────────────────────────────────────────────────────

(deftest incr-edit-byte-delta-positive
  "edit-byte-delta returns positive delta for insertion."
  (let ((edit (make-test-edit 10 10 15)))
    (assert-equal 5 (cl-cc::edit-byte-delta edit))))

(deftest incr-edit-byte-delta-negative
  "edit-byte-delta returns negative delta for deletion."
  (let ((edit (make-test-edit 10 20 15)))
    (assert-equal -5 (cl-cc::edit-byte-delta edit))))

(deftest incr-edit-byte-delta-zero
  "edit-byte-delta returns zero for same-length replacement."
  (let ((edit (make-test-edit 10 20 20)))
    (assert-equal 0 (cl-cc::edit-byte-delta edit))))

(deftest incr-shift-bytes-token
  "cst-shift-bytes shifts token byte positions."
  (let* ((tok (make-test-token :T-INT 42 10 15))
         (shifted (cl-cc::cst-shift-bytes tok 5)))
    (assert-true (cl-cc::cst-token-p shifted))
    (assert-equal 15 (cl-cc::cst-node-start-byte shifted))
    (assert-equal 20 (cl-cc::cst-node-end-byte shifted))
    (assert-equal 42 (cl-cc::cst-token-value shifted))))

(deftest incr-shift-bytes-interior
  "cst-shift-bytes recursively shifts interior node and children."
  (let* ((child (make-test-token :T-INT 1 5 10))
         (parent (make-test-interior :list 0 20 (list child)))
         (shifted (cl-cc::cst-shift-bytes parent 10)))
    (assert-true (cl-cc::cst-interior-p shifted))
    (assert-equal 10 (cl-cc::cst-node-start-byte shifted))
    (assert-equal 30 (cl-cc::cst-node-end-byte shifted))
    (let ((shifted-child (first (cl-cc::cst-interior-children shifted))))
      (assert-equal 15 (cl-cc::cst-node-start-byte shifted-child))
      (assert-equal 20 (cl-cc::cst-node-end-byte shifted-child)))))

(deftest incr-shift-bytes-error-node
  "cst-shift-bytes shifts error node byte positions."
  (let* ((err (cl-cc::make-cst-error-node :kind :error :start-byte 10 :end-byte 20
                                           :message "bad token"))
         (shifted (cl-cc::cst-shift-bytes err 5)))
    (assert-true (cl-cc::cst-error-p shifted))
    (assert-equal 15 (cl-cc::cst-node-start-byte shifted))
    (assert-equal 25 (cl-cc::cst-node-end-byte shifted))))

(deftest incr-shift-bytes-negative
  "cst-shift-bytes handles negative delta."
  (let* ((tok (make-test-token :T-INT 1 20 30))
         (shifted (cl-cc::cst-shift-bytes tok -5)))
    (assert-equal 15 (cl-cc::cst-node-start-byte shifted))
    (assert-equal 25 (cl-cc::cst-node-end-byte shifted))))

;;; ─── Minimal Reparse Detection ──────────────────────────────────────────────

(deftest incr-find-minimal-reparse-leaf
  "find-minimal-reparse-node returns leaf node directly."
  (let ((leaf (make-test-token :T-INT 1 0 10))
        (edit (make-test-edit 0 5 5)))
    (assert-eq leaf (cl-cc::find-minimal-reparse-node leaf edit))))

(deftest incr-find-minimal-reparse-single-child
  "find-minimal-reparse-node recurses into single overlapping child."
  (let* ((child1 (make-test-token :T-INT 1 0 5))
         (child2 (make-test-interior :list 5 15
                   (list (make-test-token :T-INT 2 5 10)
                         (make-test-token :T-INT 3 10 15))))
         (child3 (make-test-token :T-INT 4 15 20))
         (root (make-test-interior :root 0 20 (list child1 child2 child3)))
         (edit (make-test-edit 7 12 12)))
    ;; Edit overlaps only child2 → should recurse into it
    (let ((result (cl-cc::find-minimal-reparse-node root edit)))
      (assert-eq child2 result))))

(deftest incr-find-minimal-reparse-multiple-children
  "find-minimal-reparse-node returns parent when edit spans multiple children."
  (let* ((child1 (make-test-interior :a 0 10
                   (list (make-test-token :T-INT 1 0 10))))
         (child2 (make-test-interior :b 10 20
                   (list (make-test-token :T-INT 2 10 20))))
         (root (make-test-interior :root 0 20 (list child1 child2)))
         (edit (make-test-edit 5 15 15)))
    ;; Edit spans both children → root is the minimal node
    (assert-eq root (cl-cc::find-minimal-reparse-node root edit))))

;;; ─── Rebuild Tree ──────────────────────────────────────────────────────────

(deftest incr-rebuild-tree-basic
  "rebuild-tree combines before, new, and shifted-after nodes."
  (let* ((before (make-test-token :T-INT 1 0 5))
         (overlap (make-test-token :T-INT 2 5 15))
         (after (make-test-token :T-INT 3 15 20))
         (edit (make-test-edit 5 15 20))  ; insert 5 bytes
         (new-nodes (list (make-test-token :T-INT 99 5 20)))
         (result (cl-cc::rebuild-tree (list before overlap after) edit 5 new-nodes)))
    ;; before + new + shifted-after
    (assert-equal 3 (length result))
    ;; First: before node unchanged
    (assert-equal 0 (cl-cc::cst-node-start-byte (first result)))
    ;; Second: new node
    (assert-equal 99 (cl-cc::cst-token-value (second result)))
    ;; Third: after node shifted by +5
    (assert-equal 20 (cl-cc::cst-node-start-byte (third result)))
    (assert-equal 25 (cl-cc::cst-node-end-byte (third result)))))

;;; ─── Parse Cache ───────────────────────────────────────────────────────────

(deftest incr-cache-store-and-lookup
  "cache-store + cache-lookup roundtrips."
  (let ((cl-cc::*parse-cache* (make-hash-table :test 'equal)))
    (let ((nodes (list (make-test-token :T-INT 42 0 2))))
      (cl-cc::cache-store "42" nodes)
      (assert-equal nodes (cl-cc::cache-lookup "42")))))

(deftest incr-cache-lookup-miss
  "cache-lookup returns nil for unknown source."
  (let ((cl-cc::*parse-cache* (make-hash-table :test 'equal)))
    (assert-null (cl-cc::cache-lookup "unknown"))))

(deftest incr-cache-invalidate
  "invalidate-parse-cache clears all entries."
  (let ((cl-cc::*parse-cache* (make-hash-table :test 'equal)))
    (cl-cc::cache-store "a" '(1))
    (cl-cc::cache-store "b" '(2))
    (cl-cc::invalidate-parse-cache)
    (assert-null (cl-cc::cache-lookup "a"))
    (assert-null (cl-cc::cache-lookup "b"))))

(deftest incr-content-hash-deterministic
  "content-hash returns same value for same string."
  (assert-equal (cl-cc::content-hash "hello")
                (cl-cc::content-hash "hello")))

;;; ─── CST Equality ──────────────────────────────────────────────────────────

(deftest incr-cst-equal-tokens
  "cst-equal-p returns true for identical tokens."
  (let ((a (make-test-token :T-INT 42 0 2))
        (b (make-test-token :T-INT 42 0 2)))
    (assert-true (cl-cc::cst-equal-p a b))))

(deftest incr-cst-equal-tokens-differ-value
  "cst-equal-p returns false for different token values."
  (let ((a (make-test-token :T-INT 42 0 2))
        (b (make-test-token :T-INT 99 0 2)))
    (assert-false (cl-cc::cst-equal-p a b))))

(deftest incr-cst-equal-tokens-differ-kind
  "cst-equal-p returns false for different token kinds."
  (let ((a (make-test-token :T-INT 1 0 2))
        (b (make-test-token :T-IDENT 1 0 2)))
    (assert-false (cl-cc::cst-equal-p a b))))

(deftest incr-cst-equal-interior
  "cst-equal-p returns true for structurally equal interior nodes."
  (let ((a (make-test-interior :list 0 10
             (list (make-test-token :T-INT 1 0 5)
                   (make-test-token :T-INT 2 5 10))))
        (b (make-test-interior :list 0 10
             (list (make-test-token :T-INT 1 0 5)
                   (make-test-token :T-INT 2 5 10)))))
    (assert-true (cl-cc::cst-equal-p a b))))

(deftest incr-cst-equal-nil
  "cst-equal-p returns true for nil/nil, false for nil/non-nil."
  (assert-true (cl-cc::cst-equal-p nil nil))
  (assert-false (cl-cc::cst-equal-p nil (make-test-token :T-INT 1 0 1)))
  (assert-false (cl-cc::cst-equal-p (make-test-token :T-INT 1 0 1) nil)))

(deftest incr-cst-equal-error-nodes
  "cst-equal-p compares error nodes by message and position."
  (let ((a (cl-cc::make-cst-error-node :kind :error :start-byte 0 :end-byte 5
                                        :message "bad"))
        (b (cl-cc::make-cst-error-node :kind :error :start-byte 0 :end-byte 5
                                        :message "bad")))
    (assert-true (cl-cc::cst-equal-p a b))))

(deftest incr-cst-equal-error-nodes-differ
  "cst-equal-p returns false for error nodes with different messages."
  (let ((a (cl-cc::make-cst-error-node :kind :error :start-byte 0 :end-byte 5
                                        :message "bad"))
        (b (cl-cc::make-cst-error-node :kind :error :start-byte 0 :end-byte 5
                                        :message "worse")))
    (assert-false (cl-cc::cst-equal-p a b))))
