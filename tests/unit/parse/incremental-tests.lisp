;;;; tests/unit/parse/incremental-tests.lisp — Incremental Parser Tests
;;;;
;;;; Tests for tree-sitter style incremental parsing: edit operations,
;;;; geometric predicates, byte shifting, minimal reparse detection,
;;;; tree rebuilding, parse cache, and CST equality.

(in-package :cl-cc/test)

(defsuite incremental-suite :description "Incremental parser unit tests"
  :parent cl-cc-unit-suite)


(in-suite incremental-suite)
;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-test-cst-token (kind value start end)
  "Create a CST token with given byte range."
  (cl-cc::make-cst-token :kind kind :value value
                          :start-byte start :end-byte end))

(defun make-test-interior (kind start end children)
  "Create a CST interior node with given byte range and children."
  (cl-cc/parse::make-cst-interior :kind kind :start-byte start :end-byte end
                             :children children))

(defun make-test-edit (start old-end new-end)
  "Create an edit operation."
  (cl-cc/parse::make-edit-operation :start-byte start
                               :old-end-byte old-end
                               :new-end-byte new-end))

;;; ─── edit-operation struct ──────────────────────────────────────────────────

(deftest incr-edit-operation-accessors
  "edit-operation struct has correct fields."
  (let ((e (make-test-edit 10 20 25)))
    (assert-equal 10 (cl-cc/parse::edit-operation-start-byte e))
    (assert-equal 20 (cl-cc/parse::edit-operation-old-end-byte e))
    (assert-equal 25 (cl-cc/parse::edit-operation-new-end-byte e))))

;;; ─── Geometric Predicates ──────────────────────────────────────────────────

(deftest-each incr-overlaps-edit-cases
  "cst-overlaps-edit-p returns correct result for each node position."
  ((node-start node-end expected)
   (5  15 t)
   (0  5  nil)
   (25 30 nil))
  (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
        (edit (make-test-edit 10 20 25)))
    (assert-equal expected (cl-cc/parse::cst-overlaps-edit-p node edit))))

(deftest-each incr-before-after-edit-cases
  "cst-before-edit-p and cst-after-edit-p return correct results for each node position."
  ((predicate node-start node-end expected)
   (cl-cc/parse::cst-before-edit-p  0  10 t)
   (cl-cc/parse::cst-before-edit-p  0  15 nil)
   (cl-cc/parse::cst-after-edit-p  20  30 t)
   (cl-cc/parse::cst-after-edit-p  15  25 nil))
  (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
        (edit (make-test-edit 10 20 25)))
    (assert-equal expected (funcall predicate node edit))))

(deftest-each incr-reuse-p-cases
  "cst-reuse-p returns correct result for node before vs overlapping edit."
  ((node-start node-end expected)
   (0  5 t)
   (5 15 nil))
  (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
        (edit (make-test-edit 10 20 25)))
    (assert-equal expected (cl-cc/parse::cst-reuse-p node edit))))

;;; ─── Byte Shifting ──────────────────────────────────────────────────────────

(deftest-each incr-edit-byte-delta-cases
  "edit-byte-delta returns correct signed delta for insertion, deletion, and replacement."
  ((old-end new-end expected)
   (10 15   5)
   (20 15  -5)
   (20 20   0))
  (let ((edit (make-test-edit 10 old-end new-end)))
    (assert-equal expected (cl-cc/parse::edit-byte-delta edit))))

(deftest-each incr-shift-bytes-cases
  "cst-shift-bytes adjusts byte positions for tokens, interior nodes, error nodes, and negative deltas."
  ((label)
   (:token)
   (:interior)
   (:error-node)
   (:negative))
  (ecase label
    (:token
     (let* ((tok (make-test-cst-token :T-INT 42 10 15))
            (shifted (cl-cc/parse::cst-shift-bytes tok 5)))
       (assert-true (cl-cc/parse::cst-token-p shifted))
       (assert-equal 15 (cl-cc/parse::cst-node-start-byte shifted))
       (assert-equal 20 (cl-cc/parse::cst-node-end-byte shifted))
       (assert-equal 42 (cl-cc/parse::cst-token-value shifted))))
    (:interior
     (let* ((child (make-test-cst-token :T-INT 1 5 10))
            (parent (make-test-interior :list 0 20 (list child)))
            (shifted (cl-cc/parse::cst-shift-bytes parent 10)))
       (assert-true (cl-cc/parse::cst-interior-p shifted))
       (assert-equal 10 (cl-cc/parse::cst-node-start-byte shifted))
       (assert-equal 30 (cl-cc/parse::cst-node-end-byte shifted))
       (let ((shifted-child (first (cl-cc/parse::cst-interior-children shifted))))
         (assert-equal 15 (cl-cc/parse::cst-node-start-byte shifted-child))
         (assert-equal 20 (cl-cc/parse::cst-node-end-byte shifted-child)))))
    (:error-node
     (let* ((err (cl-cc/parse::make-cst-error-node :kind :error :start-byte 10 :end-byte 20
                                              :message "bad token"))
            (shifted (cl-cc/parse::cst-shift-bytes err 5)))
       (assert-true (cl-cc/parse::cst-error-p shifted))
       (assert-equal 15 (cl-cc/parse::cst-node-start-byte shifted))
       (assert-equal 25 (cl-cc/parse::cst-node-end-byte shifted))))
    (:negative
     (let* ((tok (make-test-cst-token :T-INT 1 20 30))
            (shifted (cl-cc/parse::cst-shift-bytes tok -5)))
       (assert-equal 15 (cl-cc/parse::cst-node-start-byte shifted))
       (assert-equal 25 (cl-cc/parse::cst-node-end-byte shifted))))))

;;; ─── Minimal Reparse Detection ──────────────────────────────────────────────

(deftest incr-find-minimal-reparse-leaf
  "find-minimal-reparse-node returns leaf node directly."
  (let ((leaf (make-test-cst-token :T-INT 1 0 10))
        (edit (make-test-edit 0 5 5)))
    (assert-eq leaf (cl-cc/parse::find-minimal-reparse-node leaf edit))))

(deftest incr-find-minimal-reparse-single-child
  "find-minimal-reparse-node recurses into single overlapping child."
  (let* ((child1 (make-test-cst-token :T-INT 1 0 5))
         (child2 (make-test-interior :list 5 15
                   (list (make-test-cst-token :T-INT 2 5 10)
                         (make-test-cst-token :T-INT 3 10 15))))
         (child3 (make-test-cst-token :T-INT 4 15 20))
         (root (make-test-interior :root 0 20 (list child1 child2 child3)))
         (edit (make-test-edit 7 12 12)))
    ;; Edit overlaps only child2 → should recurse into it
    (let ((result (cl-cc/parse::find-minimal-reparse-node root edit)))
      (assert-eq child2 result))))

(deftest incr-find-minimal-reparse-multiple-children
  "find-minimal-reparse-node returns parent when edit spans multiple children."
  (let* ((child1 (make-test-interior :a 0 10
                   (list (make-test-cst-token :T-INT 1 0 10))))
         (child2 (make-test-interior :b 10 20
                   (list (make-test-cst-token :T-INT 2 10 20))))
         (root (make-test-interior :root 0 20 (list child1 child2)))
         (edit (make-test-edit 5 15 15)))
    ;; Edit spans both children → root is the minimal node
    (assert-eq root (cl-cc/parse::find-minimal-reparse-node root edit))))

;;; ─── Rebuild Tree ──────────────────────────────────────────────────────────

(deftest incr-rebuild-tree-basic
  "rebuild-tree combines before, new, and shifted-after nodes."
  (let* ((before (make-test-cst-token :T-INT 1 0 5))
         (overlap (make-test-cst-token :T-INT 2 5 15))
         (after (make-test-cst-token :T-INT 3 15 20))
         (edit (make-test-edit 5 15 20))  ; insert 5 bytes
         (new-nodes (list (make-test-cst-token :T-INT 99 5 20)))
         (result (cl-cc/parse::rebuild-tree (list before overlap after) edit 5 new-nodes)))
    ;; before + new + shifted-after
    (assert-equal 3 (length result))
    ;; First: before node unchanged
    (assert-equal 0 (cl-cc/parse::cst-node-start-byte (first result)))
    ;; Second: new node
    (assert-equal 99 (cl-cc/parse::cst-token-value (second result)))
    ;; Third: after node shifted by +5
    (assert-equal 20 (cl-cc/parse::cst-node-start-byte (third result)))
    (assert-equal 25 (cl-cc/parse::cst-node-end-byte (third result)))))

;;; ─── Parse Cache ───────────────────────────────────────────────────────────

(deftest incr-cache-operations
  "cache-store, cache-lookup, and invalidate-parse-cache work correctly."
  (let ((cl-cc/parse::*parse-cache* (make-hash-table :test 'equal)))
    (let ((nodes (list (make-test-cst-token :T-INT 42 0 2))))
      (cl-cc/parse::cache-store "42" nodes)
      (assert-equal nodes (cl-cc/parse::cache-lookup "42"))))
  (let ((cl-cc/parse::*parse-cache* (make-hash-table :test 'equal)))
    (assert-null (cl-cc/parse::cache-lookup "unknown")))
  (let ((cl-cc/parse::*parse-cache* (make-hash-table :test 'equal)))
    (cl-cc/parse::cache-store "a" '(1))
    (cl-cc/parse::cache-store "b" '(2))
    (cl-cc/parse::invalidate-parse-cache)
    (assert-null (cl-cc/parse::cache-lookup "a"))
    (assert-null (cl-cc/parse::cache-lookup "b"))))

(deftest incr-content-hash-deterministic
  "content-hash returns same value for same string."
  (assert-equal (cl-cc/parse::content-hash "hello")
                (cl-cc/parse::content-hash "hello")))

;;; ─── CST Equality ──────────────────────────────────────────────────────────

(deftest-each incr-cst-equal-token-cases
  "cst-equal-p returns correct result for identical vs differing tokens."
  ((a-kind a-val b-kind b-val expected)
   (:T-INT   42 :T-INT   42 t)
   (:T-INT   42 :T-INT   99 nil)
   (:T-INT    1 :T-IDENT  1 nil))
  (let ((a (make-test-cst-token a-kind a-val 0 2))
        (b (make-test-cst-token b-kind b-val 0 2)))
    (assert-equal expected (cl-cc/parse::cst-equal-p a b))))

(deftest incr-cst-equal-interior
  "cst-equal-p returns true for structurally equal interior nodes."
  (let ((a (make-test-interior :list 0 10
             (list (make-test-cst-token :T-INT 1 0 5)
                   (make-test-cst-token :T-INT 2 5 10))))
        (b (make-test-interior :list 0 10
             (list (make-test-cst-token :T-INT 1 0 5)
                   (make-test-cst-token :T-INT 2 5 10)))))
    (assert-true (cl-cc/parse::cst-equal-p a b))))

(deftest incr-cst-equal-nil
  "cst-equal-p returns true for nil/nil, false for nil/non-nil."
  (assert-true (cl-cc/parse::cst-equal-p nil nil))
  (assert-false (cl-cc/parse::cst-equal-p nil (make-test-cst-token :T-INT 1 0 1)))
  (assert-false (cl-cc/parse::cst-equal-p (make-test-cst-token :T-INT 1 0 1) nil)))

(deftest-each incr-cst-equal-error-node-cases
  "cst-equal-p compares error nodes by message: equal messages → true, differing → false."
  ((msg-a msg-b expected)
   ("bad"  "bad"   t)
   ("bad"  "worse" nil))
  (let ((a (cl-cc/parse::make-cst-error-node :kind :error :start-byte 0 :end-byte 5
                                        :message msg-a))
        (b (cl-cc/parse::make-cst-error-node :kind :error :start-byte 0 :end-byte 5
                                        :message msg-b)))
    (assert-equal expected (cl-cc/parse::cst-equal-p a b))))
