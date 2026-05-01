;;;; incremental.lisp — Tree-sitter style incremental parsing
;;;;
;;;; Provides infrastructure for incremental re-parsing: given an edit
;;;; operation and a previous CST, determine the minimal sub-tree that
;;;; needs re-parsing and reuse unchanged nodes.

(in-package :cl-cc/parse)

;;; ─── Edit Operations ─────────────────────────────────────────────────────────

(defstruct edit-operation
  "Describes a source edit as a byte range replacement."
  (start-byte    0   :type integer)
  (old-end-byte  0   :type integer)
  (new-end-byte  0   :type integer)
  (new-source    ""  :type string))

;;; ─── Node/Edit Geometric Predicates ──────────────────────────────────────────

(defun cst-overlaps-edit-p (node edit)
  "Does NODE's byte range overlap with EDIT's old byte range?"
  (let ((ns (cst-node-start-byte node))
        (ne (cst-node-end-byte node))
        (es (edit-operation-start-byte edit))
        (ee (edit-operation-old-end-byte edit)))
    (and (< ns ee) (> ne es))))

(defun cst-before-edit-p (node edit)
  "Is NODE entirely before the edited region?"
  (<= (cst-node-end-byte node) (edit-operation-start-byte edit)))

(defun cst-after-edit-p (node edit)
  "Is NODE entirely after the edited region?"
  (>= (cst-node-start-byte node) (edit-operation-old-end-byte edit)))

(defun cst-reuse-p (node edit)
  "Can NODE be reused without re-parsing? True if it does not overlap the edit."
  (not (cst-overlaps-edit-p node edit)))

;;; ─── Byte Shifting ──────────────────────────────────────────────────────────

(defun edit-byte-delta (edit)
  "Return the byte offset delta introduced by EDIT."
  (- (edit-operation-new-end-byte edit)
     (edit-operation-old-end-byte edit)))

(defun cst-shift-bytes (node delta)
  "Return a copy of NODE with all byte positions shifted by DELTA.
   Recursively shifts children for interior nodes."
  (cond
    ((cst-token-p node)
     (make-cst-token :kind (cst-node-kind node)
                     :value (cst-token-value node)
                     :start-byte (+ (cst-node-start-byte node) delta)
                     :end-byte (+ (cst-node-end-byte node) delta)
                     :source-file (cst-node-source-file node)
                     :trivia (cst-token-trivia node)))
    ((cst-interior-p node)
     (make-cst-interior :kind (cst-node-kind node)
                        :start-byte (+ (cst-node-start-byte node) delta)
                        :end-byte (+ (cst-node-end-byte node) delta)
                        :source-file (cst-node-source-file node)
                        :children (mapcar (lambda (c) (cst-shift-bytes c delta))
                                          (cst-interior-children node))
                        :trivia (cst-interior-trivia node)))
    ((cst-error-p node)
     (make-cst-error :kind (cst-node-kind node)
                          :start-byte (+ (cst-node-start-byte node) delta)
                          :end-byte (+ (cst-node-end-byte node) delta)
                          :source-file (cst-node-source-file node)
                          :message (cst-error-message node)
                          :raw-tokens (cst-error-raw-tokens node)))
    (t node)))

;;; ─── Minimal Reparse Detection ──────────────────────────────────────────────

(defun find-minimal-reparse-node (root edit)
  "Walk ROOT to find the smallest interior node that fully contains the edit range.
   Returns the node that should be re-parsed (or ROOT if no better candidate)."
  (if (not (cst-interior-p root))
      root
      (let ((candidates nil))
        (dolist (child (cst-interior-children root))
          (when (and (cst-overlaps-edit-p child edit)
                     (cst-interior-p child))
            (push child candidates)))
        (cond
          ;; Exactly one child contains the edit — recurse into it
          ((= (length candidates) 1)
           (find-minimal-reparse-node (first candidates) edit))
          ;; Multiple children or leaf overlap — this node is the minimal
          (t root)))))

;;; ─── Incremental Parse ──────────────────────────────────────────────────────

(defun parse-incremental (old-tree new-source edit parse-fn)
  "Incrementally re-parse NEW-SOURCE given OLD-TREE and EDIT.
   PARSE-FN is (lambda (source start-byte end-byte) -> cst-node-list)
   that parses a substring. Returns the new root CST list."
  (let* ((delta (edit-byte-delta edit))
         (reparse-node (find-minimal-reparse-node
                        (make-cst-interior :kind :root :children old-tree
                                          :start-byte 0
                                          :end-byte (+ (length new-source) (- delta)))
                        edit))
         (reparse-start (cst-node-start-byte reparse-node))
         (reparse-end (+ (cst-node-end-byte reparse-node) delta)))
    ;; Re-parse the affected region
    (let ((new-nodes (funcall parse-fn new-source
                              reparse-start
                              (min reparse-end (length new-source)))))
      ;; Rebuild tree by combining: before-nodes + new-nodes + shifted-after-nodes
      (rebuild-tree old-tree edit delta new-nodes))))

(defun rebuild-tree (old-nodes edit delta new-nodes)
  "Rebuild the top-level node list by:
   1. Keeping nodes before the edit unchanged
   2. Replacing overlapping nodes with NEW-NODES
   3. Shifting nodes after the edit by DELTA."
  (let ((before nil)
        (after nil))
    (dolist (node old-nodes)
      (cond
        ((cst-before-edit-p node edit)
         (push node before))
        ((cst-after-edit-p node edit)
         (push (cst-shift-bytes node delta) after))))
    (append (nreverse before) new-nodes (nreverse after))))

;;; ─── Parse Cache ─────────────────────────────────────────────────────────────

(defvar *parse-cache* (make-hash-table :test 'equal)
  "Cache mapping content hashes to parsed CST node lists.")

(defun cache-lookup (source)
  "Look up SOURCE in the parse cache. Returns cached CST nodes or NIL."
  (gethash (sxhash source) *parse-cache*))

(defun cache-store (source nodes)
  "Store NODES in the parse cache for SOURCE."
  (setf (gethash (sxhash source) *parse-cache*) nodes))

(defun invalidate-parse-cache ()
  "Clear the entire parse cache."
  (clrhash *parse-cache*))

;;; ─── CST Equality (for testing) ──────────────────────────────────────────────

(defun cst-equal-p (a b)
  "Structurally compare two CST nodes for equality (ignoring trivia and source-file)."
  (cond
    ((and (null a) (null b)) t)
    ((or (null a) (null b)) nil)
    ((and (cst-token-p a) (cst-token-p b))
     (and (eq (cst-node-kind a) (cst-node-kind b))
          (equal (cst-token-value a) (cst-token-value b))
          (= (cst-node-start-byte a) (cst-node-start-byte b))
          (= (cst-node-end-byte a) (cst-node-end-byte b))))
    ((and (cst-interior-p a) (cst-interior-p b))
     (and (eq (cst-node-kind a) (cst-node-kind b))
          (= (cst-node-start-byte a) (cst-node-start-byte b))
          (= (cst-node-end-byte a) (cst-node-end-byte b))
          (= (length (cst-interior-children a)) (length (cst-interior-children b)))
          (every #'cst-equal-p
                 (cst-interior-children a)
                 (cst-interior-children b))))
    ((and (cst-error-p a) (cst-error-p b))
     (and (string= (cst-error-message a) (cst-error-message b))
          (= (cst-node-start-byte a) (cst-node-start-byte b))
          (= (cst-node-end-byte a) (cst-node-end-byte b))))
    (t nil)))
