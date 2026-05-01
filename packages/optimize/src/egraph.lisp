(in-package :cl-cc/optimize)

;;; ─── E-Graph Equality Saturation ─────────────────────────────────────────
;;;
;;; Implements an E-graph (equality graph) for equality saturation.
;;; Core operations:
;;;   egraph-add     — add an e-node, return its e-class ID
;;;   egraph-merge   — assert two e-class IDs are equal
;;;   egraph-rebuild — restore congruence invariant after merges
;;;   egraph-saturate — apply rewrite rules until fixed-point
;;;   egraph-extract — extract optimal term using cost model
;;;
;;; The E-graph subsumes the following existing optimizer passes:
;;;   opt-pass-fold            → constant folding rules
;;;   opt-simplify-binop       → algebraic identity rules
;;;   opt-pass-strength-reduce → strength reduction rules
;;;   opt-pass-copy-prop       → FREE: e-class merge subsumes copy propagation
;;;   opt-pass-cse             → FREE: hash-consing deduplicates automatically
;;;
;;; References:
;;;   Willsey et al. (2021). "egg: Fast and Extensible Equality Saturation"
;;;   Nelson & Oppen (1979). "Simplification by Cooperating Decision Procedures"

;;; ─── Data Structures ─────────────────────────────────────────────────────

(defstruct (e-node (:conc-name en-))
  "An e-node represents a function application f(c1, c2, ...) where each
   argument ci is an e-class ID (not a concrete value).  Structurally
   identical e-nodes (same op + same canonicalized child IDs) are the same."
  (op       nil  :type symbol)         ; operation symbol (e.g., 'add, 'const)
  (children nil  :type list)           ; list of e-class IDs (fixnums)
  (eclass   -1   :type fixnum))        ; owning e-class ID (-1 = unowned)

(defstruct (e-class (:conc-name ec-))
  "An e-class is an equivalence class of expressions that are all equal.
   It holds a list of e-nodes and analysis data (type, cost bounds, etc.)."
  (id       0    :type fixnum)         ; canonical ID
  (nodes    nil  :type list)           ; list of e-node in this class
  (parents  nil  :type list)           ; list of (e-node . parent-eclass-id) referencing us
  (data     nil))                      ; analysis data (type, cost, etc.)

(defstruct (e-graph (:conc-name eg-))
  "The E-graph maintains a union-find structure over e-classes, a memo table
   for hash-consing, and a worklist of pending merges."
  (classes    (make-hash-table)           :type hash-table)  ; id → e-class
  (memo       (make-hash-table :test #'equal) :type hash-table)  ; canonical-enode-key → class-id
  (union-find (make-hash-table)           :type hash-table)  ; id → canonical id
  (worklist   nil                         :type list)        ; (id1 . id2) pending merges
  (next-id    0                           :type fixnum))

;;; ─── Union-Find ──────────────────────────────────────────────────────────

(defun egraph-find (eg id)
  "Path-compressed union-find lookup.  Returns the canonical e-class ID for ID."
  (let ((parent (gethash id (eg-union-find eg))))
    (if (or (null parent) (= parent id))
        id
        (let ((root (egraph-find eg parent)))
          ;; Path compression
          (setf (gethash id (eg-union-find eg)) root)
          root))))

(defun egraph-canonical-enode (eg node)
  "Return an e-node with all child IDs canonicalized via union-find."
  (make-e-node :op (en-op node)
               :children (mapcar (lambda (c) (egraph-find eg c))
                                 (en-children node))))

;;; ─── E-Node Memo Key ─────────────────────────────────────────────────────

(defun enode-memo-key (node)
  "Compute the memo table key for an e-node (op + canonicalized child IDs).
   Uses a list as the key (compared with #'equal)."
  (cons (en-op node) (en-children node)))

;;; ─── Core Operations ─────────────────────────────────────────────────────

(defun egraph-add (eg op &rest child-ids)
  "Add an e-node with operation OP and CHILD-IDS to the e-graph.
   Returns the e-class ID.
   If a structurally identical node already exists (memo hit), returns its class."
  (let* ((canon-children (mapcar (lambda (c) (egraph-find eg c)) child-ids))
         (node (make-e-node :op op :children canon-children))
         (key  (enode-memo-key node)))
    (or (gethash key (eg-memo eg))
        (let* ((id  (eg-next-id eg))
               (cls (make-e-class :id id :nodes (list node))))
          (incf (eg-next-id eg))
          (setf (en-eclass node) id
                (gethash id  (eg-classes eg))    cls
                (gethash id  (eg-union-find eg)) id
                (gethash key (eg-memo eg))        id)
          (dolist (c canon-children)
            (let ((child-cls (gethash (egraph-find eg c) (eg-classes eg))))
              (when child-cls
                (push (cons node id) (ec-parents child-cls)))))
          id))))

(defun egraph-merge (eg id1 id2)
  "Assert that e-classes ID1 and ID2 are equal.
   Queues the merge on the worklist (deferred rebuild for efficiency).
   Returns the canonical ID of the merged class."
  (let ((r1 (egraph-find eg id1))
        (r2 (egraph-find eg id2)))
    (if (= r1 r2)
        r1
        ;; Union: smaller-ID class wins as canonical (keeps canonical ID stable)
        (let ((canon     (min r1 r2))
              (non-canon (max r1 r2)))
          (push (cons r1 r2) (eg-worklist eg))
          (setf (gethash non-canon (eg-union-find eg)) canon)
          canon))))

(defun %egraph-absorb-class (eg r1 r2)
  "Destructively merge e-class R2 into R1: move all nodes and parents, then remove R2."
  (let ((cls1 (gethash r1 (eg-classes eg)))
        (cls2 (gethash r2 (eg-classes eg))))
    (when (and cls1 cls2)
      (dolist (n (ec-nodes cls2))
        (setf (en-eclass n) r1)
        (push n (ec-nodes cls1)))
      (dolist (p (ec-parents cls2))
        (push p (ec-parents cls1)))
      (remhash r2 (eg-classes eg)))))

(defun %egraph-repair-parent-node (eg pn)
  "Re-canonicalize parent e-node PN after a class merge; apply congruence merge if needed."
  (let* ((old-key (enode-memo-key pn))
         (new-pn  (egraph-canonical-enode eg pn))
         (new-key (enode-memo-key new-pn)))
    (unless (equal old-key new-key)
      (remhash old-key (eg-memo eg))
      (let ((existing (gethash new-key (eg-memo eg))))
        (if existing
            (let ((pn-class (egraph-find eg (en-eclass pn))))
              (unless (= pn-class existing)
                (egraph-merge eg pn-class existing)))
            (progn
              (setf (en-children pn) (en-children new-pn))
              (setf (gethash new-key (eg-memo eg)) (en-eclass pn))))))))

(defun egraph-rebuild (eg)
  "Process the worklist of pending merges to restore the congruence invariant."
  (let ((worklist (eg-worklist eg)))
    (setf (eg-worklist eg) nil)
    (dolist (pair worklist)
      (let ((r1 (egraph-find eg (car pair)))
            (r2 (egraph-find eg (cdr pair))))
        (unless (= r1 r2)
          (%egraph-absorb-class eg r1 r2)
          (let ((cls1 (gethash r1 (eg-classes eg))))
            (when cls1
              (dolist (parent-pair (ec-parents cls1))
                (%egraph-repair-parent-node eg (car parent-pair)))))
          (when (eg-worklist eg)
            (egraph-rebuild eg)))))))

;;; Pattern matching and rule application are in egraph-match.lisp (loads after).
