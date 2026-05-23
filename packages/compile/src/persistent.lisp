;;;; packages/compile/src/persistent.lisp — FR-611 Persistent Data Structures
;;;; Immutable collections with structural sharing.
;;;; Clojure persistent collections / Scala Vector / Haskell Map equivalent.

(in-package :cl-cc/compile)

;;; ──── Persistent Vector (RRB-Tree) ────
;; Radix-Balanced Tree: Bagwell 2012.
;; O(log₃₂ n) for updates, structural sharing.

(defstruct (pvec-node (:conc-name pvn-))
  "Internal node of a persistent vector (branching factor 32)."
  (children (make-array 32 :initial-element nil) :type simple-vector)
  (shift 0 :type fixnum))

(defstruct (pvec (:conc-name pv-))
  "Persistent vector: immutable, indexable, O(log₃₂ n) updates."
  (root (make-pvec-node) :type pvec-node)
  (tail (make-array 0) :type simple-vector)
  (count 0 :type fixnum)
  (shift 5 :type fixnum)) ; 32 = 2^5

(defun pvec (&rest items)
  "Create a persistent vector from ITEMS."
  (let ((v (make-pvec)))
    (dolist (item items v)
      (setf v (pvec-assoc v (pv-count v) item)))))

(defun pvec-assoc (vec idx val)
  "Return a new persistent vector with VAL at IDX.
Structural sharing: unchanged subtrees are reused."
  (if (and (>= idx 0) (< idx (pv-count vec)))
      (if (< idx (pv-count vec))
          ;; Within bounds: create new vector with updated path
          (let ((new-root (pvec-assoc-internal (pv-root vec)
                                                (pv-shift vec)
                                                idx val)))
            (make-pvec :root new-root
                       :tail (pv-tail vec)
                       :count (pv-count vec)
                       :shift (pv-shift vec)))
          (error "Index ~D out of bounds [0, ~D]" idx (1- (pv-count vec))))
      ;; Append
      (let ((new-tail (copy-seq (pv-tail vec))))
        (vector-push-extend val new-tail)
        (if (< (length new-tail) 32)
            (make-pvec :root (pv-root vec)
                       :tail new-tail
                       :count (1+ (pv-count vec))
                       :shift (pv-shift vec))
            ;; Tail full: push to tree
            (let ((new-root (pvec-push-tail (pv-root vec)
                                            (pv-shift vec)
                                            (pv-tail vec))))
              (make-pvec :root new-root
                         :tail (make-array 1 :initial-element val)
                         :count (1+ (pv-count vec))
                         :shift (pv-shift vec)))))))

(defun pvec-nth (vec idx)
  "Get element at IDX in persistent vector VEC."
  (if (< idx (pv-count vec))
      (aref (pv-tail vec) (mod idx 32))
      (let ((node (pv-root vec))
            (shift (pv-shift vec)))
        ;; Navigate tree
        (loop while (> shift 0)
              do (setf node (aref (pvn-children node)
                                  (logand (ash idx (- shift)) 31))
                       shift (- shift 5)))
        (aref (pvn-children node) (logand idx 31)))))

;;; ──── Internal helpers ────
(defun pvec-assoc-internal (node shift idx val)
  "Internal persistent update at tree level."
  (let* ((child-idx (logand (ash idx (- shift)) 31))
         (new-children (copy-seq (pvn-children node))))
    (if (zerop shift)
        (setf (aref new-children child-idx) val)
        (setf (aref new-children child-idx)
              (pvec-assoc-internal (aref (pvn-children node) child-idx)
                                   (- shift 5) idx val)))
    (make-pvec-node :children new-children :shift shift)))

(defun pvec-push-tail (node shift tail)
  "Push TAIL (32-element array) into the tree at NODE."
  (if (zerop shift)
      (make-pvec-node :children (copy-seq (pvn-children node)) :shift shift)
      (let* ((child-idx (logand (ash (1- (length tail)) (- shift)) 31))
             (child (aref (pvn-children node) child-idx))
             (new-children (copy-seq (pvn-children node))))
        (if child
            (setf (aref new-children child-idx)
                  (pvec-push-tail child (- shift 5) tail))
            (setf (aref new-children child-idx)
                  (make-pvec-node)))
        (make-pvec-node :children new-children :shift shift))))

;;; ──── Persistent Hash Map (HAMT) ────
;; Hash Array Mapped Trie: Bagwell 2001.
;; O(log₃₂ n) for lookups, structural sharing.

(defstruct (pmap-node (:conc-name pmn-))
  "HAMT node: bitmap + children array."
  (bitmap 0 :type (unsigned-byte 32))
  (children (make-array 0) :type simple-vector))

(defstruct (pmap (:conc-name pm-))
  "Persistent hash map (immutable)."
  (root (make-pmap-node) :type pmap-node)
  (count 0 :type fixnum))

(defun pmap ()
  "Create an empty persistent hash map."
  (make-pmap))

(defun pmap-assoc (map key val)
  "Return a new persistent map with KEY → VAL (copy-on-write)."
  (let* ((hash (sxhash key))
         (new-root (pmap-assoc-internal (pm-root map) 0 hash key val)))
    (make-pmap :root new-root :count (1+ (pm-count map)))))

(defun pmap-get (map key &optional default)
  "Get value for KEY in persistent map MAP."
  (pmap-get-internal (pm-root map) 0 (sxhash key) key default))

(defun pmap-assoc-internal (node shift hash key val)
  "Internal HAMT insert."
  (let* ((idx (logand (ash hash (- shift)) 31))
         (bit (ash 1 idx)))
    (if (logtest (pmn-bitmap node) bit)
        ;; Slot occupied: recurse or replace
        (let* ((child-pos (popcount (logand (pmn-bitmap node) (1- bit))))
               (child (aref (pmn-children node) child-pos))
               (new-children (copy-seq (pmn-children node))))
          (setf (aref new-children child-pos)
                (if (pmap-node-p child)
                    (pmap-assoc-internal child (+ shift 5) hash key val)
                    ;; Leaf: create node + reinsert both
                    (let ((new-node (make-pmap-node)))
                      (setf new-node (pmap-assoc-internal new-node shift
                                                         (sxhash (car child))
                                                         (car child) (cdr child)))
                      (pmap-assoc-internal new-node (+ shift 5) hash key val))))
          (make-pmap-node :bitmap (pmn-bitmap node) :children new-children))
        ;; Empty slot: insert
        (let* ((new-pos (popcount (logand (pmn-bitmap node) (1- bit))))
               (new-children (make-array (1+ (length (pmn-children node)))))
               (i 0))
          (dotimes (j (length new-children))
            (if (= j new-pos)
                (setf (aref new-children j) (cons key val))
                (progn
                  (setf (aref new-children j) (aref (pmn-children node) i))
                  (incf i))))
          (make-pmap-node :bitmap (logior (pmn-bitmap node) bit)
                         :children new-children)))))

(defun pmap-get-internal (node shift hash key default)
  "Internal HAMT lookup."
  (let* ((idx (logand (ash hash (- shift)) 31))
         (bit (ash 1 idx)))
    (if (logtest (pmn-bitmap node) bit)
        (let* ((child-pos (popcount (logand (pmn-bitmap node) (1- bit))))
               (child (aref (pmn-children node) child-pos)))
          (if (pmap-node-p child)
              (pmap-get-internal child (+ shift 5) hash key default)
              (if (equal (car child) key)
                  (cdr child)
                  default)))
        default)))

;;; ──── Helpers ────
(defun popcount (n)
  "Count set bits in N (population count)."
  (logcount n))

;;; ──── STM integration hint ────
;; Persistent data structures inside STM transactions have zero conflicts
;; because updates create new versions (no mutation).
(defun pvec-to-list (vec)
  "Convert persistent vector VEC to a regular list."
  (loop for i from 0 below (pv-count vec)
        collect (pvec-nth vec i)))
