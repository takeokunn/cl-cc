;;;; persistent.lisp — Immutable persistent hash-map for the test framework.
;;;;
;;;; Phase 1 of the mutable-registry → persistent-datastructure migration.
;;;; Zero external dependencies — pure Common Lisp, uses only `:cl`.
;;;;
;;;; Implementation strategy: a binary search tree keyed on `sxhash` with
;;;; association-list buckets for hash collisions. Every slot is read-only
;;;; and `persist-assoc`/`persist-remove` return freshly allocated trees
;;;; (path-copying) without mutating their input, giving O(log N) per op.
;;;; This is sufficient for the framework's registries while keeping the
;;;; implementation under ~300 lines and requiring no HAMT plumbing.

(in-package :cl-cc/test)

;;; Exports live in package.lisp alongside the rest of the :cl-cc/test API.

;;; ------------------------------------------------------------
;;; Node / Map structures
;;; ------------------------------------------------------------

(defstruct (pnode (:copier nil)
                  (:predicate pnode-p)
                  (:constructor %make-pnode (hash bucket left right)))
  "Internal BST node keyed by `sxhash`. BUCKET holds an alist of
   ((key . value) ...) for every entry whose sxhash equals HASH.
   All slots are read-only to preserve immutability of the map."
  (hash 0 :type fixnum :read-only t)
  (bucket nil :read-only t)
  (left nil :read-only t)
  (right nil :read-only t))

(defstruct (persistent-map (:copier nil)
                           (:predicate persistent-map-p)
                           (:constructor %make-persistent-map (root count test)))
  "Immutable hash map. ROOT is either NIL or a pnode. COUNT is the total
   number of key/value entries across all buckets. TEST is the symbol
   naming the equality function used for key comparison within a bucket
   (e.g. `eql`, `equal`, `equalp`)."
  (root nil :read-only t)
  (count 0 :type (integer 0) :read-only t)
  (test 'eql :type symbol :read-only t))

;;; ------------------------------------------------------------
;;; Internal helpers
;;; ------------------------------------------------------------

(declaim (inline %hash))
(defun %hash (key)
  "Map KEY to a fixnum hash for ordering within the BST."
  (sxhash key))

(defun %bucket-find (bucket key test)
  "Return the ((key . value)) cons in BUCKET matching KEY under TEST,
   or NIL if absent. TEST is a symbol."
  (assoc key bucket :test (symbol-function test)))

(defun %bucket-assoc (bucket key value test)
  "Return (values new-bucket added-p). ADDED-P is T if KEY was not
   previously present. New bucket is a fresh list; BUCKET is unchanged."
  (let ((existing (%bucket-find bucket key test)))
    (cond
      (existing
       ;; Replace value — copy every cell except the matching one.
       (let ((new (loop for cell in bucket
                        if (eq cell existing)
                          collect (cons key value)
                        else
                          collect cell)))
         (values new nil)))
      (t
       (values (cons (cons key value) bucket) t)))))

(defun %bucket-remove (bucket key test)
  "Return (values new-bucket removed-p)."
  (let ((existing (%bucket-find bucket key test)))
    (if existing
        (values (loop for cell in bucket
                      unless (eq cell existing)
                        collect cell)
                t)
        (values bucket nil))))

;;; --- BST operations: path-copying insert / lookup / remove ---

(defun %tree-lookup (node hash key test)
  "Return (values value found-p) for KEY in subtree NODE."
  (cond
    ((null node) (values nil nil))
    ((< hash (pnode-hash node))
     (%tree-lookup (pnode-left node) hash key test))
    ((> hash (pnode-hash node))
     (%tree-lookup (pnode-right node) hash key test))
    (t
     (let ((cell (%bucket-find (pnode-bucket node) key test)))
       (if cell
           (values (cdr cell) t)
           (values nil nil))))))

(defun %tree-insert (node hash key value test)
  "Return (values new-node added-p). Path-copying: unchanged subtrees
   are shared; touched nodes are freshly allocated."
  (cond
    ((null node)
     (values (%make-pnode hash (list (cons key value)) nil nil) t))
    ((< hash (pnode-hash node))
     (multiple-value-bind (new-left added-p)
         (%tree-insert (pnode-left node) hash key value test)
       (values (%make-pnode (pnode-hash node)
                            (pnode-bucket node)
                            new-left
                            (pnode-right node))
               added-p)))
    ((> hash (pnode-hash node))
     (multiple-value-bind (new-right added-p)
         (%tree-insert (pnode-right node) hash key value test)
       (values (%make-pnode (pnode-hash node)
                            (pnode-bucket node)
                            (pnode-left node)
                            new-right)
               added-p)))
    (t
     (multiple-value-bind (new-bucket added-p)
         (%bucket-assoc (pnode-bucket node) key value test)
       (values (%make-pnode (pnode-hash node)
                            new-bucket
                            (pnode-left node)
                            (pnode-right node))
               added-p)))))

(defun %tree-min (node)
  "Return the leftmost pnode of subtree NODE."
  (if (pnode-left node)
      (%tree-min (pnode-left node))
      node))

(defun %tree-remove-min (node)
  "Remove the leftmost node; return the new subtree (path-copied)."
  (cond
    ((null (pnode-left node)) (pnode-right node))
    (t
     (%make-pnode (pnode-hash node)
                  (pnode-bucket node)
                  (%tree-remove-min (pnode-left node))
                  (pnode-right node)))))

(defun %tree-delete-node (node)
  "Remove NODE from its position, splicing left/right subtrees."
  (cond
    ((null (pnode-left node))  (pnode-right node))
    ((null (pnode-right node)) (pnode-left node))
    (t
     (let ((successor (%tree-min (pnode-right node))))
       (%make-pnode (pnode-hash successor)
                    (pnode-bucket successor)
                    (pnode-left node)
                    (%tree-remove-min (pnode-right node)))))))

(defun %tree-remove (node hash key test)
  "Return (values new-node removed-p). Path-copying."
  (cond
    ((null node) (values nil nil))
    ((< hash (pnode-hash node))
     (multiple-value-bind (new-left removed-p)
         (%tree-remove (pnode-left node) hash key test)
       (values (%make-pnode (pnode-hash node)
                            (pnode-bucket node)
                            new-left
                            (pnode-right node))
               removed-p)))
    ((> hash (pnode-hash node))
     (multiple-value-bind (new-right removed-p)
         (%tree-remove (pnode-right node) hash key test)
       (values (%make-pnode (pnode-hash node)
                            (pnode-bucket node)
                            (pnode-left node)
                            new-right)
               removed-p)))
    (t
     (multiple-value-bind (new-bucket removed-p)
         (%bucket-remove (pnode-bucket node) key test)
       (cond
         ((not removed-p) (values node nil))
         ((null new-bucket)
          ;; Bucket emptied — splice the two subtrees.
          (values (%tree-delete-node node) t))
         (t
          (values (%make-pnode (pnode-hash node)
                               new-bucket
                               (pnode-left node)
                               (pnode-right node))
                  t)))))))

(defun %tree-walk (node fn)
  "Call (funcall FN key value) for every entry, in BST/alist order."
  (when node
    (%tree-walk (pnode-left node) fn)
    (dolist (cell (pnode-bucket node))
      (funcall fn (car cell) (cdr cell)))
    (%tree-walk (pnode-right node) fn))
  nil)

;;; ------------------------------------------------------------
;;; Public API
;;; ------------------------------------------------------------

(defun persist-empty (&key (test 'eql))
  "Return a fresh empty persistent-map using TEST (a symbol) for key equality."
  (%make-persistent-map nil 0 test))

(defun persist-count (pm)
  "Number of entries in PM."
  (persistent-map-count pm))

(defun persist-assoc (pm key value)
  "Return a new persistent-map equal to PM with KEY mapped to VALUE.
   Does not mutate PM. If KEY is present, the mapping is replaced and
   the count is unchanged."
  (let ((test (persistent-map-test pm)))
    (multiple-value-bind (new-root added-p)
        (%tree-insert (persistent-map-root pm) (%hash key) key value test)
      (%make-persistent-map new-root
                            (if added-p
                                (1+ (persistent-map-count pm))
                                (persistent-map-count pm))
                            test))))

(defun persist-lookup (pm key &optional default)
  "Return (values value found-p). If KEY is absent, VALUE is DEFAULT and
   FOUND-P is NIL."
  (multiple-value-bind (v found-p)
      (%tree-lookup (persistent-map-root pm)
                    (%hash key)
                    key
                    (persistent-map-test pm))
    (if found-p
        (values v t)
        (values default nil))))

(defun persist-contains-p (pm key)
  "True iff KEY is a key in PM."
  (multiple-value-bind (v found-p) (persist-lookup pm key)
    (declare (ignore v))
    found-p))

(defun persist-remove (pm key)
  "Return a new persistent-map equal to PM without KEY. If KEY is
   absent, returns a map equivalent to PM with count unchanged."
  (let ((test (persistent-map-test pm)))
    (multiple-value-bind (new-root removed-p)
        (%tree-remove (persistent-map-root pm) (%hash key) key test)
      (%make-persistent-map new-root
                            (if removed-p
                                (1- (persistent-map-count pm))
                                (persistent-map-count pm))
                            test))))

(defun persist-each (pm fn)
  "Call (funcall FN key value) for every entry of PM. Returns NIL."
  (%tree-walk (persistent-map-root pm) fn)
  nil)

(defun persist-keys (pm)
  "Return a list of every key in PM. Order is unspecified."
  (let ((acc '()))
    (persist-each pm (lambda (k v) (declare (ignore v)) (push k acc)))
    acc))

(defun persist-values (pm)
  "Return a list of every value in PM. Order is unspecified."
  (let ((acc '()))
    (persist-each pm (lambda (k v) (declare (ignore k)) (push v acc)))
    acc))

(defun persist-to-alist (pm)
  "Return an alist ((key . value) ...) with every entry of PM."
  (let ((acc '()))
    (persist-each pm (lambda (k v) (push (cons k v) acc)))
    acc))

(defun persist-from-alist (alist &key (test 'eql))
  "Return a persistent-map populated from ALIST. If ALIST contains
   duplicate keys, the leftmost binding wins — matching CL `assoc'
   semantics."
  (let ((pm (persist-empty :test test)))
    (dolist (cell (reverse alist))
      (setf pm (persist-assoc pm (car cell) (cdr cell))))
    pm))
