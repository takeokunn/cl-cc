(in-package :cl-cc)

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
  ;; Canonicalize child IDs
  (let* ((canon-children (mapcar (lambda (c) (egraph-find eg c)) child-ids))
         (node  (make-e-node :op op :children canon-children))
         (key   (enode-memo-key node)))
    ;; Memo check: if identical node exists, return its class
    (let ((existing (gethash key (eg-memo eg))))
      (if existing
          existing
          ;; Allocate new e-class
          (let* ((id   (eg-next-id eg))
                 (cls  (make-e-class :id id :nodes (list node))))
            (incf (eg-next-id eg))
            (setf (en-eclass node) id)
            (setf (gethash id (eg-classes eg)) cls)
            (setf (gethash id (eg-union-find eg)) id)
            (setf (gethash key (eg-memo eg)) id)
            ;; Register this node as a parent of each child e-class
            (dolist (c canon-children)
              (let ((child-cls (gethash (egraph-find eg c) (eg-classes eg))))
                (when child-cls
                  (push (cons node id) (ec-parents child-cls)))))
            id)))))

(defun egraph-merge (eg id1 id2)
  "Assert that e-classes ID1 and ID2 are equal.
   Queues the merge on the worklist (deferred rebuild for efficiency).
   Returns the canonical ID of the merged class."
  (let ((r1 (egraph-find eg id1))
        (r2 (egraph-find eg id2)))
    (if (= r1 r2)
        r1
        (progn
          (push (cons r1 r2) (eg-worklist eg))
          ;; Union: always merge the smaller-ID class into the larger-ID class
          ;; (arbitrary choice; keeps the canonical ID stable)
          (let ((canon (min r1 r2))
                (non-canon (max r1 r2)))
            (setf (gethash non-canon (eg-union-find eg)) canon)
            canon)))))

(defun egraph-rebuild (eg)
  "Process the worklist of pending merges to restore congruence invariant.
   After merging two e-classes, any e-node whose children are now in the
   same class as another e-node should also be merged (congruence closure)."
  (let ((worklist (eg-worklist eg)))
    (setf (eg-worklist eg) nil)
    (dolist (pair worklist)
      (let* ((r1 (egraph-find eg (car pair)))
             (r2 (egraph-find eg (cdr pair))))
        (unless (= r1 r2)
          ;; Merge e-class data
          (let ((cls1 (gethash r1 (eg-classes eg)))
                (cls2 (gethash r2 (eg-classes eg))))
            (when (and cls1 cls2)
              ;; Move all nodes from cls2 into cls1
              (dolist (n (ec-nodes cls2))
                (setf (en-eclass n) r1)
                (push n (ec-nodes cls1)))
              ;; Move all parents from cls2 into cls1
              (dolist (p (ec-parents cls2))
                (push p (ec-parents cls1)))
              ;; Remove cls2
              (remhash r2 (eg-classes eg))))
          ;; Repair: re-canonicalize parent e-nodes of the merged classes
          (let ((cls1 (gethash r1 (eg-classes eg))))
            (when cls1
              (dolist (parent-pair (ec-parents cls1))
                (let* ((pn      (car parent-pair))
                       (old-key (enode-memo-key pn))
                       (new-pn  (egraph-canonical-enode eg pn))
                       (new-key (enode-memo-key new-pn)))
                  (unless (equal old-key new-key)
                    ;; Remove old memo entry
                    (remhash old-key (eg-memo eg))
                    ;; Check if new-key already exists (congruence merge needed)
                    (let ((existing (gethash new-key (eg-memo eg))))
                      (if existing
                          ;; Merge this parent's class with the existing one
                          (let ((pn-class (egraph-find eg (en-eclass pn))))
                            (unless (= pn-class existing)
                              (egraph-merge eg pn-class existing)))
                          ;; Update memo with canonical key
                          (progn
                            (setf (en-children pn) (en-children new-pn))
                            (setf (gethash new-key (eg-memo eg)) (en-eclass pn))))))))))
          ;; Recurse if worklist grew
          (when (eg-worklist eg)
            (egraph-rebuild eg)))))))

;;; ─── Pattern Matching ────────────────────────────────────────────────────
;;;
;;; Patterns are s-expressions with ?-prefixed symbols as pattern variables.
;;; Example: (add ?x (const 0))
;;; The pattern matcher walks the e-graph and collects all substitutions.

(defun egraph-pattern-var-p (x)
  "T if X is a pattern variable: a symbol whose name starts with '?'."
  (and (symbolp x)
       (> (length (symbol-name x)) 0)
       (char= (char (symbol-name x) 0) #\?)))

(defun egraph-match-pattern (eg pattern class-id &optional bindings)
  "Match PATTERN against the e-class CLASS-ID in e-graph EG.
   Returns a list of all binding alists that satisfy the match, or NIL.
   Pattern variables (?x) bind to e-class IDs."
  (let ((cid (egraph-find eg class-id)))
    (cond
      ;; Pattern variable: bind to this class (check consistency)
      ((egraph-pattern-var-p pattern)
       (let ((existing (assoc pattern bindings)))
         (if existing
             (if (= (egraph-find eg (cdr existing)) cid)
                 (list bindings)
                 nil)
             (list (cons (cons pattern cid) bindings)))))

      ;; Constant pattern: match if the class contains a const e-node with this value
      ((and (consp pattern) (eq (car pattern) 'const) (cdr pattern))
       (let ((val (cadr pattern)))
         (let ((cls (gethash cid (eg-classes eg))))
           (when cls
             (loop for n in (ec-nodes cls)
                   when (and (eq (en-op n) 'const)
                             (null (en-children n))
                             ;; The constant value is stored in ec-data
                             (let ((cls-data (ec-data cls)))
                               (or (equal cls-data val)
                                   (and (consp cls-data)
                                        (equal (car cls-data) val)))))
                   collect bindings)))))

      ;; Compound pattern: (op arg1 arg2 ...)
      ((consp pattern)
       (let ((op       (car pattern))
             (arg-pats (cdr pattern)))
         (let ((cls (gethash cid (eg-classes eg))))
           (when cls
             (loop for n in (ec-nodes cls)
                   when (and (eq (en-op n) op)
                             (= (length (en-children n)) (length arg-pats)))
                   nconc (egraph-match-pattern-args eg arg-pats (en-children n) bindings))))))

      ;; Literal symbol: match if the class has a node with that op and no children
      ((symbolp pattern)
       (let ((cls (gethash cid (eg-classes eg))))
         (when cls
           (loop for n in (ec-nodes cls)
                 when (and (eq (en-op n) pattern) (null (en-children n)))
                 collect bindings))))

      (t nil))))

(defun egraph-match-pattern-args (eg arg-pats arg-ids bindings)
  "Match argument patterns ARG-PATS against e-class IDs ARG-IDS.
   Returns list of all consistent binding alists."
  (if (null arg-pats)
      (list bindings)
      (let ((matches (egraph-match-pattern eg (car arg-pats) (car arg-ids) bindings)))
        (loop for b in matches
              nconc (egraph-match-pattern-args eg (cdr arg-pats) (cdr arg-ids) b)))))

;;; ─── Rule Application ────────────────────────────────────────────────────

(defun egraph-apply-rule (eg rule)
  "Apply one rewrite rule RULE = (:lhs pattern :rhs template :when guard-fn)
   to the e-graph EG.  For every match of :lhs, construct the :rhs and
   merge the resulting e-class with the matched class.
   Returns the number of new merges added."
  (let ((lhs      (getf rule :lhs))
        (rhs      (getf rule :rhs))
        (guard-fn (getf rule :when))
        (merges   0))
    (loop for cid being the hash-keys of (eg-classes eg)
          do (let ((matches (egraph-match-pattern eg lhs cid)))
               (dolist (bindings matches)
                 (when (or (null guard-fn) (funcall guard-fn bindings eg))
                   (let ((new-id (egraph-build-rhs eg rhs bindings)))
                     (when new-id
                       (unless (= (egraph-find eg cid) (egraph-find eg new-id))
                         (egraph-merge eg cid new-id)
                         (incf merges))))))))
    merges))

(defun egraph-build-rhs (eg template bindings)
  "Build the right-hand side TEMPLATE into the e-graph EG using BINDINGS.
   Returns the e-class ID of the constructed term."
  (cond
    ;; Pattern variable: look up binding
    ((egraph-pattern-var-p template)
     (let ((b (assoc template bindings)))
       (when b (cdr b))))

    ;; Compound template: (op arg1 arg2 ...)
    ((consp template)
     (let* ((op       (car template))
            (arg-tmps (cdr template))
            (arg-ids  (mapcar (lambda (a) (egraph-build-rhs eg a bindings))
                              arg-tmps)))
       (when (every #'identity arg-ids)
         (apply #'egraph-add eg op arg-ids))))

    ;; Literal op (nullary)
    ((symbolp template)
     (egraph-add eg template))

    ;; Numeric or string constant
    ((or (numberp template) (stringp template))
     (let ((id (egraph-add eg 'const)))
       ;; Store constant value in e-class data
       (let ((cls (gethash id (eg-classes eg))))
         (when cls (setf (ec-data cls) template)))
       id))

    (t nil)))

;;; ─── Saturation ──────────────────────────────────────────────────────────

(defun egraph-saturate (eg rules &key (limit 30) (fuel 10000))
  "Apply RULES to EG until saturation (no new merges) or resource limits.
   LIMIT: maximum number of full-pass iterations.
   FUEL:  maximum total merges across all iterations.
   Returns (values saturated-p iterations fuel-remaining)."
  (let ((total-merges 0))
    (loop for iter from 0 below limit
          do (let ((iter-merges 0))
               (dolist (rule rules)
                 (let ((n (egraph-apply-rule eg rule)))
                   (incf iter-merges n)
                   (incf total-merges n)
                   (when (> total-merges fuel)
                     (egraph-rebuild eg)
                     (return-from egraph-saturate (values nil iter fuel)))))
               (egraph-rebuild eg)
               (when (= iter-merges 0)
                 (return-from egraph-saturate (values t iter (- fuel total-merges)))))
          finally (return (values nil limit 0)))))

;;; ─── Cost Model + Extraction ─────────────────────────────────────────────

(defparameter *egraph-op-base-costs*
  '(("CONST"        . 0)
    ("MOVE"         . 1)
    ("ADD"          . 1) ("SUB"          . 1) ("NEG"          . 1)
    ("MUL"          . 3)
    ("ASH"          . 2)
    ("DIV"          . 4) ("MOD"          . 4)
    ("CALL"         . 10)
    ("GENERIC-CALL" . 20))
  "Base instruction latency costs for the e-graph default cost model.
   Keys are op symbol-name strings (uppercase); values are integer base costs.
   Unlisted operations default to 2.  Compares with #'equal (string).")

(defun egraph-default-cost (op children-costs)
  "Default cost model: base latency + sum of children costs.
   Constants are free; simple arithmetic costs 1; calls cost 10+.
   Compares by symbol-name so cross-package symbol variants work."
  (let* ((op-str (when (symbolp op) (symbol-name op)))
         (entry  (assoc op-str *egraph-op-base-costs* :test #'equal))
         (base   (if entry (cdr entry) 2)))
    (+ base (reduce #'+ children-costs :initial-value 0))))

(defun egraph-extract (eg root-id &optional (cost-fn #'egraph-default-cost))
  "Bottom-up extraction: for each e-class reachable from ROOT-ID,
   pick the e-node with the minimum cost.
   Returns an s-expression (nested list) representing the cheapest term.

   COST-FN: (op children-costs) → numeric cost."
  (let ((cache (make-hash-table))) ; class-id → (cost . sexp)
    (labels ((extract-class (cid)
               (let ((canon (egraph-find eg cid)))
                 (or (gethash canon cache)
                     (let ((cls (gethash canon (eg-classes eg))))
                       (if (null cls)
                           (progn (setf (gethash canon cache) (cons most-positive-fixnum cid)) nil)
                           (let ((best-cost most-positive-fixnum)
                                 (best-sexp nil))
                             ;; Temporarily record a high-cost sentinel to handle cycles
                             (setf (gethash canon cache) (cons most-positive-fixnum nil))
                             (dolist (n (ec-nodes cls))
                               (let* ((child-results (mapcar #'extract-class (en-children n)))
                                      (child-costs   (mapcar (lambda (r) (if r (car r) most-positive-fixnum))
                                                             child-results))
                                      (child-sexps   (mapcar (lambda (r) (when r (cdr r)))
                                                             child-results))
                                      (total-cost    (funcall cost-fn (en-op n) child-costs)))
                                 (when (< total-cost best-cost)
                                   (setf best-cost total-cost)
                                   (setf best-sexp
                                         (if (null (en-children n))
                                             (let ((data (ec-data (gethash canon (eg-classes eg)))))
                                               (or data (en-op n)))
                                             (cons (en-op n) child-sexps))))))
                             (let ((result (cons best-cost best-sexp)))
                               (setf (gethash canon cache) result)
                               result))))))))
      (let ((result (extract-class root-id)))
        (when result (cdr result))))))

;;; ─── VM Instruction ↔ E-graph ────────────────────────────────────────────

(defun vm-inst-to-enode-op (inst)
  "Convert a VM instruction to an e-graph operation symbol."
  (let ((type-name (symbol-name (type-of inst))))
    ;; Strip 'VM-' prefix: VM-ADD → ADD, VM-CONST → CONST, etc.
    (if (and (> (length type-name) 3)
             (string= (subseq type-name 0 3) "VM-"))
        (intern (subseq type-name 3) :cl-cc)
        (type-of inst))))

(defun egraph-add-instructions (eg instructions)
  "Add all instructions in the list to the e-graph.
   Returns a hash-table: register → e-class-id mapping the VM registers
   to their e-class IDs in the graph."
  (let ((reg->class (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (let* ((op       (vm-inst-to-enode-op inst))
             (reads    (opt-inst-read-regs inst))
             (dst      (ignore-errors (vm-dst inst)))
             ;; Map source registers to e-class IDs (create fresh if unseen)
             (child-ids (mapcar (lambda (r)
                                  (or (gethash r reg->class)
                                      (let ((id (egraph-add eg 'reg-ref r)))
                                        (setf (gethash r reg->class) id)
                                        id)))
                                reads))
             (class-id  (apply #'egraph-add eg op child-ids)))
        ;; Handle constant values specially
        (when (vm-const-p inst)
          (let ((cls (gethash class-id (eg-classes eg))))
            (when cls (setf (ec-data cls) (vm-value inst)))))
        ;; Map destination register to this e-class
        (when dst
          (setf (gethash dst reg->class) class-id))))
    reg->class))

;;; ─── E-Graph Statistics ──────────────────────────────────────────────────

(defun egraph-stats (eg)
  "Return a plist of e-graph statistics for debugging."
  (list :classes   (hash-table-count (eg-classes eg))
        :memo-size (hash-table-count (eg-memo eg))
        :worklist  (length (eg-worklist eg))))
