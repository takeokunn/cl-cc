;;;; packages/engine/optimize/src/egraph-saturation.lisp — E-Graph Saturation, Extraction, and VM Interface
;;;;
;;;; Contains:
;;;;   egraph-saturate — equality saturation (rule application loop)
;;;;   *egraph-op-base-costs*, egraph-default-cost, egraph-extract — cost model + extraction
;;;;   vm-inst-to-enode-op, egraph-add-instructions — VM instruction interface
;;;;   egraph-class-has-op-p, egraph-class-const-value — class queries
;;;;   egraph-stats — statistics
;;;;
;;;; Data structures, union-find, core ops, pattern matching, and rule application
;;;; are in egraph.lisp (loads before).
;;;;
;;;; Load order: after egraph.lisp, before egraph-rules.lisp.

(in-package :cl-cc/optimize)

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

(defun %egraph-extract-class (cid eg cache cost-fn)
  "Recursively extract the minimum-cost term for e-class CID from EG."
  (let ((canon (egraph-find eg cid)))
    (or (gethash canon cache)
        (let ((cls (gethash canon (eg-classes eg))))
          (if (null cls)
              (progn (setf (gethash canon cache) (cons most-positive-fixnum cid)) nil)
              (let ((best-cost most-positive-fixnum)
                    (best-sexp nil))
                (setf (gethash canon cache) (cons most-positive-fixnum nil))
                (dolist (n (ec-nodes cls))
                  (let* ((child-results (mapcar (lambda (c) (%egraph-extract-class c eg cache cost-fn))
                                               (en-children n)))
                         (child-costs   (mapcar (lambda (r) (if r (car r) most-positive-fixnum))
                                               child-results))
                         (child-sexps   (mapcar (lambda (r) (when r (cdr r))) child-results))
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
                  result)))))))

(defun egraph-extract (eg root-id &optional (cost-fn #'egraph-default-cost))
  "Bottom-up extraction: for each e-class reachable from ROOT-ID,
   pick the e-node with the minimum cost.
   Returns an s-expression (nested list) representing the cheapest term.

   COST-FN: (op children-costs) → numeric cost."
  (let ((cache (make-hash-table)))
    (let ((result (%egraph-extract-class root-id eg cache cost-fn)))
      (when result (cdr result)))))

;;; ─── VM Instruction ↔ E-graph ────────────────────────────────────────────

(defun vm-inst-to-enode-op (inst)
  "Convert a VM instruction to an e-graph operation symbol."
  (let ((type-name (symbol-name (type-of inst))))
    ;; Strip 'VM-' prefix: VM-ADD → ADD, VM-CONST → CONST, etc.
    (if (and (> (length type-name) 3)
             (string= (subseq type-name 0 3) "VM-"))
        (intern (subseq type-name 3) :cl-cc/optimize)
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

(defun egraph-class-has-op-p (eg class-id op)
  (let ((cls (gethash (egraph-find eg class-id) (eg-classes eg))))
    (and cls
         (some (lambda (node) (eq (en-op node) op)) (ec-nodes cls)))))

(defun egraph-class-const-value (eg class-id)
  (let ((canon (egraph-find eg class-id)))
    (when (egraph-class-has-op-p eg canon 'const)
      (ec-data (gethash canon (eg-classes eg))))))

;;; ─── E-Graph Statistics ──────────────────────────────────────────────────

(defun egraph-stats (eg)
  "Return a plist of e-graph statistics for debugging."
  (list :classes   (hash-table-count (eg-classes eg))
        :memo-size (hash-table-count (eg-memo eg))
        :worklist  (length (eg-worklist eg))))
