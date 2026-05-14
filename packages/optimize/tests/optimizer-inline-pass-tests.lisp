;;;; tests/unit/optimize/optimizer-inline-pass-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline-pass.lisp
;;;;
;;;; Covers:
;;;;   Memo utilities  — opt-make-pure-function-memo-table,
;;;;                     opt-pure-function-memo-get, opt-pure-function-memo-put
;;;;   Body analysis   — opt-function-body-instruction-tables (single-pass, returns values)
;;;;   Reachability    — opt-reachable-function-labels

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

;;; Note: opt-collect-function-defs requires (vm-closure-params inst) to be
;;; non-nil — functions with zero parameters are excluded from collection.
;;; Always give test closures at least one param so they are collected.

(defun make-inline-pass-func-insts (label params body-insts)
  "Return instruction list: vm-closure + vm-label + BODY-INSTS + vm-ret."
  (list* (make-vm-closure :dst :r9 :label label :params params :captured nil)
         (make-vm-label  :name label)
         (append body-insts
                 (list (make-vm-ret :reg (first params))))))

(defun collect-inline-pass-func-defs (label params body-insts)
  "Build func-defs table directly for functions with the given structure."
  (cl-cc/optimize::opt-collect-function-defs
   (make-inline-pass-func-insts label params body-insts)))

;;; ─── opt-make-pure-function-memo-table ──────────────────────────────────────

(deftest opt-memo-table-is-hash-table
  "opt-make-pure-function-memo-table returns an empty equal hash-table."
  (let ((ht (cl-cc/optimize::opt-make-pure-function-memo-table)))
    (assert-true (hash-table-p ht))
    (assert-= 0 (hash-table-count ht))))

;;; ─── opt-pure-function-memo-get / opt-pure-function-memo-put ────────────────

(deftest-each opt-memo-get-miss-cases
  "opt-pure-function-memo-get returns nil/nil when label is impure or pure but no value stored."
  :cases (("impure-label" nil)
          ("pure-no-put"  t))
  (mark-pure-p)
  (let ((memo  (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pures (make-hash-table :test #'equal)))
    (when mark-pure-p (setf (gethash "fn" pures) t))
    (multiple-value-bind (val found-p)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (assert-null val)
      (assert-null found-p))))

(deftest opt-memo-roundtrip
  "opt-pure-function-memo-put then get returns the stored result."
  (let ((memo   (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pures  (make-hash-table :test #'equal)))
    (setf (gethash "fn" pures) t)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(2) :answer)
    (multiple-value-bind (val found-p)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(2))
      (assert-eq :answer val)
      (assert-true found-p))))

(deftest opt-memo-put-ignores-impure-label
  "opt-pure-function-memo-put does not store for labels absent from pure-labels."
  (let ((memo   (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pures  (make-hash-table :test #'equal)))
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "impure" '() :result)
    (assert-= 0 (hash-table-count memo))))

(deftest opt-memo-lru-evicts-oldest-entry-at-max-size
  "When max-size is set, inserting over capacity evicts the least-recently-used key."
  (let ((memo  (cl-cc/optimize::opt-make-pure-function-memo-table :max-size 2))
        (pures (make-hash-table :test #'equal)))
    (setf (gethash "fn" pures) t)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(1) :a)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(2) :b)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(3) :c)
    (assert-= 2 (hash-table-count memo))
    (multiple-value-bind (v1 f1)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (declare (ignore v1))
      (assert-false f1))
    (multiple-value-bind (v2 f2)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(2))
      (assert-true f2)
      (assert-eq :b v2))
    (multiple-value-bind (v3 f3)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(3))
      (assert-true f3)
      (assert-eq :c v3))))

(deftest opt-memo-lru-touch-on-get-updates-recentness
  "A successful memo get updates recency so a different key is evicted next."
  (let ((memo  (cl-cc/optimize::opt-make-pure-function-memo-table :max-size 2))
        (pures (make-hash-table :test #'equal)))
    (setf (gethash "fn" pures) t)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(1) :a)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(2) :b)
    ;; touch key(1): now key(2) becomes LRU
    (multiple-value-bind (_v f)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (declare (ignore _v))
      (assert-true f))
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(3) :c)
    (multiple-value-bind (_v f)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(2))
      (declare (ignore _v))
      (assert-false f))
    (multiple-value-bind (v1 f1)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (assert-true f1)
      (assert-eq :a v1))
    (multiple-value-bind (v3 f3)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(3))
      (assert-true f3)
      (assert-eq :c v3))))

;;; ─── opt-function-body-instruction-tables ───────────────────────────────────

(deftest opt-body-instruction-set-empty
  "opt-function-body-instruction-tables on empty func-defs returns empty tables."
  (multiple-value-bind (ht _labels)
      (cl-cc/optimize::opt-function-body-instruction-tables (make-hash-table :test #'equal))
    (assert-= 0 (hash-table-count ht))))

(deftest opt-body-instruction-set-contains-body-insts
  "opt-function-body-instruction-tables body-inst-set includes every body instruction."
  (let* ((c1    (make-vm-const :dst :r1 :value 42))
         (ret   (make-vm-ret   :reg :r1))
         (fdefs (make-hash-table :test #'equal)))
    (setf (gethash "f" fdefs)
          (list :closure nil :params nil :body (list c1 ret)))
    (multiple-value-bind (s _labels)
        (cl-cc/optimize::opt-function-body-instruction-tables fdefs)
      (assert-true (gethash c1  s))
      (assert-true (gethash ret s)))))

(deftest opt-body-instruction-labels-maps-to-label
  "opt-function-body-instruction-tables inst->label maps each body inst to its label."
  (let* ((c1    (make-vm-const :dst :r0 :value 1))
         (ret   (make-vm-ret   :reg :r0))
         (fdefs (make-hash-table :test #'equal)))
    (setf (gethash "myfn" fdefs)
          (list :closure nil :params nil :body (list c1 ret)))
    (multiple-value-bind (_set inst->label)
        (cl-cc/optimize::opt-function-body-instruction-tables fdefs)
      (assert-equal "myfn" (gethash c1  inst->label))
      (assert-equal "myfn" (gethash ret inst->label)))))

;;; ─── opt-reachable-function-labels ──────────────────────────────────────────

(deftest opt-reachable-empty-roots
  "opt-reachable-function-labels returns empty set when roots is empty."
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (let ((r (cl-cc/optimize::opt-reachable-function-labels graph roots)))
      (assert-= 0 (hash-table-count r)))))

(deftest opt-reachable-direct-root
  "opt-reachable-function-labels includes the root label itself."
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "fn" graph) nil)
    (setf (gethash "fn" roots) t)
    (let ((r (cl-cc/optimize::opt-reachable-function-labels graph roots)))
      (assert-true (gethash "fn" r)))))

(deftest opt-reachable-transitive
  "opt-reachable-function-labels follows call edges transitively."
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (setf (gethash "b" graph) '("c"))
    (setf (gethash "c" graph) nil)
    (setf (gethash "a" roots) t)
    (let ((r (cl-cc/optimize::opt-reachable-function-labels graph roots)))
      (assert-true (gethash "a" r))
      (assert-true (gethash "b" r))
      (assert-true (gethash "c" r)))))

(deftest opt-reachable-cycle-terminates
  "opt-reachable-function-labels terminates on cyclic call graphs."
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "x" graph) '("y"))
    (setf (gethash "y" graph) '("x"))
    (setf (gethash "x" roots) t)
    (let ((r (cl-cc/optimize::opt-reachable-function-labels graph roots)))
      (assert-true  (gethash "x" r))
      (assert-true  (gethash "y" r)))))
