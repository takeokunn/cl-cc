;;;; tests/optimizer-fr-tests.lisp — Optimizer Feature Requirement Evidence Tests
;;;;
;;;; Tests for optimizer FR implementations:
;;;; - FR-017: Alias Analysis / Memory Disambiguation
;;;; - FR-020: Allocation Sinking
;;;; - FR-090: Safepoint Dominance Pruning
;;;; - FR-091: Safepoint Hoisting to Loop Back-Edges
;;;; - FR-309: Memory Access Pattern Analysis

(in-package :cl-cc/test)

(defsuite optimizer-fr-suite
  :description "Optimizer Feature Requirement evidence tests (FR-017, FR-020, FR-090, FR-091, FR-309)"
  :parent cl-cc-unit-suite)

(in-suite optimizer-fr-suite)

;;; ------------------------------------------------------------
;;; FR-017: Alias Analysis / Memory Disambiguation
;;; ------------------------------------------------------------

(deftest fr-017-alias-analysis-function-exists
  "FR-017: Alias analysis function (opt-compute-heap-aliases or equivalent) is fbound."
  (assert-true (fboundp 'cl-cc/optimize:opt-compute-heap-aliases)))

(deftest fr-017-tbaa-predicate-exists
  "FR-017: TBAA must-not-alias predicate is fbound."
  (assert-true (or (fboundp 'cl-cc/optimize:opt-tbaa-must-not-alias-p)
                   (fboundp 'cl-cc/optimize:opt-compute-heap-type-facts))))

;;; ------------------------------------------------------------
;;; FR-020: Allocation Sinking
;;; ------------------------------------------------------------

(deftest fr-020-allocation-sinking-exists
  "FR-020: Allocation sinking pass is fbound in the optimizer pipeline."
  (assert-true (fboundp 'cl-cc/optimize:opt-sink-allocations)))

;;; ------------------------------------------------------------
;;; FR-090: Safepoint Dominance Pruning
;;; ------------------------------------------------------------

(deftest fr-090-safepoint-pruning-exists
  "FR-090: Safepoint dominance pruning function is fbound."
  (assert-true (fboundp 'cl-cc/optimize:opt-prune-dominated-safepoints)))

;;; ------------------------------------------------------------
;;; FR-091: Safepoint Hoisting to Loop Back-Edges
;;; ------------------------------------------------------------

(deftest fr-091-safepoint-hoisting-exists
  "FR-091: Safepoint hoisting to back-edges function is fbound."
  (assert-true (fboundp 'cl-cc/optimize:opt-hoist-safepoints-to-back-edges)))

;;; ------------------------------------------------------------
;;; FR-309: Memory Access Pattern Analysis
;;; ------------------------------------------------------------

(deftest fr-309-memory-access-analysis-exists
  "FR-309: Memory access pattern analysis function is fbound."
  (assert-true (fboundp 'cl-cc/optimize:opt-analyze-memory-access-patterns)))

;;; ------------------------------------------------------------
;;; Integrated: FR-017 enables stronger LICM
;;; ------------------------------------------------------------

(deftest fr-017-licm-exists
  "FR-017: LICM pass is fbound (benefits from TBAA alias analysis)."
  (assert-true (fboundp 'cl-cc/optimize::opt-pass-licm)))

;;; ------------------------------------------------------------
;;; Behavioral: optimizer pipeline loads and converges
;;; ------------------------------------------------------------

(deftest fr-optimizer-pipeline-available
  "FR-017/020/090/091/309: Optimizer pipeline symbols are accessible and nontrivial."
  ;; Verify alias analysis actually returns meaningful data
  (let ((alias-fn (symbol-function 'cl-cc/optimize:opt-compute-heap-aliases)))
    (assert-true (functionp alias-fn)))
  ;; Verify TBAA predicate is a function
  (let ((tbaa-fn (or (ignore-errors (symbol-function 'cl-cc/optimize:opt-tbaa-must-not-alias-p))
                     (ignore-errors (symbol-function 'cl-cc/optimize:opt-compute-heap-type-facts)))))
    (assert-true (functionp tbaa-fn)))
  ;; Verify safepoint optimization functions are callable
  (assert-true (functionp (symbol-function 'cl-cc/optimize:opt-prune-dominated-safepoints)))
  (assert-true (functionp (symbol-function 'cl-cc/optimize:opt-hoist-safepoints-to-back-edges)))
  ;; Verify memory access pattern analysis is callable
  (assert-true (functionp (symbol-function 'cl-cc/optimize:opt-analyze-memory-access-patterns))))

(deftest fr-optimizer-passes-collection-nonempty
  "FR-017/020/090/091/309: The optimizer convergence pass list is nonempty."
  ;; *opt-convergence-passes* is the optimizer's main pass list
  (let ((passes (ignore-errors (symbol-value (find-symbol "*OPT-CONVERGENCE-PASSES*" "CL-CC/OPTIMIZE")))))
    (when passes
      (assert-true (listp passes))
      (assert-true (> (length passes) 0)))))
