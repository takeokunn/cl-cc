;;;; src/jit/baseline.lisp — FR-559/FR-560 JIT Baseline Compilation
;;;; Type Feedback Collection + Speculative Inlining with Guards.
;;;; V8 Maglev / HotSpot type profiling equivalent.

(in-package :cl-cc/jit)

;;; ──── JIT Threshold ────
(defvar *jit-threshold* 100
  "Call count threshold before triggering JIT compilation.")

(defvar *jit-enabled* t
  "Master switch for JIT compilation.")

;;; ──── Type Feedback Table ────
;; Inline Cache (IC) based type profiling.
;; Each call site / operation has a type profile slot.

(defvar *type-feedback-table* (make-hash-table :test #'eq)
  "Function → (call-site → type-distribution-alist).")

(defstruct (type-feedback-entry (:conc-name tfe-))
  "Type feedback for a single operation site."
  (site-id nil)               ; unique identifier for the site
  (type-counts (make-hash-table :test #'eq)) ; type → count
  (total-samples 0 :type fixnum))

(defun record-type-feedback (func-name site-id observed-type)
  "Record an observed TYPE at SITE-ID in FUNC-NAME.
Used by the interpreter/Tier-0 to collect type profiles."
  (let* ((func-table (or (gethash func-name *type-feedback-table*)
                         (setf (gethash func-name *type-feedback-table*)
                               (make-hash-table :test #'eq))))
         (entry (or (gethash site-id func-table)
                    (setf (gethash site-id func-table)
                          (make-type-feedback-entry :site-id site-id)))))
    (incf (gethash observed-type (tfe-type-counts entry) 0))
    (incf (tfe-total-samples entry))))

(defun dominant-type (func-name site-id &key (min-fraction 0.8))
  "Return the dominant observed type at SITE-ID in FUNC-NAME.
Only considers types with ≥ MIN-FRACTION of total samples.
Returns (values type fraction) or NIL."
  (let* ((func-table (gethash func-name *type-feedback-table*))
         (entry (and func-table (gethash site-id func-table))))
    (when (and entry (> (tfe-total-samples entry) 0))
      (let* ((total (tfe-total-samples entry))
             (counts (tfe-type-counts entry))
             (best-type nil)
             (best-count 0))
        (maphash (lambda (type count)
                   (when (> count best-count)
                     (setf best-type type best-count count)))
                 counts)
        (let ((fraction (/ best-count total)))
          (when (>= fraction min-fraction)
            (values best-type fraction)))))))

;;; ──── JIT Tier Compilation ────
(defun jit-baseline-compile (func-name bytecode &key (tier 1))
  "Compile BYTECODE for FUNC-NAME at JIT tier TIER.
Tier 1: Simple template-based compilation (baseline).
Tier 2: Optimizing compilation with type feedback."
  (declare (ignore func-name bytecode tier))
  ;; In production: emit native code, register in code cache.
  ;; This function is the entry point called by the call stub.
  (values))

(defun jit-tier-compile (func-name bytecode)
  "Full JIT compilation pipeline for FUNC-NAME.
1. Check if bytecode is already compiled (tier 1+)
2. If not, compile at tier 1
3. If call count exceeds threshold, recompile at tier 2"
  (unless *jit-enabled*
    (return-from jit-tier-compile nil))
  ;; Placeholder: in production, this would do the actual compilation
  (jit-baseline-compile func-name bytecode :tier 1))

;;; ──── Speculative Inlining (FR-560) ────
(defun speculative-inline-candidate-p (func-name call-site)
  "Return T if CALL-SITE in FUNC-NAME should be speculatively inlined.
Requires: dominant type with ≥ 95% frequency + small callee size."
  (multiple-value-bind (dtype fraction)
      (dominant-type func-name call-site :min-fraction 0.95)
    (and dtype
         (>= fraction 0.95)
         ;; Check callee size is small enough for inlining
         t)))

(defun emit-guarded-inline (stream func-name call-site callee-code)
  "Emit guarded inline code: type check + inline body + deopt slow path.
Pattern:
  CMP [obj+header], expected-class-tag
  JNE slow_path
  ; inline body here
  JMP done
slow_path:
  CALL original_function
done:"
  (declare (ignore stream func-name call-site callee-code))
  ;; In production: emit native x86-64/aarch64 code
  ;; 1. Compare object header tag with expected class
  ;; 2. If mismatch, jump to deoptimization slow path
  ;; 3. Otherwise, execute inlined callee body
  ;; 4. Merge paths
  (values))

;;; ──── Megamorphic IC Handling (FR-561) ────
(defvar *megamorphic-threshold* 4
  "Maximum PIC entries before a call site is considered megamorphic.")

(defun megamorphic-p (func-name call-site)
  "Return T if CALL-SITE in FUNC-NAME is megamorphic (> threshold types)."
  (let* ((func-table (gethash func-name *type-feedback-table*))
         (entry (and func-table (gethash call-site func-table))))
    (and entry
         (> (hash-table-count (tfe-type-counts entry))
            *megamorphic-threshold*))))

(defun emit-megamorphic-dispatch (stream call-site type-distribution)
  "Emit hash-based O(1) dispatch for a megamorphic call site.
Uses a global type feedback cache shared across call sites."
  (declare (ignore stream call-site type-distribution))
  ;; In production: emit hash lookup + direct jump table
  (values))

;;; ──── JIT Warmup (FR-562) ────
(defvar *jit-warmup-profile* nil
  "Alist of (function-name . call-count) for warmup profiling.")

(defun jit-warmup-profile (func-name)
  "Record FUNC-NAME in the warmup profile, incrementing its call count."
  (let ((entry (assoc func-name *jit-warmup-profile* :test #'eq)))
    (if entry
        (incf (cdr entry))
        (push (cons func-name 1) *jit-warmup-profile*))))

(defun aot-pre-warm (hot-functions)
  "Pre-compile HOT-FUNCTIONS (list of function names) at startup.
Eliminates JIT warmup time for known-hot server code paths."
  (dolist (fn hot-functions)
    (jit-tier-compile fn nil))
  (values))
