(in-package :cl-cc/pipeline)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compile — Pipeline Policy Data
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─────────────────────────────────────────────────────────────────────────
;;; Compilation-options bundle
;;; ─────────────────────────────────────────────────────────────────────────

(defparameter *definition-and-declaration-form-heads*
  '("DEFUN" "DEFMACRO" "DEFVAR" "DEFPARAMETER" "DEFCONSTANT"
    "DEFCLASS" "DEFMETHOD" "DEFGENERIC" "DEFSTRUCT"
    "DEFTYPE" "DEFINE-COMPILER-MACRO" "IN-PACKAGE"
    "DECLAIM" "PROCLAIM" "EVAL-WHEN")
  "Uppercased names of forms that define or declare rather than compute.
These cannot be batched through the CPS fast path.")

(defparameter *compilation-tier* 1
  "Default compilation tier. 0 is fast Tier-0 codegen with optimizer passes
disabled; 1 is Tier-1 full optimization.")

(defun normalize-compilation-tier (tier)
  "Normalize TIER to 0 or 1, signaling an error for invalid values."
  (let ((value (cond
                 ((null tier) *compilation-tier*)
                 ((integerp tier) tier)
                 ((stringp tier) (parse-integer tier :junk-allowed nil))
                 (t (error "Invalid compilation tier: ~S" tier)))))
    (unless (member value '(0 1))
      (error "Invalid compilation tier: ~S (expected 0 or 1)" tier))
    value))

(defstruct (pipeline-opts (:constructor %make-pipeline-opts))
  "All compilation options in one bundle. Pass as a single argument to internal
pipeline functions instead of threading 12 separate keyword arguments."
  (target             :x86_64 :type keyword)
  (type-check         nil)
  (safety             1)
  (speed              nil)
  (inline-threshold-scale 1)
  (block-compile     nil)
  (pass-pipeline      nil)
  (debug-info         nil)
  (sanitize           nil)
  (lto                nil)
  (eh-model           nil)
  (incremental        nil)
  (perf-map           nil)
  (bolt               nil)
  (bolt-profile       nil)
  (verify-transforms  nil)
  (parallel           nil)
  (print-pass-timings nil)
  (timing-stream      nil)
  (print-pass-stats   nil)
  (stats-stream       nil)
  (trace-json-stream  nil)
  (coverage           nil)
  (print-opt-remarks  nil)
  (opt-remarks-stream nil)
  (opt-remarks-mode   :all    :type keyword)
  (opt-bisect-limit   nil)
  (retpoline          nil)
  (spectre-mitigations nil)
  (stack-protector    nil)
  (shadow-stack       nil)
  (asan               nil)
  (msan               nil)
  (tsan               nil)
  (ubsan              nil)
  (hwasan             nil)
  (compress           nil)
  (pgo-profile-data   nil)
  (werror             nil)
  (werror-categories  nil)
  (compilation-tier   *compilation-tier* :type integer))

;;; ─────────────────────────────────────────────────────────────────────────

(defparameter *cps-host-eval-unsafe-ast-types*
  '(cl-cc/ast:ast-defun
    cl-cc/ast:ast-defvar
    cl-cc/ast:ast-defclass
    cl-cc/ast:ast-defgeneric
    cl-cc/ast:ast-defmethod
    cl-cc/ast:ast-flet
    cl-cc/ast:ast-labels
    cl-cc/ast:ast-block
    cl-cc/ast:ast-return-from
    cl-cc/ast:ast-catch
    cl-cc/ast:ast-throw
    cl-cc/ast:ast-tagbody
    cl-cc/ast:ast-go
    cl-cc/ast:ast-unwind-protect
    cl-cc/ast:ast-handler-case)
  "AST node types whose side effects require the full compile→VM pipeline.
Definition forms, non-local exits, and condition handling must not short-circuit
through the host CPS fast path.")
