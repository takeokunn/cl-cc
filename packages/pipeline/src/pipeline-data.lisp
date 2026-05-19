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
  (print-pass-timings nil)
  (timing-stream      nil)
  (print-pass-stats   nil)
  (stats-stream       nil)
  (trace-json-stream  nil)
  (coverage           nil)
  (print-opt-remarks  nil)
  (opt-remarks-stream nil)
  (opt-remarks-mode   :all    :type keyword)
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
  (pgo-profile-data   nil))

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
