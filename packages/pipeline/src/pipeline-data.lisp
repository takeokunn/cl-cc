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
  (pass-pipeline      nil)
  (print-pass-timings nil)
  (timing-stream      nil)
  (print-pass-stats   nil)
  (stats-stream       nil)
  (trace-json-stream  nil)
  (print-opt-remarks  nil)
  (opt-remarks-stream nil)
  (opt-remarks-mode   :all    :type keyword))

(defun %opts->optimize-kwargs (opts)
  "Extract the pass/stats/remarks subset of OPTS as a keyword plist for
optimize-instructions."
  (list :pass-pipeline      (pipeline-opts-pass-pipeline opts)
        :print-pass-timings (pipeline-opts-print-pass-timings opts)
        :timing-stream      (pipeline-opts-timing-stream opts)
        :print-pass-stats   (pipeline-opts-print-pass-stats opts)
        :stats-stream       (pipeline-opts-stats-stream opts)
        :trace-json-stream  (pipeline-opts-trace-json-stream opts)
        :print-opt-remarks  (pipeline-opts-print-opt-remarks opts)
        :opt-remarks-stream (pipeline-opts-opt-remarks-stream opts)
        :opt-remarks-mode   (pipeline-opts-opt-remarks-mode opts)))

(defun %opts->compile-kwargs (opts)
  "Spread all OPTS fields into a keyword plist for compile-expression /
compile-toplevel-forms."
  (list* :target     (pipeline-opts-target opts)
         :type-check (pipeline-opts-type-check opts)
         :safety     (pipeline-opts-safety opts)
         (%opts->optimize-kwargs opts)))

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


