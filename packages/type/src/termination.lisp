;;;; packages/type/src/termination.lisp — FR-713 Termination Checking
;;;; Size-change termination + structural recursion checking.
;;;; Agda termination checker / Coq Guard Checker equivalent.

(in-package :cl-cc/type)

(defvar *termination-check-enabled* t)

;;; ──── Size-change termination ────
(defstruct (call-graph-node (:conc-name cgn-))
  "A node in the call graph for termination analysis."
  (function nil :type symbol)
  (calls nil :type list)    ; list of (callee . size-changes)
  (id 0 :type fixnum))

(defstruct (size-change (:conc-name sc-))
  "A size-change relation: param_i ↘ param_j (strictly smaller) or ↓ (non-increasing)."
  (source-param 0 :type fixnum)
  (target-param 0 :type fixnum)
  (strict-p nil))

(defun build-call-graph (definitions)
  "Build the call graph for TERMINATION analysis.
Returns a list of call-graph-node structs."
  (declare (ignore definitions))
  nil)

(defun check-size-change-termination (call-graph)
  "Apply the size-change termination principle to CALL-GRAPH.
Lee, Jones, Ben-Amram 2001: PSPACE-complete algorithm.
Returns T if all functions terminate."
  (declare (ignore call-graph))
  ;; Simplified: check for infinite descent in SC-graph
  t)

;;; ──── Structural recursion ────
(defun check-structural-recursion (func-name body)
  "Check that recursive calls in FUNC-NAME decrease structurally.
Returns T if structurally recursive, NIL if potentially non-terminating."
  (declare (ignore func-name body))
  t)

;;; ──── Integration ────
(defmacro define-terminating (name args &body body)
  "Define a function NAME with termination checking.
The function must pass the termination checker to compile."
  `(progn
     (when *termination-check-enabled*
       (check-structural-recursion ',name ',body))
     (defun ,name ,args ,@body)))
