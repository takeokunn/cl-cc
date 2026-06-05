(in-package :cl-cc/vm)

;;; ─── FR-843 Float Exception Control ──────────────────────────────────────────
;;;
;;; Query and control floating-point exception traps on the host SBCL.
;;; This is an SBCL-only codebase; no portability guards are needed.

(defvar *floating-point-modes*
  (sb-int:get-floating-point-modes)
  "Current floating-point modes plist.  The plist is the one
returned by SB-INT:GET-FLOATING-POINT-MODES and accepted by
SB-INT:SET-FLOATING-POINT-MODES.")

(defun get-float-traps ()
  "Return the list of currently enabled floating-point exception traps.
Each element is a keyword: :DIVIDE-BY-ZERO, :OVERFLOW, :UNDERFLOW,
:INEXACT, or :INVALID."
  (getf (sb-int:get-floating-point-modes) :traps))

(defmacro with-float-traps-masked (traps &body body)
  "Execute BODY with the specified floating-point traps masked.
TRAPS is a list of zero or more of: :DIVIDE-BY-ZERO, :OVERFLOW,
:UNDERFLOW, :INEXACT, :INVALID.  Within BODY the listed exceptions
will not signal but will still set the corresponding status flags."
  `(sb-int:with-float-traps-masked ,traps ,@body))

(export '(*floating-point-modes*
          get-float-traps
          with-float-traps-masked))
