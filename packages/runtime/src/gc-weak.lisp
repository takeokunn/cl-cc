;;;; packages/runtime/src/gc-weak.lisp - Weak pointer API (FR-730)

(in-package :cl-cc/runtime)

(defvar *weak-references* nil
  "Global list of weak pointer objects scanned after regular GC marking.")

(defun make-weak-pointer (object)
  "Create a weak pointer wrapper for OBJECT.

The weak pointer itself may be strongly referenced, but its target is not treated
as a root by runtime GC reference processing."
  (let ((weak-pointer (rt-make-weak-pointer object nil)))
    (pushnew weak-pointer *weak-references* :test #'eq)
    weak-pointer))

(defun weak-pointer-value (weak-pointer)
  "Return WEAK-POINTER's target, or NIL after the GC clears it."
  (check-type weak-pointer rt-weak-ref)
  (rt-weak-ref-referent weak-pointer))

(defun (setf weak-pointer-value) (value weak-pointer)
  "Retarget WEAK-POINTER to VALUE without creating a strong GC root."
  (check-type weak-pointer rt-weak-ref)
  (setf (rt-weak-ref-referent weak-pointer) value))

(defun %rt-gc-process-weak-pointers (heap marked-set)
  "Compatibility entry point for legacy callers.

The canonical weak-reference implementation lives in gc-references.lisp and is
driven by *RT-REFERENCE-REGISTRY*.  This wrapper preserves the older
*WEAK-REFERENCES* list as a diagnostic subset without defining a second weak
pointer implementation."
  (%rt-gc-process-weak-references heap marked-set))
