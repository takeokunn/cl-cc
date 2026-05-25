;;; vm-dynamic-wind.lisp — FR-1060: dynamic-wind with continuation re-entry
;;;
;;; Full ANSI CL dynamic-wind: before/after thunks, non-local exit safety,
;;; and continuation re-entry tracking.

(in-package :cl-cc/vm)

(defvar *dynamic-wind-stack* nil)

(defstruct vm-dynamic-wind-frame
  "Tracks before/after thunks for re-entry on continuation invocation."
  before-thunk after-thunk (reentered-p nil))

(defun vm-dynamic-wind (before-thunk body-thunk after-thunk)
  "Execute BEFORE-THUNK, BODY-THUNK, then AFTER-THUNK with non-local exit safety."
  (funcall before-thunk)
  (let ((frame (make-vm-dynamic-wind-frame
                 :before-thunk before-thunk :after-thunk after-thunk)))
    (unwind-protect
         (let ((*dynamic-wind-stack* (cons frame *dynamic-wind-stack*)))
           (funcall body-thunk))
      (unless (vm-dynamic-wind-frame-reentered-p frame)
        (funcall after-thunk)))))

(defun vm-dynamic-wind-reenter (frame thunk)
  "Re-enter FRAME: run before, execute THUNK, run after.
   For continuation-based re-entry of dynamic-wind frames."
  (setf (vm-dynamic-wind-frame-reentered-p frame) t)
  (funcall (vm-dynamic-wind-frame-before-thunk frame))
  (unwind-protect (funcall thunk)
    (funcall (vm-dynamic-wind-frame-after-thunk frame))))

(defun vm-call-with-dynamic-wind-context (before after thunk)
  (vm-dynamic-wind before thunk after))

(defmacro dynamic-wind (before body after)
  "ANSI CL dynamic-wind macro."
  `(vm-dynamic-wind (lambda () ,before)
                    (lambda () ,body)
                    (lambda () ,after)))

(export '(dynamic-wind vm-dynamic-wind vm-call-with-dynamic-wind-context
          vm-dynamic-wind-frame vm-dynamic-wind-frame-p
          vm-dynamic-wind-frame-before-thunk vm-dynamic-wind-frame-after-thunk
          vm-dynamic-wind-reenter *dynamic-wind-stack*))
