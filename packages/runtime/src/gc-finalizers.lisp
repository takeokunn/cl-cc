;;;; packages/runtime/src/gc-finalizers.lisp - Finalizer scheduling (FR-732)

(in-package :cl-cc/runtime)

(defvar *rt-finalizer-registry* nil
  "Canonical list of (object . finalizer-function) pairs awaiting object death.")

(defvar *rt-finalization-queue* nil
  "Objects whose finalizers were scheduled during the current GC cycle.")

(defvar *finalizer-queue* nil
  "Backward-compatible alias storage for *RT-FINALIZER-REGISTRY*.")

(defvar *pending-finalizers* nil
  "Finalizers scheduled during GC and executed after the GC pause.")

(defun %rt-sync-finalizer-aliases ()
  (setf *finalizer-queue* *rt-finalizer-registry*)
  *rt-finalizer-registry*)

(defun register-finalizer (object finalizer-fn)
  "Register FINALIZER-FN to run with OBJECT after OBJECT becomes unreachable."
  (check-type finalizer-fn function)
  (rt-unregister-finalizer object)
  (push (cons object finalizer-fn) *rt-finalizer-registry*)
  (%rt-sync-finalizer-aliases)
  object)

(defun rt-register-finalizer (object finalizer-fn)
  "Runtime-prefixed alias for REGISTER-FINALIZER."
  (register-finalizer object finalizer-fn))

(defun rt-unregister-finalizer (object)
  "Remove any finalizer registered for OBJECT."
  (setf *rt-finalizer-registry*
        (delete object *rt-finalizer-registry* :key #'car :test #'eql))
  (%rt-sync-finalizer-aliases)
  object)

(defun rt-finalize (object function)
  "Register FUNCTION to run with OBJECT when OBJECT becomes unreachable."
  (register-finalizer object function))

(defun rt-cancel-finalization (object)
  "Cancel any pending finalization for OBJECT."
  (rt-unregister-finalizer object))

(defun %rt-gc-process-finalizers (heap marked-set)
  "Schedule finalizers for unreachable objects without running them in the GC pause.

Objects with newly pending finalizers are marked so they survive this collection.
The registry entry is removed immediately, preventing repeated finalization; if no
finalizer resurrects the object, a later collection may reclaim it."
  (let ((remaining nil)
        (pending nil))
    (dolist (entry *rt-finalizer-registry*)
      (destructuring-bind (object . function) entry
        (if (%rt-gc-reference-live-p heap marked-set object)
            (push entry remaining)
            (progn
              (%rt-gc-mark-reference-value heap marked-set object)
              (push (cons object function) pending)))))
    (setf *rt-finalizer-registry* (nreverse remaining)
          *pending-finalizers* (nconc *pending-finalizers* (nreverse pending))
          *rt-finalization-queue* (mapcar #'car *pending-finalizers*))
    (%rt-sync-finalizer-aliases)))

(defun rt-run-pending-finalizers ()
  "Execute finalizers scheduled by the last GC cycle."
  (let ((pending *pending-finalizers*)
        (count 0))
    (setf *pending-finalizers* nil
          *rt-finalization-queue* nil)
    (dolist (entry pending count)
      (destructuring-bind (object . function) entry
        (handler-case
            (progn
              (funcall function object)
              (incf count))
          (error (e)
            (format *error-output*
                    "WARNING: Finalizer for ~S failed: ~A~%" object e)))))))
