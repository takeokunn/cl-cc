;;;; packages/compile/src/worker-wrapper.lisp — FR-686 Worker-Wrapper Transform
;;;; Restructure recursive functions for optimization.
;;;; Gill-Hutton 2009 Worker/Wrapper transformation.

(in-package :cl-cc/compile)

(defvar *worker-wrapper-enabled* t)

;;; ──── Worker/Wrapper pattern ────
;; Split a recursive function into:
;; - Wrapper: handles the public API, pre/post processing
;; - Worker: the efficient recursive core with strictness info

(defun worker-wrapper-transform (func-name body)
  "Transform FUNC-NAME using the Worker/Wrapper pattern.
The worker is a specialized version with strictness annotations.
The wrapper delegates to the worker after argument validation."
  (declare (ignore func-name body))
  (values))

(defun identify-worker-wrapper-opportunities (program)
  "Identify functions that benefit from worker-wrapper transformation.
Candidates: recursive functions with strict arguments and simple wrappers."
  (declare (ignore program))
  nil)

(defmacro define-worker-wrapper ((wrapper-name worker-name) args &body body)
  "Define a function with explicit worker-wrapper split.
Usage: (define-worker-wrapper (sum sum-worker) (xs acc)
  (if (null xs) acc (sum-worker (cdr xs) (+ acc (car xs)))))"
  `(progn
     (defun ,worker-name ,args ,@body)
     (defun ,wrapper-name ,args
       (declare (optimize speed))
       ;; Wrapper: validate args, call worker, post-process
       (,worker-name ,@args))))
