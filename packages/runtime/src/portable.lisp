;;;; packages/runtime/src/portable.lisp — Self-Host Portability Facades
;;;;
;;;; Thin wrappers over cl-cc/runtime synchronization primitives and
;;;; SBCL-specific APIs. Provides consistent names (rt-make-lock,
;;;; rt-with-lock) that map to sync.lisp's API. Under self-host,
;;;; provides deterministic single-thread/no-op/stub behavior.
;;;;
;;;; This file must be loaded AFTER sync.lisp in the ASDF component order.

(in-package :cl-cc/runtime)

;; ── Unsupported operation ─────────────────────────────────────────────────

(define-condition rt-unsupported-operation (simple-error)
  ((operation :initarg :operation :reader rt-unsupported-operation-name))
  (:report (lambda (c s)
             (format s "Runtime operation not supported in this context: ~A"
                     (rt-unsupported-operation-name c)))))

(defun rt-unsupported (operation)
  (cerror "Continue (may cause undefined behavior)"
          'rt-unsupported-operation :operation operation))

;; ── Thread identity ───────────────────────────────────────────────────────

(defun rt-current-thread-token ()
  "Return a stable identifier for the current thread."
  sb-thread:*current-thread*)

;; ── Lock wrappers (map to sync.lisp API) ─────────────────────────────────

(defun rt-make-lock (&optional name)
  "Create a lock. Maps to rt-make-mutex from sync.lisp."
  (rt-make-mutex :name name))

(defmacro rt-with-lock ((lock) &body body)
  "Execute BODY with LOCK held. Maps to rt-with-mutex from sync.lisp."
  `(rt-with-mutex (,lock) ,@body))

(defun rt-lock (lock &optional (wait-p t))
  "Acquire LOCK. Maps to rt-mutex-lock from sync.lisp."
  (rt-mutex-lock lock :timeout (unless wait-p 0)))

(defun rt-unlock (lock)
  "Release LOCK. Maps to rt-mutex-unlock from sync.lisp."
  (rt-mutex-unlock lock))

(defun rt-try-lock (lock)
  "Try to acquire LOCK without blocking."
  (rt-mutex-lock lock :timeout 0))

;; ── Thread yield ──────────────────────────────────────────────────────────

(defun rt-thread-yield ()
  "Yield the current thread."
  (sb-thread:thread-yield))

;; ── Atomic compare-and-swap ──────────────────────────────────────────────

(defun rt-atomic-compare-and-swap-symbol (symbol old new)
  "Atomically CAS the value of SYMBOL."
  (sb-ext:compare-and-swap (symbol-value symbol) old new))

;; ── Timeout macro ─────────────────────────────────────────────────────────

(defmacro rt-with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY with a timeout of SECONDS."
  `(handler-case
       (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout ()
       ,@timeout-forms)))
