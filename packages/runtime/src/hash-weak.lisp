;;;; packages/runtime/src/hash-weak.lisp - Weak hash table public constructor

(in-package :cl-cc/runtime)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'make-hash-table :cl-cc/runtime))

(defvar *weak-hash-tables* nil
  "Backward-compatible mirror of runtime weak hash tables.")

(defun make-hash-table (&rest args &key weakness &allow-other-keys)
  "Runtime MAKE-HASH-TABLE supporting :WEAKNESS.

When WEAKNESS is NIL this returns an ordinary host hash table.  When WEAKNESS is
one of :KEY, :VALUE, :KEY-AND-VALUE, or :KEY-OR-VALUE, the result is an
RT-WEAK-HASH-TABLE wrapper tracked by the runtime GC."
  (let ((table (apply #'rt-make-hash-table args)))
    (when (rt-weak-hash-table-p table)
      (pushnew table *weak-hash-tables* :test #'eq))
    table))
