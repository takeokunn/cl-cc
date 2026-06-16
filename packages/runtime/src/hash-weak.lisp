;;;; packages/runtime/src/hash-weak.lisp - Weak hash table public constructor

(in-package :cl-cc/runtime)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'make-hash-table :cl-cc/runtime))

(defun make-hash-table (&rest args)
  "Runtime MAKE-HASH-TABLE supporting :WEAKNESS.

  When WEAKNESS is NIL this returns an ordinary host hash table.  When WEAKNESS is
  one of :KEY, :VALUE, :KEY-AND-VALUE, or :KEY-OR-VALUE, the result is an
  RT-WEAK-HASH-TABLE wrapper tracked by the runtime GC."
  (apply #'rt-make-hash-table args))
