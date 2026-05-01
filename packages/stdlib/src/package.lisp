;;;; packages/stdlib/src/package.lisp — feature package for cl-cc/stdlib
;;;;
;;;; Pure data: the standard library source as a string. Loaded before
;;;; pipeline so pipeline-stdlib can read *standard-library-source*.

(defpackage :cl-cc/stdlib
  (:use :cl)
  (:export
   #:*standard-library-source-core*
   #:*standard-library-source*))
