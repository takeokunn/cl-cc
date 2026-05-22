;;;; packages/debug/src/package.lisp -- Debugging package facade.

(defpackage :cl-cc/debug
  (:use :cl :cl-cc/bootstrap)
  (:shadow #:inspect)
  (:import-from :cl-cc/vm
                #:vm-closure-object
                #:vm-closure-captured-regs
                #:vm-closure-captured-vals)
  (:export
   ;; FR-687: minimal Swank protocol skeleton
   #:swank-server
   #:swank-server-host
   #:swank-server-port
   #:swank-server-socket
   #:swank-server-thread
   #:swank-server-running-p
   #:create-server
   #:stop-server
   #:interactive-eval
   #:compile-string
   #:completions
   #:operator-arglist
   ;; FR-688: object inspector
   #:*inspected-objects*
   #:inspect))

(in-package :cl-cc/debug)
