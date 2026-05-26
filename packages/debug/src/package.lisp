;;;; packages/debug/src/package.lisp -- Debugging package facade.

(defpackage :cl-cc/debug
  (:use :cl :cl-cc/bootstrap)
  (:shadow #:inspect)
  (:import-from :cl-cc/vm
                #:vm-closure-object
                #:vm-closure-captured-regs
                #:vm-closure-captured-vals
                #:vm-state
                #:vm-reg-set
                #:vm-state-registers)
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
    #:inspect
    ;; FR-314: VM watchpoints
    #:*vm-watchpoints*
    #:vm-watchpoint-condition
    #:vm-watchpoint-reg
    #:vm-watchpoint-old-value
    #:vm-watchpoint-new-value
    #:add-vm-watchpoint
    #:remove-vm-watchpoint
    #:clear-vm-watchpoints))

(in-package :cl-cc/debug)
