(in-package :cl-cc/codegen)
;;;; FR-299: MIR ISel rules — AArch64 target
(defparameter *isel-aarch64-opcode-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :add ht) :add)   (setf (gethash :sub ht) :sub)
    (setf (gethash :mul ht) :mul)   (setf (gethash :div ht) :sdiv)
    (setf (gethash :load ht) :ldr)  (setf (gethash :store ht) :str)
    (setf (gethash :branch ht) :b)  (setf (gethash :branch-eq ht) :b.eq)
    (setf (gethash :branch-ne ht) :b.ne)(setf (gethash :call ht) :blr)
    (setf (gethash :return ht) :ret)(setf (gethash :move ht) :mov)
    (setf (gethash :const ht) :movz)(setf (gethash :nop ht) :nop)
    ht))
