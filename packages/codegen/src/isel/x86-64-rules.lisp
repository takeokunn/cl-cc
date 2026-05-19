(in-package :cl-cc/codegen)
;;;; FR-299: MIR ISel rules — x86-64 target
(defparameter *isel-x86-64-opcode-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :add ht) :add)   (setf (gethash :sub ht) :sub)
    (setf (gethash :mul ht) :imul)  (setf (gethash :div ht) :idiv)
    (setf (gethash :load ht) :mov)  (setf (gethash :store ht) :mov)
    (setf (gethash :branch ht) :jmp)(setf (gethash :branch-eq ht) :je)
    (setf (gethash :branch-ne ht) :jne)(setf (gethash :call ht) :call)
    (setf (gethash :return ht) :ret)(setf (gethash :move ht) :mov)
    (setf (gethash :const ht) :mov-imm)(setf (gethash :nop ht) :nop)
    ht))
