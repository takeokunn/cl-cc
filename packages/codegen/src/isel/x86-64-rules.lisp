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

(defparameter *isel-x86-64-rules*
  (list
   (register-isel-rule
    (make-isel-rule :name :x86-mov-imm :target :x86-64
                    :pattern '(:const ?value) :result-op :mov-imm :cost 1 :size 1))
   (register-isel-rule
    (make-isel-rule :name :x86-reg :target :x86-64
                    :pattern '(:reg ?name) :result-op :reg :cost 0 :size 1))
   (register-isel-rule
    (make-isel-rule :name :x86-move :target :x86-64
                    :pattern '(:move ?src) :result-op :mov :cost 1 :size 1))
   (register-isel-rule
    (make-isel-rule :name :x86-add-reg :target :x86-64
                    :pattern '(:add ?lhs ?rhs) :result-op :add :cost 1 :size 1))
   (register-isel-rule
    (make-isel-rule :name :x86-add-mem-reg :target :x86-64
                    :pattern '(:add (:load ?base ?disp) ?rhs)
                    :result-op :add-mem-reg :cost 1 :size 2))
   (register-isel-rule
    (make-isel-rule :name :x86-sub-reg :target :x86-64
                    :pattern '(:sub ?lhs ?rhs) :result-op :sub :cost 1 :size 1))
   (register-isel-rule
    (make-isel-rule :name :x86-mul-reg :target :x86-64
                    :pattern '(:mul ?lhs ?rhs) :result-op :imul :cost 2 :size 1))
   (register-isel-rule
    (make-isel-rule :name :x86-address-base-index-scale-disp :target :x86-64
                    :pattern '(:address ?base (:mul ?index (:const ?scale)) (:const ?disp))
                    :result-op :x86-address :cost 0 :size 4))
   (register-isel-rule
    (make-isel-rule :name :x86-lea-address :target :x86-64
                    :pattern '(:add ?base (:add (:mul ?index (:const ?scale)) (:const ?disp)))
                    :result-op :lea :cost 1 :size 5)))
  "x86-64 maximal-munch rules, including addressing and LEA tiles.")
