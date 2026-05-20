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

(defparameter *isel-aarch64-rules*
  (list
   (register-isel-rule
    (make-isel-rule :name :a64-movz-imm :target :aarch64
                    :pattern '(:const ?value) :result-op :movz :cost 1 :size 1))
   (register-isel-rule
    (make-isel-rule :name :a64-reg :target :aarch64
                    :pattern '(:reg ?name) :result-op :reg :cost 0 :size 1))
   (register-isel-rule
    (make-isel-rule :name :a64-move :target :aarch64
                    :pattern '(:move ?src) :result-op :mov :cost 1 :size 1))
   (register-isel-rule
    (make-isel-rule :name :a64-add-reg :target :aarch64
                    :pattern '(:add ?lhs ?rhs) :result-op :add :cost 1 :size 1))
   (register-isel-rule
    (make-isel-rule :name :a64-add-scaled-address :target :aarch64
                    :pattern '(:add ?base (:mul ?index (:const ?scale)))
                    :result-op :add-scaled :cost 1 :size 3))
   (register-isel-rule
    (make-isel-rule :name :a64-sub-reg :target :aarch64
                    :pattern '(:sub ?lhs ?rhs) :result-op :sub :cost 1 :size 1))
   (register-isel-rule
    (make-isel-rule :name :a64-mul-reg :target :aarch64
                    :pattern '(:mul ?lhs ?rhs) :result-op :mul :cost 2 :size 1))
   (register-isel-rule
    (make-isel-rule :name :a64-ldr-scaled :target :aarch64
                    :pattern '(:load ?base (:mul ?index (:const ?scale)))
                    :result-op :ldr-scaled :cost 1 :size 3))
   (register-isel-rule
    (make-isel-rule :name :a64-bitfield-extract :target :aarch64
                    :pattern '(:band (:shr ?value (:const ?lsb)) (:const ?mask))
                    :result-op :ubfx :cost 1 :size 4)))
  "AArch64 maximal-munch rules, including scaled addressing and bitfield tiles.")
