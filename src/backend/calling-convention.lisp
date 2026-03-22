;;;; src/backend/calling-convention.lisp - Calling Convention Definitions
;;;
;;; Defines the physical register pools, caller/callee-saved sets,
;;; and argument registers for each target architecture.

(in-package :cl-cc)

(defclass calling-convention ()
  ((gpr-pool :initarg :gpr-pool :reader cc-gpr-pool
             :documentation "Physical registers available for allocation")
   (caller-saved :initarg :caller-saved :reader cc-caller-saved
                 :documentation "Registers clobbered by function calls")
   (callee-saved :initarg :callee-saved :reader cc-callee-saved
                 :documentation "Registers preserved across function calls")
   (arg-registers :initarg :arg-registers :reader cc-arg-registers
                  :documentation "Registers used for passing arguments")
   (return-register :initarg :return-register :reader cc-return-register
                    :documentation "Register for return values")
   (scratch-register :initarg :scratch-register :reader cc-scratch-register
                     :documentation "Reserved register for spill loads/stores")))

;;; x86-64 System V ABI
(defvar *x86-64-calling-convention*
  (make-instance 'calling-convention
    :gpr-pool '(:rax :rcx :rdx :rbx :rsi :rdi :r8 :r9 :r10 :r12 :r13 :r14 :r15)
    :caller-saved '(:rax :rcx :rdx :rsi :rdi :r8 :r9 :r10)
    :callee-saved '(:rbx :r12 :r13 :r14 :r15)
    :arg-registers '(:rdi :rsi :rdx :rcx :r8 :r9)
    :return-register :rax
    :scratch-register :r11))

;;; AArch64 AAPCS
(defvar *aarch64-calling-convention*
  (make-instance 'calling-convention
    :gpr-pool '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
                :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
                :x17 :x19 :x20 :x21 :x22 :x23 :x24 :x25 :x26 :x27 :x28)
    :caller-saved '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
                    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15 :x17)
    :callee-saved '(:x19 :x20 :x21 :x22 :x23 :x24 :x25 :x26 :x27 :x28)
    :arg-registers '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7)
    :return-register :x0
    :scratch-register :x16))
