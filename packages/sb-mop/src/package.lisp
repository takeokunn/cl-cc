;;;; packages/sb-mop/src/package.lisp --- SB-MOP compatibility facade

(defpackage :cl-cc/sb-mop
  #-sbcl (:nicknames :sb-mop)
  (:use :cl)
  (:import-from :cl-cc/vm
   #:compute-class-precedence-list
   #:slot-definition-name
   #:slot-definition-location
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-type
   #:slot-definition-allocation
   #:compute-effective-slot-definition
   #:generic-function-methods
   #:generic-function-method-combination)
  (:export
   #:class-direct-slots
   #:class-direct-superclasses
   #:class-direct-subclasses
   #:class-precedence-list
   #:class-slots
   #:class-default-initargs
   #:compute-class-precedence-list
   #:ensure-class
   #:ensure-class-using-class
   #:find-class
   #:slot-definition-name
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-type
   #:slot-definition-allocation
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-location
   #:compute-effective-slot-definition
   #:generic-function-methods
   #:generic-function-method-combination
   #:method-specializers
   #:method-qualifiers
   #:add-method
   #:remove-method
   #:find-method
   #:standard-instance-access
   #:funcallable-standard-instance-access))

(in-package :cl-cc/sb-mop)

;;; On SBCL the implementation-owned SB-MOP package already exists and remains
;;; untouched.  CL-CC exposes its portable facade as CL-CC/SB-MOP; on other
;;; hosts this package also provides the SB-MOP nickname.
