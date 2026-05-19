;;;; packages/closer-mop/src/package.lisp --- Closer-MOP compatibility facade

(defpackage :closer-mop
  (:nicknames :cl-cc/closer-mop)
  (:use :cl)
  (:import-from :cl-cc/sb-mop
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
   #:funcallable-standard-instance-access)
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

(in-package :closer-mop)
