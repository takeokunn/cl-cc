;;;; packages/sb-pcl/src/package.lisp --- SB-PCL compatibility facade

(defpackage :cl-cc/sb-pcl
  #-sbcl (:nicknames :sb-pcl)
  (:use :cl)
  (:import-from :cl-cc/vm
   #:vm-generic-function-p)
  (:export
   #:satiating-gfs-p))

(in-package :cl-cc/sb-pcl)

;;; On SBCL the implementation-owned SB-PCL package already exists and remains
;;; untouched.  CL-CC exposes its portable facade as CL-CC/SB-PCL; on other
;;; hosts this package also provides the SB-PCL nickname.

(defun satiating-gfs-p (gf)
  "Return T when GF is a CL-CC generic-function dispatch table marked satiated.

Non-generic-function values return NIL.  This is the minimal compatibility
surface for SBCL's internal SB-PCL:SATIATING-GFS-P predicate and mirrors the
runtime flag used by the standard-library SATIATED-GENERIC-FUNCTION-P helper."
  (if (and (vm-generic-function-p gf)
           (gethash :__satiated__ gf))
      t
      nil))
