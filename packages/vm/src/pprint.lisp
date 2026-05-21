(in-package :cl-cc/vm)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; ANSI CL Pretty Printer Support (FR-605)
;;;
;;; This layer keeps a small VM-visible dispatch table wrapper while delegating
;;; low-level line breaking to the host ANSI pretty printer.  The default table
;;; installs Lisp-form rules for common binding and definition forms so that
;;; VM-visible WRITE/FORMAT paths can bind CL:*PRINT-PPRINT-DISPATCH* without
;;; changing the existing FORMAT directive implementation.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defstruct (pprint-dispatch-table (:constructor %make-pprint-dispatch-table))
  "VM-visible pretty-print dispatch table backed by a host CL dispatch table."
  (host (cl:copy-pprint-dispatch nil) :type t)
  (entries nil :type list))

(defun %pprint-dispatch-host (table)
  (cond
    ((pprint-dispatch-table-p table) (pprint-dispatch-table-host table))
    ((null table) nil)
    (t table)))

(defun %pprint-wrap-function (function)
  (lambda (stream object)
    (funcall function stream object)))

(defparameter *print-pprint-dispatch* (%make-pprint-dispatch-table)
  "Current VM pretty-print dispatch table.")

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  "Return a copy of TABLE as a VM pprint-dispatch-table."
  (let* ((source (if (pprint-dispatch-table-p table)
                     table
                     nil))
         (host (cl:copy-pprint-dispatch (%pprint-dispatch-host table))))
    (%make-pprint-dispatch-table
     :host host
     :entries (and source (copy-list (pprint-dispatch-table-entries source))))))

(defun make-pprint-dispatch-table (&optional (from nil from-supplied-p))
  "Create a VM pprint dispatch table, copying FROM when supplied."
  (copy-pprint-dispatch (if from-supplied-p from nil)))

(defun set-pprint-dispatch (type-specifier function
                            &optional (priority 0) (table *print-pprint-dispatch*))
  "Install FUNCTION as the pretty printer for TYPE-SPECIFIER in TABLE."
  (let ((host (%pprint-dispatch-host table)))
    (cl:set-pprint-dispatch type-specifier
                            (and function (%pprint-wrap-function function))
                            priority
                            host)
    (when (pprint-dispatch-table-p table)
      (setf (pprint-dispatch-table-entries table)
            (acons type-specifier (list function priority)
                   (remove type-specifier
                           (pprint-dispatch-table-entries table)
                           :key #'car :test #'equal)))))
  nil)

(defun get-pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  "Return the dispatch function and specificity for OBJECT in TABLE."
  (if (pprint-dispatch-table-p table)
      (dolist (entry (pprint-dispatch-table-entries table) (values nil nil))
        (when (typep object (car entry))
          (destructuring-bind (function priority) (cdr entry)
            (return (values function priority)))))
      (values nil nil)))

(defmacro pprint-logical-block ((stream-symbol list &key prefix per-line-prefix suffix)
                                &body body)
  "Execute BODY in a logical pretty-printing block."
  `(cl:pprint-logical-block (,stream-symbol ,list
                             ,@(when prefix `(:prefix ,prefix))
                             ,@(when per-line-prefix `(:per-line-prefix ,per-line-prefix))
                             ,@(when suffix `(:suffix ,suffix)))
     ,@body))

(defun pprint-indent (relative-to n &optional (stream cl:*standard-output*))
  "Adjust indentation in the current logical block."
  (cl:pprint-indent relative-to n stream))

(defun pprint-newline (kind &optional (stream cl:*standard-output*))
  "Emit a conditional pretty-printer newline of KIND."
  (cl:pprint-newline kind stream))

(defun pprint-tab (kind colnum colinc &optional (stream cl:*standard-output*))
  "Emit a pretty-printer tab directive."
  (cl:pprint-tab kind colnum colinc stream))

(defun %pprint-linear-list (stream list)
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (loop for tail = list then (cdr tail)
          for firstp = t then nil
          while (consp tail)
          do (unless firstp (write-char #\Space stream) (pprint-newline :fill stream))
             (write (car tail) :stream stream)
          finally (when tail
                    (write-string " . " stream)
                    (write tail :stream stream)))))

(defun %pprint-body (stream forms)
  (loop for form in forms
        for firstp = t then nil
        do (unless firstp (pprint-newline :linear stream))
           (write form :stream stream)))

(defun %pprint-defun-like (stream form)
  (pprint-logical-block (stream form :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (write-char #\Space stream)
    (write (pprint-pop) :stream stream)
    (when form
      (write-char #\Space stream)
      (write (pprint-pop) :stream stream))
    (pprint-indent :block 2 stream)
    (when form
      (pprint-newline :linear stream)
      (%pprint-body stream form))))

(defun %pprint-binding-form (stream form)
  (pprint-logical-block (stream form :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (write-char #\Space stream)
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 2 stream)
    (when form
      (pprint-newline :linear stream)
      (%pprint-body stream form))))

(defun %pprint-if-form (stream form)
  (pprint-logical-block (stream form :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 2 stream)
    (loop while form
          do (write-char #\Space stream)
             (pprint-newline :linear stream)
             (write (pprint-pop) :stream stream))))

(defun %pprint-progn-like (stream form)
  (pprint-logical-block (stream form :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 2 stream)
    (when form
      (pprint-newline :linear stream)
      (%pprint-body stream form))))

(defun %pprint-lisp-form (stream object)
  (let ((head (and (consp object) (car object))))
    (case head
      ((defun defmacro defgeneric defmethod lambda) (%pprint-defun-like stream object))
      ((let let* flet labels macrolet symbol-macrolet) (%pprint-binding-form stream object))
      ((if when unless) (%pprint-if-form stream object))
      ((progn block tagbody catch unwind-protect) (%pprint-progn-like stream object))
      (otherwise (%pprint-linear-list stream object)))))

(set-pprint-dispatch 'cons #'%pprint-lisp-form 0 *print-pprint-dispatch*)

(eval-when (:load-toplevel :execute)
  (vm-register-host-bridge 'copy-pprint-dispatch #'copy-pprint-dispatch)
  (vm-register-host-bridge 'set-pprint-dispatch #'set-pprint-dispatch)
  (vm-register-host-bridge 'get-pprint-dispatch #'get-pprint-dispatch)
  (vm-register-host-bridge 'pprint-indent #'pprint-indent)
  (vm-register-host-bridge 'pprint-newline #'pprint-newline)
  (vm-register-host-bridge 'pprint-tab #'pprint-tab))
