;;;; packages/javascript/src/runtime-class.lisp — JS Class/OOP, nullish coalesce, misc
;;;;
;;;; Class instantiation, private fields, and miscellaneous JS operators.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Class / OOP
;;; -----------------------------------------------------------------------

(defun %js-new (constructor &optional (args nil))
  "Instantiate JS class.  CONSTRUCTOR is a HT with __new__ or __prototype__."
  (cond
    ((and (%js-ht-p constructor) (gethash "__new__" constructor))
     (apply (gethash "__new__" constructor) args))
    ((functionp constructor)
     (apply constructor args))
    (t
     (let ((obj (%js-make-ht)))
       (when (%js-ht-p constructor)
         (let ((proto (gethash "__prototype__" constructor)))
           (when proto (setf (gethash "__proto__" obj) proto)))
         (let ((ctor (gethash "__constructor__" constructor)))
           (when ctor
             (apply ctor obj args))))
       obj))))

(defun %js-class-private-field-get (obj field-name)
  "Read a private field from OBJ."
  (let ((privates (and (%js-ht-p obj) (gethash "__private__" obj))))
    (if (and privates (%js-ht-p privates))
        (multiple-value-bind (v f) (gethash field-name privates)
          (if f v +js-undefined+))
        +js-undefined+)))

(defun %js-class-private-field-set (obj field-name value)
  "Write a private field on OBJ."
  (when (%js-ht-p obj)
    (let ((privates (gethash "__private__" obj)))
      (unless (and privates (%js-ht-p privates))
        (setf privates (%js-make-ht)
              (gethash "__private__" obj) privates))
      (setf (gethash field-name privates) value)))
  value)

(defun %js-has-private-field (obj field-name)
  "True if OBJ has the named private field."
  (let ((privates (and (%js-ht-p obj) (gethash "__private__" obj))))
    (if (and privates (%js-ht-p privates))
        (nth-value 1 (gethash field-name privates))
        nil)))

;;; -----------------------------------------------------------------------
;;;  Nullish coalesce
;;; -----------------------------------------------------------------------

(defun %js-nullish-coalesce (a b)
  "JS ?? operator."
  (if (%js-not-nullish a) a b))

;;; -----------------------------------------------------------------------
;;;  Misc
;;; -----------------------------------------------------------------------

(defun %js-void (x)
  "JS void operator."
  (declare (ignore x))
  +js-undefined+)

(defun %js-debugger ()
  "JS debugger statement — no-op."
  +js-undefined+)

(defun %js-import (module-name &optional with-opts)
  "Dynamic import (stub — returns empty namespace object)."
  (declare (ignore with-opts))
  (%js-promise-resolve
   (%js-make-object "default" +js-undefined+ "__moduleName__" module-name)))

(defun %js-export (kind value)
  "Mark a value as exported (stub — returns value)."
  (declare (ignore kind))
  value)

(defun %js-spread (iterable)
  "Expand iterable into a list (for use with apply)."
  (cond
    ((%js-vec-p iterable)
     (loop for i below (length iterable) collect (aref iterable i)))
    ((stringp iterable)
     (loop for ch across iterable collect (string ch)))
    (t
     (%js-iterator-to-array
      (if (%js-ht-p iterable)
          (let ((iter-fn (gethash "@@iterator" iterable)))
            (if iter-fn (funcall iter-fn) iterable))
          iterable)))))
