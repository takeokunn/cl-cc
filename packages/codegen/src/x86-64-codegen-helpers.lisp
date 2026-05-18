(in-package :cl-cc/codegen)

(defun populate-size-table (specs)
  "Build an eq-hash-table from declarative size specifications."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (entry specs ht)
      (destructuring-bind (types size) entry
        (if (consp types)
            (dolist (tp types) (setf (gethash tp ht) size))
            (setf (gethash types ht) size))))))
