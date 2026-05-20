(in-package :cl-cc/runtime)
(defvar *rt-dl-enabled* nil)
(defun rt-deadlock-detect (&optional starting) (declare (ignore starting)) nil)
(defun rt-deadlock-init () (setf *rt-dl-enabled* nil) t)
