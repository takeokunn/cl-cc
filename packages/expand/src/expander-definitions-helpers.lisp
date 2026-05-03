(in-package :cl-cc/expand)

(defun expand-lambda-list-defaults (params)
  "Expand macro calls in &optional/&key default value positions within PARAMS.
Leaves required params, lambda-list keywords, and supplied-p vars untouched."
  (let ((in-extended nil))
    (mapcar (lambda (p)
              (cond
                ((%list-contains-eq p '(&optional &rest &key &allow-other-keys &aux &body &whole))
                 (setf in-extended t)
                 p)
                ((and in-extended (consp p))
                 (let* ((head (car p))
                        (initform (cadr p))
                        (supplied-p (caddr p))
                        (expanded-init (and (consp (cdr p))
                                            (compiler-macroexpand-all initform))))
                   (cond
                     ((and (symbolp head) (or (= (length p) 2) (= (length p) 3)))
                      (if supplied-p
                          (list head expanded-init supplied-p)
                          (list head expanded-init)))
                     ((and (consp head) (>= (length head) 1))
                      (let ((key (first head))
                            (var (if (= (length head) 1) (first head) (second head))))
                        (if supplied-p
                            (list (list key var) expanded-init supplied-p)
                            (list (list key var) expanded-init))))
                     (t p))))
                (t p)))
            params)))
