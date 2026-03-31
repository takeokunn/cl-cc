(in-package :cl-cc)

(defun expand-lambda-list-defaults (params)
  "Expand macro calls in &optional/&key default value positions within PARAMS.
Leaves required params, lambda-list keywords, and supplied-p vars untouched."
  (let (in-extended)
    (mapcar (lambda (p)
               (cond
                 ((member p '(&optional &rest &key &allow-other-keys &aux &body &whole))
                  (setf in-extended t)
                  p)
                 ((and in-extended (consp p))
                  (cond
                    ((and (symbolp (car p)) (or (= (length p) 2) (= (length p) 3)))
                     (destructuring-bind (var initform &optional supplied-p) p
                       (if supplied-p
                           (list var (compiler-macroexpand-all initform) supplied-p)
                           (list var (compiler-macroexpand-all initform)))))
                    ((and (consp (car p)) (>= (length (car p)) 1))
                     (destructuring-bind (varspec initform &optional supplied-p) p
                       (let ((key (first varspec))
                             (var (if (= (length varspec) 1) (first varspec) (second varspec))))
                         (if supplied-p
                             (list (list key var)
                                   (compiler-macroexpand-all initform)
                                   supplied-p)
                             (list (list key var)
                                   (compiler-macroexpand-all initform))))))
                    (t p)))
                 (t p)))
             params)))
