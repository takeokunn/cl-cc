(in-package :cl-cc/expand)

;; let — destructuring bindings desugar to destructuring-bind chains; plain let expands values
;; FR-623: (let () body) → (progn body)
(define-expander-for let (form)
  (cond
    ((and (>= (length form) 2) (listp (second form)) (null (second form)))
     (compiler-macroexpand-all (cons 'progn (cddr form))))
    ((and (>= (length form) 2) (listp (second form))
          (some (lambda (b) (and (consp b) (>= (length b) 2) (consp (first b))))
                (second form)))
     (let ((simple nil) (destructuring nil))
       (dolist (b (second form))
         (if (and (consp b) (>= (length b) 2) (consp (first b)))
             (push b destructuring)
             (push b simple)))
       (setf simple (nreverse simple) destructuring (nreverse destructuring))
       (let ((inner (if simple
                        (list* 'let simple (cddr form))
                        (cons 'progn (cddr form)))))
         (dolist (d (reverse destructuring))
           (setf inner (list 'destructuring-bind (first d) (second d) inner)))
         (compiler-macroexpand-all inner))))
    ((and (>= (length form) 2) (listp (second form)))
     (list* 'let
            (mapcar #'expand-let-binding (second form))
            (mapcar #'compiler-macroexpand-all (cddr form))))
    (t (cons 'let (mapcar #'compiler-macroexpand-all (cdr form))))))

;; flet/labels — expand only function bodies; head symbol tells them apart
;; FR-623: (flet () body) / (labels () body) → (progn body)
(defun %expand-flet-or-labels (head form)
  (if (and (>= (length form) 3) (listp (second form)))
      (if (null (second form))
          (compiler-macroexpand-all (cons 'progn (cddr form)))
          (list* head
                 (mapcar #'expand-flet-labels-binding (second form))
                 (mapcar #'compiler-macroexpand-all (cddr form))))
      (cons head (mapcar #'compiler-macroexpand-all (cdr form)))))

(define-expander-for flet   (form) (%expand-flet-or-labels 'flet   form))
(define-expander-for labels (form) (%expand-flet-or-labels 'labels form))

;; FR-585: handler-case :no-error clause
;; (handler-case form (type (v) ...) (:no-error (x) success-body))
;; → (block #:tag
;;     (let ((#:r (handler-case form (type (v) (return-from #:tag ...)))))
;;       success-body-with-x-bound))
(define-expander-for handler-case (form)
  (let* ((protected (second form))
         (all-clauses (cddr form))
         (no-error-clause (find :no-error all-clauses :key #'car))
         (error-clauses (remove :no-error all-clauses :key #'car)))
    (if no-error-clause
        (let ((tag (gensym "NO-ERROR-"))
              (ne-vars (second no-error-clause))
              (ne-body (cddr no-error-clause)))
          (compiler-macroexpand-all
           (let ((result-var (if (and ne-vars (car ne-vars)) (car ne-vars) (gensym "R-"))))
             (list 'block tag
                   (list 'let
                         (list
                          (list result-var
                                (cons 'handler-case
                                      (cons protected
                                            (mapcar (lambda (c)
                                                      (list (first c) (second c)
                                                            (list 'return-from tag
                                                                  (cons 'progn (cddr c)))))
                                                    error-clauses)))))
                         (if ne-body
                             (cons 'progn ne-body)
                             nil))))))
        ;; No :no-error clause — recurse into subforms normally
        (cons 'handler-case (mapcar #'compiler-macroexpand-all (cdr form))))))
