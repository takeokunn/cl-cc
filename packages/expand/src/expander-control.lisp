(in-package :cl-cc/expand)

;; let — destructuring bindings desugar to destructuring-bind chains; plain let expands values
;; FR-623: (let () body) → (progn body)
(progn
  (setf (gethash 'let *expander-head-table*)
        (lambda (form)
          (%expand-let-form form))))

;; flet/labels — expand only function bodies; head symbol tells them apart
;; FR-623: (flet () body) / (labels () body) → (progn body)
(progn
  (setf (gethash 'flet *expander-head-table*)
        (lambda (form)
          (%expand-flet-or-labels 'flet form))))

(progn
  (setf (gethash 'labels *expander-head-table*)
        (lambda (form)
          (%expand-flet-or-labels 'labels form))))

;; FR-585: handler-case :no-error clause
;; (handler-case form (type (v) ...) (:no-error (x) success-body))
;; → (block #:tag
;;     (let ((#:r (handler-case form (type (v) (return-from #:tag ...)))))
;;       success-body-with-x-bound))
(progn
  (setf (gethash 'handler-case *expander-head-table*)
        (lambda (form)
          (%expand-handler-case-form form))))
