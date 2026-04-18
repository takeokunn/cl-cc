(in-package :cl-cc/expand)

;; FR-301: normalise 1-arg rounding ops to 2-arg form with divisor 1.
;; Registered via dolist for all *rounding-ops* at once (data-driven).
(dolist (op *rounding-ops*)
  (setf (gethash op *expander-head-table*)
        (lambda (form)
          (if (= (length form) 2)
              (compiler-macroexpand-all (list (car form) (second form) 1))
              (list (car form)
                    (compiler-macroexpand-all (second form))
                    (compiler-macroexpand-all (third form)))))))

;;; ── FR-626: error/warn format-string desugaring ──────────────────────────
;;;
;;; (error fmt arg ...) → (error (format nil fmt arg ...)) when 2+ args.
;;; Single-arg form passes through to builtin dispatch unchanged.
;; Only desugar when datum is a string and extra format args are present.
;; Handler-case clauses (error (var) body) have a list as second element —
;; those must pass through to the parser unchanged.
(dolist (op '(error warn))
  (setf (gethash op *expander-head-table*)
        (lambda (form)
          (if (and (> (length form) 2) (stringp (second form)))
              ;; (error "fmt" arg...) → (error (format nil "fmt" arg...))
              (compiler-macroexpand-all
               (list (car form)
                     (cons 'format (cons nil (cdr form)))))
              ;; 1-arg form or handler-case clause: pass through unchanged
              (mapcar #'compiler-macroexpand-all form)))))
