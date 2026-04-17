(in-package :cl-cc/expand)

;;; ─── CXR Accessor Macros (algorithmic registration) ──────────────────────────
;;; cXXr..cXXXXr forms are registered programmatically by
;;; analysing the sequence of a/d letters between the outer c/r.

(let ((cxr-names '(caar cadr cdar cddr
                   caaar cdaar cadar cddar
                   caadr cdadr caddr cdddr
                   caaaar cadaar caadar caddar
                   cdaaar cddaar cdadar cdddar
                   caaadr cadadr caaddr cadddr
                   cdaadr cddadr cdaddr cddddr)))
  (flet ((expand-cxr (sym arg)
           (let* ((name (symbol-name sym))
                  (letters (subseq name 1 (1- (length name)))))
             (reduce (lambda (acc letter)
                       (if (char-equal letter #\a) `(car ,acc) `(cdr ,acc)))
                     (reverse (coerce letters 'list))
                     :initial-value arg))))
    (dolist (cxr-sym cxr-names)
      (let ((cxr cxr-sym))
        (register-macro cxr
                        (lambda (form env)
                          (declare (ignore env))
                          (expand-cxr cxr (second form))))))))
