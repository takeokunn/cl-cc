(in-package :cl-cc/parse)

;;; ─── Top-Level Entry Points ──────────────────────────────────────────────────

(defun parse-cl-source (source &optional source-file)
  "Parse CL SOURCE string into CST nodes using the hand-written lexer and
   recursive descent parser. Returns (values cst-list diagnostics)."
  (let* ((tokens (lex-all source))
         (ts (make-token-stream :tokens tokens
                                :source source
                                :source-file source-file))
         (forms nil))
    (loop until (ts-at-end-p ts)
          do (let ((form (parse-cl-form ts)))
               (if form
                   (push form forms)
                   (return))))
    (values (nreverse forms)
            (nreverse (token-stream-diagnostics ts)))))

;;; ─── Public CL grammar entry points ──────────────────────────────────────────
;;;
;;; parse-cl-source is the CST-producing entry point for the hand-written CL
;;; grammar pipeline. Higher-level helpers build on top of this interface.
