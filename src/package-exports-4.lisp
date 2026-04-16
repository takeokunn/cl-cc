(in-package :cl-cc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (name '(
           ;; emit symbols (calling-convention, regalloc, native codegen) moved to
           ;; :cl-cc/emit — see cl-cc-emit.asd (Phase 2)
           ;; *grammar-rules*, def-grammar-rule, query-grammar, clear-grammar-rules,
           ;; parse-combinator, parse-ok-p, parse-with-grammar, tokenize-php-source,
           ;; parse-php-source, parse-source-for-language
           ;; moved to :cl-cc/parse — see cl-cc-parse.asd (Phase 2)
           "ast-int-p"
           "ast-print-p"
           "ast-let-p"
           "ast-setq-p"
           "ast-if-p"
           "ast-defun-p"
           "ast-quote-p"
           "ast-defclass-p"
           "ast-call-p"
           "ast-binop-p"
           ;; ir-* symbols extracted to :cl-cc/ir — see cl-cc-ir.asd (Phase 2)
           ;; mir/target symbols extracted to :cl-cc/mir — see cl-cc-mir.asd (Phase 2)
           ;; optimize exports moved to src/optimize/package.lisp — see cl-cc-optimize.asd (Phase 2)
           ))
    (export (intern (string-upcase name) :cl-cc) :cl-cc)))
