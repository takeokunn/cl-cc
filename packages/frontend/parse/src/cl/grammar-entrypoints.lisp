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

;;; ─── Pratt Bridge: CL NUD/LED Tables ────────────────────────────────────────
;;;
;;; These tables bind the generic Pratt engine (pratt.lisp) to CL tokens.
;;; CL is fully prefix — all compound forms are written (op arg...) — so
;;; the LED table is always empty.  The NUD table has a handler for each
;;; atom token type plus LPAREN and QUOTE.

(defvar *cl-nud-table*
  (let ((ht (make-hash-table :test #'eq)))
    (flet ((tok-nud (type)
             (setf (gethash type ht)
                   (lambda (ctx tok)
                     (declare (ignore ctx))
                     (%tok-to-cst tok)))))
      (tok-nud :T-INT)
      (tok-nud :T-FLOAT)
      (tok-nud :T-RATIO)
      (tok-nud :T-STRING)
      (tok-nud :T-CHAR)
      (tok-nud :T-IDENT)
      (tok-nud :T-KEYWORD)
      (tok-nud :T-BOOL-TRUE)
      (tok-nud :T-BOOL-FALSE))
    ;; QUOTE: build a quote-wrapper CST node
    (setf (gethash :T-QUOTE ht)
          (lambda (ctx tok)
            (declare (ignore ctx tok))
            (make-cst-interior :kind :quote :children nil
                               :start-byte 0 :end-byte 0)))
    ;; LPAREN: delegate to a minimal list stub
    (setf (gethash :T-LPAREN ht)
          (lambda (ctx tok)
            (declare (ignore ctx tok))
            (make-cst-interior :kind :list :children nil
                               :start-byte 0 :end-byte 0)))
    ht)
  "NUD dispatch table for the CL Pratt parser bridge (token-type → handler).")

(defvar *cl-led-table*
  (make-hash-table :test #'eq)
  "LED dispatch table for the CL Pratt parser bridge.
Always empty — CL has no infix operators.")

(defun make-cl-pratt-context (tokens source source-file)
  "Create a pratt-context pre-loaded with the CL NUD/LED grammar tables."
  (make-pratt-context
   :tokens      tokens
   :source      (or source "")
   :source-file source-file
   :nud-table   *cl-nud-table*
   :led-table   *cl-led-table*))

;;; ─── Backward Compatibility Wrappers ─────────────────────────────────────────
;;;
;;; NOTE: parse-source and parse-all-forms are NOT redefined here.
;;; The original read-from-string-based versions in cl/parser.lisp remain
;;; active for backward compatibility. Use parse-cl-source directly for
;;; the CST pipeline.
