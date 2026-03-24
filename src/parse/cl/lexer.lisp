;;;; frontend/cl/lexer.lisp - Common Lisp Lexer (thin wrapper around CL reader)
;;;;
;;;; Common Lisp uses the built-in READ function as its lexer/parser.
;;;; This module provides token type definitions and a streaming interface
;;;; consistent with the multi-language frontend architecture.

(in-package :cl-cc)

;;; CL Token Types (for symmetry with other language lexers)

(defstruct (cl-token (:conc-name cl-tok-))
  "A token produced by the CL lexer."
  (type nil)    ; :integer, :float, :string, :symbol, :list, :eof
  (value nil)   ; the read value
  (position 0)) ; byte offset in source

;;; CL Tokenizer

(defun tokenize-cl-source (source)
  "Tokenize SOURCE string into a list of CL-TOKEN structs using READ.
   Since CL uses a homoiconic reader, each top-level form is one token."
  (let ((tokens nil)
        (position 0)
        (len (length source)))
    (loop
      ;; Skip whitespace
      (loop while (and (< position len)
                       (member (char source position) '(#\Space #\Tab #\Newline #\Return)))
            do (incf position))
      (when (>= position len)
        (return))
      (let ((start position))
        (multiple-value-bind (form next-pos)
            (handler-case
                (read-from-string source nil :eof :start position)
              (error (e)
                (error "CL tokenizer error at position ~D: ~A" position e)))
          (when (eq form :eof)
            (return))
          (push (make-cl-token
                 :type (etypecase form
                         (integer  :integer)
                         (float    :float)
                         (string   :string)
                         (symbol   :symbol)
                         (cons     :list)
                         (t        :atom))
                 :value form
                 :position start)
                tokens)
          (setf position next-pos))))
    (nreverse tokens)))
