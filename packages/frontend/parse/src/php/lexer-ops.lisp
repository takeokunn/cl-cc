;;;; packages/frontend/parse/src/php/lexer-ops.lisp — PHP Lexer: Operator Dispatch + Main Tokenizer
;;;;
;;;; Contains the operator/punctuation lexer, the PHP opening-tag skipper,
;;;; and the main tokenize-php-source entry point.
;;;; Character classification, string/number/identifier/variable sub-lexers
;;;; are in lexer.lisp (loads before).
;;;;
;;;; Load order: after lexer.lisp.

(in-package :cl-cc/parse)

;;; Operator / punctuation lexer

(defun lex-operator (source pos)
  "Lex an operator or punctuation at POS. Returns (values token new-pos)."
  (let ((ch  (char source pos))
        (ch2 (php-lex-peek2 source pos))    ; char at pos+1
        (ch3 (when (< (+ pos 2) (length source)) (char source (+ pos 2)))))
    (macrolet ((tok3 (str type val)
                 `(when (and ch2 ch3
                             (string= (string ch2) (subseq ,str 1 2))
                             (string= (string ch3) (subseq ,str 2 3)))
                    (return-from lex-operator
                      (values (make-php-token ,type ,val) (+ pos 3)))))
               (tok2 (str type val)
                 `(when (and ch2 (string= (string ch2) (subseq ,str 1 2)))
                    (return-from lex-operator
                      (values (make-php-token ,type ,val) (+ pos 2)))))
               (tok1 (type val)
                 `(return-from lex-operator
                    (values (make-php-token ,type ,val) (1+ pos)))))
      (case ch
        (#\(  (tok1 :T-LPAREN "("))
        (#\)  (tok1 :T-RPAREN ")"))
        (#\{  (tok1 :T-LBRACE "{"))
        (#\}  (tok1 :T-RBRACE "}"))
        (#\[  (tok1 :T-LBRACKET "["))
        (#\]  (tok1 :T-RBRACKET "]"))
        (#\;  (tok1 :T-SEMI ";"))
        (#\,  (tok1 :T-COMMA ","))
        (#\~  (tok1 :T-OP "~"))
        (#\@  (tok1 :T-OP "@"))
        (#\%  (tok1 :T-OP "%"))
        (#\^  (tok1 :T-OP "^"))
        (#\&
         (tok2 "&&" :T-OP "&&")
         (tok1 :T-OP "&"))
        (#\|
         (tok2 "||" :T-OP "||")
         (tok1 :T-OP "|"))
        (#\!
         (tok3 "!==" :T-OP "!==")
         (tok2 "!=" :T-OP "!=")
         (tok1 :T-OP "!"))
        (#\=
         (tok3 "===" :T-OP "===")
         (tok2 "==" :T-OP "==")
         (tok2 "=>" :T-OP "=>")
         (tok1 :T-OP "="))
        (#\<
         (tok3 "<=>" :T-OP "<=>")
         (tok2 "<=" :T-OP "<=")
         (tok2 "<<" :T-OP "<<")
         (tok1 :T-OP "<"))
        (#\>
         (tok2 ">=" :T-OP ">=")
         (tok2 ">>" :T-OP ">>")
         (tok1 :T-OP ">"))
        (#\+
         (tok2 "++" :T-OP "++")
         (tok2 "+=" :T-OP "+=")
         (tok1 :T-OP "+"))
        (#\-
         ;; ?-> nullsafe arrow is handled in ? branch; -> here
         (tok2 "->" :T-ARROW "->")
         (tok2 "--" :T-OP "--")
         (tok2 "-=" :T-OP "-=")
         (tok1 :T-OP "-"))
        (#\*
         (tok2 "**" :T-OP "**")
         (tok2 "*=" :T-OP "*=")
         (tok1 :T-OP "*"))
        (#\/
         (tok2 "/=" :T-OP "/=")
         (tok1 :T-OP "/"))
        (#\.
         (tok2 ".=" :T-OP ".=")
         (tok1 :T-OP "."))
        (#\:
         (tok2 "::" :T-DOUBLE-COLON "::")
         (tok1 :T-COLON ":"))
        (#\?
         ;; ?-> nullsafe arrow
         (when (and ch2 (char= ch2 #\-)
                    ch3 (char= ch3 #\>))
           (return-from lex-operator
             (values (make-php-token :T-NULLSAFE-ARROW "?->") (+ pos 3))))
         ;; ?? null coalescing
         (tok2 "??" :T-OP "??")
         ;; ? as nullable marker
         (tok1 :T-NULLABLE "?"))
        (otherwise
         (error "PHP lex error: unexpected character ~S at position ~D" ch pos))))))

;;; PHP opening tag skipper

(defun skip-php-open-tag (source pos)
  "Skip <?php or <? opening tag. Returns position after the tag."
  (let ((len (length source)))
    (cond
      ;; <?php
      ((and (< (+ pos 4) len)
            (string= source "<?php" :start1 pos :end1 (+ pos 5)))
       ;; Also skip trailing whitespace after <?php
       (skip-whitespace-and-comments source (+ pos 5)))
      ;; <?
      ((and (< (1+ pos) len)
            (string= source "<?" :start1 pos :end1 (+ pos 2)))
       (skip-whitespace-and-comments source (+ pos 2)))
      (t pos))))

;;; Main tokenizer

(defun tokenize-php-source (source)
  "Tokenize PHP SOURCE string into a list of token plists.
   Returns a list ending with (:type :T-EOF :value nil).

   Token format: (:type :T-XXX :value val)"
  (let ((tokens nil)
        (pos 0)
        (len (length source)))
    ;; Skip leading whitespace
    (loop while (and (< pos len) (php-whitespace-p (char source pos)))
          do (incf pos))
    ;; Skip <?php opening tag if present
    (setf pos (skip-php-open-tag source pos))
    ;; Main tokenization loop
    (loop
      (setf pos (skip-whitespace-and-comments source pos))
      (when (>= pos len)
        (push (make-php-token :T-EOF nil) tokens)
        (return))
      (let ((ch (char source pos)))
        (cond
          ;; Variable: $name
          ((char= ch #\$)
           (multiple-value-bind (tok new-pos) (lex-variable source (1+ pos))
             (push tok tokens)
             (setf pos new-pos)))
          ;; String: double-quoted
          ((char= ch #\")
           (multiple-value-bind (str new-pos) (lex-double-quoted-string source (1+ pos))
             (push (make-php-token :T-STRING str) tokens)
             (setf pos new-pos)))
          ;; String: single-quoted
          ((char= ch #\')
           (multiple-value-bind (str new-pos) (lex-single-quoted-string source (1+ pos))
             (push (make-php-token :T-STRING str) tokens)
             (setf pos new-pos)))
          ;; Number
          ((php-digit-p ch)
           (multiple-value-bind (tok new-pos) (lex-number source pos)
             (push tok tokens)
             (setf pos new-pos)))
          ;; Identifier or keyword
          ((php-alpha-p ch)
           (multiple-value-bind (tok new-pos) (lex-identifier source pos)
             (push tok tokens)
             (setf pos new-pos)))
          ;; Heredoc <<< (minimal: skip to end marker)
          ;; Not fully supported; signal an informative error
          ((and (char= ch #\<)
                (< (+ pos 2) len)
                (char= (char source (1+ pos)) #\<)
                (char= (char source (+ pos 2)) #\<))
           (error "PHP lex error: heredoc syntax (<<<) is not supported"))
          ;; Operators, punctuation
          (t
           (multiple-value-bind (tok new-pos) (lex-operator source pos)
             (push tok tokens)
             (setf pos new-pos))))))
    (nreverse tokens)))
