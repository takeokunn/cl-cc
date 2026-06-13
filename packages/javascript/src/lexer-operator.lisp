;;;; packages/javascript/src/lexer-operator.lisp — JS operator/punctuation lexer + main tokenizer
;;;;
;;;; lex-js-operator: table-driven dispatch for all JS multi-character operators
;;;; tokenize-js-source: main tokenizer assembling all sub-lexers into a token list
;;;;
;;;; Load order: after lexer.lisp (needs make-js-token, js-*-p predicates,
;;;;             skip-js-whitespace-and-comments, lex-js-string, lex-js-identifier,
;;;;             lex-js-private-ident, js-regex-follows-p).

(in-package :cl-cc/javascript)

;;; Operator lexer

(defun lex-js-operator (source pos)
  "Lex an operator or punctuation token at POS.
Handles all multi-character JS operators.
Returns (values token new-pos)."
  (let ((ch  (char source pos))
        (ch2 (%js-lex-peek-char2 source pos))
        (ch3 (when (< (+ pos 2) (length source)) (char source (+ pos 2)))))
    (macrolet ((tok3 (str type val)
                 `(when (and ch2 ch3
                             (char= ch2 ,(char str 1))
                             (char= ch3 ,(char str 2)))
                    (return-from lex-js-operator
                      (values (make-js-token ,type ,val) (+ pos 3)))))
               (tok2 (str type val)
                 `(when (and ch2 (char= ch2 ,(char str 1)))
                    (return-from lex-js-operator
                      (values (make-js-token ,type ,val) (+ pos 2)))))
               (tok1 (type val)
                 `(return-from lex-js-operator
                    (values (make-js-token ,type ,val) (1+ pos)))))
      (case ch
        (#\(  (tok1 :T-LPAREN "("))
        (#\)  (tok1 :T-RPAREN ")"))
        (#\{  (tok1 :T-LBRACE "{"))
        (#\}  (tok1 :T-RBRACE "}"))
        (#\[  (tok1 :T-LBRACKET "["))
        (#\]  (tok1 :T-RBRACKET "]"))
        (#\;  (tok1 :T-SEMI ";"))
        (#\,  (tok1 :T-COMMA ","))
        (#\@  (tok1 :T-AT "@"))
        (#\~  (tok1 :T-OP "~"))
        (#\:  (tok1 :T-COLON ":"))
        (#\.
         ;; ... ellipsis
         (when (and ch2 ch3 (char= ch2 #\.) (char= ch3 #\.))
           (return-from lex-js-operator
             (values (make-js-token :T-ELLIPSIS "...") (+ pos 3))))
         (tok1 :T-DOT "."))
        (#\?
         ;; ??= null-coalescing assignment
         (when (and ch2 ch3 (char= ch2 #\?) (char= ch3 #\=))
           (return-from lex-js-operator
             (values (make-js-token :T-OP "??=") (+ pos 3))))
         ;; ?. optional chaining
         (tok2 "?." :T-OP "?.")
         ;; ?? null-coalescing
         (tok2 "??" :T-OP "??")
         (tok1 :T-QUESTION "?"))
        (#\=
         ;; === strict equality
         (tok3 "===" :T-OP "===")
         ;; => arrow
         (tok2 "=>" :T-ARROW "=>")
         ;; == equality
         (tok2 "==" :T-OP "==")
         (tok1 :T-OP "="))
        (#\!
         ;; !==
         (tok3 "!==" :T-OP "!==")
         ;; !=
         (tok2 "!=" :T-OP "!=")
         (tok1 :T-OP "!"))
        (#\<
         ;; <<=
         (when (and ch2 ch3 (char= ch2 #\<) (char= ch3 #\=))
           (return-from lex-js-operator
             (values (make-js-token :T-OP "<<=") (+ pos 3))))
         ;; <<
         (tok2 "<<" :T-OP "<<")
         ;; <=
         (tok2 "<=" :T-OP "<=")
         (tok1 :T-OP "<"))
        (#\>
         ;; >>>= unsigned right-shift assign
         (let ((ch4 (when (< (+ pos 3) (length source)) (char source (+ pos 3)))))
           (when (and ch2 ch3 ch4
                      (char= ch2 #\>) (char= ch3 #\>) (char= ch4 #\=))
             (return-from lex-js-operator
               (values (make-js-token :T-OP ">>>=") (+ pos 4)))))
         ;; >>>
         (when (and ch2 ch3 (char= ch2 #\>) (char= ch3 #\>))
           (return-from lex-js-operator
             (values (make-js-token :T-OP ">>>") (+ pos 3))))
         ;; >>=
         (when (and ch2 ch3 (char= ch2 #\>) (char= ch3 #\=))
           (return-from lex-js-operator
             (values (make-js-token :T-OP ">>=") (+ pos 3))))
         ;; >>
         (tok2 ">>" :T-OP ">>")
         ;; >=
         (tok2 ">=" :T-OP ">=")
         (tok1 :T-OP ">"))
        (#\+
         ;; ++
         (tok2 "++" :T-OP "++")
         ;; +=
         (tok2 "+=" :T-OP "+=")
         (tok1 :T-OP "+"))
        (#\-
         ;; --
         (tok2 "--" :T-OP "--")
         ;; -=
         (tok2 "-=" :T-OP "-=")
         (tok1 :T-OP "-"))
        (#\*
         ;; **= exponent assign
         (when (and ch2 ch3 (char= ch2 #\*) (char= ch3 #\=))
           (return-from lex-js-operator
             (values (make-js-token :T-OP "**=") (+ pos 3))))
         ;; **
         (tok2 "**" :T-OP "**")
         ;; *=
         (tok2 "*=" :T-OP "*=")
         (tok1 :T-OP "*"))
        (#\/
         ;; /=
         (tok2 "/=" :T-OP "/=")
         (tok1 :T-OP "/"))
        (#\%
         ;; %=
         (tok2 "%=" :T-OP "%=")
         (tok1 :T-OP "%"))
        (#\&
         ;; &&=
         (when (and ch2 ch3 (char= ch2 #\&) (char= ch3 #\=))
           (return-from lex-js-operator
             (values (make-js-token :T-OP "&&=") (+ pos 3))))
         ;; &&
         (tok2 "&&" :T-OP "&&")
         ;; &=
         (tok2 "&=" :T-OP "&=")
         (tok1 :T-OP "&"))
        (#\|
         ;; ||=
         (when (and ch2 ch3 (char= ch2 #\|) (char= ch3 #\=))
           (return-from lex-js-operator
             (values (make-js-token :T-OP "||=") (+ pos 3))))
         ;; ||
         (tok2 "||" :T-OP "||")
         ;; |=
         (tok2 "|=" :T-OP "|=")
         (tok1 :T-OP "|"))
        (#\^
         ;; ^=
         (tok2 "^=" :T-OP "^=")
         (tok1 :T-OP "^"))
        (otherwise
         (error "JS lex error: unexpected character ~S at position ~D" ch pos))))))

;;; Main tokenizer

(defun tokenize-js-source (source)
  "Tokenize JavaScript SOURCE string into a list of token plists.
Returns a list ending with (:type :T-EOF :value nil).

Handles: identifiers, keywords, numbers, strings, operators, regex literals
(using js-regex-follows-p for disambiguation), template literals, private
identifiers, and hashbang comments at position 0."
  (let ((tokens nil)
        (pos 0)
        (len (length source))
        (prev-token-type nil))
    (loop
      ;; Skip whitespace and comments, with special hashbang handling.
      ;; The skip-js-whitespace-and-comments function handles hashbang at pos=0.
      (setf pos (skip-js-whitespace-and-comments source pos))
      (when (>= pos len)
        (push (make-js-token :T-EOF nil) tokens)
        (return))
      (let ((ch (char source pos)))
        (cond
          ;; String: double-quoted
          ((char= ch #\")
           (multiple-value-bind (str new-pos)
               (lex-js-string source (1+ pos) #\")
             (let ((tok (make-js-token :T-STRING str)))
               (push tok tokens)
               (setf prev-token-type :T-STRING
                     pos new-pos))))
          ;; String: single-quoted
          ((char= ch #\')
           (multiple-value-bind (str new-pos)
               (lex-js-string source (1+ pos) #\')
             (let ((tok (make-js-token :T-STRING str)))
               (push tok tokens)
               (setf prev-token-type :T-STRING
                     pos new-pos))))
          ;; Template literal: backtick
          ((char= ch #\`)
           (cond
             ;; If js-lex-template is available (defined in lexer-template.lisp), use it
             ((fboundp 'js-lex-template)
              ;; js-lex-template returns a SINGLE token (:T-STRING for a simple
              ;; template, or :T-TEMPLATE-PARTS for an interpolated one) — push it
              ;; as one token, not as a list of its plist elements.
              (multiple-value-bind (tok new-pos)
                  (js-lex-template source (1+ pos))
                (push tok tokens)
                (setf prev-token-type (getf tok :type)
                      pos new-pos)))
             ;; Otherwise emit :T-TEMPLATE-START and scan inline to closing backtick
             (t
              (let ((tok (make-js-token :T-TEMPLATE-START "`")))
                (push tok tokens)
                (setf prev-token-type :T-TEMPLATE-START)
                (incf pos)
                ;; Simple inline scan: collect template content until closing backtick
                (let ((buf (make-array 64 :element-type 'character
                                       :fill-pointer 0 :adjustable t)))
                  (loop
                    (when (>= pos len)
                      (error "JS lex error: unterminated template literal"))
                    (let ((tc (char source pos)))
                      (cond
                        ((char= tc #\`)
                         (push (make-js-token :T-STRING (copy-seq buf)) tokens)
                         (setf prev-token-type :T-STRING)
                         (incf pos)
                         (return))
                        ((and (char= tc #\\) (< (1+ pos) len))
                         (vector-push-extend (char source (1+ pos)) buf)
                         (incf pos 2))
                        (t
                         (vector-push-extend tc buf)
                         (incf pos))))))))))
          ;; Number
          ((js-digit-p ch)
           (multiple-value-bind (tok new-pos) (lex-js-number source pos)
             (push tok tokens)
             (setf prev-token-type (getf tok :type)
                   pos new-pos)))
          ;; Identifier or keyword
          ((js-id-start-p ch)
           (multiple-value-bind (tok new-pos) (lex-js-identifier source pos)
             (push tok tokens)
             (setf prev-token-type (getf tok :type)
                   pos new-pos)))
          ;; Private identifier or hashbang
          ((char= ch #\#)
           (cond
             ;; Hashbang #! at non-zero position: treat # as error or skip line
             ((and (< (1+ pos) len)
                   (char= (char source (1+ pos)) #\!))
              ;; Non-zero position hashbang: skip the line
              (setf pos (skip-js-line-comment source (+ pos 2))))
             ;; Private identifier: # followed by identifier start
             ((and (< (1+ pos) len)
                   (js-id-start-p (char source (1+ pos))))
              (multiple-value-bind (tok new-pos) (lex-js-private-ident source (1+ pos))
                (push tok tokens)
                (setf prev-token-type :T-PRIVATE-IDENT
                      pos new-pos)))
             (t
              (error "JS lex error: unexpected # at position ~D" pos))))
          ;; Slash: regex or division
          ((char= ch #\/)
           (cond
             ;; Check if this slash starts a regex
             ((js-regex-follows-p prev-token-type)
              (cond
                ;; If js-lex-regex is available (from lexer-regex.lisp), use it.
                ;; js-lex-regex expects POS to be AFTER the opening '/', so skip it
                ;; with (1+ pos) — otherwise it sees the opening slash as an empty
                ;; pattern's closing slash and reads the pattern body as flags
                ;; ("/ab+c/gi" -> unknown flag 'a').
                ((fboundp 'js-lex-regex)
                 (multiple-value-bind (tok new-pos)
                     (js-lex-regex source (1+ pos))
                   (push tok tokens)
                   (setf prev-token-type :T-REGEX
                         pos new-pos)))
                ;; Otherwise emit a simple :T-OP "/" token
                (t
                 (let ((tok (make-js-token :T-OP "/")))
                   (push tok tokens)
                   (setf prev-token-type :T-OP
                         pos (1+ pos))))))
             ;; Division operator (or /=)
             (t
              (multiple-value-bind (tok new-pos) (lex-js-operator source pos)
                (push tok tokens)
                (setf prev-token-type (getf tok :type)
                      pos new-pos)))))
          ;; Operators and punctuation
          (t
           (multiple-value-bind (tok new-pos) (lex-js-operator source pos)
             (push tok tokens)
             (setf prev-token-type (getf tok :type)
                   pos new-pos))))))
    (nreverse tokens)))
