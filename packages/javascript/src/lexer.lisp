;;;; packages/javascript/src/lexer.lisp - ES2026 JavaScript Lexer
;;;;
;;;; Tokenizes JavaScript source code into a list of token plists.
;;;; Token format: (:type :T-XXX :value val)
;;;;
;;;; Token types:
;;;;   :T-EOF            - end of input
;;;;   :T-NUMBER         - numeric literal (value = number)
;;;;   :T-STRING         - string literal (value = string content)
;;;;   :T-BIGINT         - BigInt literal (value = integer)
;;;;   :T-REGEX          - regular expression (value = string)
;;;;   :T-IDENT          - identifier (value = string)
;;;;   :T-PRIVATE-IDENT  - private field #name (value = string)
;;;;   :T-TEMPLATE-START - opening backtick
;;;;   :T-OP             - operator (value = string)
;;;;   :T-ARROW          - => (value = "=>")
;;;;   :T-AT             - @ for decorators
;;;;   :T-LPAREN         - (
;;;;   :T-RPAREN         - )
;;;;   :T-LBRACE         - {
;;;;   :T-RBRACE         - }
;;;;   :T-LBRACKET       - [
;;;;   :T-RBRACKET       - ]
;;;;   :T-SEMI           - ;
;;;;   :T-COMMA          - ,
;;;;   :T-DOT            - .
;;;;   :T-ELLIPSIS       - ...
;;;;   :T-COLON          - :
;;;;   :T-QUESTION       - ?
;;;;   :T-BREAK ... :T-USING  - keyword types
;;;;   :T-TRUE :T-FALSE :T-NULL :T-UNDEFINED - literal keyword types

(in-package :cl-cc/javascript)

;;; JavaScript keyword hash table

(defvar *js-keywords*
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair '(("break"      . :T-BREAK)
                    ("case"       . :T-CASE)
                    ("catch"      . :T-CATCH)
                    ("class"      . :T-CLASS)
                    ("const"      . :T-CONST)
                    ("continue"   . :T-CONTINUE)
                    ("debugger"   . :T-DEBUGGER)
                    ("default"    . :T-DEFAULT)
                    ("delete"     . :T-DELETE)
                    ("do"         . :T-DO)
                    ("else"       . :T-ELSE)
                    ("export"     . :T-EXPORT)
                    ("extends"    . :T-EXTENDS)
                    ("finally"    . :T-FINALLY)
                    ("for"        . :T-FOR)
                    ("function"   . :T-FUNCTION)
                    ("if"         . :T-IF)
                    ("import"     . :T-IMPORT)
                    ("in"         . :T-IN)
                    ("instanceof" . :T-INSTANCEOF)
                    ("let"        . :T-LET)
                    ("new"        . :T-NEW)
                    ("of"         . :T-OF)
                    ("return"     . :T-RETURN)
                    ("static"     . :T-STATIC)
                    ("super"      . :T-SUPER)
                    ("switch"     . :T-SWITCH)
                    ("this"       . :T-THIS)
                    ("throw"      . :T-THROW)
                    ("try"        . :T-TRY)
                    ("typeof"     . :T-TYPEOF)
                    ("var"        . :T-VAR)
                    ("void"       . :T-VOID)
                    ("while"      . :T-WHILE)
                    ("with"       . :T-WITH)
                    ("yield"      . :T-YIELD)
                    ("async"      . :T-ASYNC)
                    ("await"      . :T-AWAIT)
                    ("from"       . :T-FROM)
                    ("as"         . :T-AS)
                    ("get"        . :T-GET)
                    ("set"        . :T-SET)
                    ("target"     . :T-TARGET)
                    ("meta"       . :T-META)
                    ("using"      . :T-USING)
                    ("true"       . :T-TRUE)
                    ("false"      . :T-FALSE)
                    ("null"       . :T-NULL)
                    ("undefined"  . :T-UNDEFINED)))
      (setf (gethash (car pair) ht) (cdr pair)))
    ht)
  "Hash table mapping JS keyword strings to token type keywords.")

;;; Token constructor

(defun make-js-token (type value)
  "Construct a JavaScript token plist."
  (list :type type :value value))

;;; Character classification helpers

(defun js-digit-p (ch)
  "Return T if CH is a decimal digit 0-9."
  (and ch (char<= #\0 ch #\9)))

(defun js-hex-digit-p (ch)
  "Return T if CH is a hexadecimal digit 0-9 a-f A-F."
  (and ch (or (char<= #\0 ch #\9)
              (char<= #\a ch #\f)
              (char<= #\A ch #\F))))

(defun js-alpha-p (ch)
  "Return T if CH is a letter, underscore, or dollar sign."
  (and ch (or (char<= #\a ch #\z)
              (char<= #\A ch #\Z)
              (char= ch #\_)
              (char= ch #\$))))

(defun js-alnum-p (ch)
  "Return T if CH is alphanumeric, underscore, or dollar sign."
  (or (js-alpha-p ch) (js-digit-p ch)))

(defun js-id-start-p (ch)
  "Return T if CH is a valid JS identifier start character."
  (js-alpha-p ch))

(defun js-id-continue-p (ch)
  "Return T if CH is a valid JS identifier continuation character."
  (js-alnum-p ch))

(defun js-whitespace-p (ch)
  "Return T if CH is a JS whitespace character."
  (and ch (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=)))

;;; Peek helpers

(defun %js-lex-peek-char (source pos)
  "Return character at POS in SOURCE, or NIL if out of bounds."
  (when (< pos (length source))
    (char source pos)))

(defun %js-lex-peek-char2 (source pos)
  "Return character at POS+1 in SOURCE, or NIL if out of bounds."
  (when (< (1+ pos) (length source))
    (char source (1+ pos))))

;;; Comment skipping

(defun skip-js-line-comment (source pos)
  "Skip from POS to end of line. Returns new pos after the newline."
  (loop while (and (< pos (length source))
                   (not (char= (char source pos) #\Newline)))
        do (incf pos))
  ;; consume the newline if present
  (when (and (< pos (length source))
             (char= (char source pos) #\Newline))
    (incf pos))
  pos)

(defun skip-js-block-comment (source pos)
  "Skip a /* ... */ block comment. POS is after /* was consumed.
Returns new pos after the closing */."
  (loop
    (when (>= pos (length source))
      (error "JS lex error: unterminated block comment"))
    (let ((ch (char source pos)))
      (cond
        ((and (char= ch #\*)
              (< (1+ pos) (length source))
              (char= (char source (1+ pos)) #\/))
         (return (+ pos 2)))
        (t (incf pos))))))

(defun skip-js-whitespace-and-comments (source pos)
  "Skip whitespace, // line comments, /* */ block comments, and #! hashbang.
Returns new pos."
  (loop
    (cond
      ;; End of source
      ((>= pos (length source))
       (return pos))
      ;; Whitespace
      ((js-whitespace-p (char source pos))
       (incf pos))
      ;; Hashbang #! only at start of source (position 0 effectively = line 0)
      ((and (char= (char source pos) #\#)
            (< (1+ pos) (length source))
            (char= (char source (1+ pos)) #\!)
            ;; Only skip if at the very beginning (no non-whitespace seen before)
            (= pos 0))
       (setf pos (skip-js-line-comment source (+ pos 2))))
      ;; Line comment //
      ((and (char= (char source pos) #\/)
            (< (1+ pos) (length source))
            (char= (char source (1+ pos)) #\/))
       (setf pos (skip-js-line-comment source (+ pos 2))))
      ;; Block comment /* */
      ((and (char= (char source pos) #\/)
            (< (1+ pos) (length source))
            (char= (char source (1+ pos)) #\*))
       (setf pos (skip-js-block-comment source (+ pos 2))))
      ;; Not whitespace/comment
      (t (return pos)))))

;;; String literal lexer

(defun lex-js-string (source pos quote-char)
  "Lex a quoted string starting AFTER the opening quote character.
QUOTE-CHAR is either #\\' or #\\\".
Handles escape sequences including \\uXXXX, \\u{XXXXXX}, \\xXX.
Returns (values string-content new-pos)."
  (let ((buf (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (when (>= pos (length source))
        (error "JS lex error: unterminated string literal"))
      (let ((ch (char source pos)))
        (cond
          ;; Closing quote
          ((char= ch quote-char)
           (return (values (copy-seq buf) (1+ pos))))
          ;; Newline in string (not allowed in non-template strings)
          ((or (char= ch #\Newline) (char= ch #\Return))
           (error "JS lex error: unterminated string literal (newline)"))
          ;; Escape sequence
          ((char= ch #\\)
           (incf pos)
           (when (>= pos (length source))
             (error "JS lex error: trailing backslash in string"))
           (let ((esc (char source pos)))
             (incf pos)
             (cond
               ;; Unicode escape \uXXXX or \u{XXXXXX}
               ((char= esc #\u)
                (cond
                  ;; \u{hex-digits} — ES2015 code point escape
                  ((and (< pos (length source)) (char= (char source pos) #\{))
                   (incf pos)
                   (let ((hex-start pos))
                     (loop while (and (< pos (length source))
                                      (js-hex-digit-p (char source pos)))
                           do (incf pos))
                     (unless (and (< pos (length source))
                                  (char= (char source pos) #\}))
                       (error "JS lex error: expected } in \\u{} escape"))
                     (let* ((hex-str (subseq source hex-start pos))
                            (code-point (parse-integer hex-str :radix 16)))
                       (incf pos) ; consume }
                       (vector-push-extend (code-char code-point) buf))))
                  ;; \uXXXX — 4 hex digits
                  (t
                   (when (> (+ pos 4) (length source))
                     (error "JS lex error: incomplete \\uXXXX escape"))
                   (let* ((hex-str (subseq source pos (+ pos 4)))
                          (code-point (parse-integer hex-str :radix 16)))
                     (incf pos 4)
                     (vector-push-extend (code-char code-point) buf)))))
               ;; Hex escape \xXX
               ((char= esc #\x)
                (when (> (+ pos 2) (length source))
                  (error "JS lex error: incomplete \\xXX escape"))
                (let* ((hex-str (subseq source pos (+ pos 2)))
                       (code-point (parse-integer hex-str :radix 16)))
                  (incf pos 2)
                  (vector-push-extend (code-char code-point) buf)))
               ;; Standard single-char escapes
               (t
                (vector-push-extend
                 (case esc
                   (#\n  #\Newline)
                   (#\r  #\Return)
                   (#\t  #\Tab)
                   (#\b  #\Backspace)
                   (#\f  #\Page)
                   (#\v  (code-char 11))    ; vertical tab
                   (#\0  #\Null)
                   (#\\  #\\)
                   (#\'  #\')
                   (#\"  #\")
                   (otherwise esc))
                 buf)))))
          ;; Normal character
          (t
           (vector-push-extend ch buf)
           (incf pos)))))))

;;; Number literal lexer

(defun %lex-js-hex-digits (source pos)
  "Read hex digits (with optional _ separators) from POS. Returns (values string new-pos)."
  (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t))
        (start pos))
    (loop while (and (< pos (length source))
                     (let ((c (char source pos)))
                       (or (js-hex-digit-p c) (char= c #\_))))
          do (let ((c (char source pos)))
               (unless (char= c #\_)
                 (vector-push-extend c buf))
               (incf pos)))
    (when (= pos start)
      (error "JS lex error: expected hex digits"))
    (values (copy-seq buf) pos)))

(defun %lex-js-binary-digits (source pos)
  "Read binary digits (with optional _ separators) from POS. Returns (values string new-pos)."
  (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t))
        (start pos))
    (loop while (and (< pos (length source))
                     (let ((c (char source pos)))
                       (or (char= c #\0) (char= c #\1) (char= c #\_))))
          do (let ((c (char source pos)))
               (unless (char= c #\_)
                 (vector-push-extend c buf))
               (incf pos)))
    (when (= pos start)
      (error "JS lex error: expected binary digits"))
    (values (copy-seq buf) pos)))

(defun %lex-js-octal-digits (source pos)
  "Read octal digits (with optional _ separators) from POS. Returns (values string new-pos)."
  (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t))
        (start pos))
    (loop while (and (< pos (length source))
                     (let ((c (char source pos)))
                       (or (char<= #\0 c #\7) (char= c #\_))))
          do (let ((c (char source pos)))
               (unless (char= c #\_)
                 (vector-push-extend c buf))
               (incf pos)))
    (when (= pos start)
      (error "JS lex error: expected octal digits"))
    (values (copy-seq buf) pos)))

(defun %lex-js-decimal-digits (source pos)
  "Read decimal digits (with optional _ separators) from POS. Returns (values string new-pos)."
  (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop while (and (< pos (length source))
                     (let ((c (char source pos)))
                       (or (js-digit-p c) (char= c #\_))))
          do (let ((c (char source pos)))
               (unless (char= c #\_)
                 (vector-push-extend c buf))
               (incf pos)))
    (values (copy-seq buf) pos)))

(defun lex-js-number (source pos)
  "Lex a numeric literal starting at POS.
Handles decimal, hex (0x), octal (0o), binary (0b), BigInt (n suffix),
and numeric separators (_). Returns (values token new-pos)."
  (let ((ch (char source pos)))
    (cond
      ;; Hex: 0x or 0X
      ((and (char= ch #\0)
            (< (1+ pos) (length source))
            (member (char source (1+ pos)) '(#\x #\X) :test #'char=))
       (multiple-value-bind (digits new-pos)
           (%lex-js-hex-digits source (+ pos 2))
         (let ((bigint-p (and (< new-pos (length source))
                              (char= (char source new-pos) #\n))))
           (when bigint-p (incf new-pos))
           (let ((val (parse-integer digits :radix 16)))
             (values (make-js-token (if bigint-p :T-BIGINT :T-NUMBER) val)
                     new-pos)))))
      ;; Octal: 0o or 0O
      ((and (char= ch #\0)
            (< (1+ pos) (length source))
            (member (char source (1+ pos)) '(#\o #\O) :test #'char=))
       (multiple-value-bind (digits new-pos)
           (%lex-js-octal-digits source (+ pos 2))
         (let ((bigint-p (and (< new-pos (length source))
                              (char= (char source new-pos) #\n))))
           (when bigint-p (incf new-pos))
           (let ((val (parse-integer digits :radix 8)))
             (values (make-js-token (if bigint-p :T-BIGINT :T-NUMBER) val)
                     new-pos)))))
      ;; Binary: 0b or 0B
      ((and (char= ch #\0)
            (< (1+ pos) (length source))
            (member (char source (1+ pos)) '(#\b #\B) :test #'char=))
       (multiple-value-bind (digits new-pos)
           (%lex-js-binary-digits source (+ pos 2))
         (let ((bigint-p (and (< new-pos (length source))
                              (char= (char source new-pos) #\n))))
           (when bigint-p (incf new-pos))
           (let ((val (parse-integer digits :radix 2)))
             (values (make-js-token (if bigint-p :T-BIGINT :T-NUMBER) val)
                     new-pos)))))
      ;; Decimal (integer or float)
      (t
       (multiple-value-bind (int-digits pos2)
           (%lex-js-decimal-digits source pos)
         (let ((is-float nil)
               (num-str int-digits))
           ;; Fractional part
           (when (and (< pos2 (length source))
                      (char= (char source pos2) #\.)
                      (or (and (< (1+ pos2) (length source))
                               (js-digit-p (char source (1+ pos2))))
                          ;; e.g. "1." is valid float too
                          (not (and (< (1+ pos2) (length source))
                                    (js-id-start-p (char source (1+ pos2)))))))
             (setf is-float t)
             (incf pos2)
             (multiple-value-bind (frac-digits pos3)
                 (%lex-js-decimal-digits source pos2)
               (setf num-str (concatenate 'string num-str "." frac-digits))
               (setf pos2 pos3)))
           ;; Exponent part
           (when (and (< pos2 (length source))
                      (member (char source pos2) '(#\e #\E) :test #'char=))
             (setf is-float t)
             (let ((exp-str "e"))
               (incf pos2)
               (when (and (< pos2 (length source))
                          (member (char source pos2) '(#\+ #\-) :test #'char=))
                 (setf exp-str (concatenate 'string exp-str (string (char source pos2))))
                 (incf pos2))
               (multiple-value-bind (exp-digits pos3)
                   (%lex-js-decimal-digits source pos2)
                 (setf num-str (concatenate 'string num-str exp-str exp-digits))
                 (setf pos2 pos3))))
           ;; BigInt suffix n (only on integer, not float)
           (when (and (not is-float)
                      (< pos2 (length source))
                      (char= (char source pos2) #\n))
             (incf pos2)
             (return-from lex-js-number
               (values (make-js-token :T-BIGINT (parse-integer num-str))
                       pos2)))
           (let ((val (if is-float
                          ;; JS numbers are IEEE-754 doubles. Bind the reader's
                          ;; default float format to double-float so e.g. "1.5e-3"
                          ;; is read at full double precision instead of being read
                          ;; as a single-float (losing ~1e-7 precision) and only
                          ;; then widened.
                          (let ((*read-eval* nil)
                                (*read-default-float-format* 'double-float))
                            (let ((v (read-from-string num-str)))
                              (float v 1.0d0)))
                          (parse-integer num-str))))
             (values (make-js-token :T-NUMBER val) pos2))))))))

;;; Identifier / keyword lexer

(defun lex-js-identifier (source pos)
  "Lex an identifier or keyword starting at POS.
Looks up in *js-keywords*; returns keyword token or :T-IDENT.
Returns (values token new-pos)."
  (let ((start pos))
    (loop while (and (< pos (length source))
                     (js-id-continue-p (char source pos)))
          do (incf pos))
    (let* ((name (subseq source start pos))
           (kw-type (gethash name *js-keywords*)))
      (if kw-type
          (values (make-js-token kw-type name) pos)
          (values (make-js-token :T-IDENT name) pos)))))

;;; Private identifier lexer (#name)

(defun lex-js-private-ident (source pos)
  "Lex a private identifier. POS is after the # was confirmed as private-ident start.
Reads identifier chars and returns :T-PRIVATE-IDENT token with the name (without #).
Returns (values token new-pos)."
  (let ((start pos))
    (loop while (and (< pos (length source))
                     (js-id-continue-p (char source pos)))
          do (incf pos))
    (when (= pos start)
      (error "JS lex error: expected identifier after #"))
    (values (make-js-token :T-PRIVATE-IDENT (subseq source start pos)) pos)))

;;; Regex disambiguation

(defun js-regex-follows-p (prev-type)
  "Return T when a / should be lexed as the start of a regex literal.
Return NIL when it should be treated as a division operator."
  (member prev-type
          '(nil
            :T-OP :T-LPAREN :T-LBRACE :T-LBRACKET
            :T-RETURN :T-TYPEOF :T-INSTANCEOF :T-IN :T-DELETE :T-VOID
            :T-BREAK :T-CASE :T-ELSE :T-THROW :T-YIELD :T-AWAIT :T-NEW)
          :test #'eq))

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
              (multiple-value-bind (tok-list new-pos)
                  (js-lex-template source (1+ pos))
                (dolist (tok tok-list)
                  (push tok tokens))
                (setf prev-token-type :T-TEMPLATE-START
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
                ;; If js-lex-regex is available (from lexer-regex.lisp), use it
                ((fboundp 'js-lex-regex)
                 (multiple-value-bind (tok new-pos)
                     (js-lex-regex source pos)
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
