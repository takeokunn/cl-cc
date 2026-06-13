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

;;; Number literal lexer — see lexer-number.lisp

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


;;; lex-js-operator + tokenize-js-source → see lexer-operator.lisp
