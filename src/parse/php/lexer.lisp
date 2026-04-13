;;;; frontend/php/lexer.lisp - PHP 8.x Lexer
;;;;
;;;; Tokenizes PHP source code into a list of token plists.
;;;; Token format: (:type :T-XXX :value val)
;;;;
;;;; Token types:
;;;;   :T-EOF            - end of input
;;;;   :T-VAR            - $name (value = interned symbol, no $)
;;;;   :T-INT            - integer literal (value = integer)
;;;;   :T-FLOAT          - float literal (value = float)
;;;;   :T-STRING         - string literal (value = string content)
;;;;   :T-IDENT          - bare identifier (value = string)
;;;;   :T-KEYWORD        - PHP keyword (value = keyword symbol)
;;;;   :T-TYPE           - type keyword (value = keyword symbol)
;;;;   :T-OP             - operator (value = string)
;;;;   :T-ARROW          - -> (value = "->")
;;;;   :T-NULLSAFE-ARROW - ?-> (value = "?->")
;;;;   :T-DOUBLE-COLON   - :: (value = "::")
;;;;   :T-NULLABLE       - ? before a type (value = "?")
;;;;   :T-LPAREN         - (
;;;;   :T-RPAREN         - )
;;;;   :T-LBRACE         - {
;;;;   :T-RBRACE         - }
;;;;   :T-LBRACKET       - [
;;;;   :T-RBRACKET       - ]
;;;;   :T-SEMI           - ;
;;;;   :T-COMMA          - ,
;;;;   :T-COLON          - :

(in-package :cl-cc)

;;; PHP keyword sets

(defvar *php-keywords*
  '("if" "else" "elseif" "while" "for" "foreach" "function" "return" "echo"
    "class" "interface" "trait" "extends" "implements" "new" "match" "throw"
    "null" "true" "false" "readonly" "enum" "static" "abstract" "final"
    "public" "protected" "private" "do" "break" "continue" "switch" "case"
    "default" "try" "catch" "finally" "use" "namespace" "list" "as" "in"
    "instanceof" "print" "yield" "from" "fn" "const")
  "PHP reserved keywords (lowercased).")

(defvar *php-type-keywords*
  '("int" "string" "float" "bool" "array" "void" "mixed" "null" "never"
    "object" "callable" "iterable" "self" "parent" "static")
  "PHP type keywords (lowercased).")

;;; Token constructor helper

(defun make-php-token (type value)
  "Construct a PHP token plist."
  (list :type type :value value))

;;; Character classification helpers

(defun php-digit-p (ch)
  (and ch (char<= #\0 ch #\9)))

(defun php-alpha-p (ch)
  (and ch (or (char<= #\a ch #\z)
              (char<= #\A ch #\Z)
              (char= ch #\_))))

(defun php-alnum-p (ch)
  (or (php-alpha-p ch) (php-digit-p ch)))

(defun php-whitespace-p (ch)
  (and ch (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=)))

;;; String character reader helpers

(defun php-lex-peek (source pos)
  "Return character at POS, or NIL if out of bounds."
  (when (< pos (length source))
    (char source pos)))

(defun php-lex-peek2 (source pos)
  "Return character at POS+1, or NIL."
  (when (< (1+ pos) (length source))
    (char source (1+ pos))))

;;; Comment skipping

(defun skip-line-comment (source pos)
  "Skip from POS past end of line (// or # comment). Returns new pos."
  (loop while (and (< pos (length source))
                   (not (char= (char source pos) #\Newline)))
        do (incf pos))
  pos)

(defun skip-block-comment (source pos)
  "Skip /* ... */ block comment starting after /*. Returns new pos."
  (loop
    (when (>= pos (length source))
      (error "PHP lex error: unterminated block comment"))
    (let ((ch (char source pos)))
      (cond
        ((and (char= ch #\*)
              (< (1+ pos) (length source))
              (char= (char source (1+ pos)) #\/))
         (return (+ pos 2)))
        (t (incf pos))))))

(defun skip-whitespace-and-comments (source pos)
  "Skip whitespace and PHP comments (// # /* */). Returns new position."
  (loop
    (cond
      ;; End of source
      ((>= pos (length source))
       (return pos))
      ;; Whitespace
      ((php-whitespace-p (char source pos))
       (incf pos))
      ;; Line comment: // or #
      ((and (char= (char source pos) #\/)
            (< (1+ pos) (length source))
            (char= (char source (1+ pos)) #\/))
       (setf pos (skip-line-comment source (+ pos 2))))
      ((char= (char source pos) #\#)
       (setf pos (skip-line-comment source (1+ pos))))
      ;; Block comment: /* ... */
      ((and (char= (char source pos) #\/)
            (< (1+ pos) (length source))
            (char= (char source (1+ pos)) #\*))
       (setf pos (skip-block-comment source (+ pos 2))))
      ;; Not whitespace/comment
      (t (return pos)))))

;;; String literal lexers

(defun lex-double-quoted-string (source pos)
  "Lex a double-quoted string starting AFTER the opening \". Returns (values string new-pos)."
  (let ((buf (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (when (>= pos (length source))
        (error "PHP lex error: unterminated double-quoted string"))
      (let ((ch (char source pos)))
        (cond
          ((char= ch #\")
           (return (values (copy-seq buf) (1+ pos))))
          ((char= ch #\\)
           ;; Escape sequence
           (incf pos)
           (when (>= pos (length source))
             (error "PHP lex error: trailing backslash in string"))
           (let ((esc (char source pos)))
             (vector-push-extend
              (case esc
                (#\n  #\Newline)
                (#\t  #\Tab)
                (#\r  #\Return)
                (#\\  #\\)
                (#\"  #\")
                (#\$  #\$)
                (otherwise esc))
              buf)
             (incf pos)))
          ;; Simple variable interpolation: $var - just skip the $ for now and treat as literal
          ;; (Full interpolation would require a sub-lexer; for correctness we treat $var as text)
          (t
           (vector-push-extend ch buf)
           (incf pos)))))))

(defun lex-single-quoted-string (source pos)
  "Lex a single-quoted string starting AFTER the opening '. Returns (values string new-pos)."
  (let ((buf (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (when (>= pos (length source))
        (error "PHP lex error: unterminated single-quoted string"))
      (let ((ch (char source pos)))
        (cond
          ((char= ch #\')
           (return (values (copy-seq buf) (1+ pos))))
          ((and (char= ch #\\)
                (< (1+ pos) (length source))
                (member (char source (1+ pos)) '(#\' #\\) :test #'char=))
           (vector-push-extend (char source (1+ pos)) buf)
           (incf pos 2))
          (t
           (vector-push-extend ch buf)
           (incf pos)))))))

;;; Number lexer

(defun lex-number (source pos)
  "Lex an integer or float starting at POS. Returns (values token new-pos)."
  (let ((start pos)
        (is-float nil))
    ;; Collect digits
    (loop while (and (< pos (length source)) (php-digit-p (char source pos)))
          do (incf pos))
    ;; Check for decimal point
    (when (and (< pos (length source))
               (char= (char source pos) #\.)
               (< (1+ pos) (length source))
               (php-digit-p (char source (1+ pos))))
      (setf is-float t)
      (incf pos)
      (loop while (and (< pos (length source)) (php-digit-p (char source pos)))
            do (incf pos)))
    ;; Check for exponent
    (when (and (< pos (length source))
               (member (char source pos) '(#\e #\E) :test #'char=))
      (setf is-float t)
      (incf pos)
      (when (and (< pos (length source))
                 (member (char source pos) '(#\+ #\-) :test #'char=))
        (incf pos))
      (loop while (and (< pos (length source)) (php-digit-p (char source pos)))
            do (incf pos)))
    (let ((num-str (subseq source start pos)))
      (if is-float
          ;; Bind *read-eval* nil to prevent reader macro execution.
          ;; Wrap in handler-case to catch malformed exponents like "1e+" or "1e".
          (handler-case
            (let* ((*read-eval* nil)
                   (val (read-from-string num-str)))
              (if (realp val)
                  (values (make-php-token :T-FLOAT (float val 1.0d0)) pos)
                  (error "PHP lex error: ~S is not a number" num-str)))
            (error (e)
              (error "PHP lex error: malformed float literal ~S: ~A" num-str e)))
          (values (make-php-token :T-INT (parse-integer num-str)) pos)))))

;;; Identifier / keyword lexer

(defun lex-identifier (source pos)
  "Lex an identifier or keyword starting at POS. Returns (values token new-pos)."
  (let ((start pos))
    (loop while (and (< pos (length source)) (php-alnum-p (char source pos)))
          do (incf pos))
    (let* ((raw (subseq source start pos))
           (lower (string-downcase raw)))
      (cond
        ((member lower *php-type-keywords* :test #'string=)
         (values (make-php-token :T-TYPE (intern (string-upcase lower) :keyword)) pos))
        ((member lower *php-keywords* :test #'string=)
         (values (make-php-token :T-KEYWORD (intern (string-upcase lower) :keyword)) pos))
        (t
         (values (make-php-token :T-IDENT raw) pos))))))

;;; Variable lexer ($name)

(defun lex-variable (source pos)
  "Lex a PHP variable $name starting AFTER the $. Returns (values token new-pos)."
  (unless (and (< pos (length source)) (php-alpha-p (char source pos)))
    (error "PHP lex error: expected identifier after $"))
  (let ((start pos))
    (loop while (and (< pos (length source)) (php-alnum-p (char source pos)))
          do (incf pos))
    (let ((name (intern (string-upcase (subseq source start pos)))))
      (values (make-php-token :T-VAR name) pos))))


;;; Operator lexer, PHP opening-tag skipper, and main tokenize-php-source
;;; are in lexer-ops.lisp (loads after this file).
