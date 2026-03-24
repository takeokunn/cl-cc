;;;; lexer.lisp — Complete hand-written Common Lisp lexer
;;;;
;;;; NO read-from-string. Full tokenization with trivia tracking,
;;;; source positions, and all CL reader macros.

(in-package :cl-cc)

;;; ─── Lexer Structs ───────────────────────────────────────────────────────────

(defstruct lexer-token
  "A token produced by the CL lexer."
  (type       nil :type (or keyword null))
  (value      nil)
  (start-byte 0   :type integer)
  (end-byte   0   :type integer)
  (line       1   :type integer)
  (column     0   :type integer)
  (trivia     nil :type list))

(defstruct lexer-state
  "Mutable state for the lexer."
  (source    ""  :type string)
  (pos       0   :type integer)
  (line      1   :type integer)
  (column    0   :type integer)
  (trivia-acc nil :type list))

;;; ─── Character Classification ────────────────────────────────────────────────

(defun lex-whitespace-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun lex-digit-p (ch)
  (and ch (digit-char-p ch)))

(defun lex-constituent-p (ch)
  "Is CH a constituent character for CL symbols?"
  (and ch
       (not (lex-whitespace-p ch))
       (not (member ch '(#\( #\) #\' #\` #\, #\" #\;) :test #'char=))))

(defun lex-terminating-p (ch)
  "Is CH a terminating macro character or whitespace or nil?"
  (or (null ch)
      (lex-whitespace-p ch)
      (member ch '(#\( #\) #\' #\` #\, #\" #\;) :test #'char=)))

;;; ─── Low-Level State Operations ──────────────────────────────────────────────

(defun lex-peek (state)
  "Return current character or NIL at end."
  (let ((pos (lexer-state-pos state))
        (src (lexer-state-source state)))
    (when (< pos (length src))
      (char src pos))))

(defun lex-advance (state)
  "Advance by one character, updating line/column."
  (let ((ch (lex-peek state)))
    (when ch
      (if (char= ch #\Newline)
          (progn
            (incf (lexer-state-line state))
            (setf (lexer-state-column state) 0))
          (incf (lexer-state-column state)))
      (incf (lexer-state-pos state)))
    ch))

(defun lex-at-end-p (state)
  (>= (lexer-state-pos state) (length (lexer-state-source state))))

;;; ─── Trivia Collection ──────────────────────────────────────────────────────

(defun lex-skip-whitespace (state)
  "Skip whitespace, accumulating trivia."
  (let ((start (lexer-state-pos state)))
    (loop while (and (not (lex-at-end-p state))
                     (lex-whitespace-p (lex-peek state)))
          do (lex-advance state))
    (when (> (lexer-state-pos state) start)
      (push (make-cst-trivia :kind :whitespace
                             :text (subseq (lexer-state-source state) start (lexer-state-pos state))
                             :start-byte start
                             :end-byte (lexer-state-pos state))
            (lexer-state-trivia-acc state)))))

(defun lex-skip-line-comment (state)
  "Skip a ; line comment, accumulating trivia."
  (let ((start (lexer-state-pos state)))
    (loop while (and (not (lex-at-end-p state))
                     (not (char= (lex-peek state) #\Newline)))
          do (lex-advance state))
    (push (make-cst-trivia :kind :line-comment
                           :text (subseq (lexer-state-source state) start (lexer-state-pos state))
                           :start-byte start
                           :end-byte (lexer-state-pos state))
          (lexer-state-trivia-acc state))))

(defun lex-skip-block-comment (state)
  "Skip a #|...|# block comment, accumulating trivia."
  (let ((start (- (lexer-state-pos state) 2))
        (depth 1))
    (loop while (and (> depth 0) (not (lex-at-end-p state)))
          do (let ((ch (lex-peek state)))
               (cond
                 ((and (char= ch #\#)
                       (< (1+ (lexer-state-pos state)) (length (lexer-state-source state)))
                       (char= (char (lexer-state-source state) (1+ (lexer-state-pos state))) #\|))
                  (lex-advance state) (lex-advance state)
                  (incf depth))
                 ((and (char= ch #\|)
                       (< (1+ (lexer-state-pos state)) (length (lexer-state-source state)))
                       (char= (char (lexer-state-source state) (1+ (lexer-state-pos state))) #\#))
                  (lex-advance state) (lex-advance state)
                  (decf depth))
                 (t (lex-advance state)))))
    (push (make-cst-trivia :kind :block-comment
                           :text (subseq (lexer-state-source state) start (lexer-state-pos state))
                           :start-byte start
                           :end-byte (lexer-state-pos state))
          (lexer-state-trivia-acc state))))

(defun lex-skip-trivia (state)
  "Skip all whitespace and comments, collecting trivia."
  (loop
    (cond
      ((lex-at-end-p state) (return))
      ((lex-whitespace-p (lex-peek state))
       (lex-skip-whitespace state))
      ((char= (lex-peek state) #\;)
       (lex-skip-line-comment state))
      (t (return)))))

(defun lex-drain-trivia (state)
  "Return accumulated trivia and clear the accumulator."
  (let ((trivia (nreverse (lexer-state-trivia-acc state))))
    (setf (lexer-state-trivia-acc state) nil)
    trivia))

;;; ─── Token Constructor ──────────────────────────────────────────────────────

(defun lex-make-token (state type value start-byte)
  "Create a token, attaching accumulated trivia."
  (make-lexer-token :type type
                    :value value
                    :start-byte start-byte
                    :end-byte (lexer-state-pos state)
                    :line (lexer-state-line state)
                    :column (lexer-state-column state)
                    :trivia (lex-drain-trivia state)))

;;; ─── Number Lexer ───────────────────────────────────────────────────────────

(defun lex-read-radix-integer (state radix)
  "Read an integer in the given RADIX from STATE."
  (let ((start (lexer-state-pos state))
        (value 0)
        (count 0))
    (loop for ch = (lex-peek state)
          while (and ch (digit-char-p ch radix))
          do (setf value (+ (* value radix) (digit-char-p ch radix)))
             (lex-advance state)
             (incf count))
    (when (zerop count)
      (error "Lexer error at byte ~D: expected digit in radix ~D" start radix))
    value))

(defun lex-read-number (state)
  "Read a number token (integer, float, or ratio)."
  (let ((start (lexer-state-pos state))
        (src (lexer-state-source state)))
    ;; Check for sign
    (let ((sign 1))
      (when (and (not (lex-at-end-p state))
                 (or (char= (lex-peek state) #\+)
                     (char= (lex-peek state) #\-)))
        (when (char= (lex-peek state) #\-)
          (setf sign -1))
        (lex-advance state))
      ;; Read integer part
      (let ((int-part 0)
            (has-int nil))
        (loop for ch = (lex-peek state)
              while (and ch (digit-char-p ch))
              do (setf int-part (+ (* int-part 10) (digit-char-p ch)))
                 (setf has-int t)
                 (lex-advance state))
        (cond
          ;; Ratio: digits / digits
          ((and has-int (not (lex-at-end-p state)) (char= (lex-peek state) #\/))
           (lex-advance state)
           (let ((denom (lex-read-radix-integer state 10)))
             (lex-make-token state :T-RATIO (/ (* sign int-part) denom) start)))
          ;; Float: digits . digits [e[+-]digits]
          ((and has-int (not (lex-at-end-p state)) (char= (lex-peek state) #\.)
                (let ((next-pos (1+ (lexer-state-pos state))))
                  (and (< next-pos (length src))
                       (digit-char-p (char src next-pos)))))
           (lex-advance state) ; consume dot
           (let ((frac-start (lexer-state-pos state))
                 (frac-val 0)
                 (frac-digits 0))
             (loop for ch = (lex-peek state)
                   while (and ch (digit-char-p ch))
                   do (setf frac-val (+ (* frac-val 10) (digit-char-p ch)))
                      (incf frac-digits)
                      (lex-advance state))
             (let ((mantissa (+ int-part (/ frac-val (expt 10.0d0 frac-digits)))))
               ;; Exponent
               (when (and (not (lex-at-end-p state))
                          (member (lex-peek state) '(#\e #\E #\d #\D #\f #\F #\s #\S #\l #\L)
                                  :test #'char=))
                 (lex-advance state)
                 (let ((exp-sign 1))
                   (when (and (not (lex-at-end-p state))
                              (or (char= (lex-peek state) #\+)
                                  (char= (lex-peek state) #\-)))
                     (when (char= (lex-peek state) #\-)
                       (setf exp-sign -1))
                     (lex-advance state))
                   (let ((exp-val (lex-read-radix-integer state 10)))
                     (setf mantissa (* mantissa (expt 10.0d0 (* exp-sign exp-val)))))))
               (lex-make-token state :T-FLOAT (* sign mantissa) start))))
          ;; Plain integer
          (has-int
           (lex-make-token state :T-INT (* sign int-part) start))
          ;; Just a sign with no digits — put back and let symbol reader handle it
          (t
           (setf (lexer-state-pos state) start)
           nil))))))

;;; ─── String Lexer ───────────────────────────────────────────────────────────

(defun lex-read-string (state)
  "Read a double-quoted string literal."
  (let ((start (lexer-state-pos state)))
    (lex-advance state) ; skip opening "
    (let ((buf (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))
      (loop
        (when (lex-at-end-p state)
          (error "Lexer error at byte ~D: unterminated string" start))
        (let ((ch (lex-peek state)))
          (cond
            ((char= ch #\")
             (lex-advance state)
             (return (lex-make-token state :T-STRING (copy-seq buf) start)))
            ((char= ch #\\)
             (lex-advance state)
             (when (lex-at-end-p state)
               (error "Lexer error at byte ~D: trailing backslash in string" start))
             (let ((esc (lex-peek state)))
               (vector-push-extend
                (case esc
                  (#\n #\Newline) (#\t #\Tab) (#\r #\Return)
                  (#\\ #\\) (#\" #\") (#\0 #\Nul)
                  (otherwise esc))
                buf)
               (lex-advance state)))
            (t
             (vector-push-extend ch buf)
             (lex-advance state))))))))

;;; ─── Character Lexer ────────────────────────────────────────────────────────

(defun lex-read-character (state)
  "Read a #\\char character literal. Assumes # and \\ already consumed."
  (let ((start (- (lexer-state-pos state) 2)))
    (when (lex-at-end-p state)
      (error "Lexer error at byte ~D: expected character after #\\\\" start))
    ;; Read character name or single char
    (let ((first-ch (lex-peek state)))
      (lex-advance state)
      ;; Check if it is a named character
      (if (and (alpha-char-p first-ch)
               (not (lex-at-end-p state))
               (alpha-char-p (lex-peek state)))
          ;; Named character: read rest of name
          (let ((name (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
            (vector-push-extend first-ch name)
            (loop while (and (not (lex-at-end-p state))
                             (lex-constituent-p (lex-peek state)))
                  do (vector-push-extend (lex-peek state) name)
                     (lex-advance state))
            (let ((name-str (string-upcase (copy-seq name))))
              (lex-make-token state :T-CHAR
                             (cond
                               ((string= name-str "SPACE") #\Space)
                               ((string= name-str "NEWLINE") #\Newline)
                               ((string= name-str "TAB") #\Tab)
                               ((string= name-str "RETURN") #\Return)
                               ((string= name-str "BACKSPACE") #\Backspace)
                               ((string= name-str "PAGE") #\Page)
                               ((string= name-str "RUBOUT") #\Rubout)
                               ((string= name-str "LINEFEED") #\Linefeed)
                               ((string= name-str "NUL") #\Nul)
                               (t (error "Lexer error: unknown character name ~S" name-str)))
                             start)))
          ;; Single character
          (lex-make-token state :T-CHAR first-ch start)))))

;;; ─── Symbol / Keyword Lexer ────────────────────────────────────────────────

(defun lex-read-symbol-name (state)
  "Read a symbol token name, handling |pipe| escaping.
   Returns the symbol name string (already uppercased unless pipe-escaped)."
  (let ((buf (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t))
        (pipe-escaped nil))
    (when (and (not (lex-at-end-p state))
               (char= (lex-peek state) #\|))
      (setf pipe-escaped t)
      (lex-advance state)
      (loop
        (when (lex-at-end-p state)
          (error "Lexer error: unterminated pipe-escaped symbol"))
        (let ((ch (lex-peek state)))
          (lex-advance state)
          (when (char= ch #\|) (return))
          (vector-push-extend ch buf)))
      (return-from lex-read-symbol-name (copy-seq buf)))
    ;; Normal constituent characters
    (loop while (and (not (lex-at-end-p state))
                     (lex-constituent-p (lex-peek state)))
          do (vector-push-extend (lex-peek state) buf)
             (lex-advance state))
    (string-upcase (copy-seq buf))))

(defun lex-read-symbol-or-number (state)
  "Read a symbol, keyword, or number from the current position."
  (let ((start (lexer-state-pos state)))
    ;; Try number first
    (let ((num-tok (lex-read-number state)))
      (when num-tok
        ;; Ensure the number is terminated properly
        (when (or (lex-at-end-p state) (lex-terminating-p (lex-peek state)))
          (return-from lex-read-symbol-or-number num-tok))
        ;; Not terminated — revert and read as symbol
        (setf (lexer-state-pos state) start)))
    ;; Read as symbol
    (let* ((name (lex-read-symbol-name state))
           (sym-name name))
      ;; Check for package qualifier
      (when (and (not (lex-at-end-p state))
                 (char= (lex-peek state) #\:))
        ;; This was actually a package prefix
        (lex-advance state)
        ;; Skip optional second colon (internal symbol)
        (when (and (not (lex-at-end-p state))
                   (char= (lex-peek state) #\:))
          (lex-advance state))
        (let ((sym-part (lex-read-symbol-name state)))
          (setf sym-name (format nil "~A:~A" name sym-part))))
      ;; Determine token type
      (cond
        ((string= sym-name "T")
         (lex-make-token state :T-BOOL-TRUE t start))
        ((string= sym-name "NIL")
         (lex-make-token state :T-BOOL-FALSE nil start))
        ((string= sym-name ".")
         (lex-make-token state :T-DOT nil start))
        (t
         (lex-make-token state :T-IDENT (intern sym-name) start))))))

(defun lex-read-keyword (state)
  "Read a keyword symbol (after the colon)."
  (let ((start (- (lexer-state-pos state) 1)))
    (let ((name (lex-read-symbol-name state)))
      (lex-make-token state :T-KEYWORD (intern name :keyword) start))))

;;; ─── Hash Dispatch ──────────────────────────────────────────────────────────

(defun lex-read-hash-dispatch (state)
  "Handle # dispatch: #', #\\, #(, #b, #o, #x, #t, #f, #|...|#."
  (let ((start (lexer-state-pos state)))
    (lex-advance state) ; skip #
    (when (lex-at-end-p state)
      (error "Lexer error at byte ~D: unexpected end after #" start))
    (let ((ch (lex-peek state)))
      (case ch
        ;; #' function
        (#\' (lex-advance state)
             (lex-make-token state :T-FUNCTION nil start))
        ;; #\ character
        (#\\ (lex-advance state)
             (lex-read-character state))
        ;; #( vector
        (#\( (lex-advance state)
             (lex-make-token state :T-VECTOR-START nil start))
        ;; #b binary
        (#\b (lex-advance state)
             (let ((val (lex-read-radix-integer state 2)))
               (lex-make-token state :T-INT val start)))
        (#\B (lex-advance state)
             (let ((val (lex-read-radix-integer state 2)))
               (lex-make-token state :T-INT val start)))
        ;; #o octal
        (#\o (lex-advance state)
             (let ((val (lex-read-radix-integer state 8)))
               (lex-make-token state :T-INT val start)))
        (#\O (lex-advance state)
             (let ((val (lex-read-radix-integer state 8)))
               (lex-make-token state :T-INT val start)))
        ;; #x hexadecimal
        (#\x (lex-advance state)
             (let ((val (lex-read-radix-integer state 16)))
               (lex-make-token state :T-INT val start)))
        (#\X (lex-advance state)
             (let ((val (lex-read-radix-integer state 16)))
               (lex-make-token state :T-INT val start)))
        ;; #| block comment
        (#\| (lex-advance state)
             (lex-skip-block-comment state)
             ;; Return nil to signal "no token, just trivia"
             nil)
        ;; Boolean dispatch (non-standard but useful)
        (#\t (lex-advance state)
             (lex-make-token state :T-BOOL-TRUE t start))
        (#\f (lex-advance state)
             (lex-make-token state :T-BOOL-FALSE nil start))
        (otherwise
         (error "Lexer error at byte ~D: unknown dispatch character #~C" start ch))))))

;;; ─── Main Token Reader ──────────────────────────────────────────────────────

(defun lexer-read-token (state)
  "Read the next token from STATE. Returns a lexer-token or EOF token."
  (lex-skip-trivia state)
  (when (lex-at-end-p state)
    (return-from lexer-read-token
      (lex-make-token state :T-EOF nil (lexer-state-pos state))))
  (let ((ch (lex-peek state))
        (start (lexer-state-pos state)))
    (cond
      ;; Left paren
      ((char= ch #\()
       (lex-advance state)
       (lex-make-token state :T-LPAREN nil start))
      ;; Right paren
      ((char= ch #\))
       (lex-advance state)
       (lex-make-token state :T-RPAREN nil start))
      ;; Quote
      ((char= ch #\')
       (lex-advance state)
       (lex-make-token state :T-QUOTE nil start))
      ;; Backquote
      ((char= ch #\`)
       (lex-advance state)
       (lex-make-token state :T-BACKQUOTE nil start))
      ;; Unquote (and unquote-splicing)
      ((char= ch #\,)
       (lex-advance state)
       (if (and (not (lex-at-end-p state)) (char= (lex-peek state) #\@))
           (progn (lex-advance state)
                  (lex-make-token state :T-UNQUOTE-SPLICING nil start))
           (lex-make-token state :T-UNQUOTE nil start)))
      ;; String
      ((char= ch #\")
       (lex-read-string state))
      ;; Hash dispatch
      ((char= ch #\#)
       (let ((tok (lex-read-hash-dispatch state)))
         (if tok tok
             ;; Block comment produced trivia, not a token — recurse
             (lexer-read-token state))))
      ;; Keyword
      ((char= ch #\:)
       (lex-advance state)
       (lex-read-keyword state))
      ;; Symbol or number
      (t
       (lex-read-symbol-or-number state)))))

;;; ─── Public API ──────────────────────────────────────────────────────────────

(defun make-lexer (source)
  "Create a new lexer state for SOURCE string."
  (make-lexer-state :source source))

(defun lex-all (source)
  "Tokenize SOURCE string completely, returning a list of lexer-token structs.
   The last token has type :T-EOF."
  (let ((state (make-lexer source))
        (tokens nil))
    (loop
      (let ((tok (lexer-read-token state)))
        (push tok tokens)
        (when (eq (lexer-token-type tok) :T-EOF)
          (return))))
    (nreverse tokens)))
