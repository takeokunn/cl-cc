(in-package :cl-cc)
;;;; lexer-readers.lisp — Token readers: number, string, character, symbol, keyword.
;;;;
;;;; Depends on lexer.lisp (structs, state ops, char classification, lex-make-token).
;;;; See also: lexer-dispatch.lisp for the main token router.

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

(defun %lex-read-sign (state)
  "Consume a leading +/- sign from STATE and return the integer sign (1 or -1).
Does NOT advance state if neither char is present."
  (let ((sign 1))
    (when (and (not (lex-at-end-p state))
               (or (char= (lex-peek state) #\+)
                   (char= (lex-peek state) #\-)))
      (when (char= (lex-peek state) #\-)
        (setf sign -1))
      (lex-advance state))
    sign))

(defun %lex-read-decimal-int (state)
  "Consume decimal digits from STATE and return (values integer-value found-p)."
  (let ((int-part 0)
        (has-int nil))
    (loop for ch = (lex-peek state)
          while (and ch (digit-char-p ch))
          do (setf int-part (+ (* int-part 10) (digit-char-p ch)))
             (setf has-int t)
             (lex-advance state))
    (values int-part has-int)))

(defun %lex-ratio-next-p (state has-int)
  "Return true when the next character is / after a valid integer part."
  (and has-int (not (lex-at-end-p state)) (char= (lex-peek state) #\/)))

(defun %lex-float-next-p (state has-int)
  "Return true when the next character is . followed by a digit (float literal)."
  (and has-int
       (not (lex-at-end-p state))
       (char= (lex-peek state) #\.)
       (let ((next-pos (1+ (lexer-state-pos state)))
             (src (lexer-state-source state)))
         (and (< next-pos (length src))
              (digit-char-p (char src next-pos))))))

(defun %lex-read-ratio-tail (state sign int-part start)
  "Called after HAS-INT and / detected; advance past /, read denominator, make :T-RATIO token."
  (lex-advance state)
  (let ((denom (lex-read-radix-integer state 10)))
    (lex-make-token state :T-RATIO (/ (* sign int-part) denom) start)))

(defun %lex-read-float-tail (state sign int-part start)
  "Called after . detected (and next char is digit); reads .digits[exp] and makes :T-FLOAT token."
  (lex-advance state) ; consume dot
  (let ((frac-start (lexer-state-pos state))
        (frac-val 0)
        (frac-digits 0)
        (exp-marker nil))
    (declare (ignore frac-start))
    (loop for ch = (lex-peek state)
          while (and ch (digit-char-p ch))
          do (setf frac-val (+ (* frac-val 10) (digit-char-p ch)))
             (incf frac-digits)
             (lex-advance state))
    ;; Compute mantissa as exact rational to avoid premature type coercion
    (let ((mantissa (+ int-part (/ frac-val (expt 10 frac-digits))))
          (exp-val 0)
          (exp-sign 1))
      ;; Exponent — record marker char to determine float type
      (when (and (not (lex-at-end-p state))
                 (member (lex-peek state) '(#\e #\E #\d #\D #\f #\F #\s #\S #\l #\L)
                         :test #'char=))
        (setf exp-marker (lex-peek state))
        (lex-advance state)
        (when (and (not (lex-at-end-p state))
                   (or (char= (lex-peek state) #\+)
                       (char= (lex-peek state) #\-)))
          (when (char= (lex-peek state) #\-)
            (setf exp-sign -1))
          (lex-advance state))
        (setf exp-val (lex-read-radix-integer state 10)))
      ;; Coerce to the right float type based on exponent marker:
      ;;   d/D → double-float; all others → single-float (ANSI default)
      (let* ((scaled (* mantissa (expt 10 (* exp-sign exp-val))))
             (float-val (if (and exp-marker
                                 (or (char= exp-marker #\d)
                                     (char= exp-marker #\D)))
                            (float (* sign scaled) 0.0d0)
                            (float (* sign scaled) 0.0))))
        (lex-make-token state :T-FLOAT float-val start)))))

(defun lex-read-number (state)
  "Read a number token: integer, float, or ratio."
  (let ((start (lexer-state-pos state))
        (sign  (%lex-read-sign state)))
    (multiple-value-bind (int-part has-int) (%lex-read-decimal-int state)
      (cond
        ((%lex-ratio-next-p state has-int)
         (%lex-read-ratio-tail state sign int-part start))
        ((%lex-float-next-p state has-int)
         (%lex-read-float-tail state sign int-part start))
        (has-int
         ;; Plain integer
         (lex-make-token state :T-INT (* sign int-part) start))
        (t
         ;; Just a sign with no digits — put back and let symbol reader handle it
         (setf (lexer-state-pos state) start)
         nil)))))

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

;;; Data table: maps standard character name strings to their character values.
;;; Used by lex-read-character for named character literals like #\Space, #\Newline.
(defparameter *lex-named-characters*
  '(("SPACE"     . #\Space)
    ("NEWLINE"   . #\Newline)
    ("TAB"       . #\Tab)
    ("RETURN"    . #\Return)
    ("BACKSPACE" . #\Backspace)
    ("PAGE"      . #\Page)
    ("RUBOUT"    . #\Rubout)
    ("LINEFEED"  . #\Linefeed)
    ("NUL"       . #\Nul)
    ("NULL"      . #\Nul)
    ("ESCAPE"    . #\Escape)
    ("ESC"       . #\Escape))
  "Alist mapping uppercase character name strings to character objects.")

(defun lex-read-character (state)
  "Read a #\\char character literal. Assumes # and \\ already consumed."
  (let ((start (- (lexer-state-pos state) 2)))
    (when (lex-at-end-p state)
      (error "Lexer error at byte ~D: expected character after #\\\\" start))
    (let ((first-ch (lex-peek state)))
      (lex-advance state)
      (if (and (alpha-char-p first-ch)
               (not (lex-at-end-p state))
               (alpha-char-p (lex-peek state)))
          ;; Named character: read rest of name, then look up in data table
          (let ((name (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
            (vector-push-extend first-ch name)
            (loop while (and (not (lex-at-end-p state))
                             (lex-constituent-p (lex-peek state)))
                  do (vector-push-extend (lex-peek state) name)
                     (lex-advance state))
            (let* ((name-str (string-upcase (copy-seq name)))
                   (ch-val (or (cdr (assoc name-str *lex-named-characters* :test #'string=))
                               (cl:name-char name-str)
                               (error "Lexer error: unknown character name ~S" name-str))))
              (lex-make-token state :T-CHAR ch-val start)))
          ;; Single character
          (lex-make-token state :T-CHAR first-ch start)))))

;;; ─── Symbol / Keyword Lexer ────────────────────────────────────────────────

(defun lex-read-symbol-name (state)
  "Read a symbol token name, handling |pipe| escaping.
   Returns the symbol name string (already uppercased unless pipe-escaped)."
  (let ((buf (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)))
    ;; Pipe-escaped: |...| — case-preserved, returned immediately
    (when (and (not (lex-at-end-p state))
               (char= (lex-peek state) #\|))
      (lex-advance state)
      (loop
        (when (lex-at-end-p state)
          (error "Lexer error: unterminated pipe-escaped symbol"))
        (let ((ch (lex-peek state)))
          (lex-advance state)
          (when (char= ch #\|) (return))
          (vector-push-extend ch buf)))
      (return-from lex-read-symbol-name (copy-seq buf)))
    ;; Normal constituent characters: upcase on return
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
           (sym-name name)
           (pkg-name nil))
      ;; Check for package qualifier
      (when (and (not (lex-at-end-p state))
                 (char= (lex-peek state) #\:))
        ;; This was actually a package prefix
        (setf pkg-name name)
        (lex-advance state)
        ;; Skip optional second colon (internal symbol)
        (when (and (not (lex-at-end-p state))
                   (char= (lex-peek state) #\:))
          (lex-advance state))
        (setf sym-name (lex-read-symbol-name state)))
      ;; Determine token type
      (cond
        ((and (null pkg-name) (string= sym-name "T"))
         (lex-make-token state :T-BOOL-TRUE t start))
        ((and (null pkg-name) (string= sym-name "NIL"))
         (lex-make-token state :T-BOOL-FALSE nil start))
        ((and (null pkg-name) (string= sym-name "."))
         (lex-make-token state :T-DOT nil start))
        (t
         (let ((symbol (if pkg-name
                           (let ((pkg (find-package (string-upcase pkg-name))))
                             (if pkg
                                 (intern sym-name pkg)
                                 (intern (format nil "~A::~A" pkg-name sym-name))))
                           (intern sym-name))))
           (lex-make-token state :T-IDENT symbol start)))))))

(defun lex-read-keyword (state)
  "Read a keyword symbol (after the colon)."
  (let ((start (- (lexer-state-pos state) 1)))
    (let ((name (lex-read-symbol-name state)))
      (lex-make-token state :T-KEYWORD (intern name :keyword) start))))
