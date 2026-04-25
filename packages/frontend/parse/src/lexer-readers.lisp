(in-package :cl-cc/parse)
;;;; lexer-readers.lisp — String, character, and symbol/keyword token readers.
;;;;
;;;; Depends on lexer.lisp (structs, state ops, char classification, lex-make-token).
;;;; Number reader: lexer-number.lisp (loads before this file).
;;;; See also: lexer-dispatch.lisp for the main token router.

;;; ─── String Lexer ───────────────────────────────────────────────────────────

;;; Data table: backslash escape character → replacement.
(defparameter *lex-string-escape-table*
  '((#\n . #\Newline)
    (#\t . #\Tab)
    (#\r . #\Return)
    (#\\ . #\\)
    (#\" . #\")
    (#\0 . #\Nul))
  "Alist of (escape-char . literal-char) for string backslash escapes.")

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
                (or (cdr (assoc esc *lex-string-escape-table*)) esc)
                buf)
               (lex-advance state)))
            (t
             (vector-push-extend ch buf)
             (lex-advance state))))))))

;;; ─── Character Lexer ────────────────────────────────────────────────────────

;;; Data table: maps standard character name strings to their character values.
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
          (lex-make-token state :T-CHAR first-ch start)))))

;;; ─── Symbol / Keyword Lexer ─────────────────────────────────────────────────

(defun lex-read-symbol-name (state)
  "Read a symbol token name, handling |pipe| escaping.
   Returns the symbol name string (already uppercased unless pipe-escaped)."
  (let ((buf (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)))
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
    (loop while (and (not (lex-at-end-p state))
                     (lex-constituent-p (lex-peek state)))
          do (vector-push-extend (lex-peek state) buf)
             (lex-advance state))
    (string-upcase (copy-seq buf))))

(defun %lex-find-package (name)
  "Resolve package NAME through the runtime package layer when available.
Returns NIL when the runtime package layer is unavailable."
  (when cl-cc/bootstrap::*runtime-find-package-fn*
    (funcall cl-cc/bootstrap::*runtime-find-package-fn* name)))

(defun %lex-runtime-intern (name package)
  "Intern NAME through the runtime package layer when available.
Falls back to MAKE-SYMBOL semantics when runtime intern is unavailable." 
  (if cl-cc/bootstrap::*runtime-intern-fn*
      (funcall cl-cc/bootstrap::*runtime-intern-fn* name package)
      (make-symbol (string name))))

(defun lex-read-symbol-or-number (state)
  "Read a symbol, keyword, or number from the current position."
  (let ((start (lexer-state-pos state)))
    (let ((num-tok (lex-read-number state)))
      (when num-tok
        (when (or (lex-at-end-p state) (lex-terminating-p (lex-peek state)))
          (return-from lex-read-symbol-or-number num-tok))
        (setf (lexer-state-pos state) start)))
    (let* ((name (lex-read-symbol-name state))
           (sym-name name)
           (pkg-name nil))
      (when (and (not (lex-at-end-p state))
                 (char= (lex-peek state) #\:))
        (setf pkg-name name)
        (lex-advance state)
        (when (and (not (lex-at-end-p state))
                   (char= (lex-peek state) #\:))
          (lex-advance state))
        (setf sym-name (lex-read-symbol-name state)))
      (cond
        ((and (null pkg-name) (string= sym-name "T"))
         (lex-make-token state :T-BOOL-TRUE t start))
        ((and (null pkg-name) (string= sym-name "NIL"))
         (lex-make-token state :T-BOOL-FALSE nil start))
        ((and (null pkg-name) (string= sym-name "."))
         (lex-make-token state :T-DOT nil start))
        (t
          (let ((symbol (if pkg-name
                            (let ((pkg (%lex-find-package (string-upcase pkg-name))))
                              (if pkg
                                  (%lex-runtime-intern sym-name pkg)
                                  (intern (format nil "~A::~A" pkg-name sym-name))))
                             (intern sym-name))))
            (lex-make-token state :T-IDENT symbol start)))))))

(defun lex-read-keyword (state)
  "Read a keyword symbol (after the colon)."
  (let ((start (- (lexer-state-pos state) 1)))
    (let ((name (lex-read-symbol-name state)))
      (lex-make-token state :T-KEYWORD (intern name :keyword) start))))
