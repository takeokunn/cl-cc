;;;; lexer-dispatch.lisp — Feature Conditionals, Hash Dispatch, and Main Token Reader
;;;;
;;;; Depends on lexer.lisp (structs, character classification, low-level state ops,
;;;; trivia, token constructor, number/string/character/symbol readers).

(in-package :cl-cc)

;;; ─── Feature Conditionals & Read-Time Eval ─────────────────────────────────

(defun lex-skip-form (state)
  "Skip a single balanced form from STATE (for #+/- when feature test fails).
   Handles atoms, lists, strings, and nested forms."
  (lex-skip-trivia state)
  (when (lex-at-end-p state) (return-from lex-skip-form))
  (let ((ch (lex-peek state)))
    (cond
      ;; Skip a list form
      ((char= ch #\()
       (lex-advance state)
       (let ((depth 1))
         (loop while (and (> depth 0) (not (lex-at-end-p state)))
               do (let ((c (lex-peek state)))
                    (lex-advance state)
                    (cond ((char= c #\() (incf depth))
                          ((char= c #\)) (decf depth))
                          ((char= c #\") ; skip string
                           (loop until (or (lex-at-end-p state)
                                           (char= (lex-peek state) #\"))
                                 do (when (char= (lex-peek state) #\\)
                                      (lex-advance state))
                                    (lex-advance state))
                           (unless (lex-at-end-p state) (lex-advance state)))
                          ((char= c #\;) ; skip line comment
                           (loop until (or (lex-at-end-p state)
                                           (char= (lex-peek state) #\Newline))
                                 do (lex-advance state))))))))
      ;; Skip a string
      ((char= ch #\")
       (lex-advance state)
       (loop until (or (lex-at-end-p state) (char= (lex-peek state) #\"))
             do (when (char= (lex-peek state) #\\) (lex-advance state))
                (lex-advance state))
       (unless (lex-at-end-p state) (lex-advance state)))
      ;; Skip quote/backquote prefix + the following form
      ((or (char= ch #\') (char= ch #\`) (char= ch #\,))
       (lex-advance state)
       (when (and (not (lex-at-end-p state)) (char= (lex-peek state) #\@))
         (lex-advance state))
       (lex-skip-form state))
      ;; Skip #-dispatched form
      ((char= ch #\#)
       (lex-advance state)
       (unless (lex-at-end-p state)
         (let ((dispatch-ch (lex-peek state)))
           (lex-advance state)
           (cond
             ;; #( vector — skip balanced parens
             ((char= dispatch-ch #\()
              (let ((depth 1))
                (loop while (and (> depth 0) (not (lex-at-end-p state)))
                      do (let ((c (lex-peek state)))
                           (lex-advance state)
                           (cond ((char= c #\() (incf depth))
                                 ((char= c #\)) (decf depth)))))))
             ;; #' #. — skip the next form
             ((or (char= dispatch-ch #\') (char= dispatch-ch #\.))
              (lex-skip-form state))
             ;; #\ character — skip one or more chars (e.g. #\Space)
             ((char= dispatch-ch #\\)
              (unless (lex-at-end-p state)
                (lex-advance state)
                (loop while (and (not (lex-at-end-p state))
                                 (lex-constituent-p (lex-peek state)))
                      do (lex-advance state))))
             ;; #+ #- — skip feature + form
             ((or (char= dispatch-ch #\+) (char= dispatch-ch #\-))
              (lex-skip-form state) ; feature
              (lex-skip-form state)) ; body
             ;; #| block comment
             ((char= dispatch-ch #\|)
              (lex-skip-block-comment state))
             ;; Other: skip an atom
             (t (loop while (and (not (lex-at-end-p state))
                                  (lex-constituent-p (lex-peek state)))
                      do (lex-advance state)))))))
      ;; Skip an atom (symbol, number, keyword)
      (t
       (when (char= ch #\:) (lex-advance state)) ; keyword colon
       (loop while (and (not (lex-at-end-p state))
                        (lex-constituent-p (lex-peek state)))
             do (lex-advance state))))))

(defun lex-read-feature-expr (state)
  "Read a feature expression for #+/#-. Returns a keyword symbol or a list like (:or :sbcl :ccl)."
  (lex-skip-trivia state)
  (when (lex-at-end-p state)
    (error "Lexer error: unexpected end in feature expression"))
  (let ((ch (lex-peek state)))
    (if (char= ch #\()
        ;; Compound feature expression: (:or ...), (:and ...), (:not ...)
        (progn
          (lex-advance state) ; skip (
          (let ((parts nil))
            (loop
              (lex-skip-trivia state)
              (when (or (lex-at-end-p state) (char= (lex-peek state) #\)))
                (unless (lex-at-end-p state) (lex-advance state))
                (return (nreverse parts)))
              (push (lex-read-feature-expr state) parts))))
        ;; Simple feature keyword
        (let ((name (lex-read-symbol-name state)))
          (intern (string-upcase name) :keyword)))))

(defun lex-feature-present-p (feature)
  "Evaluate a feature expression against *features*."
  (cond
    ((keywordp feature) (member feature *features*))
    ((and (consp feature) (eq (car feature) :or))
     (some #'lex-feature-present-p (cdr feature)))
    ((and (consp feature) (eq (car feature) :and))
     (every #'lex-feature-present-p (cdr feature)))
    ((and (consp feature) (eq (car feature) :not))
     (not (lex-feature-present-p (cadr feature))))
    (t nil)))

(defun lex-read-form-text (state)
  "Read a single balanced form's raw text from STATE and return it as a string.
   Advances STATE past the form."
  (lex-skip-trivia state)
  (when (lex-at-end-p state)
    (error "Lexer error: unexpected end in form"))
  (let ((start (lexer-state-pos state))
        (ch (lex-peek state)))
    (cond
      ;; Parenthesized form
      ((char= ch #\()
       (lex-advance state)
       (let ((depth 1))
         (loop while (and (> depth 0) (not (lex-at-end-p state)))
               do (let ((c (lex-peek state)))
                    (lex-advance state)
                    (cond ((char= c #\() (incf depth))
                          ((char= c #\)) (decf depth))
                          ((char= c #\")
                           (loop until (or (lex-at-end-p state)
                                           (char= (lex-peek state) #\"))
                                 do (when (char= (lex-peek state) #\\)
                                      (lex-advance state))
                                    (lex-advance state))
                           (unless (lex-at-end-p state) (lex-advance state))))))))
      ;; String
      ((char= ch #\")
       (lex-advance state)
       (loop until (or (lex-at-end-p state) (char= (lex-peek state) #\"))
             do (when (char= (lex-peek state) #\\) (lex-advance state))
                (lex-advance state))
       (unless (lex-at-end-p state) (lex-advance state)))
      ;; Atom (symbol, number, keyword)
      (t
       (when (char= ch #\:) (lex-advance state))
       (loop while (and (not (lex-at-end-p state))
                        (lex-constituent-p (lex-peek state)))
             do (lex-advance state))))
    (subseq (lexer-state-source state) start (lexer-state-pos state))))

;;; ─── Label Table for #n= / #n# (FR-599) ────────────────────────────────────

(defvar *lexer-label-table* nil
  "Hash table mapping integer labels to objects for #n= / #n# reader macros.
Bound to a fresh HT by lex-all; nil outside lexing context.")

;;; ─── Hash Dispatch ──────────────────────────────────────────────────────────

(defun lex-read-hash-dispatch (state)
  "Handle # dispatch: #', #\\, #(, #b/B, #o/O, #x/X, #s/S, #t, #f, #|...|#, #:, #+, #-, #., #0-9."
  (let ((start (lexer-state-pos state)))
    (lex-advance state) ; skip #
    (when (lex-at-end-p state)
      (error "Lexer error at byte ~D: unexpected end after #" start))
    (let ((ch (char-downcase (lex-peek state))))
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
        ;; #b/#B binary
        (#\b (lex-advance state)
             (let ((val (lex-read-radix-integer state 2)))
               (lex-make-token state :T-INT val start)))
        ;; #o/#O octal
        (#\o (lex-advance state)
             (let ((val (lex-read-radix-integer state 8)))
               (lex-make-token state :T-INT val start)))
        ;; #x/#X hexadecimal
        (#\x (lex-advance state)
             (let ((val (lex-read-radix-integer state 16)))
               (lex-make-token state :T-INT val start)))
        ;; #| block comment
        (#\| (lex-advance state)
             (lex-skip-block-comment state)
             ;; Return nil to signal "no token, just trivia"
             nil)
        ;; #: uninterned symbol
        (#\: (lex-advance state)
             (let ((name (lex-read-symbol-name state)))
               (lex-make-token state :T-IDENT (make-symbol name) start)))
        ;; #+ feature conditional (include if feature present)
        (#\+ (lex-advance state)
             (let ((feature (lex-read-feature-expr state)))
               (if (lex-feature-present-p feature)
                   ;; Feature present: return nil (like block comment), next token is the real one
                   nil
                   ;; Feature absent: skip the next form, return nil
                   (progn (lex-skip-form state) nil))))
        ;; #- feature conditional (include if feature absent)
        (#\- (lex-advance state)
             (let ((feature (lex-read-feature-expr state)))
               (if (lex-feature-present-p feature)
                   ;; Feature present: skip the next form
                   (progn (lex-skip-form state) nil)
                   ;; Feature absent: include the next form
                   nil)))
        ;; #. read-time eval — must use host eval for access to host-defined constants
        (#\. (lex-advance state)
             (unless *read-eval*
               (error "Reader error: #. is not allowed when *read-eval* is NIL"))
             (let* ((text (lex-read-form-text state))
                    (form (read-from-string text))
                    (value (eval form)))
               (lex-make-token state :T-INT value start)))
        ;; #C complex literal — #C(real imag) → (complex real imag)
        (#\c (lex-advance state)
             (let* ((form-text (lex-read-form-text state))
                    (full-text (concatenate 'string "#C" form-text))
                    (value (read-from-string full-text)))
               (lex-make-token state :T-INT value start)))
        ;; #S(struct-name slot1 val1 ...) — read-time struct constructor
        ;; Delegates to host CL read to construct the struct at read time
        (#\s (lex-advance state)
             (let* ((form-text (lex-read-form-text state))
                    (full-text (concatenate 'string "#S" form-text))
                    (value (read-from-string full-text)))
               (lex-make-token state :T-INT value start)))
        ;; #* bit-vector literal — #*1010 → #*1010 (constructed as a bit-vector)
        (#\* (lex-advance state)
             (let ((bits ""))
               (loop while (and (not (lex-at-end-p state))
                                (or (char= (lex-peek state) #\0)
                                    (char= (lex-peek state) #\1)))
                     do (setf bits (concatenate 'string bits (string (lex-advance state)))))
               (let* ((len (length bits))
                      (bv  (make-array len :element-type 'bit)))
                 (dotimes (i len)
                   (setf (sbit bv i) (if (char= (char bits i) #\1) 1 0)))
                 (lex-make-token state :T-INT bv start))))
        ;; #P"pathname" — pathname literal (FR-572)
        ;; ch is char-downcase'd, so #P and #p both dispatch here
        (#\p (lex-advance state)
             (let ((str-token (lex-read-string state)))
               (lex-make-token state :T-INT
                               (pathname (lexer-token-value str-token))
                               start)))
        ;; Boolean dispatch (non-standard but useful)
        (#\t (lex-advance state)
             (lex-make-token state :T-BOOL-TRUE t start))
        (#\f (lex-advance state)
             (lex-make-token state :T-BOOL-FALSE nil start))
        (otherwise
         (cond
           ;; #nR — arbitrary radix; #n= — label assignment; #n# — label reference
           ((digit-char-p ch)
            (let ((num-str ""))
              (loop while (and (not (lex-at-end-p state)) (digit-char-p (lex-peek state)))
                    do (setf num-str (concatenate 'string num-str (string (lex-advance state)))))
              (when (lex-at-end-p state)
                (error "Lexer error at byte ~D: unexpected end after #~A" start num-str))
              (let ((dispatch (lex-peek state)))
                (cond
                  ;; #nR — radix integer
                  ((char-equal dispatch #\r)
                   (lex-advance state) ; skip 'r'/'R'
                   (let ((radix (parse-integer num-str)))
                     (when (or (< radix 2) (> radix 36))
                       (error "Lexer error at byte ~D: radix ~D out of range 2-36" start radix))
                     (lex-make-token state :T-INT (lex-read-radix-integer state radix) start)))
                  ;; #n= — label assignment: #0=(1 2 3) caches (1 2 3) as label 0
                  ((char= dispatch #\=)
                   (lex-advance state) ; skip '='
                   (let* ((form-text (lex-read-form-text state))
                          (n (parse-integer num-str))
                          (value (read-from-string form-text)))
                     (when *lexer-label-table*
                       (setf (gethash n *lexer-label-table*) value))
                     (lex-make-token state :T-INT value start)))
                  ;; #n# — label reference: return previously cached object
                  ((char= dispatch #\#)
                   (lex-advance state) ; skip '#'
                   (let* ((n (parse-integer num-str))
                          (value (when *lexer-label-table*
                                   (gethash n *lexer-label-table*))))
                     (lex-make-token state :T-INT value start)))
                  ;; #nA(...) — multi-dimensional array literal: #2A((1 2)(3 4))
                  ((char-equal dispatch #\a)
                   (lex-advance state) ; skip 'a'/'A'
                   (let* ((form-text (lex-read-form-text state))
                          (full-text (concatenate 'string "#" num-str "A" form-text))
                          (value (read-from-string full-text)))
                     (lex-make-token state :T-INT value start)))
                  (t
                   (error "Lexer error at byte ~D: expected R, =, #, or A after #~A, got ~C"
                          start num-str dispatch))))))
           (t
            (error "Lexer error at byte ~D: unknown dispatch character #~C" start ch))))))))


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
  (let ((*lexer-label-table* (make-hash-table :test #'eql))
        (state (make-lexer source))
        (tokens nil))
    (loop
      (let ((tok (lexer-read-token state)))
        (push tok tokens)
        (when (eq (lexer-token-type tok) :T-EOF)
          (return))))
    (nreverse tokens)))
