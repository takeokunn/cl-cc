;;;; lexer.lisp — Core lexer primitives: structs, character classification,
;;;;              low-level state ops, trivia collection, and the token constructor.
;;;;
;;;; Token readers (number, string, character, symbol/keyword) live in lexer-readers.lisp.
;;;; See lexer-dispatch.lisp for feature conditionals, hash dispatch, and the
;;;; main token reader / public API.

(in-package :cl-cc/parse)

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

(defun lex-constituent-p (ch)
  "Is CH a constituent character for CL symbols?"
  (and ch
       (not (lex-whitespace-p ch))
       (not (member ch '(#\( #\) #\' #\` #\, #\" #\; #\:) :test #'char=))))

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
