;;;; lexer-skip.lisp — Form-Skipping Helpers, Feature Conditionals, and Read-Form-Text
;;;;
;;;; These subsystems are consumed exclusively by lex-read-hash-dispatch (lexer-dispatch.lisp).
;;;; Keeping them here avoids cluttering the main dispatch table with low-level skip logic.
;;;;
;;;; Depends on lexer.lisp (structs, lex-advance, lex-peek, lex-at-end-p, lex-constituent-p,
;;;; lex-skip-trivia, lex-skip-block-comment, lex-read-symbol-name, lexer-state-{pos,source}).

(in-package :cl-cc/parse)

;;; ─── Form-Skipping Helpers ───────────────────────────────────────────────────
;;; Used by lex-skip-form (below) and lex-read-form-text.

(defun %lex-skip-string-chars (state)
  "Advance STATE past a string body and its closing double-quote.
Caller must have already consumed the opening quote."
  (loop until (or (lex-at-end-p state) (char= (lex-peek state) #\"))
        do (when (char= (lex-peek state) #\\) (lex-advance state))
           (lex-advance state))
  (unless (lex-at-end-p state) (lex-advance state)))

(defun %lex-skip-list-body (state)
  "Advance STATE past a balanced list, handling nested strings and line comments.
Caller must have already consumed the opening '('."
  (let ((depth 1))
    (loop while (and (> depth 0) (not (lex-at-end-p state)))
          do (let ((c (lex-peek state)))
               (lex-advance state)
               (cond ((char= c #\()  (incf depth))
                     ((char= c #\))  (decf depth))
                     ((char= c #\")  (%lex-skip-string-chars state))
                     ((char= c #\;)  ; line comment — skip to newline
                      (loop until (or (lex-at-end-p state)
                                      (char= (lex-peek state) #\Newline))
                            do (lex-advance state))))))))

(defun %lex-skip-atom (state)
  "Advance STATE past an atom (symbol, number, or keyword including leading colon)."
  (when (and (not (lex-at-end-p state)) (char= (lex-peek state) #\:))
    (lex-advance state))
  (loop while (and (not (lex-at-end-p state)) (lex-constituent-p (lex-peek state)))
        do (lex-advance state)))

(defun %lex-skip-char-literal (state)
  "Skip a #\\ character literal name. Caller must have already consumed the '#\\'."
  (unless (lex-at-end-p state)
    (lex-advance state)  ; consume first char (or start of long name like Space)
    (loop while (and (not (lex-at-end-p state)) (lex-constituent-p (lex-peek state)))
          do (lex-advance state))))

(defun %lex-skip-two-forms (state)
  "Skip two consecutive forms (used for #+/- feature conditionals)."
  (lex-skip-form state)
  (lex-skip-form state))

(defparameter *lex-hash-dispatch-table*
  ;; Keys: hash-dispatch characters. Values: function symbols (looked up at call time).
  ;; Special case #\\ is handled outside this table (it needs an extra char advance).
  '((#\(  . %lex-skip-list-body)      ; #(...) — vector literal
    (#\'  . lex-skip-form)             ; #'fn  — function notation
    (#\.  . lex-skip-form)             ; #.x   — read-time eval
    (#\+  . %lex-skip-two-forms)       ; #+feat body
    (#\-  . %lex-skip-two-forms)       ; #-feat body
    (#\|  . lex-skip-block-comment))   ; #|...|# block comment
  "Alist mapping hash-dispatch characters to handler function symbols.
Uses symbol names so forward references resolve at call time via funcall.
Does not include #\\ (character literals), which need special advance logic.")

(defun %lex-skip-hash-form (state)
  "Skip a #-dispatched form. Caller must have already consumed the '#'."
  (unless (lex-at-end-p state)
    (let* ((dispatch-ch (lex-peek state))
           (handler (cdr (assoc dispatch-ch *lex-hash-dispatch-table*))))
      (lex-advance state)
      (cond
        (handler (funcall handler state))
        ((char= dispatch-ch #\\) (%lex-skip-char-literal state))
        (t ; unknown dispatch — skip atom
         (loop while (and (not (lex-at-end-p state)) (lex-constituent-p (lex-peek state)))
               do (lex-advance state)))))))

(defun lex-skip-form (state)
  "Skip a single balanced form from STATE (for #+/- when feature test fails)."
  (lex-skip-trivia state)
  (when (lex-at-end-p state) (return-from lex-skip-form))
  (let ((ch (lex-peek state)))
    (cond
      ((char= ch #\()
       (lex-advance state) (%lex-skip-list-body state))
      ((char= ch #\")
       (lex-advance state) (%lex-skip-string-chars state))
      ((or (char= ch #\') (char= ch #\`) (char= ch #\,))
       (lex-advance state)
       (when (and (not (lex-at-end-p state)) (char= (lex-peek state) #\@))
         (lex-advance state))
       (lex-skip-form state))
      ((char= ch #\#)
       (lex-advance state) (%lex-skip-hash-form state))
      (t
       (%lex-skip-atom state)))))

;;; ─── Feature Conditionals ────────────────────────────────────────────────────

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

;;; ─── Read-Form-Text ──────────────────────────────────────────────────────────

(defun lex-read-form-text (state)
  "Read a single balanced form's raw text from STATE and return it as a string.
Advances STATE past the form."
  (lex-skip-trivia state)
  (when (lex-at-end-p state)
    (error "Lexer error: unexpected end in form"))
  (let ((start (lexer-state-pos state))
        (ch    (lex-peek state)))
    (cond
      ((char= ch #\() (lex-advance state) (%lex-skip-list-body state))
      ((char= ch #\") (lex-advance state) (%lex-skip-string-chars state))
      (t              (%lex-skip-atom state)))
    (subseq (lexer-state-source state) start (lexer-state-pos state))))

;;; ─── Label Table for #n= / #n# (FR-599) ────────────────────────────────────

(defvar *lexer-label-table* nil
  "Hash table mapping integer labels to objects for #n= / #n# reader macros.
Bound to a fresh HT by lex-all; nil outside lexing context.")
