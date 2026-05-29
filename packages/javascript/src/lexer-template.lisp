;;;; packages/javascript/src/lexer-template.lisp — ES2026 Template Literal Lexer
;;;;
;;;; Lexes JavaScript template literals (backtick-delimited strings).
;;;;
;;;; Token format (plist): (:type :T-STRING :value string)
;;;;                    or (:type :T-TEMPLATE-PARTS :value parts-list)
;;;;
;;;; parts-list elements are:
;;;;   - a string (literal text segment)
;;;;   - (:template-expr token-list) where token-list is the inner expression tokens
;;;;
;;;; Escape sequences handled:
;;;;   \n  -> newline
;;;;   \r  -> carriage return
;;;;   \t  -> tab
;;;;   \\  -> literal backslash
;;;;   \`  -> literal backtick
;;;;   \$  -> literal dollar sign
;;;;   \uXXXX     -> unicode codepoint (4 hex digits)
;;;;   \u{XXXXXX} -> unicode codepoint (variable hex digits)
;;;;   \xXX       -> hex escape (2 hex digits)
;;;;   other      -> character as-is (permissive template tagged literal support)

(in-package :cl-cc/javascript)

;;; ─── Character helpers ───────────────────────────────────────────────────────

(defun js-hex-digit-p (ch)
  "Return true if CH is a hexadecimal digit character."
  (and ch (or (char<= #\0 ch #\9)
              (char<= #\a ch #\f)
              (char<= #\A ch #\F))))

(defun js-hex-digit-value (ch)
  "Return the numeric value of hex digit character CH."
  (cond
    ((char<= #\0 ch #\9) (- (char-code ch) (char-code #\0)))
    ((char<= #\a ch #\f) (+ 10 (- (char-code ch) (char-code #\a))))
    ((char<= #\A ch #\F) (+ 10 (- (char-code ch) (char-code #\A))))
    (t (error "JS template lex error: ~S is not a hex digit" ch))))

;;; ─── Escape sequence processor ───────────────────────────────────────────────

(defun js-lex-template-escape (source pos)
  "Process escape sequence starting at POS (after backslash was consumed).
  Returns (values char new-pos).
  Handles: n r t \\ ` $ uXXXX u{...} xXX and permissive fallback."
  (when (>= pos (length source))
    (error "JS template lex error: trailing backslash at end of source"))
  (let ((esc (char source pos)))
    (case esc
      (#\n  (values #\Newline (1+ pos)))
      (#\r  (values #\Return  (1+ pos)))
      (#\t  (values #\Tab     (1+ pos)))
      (#\\  (values #\\       (1+ pos)))
      (#\`  (values #\`       (1+ pos)))
      (#\$  (values #\$       (1+ pos)))
      (#\0  (values #\Null    (1+ pos)))
      (#\u
       ;; Unicode escape: \uXXXX or \u{XXXXXX}
       (let ((next-pos (1+ pos)))
         (when (>= next-pos (length source))
           (error "JS template lex error: incomplete \\u escape"))
         (cond
           ;; \u{...} form
           ((char= (char source next-pos) #\{)
            (let ((hex-start (+ next-pos 1))
                  (hex-end   (+ next-pos 1)))
              (loop while (and (< hex-end (length source))
                               (js-hex-digit-p (char source hex-end)))
                    do (incf hex-end))
              (when (>= hex-end (length source))
                (error "JS template lex error: unterminated \\u{} escape"))
              (unless (char= (char source hex-end) #\})
                (error "JS template lex error: expected } to close \\u{} escape"))
              (when (= hex-start hex-end)
                (error "JS template lex error: empty \\u{} escape"))
              (let ((codepoint 0))
                (loop for i from hex-start below hex-end
                      do (setf codepoint
                               (+ (* codepoint 16)
                                  (js-hex-digit-value (char source i)))))
                (when (> codepoint #x10FFFF)
                  (error "JS template lex error: unicode codepoint ~X out of range" codepoint))
                (values (code-char codepoint) (1+ hex-end)))))
           ;; \uXXXX form — exactly 4 hex digits required
           (t
            (let ((end (+ next-pos 4)))
              (when (> end (length source))
                (error "JS template lex error: incomplete \\uXXXX escape"))
              (loop for i from next-pos below end
                    unless (js-hex-digit-p (char source i))
                    do (error "JS template lex error: non-hex digit in \\uXXXX escape at position ~D" i))
              (let ((codepoint 0))
                (loop for i from next-pos below end
                      do (setf codepoint
                               (+ (* codepoint 16)
                                  (js-hex-digit-value (char source i)))))
                (values (code-char codepoint) end)))))))
      (#\x
       ;; Hex escape: \xXX — exactly 2 hex digits required
       (let ((hex-start (1+ pos))
             (hex-end   (+ pos 3)))
         (when (> hex-end (length source))
           (error "JS template lex error: incomplete \\xXX escape"))
         (loop for i from hex-start below hex-end
               unless (js-hex-digit-p (char source i))
               do (error "JS template lex error: non-hex digit in \\xXX escape at position ~D" i))
         (let ((byte (+ (* (js-hex-digit-value (char source hex-start)) 16)
                        (js-hex-digit-value (char source (1+ hex-start))))))
           (values (code-char byte) hex-end))))
      (otherwise
       ;; Permissive fallback — e.g. \a, \b, \v, \f, unrecognized sequences
       ;; In tagged template literals these may be intentional (cooked = undefined)
       ;; We preserve the character as-is for maximum compatibility.
       (values esc (1+ pos))))))

;;; ─── Template text segment scanner ──────────────────────────────────────────

(defun js-lex-template-text-part (source pos)
  "Scan template chars from POS until backtick or dollar-brace.
  Returns (values text-string end-pos end-reason) where end-reason is
  :end-of-template (closing backtick found) or :start-interp (dollar-brace found)."
  (let ((buf (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (when (>= pos (length source))
        (error "JS template lex error: unterminated template literal"))
      (let ((ch (char source pos)))
        (cond
          ;; Closing backtick — end of template
          ((char= ch #\`)
           (return (values (copy-seq buf) (1+ pos) :end-of-template)))
          ;; Start of interpolation: ${
          ((and (char= ch #\$)
                (< (1+ pos) (length source))
                (char= (char source (1+ pos)) #\{))
           (return (values (copy-seq buf) (+ pos 2) :start-interp)))
          ;; Escape sequence
          ((char= ch #\\)
           (multiple-value-bind (escaped-char new-pos)
               (js-lex-template-escape source (1+ pos))
             (vector-push-extend escaped-char buf)
             (setf pos new-pos)))
          ;; Ordinary character
          (t
           (vector-push-extend ch buf)
           (incf pos)))))))

;;; ─── Inner expression tokenizer ──────────────────────────────────────────────

(defun js-lex-template-inner-tokens (source pos)
  "Tokenize inside template interpolation ${ ... } until the matching closing brace.
  Tracks brace nesting depth so inner objects/blocks are handled correctly.
  Returns (values token-list new-pos)."
  ;; We need the JS main tokenizer — forward-reference via symbol-function
  ;; so this file can be loaded before js-next-token is defined.
  (let ((tokens '())
        (depth 1))
    (loop
      (when (>= pos (length source))
        (error "JS template lex error: unterminated template interpolation"))
      (multiple-value-bind (tok new-pos)
          (funcall (symbol-function 'js-next-token) source pos)
        (let ((type  (getf tok :type))
              (value (getf tok :value)))
          (cond
            ;; EOF inside interpolation is an error
            ((eq type :T-EOF)
             (error "JS template lex error: unexpected EOF inside template interpolation"))
            ;; Another opening brace increases depth
            ((and (eq type :T-LBRACE) (string= value "{"))
             (incf depth)
             (push tok tokens)
             (setf pos new-pos))
            ;; Closing brace — decrement depth
            ((and (eq type :T-RBRACE) (string= value "}"))
             (decf depth)
             (cond
               ;; Matched the interpolation's closing brace — done
               ((zerop depth)
                (return (values (nreverse tokens) new-pos)))
               ;; Inner closing brace — collect it
               (t
                (push tok tokens)
                (setf pos new-pos))))
            ;; All other tokens accumulated
            (t
             (push tok tokens)
             (setf pos new-pos))))))))

;;; ─── Top-level template literal lexer ───────────────────────────────────────

(defun js-lex-template (source pos)
  "Lex a complete template literal. POS is AFTER the opening backtick.
  Returns (values token new-pos).

  For simple (no interpolation):  (:type :T-STRING        :value string)
  For interpolated:               (:type :T-TEMPLATE-PARTS :value (list part ...))"
  (let ((parts '())
        (has-interp nil))
    (loop
      (multiple-value-bind (text end-pos end-reason)
          (js-lex-template-text-part source pos)
        (cond
          ;; End of template — push final text segment (even if empty when
          ;; the template ends with an interpolation: `${x}`)
          ((eq end-reason :end-of-template)
           (push text parts)
           (let ((all-parts (nreverse parts)))
             (if has-interp
                 (return (values (list :type :T-TEMPLATE-PARTS :value all-parts) end-pos))
                 ;; Simple string — single part with no expr segments
                 (return (values (list :type :T-STRING :value (car all-parts)) end-pos)))))
          ;; Start of interpolation: ${ ...tokens... }
          ((eq end-reason :start-interp)
           (setf has-interp t)
           ;; Push the text segment (may be empty string between adjacent interpolations)
           (push text parts)
           ;; Tokenize the inner expression
           (multiple-value-bind (inner-tokens expr-end-pos)
               (js-lex-template-inner-tokens source end-pos)
             (push (list :template-expr inner-tokens) parts)
             (setf pos expr-end-pos))))))))
