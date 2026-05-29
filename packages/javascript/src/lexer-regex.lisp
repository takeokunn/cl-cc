;;;; lexer-regex.lisp — ES2026 regex literal lexer for the JavaScript frontend
;;;;
;;;; Called by the main lexer when it determines that / begins a regex literal
;;;; rather than a division operator.
;;;;
;;;; Token format: (:type :T-REGEX :value (:regex pattern-string flags-string))
;;;;
;;;; ES2026 valid flags: d g i m s u v y
;;;;   The /v (Unicode Sets) flag introduces nested character classes [[...]]
;;;;   which require tracking inner bracket depth inside [...].

(in-package :cl-cc/javascript)

;;; ---------------------------------------------------------------------------
;;; Flag character predicate
;;; ---------------------------------------------------------------------------

(defun js-valid-regex-flag-p (ch)
  "Return true if CH is a letter (a-z or A-Z).
  All current and forward-compatible ES regex flags are single ASCII letters."
  (and ch
       (or (char<= #\a ch #\z)
           (char<= #\A ch #\Z))))

;;; ---------------------------------------------------------------------------
;;; ES2026 flag validator
;;; ---------------------------------------------------------------------------

(defparameter *js-regex-valid-flags*
  (coerce "dgimsuvy" 'list)
  "ES2026 regex flags: d g i m s u v y (as a list of characters).
The u (Unicode) flag was previously omitted from the literal, rejecting /.../u.")

(defun js-validate-regex-flags (flags-string)
  "Signal an error for unknown or duplicate flags in FLAGS-STRING.
  ES2026 valid flags are d g i m s u v y.
  Note: /u and /v are mutually exclusive, but that is a parse-level constraint;
  the lexer only checks for unknown and duplicate flags."
  (let ((seen nil))
    (loop for ch across flags-string do
      (unless (member ch *js-regex-valid-flags* :test #'char=)
        (error "JS lex error: unknown regex flag ~C" ch))
      (when (member ch seen :test #'char=)
        (error "JS lex error: duplicate regex flag ~C" ch))
      (push ch seen))))

;;; ---------------------------------------------------------------------------
;;; Unicode Sets mode (/v) nested bracket depth helper
;;; ---------------------------------------------------------------------------

(defun %js-regex-in-char-class-p (in-char-class inner-depth)
  "Return true if the lexer is currently inside a character class.
  IN-CHAR-CLASS is the outer [...] flag; INNER-DEPTH counts nested [[ ]] levels."
  (or in-char-class (plusp inner-depth)))

;;; ---------------------------------------------------------------------------
;;; Main regex lexer
;;; ---------------------------------------------------------------------------

(defun js-lex-regex (source pos)
  "Lex a JavaScript regex literal.
  POS is the index AFTER the opening / has already been consumed by the caller.

  Handles:
    - Escape sequences: backslash makes the next character literal.
    - Character classes: [...] — a / inside is not the closing delimiter.
    - Nested character classes: [[...]] for Unicode Sets mode (/v flag).
    - Flags: any letters immediately following the closing /.
    - Error on unterminated regex (end-of-source inside pattern).
    - Error on bare (unescaped) newline inside the pattern.
    - Validation of flags (unknown / duplicate).

  Returns (values token new-pos) where:
    token = (:type :T-REGEX :value (:regex pattern-string flags-string))
    new-pos is the index of the first character after the flags."
  (let ((pattern-buf (make-array 64 :element-type 'character
                                 :fill-pointer 0 :adjustable t))
        ;; Are we inside a top-level [...] character class?
        (in-char-class nil)
        ;; Depth counter for nested [[ ]] inside a char class (/v mode).
        ;; 0 means we are at the outermost [...] level (or not in a class).
        (inner-depth 0)
        ;; Did the previous character consume a backslash (escape-next)?
        (escape-next nil))
    (loop
      ;; --- end-of-source check ---
      (when (>= pos (length source))
        (error "JS lex error: unterminated regex literal"))
      (let ((ch (char source pos)))
        (cond

          ;; 1. Escape state: current char is literal, clear escape flag.
          (escape-next
           (vector-push-extend ch pattern-buf)
           (setf escape-next nil)
           (incf pos))

          ;; 2. Start of escape sequence.
          ((char= ch #\\)
           (vector-push-extend ch pattern-buf)
           (setf escape-next t)
           (incf pos))

          ;; 3. Bare newline — always an error regardless of char-class depth.
          ((or (char= ch #\Newline)
               (char= ch #\Return)
               ;; Unicode line/paragraph separators (U+2028, U+2029) are also
               ;; line terminators in ES source.
               (char= ch (code-char #x2028))
               (char= ch (code-char #x2029)))
           (error "JS lex error: newline inside regex literal"))

          ;; 4. Opening [ — enter or deepen character class.
          ((char= ch #\[)
           (vector-push-extend ch pattern-buf)
           (cond
             ((not in-char-class)
              ;; Entering the outermost character class.
              (setf in-char-class t))
             (t
              ;; Nested [ inside a char class (Unicode Sets /v mode).
              (incf inner-depth)))
           (incf pos))

          ;; 5. Closing ] — exit or shallow character class.
          ((char= ch #\])
           (vector-push-extend ch pattern-buf)
           (cond
             ((and in-char-class (plusp inner-depth))
              ;; Closing a nested [[...]] level.
              (decf inner-depth))
             (in-char-class
              ;; Closing the outermost [...].
              (setf in-char-class nil)))
           (incf pos))

          ;; 6. Closing / outside any character class — end of pattern.
          ((and (char= ch #\/) (not in-char-class) (zerop inner-depth))
           ;; Consume the closing slash.
           (incf pos)
           ;; Collect flags: consecutive letter characters.
           (let ((flag-buf (make-array 8 :element-type 'character
                                       :fill-pointer 0 :adjustable t)))
             (loop while (and (< pos (length source))
                              (js-valid-regex-flag-p (char source pos)))
                   do (vector-push-extend (char source pos) flag-buf)
                      (incf pos))
             (let ((flags-str (copy-seq flag-buf)))
               ;; Validate flags (unknown / duplicate).
               (js-validate-regex-flags flags-str)
               (return
                 (values (list :type :T-REGEX
                               :value (list :regex
                                            (copy-seq pattern-buf)
                                            flags-str))
                         pos)))))

          ;; 7. Any other character — accumulate into the pattern buffer.
          (t
           (vector-push-extend ch pattern-buf)
           (incf pos)))))))
