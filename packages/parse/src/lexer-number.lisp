(in-package :cl-cc/parse)
;;;; lexer-number.lisp — Number token reader: integer, float, ratio.
;;;;
;;;; Depends on lexer.lisp (structs, state ops, lex-make-token, lex-peek, lex-advance).
;;;; See also: lexer-readers.lisp for string, character, and symbol readers.

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
  (let ((frac-val 0)
        (frac-digits 0)
        (exp-marker nil))
    (loop for ch = (lex-peek state)
          while (and ch (digit-char-p ch))
          do (setf frac-val (+ (* frac-val 10) (digit-char-p ch)))
             (incf frac-digits)
             (lex-advance state))
    (let ((mantissa (+ int-part (/ frac-val (expt 10 frac-digits))))
          (exp-val 0)
          (exp-sign 1))
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
         (lex-make-token state :T-INT (* sign int-part) start))
        (t
         (setf (lexer-state-pos state) start)
         nil)))))
