(in-package :cl-cc/vm)

;;; VM Character Instructions
;;;
;;; Character comparison and predicate instruction structs + execute methods.
;;; Character comparison for vm-char= and vm-char< are in strings.lisp (loads before).
;;; Symbol manipulation and character predicate instructions: symbols.lisp.

;;; Character Comparison Instructions (FR-406/FR-407)

(define-vm-char-comparison vm-char>          :char>          "Character greater-than. Returns 1 if CHAR1 > CHAR2, 0 otherwise.")
(define-vm-char-comparison vm-char<=         :char<=         "Character less-or-equal. Returns 1 if CHAR1 <= CHAR2, 0 otherwise.")
(define-vm-char-comparison vm-char>=         :char>=         "Character greater-or-equal. Returns 1 if CHAR1 >= CHAR2, 0 otherwise.")
(define-vm-char-comparison vm-char/=         :char/=         "Character inequality. Returns 1 if CHAR1 /= CHAR2, 0 otherwise.")
(define-vm-char-comparison vm-char-equal     :char-equal     "Case-insensitive character equality.")
(define-vm-char-comparison vm-char-not-equal :char-not-equal "Case-insensitive character inequality.")
(define-vm-char-comparison vm-char-lessp     :char-lessp     "Case-insensitive character less-than.")
(define-vm-char-comparison vm-char-greaterp  :char-greaterp  "Case-insensitive character greater-than.")
(define-vm-char-comparison vm-char-not-lessp    :char-not-lessp    "Case-insensitive char<=.")
(define-vm-char-comparison vm-char-not-greaterp :char-not-greaterp "Case-insensitive char>=.")

(define-simple-instruction vm-char>          :pred2 char>          :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char<=         :pred2 char<=         :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char>=         :pred2 char>=         :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char/=         :pred2 char/=         :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char-equal     :pred2 char-equal     :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char-not-equal :pred2 char-not-equal :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char-lessp     :pred2 char-lessp     :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char-greaterp  :pred2 char-greaterp  :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char-not-lessp    :pred2 char-not-lessp    :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char-not-greaterp :pred2 char-not-greaterp :lhs vm-char1 :rhs vm-char2)

;;; Character Predicate and Conversion Instructions (FR-409/FR-410)

(define-vm-unary-instruction vm-both-case-p     :both-case-p     "Test if character has both upper and lower case versions.")
(define-vm-unary-instruction vm-graphic-char-p  :graphic-char-p  "Test if character is a graphic (printable) character.")
(define-vm-unary-instruction vm-standard-char-p :standard-char-p "Test if character is a standard character.")
(define-vm-unary-instruction vm-digit-char      :digit-char      "Convert digit (0-9) to the character representing it.")
(define-vm-unary-instruction vm-char-name       :char-name       "Return the name of a character as a string, or nil.")
(define-vm-unary-instruction vm-name-char       :name-char       "Return the character with the given name string, or nil.")

(define-simple-instruction vm-both-case-p    :pred1 both-case-p)
(define-simple-instruction vm-graphic-char-p :pred1 graphic-char-p)
(define-simple-instruction vm-standard-char-p :pred1 standard-char-p)
(define-simple-instruction vm-digit-char     :unary digit-char)
(define-simple-instruction vm-char-name      :unary char-name)
(define-simple-instruction vm-name-char      :unary name-char)

;;; FR-405: make-string

(define-vm-instruction vm-make-string (vm-instruction)
  "Create a string of SIZE characters, initially spaces (or CHAR if provided)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (char nil :reader vm-char)
  (:sexp-tag :make-string)
  (:sexp-slots dst src char))

(defmethod execute-instruction ((inst vm-make-string) state pc labels)
  (declare (ignore labels))
  (let* ((size      (vm-reg-get state (vm-src inst)))
         (char-reg  (vm-char inst))
         (init-char (if char-reg (vm-reg-get state char-reg) #\Space)))
    (vm-reg-set state (vm-dst inst) (make-string size :initial-element init-char))
    (values (1+ pc) nil nil)))
