(in-package :cl-cc)

;;; VM String Instructions
;;;
;;; This file extends the VM with string operations including comparisons,
;;; manipulation, and character access.

;;; String Comparison Instructions

;; All binary string comparisons share the same (dst str1 str2) slot structure.
(defmacro define-vm-string-comparison (name tag docstring)
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (str1 nil :reader vm-str1)
     (str2 nil :reader vm-str2)
     (:sexp-tag ,tag)
     (:sexp-slots dst str1 str2)))

(define-vm-string-comparison vm-string=           :string=          "Case-sensitive string equality. Returns 1 if equal, 0 otherwise.")
(define-vm-string-comparison vm-string<           :string<          "Case-sensitive string less than. Returns 1 if STR1 < STR2, 0 otherwise.")
(define-vm-string-comparison vm-string>           :string>          "Case-sensitive string greater than. Returns 1 if STR1 > STR2, 0 otherwise.")
(define-vm-string-comparison vm-string<=          :string<=         "Case-sensitive string <=. Returns 1 if STR1 <= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string>=          :string>=         "Case-sensitive string >=. Returns 1 if STR1 >= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string-equal      :string-equal     "Case-insensitive string equality. Returns 1 if equal (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-lessp      :string-lessp     "Case-insensitive string <. Returns 1 if STR1 < STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-greaterp   :string-greaterp  "Case-insensitive string >. Returns 1 if STR1 > STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-not-equal  :string-not-equal "Case-sensitive string inequality. Returns 1 if STR1 /= STR2, 0 otherwise.")
(define-vm-string-comparison vm-string-not-greaterp :string-not-greaterp "Case-insensitive string <=. Returns 1 if STR1 <= STR2 (ignoring case), 0 otherwise.")
(define-vm-string-comparison vm-string-not-lessp  :string-not-lessp "Case-insensitive string >=. Returns 1 if STR1 >= STR2 (ignoring case), 0 otherwise.")

;;; String Access and Query Instructions

;; define-vm-unary-instruction and define-vm-char-comparison are defined in vm.lisp.

(define-vm-unary-instruction vm-string-length :string-length "String length. DST = length of SRC string.")

(define-vm-instruction vm-char (vm-instruction)
  "Character at position. DST = STRING[INDEX]."
  (dst nil :reader vm-dst)
  (string nil :reader vm-string-reg)
  (index nil :reader vm-index)
  (:sexp-tag :char)
  (:sexp-slots dst string index))

(define-vm-unary-instruction vm-char-code :char-code "Character code. DST = ASCII/Unicode code of SRC character.")
(define-vm-unary-instruction vm-code-char :code-char "Character from code. DST = character with code SRC.")

(define-vm-char-comparison vm-char= :char= "Character equality. Returns 1 if CHAR1 equals CHAR2, 0 otherwise.")
(define-vm-char-comparison vm-char< :char< "Character less than. Returns 1 if CHAR1 < CHAR2, 0 otherwise.")

;;; String Manipulation Instructions

(define-vm-instruction vm-subseq (vm-instruction)
  "Substring extraction. DST = STRING[START:END]."
  (dst nil :reader vm-dst)
  (string nil :reader vm-string-reg)
  (start nil :reader vm-start)
  (end nil :reader vm-end)
  (:sexp-tag :subseq)
  (:sexp-slots dst string start end))

(define-vm-instruction vm-concatenate (vm-instruction)
  "String concatenation. DST = STR1 + STR2."
  (dst nil :reader vm-dst)
  (str1 nil :reader vm-str1)
  (str2 nil :reader vm-str2)
  (parts nil :reader vm-parts)
  (:sexp-tag :concatenate)
  (:sexp-slots dst str1 str2 parts))

(define-vm-unary-instruction vm-string-upcase    :string-upcase    "Uppercase conversion. DST = uppercase of SRC.")
(define-vm-unary-instruction vm-string-downcase  :string-downcase  "Lowercase conversion. DST = lowercase of SRC.")
(define-vm-unary-instruction vm-string-capitalize :string-capitalize "Capitalize string. DST = capitalized form of SRC.")
;;; FR-475: Destructive string case operations
(define-vm-unary-instruction vm-nstring-upcase    :nstring-upcase    "Destructive uppercase. Modifies and returns SRC.")
(define-vm-unary-instruction vm-nstring-downcase  :nstring-downcase  "Destructive lowercase. Modifies and returns SRC.")
(define-vm-unary-instruction vm-nstring-capitalize :nstring-capitalize "Destructive capitalize. Modifies and returns SRC.")

(define-vm-instruction vm-string-trim (vm-instruction)
  "Trim characters from both ends. DST = STRING with CHAR-BAG chars trimmed from both ends."
  (dst nil :reader vm-dst)
  (char-bag nil :reader vm-char-bag)
  (string nil :reader vm-string-reg)
  (:sexp-tag :string-trim)
  (:sexp-slots dst char-bag string))

(define-vm-instruction vm-string-left-trim (vm-instruction)
  "Trim characters from left. DST = STRING with CHAR-BAG chars trimmed from left."
  (dst nil :reader vm-dst)
  (char-bag nil :reader vm-char-bag)
  (string nil :reader vm-string-reg)
  (:sexp-tag :string-left-trim)
  (:sexp-slots dst char-bag string))

(define-vm-instruction vm-string-right-trim (vm-instruction)
  "Trim characters from right. DST = STRING with CHAR-BAG chars trimmed from right."
  (dst nil :reader vm-dst)
  (char-bag nil :reader vm-char-bag)
  (string nil :reader vm-string-reg)
  (:sexp-tag :string-right-trim)
  (:sexp-slots dst char-bag string))

;;; String Search Instructions

(define-vm-instruction vm-search-string (vm-instruction)
  "Search for pattern in string. DST = index of PATTERN in STRING from START, or -1 if not found."
  (dst nil :reader vm-dst)
  (pattern nil :reader vm-pattern)
  (string nil :reader vm-string-reg)
  (start nil :reader vm-start)
  (:sexp-tag :search-string)
  (:sexp-slots dst pattern string start))

;;; S-expression -> Instruction Conversion (Extended)

;;; Instruction Execution - String Comparisons
;;; Use :binary (pass-through) so ANSI return values are preserved:
;;;   string=, string-equal     → T or NIL
;;;   string<, string>, etc.    → mismatch index (integer) or NIL
;;; Previously used :pred2 which returned 0/1, but 0 is truthy in CL —
;;; this caused (if (string< "xyz" "abc") ...) to always take the true branch.

(define-simple-instruction vm-string= :binary string= :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string< :binary string< :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string> :binary string> :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string<= :binary string<= :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string>= :binary string>= :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-equal :binary string-equal :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-lessp :binary string-lessp :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-greaterp :binary string-greaterp :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-not-equal :binary string-not-equal :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-not-greaterp :binary string-not-greaterp :lhs vm-str1 :rhs vm-str2)
(define-simple-instruction vm-string-not-lessp :binary string-not-lessp :lhs vm-str1 :rhs vm-str2)

;;; Instruction Execution - String Access and Query

(define-simple-instruction vm-string-length :unary length)

(defmethod execute-instruction ((inst vm-char) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
         (index (vm-reg-get state (vm-index inst)))
         (result (char string index)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-char-code :unary char-code)
(define-simple-instruction vm-code-char :unary code-char)

(define-simple-instruction vm-char= :pred2 char= :lhs vm-char1 :rhs vm-char2)
(define-simple-instruction vm-char< :pred2 char< :lhs vm-char1 :rhs vm-char2)

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

;; FR-405: make-string (1-arg: size; optional char slot for :initial-element)
(define-vm-instruction vm-make-string (vm-instruction)
  "Create a string of SIZE characters, initially spaces (or CHAR if provided)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (char nil :reader vm-char)
  (:sexp-tag :make-string)
  (:sexp-slots dst src char))

(defmethod execute-instruction ((inst vm-make-string) state pc labels)
  (declare (ignore labels))
  (let* ((size (vm-reg-get state (vm-src inst)))
         (init-char (let ((c (vm-char inst)))
                      (if c (vm-reg-get state c) #\Space)))
         (result (make-string size :initial-element init-char)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - String Manipulation

(defmethod execute-instruction ((inst vm-subseq) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
         (start  (vm-reg-get state (vm-start inst)))
         ;; nil end-slot means no upper bound — pass nil to subseq (= end of sequence)
         (end    (if (vm-end inst) (vm-reg-get state (vm-end inst)) nil))
         (result (subseq string start end)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-concatenate) state pc labels)
  (declare (ignore labels))
  (let* ((parts (or (vm-parts inst)
                    (list (vm-str1 inst) (vm-str2 inst))))
         (result (apply #'concatenate 'string
                        (mapcar (lambda (reg) (vm-reg-get state reg)) parts))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-simple-instruction vm-string-upcase :unary string-upcase)
(define-simple-instruction vm-string-downcase :unary string-downcase)
(define-simple-instruction vm-string-capitalize :unary string-capitalize)
(define-simple-instruction vm-nstring-upcase :unary nstring-upcase)
(define-simple-instruction vm-nstring-downcase :unary nstring-downcase)
(define-simple-instruction vm-nstring-capitalize :unary nstring-capitalize)

(defmethod execute-instruction ((inst vm-string-trim) state pc labels)
  (declare (ignore labels))
  (let ((result (string-trim (vm-reg-get state (vm-char-bag inst))
                             (vm-reg-get state (vm-string-reg inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-left-trim) state pc labels)
  (declare (ignore labels))
  (let ((result (string-left-trim (vm-reg-get state (vm-char-bag inst))
                                  (vm-reg-get state (vm-string-reg inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-string-right-trim) state pc labels)
  (declare (ignore labels))
  (let ((result (string-right-trim (vm-reg-get state (vm-char-bag inst))
                                   (vm-reg-get state (vm-string-reg inst)))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - String Search

(defmethod execute-instruction ((inst vm-search-string) state pc labels)
  (declare (ignore labels))
  (let* ((pattern (vm-reg-get state (vm-pattern inst)))
         (string (vm-reg-get state (vm-string-reg inst)))
         (start (vm-reg-get state (vm-start inst)))
         (result (or (search pattern string :start2 start) -1)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))


;;; ─── String character mutation (FR-614) ─────────────────────────────────
;;;
;;; (setf (char s i) v) / (setf (schar s i) v) → (rt-string-set s i v)

(define-vm-instruction vm-string-set (vm-instruction)
  "Set character in string at index. Returns the new character."
  (dst nil :reader vm-dst)
  (str nil :reader vm-str-reg)
  (idx nil :reader vm-idx)
  (val nil :reader vm-val-reg)
  (:sexp-tag :string-set)
  (:sexp-slots dst str idx val))

(defmethod execute-instruction ((inst vm-string-set) state pc labels)
  (declare (ignore labels))
  (let ((v (vm-reg-get state (vm-val-reg inst))))
    (setf (char (vm-reg-get state (vm-str-reg inst))
                (vm-reg-get state (vm-idx inst))) v)
    (vm-reg-set state (vm-dst inst) v)
    (values (1+ pc) nil nil)))

;;; (Symbol manipulation and character predicate instructions moved to src/vm/symbols.lisp)
